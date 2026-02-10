🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.5 Développement d'un ORM personnalisé portable

## Introduction

Un **ORM (Object-Relational Mapping)** est une technique de programmation qui permet de manipuler des données de base de données comme s'il s'agissait d'objets dans votre code. Au lieu d'écrire des requêtes SQL complexes, vous travaillez avec des classes et des objets Pascal.

### Qu'est-ce qu'un ORM ?

**Sans ORM (SQL brut) :**
```pascal
Query.SQL.Text := 'SELECT * FROM clients WHERE id = ' + IntToStr(ClientID);
Query.Open;
Nom := Query.FieldByName('nom').AsString;
Email := Query.FieldByName('email').AsString;
```

**Avec ORM :**
```pascal
Client := TClient.Load(ClientID);
Nom := Client.Nom;
Email := Client.Email;
```

### Avantages d'un ORM

✅ **Code plus lisible** - Manipulation orientée objet  
✅ **Moins de SQL à écrire** - L'ORM génère les requêtes  
✅ **Sécurité** - Protection automatique contre les injections SQL  
✅ **Portabilité** - Changez de SGBD sans modifier le code métier  
✅ **Maintenance** - Modifications centralisées  
✅ **Validation** - Règles métier intégrées aux objets

### Pourquoi développer son propre ORM ?

Bien qu'il existe des ORM comme **mORMot** ou **tiOPF**, créer le vôtre offre :

- **Contrôle total** sur le comportement
- **Légèreté** - Seulement les fonctionnalités dont vous avez besoin
- **Apprentissage** - Comprendre les mécanismes internes
- **Adaptation** - Personnalisation pour vos besoins spécifiques
- **Portabilité** - Conçu dès le départ pour Windows et Linux

## Architecture de base d'un ORM

### Les composants essentiels

```
┌────────────────────────────────────────┐
│  Application (Code métier)             │
├────────────────────────────────────────┤
│  Entités (Classes métier)              │
│  TClient, TCommande, TProduit...       │
├────────────────────────────────────────┤
│  Gestionnaire d'entités (EntityManager)│
│  - CRUD (Create, Read, Update, Delete) │
│  - Transactions                        │
├────────────────────────────────────────┤
│  Mappeur SQL (SQL Mapper)              │
│  - Génération des requêtes SQL         │
│  - Mapping objet ↔ table               │
├────────────────────────────────────────┤
│  Couche d'accès aux données (DAL)      │
│  - Connexion BD (ZEOS, SQLdb...)       │
├────────────────────────────────────────┤
│  Base de données                       │
└────────────────────────────────────────┘
```

## Phase 1 : Fondations - La classe de base

Toutes nos entités hériteront d'une classe de base commune.

```pascal
unit ORM.Base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo;

type
  // Attribut pour marquer une propriété comme clé primaire
  TPrimaryKeyAttribute = class(TCustomAttribute)
  end;

  // Attribut pour mapper une propriété à un champ de table
  TColumnAttribute = class(TCustomAttribute)
  private
    FColumnName: string;
  public
    constructor Create(const AColumnName: string);
    property ColumnName: string read FColumnName;
  end;

  // Attribut pour spécifier le nom de la table
  TTableAttribute = class(TCustomAttribute)
  private
    FTableName: string;
  public
    constructor Create(const ATableName: string);
    property TableName: string read FTableName;
  end;

  // Classe de base pour toutes les entités
  TEntityBase = class(TPersistent)
  private
    FIsNew: Boolean;
    FIsModified: Boolean;
  protected
    procedure MarkAsModified;
  public
    constructor Create; virtual;

    property IsNew: Boolean read FIsNew write FIsNew;
    property IsModified: Boolean read FIsModified write FIsModified;
  end;

implementation

{ TColumnAttribute }

constructor TColumnAttribute.Create(const AColumnName: string);
begin
  inherited Create;
  FColumnName := AColumnName;
end;

{ TTableAttribute }

constructor TTableAttribute.Create(const ATableName: string);
begin
  inherited Create;
  FTableName := ATableName;
end;

{ TEntityBase }

constructor TEntityBase.Create;
begin
  inherited Create;
  FIsNew := True;
  FIsModified := False;
end;

procedure TEntityBase.MarkAsModified;
begin
  if not FIsNew then
    FIsModified := True;
end;

end.
```

## Phase 2 : Définition des entités métier

Créons une entité Client pour illustrer l'utilisation.

```pascal
unit Entities.Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ORM.Base;

type
  [TTable('clients')]
  TClient = class(TEntityBase)
  private
    FID: Integer;
    FNom: string;
    FPrenom: string;
    FEmail: string;
    FTelephone: string;
    FVille: string;
    FDateInscription: TDateTime;
    FActif: Boolean;

    procedure SetNom(const Value: string);
    procedure SetPrenom(const Value: string);
    procedure SetEmail(const Value: string);
    procedure SetTelephone(const Value: string);
    procedure SetVille(const Value: string);
    procedure SetActif(Value: Boolean);
  public
    constructor Create; override;
  published
    [TPrimaryKey]
    [TColumn('id')]
    property ID: Integer read FID write FID;

    [TColumn('nom')]
    property Nom: string read FNom write SetNom;

    [TColumn('prenom')]
    property Prenom: string read FPrenom write SetPrenom;

    [TColumn('email')]
    property Email: string read FEmail write SetEmail;

    [TColumn('telephone')]
    property Telephone: string read FTelephone write SetTelephone;

    [TColumn('ville')]
    property Ville: string read FVille write SetVille;

    [TColumn('date_inscription')]
    property DateInscription: TDateTime read FDateInscription write FDateInscription;

    [TColumn('actif')]
    property Actif: Boolean read FActif write SetActif;
  end;

implementation

{ TClient }

constructor TClient.Create;
begin
  inherited Create;
  FActif := True;
  FDateInscription := Now;
end;

procedure TClient.SetNom(const Value: string);
begin
  if FNom <> Value then
  begin
    FNom := Value;
    MarkAsModified;
  end;
end;

procedure TClient.SetPrenom(const Value: string);
begin
  if FPrenom <> Value then
  begin
    FPrenom := Value;
    MarkAsModified;
  end;
end;

procedure TClient.SetEmail(const Value: string);
begin
  if FEmail <> Value then
  begin
    FEmail := Value;
    MarkAsModified;
  end;
end;

procedure TClient.SetTelephone(const Value: string);
begin
  if FTelephone <> Value then
  begin
    FTelephone := Value;
    MarkAsModified;
  end;
end;

procedure TClient.SetVille(const Value: string);
begin
  if FVille <> Value then
  begin
    FVille := Value;
    MarkAsModified;
  end;
end;

procedure TClient.SetActif(Value: Boolean);
begin
  if FActif <> Value then
  begin
    FActif := Value;
    MarkAsModified;
  end;
end;

end.
```

## Phase 3 : Le mappeur SQL

Ce composant traduit les objets en requêtes SQL.

```pascal
unit ORM.SQLMapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Rtti, ORM.Base;

type
  TSQLMapper = class
  private
    class function GetTableName(EntityClass: TClass): string;
    class function GetPrimaryKeyName(EntityClass: TClass): string;
    class function GetColumnName(PropInfo: PPropInfo): string;
  public
    class function GenerateSelectSQL(EntityClass: TClass; const WhereClause: string = ''): string;
    class function GenerateSelectByIDSQL(EntityClass: TClass): string;
    class function GenerateInsertSQL(Entity: TEntityBase): string;
    class function GenerateUpdateSQL(Entity: TEntityBase): string;
    class function GenerateDeleteSQL(EntityClass: TClass; ID: Integer): string;
  end;

implementation

{ TSQLMapper }

class function TSQLMapper.GetTableName(EntityClass: TClass): string;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Attr: TCustomAttribute;
begin
  Result := EntityClass.ClassName;

  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(EntityClass);
    for Attr in RttiType.GetAttributes do
    begin
      if Attr is TTableAttribute then
      begin
        Result := TTableAttribute(Attr).TableName;
        Break;
      end;
    end;
  finally
    Context.Free;
  end;
end;

class function TSQLMapper.GetPrimaryKeyName(EntityClass: TClass): string;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
begin
  Result := 'id';  // Valeur par défaut

  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(EntityClass);
    for Prop in RttiType.GetProperties do
    begin
      for Attr in Prop.GetAttributes do
      begin
        if Attr is TPrimaryKeyAttribute then
        begin
          Result := LowerCase(Prop.Name);
          Exit;
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

class function TSQLMapper.GetColumnName(PropInfo: PPropInfo): string;
begin
  // Simplification : retourne le nom de la propriété en minuscules
  // Dans une version complète, on lirait l'attribut TColumn
  Result := LowerCase(PropInfo^.Name);
end;

class function TSQLMapper.GenerateSelectSQL(EntityClass: TClass;
  const WhereClause: string): string;
var
  TableName: string;
begin
  TableName := GetTableName(EntityClass);
  Result := Format('SELECT * FROM %s', [TableName]);

  if WhereClause <> '' then
    Result := Result + ' WHERE ' + WhereClause;
end;

class function TSQLMapper.GenerateSelectByIDSQL(EntityClass: TClass): string;
var
  TableName, PKName: string;
begin
  TableName := GetTableName(EntityClass);
  PKName := GetPrimaryKeyName(EntityClass);
  Result := Format('SELECT * FROM %s WHERE %s = :id', [TableName, PKName]);
end;

class function TSQLMapper.GenerateInsertSQL(Entity: TEntityBase): string;
var
  TableName: string;
  PropList: PPropList;
  PropCount, i: Integer;
  PropInfo: PPropInfo;
  Columns, Values: string;
begin
  TableName := GetTableName(Entity.ClassType);
  PropCount := GetPropList(Entity, PropList);

  try
    Columns := '';
    Values := '';

    for i := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[i];

      // Ignorer la clé primaire (auto-incrémentée)
      if LowerCase(PropInfo^.Name) = 'id' then
        Continue;

      if Columns <> '' then
      begin
        Columns := Columns + ', ';
        Values := Values + ', ';
      end;

      Columns := Columns + GetColumnName(PropInfo);
      Values := Values + ':' + LowerCase(PropInfo^.Name);
    end;

    Result := Format('INSERT INTO %s (%s) VALUES (%s)',
      [TableName, Columns, Values]);
  finally
    FreeMem(PropList);
  end;
end;

class function TSQLMapper.GenerateUpdateSQL(Entity: TEntityBase): string;
var
  TableName, PKName: string;
  PropList: PPropList;
  PropCount, i: Integer;
  PropInfo: PPropInfo;
  SetClause: string;
begin
  TableName := GetTableName(Entity.ClassType);
  PKName := GetPrimaryKeyName(Entity.ClassType);
  PropCount := GetPropList(Entity, PropList);

  try
    SetClause := '';

    for i := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[i];

      // Ignorer la clé primaire
      if LowerCase(PropInfo^.Name) = 'id' then
        Continue;

      if SetClause <> '' then
        SetClause := SetClause + ', ';

      SetClause := SetClause + GetColumnName(PropInfo) + ' = :' +
        LowerCase(PropInfo^.Name);
    end;

    Result := Format('UPDATE %s SET %s WHERE %s = :id',
      [TableName, SetClause, PKName]);
  finally
    FreeMem(PropList);
  end;
end;

class function TSQLMapper.GenerateDeleteSQL(EntityClass: TClass; ID: Integer): string;
var
  TableName, PKName: string;
begin
  TableName := GetTableName(EntityClass);
  PKName := GetPrimaryKeyName(EntityClass);
  Result := Format('DELETE FROM %s WHERE %s = %d', [TableName, PKName, ID]);
end;

end.
```

## Phase 4 : Le gestionnaire d'entités

C'est le cœur de l'ORM, qui orchestre toutes les opérations.

```pascal
unit ORM.EntityManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Generics.Collections, TypInfo,
  ORM.Base, ORM.SQLMapper, ZConnection, ZDataset;

type
  TEntityManager = class
  private
    FConnection: TZConnection;
    FInTransaction: Boolean;

    procedure FillEntityFromDataset(Entity: TEntityBase; Dataset: TDataset);
    procedure SetParametersFromEntity(Query: TZQuery; Entity: TEntityBase);
  public
    constructor Create(Connection: TZConnection);
    destructor Destroy; override;

    // Opérations CRUD
    function Find<T: TEntityBase>(ID: Integer): T;
    function FindAll<T: TEntityBase>(const WhereClause: string = ''): TList<T>;
    procedure Save(Entity: TEntityBase);
    procedure Delete(Entity: TEntityBase); overload;
    procedure Delete<T: TEntityBase>(ID: Integer); overload;

    // Transactions
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;

    property Connection: TZConnection read FConnection;
    property InTransaction: Boolean read FInTransaction;
  end;

implementation

{ TEntityManager }

constructor TEntityManager.Create(Connection: TZConnection);
begin
  inherited Create;
  FConnection := Connection;
  FInTransaction := False;
end;

destructor TEntityManager.Destroy;
begin
  if FInTransaction then
    Rollback;
  inherited Destroy;
end;

procedure TEntityManager.FillEntityFromDataset(Entity: TEntityBase; Dataset: TDataset);
var
  PropList: PPropList;
  PropCount, i: Integer;
  PropInfo: PPropInfo;
  Field: TField;
  FieldName: string;
begin
  PropCount := GetPropList(Entity, PropList);
  try
    for i := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[i];
      FieldName := LowerCase(PropInfo^.Name);
      Field := Dataset.FindField(FieldName);

      if Assigned(Field) and not Field.IsNull then
      begin
        case PropInfo^.PropType^.Kind of
          tkInteger, tkInt64:
            SetOrdProp(Entity, PropInfo, Field.AsInteger);
          tkFloat:
            SetFloatProp(Entity, PropInfo, Field.AsFloat);
          tkString, tkLString, tkAString:
            SetStrProp(Entity, PropInfo, Field.AsString);
          tkBool:
            SetOrdProp(Entity, PropInfo, Ord(Field.AsBoolean));
        end;
      end;
    end;

    Entity.IsNew := False;
    Entity.IsModified := False;
  finally
    FreeMem(PropList);
  end;
end;

procedure TEntityManager.SetParametersFromEntity(Query: TZQuery; Entity: TEntityBase);
var
  PropList: PPropList;
  PropCount, i: Integer;
  PropInfo: PPropInfo;
  ParamName: string;
  Param: TParam;
begin
  PropCount := GetPropList(Entity, PropList);
  try
    for i := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[i];
      ParamName := LowerCase(PropInfo^.Name);
      Param := Query.Params.FindParam(ParamName);

      if Assigned(Param) then
      begin
        case PropInfo^.PropType^.Kind of
          tkInteger, tkInt64:
            Param.AsInteger := GetOrdProp(Entity, PropInfo);
          tkFloat:
            Param.AsFloat := GetFloatProp(Entity, PropInfo);
          tkString, tkLString, tkAString:
            Param.AsString := GetStrProp(Entity, PropInfo);
          tkBool:
            Param.AsBoolean := Boolean(GetOrdProp(Entity, PropInfo));
        end;
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

function TEntityManager.Find<T>(ID: Integer): T;
var
  Query: TZQuery;
  SQL: string;
begin
  Result := nil;
  Query := TZQuery.Create(nil);
  try
    Query.Connection := FConnection;
    SQL := TSQLMapper.GenerateSelectByIDSQL(T);
    Query.SQL.Text := SQL;
    Query.ParamByName('id').AsInteger := ID;
    Query.Open;

    if not Query.EOF then
    begin
      Result := T.Create;
      FillEntityFromDataset(Result, Query);
    end;
  finally
    Query.Free;
  end;
end;

function TEntityManager.FindAll<T>(const WhereClause: string): TList<T>;
var
  Query: TZQuery;
  SQL: string;
  Entity: T;
begin
  Result := TList<T>.Create;
  Query := TZQuery.Create(nil);
  try
    Query.Connection := FConnection;
    SQL := TSQLMapper.GenerateSelectSQL(T, WhereClause);
    Query.SQL.Text := SQL;
    Query.Open;

    while not Query.EOF do
    begin
      Entity := T.Create;
      FillEntityFromDataset(Entity, Query);
      Result.Add(Entity);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TEntityManager.Save(Entity: TEntityBase);
var
  Query: TZQuery;
  SQL: string;
begin
  if Entity.IsNew then
  begin
    // INSERT
    SQL := TSQLMapper.GenerateInsertSQL(Entity);
  end
  else if Entity.IsModified then
  begin
    // UPDATE
    SQL := TSQLMapper.GenerateUpdateSQL(Entity);
  end
  else
    Exit; // Rien à faire

  Query := TZQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := SQL;
    SetParametersFromEntity(Query, Entity);
    Query.ExecSQL;

    Entity.IsNew := False;
    Entity.IsModified := False;
  finally
    Query.Free;
  end;
end;

procedure TEntityManager.Delete(Entity: TEntityBase);
var
  ID: Integer;
begin
  ID := GetOrdProp(Entity, 'ID');
  Delete(Entity.ClassType, ID);
end;

procedure TEntityManager.Delete<T>(ID: Integer);
var
  Query: TZQuery;
  SQL: string;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := FConnection;
    SQL := TSQLMapper.GenerateDeleteSQL(T, ID);
    Query.SQL.Text := SQL;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TEntityManager.BeginTransaction;
begin
  if not FInTransaction then
  begin
    FConnection.StartTransaction;
    FInTransaction := True;
  end;
end;

procedure TEntityManager.Commit;
begin
  if FInTransaction then
  begin
    FConnection.Commit;
    FInTransaction := False;
  end;
end;

procedure TEntityManager.Rollback;
begin
  if FInTransaction then
  begin
    FConnection.Rollback;
    FInTransaction := False;
  end;
end;

end.
```

## Utilisation de l'ORM

Maintenant que notre ORM est complet, voyons comment l'utiliser dans une application.

### Configuration initiale

```pascal
program TestORM;

uses
  SysUtils, ZConnection,
  ORM.EntityManager, Entities.Client;

var
  Connection: TZConnection;
  EntityManager: TEntityManager;
begin
  // Configuration de la connexion
  Connection := TZConnection.Create(nil);
  try
    Connection.Protocol := 'postgresql-9';
    Connection.HostName := 'localhost';
    Connection.Database := 'ma_base';
    Connection.User := 'postgres';
    Connection.Password := 'password';

    {$IFDEF WINDOWS}
    Connection.LibraryLocation := 'libs\libpq.dll';
    {$ENDIF}

    Connection.Connected := True;

    // Créer le gestionnaire d'entités
    EntityManager := TEntityManager.Create(Connection);
    try
      // Utiliser l'ORM...
      TestOperationsCRUD(EntityManager);
    finally
      EntityManager.Free;
    end;
  finally
    Connection.Free;
  end;
end.
```

### Créer un nouvel enregistrement (INSERT)

```pascal
procedure CreerNouveauClient(EM: TEntityManager);
var
  Client: TClient;
begin
  Client := TClient.Create;
  try
    Client.Nom := 'Dupont';
    Client.Prenom := 'Jean';
    Client.Email := 'jean.dupont@example.com';
    Client.Telephone := '0123456789';
    Client.Ville := 'Paris';

    // Sauvegarder dans la base de données
    EM.Save(Client);

    WriteLn('Client créé avec ID: ', Client.ID);
  finally
    Client.Free;
  end;
end;
```

### Lire un enregistrement (SELECT)

```pascal
procedure AfficherClient(EM: TEntityManager; ClientID: Integer);
var
  Client: TClient;
begin
  Client := EM.Find<TClient>(ClientID);
  if Assigned(Client) then
  try
    WriteLn('Nom: ', Client.Nom);
    WriteLn('Prénom: ', Client.Prenom);
    WriteLn('Email: ', Client.Email);
    WriteLn('Ville: ', Client.Ville);
  finally
    Client.Free;
  end
  else
    WriteLn('Client non trouvé');
end;
```

### Modifier un enregistrement (UPDATE)

```pascal
procedure ModifierEmailClient(EM: TEntityManager; ClientID: Integer);
var
  Client: TClient;
begin
  Client := EM.Find<TClient>(ClientID);
  if Assigned(Client) then
  try
    Client.Email := 'nouveau.email@example.com';
    Client.Telephone := '0987654321';

    // Les setters ont marqué l'objet comme modifié
    EM.Save(Client);  // Génère un UPDATE

    WriteLn('Client mis à jour');
  finally
    Client.Free;
  end;
end;
```

### Supprimer un enregistrement (DELETE)

```pascal
procedure SupprimerClient(EM: TEntityManager; ClientID: Integer);
begin
  // Méthode 1 : Supprimer directement par ID
  EM.Delete<TClient>(ClientID);
  WriteLn('Client supprimé');
end;

procedure SupprimerClientAvecObjet(EM: TEntityManager; ClientID: Integer);
var
  Client: TClient;
begin
  // Méthode 2 : Charger puis supprimer
  Client := EM.Find<TClient>(ClientID);
  if Assigned(Client) then
  try
    EM.Delete(Client);
    WriteLn('Client supprimé');
  finally
    Client.Free;
  end;
end;
```

### Lister tous les enregistrements

```pascal
procedure ListerTousLesClients(EM: TEntityManager);
var
  Clients: TList<TClient>;
  Client: TClient;
begin
  Clients := EM.FindAll<TClient>();
  try
    WriteLn('Nombre de clients: ', Clients.Count);

    for Client in Clients do
    begin
      WriteLn(Format('%d - %s %s (%s)',
        [Client.ID, Client.Prenom, Client.Nom, Client.Email]));
    end;
  finally
    // Libérer tous les objets
    for Client in Clients do
      Client.Free;
    Clients.Free;
  end;
end;
```

### Rechercher avec filtre

```pascal
procedure RechercherClientsParVille(EM: TEntityManager; const Ville: string);
var
  Clients: TList<TClient>;
  Client: TClient;
begin
  Clients := EM.FindAll<TClient>('ville = ' + QuotedStr(Ville));
  try
    WriteLn(Format('Clients à %s: %d', [Ville, Clients.Count]));

    for Client in Clients do
      WriteLn('  - ', Client.Nom, ' ', Client.Prenom);
  finally
    for Client in Clients do
      Client.Free;
    Clients.Free;
  end;
end;
```

## Gestion avancée des transactions

```pascal
procedure TransfertClientEntreSections(EM: TEntityManager;
  ClientID: Integer; const NouvelleVille: string);
var
  Client: TClient;
begin
  EM.BeginTransaction;
  try
    // Charger le client
    Client := EM.Find<TClient>(ClientID);
    if not Assigned(Client) then
      raise Exception.Create('Client introuvable');

    try
      // Modifier
      Client.Ville := NouvelleVille;
      EM.Save(Client);

      // ... autres opérations ...

      // Valider la transaction
      EM.Commit;
      WriteLn('Transaction validée');
    finally
      Client.Free;
    end;
  except
    on E: Exception do
    begin
      EM.Rollback;
      WriteLn('Transaction annulée: ', E.Message);
      raise;
    end;
  end;
end;
```

## Extension : Relations entre entités

### Ajout d'une entité Commande

```pascal
unit Entities.Commande;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ORM.Base;

type
  [TTable('commandes')]
  TCommande = class(TEntityBase)
  private
    FID: Integer;
    FClientID: Integer;
    FDateCommande: TDateTime;
    FMontantTotal: Currency;
    FStatut: string;
  published
    [TPrimaryKey]
    [TColumn('id')]
    property ID: Integer read FID write FID;

    [TColumn('client_id')]
    property ClientID: Integer read FClientID write FClientID;

    [TColumn('date_commande')]
    property DateCommande: TDateTime read FDateCommande write FDateCommande;

    [TColumn('montant_total')]
    property MontantTotal: Currency read FMontantTotal write FMontantTotal;

    [TColumn('statut')]
    property Statut: string read FStatut write FStatut;
  end;

implementation

end.
```

### Requête avec jointure

```pascal
procedure AfficherCommandesClient(EM: TEntityManager; ClientID: Integer);
var
  Commandes: TList<TCommande>;
  Commande: TCommande;
begin
  Commandes := EM.FindAll<TCommande>(
    Format('client_id = %d', [ClientID]));
  try
    WriteLn('Commandes du client:');

    for Commande in Commandes do
    begin
      WriteLn(Format('  Commande #%d - %s - %.2f €',
        [Commande.ID,
         FormatDateTime('dd/mm/yyyy', Commande.DateCommande),
         Commande.MontantTotal]));
    end;
  finally
    for Commande in Commandes do
      Commande.Free;
    Commandes.Free;
  end;
end;
```

## Optimisations et fonctionnalités avancées

### Cache d'entités

```pascal
type
  TEntityCache = class
  private
    FCache: TDictionary<string, TEntityBase>;
    function GenerateKey(EntityClass: TClass; ID: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Entity: TEntityBase);
    function Get(EntityClass: TClass; ID: Integer): TEntityBase;
    procedure Remove(EntityClass: TClass; ID: Integer);
    procedure Clear;
  end;

implementation

constructor TEntityCache.Create;
begin
  inherited Create;
  FCache := TDictionary<string, TEntityBase>.Create;
end;

destructor TEntityCache.Destroy;
begin
  Clear;
  FCache.Free;
  inherited Destroy;
end;

function TEntityCache.GenerateKey(EntityClass: TClass; ID: Integer): string;
begin
  Result := EntityClass.ClassName + '_' + IntToStr(ID);
end;

procedure TEntityCache.Add(Entity: TEntityBase);
var
  Key: string;
  ID: Integer;
begin
  ID := GetOrdProp(Entity, 'ID');
  Key := GenerateKey(Entity.ClassType, ID);
  FCache.AddOrSetValue(Key, Entity);
end;

function TEntityCache.Get(EntityClass: TClass; ID: Integer): TEntityBase;
var
  Key: string;
begin
  Key := GenerateKey(EntityClass, ID);
  if not FCache.TryGetValue(Key, Result) then
    Result := nil;
end;

procedure TEntityCache.Remove(EntityClass: TClass; ID: Integer);
var
  Key: string;
begin
  Key := GenerateKey(EntityClass, ID);
  FCache.Remove(Key);
end;

procedure TEntityCache.Clear;
var
  Entity: TEntityBase;
begin
  for Entity in FCache.Values do
    Entity.Free;
  FCache.Clear;
end;

end.
```

### Validation des données

Ajoutons un système de validation aux entités :

```pascal
unit ORM.Validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TValidationError = class
  private
    FPropertyName: string;
    FErrorMessage: string;
  public
    constructor Create(const APropName, AMessage: string);
    property PropertyName: string read FPropertyName;
    property ErrorMessage: string read FErrorMessage;
  end;

  TValidationResult = class
  private
    FErrors: TList<TValidationError>;
    function GetIsValid: Boolean;
    function GetErrorCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddError(const PropertyName, ErrorMessage: string);
    function GetErrorsAsString: string;

    property IsValid: Boolean read GetIsValid;
    property ErrorCount: Integer read GetErrorCount;
    property Errors: TList<TValidationError> read FErrors;
  end;

  IValidatable = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']
    function Validate: TValidationResult;
  end;

implementation

{ TValidationError }

constructor TValidationError.Create(const APropName, AMessage: string);
begin
  inherited Create;
  FPropertyName := APropName;
  FErrorMessage := AMessage;
end;

{ TValidationResult }

constructor TValidationResult.Create;
begin
  inherited Create;
  FErrors := TList<TValidationError>.Create;
end;

destructor TValidationResult.Destroy;
var
  Error: TValidationError;
begin
  for Error in FErrors do
    Error.Free;
  FErrors.Free;
  inherited Destroy;
end;

function TValidationResult.GetIsValid: Boolean;
begin
  Result := FErrors.Count = 0;
end;

function TValidationResult.GetErrorCount: Integer;
begin
  Result := FErrors.Count;
end;

procedure TValidationResult.AddError(const PropertyName, ErrorMessage: string);
begin
  FErrors.Add(TValidationError.Create(PropertyName, ErrorMessage));
end;

function TValidationResult.GetErrorsAsString: string;
var
  Error: TValidationError;
begin
  Result := '';
  for Error in FErrors do
  begin
    if Result <> '' then
      Result := Result + #13#10;
    Result := Result + Error.PropertyName + ': ' + Error.ErrorMessage;
  end;
end;

end.
```

### Client avec validation

```pascal
unit Entities.Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ORM.Base, ORM.Validation;

type
  [TTable('clients')]
  TClient = class(TEntityBase, IValidatable)
  private
    FID: Integer;
    FNom: string;
    FPrenom: string;
    FEmail: string;
    FTelephone: string;
    // ... autres champs ...
  public
    function Validate: TValidationResult;
  published
    // ... properties ...
  end;

implementation

uses
  RegExpr;  // Pour validation email

function TClient.Validate: TValidationResult;
var
  EmailRegex: TRegExpr;
begin
  Result := TValidationResult.Create;

  // Validation du nom
  if Trim(FNom) = '' then
    Result.AddError('Nom', 'Le nom est obligatoire');

  if Length(FNom) < 2 then
    Result.AddError('Nom', 'Le nom doit contenir au moins 2 caractères');

  // Validation du prénom
  if Trim(FPrenom) = '' then
    Result.AddError('Prenom', 'Le prénom est obligatoire');

  // Validation de l'email
  if Trim(FEmail) = '' then
    Result.AddError('Email', 'L''email est obligatoire')
  else
  begin
    EmailRegex := TRegExpr.Create;
    try
      EmailRegex.Expression := '^[\w\.-]+@[\w\.-]+\.\w+;
      if not EmailRegex.Exec(FEmail) then
        Result.AddError('Email', 'Format d''email invalide');
    finally
      EmailRegex.Free;
    end;
  end;

  // Validation du téléphone
  if (FTelephone <> '') and (Length(FTelephone) < 10) then
    Result.AddError('Telephone', 'Le téléphone doit contenir au moins 10 chiffres');
end;

end.
```

### Utilisation avec validation

```pascal
procedure CreerClientAvecValidation(EM: TEntityManager);
var
  Client: TClient;
  ValidationResult: TValidationResult;
begin
  Client := TClient.Create;
  try
    Client.Nom := 'D';  // Trop court
    Client.Prenom := 'Jean';
    Client.Email := 'email-invalide';  // Format invalide

    // Valider avant de sauvegarder
    ValidationResult := Client.Validate;
    try
      if ValidationResult.IsValid then
      begin
        EM.Save(Client);
        WriteLn('Client créé avec succès');
      end
      else
      begin
        WriteLn('Erreurs de validation:');
        WriteLn(ValidationResult.GetErrorsAsString);
      end;
    finally
      ValidationResult.Free;
    end;
  finally
    Client.Free;
  end;
end;
```

## Lazy Loading (chargement différé)

Pour optimiser les performances, on peut charger les relations uniquement quand nécessaire :

```pascal
unit Entities.Client;

interface

uses
  Classes, SysUtils, Generics.Collections, ORM.Base, Entities.Commande;

type
  TClient = class(TEntityBase)
  private
    FID: Integer;
    FNom: string;
    // ... autres champs ...
    FCommandes: TList<TCommande>;
    FCommandesLoaded: Boolean;
    FEntityManager: TObject;  // Référence au EntityManager

    function GetCommandes: TList<TCommande>;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Commandes: TList<TCommande> read GetCommandes;
    property EntityManager: TObject read FEntityManager write FEntityManager;
  published
    // ... properties ...
  end;

implementation

uses
  ORM.EntityManager;

function TClient.GetCommandes: TList<TCommande>;
begin
  if not FCommandesLoaded then
  begin
    if Assigned(FEntityManager) then
    begin
      FCommandes := TEntityManager(FEntityManager).FindAll<TCommande>(
        Format('client_id = %d', [FID]));
      FCommandesLoaded := True;
    end;
  end;
  Result := FCommandes;
end;

// Utilisation
procedure AfficherClientAvecCommandes(EM: TEntityManager; ClientID: Integer);
var
  Client: TClient;
  Commande: TCommande;
begin
  Client := EM.Find<TClient>(ClientID);
  if Assigned(Client) then
  try
    Client.EntityManager := EM;  // Injecter l'EntityManager

    WriteLn('Client: ', Client.Nom);
    WriteLn('Commandes:');

    // Les commandes sont chargées automatiquement ici
    for Commande in Client.Commandes do
      WriteLn('  - Commande #', Commande.ID);
  finally
    Client.Free;
  end;
end;
```

## Query Builder - Constructeur de requêtes fluide

Pour des requêtes plus complexes sans écrire de SQL :

```pascal
unit ORM.QueryBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TQueryBuilder = class
  private
    FTableName: string;
    FWhereConditions: TStringList;
    FOrderBy: string;
    FLimit: Integer;
    FOffset: Integer;
  public
    constructor Create(const TableName: string);
    destructor Destroy; override;

    function Where(const Condition: string): TQueryBuilder;
    function AndWhere(const Condition: string): TQueryBuilder;
    function OrWhere(const Condition: string): TQueryBuilder;
    function OrderBy(const Field: string; Ascending: Boolean = True): TQueryBuilder;
    function Limit(Count: Integer): TQueryBuilder;
    function Offset(Count: Integer): TQueryBuilder;

    function BuildSQL: string;
  end;

implementation

constructor TQueryBuilder.Create(const TableName: string);
begin
  inherited Create;
  FTableName := TableName;
  FWhereConditions := TStringList.Create;
  FLimit := 0;
  FOffset := 0;
end;

destructor TQueryBuilder.Destroy;
begin
  FWhereConditions.Free;
  inherited Destroy;
end;

function TQueryBuilder.Where(const Condition: string): TQueryBuilder;
begin
  FWhereConditions.Clear;
  FWhereConditions.Add(Condition);
  Result := Self;
end;

function TQueryBuilder.AndWhere(const Condition: string): TQueryBuilder;
begin
  if FWhereConditions.Count > 0 then
    FWhereConditions.Add('AND ' + Condition)
  else
    FWhereConditions.Add(Condition);
  Result := Self;
end;

function TQueryBuilder.OrWhere(const Condition: string): TQueryBuilder;
begin
  if FWhereConditions.Count > 0 then
    FWhereConditions.Add('OR ' + Condition)
  else
    FWhereConditions.Add(Condition);
  Result := Self;
end;

function TQueryBuilder.OrderBy(const Field: string; Ascending: Boolean): TQueryBuilder;
begin
  FOrderBy := Field;
  if not Ascending then
    FOrderBy := FOrderBy + ' DESC';
  Result := Self;
end;

function TQueryBuilder.Limit(Count: Integer): TQueryBuilder;
begin
  FLimit := Count;
  Result := Self;
end;

function TQueryBuilder.Offset(Count: Integer): TQueryBuilder;
begin
  FOffset := Count;
  Result := Self;
end;

function TQueryBuilder.BuildSQL: string;
var
  i: Integer;
begin
  Result := 'SELECT * FROM ' + FTableName;

  if FWhereConditions.Count > 0 then
  begin
    Result := Result + ' WHERE ';
    for i := 0 to FWhereConditions.Count - 1 do
    begin
      if i > 0 then
        Result := Result + ' ';
      Result := Result + FWhereConditions[i];
    end;
  end;

  if FOrderBy <> '' then
    Result := Result + ' ORDER BY ' + FOrderBy;

  if FLimit > 0 then
    Result := Result + ' LIMIT ' + IntToStr(FLimit);

  if FOffset > 0 then
    Result := Result + ' OFFSET ' + IntToStr(FOffset);
end;

end.
```

### Utilisation du Query Builder

```pascal
procedure RechercheAvecQueryBuilder(EM: TEntityManager);
var
  QB: TQueryBuilder;
  SQL: string;
  Clients: TList<TClient>;
begin
  QB := TQueryBuilder.Create('clients');
  try
    SQL := QB
      .Where('ville = ' + QuotedStr('Paris'))
      .AndWhere('actif = 1')
      .OrderBy('nom', True)
      .Limit(10)
      .BuildSQL;

    WriteLn('SQL généré: ', SQL);
    // SELECT * FROM clients WHERE ville = 'Paris' AND actif = 1 ORDER BY nom LIMIT 10

    // Utiliser avec l'EntityManager
    Clients := EM.FindAll<TClient>(QB.BuildSQL);
    try
      // Traiter les résultats...
    finally
      for Client in Clients do
        Client.Free;
      Clients.Free;
    end;
  finally
    QB.Free;
  end;
end;
```

## Portabilité multi-SGBD

Gérons les différences entre les bases de données :

```pascal
unit ORM.DatabaseAdapter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDatabaseType = (dtPostgreSQL, dtMySQL, dtSQLite, dtFirebird);

  IDatabaseAdapter = interface
    ['{C3D4E5F6-A7B8-9012-CDEF-123456789012}']
    function GetAutoIncrementSyntax: string;
    function GetLimitSyntax(Limit, Offset: Integer): string;
    function QuoteIdentifier(const Identifier: string): string;
    function GetDateTimeLiteral(const DateTime: TDateTime): string;
  end;

  TDatabaseAdapterFactory = class
  public
    class function CreateAdapter(DBType: TDatabaseType): IDatabaseAdapter;
  end;

  TPostgreSQLAdapter = class(TInterfacedObject, IDatabaseAdapter)
  public
    function GetAutoIncrementSyntax: string;
    function GetLimitSyntax(Limit, Offset: Integer): string;
    function QuoteIdentifier(const Identifier: string): string;
    function GetDateTimeLiteral(const DateTime: TDateTime): string;
  end;

  TMySQLAdapter = class(TInterfacedObject, IDatabaseAdapter)
  public
    function GetAutoIncrementSyntax: string;
    function GetLimitSyntax(Limit, Offset: Integer): string;
    function QuoteIdentifier(const Identifier: string): string;
    function GetDateTimeLiteral(const DateTime: TDateTime): string;
  end;

  TSQLiteAdapter = class(TInterfacedObject, IDatabaseAdapter)
  public
    function GetAutoIncrementSyntax: string;
    function GetLimitSyntax(Limit, Offset: Integer): string;
    function QuoteIdentifier(const Identifier: string): string;
    function GetDateTimeLiteral(const DateTime: TDateTime): string;
  end;

implementation

{ TDatabaseAdapterFactory }

class function TDatabaseAdapterFactory.CreateAdapter(DBType: TDatabaseType): IDatabaseAdapter;
begin
  case DBType of
    dtPostgreSQL: Result := TPostgreSQLAdapter.Create;
    dtMySQL: Result := TMySQLAdapter.Create;
    dtSQLite: Result := TSQLiteAdapter.Create;
    else
      raise Exception.Create('Type de base de données non supporté');
  end;
end;

{ TPostgreSQLAdapter }

function TPostgreSQLAdapter.GetAutoIncrementSyntax: string;
begin
  Result := 'SERIAL PRIMARY KEY';
end;

function TPostgreSQLAdapter.GetLimitSyntax(Limit, Offset: Integer): string;
begin
  Result := Format('LIMIT %d OFFSET %d', [Limit, Offset]);
end;

function TPostgreSQLAdapter.QuoteIdentifier(const Identifier: string): string;
begin
  Result := '"' + Identifier + '"';
end;

function TPostgreSQLAdapter.GetDateTimeLiteral(const DateTime: TDateTime): string;
begin
  Result := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', DateTime) + '''';
end;

{ TMySQLAdapter }

function TMySQLAdapter.GetAutoIncrementSyntax: string;
begin
  Result := 'INT AUTO_INCREMENT PRIMARY KEY';
end;

function TMySQLAdapter.GetLimitSyntax(Limit, Offset: Integer): string;
begin
  Result := Format('LIMIT %d, %d', [Offset, Limit]);
end;

function TMySQLAdapter.QuoteIdentifier(const Identifier: string): string;
begin
  Result := '`' + Identifier + '`';
end;

function TMySQLAdapter.GetDateTimeLiteral(const DateTime: TDateTime): string;
begin
  Result := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', DateTime) + '''';
end;

{ TSQLiteAdapter }

function TSQLiteAdapter.GetAutoIncrementSyntax: string;
begin
  Result := 'INTEGER PRIMARY KEY AUTOINCREMENT';
end;

function TSQLiteAdapter.GetLimitSyntax(Limit, Offset: Integer): string;
begin
  Result := Format('LIMIT %d OFFSET %d', [Limit, Offset]);
end;

function TSQLiteAdapter.QuoteIdentifier(const Identifier: string): string;
begin
  Result := '"' + Identifier + '"';
end;

function TSQLiteAdapter.GetDateTimeLiteral(const DateTime: TDateTime): string;
begin
  Result := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', DateTime) + '''';
end;

end.
```

## Migration de schéma de base de données

Pour gérer l'évolution du schéma :

```pascal
unit ORM.Migration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, ZConnection;

type
  TMigration = class abstract
  private
    FVersion: Integer;
    FDescription: string;
  public
    constructor Create(AVersion: Integer; const ADescription: string);
    procedure Up(Connection: TZConnection); virtual; abstract;
    procedure Down(Connection: TZConnection); virtual; abstract;

    property Version: Integer read FVersion;
    property Description: string read FDescription;
  end;

  TMigrationManager = class
  private
    FConnection: TZConnection;
    FMigrations: TList<TMigration>;

    procedure CreateMigrationTable;
    function GetCurrentVersion: Integer;
    procedure SetVersion(Version: Integer);
  public
    constructor Create(Connection: TZConnection);
    destructor Destroy; override;

    procedure RegisterMigration(Migration: TMigration);
    procedure MigrateToLatest;
    procedure MigrateTo(TargetVersion: Integer);
    procedure Rollback(Steps: Integer = 1);
  end;

implementation

uses
  ZDataset;

{ TMigration }

constructor TMigration.Create(AVersion: Integer; const ADescription: string);
begin
  inherited Create;
  FVersion := AVersion;
  FDescription := ADescription;
end;

{ TMigrationManager }

constructor TMigrationManager.Create(Connection: TZConnection);
begin
  inherited Create;
  FConnection := Connection;
  FMigrations := TList<TMigration>.Create;
  CreateMigrationTable;
end;

destructor TMigrationManager.Destroy;
begin
  FMigrations.Free;
  inherited Destroy;
end;

procedure TMigrationManager.CreateMigrationTable;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS schema_migrations (' +
      '  version INTEGER PRIMARY KEY,' +
      '  applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
      ')';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TMigrationManager.GetCurrentVersion: Integer;
var
  Query: TZQuery;
begin
  Result := 0;
  Query := TZQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT MAX(version) as ver FROM schema_migrations';
    Query.Open;

    if not Query.EOF and not Query.FieldByName('ver').IsNull then
      Result := Query.FieldByName('ver').AsInteger;
  finally
    Query.Free;
  end;
end;

procedure TMigrationManager.SetVersion(Version: Integer);
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'INSERT INTO schema_migrations (version) VALUES (:version)';
    Query.ParamByName('version').AsInteger := Version;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TMigrationManager.RegisterMigration(Migration: TMigration);
begin
  FMigrations.Add(Migration);
end;

procedure TMigrationManager.MigrateToLatest;
var
  CurrentVersion: Integer;
  Migration: TMigration;
begin
  CurrentVersion := GetCurrentVersion;

  for Migration in FMigrations do
  begin
    if Migration.Version > CurrentVersion then
    begin
      WriteLn(Format('Applying migration %d: %s',
        [Migration.Version, Migration.Description]));

      FConnection.StartTransaction;
      try
        Migration.Up(FConnection);
        SetVersion(Migration.Version);
        FConnection.Commit;
        WriteLn('  Success!');
      except
        on E: Exception do
        begin
          FConnection.Rollback;
          WriteLn('  Failed: ', E.Message);
          raise;
        end;
      end;
    end;
  end;
end;

end.
```

### Exemple de migration

```pascal
unit Migrations.CreateClientsTable;

{$mode objfpc}{$H+}

interface

uses
  ORM.Migration, ZConnection, ZDataset;

type
  TCreateClientsTable = class(TMigration)
  public
    constructor Create;
    procedure Up(Connection: TZConnection); override;
    procedure Down(Connection: TZConnection); override;
  end;

implementation

constructor TCreateClientsTable.Create;
begin
  inherited Create(1, 'Create clients table');
end;

procedure TCreateClientsTable.Up(Connection: TZConnection);
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'CREATE TABLE clients (' +
      '  id SERIAL PRIMARY KEY,' +
      '  nom VARCHAR(100) NOT NULL,' +
      '  prenom VARCHAR(100) NOT NULL,' +
      '  email VARCHAR(255) UNIQUE NOT NULL,' +
      '  telephone VARCHAR(20),' +
      '  ville VARCHAR(100),' +
      '  date_inscription TIMESTAMP DEFAULT CURRENT_TIMESTAMP,' +
      '  actif BOOLEAN DEFAULT TRUE' +
      ')';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TCreateClientsTable.Down(Connection: TZConnection);
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DROP TABLE IF EXISTS clients';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

end.
```

### Utilisation des migrations

```pascal
procedure ExecuterMigrations;
var
  Connection: TZConnection;
  MigrationManager: TMigrationManager;
begin
  Connection := TZConnection.Create(nil);
  try
    Connection.Protocol := 'postgresql-9';
    Connection.Database := 'ma_base';
    // ... configuration ...
    Connection.Connected := True;

    MigrationManager := TMigrationManager.Create(Connection);
    try
      // Enregistrer les migrations
      MigrationManager.RegisterMigration(TCreateClientsTable.Create);
      // MigrationManager.RegisterMigration(TAddCommandesTable.Create);
      // ... autres migrations ...

      // Exécuter toutes les migrations pending
      MigrationManager.MigrateToLatest;
    finally
      MigrationManager.Free;
    end;
  finally
    Connection.Free;
  end;
end;
```

## Tests unitaires de l'ORM

```pascal
unit Tests.ORM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  ZConnection, ORM.EntityManager, Entities.Client;

type
  TTestORM = class(TTestCase)
  private
    FConnection: TZConnection;
    FEM: TEntityManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateClient;
    procedure TestFindClient;
    procedure TestUpdateClient;
    procedure TestDeleteClient;
    procedure TestFindAllClients;
    procedure TestTransaction;
    procedure TestValidation;
  end;

implementation

procedure TTestORM.SetUp;
begin
  // Utiliser une base de données de test en mémoire
  FConnection := TZConnection.Create(nil);
  FConnection.Protocol := 'sqlite-3';
  FConnection.Database := ':memory:';
  FConnection.Connected := True;

  // Créer la table de test
  // ... SQL de création ...

  FEM := TEntityManager.Create(FConnection);
end;

procedure TTestORM.TearDown;
begin
  FEM.Free;
  FConnection.Free;
end;

procedure TTestORM.TestCreateClient;
var
  Client: TClient;
begin
  Client := TClient.Create;
  try
    Client.Nom := 'Test';
    Client.Prenom := 'User';
    Client.Email := 'test@example.com';

    FEM.Save(Client);

    AssertTrue('Client devrait avoir un ID', Client.ID > 0);
    AssertFalse('Client ne devrait plus être nouveau', Client.IsNew);
  finally
    Client.Free;
  end;
end;

procedure TTestORM.TestFindClient;
var
  Client, FoundClient: TClient;
begin
  // Créer un client
  Client := TClient.Create;
  try
    Client.Nom := 'FindTest';
    Client.Email := 'find@test.com';
    FEM.Save(Client);

    // Le rechercher
    FoundClient := FEM.Find<TClient>(Client.ID);
    try
      AssertNotNull('Client devrait être trouvé', FoundClient);
      AssertEquals('Le nom devrait correspondre', 'FindTest', FoundClient.Nom);
    finally
      FoundClient.Free;
    end;
  finally
    Client.Free;
  end;
end;

procedure TTestORM.TestTransaction;
var
  Client: TClient;
begin
  FEM.BeginTransaction;
  try
    Client := TClient.Create;
    try
      Client.Nom := 'TransacTest';
      Client.Email := 'transac@test.com';
      FEM.Save(Client);
    finally
      Client.Free;
    end;

    // Annuler
    FEM.Rollback;

    // Vérifier que rien n'a été sauvegardé
    Client := FEM.Find<TClient>(1);
    AssertNull('Le client ne devrait pas exister après rollback', Client);
  except
    FEM.Rollback;
    raise;
  end;
end;

initialization
  RegisterTest(TTestORM);

end.
```

## Bonnes pratiques et recommandations

### 1. Séparation des responsabilités

```
MonProjet/
├── src/
│   ├── ORM/
│   │   ├── ORM.Base.pas
│   │   ├── ORM.EntityManager.pas
│   │   ├── ORM.SQLMapper.pas
│   │   └── ORM.Validation.pas
│   ├── Entities/
│   │   ├── Entities.Client.pas
│   │   ├── Entities.Commande.pas
│   │   └── Entities.Produit.pas
│   └── Business/
│       ├── Services.Client.pas
│       └── Services.Commande.pas
```

### 2. Utiliser des interfaces

```pascal
type
  IClientRepository = interface
    ['{D4E5F6A7-B8C9-0123-DEFG-234567890123}']
    function GetByID(ID: Integer): TClient;
    function GetAll: TList<TClient>;
    function GetByVille(const Ville: string): TList<TClient>;
    procedure Save(Client: TClient);
    procedure Delete(Client: TClient);
  end;
```

### 3. Gestion de la mémoire

```pascal
// ✅ BON
var
  Clients: TList<TClient>;
  Client: TClient;
begin
  Clients := EM.FindAll<TClient>();
  try
    for Client in Clients do
    begin
      // Utiliser Client
    end;
  finally
    for Client in Clients do
      Client.Free;
    Clients.Free;
  end;
end;

// ❌ MAUVAIS - Fuite mémoire
var
  Clients: TList<TClient>;
begin
  Clients := EM.FindAll<TClient>();
  // Oubli de libérer les objets !
end;
```

### 4. Transactions pour opérations critiques

```pascal
procedure TraiterCommande(EM: TEntityManager; CommandeID: Integer);
begin
  EM.BeginTransaction;
  try
    // Opérations multiples
    // ...

    EM.Commit;
  except
    EM.Rollback;
    raise;
  end;
end;
```

### 5. Logging et débogage

```pascal
unit ORM.Logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  IORMLogger = interface
    ['{E5F6A7B8-C9D0-1234-EFAB-345678901234}']
    procedure Log(Level: TLogLevel; const Message: string);
    procedure LogSQL(const SQL: string);
    procedure LogError(const ErrorMsg: string);
  end;

  TFileLogger = class(TInterfacedObject, IORMLogger)
  private
    FLogFile: string;
    procedure WriteToFile(const Line: string);
  public
    constructor Create(const LogFileName: string);
    procedure Log(Level: TLogLevel; const Message: string);
    procedure LogSQL(const SQL: string);
    procedure LogError(const ErrorMsg: string);
  end;

implementation

constructor TFileLogger.Create(const LogFileName: string);
begin
  inherited Create;
  FLogFile := LogFileName;
end;

procedure TFileLogger.WriteToFile(const Line: string);
var
  F: TextFile;
begin
  AssignFile(F, FLogFile);
  if FileExists(FLogFile) then
    Append(F)
  else
    Rewrite(F);
  try
    WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' - ', Line);
  finally
    CloseFile(F);
  end;
end;

procedure TFileLogger.Log(Level: TLogLevel; const Message: string);
const
  LevelStr: array[TLogLevel] of string = ('DEBUG', 'INFO', 'WARNING', 'ERROR');
begin
  WriteToFile(Format('[%s] %s', [LevelStr[Level], Message]));
end;

procedure TFileLogger.LogSQL(const SQL: string);
begin
  WriteToFile('[SQL] ' + SQL);
end;

procedure TFileLogger.LogError(const ErrorMsg: string);
begin
  Log(llError, ErrorMsg);
end;

end.
```

### Intégration du logging dans l'EntityManager

```pascal
type
  TEntityManager = class
  private
    FConnection: TZConnection;
    FLogger: IORMLogger;
    // ...
  public
    constructor Create(Connection: TZConnection; Logger: IORMLogger = nil);
    // ...
  end;

constructor TEntityManager.Create(Connection: TZConnection; Logger: IORMLogger);
begin
  inherited Create;
  FConnection := Connection;
  FLogger := Logger;
  FInTransaction := False;
end;

procedure TEntityManager.Save(Entity: TEntityBase);
var
  Query: TZQuery;
  SQL: string;
begin
  if Entity.IsNew then
    SQL := TSQLMapper.GenerateInsertSQL(Entity)
  else if Entity.IsModified then
    SQL := TSQLMapper.GenerateUpdateSQL(Entity)
  else
    Exit;

  // Logger le SQL si disponible
  if Assigned(FLogger) then
    FLogger.LogSQL(SQL);

  Query := TZQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := SQL;
    SetParametersFromEntity(Query, Entity);
    Query.ExecSQL;

    Entity.IsNew := False;
    Entity.IsModified := False;

    if Assigned(FLogger) then
      FLogger.Log(llInfo, Format('Entity %s saved', [Entity.ClassName]));
  except
    on E: Exception do
    begin
      if Assigned(FLogger) then
        FLogger.LogError(Format('Error saving entity: %s', [E.Message]));
      raise;
    end;
  end;

  Query.Free;
end;
```

## Performance et optimisations avancées

### Batch Operations (opérations par lot)

```pascal
unit ORM.BatchOperations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, ORM.Base, ORM.EntityManager;

type
  TBatchProcessor = class
  private
    FEntityManager: TEntityManager;
    FBatchSize: Integer;
  public
    constructor Create(EntityManager: TEntityManager; BatchSize: Integer = 100);

    procedure SaveBatch<T: TEntityBase>(Entities: TList<T>);
    procedure DeleteBatch<T: TEntityBase>(IDs: TArray<Integer>);
  end;

implementation

constructor TBatchProcessor.Create(EntityManager: TEntityManager; BatchSize: Integer);
begin
  inherited Create;
  FEntityManager := EntityManager;
  FBatchSize := BatchSize;
end;

procedure TBatchProcessor.SaveBatch<T>(Entities: TList<T>);
var
  i, Count: Integer;
  Entity: T;
begin
  Count := 0;
  FEntityManager.BeginTransaction;
  try
    for Entity in Entities do
    begin
      FEntityManager.Save(Entity);
      Inc(Count);

      // Commit tous les N enregistrements
      if Count mod FBatchSize = 0 then
      begin
        FEntityManager.Commit;
        FEntityManager.BeginTransaction;
      end;
    end;

    FEntityManager.Commit;
  except
    FEntityManager.Rollback;
    raise;
  end;
end;

procedure TBatchProcessor.DeleteBatch<T>(IDs: TArray<Integer>);
var
  i, Count: Integer;
  ID: Integer;
begin
  Count := 0;
  FEntityManager.BeginTransaction;
  try
    for ID in IDs do
    begin
      FEntityManager.Delete<T>(ID);
      Inc(Count);

      if Count mod FBatchSize = 0 then
      begin
        FEntityManager.Commit;
        FEntityManager.BeginTransaction;
      end;
    end;

    FEntityManager.Commit;
  except
    FEntityManager.Rollback;
    raise;
  end;
end;

end.
```

### Utilisation du batch processor

```pascal
procedure ImporterClients(EM: TEntityManager; const CheminCSV: string);
var
  BatchProcessor: TBatchProcessor;
  Clients: TList<TClient>;
  CSV: TStringList;
  i: Integer;
  Client: TClient;
  Ligne, Champs: TStringArray;
begin
  Clients := TList<TClient>.Create;
  try
    // Lire le CSV
    CSV := TStringList.Create;
    try
      CSV.LoadFromFile(CheminCSV);

      for i := 1 to CSV.Count - 1 do  // Ignorer l'en-tête
      begin
        Champs := CSV[i].Split([';']);

        Client := TClient.Create;
        Client.Nom := Champs[0];
        Client.Prenom := Champs[1];
        Client.Email := Champs[2];
        Client.Ville := Champs[3];

        Clients.Add(Client);
      end;
    finally
      CSV.Free;
    end;

    // Sauvegarder par lots de 100
    BatchProcessor := TBatchProcessor.Create(EM, 100);
    try
      WriteLn(Format('Import de %d clients...', [Clients.Count]));
      BatchProcessor.SaveBatch<TClient>(Clients);
      WriteLn('Import terminé !');
    finally
      BatchProcessor.Free;
    end;
  finally
    for Client in Clients do
      Client.Free;
    Clients.Free;
  end;
end;
```

## Patterns de conception (Design Patterns)

### Repository Pattern

```pascal
unit Repositories.Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  ORM.EntityManager, Entities.Client;

type
  IClientRepository = interface
    ['{F6A7B8C9-D0E1-2345-FABC-456789012345}']
    function GetByID(ID: Integer): TClient;
    function GetAll: TList<TClient>;
    function GetByEmail(const Email: string): TClient;
    function GetByVille(const Ville: string): TList<TClient>;
    function GetActifs: TList<TClient>;
    procedure Save(Client: TClient);
    procedure Delete(Client: TClient);
  end;

  TClientRepository = class(TInterfacedObject, IClientRepository)
  private
    FEntityManager: TEntityManager;
  public
    constructor Create(EntityManager: TEntityManager);

    function GetByID(ID: Integer): TClient;
    function GetAll: TList<TClient>;
    function GetByEmail(const Email: string): TClient;
    function GetByVille(const Ville: string): TList<TClient>;
    function GetActifs: TList<TClient>;
    procedure Save(Client: TClient);
    procedure Delete(Client: TClient);
  end;

implementation

constructor TClientRepository.Create(EntityManager: TEntityManager);
begin
  inherited Create;
  FEntityManager := EntityManager;
end;

function TClientRepository.GetByID(ID: Integer): TClient;
begin
  Result := FEntityManager.Find<TClient>(ID);
end;

function TClientRepository.GetAll: TList<TClient>;
begin
  Result := FEntityManager.FindAll<TClient>();
end;

function TClientRepository.GetByEmail(const Email: string): TClient;
var
  Clients: TList<TClient>;
begin
  Result := nil;
  Clients := FEntityManager.FindAll<TClient>(
    'email = ' + QuotedStr(Email));
  try
    if Clients.Count > 0 then
      Result := Clients[0];
  finally
    if not Assigned(Result) then
    begin
      for var Client in Clients do
        Client.Free;
    end;
    Clients.Free;
  end;
end;

function TClientRepository.GetByVille(const Ville: string): TList<TClient>;
begin
  Result := FEntityManager.FindAll<TClient>(
    'ville = ' + QuotedStr(Ville));
end;

function TClientRepository.GetActifs: TList<TClient>;
begin
  Result := FEntityManager.FindAll<TClient>('actif = 1');
end;

procedure TClientRepository.Save(Client: TClient);
begin
  FEntityManager.Save(Client);
end;

procedure TClientRepository.Delete(Client: TClient);
begin
  FEntityManager.Delete(Client);
end;

end.
```

### Unit of Work Pattern

```pascal
unit ORM.UnitOfWork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  ORM.Base, ORM.EntityManager;

type
  TUnitOfWork = class
  private
    FEntityManager: TEntityManager;
    FNewEntities: TList<TEntityBase>;
    FModifiedEntities: TList<TEntityBase>;
    FDeletedEntities: TList<TEntityBase>;
  public
    constructor Create(EntityManager: TEntityManager);
    destructor Destroy; override;

    procedure RegisterNew(Entity: TEntityBase);
    procedure RegisterModified(Entity: TEntityBase);
    procedure RegisterDeleted(Entity: TEntityBase);
    procedure Commit;
    procedure Rollback;
  end;

implementation

constructor TUnitOfWork.Create(EntityManager: TEntityManager);
begin
  inherited Create;
  FEntityManager := EntityManager;
  FNewEntities := TList<TEntityBase>.Create;
  FModifiedEntities := TList<TEntityBase>.Create;
  FDeletedEntities := TList<TEntityBase>.Create;
end;

destructor TUnitOfWork.Destroy;
begin
  FNewEntities.Free;
  FModifiedEntities.Free;
  FDeletedEntities.Free;
  inherited Destroy;
end;

procedure TUnitOfWork.RegisterNew(Entity: TEntityBase);
begin
  FNewEntities.Add(Entity);
end;

procedure TUnitOfWork.RegisterModified(Entity: TEntityBase);
begin
  if not FModifiedEntities.Contains(Entity) then
    FModifiedEntities.Add(Entity);
end;

procedure TUnitOfWork.RegisterDeleted(Entity: TEntityBase);
begin
  FDeletedEntities.Add(Entity);
end;

procedure TUnitOfWork.Commit;
var
  Entity: TEntityBase;
begin
  FEntityManager.BeginTransaction;
  try
    // Sauvegarder les nouvelles entités
    for Entity in FNewEntities do
      FEntityManager.Save(Entity);

    // Mettre à jour les entités modifiées
    for Entity in FModifiedEntities do
      FEntityManager.Save(Entity);

    // Supprimer les entités marquées
    for Entity in FDeletedEntities do
      FEntityManager.Delete(Entity);

    FEntityManager.Commit;

    // Nettoyer les listes
    FNewEntities.Clear;
    FModifiedEntities.Clear;
    FDeletedEntities.Clear;
  except
    FEntityManager.Rollback;
    raise;
  end;
end;

procedure TUnitOfWork.Rollback;
begin
  FNewEntities.Clear;
  FModifiedEntities.Clear;
  FDeletedEntities.Clear;
end;

end.
```

### Utilisation du Unit of Work

```pascal
procedure TraiterPlusieursOperations(EM: TEntityManager);
var
  UoW: TUnitOfWork;
  Client1, Client2: TClient;
begin
  UoW := TUnitOfWork.Create(EM);
  try
    // Créer un nouveau client
    Client1 := TClient.Create;
    Client1.Nom := 'Nouveau';
    Client1.Email := 'nouveau@test.com';
    UoW.RegisterNew(Client1);

    // Modifier un client existant
    Client2 := EM.Find<TClient>(5);
    if Assigned(Client2) then
    begin
      Client2.Ville := 'Lyon';
      UoW.RegisterModified(Client2);
    end;

    // Valider toutes les opérations en une seule transaction
    UoW.Commit;

    WriteLn('Toutes les opérations ont été validées');
  except
    on E: Exception do
    begin
      UoW.Rollback;
      WriteLn('Erreur, toutes les opérations ont été annulées: ', E.Message);
      raise;
    end;
  end;

  UoW.Free;
  Client1.Free;
  if Assigned(Client2) then
    Client2.Free;
end;
```

## Exemple d'application complète

### Structure du projet

```
MonAppORM/
├── src/
│   ├── ORM/
│   │   ├── ORM.Base.pas
│   │   ├── ORM.EntityManager.pas
│   │   ├── ORM.SQLMapper.pas
│   │   ├── ORM.Validation.pas
│   │   └── ORM.Logger.pas
│   ├── Entities/
│   │   ├── Entities.Client.pas
│   │   └── Entities.Commande.pas
│   ├── Repositories/
│   │   ├── Repositories.Client.pas
│   │   └── Repositories.Commande.pas
│   ├── Services/
│   │   └── Services.Client.pas
│   └── Forms/
│       └── MainForm.pas
└── MonAppORM.lpr
```

### Service métier

```pascal
unit Services.Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  ORM.EntityManager, Entities.Client, Repositories.Client;

type
  TClientService = class
  private
    FClientRepository: IClientRepository;
  public
    constructor Create(Repository: IClientRepository);

    function CreerClient(const Nom, Prenom, Email, Ville: string): TClient;
    function RechercherParEmail(const Email: string): TClient;
    function ObtenirClientsParVille(const Ville: string): TList<TClient>;
    procedure EnvoyerEmailBienvenue(Client: TClient);
    procedure DesactiverClient(ClientID: Integer);
  end;

implementation

constructor TClientService.Create(Repository: IClientRepository);
begin
  inherited Create;
  FClientRepository := Repository;
end;

function TClientService.CreerClient(const Nom, Prenom, Email, Ville: string): TClient;
var
  ValidationResult: TValidationResult;
begin
  Result := TClient.Create;
  Result.Nom := Nom;
  Result.Prenom := Prenom;
  Result.Email := Email;
  Result.Ville := Ville;

  // Valider
  ValidationResult := Result.Validate;
  try
    if not ValidationResult.IsValid then
      raise Exception.Create('Données invalides: ' +
        ValidationResult.GetErrorsAsString);
  finally
    ValidationResult.Free;
  end;

  // Vérifier unicité de l'email
  if Assigned(FClientRepository.GetByEmail(Email)) then
    raise Exception.Create('Un client avec cet email existe déjà');

  // Sauvegarder
  FClientRepository.Save(Result);

  // Envoyer email de bienvenue
  EnvoyerEmailBienvenue(Result);
end;

function TClientService.RechercherParEmail(const Email: string): TClient;
begin
  Result := FClientRepository.GetByEmail(Email);
end;

function TClientService.ObtenirClientsParVille(const Ville: string): TList<TClient>;
begin
  Result := FClientRepository.GetByVille(Ville);
end;

procedure TClientService.EnvoyerEmailBienvenue(Client: TClient);
begin
  // Logique d'envoi d'email (simplifié)
  WriteLn(Format('Email de bienvenue envoyé à %s', [Client.Email]));
end;

procedure TClientService.DesactiverClient(ClientID: Integer);
var
  Client: TClient;
begin
  Client := FClientRepository.GetByID(ClientID);
  if Assigned(Client) then
  try
    Client.Actif := False;
    FClientRepository.Save(Client);
  finally
    Client.Free;
  end;
end;

end.
```

### Application principale

```pascal
program MonAppORM;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, ZConnection,
  ORM.EntityManager, ORM.Logger,
  Entities.Client, Repositories.Client, Services.Client;

var
  Connection: TZConnection;
  EntityManager: TEntityManager;
  Logger: IORMLogger;
  ClientRepo: IClientRepository;
  ClientService: TClientService;
  Client: TClient;

procedure ConfigurerConnexion;
begin
  Connection := TZConnection.Create(nil);
  Connection.Protocol := 'postgresql-9';
  Connection.HostName := 'localhost';
  Connection.Database := 'gestion_clients';
  Connection.User := 'postgres';
  Connection.Password := 'password';

  {$IFDEF WINDOWS}
  Connection.LibraryLocation := 'libs\libpq.dll';
  {$ENDIF}

  Connection.Connected := True;
  WriteLn('Connecté à la base de données');
end;

procedure DemonstrationORM;
begin
  WriteLn('=== Création d''un client ===');
  Client := ClientService.CreerClient(
    'Martin', 'Sophie', 'sophie.martin@example.com', 'Paris');
  try
    WriteLn('Client créé avec ID: ', Client.ID);
  finally
    Client.Free;
  end;

  WriteLn;
  WriteLn('=== Recherche par email ===');
  Client := ClientService.RechercherParEmail('sophie.martin@example.com');
  if Assigned(Client) then
  try
    WriteLn('Client trouvé: ', Client.Nom, ' ', Client.Prenom);
    WriteLn('Ville: ', Client.Ville);
  finally
    Client.Free;
  end
  else
    WriteLn('Client non trouvé');

  WriteLn;
  WriteLn('=== Clients de Paris ===');
  var ClientsParis := ClientService.ObtenirClientsParVille('Paris');
  try
    WriteLn('Nombre de clients à Paris: ', ClientsParis.Count);
    for Client in ClientsParis do
      WriteLn('  - ', Client.Prenom, ' ', Client.Nom);
  finally
    for Client in ClientsParis do
      Client.Free;
    ClientsParis.Free;
  end;
end;

begin
  try
    // Configuration
    ConfigurerConnexion;
    Logger := TFileLogger.Create('orm.log');
    EntityManager := TEntityManager.Create(Connection, Logger);
    ClientRepo := TClientRepository.Create(EntityManager);
    ClientService := TClientService.Create(ClientRepo);

    try
      // Démonstration
      DemonstrationORM;

      WriteLn;
      WriteLn('Appuyez sur Entrée pour quitter...');
      ReadLn;
    finally
      ClientService.Free;
      EntityManager.Free;
      Connection.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur: ', E.ClassName, ' - ', E.Message);
      ReadLn;
    end;
  end;
end.
```

## Comparaison avec les ORMs existants

| Fonctionnalité | Notre ORM | mORMot | tiOPF |
|----------------|-----------|---------|-------|
| Simplicité | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐ |
| Performance | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| Fonctionnalités | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| Documentation | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ |
| Courbe d'apprentissage | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐ |
| Multi-plateforme | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| Personnalisation | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐ |

## Évolutions possibles

### Fonctionnalités à ajouter

1. **Relations automatiques** (One-to-Many, Many-to-Many)
2. **Héritage d'entités** (Table per Class, Table per Hierarchy)
3. **Eager vs Lazy Loading** configurable
4. **Query caching** pour améliorer les performances
5. **Second-level cache** avec Redis ou Memcached
6. **Change tracking** automatique
7. **Optimistic locking** pour la concurrence
8. **Soft delete** (suppression logique)
9. **Audit trail** (historique des modifications)
10. **Full-text search** intégré

### Exemple de relation Many-to-One

```pascal
type
  [TTable('commandes')]
  TCommande = class(TEntityBase)
  private
    FID: Integer;
    FClientID: Integer;
    FClient: TClient;  // Relation
    FClientLoaded: Boolean;

    function GetClient: TClient;
  published
    property ClientID: Integer read FClientID write FClientID;
    property Client: TClient read GetClient;
  end;

function TCommande.GetClient: TClient;
begin
  if not FClientLoaded and Assigned(EntityManager) then
  begin
    FClient := TEntityManager(EntityManager).Find<TClient>(FClientID);
    FClientLoaded := True;
  end;
  Result := FClient;
end;
```

## Conclusion

Développer son propre ORM est un excellent exercice d'apprentissage qui offre :

✅ **Compréhension profonde** des mécanismes de persistance  
✅ **Contrôle total** sur le comportement et les performances  
✅ **Adaptation** aux besoins spécifiques de votre projet  
✅ **Légèreté** - Seulement les fonctionnalités nécessaires  
✅ **Portabilité** - Conçu pour Windows et Linux dès le départ  
✅ **Maintenance** - Code que vous maîtrisez complètement

### Points clés à retenir

1. **Utilisez RTTI** pour l'introspection et le mapping automatique
2. **Générez le SQL** dynamiquement plutôt que manuellement
3. **Gérez le cycle de vie** des entités (New, Modified, Deleted)
4. **Implémentez les transactions** pour garantir l'intégrité
5. **Validez les données** avant la persistance
6. **Loggez les opérations** pour faciliter le débogage
7. **Testez unitairement** chaque composant
8. **Documentez** votre code et les conventions

### Quand utiliser un ORM personnalisé vs existant ?

**Utilisez votre propre ORM si :**
- Projet de taille petite à moyenne
- Besoins spécifiques non couverts par les ORMs existants
- Volonté d'apprendre et de maîtriser la technologie
- Besoin de contrôle total sur les performances

**Utilisez un ORM existant (mORMot, tiOPF) si :**
- Projet complexe avec beaucoup de relations
- Besoin de fonctionnalités avancées (cache, clustering, etc.)
- Équipe grande avec besoin de standards reconnus
- Priorité sur la productivité immédiate

Notre ORM personnalisé offre une base solide et extensible pour la plupart des applications de gestion de données, tout en restant simple à comprendre et à maintenir. Il est entièrement portable entre Windows et Linux grâce à l'utilisation de ZEOS et des abstractions appropriées.

## Ressources complémentaires

- **Documentation FreePascal RTTI** : https://www.freepascal.org/docs-html/rtl/typinfo/index.html
- **ZEOS Documentation** : https://zeoslib.sourceforge.io/
- **Design Patterns** : Gang of Four (GoF) patterns
- **Domain-Driven Design** : Eric Evans
- **Patterns of Enterprise Application Architecture** : Martin Fowler

Avec ces connaissances, vous êtes maintenant capable de créer, maintenir et faire évoluer un ORM personnalisé adapté à vos besoins spécifiques, fonctionnant de manière identique sur Windows et Linux !Qu'est-ce qu'un ORM ?


⏭️ [tiOPF - Framework de persistance objet](/08-bases-donnees-orm-multiplatefomes/06-tiopf-framework-persistance-objet.md)
