🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.6 tiOPF - Framework de persistance objet

## Introduction

**tiOPF** (TechInsite Object Persistence Framework) est un framework mature et robuste de persistance objet pour FreePascal et Delphi. Développé depuis plus de 20 ans, il offre une solution complète et éprouvée pour gérer la persistance des objets métier dans vos applications.

### Qu'est-ce que tiOPF ?

tiOPF est bien plus qu'un simple ORM. C'est un **framework complet** qui fournit :

- **Persistance objet** : Sauvegarde et chargement automatiques d'objets
- **Pattern Model-View-Controller (MVC)** : Séparation claire des responsabilités
- **Système de validation** : Règles métier intégrées
- **Gestion des transactions** : Support complet des transactions
- **Mapping objet-relationnel** : Liaison automatique avec la base de données
- **Support multi-SGBD** : PostgreSQL, MySQL, SQLite, Firebird, Oracle, etc.
- **Multi-plateforme** : Windows, Linux, macOS

### Philosophie de tiOPF

tiOPF suit le principe **"Business Objects First"** :
- Vous commencez par concevoir vos objets métier
- Le framework gère ensuite leur persistance
- Votre code métier reste propre et découplé de la base de données

### Différences avec un ORM classique

| Aspect | ORM classique | tiOPF |
|--------|---------------|-------|
| Approche | Base de données → Objets | Objets → Base de données |
| Focus | Mapping tables/classes | Logique métier |
| Architecture | Simple couche de persistance | Framework MVC complet |
| Complexité | Moyenne | Plus élevée (mais plus puissant) |
| Courbe d'apprentissage | Rapide | Moyenne |

## Installation de tiOPF

### Sous Windows

#### Méthode 1 : Téléchargement manuel

1. Télécharger tiOPF depuis : https://github.com/graemeg/tiopf
2. Extraire l'archive (par exemple dans `C:\Dev\tiOPF\`)
3. Dans Lazarus : **Paquets → Ouvrir un fichier de paquet (.lpk)**
4. Naviguer vers : `tiOPF\Source\tiOPF.lpk`
5. Cliquer sur **Compiler** puis **Utiliser → Installer**
6. Redémarrer Lazarus

#### Méthode 2 : Git

```bash
cd C:\Dev  
git clone https://github.com/graemeg/tiopf.git
```

### Sous Ubuntu/Linux

#### Installation avec Git

```bash
cd ~/Developpement  
git clone https://github.com/graemeg/tiopf.git  
cd tiopf/Source
# Ouvrir tiOPF.lpk dans Lazarus et compiler
```

### Vérification de l'installation

Après redémarrage de Lazarus, vérifiez dans **Paquets → Paquets installés** que vous voyez :
- `tiOPF` (package principal)
- `tiOPFCore`
- `tiOPFGUI` (composants visuels)

## Architecture de tiOPF

### Les composants principaux

```
┌─────────────────────────────────────────┐
│   Interface utilisateur (GUI)           │
├─────────────────────────────────────────┤
│   TtiObject (Objets métier)             │
│   - Propriétés                          │
│   - Validation                          │
│   - Logique métier                      │
├─────────────────────────────────────────┤
│   TtiObjectList (Collections)           │
│   - Gestion de listes d'objets          │
├─────────────────────────────────────────┤
│   TtiVisitor (Persistance)              │
│   - Save, Load, Delete                  │
├─────────────────────────────────────────┤
│   TPersistenceLayer (Couche de données) │
│   - Mapping objet/table                 │
├─────────────────────────────────────────┤
│   Base de données                       │
└─────────────────────────────────────────┘
```

### Classes fondamentales

#### TtiObject
La classe de base pour tous vos objets métier.

```pascal
TtiObject = class
  - ObjectState: Lecture, Insertion, Modification, Suppression
  - Dirty: Indicateur de modification
  - Valid: Résultat de la validation
  - Read/Write: Persistance
end;
```

#### TtiObjectList
Collection d'objets métier avec fonctionnalités avancées.

```pascal
TtiObjectList = class
  - Add/Remove/Find: Gestion d'objets
  - Sort/Filter: Tri et filtrage
  - Save/Load: Persistance de la collection
end;
```

#### TtiVisitor
Pattern Visitor pour les opérations de persistance.

```pascal
TtiVisitor = class
  - AcceptVisitor: Méthode d'acceptation
  - Read/Write: Implémentations concrètes
end;
```

## Création de votre première classe métier

### Définition d'une entité Client

```pascal
unit Model.Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject, tiOPFManager;

type
  // Classe représentant un client
  TClient = class(TtiObject)
  private
    FNom: string;
    FPrenom: string;
    FEmail: string;
    FTelephone: string;
    FVille: string;
    FDateInscription: TDateTime;
    FActif: Boolean;

    procedure SetNom(const AValue: string);
    procedure SetPrenom(const AValue: string);
    procedure SetEmail(const AValue: string);
  protected
    // Validation des règles métier
    procedure DoValidate(const AErrors: TtiObjectErrors); override;
  public
    constructor Create; override;

    // Méthodes métier
    function NomComplet: string;
    procedure Activer;
    procedure Desactiver;
  published
    // Propriétés publiées pour la persistance
    property Nom: string read FNom write SetNom;
    property Prenom: string read FPrenom write SetPrenom;
    property Email: string read FEmail write SetEmail;
    property Telephone: string read FTelephone write FTelephone;
    property Ville: string read FVille write FVille;
    property DateInscription: TDateTime read FDateInscription write FDateInscription;
    property Actif: Boolean read FActif write FActif;
  end;

  // Liste de clients
  TClientList = class(TtiObjectList)
  private
    function GetItems(Index: Integer): TClient;
  public
    property Items[Index: Integer]: TClient read GetItems; default;
  end;

implementation

uses
  tiUtils;

{ TClient }

constructor TClient.Create;  
begin
  inherited Create;
  FActif := True;
  FDateInscription := Now;
end;

procedure TClient.SetNom(const AValue: string);  
begin
  if FNom <> AValue then
  begin
    FNom := AValue;
    Dirty := True;  // Marquer l'objet comme modifié
  end;
end;

procedure TClient.SetPrenom(const AValue: string);  
begin
  if FPrenom <> AValue then
  begin
    FPrenom := AValue;
    Dirty := True;
  end;
end;

procedure TClient.SetEmail(const AValue: string);  
begin
  if FEmail <> AValue then
  begin
    FEmail := AValue;
    Dirty := True;
  end;
end;

procedure TClient.DoValidate(const AErrors: TtiObjectErrors);  
begin
  inherited DoValidate(AErrors);

  // Validation du nom
  if Trim(FNom) = '' then
    AErrors.AddError('Nom', 'Le nom est obligatoire');

  if Length(FNom) < 2 then
    AErrors.AddError('Nom', 'Le nom doit contenir au moins 2 caractères');

  // Validation du prénom
  if Trim(FPrenom) = '' then
    AErrors.AddError('Prenom', 'Le prénom est obligatoire');

  // Validation de l'email
  if Trim(FEmail) = '' then
    AErrors.AddError('Email', 'L''email est obligatoire')
  else if not tiIsValidEmail(FEmail) then
    AErrors.AddError('Email', 'Format d''email invalide');
end;

function TClient.NomComplet: string;  
begin
  Result := FPrenom + ' ' + FNom;
end;

procedure TClient.Activer;  
begin
  FActif := True;
  Dirty := True;
end;

procedure TClient.Desactiver;  
begin
  FActif := False;
  Dirty := True;
end;

{ TClientList }

function TClientList.GetItems(Index: Integer): TClient;  
begin
  Result := TClient(inherited Items[Index]);
end;

end.
```

## Configuration de la persistance

### Création du mapping objet-table

```pascal
unit Persistence.ClientMapping;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject, tiVisitorDB, Model.Client;

type
  // Visitor pour lire/écrire les clients
  TVisClient = class(TtiVisitorDB)
  protected
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  public
    procedure Read(const AObject: TtiObject); override;
    procedure Update(const AObject: TtiObject); override;
    procedure Insert(const AObject: TtiObject); override;
    procedure Delete(const AObject: TtiObject); override;
  end;

implementation

{ TVisClient }

procedure TVisClient.MapRowToObject;  
var
  LClient: TClient;
begin
  LClient := TClient(OwnerObject);

  // Mapper les champs de la base vers les propriétés
  LClient.OID.AssignFromTIQuery('OID', Query);
  LClient.Nom := Query.FieldAsString['nom'];
  LClient.Prenom := Query.FieldAsString['prenom'];
  LClient.Email := Query.FieldAsString['email'];
  LClient.Telephone := Query.FieldAsString['telephone'];
  LClient.Ville := Query.FieldAsString['ville'];
  LClient.DateInscription := Query.FieldAsDateTime['date_inscription'];
  LClient.Actif := Query.FieldAsBoolean['actif'];

  LClient.ObjectState := posClean;  // L'objet est maintenant propre
end;

procedure TVisClient.SetupParams;  
var
  LClient: TClient;
begin
  LClient := TClient(OwnerObject);

  // Définir les paramètres pour l'insertion/modification
  Query.ParamAsString['nom'] := LClient.Nom;
  Query.ParamAsString['prenom'] := LClient.Prenom;
  Query.ParamAsString['email'] := LClient.Email;
  Query.ParamAsString['telephone'] := LClient.Telephone;
  Query.ParamAsString['ville'] := LClient.Ville;
  Query.ParamAsDateTime['date_inscription'] := LClient.DateInscription;
  Query.ParamAsBoolean['actif'] := LClient.Actif;
end;

procedure TVisClient.Read(const AObject: TtiObject);  
begin
  Query.SQL.Text :=
    'SELECT * FROM clients WHERE oid = :oid';
  Query.ParamAsString['oid'] := AObject.OID.AsString;
  Query.Open;

  if not Query.EOF then
    MapRowToObject;
end;

procedure TVisClient.Update(const AObject: TtiObject);  
begin
  Query.SQL.Text :=
    'UPDATE clients SET ' +
    '  nom = :nom, ' +
    '  prenom = :prenom, ' +
    '  email = :email, ' +
    '  telephone = :telephone, ' +
    '  ville = :ville, ' +
    '  date_inscription = :date_inscription, ' +
    '  actif = :actif ' +
    'WHERE oid = :oid';

  Query.ParamAsString['oid'] := AObject.OID.AsString;
  SetupParams;
  Query.ExecSQL;
end;

procedure TVisClient.Insert(const AObject: TtiObject);  
begin
  AObject.OID.CreateOID;  // Générer un nouvel OID

  Query.SQL.Text :=
    'INSERT INTO clients (oid, nom, prenom, email, telephone, ' +
    '                     ville, date_inscription, actif) ' +
    'VALUES (:oid, :nom, :prenom, :email, :telephone, ' +
    '        :ville, :date_inscription, :actif)';

  Query.ParamAsString['oid'] := AObject.OID.AsString;
  SetupParams;
  Query.ExecSQL;
end;

procedure TVisClient.Delete(const AObject: TtiObject);  
begin
  Query.SQL.Text := 'DELETE FROM clients WHERE oid = :oid';
  Query.ParamAsString['oid'] := AObject.OID.AsString;
  Query.ExecSQL;
end;

end.
```

### Enregistrement des mappings

```pascal
unit Persistence.Mappings;

{$mode objfpc}{$H+}

interface

procedure RegisterMappings;

implementation

uses
  tiOPFManager, Model.Client, Persistence.ClientMapping;

procedure RegisterMappings;  
begin
  // Enregistrer le visitor pour la classe TClient
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(
    TClient,
    'clients',  // Nom de la table
    'oid',      // Champ clé primaire
    TVisClient  // Classe visitor
  );

  // Enregistrer pour la liste
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(
    TClientList,
    'clients',
    'oid',
    TVisClient
  );
end;

end.
```

## Configuration de la base de données

### Initialisation de la couche de persistance

```pascal
unit AppConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiOPFManager, tiPersistenceLayers;

procedure InitializePersistenceLayer;  
procedure FinalizePersistenceLayer;

implementation

uses
  Persistence.Mappings;

procedure InitializePersistenceLayer;  
begin
  // Créer le gestionnaire global si nécessaire
  if not Assigned(GTIOPFManager) then
    GTIOPFManager := TtiOPFManager.Create;

  {$IFDEF WINDOWS}
  // Configuration Windows avec PostgreSQL
  GTIOPFManager.ConnectDatabase(
    'PostgreSQL',              // Type de base de données
    'localhost',               // Serveur
    'ma_base',                 // Nom de la base
    'postgres',                // Utilisateur
    'password',                // Mot de passe
    ''                         // Paramètres additionnels
  );
  {$ENDIF}

  {$IFDEF LINUX}
  // Configuration Linux (identique, mais peut utiliser socket Unix)
  GTIOPFManager.ConnectDatabase(
    'PostgreSQL',
    'localhost',
    'ma_base',
    'postgres',
    'password',
    'ClientEncoding=UTF8'
  );
  {$ENDIF}

  // Enregistrer tous les mappings
  RegisterMappings;

  WriteLn('Couche de persistance initialisée');
end;

procedure FinalizePersistenceLayer;  
begin
  if Assigned(GTIOPFManager) then
  begin
    GTIOPFManager.DisconnectDatabase;
    FreeAndNil(GTIOPFManager);
  end;
end;

end.
```

### Configuration multi-SGBD

```pascal
procedure ConfigurerMySQL;  
begin
  GTIOPFManager.ConnectDatabase(
    'MySQL',
    'localhost',
    'ma_base',
    'root',
    'password',
    'Port=3306'
  );
end;

procedure ConfigurerSQLite;  
begin
  GTIOPFManager.ConnectDatabase(
    'SQLite',
    '',  // Pas de serveur pour SQLite
    ExtractFilePath(ParamStr(0)) + 'data' + PathDelim + 'ma_base.db',
    '',  // Pas d'utilisateur
    '',  // Pas de mot de passe
    ''
  );
end;

procedure ConfigurerFirebird;  
begin
  GTIOPFManager.ConnectDatabase(
    'Firebird',
    'localhost',
    'C:\Bases\ma_base.fdb',  // Windows
    // '/var/lib/firebird/ma_base.fdb',  // Linux
    'SYSDBA',
    'masterkey',
    'Charset=UTF8'
  );
end;
```

## Utilisation de base - Opérations CRUD

### Créer un client (INSERT)

```pascal
procedure CreerNouveauClient;  
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

    // Valider
    if not Client.IsValid then
    begin
      WriteLn('Erreurs de validation:');
      WriteLn(Client.Errors.ToString);
      Exit;
    end;

    // Sauvegarder
    Client.Save;
    WriteLn('Client créé avec OID: ', Client.OID.AsString);
  finally
    Client.Free;
  end;
end;
```

### Lire un client (SELECT)

```pascal
procedure ChargerClient(const AOID: string);  
var
  Client: TClient;
begin
  Client := TClient.Create;
  try
    Client.OID.AsString := AOID;
    Client.Read;

    WriteLn('Client chargé:');
    WriteLn('Nom: ', Client.NomComplet);
    WriteLn('Email: ', Client.Email);
    WriteLn('Ville: ', Client.Ville);
  finally
    Client.Free;
  end;
end;
```

### Modifier un client (UPDATE)

```pascal
procedure ModifierClient(const AOID: string);  
var
  Client: TClient;
begin
  Client := TClient.Create;
  try
    // Charger le client
    Client.OID.AsString := AOID;
    Client.Read;

    // Modifier
    Client.Email := 'nouveau.email@example.com';
    Client.Ville := 'Lyon';

    // Les setters ont marqué l'objet comme Dirty
    if Client.Dirty then
    begin
      if Client.IsValid then
      begin
        Client.Save;
        WriteLn('Client mis à jour');
      end
      else
        WriteLn('Erreurs: ', Client.Errors.ToString);
    end;
  finally
    Client.Free;
  end;
end;
```

### Supprimer un client (DELETE)

```pascal
procedure SupprimerClient(const AOID: string);  
var
  Client: TClient;
begin
  Client := TClient.Create;
  try
    Client.OID.AsString := AOID;
    Client.Read;

    if MessageDlg('Confirmation',
       Format('Supprimer %s ?', [Client.NomComplet]),
       mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Client.Delete;
      WriteLn('Client supprimé');
    end;
  finally
    Client.Free;
  end;
end;
```

## Travail avec des listes

### Charger tous les clients

```pascal
procedure ChargerTousLesClients;  
var
  Clients: TClientList;
  Client: TClient;
begin
  Clients := TClientList.Create;
  try
    Clients.Read;  // Charge tous les clients

    WriteLn(Format('Nombre de clients: %d', [Clients.Count]));

    for Client in Clients do
      WriteLn(Format('%s - %s', [Client.NomComplet, Client.Email]));
  finally
    Clients.Free;
  end;
end;
```

### Charger avec critères

```pascal
procedure ChargerClientsParVille(const AVille: string);  
var
  Clients: TClientList;
  Client: TClient;
begin
  Clients := TClientList.Create;
  try
    // Définir les critères de recherche
    Clients.Criteria.AddEqualTo('ville', AVille);
    Clients.Read;

    WriteLn(Format('Clients à %s: %d', [AVille, Clients.Count]));

    for Client in Clients do
      WriteLn('  - ', Client.NomComplet);
  finally
    Clients.Free;
  end;
end;
```

### Recherche avancée avec critères multiples

```pascal
procedure RechercheAvancee;  
var
  Clients: TClientList;
begin
  Clients := TClientList.Create;
  try
    // Critères multiples avec opérateurs
    Clients.Criteria.AddEqualTo('ville', 'Paris');
    Clients.Criteria.AddEqualTo('actif', True);
    Clients.Criteria.AddGreaterThan('date_inscription', EncodeDate(2024, 1, 1));

    Clients.Read;

    WriteLn(Format('Clients trouvés: %d', [Clients.Count]));
  finally
    Clients.Free;
  end;
end;
```

## Tri et filtrage

### Tri d'une liste

```pascal
procedure TrierClients;  
var
  Clients: TClientList;
begin
  Clients := TClientList.Create;
  try
    Clients.Read;

    // Trier par nom
    Clients.Sort('Nom');
    WriteLn('=== Tri par nom ===');
    for var Client in Clients do
      WriteLn(Client.Nom);

    // Trier par ville puis nom
    Clients.Sort('Ville,Nom');
    WriteLn('=== Tri par ville puis nom ===');
    for var Client in Clients do
      WriteLn(Format('%s - %s', [Client.Ville, Client.Nom]));
  finally
    Clients.Free;
  end;
end;
```

### Filtrage côté client

```pascal
procedure FiltrerListe;  
var
  Clients, ClientsFiltres: TClientList;
  Client: TClient;
begin
  Clients := TClientList.Create;
  try
    Clients.Read;

    ClientsFiltres := TClientList.Create;
    try
      // Filtrer les clients actifs de Paris
      for Client in Clients do
      begin
        if (Client.Actif) and (Client.Ville = 'Paris') then
          ClientsFiltres.Add(Client.Clone as TClient);
      end;

      WriteLn(Format('Clients actifs à Paris: %d', [ClientsFiltres.Count]));
    finally
      ClientsFiltres.Free;
    end;
  finally
    Clients.Free;
  end;
end;
```

## Transactions

### Transaction simple

```pascal
procedure TransactionSimple;  
var
  Client1, Client2: TClient;
begin
  GTIOPFManager.BeginTransaction;
  try
    // Créer le premier client
    Client1 := TClient.Create;
    try
      Client1.Nom := 'Martin';
      Client1.Prenom := 'Sophie';
      Client1.Email := 'sophie@test.com';
      Client1.Save;
    finally
      Client1.Free;
    end;

    // Créer le deuxième client
    Client2 := TClient.Create;
    try
      Client2.Nom := 'Bernard';
      Client2.Prenom := 'Pierre';
      Client2.Email := 'pierre@test.com';
      Client2.Save;
    finally
      Client2.Free;
    end;

    // Valider la transaction
    GTIOPFManager.Commit;
    WriteLn('Transaction validée');
  except
    on E: Exception do
    begin
      GTIOPFManager.Rollback;
      WriteLn('Transaction annulée: ', E.Message);
      raise;
    end;
  end;
end;
```

### Transaction avec sauvegarde de liste

```pascal
procedure SauvegarderListe;  
var
  Clients: TClientList;
  Client: TClient;
  i: Integer;
begin
  Clients := TClientList.Create;
  try
    // Créer plusieurs clients
    for i := 1 to 10 do
    begin
      Client := TClient.Create;
      Client.Nom := 'Client' + IntToStr(i);
      Client.Prenom := 'Prenom' + IntToStr(i);
      Client.Email := Format('client%d@test.com', [i]);
      Client.Ville := 'Paris';
      Clients.Add(Client);
    end;

    // Sauvegarder toute la liste en une transaction
    Clients.Save;
    WriteLn(Format('%d clients sauvegardés', [Clients.Count]));
  finally
    Clients.Free;
  end;
end;
```

## Validation avancée

### Règles de validation personnalisées

```pascal
procedure TClient.DoValidate(const AErrors: TtiObjectErrors);  
begin
  inherited DoValidate(AErrors);

  // Validation basique
  if Trim(FNom) = '' then
    AErrors.AddError('Nom', 'Le nom est obligatoire');

  // Validation avec règle métier
  if (FDateInscription > Now) then
    AErrors.AddError('DateInscription',
      'La date d''inscription ne peut pas être dans le futur');

  // Validation conditionnelle
  if FActif and (Trim(FEmail) = '') then
    AErrors.AddError('Email',
      'L''email est obligatoire pour un client actif');

  // Validation avec vérification en base (exemple)
  if not EmailUniqueEnBase(FEmail) then
    AErrors.AddError('Email',
      'Cet email est déjà utilisé par un autre client');
end;

function TClient.EmailUniqueEnBase(const AEmail: string): Boolean;  
var
  Clients: TClientList;
begin
  Result := True;
  Clients := TClientList.Create;
  try
    Clients.Criteria.AddEqualTo('email', AEmail);
    Clients.Read;

    // Si on trouve un client avec cet email et que ce n'est pas nous
    if (Clients.Count > 0) and
       (Clients[0].OID.AsString <> Self.OID.AsString) then
      Result := False;
  finally
    Clients.Free;
  end;
end;
```

### Utilisation de la validation

```pascal
procedure ValiderAvantSauvegarde;  
var
  Client: TClient;
  Error: TtiObjectError;
begin
  Client := TClient.Create;
  try
    Client.Nom := 'D';  // Trop court
    Client.Prenom := '';  // Manquant
    Client.Email := 'email-invalide';  // Format invalide

    if not Client.IsValid then
    begin
      WriteLn('Erreurs de validation:');
      for Error in Client.Errors do
        WriteLn(Format('  %s: %s', [Error.PropertyName, Error.ErrorMessage]));
    end
    else
      Client.Save;
  finally
    Client.Free;
  end;
end;
```

## Intégration avec l'interface graphique

### Liaison avec les composants visuels

tiOPF fournit des composants pour lier facilement vos objets métier à l'interface :

```pascal
unit FormClients;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DBGrids,
  ExtCtrls, tiObject, tiOPFManager, Model.Client;

type
  TFormClients = class(TForm)
    ButtonNouveau: TButton;
    ButtonModifier: TButton;
    ButtonSupprimer: TButton;
    ButtonActualiser: TButton;
    GridClients: TStringGrid;
    PanelTop: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonNouveauClick(Sender: TObject);
    procedure ButtonModifierClick(Sender: TObject);
    procedure ButtonSupprimerClick(Sender: TObject);
    procedure ButtonActualiserClick(Sender: TObject);
  private
    FClients: TClientList;
    procedure ChargerClients;
    procedure AfficherClients;
  end;

var
  FormClients: TFormClients;

implementation

{$R *.lfm}

procedure TFormClients.FormCreate(Sender: TObject);  
begin
  FClients := TClientList.Create;
  ChargerClients;

  // Configurer la grille
  GridClients.ColCount := 5;
  GridClients.RowCount := 1;
  GridClients.Cells[0, 0] := 'Nom';
  GridClients.Cells[1, 0] := 'Prénom';
  GridClients.Cells[2, 0] := 'Email';
  GridClients.Cells[3, 0] := 'Ville';
  GridClients.Cells[4, 0] := 'Actif';

  AfficherClients;
end;

procedure TFormClients.FormDestroy(Sender: TObject);  
begin
  FClients.Free;
end;

procedure TFormClients.ChargerClients;  
begin
  FClients.Clear;
  FClients.Read;
end;

procedure TFormClients.AfficherClients;  
var
  i: Integer;
  Client: TClient;
begin
  GridClients.RowCount := FClients.Count + 1;

  for i := 0 to FClients.Count - 1 do
  begin
    Client := FClients[i];
    GridClients.Cells[0, i + 1] := Client.Nom;
    GridClients.Cells[1, i + 1] := Client.Prenom;
    GridClients.Cells[2, i + 1] := Client.Email;
    GridClients.Cells[3, i + 1] := Client.Ville;
    GridClients.Cells[4, i + 1] := IfThen(Client.Actif, 'Oui', 'Non');
    GridClients.Objects[0, i + 1] := Client;  // Stocker la référence
  end;
end;

procedure TFormClients.ButtonNouveauClick(Sender: TObject);  
var
  FormEdition: TFormEditionClient;
  Client: TClient;
begin
  FormEdition := TFormEditionClient.Create(Self);
  try
    if FormEdition.ShowModal = mrOK then
    begin
      Client := FormEdition.Client;
      FClients.Add(Client);
      AfficherClients;
    end;
  finally
    FormEdition.Free;
  end;
end;

procedure TFormClients.ButtonModifierClick(Sender: TObject);  
var
  FormEdition: TFormEditionClient;
  Client: TClient;
  Row: Integer;
begin
  Row := GridClients.Row;
  if Row < 1 then
    Exit;

  Client := TClient(GridClients.Objects[0, Row]);
  if not Assigned(Client) then
    Exit;

  FormEdition := TFormEditionClient.Create(Self);
  try
    FormEdition.Client := Client;
    if FormEdition.ShowModal = mrOK then
    begin
      Client.Save;
      AfficherClients;
    end;
  finally
    FormEdition.Free;
  end;
end;

procedure TFormClients.ButtonSupprimerClick(Sender: TObject);  
var
  Client: TClient;
  Row: Integer;
begin
  Row := GridClients.Row;
  if Row < 1 then
    Exit;

  Client := TClient(GridClients.Objects[0, Row]);
  if not Assigned(Client) then
    Exit;

  if MessageDlg('Confirmation',
     Format('Supprimer le client %s ?', [Client.NomComplet]),
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Client.Delete;
    FClients.Remove(Client);
    AfficherClients;
  end;
end;

procedure TFormClients.ButtonActualiserClick(Sender: TObject);  
begin
  ChargerClients;
  AfficherClients;
end;

end.
```

### Formulaire d'édition

```pascal
unit FormEditionClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Model.Client;

type
  TFormEditionClient = class(TForm)
    EditNom: TEdit;
    EditPrenom: TEdit;
    EditEmail: TEdit;
    EditTelephone: TEdit;
    EditVille: TEdit;
    CheckBoxActif: TCheckBox;
    ButtonOK: TButton;
    ButtonAnnuler: TButton;
    LabelNom: TLabel;
    LabelPrenom: TLabel;
    LabelEmail: TLabel;
    LabelTelephone: TLabel;
    LabelVille: TLabel;
    MemoErreurs: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
  private
    FClient: TClient;
    FNouveauClient: Boolean;

    procedure ChargerDonnees;
    procedure SauvegarderDonnees;
    procedure AfficherErreurs;
  public
    property Client: TClient read FClient write FClient;
  end;

implementation

{$R *.lfm}

procedure TFormEditionClient.FormCreate(Sender: TObject);  
begin
  if not Assigned(FClient) then
  begin
    FClient := TClient.Create;
    FNouveauClient := True;
  end
  else
  begin
    FNouveauClient := False;
    ChargerDonnees;
  end;

  MemoErreurs.Visible := False;
end;

procedure TFormEditionClient.ChargerDonnees;  
begin
  EditNom.Text := FClient.Nom;
  EditPrenom.Text := FClient.Prenom;
  EditEmail.Text := FClient.Email;
  EditTelephone.Text := FClient.Telephone;
  EditVille.Text := FClient.Ville;
  CheckBoxActif.Checked := FClient.Actif;
end;

procedure TFormEditionClient.SauvegarderDonnees;  
begin
  FClient.Nom := EditNom.Text;
  FClient.Prenom := EditPrenom.Text;
  FClient.Email := EditEmail.Text;
  FClient.Telephone := EditTelephone.Text;
  FClient.Ville := EditVille.Text;
  FClient.Actif := CheckBoxActif.Checked;
end;

procedure TFormEditionClient.AfficherErreurs;  
var
  Error: TtiObjectError;
begin
  MemoErreurs.Clear;
  MemoErreurs.Lines.Add('Erreurs de validation:');

  for Error in FClient.Errors do
    MemoErreurs.Lines.Add(Format('• %s: %s',
      [Error.PropertyName, Error.ErrorMessage]));

  MemoErreurs.Visible := True;
end;

procedure TFormEditionClient.ButtonOKClick(Sender: TObject);  
begin
  SauvegarderDonnees;

  if FClient.IsValid then
  begin
    if FNouveauClient then
      FClient.Save;
    ModalResult := mrOK;
  end
  else
    AfficherErreurs;
end;

procedure TFormEditionClient.ButtonAnnulerClick(Sender: TObject);  
begin
  if FNouveauClient then
    FreeAndNil(FClient);
  ModalResult := mrCancel;
end;

end.
```

## Pattern Observer avec tiOPF

tiOPF intègre le pattern Observer pour notifier les changements.

### Implémentation d'un observateur

```pascal
unit Observers.ClientObserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject, Model.Client;

type
  TClientObserver = class(TtiObjectObserver)
  public
    procedure Update(ASubject: TtiObject); override;
  end;

implementation

procedure TClientObserver.Update(ASubject: TtiObject);  
var
  Client: TClient;
begin
  if not (ASubject is TClient) then
    Exit;

  Client := TClient(ASubject);

  // Réagir aux changements
  case Client.ObjectState of
    posCreate:
      WriteLn('Nouveau client créé: ', Client.NomComplet);
    posUpdate:
      WriteLn('Client modifié: ', Client.NomComplet);
    posDelete:
      WriteLn('Client supprimé: ', Client.NomComplet);
  end;

  // Journaliser les modifications
  if Client.Dirty then
    WriteLn('  Propriétés modifiées détectées');
end;

end.
```

### Utilisation de l'observateur

```pascal
procedure UtiliserObservateur;  
var
  Client: TClient;
  Observer: TClientObserver;
begin
  Client := TClient.Create;
  Observer := TClientObserver.Create;
  try
    // Attacher l'observateur
    Client.AttachObserver(Observer);

    // Modifier le client (déclenche les notifications)
    Client.Nom := 'Dupont';
    Client.Prenom := 'Jean';
    Client.Email := 'jean@test.com';

    // Sauvegarder (déclenche notification)
    Client.Save;
  finally
    Client.DetachObserver(Observer);
    Observer.Free;
    Client.Free;
  end;
end;
```

## Gestion des relations entre objets

### Relation One-to-Many (Un client, plusieurs commandes)

```pascal
unit Model.Commande;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject, Model.Client;

type
  TCommande = class(TtiObject)
  private
    FClientOID: string;
    FClient: TClient;
    FNumero: string;
    FDateCommande: TDateTime;
    FMontantTotal: Currency;
    FStatut: string;

    function GetClient: TClient;
  protected
    procedure DoValidate(const AErrors: TtiObjectErrors); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadClient;
    function EstLivree: Boolean;
  published
    property ClientOID: string read FClientOID write FClientOID;
    property Client: TClient read GetClient;
    property Numero: string read FNumero write FNumero;
    property DateCommande: TDateTime read FDateCommande write FDateCommande;
    property MontantTotal: Currency read FMontantTotal write FMontantTotal;
    property Statut: string read FStatut write FStatut;
  end;

  TCommandeList = class(TtiObjectList)
  private
    function GetItems(Index: Integer): TCommande;
  public
    property Items[Index: Integer]: TCommande read GetItems; default;
  end;

implementation

{ TCommande }

constructor TCommande.Create;  
begin
  inherited Create;
  FDateCommande := Now;
  FStatut := 'En attente';
  FClient := nil;
end;

destructor TCommande.Destroy;  
begin
  if Assigned(FClient) then
    FClient.Free;
  inherited Destroy;
end;

function TCommande.GetClient: TClient;  
begin
  if not Assigned(FClient) then
    LoadClient;
  Result := FClient;
end;

procedure TCommande.LoadClient;  
begin
  if FClientOID <> '' then
  begin
    FClient := TClient.Create;
    FClient.OID.AsString := FClientOID;
    FClient.Read;
  end;
end;

procedure TCommande.DoValidate(const AErrors: TtiObjectErrors);  
begin
  inherited DoValidate(AErrors);

  if FClientOID = '' then
    AErrors.AddError('ClientOID', 'Le client est obligatoire');

  if FMontantTotal <= 0 then
    AErrors.AddError('MontantTotal', 'Le montant doit être positif');

  if Trim(FNumero) = '' then
    AErrors.AddError('Numero', 'Le numéro de commande est obligatoire');
end;

function TCommande.EstLivree: Boolean;  
begin
  Result := FStatut = 'Livrée';
end;

{ TCommandeList }

function TCommandeList.GetItems(Index: Integer): TCommande;  
begin
  Result := TCommande(inherited Items[Index]);
end;

end.
```

### Extension du client avec ses commandes

```pascal
type
  TClient = class(TtiObject)
  private
    // ... propriétés existantes ...
    FCommandes: TCommandeList;
    FCommandesLoaded: Boolean;

    function GetCommandes: TCommandeList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadCommandes;
    function NombreCommandes: Integer;
    function MontantTotalCommandes: Currency;

    property Commandes: TCommandeList read GetCommandes;
  published
    // ... propriétés existantes ...
  end;

implementation

constructor TClient.Create;  
begin
  inherited Create;
  FCommandes := nil;
  FCommandesLoaded := False;
end;

destructor TClient.Destroy;  
begin
  if Assigned(FCommandes) then
    FCommandes.Free;
  inherited Destroy;
end;

function TClient.GetCommandes: TCommandeList;  
begin
  if not FCommandesLoaded then
    LoadCommandes;
  Result := FCommandes;
end;

procedure TClient.LoadCommandes;  
begin
  if not Assigned(FCommandes) then
    FCommandes := TCommandeList.Create;

  FCommandes.Clear;
  FCommandes.Criteria.AddEqualTo('client_oid', Self.OID.AsString);
  FCommandes.Read;
  FCommandesLoaded := True;
end;

function TClient.NombreCommandes: Integer;  
begin
  Result := Commandes.Count;
end;

function TClient.MontantTotalCommandes: Currency;  
var
  Commande: TCommande;
begin
  Result := 0;
  for Commande in Commandes do
    Result := Result + Commande.MontantTotal;
end;
```

### Utilisation des relations

```pascal
procedure AfficherClientAvecCommandes(const ClientOID: string);  
var
  Client: TClient;
  Commande: TCommande;
begin
  Client := TClient.Create;
  try
    Client.OID.AsString := ClientOID;
    Client.Read;

    WriteLn('Client: ', Client.NomComplet);
    WriteLn('Email: ', Client.Email);
    WriteLn;
    WriteLn(Format('Nombre de commandes: %d', [Client.NombreCommandes]));
    WriteLn(Format('Montant total: %.2f €', [Client.MontantTotalCommandes]));
    WriteLn;
    WriteLn('Détail des commandes:');

    for Commande in Client.Commandes do
    begin
      WriteLn(Format('  %s - %s - %.2f € - %s',
        [Commande.Numero,
         FormatDateTime('dd/mm/yyyy', Commande.DateCommande),
         Commande.MontantTotal,
         Commande.Statut]));
    end;
  finally
    Client.Free;
  end;
end;
```

## Requêtes personnalisées

### Requête SQL directe

```pascal
unit Queries.ClientQueries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiQuery, tiOPFManager, Model.Client;

type
  TClientQueries = class
  public
    class function GetClientsActifs: TClientList;
    class function GetClientsParVille(const AVille: string): TClientList;
    class function GetTopClients(TopN: Integer): TClientList;
    class function GetStatistiquesParVille: TStringList;
  end;

implementation

class function TClientQueries.GetClientsActifs: TClientList;  
var
  Query: TtiQuery;
  Client: TClient;
begin
  Result := TClientList.Create;
  Query := GTIOPFManager.PersistenceLayerMgr.CreateTIQuery;
  try
    Query.SQL.Text :=
      'SELECT * FROM clients WHERE actif = TRUE ORDER BY nom';
    Query.Open;

    while not Query.EOF do
    begin
      Client := TClient.Create;
      // Mapper manuellement
      Client.OID.AsString := Query.FieldAsString['oid'];
      Client.Nom := Query.FieldAsString['nom'];
      Client.Prenom := Query.FieldAsString['prenom'];
      Client.Email := Query.FieldAsString['email'];
      Client.Telephone := Query.FieldAsString['telephone'];
      Client.Ville := Query.FieldAsString['ville'];
      Client.DateInscription := Query.FieldAsDateTime['date_inscription'];
      Client.Actif := Query.FieldAsBoolean['actif'];
      Client.ObjectState := posClean;

      Result.Add(Client);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

class function TClientQueries.GetClientsParVille(const AVille: string): TClientList;  
var
  Query: TtiQuery;
  Client: TClient;
begin
  Result := TClientList.Create;
  Query := GTIOPFManager.PersistenceLayerMgr.CreateTIQuery;
  try
    Query.SQL.Text :=
      'SELECT * FROM clients WHERE ville = :ville ORDER BY nom';
    Query.ParamAsString['ville'] := AVille;
    Query.Open;

    while not Query.EOF do
    begin
      Client := TClient.Create;
      // Mapping...
      Result.Add(Client);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

class function TClientQueries.GetTopClients(TopN: Integer): TClientList;  
var
  Query: TtiQuery;
  Client: TClient;
begin
  Result := TClientList.Create;
  Query := GTIOPFManager.PersistenceLayerMgr.CreateTIQuery;
  try
    Query.SQL.Text :=
      'SELECT c.* ' +
      'FROM clients c ' +
      'LEFT JOIN commandes cmd ON c.oid = cmd.client_oid ' +
      'GROUP BY c.oid, c.nom, c.prenom, c.email, c.telephone, ' +
      '         c.ville, c.date_inscription, c.actif ' +
      'ORDER BY SUM(cmd.montant_total) DESC ' +
      'LIMIT :top_n';
    Query.ParamAsInteger['top_n'] := TopN;
    Query.Open;

    while not Query.EOF do
    begin
      Client := TClient.Create;
      // Mapping...
      Result.Add(Client);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

class function TClientQueries.GetStatistiquesParVille: TStringList;  
var
  Query: TtiQuery;
begin
  Result := TStringList.Create;
  Query := GTIOPFManager.PersistenceLayerMgr.CreateTIQuery;
  try
    Query.SQL.Text :=
      'SELECT ville, COUNT(*) as nb_clients ' +
      'FROM clients ' +
      'WHERE actif = TRUE ' +
      'GROUP BY ville ' +
      'ORDER BY nb_clients DESC';
    Query.Open;

    while not Query.EOF do
    begin
      Result.Add(Format('%s: %d clients',
        [Query.FieldAsString['ville'],
         Query.FieldAsInteger['nb_clients']]));
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
```

### Utilisation des requêtes personnalisées

```pascal
procedure UtiliserRequetesPersonnalisees;  
var
  Clients: TClientList;
  Stats: TStringList;
begin
  // Top 10 des clients
  Clients := TClientQueries.GetTopClients(10);
  try
    WriteLn('=== Top 10 des clients ===');
    for var Client in Clients do
      WriteLn(Client.NomComplet);
  finally
    Clients.Free;
  end;

  WriteLn;

  // Statistiques par ville
  Stats := TClientQueries.GetStatistiquesParVille;
  try
    WriteLn('=== Clients par ville ===');
    for var Stat in Stats do
      WriteLn(Stat);
  finally
    Stats.Free;
  end;
end;
```

## Cache et optimisations

### Mise en cache des objets

```pascal
unit Cache.ClientCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Model.Client;

type
  TClientCache = class
  private
    FCache: TDictionary<string, TClient>;
    FCacheEnabled: Boolean;
    FMaxCacheSize: Integer;
  public
    constructor Create(MaxSize: Integer = 100);
    destructor Destroy; override;

    function Get(const AOID: string): TClient;
    procedure Put(Client: TClient);
    procedure Remove(const AOID: string);
    procedure Clear;

    property Enabled: Boolean read FCacheEnabled write FCacheEnabled;
    property Count: Integer read GetCount;
  end;

var
  ClientCache: TClientCache;

implementation

constructor TClientCache.Create(MaxSize: Integer);  
begin
  inherited Create;
  FCache := TDictionary<string, TClient>.Create;
  FCacheEnabled := True;
  FMaxCacheSize := MaxSize;
end;

destructor TClientCache.Destroy;  
begin
  Clear;
  FCache.Free;
  inherited Destroy;
end;

function TClientCache.Get(const AOID: string): TClient;  
begin
  if not FCacheEnabled then
  begin
    Result := nil;
    Exit;
  end;

  if not FCache.TryGetValue(AOID, Result) then
    Result := nil;
end;

procedure TClientCache.Put(Client: TClient);  
begin
  if not FCacheEnabled then
    Exit;

  // Si le cache est plein, vider
  if FCache.Count >= FMaxCacheSize then
    Clear;

  // Cloner l'objet pour le cache
  FCache.AddOrSetValue(Client.OID.AsString, Client.Clone as TClient);
end;

procedure TClientCache.Remove(const AOID: string);  
var
  Client: TClient;
begin
  if FCache.TryGetValue(AOID, Client) then
  begin
    Client.Free;
    FCache.Remove(AOID);
  end;
end;

procedure TClientCache.Clear;  
var
  Client: TClient;
begin
  for Client in FCache.Values do
    Client.Free;
  FCache.Clear;
end;

function TClientCache.GetCount: Integer;  
begin
  Result := FCache.Count;
end;

initialization
  ClientCache := TClientCache.Create;

finalization
  ClientCache.Free;

end.
```

### Utilisation du cache

```pascal
function ChargerClientAvecCache(const AOID: string): TClient;  
begin
  // Vérifier le cache d'abord
  Result := ClientCache.Get(AOID);

  if not Assigned(Result) then
  begin
    // Pas en cache, charger depuis la base
    Result := TClient.Create;
    Result.OID.AsString := AOID;
    Result.Read;

    // Mettre en cache
    ClientCache.Put(Result);
  end
  else
    WriteLn('Client chargé depuis le cache');
end;
```

## Tests unitaires avec tiOPF

### Configuration des tests

```pascal
unit Tests.Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  tiOPFManager, Model.Client;

type
  TTestClient = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreation;
    procedure TestValidation;
    procedure TestSauvegarde;
    procedure TestChargement;
    procedure TestModification;
    procedure TestSuppression;
    procedure TestListe;
  end;

implementation

uses
  AppConfig;

procedure TTestClient.SetUp;  
begin
  // Utiliser une base de données de test
  InitializePersistenceLayer;

  // Créer les tables si nécessaire
  // ...
end;

procedure TTestClient.TearDown;  
begin
  FinalizePersistenceLayer;
end;

procedure TTestClient.TestCreation;  
var
  Client: TClient;
begin
  Client := TClient.Create;
  try
    AssertNotNull('Client doit être créé', Client);
    AssertTrue('Client doit être actif par défaut', Client.Actif);
    AssertTrue('Date d''inscription doit être définie',
      Client.DateInscription > 0);
  finally
    Client.Free;
  end;
end;

procedure TTestClient.TestValidation;  
var
  Client: TClient;
begin
  Client := TClient.Create;
  try
    // Client invalide (données manquantes)
    AssertFalse('Client vide doit être invalide', Client.IsValid);

    // Remplir les données obligatoires
    Client.Nom := 'Dupont';
    Client.Prenom := 'Jean';
    Client.Email := 'jean@test.com';

    // Client valide
    AssertTrue('Client complet doit être valide', Client.IsValid);

    // Email invalide
    Client.Email := 'email-invalide';
    AssertFalse('Client avec email invalide doit être invalide',
      Client.IsValid);
  finally
    Client.Free;
  end;
end;

procedure TTestClient.TestSauvegarde;  
var
  Client: TClient;
  OID: string;
begin
  Client := TClient.Create;
  try
    Client.Nom := 'Test';
    Client.Prenom := 'User';
    Client.Email := 'test@example.com';

    Client.Save;

    AssertTrue('OID doit être généré', Client.OID.AsString <> '');
    AssertFalse('Client ne doit plus être Dirty', Client.Dirty);
    AssertEquals('ObjectState doit être posClean',
      Ord(posClean), Ord(Client.ObjectState));

    OID := Client.OID.AsString;
  finally
    Client.Free;
  end;
end;

procedure TTestClient.TestChargement;  
var
  Client1, Client2: TClient;
begin
  // Créer et sauvegarder
  Client1 := TClient.Create;
  try
    Client1.Nom := 'Chargement';
    Client1.Prenom := 'Test';
    Client1.Email := 'chargement@test.com';
    Client1.Save;

    // Charger dans un nouvel objet
    Client2 := TClient.Create;
    try
      Client2.OID.AsString := Client1.OID.AsString;
      Client2.Read;

      AssertEquals('Nom doit correspondre', Client1.Nom, Client2.Nom);
      AssertEquals('Email doit correspondre', Client1.Email, Client2.Email);
    finally
      Client2.Free;
    end;
  finally
    Client1.Free;
  end;
end;

procedure TTestClient.TestModification;  
var
  Client: TClient;
  NouvelEmail: string;
begin
  Client := TClient.Create;
  try
    // Créer
    Client.Nom := 'Modif';
    Client.Prenom := 'Test';
    Client.Email := 'avant@test.com';
    Client.Save;

    // Modifier
    NouvelEmail := 'apres@test.com';
    Client.Email := NouvelEmail;
    AssertTrue('Client doit être Dirty après modification', Client.Dirty);

    Client.Save;

    // Recharger et vérifier
    Client.Read;
    AssertEquals('Email doit être modifié', NouvelEmail, Client.Email);
  finally
    Client.Free;
  end;
end;

procedure TTestClient.TestSuppression;  
var
  Client: TClient;
  OID: string;
begin
  // Créer
  Client := TClient.Create;
  try
    Client.Nom := 'Suppr';
    Client.Prenom := 'Test';
    Client.Email := 'suppr@test.com';
    Client.Save;
    OID := Client.OID.AsString;

    // Supprimer
    Client.Delete;
  finally
    Client.Free;
  end;

  // Vérifier que l'objet n'existe plus
  Client := TClient.Create;
  try
    Client.OID.AsString := OID;
    try
      Client.Read;
      Fail('Le client supprimé ne devrait pas être trouvé');
    except
      // Exception attendue
    end;
  finally
    Client.Free;
  end;
end;

procedure TTestClient.TestListe;  
var
  Clients: TClientList;
  Client: TClient;
  i: Integer;
begin
  // Créer plusieurs clients
  for i := 1 to 5 do
  begin
    Client := TClient.Create;
    try
      Client.Nom := 'Client' + IntToStr(i);
      Client.Prenom := 'Test';
      Client.Email := Format('client%d@test.com', [i]);
      Client.Save;
    finally
      Client.Free;
    end;
  end;

  // Charger la liste
  Clients := TClientList.Create;
  try
    Clients.Read;
    AssertTrue('La liste doit contenir au moins 5 clients',
      Clients.Count >= 5);
  finally
    Clients.Free;
  end;
end;

initialization
  RegisterTest(TTestClient);

end.
```

## Logging et diagnostic

### Logger personnalisé pour tiOPF

```pascal
unit Logger.tiOPFLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiLog;

type
  TCustomtiOPFLogger = class(TtiLog)
  private
    FLogFile: string;
    procedure WriteToFile(const AMessage: string);
  public
    constructor Create(const ALogFile: string);
    procedure Log(const AMessage: string); override;
    procedure LogSQL(const ASQL: string); override;
    procedure LogError(const AMessage: string); override;
  end;

implementation

constructor TCustomtiOPFLogger.Create(const ALogFile: string);  
begin
  inherited Create;
  FLogFile := ALogFile;
end;

procedure TCustomtiOPFLogger.WriteToFile(const AMessage: string);  
var
  F: TextFile;
begin
  AssignFile(F, FLogFile);
  if FileExists(FLogFile) then
    Append(F)
  else
    Rewrite(F);
  try
    WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), ' - ', AMessage);
  finally
    CloseFile(F);
  end;
end;

procedure TCustomtiOPFLogger.Log(const AMessage: string);  
begin
  WriteToFile('[INFO] ' + AMessage);
end;

procedure TCustomtiOPFLogger.LogSQL(const ASQL: string);  
begin
  WriteToFile('[SQL] ' + ASQL);
end;

procedure TCustomtiOPFLogger.LogError(const AMessage: string);  
begin
  WriteToFile('[ERROR] ' + AMessage);
end;

end.
```

### Configuration du logger

```pascal
procedure ConfigurerLogging;  
var
  Logger: TCustomtiOPFLogger;
  CheminLog: string;
begin
  CheminLog := ExtractFilePath(ParamStr(0)) + 'logs' + PathDelim;
  ForceDirectories(CheminLog);
  CheminLog := CheminLog + 'tiopf_' +
    FormatDateTime('yyyymmdd', Now) + '.log';

  Logger := TCustomtiOPFLogger.Create(CheminLog);
  GTIOPFManager.Log := Logger;

  // Activer les différents niveaux de log
  GTIOPFManager.Log.Active := True;
  GTIOPFManager.Log.LogSQL := True;
  GTIOPFManager.Log.LogErrors := True;

  WriteLn('Logging configuré: ', CheminLog);
end;
```

### Utilisation du logging

```pascal
procedure OperationAvecLogging;  
var
  Client: TClient;
begin
  GTIOPFManager.Log.Log('Début de l''opération');

  Client := TClient.Create;
  try
    Client.Nom := 'Test';
    Client.Prenom := 'Logging';
    Client.Email := 'logging@test.com';

    GTIOPFManager.Log.Log('Sauvegarde du client: ' + Client.NomComplet);
    Client.Save;

    GTIOPFManager.Log.Log('Client sauvegardé avec OID: ' + Client.OID.AsString);
  except
    on E: Exception do
    begin
      GTIOPFManager.Log.LogError('Erreur: ' + E.Message);
      raise;
    end;
  end;

  Client.Free;
  GTIOPFManager.Log.Log('Fin de l''opération');
end;
```

## Patterns avancés avec tiOPF

### Repository Pattern

```pascal
unit Repositories.BaseRepository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, tiObject;

type
  generic TBaseRepository<T: TtiObject> = class
  protected
    function CreateEntity: T; virtual; abstract;
    function CreateList: TtiObjectList; virtual; abstract;
  public
    function FindByID(const AOID: string): T;
    function FindAll: TList<T>;
    function FindByCriteria(const ACriteria: TtiCriteria): TList<T>;
    procedure Save(AEntity: T);
    procedure Delete(AEntity: T);
    function Count: Integer;
  end;

implementation

function TBaseRepository.FindByID(const AOID: string): T;  
begin
  Result := CreateEntity;
  Result.OID.AsString := AOID;
  try
    Result.Read;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TBaseRepository.FindAll: TList<T>;  
var
  List: TtiObjectList;
  Item: TtiObject;
begin
  Result := TList<T>.Create;
  List := CreateList;
  try
    List.Read;
    for Item in List do
      Result.Add(T(Item));
  finally
    List.Free;
  end;
end;

function TBaseRepository.FindByCriteria(const ACriteria: TtiCriteria): TList<T>;  
var
  List: TtiObjectList;
  Item: TtiObject;
begin
  Result := TList<T>.Create;
  List := CreateList;
  try
    List.Criteria.Assign(ACriteria);
    List.Read;
    for Item in List do
      Result.Add(T(Item));
  finally
    List.Free;
  end;
end;

procedure TBaseRepository.Save(AEntity: T);  
begin
  AEntity.Save;
end;

procedure TBaseRepository.Delete(AEntity: T);  
begin
  AEntity.Delete;
end;

function TBaseRepository.Count: Integer;  
var
  List: TtiObjectList;
begin
  List := CreateList;
  try
    List.Read;
    Result := List.Count;
  finally
    List.Free;
  end;
end;

end.
```

### Repository Client

```pascal
unit Repositories.ClientRepository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  tiObject, Model.Client, Repositories.BaseRepository;

type
  TClientRepository = class(specialize TBaseRepository<TClient>)
  protected
    function CreateEntity: TClient; override;
    function CreateList: TtiObjectList; override;
  public
    function FindByEmail(const AEmail: string): TClient;
    function FindByVille(const AVille: string): TList<TClient>;
    function FindActifs: TList<TClient>;
  end;

implementation

function TClientRepository.CreateEntity: TClient;  
begin
  Result := TClient.Create;
end;

function TClientRepository.CreateList: TtiObjectList;  
begin
  Result := TClientList.Create;
end;

function TClientRepository.FindByEmail(const AEmail: string): TClient;  
var
  List: TClientList;
begin
  Result := nil;
  List := TClientList.Create;
  try
    List.Criteria.AddEqualTo('email', AEmail);
    List.Read;

    if List.Count > 0 then
      Result := List[0].Clone as TClient;
  finally
    List.Free;
  end;
end;

function TClientRepository.FindByVille(const AVille: string): TList<TClient>;  
var
  Criteria: TtiCriteria;
begin
  Criteria := TtiCriteria.Create;
  try
    Criteria.AddEqualTo('ville', AVille);
    Result := FindByCriteria(Criteria);
  finally
    Criteria.Free;
  end;
end;

function TClientRepository.FindActifs: TList<TClient>;  
var
  Criteria: TtiCriteria;
begin
  Criteria := TtiCriteria.Create;
  try
    Criteria.AddEqualTo('actif', True);
    Result := FindByCriteria(Criteria);
  finally
    Criteria.Free;
  end;
end;

end.
```

### Service Layer Pattern

```pascal
unit Services.ClientService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  Model.Client, Repositories.ClientRepository;

type
  TClientService = class
  private
    FRepository: TClientRepository;
  public
    constructor Create;
    destructor Destroy; override;

    function CreerClient(const ANom, APrenom, AEmail, AVille: string): TClient;
    function ObtenirClient(const AOID: string): TClient;
    function RechercherParEmail(const AEmail: string): TClient;
    function ObtenirTous: TList<TClient>;
    function ObtenirParVille(const AVille: string): TList<TClient>;

    procedure ModifierClient(AClient: TClient);
    procedure SupprimerClient(const AOID: string);
    procedure ActiverClient(const AOID: string);
    procedure DesactiverClient(const AOID: string);

    function NombreClientsActifs: Integer;
    function VerifierEmailUnique(const AEmail: string): Boolean;
  end;

implementation

constructor TClientService.Create;  
begin
  inherited Create;
  FRepository := TClientRepository.Create;
end;

destructor TClientService.Destroy;  
begin
  FRepository.Free;
  inherited Destroy;
end;

function TClientService.CreerClient(const ANom, APrenom, AEmail,
  AVille: string): TClient;
begin
  // Vérifier l'unicité de l'email
  if not VerifierEmailUnique(AEmail) then
    raise Exception.Create('Un client avec cet email existe déjà');

  Result := TClient.Create;
  Result.Nom := ANom;
  Result.Prenom := APrenom;
  Result.Email := AEmail;
  Result.Ville := AVille;

  // Valider
  if not Result.IsValid then
    raise Exception.Create('Données client invalides: ' +
      Result.Errors.ToString);

  // Sauvegarder
  FRepository.Save(Result);

  // Logger
  GTIOPFManager.Log.Log(Format('Client créé: %s (OID: %s)',
    [Result.NomComplet, Result.OID.AsString]));
end;

function TClientService.ObtenirClient(const AOID: string): TClient;  
begin
  Result := FRepository.FindByID(AOID);
end;

function TClientService.RechercherParEmail(const AEmail: string): TClient;  
begin
  Result := FRepository.FindByEmail(AEmail);
end;

function TClientService.ObtenirTous: TList<TClient>;  
begin
  Result := FRepository.FindAll;
end;

function TClientService.ObtenirParVille(const AVille: string): TList<TClient>;  
begin
  Result := FRepository.FindByVille(AVille);
end;

procedure TClientService.ModifierClient(AClient: TClient);  
begin
  if not AClient.IsValid then
    raise Exception.Create('Données client invalides');

  FRepository.Save(AClient);

  GTIOPFManager.Log.Log(Format('Client modifié: %s', [AClient.NomComplet]));
end;

procedure TClientService.SupprimerClient(const AOID: string);  
var
  Client: TClient;
begin
  Client := FRepository.FindByID(AOID);
  try
    FRepository.Delete(Client);
    GTIOPFManager.Log.Log(Format('Client supprimé: %s', [Client.NomComplet]));
  finally
    Client.Free;
  end;
end;

procedure TClientService.ActiverClient(const AOID: string);  
var
  Client: TClient;
begin
  Client := FRepository.FindByID(AOID);
  try
    Client.Activer;
    FRepository.Save(Client);
  finally
    Client.Free;
  end;
end;

procedure TClientService.DesactiverClient(const AOID: string);  
var
  Client: TClient;
begin
  Client := FRepository.FindByID(AOID);
  try
    Client.Desactiver;
    FRepository.Save(Client);
  finally
    Client.Free;
  end;
end;

function TClientService.NombreClientsActifs: Integer;  
var
  Clients: TList<TClient>;
begin
  Clients := FRepository.FindActifs;
  try
    Result := Clients.Count;
  finally
    for var Client in Clients do
      Client.Free;
    Clients.Free;
  end;
end;

function TClientService.VerifierEmailUnique(const AEmail: string): Boolean;  
var
  Client: TClient;
begin
  Client := FRepository.FindByEmail(AEmail);
  Result := not Assigned(Client);
  if Assigned(Client) then
    Client.Free;
end;

end.
```

## Migration depuis d'autres systèmes

### Importation depuis CSV

```pascal
unit Importers.ClientCSVImporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Model.Client, Services.ClientService;

type
  TClientCSVImporter = class
  private
    FService: TClientService;
    FErrors: TStringList;
    FSuccessCount: Integer;
    FErrorCount: Integer;
  public
    constructor Create(AService: TClientService);
    destructor Destroy; override;

    procedure ImportFromFile(const AFileName: string);

    property Errors: TStringList read FErrors;
    property SuccessCount: Integer read FSuccessCount;
    property ErrorCount: Integer read FErrorCount;
  end;

implementation

uses
  StrUtils;

constructor TClientCSVImporter.Create(AService: TClientService);  
begin
  inherited Create;
  FService := AService;
  FErrors := TStringList.Create;
  FSuccessCount := 0;
  FErrorCount := 0;
end;

destructor TClientCSVImporter.Destroy;  
begin
  FErrors.Free;
  inherited Destroy;
end;

procedure TClientCSVImporter.ImportFromFile(const AFileName: string);  
var
  CSV: TStringList;
  i: Integer;
  Line: string;
  Fields: TStringArray;
  Client: TClient;
begin
  FErrors.Clear;
  FSuccessCount := 0;
  FErrorCount := 0;

  CSV := TStringList.Create;
  try
    CSV.LoadFromFile(AFileName);

    // Démarrer une transaction pour l'import
    GTIOPFManager.BeginTransaction;
    try
      for i := 1 to CSV.Count - 1 do  // Ignorer l'en-tête (ligne 0)
      begin
        Line := CSV[i];
        if Trim(Line) = '' then
          Continue;

        Fields := Line.Split([';']);

        if Length(Fields) < 4 then
        begin
          FErrors.Add(Format('Ligne %d: Format invalide', [i + 1]));
          Inc(FErrorCount);
          Continue;
        end;

        try
          Client := FService.CreerClient(
            Trim(Fields[0]),  // Nom
            Trim(Fields[1]),  // Prenom
            Trim(Fields[2]),  // Email
            Trim(Fields[3])   // Ville
          );
          try
            Inc(FSuccessCount);
          finally
            Client.Free;
          end;
        except
          on E: Exception do
          begin
            FErrors.Add(Format('Ligne %d: %s', [i + 1, E.Message]));
            Inc(FErrorCount);
          end;
        end;
      end;

      GTIOPFManager.Commit;
      WriteLn(Format('Import terminé: %d succès, %d erreurs',
        [FSuccessCount, FErrorCount]));
    except
      GTIOPFManager.Rollback;
      raise;
    end;
  finally
    CSV.Free;
  end;
end;

end.
```

### Utilisation de l'importateur

```pascal
procedure ImporterClients;  
var
  Service: TClientService;
  Importer: TClientCSVImporter;
begin
  Service := TClientService.Create;
  try
    Importer := TClientCSVImporter.Create(Service);
    try
      WriteLn('Import en cours...');
      Importer.ImportFromFile('clients.csv');

      if Importer.ErrorCount > 0 then
      begin
        WriteLn('Erreurs rencontrées:');
        for var Error in Importer.Errors do
          WriteLn('  ', Error);
      end;
    finally
      Importer.Free;
    end;
  finally
    Service.Free;
  end;
end;
```

## Exportation de données

### Exportateur CSV

```pascal
unit Exporters.ClientCSVExporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Model.Client, Services.ClientService;

type
  TClientCSVExporter = class
  private
    FService: TClientService;
    FDelimiter: Char;
  public
    constructor Create(AService: TClientService; ADelimiter: Char = ';');

    procedure ExportToFile(const AFileName: string);
    procedure ExportByVille(const AVille, AFileName: string);
  end;

implementation

constructor TClientCSVExporter.Create(AService: TClientService;
  ADelimiter: Char);
begin
  inherited Create;
  FService := AService;
  FDelimiter := ADelimiter;
end;

procedure TClientCSVExporter.ExportToFile(const AFileName: string);  
var
  CSV: TStringList;
  Clients: TList<TClient>;
  Client: TClient;
begin
  CSV := TStringList.Create;
  try
    // En-tête
    CSV.Add('Nom' + FDelimiter + 'Prenom' + FDelimiter + 'Email' +
            FDelimiter + 'Telephone' + FDelimiter + 'Ville' +
            FDelimiter + 'Actif');

    // Données
    Clients := FService.ObtenirTous;
    try
      for Client in Clients do
      begin
        CSV.Add(
          Client.Nom + FDelimiter +
          Client.Prenom + FDelimiter +
          Client.Email + FDelimiter +
          Client.Telephone + FDelimiter +
          Client.Ville + FDelimiter +
          IfThen(Client.Actif, 'Oui', 'Non')
        );
      end;
    finally
      for Client in Clients do
        Client.Free;
      Clients.Free;
    end;

    CSV.SaveToFile(AFileName);
    WriteLn(Format('Export terminé: %d clients exportés', [CSV.Count - 1]));
  finally
    CSV.Free;
  end;
end;

procedure TClientCSVExporter.ExportByVille(const AVille, AFileName: string);  
var
  CSV: TStringList;
  Clients: TList<TClient>;
  Client: TClient;
begin
  CSV := TStringList.Create;
  try
    CSV.Add('Nom' + FDelimiter + 'Prenom' + FDelimiter + 'Email');

    Clients := FService.ObtenirParVille(AVille);
    try
      for Client in Clients do
        CSV.Add(Client.Nom + FDelimiter + Client.Prenom +
                FDelimiter + Client.Email);
    finally
      for Client in Clients do
        Client.Free;
      Clients.Free;
    end;

    CSV.SaveToFile(AFileName);
  finally
    CSV.Free;
  end;
end;

end.
```

## Application complète avec tiOPF

### Programme principal

```pascal
program GestionClients;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Forms,
  tiOPFManager,
  AppConfig,
  Model.Client,
  Persistence.Mappings,
  Services.ClientService,
  FormClients;

{$R *.res}

begin
  try
    // Initialiser tiOPF
    WriteLn('Initialisation de tiOPF...');
    InitializePersistenceLayer;
    ConfigurerLogging;

    // Lancer l'application
    Application.Title := 'Gestion Clients - tiOPF';
    RequireDerivedFormResource := True;
    Application.Scaled := True;
    Application.Initialize;
    Application.CreateForm(TFormClients, FormClients);
    Application.Run;

  except
    on E: Exception do
    begin
      WriteLn('Erreur: ', E.ClassName, ' - ', E.Message);
      Halt(1);
    end;
  end;

  // Finaliser
  FinalizePersistenceLayer;
end.
```

## Bonnes pratiques tiOPF

### 1. Organisation du code

```
MonProjet/
├── src/
│   ├── Model/              # Objets métier
│   │   ├── Model.Client.pas
│   │   └── Model.Commande.pas
│   ├── Persistence/        # Mappings et visitors
│   │   ├── Persistence.ClientMapping.pas
│   │   └── Persistence.Mappings.pas
│   ├── Repositories/       # Couche d'accès aux données
│   │   └── Repositories.ClientRepository.pas
│   ├── Services/           # Logique métier
│   │   └── Services.ClientService.pas
│   ├── Forms/              # Interface utilisateur
│   │   ├── FormClients.pas
│   │   └── FormEditionClient.pas
│   └── Utils/              # Utilitaires
│       ├── AppConfig.pas
│       └── Logger.tiOPFLogger.pas
└── MonProjet.lpr
```

### 2. Gestion de la mémoire

```pascal
// ✅ BON - Toujours libérer les objets
procedure BonneGestion;  
var
  Client: TClient;
begin
  Client := TClient.Create;
  try
    Client.Nom := 'Test';
    Client.Save;
  finally
    Client.Free;  // Toujours libérer
  end;
end;

// ✅ BON - Libérer les listes ET leur contenu
procedure BonneGestionListe;  
var
  Clients: TList<TClient>;
  Client: TClient;
begin
  Clients := Service.ObtenirTous;
  try
    for Client in Clients do
    begin
      // Utiliser Client
    end;
  finally
    for Client in Clients do
      Client.Free;  // Libérer chaque objet
    Clients.Free;   // Puis la liste
  end;
end;

// ❌ MAUVAIS - Fuite mémoire
procedure MauvaiseGestion;  
var
  Client: TClient;
begin
  Client := TClient.Create;
  Client.Nom := 'Test';
  Client.Save;
  // Oubli du Free !
end;
```

### 3. Validation systématique

```pascal
procedure ToujoursValider;  
var
  Client: TClient;
begin
  Client := TClient.Create;
  try
    Client.Nom := 'Test';
    Client.Email := 'test@example.com';

    // ✅ BON - Toujours valider avant de sauvegarder
    if Client.IsValid then
      Client.Save
    else
    begin
      WriteLn('Erreurs:');
      WriteLn(Client.Errors.ToString);
    end;
  finally
    Client.Free;
  end;
end;
```

### 4. Utilisation des transactions

```pascal
// ✅ BON - Transactions pour opérations multiples
procedure OperationsMultiples;  
begin
  GTIOPFManager.BeginTransaction;
  try
    // Plusieurs opérations
    // ...

    GTIOPFManager.Commit;
  except
    GTIOPFManager.Rollback;
    raise;
  end;
end;
```

### 5. Logging approprié

```pascal
procedure LoggerCorrectement;  
var
  Client: TClient;
begin
  GTIOPFManager.Log.Log('Début opération');

  try
    Client := TClient.Create;
    try
      Client.Nom := 'Test';
      Client.Save;
      GTIOPFManager.Log.Log('Client sauvegardé: ' + Client.OID.AsString);
    finally
      Client.Free;
    end;
  except
    on E: Exception do
    begin
      GTIOPFManager.Log.LogError('Erreur: ' + E.Message);
      raise;
    end;
  end;

  GTIOPFManager.Log.Log('Fin opération');
end;
```

## Avantages et inconvénients de tiOPF

### Avantages

✅ **Framework mature** - Plus de 20 ans de développement  
✅ **Architecture MVC complète** - Séparation claire des responsabilités  
✅ **Multi-SGBD** - Support de nombreuses bases de données  
✅ **Multi-plateforme** - Windows, Linux, macOS  
✅ **Pattern Observer intégré** - Notification automatique des changements  
✅ **Validation robuste** - Système de validation complet  
✅ **Gestion des transactions** - Support complet  
✅ **Communauté active** - Documentation et support  
✅ **Open source** - Code source disponible et modifiable

### Inconvénients

⚠️ **Courbe d'apprentissage** - Plus complexe qu'un ORM simple  
⚠️ **Verbosité** - Nécessite plus de code initial (mappings, visitors)  
⚠️ **Performance** - Peut être moins rapide que du SQL direct pour certaines opérations  
⚠️ **Documentation** - Moins abondante que pour des frameworks plus récents  
⚠️ **OID obligatoire** - Utilisation de GUID plutôt qu'ID auto-incrémenté

## Comparaison avec d'autres solutions

| Aspect | tiOPF | ORM Custom | ZEOS | mORMot |
|--------|-------|------------|------|--------|
| **Complexité** | Moyenne-Élevée | Variable | Faible | Élevée |
| **Framework complet** | ✅ | ❌ | ❌ | ✅ |
| **MVC intégré** | ✅ | ❌ | ❌ | ✅ |
| **Performance** | ⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Facilité d'utilisation** | ⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐ |
| **Documentation** | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Multi-SGBD** | ✅ | ✅ | ✅ | ✅ |
| **Validation intégrée** | ✅ | ✅ | ❌ | ✅ |

## Quand utiliser tiOPF ?

### Utilisez tiOPF si :

- Vous développez une **application d'entreprise complexe**
- Vous voulez une **architecture MVC** robuste
- Vous avez besoin de **validation métier** avancée
- Vous recherchez un **framework éprouvé**
- Vous appréciez la **séparation des responsabilités**
- Vous voulez un **pattern Observer** intégré

### Évitez tiOPF si :

- Vous avez besoin de **performances maximales**
- Votre projet est **simple** avec peu d'entités
- Vous préférez une **approche minimaliste**
- Vous voulez un **développement rapide** sans configuration
- Vous avez une **équipe débutante** en POO

## Ressources et documentation

### Documentation officielle

- **Site web** : http://tiopf.sourceforge.net/
- **GitHub** : https://github.com/graemeg/tiopf
- **Wiki** : https://github.com/graemeg/tiopf/wiki
- **Forum** : https://sourceforge.net/p/tiopf/discussion/

### Tutoriels et exemples

Le dépôt tiOPF contient de nombreux exemples dans le dossier `/Demos/` :
- Exemple simple (Client/Commande)
- Application complète avec GUI
- Utilisation avancée des critères
- Gestion des relations
- Tests unitaires

### Livres et articles

- "Object-Relational Mapping with tiOPF" (documentation du projet)
- Articles sur le blog officiel tiOPF
- Exemples de la communauté FreePascal/Lazarus

## Conclusion

tiOPF est un **framework complet et mature** pour la persistance objet en FreePascal/Lazarus. Il offre bien plus qu'un simple ORM : c'est une architecture MVC complète avec validation, transactions, observers et support multi-SGBD.

### Points clés à retenir

1. **Architecture MVC** - Séparation claire entre modèle, vue et contrôleur
2. **TtiObject** - Classe de base pour tous les objets métier
3. **Visitors** - Pattern pour la persistance (lecture/écriture)
4. **Validation** - Système robuste de validation métier
5. **Transactions** - Support complet des transactions
6. **Multi-plateforme** - Fonctionne identiquement sur Windows et Linux
7. **Extensible** - Architecture modulaire et personnalisable

### Pour aller plus loin

- Explorez les **exemples fournis** avec tiOPF
- Créez vos propres **visitors personnalisés**
- Implémentez des **observers** pour vos besoins métier
- Utilisez les **patterns Repository et Service Layer**
- Contribuez au projet open source

tiOPF est un excellent choix pour les applications d'entreprise nécessitant une architecture solide, une maintenance à long terme, et une séparation claire des responsabilités.

## Lecture de la configuration

### Gestionnaire de configuration

```pascal
unit AppConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, tiOPFManager;

type
  TAppConfig = class
  private
    FIniFile: TIniFile;
    FConfigFile: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitializePersistenceLayer;
    function GetDatabaseType: string;
    function GetLogEnabled: Boolean;
    function GetCacheEnabled: Boolean;
    function GetBatchSize: Integer;
  end;

var
  AppConfig: TAppConfig;

implementation

constructor TAppConfig.Create;  
begin
  inherited Create;
  FConfigFile := ExtractFilePath(ParamStr(0)) + 'config.ini';
  FIniFile := TIniFile.Create(FConfigFile);
end;

destructor TAppConfig.Destroy;  
begin
  FIniFile.Free;
  inherited Destroy;
end;

procedure TAppConfig.InitializePersistenceLayer;  
var
  DBType: string;
  Host, Database, User, Password, Params: string;
  Port: Integer;
begin
  DBType := FIniFile.ReadString('Database', 'Type', 'PostgreSQL');

  case DBType of
    'PostgreSQL':
    begin
      Host := FIniFile.ReadString('PostgreSQL', 'Host', 'localhost');
      Port := FIniFile.ReadInteger('PostgreSQL', 'Port', 5432);
      Database := FIniFile.ReadString('PostgreSQL', 'Database', '');
      User := FIniFile.ReadString('PostgreSQL', 'User', 'postgres');
      Password := FIniFile.ReadString('PostgreSQL', 'Password', '');
      Params := FIniFile.ReadString('PostgreSQL', 'Params', '');

      GTIOPFManager.ConnectDatabase(
        'PostgreSQL',
        Host,
        Database,
        User,
        Password,
        Params
      );
    end;

    'MySQL':
    begin
      Host := FIniFile.ReadString('MySQL', 'Host', 'localhost');
      Port := FIniFile.ReadInteger('MySQL', 'Port', 3306);
      Database := FIniFile.ReadString('MySQL', 'Database', '');
      User := FIniFile.ReadString('MySQL', 'User', 'root');
      Password := FIniFile.ReadString('MySQL', 'Password', '');
      Params := FIniFile.ReadString('MySQL', 'Params', '');

      GTIOPFManager.ConnectDatabase(
        'MySQL',
        Host,
        Database,
        User,
        Password,
        Params
      );
    end;

    'SQLite':
    begin
      Database := FIniFile.ReadString('SQLite', 'Database', 'data/app.db');

      // Créer le répertoire si nécessaire
      ForceDirectories(ExtractFilePath(Database));

      GTIOPFManager.ConnectDatabase(
        'SQLite',
        '',
        Database,
        '',
        '',
        ''
      );
    end;

    else
      raise Exception.CreateFmt('Type de base de données non supporté: %s', [DBType]);
  end;

  WriteLn(Format('Connecté à %s: %s', [DBType, Database]));
end;

function TAppConfig.GetDatabaseType: string;  
begin
  Result := FIniFile.ReadString('Database', 'Type', 'PostgreSQL');
end;

function TAppConfig.GetLogEnabled: Boolean;  
begin
  Result := FIniFile.ReadBool('Logging', 'Enabled', True);
end;

function TAppConfig.GetCacheEnabled: Boolean;  
begin
  Result := FIniFile.ReadBool('Application', 'CacheEnabled', True);
end;

function TAppConfig.GetBatchSize: Integer;  
begin
  Result := FIniFile.ReadInteger('Application', 'BatchSize', 100);
end;

initialization
  AppConfig := TAppConfig.Create;

finalization
  AppConfig.Free;

end.
```

## Utilitaires et helpers

### Helper pour TtiObject

```pascal
unit Helpers.tiObjectHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject;

type
  TtiObjectHelper = class helper for TtiObject
  public
    function ToJSON: string;
    procedure FromJSON(const AJSON: string);
    function Clone: TtiObject;
    function GetPropertyValue(const PropName: string): Variant;
    procedure SetPropertyValue(const PropName: string; const Value: Variant);
  end;

implementation

uses
  TypInfo, Variants, fpjson, jsonparser;

function TtiObjectHelper.ToJSON: string;  
var
  JSON: TJSONObject;
  PropList: PPropList;
  PropCount, i: Integer;
  PropInfo: PPropInfo;
  PropValue: Variant;
begin
  JSON := TJSONObject.Create;
  try
    // OID
    JSON.Add('oid', Self.OID.AsString);

    // Propriétés
    PropCount := GetPropList(Self, PropList);
    try
      for i := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[i];

        case PropInfo^.PropType^.Kind of
          tkInteger, tkInt64:
            JSON.Add(PropInfo^.Name, GetOrdProp(Self, PropInfo));
          tkFloat:
            JSON.Add(PropInfo^.Name, GetFloatProp(Self, PropInfo));
          tkString, tkLString, tkAString:
            JSON.Add(PropInfo^.Name, GetStrProp(Self, PropInfo));
          tkBool:
            JSON.Add(PropInfo^.Name, Boolean(GetOrdProp(Self, PropInfo)));
        end;
      end;
    finally
      FreeMem(PropList);
    end;

    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

procedure TtiObjectHelper.FromJSON(const AJSON: string);  
var
  JSON: TJSONData;
  JSONObj: TJSONObject;
  i: Integer;
  PropName: string;
  PropInfo: PPropInfo;
begin
  JSON := GetJSON(AJSON);
  try
    if JSON is TJSONObject then
    begin
      JSONObj := TJSONObject(JSON);

      // OID
      if JSONObj.Find('oid') <> nil then
        Self.OID.AsString := JSONObj.Get('oid', '');

      // Propriétés
      for i := 0 to JSONObj.Count - 1 do
      begin
        PropName := JSONObj.Names[i];
        if PropName = 'oid' then
          Continue;

        PropInfo := GetPropInfo(Self, PropName);
        if Assigned(PropInfo) then
        begin
          case PropInfo^.PropType^.Kind of
            tkInteger, tkInt64:
              SetOrdProp(Self, PropInfo, JSONObj.Get(PropName, 0));
            tkFloat:
              SetFloatProp(Self, PropInfo, JSONObj.Get(PropName, 0.0));
            tkString, tkLString, tkAString:
              SetStrProp(Self, PropInfo, JSONObj.Get(PropName, ''));
            tkBool:
              SetOrdProp(Self, PropInfo, Ord(JSONObj.Get(PropName, False)));
          end;
        end;
      end;
    end;
  finally
    JSON.Free;
  end;
end;

function TtiObjectHelper.Clone: TtiObject;  
begin
  Result := TtiObjectClass(Self.ClassType).Create;
  Result.Assign(Self);
end;

function TtiObjectHelper.GetPropertyValue(const PropName: string): Variant;  
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Self, PropName);
  if not Assigned(PropInfo) then
  begin
    Result := Null;
    Exit;
  end;

  case PropInfo^.PropType^.Kind of
    tkInteger, tkInt64:
      Result := GetOrdProp(Self, PropInfo);
    tkFloat:
      Result := GetFloatProp(Self, PropInfo);
    tkString, tkLString, tkAString:
      Result := GetStrProp(Self, PropInfo);
    tkBool:
      Result := Boolean(GetOrdProp(Self, PropInfo));
    else
      Result := Null;
  end;
end;

procedure TtiObjectHelper.SetPropertyValue(const PropName: string;
  const Value: Variant);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Self, PropName);
  if not Assigned(PropInfo) then
    Exit;

  case PropInfo^.PropType^.Kind of
    tkInteger, tkInt64:
      SetOrdProp(Self, PropInfo, Value);
    tkFloat:
      SetFloatProp(Self, PropInfo, Value);
    tkString, tkLString, tkAString:
      SetStrProp(Self, PropInfo, VarToStr(Value));
    tkBool:
      SetOrdProp(Self, PropInfo, Ord(Boolean(Value)));
  end;
end;

end.
```

### Utilisation du helper

```pascal
procedure UtiliserHelper;  
var
  Client: TClient;
  JSON: string;
  Clone: TClient;
begin
  Client := TClient.Create;
  try
    Client.Nom := 'Dupont';
    Client.Prenom := 'Jean';
    Client.Email := 'jean@test.com';

    // Conversion en JSON
    JSON := Client.ToJSON;
    WriteLn('JSON: ', JSON);

    // Clonage
    Clone := Client.Clone as TClient;
    try
      WriteLn('Clone: ', Clone.NomComplet);
    finally
      Clone.Free;
    end;

    // Accès dynamique aux propriétés
    WriteLn('Nom (dynamique): ', Client.GetPropertyValue('Nom'));
    Client.SetPropertyValue('Ville', 'Paris');
    WriteLn('Ville: ', Client.Ville);
  finally
    Client.Free;
  end;
end;
```

## Génération de rapports

### Générateur de rapports simple

```pascal
unit Reports.ClientReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Model.Client, Services.ClientService;

type
  TClientReportFormat = (rfText, rfHTML, rfCSV);

  TClientReport = class
  private
    FService: TClientService;

    function GenerateTextReport(Clients: TList<TClient>): string;
    function GenerateHTMLReport(Clients: TList<TClient>): string;
    function GenerateCSVReport(Clients: TList<TClient>): string;
  public
    constructor Create(AService: TClientService);

    procedure GenerateReport(const AFileName: string;
      Format: TClientReportFormat);
    procedure GenerateReportByVille(const AVille, AFileName: string;
      Format: TClientReportFormat);
  end;

implementation

uses
  DateUtils;

constructor TClientReport.Create(AService: TClientService);  
begin
  inherited Create;
  FService := AService;
end;

function TClientReport.GenerateTextReport(Clients: TList<TClient>): string;  
var
  Client: TClient;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('========================================');
    Lines.Add('        RAPPORT CLIENTS');
    Lines.Add('========================================');
    Lines.Add('');
    Lines.Add(Format('Date du rapport: %s',
      [FormatDateTime('dd/mm/yyyy hh:nn', Now)]));
    Lines.Add(Format('Nombre de clients: %d', [Clients.Count]));
    Lines.Add('');
    Lines.Add('----------------------------------------');
    Lines.Add('');

    for Client in Clients do
    begin
      Lines.Add(Format('Nom: %s %s', [Client.Prenom, Client.Nom]));
      Lines.Add(Format('Email: %s', [Client.Email]));
      Lines.Add(Format('Ville: %s', [Client.Ville]));
      Lines.Add(Format('Téléphone: %s', [Client.Telephone]));
      Lines.Add(Format('Actif: %s', [IfThen(Client.Actif, 'Oui', 'Non')]));
      Lines.Add(Format('Inscrit le: %s',
        [FormatDateTime('dd/mm/yyyy', Client.DateInscription)]));
      Lines.Add('----------------------------------------');
      Lines.Add('');
    end;

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TClientReport.GenerateHTMLReport(Clients: TList<TClient>): string;  
var
  Client: TClient;
  HTML: TStringList;
begin
  HTML := TStringList.Create;
  try
    HTML.Add('<!DOCTYPE html>');
    HTML.Add('<html>');
    HTML.Add('<head>');
    HTML.Add('    <meta charset="UTF-8">');
    HTML.Add('    <title>Rapport Clients</title>');
    HTML.Add('    <style>');
    HTML.Add('        body { font-family: Arial, sans-serif; margin: 20px; }');
    HTML.Add('        h1 { color: #333; }');
    HTML.Add('        table { border-collapse: collapse; width: 100%; }');
    HTML.Add('        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }');
    HTML.Add('        th { background-color: #4CAF50; color: white; }');
    HTML.Add('        tr:nth-child(even) { background-color: #f2f2f2; }');
    HTML.Add('        .actif { color: green; font-weight: bold; }');
    HTML.Add('        .inactif { color: red; }');
    HTML.Add('    </style>');
    HTML.Add('</head>');
    HTML.Add('<body>');
    HTML.Add('    <h1>Rapport Clients</h1>');
    HTML.Add(Format('    <p>Date du rapport: %s</p>',
      [FormatDateTime('dd/mm/yyyy hh:nn', Now)]));
    HTML.Add(Format('    <p>Nombre de clients: %d</p>', [Clients.Count]));
    HTML.Add('    <table>');
    HTML.Add('        <tr>');
    HTML.Add('            <th>Nom</th>');
    HTML.Add('            <th>Prénom</th>');
    HTML.Add('            <th>Email</th>');
    HTML.Add('            <th>Ville</th>');
    HTML.Add('            <th>Téléphone</th>');
    HTML.Add('            <th>Statut</th>');
    HTML.Add('            <th>Date inscription</th>');
    HTML.Add('        </tr>');

    for Client in Clients do
    begin
      HTML.Add('        <tr>');
      HTML.Add(Format('            <td>%s</td>', [Client.Nom]));
      HTML.Add(Format('            <td>%s</td>', [Client.Prenom]));
      HTML.Add(Format('            <td>%s</td>', [Client.Email]));
      HTML.Add(Format('            <td>%s</td>', [Client.Ville]));
      HTML.Add(Format('            <td>%s</td>', [Client.Telephone]));
      HTML.Add(Format('            <td class="%s">%s</td>',
        [IfThen(Client.Actif, 'actif', 'inactif'),
         IfThen(Client.Actif, 'Actif', 'Inactif')]));
      HTML.Add(Format('            <td>%s</td>',
        [FormatDateTime('dd/mm/yyyy', Client.DateInscription)]));
      HTML.Add('        </tr>');
    end;

    HTML.Add('    </table>');
    HTML.Add('</body>');
    HTML.Add('</html>');

    Result := HTML.Text;
  finally
    HTML.Free;
  end;
end;

function TClientReport.GenerateCSVReport(Clients: TList<TClient>): string;  
var
  Client: TClient;
  CSV: TStringList;
begin
  CSV := TStringList.Create;
  try
    // En-tête
    CSV.Add('Nom;Prenom;Email;Ville;Telephone;Actif;Date_Inscription');

    // Données
    for Client in Clients do
    begin
      CSV.Add(Format('%s;%s;%s;%s;%s;%s;%s',
        [Client.Nom,
         Client.Prenom,
         Client.Email,
         Client.Ville,
         Client.Telephone,
         IfThen(Client.Actif, 'Oui', 'Non'),
         FormatDateTime('dd/mm/yyyy', Client.DateInscription)]));
    end;

    Result := CSV.Text;
  finally
    CSV.Free;
  end;
end;

procedure TClientReport.GenerateReport(const AFileName: string;
  Format: TClientReportFormat);
var
  Clients: TList<TClient>;
  Content: string;
begin
  Clients := FService.ObtenirTous;
  try
    case Format of
      rfText: Content := GenerateTextReport(Clients);
      rfHTML: Content := GenerateHTMLReport(Clients);
      rfCSV: Content := GenerateCSVReport(Clients);
    end;

    with TStringList.Create do
    try
      Text := Content;
      SaveToFile(AFileName);
      WriteLn(Format('Rapport généré: %s', [AFileName]));
    finally
      Free;
    end;
  finally
    for var Client in Clients do
      Client.Free;
    Clients.Free;
  end;
end;

procedure TClientReport.GenerateReportByVille(const AVille, AFileName: string;
  Format: TClientReportFormat);
var
  Clients: TList<TClient>;
  Content: string;
begin
  Clients := FService.ObtenirParVille(AVille);
  try
    case Format of
      rfText: Content := GenerateTextReport(Clients);
      rfHTML: Content := GenerateHTMLReport(Clients);
      rfCSV: Content := GenerateCSVReport(Clients);
    end;

    with TStringList.Create do
    try
      Text := Content;
      SaveToFile(AFileName);
    finally
      Free;
    end;
  finally
    for var Client in Clients do
      Client.Free;
    Clients.Free;
  end;
end;

end.
```

### Utilisation du générateur de rapports

```pascal
procedure GenererRapports;  
var
  Service: TClientService;
  Report: TClientReport;
begin
  Service := TClientService.Create;
  try
    Report := TClientReport.Create(Service);
    try
      // Rapport texte
      Report.GenerateReport('rapport_clients.txt', rfText);

      // Rapport HTML
      Report.GenerateReport('rapport_clients.html', rfHTML);

      // Rapport CSV
      Report.GenerateReport('rapport_clients.csv', rfCSV);

      // Rapport par ville
      Report.GenerateReportByVille('Paris', 'rapport_paris.html', rfHTML);

      WriteLn('Tous les rapports ont été générés');
    finally
      Report.Free;
    end;
  finally
    Service.Free;
  end;
end;
```

## Audit Trail (historique des modifications)

### Table d'audit

```sql
-- PostgreSQL
CREATE TABLE audit_log (
    id SERIAL PRIMARY KEY,
    table_name VARCHAR(100) NOT NULL,
    record_oid VARCHAR(36) NOT NULL,
    operation VARCHAR(10) NOT NULL, -- INSERT, UPDATE, DELETE
    old_values TEXT,
    new_values TEXT,
    user_name VARCHAR(100),
    changed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_audit_table ON audit_log(table_name);  
CREATE INDEX idx_audit_oid ON audit_log(record_oid);  
CREATE INDEX idx_audit_date ON audit_log(changed_at);
```

### Gestionnaire d'audit

```pascal
unit Utils.AuditManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject, tiQuery, tiOPFManager;

type
  TAuditOperation = (aoInsert, aoUpdate, aoDelete);

  TAuditManager = class
  private
    class function OperationToString(Op: TAuditOperation): string;
  public
    class procedure LogChange(AObject: TtiObject; Operation: TAuditOperation;
      const OldValues, NewValues: string);
    class function GetHistory(const TableName, RecordOID: string): TStringList;
  end;

implementation

uses
  fpjson;

class function TAuditManager.OperationToString(Op: TAuditOperation): string;  
begin
  case Op of
    aoInsert: Result := 'INSERT';
    aoUpdate: Result := 'UPDATE';
    aoDelete: Result := 'DELETE';
  end;
end;

class procedure TAuditManager.LogChange(AObject: TtiObject;
  Operation: TAuditOperation; const OldValues, NewValues: string);
var
  Query: TtiQuery;
  TableName: string;
begin
  Query := GTIOPFManager.PersistenceLayerMgr.CreateTIQuery;
  try
    // Obtenir le nom de la table
    TableName := GTIOPFManager.ClassDBMappingMgr.GetTableName(
      AObject.ClassType);

    Query.SQL.Text :=
      'INSERT INTO audit_log (table_name, record_oid, operation, ' +
      '                       old_values, new_values, user_name) ' +
      'VALUES (:table_name, :record_oid, :operation, ' +
      '        :old_values, :new_values, :user_name)';

    Query.ParamAsString['table_name'] := TableName;
    Query.ParamAsString['record_oid'] := AObject.OID.AsString;
    Query.ParamAsString['operation'] := OperationToString(Operation);
    Query.ParamAsString['old_values'] := OldValues;
    Query.ParamAsString['new_values'] := NewValues;
    Query.ParamAsString['user_name'] := GetEnvironmentVariable('USERNAME');

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

class function TAuditManager.GetHistory(const TableName, RecordOID: string): TStringList;  
var
  Query: TtiQuery;
begin
  Result := TStringList.Create;
  Query := GTIOPFManager.PersistenceLayerMgr.CreateTIQuery;
  try
    Query.SQL.Text :=
      'SELECT operation, old_values, new_values, user_name, changed_at ' +
      'FROM audit_log ' +
      'WHERE table_name = :table_name AND record_oid = :record_oid ' +
      'ORDER BY changed_at DESC';

    Query.ParamAsString['table_name'] := TableName;
    Query.ParamAsString['record_oid'] := RecordOID;
    Query.Open;

    while not Query.EOF do
    begin
      Result.Add(Format('[%s] %s par %s',
        [FormatDateTime('dd/mm/yyyy hh:nn:ss',
          Query.FieldAsDateTime['changed_at']),
         Query.FieldAsString['operation'],
         Query.FieldAsString['user_name']]));

      if Query.FieldAsString['old_values'] <> '' then
        Result.Add('  Avant: ' + Query.FieldAsString['old_values']);

      if Query.FieldAsString['new_values'] <> '' then
        Result.Add('  Après: ' + Query.FieldAsString['new_values']);

      Result.Add('');
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
```

### Intégration de l'audit dans le Visitor

```pascal
procedure TVisClient.Insert(const AObject: TtiObject);  
var
  NewValues: string;
begin
  AObject.OID.CreateOID;

  Query.SQL.Text :=
    'INSERT INTO clients (oid, nom, prenom, email, telephone, ' +
    '                     ville, date_inscription, actif) ' +
    'VALUES (:oid, :nom, :prenom, :email, :telephone, ' +
    '        :ville, :date_inscription, :actif)';

  Query.ParamAsString['oid'] := AObject.OID.AsString;
  SetupParams;
  Query.ExecSQL;

  // Logger l'insertion
  NewValues := TClient(AObject).ToJSON;  // Utilise le helper
  TAuditManager.LogChange(AObject, aoInsert, '', NewValues);
end;

procedure TVisClient.Update(const AObject: TtiObject);  
var
  OldClient: TClient;
  OldValues, NewValues: string;
begin
  // Charger l'ancienne version
  OldClient := TClient.Create;
  try
    OldClient.OID.AsString := AObject.OID.AsString;
    OldClient.Read;
    OldValues := OldClient.ToJSON;
  finally
    OldClient.Free;
  end;

  // Mettre à jour
  Query.SQL.Text :=
    'UPDATE clients SET ' +
    '  nom = :nom, prenom = :prenom, email = :email, ' +
    '  telephone = :telephone, ville = :ville, ' +
    '  date_inscription = :date_inscription, actif = :actif ' +
    'WHERE oid = :oid';

  Query.ParamAsString['oid'] := AObject.OID.AsString;
  SetupParams;
  Query.ExecSQL;

  // Logger la modification
  NewValues := TClient(AObject).ToJSON;
  TAuditManager.LogChange(AObject, aoUpdate, OldValues, NewValues);
end;
```

## Performance : Requêtes optimisées

### Comptage optimisé

```pascal
function TClientRepository.CountActifs: Integer;  
var
  Query: TtiQuery;
begin
  Query := GTIOPFManager.PersistenceLayerMgr.CreateTIQuery;
  try
    Query.SQL.Text := 'SELECT COUNT(*) as total FROM clients WHERE actif = TRUE';
    Query.Open;
    Result := Query.FieldAsInteger['total'];
  finally
    Query.Free;
  end;
end;
```

### Pagination efficace

```pascal
type
  TPagedResult<T> = record
    Items: TList<T>;
    TotalCount: Integer;
    PageNumber: Integer;
    PageSize: Integer;
    TotalPages: Integer;
  end;

function TClientRepository.GetPaged(PageNumber, PageSize: Integer): TPagedResult<TClient>;  
var
  Query: TtiQuery;
  Client: TClient;
  Offset: Integer;
begin
  Result.Items := TList<TClient>.Create;
  Result.PageNumber := PageNumber;
  Result.PageSize := PageSize;

  Query := GTIOPFManager.PersistenceLayerMgr.CreateTIQuery;
  try
    // Compter le total
    Query.SQL.Text := 'SELECT COUNT(*) as total FROM clients';
    Query.Open;
    Result.TotalCount := Query.FieldAsInteger['total'];
    Result.TotalPages := (Result.TotalCount + PageSize - 1) div PageSize;
    Query.Close;

    // Récupérer la page
    Offset := (PageNumber - 1) * PageSize;
    Query.SQL.Text :=
      'SELECT * FROM clients ' +
      'ORDER BY nom, prenom ' +
      'LIMIT :limit OFFSET :offset';
    Query.ParamAsInteger['limit'] := PageSize;
    Query.ParamAsInteger['offset'] := Offset;
    Query.Open;

    while not Query.EOF do
    begin
      Client := TClient.Create;
      // Mapper les données...
      Result.Items.Add(Client);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;
```

### Utilisation de la pagination

```pascal
procedure AfficherClientsParPage;  
var
  Repository: TClientRepository;
  PagedResult: TPagedResult<TClient>;
  Client: TClient;
  PageNum: Integer;
begin
  Repository := TClientRepository.Create;
  try
    PageNum := 1;
    repeat
      PagedResult := Repository.GetPaged(PageNum, 10);
      try
        WriteLn(Format('=== Page %d/%d ===',
          [PagedResult.PageNumber, PagedResult.TotalPages]));

        for Client in PagedResult.Items do
          WriteLn(Format('%s - %s', [Client.NomComplet, Client.Email]));

        WriteLn;
        Write('Page suivante? (O/N): ');
        if UpperCase(ReadLn) <> 'O' then
          Break;

        Inc(PageNum);
      finally
        for Client in PagedResult.Items do
          Client.Free;
        PagedResult.Items.Free;
      end;
    until PageNum > PagedResult.TotalPages;
  finally
    Repository.Free;
  end;
end;
```

## Checklist de déploiement

### Avant le déploiement

- [ ] **Tests unitaires** : Tous les tests passent
- [ ] **Tests d'intégration** : Base de données testée
- [ ] **Configuration** : Fichier config.ini correct
- [ ] **Base de données** : Schéma créé et migrations appliquées
- [ ] **Bibliothèques** : Toutes les DLL/SO nécessaires présentes
- [ ] **Permissions** : Droits d'accès base de données configurés
- [ ] **Logging** : Répertoire logs/ créé avec permissions
- [ ] **Sauvegarde** : Procédure de backup en place
- [ ] **Documentation** : README et guide utilisateur à jour
- [ ] **Validation** : Tests sur environnement de staging

### Structure de déploiement Windows

```
MonApplication\
├── MonApp.exe
├── config.ini
├── libs\
│   ├── libpq.dll          # PostgreSQL
│   ├── libeay32.dll       # OpenSSL (si PostgreSQL)
│   ├── ssleay32.dll       # OpenSSL (si PostgreSQL)
│   ├── libmysql.dll       # MySQL (optionnel)
│   └── sqlite3.dll        # SQLite (optionnel)
├── data\                  # Pour SQLite ou fichiers locaux
├── logs\                  # Fichiers de log
├── reports\               # Rapports générés
├── backup\                # Sauvegardes
└── docs\
    ├── README.txt
    └── LICENCE.txt
```

### Structure de déploiement Linux

```
/opt/monapplication/
├── bin/
│   └── monapp             # Exécutable
├── etc/
│   └── config.ini         # Configuration
├── lib/
│   └── (bibliothèques si nécessaire)
├── var/
│   ├── data/              # Données applicatives
│   ├── logs/              # Logs
│   └── backup/            # Sauvegardes
├── share/
│   └── docs/              # Documentation
└── tmp/                   # Fichiers temporaires
```

### Script de déploiement Windows

```batch
@echo off
REM deploy_windows.bat

echo ========================================  
echo Deploiement de MonApplication  
echo ========================================  
echo.

REM Créer la structure de répertoires  
echo Creation des repertoires...  
if not exist ".\logs" mkdir ".\logs"  
if not exist ".\data" mkdir ".\data"  
if not exist ".\reports" mkdir ".\reports"  
if not exist ".\backup" mkdir ".\backup"

REM Copier les fichiers  
echo Copie des fichiers...  
copy /Y ".\build\MonApp.exe" ".\MonApp.exe"  
copy /Y ".\config\config.ini" ".\config.ini"

REM Copier les DLLs  
echo Copie des bibliotheques...  
if not exist ".\libs" mkdir ".\libs"  
copy /Y "C:\PostgreSQL\14\bin\libpq.dll" ".\libs\"  
copy /Y "C:\PostgreSQL\14\bin\libeay32.dll" ".\libs\"  
copy /Y "C:\PostgreSQL\14\bin\ssleay32.dll" ".\libs\"

REM Vérifier la connexion à la base  
echo Verification de la base de donnees...  
MonApp.exe --check-db

if %ERRORLEVEL% NEQ 0 (
    echo ERREUR: Impossible de se connecter a la base de donnees
    pause
    exit /b 1
)

echo.  
echo ========================================  
echo Deploiement termine avec succes!  
echo ========================================  
pause
```

### Script de déploiement Linux

```bash
#!/bin/bash
# deploy_linux.sh

set -e  # Arrêter en cas d'erreur

APP_NAME="monapp"  
INSTALL_DIR="/opt/monapplication"  
USER="monapplication"  
GROUP="monapplication"

echo "========================================"  
echo "Déploiement de $APP_NAME"  
echo "========================================"  
echo

# Créer l'utilisateur si nécessaire
if ! id "$USER" &>/dev/null; then
    echo "Création de l'utilisateur $USER..."
    sudo useradd -r -s /bin/false $USER
fi

# Créer la structure de répertoires
echo "Création des répertoires..."  
sudo mkdir -p $INSTALL_DIR/{bin,etc,lib,var/{data,logs,backup},share/docs,tmp}

# Copier les fichiers
echo "Copie des fichiers..."  
sudo cp ./build/$APP_NAME $INSTALL_DIR/bin/  
sudo cp ./config/config.ini $INSTALL_DIR/etc/  
sudo cp -r ./docs/* $INSTALL_DIR/share/docs/

# Permissions
echo "Configuration des permissions..."  
sudo chown -R $USER:$GROUP $INSTALL_DIR  
sudo chmod 755 $INSTALL_DIR/bin/$APP_NAME  
sudo chmod 644 $INSTALL_DIR/etc/config.ini  
sudo chmod 755 $INSTALL_DIR/var/{data,logs,backup}

# Créer un lien symbolique
echo "Création du lien symbolique..."  
sudo ln -sf $INSTALL_DIR/bin/$APP_NAME /usr/local/bin/$APP_NAME

# Vérifier la connexion à la base
echo "Vérification de la base de données..."  
sudo -u $USER $INSTALL_DIR/bin/$APP_NAME --check-db

if [ $? -ne 0 ]; then
    echo "ERREUR: Impossible de se connecter à la base de données"
    exit 1
fi

echo  
echo "========================================"  
echo "Déploiement terminé avec succès!"  
echo "========================================"  
echo "Pour démarrer: sudo systemctl start $APP_NAME"
```

### Service systemd (Linux)

```ini
# /etc/systemd/system/monapplication.service

[Unit]
Description=Mon Application tiOPF  
After=network.target postgresql.service  
Wants=postgresql.service

[Service]
Type=simple  
User=monapplication  
Group=monapplication  
WorkingDirectory=/opt/monapplication  
ExecStart=/opt/monapplication/bin/monapp  
Restart=on-failure  
RestartSec=10  
StandardOutput=journal  
StandardError=journal

# Variables d'environnement
Environment="APP_ENV=production"  
Environment="CONFIG_FILE=/opt/monapplication/etc/config.ini"

# Limites de sécurité
NoNewPrivileges=true  
PrivateTmp=true  
ProtectSystem=strict  
ProtectHome=true  
ReadWritePaths=/opt/monapplication/var

[Install]
WantedBy=multi-user.target
```

### Installation du service

```bash
# Copier le fichier service
sudo cp monapplication.service /etc/systemd/system/

# Recharger systemd
sudo systemctl daemon-reload

# Activer le service au démarrage
sudo systemctl enable monapplication

# Démarrer le service
sudo systemctl start monapplication

# Vérifier le statut
sudo systemctl status monapplication

# Voir les logs
sudo journalctl -u monapplication -f
```

## Sauvegarde et restauration

### Script de sauvegarde PostgreSQL

```bash
#!/bin/bash
# backup_postgres.sh

DB_NAME="gestion_clients"  
DB_USER="postgres"  
BACKUP_DIR="/opt/monapplication/var/backup"  
DATE=$(date +%Y%m%d_%H%M%S)  
BACKUP_FILE="$BACKUP_DIR/${DB_NAME}_${DATE}.sql.gz"

# Créer le répertoire si nécessaire
mkdir -p $BACKUP_DIR

# Sauvegarde
echo "Sauvegarde de la base $DB_NAME..."  
pg_dump -U $DB_USER $DB_NAME | gzip > $BACKUP_FILE

if [ $? -eq 0 ]; then
    echo "Sauvegarde réussie: $BACKUP_FILE"

    # Supprimer les sauvegardes de plus de 30 jours
    find $BACKUP_DIR -name "*.sql.gz" -mtime +30 -delete
    echo "Anciennes sauvegardes supprimées"
else
    echo "ERREUR lors de la sauvegarde"
    exit 1
fi
```

### Script de restauration

```bash
#!/bin/bash
# restore_postgres.sh

if [ -z "$1" ]; then
    echo "Usage: $0 <fichier_sauvegarde.sql.gz>"
    exit 1
fi

DB_NAME="gestion_clients"  
DB_USER="postgres"  
BACKUP_FILE="$1"

if [ ! -f "$BACKUP_FILE" ]; then
    echo "Fichier introuvable: $BACKUP_FILE"
    exit 1
fi

echo "ATTENTION: Cette opération va ECRASER la base $DB_NAME"  
read -p "Continuer? (oui/non): " confirm

if [ "$confirm" != "oui" ]; then
    echo "Restauration annulée"
    exit 0
fi

# Déconnecter tous les utilisateurs
psql -U $DB_USER -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='$DB_NAME';"

# Supprimer et recréer la base
dropdb -U $DB_USER $DB_NAME  
createdb -U $DB_USER $DB_NAME

# Restaurer
echo "Restauration en cours..."  
gunzip -c $BACKUP_FILE | psql -U $DB_USER $DB_NAME

if [ $? -eq 0 ]; then
    echo "Restauration réussie"
else
    echo "ERREUR lors de la restauration"
    exit 1
fi
```

### Tâche planifiée (Cron)

```bash
# Ajouter au crontab: crontab -e

# Sauvegarde quotidienne à 2h du matin
0 2 * * * /opt/monapplication/bin/backup_postgres.sh >> /opt/monapplication/var/logs/backup.log 2>&1

# Nettoyage des logs hebdomadaire (dimanche à 3h)
0 3 * * 0 find /opt/monapplication/var/logs -name "*.log" -mtime +7 -delete
```

## Monitoring et maintenance

### Script de monitoring

```pascal
unit Utils.HealthCheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiOPFManager;

type
  THealthStatus = (hsHealthy, hsWarning, hsCritical);

  THealthCheck = class
  public
    class function CheckDatabase: THealthStatus;
    class function CheckDiskSpace: THealthStatus;
    class function CheckMemory: THealthStatus;
    class function CheckLogSize: THealthStatus;
    class function GenerateReport: string;
  end;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  Unix, BaseUnix,
  {$ENDIF}
  tiQuery;

class function THealthCheck.CheckDatabase: THealthStatus;  
var
  Query: TtiQuery;
begin
  Result := hsCritical;
  try
    Query := GTIOPFManager.PersistenceLayerMgr.CreateTIQuery;
    try
      Query.SQL.Text := 'SELECT 1';
      Query.Open;
      Result := hsHealthy;
    finally
      Query.Free;
    end;
  except
    Result := hsCritical;
  end;
end;

class function THealthCheck.CheckDiskSpace: THealthStatus;
{$IFDEF WINDOWS}
var
  FreeBytesAvailable, TotalBytes, FreeBytes: Int64;
  PercentFree: Double;
begin
  if GetDiskFreeSpaceEx(PChar('C:\'), FreeBytesAvailable, TotalBytes, @FreeBytes) then
  begin
    PercentFree := (FreeBytes / TotalBytes) * 100;
    if PercentFree < 5 then
      Result := hsCritical
    else if PercentFree < 10 then
      Result := hsWarning
    else
      Result := hsHealthy;
  end
  else
    Result := hsWarning;
end;
{$ENDIF}
{$IFDEF UNIX}
var
  StatBuf: TStatFS;
  PercentFree: Double;
begin
  if fpStatFS('/', @StatBuf) = 0 then
  begin
    PercentFree := (StatBuf.bavail / StatBuf.blocks) * 100;
    if PercentFree < 5 then
      Result := hsCritical
    else if PercentFree < 10 then
      Result := hsWarning
    else
      Result := hsHealthy;
  end
  else
    Result := hsWarning;
end;
{$ENDIF}

class function THealthCheck.CheckMemory: THealthStatus;
{$IFDEF WINDOWS}
var
  MemStatus: TMemoryStatus;
  PercentUsed: Double;
begin
  MemStatus.dwLength := SizeOf(MemStatus);
  GlobalMemoryStatus(MemStatus);
  PercentUsed := (1 - (MemStatus.dwAvailPhys / MemStatus.dwTotalPhys)) * 100;

  if PercentUsed > 90 then
    Result := hsCritical
  else if PercentUsed > 80 then
    Result := hsWarning
  else
    Result := hsHealthy;
end;
{$ENDIF}
{$IFDEF UNIX}
var
  Info: TSysInfo;
  PercentUsed: Double;
begin
  if SysInfo(@Info) = 0 then
  begin
    PercentUsed := (1 - (Info.freeram / Info.totalram)) * 100;
    if PercentUsed > 90 then
      Result := hsCritical
    else if PercentUsed > 80 then
      Result := hsWarning
    else
      Result := hsHealthy;
  end
  else
    Result := hsWarning;
end;
{$ENDIF}

class function THealthCheck.CheckLogSize: THealthStatus;  
var
  LogDir: string;
  TotalSize: Int64;
  SearchRec: TSearchRec;
begin
  LogDir := ExtractFilePath(ParamStr(0)) + 'logs' + PathDelim;
  TotalSize := 0;

  if FindFirst(LogDir + '*.log', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      Inc(TotalSize, SearchRec.Size);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  // Taille en Mo
  TotalSize := TotalSize div (1024 * 1024);

  if TotalSize > 1000 then  // > 1 Go
    Result := hsCritical
  else if TotalSize > 500 then  // > 500 Mo
    Result := hsWarning
  else
    Result := hsHealthy;
end;

class function THealthCheck.GenerateReport: string;  
const
  StatusStr: array[THealthStatus] of string = ('OK', 'AVERTISSEMENT', 'CRITIQUE');
var
  Report: TStringList;
  DBStatus, DiskStatus, MemStatus, LogStatus: THealthStatus;
begin
  Report := TStringList.Create;
  try
    Report.Add('========================================');
    Report.Add('RAPPORT DE SANTE SYSTEME');
    Report.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));
    Report.Add('========================================');
    Report.Add('');

    // Base de données
    DBStatus := CheckDatabase;
    Report.Add(Format('Base de données: %s', [StatusStr[DBStatus]]));

    // Espace disque
    DiskStatus := CheckDiskSpace;
    Report.Add(Format('Espace disque: %s', [StatusStr[DiskStatus]]));

    // Mémoire
    MemStatus := CheckMemory;
    Report.Add(Format('Mémoire: %s', [StatusStr[MemStatus]]));

    // Logs
    LogStatus := CheckLogSize;
    Report.Add(Format('Logs: %s', [StatusStr[LogStatus]]));

    Report.Add('');
    Report.Add('========================================');

    // Statut global
    if (DBStatus = hsCritical) or (DiskStatus = hsCritical) or
       (MemStatus = hsCritical) or (LogStatus = hsCritical) then
      Report.Add('STATUT GLOBAL: CRITIQUE')
    else if (DBStatus = hsWarning) or (DiskStatus = hsWarning) or
            (MemStatus = hsWarning) or (LogStatus = hsWarning) then
      Report.Add('STATUT GLOBAL: AVERTISSEMENT')
    else
      Report.Add('STATUT GLOBAL: OK');

    Result := Report.Text;
  finally
    Report.Free;
  end;
end;

end.
```

### Utilisation du health check

```pascal
program HealthCheckApp;

{$mode objfpc}{$H+}

uses
  SysUtils, Utils.HealthCheck, AppConfig;

begin
  try
    // Initialiser
    AppConfig.InitializePersistenceLayer;

    // Générer et afficher le rapport
    WriteLn(THealthCheck.GenerateReport);

    // Envoyer par email si critique (à implémenter)
    // ...

  except
    on E: Exception do
    begin
      WriteLn('ERREUR: ', E.Message);
      Halt(1);
    end;
  end;
end.
```

## Résolution de problèmes courants

### Problème : "Impossible de se connecter à la base de données"

**Diagnostic :**
```pascal
procedure DiagnosticConnexion;  
begin
  WriteLn('=== Diagnostic de connexion ===');

  // Vérifier la configuration
  WriteLn('Type BD: ', AppConfig.GetDatabaseType);

  // Tester la connexion
  try
    GTIOPFManager.ConnectDatabase(...);
    WriteLn('Connexion: OK');
  except
    on E: Exception do
      WriteLn('Connexion: ECHEC - ', E.Message);
  end;

  // Vérifier les bibliothèques
  {$IFDEF WINDOWS}
  if FileExists('libs\libpq.dll') then
    WriteLn('libpq.dll: Présent')
  else
    WriteLn('libpq.dll: MANQUANT');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('Recherche de libpq.so...');
  // Vérifier dans /usr/lib, /usr/local/lib, etc.
  {$ENDIF}
end;
```

**Solutions :**
1. Vérifier le fichier `config.ini`
2. Vérifier que le serveur de base de données est démarré
3. Vérifier les credentials (utilisateur/mot de passe)
4. Vérifier les bibliothèques clientes (DLL/SO)
5. Vérifier les permissions réseau/firewall

### Problème : "Erreur OID.CreateOID"

**Cause :** Problème de génération d'OID unique

**Solution :**
```pascal
// Vérifier que l'OID n'existe pas déjà
procedure TVisClient.Insert(const AObject: TtiObject);  
var
  MaxRetries: Integer;
  Retry: Integer;
begin
  MaxRetries := 3;
  Retry := 0;

  repeat
    try
      AObject.OID.CreateOID;

      Query.SQL.Text := 'INSERT INTO clients ...';
      // ... reste du code ...

      Query.ExecSQL;
      Break;  // Succès
    except
      on E: Exception do
      begin
        Inc(Retry);
        if Retry >= MaxRetries then
          raise
        else
          Sleep(100);  // Attendre un peu avant de réessayer
      end;
    end;
  until False;
end;
```

### Problème : "Fuite mémoire"

**Diagnostic :**
```pascal
// Utiliser HeapTrc pour détecter les fuites
{$IFDEF DEBUG}
  {$DEFINE HEAPTRC}
{$ENDIF}

program MonApp;  
uses
  {$IFDEF HEAPTRC}
  heaptrc,
  {$ENDIF}
  // ... autres units
```

**Solutions :**
1. Toujours libérer les objets avec `try..finally`
2. Libérer les listes ET leur contenu
3. Vérifier les observers (détacher avant de libérer)
4. Utiliser des outils comme Valgrind (Linux)

### Problème : "Performances lentes"

**Diagnostic :**
```pascal
uses
  tiProfiler;

procedure ProfilerRequete;  
var
  Profiler: TtiProfiler;
begin
  Profiler := TtiProfiler.Create;
  try
    Profiler.Start;

    // Code à profiler
    Clients := Service.ObtenirTous;

    Profiler.Stop;
    WriteLn('Temps écoulé: ', Profiler.ElapsedTime, ' ms');
  finally
    Profiler.Free;
  end;
end;
```

**Solutions :**
1. Ajouter des index sur les colonnes fréquemment interrogées
2. Utiliser la pagination au lieu de charger tous les enregistrements
3. Activer le cache pour les objets fréquemment accédés
4. Optimiser les requêtes SQL (EXPLAIN ANALYZE)
5. Utiliser des requêtes personnalisées au lieu de charger tous les champs

## FAQ (Foire Aux Questions)

### Q1 : Puis-je utiliser tiOPF avec une base SQLite embarquée ?

**R :** Oui, tiOPF supporte parfaitement SQLite. Configuration :
```pascal
GTIOPFManager.ConnectDatabase(
  'SQLite',
  '',
  ExtractFilePath(ParamStr(0)) + 'data/ma_base.db',
  '', '', ''
);
```

### Q2 : Comment gérer plusieurs bases de données simultanément ?

**R :** Créez plusieurs instances de connexion :
```pascal
var
  Connection1, Connection2: TtiPersistenceLayer;
begin
  Connection1 := GTIOPFManager.CreatePersistenceLayer('PostgreSQL');
  Connection1.Connect('localhost', 'base1', ...);

  Connection2 := GTIOPFManager.CreatePersistenceLayer('MySQL');
  Connection2.Connect('localhost', 'base2', ...);
end;
```

### Q3 : tiOPF est-il thread-safe ?

**R :** Non par défaut. Pour utiliser tiOPF en multi-threading :
- Créez une connexion par thread
- Utilisez des sections critiques pour les objets partagés
- Évitez de partager les instances d'objets métier entre threads

### Q4 : Puis-je utiliser des ID auto-incrémentés au lieu d'OID ?

**R :** Oui, mais cela nécessite des modifications du framework. Il est recommandé d'utiliser les OID (GUID) car ils garantissent l'unicité même en environnement distribué.

### Q5 : Comment migrer une application Delphi/tiOPF vers Lazarus ?

**R :**
1. Installer tiOPF pour Lazarus
2. Convertir les fichiers .dfm en .lfm avec l'outil de Lazarus
3. Adapter les chemins de bibliothèques ({$IFDEF WINDOWS}/{$IFDEF LINUX})
4. Tester exhaustivement

## Ressources externes

### Documentation

- **Site officiel** : http://tiopf.sourceforge.net/
- **GitHub** : https://github.com/graemeg/tiopf
- **Wiki** : https://github.com/graemeg/tiopf/wiki
- **Forum** : https://sourceforge.net/p/tiopf/discussion/

### Tutoriels et articles

- "Getting Started with tiOPF" (documentation officielle)
- "tiOPF Architecture Explained" (articles de blog)
- Exemples dans `/Demos/` du dépôt GitHub

### Livres recommandés

- "Design Patterns" - Gang of Four
- "Domain-Driven Design" - Eric Evans
- "Patterns of Enterprise Application Architecture" - Martin Fowler
- "Clean Architecture" - Robert C. Martin

### Communauté FreePascal/Lazarus

- **Forum FreePascal** : https://forum.lazarus.freepascal.org/
- **Reddit r/freepascal** : https://www.reddit.com/r/freepascal/
- **Discord FreePascal** : Serveur communautaire

## Conclusion générale

tiOPF (TechInsite Object Persistence Framework) représente une solution mature et éprouvée pour le développement d'applications d'entreprise avec FreePascal et Lazarus. Après plus de 20 ans d'évolution, ce framework offre une architecture solide basée sur des patterns reconnus.

### Récapitulatif des points clés

**Architecture MVC complète**
- Séparation claire entre modèle, vue et contrôleur
- Pattern Observer intégré pour la réactivité
- Système de validation robuste

**Persistance objet avancée**
- Mapping objet-relationnel flexible
- Support de multiples SGBD
- Gestion automatique des états d'objets

**Multi-plateforme natif**
- Fonctionne identiquement sur Windows, Linux et macOS
- Abstraction complète des spécificités système
- Déploiement simplifié

**Extensibilité**
- Visitors personnalisables
- Hooks pour intégrer votre logique métier
- Architecture modulaire

### Quand utiliser tiOPF ?

tiOPF est particulièrement adapté pour :
- **Applications d'entreprise** avec logique métier complexe
- **Projets à long terme** nécessitant une architecture solide
- **Équipes expérimentées** en POO et patterns
- **Applications multi-plateformes** Windows/Linux/macOS
- **Systèmes nécessitant** audit trail et validation avancée

### Alternatives à considérer

Selon vos besoins, vous pouvez aussi évaluer :
- **ORM personnalisé** : Plus simple, mieux adapté à vos besoins spécifiques
- **ZEOS seul** : Pour des applications simples sans architecture MVC
- **mORMot** : Pour des performances maximales et SOA

### Mot de la fin

tiOPF démontre qu'il est possible de construire des applications d'entreprise robustes et maintenables avec FreePascal/Lazarus. Sa philosophie "Business Objects First" encourage les bonnes pratiques et une séparation claire des responsabilités.

Bien que la courbe d'apprentissage puisse sembler intimidante au début, l'investissement en vaut la peine pour des projets d'envergure. La structure imposée par tiOPF facilite la maintenance à long terme et l'évolution de l'application.

**Commencez petit**, explorez les exemples fournis, et progressivement adoptez les patterns plus avancés. La communauté tiOPF et FreePascal est accueillante et prête à aider les nouveaux venus.

Bonne chance dans vos projets avec tiOPF ! 🚀

---

*Ce tutoriel fait partie de la formation "FreePascal/Lazarus - Niveau Développeur Avancé" - Chapitre 8 : Bases de Données et ORM*

⏭️ [mORMot - SOA et ORM haute performance](/08-bases-donnees-orm-multiplatefomes/07-mormot-soa-orm-haute-performance.md)
