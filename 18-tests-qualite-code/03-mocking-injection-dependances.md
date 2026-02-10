🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.3 Mocking et injection de dépendances

## Introduction

Lorsque vous écrivez des tests, vous rencontrez souvent un problème : votre code dépend d'autres composants (base de données, services web, système de fichiers, etc.). Comment tester votre code de manière isolée sans dépendre de ces ressources externes ?

C'est là qu'interviennent deux concepts fondamentaux :
- **L'injection de dépendances** : une technique de conception qui rend votre code testable
- **Le mocking** : la création de "faux" objets qui simulent le comportement des vraies dépendances

### Pourquoi ces techniques sont essentielles ?

Sans ces techniques, vos tests :
- Sont **lents** (accès base de données, réseau)
- Sont **fragiles** (dépendent de ressources externes)
- Sont **difficiles à maintenir** (configuration complexe)
- Ne testent **pas qu'une seule chose** (mélange de responsabilités)

Avec ces techniques, vos tests deviennent :
- **Rapides** (millisecondes au lieu de secondes)
- **Fiables** (pas de dépendances externes)
- **Focalisés** (testent une seule unité de code)
- **Maintenables** (faciles à comprendre et modifier)

## Comprendre le problème : le code fortement couplé

### Exemple problématique

Voici un code typique avec des dépendances "en dur" :

```pascal
unit GestionnaireUtilisateurs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, mysql56conn;

type
  TGestionnaireUtilisateurs = class
  private
    FConnexion: TMySQL56Connection;
    FTransaction: TSQLTransaction;
  public
    constructor Create;
    destructor Destroy; override;

    function CreerUtilisateur(const Email, MotDePasse: String): Integer;
    function VerifierMotDePasse(const Email, MotDePasse: String): Boolean;
  end;

implementation

constructor TGestionnaireUtilisateurs.Create;  
begin
  // Dépendances créées DANS le constructeur
  FConnexion := TMySQL56Connection.Create(nil);
  FConnexion.HostName := 'localhost';
  FConnexion.DatabaseName := 'production_db';  // Base de production !
  FConnexion.UserName := 'root';
  FConnexion.Password := 'secret';

  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Database := FConnexion;
  FConnexion.Transaction := FTransaction;

  FConnexion.Open;
end;

destructor TGestionnaireUtilisateurs.Destroy;  
begin
  FTransaction.Free;
  FConnexion.Free;
  inherited;
end;

function TGestionnaireUtilisateurs.CreerUtilisateur(const Email, MotDePasse: String): Integer;  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnexion;
    Query.SQL.Text := 'INSERT INTO utilisateurs (email, password) VALUES (:email, :password)';
    Query.Params.ParamByName('email').AsString := Email;
    Query.Params.ParamByName('password').AsString := MotDePasse;
    Query.ExecSQL;
    FTransaction.Commit;
    Result := Query.LastInsertID;
  finally
    Query.Free;
  end;
end;

end.
```

### Problèmes de ce code

❌ **Impossible à tester unitairement** : Pour tester `CreerUtilisateur`, vous DEVEZ avoir :
- Un serveur MySQL en cours d'exécution
- Une base de données configurée
- Les bonnes permissions
- Une connexion réseau

❌ **Dangereux** : Les tests modifieraient la base de production !

❌ **Lent** : Chaque test prend plusieurs secondes

❌ **Inflexible** : Impossible de changer le type de base de données sans modifier le code

## Solution 1 : L'injection de dépendances

### Principe de base

Au lieu de créer les dépendances à l'intérieur de la classe, on les **injecte depuis l'extérieur**.

### Trois types d'injection

#### 1. Injection par constructeur (recommandée)

```pascal
type
  TGestionnaireUtilisateurs = class
  private
    FConnexion: TSQLConnection;      // Interface, pas implémentation
    FTransaction: TSQLTransaction;
  public
    // Les dépendances sont passées en paramètre
    constructor Create(AConnexion: TSQLConnection; ATransaction: TSQLTransaction);

    function CreerUtilisateur(const Email, MotDePasse: String): Integer;
  end;

constructor TGestionnaireUtilisateurs.Create(AConnexion: TSQLConnection;
  ATransaction: TSQLTransaction);
begin
  FConnexion := AConnexion;
  FTransaction := ATransaction;
  // Pas de création, juste assignation !
end;
```

**Avantages :**
- Les dépendances sont clairement visibles
- Impossibilité de créer l'objet sans ses dépendances
- Facilite les tests

#### 2. Injection par propriété

```pascal
type
  TGestionnaireUtilisateurs = class
  private
    FConnexion: TSQLConnection;
  public
    constructor Create;

    property Connexion: TSQLConnection read FConnexion write FConnexion;

    function CreerUtilisateur(const Email, MotDePasse: String): Integer;
  end;

// Utilisation
Gestionnaire := TGestionnaireUtilisateurs.Create;  
Gestionnaire.Connexion := MaConnexion;  // Injection après création
```

**Avantages :**
- Flexibilité : peut créer l'objet et injecter plus tard
- Utile pour les dépendances optionnelles

**Inconvénients :**
- Risque d'oublier l'injection
- Moins explicite

#### 3. Injection par méthode

```pascal
function TGestionnaireUtilisateurs.CreerUtilisateur(
  AConnexion: TSQLConnection;
  const Email, MotDePasse: String): Integer;
begin
  // Utilise AConnexion passé en paramètre
  // ...
end;
```

**Avantages :**
- Utile quand la dépendance change à chaque appel

**Inconvénients :**
- Signatures de méthodes plus complexes

### Code refactorisé avec injection de dépendances

```pascal
unit GestionnaireUtilisateurs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb;

type
  TGestionnaireUtilisateurs = class
  private
    FConnexion: TSQLConnection;
    FTransaction: TSQLTransaction;
  public
    // Injection par constructeur
    constructor Create(AConnexion: TSQLConnection; ATransaction: TSQLTransaction);

    function CreerUtilisateur(const Email, MotDePasse: String): Integer;
    function VerifierMotDePasse(const Email, MotDePasse: String): Boolean;
  end;

implementation

constructor TGestionnaireUtilisateurs.Create(AConnexion: TSQLConnection;
  ATransaction: TSQLTransaction);
begin
  if AConnexion = nil then
    raise Exception.Create('La connexion ne peut pas être nil');
  if ATransaction = nil then
    raise Exception.Create('La transaction ne peut pas être nil');

  FConnexion := AConnexion;
  FTransaction := ATransaction;
end;

function TGestionnaireUtilisateurs.CreerUtilisateur(const Email, MotDePasse: String): Integer;  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnexion;
    Query.Transaction := FTransaction;
    Query.SQL.Text := 'INSERT INTO utilisateurs (email, password) VALUES (:email, :password)';
    Query.Params.ParamByName('email').AsString := Email;
    Query.Params.ParamByName('password').AsString := MotDePasse;
    Query.ExecSQL;
    FTransaction.Commit;
    Result := Query.LastInsertID;
  finally
    Query.Free;
  end;
end;

function TGestionnaireUtilisateurs.VerifierMotDePasse(const Email, MotDePasse: String): Boolean;  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnexion;
    Query.Transaction := FTransaction;
    Query.SQL.Text := 'SELECT password FROM utilisateurs WHERE email = :email';
    Query.Params.ParamByName('email').AsString := Email;
    Query.Open;

    if Query.EOF then
      Result := False
    else
      Result := Query.FieldByName('password').AsString = MotDePasse;
  finally
    Query.Close;
    Query.Free;
  end;
end;

end.
```

### Utilisation en production

```pascal
var
  Connexion: TMySQL56Connection;
  Transaction: TSQLTransaction;
  Gestionnaire: TGestionnaireUtilisateurs;
begin
  // Créer les dépendances
  Connexion := TMySQL56Connection.Create(nil);
  try
    Connexion.HostName := 'localhost';
    Connexion.DatabaseName := 'production_db';
    Connexion.UserName := 'root';
    Connexion.Password := 'secret';

    Transaction := TSQLTransaction.Create(nil);
    try
      Transaction.Database := Connexion;
      Connexion.Transaction := Transaction;
      Connexion.Open;

      // Injecter les dépendances
      Gestionnaire := TGestionnaireUtilisateurs.Create(Connexion, Transaction);
      try
        // Utiliser
        Gestionnaire.CreerUtilisateur('test@example.com', 'password123');
      finally
        Gestionnaire.Free;
      end;

    finally
      Transaction.Free;
    end;
  finally
    Connexion.Free;
  end;
end;
```

### Utilisation en tests

```pascal
procedure TTestGestionnaireUtilisateurs.TestCreerUtilisateur;  
var
  Connexion: TSQLite3Connection;  // SQLite en mémoire pour les tests !
  Transaction: TSQLTransaction;
  Gestionnaire: TGestionnaireUtilisateurs;
  ID: Integer;
begin
  // Créer une base de données de test en mémoire
  Connexion := TSQLite3Connection.Create(nil);
  try
    Connexion.DatabaseName := ':memory:';

    Transaction := TSQLTransaction.Create(nil);
    try
      Transaction.Database := Connexion;
      Connexion.Transaction := Transaction;
      Connexion.Open;

      // Créer la structure de la base
      Connexion.ExecuteDirect('CREATE TABLE utilisateurs (id INTEGER PRIMARY KEY, email TEXT, password TEXT)');

      // Injecter les dépendances de test
      Gestionnaire := TGestionnaireUtilisateurs.Create(Connexion, Transaction);
      try
        // Tester
        ID := Gestionnaire.CreerUtilisateur('test@example.com', 'password123');

        AssertTrue('ID positif', ID > 0);

        // Vérifier que l'utilisateur existe
        AssertTrue('Vérification mot de passe',
          Gestionnaire.VerifierMotDePasse('test@example.com', 'password123'));
      finally
        Gestionnaire.Free;
      end;

    finally
      Transaction.Free;
    end;
  finally
    Connexion.Free;
  end;
end;
```

## Solution 2 : Les interfaces

### Problème restant

Même avec l'injection de dépendances, notre code dépend encore de classes concrètes (`TSQLConnection`, etc.). Pour un vrai découplage, utilisons des **interfaces**.

### Qu'est-ce qu'une interface ?

Une interface définit un **contrat** : quelles méthodes doivent être disponibles, sans dire comment elles sont implémentées.

```pascal
type
  // Interface pour la connexion base de données
  IDatabaseConnection = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure Open;
    procedure Close;
    function IsConnected: Boolean;
    function ExecuteQuery(const SQL: String): IResultSet;
  end;

  // Interface pour un résultat de requête
  IResultSet = interface
    ['{87654321-4321-4321-4321-CBA987654321}']
    function Next: Boolean;
    function GetFieldValue(const FieldName: String): String;
    function GetFieldAsInteger(const FieldName: String): Integer;
    property EOF: Boolean read GetEOF;
  end;
```

### Avantages des interfaces

✅ **Découplage total** : Le code ne dépend que du contrat, pas de l'implémentation

✅ **Substitution facile** : On peut remplacer l'implémentation sans changer le code

✅ **Tests simplifiés** : Facile de créer des implémentations de test

✅ **Flexibilité** : Plusieurs implémentations possibles (MySQL, PostgreSQL, Mock)

### Refactorisation avec interfaces

```pascal
unit InterfacesDatabase;

{$mode objfpc}{$H+}

interface

type
  // Interface pour les résultats
  IResultSet = interface
    ['{87654321-4321-4321-4321-CBA987654321}']
    function Next: Boolean;
    function GetString(const FieldName: String): String;
    function GetInteger(const FieldName: String): Integer;
    function IsEOF: Boolean;
  end;

  // Interface pour la connexion
  IDatabaseConnection = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure Open;
    procedure Close;
    function IsConnected: Boolean;
    function ExecuteNonQuery(const SQL: String; const Params: array of Variant): Integer;
    function ExecuteQuery(const SQL: String; const Params: array of Variant): IResultSet;
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
  end;

implementation

end.
```

```pascal
unit GestionnaireUtilisateurs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InterfacesDatabase;

type
  TGestionnaireUtilisateurs = class
  private
    FDB: IDatabaseConnection;  // Interface, pas classe concrète !
  public
    constructor Create(ADatabase: IDatabaseConnection);

    function CreerUtilisateur(const Email, MotDePasse: String): Integer;
    function VerifierMotDePasse(const Email, MotDePasse: String): Boolean;
  end;

implementation

constructor TGestionnaireUtilisateurs.Create(ADatabase: IDatabaseConnection);  
begin
  if ADatabase = nil then
    raise Exception.Create('La base de données ne peut pas être nil');

  FDB := ADatabase;
end;

function TGestionnaireUtilisateurs.CreerUtilisateur(const Email, MotDePasse: String): Integer;  
begin
  FDB.BeginTransaction;
  try
    Result := FDB.ExecuteNonQuery(
      'INSERT INTO utilisateurs (email, password) VALUES (?, ?)',
      [Email, MotDePasse]
    );
    FDB.Commit;
  except
    FDB.Rollback;
    raise;
  end;
end;

function TGestionnaireUtilisateurs.VerifierMotDePasse(const Email, MotDePasse: String): Boolean;  
var
  ResultSet: IResultSet;
begin
  ResultSet := FDB.ExecuteQuery(
    'SELECT password FROM utilisateurs WHERE email = ?',
    [Email]
  );

  if ResultSet.IsEOF then
    Result := False
  else
    Result := ResultSet.GetString('password') = MotDePasse;
end;

end.
```

## Le Mocking : créer des "faux" objets

### Qu'est-ce qu'un Mock ?

Un **mock** (ou "bouchon") est un objet qui :
- Implémente la même interface que l'objet réel
- Simule son comportement de manière contrôlée
- Permet de tester sans dépendances externes

### Types d'objets de test

#### 1. Dummy (Mannequin)

Objet passé mais jamais utilisé, juste pour satisfaire la signature :

```pascal
// On a besoin d'une connexion mais on ne l'utilise pas
Gestionnaire := TGestionnaire.Create(DummyConnexion);
```

#### 2. Stub (Bouchon)

Retourne des valeurs prédéfinies :

```pascal
type
  TStubDatabase = class(TInterfacedObject, IDatabaseConnection)
  public
    // Retourne toujours le même résultat
    function ExecuteQuery(const SQL: String; const Params: array of Variant): IResultSet;
  end;

function TStubDatabase.ExecuteQuery(const SQL: String; const Params: array of Variant): IResultSet;  
begin
  // Toujours retourner un résultat vide
  Result := TEmptyResultSet.Create;
end;
```

#### 3. Fake (Faux)

Implémentation simplifiée qui fonctionne :

```pascal
type
  TFakeDatabase = class(TInterfacedObject, IDatabaseConnection)
  private
    FData: TStringList;  // Stockage en mémoire
  public
    constructor Create;
    destructor Destroy; override;

    function ExecuteNonQuery(const SQL: String; const Params: array of Variant): Integer;
    function ExecuteQuery(const SQL: String; const Params: array of Variant): IResultSet;
  end;

// Implémentation qui stocke en mémoire plutôt qu'en base
constructor TFakeDatabase.Create;  
begin
  FData := TStringList.Create;
end;
```

#### 4. Mock (Simulacre)

Vérifie que les bonnes méthodes sont appelées avec les bons paramètres :

```pascal
type
  TMockDatabase = class(TInterfacedObject, IDatabaseConnection)
  private
    FExecuteNonQueryCalled: Boolean;
    FLastSQL: String;
    FLastParams: array of Variant;
  public
    function ExecuteNonQuery(const SQL: String; const Params: array of Variant): Integer;

    // Méthodes de vérification
    function WasExecuteNonQueryCalled: Boolean;
    function GetLastSQL: String;
  end;

function TMockDatabase.ExecuteNonQuery(const SQL: String; const Params: array of Variant): Integer;  
begin
  FExecuteNonQueryCalled := True;
  FLastSQL := SQL;
  SetLength(FLastParams, Length(Params));
  // Copier les paramètres...
  Result := 1;
end;
```

### Créer un mock simple

Exemple complet d'un mock pour les tests :

```pascal
unit MockDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, InterfacesDatabase;

type
  // Mock simple qui enregistre les appels
  TMockDatabase = class(TInterfacedObject, IDatabaseConnection)
  private
    FIsOpen: Boolean;
    FInTransaction: Boolean;
    FExecutedSQL: TStringList;
    FData: TDictionary<String, String>;
  public
    constructor Create;
    destructor Destroy; override;

    // Implémentation IDatabaseConnection
    procedure Open;
    procedure Close;
    function IsConnected: Boolean;
    function ExecuteNonQuery(const SQL: String; const Params: array of Variant): Integer;
    function ExecuteQuery(const SQL: String; const Params: array of Variant): IResultSet;
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;

    // Méthodes pour les tests
    procedure AddData(const Key, Value: String);
    function GetExecutedSQL: TStringList;
    function WasInTransaction: Boolean;
    procedure Reset;
  end;

  TMockResultSet = class(TInterfacedObject, IResultSet)
  private
    FData: TDictionary<String, String>;
    FIsEmpty: Boolean;
  public
    constructor Create(AData: TDictionary<String, String>; AIsEmpty: Boolean);

    function Next: Boolean;
    function GetString(const FieldName: String): String;
    function GetInteger(const FieldName: String): Integer;
    function IsEOF: Boolean;
  end;

implementation

{ TMockDatabase }

constructor TMockDatabase.Create;  
begin
  FExecutedSQL := TStringList.Create;
  FData := TDictionary<String, String>.Create;
  FIsOpen := False;
  FInTransaction := False;
end;

destructor TMockDatabase.Destroy;  
begin
  FData.Free;
  FExecutedSQL.Free;
  inherited;
end;

procedure TMockDatabase.Open;  
begin
  FIsOpen := True;
end;

procedure TMockDatabase.Close;  
begin
  FIsOpen := False;
end;

function TMockDatabase.IsConnected: Boolean;  
begin
  Result := FIsOpen;
end;

function TMockDatabase.ExecuteNonQuery(const SQL: String; const Params: array of Variant): Integer;  
begin
  FExecutedSQL.Add(SQL);
  Result := 1;  // Simuler 1 ligne affectée
end;

function TMockDatabase.ExecuteQuery(const SQL: String; const Params: array of Variant): IResultSet;  
begin
  FExecutedSQL.Add(SQL);
  Result := TMockResultSet.Create(FData, FData.Count = 0);
end;

procedure TMockDatabase.BeginTransaction;  
begin
  FInTransaction := True;
end;

procedure TMockDatabase.Commit;  
begin
  FInTransaction := False;
end;

procedure TMockDatabase.Rollback;  
begin
  FInTransaction := False;
end;

procedure TMockDatabase.AddData(const Key, Value: String);  
begin
  FData.Add(Key, Value);
end;

function TMockDatabase.GetExecutedSQL: TStringList;  
begin
  Result := FExecutedSQL;
end;

function TMockDatabase.WasInTransaction: Boolean;  
begin
  Result := FInTransaction or (FExecutedSQL.Count > 0);
end;

procedure TMockDatabase.Reset;  
begin
  FExecutedSQL.Clear;
  FData.Clear;
  FIsOpen := False;
  FInTransaction := False;
end;

{ TMockResultSet }

constructor TMockResultSet.Create(AData: TDictionary<String, String>; AIsEmpty: Boolean);  
begin
  FData := AData;
  FIsEmpty := AIsEmpty;
end;

function TMockResultSet.Next: Boolean;  
begin
  Result := not FIsEmpty;
end;

function TMockResultSet.GetString(const FieldName: String): String;  
begin
  if FData.ContainsKey(FieldName) then
    Result := FData[FieldName]
  else
    Result := '';
end;

function TMockResultSet.GetInteger(const FieldName: String): Integer;  
begin
  Result := StrToIntDef(GetString(FieldName), 0);
end;

function TMockResultSet.IsEOF: Boolean;  
begin
  Result := FIsEmpty;
end;

end.
```

### Utiliser le mock dans les tests

```pascal
unit TestGestionnaireUtilisateurs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  GestionnaireUtilisateurs, MockDatabase, InterfacesDatabase;

type
  TTestGestionnaireUtilisateurs = class(TTestCase)
  private
    FMockDB: TMockDatabase;
    FGestionnaire: TGestionnaireUtilisateurs;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreerUtilisateur;
    procedure TestCreerUtilisateurUtiliseTransaction;
    procedure TestVerifierMotDePasseCorrect;
    procedure TestVerifierMotDePasseIncorrect;
    procedure TestVerifierUtilisateurInexistant;
  end;

implementation

procedure TTestGestionnaireUtilisateurs.SetUp;  
begin
  inherited;
  FMockDB := TMockDatabase.Create;
  FMockDB.Open;
  FGestionnaire := TGestionnaireUtilisateurs.Create(FMockDB);
end;

procedure TTestGestionnaireUtilisateurs.TearDown;  
begin
  FGestionnaire.Free;
  // Le mock est libéré automatiquement (interface)
  inherited;
end;

procedure TTestGestionnaireUtilisateurs.TestCreerUtilisateur;  
var
  ID: Integer;
begin
  // Test
  ID := FGestionnaire.CreerUtilisateur('test@example.com', 'password123');

  // Vérifications
  AssertTrue('ID valide', ID > 0);

  // Vérifier que le SQL a été exécuté
  AssertTrue('SQL exécuté', FMockDB.GetExecutedSQL.Count > 0);

  // Vérifier que le SQL contient INSERT
  AssertTrue('Contient INSERT',
    Pos('INSERT', UpperCase(FMockDB.GetExecutedSQL[0])) > 0);
end;

procedure TTestGestionnaireUtilisateurs.TestCreerUtilisateurUtiliseTransaction;  
begin
  // Test
  FGestionnaire.CreerUtilisateur('test@example.com', 'password123');

  // Vérifier qu'une transaction a été utilisée
  AssertTrue('Transaction utilisée', FMockDB.WasInTransaction);
end;

procedure TTestGestionnaireUtilisateurs.TestVerifierMotDePasseCorrect;  
var
  Resultat: Boolean;
begin
  // Préparer les données du mock
  FMockDB.AddData('password', 'password123');

  // Test
  Resultat := FGestionnaire.VerifierMotDePasse('test@example.com', 'password123');

  // Vérification
  AssertTrue('Mot de passe correct', Resultat);
end;

procedure TTestGestionnaireUtilisateurs.TestVerifierMotDePasseIncorrect;  
var
  Resultat: Boolean;
begin
  // Préparer les données du mock
  FMockDB.AddData('password', 'password123');

  // Test avec mauvais mot de passe
  Resultat := FGestionnaire.VerifierMotDePasse('test@example.com', 'mauvais_password');

  // Vérification
  AssertFalse('Mot de passe incorrect', Resultat);
end;

procedure TTestGestionnaireUtilisateurs.TestVerifierUtilisateurInexistant;  
var
  Resultat: Boolean;
begin
  // Ne pas ajouter de données au mock (utilisateur inexistant)

  // Test
  Resultat := FGestionnaire.VerifierMotDePasse('inexistant@example.com', 'password');

  // Vérification
  AssertFalse('Utilisateur inexistant', Resultat);
end;

initialization
  RegisterTest(TTestGestionnaireUtilisateurs);

end.
```

## Patterns avancés

### Pattern Repository

Isoler complètement l'accès aux données :

```pascal
unit RepositoryUtilisateurs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Utilisateur;

type
  // Interface du repository
  IUtilisateurRepository = interface
    ['{AAAAAAAA-AAAA-AAAA-AAAA-AAAAAAAAAAAA}']
    function Creer(Utilisateur: TUtilisateur): Integer;
    function ChargerParID(ID: Integer): TUtilisateur;
    function ChargerParEmail(const Email: String): TUtilisateur;
    function Modifier(Utilisateur: TUtilisateur): Boolean;
    function Supprimer(ID: Integer): Boolean;
    function ChargerTous: TList<TUtilisateur>;
  end;

  // Implémentation réelle avec base de données
  TUtilisateurRepositoryDB = class(TInterfacedObject, IUtilisateurRepository)
  private
    FDB: IDatabaseConnection;
  public
    constructor Create(ADatabase: IDatabaseConnection);

    function Creer(Utilisateur: TUtilisateur): Integer;
    function ChargerParID(ID: Integer): TUtilisateur;
    function ChargerParEmail(const Email: String): TUtilisateur;
    function Modifier(Utilisateur: TUtilisateur): Boolean;
    function Supprimer(ID: Integer): Boolean;
    function ChargerTous: TList<TUtilisateur>;
  end;

  // Mock pour les tests
  TUtilisateurRepositoryMock = class(TInterfacedObject, IUtilisateurRepository)
  private
    FUtilisateurs: TDictionary<Integer, TUtilisateur>;
    FNextID: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Creer(Utilisateur: TUtilisateur): Integer;
    function ChargerParID(ID: Integer): TUtilisateur;
    function ChargerParEmail(const Email: String): TUtilisateur;
    function Modifier(Utilisateur: TUtilisateur): Boolean;
    function Supprimer(ID: Integer): Boolean;
    function ChargerTous: TList<TUtilisateur>;

    // Méthodes utilitaires pour les tests
    procedure AjouterUtilisateur(Utilisateur: TUtilisateur);
    function Count: Integer;
  end;

implementation

{ TUtilisateurRepositoryDB }

constructor TUtilisateurRepositoryDB.Create(ADatabase: IDatabaseConnection);  
begin
  FDB := ADatabase;
end;

function TUtilisateurRepositoryDB.Creer(Utilisateur: TUtilisateur): Integer;  
begin
  FDB.BeginTransaction;
  try
    Result := FDB.ExecuteNonQuery(
      'INSERT INTO utilisateurs (email, nom, prenom) VALUES (?, ?, ?)',
      [Utilisateur.Email, Utilisateur.Nom, Utilisateur.Prenom]
    );
    Utilisateur.ID := Result;
    FDB.Commit;
  except
    FDB.Rollback;
    raise;
  end;
end;

function TUtilisateurRepositoryDB.ChargerParID(ID: Integer): TUtilisateur;  
var
  RS: IResultSet;
begin
  RS := FDB.ExecuteQuery('SELECT * FROM utilisateurs WHERE id = ?', [ID]);

  if RS.IsEOF then
    Result := nil
  else
  begin
    Result := TUtilisateur.Create;
    Result.ID := RS.GetInteger('id');
    Result.Email := RS.GetString('email');
    Result.Nom := RS.GetString('nom');
    Result.Prenom := RS.GetString('prenom');
  end;
end;

function TUtilisateurRepositoryDB.ChargerParEmail(const Email: String): TUtilisateur;  
var
  RS: IResultSet;
begin
  RS := FDB.ExecuteQuery('SELECT * FROM utilisateurs WHERE email = ?', [Email]);

  if RS.IsEOF then
    Result := nil
  else
  begin
    Result := TUtilisateur.Create;
    Result.ID := RS.GetInteger('id');
    Result.Email := RS.GetString('email');
    Result.Nom := RS.GetString('nom');
    Result.Prenom := RS.GetString('prenom');
  end;
end;

function TUtilisateurRepositoryDB.Modifier(Utilisateur: TUtilisateur): Boolean;  
begin
  FDB.BeginTransaction;
  try
    FDB.ExecuteNonQuery(
      'UPDATE utilisateurs SET email = ?, nom = ?, prenom = ? WHERE id = ?',
      [Utilisateur.Email, Utilisateur.Nom, Utilisateur.Prenom, Utilisateur.ID]
    );
    FDB.Commit;
    Result := True;
  except
    FDB.Rollback;
    Result := False;
  end;
end;

function TUtilisateurRepositoryDB.Supprimer(ID: Integer): Boolean;  
begin
  FDB.BeginTransaction;
  try
    FDB.ExecuteNonQuery('DELETE FROM utilisateurs WHERE id = ?', [ID]);
    FDB.Commit;
    Result := True;
  except
    FDB.Rollback;
    Result := False;
  end;
end;

function TUtilisateurRepositoryDB.ChargerTous: TList<TUtilisateur>;  
var
  RS: IResultSet;
  Utilisateur: TUtilisateur;
begin
  Result := TList<TUtilisateur>.Create;

  RS := FDB.ExecuteQuery('SELECT * FROM utilisateurs', []);

  while not RS.IsEOF do
  begin
    Utilisateur := TUtilisateur.Create;
    Utilisateur.ID := RS.GetInteger('id');
    Utilisateur.Email := RS.GetString('email');
    Utilisateur.Nom := RS.GetString('nom');
    Utilisateur.Prenom := RS.GetString('prenom');
    Result.Add(Utilisateur);

    RS.Next;
  end;
end;

{ TUtilisateurRepositoryMock }

constructor TUtilisateurRepositoryMock.Create;  
begin
  FUtilisateurs := TDictionary<Integer, TUtilisateur>.Create;
  FNextID := 1;
end;

destructor TUtilisateurRepositoryMock.Destroy;  
var
  Utilisateur: TUtilisateur;
begin
  // Libérer tous les utilisateurs stockés
  for Utilisateur in FUtilisateurs.Values do
    Utilisateur.Free;

  FUtilisateurs.Free;
  inherited;
end;

function TUtilisateurRepositoryMock.Creer(Utilisateur: TUtilisateur): Integer;  
var
  Copie: TUtilisateur;
begin
  // Créer une copie pour éviter les problèmes de mémoire
  Copie := TUtilisateur.Create;
  Copie.Email := Utilisateur.Email;
  Copie.Nom := Utilisateur.Nom;
  Copie.Prenom := Utilisateur.Prenom;
  Copie.ID := FNextID;

  FUtilisateurs.Add(FNextID, Copie);

  Result := FNextID;
  Utilisateur.ID := FNextID;
  Inc(FNextID);
end;

function TUtilisateurRepositoryMock.ChargerParID(ID: Integer): TUtilisateur;  
var
  Original: TUtilisateur;
begin
  if FUtilisateurs.ContainsKey(ID) then
  begin
    Original := FUtilisateurs[ID];

    // Retourner une copie, pas l'original
    Result := TUtilisateur.Create;
    Result.ID := Original.ID;
    Result.Email := Original.Email;
    Result.Nom := Original.Nom;
    Result.Prenom := Original.Prenom;
  end
  else
    Result := nil;
end;

function TUtilisateurRepositoryMock.ChargerParEmail(const Email: String): TUtilisateur;  
var
  Utilisateur: TUtilisateur;
begin
  Result := nil;

  for Utilisateur in FUtilisateurs.Values do
  begin
    if Utilisateur.Email = Email then
    begin
      // Retourner une copie
      Result := TUtilisateur.Create;
      Result.ID := Utilisateur.ID;
      Result.Email := Utilisateur.Email;
      Result.Nom := Utilisateur.Nom;
      Result.Prenom := Utilisateur.Prenom;
      Break;
    end;
  end;
end;

function TUtilisateurRepositoryMock.Modifier(Utilisateur: TUtilisateur): Boolean;  
var
  Existant: TUtilisateur;
begin
  if FUtilisateurs.ContainsKey(Utilisateur.ID) then
  begin
    Existant := FUtilisateurs[Utilisateur.ID];
    Existant.Email := Utilisateur.Email;
    Existant.Nom := Utilisateur.Nom;
    Existant.Prenom := Utilisateur.Prenom;
    Result := True;
  end
  else
    Result := False;
end;

function TUtilisateurRepositoryMock.Supprimer(ID: Integer): Boolean;  
var
  Utilisateur: TUtilisateur;
begin
  if FUtilisateurs.ContainsKey(ID) then
  begin
    Utilisateur := FUtilisateurs[ID];
    FUtilisateurs.Remove(ID);
    Utilisateur.Free;
    Result := True;
  end
  else
    Result := False;
end;

function TUtilisateurRepositoryMock.ChargerTous: TList<TUtilisateur>;  
var
  Original, Copie: TUtilisateur;
begin
  Result := TList<TUtilisateur>.Create;

  for Original in FUtilisateurs.Values do
  begin
    Copie := TUtilisateur.Create;
    Copie.ID := Original.ID;
    Copie.Email := Original.Email;
    Copie.Nom := Original.Nom;
    Copie.Prenom := Original.Prenom;
    Result.Add(Copie);
  end;
end;

procedure TUtilisateurRepositoryMock.AjouterUtilisateur(Utilisateur: TUtilisateur);  
var
  Copie: TUtilisateur;
begin
  Copie := TUtilisateur.Create;
  Copie.ID := Utilisateur.ID;
  Copie.Email := Utilisateur.Email;
  Copie.Nom := Utilisateur.Nom;
  Copie.Prenom := Utilisateur.Prenom;

  FUtilisateurs.Add(Copie.ID, Copie);

  if Copie.ID >= FNextID then
    FNextID := Copie.ID + 1;
end;

function TUtilisateurRepositoryMock.Count: Integer;  
begin
  Result := FUtilisateurs.Count;
end;

end.
```

### Utilisation du Repository dans les tests

```pascal
unit TestServiceUtilisateurs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Utilisateur, RepositoryUtilisateurs, ServiceUtilisateurs;

type
  TTestServiceUtilisateurs = class(TTestCase)
  private
    FRepository: TUtilisateurRepositoryMock;
    FService: TServiceUtilisateurs;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInscriptionUtilisateur;
    procedure TestInscriptionEmailDuplique;
    procedure TestAuthentification;
    procedure TestAuthentificationEchec;
    procedure TestModificationProfil;
  end;

implementation

procedure TTestServiceUtilisateurs.SetUp;  
begin
  inherited;
  FRepository := TUtilisateurRepositoryMock.Create;
  FService := TServiceUtilisateurs.Create(FRepository);
end;

procedure TTestServiceUtilisateurs.TearDown;  
begin
  FService.Free;
  // Le repository mock est libéré automatiquement (interface)
  inherited;
end;

procedure TTestServiceUtilisateurs.TestInscriptionUtilisateur;  
var
  Utilisateur: TUtilisateur;
  ID: Integer;
begin
  // Créer un nouvel utilisateur
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'nouveau@test.com';
    Utilisateur.Nom := 'Dupont';
    Utilisateur.Prenom := 'Jean';

    // Inscrire via le service
    ID := FService.InscrireUtilisateur(Utilisateur, 'password123');

    // Vérifications
    AssertTrue('ID valide', ID > 0);
    AssertEquals('Un utilisateur créé', 1, FRepository.Count);

    // Vérifier que l'utilisateur existe
    Utilisateur := FRepository.ChargerParEmail('nouveau@test.com');
    try
      AssertNotNull('Utilisateur existe', Utilisateur);
      AssertEquals('Nom correct', 'Dupont', Utilisateur.Nom);
    finally
      Utilisateur.Free;
    end;
  finally
    Utilisateur.Free;
  end;
end;

procedure TTestServiceUtilisateurs.TestInscriptionEmailDuplique;  
var
  Utilisateur1, Utilisateur2: TUtilisateur;
begin
  // Créer premier utilisateur
  Utilisateur1 := TUtilisateur.Create;
  try
    Utilisateur1.Email := 'double@test.com';
    Utilisateur1.Nom := 'Premier';
    FService.InscrireUtilisateur(Utilisateur1, 'password1');
  finally
    Utilisateur1.Free;
  end;

  // Tenter de créer un doublon
  Utilisateur2 := TUtilisateur.Create;
  try
    Utilisateur2.Email := 'double@test.com';  // Même email !
    Utilisateur2.Nom := 'Second';

    try
      FService.InscrireUtilisateur(Utilisateur2, 'password2');
      Fail('Devrait lever EEmailDuplique');
    except
      on E: EEmailDuplique do
        ; // Comportement attendu
    end;
  finally
    Utilisateur2.Free;
  end;

  // Vérifier qu'il n'y a qu'un seul utilisateur
  AssertEquals('Un seul utilisateur', 1, FRepository.Count);
end;

procedure TTestServiceUtilisateurs.TestAuthentification;  
var
  Utilisateur: TUtilisateur;
  UtilisateurConnecte: TUtilisateur;
begin
  // Créer et inscrire un utilisateur
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'auth@test.com';
    Utilisateur.Nom := 'Test';
    FService.InscrireUtilisateur(Utilisateur, 'MotDePasseSecret123');
  finally
    Utilisateur.Free;
  end;

  // Tester l'authentification
  UtilisateurConnecte := FService.Authentifier('auth@test.com', 'MotDePasseSecret123');
  try
    AssertNotNull('Utilisateur authentifié', UtilisateurConnecte);
    AssertEquals('Bon email', 'auth@test.com', UtilisateurConnecte.Email);
  finally
    UtilisateurConnecte.Free;
  end;
end;

procedure TTestServiceUtilisateurs.TestAuthentificationEchec;  
var
  Utilisateur: TUtilisateur;
  UtilisateurConnecte: TUtilisateur;
begin
  // Créer un utilisateur
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'echec@test.com';
    FService.InscrireUtilisateur(Utilisateur, 'BonMotDePasse');
  finally
    Utilisateur.Free;
  end;

  // Tenter avec mauvais mot de passe
  UtilisateurConnecte := FService.Authentifier('echec@test.com', 'MauvaisMotDePasse');
  AssertNull('Authentification échouée', UtilisateurConnecte);

  // Tenter avec email inexistant
  UtilisateurConnecte := FService.Authentifier('inexistant@test.com', 'password');
  AssertNull('Utilisateur inexistant', UtilisateurConnecte);
end;

procedure TTestServiceUtilisateurs.TestModificationProfil;  
var
  Utilisateur: TUtilisateur;
  ID: Integer;
begin
  // Créer un utilisateur
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'modif@test.com';
    Utilisateur.Nom := 'Avant';
    ID := FService.InscrireUtilisateur(Utilisateur, 'password');
  finally
    Utilisateur.Free;
  end;

  // Modifier le profil
  Utilisateur := FRepository.ChargerParID(ID);
  try
    Utilisateur.Nom := 'Apres';
    Utilisateur.Prenom := 'Nouveau';

    AssertTrue('Modification réussie', FService.ModifierProfil(Utilisateur));
  finally
    Utilisateur.Free;
  end;

  // Vérifier les modifications
  Utilisateur := FRepository.ChargerParID(ID);
  try
    AssertEquals('Nom modifié', 'Apres', Utilisateur.Nom);
    AssertEquals('Prénom ajouté', 'Nouveau', Utilisateur.Prenom);
  finally
    Utilisateur.Free;
  end;
end;

initialization
  RegisterTest(TTestServiceUtilisateurs);

end.
```

## Spy : espionner les appels

Un **spy** enregistre les appels de méthodes pour vérification ultérieure :

```pascal
unit SpyLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TLogEntry = record
    Level: TLogLevel;
    Message: String;
    Timestamp: TDateTime;
  end;

  ILogger = interface
    ['{BBBBBBBB-BBBB-BBBB-BBBB-BBBBBBBBBBBB}']
    procedure Debug(const Message: String);
    procedure Info(const Message: String);
    procedure Warning(const Message: String);
    procedure Error(const Message: String);
  end;

  // Spy qui enregistre tous les appels
  TSpyLogger = class(TInterfacedObject, ILogger)
  private
    FLogEntries: TList<TLogEntry>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Debug(const Message: String);
    procedure Info(const Message: String);
    procedure Warning(const Message: String);
    procedure Error(const Message: String);

    // Méthodes d'inspection
    function GetLogCount: Integer;
    function GetLogEntry(Index: Integer): TLogEntry;
    function WasDebugCalled: Boolean;
    function WasErrorCalled: Boolean;
    function GetMessagesContaining(const Text: String): TList<TLogEntry>;
    procedure Clear;
  end;

implementation

constructor TSpyLogger.Create;  
begin
  FLogEntries := TList<TLogEntry>.Create;
end;

destructor TSpyLogger.Destroy;  
begin
  FLogEntries.Free;
  inherited;
end;

procedure TSpyLogger.Debug(const Message: String);  
var
  Entry: TLogEntry;
begin
  Entry.Level := llDebug;
  Entry.Message := Message;
  Entry.Timestamp := Now;
  FLogEntries.Add(Entry);
end;

procedure TSpyLogger.Info(const Message: String);  
var
  Entry: TLogEntry;
begin
  Entry.Level := llInfo;
  Entry.Message := Message;
  Entry.Timestamp := Now;
  FLogEntries.Add(Entry);
end;

procedure TSpyLogger.Warning(const Message: String);  
var
  Entry: TLogEntry;
begin
  Entry.Level := llWarning;
  Entry.Message := Message;
  Entry.Timestamp := Now;
  FLogEntries.Add(Entry);
end;

procedure TSpyLogger.Error(const Message: String);  
var
  Entry: TLogEntry;
begin
  Entry.Level := llError;
  Entry.Message := Message;
  Entry.Timestamp := Now;
  FLogEntries.Add(Entry);
end;

function TSpyLogger.GetLogCount: Integer;  
begin
  Result := FLogEntries.Count;
end;

function TSpyLogger.GetLogEntry(Index: Integer): TLogEntry;  
begin
  Result := FLogEntries[Index];
end;

function TSpyLogger.WasDebugCalled: Boolean;  
var
  Entry: TLogEntry;
begin
  Result := False;
  for Entry in FLogEntries do
    if Entry.Level = llDebug then
      Exit(True);
end;

function TSpyLogger.WasErrorCalled: Boolean;  
var
  Entry: TLogEntry;
begin
  Result := False;
  for Entry in FLogEntries do
    if Entry.Level = llError then
      Exit(True);
end;

function TSpyLogger.GetMessagesContaining(const Text: String): TList<TLogEntry>;  
var
  Entry: TLogEntry;
begin
  Result := TList<TLogEntry>.Create;
  for Entry in FLogEntries do
    if Pos(LowerCase(Text), LowerCase(Entry.Message)) > 0 then
      Result.Add(Entry);
end;

procedure TSpyLogger.Clear;  
begin
  FLogEntries.Clear;
end;

end.
```

### Utilisation du Spy

```pascal
procedure TTestServiceAvecLogging.TestCreationUtilisateurLogsInfo;  
var
  SpyLogger: TSpyLogger;
  Service: TServiceUtilisateurs;
  Utilisateur: TUtilisateur;
begin
  // Créer le spy
  SpyLogger := TSpyLogger.Create;

  // Injecter le spy dans le service
  Service := TServiceUtilisateurs.Create(FRepository, SpyLogger);
  try
    Utilisateur := TUtilisateur.Create;
    try
      Utilisateur.Email := 'test@example.com';
      Service.InscrireUtilisateur(Utilisateur, 'password');
    finally
      Utilisateur.Free;
    end;

    // Vérifier que le logger a été appelé
    AssertTrue('Au moins un log', SpyLogger.GetLogCount > 0);
    AssertTrue('Info appelé', Pos('Info', SpyLogger.GetLogEntry(0).Message) > 0);

    // Vérifier le contenu des logs
    AssertTrue('Mention de l''email',
      Pos('test@example.com', SpyLogger.GetLogEntry(0).Message) > 0);

  finally
    Service.Free;
  end;
end;

procedure TTestServiceAvecLogging.TestErreurLogsError;  
var
  SpyLogger: TSpyLogger;
  Service: TServiceUtilisateurs;
begin
  SpyLogger := TSpyLogger.Create;
  Service := TServiceUtilisateurs.Create(FRepository, SpyLogger);
  try
    // Provoquer une erreur
    try
      Service.InscrireUtilisateur(nil, 'password');
    except
      // Ignorer l'exception
    end;

    // Vérifier qu'une erreur a été loggée
    AssertTrue('Erreur loggée', SpyLogger.WasErrorCalled);

  finally
    Service.Free;
  end;
end;
```

## Conteneurs d'injection de dépendances

Pour les projets complexes, un **conteneur DI** automatise la création et l'injection :

```pascal
unit DIContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, TypInfo;

type
  TFactoryMethod = function: TObject;

  TDIContainer = class
  private
    FBindings: TDictionary<String, TFactoryMethod>;
    FSingletons: TDictionary<String, TObject>;
  public
    constructor Create;
    destructor Destroy; override;

    // Enregistrer une liaison
    procedure Bind(const InterfaceName: String; Factory: TFactoryMethod; Singleton: Boolean = False);

    // Résoudre une dépendance
    function Resolve(const InterfaceName: String): TObject;

    // Libérer tous les singletons
    procedure Clear;
  end;

implementation

constructor TDIContainer.Create;  
begin
  FBindings := TDictionary<String, TFactoryMethod>.Create;
  FSingletons := TDictionary<String, TObject>.Create;
end;

destructor TDIContainer.Destroy;  
var
  Obj: TObject;
begin
  // Libérer tous les singletons
  for Obj in FSingletons.Values do
    Obj.Free;

  FSingletons.Free;
  FBindings.Free;
  inherited;
end;

procedure TDIContainer.Bind(const InterfaceName: String; Factory: TFactoryMethod;
  Singleton: Boolean = False);
begin
  FBindings.AddOrSetValue(InterfaceName, Factory);

  if Singleton then
  begin
    // Créer immédiatement le singleton
    if not FSingletons.ContainsKey(InterfaceName) then
      FSingletons.Add(InterfaceName, Factory());
  end;
end;

function TDIContainer.Resolve(const InterfaceName: String): TObject;  
begin
  // Vérifier si c'est un singleton déjà créé
  if FSingletons.ContainsKey(InterfaceName) then
    Exit(FSingletons[InterfaceName]);

  // Sinon, créer une nouvelle instance
  if FBindings.ContainsKey(InterfaceName) then
    Result := FBindings[InterfaceName]()
  else
    raise Exception.CreateFmt('Aucune liaison trouvée pour %s', [InterfaceName]);
end;

procedure TDIContainer.Clear;  
var
  Obj: TObject;
begin
  for Obj in FSingletons.Values do
    Obj.Free;
  FSingletons.Clear;
end;

end.
```

### Utilisation du conteneur DI

```pascal
program MonApplication;

uses
  DIContainer, InterfacesDatabase, RepositoryUtilisateurs, ServiceUtilisateurs;

var
  Container: TDIContainer;
  Service: TServiceUtilisateurs;

begin
  Container := TDIContainer.Create;
  try
    // Configuration du conteneur
    Container.Bind('IDatabaseConnection',
      @function: TObject
      begin
        Result := TDatabaseConnectionMySQL.Create('localhost', 'mydb', 'user', 'pass');
      end,
      True  // Singleton
    );

    Container.Bind('IUtilisateurRepository',
      @function: TObject
      var
        DB: IDatabaseConnection;
      begin
        DB := Container.Resolve('IDatabaseConnection') as IDatabaseConnection;
        Result := TUtilisateurRepositoryDB.Create(DB);
      end
    );

    Container.Bind('TServiceUtilisateurs',
      @function: TObject
      var
        Repo: IUtilisateurRepository;
      begin
        Repo := Container.Resolve('IUtilisateurRepository') as IUtilisateurRepository;
        Result := TServiceUtilisateurs.Create(Repo);
      end
    );

    // Utilisation
    Service := Container.Resolve('TServiceUtilisateurs') as TServiceUtilisateurs;
    try
      // Utiliser le service...
    finally
      Service.Free;
    end;

  finally
    Container.Free;
  end;
end.
```

### Configuration pour les tests

```pascal
procedure ConfigurerContainerPourTests(Container: TDIContainer);  
begin
  // Utiliser des mocks pour les tests
  Container.Bind('IDatabaseConnection',
    @function: TObject
    begin
      Result := TMockDatabase.Create;
    end
  );

  Container.Bind('IUtilisateurRepository',
    @function: TObject
    begin
      Result := TUtilisateurRepositoryMock.Create;
    end
  );

  Container.Bind('ILogger',
    @function: TObject
    begin
      Result := TSpyLogger.Create;
    end
  );
end;
```

## Stratégies multi-plateformes

### Abstraire les différences OS

```pascal
unit InterfacesFichiers;

{$mode objfpc}{$H+}

interface

type
  IFileSystem = interface
    ['{CCCCCCCC-CCCC-CCCC-CCCC-CCCCCCCCCCCC}']
    function ReadFile(const FileName: String): String;
    procedure WriteFile(const FileName, Content: String);
    function FileExists(const FileName: String): Boolean;
    function GetTempPath: String;
    function GetPathSeparator: Char;
  end;

implementation

end.
```

### Implémentations spécifiques

```pascal
unit FileSystemWindows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InterfacesFichiers;

type
  TFileSystemWindows = class(TInterfacedObject, IFileSystem)
  public
    function ReadFile(const FileName: String): String;
    procedure WriteFile(const FileName, Content: String);
    function FileExists(const FileName: String): Boolean;
    function GetTempPath: String;
    function GetPathSeparator: Char;
  end;

implementation

function TFileSystemWindows.ReadFile(const FileName: String): String;  
var
  FileStream: TFileStream;
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FileName);
    Result := StringList.Text;
  finally
    StringList.Free;
  end;
end;

procedure TFileSystemWindows.WriteFile(const FileName, Content: String);  
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.Text := Content;
    StringList.SaveToFile(FileName);
  finally
    StringList.Free;
  end;
end;

function TFileSystemWindows.FileExists(const FileName: String): Boolean;  
begin
  Result := SysUtils.FileExists(FileName);
end;

function TFileSystemWindows.GetTempPath: String;  
begin
  Result := GetEnvironmentVariable('TEMP');
  if Result = '' then
    Result := 'C:\Temp';
end;

function TFileSystemWindows.GetPathSeparator: Char;  
begin
  Result := '\';
end;

end.
```

```pascal
unit FileSystemLinux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InterfacesFichiers;

type
  TFileSystemLinux = class(TInterfacedObject, IFileSystem)
  public
    function ReadFile(const FileName: String): String;
    procedure WriteFile(const FileName, Content: String);
    function FileExists(const FileName: String): Boolean;
    function GetTempPath: String;
    function GetPathSeparator: Char;
  end;

implementation

function TFileSystemLinux.ReadFile(const FileName: String): String;  
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FileName);
    Result := StringList.Text;
  finally
    StringList.Free;
  end;
end;

procedure TFileSystemLinux.WriteFile(const FileName, Content: String);  
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.Text := Content;
    StringList.SaveToFile(FileName);
  finally
    StringList.Free;
  end;
end;

function TFileSystemLinux.FileExists(const FileName: String): Boolean;  
begin
  Result := SysUtils.FileExists(FileName);
end;

function TFileSystemLinux.GetTempPath: String;  
begin
  Result := '/tmp';
end;

function TFileSystemLinux.GetPathSeparator: Char;  
begin
  Result := '/';
end;

end.
```

### Factory pour créer la bonne implémentation

```pascal
unit FileSystemFactory;

{$mode objfpc}{$H+}

interface

uses
  InterfacesFichiers
  {$IFDEF WINDOWS}
  , FileSystemWindows
  {$ENDIF}
  {$IFDEF UNIX}
  , FileSystemLinux
  {$ENDIF};

type
  TFileSystemFactory = class
  public
    class function Create: IFileSystem;
    class function CreateForTesting: IFileSystem;
  end;

implementation

uses
  MockFileSystem;  // Pour les tests

class function TFileSystemFactory.Create: IFileSystem;  
begin
  {$IFDEF WINDOWS}
  Result := TFileSystemWindows.Create;
  {$ENDIF}

  {$IFDEF UNIX}
  Result := TFileSystemLinux.Create;
  {$ENDIF}
end;

class function TFileSystemFactory.CreateForTesting: IFileSystem;  
begin
  // Retourner toujours un mock pour les tests
  Result := TMockFileSystem.Create;
end;

end.
```

### Mock pour le système de fichiers

```pascal
unit MockFileSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, InterfacesFichiers;

type
  TMockFileSystem = class(TInterfacedObject, IFileSystem)
  private
    FFiles: TDictionary<String, String>;  // Nom fichier → Contenu
    FReadCalls: TStringList;
    FWriteCalls: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    // Implémentation IFileSystem
    function ReadFile(const FileName: String): String;
    procedure WriteFile(const FileName, Content: String);
    function FileExists(const FileName: String): Boolean;
    function GetTempPath: String;
    function GetPathSeparator: Char;

    // Méthodes pour configurer le mock
    procedure AddFile(const FileName, Content: String);
    procedure RemoveFile(const FileName: String);
    procedure Clear;

    // Méthodes d'inspection pour les tests
    function WasReadCalled(const FileName: String): Boolean;
    function WasWriteCalled(const FileName: String): Boolean;
    function GetWrittenContent(const FileName: String): String;
    function GetReadCallCount: Integer;
    function GetWriteCallCount: Integer;
  end;

implementation

constructor TMockFileSystem.Create;  
begin
  FFiles := TDictionary<String, String>.Create;
  FReadCalls := TStringList.Create;
  FWriteCalls := TStringList.Create;
end;

destructor TMockFileSystem.Destroy;  
begin
  FWriteCalls.Free;
  FReadCalls.Free;
  FFiles.Free;
  inherited;
end;

function TMockFileSystem.ReadFile(const FileName: String): String;  
begin
  FReadCalls.Add(FileName);

  if FFiles.ContainsKey(FileName) then
    Result := FFiles[FileName]
  else
    raise Exception.CreateFmt('Fichier non trouvé : %s', [FileName]);
end;

procedure TMockFileSystem.WriteFile(const FileName, Content: String);  
begin
  FWriteCalls.Add(FileName);
  FFiles.AddOrSetValue(FileName, Content);
end;

function TMockFileSystem.FileExists(const FileName: String): Boolean;  
begin
  Result := FFiles.ContainsKey(FileName);
end;

function TMockFileSystem.GetTempPath: String;  
begin
  Result := '/mock/temp/';
end;

function TMockFileSystem.GetPathSeparator: Char;  
begin
  Result := '/';
end;

procedure TMockFileSystem.AddFile(const FileName, Content: String);  
begin
  FFiles.AddOrSetValue(FileName, Content);
end;

procedure TMockFileSystem.RemoveFile(const FileName: String);  
begin
  FFiles.Remove(FileName);
end;

procedure TMockFileSystem.Clear;  
begin
  FFiles.Clear;
  FReadCalls.Clear;
  FWriteCalls.Clear;
end;

function TMockFileSystem.WasReadCalled(const FileName: String): Boolean;  
begin
  Result := FReadCalls.IndexOf(FileName) >= 0;
end;

function TMockFileSystem.WasWriteCalled(const FileName: String): Boolean;  
begin
  Result := FWriteCalls.IndexOf(FileName) >= 0;
end;

function TMockFileSystem.GetWrittenContent(const FileName: String): String;  
begin
  if FFiles.ContainsKey(FileName) then
    Result := FFiles[FileName]
  else
    Result := '';
end;

function TMockFileSystem.GetReadCallCount: Integer;  
begin
  Result := FReadCalls.Count;
end;

function TMockFileSystem.GetWriteCallCount: Integer;  
begin
  Result := FWriteCalls.Count;
end;

end.
```

### Tests avec le mock de système de fichiers

```pascal
unit TestGestionnaireFichiers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  InterfacesFichiers, MockFileSystem, GestionnaireFichiers;

type
  TTestGestionnaireFichiers = class(TTestCase)
  private
    FMockFS: TMockFileSystem;
    FGestionnaire: TGestionnaireFichiers;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestChargerConfiguration;
    procedure TestSauvegarderConfiguration;
    procedure TestChargerFichierInexistant;
    procedure TestMultiplesLectures;
    procedure TestCheminMultiPlateforme;
  end;

implementation

procedure TTestGestionnaireFichiers.SetUp;  
begin
  inherited;
  FMockFS := TMockFileSystem.Create;
  FGestionnaire := TGestionnaireFichiers.Create(FMockFS);
end;

procedure TTestGestionnaireFichiers.TearDown;  
begin
  FGestionnaire.Free;
  // FMockFS libéré automatiquement (interface)
  inherited;
end;

procedure TTestGestionnaireFichiers.TestChargerConfiguration;  
var
  Config: String;
begin
  // Préparer le mock avec un fichier de config
  FMockFS.AddFile('config.ini',
    '[Database]' + LineEnding +
    'Host=localhost' + LineEnding +
    'Port=5432');

  // Charger la configuration
  Config := FGestionnaire.ChargerConfiguration('config.ini');

  // Vérifications
  AssertTrue('Config non vide', Length(Config) > 0);
  AssertTrue('Contient Database', Pos('Database', Config) > 0);

  // Vérifier que le fichier a été lu
  AssertTrue('Lecture effectuée', FMockFS.WasReadCalled('config.ini'));
end;

procedure TTestGestionnaireFichiers.TestSauvegarderConfiguration;  
var
  Config: String;
begin
  Config := '[Settings]' + LineEnding + 'Theme=Dark';

  // Sauvegarder
  FGestionnaire.SauvegarderConfiguration('settings.ini', Config);

  // Vérifications
  AssertTrue('Écriture effectuée', FMockFS.WasWriteCalled('settings.ini'));
  AssertEquals('Contenu correct', Config, FMockFS.GetWrittenContent('settings.ini'));
end;

procedure TTestGestionnaireFichiers.TestChargerFichierInexistant;  
begin
  // Ne pas créer le fichier dans le mock

  try
    FGestionnaire.ChargerConfiguration('inexistant.ini');
    Fail('Devrait lever une exception');
  except
    on E: Exception do
      AssertTrue('Message d''erreur', Pos('non trouvé', E.Message) > 0);
  end;
end;

procedure TTestGestionnaireFichiers.TestMultiplesLectures;  
begin
  FMockFS.AddFile('file1.txt', 'Contenu 1');
  FMockFS.AddFile('file2.txt', 'Contenu 2');
  FMockFS.AddFile('file3.txt', 'Contenu 3');

  // Lire plusieurs fichiers
  FGestionnaire.ChargerConfiguration('file1.txt');
  FGestionnaire.ChargerConfiguration('file2.txt');
  FGestionnaire.ChargerConfiguration('file3.txt');

  // Vérifier le nombre d'appels
  AssertEquals('3 lectures', 3, FMockFS.GetReadCallCount);
end;

procedure TTestGestionnaireFichiers.TestCheminMultiPlateforme;  
var
  Chemin: String;
begin
  // Construire un chemin avec le séparateur de la plateforme
  Chemin := FGestionnaire.ConstruireChemin(['dossier', 'sous-dossier', 'fichier.txt']);

  // Le mock retourne toujours '/'
  AssertTrue('Contient séparateur', Pos('/', Chemin) > 0);
  AssertTrue('Se termine par fichier.txt', Pos('fichier.txt', Chemin) > 0);
end;

initialization
  RegisterTest(TTestGestionnaireFichiers);

end.
```

## Frameworks de mocking avancés

### Concept : génération automatique de mocks

Pour les projets complexes, vous pouvez créer un système qui génère automatiquement des mocks à partir des interfaces :

```pascal
unit MockGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo;

type
  TMockGenerator = class
  public
    class function GenerateMock(InterfaceInfo: PTypeInfo): String;
  end;

implementation

class function TMockGenerator.GenerateMock(InterfaceInfo: PTypeInfo): String;  
var
  IntfData: PTypeData;
  i: Integer;
  MethodName: String;
begin
  Result := '';

  if InterfaceInfo^.Kind <> tkInterface then
    Exit;

  IntfData := GetTypeData(InterfaceInfo);

  // Générer le code du mock
  Result := Format('type%s  TMock%s = class(TInterfacedObject, %s)%s',
    [LineEnding, InterfaceInfo^.Name, InterfaceInfo^.Name, LineEnding]);
  Result := Result + '  private' + LineEnding;
  Result := Result + '    FCallCount: Integer;' + LineEnding;
  Result := Result + '  public' + LineEnding;

  // Parcourir les méthodes de l'interface
  // (Simplifié - implémentation complète plus complexe)

  Result := Result + '    property CallCount: Integer read FCallCount;' + LineEnding;
  Result := Result + '  end;' + LineEnding;
end;

end.
```

## Bibliothèques tierces pour le mocking

### MockManager (exemple conceptuel)

Bien que FreePascal n'ait pas l'équivalent de Mockito (Java) ou Moq (C#), vous pouvez créer un système simplifié :

```pascal
unit SimpleMockManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TMethodCall = record
    MethodName: String;
    Params: array of Variant;
    ReturnValue: Variant;
  end;

  TSimpleMock = class
  private
    FCalls: TList<TMethodCall>;
    FExpectedCalls: TDictionary<String, Variant>;
  public
    constructor Create;
    destructor Destroy; override;

    // Enregistrer un appel
    procedure RecordCall(const MethodName: String; const Params: array of Variant);

    // Définir une valeur de retour attendue
    procedure ExpectCall(const MethodName: String; ReturnValue: Variant);

    // Obtenir la valeur de retour pour un appel
    function GetReturnValue(const MethodName: String): Variant;

    // Vérifications
    function WasCalled(const MethodName: String): Boolean;
    function GetCallCount(const MethodName: String): Integer;
    function GetTotalCallCount: Integer;

    procedure Reset;
  end;

implementation

constructor TSimpleMock.Create;  
begin
  FCalls := TList<TMethodCall>.Create;
  FExpectedCalls := TDictionary<String, Variant>.Create;
end;

destructor TSimpleMock.Destroy;  
begin
  FExpectedCalls.Free;
  FCalls.Free;
  inherited;
end;

procedure TSimpleMock.RecordCall(const MethodName: String; const Params: array of Variant);  
var
  Call: TMethodCall;
  i: Integer;
begin
  Call.MethodName := MethodName;
  SetLength(Call.Params, Length(Params));
  for i := 0 to High(Params) do
    Call.Params[i] := Params[i];

  FCalls.Add(Call);
end;

procedure TSimpleMock.ExpectCall(const MethodName: String; ReturnValue: Variant);  
begin
  FExpectedCalls.AddOrSetValue(MethodName, ReturnValue);
end;

function TSimpleMock.GetReturnValue(const MethodName: String): Variant;  
begin
  if FExpectedCalls.ContainsKey(MethodName) then
    Result := FExpectedCalls[MethodName]
  else
    Result := Null;
end;

function TSimpleMock.WasCalled(const MethodName: String): Boolean;  
var
  Call: TMethodCall;
begin
  Result := False;
  for Call in FCalls do
    if Call.MethodName = MethodName then
      Exit(True);
end;

function TSimpleMock.GetCallCount(const MethodName: String): Integer;  
var
  Call: TMethodCall;
begin
  Result := 0;
  for Call in FCalls do
    if Call.MethodName = MethodName then
      Inc(Result);
end;

function TSimpleMock.GetTotalCallCount: Integer;  
begin
  Result := FCalls.Count;
end;

procedure TSimpleMock.Reset;  
begin
  FCalls.Clear;
  FExpectedCalls.Clear;
end;

end.
```

## Bonnes pratiques

### 1. Préférer l'injection par constructeur

✅ **Bon :**
```pascal
constructor TService.Create(ARepository: IRepository; ALogger: ILogger);  
begin
  if ARepository = nil then
    raise Exception.Create('Repository requis');
  if ALogger = nil then
    raise Exception.Create('Logger requis');

  FRepository := ARepository;
  FLogger := ALogger;
end;
```

❌ **Mauvais :**
```pascal
constructor TService.Create;  
begin
  // Dépendances créées en dur
  FRepository := TRepository.Create(ConnectToDB);
  FLogger := TFileLogger.Create('log.txt');
end;
```

### 2. Utiliser des interfaces, pas des classes concrètes

✅ **Bon :**
```pascal
type
  TService = class
  private
    FRepository: IRepository;  // Interface
  public
    constructor Create(ARepository: IRepository);
  end;
```

❌ **Mauvais :**
```pascal
type
  TService = class
  private
    FRepository: TConcreteRepository;  // Classe concrète
  public
    constructor Create(ARepository: TConcreteRepository);
  end;
```

### 3. Un mock par responsabilité

✅ **Bon :**
```pascal
// Mock spécialisé pour la base de données
TMockDatabase = class(TInterfacedObject, IDatabase);

// Mock spécialisé pour le logger
TMockLogger = class(TInterfacedObject, ILogger);
```

❌ **Mauvais :**
```pascal
// Mock fourre-tout
TMockEverything = class(TInterfacedObject, IDatabase, ILogger, IEmailSender);
```

### 4. Nommer clairement les mocks

```pascal
// Noms explicites
TMockUserRepository  
TStubEmailService  
TSpyLogger  
TFakeDatabase

// Pas seulement "Mock"
TMock1, TMock2, TMyMock
```

### 5. Réinitialiser les mocks entre les tests

```pascal
procedure TMyTest.SetUp;  
begin
  inherited;
  FMock := TMockDatabase.Create;
  FMock.Reset;  // S'assurer qu'il est vide
end;

procedure TMyTest.TearDown;  
begin
  FMock := nil;  // Libération automatique (interface)
  inherited;
end;
```

### 6. Vérifier le comportement, pas l'implémentation

✅ **Bon :**
```pascal
// Vérifier le résultat
AssertEquals('Utilisateur créé', 'user@test.com', User.Email);
```

❌ **Mauvais :**
```pascal
// Vérifier les détails d'implémentation
AssertTrue('BeginTransaction appelé', Mock.WasBeginTransactionCalled);  
AssertTrue('SQL exact', Mock.LastSQL = 'INSERT INTO users...');
```

### 7. Documenter les mocks complexes

```pascal
type
  {
    Mock pour IEmailService utilisé dans les tests.

    Comportement :
    - Enregistre tous les emails "envoyés" dans une liste
    - Ne tente jamais de connexion SMTP réelle
    - Peut simuler des échecs d'envoi via ForceFailure

    Utilisation :
      Mock := TMockEmailService.Create;
      Mock.ForceFailure := True;  // Simuler un échec
      Service.SendEmail(...);
      Assert(Mock.GetSentCount = 0);  // Vérifier qu'aucun email n'a été envoyé
  }
  TMockEmailService = class(TInterfacedObject, IEmailService)
  // ...
  end;
```

## Stratégies de test selon le contexte

### Tests unitaires purs

```pascal
// 100% mock, aucune dépendance réelle
procedure TestCalculerTotal;  
var
  MockRepo: IProductRepository;
  MockTax: ITaxCalculator;
  Service: TOrderService;
begin
  MockRepo := TMockProductRepository.Create;
  MockTax := TMockTaxCalculator.Create;

  Service := TOrderService.Create(MockRepo, MockTax);
  try
    // Test isolé, rapide, prévisible
    AssertEquals(110.0, Service.CalculateTotal(100.0));
  finally
    Service.Free;
  end;
end;
```

### Tests d'intégration

```pascal
// Mix de vrais composants et mocks
procedure TestCreateOrder;  
var
  RealDB: IDatabaseConnection;
  MockEmail: IEmailService;
  Service: TOrderService;
begin
  RealDB := TSQLiteConnection.Create(':memory:');
  MockEmail := TMockEmailService.Create;  // Mock pour éviter SMTP

  Service := TOrderService.Create(RealDB, MockEmail);
  try
    // Test plus réaliste, un peu plus lent
    Service.CreateOrder(...);
    AssertTrue('Email de confirmation envoyé', MockEmail.WasSent);
  finally
    Service.Free;
  end;
end;
```

### Tests E2E

```pascal
// Tout est réel, sauf les services externes critiques
procedure TestCompleteCheckoutFlow;  
var
  RealDB: IDatabaseConnection;
  RealFileSystem: IFileSystem;
  MockPayment: IPaymentGateway;  // Mock seulement le paiement externe
  App: TApplication;
begin
  RealDB := ConnectToTestDatabase;
  RealFileSystem := TFileSystemFactory.Create;
  MockPayment := TMockPaymentGateway.Create;

  App := TApplication.Create(RealDB, RealFileSystem, MockPayment);
  try
    // Scénario complet, lent mais exhaustif
    App.ExecuteCheckout(...);
  finally
    App.Free;
  end;
end;
```

## Comparaison Windows vs Ubuntu

### Même approche, implémentations différentes

L'avantage majeur de l'injection de dépendances et du mocking est que votre **logique de test reste identique** sur Windows et Ubuntu :

```pascal
// Ce test fonctionne à l'identique sur les deux OS
procedure TestSauvegarderFichier;  
var
  MockFS: IFileSystem;
  Service: TDocumentService;
begin
  // Même mock sur Windows et Ubuntu
  MockFS := TMockFileSystem.Create;
  Service := TDocumentService.Create(MockFS);
  try
    Service.SaveDocument('test.doc', 'Contenu');
    AssertTrue('Fichier sauvegardé', MockFS.WasWriteCalled('test.doc'));
  finally
    Service.Free;
  end;
end;
```

Seules les **implémentations réelles** diffèrent :

```pascal
// Production sur Windows
{$IFDEF WINDOWS}
FileSystem := TFileSystemWindows.Create;
{$ENDIF}

// Production sur Ubuntu
{$IFDEF UNIX}
FileSystem := TFileSystemLinux.Create;
{$ENDIF}

// Tests : toujours le même mock
FileSystem := TMockFileSystem.Create;
```

## Résumé et conclusion

### Points clés à retenir

✅ **L'injection de dépendances rend le code testable**
- Passer les dépendances par constructeur
- Utiliser des interfaces plutôt que des classes concrètes
- Permet de substituer les implémentations

✅ **Le mocking isole les tests**
- Tests rapides (millisecondes)
- Tests fiables (pas de dépendances externes)
- Tests focalisés (une seule responsabilité)

✅ **Différents types d'objets de test**
- **Dummy** : remplir une signature
- **Stub** : retourner des valeurs fixes
- **Fake** : implémentation simplifiée qui fonctionne
- **Mock** : vérifier les interactions
- **Spy** : enregistrer les appels

✅ **Patterns utiles**
- Repository : abstraire l'accès aux données
- Factory : créer les bonnes implémentations selon le contexte
- Conteneur DI : automatiser l'injection

✅ **Multi-plateforme facilité**
- Même logique de test sur Windows et Ubuntu
- Seules les implémentations réelles diffèrent
- Les mocks sont identiques partout

### Progression recommandée

1. **Semaine 1** : Refactoriser une classe pour utiliser l'injection de dépendances
2. **Semaine 2** : Créer votre premier mock simple
3. **Semaine 3** : Extraire des interfaces de vos classes principales
4. **Semaine 4** : Créer des mocks pour vos interfaces
5. **Mois 2** : Implémenter le pattern Repository
6. **Mois 3** : Créer un conteneur DI simple

### Quand utiliser quoi ?

| Situation | Solution |
|-----------|----------|
| Test unitaire pur | Mock complet |
| Test d'intégration | Mix réel + mocks |
| Test E2E | Tout réel sauf services externes |
| Dépendance lente (DB, réseau) | Mock ou Fake |
| Dépendance externe (API tierce) | Mock ou Stub |
| Vérifier les appels | Spy ou Mock |
| Simplifier l'implémentation | Fake |

### Bénéfices concrets

Avec l'injection de dépendances et le mocking :

- **Tests 100x plus rapides** : millisecondes au lieu de secondes
- **Tests 100% fiables** : pas de dépendances réseau/DB
- **Code mieux conçu** : couplage faible, haute cohésion
- **Refactoring sûr** : les tests protègent contre les régressions
- **Multi-plateforme simplifié** : même logique, implémentations différentes

### Ressources complémentaires

- **Design Patterns** : Gang of Four (GoF)
- **Dependency Injection Principles** : Martin Fowler
- **Test Driven Development** : Kent Beck
- **Clean Code** : Robert C. Martin

L'injection de dépendances et le mocking sont des techniques **essentielles** pour tout développeur avancé. Elles transforment votre façon d'écrire et de tester du code, le rendant plus modulaire, testable et maintenable. Dans un contexte multi-plateforme comme FreePascal/Lazarus sur Windows et Ubuntu, ces techniques sont d'autant plus précieuses ! 🚀

⏭️ [Couverture de code](/18-tests-qualite-code/04-couverture-code.md)
