🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.2 Tests d'intégration automatisés

## Introduction

Après avoir maîtrisé les tests unitaires avec FPCUnit, nous passons à l'étape supérieure : les **tests d'intégration**. Alors que les tests unitaires vérifient des morceaux de code isolés, les tests d'intégration s'assurent que ces morceaux **fonctionnent correctement ensemble**.

### Différence entre tests unitaires et tests d'intégration

**Tests Unitaires :**
- Testent une seule fonction ou méthode
- Sont isolés de tout contexte externe
- S'exécutent en quelques millisecondes
- N'accèdent pas aux ressources externes (base de données, fichiers, réseau)

**Tests d'Intégration :**
- Testent plusieurs composants qui interagissent
- Utilisent de vraies ressources (base de données de test, fichiers, API)
- S'exécutent en secondes
- Vérifient que l'assemblage fonctionne

### Exemple concret

Imaginez une application de gestion d'utilisateurs :

**Test Unitaire :**
```pascal
// Teste uniquement la validation du mot de passe
procedure TestValidationMotDePasse;  
begin
  AssertTrue(EstMotDePasseValide('MonP@ssw0rd123'));
  AssertFalse(EstMotDePasseValide('12345'));
end;
```

**Test d'Intégration :**
```pascal
// Teste la création complète d'un utilisateur en base de données
procedure TestCreationUtilisateur;  
var
  Utilisateur: TUtilisateur;
begin
  // Crée l'utilisateur
  Utilisateur := TUtilisateur.Create('jean@example.com', 'MonP@ssw0rd123');

  // Sauvegarde en base de données
  GestionnaireUtilisateurs.Enregistrer(Utilisateur);

  // Vérifie qu'on peut le récupérer
  Utilisateur := GestionnaireUtilisateurs.ChargerParEmail('jean@example.com');
  AssertNotNull('Utilisateur doit exister', Utilisateur);
  AssertEquals('jean@example.com', Utilisateur.Email);
end;
```

## Pourquoi des tests d'intégration ?

### Les bugs d'intégration

Même si tous vos tests unitaires passent, votre application peut échouer pour plusieurs raisons :

1. **Problèmes de communication** : deux modules qui ne se comprennent pas
2. **Mauvaise gestion des transactions** : données partiellement enregistrées
3. **Erreurs de configuration** : mauvais paramètres de connexion
4. **Incompatibilités de version** : bibliothèque externe qui se comporte différemment
5. **Problèmes de timing** : opérations asynchrones mal synchronisées
6. **Différences d'environnement** : comportement différent sur Windows vs Ubuntu

### Exemple de bug d'intégration classique

```pascal
// Ces deux fonctions passent leurs tests unitaires individuellement
function CalculerTotalCommande(Commande: TCommande): Currency;  
begin
  Result := Commande.MontantHT * (1 + Commande.TauxTVA);
end;

function EnregistrerCommande(Commande: TCommande): Boolean;  
begin
  Commande.Total := CalculerTotalCommande(Commande);
  BaseDeDonnees.Sauvegarder(Commande);
  Result := True;
end;

// Mais il y a un bug d'intégration !
// Si TauxTVA est stocké en pourcentage (20) au lieu de décimal (0.20),
// le calcul sera faux uniquement quand les deux fonctions travaillent ensemble.
```

Un test d'intégration aurait détecté ce problème immédiatement.

## Architecture des tests d'intégration

### Structure recommandée

```
MonProjet/
├── src/                           # Code source
│   ├── models/
│   │   └── Utilisateur.pas
│   ├── database/
│   │   └── GestionnaireDB.pas
│   └── business/
│       └── GestionnaireUtilisateurs.pas
├── tests/
│   ├── unit/                      # Tests unitaires
│   │   └── TestUtilisateur.pas
│   └── integration/               # Tests d'intégration
│       ├── TestIntegrationDB.pas
│       └── TestIntegrationUtilisateurs.pas
├── config/
│   ├── test.ini                   # Configuration de test
│   └── test_database.sql          # Script d'initialisation DB
└── MonProjetTests.lpr
```

### Configuration de l'environnement de test

Les tests d'intégration nécessitent un environnement dédié :

```pascal
unit ConfigurationTests;

{$mode objfpc}{$H+}

interface

type
  TConfigurationTests = class
  private
    class var FCheminBase: String;
    class var FCheminFichiersTest: String;
    class var FConnectionStringDB: String;
  public
    class procedure Initialiser;
    class procedure Nettoyer;

    class property CheminBase: String read FCheminBase;
    class property CheminFichiersTest: String read FCheminFichiersTest;
    class property ConnectionStringDB: String read FConnectionStringDB;
  end;

implementation

uses
  SysUtils, IniFiles;

class procedure TConfigurationTests.Initialiser;  
var
  Config: TIniFile;
  CheminConfig: String;
begin
  // Chemin différent selon l'OS
  {$IFDEF WINDOWS}
  FCheminBase := GetEnvironmentVariable('TEMP') + '\MonProjetTests\';
  {$ENDIF}
  {$IFDEF UNIX}
  FCheminBase := '/tmp/MonProjetTests/';
  {$ENDIF}

  // Créer le répertoire de test s'il n'existe pas
  if not DirectoryExists(FCheminBase) then
    ForceDirectories(FCheminBase);

  FCheminFichiersTest := FCheminBase + 'fichiers' + PathDelim;
  ForceDirectories(FCheminFichiersTest);

  // Charger la configuration
  CheminConfig := ExtractFilePath(ParamStr(0)) + 'config' + PathDelim + 'test.ini';
  Config := TIniFile.Create(CheminConfig);
  try
    FConnectionStringDB := Config.ReadString('Database', 'ConnectionString',
      'localhost:test_db:testuser:testpass');
  finally
    Config.Free;
  end;
end;

class procedure TConfigurationTests.Nettoyer;  
begin
  // Nettoyer les fichiers temporaires
  if DirectoryExists(FCheminBase) then
    DeleteDirectory(FCheminBase, False);
end;

end.
```

## Tests d'intégration avec base de données

### Principe : base de données de test

**Règle d'or** : Ne jamais tester sur la base de données de production !

Créez une base de données dédiée aux tests :

```sql
-- Script de création : config/test_database.sql

-- Windows (SQL Server / MySQL)
CREATE DATABASE test_monprojet;  
USE test_monprojet;

-- Ubuntu (PostgreSQL)
CREATE DATABASE test_monprojet;
\c test_monprojet;

-- Tables (exemple)
CREATE TABLE utilisateurs (
    id INTEGER PRIMARY KEY AUTO_INCREMENT,
    email VARCHAR(255) NOT NULL UNIQUE,
    mot_de_passe_hash VARCHAR(255) NOT NULL,
    date_creation TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE commandes (
    id INTEGER PRIMARY KEY AUTO_INCREMENT,
    utilisateur_id INTEGER NOT NULL,
    montant_ht DECIMAL(10,2) NOT NULL,
    taux_tva DECIMAL(5,4) NOT NULL,
    total DECIMAL(10,2) NOT NULL,
    date_commande TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (utilisateur_id) REFERENCES utilisateurs(id)
);
```

### Classe de base pour les tests d'intégration DB

```pascal
unit TestBaseIntegrationDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  sqldb, pqconnection, mysql56conn, sqlite3conn,
  ConfigurationTests;

type
  TTestBaseIntegrationDB = class(TTestCase)
  private
    FConnexion: TSQLConnection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    // Méthodes utilitaires
    procedure InitialiserSchema;
    procedure NettoierTables;
    procedure ExecuterSQL(const SQL: String);
    function CompterLignes(const NomTable: String): Integer;

    property Connexion: TSQLConnection read FConnexion;
    property Transaction: TSQLTransaction read FTransaction;
    property Query: TSQLQuery read FQuery;
  end;

implementation

procedure TTestBaseIntegrationDB.SetUp;  
begin
  inherited;

  // Créer la connexion selon l'OS et le SGBD configuré
  {$IFDEF WINDOWS}
  // MySQL sur Windows
  FConnexion := TMySQL56Connection.Create(nil);
  FConnexion.HostName := 'localhost';
  FConnexion.DatabaseName := 'test_monprojet';
  FConnexion.UserName := 'testuser';
  FConnexion.Password := 'testpass';
  {$ENDIF}

  {$IFDEF UNIX}
  // PostgreSQL sur Ubuntu
  FConnexion := TPQConnection.Create(nil);
  FConnexion.HostName := 'localhost';
  FConnexion.DatabaseName := 'test_monprojet';
  FConnexion.UserName := 'testuser';
  FConnexion.Password := 'testpass';
  {$ENDIF}

  // Transaction
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Database := FConnexion;
  FConnexion.Transaction := FTransaction;

  // Query
  FQuery := TSQLQuery.Create(nil);
  FQuery.Database := FConnexion;
  FQuery.Transaction := FTransaction;

  // Connexion
  FConnexion.Open;

  // Nettoyer les tables avant chaque test
  NettoierTables;
end;

procedure TTestBaseIntegrationDB.TearDown;  
begin
  // Toujours faire un rollback pour ne pas polluer la base
  if FTransaction.Active then
    FTransaction.Rollback;

  FQuery.Free;
  FTransaction.Free;
  FConnexion.Free;

  inherited;
end;

procedure TTestBaseIntegrationDB.InitialiserSchema;  
begin
  // Lire et exécuter le script SQL de création
  // (implémentation simplifiée)
  ExecuterSQL('CREATE TABLE IF NOT EXISTS utilisateurs (...)');
  ExecuterSQL('CREATE TABLE IF NOT EXISTS commandes (...)');
end;

procedure TTestBaseIntegrationDB.NettoierTables;  
begin
  try
    // Supprimer toutes les données
    ExecuterSQL('DELETE FROM commandes');
    ExecuterSQL('DELETE FROM utilisateurs');
    FTransaction.Commit;
  except
    FTransaction.Rollback;
    raise;
  end;
end;

procedure TTestBaseIntegrationDB.ExecuterSQL(const SQL: String);  
begin
  FQuery.SQL.Text := SQL;
  FQuery.ExecSQL;
end;

function TTestBaseIntegrationDB.CompterLignes(const NomTable: String): Integer;  
begin
  FQuery.SQL.Text := 'SELECT COUNT(*) as total FROM ' + NomTable;
  FQuery.Open;
  try
    Result := FQuery.FieldByName('total').AsInteger;
  finally
    FQuery.Close;
  end;
end;

end.
```

### Exemple de test d'intégration avec base de données

```pascal
unit TestIntegrationUtilisateurs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  TestBaseIntegrationDB,
  Utilisateur, GestionnaireUtilisateurs;

type
  TTestIntegrationUtilisateurs = class(TTestBaseIntegrationDB)
  private
    FGestionnaire: TGestionnaireUtilisateurs;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreerUtilisateur;
    procedure TestChargerUtilisateur;
    procedure TestModifierUtilisateur;
    procedure TestSupprimerUtilisateur;
    procedure TestEmailDuplique;
    procedure TestTransactionRollback;
  end;

implementation

procedure TTestIntegrationUtilisateurs.SetUp;  
begin
  inherited;
  FGestionnaire := TGestionnaireUtilisateurs.Create(Connexion, Transaction);
end;

procedure TTestIntegrationUtilisateurs.TearDown;  
begin
  FGestionnaire.Free;
  inherited;
end;

procedure TTestIntegrationUtilisateurs.TestCreerUtilisateur;  
var
  Utilisateur: TUtilisateur;
  ID: Integer;
begin
  // Créer un utilisateur
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'test@example.com';
    Utilisateur.MotDePasse := 'MonP@ssw0rd123';

    // Sauvegarder
    ID := FGestionnaire.Creer(Utilisateur);
    Transaction.Commit;

    // Vérifications
    AssertTrue('ID doit être positif', ID > 0);
    AssertEquals('Doit y avoir 1 utilisateur', 1, CompterLignes('utilisateurs'));

    // Recharger et vérifier
    Utilisateur := FGestionnaire.ChargerParID(ID);
    AssertNotNull('Utilisateur doit exister', Utilisateur);
    AssertEquals('test@example.com', Utilisateur.Email);
  finally
    Utilisateur.Free;
  end;
end;

procedure TTestIntegrationUtilisateurs.TestChargerUtilisateur;  
var
  Utilisateur, UtilisateurCharge: TUtilisateur;
  ID: Integer;
begin
  // Créer et sauvegarder
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'charge@example.com';
    Utilisateur.MotDePasse := 'Test123!';
    ID := FGestionnaire.Creer(Utilisateur);
    Transaction.Commit;
  finally
    Utilisateur.Free;
  end;

  // Charger par ID
  UtilisateurCharge := FGestionnaire.ChargerParID(ID);
  try
    AssertNotNull('Chargement par ID', UtilisateurCharge);
    AssertEquals('charge@example.com', UtilisateurCharge.Email);
  finally
    UtilisateurCharge.Free;
  end;

  // Charger par email
  UtilisateurCharge := FGestionnaire.ChargerParEmail('charge@example.com');
  try
    AssertNotNull('Chargement par email', UtilisateurCharge);
    AssertEquals(ID, UtilisateurCharge.ID);
  finally
    UtilisateurCharge.Free;
  end;
end;

procedure TTestIntegrationUtilisateurs.TestModifierUtilisateur;  
var
  Utilisateur: TUtilisateur;
  ID: Integer;
begin
  // Créer
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'avant@example.com';
    Utilisateur.MotDePasse := 'Pass123!';
    ID := FGestionnaire.Creer(Utilisateur);
    Transaction.Commit;
  finally
    Utilisateur.Free;
  end;

  // Modifier
  Utilisateur := FGestionnaire.ChargerParID(ID);
  try
    Utilisateur.Email := 'apres@example.com';
    FGestionnaire.Modifier(Utilisateur);
    Transaction.Commit;
  finally
    Utilisateur.Free;
  end;

  // Vérifier
  Utilisateur := FGestionnaire.ChargerParID(ID);
  try
    AssertEquals('Email modifié', 'apres@example.com', Utilisateur.Email);
  finally
    Utilisateur.Free;
  end;
end;

procedure TTestIntegrationUtilisateurs.TestSupprimerUtilisateur;  
var
  Utilisateur: TUtilisateur;
  ID: Integer;
begin
  // Créer
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'supprimer@example.com';
    Utilisateur.MotDePasse := 'Pass123!';
    ID := FGestionnaire.Creer(Utilisateur);
    Transaction.Commit;
  finally
    Utilisateur.Free;
  end;

  AssertEquals('1 utilisateur avant suppression', 1, CompterLignes('utilisateurs'));

  // Supprimer
  FGestionnaire.Supprimer(ID);
  Transaction.Commit;

  // Vérifier
  AssertEquals('0 utilisateur après suppression', 0, CompterLignes('utilisateurs'));
  Utilisateur := FGestionnaire.ChargerParID(ID);
  AssertNull('Utilisateur supprimé', Utilisateur);
end;

procedure TTestIntegrationUtilisateurs.TestEmailDuplique;  
var
  Utilisateur1, Utilisateur2: TUtilisateur;
begin
  // Créer premier utilisateur
  Utilisateur1 := TUtilisateur.Create;
  try
    Utilisateur1.Email := 'double@example.com';
    Utilisateur1.MotDePasse := 'Pass123!';
    FGestionnaire.Creer(Utilisateur1);
    Transaction.Commit;
  finally
    Utilisateur1.Free;
  end;

  // Tenter de créer un doublon
  Utilisateur2 := TUtilisateur.Create;
  try
    Utilisateur2.Email := 'double@example.com';  // Même email !
    Utilisateur2.MotDePasse := 'AutrePass456!';

    // Doit lever une exception
    try
      FGestionnaire.Creer(Utilisateur2);
      Transaction.Commit;
      Fail('Devrait lever une exception pour email dupliqué');
    except
      on E: Exception do
      begin
        // C'est le comportement attendu
        Transaction.Rollback;
        AssertTrue('Message d''erreur approprié',
          Pos('email', LowerCase(E.Message)) > 0);
      end;
    end;
  finally
    Utilisateur2.Free;
  end;
end;

procedure TTestIntegrationUtilisateurs.TestTransactionRollback;  
var
  Utilisateur: TUtilisateur;
begin
  // Créer un utilisateur
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'rollback@example.com';
    Utilisateur.MotDePasse := 'Pass123!';
    FGestionnaire.Creer(Utilisateur);

    // NE PAS COMMIT, faire un rollback
    Transaction.Rollback;
  finally
    Utilisateur.Free;
  end;

  // Vérifier que l'utilisateur n'existe pas
  AssertEquals('Aucun utilisateur après rollback', 0, CompterLignes('utilisateurs'));
  Utilisateur := FGestionnaire.ChargerParEmail('rollback@example.com');
  AssertNull('Utilisateur annulé', Utilisateur);
end;

initialization
  RegisterTest(TTestIntegrationUtilisateurs);

end.
```

## Tests d'intégration avec fichiers

### Gestion des fichiers de test

```pascal
unit TestIntegrationFichiers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  ConfigurationTests, GestionnaireFichiers;

type
  TTestIntegrationFichiers = class(TTestCase)
  private
    FGestionnaire: TGestionnaireFichiers;
    FCheminTest: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function CreerFichierTest(const NomFichier, Contenu: String): String;
    procedure SupprimerFichierTest(const CheminComplet: String);
  published
    procedure TestLireFichier;
    procedure TestEcrireFichier;
    procedure TestCopierFichier;
    procedure TestDeplacerFichier;
    procedure TestSupprimerFichier;
    procedure TestCreerRepertoire;
    procedure TestListerFichiers;
    procedure TestCheminMultiPlateforme;
  end;

implementation

procedure TTestIntegrationFichiers.SetUp;  
begin
  inherited;

  // Initialiser le chemin de test
  FCheminTest := TConfigurationTests.CheminFichiersTest;

  // Nettoyer le répertoire
  if DirectoryExists(FCheminTest) then
    DeleteDirectory(FCheminTest, False);
  ForceDirectories(FCheminTest);

  FGestionnaire := TGestionnaireFichiers.Create;
end;

procedure TTestIntegrationFichiers.TearDown;  
begin
  FGestionnaire.Free;

  // Nettoyer les fichiers de test
  if DirectoryExists(FCheminTest) then
    DeleteDirectory(FCheminTest, False);

  inherited;
end;

function TTestIntegrationFichiers.CreerFichierTest(const NomFichier, Contenu: String): String;  
var
  Fichier: TextFile;
begin
  Result := FCheminTest + NomFichier;
  AssignFile(Fichier, Result);
  Rewrite(Fichier);
  try
    WriteLn(Fichier, Contenu);
  finally
    CloseFile(Fichier);
  end;
end;

procedure TTestIntegrationFichiers.SupprimerFichierTest(const CheminComplet: String);  
begin
  if FileExists(CheminComplet) then
    DeleteFile(CheminComplet);
end;

procedure TTestIntegrationFichiers.TestLireFichier;  
var
  CheminFichier, Contenu: String;
begin
  // Créer un fichier de test
  CheminFichier := CreerFichierTest('lecture.txt', 'Contenu de test');

  // Lire le fichier
  Contenu := FGestionnaire.LireFichier(CheminFichier);

  // Vérifier
  AssertTrue('Contenu non vide', Length(Contenu) > 0);
  AssertTrue('Contenu correct', Pos('Contenu de test', Contenu) > 0);
end;

procedure TTestIntegrationFichiers.TestEcrireFichier;  
var
  CheminFichier, ContenuEcrit, ContenuLu: String;
begin
  CheminFichier := FCheminTest + 'ecriture.txt';
  ContenuEcrit := 'Nouveau contenu' + LineEnding + 'Ligne 2';

  // Écrire
  FGestionnaire.EcrireFichier(CheminFichier, ContenuEcrit);

  // Vérifier que le fichier existe
  AssertTrue('Fichier créé', FileExists(CheminFichier));

  // Lire et vérifier le contenu
  ContenuLu := FGestionnaire.LireFichier(CheminFichier);
  AssertEquals('Contenu identique', ContenuEcrit, ContenuLu);
end;

procedure TTestIntegrationFichiers.TestCopierFichier;  
var
  CheminSource, CheminDest, ContenuSource, ContenuDest: String;
begin
  // Créer le fichier source
  CheminSource := CreerFichierTest('source.txt', 'Contenu original');
  CheminDest := FCheminTest + 'destination.txt';

  // Copier
  FGestionnaire.CopierFichier(CheminSource, CheminDest);

  // Vérifier que les deux fichiers existent
  AssertTrue('Source existe toujours', FileExists(CheminSource));
  AssertTrue('Destination créée', FileExists(CheminDest));

  // Vérifier que le contenu est identique
  ContenuSource := FGestionnaire.LireFichier(CheminSource);
  ContenuDest := FGestionnaire.LireFichier(CheminDest);
  AssertEquals('Contenu identique', ContenuSource, ContenuDest);
end;

procedure TTestIntegrationFichiers.TestDeplacerFichier;  
var
  CheminSource, CheminDest, Contenu: String;
begin
  // Créer le fichier source
  CheminSource := CreerFichierTest('avantDeplacement.txt', 'Contenu à déplacer');
  CheminDest := FCheminTest + 'apresDeplacement.txt';

  Contenu := FGestionnaire.LireFichier(CheminSource);

  // Déplacer
  FGestionnaire.DeplacerFichier(CheminSource, CheminDest);

  // Vérifier
  AssertFalse('Source n''existe plus', FileExists(CheminSource));
  AssertTrue('Destination existe', FileExists(CheminDest));
  AssertEquals('Contenu préservé', Contenu, FGestionnaire.LireFichier(CheminDest));
end;

procedure TTestIntegrationFichiers.TestSupprimerFichier;  
var
  CheminFichier: String;
begin
  // Créer un fichier
  CheminFichier := CreerFichierTest('aSupprimer.txt', 'Ce fichier sera supprimé');
  AssertTrue('Fichier existe avant', FileExists(CheminFichier));

  // Supprimer
  FGestionnaire.SupprimerFichier(CheminFichier);

  // Vérifier
  AssertFalse('Fichier supprimé', FileExists(CheminFichier));
end;

procedure TTestIntegrationFichiers.TestCreerRepertoire;  
var
  CheminRepertoire: String;
begin
  CheminRepertoire := FCheminTest + 'nouveau_repertoire' + PathDelim;

  // Créer
  FGestionnaire.CreerRepertoire(CheminRepertoire);

  // Vérifier
  AssertTrue('Répertoire créé', DirectoryExists(CheminRepertoire));
end;

procedure TTestIntegrationFichiers.TestListerFichiers;  
var
  Fichiers: TStringList;
begin
  // Créer plusieurs fichiers
  CreerFichierTest('fichier1.txt', 'Contenu 1');
  CreerFichierTest('fichier2.txt', 'Contenu 2');
  CreerFichierTest('fichier3.log', 'Contenu 3');

  // Lister tous les fichiers
  Fichiers := FGestionnaire.ListerFichiers(FCheminTest);
  try
    AssertEquals('3 fichiers', 3, Fichiers.Count);
  finally
    Fichiers.Free;
  end;

  // Lister seulement les .txt
  Fichiers := FGestionnaire.ListerFichiers(FCheminTest, '*.txt');
  try
    AssertEquals('2 fichiers .txt', 2, Fichiers.Count);
  finally
    Fichiers.Free;
  end;
end;

procedure TTestIntegrationFichiers.TestCheminMultiPlateforme;  
var
  Chemin: String;
begin
  // Construire un chemin multi-plateforme
  Chemin := FGestionnaire.ConstruireChemin(['dossier1', 'dossier2', 'fichier.txt']);

  {$IFDEF WINDOWS}
  AssertTrue('Séparateur Windows', Pos('\', Chemin) > 0);
  {$ENDIF}

  {$IFDEF UNIX}
  AssertTrue('Séparateur Unix', Pos('/', Chemin) > 0);
  {$ENDIF}

  // Le chemin doit se terminer par 'fichier.txt'
  AssertTrue('Nom de fichier correct', Pos('fichier.txt', Chemin) > 0);
end;

initialization
  RegisterTest(TTestIntegrationFichiers);

end.
```

## Tests d'intégration avec API externes

### Mock vs tests réels

Pour les API externes (services web, API REST), vous avez deux options :

**Option 1 : Tests avec mock (simulation)**
- Plus rapides
- Pas de dépendance réseau
- Contrôle total des réponses
- Ne testent pas la vraie API

**Option 2 : Tests avec vraie API**
- Plus lents
- Dépendent de la disponibilité du service
- Testent l'intégration réelle
- Plus proches de la production

### Exemple avec une API REST

```pascal
unit TestIntegrationAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fphttpclient, fpjson, jsonparser,
  ClientAPI;

type
  TTestIntegrationAPI = class(TTestCase)
  private
    FClient: TClientAPI;
    FURLBase: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnexionAPI;
    procedure TestRecupererUtilisateur;
    procedure TestCreerUtilisateur;
    procedure TestModifierUtilisateur;
    procedure TestSupprimerUtilisateur;
    procedure TestGestionErreurs;
  end;

implementation

procedure TTestIntegrationAPI.SetUp;  
begin
  inherited;

  // URL de l'API de test
  {$IFDEF WINDOWS}
  FURLBase := 'http://localhost:8080/api/';
  {$ENDIF}
  {$IFDEF UNIX}
  FURLBase := 'http://localhost:8080/api/';
  {$ENDIF}

  FClient := TClientAPI.Create;
  FClient.URLBase := FURLBase;
  FClient.Token := 'test_token_12345';  // Token de test
end;

procedure TTestIntegrationAPI.TearDown;  
begin
  FClient.Free;
  inherited;
end;

procedure TTestIntegrationAPI.TestConnexionAPI;  
var
  Reponse: TJSONObject;
begin
  // Tester la connexion à l'API
  Reponse := FClient.Ping;
  try
    AssertNotNull('Réponse non nulle', Reponse);
    AssertEquals('Statut OK', 'ok', Reponse.Get('status', ''));
  finally
    Reponse.Free;
  end;
end;

procedure TTestIntegrationAPI.TestRecupererUtilisateur;  
var
  Utilisateur: TJSONObject;
begin
  // Récupérer un utilisateur de test (ID 1)
  Utilisateur := FClient.ObtenirUtilisateur(1);
  try
    AssertNotNull('Utilisateur non nul', Utilisateur);
    AssertTrue('ID présent', Utilisateur.IndexOfName('id') >= 0);
    AssertTrue('Email présent', Utilisateur.IndexOfName('email') >= 0);
    AssertEquals('ID correct', 1, Utilisateur.Get('id', 0));
  finally
    Utilisateur.Free;
  end;
end;

procedure TTestIntegrationAPI.TestCreerUtilisateur;  
var
  NouvelUtilisateur, Resultat: TJSONObject;
  ID: Integer;
begin
  // Créer les données de l'utilisateur
  NouvelUtilisateur := TJSONObject.Create;
  try
    NouvelUtilisateur.Add('email', 'nouveau@test.com');
    NouvelUtilisateur.Add('nom', 'Test');
    NouvelUtilisateur.Add('prenom', 'Utilisateur');

    // Envoyer à l'API
    Resultat := FClient.CreerUtilisateur(NouvelUtilisateur);
    try
      AssertNotNull('Résultat non nul', Resultat);
      ID := Resultat.Get('id', 0);
      AssertTrue('ID assigné', ID > 0);
      AssertEquals('Email correct', 'nouveau@test.com', Resultat.Get('email', ''));

      // Nettoyer : supprimer l'utilisateur créé
      FClient.SupprimerUtilisateur(ID);
    finally
      Resultat.Free;
    end;
  finally
    NouvelUtilisateur.Free;
  end;
end;

procedure TTestIntegrationAPI.TestModifierUtilisateur;  
var
  Utilisateur, Modification, UtilisateurModifie: TJSONObject;
  ID: Integer;
begin
  // D'abord créer un utilisateur
  Utilisateur := TJSONObject.Create;
  try
    Utilisateur.Add('email', 'amodifier@test.com');
    Utilisateur.Add('nom', 'Avant');

    Utilisateur := FClient.CreerUtilisateur(Utilisateur);
    ID := Utilisateur.Get('id', 0);
  finally
    Utilisateur.Free;
  end;

  // Modifier l'utilisateur
  Modification := TJSONObject.Create;
  try
    Modification.Add('nom', 'Apres');

    UtilisateurModifie := FClient.ModifierUtilisateur(ID, Modification);
    try
      AssertEquals('Nom modifié', 'Apres', UtilisateurModifie.Get('nom', ''));
      AssertEquals('Email inchangé', 'amodifier@test.com',
                   UtilisateurModifie.Get('email', ''));
    finally
      UtilisateurModifie.Free;
    end;
  finally
    Modification.Free;
  end;

  // Nettoyer
  FClient.SupprimerUtilisateur(ID);
end;

procedure TTestIntegrationAPI.TestSupprimerUtilisateur;  
var
  Utilisateur: TJSONObject;
  ID: Integer;
  Supprime: Boolean;
begin
  // Créer un utilisateur
  Utilisateur := TJSONObject.Create;
  try
    Utilisateur.Add('email', 'asupprimer@test.com');
    Utilisateur.Add('nom', 'Test');

    Utilisateur := FClient.CreerUtilisateur(Utilisateur);
    ID := Utilisateur.Get('id', 0);
  finally
    Utilisateur.Free;
  end;

  // Supprimer
  Supprime := FClient.SupprimerUtilisateur(ID);
  AssertTrue('Suppression réussie', Supprime);

  // Vérifier que l'utilisateur n'existe plus
  try
    Utilisateur := FClient.ObtenirUtilisateur(ID);
    if Utilisateur <> nil then
    begin
      Utilisateur.Free;
      Fail('L''utilisateur devrait être supprimé');
    end;
  except
    on E: Exception do
      // Exception attendue (404 Not Found)
      AssertTrue('Erreur 404', Pos('404', E.Message) > 0);
  end;
end;

procedure TTestIntegrationAPI.TestGestionErreurs;  
begin
  // Tester avec un ID inexistant
  try
    FClient.ObtenirUtilisateur(999999);
    Fail('Devrait lever une exception');
  except
    on E: Exception do
      AssertTrue('Erreur appropriée', Pos('404', E.Message) > 0);
  end;

  // Tester avec des données invalides
  try
    FClient.CreerUtilisateur(nil);
    Fail('Devrait lever une exception');
  except
    on E: Exception do
      AssertTrue('Erreur de validation', Length(E.Message) > 0);
  end;
end;

initialization
  RegisterTest(TTestIntegrationAPI);

end.
```

## Tests d'intégration multi-composants

### Scénario complet : commande e-commerce

Testons un scénario réaliste impliquant plusieurs composants :

```pascal
unit TestIntegrationCommande;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  TestBaseIntegrationDB,
  Utilisateur, Produit, Commande, Panier,
  GestionnaireUtilisateurs, GestionnaireProduits,
  GestionnaireCommandes, ServicePaiement;

type
  TTestIntegrationCommande = class(TTestBaseIntegrationDB)
  private
    FGestionnaireUtilisateurs: TGestionnaireUtilisateurs;
    FGestionnaireProduits: TGestionnaireProduits;
    FGestionnaireCommandes: TGestionnaireCommandes;
    FServicePaiement: TServicePaiement;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestScenarioCompletCommande;
    procedure TestCommandeAvecProduitsIndisponibles;
    procedure TestCommandeAvecPaiementEchoue;
    procedure TestCommandeAvecReduction;
  end;

implementation

procedure TTestIntegrationCommande.SetUp;  
begin
  inherited;

  FGestionnaireUtilisateurs := TGestionnaireUtilisateurs.Create(Connexion, Transaction);
  FGestionnaireProduits := TGestionnaireProduits.Create(Connexion, Transaction);
  FGestionnaireCommandes := TGestionnaireCommandes.Create(Connexion, Transaction);
  FServicePaiement := TServicePaiement.Create;

  // Configurer le service de paiement en mode test
  FServicePaiement.ModeTest := True;
end;

procedure TTestIntegrationCommande.TearDown;  
begin
  FServicePaiement.Free;
  FGestionnaireCommandes.Free;
  FGestionnaireProduits.Free;
  FGestionnaireUtilisateurs.Free;
  inherited;
end;

procedure TTestIntegrationCommande.TestScenarioCompletCommande;  
var
  Utilisateur: TUtilisateur;
  Produit1, Produit2: TProduit;
  Panier: TPanier;
  Commande: TCommande;
  IDUtilisateur, IDCommande: Integer;
begin
  // 1. Créer un utilisateur
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'client@test.com';
    Utilisateur.Nom := 'Dupont';
    Utilisateur.Prenom := 'Jean';
    IDUtilisateur := FGestionnaireUtilisateurs.Creer(Utilisateur);
    Transaction.Commit;
  finally
    Utilisateur.Free;
  end;

  // 2. Créer des produits
  Produit1 := TProduit.Create;
  try
    Produit1.Nom := 'Ordinateur portable';
    Produit1.Prix := 899.99;
    Produit1.Stock := 10;
    FGestionnaireProduits.Creer(Produit1);
  finally
    Produit1.Free;
  end;

  Produit2 := TProduit.Create;
  try
    Produit2.Nom := 'Souris sans fil';
    Produit2.Prix := 29.99;
    Produit2.Stock := 50;
    FGestionnaireProduits.Creer(Produit2);
    Transaction.Commit;
  finally
    Produit2.Free;
  end;

  // 3. Créer un panier
  Panier := TPanier.Create(IDUtilisateur);
  try
    // Recharger les produits avec leurs IDs
    Produit1 := FGestionnaireProduits.ChargerParNom('Ordinateur portable');
    Produit2 := FGestionnaireProduits.ChargerParNom('Souris sans fil');

    try
      // Ajouter au panier
      Panier.AjouterProduit(Produit1, 1);
      Panier.AjouterProduit(Produit2, 2);

      // Vérifier le total
      AssertEquals('Total panier', 959.97, Panier.CalculerTotal, 0.01);

      // 4. Créer la commande
      Commande := FGestionnaireCommandes.CreerDepuisPanier(Panier);
      try
        IDCommande := Commande.ID;
        AssertTrue('Commande créée', IDCommande > 0);
        AssertEquals('Statut initial', 'en_attente', Commande.Statut);

        // 5. Effectuer le paiement
        AssertTrue('Paiement réussi',
          FServicePaiement.Traiter(Commande.ID, Commande.Total));

        // 6. Mettre à jour le statut
        FGestionnaireCommandes.ChangerStatut(IDCommande, 'payee');
        Transaction.Commit;

        // 7. Vérifier le stock des produits
        Produit1 := FGestionnaireProduits.ChargerParID(Produit1.ID);
        Produit2 := FGestionnaireProduits.ChargerParID(Produit2.ID);

        AssertEquals('Stock produit 1 diminué', 9, Produit1.Stock);
        AssertEquals('Stock produit 2 diminué', 48, Produit2.Stock);

        // 8. Vérifier la commande finale
        Commande := FGestionnaireCommandes.ChargerParID(IDCommande);
        AssertEquals('Statut final', 'payee', Commande.Statut);
        AssertEquals('3 articles', 3, Commande.NombreArticles);

      finally
        Commande.Free;
      end;

    finally
      Produit1.Free;
      Produit2.Free;
    end;

  finally
    Panier.Free;
  end;
end;

procedure TTestIntegrationCommande.TestCommandeAvecProduitsIndisponibles;  
var
  Utilisateur: TUtilisateur;
  Produit: TProduit;
  Panier: TPanier;
  IDUtilisateur: Integer;
begin
  // Créer utilisateur
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'client2@test.com';
    IDUtilisateur := FGestionnaireUtilisateurs.Creer(Utilisateur);
    Transaction.Commit;
  finally
    Utilisateur.Free;
  end;

  // Créer un produit avec stock limité
  Produit := TProduit.Create;
  try
    Produit.Nom := 'Produit rare';
    Produit.Prix := 99.99;
    Produit.Stock := 1;  // Seulement 1 en stock
    FGestionnaireProduits.Creer(Produit);
    Transaction.Commit;
  finally
    Produit.Free;
  end;

  // Tenter d'ajouter plus que le stock disponible
  Panier := TPanier.Create(IDUtilisateur);
  try
    Produit := FGestionnaireProduits.ChargerParNom('Produit rare');
    try
      try
        Panier.AjouterProduit(Produit, 5);  // Demander 5 alors qu'il n'y en a que 1
        Fail('Devrait lever une exception de stock insuffisant');
      except
        on E: EStockInsuffisant do
        begin
          // Exception attendue
          AssertTrue('Message d''erreur approprié',
            Pos('stock', LowerCase(E.Message)) > 0);
        end;
      end;
    finally
      Produit.Free;
    end;
  finally
    Panier.Free;
  end;
end;

procedure TTestIntegrationCommande.TestCommandeAvecPaiementEchoue;  
var
  Utilisateur: TUtilisateur;
  Produit: TProduit;
  Panier: TPanier;
  Commande: TCommande;
  IDUtilisateur: Integer;
begin
  // Créer l'utilisateur et le produit
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'client3@test.com';
    IDUtilisateur := FGestionnaireUtilisateurs.Creer(Utilisateur);
    Transaction.Commit;
  finally
    Utilisateur.Free;
  end;

  Produit := TProduit.Create;
  try
    Produit.Nom := 'Produit test';
    Produit.Prix := 50.00;
    Produit.Stock := 10;
    FGestionnaireProduits.Creer(Produit);
    Transaction.Commit;
  finally
    Produit.Free;
  end;

  // Créer panier et commande
  Panier := TPanier.Create(IDUtilisateur);
  try
    Produit := FGestionnaireProduits.ChargerParNom('Produit test');
    try
      Panier.AjouterProduit(Produit, 1);

      Commande := FGestionnaireCommandes.CreerDepuisPanier(Panier);
      try
        // Forcer l'échec du paiement en mode test
        FServicePaiement.ForceEchec := True;

        try
          FServicePaiement.Traiter(Commande.ID, Commande.Total);
          Fail('Le paiement aurait dû échouer');
        except
          on E: EPaiementEchoue do
          begin
            // Exception attendue
            // Vérifier que la commande est en statut 'echec'
            FGestionnaireCommandes.ChangerStatut(Commande.ID, 'echec_paiement');
            Transaction.Commit;

            Commande := FGestionnaireCommandes.ChargerParID(Commande.ID);
            AssertEquals('Statut échec', 'echec_paiement', Commande.Statut);

            // Vérifier que le stock n'a pas été débité
            Produit := FGestionnaireProduits.ChargerParNom('Produit test');
            AssertEquals('Stock inchangé', 10, Produit.Stock);
          end;
        end;

      finally
        Commande.Free;
      end;

    finally
      Produit.Free;
    end;
  finally
    Panier.Free;
  end;
end;

procedure TTestIntegrationCommande.TestCommandeAvecReduction;  
var
  Utilisateur: TUtilisateur;
  Produit: TProduit;
  Panier: TPanier;
  Commande: TCommande;
  CodePromo: TCodePromo;
  IDUtilisateur: Integer;
  TotalAvant, TotalApres: Currency;
begin
  // Créer utilisateur et produit
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'client4@test.com';
    IDUtilisateur := FGestionnaireUtilisateurs.Creer(Utilisateur);
    Transaction.Commit;
  finally
    Utilisateur.Free;
  end;

  Produit := TProduit.Create;
  try
    Produit.Nom := 'Produit promo';
    Produit.Prix := 100.00;
    Produit.Stock := 10;
    FGestionnaireProduits.Creer(Produit);
    Transaction.Commit;
  finally
    Produit.Free;
  end;

  // Créer un code promo
  CodePromo := TCodePromo.Create;
  try
    CodePromo.Code := 'REDUC20';
    CodePromo.Pourcentage := 20;
    FGestionnaireCommandes.CreerCodePromo(CodePromo);
    Transaction.Commit;
  finally
    CodePromo.Free;
  end;

  // Créer panier et appliquer le code promo
  Panier := TPanier.Create(IDUtilisateur);
  try
    Produit := FGestionnaireProduits.ChargerParNom('Produit promo');
    try
      Panier.AjouterProduit(Produit, 1);
      TotalAvant := Panier.CalculerTotal;

      // Appliquer le code promo
      Panier.AppliquerCodePromo('REDUC20');
      TotalApres := Panier.CalculerTotal;

      // Vérifications
      AssertEquals('Total avant réduction', 100.00, TotalAvant, 0.01);
      AssertEquals('Total après réduction', 80.00, TotalApres, 0.01);

      // Créer la commande avec réduction
      Commande := FGestionnaireCommandes.CreerDepuisPanier(Panier);
      try
        AssertEquals('Code promo enregistré', 'REDUC20', Commande.CodePromo);
        AssertEquals('Total avec réduction', 80.00, Commande.Total, 0.01);
      finally
        Commande.Free;
      end;

    finally
      Produit.Free;
    end;
  finally
    Panier.Free;
  end;
end;

initialization
  RegisterTest(TTestIntegrationCommande);

end.
```

## Stratégies de nettoyage et isolation

### Pourquoi le nettoyage est crucial

Chaque test doit partir d'un état propre et prévisible. Sans nettoyage approprié :
- Un test peut échouer à cause d'un test précédent
- Les tests ne sont plus indépendants
- L'ordre d'exécution devient important
- Le debugging devient un cauchemar

### Stratégies de nettoyage

#### 1. Rollback des transactions

```pascal
procedure TTestBaseIntegrationDB.TearDown;  
begin
  // TOUJOURS faire un rollback si la transaction est active
  if FTransaction.Active then
    FTransaction.Rollback;

  // Puis libérer les ressources
  FQuery.Free;
  FTransaction.Free;
  FConnexion.Free;

  inherited;
end;
```

#### 2. Suppression explicite

```pascal
procedure TTestIntegrationFichiers.TearDown;  
begin
  // Supprimer tous les fichiers de test
  if DirectoryExists(FCheminTest) then
  begin
    DeleteDirectory(FCheminTest, False);
    WriteLn('Répertoire de test nettoyé : ', FCheminTest);
  end;

  FGestionnaire.Free;
  inherited;
end;
```

#### 3. Réinitialisation de base de données

```pascal
procedure TTestBaseIntegrationDB.NettoierTables;  
begin
  try
    // Désactiver temporairement les contraintes
    ExecuterSQL('SET FOREIGN_KEY_CHECKS = 0');  // MySQL
    // ou
    ExecuterSQL('SET session_replication_role = replica');  // PostgreSQL

    // Vider toutes les tables de test
    ExecuterSQL('TRUNCATE TABLE commandes');
    ExecuterSQL('TRUNCATE TABLE produits');
    ExecuterSQL('TRUNCATE TABLE utilisateurs');

    // Réactiver les contraintes
    ExecuterSQL('SET FOREIGN_KEY_CHECKS = 1');  // MySQL
    // ou
    ExecuterSQL('SET session_replication_role = DEFAULT');  // PostgreSQL

    FTransaction.Commit;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      WriteLn('ERREUR nettoyage : ', E.Message);
      raise;
    end;
  end;
end;
```

#### 4. Pattern SetUp/TearDown avec compteur

```pascal
type
  TTestAvecCompteur = class(TTestCase)
  private
    class var FCompteurTests: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

procedure TTestAvecCompteur.SetUp;  
begin
  inherited;
  Inc(FCompteurTests);
  WriteLn(Format('Début test #%d : %s', [FCompteurTests, TestName]));
end;

procedure TTestAvecCompteur.TearDown;  
begin
  WriteLn(Format('Fin test #%d : %s', [FCompteurTests, TestName]));
  inherited;
end;
```

## Gestion des dépendances externes

### Problème des dépendances

Les tests d'intégration dépendent souvent de :
- Bases de données
- Services web externes
- Systèmes de fichiers
- Services système (email, SMS, etc.)

### Solution 1 : Variables d'environnement

```pascal
function ObtenirConnectionString: String;  
begin
  // Essayer d'abord la variable d'environnement
  Result := GetEnvironmentVariable('DB_TEST_CONNECTION');

  // Sinon, utiliser une valeur par défaut
  if Result = '' then
  begin
    {$IFDEF WINDOWS}
    Result := 'localhost:3306:test_db:root:';
    {$ENDIF}
    {$IFDEF UNIX}
    Result := 'localhost:5432:test_db:postgres:postgres';
    {$ENDIF}
  end;
end;
```

### Solution 2 : Fichier de configuration

```ini
; config/test.ini
[Database]
Host=localhost  
Port=5432  
Database=test_monprojet  
User=testuser  
Password=testpass

[API]
BaseURL=http://localhost:8080/api  
Token=test_token_12345  
Timeout=30

[Files]
TempPath=/tmp/tests/
```

```pascal
procedure ChargerConfiguration;  
var
  Config: TIniFile;
begin
  Config := TIniFile.Create('config' + PathDelim + 'test.ini');
  try
    FDBHost := Config.ReadString('Database', 'Host', 'localhost');
    FDBPort := Config.ReadInteger('Database', 'Port', 5432);
    FDBName := Config.ReadString('Database', 'Database', 'test');
    FDBUser := Config.ReadString('Database', 'User', 'test');
    FDBPass := Config.ReadString('Database', 'Password', '');
  finally
    Config.Free;
  end;
end;
```

### Solution 3 : Mode test pour services externes

```pascal
type
  TServiceEmail = class
  private
    FModeTest: Boolean;
    FEmailsEnvoyes: TStringList;  // Stocker les emails en mode test
  public
    constructor Create;
    destructor Destroy; override;

    procedure EnvoyerEmail(const Destinataire, Sujet, Corps: String);

    property ModeTest: Boolean read FModeTest write FModeTest;
    property EmailsEnvoyes: TStringList read FEmailsEnvoyes;
  end;

procedure TServiceEmail.EnvoyerEmail(const Destinataire, Sujet, Corps: String);  
begin
  if FModeTest then
  begin
    // En mode test, ne pas envoyer réellement, juste logger
    FEmailsEnvoyes.Add(Format('%s|%s|%s', [Destinataire, Sujet, Corps]));
    WriteLn(Format('[MODE TEST] Email à %s : %s', [Destinataire, Sujet]));
  end
  else
  begin
    // En mode production, envoyer vraiment
    EnvoyerEmailReel(Destinataire, Sujet, Corps);
  end;
end;
```

## Tests multi-plateformes (Windows/Ubuntu)

### Défis spécifiques

#### Chemins de fichiers

```pascal
procedure TTestMultiPlateforme.TestCheminFichier;  
var
  CheminAttendu: String;
begin
  {$IFDEF WINDOWS}
  CheminAttendu := 'C:\temp\test.txt';
  {$ENDIF}
  {$IFDEF UNIX}
  CheminAttendu := '/tmp/test.txt';
  {$ENDIF}

  AssertEquals('Chemin correct', CheminAttendu,
    FGestionnaire.ObtenirCheminTemp('test.txt'));
end;

// Meilleure approche : utiliser PathDelim
procedure TTestMultiPlateforme.TestCheminFichierPortable;  
var
  Chemin: String;
begin
  Chemin := FGestionnaire.ObtenirCheminTemp('test.txt');

  // Vérifier que le séparateur est correct
  {$IFDEF WINDOWS}
  AssertTrue('Séparateur Windows', Pos('\', Chemin) > 0);
  {$ENDIF}
  {$IFDEF UNIX}
  AssertTrue('Séparateur Unix', Pos('/', Chemin) > 0);
  {$ENDIF}

  // Vérifier que le fichier se termine bien par 'test.txt'
  AssertTrue('Nom fichier correct', EndsStr('test.txt', Chemin));
end;
```

#### Fins de ligne

```pascal
procedure TTestMultiPlateforme.TestFinsDeLigne;  
var
  Contenu, ContenuLu: String;
begin
  // Créer un fichier avec les fins de ligne de la plateforme
  Contenu := 'Ligne 1' + LineEnding + 'Ligne 2' + LineEnding + 'Ligne 3';
  FGestionnaire.EcrireFichier('test.txt', Contenu);

  // Lire et vérifier
  ContenuLu := FGestionnaire.LireFichier('test.txt');
  AssertEquals('Contenu identique', Contenu, ContenuLu);

  {$IFDEF WINDOWS}
  AssertTrue('CRLF sur Windows', Pos(#13#10, ContenuLu) > 0);
  {$ENDIF}
  {$IFDEF UNIX}
  AssertTrue('LF sur Unix', Pos(#10, ContenuLu) > 0);
  AssertFalse('Pas de CR sur Unix', Pos(#13, ContenuLu) > 0);
  {$ENDIF}
end;
```

#### Permissions de fichiers (Unix)

```pascal
{$IFDEF UNIX}
procedure TTestMultiPlateformeUnix.TestPermissionsFichier;  
var
  CheminFichier: String;
  Permissions: Integer;
begin
  CheminFichier := FCheminTest + 'fichier_prive.txt';

  // Créer un fichier
  FGestionnaire.EcrireFichier(CheminFichier, 'Contenu privé');

  // Définir les permissions à 600 (rw-------)
  FGestionnaire.DefinirPermissions(CheminFichier, $1B0);  // Octal 600

  // Vérifier les permissions
  Permissions := FGestionnaire.ObtenirPermissions(CheminFichier);
  AssertEquals('Permissions 600', $1B0, Permissions);

  // Vérifier qu'on peut lire en tant que propriétaire
  AssertTrue('Fichier lisible', FileExists(CheminFichier));

  // Vérifier que le fichier n'est pas exécutable
  AssertFalse('Pas exécutable', FGestionnaire.EstExecutable(CheminFichier));
end;
{$ENDIF}

{$IFDEF WINDOWS}
procedure TTestMultiPlateformeWindows.TestAttributsFichier;  
var
  CheminFichier: String;
  Attributs: Integer;
begin
  CheminFichier := FCheminTest + 'fichier_cache.txt';

  // Créer un fichier
  FGestionnaire.EcrireFichier(CheminFichier, 'Contenu caché');

  // Définir l'attribut caché sur Windows
  FGestionnaire.DefinirAttribut(CheminFichier, faHidden);

  // Vérifier l'attribut
  Attributs := FileGetAttr(CheminFichier);
  AssertTrue('Fichier caché', (Attributs and faHidden) <> 0);

  // Rendre visible à nouveau
  FGestionnaire.EnleverAttribut(CheminFichier, faHidden);
  Attributs := FileGetAttr(CheminFichier);
  AssertFalse('Fichier visible', (Attributs and faHidden) <> 0);
end;
{$ENDIF}
```

### Sensibilité à la casse

```pascal
procedure TTestMultiPlateforme.TestSensibiliteCasse;  
var
  CheminMaj, CheminMin: String;
begin
  CheminMaj := FCheminTest + 'FICHIER.txt';
  CheminMin := FCheminTest + 'fichier.txt';

  // Créer un fichier en majuscules
  FGestionnaire.EcrireFichier(CheminMaj, 'Test majuscules');

  {$IFDEF WINDOWS}
  // Sur Windows, les deux chemins pointent vers le même fichier
  AssertTrue('Fichier MAJ existe', FileExists(CheminMaj));
  AssertTrue('Fichier min existe aussi', FileExists(CheminMin));
  AssertEquals('Même fichier',
    FGestionnaire.LireFichier(CheminMaj),
    FGestionnaire.LireFichier(CheminMin));
  {$ENDIF}

  {$IFDEF UNIX}
  // Sur Unix, ce sont deux fichiers différents
  AssertTrue('Fichier MAJ existe', FileExists(CheminMaj));
  AssertFalse('Fichier min n''existe pas', FileExists(CheminMin));

  // Créer aussi le fichier en minuscules
  FGestionnaire.EcrireFichier(CheminMin, 'Test minuscules');
  AssertTrue('Les deux existent', FileExists(CheminMaj) and FileExists(CheminMin));
  AssertNotEquals('Fichiers différents',
    FGestionnaire.LireFichier(CheminMaj),
    FGestionnaire.LireFichier(CheminMin));
  {$ENDIF}
end;
```

### Bibliothèques système

```pascal
unit TestIntegrationBibliotheques;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  GestionnaireBibliotheques;

type
  TTestIntegrationBibliotheques = class(TTestCase)
  private
    FGestionnaire: TGestionnaireBibliotheques;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestChargerBibliotheque;
    procedure TestAppelerFonction;
    procedure TestBibliothequeInexistante;
  end;

implementation

procedure TTestIntegrationBibliotheques.SetUp;  
begin
  inherited;
  FGestionnaire := TGestionnaireBibliotheques.Create;
end;

procedure TTestIntegrationBibliotheques.TearDown;  
begin
  FGestionnaire.Free;
  inherited;
end;

procedure TTestIntegrationBibliotheques.TestChargerBibliotheque;  
var
  CheminBiblio: String;
  Handle: TLibHandle;
begin
  {$IFDEF WINDOWS}
  // Charger kernel32.dll sur Windows
  CheminBiblio := 'kernel32.dll';
  {$ENDIF}

  {$IFDEF UNIX}
  // Charger libc.so sur Linux
  CheminBiblio := 'libc.so.6';
  {$ENDIF}

  Handle := FGestionnaire.Charger(CheminBiblio);
  try
    AssertTrue('Bibliothèque chargée', Handle <> 0);
  finally
    FGestionnaire.Decharger(Handle);
  end;
end;

procedure TTestIntegrationBibliotheques.TestAppelerFonction;  
var
  Handle: TLibHandle;
  Fonction: Pointer;
  Resultat: Integer;
begin
  {$IFDEF WINDOWS}
  Handle := FGestionnaire.Charger('kernel32.dll');
  try
    // Obtenir l'adresse de GetCurrentProcessId
    Fonction := FGestionnaire.ObtenirFonction(Handle, 'GetCurrentProcessId');
    AssertNotNull('Fonction trouvée', Fonction);

    // Appeler la fonction (retourne le PID du processus)
    Resultat := TGetCurrentProcessId(Fonction)();
    AssertTrue('PID valide', Resultat > 0);
    WriteLn('PID Windows : ', Resultat);
  finally
    FGestionnaire.Decharger(Handle);
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  Handle := FGestionnaire.Charger('libc.so.6');
  try
    // Obtenir l'adresse de getpid
    Fonction := FGestionnaire.ObtenirFonction(Handle, 'getpid');
    AssertNotNull('Fonction trouvée', Fonction);

    // Appeler la fonction
    Resultat := TGetPid(Fonction)();
    AssertTrue('PID valide', Resultat > 0);
    WriteLn('PID Linux : ', Resultat);
  finally
    FGestionnaire.Decharger(Handle);
  end;
  {$ENDIF}
end;

procedure TTestIntegrationBibliotheques.TestBibliothequeInexistante;  
begin
  try
    FGestionnaire.Charger('bibliotheque_inexistante.xyz');
    Fail('Devrait lever une exception');
  except
    on E: Exception do
      AssertTrue('Erreur de chargement', Length(E.Message) > 0);
  end;
end;

initialization
  RegisterTest(TTestIntegrationBibliotheques);

end.
```

## Performances des tests d'intégration

### Mesurer le temps d'exécution

```pascal
unit TestPerformanceIntegration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils,
  TestBaseIntegrationDB, GestionnaireUtilisateurs;

type
  TTestPerformanceIntegration = class(TTestBaseIntegrationDB)
  private
    FGestionnaire: TGestionnaireUtilisateurs;
    FTempsDebut: TDateTime;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure DemarrerChrono;
    function ArreterChrono: Int64;  // Retourne les millisecondes
  published
    procedure TestPerformanceInsertionMasse;
    procedure TestPerformanceRequeteComplexe;
    procedure TestPerformanceTransaction;
  end;

implementation

procedure TTestPerformanceIntegration.SetUp;  
begin
  inherited;
  FGestionnaire := TGestionnaireUtilisateurs.Create(Connexion, Transaction);
end;

procedure TTestPerformanceIntegration.TearDown;  
begin
  FGestionnaire.Free;
  inherited;
end;

procedure TTestPerformanceIntegration.DemarrerChrono;  
begin
  FTempsDebut := Now;
end;

function TTestPerformanceIntegration.ArreterChrono: Int64;  
begin
  Result := MilliSecondsBetween(Now, FTempsDebut);
  WriteLn(Format('Temps d''exécution : %d ms', [Result]));
end;

procedure TTestPerformanceIntegration.TestPerformanceInsertionMasse;  
var
  i: Integer;
  Utilisateur: TUtilisateur;
  TempsPasse: Int64;
const
  NB_UTILISATEURS = 1000;
begin
  DemarrerChrono;

  // Insérer 1000 utilisateurs
  for i := 1 to NB_UTILISATEURS do
  begin
    Utilisateur := TUtilisateur.Create;
    try
      Utilisateur.Email := Format('user%d@test.com', [i]);
      Utilisateur.Nom := Format('Utilisateur %d', [i]);
      FGestionnaire.Creer(Utilisateur);
    finally
      Utilisateur.Free;
    end;
  end;

  Transaction.Commit;
  TempsPasse := ArreterChrono;

  // Vérifier le nombre d'utilisateurs
  AssertEquals('1000 utilisateurs créés', NB_UTILISATEURS,
    CompterLignes('utilisateurs'));

  // Performance attendue : moins de 10 secondes pour 1000 insertions
  AssertTrue('Performance acceptable', TempsPasse < 10000);

  WriteLn(Format('%.2f insertions/seconde', [NB_UTILISATEURS / (TempsPasse / 1000)]));
end;

procedure TTestPerformanceIntegration.TestPerformanceRequeteComplexe;  
var
  i: Integer;
  Utilisateur: TUtilisateur;
  TempsPasse: Int64;
  Resultats: TList;
begin
  // Créer des données de test
  for i := 1 to 100 do
  begin
    Utilisateur := TUtilisateur.Create;
    try
      Utilisateur.Email := Format('perf%d@test.com', [i]);
      Utilisateur.Nom := 'Performance';
      Utilisateur.DateInscription := Now - i;  // Dates différentes
      FGestionnaire.Creer(Utilisateur);
    finally
      Utilisateur.Free;
    end;
  end;
  Transaction.Commit;

  // Tester une requête complexe
  DemarrerChrono;

  Resultats := FGestionnaire.RechercherAvecFiltres(
    'Performance',           // Nom
    Now - 50,               // Date début
    Now,                    // Date fin
    True                    // Actifs seulement
  );

  try
    TempsPasse := ArreterChrono;

    AssertTrue('Résultats trouvés', Resultats.Count > 0);
    AssertTrue('Performance de recherche', TempsPasse < 1000);  // Moins d'1 seconde
  finally
    Resultats.Free;
  end;
end;

procedure TTestPerformanceIntegration.TestPerformanceTransaction;  
var
  i: Integer;
  Utilisateur: TUtilisateur;
  TempsPasse: Int64;
begin
  DemarrerChrono;

  // Effectuer 100 transactions
  for i := 1 to 100 do
  begin
    Utilisateur := TUtilisateur.Create;
    try
      Utilisateur.Email := Format('trans%d@test.com', [i]);
      Utilisateur.Nom := 'Transaction';
      FGestionnaire.Creer(Utilisateur);
      Transaction.Commit;  // Commit après chaque insertion
    finally
      Utilisateur.Free;
    end;
  end;

  TempsPasse := ArreterChrono;

  // Vérifier
  AssertEquals('100 utilisateurs', 100, CompterLignes('utilisateurs'));

  WriteLn(Format('%.2f transactions/seconde', [100 / (TempsPasse / 1000)]));
end;

initialization
  RegisterTest(TTestPerformanceIntegration);

end.
```

### Optimiser les tests lents

**Problème** : Les tests d'intégration peuvent être lents (plusieurs secondes chacun).

**Solutions :**

1. **Tests en parallèle** (avec précaution)
2. **Réutiliser les connexions** plutôt que les recréer
3. **Transactions groupées** au lieu de commits individuels
4. **Base de données en mémoire** (SQLite)
5. **Mocks pour les services externes lents**

```pascal
// Utiliser SQLite en mémoire pour tests rapides
procedure TTestRapideIntegration.SetUp;  
begin
  inherited;

  FConnexion := TSQLite3Connection.Create(nil);
  FConnexion.DatabaseName := ':memory:';  // Base en mémoire !
  FConnexion.Open;

  // Le reste de la configuration...
  InitialiserSchema;  // Créer les tables
end;
```

## Organisation et exécution

### Structure de projet complète

```
MonProjet/
├── src/
│   ├── models/
│   ├── business/
│   └── database/
├── tests/
│   ├── unit/
│   │   ├── TestUtilisateur.pas
│   │   └── TestCalculatrice.pas
│   ├── integration/
│   │   ├── TestIntegrationDB.pas
│   │   ├── TestIntegrationAPI.pas
│   │   └── TestIntegrationFichiers.pas
│   └── helpers/
│       ├── TestBaseIntegrationDB.pas
│       └── ConfigurationTests.pas
├── config/
│   ├── test.ini
│   ├── test_windows.ini
│   └── test_ubuntu.ini
└── scripts/
    ├── run_all_tests.bat          # Windows
    ├── run_all_tests.sh           # Ubuntu
    ├── run_integration_tests.bat
    └── run_integration_tests.sh
```

### Scripts d'exécution

**Windows (run_integration_tests.bat) :**

```batch
@echo off
echo ========================================  
echo Tests d'integration - Windows  
echo ========================================

REM Définir les variables d'environnement  
set DB_TEST_CONNECTION=localhost:3306:test_db:root:

REM Compiler  
echo Compilation...  
lazbuild MonProjetTests.lpi

REM Exécuter seulement les tests d'intégration  
echo.  
echo Execution des tests d'integration...  
MonProjetTests.exe --suite=TTestIntegration --format=plain

REM Vérifier le code de retour  
if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ECHEC : Des tests ont echoue
    exit /b 1
) else (
    echo.
    echo SUCCES : Tous les tests ont reussi
    exit /b 0
)
```

**Ubuntu (run_integration_tests.sh) :**

```bash
#!/bin/bash

echo "========================================"  
echo "Tests d'intégration - Ubuntu"  
echo "========================================"

# Définir les variables d'environnement
export DB_TEST_CONNECTION="localhost:5432:test_db:postgres:postgres"

# Compiler
echo "Compilation..."  
lazbuild MonProjetTests.lpi

# Exécuter seulement les tests d'intégration
echo ""  
echo "Exécution des tests d'intégration..."
./MonProjetTests --suite=TTestIntegration --format=plain

# Vérifier le code de retour
if [ $? -ne 0 ]; then
    echo ""
    echo "ÉCHEC : Des tests ont échoué"
    exit 1
else
    echo ""
    echo "SUCCÈS : Tous les tests ont réussi"
    exit 0
fi
```

### Exécution sélective

```bash
# Exécuter tous les tests
./MonProjetTests

# Exécuter uniquement les tests d'intégration DB
./MonProjetTests --suite=TTestIntegrationDB

# Exécuter un test spécifique
./MonProjetTests --suite=TTestIntegrationDB.TestCreerUtilisateur

# Format de sortie XML pour CI/CD
./MonProjetTests --format=xml --file=resultats_integration.xml
```

## Intégration CI/CD multi-plateforme

### GitHub Actions

```yaml
# .github/workflows/integration-tests.yml
name: Tests d'intégration

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  test-windows:
    name: Tests Windows
    runs-on: windows-latest

    services:
      mysql:
        image: mysql:8.0
        env:
          MYSQL_ROOT_PASSWORD: root
          MYSQL_DATABASE: test_db
        ports:
          - 3306:3306
        options: >-
          --health-cmd="mysqladmin ping"
          --health-interval=10s
          --health-timeout=5s
          --health-retries=3

    steps:
      - uses: actions/checkout@v3

      - name: Installer FreePascal
        run: choco install freepascal lazarus -y

      - name: Configurer les variables d'environnement
        run: |
          echo "DB_TEST_CONNECTION=localhost:3306:test_db:root:root" >> $GITHUB_ENV

      - name: Compiler les tests
        run: lazbuild MonProjetTests.lpi

      - name: Exécuter tests unitaires
        run: .\MonProjetTests.exe --suite=TTestUnit --format=plain

      - name: Exécuter tests d'intégration
        run: .\MonProjetTests.exe --suite=TTestIntegration --format=xml --file=resultats_windows.xml

      - name: Publier les résultats
        uses: EnricoMi/publish-unit-test-result-action/composite@v2
        if: always()
        with:
          files: resultats_windows.xml

  test-ubuntu:
    name: Tests Ubuntu
    runs-on: ubuntu-latest

    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_PASSWORD: postgres
          POSTGRES_DB: test_db
        ports:
          - 5432:5432
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

    steps:
      - uses: actions/checkout@v3

      - name: Installer FreePascal et Lazarus
        run: |
          sudo apt-get update
          sudo apt-get install -y fp-compiler lazarus lcl

      - name: Configurer les variables d'environnement
        run: |
          echo "DB_TEST_CONNECTION=localhost:5432:test_db:postgres:postgres" >> $GITHUB_ENV

      - name: Compiler les tests
        run: lazbuild MonProjetTests.lpi

      - name: Exécuter tests unitaires
        run: ./MonProjetTests --suite=TTestUnit --format=plain

      - name: Exécuter tests d'intégration
        run: ./MonProjetTests --suite=TTestIntegration --format=xml --file=resultats_ubuntu.xml

      - name: Publier les résultats
        uses: EnricoMi/publish-unit-test-result-action/composite@v2
        if: always()
        with:
          files: resultats_ubuntu.xml
```

### GitLab CI

```yaml
# .gitlab-ci.yml
stages:
  - build
  - test

variables:
  GIT_STRATEGY: clone

# Template pour les tests
.test_template: &test_template
  stage: test
  script:
    - lazbuild MonProjetTests.lpi
    - ./MonProjetTests --format=junit --file=resultats.xml
  artifacts:
    when: always
    reports:
      junit: resultats.xml
    paths:
      - resultats.xml
    expire_in: 1 week

test:windows:
  <<: *test_template
  tags:
    - windows
  variables:
    DB_TEST_CONNECTION: "localhost:3306:test_db:root:root"
  before_script:
    - choco install freepascal lazarus -y
  services:
    - name: mysql:8.0
      alias: mysql

test:ubuntu:
  <<: *test_template
  tags:
    - ubuntu
  variables:
    DB_TEST_CONNECTION: "localhost:5432:test_db:postgres:postgres"
  before_script:
    - apt-get update -qq
    - apt-get install -y fp-compiler lazarus lcl
  services:
    - name: postgres:15
      alias: postgres
```

## Bonnes pratiques des tests d'intégration

### 1. Tests indépendants et reproductibles

✅ **Bon :**
```pascal
procedure TestCreerUtilisateur;  
begin
  // Toujours partir d'un état propre
  NettoierTables;

  // Créer les données nécessaires
  Utilisateur := CreerUtilisateurTest;

  // Effectuer le test
  // ...

  // Nettoyer automatiquement dans TearDown
end;
```

❌ **Mauvais :**
```pascal
procedure TestCreerUtilisateur;  
begin
  // Suppose qu'il y a déjà des données
  Utilisateur := ChargerUtilisateur(1);  // Et si l'ID 1 n'existe pas ?
  // ...
end;
```

### 2. Messages d'erreur descriptifs

✅ **Bon :**
```pascal
AssertEquals('L''utilisateur créé devrait avoir l''email correct',
  'test@example.com', Utilisateur.Email);
```

❌ **Mauvais :**
```pascal
AssertEquals('test@example.com', Utilisateur.Email);
```

### 3. Tester les cas d'erreur

```pascal
procedure TestCreationAvecEmailInvalide;  
begin
  Utilisateur := TUtilisateur.Create;
  try
    Utilisateur.Email := 'email_invalide';  // Pas de @

    try
      FGestionnaire.Creer(Utilisateur);
      Fail('Devrait lever EEmailInvalide');
    except
      on E: EEmailInvalide do
        ; // Comportement attendu
    end;
  finally
    Utilisateur.Free;
  end;
end;
```

### 4. Éviter les tests fragiles

✅ **Bon :**
```pascal
// Compter les résultats
AssertTrue('Au moins 1 résultat', Resultats.Count > 0);
```

❌ **Mauvais :**
```pascal
// Dépend de l'état exact de la base
AssertEquals(42, Resultats.Count);  // Très fragile !
```

### 5. Isoler les dépendances externes

```pascal
type
  TTestAvecServiceExterne = class(TTestCase)
  private
    FServiceEmail: TServiceEmail;
  protected
    procedure SetUp; override;
  published
    procedure TestEnvoiEmail;
  end;

procedure TTestAvecServiceExterne.SetUp;  
begin
  inherited;
  FServiceEmail := TServiceEmail.Create;
  FServiceEmail.ModeTest := True;  // Ne pas envoyer réellement
end;

procedure TTestAvecServiceExterne.TestEnvoiEmail;  
begin
  // Test sans dépendre d'un serveur SMTP réel
  FServiceEmail.EnvoyerEmail('test@example.com', 'Sujet', 'Corps');

  AssertEquals('1 email envoyé', 1, FServiceEmail.EmailsEnvoyes.Count);
  AssertTrue('Destinataire correct',
    Pos('test@example.com', FServiceEmail.EmailsEnvoyes[0]) > 0);
end;
```

### 6. Documentation des tests

```pascal
{
  Test la création complète d'une commande avec les étapes :
  1. Création d'un utilisateur
  2. Ajout de produits au panier
  3. Validation du panier
  4. Traitement du paiement
  5. Vérification du stock

  Cas testé : scénario nominal avec paiement réussi
  Prérequis : base de données vide
  Postcondition : 1 utilisateur, 2 produits, 1 commande
}
procedure TTestIntegrationCommande.TestScenarioCompletCommande;  
begin
  // ...
end;
```

## Résumé et conclusion

### Points clés à retenir

✅ **Les tests d'intégration vérifient que les composants fonctionnent ensemble**
- Complémentaires aux tests unitaires
- Plus lents mais essentiels
- Détectent les problèmes d'assemblage

✅ **Configuration et nettoyage rigoureux**
- SetUp/TearDown systématiques
- Transactions avec rollback
- État initial prévisible

✅ **Multi-plateforme nécessite attention**
- Chemins, permissions, fins de ligne
- Tests spécifiques par OS quand nécessaire
- Variables d'environnement et configuration

✅ **Automatisation avec CI/CD**
- Tests sur Windows ET Ubuntu
- Bases de données de test en conteneurs
- Rapports de tests automatiques

✅ **Performance importante**
- Utiliser SQLite en mémoire si possible
- Regrouper les opérations
- Mesurer et optimiser les tests lents

### Quand utiliser les tests d'intégration ?

**Toujours :**
- Interactions avec base de données
- Appels à des API externes
- Opérations sur le système de fichiers
- Scénarios métier complets

**Parfois :**
- Composants fortement couplés
- Logique complexe multi-modules

**Jamais :**
- Code isolé (tests unitaires suffisent)
- Interface utilisateur (tests E2E ou manuels)

### Progression recommandée

1. **Semaine 1** : Créer l'infrastructure de tests (classe de base, configuration)
2. **Semaine 2** : Premiers tests d'intégration DB simples
3. **Semaine 3** : Tests avec fichiers et API
4. **Semaine 4** : Scénarios complets multi-composants
5. **Mois 2** : Automatisation CI/CD sur les deux OS
6. **Continue** : Ajouter des tests pour chaque nouveau module

### Ressources complémentaires

- **Documentation FPCUnit** : https://wiki.freepascal.org/fpcunit
- **SQLdb documentation** : https://wiki.freepascal.org/SQLdb_Tutorial
- **GitHub Actions** : https://docs.github.com/en/actions
- **Docker pour tests** : https://www.docker.com/

Les tests d'intégration sont essentiels pour garantir que votre application FreePascal/Lazarus fonctionne correctement sur Windows et Ubuntu. Investissez du temps dans leur création et leur maintenance, vous serez récompensé par moins de bugs en production et plus de confiance dans vos livraisons ! 🚀

⏭️ [Mocking et injection de dépendances](/18-tests-qualite-code/03-mocking-injection-dependances.md)
