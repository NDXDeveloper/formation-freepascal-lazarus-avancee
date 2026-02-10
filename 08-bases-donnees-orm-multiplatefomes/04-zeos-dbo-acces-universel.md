🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.4 ZEOS DBO pour accès universel

## Introduction

ZEOS (Zeos Database Objects) est une bibliothèque de composants open source pour l'accès aux bases de données dans FreePascal et Lazarus. Elle offre une interface unifiée et cohérente pour travailler avec de nombreux systèmes de gestion de bases de données (SGBD), tout en étant véritablement multi-plateforme.

### Pourquoi choisir ZEOS ?

**Avantages principaux :**
- **Interface unique** pour plusieurs SGBD (MySQL, PostgreSQL, SQLite, Oracle, etc.)
- **Haute performance** avec optimisations natives
- **Multi-plateforme** : fonctionne identiquement sur Windows, Linux, macOS
- **Open source** et activement maintenu
- **Riche en fonctionnalités** : transactions, prepared statements, blobs, etc.
- **Compatible** avec les composants standard Lazarus

**Comparaison avec SQLdb (natif Lazarus) :**

| Aspect | ZEOS | SQLdb |
|--------|------|-------|
| Bases supportées | 15+ | 8 |
| Performance | Excellente | Bonne |
| API unifiée | Oui | Oui |
| Installation | Package externe | Intégré |
| Fonctionnalités avancées | Très complètes | Standards |
| Communauté | Large | Officielle FPC |

## Installation de ZEOS

### Sous Windows

#### Méthode 1 : Via Online Package Manager (OPM)

1. Dans Lazarus, ouvrir **Paquets → Online Package Manager**
2. Chercher "**zeosdbo**" dans la liste
3. Sélectionner le paquet et cliquer sur **Install**
4. Redémarrer Lazarus quand demandé

#### Méthode 2 : Installation manuelle

1. Télécharger ZEOS depuis https://sourceforge.net/projects/zeoslib/
2. Extraire l'archive (par exemple dans `C:\Developpement\zeos\`)
3. Dans Lazarus : **Paquets → Ouvrir un fichier de paquet (.lpk)**
4. Naviguer vers : `zeos\packages\lazarus\zcomponent.lpk`
5. Cliquer sur **Compiler** puis **Utiliser → Installer**
6. Redémarrer Lazarus

### Sous Ubuntu/Linux

#### Méthode 1 : Via Online Package Manager

Identique à la méthode Windows :
1. **Paquets → Online Package Manager**
2. Rechercher "**zeosdbo**"
3. Installer et redémarrer

#### Méthode 2 : Depuis les sources

```bash
# Télécharger les sources
cd ~/Downloads
wget https://sourceforge.net/projects/zeoslib/files/latest/download -O zeos.zip
unzip zeos.zip

# Déplacer dans un répertoire approprié
sudo mv zeos-* /usr/local/share/zeos

# Dans Lazarus, ouvrir le paquet
# Paquets → Ouvrir un fichier de paquet
# /usr/local/share/zeos/packages/lazarus/zcomponent.lpk
```

### Vérification de l'installation

Après redémarrage de Lazarus, vous devriez voir un nouvel onglet **"Zeos Access"** dans la palette de composants, contenant :
- `TZConnection`
- `TZQuery`
- `TZTable`
- `TZReadOnlyQuery`
- Et d'autres composants...

## Architecture de ZEOS

### Structure en couches

```
┌─────────────────────────────────────┐
│   Application Lazarus/FPC           │
├─────────────────────────────────────┤
│   Composants ZEOS (TZConnection,    │
│   TZQuery, TZTable...)              │
├─────────────────────────────────────┤
│   Couche d'abstraction ZEOS         │
│   (Plain Drivers)                   │
├─────────────────────────────────────┤
│   Bibliothèques clientes natives    │
│   (.dll sur Windows / .so sur Linux)│
├─────────────────────────────────────┤
│   Serveur de base de données        │
└─────────────────────────────────────┘
```

### Composants principaux

#### TZConnection
Le composant de connexion central - équivalent de `TSQLConnection` en SQLdb.

**Propriétés importantes :**
- `Protocol` : Type de base de données (mysql-5, postgresql-9, sqlite-3, etc.)
- `HostName` : Serveur de la base de données
- `Port` : Port de connexion
- `Database` : Nom de la base de données
- `User` : Nom d'utilisateur
- `Password` : Mot de passe
- `LibraryLocation` : Chemin vers la bibliothèque cliente
- `Connected` : État de connexion

#### TZQuery
Composant pour exécuter des requêtes SQL - équivalent de `TSQLQuery`.

**Propriétés importantes :**
- `Connection` : Lien vers TZConnection
- `SQL` : Requête SQL à exécuter
- `Active` : Ouvre/ferme le dataset
- `Params` : Paramètres de la requête

#### TZTable
Accès direct à une table - équivalent de `TSQLTable`.

#### TZReadOnlyQuery
Version optimisée de TZQuery pour les lectures seules.

## Configuration de base par SGBD

### PostgreSQL

#### Configuration Windows

```pascal
procedure ConfigurerPostgreSQL_Windows;
begin
  ZConnection1.Protocol := 'postgresql-9';  // ou 'postgresql' pour auto-detect
  ZConnection1.HostName := 'localhost';
  ZConnection1.Port := 5432;
  ZConnection1.Database := 'ma_base';
  ZConnection1.User := 'postgres';
  ZConnection1.Password := 'mon_mot_de_passe';

  // Spécifier le chemin de la DLL
  ZConnection1.LibraryLocation :=
    ExtractFilePath(Application.ExeName) + 'libs\libpq.dll';

  // Options supplémentaires
  ZConnection1.Properties.Add('codepage=UTF8');

  ZConnection1.Connected := True;
end;
```

#### Configuration Linux

```pascal
procedure ConfigurerPostgreSQL_Linux;
begin
  ZConnection1.Protocol := 'postgresql-9';
  ZConnection1.HostName := 'localhost';
  ZConnection1.Port := 5432;
  ZConnection1.Database := 'ma_base';
  ZConnection1.User := 'postgres';
  ZConnection1.Password := 'mon_mot_de_passe';

  // Sous Linux, souvent inutile de spécifier - détection automatique
  // Si nécessaire :
  // ZConnection1.LibraryLocation := 'libpq.so.5';

  ZConnection1.Properties.Add('codepage=UTF8');

  ZConnection1.Connected := True;
end;
```

#### Configuration multi-plateforme universelle

```pascal
procedure ConfigurerPostgreSQL;
begin
  ZConnection1.Protocol := 'postgresql-9';
  ZConnection1.HostName := 'localhost';
  ZConnection1.Port := 5432;
  ZConnection1.Database := 'ma_base';
  ZConnection1.User := 'postgres';
  ZConnection1.Password := 'mon_mot_de_passe';

  {$IFDEF WINDOWS}
  ZConnection1.LibraryLocation :=
    ExtractFilePath(Application.ExeName) + 'libs\libpq.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  // Laisser ZEOS détecter automatiquement
  // ou spécifier : ZConnection1.LibraryLocation := 'libpq.so.5';
  {$ENDIF}

  ZConnection1.Properties.Add('codepage=UTF8');
  ZConnection1.Connected := True;
end;
```

### MySQL / MariaDB

```pascal
procedure ConfigurerMySQL;
begin
  // Protocoles disponibles : 'mysql-5', 'mysql-8', 'mariadb-10'
  ZConnection1.Protocol := 'mysql-8';
  ZConnection1.HostName := 'localhost';
  ZConnection1.Port := 3306;
  ZConnection1.Database := 'ma_base';
  ZConnection1.User := 'root';
  ZConnection1.Password := 'mon_mot_de_passe';

  {$IFDEF WINDOWS}
  ZConnection1.LibraryLocation :=
    ExtractFilePath(Application.ExeName) + 'libs\libmysql.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  // Détection automatique ou :
  // ZConnection1.LibraryLocation := 'libmysqlclient.so.21';
  {$ENDIF}

  // Options MySQL
  ZConnection1.Properties.Add('compress=true');
  ZConnection1.Properties.Add('CLIENT_MULTI_STATEMENTS=1');

  ZConnection1.Connected := True;
end;
```

### SQLite (base de données embarquée)

SQLite est particulièrement intéressant car il ne nécessite pas de serveur.

```pascal
procedure ConfigurerSQLite;
var
  CheminBD: string;
begin
  ZConnection1.Protocol := 'sqlite-3';

  // Chemin de la base de données
  CheminBD := ExtractFilePath(Application.ExeName) +
              'data' + PathDelim + 'ma_base.db';
  ZConnection1.Database := CheminBD;

  // Pas d'utilisateur/mot de passe pour SQLite
  ZConnection1.User := '';
  ZConnection1.Password := '';

  {$IFDEF WINDOWS}
  ZConnection1.LibraryLocation :=
    ExtractFilePath(Application.ExeName) + 'libs\sqlite3.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  // Souvent déjà installé sur le système
  // ZConnection1.LibraryLocation := 'libsqlite3.so.0';
  {$ENDIF}

  // Options SQLite
  ZConnection1.Properties.Add('busytimeout=5000');  // Timeout en ms

  ZConnection1.Connected := True;
end;
```

### Firebird

```pascal
procedure ConfigurerFirebird;
begin
  ZConnection1.Protocol := 'firebird-3.0';  // ou 'firebird-4.0'
  ZConnection1.HostName := 'localhost';
  ZConnection1.Port := 3050;
  ZConnection1.Database := 'C:\Bases\ma_base.fdb';  // Windows
  // ZConnection1.Database := '/var/lib/firebird/ma_base.fdb';  // Linux
  ZConnection1.User := 'SYSDBA';
  ZConnection1.Password := 'masterkey';

  {$IFDEF WINDOWS}
  ZConnection1.LibraryLocation :=
    ExtractFilePath(Application.ExeName) + 'libs\fbclient.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  ZConnection1.LibraryLocation := 'libfbclient.so.2';
  {$ENDIF}

  ZConnection1.Properties.Add('charset=UTF8');

  ZConnection1.Connected := True;
end;
```

### Oracle (pour utilisateurs avancés)

```pascal
procedure ConfigurerOracle;
begin
  ZConnection1.Protocol := 'oracle';
  ZConnection1.HostName := 'localhost';
  ZConnection1.Port := 1521;
  ZConnection1.Database := 'XE';  // ou un TNS name
  ZConnection1.User := 'system';
  ZConnection1.Password := 'oracle';

  {$IFDEF WINDOWS}
  // Oracle Instant Client
  ZConnection1.LibraryLocation :=
    'C:\oracle\instantclient_21_3\oci.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  ZConnection1.LibraryLocation :=
    '/usr/lib/oracle/21/client64/lib/libclntsh.so';
  {$ENDIF}

  ZConnection1.Connected := True;
end;
```

## Utilisation des requêtes avec TZQuery

### Requêtes SELECT simples

```pascal
procedure LireClients;
begin
  ZQuery1.Connection := ZConnection1;
  ZQuery1.SQL.Text := 'SELECT * FROM clients ORDER BY nom';
  ZQuery1.Open;

  // Parcourir les résultats
  while not ZQuery1.EOF do
  begin
    WriteLn('Client: ', ZQuery1.FieldByName('nom').AsString);
    WriteLn('Email: ', ZQuery1.FieldByName('email').AsString);
    ZQuery1.Next;
  end;

  ZQuery1.Close;
end;
```

### Requêtes paramétrées (Prepared Statements)

Les requêtes paramétrées sont essentielles pour la sécurité (protection contre les injections SQL) et les performances.

```pascal
procedure RechercherClientParVille(const Ville: string);
begin
  ZQuery1.SQL.Text := 'SELECT * FROM clients WHERE ville = :ville';
  ZQuery1.ParamByName('ville').AsString := Ville;
  ZQuery1.Open;

  ShowMessage('Trouvé ' + IntToStr(ZQuery1.RecordCount) + ' clients');

  ZQuery1.Close;
end;
```

**Exemple avec plusieurs paramètres :**

```pascal
procedure RechercherClientsAvances(const VilleMin, DateMin: string;
  MontantMin: Currency);
begin
  ZQuery1.SQL.Text :=
    'SELECT * FROM clients ' +
    'WHERE ville = :ville ' +
    'AND date_inscription >= :date_min ' +
    'AND total_achats >= :montant_min ' +
    'ORDER BY nom';

  ZQuery1.ParamByName('ville').AsString := VilleMin;
  ZQuery1.ParamByName('date_min').AsString := DateMin;
  ZQuery1.ParamByName('montant_min').AsCurrency := MontantMin;

  ZQuery1.Open;
end;
```

### Requêtes INSERT

```pascal
procedure AjouterClient(const Nom, Email, Ville: string);
begin
  ZQuery1.SQL.Text :=
    'INSERT INTO clients (nom, email, ville, date_inscription) ' +
    'VALUES (:nom, :email, :ville, CURRENT_DATE)';

  ZQuery1.ParamByName('nom').AsString := Nom;
  ZQuery1.ParamByName('email').AsString := Email;
  ZQuery1.ParamByName('ville').AsString := Ville;

  ZQuery1.ExecSQL;  // Pour INSERT, UPDATE, DELETE

  ShowMessage('Client ajouté avec succès');
end;
```

### Requêtes UPDATE

```pascal
procedure ModifierEmailClient(const ClientID: Integer; const NouvelEmail: string);
begin
  ZQuery1.SQL.Text :=
    'UPDATE clients SET email = :email WHERE id = :id';

  ZQuery1.ParamByName('id').AsInteger := ClientID;
  ZQuery1.ParamByName('email').AsString := NouvelEmail;

  ZQuery1.ExecSQL;

  ShowMessage(Format('%d ligne(s) modifiée(s)', [ZQuery1.RowsAffected]));
end;
```

### Requêtes DELETE

```pascal
procedure SupprimerClient(const ClientID: Integer);
begin
  if MessageDlg('Confirmation',
     'Voulez-vous vraiment supprimer ce client ?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ZQuery1.SQL.Text := 'DELETE FROM clients WHERE id = :id';
    ZQuery1.ParamByName('id').AsInteger := ClientID;
    ZQuery1.ExecSQL;

    ShowMessage('Client supprimé');
  end;
end;
```

## Gestion des transactions

Les transactions garantissent l'intégrité des données lors d'opérations multiples.

### Transaction simple

```pascal
procedure TransfertArgent(const CompteSource, CompteDest: Integer;
  Montant: Currency);
begin
  ZConnection1.StartTransaction;
  try
    // Débiter le compte source
    ZQuery1.SQL.Text :=
      'UPDATE comptes SET solde = solde - :montant WHERE id = :id';
    ZQuery1.ParamByName('montant').AsCurrency := Montant;
    ZQuery1.ParamByName('id').AsInteger := CompteSource;
    ZQuery1.ExecSQL;

    // Créditer le compte destination
    ZQuery1.SQL.Text :=
      'UPDATE comptes SET solde = solde + :montant WHERE id = :id';
    ZQuery1.ParamByName('montant').AsCurrency := Montant;
    ZQuery1.ParamByName('id').AsInteger := CompteDest;
    ZQuery1.ExecSQL;

    // Tout s'est bien passé - valider
    ZConnection1.Commit;
    ShowMessage('Transfert réussi');
  except
    on E: Exception do
    begin
      // Erreur - annuler toutes les modifications
      ZConnection1.Rollback;
      ShowMessage('Erreur lors du transfert: ' + E.Message);
    end;
  end;
end;
```

### Transaction avec niveaux d'isolation

```pascal
procedure TransactionAvecIsolation;
begin
  // Définir le niveau d'isolation
  ZConnection1.TransactIsolationLevel := tiReadCommitted;
  // Autres options : tiReadUncommitted, tiRepeatableRead, tiSerializable

  ZConnection1.StartTransaction;
  try
    // ... opérations sur la base ...
    ZConnection1.Commit;
  except
    ZConnection1.Rollback;
    raise;
  end;
end;
```

## Fonctionnalités avancées

### Gestion des BLOBs (fichiers, images)

```pascal
procedure SauvegarderImage(const ClientID: Integer; const CheminImage: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(CheminImage, fmOpenRead);
  try
    ZQuery1.SQL.Text :=
      'UPDATE clients SET photo = :photo WHERE id = :id';
    ZQuery1.ParamByName('id').AsInteger := ClientID;

    // Charger l'image dans le paramètre BLOB
    TBlobField(ZQuery1.ParamByName('photo')).LoadFromStream(FileStream);

    ZQuery1.ExecSQL;
  finally
    FileStream.Free;
  end;
end;

procedure ChargerImage(const ClientID: Integer; const CheminDestination: string);
var
  FileStream: TFileStream;
begin
  ZQuery1.SQL.Text := 'SELECT photo FROM clients WHERE id = :id';
  ZQuery1.ParamByName('id').AsInteger := ClientID;
  ZQuery1.Open;

  if not ZQuery1.FieldByName('photo').IsNull then
  begin
    FileStream := TFileStream.Create(CheminDestination, fmCreate);
    try
      TBlobField(ZQuery1.FieldByName('photo')).SaveToStream(FileStream);
    finally
      FileStream.Free;
    end;
  end;

  ZQuery1.Close;
end;
```

### Procédures stockées

```pascal
procedure AppelerProcedureStockee;
begin
  ZQuery1.SQL.Text := 'CALL calculer_statistiques(:annee)';
  ZQuery1.ParamByName('annee').AsInteger := 2025;
  ZQuery1.ExecSQL;
end;

// Avec résultat
procedure AppelerFonction;
begin
  ZQuery1.SQL.Text := 'SELECT calculer_remise(:client_id) AS remise';
  ZQuery1.ParamByName('client_id').AsInteger := 123;
  ZQuery1.Open;

  ShowMessage('Remise: ' + ZQuery1.FieldByName('remise').AsString);

  ZQuery1.Close;
end;
```

### Métadonnées de la base

```pascal
procedure ListerTables;
var
  Tables: TStringList;
begin
  Tables := TStringList.Create;
  try
    ZConnection1.GetTableNames('', Tables);

    ShowMessage('Tables disponibles: ' + Tables.CommaText);
  finally
    Tables.Free;
  end;
end;

procedure ListerChamps(const TableName: string);
var
  Columns: TStringList;
  i: Integer;
begin
  Columns := TStringList.Create;
  try
    ZConnection1.GetColumnNames('', TableName, Columns);

    for i := 0 to Columns.Count - 1 do
      WriteLn('Colonne: ', Columns[i]);
  finally
    Columns.Free;
  end;
end;
```

## Gestion des erreurs

### Détection des erreurs de connexion

```pascal
function TenterConnexion: Boolean;
begin
  Result := False;
  try
    ZConnection1.Connected := True;
    Result := True;
    ShowMessage('Connexion réussie !');
  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion: ' + E.Message);

      // Analyser le type d'erreur
      if Pos('password', LowerCase(E.Message)) > 0 then
        ShowMessage('Vérifiez le mot de passe')
      else if Pos('host', LowerCase(E.Message)) > 0 then
        ShowMessage('Vérifiez le nom du serveur')
      else if Pos('database', LowerCase(E.Message)) > 0 then
        ShowMessage('La base de données n''existe pas');
    end;
  end;
end;
```

### Gestion des erreurs SQL

```pascal
procedure ExecuterRequeteSecurisee(const SQL: string);
begin
  try
    ZQuery1.SQL.Text := SQL;
    ZQuery1.ExecSQL;
    ShowMessage('Requête exécutée avec succès');
  except
    on E: EZSQLException do
    begin
      // Erreur spécifique ZEOS
      ShowMessage(Format('Erreur SQL [%d]: %s',
        [E.ErrorCode, E.Message]));
    end;
    on E: Exception do
    begin
      // Autre erreur
      ShowMessage('Erreur: ' + E.Message);
    end;
  end;
end;
```

## Optimisation et performance

### Utilisation de TZReadOnlyQuery

Pour les requêtes en lecture seule, utilisez `TZReadOnlyQuery` qui est optimisé :

```pascal
procedure LireGrosVolume;
begin
  // Plus rapide que TZQuery pour les lectures seules
  ZReadOnlyQuery1.SQL.Text := 'SELECT * FROM gros_tableau';
  ZReadOnlyQuery1.Open;

  while not ZReadOnlyQuery1.EOF do
  begin
    // Traitement...
    ZReadOnlyQuery1.Next;
  end;

  ZReadOnlyQuery1.Close;
end;
```

### Batch Insert (insertion en masse)

```pascal
procedure InsertionMassive;
var
  i: Integer;
begin
  ZConnection1.StartTransaction;
  try
    ZQuery1.SQL.Text :=
      'INSERT INTO logs (date, message) VALUES (:date, :message)';

    for i := 1 to 10000 do
    begin
      ZQuery1.ParamByName('date').AsDateTime := Now;
      ZQuery1.ParamByName('message').AsString := 'Log ' + IntToStr(i);
      ZQuery1.ExecSQL;
    end;

    ZConnection1.Commit;
  except
    ZConnection1.Rollback;
    raise;
  end;
end;
```

### Pool de connexions (concept avancé)

```pascal
unit PoolConnexions;

interface

uses
  Classes, ZConnection;

type
  TPoolConnexions = class
  private
    FConnexions: TList;
    FProtocole: string;
    FHote: string;
    FBase: string;
  public
    constructor Create(const Protocole, Hote, Base: string; NbConnexions: Integer);
    destructor Destroy; override;
    function ObtenirConnexion: TZConnection;
    procedure LibererConnexion(Connexion: TZConnection);
  end;

implementation

constructor TPoolConnexions.Create(const Protocole, Hote, Base: string;
  NbConnexions: Integer);
var
  i: Integer;
  Conn: TZConnection;
begin
  FProtocole := Protocole;
  FHote := Hote;
  FBase := Base;
  FConnexions := TList.Create;

  // Créer le pool de connexions
  for i := 1 to NbConnexions do
  begin
    Conn := TZConnection.Create(nil);
    Conn.Protocol := FProtocole;
    Conn.HostName := FHote;
    Conn.Database := FBase;
    // Configuration supplémentaire...
    FConnexions.Add(Conn);
  end;
end;

// ... Implémentation des autres méthodes ...

end.
```

## Migration depuis SQLdb vers ZEOS

Si vous avez déjà du code utilisant SQLdb, la migration est simple :

| SQLdb | ZEOS | Notes |
|-------|------|-------|
| `TSQLConnection` | `TZConnection` | Même concept |
| `TSQLQuery` | `TZQuery` | API quasi-identique |
| `TSQLTransaction` | Intégré dans `TZConnection` | Plus simple |
| `TSQLScript` | `TZSQLProcessor` | Exécution de scripts |

### Exemple de conversion

**Avant (SQLdb) :**
```pascal
SQLConnection1.DatabaseName := 'ma_base';
SQLTransaction1.Database := SQLConnection1;
SQLQuery1.Database := SQLConnection1;
SQLQuery1.Transaction := SQLTransaction1;
```

**Après (ZEOS) :**
```pascal
ZConnection1.Database := 'ma_base';
ZQuery1.Connection := ZConnection1;
// Pas besoin de TSQLTransaction séparé !
```

## Classe utilitaire pour simplifier l'utilisation

```pascal
unit GestionnaireZEOS;

interface

uses
  ZConnection, ZDataset, SysUtils, Classes;

type
  TGestionnaireZEOS = class
  private
    FConnection: TZConnection;
  public
    constructor Create(const Protocole, Hote, Base, User, Pass: string);
    destructor Destroy; override;

    function ExecuterRequete(const SQL: string): TZQuery;
    function ExecuterScalaire(const SQL: string): Variant;
    procedure ExecuterCommande(const SQL: string);

    property Connection: TZConnection read FConnection;
  end;

implementation

constructor TGestionnaireZEOS.Create(const Protocole, Hote, Base,
  User, Pass: string);
begin
  FConnection := TZConnection.Create(nil);
  FConnection.Protocol := Protocole;
  FConnection.HostName := Hote;
  FConnection.Database := Base;
  FConnection.User := User;
  FConnection.Password := Pass;
  FConnection.Connected := True;
end;

destructor TGestionnaireZEOS.Destroy;
begin
  FConnection.Free;
  inherited;
end;

function TGestionnaireZEOS.ExecuterRequete(const SQL: string): TZQuery;
begin
  Result := TZQuery.Create(nil);
  Result.Connection := FConnection;
  Result.SQL.Text := SQL;
  Result.Open;
end;

function TGestionnaireZEOS.ExecuterScalaire(const SQL: string): Variant;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := SQL;
    Query.Open;

    if not Query.EOF then
      Result := Query.Fields[0].Value
    else
      Result := Null;
  finally
    Query.Free;
  end;
end;

procedure TGestionnaireZEOS.ExecuterCommande(const SQL: string);
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := SQL;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

end.
```

**Utilisation simplifiée :**

```pascal
var
  DB: TGestionnaireZEOS;
  Query: TZQuery;
  NbClients: Integer;
begin
  DB := TGestionnaireZEOS.Create('postgresql-9', 'localhost',
    'ma_base', 'postgres', 'pass');
  try
    // Requête SELECT
    Query := DB.ExecuterRequete('SELECT * FROM clients');
    try
      // Traiter les résultats...
    finally
      Query.Free;
    end;

    // Valeur scalaire
    NbClients := DB.ExecuterScalaire('SELECT COUNT(*) FROM clients');

    // Commande
    DB.ExecuterCommande('UPDATE clients SET actif = 1');
  finally
    DB.Free;
  end;
end;
```

## Bonnes pratiques avec ZEOS

1. **Toujours utiliser des requêtes paramétrées** pour éviter les injections SQL
2. **Gérer les transactions** pour les opérations critiques
3. **Fermer les requêtes** après utilisation (`ZQuery.Close`)
4. **Libérer les ressources** (`ZQuery.Free`) quand elles ne sont plus nécessaires
5. **Tester sur les deux OS** (Windows et Linux) avant déploiement
6. **Documenter les protocoles** utilisés et leurs versions
7. **Prévoir la gestion des erreurs** dès le départ
8. **Utiliser TZReadOnlyQuery** pour les lectures seules volumineuses

## Ressources et documentation

- **Site officiel** : https://zeoslib.sourceforge.io/
- **Documentation** : https://zeoslib.sourceforge.io/portal.php
- **Forum** : https://sourceforge.net/p/zeoslib/discussion/
- **GitHub** : https://github.com/marsupilami79/zeoslib
- **Wiki** : https://sourceforge.net/p/zeoslib/wiki/

## Exemple complet d'application multi-plateforme

Voici un exemple d'application complète utilisant ZEOS de manière portable :

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DBGrids,
  ZConnection, ZDataset, DB;

type
  TFormPrincipale = class(TForm)
    ZConnection1: TZConnection;
    ZQuery1: TZQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    ButtonConnecter: TButton;
    ButtonCharger: TButton;
    ButtonAjouter: TButton;
    EditNom: TEdit;
    EditEmail: TEdit;
    LabelStatut: TLabel;

    procedure ButtonConnecterClick(Sender: TObject);
    procedure ButtonChargerClick(Sender: TObject);
    procedure ButtonAjouterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure ConfigurerConnexion;
    procedure AfficherStatut(const Message: string; Erreur: Boolean = False);
  end;

var
  FormPrincipale: TFormPrincipale;

implementation

{$R *.lfm}

procedure TFormPrincipale.FormCreate(Sender: TObject);
begin
  ConfigurerConnexion;

  // Lier le DataSource
  DataSource1.DataSet := ZQuery1;
  DBGrid1.DataSource := DataSource1;

  AfficherStatut('Application démarrée');
end;

procedure TFormPrincipale.ConfigurerConnexion;
begin
  // Configuration multi-plateforme
  ZConnection1.Protocol := 'postgresql-9';
  ZConnection1.HostName := 'localhost';
  ZConnection1.Port := 5432;
  ZConnection1.Database := 'test_db';
  ZConnection1.User := 'postgres';
  ZConnection1.Password := 'password';

  {$IFDEF WINDOWS}
  // Chemin Windows
  ZConnection1.LibraryLocation :=
    ExtractFilePath(Application.ExeName) + 'libs\libpq.dll';
  AfficherStatut('Configuration Windows');
  {$ENDIF}

  {$IFDEF LINUX}
  // Sous Linux, laisser la détection automatique
  // ou spécifier : ZConnection1.LibraryLocation := 'libpq.so.5';
  AfficherStatut('Configuration Linux');
  {$ENDIF}

  // Propriétés communes
  ZConnection1.Properties.Add('codepage=UTF8');
  ZConnection1.AutoCommit := True;
end;

procedure TFormPrincipale.ButtonConnecterClick(Sender: TObject);
begin
  try
    if not ZConnection1.Connected then
    begin
      ZConnection1.Connected := True;
      ButtonConnecter.Caption := 'Déconnecter';
      AfficherStatut('Connecté à la base de données');
      ButtonCharger.Enabled := True;
      ButtonAjouter.Enabled := True;
    end
    else
    begin
      ZConnection1.Connected := False;
      ButtonConnecter.Caption := 'Connecter';
      AfficherStatut('Déconnecté');
      ButtonCharger.Enabled := False;
      ButtonAjouter.Enabled := False;
    end;
  except
    on E: Exception do
    begin
      AfficherStatut('Erreur de connexion: ' + E.Message, True);
    end;
  end;
end;

procedure TFormPrincipale.ButtonChargerClick(Sender: TObject);
begin
  try
    ZQuery1.Close;
    ZQuery1.SQL.Text := 'SELECT * FROM clients ORDER BY nom';
    ZQuery1.Open;

    AfficherStatut(Format('%d enregistrements chargés',
      [ZQuery1.RecordCount]));
  except
    on E: Exception do
    begin
      AfficherStatut('Erreur lors du chargement: ' + E.Message, True);
    end;
  end;
end;

procedure TFormPrincipale.ButtonAjouterClick(Sender: TObject);
begin
  if (Trim(EditNom.Text) = '') or (Trim(EditEmail.Text) = '') then
  begin
    ShowMessage('Veuillez remplir tous les champs');
    Exit;
  end;

  try
    ZQuery1.Close;
    ZQuery1.SQL.Text :=
      'INSERT INTO clients (nom, email) VALUES (:nom, :email)';
    ZQuery1.ParamByName('nom').AsString := EditNom.Text;
    ZQuery1.ParamByName('email').AsString := EditEmail.Text;
    ZQuery1.ExecSQL;

    AfficherStatut('Client ajouté avec succès');

    // Vider les champs
    EditNom.Clear;
    EditEmail.Clear;

    // Recharger les données
    ButtonChargerClick(nil);
  except
    on E: Exception do
    begin
      AfficherStatut('Erreur lors de l''ajout: ' + E.Message, True);
    end;
  end;
end;

procedure TFormPrincipale.AfficherStatut(const Message: string;
  Erreur: Boolean);
begin
  LabelStatut.Caption := Message;
  if Erreur then
    LabelStatut.Font.Color := clRed
  else
    LabelStatut.Font.Color := clGreen;
end;

procedure TFormPrincipale.FormDestroy(Sender: TObject);
begin
  if ZConnection1.Connected then
    ZConnection1.Connected := False;
end;

end.
```

## Intégration avec les composants visuels Lazarus

### Liaison avec TDBGrid

```pascal
procedure ConfigurerGrille;
begin
  // Configurer la requête
  ZQuery1.Connection := ZConnection1;
  ZQuery1.SQL.Text := 'SELECT id, nom, email, ville FROM clients';
  ZQuery1.Open;

  // Lier au DataSource
  DataSource1.DataSet := ZQuery1;

  // Configurer la grille
  DBGrid1.DataSource := DataSource1;

  // Personnaliser les colonnes
  with DBGrid1.Columns.Add do
  begin
    Title.Caption := 'Identifiant';
    FieldName := 'id';
    Width := 80;
  end;

  with DBGrid1.Columns.Add do
  begin
    Title.Caption := 'Nom complet';
    FieldName := 'nom';
    Width := 200;
  end;

  // Options d'affichage
  DBGrid1.Options := DBGrid1.Options + [dgRowSelect, dgAlwaysShowSelection];
end;
```

### Liaison avec TDBEdit et TDBComboBox

```pascal
procedure ConfigurerEdition;
begin
  // Édition de texte
  DBEdit1.DataSource := DataSource1;
  DBEdit1.DataField := 'nom';

  DBEdit2.DataSource := DataSource1;
  DBEdit2.DataField := 'email';

  // ComboBox lié à la base
  DBComboBox1.DataSource := DataSource1;
  DBComboBox1.DataField := 'ville';
  DBComboBox1.Items.Add('Paris');
  DBComboBox1.Items.Add('Lyon');
  DBComboBox1.Items.Add('Marseille');
end;
```

### Navigation dans les enregistrements

```pascal
procedure BoutonPrecedentClick(Sender: TObject);
begin
  if not ZQuery1.BOF then
    ZQuery1.Prior;
end;

procedure BoutonSuivantClick(Sender: TObject);
begin
  if not ZQuery1.EOF then
    ZQuery1.Next;
end;

procedure BoutonPremierClick(Sender: TObject);
begin
  ZQuery1.First;
end;

procedure BoutonDernierClick(Sender: TObject);
begin
  ZQuery1.Last;
end;

procedure AfficherPosition;
begin
  LabelPosition.Caption := Format('Enregistrement %d sur %d',
    [ZQuery1.RecNo, ZQuery1.RecordCount]);
end;
```

## Cas d'usage avancés

### Master-Detail (Maître-Détail)

Afficher des commandes avec leurs lignes de détail :

```pascal
procedure ConfigurerMasterDetail;
begin
  // Table maître (commandes)
  ZQueryCommandes.Connection := ZConnection1;
  ZQueryCommandes.SQL.Text := 'SELECT * FROM commandes';
  ZQueryCommandes.Open;

  // Table détail (lignes de commande)
  ZQueryDetails.Connection := ZConnection1;
  ZQueryDetails.SQL.Text :=
    'SELECT * FROM lignes_commande WHERE commande_id = :id';

  // Lier le paramètre au champ de la table maître
  ZQueryDetails.DataSource := DataSourceCommandes;
  ZQueryDetails.MasterSource := DataSourceCommandes;
  ZQueryDetails.MasterFields := 'id';

  ZQueryDetails.Open;

  // Configuration des grilles
  DBGrid1.DataSource := DataSourceCommandes;  // Commandes
  DBGrid2.DataSource := DataSourceDetails;    // Détails
end;
```

### Filtrage côté client

```pascal
procedure FiltrerClients(const Ville: string);
begin
  if Ville = '' then
    ZQuery1.Filtered := False
  else
  begin
    ZQuery1.Filter := 'ville = ' + QuotedStr(Ville);
    ZQuery1.Filtered := True;
  end;
end;

// Filtre plus complexe
procedure FiltrerParNomEtVille(const Nom, Ville: string);
var
  Filtre: string;
begin
  Filtre := '';

  if Nom <> '' then
    Filtre := 'nom LIKE ' + QuotedStr('%' + Nom + '%');

  if Ville <> '' then
  begin
    if Filtre <> '' then
      Filtre := Filtre + ' AND ';
    Filtre := Filtre + 'ville = ' + QuotedStr(Ville);
  end;

  if Filtre = '' then
    ZQuery1.Filtered := False
  else
  begin
    ZQuery1.Filter := Filtre;
    ZQuery1.Filtered := True;
  end;
end;
```

### Recherche incrémentale

```pascal
procedure EditRechercheChange(Sender: TObject);
begin
  if Trim(EditRecherche.Text) = '' then
  begin
    ZQuery1.Filtered := False;
  end
  else
  begin
    ZQuery1.Filter :=
      'nom LIKE ' + QuotedStr(EditRecherche.Text + '%');
    ZQuery1.Filtered := True;
  end;
end;
```

### Export de données

```pascal
uses
  Clipbrd;  // Pour le presse-papiers

procedure ExporterVersCSV(const NomFichier: string);
var
  F: TextFile;
  i: Integer;
  Ligne: string;
begin
  AssignFile(F, NomFichier);
  Rewrite(F);
  try
    // En-têtes
    Ligne := '';
    for i := 0 to ZQuery1.FieldCount - 1 do
    begin
      if i > 0 then
        Ligne := Ligne + ';';
      Ligne := Ligne + ZQuery1.Fields[i].FieldName;
    end;
    WriteLn(F, Ligne);

    // Données
    ZQuery1.First;
    while not ZQuery1.EOF do
    begin
      Ligne := '';
      for i := 0 to ZQuery1.FieldCount - 1 do
      begin
        if i > 0 then
          Ligne := Ligne + ';';
        Ligne := Ligne + ZQuery1.Fields[i].AsString;
      end;
      WriteLn(F, Ligne);
      ZQuery1.Next;
    end;

    ShowMessage('Export terminé: ' + NomFichier);
  finally
    CloseFile(F);
  end;
end;

procedure CopierVersPressePapiers;
var
  Texte: string;
  i: Integer;
begin
  if ZQuery1.IsEmpty then
    Exit;

  Texte := '';
  for i := 0 to ZQuery1.FieldCount - 1 do
  begin
    if i > 0 then
      Texte := Texte + #9;  // Tabulation
    Texte := Texte + ZQuery1.Fields[i].AsString;
  end;

  Clipboard.AsText := Texte;
  ShowMessage('Données copiées dans le presse-papiers');
end;
```

## Gestion de connexions multiples

Parfois, vous devez vous connecter à plusieurs bases de données simultanément :

```pascal
type
  TGestionnaireMultiBases = class
  private
    FConnexionPrincipale: TZConnection;
    FConnexionSecondaire: TZConnection;
    FConnexionLogs: TZConnection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SynchroniserDonnees;
    procedure LoguerAction(const Action: string);
  end;

constructor TGestionnaireMultiBases.Create;
begin
  // Connexion principale (PostgreSQL)
  FConnexionPrincipale := TZConnection.Create(nil);
  FConnexionPrincipale.Protocol := 'postgresql-9';
  FConnexionPrincipale.HostName := 'serveur1.local';
  FConnexionPrincipale.Database := 'production';

  // Connexion secondaire (MySQL)
  FConnexionSecondaire := TZConnection.Create(nil);
  FConnexionSecondaire.Protocol := 'mysql-8';
  FConnexionSecondaire.HostName := 'serveur2.local';
  FConnexionSecondaire.Database := 'archives';

  // Connexion logs (SQLite local)
  FConnexionLogs := TZConnection.Create(nil);
  FConnexionLogs.Protocol := 'sqlite-3';
  FConnexionLogs.Database := ExtractFilePath(ParamStr(0)) + 'logs.db';

  // Connecter toutes les bases
  FConnexionPrincipale.Connected := True;
  FConnexionSecondaire.Connected := True;
  FConnexionLogs.Connected := True;
end;

procedure TGestionnaireMultiBases.SynchroniserDonnees;
var
  QuerySource, QueryDest: TZQuery;
begin
  QuerySource := TZQuery.Create(nil);
  QueryDest := TZQuery.Create(nil);
  try
    // Lire depuis la base principale
    QuerySource.Connection := FConnexionPrincipale;
    QuerySource.SQL.Text := 'SELECT * FROM clients WHERE synchro = 0';
    QuerySource.Open;

    // Préparer l'insertion dans la base secondaire
    QueryDest.Connection := FConnexionSecondaire;
    QueryDest.SQL.Text :=
      'INSERT INTO clients_archive (nom, email) VALUES (:nom, :email)';

    // Synchroniser
    while not QuerySource.EOF do
    begin
      QueryDest.ParamByName('nom').AsString :=
        QuerySource.FieldByName('nom').AsString;
      QueryDest.ParamByName('email').AsString :=
        QuerySource.FieldByName('email').AsString;
      QueryDest.ExecSQL;

      QuerySource.Next;
    end;

    LoguerAction('Synchronisation terminée');
  finally
    QuerySource.Free;
    QueryDest.Free;
  end;
end;
```

## Performances et optimisation

### Utilisation du cache

```pascal
procedure ActiverCache;
begin
  // Activer le cache des requêtes
  ZConnection1.Properties.Add('cachedlobs=true');
  ZConnection1.Properties.Add('ChunkSize=4096');
end;
```

### Limiter le nombre de résultats

```pascal
procedure ChargerAvecLimit(Limit, Offset: Integer);
begin
  // PostgreSQL, MySQL
  ZQuery1.SQL.Text :=
    'SELECT * FROM clients LIMIT :limit OFFSET :offset';
  ZQuery1.ParamByName('limit').AsInteger := Limit;
  ZQuery1.ParamByName('offset').AsInteger := Offset;
  ZQuery1.Open;
end;
```

### Pagination efficace

```pascal
type
  TPagination = class
  private
    FQuery: TZQuery;
    FPageCourante: Integer;
    FLignesParPage: Integer;
    FTotalEnregistrements: Integer;
  public
    constructor Create(Query: TZQuery; LignesParPage: Integer);
    procedure PageSuivante;
    procedure PagePrecedente;
    procedure AllerPage(NumPage: Integer);
    function NombrePages: Integer;
    property PageCourante: Integer read FPageCourante;
  end;

constructor TPagination.Create(Query: TZQuery; LignesParPage: Integer);
var
  QueryCount: TZQuery;
begin
  FQuery := Query;
  FLignesParPage := LignesParPage;
  FPageCourante := 1;

  // Compter le total
  QueryCount := TZQuery.Create(nil);
  try
    QueryCount.Connection := FQuery.Connection;
    QueryCount.SQL.Text := 'SELECT COUNT(*) FROM clients';
    QueryCount.Open;
    FTotalEnregistrements := QueryCount.Fields[0].AsInteger;
  finally
    QueryCount.Free;
  end;
end;

procedure TPagination.AllerPage(NumPage: Integer);
var
  Offset: Integer;
begin
  if (NumPage < 1) or (NumPage > NombrePages) then
    Exit;

  FPageCourante := NumPage;
  Offset := (FPageCourante - 1) * FLignesParPage;

  FQuery.Close;
  FQuery.SQL.Text :=
    'SELECT * FROM clients LIMIT :limit OFFSET :offset';
  FQuery.ParamByName('limit').AsInteger := FLignesParPage;
  FQuery.ParamByName('offset').AsInteger := Offset;
  FQuery.Open;
end;

function TPagination.NombrePages: Integer;
begin
  Result := (FTotalEnregistrements + FLignesParPage - 1) div FLignesParPage;
end;
```

## Tests unitaires avec ZEOS

```pascal
unit TestsZEOS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ZConnection, ZDataset;

type
  TTestZEOS = class(TTestCase)
  private
    FConnection: TZConnection;
    FQuery: TZQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnexion;
    procedure TestInsertionSimple;
    procedure TestRequeteParametree;
    procedure TestTransaction;
  end;

implementation

procedure TTestZEOS.SetUp;
begin
  FConnection := TZConnection.Create(nil);
  FConnection.Protocol := 'sqlite-3';
  FConnection.Database := ':memory:';  // Base en mémoire pour les tests
  FConnection.Connected := True;

  // Créer une table de test
  FQuery := TZQuery.Create(nil);
  FQuery.Connection := FConnection;
  FQuery.SQL.Text :=
    'CREATE TABLE test_clients (id INTEGER PRIMARY KEY, nom TEXT, email TEXT)';
  FQuery.ExecSQL;
end;

procedure TTestZEOS.TearDown;
begin
  FQuery.Free;
  FConnection.Free;
end;

procedure TTestZEOS.TestConnexion;
begin
  AssertTrue('Connexion doit être active', FConnection.Connected);
end;

procedure TTestZEOS.TestInsertionSimple;
begin
  FQuery.SQL.Text :=
    'INSERT INTO test_clients (nom, email) VALUES (:nom, :email)';
  FQuery.ParamByName('nom').AsString := 'Test User';
  FQuery.ParamByName('email').AsString := 'test@example.com';
  FQuery.ExecSQL;

  FQuery.SQL.Text := 'SELECT COUNT(*) FROM test_clients';
  FQuery.Open;

  AssertEquals('Devrait avoir 1 enregistrement', 1,
    FQuery.Fields[0].AsInteger);
end;

procedure TTestZEOS.TestRequeteParametree;
var
  NomRecherche: string;
begin
  // Insérer des données
  FQuery.SQL.Text :=
    'INSERT INTO test_clients (nom, email) VALUES (:nom, :email)';
  FQuery.ParamByName('nom').AsString := 'Alice';
  FQuery.ParamByName('email').AsString := 'alice@test.com';
  FQuery.ExecSQL;

  // Rechercher
  FQuery.SQL.Text := 'SELECT nom FROM test_clients WHERE nom = :nom';
  FQuery.ParamByName('nom').AsString := 'Alice';
  FQuery.Open;

  AssertFalse('Devrait trouver un résultat', FQuery.EOF);
  AssertEquals('Le nom devrait être Alice', 'Alice',
    FQuery.FieldByName('nom').AsString);
end;

procedure TTestZEOS.TestTransaction;
begin
  FConnection.StartTransaction;
  try
    FQuery.SQL.Text :=
      'INSERT INTO test_clients (nom, email) VALUES (:nom, :email)';
    FQuery.ParamByName('nom').AsString := 'Transac Test';
    FQuery.ParamByName('email').AsString := 'transac@test.com';
    FQuery.ExecSQL;

    FConnection.Rollback;  // Annuler
  except
    FConnection.Rollback;
    raise;
  end;

  // Vérifier que rien n'a été inséré
  FQuery.SQL.Text := 'SELECT COUNT(*) FROM test_clients';
  FQuery.Open;

  AssertEquals('Aucun enregistrement après rollback', 0,
    FQuery.Fields[0].AsInteger);
end;

initialization
  RegisterTest(TTestZEOS);

end.
```

## Résumé

ZEOS DBO est une solution puissante et universelle pour l'accès aux bases de données dans vos applications FreePascal/Lazarus. Ses points forts :

✅ **Interface unifiée** pour de nombreux SGBD  
✅ **Multi-plateforme** natif (Windows, Linux, macOS)  
✅ **Hautes performances** avec optimisations natives  
✅ **Open source** et bien maintenu  
✅ **API simple et intuitive**  
✅ **Intégration parfaite** avec les composants Lazarus  
✅ **Support complet** des transactions et prepared statements  
✅ **Communauté active** et documentation riche

Avec ZEOS, vous pouvez développer des applications professionnelles de gestion de bases de données qui fonctionnent de manière identique sur Windows et Linux, sans vous soucier des différences de bas niveau entre les systèmes.

Dans les prochains chapitres, nous explorerons d'autres aspects avancés de la gestion des bases de données, notamment le développement d'ORM personnalisés et l'utilisation de frameworks comme mORMot.

⏭️ [Développement d'un ORM personnalisé portable](/08-bases-donnees-orm-multiplatefomes/05-developpement-orm-personnalise-portable.md)
