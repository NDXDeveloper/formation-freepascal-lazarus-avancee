🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.8 Migration et versionnement de schémas

## Introduction

La migration et le versionnement de schémas de base de données sont des pratiques essentielles pour gérer l'évolution de votre application dans le temps. Au fur et à mesure que votre projet grandit, vous devrez modifier la structure de votre base de données : ajouter des tables, modifier des colonnes, créer des index, etc.

Sans un système de migration approprié, ces changements peuvent devenir chaotiques et sources d'erreurs, particulièrement lorsque vous devez déployer votre application sur plusieurs environnements (développement, test, production) ou sur différents systèmes d'exploitation (Windows et Ubuntu).

## Qu'est-ce que le versionnement de schémas ?

Le **versionnement de schémas** consiste à traiter votre structure de base de données comme du code source : chaque modification est enregistrée, numérotée et peut être appliquée de manière contrôlée et reproductible.

### Analogie simple

Imaginez que votre base de données est comme une maison :
- **Version 1** : Construction de la maison de base (tables principales)
- **Version 2** : Ajout d'une extension (nouvelles tables)
- **Version 3** : Installation d'une nouvelle fenêtre (ajout d'une colonne)
- **Version 4** : Rénovation de la cuisine (modification de contraintes)

Chaque étape est documentée et peut être reproduite sur une autre maison identique.

## Pourquoi est-ce important ?

### 1. Traçabilité
Vous savez exactement quels changements ont été appliqués à votre base de données et quand.

### 2. Reproductibilité
Vous pouvez recréer une base de données identique sur n'importe quel environnement (Windows ou Ubuntu).

### 3. Travail en équipe
Plusieurs développeurs peuvent travailler sur le même projet sans écraser les modifications des autres.

### 4. Déploiement sécurisé
Vous pouvez mettre à jour vos bases de données de production de manière contrôlée et réversible.

### 5. Multi-plateforme
Les migrations fonctionnent de la même manière sur Windows et Ubuntu, garantissant la cohérence.

## Concepts fondamentaux

### Version de schéma

Chaque état de votre base de données possède un **numéro de version**. Ce numéro est généralement stocké dans une table spéciale de la base de données elle-même.

```sql
CREATE TABLE schema_version (
    version INTEGER PRIMARY KEY,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    description VARCHAR(255)
);
```

### Migration UP et DOWN

Chaque migration contient deux parties :

- **UP (montée)** : Les changements à appliquer pour passer à la version suivante
- **DOWN (descente)** : Les changements pour revenir à la version précédente (rollback)

**Exemple de migration Version 1 → Version 2 :**

```sql
-- UP : Ajouter une table clients
CREATE TABLE clients (
    id INTEGER PRIMARY KEY AUTO_INCREMENT,
    nom VARCHAR(100) NOT NULL,
    email VARCHAR(255) UNIQUE
);

-- DOWN : Supprimer la table clients
DROP TABLE clients;
```

### Scripts de migration

Chaque migration est généralement stockée dans un fichier séparé avec une convention de nommage claire :

```
migrations/
├── 001_create_users_table.sql
├── 002_add_clients_table.sql
├── 003_add_email_index.sql
└── 004_add_address_column.sql
```

## Architecture d'un système de migration en FreePascal

### Structure de base

Voici les composants principaux d'un système de migration :

```pascal
unit MigrationManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB;

type
  TMigration = class
  private
    FVersion: Integer;
    FDescription: String;
    FUpSQL: TStringList;
    FDownSQL: TStringList;
  public
    constructor Create(AVersion: Integer; ADescription: String);
    destructor Destroy; override;

    property Version: Integer read FVersion;
    property Description: String read FDescription;
    property UpSQL: TStringList read FUpSQL;
    property DownSQL: TStringList read FDownSQL;
  end;

  TMigrationManager = class
  private
    FConnection: TSQLConnection;
    FMigrations: TList;

    function GetCurrentVersion: Integer;
    procedure SetVersion(AVersion: Integer);
    function EnsureVersionTable: Boolean;
  public
    constructor Create(AConnection: TSQLConnection);
    destructor Destroy; override;

    procedure RegisterMigration(AMigration: TMigration);
    function MigrateUp(ATargetVersion: Integer = -1): Boolean;
    function MigrateDown(ATargetVersion: Integer): Boolean;
    function GetPendingMigrations: Integer;

    property CurrentVersion: Integer read GetCurrentVersion;
  end;

implementation

// Implémentation...

end.
```

### Création de la table de versions

La première étape consiste à créer une table pour suivre les versions appliquées :

```pascal
function TMigrationManager.EnsureVersionTable: Boolean;  
var
  Query: TSQLQuery;
begin
  Result := False;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    // SQL compatible Windows/Ubuntu pour PostgreSQL, MySQL, SQLite
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS schema_version (' +
      '  version INTEGER PRIMARY KEY, ' +
      '  applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, ' +
      '  description VARCHAR(255) ' +
      ')';

    try
      Query.ExecSQL;
      FConnection.Transaction.Commit;
      Result := True;
    except
      on E: Exception do
      begin
        FConnection.Transaction.Rollback;
        WriteLn('Erreur création table version: ', E.Message);
      end;
    end;
  finally
    Query.Free;
  end;
end;
```

### Obtenir la version actuelle

```pascal
function TMigrationManager.GetCurrentVersion: Integer;  
var
  Query: TSQLQuery;
begin
  Result := 0;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := 'SELECT MAX(version) as current_version FROM schema_version';

    try
      Query.Open;
      if not Query.EOF then
        Result := Query.FieldByName('current_version').AsInteger;
      Query.Close;
    except
      // Si la table n'existe pas encore
      Result := 0;
    end;
  finally
    Query.Free;
  end;
end;
```

### Enregistrer une migration

```pascal
procedure TMigrationManager.RegisterMigration(AMigration: TMigration);  
begin
  FMigrations.Add(AMigration);
end;
```

### Appliquer les migrations (UP)

```pascal
function TMigrationManager.MigrateUp(ATargetVersion: Integer = -1): Boolean;  
var
  i: Integer;
  Migration: TMigration;
  Query: TSQLQuery;
  CurrentVer: Integer;
begin
  Result := False;

  // S'assurer que la table de version existe
  if not EnsureVersionTable then
    Exit;

  CurrentVer := GetCurrentVersion;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    // Parcourir toutes les migrations enregistrées
    for i := 0 to FMigrations.Count - 1 do
    begin
      Migration := TMigration(FMigrations[i]);

      // Appliquer seulement les migrations non encore appliquées
      if Migration.Version <= CurrentVer then
        Continue;

      // Si une version cible est spécifiée, s'arrêter là
      if (ATargetVersion > 0) and (Migration.Version > ATargetVersion) then
        Break;

      WriteLn('Application migration v', Migration.Version, ': ', Migration.Description);

      try
        FConnection.Transaction.StartTransaction;

        // Exécuter tous les scripts UP
        Query.SQL.Text := Migration.UpSQL.Text;
        Query.ExecSQL;

        // Enregistrer la version appliquée
        Query.SQL.Text :=
          'INSERT INTO schema_version (version, description) ' +
          'VALUES (:version, :description)';
        Query.Params.ParamByName('version').AsInteger := Migration.Version;
        Query.Params.ParamByName('description').AsString := Migration.Description;
        Query.ExecSQL;

        FConnection.Transaction.Commit;
        WriteLn('Migration v', Migration.Version, ' appliquée avec succès');
      except
        on E: Exception do
        begin
          FConnection.Transaction.Rollback;
          WriteLn('ERREUR lors de la migration v', Migration.Version, ': ', E.Message);
          Exit;
        end;
      end;
    end;

    Result := True;
  finally
    Query.Free;
  end;
end;
```

### Revenir en arrière (DOWN)

```pascal
function TMigrationManager.MigrateDown(ATargetVersion: Integer): Boolean;  
var
  i: Integer;
  Migration: TMigration;
  Query: TSQLQuery;
  CurrentVer: Integer;
begin
  Result := False;
  CurrentVer := GetCurrentVersion;

  if ATargetVersion >= CurrentVer then
  begin
    WriteLn('Version cible >= version actuelle, aucun rollback nécessaire');
    Exit(True);
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    // Parcourir les migrations en ordre inverse
    for i := FMigrations.Count - 1 downto 0 do
    begin
      Migration := TMigration(FMigrations[i]);

      // Appliquer les rollbacks des versions supérieures à la cible
      if Migration.Version <= ATargetVersion then
        Break;

      if Migration.Version > CurrentVer then
        Continue;

      WriteLn('Rollback migration v', Migration.Version, ': ', Migration.Description);

      try
        FConnection.Transaction.StartTransaction;

        // Exécuter le script DOWN
        Query.SQL.Text := Migration.DownSQL.Text;
        Query.ExecSQL;

        // Supprimer l'entrée de version
        Query.SQL.Text := 'DELETE FROM schema_version WHERE version = :version';
        Query.Params.ParamByName('version').AsInteger := Migration.Version;
        Query.ExecSQL;

        FConnection.Transaction.Commit;
        WriteLn('Rollback v', Migration.Version, ' effectué avec succès');
      except
        on E: Exception do
        begin
          FConnection.Transaction.Rollback;
          WriteLn('ERREUR lors du rollback v', Migration.Version, ': ', E.Message);
          Exit;
        end;
      end;
    end;

    Result := True;
  finally
    Query.Free;
  end;
end;
```

## Exemple d'utilisation pratique

### Définir les migrations

```pascal
program MigrationExample;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, SQLDB, PQConnection, MigrationManager;

procedure DefineMigrations(Manager: TMigrationManager);  
var
  Migration: TMigration;
begin
  // Migration 1 : Créer la table utilisateurs
  Migration := TMigration.Create(1, 'Création table utilisateurs');
  Migration.UpSQL.Add('CREATE TABLE users (');
  Migration.UpSQL.Add('  id SERIAL PRIMARY KEY,');
  Migration.UpSQL.Add('  username VARCHAR(50) NOT NULL UNIQUE,');
  Migration.UpSQL.Add('  email VARCHAR(255) NOT NULL,');
  Migration.UpSQL.Add('  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP');
  Migration.UpSQL.Add(')');
  Migration.DownSQL.Add('DROP TABLE users');
  Manager.RegisterMigration(Migration);

  // Migration 2 : Ajouter la table produits
  Migration := TMigration.Create(2, 'Création table produits');
  Migration.UpSQL.Add('CREATE TABLE products (');
  Migration.UpSQL.Add('  id SERIAL PRIMARY KEY,');
  Migration.UpSQL.Add('  name VARCHAR(200) NOT NULL,');
  Migration.UpSQL.Add('  price DECIMAL(10,2) NOT NULL,');
  Migration.UpSQL.Add('  stock INTEGER DEFAULT 0');
  Migration.UpSQL.Add(')');
  Migration.DownSQL.Add('DROP TABLE products');
  Manager.RegisterMigration(Migration);

  // Migration 3 : Ajouter un index sur l'email
  Migration := TMigration.Create(3, 'Ajout index email utilisateurs');
  Migration.UpSQL.Add('CREATE INDEX idx_users_email ON users(email)');
  Migration.DownSQL.Add('DROP INDEX idx_users_email');
  Manager.RegisterMigration(Migration);

  // Migration 4 : Ajouter une colonne phone
  Migration := TMigration.Create(4, 'Ajout colonne téléphone');
  Migration.UpSQL.Add('ALTER TABLE users ADD COLUMN phone VARCHAR(20)');
  Migration.DownSQL.Add('ALTER TABLE users DROP COLUMN phone');
  Manager.RegisterMigration(Migration);
end;

var
  Connection: TPQConnection;
  Transaction: TSQLTransaction;
  Manager: TMigrationManager;
begin
  // Configuration de la connexion (identique Windows/Ubuntu)
  Connection := TPQConnection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := 'myapp_db';
    Connection.HostName := 'localhost';
    Connection.UserName := 'postgres';
    Connection.Password := 'password';

    Connection.Open;

    // Créer le gestionnaire de migrations
    Manager := TMigrationManager.Create(Connection);
    try
      // Définir toutes les migrations
      DefineMigrations(Manager);

      WriteLn('Version actuelle: ', Manager.CurrentVersion);
      WriteLn('Migrations en attente: ', Manager.GetPendingMigrations);

      // Appliquer toutes les migrations
      WriteLn('Application des migrations...');
      if Manager.MigrateUp then
        WriteLn('Toutes les migrations appliquées avec succès!')
      else
        WriteLn('Erreur lors des migrations');

      WriteLn('Nouvelle version: ', Manager.CurrentVersion);
    finally
      Manager.Free;
    end;
  finally
    Connection.Close;
    Connection.Free;
    Transaction.Free;
  end;
end.
```

## Bonnes pratiques

### 1. Ne jamais modifier une migration déjà appliquée

Une fois qu'une migration a été appliquée en production, **ne la modifiez jamais**. Créez plutôt une nouvelle migration pour corriger ou modifier.

❌ **Mauvais :**
```pascal
// Modifier migration_002 déjà en production
Migration.UpSQL.Add('ALTER TABLE users ADD COLUMN age INTEGER'); // Ajout tardif
```

✅ **Bon :**
```pascal
// Créer migration_005
Migration := TMigration.Create(5, 'Ajout colonne age');  
Migration.UpSQL.Add('ALTER TABLE users ADD COLUMN age INTEGER');
```

### 2. Tester les migrations UP et DOWN

Toujours tester que vos migrations peuvent monter ET descendre correctement :

```pascal
// Test du cycle complet
Manager.MigrateUp(5);      // Monter à v5  
Manager.MigrateDown(3);    // Descendre à v3  
Manager.MigrateUp;         // Remonter à la dernière version
```

### 3. Utiliser des transactions

Chaque migration doit être exécutée dans une transaction pour garantir l'atomicité :

```pascal
FConnection.Transaction.StartTransaction;  
try
  // Appliquer la migration
  Query.ExecSQL;
  FConnection.Transaction.Commit;
except
  FConnection.Transaction.Rollback;
  raise;
end;
```

### 4. Documenter chaque migration

Utilisez des descriptions claires et explicites :

```pascal
// ✅ Bon
TMigration.Create(7, 'Ajout index pour optimiser recherche utilisateurs par email');

// ❌ Mauvais
TMigration.Create(7, 'Ajout index');
```

### 5. Gérer les données existantes

Lors de modifications de structure, pensez aux données existantes :

```pascal
// Ajouter une colonne NOT NULL avec valeur par défaut
Migration.UpSQL.Add('ALTER TABLE users ADD COLUMN role VARCHAR(20) DEFAULT ''user''');  
Migration.UpSQL.Add('UPDATE users SET role = ''user'' WHERE role IS NULL');  
Migration.UpSQL.Add('ALTER TABLE users ALTER COLUMN role SET NOT NULL');
```

### 6. Sauvegardes avant migration en production

Toujours faire une sauvegarde complète avant d'appliquer des migrations en production :

```bash
# Ubuntu/Linux
pg_dump -U postgres myapp_db > backup_avant_migration.sql

# Windows (même commande dans PowerShell)
pg_dump -U postgres myapp_db > backup_avant_migration.sql
```

## Compatibilité multi-plateforme Windows/Ubuntu

### SQL standard

Privilégiez le SQL standard compatible entre PostgreSQL, MySQL et SQLite :

```sql
-- ✅ Compatible
CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    name VARCHAR(100)
);

-- ❌ Spécifique PostgreSQL
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100)
);
```

### Gestion des types de données

Certains types varient entre SGBD :

```pascal
// Fonction helper pour générer du SQL compatible
function GetAutoIncrementSQL(DBType: TDatabaseType): String;  
begin
  case DBType of
    dtPostgreSQL: Result := 'SERIAL';
    dtMySQL: Result := 'INTEGER AUTO_INCREMENT';
    dtSQLite: Result := 'INTEGER PRIMARY KEY AUTOINCREMENT';
  end;
end;
```

### Chemins des fichiers de migration

Utilisez des chemins compatibles :

```pascal
function GetMigrationPath: String;  
begin
  {$IFDEF WINDOWS}
  Result := ExtractFilePath(ParamStr(0)) + 'migrations\';
  {$ELSE}
  Result := ExtractFilePath(ParamStr(0)) + 'migrations/';
  {$ENDIF}

  // Ou mieux, utiliser les fonctions multi-plateformes
  Result := IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0)) + 'migrations'
  );
end;
```

## Chargement des migrations depuis des fichiers

Pour des projets complexes, stockez vos migrations dans des fichiers SQL séparés :

```pascal
procedure TMigrationManager.LoadMigrationsFromFiles(const APath: String);  
var
  SearchRec: TSearchRec;
  FileName, FilePath: String;
  Version: Integer;
  Description: String;
  Migration: TMigration;
  Content: TStringList;
begin
  if FindFirst(APath + '*.sql', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      FileName := SearchRec.Name;

      // Format attendu: 001_description.sql
      if TryStrToInt(Copy(FileName, 1, 3), Version) then
      begin
        Description := Copy(FileName, 5, Length(FileName) - 8); // Enlever .sql
        Description := StringReplace(Description, '_', ' ', [rfReplaceAll]);

        Migration := TMigration.Create(Version, Description);

        FilePath := APath + FileName;
        Content := TStringList.Create;
        try
          Content.LoadFromFile(FilePath);

          // Parser le contenu pour séparer UP et DOWN
          ParseMigrationFile(Content, Migration.UpSQL, Migration.DownSQL);

          RegisterMigration(Migration);
        finally
          Content.Free;
        end;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;
```

Format de fichier migration :

```sql
-- Migration: 001_create_users_table.sql
-- Description: Création de la table utilisateurs

-- === UP ===

CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    email VARCHAR(255) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_users_email ON users(email);

-- === DOWN ===

DROP INDEX idx_users_email;  
DROP TABLE users;
```

## Outils de ligne de commande

Créer un outil CLI pour gérer les migrations :

```pascal
program migrate;

{$mode objfpc}{$H+}

uses
  SysUtils, MigrationManager;

procedure ShowHelp;  
begin
  WriteLn('Gestionnaire de migrations de base de données');
  WriteLn('Usage: migrate [commande]');
  WriteLn;
  WriteLn('Commandes:');
  WriteLn('  status    - Afficher la version actuelle et migrations en attente');
  WriteLn('  up        - Appliquer toutes les migrations en attente');
  WriteLn('  up [n]    - Migrer jusqu''à la version n');
  WriteLn('  down [n]  - Revenir à la version n');
  WriteLn('  create    - Créer un nouveau fichier de migration');
end;

procedure ExecuteMigration(const Command: String; const Param: String = '');  
var
  Manager: TMigrationManager;
  Connection: TSQLConnection;
  TargetVersion: Integer;
begin
  // Initialiser la connexion et le manager...

  case LowerCase(Command) of
    'status':
      begin
        WriteLn('Version actuelle: ', Manager.CurrentVersion);
        WriteLn('Migrations en attente: ', Manager.GetPendingMigrations);
      end;

    'up':
      begin
        if Param <> '' then
          TargetVersion := StrToInt(Param)
        else
          TargetVersion := -1;

        if Manager.MigrateUp(TargetVersion) then
          WriteLn('Migrations appliquées avec succès')
        else
          WriteLn('Erreur lors des migrations');
      end;

    'down':
      begin
        TargetVersion := StrToInt(Param);
        if Manager.MigrateDown(TargetVersion) then
          WriteLn('Rollback effectué avec succès')
        else
          WriteLn('Erreur lors du rollback');
      end;

    'create':
      CreateNewMigrationFile;
  end;
end;

begin
  if ParamCount < 1 then
  begin
    ShowHelp;
    Exit;
  end;

  ExecuteMigration(ParamStr(1), ParamStr(2));
end.
```

Utilisation :

```bash
# Windows (PowerShell) et Ubuntu (Terminal)
./migrate status
./migrate up
./migrate up 5
./migrate down 3
```

## Intégration avec le contrôle de version (Git)

Les fichiers de migration doivent être versionnés avec votre code :

```
monprojet/
├── src/
├── migrations/
│   ├── 001_create_users.sql
│   ├── 002_create_products.sql
│   └── 003_add_indexes.sql
└── .git/
```

Dans votre `.gitignore`, ne pas exclure les migrations :

```gitignore
# .gitignore
*.exe
*.o
*.ppu
lib/
# Ne PAS ignorer migrations/
```

## Conclusion

Le versionnement de schémas est une pratique essentielle pour tout projet professionnel utilisant une base de données. Avec FreePascal/Lazarus, vous pouvez créer un système de migration robuste qui fonctionne de manière identique sur Windows et Ubuntu.

### Points clés à retenir

1. **Chaque changement de schéma = une migration numérotée**
2. **Toujours prévoir un rollback (DOWN)**
3. **Ne jamais modifier une migration appliquée**
4. **Utiliser des transactions pour l'atomicité**
5. **Tester sur tous les environnements cibles**
6. **Sauvegarder avant toute migration en production**

En suivant ces principes, vous garantissez l'évolution contrôlée et sécurisée de vos bases de données tout au long du cycle de vie de votre application.

⏭️ [Optimisation des requêtes et indexation](/08-bases-donnees-orm-multiplatefomes/09-optimisation-requetes-indexation.md)
