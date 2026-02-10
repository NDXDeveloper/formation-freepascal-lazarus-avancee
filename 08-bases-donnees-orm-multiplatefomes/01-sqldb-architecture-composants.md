🔝 Retour au [Sommaire](/SOMMAIRE.md)

# SQLdb : Architecture et Composants - Guide pour Débutants

## Introduction

SQLdb est le framework de base de données intégré à FreePascal/Lazarus qui permet de créer des applications capables de communiquer avec différents systèmes de gestion de bases de données (SGBD). Ce guide vous expliquera son architecture et ses composants principaux de manière accessible.

## Qu'est-ce que SQLdb ?

SQLdb est une bibliothèque qui fait le pont entre votre application Lazarus et une base de données. Imaginez-le comme un traducteur universel qui permet à votre programme Pascal de "parler" avec différentes bases de données (MySQL, PostgreSQL, SQLite, etc.) sans avoir à apprendre le dialecte spécifique de chacune.

### Avantages de SQLdb

- **Multi-bases de données** : Un seul code peut fonctionner avec différents SGBD
- **Multi-plateforme** : Fonctionne identiquement sur Windows et Linux/Ubuntu
- **Intégré à Lazarus** : Pas besoin d'installer de bibliothèques supplémentaires
- **Composants visuels** : Conception facile avec glisser-déposer dans l'IDE

## Architecture de SQLdb

### Vue d'ensemble

L'architecture de SQLdb suit un modèle en couches qui sépare votre application de la base de données physique :

```
Application (votre programme)
        ↓
    SQLdb API
        ↓
Couche d'abstraction
        ↓
Connecteur spécifique (MySQL, PostgreSQL, etc.)
        ↓
Base de données
```

### Les trois piliers de SQLdb

1. **La connexion** : Établit le lien avec la base de données
2. **La transaction** : Gère les modifications de données de manière sécurisée
3. **Les requêtes** : Permettent de lire et modifier les données

## Les Composants Principaux

### 1. TSQLConnection et ses descendants

Le composant de connexion est le point de départ. Il existe un composant spécifique pour chaque type de base de données :

- **TPQConnection** : Pour PostgreSQL
- **TMySQL56Connection** : Pour MySQL 5.6
- **TMySQL57Connection** : Pour MySQL 5.7
- **TSQLite3Connection** : Pour SQLite3
- **TIBConnection** : Pour Firebird/InterBase
- **TODBCConnection** : Pour toute base via ODBC

#### Propriétés essentielles d'une connexion

```pascal
// Exemple avec MySQL
MySQL57Connection1.HostName := 'localhost';     // Serveur de base de données  
MySQL57Connection1.DatabaseName := 'ma_base';    // Nom de la base  
MySQL57Connection1.UserName := 'utilisateur';    // Nom d'utilisateur  
MySQL57Connection1.Password := 'motdepasse';     // Mot de passe  
MySQL57Connection1.Port := 3306;                 // Port (optionnel, valeur par défaut)
```

### 2. TSQLTransaction

La transaction gère l'intégrité de vos données. C'est comme une enveloppe de sécurité autour de vos opérations :

- Si tout se passe bien, on **valide** (commit) les changements
- Si un problème survient, on **annule** (rollback) tout

#### Configuration de base

```pascal
SQLTransaction1.Database := MySQL57Connection1;  // Lier à la connexion  
SQLTransaction1.Action := caCommit;              // Action par défaut
```

#### Pourquoi les transactions sont importantes

Imaginez que vous transférez de l'argent entre deux comptes bancaires :
1. Retirer de l'argent du compte A
2. Ajouter de l'argent au compte B

Si l'étape 2 échoue, vous voulez annuler l'étape 1. C'est le rôle de la transaction.

### 3. TSQLQuery

C'est le composant qui exécute vos requêtes SQL et récupère les résultats.

#### Propriétés principales

```pascal
SQLQuery1.Database := MySQL57Connection1;        // Lien vers la connexion  
SQLQuery1.Transaction := SQLTransaction1;        // Lien vers la transaction  
SQLQuery1.SQL.Text := 'SELECT * FROM clients';  // La requête SQL
```

#### Types d'utilisation

**Pour lire des données (SELECT) :**
```pascal
SQLQuery1.SQL.Text := 'SELECT nom, prenom FROM clients';  
SQLQuery1.Open;  // Ouvre et exécute la requête
```

**Pour modifier des données (INSERT, UPDATE, DELETE) :**
```pascal
SQLQuery1.SQL.Text := 'UPDATE clients SET ville = :ville WHERE id = :id';  
SQLQuery1.ParamByName('ville').AsString := 'Paris';  
SQLQuery1.ParamByName('id').AsInteger := 1;  
SQLQuery1.ExecSQL;  // Exécute sans retourner de résultats
```

### 4. TSQLScript

Pour exécuter plusieurs commandes SQL d'un coup (comme un script de création de base de données).

```pascal
SQLScript1.Database := MySQL57Connection1;  
SQLScript1.Transaction := SQLTransaction1;  
SQLScript1.Script.Text :=
  'CREATE TABLE clients (id INT, nom VARCHAR(50));' + LineEnding +
  'CREATE TABLE commandes (id INT, client_id INT);';
SQLScript1.Execute;
```

### 5. Composants de liaison de données

Ces composants créent le pont entre les données et l'interface utilisateur :

#### TDataSource

Fait le lien entre une requête et les composants visuels :

```pascal
DataSource1.DataSet := SQLQuery1;
```

#### Composants visuels data-aware

- **TDBGrid** : Affiche les données sous forme de tableau
- **TDBEdit** : Champ de saisie lié à une colonne
- **TDBMemo** : Zone de texte multiligne
- **TDBComboBox** : Liste déroulante liée aux données
- **TDBNavigator** : Barre de navigation dans les enregistrements

## Organisation typique des composants

Voici comment les composants s'organisent généralement dans une application :

```
TSQLConnection (ex: TPQConnection)
    ↓ (propriété Database)
TSQLTransaction
    ↓ (propriété Transaction)
TSQLQuery
    ↓ (propriété DataSet)
TDataSource
    ↓ (propriété DataSource)
Composants visuels (TDBGrid, TDBEdit, etc.)
```

## Cycle de vie d'une opération base de données

### 1. Connexion

```pascal
// Configurer la connexion
MySQL57Connection1.HostName := 'localhost';  
MySQL57Connection1.DatabaseName := 'ma_base';  
MySQL57Connection1.UserName := 'user';  
MySQL57Connection1.Password := 'pass';

// Se connecter
MySQL57Connection1.Connected := True;
```

### 2. Préparation de la requête

```pascal
SQLQuery1.Close;  // Fermer si déjà ouverte  
SQLQuery1.SQL.Clear;  
SQLQuery1.SQL.Text := 'SELECT * FROM produits WHERE prix < :prix_max';  
SQLQuery1.ParamByName('prix_max').AsFloat := 100.00;
```

### 3. Exécution

```pascal
// Pour une requête SELECT
SQLQuery1.Open;

// Pour INSERT/UPDATE/DELETE
SQLQuery1.ExecSQL;
```

### 4. Traitement des résultats

```pascal
// Parcourir les résultats
while not SQLQuery1.EOF do  
begin
  ShowMessage(SQLQuery1.FieldByName('nom').AsString);
  SQLQuery1.Next;
end;
```

### 5. Validation ou annulation

```pascal
// Valider les changements
SQLTransaction1.Commit;

// Ou annuler en cas d'erreur
SQLTransaction1.Rollback;
```

### 6. Fermeture

```pascal
SQLQuery1.Close;  
MySQL57Connection1.Connected := False;
```

## Gestion des erreurs

Il est important de gérer les erreurs pour éviter les plantages :

```pascal
try
  MySQL57Connection1.Connected := True;
  SQLQuery1.Open;
  // Traitement des données
  SQLTransaction1.Commit;
except
  on E: Exception do
  begin
    ShowMessage('Erreur : ' + E.Message);
    SQLTransaction1.Rollback;
  end;
end;
```

## Paramètres et sécurité

### Pourquoi utiliser des paramètres ?

Les paramètres protègent contre les injections SQL et améliorent les performances :

**Mauvaise pratique (dangereux) :**
```pascal
SQLQuery1.SQL.Text := 'SELECT * FROM users WHERE login = ''' + Edit1.Text + '''';
```

**Bonne pratique (sécurisé) :**
```pascal
SQLQuery1.SQL.Text := 'SELECT * FROM users WHERE login = :login';  
SQLQuery1.ParamByName('login').AsString := Edit1.Text;
```

## Méta-données et informations de schéma

SQLdb permet d'obtenir des informations sur la structure de la base :

```pascal
// Obtenir la liste des tables
MySQL57Connection1.GetTableNames(ListBox1.Items);

// Obtenir la liste des champs d'une table
MySQL57Connection1.GetFieldNames('clients', ListBox2.Items);
```

## Optimisation et bonnes pratiques

### 1. Préparer les requêtes réutilisables

```pascal
SQLQuery1.Prepare;  // Prépare la requête une seule fois
// Utilisation multiple avec différents paramètres
for i := 1 to 100 do  
begin
  SQLQuery1.ParamByName('id').AsInteger := i;
  SQLQuery1.Open;
  // Traitement
  SQLQuery1.Close;
end;
```

### 2. Utiliser les transactions appropriées

- **Lecture seule** : Pour les SELECT simples
- **Lecture/Écriture** : Pour les modifications

```pascal
SQLTransaction1.Options := [stoUseImplicitTransaction];  // Transaction implicite
```

### 3. Libérer les ressources

Toujours fermer les requêtes et connexions non utilisées :

```pascal
try
  SQLQuery1.Open;
  // Utilisation
finally
  SQLQuery1.Close;
end;
```

## Configuration multi-plateforme

### Windows

Les DLL des clients de base de données doivent être présentes :
- **MySQL** : libmysql.dll
- **PostgreSQL** : libpq.dll
- **SQLite** : sqlite3.dll

Placer ces fichiers dans :
- Le répertoire de l'application
- Ou dans le PATH système

### Linux/Ubuntu

Installation des bibliothèques clientes :

```bash
# Pour MySQL
sudo apt-get install libmysqlclient-dev

# Pour PostgreSQL
sudo apt-get install libpq-dev

# Pour SQLite
sudo apt-get install libsqlite3-dev
```

## Modes de travail avec les données

### Mode connecté

Les données sont lues directement depuis la base :

```pascal
SQLQuery1.Open;
// Les données sont lues au fur et à mesure
while not SQLQuery1.EOF do  
begin
  ProcessRecord(SQLQuery1);
  SQLQuery1.Next;
end;
```

### Mode déconnecté (cache local)

Toutes les données sont chargées en mémoire :

```pascal
SQLQuery1.PacketRecords := -1;  // Charger tous les enregistrements  
SQLQuery1.Open;
// Travail en local, sans connexion active
```

## Exemples de configurations types

### Configuration SQLite (base embarquée)

```pascal
// SQLite ne nécessite pas de serveur
SQLite3Connection1.DatabaseName := 'data.db';  // Fichier local  
SQLite3Connection1.Connected := True;
// Pas besoin de username/password
```

### Configuration MySQL

```pascal
MySQL57Connection1.HostName := 'localhost';  
MySQL57Connection1.Port := 3306;  
MySQL57Connection1.DatabaseName := 'mabase';  
MySQL57Connection1.UserName := 'root';  
MySQL57Connection1.Password := 'motdepasse';  
MySQL57Connection1.Connected := True;
```

### Configuration PostgreSQL

```pascal
PQConnection1.HostName := 'localhost';  
PQConnection1.Port := 5432;  
PQConnection1.DatabaseName := 'mabase';  
PQConnection1.UserName := 'postgres';  
PQConnection1.Password := 'motdepasse';  
PQConnection1.Connected := True;
```

## Diagnostic et débogage

### Voir les requêtes SQL exécutées

```pascal
// Activer le log SQL
SQLQuery1.Database.LogEvents := [detActualSQL, detParamValue];  
SQLQuery1.Database.OnLog := @DatabaseLog;

procedure TForm1.DatabaseLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);  
begin
  Memo1.Lines.Add(Msg);  // Afficher dans un mémo
end;
```

## Conclusion

SQLdb offre une approche unifiée et portable pour travailler avec les bases de données dans Lazarus. Ses composants suivent une logique claire :

1. **Connection** établit le lien
2. **Transaction** sécurise les opérations
3. **Query** exécute les requêtes
4. **DataSource** lie aux composants visuels

Cette architecture permet de créer des applications robustes qui fonctionnent aussi bien sur Windows que sur Linux/Ubuntu, avec différents SGBD, tout en gardant le même code source.

La clé est de bien comprendre le rôle de chaque composant et de respecter l'ordre des opérations : connexion → transaction → requête → traitement → validation/annulation → fermeture.

⏭️ [Connexions natives identiques sur les deux OS](/08-bases-donnees-orm-multiplatefomes/02-connexions-natives-identiques.md)
