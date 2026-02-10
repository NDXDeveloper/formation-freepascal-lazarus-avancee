🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.11 NoSQL avec MongoDB et Redis

## Introduction

Les bases de données NoSQL (Not Only SQL) offrent des alternatives aux bases de données relationnelles traditionnelles comme PostgreSQL ou MySQL. Elles sont particulièrement adaptées pour certains types d'applications modernes.

Dans ce chapitre, nous allons explorer comment utiliser **MongoDB** (base orientée documents) et **Redis** (base clé-valeur en mémoire) avec FreePascal/Lazarus, sur Windows et Ubuntu.

## Qu'est-ce que le NoSQL ?

### Analogie simple

**Base de données SQL (relationnelle) :**
Imaginez une bibliothèque très organisée avec des fiches cartonnées. Chaque livre doit avoir exactement les mêmes informations : titre, auteur, ISBN, date... Tout est structuré dans des cases bien définies.

**Base de données NoSQL :**
Imaginez un classeur où vous pouvez ranger n'importe quel type de document : des livres complets, des photos, des notes manuscrites, des articles de journaux. Chaque document peut avoir sa propre structure.

### Différences fondamentales

| Aspect | SQL | NoSQL |
|--------|-----|-------|
| Structure | Tables rigides avec schéma fixe | Documents flexibles, schéma dynamique |
| Relations | Jointures entre tables | Documents imbriqués ou références |
| Scalabilité | Verticale (serveur plus puissant) | Horizontale (plus de serveurs) |
| Consistance | ACID (forte) | BASE (éventuelle) |
| Utilisation | Données structurées, transactions | Données non structurées, big data |

## Quand utiliser NoSQL ?

✅ **Utilisez NoSQL quand :**
- Vos données n'ont pas de structure fixe
- Vous avez besoin d'une scalabilité horizontale massive
- Vous stockez des documents JSON/XML complexes
- Vous avez besoin de très hautes performances en lecture
- Votre schéma change fréquemment

❌ **Préférez SQL quand :**
- Vous avez besoin de transactions complexes
- Vos données sont fortement relationnelles
- Vous avez besoin de consistance forte
- Vous utilisez beaucoup de jointures complexes

## MongoDB : Base de données orientée documents

### Qu'est-ce que MongoDB ?

MongoDB stocke les données sous forme de **documents JSON** (en réalité BSON - Binary JSON). C'est comme avoir une énorme collection de fichiers JSON organisés.

**Exemple de document MongoDB :**

```json
{
  "_id": "507f1f77bcf86cd799439011",
  "nom": "Dupont",
  "prenom": "Jean",
  "age": 30,
  "email": "jean.dupont@example.com",
  "adresses": [
    {
      "type": "domicile",
      "rue": "123 Rue de la Paix",
      "ville": "Paris",
      "cp": "75001"
    },
    {
      "type": "travail",
      "rue": "456 Avenue des Champs",
      "ville": "Paris",
      "cp": "75008"
    }
  ],
  "competences": ["Pascal", "Python", "JavaScript"],
  "date_inscription": "2024-01-15T10:30:00Z"
}
```

### Concepts MongoDB

#### Collections et Documents

- **Base de données** : Conteneur principal (équivalent d'une base SQL)
- **Collection** : Groupe de documents (équivalent d'une table SQL)
- **Document** : Enregistrement individuel (équivalent d'une ligne SQL)

```
Base de données: mon_app
├── Collection: clients
│   ├── Document 1 (Jean Dupont)
│   ├── Document 2 (Marie Martin)
│   └── Document 3 (Pierre Durand)
├── Collection: produits
│   ├── Document 1 (Ordinateur)
│   └── Document 2 (Souris)
└── Collection: commandes
    ├── Document 1 (Commande #1001)
    └── Document 2 (Commande #1002)
```

### Installation MongoDB

#### Sur Ubuntu

```bash
# Importer la clé GPG publique
wget -qO - https://www.mongodb.org/static/pgp/server-7.0.asc | sudo apt-key add -

# Ajouter le dépôt MongoDB
echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu jammy/mongodb-org/7.0 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-7.0.list

# Mettre à jour et installer
sudo apt-get update  
sudo apt-get install -y mongodb-org

# Démarrer MongoDB
sudo systemctl start mongod  
sudo systemctl enable mongod

# Vérifier le statut
sudo systemctl status mongod

# Se connecter au shell MongoDB
mongosh
```

#### Sur Windows

```powershell
# Télécharger l'installeur depuis mongodb.com
# Ou utiliser Chocolatey
choco install mongodb

# Créer les répertoires de données
mkdir C:\data\db

# Démarrer MongoDB
mongod

# Dans un autre terminal, se connecter
mongosh
```

### Utilisation de MongoDB avec FreePascal

MongoDB n'a pas de bibliothèque native pour FreePascal, mais nous pouvons utiliser l'**API REST** de MongoDB ou créer des **bindings vers la bibliothèque C**.

#### Option 1 : Via l'API REST (Simple)

```pascal
unit MongoDBClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser;

type
  TMongoClient = class
  private
    FBaseURL: String;
    FDatabase: String;
    FHTTPClient: TFPHTTPClient;
  public
    constructor Create(const AHost: String; APort: Integer; const ADatabase: String);
    destructor Destroy; override;

    function InsertDocument(const Collection, JSONDoc: String): String;
    function FindDocuments(const Collection: String; const Filter: String = '{}'): TJSONArray;
    function UpdateDocument(const Collection, Filter, Update: String): Boolean;
    function DeleteDocument(const Collection, Filter: String): Boolean;
    function CountDocuments(const Collection: String): Integer;
  end;

implementation

constructor TMongoClient.Create(const AHost: String; APort: Integer; const ADatabase: String);  
begin
  FBaseURL := Format('http://%s:%d', [AHost, APort]);
  FDatabase := ADatabase;
  FHTTPClient := TFPHTTPClient.Create(nil);
end;

destructor TMongoClient.Destroy;  
begin
  FHTTPClient.Free;
  inherited;
end;

function TMongoClient.InsertDocument(const Collection, JSONDoc: String): String;  
var
  URL: String;
  Response: String;
  JSONResponse: TJSONObject;
begin
  URL := Format('%s/%s/%s', [FBaseURL, FDatabase, Collection]);

  try
    FHTTPClient.RequestBody := TStringStream.Create(JSONDoc);
    FHTTPClient.AddHeader('Content-Type', 'application/json');

    Response := FHTTPClient.Post(URL);

    // Parser la réponse pour obtenir l'ID
    JSONResponse := TJSONObject(GetJSON(Response));
    try
      Result := JSONResponse.Get('_id', '');
    finally
      JSONResponse.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur insertion: ', E.Message);
      Result := '';
    end;
  end;
end;

function TMongoClient.FindDocuments(const Collection: String; const Filter: String = '{}'): TJSONArray;  
var
  URL: String;
  Response: String;
begin
  URL := Format('%s/%s/%s?filter=%s', [FBaseURL, FDatabase, Collection, Filter]);

  try
    Response := FHTTPClient.Get(URL);
    Result := TJSONArray(GetJSON(Response));
  except
    on E: Exception do
    begin
      WriteLn('Erreur recherche: ', E.Message);
      Result := TJSONArray.Create;
    end;
  end;
end;

function TMongoClient.UpdateDocument(const Collection, Filter, Update: String): Boolean;  
var
  URL: String;
  Response: String;
begin
  URL := Format('%s/%s/%s', [FBaseURL, FDatabase, Collection]);

  try
    FHTTPClient.RequestBody := TStringStream.Create(
      Format('{"filter":%s,"update":%s}', [Filter, Update])
    );
    FHTTPClient.AddHeader('Content-Type', 'application/json');

    Response := FHTTPClient.Patch(URL);
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Erreur mise à jour: ', E.Message);
      Result := False;
    end;
  end;
end;

function TMongoClient.DeleteDocument(const Collection, Filter: String): Boolean;  
var
  URL: String;
begin
  URL := Format('%s/%s/%s?filter=%s', [FBaseURL, FDatabase, Collection, Filter]);

  try
    FHTTPClient.Delete(URL);
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Erreur suppression: ', E.Message);
      Result := False;
    end;
  end;
end;

function TMongoClient.CountDocuments(const Collection: String): Integer;  
var
  URL: String;
  Response: String;
  JSONResponse: TJSONObject;
begin
  URL := Format('%s/%s/%s/count', [FBaseURL, FDatabase, Collection]);

  try
    Response := FHTTPClient.Get(URL);
    JSONResponse := TJSONObject(GetJSON(Response));
    try
      Result := JSONResponse.Get('count', 0);
    finally
      JSONResponse.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur comptage: ', E.Message);
      Result := 0;
    end;
  end;
end;

end.
```

#### Utilisation du client MongoDB

```pascal
program MongoExample;

{$mode objfpc}{$H+}

uses
  SysUtils, MongoDBClient, fpjson;

var
  Mongo: TMongoClient;
  DocumentID: String;
  Documents: TJSONArray;
  i: Integer;

begin
  // Connexion à MongoDB (local)
  Mongo := TMongoClient.Create('localhost', 27017, 'mon_app');
  try
    WriteLn('=== Test MongoDB avec FreePascal ===');
    WriteLn;

    // 1. Insérer un document
    WriteLn('1. Insertion d''un client...');
    DocumentID := Mongo.InsertDocument('clients',
      '{"nom":"Dupont","prenom":"Jean","age":30,"email":"jean@example.com"}');

    if DocumentID <> '' then
      WriteLn('✅ Document inséré avec ID: ', DocumentID)
    else
      WriteLn('❌ Échec insertion');
    WriteLn;

    // 2. Insérer plusieurs documents
    WriteLn('2. Insertion de plusieurs clients...');
    Mongo.InsertDocument('clients',
      '{"nom":"Martin","prenom":"Marie","age":25,"email":"marie@example.com"}');
    Mongo.InsertDocument('clients',
      '{"nom":"Durand","prenom":"Pierre","age":35,"email":"pierre@example.com"}');
    WriteLn('✅ Clients insérés');
    WriteLn;

    // 3. Compter les documents
    WriteLn('3. Nombre de clients: ', Mongo.CountDocuments('clients'));
    WriteLn;

    // 4. Rechercher tous les documents
    WriteLn('4. Liste de tous les clients:');
    Documents := Mongo.FindDocuments('clients');
    try
      for i := 0 to Documents.Count - 1 do
      begin
        with TJSONObject(Documents[i]) do
        begin
          WriteLn(Format('  - %s %s (%d ans) - %s', [
            Get('prenom', ''),
            Get('nom', ''),
            Get('age', 0),
            Get('email', '')
          ]));
        end;
      end;
    finally
      Documents.Free;
    end;
    WriteLn;

    // 5. Rechercher avec filtre
    WriteLn('5. Clients de plus de 30 ans:');
    Documents := Mongo.FindDocuments('clients', '{"age":{"$gt":30}}');
    try
      WriteLn('  Trouvé ', Documents.Count, ' client(s)');
    finally
      Documents.Free;
    end;
    WriteLn;

    // 6. Mettre à jour un document
    WriteLn('6. Mise à jour de l''âge de Jean...');
    if Mongo.UpdateDocument('clients',
      '{"nom":"Dupont"}',
      '{"$set":{"age":31}}') then
      WriteLn('✅ Document mis à jour')
    else
      WriteLn('❌ Échec mise à jour');
    WriteLn;

    // 7. Supprimer un document
    WriteLn('7. Suppression d''un client...');
    if Mongo.DeleteDocument('clients', '{"nom":"Durand"}') then
      WriteLn('✅ Document supprimé')
    else
      WriteLn('❌ Échec suppression');
    WriteLn;

    // Vérifier le nouveau total
    WriteLn('Nombre final de clients: ', Mongo.CountDocuments('clients'));

  finally
    Mongo.Free;
  end;
end.
```

### Opérations avancées MongoDB

#### Recherches complexes

```pascal
procedure ExemplesRecherchesComplexes(Mongo: TMongoClient);  
var
  Documents: TJSONArray;
begin
  // Recherche avec opérateurs de comparaison
  WriteLn('Clients entre 25 et 35 ans:');
  Documents := Mongo.FindDocuments('clients',
    '{"age":{"$gte":25,"$lte":35}}');
  Documents.Free;

  // Recherche avec OR
  WriteLn('Clients nommés Dupont OU Martin:');
  Documents := Mongo.FindDocuments('clients',
    '{"$or":[{"nom":"Dupont"},{"nom":"Martin"}]}');
  Documents.Free;

  // Recherche dans un tableau
  WriteLn('Clients avec compétence "Pascal":');
  Documents := Mongo.FindDocuments('clients',
    '{"competences":"Pascal"}');
  Documents.Free;

  // Recherche avec regex
  WriteLn('Clients dont l''email contient "example.com":');
  Documents := Mongo.FindDocuments('clients',
    '{"email":{"$regex":"example\\.com"}}');
  Documents.Free;

  // Recherche dans un sous-document
  WriteLn('Clients habitant à Paris:');
  Documents := Mongo.FindDocuments('clients',
    '{"adresses.ville":"Paris"}');
  Documents.Free;
end;
```

#### Agrégations

Les agrégations permettent de traiter et transformer les données (équivalent de GROUP BY en SQL).

```pascal
function AggregateClients(Mongo: TMongoClient): TJSONArray;  
var
  Pipeline: String;
begin
  // Pipeline d'agrégation : grouper par ville et compter
  Pipeline := '[' +
    '{"$unwind":"$adresses"},' +
    '{"$group":{' +
      '"_id":"$adresses.ville",' +
      '"total":{"$sum":1},' +
      '"age_moyen":{"$avg":"$age"}' +
    '}},' +
    '{"$sort":{"total":-1}}' +
  ']';

  // Exécuter l'agrégation
  Result := Mongo.Aggregate('clients', Pipeline);
end;
```

### Structure de données recommandée

#### Modèle embarqué (Embedded)

Pour les données qui vont toujours ensemble :

```json
{
  "_id": "...",
  "nom": "Dupont",
  "commandes": [
    {
      "numero": "CMD-001",
      "date": "2024-01-15",
      "montant": 150.00,
      "articles": [
        {"nom": "Souris", "prix": 25.00},
        {"nom": "Clavier", "prix": 125.00}
      ]
    },
    {
      "numero": "CMD-002",
      "date": "2024-01-20",
      "montant": 50.00,
      "articles": [
        {"nom": "Tapis de souris", "prix": 50.00}
      ]
    }
  ]
}
```

✅ **Avantages :**
- Une seule requête pour tout récupérer
- Pas de jointures
- Performances excellentes

❌ **Inconvénients :**
- Duplication de données
- Limite de taille du document (16 MB)

#### Modèle référencé (Referenced)

Pour les données indépendantes :

```json
// Collection: clients
{
  "_id": "client_123",
  "nom": "Dupont"
}

// Collection: commandes
{
  "_id": "cmd_001",
  "client_id": "client_123",
  "date": "2024-01-15",
  "montant": 150.00
}
```

✅ **Avantages :**
- Pas de duplication
- Flexibilité

❌ **Inconvénients :**
- Plusieurs requêtes nécessaires
- Pas de jointures natives (avant MongoDB 3.2)

## Redis : Base de données clé-valeur en mémoire

### Qu'est-ce que Redis ?

Redis (REmote DIctionary Server) est une base de données **en mémoire** ultra-rapide qui stocke des paires clé-valeur. C'est comme un énorme dictionnaire ou HashMap géant.

**Caractéristiques :**
- Extrêmement rapide (< 1ms par opération)
- Stockage en RAM (avec persistance optionnelle sur disque)
- Support de structures de données avancées
- Pub/Sub (publication/souscription)
- Expiration automatique des clés

### Cas d'usage de Redis

✅ **Redis est parfait pour :**
- Cache d'application
- Sessions utilisateur
- Files d'attente (queues)
- Compteurs temps réel
- Leaderboards/classements
- Rate limiting
- Pub/Sub messaging

### Installation Redis

#### Sur Ubuntu

```bash
# Installer Redis
sudo apt-get update  
sudo apt-get install redis-server

# Configurer Redis pour démarrer automatiquement
sudo systemctl enable redis-server  
sudo systemctl start redis-server

# Vérifier le statut
sudo systemctl status redis-server

# Tester la connexion
redis-cli ping
# Doit répondre: PONG

# Se connecter au CLI
redis-cli
```

#### Sur Windows

```powershell
# Via Chocolatey
choco install redis-64

# Ou télécharger depuis GitHub:
# https://github.com/microsoftarchive/redis/releases

# Démarrer Redis
redis-server

# Dans un autre terminal, tester
redis-cli ping
```

### Configuration Redis

Fichier de configuration :
- Ubuntu : `/etc/redis/redis.conf`
- Windows : `redis.windows.conf`

**Paramètres importants :**

```ini
# Écouter sur toutes les interfaces (attention: sécurité!)
bind 0.0.0.0

# Port par défaut
port 6379

# Mot de passe (IMPORTANT en production)
requirepass mon_mot_de_passe_securise

# Persistance sur disque
save 900 1      # Sauvegarder après 900s si 1 clé modifiée  
save 300 10     # Sauvegarder après 300s si 10 clés modifiées  
save 60 10000   # Sauvegarder après 60s si 10000 clés modifiées

# Limite mémoire
maxmemory 256mb

# Politique d'éviction quand mémoire pleine
maxmemory-policy allkeys-lru
```

### Utilisation de Redis avec FreePascal

```pascal
unit RedisClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets;

type
  TRedisClient = class
  private
    FSocket: TInetSocket;
    FHost: String;
    FPort: Integer;
    FPassword: String;
    FConnected: Boolean;

    function SendCommand(const Command: String): String;
    function ReadResponse: String;
  public
    constructor Create(const AHost: String = 'localhost'; APort: Integer = 6379);
    destructor Destroy; override;

    function Connect: Boolean;
    procedure Disconnect;
    function Auth(const Password: String): Boolean;

    // Opérations de base
    function SetKey(const Key, Value: String): Boolean;
    function GetKey(const Key: String): String;
    function DeleteKey(const Key: String): Boolean;
    function Exists(const Key: String): Boolean;
    function Expire(const Key: String; Seconds: Integer): Boolean;
    function TTL(const Key: String): Integer;

    // Opérations sur les compteurs
    function Incr(const Key: String): Integer;
    function IncrBy(const Key: String; Increment: Integer): Integer;
    function Decr(const Key: String): Integer;

    // Opérations sur les listes
    function LPush(const Key, Value: String): Integer;
    function RPush(const Key, Value: String): Integer;
    function LPop(const Key: String): String;
    function RPop(const Key: String): String;
    function LLen(const Key: String): Integer;
    function LRange(const Key: String; Start, Stop: Integer): TStringList;

    // Opérations sur les ensembles (sets)
    function SAdd(const Key, Member: String): Boolean;
    function SRem(const Key, Member: String): Boolean;
    function SMembers(const Key: String): TStringList;
    function SCard(const Key: String): Integer;

    // Opérations sur les hashes
    function HSet(const Key, Field, Value: String): Boolean;
    function HGet(const Key, Field: String): String;
    function HGetAll(const Key: String): TStringList;
    function HDel(const Key, Field: String): Boolean;

    property Connected: Boolean read FConnected;
  end;

implementation

constructor TRedisClient.Create(const AHost: String; APort: Integer);  
begin
  FHost := AHost;
  FPort := APort;
  FConnected := False;
  FSocket := TInetSocket.Create(FHost, FPort);
end;

destructor TRedisClient.Destroy;  
begin
  if FConnected then
    Disconnect;
  FSocket.Free;
  inherited;
end;

function TRedisClient.Connect: Boolean;  
begin
  try
    FSocket.Connect;
    FConnected := True;
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Erreur connexion Redis: ', E.Message);
      Result := False;
    end;
  end;
end;

procedure TRedisClient.Disconnect;  
begin
  if FConnected then
  begin
    FSocket.Close;
    FConnected := False;
  end;
end;

function TRedisClient.Auth(const Password: String): Boolean;  
var
  Response: String;
begin
  Response := SendCommand('AUTH ' + Password);
  Result := Pos('OK', Response) > 0;
end;

function TRedisClient.SendCommand(const Command: String): String;  
begin
  if not FConnected then
  begin
    WriteLn('Erreur: Non connecté à Redis');
    Exit('');
  end;

  // Envoyer la commande
  FSocket.WriteString(Command + #13#10);

  // Lire la réponse
  Result := ReadResponse;
end;

function TRedisClient.ReadResponse: String;  
var
  Line: String;
  FirstChar: Char;
  Len: Integer;
begin
  Line := FSocket.ReadString;

  if Length(Line) = 0 then
    Exit('');

  FirstChar := Line[1];

  case FirstChar of
    '+': Result := Copy(Line, 2, Length(Line) - 1);  // Simple string
    '-': Result := 'ERROR: ' + Copy(Line, 2, Length(Line) - 1);  // Error
    ':': Result := Copy(Line, 2, Length(Line) - 1);  // Integer
    '$':  // Bulk string
    begin
      Len := StrToIntDef(Copy(Line, 2, Length(Line) - 1), -1);
      if Len = -1 then
        Result := ''
      else
        Result := FSocket.ReadString;
    end;
    '*': Result := Line;  // Array (traitement spécial nécessaire)
  else
    Result := Line;
  end;
end;

function TRedisClient.SetKey(const Key, Value: String): Boolean;  
var
  Response: String;
begin
  Response := SendCommand(Format('SET %s %s', [Key, Value]));
  Result := Pos('OK', Response) > 0;
end;

function TRedisClient.GetKey(const Key: String): String;  
begin
  Result := SendCommand('GET ' + Key);
end;

function TRedisClient.DeleteKey(const Key: String): Boolean;  
var
  Response: String;
begin
  Response := SendCommand('DEL ' + Key);
  Result := Response = '1';
end;

function TRedisClient.Exists(const Key: String): Boolean;  
var
  Response: String;
begin
  Response := SendCommand('EXISTS ' + Key);
  Result := Response = '1';
end;

function TRedisClient.Expire(const Key: String; Seconds: Integer): Boolean;  
var
  Response: String;
begin
  Response := SendCommand(Format('EXPIRE %s %d', [Key, Seconds]));
  Result := Response = '1';
end;

function TRedisClient.TTL(const Key: String): Integer;  
var
  Response: String;
begin
  Response := SendCommand('TTL ' + Key);
  Result := StrToIntDef(Response, -1);
end;

function TRedisClient.Incr(const Key: String): Integer;  
var
  Response: String;
begin
  Response := SendCommand('INCR ' + Key);
  Result := StrToIntDef(Response, 0);
end;

function TRedisClient.IncrBy(const Key: String; Increment: Integer): Integer;  
var
  Response: String;
begin
  Response := SendCommand(Format('INCRBY %s %d', [Key, Increment]));
  Result := StrToIntDef(Response, 0);
end;

function TRedisClient.Decr(const Key: String): Integer;  
var
  Response: String;
begin
  Response := SendCommand('DECR ' + Key);
  Result := StrToIntDef(Response, 0);
end;

function TRedisClient.LPush(const Key, Value: String): Integer;  
var
  Response: String;
begin
  Response := SendCommand(Format('LPUSH %s %s', [Key, Value]));
  Result := StrToIntDef(Response, 0);
end;

function TRedisClient.RPush(const Key, Value: String): Integer;  
var
  Response: String;
begin
  Response := SendCommand(Format('RPUSH %s %s', [Key, Value]));
  Result := StrToIntDef(Response, 0);
end;

function TRedisClient.LPop(const Key: String): String;  
begin
  Result := SendCommand('LPOP ' + Key);
end;

function TRedisClient.RPop(const Key: String): String;  
begin
  Result := SendCommand('RPOP ' + Key);
end;

function TRedisClient.LLen(const Key: String): Integer;  
var
  Response: String;
begin
  Response := SendCommand('LLEN ' + Key);
  Result := StrToIntDef(Response, 0);
end;

function TRedisClient.LRange(const Key: String; Start, Stop: Integer): TStringList;  
begin
  // Implémentation simplifiée
  Result := TStringList.Create;
  // Parser la réponse array de Redis
end;

function TRedisClient.SAdd(const Key, Member: String): Boolean;  
var
  Response: String;
begin
  Response := SendCommand(Format('SADD %s %s', [Key, Member]));
  Result := Response = '1';
end;

function TRedisClient.SRem(const Key, Member: String): Boolean;  
var
  Response: String;
begin
  Response := SendCommand(Format('SREM %s %s', [Key, Member]));
  Result := Response = '1';
end;

function TRedisClient.SMembers(const Key: String): TStringList;  
begin
  Result := TStringList.Create;
  // Parser la réponse array de Redis
end;

function TRedisClient.SCard(const Key: String): Integer;  
var
  Response: String;
begin
  Response := SendCommand('SCARD ' + Key);
  Result := StrToIntDef(Response, 0);
end;

function TRedisClient.HSet(const Key, Field, Value: String): Boolean;  
var
  Response: String;
begin
  Response := SendCommand(Format('HSET %s %s %s', [Key, Field, Value]));
  Result := Pos('1', Response) > 0;
end;

function TRedisClient.HGet(const Key, Field: String): String;  
begin
  Result := SendCommand(Format('HGET %s %s', [Key, Field]));
end;

function TRedisClient.HGetAll(const Key: String): TStringList;  
begin
  Result := TStringList.Create;
  // Parser la réponse array de Redis
end;

function TRedisClient.HDel(const Key, Field: String): Boolean;  
var
  Response: String;
begin
  Response := SendCommand(Format('HDEL %s %s', [Key, Field]));
  Result := Response = '1';
end;

end.
```

## Exemples d'utilisation Redis

```pascal
program RedisExample;

{$mode objfpc}{$H+}

uses
  SysUtils, RedisClient;

var
  Redis: TRedisClient;
  Value: String;
  Counter: Integer;

begin
  WriteLn('=== Test Redis avec FreePascal ===');
  WriteLn;

  // Connexion à Redis
  Redis := TRedisClient.Create('localhost', 6379);
  try
    if not Redis.Connect then
    begin
      WriteLn('❌ Impossible de se connecter à Redis');
      Exit;
    end;

    WriteLn('✅ Connecté à Redis');
    WriteLn;

    // === 1. Opérations simples clé-valeur ===
    WriteLn('1. Opérations clé-valeur de base:');

    // Définir une valeur
    if Redis.SetKey('nom', 'Jean Dupont') then
      WriteLn('  ✅ Clé "nom" définie');

    // Récupérer une valeur
    Value := Redis.GetKey('nom');
    WriteLn('  Valeur récupérée: ', Value);

    // Vérifier l'existence
    if Redis.Exists('nom') then
      WriteLn('  ✅ La clé "nom" existe');

    // Définir une expiration (TTL)
    Redis.SetKey('session_123', 'donnees_session');
    Redis.Expire('session_123', 3600);  // Expire dans 1 heure
    WriteLn('  ⏱️  Session expire dans: ', Redis.TTL('session_123'), ' secondes');

    WriteLn;

    // === 2. Compteurs ===
    WriteLn('2. Compteurs:');

    // Incrémenter
    Counter := Redis.Incr('visiteurs');
    WriteLn('  Visiteurs: ', Counter);

    Counter := Redis.Incr('visiteurs');
    WriteLn('  Visiteurs: ', Counter);

    Counter := Redis.IncrBy('visiteurs', 5);
    WriteLn('  Visiteurs (+5): ', Counter);

    Counter := Redis.Decr('visiteurs');
    WriteLn('  Visiteurs (-1): ', Counter);

    WriteLn;

    // === 3. Listes (files d'attente) ===
    WriteLn('3. Listes (files d''attente):');

    // Créer une file de tâches
    Redis.RPush('taches', 'Tâche 1');
    Redis.RPush('taches', 'Tâche 2');
    Redis.RPush('taches', 'Tâche 3');
    WriteLn('  ✅ 3 tâches ajoutées à la file');

    WriteLn('  Nombre de tâches: ', Redis.LLen('taches'));

    // Traiter les tâches (FIFO)
    Value := Redis.LPop('taches');
    WriteLn('  Tâche traitée: ', Value);

    WriteLn('  Tâches restantes: ', Redis.LLen('taches'));

    WriteLn;

    // === 4. Ensembles (Sets) ===
    WriteLn('4. Ensembles (sets):');

    // Créer un ensemble d'utilisateurs en ligne
    Redis.SAdd('utilisateurs_en_ligne', 'user_123');
    Redis.SAdd('utilisateurs_en_ligne', 'user_456');
    Redis.SAdd('utilisateurs_en_ligne', 'user_789');
    Redis.SAdd('utilisateurs_en_ligne', 'user_123');  // Doublon ignoré

    WriteLn('  Utilisateurs en ligne: ', Redis.SCard('utilisateurs_en_ligne'));

    // Retirer un utilisateur
    Redis.SRem('utilisateurs_en_ligne', 'user_456');
    WriteLn('  Après déconnexion: ', Redis.SCard('utilisateurs_en_ligne'));

    WriteLn;

    // === 5. Hashes (objets) ===
    WriteLn('5. Hashes (objets):');

    // Stocker un utilisateur comme hash
    Redis.HSet('user:1000', 'nom', 'Dupont');
    Redis.HSet('user:1000', 'prenom', 'Jean');
    Redis.HSet('user:1000', 'email', 'jean@example.com');
    Redis.HSet('user:1000', 'age', '30');
    WriteLn('  ✅ Utilisateur 1000 créé');

    // Récupérer un champ
    Value := Redis.HGet('user:1000', 'email');
    WriteLn('  Email: ', Value);

    WriteLn;

    // === 6. Nettoyage ===
    WriteLn('6. Nettoyage:');
    Redis.DeleteKey('nom');
    Redis.DeleteKey('visiteurs');
    WriteLn('  ✅ Clés de test supprimées');

  finally
    Redis.Disconnect;
    Redis.Free;
  end;

  WriteLn;
  WriteLn('=== Fin des tests ===');
end.
```

## Cas d'usage pratiques avec Redis

### 1. Système de cache

Redis est excellent comme couche de cache devant une base de données SQL.

```pascal
unit CacheManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, RedisClient;

type
  TCacheManager = class
  private
    FRedis: TRedisClient;
    FDatabase: TSQLConnection;
    FTTL: Integer;  // Durée de vie du cache en secondes
  public
    constructor Create(ARedis: TRedisClient; ADatabase: TSQLConnection; ATTL: Integer = 300);

    function GetUser(UserID: Integer): String;
    procedure InvalidateUser(UserID: Integer);
    procedure InvalidateAll;
  end;

implementation

uses
  fpjson, jsonparser;

constructor TCacheManager.Create(ARedis: TRedisClient; ADatabase: TSQLConnection; ATTL: Integer);  
begin
  FRedis := ARedis;
  FDatabase := ADatabase;
  FTTL := ATTL;
end;

function TCacheManager.GetUser(UserID: Integer): String;  
var
  CacheKey: String;
  CachedData: String;
  Query: TSQLQuery;
  JSONObj: TJSONObject;
begin
  CacheKey := Format('user:%d', [UserID]);

  // 1. Vérifier le cache Redis
  CachedData := FRedis.GetKey(CacheKey);

  if CachedData <> '' then
  begin
    WriteLn('✅ Cache HIT pour user ', UserID);
    Exit(CachedData);
  end;

  // 2. Cache MISS - Récupérer depuis la base de données
  WriteLn('⚠️  Cache MISS pour user ', UserID, ' - Requête BDD');

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabase;
    Query.SQL.Text := 'SELECT id, nom, prenom, email FROM users WHERE id = :id';
    Query.Params.ParamByName('id').AsInteger := UserID;
    Query.Open;

    if not Query.EOF then
    begin
      // Construire un objet JSON
      JSONObj := TJSONObject.Create;
      try
        JSONObj.Add('id', Query.FieldByName('id').AsInteger);
        JSONObj.Add('nom', Query.FieldByName('nom').AsString);
        JSONObj.Add('prenom', Query.FieldByName('prenom').AsString);
        JSONObj.Add('email', Query.FieldByName('email').AsString);

        Result := JSONObj.AsJSON;

        // 3. Mettre en cache avec expiration
        FRedis.SetKey(CacheKey, Result);
        FRedis.Expire(CacheKey, FTTL);

        WriteLn('💾 Données mises en cache (TTL: ', FTTL, 's)');
      finally
        JSONObj.Free;
      end;
    end
    else
      Result := '';

    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TCacheManager.InvalidateUser(UserID: Integer);  
var
  CacheKey: String;
begin
  CacheKey := Format('user:%d', [UserID]);
  FRedis.DeleteKey(CacheKey);
  WriteLn('🗑️  Cache invalidé pour user ', UserID);
end;

procedure TCacheManager.InvalidateAll;  
begin
  // Note: Redis FLUSHDB supprime TOUTE la base
  // En production, utiliser un préfixe et SCAN pour supprimer sélectivement
  WriteLn('🗑️  Cache complet invalidé');
end;

end.
```

**Utilisation du cache :**

```pascal
var
  Redis: TRedisClient;
  DB: TSQLConnection;
  Cache: TCacheManager;
  UserData: String;
  i: Integer;

begin
  Redis := TRedisClient.Create;
  Redis.Connect;

  // Initialiser la connexion DB...

  Cache := TCacheManager.Create(Redis, DB, 300);  // Cache 5 minutes
  try
    // Première requête - va en base de données
    UserData := Cache.GetUser(123);
    WriteLn('Données: ', UserData);

    // Deuxième requête - vient du cache
    UserData := Cache.GetUser(123);
    WriteLn('Données: ', UserData);

    // L'utilisateur est modifié dans la base
    // ... UPDATE users SET nom = 'Nouveau' WHERE id = 123 ...

    // Invalider le cache
    Cache.InvalidateUser(123);

    // Prochaine requête va recharger depuis la BDD
    UserData := Cache.GetUser(123);
  finally
    Cache.Free;
  end;
end.
```

### 2. Gestion de sessions

```pascal
unit SessionManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RedisClient, fpjson;

type
  TSessionManager = class
  private
    FRedis: TRedisClient;
    FSessionTTL: Integer;

    function GenerateSessionID: String;
  public
    constructor Create(ARedis: TRedisClient; ASessionTTL: Integer = 3600);

    function CreateSession(UserID: Integer; const UserData: String): String;
    function GetSession(const SessionID: String): TJSONObject;
    function UpdateSession(const SessionID: String; const Key, Value: String): Boolean;
    function DestroySession(const SessionID: String): Boolean;
    function RefreshSession(const SessionID: String): Boolean;
    function IsSessionValid(const SessionID: String): Boolean;
  end;

implementation

uses
  MD5;

constructor TSessionManager.Create(ARedis: TRedisClient; ASessionTTL: Integer);  
begin
  FRedis := ARedis;
  FSessionTTL := ASessionTTL;
end;

function TSessionManager.GenerateSessionID: String;  
begin
  // Générer un ID de session unique
  Result := MD5Print(MD5String(
    FormatDateTime('yyyymmddhhnnsszzz', Now) +
    IntToStr(Random(999999))
  ));
end;

function TSessionManager.CreateSession(UserID: Integer; const UserData: String): String;  
var
  SessionID, SessionKey: String;
begin
  SessionID := GenerateSessionID;
  SessionKey := 'session:' + SessionID;

  // Stocker les données de session dans un hash Redis
  FRedis.HSet(SessionKey, 'user_id', IntToStr(UserID));
  FRedis.HSet(SessionKey, 'user_data', UserData);
  FRedis.HSet(SessionKey, 'created_at', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  FRedis.HSet(SessionKey, 'last_activity', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

  // Définir l'expiration
  FRedis.Expire(SessionKey, FSessionTTL);

  WriteLn('✅ Session créée: ', SessionID);
  Result := SessionID;
end;

function TSessionManager.GetSession(const SessionID: String): TJSONObject;  
var
  SessionKey: String;
  SessionData: TStringList;
begin
  SessionKey := 'session:' + SessionID;

  if not FRedis.Exists(SessionKey) then
    Exit(nil);

  // Récupérer toutes les données du hash
  SessionData := FRedis.HGetAll(SessionKey);
  try
    Result := TJSONObject.Create;
    // Parser SessionData et remplir le JSON
    // (implémentation simplifiée)
  finally
    SessionData.Free;
  end;

  // Rafraîchir la session
  RefreshSession(SessionID);
end;

function TSessionManager.UpdateSession(const SessionID: String; const Key, Value: String): Boolean;  
var
  SessionKey: String;
begin
  SessionKey := 'session:' + SessionID;

  if not FRedis.Exists(SessionKey) then
    Exit(False);

  Result := FRedis.HSet(SessionKey, Key, Value);

  // Mettre à jour l'activité
  FRedis.HSet(SessionKey, 'last_activity', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

  // Rafraîchir l'expiration
  RefreshSession(SessionID);
end;

function TSessionManager.DestroySession(const SessionID: String): Boolean;  
var
  SessionKey: String;
begin
  SessionKey := 'session:' + SessionID;
  Result := FRedis.DeleteKey(SessionKey);

  if Result then
    WriteLn('✅ Session détruite: ', SessionID);
end;

function TSessionManager.RefreshSession(const SessionID: String): Boolean;  
var
  SessionKey: String;
begin
  SessionKey := 'session:' + SessionID;
  Result := FRedis.Expire(SessionKey, FSessionTTL);
end;

function TSessionManager.IsSessionValid(const SessionID: String): Boolean;  
var
  SessionKey: String;
  TTL: Integer;
begin
  SessionKey := 'session:' + SessionID;
  TTL := FRedis.TTL(SessionKey);
  Result := TTL > 0;
end;

end.
```

**Utilisation :**

```pascal
var
  Sessions: TSessionManager;
  SessionID: String;
  SessionData: TJSONObject;

begin
  Sessions := TSessionManager.Create(Redis, 3600);  // 1 heure
  try
    // Créer une nouvelle session lors de la connexion
    SessionID := Sessions.CreateSession(123, '{"role":"admin"}');
    WriteLn('Session ID: ', SessionID);

    // Récupérer les données de session
    SessionData := Sessions.GetSession(SessionID);
    if SessionData <> nil then
    begin
      WriteLn('User ID: ', SessionData.Get('user_id', 0));
      SessionData.Free;
    end;

    // Mettre à jour la session
    Sessions.UpdateSession(SessionID, 'last_page', '/dashboard');

    // Vérifier la validité
    if Sessions.IsSessionValid(SessionID) then
      WriteLn('✅ Session valide')
    else
      WriteLn('❌ Session expirée');

    // Déconnexion - détruire la session
    Sessions.DestroySession(SessionID);
  finally
    Sessions.Free;
  end;
end;
```

### 3. Rate Limiting (limitation de taux)

Limiter le nombre de requêtes par utilisateur/IP pour éviter les abus.

```pascal
unit RateLimiter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RedisClient;

type
  TRateLimiter = class
  private
    FRedis: TRedisClient;
    FMaxRequests: Integer;
    FWindowSeconds: Integer;
  public
    constructor Create(ARedis: TRedisClient; AMaxRequests, AWindowSeconds: Integer);

    function IsAllowed(const Identifier: String): Boolean;
    function GetRemainingRequests(const Identifier: String): Integer;
    function GetResetTime(const Identifier: String): Integer;
  end;

implementation

constructor TRateLimiter.Create(ARedis: TRedisClient; AMaxRequests, AWindowSeconds: Integer);  
begin
  FRedis := ARedis;
  FMaxRequests := AMaxRequests;
  FWindowSeconds := AWindowSeconds;
end;

function TRateLimiter.IsAllowed(const Identifier: String): Boolean;  
var
  Key: String;
  CurrentCount: Integer;
begin
  Key := 'ratelimit:' + Identifier;

  // Incrémenter le compteur
  CurrentCount := FRedis.Incr(Key);

  // Si c'est la première requête, définir l'expiration
  if CurrentCount = 1 then
    FRedis.Expire(Key, FWindowSeconds);

  // Vérifier si la limite est atteinte
  Result := CurrentCount <= FMaxRequests;

  if not Result then
    WriteLn('⚠️  Rate limit atteint pour: ', Identifier);
end;

function TRateLimiter.GetRemainingRequests(const Identifier: String): Integer;  
var
  Key: String;
  CurrentCount: Integer;
begin
  Key := 'ratelimit:' + Identifier;
  CurrentCount := StrToIntDef(FRedis.GetKey(Key), 0);
  Result := FMaxRequests - CurrentCount;

  if Result < 0 then
    Result := 0;
end;

function TRateLimiter.GetResetTime(const Identifier: String): Integer;  
var
  Key: String;
begin
  Key := 'ratelimit:' + Identifier;
  Result := FRedis.TTL(Key);
end;

end.
```

**Utilisation :**

```pascal
var
  RateLimiter: TRateLimiter;
  UserIP: String;
  i: Integer;

begin
  // Limiter à 100 requêtes par minute
  RateLimiter := TRateLimiter.Create(Redis, 100, 60);
  try
    UserIP := '192.168.1.100';

    // Simuler des requêtes
    for i := 1 to 105 do
    begin
      if RateLimiter.IsAllowed(UserIP) then
        WriteLn('✅ Requête ', i, ' autorisée')
      else
      begin
        WriteLn('❌ Requête ', i, ' bloquée');
        WriteLn('   Requêtes restantes: ', RateLimiter.GetRemainingRequests(UserIP));
        WriteLn('   Réinitialisation dans: ', RateLimiter.GetResetTime(UserIP), 's');
        Break;
      end;
    end;
  finally
    RateLimiter.Free;
  end;
end;
```

### 4. Leaderboard (classement)

Redis est parfait pour gérer des classements en temps réel avec les Sorted Sets.

```pascal
unit Leaderboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RedisClient;

type
  TLeaderboardEntry = record
    UserID: String;
    Score: Integer;
    Rank: Integer;
  end;

  TLeaderboard = class
  private
    FRedis: TRedisClient;
    FKey: String;
  public
    constructor Create(ARedis: TRedisClient; const ALeaderboardName: String);

    procedure AddScore(const UserID: String; Score: Integer);
    procedure IncrementScore(const UserID: String; Increment: Integer);
    function GetScore(const UserID: String): Integer;
    function GetRank(const UserID: String): Integer;
    function GetTop(Count: Integer): array of TLeaderboardEntry;
    function GetAround(const UserID: String; Range: Integer): array of TLeaderboardEntry;
    procedure Reset;
  end;

implementation

constructor TLeaderboard.Create(ARedis: TRedisClient; const ALeaderboardName: String);  
begin
  FRedis := ARedis;
  FKey := 'leaderboard:' + ALeaderboardName;
end;

procedure TLeaderboard.AddScore(const UserID: String; Score: Integer);  
begin
  // ZADD ajoute un membre avec son score dans un sorted set
  FRedis.SendCommand(Format('ZADD %s %d %s', [FKey, Score, UserID]));
  WriteLn('✅ Score ajouté: ', UserID, ' = ', Score);
end;

procedure TLeaderboard.IncrementScore(const UserID: String; Increment: Integer);  
begin
  // ZINCRBY incrémente le score d'un membre
  FRedis.SendCommand(Format('ZINCRBY %s %d %s', [FKey, Increment, UserID]));
  WriteLn('✅ Score incrémenté: ', UserID, ' +', Increment);
end;

function TLeaderboard.GetScore(const UserID: String): Integer;  
var
  Response: String;
begin
  Response := FRedis.SendCommand(Format('ZSCORE %s %s', [FKey, UserID]));
  Result := StrToIntDef(Response, 0);
end;

function TLeaderboard.GetRank(const UserID: String): Integer;  
var
  Response: String;
begin
  // ZREVRANK retourne le rang (0-based, du plus haut au plus bas)
  Response := FRedis.SendCommand(Format('ZREVRANK %s %s', [FKey, UserID]));
  Result := StrToIntDef(Response, -1) + 1;  // Convertir en 1-based
end;

function TLeaderboard.GetTop(Count: Integer): array of TLeaderboardEntry;  
var
  Response: String;
begin
  // ZREVRANGE retourne les membres par score décroissant
  Response := FRedis.SendCommand(
    Format('ZREVRANGE %s 0 %d WITHSCORES', [FKey, Count - 1])
  );

  // Parser la réponse et créer le tableau
  // (implémentation simplifiée)
  SetLength(Result, 0);
end;

function TLeaderboard.GetAround(const UserID: String; Range: Integer): array of TLeaderboardEntry;  
var
  Rank: Integer;
  StartRank, EndRank: Integer;
begin
  Rank := GetRank(UserID);
  if Rank < 0 then
    Exit(nil);

  StartRank := Max(0, Rank - Range - 1);
  EndRank := Rank + Range - 1;

  // Récupérer les joueurs autour de ce rang
  // (implémentation simplifiée)
  SetLength(Result, 0);
end;

procedure TLeaderboard.Reset;  
begin
  FRedis.DeleteKey(FKey);
  WriteLn('🗑️  Classement réinitialisé');
end;

end.
```

**Utilisation :**

```pascal
var
  Leaderboard: TLeaderboard;
  Top10: array of TLeaderboardEntry;
  Entry: TLeaderboardEntry;

begin
  Leaderboard := TLeaderboard.Create(Redis, 'global');
  try
    // Ajouter des scores
    Leaderboard.AddScore('player_001', 1500);
    Leaderboard.AddScore('player_002', 2300);
    Leaderboard.AddScore('player_003', 1800);
    Leaderboard.AddScore('player_004', 2100);
    Leaderboard.AddScore('player_005', 1950);

    // Un joueur gagne des points
    Leaderboard.IncrementScore('player_001', 500);

    // Obtenir le score d'un joueur
    WriteLn('Score de player_001: ', Leaderboard.GetScore('player_001'));

    // Obtenir le rang
    WriteLn('Rang de player_001: ', Leaderboard.GetRank('player_001'));

    // Obtenir le top 10
    Top10 := Leaderboard.GetTop(10);
    WriteLn('Top 10:');
    for Entry in Top10 do
      WriteLn(Format('  %d. %s - %d points', [Entry.Rank, Entry.UserID, Entry.Score]));
  finally
    Leaderboard.Free;
  end;
end;
```

### 5. File d'attente de tâches (Job Queue)

```pascal
unit JobQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RedisClient, fpjson;

type
  TJobStatus = (jsQueued, jsProcessing, jsCompleted, jsFailed);

  TJob = record
    ID: String;
    TaskType: String;
    Payload: String;
    Status: TJobStatus;
    CreatedAt: TDateTime;
    ProcessedAt: TDateTime;
  end;

  TJobQueue = class
  private
    FRedis: TRedisClient;
    FQueueName: String;

    function GenerateJobID: String;
  public
    constructor Create(ARedis: TRedisClient; const AQueueName: String);

    function EnqueueJob(const TaskType, Payload: String): String;
    function DequeueJob: TJob;
    procedure CompleteJob(const JobID: String);
    procedure FailJob(const JobID: String; const ErrorMessage: String);
    function GetQueueSize: Integer;
    function GetJobStatus(const JobID: String): TJobStatus;
  end;

implementation

uses
  MD5;

constructor TJobQueue.Create(ARedis: TRedisClient; const AQueueName: String);  
begin
  FRedis := ARedis;
  FQueueName := AQueueName;
end;

function TJobQueue.GenerateJobID: String;  
begin
  Result := 'job_' + MD5Print(MD5String(
    FormatDateTime('yyyymmddhhnnsszzz', Now) + IntToStr(Random(999999))
  ));
end;

function TJobQueue.EnqueueJob(const TaskType, Payload: String): String;  
var
  JobID, JobKey: String;
  JobData: TJSONObject;
begin
  JobID := GenerateJobID;
  JobKey := 'job:' + JobID;

  // Créer les données du job
  JobData := TJSONObject.Create;
  try
    JobData.Add('id', JobID);
    JobData.Add('task_type', TaskType);
    JobData.Add('payload', Payload);
    JobData.Add('status', 'queued');
    JobData.Add('created_at', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

    // Stocker les données du job
    FRedis.SetKey(JobKey, JobData.AsJSON);

    // Ajouter à la file d'attente
    FRedis.RPush('queue:' + FQueueName, JobID);

    WriteLn('✅ Job enfilé: ', JobID);
    Result := JobID;
  finally
    JobData.Free;
  end;
end;

function TJobQueue.DequeueJob: TJob;  
var
  JobID, JobKey, JobDataStr: String;
  JobData: TJSONObject;
begin
  // Retirer un job de la file
  JobID := FRedis.LPop('queue:' + FQueueName);

  if JobID = '' then
  begin
    Result.ID := '';
    Exit;
  end;

  JobKey := 'job:' + JobID;
  JobDataStr := FRedis.GetKey(JobKey);

  if JobDataStr = '' then
  begin
    Result.ID := '';
    Exit;
  end;

  // Parser les données
  JobData := TJSONObject(GetJSON(JobDataStr));
  try
    Result.ID := JobData.Get('id', '');
    Result.TaskType := JobData.Get('task_type', '');
    Result.Payload := JobData.Get('payload', '');
    Result.Status := jsProcessing;

    // Mettre à jour le statut
    JobData.Strings['status'] := 'processing';
    FRedis.SetKey(JobKey, JobData.AsJSON);

    WriteLn('📤 Job défilé: ', Result.ID);
  finally
    JobData.Free;
  end;
end;

procedure TJobQueue.CompleteJob(const JobID: String);  
var
  JobKey: String;
begin
  JobKey := 'job:' + JobID;

  // Mettre à jour le statut (implémentation simplifiée)
  FRedis.HSet(JobKey, 'status', 'completed');
  FRedis.HSet(JobKey, 'completed_at', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

  WriteLn('✅ Job complété: ', JobID);
end;

procedure TJobQueue.FailJob(const JobID: String; const ErrorMessage: String);  
var
  JobKey: String;
begin
  JobKey := 'job:' + JobID;

  FRedis.HSet(JobKey, 'status', 'failed');
  FRedis.HSet(JobKey, 'error', ErrorMessage);
  FRedis.HSet(JobKey, 'failed_at', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

  WriteLn('❌ Job échoué: ', JobID, ' - ', ErrorMessage);
end;

function TJobQueue.GetQueueSize: Integer;  
begin
  Result := FRedis.LLen('queue:' + FQueueName);
end;

function TJobQueue.GetJobStatus(const JobID: String): TJobStatus;  
var
  JobKey, Status: String;
begin
  JobKey := 'job:' + JobID;
  Status := FRedis.HGet(JobKey, 'status');

  if Status = 'queued' then
    Result := jsQueued
  else if Status = 'processing' then
    Result := jsProcessing
  else if Status = 'completed' then
    Result := jsCompleted
  else if Status = 'failed' then
    Result := jsFailed
  else
    Result := jsQueued;
end;

end.
```

### Worker pour traiter les jobs

```pascal
program JobWorker;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, RedisClient, JobQueue;

var
  Redis: TRedisClient;
  Queue: TJobQueue;
  Job: TJob;
  Running: Boolean;

procedure ProcessJob(const Job: TJob);  
begin
  WriteLn('🔧 Traitement du job: ', Job.ID);
  WriteLn('   Type: ', Job.TaskType);
  WriteLn('   Payload: ', Job.Payload);

  try
    // Simuler le traitement
    case Job.TaskType of
      'send_email':
      begin
        WriteLn('   📧 Envoi d''email...');
        Sleep(1000);
      end;

      'generate_report':
      begin
        WriteLn('   📊 Génération de rapport...');
        Sleep(2000);
      end;

      'process_image':
      begin
        WriteLn('   🖼️  Traitement d''image...');
        Sleep(1500);
      end;

    else
      WriteLn('   ⚠️  Type de job inconnu');
    end;

    // Marquer comme complété
    Queue.CompleteJob(Job.ID);
  except
    on E: Exception do
      Queue.FailJob(Job.ID, E.Message);
  end;
end;

begin
  WriteLn('=== Worker de traitement de jobs ===');
  WriteLn('Démarrage...');
  WriteLn;

  Redis := TRedisClient.Create('localhost', 6379);
  try
    if not Redis.Connect then
    begin
      WriteLn('❌ Impossible de se connecter à Redis');
      Exit;
    end;

    Queue := TJobQueue.Create(Redis, 'default');
    try
      Running := True;

      WriteLn('✅ Worker prêt à traiter les jobs');
      WriteLn('Appuyez sur Ctrl+C pour arrêter');
      WriteLn;

      while Running do
      begin
        // Récupérer un job de la file
        Job := Queue.DequeueJob;

        if Job.ID <> '' then
          ProcessJob(Job)
        else
        begin
          // Aucun job en attente, attendre un peu
          Sleep(1000);
        end;
      end;

    finally
      Queue.Free;
    end;
  finally
    Redis.Disconnect;
    Redis.Free;
  end;

  WriteLn('Worker arrêté');
end.
```

### Application d'envoi de jobs

```pascal
program JobProducer;

{$mode objfpc}{$H+}

uses
  SysUtils, RedisClient, JobQueue;

var
  Redis: TRedisClient;
  Queue: TJobQueue;
  JobID: String;
  i: Integer;

begin
  WriteLn('=== Producteur de jobs ===');
  WriteLn;

  Redis := TRedisClient.Create('localhost', 6379);
  try
    if not Redis.Connect then
    begin
      WriteLn('❌ Impossible de se connecter à Redis');
      Exit;
    end;

    Queue := TJobQueue.Create(Redis, 'default');
    try
      // Créer plusieurs jobs
      WriteLn('Création de jobs...');
      WriteLn;

      JobID := Queue.EnqueueJob('send_email', '{"to":"user@example.com","subject":"Bienvenue"}');
      WriteLn('Job créé: ', JobID);

      JobID := Queue.EnqueueJob('generate_report', '{"type":"monthly","year":2024,"month":1}');
      WriteLn('Job créé: ', JobID);

      JobID := Queue.EnqueueJob('process_image', '{"path":"/uploads/photo.jpg","size":"800x600"}');
      WriteLn('Job créé: ', JobID);

      // Créer 10 jobs en masse
      WriteLn;
      WriteLn('Création de 10 jobs supplémentaires...');
      for i := 1 to 10 do
      begin
        JobID := Queue.EnqueueJob('send_email',
          Format('{"to":"user%d@example.com","subject":"Newsletter"}', [i]));
      end;

      WriteLn;
      WriteLn('✅ Tous les jobs ont été créés');
      WriteLn('Taille de la file: ', Queue.GetQueueSize);
    finally
      Queue.Free;
    end;
  finally
    Redis.Disconnect;
    Redis.Free;
  end;
end.
```

## Comparaison MongoDB vs Redis

| Aspect | MongoDB | Redis |
|--------|---------|-------|
| **Type** | Base orientée documents | Base clé-valeur en mémoire |
| **Stockage** | Disque (persistent) | RAM (avec persistance optionnelle) |
| **Performance** | Bonne (ms) | Excellente (< 1ms) |
| **Taille des données** | Très grandes bases (TB) | Limitée par la RAM (GB) |
| **Requêtes complexes** | Oui (agrégations, recherche) | Non (simple clé-valeur) |
| **Transactions** | Oui (ACID depuis 4.0) | Limitées |
| **Cas d'usage** | Données semi-structurées, documents | Cache, sessions, compteurs, queues |
| **Scalabilité** | Sharding horizontal | Replication, Redis Cluster |

### Quand utiliser MongoDB ?

✅ **Utilisez MongoDB pour :**
- Stocker des documents JSON complexes
- Données avec schéma flexible
- Requêtes et agrégations complexes
- Grandes bases de données (plusieurs TB)
- Données qui doivent persister à long terme

**Exemples :**
- Catalogue de produits e-commerce
- Profils utilisateurs avec données variées
- Logs et événements
- Système de gestion de contenu (CMS)
- Données IoT

### Quand utiliser Redis ?

✅ **Utilisez Redis pour :**
- Cache haute performance
- Sessions web
- Compteurs temps réel
- Files d'attente
- Données éphémères (TTL)
- Leaderboards/classements

**Exemples :**
- Cache de base de données
- Gestion de sessions utilisateur
- Rate limiting
- Compteurs de vues/likes
- File d'attente de jobs

## Architecture hybride MongoDB + Redis

L'idéal est souvent de combiner les deux technologies.

```
┌─────────────────────────────────────────┐
│         Application FreePascal          │
└──────────┬─────────────────┬────────────┘
           │                 │
           ↓                 ↓
    ┌──────────┐      ┌──────────┐
    │  Redis   │      │ MongoDB  │
    │  (Cache) │      │ (Storage)│
    └──────────┘      └──────────┘
    - Sessions        - Documents
    - Cache           - Données permanentes
    - Compteurs       - Historique
    - Queue           - Recherches complexes
```

### Exemple d'architecture hybride

```pascal
unit HybridDataManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RedisClient, MongoDBClient;

type
  THybridDataManager = class
  private
    FRedis: TRedisClient;
    FMongo: TMongoClient;
    FCacheTTL: Integer;
  public
    constructor Create(ARedis: TRedisClient; AMongo: TMongoClient; ACacheTTL: Integer = 300);

    // Lecture avec cache automatique
    function GetDocument(const Collection, DocumentID: String): String;

    // Écriture (invalide le cache)
    function SaveDocument(const Collection, DocumentID, Data: String): Boolean;

    // Suppression (invalide le cache)
    function DeleteDocument(const Collection, DocumentID: String): Boolean;

    // Compteurs avec Redis (pas de cache, toujours à jour)
    procedure IncrementViewCount(const DocumentID: String);
    function GetViewCount(const DocumentID: String): Integer;
  end;

implementation

uses
  fpjson;

constructor THybridDataManager.Create(ARedis: TRedisClient;
  AMongo: TMongoClient; ACacheTTL: Integer);
begin
  FRedis := ARedis;
  FMongo := AMongo;
  FCacheTTL := ACacheTTL;
end;

function THybridDataManager.GetDocument(const Collection, DocumentID: String): String;  
var
  CacheKey: String;
  CachedData: String;
begin
  CacheKey := Format('doc:%s:%s', [Collection, DocumentID]);

  // 1. Essayer le cache Redis
  CachedData := FRedis.GetKey(CacheKey);

  if CachedData <> '' then
  begin
    WriteLn('✅ Cache HIT: ', DocumentID);
    Exit(CachedData);
  end;

  // 2. Cache MISS - Récupérer depuis MongoDB
  WriteLn('⚠️  Cache MISS: ', DocumentID, ' - Requête MongoDB');
  Result := FMongo.FindDocuments(Collection,
    Format('{"_id":"%s"}', [DocumentID]));

  if Result <> '' then
  begin
    // 3. Mettre en cache dans Redis
    FRedis.SetKey(CacheKey, Result);
    FRedis.Expire(CacheKey, FCacheTTL);
    WriteLn('💾 Données mises en cache');
  end;
end;

function THybridDataManager.SaveDocument(const Collection, DocumentID, Data: String): Boolean;  
var
  CacheKey: String;
begin
  // 1. Sauvegarder dans MongoDB
  Result := FMongo.UpdateDocument(Collection,
    Format('{"_id":"%s"}', [DocumentID]),
    Data);

  if Result then
  begin
    WriteLn('✅ Document sauvegardé dans MongoDB');

    // 2. Invalider le cache Redis
    CacheKey := Format('doc:%s:%s', [Collection, DocumentID]);
    FRedis.DeleteKey(CacheKey);
    WriteLn('🗑️  Cache invalidé');
  end;
end;

function THybridDataManager.DeleteDocument(const Collection, DocumentID: String): Boolean;  
var
  CacheKey: String;
begin
  // 1. Supprimer de MongoDB
  Result := FMongo.DeleteDocument(Collection,
    Format('{"_id":"%s"}', [DocumentID]));

  if Result then
  begin
    WriteLn('✅ Document supprimé de MongoDB');

    // 2. Invalider le cache Redis
    CacheKey := Format('doc:%s:%s', [Collection, DocumentID]);
    FRedis.DeleteKey(CacheKey);
    WriteLn('🗑️  Cache invalidé');
  end;
end;

procedure THybridDataManager.IncrementViewCount(const DocumentID: String);  
var
  CountKey: String;
begin
  // Utiliser Redis pour les compteurs (temps réel)
  CountKey := Format('views:%s', [DocumentID]);
  FRedis.Incr(CountKey);
end;

function THybridDataManager.GetViewCount(const DocumentID: String): Integer;  
var
  CountKey, Value: String;
begin
  CountKey := Format('views:%s', [DocumentID]);
  Value := FRedis.GetKey(CountKey);
  Result := StrToIntDef(Value, 0);
end;

end.
```

## Configuration multi-OS

### Configuration Redis

**Ubuntu :**

```bash
# Fichier de configuration
sudo nano /etc/redis/redis.conf

# Redémarrer
sudo systemctl restart redis-server

# Logs
sudo tail -f /var/log/redis/redis-server.log
```

**Windows :**

```powershell
# Fichier de configuration
notepad redis.windows.conf

# Redémarrer (en tant qu'administrateur)
net stop Redis  
net start Redis

# Logs
type redis.log
```

### Configuration MongoDB

**Ubuntu :**

```bash
# Fichier de configuration
sudo nano /etc/mongod.conf

# Redémarrer
sudo systemctl restart mongod

# Logs
sudo tail -f /var/log/mongodb/mongod.log

# Shell interactif
mongosh
```

**Windows :**

```powershell
# Fichier de configuration
notepad "C:\Program Files\MongoDB\Server\7.0\bin\mongod.cfg"

# Redémarrer (en tant qu'administrateur)
net stop MongoDB  
net start MongoDB

# Shell interactif
mongosh
```

## Sécurité

### Sécuriser Redis

**1. Définir un mot de passe**

```ini
# redis.conf
requirepass votre_mot_de_passe_fort
```

**2. Désactiver les commandes dangereuses**

```ini
# redis.conf
rename-command FLUSHDB ""  
rename-command FLUSHALL ""  
rename-command CONFIG "CONFIG_SECRET_NAME"
```

**3. Limiter l'accès réseau**

```ini
# redis.conf
bind 127.0.0.1  # Uniquement localhost
# ou
bind 127.0.0.1 192.168.1.100  # IPs spécifiques
```

**4. Activer le mode protégé**

```ini
# redis.conf
protected-mode yes
```

### Sécuriser MongoDB

**1. Activer l'authentification**

```bash
# Se connecter à MongoDB
mongosh

# Créer un administrateur
use admin  
db.createUser({
  user: "admin",
  pwd: "mot_de_passe_securise",
  roles: [ { role: "userAdminAnyDatabase", db: "admin" } ]
})

# Créer un utilisateur pour l'application
use mon_app  
db.createUser({
  user: "app_user",
  pwd: "mot_de_passe_app",
  roles: [ { role: "readWrite", db: "mon_app" } ]
})
```

**2. Activer l'authentification dans la configuration**

```yaml
# /etc/mongod.conf (Ubuntu) ou mongod.cfg (Windows)
security:
  authorization: enabled
```

**3. Limiter l'accès réseau**

```yaml
# mongod.conf
net:
  bindIp: 127.0.0.1  # Uniquement localhost
  port: 27017
```

**4. Utiliser SSL/TLS**

```yaml
# mongod.conf
net:
  ssl:
    mode: requireSSL
    PEMKeyFile: /path/to/mongodb.pem
```

## Monitoring et maintenance

### Monitoring Redis

```pascal
program RedisMonitor;

{$mode objfpc}{$H+}

uses
  SysUtils, RedisClient;

procedure DisplayRedisInfo(Redis: TRedisClient);  
var
  Info: String;
begin
  Info := Redis.SendCommand('INFO');
  WriteLn('=== Informations Redis ===');
  WriteLn(Info);
  WriteLn;

  // Statistiques spécifiques
  WriteLn('Mémoire utilisée: ', Redis.SendCommand('INFO memory'));
  WriteLn('Nombre de clés: ', Redis.SendCommand('DBSIZE'));
  WriteLn('Clients connectés: ', Redis.SendCommand('CLIENT LIST'));
end;

var
  Redis: TRedisClient;

begin
  Redis := TRedisClient.Create;
  try
    if Redis.Connect then
    begin
      DisplayRedisInfo(Redis);
      Redis.Disconnect;
    end;
  finally
    Redis.Free;
  end;
end.
```

### Monitoring MongoDB

```pascal
program MongoMonitor;

{$mode objfpc}{$H+}

uses
  SysUtils, MongoDBClient;

procedure DisplayMongoStats(Mongo: TMongoClient);  
var
  Stats: String;
begin
  WriteLn('=== Statistiques MongoDB ===');

  // Statistiques de la base
  Stats := Mongo.RunCommand('{ dbStats: 1 }');
  WriteLn('Base de données: ', Stats);
  WriteLn;

  // Statistiques serveur
  Stats := Mongo.RunCommand('{ serverStatus: 1 }');
  WriteLn('Serveur: ', Stats);
end;

var
  Mongo: TMongoClient;

begin
  Mongo := TMongoClient.Create('localhost', 27017, 'mon_app');
  try
    DisplayMongoStats(Mongo);
  finally
    Mongo.Free;
  end;
end.
```

## Sauvegarde et restauration

### Redis

**Sauvegarde :**

```bash
# Ubuntu/Windows
redis-cli SAVE
# ou
redis-cli BGSAVE  # En arrière-plan

# Copier le fichier dump.rdb
# Ubuntu: /var/lib/redis/dump.rdb
# Windows: dump.rdb dans le répertoire Redis
```

**Restauration :**

```bash
# 1. Arrêter Redis
sudo systemctl stop redis-server  # Ubuntu  
net stop Redis  # Windows

# 2. Remplacer dump.rdb
sudo cp backup_dump.rdb /var/lib/redis/dump.rdb  # Ubuntu  
copy backup_dump.rdb dump.rdb  # Windows

# 3. Redémarrer Redis
sudo systemctl start redis-server  # Ubuntu  
net start Redis  # Windows
```

### MongoDB

**Sauvegarde :**

```bash
# Sauvegarder toute la base
mongodump --db mon_app --out /backup/mongodb/

# Sauvegarder une collection spécifique
mongodump --db mon_app --collection clients --out /backup/mongodb/
```

**Restauration :**

```bash
# Restaurer toute la base
mongorestore --db mon_app /backup/mongodb/mon_app/

# Restaurer une collection
mongorestore --db mon_app --collection clients /backup/mongodb/mon_app/clients.bson
```

## Bonnes pratiques

### Redis

✅ **À faire :**
- Utiliser des clés avec préfixes descriptifs (`user:123`, `session:abc`)
- Définir des TTL sur les données temporaires
- Surveiller l'utilisation mémoire
- Utiliser des pipelines pour les opérations multiples
- Configurer une politique d'éviction appropriée

❌ **À éviter :**
- Stocker des données volumineuses (> 1 MB par clé)
- Utiliser Redis comme base de données principale
- Oublier de définir des expirations
- Utiliser des commandes bloquantes en production

### MongoDB

✅ **À faire :**
- Créer des index sur les champs fréquemment interrogés
- Utiliser des projections pour limiter les champs retournés
- Valider le schéma avec des validators
- Utiliser l'agrégation pour les calculs complexes
- Sauvegarder régulièrement

❌ **À éviter :**
- Faire des scans complets de collections
- Créer trop d'index (ralentit les écritures)
- Stocker des documents > 16 MB
- Ignorer les erreurs de réplication

## Conclusion

MongoDB et Redis sont deux technologies NoSQL complémentaires qui, utilisées avec FreePascal/Lazarus, permettent de créer des applications performantes et scalables sur Windows et Ubuntu.

### Points clés à retenir

1. **MongoDB** est idéal pour les documents JSON flexibles et les requêtes complexes
2. **Redis** excelle pour le cache, les sessions et les opérations temps réel
3. **L'architecture hybride** combine le meilleur des deux mondes
4. **La sécurité** est primordiale : authentification, chiffrement, firewall
5. **Le monitoring** permet de détecter les problèmes avant qu'ils n'impactent les utilisateurs
6. **Les sauvegardes** régulières protègent vos données

Avec les connaissances acquises dans ce chapitre, vous êtes maintenant capable d'intégrer des bases de données NoSQL modernes dans vos applications FreePascal/Lazarus multi-plateformes.

⏭️ [GraphQL et API modernes](/08-bases-donnees-orm-multiplatefomes/12-graphql-api-modernes.md)
