🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.3 Brook Framework pour applications REST

## Introduction à Brook Framework

**Brook Framework** est un framework web moderne pour FreePascal, spécialement conçu pour créer des **API REST** performantes et élégantes. Contrairement à fpWeb qui est bas niveau, Brook fournit une abstraction de haut niveau avec une syntaxe intuitive et expressive.

### Qu'est-ce qu'une API REST ?

**REST** (Representational State Transfer) est un style d'architecture pour les services web. Une API REST utilise les **méthodes HTTP standard** pour effectuer des opérations CRUD (Create, Read, Update, Delete) sur des ressources.

**Principes REST** :
- Chaque ressource a une **URL unique** (`/users/123`)
- Utilisation des **méthodes HTTP** (GET, POST, PUT, DELETE)
- Communication **sans état** (stateless)
- Format de données **JSON** ou XML
- Codes de statut HTTP standards

**Exemple d'API REST** :

| Méthode | URL | Action | Code de succès |
|---------|-----|--------|----------------|
| GET | `/users` | Liste tous les utilisateurs | 200 OK |
| GET | `/users/123` | Détails de l'utilisateur 123 | 200 OK |
| POST | `/users` | Créer un utilisateur | 201 Created |
| PUT | `/users/123` | Modifier l'utilisateur 123 | 200 OK |
| DELETE | `/users/123` | Supprimer l'utilisateur 123 | 204 No Content |

### Pourquoi Brook Framework ?

**Avantages de Brook** :

✅ **Syntaxe élégante** - Code lisible et concis  
✅ **Routage puissant** - URLs dynamiques et expressives  
✅ **JSON natif** - Support intégré du format JSON  
✅ **Middlewares** - Logique réutilisable (auth, logs, CORS)  
✅ **Validation** - Validation automatique des données  
✅ **Multi-plateforme** - Windows, Linux, macOS  
✅ **FastCGI natif** - Performances excellentes  
✅ **Documentation claire** - Communauté active

**Comparaison avec d'autres solutions** :

| Aspect | fpWeb | Brook | mORMot |
|--------|-------|-------|--------|
| **Courbe d'apprentissage** | Moyenne | Facile | Difficile |
| **Syntaxe** | Verbose | Élégante | Complexe |
| **API REST** | Manuel | Natif | Natif |
| **JSON** | Support basique | Excellent | Excellent |
| **Middlewares** | Non | Oui | Oui |
| **ORM** | Non | Non | Oui |
| **Performance** | Bonne | Excellente | Exceptionnelle |
| **Usage recommandé** | Applications générales | API REST modernes | Systèmes complexes |

### Versions de Brook

Brook existe en deux versions majeures :

- **Brook Framework 5.x** (dernière version, recommandée)
- **Brook Framework 4.x et antérieurs** (ancienne architecture)

Ce tutoriel couvre **Brook 5.x**, la version moderne et la plus performante.

## Installation de Brook Framework

### Prérequis

```bash
# FreePascal et Lazarus
sudo apt install -y fpc lazarus  # Ubuntu
# ou télécharger depuis freepascal.org pour Windows

# Git pour cloner le dépôt
sudo apt install -y git  # Ubuntu
# ou Git for Windows
```

### Méthode 1 : Installation depuis GitHub (recommandée)

**Cloner le dépôt** :

```bash
# Créer un répertoire pour les bibliothèques
mkdir -p ~/fpclibs
cd ~/fpclibs

# Cloner Brook Framework
git clone https://github.com/risoflora/brookframework.git

# Se positionner sur la version stable
cd brookframework
git checkout v5.8.0  # Remplacer par la dernière version stable
```

**Configurer le chemin de compilation** :

```bash
# Linux/macOS - Ajouter au .bashrc ou .zshrc
export BROOK_PATH="$HOME/fpclibs/brookframework/Source"

# Windows - Variables d'environnement système
# BROOK_PATH = C:\fpclibs\brookframework\Source
```

**Compiler les exemples pour tester** :

```bash
cd ~/fpclibs/brookframework/Examples/Simple
fpc -Fu../../Source simple.lpr
./simple
# Tester : http://localhost:8080
```

### Méthode 2 : Installation via Online Package Manager (Lazarus)

1. Ouvrir Lazarus
2. Menu **Package** → **Online Package Manager**
3. Chercher "Brook Framework"
4. Cliquer sur **Install**
5. Reconstruire Lazarus

### Méthode 3 : Installation manuelle

**Télécharger** :
- Aller sur https://github.com/risoflora/brookframework
- Télécharger le ZIP de la dernière release
- Extraire dans un répertoire (ex: `C:\brook` ou `/home/user/brook`)

**Ajouter au chemin de recherche** :

Lors de la compilation, ajouter l'option :

```bash
fpc -Fu/chemin/vers/brook/Source votre_app.lpr
```

## Première application Brook

### Application "Hello World"

**Fichier** : `hello_brook.lpr`

```pascal
program HelloBrook;

{$mode objfpc}{$H+}

uses
  BrookApplication,
  BrookHTTPRouter;

begin
  // Définir une route simple
  BrookHTTPRouter.Route('/',
    procedure(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse)
    begin
      AResponse.Send('Hello from Brook Framework!');
    end
  );

  // Configuration du serveur
  BrookApp.Port := 8080;

  // Démarrer l'application
  BrookApp.Run;
end.
```

**Compiler et exécuter** :

```bash
# Compilation
fpc -Fu~/fpclibs/brookframework/Source hello_brook.lpr

# Exécution
./hello_brook

# Tester
curl http://localhost:8080
# ou ouvrir dans un navigateur
```

**Explication du code** :

- `BrookApplication` : Unité principale de Brook
- `BrookHTTPRouter` : Gestion du routage HTTP
- `BrookApp` : Instance globale de l'application
- `Route('/', ...)` : Définir une route pour l'URL `/`
- `ARequest` : Objet contenant les données de la requête
- `AResponse` : Objet pour construire la réponse
- `Send()` : Envoyer la réponse au client

### Application avec routage structuré

Pour des applications plus complexes, on sépare les routes :

**Fichier** : `api_simple.lpr`

```pascal
program APISimple;

{$mode objfpc}{$H+}

uses
  SysUtils,
  BrookApplication,
  BrookHTTPRouter,
  BrookHTTPRequest,
  BrookHTTPResponse;

// Page d'accueil
procedure HandleHome(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('<h1>Bienvenue sur l''API Brook</h1>' +
                 '<p>Essayez /api/hello</p>');
end;

// Route API
procedure HandleHello(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('{"message": "Hello from API!"}', 'application/json');
end;

// Route avec paramètre
procedure HandleGreet(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  Name: string;
begin
  Name := ARequest.Params['name'];
  AResponse.Send(Format('{"greeting": "Hello %s!"}', [Name]), 'application/json');
end;

begin
  // Enregistrer les routes
  BrookHTTPRouter.Route('/', @HandleHome);
  BrookHTTPRouter.Route('/api/hello', @HandleHello);
  BrookHTTPRouter.Route('/api/greet/:name', @HandleGreet);

  // Configuration
  BrookApp.Port := 8080;

  WriteLn('Serveur démarré sur http://localhost:8080');
  WriteLn('Appuyez sur Ctrl+C pour arrêter');

  // Démarrer
  BrookApp.Run;
end.
```

**Tester les routes** :

```bash
curl http://localhost:8080/
curl http://localhost:8080/api/hello
curl http://localhost:8080/api/greet/Alice
```

## Système de routage Brook

### Routes simples

```pascal
// Route statique
BrookHTTPRouter.Route('/users', @HandleUsers);

// Méthode spécifique
BrookHTTPRouter.Route('/users', @HandleUsersGET).Method('GET');
BrookHTTPRouter.Route('/users', @HandleUsersPOST).Method('POST');
```

### Routes avec paramètres

```pascal
// Un paramètre
BrookHTTPRouter.Route('/users/:id', @HandleUser);

// Accès au paramètre
procedure HandleUser(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  UserID: string;
begin
  UserID := ARequest.Params['id'];
  AResponse.Send(Format('User ID: %s', [UserID]));
end;

// Plusieurs paramètres
BrookHTTPRouter.Route('/posts/:id/comments/:comment_id', @HandleComment);
```

### Routes avec wildcards

```pascal
// Capturer tout après /files/
BrookHTTPRouter.Route('/files/*path', @HandleFiles);

procedure HandleFiles(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  FilePath: string;
begin
  FilePath := ARequest.Params['path'];  // Ex: images/photo.jpg
  // Servir le fichier...
end;
```

### Groupes de routes (prefixes)

```pascal
// Groupe API v1
BrookHTTPRouter.Group('/api/v1',
  procedure
  begin
    BrookHTTPRouter.Route('/users', @V1GetUsers);
    BrookHTTPRouter.Route('/users/:id', @V1GetUser);
    BrookHTTPRouter.Route('/posts', @V1GetPosts);
  end
);

// Groupe API v2
BrookHTTPRouter.Group('/api/v2',
  procedure
  begin
    BrookHTTPRouter.Route('/users', @V2GetUsers);
    BrookHTTPRouter.Route('/users/:id', @V2GetUser);
  end
);
```

## Gestion du JSON

Brook facilite grandement le travail avec JSON.

### Envoyer du JSON

```pascal
procedure HandleUserJSON(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONStr: string;
begin
  JSONStr := '{"id": 1, "name": "Alice", "email": "alice@example.com"}';
  AResponse.Send(JSONStr, 'application/json');
end;
```

### Construction de JSON avec fpjson

```pascal
uses
  fpjson, jsonparser;

procedure HandleUser(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('id', 1);
    JSONObj.Add('name', 'Alice');
    JSONObj.Add('email', 'alice@example.com');
    JSONObj.Add('active', True);

    AResponse.Send(JSONObj.AsJSON, 'application/json');
  finally
    JSONObj.Free;
  end;
end;
```

### Tableau JSON

```pascal
procedure HandleUsers(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONArray: TJSONArray;
  User: TJSONObject;
begin
  JSONArray := TJSONArray.Create;
  try
    // Premier utilisateur
    User := TJSONObject.Create;
    User.Add('id', 1);
    User.Add('name', 'Alice');
    JSONArray.Add(User);

    // Deuxième utilisateur
    User := TJSONObject.Create;
    User.Add('id', 2);
    User.Add('name', 'Bob');
    JSONArray.Add(User);

    AResponse.Send(JSONArray.AsJSON, 'application/json');
  finally
    JSONArray.Free;
  end;
end;
```

### Parser le JSON reçu

```pascal
procedure HandleCreateUser(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  Name, Email: string;
begin
  try
    // Parser le JSON du corps de la requête
    JSONData := GetJSON(ARequest.Body.AsString);
    try
      if JSONData is TJSONObject then
      begin
        JSONObj := TJSONObject(JSONData);

        // Extraire les données
        Name := JSONObj.Get('name', '');
        Email := JSONObj.Get('email', '');

        // Valider
        if (Name = '') or (Email = '') then
        begin
          AResponse.SendStatus(400, '{"error": "Missing fields"}', 'application/json');
          Exit;
        end;

        // Créer l'utilisateur en BDD...
        // ...

        // Réponse succès
        AResponse.SendStatus(201,
          Format('{"id": 123, "name": "%s", "email": "%s"}', [Name, Email]),
          'application/json');
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      AResponse.SendStatus(400,
        Format('{"error": "Invalid JSON: %s"}', [E.Message]),
        'application/json');
  end;
end;
```

## API REST complète - Exemple CRUD

Créons une API complète pour gérer des utilisateurs.

### Structure de l'application

```pascal
program UserAPI;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fpjson, jsonparser,
  BrookApplication,
  BrookHTTPRouter,
  BrookHTTPRequest,
  BrookHTTPResponse;

type
  // Structure utilisateur simple
  TUser = record
    ID: Integer;
    Name: string;
    Email: string;
    Active: Boolean;
  end;

var
  // Stockage en mémoire (remplacer par BDD en production)
  Users: array of TUser;
  NextID: Integer = 1;

// Fonctions helper
function FindUserByID(ID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(Users) do
    if Users[i].ID = ID then
      Exit(i);
end;

function UserToJSON(const User: TUser): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', User.ID);
  Result.Add('name', User.Name);
  Result.Add('email', User.Email);
  Result.Add('active', User.Active);
end;

// GET /api/users - Liste tous les utilisateurs
procedure GetUsers(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONArray: TJSONArray;
  i: Integer;
begin
  JSONArray := TJSONArray.Create;
  try
    for i := 0 to High(Users) do
      JSONArray.Add(UserToJSON(Users[i]));

    AResponse.Send(JSONArray.AsJSON, 'application/json');
  finally
    JSONArray.Free;
  end;
end;

// GET /api/users/:id - Détails d'un utilisateur
procedure GetUser(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  UserID, Index: Integer;
  JSONObj: TJSONObject;
begin
  UserID := StrToIntDef(ARequest.Params['id'], 0);
  Index := FindUserByID(UserID);

  if Index = -1 then
  begin
    AResponse.SendStatus(404, '{"error": "User not found"}', 'application/json');
    Exit;
  end;

  JSONObj := UserToJSON(Users[Index]);
  try
    AResponse.Send(JSONObj.AsJSON, 'application/json');
  finally
    JSONObj.Free;
  end;
end;

// POST /api/users - Créer un utilisateur
procedure CreateUser(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  NewUser: TUser;
begin
  try
    JSONData := GetJSON(ARequest.Body.AsString);
    try
      if not (JSONData is TJSONObject) then
      begin
        AResponse.SendStatus(400, '{"error": "Invalid JSON"}', 'application/json');
        Exit;
      end;

      JSONObj := TJSONObject(JSONData);

      // Validation
      if (JSONObj.Get('name', '') = '') or (JSONObj.Get('email', '') = '') then
      begin
        AResponse.SendStatus(400, '{"error": "Missing required fields"}', 'application/json');
        Exit;
      end;

      // Créer l'utilisateur
      NewUser.ID := NextID;
      Inc(NextID);
      NewUser.Name := JSONObj.Get('name', '');
      NewUser.Email := JSONObj.Get('email', '');
      NewUser.Active := JSONObj.Get('active', True);

      // Ajouter au tableau
      SetLength(Users, Length(Users) + 1);
      Users[High(Users)] := NewUser;

      // Réponse
      JSONObj := UserToJSON(NewUser);
      try
        AResponse.SendStatus(201, JSONObj.AsJSON, 'application/json');
      finally
        JSONObj.Free;
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      AResponse.SendStatus(400,
        Format('{"error": "%s"}', [E.Message]),
        'application/json');
  end;
end;

// PUT /api/users/:id - Modifier un utilisateur
procedure UpdateUser(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  UserID, Index: Integer;
  JSONData: TJSONData;
  JSONObj, ResultObj: TJSONObject;
begin
  UserID := StrToIntDef(ARequest.Params['id'], 0);
  Index := FindUserByID(UserID);

  if Index = -1 then
  begin
    AResponse.SendStatus(404, '{"error": "User not found"}', 'application/json');
    Exit;
  end;

  try
    JSONData := GetJSON(ARequest.Body.AsString);
    try
      if not (JSONData is TJSONObject) then
      begin
        AResponse.SendStatus(400, '{"error": "Invalid JSON"}', 'application/json');
        Exit;
      end;

      JSONObj := TJSONObject(JSONData);

      // Mettre à jour les champs
      if JSONObj.IndexOfName('name') >= 0 then
        Users[Index].Name := JSONObj.Get('name', Users[Index].Name);

      if JSONObj.IndexOfName('email') >= 0 then
        Users[Index].Email := JSONObj.Get('email', Users[Index].Email);

      if JSONObj.IndexOfName('active') >= 0 then
        Users[Index].Active := JSONObj.Get('active', Users[Index].Active);

      // Réponse
      ResultObj := UserToJSON(Users[Index]);
      try
        AResponse.Send(ResultObj.AsJSON, 'application/json');
      finally
        ResultObj.Free;
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      AResponse.SendStatus(400,
        Format('{"error": "%s"}', [E.Message]),
        'application/json');
  end;
end;

// DELETE /api/users/:id - Supprimer un utilisateur
procedure DeleteUser(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  UserID, Index, i: Integer;
begin
  UserID := StrToIntDef(ARequest.Params['id'], 0);
  Index := FindUserByID(UserID);

  if Index = -1 then
  begin
    AResponse.SendStatus(404, '{"error": "User not found"}', 'application/json');
    Exit;
  end;

  // Supprimer du tableau
  for i := Index to High(Users) - 1 do
    Users[i] := Users[i + 1];
  SetLength(Users, Length(Users) - 1);

  // Réponse 204 No Content
  AResponse.SendStatus(204, '', '');
end;

begin
  // Configuration des routes
  BrookHTTPRouter.Route('/api/users', @GetUsers).Method('GET');
  BrookHTTPRouter.Route('/api/users/:id', @GetUser).Method('GET');
  BrookHTTPRouter.Route('/api/users', @CreateUser).Method('POST');
  BrookHTTPRouter.Route('/api/users/:id', @UpdateUser).Method('PUT');
  BrookHTTPRouter.Route('/api/users/:id', @DeleteUser).Method('DELETE');

  // Configuration du serveur
  BrookApp.Port := 8080;

  WriteLn('API REST démarrée sur http://localhost:8080');
  WriteLn('Endpoints disponibles:');
  WriteLn('  GET    /api/users');
  WriteLn('  GET    /api/users/:id');
  WriteLn('  POST   /api/users');
  WriteLn('  PUT    /api/users/:id');
  WriteLn('  DELETE /api/users/:id');

  // Démarrer
  BrookApp.Run;
end.
```

### Tester l'API

```bash
# Lister les utilisateurs (vide au début)
curl http://localhost:8080/api/users

# Créer un utilisateur
curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{"name":"Alice","email":"alice@example.com"}'

# Créer un deuxième
curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{"name":"Bob","email":"bob@example.com","active":false}'

# Lister tous les utilisateurs
curl http://localhost:8080/api/users

# Obtenir un utilisateur spécifique
curl http://localhost:8080/api/users/1

# Modifier un utilisateur
curl -X PUT http://localhost:8080/api/users/1 \
  -H "Content-Type: application/json" \
  -d '{"name":"Alice Smith","active":true}'

# Supprimer un utilisateur
curl -X DELETE http://localhost:8080/api/users/2

# Vérifier
curl http://localhost:8080/api/users
```

## Middlewares

Les middlewares sont des fonctions qui s'exécutent avant ou après les gestionnaires de routes. Ils sont parfaits pour :
- Logging
- Authentification
- CORS
- Gestion d'erreurs
- Compression

### Middleware de logging

```pascal
procedure LoggingMiddleware(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  WriteLn(Format('[%s] %s %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    ARequest.Method,
    ARequest.Path
  ]));
end;

// Enregistrer le middleware
BrookHTTPRouter.Use(@LoggingMiddleware);
```

### Middleware CORS

```pascal
procedure CORSMiddleware(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Headers['Access-Control-Allow-Origin'] := '*';
  AResponse.Headers['Access-Control-Allow-Methods'] := 'GET, POST, PUT, DELETE, OPTIONS';
  AResponse.Headers['Access-Control-Allow-Headers'] := 'Content-Type, Authorization';

  // Répondre immédiatement aux requêtes OPTIONS (preflight)
  if ARequest.Method = 'OPTIONS' then
  begin
    AResponse.SendStatus(204, '', '');
    // Arrêter le traitement
    Halt;
  end;
end;

BrookHTTPRouter.Use(@CORSMiddleware);
```

### Middleware d'authentification

```pascal
procedure AuthMiddleware(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  Token: string;
begin
  // Vérifier le header Authorization
  Token := ARequest.Headers['Authorization'];

  if Token = '' then
  begin
    AResponse.SendStatus(401, '{"error": "Missing authorization"}', 'application/json');
    Halt;  // Arrêter le traitement
  end;

  // Vérifier le token (simplifié)
  if not (Token = 'Bearer secret-token-123') then
  begin
    AResponse.SendStatus(403, '{"error": "Invalid token"}', 'application/json');
    Halt;
  end;

  // Token valide, continuer
end;

// Appliquer uniquement aux routes protégées
BrookHTTPRouter.Group('/api/admin',
  procedure
  begin
    BrookHTTPRouter.Use(@AuthMiddleware);
    BrookHTTPRouter.Route('/users', @AdminGetUsers);
    BrookHTTPRouter.Route('/stats', @AdminGetStats);
  end
);
```

## Intégration base de données

Brook fonctionne parfaitement avec SQLdb pour accéder aux bases de données.

### Configuration de la connexion

```pascal
uses
  sqldb, mysql57conn, pqconnection;

var
  DBConnection: TSQLConnection;
  DBTransaction: TSQLTransaction;

procedure InitDatabase;
begin
  // MySQL
  DBConnection := TMySQL57Connection.Create(nil);
  DBConnection.HostName := 'localhost';
  DBConnection.DatabaseName := 'myapp';
  DBConnection.UserName := 'root';
  DBConnection.Password := 'password';

  // PostgreSQL
  // DBConnection := TPQConnection.Create(nil);
  // DBConnection.HostName := 'localhost';
  // DBConnection.DatabaseName := 'myapp';
  // DBConnection.UserName := 'postgres';
  // DBConnection.Password := 'password';

  DBTransaction := TSQLTransaction.Create(nil);
  DBTransaction.Database := DBConnection;
  DBConnection.Transaction := DBTransaction;

  DBConnection.Open;
  WriteLn('Connexion BDD établie');
end;
```

### GET avec base de données

```pascal
procedure GetUsersFromDB(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  Query: TSQLQuery;
  JSONArray: TJSONArray;
  User: TJSONObject;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := DBConnection;
    Query.SQL.Text := 'SELECT id, name, email, active FROM users';
    Query.Open;

    JSONArray := TJSONArray.Create;
    try
      while not Query.EOF do
      begin
        User := TJSONObject.Create;
        User.Add('id', Query.FieldByName('id').AsInteger);
        User.Add('name', Query.FieldByName('name').AsString);
        User.Add('email', Query.FieldByName('email').AsString);
        User.Add('active', Query.FieldByName('active').AsBoolean);

        JSONArray.Add(User);
        Query.Next;
      end;

      AResponse.Send(JSONArray.AsJSON, 'application/json');
    finally
      JSONArray.Free;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;
```

### POST avec base de données

```pascal
procedure CreateUserInDB(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  Query: TSQLQuery;
  Name, Email: string;
  NewID: Integer;
begin
  try
    JSONData := GetJSON(ARequest.Body.AsString);
    try
      JSONObj := TJSONObject(JSONData);

      Name := JSONObj.Get('name', '');
      Email := JSONObj.Get('email', '');

      if (Name = '') or (Email = '') then
      begin
        AResponse.SendStatus(400, '{"error": "Missing fields"}', 'application/json');
        Exit;
      end;

      // Insertion en BDD
      Query := TSQLQuery.Create(nil);
      try
        Query.Database := DBConnection;
        Query.SQL.Text := 'INSERT INTO users (name, email) VALUES (:name, :email)';
        Query.Params.ParamByName('name').AsString := Name;
        Query.Params.ParamByName('email').AsString := Email;
        Query.ExecSQL;

        DBTransaction.Commit;

        // Récupérer l'ID inséré
        Query.SQL.Text := 'SELECT LAST_INSERT_ID() as id';  // MySQL
        // Query.SQL.Text := 'SELECT lastval() as id';  // PostgreSQL
        Query.Open;
        NewID := Query.FieldByName('id').AsInteger;
        Query.Close;

        // Réponse
        JSONObj := TJSONObject.Create;
        try
          JSONObj.Add('id', NewID);
          JSONObj.Add('name', Name);
          JSONObj.Add('email', Email);

          AResponse.SendStatus(201, JSONObj.AsJSON, 'application/json');
        finally
          JSONObj.Free;
        end;
      finally
        Query.Free;
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
    begin
      DBTransaction.Rollback;
      AResponse.SendStatus(500,
        Format('{"error": "Database error: %s"}', [E.Message]),
        'application/json');
    end;
  end;
end;
```

### PUT avec base de données

```pascal
procedure UpdateUserInDB(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  Query: TSQLQuery;
  UserID: Integer;
  Name, Email: string;
begin
  UserID := StrToIntDef(ARequest.Params['id'], 0);

  if UserID = 0 then
  begin
    AResponse.SendStatus(400, '{"error": "Invalid ID"}', 'application/json');
    Exit;
  end;

  try
    JSONData := GetJSON(ARequest.Body.AsString);
    try
      JSONObj := TJSONObject(JSONData);

      Name := JSONObj.Get('name', '');
      Email := JSONObj.Get('email', '');

      Query := TSQLQuery.Create(nil);
      try
        Query.Database := DBConnection;

        // Vérifier que l'utilisateur existe
        Query.SQL.Text := 'SELECT COUNT(*) as cnt FROM users WHERE id = :id';
        Query.Params.ParamByName('id').AsInteger := UserID;
        Query.Open;

        if Query.FieldByName('cnt').AsInteger = 0 then
        begin
          AResponse.SendStatus(404, '{"error": "User not found"}', 'application/json');
          Query.Close;
          Exit;
        end;
        Query.Close;

        // Mise à jour
        Query.SQL.Text := 'UPDATE users SET name = :name, email = :email WHERE id = :id';
        Query.Params.ParamByName('id').AsInteger := UserID;
        Query.Params.ParamByName('name').AsString := Name;
        Query.Params.ParamByName('email').AsString := Email;
        Query.ExecSQL;

        DBTransaction.Commit;

        // Récupérer l'utilisateur mis à jour
        Query.SQL.Text := 'SELECT id, name, email, active FROM users WHERE id = :id';
        Query.Params.ParamByName('id').AsInteger := UserID;
        Query.Open;

        JSONObj := TJSONObject.Create;
        try
          JSONObj.Add('id', Query.FieldByName('id').AsInteger);
          JSONObj.Add('name', Query.FieldByName('name').AsString);
          JSONObj.Add('email', Query.FieldByName('email').AsString);
          JSONObj.Add('active', Query.FieldByName('active').AsBoolean);

          AResponse.Send(JSONObj.AsJSON, 'application/json');
        finally
          JSONObj.Free;
        end;

        Query.Close;
      finally
        Query.Free;
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
    begin
      DBTransaction.Rollback;
      AResponse.SendStatus(500,
        Format('{"error": "%s"}', [E.Message]),
        'application/json');
    end;
  end;
end;
```

### DELETE avec base de données

```pascal
procedure DeleteUserFromDB(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  Query: TSQLQuery;
  UserID: Integer;
begin
  UserID := StrToIntDef(ARequest.Params['id'], 0);

  if UserID = 0 then
  begin
    AResponse.SendStatus(400, '{"error": "Invalid ID"}', 'application/json');
    Exit;
  end;

  try
    Query := TSQLQuery.Create(nil);
    try
      Query.Database := DBConnection;

      // Vérifier l'existence
      Query.SQL.Text := 'SELECT COUNT(*) as cnt FROM users WHERE id = :id';
      Query.Params.ParamByName('id').AsInteger := UserID;
      Query.Open;

      if Query.FieldByName('cnt').AsInteger = 0 then
      begin
        AResponse.SendStatus(404, '{"error": "User not found"}', 'application/json');
        Query.Close;
        Exit;
      end;
      Query.Close;

      // Suppression
      Query.SQL.Text := 'DELETE FROM users WHERE id = :id';
      Query.Params.ParamByName('id').AsInteger := UserID;
      Query.ExecSQL;

      DBTransaction.Commit;

      // Réponse 204 No Content
      AResponse.SendStatus(204, '', '');
    finally
      Query.Free;
    end;
  except
    on E: Exception do
    begin
      DBTransaction.Rollback;
      AResponse.SendStatus(500,
        Format('{"error": "%s"}', [E.Message]),
        'application/json');
    end;
  end;
end;
```

### Pagination

```pascal
procedure GetUsersPaginated(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  Query: TSQLQuery;
  JSONArray: TJSONArray;
  JSONObj, ResultObj: TJSONObject;
  Page, Limit, Offset, TotalCount: Integer;
begin
  // Paramètres de pagination
  Page := StrToIntDef(ARequest.Query['page'], 1);
  Limit := StrToIntDef(ARequest.Query['limit'], 10);

  // Limites de sécurité
  if Limit > 100 then Limit := 100;
  if Limit < 1 then Limit := 10;
  if Page < 1 then Page := 1;

  Offset := (Page - 1) * Limit;

  try
    Query := TSQLQuery.Create(nil);
    try
      Query.Database := DBConnection;

      // Compter le total
      Query.SQL.Text := 'SELECT COUNT(*) as total FROM users';
      Query.Open;
      TotalCount := Query.FieldByName('total').AsInteger;
      Query.Close;

      // Récupérer la page
      Query.SQL.Text := 'SELECT id, name, email, active FROM users ' +
                        'ORDER BY id LIMIT :limit OFFSET :offset';
      Query.Params.ParamByName('limit').AsInteger := Limit;
      Query.Params.ParamByName('offset').AsInteger := Offset;
      Query.Open;

      JSONArray := TJSONArray.Create;
      try
        while not Query.EOF do
        begin
          JSONObj := TJSONObject.Create;
          JSONObj.Add('id', Query.FieldByName('id').AsInteger);
          JSONObj.Add('name', Query.FieldByName('name').AsString);
          JSONObj.Add('email', Query.FieldByName('email').AsString);
          JSONObj.Add('active', Query.FieldByName('active').AsBoolean);

          JSONArray.Add(JSONObj);
          Query.Next;
        end;

        // Construire la réponse avec métadonnées
        ResultObj := TJSONObject.Create;
        try
          ResultObj.Add('data', JSONArray);
          ResultObj.Add('page', Page);
          ResultObj.Add('limit', Limit);
          ResultObj.Add('total', TotalCount);
          ResultObj.Add('total_pages', (TotalCount + Limit - 1) div Limit);

          AResponse.Send(ResultObj.AsJSON, 'application/json');
        finally
          ResultObj.Free;
        end;
      except
        JSONArray.Free;
        raise;
      end;

      Query.Close;
    finally
      Query.Free;
    end;
  except
    on E: Exception do
      AResponse.SendStatus(500,
        Format('{"error": "%s"}', [E.Message]),
        'application/json');
  end;
end;
```

**Utilisation** :

```bash
# Page 1 (10 éléments par défaut)
curl http://localhost:8080/api/users?page=1

# Page 2 avec 20 éléments
curl http://localhost:8080/api/users?page=2&limit=20
```

### Recherche et filtres

```pascal
procedure SearchUsers(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  Query: TSQLQuery;
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  SearchTerm, SQLWhere: string;
begin
  SearchTerm := ARequest.Query['q'];

  if SearchTerm = '' then
  begin
    AResponse.SendStatus(400, '{"error": "Missing search term"}', 'application/json');
    Exit;
  end;

  try
    Query := TSQLQuery.Create(nil);
    try
      Query.Database := DBConnection;

      // Recherche dans name et email
      Query.SQL.Text :=
        'SELECT id, name, email, active FROM users ' +
        'WHERE name LIKE :search OR email LIKE :search ' +
        'ORDER BY name LIMIT 50';

      Query.Params.ParamByName('search').AsString := '%' + SearchTerm + '%';
      Query.Open;

      JSONArray := TJSONArray.Create;
      try
        while not Query.EOF do
        begin
          JSONObj := TJSONObject.Create;
          JSONObj.Add('id', Query.FieldByName('id').AsInteger);
          JSONObj.Add('name', Query.FieldByName('name').AsString);
          JSONObj.Add('email', Query.FieldByName('email').AsString);
          JSONObj.Add('active', Query.FieldByName('active').AsBoolean);

          JSONArray.Add(JSONObj);
          Query.Next;
        end;

        AResponse.Send(JSONArray.AsJSON, 'application/json');
      finally
        JSONArray.Free;
      end;

      Query.Close;
    finally
      Query.Free;
    end;
  except
    on E: Exception do
      AResponse.SendStatus(500,
        Format('{"error": "%s"}', [E.Message]),
        'application/json');
  end;
end;

// Enregistrer la route
BrookHTTPRouter.Route('/api/users/search', @SearchUsers).Method('GET');
```

**Utilisation** :

```bash
curl "http://localhost:8080/api/users/search?q=alice"
```

## Gestion des erreurs centralisée

### Middleware de gestion d'erreurs

```pascal
var
  ErrorHandler: TBrookHTTPErrorHandler;

procedure HandleError(AException: Exception; ARequest: TBrookHTTPRequest;
                      AResponse: TBrookHTTPResponse);
var
  JSONObj: TJSONObject;
  StatusCode: Integer;
begin
  // Déterminer le code d'erreur
  StatusCode := 500;

  if AException is EHTTPError then
    StatusCode := EHTTPError(AException).StatusCode
  else if Pos('not found', AException.Message) > 0 then
    StatusCode := 404
  else if Pos('validation', AException.Message) > 0 then
    StatusCode := 400;

  // Logger l'erreur
  WriteLn(Format('[ERROR] %s: %s', [AException.ClassName, AException.Message]));

  // Réponse JSON
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('error', AException.Message);
    JSONObj.Add('type', AException.ClassName);
    JSONObj.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));

    AResponse.SendStatus(StatusCode, JSONObj.AsJSON, 'application/json');
  finally
    JSONObj.Free;
  end;
end;

begin
  // Configurer le gestionnaire d'erreurs
  ErrorHandler := BrookApp.ErrorHandler;
  ErrorHandler.OnError := @HandleError;

  // Configuration et démarrage
  BrookApp.Port := 8080;
  BrookApp.Run;
end.
```

## Validation des données

### Classe de validation

```pascal
type
  TValidator = class
  public
    class function ValidateEmail(const Email: string): Boolean;
    class function ValidateRequired(const Value: string): Boolean;
    class function ValidateLength(const Value: string; MinLen, MaxLen: Integer): Boolean;
    class function ValidateJSON(const JSONStr: string; out ErrorMsg: string): Boolean;
  end;

class function TValidator.ValidateEmail(const Email: string): Boolean;
var
  AtPos: Integer;
begin
  Result := False;
  if Email = '' then Exit;

  AtPos := Pos('@', Email);
  if (AtPos > 1) and (AtPos < Length(Email)) then
    Result := Pos('.', Email, AtPos) > AtPos;
end;

class function TValidator.ValidateRequired(const Value: string): Boolean;
begin
  Result := Trim(Value) <> '';
end;

class function TValidator.ValidateLength(const Value: string; MinLen, MaxLen: Integer): Boolean;
var
  Len: Integer;
begin
  Len := Length(Value);
  Result := (Len >= MinLen) and (Len <= MaxLen);
end;

class function TValidator.ValidateJSON(const JSONStr: string; out ErrorMsg: string): Boolean;
var
  JSONData: TJSONData;
begin
  Result := False;
  ErrorMsg := '';

  try
    JSONData := GetJSON(JSONStr);
    try
      Result := True;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      ErrorMsg := E.Message;
  end;
end;
```

### Utilisation dans les routes

```pascal
procedure CreateUserValidated(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  Name, Email, ErrorMsg: string;
  Errors: TJSONArray;
begin
  // Valider le JSON
  if not TValidator.ValidateJSON(ARequest.Body.AsString, ErrorMsg) then
  begin
    AResponse.SendStatus(400,
      Format('{"error": "Invalid JSON: %s"}', [ErrorMsg]),
      'application/json');
    Exit;
  end;

  JSONData := GetJSON(ARequest.Body.AsString);
  try
    JSONObj := TJSONObject(JSONData);

    Name := JSONObj.Get('name', '');
    Email := JSONObj.Get('email', '');

    // Collecter les erreurs de validation
    Errors := TJSONArray.Create;
    try
      if not TValidator.ValidateRequired(Name) then
        Errors.Add('Name is required');

      if not TValidator.ValidateLength(Name, 2, 100) then
        Errors.Add('Name must be between 2 and 100 characters');

      if not TValidator.ValidateRequired(Email) then
        Errors.Add('Email is required');

      if not TValidator.ValidateEmail(Email) then
        Errors.Add('Invalid email format');

      // Si erreurs, retourner 422 Unprocessable Entity
      if Errors.Count > 0 then
      begin
        JSONObj := TJSONObject.Create;
        try
          JSONObj.Add('errors', Errors);
          AResponse.SendStatus(422, JSONObj.AsJSON, 'application/json');
        finally
          JSONObj.Free;
        end;
        Exit;
      end;
    except
      Errors.Free;
      raise;
    end;

    // Validation OK, créer l'utilisateur
    // ... (code de création)

  finally
    JSONData.Free;
  end;
end;
```

## Configuration et environnements

### Fichier de configuration

**config.ini** :

```ini
[server]
port=8080
host=0.0.0.0

[database]
host=localhost
name=myapp
user=root
password=secret
port=3306

[app]
environment=development
debug=true
log_level=info
```

### Lecture de la configuration

```pascal
uses
  IniFiles;

type
  TAppConfig = class
  private
    FIni: TIniFile;
  public
    ServerPort: Integer;
    ServerHost: string;
    DBHost: string;
    DBName: string;
    DBUser: string;
    DBPassword: string;
    DBPort: Integer;
    Environment: string;
    Debug: Boolean;

    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Load;
  end;

constructor TAppConfig.Create(const FileName: string);
begin
  inherited Create;
  FIni := TIniFile.Create(FileName);
  Load;
end;

destructor TAppConfig.Destroy;
begin
  FIni.Free;
  inherited;
end;

procedure TAppConfig.Load;
begin
  // Server
  ServerPort := FIni.ReadInteger('server', 'port', 8080);
  ServerHost := FIni.ReadString('server', 'host', '0.0.0.0');

  // Database
  DBHost := FIni.ReadString('database', 'host', 'localhost');
  DBName := FIni.ReadString('database', 'name', 'myapp');
  DBUser := FIni.ReadString('database', 'user', 'root');
  DBPassword := FIni.ReadString('database', 'password', '');
  DBPort := FIni.ReadInteger('database', 'port', 3306);

  // App
  Environment := FIni.ReadString('app', 'environment', 'development');
  Debug := FIni.ReadBool('app', 'debug', True);
end;

var
  Config: TAppConfig;

begin
  Config := TAppConfig.Create('config.ini');
  try
    // Utiliser la configuration
    BrookApp.Port := Config.ServerPort;

    // Configurer la BDD avec les paramètres
    DBConnection.HostName := Config.DBHost;
    DBConnection.DatabaseName := Config.DBName;
    // ...

    BrookApp.Run;
  finally
    Config.Free;
  end;
end.
```

## Déploiement en production

### Compilation optimisée

```bash
# Compilation avec optimisations maximales
fpc -O3 -MObjFPC -Scghi -XX -CX api.lpr

# Options:
# -O3: Optimisation maximale
# -XX: Smartlinking (réduire la taille)
# -CX: Smartlinking dynamique
```

### Service systemd (Linux)

**Créer** : `/etc/systemd/system/myapi.service`

```ini
[Unit]
Description=API REST Brook Framework
After=network.target mysql.service

[Service]
Type=simple
User=www-data
Group=www-data
WorkingDirectory=/var/www/myapi
ExecStart=/var/www/myapi/api
Restart=always
RestartSec=5

# Variables d'environnement
Environment="APP_ENV=production"
Environment="APP_CONFIG=/var/www/myapi/config.ini"

# Limites
LimitNOFILE=65536
MemoryLimit=512M

# Logs
StandardOutput=journal
StandardError=journal
SyslogIdentifier=myapi

[Install]
WantedBy=multi-user.target
```

**Activer et démarrer** :

```bash
sudo systemctl daemon-reload
sudo systemctl enable myapi
sudo systemctl start myapi
sudo systemctl status myapi

# Logs
sudo journalctl -u myapi -f
```

### Reverse proxy Nginx

**Configuration Nginx** : `/etc/nginx/sites-available/myapi`

```nginx
upstream brook_api {
    server 127.0.0.1:8080;
    keepalive 32;
}

server {
    listen 80;
    server_name api.example.com;

    # Redirection HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name api.example.com;

    # SSL
    ssl_certificate /etc/letsencrypt/live/api.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/api.example.com/privkey.pem;

    # Logs
    access_log /var/log/nginx/api-access.log;
    error_log /var/log/nginx/api-error.log;

    # Proxy vers Brook
    location / {
        proxy_pass http://brook_api;
        proxy_http_version 1.1;

        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # Timeouts
        proxy_connect_timeout 60s;
        proxy_send_timeout 60s;
        proxy_read_timeout 60s;

        # Buffers
        proxy_buffering on;
        proxy_buffer_size 4k;
        proxy_buffers 8 4k;

        # Keep-alive
        proxy_set_header Connection "";
    }

    # Health check
    location /health {
        proxy_pass http://brook_api/health;
        access_log off;
    }

    # Rate limiting
    limit_req_zone $binary_remote_addr zone=api_limit:10m rate=100r/s;
    limit_req zone=api_limit burst=200 nodelay;
}
```

**Activer** :

```bash
sudo ln -s /etc/nginx/sites-available/myapi /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

### Docker (optionnel)

**Dockerfile** :

```dockerfile
FROM debian:bookworm-slim

# Installer FreePascal
RUN apt-get update && \
    apt-get install -y fpc && \
    rm -rf /var/lib/apt/lists/*

# Copier les sources
WORKDIR /app
COPY . .

# Compiler
RUN fpc -O3 -MObjFPC -Scghi api.lpr

# Créer un utilisateur non-root
RUN useradd -r -s /bin/false apiuser && \
    chown -R apiuser:apiuser /app

USER apiuser

# Exposer le port
EXPOSE 8080

# Démarrer l'application
CMD ["./api"]
```

**docker-compose.yml** :

```yaml
version: '3.8'

services:
  api:
    build: .
    ports:
      - "8080:8080"
    environment:
      - APP_ENV=production
    volumes:
      - ./config.ini:/app/config.ini:ro
    depends_on:
      - db
    restart: unless-stopped

  db:
    image: mysql:8.0
    environment:
      MYSQL_ROOT_PASSWORD: secret
      MYSQL_DATABASE: myapp
    volumes:
      - db_data:/var/lib/mysql
    restart: unless-stopped

volumes:
  db_data:
```

**Utilisation** :

```bash
docker-compose up -d
docker-compose logs -f api
```

## Monitoring et logging

### Endpoint de santé

```pascal
procedure HandleHealth(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONObj: TJSONObject;
  DBConnected: Boolean;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('status', 'ok');
    JSONObj.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));
    JSONObj.Add('version', '1.0.0');

    // Vérifier la connexion BDD
    try
      DBConnected := DBConnection.Connected;
      JSONObj.Add('database', IfThen(DBConnected, 'connected', 'disconnected'));
    except
      JSONObj.Add('database', 'error');
      DBConnected := False;
    end;

    // Code de statut selon l'état
    if DBConnected then
      AResponse.Send(JSONObj.AsJSON, 'application/json')
    else
      AResponse.SendStatus(503, JSONObj.AsJSON, 'application/json');
  finally
    JSONObj.Free;
  end;
end;

BrookHTTPRouter.Route('/health', @HandleHealth);
```

### Logging structuré

```pascal
type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

procedure Log(Level: TLogLevel; const Msg: string);
const
  LevelStr: array[TLogLevel] of string = ('DEBUG', 'INFO', 'WARN', 'ERROR');
var
  LogFile: TextFile;
  Timestamp: string;
begin
  Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // Console
  WriteLn(Format('[%s] [%s] %s', [Timestamp, LevelStr[Level], Msg]));

  // Fichier (en production)
  if Config.Environment = 'production' then
  begin
    AssignFile(LogFile, 'app.log');
    try
      if FileExists('app.log') then
        Append(LogFile)
      else
        Rewrite(LogFile);

      WriteLn(LogFile, Format('[%s] [%s] %s', [Timestamp, LevelStr[Level], Msg]));
    finally
      CloseFile(LogFile);
    end;
  end;
end;

// Utilisation
Log(llInfo, 'Application démarrée');
Log(llError, 'Erreur de connexion BDD: ' + E.Message);
```

## Tests de l'API

### Tests avec curl

```bash
# GET
curl -X GET http://localhost:8080/api/users

# POST
curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{"name":"Alice","email":"alice@example.com"}'

# PUT
curl -X PUT http://localhost:8080/api/users/1 \
  -H "Content-Type: application/json" \
  -d '{"name":"Alice Smith","email":"alice.smith@example.com"}'

# DELETE
curl -X DELETE http://localhost:8080/api/users/1

# Avec authentification
curl -X GET http://localhost:8080/api/admin/users \
  -H "Authorization: Bearer secret-token-123"
```

### Script de test bash

**test_api.sh** :

```bash
#!/bin/bash

API_URL="http://localhost:8080/api"

echo "=== Tests API REST ==="

# Test 1: Health check
echo -n "Health check... "
HEALTH=$(curl -s $API_URL/../health | jq -r '.status')
if [ "$HEALTH" = "ok" ]; then
    echo "✓ OK"
else
    echo "✗ FAILED"
    exit 1
fi

# Test 2: Créer un utilisateur
echo -n "Create user... "
RESPONSE=$(curl -s -X POST $API_URL/users \
    -H "Content-Type: application/json" \
    -d '{"name":"Test User","email":"test@example.com"}')
USER_ID=$(echo $RESPONSE | jq -r '.id')
if [ "$USER_ID" != "null" ] && [ "$USER_ID" != "" ]; then
    echo "✓ OK (ID: $USER_ID)"
else
    echo "✗ FAILED"
    exit 1
fi

# Test 3: Récupérer l'utilisateur
echo -n "Get user... "
NAME=$(curl -s $API_URL/users/$USER_ID | jq -r '.name')
if [ "$NAME" = "Test User" ]; then
    echo "✓ OK"
else
    echo "✗ FAILED"
    exit 1
fi

# Test 4: Modifier l'utilisateur
echo -n "Update user... "
curl -s -X PUT $API_URL/users/$USER_ID \
    -H "Content-Type: application/json" \
    -d '{"name":"Updated User","email":"updated@example.com"}' > /dev/null
NAME=$(curl -s $API_URL/users/$USER_ID | jq -r '.name')
if [ "$NAME" = "Updated User" ]; then
    echo "✓ OK"
else
    echo "✗ FAILED"
    exit 1
fi

# Test 5: Lister les utilisateurs
echo -n "List users... "
COUNT=$(curl -s $API_URL/users | jq '. | length')
if [ "$COUNT" -gt 0 ]; then
    echo "✓ OK ($COUNT users)"
else
    echo "✗ FAILED"
    exit 1
fi

# Test 6: Supprimer l'utilisateur
echo -n "Delete user... "
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" -X DELETE $API_URL/users/$USER_ID)
if [ "$HTTP_CODE" = "204" ]; then
    echo "✓ OK"
else
    echo "✗ FAILED (HTTP $HTTP_CODE)"
    exit 1
fi

# Test 7: Vérifier la suppression
echo -n "Verify deletion... "
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" $API_URL/users/$USER_ID)
if [ "$HTTP_CODE" = "404" ]; then
    echo "✓ OK"
else
    echo "✗ FAILED (HTTP $HTTP_CODE)"
    exit 1
fi

echo ""
echo "✓ Tous les tests sont passés avec succès !"
```

**Rendre exécutable et lancer** :

```bash
chmod +x test_api.sh
./test_api.sh
```

### Tests avec Postman/Insomnia

**Collection Postman** (fichier JSON exportable) :

```json
{
  "info": {
    "name": "Brook API",
    "schema": "https://schema.getpostman.com/json/collection/v2.1.0/collection.json"
  },
  "item": [
    {
      "name": "Health Check",
      "request": {
        "method": "GET",
        "header": [],
        "url": {
          "raw": "{{base_url}}/health",
          "host": ["{{base_url}}"],
          "path": ["health"]
        }
      }
    },
    {
      "name": "List Users",
      "request": {
        "method": "GET",
        "header": [],
        "url": {
          "raw": "{{base_url}}/api/users",
          "host": ["{{base_url}}"],
          "path": ["api", "users"]
        }
      }
    },
    {
      "name": "Create User",
      "request": {
        "method": "POST",
        "header": [
          {
            "key": "Content-Type",
            "value": "application/json"
          }
        ],
        "body": {
          "mode": "raw",
          "raw": "{\n  \"name\": \"Alice\",\n  \"email\": \"alice@example.com\"\n}"
        },
        "url": {
          "raw": "{{base_url}}/api/users",
          "host": ["{{base_url}}"],
          "path": ["api", "users"]
        }
      }
    }
  ],
  "variable": [
    {
      "key": "base_url",
      "value": "http://localhost:8080"
    }
  ]
}
```

## Bonnes pratiques

### Structure de projet recommandée

```
myapi/
├── src/
│   ├── controllers/          # Gestionnaires de routes
│   │   ├── users.pas
│   │   ├── auth.pas
│   │   └── products.pas
│   ├── models/               # Structures de données
│   │   ├── user.pas
│   │   └── product.pas
│   ├── middleware/           # Middlewares
│   │   ├── auth.pas
│   │   ├── cors.pas
│   │   └── logging.pas
│   ├── database/             # Accès base de données
│   │   ├── connection.pas
│   │   └── migrations.pas
│   ├── utils/                # Utilitaires
│   │   ├── validation.pas
│   │   ├── json_helper.pas
│   │   └── crypto.pas
│   └── config/               # Configuration
│       └── config.pas
├── tests/                    # Tests
│   ├── test_users.sh
│   └── test_api.sh
├── api.lpr                   # Programme principal
├── config.ini                # Configuration
├── README.md
└── Makefile                  # Automatisation build
```

### Programme principal modulaire

**api.lpr** :

```pascal
program API;

{$mode objfpc}{$H+}

uses
  BrookApplication,
  BrookHTTPRouter,
  // Controllers
  UserController,
  AuthController,
  // Middleware
  CORSMiddleware,
  LoggingMiddleware,
  AuthMiddleware,
  // Config et utils
  AppConfig,
  DatabaseConnection;

procedure SetupMiddlewares;
begin
  BrookHTTPRouter.Use(@CORSMiddleware.Handle);
  BrookHTTPRouter.Use(@LoggingMiddleware.Handle);
end;

procedure SetupRoutes;
begin
  // Routes publiques
  BrookHTTPRouter.Route('/health', @HandleHealth);
  BrookHTTPRouter.Route('/api/auth/login', @AuthController.Login).Method('POST');

  // Routes protégées
  BrookHTTPRouter.Group('/api',
    procedure
    begin
      BrookHTTPRouter.Use(@AuthMiddleware.Check);

      // Users
      BrookHTTPRouter.Route('/users', @UserController.List).Method('GET');
      BrookHTTPRouter.Route('/users/:id', @UserController.Get).Method('GET');
      BrookHTTPRouter.Route('/users', @UserController.Create).Method('POST');
      BrookHTTPRouter.Route('/users/:id', @UserController.Update).Method('PUT');
      BrookHTTPRouter.Route('/users/:id', @UserController.Delete).Method('DELETE');
    end
  );
end;

procedure HandleHealth(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONStr: string;
begin
  JSONStr := '{"status":"ok","version":"1.0.0"}';
  AResponse.Send(JSONStr, 'application/json');
end;

var
  Config: TAppConfig;

begin
  try
    // Charger la configuration
    Config := TAppConfig.Create('config.ini');
    try
      // Initialiser la BDD
      InitDatabase(Config);

      // Configuration du serveur
      BrookApp.Port := Config.ServerPort;

      // Setup middlewares et routes
      SetupMiddlewares;
      SetupRoutes;

      // Démarrer
      WriteLn(Format('API démarrée sur http://%s:%d', [Config.ServerHost, Config.ServerPort]));
      WriteLn('Appuyez sur Ctrl+C pour arrêter');

      BrookApp.Run;
    finally
      Config.Free;
      CloseDatabase;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERREUR FATALE: ', E.Message);
      Halt(1);
    end;
  end;
end.
```

### Controller modulaire

**controllers/users.pas** :

```pascal
unit UserController;

{$mode objfpc}{$H+}

interface

uses
  BrookHTTPRequest,
  BrookHTTPResponse;

type
  TUserController = class
  public
    class procedure List(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    class procedure Get(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    class procedure Create(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    class procedure Update(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    class procedure Delete(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
  end;

implementation

uses
  SysUtils, fpjson, jsonparser,
  UserModel, DatabaseConnection;

class procedure TUserController.List(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  Users: TUserArray;
  JSONArray: TJSONArray;
  i: Integer;
begin
  try
    Users := TUserModel.GetAll;

    JSONArray := TJSONArray.Create;
    try
      for i := 0 to High(Users) do
        JSONArray.Add(Users[i].ToJSON);

      AResponse.Send(JSONArray.AsJSON, 'application/json');
    finally
      JSONArray.Free;
    end;
  except
    on E: Exception do
      AResponse.SendStatus(500,
        Format('{"error":"%s"}', [E.Message]),
        'application/json');
  end;
end;

class procedure TUserController.Get(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  UserID: Integer;
  User: TUser;
  JSONObj: TJSONObject;
begin
  UserID := StrToIntDef(ARequest.Params['id'], 0);

  if UserID = 0 then
  begin
    AResponse.SendStatus(400, '{"error":"Invalid ID"}', 'application/json');
    Exit;
  end;

  try
    if TUserModel.GetByID(UserID, User) then
    begin
      JSONObj := User.ToJSON;
      try
        AResponse.Send(JSONObj.AsJSON, 'application/json');
      finally
        JSONObj.Free;
      end;
    end
    else
      AResponse.SendStatus(404, '{"error":"User not found"}', 'application/json');
  except
    on E: Exception do
      AResponse.SendStatus(500,
        Format('{"error":"%s"}', [E.Message]),
        'application/json');
  end;
end;

class procedure TUserController.Create(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONData: TJSONData;
  User: TUser;
  NewID: Integer;
begin
  try
    JSONData := GetJSON(ARequest.Body.AsString);
    try
      User.FromJSON(TJSONObject(JSONData));

      // Valider
      if not User.Validate then
      begin
        AResponse.SendStatus(422, '{"error":"Validation failed"}', 'application/json');
        Exit;
      end;

      // Créer en BDD
      NewID := TUserModel.Create(User);
      User.ID := NewID;

      // Réponse
      AResponse.SendStatus(201, User.ToJSON.AsJSON, 'application/json');
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      AResponse.SendStatus(500,
        Format('{"error":"%s"}', [E.Message]),
        'application/json');
  end;
end;

// Méthodes Update et Delete similaires...

end.
```

### Modèle de données

**models/user.pas** :

```pascal
unit UserModel;

{$mode objfpc}{$H+}

interface

uses
  fpjson, SysUtils;

type
  TUser = record
    ID: Integer;
    Name: string;
    Email: string;
    Active: Boolean;
    CreatedAt: TDateTime;

    function ToJSON: TJSONObject;
    procedure FromJSON(JSONObj: TJSONObject);
    function Validate: Boolean;
  end;

  TUserArray = array of TUser;

  TUserModel = class
  public
    class function GetAll: TUserArray;
    class function GetByID(ID: Integer; out User: TUser): Boolean;
    class function Create(const User: TUser): Integer;
    class function Update(const User: TUser): Boolean;
    class function Delete(ID: Integer): Boolean;
  end;

implementation

uses
  sqldb, DatabaseConnection;

{ TUser }

function TUser.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', ID);
  Result.Add('name', Name);
  Result.Add('email', Email);
  Result.Add('active', Active);
  Result.Add('created_at', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', CreatedAt));
end;

procedure TUser.FromJSON(JSONObj: TJSONObject);
begin
  Name := JSONObj.Get('name', '');
  Email := JSONObj.Get('email', '');
  Active := JSONObj.Get('active', True);
end;

function TUser.Validate: Boolean;
begin
  Result := (Trim(Name) <> '') and
            (Trim(Email) <> '') and
            (Pos('@', Email) > 0);
end;

{ TUserModel }

class function TUserModel.GetAll: TUserArray;
var
  Query: TSQLQuery;
  i: Integer;
begin
  SetLength(Result, 0);

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := GetDBConnection;
    Query.SQL.Text := 'SELECT id, name, email, active, created_at FROM users ORDER BY id';
    Query.Open;

    SetLength(Result, Query.RecordCount);
    i := 0;

    while not Query.EOF do
    begin
      Result[i].ID := Query.FieldByName('id').AsInteger;
      Result[i].Name := Query.FieldByName('name').AsString;
      Result[i].Email := Query.FieldByName('email').AsString;
      Result[i].Active := Query.FieldByName('active').AsBoolean;
      Result[i].CreatedAt := Query.FieldByName('created_at').AsDateTime;

      Inc(i);
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

class function TUserModel.GetByID(ID: Integer; out User: TUser): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := GetDBConnection;
    Query.SQL.Text := 'SELECT id, name, email, active, created_at FROM users WHERE id = :id';
    Query.Params.ParamByName('id').AsInteger := ID;
    Query.Open;

    if not Query.EOF then
    begin
      User.ID := Query.FieldByName('id').AsInteger;
      User.Name := Query.FieldByName('name').AsString;
      User.Email := Query.FieldByName('email').AsString;
      User.Active := Query.FieldByName('active').AsBoolean;
      User.CreatedAt := Query.FieldByName('created_at').AsDateTime;
      Result := True;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

class function TUserModel.Create(const User: TUser): Integer;
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := GetDBConnection;
    Query.SQL.Text := 'INSERT INTO users (name, email, active) VALUES (:name, :email, :active)';
    Query.Params.ParamByName('name').AsString := User.Name;
    Query.Params.ParamByName('email').AsString := User.Email;
    Query.Params.ParamByName('active').AsBoolean := User.Active;
    Query.ExecSQL;

    GetDBTransaction.Commit;

    // Récupérer l'ID
    Query.SQL.Text := 'SELECT LAST_INSERT_ID() as id';
    Query.Open;
    Result := Query.FieldByName('id').AsInteger;
    Query.Close;
  finally
    Query.Free;
  end;
end;

// Méthodes Update et Delete...

end.
```

### Authentification JWT

**middleware/auth.pas** :

```pascal
unit AuthMiddleware;

{$mode objfpc}{$H+}

interface

uses
  BrookHTTPRequest,
  BrookHTTPResponse;

type
  TAuthMiddleware = class
  public
    class procedure Check(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    class function GenerateToken(UserID: Integer): string;
    class function ValidateToken(const Token: string; out UserID: Integer): Boolean;
  end;

implementation

uses
  SysUtils, base64, md5;

class procedure TAuthMiddleware.Check(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  AuthHeader, Token: string;
  UserID: Integer;
begin
  AuthHeader := ARequest.Headers['Authorization'];

  if AuthHeader = '' then
  begin
    AResponse.SendStatus(401, '{"error":"Missing authorization header"}', 'application/json');
    Halt;
  end;

  // Format: "Bearer TOKEN"
  if Copy(AuthHeader, 1, 7) <> 'Bearer ' then
  begin
    AResponse.SendStatus(401, '{"error":"Invalid authorization format"}', 'application/json');
    Halt;
  end;

  Token := Copy(AuthHeader, 8, Length(AuthHeader));

  if not ValidateToken(Token, UserID) then
  begin
    AResponse.SendStatus(403, '{"error":"Invalid or expired token"}', 'application/json');
    Halt;
  end;

  // Stocker l'ID utilisateur dans la requête pour usage ultérieur
  ARequest.Values['user_id'] := IntToStr(UserID);
end;

class function TAuthMiddleware.GenerateToken(UserID: Integer): string;
var
  Payload, Secret, Signature: string;
begin
  Secret := 'your-secret-key-change-me';  // À stocker en config !

  // Payload simple: UserID + Timestamp
  Payload := Format('%d:%d', [UserID, DateTimeToUnix(Now)]);

  // Signature: MD5(Payload + Secret)
  Signature := MD5Print(MD5String(Payload + Secret));

  // Token: Base64(Payload).Signature
  Result := EncodeStringBase64(Payload) + '.' + Signature;
end;

class function TAuthMiddleware.ValidateToken(const Token: string; out UserID: Integer): Boolean;
var
  Payload, Signature, ExpectedSignature, Secret: string;
  Timestamp: Int64;
  DotPos, ColonPos: Integer;
begin
  Result := False;
  UserID := 0;

  // Découper le token par '.'
  DotPos := Pos('.', Token);
  if DotPos = 0 then Exit;

  try
    // Décoder le payload
    Payload := DecodeStringBase64(Copy(Token, 1, DotPos - 1));
    Signature := Copy(Token, DotPos + 1, Length(Token));

    // Vérifier la signature
    Secret := 'your-secret-key-change-me';
    ExpectedSignature := MD5Print(MD5String(Payload + Secret));

    if Signature <> ExpectedSignature then Exit;

    // Extraire UserID et Timestamp
    ColonPos := Pos(':', Payload);
    if ColonPos = 0 then Exit;

    UserID := StrToInt(Copy(Payload, 1, ColonPos - 1));
    Timestamp := StrToInt64(Copy(Payload, ColonPos + 1, Length(Payload)));

    // Vérifier l'expiration (24 heures)
    if (DateTimeToUnix(Now) - Timestamp) > 86400 then Exit;

    Result := True;
  except
    Result := False;
  end;
end;

end.
```

### Login endpoint

**controllers/auth.pas** :

```pascal
procedure Login(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  JSONData: TJSONData;
  JSONObj, ResponseObj: TJSONObject;
  Email, Password: string;
  UserID: Integer;
  Token: string;
begin
  try
    JSONData := GetJSON(ARequest.Body.AsString);
    try
      JSONObj := TJSONObject(JSONData);

      Email := JSONObj.Get('email', '');
      Password := JSONObj.Get('password', '');

      // Vérifier les identifiants (simplifié - utiliser password hashing en prod!)
      if not AuthenticateUser(Email, Password, UserID) then
      begin
        AResponse.SendStatus(401, '{"error":"Invalid credentials"}', 'application/json');
        Exit;
      end;

      // Générer le token
      Token := TAuthMiddleware.GenerateToken(UserID);

      // Réponse
      ResponseObj := TJSONObject.Create;
      try
        ResponseObj.Add('token', Token);
        ResponseObj.Add('user_id', UserID);
        ResponseObj.Add('expires_in', 86400);  // 24h en secondes

        AResponse.Send(ResponseObj.AsJSON, 'application/json');
      finally
        ResponseObj.Free;
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      AResponse.SendStatus(500,
        Format('{"error":"%s"}', [E.Message]),
        'application/json');
  end;
end;
```

## Makefile pour automatisation

**Makefile** :

```makefile
# Variables
FPC = fpc
FPC_FLAGS = -O3 -MObjFPC -Scghi -Fu./src
TARGET = api
SOURCES = api.lpr

# Répertoires
BUILD_DIR = build
BIN_DIR = bin

.PHONY: all clean run test deploy

all: $(TARGET)

$(TARGET): $(SOURCES)
	@echo "Compilation de l'API..."
	@mkdir -p $(BIN_DIR)
	$(FPC) $(FPC_FLAGS) -FE$(BIN_DIR) $(SOURCES)
	@echo "✓ Compilation terminée: $(BIN_DIR)/$(TARGET)"

clean:
	@echo "Nettoyage..."
	@rm -rf $(BIN_DIR) $(BUILD_DIR)
	@rm -f *.o *.ppu
	@echo "✓ Nettoyage terminé"

run: $(TARGET)
	@echo "Démarrage de l'API..."
	@cd $(BIN_DIR) && ./$(TARGET)

test:
	@echo "Exécution des tests..."
	@bash tests/test_api.sh

deploy: $(TARGET)
	@echo "Déploiement..."
	@sudo systemctl stop myapi || true
	@sudo cp $(BIN_DIR)/$(TARGET) /var/www/myapi/
	@sudo systemctl start myapi
	@echo "✓ Déploiement terminé"

install-deps:
	@echo "Installation des dépendances..."
	@git clone https://github.com/risoflora/brookframework.git libs/brook || true
	@echo "✓ Dépendances installées"

.SILENT: clean
```

**Utilisation** :

```bash
# Compiler
make

# Compiler et lancer
make run

# Tests
make test

# Déployer
make deploy

# Nettoyer
make clean
```

## Documentation de l'API

### README.md

````markdown
# API REST Brook Framework

API RESTful moderne construite avec Brook Framework et FreePascal.

## Installation

```bash
# Cloner le dépôt
git clone https://github.com/user/myapi.git
cd myapi

# Installer les dépendances
make install-deps

# Compiler
make

# Lancer
make run
```

## Endpoints

### Authentification

#### POST /api/auth/login
Authentifier un utilisateur et obtenir un token JWT.

**Request:**
```json
{
  "email": "user@example.com",
  "password": "secret"
}
```

**Response (200):**
```json
{
  "token": "eyJhbGc...",
  "user_id": 123,
  "expires_in": 86400
}
```

### Utilisateurs

#### GET /api/users
Liste tous les utilisateurs (authentification requise).

**Headers:**
````
Authorization: Bearer {token}
```

**Response (200):**
```json
[
  {
    "id": 1,
    "name": "Alice",
    "email": "alice@example.com",
    "active": true
  }
]
```

#### POST /api/users
Créer un utilisateur.

**Request:**
```json
{
  "name": "Bob",
  "email": "bob@example.com"
}
```

**Response (201):**
```json
{
  "id": 2,
  "name": "Bob",
  "email": "bob@example.com",
  "active": true
}
```

## Configuration

Copier `config.ini.example` vers `config.ini` et ajuster les paramètres.

## Tests

```bash
make test
```

## Déploiement

```bash
make deploy
```

## License

MIT
```

## Performance et optimisation

### Conseils de performance

1. **Pool de connexions BDD** :

```pascal
var
  ConnectionPool: array[0..9] of TSQLConnection;

procedure InitConnectionPool;
var
  i: Integer;
begin
  for i := 0 to 9 do
  begin
    ConnectionPool[i] := TMySQL57Connection.Create(nil);
    // Configuration...
    ConnectionPool[i].Open;
  end;
end;

function GetFreeConnection: TSQLConnection;
var
  i: Integer;
begin
  // Logique pour trouver une connexion libre
  // (simplifiée - utiliser un vrai pool en production)
  Result := ConnectionPool[0];
end;
```

2. **Cache des résultats** :

```pascal
type
  TCacheEntry = record
    Data: string;
    Timestamp: TDateTime;
  end;

var
  Cache: TFPHashList;

type
  TFetchFunc = function: string;

function GetCachedOrFetch(const Key: string; FetchFunc: TFetchFunc): string;
var
  Entry: TCacheEntry;
begin
  if Cache.Find(Key) <> nil then
  begin
    Entry := PCacheEntry(Cache.Find(Key))^;

    // Cache valide 5 minutes
    if (Now - Entry.Timestamp) < (5 / (24 * 60)) then
      Exit(Entry.Data);
  end;

  // Fetch depuis la source
  Result := FetchFunc();

  // Mettre en cache
  Entry.Data := Result;
  Entry.Timestamp := Now;
  Cache.Add(Key, @Entry);
end;
```

3. **Compression des réponses** :

```pascal
uses
  zstream;

procedure CompressResponse(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  Original, Compressed: TMemoryStream;
  Compressor: TCompressionStream;
begin
  if Pos('gzip', ARequest.Headers['Accept-Encoding']) > 0 then
  begin
    Original := TMemoryStream.Create;
    Compressed := TMemoryStream.Create;
    try
      // Données originales
      Original.Write(AResponse.Content[1], Length(AResponse.Content));
      Original.Position := 0;

      // Compression
      Compressor := TCompressionStream.Create(clDefault, Compressed);
      try
        Compressor.CopyFrom(Original, Original.Size);
      finally
        Compressor.Free;
      end;

      // Remplacer la réponse
      AResponse.Headers['Content-Encoding'] := 'gzip';
      Compressed.Position := 0;
      SetLength(AResponse.Content, Compressed.Size);
      Compressed.Read(AResponse.Content[1], Compressed.Size);
    finally
      Original.Free;
      Compressed.Free;
    end;
  end;
end;
```

## Conclusion

Brook Framework offre une solution moderne, élégante et performante pour créer des API REST avec FreePascal :

✅ **Syntaxe intuitive** - Code lisible et maintenable  
✅ **Routage puissant** - URLs flexibles avec paramètres  
✅ **JSON natif** - Support complet et facile  
✅ **Middlewares** - Architecture modulaire  
✅ **Performance** - Excellente pour applications web  
✅ **Production-ready** - Déploiement professionnel  
✅ **Multi-plateforme** - Windows, Linux, macOS

Brook Framework est le choix idéal pour développer des API REST modernes, des microservices et des backends performants avec Free

⏭️ [WebSockets et Server-Sent Events](/09-programmation-web-freepascal/04-websockets-server-sent-events.md)
