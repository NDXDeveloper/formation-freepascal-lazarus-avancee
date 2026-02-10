🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.12 GraphQL et API modernes

## Introduction

Les API modernes permettent aux applications de communiquer entre elles de manière efficace et structurée. GraphQL représente une évolution majeure par rapport aux API REST traditionnelles.

Dans ce chapitre, nous allons explorer comment créer et consommer des API GraphQL avec FreePascal/Lazarus, sur Windows et Ubuntu.

## Qu'est-ce qu'une API ?

### Analogie simple

Imaginez un restaurant :

**Vous (le client)** = Votre application  
**Le serveur** = L'API  
**La cuisine** = Le serveur/base de données  

Vous ne pouvez pas aller directement en cuisine prendre votre plat. Vous devez passer par le serveur (l'API) qui prend votre commande, va en cuisine, et vous rapporte votre plat.

L'API est donc un **intermédiaire** qui permet à votre application de demander des données ou d'effectuer des actions sur un serveur distant.

## REST vs GraphQL

### API REST traditionnelle

**Principe :** Chaque ressource a sa propre URL (endpoint).

```
GET /users/123              → Obtenir l'utilisateur 123  
GET /users/123/posts        → Obtenir les posts de l'utilisateur 123  
GET /posts/456              → Obtenir le post 456  
GET /posts/456/comments     → Obtenir les commentaires du post 456
```

**Problèmes :**

1. **Over-fetching** : On récupère trop de données
```json
GET /users/123
{
  "id": 123,
  "nom": "Dupont",
  "prenom": "Jean",
  "email": "jean@example.com",
  "adresse": "...",
  "telephone": "...",
  "date_naissance": "...",
  "preferences": {...}
}
// Alors qu'on voulait juste le nom !
```

2. **Under-fetching** : On ne récupère pas assez de données
```
// Pour afficher un utilisateur avec ses posts et commentaires :
GET /users/123           → 1 requête  
GET /users/123/posts     → 1 requête  
GET /posts/1/comments    → 1 requête  
GET /posts/2/comments    → 1 requête  
GET /posts/3/comments    → 1 requête
// Total : 5 requêtes !
```

### API GraphQL

**Principe :** Un seul endpoint, vous demandez exactement ce dont vous avez besoin.

```graphql
# Une seule requête pour tout récupérer
query {
  user(id: 123) {
    nom
    prenom
    posts {
      titre
      contenu
      comments {
        auteur
        texte
      }
    }
  }
}
```

**Réponse :**

```json
{
  "data": {
    "user": {
      "nom": "Dupont",
      "prenom": "Jean",
      "posts": [
        {
          "titre": "Mon premier post",
          "contenu": "...",
          "comments": [
            {"auteur": "Marie", "texte": "Super !"},
            {"auteur": "Pierre", "texte": "Intéressant"}
          ]
        }
      ]
    }
  }
}
```

✅ **Avantages GraphQL :**
- Un seul endpoint (`/graphql`)
- Le client demande exactement ce qu'il veut
- Pas de sur-chargement ni sous-chargement de données
- Fortement typé (schéma)
- Documentation auto-générée
- Temps réel avec subscriptions

## Structure d'une requête GraphQL

GraphQL utilise trois types d'opérations :

### 1. Query (Lecture)

Pour récupérer des données (équivalent GET en REST).

```graphql
query {
  # Récupérer un utilisateur
  user(id: 123) {
    nom
    prenom
    email
  }

  # Récupérer plusieurs utilisateurs
  users {
    id
    nom
  }

  # Avec filtres
  posts(limit: 10, orderBy: "date_desc") {
    titre
    date
  }
}
```

### 2. Mutation (Écriture)

Pour créer, modifier ou supprimer des données (équivalent POST, PUT, DELETE en REST).

```graphql
mutation {
  # Créer un utilisateur
  createUser(
    nom: "Dupont"
    prenom: "Jean"
    email: "jean@example.com"
  ) {
    id
    nom
  }

  # Mettre à jour un utilisateur
  updateUser(
    id: 123
    email: "nouveau@example.com"
  ) {
    id
    email
  }

  # Supprimer un utilisateur
  deleteUser(id: 123) {
    success
    message
  }
}
```

### 3. Subscription (Temps réel)

Pour recevoir des mises à jour en temps réel.

```graphql
subscription {
  # S'abonner aux nouveaux messages
  newMessage {
    id
    auteur
    texte
    date
  }
}
```

## Schéma GraphQL

Le schéma définit la structure de votre API (types de données, requêtes possibles).

```graphql
# Types de données
type User {
  id: ID!              # ! = obligatoire
  nom: String!
  prenom: String!
  email: String!
  age: Int
  posts: [Post!]!      # Liste de posts
}

type Post {
  id: ID!
  titre: String!
  contenu: String!
  auteur: User!
  comments: [Comment!]!
  date: String!
}

type Comment {
  id: ID!
  auteur: User!
  texte: String!
  date: String!
}

# Requêtes disponibles
type Query {
  user(id: ID!): User
  users: [User!]!
  post(id: ID!): Post
  posts(limit: Int, orderBy: String): [Post!]!
}

# Mutations disponibles
type Mutation {
  createUser(nom: String!, prenom: String!, email: String!): User!
  updateUser(id: ID!, email: String): User!
  deleteUser(id: ID!): DeleteResult!

  createPost(auteurId: ID!, titre: String!, contenu: String!): Post!
}

type DeleteResult {
  success: Boolean!
  message: String!
}
```

## Créer un serveur GraphQL avec FreePascal

Nous allons créer un serveur GraphQL simple en utilisant le framework **Brook** pour FreePascal.

### Installation de Brook

**Ubuntu :**

```bash
# Installer les dépendances
sudo apt-get install libssl-dev

# Cloner Brook
git clone https://github.com/risoflora/brookframework.git  
cd brookframework

# Compiler et installer
lazbuild Source/BrookRT.lpk
```

**Windows :**

```powershell
# Télécharger Brook depuis GitHub
# https://github.com/risoflora/brookframework

# Ouvrir BrookRT.lpk dans Lazarus et compiler
```

### Serveur GraphQL de base

```pascal
unit GraphQLServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, httpprotocol, fpjson, jsonparser;

type
  TGraphQLServer = class
  private
    FServer: TFPHTTPServer;
    FPort: Word;

    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    function ProcessGraphQLRequest(const Query: String; const Variables: TJSONObject): TJSONObject;
    function ExecuteQuery(const Query: String; const Variables: TJSONObject): TJSONObject;
    function ExecuteMutation(const Query: String; const Variables: TJSONObject): TJSONObject;
  public
    constructor Create(APort: Word);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Port: Word read FPort;
  end;

implementation

constructor TGraphQLServer.Create(APort: Word);  
begin
  FPort := APort;
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FPort;
  FServer.OnRequest := @HandleRequest;
end;

destructor TGraphQLServer.Destroy;  
begin
  FServer.Free;
  inherited;
end;

procedure TGraphQLServer.Start;  
begin
  WriteLn('🚀 Serveur GraphQL démarré sur le port ', FPort);
  WriteLn('📍 Endpoint: http://localhost:', FPort, '/graphql');
  WriteLn;
  FServer.Active := True;
end;

procedure TGraphQLServer.Stop;  
begin
  FServer.Active := False;
  WriteLn('Serveur arrêté');
end;

procedure TGraphQLServer.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  RequestBody: String;
  RequestJSON, ResponseJSON, Variables: TJSONObject;
  Query: String;
begin
  // Gérer CORS pour les requêtes depuis un navigateur
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
  AResponse.SetCustomHeader('Access-Control-Allow-Methods', 'POST, GET, OPTIONS');
  AResponse.SetCustomHeader('Access-Control-Allow-Headers', 'Content-Type');

  // Répondre aux requêtes OPTIONS (preflight)
  if ARequest.Method = 'OPTIONS' then
  begin
    AResponse.Code := 200;
    AResponse.Content := '';
    Exit;
  end;

  // Vérifier que c'est une requête GraphQL
  if ARequest.URI <> '/graphql' then
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Endpoint not found"}';
    Exit;
  end;

  try
    // Lire le corps de la requête
    RequestBody := ARequest.Content;

    if RequestBody = '' then
    begin
      AResponse.Code := 400;
      AResponse.Content := '{"error": "Empty request body"}';
      Exit;
    end;

    // Parser le JSON
    RequestJSON := TJSONObject(GetJSON(RequestBody));
    try
      Query := RequestJSON.Get('query', '');

      // Récupérer les variables si présentes
      if RequestJSON.Find('variables') <> nil then
        Variables := TJSONObject(RequestJSON.Get('variables'))
      else
        Variables := nil;

      // Traiter la requête GraphQL
      ResponseJSON := ProcessGraphQLRequest(Query, Variables);

      // Renvoyer la réponse
      AResponse.Code := 200;
      AResponse.ContentType := 'application/json';
      AResponse.Content := ResponseJSON.AsJSON;

      ResponseJSON.Free;
    finally
      RequestJSON.Free;
    end;

  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

function TGraphQLServer.ProcessGraphQLRequest(const Query: String;
  const Variables: TJSONObject): TJSONObject;
begin
  WriteLn('📥 Requête GraphQL reçue:');
  WriteLn('   ', Query);
  WriteLn;

  Result := TJSONObject.Create;

  // Déterminer le type d'opération
  if Pos('mutation', LowerCase(Query)) > 0 then
  begin
    WriteLn('🔧 Traitement d''une mutation');
    Result.Add('data', ExecuteMutation(Query, Variables));
  end
  else
  begin
    WriteLn('🔍 Traitement d''une query');
    Result.Add('data', ExecuteQuery(Query, Variables));
  end;
end;

function TGraphQLServer.ExecuteQuery(const Query: String;
  const Variables: TJSONObject): TJSONObject;
var
  DataObj, UserObj, PostsArray, PostObj: TJSONObject;
  UsersArray: TJSONArray;
  i: Integer;
begin
  Result := TJSONObject.Create;

  // Exemple simple : répondre à une requête "user"
  if Pos('user', LowerCase(Query)) > 0 then
  begin
    UserObj := TJSONObject.Create;
    UserObj.Add('id', 123);
    UserObj.Add('nom', 'Dupont');
    UserObj.Add('prenom', 'Jean');
    UserObj.Add('email', 'jean@example.com');

    // Si la requête demande aussi les posts
    if Pos('posts', LowerCase(Query)) > 0 then
    begin
      PostsArray := TJSONArray.Create;

      PostObj := TJSONObject.Create;
      PostObj.Add('id', 1);
      PostObj.Add('titre', 'Mon premier post');
      PostObj.Add('contenu', 'Contenu du post...');
      PostsArray.Add(PostObj);

      PostObj := TJSONObject.Create;
      PostObj.Add('id', 2);
      PostObj.Add('titre', 'Deuxième post');
      PostObj.Add('contenu', 'Autre contenu...');
      PostsArray.Add(PostObj);

      UserObj.Add('posts', PostsArray);
    end;

    Result.Add('user', UserObj);
  end

  // Requête "users" (liste)
  else if Pos('users', LowerCase(Query)) > 0 then
  begin
    UsersArray := TJSONArray.Create;

    for i := 1 to 3 do
    begin
      UserObj := TJSONObject.Create;
      UserObj.Add('id', i);
      UserObj.Add('nom', 'User' + IntToStr(i));
      UserObj.Add('prenom', 'Prenom' + IntToStr(i));
      UsersArray.Add(UserObj);
    end;

    Result.Add('users', UsersArray);
  end

  else
  begin
    Result.Add('error', 'Query not implemented');
  end;
end;

function TGraphQLServer.ExecuteMutation(const Query: String;
  const Variables: TJSONObject): TJSONObject;
var
  UserObj: TJSONObject;
  DeleteResult: TJSONObject;
begin
  Result := TJSONObject.Create;

  // Exemple : createUser
  if Pos('createUser', Query) > 0 then
  begin
    WriteLn('✅ Création d''un utilisateur');

    UserObj := TJSONObject.Create;
    UserObj.Add('id', Random(9999));
    UserObj.Add('nom', 'Nouveau');
    UserObj.Add('prenom', 'Utilisateur');
    UserObj.Add('email', 'nouveau@example.com');

    Result.Add('createUser', UserObj);
  end

  // Exemple : updateUser
  else if Pos('updateUser', Query) > 0 then
  begin
    WriteLn('✅ Mise à jour d''un utilisateur');

    UserObj := TJSONObject.Create;
    UserObj.Add('id', 123);
    UserObj.Add('nom', 'Dupont');
    UserObj.Add('prenom', 'Jean');
    UserObj.Add('email', 'nouveau_email@example.com');

    Result.Add('updateUser', UserObj);
  end

  // Exemple : deleteUser
  else if Pos('deleteUser', Query) > 0 then
  begin
    WriteLn('✅ Suppression d''un utilisateur');

    DeleteResult := TJSONObject.Create;
    DeleteResult.Add('success', True);
    DeleteResult.Add('message', 'User deleted successfully');

    Result.Add('deleteUser', DeleteResult);
  end

  else
  begin
    Result.Add('error', 'Mutation not implemented');
  end;
end;

end.
```

### Application serveur

```pascal
program GraphQLServerApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, GraphQLServer;

var
  Server: TGraphQLServer;

begin
  WriteLn('=== Serveur GraphQL FreePascal ===');
  WriteLn;

  Server := TGraphQLServer.Create(4000);
  try
    Server.Start;

    WriteLn('Appuyez sur Entrée pour arrêter le serveur...');
    ReadLn;

    Server.Stop;
  finally
    Server.Free;
  end;
end.
```

### Tester le serveur

**Avec curl (Ubuntu/Windows) :**

```bash
# Query simple
curl -X POST http://localhost:4000/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "{ user(id: 123) { nom prenom email } }"}'

# Query avec posts
curl -X POST http://localhost:4000/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "{ user(id: 123) { nom prenom posts { titre } } }"}'

# Mutation
curl -X POST http://localhost:4000/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "mutation { createUser(nom: \"Dupont\", prenom: \"Jean\", email: \"jean@example.com\") { id nom } }"}'
```

**Avec Postman :**

1. Créer une nouvelle requête POST
2. URL : `http://localhost:4000/graphql`
3. Headers : `Content-Type: application/json`
4. Body (raw JSON) :
```json
{
  "query": "{ user(id: 123) { nom prenom email } }"
}
```

## Client GraphQL avec FreePascal

Maintenant, créons un client pour consommer une API GraphQL.

```pascal
unit GraphQLClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser;

type
  TGraphQLClient = class
  private
    FEndpoint: String;
    FHTTPClient: TFPHTTPClient;
    FHeaders: TStringList;
  public
    constructor Create(const AEndpoint: String);
    destructor Destroy; override;

    function Query(const QueryStr: String; Variables: TJSONObject = nil): TJSONObject;
    function Mutate(const MutationStr: String; Variables: TJSONObject = nil): TJSONObject;
    procedure SetHeader(const Name, Value: String);

    property Endpoint: String read FEndpoint;
  end;

implementation

constructor TGraphQLClient.Create(const AEndpoint: String);  
begin
  FEndpoint := AEndpoint;
  FHTTPClient := TFPHTTPClient.Create(nil);
  FHeaders := TStringList.Create;

  // Headers par défaut
  FHTTPClient.AddHeader('Content-Type', 'application/json');
end;

destructor TGraphQLClient.Destroy;  
begin
  FHeaders.Free;
  FHTTPClient.Free;
  inherited;
end;

procedure TGraphQLClient.SetHeader(const Name, Value: String);  
begin
  FHTTPClient.AddHeader(Name, Value);
end;

function TGraphQLClient.Query(const QueryStr: String; Variables: TJSONObject): TJSONObject;  
var
  RequestBody: TJSONObject;
  ResponseStr: String;
  ResponseJSON: TJSONObject;
begin
  // Construire le corps de la requête
  RequestBody := TJSONObject.Create;
  try
    RequestBody.Add('query', QueryStr);

    if Variables <> nil then
      RequestBody.Add('variables', Variables);

    // Envoyer la requête
    FHTTPClient.RequestBody := TStringStream.Create(RequestBody.AsJSON);
    try
      ResponseStr := FHTTPClient.Post(FEndpoint);
    finally
      FHTTPClient.RequestBody.Free;
      FHTTPClient.RequestBody := nil;
    end;

    // Parser la réponse
    ResponseJSON := TJSONObject(GetJSON(ResponseStr));

    // Vérifier les erreurs
    if ResponseJSON.Find('errors') <> nil then
    begin
      WriteLn('❌ Erreur GraphQL:');
      WriteLn(ResponseJSON.Get('errors').AsJSON);
    end;

    // Retourner les données
    if ResponseJSON.Find('data') <> nil then
      Result := TJSONObject(ResponseJSON.Get('data').Clone)
    else
      Result := TJSONObject.Create;

    ResponseJSON.Free;
  finally
    RequestBody.Free;
  end;
end;

function TGraphQLClient.Mutate(const MutationStr: String; Variables: TJSONObject): TJSONObject;  
begin
  // Une mutation est traitée comme une query
  Result := Query(MutationStr, Variables);
end;

end.
```

### Exemples d'utilisation du client

```pascal
program GraphQLClientApp;

{$mode objfpc}{$H+}

uses
  SysUtils, GraphQLClient, fpjson;

var
  Client: TGraphQLClient;
  Response: TJSONObject;
  User: TJSONObject;
  Variables: TJSONObject;

begin
  WriteLn('=== Client GraphQL FreePascal ===');
  WriteLn;

  // Se connecter à un serveur GraphQL
  Client := TGraphQLClient.Create('http://localhost:4000/graphql');
  try
    // === 1. Query simple ===
    WriteLn('1. Récupérer un utilisateur:');
    Response := Client.Query('{ user(id: 123) { nom prenom email } }');
    try
      if Response.Find('user') <> nil then
      begin
        User := TJSONObject(Response.Get('user'));
        WriteLn('   Nom: ', User.Get('nom', ''));
        WriteLn('   Prénom: ', User.Get('prenom', ''));
        WriteLn('   Email: ', User.Get('email', ''));
      end;
    finally
      Response.Free;
    end;
    WriteLn;

    // === 2. Query avec imbrication ===
    WriteLn('2. Récupérer un utilisateur avec ses posts:');
    Response := Client.Query(
      '{ user(id: 123) { ' +
      '  nom ' +
      '  prenom ' +
      '  posts { ' +
      '    titre ' +
      '    contenu ' +
      '  } ' +
      '} }'
    );
    try
      WriteLn('   Réponse: ', Response.AsJSON);
    finally
      Response.Free;
    end;
    WriteLn;

    // === 3. Query avec variables ===
    WriteLn('3. Query avec variables:');
    Variables := TJSONObject.Create;
    try
      Variables.Add('userId', 123);

      Response := Client.Query(
        'query GetUser($userId: ID!) { ' +
        '  user(id: $userId) { ' +
        '    nom ' +
        '    prenom ' +
        '  } ' +
        '}',
        Variables
      );
      try
        WriteLn('   Réponse: ', Response.AsJSON);
      finally
        Response.Free;
      end;
    finally
      Variables.Free;
    end;
    WriteLn;

    // === 4. Mutation - Créer un utilisateur ===
    WriteLn('4. Créer un utilisateur:');
    Response := Client.Mutate(
      'mutation { ' +
      '  createUser(' +
      '    nom: "Martin"' +
      '    prenom: "Marie"' +
      '    email: "marie@example.com"' +
      '  ) { ' +
      '    id ' +
      '    nom ' +
      '    prenom ' +
      '  } ' +
      '}'
    );
    try
      WriteLn('   Réponse: ', Response.AsJSON);
    finally
      Response.Free;
    end;
    WriteLn;

    // === 5. Mutation avec variables ===
    WriteLn('5. Mettre à jour un utilisateur:');
    Variables := TJSONObject.Create;
    try
      Variables.Add('userId', 123);
      Variables.Add('email', 'nouveau_email@example.com');

      Response := Client.Mutate(
        'mutation UpdateUser($userId: ID!, $email: String!) { ' +
        '  updateUser(id: $userId, email: $email) { ' +
        '    id ' +
        '    email ' +
        '  } ' +
        '}',
        Variables
      );
      try
        WriteLn('   Réponse: ', Response.AsJSON);
      finally
        Response.Free;
      end;
    finally
      Variables.Free;
    end;

  finally
    Client.Free;
  end;

  WriteLn;
  WriteLn('=== Fin des tests ===');
end.
```

## Intégration avec une base de données

Connectons notre serveur GraphQL à une vraie base de données PostgreSQL.

```pascal
unit GraphQLResolvers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, PQConnection, fpjson;

type
  TGraphQLResolvers = class
  private
    FConnection: TSQLConnection;
  public
    constructor Create(AConnection: TSQLConnection);

    function GetUser(UserID: Integer): TJSONObject;
    function GetUsers: TJSONArray;
    function GetUserPosts(UserID: Integer): TJSONArray;
    function CreateUser(const Nom, Prenom, Email: String): TJSONObject;
    function UpdateUser(UserID: Integer; const Email: String): TJSONObject;
    function DeleteUser(UserID: Integer): TJSONObject;
  end;

implementation

constructor TGraphQLResolvers.Create(AConnection: TSQLConnection);  
begin
  FConnection := AConnection;
end;

function TGraphQLResolvers.GetUser(UserID: Integer): TJSONObject;  
var
  Query: TSQLQuery;
begin
  Result := TJSONObject.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := 'SELECT id, nom, prenom, email FROM users WHERE id = :id';
    Query.Params.ParamByName('id').AsInteger := UserID;
    Query.Open;

    if not Query.EOF then
    begin
      Result.Add('id', Query.FieldByName('id').AsInteger);
      Result.Add('nom', Query.FieldByName('nom').AsString);
      Result.Add('prenom', Query.FieldByName('prenom').AsString);
      Result.Add('email', Query.FieldByName('email').AsString);
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

function TGraphQLResolvers.GetUsers: TJSONArray;  
var
  Query: TSQLQuery;
  UserObj: TJSONObject;
begin
  Result := TJSONArray.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := 'SELECT id, nom, prenom, email FROM users ORDER BY nom';
    Query.Open;

    while not Query.EOF do
    begin
      UserObj := TJSONObject.Create;
      UserObj.Add('id', Query.FieldByName('id').AsInteger);
      UserObj.Add('nom', Query.FieldByName('nom').AsString);
      UserObj.Add('prenom', Query.FieldByName('prenom').AsString);
      UserObj.Add('email', Query.FieldByName('email').AsString);

      Result.Add(UserObj);
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

function TGraphQLResolvers.GetUserPosts(UserID: Integer): TJSONArray;  
var
  Query: TSQLQuery;
  PostObj: TJSONObject;
begin
  Result := TJSONArray.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT id, titre, contenu, date_creation ' +
      'FROM posts WHERE user_id = :user_id ORDER BY date_creation DESC';
    Query.Params.ParamByName('user_id').AsInteger := UserID;
    Query.Open;

    while not Query.EOF do
    begin
      PostObj := TJSONObject.Create;
      PostObj.Add('id', Query.FieldByName('id').AsInteger);
      PostObj.Add('titre', Query.FieldByName('titre').AsString);
      PostObj.Add('contenu', Query.FieldByName('contenu').AsString);
      PostObj.Add('date', FormatDateTime('yyyy-mm-dd',
        Query.FieldByName('date_creation').AsDateTime));

      Result.Add(PostObj);
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

function TGraphQLResolvers.CreateUser(const Nom, Prenom, Email: String): TJSONObject;  
var
  Query: TSQLQuery;
  NewID: Integer;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    FConnection.Transaction.StartTransaction;
    try
      Query.SQL.Text :=
        'INSERT INTO users (nom, prenom, email) ' +
        'VALUES (:nom, :prenom, :email) RETURNING id';
      Query.Params.ParamByName('nom').AsString := Nom;
      Query.Params.ParamByName('prenom').AsString := Prenom;
      Query.Params.ParamByName('email').AsString := Email;
      Query.Open;

      NewID := Query.FieldByName('id').AsInteger;
      Query.Close;

      FConnection.Transaction.Commit;

      // Retourner l'utilisateur créé
      Result := GetUser(NewID);
    except
      on E: Exception do
      begin
        FConnection.Transaction.Rollback;
        raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

function TGraphQLResolvers.UpdateUser(UserID: Integer; const Email: String): TJSONObject;  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    FConnection.Transaction.StartTransaction;
    try
      Query.SQL.Text :=
        'UPDATE users SET email = :email WHERE id = :id';
      Query.Params.ParamByName('id').AsInteger := UserID;
      Query.Params.ParamByName('email').AsString := Email;
      Query.ExecSQL;

      FConnection.Transaction.Commit;

      // Retourner l'utilisateur mis à jour
      Result := GetUser(UserID);
    except
      on E: Exception do
      begin
        FConnection.Transaction.Rollback;
        raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

function TGraphQLResolvers.DeleteUser(UserID: Integer): TJSONObject;  
var
  Query: TSQLQuery;
begin
  Result := TJSONObject.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    FConnection.Transaction.StartTransaction;
    try
      Query.SQL.Text := 'DELETE FROM users WHERE id = :id';
      Query.Params.ParamByName('id').AsInteger := UserID;
      Query.ExecSQL;

      FConnection.Transaction.Commit;

      Result.Add('success', True);
      Result.Add('message', 'User deleted successfully');
    except
      on E: Exception do
      begin
        FConnection.Transaction.Rollback;
        Result.Add('success', False);
        Result.Add('message', E.Message);
      end;
    end;
  finally
    Query.Free;
  end;
end;

end.
```

### Serveur GraphQL complet avec base de données

```pascal
program GraphQLServerDB;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, GraphQLServer, GraphQLResolvers, PQConnection, SQLDB;

var
  Server: TGraphQLServer;
  Connection: TPQConnection;
  Transaction: TSQLTransaction;
  Resolvers: TGraphQLResolvers;

begin
  WriteLn('=== Serveur GraphQL avec PostgreSQL ===');
  WriteLn;

  // Configurer la connexion à la base de données
  Connection := TPQConnection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.HostName := 'localhost';
    Connection.DatabaseName := 'mon_app';
    Connection.UserName := 'postgres';
    Connection.Password := 'password';

    try
      Connection.Open;
      WriteLn('✅ Connecté à PostgreSQL');

      Resolvers := TGraphQLResolvers.Create(Connection);
      try
        Server := TGraphQLServer.Create(4000);
        try
          // Passer les resolvers au serveur
          Server.Resolvers := Resolvers;
          Server.Start;

          WriteLn('Appuyez sur Entrée pour arrêter...');
          ReadLn;

          Server.Stop;
        finally
          Server.Free;
        end;
      finally
        Resolvers.Free;
      end;
    except
      on E: Exception do
        WriteLn('❌ Erreur: ', E.Message);
    end;
  finally
    Connection.Close;
    Connection.Free;
    Transaction.Free;
  end;
end.
```

## Authentification et autorisation

La sécurité est cruciale pour les API. Voici comment implémenter JWT (JSON Web Tokens).

### Générateur de tokens JWT

```pascal
unit JWTAuth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, base64, HMAC, SHA1;

type
  TJWTAuth = class
  private
    FSecretKey: String;

    function Base64URLEncode(const Data: String): String;
    function Base64URLDecode(const Data: String): String;
    function HMACSHA256(const Key, Data: String): String;
  public
    constructor Create(const ASecretKey: String);

    function GenerateToken(UserID: Integer; const Email: String;
      ExpiresInSeconds: Integer = 3600): String;
    function ValidateToken(const Token: String): TJSONObject;
    function GetUserIDFromToken(const Token: String): Integer;
  end;

implementation

constructor TJWTAuth.Create(const ASecretKey: String);  
begin
  FSecretKey := ASecretKey;
end;

function TJWTAuth.Base64URLEncode(const Data: String): String;  
begin
  Result := EncodeStringBase64(Data);
  // Rendre compatible URL
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

function TJWTAuth.Base64URLDecode(const Data: String): String;  
var
  Temp: String;
begin
  Temp := StringReplace(Data, '-', '+', [rfReplaceAll]);
  Temp := StringReplace(Temp, '_', '/', [rfReplaceAll]);

  // Ajouter le padding si nécessaire
  case Length(Temp) mod 4 of
    2: Temp := Temp + '==';
    3: Temp := Temp + '=';
  end;

  Result := DecodeStringBase64(Temp);
end;

function TJWTAuth.HMACSHA256(const Key, Data: String): String;  
var
  HMAC: THMAC;
begin
  // Implémentation simplifiée
  // En production, utiliser une vraie librairie HMAC-SHA256
  HMAC := THMAC.Create;
  try
    Result := HMAC.ComputeHMAC(Key, Data);
  finally
    HMAC.Free;
  end;
end;

function TJWTAuth.GenerateToken(UserID: Integer; const Email: String;
  ExpiresInSeconds: Integer): String;
var
  Header, Payload, Signature: String;
  HeaderJSON, PayloadJSON: TJSONObject;
  ExpirationTime: Int64;
begin
  // Header
  HeaderJSON := TJSONObject.Create;
  try
    HeaderJSON.Add('alg', 'HS256');
    HeaderJSON.Add('typ', 'JWT');
    Header := Base64URLEncode(HeaderJSON.AsJSON);
  finally
    HeaderJSON.Free;
  end;

  // Payload
  ExpirationTime := DateTimeToUnix(Now) + ExpiresInSeconds;
  PayloadJSON := TJSONObject.Create;
  try
    PayloadJSON.Add('user_id', UserID);
    PayloadJSON.Add('email', Email);
    PayloadJSON.Add('exp', ExpirationTime);
    PayloadJSON.Add('iat', DateTimeToUnix(Now));
    Payload := Base64URLEncode(PayloadJSON.AsJSON);
  finally
    PayloadJSON.Free;
  end;

  // Signature
  Signature := Base64URLEncode(HMACSHA256(FSecretKey, Header + '.' + Payload));

  // Token complet
  Result := Header + '.' + Payload + '.' + Signature;
end;

function TJWTAuth.ValidateToken(const Token: String): TJSONObject;  
var
  Parts: TStringArray;
  Header, Payload, Signature: String;
  ExpectedSignature: String;
  PayloadJSON: TJSONObject;
  ExpirationTime: Int64;
begin
  Result := nil;

  // Séparer les parties du token
  Parts := Token.Split('.');
  if Length(Parts) <> 3 then
    Exit;

  Header := Parts[0];
  Payload := Parts[1];
  Signature := Parts[2];

  // Vérifier la signature
  ExpectedSignature := Base64URLEncode(HMACSHA256(FSecretKey, Header + '.' + Payload));
  if Signature <> ExpectedSignature then
  begin
    WriteLn('❌ Signature invalide');
    Exit;
  end;

  // Décoder le payload
  PayloadJSON := TJSONObject(GetJSON(Base64URLDecode(Payload)));

  // Vérifier l'expiration
  ExpirationTime := PayloadJSON.Get('exp', 0);
  if DateTimeToUnix(Now) > ExpirationTime then
  begin
    WriteLn('❌ Token expiré');
    PayloadJSON.Free;
    Exit;
  end;

  Result := PayloadJSON;
end;

function TJWTAuth.GetUserIDFromToken(const Token: String): Integer;  
var
  PayloadJSON: TJSONObject;
begin
  Result := -1;
  PayloadJSON := ValidateToken(Token);
  if PayloadJSON <> nil then
  begin
    try
      Result := PayloadJSON.Get('user_id', -1);
    finally
      PayloadJSON.Free;
    end;
  end;
end;

end.
```

### Middleware d'authentification

```pascal
unit GraphQLAuth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, JWTAuth;

type
  TAuthMiddleware = class
  private
    FJWTAuth: TJWTAuth;
  public
    constructor Create(const SecretKey: String);
    destructor Destroy; override;

    function AuthenticateRequest(Request: TFPHTTPConnectionRequest): Integer;
    function IsAuthenticated(Request: TFPHTTPConnectionRequest): Boolean;
  end;

implementation

constructor TAuthMiddleware.Create(const SecretKey: String);  
begin
  FJWTAuth := TJWTAuth.Create(SecretKey);
end;

destructor TAuthMiddleware.Destroy;  
begin
  FJWTAuth.Free;
  inherited;
end;

function TAuthMiddleware.AuthenticateRequest(Request: TFPHTTPConnectionRequest): Integer;  
var
  AuthHeader: String;
  Token: String;
begin
  Result := -1;

  // Récupérer le header Authorization
  AuthHeader := Request.GetCustomHeader('Authorization');

  if AuthHeader = '' then
  begin
    WriteLn('⚠️  Aucun header Authorization');
    Exit;
  end;

  // Format: "Bearer TOKEN"
  if Pos('Bearer ', AuthHeader) <> 1 then
  begin
    WriteLn('⚠️  Format Authorization invalide');
    Exit;
  end;

  Token := Copy(AuthHeader, 8, Length(AuthHeader) - 7);

  // Valider le token et récupérer l'ID utilisateur
  Result := FJWTAuth.GetUserIDFromToken(Token);

  if Result > 0 then
    WriteLn('✅ Utilisateur authentifié: ', Result)
  else
    WriteLn('❌ Token invalide');
end;

function TAuthMiddleware.IsAuthenticated(Request: TFPHTTPConnectionRequest): Boolean;  
begin
  Result := AuthenticateRequest(Request) > 0;
end;

end.
```

### Requêtes protégées

```pascal
// Dans GraphQLServer, ajouter la vérification d'authentification

procedure TGraphQLServer.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  UserID: Integer;
  RequiresAuth: Boolean;
begin
  // ... (code existant pour CORS) ...

  // Vérifier si la requête nécessite une authentification
  RequiresAuth := True;  // Par défaut, toutes les requêtes sont protégées

  // Certaines requêtes publiques
  if (Pos('login', ARequest.Content) > 0) or
     (Pos('register', ARequest.Content) > 0) then
    RequiresAuth := False;

  if RequiresAuth then
  begin
    UserID := FAuthMiddleware.AuthenticateRequest(ARequest);

    if UserID <= 0 then
    begin
      AResponse.Code := 401;
      AResponse.Content := '{"error": "Unauthorized - Invalid or missing token"}';
      Exit;
    end;

    // Stocker l'ID utilisateur pour les resolvers
    FCurrentUserID := UserID;
  end;

  // ... (reste du code) ...
end;
```

### Mutation de connexion

```pascal
// Ajouter au resolver

function TGraphQLResolvers.Login(const Email, Password: String): TJSONObject;  
var
  Query: TSQLQuery;
  UserID: Integer;
  Token: String;
  JWTAuth: TJWTAuth;
begin
  Result := TJSONObject.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    // Vérifier les identifiants
    Query.SQL.Text :=
      'SELECT id, nom, prenom FROM users ' +
      'WHERE email = :email AND password = crypt(:password, password)';
    Query.Params.ParamByName('email').AsString := Email;
    Query.Params.ParamByName('password').AsString := Password;
    Query.Open;

    if Query.EOF then
    begin
      Result.Add('success', False);
      Result.Add('message', 'Email ou mot de passe incorrect');
      Query.Close;
      Exit;
    end;

    UserID := Query.FieldByName('id').AsInteger;
    Query.Close;

    // Générer un token JWT
    JWTAuth := TJWTAuth.Create('votre_secret_key_securise');
    try
      Token := JWTAuth.GenerateToken(UserID, Email, 86400);  // 24 heures
    finally
      JWTAuth.Free;
    end;

    Result.Add('success', True);
    Result.Add('token', Token);
    Result.Add('user_id', UserID);
  finally
    Query.Free;
  end;
end;
```

## Pagination et filtrage

Pour gérer efficacement les grandes listes de données.

### Pagination Cursor-based

```pascal
type
  TPaginationInfo = record
    HasNextPage: Boolean;
    HasPreviousPage: Boolean;
    StartCursor: String;
    EndCursor: String;
  end;

function TGraphQLResolvers.GetUsersWithPagination(
  First: Integer = 10;
  After: String = ''): TJSONObject;
var
  Query: TSQLQuery;
  Users: TJSONArray;
  UserObj: TJSONObject;
  PageInfo: TJSONObject;
  LastID: Integer;
  Count: Integer;
begin
  Result := TJSONObject.Create;
  Users := TJSONArray.Create;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    // Construction de la requête avec pagination
    if After = '' then
      Query.SQL.Text :=
        'SELECT id, nom, prenom, email FROM users ' +
        'ORDER BY id LIMIT :limit'
    else
      Query.SQL.Text :=
        'SELECT id, nom, prenom, email FROM users ' +
        'WHERE id > :after_id ' +
        'ORDER BY id LIMIT :limit';

    Query.Params.ParamByName('limit').AsInteger := First + 1;  // +1 pour savoir s'il y a une page suivante

    if After <> '' then
      Query.Params.ParamByName('after_id').AsInteger := StrToIntDef(After, 0);

    Query.Open;

    LastID := 0;
    Count := 0;

    while not Query.EOF and (Count < First) do
    begin
      UserObj := TJSONObject.Create;
      UserObj.Add('id', Query.FieldByName('id').AsInteger);
      UserObj.Add('nom', Query.FieldByName('nom').AsString);
      UserObj.Add('prenom', Query.FieldByName('prenom').AsString);
      UserObj.Add('email', Query.FieldByName('email').AsString);

      Users.Add(UserObj);
      LastID := Query.FieldByName('id').AsInteger;

      Inc(Count);
      Query.Next;
    end;

    // Informations de pagination
    PageInfo := TJSONObject.Create;
    PageInfo.Add('hasNextPage', not Query.EOF);
    PageInfo.Add('hasPreviousPage', After <> '');
    PageInfo.Add('endCursor', IntToStr(LastID));

    Query.Close;

    Result.Add('edges', Users);
    Result.Add('pageInfo', PageInfo);
  finally
    Query.Free;
  end;
end;
```

### Filtrage et tri

```pascal
function TGraphQLResolvers.SearchUsers(
  const SearchTerm: String;
  OrderBy: String = 'nom';
  OrderDirection: String = 'ASC'): TJSONArray;
var
  Query: TSQLQuery;
  UserObj: TJSONObject;
  SQL: String;
begin
  Result := TJSONArray.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    // Construction dynamique de la requête
    SQL := 'SELECT id, nom, prenom, email FROM users WHERE 1=1 ';

    if SearchTerm <> '' then
      SQL := SQL + 'AND (nom ILIKE :search OR prenom ILIKE :search OR email ILIKE :search) ';

    // Validation du tri (sécurité)
    if not (OrderBy in ['nom', 'prenom', 'email', 'id']) then
      OrderBy := 'nom';

    if not (UpperCase(OrderDirection) in ['ASC', 'DESC']) then
      OrderDirection := 'ASC';

    SQL := SQL + Format('ORDER BY %s %s LIMIT 100', [OrderBy, OrderDirection]);

    Query.SQL.Text := SQL;

    if SearchTerm <> '' then
      Query.Params.ParamByName('search').AsString := '%' + SearchTerm + '%';

    Query.Open;

    while not Query.EOF do
    begin
      UserObj := TJSONObject.Create;
      UserObj.Add('id', Query.FieldByName('id').AsInteger);
      UserObj.Add('nom', Query.FieldByName('nom').AsString);
      UserObj.Add('prenom', Query.FieldByName('prenom').AsString);
      UserObj.Add('email', Query.FieldByName('email').AsString);

      Result.Add(UserObj);
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;
```

### Schéma GraphQL avec pagination

```graphql
type Query {
  # Pagination simple
  users(limit: Int, offset: Int): [User!]!

  # Pagination cursor-based (recommandé)
  usersConnection(
    first: Int
    after: String
    last: Int
    before: String
  ): UserConnection!

  # Recherche avec filtres
  searchUsers(
    search: String
    orderBy: String
    orderDirection: String
  ): [User!]!
}

type UserConnection {
  edges: [UserEdge!]!
  pageInfo: PageInfo!
}

type UserEdge {
  node: User!
  cursor: String!
}

type PageInfo {
  hasNextPage: Boolean!
  hasPreviousPage: Boolean!
  startCursor: String
  endCursor: String
}
```

## Subscriptions (temps réel)

Les subscriptions permettent de recevoir des mises à jour en temps réel via WebSocket.

### Serveur avec WebSocket

```pascal
unit GraphQLSubscriptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, ssockets;

type
  TSubscriptionClient = class
    Socket: TSocketStream;
    UserID: Integer;
    Subscriptions: TStringList;
  end;

  TClientList = specialize TFPGObjectList<TSubscriptionClient>;

  TSubscriptionManager = class
  private
    FClients: TClientList;
    FServer: TInetServer;

    procedure HandleNewConnection(Sender: TObject; Data: TSocketStream);
    procedure BroadcastToClients(const Event: String; Data: TJSONObject);
  public
    constructor Create(APort: Word);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    // Publier des événements
    procedure PublishNewUser(UserData: TJSONObject);
    procedure PublishUserUpdate(UserData: TJSONObject);
    procedure PublishNewPost(PostData: TJSONObject);
  end;

implementation

constructor TSubscriptionManager.Create(APort: Word);  
begin
  FClients := TClientList.Create(True);
  FServer := TInetServer.Create('0.0.0.0', APort);
  FServer.OnConnect := @HandleNewConnection;
end;

destructor TSubscriptionManager.Destroy;  
begin
  Stop;
  FClients.Free;
  FServer.Free;
  inherited;
end;

procedure TSubscriptionManager.Start;  
begin
  FServer.Bind;
  FServer.Listen;
  WriteLn('🔌 Serveur WebSocket démarré');
end;

procedure TSubscriptionManager.Stop;  
begin
  FServer.Close;
  WriteLn('🔌 Serveur WebSocket arrêté');
end;

procedure TSubscriptionManager.HandleNewConnection(Sender: TObject; Data: TSocketStream);  
var
  Client: TSubscriptionClient;
begin
  Client := TSubscriptionClient.Create;
  Client.Socket := Data;
  Client.Subscriptions := TStringList.Create;

  FClients.Add(Client);

  WriteLn('✅ Nouveau client WebSocket connecté');
end;

procedure TSubscriptionManager.BroadcastToClients(const Event: String; Data: TJSONObject);  
var
  Client: TSubscriptionClient;
  Message: TJSONObject;
  MessageStr: String;
begin
  Message := TJSONObject.Create;
  try
    Message.Add('event', Event);
    Message.Add('data', Data.Clone);
    MessageStr := Message.AsJSON;

    for Client in FClients do
    begin
      try
        Client.Socket.WriteBuffer(MessageStr[1], Length(MessageStr));
      except
        on E: Exception do
          WriteLn('❌ Erreur envoi au client: ', E.Message);
      end;
    end;
  finally
    Message.Free;
  end;
end;

procedure TSubscriptionManager.PublishNewUser(UserData: TJSONObject);  
begin
  WriteLn('📢 Publication: Nouvel utilisateur');
  BroadcastToClients('newUser', UserData);
end;

procedure TSubscriptionManager.PublishUserUpdate(UserData: TJSONObject);  
begin
  WriteLn('📢 Publication: Mise à jour utilisateur');
  BroadcastToClients('userUpdate', UserData);
end;

procedure TSubscriptionManager.PublishNewPost(PostData: TJSONObject);  
begin
  WriteLn('📢 Publication: Nouveau post');
  BroadcastToClients('newPost', PostData);
end;

end.
```

### Client avec WebSocket

```pascal
program SubscriptionClient;

{$mode objfpc}{$H+}

uses
  SysUtils, ssockets, fpjson, jsonparser;

var
  Client: TInetSocket;
  Buffer: String;
  MessageJSON: TJSONObject;

begin
  WriteLn('=== Client GraphQL Subscriptions ===');
  WriteLn;

  Client := TInetSocket.Create('localhost', 4001);
  try
    Client.Connect;
    WriteLn('✅ Connecté au serveur WebSocket');
    WriteLn('En attente de messages...');
    WriteLn;

    // Boucle de réception
    while True do
    begin
      SetLength(Buffer, 1024);
      Client.Read(Buffer[1], 1024);

      if Buffer <> '' then
      begin
        try
          MessageJSON := TJSONObject(GetJSON(Buffer));
          try
            WriteLn('📨 Message reçu:');
            WriteLn('   Événement: ', MessageJSON.Get('event', ''));
            WriteLn('   Données: ', MessageJSON.Get('data').AsJSON);
            WriteLn;
          finally
            MessageJSON.Free;
          end;
        except
          on E: Exception do
            WriteLn('❌ Erreur parsing: ', E.Message);
        end;
      end;

      Sleep(100);
    end;
  finally
    Client.Free;
  end;
end.
```

## API REST vs GraphQL - Tableau comparatif complet

| Aspect | REST | GraphQL |
|--------|------|---------|
| **Endpoints** | Multiples (`/users`, `/posts`) | Un seul (`/graphql`) |
| **Récupération de données** | Endpoints fixes | Flexible, à la demande |
| **Over-fetching** | Fréquent | Aucun |
| **Under-fetching** | Requêtes multiples nécessaires | Une seule requête suffit |
| **Versioning** | `/v1/`, `/v2/` | Pas nécessaire (évolutif) |
| **Caching** | HTTP caching natif | Plus complexe |
| **Typage** | Optionnel (Swagger/OpenAPI) | Intégré (schéma) |
| **Documentation** | Manuelle (Swagger) | Auto-générée |
| **Courbe d'apprentissage** | Simple | Moyenne |
| **Complexité serveur** | Simple | Plus complexe |
| **Temps réel** | WebSocket séparés | Subscriptions intégrées |
| **Performance** | Bonne (avec cache) | Excellente (requêtes optimisées) |

## Outils de développement GraphQL

### GraphiQL - Interface de test

GraphiQL est un IDE dans le navigateur pour tester vos API GraphQL.

**Installation :**

```html
<!DOCTYPE html>
<html>
<head>
  <title>GraphiQL</title>
  <link href="https://unpkg.com/graphiql/graphiql.min.css" rel="stylesheet" />
</head>
<body style="margin: 0;">
  <div id="graphiql" style="height: 100vh;"></div>

  <script
    crossorigin
    src="https://unpkg.com/react/umd/react.production.min.js"
  ></script>
  <script
    crossorigin
    src="https://unpkg.com/react-dom/umd/react-dom.production.min.js"
  ></script>
  <script
    crossorigin
    src="https://unpkg.com/graphiql/graphiql.min.js"
  ></script>

  <script>
    const fetcher = GraphiQL.createFetcher({
      url: 'http://localhost:4000/graphql',
    });

    ReactDOM.render(
      React.createElement(GraphiQL, { fetcher: fetcher }),
      document.getElementById('graphiql'),
    );
  </script>
</body>
</html>
```

### Postman

Postman supporte GraphQL nativement :

1. Créer une nouvelle requête POST
2. Sélectionner "GraphQL" dans le body
3. Écrire votre query
4. Ajouter des variables si nécessaire

### Apollo Studio

Apollo Studio offre des outils professionnels pour GraphQL :
- Explorateur de schéma
- Métriques de performance
- Analyse des requêtes
- Tests automatisés

## Configuration multi-OS

### Ubuntu

```bash
# Compiler le serveur
fpc GraphQLServerApp.pas -Fu/path/to/units

# Exécuter
./GraphQLServerApp

# En arrière-plan avec systemd
sudo nano /etc/systemd/system/graphql.service
```

**Fichier systemd :**

```ini
[Unit]
Description=GraphQL API Server  
After=network.target postgresql.service

[Service]
Type=simple  
User=www-data  
WorkingDirectory=/opt/graphql  
ExecStart=/opt/graphql/GraphQLServerApp  
Restart=always

[Install]
WantedBy=multi-user.target
```

### Windows

```powershell
# Compiler
fpc GraphQLServerApp.pas

# Exécuter
.\GraphQLServerApp.exe

# Installer comme service Windows
sc create GraphQLService binPath= "C:\Path\To\GraphQLServerApp.exe"  
sc start GraphQLService
```

## Bonnes pratiques GraphQL

### 1. Nommage cohérent

✅ **Bon :**
```graphql
type User {
  id: ID!
  firstName: String!    # camelCase
  lastName: String!
  emailAddress: String!
}
```

❌ **Mauvais :**
```graphql
type User {
  id: ID!
  first_name: String!   # snake_case mélangé avec camelCase
  LastName: String!     # PascalCase
  email: String!
}
```

### 2. Utiliser des types nullables avec précaution

✅ **Bon :**
```graphql
type User {
  id: ID!              # Toujours présent
  name: String!        # Toujours présent
  bio: String          # Optionnel (peut être null)
  posts: [Post!]!      # Liste jamais null, éléments jamais null
}
```

❌ **Mauvais :**
```graphql
type User {
  id: ID               # ID peut être null ? Non !
  name: String         # Nom peut être null ? Peu probable
  posts: [Post]        # Liste ET éléments peuvent être null (ambiguité)
}
```

### 3. Pagination pour les listes

✅ **Bon :**
```graphql
type Query {
  users(first: Int, after: String): UserConnection!
}

type UserConnection {
  edges: [UserEdge!]!
  pageInfo: PageInfo!
  totalCount: Int!
}
```

❌ **Mauvais :**
```graphql
type Query {
  users: [User!]!  # Peut retourner des millions de résultats !
}
```

### 4. Descriptions de schéma

✅ **Bon :**
```graphql
"""
Représente un utilisateur de l'application
"""
type User {
  "Identifiant unique de l'utilisateur"
  id: ID!

  "Nom complet de l'utilisateur"
  fullName: String!

  """
  Adresse email de l'utilisateur
  Doit être unique dans le système
  """
  email: String!
}
```

### 5. Gestion des erreurs

✅ **Bon :**
```graphql
type Mutation {
  createUser(input: CreateUserInput!): CreateUserPayload!
}

type CreateUserPayload {
  user: User
  userErrors: [UserError!]!
}

type UserError {
  field: String!
  message: String!
}
```

**Utilisation :**
```pascal
function CreateUser(Input: TCreateUserInput): TJSONObject;  
var
  Errors, Error: TJSONObject;
  ErrorsArray: TJSONArray;
  User: TJSONObject;
begin
  Result := TJSONObject.Create;
  ErrorsArray := TJSONArray.Create;

  // Validation
  if Input.Email = '' then
  begin
    Error := TJSONObject.Create;
    Error.Add('field', 'email');
    Error.Add('message', 'Email requis');
    ErrorsArray.Add(Error);
  end;

  if ErrorsArray.Count > 0 then
  begin
    Result.Add('user', TJSONNull.Create);
    Result.Add('userErrors', ErrorsArray);
  end
  else
  begin
    // Créer l'utilisateur
    User := CreateUserInDatabase(Input);
    Result.Add('user', User);
    Result.Add('userErrors', TJSONArray.Create);
  end;
end;
```

### 6. Éviter les requêtes N+1

❌ **Problème N+1 :**
```pascal
// Mauvais : Une requête par utilisateur pour récupérer ses posts
function GetUsers: TJSONArray;  
var
  Query: TSQLQuery;
  User: TJSONObject;
begin
  Query.SQL.Text := 'SELECT * FROM users';
  Query.Open;

  while not Query.EOF do
  begin
    User := CreateUserJSON(Query);

    // Requête supplémentaire pour CHAQUE utilisateur !
    User.Add('posts', GetUserPosts(Query.FieldByName('id').AsInteger));

    Result.Add(User);
    Query.Next;
  end;
end;
```

✅ **Solution avec DataLoader / Batching :**
```pascal
// Bon : Une seule requête pour tous les posts
function GetUsersWithPosts: TJSONArray;  
var
  UsersQuery, PostsQuery: TSQLQuery;
  PostsByUser: TDictionary;
  UserID: Integer;
  UserIDs: String;
  User: TJSONObject;
begin
  // 1. Récupérer tous les utilisateurs
  UsersQuery.SQL.Text := 'SELECT * FROM users';
  UsersQuery.Open;

  UserIDs := CollectUserIDs(UsersQuery);

  // 2. Récupérer tous les posts en une seule requête
  PostsQuery.SQL.Text :=
    'SELECT * FROM posts WHERE user_id = ANY(:user_ids)';
  PostsQuery.Params.ParamByName('user_ids').Value := UserIDs;
  PostsQuery.Open;

  // 3. Organiser les posts par utilisateur
  PostsByUser := GroupPostsByUser(PostsQuery);

  // 4. Assembler le résultat
  UsersQuery.First;
  while not UsersQuery.EOF do
  begin
    User := CreateUserJSON(UsersQuery);
    UserID := UsersQuery.FieldByName('id').AsInteger;

    if PostsByUser.ContainsKey(UserID) then
      User.Add('posts', PostsByUser[UserID])
    else
      User.Add('posts', TJSONArray.Create);

    Result.Add(User);
    UsersQuery.Next;
  end;
end;
```

### 7. Limitation de profondeur des requêtes

Pour éviter les requêtes abusives :

```pascal
unit QueryDepthLimiter;

{$mode objfpc}{$H+}

interface

type
  TQueryDepthLimiter = class
  private
    FMaxDepth: Integer;
    function CalculateDepth(const Query: String): Integer;
  public
    constructor Create(AMaxDepth: Integer = 10);
    function ValidateQuery(const Query: String): Boolean;
  end;

implementation

constructor TQueryDepthLimiter.Create(AMaxDepth: Integer);  
begin
  FMaxDepth := AMaxDepth;
end;

function TQueryDepthLimiter.CalculateDepth(const Query: String): Integer;  
var
  CurrentDepth, MaxDepth: Integer;
  i: Integer;
begin
  CurrentDepth := 0;
  MaxDepth := 0;

  // Compter les accolades ouvrantes et fermantes
  for i := 1 to Length(Query) do
  begin
    if Query[i] = '{' then
    begin
      Inc(CurrentDepth);
      if CurrentDepth > MaxDepth then
        MaxDepth := CurrentDepth;
    end
    else if Query[i] = '}' then
      Dec(CurrentDepth);
  end;

  Result := MaxDepth;
end;

function TQueryDepthLimiter.ValidateQuery(const Query: String): Boolean;  
var
  Depth: Integer;
begin
  Depth := CalculateDepth(Query);
  Result := Depth <= FMaxDepth;

  if not Result then
    WriteLn(Format('⚠️  Requête trop profonde: %d (max: %d)', [Depth, FMaxDepth]));
end;

end.
```

### 8. Rate Limiting par complexité

```pascal
unit QueryComplexityLimiter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TQueryComplexityLimiter = class
  private
    FMaxComplexity: Integer;
    function CalculateComplexity(const Query: String): Integer;
  public
    constructor Create(AMaxComplexity: Integer = 1000);
    function ValidateQuery(const Query: String): Boolean;
  end;

implementation

constructor TQueryComplexityLimiter.Create(AMaxComplexity: Integer);  
begin
  FMaxComplexity := AMaxComplexity;
end;

function TQueryComplexityLimiter.CalculateComplexity(const Query: String): Integer;  
var
  Complexity: Integer;
begin
  Complexity := 0;

  // Chaque champ = 1 point
  Complexity := Complexity + CountOccurrences(Query, '{');

  // Chaque liste = 10 points (potentiellement beaucoup de données)
  if Pos('users', Query) > 0 then
    Complexity := Complexity + 10;
  if Pos('posts', Query) > 0 then
    Complexity := Complexity + 10;

  // Relations imbriquées = multiplier
  if (Pos('users', Query) > 0) and (Pos('posts', Query) > 0) then
    Complexity := Complexity * 2;

  Result := Complexity;
end;

function TQueryComplexityLimiter.ValidateQuery(const Query: String): Boolean;  
var
  Complexity: Integer;
begin
  Complexity := CalculateComplexity(Query);
  Result := Complexity <= FMaxComplexity;

  if not Result then
    WriteLn(Format('⚠️  Requête trop complexe: %d (max: %d)',
      [Complexity, FMaxComplexity]));
end;

end.
```

## Monitoring et analytics

### Logger les requêtes

```pascal
unit GraphQLLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TQueryLog = record
    Timestamp: TDateTime;
    Query: String;
    Variables: String;
    Duration: Integer;  // en ms
    UserID: Integer;
    Success: Boolean;
    ErrorMessage: String;
  end;

  TGraphQLLogger = class
  private
    FLogFile: String;
    procedure WriteLog(const Log: TQueryLog);
  public
    constructor Create(const ALogFile: String);

    procedure LogQuery(const Query, Variables: String;
      Duration: Integer; UserID: Integer; Success: Boolean;
      const ErrorMessage: String = '');
    procedure LogSlowQuery(const Query: String; Duration: Integer);
    procedure GenerateReport;
  end;

implementation

constructor TGraphQLLogger.Create(const ALogFile: String);  
begin
  FLogFile := ALogFile;
end;

procedure TGraphQLLogger.WriteLog(const Log: TQueryLog);  
var
  F: TextFile;
begin
  AssignFile(F, FLogFile);
  try
    if FileExists(FLogFile) then
      Append(F)
    else
      Rewrite(F);

    WriteLn(F, Format('[%s] UserID:%d Duration:%dms Success:%s', [
      FormatDateTime('yyyy-mm-dd hh:nn:ss', Log.Timestamp),
      Log.UserID,
      Log.Duration,
      BoolToStr(Log.Success, True)
    ]));
    WriteLn(F, 'Query: ', Log.Query);

    if Log.Variables <> '' then
      WriteLn(F, 'Variables: ', Log.Variables);

    if not Log.Success then
      WriteLn(F, 'Error: ', Log.ErrorMessage);

    WriteLn(F, '---');

    CloseFile(F);
  except
    on E: Exception do
      WriteLn('Erreur log: ', E.Message);
  end;
end;

procedure TGraphQLLogger.LogQuery(const Query, Variables: String;
  Duration: Integer; UserID: Integer; Success: Boolean;
  const ErrorMessage: String);
var
  Log: TQueryLog;
begin
  Log.Timestamp := Now;
  Log.Query := Query;
  Log.Variables := Variables;
  Log.Duration := Duration;
  Log.UserID := UserID;
  Log.Success := Success;
  Log.ErrorMessage := ErrorMessage;

  WriteLog(Log);

  // Alerter si la requête est lente
  if Duration > 1000 then  // Plus d'1 seconde
    LogSlowQuery(Query, Duration);
end;

procedure TGraphQLLogger.LogSlowQuery(const Query: String; Duration: Integer);  
begin
  WriteLn('⚠️  REQUÊTE LENTE détectée:');
  WriteLn('   Durée: ', Duration, 'ms');
  WriteLn('   Query: ', Copy(Query, 1, 100), '...');
end;

procedure TGraphQLLogger.GenerateReport;  
var
  F: TextFile;
  Line: String;
  TotalQueries, SuccessCount, ErrorCount: Integer;
  TotalDuration, MaxDuration: Integer;
  DurationStr: String;
  Duration: Integer;
begin
  AssignFile(F, FLogFile);
  try
    Reset(F);

    TotalQueries := 0;
    SuccessCount := 0;
    ErrorCount := 0;
    TotalDuration := 0;
    MaxDuration := 0;

    while not EOF(F) do
    begin
      ReadLn(F, Line);

      if Pos('Duration:', Line) > 0 then
      begin
        Inc(TotalQueries);

        // Parser la durée
        DurationStr := Copy(Line, Pos('Duration:', Line) + 9, 10);
        Duration := StrToIntDef(Copy(DurationStr, 1, Pos('ms', DurationStr) - 1), 0);

        TotalDuration := TotalDuration + Duration;
        if Duration > MaxDuration then
          MaxDuration := Duration;

        if Pos('Success:True', Line) > 0 then
          Inc(SuccessCount)
        else
          Inc(ErrorCount);
      end;
    end;

    CloseFile(F);

    WriteLn('=== Rapport GraphQL ===');
    WriteLn('Requêtes totales: ', TotalQueries);
    WriteLn('Succès: ', SuccessCount, ' (',
      FormatFloat('0.00', (SuccessCount / TotalQueries) * 100), '%)');
    WriteLn('Erreurs: ', ErrorCount, ' (',
      FormatFloat('0.00', (ErrorCount / TotalQueries) * 100), '%)');
    WriteLn('Durée moyenne: ', TotalDuration div TotalQueries, 'ms');
    WriteLn('Durée maximum: ', MaxDuration, 'ms');
  except
    on E: Exception do
      WriteLn('Erreur rapport: ', E.Message);
  end;
end;

end.
```

## Tests automatisés

### Tests unitaires pour resolvers

```pascal
program TestGraphQLResolvers;

{$mode objfpc}{$H+}

uses
  SysUtils, GraphQLResolvers, PQConnection, SQLDB;

var
  Connection: TPQConnection;
  Resolvers: TGraphQLResolvers;
  TestsPassed, TestsFailed: Integer;

procedure AssertEquals(const Expected, Actual: String; const TestName: String);  
begin
  if Expected = Actual then
  begin
    WriteLn('✅ ', TestName, ' : PASS');
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('❌ ', TestName, ' : FAIL');
    WriteLn('   Attendu: ', Expected);
    WriteLn('   Obtenu: ', Actual);
    Inc(TestsFailed);
  end;
end;

procedure TestGetUser;  
var
  User: TJSONObject;
begin
  WriteLn('Test: GetUser');
  User := Resolvers.GetUser(1);
  try
    AssertEquals('Dupont', User.Get('nom', ''), 'GetUser - Nom');
    AssertEquals('jean@example.com', User.Get('email', ''), 'GetUser - Email');
  finally
    User.Free;
  end;
end;

procedure TestCreateUser;  
var
  User: TJSONObject;
  UserID: Integer;
begin
  WriteLn('Test: CreateUser');
  User := Resolvers.CreateUser('Test', 'Utilisateur', 'test@example.com');
  try
    UserID := User.Get('id', 0);
    if UserID > 0 then
    begin
      WriteLn('✅ CreateUser : PASS');
      Inc(TestsPassed);

      // Nettoyer
      Resolvers.DeleteUser(UserID);
    end
    else
    begin
      WriteLn('❌ CreateUser : FAIL');
      Inc(TestsFailed);
    end;
  finally
    User.Free;
  end;
end;

procedure TestGetUsers;  
var
  Users: TJSONArray;
begin
  WriteLn('Test: GetUsers');
  Users := Resolvers.GetUsers;
  try
    if Users.Count > 0 then
    begin
      WriteLn('✅ GetUsers : PASS (', Users.Count, ' utilisateurs)');
      Inc(TestsPassed);
    end
    else
    begin
      WriteLn('❌ GetUsers : FAIL (aucun utilisateur)');
      Inc(TestsFailed);
    end;
  finally
    Users.Free;
  end;
end;

begin
  WriteLn('=== Tests GraphQL Resolvers ===');
  WriteLn;

  TestsPassed := 0;
  TestsFailed := 0;

  // Configurer la connexion de test
  Connection := TPQConnection.Create(nil);
  try
    Connection.HostName := 'localhost';
    Connection.DatabaseName := 'test_db';
    Connection.UserName := 'postgres';
    Connection.Password := 'password';
    Connection.Open;

    Resolvers := TGraphQLResolvers.Create(Connection);
    try
      // Exécuter les tests
      TestGetUser;
      TestGetUsers;
      TestCreateUser;

      WriteLn;
      WriteLn('=== Résultats ===');
      WriteLn('Tests réussis: ', TestsPassed);
      WriteLn('Tests échoués: ', TestsFailed);

      if TestsFailed = 0 then
        WriteLn('✅ Tous les tests sont passés!')
      else
        WriteLn('❌ Certains tests ont échoué');
    finally
      Resolvers.Free;
    end;
  finally
    Connection.Close;
    Connection.Free;
  end;
end.
```

## Déploiement en production

### Configuration Nginx (Reverse Proxy)

**Ubuntu :**

```nginx
# /etc/nginx/sites-available/graphql

server {
    listen 80;
    server_name api.example.com;

    location /graphql {
        proxy_pass http://localhost:4000/graphql;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;

        # CORS
        add_header 'Access-Control-Allow-Origin' '*' always;
        add_header 'Access-Control-Allow-Methods' 'GET, POST, OPTIONS' always;
        add_header 'Access-Control-Allow-Headers' 'Content-Type, Authorization' always;

        # Rate limiting
        limit_req zone=api burst=20 nodelay;
    }

    # WebSocket pour subscriptions
    location /subscriptions {
        proxy_pass http://localhost:4001;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
    }
}

# Rate limiting zone
limit_req_zone $binary_remote_addr zone=api:10m rate=10r/s;
```

```bash
# Activer le site
sudo ln -s /etc/nginx/sites-available/graphql /etc/nginx/sites-enabled/  
sudo nginx -t  
sudo systemctl reload nginx
```

### Configuration Apache (Windows/Ubuntu)

```apache
# Windows: httpd.conf
# Ubuntu: /etc/apache2/sites-available/graphql.conf

<VirtualHost *:80>
    ServerName api.example.com

    ProxyPreserveHost On
    ProxyPass /graphql http://localhost:4000/graphql
    ProxyPassReverse /graphql http://localhost:4000/graphql

    # WebSocket
    ProxyPass /subscriptions ws://localhost:4001/
    ProxyPassReverse /subscriptions ws://localhost:4001/

    # CORS
    Header always set Access-Control-Allow-Origin "*"
    Header always set Access-Control-Allow-Methods "GET, POST, OPTIONS"
    Header always set Access-Control-Allow-Headers "Content-Type, Authorization"
</VirtualHost>
```

### SSL/TLS avec Let's Encrypt (Ubuntu)

```bash
# Installer Certbot
sudo apt-get install certbot python3-certbot-nginx

# Obtenir un certificat
sudo certbot --nginx -d api.example.com

# Auto-renouvellement
sudo certbot renew --dry-run
```

### Docker

**Dockerfile :**

```dockerfile
FROM ubuntu:22.04

# Installer les dépendances
RUN apt-get update && apt-get install -y \
    fpc \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Copier l'application
WORKDIR /app  
COPY GraphQLServerApp /app/  
COPY config.ini /app/

# Port exposé
EXPOSE 4000

# Démarrer le serveur
CMD ["./GraphQLServerApp"]
```

**docker-compose.yml :**

```yaml
version: '3.8'

services:
  graphql:
    build: .
    ports:
      - "4000:4000"
    environment:
      - DB_HOST=postgres
      - DB_PORT=5432
      - DB_NAME=myapp
      - DB_USER=postgres
      - DB_PASSWORD=password
    depends_on:
      - postgres
    restart: always

  postgres:
    image: postgres:15
    environment:
      - POSTGRES_DB=myapp
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=password
    volumes:
      - postgres_data:/var/lib/postgresql/data
    restart: always

volumes:
  postgres_data:
```

```bash
# Démarrer
docker-compose up -d

# Logs
docker-compose logs -f graphql

# Arrêter
docker-compose down
```

## Sécurité avancée

### Protection contre les injections

```pascal
function SanitizeInput(const Input: String): String;  
begin
  Result := Input;

  // Supprimer les caractères dangereux
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);

  // Limiter la longueur
  if Length(Result) > 1000 then
    Result := Copy(Result, 1, 1000);
end;
```

### Validation des entrées

```pascal
function ValidateEmail(const Email: String): Boolean;  
const
  EmailRegex = '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$';
begin
  Result := TRegEx.IsMatch(Email, EmailRegex);
end;

function ValidateCreateUserInput(const Input: TCreateUserInput): TStringList;  
begin
  Result := TStringList.Create;

  if Trim(Input.Nom) = '' then
    Result.Add('Le nom est requis');

  if Trim(Input.Prenom) = '' then
    Result.Add('Le prénom est requis');

  if not ValidateEmail(Input.Email) then
    Result.Add('Email invalide');

  if Length(Input.Password) < 8 then
    Result.Add('Le mot de passe doit contenir au moins 8 caractères');
end;
```

### Headers de sécurité

```pascal
procedure SetSecurityHeaders(Response: TFPHTTPConnectionResponse);  
begin
  Response.SetCustomHeader('X-Content-Type-Options', 'nosniff');
  Response.SetCustomHeader('X-Frame-Options', 'DENY');
  Response.SetCustomHeader('X-XSS-Protection', '1; mode=block');
  Response.SetCustomHeader('Strict-Transport-Security',
    'max-age=31536000; includeSubDomains');
  Response.SetCustomHeader('Content-Security-Policy',
    'default-src ''self''; script-src ''self'' ''unsafe-inline''');
end;
```

## Conclusion

GraphQL représente une évolution majeure dans la conception d'API modernes. Avec FreePascal/Lazarus, vous pouvez créer des serveurs GraphQL performants et scalables qui fonctionnent de manière identique sur Windows et Ubuntu.

### Points clés à retenir

1. **GraphQL offre plus de flexibilité** que REST pour les clients
2. **Un seul endpoint** simplifie l'architecture
3. **Le typage fort** améliore la qualité et la documentation
4. **L'authentification JWT** sécurise vos API
5. **La pagination** est essentielle pour les grandes listes
6. **Le monitoring** permet d'identifier les problèmes rapidement
7. **Les tests automatisés** garantissent la fiabilité
8. **La sécurité** doit être une priorité dès le début

### Quand utiliser GraphQL ?

✅ **Utilisez GraphQL pour :**
- Applications mobiles (réduction du transfert de données)
- Interfaces complexes avec beaucoup de vues différentes
- APIs publiques avec de nombreux clients
- Microservices nécessitant une agrégation de données
- Applications temps réel avec subscriptions

❌ **Préférez REST pour :**
- APIs très simples avec peu d'endpoints
- Fichiers/uploads volumineux
- Caching HTTP intensif requis
- Équipe peu familière avec GraphQL

Avec les connaissances acquises dans ce chapitre, vous êtes maintenant capable de créer des API GraphQL modernes et performantes avec FreePascal/Lazarus, que ce soit sur Windows ou Ubuntu.

⏭️ [Programmation Web avec FreePascal](/09-programmation-web-freepascal/README.md)
