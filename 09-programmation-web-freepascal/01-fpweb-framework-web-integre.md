🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.1 fpWeb - Framework web intégré

## Introduction

fpWeb (FreePascal Web) est le framework web officiel intégré directement dans FreePascal. Il permet de créer des applications web complètes sans dépendances externes, aussi bien sur Windows que sur Ubuntu/Linux. Contrairement aux frameworks web modernes qui nécessitent de nombreuses bibliothèques tierces, fpWeb est minimaliste, performant et parfaitement intégré à l'écosystème FreePascal.

### Pourquoi utiliser fpWeb ?

- **Intégré nativement** : Aucune installation supplémentaire nécessaire
- **Multi-plateforme** : Fonctionne identiquement sur Windows et Linux
- **Léger et rapide** : Faible empreinte mémoire et excellentes performances
- **Contrôle total** : Accès bas niveau aux requêtes et réponses HTTP
- **Modes multiples** : CGI, FastCGI, serveur HTTP autonome
- **Production-ready** : Utilisé dans de nombreuses applications professionnelles

## Architecture de fpWeb

### Composants principaux

fpWeb repose sur plusieurs unités clés :

```pascal
uses
  fphttpapp,    // Application HTTP de base
  httpdefs,     // Définitions HTTP (codes, méthodes, etc.)
  httproute,    // Système de routage d'URLs
  fpwebdata,    // Gestion des sessions et données
  fphtml,       // Génération HTML programmatique
  fpjson;       // Support JSON natif
```

### Modèle de fonctionnement

fpWeb suit un modèle **requête-réponse** classique :

1. **Réception** : Une requête HTTP arrive (GET, POST, etc.)
2. **Routage** : L'URL est analysée et dirigée vers le bon gestionnaire
3. **Traitement** : Le code Pascal traite la requête
4. **Réponse** : Une réponse HTTP est générée (HTML, JSON, etc.)

### Les trois modes de déploiement

fpWeb peut fonctionner selon trois modes différents :

#### 1. Mode CGI (Common Gateway Interface)

Le plus simple pour débuter. Chaque requête lance un nouveau processus.

**Avantages** :
- Configuration minimale
- Compatible avec tous les serveurs web (Apache, Nginx, IIS)
- Isolation complète entre requêtes

**Inconvénients** :
- Plus lent (création de processus à chaque requête)
- Non adapté aux charges élevées

#### 2. Mode FastCGI

Un processus persistant communique avec le serveur web via un socket.

**Avantages** :
- Excellentes performances
- Processus réutilisé entre requêtes
- Standard industriel bien supporté

**Inconvénients** :
- Configuration légèrement plus complexe
- Nécessite un serveur web externe

#### 3. Mode serveur HTTP autonome (Standalone)

fpWeb intègre son propre serveur HTTP.

**Avantages** :
- Aucune dépendance externe
- Parfait pour le développement
- Idéal pour microservices et API

**Inconvénients** :
- Moins de fonctionnalités qu'Apache/Nginx
- Nécessite un reverse proxy en production

## Création d'une première application fpWeb

### Application minimale en mode standalone

Voici une application web complète qui tient en quelques lignes :

```pascal
program HelloWeb;

{$mode objfpc}{$H+}

uses
  fphttpapp;

procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);  
begin
  AResponse.Content :=
    '<html><body><h1>Bonjour depuis fpWeb !</h1></body></html>';
end;

begin
  Application.Title := 'Mon premier serveur fpWeb';
  Application.Port := 8080;
  Application.OnRequest := @HandleRequest;
  Application.Initialize;
  Application.Run;
end.
```

**Compilation et exécution** :

```bash
# Compilation
fpc HelloWeb.pas

# Exécution sur Windows
HelloWeb.exe

# Exécution sur Ubuntu/Linux
./HelloWeb
```

Ouvrez ensuite votre navigateur à l'adresse `http://localhost:8080`

### Analyse du code

- `fphttpapp` : L'unité principale pour les applications HTTP
- `TRequest` : Objet contenant toutes les informations de la requête
- `TResponse` : Objet pour construire la réponse
- `Application.Port` : Port d'écoute du serveur (8080 par défaut)
- `Application.OnRequest` : Gestionnaire appelé pour chaque requête

## Le système de routage avec HTTPRoute

Pour des applications plus complexes, le routage manuel devient fastidieux. fpWeb propose `HTTPRoute` pour gérer les URLs de manière élégante.

### Exemple avec routage

```pascal
program WebRoutes;

{$mode objfpc}{$H+}

uses
  fphttpapp, httproute, httpdefs;

procedure ShowHomePage(ARequest: TRequest; AResponse: TResponse);  
begin
  AResponse.Content := '<h1>Page d''accueil</h1><p>Bienvenue !</p>';
end;

procedure ShowAboutPage(ARequest: TRequest; AResponse: TResponse);  
begin
  AResponse.Content := '<h1>À propos</h1><p>Application fpWeb</p>';
end;

procedure ShowUserProfile(ARequest: TRequest; AResponse: TResponse);  
var
  Username: String;
begin
  Username := ARequest.RouteParams['username'];
  AResponse.Content := Format('<h1>Profil de %s</h1>', [Username]);
end;

begin
  // Configuration des routes
  HTTPRouter.RegisterRoute('/', @ShowHomePage);
  HTTPRouter.RegisterRoute('/about', @ShowAboutPage);
  HTTPRouter.RegisterRoute('/user/:username', @ShowUserProfile);

  Application.Port := 8080;
  Application.Initialize;
  Application.Run;
end.
```

### Paramètres dynamiques dans les routes

Les routes peuvent contenir des paramètres dynamiques :

- `/user/:username` → Capture le nom d'utilisateur
- `/article/:id` → Capture l'ID de l'article
- `/category/:cat/product/:id` → Plusieurs paramètres

Accès aux paramètres via : `ARequest.RouteParams['nom_parametre']`

## Gestion des méthodes HTTP

fpWeb reconnaît automatiquement les différentes méthodes HTTP (GET, POST, PUT, DELETE, etc.).

### Exemple avec différentes méthodes

```pascal
procedure HandleUsers(ARequest: TRequest; AResponse: TResponse);  
var
  Method: string;
begin
  Method := ARequest.Method;

  if Method = 'GET' then
    // Lire la liste des utilisateurs
    AResponse.Content := '{"users": ["Alice", "Bob"]}'
  else if Method = 'POST' then
  begin
    // Créer un nouvel utilisateur
    // Traiter ARequest.Content (données POST)
    AResponse.Content := '{"status": "created"}';
  end
  else if Method = 'PUT' then
    // Modifier un utilisateur
    AResponse.Content := '{"status": "updated"}'
  else if Method = 'DELETE' then
    // Supprimer un utilisateur
    AResponse.Content := '{"status": "deleted"}'
  else
    AResponse.Code := 405; // Method Not Allowed

  AResponse.ContentType := 'application/json';
end;
```

## Traitement des données de formulaire

### Formulaires GET

Les paramètres GET sont accessibles via `QueryFields` :

```pascal
procedure SearchProducts(ARequest: TRequest; AResponse: TResponse);  
var
  SearchTerm: String;
begin
  SearchTerm := ARequest.QueryFields.Values['q'];
  AResponse.Content := Format('Recherche : %s', [SearchTerm]);
end;

// URL : /search?q=ordinateur
```

### Formulaires POST

Les données POST sont dans `ContentFields` :

```pascal
procedure HandleLogin(ARequest: TRequest; AResponse: TResponse);  
var
  Username, Password: String;
begin
  Username := ARequest.ContentFields.Values['username'];
  Password := ARequest.ContentFields.Values['password'];

  // Vérification des identifiants...

  if ValidCredentials(Username, Password) then
    AResponse.Content := 'Connexion réussie'
  else
    AResponse.Content := 'Identifiants incorrects';
end;
```

## Gestion des sessions

fpWeb inclut un système de sessions intégré pour maintenir l'état entre requêtes.

### Activation des sessions

```pascal
uses
  fphttpapp, httpdefs, fpwebdata, fpsessioncookie;

var
  SessionFactory: TFPWebSessionFactory;

begin
  // Créer une fabrique de sessions
  SessionFactory := TFPWebSessionFactory.Create;
  SessionFactory.SessionCookieName := 'FPWEB_SESSIONID';
  SessionFactory.SessionDir := 'sessions'; // Répertoire de stockage
  SessionFactory.DefaultTimeOut := 30; // 30 minutes

  // Enregistrer la fabrique
  SetDefaultSessionFactory(SessionFactory);

  Application.Initialize;
  Application.Run;
end.
```

### Utilisation des sessions

```pascal
procedure HandleUserArea(ARequest: TRequest; AResponse: TResponse);  
var
  Session: TCustomSession;
  Username: String;
begin
  Session := ARequest.Session;

  // Stocker une valeur
  Session.Variables['username'] := 'Alice';

  // Lire une valeur
  if Session.Variables['username'] <> '' then
  begin
    Username := Session.Variables['username'];
    AResponse.Content := Format('Bonjour %s !', [Username]);
  end
  else
    AResponse.Content := 'Veuillez vous connecter';
end;
```

### Stockage de sessions

Les sessions peuvent être stockées de différentes manières :

1. **Fichiers** (par défaut) : Dans un répertoire local
2. **Base de données** : Pour les déploiements distribués
3. **Mémoire** : Rapide mais volatile

## Génération de contenu dynamique

### HTML programmatique avec fpHTML

```pascal
uses
  fphtml;

procedure GeneratePage(ARequest: TRequest; AResponse: TResponse);  
var
  Doc: THTMLDocument;
  Body, H1, Para: THTMLElement;
begin
  Doc := THTMLDocument.Create;
  try
    // Structure de base
    Body := Doc.Body;

    // Titre
    H1 := Body.AppendElement('h1');
    H1.TextContent := 'Page générée dynamiquement';

    // Paragraphe
    Para := Body.AppendElement('p');
    Para.TextContent := 'Contenu créé avec fpHTML';
    Para.SetAttribute('class', 'description');

    // Rendu
    AResponse.Content := Doc.AsHTML;
  finally
    Doc.Free;
  end;
end;
```

### Réponses JSON avec fpJSON

```pascal
uses
  fpjson, jsonparser;

procedure GetUserJSON(ARequest: TRequest; AResponse: TResponse);  
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('id', 1);
    JSONObj.Add('name', 'Alice');
    JSONObj.Add('email', 'alice@example.com');

    AResponse.Content := JSONObj.AsJSON;
    AResponse.ContentType := 'application/json';
  finally
    JSONObj.Free;
  end;
end;
```

### Parsing de JSON reçu

```pascal
procedure CreateUser(ARequest: TRequest; AResponse: TResponse);  
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  Name, Email: String;
begin
  // Parser le JSON reçu
  JSONData := GetJSON(ARequest.Content);
  try
    if JSONData is TJSONObject then
    begin
      JSONObj := TJSONObject(JSONData);
      Name := JSONObj.Get('name', '');
      Email := JSONObj.Get('email', '');

      // Traiter les données...

      AResponse.Content := '{"status": "success"}';
      AResponse.ContentType := 'application/json';
    end;
  finally
    JSONData.Free;
  end;
end;
```

## Gestion des erreurs et codes HTTP

### Codes de statut HTTP

```pascal
procedure HandleNotFound(ARequest: TRequest; AResponse: TResponse);  
begin
  AResponse.Code := 404;
  AResponse.CodeText := 'Not Found';
  AResponse.Content := '<h1>Page non trouvée</h1>';
end;

procedure HandleServerError(ARequest: TRequest; AResponse: TResponse);  
begin
  AResponse.Code := 500;
  AResponse.CodeText := 'Internal Server Error';
  AResponse.Content := '<h1>Erreur serveur</h1>';
end;
```

### Gestion globale des exceptions

```pascal
procedure SafeHandler(ARequest: TRequest; AResponse: TResponse);  
begin
  try
    // Code potentiellement dangereux
    ProcessRequest(ARequest, AResponse);
  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := Format('<h1>Erreur</h1><p>%s</p>', [E.Message]);
    end;
  end;
end;
```

## Headers HTTP personnalisés

### Définir des headers

```pascal
procedure HandleCORS(ARequest: TRequest; AResponse: TResponse);  
begin
  // Autoriser les requêtes cross-origin
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
  AResponse.SetCustomHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE');

  // Cache control
  AResponse.SetCustomHeader('Cache-Control', 'no-cache, no-store');

  AResponse.Content := '{"message": "API publique"}';
end;
```

### Lire les headers de la requête

```pascal
procedure CheckAuthorization(ARequest: TRequest; AResponse: TResponse);  
var
  AuthHeader: String;
begin
  AuthHeader := ARequest.GetCustomHeader('Authorization');

  if AuthHeader = '' then
  begin
    AResponse.Code := 401;
    AResponse.SetCustomHeader('WWW-Authenticate', 'Bearer');
    AResponse.Content := 'Authentification requise';
  end
  else
    AResponse.Content := 'Accès autorisé';
end;
```

## Cookies

### Créer un cookie

```pascal
procedure SetUserCookie(ARequest: TRequest; AResponse: TResponse);  
var
  Cookie: TCookie;
begin
  Cookie := AResponse.Cookies.Add;
  Cookie.Name := 'username';
  Cookie.Value := 'Alice';
  Cookie.Path := '/';
  Cookie.MaxAge := 3600; // 1 heure en secondes
  Cookie.HttpOnly := True; // Protection XSS
  Cookie.Secure := False; // True pour HTTPS uniquement

  AResponse.Content := 'Cookie défini';
end;
```

### Lire un cookie

```pascal
procedure ReadUserCookie(ARequest: TRequest; AResponse: TResponse);  
var
  Username: String;
begin
  Username := ARequest.CookieFields.Values['username'];

  if Username <> '' then
    AResponse.Content := Format('Bonjour %s !', [Username])
  else
    AResponse.Content := 'Aucun cookie trouvé';
end;
```

## Upload de fichiers

```pascal
procedure HandleFileUpload(ARequest: TRequest; AResponse: TResponse);  
var
  UploadedFile: TUploadedFile;
  FileStream: TFileStream;
  i: Integer;
begin
  if ARequest.Files.Count > 0 then
  begin
    for i := 0 to ARequest.Files.Count - 1 do
    begin
      UploadedFile := ARequest.Files[i];

      // Sauvegarder le fichier
      FileStream := TFileStream.Create(
        'uploads/' + UploadedFile.FileName,
        fmCreate
      );
      try
        FileStream.CopyFrom(UploadedFile.Stream, 0);
      finally
        FileStream.Free;
      end;
    end;

    AResponse.Content := 'Fichier(s) téléchargé(s) avec succès';
  end
  else
    AResponse.Content := 'Aucun fichier reçu';
end;
```

## Configuration multi-plateforme

### Chemins et séparateurs

```pascal
uses
  sysutils;

function GetSessionPath: String;  
begin
  {$IFDEF WINDOWS}
  Result := 'C:\webapp\sessions\';
  {$ELSE}
  Result := '/var/www/sessions/';
  {$ENDIF}
end;
```

### Port et binding

```pascal
begin
  // Écouter sur toutes les interfaces
  Application.Address := '0.0.0.0';

  {$IFDEF WINDOWS}
  Application.Port := 8080; // Port non privilégié
  {$ELSE}
  Application.Port := 80;   // Port standard (nécessite root)
  {$ENDIF}

  Application.Initialize;
  Application.Run;
end.
```

## Logging et débogage

### Logging basique

```pascal
uses
  eventlog;

var
  Logger: TEventLog;

procedure LogRequest(ARequest: TRequest);  
begin
  Logger.Info(Format('Requête: %s %s depuis %s',
    [ARequest.Method, ARequest.URI, ARequest.RemoteAddr]));
end;

begin
  Logger := TEventLog.Create(nil);
  Logger.LogType := ltFile;
  Logger.FileName := 'webapp.log';

  // ... configuration de l'application

  Logger.Free;
end.
```

### Mode debug

```pascal
{$DEFINE DEBUG}

procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);  
begin
  {$IFDEF DEBUG}
  WriteLn('URI: ', ARequest.URI);
  WriteLn('Method: ', ARequest.Method);
  WriteLn('Content-Type: ', ARequest.ContentType);
  {$ENDIF}

  // Traitement normal...
end;
```

## Optimisations et bonnes pratiques

### Réutilisation d'objets

```pascal
var
  GlobalJSONFactory: TJSONObject;

procedure OptimizedHandler(ARequest: TRequest; AResponse: TResponse);  
begin
  // Réutiliser des objets plutôt que créer/détruire
  GlobalJSONFactory.Clear;
  GlobalJSONFactory.Add('timestamp', Now);

  AResponse.Content := GlobalJSONFactory.AsJSON;
end;
```

### Mise en cache

```pascal
var
  CachedContent: String;
  CacheTime: TDateTime;

procedure CachedHandler(ARequest: TRequest; AResponse: TResponse);  
begin
  // Cache de 5 minutes
  if (CachedContent = '') or (Now - CacheTime > 1/288) then
  begin
    CachedContent := GenerateExpensiveContent();
    CacheTime := Now;
  end;

  AResponse.Content := CachedContent;
end;
```

### Compression

```pascal
uses
  zstream;

procedure CompressedResponse(ARequest: TRequest; AResponse: TResponse);  
var
  Content: String;
  Compressed: TMemoryStream;
  Compressor: TCompressionStream;
begin
  Content := GenerateLargeContent();

  if Pos('gzip', ARequest.GetCustomHeader('Accept-Encoding')) > 0 then
  begin
    Compressed := TMemoryStream.Create;
    try
      Compressor := TCompressionStream.Create(clDefault, Compressed);
      try
        Compressor.WriteBuffer(Content[1], Length(Content));
      finally
        Compressor.Free;
      end;

      AResponse.SetCustomHeader('Content-Encoding', 'gzip');
      AResponse.ContentStream := Compressed;
    except
      Compressed.Free;
      raise;
    end;
  end
  else
    AResponse.Content := Content;
end;
```

## Conclusion

fpWeb est un framework web puissant et flexible qui offre :

- **Simplicité** : API claire et intuitive
- **Performance** : Overhead minimal
- **Portabilité** : Code identique Windows/Linux
- **Flexibilité** : Trois modes de déploiement
- **Intégration** : Natif dans FreePascal

Avec fpWeb, vous pouvez créer aussi bien des APIs REST légères que des applications web complètes, le tout avec les performances et la fiabilité du Pascal compilé.

Dans les prochaines sections, nous explorerons l'intégration de fpWeb avec FastCGI pour des déploiements en production haute performance sur Apache et Nginx.

⏭️ [FastCGI et développement CGI](/09-programmation-web-freepascal/02-fastcgi-developpement-cgi.md)
