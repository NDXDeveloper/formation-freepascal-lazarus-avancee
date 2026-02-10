🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.2 FastCGI et développement CGI

## Introduction

CGI (Common Gateway Interface) et FastCGI sont deux technologies fondamentales pour l'intégration d'applications web avec des serveurs HTTP professionnels comme Apache, Nginx et IIS. Bien que fpWeb puisse fonctionner en mode standalone, l'utilisation de CGI ou FastCGI avec un serveur web établi offre de nombreux avantages en production.

### Comprendre CGI et FastCGI

#### Qu'est-ce que CGI ?

**CGI** (Common Gateway Interface) est le protocole standard le plus ancien et le plus simple pour exécuter des programmes externes depuis un serveur web.

**Principe de fonctionnement** :

1. Le serveur web reçoit une requête HTTP
2. Il lance un **nouveau processus** de votre application FreePascal
3. Les paramètres de la requête sont passés via **variables d'environnement**
4. Votre programme génère une réponse sur la **sortie standard** (stdout)
5. Le serveur web envoie cette réponse au client
6. Le processus se **termine**

**Schéma simplifié** :
```
Requête → Apache/Nginx/IIS → [Nouveau processus FreePascal]
                               ↓
                             Variables d'environnement (GET, POST, etc.)
                               ↓
                             Traitement
                               ↓
                             Sortie standard (HTML, JSON...)
                               ↓
Réponse ← Apache/Nginx/IIS ← [Fin du processus]
```

#### Qu'est-ce que FastCGI ?

**FastCGI** est une évolution de CGI qui résout ses problèmes de performance en maintenant des **processus persistants**.

**Principe de fonctionnement** :

1. Votre application FreePascal démarre **une seule fois**
2. Elle reste en mémoire et **écoute sur un socket** (TCP ou Unix socket)
3. Le serveur web communique avec ce processus persistant
4. Une même instance traite **toutes les requêtes**
5. Pas de création/destruction de processus

**Schéma simplifié** :
```
                    ┌─────────────────────────────┐
                    │  Processus FreePascal       │
                    │  (persistant)               │
Requête → Apache ───┼→ Socket FastCGI ──→ Traitement
                    │                             │
Réponse ← Apache ←──┼─ Réponse ←──────── Handler  │
                    └─────────────────────────────┘
                         (reste en mémoire)
```

### Comparaison CGI vs FastCGI

| Aspect | CGI | FastCGI |
|--------|-----|---------|
| **Performance** | Lente (nouveau processus) | Rapide (processus réutilisé) |
| **Consommation mémoire** | Élevée (multiples processus) | Faible (processus partagé) |
| **Temps de démarrage** | À chaque requête | Une seule fois |
| **Connexions BDD** | Nouvelle à chaque fois | Persistantes (pool) |
| **État en mémoire** | Impossible | Cache, sessions... |
| **Complexité setup** | Très simple | Moyenne |
| **Isolation** | Totale (nouveau processus) | Partagée (attention threads) |
| **Charge supportée** | Faible (<100 req/s) | Élevée (>1000 req/s) |
| **Usage recommandé** | Prototypage, faible trafic | Production, trafic élevé |

### Quand utiliser CGI vs FastCGI ?

#### Utiliser CGI si :

✅ Vous débutez et voulez la simplicité  
✅ Trafic très faible (quelques requêtes/minute)  
✅ Application qui doit être isolée à chaque exécution  
✅ Prototypage rapide  
✅ Scripts d'administration ponctuels

#### Utiliser FastCGI si :

✅ Application en production  
✅ Trafic moyen à élevé (>50 req/min)  
✅ Besoin de performance  
✅ Connexions base de données à optimiser  
✅ Cache en mémoire souhaité  
✅ **Recommandé pour 99% des cas réels**

## Développement CGI avec FreePascal

### Structure d'une application CGI

Une application CGI FreePascal ressemble beaucoup à une application console, mais utilise des fonctions spéciales pour lire les variables d'environnement HTTP.

### Application CGI minimale

```pascal
program SimpleCGI;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

var
  QueryString: String;

begin
  // En-tête HTTP obligatoire
  WriteLn('Content-Type: text/html');
  WriteLn; // Ligne vide importante !

  // Récupérer la query string
  QueryString := GetEnvironmentVariable('QUERY_STRING');

  // Génération de la réponse
  WriteLn('<html>');
  WriteLn('<head><title>CGI Simple</title></head>');
  WriteLn('<body>');
  WriteLn('<h1>Bonjour depuis CGI !</h1>');
  WriteLn('<p>Query string: ', QueryString, '</p>');
  WriteLn('</body>');
  WriteLn('</html>');
end.
```

**Points essentiels** :

1. **En-tête Content-Type** : Doit être la première sortie
2. **Ligne vide** : Sépare les headers du contenu
3. **Variables d'environnement** : Contiennent toutes les infos de la requête
4. **Sortie standard** : Tout ce qui est écrit va au client

### Variables d'environnement CGI

Le serveur web passe des informations via des variables d'environnement standard :

| Variable | Description | Exemple |
|----------|-------------|---------|
| `REQUEST_METHOD` | Méthode HTTP | GET, POST, PUT, DELETE |
| `QUERY_STRING` | Paramètres GET | name=Alice&age=30 |
| `CONTENT_TYPE` | Type de contenu POST | application/x-www-form-urlencoded |
| `CONTENT_LENGTH` | Taille du corps POST | 256 |
| `REQUEST_URI` | URL complète | /users/profile?id=123 |
| `SCRIPT_NAME` | Chemin du script | /cgi-bin/app.cgi |
| `SERVER_NAME` | Nom du serveur | www.example.com |
| `SERVER_PORT` | Port du serveur | 80, 443 |
| `REMOTE_ADDR` | IP du client | 192.168.1.100 |
| `HTTP_USER_AGENT` | Navigateur client | Mozilla/5.0... |
| `HTTP_COOKIE` | Cookies envoyés | session=abc123; user=Alice |

### Lecture des variables d'environnement

```pascal
uses
  SysUtils;

function GetCGIVar(const VarName: String): String;  
begin
  Result := GetEnvironmentVariable(VarName);
end;

// Exemples d'utilisation
var
  Method, QueryString, RemoteIP: String;
begin
  Method := GetCGIVar('REQUEST_METHOD');
  QueryString := GetCGIVar('QUERY_STRING');
  RemoteIP := GetCGIVar('REMOTE_ADDR');
end;
```

### Traitement des données GET

Les paramètres GET sont dans la `QUERY_STRING` au format `key1=value1&key2=value2`.

```pascal
uses
  Classes, SysUtils, StrUtils;

type
  TStringPairArray = array of record
    Key, Value: String;
  end;

function ParseQueryString(const QS: String): TStringPairArray;  
var
  Pairs: TStringList;
  Pair: String;
  EqualPos: Integer;
  i: Integer;
begin
  Pairs := TStringList.Create;
  try
    // Découper par &
    ExtractStrings(['&'], [], PChar(QS), Pairs);

    SetLength(Result, Pairs.Count);

    for i := 0 to Pairs.Count - 1 do
    begin
      Pair := Pairs[i];
      EqualPos := Pos('=', Pair);

      if EqualPos > 0 then
      begin
        Result[i].Key := Copy(Pair, 1, EqualPos - 1);
        Result[i].Value := Copy(Pair, EqualPos + 1, Length(Pair));

        // Décoder les caractères encodés (%20, etc.)
        Result[i].Key := HTTPDecode(Result[i].Key);
        Result[i].Value := HTTPDecode(Result[i].Value);
      end;
    end;
  finally
    Pairs.Free;
  end;
end;

function HTTPDecode(const AStr: String): String;  
var
  i: Integer;
  HexStr: String;
begin
  Result := '';
  i := 1;

  while i <= Length(AStr) do
  begin
    if AStr[i] = '+' then
      Result := Result + ' '
    else if (AStr[i] = '%') and (i + 2 <= Length(AStr)) then
    begin
      HexStr := '$' + Copy(AStr, i + 1, 2);
      Result := Result + Chr(StrToInt(HexStr));
      Inc(i, 2);
    end
    else
      Result := Result + AStr[i];

    Inc(i);
  end;
end;
```

**Utilisation** :
```pascal
var
  Params: TStringPairArray;
  i: Integer;
begin
  Params := ParseQueryString(GetCGIVar('QUERY_STRING'));

  for i := 0 to High(Params) do
    WriteLn('Paramètre: ', Params[i].Key, ' = ', Params[i].Value);
end;
```

### Traitement des données POST

Les données POST arrivent sur l'**entrée standard** (stdin).

```pascal
uses
  Classes, SysUtils;

function ReadPOSTData: String;  
var
  ContentLength: Integer;
  Buffer: String;
begin
  Result := '';

  // Récupérer la taille des données
  ContentLength := StrToIntDef(GetCGIVar('CONTENT_LENGTH'), 0);

  if ContentLength > 0 then
  begin
    SetLength(Buffer, ContentLength);

    // Lire depuis stdin
    BlockRead(Input, Buffer[1], ContentLength);
    Result := Buffer;
  end;
end;

// Exemple d'utilisation
var
  POSTData: String;
  Params: TStringPairArray;
begin
  if GetCGIVar('REQUEST_METHOD') = 'POST' then
  begin
    POSTData := ReadPOSTData;
    Params := ParseQueryString(POSTData); // Même parsing que GET

    // Traiter les données...
  end;
end;
```

### Application CGI complète avec formulaire

```pascal
program FormCGI;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

function GetCGIVar(const VarName: String): String;  
begin
  Result := GetEnvironmentVariable(VarName);
end;

function ReadPOSTData: String;  
var
  ContentLength: Integer;
  Buffer: String;
begin
  Result := '';
  ContentLength := StrToIntDef(GetCGIVar('CONTENT_LENGTH'), 0);

  if ContentLength > 0 then
  begin
    SetLength(Buffer, ContentLength);
    BlockRead(Input, Buffer[1], ContentLength);
    Result := Buffer;
  end;
end;

procedure ShowForm;  
begin
  WriteLn('<form method="POST">');
  WriteLn('  <label>Nom: <input type="text" name="name"></label><br>');
  WriteLn('  <label>Email: <input type="email" name="email"></label><br>');
  WriteLn('  <button type="submit">Envoyer</button>');
  WriteLn('</form>');
end;

procedure ProcessForm(const POSTData: String);  
var
  Name, Email: String;
begin
  // Extraction simple (production : utiliser un parser robuste)
  if Pos('name=', POSTData) > 0 then
    Name := Copy(POSTData, Pos('name=', POSTData) + 5, 50);

  WriteLn('<h2>Données reçues :</h2>');
  WriteLn('<p>Nom: ', Name, '</p>');
  WriteLn('<p>Merci pour votre soumission !</p>');
end;

var
  Method, POSTData: String;

begin
  // En-têtes HTTP
  WriteLn('Content-Type: text/html; charset=utf-8');
  WriteLn;

  // Structure HTML
  WriteLn('<!DOCTYPE html>');
  WriteLn('<html>');
  WriteLn('<head><title>Formulaire CGI</title></head>');
  WriteLn('<body>');
  WriteLn('<h1>Application CGI avec formulaire</h1>');

  Method := GetCGIVar('REQUEST_METHOD');

  if Method = 'POST' then
  begin
    POSTData := ReadPOSTData;
    ProcessForm(POSTData);
  end
  else
    ShowForm;

  WriteLn('</body>');
  WriteLn('</html>');
end.
```

### Gestion des cookies en CGI

**Envoyer un cookie** :
```pascal
procedure SetCookie(const Name, Value: String; MaxAge: Integer = 3600);  
begin
  WriteLn('Set-Cookie: ', Name, '=', Value,
          '; Max-Age=', MaxAge, '; Path=/');
end;

// Utilisation
begin
  WriteLn('Content-Type: text/html');
  SetCookie('user', 'Alice', 7200); // 2 heures
  SetCookie('session', 'xyz123', 3600);
  WriteLn; // Ligne vide après les headers

  // Contenu...
end;
```

**Lire les cookies** :
```pascal
function GetCookie(const Name: String): String;  
var
  CookieHeader: String;
  Pos1, Pos2: Integer;
begin
  Result := '';
  CookieHeader := GetCGIVar('HTTP_COOKIE');

  Pos1 := Pos(Name + '=', CookieHeader);
  if Pos1 > 0 then
  begin
    Pos1 := Pos1 + Length(Name) + 1;
    Pos2 := PosEx(';', CookieHeader, Pos1);

    if Pos2 = 0 then
      Pos2 := Length(CookieHeader) + 1;

    Result := Copy(CookieHeader, Pos1, Pos2 - Pos1);
  end;
end;
```

## Développement FastCGI avec FreePascal

### Différences fondamentales avec CGI

En FastCGI, votre application :
- Démarre **une seule fois**
- Entre dans une **boucle d'écoute**
- Traite les requêtes dans cette boucle
- Ne se termine jamais (sauf arrêt explicite)

### Application FastCGI minimale

```pascal
program SimpleFastCGI;

{$mode objfpc}{$H+}

uses
  fphttpapp, httpdefs;

procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);  
begin
  AResponse.Content :=
    '<html><body>' +
    '<h1>Bonjour depuis FastCGI !</h1>' +
    '<p>Requête traitée par un processus persistant</p>' +
    '</body></html>';
end;

begin
  Application.Title := 'Application FastCGI';
  Application.OnRequest := @HandleRequest;
  Application.Initialize;
  Application.Run; // Boucle infinie d'écoute
end.
```

**Points essentiels** :

1. Utilisation de `fphttpapp` (comme mode standalone)
2. `Application.Run` ne se termine pas
3. Gestion automatique des headers HTTP
4. Objets `TRequest` et `TResponse` fournis automatiquement

### Structure d'une application FastCGI

```pascal
program WebApp;

{$mode objfpc}{$H+}

uses
  fphttpapp, httpdefs, httproute, fpjson;

// Gestionnaires de routes
procedure ShowHome(ARequest: TRequest; AResponse: TResponse);  
begin
  AResponse.Content := '<h1>Page d''accueil</h1>';
end;

procedure ShowUser(ARequest: TRequest; AResponse: TResponse);  
var
  UserID: String;
begin
  UserID := ARequest.RouteParams['id'];
  AResponse.Content := Format('<h1>Utilisateur %s</h1>', [UserID]);
end;

procedure GetUserJSON(ARequest: TRequest; AResponse: TResponse);  
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('id', 123);
    JSONObj.Add('name', 'Alice');

    AResponse.ContentType := 'application/json';
    AResponse.Content := JSONObj.AsJSON;
  finally
    JSONObj.Free;
  end;
end;

begin
  // Configuration des routes
  HTTPRouter.RegisterRoute('/', @ShowHome);
  HTTPRouter.RegisterRoute('/user/:id', @ShowUser);
  HTTPRouter.RegisterRoute('/api/user/:id', @GetUserJSON);

  // Démarrage de l'application
  Application.Title := 'Mon Application Web';
  Application.Initialize;
  Application.Run;
end.
```

### Accès aux informations de requête

L'objet `TRequest` fournit toutes les informations :

```pascal
procedure AnalyzeRequest(ARequest: TRequest; AResponse: TResponse);  
var
  Info: String;
  i: Integer;
begin
  Info := '<h2>Informations de la requête</h2>';

  // Méthode HTTP
  Info := Info + '<p><strong>Méthode:</strong> ' + ARequest.Method + '</p>';

  // URL complète
  Info := Info + '<p><strong>URI:</strong> ' + ARequest.URI + '</p>';

  // IP du client
  Info := Info + '<p><strong>IP:</strong> ' + ARequest.RemoteAddr + '</p>';

  // User-Agent
  Info := Info + '<p><strong>Navigateur:</strong> ' +
          ARequest.GetCustomHeader('User-Agent') + '</p>';

  // Paramètres GET
  if ARequest.QueryFields.Count > 0 then
  begin
    Info := Info + '<h3>Paramètres GET:</h3><ul>';
    for i := 0 to ARequest.QueryFields.Count - 1 do
      Info := Info + '<li>' + ARequest.QueryFields.Names[i] + ' = ' +
              ARequest.QueryFields.ValueFromIndex[i] + '</li>';
    Info := Info + '</ul>';
  end;

  AResponse.Content := Info;
end;
```

### Gestion des sessions en FastCGI

FastCGI permet de maintenir l'état en mémoire entre requêtes :

```pascal
uses
  fphttpapp, httpdefs, fpsession;

var
  SessionFactory: TFPWebSessionFactory;

procedure ConfigureSessions;  
begin
  SessionFactory := TFPWebSessionFactory.Create;
  SessionFactory.SessionCookieName := 'SESSIONID';
  SessionFactory.DefaultTimeOut := 30; // 30 minutes

  SetDefaultSessionFactory(SessionFactory);
end;

procedure HandleUserArea(ARequest: TRequest; AResponse: TResponse);  
var
  Session: TCustomSession;
  Username: String;
begin
  Session := ARequest.Session;

  // Première visite ?
  if Session.Variables['visits'] = '' then
    Session.Variables['visits'] := '1'
  else
    Session.Variables['visits'] :=
      IntToStr(StrToInt(Session.Variables['visits']) + 1);

  AResponse.Content := Format(
    '<h1>Bienvenue !</h1>' +
    '<p>Nombre de visites: %s</p>',
    [Session.Variables['visits']]
  );
end;

begin
  ConfigureSessions;

  HTTPRouter.RegisterRoute('/protected', @HandleUserArea);

  Application.Initialize;
  Application.Run;
end.
```

### Cache et état global

Puisque le processus est persistant, vous pouvez maintenir des données en mémoire :

```pascal
var
  GlobalCache: TStringList;
  CacheLastUpdate: TDateTime;

procedure InitializeCache;  
begin
  GlobalCache := TStringList.Create;
  GlobalCache.Add('key1=value1');
  GlobalCache.Add('key2=value2');
  CacheLastUpdate := Now;
end;

procedure RefreshCacheIfNeeded;  
begin
  // Rafraîchir toutes les 5 minutes
  if (Now - CacheLastUpdate) > (5 / (24 * 60)) then
  begin
    // Recharger depuis la BDD...
    GlobalCache.Clear;
    // ... ajout de nouvelles données
    CacheLastUpdate := Now;
  end;
end;

procedure HandleCachedData(ARequest: TRequest; AResponse: TResponse);  
var
  i: Integer;
begin
  RefreshCacheIfNeeded;

  AResponse.Content := '<h1>Données en cache:</h1><ul>';
  for i := 0 to GlobalCache.Count - 1 do
    AResponse.Content := AResponse.Content +
      '<li>' + GlobalCache[i] + '</li>';
  AResponse.Content := AResponse.Content + '</ul>';
end;

begin
  InitializeCache;
  // ... reste de l'application
end.
```

### Connexions base de données persistantes

```pascal
uses
  sqldb, mysql55conn;

var
  DBConnection: TSQLConnection;
  DBTransaction: TSQLTransaction;

procedure InitializeDatabase;  
begin
  DBConnection := TMySQL55Connection.Create(nil);
  DBConnection.HostName := 'localhost';
  DBConnection.DatabaseName := 'mydb';
  DBConnection.UserName := 'user';
  DBConnection.Password := 'password';

  DBTransaction := TSQLTransaction.Create(nil);
  DBTransaction.Database := DBConnection;
  DBConnection.Transaction := DBTransaction;

  DBConnection.Open; // Une seule fois !
end;

procedure HandleDBQuery(ARequest: TRequest; AResponse: TResponse);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := DBConnection;
    Query.SQL.Text := 'SELECT * FROM users LIMIT 10';
    Query.Open;

    // Traiter les résultats...

    Query.Close;
  finally
    Query.Free;
  end;
end;

begin
  InitializeDatabase;

  HTTPRouter.RegisterRoute('/users', @HandleDBQuery);

  Application.Initialize;
  Application.Run;

  // Nettoyage (jamais atteint sauf arrêt)
  DBConnection.Free;
  DBTransaction.Free;
end.
```

### Compilation pour FastCGI

**Compilation identique** :
```bash
# Windows
fpc -MObjFPC -Scghi webapp.pas

# Linux
fpc -MObjFPC -Scghi webapp.pas
```

Le même code fonctionne en CGI, FastCGI ou standalone selon la configuration du serveur web !

### Mode de communication FastCGI

FastCGI peut utiliser deux modes de communication :

#### 1. Socket TCP/IP
```pascal
Application.Port := 9000; // Port d'écoute  
Application.Address := '127.0.0.1'; // Localhost uniquement
```

Serveur web → `localhost:9000` → Application

#### 2. Unix Socket (Linux uniquement)
```pascal
Application.Address := '/var/run/webapp.sock';
```

Plus rapide que TCP, mais uniquement local.

## Gestion des erreurs et logging

### Gestion des exceptions

```pascal
procedure SafeHandler(ARequest: TRequest; AResponse: TResponse);  
begin
  try
    // Code potentiellement dangereux
    ProcessRequest(ARequest, AResponse);
  except
    on E: Exception do
    begin
      // Logger l'erreur
      WriteLn(StdErr, 'Erreur: ', E.Message);

      // Réponse utilisateur
      AResponse.Code := 500;
      AResponse.Content :=
        '<h1>Erreur serveur</h1>' +
        '<p>Une erreur est survenue.</p>';
    end;
  end;
end;
```

### Logging robuste

```pascal
uses
  eventlog;

var
  Logger: TEventLog;

procedure InitLogger;  
begin
  Logger := TEventLog.Create(nil);
  Logger.LogType := ltFile;

  {$IFDEF WINDOWS}
  Logger.FileName := 'C:\logs\webapp.log';
  {$ELSE}
  Logger.FileName := '/var/log/webapp.log';
  {$ENDIF}

  Logger.Active := True;
end;

procedure LogRequest(ARequest: TRequest);  
begin
  Logger.Info(Format('[%s] %s %s from %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    ARequest.Method,
    ARequest.URI,
    ARequest.RemoteAddr
  ]));
end;

procedure LogError(const ErrorMsg: String);  
begin
  Logger.Error(Format('[%s] ERROR: %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    ErrorMsg
  ]));
end;
```

## Tests et développement local

### Mode de test standalone

Pendant le développement, testez en mode standalone :

```pascal
{$DEFINE STANDALONE_MODE}

begin
  {$IFDEF STANDALONE_MODE}
  // Mode développement : serveur autonome
  Application.Port := 8080;
  WriteLn('Serveur de développement sur http://localhost:8080');
  {$ELSE}
  // Mode production : FastCGI
  {$ENDIF}

  Application.Initialize;
  Application.Run;
end.
```

Compilez avec ou sans `-dSTANDALONE_MODE` :
```bash
# Développement
fpc -dSTANDALONE_MODE webapp.pas

# Production
fpc webapp.pas
```

### Simulation de variables CGI

Pour tester en mode CGI sans serveur web :

```bash
# Linux/macOS
export REQUEST_METHOD=GET  
export QUERY_STRING="name=Alice&age=30"
./app.cgi

# Windows (PowerShell)
$env:REQUEST_METHOD="GET"
$env:QUERY_STRING="name=Alice&age=30"
.\app.exe
```

## Bonnes pratiques

### Sécurité

1. **Toujours valider les entrées**
```pascal
function SanitizeInput(const Input: String): String;  
begin
  Result := StringReplace(Input, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
end;
```

2. **Limiter la taille des données POST**
```pascal
const
  MAX_POST_SIZE = 10 * 1024 * 1024; // 10 MB

function ReadPOSTData: String;  
var
  ContentLength: Integer;
begin
  ContentLength := StrToIntDef(GetCGIVar('CONTENT_LENGTH'), 0);

  if ContentLength > MAX_POST_SIZE then
    raise Exception.Create('POST data too large');

  // ... lecture
end;
```

3. **Headers de sécurité**
```pascal
procedure SetSecurityHeaders(AResponse: TResponse);  
begin
  AResponse.SetCustomHeader('X-Content-Type-Options', 'nosniff');
  AResponse.SetCustomHeader('X-Frame-Options', 'DENY');
  AResponse.SetCustomHeader('X-XSS-Protection', '1; mode=block');
end;
```

### Performance

1. **Mettre en cache les résultats coûteux**
2. **Réutiliser les connexions BDD** (FastCGI uniquement)
3. **Limiter les allocations mémoire**
4. **Compresser les réponses volumineuses**

### Organisation du code

```
projet/
├── src/
│   ├── handlers/       (gestionnaires de routes)
│   ├── models/         (logique métier)
│   └── utils/          (fonctions utilitaires)
├── webapp.lpr          (programme principal)
└── config.inc          (configuration)
```

## Conclusion

CGI et FastCGI sont deux approches complémentaires :

- **CGI** : Simple, parfait pour débuter et petites applications
- **FastCGI** : Performant, professionnel, pour la production

FreePascal excelle dans les deux modes grâce à :
- Compilation native ultra-rapide
- Faible consommation mémoire
- Code identique pour les deux modes
- Support multi-plateforme transparent

Dans les sections suivantes, nous verrons comment configurer ces applications sur différents serveurs web :
- IIS (Windows)
- Apache (Windows/Ubuntu)
- Nginx (Ubuntu)

Prêt à déployer votre application en production ? Continuons !

⏭️ [Configuration IIS (Windows)](/09-programmation-web-freepascal/02.1-configuration-iis-windows.md)
