🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.6 Sessions et authentification

## Introduction

Les sessions et l'authentification sont des mécanismes essentiels pour créer des applications web sécurisées et interactives. Une session permet de maintenir l'état d'un utilisateur entre plusieurs requêtes HTTP, tandis que l'authentification vérifie l'identité de l'utilisateur. Ce chapitre explore ces concepts fondamentaux avec FreePascal et fpWeb.

## 9.6.1 Comprendre les sessions HTTP

### Le problème : HTTP est sans état

Le protocole HTTP est "stateless" (sans état), c'est-à-dire que chaque requête est indépendante. Le serveur ne "se souvient" pas des requêtes précédentes. Cela pose problème quand on veut :

- Savoir si un utilisateur est connecté
- Maintenir un panier d'achat
- Conserver des préférences utilisateur
- Suivre une progression dans un formulaire multi-étapes

### La solution : les sessions

Une **session** est un mécanisme qui permet d'associer des données à un utilisateur particulier pendant toute la durée de sa visite. Voici comment cela fonctionne :

1. L'utilisateur fait sa première requête
2. Le serveur crée une session unique avec un identifiant (Session ID)
3. Le Session ID est envoyé au navigateur (généralement via un cookie)
4. Le navigateur renvoie ce Session ID à chaque requête suivante
5. Le serveur utilise ce Session ID pour retrouver les données de session

```
┌─────────────┐                    ┌─────────────┐
│  Navigateur │                    │   Serveur   │
└──────┬──────┘                    └──────┬──────┘
       │                                  │
       │ 1. Première requête              │
       │─────────────────────────────────>│
       │                                  │
       │                      2. Crée session (ID: abc123)
       │                                  │
       │ 3. Réponse + Cookie (ID: abc123) │
       │<─────────────────────────────────│
       │                                  │
       │ 4. Requête + Cookie (ID: abc123) │
       │─────────────────────────────────>│
       │                                  │
       │          5. Retrouve les données de la session
       │                                  │
```

## 9.6.2 Gestion des sessions avec fpWeb

### Configuration de base des sessions

fpWeb fournit un système de gestion de sessions intégré via `TFPWebSession` :

```pascal
uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, fpSessIni;

type
  TMyWebModule = class(TFPWebModule)
  private
    procedure InitSession;
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

procedure TMyWebModule.InitSession;  
begin
  // Active les sessions
  CreateSession := True;

  // Durée de vie d'une session en minutes (par défaut : 15 minutes)
  SessionTimeout := 30;
end;

procedure TMyWebModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);  
var
  SessionObj: TCustomSession;
begin
  InitSession;

  // Obtenir ou créer la session
  SessionObj := Session;

  if Assigned(SessionObj) then
  begin
    // La session existe, on peut l'utiliser
    AResponse.Content := '<h1>Session ID : ' + SessionObj.SessionID + '</h1>';
  end
  else
  begin
    // Problème avec la session
    AResponse.Content := '<h1>Erreur : impossible de créer la session</h1>';
  end;

  AResponse.SendResponse;
end;
```

### Stockage de données dans une session

On peut stocker des valeurs dans la session sous forme de paires clé-valeur :

```pascal
procedure TMyWebModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);  
var
  SessionObj: TCustomSession;
  VisitCount: Integer;
begin
  InitSession;
  SessionObj := Session;

  // Lire une valeur de session (avec valeur par défaut si inexistante)
  if SessionObj.Variables['visit_count'] = '' then
    VisitCount := 0
  else
    VisitCount := StrToInt(SessionObj.Variables['visit_count']);

  // Incrémenter le compteur
  Inc(VisitCount);

  // Sauvegarder dans la session
  SessionObj.Variables['visit_count'] := IntToStr(VisitCount);

  // Afficher
  AResponse.Content := Format('<h1>Vous avez visité cette page %d fois</h1>',
                              [VisitCount]);
  AResponse.SendResponse;
end;
```

## 9.6.3 Types de stockage de sessions

FreePascal propose plusieurs méthodes pour stocker les données de session :

### Sessions en mémoire (par défaut)

```pascal
uses
  fpSessMem;

// Les sessions sont stockées en mémoire
// Avantages : très rapide
// Inconvénients : perdues au redémarrage du serveur
```

### Sessions dans des fichiers INI

```pascal
uses
  fpSessIni;

// Les sessions sont stockées dans des fichiers .ini
// Avantages : persistantes, simples
// Inconvénients : performances limitées avec beaucoup d'utilisateurs

procedure TMyWebModule.InitSession;  
begin
  CreateSession := True;

  // Spécifier le répertoire de stockage
  {$IFDEF WINDOWS}
  SessionDir := 'C:\sessions\';
  {$ELSE}
  SessionDir := '/var/lib/sessions/';
  {$ENDIF}
end;
```

### Sessions en base de données

```pascal
uses
  fpSessDB, sqldb;

// Les sessions sont stockées dans une base de données
// Avantages : scalable, persistantes, partagées entre serveurs
// Inconvénients : plus complexe à configurer

type
  TMyWebModule = class(TFPWebModule)
  private
    FConnection: TSQLConnection;
    procedure InitDatabase;
  end;

procedure TMyWebModule.InitDatabase;  
begin
  FConnection := TSQLConnection.Create(nil);
  // Configuration de la connexion...

  // Configurer le stockage de sessions en BDD
  SessionFactory := TIniSessionFactory.Create;
  TIniSessionFactory(SessionFactory).Connection := FConnection;
end;
```

**Structure de table pour les sessions :**

```sql
CREATE TABLE sessions (
    session_id VARCHAR(64) PRIMARY KEY,
    session_data TEXT,
    last_access TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

## 9.6.4 Cookies et Session ID

### Comprendre les cookies

Un cookie est une petite donnée stockée par le navigateur et renvoyée au serveur à chaque requête. Par défaut, fpWeb utilise un cookie nommé `FPWEBSID` pour stocker le Session ID.

```pascal
procedure TMyWebModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);  
var
  CookieValue: string;
begin
  // Lire un cookie
  CookieValue := ARequest.CookieFields.Values['FPWEBSID'];

  // Créer/modifier un cookie personnalisé
  AResponse.SetCookie('user_preference', 'dark_mode');

  // Cookie avec options avancées
  with AResponse.Cookies.Add do
  begin
    Name := 'remember_me';
    Value := 'yes';
    Path := '/';
    MaxAge := 86400 * 30; // 30 jours en secondes
    HttpOnly := True;     // Protection XSS
    Secure := True;       // HTTPS uniquement
    SameSite := 'Strict'; // Protection CSRF
  end;

  AResponse.SendResponse;
end;
```

### Sécurité des cookies de session

**Options importantes pour les cookies de session :**

```pascal
procedure ConfigureSessionCookie(AResponse: TResponse);  
begin
  with AResponse.Cookies.FindCookie('FPWEBSID') do
  begin
    // HttpOnly : empêche JavaScript d'accéder au cookie (protection XSS)
    HttpOnly := True;

    // Secure : cookie transmis uniquement via HTTPS
    Secure := True;

    // SameSite : protection contre les attaques CSRF
    // 'Strict' : cookie jamais envoyé depuis un autre site
    // 'Lax' : cookie envoyé pour la navigation normale
    // 'None' : cookie toujours envoyé (nécessite Secure=True)
    SameSite := 'Strict';

    // Path : limite le cookie à un chemin spécifique
    Path := '/';

    // MaxAge : durée de vie en secondes (0 = session navigateur)
    MaxAge := 0;
  end;
end;
```

## 9.6.5 Authentification basique

### Formulaire de connexion

**Template HTML** (`login.html`) :

```html
<!DOCTYPE html>
<html>
<head>
    <title>Connexion</title>
    <style>
        .login-form {
            max-width: 400px;
            margin: 50px auto;
            padding: 20px;
            border: 1px solid #ccc;
            border-radius: 5px;
        }
        .error { color: red; }
        input { width: 100%; padding: 10px; margin: 10px 0; }
        button { width: 100%; padding: 10px; background: #007bff; color: white; border: none; }
    </style>
</head>
<body>
    <div class="login-form">
        <h2>Connexion</h2>
        {{#error}}
        <p class="error">{{error_message}}</p>
        {{/error}}
        <form method="POST" action="/login">
            <input type="text" name="username" placeholder="Nom d'utilisateur" required>
            <input type="password" name="password" placeholder="Mot de passe" required>
            <button type="submit">Se connecter</button>
        </form>
    </div>
</body>
</html>
```

### Gestionnaire de connexion

```pascal
uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, fpSessIni, md5;

type
  TLoginModule = class(TFPWebModule)
  private
    function VerifyCredentials(const Username, Password: string): Boolean;
    function HashPassword(const Password: string): string;
  public
    procedure HandleLoginPage(ARequest: TRequest; AResponse: TResponse);
    procedure HandleLoginPost(ARequest: TRequest; AResponse: TResponse);
    procedure HandleLogout(ARequest: TRequest; AResponse: TResponse);
  end;

implementation

function TLoginModule.HashPassword(const Password: string): string;  
begin
  // Utilise MD5 pour hasher le mot de passe
  // ATTENTION : MD5 est obsolète, utiliser bcrypt ou Argon2 en production !
  Result := MD5Print(MD5String(Password));
end;

function TLoginModule.VerifyCredentials(const Username, Password: string): Boolean;  
var
  HashedPassword: string;
begin
  // Dans un vrai système, vérifier contre une base de données
  HashedPassword := HashPassword(Password);

  // Exemple simple (À NE PAS utiliser en production !)
  Result := (Username = 'admin') and (HashedPassword = HashPassword('secret123'));
end;

procedure TLoginModule.HandleLoginPage(ARequest: TRequest; AResponse: TResponse);  
var
  HTML: string;
begin
  // Charger le template de connexion
  HTML := LoadFileToString('templates/login.html');

  // Afficher sans message d'erreur
  HTML := StringReplace(HTML, '{{#error}}', '<!--', []);
  HTML := StringReplace(HTML, '{{/error}}', '-->', []);

  AResponse.Content := HTML;
  AResponse.SendResponse;
end;

procedure TLoginModule.HandleLoginPost(ARequest: TRequest; AResponse: TResponse);  
var
  Username, Password: string;
  SessionObj: TCustomSession;
  HTML: string;
begin
  // Récupérer les données du formulaire
  Username := ARequest.ContentFields.Values['username'];
  Password := ARequest.ContentFields.Values['password'];

  // Vérifier les identifiants
  if VerifyCredentials(Username, Password) then
  begin
    // Créer une session
    CreateSession := True;
    SessionObj := Session;

    // Stocker les informations utilisateur
    SessionObj.Variables['authenticated'] := 'true';
    SessionObj.Variables['username'] := Username;
    SessionObj.Variables['login_time'] := DateTimeToStr(Now);

    // Rediriger vers la page d'accueil
    AResponse.Code := 302;
    AResponse.SetCustomHeader('Location', '/dashboard');
    AResponse.SendResponse;
  end
  else
  begin
    // Identifiants incorrects
    HTML := LoadFileToString('templates/login.html');
    HTML := StringReplace(HTML, '{{error_message}}',
                         'Nom d''utilisateur ou mot de passe incorrect', []);

    AResponse.Content := HTML;
    AResponse.SendResponse;
  end;
end;

procedure TLoginModule.HandleLogout(ARequest: TRequest; AResponse: TResponse);  
var
  SessionObj: TCustomSession;
begin
  SessionObj := Session;

  if Assigned(SessionObj) then
  begin
    // Détruire la session
    SessionObj.Terminate;
  end;

  // Rediriger vers la page de connexion
  AResponse.Code := 302;
  AResponse.SetCustomHeader('Location', '/login');
  AResponse.SendResponse;
end;
```

## 9.6.6 Middleware d'authentification

### Vérification de l'authentification

Créer une fonction de vérification pour protéger les pages :

```pascal
function IsAuthenticated(ARequest: TRequest): Boolean;  
var
  SessionObj: TCustomSession;
begin
  Result := False;

  // Obtenir la session
  SessionObj := ARequest.Session;

  if Assigned(SessionObj) then
  begin
    // Vérifier si l'utilisateur est authentifié
    Result := SessionObj.Variables['authenticated'] = 'true';
  end;
end;

procedure HandleProtectedPage(ARequest: TRequest; AResponse: TResponse);  
begin
  if not IsAuthenticated(ARequest) then
  begin
    // Non authentifié : rediriger vers login
    AResponse.Code := 302;
    AResponse.SetCustomHeader('Location', '/login');
    AResponse.SendResponse;
    Exit;
  end;

  // Utilisateur authentifié : afficher la page
  AResponse.Content := '<h1>Page protégée</h1>' +
                      '<p>Bienvenue, ' +
                      ARequest.Session.Variables['username'] +
                      ' !</p>';
  AResponse.SendResponse;
end;
```

### Classe d'authentification réutilisable

```pascal
type
  TAuthManager = class
  private
    FSessionTimeout: Integer;
  public
    constructor Create;
    function Login(ARequest: TRequest; const Username, Password: string): Boolean;
    procedure Logout(ARequest: TRequest);
    function IsAuthenticated(ARequest: TRequest): Boolean;
    function GetCurrentUser(ARequest: TRequest): string;
    function RequireAuth(ARequest: TRequest; AResponse: TResponse): Boolean;
    property SessionTimeout: Integer read FSessionTimeout write FSessionTimeout;
  end;

constructor TAuthManager.Create;  
begin
  inherited Create;
  FSessionTimeout := 30; // 30 minutes par défaut
end;

function TAuthManager.Login(ARequest: TRequest; const Username, Password: string): Boolean;  
var
  SessionObj: TCustomSession;
begin
  Result := False;

  // Vérifier les identifiants (à implémenter selon vos besoins)
  if VerifyUserCredentials(Username, Password) then
  begin
    SessionObj := ARequest.Session;
    if Assigned(SessionObj) then
    begin
      SessionObj.Variables['authenticated'] := 'true';
      SessionObj.Variables['username'] := Username;
      SessionObj.Variables['login_time'] := DateTimeToStr(Now);
      Result := True;
    end;
  end;
end;

procedure TAuthManager.Logout(ARequest: TRequest);  
var
  SessionObj: TCustomSession;
begin
  SessionObj := ARequest.Session;
  if Assigned(SessionObj) then
    SessionObj.Terminate;
end;

function TAuthManager.IsAuthenticated(ARequest: TRequest): Boolean;  
var
  SessionObj: TCustomSession;
begin
  Result := False;
  SessionObj := ARequest.Session;

  if Assigned(SessionObj) then
    Result := SessionObj.Variables['authenticated'] = 'true';
end;

function TAuthManager.GetCurrentUser(ARequest: TRequest): string;  
var
  SessionObj: TCustomSession;
begin
  Result := '';
  SessionObj := ARequest.Session;

  if Assigned(SessionObj) and IsAuthenticated(ARequest) then
    Result := SessionObj.Variables['username'];
end;

function TAuthManager.RequireAuth(ARequest: TRequest; AResponse: TResponse): Boolean;  
begin
  Result := IsAuthenticated(ARequest);

  if not Result then
  begin
    // Rediriger vers la page de connexion
    AResponse.Code := 302;
    AResponse.SetCustomHeader('Location', '/login?redirect=' +
                             ARequest.PathInfo);
    AResponse.SendResponse;
  end;
end;
```

## 9.6.7 Hachage sécurisé des mots de passe

### Pourquoi ne jamais stocker les mots de passe en clair

**Mauvaise pratique :** Stocker `password = "secret123"` dans la base de données

**Bonne pratique :** Stocker un hash : `password_hash = "bcrypt_hash_here"`

### Utilisation de bcrypt (recommandé)

```pascal
uses
  BCrypt; // Package fpbcrypt

type
  TPasswordManager = class
  public
    class function HashPassword(const Password: string): string;
    class function VerifyPassword(const Password, Hash: string): Boolean;
  end;

class function TPasswordManager.HashPassword(const Password: string): string;  
begin
  // Générer un hash bcrypt avec un coût de 12
  Result := BCryptHash(Password, BCryptGenerateSalt(12));
end;

class function TPasswordManager.VerifyPassword(const Password, Hash: string): Boolean;  
begin
  // Vérifier si le mot de passe correspond au hash
  Result := BCryptVerify(Password, Hash);
end;

// Utilisation lors de l'inscription
procedure RegisterUser(const Username, Password: string);  
var
  HashedPassword: string;
begin
  HashedPassword := TPasswordManager.HashPassword(Password);

  // Sauvegarder dans la base de données
  // INSERT INTO users (username, password_hash) VALUES (?, ?)
  SaveUserToDatabase(Username, HashedPassword);
end;

// Utilisation lors de la connexion
function AuthenticateUser(const Username, Password: string): Boolean;  
var
  StoredHash: string;
begin
  // Récupérer le hash depuis la base de données
  StoredHash := GetUserPasswordHash(Username);

  // Vérifier le mot de passe
  Result := TPasswordManager.VerifyPassword(Password, StoredHash);
end;
```

### Alternative : PBKDF2

```pascal
uses
  DCPsha256, DCPcrypt2;

function PBKDF2_SHA256(const Password, Salt: string; Iterations: Integer): string;  
var
  Hash: TDCP_sha256;
  Key: array[0..31] of Byte;
  i: Integer;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    // Dériver la clé avec PBKDF2
    Hash.Init;
    // Implémentation simplifiée - utilisez une bibliothèque PBKDF2 complète
    for i := 1 to Iterations do
    begin
      Hash.UpdateStr(Password + Salt + IntToStr(i));
    end;
    Hash.Final(Key);

    // Convertir en hexadécimal
    Result := '';
    for i := 0 to 31 do
      Result := Result + IntToHex(Key[i], 2);
  finally
    Hash.Free;
  end;
end;
```

## 9.6.8 Gestion des rôles et permissions

### Système de rôles simple

```pascal
type
  TUserRole = (urGuest, urUser, urModerator, urAdmin);

  TUser = class
  private
    FUsername: string;
    FRole: TUserRole;
  public
    property Username: string read FUsername write FUsername;
    property Role: TUserRole read FRole write FRole;
    function HasRole(MinRole: TUserRole): Boolean;
  end;

function TUser.HasRole(MinRole: TUserRole): Boolean;  
begin
  Result := FRole >= MinRole;
end;

// Stockage dans la session
procedure StoreUserInSession(ARequest: TRequest; User: TUser);  
var
  SessionObj: TCustomSession;
begin
  SessionObj := ARequest.Session;
  SessionObj.Variables['authenticated'] := 'true';
  SessionObj.Variables['username'] := User.Username;
  SessionObj.Variables['role'] := IntToStr(Ord(User.Role));
end;

// Récupération depuis la session
function GetUserFromSession(ARequest: TRequest): TUser;  
var
  SessionObj: TCustomSession;
begin
  Result := nil;
  SessionObj := ARequest.Session;

  if Assigned(SessionObj) and (SessionObj.Variables['authenticated'] = 'true') then
  begin
    Result := TUser.Create;
    Result.Username := SessionObj.Variables['username'];
    Result.Role := TUserRole(StrToInt(SessionObj.Variables['role']));
  end;
end;

// Vérification des permissions
function RequireRole(ARequest: TRequest; AResponse: TResponse;
                    MinRole: TUserRole): Boolean;
var
  User: TUser;
begin
  Result := False;
  User := GetUserFromSession(ARequest);

  if not Assigned(User) then
  begin
    // Non connecté
    AResponse.Code := 302;
    AResponse.SetCustomHeader('Location', '/login');
    AResponse.SendResponse;
    Exit;
  end;

  if not User.HasRole(MinRole) then
  begin
    // Permissions insuffisantes
    AResponse.Code := 403;
    AResponse.Content := '<h1>403 Forbidden</h1><p>Accès refusé</p>';
    AResponse.SendResponse;
    User.Free;
    Exit;
  end;

  Result := True;
  User.Free;
end;

// Exemple d'utilisation
procedure HandleAdminPage(ARequest: TRequest; AResponse: TResponse);  
begin
  if not RequireRole(ARequest, AResponse, urAdmin) then
    Exit;

  // L'utilisateur a les droits admin
  AResponse.Content := '<h1>Panel Administration</h1>';
  AResponse.SendResponse;
end;
```

## 9.6.9 Protection CSRF (Cross-Site Request Forgery)

### Qu'est-ce qu'une attaque CSRF ?

Une attaque CSRF se produit quand un site malveillant fait exécuter une action non désirée sur un site où l'utilisateur est authentifié.

**Exemple :** Un utilisateur connecté sur `monsite.com` visite `sitemalveillant.com` qui contient :

```html
<form action="https://monsite.com/delete-account" method="POST">
    <input type="submit" value="Cliquez ici !">
</form>
```

### Protection avec des tokens CSRF

```pascal
uses
  SysUtils, md5;

function GenerateCSRFToken(SessionID: string): string;  
var
  RandomData: string;
begin
  // Générer un token unique basé sur la session et des données aléatoires
  RandomData := SessionID + FloatToStr(Now) + IntToStr(Random(MaxInt));
  Result := MD5Print(MD5String(RandomData));
end;

procedure AddCSRFToken(ARequest: TRequest);  
var
  SessionObj: TCustomSession;
  Token: string;
begin
  SessionObj := ARequest.Session;

  // Générer et stocker le token si inexistant
  if SessionObj.Variables['csrf_token'] = '' then
  begin
    Token := GenerateCSRFToken(SessionObj.SessionID);
    SessionObj.Variables['csrf_token'] := Token;
  end;
end;

function VerifyCSRFToken(ARequest: TRequest; const SubmittedToken: string): Boolean;  
var
  SessionObj: TCustomSession;
  StoredToken: string;
begin
  Result := False;
  SessionObj := ARequest.Session;

  if Assigned(SessionObj) then
  begin
    StoredToken := SessionObj.Variables['csrf_token'];
    Result := (StoredToken <> '') and (StoredToken = SubmittedToken);
  end;
end;

// Utilisation dans un formulaire
procedure HandleFormPage(ARequest: TRequest; AResponse: TResponse);  
var
  SessionObj: TCustomSession;
  CSRFToken: string;
begin
  AddCSRFToken(ARequest);
  SessionObj := ARequest.Session;
  CSRFToken := SessionObj.Variables['csrf_token'];

  AResponse.Content :=
    '<form method="POST" action="/submit">' +
    '  <input type="hidden" name="csrf_token" value="' + CSRFToken + '">' +
    '  <input type="text" name="data" placeholder="Entrez des données">' +
    '  <button type="submit">Envoyer</button>' +
    '</form>';
  AResponse.SendResponse;
end;

// Vérification lors de la soumission
procedure HandleFormSubmit(ARequest: TRequest; AResponse: TResponse);  
var
  SubmittedToken: string;
begin
  SubmittedToken := ARequest.ContentFields.Values['csrf_token'];

  if not VerifyCSRFToken(ARequest, SubmittedToken) then
  begin
    AResponse.Code := 403;
    AResponse.Content := '<h1>Erreur : Token CSRF invalide</h1>';
    AResponse.SendResponse;
    Exit;
  end;

  // Token valide, traiter le formulaire
  // ...
end;
```

## 9.6.10 Remember Me (Se souvenir de moi)

### Implémentation sécurisée

```pascal
type
  TRememberMeToken = record
    UserID: Integer;
    Selector: string;
    Validator: string;
    Expiry: TDateTime;
  end;

function GenerateRememberMeToken(UserID: Integer): TRememberMeToken;  
begin
  Result.UserID := UserID;
  Result.Selector := GenerateRandomString(16);
  Result.Validator := GenerateRandomString(32);
  Result.Expiry := Now + 30; // 30 jours

  // Stocker en base : (selector, hash(validator), user_id, expiry)
  SaveRememberMeToken(Result.Selector,
                     HashPassword(Result.Validator),
                     Result.UserID,
                     Result.Expiry);
end;

procedure SetRememberMeCookie(AResponse: TResponse; Token: TRememberMeToken);  
var
  CookieValue: string;
begin
  // Format : selector:validator
  CookieValue := Token.Selector + ':' + Token.Validator;

  with AResponse.Cookies.Add do
  begin
    Name := 'remember_me';
    Value := CookieValue;
    Path := '/';
    MaxAge := 86400 * 30; // 30 jours
    HttpOnly := True;
    Secure := True;
    SameSite := 'Strict';
  end;
end;

function ValidateRememberMeToken(const CookieValue: string): Integer;  
var
  Parts: TStringArray;
  Selector, Validator: string;
  StoredHash: string;
  UserID: Integer;
  Expiry: TDateTime;
begin
  Result := -1; // Invalide par défaut

  Parts := SplitString(CookieValue, ':');
  if Length(Parts) <> 2 then
    Exit;

  Selector := Parts[0];
  Validator := Parts[1];

  // Récupérer depuis la base de données
  if not GetRememberMeToken(Selector, StoredHash, UserID, Expiry) then
    Exit;

  // Vérifier l'expiration
  if Now > Expiry then
  begin
    DeleteRememberMeToken(Selector);
    Exit;
  end;

  // Vérifier le validator
  if VerifyPassword(Validator, StoredHash) then
    Result := UserID;
end;
```

## 9.6.11 Différences multi-plateformes

### Chemins de stockage des sessions

```pascal
function GetSessionStoragePath: string;  
begin
  {$IFDEF WINDOWS}
  Result := 'C:\ProgramData\MyApp\sessions\';
  {$ELSE}
  Result := '/var/lib/myapp/sessions/';
  {$ENDIF}

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;
```

### Configuration SSL/TLS

Pour activer HTTPS (recommandé pour l'authentification) :

**Windows (IIS) :**
- Utiliser le gestionnaire IIS pour configurer les certificats SSL
- Binding HTTPS sur le port 443

**Ubuntu (Apache/Nginx) :**

```bash
# Avec Let's Encrypt
sudo apt-get install certbot python3-certbot-apache  
sudo certbot --apache -d mondomaine.com

# Ou avec Nginx
sudo certbot --nginx -d mondomaine.com
```

### Permissions des fichiers de session

**Linux :**

```bash
# Créer le répertoire avec les bonnes permissions
sudo mkdir -p /var/lib/myapp/sessions  
sudo chown www-data:www-data /var/lib/myapp/sessions  
sudo chmod 700 /var/lib/myapp/sessions
```

**Windows :**
- Configurer les permissions NTFS via l'explorateur
- Accorder les droits au compte de service IIS

**Code FreePascal pour vérifier les permissions :**

```pascal
{$IFDEF UNIX}
uses
  BaseUnix;

function CheckSessionDirectoryPermissions(const DirPath: string): Boolean;  
var
  StatInfo: TStat;
begin
  Result := False;

  if FpStat(DirPath, StatInfo) = 0 then
  begin
    // Vérifier que seul le propriétaire a les droits (700)
    Result := (StatInfo.st_mode and S_IRWXO = 0) and
              (StatInfo.st_mode and S_IRWXG = 0);
  end;
end;
{$ENDIF}

procedure InitializeSessionDirectory;  
var
  SessionPath: string;
begin
  SessionPath := GetSessionStoragePath;

  {$IFDEF UNIX}
  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(SessionPath) then
    ForceDirectories(SessionPath);

  // Définir les permissions restrictives
  FpChmod(SessionPath, S_IRWXU); // 700 - lecture/écriture/exécution pour propriétaire uniquement

  if not CheckSessionDirectoryPermissions(SessionPath) then
    WriteLn('AVERTISSEMENT : Les permissions du répertoire de sessions ne sont pas sécurisées !');
  {$ENDIF}

  {$IFDEF WINDOWS}
  // Sous Windows, créer simplement le répertoire
  // Les permissions NTFS doivent être configurées manuellement
  if not DirectoryExists(SessionPath) then
    ForceDirectories(SessionPath);
  {$ENDIF}
end;
```

## 9.6.12 OAuth 2.0 et authentification externe

### Introduction à OAuth 2.0

OAuth 2.0 permet aux utilisateurs de se connecter avec des comptes externes (Google, Facebook, GitHub, etc.) sans partager leurs mots de passe avec votre application.

**Flux d'authentification OAuth 2.0 :**

```
┌──────────┐                                      ┌───────────────┐
│          │                                      │  Fournisseur  │
│  Client  │                                      │     OAuth     │
│(Navigat.)│                                      │ (ex: Google)  │
└────┬─────┘                                      └───────┬───────┘
     │                                                    │
     │ 1. Clic "Se connecter avec Google"                 │
     │───────────────────────────────────>                │
     │                                                    │
     │ 2. Redirection vers la page de connexion Google    │
     │<───────────────────────────────────────────────────│
     │                                                    │
     │ 3. L'utilisateur s'authentifie sur Google          │
     │───────────────────────────────────>                │
     │                                                    │
     │ 4. Google redirige vers votre application          │
     │    avec un code d'autorisation                     │
     │<───────────────────────────────────────────────────│
     │                                                    │
┌────▼─────┐                                      ┌───────▼───────┐
│          │ 5. Échange du code contre un token   │               │
│  Serveur │───────────────────────────────────>  │  Fournisseur  │
│   Web    │                                      │     OAuth     │
│          │ 6. Token d'accès                     │               │
│          │<───────────────────────────────────  │               │
└──────────┘                                      └───────────────┘
```

### Implémentation OAuth 2.0 avec Google

```pascal
uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, fpjson, fphttpclient;

type
  TOAuthConfig = record
    ClientID: string;
    ClientSecret: string;
    RedirectURI: string;
    AuthURL: string;
    TokenURL: string;
    UserInfoURL: string;
  end;

  TOAuthManager = class
  private
    FConfig: TOAuthConfig;
    function ExchangeCodeForToken(const Code: string): string;
    function GetUserInfo(const AccessToken: string): TJSONObject;
  public
    constructor Create(const Config: TOAuthConfig);
    function GetAuthorizationURL(const State: string): string;
    function HandleCallback(const Code, State: string): TJSONObject;
  end;

constructor TOAuthManager.Create(const Config: TOAuthConfig);  
begin
  inherited Create;
  FConfig := Config;
end;

function TOAuthManager.GetAuthorizationURL(const State: string): string;  
begin
  // Construire l'URL d'autorisation
  Result := FConfig.AuthURL +
            '?client_id=' + FConfig.ClientID +
            '&redirect_uri=' + HTTPEncode(FConfig.RedirectURI) +
            '&response_type=code' +
            '&scope=' + HTTPEncode('openid email profile') +
            '&state=' + State;
end;

function TOAuthManager.ExchangeCodeForToken(const Code: string): string;  
var
  HTTPClient: TFPHTTPClient;
  Response: string;
  JSONData: TJSONObject;
  PostData: string;
begin
  Result := '';
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    // Préparer les données POST
    PostData := 'code=' + HTTPEncode(Code) +
                '&client_id=' + HTTPEncode(FConfig.ClientID) +
                '&client_secret=' + HTTPEncode(FConfig.ClientSecret) +
                '&redirect_uri=' + HTTPEncode(FConfig.RedirectURI) +
                '&grant_type=authorization_code';

    // Envoyer la requête
    HTTPClient.RequestBody := TStringStream.Create(PostData);
    HTTPClient.AddHeader('Content-Type', 'application/x-www-form-urlencoded');

    Response := HTTPClient.Post(FConfig.TokenURL);

    // Parser la réponse JSON
    JSONData := GetJSON(Response) as TJSONObject;
    try
      Result := JSONData.Get('access_token', '');
    finally
      JSONData.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;

function TOAuthManager.GetUserInfo(const AccessToken: string): TJSONObject;  
var
  HTTPClient: TFPHTTPClient;
  Response: string;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.AddHeader('Authorization', 'Bearer ' + AccessToken);
    Response := HTTPClient.Get(FConfig.UserInfoURL);
    Result := GetJSON(Response) as TJSONObject;
  finally
    HTTPClient.Free;
  end;
end;

function TOAuthManager.HandleCallback(const Code, State: string): TJSONObject;  
var
  AccessToken: string;
begin
  Result := nil;

  // Échanger le code contre un token
  AccessToken := ExchangeCodeForToken(Code);

  if AccessToken <> '' then
  begin
    // Récupérer les informations utilisateur
    Result := GetUserInfo(AccessToken);
  end;
end;

// Configuration pour Google
function CreateGoogleOAuthConfig: TOAuthConfig;  
begin
  Result.ClientID := 'VOTRE_CLIENT_ID.apps.googleusercontent.com';
  Result.ClientSecret := 'VOTRE_CLIENT_SECRET';
  Result.RedirectURI := 'https://votresite.com/oauth/callback';
  Result.AuthURL := 'https://accounts.google.com/o/oauth2/v2/auth';
  Result.TokenURL := 'https://oauth2.googleapis.com/token';
  Result.UserInfoURL := 'https://www.googleapis.com/oauth2/v2/userinfo';
end;

// Gestionnaire de connexion OAuth
procedure HandleOAuthLogin(ARequest: TRequest; AResponse: TResponse);  
var
  OAuthManager: TOAuthManager;
  State: string;
  AuthURL: string;
begin
  OAuthManager := TOAuthManager.Create(CreateGoogleOAuthConfig);
  try
    // Générer un état aléatoire pour la sécurité (protection CSRF)
    State := GenerateRandomString(32);

    // Stocker l'état dans la session
    ARequest.Session.Variables['oauth_state'] := State;

    // Obtenir l'URL d'autorisation
    AuthURL := OAuthManager.GetAuthorizationURL(State);

    // Rediriger vers Google
    AResponse.Code := 302;
    AResponse.SetCustomHeader('Location', AuthURL);
    AResponse.SendResponse;
  finally
    OAuthManager.Free;
  end;
end;

// Gestionnaire du callback OAuth
procedure HandleOAuthCallback(ARequest: TRequest; AResponse: TResponse);  
var
  OAuthManager: TOAuthManager;
  Code, State, StoredState: string;
  UserInfo: TJSONObject;
  Email, Name: string;
begin
  OAuthManager := TOAuthManager.Create(CreateGoogleOAuthConfig);
  try
    // Récupérer le code et l'état
    Code := ARequest.QueryFields.Values['code'];
    State := ARequest.QueryFields.Values['state'];
    StoredState := ARequest.Session.Variables['oauth_state'];

    // Vérifier l'état (protection CSRF)
    if State <> StoredState then
    begin
      AResponse.Code := 403;
      AResponse.Content := '<h1>Erreur : État OAuth invalide</h1>';
      AResponse.SendResponse;
      Exit;
    end;

    // Traiter le callback
    UserInfo := OAuthManager.HandleCallback(Code, State);
    try
      if Assigned(UserInfo) then
      begin
        // Extraire les informations utilisateur
        Email := UserInfo.Get('email', '');
        Name := UserInfo.Get('name', '');

        // Créer ou mettre à jour l'utilisateur dans la base de données
        // CreateOrUpdateUser(Email, Name);

        // Connecter l'utilisateur
        ARequest.Session.Variables['authenticated'] := 'true';
        ARequest.Session.Variables['email'] := Email;
        ARequest.Session.Variables['name'] := Name;
        ARequest.Session.Variables['oauth_provider'] := 'google';

        // Rediriger vers le tableau de bord
        AResponse.Code := 302;
        AResponse.SetCustomHeader('Location', '/dashboard');
        AResponse.SendResponse;
      end
      else
      begin
        AResponse.Content := '<h1>Erreur lors de l''authentification OAuth</h1>';
        AResponse.SendResponse;
      end;
    finally
      if Assigned(UserInfo) then
        UserInfo.Free;
    end;
  finally
    OAuthManager.Free;
  end;
end;
```

## 9.6.13 JSON Web Tokens (JWT)

### Qu'est-ce qu'un JWT ?

Un JWT (JSON Web Token) est un standard pour créer des tokens d'accès contenant des données JSON. Il est particulièrement utile pour les API REST.

**Structure d'un JWT :**

```
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c
```

Composé de trois parties séparées par des points :
1. **Header** : algorithme et type
2. **Payload** : données (claims)
3. **Signature** : vérification d'intégrité

### Implémentation JWT simple

```pascal
uses
  SysUtils, Classes, fpjson, base64, hmac, sha256;

type
  TJWTManager = class
  private
    FSecretKey: string;
    function Base64URLEncode(const Data: string): string;
    function Base64URLDecode(const Data: string): string;
    function HMACSHA256(const Data, Key: string): string;
  public
    constructor Create(const SecretKey: string);
    function CreateToken(const Payload: TJSONObject): string;
    function VerifyToken(const Token: string): TJSONObject;
    function IsTokenValid(const Token: string): Boolean;
  end;

constructor TJWTManager.Create(const SecretKey: string);  
begin
  inherited Create;
  FSecretKey := SecretKey;
end;

function TJWTManager.Base64URLEncode(const Data: string): string;  
begin
  Result := EncodeStringBase64(Data);
  // Remplacer les caractères pour le format URL-safe
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

function TJWTManager.Base64URLDecode(const Data: string): string;  
var
  Temp: string;
  Padding: Integer;
begin
  Temp := Data;
  // Restaurer les caractères standard
  Temp := StringReplace(Temp, '-', '+', [rfReplaceAll]);
  Temp := StringReplace(Temp, '_', '/', [rfReplaceAll]);

  // Ajouter le padding nécessaire
  Padding := Length(Temp) mod 4;
  if Padding > 0 then
    Temp := Temp + StringOfChar('=', 4 - Padding);

  Result := DecodeStringBase64(Temp);
end;

function TJWTManager.HMACSHA256(const Data, Key: string): string;  
var
  Hash: TSHA256Digest;
  i: Integer;
begin
  // Utiliser une bibliothèque HMAC appropriée
  // Ceci est une simplification - utilisez DCPcrypt ou une autre bibliothèque
  Hash := SHA256String(Key + Data);

  Result := '';
  for i := 0 to High(Hash) do
    Result := Result + IntToHex(Hash[i], 2);
end;

function TJWTManager.CreateToken(const Payload: TJSONObject): string;  
var
  Header: TJSONObject;
  HeaderStr, PayloadStr, Signature: string;
  DataToSign: string;
begin
  // Créer le header
  Header := TJSONObject.Create;
  try
    Header.Add('alg', 'HS256');
    Header.Add('typ', 'JWT');

    // Encoder le header et le payload en Base64URL
    HeaderStr := Base64URLEncode(Header.AsJSON);
    PayloadStr := Base64URLEncode(Payload.AsJSON);

    // Créer la signature
    DataToSign := HeaderStr + '.' + PayloadStr;
    Signature := Base64URLEncode(HMACSHA256(DataToSign, FSecretKey));

    // Assembler le token
    Result := HeaderStr + '.' + PayloadStr + '.' + Signature;
  finally
    Header.Free;
  end;
end;

function TJWTManager.VerifyToken(const Token: string): TJSONObject;  
var
  HeaderStr, PayloadStr, SignatureStr: string;
  DataToVerify, ExpectedSignature: string;
  PayloadJSON: string;
  DotPos, DotPos2: Integer;
begin
  Result := nil;

  // Séparer les parties du token (Header.Payload.Signature)
  DotPos := Pos('.', Token);
  if DotPos = 0 then
    Exit;
  HeaderStr := Copy(Token, 1, DotPos - 1);

  DotPos2 := Pos('.', Token, DotPos + 1);
  if DotPos2 = 0 then
    Exit;
  PayloadStr := Copy(Token, DotPos + 1, DotPos2 - DotPos - 1);
  SignatureStr := Copy(Token, DotPos2 + 1, Length(Token) - DotPos2);

  // Vérifier la signature
  DataToVerify := HeaderStr + '.' + PayloadStr;
  ExpectedSignature := Base64URLEncode(HMACSHA256(DataToVerify, FSecretKey));

  if SignatureStr <> ExpectedSignature then
    Exit; // Signature invalide

  // Décoder le payload
  PayloadJSON := Base64URLDecode(PayloadStr);
  Result := GetJSON(PayloadJSON) as TJSONObject;
end;

function TJWTManager.IsTokenValid(const Token: string): Boolean;  
var
  Payload: TJSONObject;
  ExpirationTime: Int64;
begin
  Result := False;
  Payload := VerifyToken(Token);

  if Assigned(Payload) then
  try
    // Vérifier l'expiration si présente
    if Payload.IndexOfName('exp') >= 0 then
    begin
      ExpirationTime := Payload.Get('exp', Int64(0));
      Result := DateTimeToUnix(Now) < ExpirationTime;
    end
    else
      Result := True; // Pas d'expiration définie
  finally
    Payload.Free;
  end;
end;

// Utilisation pour l'authentification API
procedure HandleAPILogin(ARequest: TRequest; AResponse: TResponse);  
var
  JWTManager: TJWTManager;
  Payload: TJSONObject;
  Token: string;
  Username, Password: string;
begin
  Username := ARequest.ContentFields.Values['username'];
  Password := ARequest.ContentFields.Values['password'];

  // Vérifier les identifiants
  if VerifyCredentials(Username, Password) then
  begin
    JWTManager := TJWTManager.Create('votre_secret_key_très_sécurisée');
    try
      // Créer le payload
      Payload := TJSONObject.Create;
      try
        Payload.Add('sub', Username);
        Payload.Add('name', GetUserFullName(Username));
        Payload.Add('role', 'user');
        Payload.Add('iat', DateTimeToUnix(Now)); // Issued at
        Payload.Add('exp', DateTimeToUnix(Now + 1)); // Expire dans 1 jour

        // Créer le token
        Token := JWTManager.CreateToken(Payload);

        // Envoyer le token au client
        AResponse.ContentType := 'application/json';
        AResponse.Content := Format('{"token":"%s"}', [Token]);
        AResponse.SendResponse;
      finally
        Payload.Free;
      end;
    finally
      JWTManager.Free;
    end;
  end
  else
  begin
    AResponse.Code := 401;
    AResponse.Content := '{"error":"Invalid credentials"}';
    AResponse.SendResponse;
  end;
end;

// Middleware pour vérifier le token JWT
function RequireJWT(ARequest: TRequest; AResponse: TResponse): TJSONObject;  
var
  JWTManager: TJWTManager;
  AuthHeader, Token: string;
begin
  Result := nil;

  // Récupérer le header Authorization
  AuthHeader := ARequest.GetCustomHeader('Authorization');

  if (AuthHeader = '') or (Copy(AuthHeader, 1, 7) <> 'Bearer ') then
  begin
    AResponse.Code := 401;
    AResponse.Content := '{"error":"Missing or invalid Authorization header"}';
    AResponse.SendResponse;
    Exit;
  end;

  // Extraire le token
  Token := Copy(AuthHeader, 8, Length(AuthHeader) - 7);

  // Vérifier le token
  JWTManager := TJWTManager.Create('votre_secret_key_très_sécurisée');
  try
    Result := JWTManager.VerifyToken(Token);

    if not Assigned(Result) then
    begin
      AResponse.Code := 401;
      AResponse.Content := '{"error":"Invalid or expired token"}';
      AResponse.SendResponse;
    end;
  finally
    JWTManager.Free;
  end;
end;

// Utilisation dans une API protégée
procedure HandleProtectedAPI(ARequest: TRequest; AResponse: TResponse);  
var
  UserPayload: TJSONObject;
  Username: string;
begin
  UserPayload := RequireJWT(ARequest, AResponse);

  if not Assigned(UserPayload) then
    Exit; // Le middleware a déjà envoyé une réponse d'erreur

  try
    Username := UserPayload.Get('sub', '');

    // Traiter la requête API
    AResponse.ContentType := 'application/json';
    AResponse.Content := Format('{"message":"Hello, %s!"}', [Username]);
    AResponse.SendResponse;
  finally
    UserPayload.Free;
  end;
end;
```

## 9.6.14 Authentification multi-facteurs (2FA)

### TOTP (Time-based One-Time Password)

Le 2FA ajoute une couche de sécurité supplémentaire en demandant un code temporaire en plus du mot de passe.

```pascal
uses
  SysUtils, Classes, Base32, hmac, sha1;

type
  TTOTPManager = class
  private
    function GenerateSecret: string;
    function GetCurrentTimestamp: Int64;
  public
    function CreateSecret: string;
    function GetTOTPCode(const Secret: string; TimeOffset: Integer = 0): string;
    function VerifyTOTPCode(const Secret, Code: string): Boolean;
    function GetQRCodeURL(const Secret, AccountName, Issuer: string): string;
  end;

function TTOTPManager.GenerateSecret: string;  
const
  Base32Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
var
  i: Integer;
begin
  Result := '';
  Randomize;
  for i := 1 to 16 do
    Result := Result + Base32Chars[Random(32) + 1];
end;

function TTOTPManager.GetCurrentTimestamp: Int64;  
begin
  Result := DateTimeToUnix(Now) div 30; // Fenêtre de 30 secondes
end;

function TTOTPManager.CreateSecret: string;  
begin
  Result := GenerateSecret;
end;

function TTOTPManager.GetTOTPCode(const Secret: string; TimeOffset: Integer = 0): string;  
var
  TimeCounter: Int64;
  Hash: string;
  Offset, BinaryCode: Integer;
  Code: Integer;
begin
  // Obtenir le compteur de temps
  TimeCounter := GetCurrentTimestamp + TimeOffset;

  // Calculer le HMAC-SHA1
  // (Implémentation simplifiée - utilisez une vraie bibliothèque HMAC)
  Hash := HMACSHA1(Secret, IntToStr(TimeCounter));

  // Extraire le code à 6 chiffres
  Offset := StrToInt('$' + Copy(Hash, Length(Hash) - 1, 1)) and $0F;
  BinaryCode := (StrToInt('$' + Copy(Hash, Offset * 2 + 1, 8))) and $7FFFFFFF;
  Code := BinaryCode mod 1000000;

  Result := Format('%.6d', [Code]);
end;

function TTOTPManager.VerifyTOTPCode(const Secret, Code: string): Boolean;  
var
  ExpectedCode: string;
  i: Integer;
begin
  Result := False;

  // Vérifier le code actuel et les fenêtres adjacentes (±1)
  for i := -1 to 1 do
  begin
    ExpectedCode := GetTOTPCode(Secret, i);
    if Code = ExpectedCode then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TTOTPManager.GetQRCodeURL(const Secret, AccountName, Issuer: string): string;  
var
  OTPAuthURL: string;
begin
  // Créer l'URL otpauth pour le QR code
  OTPAuthURL := Format('otpauth://totp/%s:%s?secret=%s&issuer=%s',
                      [HTTPEncode(Issuer), HTTPEncode(AccountName),
                       Secret, HTTPEncode(Issuer)]);

  // URL pour générer le QR code via Google Charts API
  Result := 'https://chart.googleapis.com/chart?chs=200x200&cht=qr&chl=' +
            HTTPEncode(OTPAuthURL);
end;

// Activation du 2FA pour un utilisateur
procedure HandleEnable2FA(ARequest: TRequest; AResponse: TResponse);  
var
  TOTPManager: TTOTPManager;
  Secret, QRCodeURL: string;
  Username: string;
begin
  if not IsAuthenticated(ARequest) then
  begin
    AResponse.Code := 401;
    AResponse.SendResponse;
    Exit;
  end;

  Username := ARequest.Session.Variables['username'];

  TOTPManager := TTOTPManager.Create;
  try
    // Générer un nouveau secret
    Secret := TOTPManager.CreateSecret;

    // Stocker temporairement le secret (en attente de confirmation)
    ARequest.Session.Variables['totp_secret_pending'] := Secret;

    // Générer l'URL du QR code
    QRCodeURL := TOTPManager.GetQRCodeURL(Secret, Username, 'Mon Application');

    // Afficher la page de configuration
    AResponse.Content :=
      '<!DOCTYPE html>' +
      '<html>' +
      '<head><title>Activer 2FA</title></head>' +
      '<body>' +
      '  <h1>Activer l''authentification à deux facteurs</h1>' +
      '  <p>Scannez ce QR code avec votre application d''authentification :</p>' +
      '  <img src="' + QRCodeURL + '" alt="QR Code">' +
      '  <p>Ou entrez manuellement ce code : <strong>' + Secret + '</strong></p>' +
      '  <form method="POST" action="/verify-2fa">' +
      '    <input type="text" name="code" placeholder="Code à 6 chiffres" required>' +
      '    <button type="submit">Vérifier et activer</button>' +
      '  </form>' +
      '</body>' +
      '</html>';
    AResponse.SendResponse;
  finally
    TOTPManager.Free;
  end;
end;

// Vérification et activation du 2FA
procedure HandleVerify2FA(ARequest: TRequest; AResponse: TResponse);  
var
  TOTPManager: TTOTPManager;
  Code, Secret, Username: string;
begin
  if not IsAuthenticated(ARequest) then
  begin
    AResponse.Code := 401;
    AResponse.SendResponse;
    Exit;
  end;

  Code := ARequest.ContentFields.Values['code'];
  Secret := ARequest.Session.Variables['totp_secret_pending'];
  Username := ARequest.Session.Variables['username'];

  if Secret = '' then
  begin
    AResponse.Content := '<h1>Erreur : Aucune configuration 2FA en attente</h1>';
    AResponse.SendResponse;
    Exit;
  end;

  TOTPManager := TTOTPManager.Create;
  try
    if TOTPManager.VerifyTOTPCode(Secret, Code) then
    begin
      // Code valide : activer le 2FA pour l'utilisateur
      // SaveUserTOTPSecret(Username, Secret);

      // Nettoyer la session
      ARequest.Session.Variables['totp_secret_pending'] := '';

      AResponse.Content := '<h1>2FA activé avec succès !</h1>';
    end
    else
    begin
      AResponse.Content := '<h1>Code invalide. Veuillez réessayer.</h1>';
    end;
    AResponse.SendResponse;
  finally
    TOTPManager.Free;
  end;
end;

// Login avec 2FA
procedure HandleLoginWith2FA(ARequest: TRequest; AResponse: TResponse);  
var
  Username, Password, Code: string;
  TOTPSecret: string;
  TOTPManager: TTOTPManager;
begin
  Username := ARequest.ContentFields.Values['username'];
  Password := ARequest.ContentFields.Values['password'];
  Code := ARequest.ContentFields.Values['totp_code'];

  // Vérifier d'abord le mot de passe
  if not VerifyCredentials(Username, Password) then
  begin
    AResponse.Content := '<h1>Identifiants invalides</h1>';
    AResponse.SendResponse;
    Exit;
  end;

  // Récupérer le secret TOTP de l'utilisateur
  TOTPSecret := GetUserTOTPSecret(Username);

  // Si le 2FA est activé pour cet utilisateur
  if TOTPSecret <> '' then
  begin
    if Code = '' then
    begin
      // Demander le code 2FA
      AResponse.Content :=
        '<form method="POST">' +
        '  <input type="hidden" name="username" value="' + Username + '">' +
        '  <input type="hidden" name="password" value="' + Password + '">' +
        '  <input type="text" name="totp_code" placeholder="Code 2FA" required>' +
        '  <button type="submit">Vérifier</button>' +
        '</form>';
      AResponse.SendResponse;
      Exit;
    end;

    // Vérifier le code TOTP
    TOTPManager := TTOTPManager.Create;
    try
      if not TOTPManager.VerifyTOTPCode(TOTPSecret, Code) then
      begin
        AResponse.Content := '<h1>Code 2FA invalide</h1>';
        AResponse.SendResponse;
        Exit;
      end;
    finally
      TOTPManager.Free;
    end;
  end;

  // Connexion réussie
  CreateSession := True;
  ARequest.Session.Variables['authenticated'] := 'true';
  ARequest.Session.Variables['username'] := Username;

  AResponse.Code := 302;
  AResponse.SetCustomHeader('Location', '/dashboard');
  AResponse.SendResponse;
end;
```

## 9.6.15 Limitation du taux de requêtes (Rate Limiting)

### Protection contre les attaques par force brute

Le rate limiting limite le nombre de tentatives de connexion pour prévenir les attaques par force brute.

```pascal
uses
  SysUtils, Classes, DateUtils, Contnrs;

type
  TLoginAttempt = class
  public
    IPAddress: string;
    Timestamp: TDateTime;
    Username: string;
  end;

  TRateLimiter = class
  private
    FAttempts: TObjectList;
    FMaxAttempts: Integer;
    FTimeWindow: Integer; // En minutes
    function CountRecentAttempts(const IPAddress: string): Integer;
    procedure CleanOldAttempts;
  public
    constructor Create(MaxAttempts: Integer = 5; TimeWindowMinutes: Integer = 15);
    destructor Destroy; override;
    procedure RecordAttempt(const IPAddress, Username: string);
    function IsBlocked(const IPAddress: string): Boolean;
    function GetBlockTimeRemaining(const IPAddress: string): Integer; // En secondes
  end;

constructor TRateLimiter.Create(MaxAttempts: Integer; TimeWindowMinutes: Integer);  
begin
  inherited Create;
  FAttempts := TObjectList.Create(True);
  FMaxAttempts := MaxAttempts;
  FTimeWindow := TimeWindowMinutes;
end;

destructor TRateLimiter.Destroy;  
begin
  FAttempts.Free;
  inherited Destroy;
end;

procedure TRateLimiter.CleanOldAttempts;  
var
  i: Integer;
  Attempt: TLoginAttempt;
  CutoffTime: TDateTime;
begin
  CutoffTime := Now - (FTimeWindow / (24 * 60));

  for i := FAttempts.Count - 1 downto 0 do
  begin
    Attempt := TLoginAttempt(FAttempts[i]);
    if Attempt.Timestamp < CutoffTime then
      FAttempts.Delete(i);
  end;
end;

function TRateLimiter.CountRecentAttempts(const IPAddress: string): Integer;  
var
  i: Integer;
  Attempt: TLoginAttempt;
  CutoffTime: TDateTime;
begin
  Result := 0;
  CutoffTime := Now - (FTimeWindow / (24 * 60));

  for i := 0 to FAttempts.Count - 1 do
  begin
    Attempt := TLoginAttempt(FAttempts[i]);
    if (Attempt.IPAddress = IPAddress) and (Attempt.Timestamp >= CutoffTime) then
      Inc(Result);
  end;
end;

procedure TRateLimiter.RecordAttempt(const IPAddress, Username: string);  
var
  Attempt: TLoginAttempt;
begin
  CleanOldAttempts;

  Attempt := TLoginAttempt.Create;
  Attempt.IPAddress := IPAddress;
  Attempt.Username := Username;
  Attempt.Timestamp := Now;

  FAttempts.Add(Attempt);
end;

function TRateLimiter.IsBlocked(const IPAddress: string): Boolean;  
begin
  CleanOldAttempts;
  Result := CountRecentAttempts(IPAddress) >= FMaxAttempts;
end;

function TRateLimiter.GetBlockTimeRemaining(const IPAddress: string): Integer;  
var
  i: Integer;
  Attempt: TLoginAttempt;
  OldestAttempt: TDateTime;
  UnblockTime: TDateTime;
begin
  Result := 0;
  OldestAttempt := Now;

  // Trouver la tentative la plus ancienne pour cette IP
  for i := 0 to FAttempts.Count - 1 do
  begin
    Attempt := TLoginAttempt(FAttempts[i]);
    if (Attempt.IPAddress = IPAddress) and (Attempt.Timestamp < OldestAttempt) then
      OldestAttempt := Attempt.Timestamp;
  end;

  // Calculer quand le blocage sera levé
  UnblockTime := OldestAttempt + (FTimeWindow / (24 * 60));
  if UnblockTime > Now then
    Result := SecondsBetween(UnblockTime, Now);
end;

// Instance globale du rate limiter
var
  GlobalRateLimiter: TRateLimiter;

initialization
  GlobalRateLimiter := TRateLimiter.Create(5, 15); // 5 tentatives max en 15 minutes

finalization
  GlobalRateLimiter.Free;

// Utilisation dans la procédure de login
procedure HandleLoginWithRateLimit(ARequest: TRequest; AResponse: TResponse);  
var
  Username, Password: string;
  ClientIP: string;
  TimeRemaining: Integer;
begin
  // Obtenir l'adresse IP du client
  ClientIP := ARequest.RemoteAddr;

  // Vérifier si l'IP est bloquée
  if GlobalRateLimiter.IsBlocked(ClientIP) then
  begin
    TimeRemaining := GlobalRateLimiter.GetBlockTimeRemaining(ClientIP);

    AResponse.Code := 429; // Too Many Requests
    AResponse.Content := Format(
      '<h1>Trop de tentatives de connexion</h1>' +
      '<p>Veuillez réessayer dans %d minutes.</p>',
      [TimeRemaining div 60 + 1]
    );
    AResponse.SendResponse;
    Exit;
  end;

  Username := ARequest.ContentFields.Values['username'];
  Password := ARequest.ContentFields.Values['password'];

  // Enregistrer la tentative
  GlobalRateLimiter.RecordAttempt(ClientIP, Username);

  // Vérifier les identifiants
  if VerifyCredentials(Username, Password) then
  begin
    // Connexion réussie : réinitialiser les tentatives pour cette IP
    // (optionnel, selon votre politique de sécurité)

    CreateSession := True;
    ARequest.Session.Variables['authenticated'] := 'true';
    ARequest.Session.Variables['username'] := Username;

    AResponse.Code := 302;
    AResponse.SetCustomHeader('Location', '/dashboard');
    AResponse.SendResponse;
  end
  else
  begin
    AResponse.Content := '<h1>Identifiants incorrects</h1>';
    AResponse.SendResponse;
  end;
end;
```

## 9.6.16 Gestion de l'expiration des sessions

### Timeout d'inactivité

```pascal
type
  TSessionManager = class
  private
    FInactivityTimeout: Integer; // En minutes
  public
    constructor Create(InactivityTimeoutMinutes: Integer = 30);
    procedure UpdateLastActivity(ARequest: TRequest);
    function IsSessionExpired(ARequest: TRequest): Boolean;
    procedure RefreshSession(ARequest: TRequest);
  end;

constructor TSessionManager.Create(InactivityTimeoutMinutes: Integer);  
begin
  inherited Create;
  FInactivityTimeout := InactivityTimeoutMinutes;
end;

procedure TSessionManager.UpdateLastActivity(ARequest: TRequest);  
var
  SessionObj: TCustomSession;
begin
  SessionObj := ARequest.Session;
  if Assigned(SessionObj) then
    SessionObj.Variables['last_activity'] := DateTimeToStr(Now);
end;

function TSessionManager.IsSessionExpired(ARequest: TRequest): Boolean;  
var
  SessionObj: TCustomSession;
  LastActivity: TDateTime;
  LastActivityStr: string;
begin
  Result := True;
  SessionObj := ARequest.Session;

  if not Assigned(SessionObj) then
    Exit;

  LastActivityStr := SessionObj.Variables['last_activity'];
  if LastActivityStr = '' then
    Exit;

  LastActivity := StrToDateTime(LastActivityStr);
  Result := MinutesBetween(Now, LastActivity) > FInactivityTimeout;
end;

procedure TSessionManager.RefreshSession(ARequest: TRequest);  
var
  SessionObj: TCustomSession;
  OldSessionID: string;
begin
  SessionObj := ARequest.Session;
  if Assigned(SessionObj) then
  begin
    // Régénérer l'ID de session (protection contre le session fixation)
    OldSessionID := SessionObj.SessionID;
    // SessionObj.RegenerateID; // Si disponible dans votre version de fpWeb

    UpdateLastActivity(ARequest);
  end;
end;

// Middleware de vérification de session
function CheckSessionValidity(ARequest: TRequest; AResponse: TResponse): Boolean;  
var
  SessionManager: TSessionManager;
begin
  Result := False;

  if not IsAuthenticated(ARequest) then
  begin
    AResponse.Code := 302;
    AResponse.SetCustomHeader('Location', '/login');
    AResponse.SendResponse;
    Exit;
  end;

  SessionManager := TSessionManager.Create(30);
  try
    if SessionManager.IsSessionExpired(ARequest) then
    begin
      // Session expirée
      ARequest.Session.Terminate;

      AResponse.Code := 302;
      AResponse.SetCustomHeader('Location', '/login?expired=1');
      AResponse.SendResponse;
      Exit;
    end;

    // Mettre à jour l'activité
    SessionManager.UpdateLastActivity(ARequest);
    Result := True;
  finally
    SessionManager.Free;
  end;
end;
```

## 9.6.17 Audit et journalisation des connexions

### Enregistrement des événements de sécurité

```pascal
type
  TAuthEvent = (aeLoginSuccess, aeLoginFailed, aeLogout, ae2FAEnabled,
                ae2FADisabled, aePasswordChanged, aeSessionExpired);

  TAuditLogger = class
  private
    FLogFile: string;
    procedure WriteLog(const Message: string);
  public
    constructor Create(const LogFilePath: string);
    procedure LogAuthEvent(Event: TAuthEvent; const Username, IPAddress,
                          Details: string);
    procedure LogSecurityEvent(const EventType, Username, IPAddress,
                              Details: string);
  end;

constructor TAuditLogger.Create(const LogFilePath: string);  
begin
  inherited Create;
  FLogFile := LogFilePath;

  // Créer le répertoire de logs s'il n'existe pas
  ForceDirectories(ExtractFilePath(FLogFile));
end;

procedure TAuditLogger.WriteLog(const Message: string);  
var
  LogFileStream: TFileStream;
  LogLine: string;
begin
  LogLine := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' | ' +
             Message + LineEnding;

  if FileExists(FLogFile) then
    LogFileStream := TFileStream.Create(FLogFile, fmOpenWrite or fmShareDenyWrite)
  else
    LogFileStream := TFileStream.Create(FLogFile, fmCreate);

  try
    LogFileStream.Seek(0, soEnd);
    LogFileStream.WriteBuffer(LogLine[1], Length(LogLine));
  finally
    LogFileStream.Free;
  end;
end;

procedure TAuditLogger.LogAuthEvent(Event: TAuthEvent; const Username,
                                    IPAddress, Details: string);
var
  EventStr: string;
begin
  case Event of
    aeLoginSuccess:    EventStr := 'LOGIN_SUCCESS';
    aeLoginFailed:     EventStr := 'LOGIN_FAILED';
    aeLogout:          EventStr := 'LOGOUT';
    ae2FAEnabled:      EventStr := '2FA_ENABLED';
    ae2FADisabled:     EventStr := '2FA_DISABLED';
    aePasswordChanged: EventStr := 'PASSWORD_CHANGED';
    aeSessionExpired:  EventStr := 'SESSION_EXPIRED';
  end;

  WriteLog(Format('%s | User: %s | IP: %s | %s',
                 [EventStr, Username, IPAddress, Details]));
end;

procedure TAuditLogger.LogSecurityEvent(const EventType, Username, IPAddress,
                                        Details: string);
begin
  WriteLog(Format('%s | User: %s | IP: %s | %s',
                 [EventType, Username, IPAddress, Details]));
end;

// Instance globale
var
  AuditLogger: TAuditLogger;

initialization
  {$IFDEF WINDOWS}
  AuditLogger := TAuditLogger.Create('C:\logs\auth\audit.log');
  {$ELSE}
  AuditLogger := TAuditLogger.Create('/var/log/myapp/audit.log');
  {$ENDIF}

finalization
  AuditLogger.Free;

// Utilisation dans les fonctions d'authentification
procedure HandleLoginWithAudit(ARequest: TRequest; AResponse: TResponse);  
var
  Username, Password: string;
  ClientIP: string;
begin
  Username := ARequest.ContentFields.Values['username'];
  Password := ARequest.ContentFields.Values['password'];
  ClientIP := ARequest.RemoteAddr;

  if VerifyCredentials(Username, Password) then
  begin
    // Connexion réussie
    CreateSession := True;
    ARequest.Session.Variables['authenticated'] := 'true';
    ARequest.Session.Variables['username'] := Username;

    // Logger l'événement
    AuditLogger.LogAuthEvent(aeLoginSuccess, Username, ClientIP,
                            'User logged in successfully');

    AResponse.Code := 302;
    AResponse.SetCustomHeader('Location', '/dashboard');
    AResponse.SendResponse;
  end
  else
  begin
    // Connexion échouée
    AuditLogger.LogAuthEvent(aeLoginFailed, Username, ClientIP,
                            'Invalid credentials provided');

    AResponse.Content := '<h1>Identifiants incorrects</h1>';
    AResponse.SendResponse;
  end;
end;

procedure HandleLogoutWithAudit(ARequest: TRequest; AResponse: TResponse);  
var
  Username, ClientIP: string;
begin
  Username := ARequest.Session.Variables['username'];
  ClientIP := ARequest.RemoteAddr;

  // Logger l'événement
  AuditLogger.LogAuthEvent(aeLogout, Username, ClientIP,
                          'User logged out');

  // Terminer la session
  ARequest.Session.Terminate;

  AResponse.Code := 302;
  AResponse.SetCustomHeader('Location', '/login');
  AResponse.SendResponse;
end;
```

## 9.6.18 SSO (Single Sign-On) avec SAML

### Introduction à SAML

SAML (Security Assertion Markup Language) est un standard pour l'authentification unique entre plusieurs applications.

**Flux SAML simplifié :**

```
┌─────────┐                 ┌──────────────┐                 ┌─────────┐
│         │                 │   Service    │                 │Identity │
│  User   │                 │   Provider   │                 │Provider │
│         │                 │     (SP)     │                 │  (IdP)  │
└────┬────┘                 └──────┬───────┘                 └────┬────┘
     │                             │                              │
     │ 1. Accès à l'application    │                              │
     │────────────────────────────>│                              │
     │                             │                              │
     │ 2. Redirection vers IdP     │                              │
     │<────────────────────────────│                              │
     │                             │                              │
     │ 3. Authentification         │                              │
     │─────────────────────────────────────────────────────────>  │
     │                             │                              │
     │ 4. Assertion SAML           │                              │
     │<─────────────────────────────────────────────────────────  │
     │                             │                              │
     │ 5. Envoi assertion au SP    │                              │
     │────────────────────────────>│                              │
     │                             │                              │
     │                    6. Vérification et création session     │
     │                             │                              │
     │ 7. Accès accordé            │                              │
     │<────────────────────────────│                              │
```

### Implémentation basique SAML

```pascal
uses
  SysUtils, Classes, DOM, XMLRead, XMLWrite, Base64;

type
  TSAMLManager = class
  private
    FEntityID: string;
    FAssertionConsumerServiceURL: string;
    FCertificate: string;
    function CreateAuthRequest: string;
    function ValidateAssertion(const SAMLResponse: string): Boolean;
    function ExtractUserInfo(const SAMLResponse: string): TJSONObject;
  public
    constructor Create(const EntityID, ACSURL, Certificate: string);
    function GetSSORedirectURL(const IdPURL: string): string;
    function HandleSAMLResponse(const SAMLResponse: string): TJSONObject;
  end;

constructor TSAMLManager.Create(const EntityID, ACSURL, Certificate: string);  
begin
  inherited Create;
  FEntityID := EntityID;
  FAssertionConsumerServiceURL := ACSURL;
  FCertificate := Certificate;
end;

function TSAMLManager.CreateAuthRequest: string;  
var
  Doc: TXMLDocument;
  Root, Element: TDOMElement;
  StringStream: TStringStream;
begin
  Doc := TXMLDocument.Create;
  try
    // Créer la structure XML de la requête SAML
    Root := Doc.CreateElement('samlp:AuthnRequest');
    Doc.AppendChild(Root);

    Root.SetAttribute('xmlns:samlp', 'urn:oasis:names:tc:SAML:2.0:protocol');
    Root.SetAttribute('xmlns:saml', 'urn:oasis:names:tc:SAML:2.0:assertion');
    Root.SetAttribute('ID', '_' + GenerateRandomString(40));
    Root.SetAttribute('Version', '2.0');
    Root.SetAttribute('IssueInstant', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', Now));
    Root.SetAttribute('AssertionConsumerServiceURL', FAssertionConsumerServiceURL);

    // Ajouter l'émetteur
    Element := Doc.CreateElement('saml:Issuer');
    Element.TextContent := FEntityID;
    Root.AppendChild(Element);

    // Convertir en string
    StringStream := TStringStream.Create('');
    try
      WriteXMLFile(Doc, StringStream);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    Doc.Free;
  end;
end;

function TSAMLManager.GetSSORedirectURL(const IdPURL: string): string;  
var
  AuthRequest: string;
  EncodedRequest: string;
begin
  // Créer la requête d'authentification
  AuthRequest := CreateAuthRequest;

  // Encoder en Base64
  EncodedRequest := EncodeStringBase64(AuthRequest);

  // Créer l'URL de redirection
  Result := IdPURL + '?SAMLRequest=' + HTTPEncode(EncodedRequest);
end;

function TSAMLManager.ValidateAssertion(const SAMLResponse: string): Boolean;  
begin
  // Implémentation simplifiée
  // En production, vérifier :
  // - La signature XML
  // - Le certificat
  // - La validité temporelle
  // - Le destinataire
  Result := True; // Placeholder
end;

function TSAMLManager.ExtractUserInfo(const SAMLResponse: string): TJSONObject;  
var
  Doc: TXMLDocument;
  DecodedResponse: string;
  NodeList: TDOMNodeList;
  i: Integer;
begin
  Result := TJSONObject.Create;

  // Décoder la réponse Base64
  DecodedResponse := DecodeStringBase64(SAMLResponse);

  // Parser le XML
  ReadXMLFragment(Doc, DecodedResponse);
  try
    // Extraire les attributs utilisateur
    // (Implémentation simplifiée)
    NodeList := Doc.GetElementsByTagName('saml:Attribute');

    for i := 0 to NodeList.Count - 1 do
    begin
      // Extraire les attributs comme email, name, etc.
      // et les ajouter à l'objet JSON
    end;
  finally
    Doc.Free;
  end;
end;

function TSAMLManager.HandleSAMLResponse(const SAMLResponse: string): TJSONObject;  
begin
  Result := nil;

  // Valider la réponse SAML
  if not ValidateAssertion(SAMLResponse) then
    Exit;

  // Extraire les informations utilisateur
  Result := ExtractUserInfo(SAMLResponse);
end;

// Utilisation dans l'application
procedure HandleSAMLLogin(ARequest: TRequest; AResponse: TResponse);  
var
  SAMLManager: TSAMLManager;
  RedirectURL: string;
begin
  SAMLManager := TSAMLManager.Create(
    'https://monapp.com/saml/metadata',
    'https://monapp.com/saml/acs',
    LoadCertificateFromFile('certificate.pem')
  );
  try
    // Obtenir l'URL de redirection vers l'IdP
    RedirectURL := SAMLManager.GetSSORedirectURL('https://idp.example.com/sso');

    // Rediriger l'utilisateur
    AResponse.Code := 302;
    AResponse.SetCustomHeader('Location', RedirectURL);
    AResponse.SendResponse;
  finally
    SAMLManager.Free;
  end;
end;

procedure HandleSAMLCallback(ARequest: TRequest; AResponse: TResponse);  
var
  SAMLManager: TSAMLManager;
  SAMLResponse: string;
  UserInfo: TJSONObject;
  Email, Name: string;
begin
  SAMLResponse := ARequest.ContentFields.Values['SAMLResponse'];

  SAMLManager := TSAMLManager.Create(
    'https://monapp.com/saml/metadata',
    'https://monapp.com/saml/acs',
    LoadCertificateFromFile('certificate.pem')
  );
  try
    UserInfo := SAMLManager.HandleSAMLResponse(SAMLResponse);

    if Assigned(UserInfo) then
    try
      // Extraire les informations
      Email := UserInfo.Get('email', '');
      Name := UserInfo.Get('name', '');

      // Créer la session
      CreateSession := True;
      ARequest.Session.Variables['authenticated'] := 'true';
      ARequest.Session.Variables['email'] := Email;
      ARequest.Session.Variables['name'] := Name;
      ARequest.Session.Variables['auth_method'] := 'saml';

      // Rediriger
      AResponse.Code := 302;
      AResponse.SetCustomHeader('Location', '/dashboard');
      AResponse.SendResponse;
    finally
      UserInfo.Free;
    end
    else
    begin
      AResponse.Content := '<h1>Erreur d''authentification SAML</h1>';
      AResponse.SendResponse;
    end;
  finally
    SAMLManager.Free;
  end;
end;
```

## 9.6.19 Bonnes pratiques de sécurité

### Liste de contrôle de sécurité

**Configuration des sessions :**
- ✅ Utiliser HTTPS exclusivement en production
- ✅ Configurer les cookies avec `HttpOnly`, `Secure`, et `SameSite`
- ✅ Définir un timeout de session raisonnable (15-30 minutes)
- ✅ Régénérer l'ID de session après connexion
- ✅ Stocker les sessions de manière sécurisée

**Authentification :**
- ✅ Hasher les mots de passe avec bcrypt ou Argon2
- ✅ Ne jamais stocker les mots de passe en clair
- ✅ Implémenter un rate limiting contre la force brute
- ✅ Utiliser le 2FA pour les comptes sensibles
- ✅ Valider les redirections pour éviter l'open redirect

**Protection CSRF :**
- ✅ Utiliser des tokens CSRF pour tous les formulaires
- ✅ Vérifier l'origine des requêtes
- ✅ Utiliser `SameSite=Strict` ou `Lax` sur les cookies

**Audit et monitoring :**
- ✅ Logger tous les événements d'authentification
- ✅ Surveiller les tentatives de connexion suspectes
- ✅ Alerter en cas d'activité anormale
- ✅ Conserver les logs pour l'analyse forensique

### Exemple de configuration sécurisée complète

```pascal
procedure ConfigureSecureSession(ARequest: TRequest; AResponse: TResponse);  
begin
  // Activer les sessions
  CreateSession := True;

  // Configurer le timeout
  SessionTimeout := 30; // 30 minutes

  // Configurer le cookie de session
  with AResponse.Cookies.FindCookie('FPWEBSID') do
  begin
    // Protection XSS
    HttpOnly := True;

    // HTTPS uniquement
    Secure := True;

    // Protection CSRF
    SameSite := 'Strict';

    // Limiter au chemin de l'application
    Path := '/';

    // Session uniquement (pas de persistance)
    MaxAge := 0;
  end;

  // Configurer les en-têtes de sécurité
  AResponse.SetCustomHeader('X-Content-Type-Options', 'nosniff');
  AResponse.SetCustomHeader('X-Frame-Options', 'DENY');
  AResponse.SetCustomHeader('X-XSS-Protection', '1; mode=block');
  AResponse.SetCustomHeader('Strict-Transport-Security',
                           'max-age=31536000; includeSubDomains');
  AResponse.SetCustomHeader('Content-Security-Policy',
                           'default-src ''self''; script-src ''self''');
end;
```

## 9.6.20 Conclusion

### Récapitulatif

Ce chapitre a couvert les aspects essentiels des sessions et de l'authentification :

1. **Sessions HTTP** : Mécanisme pour maintenir l'état utilisateur
2. **Stockage** : Mémoire, fichiers INI, base de données
3. **Cookies** : Transport du Session ID avec options de sécurité
4. **Authentification basique** : Formulaires de connexion et vérification
5. **Hachage de mots de passe** : bcrypt, PBKDF2, Argon2
6. **Rôles et permissions** : Contrôle d'accès basé sur les rôles
7. **Protection CSRF** : Tokens pour prévenir les attaques
8. **Remember Me** : Persistance de la connexion
9. **OAuth 2.0** : Authentification via services externes
10. **JWT** : Tokens pour API REST
11. **2FA/TOTP** : Authentification à deux facteurs
12. **Rate Limiting** : Protection contre la force brute
13. **Expiration** : Gestion du timeout d'inactivité
14. **Audit** : Journalisation des événements de sécurité
15. **SAML/SSO** : Authentification unique entre applications

### Points clés à retenir

**Sécurité avant tout :**
- Ne jamais faire de compromis sur la sécurité
- Utiliser HTTPS en production
- Hasher tous les mots de passe
- Implémenter une protection CSRF
- Logger les événements d'authentification

**Multi-plateforme :**
- Tester sur Windows et Ubuntu
- Adapter les chemins de fichiers
- Considérer les différences de permissions
- Utiliser des bibliothèques portables

**Évolutivité :**
- Choisir un stockage de sessions adapté à la charge
- Implémenter le rate limiting dès le début
- Prévoir l'authentification externe (OAuth, SAML)
- Structurer le code pour faciliter les évolutions

### Ressources complémentaires

**Standards et RFC :**
- RFC 6265 : HTTP State Management Mechanism (Cookies)
- RFC 6749 : OAuth 2.0 Authorization Framework
- RFC 7519 : JSON Web Token (JWT)
- RFC 6238 : TOTP Time-Based One-Time Password

**Bibliothèques FreePascal :**
- fpWeb : Framework web intégré
- DCPcrypt : Cryptographie
- fpBCrypt : Hachage bcrypt
- Synapse : Bibliothèque réseau

**Outils de test :**
- OWASP ZAP : Test de sécurité web
- Burp Suite : Analyse de trafic HTTP
- Postman : Test d'API
- curl : Client HTTP en ligne de commande

La sécurité des sessions et de l'authentification est un domaine en constante évolution. Il est important de rester informé des nouvelles vulnérabilités et des bonnes pratiques actuelles.

⏭️ [Microservices avec FreePascal](/09-programmation-web-freepascal/07-microservices-avec-freepascal.md)
