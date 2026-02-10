🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.3 Authentification et OAuth 2.0

## Introduction

L'authentification est le processus de vérification de l'identité d'un utilisateur ou d'un système. Dans le développement d'applications modernes, il est essentiel de mettre en place des mécanismes d'authentification robustes et sécurisés.

OAuth 2.0 est devenu le standard de facto pour l'authentification et l'autorisation dans les applications web et mobiles. Il permet à vos utilisateurs de se connecter en utilisant leurs comptes existants (Google, Facebook, GitHub, etc.) sans avoir à créer de nouveaux identifiants.

## Concepts fondamentaux de l'authentification

### Authentification vs Autorisation

**Authentification** : "Qui êtes-vous ?"
- Vérifier l'identité d'un utilisateur
- Exemples : mot de passe, empreinte digitale, carte d'identité
- Réponse : "Je suis Alice"

**Autorisation** : "Que pouvez-vous faire ?"
- Déterminer les permissions d'un utilisateur
- Exemples : accès admin, lecture seule, modification
- Réponse : "Alice peut lire et modifier les documents"

### Facteurs d'authentification

**Quelque chose que vous savez** :
- Mot de passe
- Code PIN
- Question secrète

**Quelque chose que vous possédez** :
- Téléphone mobile (pour SMS/TOTP)
- Clé de sécurité (YubiKey)
- Carte à puce

**Quelque chose que vous êtes** :
- Empreinte digitale
- Reconnaissance faciale
- Reconnaissance vocale

### Authentification multi-facteurs (MFA/2FA)

Combiner au moins deux facteurs différents pour renforcer la sécurité :

```
Mot de passe (ce que vous savez)
    +
Code SMS (ce que vous possédez)
    =
Authentification à deux facteurs (2FA)
```

## Méthodes d'authentification traditionnelles

### 1. Authentification par mot de passe

La méthode la plus courante, mais aussi la plus vulnérable.

**Flux d'authentification** :
```
1. Utilisateur envoie : username + password
2. Serveur vérifie : hash(password) == stored_hash ?
3. Si oui : Session créée, cookie/token envoyé
4. Si non : Accès refusé
```

**Exemple conceptuel** :

```pascal
type
  TUser = record
    Username: string;
    PasswordHash: string;
    Salt: string;
  end;

function AuthenticateUser(const Username, Password: string): Boolean;  
var
  User: TUser;
  ComputedHash: string;
begin
  Result := False;

  // 1. Récupérer l'utilisateur depuis la base de données
  User := GetUserFromDatabase(Username);
  if User.Username = '' then
    Exit; // Utilisateur inexistant

  // 2. Calculer le hash du mot de passe fourni
  ComputedHash := HashPassword(Password, User.Salt);

  // 3. Comparer avec le hash stocké
  Result := (ComputedHash = User.PasswordHash);
end;
```

**Bonnes pratiques** :
- ✅ Toujours utiliser un hash fort (bcrypt, Argon2, scrypt)
- ✅ Utiliser un salt unique par utilisateur
- ✅ Implémenter un délai progressif après échecs
- ❌ Ne jamais stocker les mots de passe en clair
- ❌ Ne jamais envoyer les mots de passe par email

### 2. Authentification basée sur session

Après authentification réussie, le serveur crée une session :

```pascal
type
  TSession = record
    SessionID: string;
    UserID: Integer;
    CreatedAt: TDateTime;
    ExpiresAt: TDateTime;
    IPAddress: string;
  end;

function CreateSession(UserID: Integer): string;  
var
  Session: TSession;
begin
  Session.SessionID := GenerateRandomToken(32); // Token aléatoire
  Session.UserID := UserID;
  Session.CreatedAt := Now;
  Session.ExpiresAt := Now + (1/24); // Expire dans 1 heure
  Session.IPAddress := GetClientIP;

  // Stocker en base ou en cache (Redis)
  SaveSession(Session);

  Result := Session.SessionID;
end;

function ValidateSession(const SessionID: string): Boolean;  
var
  Session: TSession;
begin
  Session := GetSession(SessionID);

  Result := (Session.SessionID <> '') and
            (Now < Session.ExpiresAt);
end;
```

**Avantages** :
- Simple à implémenter
- Le serveur contrôle complètement les sessions
- Facile à révoquer

**Inconvénients** :
- Nécessite stockage côté serveur
- Difficile à scaler horizontalement
- Problèmes avec CORS et applications multi-domaines

### 3. Authentification par token (JWT)

Les JSON Web Tokens (JWT) sont des tokens auto-contenus qui portent l'information d'authentification.

**Structure d'un JWT** :
```
Header.Payload.Signature  
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c
```

**Décodage** :

Header (base64) :
```json
{
  "alg": "HS256",
  "typ": "JWT"
}
```

Payload (base64) :
```json
{
  "sub": "1234567890",
  "name": "John Doe",
  "iat": 1516239022,
  "exp": 1516242622
}
```

Signature :
```
HMACSHA256(
  base64UrlEncode(header) + "." + base64UrlEncode(payload),
  secret
)
```

**Exemple conceptuel** :

```pascal
uses
  SysUtils, Base64;

type
  TJWTClaims = record
    Subject: string;  // sub : ID utilisateur
    Name: string;     // name : Nom de l'utilisateur
    IssuedAt: Int64;  // iat : Timestamp de création
    ExpiresAt: Int64; // exp : Timestamp d'expiration
  end;

function CreateJWT(const Claims: TJWTClaims; const Secret: string): string;  
var
  Header, Payload, Signature: string;
  ToSign: string;
begin
  // Header
  Header := Base64URLEncode('{"alg":"HS256","typ":"JWT"}');

  // Payload
  Payload := Base64URLEncode(Format(
    '{"sub":"%s","name":"%s","iat":%d,"exp":%d}',
    [Claims.Subject, Claims.Name, Claims.IssuedAt, Claims.ExpiresAt]
  ));

  // Signature
  ToSign := Header + '.' + Payload;
  Signature := Base64URLEncode(HMACSHA256(ToSign, Secret));

  Result := Header + '.' + Payload + '.' + Signature;
end;

function ValidateJWT(const Token, Secret: string): Boolean;  
var
  Parts: TStringList;
  Header, Payload, Signature, ComputedSignature: string;
  ToSign: string;
begin
  Result := False;

  // Séparer les parties
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '.';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Token;
    if Parts.Count <> 3 then Exit;

    Header := Parts[0];
    Payload := Parts[1];
    Signature := Parts[2];

  // Vérifier la signature
  ToSign := Header + '.' + Payload;
  ComputedSignature := Base64URLEncode(HMACSHA256(ToSign, Secret));

    if Signature <> ComputedSignature then Exit;

    // Vérifier l'expiration
    // (parser le payload et vérifier exp > now)

    Result := True;
  finally
    Parts.Free;
  end;
end;
```

**Avantages JWT** :
- ✅ Stateless (pas de stockage serveur)
- ✅ Auto-contenu (toutes les infos dans le token)
- ✅ Facilite le scaling horizontal
- ✅ Fonctionne bien avec CORS et microservices

**Inconvénients JWT** :
- ❌ Impossible de révoquer facilement
- ❌ Taille plus grande qu'un ID de session
- ❌ Risque si le secret est compromis

## Introduction à OAuth 2.0

### Qu'est-ce qu'OAuth 2.0 ?

OAuth 2.0 est un **protocole d'autorisation** (pas d'authentification à l'origine) qui permet à une application d'accéder aux ressources d'un utilisateur sur un autre service, sans partager les identifiants.

**Exemple concret** :
```
Vous voulez que l'application "PhotoPrint" accède à vos photos Google :
- Sans donner votre mot de passe Google à PhotoPrint
- En contrôlant quelles photos sont accessibles
- En pouvant révoquer l'accès à tout moment
```

### Acteurs d'OAuth 2.0

**Resource Owner (Propriétaire de ressource)** :
- L'utilisateur qui possède les données
- Exemple : Vous

**Client (Application cliente)** :
- L'application qui veut accéder aux ressources
- Exemple : PhotoPrint

**Authorization Server (Serveur d'autorisation)** :
- Le serveur qui authentifie l'utilisateur et émet les tokens
- Exemple : accounts.google.com

**Resource Server (Serveur de ressources)** :
- Le serveur qui héberge les ressources protégées
- Exemple : photos.google.com

### Flux OAuth 2.0 simplifié

```
1. Client redirige vers Authorization Server
   → "Voulez-vous autoriser PhotoPrint à accéder à vos photos ?"

2. Utilisateur s'authentifie et accepte

3. Authorization Server redirige vers Client avec un code

4. Client échange le code contre un Access Token

5. Client utilise l'Access Token pour accéder aux ressources
```

## Types de flux OAuth 2.0 (Grant Types)

### 1. Authorization Code Flow (Recommandé)

Le flux le plus sécurisé, utilisé pour les applications web avec backend.

**Étapes détaillées** :

```
1. Client → User Agent → Authorization Server
   GET /authorize?
       response_type=code
       &client_id=ABC123
       &redirect_uri=https://app.com/callback
       &scope=read:photos
       &state=xyz

2. Authorization Server authentifie l'utilisateur
   → Page de consentement

3. Authorization Server → User Agent → Client
   GET https://app.com/callback?
       code=AUTH_CODE_HERE
       &state=xyz

4. Client → Authorization Server
   POST /token
   {
     "grant_type": "authorization_code",
     "code": "AUTH_CODE_HERE",
     "client_id": "ABC123",
     "client_secret": "SECRET",
     "redirect_uri": "https://app.com/callback"
   }

5. Authorization Server → Client
   {
     "access_token": "ACCESS_TOKEN",
     "token_type": "Bearer",
     "expires_in": 3600,
     "refresh_token": "REFRESH_TOKEN"
   }

6. Client → Resource Server
   GET /api/photos
   Authorization: Bearer ACCESS_TOKEN
```

**Exemple FreePascal** :

```pascal
uses
  SysUtils, httpsend, ssl_openssl;

const
  CLIENT_ID = 'votre_client_id';
  CLIENT_SECRET = 'votre_client_secret';
  REDIRECT_URI = 'http://localhost:8080/callback';
  AUTH_URL = 'https://accounts.google.com/o/oauth2/v2/auth';
  TOKEN_URL = 'https://oauth2.googleapis.com/token';

function GetAuthorizationURL(const State: string): string;  
begin
  Result := AUTH_URL + '?' +
    'client_id=' + CLIENT_ID +
    '&redirect_uri=' + UrlEncode(REDIRECT_URI) +
    '&response_type=code' +
    '&scope=' + UrlEncode('https://www.googleapis.com/auth/userinfo.email') +
    '&state=' + State;
end;

function ExchangeCodeForToken(const Code: string): string;  
var
  HTTP: THTTPSend;
  PostData: string;
  Response: string;
begin
  HTTP := THTTPSend.Create;
  try
    // Préparer les données POST
    PostData :=
      'grant_type=authorization_code' +
      '&code=' + UrlEncode(Code) +
      '&client_id=' + CLIENT_ID +
      '&client_secret=' + CLIENT_SECRET +
      '&redirect_uri=' + UrlEncode(REDIRECT_URI);

    // Envoyer la requête
    HTTP.Document.Write(PostData[1], Length(PostData));
    HTTP.MimeType := 'application/x-www-form-urlencoded';

    if HTTP.HTTPMethod('POST', TOKEN_URL) then
    begin
      SetLength(Response, HTTP.Document.Size);
      HTTP.Document.Position := 0;
      HTTP.Document.Read(Response[1], HTTP.Document.Size);

      // Parser le JSON pour extraire access_token
      Result := ExtractAccessToken(Response);
    end;
  finally
    HTTP.Free;
  end;
end;

function MakeAuthenticatedRequest(const AccessToken, URL: string): string;  
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    // Ajouter le token dans l'en-tête Authorization
    HTTP.Headers.Add('Authorization: Bearer ' + AccessToken);

    if HTTP.HTTPMethod('GET', URL) then
    begin
      SetLength(Result, HTTP.Document.Size);
      HTTP.Document.Position := 0;
      HTTP.Document.Read(Result[1], HTTP.Document.Size);
    end;
  finally
    HTTP.Free;
  end;
end;
```

### 2. Implicit Flow (Déprécié)

⚠️ **Ne plus utiliser** - Moins sécurisé, remplacé par Authorization Code Flow avec PKCE.

Anciennement utilisé pour les Single Page Applications (SPA), l'access token est retourné directement dans l'URL de redirection.

**Problèmes** :
- Token exposé dans l'URL du navigateur
- Token peut être intercepté via l'historique
- Pas de refresh token

### 3. Client Credentials Flow

Pour les applications qui accèdent à leurs propres ressources (pas d'utilisateur).

**Utilisation** :
- Communication serveur à serveur
- Microservices
- Tâches automatisées

**Flux** :

```
Client → Authorization Server  
POST /token
{
  "grant_type": "client_credentials",
  "client_id": "ABC123",
  "client_secret": "SECRET",
  "scope": "api:read"
}

Authorization Server → Client
{
  "access_token": "ACCESS_TOKEN",
  "token_type": "Bearer",
  "expires_in": 3600
}
```

**Exemple** :

```pascal
function GetClientCredentialsToken: string;  
var
  HTTP: THTTPSend;
  PostData, Response: string;
begin
  HTTP := THTTPSend.Create;
  try
    PostData :=
      'grant_type=client_credentials' +
      '&client_id=' + CLIENT_ID +
      '&client_secret=' + CLIENT_SECRET +
      '&scope=api:read';

    HTTP.Document.Write(PostData[1], Length(PostData));
    HTTP.MimeType := 'application/x-www-form-urlencoded';

    if HTTP.HTTPMethod('POST', TOKEN_URL) then
    begin
      SetLength(Response, HTTP.Document.Size);
      HTTP.Document.Read(Response[1], HTTP.Document.Size);
      Result := ExtractAccessToken(Response);
    end;
  finally
    HTTP.Free;
  end;
end;
```

### 4. Resource Owner Password Credentials (Déconseillé)

⚠️ **À éviter** - L'utilisateur donne son mot de passe directement à l'application.

Seulement acceptable pour :
- Applications first-party (développées par le même propriétaire que l'API)
- Migration depuis anciens systèmes

### 5. Authorization Code Flow with PKCE

Extension du Authorization Code Flow pour les applications publiques (mobiles, SPA).

**PKCE = Proof Key for Code Exchange**

**Ajouts** :
- `code_verifier` : Chaîne aléatoire générée par le client
- `code_challenge` : Hash du code_verifier

**Flux** :

```
1. Client génère code_verifier (chaîne aléatoire)
   code_verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"

2. Client calcule code_challenge
   code_challenge = BASE64URL(SHA256(code_verifier))

3. Requête d'autorisation avec code_challenge
   GET /authorize?
       ...
       &code_challenge=E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM
       &code_challenge_method=S256

4. Échange du code avec code_verifier
   POST /token
   {
     "code": "...",
     "code_verifier": "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
   }
```

## Refresh Tokens

Les access tokens ont une durée de vie courte (souvent 1 heure). Les refresh tokens permettent d'obtenir de nouveaux access tokens sans ré-authentification.

**Flux** :

```
1. Access Token expire

2. Client → Authorization Server
   POST /token
   {
     "grant_type": "refresh_token",
     "refresh_token": "REFRESH_TOKEN",
     "client_id": "ABC123",
     "client_secret": "SECRET"
   }

3. Authorization Server → Client
   {
     "access_token": "NEW_ACCESS_TOKEN",
     "token_type": "Bearer",
     "expires_in": 3600,
     "refresh_token": "NEW_REFRESH_TOKEN"
   }
```

**Exemple** :

```pascal
function RefreshAccessToken(const RefreshToken: string): string;  
var
  HTTP: THTTPSend;
  PostData, Response: string;
begin
  HTTP := THTTPSend.Create;
  try
    PostData :=
      'grant_type=refresh_token' +
      '&refresh_token=' + UrlEncode(RefreshToken) +
      '&client_id=' + CLIENT_ID +
      '&client_secret=' + CLIENT_SECRET;

    HTTP.Document.Write(PostData[1], Length(PostData));
    HTTP.MimeType := 'application/x-www-form-urlencoded';

    if HTTP.HTTPMethod('POST', TOKEN_URL) then
    begin
      SetLength(Response, HTTP.Document.Size);
      HTTP.Document.Read(Response[1], HTTP.Document.Size);
      Result := ExtractAccessToken(Response);
    end;
  finally
    HTTP.Free;
  end;
end;
```

**Bonnes pratiques** :
- ✅ Stocker les refresh tokens de manière sécurisée
- ✅ Rotation des refresh tokens (nouveau à chaque utilisation)
- ✅ Limite de temps pour les refresh tokens
- ❌ Ne jamais exposer les refresh tokens au frontend

## Scopes (Portées)

Les scopes définissent les permissions demandées.

**Exemples de scopes** :
```
read:user         → Lire les informations utilisateur  
write:repos       → Modifier les dépôts  
delete:posts      → Supprimer des posts  
admin:org         → Administration de l'organisation
```

**Dans la requête** :
```
GET /authorize?
    ...
    &scope=read:user write:repos
```

**Vérification côté serveur** :

```pascal
function HasScope(const Token: string; const RequiredScope: string): Boolean;  
var
  TokenScopes: TStringList;
begin
  // Extraire les scopes du token (depuis claims JWT ou base de données)
  TokenScopes := GetTokenScopes(Token);
  try
    Result := TokenScopes.IndexOf(RequiredScope) >= 0;
  finally
    TokenScopes.Free;
  end;
end;

// Utilisation
if not HasScope(AccessToken, 'write:repos') then  
begin
  // HTTP 403 Forbidden
  Response.StatusCode := 403;
  Response.Content := '{"error":"insufficient_scope"}';
  Exit;
end;
```

## Implémentation d'un serveur OAuth 2.0

### Composants nécessaires

**1. Endpoint d'autorisation** (`/authorize`) :
- Authentifie l'utilisateur
- Affiche la page de consentement
- Génère et retourne un authorization code

**2. Endpoint de token** (`/token`) :
- Échange le code contre un access token
- Gère le refresh token
- Gère client credentials

**3. Stockage** :
- Clients OAuth (client_id, client_secret, redirect_uris)
- Authorization codes (temporaires, ~10 minutes)
- Access tokens et Refresh tokens
- User consents (consentements utilisateur)

### Exemple de structure de données

```pascal
type
  TOAuthClient = record
    ClientID: string;
    ClientSecret: string;
    Name: string;
    RedirectURIs: TStringList;
    AllowedScopes: TStringList;
  end;

  TAuthorizationCode = record
    Code: string;
    ClientID: string;
    UserID: Integer;
    RedirectURI: string;
    Scopes: string;
    ExpiresAt: TDateTime;
    CodeChallenge: string;      // Pour PKCE
    CodeChallengeMethod: string; // S256 ou plain
  end;

  TAccessToken = record
    Token: string;
    ClientID: string;
    UserID: Integer;
    Scopes: string;
    ExpiresAt: TDateTime;
  end;

  TRefreshToken = record
    Token: string;
    ClientID: string;
    UserID: Integer;
    Scopes: string;
    ExpiresAt: TDateTime;
  end;
```

### Endpoint /authorize (simplifié)

```pascal
procedure HandleAuthorize(Request: TRequest; Response: TResponse);  
var
  ClientID, RedirectURI, Scope, State, ResponseType: string;
  Client: TOAuthClient;
  UserID: Integer;
  AuthCode: string;
begin
  // 1. Extraire les paramètres
  ClientID := Request.Query['client_id'];
  RedirectURI := Request.Query['redirect_uri'];
  Scope := Request.Query['scope'];
  State := Request.Query['state'];
  ResponseType := Request.Query['response_type'];

  // 2. Valider le client
  Client := GetClient(ClientID);
  if Client.ClientID = '' then
  begin
    Response.StatusCode := 400;
    Response.Content := 'Invalid client_id';
    Exit;
  end;

  // 3. Valider le redirect_uri
  if Client.RedirectURIs.IndexOf(RedirectURI) < 0 then
  begin
    Response.StatusCode := 400;
    Response.Content := 'Invalid redirect_uri';
    Exit;
  end;

  // 4. Authentifier l'utilisateur (si pas déjà connecté)
  UserID := GetAuthenticatedUserID(Request);
  if UserID = 0 then
  begin
    // Rediriger vers la page de login
    Response.Redirect('/login?return=' + UrlEncode(Request.URI));
    Exit;
  end;

  // 5. Vérifier le consentement (ou afficher la page de consentement)
  if not HasUserConsent(UserID, ClientID, Scope) then
  begin
    // Afficher la page de consentement
    ShowConsentPage(Response, Client, Scope);
    Exit;
  end;

  // 6. Générer un authorization code
  AuthCode := GenerateAuthorizationCode(ClientID, UserID, RedirectURI, Scope);

  // 7. Rediriger vers le client avec le code
  Response.Redirect(RedirectURI + '?code=' + AuthCode + '&state=' + State);
end;
```

### Endpoint /token (simplifié)

```pascal
procedure HandleToken(Request: TRequest; Response: TResponse);  
var
  GrantType, Code, ClientID, ClientSecret: string;
  AccessToken, RefreshToken: string;
begin
  // 1. Extraire les paramètres
  GrantType := Request.Post['grant_type'];

  if GrantType = 'authorization_code' then
  begin
    Code := Request.Post['code'];
    ClientID := Request.Post['client_id'];
    ClientSecret := Request.Post['client_secret'];

    // Vérifier le client
    if not ValidateClient(ClientID, ClientSecret) then
    begin
      Response.StatusCode := 401;
      Response.Content := '{"error":"invalid_client"}';
      Exit;
    end;

    // Vérifier le code
    if not ValidateAuthorizationCode(Code, ClientID) then
    begin
      Response.StatusCode := 400;
      Response.Content := '{"error":"invalid_grant"}';
      Exit;
    end;

    // Générer les tokens
    AccessToken := GenerateAccessToken(ClientID, GetUserIDFromCode(Code));
    RefreshToken := GenerateRefreshToken(ClientID, GetUserIDFromCode(Code));

    // Invalider le code
    InvalidateAuthorizationCode(Code);

    // Retourner les tokens
    Response.ContentType := 'application/json';
    Response.Content := Format(
      '{"access_token":"%s","token_type":"Bearer","expires_in":3600,"refresh_token":"%s"}',
      [AccessToken, RefreshToken]
    );
  end
  else if GrantType = 'refresh_token' then
  begin
    // Gérer le refresh token
    // ...
  end
  else if GrantType = 'client_credentials' then
  begin
    // Gérer client credentials
    // ...
  end
  else
  begin
    Response.StatusCode := 400;
    Response.Content := '{"error":"unsupported_grant_type"}';
  end;
end;
```

## OpenID Connect (OIDC)

OpenID Connect est une couche d'authentification construite au-dessus d'OAuth 2.0.

### Différence OAuth 2.0 vs OpenID Connect

**OAuth 2.0** :
- Protocole d'autorisation
- "Que peut faire l'application ?"
- Ne définit pas comment obtenir les infos utilisateur

**OpenID Connect** :
- Protocole d'authentification
- "Qui est l'utilisateur ?"
- Ajoute un ID Token (JWT) avec les infos utilisateur

### ID Token

En plus de l'access token, OIDC retourne un ID Token :

```json
{
  "access_token": "ACCESS_TOKEN",
  "token_type": "Bearer",
  "expires_in": 3600,
  "id_token": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9..."
}
```

**Contenu de l'ID Token** :
```json
{
  "iss": "https://accounts.google.com",
  "sub": "110169484474386276334",
  "aud": "votre_client_id",
  "exp": 1516242622,
  "iat": 1516239022,
  "email": "user@example.com",
  "email_verified": true,
  "name": "John Doe",
  "picture": "https://..."
}
```

### Scopes OpenID Connect

**Scopes standards** :
- `openid` (obligatoire) : Active OIDC
- `profile` : Nom, photo, etc.
- `email` : Adresse email
- `address` : Adresse postale
- `phone` : Numéro de téléphone

**Exemple de requête** :
```
GET /authorize?
    ...
    &scope=openid profile email
```

## Intégration avec des providers populaires

### Google OAuth 2.0

**1. Créer un projet dans Google Cloud Console** :
- https://console.cloud.google.com/
- Créer un projet
- Activer l'API Google+ ou People API
- Créer des identifiants OAuth 2.0

**2. Configuration** :

```pascal
const
  GOOGLE_CLIENT_ID = 'votre_client_id.apps.googleusercontent.com';
  GOOGLE_CLIENT_SECRET = 'votre_secret';
  GOOGLE_REDIRECT_URI = 'http://localhost:8080/callback';
  GOOGLE_AUTH_URL = 'https://accounts.google.com/o/oauth2/v2/auth';
  GOOGLE_TOKEN_URL = 'https://oauth2.googleapis.com/token';
  GOOGLE_USERINFO_URL = 'https://www.googleapis.com/oauth2/v2/userinfo';
```

**3. Flux complet** :

```pascal
// Étape 1 : Rediriger vers Google
function GetGoogleAuthURL: string;  
begin
  Result := GOOGLE_AUTH_URL + '?' +
    'client_id=' + GOOGLE_CLIENT_ID +
    '&redirect_uri=' + UrlEncode(GOOGLE_REDIRECT_URI) +
    '&response_type=code' +
    '&scope=' + UrlEncode('openid profile email');
end;

// Étape 2 : Callback - Échanger le code
procedure HandleCallback(const Code: string);  
var
  AccessToken: string;
  UserInfo: string;
begin
  // Échanger le code contre un token
  AccessToken := ExchangeCodeForToken(Code);

  // Obtenir les infos utilisateur
  UserInfo := GetUserInfo(AccessToken);

  WriteLn('Informations utilisateur : ', UserInfo);
  // {"email":"user@gmail.com","name":"John Doe","picture":"https://..."}
end;

function GetUserInfo(const AccessToken: string): string;  
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.Headers.Add('Authorization: Bearer ' + AccessToken);

    if HTTP.HTTPMethod('GET', GOOGLE_USERINFO_URL) then
    begin
      SetLength(Result, HTTP.Document.Size);
      HTTP.Document.Position := 0;
      HTTP.Document.Read(Result[1], HTTP.Document.Size);
    end;
  finally
    HTTP.Free;
  end;
end;
```

### GitHub OAuth 2.0

**1. Créer une OAuth App sur GitHub** :
- https://github.com/settings/developers
- New OAuth App
- Définir Authorization callback URL

**2. Configuration** :

```pascal
const
  GITHUB_CLIENT_ID = 'votre_client_id';
  GITHUB_CLIENT_SECRET = 'votre_secret';
  GITHUB_REDIRECT_URI = 'http://localhost:8080/callback';
  GITHUB_AUTH_URL = 'https://github.com/login/oauth/authorize';
  GITHUB_TOKEN_URL = 'https://github.com/login/oauth/access_token';
  GITHUB_API_URL = 'https://api.github.com/user';

function GetGitHubAuthURL: string;  
begin
  Result := GITHUB_AUTH_URL + '?' +
    'client_id=' + GITHUB_CLIENT_ID +
    '&redirect_uri=' + UrlEncode(GITHUB_REDIRECT_URI) +
    '&scope=' + UrlEncode('read:user user:email');
end;

function GetGitHubUserInfo(const AccessToken: string): string;  
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.Headers.Add('Authorization: Bearer ' + AccessToken);
    HTTP.Headers.Add('Accept: application/vnd.github.v3+json');
    HTTP.Headers.Add('User-Agent: MonApplication');

    if HTTP.HTTPMethod('GET', GITHUB_API_URL) then
    begin
      SetLength(Result, HTTP.Document.Size);
      HTTP.Document.Read(Result[1], HTTP.Document.Size);
    end;
  finally
    HTTP.Free;
  end;
end;
```

### Microsoft OAuth 2.0 (Azure AD)

```pascal
const
  MS_CLIENT_ID = 'votre_client_id';
  MS_CLIENT_SECRET = 'votre_secret';
  MS_TENANT_ID = 'common'; // ou votre tenant ID
  MS_REDIRECT_URI = 'http://localhost:8080/callback';
  MS_AUTH_URL = 'https://login.microsoftonline.com/' + MS_TENANT_ID + '/oauth2/v2.0/authorize';
  MS_TOKEN_URL = 'https://login.microsoftonline.com/' + MS_TENANT_ID + '/oauth2/v2.0/token';
  MS_GRAPH_URL = 'https://graph.microsoft.com/v1.0/me';

function GetMicrosoftAuthURL: string;  
begin
  Result := MS_AUTH_URL + '?' +
    'client_id=' + MS_CLIENT_ID +
    '&response_type=code' +
    '&redirect_uri=' + UrlEncode(MS_REDIRECT_URI) +
    '&scope=' + UrlEncode('openid profile email User.Read');
end;
```

### Facebook OAuth 2.0

```pascal
const
  FB_APP_ID = 'votre_app_id';
  FB_APP_SECRET = 'votre_secret';
  FB_REDIRECT_URI = 'http://localhost:8080/callback';
  FB_AUTH_URL = 'https://www.facebook.com/v18.0/dialog/oauth';
  FB_TOKEN_URL = 'https://graph.facebook.com/v18.0/oauth/access_token';
  FB_GRAPH_URL = 'https://graph.facebook.com/v18.0/me';

function GetFacebookAuthURL: string;  
begin
  Result := FB_AUTH_URL + '?' +
    'client_id=' + FB_APP_ID +
    '&redirect_uri=' + UrlEncode(FB_REDIRECT_URI) +
    '&scope=' + UrlEncode('email public_profile');
end;

function GetFacebookUserInfo(const AccessToken: string): string;  
var
  HTTP: THTTPSend;
  URL: string;
begin
  URL := FB_GRAPH_URL + '?fields=id,name,email,picture&access_token=' + AccessToken;

  HTTP := THTTPSend.Create;
  try
    if HTTP.HTTPMethod('GET', URL) then
    begin
      SetLength(Result, HTTP.Document.Size);
      HTTP.Document.Read(Result[1], HTTP.Document.Size);
    end;
  finally
    HTTP.Free;
  end;
end;
```

## Bibliothèques OAuth pour FreePascal

### Utiliser fpjson pour parser les réponses

```pascal
uses
  fpjson, jsonparser;

type
  TTokenResponse = record
    AccessToken: string;
    TokenType: string;
    ExpiresIn: Integer;
    RefreshToken: string;
    Scope: string;
  end;

function ParseTokenResponse(const JSONResponse: string): TTokenResponse;  
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
begin
  JSONData := GetJSON(JSONResponse);
  try
    if JSONData is TJSONObject then
    begin
      JSONObject := TJSONObject(JSONData);

      Result.AccessToken := JSONObject.Get('access_token', '');
      Result.TokenType := JSONObject.Get('token_type', 'Bearer');
      Result.ExpiresIn := JSONObject.Get('expires_in', 3600);
      Result.RefreshToken := JSONObject.Get('refresh_token', '');
      Result.Scope := JSONObject.Get('scope', '');
    end;
  finally
    JSONData.Free;
  end;
end;

// Utilisation
var
  Response: string;
  TokenData: TTokenResponse;
begin
  Response := '{"access_token":"abc123","token_type":"Bearer","expires_in":3600}';
  TokenData := ParseTokenResponse(Response);
  WriteLn('Access Token: ', TokenData.AccessToken);
end;
```

### Parser les informations utilisateur

```pascal
type
  TUserInfo = record
    ID: string;
    Email: string;
    Name: string;
    Picture: string;
    EmailVerified: Boolean;
  end;

function ParseUserInfo(const JSONResponse: string): TUserInfo;  
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
begin
  JSONData := GetJSON(JSONResponse);
  try
    if JSONData is TJSONObject then
    begin
      JSONObject := TJSONObject(JSONData);

      Result.ID := JSONObject.Get('id', '');
      Result.Email := JSONObject.Get('email', '');
      Result.Name := JSONObject.Get('name', '');
      Result.Picture := JSONObject.Get('picture', '');
      Result.EmailVerified := JSONObject.Get('email_verified', False);
    end;
  finally
    JSONData.Free;
  end;
end;
```

## Stockage sécurisé des tokens

### Ne jamais stocker en clair

```pascal
// ❌ DANGEREUX - Ne JAMAIS faire
procedure BadTokenStorage(const Token: string);  
begin
  WriteToFile('token.txt', Token); // Token en clair !
end;

// ✅ CORRECT - Chiffrer avant stockage
procedure SecureTokenStorage(const Token: string);  
var
  EncryptedToken: string;
begin
  EncryptedToken := EncryptAES256(Token, GetMasterKey);
  WriteToFile('token.enc', EncryptedToken);
end;
```

### Utiliser les mécanismes du système d'exploitation

#### Windows : Data Protection API (DPAPI)

```pascal
{$IFDEF WINDOWS}
uses
  Windows, WinCrypt;

function ProtectData(const Data: string): string;  
var
  DataIn, DataOut: DATA_BLOB;
  DataBytes: TBytes;
begin
  // Convertir la chaîne en bytes
  DataBytes := TEncoding.UTF8.GetBytes(Data);

  DataIn.cbData := Length(DataBytes);
  DataIn.pbData := @DataBytes[0];

  if CryptProtectData(@DataIn, nil, nil, nil, nil, 0, @DataOut) then
  begin
    SetLength(DataBytes, DataOut.cbData);
    Move(DataOut.pbData^, DataBytes[0], DataOut.cbData);
    Result := EncodeBase64(DataBytes);
    LocalFree(HLOCAL(DataOut.pbData));
  end;
end;

function UnprotectData(const EncryptedData: string): string;  
var
  DataIn, DataOut: DATA_BLOB;
  DataBytes: TBytes;
begin
  DataBytes := DecodeBase64(EncryptedData);

  DataIn.cbData := Length(DataBytes);
  DataIn.pbData := @DataBytes[0];

  if CryptUnprotectData(@DataIn, nil, nil, nil, nil, 0, @DataOut) then
  begin
    SetLength(Result, DataOut.cbData);
    Move(DataOut.pbData^, Result[1], DataOut.cbData);
    LocalFree(HLOCAL(DataOut.pbData));
  end;
end;
{$ENDIF}
```

#### Linux : Secret Service (libsecret)

```pascal
{$IFDEF UNIX}
// Utiliser libsecret via DBus ou l'outil secret-tool

procedure StoreTokenLinux(const Service, Account, Token: string);  
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'secret-tool';
    Process.Parameters.Add('store');
    Process.Parameters.Add('--label=' + Service);
    Process.Parameters.Add('service');
    Process.Parameters.Add(Service);
    Process.Parameters.Add('account');
    Process.Parameters.Add(Account);

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Process.Input.Write(Token[1], Length(Token));
    Process.CloseInput;
  finally
    Process.Free;
  end;
end;

function RetrieveTokenLinux(const Service, Account: string): string;  
var
  Process: TProcess;
  Buffer: string;
  BytesRead: Integer;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'secret-tool';
    Process.Parameters.Add('lookup');
    Process.Parameters.Add('service');
    Process.Parameters.Add(Service);
    Process.Parameters.Add('account');
    Process.Parameters.Add(Account);

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    SetLength(Buffer, 4096);
    BytesRead := Process.Output.Read(Buffer[1], Length(Buffer));
    SetLength(Buffer, BytesRead);
    Result := Trim(Buffer);
  finally
    Process.Free;
  end;
end;
{$ENDIF}
```

### Abstraction multi-plateforme

```pascal
type
  TSecureStorage = class
  public
    class procedure StoreToken(const Key, Token: string);
    class function RetrieveToken(const Key: string): string;
    class procedure DeleteToken(const Key: string);
  end;

class procedure TSecureStorage.StoreToken(const Key, Token: string);  
begin
  {$IFDEF WINDOWS}
    WriteToFile(Key + '.dat', ProtectData(Token));
  {$ENDIF}
  {$IFDEF UNIX}
    StoreTokenLinux('MonApplication', Key, Token);
  {$ENDIF}
end;

class function TSecureStorage.RetrieveToken(const Key: string): string;  
begin
  {$IFDEF WINDOWS}
    Result := UnprotectData(ReadFromFile(Key + '.dat'));
  {$ENDIF}
  {$IFDEF UNIX}
    Result := RetrieveTokenLinux('MonApplication', Key);
  {$ENDIF}
end;

// Utilisation
begin
  TSecureStorage.StoreToken('google_access_token', AccessToken);
  // Plus tard...
  AccessToken := TSecureStorage.RetrieveToken('google_access_token');
end;
```

## Gestion de la session utilisateur

### Créer une session après authentification OAuth

```pascal
type
  TUserSession = record
    SessionID: string;
    UserID: Integer;
    Email: string;
    Name: string;
    AccessToken: string;
    RefreshToken: string;
    TokenExpiresAt: TDateTime;
    CreatedAt: TDateTime;
  end;

function CreateSessionFromOAuth(const UserInfo: TUserInfo;
                                 const TokenData: TTokenResponse): TUserSession;
begin
  Result.SessionID := GenerateRandomToken(32);
  Result.UserID := GetOrCreateUser(UserInfo); // Cherche ou crée l'utilisateur
  Result.Email := UserInfo.Email;
  Result.Name := UserInfo.Name;
  Result.AccessToken := TokenData.AccessToken;
  Result.RefreshToken := TokenData.RefreshToken;
  Result.TokenExpiresAt := Now + (TokenData.ExpiresIn / 86400); // Secondes en jours
  Result.CreatedAt := Now;

  // Stocker la session (base de données, Redis, etc.)
  SaveSession(Result);
end;

function GetOrCreateUser(const UserInfo: TUserInfo): Integer;  
var
  User: TUser;
begin
  // Chercher l'utilisateur par email
  User := FindUserByEmail(UserInfo.Email);

  if User.ID = 0 then
  begin
    // Créer un nouveau utilisateur
    User.Email := UserInfo.Email;
    User.Name := UserInfo.Name;
    User.Picture := UserInfo.Picture;
    User.EmailVerified := UserInfo.EmailVerified;
    User.AuthProvider := 'google'; // ou 'github', 'facebook', etc.
    User.AuthProviderID := UserInfo.ID;
    User.CreatedAt := Now;

    Result := CreateUser(User);
  end
  else
    Result := User.ID;
end;
```

### Middleware de vérification de session

```pascal
function RequireAuth(Request: TRequest; Response: TResponse): Boolean;  
var
  SessionID: string;
  Session: TUserSession;
begin
  Result := False;

  // Extraire le session ID du cookie ou header
  SessionID := Request.Cookies['session_id'];

  if SessionID = '' then
  begin
    Response.StatusCode := 401;
    Response.Content := '{"error":"authentication_required"}';
    Exit;
  end;

  // Vérifier la session
  Session := GetSession(SessionID);

  if Session.SessionID = '' then
  begin
    Response.StatusCode := 401;
    Response.Content := '{"error":"invalid_session"}';
    Exit;
  end;

  // Vérifier l'expiration du token
  if Now > Session.TokenExpiresAt then
  begin
    // Tenter de rafraîchir le token
    if not RefreshUserToken(Session) then
    begin
      Response.StatusCode := 401;
      Response.Content := '{"error":"token_expired"}';
      Exit;
    end;
  end;

  // Attacher l'utilisateur à la requête
  Request.User := Session;
  Result := True;
end;

// Utilisation
procedure HandleProtectedEndpoint(Request: TRequest; Response: TResponse);  
begin
  if not RequireAuth(Request, Response) then
    Exit;

  // L'utilisateur est authentifié
  Response.Content := 'Hello, ' + Request.User.Name;
end;
```

## Single Sign-On (SSO)

### Qu'est-ce que le SSO ?

Le Single Sign-On permet à un utilisateur de s'authentifier une seule fois pour accéder à plusieurs applications.

**Exemple** :
```
Utilisateur se connecte à Google
    ↓
Accède à Gmail (pas de nouvelle connexion)
    ↓
Accède à YouTube (pas de nouvelle connexion)
    ↓
Accède à Google Drive (pas de nouvelle connexion)
```

### Implémentation SSO avec OAuth 2.0

```pascal
// Application 1 : Serveur d'authentification principal
procedure HandleLogin(Request: TRequest; Response: TResponse);  
var
  Session: TUserSession;
  SSOToken: string;
begin
  // Authentifier l'utilisateur
  Session := AuthenticateUser(Request.Post['email'], Request.Post['password']);

  // Créer un token SSO
  SSOToken := CreateSSOToken(Session.UserID);

  // Stocker en cookie sur le domaine principal (.example.com)
  Response.SetCookie('sso_token', SSOToken, '.example.com',
                     Now + 7, // 7 jours
                     True,    // HttpOnly
                     True);   // Secure

  Response.Redirect('/dashboard');
end;

// Application 2 : Vérifier le SSO
procedure CheckSSO(Request: TRequest; Response: TResponse);  
var
  SSOToken: string;
  UserID: Integer;
begin
  SSOToken := Request.Cookies['sso_token'];

  if SSOToken <> '' then
  begin
    UserID := ValidateSSOToken(SSOToken);

    if UserID > 0 then
    begin
      // Créer une session locale
      CreateLocalSession(UserID);
      Response.Redirect('/app');
      Exit;
    end;
  end;

  // Pas de SSO valide, rediriger vers login
  Response.Redirect('https://auth.example.com/login?return=' +
                    UrlEncode(Request.URI));
end;
```

### SAML (Security Assertion Markup Language)

SAML est un autre protocole SSO, souvent utilisé en entreprise.

**Différences SAML vs OAuth 2.0** :

| Caractéristique | SAML | OAuth 2.0 |
|-----------------|------|-----------|
| Format | XML | JSON |
| Cas d'usage | Enterprise SSO | API Authorization |
| Complexité | Plus complexe | Plus simple |
| Mobile | Moins adapté | Bien adapté |

**Note** : Pour FreePascal, OAuth 2.0 est généralement plus facile à implémenter que SAML.

## Sécurité OAuth 2.0

### Attaques courantes et protections

#### 1. CSRF (Cross-Site Request Forgery)

**Attaque** : Un attaquant force un utilisateur à exécuter une action non désirée.

**Protection** : Paramètre `state`

```pascal
// Génération du state
function GenerateState: string;  
begin
  Result := GenerateRandomToken(32);
  // Stocker en session
  StoreInSession('oauth_state', Result);
end;

// Vérification
function ValidateState(const ReceivedState: string): Boolean;  
var
  StoredState: string;
begin
  StoredState := GetFromSession('oauth_state');
  Result := (ReceivedState = StoredState) and (StoredState <> '');

  // Invalider après utilisation (one-time use)
  DeleteFromSession('oauth_state');
end;

// Utilisation
AuthURL := GetAuthURL + '&state=' + GenerateState;

// Dans le callback
if not ValidateState(Request.Query['state']) then  
begin
  Response.StatusCode := 400;
  Response.Content := 'Invalid state parameter';
  Exit;
end;
```

#### 2. Authorization Code Interception

**Attaque** : Un attaquant intercepte le code d'autorisation.

**Protection** : PKCE (Proof Key for Code Exchange)

```pascal
// Génération du code_verifier et code_challenge
function GeneratePKCE(out CodeVerifier, CodeChallenge: string);  
var
  RandomBytes: TBytes;
  Hash: TBytes;
begin
  // Générer code_verifier (43-128 caractères)
  SetLength(RandomBytes, 32);
  RandomFillBytes(RandomBytes);
  CodeVerifier := Base64URLEncode(RandomBytes);

  // Calculer code_challenge = BASE64URL(SHA256(code_verifier))
  Hash := SHA256(CodeVerifier);
  CodeChallenge := Base64URLEncode(Hash);
end;

// Utilisation
var
  CodeVerifier, CodeChallenge: string;
begin
  GeneratePKCE(CodeVerifier, CodeChallenge);

  // Stocker le code_verifier
  StoreInSession('code_verifier', CodeVerifier);

  // Ajouter à l'URL d'autorisation
  AuthURL := AuthURL +
    '&code_challenge=' + CodeChallenge +
    '&code_challenge_method=S256';

  // Plus tard, dans l'échange du code
  CodeVerifier := GetFromSession('code_verifier');
  // Envoyer code_verifier avec la requête de token
end;
```

#### 3. Token Theft

**Protection** :
- Utiliser HTTPS partout
- HttpOnly cookies
- Courte durée de vie des access tokens
- Rotation des refresh tokens

```pascal
// Configurer un cookie sécurisé
procedure SetSecureCookie(Response: TResponse; const Name, Value: string);  
begin
  Response.SetCookie(
    Name,
    Value,
    '',           // Path (vide = /)
    Now + 7,      // Expires (7 jours)
    True,         // HttpOnly (pas accessible en JavaScript)
    True,         // Secure (HTTPS uniquement)
    'Strict'      // SameSite
  );
end;
```

#### 4. Open Redirect

**Attaque** : Rediriger vers un site malveillant après authentification.

**Protection** : Valider le redirect_uri

```pascal
function ValidateRedirectURI(const ClientID, RedirectURI: string): Boolean;  
var
  Client: TOAuthClient;
begin
  Result := False;

  Client := GetClient(ClientID);
  if Client.ClientID = '' then Exit;

  // Vérifier que le redirect_uri est dans la liste autorisée
  Result := Client.RedirectURIs.IndexOf(RedirectURI) >= 0;
end;

// Ne JAMAIS accepter un redirect_uri arbitraire
if not ValidateRedirectURI(ClientID, RedirectURI) then  
begin
  Response.StatusCode := 400;
  Response.Content := 'Invalid redirect_uri';
  Exit;
end;
```

### Bonnes pratiques de sécurité

**1. Toujours utiliser HTTPS** :
```pascal
// Vérifier que la requête est en HTTPS
if Request.Protocol <> 'https' then  
begin
  Response.StatusCode := 400;
  Response.Content := 'HTTPS required';
  Exit;
end;
```

**2. Valider tous les paramètres** :
```pascal
// Exemple de validation
if (Length(Code) < 10) or (Length(Code) > 512) then  
begin
  Response.StatusCode := 400;
  Response.Content := '{"error":"invalid_request"}';
  Exit;
end;
```

**3. Limiter les tentatives** :
```pascal
// Rate limiting
if GetFailedAttempts(ClientID) > 10 then  
begin
  Response.StatusCode := 429;
  Response.Content := '{"error":"too_many_requests"}';
  Response.Headers.Add('Retry-After: 3600');
  Exit;
end;
```

**4. Logger les événements de sécurité** :
```pascal
procedure LogSecurityEvent(const Event: string; const Details: string);  
begin
  WriteToLog(Format('[SECURITY] %s - %s - IP: %s - Time: %s',
    [Event, Details, GetClientIP, DateTimeToStr(Now)]));
end;

// Utilisation
LogSecurityEvent('INVALID_TOKEN', 'Token: ' + Token);  
LogSecurityEvent('LOGIN_FAILED', 'User: ' + Email);  
LogSecurityEvent('TOKEN_REFRESH', 'User: ' + UserID);
```

## Testing et débogage OAuth 2.0

### Outils de test

**1. Postman** :
- Configurez des collections pour tester vos endpoints
- Supporte OAuth 2.0 nativement

**2. OAuth Debugger** :
- https://oauthdebugger.com/
- Permet de tester le flux OAuth sans écrire de code

**3. JWT.io** :
- https://jwt.io/
- Décode et vérifie les JWT

### Logs de débogage

```pascal
procedure DebugOAuthFlow(const Step: string; const Data: string);  
begin
  {$IFDEF DEBUG}
  WriteLn('=== OAuth Debug ===');
  WriteLn('Step: ', Step);
  WriteLn('Data: ', Data);
  WriteLn('Time: ', DateTimeToStr(Now));
  WriteLn('==================');
  {$ENDIF}
end;

// Utilisation
DebugOAuthFlow('AUTH_REQUEST', 'URL: ' + AuthURL);  
DebugOAuthFlow('CALLBACK', 'Code: ' + Code);  
DebugOAuthFlow('TOKEN_RESPONSE', TokenJSON);
```

### Environnements de test

**Créer un environnement de dev** :
```pascal
const
  {$IFDEF DEBUG}
  OAUTH_CLIENT_ID = 'test_client_id';
  OAUTH_CLIENT_SECRET = 'test_secret';
  OAUTH_REDIRECT_URI = 'http://localhost:8080/callback';
  {$ELSE}
  OAUTH_CLIENT_ID = 'production_client_id';
  OAUTH_CLIENT_SECRET = 'production_secret';
  OAUTH_REDIRECT_URI = 'https://app.example.com/callback';
  {$ENDIF}
```

## Exemple complet d'application

### Application console OAuth 2.0

```pascal
program OAuthExample;

{$mode objfpc}{$H+}

uses
  SysUtils, httpsend, ssl_openssl, fpjson, jsonparser, process;

const
  CLIENT_ID = 'votre_client_id';
  CLIENT_SECRET = 'votre_secret';
  REDIRECT_URI = 'http://localhost:8080';
  AUTH_URL = 'https://github.com/login/oauth/authorize';
  TOKEN_URL = 'https://github.com/login/oauth/access_token';
  API_URL = 'https://api.github.com/user';

function GetAuthURL: string;  
begin
  Result := AUTH_URL + '?client_id=' + CLIENT_ID +
    '&redirect_uri=' + REDIRECT_URI +
    '&scope=read:user';
  WriteLn('URL d''autorisation:');
  WriteLn(Result);
  WriteLn;
end;

function ExchangeCode(const Code: string): string;  
var
  HTTP: THTTPSend;
  PostData, Response: string;
  JSON: TJSONData;
begin
  HTTP := THTTPSend.Create;
  try
    PostData := 'client_id=' + CLIENT_ID +
      '&client_secret=' + CLIENT_SECRET +
      '&code=' + Code +
      '&redirect_uri=' + REDIRECT_URI;

    HTTP.Document.Write(PostData[1], Length(PostData));
    HTTP.MimeType := 'application/x-www-form-urlencoded';
    HTTP.Headers.Add('Accept: application/json');

    if HTTP.HTTPMethod('POST', TOKEN_URL) then
    begin
      SetLength(Response, HTTP.Document.Size);
      HTTP.Document.Position := 0;
      HTTP.Document.Read(Response[1], HTTP.Document.Size);

      JSON := GetJSON(Response);
      try
        Result := TJSONObject(JSON).Get('access_token', '');
      finally
        JSON.Free;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

function GetUserInfo(const Token: string): string;  
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.Headers.Add('Authorization: Bearer ' + Token);
    HTTP.Headers.Add('User-Agent: OAuth-Example');

    if HTTP.HTTPMethod('GET', API_URL) then
    begin
      SetLength(Result, HTTP.Document.Size);
      HTTP.Document.Read(Result[1], HTTP.Document.Size);
    end;
  finally
    HTTP.Free;
  end;
end;

var
  AuthURL, Code, Token, UserInfo: string;
begin
  WriteLn('=== Exemple OAuth 2.0 avec GitHub ===');
  WriteLn;

  // Étape 1: Obtenir l'URL d'autorisation
  AuthURL := GetAuthURL;
  WriteLn('Ouvrez cette URL dans votre navigateur.');
  WriteLn('Après autorisation, copiez le code depuis l''URL de callback.');
  WriteLn;

  // Étape 2: Demander le code
  Write('Entrez le code: ');
  ReadLn(Code);

  // Étape 3: Échanger le code contre un token
  WriteLn('Échange du code contre un token...');
  Token := ExchangeCode(Code);

  if Token <> '' then
  begin
    WriteLn('Token obtenu: ', Copy(Token, 1, 20), '...');
    WriteLn;

    // Étape 4: Utiliser le token pour accéder à l'API
    WriteLn('Récupération des informations utilisateur...');
    UserInfo := GetUserInfo(Token);
    WriteLn('Informations utilisateur:');
    WriteLn(UserInfo);
  end
  else
    WriteLn('Erreur: Impossible d''obtenir le token');

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Serveur web local pour callback

Pour améliorer l'expérience utilisateur, créons un petit serveur web qui capture automatiquement le code de callback :

```pascal
program OAuthWebServer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpserver, httpdefs, fpjson, jsonparser,
  httpsend, ssl_openssl;

type
  TOAuthServer = class(TFPHTTPServer)
  private
    FCode: string;
    FToken: string;
    FUserInfo: string;
  public
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
    property Code: string read FCode;
    property Token: string read FToken;
    property UserInfo: string read FUserInfo;
  end;

const
  CLIENT_ID = 'votre_client_id';
  CLIENT_SECRET = 'votre_secret';
  REDIRECT_URI = 'http://localhost:8080/callback';
  AUTH_URL = 'https://github.com/login/oauth/authorize';
  TOKEN_URL = 'https://github.com/login/oauth/access_token';
  API_URL = 'https://api.github.com/user';

function ExchangeCodeForToken(const Code: string): string;  
var
  HTTP: THTTPSend;
  PostData, Response: string;
  JSON: TJSONData;
begin
  HTTP := THTTPSend.Create;
  try
    PostData := Format('client_id=%s&client_secret=%s&code=%s',
      [CLIENT_ID, CLIENT_SECRET, Code]);

    HTTP.Document.Write(PostData[1], Length(PostData));
    HTTP.MimeType := 'application/x-www-form-urlencoded';
    HTTP.Headers.Add('Accept: application/json');

    if HTTP.HTTPMethod('POST', TOKEN_URL) then
    begin
      SetLength(Response, HTTP.Document.Size);
      HTTP.Document.Position := 0;
      HTTP.Document.Read(Response[1], HTTP.Document.Size);

      JSON := GetJSON(Response);
      try
        Result := TJSONObject(JSON).Get('access_token', '');
      finally
        JSON.Free;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

function GetUserInfo(const Token: string): string;  
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.Headers.Add('Authorization: Bearer ' + Token);
    HTTP.Headers.Add('User-Agent: OAuth-Server');

    if HTTP.HTTPMethod('GET', API_URL) then
    begin
      SetLength(Result, HTTP.Document.Size);
      HTTP.Document.Read(Result[1], HTTP.Document.Size);
    end;
  finally
    HTTP.Free;
  end;
end;

procedure TOAuthServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  JSON: TJSONData;
  Name, Login, Email: string;
begin
  if ARequest.URI = '/' then
  begin
    // Page d'accueil avec bouton de connexion
    AResponse.Content :=
      '<html><body>' +
      '<h1>OAuth 2.0 Example</h1>' +
      '<a href="' + AUTH_URL + '?client_id=' + CLIENT_ID +
      '&redirect_uri=' + REDIRECT_URI + '&scope=read:user">' +
      '<button>Se connecter avec GitHub</button></a>' +
      '</body></html>';
  end
  else if Copy(ARequest.URI, 1, 9) = '/callback' then
  begin
    // Callback OAuth
    FCode := ARequest.QueryFields.Values['code'];

    if FCode <> '' then
    begin
      WriteLn('Code reçu: ', FCode);

      // Échanger le code contre un token
      FToken := ExchangeCodeForToken(FCode);

      if FToken <> '' then
      begin
        WriteLn('Token obtenu: ', Copy(FToken, 1, 20), '...');

        // Récupérer les infos utilisateur
        FUserInfo := GetUserInfo(FToken);

        // Parser les infos pour l'affichage
        JSON := GetJSON(FUserInfo);
        try
          Login := TJSONObject(JSON).Get('login', 'Inconnu');
          Name := TJSONObject(JSON).Get('name', 'Inconnu');
          Email := TJSONObject(JSON).Get('email', 'Non disponible');
        finally
          JSON.Free;
        end;

        AResponse.Content :=
          '<html><body>' +
          '<h1>Authentification réussie !</h1>' +
          '<p><strong>Login:</strong> ' + Login + '</p>' +
          '<p><strong>Nom:</strong> ' + Name + '</p>' +
          '<p><strong>Email:</strong> ' + Email + '</p>' +
          '<p><a href="/">Retour</a></p>' +
          '</body></html>';
      end
      else
      begin
        AResponse.Content := '<html><body><h1>Erreur lors de l''échange du code</h1></body></html>';
      end;
    end
    else
    begin
      AResponse.Content := '<html><body><h1>Code manquant</h1></body></html>';
    end;
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := '<html><body><h1>404 Not Found</h1></body></html>';
  end;
end;

var
  Server: TOAuthServer;
begin
  Server := TOAuthServer.Create(nil);
  try
    Server.Port := 8080;
    Server.Active := True;

    WriteLn('Serveur démarré sur http://localhost:8080');
    WriteLn('Ouvrez votre navigateur sur cette adresse.');
    WriteLn('Appuyez sur Entrée pour arrêter le serveur...');
    ReadLn;
  finally
    Server.Free;
  end;
end.
```

## Application Lazarus avec interface graphique

### Formulaire principal

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  httpsend, ssl_openssl, fpjson, jsonparser;

type
  TFormMain = class(TForm)
    BtnLogin: TButton;
    BtnLogout: TButton;
    LabelStatus: TLabel;
    PanelInfo: TPanel;
    LabelName: TLabel;
    LabelEmail: TLabel;
    ImageProfile: TImage;
    MemoToken: TMemo;
    procedure BtnLoginClick(Sender: TObject);
    procedure BtnLogoutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAccessToken: string;
    FRefreshToken: string;
    procedure StartOAuthFlow;
    procedure HandleOAuthCallback(const Code: string);
    procedure ExchangeCodeForToken(const Code: string);
    procedure LoadUserInfo;
    procedure UpdateUI(LoggedIn: Boolean);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

const
  CLIENT_ID = 'votre_client_id';
  CLIENT_SECRET = 'votre_secret';
  REDIRECT_URI = 'http://localhost:8080/callback';
  AUTH_URL = 'https://accounts.google.com/o/oauth2/v2/auth';
  TOKEN_URL = 'https://oauth2.googleapis.com/token';
  USERINFO_URL = 'https://www.googleapis.com/oauth2/v2/userinfo';

procedure TFormMain.FormCreate(Sender: TObject);  
begin
  UpdateUI(False);

  // Charger le token depuis le stockage sécurisé si disponible
  FAccessToken := LoadTokenFromSecureStorage('google_access_token');
  if FAccessToken <> '' then
  begin
    LoadUserInfo;
    UpdateUI(True);
  end;
end;

procedure TFormMain.BtnLoginClick(Sender: TObject);  
begin
  StartOAuthFlow;
end;

procedure TFormMain.BtnLogoutClick(Sender: TObject);  
begin
  FAccessToken := '';
  FRefreshToken := '';
  DeleteTokenFromSecureStorage('google_access_token');
  DeleteTokenFromSecureStorage('google_refresh_token');
  UpdateUI(False);
end;

procedure TFormMain.StartOAuthFlow;  
var
  AuthURL: string;
  State: string;
begin
  // Générer un state pour la protection CSRF
  State := GenerateRandomString(32);
  SaveToSession('oauth_state', State);

  // Construire l'URL d'autorisation
  AuthURL := AUTH_URL + '?' +
    'client_id=' + CLIENT_ID +
    '&redirect_uri=' + UrlEncode(REDIRECT_URI) +
    '&response_type=code' +
    '&scope=' + UrlEncode('openid profile email') +
    '&state=' + State;

  // Ouvrir dans le navigateur
  OpenURL(AuthURL);

  // Note: Dans une vraie application, vous devriez démarrer un serveur local
  // pour capturer le callback automatiquement
  ShowMessage('Connectez-vous dans votre navigateur. ' +
              'Copiez le code de l''URL de retour.');

  // Pour simplifier, on demande le code manuellement
  var Code := InputBox('OAuth Callback', 'Entrez le code:', '');
  if Code <> '' then
    HandleOAuthCallback(Code);
end;

procedure TFormMain.HandleOAuthCallback(const Code: string);  
begin
  LabelStatus.Caption := 'Échange du code...';
  Application.ProcessMessages;

  ExchangeCodeForToken(Code);

  if FAccessToken <> '' then
  begin
    // Sauvegarder le token de manière sécurisée
    SaveToSecureStorage('google_access_token', FAccessToken);
    SaveToSecureStorage('google_refresh_token', FRefreshToken);

    LoadUserInfo;
    UpdateUI(True);
  end
  else
  begin
    ShowMessage('Erreur lors de l''authentification');
    UpdateUI(False);
  end;
end;

procedure TFormMain.ExchangeCodeForToken(const Code: string);  
var
  HTTP: THTTPSend;
  PostData, Response: string;
  JSON: TJSONData;
  JSONObj: TJSONObject;
begin
  HTTP := THTTPSend.Create;
  try
    PostData := Format(
      'grant_type=authorization_code&code=%s&client_id=%s&client_secret=%s&redirect_uri=%s',
      [Code, CLIENT_ID, CLIENT_SECRET, UrlEncode(REDIRECT_URI)]
    );

    HTTP.Document.Write(PostData[1], Length(PostData));
    HTTP.MimeType := 'application/x-www-form-urlencoded';

    if HTTP.HTTPMethod('POST', TOKEN_URL) then
    begin
      SetLength(Response, HTTP.Document.Size);
      HTTP.Document.Position := 0;
      HTTP.Document.Read(Response[1], HTTP.Document.Size);

      JSON := GetJSON(Response);
      try
        JSONObj := TJSONObject(JSON);
        FAccessToken := JSONObj.Get('access_token', '');
        FRefreshToken := JSONObj.Get('refresh_token', '');

        MemoToken.Lines.Add('Access Token: ' + Copy(FAccessToken, 1, 50) + '...');
        MemoToken.Lines.Add('Refresh Token: ' + Copy(FRefreshToken, 1, 50) + '...');
      finally
        JSON.Free;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

procedure TFormMain.LoadUserInfo;  
var
  HTTP: THTTPSend;
  Response: string;
  JSON: TJSONData;
  JSONObj: TJSONObject;
  Name, Email, Picture: string;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.Headers.Add('Authorization: Bearer ' + FAccessToken);

    if HTTP.HTTPMethod('GET', USERINFO_URL) then
    begin
      SetLength(Response, HTTP.Document.Size);
      HTTP.Document.Position := 0;
      HTTP.Document.Read(Response[1], HTTP.Document.Size);

      JSON := GetJSON(Response);
      try
        JSONObj := TJSONObject(JSON);
        Name := JSONObj.Get('name', 'Inconnu');
        Email := JSONObj.Get('email', 'Non disponible');
        Picture := JSONObj.Get('picture', '');

        LabelName.Caption := 'Nom: ' + Name;
        LabelEmail.Caption := 'Email: ' + Email;

        // Charger l'image de profil
        if Picture <> '' then
          LoadImageFromURL(Picture, ImageProfile);
      finally
        JSON.Free;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

procedure TFormMain.UpdateUI(LoggedIn: Boolean);  
begin
  BtnLogin.Visible := not LoggedIn;
  BtnLogout.Visible := LoggedIn;
  PanelInfo.Visible := LoggedIn;

  if LoggedIn then
    LabelStatus.Caption := 'Connecté'
  else
    LabelStatus.Caption := 'Non connecté';
end;

end.
```

## Gestion avancée des tokens

### Rafraîchissement automatique des tokens

```pascal
type
  TTokenManager = class
  private
    FAccessToken: string;
    FRefreshToken: string;
    FExpiresAt: TDateTime;
    FOnTokenRefreshed: TNotifyEvent;
    procedure RefreshTokenIfNeeded;
  public
    function GetValidAccessToken: string;
    procedure RefreshAccessToken;
    property AccessToken: string read FAccessToken write FAccessToken;
    property RefreshToken: string read FRefreshToken write FRefreshToken;
    property ExpiresAt: TDateTime read FExpiresAt write FExpiresAt;
    property OnTokenRefreshed: TNotifyEvent read FOnTokenRefreshed write FOnTokenRefreshed;
  end;

procedure TTokenManager.RefreshTokenIfNeeded;  
begin
  // Rafraîchir si le token expire dans moins de 5 minutes
  if Now > (FExpiresAt - (5 / 1440)) then
    RefreshAccessToken;
end;

function TTokenManager.GetValidAccessToken: string;  
begin
  RefreshTokenIfNeeded;
  Result := FAccessToken;
end;

procedure TTokenManager.RefreshAccessToken;  
var
  HTTP: THTTPSend;
  PostData, Response: string;
  JSON: TJSONData;
  JSONObj: TJSONObject;
begin
  HTTP := THTTPSend.Create;
  try
    PostData := Format(
      'grant_type=refresh_token&refresh_token=%s&client_id=%s&client_secret=%s',
      [FRefreshToken, CLIENT_ID, CLIENT_SECRET]
    );

    HTTP.Document.Write(PostData[1], Length(PostData));
    HTTP.MimeType := 'application/x-www-form-urlencoded';

    if HTTP.HTTPMethod('POST', TOKEN_URL) then
    begin
      SetLength(Response, HTTP.Document.Size);
      HTTP.Document.Read(Response[1], HTTP.Document.Size);

      JSON := GetJSON(Response);
      try
        JSONObj := TJSONObject(JSON);
        FAccessToken := JSONObj.Get('access_token', '');

        // Certains providers retournent un nouveau refresh token
        if JSONObj.Find('refresh_token') <> nil then
          FRefreshToken := JSONObj.Get('refresh_token', '');

        // Calculer l'expiration
        FExpiresAt := Now + (JSONObj.Get('expires_in', 3600) / 86400);

        // Sauvegarder
        SaveToSecureStorage('access_token', FAccessToken);
        SaveToSecureStorage('refresh_token', FRefreshToken);

        // Notifier
        if Assigned(FOnTokenRefreshed) then
          FOnTokenRefreshed(Self);
      finally
        JSON.Free;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

// Utilisation
var
  TokenManager: TTokenManager;
begin
  TokenManager := TTokenManager.Create;
  try
    TokenManager.AccessToken := 'existing_token';
    TokenManager.RefreshToken := 'existing_refresh';
    TokenManager.ExpiresAt := Now + (1/24); // Expire dans 1 heure

    // Obtenir un token valide (rafraîchit automatiquement si nécessaire)
    Token := TokenManager.GetValidAccessToken;

    // Faire une requête API
    MakeAPIRequest(Token);
  finally
    TokenManager.Free;
  end;
end;
```

### Cache de tokens avec expiration

```pascal
type
  TTokenCache = class
  private
    type
      TCachedToken = record
        Token: string;
        ExpiresAt: TDateTime;
      end;
    var
      FCache: TDictionary<string, TCachedToken>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetToken(const Key, Token: string; ExpiresIn: Integer);
    function GetToken(const Key: string): string;
    procedure Clear;
  end;

constructor TTokenCache.Create;  
begin
  inherited Create;
  FCache := TDictionary<string, TCachedToken>.Create;
end;

destructor TTokenCache.Destroy;  
begin
  FCache.Free;
  inherited Destroy;
end;

procedure TTokenCache.SetToken(const Key, Token: string; ExpiresIn: Integer);  
var
  CachedToken: TCachedToken;
begin
  CachedToken.Token := Token;
  CachedToken.ExpiresAt := Now + (ExpiresIn / 86400);
  FCache.AddOrSetValue(Key, CachedToken);
end;

function TTokenCache.GetToken(const Key: string): string;  
var
  CachedToken: TCachedToken;
begin
  Result := '';

  if FCache.TryGetValue(Key, CachedToken) then
  begin
    if Now < CachedToken.ExpiresAt then
      Result := CachedToken.Token
    else
      FCache.Remove(Key); // Token expiré, le supprimer
  end;
end;

procedure TTokenCache.Clear;  
begin
  FCache.Clear;
end;
```

## Considérations multi-plateformes

### Différences Windows vs Ubuntu

**Stockage sécurisé** :
- Windows : DPAPI, Credential Manager
- Ubuntu : Secret Service, GNOME Keyring

**Ouverture d'URL** :
```pascal
procedure OpenURL(const URL: string);  
begin
  {$IFDEF WINDOWS}
    ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
  {$ENDIF}
  {$IFDEF UNIX}
    ExecuteProcess('xdg-open', [URL]);
  {$ENDIF}
  {$IFDEF DARWIN}
    ExecuteProcess('open', [URL]);
  {$ENDIF}
end;
```

**Serveur HTTP local** :
- Même code sur toutes les plateformes avec fphttpserver
- Port par défaut : 8080 (ne nécessite pas de privilèges)

## Ressources et documentation

### Standards et spécifications

- **OAuth 2.0 (RFC 6749)** : https://datatracker.ietf.org/doc/html/rfc6749
- **OAuth 2.0 Bearer Token (RFC 6750)** : https://datatracker.ietf.org/doc/html/rfc6750
- **PKCE (RFC 7636)** : https://datatracker.ietf.org/doc/html/rfc7636
- **OpenID Connect** : https://openid.net/connect/
- **JWT (RFC 7519)** : https://datatracker.ietf.org/doc/html/rfc7519

### Documentation des providers

- **Google OAuth 2.0** : https://developers.google.com/identity/protocols/oauth2
- **GitHub OAuth** : https://docs.github.com/en/developers/apps/building-oauth-apps
- **Microsoft Identity Platform** : https://docs.microsoft.com/en-us/azure/active-directory/develop/
- **Facebook Login** : https://developers.facebook.com/docs/facebook-login

### Outils et bibliothèques

- **OAuth 2.0 Playground (Google)** : https://developers.google.com/oauthplayground/
- **JWT.io** : Décodeur et vérificateur de JWT
- **Postman** : Tester les flux OAuth 2.0
- **oauth.net** : Ressources et documentation OAuth

### Sécurité

- **OAuth 2.0 Security Best Practices** : https://datatracker.ietf.org/doc/html/draft-ietf-oauth-security-topics
- **OWASP Authentication Cheat Sheet** : https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html
- **OAuth 2.0 Threat Model** : https://datatracker.ietf.org/doc/html/rfc6819

## Conclusion

OAuth 2.0 est devenu le standard pour l'authentification et l'autorisation dans les applications modernes. Bien que le protocole puisse sembler complexe au premier abord, une compréhension claire des différents flux et de leurs cas d'usage permet de l'implémenter efficacement.

**Points clés à retenir** :

✅ **Choisir le bon flux** :
- Authorization Code Flow (+ PKCE) pour la plupart des applications
- Client Credentials pour server-to-server
- Éviter Implicit Flow et Resource Owner Password

✅ **Sécurité** :
- Toujours utiliser HTTPS
- Implémenter le paramètre `state` (CSRF protection)
- Utiliser PKCE pour les applications publiques
- Valider tous les paramètres et redirections
- Stocker les tokens de manière sécurisée

✅ **Gestion des tokens** :
- Access tokens : courte durée de vie (1 heure typiquement)
- Refresh tokens : longue durée, stockage sécurisé
- Rafraîchir automatiquement avant expiration
- Logger les événements de sécurité

✅ **Multi-plateforme** :
- Le protocole OAuth est identique sur toutes les plateformes
- Différences uniquement dans le stockage sécurisé
- Tester sur Windows et Ubuntu

✅ **Intégration** :
- Utiliser les bibliothèques existantes quand possible
- Suivre les bonnes pratiques des providers
- Implémenter une gestion d'erreurs robuste

Avec FreePascal et Lazarus, vous disposez de tous les outils nécessaires pour implémenter OAuth 2.0 de manière sécurisée et portable sur Windows et Ubuntu. Les bibliothèques comme Synapse facilitent les communications HTTPS, tandis que fpjson permet de parser facilement les réponses JSON des serveurs OAuth.

**Prochaine section** : 17.4 JWT et tokens sécurisés

⏭️ [JWT et tokens sécurisés](/17-securite-cryptographie/04-jwt-tokens-securises.md)
