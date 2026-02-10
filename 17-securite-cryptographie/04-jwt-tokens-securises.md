🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.4 JWT et tokens sécurisés

## Introduction

Les JSON Web Tokens (JWT) sont devenus le standard pour l'authentification et l'échange sécurisé d'informations dans les applications modernes. Un JWT est un token compact, auto-contenu et sécurisé qui peut être vérifié et auquel on peut faire confiance car il est signé numériquement.

Dans cette section, nous allons explorer en détail les JWT, leur structure, leur utilisation et les meilleures pratiques pour les implémenter de manière sécurisée avec FreePascal et Lazarus.

## Qu'est-ce qu'un JWT ?

### Définition

JWT (JSON Web Token) est un standard ouvert (RFC 7519) qui définit une manière compacte et auto-suffisante de transmettre de l'information entre parties sous forme d'objet JSON. Cette information peut être vérifiée et approuvée car elle est signée numériquement.

### Pourquoi utiliser des JWT ?

**Avantages** :
- ✅ **Stateless** : Pas besoin de stocker les sessions côté serveur
- ✅ **Portable** : Fonctionne sur tous les types d'applications (web, mobile, desktop)
- ✅ **Auto-contenu** : Contient toutes les informations nécessaires
- ✅ **Compact** : Peut être envoyé via URL, POST, ou HTTP header
- ✅ **Extensible** : Peut contenir des informations personnalisées

**Cas d'usage** :
- Authentification (le plus courant)
- Échange d'informations sécurisé
- Single Sign-On (SSO)
- API stateless
- Autorisations granulaires

## Structure d'un JWT

Un JWT est composé de trois parties séparées par des points (`.`) :

```
Header.Payload.Signature
```

**Exemple de JWT** :
```
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c
```

### 1. Header (En-tête)

Le header contient généralement deux informations :
- Le type de token (JWT)
- L'algorithme de signature utilisé

**Exemple de header décodé** :
```json
{
  "alg": "HS256",
  "typ": "JWT"
}
```

**Algorithmes courants** :
- `HS256` : HMAC avec SHA-256 (symétrique)
- `RS256` : RSA avec SHA-256 (asymétrique)
- `ES256` : ECDSA avec SHA-256 (asymétrique)

**Encodage** : Le header est encodé en Base64URL

### 2. Payload (Charge utile)

Le payload contient les "claims" (revendications), c'est-à-dire les informations que vous souhaitez transmettre.

**Exemple de payload décodé** :
```json
{
  "sub": "1234567890",
  "name": "John Doe",
  "email": "john@example.com",
  "role": "admin",
  "iat": 1516239022,
  "exp": 1516242622
}
```

**Types de claims** :

**Claims réservés (recommandés mais optionnels)** :
- `iss` (issuer) : Émetteur du token
- `sub` (subject) : Sujet du token (généralement l'ID utilisateur)
- `aud` (audience) : Destinataire prévu du token
- `exp` (expiration) : Date d'expiration (timestamp Unix)
- `nbf` (not before) : Date avant laquelle le token n'est pas valide
- `iat` (issued at) : Date d'émission du token
- `jti` (JWT ID) : Identifiant unique du token

**Claims publics** :
- Peuvent être définis librement
- Devraient être enregistrés dans le IANA JWT Registry ou utiliser un nom résistant aux collisions

**Claims privés** :
- Claims personnalisés créés pour partager des informations entre parties qui les utilisent
- Exemples : `name`, `email`, `role`, `permissions`

**Encodage** : Le payload est encodé en Base64URL

⚠️ **Important** : Le payload n'est PAS chiffré, seulement encodé. N'y mettez jamais d'informations sensibles comme des mots de passe !

### 3. Signature

La signature sert à vérifier que le message n'a pas été modifié en cours de route.

**Calcul de la signature** :
```
HMACSHA256(
  base64UrlEncode(header) + "." + base64UrlEncode(payload),
  secret
)
```

Pour vérifier un JWT :
1. Recalculer la signature avec le header et payload reçus
2. Comparer avec la signature reçue
3. Si elles correspondent, le token est valide

## Implémentation JWT en FreePascal

### Encodage Base64URL

Le JWT utilise une variante de Base64 appelée Base64URL :

```pascal
uses
  Base64, SysUtils;

function Base64URLEncode(const Data: string): string;
begin
  Result := EncodeStringBase64(Data);

  // Remplacer les caractères Base64 standard par Base64URL
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);

  // Supprimer le padding '='
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

function Base64URLDecode(const Data: string): string;
var
  S: string;
  Padding: Integer;
begin
  S := Data;

  // Restaurer les caractères Base64 standard
  S := StringReplace(S, '-', '+', [rfReplaceAll]);
  S := StringReplace(S, '_', '/', [rfReplaceAll]);

  // Ajouter le padding si nécessaire
  Padding := Length(S) mod 4;
  if Padding > 0 then
    S := S + StringOfChar('=', 4 - Padding);

  Result := DecodeStringBase64(S);
end;
```

### Création d'un JWT avec HMAC-SHA256

```pascal
uses
  SysUtils, fpjson, jsonparser, DCPsha256, DCPcrypt2;

type
  TJWTHeader = record
    Alg: string;  // Algorithme
    Typ: string;  // Type
  end;

  TJWTPayload = record
    Sub: string;      // Subject (user ID)
    Name: string;     // Nom de l'utilisateur
    Email: string;    // Email
    Role: string;     // Rôle
    Iat: Int64;       // Issued at
    Exp: Int64;       // Expiration
  end;

function HMACSHA256(const Data, Key: string): string;
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
  KeyBytes: TBytes;
  i: Integer;
begin
  SetLength(KeyBytes, Length(Key));
  Move(Key[1], KeyBytes[0], Length(Key));

  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.Update(KeyBytes[0], Length(KeyBytes));
    Hash.UpdateStr(Data);
    Hash.Final(Digest);

    SetLength(Result, 32);
    Move(Digest[0], Result[1], 32);
  finally
    Hash.Free;
  end;
end;

function CreateJWT(const Payload: TJWTPayload; const Secret: string): string;
var
  Header: TJWTHeader;
  HeaderJSON, PayloadJSON: string;
  EncodedHeader, EncodedPayload, Signature: string;
  ToSign: string;
begin
  // 1. Créer le header
  Header.Alg := 'HS256';
  Header.Typ := 'JWT';

  HeaderJSON := Format('{"alg":"%s","typ":"%s"}', [Header.Alg, Header.Typ]);

  // 2. Créer le payload
  PayloadJSON := Format(
    '{"sub":"%s","name":"%s","email":"%s","role":"%s","iat":%d,"exp":%d}',
    [Payload.Sub, Payload.Name, Payload.Email, Payload.Role,
     Payload.Iat, Payload.Exp]
  );

  // 3. Encoder en Base64URL
  EncodedHeader := Base64URLEncode(HeaderJSON);
  EncodedPayload := Base64URLEncode(PayloadJSON);

  // 4. Créer la signature
  ToSign := EncodedHeader + '.' + EncodedPayload;
  Signature := Base64URLEncode(HMACSHA256(ToSign, Secret));

  // 5. Assembler le JWT
  Result := EncodedHeader + '.' + EncodedPayload + '.' + Signature;
end;
```

### Vérification d'un JWT

```pascal
function VerifyJWT(const Token, Secret: string): Boolean;
var
  Parts: TStringList;
  EncodedHeader, EncodedPayload, ReceivedSignature: string;
  ToSign, ComputedSignature: string;
begin
  Result := False;

  // 1. Séparer les parties du JWT
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '.';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Token;
    if Parts.Count <> 3 then
      Exit; // Format invalide

    EncodedHeader := Parts[0];
    EncodedPayload := Parts[1];
    ReceivedSignature := Parts[2];

    // 2. Recalculer la signature
    ToSign := EncodedHeader + '.' + EncodedPayload;
    ComputedSignature := Base64URLEncode(HMACSHA256(ToSign, Secret));

    // 3. Comparer les signatures
    if ComputedSignature <> ReceivedSignature then
      Exit; // Signature invalide

    // 4. Vérifier l'expiration
    Result := VerifyExpiration(EncodedPayload);
  finally
    Parts.Free;
  end;
end;

function VerifyExpiration(const EncodedPayload: string): Boolean;
var
  PayloadJSON: string;
  JSON: TJSONData;
  Exp: Int64;
  Now: Int64;
begin
  Result := False;

  PayloadJSON := Base64URLDecode(EncodedPayload);
  JSON := GetJSON(PayloadJSON);
  try
    Exp := TJSONObject(JSON).Get('exp', Int64(0));
    Now := DateTimeToUnix(SysUtils.Now);

    Result := (Exp > 0) and (Now < Exp);
  finally
    JSON.Free;
  end;
end;
```

### Décoder un JWT

```pascal
function DecodeJWT(const Token: string; out Payload: TJWTPayload): Boolean;
var
  Parts: TStringList;
  PayloadJSON: string;
  JSON: TJSONData;
  JSONObj: TJSONObject;
begin
  Result := False;

  Parts := TStringList.Create;
  try
    Parts.Delimiter := '.';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Token;
    if Parts.Count <> 3 then
      Exit;

    try
      PayloadJSON := Base64URLDecode(Parts[1]);
      JSON := GetJSON(PayloadJSON);
      try
        JSONObj := TJSONObject(JSON);

        Payload.Sub := JSONObj.Get('sub', '');
        Payload.Name := JSONObj.Get('name', '');
        Payload.Email := JSONObj.Get('email', '');
        Payload.Role := JSONObj.Get('role', '');
        Payload.Iat := JSONObj.Get('iat', Int64(0));
        Payload.Exp := JSONObj.Get('exp', Int64(0));

        Result := True;
      finally
        JSON.Free;
      end;
    except
      Result := False;
    end;
  finally
    Parts.Free;
  end;
end;
```

## Exemple complet d'utilisation

```pascal
program JWTExample;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils;

const
  SECRET_KEY = 'votre-cle-secrete-tres-longue-et-aleatoire';

var
  Token: string;
  Payload: TJWTPayload;
  IsValid: Boolean;
begin
  WriteLn('=== Exemple JWT ===');
  WriteLn;

  // 1. Créer un JWT
  Payload.Sub := '12345';
  Payload.Name := 'John Doe';
  Payload.Email := 'john@example.com';
  Payload.Role := 'admin';
  Payload.Iat := DateTimeToUnix(Now);
  Payload.Exp := DateTimeToUnix(Now + 1); // Expire dans 1 jour

  Token := CreateJWT(Payload, SECRET_KEY);

  WriteLn('JWT créé:');
  WriteLn(Token);
  WriteLn;

  // 2. Vérifier le JWT
  IsValid := VerifyJWT(Token, SECRET_KEY);
  WriteLn('JWT valide: ', IsValid);
  WriteLn;

  // 3. Décoder le JWT
  if DecodeJWT(Token, Payload) then
  begin
    WriteLn('Informations décodées:');
    WriteLn('  User ID: ', Payload.Sub);
    WriteLn('  Name: ', Payload.Name);
    WriteLn('  Email: ', Payload.Email);
    WriteLn('  Role: ', Payload.Role);
    WriteLn('  Issued at: ', UnixToDateTime(Payload.Iat));
    WriteLn('  Expires at: ', UnixToDateTime(Payload.Exp));
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## JWT avec RSA (Asymétrique)

### Pourquoi utiliser RSA ?

**HMAC (HS256)** :
- Même clé pour signer et vérifier
- Le serveur qui crée le JWT doit avoir la clé secrète
- Tous les serveurs qui vérifient doivent avoir la même clé

**RSA (RS256)** :
- Clé privée pour signer
- Clé publique pour vérifier
- Le créateur garde la clé privée secrète
- N'importe qui peut vérifier avec la clé publique

### Génération de clés RSA

Avec OpenSSL en ligne de commande :

```bash
# Générer une clé privée RSA de 2048 bits
openssl genrsa -out private.pem 2048

# Extraire la clé publique
openssl rsa -in private.pem -pubout -out public.pem
```

### Implémentation RSA-SHA256

```pascal
uses
  OpenSSL, OpenSSLUtils;

function SignJWTWithRSA(const HeaderPayload: string;
                        const PrivateKeyFile: string): string;
var
  PrivateKey: PEVP_PKEY;
  Context: PEVP_MD_CTX;
  Signature: array of Byte;
  SigLen: Cardinal;
begin
  // Charger la clé privée
  PrivateKey := LoadPrivateKey(PrivateKeyFile);
  if PrivateKey = nil then
    raise Exception.Create('Impossible de charger la clé privée');

  try
    // Créer le contexte de signature
    Context := EVP_MD_CTX_new;
    try
      // Initialiser pour RS256
      EVP_DigestSignInit(Context, nil, EVP_sha256, nil, PrivateKey);

      // Mettre à jour avec les données
      EVP_DigestSignUpdate(Context, PChar(HeaderPayload), Length(HeaderPayload));

      // Obtenir la taille de la signature
      EVP_DigestSignFinal(Context, nil, @SigLen);

      // Allouer et obtenir la signature
      SetLength(Signature, SigLen);
      EVP_DigestSignFinal(Context, @Signature[0], @SigLen);

      // Encoder en Base64URL
      SetLength(Result, SigLen);
      Move(Signature[0], Result[1], SigLen);
      Result := Base64URLEncode(Result);
    finally
      EVP_MD_CTX_free(Context);
    end;
  finally
    EVP_PKEY_free(PrivateKey);
  end;
end;

function VerifyJWTWithRSA(const Token: string;
                          const PublicKeyFile: string): Boolean;
var
  Parts: TStringList;
  HeaderPayload, Signature: string;
  PublicKey: PEVP_PKEY;
  Context: PEVP_MD_CTX;
  SigBytes: TBytes;
begin
  Result := False;

  // Séparer le token
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '.';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Token;
    if Parts.Count <> 3 then Exit;

    HeaderPayload := Parts[0] + '.' + Parts[1];
    Signature := Base64URLDecode(Parts[2]);
  finally
    Parts.Free;
  end;

  // Convertir la signature en bytes
  SetLength(SigBytes, Length(Signature));
  Move(Signature[1], SigBytes[0], Length(Signature));

  // Charger la clé publique
  PublicKey := LoadPublicKey(PublicKeyFile);
  if PublicKey = nil then Exit;

  try
    Context := EVP_MD_CTX_new;
    try
      EVP_DigestVerifyInit(Context, nil, EVP_sha256, nil, PublicKey);
      EVP_DigestVerifyUpdate(Context, PChar(HeaderPayload), Length(HeaderPayload));

      Result := EVP_DigestVerifyFinal(Context, @SigBytes[0], Length(SigBytes)) = 1;
    finally
      EVP_MD_CTX_free(Context);
    end;
  finally
    EVP_PKEY_free(PublicKey);
  end;
end;
```

## Bibliothèques JWT pour FreePascal

### jwt-paslib

Une bibliothèque JWT complète pour FreePascal :

**Installation** :
```bash
git clone https://github.com/gcarreno/jwt-paslib.git
```

**Utilisation** :
```pascal
uses
  JOSE.Core.JWT,
  JOSE.Core.Builder;

var
  Token: TJWT;
  CompactToken: string;
begin
  Token := TJWT.Create;
  try
    // Définir les claims
    Token.Claims.Subject := '12345';
    Token.Claims.Expiration := Now + 1; // 1 jour
    Token.Claims.IssuedAt := Now;

    // Ajouter des claims personnalisés
    Token.Claims.SetClaimOfType<string>('name', 'John Doe');
    Token.Claims.SetClaimOfType<string>('email', 'john@example.com');
    Token.Claims.SetClaimOfType<string>('role', 'admin');

    // Signer avec HMAC-SHA256
    CompactToken := TJOSE.SHA256CompactToken('your-secret-key', Token);

    WriteLn('JWT: ', CompactToken);
  finally
    Token.Free;
  end;
end;
```

### mORMot JWT

Le framework mORMot inclut un support JWT complet :

```pascal
uses
  SynCrypto, SynCommons;

var
  JWT: TJWTContent;
  Token: RawUTF8;
begin
  // Créer le contenu du JWT
  JWT.reg[jrcSubject] := '12345';
  JWT.reg[jrcExpirationTime] := DateTimeToUnix(Now + 1);
  JWT.reg[jrcIssuedAt] := DateTimeToUnix(Now);

  JWT.data.AddNameValue('name', 'John Doe');
  JWT.data.AddNameValue('email', 'john@example.com');
  JWT.data.AddNameValue('role', 'admin');

  // Générer le token
  Token := JWTEncode(JWT, 'your-secret-key');

  WriteLn('JWT: ', Token);
end;
```

## Stockage sécurisé des JWT

### Où stocker les JWT ?

**Applications web** :

**1. HttpOnly Cookies (Recommandé)** :
```pascal
Response.SetCookie(
  'access_token',
  Token,
  '',          // Path
  Now + 7,     // Expires
  True,        // HttpOnly - pas accessible en JavaScript
  True,        // Secure - HTTPS uniquement
  'Strict'     // SameSite
);
```

✅ Avantages :
- Protégé contre XSS (pas accessible en JavaScript)
- Envoyé automatiquement avec les requêtes
- Support du flag Secure et SameSite

❌ Inconvénients :
- Vulnérable au CSRF (nécessite une protection)
- Pas accessible côté client pour d'autres usages

**2. LocalStorage (À éviter)** :
```javascript
// ❌ Vulnérable aux attaques XSS
localStorage.setItem('token', jwt);
```

**3. SessionStorage** :
```javascript
// ❌ Également vulnérable aux XSS
sessionStorage.setItem('token', jwt);
```

**Applications desktop/mobile** :

**Windows** :
```pascal
{$IFDEF WINDOWS}
// Utiliser DPAPI
function StoreJWT(const Token: string): Boolean;
var
  EncryptedToken: string;
begin
  EncryptedToken := ProtectDataDPAPI(Token);
  Result := SaveToFile('token.dat', EncryptedToken);
end;
{$ENDIF}
```

**Linux/Ubuntu** :
```pascal
{$IFDEF UNIX}
// Utiliser Secret Service
procedure StoreJWT(const Token: string);
begin
  ExecuteProcess('secret-tool', [
    'store',
    '--label=AppToken',
    'service', 'MonApplication',
    'token', Token
  ]);
end;
{$ENDIF}
```

### Ne jamais stocker en clair

```pascal
// ❌ DANGEREUX
procedure BadStorage(const Token: string);
begin
  WriteToFile('token.txt', Token); // Token visible !
end;

// ✅ CORRECT
procedure SecureStorage(const Token: string);
var
  EncryptedToken: string;
begin
  EncryptedToken := EncryptAES256(Token, GetMasterKey);
  WriteToFile('token.enc', EncryptedToken);
end;
```

## Gestion de l'expiration

### Vérifier l'expiration

```pascal
function IsJWTExpired(const Token: string): Boolean;
var
  Parts: TStringList;
  PayloadJSON: string;
  JSON: TJSONData;
  Exp: Int64;
begin
  Result := True; // Par défaut, considérer comme expiré

  Parts := TStringList.Create;
  try
    Parts.Delimiter := '.';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Token;
    if Parts.Count <> 3 then Exit;

    try
      PayloadJSON := Base64URLDecode(Parts[1]);
      JSON := GetJSON(PayloadJSON);
      try
        Exp := TJSONObject(JSON).Get('exp', Int64(0));
        Result := DateTimeToUnix(Now) >= Exp;
      finally
        JSON.Free;
      end;
    except
      Result := True;
    end;
  finally
    Parts.Free;
  end;
end;
```

### Refresh Token pattern

```pascal
type
  TTokenPair = record
    AccessToken: string;   // JWT court (15 min - 1h)
    RefreshToken: string;  // Token long (7 jours - 30 jours)
  end;

function CreateTokenPair(const UserID: string): TTokenPair;
var
  AccessPayload: TJWTPayload;
begin
  // Access Token - courte durée
  AccessPayload.Sub := UserID;
  AccessPayload.Iat := DateTimeToUnix(Now);
  AccessPayload.Exp := DateTimeToUnix(Now + (1/24)); // 1 heure

  Result.AccessToken := CreateJWT(AccessPayload, SECRET_KEY);

  // Refresh Token - longue durée, stocké en base
  Result.RefreshToken := GenerateSecureRandomToken(32);
  StoreRefreshToken(UserID, Result.RefreshToken, Now + 30); // 30 jours
end;

function RefreshAccessToken(const RefreshToken: string): string;
var
  UserID: string;
  NewPayload: TJWTPayload;
begin
  // Vérifier le refresh token
  if not ValidateRefreshToken(RefreshToken, UserID) then
    raise Exception.Create('Invalid refresh token');

  // Créer un nouveau access token
  NewPayload.Sub := UserID;
  NewPayload.Iat := DateTimeToUnix(Now);
  NewPayload.Exp := DateTimeToUnix(Now + (1/24));

  Result := CreateJWT(NewPayload, SECRET_KEY);
end;
```

## Sécurité des JWT

### Attaques courantes et protections

#### 1. Attaque "None Algorithm"

**Attaque** : Modifier l'algorithme à "none" pour bypasser la vérification

```json
{
  "alg": "none",
  "typ": "JWT"
}
```

**Protection** :
```pascal
function VerifyJWT(const Token, Secret: string): Boolean;
var
  HeaderJSON: string;
  JSON: TJSONData;
  Alg: string;
begin
  Result := False;

  // Décoder le header
  HeaderJSON := Base64URLDecode(Copy(Token, 1, Pos('.', Token) - 1));
  JSON := GetJSON(HeaderJSON);
  try
    Alg := TJSONObject(JSON).Get('alg', '');

    // ✅ Rejeter "none"
    if (Alg = 'none') or (Alg = '') then
      Exit;

    // Continuer la vérification...
  finally
    JSON.Free;
  end;
end;
```

#### 2. Attaque par substitution d'algorithme

**Attaque** : Changer RS256 en HS256 et signer avec la clé publique

**Protection** : Toujours vérifier l'algorithme attendu

```pascal
const
  EXPECTED_ALGORITHM = 'HS256';

function VerifyJWT(const Token, Secret: string): Boolean;
var
  Algorithm: string;
begin
  Algorithm := GetAlgorithm(Token);

  // ✅ Vérifier l'algorithme
  if Algorithm <> EXPECTED_ALGORITHM then
  begin
    LogSecurityEvent('JWT_ALGORITHM_MISMATCH',
      'Expected: ' + EXPECTED_ALGORITHM + ', Got: ' + Algorithm);
    Exit(False);
  end;

  // Continuer...
end;
```

#### 3. Token non expiré

**Problème** : JWT sans claim `exp`

**Protection** :
```pascal
function VerifyJWT(const Token, Secret: string): Boolean;
var
  Payload: TJWTPayload;
begin
  if not DecodeJWT(Token, Payload) then
    Exit(False);

  // ✅ Exiger une expiration
  if Payload.Exp = 0 then
  begin
    LogSecurityEvent('JWT_NO_EXPIRATION', 'Token without exp claim');
    Exit(False);
  end;

  // ✅ Vérifier qu'il n'est pas expiré
  if DateTimeToUnix(Now) >= Payload.Exp then
  begin
    LogSecurityEvent('JWT_EXPIRED', 'Token expired');
    Exit(False);
  end;

  Result := True;
end;
```

#### 4. Révocation de JWT

**Problème** : Les JWT ne peuvent pas être révoqués facilement

**Solutions** :

**A. Liste noire (Blacklist)** :
```pascal
// Stocker les tokens révoqués (Redis, base de données)
procedure RevokeJWT(const JTI: string; ExpiresAt: TDateTime);
begin
  AddToBlacklist(JTI, ExpiresAt);
end;

function IsJWTRevoked(const Token: string): Boolean;
var
  Payload: TJWTPayload;
begin
  DecodeJWT(Token, Payload);
  Result := IsInBlacklist(Payload.Jti);
end;
```

**B. Courte durée de vie** :
```pascal
// Access tokens très courts (5-15 minutes)
// Utiliser refresh tokens pour renouveler
Payload.Exp := DateTimeToUnix(Now + (15 / 1440)); // 15 minutes
```

**C. Versioning** :
```pascal
// Ajouter un numéro de version
Payload.Ver := GetUserTokenVersion(UserID);

// Incrémenter à la déconnexion
procedure LogoutUser(UserID: string);
begin
  IncrementUserTokenVersion(UserID);
  // Tous les anciens tokens deviennent invalides
end;
```

### Bonnes pratiques de sécurité

**1. Utiliser HTTPS partout** :
```pascal
// Vérifier que la requête est sécurisée
if Request.Protocol <> 'https' then
begin
  Response.StatusCode := 400;
  Response.Content := 'HTTPS required';
  Exit;
end;
```

**2. Clé secrète forte** :
```pascal
// ❌ Faible
const SECRET = '123456';

// ✅ Fort
const SECRET = 'kJh#8d$fK2!mN9@pQw7*vX4&zL6^tR3%bG1+hY5)cF0-sV8=nM2';

// ✅ Encore mieux : générer aléatoirement et stocker de manière sécurisée
function GenerateSecretKey: string;
var
  Bytes: TBytes;
  i: Integer;
begin
  SetLength(Bytes, 64);
  RandomFillBytes(Bytes);

  // Convertir en chaîne hexadécimale
  Result := '';
  for i := 0 to High(Bytes) do
    Result := Result + IntToHex(Bytes[i], 2);
end;

// Utilisation
var
  SecretKey: string;
begin
  {$IFDEF DEBUG}
  // En développement : clé fixe pour faciliter les tests
  SecretKey := 'dev-secret-key-not-for-production';
  {$ELSE}
  // En production : charger depuis variable d'environnement ou fichier sécurisé
  SecretKey := GetEnvironmentVariable('JWT_SECRET');
  if SecretKey = '' then
    raise Exception.Create('JWT_SECRET not configured');
  {$ENDIF}
end;
```

**3. Durée de vie appropriée** :

```pascal
// Différentes durées selon le type de token
const
  ACCESS_TOKEN_LIFETIME = 15 / 1440;    // 15 minutes
  REFRESH_TOKEN_LIFETIME = 30;           // 30 jours
  REMEMBER_ME_LIFETIME = 365;            // 1 an (si option "Se souvenir")

function CreateAccessToken(const UserID: string): string;
var
  Payload: TJWTPayload;
begin
  Payload.Sub := UserID;
  Payload.Iat := DateTimeToUnix(Now);
  Payload.Exp := DateTimeToUnix(Now + ACCESS_TOKEN_LIFETIME);
  Payload.Jti := GenerateUUID; // Identifiant unique

  Result := CreateJWT(Payload, GetSecretKey);
end;
```

**4. Ne pas stocker d'informations sensibles** :

```pascal
// ❌ DANGEREUX - Ne jamais faire
Payload.Password := UserPassword;           // Mot de passe
Payload.CreditCard := '1234-5678-9012-3456'; // Carte bancaire
Payload.SSN := '123-45-6789';                // Numéro de sécurité sociale

// ✅ CORRECT - Informations non sensibles uniquement
Payload.Sub := UserID;
Payload.Name := UserName;
Payload.Email := UserEmail;
Payload.Role := UserRole;
Payload.Permissions := 'read,write'; // Permissions publiques
```

**5. Valider tous les claims** :

```pascal
function ValidateJWTClaims(const Token: string): Boolean;
var
  Payload: TJWTPayload;
  Now: Int64;
begin
  Result := False;

  if not DecodeJWT(Token, Payload) then
    Exit;

  Now := DateTimeToUnix(SysUtils.Now);

  // Vérifier le subject (obligatoire)
  if Payload.Sub = '' then
  begin
    LogSecurityEvent('JWT_INVALID', 'Missing subject claim');
    Exit;
  end;

  // Vérifier l'expiration (obligatoire)
  if Payload.Exp = 0 then
  begin
    LogSecurityEvent('JWT_INVALID', 'Missing expiration claim');
    Exit;
  end;

  if Now >= Payload.Exp then
  begin
    LogSecurityEvent('JWT_EXPIRED', 'Token expired');
    Exit;
  end;

  // Vérifier "not before" si présent
  if (Payload.Nbf > 0) and (Now < Payload.Nbf) then
  begin
    LogSecurityEvent('JWT_NOT_YET_VALID', 'Token not yet valid');
    Exit;
  end;

  // Vérifier l'issuer si configuré
  if (EXPECTED_ISSUER <> '') and (Payload.Iss <> EXPECTED_ISSUER) then
  begin
    LogSecurityEvent('JWT_INVALID_ISSUER',
      'Expected: ' + EXPECTED_ISSUER + ', Got: ' + Payload.Iss);
    Exit;
  end;

  Result := True;
end;
```

## Middleware d'authentification JWT

### Middleware pour API REST

```pascal
type
  TJWTMiddleware = class
  private
    FSecretKey: string;
    FRequiredScopes: TStringList;
  public
    constructor Create(const SecretKey: string);
    destructor Destroy; override;
    function Authenticate(Request: TRequest; Response: TResponse): Boolean;
    procedure RequireScope(const Scope: string);
  end;

constructor TJWTMiddleware.Create(const SecretKey: string);
begin
  inherited Create;
  FSecretKey := SecretKey;
  FRequiredScopes := TStringList.Create;
end;

destructor TJWTMiddleware.Destroy;
begin
  FRequiredScopes.Free;
  inherited Destroy;
end;

function TJWTMiddleware.Authenticate(Request: TRequest;
                                      Response: TResponse): Boolean;
var
  AuthHeader: string;
  Token: string;
  Payload: TJWTPayload;
  i: Integer;
begin
  Result := False;

  // 1. Extraire le token du header Authorization
  AuthHeader := Request.GetHeader('Authorization');

  if Copy(AuthHeader, 1, 7) <> 'Bearer ' then
  begin
    Response.StatusCode := 401;
    Response.ContentType := 'application/json';
    Response.Content := '{"error":"missing_authorization_header"}';
    Exit;
  end;

  Token := Copy(AuthHeader, 8, Length(AuthHeader)); // Enlever "Bearer "

  // 2. Vérifier la signature du token
  if not VerifyJWT(Token, FSecretKey) then
  begin
    Response.StatusCode := 401;
    Response.Content := '{"error":"invalid_token"}';
    Exit;
  end;

  // 3. Décoder et valider les claims
  if not DecodeJWT(Token, Payload) then
  begin
    Response.StatusCode := 401;
    Response.Content := '{"error":"invalid_token_format"}';
    Exit;
  end;

  if not ValidateJWTClaims(Token) then
  begin
    Response.StatusCode := 401;
    Response.Content := '{"error":"invalid_claims"}';
    Exit;
  end;

  // 4. Vérifier les scopes requis
  if FRequiredScopes.Count > 0 then
  begin
    for i := 0 to FRequiredScopes.Count - 1 do
    begin
      if not HasScope(Payload, FRequiredScopes[i]) then
      begin
        Response.StatusCode := 403;
        Response.Content := '{"error":"insufficient_scope"}';
        Exit;
      end;
    end;
  end;

  // 5. Attacher les informations utilisateur à la requête
  Request.User := Payload;

  Result := True;
end;

procedure TJWTMiddleware.RequireScope(const Scope: string);
begin
  FRequiredScopes.Add(Scope);
end;

// Utilisation
procedure HandleProtectedEndpoint(Request: TRequest; Response: TResponse);
var
  Auth: TJWTMiddleware;
begin
  Auth := TJWTMiddleware.Create(SECRET_KEY);
  try
    Auth.RequireScope('admin');

    if not Auth.Authenticate(Request, Response) then
      Exit; // Déjà géré par le middleware

    // L'utilisateur est authentifié et autorisé
    Response.Content := 'Welcome, ' + Request.User.Name;
  finally
    Auth.Free;
  end;
end;
```

### Extraction du token de différentes sources

```pascal
function ExtractToken(Request: TRequest): string;
begin
  Result := '';

  // 1. Essayer le header Authorization
  if Copy(Request.GetHeader('Authorization'), 1, 7) = 'Bearer ' then
  begin
    Result := Copy(Request.GetHeader('Authorization'), 8, MaxInt);
    Exit;
  end;

  // 2. Essayer les cookies
  if Request.Cookies['access_token'] <> '' then
  begin
    Result := Request.Cookies['access_token'];
    Exit;
  end;

  // 3. Essayer les paramètres de requête (déconseillé, sauf cas spécifiques)
  if Request.Query['token'] <> '' then
  begin
    Result := Request.Query['token'];
    Exit;
  end;
end;
```

## Claims personnalisés et permissions

### Structure de permissions

```pascal
type
  TPermission = record
    Resource: string;  // users, posts, comments
    Action: string;    // read, write, delete, admin
  end;

  TPermissions = array of TPermission;

function CreateJWTWithPermissions(const UserID: string;
                                  const Permissions: TPermissions): string;
var
  Payload: TJSONObject;
  PermArray: TJSONArray;
  i: Integer;
  PermObj: TJSONObject;
  PayloadStr: string;
begin
  Payload := TJSONObject.Create;
  try
    // Claims standard
    Payload.Add('sub', UserID);
    Payload.Add('iat', DateTimeToUnix(Now));
    Payload.Add('exp', DateTimeToUnix(Now + (1/24)));

    // Permissions personnalisées
    PermArray := TJSONArray.Create;
    for i := 0 to High(Permissions) do
    begin
      PermObj := TJSONObject.Create;
      PermObj.Add('resource', Permissions[i].Resource);
      PermObj.Add('action', Permissions[i].Action);
      PermArray.Add(PermObj);
    end;
    Payload.Add('permissions', PermArray);

    PayloadStr := Payload.AsJSON;

    // Créer le JWT
    Result := CreateJWTFromJSON(PayloadStr, SECRET_KEY);
  finally
    Payload.Free;
  end;
end;

// Vérification des permissions
function HasPermission(const Token: string;
                       const Resource, Action: string): Boolean;
var
  Payload: TJSONObject;
  Permissions: TJSONArray;
  i: Integer;
  Perm: TJSONObject;
begin
  Result := False;

  Payload := DecodeJWTToJSON(Token);
  try
    if not Payload.Find('permissions', jtArray) then
      Exit;

    Permissions := TJSONArray(Payload.Get('permissions'));

    for i := 0 to Permissions.Count - 1 do
    begin
      Perm := TJSONObject(Permissions[i]);

      if (Perm.Get('resource', '') = Resource) and
         (Perm.Get('action', '') = Action) then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    Payload.Free;
  end;
end;

// Utilisation
if not HasPermission(Token, 'users', 'delete') then
begin
  Response.StatusCode := 403;
  Response.Content := '{"error":"Permission denied"}';
  Exit;
end;
```

### Claims de rôle (RBAC)

```pascal
type
  TRole = (rGuest, rUser, rModerator, rAdmin, rSuperAdmin);
  TRoles = set of TRole;

const
  RoleNames: array[TRole] of string = (
    'guest', 'user', 'moderator', 'admin', 'superadmin'
  );

function CreateJWTWithRoles(const UserID: string;
                            const Roles: TRoles): string;
var
  Payload: TJSONObject;
  RolesArray: TJSONArray;
  Role: TRole;
begin
  Payload := TJSONObject.Create;
  try
    Payload.Add('sub', UserID);
    Payload.Add('iat', DateTimeToUnix(Now));
    Payload.Add('exp', DateTimeToUnix(Now + (1/24)));

    // Ajouter les rôles
    RolesArray := TJSONArray.Create;
    for Role := Low(TRole) to High(TRole) do
    begin
      if Role in Roles then
        RolesArray.Add(RoleNames[Role]);
    end;
    Payload.Add('roles', RolesArray);

    Result := CreateJWTFromJSON(Payload.AsJSON, SECRET_KEY);
  finally
    Payload.Free;
  end;
end;

function HasRole(const Token: string; const RequiredRole: TRole): Boolean;
var
  Payload: TJSONObject;
  RolesArray: TJSONArray;
  i: Integer;
  Role: string;
begin
  Result := False;

  Payload := DecodeJWTToJSON(Token);
  try
    if not Payload.Find('roles', jtArray) then
      Exit;

    RolesArray := TJSONArray(Payload.Get('roles'));

    for i := 0 to RolesArray.Count - 1 do
    begin
      Role := RolesArray[i].AsString;
      if Role = RoleNames[RequiredRole] then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    Payload.Free;
  end;
end;

// Middleware de vérification de rôle
procedure RequireRole(Request: TRequest; Response: TResponse;
                      const MinRole: TRole);
var
  Token: string;
begin
  Token := ExtractToken(Request);

  if not HasRole(Token, MinRole) then
  begin
    Response.StatusCode := 403;
    Response.Content := Format(
      '{"error":"requires_%s_role"}', [RoleNames[MinRole]]
    );
    Abort;
  end;
end;

// Utilisation
procedure HandleAdminEndpoint(Request: TRequest; Response: TResponse);
begin
  RequireRole(Request, Response, rAdmin);

  // L'utilisateur a le rôle admin
  Response.Content := '{"message":"Admin area"}';
end;
```

## JWT pour différents cas d'usage

### 1. JWT pour authentification API

```pascal
type
  TAPIAuthService = class
  private
    FSecretKey: string;
  public
    constructor Create(const SecretKey: string);
    function Login(const Email, Password: string): string;
    function ValidateToken(const Token: string): Boolean;
    function GetUserIDFromToken(const Token: string): string;
  end;

constructor TAPIAuthService.Create(const SecretKey: string);
begin
  inherited Create;
  FSecretKey := SecretKey;
end;

function TAPIAuthService.Login(const Email, Password: string): string;
var
  User: TUser;
  Payload: TJWTPayload;
begin
  // Vérifier les identifiants
  User := AuthenticateUser(Email, Password);
  if User.ID = 0 then
    raise Exception.Create('Invalid credentials');

  // Créer le JWT
  Payload.Sub := IntToStr(User.ID);
  Payload.Email := User.Email;
  Payload.Name := User.Name;
  Payload.Role := User.Role;
  Payload.Iat := DateTimeToUnix(Now);
  Payload.Exp := DateTimeToUnix(Now + (1/24));

  Result := CreateJWT(Payload, FSecretKey);

  // Logger la connexion
  LogSecurityEvent('USER_LOGIN', 'User: ' + Email);
end;

function TAPIAuthService.ValidateToken(const Token: string): Boolean;
begin
  Result := VerifyJWT(Token, FSecretKey) and ValidateJWTClaims(Token);
end;

function TAPIAuthService.GetUserIDFromToken(const Token: string): string;
var
  Payload: TJWTPayload;
begin
  if DecodeJWT(Token, Payload) then
    Result := Payload.Sub
  else
    Result := '';
end;
```

### 2. JWT pour réinitialisation de mot de passe

```pascal
function CreatePasswordResetToken(const Email: string): string;
var
  Payload: TJSONObject;
  Token: string;
begin
  Payload := TJSONObject.Create;
  try
    Payload.Add('email', Email);
    Payload.Add('purpose', 'password_reset');
    Payload.Add('iat', DateTimeToUnix(Now));
    Payload.Add('exp', DateTimeToUnix(Now + (1/24))); // Expire dans 1 heure
    Payload.Add('jti', GenerateUUID); // Identifiant unique

    Token := CreateJWTFromJSON(Payload.AsJSON, PASSWORD_RESET_SECRET);

    // Stocker le JTI pour éviter la réutilisation
    StoreUsedToken(Payload.Get('jti', ''), Payload.Get('exp', Int64(0)));

    Result := Token;
  finally
    Payload.Free;
  end;
end;

function ValidatePasswordResetToken(const Token: string;
                                    out Email: string): Boolean;
var
  Payload: TJSONObject;
  Purpose, JTI: string;
begin
  Result := False;

  if not VerifyJWT(Token, PASSWORD_RESET_SECRET) then
    Exit;

  Payload := DecodeJWTToJSON(Token);
  try
    // Vérifier le purpose
    Purpose := Payload.Get('purpose', '');
    if Purpose <> 'password_reset' then
      Exit;

    // Vérifier que le token n'a pas déjà été utilisé
    JTI := Payload.Get('jti', '');
    if IsTokenUsed(JTI) then
    begin
      LogSecurityEvent('TOKEN_REUSE_ATTEMPT', 'JTI: ' + JTI);
      Exit;
    end;

    // Marquer comme utilisé
    MarkTokenAsUsed(JTI);

    Email := Payload.Get('email', '');
    Result := Email <> '';
  finally
    Payload.Free;
  end;
end;
```

### 3. JWT pour email de vérification

```pascal
function CreateEmailVerificationToken(const Email: string): string;
var
  Payload: TJSONObject;
begin
  Payload := TJSONObject.Create;
  try
    Payload.Add('email', Email);
    Payload.Add('purpose', 'email_verification');
    Payload.Add('iat', DateTimeToUnix(Now));
    Payload.Add('exp', DateTimeToUnix(Now + 7)); // 7 jours

    Result := CreateJWTFromJSON(Payload.AsJSON, EMAIL_VERIFICATION_SECRET);
  finally
    Payload.Free;
  end;
end;

function VerifyEmailToken(const Token: string): string;
var
  Payload: TJSONObject;
begin
  Result := '';

  if not VerifyJWT(Token, EMAIL_VERIFICATION_SECRET) then
    Exit;

  Payload := DecodeJWTToJSON(Token);
  try
    if Payload.Get('purpose', '') = 'email_verification' then
      Result := Payload.Get('email', '');
  finally
    Payload.Free;
  end;
end;
```

### 4. JWT pour Single Sign-On (SSO)

```pascal
type
  TSSOService = class
  private
    FIssuer: string;
    FPrivateKey: string;
  public
    constructor Create(const Issuer, PrivateKeyFile: string);
    function CreateSSOToken(const UserID: string;
                            const TargetService: string): string;
    function ValidateSSOToken(const Token: string): Boolean;
  end;

function TSSOService.CreateSSOToken(const UserID: string;
                                     const TargetService: string): string;
var
  Payload: TJSONObject;
begin
  Payload := TJSONObject.Create;
  try
    Payload.Add('iss', FIssuer);           // Émetteur (notre service SSO)
    Payload.Add('sub', UserID);             // ID utilisateur
    Payload.Add('aud', TargetService);      // Service cible
    Payload.Add('iat', DateTimeToUnix(Now));
    Payload.Add('exp', DateTimeToUnix(Now + (5 / 1440))); // 5 minutes
    Payload.Add('jti', GenerateUUID);

    // Signer avec RSA pour que les autres services puissent vérifier
    Result := SignJWTWithRSA(Payload.AsJSON, FPrivateKey);
  finally
    Payload.Free;
  end;
end;
```

## Testing et débogage JWT

### Décodeur JWT pour tests

```pascal
procedure DebugJWT(const Token: string);
var
  Parts: TStringList;
  Header, Payload: string;
begin
  WriteLn('=== JWT Debug ===');
  WriteLn('Token: ', Token);
  WriteLn;

  Parts := TStringList.Create;
  try
    Parts.Delimiter := '.';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Token;
    if Parts.Count <> 3 then
    begin
      WriteLn('ERREUR: Format invalide');
      Exit;
    end;

    // Décoder le header
    Header := Base64URLDecode(Parts[0]);
    WriteLn('Header:');
    WriteLn(FormatJSON(Header));
    WriteLn;

    // Décoder le payload
    Payload := Base64URLDecode(Parts[1]);
    WriteLn('Payload:');
    WriteLn(FormatJSON(Payload));
    WriteLn;

    // Afficher la signature (premiers caractères)
    WriteLn('Signature: ', Copy(Parts[2], 1, 20), '...');
    WriteLn;

    // Vérifier la validité
    if VerifyJWT(Token, SECRET_KEY) then
      WriteLn('✓ Signature valide')
    else
      WriteLn('✗ Signature invalide');

    if not IsJWTExpired(Token) then
      WriteLn('✓ Token non expiré')
    else
      WriteLn('✗ Token expiré');
  finally
    Parts.Free;
  end;
end;
```

### Tests unitaires

```pascal
uses
  fpcunit, testregistry;

type
  TJWTTests = class(TTestCase)
  published
    procedure TestCreateJWT;
    procedure TestVerifyValidJWT;
    procedure TestRejectInvalidSignature;
    procedure TestRejectExpiredToken;
    procedure TestRejectNoneAlgorithm;
  end;

procedure TJWTTests.TestCreateJWT;
var
  Payload: TJWTPayload;
  Token: string;
begin
  Payload.Sub := '123';
  Payload.Name := 'Test User';
  Payload.Iat := DateTimeToUnix(Now);
  Payload.Exp := DateTimeToUnix(Now + 1);

  Token := CreateJWT(Payload, 'test-secret');

  AssertTrue('Token should not be empty', Token <> '');
  // Un JWT valide a le format xxx.yyy.zzz (exactement 2 points)
  AssertTrue('Token should have 3 parts',
    (Pos('.', Token) > 0) and
    (Pos('.', Copy(Token, Pos('.', Token) + 1, Length(Token))) > 0));
end;

procedure TJWTTests.TestVerifyValidJWT;
var
  Payload: TJWTPayload;
  Token: string;
begin
  Payload.Sub := '123';
  Payload.Iat := DateTimeToUnix(Now);
  Payload.Exp := DateTimeToUnix(Now + 1);

  Token := CreateJWT(Payload, 'test-secret');

  AssertTrue('Valid token should verify',
    VerifyJWT(Token, 'test-secret'));
end;

procedure TJWTTests.TestRejectInvalidSignature;
var
  Payload: TJWTPayload;
  Token: string;
begin
  Payload.Sub := '123';
  Payload.Iat := DateTimeToUnix(Now);
  Payload.Exp := DateTimeToUnix(Now + 1);

  Token := CreateJWT(Payload, 'test-secret');

  AssertFalse('Wrong secret should fail',
    VerifyJWT(Token, 'wrong-secret'));
end;

procedure TJWTTests.TestRejectExpiredToken;
var
  Payload: TJWTPayload;
  Token: string;
begin
  Payload.Sub := '123';
  Payload.Iat := DateTimeToUnix(Now - 2);
  Payload.Exp := DateTimeToUnix(Now - 1); // Expiré hier

  Token := CreateJWT(Payload, 'test-secret');

  AssertTrue('Expired token should be detected',
    IsJWTExpired(Token));
end;
```

## Considérations multi-plateformes

### Différences Windows vs Ubuntu

**Stockage des clés secrètes** :

**Windows** :
```pascal
{$IFDEF WINDOWS}
function GetSecretKey: string;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKeyReadOnly('SOFTWARE\MonApp\Security') then
      Result := Registry.ReadString('JWTSecret')
    else
      raise Exception.Create('JWT secret not configured');
  finally
    Registry.Free;
  end;
end;
{$ENDIF}
```

**Ubuntu/Linux** :
```pascal
{$IFDEF UNIX}
function GetSecretKey: string;
var
  F: TextFile;
  SecretFile: string;
begin
  SecretFile := '/etc/monapp/jwt.secret';

  if not FileExists(SecretFile) then
    raise Exception.Create('JWT secret file not found');

  // Vérifier les permissions (doit être 600)
  if not CheckFilePermissions(SecretFile, '600') then
    raise Exception.Create('Insecure permissions on secret file');

  AssignFile(F, SecretFile);
  Reset(F);
  try
    ReadLn(F, Result);
    Result := Trim(Result);
  finally
    CloseFile(F);
  end;
end;
{$ENDIF}
```

**Timestamp Unix** :

Les deux plateformes utilisent le même format (secondes depuis 1970-01-01), donc pas de différence.

```pascal
// Identique sur Windows et Ubuntu
function DateTimeToUnix(const AValue: TDateTime): Int64;
begin
  Result := Round((AValue - UnixDateDelta) * SecsPerDay);
end;

function UnixToDateTime(const AValue: Int64): TDateTime;
begin
  Result := UnixDateDelta + (AValue / SecsPerDay);
end;
```

## Performance et optimisation

### Cache de validation JWT

```pascal
type
  TJWTCache = class
  private
    type
      TCachedValidation = record
        IsValid: Boolean;
        CachedAt: TDateTime;
        UserID: string;
      end;
    var
      FCache: TDictionary<string, TCachedValidation>;
      FCacheDuration: TDateTime;
  public
    constructor Create(CacheDurationMinutes: Integer = 5);
    destructor Destroy; override;
    function ValidateToken(const Token, Secret: string): Boolean;
    procedure Clear;
  end;

constructor TJWTCache.Create(CacheDurationMinutes: Integer);
begin
  inherited Create;
  FCache := TDictionary<string, TCachedValidation>.Create;
  FCacheDuration := CacheDurationMinutes / 1440; // Minutes en jours
end;

destructor TJWTCache.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

function TJWTCache.ValidateToken(const Token, Secret: string): Boolean;
var
  Cached: TCachedValidation;
  TokenHash: string;
  Payload: TJWTPayload;
begin
  // Utiliser un hash du token comme clé (pour économiser mémoire)
  TokenHash := SHA256Hash(Token);

  // Vérifier le cache
  if FCache.TryGetValue(TokenHash, Cached) then
  begin
    if Now - Cached.CachedAt < FCacheDuration then
    begin
      Result := Cached.IsValid;
      Exit;
    end;
  end;

  // Valider et mettre en cache
  Result := VerifyJWT(Token, Secret) and ValidateJWTClaims(Token);

  Cached.IsValid := Result;
  Cached.CachedAt := Now;
  if Result then
  begin
    if DecodeJWT(Token, Payload) then
      Cached.UserID := Payload.Sub;
  end;

  FCache.AddOrSetValue(TokenHash, Cached);
end;

procedure TJWTCache.Clear;
begin
  FCache.Clear;
end;
```

### Optimisation de la vérification de signature

```pascal
// Pré-calculer la clé pour HMAC
type
  TJWTVerifier = class
  private
    FSecretHash: TBytes;
  public
    constructor Create(const Secret: string);
    function Verify(const Token: string): Boolean;
  end;

constructor TJWTVerifier.Create(const Secret: string);
begin
  inherited Create;
  // Pré-hasher la clé secrète pour optimiser
  FSecretHash := HashSecret(Secret);
end;

function TJWTVerifier.Verify(const Token: string): Boolean;
begin
  // Utiliser la clé pré-hachée (plus rapide)
  Result := VerifyWithPreHashedSecret(Token, FSecretHash);
end;
```

## Ressources et documentation

### Standards et spécifications

- **JWT (RFC 7519)** : https://datatracker.ietf.org/doc/html/rfc7519
  - Standard définissant la structure et le format des JWT

- **JWS (RFC 7515)** : JSON Web Signature
  - https://datatracker.ietf.org/doc/html/rfc7515
  - Spécification pour la signature des JWT

- **JWE (RFC 7516)** : JSON Web Encryption
  - https://datatracker.ietf.org/doc/html/rfc7516
  - Chiffrement de JWT (pour informations sensibles)

- **JWA (RFC 7518)** : JSON Web Algorithms
  - https://datatracker.ietf.org/doc/html/rfc7518
  - Algorithmes cryptographiques pour JWT

- **JWK (RFC 7517)** : JSON Web Key
  - https://datatracker.ietf.org/doc/html/rfc7517
  - Représentation des clés cryptographiques en JSON

### Outils en ligne

- **JWT.io** : https://jwt.io/
  - Décodeur et vérificateur de JWT
  - Debugger visuel
  - Bibliothèques pour différents langages

- **JWT Tool** : https://www.jwtool.io/
  - Générateur et vérificateur de JWT
  - Support de différents algorithmes

- **JWT Inspector (Chrome Extension)**
  - Extension de navigateur pour inspecter les JWT

### Bibliothèques FreePascal

- **jwt-paslib** : https://github.com/gcarreno/jwt-paslib
  - Bibliothèque JWT complète pour FreePascal
  - Support HMAC et RSA

- **mORMot** : https://github.com/synopse/mORMot2
  - Framework complet avec support JWT intégré
  - Haute performance

- **JOSE (JSON Object Signing and Encryption)** pour FreePascal
  - Implémentations diverses dans la communauté

### Documentation de sécurité

- **OWASP JWT Cheat Sheet** :
  - https://cheatsheetseries.owasp.org/cheatsheets/JSON_Web_Token_for_Java_Cheat_Sheet.html
  - Bonnes pratiques de sécurité

- **JWT Handbook** par Auth0 :
  - Guide complet sur les JWT

- **Critical vulnerabilities in JWT libraries** :
  - https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries/

## Exemple d'application complète

### Serveur API avec authentification JWT

```pascal
program JWTAPIServer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpserver, httpdefs, fpjson, jsonparser,
  DCPsha256;

type
  TAPIServer = class(TFPHTTPServer)
  private
    FSecretKey: string;
    function ValidateJWT(const Token: string): Boolean;
    function ExtractUserID(const Token: string): string;
  public
    constructor Create(const SecretKey: string); reintroduce;
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  end;

const
  SECRET_KEY = 'votre-super-secret-key-de-64-caracteres-minimum-ici';

constructor TAPIServer.Create(const SecretKey: string);
begin
  inherited Create(nil);
  FSecretKey := SecretKey;
end;

function TAPIServer.ValidateJWT(const Token: string): Boolean;
begin
  Result := VerifyJWT(Token, FSecretKey) and not IsJWTExpired(Token);
end;

function TAPIServer.ExtractUserID(const Token: string): string;
var
  Payload: TJWTPayload;
begin
  if DecodeJWT(Token, Payload) then
    Result := Payload.Sub
  else
    Result := '';
end;

procedure TAPIServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  AuthHeader, Token, UserID: string;
  Email, Password: string;
  Payload: TJWTPayload;
  Response: TJSONObject;
begin
  AResponse.ContentType := 'application/json';

  // Route: POST /login
  if (ARequest.Method = 'POST') and (ARequest.URI = '/login') then
  begin
    // Authentifier et créer un JWT
    Email := ARequest.ContentFields.Values['email'];
    Password := ARequest.ContentFields.Values['password'];

    // Vérifier les identifiants (simplifié pour l'exemple)
    if (Email = 'user@example.com') and (Password = 'password123') then
    begin
      Payload.Sub := '12345';
      Payload.Email := Email;
      Payload.Name := 'John Doe';
      Payload.Role := 'user';
      Payload.Iat := DateTimeToUnix(Now);
      Payload.Exp := DateTimeToUnix(Now + (1/24)); // 1 heure

      Token := CreateJWT(Payload, FSecretKey);

      Response := TJSONObject.Create;
      try
        Response.Add('success', True);
        Response.Add('token', Token);
        Response.Add('expires_in', 3600);
        AResponse.Content := Response.AsJSON;
        AResponse.Code := 200;
      finally
        Response.Free;
      end;
    end
    else
    begin
      Response := TJSONObject.Create;
      try
        Response.Add('success', False);
        Response.Add('error', 'Invalid credentials');
        AResponse.Content := Response.AsJSON;
        AResponse.Code := 401;
      finally
        Response.Free;
      end;
    end;
    Exit;
  end;

  // Route protégée: GET /profile
  if (ARequest.Method = 'GET') and (ARequest.URI = '/profile') then
  begin
    // Extraire le token
    AuthHeader := ARequest.GetHeader('Authorization');

    if Copy(AuthHeader, 1, 7) <> 'Bearer ' then
    begin
      AResponse.Code := 401;
      AResponse.Content := '{"error":"Missing authorization header"}';
      Exit;
    end;

    Token := Copy(AuthHeader, 8, Length(AuthHeader));

    // Valider le token
    if not ValidateJWT(Token) then
    begin
      AResponse.Code := 401;
      AResponse.Content := '{"error":"Invalid or expired token"}';
      Exit;
    end;

    // Extraire l'ID utilisateur
    UserID := ExtractUserID(Token);

    // Retourner le profil
    Response := TJSONObject.Create;
    try
      Response.Add('user_id', UserID);
      Response.Add('name', 'John Doe');
      Response.Add('email', 'user@example.com');
      Response.Add('role', 'user');
      AResponse.Content := Response.AsJSON;
      AResponse.Code := 200;
    finally
      Response.Free;
    end;
    Exit;
  end;

  // Route non trouvée
  AResponse.Code := 404;
  AResponse.Content := '{"error":"Route not found"}';
end;

var
  Server: TAPIServer;
begin
  Server := TAPIServer.Create(SECRET_KEY);
  try
    Server.Port := 8080;
    Server.Active := True;

    WriteLn('Serveur API JWT démarré sur http://localhost:8080');
    WriteLn;
    WriteLn('Endpoints disponibles:');
    WriteLn('  POST /login       - Authentification (email/password)');
    WriteLn('  GET  /profile     - Profil utilisateur (nécessite JWT)');
    WriteLn;
    WriteLn('Exemple de requête login:');
    WriteLn('  curl -X POST http://localhost:8080/login \');
    WriteLn('    -H "Content-Type: application/x-www-form-urlencoded" \');
    WriteLn('    -d "email=user@example.com&password=password123"');
    WriteLn;
    WriteLn('Appuyez sur Entrée pour arrêter le serveur...');
    ReadLn;
  finally
    Server.Free;
  end;
end.
```

### Client de test pour l'API

```pascal
program JWTAPIClient;

{$mode objfpc}{$H+}

uses
  SysUtils, httpsend, ssl_openssl, fpjson, jsonparser;

function Login(const Email, Password: string): string;
var
  HTTP: THTTPSend;
  PostData, Response: string;
  JSON: TJSONData;
begin
  Result := '';

  HTTP := THTTPSend.Create;
  try
    PostData := Format('email=%s&password=%s', [Email, Password]);
    HTTP.Document.Write(PostData[1], Length(PostData));
    HTTP.MimeType := 'application/x-www-form-urlencoded';

    if HTTP.HTTPMethod('POST', 'http://localhost:8080/login') then
    begin
      SetLength(Response, HTTP.Document.Size);
      HTTP.Document.Position := 0;
      HTTP.Document.Read(Response[1], HTTP.Document.Size);

      JSON := GetJSON(Response);
      try
        if TJSONObject(JSON).Get('success', False) then
          Result := TJSONObject(JSON).Get('token', '');
      finally
        JSON.Free;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

function GetProfile(const Token: string): string;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.Headers.Add('Authorization: Bearer ' + Token);

    if HTTP.HTTPMethod('GET', 'http://localhost:8080/profile') then
    begin
      SetLength(Result, HTTP.Document.Size);
      HTTP.Document.Read(Result[1], HTTP.Document.Size);
    end;
  finally
    HTTP.Free;
  end;
end;

var
  Token, Profile: string;
begin
  WriteLn('=== Client API JWT ===');
  WriteLn;

  // 1. Login
  WriteLn('1. Connexion...');
  Token := Login('user@example.com', 'password123');

  if Token <> '' then
  begin
    WriteLn('✓ Connexion réussie');
    WriteLn('Token: ', Copy(Token, 1, 50), '...');
    WriteLn;

    // 2. Accéder au profil
    WriteLn('2. Récupération du profil...');
    Profile := GetProfile(Token);
    WriteLn('Profil:');
    WriteLn(Profile);
  end
  else
  begin
    WriteLn('✗ Échec de la connexion');
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Comparaison JWT vs Sessions

### Tableau comparatif

| Caractéristique | JWT | Sessions |
|-----------------|-----|----------|
| **Stockage serveur** | ❌ Aucun (stateless) | ✅ Nécessaire |
| **Scalabilité horizontale** | ✅ Facile | ⚠️ Complexe (session partagée) |
| **Taille** | ⚠️ Plus grand (~1-2 KB) | ✅ Petit (ID session ~32 bytes) |
| **Révocation** | ⚠️ Difficile | ✅ Immédiate |
| **Performances** | ✅ Pas de requête BD | ⚠️ Requête BD par requête |
| **Sécurité** | ⚠️ Attention aux XSS | ✅ Plus sécurisé si HttpOnly |
| **Données utilisateur** | ✅ Incluses dans le token | ⚠️ Requête BD supplémentaire |
| **Expiration** | ✅ Intégrée | ⚠️ Gérée manuellement |

### Quand utiliser JWT ?

**✅ Utilisez JWT quand** :
- Vous avez besoin d'une architecture stateless
- Vous devez scaler horizontalement facilement
- Vous construisez une API REST publique
- Vous avez plusieurs services/microservices
- Vous voulez éviter les requêtes de base de données pour chaque requête
- Le client est une application mobile ou SPA

**❌ Évitez JWT quand** :
- Vous avez besoin de révocation immédiate
- La taille de la requête est critique
- Vous stockez beaucoup d'informations utilisateur
- Vous avez une application monolithique simple
- La sécurité maximale est requise (serveurs critiques)

### Approche hybride

Combiner JWT et sessions pour le meilleur des deux mondes :

```pascal
type
  THybridAuthService = class
  private
    FSecretKey: string;
  public
    function CreateSession(const UserID: string): TSessionPair;
    function ValidateRequest(const Token: string): Boolean;
    procedure RevokeSession(const SessionID: string);
  end;

type
  TSessionPair = record
    JWT: string;           // Pour l'authentification
    SessionID: string;     // Pour la révocation
  end;

function THybridAuthService.CreateSession(const UserID: string): TSessionPair;
var
  Payload: TJWTPayload;
begin
  // Créer un SessionID unique
  Result.SessionID := GenerateUUID;

  // Créer le JWT avec le SessionID
  Payload.Sub := UserID;
  Payload.Jti := Result.SessionID; // JWT ID = Session ID
  Payload.Iat := DateTimeToUnix(Now);
  Payload.Exp := DateTimeToUnix(Now + (1/24));

  Result.JWT := CreateJWT(Payload, FSecretKey);

  // Stocker la session pour permettre la révocation
  StoreActiveSession(Result.SessionID, UserID, Payload.Exp);
end;

function THybridAuthService.ValidateRequest(const Token: string): Boolean;
var
  Payload: TJWTPayload;
begin
  // 1. Vérifier la signature JWT
  if not VerifyJWT(Token, FSecretKey) then
    Exit(False);

  // 2. Décoder et extraire le SessionID
  if not DecodeJWT(Token, Payload) then
    Exit(False);

  // 3. Vérifier que la session est toujours active
  Result := IsSessionActive(Payload.Jti);
end;

procedure THybridAuthService.RevokeSession(const SessionID: string);
begin
  // Supprimer de la liste des sessions actives
  RemoveActiveSession(SessionID);

  // Le JWT devient invalide immédiatement, même s'il n'est pas expiré
end;
```

## Migration depuis un système existant

### Depuis sessions vers JWT

```pascal
type
  TMigrationHelper = class
  public
    class function ConvertSessionToJWT(const SessionID: string): string;
    class function SupportBothMethods(Request: TRequest): Boolean;
  end;

class function TMigrationHelper.ConvertSessionToJWT(const SessionID: string): string;
var
  Session: TSession;
  Payload: TJWTPayload;
begin
  // Récupérer la session existante
  Session := GetSessionByID(SessionID);

  if Session.ID = '' then
    raise Exception.Create('Session not found');

  // Convertir en JWT
  Payload.Sub := IntToStr(Session.UserID);
  Payload.Email := Session.UserEmail;
  Payload.Name := Session.UserName;
  Payload.Iat := DateTimeToUnix(Now);
  Payload.Exp := DateTimeToUnix(Session.ExpiresAt);

  Result := CreateJWT(Payload, SECRET_KEY);

  // Optionnel : marquer la session comme migrée
  MarkSessionAsMigrated(SessionID);
end;

class function TMigrationHelper.SupportBothMethods(Request: TRequest): Boolean;
var
  SessionID, JWT: string;
begin
  Result := False;

  // Essayer JWT d'abord
  JWT := ExtractJWT(Request);
  if JWT <> '' then
  begin
    Result := ValidateJWT(JWT);
    if Result then
      Exit;
  end;

  // Fallback sur session traditionnelle
  SessionID := Request.Cookies['session_id'];
  if SessionID <> '' then
  begin
    Result := ValidateSession(SessionID);

    // Si la session est valide, proposer un JWT
    if Result then
    begin
      JWT := ConvertSessionToJWT(SessionID);
      Request.SuggestedJWT := JWT; // Pour que le client puisse migrer
    end;
  end;
end;
```

## Audit et monitoring

### Logger les événements JWT

```pascal
type
  TJWTEvent = (
    jeCreated,
    jeVerified,
    jeExpired,
    jeInvalidSignature,
    jeRevoked,
    jeMissingClaims
  );

procedure LogJWTEvent(const Event: TJWTEvent; const Details: string);
var
  EventName: string;
begin
  case Event of
    jeCreated: EventName := 'JWT_CREATED';
    jeVerified: EventName := 'JWT_VERIFIED';
    jeExpired: EventName := 'JWT_EXPIRED';
    jeInvalidSignature: EventName := 'JWT_INVALID_SIGNATURE';
    jeRevoked: EventName := 'JWT_REVOKED';
    jeMissingClaims: EventName := 'JWT_MISSING_CLAIMS';
  end;

  WriteToLog(Format('[%s] %s - %s - IP: %s - Time: %s',
    [EventName, Details, GetCurrentUser, GetClientIP, DateTimeToStr(Now)]));

  // Alerter si événement suspect
  if Event in [jeInvalidSignature, jeMissingClaims] then
    SendSecurityAlert(EventName, Details);
end;

// Utilisation
function VerifyJWTWithLogging(const Token: string): Boolean;
begin
  Result := VerifyJWT(Token, SECRET_KEY);

  if Result then
    LogJWTEvent(jeVerified, 'Token valid')
  else
  begin
    if IsJWTExpired(Token) then
      LogJWTEvent(jeExpired, 'Token expired')
    else
      LogJWTEvent(jeInvalidSignature, 'Invalid signature');
  end;
end;
```

### Métriques et statistiques

```pascal
type
  TJWTMetrics = class
  private
    FTokensCreated: Int64;
    FTokensVerified: Int64;
    FTokensExpired: Int64;
    FTokensRevoked: Int64;
    FInvalidTokens: Int64;
  public
    procedure IncrementCreated;
    procedure IncrementVerified;
    procedure IncrementExpired;
    procedure IncrementRevoked;
    procedure IncrementInvalid;
    function GetStats: TJSONObject;
  end;

procedure TJWTMetrics.IncrementCreated;
begin
  InterlockedIncrement(FTokensCreated);
end;

function TJWTMetrics.GetStats: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('tokens_created', FTokensCreated);
  Result.Add('tokens_verified', FTokensVerified);
  Result.Add('tokens_expired', FTokensExpired);
  Result.Add('tokens_revoked', FTokensRevoked);
  Result.Add('invalid_tokens', FInvalidTokens);
  Result.Add('success_rate',
    (FTokensVerified / Max(FTokensCreated, 1)) * 100);
end;

// Exposition via endpoint
procedure HandleMetrics(Request: TRequest; Response: TResponse);
begin
  Response.ContentType := 'application/json';
  Response.Content := GlobalJWTMetrics.GetStats.AsJSON;
end;
```

## Conclusion

Les JSON Web Tokens (JWT) sont devenus un standard incontournable pour l'authentification dans les applications modernes, particulièrement pour les architectures distribuées, les APIs REST et les applications mobiles.

### Points clés à retenir

**✅ Structure et format** :
- Header + Payload + Signature, séparés par des points
- Encodé en Base64URL (pas chiffré !)
- Auto-contenu et vérifiable

**✅ Sécurité** :
- Toujours utiliser HTTPS
- Clés secrètes fortes (64+ caractères aléatoires)
- Courte durée de vie (15 min - 1h pour access tokens)
- Valider tous les claims (exp, iss, aud, etc.)
- Se protéger contre "none algorithm" et autres attaques

**✅ Implémentation** :
- HMAC-SHA256 (HS256) pour la plupart des cas
- RSA (RS256) pour les architectures distribuées
- Utiliser des bibliothèques éprouvées
- Implémenter un système de refresh tokens

**✅ Stockage** :
- HttpOnly cookies en web (protection XSS)
- Stockage sécurisé du système en desktop (DPAPI, Secret Service)
- Jamais en localStorage sans chiffrement

**✅ Gestion** :
- Claims personnalisés pour permissions et rôles
- Révocation via blacklist ou approche hybride
- Logging et monitoring des événements
- Tests unitaires complets

**✅ Multi-plateforme** :
- Même format sur Windows et Ubuntu
- Différences uniquement dans le stockage sécurisé
- Timestamp Unix universel

### Quand utiliser JWT ?

**Applications idéales** :
- APIs REST stateless
- Microservices
- Applications mobiles
- Single Page Applications (SPA)
- Architectures distribuées
- Single Sign-On (SSO)

**À éviter pour** :
- Applications nécessitant révocation immédiate
- Très haute sécurité (finance, santé)
- Limitations strictes de bande passante
- Applications monolithiques simples

### Avec FreePascal et Lazarus

FreePascal offre tous les outils nécessaires pour implémenter JWT de manière sécurisée :
- DCPCrypt pour le hachage et la signature
- OpenSSL pour RSA et algorithmes avancés
- fpjson pour la manipulation JSON
- Synapse/Indy pour les communications HTTPS
- Support multi-plateforme natif

L'implémentation de JWT en FreePascal est performante, sécurisée et portable entre Windows et Ubuntu, ce qui en fait un excellent choix pour vos applications d'authentification moderne.

**Prochaine section** : 17.5 Hashing et signatures numériques

⏭️ [Hashing et signatures numériques](/17-securite-cryptographie/05-hashing-signatures-numeriques.md)
