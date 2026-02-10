🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.5 Hashing et signatures numériques

## Introduction

Le hashing et les signatures numériques sont des concepts fondamentaux de la cryptographie moderne. Ils permettent de garantir l'intégrité des données, d'authentifier leur origine et de détecter toute modification non autorisée.

Dans cette section, nous allons explorer ces techniques cryptographiques, comprendre leur fonctionnement et apprendre à les implémenter avec FreePascal et Lazarus sur Windows et Ubuntu.

## Qu'est-ce que le hashing ?

### Définition

Le hashing (ou hachage) est un processus qui transforme des données de taille variable en une empreinte de taille fixe, appelée **hash** ou **digest**. Cette transformation est :

- **Unidirectionnelle** : Impossible de retrouver les données d'origine à partir du hash
- **Déterministe** : Même entrée = même sortie, toujours
- **Rapide** : Le calcul du hash est très rapide
- **Avalanche effect** : Un petit changement dans l'entrée change complètement le hash
- **Résistante aux collisions** : Très difficile de trouver deux entrées différentes avec le même hash

### Illustration

```
Entrée : "Bonjour le monde"
  ↓ (Fonction de hachage SHA-256)
Hash : 7f83b1657ff1fc53b92dc18148a1d65dfc2d4b1fa3d677284addd200126d9069

Entrée : "bonjour le monde" (petit 'b')
  ↓ (Fonction de hachage SHA-256)
Hash : 509af1b7c12f8b07f772e87c43fc44f4c7a3cf59c1a06e3c2d91e956d3d8e5b3

→ Complètement différent !
```

### À quoi sert le hashing ?

**1. Vérification d'intégrité** :
```
Télécharger un fichier → Vérifier son hash  
Hash attendu : abc123...  
Hash calculé : abc123... ✓ Fichier non corrompu
```

**2. Stockage de mots de passe** :
```
Mot de passe → Hash → Stockage en base de données  
Au login : hash(password saisi) == hash stocké ?
```

**3. Signatures numériques** :
```
Document → Hash → Chiffrement avec clé privée = Signature
```

**4. Identification de fichiers** :
```
Deux fichiers ont-ils le même contenu ?
→ Comparer leurs hashs (beaucoup plus rapide)
```

## Algorithmes de hashing courants

### Vue d'ensemble

| Algorithme | Taille hash | Statut | Utilisation |
|------------|-------------|--------|-------------|
| **MD5** | 128 bits (32 hex) | ❌ Cassé | À éviter |
| **SHA-1** | 160 bits (40 hex) | ⚠️ Faible | Legacy uniquement |
| **SHA-256** | 256 bits (64 hex) | ✅ Sécurisé | Recommandé |
| **SHA-512** | 512 bits (128 hex) | ✅ Très sécurisé | Haute sécurité |
| **SHA-3** | Variable | ✅ Moderne | Alternative à SHA-2 |
| **BLAKE2** | Variable | ✅ Rapide | Haute performance |

### MD5 (Message Digest 5)

**Historique** : Créé en 1991, largement utilisé jusqu'aux années 2000.

**Statut** : ❌ **Cassé** - Des collisions ont été trouvées.

**Ne plus utiliser pour** :
- Stockage de mots de passe
- Signatures numériques
- Certificats SSL

**Peut encore servir pour** :
- Checksums non-critiques (vérification d'intégrité basique)
- Identifiants uniques non-sécuritaires

**Exemple en FreePascal** :
```pascal
uses
  DCPmd5, DCPcrypt2;

function CalculateMD5(const Data: string): string;  
var
  Hash: TDCP_md5;
  Digest: array[0..15] of Byte;
  i: Integer;
begin
  Hash := TDCP_md5.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);

    Result := '';
    for i := 0 to 15 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
  end;
end;

// Utilisation
var
  Hash: string;
begin
  Hash := CalculateMD5('Hello World');
  WriteLn('MD5: ', Hash);
  // MD5: B10A8DB164E0754105B7A99BE72E3FE5
end.
```

### SHA-1 (Secure Hash Algorithm 1)

**Historique** : Créé par la NSA en 1995.

**Statut** : ⚠️ **Déprécié** - Collisions théoriquement et pratiquement trouvées (2017).

**Ne plus utiliser pour** :
- Nouveaux développements
- Signatures numériques
- Certificats SSL (interdit depuis 2017)

**Exemple en FreePascal** :
```pascal
uses
  DCPsha1, DCPcrypt2;

function CalculateSHA1(const Data: string): string;  
var
  Hash: TDCP_sha1;
  Digest: array[0..19] of Byte;
  i: Integer;
begin
  Hash := TDCP_sha1.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);

    Result := '';
    for i := 0 to 19 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
  end;
end;

// Utilisation
begin
  WriteLn('SHA-1: ', CalculateSHA1('Hello World'));
  // SHA-1: 0A4D55A8D778E5022FAB701977C5D840BBC486D0
end.
```

### SHA-256 (SHA-2 family)

**Historique** : Créé par la NSA en 2001.

**Statut** : ✅ **Recommandé** - Standard actuel.

**Utilisations** :
- Stockage de mots de passe (avec salt)
- Signatures numériques
- Certificats SSL
- Blockchain (Bitcoin utilise SHA-256)
- Vérification d'intégrité

**Exemple en FreePascal** :
```pascal
uses
  DCPsha256, DCPcrypt2;

function CalculateSHA256(const Data: string): string;  
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
  i: Integer;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);

    Result := '';
    for i := 0 to 31 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
  end;
end;

// Utilisation
begin
  WriteLn('SHA-256: ', CalculateSHA256('Hello World'));
  // SHA-256: A591A6D40BF420404A011733CFB7B190D62C65BF0BCDA32B57B277D9AD9F146E
end.
```

### SHA-512 (SHA-2 family)

**Statut** : ✅ **Très sécurisé**

**Avantages** :
- Plus sécurisé que SHA-256
- Plus rapide sur architectures 64 bits
- Résistant aux attaques quantiques (pour l'instant)

**Exemple en FreePascal** :
```pascal
uses
  DCPsha512, DCPcrypt2;

function CalculateSHA512(const Data: string): string;  
var
  Hash: TDCP_sha512;
  Digest: array[0..63] of Byte;
  i: Integer;
begin
  Hash := TDCP_sha512.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);

    Result := '';
    for i := 0 to 63 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
  end;
end;

// Utilisation
begin
  WriteLn('SHA-512: ', CalculateSHA512('Hello World'));
  // Résultat : 128 caractères hexadécimaux
end.
```

## Hashing de fichiers

### Calculer le hash d'un fichier

```pascal
uses
  Classes, SysUtils, DCPsha256;

function CalculateFileSHA256(const FileName: string): string;  
var
  FileStream: TFileStream;
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
  Buffer: array[0..8191] of Byte;
  BytesRead: Integer;
  i: Integer;
begin
  Result := '';

  if not FileExists(FileName) then
    raise Exception.Create('Fichier non trouvé');

  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;

    // Lire le fichier par blocs
    repeat
      BytesRead := FileStream.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
        Hash.Update(Buffer, BytesRead);
    until BytesRead = 0;

    Hash.Final(Digest);

    // Convertir en hexadécimal
    for i := 0 to 31 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
    FileStream.Free;
  end;
end;

// Utilisation
var
  FileHash: string;
begin
  FileHash := CalculateFileSHA256('document.pdf');
  WriteLn('Hash du fichier: ', FileHash);
end.
```

### Vérification d'intégrité de fichier

```pascal
function VerifyFileIntegrity(const FileName, ExpectedHash: string): Boolean;  
var
  CalculatedHash: string;
begin
  CalculatedHash := CalculateFileSHA256(FileName);
  Result := (LowerCase(CalculatedHash) = LowerCase(ExpectedHash));

  if Result then
    WriteLn('✓ Fichier intègre')
  else
    WriteLn('✗ Fichier corrompu ou modifié !');
end;

// Utilisation
begin
  VerifyFileIntegrity(
    'ubuntu-22.04.iso',
    '84eed5a40e52bf0e35f27c56e6ae34f32e86bc75cef4cc4f88dc9d1d888b30fb'
  );
end.
```

## Salage (Salt) pour les mots de passe

### Pourquoi saler les mots de passe ?

**Problème sans salt** :
```
Utilisateur 1 : password123 → hash: abc...  
Utilisateur 2 : password123 → hash: abc... (identique !)
```

Un attaquant peut utiliser des **rainbow tables** (tables pré-calculées) pour retrouver les mots de passe.

**Solution avec salt** :
```
Utilisateur 1 : password123 + salt1 → hash: abc...  
Utilisateur 2 : password123 + salt2 → hash: xyz... (différent !)
```

### Implémentation du salage

```pascal
uses
  DCPsha256, DCPcrypt2, SysUtils;

function GenerateSalt(Length: Integer = 16): string;  
var
  Bytes: TBytes;
  i: Integer;
  F: File;
begin
  SetLength(Bytes, Length);

  // Générer des bytes aléatoires
  {$IFDEF WINDOWS}
  // Utiliser l'API Windows pour générer des bytes cryptographiquement sécurisés
  CryptGenRandom(hProv, Length, @Bytes[0]);
  {$ELSE}
  // Utiliser /dev/urandom sur Linux
  AssignFile(F, '/dev/urandom');
  Reset(F, 1);
  BlockRead(F, Bytes[0], Length);
  CloseFile(F);
  {$ENDIF}

  // Convertir en hexadécimal
  Result := '';
  for i := 0 to High(Bytes) do
    Result := Result + IntToHex(Bytes[i], 2);
end;

function HashPasswordWithSalt(const Password, Salt: string): string;  
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
  i: Integer;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Salt + Password); // Salt + Password
    Hash.Final(Digest);

    Result := '';
    for i := 0 to 31 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
  end;
end;

type
  TPasswordHash = record
    Hash: string;
    Salt: string;
  end;

function CreatePasswordHash(const Password: string): TPasswordHash;  
begin
  Result.Salt := GenerateSalt(16);
  Result.Hash := HashPasswordWithSalt(Password, Result.Salt);
end;

function VerifyPassword(const Password: string;
                        const StoredHash: TPasswordHash): Boolean;
var
  ComputedHash: string;
begin
  ComputedHash := HashPasswordWithSalt(Password, StoredHash.Salt);
  Result := (ComputedHash = StoredHash.Hash);
end;

// Utilisation
var
  PasswordHash: TPasswordHash;
begin
  // Enregistrement d'un utilisateur
  PasswordHash := CreatePasswordHash('MonMotDePasse123');

  WriteLn('Hash: ', PasswordHash.Hash);
  WriteLn('Salt: ', PasswordHash.Salt);

  // Stocker Hash et Salt en base de données
  SaveToDatabase(UserID, PasswordHash.Hash, PasswordHash.Salt);

  // Vérification lors du login
  if VerifyPassword('MonMotDePasse123', PasswordHash) then
    WriteLn('✓ Mot de passe correct')
  else
    WriteLn('✗ Mot de passe incorrect');
end.
```

## Fonctions de dérivation de clé (KDF)

### Pourquoi utiliser une KDF ?

Les fonctions de hachage simples (SHA-256, etc.) sont **trop rapides** pour les mots de passe. Un attaquant peut tester des millions de mots de passe par seconde.

Les **KDF (Key Derivation Functions)** sont intentionnellement **lentes** pour ralentir les attaques par force brute.

### PBKDF2 (Password-Based Key Derivation Function 2)

**Principe** : Appliquer un hash plusieurs milliers de fois.

```pascal
uses
  DCPsha256, SysUtils;

function PBKDF2_SHA256(const Password, Salt: string;
                       Iterations: Integer = 10000): string;
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
  TempDigest: array[0..31] of Byte;
  i, j: Integer;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    // Première itération
    Hash.Init;
    Hash.UpdateStr(Salt + Password);
    Hash.Final(Digest);

    // Itérations suivantes
    for i := 1 to Iterations - 1 do
    begin
      Move(Digest, TempDigest, SizeOf(Digest));
      Hash.Init;
      Hash.Update(TempDigest, SizeOf(TempDigest));
      Hash.Final(Digest);
    end;

    // Convertir en hexadécimal
    Result := '';
    for j := 0 to 31 do
      Result := Result + IntToHex(Digest[j], 2);
  finally
    Hash.Free;
  end;
end;

// Utilisation
var
  PasswordHash: string;
begin
  PasswordHash := PBKDF2_SHA256('MonMotDePasse', 'SaltUnique123', 10000);
  WriteLn('PBKDF2: ', PasswordHash);

  // 10000 itérations rend l'attaque beaucoup plus lente
end.
```

**Recommandations** :
- Minimum 10 000 itérations (2023)
- 100 000 itérations pour haute sécurité
- Ajuster selon la puissance du serveur

### bcrypt (recommandé)

bcrypt est spécialement conçu pour le hashing de mots de passe.

**Avantages** :
- Intentionnellement lent
- Facteur de coût ajustable
- Salt intégré automatiquement
- Résistant aux attaques GPU

**Note** : DCPCrypt ne fournit pas bcrypt nativement. Vous pouvez utiliser des bibliothèques externes ou OpenSSL.

```pascal
// Exemple conceptuel avec OpenSSL
uses
  OpenSSL;

function BCryptHash(const Password: string; Cost: Integer = 12): string;  
begin
  // Cost = 12 signifie 2^12 itérations = 4096
  // Augmenter le cost rend le hash plus lent mais plus sécurisé

  Result := BCrypt_Create(Password, BCrypt_GenSalt(Cost));
end;

function BCryptVerify(const Password, Hash: string): Boolean;  
begin
  Result := BCrypt_Verify(Password, Hash);
end;
```

## HMAC (Hash-based Message Authentication Code)

### Qu'est-ce que HMAC ?

HMAC combine un hash et une clé secrète pour créer un **code d'authentification de message**.

**Différences avec un hash simple** :

| Hash simple | HMAC |
|-------------|------|
| Hash(data) | Hash(key + data) |
| Vérification d'intégrité | Authentification + intégrité |
| Pas de clé | Nécessite une clé secrète |

**Utilisation** :
- Vérifier qu'un message n'a pas été modifié
- Vérifier que le message provient bien de l'émetteur prévu
- JWT (JSON Web Tokens)
- API signatures

### Implémentation HMAC-SHA256

```pascal
uses
  DCPsha256, SysUtils;

function HMACSHA256(const Key, Data: string): string;  
var
  Hash: TDCP_sha256;
  KeyBytes: TBytes;
  ipad, opad: array[0..63] of Byte;
  i: Integer;
  InnerHash, OuterHash: array[0..31] of Byte;
const
  BLOCK_SIZE = 64; // SHA-256 block size
begin
  Hash := TDCP_sha256.Create(nil);
  try
    // Préparer la clé
    SetLength(KeyBytes, Length(Key));
    if Length(Key) > 0 then
      Move(Key[1], KeyBytes[0], Length(Key));

    // Si la clé est trop longue, la hasher
    if Length(KeyBytes) > BLOCK_SIZE then
    begin
      Hash.Init;
      Hash.Update(KeyBytes[0], Length(KeyBytes));
      Hash.Final(InnerHash);
      SetLength(KeyBytes, 32);
      Move(InnerHash, KeyBytes[0], 32);
    end;

    // Padding de la clé
    FillChar(ipad, SizeOf(ipad), $36);
    FillChar(opad, SizeOf(opad), $5C);

    for i := 0 to Length(KeyBytes) - 1 do
    begin
      ipad[i] := ipad[i] xor KeyBytes[i];
      opad[i] := opad[i] xor KeyBytes[i];
    end;

    // Hash interne : H((K ⊕ ipad) || message)
    Hash.Init;
    Hash.Update(ipad, BLOCK_SIZE);
    Hash.UpdateStr(Data);
    Hash.Final(InnerHash);

    // Hash externe : H((K ⊕ opad) || hash interne)
    Hash.Init;
    Hash.Update(opad, BLOCK_SIZE);
    Hash.Update(InnerHash, SizeOf(InnerHash));
    Hash.Final(OuterHash);

    // Convertir en hexadécimal
    Result := '';
    for i := 0 to 31 do
      Result := Result + IntToHex(OuterHash[i], 2);
  finally
    Hash.Free;
  end;
end;

// Utilisation
var
  Signature: string;
begin
  Signature := HMACSHA256('secret-key', 'Message à signer');
  WriteLn('HMAC-SHA256: ', Signature);

  // Vérifier l'authenticité
  if HMACSHA256('secret-key', 'Message à signer') = Signature then
    WriteLn('✓ Message authentique')
  else
    WriteLn('✗ Message modifié ou clé invalide');
end.
```

### Application : Signature d'API

```pascal
type
  TAPIRequest = record
    Method: string;      // GET, POST, etc.
    Path: string;        // /api/users
    Timestamp: string;   // ISO 8601
    Body: string;        // JSON payload
  end;

function SignAPIRequest(const Request: TAPIRequest;
                        const APISecret: string): string;
var
  StringToSign: string;
begin
  // Construire la chaîne à signer
  StringToSign := Request.Method + #10 +
                  Request.Path + #10 +
                  Request.Timestamp + #10 +
                  Request.Body;

  // Signer avec HMAC-SHA256
  Result := HMACSHA256(APISecret, StringToSign);
end;

function VerifyAPISignature(const Request: TAPIRequest;
                            const Signature, APISecret: string): Boolean;
var
  ComputedSignature: string;
  RequestTime: TDateTime;
  TimeDiff: TDateTime;
begin
  ComputedSignature := SignAPIRequest(Request, APISecret);
  Result := (ComputedSignature = Signature);

  // Vérifier aussi le timestamp (éviter replay attacks)
  RequestTime := ISO8601ToDateTime(Request.Timestamp);
  TimeDiff := Abs(Now - RequestTime);

  if TimeDiff > (5 / 1440) then // Plus de 5 minutes
  begin
    WriteLn('⚠️ Requête expirée');
    Result := False;
  end;
end;

// Utilisation côté client
var
  Request: TAPIRequest;
  Signature: string;
begin
  Request.Method := 'POST';
  Request.Path := '/api/users';
  Request.Timestamp := DateTimeToISO8601(Now);
  Request.Body := '{"name":"John Doe"}';

  Signature := SignAPIRequest(Request, 'my-api-secret-key');

  // Envoyer la requête avec signature
  HTTP.Headers.Add('X-API-Signature: ' + Signature);
  HTTP.Headers.Add('X-API-Timestamp: ' + Request.Timestamp);
end.
```

## Signatures numériques

### Qu'est-ce qu'une signature numérique ?

Une signature numérique est l'équivalent électronique d'une signature manuscrite. Elle garantit :

1. **Authentification** : Prouve l'identité du signataire
2. **Intégrité** : Le document n'a pas été modifié
3. **Non-répudiation** : Le signataire ne peut pas nier avoir signé

### Principe de fonctionnement

```
SIGNATURE (avec clé privée):  
Document → Hash → Chiffrement avec clé privée = Signature

VÉRIFICATION (avec clé publique):  
Document → Hash → Déchiffrement signature avec clé publique  
Si les hashs correspondent → Signature valide ✓
```

### Signature RSA

**Génération de clés RSA** :

Avec OpenSSL en ligne de commande :
```bash
# Générer une clé privée
openssl genrsa -out private.pem 2048

# Extraire la clé publique
openssl rsa -in private.pem -pubout -out public.pem
```

**Implémentation en FreePascal** :

```pascal
uses
  OpenSSL, SysUtils, DCPsha256;

function SignDocument(const Document: string;
                      const PrivateKeyFile: string): string;
var
  Hash: string;
  PrivateKey: PEVP_PKEY;
  Context: PEVP_MD_CTX;
  Signature: array of Byte;
  SigLen: Cardinal;
  i: Integer;
begin
  // 1. Calculer le hash du document
  Hash := CalculateSHA256(Document);

  // 2. Charger la clé privée
  PrivateKey := LoadPrivateKeyFromFile(PrivateKeyFile);
  if PrivateKey = nil then
    raise Exception.Create('Impossible de charger la clé privée');

  try
    // 3. Créer le contexte de signature
    Context := EVP_MD_CTX_new;
    try
      // Initialiser pour RSA-SHA256
      EVP_DigestSignInit(Context, nil, EVP_sha256, nil, PrivateKey);

      // Mettre à jour avec les données
      EVP_DigestSignUpdate(Context, PChar(Document), Length(Document));

      // Obtenir la taille de la signature
      EVP_DigestSignFinal(Context, nil, @SigLen);

      // Créer et obtenir la signature
      SetLength(Signature, SigLen);
      EVP_DigestSignFinal(Context, @Signature[0], @SigLen);

      // Convertir en hexadécimal
      Result := '';
      for i := 0 to Integer(SigLen) - 1 do
        Result := Result + IntToHex(Signature[i], 2);
    finally
      EVP_MD_CTX_free(Context);
    end;
  finally
    EVP_PKEY_free(PrivateKey);
  end;
end;

function VerifySignature(const Document, Signature: string;
                         const PublicKeyFile: string): Boolean;
var
  PublicKey: PEVP_PKEY;
  Context: PEVP_MD_CTX;
  SigBytes: TBytes;
  i: Integer;
begin
  Result := False;

  // Convertir la signature hexadécimale en bytes
  SetLength(SigBytes, Length(Signature) div 2);
  for i := 0 to High(SigBytes) do
    SigBytes[i] := StrToInt('$' + Copy(Signature, i * 2 + 1, 2));

  // Charger la clé publique
  PublicKey := LoadPublicKeyFromFile(PublicKeyFile);
  if PublicKey = nil then Exit;

  try
    Context := EVP_MD_CTX_new;
    try
      // Initialiser la vérification
      EVP_DigestVerifyInit(Context, nil, EVP_sha256, nil, PublicKey);

      // Mettre à jour avec les données
      EVP_DigestVerifyUpdate(Context, PChar(Document), Length(Document));

      // Vérifier la signature
      Result := EVP_DigestVerifyFinal(Context, @SigBytes[0],
                                      Length(SigBytes)) = 1;
    finally
      EVP_MD_CTX_free(Context);
    end;
  finally
    EVP_PKEY_free(PublicKey);
  end;
end;

// Utilisation
var
  Document, Signature: string;
begin
  Document := 'Contrat important à signer';

  // Signer avec la clé privée
  Signature := SignDocument(Document, 'private.pem');
  WriteLn('Signature: ', Copy(Signature, 1, 64), '...');

  // Vérifier avec la clé publique
  if VerifySignature(Document, Signature, 'public.pem') then
    WriteLn('✓ Signature valide')
  else
    WriteLn('✗ Signature invalide');

  // Modifier le document
  Document := 'Contrat modifié';
  if VerifySignature(Document, Signature, 'public.pem') then
    WriteLn('✓ Signature valide')
  else
    WriteLn('✗ Signature invalide - Document modifié !');
end.
```

### Signature ECDSA (Elliptic Curve)

ECDSA offre la même sécurité que RSA avec des clés plus courtes, donc plus rapide.

**Comparaison** :
- RSA 2048 bits ≈ ECDSA 224 bits (même niveau de sécurité)
- RSA 3072 bits ≈ ECDSA 256 bits

**Avantages ECDSA** :
- Clés plus courtes → signatures plus petites
- Plus rapide
- Moins de bande passante

**Exemple conceptuel** :
```pascal
uses
  OpenSSL;

function SignWithECDSA(const Document: string;
                       const PrivateKeyFile: string): string;
var
  PrivateKey: PEC_KEY;
  Signature: PECDSA_SIG;
begin
  PrivateKey := LoadECPrivateKey(PrivateKeyFile);
  try
    Signature := ECDSA_do_sign(PChar(Document), Length(Document), PrivateKey);
    try
      Result := ECDSASigToHex(Signature);
    finally
      ECDSA_SIG_free(Signature);
    end;
  finally
    EC_KEY_free(PrivateKey);
  end;
end;
```

## Certificats numériques

### Qu'est-ce qu'un certificat numérique ?

Un certificat numérique est l'équivalent électronique d'une carte d'identité. Il contient :

- **Identité** : Nom, organisation, pays
- **Clé publique** : Pour vérifier les signatures ou chiffrer
- **Période de validité** : Date de début et de fin
- **Émetteur** : Autorité de certification (CA) qui a signé le certificat
- **Signature de la CA** : Preuve d'authenticité

### Structure d'un certificat X.509

```
Certificat X.509
├── Version
├── Numéro de série
├── Algorithme de signature
├── Émetteur (CA)
├── Période de validité
│   ├── Pas avant
│   └── Pas après
├── Sujet (propriétaire)
│   ├── CN (Common Name)
│   ├── O (Organization)
│   ├── C (Country)
│   └── ...
├── Clé publique
└── Signature de la CA
```

### Lire un certificat avec OpenSSL

**En ligne de commande** :
```bash
# Afficher le contenu d'un certificat
openssl x509 -in certificate.crt -text -noout

# Extraire la clé publique
openssl x509 -in certificate.crt -pubkey -noout > public.pem

# Vérifier les dates de validité
openssl x509 -in certificate.crt -noout -dates
```

**En FreePascal** :
```pascal
uses
  OpenSSL, SysUtils;

type
  TCertificateInfo = record
    Subject: string;
    Issuer: string;
    SerialNumber: string;
    NotBefore: TDateTime;
    NotAfter: TDateTime;
    PublicKey: string;
  end;

function LoadCertificate(const CertFile: string): PX509;  
var
  BIO: PBIO;
begin
  BIO := BIO_new_file(PChar(CertFile), 'r');
  if BIO = nil then
    raise Exception.Create('Impossible d''ouvrir le fichier certificat');

  try
    Result := PEM_read_bio_X509(BIO, nil, nil, nil);
    if Result = nil then
      raise Exception.Create('Impossible de lire le certificat');
  finally
    BIO_free(BIO);
  end;
end;

function GetCertificateInfo(const CertFile: string): TCertificateInfo;  
var
  Cert: PX509;
  SubjectName, IssuerName: PX509_NAME;
  NotBefore, NotAfter: PASN1_TIME;
begin
  Cert := LoadCertificate(CertFile);
  try
    // Extraire le sujet
    SubjectName := X509_get_subject_name(Cert);
    Result.Subject := X509_NAME_oneline(SubjectName, nil, 0);

    // Extraire l'émetteur
    IssuerName := X509_get_issuer_name(Cert);
    Result.Issuer := X509_NAME_oneline(IssuerName, nil, 0);

    // Extraire le numéro de série
    Result.SerialNumber := IntToHex(ASN1_INTEGER_get(
      X509_get_serialNumber(Cert)), 16);

    // Dates de validité
    NotBefore := X509_get_notBefore(Cert);
    NotAfter := X509_get_notAfter(Cert);
    Result.NotBefore := ASN1TimeToDateTime(NotBefore);
    Result.NotAfter := ASN1TimeToDateTime(NotAfter);
  finally
    X509_free(Cert);
  end;
end;

// Utilisation
var
  Info: TCertificateInfo;
begin
  Info := GetCertificateInfo('certificate.crt');

  WriteLn('=== Informations du certificat ===');
  WriteLn('Sujet: ', Info.Subject);
  WriteLn('Émetteur: ', Info.Issuer);
  WriteLn('Numéro de série: ', Info.SerialNumber);
  WriteLn('Valide du: ', DateTimeToStr(Info.NotBefore));
  WriteLn('Valide jusqu''au: ', DateTimeToStr(Info.NotAfter));
end.
```

### Vérifier un certificat

```pascal
function VerifyCertificate(const CertFile: string;
                           const CAFile: string): Boolean;
var
  Cert, CACert: PX509;
  CAPublicKey: PEVP_PKEY;
begin
  Result := False;

  // Charger le certificat à vérifier
  Cert := LoadCertificate(CertFile);
  try
    // Charger le certificat de la CA
    CACert := LoadCertificate(CAFile);
    try
      // Extraire la clé publique de la CA
      CAPublicKey := X509_get_pubkey(CACert);
      try
        // Vérifier la signature
        Result := X509_verify(Cert, CAPublicKey) = 1;
      finally
        EVP_PKEY_free(CAPublicKey);
      end;
    finally
      X509_free(CACert);
    end;
  finally
    X509_free(Cert);
  end;
end;

// Utilisation
begin
  if VerifyCertificate('server.crt', 'ca.crt') then
    WriteLn('✓ Certificat valide et signé par la CA')
  else
    WriteLn('✗ Certificat invalide ou non signé par cette CA');
end.
```

## Chaîne de certification

### Concept

Une chaîne de certification relie un certificat à une autorité racine de confiance :

```
Certificat racine (Root CA) - Auto-signé, de confiance
    ↓ signe
Certificat intermédiaire (Intermediate CA)
    ↓ signe
Certificat du serveur (www.example.com)
```

### Vérifier une chaîne de certification

```pascal
function VerifyCertificateChain(const CertFile: string;
                                const ChainFiles: array of string): Boolean;
var
  Store: PX509_STORE;
  StoreCtx: PX509_STORE_CTX;
  Cert, ChainCert: PX509;
  i: Integer;
  ErrorCode: Integer;
begin
  Result := False;

  // Créer le magasin de certificats
  Store := X509_STORE_new;
  if Store = nil then Exit;

  try
    // Ajouter les certificats de la chaîne au magasin
    for i := 0 to High(ChainFiles) do
    begin
      ChainCert := LoadCertificate(ChainFiles[i]);
      X509_STORE_add_cert(Store, ChainCert);
      X509_free(ChainCert);
    end;

    // Charger le certificat à vérifier
    Cert := LoadCertificate(CertFile);
    try
      // Créer le contexte de vérification
      StoreCtx := X509_STORE_CTX_new;
      try
        X509_STORE_CTX_init(StoreCtx, Store, Cert, nil);

        // Vérifier la chaîne
        Result := X509_verify_cert(StoreCtx) = 1;

        if not Result then
        begin
          ErrorCode := X509_STORE_CTX_get_error(StoreCtx);
          WriteLn('Erreur de vérification: ',
                  X509_verify_cert_error_string(ErrorCode));
        end;
      finally
        X509_STORE_CTX_free(StoreCtx);
      end;
    finally
      X509_free(Cert);
    end;
  finally
    X509_STORE_free(Store);
  end;
end;

// Utilisation
var
  ChainFiles: array[0..1] of string;
begin
  ChainFiles[0] := 'intermediate-ca.crt';
  ChainFiles[1] := 'root-ca.crt';

  if VerifyCertificateChain('server.crt', ChainFiles) then
    WriteLn('✓ Chaîne de certification valide')
  else
    WriteLn('✗ Chaîne de certification invalide');
end.
```

## Code signing (Signature de code)

### Pourquoi signer son code ?

**Avantages** :
- ✅ Prouve l'identité du développeur
- ✅ Garantit que le code n'a pas été modifié
- ✅ Évite les avertissements "Éditeur inconnu"
- ✅ Requis pour certaines plateformes (Windows Store, macOS)

### Signature Authenticode (Windows)

**Obtenir un certificat code signing** :
1. Acheter auprès d'une CA (DigiCert, Sectigo, etc.)
2. Ou créer un certificat auto-signé pour tests

**Créer un certificat de test** :
```powershell
# PowerShell (Windows)
New-SelfSignedCertificate `
  -Type CodeSigningCert `
  -Subject "CN=Mon Application" `
  -CertStoreLocation Cert:\CurrentUser\My
```

**Signer un exécutable** :
```bash
# Avec signtool.exe (Windows SDK)
signtool sign /f certificate.pfx /p password /t http://timestamp.digicert.com MonApp.exe

# Vérifier la signature
signtool verify /pa MonApp.exe
```

**En FreePascal** :
```pascal
uses
  Windows, SysUtils;

function SignExecutable(const ExeFile, CertFile, Password: string): Boolean;  
var
  Command: string;
  ExitCode: Integer;
begin
  Command := Format(
    'signtool sign /f "%s" /p "%s" /t http://timestamp.digicert.com "%s"',
    [CertFile, Password, ExeFile]
  );

  ExitCode := ExecuteProcess('cmd.exe', ['/c', Command]);
  Result := (ExitCode = 0);

  if Result then
    WriteLn('✓ Exécutable signé avec succès')
  else
    WriteLn('✗ Erreur lors de la signature');
end;

// Utilisation
begin
  SignExecutable('MonApp.exe', 'certificate.pfx', 'mypassword');
end.
```

### Signature sous Linux

**Avec GPG** :
```bash
# Signer un fichier
gpg --detach-sign --armor MonApp

# Crée MonApp.asc (signature détachée)

# Vérifier la signature
gpg --verify MonApp.asc MonApp
```

**En FreePascal** :
```pascal
function SignFileWithGPG(const FileName: string): Boolean;  
var
  ExitCode: Integer;
begin
  ExitCode := ExecuteProcess('gpg', [
    '--detach-sign',
    '--armor',
    FileName
  ]);

  Result := (ExitCode = 0);
end;

function VerifyGPGSignature(const FileName, SignatureFile: string): Boolean;  
var
  ExitCode: Integer;
begin
  ExitCode := ExecuteProcess('gpg', [
    '--verify',
    SignatureFile,
    FileName
  ]);

  Result := (ExitCode = 0);
end;

// Utilisation
begin
  if SignFileWithGPG('MonApp') then
    WriteLn('✓ Fichier signé : MonApp.asc créé');

  if VerifyGPGSignature('MonApp', 'MonApp.asc') then
    WriteLn('✓ Signature valide')
  else
    WriteLn('✗ Signature invalide');
end.
```

## Vérification d'intégrité de mise à jour

### Système de mise à jour sécurisé

```pascal
type
  TUpdateInfo = record
    Version: string;
    DownloadURL: string;
    FileSize: Int64;
    SHA256Hash: string;
    Signature: string;
  end;

function DownloadUpdate(const UpdateInfo: TUpdateInfo): Boolean;  
var
  HTTP: THTTPSend;
  DownloadedFile: TFileStream;
  CalculatedHash: string;
begin
  Result := False;

  WriteLn('Téléchargement de la mise à jour...');

  HTTP := THTTPSend.Create;
  DownloadedFile := TFileStream.Create('update.exe', fmCreate);
  try
    // Télécharger
    if not HTTP.HTTPMethod('GET', UpdateInfo.DownloadURL) then
    begin
      WriteLn('✗ Erreur de téléchargement');
      Exit;
    end;

    // Sauvegarder
    HTTP.Document.Position := 0;
    DownloadedFile.CopyFrom(HTTP.Document, HTTP.Document.Size);
    DownloadedFile.Free;
    DownloadedFile := nil;

    WriteLn('✓ Téléchargement terminé');

    // Vérifier la taille
    if GetFileSize('update.exe') <> UpdateInfo.FileSize then
    begin
      WriteLn('✗ Taille de fichier incorrecte');
      DeleteFile('update.exe');
      Exit;
    end;

    WriteLn('✓ Taille vérifiée');

    // Vérifier le hash
    CalculatedHash := CalculateFileSHA256('update.exe');
    if CalculatedHash <> UpdateInfo.SHA256Hash then
    begin
      WriteLn('✗ Hash SHA-256 invalide');
      WriteLn('  Attendu : ', UpdateInfo.SHA256Hash);
      WriteLn('  Calculé : ', CalculatedHash);
      DeleteFile('update.exe');
      Exit;
    end;

    WriteLn('✓ Hash SHA-256 valide');

    // Vérifier la signature
    if not VerifySignature('update.exe', UpdateInfo.Signature, 'public.pem') then
    begin
      WriteLn('✗ Signature invalide');
      DeleteFile('update.exe');
      Exit;
    end;

    WriteLn('✓ Signature valide');
    WriteLn('✓ Mise à jour authentique et intègre');

    Result := True;
  finally
    HTTP.Free;
    if DownloadedFile <> nil then
      DownloadedFile.Free;
  end;
end;

// Utilisation
var
  UpdateInfo: TUpdateInfo;
begin
  // Récupérer les informations de mise à jour depuis le serveur
  UpdateInfo.Version := '2.0.0';
  UpdateInfo.DownloadURL := 'https://example.com/updates/app-2.0.0.exe';
  UpdateInfo.FileSize := 5242880; // 5 MB
  UpdateInfo.SHA256Hash := 'a591a6d40bf420404a011733cfb7b190...';
  UpdateInfo.Signature := '3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d...';

  if DownloadUpdate(UpdateInfo) then
  begin
    WriteLn('Installation de la mise à jour...');
    ExecuteProcess('update.exe', ['/silent']);
  end;
end.
```

## Détection d'altération de fichiers

### Surveillance de l'intégrité

```pascal
type
  TFileHash = record
    FileName: string;
    Hash: string;
    LastCheck: TDateTime;
  end;

  TIntegrityMonitor = class
  private
    FHashes: TList<TFileHash>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile(const FileName: string);
    function CheckIntegrity: Boolean;
    procedure SaveHashes(const HashFile: string);
    procedure LoadHashes(const HashFile: string);
  end;

constructor TIntegrityMonitor.Create;  
begin
  inherited Create;
  FHashes := TList<TFileHash>.Create;
end;

destructor TIntegrityMonitor.Destroy;  
begin
  FHashes.Free;
  inherited Destroy;
end;

procedure TIntegrityMonitor.AddFile(const FileName: string);  
var
  FileHash: TFileHash;
begin
  FileHash.FileName := FileName;
  FileHash.Hash := CalculateFileSHA256(FileName);
  FileHash.LastCheck := Now;

  FHashes.Add(FileHash);
end;

function TIntegrityMonitor.CheckIntegrity: Boolean;  
var
  i: Integer;
  CurrentHash: string;
  Modified: Boolean;
begin
  Result := True;
  Modified := False;

  WriteLn('Vérification de l''intégrité des fichiers...');

  for i := 0 to FHashes.Count - 1 do
  begin
    if not FileExists(FHashes[i].FileName) then
    begin
      WriteLn('⚠️ Fichier supprimé: ', FHashes[i].FileName);
      Modified := True;
      Continue;
    end;

    CurrentHash := CalculateFileSHA256(FHashes[i].FileName);

    if CurrentHash <> FHashes[i].Hash then
    begin
      WriteLn('⚠️ Fichier modifié: ', FHashes[i].FileName);
      WriteLn('   Hash attendu: ', FHashes[i].Hash);
      WriteLn('   Hash actuel:  ', CurrentHash);
      Modified := True;
    end
    else
      WriteLn('✓ ', FHashes[i].FileName);
  end;

  if Modified then
  begin
    WriteLn('✗ Altération détectée !');
    Result := False;
  end
  else
    WriteLn('✓ Tous les fichiers sont intègres');
end;

procedure TIntegrityMonitor.SaveHashes(const HashFile: string);  
var
  F: TextFile;
  i: Integer;
begin
  AssignFile(F, HashFile);
  Rewrite(F);
  try
    for i := 0 to FHashes.Count - 1 do
      WriteLn(F, FHashes[i].FileName, '|', FHashes[i].Hash);
  finally
    CloseFile(F);
  end;
end;

procedure TIntegrityMonitor.LoadHashes(const HashFile: string);  
var
  F: TextFile;
  Line: string;
  SepPos: Integer;
  FileHash: TFileHash;
begin
  FHashes.Clear;

  if not FileExists(HashFile) then Exit;

  AssignFile(F, HashFile);
  Reset(F);
  try
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      SepPos := Pos('|', Line);
      if SepPos > 0 then
      begin
        FileHash.FileName := Copy(Line, 1, SepPos - 1);
        FileHash.Hash := Copy(Line, SepPos + 1, Length(Line) - SepPos);
        FileHash.LastCheck := Now;
        FHashes.Add(FileHash);
      end;
    end;
  finally
    CloseFile(F);
  end;
end;

// Utilisation
var
  Monitor: TIntegrityMonitor;
begin
  Monitor := TIntegrityMonitor.Create;
  try
    // Première exécution : créer les hashs de référence
    Monitor.AddFile('MonApp.exe');
    Monitor.AddFile('config.ini');
    Monitor.AddFile('data.db');
    Monitor.SaveHashes('integrity.dat');

    WriteLn('Hashs de référence créés');
    WriteLn;

    // Exécutions suivantes : vérifier l'intégrité
    Monitor.LoadHashes('integrity.dat');

    if not Monitor.CheckIntegrity then
    begin
      // Alerter l'administrateur
      SendSecurityAlert('Altération de fichiers détectée');
    end;
  finally
    Monitor.Free;
  end;
end.
```

## Timestamping (horodatage)

### Pourquoi horodater ?

L'horodatage prouve qu'un document existait à un moment donné et n'a pas été modifié depuis.

**Cas d'usage** :
- Preuves légales
- Brevets et propriété intellectuelle
- Signatures de code (certificats expirés)
- Audit trail

### Principe

```
1. Calculer le hash du document
2. Envoyer le hash à un serveur d'horodatage (TSA)
3. Le TSA signe le hash avec l'heure actuelle
4. Recevoir le timestamp token
5. Stocker le token avec le document
```

### RFC 3161 Timestamp

```pascal
function GetTimestamp(const DocumentHash: string;
                      const TSAUrl: string): string;
var
  HTTP: THTTPSend;
  Request, Response: TMemoryStream;
begin
  HTTP := THTTPSend.Create;
  Request := TMemoryStream.Create;
  Response := TMemoryStream.Create;
  try
    // Créer la requête de timestamp (format RFC 3161)
    CreateTimestampRequest(DocumentHash, Request);

    // Envoyer au serveur TSA
    HTTP.Document.LoadFromStream(Request);
    HTTP.MimeType := 'application/timestamp-query';

    if HTTP.HTTPMethod('POST', TSAUrl) then
    begin
      // Récupérer la réponse
      HTTP.Document.Position := 0;
      Response.CopyFrom(HTTP.Document, HTTP.Document.Size);

      // Parser la réponse de timestamp
      Result := ParseTimestampResponse(Response);
    end;
  finally
    Response.Free;
    Request.Free;
    HTTP.Free;
  end;
end;

// Utilisation avec un serveur public
var
  DocumentHash, Timestamp: string;
begin
  DocumentHash := CalculateFileSHA256('contract.pdf');

  // FreeTSA.org est un serveur public gratuit
  Timestamp := GetTimestamp(DocumentHash,
    'https://freetsa.org/tsr');

  // Sauvegarder le timestamp
  SaveToFile('contract.tst', Timestamp);

  WriteLn('✓ Document horodaté');
end.
```

## Considérations multi-plateformes

### Différences Windows vs Ubuntu

**Génération de nombres aléatoires** :

**Windows** :
```pascal
{$IFDEF WINDOWS}
uses
  Windows, WinCrypt;

function GetSecureRandomBytes(Count: Integer): TBytes;  
var
  hProv: HCRYPTPROV;
begin
  SetLength(Result, Count);

  if not CryptAcquireContext(@hProv, nil, nil, PROV_RSA_FULL,
                             CRYPT_VERIFYCONTEXT) then
    raise Exception.Create('Impossible d''obtenir le contexte crypto');

  try
    if not CryptGenRandom(hProv, Count, @Result[0]) then
      raise Exception.Create('Erreur de génération aléatoire');
  finally
    CryptReleaseContext(hProv, 0);
  end;
end;
{$ENDIF}
```

**Ubuntu/Linux** :
```pascal
{$IFDEF UNIX}
function GetSecureRandomBytes(Count: Integer): TBytes;  
var
  F: File;
begin
  SetLength(Result, Count);

  AssignFile(F, '/dev/urandom');
  Reset(F, 1);
  try
    BlockRead(F, Result[0], Count);
  finally
    CloseFile(F);
  end;
end;
{$ENDIF}
```

**Stockage de certificats** :

**Windows** :
- Magasin de certificats Windows (Certificate Store)
- Accessible via CryptoAPI

**Ubuntu/Linux** :
- Fichiers PEM dans `/etc/ssl/certs/`
- Système de confiance via update-ca-certificates

**OpenSSL** :

Les deux plateformes peuvent utiliser OpenSSL, mais les chemins diffèrent :

**Windows** :
```pascal
const
  OPENSSL_LIB_SSL = 'libssl-3-x64.dll';
  OPENSSL_LIB_CRYPTO = 'libcrypto-3-x64.dll';
```

**Ubuntu** :
```pascal
const
  OPENSSL_LIB_SSL = 'libssl.so.3';
  OPENSSL_LIB_CRYPTO = 'libcrypto.so.3';
```

## Performance et optimisation

### Optimisation du hashing

**1. Hashing incrémental pour gros fichiers** :
```pascal
function HashLargeFile(const FileName: string): string;  
const
  BUFFER_SIZE = 1024 * 1024; // 1 MB
var
  FileStream: TFileStream;
  Hash: TDCP_sha256;
  Buffer: array[0..BUFFER_SIZE-1] of Byte;
  BytesRead: Integer;
  Digest: array[0..31] of Byte;
  i: Integer;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;

    // Traiter par blocs
    repeat
      BytesRead := FileStream.Read(Buffer, BUFFER_SIZE);
      if BytesRead > 0 then
        Hash.Update(Buffer, BytesRead);
    until BytesRead = 0;

    Hash.Final(Digest);

    Result := '';
    for i := 0 to 31 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
    FileStream.Free;
  end;
end;
```

**2. Cache de hashs** :
```pascal
type
  THashCache = class
  private
    FCache: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetHash(const FileName: string): string;
  end;

function THashCache.GetHash(const FileName: string): string;  
var
  FileTime: TDateTime;
  CacheKey: string;
begin
  FileTime := FileDateToDateTime(FileAge(FileName));
  CacheKey := FileName + '|' + DateTimeToStr(FileTime);

  // Vérifier le cache
  if FCache.TryGetValue(CacheKey, Result) then
    Exit; // Hash déjà calculé

  // Calculer et mettre en cache
  Result := CalculateFileSHA256(FileName);
  FCache.Add(CacheKey, Result);
end;
```

**3. Hashing parallèle** :
```pascal
uses
  MTProcs; // Multi-threading

function HashFilesParallel(const Files: array of string): TArray<string>;  
var
  i: Integer;
begin
  SetLength(Result, Length(Files));

  // Hasher en parallèle
  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    begin
      Result[Index] := CalculateFileSHA256(Files[Index]);
    end,
    0, High(Files)
  );
end;
```

## Bonnes pratiques de sécurité

### 1. Choix de l'algorithme

```pascal
// ✅ RECOMMANDÉ
Hash := CalculateSHA256(Data);        // Pour usage général  
Hash := CalculateSHA512(Data);        // Pour haute sécurité  
Hash := PBKDF2_SHA256(Password, Salt, 100000); // Pour mots de passe

// ⚠️ DÉPRÉCIÉ
Hash := CalculateSHA1(Data);          // Seulement si nécessaire (legacy)

// ❌ À ÉVITER
Hash := CalculateMD5(Data);           // Jamais pour sécurité
```

### 2. Toujours saler les mots de passe

```pascal
// ❌ DANGEREUX
PasswordHash := SHA256(Password);

// ✅ CORRECT
Salt := GenerateRandomSalt(16);  
PasswordHash := PBKDF2_SHA256(Password, Salt, 100000);
```

### 3. Vérifier les signatures

```pascal
// ✅ Toujours vérifier avant d'exécuter
if VerifySignature(DownloadedFile, Signature, PublicKey) then
  ExecuteFile(DownloadedFile)
else
  ShowError('Signature invalide - fichier rejeté');
```

### 4. Protéger les clés privées

```pascal
// ❌ DANGEREUX - Clé privée en dur
const PRIVATE_KEY = '-----BEGIN PRIVATE KEY-----...';

// ✅ CORRECT - Charger depuis un fichier sécurisé
{$IFDEF WINDOWS}
  PrivateKey := LoadFromDPAPI('encrypted_key.dat');
{$ELSE}
  PrivateKey := LoadFromFile('/etc/myapp/private.key'); // avec chmod 600
{$ENDIF}
```

### 5. Utiliser des bibliothèques éprouvées

```pascal
// ✅ Utiliser DCPCrypt, OpenSSL, ou mORMot
uses
  DCPsha256;  // Testé et audité

// ❌ Éviter d'implémenter ses propres algorithmes
function MaFonctionHashMaison(Data: string): string;  
begin
  // Non ! Utilisez des bibliothèques standard
  // La cryptographie est trop complexe pour être réinventée
end;
```

### 6. Comparer les hashs en temps constant

Pour éviter les attaques par timing, comparez toujours les hashs en temps constant :

```pascal
// ❌ Vulnérable aux attaques par timing
function CompareHashUnsafe(const Hash1, Hash2: string): Boolean;  
begin
  Result := (Hash1 = Hash2); // S'arrête dès la première différence
end;

// ✅ Comparaison en temps constant
function CompareHashSecure(const Hash1, Hash2: string): Boolean;  
var
  i: Integer;
  Diff: Byte;
begin
  Diff := 0;

  // Comparer toujours tous les caractères
  if Length(Hash1) <> Length(Hash2) then
    Diff := 1;

  for i := 1 to Min(Length(Hash1), Length(Hash2)) do
    Diff := Diff or (Ord(Hash1[i]) xor Ord(Hash2[i]));

  Result := (Diff = 0);
end;
```

### 7. Logger les événements de sécurité

```pascal
procedure LogSecurityEvent(const EventType, Details: string);  
begin
  WriteToLog(Format('[SECURITY] %s - %s - Time: %s - IP: %s',
    [EventType, Details, DateTimeToStr(Now), GetClientIP]));

  // Alerter si critique
  if EventType in ['SIGNATURE_INVALID', 'HASH_MISMATCH'] then
    SendSecurityAlert(EventType, Details);
end;

// Utilisation
if not VerifySignature(Document, Signature, PublicKey) then  
begin
  LogSecurityEvent('SIGNATURE_INVALID',
    'Document: ' + ExtractFileName(Document));
  Exit;
end;
```

## Exemple d'application complète

### Système de distribution sécurisé

```pascal
program SecureDistribution;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, httpsend, ssl_openssl, fpjson, jsonparser,
  DCPsha256, DCPcrypt2;

type
  TPackageInfo = record
    FileName: string;
    Version: string;
    FileSize: Int64;
    SHA256: string;
    Signature: string;
    DownloadURL: string;
  end;

// === CÔTÉ SERVEUR : Création du package ===

function CreatePackage(const FileName: string): TPackageInfo;  
var
  FileStream: TFileStream;
begin
  Result.FileName := ExtractFileName(FileName);
  Result.Version := '1.0.0';

  // Calculer la taille
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result.FileSize := FileStream.Size;
  finally
    FileStream.Free;
  end;

  // Calculer le hash SHA-256
  Result.SHA256 := CalculateFileSHA256(FileName);
  WriteLn('✓ Hash SHA-256 calculé: ', Copy(Result.SHA256, 1, 32), '...');

  // Signer le hash
  Result.Signature := SignDocument(Result.SHA256, 'private.pem');
  WriteLn('✓ Signature créée');

  Result.DownloadURL := 'https://example.com/downloads/' + Result.FileName;

  // Sauvegarder les métadonnées
  SavePackageInfo(Result);
end;

procedure SavePackageInfo(const Info: TPackageInfo);  
var
  JSON: TJSONObject;
  F: TextFile;
begin
  JSON := TJSONObject.Create;
  try
    JSON.Add('filename', Info.FileName);
    JSON.Add('version', Info.Version);
    JSON.Add('filesize', Info.FileSize);
    JSON.Add('sha256', Info.SHA256);
    JSON.Add('signature', Info.Signature);
    JSON.Add('download_url', Info.DownloadURL);

    AssignFile(F, 'package.json');
    Rewrite(F);
    try
      WriteLn(F, JSON.FormatJSON);
    finally
      CloseFile(F);
    end;

    WriteLn('✓ Métadonnées sauvegardées dans package.json');
  finally
    JSON.Free;
  end;
end;

// === CÔTÉ CLIENT : Téléchargement et vérification ===

function LoadPackageInfo(const JSONFile: string): TPackageInfo;  
var
  F: TextFile;
  JSONText: string;
  JSON: TJSONData;
  JSONObj: TJSONObject;
begin
  AssignFile(F, JSONFile);
  Reset(F);
  try
    ReadLn(F, JSONText);
  finally
    CloseFile(F);
  end;

  JSON := GetJSON(JSONText);
  try
    JSONObj := TJSONObject(JSON);

    Result.FileName := JSONObj.Get('filename', '');
    Result.Version := JSONObj.Get('version', '');
    Result.FileSize := JSONObj.Get('filesize', Int64(0));
    Result.SHA256 := JSONObj.Get('sha256', '');
    Result.Signature := JSONObj.Get('signature', '');
    Result.DownloadURL := JSONObj.Get('download_url', '');
  finally
    JSON.Free;
  end;
end;

function DownloadAndVerifyPackage(const Info: TPackageInfo): Boolean;  
var
  HTTP: THTTPSend;
  LocalFile: string;
  CalculatedHash: string;
begin
  Result := False;
  LocalFile := Info.FileName;

  WriteLn('=== Téléchargement du package ===');
  WriteLn('Nom: ', Info.FileName);
  WriteLn('Version: ', Info.Version);
  WriteLn('Taille: ', Info.FileSize, ' bytes');
  WriteLn;

  // 1. Télécharger le fichier
  WriteLn('Téléchargement en cours...');
  HTTP := THTTPSend.Create;
  try
    if not HTTP.HTTPMethod('GET', Info.DownloadURL) then
    begin
      WriteLn('✗ Erreur de téléchargement');
      Exit;
    end;

    HTTP.Document.SaveToFile(LocalFile);
    WriteLn('✓ Téléchargement terminé');
  finally
    HTTP.Free;
  end;

  // 2. Vérifier la taille
  WriteLn('Vérification de la taille...');
  if GetFileSize(LocalFile) <> Info.FileSize then
  begin
    WriteLn('✗ Taille incorrecte');
    WriteLn('  Attendue: ', Info.FileSize);
    WriteLn('  Reçue: ', GetFileSize(LocalFile));
    DeleteFile(LocalFile);
    Exit;
  end;
  WriteLn('✓ Taille correcte');

  // 3. Vérifier le hash SHA-256
  WriteLn('Calcul du hash SHA-256...');
  CalculatedHash := CalculateFileSHA256(LocalFile);

  if not CompareHashSecure(CalculatedHash, Info.SHA256) then
  begin
    WriteLn('✗ Hash SHA-256 invalide');
    WriteLn('  Attendu: ', Info.SHA256);
    WriteLn('  Calculé: ', CalculatedHash);
    DeleteFile(LocalFile);
    Exit;
  end;
  WriteLn('✓ Hash SHA-256 valide');

  // 4. Vérifier la signature
  WriteLn('Vérification de la signature...');
  if not VerifySignature(CalculatedHash, Info.Signature, 'public.pem') then
  begin
    WriteLn('✗ Signature invalide');
    WriteLn('⚠️ Le fichier pourrait être compromis !');
    DeleteFile(LocalFile);
    Exit;
  end;
  WriteLn('✓ Signature valide');

  WriteLn;
  WriteLn('✓✓✓ Package vérifié avec succès ✓✓✓');
  WriteLn('Le fichier est authentique et n''a pas été modifié.');

  Result := True;
end;

// === Programme principal ===

var
  Choice: Integer;
  PackageInfo: TPackageInfo;
begin
  WriteLn('=== Système de Distribution Sécurisé ===');
  WriteLn;
  WriteLn('1. Créer un package (serveur)');
  WriteLn('2. Télécharger et vérifier un package (client)');
  WriteLn;
  Write('Votre choix: ');
  ReadLn(Choice);
  WriteLn;

  case Choice of
    1: begin
      // Créer un package
      WriteLn('=== Création d''un package ===');
      PackageInfo := CreatePackage('MonApplication.exe');
      WriteLn;
      WriteLn('Package prêt à être distribué !');
    end;

    2: begin
      // Télécharger et vérifier
      PackageInfo := LoadPackageInfo('package.json');

      if DownloadAndVerifyPackage(PackageInfo) then
      begin
        WriteLn;
        WriteLn('Installation...');
        // Procéder à l'installation
      end
      else
      begin
        WriteLn;
        WriteLn('Installation annulée pour des raisons de sécurité.');
      end;
    end;

    else
      WriteLn('Choix invalide');
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Tests et validation

### Suite de tests pour hashing

```pascal
uses
  fpcunit, testregistry;

type
  THashingTests = class(TTestCase)
  published
    procedure TestSHA256_EmptyString;
    procedure TestSHA256_KnownValue;
    procedure TestSHA256_Consistency;
    procedure TestSaltGeneration;
    procedure TestPasswordHashing;
    procedure TestHashComparison;
  end;

procedure THashingTests.TestSHA256_EmptyString;  
var
  Hash: string;
begin
  Hash := CalculateSHA256('');

  // SHA-256 d'une chaîne vide
  AssertEquals('Empty string hash',
    'E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855',
    Hash);
end;

procedure THashingTests.TestSHA256_KnownValue;  
var
  Hash: string;
begin
  Hash := CalculateSHA256('The quick brown fox jumps over the lazy dog');

  // Valeur connue
  AssertEquals('Known value hash',
    'D7A8FBB307D7809469CA9ABCB0082E4F8D5651E46D3CDB762D02D0BF37C9E592',
    Hash);
end;

procedure THashingTests.TestSHA256_Consistency;  
var
  Hash1, Hash2: string;
begin
  Hash1 := CalculateSHA256('Test');
  Hash2 := CalculateSHA256('Test');

  AssertEquals('Consistency check', Hash1, Hash2);
end;

procedure THashingTests.TestSaltGeneration;  
var
  Salt1, Salt2: string;
begin
  Salt1 := GenerateSalt(16);
  Salt2 := GenerateSalt(16);

  // Les sels doivent être différents
  AssertTrue('Salts should be different', Salt1 <> Salt2);

  // Longueur correcte (16 bytes = 32 caractères hex)
  AssertEquals('Salt length', 32, Length(Salt1));
end;

procedure THashingTests.TestPasswordHashing;  
var
  PasswordHash: TPasswordHash;
begin
  PasswordHash := CreatePasswordHash('MyPassword123');

  // Vérifier que le hash n'est pas vide
  AssertTrue('Hash not empty', PasswordHash.Hash <> '');
  AssertTrue('Salt not empty', PasswordHash.Salt <> '');

  // Vérifier le mot de passe correct
  AssertTrue('Correct password',
    VerifyPassword('MyPassword123', PasswordHash));

  // Vérifier que le mauvais mot de passe échoue
  AssertFalse('Wrong password',
    VerifyPassword('WrongPassword', PasswordHash));
end;

procedure THashingTests.TestHashComparison;  
begin
  // Hashs identiques
  AssertTrue('Same hashes',
    CompareHashSecure('ABC123', 'ABC123'));

  // Hashs différents
  AssertFalse('Different hashes',
    CompareHashSecure('ABC123', 'ABC124'));

  // Longueurs différentes
  AssertFalse('Different lengths',
    CompareHashSecure('ABC', 'ABC123'));
end;
```

## Audit et conformité

### Audit trail pour signatures

```pascal
type
  TSignatureAudit = record
    Timestamp: TDateTime;
    Document: string;
    SignerID: string;
    SignatureHash: string;
    PublicKeyFingerprint: string;
    Valid: Boolean;
  end;

procedure LogSignature(const Audit: TSignatureAudit);  
var
  F: TextFile;
  LogLine: string;
begin
  LogLine := Format('%s|%s|%s|%s|%s|%s',
    [DateTimeToStr(Audit.Timestamp),
     Audit.Document,
     Audit.SignerID,
     Audit.SignatureHash,
     Audit.PublicKeyFingerprint,
     BoolToStr(Audit.Valid, True)]);

  AssignFile(F, 'signature_audit.log');
  if FileExists('signature_audit.log') then
    Append(F)
  else
    Rewrite(F);

  try
    WriteLn(F, LogLine);
  finally
    CloseFile(F);
  end;
end;

// Utilisation
var
  Audit: TSignatureAudit;
begin
  Audit.Timestamp := Now;
  Audit.Document := 'contract.pdf';
  Audit.SignerID := 'john.doe@example.com';
  Audit.SignatureHash := Copy(Signature, 1, 64);
  Audit.PublicKeyFingerprint := GetPublicKeyFingerprint('public.pem');
  Audit.Valid := VerifySignature(Document, Signature, PublicKey);

  LogSignature(Audit);
end.
```

### Conformité RGPD

```pascal
// Hashing de données personnelles pour pseudonymisation
function PseudonymizeData(const PersonalData: string): string;  
var
  Salt: string;
begin
  // Utiliser un salt global pour l'application
  Salt := GetApplicationSalt;

  // Hash avec HMAC pour lier au système
  Result := HMACSHA256(Salt, PersonalData);
end;

// Exemple : pseudonymiser un email
var
  Email, PseudoEmail: string;
begin
  Email := 'john.doe@example.com';
  PseudoEmail := PseudonymizeData(Email);

  // Stocker PseudoEmail au lieu de l'email réel
  // Permet l'analyse sans exposer les données personnelles
  SaveToDatabase('user_id', PseudoEmail);
end.
```

## Ressources et documentation

### Standards et spécifications

- **SHA-2 (FIPS 180-4)** : https://csrc.nist.gov/publications/detail/fips/180/4/final
  - Spécification officielle des algorithmes SHA-256, SHA-512, etc.

- **HMAC (RFC 2104)** : https://datatracker.ietf.org/doc/html/rfc2104
  - Keyed-Hashing for Message Authentication

- **PBKDF2 (RFC 2898)** : https://datatracker.ietf.org/doc/html/rfc2898
  - Password-Based Cryptography Specification

- **X.509 (RFC 5280)** : https://datatracker.ietf.org/doc/html/rfc5280
  - Certificats de clé publique et CRL

- **RFC 3161** : https://datatracker.ietf.org/doc/html/rfc3161
  - Time-Stamp Protocol (TSP)

- **PKCS#7 (RFC 2315)** : https://datatracker.ietf.org/doc/html/rfc2315
  - Cryptographic Message Syntax

### Outils

**En ligne de commande** :

```bash
# OpenSSL - Couteau suisse de la cryptographie
openssl dgst -sha256 file.txt           # Hash SHA-256  
openssl dgst -sha256 -sign private.pem file.txt  # Signer  
openssl dgst -sha256 -verify public.pem -signature sig.bin file.txt  # Vérifier

# GPG - Signatures et chiffrement
gpg --gen-key                            # Générer une paire de clés  
gpg --sign file.txt                      # Signer  
gpg --verify file.txt.gpg               # Vérifier

# sha256sum (Linux)
sha256sum file.txt                       # Calculer hash  
sha256sum -c checksums.txt              # Vérifier hashs
```

**Outils graphiques** :

- **Kleopatra** (Windows/Linux) : Gestion de certificats et GPG
- **XCA** (X Certificate and Key management) : Gestion de PKI
- **HashCheck** (Windows) : Intégration hash dans l'explorateur
- **GtkHash** (Linux) : Calculateur de hash avec GUI

### Bibliothèques FreePascal

- **DCPCrypt** : https://sourceforge.net/projects/dcpcrypt/
  - Algorithmes de hashing et chiffrement

- **OpenSSL bindings** : Inclus dans FreePascal
  - Accès à toutes les fonctionnalités OpenSSL

- **mORMot** : https://github.com/synopse/mORMot2
  - Framework complet avec cryptographie intégrée

- **Crypto library** : https://www.wolfgang-ehrhardt.de/
  - Collection d'algorithmes cryptographiques

### Documentation et tutoriels

- **Cryptography I** (Coursera) : Cours de Dan Boneh (Stanford)
- **Applied Cryptography** : Livre de Bruce Schneier
- **Crypto 101** : https://www.crypto101.io/
- **OWASP Cryptographic Storage Cheat Sheet** :
  https://cheatsheetseries.owasp.org/cheatsheets/Cryptographic_Storage_Cheat_Sheet.html

## Checklist de sécurité

### Avant de déployer en production

**Hashing** :
- [ ] Utiliser SHA-256 minimum (pas MD5 ni SHA-1)
- [ ] Saler tous les mots de passe
- [ ] Utiliser PBKDF2, bcrypt ou Argon2 pour les mots de passe
- [ ] Minimum 10 000 itérations pour PBKDF2
- [ ] Générer des sels cryptographiquement sécurisés
- [ ] Comparer les hashs en temps constant

**Signatures** :
- [ ] Utiliser RSA 2048+ bits ou ECDSA 256+ bits
- [ ] Protéger les clés privées (chiffrement, permissions 600)
- [ ] Vérifier toutes les signatures avant utilisation
- [ ] Utiliser des certificats d'une CA de confiance
- [ ] Vérifier les dates de validité des certificats
- [ ] Implémenter la révocation de certificats

**Général** :
- [ ] Utiliser HTTPS partout
- [ ] Logger les événements de sécurité
- [ ] Tester sur Windows et Ubuntu
- [ ] Documenter les algorithmes utilisés
- [ ] Prévoir un plan de rotation des clés
- [ ] Effectuer des audits de sécurité réguliers

## Conclusion

Le hashing et les signatures numériques sont des piliers fondamentaux de la sécurité informatique moderne. Ils permettent de garantir l'intégrité des données, d'authentifier leur origine et de détecter toute modification non autorisée.

### Points clés à retenir

**✅ Hashing** :
- SHA-256 est le standard recommandé actuellement
- Toujours saler les mots de passe
- Utiliser PBKDF2, bcrypt ou Argon2 pour les mots de passe
- MD5 et SHA-1 ne doivent plus être utilisés pour la sécurité
- Le hashing est unidirectionnel et rapide

**✅ Signatures numériques** :
- Garantissent authentification, intégrité et non-répudiation
- RSA 2048+ bits ou ECDSA 256+ bits minimum
- Protéger les clés privées à tout prix
- Vérifier toujours les signatures avant d'exécuter du code
- Utiliser des certificats de CA de confiance

**✅ HMAC** :
- Combine hashing et clé secrète
- Idéal pour l'authentification de messages
- Utilisé dans JWT, signatures API, etc.

**✅ Certificats** :
- X.509 est le standard pour les certificats
- Vérifier la chaîne de certification complète
- Respecter les dates de validité
- Implémenter la révocation si nécessaire

**✅ Multi-plateforme** :
- Les algorithmes fonctionnent identiquement sur Windows et Ubuntu
- Différences dans la génération de nombres aléatoires
- Différences dans le stockage des certificats
- OpenSSL disponible sur les deux plateformes

**✅ Bonnes pratiques** :
- Utiliser des bibliothèques éprouvées (DCPCrypt, OpenSSL)
- Ne jamais inventer ses propres algorithmes
- Logger les événements de sécurité
- Tester exhaustivement
- Maintenir à jour les bibliothèques cryptographiques

### Avec FreePascal et Lazarus

FreePascal offre tous les outils nécessaires pour implémenter du hashing et des signatures numériques de manière professionnelle :

- **DCPCrypt** pour les algorithmes standards (SHA-256, SHA-512, etc.)
- **OpenSSL** pour les signatures RSA/ECDSA et certificats
- **Support multi-plateforme natif** pour Windows et Ubuntu
- **Performance** comparable aux langages compilés traditionnels
- **Sécurité** grâce à des bibliothèques éprouvées

Que vous développiez une application de bureau, un service web ou un système de distribution de logiciels, les techniques de hashing et de signatures numériques sont essentielles pour garantir la sécurité et la confiance de vos utilisateurs.

**Prochaine section** : 17.6 Stockage sécurisé de données

⏭️ [Stockage sécurisé de données](/17-securite-cryptographie/06-stockage-securise-donnees.md)
