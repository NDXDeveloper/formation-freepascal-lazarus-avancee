🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.6 Stockage sécurisé de données

## Introduction

Le stockage sécurisé des données sensibles est un défi majeur dans le développement d'applications. Mots de passe, tokens d'authentification, clés API, données personnelles... tous nécessitent une protection appropriée contre les accès non autorisés.

Dans cette section, nous allons explorer les différentes techniques et mécanismes pour stocker des données de manière sécurisée sur Windows et Ubuntu/Linux avec FreePascal et Lazarus.

## Pourquoi le stockage sécurisé est-il crucial ?

### Les risques du stockage non sécurisé

**Scénarios d'attaque courants** :

1. **Fichiers en clair sur le disque** :
```pascal
// ❌ DANGEREUX
procedure SavePassword(const Password: string);  
begin
  WriteToFile('password.txt', Password); // Lisible par n'importe qui !
end;
```

2. **Hardcoding dans le code source** :
```pascal
// ❌ TRÈS DANGEREUX
const
  API_KEY = 'sk_live_abc123def456...'; // Visible dans le binaire !
  DATABASE_PASSWORD = 'MySecretPassword123';
```

3. **Stockage en base de données sans chiffrement** :
```sql
-- ❌ DANGEREUX
INSERT INTO users (username, password) VALUES ('john', 'password123');
```

### Conséquences d'une fuite de données

- **Vol d'identité** : Accès non autorisé aux comptes utilisateurs
- **Perte financière** : Utilisation frauduleuse de cartes bancaires, clés API
- **Atteinte à la réputation** : Perte de confiance des clients
- **Sanctions légales** : Amendes RGPD (jusqu'à 4% du CA annuel ou 20M€)
- **Responsabilité civile** : Poursuites des utilisateurs affectés

## Types de données sensibles

### Classification des données

**Données hautement sensibles** :
- Mots de passe
- Numéros de carte bancaire
- Numéros de sécurité sociale
- Données médicales
- Clés cryptographiques

**Données sensibles** :
- Tokens d'authentification (JWT, OAuth)
- Clés API
- Certificats SSL/TLS
- Données personnelles (email, téléphone, adresse)
- Historique de navigation

**Données confidentielles** :
- Préférences utilisateur
- Configuration d'application
- Logs contenant des informations sensibles
- Cache de données

## Principes fondamentaux du stockage sécurisé

### 1. Chiffrement au repos (Encryption at Rest)

Les données doivent être chiffrées lorsqu'elles sont stockées sur le disque.

```
Données en clair → Chiffrement → Stockage chiffré sur disque
                      ↓
                  Clé de chiffrement (protégée séparément)
```

**Avantages** :
- Protège contre le vol physique du disque
- Protège contre l'accès non autorisé aux fichiers
- Conforme aux réglementations (RGPD, HIPAA, etc.)

### 2. Séparation des clés

Ne jamais stocker la clé de chiffrement avec les données chiffrées.

```
❌ Mauvais :
/app/
  ├── data.encrypted
  └── encryption.key        // Dans le même répertoire !

✅ Bon :
/app/
  └── data.encrypted

Clé stockée :
- Windows : DPAPI, Credential Manager
- Linux : Keyring, /etc/secrets/ (avec permissions)
```

### 3. Principe du moindre privilège

Limiter l'accès aux données sensibles uniquement aux processus et utilisateurs qui en ont besoin.

```pascal
// Permissions restrictives sur les fichiers
{$IFDEF UNIX}
  fpChmod(SecretFile, &600); // rw------- (propriétaire uniquement)
{$ENDIF}
```

### 4. Ne jamais faire confiance aux entrées

Toujours valider et assainir les données avant de les stocker.

```pascal
function ValidateAndSanitize(const Input: string): string;  
begin
  // Validation
  if Length(Input) > MAX_LENGTH then
    raise Exception.Create('Input too long');

  // Suppression des caractères dangereux
  Result := RemoveDangerousChars(Input);
end;
```

### 5. Destruction sécurisée

Écraser les données sensibles en mémoire après utilisation.

```pascal
procedure SecureWipeString(var Sensitive: string);  
var
  i: Integer;
begin
  // Écraser avec des zéros
  for i := 1 to Length(Sensitive) do
    Sensitive[i] := #0;

  // Libérer
  Sensitive := '';
end;
```

## Méthodes de stockage sécurisé

### Vue d'ensemble des approches

| Méthode | Windows | Linux | Sécurité | Complexité |
|---------|---------|-------|----------|------------|
| **Systèmes natifs** | DPAPI | Keyring | ⭐⭐⭐⭐⭐ | Moyenne |
| **Chiffrement AES** | ✅ | ✅ | ⭐⭐⭐⭐ | Moyenne |
| **Base de données chiffrée** | ✅ | ✅ | ⭐⭐⭐⭐ | Élevée |
| **Fichiers protégés** | ✅ | ✅ | ⭐⭐⭐ | Faible |
| **Variables d'environnement** | ✅ | ✅ | ⭐⭐ | Faible |
| **Configuration externe** | ✅ | ✅ | ⭐⭐⭐ | Moyenne |

### 1. Utilisation des API système

**Avantages** :
- ✅ Intégration native au système
- ✅ Gestion automatique des clés
- ✅ Protection matérielle (TPM sur Windows)
- ✅ Respect des meilleures pratiques de l'OS

**Inconvénients** :
- ❌ Spécifique à chaque plateforme
- ❌ Nécessite une abstraction pour le multi-plateforme

**Windows : DPAPI (Data Protection API)**
```pascal
{$IFDEF WINDOWS}
function ProtectData(const Data: string): string;
// Utilise les clés de l'utilisateur Windows
```

**Linux : Secret Service / Keyring**
```pascal
{$IFDEF UNIX}
function StoreSecret(const Key, Value: string): Boolean;
// Utilise GNOME Keyring, KWallet, etc.
```

### 2. Chiffrement manuel avec AES

**Principe** :
```
Données → AES-256 → Données chiffrées
           ↑
    Clé maîtresse (dérivée du mot de passe)
```

**Avantages** :
- ✅ Portable entre plateformes
- ✅ Contrôle total sur le chiffrement
- ✅ Standard industriel (AES)

**Inconvénients** :
- ❌ Nécessite de gérer la clé maîtresse
- ❌ Plus complexe à implémenter correctement

### 3. Base de données chiffrée

**Options** :
- SQLite avec SQLCipher
- Fichiers chiffrés avec mot de passe
- Chiffrement au niveau des colonnes

**Avantages** :
- ✅ Toutes les données protégées
- ✅ Transparence pour l'application
- ✅ Performance acceptable

### 4. Stockage distant sécurisé

**Solutions** :
- HashiCorp Vault
- AWS Secrets Manager
- Azure Key Vault
- Google Cloud Secret Manager

**Avantages** :
- ✅ Centralisation
- ✅ Rotation automatique des secrets
- ✅ Audit trail complet
- ✅ Haute disponibilité

**Inconvénients** :
- ❌ Dépendance réseau
- ❌ Coûts
- ❌ Complexité

## Où stocker quoi ?

### Matrice de décision

| Type de donnée | Stockage recommandé | Exemple |
|----------------|---------------------|---------|
| Mots de passe utilisateur | Hash + Salt en BD | Authentification |
| Tokens de session | Mémoire + optionnel BD chiffrée | JWT refresh token |
| Clés API | Keyring système ou variables env | API keys tierces |
| Certificats SSL | Fichiers avec permissions | HTTPS |
| Secrets application | Keyring ou fichier chiffré | Master key |
| Configuration sensible | Fichier chiffré ou Vault | Database credentials |
| Cache temporaire | Mémoire chiffrée | Données temporaires |

### Données en mémoire

```pascal
type
  TSecureString = class
  private
    FData: array of Byte;
    FEncrypted: Boolean;
    procedure Encrypt;
    procedure Decrypt;
  public
    constructor Create(const PlainText: string);
    destructor Destroy; override;
    function GetValue: string;
    procedure Clear;
  end;

// Utilisation
var
  Password: TSecureString;
begin
  Password := TSecureString.Create('MyPassword123');
  try
    // Les données sont chiffrées en mémoire
    DoSomething(Password.GetValue);
  finally
    Password.Free; // Destruction sécurisée
  end;
end;
```

### Données sur disque

```pascal
// Structure pour stockage sécurisé
type
  TSecureStorageHeader = packed record
    Magic: array[0..3] of Char;  // 'SSTR'
    Version: Word;
    Algorithm: Word;              // 1 = AES-256-CBC
    IVSize: Word;
    SaltSize: Word;
    DataSize: Cardinal;
  end;

function SaveSecureData(const FileName: string;
                        const Data: string;
                        const Password: string): Boolean;
var
  Header: TSecureStorageHeader;
  Salt, IV: TBytes;
  EncryptedData: TBytes;
  F: TFileStream;
begin
  // Générer sel et IV
  Salt := GenerateRandomBytes(16);
  IV := GenerateRandomBytes(16);

  // Dériver la clé du mot de passe
  Key := DeriveKeyPBKDF2(Password, Salt, 100000);

  // Chiffrer les données
  EncryptedData := EncryptAES256(Data, Key, IV);

  // Écrire le fichier
  F := TFileStream.Create(FileName, fmCreate);
  try
    // Header
    Header.Magic := 'SSTR';
    Header.Version := 1;
    Header.Algorithm := 1;
    Header.IVSize := Length(IV);
    Header.SaltSize := Length(Salt);
    Header.DataSize := Length(EncryptedData);

    F.Write(Header, SizeOf(Header));
    F.Write(Salt[0], Length(Salt));
    F.Write(IV[0], Length(IV));
    F.Write(EncryptedData[0], Length(EncryptedData));

    Result := True;
  finally
    F.Free;
  end;
end;
```

## Gestion des mots de passe

### Jamais en clair !

```pascal
// ❌ TRÈS DANGEREUX - Ne JAMAIS faire
type
  TUser = record
    Username: string;
    Password: string;  // En clair !
  end;

// ✅ CORRECT
type
  TUser = record
    Username: string;
    PasswordHash: string;
    PasswordSalt: string;
  end;
```

### Processus de stockage

```
1. Utilisateur crée un mot de passe
   ↓
2. Générer un sel aléatoire unique
   ↓
3. Dériver une clé avec PBKDF2/bcrypt/Argon2
   ↓
4. Stocker : hash + sel (jamais le mot de passe)
```

### Vérification

```
1. Utilisateur entre son mot de passe
   ↓
2. Récupérer le sel stocké
   ↓
3. Dériver la clé avec le même algorithme
   ↓
4. Comparer les hashs (temps constant)
```

## Variables d'environnement

### Avantages et limitations

**Avantages** :
- ✅ Séparation code/configuration
- ✅ Facile à changer sans recompilation
- ✅ Standard sur serveurs

**Limitations** :
- ⚠️ Visibles dans la liste des processus
- ⚠️ Peuvent être loggées
- ⚠️ Accessible aux processus enfants

### Utilisation sécurisée

```pascal
function GetSecretFromEnv(const VarName: string): string;  
begin
  Result := GetEnvironmentVariable(VarName);

  if Result = '' then
    raise Exception.CreateFmt('Variable %s non définie', [VarName]);

  // Logger l'utilisation (pas la valeur !)
  LogInfo('Secret chargé depuis variable env: ' + VarName);
end;

// Utilisation
var
  APIKey: string;
begin
  {$IFDEF DEBUG}
  // En développement : valeur de test
  APIKey := 'test_api_key';
  {$ELSE}
  // En production : depuis variable d'environnement
  APIKey := GetSecretFromEnv('API_KEY');
  {$ENDIF}
end;
```

## Fichiers de configuration

### Configuration en clair (non sensible)

```ini
; config.ini - Données NON sensibles
[Application]
Name=MonApp  
Version=1.0.0  
LogLevel=INFO

[Server]
Host=api.example.com  
Port=443  
Timeout=30
```

### Configuration chiffrée (sensible)

```pascal
type
  TSecureConfig = class
  private
    FValues: TStringList;
    FMasterKey: string;
  public
    constructor Create(const MasterKey: string);
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  end;

// Utilisation
var
  Config: TSecureConfig;
begin
  Config := TSecureConfig.Create(GetMasterKey);
  try
    Config.LoadFromFile('secure.conf');

    // Les valeurs sont déchiffrées à la demande
    DatabasePassword := Config.GetValue('DB_PASSWORD');
    APIKey := Config.GetValue('API_KEY');
  finally
    Config.Free;
  end;
end;
```

## Abstraction multi-plateforme

### Interface commune

```pascal
type
  ISecureStorage = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function Store(const Key, Value: string): Boolean;
    function Retrieve(const Key: string): string;
    function Delete(const Key: string): Boolean;
    function Exists(const Key: string): Boolean;
  end;

  TSecureStorageFactory = class
  public
    class function CreateStorage: ISecureStorage;
  end;

class function TSecureStorageFactory.CreateStorage: ISecureStorage;  
begin
  {$IFDEF WINDOWS}
  Result := TWindowsDPAPIStorage.Create;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := TLinuxKeyringStorage.Create;
  {$ENDIF}
end;

// Utilisation transparente
var
  Storage: ISecureStorage;
begin
  Storage := TSecureStorageFactory.CreateStorage;

  // Même code sur Windows et Linux
  Storage.Store('api_key', 'sk_live_abc123...');

  // Plus tard...
  APIKey := Storage.Retrieve('api_key');
end;
```

## Rotation des secrets

### Pourquoi faire tourner les secrets ?

- Limiter l'impact d'une fuite
- Conformité réglementaire
- Bonne pratique de sécurité

### Implémentation

```pascal
type
  TSecretVersion = record
    Version: Integer;
    CreatedAt: TDateTime;
    ExpiresAt: TDateTime;
    Value: string;
    Active: Boolean;
  end;

  TSecretRotation = class
  private
    FSecrets: TList<TSecretVersion>;
  public
    procedure AddVersion(const Secret: string; ValidityDays: Integer);
    function GetActiveSecret: string;
    function GetSecretByVersion(Version: Integer): string;
    procedure RotateSecret(const NewSecret: string);
    procedure ExpireOldVersions;
  end;

procedure TSecretRotation.RotateSecret(const NewSecret: string);  
begin
  // Désactiver l'ancienne version (mais la garder pour transition)
  if FSecrets.Count > 0 then
    FSecrets.Last.Active := False;

  // Ajouter la nouvelle version
  AddVersion(NewSecret, 90); // Valide 90 jours

  // Nettoyer les versions expirées
  ExpireOldVersions;
end;
```

## Audit et logging

### Ce qu'il faut logger

**✅ À logger** :
- Tentatives d'accès aux secrets
- Création/modification de secrets
- Échecs d'authentification
- Rotation de secrets
- Accès réussis (avec qui, quand, quoi)

**❌ Ne JAMAIS logger** :
- Les valeurs des secrets eux-mêmes
- Les mots de passe (même hashés)
- Les clés de chiffrement

### Exemple de logging sécurisé

```pascal
procedure LogSecretAccess(const SecretName, Action: string; Success: Boolean);  
begin
  WriteToLog(Format(
    '[SECURITY] %s - Secret: %s, Action: %s, Success: %s, User: %s, IP: %s',
    [DateTimeToStr(Now), SecretName, Action, BoolToStr(Success),
     GetCurrentUser, GetClientIP]
  ));

  // Alerter si échec suspect
  if not Success then
    IncrementFailedAttempts(SecretName);
end;

// Utilisation
procedure RetrieveSecret(const Name: string);  
begin
  try
    Secret := Storage.Retrieve(Name);
    LogSecretAccess(Name, 'RETRIEVE', True);
  except
    on E: Exception do
    begin
      LogSecretAccess(Name, 'RETRIEVE', False);
      raise;
    end;
  end;
end;
```

## Considérations de performance

### Cache en mémoire

```pascal
type
  TSecretCache = class
  private
    FCache: TDictionary<string, string>;
    FCacheTime: TDictionary<string, TDateTime>;
    FCacheDuration: TDateTime;
  public
    constructor Create(CacheDurationMinutes: Integer = 5);
    function Get(const Key: string): string;
    procedure Invalidate(const Key: string);
    procedure Clear;
  end;

function TSecretCache.Get(const Key: string): string;  
var
  CachedTime: TDateTime;
begin
  // Vérifier le cache
  if FCache.TryGetValue(Key, Result) then
  begin
    if FCacheTime.TryGetValue(Key, CachedTime) then
    begin
      if Now - CachedTime < FCacheDuration then
        Exit; // Cache valide
    end;
  end;

  // Cache expiré ou inexistant, recharger
  Result := Storage.Retrieve(Key);
  FCache.AddOrSetValue(Key, Result);
  FCacheTime.AddOrSetValue(Key, Now);
end;
```

### Lazy loading

```pascal
type
  TLazySecret = class
  private
    FKey: string;
    FValue: string;
    FLoaded: Boolean;
    function GetValue: string;
  public
    constructor Create(const Key: string);
    property Value: string read GetValue;
  end;

function TLazySecret.GetValue: string;  
begin
  if not FLoaded then
  begin
    FValue := Storage.Retrieve(FKey);
    FLoaded := True;
  end;
  Result := FValue;
end;
```

## Conclusion de l'introduction

Le stockage sécurisé des données est un aspect critique de la sécurité des applications. Il ne suffit pas de chiffrer ; il faut comprendre les menaces, choisir les bonnes méthodes pour chaque type de donnée, et implémenter correctement les protections.

**Principes essentiels** :
- 🔒 Chiffrer les données au repos
- 🔑 Séparer les clés des données
- 🚫 Ne jamais stocker en clair
- 📊 Logger les accès (pas les valeurs)
- 🔄 Faire tourner les secrets régulièrement
- 🎯 Utiliser les API système quand possible

Dans les sections suivantes, nous verrons en détail comment implémenter ces principes sur Windows et Ubuntu/Linux avec FreePascal et Lazarus.

**Prochaines sections** :
- 17.6.1 DPAPI Windows
- 17.6.2 Keyring Linux/GNOME

⏭️ [DPAPI Windows](/17-securite-cryptographie/06.1-dpapi-windows.md)
