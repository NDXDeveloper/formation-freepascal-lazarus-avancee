🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17. Sécurité et Cryptographie

## Introduction générale

La sécurité est devenue un enjeu majeur dans le développement d'applications modernes. Que vous développiez une application de bureau, un service web, ou une application mobile, la protection des données et la sécurité des communications sont essentielles.

Dans ce chapitre, nous allons explorer les différentes facettes de la sécurité et de la cryptographie dans le contexte du développement multi-plateforme avec FreePascal et Lazarus. Nous verrons comment implémenter des solutions de sécurité robustes qui fonctionnent de manière cohérente sur Windows et Ubuntu/Linux.

## Pourquoi la sécurité est-elle cruciale ?

### Protéger les données sensibles

Les applications modernes manipulent souvent des données sensibles :
- Informations personnelles des utilisateurs (nom, adresse, date de naissance)
- Données financières (numéros de carte bancaire, comptes)
- Mots de passe et identifiants
- Documents confidentiels d'entreprise
- Données médicales

Une fuite de ces données peut avoir des conséquences graves : vol d'identité, pertes financières, atteinte à la réputation, sanctions légales (RGPD).

### Sécuriser les communications

Lorsque votre application communique via le réseau :
- Les données peuvent être interceptées par des tiers malveillants
- Les messages peuvent être modifiés en transit
- Un attaquant peut se faire passer pour votre serveur (attaque "man-in-the-middle")

### Se conformer aux réglementations

De nombreuses réglementations imposent des mesures de sécurité :
- **RGPD** (Règlement Général sur la Protection des Données) en Europe
- **HIPAA** pour les données de santé aux États-Unis
- **PCI DSS** pour les transactions par carte bancaire
- **ISO 27001** pour la gestion de la sécurité de l'information

## Les principes fondamentaux de la sécurité

### La triade CIA (Confidentialité, Intégrité, Disponibilité)

**Confidentialité (Confidentiality)**
- Seules les personnes autorisées peuvent accéder aux données
- Mise en œuvre : chiffrement, contrôle d'accès, authentification

**Intégrité (Integrity)**
- Les données ne peuvent pas être modifiées sans détection
- Mise en œuvre : signatures numériques, hachage, checksums

**Disponibilité (Availability)**
- Les données et services sont accessibles quand nécessaire
- Mise en œuvre : sauvegardes, redondance, protection contre les DoS

### Authentification vs Autorisation

**Authentification** : Vérifier l'identité d'un utilisateur
- "Qui êtes-vous ?"
- Méthodes : mot de passe, biométrie, certificats, 2FA

**Autorisation** : Déterminer ce qu'un utilisateur peut faire
- "Que pouvez-vous faire ?"
- Méthodes : rôles, permissions, ACL (Access Control Lists)

### Le principe du moindre privilège

Chaque composant de votre application ne devrait avoir que les permissions strictement nécessaires à son fonctionnement :
- Un utilisateur ne devrait pas avoir de droits administrateur sauf si nécessaire
- Une application ne devrait pas demander plus de permissions qu'elle n'en a besoin
- Les processus devraient s'exécuter avec le minimum de privilèges

## Menaces courantes et attaques

### Injection (SQL, Command, LDAP)

L'injection consiste à insérer du code malveillant dans une requête :

```pascal
// ❌ DANGEREUX - Vulnérable à l'injection SQL
Query.SQL.Text := 'SELECT * FROM Users WHERE Username = ''' +
                  UserInput + '''';

// ✅ SÉCURISÉ - Utilisation de paramètres
Query.SQL.Text := 'SELECT * FROM Users WHERE Username = :Username';  
Query.ParamByName('Username').AsString := UserInput;
```

### Cross-Site Scripting (XSS)

Dans les applications web, un attaquant injecte du JavaScript malveillant :

```pascal
// ❌ DANGEREUX
Response.Content := '<div>' + UserComment + '</div>';

// ✅ SÉCURISÉ - Échapper le HTML
Response.Content := '<div>' + HTMLEncode(UserComment) + '</div>';
```

### Attaques par force brute

Un attaquant essaie toutes les combinaisons possibles pour deviner un mot de passe.

**Protection** :
- Imposer des mots de passe forts
- Limiter le nombre de tentatives
- Implémenter un délai progressif entre les tentatives
- Utiliser CAPTCHA

### Man-in-the-Middle (MITM)

Un attaquant intercepte et peut modifier les communications entre deux parties.

**Protection** :
- Utiliser TLS/SSL pour chiffrer les communications
- Valider les certificats
- Utiliser le certificate pinning pour les applications critiques

### Déni de service (DoS/DDoS)

Surcharger un système pour le rendre indisponible.

**Protection** :
- Limitation du taux de requêtes (rate limiting)
- Validation des entrées
- Utilisation de CDN et services anti-DDoS

## Stratégies de défense en profondeur

La sécurité ne repose jamais sur une seule mesure. Il faut plusieurs couches de protection :

### 1. Sécurité au niveau de l'application

```pascal
// Validation des entrées
function ValidateEmail(const Email: string): Boolean;  
begin
  Result := (Pos('@', Email) > 0) and (Pos('.', Email) > Pos('@', Email));
  // En production, utilisez une regex plus robuste
end;

// Assainissement des données
function SanitizeInput(const Input: string): string;  
begin
  Result := StringReplace(Input, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  // Continuer pour les autres caractères dangereux
end;
```

### 2. Sécurité au niveau du réseau

- Pare-feu (firewall)
- Segmentation réseau
- VPN pour les connexions distantes
- Surveillance du trafic réseau

### 3. Sécurité au niveau du système

**Windows** :
- Windows Defender
- Contrôle de compte utilisateur (UAC)
- BitLocker pour le chiffrement de disque
- Windows Firewall

**Ubuntu/Linux** :
- SELinux ou AppArmor
- iptables/nftables pour le pare-feu
- LUKS pour le chiffrement de disque
- fail2ban pour bloquer les tentatives d'intrusion

### 4. Sécurité physique

- Accès contrôlé aux serveurs
- Destruction sécurisée des supports
- Surveillance vidéo

## Cryptographie : les bases

### Qu'est-ce que la cryptographie ?

La cryptographie est la science de la protection de l'information par transformation. Elle permet de :
- Cacher le contenu d'un message (confidentialité)
- Vérifier qu'un message n'a pas été altéré (intégrité)
- Prouver l'identité de l'émetteur (authentification)
- Empêcher qu'un émetteur nie avoir envoyé un message (non-répudiation)

### Types de cryptographie

**Cryptographie symétrique**
- Même clé pour chiffrer et déchiffrer
- Rapide et efficace
- Problème : échange sécurisé de la clé
- Exemples : AES, DES, Blowfish

**Cryptographie asymétrique**
- Paire de clés : publique (chiffrer) et privée (déchiffrer)
- Résout le problème d'échange de clés
- Plus lente que la symétrique
- Exemples : RSA, ECC

**Fonctions de hachage**
- Transformation unidirectionnelle
- Empreinte digitale des données
- Utilisée pour vérifier l'intégrité
- Exemples : SHA-256, SHA-512

### Utilisation typique en pratique

En pratique, on combine souvent les approches :

1. **Chiffrement hybride** :
   - Cryptographie asymétrique pour échanger une clé de session
   - Cryptographie symétrique pour chiffrer les données avec cette clé

2. **Signature numérique** :
   - Hash du message
   - Chiffrement du hash avec la clé privée
   - Vérification avec la clé publique

## Gestion sécurisée des mots de passe

### Règles d'or

1. **Jamais en clair** : Ne jamais stocker les mots de passe en texte clair
2. **Hachage avec sel** : Utiliser une fonction de hachage avec un sel unique
3. **Fonctions lentes** : Utiliser des fonctions spécialement conçues (bcrypt, scrypt, Argon2)
4. **Politique forte** : Imposer des mots de passe robustes

### Exemple conceptuel

```pascal
type
  TPasswordHash = record
    Hash: string;    // Le hash du mot de passe
    Salt: string;    // Le sel unique
    Algorithm: string; // L'algorithme utilisé
  end;

function HashPassword(const Password: string): TPasswordHash;  
begin
  // Générer un sel aléatoire
  Result.Salt := GenerateRandomSalt();

  // Utiliser un algorithme de hachage approprié
  Result.Hash := SecureHash(Password + Result.Salt);
  Result.Algorithm := 'SHA-256-PBKDF2';
end;

function VerifyPassword(const Password: string;
                        const StoredHash: TPasswordHash): Boolean;
var
  ComputedHash: string;
begin
  ComputedHash := SecureHash(Password + StoredHash.Salt);
  Result := (ComputedHash = StoredHash.Hash);
end;
```

## Considérations multi-plateformes

### Différences Windows vs Linux

**Stockage sécurisé des secrets**
- **Windows** : Data Protection API (DPAPI), Credential Manager
- **Linux** : Secret Service (libsecret), GNOME Keyring, KWallet

**Générateurs de nombres aléatoires**
- **Windows** : CryptGenRandom (CryptoAPI)
- **Linux** : /dev/urandom, getrandom()

**Permissions fichiers**
- **Windows** : ACL (Access Control Lists), attributs NTFS
- **Linux** : Permissions Unix (rwx), ACL POSIX

### Écrire du code portable

```pascal
{$IFDEF WINDOWS}
uses
  Windows, WinCrypt;

function GetSecureRandomBytes(Count: Integer): TBytes;  
var
  hProv: HCRYPTPROV;
begin
  SetLength(Result, Count);
  CryptAcquireContext(@hProv, nil, nil, PROV_RSA_FULL, 0);
  CryptGenRandom(hProv, Count, @Result[0]);
  CryptReleaseContext(hProv, 0);
end;
{$ENDIF}

{$IFDEF UNIX}
function GetSecureRandomBytes(Count: Integer): TBytes;  
var
  F: File;
begin
  SetLength(Result, Count);
  AssignFile(F, '/dev/urandom');
  Reset(F, 1);
  BlockRead(F, Result[0], Count);
  CloseFile(F);
end;
{$ENDIF}
```

## Bibliothèques et frameworks de sécurité

### Pour FreePascal/Lazarus

**Cryptographie**
- **DCPCrypt** : Bibliothèque cryptographique complète
- **mORMot Cryptography** : Suite cryptographique performante
- **OpenSSL bindings** : Accès à OpenSSL depuis FreePascal

**Réseau sécurisé**
- **Synapse** : Support SSL/TLS intégré
- **Indy** : Composants réseau avec SSL
- **lNet** : Bibliothèque réseau légère avec SSL

**Authentification**
- **JOSE/JWT** : JSON Web Tokens pour l'authentification
- **OAuth2** : Implémentations du protocole OAuth 2.0

### Bibliothèques système

**Windows**
- CryptoAPI / Cryptography API: Next Generation (CNG)
- Secure Channel (Schannel) pour SSL/TLS
- Active Directory pour l'authentification

**Linux/Ubuntu**
- OpenSSL / LibreSSL
- GnuTLS
- libgcrypt
- PAM (Pluggable Authentication Modules)

## Meilleures pratiques de développement sécurisé

### 1. Ne jamais faire confiance aux entrées utilisateur

```pascal
// Toujours valider et assainir
function ProcessUserInput(const Input: string): string;  
begin
  // Validation
  if Length(Input) > 1000 then
    raise Exception.Create('Input trop long');

  if not IsValidFormat(Input) then
    raise Exception.Create('Format invalide');

  // Assainissement
  Result := SanitizeInput(Input);
end;
```

### 2. Utiliser des bibliothèques éprouvées

Ne réinventez pas la roue en cryptographie. Utilisez des bibliothèques :
- Bien testées
- Activement maintenues
- Auditées par la communauté
- Conformes aux standards

### 3. Mettre à jour régulièrement

- Surveillez les avis de sécurité
- Appliquez les correctifs rapidement
- Utilisez des dépendances à jour

### 4. Journaliser les événements de sécurité

```pascal
procedure LogSecurityEvent(const Event: string; Level: TSecurityLevel);  
begin
  // Journaliser avec horodatage, utilisateur, IP, action
  WriteToSecurityLog(
    Format('[%s] [%s] %s - User: %s, IP: %s',
      [DateTimeToStr(Now), SecurityLevelToStr(Level),
       Event, CurrentUser, RemoteIP])
  );

  // Alerter si critique
  if Level = slCritical then
    SendSecurityAlert(Event);
end;
```

### 5. Effectuer des tests de sécurité

- Tests de pénétration (pentesting)
- Revues de code focalisées sur la sécurité
- Analyse statique du code (outils SAST)
- Scan de vulnérabilités des dépendances

## Ressources et standards

### Standards de sécurité

- **OWASP** (Open Web Application Security Project) : Top 10 des vulnérabilités
- **NIST** : Standards cryptographiques et bonnes pratiques
- **ISO 27001** : Gestion de la sécurité de l'information
- **PCI DSS** : Sécurité des données de paiement

### Ressources d'apprentissage

- **CWE** (Common Weakness Enumeration) : Catalogue des faiblesses logicielles
- **CVE** (Common Vulnerabilities and Exposures) : Base de données des vulnérabilités
- **SANS Institute** : Formation et certification en sécurité
- **Cryptography Stack Exchange** : Communauté de cryptographes

### Outils utiles

**Analyse de sécurité**
- **OWASP ZAP** : Scanner de sécurité d'applications web
- **Burp Suite** : Plateforme de test de sécurité web
- **Wireshark** : Analyse de trafic réseau

**Génération de certificats**
- **OpenSSL** : Génération et gestion de certificats
- **Let's Encrypt** : Certificats SSL/TLS gratuits
- **XCA** : Interface graphique pour la gestion de certificats

## Plan du chapitre

Dans les sections suivantes, nous allons approfondir :

- **17.1** DCPCrypt et algorithmes cryptographiques
- **17.2** TLS/SSL avec OpenSSL
- **17.3** Authentification et OAuth 2.0
- **17.4** JWT et tokens sécurisés
- **17.5** Hashing et signatures numériques
- **17.6** Stockage sécurisé de données
- **17.7** Analyse de vulnérabilités
- **17.8** Obfuscation et protection du code
- **17.9** Sandboxing et isolation
- **17.10** Audit et conformité GDPR
- **17.11** SELinux/AppArmor vs Windows Defender

## Conclusion de l'introduction

La sécurité n'est pas une fonctionnalité que l'on ajoute à la fin du développement. C'est une préoccupation qui doit être intégrée dès la conception de votre application.

En développement multi-plateforme avec FreePascal et Lazarus, vous avez accès à des outils puissants et des bibliothèques robustes pour implémenter une sécurité de niveau professionnel. Les différences entre Windows et Linux nécessitent une attention particulière, mais avec les bonnes abstractions, vous pouvez créer des applications sécurisées qui fonctionnent de manière cohérente sur toutes les plateformes.

**Principes à retenir** :
- La sécurité est une responsabilité permanente
- Défense en profondeur : plusieurs couches de protection
- Ne jamais faire confiance aux entrées
- Utiliser des bibliothèques cryptographiques éprouvées
- Tester, auditer, et mettre à jour régulièrement
- Se former continuellement aux nouvelles menaces

Dans la section suivante, nous commencerons par étudier DCPCrypt, une bibliothèque cryptographique complète pour FreePascal, qui nous permettra d'implémenter le chiffrement et le hachage dans nos applications.

⏭️ [DCPCrypt et algorithmes cryptographiques](/17-securite-cryptographie/01-dcpcrypt-algorithmes-cryptographiques.md)
