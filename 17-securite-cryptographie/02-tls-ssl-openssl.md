🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.2 TLS/SSL avec OpenSSL

## Introduction

Dans le monde connecté d'aujourd'hui, la sécurisation des communications réseau est absolument essentielle. Que vous développiez un client web, un serveur API, ou une application qui communique avec des services distants, vous devez protéger les données en transit contre l'interception et la modification.

TLS/SSL (Transport Layer Security / Secure Sockets Layer) est le standard de facto pour sécuriser les communications sur Internet. OpenSSL est la bibliothèque la plus utilisée pour implémenter ces protocoles, et elle est disponible sur toutes les plateformes majeures, y compris Windows et Linux.

## Qu'est-ce que TLS/SSL ?

### SSL : Secure Sockets Layer

SSL a été développé par Netscape dans les années 1990 pour sécuriser les communications web. Il a connu plusieurs versions :
- SSL 1.0 (jamais publié publiquement)
- SSL 2.0 (1995) - **Obsolète et non sécurisé**
- SSL 3.0 (1996) - **Obsolète et non sécurisé**

### TLS : Transport Layer Security

TLS est le successeur de SSL, standardisé par l'IETF :
- TLS 1.0 (1999) - Basé sur SSL 3.0
- TLS 1.1 (2006) - **Déprécié**
- TLS 1.2 (2008) - **Encore largement utilisé**
- TLS 1.3 (2018) - **Version actuelle recommandée**

**Note importante** : Bien qu'on parle souvent de "SSL", on utilise en réalité TLS dans les applications modernes. Le terme "SSL/TLS" ou simplement "TLS" est plus approprié.

### À quoi sert TLS/SSL ?

TLS fournit trois garanties essentielles :

**1. Confidentialité** : Les données sont chiffrées, personne ne peut les lire en transit
```
Client → [Données chiffrées] → Serveur
        ↑ Attaquant ne peut pas lire ↑
```

**2. Intégrité** : Les données ne peuvent pas être modifiées sans détection
```
Si un attaquant modifie les données → Détection et rejet
```

**3. Authentification** : Vérification de l'identité du serveur (et optionnellement du client)
```
Client vérifie : "Est-ce vraiment le serveur de ma banque ?"
```

## Comment fonctionne TLS/SSL ?

### Le protocole de handshake (poignée de main)

Lorsqu'un client se connecte à un serveur TLS, ils effectuent un "handshake" :

```
1. Client Hello
   Client → Serveur : "Bonjour, je supporte TLS 1.3, 1.2..."

2. Server Hello
   Serveur → Client : "OK, utilisons TLS 1.2"
   Serveur → Client : [Certificat du serveur]

3. Vérification du certificat
   Client vérifie : Le certificat est-il valide ?
                    Est-il signé par une autorité de confiance ?
                    Le nom correspond-il au serveur ?

4. Échange de clés
   Client et Serveur : Négocient une clé de session partagée

5. Communication chiffrée
   Client ↔ Serveur : Données chiffrées avec la clé de session
```

### Les certificats numériques

Un certificat numérique est comme une carte d'identité pour un serveur. Il contient :
- Le nom de domaine (exemple : `www.exemple.com`)
- La clé publique du serveur
- La période de validité
- L'autorité de certification (CA) qui l'a signé

**Types de certificats** :
- **DV (Domain Validated)** : Vérifie juste la propriété du domaine
- **OV (Organization Validated)** : Vérifie l'organisation
- **EV (Extended Validation)** : Vérification approfondie de l'organisation

### Les autorités de certification (CA)

Les CA sont des organisations de confiance qui signent les certificats :
- Let's Encrypt (gratuit, automatisé)
- DigiCert
- GlobalSign
- Comodo

Votre système d'exploitation et navigateur contiennent une liste de CA de confiance.

## Qu'est-ce qu'OpenSSL ?

OpenSSL est une bibliothèque logicielle robuste et complète qui implémente :
- Les protocoles TLS/SSL
- Des algorithmes de chiffrement
- Des fonctions de hachage
- La gestion de certificats
- Et bien plus encore

### Historique d'OpenSSL

- **1998** : Création du projet OpenSSL
- **2014** : Découverte de Heartbleed, une faille critique
- **2016** : Fork de LibreSSL (par OpenBSD) et BoringSSL (par Google)
- **2018** : Sortie d'OpenSSL 1.1.1 avec TLS 1.3
- **2021** : Sortie d'OpenSSL 3.0 avec nouvelle architecture

### Versions d'OpenSSL

| Version | Date de sortie | Support | Recommandation |
|---------|----------------|---------|----------------|
| OpenSSL 1.0.2 | 2015 | Fin de vie (2019) | ❌ Ne plus utiliser |
| OpenSSL 1.1.0 | 2016 | Fin de vie (2019) | ❌ Ne plus utiliser |
| OpenSSL 1.1.1 | 2018 | LTS jusqu'en 2023 | ⚠️ Migration recommandée |
| OpenSSL 3.0 | 2021 | LTS jusqu'en 2026 | ✅ Version actuelle |
| OpenSSL 3.1+ | 2023+ | Support standard | ✅ Dernières fonctionnalités |

## Pourquoi utiliser OpenSSL avec FreePascal ?

### Avantages

**1. Standard de l'industrie**
- Utilisé par la majorité des serveurs web
- Largement testé et audité
- Support de tous les protocoles modernes

**2. Multi-plateforme**
- Disponible sur Windows, Linux, macOS, BSD, etc.
- Même API sur toutes les plateformes
- Code portable entre OS

**3. Performance**
- Implémentations optimisées
- Support des accélérateurs matériels (AES-NI)
- Utilisable en production

**4. Fonctionnalités complètes**
- Tous les algorithmes standards
- Gestion de certificats
- Support des formats standards (PEM, DER, PKCS#12)

### Alternatives à OpenSSL

**LibreSSL**
- Fork d'OpenSSL par OpenBSD
- Accent sur la sécurité et la simplicité
- API compatible avec OpenSSL 1.0.1

**BoringSSL**
- Fork par Google
- Optimisé pour Chrome et Android
- API non compatible, en évolution constante

**mbedTLS** (anciennement PolarSSL)
- Léger, conçu pour l'embarqué
- API différente d'OpenSSL
- Excellente documentation

**GnuTLS**
- Alternative complète à OpenSSL
- Licence LGPL
- API différente

Pour FreePascal/Lazarus, **OpenSSL reste le choix recommandé** car :
- Nombreux bindings disponibles
- Large communauté
- Documentation abondante

## Concepts clés de TLS/SSL

### Chiffrement symétrique vs asymétrique

**Dans le handshake TLS** :
1. Chiffrement **asymétrique** (RSA, ECDH) : Pour échanger la clé de session
2. Chiffrement **symétrique** (AES, ChaCha20) : Pour chiffrer les données

Pourquoi cette combinaison ?
- Asymétrique : Sécurise l'échange de clés sans secret partagé préalable
- Symétrique : Beaucoup plus rapide pour chiffrer de grandes quantités de données

### Cipher Suites (suites de chiffrement)

Une cipher suite définit les algorithmes utilisés pour :
- L'échange de clés (exemple : ECDHE)
- L'authentification (exemple : RSA)
- Le chiffrement (exemple : AES-256-GCM)
- L'intégrité (exemple : SHA-384)

Exemple de cipher suite : `TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384`

Décomposition :
- `TLS` : Protocole TLS
- `ECDHE` : Échange de clés avec Elliptic Curve Diffie-Hellman Ephemeral
- `RSA` : Authentification par RSA
- `AES_256_GCM` : Chiffrement AES 256 bits en mode GCM
- `SHA384` : Fonction de hachage SHA-384

### Perfect Forward Secrecy (PFS)

Le PFS garantit que même si la clé privée du serveur est compromise, les sessions passées restent sécurisées.

**Comment ?** En utilisant des clés de session éphémères (temporaires) :
- Chaque session utilise une nouvelle clé
- Les clés ne sont jamais stockées
- Utilise ECDHE ou DHE

**Cipher suites avec PFS** :
- `TLS_ECDHE_*` ✅
- `TLS_DHE_*` ✅
- `TLS_RSA_*` ❌ (pas de PFS)

### SNI : Server Name Indication

SNI permet d'héberger plusieurs sites HTTPS sur la même adresse IP.

**Problème sans SNI** :
```
IP 192.168.1.1 → Un seul certificat possible
```

**Avec SNI** :
```
IP 192.168.1.1 → Certificat pour site1.com
               → Certificat pour site2.com
               → Certificat pour site3.com
```

Le client indique le nom du site dans le handshake, et le serveur présente le bon certificat.

## OpenSSL : Les composants

### La bibliothèque OpenSSL

OpenSSL est composé de plusieurs bibliothèques :

**libssl** : Implémentation de TLS/SSL
- Gestion des connexions TLS
- Handshake protocol
- Sessions et cache

**libcrypto** : Algorithmes cryptographiques
- Chiffrement (AES, DES, etc.)
- Hachage (SHA, MD5, etc.)
- Clés publiques (RSA, EC, etc.)

### L'outil en ligne de commande

OpenSSL fournit aussi un outil en ligne de commande très puissant :

```bash
# Générer une clé privée
openssl genrsa -out private.key 2048

# Générer un certificat auto-signé
openssl req -new -x509 -key private.key -out certificate.crt -days 365

# Tester une connexion TLS
openssl s_client -connect www.google.com:443

# Afficher les informations d'un certificat
openssl x509 -in certificate.crt -text -noout

# Convertir un certificat PEM en DER
openssl x509 -in cert.pem -outform DER -out cert.der
```

## Utiliser OpenSSL avec FreePascal

### Bindings disponibles

Plusieurs options existent pour utiliser OpenSSL avec FreePascal :

**1. Synapse**
- Bibliothèque réseau complète
- Bindings OpenSSL intégrés
- Simple d'utilisation
- Recommandé pour débuter

**2. Indy (Internet Direct)**
- Suite de composants réseau
- Support SSL via OpenSSL
- Nombreux protocoles

**3. Bindings OpenSSL natifs**
- Plus bas niveau
- Contrôle total
- Nécessite plus de code

**4. mORMot**
- Framework complet
- Bindings OpenSSL optimisés
- Excellentes performances

### Exemple conceptuel avec Synapse

```pascal
uses
  httpsend, ssl_openssl;

function GetHTTPS(const URL: string): string;  
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    if HTTP.HTTPMethod('GET', URL) then
      Result := HTTP.Document.DataString
    else
      Result := 'Erreur: ' + IntToStr(HTTP.ResultCode);
  finally
    HTTP.Free;
  end;
end;
```

Synapse charge automatiquement les bibliothèques OpenSSL et gère le TLS.

### Chargement des bibliothèques OpenSSL

Sur Windows et Linux, OpenSSL est constitué de fichiers de bibliothèque :

**Windows** :
- `libssl-3.dll` (ou `ssleay32.dll` pour les anciennes versions)
- `libcrypto-3.dll` (ou `libeay32.dll`)

**Linux** :
- `libssl.so.3` (ou `libssl.so.1.1`)
- `libcrypto.so.3` (ou `libcrypto.so.1.1`)

Les bindings FreePascal chargent dynamiquement ces bibliothèques au démarrage.

## Vérification et validation de certificats

### Chaîne de confiance

Un certificat est généralement signé par une CA intermédiaire, elle-même signée par une CA racine :

```
Certificat racine (CA racine - auto-signé)
    ↓ signe
Certificat intermédiaire (CA intermédiaire)
    ↓ signe
Certificat du serveur (www.exemple.com)
```

Le client doit vérifier toute la chaîne jusqu'à une CA racine de confiance.

### Ce que vérifie un client TLS

1. **Validité temporelle** : Le certificat est-il dans sa période de validité ?
2. **Chaîne de confiance** : La signature remonte-t-elle à une CA de confiance ?
3. **Révocation** : Le certificat a-t-il été révoqué ? (CRL ou OCSP)
4. **Nom de domaine** : Le nom dans le certificat correspond-il au serveur ?
5. **Usage** : Le certificat est-il autorisé pour TLS serveur ?

### Certificate Pinning

Pour une sécurité accrue, vous pouvez "épingler" un certificat :

```pascal
// Concept : vérifier que le certificat reçu correspond exactement
// à celui attendu (ou sa clé publique)
const
  EXPECTED_CERT_HASH = 'A1B2C3D4...'; // Hash SHA-256 du certificat

function VerifyCertificate(const ReceivedCertHash: string): Boolean;  
begin
  Result := (ReceivedCertHash = EXPECTED_CERT_HASH);
end;
```

**Avantages** :
- Protection contre les CA compromises
- Protection contre les attaques MITM avec faux certificats

**Inconvénients** :
- Moins flexible (renouvellement de certificat compliqué)
- Nécessite une mise à jour de l'application

## Considérations de sécurité

### Désactiver les protocoles obsolètes

```pascal
// Concept : configurer pour n'accepter que TLS 1.2+
SSL.MinVersion := TLS1_2_VERSION;  
SSL.MaxVersion := TLS1_3_VERSION;
```

**À désactiver** :
- SSL 2.0 ❌
- SSL 3.0 ❌ (vulnérable à POODLE)
- TLS 1.0 ❌ (vulnérable à BEAST)
- TLS 1.1 ❌

**À utiliser** :
- TLS 1.2 ✅
- TLS 1.3 ✅ (recommandé)

### Configurer des cipher suites sécurisées

```pascal
// Concept : définir les cipher suites acceptables
const
  SECURE_CIPHERS =
    'ECDHE-RSA-AES256-GCM-SHA384:' +
    'ECDHE-RSA-AES128-GCM-SHA256:' +
    'ECDHE-RSA-CHACHA20-POLY1305';
```

**À éviter** :
- Cipher suites avec DES ou 3DES ❌
- Cipher suites sans PFS (RSA key exchange) ❌
- Cipher suites avec MD5 ou SHA1 ❌
- Cipher suites EXPORT ❌

### Validation stricte des certificats

```pascal
// Toujours activer la validation des certificats
SSL.VerifyMode := SSL_VERIFY_PEER;  
SSL.VerifyDepth := 9; // Profondeur de la chaîne

// Ne JAMAIS ignorer les erreurs de certificat en production !
// Sauf pour les tests en développement
{$IFDEF DEBUG}
  SSL.VerifyMode := SSL_VERIFY_NONE; // UNIQUEMENT EN DEV !
{$ENDIF}
```

## Certificats pour le développement

### Certificats auto-signés

Pour les tests en développement :

```bash
# Générer une clé privée
openssl genrsa -out server.key 2048

# Générer un certificat auto-signé valide 365 jours
openssl req -new -x509 -key server.key -out server.crt -days 365 \
  -subj "/C=FR/ST=Normandie/L=Rouen/O=MaCompagnie/CN=localhost"
```

**Attention** : Les certificats auto-signés ne sont **jamais** acceptables en production !

### Let's Encrypt pour la production

Let's Encrypt fournit des certificats gratuits et automatisables :

```bash
# Installation de certbot (Ubuntu)
sudo apt install certbot

# Obtenir un certificat
sudo certbot certonly --standalone -d www.exemple.com
```

Les certificats sont placés dans `/etc/letsencrypt/live/www.exemple.com/`

## Performances et optimisation

### Session resumption (reprise de session)

Évite de refaire le handshake complet pour les reconnexions :

**Session IDs** :
- Le serveur assigne un ID de session
- Le client peut réutiliser cette session

**Session Tickets** (TLS 1.2+) :
- Le serveur chiffre l'état de la session
- Le client stocke le ticket
- Pas besoin de cache côté serveur

### OCSP Stapling

Au lieu que le client vérifie la révocation auprès de la CA :
- Le serveur obtient la réponse OCSP
- Le serveur "agrafe" (staple) cette réponse au certificat
- Réduit la latence et protège la vie privée

### Accélération matérielle

OpenSSL peut utiliser les instructions CPU dédiées :
- **AES-NI** : Instructions Intel/AMD pour AES
- **AVX** : Calculs vectoriels
- Peut multiplier les performances par 5-10x

## Débogage et diagnostic

### Outils de diagnostic

**1. OpenSSL s_client**
```bash
# Tester une connexion et voir les détails
openssl s_client -connect www.google.com:443 -showcerts

# Tester avec un protocole spécifique
openssl s_client -connect www.example.com:443 -tls1_2

# Tester avec SNI
openssl s_client -connect 192.168.1.1:443 -servername www.example.com
```

**2. Wireshark**
- Capture et analyse du trafic réseau
- Peut déchiffrer TLS si vous avez la clé privée
- Visualise le handshake TLS

**3. SSL Labs (en ligne)**
- https://www.ssllabs.com/ssltest/
- Teste la configuration SSL/TLS d'un serveur
- Note de A+ à F

### Erreurs courantes

| Erreur | Signification | Solution |
|--------|---------------|----------|
| `certificate verify failed` | Certificat invalide ou non fiable | Vérifier la chaîne de certification |
| `unknown ca` | CA non reconnue | Ajouter la CA au trust store |
| `certificate has expired` | Certificat expiré | Renouveler le certificat |
| `handshake failure` | Incompatibilité de configuration | Vérifier les cipher suites |
| `ssl3_get_record:wrong version number` | Problème de version SSL/TLS | Vérifier les versions supportées |

## Architecture multi-plateforme

### Abstraction des différences

Pour créer une application portable, créez une couche d'abstraction :

```pascal
type
  TSSLConfig = record
    MinVersion: Integer;
    MaxVersion: Integer;
    CipherList: string;
    CertFile: string;
    KeyFile: string;
    CAPath: string;
  end;

function InitSSL(const Config: TSSLConfig): Boolean;  
begin
  {$IFDEF WINDOWS}
    Result := InitSSL_Windows(Config);
  {$ENDIF}
  {$IFDEF UNIX}
    Result := InitSSL_Unix(Config);
  {$ENDIF}
end;
```

### Chemins de configuration

Les emplacements varient selon l'OS :

**Windows** :
- Certificats : Souvent avec l'application ou dans `%PROGRAMDATA%`
- Pas de store système par défaut pour OpenSSL
- OpenSSL DLLs : À côté de l'exécutable ou dans `System32`

**Ubuntu/Linux** :
- Certificats CA : `/etc/ssl/certs/`
- Certificats serveur : `/etc/ssl/` ou `/etc/letsencrypt/`
- Bibliothèques : `/usr/lib/x86_64-linux-gnu/` ou `/usr/lib/`

## Ressources et documentation

### Documentation officielle

- **OpenSSL Documentation** : https://www.openssl.org/docs/
- **OpenSSL Wiki** : https://wiki.openssl.org/
- **RFCs TLS** :
  - TLS 1.2 : RFC 5246
  - TLS 1.3 : RFC 8446

### Livres et guides

- *Bulletproof SSL and TLS* par Ivan Ristić
- *Network Security with OpenSSL* par Viega, Messier, Chandra
- *OpenSSL Cookbook* par Ivan Ristić (gratuit en ligne)

### Communautés

- OpenSSL Users Mailing List
- Stack Overflow (tag: openssl)
- Forums FreePascal/Lazarus

## Conclusion de l'introduction

TLS/SSL avec OpenSSL est la pierre angulaire de la sécurité des communications réseau. Que vous développiez un client HTTP, un serveur API, ou toute application communiquant sur le réseau, une compréhension solide de TLS/SSL est indispensable.

OpenSSL offre une implémentation robuste, performante et multi-plateforme de ces protocoles. Avec FreePascal et Lazarus, vous avez accès à plusieurs bibliothèques qui facilitent l'intégration d'OpenSSL dans vos applications.

**Points clés à retenir** :
- TLS 1.2+ uniquement, jamais SSL 3.0 ou inférieur
- Toujours valider les certificats en production
- Utiliser des cipher suites modernes avec PFS
- Comprendre la chaîne de certification
- Tester régulièrement votre configuration SSL/TLS
- Garder OpenSSL à jour pour les correctifs de sécurité

Dans les sections suivantes, nous verrons comment configurer OpenSSL sur Windows et Ubuntu, puis comment l'utiliser concrètement dans vos applications FreePascal.

---

**Prochaines sections** :
- 17.2.1 Configuration OpenSSL Windows
- 17.2.2 Configuration OpenSSL Ubuntu

⏭️ [Configuration OpenSSL Windows](/17-securite-cryptographie/02.1-configuration-openssl-windows.md)
