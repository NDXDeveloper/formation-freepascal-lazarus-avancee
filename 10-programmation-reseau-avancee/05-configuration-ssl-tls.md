🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.5 Configuration SSL/TLS

## Introduction

SSL (Secure Sockets Layer) et son successeur TLS (Transport Layer Security) sont des protocoles cryptographiques qui permettent de sécuriser les communications sur Internet. Ils sont essentiels pour protéger les données sensibles comme les mots de passe, les informations bancaires, ou toute information confidentielle transitant sur le réseau.

Aujourd'hui, pratiquement toutes les applications web professionnelles utilisent HTTPS (HTTP + TLS), et il est devenu indispensable de maîtriser la configuration SSL/TLS pour développer des applications sécurisées.

## Qu'est-ce que SSL/TLS ?

### Définitions

**SSL (Secure Sockets Layer)**
- Créé par Netscape dans les années 1990
- Dernière version : SSL 3.0 (1996)
- **Obsolète et dangereux** - ne plus utiliser !

**TLS (Transport Layer Security)**
- Successeur de SSL (depuis 1999)
- Versions actuelles :
  - TLS 1.2 (2008) - Encore largement utilisé
  - TLS 1.3 (2018) - **Recommandé**, plus rapide et sécurisé

**Note** : Par habitude, on parle souvent de "SSL" alors qu'on utilise en réalité TLS. Dans ce tutoriel, nous utiliserons TLS pour la configuration actuelle.

### Pourquoi utiliser SSL/TLS ?

```
Sans SSL/TLS (HTTP)          Avec SSL/TLS (HTTPS)
─────────────────────        ─────────────────────

Client ──[texte clair]──> Serveur     Client ──[chiffré]──> Serveur
                                           ▲
Pirate peut lire :                         │
- Mots de passe                     Pirate ne peut pas
- Données bancaires                 déchiffrer les données
- Informations personnelles                └─── Sécurisé
```

**Avantages de SSL/TLS :**

1. **Confidentialité** 🔒
   - Les données sont chiffrées
   - Impossible de lire le contenu intercepté

2. **Intégrité** ✅
   - Détection de toute modification
   - Les données ne peuvent pas être altérées

3. **Authentification** 🆔
   - Vérification de l'identité du serveur
   - Le client sait à qui il parle

4. **Confiance** 👍
   - Cadenas vert dans le navigateur
   - Meilleur référencement Google (SEO)

## Comment fonctionne SSL/TLS ?

### Le handshake TLS

Lorsqu'un client se connecte à un serveur HTTPS, voici ce qui se passe :

```
Client                                    Serveur
  │                                          │
  │────── 1. Client Hello ──────────────────>│
  │        (Versions TLS supportées,         │
  │         algorithmes de chiffrement)      │
  │                                          │
  │<────── 2. Server Hello ──────────────────│
  │        (Version TLS choisie,             │
  │         algorithme choisi,               │
  │         Certificat du serveur)           │
  │                                          │
  │── 3. Vérification du certificat ─────────│
  │    (Autorité de certification valide ?)  │
  │                                          │
  │────── 4. Génération de clé ─────────────>│
  │        (Clé de session chiffrée          │
  │         avec la clé publique)            │
  │                                          │
  │<────── 5. Confirmation ──────────────────│
  │                                          │
  │══════ 6. Communication chiffrée ═════════│
  │        (Avec la clé de session)          │
```

**Étapes détaillées :**

1. **Client Hello** : Le client envoie ses capacités (versions TLS, algorithmes)
2. **Server Hello** : Le serveur répond avec ses choix et son certificat
3. **Vérification** : Le client vérifie que le certificat est valide
4. **Échange de clés** : Génération d'une clé de session unique
5. **Confirmation** : Les deux parties confirment le chiffrement
6. **Communication** : Tout le trafic est maintenant chiffré

### Certificats numériques

Un certificat SSL/TLS est comme une carte d'identité pour un site web :

```
Certificat SSL/TLS
├─ Nom de domaine (example.com)
├─ Nom de l'organisation
├─ Clé publique
├─ Date de validité (début et fin)
├─ Autorité de certification (CA)
└─ Signature numérique
```

**Types de certificats :**

| Type | Validation | Utilisation | Prix |
|------|-----------|-------------|------|
| **DV** (Domain Validation) | Rapide (minutes) | Sites personnels, blogs | Gratuit - 50€/an |
| **OV** (Organization Validation) | Moyenne (quelques jours) | Sites d'entreprise | 50€ - 200€/an |
| **EV** (Extended Validation) | Complète (semaines) | Banques, e-commerce | 200€ - 500€/an |

**Exemples d'autorités de certification (CA) :**
- Let's Encrypt (gratuit, automatisé)
- DigiCert
- GlobalSign
- Comodo
- GoDaddy

### Chaîne de confiance

```
Root CA (Racine)
  │
  └─> Intermediate CA (Intermédiaire)
        │
        └─> Certificat de votre serveur
```

Les navigateurs font confiance aux Root CA préinstallées. Votre certificat doit être signé par une CA de confiance pour que les navigateurs l'acceptent.

## Concepts cryptographiques

### Chiffrement symétrique vs asymétrique

#### Chiffrement symétrique (Rapide)

```
Une seule clé pour chiffrer ET déchiffrer

Alice                           Bob
  │                              │
  │── Clé partagée : "ABC123" ───│
  │                              │
  │── Chiffré avec ABC123 ──────>│
  │                              │
  │                    Déchiffré avec ABC123
```

**Algorithmes :** AES, ChaCha20

**Problème :** Comment partager la clé de manière sécurisée ?

#### Chiffrement asymétrique (Lent mais sûr)

```
Deux clés : publique (pour chiffrer) et privée (pour déchiffrer)

Alice                           Bob
  │                              │
  │<── Clé publique de Bob ──────│
  │                              │
  │── Chiffré avec clé publique >│
  │                              │
  │                    Déchiffré avec clé privée
```

**Algorithmes :** RSA, ECDSA

**TLS utilise les deux :**
1. Asymétrique pour échanger la clé de session
2. Symétrique pour chiffrer les données (plus rapide)

### Fonctions de hachage

Pour vérifier l'intégrité :

```
Message → [Hachage] → Empreinte unique

"Hello" → SHA-256 → 185f8db32271fe25f561a6fc938b2e264306ec304eda518007d1764826381969

Propriétés :
- Toujours la même empreinte pour le même message
- Impossible de retrouver le message depuis l'empreinte
- La moindre modification change complètement l'empreinte
```

**Algorithmes courants :**
- SHA-256 (recommandé)
- SHA-384
- SHA-512

### Signatures numériques

Pour prouver l'authenticité :

```
1. Serveur hache le message
2. Serveur chiffre le hash avec sa clé privée = Signature
3. Client déchiffre la signature avec la clé publique
4. Client hache le message de son côté
5. Comparaison : les hashs correspondent ? Authentique !
```

## OpenSSL : La bibliothèque de référence

OpenSSL est la bibliothèque open-source la plus utilisée pour SSL/TLS. Elle est disponible sur toutes les plateformes et FreePascal peut l'utiliser facilement.

### Versions d'OpenSSL

| Version | Sortie | Support | Notes |
|---------|--------|---------|-------|
| 1.0.2 | 2015 | Fin 2019 | ❌ Obsolète |
| 1.1.0 | 2016 | Fin 2019 | ❌ Obsolète |
| 1.1.1 | 2018 | Sept 2023 | ⚠️ LTS, encore utilisé |
| 3.0.x | 2021 | Sept 2026 | ✅ Actuel |
| 3.1.x | 2023 | Mars 2025 | ✅ Actuel |

**Recommandation :** Utilisez OpenSSL 3.0+ pour les nouveaux projets, ou au minimum 1.1.1 (LTS).

### Composants d'OpenSSL

```
OpenSSL
├─ libssl    → Protocoles SSL/TLS
├─ libcrypto → Algorithmes cryptographiques
└─ openssl   → Outil en ligne de commande
```

**Fichiers principaux :**

**Windows :**
- `libssl-3.dll` (ou `ssleay32.dll` pour 1.1.1)
- `libcrypto-3.dll` (ou `libeay32.dll` pour 1.1.1)

**Linux :**
- `libssl.so.3` (ou `libssl.so.1.1`)
- `libcrypto.so.3` (ou `libcrypto.so.1.1`)

## Certificats auto-signés vs certificats CA

### Certificats auto-signés

**Avantages :**
- ✅ Gratuit
- ✅ Rapide à créer
- ✅ Parfait pour le développement et les tests
- ✅ Bon pour les réseaux internes

**Inconvénients :**
- ❌ Avertissement de sécurité dans les navigateurs
- ❌ Pas de chaîne de confiance
- ❌ Ne convient PAS pour la production publique

**Utilisation :**
```bash
# Création d'un certificat auto-signé
openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -nodes
```

### Certificats d'autorité de certification

**Avantages :**
- ✅ Reconnus par tous les navigateurs
- ✅ Pas d'avertissement de sécurité
- ✅ Chaîne de confiance établie
- ✅ Convient pour la production

**Inconvénients :**
- ❌ Coût (sauf Let's Encrypt)
- ❌ Processus de validation
- ❌ Renouvellement périodique

**Let's Encrypt (gratuit) :**
```bash
# Installation de certbot
sudo apt install certbot

# Obtention d'un certificat
sudo certbot certonly --standalone -d example.com
```

## Formats de fichiers

SSL/TLS utilise plusieurs formats de fichiers :

### Formats de clés et certificats

| Format | Extension | Description | Usage |
|--------|-----------|-------------|-------|
| **PEM** | .pem, .crt, .key | Base64, lisible | Le plus courant |
| **DER** | .der, .cer | Binaire | Windows |
| **PKCS#12** | .p12, .pfx | Archive (cert + clé) | Windows, échange |
| **PKCS#7** | .p7b, .p7c | Certificats seulement | Chaînes |

### Fichier PEM (le plus courant)

```
-----BEGIN CERTIFICATE-----
MIIDXTCCAkWgAwIBAgIJAKJ5...
(contenu encodé en Base64)
...KJ5vQ==
-----END CERTIFICATE-----

-----BEGIN PRIVATE KEY-----
MIIEvQIBADANBgkqhkiG9w0...
(clé privée encodée)
...9w0BAQ==
-----END PRIVATE KEY-----
```

**Avantages du PEM :**
- Lisible en texte (Base64)
- Peut contenir plusieurs certificats
- Compatible avec OpenSSL et la plupart des outils

### Conversion entre formats

```bash
# PEM vers DER
openssl x509 -in cert.pem -outform DER -out cert.der

# DER vers PEM
openssl x509 -in cert.der -inform DER -out cert.pem

# PEM vers PKCS#12
openssl pkcs12 -export -in cert.pem -inkey key.pem -out cert.p12

# PKCS#12 vers PEM
openssl pkcs12 -in cert.p12 -out cert.pem -nodes
```

## Protocoles et cipher suites

### Versions de protocole

```
Historique et recommandations :

SSL 2.0 (1995)  → ❌ Obsolète, dangereux
SSL 3.0 (1996)  → ❌ Vulnérable (POODLE)
TLS 1.0 (1999)  → ❌ Déprécié
TLS 1.1 (2006)  → ⚠️ À éviter
TLS 1.2 (2008)  → ✅ Recommandé (minimum)
TLS 1.3 (2018)  → ✅ Recommandé (idéal)
```

**Configuration recommandée :**
```
Minimum : TLS 1.2
Préféré : TLS 1.3
```

### Cipher suites

Une cipher suite est une combinaison d'algorithmes utilisés pour sécuriser la connexion :

```
TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256
│   │     │       │       │   │
│   │     │       │       │   └─ Hash (SHA-256)
│   │     │       │       └───── Mode (GCM)
│   │     │       └───────────── Chiffrement (AES-128)
│   │     └───────────────────── Signature (RSA)
│   └─────────────────────────── Échange de clés (ECDHE)
└─────────────────────────────── Protocole (TLS)
```

**Cipher suites recommandées (2024) :**

Pour **TLS 1.3** :
```
TLS_AES_256_GCM_SHA384
TLS_AES_128_GCM_SHA256
TLS_CHACHA20_POLY1305_SHA256
```

Pour **TLS 1.2** :
```
TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384
TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256
TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256
```

**À éviter :**
- Tout ce qui contient `RC4`, `MD5`, `DES`, `3DES`
- Cipher suites sans Forward Secrecy (sans `DHE` ou `ECDHE`)
- Export cipher suites

## Vérification de certificat

### Validation côté client

Lorsqu'un client se connecte, il doit vérifier :

1. **Validité temporelle**
   - Le certificat n'est pas expiré
   - Il est déjà valide

2. **Autorité de certification**
   - Le certificat est signé par une CA de confiance
   - La chaîne de certificats est valide

3. **Nom de domaine**
   - Le Common Name (CN) ou Subject Alternative Name (SAN) correspond
   - Wildcards : `*.example.com` correspond à `www.example.com`

4. **Révocation**
   - Le certificat n'a pas été révoqué (CRL ou OCSP)

### Exemple de vérification manuelle

```bash
# Voir les détails d'un certificat
openssl x509 -in cert.pem -text -noout

# Vérifier la validité d'un certificat
openssl verify -CAfile ca-bundle.crt cert.pem

# Tester une connexion SSL/TLS
openssl s_client -connect example.com:443 -showcerts
```

## Sécurité et bonnes pratiques

### Liste de contrôle de sécurité

✅ **Faire :**

1. Utiliser TLS 1.2 minimum (TLS 1.3 si possible)
2. Désactiver SSL 2.0, SSL 3.0, TLS 1.0, TLS 1.1
3. Utiliser des cipher suites fortes (AES-GCM, ChaCha20)
4. Activer Forward Secrecy (ECDHE)
5. Utiliser des certificats de 2048 bits minimum (RSA) ou 256 bits (ECDSA)
6. Renouveler les certificats avant expiration
7. Implémenter HSTS (HTTP Strict Transport Security)
8. Valider tous les certificats côté client
9. Stocker les clés privées de manière sécurisée
10. Utiliser des permissions restrictives sur les fichiers de clés

❌ **Ne pas faire :**

1. Utiliser SSL 3.0 ou TLS 1.0
2. Accepter des certificats auto-signés en production
3. Désactiver la vérification de certificat
4. Utiliser des cipher suites faibles (RC4, DES, MD5)
5. Partager ou exposer les clés privées
6. Laisser les certificats expirer
7. Utiliser des clés RSA < 2048 bits
8. Ignorer les avertissements de sécurité

### Permissions sur les fichiers

```bash
# Linux/Ubuntu
chmod 600 private-key.pem    # Lecture seule par le propriétaire
chmod 644 certificate.pem     # Lecture par tous

# Propriétaire
chown www-data:www-data *.pem
```

```powershell
# Windows
icacls private-key.pem /inheritance:r /grant:r "%USERNAME%:F"
```

## Outils de test et validation

### Tests en ligne

1. **SSL Labs Server Test**
   - https://www.ssllabs.com/ssltest/
   - Analyse complète de la configuration SSL/TLS
   - Note de A+ à F

2. **Security Headers**
   - https://securityheaders.com/
   - Vérification des en-têtes de sécurité HTTP

3. **Certificate Transparency**
   - https://crt.sh/
   - Recherche de certificats émis pour votre domaine

### Outils en ligne de commande

```bash
# nmap (scan SSL/TLS)
nmap --script ssl-enum-ciphers -p 443 example.com

# testssl.sh (audit complet)
./testssl.sh https://example.com

# sslyze (analyse Python)
sslyze --regular example.com:443
```

## Performance SSL/TLS

### Impact sur les performances

SSL/TLS ajoute un surcoût :

```
Sans SSL/TLS :
├─ Connexion TCP : ~50ms
└─ Requête HTTP : ~10ms
Total : ~60ms

Avec SSL/TLS :
├─ Connexion TCP : ~50ms
├─ Handshake TLS : ~100-200ms (première fois)
└─ Requête HTTP : ~10ms
Total : ~160-260ms (première connexion)
```

**Optimisations :**

1. **Session Resumption**
   - Réutiliser les sessions TLS
   - Économie de 1 RTT (Round-Trip Time)

2. **TLS 1.3**
   - Handshake plus rapide (1-RTT)
   - 0-RTT possible (early data)

3. **Certificate Pinning**
   - Éviter la validation complète de la chaîne

4. **OCSP Stapling**
   - Le serveur fournit la preuve de non-révocation
   - Évite une requête OCSP supplémentaire

5. **HTTP/2**
   - Multiplexage sur une seule connexion TLS

## Cas d'usage courants

### 1. Serveur web HTTPS

```
Client (Navigateur)
    │
    ├─ HTTPS (port 443)
    ↓
Serveur Web + TLS
    │
    └─ Application FreePascal
```

### 2. API REST sécurisée

```
Application Mobile
    │
    ├─ HTTPS + API Key
    ↓
API Server + TLS
    │
    └─ Base de données
```

### 3. Communication client-serveur

```
Client Desktop
    │
    ├─ TLS Socket
    ↓
Serveur Backend
```

### 4. Email sécurisé

```
Client Email
    │
    ├─ SMTPS (port 465) ou STARTTLS (port 587)
    ↓
Serveur SMTP + TLS
```

## Structure de cette section

Cette section couvre tous les aspects de SSL/TLS avec FreePascal :

**10.5.1** : OpenSSL sur Windows - Installation et configuration  
**10.5.2** : OpenSSL sur Ubuntu - Installation et configuration  
**10.5.3** : Génération de certificats - Auto-signés et Let's Encrypt  
**10.5.4** : Configuration Synapse SSL - Utilisation avec Synapse  
**10.5.5** : Configuration Indy SSL - Utilisation avec Indy  
**10.5.6** : Validation de certificats - Vérification et sécurité  
**10.5.7** : Pinning de certificats - Sécurité renforcée  
**10.5.8** : Debugging SSL/TLS - Résolution de problèmes  

## Prérequis

Avant de commencer cette section, vous devriez :

- ✅ Connaître les bases de la programmation réseau (chapitre 10.1-10.4)
- ✅ Comprendre HTTP/HTTPS
- ✅ Savoir utiliser la ligne de commande (terminal)
- ✅ Avoir des notions basiques de cryptographie
- ✅ Être à l'aise avec la compilation de programmes

## Avertissements de sécurité

⚠️ **Important** :

- La sécurité est un sujet complexe et en constante évolution
- Ce tutoriel couvre les bases, mais consultez toujours les recommandations actuelles
- Pour des applications critiques, faites appel à des experts en sécurité
- Ne jamais désactiver les vérifications de sécurité en production
- Gardez vos bibliothèques SSL/TLS à jour

🔒 **Règle d'or** : En cas de doute sur la sécurité, soyez restrictif plutôt que permissif !

---

Maintenant que vous comprenez les fondamentaux de SSL/TLS, voyons comment l'installer et le configurer sur vos systèmes.

⏭️ [OpenSSL sur Windows](/10-programmation-reseau-avancee/05.1-openssl-windows.md)
