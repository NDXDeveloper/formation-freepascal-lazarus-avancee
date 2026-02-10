🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10. Programmation Réseau Avancée

## Introduction

La programmation réseau est un domaine essentiel du développement logiciel moderne. Pratiquement toutes les applications professionnelles ont besoin de communiquer sur un réseau : applications web, APIs REST, services cloud, jeux multijoueurs, applications IoT, systèmes distribués, etc.

FreePascal et Lazarus offrent des outils puissants et flexibles pour développer des applications réseau robustes et performantes, que ce soit sur Windows, Linux, macOS ou d'autres plateformes.

## Pourquoi la programmation réseau ?

### Applications concrètes

La programmation réseau permet de créer :

- **Applications client-serveur** : Chat, messagerie instantanée, systèmes de gestion
- **Services web et APIs REST** : Microservices, backends d'applications mobiles
- **Applications distribuées** : Calcul distribué, bases de données réparties
- **Jeux multijoueurs** : Communication temps réel entre joueurs
- **IoT et domotique** : Capteurs, actionneurs, systèmes de surveillance
- **Systèmes de fichiers distribués** : Partage et synchronisation de fichiers
- **Applications de monitoring** : Surveillance de serveurs, collecte de métriques
- **Outils d'administration réseau** : Scan de ports, diagnostic réseau

### Avantages de FreePascal pour le réseau

- ✅ **Multi-plateforme** : Le même code fonctionne sur Windows et Linux
- ✅ **Performance** : Code natif compilé, pas d'interpréteur
- ✅ **Bibliothèques riches** : Synapse, Indy, sockets natifs
- ✅ **Stabilité** : Gestion robuste de la mémoire
- ✅ **Portabilité** : Du Raspberry Pi aux serveurs d'entreprise
- ✅ **Facilité** : Syntaxe claire, composants visuels disponibles

## Concepts fondamentaux

### Le modèle OSI et TCP/IP

Pour comprendre la programmation réseau, il est important de connaître les couches du réseau :

```
┌─────────────────────────────────────────────────┐
│  7. Application  │ HTTP, FTP, SMTP, DNS         │ ← Nous programmons ici
├─────────────────────────────────────────────────┤
│  6. Présentation │ SSL/TLS, Encodage            │
├─────────────────────────────────────────────────┤
│  5. Session      │ Gestion des connexions       │
├─────────────────────────────────────────────────┤
│  4. Transport    │ TCP, UDP                     │ ← Et parfois ici
├─────────────────────────────────────────────────┤
│  3. Réseau       │ IP, ICMP, Routage            │
├─────────────────────────────────────────────────┤
│  2. Liaison      │ Ethernet, WiFi               │
├─────────────────────────────────────────────────┤
│  1. Physique     │ Câbles, ondes radio          │
└─────────────────────────────────────────────────┘
```

En tant que développeurs, nous travaillons principalement aux **couches 4 (Transport)** et **7 (Application)**.

### Architecture client-serveur

Le modèle le plus courant en programmation réseau :

```
     Client                          Serveur
       │                                │
       │────── 1. Connexion ───────────>│
       │                                │
       │<───── 2. Acceptation ──────────│
       │                                │
       │────── 3. Requête ─────────────>│
       │                                │
       │                         [Traitement]
       │                                │
       │<───── 4. Réponse ──────────────│
       │                                │
       │────── 5. Fermeture ───────────>│
       │                                │
```

**Caractéristiques :**
- Le **serveur** attend passivement les connexions
- Le **client** initie activement la communication
- Peut être **1-à-1** (un client, un serveur) ou **N-à-1** (plusieurs clients, un serveur)

### Protocoles de transport : TCP vs UDP

#### TCP (Transmission Control Protocol)

**Analogie** : Comme un appel téléphonique

```
Caractéristiques :
├─ Orienté connexion (handshake)
├─ Fiable (garantie de livraison)
├─ Ordre préservé
├─ Contrôle de flux
├─ Contrôle d'erreur
└─ Plus lent qu'UDP
```

**Utilisation :**
- Navigation web (HTTP/HTTPS)
- Emails (SMTP, POP3, IMAP)
- Transfert de fichiers (FTP)
- SSH, Telnet
- Bases de données

**Exemple de connexion TCP :**
```
Client          Serveur
  │                │
  │─── SYN ───────>│  (Demande de connexion)
  │<── SYN+ACK ────│  (Acceptation)
  │─── ACK ───────>│  (Confirmation)
  │                │
  [Connexion établie]
```

#### UDP (User Datagram Protocol)

**Analogie** : Comme envoyer une lettre par la poste

```
Caractéristiques :
├─ Sans connexion
├─ Non fiable (pas de garantie)
├─ Pas d'ordre garanti
├─ Pas de contrôle de flux
├─ Très rapide
└─ Léger (peu d'overhead)
```

**Utilisation :**
- Streaming audio/vidéo
- Jeux en ligne
- DNS (requêtes DNS)
- VoIP (téléphonie IP)
- Broadcast/Multicast

**Exemple UDP :**
```
Client          Serveur
  │                │
  │─── Paquet 1 ──>│
  │─── Paquet 2 ──>│
  │─── Paquet 3 ──>│
  │                │
(Envoi direct, pas de connexion)
```

### Adresses et ports

#### Adresses IP

Une adresse IP identifie un appareil sur le réseau :

**IPv4** (la plus courante) :
```
192.168.1.100
│   │   │  │
└───┴───┴──┴─── 4 octets (32 bits)

Exemples :
- 127.0.0.1    → localhost (boucle locale)
- 192.168.x.x  → Réseaux privés
- 10.x.x.x     → Réseaux privés
- 0.0.0.0      → Toutes les interfaces
```

**IPv6** (nouvelle génération) :
```
2001:0db8:85a3:0000:0000:8a2e:0370:7334

Exemples :
- ::1          → localhost
- fe80::       → Link-local
```

#### Ports

Un port identifie une application sur un appareil :

```
Numéro de port : 0 à 65535

├─ 0 à 1023      → Ports bien connus (nécessitent privilèges)
│  ├─ 20, 21     → FTP
│  ├─ 22         → SSH
│  ├─ 25         → SMTP (email sortant)
│  ├─ 53         → DNS
│  ├─ 80         → HTTP
│  ├─ 110        → POP3 (email entrant)
│  ├─ 143        → IMAP
│  └─ 443        → HTTPS
│
├─ 1024 à 49151  → Ports enregistrés
│  ├─ 3000       → Node.js (convention)
│  ├─ 3306       → MySQL
│  ├─ 5432       → PostgreSQL
│  ├─ 6379       → Redis
│  └─ 8080       → HTTP alternatif
│
└─ 49152 à 65535 → Ports dynamiques/privés
```

**Exemple complet :**
```
http://192.168.1.100:8080/api/users
│      │              │    │
│      │              │    └── Chemin (application)
│      │              └─────── Port
│      └────────────────────── Adresse IP
└───────────────────────────── Protocole
```

### Sockets : l'interface de programmation

Un **socket** est le point de terminaison d'une communication réseau. C'est l'équivalent d'une "prise" qui permet de se brancher sur le réseau.

```
Application
     │
     ▼
  Socket ←─── Interface de programmation
     │
     ▼
 Protocole (TCP/UDP)
     │
     ▼
   Réseau
```

**Types de sockets :**
- **SOCK_STREAM** : Sockets TCP (flux continu)
- **SOCK_DGRAM** : Sockets UDP (datagrammes)
- **SOCK_RAW** : Sockets bruts (accès bas niveau)

## Outils et bibliothèques disponibles

FreePascal offre plusieurs options pour la programmation réseau, du niveau le plus bas au plus haut :

### 1. Sockets natifs (bas niveau)

```pascal
uses
  Sockets;  // Accès direct aux sockets système
```

**Avantages :**
- ✅ Contrôle total
- ✅ Performance maximale
- ✅ Pas de dépendances

**Inconvénients :**
- ❌ Plus complexe
- ❌ Gestion manuelle des erreurs
- ❌ Code plus verbeux

**Quand l'utiliser :**
- Protocoles personnalisés
- Optimisations extrêmes
- Apprentissage en profondeur

### 2. Synapse (niveau intermédiaire)

```pascal
uses
  blcksock, httpsend, smtpsend;
```

**Avantages :**
- ✅ Simple à utiliser
- ✅ Léger et portable
- ✅ Bien documenté
- ✅ Support de nombreux protocoles

**Inconvénients :**
- ❌ Pas de composants visuels
- ❌ API procédurale

**Quand l'utiliser :**
- Applications console
- Services en arrière-plan
- Scripts et outils

### 3. Indy (haut niveau)

```pascal
uses
  IdHTTP, IdTCPServer, IdSMTP;
```

**Avantages :**
- ✅ Composants visuels
- ✅ Orienté objet
- ✅ Riche en fonctionnalités
- ✅ Événements et threads intégrés

**Inconvénients :**
- ❌ Plus lourd
- ❌ Courbe d'apprentissage

**Quand l'utiliser :**
- Applications graphiques
- Développement rapide
- Serveurs complexes

### Tableau comparatif

| Critère | Sockets natifs | Synapse | Indy |
|---------|---------------|---------|------|
| **Complexité** | Élevée | Moyenne | Faible |
| **Contrôle** | Total | Bon | Moyen |
| **Performance** | Maximale | Excellente | Bonne |
| **Facilité** | Difficile | Facile | Très facile |
| **GUI** | ❌ | ❌ | ✅ |
| **Poids** | Minimal | Léger | Lourd |
| **Courbe d'apprentissage** | Raide | Douce | Très douce |

## Patterns de conception réseau

### 1. Client simple / Serveur simple

Le pattern le plus basique :

```
Client ─────> Serveur
```

- Un client se connecte
- Le serveur traite la requête
- La connexion se ferme

**Usage :** Requêtes HTTP simples, scripts

### 2. Serveur multi-client séquentiel

```
Client 1 ──┐
           ├──> Serveur (traite un à la fois)
Client 2 ──┘
```

- Le serveur traite les clients un par un
- Les autres attendent dans une queue

**Usage :** Serveurs peu sollicités

### 3. Serveur multi-thread

```
Client 1 ──> Thread 1 ─┐
                       ├──> Serveur
Client 2 ──> Thread 2 ─┘
```

- Un thread par client
- Traitement parallèle

**Usage :** Serveurs moyennement sollicités

### 4. Pool de threads

```
Client 1 ──┐            ┌─ Worker 1
           │            │
Client 2 ──┼─> Queue ───┼─ Worker 2
           │            │
Client 3 ──┘            └─ Worker 3
```

- Nombre fixe de workers
- Réutilisation des threads

**Usage :** Serveurs très sollicités

### 5. Asynchrone / Non-bloquant

```
Client 1 ─┐
          ├─> Event Loop ─> Callbacks
Client 2 ─┘
```

- Un seul thread
- Multiplexage I/O

**Usage :** Serveurs haute performance (Node.js style)

## Protocoles applicatifs courants

### HTTP (HyperText Transfer Protocol)

Le protocole du web :

```http
GET /api/users HTTP/1.1
Host: example.com
Accept: application/json

HTTP/1.1 200 OK
Content-Type: application/json

{"users": [...]}
```

**Caractéristiques :**
- Sans état (stateless)
- Requête/réponse
- Texte (lisible)
- Port 80 (HTTP) ou 443 (HTTPS)

### FTP (File Transfer Protocol)

Transfert de fichiers :

```
Control Connection (port 21) : Commandes
Data Connection (port 20)    : Transfert
```

### SMTP / POP3 / IMAP

Emails :
- **SMTP** (port 25, 587) : Envoi
- **POP3** (port 110) : Réception (téléchargement)
- **IMAP** (port 143) : Réception (synchronisation)

### DNS (Domain Name System)

Résolution de noms :

```
example.com ──[DNS]──> 93.184.216.34
```

### WebSocket

Communication bidirectionnelle en temps réel :

```
Client <══════════> Serveur
       (connexion persistante)
```

## Sécurité réseau

### SSL/TLS

Chiffrement des communications :

```
HTTP  + SSL/TLS = HTTPS
SMTP  + SSL/TLS = SMTPS
FTP   + SSL/TLS = FTPS
```

**Concepts clés :**
- Chiffrement des données
- Authentification du serveur
- Intégrité des messages
- Certificats X.509

### Bonnes pratiques de sécurité

1. **Toujours valider les entrées**
   ```pascal
   if Length(Input) > MAX_SIZE then
     raise Exception.Create('Input too large');
   ```

2. **Utiliser SSL/TLS pour les données sensibles**
   ```pascal
   HTTP.IOHandler := SSLHandler;
   ```

3. **Ne jamais stocker de mots de passe en clair**
   ```pascal
   HashedPassword := HashPassword(Password);
   ```

4. **Implémenter des timeouts**
   ```pascal
   Socket.ReceiveTimeout := 30000; // 30 secondes
   ```

5. **Filtrer les adresses IP si nécessaire**
   ```pascal
   if not IsAllowedIP(ClientIP) then
     Disconnect;
   ```

## Débogage réseau

### Outils essentiels

**1. Wireshark**
- Capture et analyse de paquets
- Visualisation du trafic réseau
- Décodage de protocoles

**2. tcpdump (Linux)**
```bash
sudo tcpdump -i eth0 port 8080
```

**3. netstat / ss**
```bash
# Voir les connexions actives
netstat -an | grep 8080
ss -an | grep 8080
```

**4. telnet / nc (netcat)**
```bash
# Tester une connexion TCP
telnet localhost 8080
nc -v localhost 8080
```

**5. curl**
```bash
# Tester une API HTTP
curl -v http://localhost:8080/api/users
```

### Problèmes courants

| Problème | Cause probable | Solution |
|----------|---------------|----------|
| Connexion refusée | Serveur non démarré ou firewall | Vérifier le serveur et le firewall |
| Timeout | Réseau lent ou serveur bloqué | Augmenter le timeout, vérifier le serveur |
| Broken pipe | Client déconnecté | Gérer l'exception |
| Address in use | Port déjà utilisé | Changer de port ou arrêter l'autre process |
| Permission denied | Port < 1024 sans privilèges | Utiliser sudo ou port > 1024 |

## Performance réseau

### Optimisations

1. **Utiliser des buffers appropriés**
   ```pascal
   const BUFFER_SIZE = 8192; // 8 KB
   ```

2. **Réutiliser les connexions (Keep-Alive)**
   ```http
   Connection: keep-alive
   ```

3. **Compresser les données**
   ```http
   Content-Encoding: gzip
   ```

4. **Paralléliser avec des threads**
   ```pascal
   ThreadPool.AddTask(ClientSocket);
   ```

5. **Utiliser des protocoles binaires pour la performance**
   - Moins verbeux que le texte
   - Plus rapide à parser

### Métriques à surveiller

- **Latence** : Temps de réponse
- **Débit** : Quantité de données transférées
- **Connexions simultanées** : Charge du serveur
- **Taux d'erreur** : Fiabilité
- **Utilisation CPU/Mémoire** : Efficacité

## Différences Windows/Linux

### Chemins de bibliothèques

**Windows :**
```pascal
{$IFDEF WINDOWS}
  LibSSL = 'ssleay32.dll';
  LibCrypto = 'libeay32.dll';
{$ENDIF}
```

**Linux :**
```pascal
{$IFDEF UNIX}
  LibSSL = 'libssl.so.1.1';
  LibCrypto = 'libcrypto.so.1.1';
{$ENDIF}
```

### Permissions

**Windows :** Pas de restriction sur les ports

**Linux :** Ports < 1024 nécessitent root
```bash
sudo ./myserver
# ou
sudo setcap 'cap_net_bind_service=+ep' ./myserver
```

### Firewall

**Windows :**
```cmd
netsh advfirewall firewall add rule name="MyApp" dir=in action=allow protocol=TCP localport=8080
```

**Linux :**
```bash
sudo ufw allow 8080/tcp
sudo firewall-cmd --add-port=8080/tcp --permanent
```

## Structure du chapitre

Ce chapitre couvre tous les aspects de la programmation réseau avec FreePascal/Lazarus :

**10.1** : Synapse - Bibliothèque légère et efficace  
**10.2** : Indy - Framework complet avec composants visuels  
**10.3** : Protocoles TCP/UDP bas niveau - Contrôle total  
**10.4** : Serveurs HTTP/HTTPS personnalisés - Web backends  
**10.5** : Configuration SSL/TLS - Sécurité des communications  
**10.6** : Clients et serveurs WebSocket - Temps réel  
**10.7** : Protocoles binaires personnalisés - Performance maximale  
**10.8** : RPC et IPC - Communication inter-processus  
**10.9** : mORMot - Framework SOA/REST haute performance  
**10.10** : gRPC et Protocol Buffers - APIs modernes  
**10.11** : P2P et protocoles décentralisés - Systèmes distribués  
**10.12** : Configuration réseau et firewall - Déploiement  

## Prérequis

Avant de commencer ce chapitre, vous devriez être à l'aise avec :

- ✅ Programmation orientée objet en Pascal
- ✅ Gestion des exceptions
- ✅ Threads et multithreading (bases)
- ✅ Manipulation de fichiers et flux (streams)
- ✅ Notions de base sur les réseaux (IP, ports)

## Ressources complémentaires

### Documentation officielle
- **FreePascal Sockets** : https://www.freepascal.org/docs-html/rtl/sockets/
- **Synapse** : http://www.ararat.cz/synapse/
- **Indy** : http://www.indyproject.org/

### Spécifications (RFCs)
- **RFC 793** : TCP
- **RFC 768** : UDP
- **RFC 2616** : HTTP/1.1
- **RFC 7540** : HTTP/2
- **RFC 6455** : WebSocket

### Livres recommandés
- *UNIX Network Programming* - W. Richard Stevens
- *TCP/IP Illustrated* - W. Richard Stevens
- *Beej's Guide to Network Programming*

## Conventions de ce chapitre

### Code

Tous les exemples sont testés sur :
- ✅ Windows 10/11
- ✅ Ubuntu 20.04/22.04 LTS
- ✅ FreePascal 3.2.2+
- ✅ Lazarus 2.2.0+

### Notation

```pascal
// Commentaire explicatif
WriteLn('Sortie console');  // Commentaire en ligne

{ Bloc de commentaire
  pour plusieurs lignes }
```

### Symboles utilisés

- 💡 **Conseil** : Astuce pratique
- ⚠️ **Attention** : Point important
- 🔒 **Sécurité** : Aspect sécurité
- 🚀 **Performance** : Optimisation
- 🐛 **Débogage** : Aide au débogage
- 📝 **Note** : Information complémentaire

---

Maintenant que vous avez une vue d'ensemble de la programmation réseau avec FreePascal, plongeons dans les détails avec les bibliothèques et techniques spécifiques !

⏭️ [Synapse - Bibliothèque réseau complète](/10-programmation-reseau-avancee/01-synapse-bibliotheque-reseau-complete.md)
