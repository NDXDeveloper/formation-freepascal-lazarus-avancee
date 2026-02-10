🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9. Programmation Web avec FreePascal

## Introduction générale

La programmation web avec FreePascal représente une approche unique dans le développement d'applications web modernes. Alors que la plupart des développeurs se tournent vers des langages interprétés comme PHP, Python ou Node.js, FreePascal offre une alternative compilée, performante et multi-plateforme qui mérite d'être explorée.

### Pourquoi FreePascal pour le web ?

#### Avantages uniques

**1. Performance exceptionnelle**

FreePascal compile en code natif, ce qui signifie :
- Aucun interpréteur à exécuter à chaque requête
- Consommation mémoire minimale
- Temps de réponse ultra-rapides
- Idéal pour les applications à forte charge

**Comparaison typique** :
- Application PHP : 50-200ms par requête
- Application Node.js : 20-100ms par requête
- Application FreePascal : 1-10ms par requête

**2. Stabilité et fiabilité**

Le typage fort de Pascal élimine de nombreuses classes d'erreurs :
- Pas d'erreurs de types à l'exécution
- Détection des erreurs à la compilation
- Code plus maintenable et prévisible
- Moins de bugs en production

**3. Sécurité native**

FreePascal offre plusieurs avantages sécuritaires :
- Pas d'injection de code dynamique
- Gestion mémoire contrôlée (pas de buffer overflow facile)
- Typage strict empêchant les conversions dangereuses
- Binaires compilés difficiles à analyser/modifier

**4. Multi-plateforme réel**

Un seul code source pour :
- Windows (32/64 bits)
- Linux (toutes distributions)
- macOS
- BSD
- Même ARM pour serveurs embarqués

**5. Déploiement simplifié**

- Un seul exécutable autonome (pas de runtime à installer)
- Pas de dépendances complexes
- Taille d'exécutable réduite (quelques Mo)
- Installation instantanée

#### Cas d'usage idéaux

FreePascal excelle particulièrement dans :

1. **API REST haute performance** - Microservices nécessitant des temps de réponse minimaux
2. **Applications d'entreprise** - Systèmes robustes avec forte charge
3. **Services backend** - Traitement de données, calculs complexes
4. **Applications temps réel** - WebSockets, streaming, notifications
5. **Prototypage rapide** - Développement et déploiement accélérés

### L'écosystème web FreePascal

FreePascal dispose d'un écosystème web mature et diversifié :

#### Frameworks et bibliothèques

**1. fpWeb** (Framework officiel intégré)
- Inclus dans FreePascal
- Aucune dépendance externe
- Modes CGI, FastCGI et standalone
- Idéal pour débuter

**2. Brook Framework**
- Framework moderne orienté REST
- Routage élégant et intuitif
- Support JSON natif
- Parfait pour les API

**3. mORMot**
- Framework complet SOA/REST
- ORM intégré haute performance
- Authentification et sécurité avancées
- Pour applications d'entreprise complexes

**4. Pas2JS**
- Transpileur Pascal vers JavaScript
- Code Pascal côté client et serveur
- Réutilisation de code maximale
- Alternative à TypeScript

**5. WebAssembly**
- FreePascal compile en WASM
- Performance native dans le navigateur
- Futur du web

#### Outils et utilitaires

**Serveurs web** :
- Apache avec mod_fcgid
- Nginx avec FastCGI
- IIS (Windows)
- Serveur HTTP intégré fpWeb

**Bases de données** :
- PostgreSQL
- MySQL/MariaDB
- SQLite
- Firebird
- MongoDB

**Outils de développement** :
- Lazarus IDE (débogage web)
- lazbuild (automatisation)
- fpcdebug (débogage distant)

### Architecture des applications web FreePascal

#### Les trois architectures principales

**1. Architecture CGI classique**

```
Navigateur → Serveur Web (Apache/Nginx) → Application CGI FreePascal
```

**Fonctionnement** :
- Le serveur web lance un nouveau processus pour chaque requête
- L'application lit les paramètres depuis l'environnement
- Elle génère une réponse HTTP
- Le processus se termine

**Avantages** :
- Configuration simple
- Isolation complète entre requêtes
- Pas de problème de mémoire

**Inconvénients** :
- Lent (création de processus)
- Non adapté à forte charge

**2. Architecture FastCGI (recommandée)**

```
Navigateur → Serveur Web → Socket FastCGI → Application FreePascal persistante
```

**Fonctionnement** :
- Un processus FreePascal démarre et reste en mémoire
- Le serveur web communique via un socket
- Le même processus traite toutes les requêtes
- Performance maximale

**Avantages** :
- Très performant
- Connexions BDD persistantes
- Cache en mémoire possible
- Standard industriel

**Inconvénients** :
- Configuration plus complexe
- Nécessite gestion des ressources partagées

**3. Architecture standalone (serveur HTTP intégré)**

```
Navigateur → Application FreePascal (serveur HTTP intégré)
```

**Fonctionnement** :
- FreePascal inclut son propre serveur HTTP
- Écoute directement sur un port (ex: 8080)
- Aucune dépendance externe
- Parfait pour microservices

**Avantages** :
- Déploiement ultra-simple
- Idéal pour développement
- Parfait pour conteneurs Docker
- Contrôle total

**Inconvénients** :
- Moins de fonctionnalités qu'Apache/Nginx
- Nécessite reverse proxy en production

### Protocole HTTP et concepts web

#### Comprendre HTTP

HTTP (HyperText Transfer Protocol) est le protocole de communication du web.

**Une requête HTTP contient** :
- Une **méthode** (GET, POST, PUT, DELETE...)
- Une **URL** (chemin vers la ressource)
- Des **headers** (informations sur la requête)
- Un **corps** optionnel (données POST, JSON...)

**Une réponse HTTP contient** :
- Un **code de statut** (200 OK, 404 Not Found...)
- Des **headers** (type de contenu, cookies...)
- Un **corps** (HTML, JSON, fichier...)

**Exemple de requête GET** :
```
GET /users/123 HTTP/1.1
Host: api.example.com
Accept: application/json
Authorization: Bearer token123
```

**Exemple de réponse** :
```
HTTP/1.1 200 OK
Content-Type: application/json
Content-Length: 87

{"id":123,"name":"Alice","email":"alice@example.com"}
```

#### Les méthodes HTTP principales

| Méthode | Usage | Idempotent | Safe |
|---------|-------|------------|------|
| GET | Lire des données | ✓ | ✓ |
| POST | Créer une ressource | ✗ | ✗ |
| PUT | Modifier/remplacer | ✓ | ✗ |
| PATCH | Modifier partiellement | ✗ | ✗ |
| DELETE | Supprimer | ✓ | ✗ |
| HEAD | Métadonnées uniquement | ✓ | ✓ |
| OPTIONS | Méthodes disponibles | ✓ | ✓ |

**Idempotent** : Peut être appelé plusieurs fois avec le même résultat  
**Safe** : Ne modifie pas l'état du serveur  

#### Codes de statut HTTP essentiels

**2xx - Succès**
- `200 OK` : Requête réussie
- `201 Created` : Ressource créée
- `204 No Content` : Succès sans contenu

**3xx - Redirections**
- `301 Moved Permanently` : Redirection permanente
- `302 Found` : Redirection temporaire
- `304 Not Modified` : Cache toujours valide

**4xx - Erreurs client**
- `400 Bad Request` : Requête mal formée
- `401 Unauthorized` : Authentification requise
- `403 Forbidden` : Accès refusé
- `404 Not Found` : Ressource inexistante
- `422 Unprocessable Entity` : Données invalides

**5xx - Erreurs serveur**
- `500 Internal Server Error` : Erreur serveur
- `502 Bad Gateway` : Erreur de passerelle
- `503 Service Unavailable` : Service indisponible

### Concepts fondamentaux du web

#### Sessions et état

HTTP est **stateless** (sans état), chaque requête est indépendante.

**Solutions pour maintenir l'état** :

**1. Cookies**
- Petit fichier texte stocké côté client
- Envoyé automatiquement à chaque requête
- Utilisé pour l'identification

**2. Sessions serveur**
- ID de session dans un cookie
- Données stockées côté serveur
- Plus sécurisé

**3. Tokens JWT**
- JSON Web Token
- Contient toutes les infos nécessaires
- Moderne et scalable

#### Sécurité web de base

**1. HTTPS (SSL/TLS)**
- Chiffrement des communications
- Obligatoire pour données sensibles
- Certificats Let's Encrypt gratuits

**2. CORS (Cross-Origin Resource Sharing)**
- Contrôle les accès depuis autres domaines
- Headers spécifiques à configurer
- Essentiel pour les API publiques

**3. Protection XSS (Cross-Site Scripting)**
- Échapper le HTML dans les sorties
- Ne jamais faire confiance aux entrées utilisateur
- Validation stricte

**4. Protection CSRF (Cross-Site Request Forgery)**
- Tokens de vérification
- Vérifier l'origine des requêtes
- Important pour les formulaires

**5. Injection SQL**
- Toujours utiliser des requêtes préparées
- Ne jamais concaténer des inputs utilisateur
- Validation des données

### Différences multi-plateformes (Windows/Ubuntu)

#### Configuration serveur web

**Windows** :
- IIS (Internet Information Services) natif
- Apache via XAMPP ou installation manuelle
- Nginx pour Windows

**Ubuntu/Linux** :
- Apache (apt install apache2)
- Nginx (apt install nginx)
- Systemd pour gestion des services

#### Chemins et conventions

**Windows** :
```
C:\inetpub\wwwroot\
C:\Program Files\MyApp\
Séparateur : \
```

**Linux** :
```
/var/www/html/
/opt/myapp/
Séparateur : /
```

**Code portable** :
```pascal
uses
  sysutils;

function GetWebRoot: String;
begin
  {$IFDEF WINDOWS}
  Result := 'C:\inetpub\wwwroot\';
  {$ELSE}
  Result := '/var/www/html/';
  {$ENDIF}
end;

// Ou mieux, portable automatiquement :
Result := IncludeTrailingPathDelimiter(GetCurrentDir) + 'web' + PathDelim;
```

#### Permissions et droits

**Windows** :
- Droits NTFS
- Compte IIS ou utilisateur système
- Pas de root/sudo

**Linux** :
- Permissions fichiers (chmod)
- Propriétaire/groupe (chown)
- Services souvent en www-data
- Ports < 1024 nécessitent root

#### Variables d'environnement

**Windows** :
```
%APPDATA%
%TEMP%
%USERPROFILE%
```

**Linux** :
```
$HOME
$USER
$PATH
```

**Code portable** :
```pascal
uses
  sysutils;

var
  TempDir: String;
begin
  TempDir := GetTempDir; // Fonction portable FreePascal
end;
```

### Outils de développement

#### Installation de l'environnement

**Windows** :
1. Installer FreePascal + Lazarus depuis le site officiel
2. Installer XAMPP (Apache + MySQL optionnel)
3. Configurer le PATH système
4. Créer répertoire de développement

**Ubuntu** :
```bash
# Installation FreePascal/Lazarus
sudo apt update
sudo apt install fpc lazarus

# Installation serveur web
sudo apt install apache2 libapache2-mod-fcgid

# Installation base de données (optionnel)
sudo apt install postgresql postgresql-client
```

#### Compilation et déploiement

**Compilation manuelle** :
```bash
# Windows
fpc -MObjFPC -Scghi -O3 webapp.pas

# Linux (identique)
fpc -MObjFPC -Scghi -O3 webapp.pas
```

**Avec Lazarus** :
- Projet → Options du projet
- Modes de compilation (Debug/Release)
- Options du compilateur
- Build (Ctrl+F9)

**Cross-compilation** :
```bash
# Depuis Linux vers Windows
fpc -Twin64 webapp.pas

# Depuis Windows vers Linux
fpc -Tlinux webapp.pas
```

### Architecture REST moderne

#### Principes REST

**REST** (Representational State Transfer) est un style d'architecture web.

**Principes fondamentaux** :
1. **Ressources** identifiées par URLs
2. **Méthodes HTTP** standard (GET, POST, PUT, DELETE)
3. **Sans état** (stateless)
4. **Représentations** (JSON, XML...)
5. **HATEOAS** (liens de navigation)

**Exemple d'API REST** :
```
GET    /api/users          → Liste tous les utilisateurs
GET    /api/users/123      → Détails de l'utilisateur 123
POST   /api/users          → Crée un utilisateur
PUT    /api/users/123      → Modifie l'utilisateur 123
DELETE /api/users/123      → Supprime l'utilisateur 123
```

#### Format JSON

JSON (JavaScript Object Notation) est le format standard du web moderne.

**Exemple** :
```json
{
  "id": 123,
  "name": "Alice",
  "email": "alice@example.com",
  "roles": ["user", "admin"],
  "active": true,
  "created_at": "2025-01-15T10:30:00Z"
}
```

**Avantages** :
- Léger et lisible
- Supporté nativement par tous les langages
- Parfait pour les API
- FreePascal a un excellent support JSON

### Concepts avancés

#### WebSockets

Communication **bidirectionnelle** en temps réel.

**Cas d'usage** :
- Chat en temps réel
- Notifications push
- Tableaux de bord live
- Jeux multijoueurs

**Différence avec HTTP** :
- HTTP : requête → réponse (puis fermeture)
- WebSocket : connexion persistante bidirectionnelle

#### Server-Sent Events (SSE)

Alternative simple aux WebSockets pour **flux unidirectionnel** (serveur → client).

**Avantages** :
- Plus simple que WebSocket
- Fonctionne sur HTTP standard
- Reconnexion automatique
- Parfait pour notifications

#### Microservices

Architecture où l'application est divisée en **petits services indépendants**.

**Avantages** :
- Scalabilité indépendante
- Technologies différentes possibles
- Déploiement plus simple
- Résilience

**FreePascal est excellent pour les microservices** :
- Démarrage instantané
- Faible consommation mémoire
- Un binaire autonome
- Parfait pour conteneurs

### Stratégies de déploiement

#### Développement local

**Windows** :
- Mode standalone sur port 8080
- Accès via http://localhost:8080
- Débogage dans Lazarus IDE

**Ubuntu** :
- Idem ou FastCGI avec Apache local
- Test avec curl ou navigateur

#### Production

**Option 1 : Serveur dédié/VPS**
- Installation FreePascal
- Compilation sur le serveur
- FastCGI avec Nginx/Apache
- Systemd pour gestion service (Linux)

**Option 2 : Conteneurs Docker**
- Image FreePascal
- Binaire compilé copié
- Déploiement ultra-rapide
- Portable Windows/Linux

**Option 3 : Serverless/Cloud**
- AWS Lambda custom runtime
- Google Cloud Run
- DigitalOcean App Platform

### Bonnes pratiques

#### Organisation du code

```
projet-web/
├── src/
│   ├── controllers/    (logique métier)
│   ├── models/         (données)
│   ├── views/          (génération HTML/JSON)
│   └── routes/         (routage)
├── public/             (fichiers statiques)
├── config/             (configuration)
├── tests/              (tests unitaires)
└── webapp.lpr          (programme principal)
```

#### Sécurité

1. **Valider toutes les entrées**
2. **Échapper toutes les sorties**
3. **Utiliser HTTPS en production**
4. **Protéger les secrets** (pas dans le code)
5. **Logger les erreurs** (pas les afficher)
6. **Limiter les requêtes** (rate limiting)

#### Performance

1. **Mettre en cache** ce qui est stable
2. **Compresser les réponses** (gzip)
3. **Optimiser les requêtes BDD**
4. **Utiliser un CDN** pour les fichiers statiques
5. **Monitorer** les performances

### Prochaines étapes

Dans les sections suivantes de ce chapitre, nous explorerons en détail :

- **9.1** fpWeb - Le framework web intégré de FreePascal
- **9.2** FastCGI - Déploiement haute performance
- **9.3** Brook Framework - Développement REST moderne
- **9.4** WebSockets - Communication temps réel
- **9.5** Templates HTML - Génération de contenu
- **9.6** Sessions et authentification - Gestion des utilisateurs
- **9.7** Microservices - Architecture distribuée
- **9.8** Pas2JS - Pascal côté client
- **9.9** WebAssembly - Performance native dans le navigateur
- **9.10** Intégration JavaScript - Interopérabilité
- **9.11** Déploiement - Windows et Linux en production

Chaque section inclura des exemples concrets, du code fonctionnel, et des explications détaillées pour maîtriser la programmation web avec FreePascal sur Windows et Ubuntu.

## Conclusion de l'introduction

FreePascal offre une approche unique et puissante pour le développement web :
- **Performance** exceptionnelle grâce à la compilation native
- **Fiabilité** du typage fort et de la compilation
- **Portabilité** réelle entre Windows, Linux et autres plateformes
- **Simplicité** de déploiement avec des binaires autonomes
- **Écosystème** mature avec plusieurs frameworks de qualité

Que vous développiez des API REST haute performance, des applications d'entreprise robustes, ou des microservices modernes, FreePascal mérite sa place dans votre boîte à outils de développement web.

Prêt à plonger dans le code ? Commençons avec fpWeb, le framework web intégré de FreePascal !

⏭️ [fpWeb - Framework web intégré](/09-programmation-web-freepascal/01-fpweb-framework-web-integre.md)
