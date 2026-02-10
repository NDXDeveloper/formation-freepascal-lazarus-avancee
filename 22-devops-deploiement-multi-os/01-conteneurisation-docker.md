🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.1 Conteneurisation avec Docker

## Introduction à la conteneurisation

La conteneurisation est une technologie qui révolutionne la façon dont nous développons, testons et déployons des applications. Pour un développeur FreePascal/Lazarus travaillant sur Windows et Ubuntu, Docker représente un outil indispensable qui simplifie considérablement le développement multi-plateforme.

### Qu'est-ce qu'un conteneur ?

Imaginez que vous déménagez d'un appartement à un autre. Vous pourriez :
- **Sans conteneur** : Porter chaque objet individuellement, un par un
- **Avec conteneur** : Emballer tout dans des cartons standardisés, faciles à transporter

En informatique, c'est le même principe :

**Sans conteneur :**
- Installer manuellement FreePascal sur chaque machine
- Configurer les bibliothèques une par une
- S'assurer que toutes les versions correspondent
- Résoudre les conflits entre applications

**Avec conteneur :**
- Empaqueter votre application FreePascal avec toutes ses dépendances
- Garantir que tout fonctionne de manière identique partout
- Déployer en une seule commande
- Isoler complètement chaque application

### Conteneur vs Machine Virtuelle

Il est important de comprendre la différence fondamentale :

**Machine Virtuelle (VM) :**
```
┌─────────────────────────────────────┐
│      Application FreePascal         │
├─────────────────────────────────────┤
│      Bibliothèques & Runtime        │
├─────────────────────────────────────┤
│      Système d'exploitation         │
│         (Ubuntu complet)            │
├─────────────────────────────────────┤
│         Hyperviseur                 │
├─────────────────────────────────────┤
│   OS Hôte (Windows ou Ubuntu)       │
└─────────────────────────────────────┘
```

**Conteneur Docker :**
```
┌─────────────────────────────────────┐
│      Application FreePascal         │
├─────────────────────────────────────┤
│      Bibliothèques & Runtime        │
├─────────────────────────────────────┤
│         Docker Engine               │
├─────────────────────────────────────┤
│   OS Hôte (Windows ou Ubuntu)       │
└─────────────────────────────────────┘
```

**Différences clés :**

| Aspect | Machine Virtuelle | Conteneur Docker |
|--------|-------------------|------------------|
| **Taille** | 1-20 GB | 10-500 MB |
| **Démarrage** | Minutes | Secondes |
| **Performance** | Overhead significatif | Quasi-native |
| **Isolation** | Complète (OS séparé) | Processus (noyau partagé) |
| **Portabilité** | Moyenne | Excellente |
| **Ressources** | Importantes | Minimales |

**Exemple concret :**
- Une VM Ubuntu : ~2 GB, démarre en 30-60 secondes
- Un conteneur Ubuntu : ~70 MB, démarre en 1-2 secondes

### Pourquoi Docker pour FreePascal/Lazarus ?

**1. Environnement de développement unifié**

Vous travaillez en équipe sur un projet FreePascal ? Chaque développeur peut avoir :
- Versions différentes de FPC
- Bibliothèques système différentes
- Configuration différente

Résultat : "Ça marche chez moi !" 🤷

**Avec Docker :**
```bash
docker-compose up
```

Tout le monde a exactement le même environnement, instantanément.

**2. Tests multi-plateformes simplifiés**

Sans Docker, tester sur Ubuntu depuis Windows nécessite :
- Une machine Ubuntu séparée
- Une VM (lente et lourde)
- Un dual-boot (redémarrages constants)

**Avec Docker :**
```bash
# Tester sur Ubuntu depuis Windows
docker run -v ./monprojet:/app ubuntu-fpc fpc /app/main.pas

# Résultat en quelques secondes !
```

**3. Déploiement reproductible**

Votre application fonctionne en développement mais plante en production ?

**Causes fréquentes :**
- Bibliothèque manquante sur le serveur
- Version différente d'une dépendance
- Configuration système différente
- Variables d'environnement manquantes

**Avec Docker :**
L'image que vous testez en développement est EXACTEMENT la même qu'en production. Si ça marche localement, ça marchera en production. Garanti.

**4. Isolation des dépendances**

Vous développez plusieurs projets FreePascal avec des versions différentes de PostgreSQL ?

**Sans Docker :**
- Conflit entre versions
- Configuration complexe
- Risque de casser un projet en travaillant sur un autre

**Avec Docker :**
Chaque projet a sa propre base de données, complètement isolée :
```bash
# Projet A : PostgreSQL 12
docker-compose -f projet-a/docker-compose.yml up

# Projet B : PostgreSQL 15
docker-compose -f projet-b/docker-compose.yml up

# Aucun conflit !
```

**5. Intégration CI/CD naturelle**

Docker s'intègre parfaitement avec les pipelines CI/CD (que nous verrons plus tard), permettant :
- Builds automatiques reproductibles
- Tests dans des environnements identiques
- Déploiement automatisé

### Concepts fondamentaux de Docker

Avant d'aller plus loin, familiarisons-nous avec le vocabulaire Docker :

**1. Image Docker**

Une image est un **modèle en lecture seule** qui contient :
- Un système d'exploitation de base (Ubuntu, Alpine, Debian...)
- Votre application FreePascal compilée
- Toutes les bibliothèques nécessaires
- Les fichiers de configuration

**Analogie :** Une image est comme un **blueprint** (plan d'architecte) ou un **template** de votre application.

**Exemple :**
```dockerfile
FROM ubuntu:22.04  
RUN apt-get update && apt-get install -y fpc  
COPY monapp.pas /app/  
RUN fpc /app/monapp.pas  
CMD ["/app/monapp"]
```

**2. Conteneur Docker**

Un conteneur est une **instance en cours d'exécution** d'une image.

**Analogie :** Si l'image est le blueprint d'une maison, le conteneur est la maison construite et habitée.

Vous pouvez créer plusieurs conteneurs à partir de la même image :
```bash
docker run --name app1 mon-image  
docker run --name app2 mon-image  
docker run --name app3 mon-image
```

Trois conteneurs indépendants, tous basés sur la même image.

**3. Dockerfile**

Un Dockerfile est un **fichier texte** contenant les instructions pour construire une image.

**Exemple simple :**
```dockerfile
# Image de base
FROM ubuntu:22.04

# Installation de FreePascal
RUN apt-get update && apt-get install -y fpc

# Copie du code source
COPY hello.pas /app/hello.pas

# Compilation
RUN fpc /app/hello.pas

# Commande de démarrage
CMD ["/app/hello"]
```

**4. Docker Registry**

Un registre est un **dépôt d'images Docker**.

Le plus connu : **Docker Hub** (https://hub.docker.com)

Similaire à :
- GitHub pour le code
- NPM pour les packages Node.js
- APT pour les paquets Ubuntu

Vous pouvez :
- Télécharger des images publiques (Ubuntu, PostgreSQL, etc.)
- Publier vos propres images
- Héberger un registre privé pour votre entreprise

**5. Volume Docker**

Un volume est un **espace de stockage persistant**.

Par défaut, les données dans un conteneur sont **éphémères** : elles disparaissent quand le conteneur est supprimé.

Les volumes permettent de **persister** les données :
```bash
docker volume create mes-donnees  
docker run -v mes-donnees:/app/data mon-application
```

Les données dans `/app/data` survivent même si le conteneur est détruit.

**6. Réseau Docker**

Docker crée des **réseaux virtuels** permettant aux conteneurs de communiquer.

**Exemple :**
```bash
# Créer un réseau
docker network create mon-reseau

# Conteneur 1 : Application FreePascal
docker run --network mon-reseau --name app mon-app

# Conteneur 2 : Base de données
docker run --network mon-reseau --name db postgres

# L'application peut contacter la base via "db" !
```

**7. Docker Compose**

Docker Compose permet de **définir et gérer plusieurs conteneurs** avec un seul fichier YAML.

**Exemple : `docker-compose.yml`**
```yaml
version: '3.8'  
services:
  app:
    build: .
    ports:
      - "8080:8080"
    depends_on:
      - database

  database:
    image: postgres:15
    environment:
      POSTGRES_PASSWORD: secret
    volumes:
      - db-data:/var/lib/postgresql/data

volumes:
  db-data:
```

**Une seule commande pour tout démarrer :**
```bash
docker-compose up
```

### Architecture de Docker

Comprendre l'architecture aide à mieux utiliser Docker :

```
┌────────────────────────────────────────────────────────┐
│                    CLIENT DOCKER                       │
│                  (docker CLI, Docker Desktop)          │
└────────────────────────┬───────────────────────────────┘
                         │ API REST
                         │
┌────────────────────────▼───────────────────────────────┐
│                   DOCKER DAEMON                        │
│                   (dockerd)                            │
│  ┌──────────────────────────────────────────────────┐  │
│  │            GESTION DES CONTENEURS                │  │
│  │  ┌──────┐  ┌──────┐  ┌──────┐  ┌──────┐          │  │
│  │  │ App1 │  │ App2 │  │ App3 │  │ DB   │          │  │
│  │  └──────┘  └──────┘  └──────┘  └──────┘          │  │
│  └──────────────────────────────────────────────────┘  │
│  ┌──────────────────────────────────────────────────┐  │
│  │            GESTION DES IMAGES                    │  │
│  │  ubuntu:22.04  |  postgres:15  |  mon-app:1.0    │  │
│  └──────────────────────────────────────────────────┘  │
│  ┌──────────────────────────────────────────────────┐  │
│  │            GESTION DES VOLUMES                   │  │
│  │  db-data  |  app-cache  |  logs                  │  │
│  └──────────────────────────────────────────────────┘  │
│  ┌──────────────────────────────────────────────────┐  │
│  │            GESTION DES RÉSEAUX                   │  │
│  │  bridge  |  app-network  |  backend              │  │
│  └──────────────────────────────────────────────────┘  │
└────────────────────────┬───────────────────────────────┘
                         │
┌────────────────────────▼───────────────────────────────┐
│                  SYSTÈME D'EXPLOITATION HÔTE           │
│              (Windows avec WSL 2 ou Ubuntu natif)      │
└────────────────────────────────────────────────────────┘
```

**Flux de travail typique :**

1. Vous exécutez une commande : `docker run ubuntu:22.04`
2. Le **client Docker** envoie la commande au **daemon**
3. Le daemon vérifie si l'image existe localement
4. Si non, il la télécharge depuis Docker Hub
5. Le daemon crée un conteneur à partir de l'image
6. Le conteneur démarre et exécute votre application

### Cas d'usage concrets pour FreePascal/Lazarus

**Cas 1 : Environnement de développement partagé**

**Problème :**
Votre équipe a 5 développeurs avec des configurations différentes. Les bugs "ça marche chez moi" sont fréquents.

**Solution Docker :**
```yaml
# docker-compose.yml
version: '3.8'  
services:
  dev:
    image: ubuntu:22.04
    volumes:
      - ./projet:/workspace
    command: bash
```

Chaque développeur a exactement le même environnement.

**Cas 2 : Tests automatisés multi-versions**

**Problème :**
Vous voulez tester votre application avec FPC 3.2.0 et 3.2.2.

**Solution Docker :**
```bash
# Test avec FPC 3.2.0
docker run -v ./src:/app fpc:3.2.0 fpc /app/main.pas

# Test avec FPC 3.2.2
docker run -v ./src:/app fpc:3.2.2 fpc /app/main.pas
```

**Cas 3 : Déploiement simplifié**

**Problème :**
Déployer sur un serveur Ubuntu nécessite 20 étapes de configuration manuelle.

**Solution Docker :**
```bash
# Sur votre machine
docker build -t mon-app:1.0 .  
docker push mon-app:1.0

# Sur le serveur
docker pull mon-app:1.0  
docker run -d -p 80:80 mon-app:1.0

# Terminé !
```

**Cas 4 : Démonstration client**

**Problème :**
Vous devez faire une démo de votre application mais le laptop du client a une configuration incompatible.

**Solution Docker :**
```bash
# Emportez simplement votre image Docker
docker save mon-app:1.0 > mon-app.tar

# Sur le laptop client
docker load < mon-app.tar  
docker run mon-app:1.0

# L'application fonctionne instantanément !
```

**Cas 5 : Base de données de test**

**Problème :**
Vous testez avec PostgreSQL mais ne voulez pas l'installer sur votre machine.

**Solution Docker :**
```bash
# Démarrer PostgreSQL en 2 secondes
docker run -d -p 5432:5432 \
  -e POSTGRES_PASSWORD=test \
  postgres:15

# Utiliser, tester
# Puis supprimer complètement
docker stop <id>  
docker rm <id>

# Aucune trace sur votre système !
```

### Workflow de développement avec Docker

Voici comment Docker s'intègre dans votre workflow quotidien :

**Phase 1 : Développement Local**

```bash
# Démarrer l'environnement de développement
docker-compose up -d

# Vos services (app, DB, cache) sont prêts
# Vous développez normalement dans Lazarus

# Les changements sont reflétés en temps réel
# (grâce aux volumes montés)
```

**Phase 2 : Tests**

```bash
# Exécuter les tests unitaires
docker-compose run app ./run_tests.sh

# Tests d'intégration
docker-compose run integration-tests

# Tout est isolé, aucun impact sur votre système
```

**Phase 3 : Build**

```bash
# Créer l'image finale
docker build -t mon-app:1.0 .

# L'image contient tout : code, dépendances, config
```

**Phase 4 : Déploiement**

```bash
# Pousser vers le registre
docker push mon-app:1.0

# Sur le serveur de production
docker pull mon-app:1.0  
docker run -d mon-app:1.0

# Déploiement en quelques secondes
```

### Avantages de Docker pour FreePascal/Lazarus

**1. Simplicité de configuration**

**Sans Docker :**
```bash
# Sur chaque machine, manuellement :
sudo apt-get update  
sudo apt-get install fpc lazarus  
sudo apt-get install postgresql-client libpq-dev
# ... 20 autres lignes ...
# Configuration manuelle des variables d'environnement
# Installation des bibliothèques spécifiques
# etc.
```

**Avec Docker :**
```bash
docker-compose up
# Terminé ! Tout est prêt.
```

**2. Portabilité garantie**

Votre application FreePascal fonctionne :
- Sur votre Windows de développement
- Sur Ubuntu de votre collègue
- Sur le serveur CentOS de production
- Sur le Raspberry Pi du client

**Exactement de la même manière**, car elle embarque tout son environnement.

**3. Isolation complète**

Vous pouvez exécuter simultanément :
- Projet A avec PostgreSQL 10
- Projet B avec PostgreSQL 15
- Projet C avec MySQL 8

Sans aucun conflit, chacun dans son conteneur.

**4. Économie de ressources**

Comparé aux VMs :
- Démarrage instantané (secondes vs minutes)
- Utilisation mémoire minimale (MB vs GB)
- Performance quasi-native

**5. Versionning de l'infrastructure**

Votre `Dockerfile` et `docker-compose.yml` sont versionnés avec Git :
- Historique complet des changements
- Retour en arrière facile
- Collaboration simplifiée

**6. Scaling horizontal**

Besoin de plus de puissance ?

```bash
# Passer de 1 à 10 instances
docker-compose up --scale app=10
```

Kubernetes (que nous verrons plus tard) automatise cela.

### Limites et considérations

**1. Courbe d'apprentissage**

Docker introduit de nouveaux concepts. Investissement initial en temps d'apprentissage requis.

**Mitigation :** Ce chapitre ! Suivez-le progressivement.

**2. Overhead (minime)**

Un tout petit overhead existe par rapport à une exécution native.

**Impact réel :** Négligeable pour la plupart des applications FreePascal.

**3. Debugging plus complexe**

Déboguer dans un conteneur demande quelques ajustements.

**Solution :** Outils de debugging disponibles, nous les couvrirons.

**4. Gestion des GUI**

Les applications LCL nécessitent une configuration spéciale (X11 forwarding).

**Solution :** Patterns et configurations fournis dans ce chapitre.

**5. Sécurité**

Les conteneurs doivent être configurés correctement pour être sécurisés.

**Mitigation :** Bonnes pratiques détaillées dans les sections suivantes.

### Prérequis pour cette section

**Connaissances :**
- Ligne de commande (Bash/PowerShell)
- Concepts réseau de base (IP, ports)
- Système de fichiers Linux et Windows

**Installation :**
- Docker Desktop (Windows) OU Docker Engine (Ubuntu)
- Git
- Éditeur de texte

**Temps estimé :**
- Lecture et compréhension : 3-4 heures
- Pratique et exercices : 8-12 heures
- Total : **12-16 heures**

### Structure de cette section 22.1

Cette section est divisée en deux parties principales, correspondant aux deux systèmes d'exploitation :

**22.1.1 Docker Desktop Windows**
- Installation et configuration sur Windows
- Particularités de Docker Desktop
- Intégration WSL 2
- Premier conteneur FreePascal sur Windows
- Bonnes pratiques Windows-specific

**22.1.2 Docker natif Ubuntu**
- Installation via APT
- Configuration native Linux
- Performance optimale
- Premier conteneur FreePascal sur Ubuntu
- Différences avec Docker Desktop

Chaque sous-section est autonome, mais nous recommandons de lire les deux pour comprendre les nuances entre les plateformes.

### Vocabulaire Docker essentiel

Avant de continuer, assurez-vous de comprendre ces termes :

| Terme | Définition |
|-------|------------|
| **Image** | Template en lecture seule pour créer des conteneurs |
| **Conteneur** | Instance en cours d'exécution d'une image |
| **Dockerfile** | Script décrivant comment construire une image |
| **Layer** | Couche d'une image (chaque instruction Dockerfile crée une layer) |
| **Registry** | Dépôt d'images (Docker Hub, registre privé) |
| **Volume** | Espace de stockage persistant |
| **Network** | Réseau virtuel pour la communication entre conteneurs |
| **Compose** | Outil pour définir des applications multi-conteneurs |
| **Tag** | Étiquette de version d'une image (ex: ubuntu:22.04) |
| **Push** | Envoyer une image vers un registry |
| **Pull** | Télécharger une image depuis un registry |
| **Build** | Construire une image depuis un Dockerfile |
| **Run** | Créer et démarrer un conteneur |
| **Exec** | Exécuter une commande dans un conteneur actif |
| **Logs** | Voir les sorties d'un conteneur |

### Commandes Docker de base

Voici les commandes que vous utiliserez le plus souvent :

```bash
# Gestion des images
docker images                    # Lister les images locales  
docker pull ubuntu:22.04        # Télécharger une image  
docker build -t mon-app:1.0 .   # Construire une image  
docker rmi mon-app:1.0          # Supprimer une image

# Gestion des conteneurs
docker ps                       # Conteneurs actifs  
docker ps -a                    # Tous les conteneurs  
docker run ubuntu:22.04         # Créer et démarrer  
docker start <id>               # Démarrer  
docker stop <id>                # Arrêter  
docker rm <id>                  # Supprimer  
docker logs <id>                # Voir les logs  
docker exec -it <id> bash       # Shell interactif

# Gestion des volumes
docker volume ls                # Lister les volumes  
docker volume create mon-vol    # Créer un volume  
docker volume rm mon-vol        # Supprimer un volume

# Gestion des réseaux
docker network ls               # Lister les réseaux  
docker network create mon-net   # Créer un réseau

# Docker Compose
docker-compose up              # Démarrer tous les services  
docker-compose down            # Arrêter et supprimer  
docker-compose logs            # Voir tous les logs  
docker-compose ps              # Status des services

# Nettoyage
docker system prune            # Nettoyer ressources inutilisées
```

### Exemple : Premier conteneur FreePascal

Pour vous donner un avant-goût de ce qui vous attend :

**Fichier : `hello.pas`**
```pascal
program HelloDocker;  
begin
  WriteLn('Bonjour depuis Docker !');
  WriteLn('FreePascal + Docker = ❤️');
end.
```

**Fichier : `Dockerfile`**
```dockerfile
FROM ubuntu:22.04  
RUN apt-get update && apt-get install -y fpc  
COPY hello.pas /app/  
RUN fpc /app/hello.pas  
CMD ["/app/hello"]
```

**Commandes :**
```bash
# Construire l'image
docker build -t hello-fpc .

# Exécuter
docker run hello-fpc
```

**Résultat :**
```
Bonjour depuis Docker !  
FreePascal + Docker = ❤️
```

C'est aussi simple que ça ! Dans les sections suivantes, nous approfondirons chaque aspect.

### Ressources pour aller plus loin

**Documentation officielle :**
- Docker Documentation : https://docs.docker.com
- Docker Hub : https://hub.docker.com
- Docker Compose : https://docs.docker.com/compose

**Tutoriels et cours :**
- Docker Getting Started : https://docs.docker.com/get-started
- Play with Docker : https://labs.play-with-docker.com (environnement de test en ligne)

**Communauté FreePascal + Docker :**
- FreePascal Forum : Section DevOps
- Docker Community Forums : https://forums.docker.com
- Stack Overflow : Tags `docker` + `freepascal`

### Ce que vous apprendrez dans les sous-sections

**Dans 22.1.1 (Docker Desktop Windows) :**
- ✅ Installer Docker Desktop sur Windows 10/11
- ✅ Configurer WSL 2 pour des performances optimales
- ✅ Créer votre première image FreePascal
- ✅ Gérer les volumes Windows
- ✅ Réseaux Docker sous Windows
- ✅ Docker Compose pour applications complexes
- ✅ Intégration avec Visual Studio Code
- ✅ Résolution des problèmes courants Windows

**Dans 22.1.2 (Docker natif Ubuntu) :**
- ✅ Installer Docker Engine sur Ubuntu
- ✅ Configuration optimale pour Linux
- ✅ Performance native maximale
- ✅ Intégration systemd
- ✅ Gestion des permissions Linux
- ✅ Sécurité avec AppArmor/SELinux
- ✅ Monitoring et logs système
- ✅ Comparaison Windows vs Ubuntu

### Philosophie "Infrastructure as Code"

Docker s'inscrit dans la philosophie **Infrastructure as Code (IaC)** :

**Principe :** Tout ce qui constitue votre infrastructure doit être défini dans des fichiers texte versionnés.

**Avantages :**
- **Reproductibilité** : Recréer l'infrastructure à tout moment
- **Documentation** : Le code EST la documentation
- **Collaboration** : Revues de code pour l'infrastructure
- **Historique** : Git conserve tous les changements
- **Automatisation** : Scripts peuvent déployer automatiquement

**Exemple avec FreePascal :**

Votre projet contient :
```
mon-projet/
├── src/              # Code FreePascal
├── Dockerfile        # Définition de l'image
├── docker-compose.yml # Orchestration des services
└── .github/
    └── workflows/
        └── deploy.yml # Pipeline CI/CD
```

Tout est versionné. Tout est reproductible. Tout est automatisable.

### Prêt à plonger ?

Docker va transformer votre façon de travailler avec FreePascal/Lazarus. Fini les "ça marche chez moi", fini les configurations manuelles fastidieuses, fini les conflits de versions.

**Conseil de départ :**
N'essayez pas de tout comprendre d'un coup. Docker est un écosystème riche. Commencez par les bases, expérimentez, faites des erreurs (c'est gratuit et sans risque avec Docker !), et progressez à votre rythme.

**Objectif des sections suivantes :**
À la fin de 22.1.1 et 22.1.2, vous serez capable de :
- ✅ Installer Docker sur Windows et Ubuntu
- ✅ Créer des images pour vos applications FreePascal
- ✅ Orchestrer des applications multi-conteneurs
- ✅ Comprendre les différences entre les plateformes
- ✅ Déployer efficacement sur les deux systèmes

### Note importante sur la pratique

**Théorie vs Pratique :**

Ce tutoriel contient beaucoup d'informations. Mais Docker s'apprend surtout par la **pratique** :

1. **Lisez** la section
2. **Essayez** les exemples
3. **Expérimentez** vos propres variations
4. **Cassez** des choses (conteneurs sont jetables !)
5. **Réparez** et apprenez

Docker est très **permissif** : vous pouvez détruire et recréer à volonté sans risque pour votre système hôte. Profitez-en pour expérimenter !

---

**Passons maintenant à l'installation et la configuration de Docker sur votre système. Commençons par Windows dans la section 22.1.1 ! 🐳**

⏭️ [Docker Desktop Windows](/22-devops-deploiement-multi-os/01.1-docker-desktop-windows.md)
