🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Services Windows avec FreePascal/Lazarus - Introduction

## Qu'est-ce qu'un Service Windows ?

Imaginez que vous ayez besoin d'un programme qui surveille en permanence un dossier pour traiter automatiquement les nouveaux fichiers qui y arrivent. Ou peut-être un serveur web qui doit démarrer automatiquement avec Windows et fonctionner même quand personne n'est connecté. C'est exactement pour ces besoins que les Services Windows existent.

Un **Service Windows** (aussi appelé Service NT) est un type spécial de programme qui :
- **Démarre automatiquement** avec Windows (optionnel)
- **Fonctionne en arrière-plan** sans interface graphique
- **Continue de fonctionner** même quand aucun utilisateur n'est connecté
- **S'exécute sous un compte système** ou utilisateur spécifique
- **Est géré par Windows** via le Gestionnaire de Services

### Analogie simple

Pensez aux services comme aux employés invisibles d'un hôtel de luxe :
- Ils travaillent 24h/24, 7j/7
- Vous ne les voyez pas, mais ils font leur travail
- Ils commencent leur service avant l'arrivée des clients
- Ils continuent après le départ des clients
- Un gestionnaire (le Service Control Manager) supervise leur travail

## Services vs Applications classiques : Les différences fondamentales

### Application classique (avec interface)

```
Démarrage Windows → Utilisateur se connecte → Lance l'application → Interface visible
                                                                    ↓
                                              L'utilisateur interagit avec l'application
                                                                    ↓
                                              Utilisateur ferme l'application → Fin
```

### Service Windows

```
Démarrage Windows → Service démarre automatiquement
                    ↓
                    Fonctionne en arrière-plan (pas d'interface)
                    ↓
                    Continue même si personne n'est connecté
                    ↓
                    S'arrête seulement sur commande ou arrêt Windows
```

### Tableau comparatif

| Caractéristique | Application classique | Service Windows |
|-----------------|----------------------|-----------------|
| **Interface utilisateur** | Fenêtres, boutons, menus | Aucune (par défaut) |
| **Démarrage** | L'utilisateur doit la lancer | Automatique avec Windows |
| **Compte d'exécution** | Compte de l'utilisateur connecté | Système, Service local, ou compte spécifique |
| **Visibilité** | Visible dans la barre des tâches | Invisible (sauf dans le Gestionnaire de services) |
| **Persistance** | Se ferme avec la session utilisateur | Continue après déconnexion |
| **Interaction utilisateur** | Directe via l'interface | Indirecte (logs, fichiers, réseau) |
| **Privilèges** | Limités à l'utilisateur | Peuvent être très élevés (SYSTEM) |
| **Utilisation type** | Traitement de texte, navigateur | Serveur web, antivirus, sauvegarde |

## Exemples concrets de Services Windows

### Services Windows que vous utilisez déjà

Ouvrez le Gestionnaire de Services (services.msc) et vous verrez des dizaines de services. Voici les plus courants :

1. **Windows Update** (wuauserv)
   - Vérifie et télécharge les mises à jour
   - Fonctionne en arrière-plan
   - Démarre automatiquement

2. **Spouleur d'impression** (Spooler)
   - Gère la file d'attente d'impression
   - Permet d'imprimer même après avoir fermé Word
   - Communique avec les imprimantes

3. **Windows Defender Antivirus Service**
   - Surveillance en temps réel
   - Protection permanente
   - Analyse des fichiers en arrière-plan

4. **SQL Server** (si installé)
   - Base de données toujours disponible
   - Accepte les connexions réseau
   - Démarre avant que les utilisateurs se connectent

### Cas d'usage typiques pour créer vos propres services

1. **Serveur d'applications**
   ```
   Service de facturation qui :
   - Écoute sur un port réseau
   - Traite les demandes de facturation
   - Génère des PDFs
   - Envoie des emails
   ```

2. **Surveillance et monitoring**
   ```
   Service de monitoring qui :
   - Vérifie l'espace disque toutes les heures
   - Surveille les performances
   - Envoie des alertes si problème
   - Génère des rapports quotidiens
   ```

3. **Synchronisation de données**
   ```
   Service de synchronisation qui :
   - Synchronise des fichiers avec le cloud
   - Sauvegarde automatique toutes les nuits
   - Réplique des bases de données
   ```

4. **Traitement par lots**
   ```
   Service de traitement qui :
   - Surveille un dossier "À traiter"
   - Convertit des fichiers automatiquement
   - Archive les anciens logs
   - Nettoie les fichiers temporaires
   ```

## Architecture d'un Service Windows

### Le Service Control Manager (SCM)

Le SCM est le chef d'orchestre des services Windows. C'est lui qui :
- **Démarre** les services au bon moment
- **Surveille** leur état (démarré, arrêté, en pause)
- **Transmet** les commandes (démarrer, arrêter, pause, continuer)
- **Gère** les dépendances entre services
- **Applique** les politiques de redémarrage en cas d'échec

### États d'un service

Un service peut être dans plusieurs états :

```
    ┌─────────┐
    │ ARRÊTÉ  │
    └────┬────┘
         │ Commande: Démarrer
         ↓
    ┌─────────────┐
    │ DÉMARRAGE   │ (transitoire)
    └─────┬───────┘
          │
          ↓
    ┌──────────┐      Commande: Pause      ┌───────────┐
    │ DÉMARRÉ  │ ←------------------------→ │ EN PAUSE  │
    └────┬─────┘                            └───────────┘
         │ Commande: Arrêter
         ↓
    ┌──────────┐
    │  ARRÊT   │ (transitoire)
    └────┬─────┘
         │
         ↓
    ┌─────────┐
    │ ARRÊTÉ  │
    └─────────┘
```

### Communication avec le service

Puisqu'un service n'a pas d'interface graphique, comment communiquer avec lui ?

1. **Fichiers de configuration**
   ```
   Le service lit ses paramètres depuis :
   - Un fichier INI ou JSON
   - Le registre Windows
   - Une base de données
   ```

2. **Logs et journalisation**
   ```
   Le service écrit son activité dans :
   - L'Observateur d'événements Windows
   - Des fichiers log
   - Une base de données
   ```

3. **Communication réseau**
   ```
   Le service peut :
   - Écouter sur un port TCP/IP
   - Exposer une API REST
   - Utiliser des Named Pipes
   ```

4. **Application de configuration séparée**
   ```
   Une application GUI qui :
   - Modifie la configuration du service
   - Affiche l'état du service
   - Envoie des commandes au service
   ```

## Types de comptes pour les services

Un service doit s'exécuter sous un compte Windows. Le choix du compte est crucial pour la sécurité :

### 1. Compte LocalSystem (SYSTEM)
- **Privilèges** : Accès total à la machine locale
- **Réseau** : Accès réseau avec l'identité de la machine
- **Usage** : Services nécessitant un accès complet
- **Risque** : Très élevé si compromis

### 2. Compte LocalService
- **Privilèges** : Limités sur la machine locale
- **Réseau** : Accès anonyme seulement
- **Usage** : Services avec besoins minimaux
- **Risque** : Faible

### 3. Compte NetworkService
- **Privilèges** : Limités sur la machine locale
- **Réseau** : Accès avec l'identité de la machine
- **Usage** : Services nécessitant un accès réseau
- **Risque** : Moyen

### 4. Compte utilisateur spécifique
- **Privilèges** : Selon la configuration du compte
- **Réseau** : Selon les droits du compte
- **Usage** : Accès à des ressources spécifiques
- **Risque** : Variable selon les droits accordés

## Avantages des Services Windows

### 1. Disponibilité permanente
- Votre code fonctionne 24/7
- Pas besoin qu'un utilisateur soit connecté
- Redémarrage automatique possible en cas de crash

### 2. Sécurité renforcée
- Isolation des autres processus
- Droits spécifiques configurables
- Pas d'interaction directe avec les utilisateurs

### 3. Gestion centralisée
- Configuration via le Gestionnaire de Services
- Démarrage/arrêt à distance
- Surveillance via les outils Windows

### 4. Performance optimale
- Pas de ressources gaspillées pour l'interface
- Priorité configurable
- Démarrage avant les applications utilisateur

### 5. Intégration Windows
- Logs dans l'Observateur d'événements
- Compteurs de performance
- Gestion par les politiques de groupe

## Défis et considérations

### 1. Pas d'interface graphique directe
**Défi** : Comment savoir ce qui se passe ?  
**Solutions** :  
- Journalisation détaillée
- Application de monitoring séparée
- Notifications par email/SMS

### 2. Débogage plus complexe
**Défi** : On ne peut pas simplement mettre un breakpoint  
**Solutions** :  
- Mode debug qui s'exécute comme application console
- Attachement du débogueur au service en cours
- Logs détaillés pour tracer l'exécution

### 3. Gestion des erreurs critique
**Défi** : Pas d'utilisateur pour voir les messages d'erreur  
**Solutions** :  
- Gestion d'erreur robuste
- Journalisation de toutes les exceptions
- Stratégies de récupération automatique

### 4. Installation et déploiement
**Défi** : Un service doit être "installé" dans Windows  
**Solutions** :  
- Installateur qui enregistre le service
- Utilitaire en ligne de commande
- Script PowerShell d'installation

## Outils pour travailler avec les services

### Outils Windows intégrés

1. **Services.msc** (Gestionnaire de Services)
   - Interface graphique pour gérer les services
   - Démarrer, arrêter, configurer
   - Voir l'état et les dépendances

2. **SC.exe** (Service Control)
   ```cmd
   sc query           # Liste les services
   sc start MyService # Démarre un service
   sc stop MyService  # Arrête un service
   sc delete MyService # Supprime un service
   ```

3. **PowerShell**
   ```powershell
   Get-Service               # Liste les services
   Start-Service MyService   # Démarre un service
   Stop-Service MyService    # Arrête un service
   New-Service               # Crée un nouveau service
   ```

4. **Observateur d'événements**
   - Voir les logs des services
   - Analyser les erreurs
   - Suivre le cycle de vie

### Outils de développement

1. **Mode Debug/Console**
   - Exécuter le service comme application console
   - Facilite le débogage pendant le développement

2. **Simulateur de service**
   - Tester la logique sans installer le service
   - Développement plus rapide

## Structure de base d'un service FreePascal

Voici l'architecture conceptuelle d'un service FreePascal/Lazarus :

```
Programme Principal
    │
    ├─→ Détection du mode d'exécution
    │   ├─→ Mode Service : Enregistrement avec le SCM
    │   └─→ Mode Console : Exécution directe (debug)
    │
    ├─→ Classe TDaemon (ou TCustomDaemon)
    │   ├─→ OnStart    : Initialisation
    │   ├─→ OnExecute  : Boucle principale
    │   ├─→ OnPause    : Mise en pause
    │   ├─→ OnContinue : Reprise
    │   └─→ OnStop     : Arrêt propre
    │
    └─→ Logique métier
        ├─→ Threads de travail
        ├─→ Timers
        ├─→ Gestionnaires d'événements
        └─→ Communication externe
```

## Cycle de vie d'un service

### 1. Installation
```
Administrateur exécute : MonService.exe /install
    ↓
Service enregistré dans Windows
    ↓
Configuration initiale (automatique, manuel, désactivé)
```

### 2. Démarrage
```
Windows démarre OU Utilisateur démarre le service
    ↓
SCM charge le service en mémoire
    ↓
Appel de OnStart
    ↓
Initialisation (connexions, threads, timers)
    ↓
Service en cours d'exécution
```

### 3. Exécution
```
Boucle principale active
    ↓
Traitement des tâches
    ↓
Réponse aux événements
    ↓
Communication avec l'extérieur
```

### 4. Arrêt
```
Windows s'arrête OU Utilisateur arrête le service
    ↓
SCM envoie signal d'arrêt
    ↓
Appel de OnStop
    ↓
Nettoyage (fermeture connexions, sauvegarde état)
    ↓
Service arrêté
```

## Bonnes pratiques pour les services

### 1. Conception robuste
- **Gestion d'erreurs exhaustive** : Tout peut mal tourner
- **Récupération automatique** : Le service doit se rétablir seul
- **Pas de blocage** : Utiliser des timeouts
- **Resources limitées** : Attention à la mémoire et au CPU

### 2. Journalisation appropriée
- **Niveau de log configurable** : Debug, Info, Warning, Error
- **Rotation des logs** : Ne pas remplir le disque
- **Informations utiles** : Timestamps, contexte, stack traces

### 3. Configuration flexible
- **Rechargeable à chaud** : Sans redémarrer le service
- **Valeurs par défaut sensées** : Le service doit démarrer même mal configuré
- **Validation** : Vérifier toutes les valeurs de configuration

### 4. Sécurité
- **Principe du moindre privilège** : Utiliser le compte avec le moins de droits possible
- **Validation des entrées** : Ne jamais faire confiance aux données externes
- **Communications sécurisées** : Chiffrement si nécessaire

### 5. Performance
- **Traitement asynchrone** : Ne pas bloquer le thread principal
- **Mise en cache** : Éviter les calculs répétitifs
- **Ressources partagées** : Attention aux accès concurrents

## Cas d'usage détaillé : Service de surveillance de dossier

Imaginons un service qui surveille un dossier et traite automatiquement les fichiers :

```
Scénario : Une entreprise reçoit des commandes par email en PDF
           Le service doit les traiter automatiquement

Fonctionnement :
1. Le service surveille C:\Commandes\Nouvelles
2. Quand un PDF arrive :
   - Extraction des données
   - Validation de la commande
   - Insertion en base de données
   - Déplacement vers C:\Commandes\Traitées
   - Email de confirmation
3. En cas d'erreur :
   - Déplacement vers C:\Commandes\Erreurs
   - Log détaillé
   - Notification à l'administrateur

Avantages du service :
- Fonctionne 24/7 sans intervention
- Traitement immédiat
- Pas d'erreur humaine
- Traçabilité complète
```

## Préparation pour le développement

### Prérequis techniques

1. **Droits administrateur** pour installer/désinstaller les services
2. **Compréhension de la sécurité Windows** pour choisir le bon compte
3. **Stratégie de logs** pour suivre l'exécution
4. **Plan de tests** car le debug est plus complexe

### Environnement de développement

1. **Machine de développement**
   - Windows 10/11 Pro (ou Server pour tests avancés)
   - Lazarus avec les packages daemon
   - Accès administrateur

2. **Machine de test** (idéalement)
   - Pour tester l'installation propre
   - Tester différents comptes
   - Simuler les redémarrages

### Mindset du développeur de services

Développer un service nécessite un changement de perspective :

1. **Pensez "sans surveillance"**
   - Votre code doit gérer toutes les situations
   - Personne ne verra les messages d'erreur
   - La récupération doit être automatique

2. **Pensez "long terme"**
   - Le service peut tourner des mois sans redémarrage
   - Attention aux fuites mémoire
   - Gestion de la croissance des logs

3. **Pensez "diagnostic"**
   - Comment savoir ce qui s'est passé il y a 3 jours ?
   - Comment déboguer un problème intermittent ?
   - Comment mesurer les performances ?

## Résumé : L'essentiel à retenir

1. **Un service Windows est un programme spécial** qui tourne en arrière-plan sans interface graphique

2. **Idéal pour les tâches automatisées** : serveurs, surveillance, traitement par lots

3. **Géré par Windows** via le Service Control Manager

4. **Plus complexe qu'une application classique** mais offre des avantages uniques

5. **FreePascal/Lazarus simplifie** le développement de services avec ses frameworks

6. **La journalisation est cruciale** car pas d'interface pour voir ce qui se passe

7. **La sécurité est importante** : choisir le bon compte d'exécution

8. **Tests approfondis nécessaires** car le débogage est plus difficile

Maintenant que vous comprenez ce qu'est un service Windows et pourquoi ils sont utiles, nous allons voir concrètement comment créer votre premier service avec FreePascal/Lazarus.

⏭️ [Création de services](/06-specificites-windows/02.1-creation-services.md)
