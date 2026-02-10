🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.2 Services systemd

## Introduction : Faire vivre vos applications en arrière-plan

Imaginez que vous avez développé une excellente application FreePascal/Lazarus : un serveur web, un système de monitoring, une API REST, ou tout autre programme qui doit fonctionner en permanence. Comment faire pour qu'elle démarre automatiquement avec le système ? Comment la faire tourner en arrière-plan sans fenêtre ? Comment la redémarrer automatiquement si elle plante ? La réponse sous Linux moderne : **systemd**.

## Qu'est-ce que systemd ?

### Le chef d'orchestre de Linux

systemd est le système d'initialisation (init system) utilisé par la majorité des distributions Linux modernes, incluant Ubuntu depuis la version 15.04. C'est lui qui :

- **Démarre votre système** : C'est le premier processus lancé (PID 1)
- **Lance tous les services** : Base de données, serveur web, votre application...
- **Gère les dépendances** : S'assure que tout démarre dans le bon ordre
- **Supervise les processus** : Redémarre les services qui plantent
- **Centralise les logs** : Avec journald, son système de journalisation
- **Gère les ressources** : Limite CPU, mémoire, etc.

### Pourquoi systemd plutôt qu'autre chose ?

Avant systemd, Linux utilisait différents systèmes selon les distributions :
- **SysV init** : Scripts shell complexes et lents
- **Upstart** : Utilisé par Ubuntu avant systemd
- **OpenRC** : Simple mais limité

systemd s'est imposé car il est :
- **Rapide** : Démarrage parallèle des services
- **Moderne** : Conçu pour les besoins actuels
- **Unifié** : Même syntaxe sur toutes les distributions
- **Puissant** : Gestion fine des dépendances et ressources
- **Bien documenté** : Excellente documentation et outils

## Concepts fondamentaux

### Qu'est-ce qu'un service ?

Un **service** (ou daemon en terminologie Unix) est un programme qui :
- Tourne en arrière-plan (pas d'interface graphique)
- Démarre généralement au boot du système
- Fonctionne indépendamment des sessions utilisateur
- Répond à des requêtes ou effectue des tâches périodiques

Exemples de services courants :
- **nginx/Apache** : Serveurs web
- **PostgreSQL/MySQL** : Bases de données
- **SSH** : Accès à distance
- **Cron** : Planification de tâches

Votre application FreePascal peut devenir un service !

### Les unités systemd

systemd ne gère pas que des services. Il utilise le concept d'**unités** :

| Type d'unité | Extension | Usage |
|--------------|-----------|--------|
| **service** | .service | Programme en arrière-plan |
| **socket** | .socket | Socket réseau ou IPC |
| **timer** | .timer | Tâche planifiée (remplace cron) |
| **mount** | .mount | Point de montage |
| **target** | .target | Groupe d'unités (comme les runlevels) |
| **device** | .device | Périphérique matériel |

Pour nos applications, nous utiliserons principalement les unités **.service**.

### États d'un service

Un service peut être dans différents états :

```bash
# Voir l'état d'un service
systemctl status mon-service

# États possibles :
# - active (running) : En cours d'exécution
# - active (exited) : Exécuté et terminé avec succès
# - inactive (dead) : Arrêté
# - failed : Planté ou erreur
# - activating : En cours de démarrage
# - deactivating : En cours d'arrêt
```

## Pourquoi transformer votre application en service systemd ?

### Les avantages immédiats

1. **Démarrage automatique**
   ```bash
   # Plus besoin de lancer manuellement votre app
   # systemd s'en charge au boot
   ```

2. **Redémarrage automatique en cas de crash**
   ```ini
   # Dans votre fichier service
   Restart=always
   RestartSec=10
   ```

3. **Gestion simple**
   ```bash
   systemctl start mon-app    # Démarrer
   systemctl stop mon-app     # Arrêter
   systemctl restart mon-app  # Redémarrer
   systemctl status mon-app   # Voir l'état
   ```

4. **Logs centralisés**
   ```bash
   journalctl -u mon-app -f   # Voir les logs en temps réel
   ```

5. **Isolation et sécurité**
   - Exécution avec un utilisateur dédié
   - Limitation des ressources
   - Isolation du système de fichiers

### Cas d'usage typiques pour vos applications FreePascal

#### 1. Serveur Web ou API REST
```pascal
// Votre serveur HTTP FreePascal
program MonServeurWeb;  
uses
  fpHTTPApp;
begin
  Application.Port := 8080;
  Application.Run;
end;
```
Transformé en service, il démarre avec le système et sert vos pages 24/7.

#### 2. Service de monitoring
```pascal
// Application qui surveille des ressources
program SystemMonitor;  
begin
  repeat
    CheckSystemResources;
    LogMetrics;
    Sleep(60000); // Check toutes les minutes
  until False;
end;
```
En service systemd, il tourne en permanence sans session utilisateur.

#### 3. Worker de traitement de queue
```pascal
// Traitement de tâches en arrière-plan
program QueueWorker;  
begin
  while True do
  begin
    Task := GetNextTaskFromQueue;
    if Task <> nil then
      ProcessTask(Task)
    else
      Sleep(1000);
  end;
end;
```
systemd garantit qu'il est toujours actif pour traiter les tâches.

## Architecture d'une application service-ready

### Différences avec une application desktop

| Application Desktop | Application Service |
|-------------------|-------------------|
| Interface graphique | Pas d'interface (ou web) |
| Lancée par l'utilisateur | Lancée par systemd |
| Vit dans une session | Indépendante des sessions |
| Sortie console visible | Logs via journald |
| Peut afficher des dialogues | Communication via logs/signaux |
| S'arrête quand on ferme | Tourne en permanence |

### Adapter votre code FreePascal

Voici les principes pour rendre votre application "service-compatible" :

```pascal
program MonService;

uses
  SysUtils, BaseUnix;

var
  Terminated: Boolean = False;

// Gestionnaire de signal pour arrêt propre
procedure SignalHandler(sig: longint); cdecl;  
begin
  if sig in [SIGTERM, SIGINT] then
  begin
    WriteLn('Signal reçu, arrêt du service...');
    Terminated := True;
  end;
end;

begin
  // Installation des gestionnaires de signaux
  FpSignal(SIGTERM, @SignalHandler);
  FpSignal(SIGINT, @SignalHandler);

  // Message de démarrage
  WriteLn('Service démarré avec PID: ', GetProcessID);

  // Boucle principale du service
  while not Terminated do
  begin
    try
      // Votre logique métier ici
      DoServiceWork;

      // Petite pause pour ne pas surcharger le CPU
      Sleep(100);
    except
      on E: Exception do
        WriteLn('Erreur: ', E.Message);
    end;
  end;

  // Nettoyage avant arrêt
  WriteLn('Service arrêté proprement');
end.
```

### Points clés pour un bon service

1. **Pas d'interaction utilisateur** : Pas de ReadLn, pas de fenêtres
2. **Gestion des signaux** : Réagir à SIGTERM pour arrêt propre
3. **Logs informatifs** : WriteLn devient vos yeux
4. **Gestion d'erreurs robuste** : Ne jamais crasher sur une exception
5. **Configuration externe** : Fichiers de config, pas de hardcoding

## Comment systemd lance votre application

### Le processus de démarrage

```mermaid
Système boot
    ↓
systemd (PID 1) démarre
    ↓
Lit les fichiers .service
    ↓
Résout les dépendances
    ↓
Lance votre service
    ↓
Supervise l'exécution
```

### Où systemd cherche les services

systemd cherche les fichiers de service dans plusieurs répertoires :

1. **/etc/systemd/system/** : Services système personnalisés (priorité haute)
2. **/lib/systemd/system/** : Services installés par les paquets
3. **/usr/lib/systemd/system/** : Alternative pour certaines distributions
4. **~/.config/systemd/user/** : Services utilisateur

Pour vos applications, vous utiliserez généralement `/etc/systemd/system/`.

### Structure d'un fichier service minimal

Voici à quoi ressemble un fichier service basique :

```ini
[Unit]
Description=Mon Application FreePascal  
After=network.target

[Service]
Type=simple  
ExecStart=/usr/local/bin/mon-app  
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

Chaque section a un rôle :
- **[Unit]** : Métadonnées et dépendances
- **[Service]** : Comment lancer et gérer le service
- **[Install]** : Quand activer le service

## Les commandes systemctl essentielles

### Commandes de base

```bash
# Gestion du service
systemctl start mon-service      # Démarrer  
systemctl stop mon-service       # Arrêter  
systemctl restart mon-service    # Redémarrer  
systemctl reload mon-service     # Recharger la config (si supporté)

# État et informations
systemctl status mon-service     # État détaillé  
systemctl is-active mon-service  # Actif ou non ?  
systemctl is-enabled mon-service # Activé au boot ?

# Activation au démarrage
systemctl enable mon-service     # Activer au boot  
systemctl disable mon-service    # Désactiver au boot

# Après modification d'un fichier .service
systemctl daemon-reload          # Recharger la configuration systemd
```

### Commandes de diagnostic

```bash
# Voir les logs du service
journalctl -u mon-service        # Tous les logs  
journalctl -u mon-service -f     # Logs en temps réel (follow)  
journalctl -u mon-service -n 50  # Les 50 dernières lignes  
journalctl -u mon-service --since "2024-01-01"  # Depuis une date

# Analyser les dépendances
systemctl list-dependencies mon-service

# Voir tous les services
systemctl list-units --type=service  
systemctl list-units --type=service --state=running

# Analyser le temps de boot
systemd-analyze blame  
systemd-analyze critical-chain
```

## Types de services systemd

### Type=simple (par défaut)

Le type le plus courant. systemd considère le service démarré dès que le processus est lancé.

```ini
[Service]
Type=simple  
ExecStart=/usr/bin/mon-app
```

Idéal pour : Applications qui tournent en boucle infinie

### Type=forking

Pour les applications qui se "détachent" (fork) pour tourner en arrière-plan.

```ini
[Service]
Type=forking  
ExecStart=/usr/bin/mon-app --daemon  
PIDFile=/var/run/mon-app.pid
```

Idéal pour : Services traditionnels qui gèrent leur propre daemonization

### Type=oneshot

Pour les scripts ou programmes qui s'exécutent une fois puis se terminent.

```ini
[Service]
Type=oneshot  
ExecStart=/usr/bin/mon-script-init  
RemainAfterExit=yes
```

Idéal pour : Scripts d'initialisation, tâches ponctuelles

### Type=notify

Le service notifie systemd quand il est prêt.

```ini
[Service]
Type=notify  
ExecStart=/usr/bin/mon-app
```

Nécessite l'intégration de sd_notify dans votre code.

### Type=dbus

Le service est considéré prêt quand il acquiert un nom sur D-Bus.

```ini
[Service]
Type=dbus  
BusName=org.monapp.Service  
ExecStart=/usr/bin/mon-app
```

Idéal pour : Services qui communiquent via D-Bus

## Gestion des dépendances

### Ordre de démarrage

systemd permet de définir précisément quand votre service doit démarrer :

```ini
[Unit]
# Démarrer après ces unités
After=network.target postgresql.service

# Démarrer avant ces unités
Before=nginx.service

# Nécessite ces unités (arrêt si elles s'arrêtent)
Requires=postgresql.service

# Souhaite ces unités (continue si elles échouent)
Wants=redis.service
```

### Les targets principaux

Les targets sont des points de synchronisation du démarrage :

| Target | Description |
|--------|-------------|
| basic.target | Système de base initialisé |
| network.target | Réseau configuré |
| multi-user.target | Mode multi-utilisateur (sans GUI) |
| graphical.target | Mode graphique complet |

## Sécurité et isolation

### Exécution avec utilisateur dédié

Ne jamais faire tourner un service en root si possible :

```ini
[Service]
User=monapp  
Group=monapp
```

Créer l'utilisateur système :
```bash
sudo useradd -r -s /bin/false monapp
```

### Limitation des ressources

systemd permet de limiter les ressources :

```ini
[Service]
# Limites mémoire
MemoryMax=512M  
MemorySwapMax=0

# Limites CPU
CPUQuota=50%

# Limites fichiers
LimitNOFILE=1024
```

### Isolation du système

```ini
[Service]
# Protection du système
ProtectSystem=strict  
ProtectHome=true  
PrivateTmp=true  
NoNewPrivileges=true

# Répertoire de travail isolé
WorkingDirectory=/var/lib/monapp  
StateDirectory=monapp
```

## Logs et journald

### Comment systemd gère les logs

Tout ce que votre application écrit sur stdout/stderr est capturé par journald :

```pascal
// Dans votre code FreePascal
WriteLn('Info: Service démarré');        // → journal niveau info  
WriteLn(StdErr, 'Erreur: Connexion DB'); // → journal niveau error
```

### Consulter les logs

```bash
# Logs du service
journalctl -u mon-service

# Filtres temporels
journalctl -u mon-service --since yesterday  
journalctl -u mon-service --since "2024-01-01 09:00"  
journalctl -u mon-service -S -1h  # Dernière heure

# Filtres par priorité
journalctl -u mon-service -p err   # Erreurs seulement  
journalctl -u mon-service -p info  # Info et plus grave

# Export des logs
journalctl -u mon-service -o json > logs.json  
journalctl -u mon-service > logs.txt
```

### Structurer vos logs

Pour des logs plus riches, utilisez des préfixes :

```pascal
procedure LogInfo(const Msg: string);  
begin
  WriteLn('[INFO] ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' - ', Msg);
end;

procedure LogError(const Msg: string);  
begin
  WriteLn(StdErr, '[ERROR] ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' - ', Msg);
end;

procedure LogDebug(const Msg: string);  
begin
  {$IFDEF DEBUG}
  WriteLn('[DEBUG] ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' - ', Msg);
  {$ENDIF}
end;
```

## Timers systemd : l'alternative moderne à cron

### Qu'est-ce qu'un timer systemd ?

Les timers systemd remplacent avantageusement cron pour les tâches planifiées :

```ini
# mon-backup.timer
[Unit]
Description=Backup quotidien

[Timer]
OnCalendar=daily  
Persistent=true

[Install]
WantedBy=timers.target
```

### Avantages sur cron

- **Logs centralisés** : Dans journald
- **Dépendances** : Peut attendre d'autres services
- **Précision** : À la seconde près
- **Flexibilité** : Conditions complexes possibles
- **Monitoring** : `systemctl list-timers`

## Préparer votre application FreePascal

### Check-list avant création du service

✅ **Application autonome**
- Pas d'interaction utilisateur requise
- Configuration via fichiers ou paramètres

✅ **Gestion des erreurs**
- Try/except sur toutes les opérations critiques
- Logs détaillés des erreurs

✅ **Signaux Unix**
- Gestion de SIGTERM pour arrêt propre
- Optionnel : SIGHUP pour recharger la config

✅ **Logs informatifs**
- Messages de démarrage/arrêt
- Logs des opérations importantes
- Erreurs détaillées

✅ **Chemins absolus**
- Pas de chemins relatifs
- Configuration des chemins de données

### Template de base pour un service

```pascal
program ServiceTemplate;

uses
  SysUtils, BaseUnix, IniFiles;

var
  Terminated: Boolean = False;
  Config: TIniFile;

procedure LoadConfiguration;  
begin
  Config := TIniFile.Create('/etc/monapp/config.ini');
  try
    // Charger votre configuration
  finally
    Config.Free;
  end;
end;

procedure SignalHandler(sig: longint); cdecl;  
begin
  case sig of
    SIGTERM, SIGINT:
      begin
        WriteLn('Arrêt demandé...');
        Terminated := True;
      end;
    SIGHUP:
      begin
        WriteLn('Rechargement configuration...');
        LoadConfiguration;
      end;
  end;
end;

procedure Initialize;  
begin
  // Configuration des signaux
  FpSignal(SIGTERM, @SignalHandler);
  FpSignal(SIGINT, @SignalHandler);
  FpSignal(SIGHUP, @SignalHandler);

  // Chargement initial
  LoadConfiguration;

  WriteLn('Service initialisé - PID: ', GetProcessID);
end;

procedure Cleanup;  
begin
  WriteLn('Nettoyage...');
  // Libérer les ressources
end;

procedure MainLoop;  
begin
  while not Terminated do
  begin
    try
      // Votre logique métier ici

      Sleep(1000); // Ajuster selon vos besoins
    except
      on E: Exception do
        WriteLn(StdErr, 'Erreur: ', E.Message);
    end;
  end;
end;

begin
  try
    Initialize;
    MainLoop;
  finally
    Cleanup;
  end;
end.
```

## Ce qui vous attend

Dans les prochaines sections, nous allons apprendre concrètement à :

- **7.2.1** Créer des unités systemd pour vos applications
- **7.2.2** Interagir avec le Service Control Manager

Vous saurez transformer n'importe quelle application FreePascal en service professionnel, robuste et facile à gérer. Prêt à donner vie à vos applications en arrière-plan ?

⏭️ [Création d'unités systemd](/07-specificites-linux-ubuntu/02.1-creation-unites-systemd.md)
