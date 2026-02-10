🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.8 Logging centralisé (ELK stack)

## Introduction au logging centralisé

Le logging centralisé est une approche qui consiste à collecter, stocker et analyser tous les logs de vos applications à un seul endroit. Au lieu d'avoir des fichiers de logs dispersés sur différents serveurs et machines, vous centralisez tout pour faciliter la surveillance, le diagnostic et l'analyse.

### Pourquoi centraliser les logs ?

Imaginez que vous avez une application FreePascal/Lazarus qui tourne sur plusieurs serveurs (certains sous Windows, d'autres sous Ubuntu). Sans centralisation :

- Vous devez vous connecter à chaque serveur pour consulter les logs
- Il est difficile de corréler les événements entre différents services
- La recherche d'erreurs spécifiques devient fastidieuse
- L'analyse de tendances sur l'ensemble du système est complexe

Avec un système centralisé, tous vos logs arrivent au même endroit, facilement recherchables et analysables.

## Qu'est-ce que la stack ELK ?

ELK est l'acronyme de trois composants open source :

### 1. **Elasticsearch**
Un moteur de recherche et d'analyse distribué. Il stocke vos logs et permet de les rechercher très rapidement, même avec des millions d'entrées.

**Analogie** : Pensez à Elasticsearch comme à une bibliothèque gigantesque avec un système de catalogage ultra-performant qui vous permet de retrouver n'importe quel livre en quelques secondes.

### 2. **Logstash**
Un pipeline de traitement de données qui collecte, transforme et envoie vos logs vers Elasticsearch. Il peut recevoir des logs de différentes sources, les parser, les enrichir et les formater.

**Analogie** : Logstash est comme un centre de tri postal qui reçoit des colis (logs) de partout, les ouvre, vérifie leur contenu, ajoute des informations, puis les envoie au bon endroit (Elasticsearch).

### 3. **Kibana**
Une interface web de visualisation qui se connecte à Elasticsearch. C'est votre tableau de bord pour explorer, rechercher et créer des graphiques à partir de vos logs.

**Analogie** : Kibana est la vitrine qui vous permet de voir tout ce qui se passe dans votre bibliothèque (Elasticsearch) avec des statistiques visuelles, des graphiques et des outils de recherche conviviaux.

### Note importante : Beats et la stack complète

Aujourd'hui, on parle souvent de **"Elastic Stack"** plutôt que ELK, car elle inclut aussi les **Beats** :

- **Filebeat** : Agent léger qui surveille des fichiers de logs et les envoie à Logstash ou Elasticsearch
- **Metricbeat** : Collecte des métriques système (CPU, RAM, etc.)
- **Packetbeat** : Analyse le trafic réseau
- Et d'autres...

Pour nos applications FreePascal, nous utiliserons principalement **Filebeat** pour envoyer nos logs.

## Architecture typique pour une application FreePascal/Lazarus

Voici comment intégrer ELK avec vos applications multi-plateformes :

```
[Application FreePascal Windows] → Fichier log → [Filebeat Windows]
                                                           ↓
[Application FreePascal Ubuntu]  → Fichier log → [Filebeat Ubuntu] → [Logstash] → [Elasticsearch] ← [Kibana]
                                                           ↑
[Application FreePascal Serveur] → Fichier log → [Filebeat Linux]
```

**Flux de données** :
1. Votre application FreePascal écrit des logs dans un fichier (JSON de préférence)
2. Filebeat surveille ce fichier et envoie les nouvelles lignes à Logstash
3. Logstash parse et enrichit les logs, puis les envoie à Elasticsearch
4. Elasticsearch indexe et stocke les logs
5. Vous utilisez Kibana pour visualiser et analyser

## Préparer votre application FreePascal pour ELK

### Format de logs recommandé : JSON

ELK fonctionne mieux avec des logs structurés au format JSON. Voici comment logger en JSON depuis FreePascal :

```pascal
unit LoggerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  TJSONLogger = class
  private
    FLogFile: TextFile;
    FFileName: string;
    FApplicationName: string;
    FHostname: string;
    procedure WriteLogEntry(ALevel: TLogLevel; AMessage: string; AData: TJSONObject = nil);
  public
    constructor Create(const AFileName, AAppName: string);
    destructor Destroy; override;

    procedure Debug(const AMessage: string; AData: TJSONObject = nil);
    procedure Info(const AMessage: string; AData: TJSONObject = nil);
    procedure Warning(const AMessage: string; AData: TJSONObject = nil);
    procedure Error(const AMessage: string; AData: TJSONObject = nil);
    procedure Critical(const AMessage: string; AData: TJSONObject = nil);
  end;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  Unix, BaseUnix,
  {$ENDIF}
  DateUtils;

constructor TJSONLogger.Create(const AFileName, AAppName: string);
begin
  FFileName := AFileName;
  FApplicationName := AAppName;

  // Récupérer le nom de la machine
  {$IFDEF WINDOWS}
  SetLength(FHostname, MAX_COMPUTERNAME_LENGTH + 1);
  GetComputerName(PChar(FHostname), Cardinal(Length(FHostname)));
  {$ENDIF}
  {$IFDEF UNIX}
  FHostname := GetHostName;
  {$ENDIF}

  AssignFile(FLogFile, FFileName);
  if FileExists(FFileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
end;

destructor TJSONLogger.Destroy;
begin
  CloseFile(FLogFile);
  inherited;
end;

procedure TJSONLogger.WriteLogEntry(ALevel: TLogLevel; AMessage: string; AData: TJSONObject);
var
  LogEntry: TJSONObject;
  LogLevel: string;
begin
  // Convertir le niveau en string
  case ALevel of
    llDebug: LogLevel := 'DEBUG';
    llInfo: LogLevel := 'INFO';
    llWarning: LogLevel := 'WARNING';
    llError: LogLevel := 'ERROR';
    llCritical: LogLevel := 'CRITICAL';
  end;

  // Créer l'objet JSON de log
  LogEntry := TJSONObject.Create;
  try
    LogEntry.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', Now));
    LogEntry.Add('level', LogLevel);
    LogEntry.Add('message', AMessage);
    LogEntry.Add('application', FApplicationName);
    LogEntry.Add('hostname', FHostname);
    LogEntry.Add('platform', {$IFDEF WINDOWS}'Windows'{$ELSE}'Linux'{$ENDIF});

    // Ajouter des données supplémentaires si fournies
    if Assigned(AData) then
      LogEntry.Add('data', AData.Clone as TJSONObject);

    // Écrire dans le fichier
    WriteLn(FLogFile, LogEntry.AsJSON);
    Flush(FLogFile);
  finally
    LogEntry.Free;
  end;
end;

procedure TJSONLogger.Debug(const AMessage: string; AData: TJSONObject);
begin
  WriteLogEntry(llDebug, AMessage, AData);
end;

procedure TJSONLogger.Info(const AMessage: string; AData: TJSONObject);
begin
  WriteLogEntry(llInfo, AMessage, AData);
end;

procedure TJSONLogger.Warning(const AMessage: string; AData: TJSONObject);
begin
  WriteLogEntry(llWarning, AMessage, AData);
end;

procedure TJSONLogger.Error(const AMessage: string; AData: TJSONObject);
begin
  WriteLogEntry(llError, AMessage, AData);
end;

procedure TJSONLogger.Critical(const AMessage: string; AData: TJSONObject);
begin
  WriteLogEntry(llCritical, AMessage, AData);
end;

end.
```

### Utilisation dans votre application

```pascal
program MyApp;

uses
  SysUtils, LoggerUnit, fpjson;

var
  Logger: TJSONLogger;
  UserData: TJSONObject;

begin
  // Initialiser le logger
  Logger := TJSONLogger.Create('application.log', 'MyFreePascalApp');
  try
    Logger.Info('Application démarrée');

    // Log avec données structurées
    UserData := TJSONObject.Create;
    try
      UserData.Add('user_id', 12345);
      UserData.Add('action', 'login');
      Logger.Info('Utilisateur connecté', UserData);
    finally
      UserData.Free;
    end;

    try
      // Votre code applicatif ici
      raise Exception.Create('Erreur de démonstration');
    except
      on E: Exception do
        Logger.Error('Exception capturée: ' + E.Message);
    end;

    Logger.Info('Application terminée normalement');
  finally
    Logger.Free;
  end;
end.
```

**Exemple de sortie dans `application.log`** :

```json
{"timestamp":"2025-10-08T14:32:15.123","level":"INFO","message":"Application démarrée","application":"MyFreePascalApp","hostname":"SERVER-WIN01","platform":"Windows"}
{"timestamp":"2025-10-08T14:32:15.456","level":"INFO","message":"Utilisateur connecté","application":"MyFreePascalApp","hostname":"SERVER-WIN01","platform":"Windows","data":{"user_id":12345,"action":"login"}}
{"timestamp":"2025-10-08T14:32:15.789","level":"ERROR","message":"Exception capturée: Erreur de démonstration","application":"MyFreePascalApp","hostname":"SERVER-WIN01","platform":"Windows"}
```

## Installation d'Elasticsearch

### Sur Ubuntu

```bash
# Importer la clé GPG d'Elasticsearch
wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | sudo apt-key add -

# Ajouter le dépôt
echo "deb https://artifacts.elastic.co/packages/8.x/apt stable main" | sudo tee /etc/apt/sources.list.d/elastic-8.x.list

# Installer Elasticsearch
sudo apt update
sudo apt install elasticsearch

# Démarrer le service
sudo systemctl enable elasticsearch
sudo systemctl start elasticsearch

# Vérifier que ça fonctionne
curl -X GET "localhost:9200/"
```

### Sur Windows

1. Téléchargez Elasticsearch depuis https://www.elastic.co/downloads/elasticsearch
2. Décompressez l'archive
3. Lancez `bin\elasticsearch.bat`
4. Vérifiez dans votre navigateur : http://localhost:9200/

### Configuration de base

Éditez le fichier de configuration :
- **Ubuntu** : `/etc/elasticsearch/elasticsearch.yml`
- **Windows** : `config\elasticsearch.yml`

```yaml
# Nom du cluster
cluster.name: my-application-cluster

# Nom du nœud
node.name: node-1

# Réseau (attention : 0.0.0.0 expose sur toutes les interfaces)
network.host: localhost

# Port HTTP
http.port: 9200

# Chemin des données (modifiez selon vos besoins)
path.data: /var/lib/elasticsearch
path.logs: /var/log/elasticsearch
```

## Installation de Logstash

### Sur Ubuntu

```bash
sudo apt install logstash

# Créer un fichier de configuration
sudo nano /etc/logstash/conf.d/freepascal-logs.conf
```

### Sur Windows

1. Téléchargez Logstash depuis https://www.elastic.co/downloads/logstash
2. Décompressez l'archive
3. Créez un fichier de configuration dans `config\`

### Configuration Logstash pour vos logs FreePascal

Créez un fichier de configuration (par exemple `freepascal-logs.conf`) :

```ruby
input {
  # Recevoir les logs depuis Filebeat
  beats {
    port => 5044
  }
}

filter {
  # Parser le JSON
  json {
    source => "message"
  }

  # Convertir le timestamp
  date {
    match => ["timestamp", "ISO8601"]
    target => "@timestamp"
  }

  # Ajouter des tags basés sur le niveau de log
  if [level] == "ERROR" or [level] == "CRITICAL" {
    mutate {
      add_tag => ["error"]
    }
  }

  # Enrichissement avec des informations géographiques (optionnel)
  # Utile si vous loggez des IP
  if [client_ip] {
    geoip {
      source => "client_ip"
    }
  }
}

output {
  # Envoyer vers Elasticsearch
  elasticsearch {
    hosts => ["localhost:9200"]
    index => "freepascal-logs-%{+YYYY.MM.dd}"
  }

  # Optionnel : afficher dans la console pour debug
  stdout {
    codec => rubydebug
  }
}
```

**Explication de la configuration** :

- **input** : Logstash écoute sur le port 5044 pour recevoir les logs de Filebeat
- **filter** :
  - Parse le JSON de vos logs
  - Convertit le timestamp au format Elasticsearch
  - Ajoute des tags pour les erreurs (facilite les recherches)
- **output** :
  - Envoie les logs vers Elasticsearch
  - Crée un index par jour (`freepascal-logs-2025.10.08`)

### Démarrer Logstash

**Ubuntu** :
```bash
sudo systemctl enable logstash
sudo systemctl start logstash
```

**Windows** :
```bash
bin\logstash.bat -f config\freepascal-logs.conf
```

## Installation de Filebeat

Filebeat est l'agent qui va surveiller vos fichiers de logs et les envoyer à Logstash.

### Sur Ubuntu

```bash
sudo apt install filebeat

# Éditer la configuration
sudo nano /etc/filebeat/filebeat.yml
```

### Sur Windows

1. Téléchargez Filebeat depuis https://www.elastic.co/downloads/beats/filebeat
2. Décompressez l'archive
3. Éditez `filebeat.yml`

### Configuration Filebeat

Voici une configuration de base pour surveiller vos logs FreePascal :

```yaml
# Configuration des inputs
filebeat.inputs:
  - type: log
    enabled: true
    paths:
      # Windows
      - C:\Logs\application.log
      # Ubuntu (commentez celui qui ne correspond pas)
      # - /var/log/myapp/application.log

    # Format JSON
    json.keys_under_root: true
    json.add_error_key: true

    # Champs supplémentaires
    fields:
      log_type: freepascal_app
      environment: production

# Configuration de la sortie vers Logstash
output.logstash:
  hosts: ["localhost:5044"]

# Optionnel : sortie directe vers Elasticsearch (sans Logstash)
# output.elasticsearch:
#   hosts: ["localhost:9200"]
#   index: "freepascal-logs-%{+yyyy.MM.dd}"

# Logging de Filebeat lui-même
logging.level: info
logging.to_files: true
logging.files:
  path: /var/log/filebeat
  name: filebeat
  keepfiles: 7
```

### Démarrer Filebeat

**Ubuntu** :
```bash
sudo systemctl enable filebeat
sudo systemctl start filebeat

# Vérifier le statut
sudo systemctl status filebeat
```

**Windows** :
```powershell
# Installer comme service (exécuter en administrateur)
.\install-service-filebeat.ps1

# Démarrer le service
Start-Service filebeat
```

## Installation de Kibana

Kibana est votre interface de visualisation.

### Sur Ubuntu

```bash
sudo apt install kibana

sudo systemctl enable kibana
sudo systemctl start kibana
```

### Sur Windows

1. Téléchargez Kibana depuis https://www.elastic.co/downloads/kibana
2. Décompressez l'archive
3. Lancez `bin\kibana.bat`

### Configuration de base

Éditez le fichier de configuration :
- **Ubuntu** : `/etc/kibana/kibana.yml`
- **Windows** : `config\kibana.yml`

```yaml
# Port sur lequel Kibana écoute
server.port: 5601

# Adresse du serveur
server.host: "localhost"

# URL d'Elasticsearch
elasticsearch.hosts: ["http://localhost:9200"]
```

Accédez à Kibana : http://localhost:5601

## Utiliser Kibana pour visualiser vos logs

### 1. Créer un Index Pattern

Lorsque vous accédez à Kibana pour la première fois :

1. Allez dans **Management** → **Index Patterns**
2. Cliquez sur **Create index pattern**
3. Entrez le pattern : `freepascal-logs-*`
4. Sélectionnez `@timestamp` comme champ de temps
5. Cliquez sur **Create**

### 2. Explorer vos logs (Discover)

1. Allez dans **Discover** dans le menu de gauche
2. Vous verrez tous vos logs apparaître
3. Utilisez la barre de recherche pour filtrer :
   - `level: ERROR` → Affiche seulement les erreurs
   - `application: "MyFreePascalApp"` → Logs d'une app spécifique
   - `platform: "Windows"` → Logs des serveurs Windows uniquement

### 3. Créer des visualisations

**Exemple : Graphique des erreurs par heure**

1. Allez dans **Visualize** → **Create visualization**
2. Choisissez **Line** (graphique en ligne)
3. Sélectionnez votre index pattern
4. Configuration :
   - **Y-axis** : Count
   - **X-axis** : Date Histogram sur `@timestamp` (intervalle : 1 heure)
   - **Split series** : Par `level.keyword`
5. Ajoutez un filtre : `level: ERROR OR level: CRITICAL`
6. Sauvegardez la visualisation

**Exemple : Tableau des applications les plus loguées**

1. Créez une nouvelle visualisation de type **Data Table**
2. Configuration :
   - **Metrics** : Count
   - **Buckets** : Terms sur `application.keyword`
3. Sauvegardez

### 4. Créer un Dashboard

1. Allez dans **Dashboard** → **Create dashboard**
2. Cliquez sur **Add** et sélectionnez vos visualisations
3. Organisez-les sur le dashboard
4. Sauvegardez

**Suggestions de visualisations utiles** :
- Graphique temporel des logs par niveau de sévérité
- Top 10 des messages d'erreur les plus fréquents
- Répartition des logs par plateforme (Windows/Ubuntu)
- Carte thermique des erreurs par heure/jour
- Liste des dernières erreurs critiques

## Bonnes pratiques pour le logging avec ELK

### 1. Structurer vos logs

Utilisez toujours le format JSON avec des champs cohérents :

```pascal
// BON : log structuré
Logger.Error('Échec de connexion à la base de données',
  CreateJSON(['database', 'PostgreSQL', 'host', '192.168.1.100', 'error_code', 1234]));

// MOINS BON : message texte non structuré
Logger.Error('Échec de connexion à la base de données PostgreSQL sur 192.168.1.100, code 1234');
```

### 2. Niveaux de log appropriés

- **DEBUG** : Informations détaillées pour le développement
- **INFO** : Événements importants normaux (démarrage, connexion utilisateur)
- **WARNING** : Quelque chose d'inhabituel mais gérable
- **ERROR** : Erreur qui nécessite attention mais l'app continue
- **CRITICAL** : Erreur grave, l'application ne peut pas continuer

### 3. Ajouter du contexte

Incluez toujours des informations qui aideront au diagnostic :

```pascal
var
  Context: TJSONObject;
begin
  Context := TJSONObject.Create;
  try
    Context.Add('user_id', CurrentUserID);
    Context.Add('session_id', CurrentSessionID);
    Context.Add('request_id', RequestID);
    Context.Add('duration_ms', ElapsedMilliseconds);
    Logger.Info('Requête traitée', Context);
  finally
    Context.Free;
  end;
end;
```

### 4. Rotation des logs

Pour éviter de remplir le disque :

**Approche simple en FreePascal** :
```pascal
procedure RotateLogFile(const BaseFileName: string; MaxSizeMB: Integer);
var
  SR: TSearchRec;
  SizeMB: Int64;
  NewName: string;
begin
  if FindFirst(BaseFileName, faAnyFile, SR) = 0 then
  begin
    SizeMB := SR.Size div (1024 * 1024); // En MB
    FindClose(SR);
    if SizeMB > MaxSizeMB then
    begin
      NewName := BaseFileName + '.' + FormatDateTime('yyyymmdd-hhnnss', Now);
      RenameFile(BaseFileName, NewName);
    end;
  end;
end;
```

**Ou utilisez logrotate sur Ubuntu** :

```bash
# /etc/logrotate.d/freepascal-app
/var/log/myapp/*.log {
    daily
    rotate 7
    compress
    delaycompress
    notifempty
    create 0640 myappuser myappgroup
}
```

### 5. Surveillance et alertes

Configurez des alertes dans Kibana pour les événements critiques :

1. Allez dans **Management** → **Watcher** (nécessite une licence)
2. Ou utilisez ElastAlert (outil open source)
3. Configurez des alertes par email/Slack quand :
   - Plus de 10 erreurs par minute
   - Une erreur CRITICAL apparaît
   - Un service ne logue plus (signe de crash)

## Performance et optimisation

### Indexation sélective

Ne loguez pas tout en production. Filtrez les logs DEBUG :

```pascal
{$IFDEF DEBUG}
Logger.Debug('Détail technique...');
{$ENDIF}

// Ou avec un paramètre de configuration
if AppConfig.LogLevel >= llInfo then
  Logger.Info('Information importante');
```

### Échantillonnage

Pour les applications à très haut volume, échantillonnez :

```pascal
// Logger seulement 10% des requêtes normales
if Random(100) < 10 then
  Logger.Info('Requête traitée');

// Mais toujours logger les erreurs
Logger.Error('Erreur critique');
```

### Taille des index Elasticsearch

Créez des index par jour (déjà fait dans notre config) et supprimez les anciens :

```bash
# Supprimer les index de plus de 30 jours
curator delete indices --older-than 30 --time-unit days --timestring '%Y.%m.%d'
```

## Dépannage courant

### Les logs n'apparaissent pas dans Kibana

1. **Vérifier que l'application écrit les logs** :
   ```bash
   # Ubuntu
   tail -f /var/log/myapp/application.log

   # Windows
   Get-Content C:\Logs\application.log -Wait -Tail 10
   ```

2. **Vérifier que Filebeat fonctionne** :
   ```bash
   # Ubuntu
   sudo systemctl status filebeat
   sudo tail -f /var/log/filebeat/filebeat

   # Windows (PowerShell)
   Get-Service filebeat
   Get-Content "C:\Program Files\Filebeat\logs\filebeat" -Tail 20
   ```

3. **Vérifier que Logstash reçoit les données** :
   ```bash
   # Ubuntu
   sudo systemctl status logstash
   sudo tail -f /var/log/logstash/logstash-plain.log
   ```

4. **Vérifier qu'Elasticsearch est accessible** :
   ```bash
   curl -X GET "localhost:9200/_cat/indices?v"
   ```

### Erreurs de parsing JSON

Si vos logs ne sont pas bien parsés :

1. Vérifiez que votre JSON est valide :
   ```bash
   cat application.log | head -1 | jq .
   ```

2. Ajustez la configuration Logstash :
   ```ruby
   filter {
     json {
       source => "message"
       skip_on_invalid_json => true  # Ne pas planter sur JSON invalide
     }
   }
   ```

## Sécurité

### Authentification Elasticsearch

En production, activez la sécurité :

```yaml
# elasticsearch.yml
xpack.security.enabled: true
```

Créez des utilisateurs :
```bash
bin/elasticsearch-setup-passwords interactive
```

### Chiffrement TLS

Configurez TLS entre les composants :

```yaml
# elasticsearch.yml
xpack.security.transport.ssl.enabled: true
xpack.security.http.ssl.enabled: true
```

### Ne jamais logger de données sensibles

```pascal
// MAUVAIS - mot de passe en clair dans les logs
Logger.Info('Connexion réussie', CreateJSON(['username', user, 'password', pass]));

// BON - pas de données sensibles
Logger.Info('Connexion réussie', CreateJSON(['username', user]));
```

## Alternatives et compléments

### Alternatives à ELK

- **Graylog** : Plus simple à installer, interface conviviale
- **Loki** (Grafana) : Très léger, optimisé pour Kubernetes
- **Fluentd** : Alternative à Logstash, très populaire

### Compléments utiles

- **Curator** : Gérer automatiquement les anciens index
- **ElastAlert** : Système d'alertes avancé
- **APM (Application Performance Monitoring)** : Intégré à Elastic pour tracer les performances

## Conclusion

Le logging centralisé avec ELK transforme la façon dont vous surveillez vos applications FreePascal multi-plateformes. En investissant du temps dans une bonne infrastructure de logging, vous :

- Détectez les problèmes plus rapidement
- Comprenez mieux le comportement de vos applications
- Facilitez le diagnostic en production
- Améliorez la qualité globale de votre code

L'essentiel est de commencer simplement : loguer en JSON, installer la stack ELK, et progressivement améliorer vos dashboards et alertes selon vos besoins réels.

---

**Ressources complémentaires** :
- Documentation officielle Elastic : https://www.elastic.co/guide/
- Filebeat documentation : https://www.elastic.co/guide/en/beats/filebeat/
- Exemples de dashboards Kibana : https://github.com/elastic/examples

⏭️ [Blue-Green deployments](/22-devops-deploiement-multi-os/09-blue-green-deployments.md)
