🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.7 Monitoring et métriques (Prometheus)

## Introduction

Le monitoring (surveillance) est l'art d'observer le comportement de votre application en production. Il vous permet de savoir si votre programme fonctionne correctement, s'il est performant, et de détecter les problèmes avant qu'ils n'impactent vos utilisateurs.

Prometheus est un système de monitoring et d'alerting open-source très populaire, créé à l'origine par SoundCloud. Il collecte et stocke des métriques (mesures) sous forme de séries temporelles, c'est-à-dire des données horodatées.

Dans ce chapitre, nous allons apprendre à intégrer Prometheus dans vos applications FreePascal/Lazarus pour surveiller leur santé et leurs performances, aussi bien sur Windows que sur Ubuntu.

## Qu'est-ce qu'une métrique ?

Une **métrique** est une mesure quantifiable d'un aspect de votre application. Par exemple :

- **Nombre de requêtes HTTP traitées** : combien de fois votre API a été appelée
- **Temps de réponse** : combien de temps prend une opération
- **Utilisation mémoire** : combien de RAM votre application consomme
- **Erreurs** : combien d'erreurs se sont produites
- **Connexions actives** : combien d'utilisateurs sont connectés simultanément

Ces métriques vous aident à répondre à des questions cruciales :
- Mon application est-elle rapide ?
- Y a-t-il des erreurs ?
- L'application utilise-t-elle trop de ressources ?
- Y a-t-il des tendances inquiétantes ?

## Architecture de Prometheus

### Composants principaux

```
┌─────────────────┐
│  Application    │  ← Votre programme FreePascal
│  FreePascal     │     expose des métriques
└────────┬────────┘
         │
         │ HTTP GET /metrics
         │
┌────────▼────────┐
│   Prometheus    │  ← Collecte les métriques
│     Server      │     toutes les X secondes
└────────┬────────┘
         │
         │ Requêtes PromQL
         │
┌────────▼────────┐
│    Grafana      │  ← Affiche de beaux graphiques
│  (dashboard)    │     et tableaux de bord
└─────────────────┘
```

**Le principe est simple :**
1. Votre application expose un endpoint HTTP (généralement `/metrics`)
2. Prometheus interroge cet endpoint régulièrement (scraping)
3. Les données sont stockées dans la base de données Prometheus
4. Grafana (ou autre outil) affiche les données sous forme de graphiques

## Installation de Prometheus

### Installation sur Ubuntu

```bash
# Télécharger la dernière version
cd /tmp
wget https://github.com/prometheus/prometheus/releases/download/v2.45.0/prometheus-2.45.0.linux-amd64.tar.gz

# Extraire
tar xvfz prometheus-2.45.0.linux-amd64.tar.gz

# Déplacer dans /opt
sudo mv prometheus-2.45.0.linux-amd64 /opt/prometheus

# Créer un utilisateur système
sudo useradd --no-create-home --shell /bin/false prometheus

# Créer les répertoires nécessaires
sudo mkdir -p /etc/prometheus
sudo mkdir -p /var/lib/prometheus

# Copier les fichiers de configuration
sudo cp /opt/prometheus/prometheus.yml /etc/prometheus/

# Définir les permissions
sudo chown -R prometheus:prometheus /opt/prometheus
sudo chown -R prometheus:prometheus /etc/prometheus
sudo chown -R prometheus:prometheus /var/lib/prometheus
```

**Créer un service systemd** (`/etc/systemd/system/prometheus.service`) :

```ini
[Unit]
Description=Prometheus
Wants=network-online.target
After=network-online.target

[Service]
User=prometheus
Group=prometheus
Type=simple
ExecStart=/opt/prometheus/prometheus \
    --config.file=/etc/prometheus/prometheus.yml \
    --storage.tsdb.path=/var/lib/prometheus/ \
    --web.console.templates=/opt/prometheus/consoles \
    --web.console.libraries=/opt/prometheus/console_libraries

[Install]
WantedBy=multi-user.target
```

**Démarrer Prometheus** :

```bash
sudo systemctl daemon-reload
sudo systemctl start prometheus
sudo systemctl enable prometheus

# Vérifier le statut
sudo systemctl status prometheus
```

Prometheus est maintenant accessible sur `http://localhost:9090`

### Installation sur Windows

1. **Télécharger** depuis [https://prometheus.io/download/](https://prometheus.io/download/)
   - Choisissez la version Windows (fichier .zip)

2. **Extraire** le fichier ZIP dans `C:\prometheus\`

3. **Créer un fichier de configuration** `C:\prometheus\prometheus.yml` :

```yaml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']
```

4. **Créer un service Windows** (optionnel) :

Téléchargez **NSSM** (Non-Sucking Service Manager) :
```
https://nssm.cc/download
```

Installez Prometheus comme service :
```cmd
nssm install Prometheus "C:\prometheus\prometheus.exe"
nssm set Prometheus AppParameters "--config.file=C:\prometheus\prometheus.yml"
nssm set Prometheus AppDirectory "C:\prometheus"
nssm start Prometheus
```

5. **Ou exécutez directement** :
```cmd
cd C:\prometheus
prometheus.exe --config.file=prometheus.yml
```

Prometheus est accessible sur `http://localhost:9090`

## Types de métriques Prometheus

Prometheus supporte quatre types de métriques :

### 1. Counter (Compteur)

Un compteur ne peut qu'augmenter. Il représente une valeur cumulative.

**Exemples :**
- Nombre total de requêtes traitées
- Nombre total d'erreurs
- Nombre de bytes envoyés

```pascal
// Pseudo-code
RequetesTotal := RequetesTotal + 1;
```

### 2. Gauge (Jauge)

Une jauge peut augmenter et diminuer. Elle représente une valeur instantanée.

**Exemples :**
- Utilisation CPU actuelle
- Nombre de connexions actives
- Température

```pascal
// Pseudo-code
ConnexionsActives := 42;  // Peut monter ou descendre
```

### 3. Histogram (Histogramme)

Un histogramme échantillonne des observations et les compte dans des buckets configurables.

**Exemples :**
- Durées de requêtes
- Tailles de réponses

### 4. Summary (Résumé)

Similaire à l'histogramme, mais calcule des quantiles configurables.

## Exposer des métriques depuis FreePascal

### Format d'exposition Prometheus

Prometheus attend un format texte simple sur un endpoint HTTP :

```
# HELP nom_metrique Description de la métrique
# TYPE nom_metrique type
nom_metrique{label1="valeur1",label2="valeur2"} valeur timestamp

# Exemple concret :
# HELP http_requests_total Nombre total de requêtes HTTP
# TYPE http_requests_total counter
http_requests_total{method="GET",endpoint="/api/users"} 1234
http_requests_total{method="POST",endpoint="/api/users"} 567
```

### Créer un serveur HTTP simple

Nous allons créer un serveur HTTP simple qui expose nos métriques :

```pascal
program MonitoringServer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpserver;

type
  { TMetricsHandler }
  TMetricsHandler = class
  public
    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  end;

var
  Server: TFPHTTPServer;
  Handler: TMetricsHandler;
  // Nos métriques
  RequetesTotal: Int64 = 0;
  ErreursTotal: Int64 = 0;
  ConnexionsActives: Integer = 0;

{ TMetricsHandler }

procedure TMetricsHandler.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Metriques: TStringList;
begin
  if ARequest.URI = '/metrics' then
  begin
    Metriques := TStringList.Create;
    try
      // En-têtes
      Metriques.Add('# HELP http_requests_total Nombre total de requêtes HTTP');
      Metriques.Add('# TYPE http_requests_total counter');
      Metriques.Add(Format('http_requests_total %d', [RequetesTotal]));
      Metriques.Add('');

      Metriques.Add('# HELP http_errors_total Nombre total d''erreurs');
      Metriques.Add('# TYPE http_errors_total counter');
      Metriques.Add(Format('http_errors_total %d', [ErreursTotal]));
      Metriques.Add('');

      Metriques.Add('# HELP connections_active Connexions actives');
      Metriques.Add('# TYPE connections_active gauge');
      Metriques.Add(Format('connections_active %d', [ConnexionsActives]));

      AResponse.ContentType := 'text/plain; version=0.0.4; charset=utf-8';
      AResponse.Content := Metriques.Text;
      AResponse.Code := 200;
    finally
      Metriques.Free;
    end;
  end
  else
  begin
    AResponse.Content := 'Endpoint non trouvé. Utilisez /metrics';
    AResponse.Code := 404;
  end;

  // Incrémenter le compteur de requêtes
  Inc(RequetesTotal);
end;

begin
  Handler := TMetricsHandler.Create;
  Server := TFPHTTPServer.Create(nil);
  try
    Server.Port := 8080;
    Server.OnRequest := @Handler.HandleRequest;

    WriteLn('Serveur de métriques démarré sur http://localhost:8080');
    WriteLn('Métriques disponibles sur http://localhost:8080/metrics');
    WriteLn('Appuyez sur Entrée pour arrêter...');

    Server.Active := True;
    ReadLn;
  finally
    Server.Free;
    Handler.Free;
  end;
end.
```

### Version plus complète avec classe

Créons une classe réutilisable pour gérer nos métriques :

```pascal
unit PrometheusMetrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  { TPrometheusCounter }
  TPrometheusCounter = class
  private
    FName: string;
    FHelp: string;
    FValue: Int64;
    FLock: TCriticalSection;
  public
    constructor Create(const AName, AHelp: string);
    destructor Destroy; override;
    procedure Inc(AAmount: Int64 = 1);
    function GetValue: Int64;
    function AsPrometheusFormat: string;
    property Name: string read FName;
  end;

  { TPrometheusGauge }
  TPrometheusGauge = class
  private
    FName: string;
    FHelp: string;
    FValue: Double;
    FLock: TCriticalSection;
  public
    constructor Create(const AName, AHelp: string);
    destructor Destroy; override;
    procedure SetValue(AValue: Double);
    procedure Inc(AAmount: Double = 1);
    procedure Dec(AAmount: Double = 1);
    function GetValue: Double;
    function AsPrometheusFormat: string;
    property Name: string read FName;
  end;

  { TPrometheusRegistry }
  TPrometheusRegistry = class
  private
    FCounters: TList;
    FGauges: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterCounter(const AName, AHelp: string): TPrometheusCounter;
    function RegisterGauge(const AName, AHelp: string): TPrometheusGauge;
    function GetMetrics: string;
  end;

var
  DefaultRegistry: TPrometheusRegistry;

implementation

{ TPrometheusCounter }

constructor TPrometheusCounter.Create(const AName, AHelp: string);
begin
  FName := AName;
  FHelp := AHelp;
  FValue := 0;
  FLock := TCriticalSection.Create;
end;

destructor TPrometheusCounter.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TPrometheusCounter.Inc(AAmount: Int64);
begin
  FLock.Acquire;
  try
    FValue := FValue + AAmount;
  finally
    FLock.Release;
  end;
end;

function TPrometheusCounter.GetValue: Int64;
begin
  FLock.Acquire;
  try
    Result := FValue;
  finally
    FLock.Release;
  end;
end;

function TPrometheusCounter.AsPrometheusFormat: string;
begin
  Result := Format('# HELP %s %s'#10, [FName, FHelp]) +
            Format('# TYPE %s counter'#10, [FName]) +
            Format('%s %d'#10, [FName, GetValue]);
end;

{ TPrometheusGauge }

constructor TPrometheusGauge.Create(const AName, AHelp: string);
begin
  FName := AName;
  FHelp := AHelp;
  FValue := 0;
  FLock := TCriticalSection.Create;
end;

destructor TPrometheusGauge.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TPrometheusGauge.SetValue(AValue: Double);
begin
  FLock.Acquire;
  try
    FValue := AValue;
  finally
    FLock.Release;
  end;
end;

procedure TPrometheusGauge.Inc(AAmount: Double);
begin
  FLock.Acquire;
  try
    FValue := FValue + AAmount;
  finally
    FLock.Release;
  end;
end;

procedure TPrometheusGauge.Dec(AAmount: Double);
begin
  FLock.Acquire;
  try
    FValue := FValue - AAmount;
  finally
    FLock.Release;
  end;
end;

function TPrometheusGauge.GetValue: Double;
begin
  FLock.Acquire;
  try
    Result := FValue;
  finally
    FLock.Release;
  end;
end;

function TPrometheusGauge.AsPrometheusFormat: string;
begin
  Result := Format('# HELP %s %s'#10, [FName, FHelp]) +
            Format('# TYPE %s gauge'#10, [FName]) +
            Format('%s %.2f'#10, [FName, GetValue]);
end;

{ TPrometheusRegistry }

constructor TPrometheusRegistry.Create;
begin
  FCounters := TList.Create;
  FGauges := TList.Create;
end;

destructor TPrometheusRegistry.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCounters.Count - 1 do
    TPrometheusCounter(FCounters[i]).Free;
  FCounters.Free;

  for i := 0 to FGauges.Count - 1 do
    TPrometheusGauge(FGauges[i]).Free;
  FGauges.Free;

  inherited;
end;

function TPrometheusRegistry.RegisterCounter(const AName, AHelp: string): TPrometheusCounter;
begin
  Result := TPrometheusCounter.Create(AName, AHelp);
  FCounters.Add(Result);
end;

function TPrometheusRegistry.RegisterGauge(const AName, AHelp: string): TPrometheusGauge;
begin
  Result := TPrometheusGauge.Create(AName, AHelp);
  FGauges.Add(Result);
end;

function TPrometheusRegistry.GetMetrics: string;
var
  i: Integer;
  Output: TStringList;
begin
  Output := TStringList.Create;
  try
    // Ajouter tous les compteurs
    for i := 0 to FCounters.Count - 1 do
      Output.Add(TPrometheusCounter(FCounters[i]).AsPrometheusFormat);

    // Ajouter toutes les jauges
    for i := 0 to FGauges.Count - 1 do
      Output.Add(TPrometheusGauge(FGauges[i]).AsPrometheusFormat);

    Result := Output.Text;
  finally
    Output.Free;
  end;
end;

initialization
  DefaultRegistry := TPrometheusRegistry.Create;

finalization
  DefaultRegistry.Free;

end.
```

### Utilisation de la classe

```pascal
program ExempleMetriques;

{$mode objfpc}{$H+}

uses
  SysUtils, PrometheusMetrics;

var
  RequetesHTTP: TPrometheusCounter;
  MemoryUsage: TPrometheusGauge;
  i: Integer;
begin
  // Enregistrer des métriques
  RequetesHTTP := DefaultRegistry.RegisterCounter(
    'http_requests_total',
    'Nombre total de requêtes HTTP'
  );

  MemoryUsage := DefaultRegistry.RegisterGauge(
    'memory_usage_bytes',
    'Utilisation mémoire en bytes'
  );

  // Simuler quelques requêtes
  for i := 1 to 100 do
  begin
    RequetesHTTP.Inc;
    MemoryUsage.SetValue(Random(1000000));
    Sleep(10);
  end;

  // Afficher les métriques au format Prometheus
  WriteLn(DefaultRegistry.GetMetrics);
end.
```

## Configuration de Prometheus pour scraper votre application

Modifiez le fichier `prometheus.yml` pour ajouter votre application :

```yaml
global:
  scrape_interval: 15s      # Collecte toutes les 15 secondes
  evaluation_interval: 15s   # Évalue les règles toutes les 15 secondes

scrape_configs:
  # Configuration pour Prometheus lui-même
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']

  # Votre application FreePascal
  - job_name: 'mon_application'
    static_configs:
      - targets: ['localhost:8080']
    scrape_interval: 5s      # Collecte plus fréquente
```

Redémarrez Prometheus pour appliquer les changements :

**Ubuntu :**
```bash
sudo systemctl restart prometheus
```

**Windows :**
```cmd
nssm restart Prometheus
```

## Métriques système standard

### Métriques d'utilisation des ressources

Voici comment collecter des métriques système de base :

```pascal
unit SystemMetrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PrometheusMetrics
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix, BaseUnix
  {$ENDIF};

type
  { TSystemMetrics }
  TSystemMetrics = class
  private
    FMemoryUsage: TPrometheusGauge;
    FCPUUsage: TPrometheusGauge;
    FThreadCount: TPrometheusGauge;
  public
    constructor Create;
    procedure Update;
  end;

implementation

{ TSystemMetrics }

constructor TSystemMetrics.Create;
begin
  FMemoryUsage := DefaultRegistry.RegisterGauge(
    'process_memory_bytes',
    'Utilisation mémoire du processus en bytes'
  );

  FCPUUsage := DefaultRegistry.RegisterGauge(
    'process_cpu_usage_percent',
    'Utilisation CPU du processus en pourcentage'
  );

  FThreadCount := DefaultRegistry.RegisterGauge(
    'process_threads',
    'Nombre de threads actifs'
  );
end;

procedure TSystemMetrics.Update;
{$IFDEF WINDOWS}
var
  MemCounters: TProcessMemoryCounters;
  SystemInfo: TSystemInfo;
{$ENDIF}
{$IFDEF UNIX}
var
  StatusFile: TextFile;
  Line: string;
  MemValue: Int64;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Mémoire Windows
  MemCounters.cb := SizeOf(MemCounters);
  if GetProcessMemoryInfo(GetCurrentProcess, @MemCounters, SizeOf(MemCounters)) then
    FMemoryUsage.SetValue(MemCounters.WorkingSetSize);

  // Nombre de threads (approximation)
  GetSystemInfo(SystemInfo);
  FThreadCount.SetValue(SystemInfo.dwNumberOfProcessors);
  {$ENDIF}

  {$IFDEF UNIX}
  // Mémoire Linux via /proc/self/status
  try
    AssignFile(StatusFile, '/proc/self/status');
    Reset(StatusFile);
    while not Eof(StatusFile) do
    begin
      ReadLn(StatusFile, Line);
      if Pos('VmRSS:', Line) = 1 then
      begin
        Delete(Line, 1, 6); // Enlever "VmRSS:"
        Line := Trim(Line);
        Delete(Line, Pos(' ', Line), Length(Line)); // Enlever " kB"
        MemValue := StrToInt64(Line) * 1024; // Convertir kB en bytes
        FMemoryUsage.SetValue(MemValue);
        Break;
      end;
    end;
    CloseFile(StatusFile);
  except
    // En cas d'erreur, ne rien faire
  end;
  {$ENDIF}
end;

end.
```

## Métriques métier personnalisées

### Exemple : Application de gestion de base de données

```pascal
program DatabaseMonitoring;

{$mode objfpc}{$H+}

uses
  SysUtils, PrometheusMetrics;

var
  // Métriques métier
  RequetesSQL: TPrometheusCounter;
  RequetesSQLDuration: TPrometheusGauge;
  ConnexionsDB: TPrometheusGauge;
  ErreursSQL: TPrometheusCounter;

procedure ExecuterRequeteSQL(const SQL: string);
var
  Debut, Fin: TDateTime;
  DureeMS: Double;
begin
  Debut := Now;

  // Incrémenter les connexions actives
  ConnexionsDB.Inc;

  try
    // Simuler l'exécution d'une requête
    Sleep(Random(100));

    // Si simulation d'erreur aléatoire
    if Random(100) < 5 then  // 5% d'erreurs
    begin
      ErreursSQL.Inc;
      raise Exception.Create('Erreur SQL simulée');
    end;

    // Incrémenter le compteur de requêtes réussies
    RequetesSQL.Inc;

  finally
    // Décrémenter les connexions actives
    ConnexionsDB.Dec;

    // Calculer et enregistrer la durée
    Fin := Now;
    DureeMS := (Fin - Debut) * 24 * 60 * 60 * 1000; // Convertir en ms
    RequetesSQLDuration.SetValue(DureeMS);
  end;
end;

var
  i: Integer;
begin
  // Enregistrer les métriques
  RequetesSQL := DefaultRegistry.RegisterCounter(
    'sql_queries_total',
    'Nombre total de requêtes SQL exécutées'
  );

  RequetesSQLDuration := DefaultRegistry.RegisterGauge(
    'sql_query_duration_ms',
    'Durée de la dernière requête SQL en millisecondes'
  );

  ConnexionsDB := DefaultRegistry.RegisterGauge(
    'db_connections_active',
    'Nombre de connexions à la base de données actives'
  );

  ErreursSQL := DefaultRegistry.RegisterCounter(
    'sql_errors_total',
    'Nombre total d''erreurs SQL'
  );

  // Simuler des requêtes
  Randomize;
  for i := 1 to 1000 do
  begin
    try
      ExecuterRequeteSQL('SELECT * FROM users');
    except
      on E: Exception do
        WriteLn('Erreur: ', E.Message);
    end;
  end;

  WriteLn('Métriques finales:');
  WriteLn(DefaultRegistry.GetMetrics);
end.
```

## Visualisation avec Grafana

### Installation de Grafana

**Ubuntu :**
```bash
# Ajouter le dépôt Grafana
sudo apt-get install -y software-properties-common
sudo add-apt-repository "deb https://packages.grafana.com/oss/deb stable main"
wget -q -O - https://packages.grafana.com/gpg.key | sudo apt-key add -

# Installer
sudo apt-get update
sudo apt-get install grafana

# Démarrer le service
sudo systemctl start grafana-server
sudo systemctl enable grafana-server
```

**Windows :**
1. Télécharger depuis [https://grafana.com/grafana/download](https://grafana.com/grafana/download)
2. Extraire le fichier ZIP dans `C:\grafana\`
3. Exécuter `C:\grafana\bin\grafana-server.exe`

Grafana est accessible sur `http://localhost:3000`
- Login par défaut : `admin`
- Password par défaut : `admin`

### Configurer Prometheus comme source de données

1. Connectez-vous à Grafana
2. Cliquez sur **Configuration** (icône engrenage) → **Data Sources**
3. Cliquez sur **Add data source**
4. Sélectionnez **Prometheus**
5. Configurez l'URL : `http://localhost:9090`
6. Cliquez sur **Save & Test**

### Créer un dashboard simple

1. Cliquez sur **+** → **Dashboard**
2. Cliquez sur **Add new panel**
3. Dans le champ **Query**, entrez une requête PromQL, par exemple :
   ```
   rate(http_requests_total[5m])
   ```
4. Configurez le titre du panneau : "Requêtes HTTP par seconde"
5. Cliquez sur **Apply**

### Exemples de requêtes PromQL utiles

```promql
# Taux de requêtes par seconde sur les 5 dernières minutes
rate(http_requests_total[5m])

# Nombre de connexions actives
connections_active

# Taux d'erreurs en pourcentage
(rate(http_errors_total[5m]) / rate(http_requests_total[5m])) * 100

# Utilisation mémoire en MB
process_memory_bytes / 1024 / 1024

# 95e percentile du temps de réponse
histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m]))
```

## Alerting avec Prometheus

### Configuration d'une alerte simple

Créez un fichier `/etc/prometheus/alerts.yml` :

```yaml
groups:
  - name: mon_application
    interval: 30s
    rules:
      # Alerte si le taux d'erreurs dépasse 5%
      - alert: TauxErreursEleve
        expr: (rate(http_errors_total[5m]) / rate(http_requests_total[5m])) * 100 > 5
        for: 2m
        labels:
          severity: warning
        annotations:
          summary: "Taux d'erreurs élevé"
          description: "Le taux d'erreurs est de {{ $value }}%"

      # Alerte si l'utilisation mémoire dépasse 1GB
      - alert: MemoryUsageHigh
        expr: process_memory_bytes > 1073741824
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: "Utilisation mémoire élevée"
          description: "L'application utilise {{ $value | humanize }} de mémoire"

      # Alerte si l'application ne répond plus
      - alert: ApplicationDown
        expr: up{job="mon_application"} == 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "Application hors ligne"
          description: "L'application ne répond plus depuis 1 minute"
```

Modifiez `prometheus.yml` pour charger les alertes :

```yaml
global:
  scrape_interval: 15s

# Charger les règles d'alertes
rule_files:
  - "alerts.yml"

scrape_configs:
  - job_name: 'mon_application'
    static_configs:
      - targets: ['localhost:8080']
```

## Bonnes pratiques

### 1. Nommage des métriques

Suivez les conventions Prometheus :

- Utilisez le **snake_case** : `http_requests_total` (pas `httpRequestsTotal`)
- Suffixe pour les unités :
  - `_total` pour les compteurs
  - `_bytes` pour les tailles
  - `_seconds` pour les durées
  - `_ratio` pour les ratios (0-1)
- Préfixe par domaine : `myapp_http_requests_total`

### 2. Labels pertinents

Ajoutez des labels pour segmenter vos métriques :

```
http_requests_total{method="GET", endpoint="/api/users", status="200"} 1234
http_requests_total{method="POST", endpoint="/api/users", status="201"} 567
http_requests_total{method="GET", endpoint="/api/products", status="200"} 890
```

**Attention :** trop de labels différents créent une **explosion de cardinalité** et surchargeront Prometheus.

**Exemple de mauvaise pratique :**
```
# NE PAS FAIRE : utiliser l'ID utilisateur comme label
http_requests_total{user_id="12345"} 1
http_requests_total{user_id="67890"} 1
# Cela créera des millions de séries temporelles !
```

**Bonne pratique :**
```
# Utiliser des catégories limitées
http_requests_total{user_type="premium"} 5678
http_requests_total{user_type="free"} 12345
```

### 3. Métriques essentielles (les 4 Golden Signals)

Google SRE (Site Reliability Engineering) recommande de surveiller quatre métriques fondamentales pour tout service :

#### 1. Latence (Latency)

Combien de temps prend une requête pour être traitée ?

```pascal
var
  RequestDuration: TPrometheusGauge;

procedure TraiterRequete;
var
  Debut, Fin: TDateTime;
  DureeSecondes: Double;
begin
  Debut := Now;
  try
    // Traiter la requête
    DoWork();
  finally
    Fin := Now;
    DureeSecondes := (Fin - Debut) * 24 * 60 * 60;
    RequestDuration.SetValue(DureeSecondes);
  end;
end;
```

#### 2. Trafic (Traffic)

Combien de requêtes le système reçoit-il ?

```pascal
var
  RequestsTotal: TPrometheusCounter;

procedure OnNouvelleRequete;
begin
  RequestsTotal.Inc;
  // Traiter la requête...
end;
```

#### 3. Erreurs (Errors)

Quel pourcentage de requêtes échoue ?

```pascal
var
  ErrorsTotal: TPrometheusCounter;

procedure TraiterRequete;
begin
  try
    // Traiter la requête
    DoWork();
  except
    on E: Exception do
    begin
      ErrorsTotal.Inc;
      raise;
    end;
  end;
end;
```

#### 4. Saturation (Saturation)

À quel point le système est-il "plein" (CPU, mémoire, disque, connexions) ?

```pascal
var
  ConnectionPoolUsage: TPrometheusGauge;
  MaxConnections: Integer = 100;
  ActiveConnections: Integer = 0;

procedure UpdateSaturation;
var
  UsagePercent: Double;
begin
  UsagePercent := (ActiveConnections / MaxConnections) * 100;
  ConnectionPoolUsage.SetValue(UsagePercent);
end;
```

### 4. Granularité des métriques

- **Ne pas** collecter toutes les 100ms : c'est trop !
- **Recommandé** : 15 secondes à 1 minute pour la plupart des métriques
- **Métriques critiques** : 5-10 secondes
- **Métriques de debug** : 1 minute ou plus

### 5. Rétention des données

Configurez la rétention dans Prometheus (`prometheus.yml`) :

```yaml
global:
  scrape_interval: 15s

storage:
  tsdb:
    # Conserver 30 jours de données
    retention.time: 30d
    # Ou limiter par taille
    retention.size: 50GB
```

## Implémentation avancée : Histogrammes

Les histogrammes permettent de mesurer la distribution des valeurs (par exemple, temps de réponse).

### Classe d'histogramme

```pascal
unit PrometheusHistogram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  { TPrometheusHistogram }
  TPrometheusHistogram = class
  private
    FName: string;
    FHelp: string;
    FBuckets: array of Double;  // Limites des buckets
    FCounts: array of Int64;    // Compteurs par bucket
    FSum: Double;               // Somme totale
    FCount: Int64;              // Nombre total d'observations
    FLock: TCriticalSection;
  public
    constructor Create(const AName, AHelp: string; const ABuckets: array of Double);
    destructor Destroy; override;
    procedure Observe(AValue: Double);
    function AsPrometheusFormat: string;
  end;

implementation

{ TPrometheusHistogram }

constructor TPrometheusHistogram.Create(const AName, AHelp: string;
  const ABuckets: array of Double);
var
  i: Integer;
begin
  FName := AName;
  FHelp := AHelp;
  FSum := 0;
  FCount := 0;
  FLock := TCriticalSection.Create;

  // Copier les buckets
  SetLength(FBuckets, Length(ABuckets));
  SetLength(FCounts, Length(ABuckets) + 1); // +1 pour le bucket +Inf

  for i := 0 to High(ABuckets) do
  begin
    FBuckets[i] := ABuckets[i];
    FCounts[i] := 0;
  end;
  FCounts[High(FCounts)] := 0; // Bucket +Inf
end;

destructor TPrometheusHistogram.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TPrometheusHistogram.Observe(AValue: Double);
var
  i: Integer;
begin
  FLock.Acquire;
  try
    // Incrémenter la somme et le compteur total
    FSum := FSum + AValue;
    Inc(FCount);

    // Trouver le bucket approprié
    for i := 0 to High(FBuckets) do
    begin
      if AValue <= FBuckets[i] then
      begin
        Inc(FCounts[i]);
        Break;
      end;
    end;

    // Si la valeur dépasse tous les buckets, incrémenter +Inf
    if AValue > FBuckets[High(FBuckets)] then
      Inc(FCounts[High(FCounts)]);

  finally
    FLock.Release;
  end;
end;

function TPrometheusHistogram.AsPrometheusFormat: string;
var
  i: Integer;
  Output: TStringList;
  CumulativeCount: Int64;
begin
  Output := TStringList.Create;
  try
    FLock.Acquire;
    try
      // En-tête
      Output.Add(Format('# HELP %s %s', [FName, FHelp]));
      Output.Add(Format('# TYPE %s histogram', [FName]));

      // Buckets cumulatifs
      CumulativeCount := 0;
      for i := 0 to High(FBuckets) do
      begin
        CumulativeCount := CumulativeCount + FCounts[i];
        Output.Add(Format('%s_bucket{le="%.2f"} %d',
          [FName, FBuckets[i], CumulativeCount]));
      end;

      // Bucket +Inf
      CumulativeCount := CumulativeCount + FCounts[High(FCounts)];
      Output.Add(Format('%s_bucket{le="+Inf"} %d', [FName, CumulativeCount]));

      // Somme et compteur
      Output.Add(Format('%s_sum %.2f', [FName, FSum]));
      Output.Add(Format('%s_count %d', [FName, FCount]));

      Result := Output.Text;
    finally
      FLock.Release;
    end;
  finally
    Output.Free;
  end;
end;

end.
```

### Utilisation de l'histogramme

```pascal
program ExempleHistogramme;

{$mode objfpc}{$H+}

uses
  SysUtils, PrometheusHistogram;

var
  ResponseTime: TPrometheusHistogram;
  i: Integer;
  Temps: Double;
begin
  // Créer un histogramme avec des buckets en secondes
  ResponseTime := TPrometheusHistogram.Create(
    'http_request_duration_seconds',
    'Durée des requêtes HTTP en secondes',
    [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]
  );

  try
    // Simuler des requêtes avec temps de réponse aléatoires
    Randomize;
    for i := 1 to 1000 do
    begin
      Temps := Random * 5; // 0 à 5 secondes
      ResponseTime.Observe(Temps);
    end;

    // Afficher les métriques
    WriteLn(ResponseTime.AsPrometheusFormat);
  finally
    ResponseTime.Free;
  end;
end.
```

## Application complète avec serveur HTTP

Voici une application complète qui expose des métriques via HTTP :

```pascal
program MetricsServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, fphttpserver, PrometheusMetrics, SystemMetrics;

type
  { TMetricsHTTPServer }
  TMetricsHTTPServer = class
  private
    FServer: TFPHTTPServer;
    FSystemMetrics: TSystemMetrics;
    FRequestCounter: TPrometheusCounter;
    FMetricsRequestCounter: TPrometheusCounter;
    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(APort: Word);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

{ TMetricsHTTPServer }

constructor TMetricsHTTPServer.Create(APort: Word);
begin
  // Initialiser les métriques
  FSystemMetrics := TSystemMetrics.Create;

  FRequestCounter := DefaultRegistry.RegisterCounter(
    'http_requests_total',
    'Nombre total de requêtes HTTP reçues'
  );

  FMetricsRequestCounter := DefaultRegistry.RegisterCounter(
    'metrics_endpoint_requests_total',
    'Nombre de fois que l''endpoint /metrics a été appelé'
  );

  // Créer le serveur HTTP
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := APort;
  FServer.OnRequest := @HandleRequest;
end;

destructor TMetricsHTTPServer.Destroy;
begin
  FServer.Free;
  FSystemMetrics.Free;
  inherited;
end;

procedure TMetricsHTTPServer.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  // Incrémenter le compteur de requêtes
  FRequestCounter.Inc;

  if ARequest.URI = '/metrics' then
  begin
    // Mettre à jour les métriques système
    FSystemMetrics.Update;

    // Incrémenter le compteur spécifique
    FMetricsRequestCounter.Inc;

    // Renvoyer les métriques
    AResponse.ContentType := 'text/plain; version=0.0.4; charset=utf-8';
    AResponse.Content := DefaultRegistry.GetMetrics;
    AResponse.Code := 200;
  end
  else if ARequest.URI = '/' then
  begin
    AResponse.ContentType := 'text/html; charset=utf-8';
    AResponse.Content :=
      '<html><body>' +
      '<h1>Serveur de Métriques Prometheus</h1>' +
      '<p>Les métriques sont disponibles sur <a href="/metrics">/metrics</a></p>' +
      '</body></html>';
    AResponse.Code := 200;
  end
  else
  begin
    AResponse.Content := 'Endpoint non trouvé';
    AResponse.Code := 404;
  end;
end;

procedure TMetricsHTTPServer.Start;
begin
  FServer.Active := True;
  WriteLn('Serveur de métriques démarré sur http://localhost:', FServer.Port);
  WriteLn('Métriques disponibles sur http://localhost:', FServer.Port, '/metrics');
end;

procedure TMetricsHTTPServer.Stop;
begin
  FServer.Active := False;
  WriteLn('Serveur arrêté');
end;

var
  Server: TMetricsHTTPServer;
begin
  Server := TMetricsHTTPServer.Create(8080);
  try
    Server.Start;
    WriteLn('Appuyez sur Entrée pour arrêter le serveur...');
    ReadLn;
    Server.Stop;
  finally
    Server.Free;
  end;
end.
```

## Métriques pour applications GUI

Pour les applications avec interface graphique (Lazarus LCL), vous pouvez surveiller :

```pascal
unit FormMetrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, PrometheusMetrics;

type
  { TFormMetrics }
  TFormMetrics = class
  private
    FFormOpenCount: TPrometheusCounter;
    FFormCloseCount: TPrometheusCounter;
    FButtonClickCount: TPrometheusCounter;
    FActiveFormsGauge: TPrometheusGauge;
  public
    constructor Create;
    procedure OnFormCreate(AForm: TForm);
    procedure OnFormClose(AForm: TForm);
    procedure OnButtonClick(const ButtonName: string);
    procedure UpdateActiveForms;
  end;

var
  FormMetrics: TFormMetrics;

implementation

{ TFormMetrics }

constructor TFormMetrics.Create;
begin
  FFormOpenCount := DefaultRegistry.RegisterCounter(
    'gui_forms_opened_total',
    'Nombre total de formulaires ouverts'
  );

  FFormCloseCount := DefaultRegistry.RegisterCounter(
    'gui_forms_closed_total',
    'Nombre total de formulaires fermés'
  );

  FButtonClickCount := DefaultRegistry.RegisterCounter(
    'gui_button_clicks_total',
    'Nombre total de clics sur les boutons'
  );

  FActiveFormsGauge := DefaultRegistry.RegisterGauge(
    'gui_active_forms',
    'Nombre de formulaires actuellement actifs'
  );
end;

procedure TFormMetrics.OnFormCreate(AForm: TForm);
begin
  FFormOpenCount.Inc;
  UpdateActiveForms;
end;

procedure TFormMetrics.OnFormClose(AForm: TForm);
begin
  FFormCloseCount.Inc;
  UpdateActiveForms;
end;

procedure TFormMetrics.OnButtonClick(const ButtonName: string);
begin
  FButtonClickCount.Inc;
end;

procedure TFormMetrics.UpdateActiveForms;
begin
  FActiveFormsGauge.SetValue(Screen.FormCount);
end;

initialization
  FormMetrics := TFormMetrics.Create;

finalization
  FormMetrics.Free;

end.
```

## Métriques pour applications de base de données

```pascal
unit DatabaseMetrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, PrometheusMetrics;

type
  { TDatabaseMetrics }
  TDatabaseMetrics = class
  private
    FQueriesExecuted: TPrometheusCounter;
    FQueriesFailed: TPrometheusCounter;
    FQueryDuration: TPrometheusGauge;
    FActiveConnections: TPrometheusGauge;
    FRowsReturned: TPrometheusCounter;
  public
    constructor Create;
    procedure OnQueryExecute(AQuery: TSQLQuery; StartTime: TDateTime);
    procedure OnQueryError(AQuery: TSQLQuery);
    procedure OnConnectionOpen;
    procedure OnConnectionClose;
  end;

implementation

{ TDatabaseMetrics }

constructor TDatabaseMetrics.Create;
begin
  FQueriesExecuted := DefaultRegistry.RegisterCounter(
    'db_queries_executed_total',
    'Nombre total de requêtes SQL exécutées'
  );

  FQueriesFailed := DefaultRegistry.RegisterCounter(
    'db_queries_failed_total',
    'Nombre total de requêtes SQL échouées'
  );

  FQueryDuration := DefaultRegistry.RegisterGauge(
    'db_query_duration_seconds',
    'Durée de la dernière requête en secondes'
  );

  FActiveConnections := DefaultRegistry.RegisterGauge(
    'db_connections_active',
    'Nombre de connexions à la base de données actives'
  );

  FRowsReturned := DefaultRegistry.RegisterCounter(
    'db_rows_returned_total',
    'Nombre total de lignes retournées'
  );
end;

procedure TDatabaseMetrics.OnQueryExecute(AQuery: TSQLQuery; StartTime: TDateTime);
var
  Duration: Double;
begin
  FQueriesExecuted.Inc;

  // Calculer la durée
  Duration := (Now - StartTime) * 24 * 60 * 60; // En secondes
  FQueryDuration.SetValue(Duration);

  // Compter les lignes retournées
  if Assigned(AQuery) and AQuery.Active then
    FRowsReturned.Inc(AQuery.RecordCount);
end;

procedure TDatabaseMetrics.OnQueryError(AQuery: TSQLQuery);
begin
  FQueriesFailed.Inc;
end;

procedure TDatabaseMetrics.OnConnectionOpen;
begin
  FActiveConnections.Inc;
end;

procedure TDatabaseMetrics.OnConnectionClose;
begin
  FActiveConnections.Dec;
end;

end.
```

## Surveillance multi-instance

Si vous avez plusieurs instances de votre application sur différentes machines :

### Configuration Prometheus pour plusieurs cibles

```yaml
scrape_configs:
  - job_name: 'mon_application'
    static_configs:
      # Instance Windows
      - targets: ['192.168.1.10:8080']
        labels:
          instance: 'windows-prod-1'
          environment: 'production'
          os: 'windows'

      # Instance Ubuntu
      - targets: ['192.168.1.20:8080']
        labels:
          instance: 'ubuntu-prod-1'
          environment: 'production'
          os: 'linux'

      # Instance de test
      - targets: ['192.168.1.30:8080']
        labels:
          instance: 'test-1'
          environment: 'staging'
          os: 'linux'
```

### Requêtes PromQL multi-instances

```promql
# Total des requêtes sur toutes les instances
sum(rate(http_requests_total[5m]))

# Requêtes par instance
sum(rate(http_requests_total[5m])) by (instance)

# Requêtes par système d'exploitation
sum(rate(http_requests_total[5m])) by (os)

# Comparer production vs staging
sum(rate(http_requests_total[5m])) by (environment)
```

## Sécurité et authentification

### Authentification basique pour l'endpoint /metrics

```pascal
procedure TMetricsHTTPServer.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  AuthHeader: string;
  Credentials: string;
  Username, Password: string;
  Pos: Integer;
begin
  if ARequest.URI = '/metrics' then
  begin
    // Vérifier l'authentification
    AuthHeader := ARequest.GetHeader('Authorization');

    if AuthHeader = '' then
    begin
      AResponse.SetHeader('WWW-Authenticate', 'Basic realm="Metrics"');
      AResponse.Code := 401;
      AResponse.Content := 'Authentification requise';
      Exit;
    end;

    // Décoder le Basic Auth (simplifiée)
    if Copy(AuthHeader, 1, 6) = 'Basic ' then
    begin
      Credentials := DecodeStringBase64(Copy(AuthHeader, 7, Length(AuthHeader)));
      Pos := System.Pos(':', Credentials);
      if Pos > 0 then
      begin
        Username := Copy(Credentials, 1, Pos - 1);
        Password := Copy(Credentials, Pos + 1, Length(Credentials));

        // Vérifier les credentials
        if (Username = 'prometheus') and (Password = 'secret_password') then
        begin
          // OK, continuer
          FSystemMetrics.Update;
          AResponse.ContentType := 'text/plain; version=0.0.4';
          AResponse.Content := DefaultRegistry.GetMetrics;
          AResponse.Code := 200;
          Exit;
        end;
      end;
    end;

    // Authentification échouée
    AResponse.Code := 403;
    AResponse.Content := 'Accès refusé';
  end;
end;
```

### Configuration Prometheus avec authentification

```yaml
scrape_configs:
  - job_name: 'mon_application_securisee'
    basic_auth:
      username: 'prometheus'
      password: 'secret_password'
    static_configs:
      - targets: ['localhost:8080']
```

## Optimisation des performances

### 1. Utiliser des pools de métriques

Pour éviter de créer trop d'objets :

```pascal
type
  TMetricsPool = class
  private
    FMetrics: TFPHashList;
  public
    function GetOrCreateCounter(const Name, Help: string): TPrometheusCounter;
  end;
```

### 2. Agrégation locale

Agrégez les métriques localement avant de les exposer :

```pascal
// Au lieu de mettre à jour immédiatement
procedure OnPetiteOperation;
var
  LocalCounter: Integer;
begin
  Inc(LocalCounter);

  // Mise à jour batch toutes les 100 opérations
  if LocalCounter mod 100 = 0 then
  begin
    GlobalCounter.Inc(100);
    LocalCounter := 0;
  end;
end;
```

### 3. Limiter la cardinalité

```pascal
// MAUVAIS : cardinalité infinie
metrics_by_user{user_id="12345"}

// BON : cardinalité limitée
metrics_by_user_type{user_type="premium"}
```

## Debugging et dépannage

### Vérifier que Prometheus scrape correctement

```bash
# Voir les cibles dans Prometheus
http://localhost:9090/targets

# Voir les métriques d'une instance spécifique
curl http://localhost:8080/metrics
```

### Logs Prometheus

**Ubuntu :**
```bash
sudo journalctl -u prometheus -f
```

**Windows :**
Vérifiez les logs dans le répertoire Prometheus ou utilisez :
```cmd
nssm status Prometheus
```

### Problèmes courants

#### 1. "Context deadline exceeded"
**Cause :** Prometheus n'arrive pas à scraper dans le délai imparti  
**Solution :** Augmentez le timeout :  
```yaml
scrape_configs:
  - job_name: 'mon_application'
    scrape_timeout: 30s  # Au lieu de 10s par défaut
    static_configs:
      - targets: ['localhost:8080']
```

#### 2. Métriques manquantes
**Vérifications :**
1. Le serveur de métriques est-il accessible ? `curl http://localhost:8080/metrics`
2. Prometheus est-il configuré correctement ?
3. Y a-t-il des erreurs dans les logs ?

#### 3. Consommation mémoire excessive
**Causes possibles :**
- Trop de métriques uniques (cardinalité élevée)
- Rétention trop longue
- Trop de labels différents

**Solutions :**
- Réduire le nombre de labels
- Diminuer la rétention
- Augmenter la RAM allouée à Prometheus

## Exemple complet : Application de monitoring de serveur de fichiers

Voici un exemple d'application complète qui surveille un serveur de fichiers :

```pascal
program FileServerMonitoring;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, fphttpserver, PrometheusMetrics;

type
  { TFileServerMetrics }
  TFileServerMetrics = class
  private
    FFilesUploaded: TPrometheusCounter;
    FFilesDownloaded: TPrometheusCounter;
    FBytesUploaded: TPrometheusCounter;
    FBytesDownloaded: TPrometheusCounter;
    FActiveTransfers: TPrometheusGauge;
    FDiskSpaceUsed: TPrometheusGauge;
    FErrors: TPrometheusCounter;
  public
    constructor Create;
    procedure OnFileUpload(FileSize: Int64);
    procedure OnFileDownload(FileSize: Int64);
    procedure OnTransferStart;
    procedure OnTransferEnd;
    procedure OnError;
    procedure UpdateDiskSpace(UsedBytes: Int64);
  end;

{ TFileServerMetrics }

constructor TFileServerMetrics.Create;
begin
  FFilesUploaded := DefaultRegistry.RegisterCounter(
    'fileserver_files_uploaded_total',
    'Nombre total de fichiers uploadés'
  );

  FFilesDownloaded := DefaultRegistry.RegisterCounter(
    'fileserver_files_downloaded_total',
    'Nombre total de fichiers téléchargés'
  );

  FBytesUploaded := DefaultRegistry.RegisterCounter(
    'fileserver_bytes_uploaded_total',
    'Nombre total de bytes uploadés'
  );

  FBytesDownloaded := DefaultRegistry.RegisterCounter(
    'fileserver_bytes_downloaded_total',
    'Nombre total de bytes téléchargés'
  );

  FActiveTransfers := DefaultRegistry.RegisterGauge(
    'fileserver_active_transfers',
    'Nombre de transferts actuellement actifs'
  );

  FDiskSpaceUsed := DefaultRegistry.RegisterGauge(
    'fileserver_disk_space_used_bytes',
    'Espace disque utilisé en bytes'
  );

  FErrors := DefaultRegistry.RegisterCounter(
    'fileserver_errors_total',
    'Nombre total d''erreurs'
  );
end;

procedure TFileServerMetrics.OnFileUpload(FileSize: Int64);
begin
  FFilesUploaded.Inc;
  FBytesUploaded.Inc(FileSize);
end;

procedure TFileServerMetrics.OnFileDownload(FileSize: Int64);
begin
  FFilesDownloaded.Inc;
  FBytesDownloaded.Inc(FileSize);
end;

procedure TFileServerMetrics.OnTransferStart;
begin
  FActiveTransfers.Inc;
end;

procedure TFileServerMetrics.OnTransferEnd;
begin
  FActiveTransfers.Dec;
end;

procedure TFileServerMetrics.OnError;
begin
  FErrors.Inc;
end;

procedure TFileServerMetrics.UpdateDiskSpace(UsedBytes: Int64);
begin
  FDiskSpaceUsed.SetValue(UsedBytes);
end;

// Programme principal
var
  Metrics: TFileServerMetrics;
  Server: TFPHTTPServer;

procedure HandleMetrics(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  if ARequest.URI = '/metrics' then
  begin
    AResponse.ContentType := 'text/plain; version=0.0.4';
    AResponse.Content := DefaultRegistry.GetMetrics;
    AResponse.Code := 200;
  end
  else
  begin
    AResponse.Code := 404;
  end;
end;

begin
  Metrics := TFileServerMetrics.Create;
  Server := TFPHTTPServer.Create(nil);
  try
    Server.Port := 9091;
    Server.OnRequest := @HandleMetrics;
    Server.Active := True;

    WriteLn('Serveur de métriques démarré sur http://localhost:9091/metrics');
    WriteLn('Simulation de l''activité du serveur de fichiers...');

    // Simuler l'activité
    while True do
    begin
      // Simuler un upload
      if Random(100) < 30 then
      begin
        Metrics.OnTransferStart;
        Metrics.OnFileUpload(Random(10000000)); // 0-10MB
        Sleep(Random(1000));
        Metrics.OnTransferEnd;
      end;

      // Simuler un download
      if Random(100) < 50 then
      begin
        Metrics.OnTransferStart;
        Metrics.OnFileDownload(Random(5000000)); // 0-5MB
        Sleep(Random(500));
        Metrics.OnTransferEnd;
      end;

      // Simuler une erreur occasionnelle
      if Random(1000) < 5 then
        Metrics.OnError;

      // Mettre à jour l'espace disque
      Metrics.UpdateDiskSpace(Random(1000000000)); // 0-1GB

      Sleep(100);
    end;
  finally
    Server.Free;
    Metrics.Free;
  end;
end.
```

## Conclusion

Le monitoring avec Prometheus est un outil puissant pour garantir la fiabilité et les performances de vos applications FreePascal/Lazarus, que ce soit sur Windows ou Ubuntu.

### Points clés à retenir :

1. **Exposez des métriques pertinentes** : Les 4 Golden Signals (latence, trafic, erreurs, saturation)
2. **Utilisez les bons types** : Counters pour les totaux cumulatifs, Gauges pour les valeurs instantanées
3. **Nommage cohérent** : Suivez les conventions Prometheus (snake_case, suffixes appropriés)
4. **Limitez la cardinalité** : N'utilisez pas de valeurs uniques comme labels
5. **Sécurisez vos endpoints** : Authentification pour /metrics en production
6. **Configurez des alertes** : Soyez proactifs face aux problèmes

### Avantages du monitoring :

- **Détection précoce des problèmes** : Identifiez les anomalies avant qu'elles n'impactent les utilisateurs
- **Capacité de planification** : Anticipez les besoins en ressources
- **Amélioration continue** : Mesurez l'impact de vos optimisations
- **Debugging facilité** : Corrélez les erreurs avec les conditions système
- **Transparence** : Dashboards visuels pour toute l'équipe

## Ressources complémentaires

### Documentation officielle

- **Prometheus** : [https://prometheus.io/docs/](https://prometheus.io/docs/)
- **Grafana** : [https://grafana.com/docs/](https://grafana.com/docs/)
- **PromQL** : [https://prometheus.io/docs/prometheus/latest/querying/basics/](https://prometheus.io/docs/prometheus/latest/querying/basics/)

### Exemples de dashboards Grafana

Grafana propose des dashboards pré-configurés que vous pouvez importer :

1. Connectez-vous à Grafana
2. Menu **+** → **Import**
3. Entrez un ID de dashboard (exemples ci-dessous)
4. Cliquez sur **Load**

**Dashboards utiles :**
- **Node Exporter Full** : ID 1860 (métriques système)
- **Prometheus Stats** : ID 2 (statistiques Prometheus)
- **HTTP Stats** : ID 3662 (métriques HTTP)

### Exporters Prometheus populaires

Si vous voulez surveiller d'autres services :

**Node Exporter** (métriques système) :
```bash
# Ubuntu
sudo apt-get install prometheus-node-exporter

# Windows
# Télécharger depuis github.com/prometheus/node_exporter
```

**Windows Exporter** (spécifique Windows) :
```
https://github.com/prometheus-community/windows_exporter
```

**PostgreSQL Exporter** :
```bash
# Ubuntu
sudo apt-get install prometheus-postgres-exporter
```

**MySQL Exporter** :
```bash
# Télécharger depuis github.com/prometheus/mysqld_exporter
```

## Tableau récapitulatif des métriques recommandées

| Catégorie | Métrique | Type | Description |
|-----------|----------|------|-------------|
| **HTTP** | `http_requests_total` | Counter | Nombre total de requêtes |
| | `http_request_duration_seconds` | Histogram | Durée des requêtes |
| | `http_errors_total` | Counter | Nombre d'erreurs HTTP |
| | `http_requests_in_flight` | Gauge | Requêtes en cours |
| **Base de données** | `db_queries_total` | Counter | Requêtes SQL exécutées |
| | `db_query_duration_seconds` | Histogram | Durée des requêtes SQL |
| | `db_connections_active` | Gauge | Connexions actives |
| | `db_errors_total` | Counter | Erreurs SQL |
| **Système** | `process_memory_bytes` | Gauge | Mémoire utilisée |
| | `process_cpu_seconds_total` | Counter | CPU consommé |
| | `process_threads` | Gauge | Nombre de threads |
| | `process_open_fds` | Gauge | Descripteurs de fichiers ouverts |
| **Application** | `app_started_timestamp` | Gauge | Timestamp de démarrage |
| | `app_uptime_seconds` | Counter | Temps d'exécution |
| | `app_version_info` | Gauge | Information de version |
| | `app_errors_total` | Counter | Erreurs applicatives |

## Script de déploiement complet

### Script Ubuntu pour déploiement automatisé

Créez un fichier `deploy-monitoring.sh` :

```bash
#!/bin/bash

set -e

echo "====================================="
echo "Déploiement du monitoring Prometheus"
echo "====================================="

# Variables
PROMETHEUS_VERSION="2.45.0"
GRAFANA_VERSION="latest"
APP_PORT="8080"
PROMETHEUS_PORT="9090"
GRAFANA_PORT="3000"

# Fonction d'installation de Prometheus
install_prometheus() {
    echo ""
    echo "Installation de Prometheus..."

    cd /tmp
    wget https://github.com/prometheus/prometheus/releases/download/v${PROMETHEUS_VERSION}/prometheus-${PROMETHEUS_VERSION}.linux-amd64.tar.gz
    tar xvfz prometheus-${PROMETHEUS_VERSION}.linux-amd64.tar.gz
    sudo mv prometheus-${PROMETHEUS_VERSION}.linux-amd64 /opt/prometheus

    sudo useradd --no-create-home --shell /bin/false prometheus || true
    sudo mkdir -p /etc/prometheus /var/lib/prometheus

    # Configuration
    cat <<EOF | sudo tee /etc/prometheus/prometheus.yml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:${PROMETHEUS_PORT}']

  - job_name: 'mon_application'
    static_configs:
      - targets: ['localhost:${APP_PORT}']
EOF

    sudo chown -R prometheus:prometheus /opt/prometheus /etc/prometheus /var/lib/prometheus

    # Service systemd
    cat <<EOF | sudo tee /etc/systemd/system/prometheus.service
[Unit]
Description=Prometheus
Wants=network-online.target
After=network-online.target

[Service]
User=prometheus
Group=prometheus
Type=simple
ExecStart=/opt/prometheus/prometheus \\
    --config.file=/etc/prometheus/prometheus.yml \\
    --storage.tsdb.path=/var/lib/prometheus/ \\
    --web.console.templates=/opt/prometheus/consoles \\
    --web.console.libraries=/opt/prometheus/console_libraries

[Install]
WantedBy=multi-user.target
EOF

    sudo systemctl daemon-reload
    sudo systemctl enable prometheus
    sudo systemctl start prometheus

    echo "✓ Prometheus installé et démarré"
}

# Fonction d'installation de Grafana
install_grafana() {
    echo ""
    echo "Installation de Grafana..."

    sudo apt-get install -y software-properties-common
    sudo add-apt-repository "deb https://packages.grafana.com/oss/deb stable main"
    wget -q -O - https://packages.grafana.com/gpg.key | sudo apt-key add -

    sudo apt-get update
    sudo apt-get install -y grafana

    sudo systemctl enable grafana-server
    sudo systemctl start grafana-server

    echo "✓ Grafana installé et démarré"
}

# Fonction d'installation de Node Exporter
install_node_exporter() {
    echo ""
    echo "Installation de Node Exporter..."

    sudo apt-get install -y prometheus-node-exporter

    # Ajouter à la configuration Prometheus
    cat <<EOF | sudo tee -a /etc/prometheus/prometheus.yml

  - job_name: 'node_exporter'
    static_configs:
      - targets: ['localhost:9100']
EOF

    sudo systemctl restart prometheus

    echo "✓ Node Exporter installé"
}

# Vérification des ports
check_ports() {
    echo ""
    echo "Vérification des ports..."

    if netstat -tuln | grep -q ":${PROMETHEUS_PORT}"; then
        echo "✓ Prometheus écoute sur le port ${PROMETHEUS_PORT}"
    else
        echo "✗ Prometheus n'écoute pas sur le port ${PROMETHEUS_PORT}"
    fi

    if netstat -tuln | grep -q ":${GRAFANA_PORT}"; then
        echo "✓ Grafana écoute sur le port ${GRAFANA_PORT}"
    else
        echo "✗ Grafana n'écoute pas sur le port ${GRAFANA_PORT}"
    fi
}

# Menu principal
echo ""
echo "Que souhaitez-vous installer ?"
echo "1) Prometheus seulement"
echo "2) Grafana seulement"
echo "3) Node Exporter seulement"
echo "4) Tout installer"
echo "5) Vérifier l'installation"
echo ""
read -p "Votre choix (1-5): " choice

case $choice in
    1)
        install_prometheus
        ;;
    2)
        install_grafana
        ;;
    3)
        install_node_exporter
        ;;
    4)
        install_prometheus
        install_grafana
        install_node_exporter
        ;;
    5)
        check_ports
        ;;
    *)
        echo "Choix invalide"
        exit 1
        ;;
esac

echo ""
echo "====================================="
echo "Déploiement terminé!"
echo "====================================="
echo ""
echo "URLs d'accès :"
echo "  Prometheus: http://localhost:${PROMETHEUS_PORT}"
echo "  Grafana:    http://localhost:${GRAFANA_PORT}"
echo "             (admin/admin)"
echo ""
echo "Pour votre application FreePascal :"
echo "  Exposez les métriques sur http://localhost:${APP_PORT}/metrics"
echo ""
```

Rendez le script exécutable et lancez-le :

```bash
chmod +x deploy-monitoring.sh
sudo ./deploy-monitoring.sh
```

### Script Windows (PowerShell)

Créez un fichier `deploy-monitoring.ps1` :

```powershell
# Script de déploiement Prometheus + Grafana sur Windows
# Exécuter en tant qu'Administrateur

$ErrorActionPreference = "Stop"

Write-Host "=====================================" -ForegroundColor Cyan
Write-Host "Déploiement du monitoring Prometheus" -ForegroundColor Cyan
Write-Host "=====================================" -ForegroundColor Cyan

# Variables
$PrometheusVersion = "2.45.0"
$GrafanaVersion = "10.0.0"
$InstallDir = "C:\monitoring"
$PrometheusPort = 9090
$GrafanaPort = 3000
$AppPort = 8080

# Créer le répertoire d'installation
New-Item -ItemType Directory -Force -Path $InstallDir | Out-Null

# Fonction pour télécharger Prometheus
function Install-Prometheus {
    Write-Host ""
    Write-Host "Installation de Prometheus..." -ForegroundColor Yellow

    $PrometheusUrl = "https://github.com/prometheus/prometheus/releases/download/v$PrometheusVersion/prometheus-$PrometheusVersion.windows-amd64.zip"
    $PrometheusZip = "$InstallDir\prometheus.zip"

    # Télécharger
    Write-Host "Téléchargement de Prometheus..."
    Invoke-WebRequest -Uri $PrometheusUrl -OutFile $PrometheusZip

    # Extraire
    Write-Host "Extraction..."
    Expand-Archive -Path $PrometheusZip -DestinationPath $InstallDir -Force
    Rename-Item "$InstallDir\prometheus-$PrometheusVersion.windows-amd64" "$InstallDir\prometheus"
    Remove-Item $PrometheusZip

    # Configuration
    $PrometheusConfig = @"
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:$PrometheusPort']

  - job_name: 'mon_application'
    static_configs:
      - targets: ['localhost:$AppPort']
"@

    $PrometheusConfig | Out-File -FilePath "$InstallDir\prometheus\prometheus.yml" -Encoding UTF8

    Write-Host "✓ Prometheus installé dans $InstallDir\prometheus" -ForegroundColor Green
}

# Fonction pour télécharger Grafana
function Install-Grafana {
    Write-Host ""
    Write-Host "Installation de Grafana..." -ForegroundColor Yellow

    $GrafanaUrl = "https://dl.grafana.com/oss/release/grafana-$GrafanaVersion.windows-amd64.zip"
    $GrafanaZip = "$InstallDir\grafana.zip"

    # Télécharger
    Write-Host "Téléchargement de Grafana..."
    Invoke-WebRequest -Uri $GrafanaUrl -OutFile $GrafanaZip

    # Extraire
    Write-Host "Extraction..."
    Expand-Archive -Path $GrafanaZip -DestinationPath $InstallDir -Force
    Rename-Item "$InstallDir\grafana-$GrafanaVersion" "$InstallDir\grafana"
    Remove-Item $GrafanaZip

    Write-Host "✓ Grafana installé dans $InstallDir\grafana" -ForegroundColor Green
}

# Fonction pour créer les services Windows
function Create-Services {
    Write-Host ""
    Write-Host "Création des services Windows..." -ForegroundColor Yellow

    # Vérifier si NSSM est installé
    if (-not (Test-Path "C:\nssm\nssm.exe")) {
        Write-Host "NSSM non trouvé. Téléchargement..." -ForegroundColor Yellow
        $NssmUrl = "https://nssm.cc/release/nssm-2.24.zip"
        $NssmZip = "$env:TEMP\nssm.zip"
        Invoke-WebRequest -Uri $NssmUrl -OutFile $NssmZip
        Expand-Archive -Path $NssmZip -DestinationPath "$env:TEMP\nssm" -Force
        New-Item -ItemType Directory -Force -Path "C:\nssm" | Out-Null
        Copy-Item "$env:TEMP\nssm\nssm-2.24\win64\nssm.exe" "C:\nssm\nssm.exe"
        Remove-Item $NssmZip
        Remove-Item "$env:TEMP\nssm" -Recurse
    }

    # Service Prometheus
    & C:\nssm\nssm.exe install Prometheus "$InstallDir\prometheus\prometheus.exe"
    & C:\nssm\nssm.exe set Prometheus AppParameters "--config.file=$InstallDir\prometheus\prometheus.yml"
    & C:\nssm\nssm.exe set Prometheus AppDirectory "$InstallDir\prometheus"
    & C:\nssm\nssm.exe set Prometheus DisplayName "Prometheus Monitoring"
    & C:\nssm\nssm.exe set Prometheus Description "Système de monitoring et d'alerting"
    & C:\nssm\nssm.exe set Prometheus Start SERVICE_AUTO_START

    # Service Grafana
    & C:\nssm\nssm.exe install Grafana "$InstallDir\grafana\bin\grafana-server.exe"
    & C:\nssm\nssm.exe set Grafana AppDirectory "$InstallDir\grafana\bin"
    & C:\nssm\nssm.exe set Grafana DisplayName "Grafana"
    & C:\nssm\nssm.exe set Grafana Description "Plateforme de visualisation et d'analyse"
    & C:\nssm\nssm.exe set Grafana Start SERVICE_AUTO_START

    Write-Host "✓ Services créés" -ForegroundColor Green
}

# Fonction pour démarrer les services
function Start-Services {
    Write-Host ""
    Write-Host "Démarrage des services..." -ForegroundColor Yellow

    Start-Service -Name "Prometheus"
    Start-Service -Name "Grafana"

    Write-Host "✓ Services démarrés" -ForegroundColor Green
}

# Fonction de vérification
function Test-Installation {
    Write-Host ""
    Write-Host "Vérification de l'installation..." -ForegroundColor Yellow

    Start-Sleep -Seconds 5

    try {
        $PrometheusResponse = Invoke-WebRequest -Uri "http://localhost:$PrometheusPort" -TimeoutSec 5
        Write-Host "✓ Prometheus est accessible sur http://localhost:$PrometheusPort" -ForegroundColor Green
    } catch {
        Write-Host "✗ Prometheus n'est pas accessible" -ForegroundColor Red
    }

    try {
        $GrafanaResponse = Invoke-WebRequest -Uri "http://localhost:$GrafanaPort" -TimeoutSec 5
        Write-Host "✓ Grafana est accessible sur http://localhost:$GrafanaPort" -ForegroundColor Green
    } catch {
        Write-Host "✗ Grafana n'est pas accessible" -ForegroundColor Red
    }
}

# Menu principal
Write-Host ""
Write-Host "Que souhaitez-vous installer ?"
Write-Host "1) Prometheus seulement"
Write-Host "2) Grafana seulement"
Write-Host "3) Tout installer"
Write-Host "4) Créer les services Windows"
Write-Host "5) Démarrer les services"
Write-Host "6) Vérifier l'installation"
Write-Host ""
$choice = Read-Host "Votre choix (1-6)"

switch ($choice) {
    "1" { Install-Prometheus }
    "2" { Install-Grafana }
    "3" {
        Install-Prometheus
        Install-Grafana
        Create-Services
        Start-Services
        Test-Installation
    }
    "4" { Create-Services }
    "5" { Start-Services }
    "6" { Test-Installation }
    default {
        Write-Host "Choix invalide" -ForegroundColor Red
        exit 1
    }
}

Write-Host ""
Write-Host "=====================================" -ForegroundColor Cyan
Write-Host "Déploiement terminé!" -ForegroundColor Cyan
Write-Host "=====================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "URLs d'accès :"
Write-Host "  Prometheus: http://localhost:$PrometheusPort" -ForegroundColor Yellow
Write-Host "  Grafana:    http://localhost:$GrafanaPort" -ForegroundColor Yellow
Write-Host "             (admin/admin)" -ForegroundColor Gray
Write-Host ""
Write-Host "Pour votre application FreePascal :"
Write-Host "  Exposez les métriques sur http://localhost:$AppPort/metrics" -ForegroundColor Yellow
Write-Host ""
```

Exécutez le script en PowerShell (en tant qu'Administrateur) :

```powershell
Set-ExecutionPolicy -ExecutionPolicy Bypass -Scope Process
.\deploy-monitoring.ps1
```

## Checklist de mise en production

Avant de déployer votre système de monitoring en production, vérifiez :

### Configuration

- [ ] Prometheus scrape les bonnes cibles
- [ ] Les intervalles de scraping sont appropriés
- [ ] La rétention des données est configurée
- [ ] Les alertes sont définies et testées
- [ ] L'authentification est activée sur /metrics
- [ ] Le firewall autorise les ports nécessaires

### Sécurité

- [ ] L'endpoint /metrics est protégé
- [ ] Prometheus n'est pas exposé publiquement
- [ ] Grafana utilise HTTPS en production
- [ ] Les mots de passe par défaut sont changés
- [ ] Les certificats SSL sont valides

### Performance

- [ ] La cardinalité des métriques est raisonnable
- [ ] Les labels sont limités et pertinents
- [ ] Les métriques coûteuses sont évitées
- [ ] La mémoire allouée est suffisante
- [ ] Les disques ont assez d'espace

### Monitoring du monitoring

- [ ] Alertes configurées si Prometheus est down
- [ ] Alertes si le scraping échoue
- [ ] Dashboard Grafana pour surveiller Prometheus
- [ ] Backups des configurations
- [ ] Documentation à jour

## Aller plus loin

### Exporters personnalisés

Créez des exporters dédiés pour vos besoins spécifiques :

```pascal
program CustomExporter;

// Exporter pour surveiller un service Windows spécifique
// ou un daemon Linux personnalisé
```

### Fédération Prometheus

Pour surveiller plusieurs instances Prometheus :

```yaml
# Prometheus central qui agrège d'autres Prometheus
scrape_configs:
  - job_name: 'federate'
    scrape_interval: 15s
    honor_labels: true
    metrics_path: '/federate'
    params:
      'match[]':
        - '{job="mon_application"}'
    static_configs:
      - targets:
        - 'prometheus1:9090'
        - 'prometheus2:9090'
```

### Intégration avec d'autres outils

- **Alertmanager** : Gestion avancée des alertes (email, Slack, PagerDuty)
- **Thanos** : Stockage long terme et haute disponibilité
- **Cortex** : Prometheus as a Service multi-tenant
- **VictoriaMetrics** : Alternative performante à Prometheus

## Conclusion finale

Le monitoring avec Prometheus transforme votre approche du développement et de l'exploitation. Au lieu de réagir aux problèmes, vous les anticipez. Au lieu de deviner, vous mesurez. Au lieu de supposer, vous savez.

Cette connaissance approfondie de vos applications FreePascal/Lazarus, qu'elles tournent sur Windows ou Ubuntu, vous permet de :

- **Dormir tranquille** : Les alertes vous préviennent avant vos utilisateurs
- **Optimiser avec confiance** : Mesurez l'impact réel de vos changements
- **Prouver la valeur** : Montrez des métriques concrètes à votre équipe
- **Grandir sereinement** : Planifiez la scalabilité basée sur des données

Commencez simple avec quelques métriques clés, puis enrichissez progressivement votre observabilité. Chaque métrique ajoutée est un pas vers une meilleure maîtrise de vos applications.

Bon monitoring ! 🚀📊

⏭️ [Logging centralisé (ELK stack)](/22-devops-deploiement-multi-os/08-logging-centralise-elk-stack.md)
