🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.9 Blue-Green deployments

## Introduction aux déploiements Blue-Green

### Qu'est-ce qu'un déploiement Blue-Green ?

Le déploiement Blue-Green est une stratégie de mise en production qui permet de déployer une nouvelle version de votre application **sans interruption de service** et avec la possibilité de revenir instantanément à l'ancienne version en cas de problème.

**L'analogie du restaurant** :

Imaginez que vous gérez un restaurant et que vous voulez changer complètement le menu. Avec une approche traditionnelle, vous fermeriez le restaurant, changeriez le menu, puis réouvririez (vos clients attendent dehors).

Avec Blue-Green, vous auriez **deux cuisines identiques** :
- **Cuisine Bleue** : Celle qui sert actuellement les clients
- **Cuisine Verte** : Celle où vous préparez le nouveau menu

Une fois que le nouveau menu est prêt et testé dans la cuisine verte, vous basculez simplement les serveurs : ils vont maintenant chercher les plats dans la cuisine verte. Si un problème survient, vous rebasculez vers la cuisine bleue instantanément.

### Pourquoi utiliser Blue-Green ?

**Avantages** :
- ✅ **Zéro downtime** : Les utilisateurs ne voient jamais le service s'arrêter
- ✅ **Rollback instantané** : Retour arrière en quelques secondes si problème
- ✅ **Tests en production** : Vous pouvez tester la nouvelle version en conditions réelles
- ✅ **Réduction du stress** : Le déploiement devient moins risqué
- ✅ **Facilite les mises à jour fréquentes** : Moins de peur = plus de déploiements

**Inconvénients** :
- ❌ **Coût en ressources** : Nécessite le double d'infrastructure (temporairement)
- ❌ **Complexité de la base de données** : Les migrations DB doivent être compatibles avec les deux versions
- ❌ **État partagé** : Gérer l'état entre deux environnements peut être complexe

## Concept et architecture

### Les deux environnements

```
┌─────────────────────────────────────────────────────────────┐
│                     LOAD BALANCER / PROXY                   │
│                   (nginx, HAProxy, etc.)                    │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       │ Bascule du trafic
                       │
         ┌─────────────┴─────────────┐
         │                           │
         ▼                           ▼
┌─────────────────┐         ┌─────────────────┐
│  ENVIRONNEMENT  │         │  ENVIRONNEMENT  │
│      BLEU       │         │      VERT       │
│   (Production)  │         │  (Standby/Test) │
│                 │         │                 │
│  App v1.0       │         │  App v1.1       │
│  Serveur(s)     │         │  Serveur(s)     │
└─────────────────┘         └─────────────────┘
         │                           │
         └───────────┬───────────────┘
                     │
                     ▼
            ┌─────────────────┐
            │   BASE DE       │
            │   DONNÉES       │
            │   (Partagée)    │
            └─────────────────┘
```

### Le processus de déploiement

**Phase 1 : État initial**
- Environnement **Bleu** est en production avec la version 1.0
- Environnement **Vert** est inactif
- Le load balancer dirige tout le trafic vers le Bleu

**Phase 2 : Déploiement**
- Déployer la version 1.1 sur l'environnement **Vert**
- Tester l'environnement Vert (tests automatisés, tests manuels)
- Le Bleu continue de servir les utilisateurs

**Phase 3 : Bascule**
- Reconfigurer le load balancer pour diriger le trafic vers le **Vert**
- L'environnement Vert devient la production
- L'environnement Bleu reste disponible (au cas où)

**Phase 4 : Validation**
- Surveiller l'environnement Vert en production
- Si tout va bien : succès ! 🎉
- Si problème : rebascule vers le Bleu en quelques secondes

**Phase 5 : Nettoyage**
- Après quelques heures/jours de stabilité
- L'ancien environnement (Bleu) peut être mis à jour ou recyclé

## Implémentation avec FreePascal

### Architecture d'application compatible Blue-Green

Pour qu'une application FreePascal soit compatible avec Blue-Green, elle doit respecter certains principes :

#### 1. Application stateless (sans état local)

```pascal
// ❌ MAUVAIS : État stocké localement
var
  GlobalUserSessions: TUserSessionList; // Perdu lors du switch

// ✅ BON : État externalisé
var
  Redis: TRedisClient; // Sessions stockées dans Redis
```

#### 2. Configuration externalisée

```pascal
unit AppConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TAppConfig = class
  private
    FEnvironmentName: string; // 'blue' ou 'green'
    FDatabaseHost: string;
    FDatabasePort: Integer;
    FRedisHost: string;
    FAppVersion: string;
    procedure LoadFromEnvironment;
    procedure LoadFromFile(const AFileName: string);
  public
    constructor Create;
    property EnvironmentName: string read FEnvironmentName;
    property DatabaseHost: string read FDatabaseHost;
    property DatabasePort: Integer read FDatabasePort;
    property RedisHost: string read FRedisHost;
    property AppVersion: string read FAppVersion;
  end;

implementation

constructor TAppConfig.Create;  
begin
  // Priorité : Variables d'environnement > Fichier config
  LoadFromEnvironment;

  if FDatabaseHost = '' then
    LoadFromFile('config.ini');
end;

procedure TAppConfig.LoadFromEnvironment;  
begin
  FEnvironmentName := GetEnvironmentVariable('APP_ENVIRONMENT');
  FDatabaseHost := GetEnvironmentVariable('DB_HOST');
  FDatabasePort := StrToIntDef(GetEnvironmentVariable('DB_PORT'), 5432);
  FRedisHost := GetEnvironmentVariable('REDIS_HOST');
  FAppVersion := GetEnvironmentVariable('APP_VERSION');
end;

procedure TAppConfig.LoadFromFile(const AFileName: string);  
var
  Ini: TIniFile;
begin
  if not FileExists(AFileName) then
    Exit;

  Ini := TIniFile.Create(AFileName);
  try
    if FEnvironmentName = '' then
      FEnvironmentName := Ini.ReadString('app', 'environment', 'blue');
    if FDatabaseHost = '' then
      FDatabaseHost := Ini.ReadString('database', 'host', 'localhost');
    if FDatabasePort = 0 then
      FDatabasePort := Ini.ReadInteger('database', 'port', 5432);
    if FRedisHost = '' then
      FRedisHost := Ini.ReadString('redis', 'host', 'localhost');
    if FAppVersion = '' then
      FAppVersion := Ini.ReadString('app', 'version', '1.0.0');
  finally
    Ini.Free;
  end;
end;

end.
```

#### 3. Health check endpoint

Chaque instance doit exposer un endpoint de santé pour que le load balancer sache si elle est prête :

```pascal
unit HealthCheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, fpjson;

type
  THealthStatus = (hsHealthy, hsDegraded, hsUnhealthy);

  THealthCheck = class
  private
    FDatabaseConnected: Boolean;
    FRedisConnected: Boolean;
    FStartTime: TDateTime;
    function GetStatus: THealthStatus;
    function GetUptimeSeconds: Int64;
  public
    constructor Create;
    procedure CheckDatabase;
    procedure CheckRedis;
    function GetHealthJSON: TJSONObject;
    property Status: THealthStatus read GetStatus;
    property UptimeSeconds: Int64 read GetUptimeSeconds;
  end;

implementation

uses
  DateUtils;

constructor THealthCheck.Create;  
begin
  FStartTime := Now;
  FDatabaseConnected := False;
  FRedisConnected := False;
end;

procedure THealthCheck.CheckDatabase;  
begin
  // Vérifier la connexion à la base de données
  try
    // Votre code de test de connexion DB
    // Ex: ExecuteQuery('SELECT 1');
    FDatabaseConnected := True;
  except
    FDatabaseConnected := False;
  end;
end;

procedure THealthCheck.CheckRedis;  
begin
  // Vérifier la connexion à Redis
  try
    // Votre code de test Redis
    // Ex: RedisPing();
    FRedisConnected := True;
  except
    FRedisConnected := False;
  end;
end;

function THealthCheck.GetStatus: THealthStatus;  
begin
  if FDatabaseConnected and FRedisConnected then
    Result := hsHealthy
  else if FDatabaseConnected or FRedisConnected then
    Result := hsDegraded
  else
    Result := hsUnhealthy;
end;

function THealthCheck.GetUptimeSeconds: Int64;  
begin
  Result := SecondsBetween(Now, FStartTime);
end;

function THealthCheck.GetHealthJSON: TJSONObject;  
var
  StatusStr: string;
begin
  case Status of
    hsHealthy: StatusStr := 'healthy';
    hsDegraded: StatusStr := 'degraded';
    hsUnhealthy: StatusStr := 'unhealthy';
  end;

  Result := TJSONObject.Create;
  Result.Add('status', StatusStr);
  Result.Add('uptime_seconds', UptimeSeconds);
  Result.Add('database', FDatabaseConnected);
  Result.Add('redis', FRedisConnected);
  Result.Add('environment', AppConfig.EnvironmentName); // Bleu ou Vert
  Result.Add('version', AppConfig.AppVersion);
end;

end.
```

**Utilisation dans votre serveur HTTP** :

```pascal
procedure TMyHTTPServer.HandleHealthCheck(ARequest: TRequest; AResponse: TResponse);  
var
  Health: THealthCheck;
  JSON: TJSONObject;
begin
  Health := THealthCheck.Create;
  try
    Health.CheckDatabase;
    Health.CheckRedis;

    JSON := Health.GetHealthJSON;
    try
      AResponse.ContentType := 'application/json';

      // Code HTTP selon le statut
      case Health.Status of
        hsHealthy: AResponse.Code := 200;
        hsDegraded: AResponse.Code := 200; // Ou 503 selon votre stratégie
        hsUnhealthy: AResponse.Code := 503; // Service Unavailable
      end;

      AResponse.Content := JSON.AsJSON;
    finally
      JSON.Free;
    end;
  finally
    Health.Free;
  end;
end;
```

**Exemple de réponse** :
```json
{
  "status": "healthy",
  "uptime_seconds": 3600,
  "database": true,
  "redis": true,
  "environment": "green",
  "version": "1.1.0"
}
```

## Configuration du Load Balancer

### Avec Nginx

Nginx est un excellent choix pour gérer le trafic entre vos environnements Blue-Green.

#### Installation

**Ubuntu** :
```bash
sudo apt update  
sudo apt install nginx
```

**Windows** :
Téléchargez depuis http://nginx.org/en/download.html

#### Configuration Blue-Green de base

Créez un fichier `/etc/nginx/sites-available/myapp` (Ubuntu) ou modifiez `conf/nginx.conf` (Windows) :

```nginx
# Définition des environnements
upstream blue_environment {
    server 192.168.1.10:8080;  # Serveur Blue
    # server 192.168.1.11:8080;  # Serveur Blue #2 (si plusieurs)
}

upstream green_environment {
    server 192.168.1.20:8080;  # Serveur Green
    # server 192.168.1.21:8080;  # Serveur Green #2
}

# L'environnement actuellement actif
# Modifiez cette ligne pour basculer entre Blue et Green
upstream active_environment {
    server 192.168.1.10:8080;  # BLUE est actif
    # server 192.168.1.20:8080;  # GREEN est actif (commenté)
}

server {
    listen 80;
    server_name myapp.example.com;

    # Health check endpoint (ne passe pas par le proxy)
    location /health {
        proxy_pass http://active_environment/health;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }

    # Toutes les autres requêtes
    location / {
        proxy_pass http://active_environment;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # Timeouts
        proxy_connect_timeout 5s;
        proxy_send_timeout 60s;
        proxy_read_timeout 60s;
    }
}
```

#### Configuration avancée avec health checks

```nginx
upstream blue_environment {
    server 192.168.1.10:8080 max_fails=3 fail_timeout=30s;
}

upstream green_environment {
    server 192.168.1.20:8080 max_fails=3 fail_timeout=30s;
}

# Module health check (nécessite nginx-plus ou module externe)
# Avec nginx standard, utilisez un script externe
upstream active_environment {
    server 192.168.1.10:8080;

    # Health check passif : nginx détecte automatiquement les pannes
    # Après 3 échecs, le serveur est marqué "down" pendant 30s
}

server {
    listen 80;
    server_name myapp.example.com;

    # Logs spécifiques
    access_log /var/log/nginx/myapp_access.log;
    error_log /var/log/nginx/myapp_error.log;

    location / {
        proxy_pass http://active_environment;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;

        # En-têtes personnalisés pour identifier l'environnement
        add_header X-Served-By $upstream_addr;
    }

    # Endpoint administratif pour vérifier la configuration
    location /nginx-status {
        stub_status on;
        access_log off;
        allow 127.0.0.1;
        deny all;
    }
}
```

### Avec HAProxy

HAProxy offre des fonctionnalités de health check plus avancées.

#### Installation

**Ubuntu** :
```bash
sudo apt install haproxy
```

#### Configuration Blue-Green

Éditez `/etc/haproxy/haproxy.cfg` :

```haproxy
global
    log /dev/log local0
    log /dev/log local1 notice
    chroot /var/lib/haproxy
    stats socket /run/haproxy/admin.sock mode 660 level admin
    stats timeout 30s
    user haproxy
    group haproxy
    daemon

defaults
    log     global
    mode    http
    option  httplog
    option  dontlognull
    timeout connect 5000
    timeout client  50000
    timeout server  50000

# Interface d'administration
listen stats
    bind *:8404
    stats enable
    stats uri /stats
    stats refresh 30s
    stats auth admin:password  # Changez le mot de passe !

# Frontend : point d'entrée
frontend http_front
    bind *:80
    default_backend blue_back  # Environnement actif

# Backend Blue
backend blue_back
    balance roundrobin
    option httpchk GET /health
    http-check expect status 200

    server blue1 192.168.1.10:8080 check inter 2s fall 3 rise 2
    # server blue2 192.168.1.11:8080 check inter 2s fall 3 rise 2

# Backend Green
backend green_back
    balance roundrobin
    option httpchk GET /health
    http-check expect status 200

    server green1 192.168.1.20:8080 check inter 2s fall 3 rise 2
    # server green2 192.168.1.21:8080 check inter 2s fall 3 rise 2
```

**Explication des paramètres** :
- `check inter 2s` : Vérifie la santé toutes les 2 secondes
- `fall 3` : Après 3 échecs, le serveur est marqué DOWN
- `rise 2` : Après 2 succès, le serveur est marqué UP
- `option httpchk GET /health` : Utilise l'endpoint /health pour vérifier

#### Basculer entre Blue et Green

**Méthode 1 : Modifier la configuration**
```bash
# Éditer /etc/haproxy/haproxy.cfg
# Changer : default_backend blue_back
# En :      default_backend green_back

# Recharger sans interruption
sudo systemctl reload haproxy
```

**Méthode 2 : Via l'API Runtime (sans redémarrage)**
```bash
# Se connecter au socket admin
echo "set server green_back/green1 state ready" | sudo socat stdio /run/haproxy/admin.sock  
echo "set server blue_back/blue1 state drain" | sudo socat stdio /run/haproxy/admin.sock
```

## Scripts de déploiement automatisés

### Script Bash pour Ubuntu

```bash
#!/bin/bash
# deploy-bluegreen.sh

set -e  # Arrêter en cas d'erreur

# Configuration
APP_NAME="myfreepascalapp"  
BLUE_SERVER="192.168.1.10"  
GREEN_SERVER="192.168.1.20"  
DEPLOY_USER="deploy"  
APP_PATH="/opt/$APP_NAME"  
BINARY_NAME="$APP_NAME"  
VERSION=$1

# Couleurs pour les messages
RED='\033[0;31m'  
GREEN='\033[0;32m'  
YELLOW='\033[1;33m'  
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Vérifier les arguments
if [ -z "$VERSION" ]; then
    log_error "Usage: $0 <version>"
    exit 1
fi

# Déterminer l'environnement actuellement actif
get_active_environment() {
    # Interroger le load balancer pour savoir qui est actif
    ACTIVE=$(curl -s http://localhost/health | jq -r '.environment')
    echo "$ACTIVE"
}

# Déterminer l'environnement de déploiement (l'inactif)
ACTIVE_ENV=$(get_active_environment)  
if [ "$ACTIVE_ENV" = "blue" ]; then
    TARGET_ENV="green"
    TARGET_SERVER=$GREEN_SERVER
else
    TARGET_ENV="blue"
    TARGET_SERVER=$BLUE_SERVER
fi

log_info "Environnement actif : $ACTIVE_ENV"  
log_info "Déploiement vers : $TARGET_ENV ($TARGET_SERVER)"

# Étape 1 : Compilation
log_info "Compilation de la version $VERSION..."  
lazbuild --build-mode=Release MyProject.lpi

# Étape 2 : Transfert vers le serveur cible
log_info "Transfert du binaire vers $TARGET_SERVER..."  
scp $BINARY_NAME $DEPLOY_USER@$TARGET_SERVER:$APP_PATH/$BINARY_NAME.new

# Étape 3 : Arrêt de l'ancienne version sur le serveur cible
log_info "Arrêt du service sur $TARGET_ENV..."  
ssh $DEPLOY_USER@$TARGET_SERVER "sudo systemctl stop $APP_NAME"

# Étape 4 : Remplacement du binaire
log_info "Mise à jour du binaire..."  
ssh $DEPLOY_USER@$TARGET_SERVER "
    cd $APP_PATH && \
    sudo mv $BINARY_NAME $BINARY_NAME.old && \
    sudo mv $BINARY_NAME.new $BINARY_NAME && \
    sudo chmod +x $BINARY_NAME
"

# Étape 5 : Configuration de l'environnement
log_info "Configuration de l'environnement..."  
ssh $DEPLOY_USER@$TARGET_SERVER "
    export APP_ENVIRONMENT=$TARGET_ENV && \
    export APP_VERSION=$VERSION
"

# Étape 6 : Démarrage du nouveau service
log_info "Démarrage du service sur $TARGET_ENV..."  
ssh $DEPLOY_USER@$TARGET_SERVER "sudo systemctl start $APP_NAME"

# Étape 7 : Attendre que le service soit prêt
log_info "Vérification de la santé du service..."  
MAX_ATTEMPTS=30  
ATTEMPT=0  
while [ $ATTEMPT -lt $MAX_ATTEMPTS ]; do
    HEALTH_STATUS=$(curl -s http://$TARGET_SERVER:8080/health | jq -r '.status' || echo "error")

    if [ "$HEALTH_STATUS" = "healthy" ]; then
        log_info "Service $TARGET_ENV est prêt !"
        break
    fi

    ATTEMPT=$((ATTEMPT + 1))
    log_warn "Tentative $ATTEMPT/$MAX_ATTEMPTS - Status: $HEALTH_STATUS"
    sleep 2
done

if [ $ATTEMPT -eq $MAX_ATTEMPTS ]; then
    log_error "Le service $TARGET_ENV ne répond pas correctement"
    log_error "Rollback recommandé !"
    exit 1
fi

# Étape 8 : Tests de fumée (smoke tests)
log_info "Exécution des tests de fumée..."  
SMOKE_TEST_RESULT=$(curl -s -o /dev/null -w "%{http_code}" http://$TARGET_SERVER:8080/api/test)  
if [ "$SMOKE_TEST_RESULT" != "200" ]; then
    log_error "Les tests de fumée ont échoué (Code: $SMOKE_TEST_RESULT)"
    exit 1
fi  
log_info "Tests de fumée réussis !"

# Étape 9 : Basculer le trafic
log_info "Prêt à basculer le trafic vers $TARGET_ENV"  
read -p "Continuer avec la bascule ? (o/n) " -n 1 -r  
echo  
if [[ ! $REPLY =~ ^[Oo]$ ]]; then
    log_warn "Bascule annulée par l'utilisateur"
    exit 0
fi

log_info "Basculement du trafic vers $TARGET_ENV..."
# Modifier nginx ou haproxy selon votre configuration
if [ "$TARGET_ENV" = "green" ]; then
    sudo sed -i 's/server 192.168.1.10:8080/server 192.168.1.20:8080/' /etc/nginx/sites-available/myapp
else
    sudo sed -i 's/server 192.168.1.20:8080/server 192.168.1.10:8080/' /etc/nginx/sites-available/myapp
fi  
sudo systemctl reload nginx

# Étape 10 : Surveillance post-déploiement
log_info "Surveillance de l'environnement $TARGET_ENV pendant 60 secondes..."  
for i in {1..12}; do
    HEALTH=$(curl -s http://localhost/health | jq -r '.status')
    log_info "[$i/12] Health status: $HEALTH"
    sleep 5
done

log_info "Déploiement terminé avec succès ! 🎉"  
log_info "Environnement $TARGET_ENV est maintenant en production"  
log_info "Environnement $ACTIVE_ENV reste disponible pour rollback si nécessaire"
```

### Script PowerShell pour Windows

```powershell
# deploy-bluegreen.ps1
param(
    [Parameter(Mandatory=$true)]
    [string]$Version
)

$ErrorActionPreference = "Stop"

# Configuration
$AppName = "myfreepascalapp"
$BlueServer = "192.168.1.10"
$GreenServer = "192.168.1.20"
$DeployUser = "deploy"
$AppPath = "C:\Apps\$AppName"
$BinaryName = "$AppName.exe"

function Log-Info {
    param([string]$Message)
    Write-Host "[INFO] $Message" -ForegroundColor Green
}

function Log-Warn {
    param([string]$Message)
    Write-Host "[WARN] $Message" -ForegroundColor Yellow
}

function Log-Error {
    param([string]$Message)
    Write-Host "[ERROR] $Message" -ForegroundColor Red
}

# Déterminer l'environnement actif
function Get-ActiveEnvironment {
    try {
        $response = Invoke-RestMethod -Uri "http://localhost/health" -Method Get
        return $response.environment
    } catch {
        Log-Error "Impossible de déterminer l'environnement actif"
        exit 1
    }
}

$ActiveEnv = Get-ActiveEnvironment
if ($ActiveEnv -eq "blue") {
    $TargetEnv = "green"
    $TargetServer = $GreenServer
} else {
    $TargetEnv = "blue"
    $TargetServer = $BlueServer
}

Log-Info "Environnement actif : $ActiveEnv"  
Log-Info "Déploiement vers : $TargetEnv ($TargetServer)"

# Compilation
Log-Info "Compilation de la version $Version..."
& lazbuild --build-mode=Release MyProject.lpi
if ($LASTEXITCODE -ne 0) {
    Log-Error "Échec de la compilation"
    exit 1
}

# Transfert (utilise PSRemoting ou un partage réseau)
Log-Info "Transfert du binaire vers $TargetServer..."  
Copy-Item -Path $BinaryName -Destination "\\$TargetServer\$AppPath\$BinaryName.new"

# Arrêt du service distant
Log-Info "Arrêt du service sur $TargetEnv..."  
Invoke-Command -ComputerName $TargetServer -ScriptBlock {
    Stop-Service -Name $using:AppName
}

# Remplacement du binaire
Log-Info "Mise à jour du binaire..."  
Invoke-Command -ComputerName $TargetServer -ScriptBlock {
    $AppPath = $using:AppPath
    $BinaryName = $using:BinaryName

    if (Test-Path "$AppPath\$BinaryName.old") {
        Remove-Item "$AppPath\$BinaryName.old"
    }

    Rename-Item "$AppPath\$BinaryName" "$AppPath\$BinaryName.old"
    Rename-Item "$AppPath\$BinaryName.new" "$AppPath\$BinaryName"
}

# Démarrage du service
Log-Info "Démarrage du service sur $TargetEnv..."  
Invoke-Command -ComputerName $TargetServer -ScriptBlock {
    [Environment]::SetEnvironmentVariable("APP_ENVIRONMENT", $using:TargetEnv, "Process")
    [Environment]::SetEnvironmentVariable("APP_VERSION", $using:Version, "Process")
    Start-Service -Name $using:AppName
}

# Vérification de santé
Log-Info "Vérification de la santé du service..."
$MaxAttempts = 30
$Attempt = 0
$Healthy = $false

while ($Attempt -lt $MaxAttempts) {
    try {
        $health = Invoke-RestMethod -Uri "http://${TargetServer}:8080/health" -Method Get
        if ($health.status -eq "healthy") {
            Log-Info "Service $TargetEnv est prêt !"
            $Healthy = $true
            break
        }
    } catch {
        # Continuer à essayer
    }

    $Attempt++
    Log-Warn "Tentative $Attempt/$MaxAttempts"
    Start-Sleep -Seconds 2
}

if (-not $Healthy) {
    Log-Error "Le service $TargetEnv ne répond pas correctement"
    exit 1
}

# Tests de fumée
Log-Info "Exécution des tests de fumée..."  
try {
    $response = Invoke-WebRequest -Uri "http://${TargetServer}:8080/api/test" -Method Get
    if ($response.StatusCode -ne 200) {
        throw "Code de statut incorrect"
    }
    Log-Info "Tests de fumée réussis !"
} catch {
    Log-Error "Les tests de fumée ont échoué"
    exit 1
}

# Confirmation manuelle
$confirmation = Read-Host "Basculer le trafic vers $TargetEnv ? (o/n)"
if ($confirmation -ne 'o') {
    Log-Warn "Bascule annulée"
    exit 0
}

# Basculement (exemple avec modification de fichier nginx)
Log-Info "Basculement du trafic vers $TargetEnv..."
# Adaptez selon votre load balancer
# Exemple : modifier un fichier de config et recharger nginx

Log-Info "Déploiement terminé avec succès ! 🎉"
```

## Gestion de la base de données

### Le défi des migrations de schéma

La base de données est souvent le point délicat dans un déploiement Blue-Green car elle est partagée entre les deux environnements.

### Stratégies de migration compatible Blue-Green

#### 1. Migrations rétrocompatibles (recommandé)

**Principe** : Les modifications de schéma doivent être compatibles avec l'ancienne ET la nouvelle version de l'application.

**Exemple : Ajouter une colonne**

```sql
-- ❌ MAUVAIS : Colonne NOT NULL sans valeur par défaut
ALTER TABLE users ADD COLUMN phone VARCHAR(20) NOT NULL;
-- L'ancienne version plantera car elle n'envoie pas ce champ

-- ✅ BON : Colonne NULL ou avec valeur par défaut
ALTER TABLE users ADD COLUMN phone VARCHAR(20) DEFAULT NULL;
-- Les deux versions fonctionnent : l'ancienne laisse NULL, la nouvelle remplit
```

**Exemple : Renommer une colonne**

Ne JAMAIS renommer directement. Utilisez une approche en 3 phases :

```sql
-- Phase 1 : Ajouter la nouvelle colonne (déploiement N)
ALTER TABLE users ADD COLUMN email_address VARCHAR(255);  
UPDATE users SET email_address = email WHERE email_address IS NULL;

-- Phase 2 : Modifier l'application pour écrire dans les deux colonnes
-- (déploiement N+1, Blue-Green compatible)
-- Code FreePascal :
-- User.Email := 'user@example.com';        -- Ancienne colonne
-- User.EmailAddress := 'user@example.com'; -- Nouvelle colonne

-- Phase 3 : Supprimer l'ancienne colonne (déploiement N+2)
-- Une fois que toutes les instances utilisent email_address
ALTER TABLE users DROP COLUMN email;
```

#### 2. Migrations en plusieurs étapes

**Exemple de migration complexe** :

Supposons que vous voulez diviser une table `users` en deux : `users` et `user_profiles`.

**Étape 1** (Déploiement Blue-Green #1) :
```sql
-- Créer la nouvelle table
CREATE TABLE user_profiles (
    user_id INT PRIMARY KEY,
    bio TEXT,
    avatar_url VARCHAR(500),
    FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Migrer les données existantes
INSERT INTO user_profiles (user_id, bio, avatar_url)  
SELECT id, bio, avatar_url FROM users;
```

**Étape 2** (Déploiement Blue-Green #2) :
Modifier l'application pour lire depuis les deux tables :
```pascal
// L'application continue d'écrire dans users.bio (ancien)
// Mais lit depuis user_profiles.bio si disponible (nouveau)
function TUserService.GetUserBio(UserID: Integer): string;  
begin
  // Essayer d'abord la nouvelle table
  Result := GetFromUserProfiles(UserID);

  // Fallback vers l'ancienne si vide
  if Result = '' then
    Result := GetFromUsersTable(UserID);
end;
```

**Étape 3** (Déploiement Blue-Green #3) :
Maintenant l'application écrit dans les deux endroits :
```pascal
procedure TUserService.UpdateUserBio(UserID: Integer; const Bio: string);  
begin
  // Écrire dans les deux tables pour compatibilité
  UpdateUsersTable(UserID, Bio);      // Pour les anciennes versions
  UpdateUserProfiles(UserID, Bio);     // Pour les nouvelles versions
end;
```

**Étape 4** (Déploiement Blue-Green #4) :
L'application ne lit/écrit plus que dans `user_profiles` :
```pascal
procedure TUserService.UpdateUserBio(UserID: Integer; const Bio: string);  
begin
  UpdateUserProfiles(UserID, Bio);  // Uniquement la nouvelle table
end;
```

**Étape 5** (Maintenance ultérieure) :
```sql
-- Supprimer les anciennes colonnes devenues inutiles
ALTER TABLE users DROP COLUMN bio;  
ALTER TABLE users DROP COLUMN avatar_url;
```

### Gestion des transactions et de la cohérence

#### Éviter les transactions longues pendant la bascule

```pascal
// ❌ MAUVAIS : Transaction qui bloque pendant la bascule
Connection.StartTransaction;  
try
  // Beaucoup d'opérations qui prennent du temps...
  ProcessBigBatch;
  Connection.Commit;
except
  Connection.Rollback;
end;

// ✅ BON : Transactions courtes
for Item in BatchItems do  
begin
  Connection.StartTransaction;
  try
    ProcessSingleItem(Item);
    Connection.Commit;  // Commit rapide après chaque item
  except
    Connection.Rollback;
  end;
end;
```

## Gestion de l'état et des sessions

### Le problème des sessions en mémoire

Si vos deux environnements (Blue et Green) maintiennent des sessions en mémoire, les utilisateurs perdront leur session lors de la bascule.

### Solution : Externaliser les sessions

#### Avec Redis

```pascal
unit SessionManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Redis; // Utilisez une bibliothèque Redis pour FreePascal

type
  TSessionManager = class
  private
    FRedisClient: TRedisClient;
    FSessionPrefix: string;
  public
    constructor Create(const ARedisHost: string; ARedisPort: Integer);
    destructor Destroy; override;

    procedure SetSession(const SessionID, Key, Value: string; TTLSeconds: Integer = 3600);
    function GetSession(const SessionID, Key: string): string;
    procedure DeleteSession(const SessionID: string);
    function SessionExists(const SessionID: string): Boolean;
  end;

implementation

constructor TSessionManager.Create(const ARedisHost: string; ARedisPort: Integer);  
begin
  FRedisClient := TRedisClient.Create(ARedisHost, ARedisPort);
  FSessionPrefix := 'session:';
end;

destructor TSessionManager.Destroy;  
begin
  FRedisClient.Free;
  inherited;
end;

procedure TSessionManager.SetSession(const SessionID, Key, Value: string; TTLSeconds: Integer);  
var
  RedisKey: string;
begin
  RedisKey := FSessionPrefix + SessionID + ':' + Key;
  FRedisClient.SetEx(RedisKey, Value, TTLSeconds);
end;

function TSessionManager.GetSession(const SessionID, Key: string): string;  
var
  RedisKey: string;
begin
  RedisKey := FSessionPrefix + SessionID + ':' + Key;
  Result := FRedisClient.Get(RedisKey);
end;

procedure TSessionManager.DeleteSession(const SessionID: string);  
var
  Pattern: string;
begin
  Pattern := FSessionPrefix + SessionID + ':*';
  // Supprimer toutes les clés correspondant au pattern
  FRedisClient.DeletePattern(Pattern);
end;

function TSessionManager.SessionExists(const SessionID: string): Boolean;  
var
  Pattern: string;
begin
  Pattern := FSessionPrefix + SessionID + ':*';
  Result := FRedisClient.Exists(Pattern);
end;

end.
```

**Utilisation** :

```pascal
var
  SessionMgr: TSessionManager;
  UserID: string;
begin
  SessionMgr := TSessionManager.Create('localhost', 6379);
  try
    // Stocker des données de session
    SessionMgr.SetSession('abc123', 'user_id', '42', 7200);  // 2 heures
    SessionMgr.SetSession('abc123', 'username', 'john_doe', 7200);

    // Récupérer des données de session
    UserID := SessionMgr.GetSession('abc123', 'user_id');
    WriteLn('User ID: ', UserID);

    // Après logout
    SessionMgr.DeleteSession('abc123');
  finally
    SessionMgr.Free;
  end;
end;
```

## Monitoring et observabilité

### Métriques à surveiller pendant un déploiement Blue-Green

#### 1. Métriques applicatives

```pascal
unit Metrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TMetricsCollector = class
  private
    FRequestCount: Int64;
    FErrorCount: Int64;
    FTotalResponseTime: Int64;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure IncrementRequests;
    procedure IncrementErrors;
    procedure RecordResponseTime(Milliseconds: Integer);

    function GetMetrics: string; // Retourne JSON
  end;

implementation

uses
  fpjson;

constructor TMetricsCollector.Create;  
begin
  FLock := TCriticalSection.Create;
  FRequestCount := 0;
  FErrorCount := 0;
  FTotalResponseTime := 0;
end;

destructor TMetricsCollector.Destroy;  
begin
  FLock.Free;
  inherited;
end;

procedure TMetricsCollector.IncrementRequests;  
begin
  FLock.Enter;
  try
    Inc(FRequestCount);
  finally
    FLock.Leave;
  end;
end;

procedure TMetricsCollector.IncrementErrors;  
begin
  FLock.Enter;
  try
    Inc(FErrorCount);
  finally
    FLock.Leave;
  end;
end;

procedure TMetricsCollector.RecordResponseTime(Milliseconds: Integer);  
begin
  FLock.Enter;
  try
    FTotalResponseTime := FTotalResponseTime + Milliseconds;
  finally
    FLock.Leave;
  end;
end;

function TMetricsCollector.GetMetrics: string;  
var
  JSON: TJSONObject;
  AvgResponseTime: Double;
  ErrorRate: Double;
begin
  FLock.Enter;
  try
    if FRequestCount > 0 then
    begin
      AvgResponseTime := FTotalResponseTime / FRequestCount;
      ErrorRate := (FErrorCount / FRequestCount) * 100;
    end
    else
    begin
      AvgResponseTime := 0;
      ErrorRate := 0;
    end;

    JSON := TJSONObject.Create;
    try
      JSON.Add('total_requests', FRequestCount);
      JSON.Add('total_errors', FErrorCount);
      JSON.Add('error_rate_percent', ErrorRate);
      JSON.Add('avg_response_time_ms', AvgResponseTime);
      Result := JSON.AsJSON;
    finally
      JSON.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
```

**Exposer via un endpoint** :

```pascal
procedure TMyHTTPServer.HandleMetrics(ARequest: TRequest; AResponse: TResponse);  
begin
  AResponse.ContentType := 'application/json';
  AResponse.Content := GlobalMetrics.GetMetrics;
end;
```

#### 2. Dashboard de monitoring

Créez un script qui compare les métriques entre Blue et Green pendant la bascule :

```bash
#!/bin/bash
# monitor-deployment.sh

BLUE_URL="http://192.168.1.10:8080"  
GREEN_URL="http://192.168.1.20:8080"  
DURATION=300  # 5 minutes de monitoring

echo "Surveillance Blue vs Green pendant $DURATION secondes"  
echo "Timestamp,Blue_Requests,Blue_Errors,Blue_AvgTime,Green_Requests,Green_Errors,Green_AvgTime"

END_TIME=$((SECONDS + DURATION))

while [ $SECONDS -lt $END_TIME ]; do
    TIMESTAMP=$(date +%s)

    # Métriques Blue
    BLUE_METRICS=$(curl -s $BLUE_URL/metrics)
    BLUE_REQUESTS=$(echo $BLUE_METRICS | jq -r '.total_requests')
    BLUE_ERRORS=$(echo $BLUE_METRICS | jq -r '.total_errors')
    BLUE_AVG=$(echo $BLUE_METRICS | jq -r '.avg_response_time_ms')

    # Métriques Green
    GREEN_METRICS=$(curl -s $GREEN_URL/metrics)
    GREEN_REQUESTS=$(echo $GREEN_METRICS | jq -r '.total_requests')
    GREEN_ERRORS=$(echo $GREEN_METRICS | jq -r '.total_errors')
    GREEN_AVG=$(echo $GREEN_METRICS | jq -r '.avg_response_time_ms')

    echo "$TIMESTAMP,$BLUE_REQUESTS,$BLUE_ERRORS,$BLUE_AVG,$GREEN_REQUESTS,$GREEN_ERRORS,$GREEN_AVG"

    sleep 5
done
```

### Alertes automatiques

Configurez des alertes qui se déclenchent si :

- Le taux d'erreur augmente de plus de 50% après la bascule
- Le temps de réponse moyen augmente de plus de 100ms
- Le nombre de requêtes par seconde chute brutalement

```bash
#!/bin/bash
# check-health-regression.sh

ACTIVE_URL="http://localhost"  
THRESHOLD_ERROR_RATE=5.0  # 5% d'erreurs max  
THRESHOLD_AVG_TIME=500    # 500ms max

METRICS=$(curl -s $ACTIVE_URL/metrics)  
ERROR_RATE=$(echo $METRICS | jq -r '.error_rate_percent')  
AVG_TIME=$(echo $METRICS | jq -r '.avg_response_time_ms')

# Comparer avec les seuils
if (( $(echo "$ERROR_RATE > $THRESHOLD_ERROR_RATE" | bc -l) )); then
    echo "ALERTE: Taux d'erreur élevé: $ERROR_RATE%"
    # Envoyer notification (email, Slack, etc.)
    exit 1
fi

if (( $(echo "$AVG_TIME > $THRESHOLD_AVG_TIME" | bc -l) )); then
    echo "ALERTE: Temps de réponse élevé: ${AVG_TIME}ms"
    exit 1
fi

echo "Santé OK - Erreurs: $ERROR_RATE%, Temps: ${AVG_TIME}ms"
```

## Procédure de rollback

### Rollback immédiat

En cas de problème détecté, vous devez pouvoir revenir en arrière en quelques secondes.

#### Script de rollback rapide

```bash
#!/bin/bash
# rollback.sh

set -e

log_info() {
    echo "[INFO] $1"
}

log_error() {
    echo "[ERROR] $1"
}

# Déterminer l'environnement actif
ACTIVE_ENV=$(curl -s http://localhost/health | jq -r '.environment')

if [ "$ACTIVE_ENV" = "blue" ]; then
    ROLLBACK_TO="green"
    ROLLBACK_SERVER="192.168.1.20"
else
    ROLLBACK_TO="blue"
    ROLLBACK_SERVER="192.168.1.10"
fi

log_info "Environnement actuel: $ACTIVE_ENV"  
log_info "ROLLBACK vers: $ROLLBACK_TO"

# Vérifier que l'environnement de rollback est disponible
HEALTH=$(curl -s http://$ROLLBACK_SERVER:8080/health | jq -r '.status')  
if [ "$HEALTH" != "healthy" ]; then
    log_error "L'environnement $ROLLBACK_TO n'est pas disponible!"
    exit 1
fi

# Basculer immédiatement
log_info "Bascule du trafic vers $ROLLBACK_TO..."

# Nginx
if [ "$ROLLBACK_TO" = "blue" ]; then
    sudo sed -i 's/server 192.168.1.20:8080/server 192.168.1.10:8080/' /etc/nginx/sites-available/myapp
else
    sudo sed -i 's/server 192.168.1.10:8080/server 192.168.1.20:8080/' /etc/nginx/sites-available/myapp
fi  
sudo systemctl reload nginx

log_info "ROLLBACK effectué avec succès vers $ROLLBACK_TO"

# Vérification
sleep 2  
NEW_ENV=$(curl -s http://localhost/health | jq -r '.environment')  
log_info "Environnement actif: $NEW_ENV"
```

### Rollback avec drainage des connexions

Pour un rollback plus gracieux qui attend la fin des requêtes en cours :

```bash
#!/bin/bash
# rollback-graceful.sh

log_info "Rollback gracieux - drainage des connexions..."

# Marquer l'environnement actuel en "drain" dans HAProxy
echo "set server blue_back/blue1 state drain" | sudo socat stdio /run/haproxy/admin.sock

# Attendre que les connexions se terminent (max 30 secondes)
for i in {1..30}; do
    ACTIVE_CONNECTIONS=$(echo "show stat" | sudo socat stdio /run/haproxy/admin.sock | grep blue_back | cut -d, -f5)

    if [ "$ACTIVE_CONNECTIONS" -eq 0 ]; then
        log_info "Toutes les connexions sont terminées"
        break
    fi

    log_info "Attente: $ACTIVE_CONNECTIONS connexions actives..."
    sleep 1
done

# Basculer vers l'autre environnement
echo "set server green_back/green1 state ready" | sudo socat stdio /run/haproxy/admin.sock

log_info "Rollback gracieux terminé"
```

## Stratégies avancées

### Canary Release (déploiement progressif)

Au lieu de basculer 100% du trafic d'un coup, vous pouvez commencer par diriger seulement 10% vers le nouvel environnement.

#### Configuration HAProxy pour Canary

```haproxy
frontend http_front
    bind *:80

    # 10% vers Green (canary), 90% vers Blue (stable)
    acl is_canary rand(100) lt 10
    use_backend green_back if is_canary
    default_backend blue_back
```

#### Script de canary progressif

```bash
#!/bin/bash
# canary-deployment.sh

# Étapes : 10% -> 25% -> 50% -> 100%
CANARY_STEPS=(10 25 50 100)  
MONITORING_DURATION=300  # 5 minutes par étape

for PERCENTAGE in "${CANARY_STEPS[@]}"; do
    log_info "Canary à $PERCENTAGE%"

    # Modifier HAProxy pour le pourcentage
    sudo sed -i "s/rand(100) lt [0-9]*/rand(100) lt $PERCENTAGE/" /etc/haproxy/haproxy.cfg
    sudo systemctl reload haproxy

    # Surveiller pendant X minutes
    log_info "Surveillance pendant $MONITORING_DURATION secondes..."
    ./monitor-deployment.sh

    # Vérifier les métriques
    if ! ./check-health-regression.sh; then
        log_error "Régression détectée à $PERCENTAGE%!"
        log_error "Rollback automatique..."
        ./rollback.sh
        exit 1
    fi

    log_info "Étape $PERCENTAGE% réussie !"
done

log_info "Déploiement canary terminé avec succès!"
```

### A/B Testing

Diriger différents utilisateurs vers différents environnements pour comparer les versions.

```nginx
# A/B Testing basé sur un cookie
map $cookie_ab_test $backend {
    default blue_environment;
    "variant_b" green_environment;
}

server {
    listen 80;

    location / {
        proxy_pass http://$backend;
    }
}
```

### Feature Flags

Combinez Blue-Green avec des feature flags pour activer/désactiver des fonctionnalités sans redéploiement.

```pascal
unit FeatureFlags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Redis;

type
  TFeatureFlagManager = class
  private
    FRedisClient: TRedisClient;
    FCache: TJSONObject;
    FCacheTime: TDateTime;
    FCacheTTL: Integer; // Secondes
    procedure RefreshCache;
  public
    constructor Create(const RedisHost: string; RedisPort: Integer);
    destructor Destroy; override;

    function IsEnabled(const FeatureName: string): Boolean;
    procedure Enable(const FeatureName: string);
    procedure Disable(const FeatureName: string);
  end;

implementation

uses
  DateUtils;

constructor TFeatureFlagManager.Create(const RedisHost: string; RedisPort: Integer);  
begin
  FRedisClient := TRedisClient.Create(RedisHost, RedisPort);
  FCache := TJSONObject.Create;
  FCacheTTL := 60; // 1 minute
  RefreshCache;
end;

destructor TFeatureFlagManager.Destroy;  
begin
  FCache.Free;
  FRedisClient.Free;
  inherited;
end;

procedure TFeatureFlagManager.RefreshCache;  
var
  Keys: TStringList;
  i: Integer;
  FeatureName, Value: string;
begin
  // Rafraîchir seulement si le cache est expiré
  if (SecondsBetween(Now, FCacheTime) < FCacheTTL) and (FCache.Count > 0) then
    Exit;

  FCache.Clear;

  Keys := FRedisClient.Keys('feature:*');
  try
    for i := 0 to Keys.Count - 1 do
    begin
      FeatureName := StringReplace(Keys[i], 'feature:', '', []);
      Value := FRedisClient.Get(Keys[i]);
      FCache.Add(FeatureName, Value = 'true');
    end;
  finally
    Keys.Free;
  end;

  FCacheTime := Now;
end;

function TFeatureFlagManager.IsEnabled(const FeatureName: string): Boolean;  
begin
  RefreshCache;

  if FCache.IndexOfName(FeatureName) >= 0 then
    Result := FCache.Booleans[FeatureName]
  else
    Result := False; // Par défaut désactivé
end;

procedure TFeatureFlagManager.Enable(const FeatureName: string);  
begin
  FRedisClient.SetValue('feature:' + FeatureName, 'true');
  RefreshCache;
end;

procedure TFeatureFlagManager.Disable(const FeatureName: string);  
begin
  FRedisClient.SetValue('feature:' + FeatureName, 'false');
  RefreshCache;
end;

end.
```

**Utilisation** :

```pascal
var
  FeatureFlags: TFeatureFlagManager;
begin
  FeatureFlags := TFeatureFlagManager.Create('localhost', 6379);
  try
    if FeatureFlags.IsEnabled('new_ui') then
      ShowNewUI
    else
      ShowOldUI;
  finally
    FeatureFlags.Free;
  end;
end;
```

## Checklist complète d'un déploiement Blue-Green

### Avant le déploiement

- [ ] Code review et tests unitaires passés
- [ ] Tests d'intégration réussis
- [ ] Migrations de base de données rétrocompatibles préparées
- [ ] Documentation mise à jour
- [ ] Feature flags configurés si nécessaire
- [ ] Fenêtre de maintenance communiquée (même si zero downtime)
- [ ] Équipe d'astreinte prévenue
- [ ] Plan de rollback documenté et testé
- [ ] Sauvegardes récentes vérifiées

### Pendant le déploiement

- [ ] Exécuter les migrations de base de données
- [ ] Déployer sur l'environnement inactif (Green ou Blue)
- [ ] Vérifier les logs de démarrage
- [ ] Health check OK sur le nouvel environnement
- [ ] Tests de fumée (smoke tests) réussis
- [ ] Monitoring en place et fonctionnel
- [ ] Basculer le trafic (progressivement si canary)
- [ ] Surveiller les métriques en temps réel
- [ ] Vérifier les logs d'erreurs

### Après le déploiement

- [ ] Monitoring continu pendant au moins 1 heure
- [ ] Vérification des métriques clés (erreurs, latence, throughput)
- [ ] Tests fonctionnels en production
- [ ] Feedback des utilisateurs
- [ ] Conserver l'ancien environnement actif pendant 24-48h
- [ ] Post-mortem si incident
- [ ] Mise à jour de la documentation

### En cas de problème

- [ ] Activer le rollback immédiat
- [ ] Vérifier que l'ancien environnement fonctionne
- [ ] Investiguer les logs et métriques
- [ ] Communication aux utilisateurs si nécessaire
- [ ] Post-mortem et correction pour prochain déploiement

## Outils et automatisation

### GitLab CI/CD pour Blue-Green

```yaml
# .gitlab-ci.yml

stages:
  - build
  - test
  - deploy_staging
  - deploy_production

variables:
  BLUE_SERVER: "192.168.1.10"
  GREEN_SERVER: "192.168.1.20"

build:
  stage: build
  script:
    - lazbuild --build-mode=Release MyProject.lpi
  artifacts:
    paths:
      - myapp
    expire_in: 1 day

test:
  stage: test
  script:
    - ./run_tests.sh
  dependencies:
    - build

deploy_staging:
  stage: deploy_staging
  script:
    - ./deploy-bluegreen.sh staging $CI_COMMIT_TAG
  only:
    - tags
  when: manual

deploy_production:
  stage: deploy_production
  script:
    - ./deploy-bluegreen.sh production $CI_COMMIT_TAG
  only:
    - tags
  when: manual
  environment:
    name: production
    url: https://myapp.example.com
```

### GitHub Actions

```yaml
# .github/workflows/deploy.yml

name: Blue-Green Deployment

on:
  push:
    tags:
      - 'v*'

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Install FreePascal
        run: sudo apt-get install fpc lazarus

      - name: Build
        run: lazbuild --build-mode=Release MyProject.lpi

      - name: Upload artifact
        uses: actions/upload-artifact@v2
        with:
          name: myapp-binary
          path: myapp

  deploy:
    needs: build
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Download artifact
        uses: actions/download-artifact@v2
        with:
          name: myapp-binary

      - name: Deploy Blue-Green
        env:
          SSH_PRIVATE_KEY: ${{ secrets.SSH_PRIVATE_KEY }}
        run: |
          eval $(ssh-agent -s)
          ssh-add - <<< "$SSH_PRIVATE_KEY"
          ./deploy-bluegreen.sh ${{ github.ref_name }}

      - name: Health Check
        run: ./health-check.sh

      - name: Notify success
        if: success()
        run: echo "Deployment successful!"
```

## Conclusion et bonnes pratiques

### Résumé des points clés

1. **Blue-Green élimine les downtimes** mais nécessite une infrastructure adaptée
2. **La base de données est le point critique** : migrations rétrocompatibles obligatoires
3. **Automatisez au maximum** : scripts de déploiement, tests, rollback
4. **Monitoring essentiel** : surveillez avant, pendant et après
5. **Testez votre rollback régulièrement** : il doit être aussi fiable que le déploiement
6. **Commencez simple** : un déploiement Blue-Green basique vaut mieux qu'un système complexe non testé

### Quand utiliser Blue-Green ?

**Cas d'usage idéaux** :
- Applications web stateless
- APIs REST
- Microservices
- Applications critiques nécessitant zero downtime
- Équipes pratiquant le déploiement continu

**Moins adapté pour** :
- Applications monolithiques avec état fortement couplé
- Systèmes avec migrations de base de données complexes non-rétrocompatibles
- Petites applications où le downtime de quelques minutes est acceptable
- Contraintes budgétaires fortes (double infrastructure temporaire)

### Évolution vers d'autres stratégies

Blue-Green est un excellent point de départ, mais vous pouvez évoluer vers :

- **Canary Releases** : Déploiement progressif plus granulaire
- **Rolling Updates** : Mise à jour instance par instance
- **Feature Toggles** : Découplage déploiement et activation de fonctionnalités
- **A/B Testing** : Tests comparatifs en production

### Ressources complémentaires

- **Livres** :
  - "Continuous Delivery" par Jez Humble et David Farley
  - "The DevOps Handbook" par Gene Kim

- **Articles** :
  - Martin Fowler : "BlueGreenDeployment"
  - Netflix Tech Blog : Déploiements à grande échelle

- **Outils** :
  - Kubernetes : Gestion native de Blue-Green
  - Terraform : Infrastructure as Code
  - Ansible : Automatisation de déploiements

---

Avec cette approche Blue-Green bien maîtrisée, vos déploiements FreePascal/Lazarus deviennent des opérations de routine sans stress, permettant des mises à jour fréquentes et sûres, que ce soit sur Windows ou Ubuntu. La clé du succès réside dans l'automatisation, le monitoring et la pratique régulière !

⏭️ [Feature flags et A/B testing](/22-devops-deploiement-multi-os/10-feature-flags-ab-testing.md)
