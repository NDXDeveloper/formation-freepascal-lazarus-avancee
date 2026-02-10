🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.11 Déploiement sur serveurs Windows/Linux

## Introduction

Le déploiement d'applications web FreePascal nécessite de configurer correctement les serveurs, qu'ils soient sous Windows ou Linux. Ce chapitre couvre toutes les étapes nécessaires pour mettre en production vos applications de manière sécurisée et performante.

**Qu'est-ce que le déploiement ?**

Le déploiement est le processus de transfert d'une application depuis l'environnement de développement vers un serveur de production où elle sera accessible aux utilisateurs finaux.

```
┌──────────────────┐         ┌──────────────────┐
│  Développement   │         │    Production    │
│   (Local)        │  ──►    │    (Serveur)     │
│                  │         │                  │
│  - Code source   │         │  - Exécutable    │
│  - Tests         │         │  - Configuration │
│  - Debugging     │         │  - Monitoring    │
└──────────────────┘         └──────────────────┘
```

## 9.11.1 Préparation de l'application

### Compilation pour la production

**Optimisations de compilation :**

```bash
# Compilation optimisée pour Linux
fpc -O3 -Xs -XX -CX mywebapp.pas

# Compilation optimisée pour Windows
fpc -O3 -Xs -XX -CX -Twin64 mywebapp.pas

# Options expliquées :
# -O3      : Optimisation maximale
# -Xs      : Strip symbols (réduire la taille)
# -XX      : Smart linking
# -CX      : Smart linking avancé
# -Twin64  : Cible Windows 64 bits
```

### Configuration par environnement

**config.pas :**

```pascal
unit Config;

{$mode objfpc}{$H+}

interface

type
  TEnvironment = (envDevelopment, envStaging, envProduction);

  TConfig = class
  private
    FEnvironment: TEnvironment;
    FDatabaseHost: string;
    FDatabasePort: Integer;
    FDatabaseName: string;
    FServerPort: Integer;
    FLogLevel: string;
  public
    constructor Create;
    procedure LoadFromEnvironment;
    property Environment: TEnvironment read FEnvironment;
    property DatabaseHost: string read FDatabaseHost;
    property DatabasePort: Integer read FDatabasePort;
    property DatabaseName: string read FDatabaseName;
    property ServerPort: Integer read FServerPort;
    property LogLevel: string read FLogLevel;
  end;

implementation

uses
  SysUtils;

constructor TConfig.Create;
begin
  inherited Create;
  LoadFromEnvironment;
end;

procedure TConfig.LoadFromEnvironment;
var
  EnvStr: string;
begin
  // Déterminer l'environnement
  EnvStr := GetEnvironmentVariable('APP_ENV');

  if EnvStr = 'production' then
    FEnvironment := envProduction
  else if EnvStr = 'staging' then
    FEnvironment := envStaging
  else
    FEnvironment := envDevelopment;

  // Configuration selon l'environnement
  case FEnvironment of
    envDevelopment:
      begin
        FDatabaseHost := 'localhost';
        FDatabasePort := 5432;
        FDatabaseName := 'myapp_dev';
        FServerPort := 8080;
        FLogLevel := 'DEBUG';
      end;

    envStaging:
      begin
        FDatabaseHost := GetEnvironmentVariable('DB_HOST');
        FDatabasePort := StrToIntDef(GetEnvironmentVariable('DB_PORT'), 5432);
        FDatabaseName := GetEnvironmentVariable('DB_NAME');
        FServerPort := StrToIntDef(GetEnvironmentVariable('SERVER_PORT'), 8080);
        FLogLevel := 'INFO';
      end;

    envProduction:
      begin
        FDatabaseHost := GetEnvironmentVariable('DB_HOST');
        FDatabasePort := StrToIntDef(GetEnvironmentVariable('DB_PORT'), 5432);
        FDatabaseName := GetEnvironmentVariable('DB_NAME');
        FServerPort := StrToIntDef(GetEnvironmentVariable('SERVER_PORT'), 80);
        FLogLevel := 'WARNING';
      end;
  end;

  WriteLn('Configuration chargée pour environnement: ', EnvStr);
end;

end.
```

### Scripts de build multi-plateformes

**build.sh (Linux) :**

```bash
#!/bin/bash

set -e  # Arrêter en cas d'erreur

echo "==================================="
echo "  Build pour Production - Linux"
echo "==================================="

# Variables
APP_NAME="mywebapp"
BUILD_DIR="build"
DIST_DIR="dist"

# Nettoyage
echo "Nettoyage..."
rm -rf $BUILD_DIR $DIST_DIR
mkdir -p $BUILD_DIR $DIST_DIR

# Compilation
echo "Compilation..."
fpc -O3 -Xs -XX -CX \
    -FU$BUILD_DIR \
    -o$DIST_DIR/$APP_NAME \
    src/$APP_NAME.pas

if [ $? -eq 0 ]; then
    echo "✓ Compilation réussie"
else
    echo "✗ Erreur de compilation"
    exit 1
fi

# Copie des fichiers nécessaires
echo "Copie des ressources..."
cp -r config $DIST_DIR/
cp -r templates $DIST_DIR/
cp -r public $DIST_DIR/

# Création de l'archive
echo "Création de l'archive..."
cd $DIST_DIR
tar -czf ../${APP_NAME}_linux_$(date +%Y%m%d).tar.gz *
cd ..

echo "✓ Build terminé"
echo "Archive: ${APP_NAME}_linux_$(date +%Y%m%d).tar.gz"
```

**build.bat (Windows) :**

```batch
@echo off
setlocal enabledelayedexpansion

echo ===================================
echo   Build pour Production - Windows
echo ===================================

REM Variables
set APP_NAME=mywebapp
set BUILD_DIR=build
set DIST_DIR=dist

REM Nettoyage
echo Nettoyage...
if exist %BUILD_DIR% rmdir /s /q %BUILD_DIR%
if exist %DIST_DIR% rmdir /s /q %DIST_DIR%
mkdir %BUILD_DIR%
mkdir %DIST_DIR%

REM Compilation
echo Compilation...
fpc -O3 -Xs -XX -CX -Twin64 ^
    -FU%BUILD_DIR% ^
    -o%DIST_DIR%\%APP_NAME%.exe ^
    src\%APP_NAME%.pas

if %ERRORLEVEL% EQU 0 (
    echo Compilation reussie
) else (
    echo Erreur de compilation
    exit /b 1
)

REM Copie des fichiers nécessaires
echo Copie des ressources...
xcopy /E /I config %DIST_DIR%\config
xcopy /E /I templates %DIST_DIR%\templates
xcopy /E /I public %DIST_DIR%\public

REM Création de l'archive
echo Creation de l'archive...
cd %DIST_DIR%
tar -czf ..\%APP_NAME%_windows_%date:~-4,4%%date:~-10,2%%date:~-7,2%.tar.gz *
cd ..

echo Build termine
echo Archive: %APP_NAME%_windows_%date:~-4,4%%date:~-10,2%%date:~-7,2%.tar.gz
```

## 9.11.2 Déploiement sur Linux (Ubuntu/Debian)

### Installation des dépendances

```bash
# Mise à jour du système
sudo apt-get update
sudo apt-get upgrade -y

# Installation des outils nécessaires
sudo apt-get install -y nginx postgresql supervisor

# Création de l'utilisateur pour l'application
sudo useradd -r -s /bin/false mywebapp

# Création des répertoires
sudo mkdir -p /opt/mywebapp
sudo mkdir -p /var/log/mywebapp
sudo mkdir -p /etc/mywebapp

# Permissions
sudo chown -R mywebapp:mywebapp /opt/mywebapp
sudo chown -R mywebapp:mywebapp /var/log/mywebapp
```

### Déploiement de l'application

```bash
# Copier l'application
sudo tar -xzf mywebapp_linux_*.tar.gz -C /opt/mywebapp/

# Rendre l'exécutable
sudo chmod +x /opt/mywebapp/mywebapp

# Créer le fichier de configuration
sudo tee /etc/mywebapp/config.env > /dev/null <<EOF
APP_ENV=production
DB_HOST=localhost
DB_PORT=5432
DB_NAME=mywebapp_prod
DB_USER=mywebapp
DB_PASSWORD=SecurePassword123
SERVER_PORT=8080
LOG_LEVEL=INFO
EOF

# Protéger le fichier de configuration
sudo chmod 600 /etc/mywebapp/config.env
sudo chown mywebapp:mywebapp /etc/mywebapp/config.env
```

### Configuration de Supervisor

Supervisor permet de gérer l'application comme un service et de la redémarrer automatiquement en cas de crash.

**/etc/supervisor/conf.d/mywebapp.conf :**

```ini
[program:mywebapp]
command=/opt/mywebapp/mywebapp
directory=/opt/mywebapp
user=mywebapp
autostart=true
autorestart=true
redirect_stderr=true
stdout_logfile=/var/log/mywebapp/app.log
stdout_logfile_maxbytes=10MB
stdout_logfile_backups=10
environment=APP_ENV="production",DB_HOST="localhost",DB_PORT="5432"

[group:mywebapp]
programs=mywebapp
priority=999
```

**Commandes Supervisor :**

```bash
# Recharger la configuration
sudo supervisorctl reread
sudo supervisorctl update

# Démarrer l'application
sudo supervisorctl start mywebapp

# Vérifier le statut
sudo supervisorctl status mywebapp

# Arrêter l'application
sudo supervisorctl stop mywebapp

# Redémarrer l'application
sudo supervisorctl restart mywebapp

# Voir les logs
sudo supervisorctl tail -f mywebapp
```

### Configuration Nginx comme reverse proxy

**/etc/nginx/sites-available/mywebapp :**

```nginx
# Configuration upstream
upstream mywebapp_backend {
    server 127.0.0.1:8080;
    keepalive 32;
}

# Redirection HTTP vers HTTPS
server {
    listen 80;
    listen [::]:80;
    server_name monapp.example.com;

    # Redirection permanente vers HTTPS
    return 301 https://$server_name$request_uri;
}

# Configuration HTTPS
server {
    listen 443 ssl http2;
    listen [::]:443 ssl http2;
    server_name monapp.example.com;

    # Certificats SSL (Let's Encrypt)
    ssl_certificate /etc/letsencrypt/live/monapp.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/monapp.example.com/privkey.pem;

    # Configuration SSL moderne
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers HIGH:!aNULL:!MD5;
    ssl_prefer_server_ciphers on;
    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;

    # Headers de sécurité
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
    add_header X-Frame-Options "SAMEORIGIN" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header X-XSS-Protection "1; mode=block" always;

    # Logs
    access_log /var/log/nginx/mywebapp_access.log;
    error_log /var/log/nginx/mywebapp_error.log;

    # Taille maximale des uploads
    client_max_body_size 50M;

    # Fichiers statiques
    location /static/ {
        alias /opt/mywebapp/public/;
        expires 1y;
        add_header Cache-Control "public, immutable";
    }

    # Proxy vers l'application FreePascal
    location / {
        proxy_pass http://mywebapp_backend;
        proxy_http_version 1.1;

        # Headers
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header Connection "";

        # Timeouts
        proxy_connect_timeout 60s;
        proxy_send_timeout 60s;
        proxy_read_timeout 60s;

        # Buffering
        proxy_buffering on;
        proxy_buffer_size 4k;
        proxy_buffers 8 4k;
    }

    # WebSocket support (si nécessaire)
    location /ws/ {
        proxy_pass http://mywebapp_backend;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_read_timeout 86400;
    }
}
```

**Activation de la configuration :**

```bash
# Créer le lien symbolique
sudo ln -s /etc/nginx/sites-available/mywebapp /etc/nginx/sites-enabled/

# Tester la configuration
sudo nginx -t

# Recharger Nginx
sudo systemctl reload nginx
```

### Installation du certificat SSL avec Let's Encrypt

```bash
# Installer Certbot
sudo apt-get install -y certbot python3-certbot-nginx

# Obtenir le certificat
sudo certbot --nginx -d monapp.example.com

# Le renouvellement automatique est configuré
# Vérifier avec :
sudo certbot renew --dry-run
```

### Configuration de PostgreSQL

```bash
# Se connecter à PostgreSQL
sudo -u postgres psql

# Créer la base de données et l'utilisateur
CREATE DATABASE mywebapp_prod;
CREATE USER mywebapp WITH ENCRYPTED PASSWORD 'SecurePassword123';
GRANT ALL PRIVILEGES ON DATABASE mywebapp_prod TO mywebapp;

# Sortir
\q

# Configuration de PostgreSQL pour accepter les connexions locales
# Éditer /etc/postgresql/*/main/pg_hba.conf
sudo nano /etc/postgresql/15/main/pg_hba.conf

# Ajouter cette ligne :
# local   mywebapp_prod   mywebapp                md5

# Redémarrer PostgreSQL
sudo systemctl restart postgresql
```

### Firewall (UFW)

```bash
# Activer le firewall
sudo ufw enable

# Autoriser SSH
sudo ufw allow 22/tcp

# Autoriser HTTP et HTTPS
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp

# Vérifier les règles
sudo ufw status
```

## 9.11.3 Déploiement sur Windows Server

### Installation des dépendances

**PowerShell (Administrateur) :**

```powershell
# Installer Chocolatey (gestionnaire de paquets)
Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Installer les outils nécessaires
choco install -y iis-urlrewrite
choco install -y nssm
choco install -y postgresql15

# Activer IIS
Enable-WindowsOptionalFeature -Online -FeatureName IIS-WebServerRole
Enable-WindowsOptionalFeature -Online -FeatureName IIS-WebServer
Enable-WindowsOptionalFeature -Online -FeatureName IIS-ApplicationDevelopment
```

### Déploiement de l'application

```powershell
# Créer les répertoires
New-Item -Path "C:\inetpub\mywebapp" -ItemType Directory
New-Item -Path "C:\ProgramData\mywebapp\config" -ItemType Directory
New-Item -Path "C:\ProgramData\mywebapp\logs" -ItemType Directory

# Extraire l'application
Expand-Archive -Path "mywebapp_windows_*.tar.gz" -DestinationPath "C:\inetpub\mywebapp"

# Créer le fichier de configuration
@"
APP_ENV=production
DB_HOST=localhost
DB_PORT=5432
DB_NAME=mywebapp_prod
DB_USER=mywebapp
DB_PASSWORD=SecurePassword123
SERVER_PORT=8080
LOG_LEVEL=INFO
"@ | Out-File -FilePath "C:\ProgramData\mywebapp\config\config.env" -Encoding UTF8

# Configurer les permissions
$acl = Get-Acl "C:\inetpub\mywebapp"
$rule = New-Object System.Security.AccessControl.FileSystemAccessRule(
    "IIS_IUSRS", "ReadAndExecute", "ContainerInherit,ObjectInherit", "None", "Allow"
)
$acl.SetAccessRule($rule)
Set-Acl "C:\inetpub\mywebapp" $acl
```

### Installation comme service Windows avec NSSM

```powershell
# Installer l'application comme service
nssm install MyWebApp "C:\inetpub\mywebapp\mywebapp.exe"

# Configuration du service
nssm set MyWebApp AppDirectory "C:\inetpub\mywebapp"
nssm set MyWebApp AppEnvironmentExtra "APP_ENV=production" "DB_HOST=localhost"
nssm set MyWebApp DisplayName "My Web Application"
nssm set MyWebApp Description "Application web FreePascal"
nssm set MyWebApp Start SERVICE_AUTO_START
nssm set MyWebApp AppStdout "C:\ProgramData\mywebapp\logs\app.log"
nssm set MyWebApp AppStderr "C:\ProgramData\mywebapp\logs\error.log"

# Démarrer le service
nssm start MyWebApp

# Vérifier le statut
nssm status MyWebApp

# Autres commandes utiles :
# nssm stop MyWebApp
# nssm restart MyWebApp
# nssm remove MyWebApp confirm
```

### Configuration IIS comme reverse proxy

**1. Installer ARR (Application Request Routing) :**

```powershell
# Télécharger et installer ARR
choco install -y iis-arr

# Activer le proxy
Import-Module WebAdministration
Set-WebConfigurationProperty -pspath 'MACHINE/WEBROOT/APPHOST' `
    -filter "system.webServer/proxy" -name "enabled" -value "True"
```

**2. Créer le site IIS :**

```powershell
# Créer le site
New-Website -Name "MyWebApp" `
    -Port 80 `
    -PhysicalPath "C:\inetpub\wwwroot\mywebapp" `
    -ApplicationPool "DefaultAppPool"

# Configurer le binding HTTPS
New-WebBinding -Name "MyWebApp" -Protocol https -Port 443
```

**3. Configuration du reverse proxy (web.config) :**

**C:\inetpub\wwwroot\mywebapp\web.config :**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<configuration>
    <system.webServer>
        <!-- URL Rewrite pour le reverse proxy -->
        <rewrite>
            <rules>
                <rule name="ReverseProxyInboundRule" stopProcessing="true">
                    <match url="(.*)" />
                    <action type="Rewrite" url="http://localhost:8080/{R:1}" />
                    <serverVariables>
                        <set name="HTTP_X_FORWARDED_HOST" value="{HTTP_HOST}" />
                        <set name="HTTP_X_FORWARDED_FOR" value="{REMOTE_ADDR}" />
                        <set name="HTTP_X_FORWARDED_PROTO" value="https" />
                    </serverVariables>
                </rule>
            </rules>
        </rewrite>

        <!-- Compression -->
        <httpCompression>
            <scheme name="gzip" dll="%Windir%\system32\inetsrv\gzip.dll" />
            <dynamicTypes>
                <add mimeType="text/*" enabled="true" />
                <add mimeType="application/json" enabled="true" />
                <add mimeType="*/*" enabled="false" />
            </dynamicTypes>
            <staticTypes>
                <add mimeType="text/*" enabled="true" />
                <add mimeType="application/javascript" enabled="true" />
                <add mimeType="*/*" enabled="false" />
            </staticTypes>
        </httpCompression>

        <!-- Headers de sécurité -->
        <httpProtocol>
            <customHeaders>
                <add name="X-Frame-Options" value="SAMEORIGIN" />
                <add name="X-Content-Type-Options" value="nosniff" />
                <add name="X-XSS-Protection" value="1; mode=block" />
                <add name="Strict-Transport-Security" value="max-age=31536000" />
            </customHeaders>
        </httpProtocol>

        <!-- Limite de taille d'upload -->
        <security>
            <requestFiltering>
                <requestLimits maxAllowedContentLength="52428800" />
            </requestFiltering>
        </security>
    </system.webServer>
</configuration>
```

### Configuration du certificat SSL

```powershell
# Avec Win-ACME (Let's Encrypt pour Windows)
choco install -y win-acme

# Exécuter Win-ACME
wacs.exe --target iis --siteid 1

# Ou importer un certificat manuel
Import-PfxCertificate -FilePath "C:\cert\mycert.pfx" `
    -CertStoreLocation Cert:\LocalMachine\My `
    -Password (ConvertTo-SecureString -String "password" -AsPlainText -Force)

# Lier le certificat au site
New-WebBinding -Name "MyWebApp" -Protocol https -Port 443
$cert = Get-ChildItem -Path Cert:\LocalMachine\My | Where-Object {$_.Subject -like "*monapp.example.com*"}
$binding = Get-WebBinding -Name "MyWebApp" -Protocol https
$binding.AddSslCertificate($cert.Thumbprint, "my")
```

### Configuration du pare-feu Windows

```powershell
# Autoriser HTTP
New-NetFirewallRule -DisplayName "HTTP" -Direction Inbound `
    -Protocol TCP -LocalPort 80 -Action Allow

# Autoriser HTTPS
New-NetFirewallRule -DisplayName "HTTPS" -Direction Inbound `
    -Protocol TCP -LocalPort 443 -Action Allow

# Autoriser le port de l'application (si accès direct nécessaire)
New-NetFirewallRule -DisplayName "MyWebApp" -Direction Inbound `
    -Protocol TCP -LocalPort 8080 -Action Allow
```

## 9.11.4 Monitoring et logs

### Configuration des logs en FreePascal

**logger.pas :**

```pascal
unit Logger;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  TLogger = class
  private
    FLogFile: string;
    FLogLevel: TLogLevel;
    FFileStream: TFileStream;
    procedure WriteToFile(const Message: string);
  public
    constructor Create(const LogFilePath: string; Level: TLogLevel = llInfo);
    destructor Destroy; override;
    procedure Debug(const Message: string);
    procedure Info(const Message: string);
    procedure Warning(const Message: string);
    procedure Error(const Message: string);
    procedure Critical(const Message: string);
    procedure Log(Level: TLogLevel; const Message: string);
  end;

implementation

constructor TLogger.Create(const LogFilePath: string; Level: TLogLevel);
begin
  inherited Create;
  FLogFile := LogFilePath;
  FLogLevel := Level;

  // Créer le répertoire si nécessaire
  ForceDirectories(ExtractFilePath(FLogFile));

  // Ouvrir le fichier en mode append
  if FileExists(FLogFile) then
    FFileStream := TFileStream.Create(FLogFile, fmOpenWrite or fmShareDenyWrite)
  else
    FFileStream := TFileStream.Create(FLogFile, fmCreate);

  FFileStream.Seek(0, soEnd);
end;

destructor TLogger.Destroy;
begin
  FFileStream.Free;
  inherited Destroy;
end;

procedure TLogger.WriteToFile(const Message: string);
var
  Line: string;
begin
  Line := Message + LineEnding;
  FFileStream.WriteBuffer(Line[1], Length(Line));
end;

procedure TLogger.Log(Level: TLogLevel; const Message: string);
const
  LevelNames: array[TLogLevel] of string = ('DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL');
var
  LogMessage: string;
begin
  if Level < FLogLevel then
    Exit;

  LogMessage := Format('[%s] [%s] %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    LevelNames[Level],
    Message
  ]);

  WriteToFile(LogMessage);
  WriteLn(LogMessage); // Aussi sur la console
end;

procedure TLogger.Debug(const Message: string);
begin
  Log(llDebug, Message);
end;

procedure TLogger.Info(const Message: string);
begin
  Log(llInfo, Message);
end;

procedure TLogger.Warning(const Message: string);
begin
  Log(llWarning, Message);
end;

procedure TLogger.Error(const Message: string);
begin
  Log(llError, Message);
end;

procedure TLogger.Critical(const Message: string);
begin
  Log(llCritical, Message);
end;

end.
```

### Rotation des logs (Linux)

**/etc/logrotate.d/mywebapp :**

```
/var/log/mywebapp/*.log {
    daily
    missingok
    rotate 14
    compress
    delaycompress
    notifempty
    create 0640 mywebapp mywebapp
    sharedscripts
    postrotate
        supervisorctl restart mywebapp > /dev/null
    endscript
}
```

### Rotation des logs (Windows)

**PowerShell script - rotate_logs.ps1 :**

```powershell
# Configuration
$LogPath = "C:\ProgramData\mywebapp\logs"
$MaxAgeDays = 14
$MaxSizeMB = 50

# Obtenir tous les fichiers logs
$LogFiles = Get-ChildItem -Path $LogPath -Filter "*.log"

foreach ($LogFile in $LogFiles) {
    # Vérifier l'âge du fichier
    $FileAge = (Get-Date) - $LogFile.LastWriteTime

    if ($FileAge.Days -gt $MaxAgeDays) {
        Write-Host "Suppression de $($LogFile.Name) (age: $($FileAge.Days) jours)"
        Remove-Item $LogFile.FullName -Force
        continue
    }

    # Vérifier la taille du fichier
    $FileSizeMB = $LogFile.Length / 1MB

    if ($FileSizeMB -gt $MaxSizeMB) {
        $NewName = $LogFile.BaseName + "_" + (Get-Date -Format "yyyyMMdd_HHmmss") + ".log"
        $NewPath = Join-Path $LogPath $NewName

        Write-Host "Rotation de $($LogFile.Name) vers $NewName"
        Move-Item $LogFile.FullName $NewPath -Force

        # Compresser l'ancien fichier
        Compress-Archive -Path $NewPath -DestinationPath "$NewPath.zip" -Force
        Remove-Item $NewPath -Force
    }
}

Write-Host "Rotation des logs terminee"
```

**Planifier la rotation (Tâche planifiée Windows) :**

```powershell
# Créer une tâche planifiée pour exécuter le script quotidiennement
$Action = New-ScheduledTaskAction -Execute "PowerShell.exe" `
    -Argument "-ExecutionPolicy Bypass -File C:\Scripts\rotate_logs.ps1"

$Trigger = New-ScheduledTaskTrigger -Daily -At 2am

$Principal = New-ScheduledTaskPrincipal -UserId "SYSTEM" `
    -LogonType ServiceAccount -RunLevel Highest

Register-ScheduledTask -TaskName "MyWebApp-LogRotation" `
    -Action $Action `
    -Trigger $Trigger `
    -Principal $Principal `
    -Description "Rotation quotidienne des logs de MyWebApp"
```

### Monitoring avec Prometheus

**Exposition des métriques en FreePascal :**

```pascal
unit Metrics;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs;

type
  TMetricsCollector = class
  private
    FRequestCount: Int64;
    FErrorCount: Int64;
    FActiveRequests: Integer;
    FTotalResponseTime: Double;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncRequestCount;
    procedure IncErrorCount;
    procedure IncActiveRequests;
    procedure DecActiveRequests;
    procedure AddResponseTime(TimeMs: Double);
    function GetMetrics: string;
  end;

implementation

constructor TMetricsCollector.Create;
begin
  inherited Create;
  FRequestCount := 0;
  FErrorCount := 0;
  FActiveRequests := 0;
  FTotalResponseTime := 0;
  FLock := TCriticalSection.Create;
end;

destructor TMetricsCollector.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TMetricsCollector.IncRequestCount;
begin
  FLock.Enter;
  try
    Inc(FRequestCount);
  finally
    FLock.Leave;
  end;
end;

procedure TMetricsCollector.IncErrorCount;
begin
  FLock.Enter;
  try
    Inc(FErrorCount);
  finally
    FLock.Leave;
  end;
end;

procedure TMetricsCollector.IncActiveRequests;
begin
  FLock.Enter;
  try
    Inc(FActiveRequests);
  finally
    FLock.Leave;
  end;
end;

procedure TMetricsCollector.DecActiveRequests;
begin
  FLock.Enter;
  try
    Dec(FActiveRequests);
  finally
    FLock.Leave;
  end;
end;

procedure TMetricsCollector.AddResponseTime(TimeMs: Double);
begin
  FLock.Enter;
  try
    FTotalResponseTime := FTotalResponseTime + TimeMs;
  finally
    FLock.Leave;
  end;
end;

function TMetricsCollector.GetMetrics: string;
var
  AvgResponseTime: Double;
begin
  FLock.Enter;
  try
    if FRequestCount > 0 then
      AvgResponseTime := FTotalResponseTime / FRequestCount
    else
      AvgResponseTime := 0;

    Result := Format(
      '# HELP http_requests_total Total number of HTTP requests'#10 +
      '# TYPE http_requests_total counter'#10 +
      'http_requests_total %d'#10 +
      #10 +
      '# HELP http_errors_total Total number of HTTP errors'#10 +
      '# TYPE http_errors_total counter'#10 +
      'http_errors_total %d'#10 +
      #10 +
      '# HELP http_requests_active Currently active HTTP requests'#10 +
      '# TYPE http_requests_active gauge'#10 +
      'http_requests_active %d'#10 +
      #10 +
      '# HELP http_response_time_avg Average response time in milliseconds'#10 +
      '# TYPE http_response_time_avg gauge'#10 +
      'http_response_time_avg %.2f'#10,
      [FRequestCount, FErrorCount, FActiveRequests, AvgResponseTime]
    );
  finally
    FLock.Leave;
  end;
end;

end.
```

**Endpoint de métriques :**

```pascal
procedure HandleMetrics(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.ContentType := 'text/plain; version=0.0.4';
  AResponse.Content := GlobalMetrics.GetMetrics;
  AResponse.SendResponse;
end;

// Enregistrer la route
HTTPRouter.RegisterRoute('/metrics', @HandleMetrics);
```

**Configuration Prometheus (prometheus.yml) :**

```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'mywebapp'
    static_configs:
      - targets: ['localhost:8080']
        labels:
          instance: 'prod-server-1'
          environment: 'production'
```

### Monitoring avec Grafana

**Installation de Grafana :**

**Linux :**

```bash
# Ajouter le repository Grafana
sudo apt-get install -y software-properties-common
sudo add-apt-repository "deb https://packages.grafana.com/oss/deb stable main"
wget -q -O - https://packages.grafana.com/gpg.key | sudo apt-key add -

# Installer Grafana
sudo apt-get update
sudo apt-get install -y grafana

# Démarrer Grafana
sudo systemctl enable grafana-server
sudo systemctl start grafana-server

# Grafana sera accessible sur http://localhost:3000
# Login par défaut : admin / admin
```

**Windows :**

```powershell
# Télécharger et installer Grafana
choco install -y grafana

# Démarrer le service
Start-Service grafana

# Ou télécharger manuellement depuis https://grafana.com/grafana/download
```

**Exemple de dashboard JSON pour Grafana :**

```json
{
  "dashboard": {
    "title": "MyWebApp Monitoring",
    "panels": [
      {
        "title": "Requêtes HTTP Total",
        "type": "graph",
        "targets": [
          {
            "expr": "http_requests_total",
            "legendFormat": "Requêtes totales"
          }
        ]
      },
      {
        "title": "Requêtes actives",
        "type": "graph",
        "targets": [
          {
            "expr": "http_requests_active",
            "legendFormat": "Requêtes en cours"
          }
        ]
      },
      {
        "title": "Temps de réponse moyen",
        "type": "graph",
        "targets": [
          {
            "expr": "http_response_time_avg",
            "legendFormat": "Temps moyen (ms)"
          }
        ]
      },
      {
        "title": "Taux d'erreurs",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(http_errors_total[5m])",
            "legendFormat": "Erreurs/seconde"
          }
        ]
      }
    ]
  }
}
```

## 9.11.5 Sauvegarde et restauration

### Script de sauvegarde automatique (Linux)

**backup.sh :**

```bash
#!/bin/bash

# Configuration
APP_NAME="mywebapp"
BACKUP_DIR="/var/backups/mywebapp"
DB_NAME="mywebapp_prod"
DB_USER="mywebapp"
RETENTION_DAYS=30
DATE=$(date +%Y%m%d_%H%M%S)

# Créer le répertoire de sauvegarde
mkdir -p $BACKUP_DIR

echo "=== Sauvegarde de $APP_NAME - $DATE ==="

# 1. Sauvegarde de la base de données
echo "Sauvegarde de la base de données..."
pg_dump -U $DB_USER -h localhost $DB_NAME | gzip > "$BACKUP_DIR/db_${DATE}.sql.gz"

if [ $? -eq 0 ]; then
    echo "✓ Base de données sauvegardée"
else
    echo "✗ Erreur lors de la sauvegarde de la base de données"
    exit 1
fi

# 2. Sauvegarde des fichiers de l'application
echo "Sauvegarde des fichiers..."
tar -czf "$BACKUP_DIR/files_${DATE}.tar.gz" \
    -C /opt/$APP_NAME \
    --exclude='logs' \
    --exclude='tmp' \
    .

if [ $? -eq 0 ]; then
    echo "✓ Fichiers sauvegardés"
else
    echo "✗ Erreur lors de la sauvegarde des fichiers"
    exit 1
fi

# 3. Sauvegarde de la configuration
echo "Sauvegarde de la configuration..."
tar -czf "$BACKUP_DIR/config_${DATE}.tar.gz" /etc/mywebapp

# 4. Nettoyage des anciennes sauvegardes
echo "Nettoyage des anciennes sauvegardes (>$RETENTION_DAYS jours)..."
find $BACKUP_DIR -name "*.gz" -mtime +$RETENTION_DAYS -delete

# 5. Rapport de sauvegarde
BACKUP_SIZE=$(du -sh $BACKUP_DIR | cut -f1)
echo ""
echo "=== Sauvegarde terminée ==="
echo "Date: $DATE"
echo "Taille totale: $BACKUP_SIZE"
echo "Fichiers:"
ls -lh $BACKUP_DIR/*${DATE}*

# 6. Optionnel : Copier vers un stockage distant
# rsync -avz $BACKUP_DIR/ user@backup-server:/backups/mywebapp/
```

**Planifier la sauvegarde (cron) :**

```bash
# Éditer le crontab
sudo crontab -e

# Ajouter une ligne pour sauvegarder chaque jour à 2h du matin
0 2 * * * /opt/mywebapp/scripts/backup.sh >> /var/log/mywebapp/backup.log 2>&1
```

### Script de sauvegarde automatique (Windows)

**backup.ps1 :**

```powershell
# Configuration
$AppName = "mywebapp"
$BackupDir = "C:\Backups\mywebapp"
$DBName = "mywebapp_prod"
$DBUser = "mywebapp"
$RetentionDays = 30
$Date = Get-Date -Format "yyyyMMdd_HHmmss"

# Créer le répertoire de sauvegarde
if (-not (Test-Path $BackupDir)) {
    New-Item -Path $BackupDir -ItemType Directory | Out-Null
}

Write-Host "=== Sauvegarde de $AppName - $Date ==="

# 1. Sauvegarde de la base de données PostgreSQL
Write-Host "Sauvegarde de la base de données..."
$DBBackupFile = Join-Path $BackupDir "db_${Date}.sql"
$env:PGPASSWORD = "SecurePassword123"

& "C:\Program Files\PostgreSQL\15\bin\pg_dump.exe" `
    -U $DBUser `
    -h localhost `
    $DBName `
    -f $DBBackupFile

if ($LASTEXITCODE -eq 0) {
    # Compresser la sauvegarde
    Compress-Archive -Path $DBBackupFile -DestinationPath "$DBBackupFile.zip" -Force
    Remove-Item $DBBackupFile
    Write-Host "✓ Base de données sauvegardée"
} else {
    Write-Host "✗ Erreur lors de la sauvegarde de la base de données"
    exit 1
}

# 2. Sauvegarde des fichiers de l'application
Write-Host "Sauvegarde des fichiers..."
$FilesBackup = Join-Path $BackupDir "files_${Date}.zip"
$SourcePath = "C:\inetpub\mywebapp"

$FilesToBackup = Get-ChildItem -Path $SourcePath -Recurse |
    Where-Object { $_.FullName -notlike "*\logs\*" -and $_.FullName -notlike "*\tmp\*" }

Compress-Archive -Path $FilesToBackup.FullName -DestinationPath $FilesBackup -Force
Write-Host "✓ Fichiers sauvegardés"

# 3. Sauvegarde de la configuration
Write-Host "Sauvegarde de la configuration..."
$ConfigBackup = Join-Path $BackupDir "config_${Date}.zip"
Compress-Archive -Path "C:\ProgramData\mywebapp\config" -DestinationPath $ConfigBackup -Force

# 4. Nettoyage des anciennes sauvegardes
Write-Host "Nettoyage des anciennes sauvegardes (>$RetentionDays jours)..."
$CutoffDate = (Get-Date).AddDays(-$RetentionDays)
Get-ChildItem -Path $BackupDir -Filter "*.zip" |
    Where-Object { $_.LastWriteTime -lt $CutoffDate } |
    Remove-Item -Force

# 5. Rapport de sauvegarde
$BackupSize = (Get-ChildItem -Path $BackupDir -Recurse | Measure-Object -Property Length -Sum).Sum / 1MB
Write-Host ""
Write-Host "=== Sauvegarde terminée ==="
Write-Host "Date: $Date"
Write-Host "Taille totale: $([Math]::Round($BackupSize, 2)) MB"
Write-Host "Fichiers:"
Get-ChildItem -Path $BackupDir -Filter "*${Date}*" | Format-Table Name, Length, LastWriteTime

# 6. Optionnel : Copier vers un stockage réseau
# Copy-Item -Path "$BackupDir\*" -Destination "\\backup-server\backups\mywebapp\" -Force
```

**Planifier la sauvegarde (Windows) :**

```powershell
$Action = New-ScheduledTaskAction -Execute "PowerShell.exe" `
    -Argument "-ExecutionPolicy Bypass -File C:\Scripts\backup.ps1"

$Trigger = New-ScheduledTaskTrigger -Daily -At 2am

$Principal = New-ScheduledTaskPrincipal -UserId "SYSTEM" `
    -LogonType ServiceAccount -RunLevel Highest

Register-ScheduledTask -TaskName "MyWebApp-Backup" `
    -Action $Action `
    -Trigger $Trigger `
    -Principal $Principal `
    -Description "Sauvegarde quotidienne de MyWebApp"
```

### Restauration depuis sauvegarde

**restore.sh (Linux) :**

```bash
#!/bin/bash

# Vérifier les arguments
if [ $# -ne 1 ]; then
    echo "Usage: $0 <date_sauvegarde>"
    echo "Exemple: $0 20250101_020000"
    exit 1
fi

DATE=$1
BACKUP_DIR="/var/backups/mywebapp"
APP_DIR="/opt/mywebapp"
DB_NAME="mywebapp_prod"
DB_USER="mywebapp"

echo "=== Restauration de MyWebApp - $DATE ==="
echo ""
read -p "ATTENTION: Cela va écraser les données actuelles. Continuer? (oui/non) " -n 3 -r
echo
if [[ ! $REPLY =~ ^oui$ ]]; then
    echo "Restauration annulée"
    exit 1
fi

# 1. Arrêter l'application
echo "Arrêt de l'application..."
sudo supervisorctl stop mywebapp

# 2. Restaurer la base de données
echo "Restauration de la base de données..."
DB_BACKUP="$BACKUP_DIR/db_${DATE}.sql.gz"

if [ ! -f "$DB_BACKUP" ]; then
    echo "✗ Fichier de sauvegarde non trouvé: $DB_BACKUP"
    exit 1
fi

# Supprimer la base existante et la recréer
sudo -u postgres psql -c "DROP DATABASE IF EXISTS $DB_NAME;"
sudo -u postgres psql -c "CREATE DATABASE $DB_NAME OWNER $DB_USER;"

# Restaurer depuis la sauvegarde
gunzip -c "$DB_BACKUP" | sudo -u postgres psql $DB_NAME

if [ $? -eq 0 ]; then
    echo "✓ Base de données restaurée"
else
    echo "✗ Erreur lors de la restauration de la base"
    exit 1
fi

# 3. Restaurer les fichiers
echo "Restauration des fichiers..."
FILES_BACKUP="$BACKUP_DIR/files_${DATE}.tar.gz"

if [ ! -f "$FILES_BACKUP" ]; then
    echo "✗ Fichier de sauvegarde non trouvé: $FILES_BACKUP"
    exit 1
fi

# Sauvegarder l'ancienne version
sudo mv $APP_DIR ${APP_DIR}.old.$(date +%Y%m%d_%H%M%S)

# Extraire la sauvegarde
sudo tar -xzf "$FILES_BACKUP" -C $(dirname $APP_DIR)

echo "✓ Fichiers restaurés"

# 4. Restaurer la configuration
echo "Restauration de la configuration..."
CONFIG_BACKUP="$BACKUP_DIR/config_${DATE}.tar.gz"

if [ -f "$CONFIG_BACKUP" ]; then
    sudo tar -xzf "$CONFIG_BACKUP" -C /
    echo "✓ Configuration restaurée"
fi

# 5. Ajuster les permissions
echo "Ajustement des permissions..."
sudo chown -R mywebapp:mywebapp $APP_DIR
sudo chmod +x $APP_DIR/mywebapp

# 6. Redémarrer l'application
echo "Redémarrage de l'application..."
sudo supervisorctl start mywebapp

# 7. Vérifier le statut
sleep 2
sudo supervisorctl status mywebapp

echo ""
echo "=== Restauration terminée ==="
```

## 9.11.6 Mise à jour de l'application

### Déploiement avec zéro downtime (Linux)

**deploy.sh :**

```bash
#!/bin/bash

set -e

APP_NAME="mywebapp"
APP_DIR="/opt/mywebapp"
NEW_VERSION=$1

if [ -z "$NEW_VERSION" ]; then
    echo "Usage: $0 <version>"
    echo "Exemple: $0 1.2.0"
    exit 1
fi

echo "=== Déploiement de $APP_NAME v$NEW_VERSION ==="

# 1. Télécharger la nouvelle version
echo "Téléchargement de la nouvelle version..."
cd /tmp
wget "https://releases.example.com/mywebapp_${NEW_VERSION}.tar.gz"

# 2. Créer une sauvegarde
echo "Création d'une sauvegarde..."
/opt/mywebapp/scripts/backup.sh

# 3. Extraire la nouvelle version dans un répertoire temporaire
echo "Extraction de la nouvelle version..."
mkdir -p /tmp/mywebapp_new
tar -xzf "mywebapp_${NEW_VERSION}.tar.gz" -C /tmp/mywebapp_new

# 4. Tester la nouvelle version (compilation, sanity check)
echo "Validation de la nouvelle version..."
if [ ! -x "/tmp/mywebapp_new/mywebapp" ]; then
    echo "✗ Exécutable non trouvé ou non exécutable"
    exit 1
fi

# 5. Basculer vers la nouvelle version
echo "Bascule vers la nouvelle version..."

# Renommer l'ancien répertoire
sudo mv $APP_DIR ${APP_DIR}.old

# Déplacer la nouvelle version
sudo mv /tmp/mywebapp_new $APP_DIR

# Copier la configuration depuis l'ancienne version
sudo cp ${APP_DIR}.old/config/* $APP_DIR/config/ 2>/dev/null || true

# Ajuster les permissions
sudo chown -R mywebapp:mywebapp $APP_DIR
sudo chmod +x $APP_DIR/mywebapp

# 6. Redémarrer l'application
echo "Redémarrage de l'application..."
sudo supervisorctl restart mywebapp

# 7. Attendre que l'application démarre
echo "Vérification du démarrage..."
sleep 3

# 8. Health check
HEALTH_CHECK_URL="http://localhost:8080/health"
HEALTH_STATUS=$(curl -s -o /dev/null -w "%{http_code}" $HEALTH_CHECK_URL)

if [ "$HEALTH_STATUS" = "200" ]; then
    echo "✓ Application démarrée avec succès"

    # Nettoyer l'ancienne version après confirmation
    read -p "Supprimer l'ancienne version? (oui/non) " -n 3 -r
    echo
    if [[ $REPLY =~ ^oui$ ]]; then
        sudo rm -rf ${APP_DIR}.old
        echo "✓ Ancienne version supprimée"
    fi
else
    echo "✗ L'application ne répond pas correctement"
    echo "Rollback vers l'ancienne version..."

    sudo supervisorctl stop mywebapp
    sudo rm -rf $APP_DIR
    sudo mv ${APP_DIR}.old $APP_DIR
    sudo supervisorctl start mywebapp

    echo "✓ Rollback effectué"
    exit 1
fi

echo ""
echo "=== Déploiement terminé avec succès ==="
echo "Version: $NEW_VERSION"
```

### Blue-Green Deployment

**blue-green-deploy.sh :**

```bash
#!/bin/bash

# Configuration
BLUE_PORT=8080
GREEN_PORT=8081
NGINX_UPSTREAM="mywebapp_backend"

# Déterminer quel environnement est actuellement actif
CURRENT_PORT=$(nginx -T 2>/dev/null | grep -A 10 "upstream $NGINX_UPSTREAM" | grep "server" | awk '{print $2}' | cut -d: -f2 | cut -d\; -f1)

if [ "$CURRENT_PORT" = "$BLUE_PORT" ]; then
    ACTIVE="blue"
    INACTIVE="green"
    INACTIVE_PORT=$GREEN_PORT
else
    ACTIVE="green"
    INACTIVE="blue"
    INACTIVE_PORT=$BLUE_PORT
fi

echo "Environnement actif: $ACTIVE (port $CURRENT_PORT)"
echo "Déploiement sur: $INACTIVE (port $INACTIVE_PORT)"

# 1. Déployer sur l'environnement inactif
echo "Déploiement de la nouvelle version..."
# [Logique de déploiement]

# 2. Démarrer l'environnement inactif
echo "Démarrage de l'environnement $INACTIVE..."
sudo supervisorctl start mywebapp_$INACTIVE

# 3. Health check
sleep 5
HEALTH=$(curl -s -o /dev/null -w "%{http_code}" "http://localhost:$INACTIVE_PORT/health")

if [ "$HEALTH" != "200" ]; then
    echo "✗ Health check échoué sur $INACTIVE"
    sudo supervisorctl stop mywebapp_$INACTIVE
    exit 1
fi

# 4. Basculer Nginx vers le nouvel environnement
echo "Bascule du trafic vers $INACTIVE..."
sudo sed -i "s/server 127.0.0.1:$CURRENT_PORT;/server 127.0.0.1:$INACTIVE_PORT;/" /etc/nginx/sites-available/mywebapp
sudo nginx -t && sudo nginx -s reload

# 5. Arrêter l'ancien environnement
sleep 10  # Laisser le temps aux connexions de se terminer
echo "Arrêt de l'environnement $ACTIVE..."
sudo supervisorctl stop mywebapp_$ACTIVE

echo "✓ Déploiement blue-green terminé"
```

## 9.11.7 Sécurité en production

### Checklist de sécurité

**Liste de contrôle complète :**

- ✅ **HTTPS activé** avec certificat valide
- ✅ **Pare-feu configuré** (UFW/Windows Firewall)
- ✅ **Ports minimaux ouverts** (80, 443 uniquement)
- ✅ **Utilisateur dédié** non-root/non-admin
- ✅ **Permissions fichiers** restrictives (chmod 600 pour config)
- ✅ **Mots de passe forts** pour base de données
- ✅ **Clés SSH** pour accès serveur (pas de password)
- ✅ **Fail2ban** installé (Linux)
- ✅ **Logs d'accès** activés et surveillés
- ✅ **Mises à jour système** automatiques
- ✅ **Sauvegardes** quotidiennes et testées
- ✅ **Rate limiting** configuré
- ✅ **Headers de sécurité** (HSTS, CSP, etc.)
- ✅ **Validation entrées** côté serveur
- ✅ **SQL injection** protection (requêtes préparées)
- ✅ **XSS protection** (échappement HTML)
- ✅ **CSRF tokens** pour formulaires

### Hardening du serveur Linux

```bash
#!/bin/bash

echo "=== Hardening du serveur ==="

# 1. Mettre à jour le système
sudo apt-get update
sudo apt-get upgrade -y

# 2. Installer Fail2ban
sudo apt-get install -y fail2ban

# Configuration Fail2ban
sudo tee /etc/fail2ban/jail.local > /dev/null <<EOF
[DEFAULT]
bantime = 3600
findtime = 600
maxretry = 5

[sshd]
enabled = true

[nginx-http-auth]
enabled = true

[nginx-noscript]
enabled = true
EOF

sudo systemctl enable fail2ban
sudo systemctl restart fail2ban

# 3. Désactiver l'accès root SSH
sudo sed -i 's/PermitRootLogin yes/PermitRootLogin no/' /etc/ssh/sshd_config
sudo sed -i 's/#PasswordAuthentication yes/PasswordAuthentication no/' /etc/ssh/sshd_config
sudo systemctl restart sshd

# 4. Configurer les mises à jour automatiques
sudo apt-get install -y unattended-upgrades
sudo dpkg-reconfigure -plow unattended-upgrades

# 5. Installer et configurer AppArmor
sudo apt-get install -y apparmor apparmor-utils
sudo systemctl enable apparmor
sudo systemctl start apparmor

echo "✓ Hardening terminé"
```

### Hardening du serveur Windows

```powershell
# Hardening serveur Windows

# 1. Activer Windows Defender
Set-MpPreference -DisableRealtimeMonitoring $false

# 2. Activer le pare-feu
Set-NetFirewallProfile -Profile Domain,Public,Private -Enabled True

# 3. Désactiver les services inutiles
$ServicesToDisable = @(
    'RemoteRegistry',
    'Telnet'
)

foreach ($Service in $ServicesToDisable) {
    Stop-Service $Service -ErrorAction SilentlyContinue
    Set-Service $Service -StartupType Disabled -ErrorAction SilentlyContinue
}

# 4. Configurer les stratégies de mot de passe
net accounts /minpwlen:12
net accounts /maxpwage:90
net accounts /minpwage:1
net accounts /uniquepw:5

# 5. Activer l'audit
auditpol /set /category:"Logon/Logoff" /success:enable /failure:enable
auditpol /set /category:"Object Access" /success:enable /failure:enable

# 6. Activer les mises à jour automatiques
$AU = (New-Object -ComObject Microsoft.Update.AutoUpdate)
$AU.EnableService()

Write-Host "✓ Hardening terminé"
```

## 9.11.8 Troubleshooting

### Problèmes courants et solutions

**1. L'application ne démarre pas**

```bash
# Linux - Vérifier les logs
sudo tail -f /var/log/mywebapp/app.log
sudo supervisorctl tail -f mywebapp

# Vérifier les permissions
ls -la /opt/mywebapp/mywebapp

# Vérifier les dépendances
ldd /opt/mywebapp/mywebapp

# Tester manuellement
cd /opt/mywebapp
sudo -u mywebapp ./mywebapp

# Vérifier si le port est déjà utilisé
sudo netstat -tlnp | grep 8080
# ou
sudo ss -tlnp | grep 8080
```

```powershell
# Windows - Vérifier les logs
Get-Content C:\ProgramData\mywebapp\logs\app.log -Tail 50 -Wait

# Vérifier le statut du service
nssm status MyWebApp

# Tester manuellement
cd C:\inetpub\mywebapp
.\mywebapp.exe

# Vérifier si le port est utilisé
netstat -ano | findstr :8080
```

**Solutions :**
- Vérifier que l'exécutable a les permissions d'exécution
- S'assurer que le port n'est pas déjà occupé
- Vérifier la configuration des variables d'environnement
- Contrôler que toutes les bibliothèques nécessaires sont présentes

**2. Erreur de connexion à la base de données**

```bash
# Tester la connexion PostgreSQL
psql -U mywebapp -h localhost -d mywebapp_prod

# Vérifier que PostgreSQL écoute
sudo netstat -tlnp | grep 5432

# Vérifier les logs PostgreSQL
sudo tail -f /var/log/postgresql/postgresql-15-main.log

# Vérifier pg_hba.conf
sudo cat /etc/postgresql/15/main/pg_hba.conf | grep mywebapp
```

```powershell
# Windows - Tester la connexion
& "C:\Program Files\PostgreSQL\15\bin\psql.exe" -U mywebapp -h localhost -d mywebapp_prod

# Vérifier le service PostgreSQL
Get-Service postgresql*

# Vérifier les logs
Get-Content "C:\Program Files\PostgreSQL\15\data\pg_log\*.log" -Tail 50
```

**Solutions :**
- Vérifier les identifiants de connexion dans config.env
- S'assurer que PostgreSQL est démarré
- Contrôler pg_hba.conf pour autoriser les connexions
- Vérifier le pare-feu (port 5432)

**3. Erreur 502 Bad Gateway (Nginx)**

```bash
# Vérifier que l'application répond
curl http://localhost:8080/health

# Vérifier les logs Nginx
sudo tail -f /var/log/nginx/mywebapp_error.log

# Tester la configuration Nginx
sudo nginx -t

# Vérifier les connexions
sudo netstat -tlnp | grep nginx
```

**Solutions :**
- Vérifier que l'application backend est démarrée
- Contrôler que le port dans la configuration Nginx correspond
- Augmenter les timeouts si nécessaire
- Vérifier les logs de l'application

**4. Performance dégradée**

```bash
# Monitorer les ressources système
top
htop
vmstat 1
iostat -x 1

# Vérifier l'utilisation disque
df -h
du -sh /opt/mywebapp/*

# Analyser les connexions
ss -s
netstat -an | grep ESTABLISHED | wc -l

# Vérifier les logs pour les erreurs
sudo grep -i error /var/log/mywebapp/app.log | tail -20
```

```powershell
# Windows - Monitorer les ressources
Get-Counter '\Processor(_Total)\% Processor Time'
Get-Counter '\Memory\Available MBytes'

# Vérifier l'utilisation disque
Get-PSDrive C | Select-Object Used,Free

# Vérifier les connexions
Get-NetTCPConnection | Where-Object State -eq Established | Measure-Object
```

**Solutions :**
- Ajouter des ressources (RAM, CPU) si nécessaire
- Optimiser les requêtes de base de données
- Implémenter un système de cache
- Augmenter le nombre de workers/threads
- Analyser les goulots d'étranglement avec profiling

**5. L'application consomme trop de mémoire**

```pascal
// Vérifier les fuites mémoire dans le code
// Ajouter du monitoring mémoire

procedure MonitorMemory;
var
  HeapStatus: TFPCHeapStatus;
begin
  HeapStatus := GetFPCHeapStatus;

  WriteLn('Mémoire utilisée: ', HeapStatus.CurrHeapUsed div 1024, ' KB');
  WriteLn('Mémoire libre: ', HeapStatus.CurrHeapFree div 1024, ' KB');

  if HeapStatus.CurrHeapUsed > 500 * 1024 * 1024 then // > 500 MB
    WriteLn('ATTENTION: Utilisation mémoire élevée!');
end;
```

**Solutions :**
- Analyser avec heaptrc ou valgrind
- S'assurer que tous les objets sont libérés
- Implémenter un pool d'objets réutilisables
- Limiter la taille des caches en mémoire
- Redémarrer périodiquement l'application si nécessaire

### Outils de diagnostic

**Linux :**

```bash
# Installation des outils de diagnostic
sudo apt-get install -y \
    htop \
    iotop \
    nethogs \
    sysstat \
    strace

# Tracer les appels système
sudo strace -p $(pgrep mywebapp)

# Profiler avec perf
sudo perf record -p $(pgrep mywebapp) -g -- sleep 30
sudo perf report

# Analyser les connexions réseau
sudo tcpdump -i any port 8080 -w capture.pcap
```

**Windows :**

```powershell
# Moniteur de ressources
resmon

# Moniteur de performances
perfmon

# Analyser un processus
Get-Process mywebapp | Select-Object *

# Capturer le trafic réseau
# Utiliser Wireshark ou Microsoft Message Analyzer
```

## 9.11.9 Déploiement continu (CI/CD)

### Pipeline GitLab CI/CD

**.gitlab-ci.yml :**

```yaml
stages:
  - build
  - test
  - deploy

variables:
  APP_NAME: "mywebapp"
  VERSION: "${CI_COMMIT_TAG:-dev-${CI_COMMIT_SHORT_SHA}}"

# Compilation
build:
  stage: build
  image: debian:bullseye
  before_script:
    - apt-get update
    - apt-get install -y fpc
  script:
    - fpc -O3 -Xs -XX -CX src/mywebapp.pas
    - mkdir -p dist
    - cp mywebapp dist/
    - cp -r config templates public dist/
  artifacts:
    paths:
      - dist/
    expire_in: 1 week
  only:
    - main
    - tags

# Tests
test:
  stage: test
  image: debian:bullseye
  dependencies:
    - build
  script:
    - cd dist
    - chmod +x mywebapp
    # Lancer les tests unitaires
    - ./mywebapp --test || true
  only:
    - main
    - tags

# Déploiement staging
deploy_staging:
  stage: deploy
  image: alpine:latest
  dependencies:
    - build
  before_script:
    - apk add --no-cache openssh-client rsync
    - eval $(ssh-agent -s)
    - echo "$SSH_PRIVATE_KEY" | tr -d '\r' | ssh-add -
    - mkdir -p ~/.ssh
    - chmod 700 ~/.ssh
    - ssh-keyscan $STAGING_HOST >> ~/.ssh/known_hosts
  script:
    - echo "Déploiement sur staging..."
    - rsync -avz --delete dist/ $STAGING_USER@$STAGING_HOST:/opt/mywebapp/
    - ssh $STAGING_USER@$STAGING_HOST "supervisorctl restart mywebapp"
  environment:
    name: staging
    url: https://staging.example.com
  only:
    - main

# Déploiement production
deploy_production:
  stage: deploy
  image: alpine:latest
  dependencies:
    - build
  before_script:
    - apk add --no-cache openssh-client rsync
    - eval $(ssh-agent -s)
    - echo "$SSH_PRIVATE_KEY" | tr -d '\r' | ssh-add -
    - mkdir -p ~/.ssh
    - chmod 700 ~/.ssh
    - ssh-keyscan $PROD_HOST >> ~/.ssh/known_hosts
  script:
    - echo "Déploiement en production..."
    - ssh $PROD_USER@$PROD_HOST "/opt/mywebapp/scripts/backup.sh"
    - rsync -avz --delete dist/ $PROD_USER@$PROD_HOST:/opt/mywebapp/
    - ssh $PROD_USER@$PROD_HOST "supervisorctl restart mywebapp"
    - sleep 5
    - ssh $PROD_USER@$PROD_HOST "curl -f http://localhost:8080/health || exit 1"
  environment:
    name: production
    url: https://www.example.com
  only:
    - tags
  when: manual
```

### Pipeline GitHub Actions

**.github/workflows/deploy.yml :**

```yaml
name: Build and Deploy

on:
  push:
    branches: [ main ]
    tags: [ 'v*' ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Install FreePascal
      run: |
        sudo apt-get update
        sudo apt-get install -y fpc

    - name: Build Application
      run: |
        fpc -O3 -Xs -XX -CX src/mywebapp.pas
        mkdir -p dist
        cp mywebapp dist/
        cp -r config templates public dist/

    - name: Upload Artifact
      uses: actions/upload-artifact@v3
      with:
        name: mywebapp
        path: dist/
        retention-days: 7

  test:
    needs: build
    runs-on: ubuntu-latest

    steps:
    - uses: actions/download-artifact@v3
      with:
        name: mywebapp
        path: dist/

    - name: Run Tests
      run: |
        cd dist
        chmod +x mywebapp
        ./mywebapp --test || true

  deploy-staging:
    needs: [build, test]
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    environment:
      name: staging
      url: https://staging.example.com

    steps:
    - uses: actions/download-artifact@v3
      with:
        name: mywebapp
        path: dist/

    - name: Deploy to Staging
      env:
        SSH_PRIVATE_KEY: ${{ secrets.SSH_PRIVATE_KEY }}
        STAGING_HOST: ${{ secrets.STAGING_HOST }}
        STAGING_USER: ${{ secrets.STAGING_USER }}
      run: |
        mkdir -p ~/.ssh
        echo "$SSH_PRIVATE_KEY" > ~/.ssh/id_rsa
        chmod 600 ~/.ssh/id_rsa
        ssh-keyscan $STAGING_HOST >> ~/.ssh/known_hosts

        rsync -avz --delete dist/ $STAGING_USER@$STAGING_HOST:/opt/mywebapp/
        ssh $STAGING_USER@$STAGING_HOST "supervisorctl restart mywebapp"

  deploy-production:
    needs: [build, test]
    runs-on: ubuntu-latest
    if: startsWith(github.ref, 'refs/tags/v')
    environment:
      name: production
      url: https://www.example.com

    steps:
    - uses: actions/download-artifact@v3
      with:
        name: mywebapp
        path: dist/

    - name: Deploy to Production
      env:
        SSH_PRIVATE_KEY: ${{ secrets.SSH_PRIVATE_KEY }}
        PROD_HOST: ${{ secrets.PROD_HOST }}
        PROD_USER: ${{ secrets.PROD_USER }}
      run: |
        mkdir -p ~/.ssh
        echo "$SSH_PRIVATE_KEY" > ~/.ssh/id_rsa
        chmod 600 ~/.ssh/id_rsa
        ssh-keyscan $PROD_HOST >> ~/.ssh/known_hosts

        # Backup
        ssh $PROD_USER@$PROD_HOST "/opt/mywebapp/scripts/backup.sh"

        # Deploy
        rsync -avz --delete dist/ $PROD_USER@$PROD_HOST:/opt/mywebapp/
        ssh $PROD_USER@$PROD_HOST "supervisorctl restart mywebapp"

        # Health check
        sleep 5
        ssh $PROD_USER@$PROD_HOST "curl -f http://localhost:8080/health"
```

## 9.11.10 Documentation de déploiement

### Guide de déploiement rapide

**DEPLOYMENT.md :**

````markdown
# Guide de déploiement MyWebApp

## Prérequis

### Linux (Ubuntu 22.04)
- FreePascal 3.2.2+
- PostgreSQL 15+
- Nginx 1.18+
- Supervisor
- 2 GB RAM minimum
- 10 GB espace disque

### Windows Server 2019+
- FreePascal 3.2.2+
- PostgreSQL 15+
- IIS 10+
- NSSM
- 2 GB RAM minimum
- 10 GB espace disque

## Installation rapide

### Linux

```bash
# 1. Télécharger la dernière version
wget https://releases.example.com/mywebapp_latest.tar.gz

# 2. Exécuter le script d'installation
sudo bash install.sh

# 3. Configurer la base de données
sudo -u postgres createdb mywebapp_prod
sudo -u postgres createuser mywebapp

# 4. Configurer l'application
sudo nano /etc/mywebapp/config.env

# 5. Démarrer
sudo supervisorctl start mywebapp
```

### Windows

```powershell
# 1. Télécharger la dernière version
Invoke-WebRequest -Uri "https://releases.example.com/mywebapp_latest.zip" -OutFile "mywebapp.zip"

# 2. Extraire
Expand-Archive mywebapp.zip -DestinationPath "C:\inetpub\mywebapp"

# 3. Exécuter le script d'installation
cd C:\inetpub\mywebapp
.\install.ps1

# 4. Démarrer le service
nssm start MyWebApp
```

## Configuration

### Variables d'environnement

| Variable | Description | Exemple |
|----------|-------------|---------|
| APP_ENV | Environnement | production |
| DB_HOST | Hôte base de données | localhost |
| DB_PORT | Port base de données | 5432 |
| DB_NAME | Nom base de données | mywebapp_prod |
| DB_USER | Utilisateur BDD | mywebapp |
| DB_PASSWORD | Mot de passe BDD | SecurePass123 |
| SERVER_PORT | Port de l'application | 8080 |
| LOG_LEVEL | Niveau de log | INFO |

## Vérification

```bash
# Tester le health check
curl http://localhost:8080/health

# Devrait retourner :
# {"status":"healthy","version":"1.0.0"}
```

## Mise à jour

```bash
# Linux
sudo /opt/mywebapp/scripts/deploy.sh 1.2.0

# Windows
C:\inetpub\mywebapp\scripts\deploy.ps1 -Version "1.2.0"
```

## Rollback

En cas de problème après une mise à jour :

```bash
# Linux
sudo /opt/mywebapp/scripts/rollback.sh

# Windows
C:\inetpub\mywebapp\scripts\rollback.ps1
```

## Monitoring

- **Logs** : /var/log/mywebapp/ (Linux) ou C:\ProgramData\mywebapp\logs (Windows)
- **Métriques** : http://localhost:8080/metrics
- **Health check** : http://localhost:8080/health

## Support

- Documentation : https://docs.example.com
- Issues : https://github.com/example/mywebapp/issues
- Email : support@example.com
````

## 9.11.11 Bonnes pratiques récapitulatives

### Checklist de déploiement

**Avant le déploiement :**

- ✅ Code testé en environnement de staging
- ✅ Sauvegarde de la base de données actuelle
- ✅ Sauvegarde de l'application actuelle
- ✅ Documentation à jour
- ✅ Plan de rollback préparé
- ✅ Fenêtre de maintenance communiquée
- ✅ Monitoring en place

**Pendant le déploiement :**

- ✅ Suivre le script de déploiement
- ✅ Vérifier chaque étape
- ✅ Monitorer les logs en temps réel
- ✅ Faire des health checks réguliers
- ✅ Vérifier les métriques de performance

**Après le déploiement :**

- ✅ Vérifier que l'application répond
- ✅ Tester les fonctionnalités principales
- ✅ Surveiller les logs d'erreurs
- ✅ Vérifier les performances
- ✅ Documenter les problèmes rencontrés
- ✅ Nettoyer les anciennes versions

### Tableau comparatif Linux vs Windows

| Aspect | Linux (Ubuntu) | Windows Server |
|--------|----------------|----------------|
| **Gestionnaire de service** | Supervisor, systemd | NSSM, Windows Service |
| **Serveur web** | Nginx, Apache | IIS |
| **Base de données** | PostgreSQL natif | PostgreSQL (installer) |
| **Logs** | /var/log/ | C:\ProgramData\logs\ |
| **Certificat SSL** | Let's Encrypt (gratuit) | Win-ACME ou commercial |
| **Firewall** | UFW, iptables | Windows Firewall |
| **Monitoring** | Prometheus + Grafana | Prometheus + Grafana |
| **Backup** | Shell scripts + cron | PowerShell + Task Scheduler |
| **Performance** | Excellent | Très bon |
| **Coût** | Gratuit | Licence requise |
| **Administration** | SSH + commandes | RDP + GUI |

### Recommandations finales

**Pour les petites applications :**
- Déploiement simple avec Supervisor (Linux) ou NSSM (Windows)
- Base de données locale
- Sauvegardes quotidiennes
- Monitoring basique

**Pour les applications moyennes :**
- Reverse proxy (Nginx/IIS)
- Base de données dédiée
- SSL/TLS obligatoire
- Monitoring avec Prometheus/Grafana
- CI/CD avec GitLab ou GitHub Actions
- Sauvegardes automatiques avec rotation

**Pour les grandes applications :**
- Architecture distribuée avec load balancer
- Base de données répliquée
- Cache Redis/Memcached
- CDN pour les assets statiques
- Monitoring avancé avec alertes
- CI/CD avec déploiement blue-green
- Sauvegardes géorépliquées
- Plan de disaster recovery

## Conclusion

Le déploiement d'applications FreePascal sur des serveurs Windows et Linux nécessite une approche méthodique et des outils appropriés. Les points clés à retenir :

**Points essentiels :**
- Automatiser au maximum (build, tests, déploiement)
- Toujours avoir un plan de rollback
- Monitorer en continu (logs, métriques, alertes)
- Sauvegarder régulièrement et tester les restaurations
- Sécuriser dès le départ (HTTPS, firewall, permissions)
- Documenter toutes les procédures

**Avantages de FreePascal en production :**
- Exécutables autonomes sans dépendances lourdes
- Performance native excellente
- Faible consommation mémoire
- Facilité de déploiement (un seul fichier)
- Stabilité et fiabilité éprouvées

Que vous déployiez sur Linux ou Windows, FreePascal offre une excellente base pour créer des applications web robustes et performantes en production.

⏭️ [Programmation Réseau Avancée](/10-programmation-reseau-avancee/README.md)
