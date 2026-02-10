🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 24.10 Remote debugging cross-platform

## Introduction

Le **remote debugging** (débogage à distance) permet de déboguer une application qui s'exécute sur une machine différente de celle où se trouve votre environnement de développement. Cette technique est essentielle pour le développement multi-plateforme.

**Analogie simple :** Imaginez un médecin qui examine un patient à distance via une vidéo-consultation. Le remote debugging fonctionne de la même façon : votre IDE sur Windows peut "examiner" et contrôler un programme qui tourne sur Linux, ou vice-versa.

**Cas d'usage courants :**
- Déboguer une application Linux depuis Windows
- Déboguer une application Windows depuis Linux
- Déboguer sur un serveur distant
- Déboguer sur un Raspberry Pi depuis votre PC
- Déboguer dans un conteneur Docker
- Tester des comportements spécifiques à une plateforme

---

## Concepts de base

### Comment fonctionne le remote debugging ?

```
[Machine de développement]          [Machine cible]
        ↓                                  ↓
   IDE Lazarus                    Application à déboguer
   (debugger client)                     +
        ↓                            GDB Server
        |                                  ↑
        |←──── Connexion réseau ──────────|
        |    (TCP/IP, SSH, etc.)          |
        |                                  |
   Points d'arrêt,                   Exécution,
   variables,                        inspection,
   pile d'appels                     contrôle
```

### Composants nécessaires

**1. Sur la machine cible (où l'application s'exécute) :**
- L'application compilée avec symboles de debug (`-g`)
- GDB Server (gdbserver) ou équivalent
- Accès réseau

**2. Sur la machine de développement (votre IDE) :**
- Lazarus IDE ou debugger compatible GDB
- Connexion réseau vers la machine cible
- Fichiers sources du projet

---

## Configuration de base

### Compilation avec symboles de debug

**Options de compilation nécessaires :**

```pascal
// Dans les options du projet Lazarus :
// Compilation → Debugging
// Cocher : "Use line info unit" (-gl)
// Cocher : "Generate debug info for GDB" (-g)
```

**En ligne de commande :**
```bash
# Compilation avec debug complet
fpc -g -gl -gw monprogramme.pas

# Options détaillées :
# -g   : Générer les informations de debug
# -gl  : Utiliser l'unité lineinfo pour les numéros de ligne
# -gw  : Générer les infos debug au format DWARF
```

**Vérifier les symboles de debug :**
```bash
# Linux/Ubuntu
file monprogramme
# Résultat attendu : "not stripped" (symboles présents)

# Afficher les symboles
nm monprogramme | head

# Informations DWARF
objdump -g monprogramme | head
```

### Installation de GDB Server

**Sur Ubuntu/Linux (machine cible) :**
```bash
# Installer gdbserver
sudo apt update  
sudo apt install gdbserver

# Vérifier l'installation
gdbserver --version
```

**Sur Windows (machine cible) :**
```cmd
# GDB Server est inclus avec MinGW-w64
# Télécharger depuis : https://www.mingw-w64.org/

# Ou utiliser celui fourni avec Lazarus :
C:\lazarus\mingw\x86_64-win64\bin\gdbserver.exe
```

---

## Scénario 1 : Déboguer Linux depuis Windows

### Configuration de la machine Linux (cible)

**Étape 1 : Préparer l'application**
```bash
# Sur la machine Linux
cd ~/monprojet

# Compiler avec symboles de debug
lazbuild --build-mode=Debug monprojet.lpi

# Ou avec fpc directement
fpc -g -gl monprogramme.pas
```

**Étape 2 : Lancer GDB Server**
```bash
# Lancer gdbserver sur un port (ex: 2345)
gdbserver :2345 ./monprogramme

# Résultat attendu :
# Process ./monprogramme created; pid = 1234
# Listening on port 2345
```

**Options de gdbserver :**
```bash
# Écouter sur toutes les interfaces
gdbserver 0.0.0.0:2345 ./monprogramme

# Écouter sur une IP spécifique
gdbserver 192.168.1.100:2345 ./monprogramme

# Avec arguments
gdbserver :2345 ./monprogramme arg1 arg2

# Attacher à un processus existant
gdbserver :2345 --attach <PID>
```

### Configuration de Lazarus (Windows)

**Étape 1 : Configurer le debugger**

Dans Lazarus :
1. Outils → Options
2. Debugger → General
3. Type de débogueur : "GNU debugger (gdb)"

**Étape 2 : Configurer la connexion distante**

1. Exécuter → Paramètres d'exécution
2. Onglet "Debugger"
3. Définir les paramètres :

```
Host: 192.168.1.100  (IP de la machine Linux)  
Port: 2345
```

**Étape 3 : Créer un profil de debug distant**

**Fichier de configuration manuelle (avancé) :**
```ini
# Dans project.lpi, ajouter :
<Debugging>
  <DebuggerType Value="GDB"/>
  <DebuggerOptions>
    <DebuggerClass Value="TGDBMIDEBUGGER"/>
    <DebuggerFilename Value="gdb"/>
    <DebuggerOptions>
      <UseRemoteTarget Value="True"/>
      <RemoteConnection Value="192.168.1.100:2345"/>
    </DebuggerOptions>
  </DebuggerOptions>
</Debugging>
```

**Étape 4 : Lancer le débogage**

1. Placer des points d'arrêt dans le code
2. Cliquer sur "Exécuter" (F9)
3. Lazarus se connecte à gdbserver
4. L'application s'arrête aux points d'arrêt

### Script d'automatisation

**Sur Linux (launch_debug.sh) :**
```bash
#!/bin/bash

# Configuration
PROJECT="monprogramme"  
PORT=2345

# Compiler
echo "Compilation..."  
lazbuild --build-mode=Debug "${PROJECT}.lpi"

# Lancer gdbserver
echo "Lancement de gdbserver sur le port $PORT..."  
gdbserver 0.0.0.0:$PORT "./$PROJECT"
```

**Sur Windows (connect_debug.bat) :**
```batch
@echo off
echo Connexion au debugger distant...

:: Lancer Lazarus avec configuration debug
start "" "C:\lazarus\lazarus.exe" --debug-remote=192.168.1.100:2345 monprojet.lpi

echo Lazarus lancé en mode debug distant
```

---

## Scénario 2 : Déboguer Windows depuis Linux

### Configuration de la machine Windows (cible)

**Étape 1 : Autoriser le trafic réseau**
```powershell
# Autoriser le port dans le pare-feu Windows
New-NetFirewallRule -DisplayName "GDB Server" -Direction Inbound -Port 2345 -Protocol TCP -Action Allow
```

**Étape 2 : Lancer GDB Server**
```cmd
cd C:\Projects\MonProjet

:: Compiler avec debug
lazbuild --build-mode=Debug monprojet.lpi

:: Lancer gdbserver
gdbserver :2345 monprojet.exe
```

### Configuration de Lazarus (Linux)

Même configuration que précédemment, en changeant l'IP pour celle de la machine Windows.

```bash
# Dans Lazarus sur Linux
# Outils → Options → Debugger
# Remote host: 192.168.1.101 (IP Windows)
# Remote port: 2345
```

---

## Debugging via SSH (méthode sécurisée)

### Pourquoi SSH ?

- **Sécurité** : Connexion chiffrée
- **Simplicité** : Pas besoin d'ouvrir des ports
- **Tunneling** : Rediriger le trafic GDB via SSH

### Configuration du tunnel SSH

**Depuis Windows vers Linux :**

**Avec PuTTY :**
1. Ouvrir PuTTY
2. Session → Host: `192.168.1.100`
3. Connection → SSH → Tunnels
4. Source port: `2345`
5. Destination: `localhost:2345`
6. Cliquer "Add" puis "Open"
7. Se connecter avec vos identifiants

**Avec OpenSSH (Windows 10/11) :**
```powershell
# Créer un tunnel SSH
ssh -L 2345:localhost:2345 user@192.168.1.100

# Laisser cette fenêtre ouverte
```

**Depuis Linux vers Windows :**
```bash
# Créer un tunnel SSH vers Windows (avec OpenSSH installé)
ssh -L 2345:localhost:2345 user@192.168.1.101
```

### Utilisation avec le tunnel

Une fois le tunnel établi :

1. Sur la machine cible, lancer :
```bash
gdbserver localhost:2345 ./monprogramme
```

2. Dans Lazarus, se connecter à :
```
Host: localhost  
Port: 2345
```

Le trafic passera automatiquement par le tunnel SSH sécurisé !

---

## Debugging avec VS Code (alternative à Lazarus)

### Installation et configuration

**Étape 1 : Installer les extensions**
- "C/C++" (Microsoft) - pour le support GDB
- "Remote Development" (Microsoft) - pour SSH
- "Pascal" (Alessandro Fragnani) - coloration syntaxe

**Étape 2 : Configurer launch.json**

**Fichier : `.vscode/launch.json`**
```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Debug Remote Linux",
            "type": "cppdbg",
            "request": "launch",
            "program": "${workspaceFolder}/monprogramme",
            "args": [],
            "stopAtEntry": false,
            "cwd": "${workspaceFolder}",
            "environment": [],
            "externalConsole": false,
            "MIMode": "gdb",
            "miDebuggerServerAddress": "192.168.1.100:2345",
            "setupCommands": [
                {
                    "description": "Enable pretty-printing",
                    "text": "-enable-pretty-printing",
                    "ignoreFailures": true
                }
            ]
        }
    ]
}
```

**Étape 3 : Déboguer**
1. Ouvrir le projet dans VS Code
2. Placer des points d'arrêt (F9)
3. Lancer le debug (F5)
4. VS Code se connecte au gdbserver distant

---

## Debugging via Docker

### Scénario : Application Linux dans un conteneur

**Dockerfile avec support debug :**
```dockerfile
FROM ubuntu:22.04

# Installer les dépendances
RUN apt-get update && apt-get install -y \
    fpc \
    gdbserver \
    && rm -rf /var/lib/apt/lists/*

# Copier l'application
WORKDIR /app  
COPY . /app

# Compiler avec debug
RUN fpc -g -gl monprogramme.pas

# Exposer le port GDB
EXPOSE 2345

# Lancer gdbserver
CMD ["gdbserver", "0.0.0.0:2345", "./monprogramme"]
```

**Lancer le conteneur :**
```bash
# Construire l'image
docker build -t mon-app-debug .

# Lancer avec le port exposé
docker run -p 2345:2345 mon-app-debug
```

**Se connecter depuis l'IDE :**
```
Host: localhost  
Port: 2345
```

### Docker Compose avec debug

**Fichier : `docker-compose.yml`**
```yaml
version: '3.8'

services:
  app:
    build: .
    ports:
      - "2345:2345"
    volumes:
      - .:/app
    command: gdbserver 0.0.0.0:2345 ./monprogramme
    cap_add:
      - SYS_PTRACE  # Nécessaire pour le debugging
```

**Lancement :**
```bash
docker-compose up
```

---

## Debugging sur Raspberry Pi

### Configuration du Raspberry Pi

**Étape 1 : Installation des outils**
```bash
# Sur le Raspberry Pi
sudo apt update  
sudo apt install fpc gdbserver

# Vérifier l'installation
fpc -iV  
gdbserver --version
```

**Étape 2 : Compiler l'application**
```bash
# Transférer le code source
# (via scp, git, ou partage réseau)

cd ~/monprojet  
fpc -g -gl -Tlinux -Parm monprogramme.pas
```

**Étape 3 : Lancer gdbserver**
```bash
# Lancer sur le port standard
gdbserver :2345 ./monprogramme
```

### Cross-compilation et debug

**Option 1 : Compiler sur le PC, déboguer sur le Pi**

**Sur PC (Windows/Linux) :**
```bash
# Cross-compiler pour ARM
fpc -Tlinux -Parm -g -gl monprogramme.pas

# Transférer vers le Pi
scp monprogramme pi@192.168.1.150:~/
```

**Sur Raspberry Pi :**
```bash
chmod +x monprogramme  
gdbserver :2345 ./monprogramme
```

**Sur PC (Lazarus) :**
- Configurer remote target: `192.168.1.150:2345`
- Déboguer normalement

**Script automatisé (build_and_debug_pi.sh) :**
```bash
#!/bin/bash

PI_IP="192.168.1.150"  
PI_USER="pi"  
PROJECT="monprogramme"

echo "=== Build and Debug for Raspberry Pi ==="

# Cross-compilation
echo "[1/3] Cross-compilation..."  
fpc -Tlinux -Parm -g -gl "${PROJECT}.pas"

# Transfert
echo "[2/3] Transfert vers le Pi..."  
scp "$PROJECT" "${PI_USER}@${PI_IP}:~/"

# Lancement distant du debug
echo "[3/3] Lancement de gdbserver sur le Pi..."  
ssh "${PI_USER}@${PI_IP}" "gdbserver :2345 ~/${PROJECT}" &

echo "Ready! Connect your debugger to ${PI_IP}:2345"
```

---

## Techniques avancées

### 1. Debug avec plusieurs machines simultanément

**Architecture :**
```
[IDE Lazarus Windows]
        ↓
   ┌────┴────┬────────────┐
   ↓         ↓            ↓
[Linux A] [Linux B]  [Raspberry Pi]
 :2345     :2346        :2347
```

**Configuration multi-cibles dans Lazarus :**

Créer plusieurs configurations de debug :
1. Debug-Linux-A (192.168.1.100:2345)
2. Debug-Linux-B (192.168.1.101:2346)
3. Debug-RaspberryPi (192.168.1.150:2347)

Basculer entre les configurations selon la cible.

### 2. Debugging avec logging distant

**Configuration de logs réseau :**

```pascal
unit RemoteLogger;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Sockets;

type
  TRemoteLogger = class
  private
    FSocket: TSocket;
    FConnected: Boolean;
  public
    constructor Create(const Host: string; Port: Word);
    destructor Destroy; override;
    procedure Log(const Msg: string);
  end;

implementation

constructor TRemoteLogger.Create(const Host: string; Port: Word);  
var
  Addr: TInetSockAddr;
begin
  inherited Create;

  FSocket := fpSocket(AF_INET, SOCK_STREAM, 0);

  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr.s_addr := StrToNetAddr(Host).s_addr;

  FConnected := fpConnect(FSocket, @Addr, SizeOf(Addr)) = 0;
end;

destructor TRemoteLogger.Destroy;  
begin
  if FConnected then
    CloseSocket(FSocket);
  inherited Destroy;
end;

procedure TRemoteLogger.Log(const Msg: string);  
var
  LogLine: string;
begin
  if not FConnected then Exit;

  LogLine := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Msg + #13#10;
  fpSend(FSocket, PChar(LogLine), Length(LogLine), 0);
end;

end.
```

**Utilisation :**
```pascal
var
  Logger: TRemoteLogger;
begin
  Logger := TRemoteLogger.Create('192.168.1.100', 9999);
  try
    Logger.Log('Application démarrée');
    Logger.Log('Calcul en cours...');
    // ...
  finally
    Logger.Free;
  end;
end;
```

**Serveur de logs (sur la machine de dev) :**
```bash
# Simple serveur netcat pour recevoir les logs
nc -l -p 9999
```

### 3. Debugging conditionnel par plateforme

```pascal
program DebugTest;

{$mode objfpc}{$H+}

uses
  SysUtils;

procedure DebugPoint(const Location: string);  
begin
  {$IFDEF DEBUG}
  WriteLn('[DEBUG] ', Location);

  {$IFDEF WINDOWS}
  // Code debug spécifique Windows
  WriteLn('  Platform: Windows');
  {$ENDIF}

  {$IFDEF LINUX}
  // Code debug spécifique Linux
  WriteLn('  Platform: Linux');
  {$ENDIF}

  // Point d'arrêt conditionnel
  {$IFDEF ENABLE_BREAKPOINTS}
  asm
    int 3  // Instruction de breakpoint x86
  end;
  {$ENDIF}
  {$ENDIF}
end;

begin
  DebugPoint('Main entry');
  // Code...
  DebugPoint('Before calculation');
  // Code...
  DebugPoint('Program end');
end.
```

---

## Outils complémentaires

### 1. Valgrind (détection de fuites mémoire)

**Sur la machine Linux cible :**
```bash
# Installer Valgrind
sudo apt install valgrind

# Exécuter avec Valgrind
valgrind --leak-check=full --log-file=valgrind.log ./monprogramme

# Analyser les résultats
cat valgrind.log
```

**Remote Valgrind debugging :**
```bash
# Lancer avec Valgrind + gdbserver
valgrind --vgdb=yes --vgdb-error=0 ./monprogramme

# Dans un autre terminal, se connecter
gdb ./monprogramme
(gdb) target remote | vgdb
```

### 2. strace (tracer les appels système)

```bash
# Tracer l'exécution
strace -o trace.log ./monprogramme

# Tracer avec timestamps
strace -tt -o trace.log ./monprogramme

# Filtrer les appels
strace -e trace=open,read,write ./monprogramme
```

### 3. HeapTrc (détecteur de fuites FreePascal)

```pascal
program TestHeapTrc;

{$mode objfpc}{$H+}

uses
  {$IFDEF DEBUG}
  HeapTrc,  // Activer le traceur de tas
  {$ENDIF}
  SysUtils;

{$IFDEF DEBUG}
{$SetHeapTraceOutput('heaptrc.log')}
{$ENDIF}

var
  P: PInteger;
begin
  // Allocation sans libération (fuite intentionnelle)
  New(P);
  P^ := 42;
  // Oubli du Dispose(P);

  WriteLn('Programme terminé');
end.
```

**Résultat dans heaptrc.log :**
```
Heap dump by heaptrc unit
1 memory blocks allocated : 4/12
1 memory blocks freed     : 0/0
1 unfreed memory blocks : 4
True heap size : 32768
```

---

## Résolution des problèmes courants

### Problème 1 : "Connection refused"

**Causes possibles :**
- Pare-feu bloque le port
- GDBServer n'est pas lancé
- Mauvaise IP/port

**Solutions :**
```bash
# Vérifier que le port est ouvert
netstat -tuln | grep 2345

# Tester la connectivité
telnet 192.168.1.100 2345

# Désactiver temporairement le pare-feu (test uniquement)
# Linux
sudo ufw disable
# Windows
netsh advfirewall set allprofiles state off
```

### Problème 2 : "No debugging symbols found"

**Cause :** Application non compilée avec `-g`

**Solution :**
```bash
# Recompiler avec symboles
fpc -g -gl monprogramme.pas

# Vérifier
file monprogramme | grep "not stripped"
```

### Problème 3 : Points d'arrêt ignorés

**Causes :**
- Fichiers sources pas synchronisés
- Chemins différents entre machines
- Optimisations du compilateur

**Solutions :**
```bash
# Compiler sans optimisations
fpc -g -gl -O- monprogramme.pas

# Vérifier les chemins des sources
gdb monprogramme
(gdb) info sources
```

### Problème 4 : Performance dégradée

**Causes :**
- Latence réseau
- Trop de logging
- Fichiers sources volumineux

**Solutions :**
- Utiliser SSH/VPN pour réduire latence
- Limiter le logging en remote
- Augmenter les timeouts dans l'IDE

---

## Bonnes pratiques

### 1. Sécurité

✅ **Toujours utiliser SSH pour les connexions distantes**
```bash
ssh -L 2345:localhost:2345 user@remote
```

✅ **Ne jamais exposer gdbserver sur Internet**
- Utiliser uniquement sur réseaux privés
- Ou via VPN

✅ **Désactiver gdbserver après debug**
```bash
killall gdbserver
```

### 2. Organisation du code

✅ **Chemins relatifs dans le projet**
```pascal
// Utiliser des chemins relatifs
ConfigFile := ExtractFilePath(ParamStr(0)) + 'config.ini';

// Éviter les chemins absolus
// ConfigFile := 'C:\Projects\config.ini';  // ❌
```

✅ **Directives de compilation pour debug**
```pascal
{$IFDEF DEBUG}
  {$DEFINE ENABLE_LOGGING}
  {$DEFINE ENABLE_ASSERTIONS}
{$ENDIF}
```

### 3. Documentation

✅ **Documenter la configuration de debug**

**Fichier : `DEBUG.md`**
````markdown
# Configuration de Debug à Distance

## Machine Linux (192.168.1.100)
```bash
gdbserver :2345 ./monprogramme
```

## IDE Lazarus (Windows)
- Remote host: 192.168.1.100
- Remote port: 2345

## Notes
- Compiler avec: `lazbuild --build-mode=Debug`
- Logs disponibles dans: `/var/log/monapp/`
````

---

## Scripts utiles

### Script de debug automatique complet

**Fichier : `remote_debug.sh`**
```bash
#!/bin/bash

# Configuration
REMOTE_HOST="192.168.1.100"  
REMOTE_USER="user"  
REMOTE_PATH="/home/user/projects/monprojet"  
LOCAL_PATH="$(pwd)"  
PROJECT="monprojet"  
DEBUG_PORT=2345

# Couleurs
GREEN='\033[0;32m'  
RED='\033[0;31m'  
NC='\033[0m'

echo "=== Remote Debugging Setup ==="

# 1. Compiler localement
echo -e "${GREEN}[1/5]${NC} Compilation..."  
lazbuild --build-mode=Debug "${PROJECT}.lpi"

# 2. Transférer
echo -e "${GREEN}[2/5]${NC} Transfert vers ${REMOTE_HOST}..."  
scp "${PROJECT}" "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}/"

# 3. Créer tunnel SSH
echo -e "${GREEN}[3/5]${NC} Création du tunnel SSH..."  
ssh -f -N -L ${DEBUG_PORT}:localhost:${DEBUG_PORT} "${REMOTE_USER}@${REMOTE_HOST}"

# 4. Lancer gdbserver sur la machine distante
echo -e "${GREEN}[4/5]${NC} Lancement de gdbserver..."  
ssh "${REMOTE_USER}@${REMOTE_HOST}" "cd ${REMOTE_PATH} && gdbserver :${DEBUG_PORT} ./${PROJECT}" &

# 5. Informations
echo -e "${GREEN}[5/5]${NC} Configuration terminée!"  
echo  
echo "Connect your debugger to: localhost:${DEBUG_PORT}"  
echo  
echo "Pour arrêter:"  
echo "  killall ssh"  
echo "  ssh ${REMOTE_USER}@${REMOTE_HOST} 'killall gdbserver'"
```

---

## Conclusion

Le remote debugging cross-platform est une compétence essentielle pour :

✅ **Développer** efficacement des applications multi-plateformes  
✅ **Tester** le comportement réel sur chaque OS  
✅ **Identifier** les bugs spécifiques à une plateforme  
✅ **Optimiser** sans quitter son environnement habituel  
✅ **Collaborer** avec des équipes distantes

**Points clés à retenir :**
- Toujours compiler avec les symboles de debug (`-g -gl`)
- Utiliser SSH pour sécuriser les connexions
- Documenter la configuration de debug
- Automatiser avec des scripts
- Tester régulièrement sur toutes les plateformes cibles

**Prochaines étapes :**
1. Configurer un environnement de test multi-plateforme
2. Créer vos scripts de debug automatique
3. Tester le remote debugging entre Windows et Linux
4. Explorer le debugging sur Raspberry Pi ou ARM
5. Intégrer le remote debugging dans votre CI/CD

Le remote debugging transforme le développement multi-plateforme en une expérience fluide et productive!

⏭️ [Projets Complexes et Études de Cas](/25-projets-complexes-etudes-cas/README.md)
