🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.12 Firewall et configuration réseau par OS

## Introduction aux firewalls et réseaux

### Qu'est-ce qu'un firewall ?

Un firewall (pare-feu) est un système de sécurité qui contrôle le trafic réseau entrant et sortant selon des règles prédéfinies. C'est comme un garde de sécurité qui vérifie qui peut entrer et sortir d'un bâtiment.

**Analogie simple :** Imaginez votre ordinateur comme une maison. Le firewall est la porte d'entrée avec un vigile qui :
- Vérifie l'identité des visiteurs (paquets réseau)
- Autorise l'entrée selon des règles (règles firewall)
- Refuse l'accès aux personnes non autorisées (attaques)
- Enregistre qui entre et sort (logs)

### Types de firewalls

**1. Firewall logiciel**
- Installé sur l'ordinateur
- Protège uniquement cet ordinateur
- Exemples : Windows Defender Firewall, UFW (Ubuntu)

**2. Firewall matériel**
- Appareil physique dédié
- Protège tout un réseau
- Exemples : Routeurs, appliances Cisco/Fortinet

**3. Firewall d'application**
- Contrôle spécifiquement les applications
- Plus granulaire
- Exemples : WAF (Web Application Firewall)

### Pourquoi c'est important pour vos applications ?

Quand vous développez une application réseau avec FreePascal/Lazarus, le firewall peut :
- ✅ **Bloquer** votre serveur (impossible de recevoir des connexions)
- ✅ **Bloquer** votre client (impossible de se connecter)
- ✅ **Ralentir** les connexions (inspection des paquets)
- ✅ **Logger** toute l'activité (traçabilité)

**Il faut donc savoir :**
1. Comment vérifier si le firewall bloque votre application
2. Comment créer des règles pour autoriser votre application
3. Comment tester la configuration réseau

## Concepts réseau fondamentaux

### Ports réseau

Un port est un numéro qui identifie une application sur un ordinateur.

**Analogie :** Si l'adresse IP est l'adresse d'un immeuble, le port est le numéro d'appartement.

```
IP : 192.168.1.10    (l'immeuble)  
Port : 8080          (l'appartement 8080)
```

**Plages de ports :**

| Plage | Nom | Usage | Exemples |
|-------|-----|-------|----------|
| 0-1023 | Well-known | Services système | 80 (HTTP), 443 (HTTPS), 22 (SSH) |
| 1024-49151 | Registered | Applications connues | 3306 (MySQL), 5432 (PostgreSQL) |
| 49152-65535 | Dynamic/Private | Usage libre | Vos applications |

**Conseil :** Utilisez des ports > 1024 pour vos applications (pas besoin de droits administrateur).

### Protocoles TCP et UDP

**TCP (Transmission Control Protocol)**
- Connexion fiable
- Garantit l'ordre des paquets
- Vérification d'erreurs
- Plus lent

**Usage :** HTTP, FTP, SSH, bases de données

**UDP (User Datagram Protocol)**
- Sans connexion
- Pas de garantie de livraison
- Pas d'ordre garanti
- Plus rapide

**Usage :** Streaming vidéo, jeux en ligne, DNS

```pascal
// En FreePascal
// Socket TCP
SocketTCP := fpSocket(AF_INET, SOCK_STREAM, 0);  // SOCK_STREAM = TCP

// Socket UDP
SocketUDP := fpSocket(AF_INET, SOCK_DGRAM, 0);   // SOCK_DGRAM = UDP
```

### Adresses IP

**IPv4 :** Format `192.168.1.1` (4 nombres de 0 à 255)  
**IPv6 :** Format `2001:0db8:85a3::8a2e:0370:7334` (plus long, plus moderne)  

**Adresses spéciales :**

| Adresse | Signification |
|---------|---------------|
| 127.0.0.1 | localhost (votre ordinateur) |
| 0.0.0.0 | Toutes les interfaces (écouter partout) |
| 192.168.x.x | Réseau local privé |
| 10.x.x.x | Réseau local privé |
| 172.16-31.x.x | Réseau local privé |

## Firewall Windows

### Windows Defender Firewall

C'est le firewall intégré à Windows (anciennement appelé "Pare-feu Windows").

### Interface graphique

**Ouvrir le firewall Windows :**

1. **Via Panneau de configuration :**
   - Ouvrir le Panneau de configuration
   - Système et sécurité → Pare-feu Windows Defender

2. **Via recherche :**
   - Appuyer sur Windows + S
   - Taper "Pare-feu"
   - Cliquer sur "Pare-feu Windows Defender"

### Créer une règle pour votre application

**Méthode graphique :**

1. Ouvrir le firewall Windows
2. Cliquer sur "Paramètres avancés" (à gauche)
3. Cliquer sur "Règles de trafic entrant" (à gauche)
4. Cliquer sur "Nouvelle règle..." (à droite)
5. Choisir "Programme"
6. Parcourir et sélectionner votre `.exe`
7. Choisir "Autoriser la connexion"
8. Cocher : Domaine, Privé, Public
9. Donner un nom : "Mon Serveur Pascal"
10. Cliquer sur "Terminer"

**Capture d'écran du processus :**
```
┌─────────────────────────────────────┐
│ Nouveau règle de trafic entrant    │
├─────────────────────────────────────┤
│ Type de règle :                     │
│   ○ Programme                       │ ← Choisir
│   ○ Port                            │
│   ○ Prédéfinie                      │
│   ○ Personnalisée                   │
└─────────────────────────────────────┘
```

### Ligne de commande (netsh)

**Syntaxe de base :**

```batch
netsh advfirewall firewall add rule ^
  name="Nom de la règle" ^
  dir=in ^
  action=allow ^
  protocol=TCP ^
  localport=8080
```

**Exemples pratiques :**

**1. Autoriser un port TCP (entrée) :**

```batch
REM Autoriser le port 8080 en TCP  
netsh advfirewall firewall add rule ^
  name="Serveur Web Pascal" ^
  dir=in ^
  action=allow ^
  protocol=TCP ^
  localport=8080

REM Vérifier  
netsh advfirewall firewall show rule name="Serveur Web Pascal"
```

**2. Autoriser un port UDP (sortie) :**

```batch
REM Autoriser le port 5000 en UDP  
netsh advfirewall firewall add rule ^
  name="Client UDP Pascal" ^
  dir=out ^
  action=allow ^
  protocol=UDP ^
  localport=5000
```

**3. Autoriser une application spécifique :**

```batch
REM Autoriser MonServeur.exe  
netsh advfirewall firewall add rule ^
  name="Mon Serveur Pascal" ^
  dir=in ^
  action=allow ^
  program="C:\Projets\MonServeur.exe" ^
  enable=yes

REM Autoriser sur un port spécifique uniquement  
netsh advfirewall firewall add rule ^
  name="Mon Serveur Port 8080" ^
  dir=in ^
  action=allow ^
  program="C:\Projets\MonServeur.exe" ^
  protocol=TCP ^
  localport=8080
```

**4. Autoriser une plage de ports :**

```batch
REM Autoriser les ports 8000 à 9000  
netsh advfirewall firewall add rule ^
  name="Serveurs Pascal 8000-9000" ^
  dir=in ^
  action=allow ^
  protocol=TCP ^
  localport=8000-9000
```

**5. Supprimer une règle :**

```batch
REM Supprimer par nom  
netsh advfirewall firewall delete rule name="Mon Serveur Pascal"

REM Supprimer toutes les règles d'un programme  
netsh advfirewall firewall delete rule ^
  program="C:\Projets\MonServeur.exe"
```

**6. Lister les règles :**

```batch
REM Toutes les règles  
netsh advfirewall firewall show rule name=all

REM Une règle spécifique  
netsh advfirewall firewall show rule name="Mon Serveur Pascal"

REM Règles par port  
netsh advfirewall firewall show rule name=all | findstr "8080"
```

### PowerShell (méthode moderne)

PowerShell offre des cmdlets plus modernes :

**Créer une règle :**

```powershell
# Autoriser le port 8080 en TCP
New-NetFirewallRule `
  -DisplayName "Serveur Pascal" `
  -Direction Inbound `
  -Protocol TCP `
  -LocalPort 8080 `
  -Action Allow

# Autoriser une application
New-NetFirewallRule `
  -DisplayName "Mon Application" `
  -Direction Inbound `
  -Program "C:\Projets\MonApp.exe" `
  -Action Allow
```

**Lister les règles :**

```powershell
# Toutes les règles
Get-NetFirewallRule

# Rechercher par nom
Get-NetFirewallRule -DisplayName "*Pascal*"

# Rechercher par port
Get-NetFirewallRule | Where-Object {$_.LocalPort -eq 8080}
```

**Supprimer une règle :**

```powershell
# Par nom
Remove-NetFirewallRule -DisplayName "Serveur Pascal"

# Par ID
Remove-NetFirewallRule -Name "{ID-de-la-règle}"
```

**Activer/Désactiver une règle :**

```powershell
# Désactiver
Disable-NetFirewallRule -DisplayName "Serveur Pascal"

# Activer
Enable-NetFirewallRule -DisplayName "Serveur Pascal"
```

### Vérifier depuis votre application Pascal

```pascal
unit FirewallWindows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

function CreerReglePascal(const NomRegle, CheminExe: String; Port: Word): Boolean;  
function SupprimerReglePascal(const NomRegle: String): Boolean;  
function VerifierRegle(const NomRegle: String): Boolean;

implementation

function ExecuterCommande(const Commande: String): String;  
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'cmd.exe';
    Process.Parameters.Add('/c');
    Process.Parameters.Add(Commande);
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    Result := Output.Text;
  finally
    Output.Free;
    Process.Free;
  end;
end;

function CreerReglePascal(const NomRegle, CheminExe: String; Port: Word): Boolean;  
var
  Commande: String;
  Output: String;
begin
  Commande := Format(
    'netsh advfirewall firewall add rule name="%s" ' +
    'dir=in action=allow program="%s" protocol=TCP localport=%d',
    [NomRegle, CheminExe, Port]
  );

  Output := ExecuterCommande(Commande);
  Result := Pos('Ok.', Output) > 0;

  if Result then
    WriteLn('✓ Règle firewall créée : ', NomRegle)
  else
    WriteLn('✗ Erreur création règle : ', Output);
end;

function SupprimerReglePascal(const NomRegle: String): Boolean;  
var
  Commande: String;
  Output: String;
begin
  Commande := Format(
    'netsh advfirewall firewall delete rule name="%s"',
    [NomRegle]
  );

  Output := ExecuterCommande(Commande);
  Result := Pos('Ok.', Output) > 0;

  if Result then
    WriteLn('✓ Règle firewall supprimée : ', NomRegle);
end;

function VerifierRegle(const NomRegle: String): Boolean;  
var
  Commande: String;
  Output: String;
begin
  Commande := Format(
    'netsh advfirewall firewall show rule name="%s"',
    [NomRegle]
  );

  Output := ExecuterCommande(Commande);
  Result := Pos('Nom de la règle', Output) > 0;
end;

end.
```

**Utilisation :**

```pascal
program TestFirewall;

{$mode objfpc}{$H+}

uses
  SysUtils, FirewallWindows;

var
  CheminExe: String;
begin
  CheminExe := ParamStr(0);  // Chemin de l'exécutable actuel

  WriteLn('=== Configuration Firewall Windows ===');
  WriteLn('Exécutable : ', CheminExe);
  WriteLn;

  // Vérifier si la règle existe
  if VerifierRegle('Mon Serveur Pascal') then
    WriteLn('Règle existe déjà')
  else
  begin
    WriteLn('Création de la règle...');
    if CreerReglePascal('Mon Serveur Pascal', CheminExe, 8080) then
      WriteLn('✓ Configuration terminée')
    else
      WriteLn('✗ Échec de la configuration');
  end;

  ReadLn;
end.
```

### Profils Windows Firewall

Windows a trois profils de firewall :

**1. Domaine** - Réseau d'entreprise (Active Directory)  
**2. Privé** - Réseau domestique de confiance  
**3. Public** - Réseau public (WiFi café, aéroport)  

**Configurer par profil :**

```batch
REM Autoriser uniquement sur réseau privé  
netsh advfirewall firewall add rule ^
  name="Serveur Local" ^
  dir=in ^
  action=allow ^
  protocol=TCP ^
  localport=8080 ^
  profile=private

REM Autoriser sur tous les profils  
netsh advfirewall firewall add rule ^
  name="Serveur Public" ^
  dir=in ^
  action=allow ^
  protocol=TCP ^
  localport=8080 ^
  profile=domain,private,public
```

### Logging Windows Firewall

**Activer les logs :**

```batch
REM Activer le logging des connexions bloquées  
netsh advfirewall set currentprofile logging droppedconnections enable

REM Activer le logging des connexions autorisées  
netsh advfirewall set currentprofile logging allowedconnections enable

REM Définir la taille max du fichier log (en KB)  
netsh advfirewall set currentprofile logging maxfilesize 4096
```

**Localisation des logs :**
```
C:\Windows\System32\LogFiles\Firewall\pfirewall.log
```

**Lire les logs depuis Pascal :**

```pascal
procedure LireLogsFirewall;  
var
  Logs: TStringList;
  i: Integer;
  Ligne: String;
begin
  Logs := TStringList.Create;
  try
    Logs.LoadFromFile('C:\Windows\System32\LogFiles\Firewall\pfirewall.log');

    WriteLn('=== Dernières entrées du firewall ===');

    // Afficher les 10 dernières lignes
    for i := Max(0, Logs.Count - 10) to Logs.Count - 1 do
    begin
      Ligne := Logs[i];
      if Pos('DROP', Ligne) > 0 then
        WriteLn('[BLOQUÉ] ', Ligne)
      else if Pos('ALLOW', Ligne) > 0 then
        WriteLn('[AUTORISÉ] ', Ligne);
    end;
  finally
    Logs.Free;
  end;
end;
```

## Firewall Linux (Ubuntu)

### UFW (Uncomplicated Firewall)

UFW est le firewall par défaut d'Ubuntu, conçu pour être simple.

### Commandes de base

**Vérifier le statut :**

```bash
# Statut actuel
sudo ufw status

# Statut détaillé
sudo ufw status verbose

# Statut numéroté (pour supprimer facilement)
sudo ufw status numbered
```

**Activer/Désactiver :**

```bash
# Activer le firewall
sudo ufw enable

# Désactiver le firewall
sudo ufw disable

# Réinitialiser (supprimer toutes les règles)
sudo ufw reset
```

### Créer des règles

**1. Autoriser un port :**

```bash
# Autoriser le port 8080 (TCP par défaut)
sudo ufw allow 8080

# Autoriser explicitement TCP
sudo ufw allow 8080/tcp

# Autoriser UDP
sudo ufw allow 5000/udp

# Autoriser TCP et UDP
sudo ufw allow 8080/tcp  
sudo ufw allow 8080/udp
```

**2. Autoriser depuis une IP spécifique :**

```bash
# Autoriser depuis 192.168.1.100
sudo ufw allow from 192.168.1.100

# Autoriser depuis 192.168.1.100 vers le port 8080
sudo ufw allow from 192.168.1.100 to any port 8080

# Autoriser depuis un sous-réseau
sudo ufw allow from 192.168.1.0/24 to any port 8080
```

**3. Autoriser une application :**

```bash
# Voir les applications disponibles
sudo ufw app list

# Autoriser une application (ex: OpenSSH)
sudo ufw allow OpenSSH

# Autoriser Nginx
sudo ufw allow 'Nginx Full'
```

**4. Bloquer un port :**

```bash
# Bloquer le port 23 (Telnet)
sudo ufw deny 23

# Bloquer depuis une IP
sudo ufw deny from 203.0.113.100
```

**5. Supprimer une règle :**

```bash
# Par numéro (voir avec status numbered)
sudo ufw status numbered  
sudo ufw delete 3

# Par description
sudo ufw delete allow 8080  
sudo ufw delete allow from 192.168.1.100
```

### Règles avancées

**Limiter les connexions (anti-brute force) :**

```bash
# Limiter SSH (max 6 connexions en 30 secondes)
sudo ufw limit ssh  
sudo ufw limit 22/tcp
```

**Redirection de port :**

```bash
# Éditer /etc/ufw/before.rules
sudo nano /etc/ufw/before.rules

# Ajouter avant *filter :
*nat
:PREROUTING ACCEPT [0:0]
-A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 8080
COMMIT
```

**Plages de ports :**

```bash
# Autoriser les ports 8000 à 9000
sudo ufw allow 8000:9000/tcp
```

### Configuration depuis Pascal

```pascal
unit FirewallLinux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

function AutoriserPort(Port: Word; Protocol: String = 'tcp'): Boolean;  
function BloquerPort(Port: Word): Boolean;  
function VerifierStatut: String;  
function ObtenirRegles: TStringList;

implementation

function ExecuterCommandeSudo(const Commande: String): String;  
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add('sudo ' + Commande);
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    Result := Output.Text;
  finally
    Output.Free;
    Process.Free;
  end;
end;

function AutoriserPort(Port: Word; Protocol: String): Boolean;  
var
  Commande: String;
  Output: String;
begin
  Commande := Format('ufw allow %d/%s', [Port, Protocol]);
  Output := ExecuterCommandeSudo(Commande);

  Result := (Pos('Rule added', Output) > 0) or
            (Pos('Skipping', Output) > 0);  // Déjà existe

  if Result then
    WriteLn('✓ Port ', Port, '/', Protocol, ' autorisé')
  else
    WriteLn('✗ Erreur : ', Output);
end;

function BloquerPort(Port: Word): Boolean;  
var
  Commande: String;
  Output: String;
begin
  Commande := Format('ufw deny %d', [Port]);
  Output := ExecuterCommandeSudo(Commande);

  Result := Pos('Rule added', Output) > 0;

  if Result then
    WriteLn('✓ Port ', Port, ' bloqué');
end;

function VerifierStatut: String;  
begin
  Result := ExecuterCommandeSudo('ufw status verbose');
end;

function ObtenirRegles: TStringList;  
var
  Output: String;
begin
  Output := ExecuterCommandeSudo('ufw status numbered');
  Result := TStringList.Create;
  Result.Text := Output;
end;

end.
```

**Utilisation :**

```pascal
program TestFirewallLinux;

{$mode objfpc}{$H+}

uses
  SysUtils, FirewallLinux;

var
  Regles: TStringList;
  i: Integer;
begin
  WriteLn('=== Configuration UFW (Ubuntu) ===');
  WriteLn;

  // Vérifier le statut
  WriteLn('Statut actuel :');
  WriteLn(VerifierStatut);
  WriteLn;

  // Autoriser le port 8080
  WriteLn('Configuration du port 8080...');
  if AutoriserPort(8080, 'tcp') then
    WriteLn('✓ Configuration réussie')
  else
    WriteLn('✗ Échec de la configuration');

  WriteLn;

  // Lister les règles
  WriteLn('Règles actuelles :');
  Regles := ObtenirRegles;
  try
    for i := 0 to Regles.Count - 1 do
      WriteLn(Regles[i]);
  finally
    Regles.Free;
  end;
end.
```

### Profils UFW

Créer un profil d'application personnalisé :

**1. Créer le fichier :**

```bash
sudo nano /etc/ufw/applications.d/monapp
```

**2. Contenu du fichier :**

```ini
[MonServeurPascal]
title=Mon Serveur Pascal  
description=Serveur HTTP développé en FreePascal  
ports=8080/tcp

[MonAppComplete]
title=Application Complète  
description=Application avec plusieurs ports  
ports=8080,8443/tcp|5000/udp
```

**3. Utiliser le profil :**

```bash
# Recharger les profils
sudo ufw app update MonServeurPascal

# Voir les détails
sudo ufw app info MonServeurPascal

# Autoriser
sudo ufw allow MonServeurPascal
```

### iptables (niveau bas)

UFW est une surcouche simplifiée d'iptables. Voici les équivalents :

**Commandes iptables directes :**

```bash
# Autoriser le port 8080 en entrée
sudo iptables -A INPUT -p tcp --dport 8080 -j ACCEPT

# Autoriser depuis une IP spécifique
sudo iptables -A INPUT -s 192.168.1.100 -p tcp --dport 8080 -j ACCEPT

# Bloquer une IP
sudo iptables -A INPUT -s 203.0.113.100 -j DROP

# Lister les règles
sudo iptables -L -n -v

# Sauvegarder les règles
sudo iptables-save > /etc/iptables/rules.v4

# Restaurer les règles
sudo iptables-restore < /etc/iptables/rules.v4
```

**Rendre les règles persistantes :**

```bash
# Installer iptables-persistent
sudo apt install iptables-persistent

# Sauvegarder
sudo netfilter-persistent save

# Restaurer au démarrage (automatique)
sudo systemctl enable netfilter-persistent
```

## Tests et diagnostic

### Tester l'ouverture d'un port

**Depuis Windows :**

```batch
REM Tester avec telnet  
telnet localhost 8080

REM Tester avec PowerShell  
Test-NetConnection -ComputerName localhost -Port 8080

REM Scanner les ports ouverts  
netstat -an | findstr "8080"  
netstat -an | findstr "LISTENING"
```

**Depuis Linux :**

```bash
# Tester avec telnet
telnet localhost 8080

# Tester avec netcat
nc -zv localhost 8080

# Scanner les ports ouverts
netstat -tuln | grep 8080  
ss -tuln | grep 8080

# Tous les ports en écoute
netstat -tuln | grep LISTEN
```

### Programme de test Pascal

```pascal
program TestPort;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Sockets;

function TesterPort(const Host: String; Port: Word): Boolean;  
var
  Socket: TSocket;
  Addr: TSockAddr;
  Timeout: TTimeVal;
begin
  Result := False;

  Socket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Socket < 0 then
    Exit;

  try
    // Configurer un timeout de 5 secondes
    Timeout.tv_sec := 5;
    Timeout.tv_usec := 0;
    fpSetSockOpt(Socket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));

    // Préparer l'adresse
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(Port);
    Addr.sin_addr := StrToNetAddr(Host);

    // Essayer de se connecter
    Result := fpConnect(Socket, @Addr, SizeOf(Addr)) = 0;
  finally
    fpClose(Socket);
  end;
end;

var
  Port: Word;
begin
  WriteLn('=== Test de ports ===');
  WriteLn;

  Write('Port à tester : ');
  ReadLn(Port);

  Write('Test en cours...');

  if TesterPort('127.0.0.1', Port) then
    WriteLn(' ✓ Port ', Port, ' est OUVERT')
  else
    WriteLn(' ✗ Port ', Port, ' est FERMÉ ou bloqué');

  ReadLn;
end.
```

### Serveur de test simple

```pascal
program ServeurTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Sockets;

var
  ListenSocket, ClientSocket: TSocket;
  Addr, ClientAddr: TSockAddr;
  AddrLen: TSockLen;
  Port: Word;
  Buffer: array[0..1023] of Char;
  BytesReceived: Integer;
begin
  WriteLn('=== Serveur de test ===');

  Write('Port d''écoute : ');
  ReadLn(Port);

  // Créer le socket
  ListenSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if ListenSocket < 0 then
  begin
    WriteLn('Erreur : Impossible de créer le socket');
    Exit;
  end;

  try
    // Configurer pour réutiliser l'adresse
    fpSetSockOpt(ListenSocket, SOL_SOCKET, SO_REUSEADDR, @True, SizeOf(Boolean));

    // Bind
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(Port);
    Addr.sin_addr.s_addr := INADDR_ANY;

    if fpBind(ListenSocket, @Addr, SizeOf(Addr)) < 0 then
    begin
      WriteLn('Erreur : Impossible de bind le port ', Port);
      WriteLn('Le port est peut-être déjà utilisé ou le firewall bloque');
      Exit;
    end;

    // Listen
    if fpListen(ListenSocket, 5) < 0 then
    begin
      WriteLn('Erreur : Impossible d''écouter');
      Exit;
    end;

    WriteLn('✓ Serveur en écoute sur le port ', Port);
    WriteLn('✓ Le firewall autorise ce port !');
    WriteLn;
    WriteLn('Attente de connexions... (Ctrl+C pour arrêter)');
    WriteLn;

    // Accepter une connexion
    AddrLen := SizeOf(ClientAddr);
    ClientSocket := fpAccept(ListenSocket, @ClientAddr, @AddrLen);

    if ClientSocket >= 0 then
    begin
      WriteLn('✓ Client connecté depuis : ', NetAddrToStr(ClientAddr.sin_addr));

      // Envoyer un message de bienvenue
      Buffer := 'Bonjour ! Connexion réussie.' + #13#10;
      fpSend(ClientSocket, @Buffer, Length(Buffer), 0);

      // Recevoir des données
      BytesReceived := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer), 0);
      if BytesReceived > 0 then
      begin
        SetString(Buffer, PChar(@Buffer), BytesReceived);
        WriteLn('Message reçu : ', Buffer);
      end;

      fpClose(ClientSocket);
    end;

  finally
    fpClose(ListenSocket);
  end;

  WriteLn('Serveur arrêté');
  ReadLn;
end.
```

### Outils de diagnostic réseau

**Windows :**

```batch
REM Afficher les connexions actives  
netstat -an

REM Afficher avec le nom du programme  
netstat -anb

REM Afficher les statistiques  
netstat -s

REM Tester la connectivité  
ping 8.8.8.8

REM Tracer la route  
tracert google.com

REM Résolution DNS  
nslookup google.com

REM Configuration IP  
ipconfig /all

REM Renouveler l'IP (DHCP)  
ipconfig /release  
ipconfig /renew

REM Vider le cache DNS  
ipconfig /flushdns
```

**Linux/Ubuntu :**

```bash
# Afficher les connexions actives
netstat -tuln  
ss -tuln

# Afficher avec le processus
sudo netstat -tulnp  
sudo ss -tulnp

# Tester la connectivité
ping -c 4 8.8.8.8

# Tracer la route
traceroute google.com  
tracepath google.com

# Résolution DNS
nslookup google.com  
dig google.com

# Configuration IP
ip addr show  
ifconfig

# Configuration détaillée
ip addr show dev eth0

# Renouveler l'IP (DHCP)
sudo dhclient -r  
sudo dhclient

# Vider le cache DNS
sudo systemd-resolve --flush-caches
```

## Configuration réseau avancée

### Interfaces réseau

**Windows - Lister les interfaces :**

```batch
REM Lister toutes les interfaces  
ipconfig

REM PowerShell - Plus d'infos  
Get-NetAdapter

REM Informations détaillées  
Get-NetAdapter | Format-List

REM Interface spécifique  
Get-NetAdapter -Name "Ethernet"
```

**Linux - Lister les interfaces :**

```bash
# Toutes les interfaces
ip link show

# Interfaces avec IP
ip addr show

# Statistiques
ip -s link show

# Interface spécifique
ip addr show dev eth0
```

### Configurer une IP statique

**Windows (PowerShell) :**

```powershell
# Définir une IP statique
New-NetIPAddress `
  -InterfaceAlias "Ethernet" `
  -IPAddress 192.168.1.100 `
  -PrefixLength 24 `
  -DefaultGateway 192.168.1.1

# Définir les DNS
Set-DnsClientServerAddress `
  -InterfaceAlias "Ethernet" `
  -ServerAddresses 8.8.8.8,8.8.4.4

# Revenir en DHCP
Set-NetIPInterface -InterfaceAlias "Ethernet" -Dhcp Enabled  
Set-DnsClientServerAddress -InterfaceAlias "Ethernet" -ResetServerAddresses
```

**Linux (Ubuntu) - Netplan :**

Éditer `/etc/netplan/01-netcfg.yaml` :

```yaml
network:
  version: 2
  renderer: networkd
  ethernets:
    eth0:
      addresses:
        - 192.168.1.100/24
      gateway4: 192.168.1.1
      nameservers:
        addresses:
          - 8.8.8.8
          - 8.8.4.4
```

Appliquer :

```bash
sudo netplan apply
```

**Linux - Interface temporaire :**

```bash
# Ajouter une IP
sudo ip addr add 192.168.1.100/24 dev eth0

# Définir la passerelle
sudo ip route add default via 192.168.1.1

# Supprimer une IP
sudo ip addr del 192.168.1.100/24 dev eth0
```

### Récupérer les interfaces depuis Pascal

```pascal
unit NetworkInterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TNetworkInterface = record
    Name: String;
    IPAddress: String;
    MACAddress: String;
    Status: String;
  end;

function GetNetworkInterfaces: array of TNetworkInterface;  
procedure DisplayInterfaces;

implementation

uses
  Process;

function ExecuteCommand(const Cmd: String): String;  
var
  Proc: TProcess;
  Output: TStringList;
begin
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Proc.Executable := {$IFDEF WINDOWS}'cmd.exe'{$ELSE}'/bin/bash'{$ENDIF};
    Proc.Parameters.Add({$IFDEF WINDOWS}'/c'{$ELSE}'-c'{$ENDIF});
    Proc.Parameters.Add(Cmd);
    Proc.Options := [poWaitOnExit, poUsePipes];
    Proc.Execute;

    Output.LoadFromStream(Proc.Output);
    Result := Output.Text;
  finally
    Output.Free;
    Proc.Free;
  end;
end;

function GetNetworkInterfaces: array of TNetworkInterface;  
var
  Output: String;
  Lines: TStringList;
  i, Count: Integer;
begin
  {$IFDEF WINDOWS}
  Output := ExecuteCommand('ipconfig /all');
  {$ELSE}
  Output := ExecuteCommand('ip addr show');
  {$ENDIF}

  // Parser la sortie (implémentation simplifiée)
  Lines := TStringList.Create;
  try
    Lines.Text := Output;
    Count := 0;
    SetLength(Result, 10);  // Taille maximale

    for i := 0 to Lines.Count - 1 do
    begin
      // Détecter une nouvelle interface
      {$IFDEF WINDOWS}
      if Pos('Carte', Lines[i]) > 0 then
      {$ELSE}
      if Pos(':', Lines[i]) > 0 then
      {$ENDIF}
      begin
        if Count < Length(Result) then
        begin
          Result[Count].Name := Trim(Lines[i]);
          Inc(Count);
        end;
      end;
    end;

    SetLength(Result, Count);
  finally
    Lines.Free;
  end;
end;

procedure DisplayInterfaces;  
var
  Interfaces: array of TNetworkInterface;
  i: Integer;
begin
  WriteLn('=== Interfaces réseau ===');
  WriteLn;

  Interfaces := GetNetworkInterfaces;

  for i := 0 to High(Interfaces) do
  begin
    WriteLn(i + 1, '. ', Interfaces[i].Name);
    if Interfaces[i].IPAddress <> '' then
      WriteLn('   IP : ', Interfaces[i].IPAddress);
    if Interfaces[i].MACAddress <> '' then
      WriteLn('   MAC : ', Interfaces[i].MACAddress);
    WriteLn;
  end;
end;

end.
```

## Routage et NAT

### Tables de routage

**Windows - Afficher les routes :**

```batch
REM Afficher la table de routage  
route print

REM Afficher seulement IPv4  
route print -4

REM PowerShell  
Get-NetRoute
```

**Ajouter/Supprimer des routes :**

```batch
REM Ajouter une route  
route add 192.168.2.0 mask 255.255.255.0 192.168.1.1

REM Ajouter une route persistante  
route add -p 192.168.2.0 mask 255.255.255.0 192.168.1.1

REM Supprimer une route  
route delete 192.168.2.0

REM PowerShell  
New-NetRoute -DestinationPrefix "192.168.2.0/24" -NextHop "192.168.1.1"  
Remove-NetRoute -DestinationPrefix "192.168.2.0/24"
```

**Linux - Afficher les routes :**

```bash
# Afficher la table de routage
ip route show  
route -n

# Route par défaut
ip route show default
```

**Ajouter/Supprimer des routes :**

```bash
# Ajouter une route
sudo ip route add 192.168.2.0/24 via 192.168.1.1

# Ajouter une route persistante (Netplan)
# Éditer /etc/netplan/01-netcfg.yaml et ajouter :
# routes:
#   - to: 192.168.2.0/24
#     via: 192.168.1.1

# Supprimer une route
sudo ip route del 192.168.2.0/24
```

### NAT (Network Address Translation)

**Windows - Partage de connexion Internet (ICS) :**

```powershell
# Activer le partage de connexion
# Via interface graphique :
# 1. Panneau de configuration → Réseau et Internet
# 2. Centre Réseau et partage
# 3. Modifier les paramètres de la carte
# 4. Clic droit sur la connexion → Propriétés
# 5. Onglet Partage → Cocher "Autoriser d'autres utilisateurs..."

# PowerShell (nécessite des droits admin)
$public = Get-NetAdapter | Where-Object {$_.Status -eq "Up"} | Select-Object -First 1
$private = Get-NetAdapter | Where-Object {$_.Status -eq "Up"} | Select-Object -Skip 1 -First 1

# Configurer le NAT
New-NetNat -Name "MonNAT" -InternalIPInterfaceAddressPrefix "192.168.137.0/24"
```

**Linux - Configuration NAT avec iptables :**

```bash
# Activer le forwarding IP
sudo sysctl -w net.ipv4.ip_forward=1

# Rendre persistant
echo "net.ipv4.ip_forward=1" | sudo tee -a /etc/sysctl.conf

# Configurer NAT (masquerading)
sudo iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE  
sudo iptables -A FORWARD -i eth1 -o eth0 -j ACCEPT  
sudo iptables -A FORWARD -i eth0 -o eth1 -m state --state RELATED,ESTABLISHED -j ACCEPT

# Sauvegarder
sudo iptables-save | sudo tee /etc/iptables/rules.v4
```

### Redirection de port (Port Forwarding)

**Windows - netsh :**

```batch
REM Rediriger le port 80 externe vers le port 8080 interne  
netsh interface portproxy add v4tov4 ^
  listenport=80 ^
  listenaddress=0.0.0.0 ^
  connectport=8080 ^
  connectaddress=127.0.0.1

REM Voir les redirections  
netsh interface portproxy show v4tov4

REM Supprimer une redirection  
netsh interface portproxy delete v4tov4 ^
  listenport=80 ^
  listenaddress=0.0.0.0
```

**Linux - iptables :**

```bash
# Rediriger le port 80 vers 8080
sudo iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 8080

# Rediriger vers une autre machine
sudo iptables -t nat -A PREROUTING -p tcp --dport 80 -j DNAT --to-destination 192.168.1.100:8080  
sudo iptables -A FORWARD -p tcp -d 192.168.1.100 --dport 8080 -j ACCEPT

# Voir les règles NAT
sudo iptables -t nat -L -n -v
```

## VPN et tunnels

### Configuration OpenVPN

**Installation :**

**Windows :**
- Télécharger depuis https://openvpn.net/community-downloads/
- Installer OpenVPN GUI

**Linux :**
```bash
sudo apt update  
sudo apt install openvpn
```

**Fichier de configuration client (.ovpn) :**

```
client  
dev tun  
proto udp  
remote vpn.example.com 1194  
resolv-retry infinite  
nobind  
persist-key  
persist-tun  
ca ca.crt  
cert client.crt  
key client.key  
remote-cert-tls server  
cipher AES-256-CBC  
verb 3
```

**Démarrer le VPN :**

**Windows :**
- Double-cliquer sur le fichier .ovpn dans OpenVPN GUI

**Linux :**
```bash
sudo openvpn --config client.ovpn
```

### WireGuard (VPN moderne)

**Installation :**

**Windows :**
- Télécharger depuis https://www.wireguard.com/install/

**Linux :**
```bash
sudo apt install wireguard
```

**Configuration (/etc/wireguard/wg0.conf) :**

```ini
[Interface]
PrivateKey = VOTRE_CLÉ_PRIVÉE  
Address = 10.0.0.2/24  
DNS = 8.8.8.8

[Peer]
PublicKey = CLÉ_PUBLIQUE_SERVEUR  
Endpoint = vpn.example.com:51820  
AllowedIPs = 0.0.0.0/0  
PersistentKeepalive = 25
```

**Démarrer WireGuard :**

**Linux :**
```bash
# Démarrer
sudo wg-quick up wg0

# Arrêter
sudo wg-quick down wg0

# Statut
sudo wg show

# Activer au démarrage
sudo systemctl enable wg-quick@wg0
```

### Tunnels SSH

**Port forwarding local (votre machine → serveur distant) :**

```bash
# Rediriger le port local 8080 vers le port 80 du serveur
ssh -L 8080:localhost:80 user@serveur.com

# Maintenant localhost:8080 → serveur.com:80
```

**Port forwarding distant (serveur → votre machine) :**

```bash
# Le port 8080 du serveur redirige vers votre port 80
ssh -R 8080:localhost:80 user@serveur.com

# Les gens peuvent accéder à votre serveur local via serveur.com:8080
```

**Tunnel dynamique (SOCKS proxy) :**

```bash
# Créer un proxy SOCKS sur le port 1080
ssh -D 1080 user@serveur.com

# Configurer votre navigateur pour utiliser localhost:1080 comme proxy SOCKS
```

## Sécurité réseau

### Scan de ports (pour tester votre sécurité)

**Nmap (Windows et Linux) :**

```bash
# Installer nmap
# Windows : https://nmap.org/download.html
# Linux : sudo apt install nmap

# Scanner les ports ouverts
nmap localhost

# Scanner des ports spécifiques
nmap -p 8080,8443 localhost

# Scanner une plage de ports
nmap -p 8000-9000 localhost

# Scan détaillé avec identification de services
nmap -sV localhost

# Scanner le réseau local
nmap 192.168.1.0/24

# Détecter le système d'exploitation
sudo nmap -O localhost
```

**Depuis Pascal :**

```pascal
program PortScanner;

{$mode objfpc}{$H+}

uses
  SysUtils, Sockets;

function IsPortOpen(const Host: String; Port: Word; TimeoutSec: Integer = 2): Boolean;  
var
  Socket: TSocket;
  Addr: TSockAddr;
  Timeout: TTimeVal;
begin
  Result := False;

  Socket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Socket < 0 then
    Exit;

  try
    // Timeout
    Timeout.tv_sec := TimeoutSec;
    Timeout.tv_usec := 0;
    fpSetSockOpt(Socket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
    fpSetSockOpt(Socket, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));

    // Adresse
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(Port);
    Addr.sin_addr := StrToNetAddr(Host);

    // Tenter la connexion
    Result := fpConnect(Socket, @Addr, SizeOf(Addr)) = 0;
  finally
    fpClose(Socket);
  end;
end;

procedure ScanPorts(const Host: String; StartPort, EndPort: Word);  
var
  Port: Word;
  OpenPorts: Integer;
begin
  WriteLn('=== Scanner de ports ===');
  WriteLn('Cible : ', Host);
  WriteLn('Plage : ', StartPort, ' - ', EndPort);
  WriteLn;
  WriteLn('Scan en cours...');
  WriteLn;

  OpenPorts := 0;

  for Port := StartPort to EndPort do
  begin
    Write(#13, 'Test du port ', Port, '...    ');

    if IsPortOpen(Host, Port, 1) then
    begin
      WriteLn;
      WriteLn('✓ Port ', Port, ' OUVERT');
      Inc(OpenPorts);
    end;
  end;

  WriteLn;
  WriteLn;
  WriteLn('=== Résultats ===');
  WriteLn('Ports ouverts : ', OpenPorts);
  WriteLn('Ports fermés : ', EndPort - StartPort + 1 - OpenPorts);
end;

var
  Host: String;
  StartPort, EndPort: Word;
begin
  Write('Hôte à scanner (ex: localhost) : ');
  ReadLn(Host);

  Write('Port de début : ');
  ReadLn(StartPort);

  Write('Port de fin : ');
  ReadLn(EndPort);

  WriteLn;

  ScanPorts(Host, StartPort, EndPort);

  WriteLn;
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
end.
```

### Fail2Ban (protection contre les attaques par force brute)

**Installation (Linux) :**

```bash
sudo apt install fail2ban
```

**Configuration de base (/etc/fail2ban/jail.local) :**

```ini
[DEFAULT]
bantime = 3600  
findtime = 600  
maxretry = 5

[sshd]
enabled = true  
port = ssh  
logpath = /var/log/auth.log

[http-get-dos]
enabled = true  
port = http,https  
filter = http-get-dos  
logpath = /var/log/apache*/access.log  
maxretry = 300  
findtime = 300  
bantime = 600
```

**Créer un filtre personnalisé pour votre application :**

`/etc/fail2ban/filter.d/monapp.conf` :

```ini
[Definition]
failregex = ^<HOST>.*Failed login attempt  
ignoreregex =
```

**Activer :**

```bash
sudo systemctl enable fail2ban  
sudo systemctl start fail2ban

# Voir les statuts
sudo fail2ban-client status

# Voir les bans d'une jail
sudo fail2ban-client status sshd

# Débanner une IP
sudo fail2ban-client set sshd unbanip 192.168.1.100
```

### Rate Limiting dans votre application

```pascal
unit RateLimiter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, SyncObjs;

type
  TRateLimiter = class
  private
    FRequests: TFPHashList;
    FLock: TCriticalSection;
    FMaxRequests: Integer;
    FTimeWindow: Integer;  // en secondes
  public
    constructor Create(MaxRequests, TimeWindowSec: Integer);
    destructor Destroy; override;

    function AllowRequest(const ClientIP: String): Boolean;
    procedure CleanOldEntries;
  end;

implementation

type
  TRequestList = class(TList);

constructor TRateLimiter.Create(MaxRequests, TimeWindowSec: Integer);  
begin
  inherited Create;
  FRequests := TFPHashList.Create;
  FLock := TCriticalSection.Create;
  FMaxRequests := MaxRequests;
  FTimeWindow := TimeWindowSec;
end;

destructor TRateLimiter.Destroy;  
var
  i: Integer;
begin
  for i := 0 to FRequests.Count - 1 do
    TRequestList(FRequests[i]).Free;

  FRequests.Free;
  FLock.Free;
  inherited Destroy;
end;

function TRateLimiter.AllowRequest(const ClientIP: String): Boolean;  
var
  List: TRequestList;
  Now: TDateTime;
  i: Integer;
  Count: Integer;
begin
  FLock.Enter;
  try
    Now := SysUtils.Now;

    // Trouver ou créer la liste pour cette IP
    List := TRequestList(FRequests.Find(ClientIP));
    if List = nil then
    begin
      List := TRequestList.Create;
      FRequests.Add(ClientIP, List);
    end;

    // Nettoyer les anciennes requêtes
    for i := List.Count - 1 downto 0 do
    begin
      if SecondsBetween(Now, TDateTime(List[i])) > FTimeWindow then
        List.Delete(i);
    end;

    // Vérifier la limite
    Count := List.Count;
    if Count >= FMaxRequests then
      Exit(False);

    // Ajouter cette requête
    List.Add(Pointer(PtrInt(Now)));
    Result := True;
  finally
    FLock.Leave;
  end;
end;

procedure TRateLimiter.CleanOldEntries;  
var
  i, j: Integer;
  List: TRequestList;
  Now: TDateTime;
begin
  FLock.Enter;
  try
    Now := SysUtils.Now;

    for i := 0 to FRequests.Count - 1 do
    begin
      List := TRequestList(FRequests[i]);
      for j := List.Count - 1 downto 0 do
      begin
        if SecondsBetween(Now, TDateTime(List[j])) > FTimeWindow then
          List.Delete(j);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
```

**Utilisation dans un serveur :**

```pascal
var
  RateLimiter: TRateLimiter;
  ClientIP: String;
begin
  RateLimiter := TRateLimiter.Create(100, 60);  // 100 requêtes par minute
  try
    // Dans la boucle de traitement des connexions
    ClientIP := GetClientIPAddress(ClientSocket);

    if not RateLimiter.AllowRequest(ClientIP) then
    begin
      WriteLn('Rate limit dépassé pour : ', ClientIP);
      SendHTTPResponse(ClientSocket, 429, 'Too Many Requests');
      CloseSocket(ClientSocket);
      Continue;
    end;

    // Traiter la requête normalement
    HandleRequest(ClientSocket);
  finally
    RateLimiter.Free;
  end;
end;
```

## Monitoring et logs

### Surveiller les connexions actives

**Programme de monitoring :**

```pascal
program NetworkMonitor;

{$mode objfpc}{$H+}

uses
  SysUtils, Process, Classes;

procedure DisplayConnections;  
var
  Proc: TProcess;
  Output: TStringList;
  i: Integer;
begin
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    Proc.Executable := 'netstat';
    Proc.Parameters.Add('-an');
    {$ELSE}
    Proc.Executable := 'ss';
    Proc.Parameters.Add('-tuln');
    {$ENDIF}

    Proc.Options := [poWaitOnExit, poUsePipes];
    Proc.Execute;

    Output.LoadFromStream(Proc.Output);

    WriteLn('=== Connexions actives ===');
    WriteLn;

    for i := 0 to Output.Count - 1 do
    begin
      if (Pos('LISTEN', Output[i]) > 0) or
         (Pos('ESTABLISHED', Output[i]) > 0) then
        WriteLn(Output[i]);
    end;
  finally
    Output.Free;
    Proc.Free;
  end;
end;

begin
  while True do
  begin
    {$IFDEF WINDOWS}
    ExecuteProcess('cls', '');
    {$ELSE}
    ExecuteProcess('clear', '');
    {$ENDIF}

    DisplayConnections;

    WriteLn;
    WriteLn('Actualisation toutes les 5 secondes... (Ctrl+C pour arrêter)');
    Sleep(5000);
  end;
end.
```

### Logger les connexions

```pascal
unit ConnectionLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TConnectionLogger = class
  private
    FLogFile: TextFile;
    FFilename: String;
  public
    constructor Create(const Filename: String);
    destructor Destroy; override;

    procedure LogConnection(const ClientIP: String; Port: Word; Accepted: Boolean);
    procedure LogDisconnection(const ClientIP: String);
    procedure LogError(const ClientIP, ErrorMsg: String);
  end;

implementation

constructor TConnectionLogger.Create(const Filename: String);  
begin
  inherited Create;
  FFilename := Filename;
  AssignFile(FLogFile, FFilename);

  if FileExists(FFilename) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
end;

destructor TConnectionLogger.Destroy;  
begin
  CloseFile(FLogFile);
  inherited Destroy;
end;

procedure TConnectionLogger.LogConnection(const ClientIP: String; Port: Word; Accepted: Boolean);  
var
  Timestamp: String;
  Status: String;
begin
  Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  if Accepted then
    Status := 'ACCEPTED'
  else
    Status := 'REJECTED';

  WriteLn(FLogFile, Format('[%s] %s - Connection from %s:%d',
                          [Timestamp, Status, ClientIP, Port]));
  Flush(FLogFile);
end;

procedure TConnectionLogger.LogDisconnection(const ClientIP: String);  
var
  Timestamp: String;
begin
  Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  WriteLn(FLogFile, Format('[%s] DISCONNECTED - %s', [Timestamp, ClientIP]));
  Flush(FLogFile);
end;

procedure TConnectionLogger.LogError(const ClientIP, ErrorMsg: String);  
var
  Timestamp: String;
begin
  Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  WriteLn(FLogFile, Format('[%s] ERROR - %s : %s', [Timestamp, ClientIP, ErrorMsg]));
  Flush(FLogFile);
end;

end.
```

**Exemple d'utilisation dans un serveur :**

```pascal
program ServeurAvecLogs;

{$mode objfpc}{$H+}

uses
  SysUtils, Sockets, ConnectionLogger;

var
  ListenSocket, ClientSocket: TSocket;
  Addr, ClientAddr: TSockAddr;
  AddrLen: TSockLen;
  Logger: TConnectionLogger;
  ClientIP: String;
begin
  Logger := TConnectionLogger.Create('server.log');
  try
    // Créer et configurer le socket
    ListenSocket := fpSocket(AF_INET, SOCK_STREAM, 0);

    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(8080);
    Addr.sin_addr.s_addr := INADDR_ANY;

    fpBind(ListenSocket, @Addr, SizeOf(Addr));
    fpListen(ListenSocket, 5);

    WriteLn('Serveur en écoute sur le port 8080...');

    while True do
    begin
      AddrLen := SizeOf(ClientAddr);
      ClientSocket := fpAccept(ListenSocket, @ClientAddr, @AddrLen);

      if ClientSocket >= 0 then
      begin
        ClientIP := NetAddrToStr(ClientAddr.sin_addr);

        // Logger la connexion
        Logger.LogConnection(ClientIP, ntohs(ClientAddr.sin_port), True);
        WriteLn('Client connecté : ', ClientIP);

        try
          // Traiter le client...
          // ...

        except
          on E: Exception do
            Logger.LogError(ClientIP, E.Message);
        end;

        // Logger la déconnexion
        Logger.LogDisconnection(ClientIP);
        fpClose(ClientSocket);
      end;
    end;
  finally
    Logger.Free;
    fpClose(ListenSocket);
  end;
end.
```

**Exemple de fichier log généré :**

```
[2025-01-15 10:30:45] ACCEPTED - Connection from 192.168.1.100:52341
[2025-01-15 10:30:50] DISCONNECTED - 192.168.1.100
[2025-01-15 10:31:12] ACCEPTED - Connection from 192.168.1.105:52389
[2025-01-15 10:31:15] ERROR - 192.168.1.105 : Invalid request format
[2025-01-15 10:31:15] DISCONNECTED - 192.168.1.105
[2025-01-15 10:32:05] REJECTED - Connection from 203.0.113.50:41234
```

## Bonnes pratiques de sécurité

### Checklist de sécurité réseau

**Configuration firewall :**

- [ ] Bloquer tous les ports par défaut
- [ ] Ouvrir uniquement les ports nécessaires
- [ ] Utiliser des règles spécifiques (pas de 0.0.0.0:* en production)
- [ ] Limiter l'accès par IP source si possible
- [ ] Séparer les règles par profil (public/privé/domaine)
- [ ] Logger les connexions bloquées

**Application :**

- [ ] Écouter sur localhost (127.0.0.1) pour les services internes
- [ ] Valider toutes les entrées utilisateur
- [ ] Implémenter un rate limiting
- [ ] Utiliser des timeouts appropriés
- [ ] Logger toutes les connexions
- [ ] Gérer proprement les erreurs réseau

**Mise en production :**

- [ ] Changer les ports par défaut si possible
- [ ] Utiliser HTTPS/TLS pour les communications sensibles
- [ ] Activer fail2ban ou équivalent
- [ ] Monitorer les logs régulièrement
- [ ] Mettre à jour le système et l'application
- [ ] Tester avec un scan de ports

### Principe du moindre privilège

**Windows - Exécuter sans droits admin :**

```pascal
{$IFDEF WINDOWS}
uses
  Windows;

function IsRunAsAdmin: Boolean;  
var
  TokenHandle: THandle;
  Elevation: TOKEN_ELEVATION;
  ReturnLength: DWORD;
begin
  Result := False;

  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
  try
    if GetTokenInformation(TokenHandle, TokenElevation, @Elevation,
                          SizeOf(Elevation), ReturnLength) then
      Result := Elevation.TokenIsElevated <> 0;
  finally
    CloseHandle(TokenHandle);
  end;
end;

begin
  if IsRunAsAdmin then
    WriteLn('ATTENTION : Exécution en tant qu''administrateur')
  else
    WriteLn('Exécution en tant qu''utilisateur normal (recommandé)');
end;
{$ENDIF}
```

**Linux - Vérifier les permissions :**

```pascal
{$IFDEF UNIX}
uses
  BaseUnix;

function IsRunAsRoot: Boolean;  
begin
  Result := fpGetUID = 0;
end;

begin
  if IsRunAsRoot then
    WriteLn('ATTENTION : Exécution en tant que root')
  else
    WriteLn('Exécution en tant qu''utilisateur normal (recommandé)');
end;
{$ENDIF}
```

**Utiliser des ports > 1024 :**

```pascal
const
  // Éviter les ports privilégiés (< 1024)
  RECOMMENDED_PORT = 8080;  // ✓ Bon
  // PRIVILEGED_PORT = 80;  // ✗ Nécessite root/admin
```

### Validation des entrées réseau

```pascal
unit NetworkValidation;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, RegExpr;

function IsValidIPv4(const IP: String): Boolean;  
function IsValidPort(Port: Integer): Boolean;  
function IsValidHostname(const Hostname: String): Boolean;  
function SanitizeInput(const Input: String): String;

implementation

function IsValidIPv4(const IP: String): Boolean;  
var
  Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}' +
                        '(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$';
    Result := Regex.Exec(IP);
  finally
    Regex.Free;
  end;
end;

function IsValidPort(Port: Integer): Boolean;  
begin
  Result := (Port >= 1) and (Port <= 65535);
end;

function IsValidHostname(const Hostname: String): Boolean;  
var
  Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^[a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?' +
                        '(\.[a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?)*$';
    Result := Regex.Exec(Hostname) and (Length(Hostname) <= 253);
  finally
    Regex.Free;
  end;
end;

function SanitizeInput(const Input: String): String;  
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Input) do
  begin
    // Garder uniquement les caractères alphanumériques et quelques symboles
    if Input[i] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '-', '_'] then
      Result := Result + Input[i];
  end;
end;

end.
```

**Utilisation :**

```pascal
var
  UserIP: String;
  UserPort: Integer;
begin
  Write('Entrez une adresse IP : ');
  ReadLn(UserIP);

  if not IsValidIPv4(UserIP) then
  begin
    WriteLn('Adresse IP invalide');
    Exit;
  end;

  Write('Entrez un port : ');
  ReadLn(UserPort);

  if not IsValidPort(UserPort) then
  begin
    WriteLn('Port invalide (doit être entre 1 et 65535)');
    Exit;
  end;

  // Utiliser les valeurs validées...
end;
```

## Dépannage réseau

### Guide de résolution de problèmes

**Problème : Mon serveur ne répond pas**

**1. Vérifier que le serveur écoute :**

```bash
# Windows
netstat -an | findstr "8080"

# Linux
netstat -tuln | grep 8080  
ss -tuln | grep 8080
```

Si rien n'apparaît → Le serveur ne démarre pas ou n'écoute pas sur ce port.

**2. Vérifier le firewall :**

```bash
# Windows
netsh advfirewall firewall show rule name=all | findstr "8080"

# Linux
sudo ufw status | grep 8080  
sudo iptables -L -n | grep 8080
```

Si la règle n'existe pas → Créer une règle firewall.

**3. Tester localement :**

```bash
# Windows et Linux
telnet localhost 8080
# ou
curl http://localhost:8080
```

Si ça fonctionne localement mais pas à distance → Problème de firewall ou routeur.

**4. Vérifier le routage :**

```bash
# Tester depuis une autre machine
ping IP_DU_SERVEUR  
telnet IP_DU_SERVEUR 8080
```

**Problème : Connexion lente ou timeouts**

**1. Vérifier la latence :**

```bash
# Windows et Linux
ping IP_CIBLE

# Traceroute
# Windows
tracert IP_CIBLE
# Linux
traceroute IP_CIBLE
```

**2. Tester la bande passante :**

```bash
# Installer iperf3
# Windows : https://iperf.fr/iperf-download.php
# Linux : sudo apt install iperf3

# Serveur
iperf3 -s

# Client (sur une autre machine)
iperf3 -c IP_SERVEUR
```

**3. Vérifier les timeouts dans le code :**

```pascal
// Définir des timeouts appropriés
var
  Timeout: TTimeVal;
begin
  Timeout.tv_sec := 30;  // 30 secondes
  Timeout.tv_usec := 0;

  fpSetSockOpt(Socket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
  fpSetSockOpt(Socket, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
end;
```

### Outils de diagnostic intégrés

```pascal
program DiagnosticReseau;

{$mode objfpc}{$H+}

uses
  SysUtils, Process;

procedure TestConnectivite(const Host: String);  
var
  Proc: TProcess;
  Output: String;
begin
  WriteLn('=== Test de connectivité vers ', Host, ' ===');

  Proc := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    Proc.Executable := 'ping';
    Proc.Parameters.Add('-n');
    Proc.Parameters.Add('4');
    {$ELSE}
    Proc.Executable := 'ping';
    Proc.Parameters.Add('-c');
    Proc.Parameters.Add('4');
    {$ENDIF}
    Proc.Parameters.Add(Host);

    Proc.Options := [poWaitOnExit, poUsePipes];
    Proc.Execute;

    SetLength(Output, Proc.Output.NumBytesAvailable);
    Proc.Output.Read(Output[1], Length(Output));

    WriteLn(Output);
  finally
    Proc.Free;
  end;
end;

procedure AfficherConfiguration;  
var
  Proc: TProcess;
  Output: String;
begin
  WriteLn('=== Configuration réseau ===');

  Proc := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    Proc.Executable := 'ipconfig';
    {$ELSE}
    Proc.Executable := 'ip';
    Proc.Parameters.Add('addr');
    Proc.Parameters.Add('show');
    {$ENDIF}

    Proc.Options := [poWaitOnExit, poUsePipes];
    Proc.Execute;

    SetLength(Output, Proc.Output.NumBytesAvailable);
    Proc.Output.Read(Output[1], Length(Output));

    WriteLn(Output);
  finally
    Proc.Free;
  end;
end;

procedure AfficherPortsOuverts;  
var
  Proc: TProcess;
  Output: String;
begin
  WriteLn('=== Ports en écoute ===');

  Proc := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    Proc.Executable := 'netstat';
    Proc.Parameters.Add('-an');
    {$ELSE}
    Proc.Executable := 'ss';
    Proc.Parameters.Add('-tuln');
    {$ENDIF}

    Proc.Options := [poWaitOnExit, poUsePipes];
    Proc.Execute;

    SetLength(Output, Proc.Output.NumBytesAvailable);
    Proc.Output.Read(Output[1], Length(Output));

    WriteLn(Output);
  finally
    Proc.Free;
  end;
end;

procedure MenuPrincipal;  
var
  Choix: String;
begin
  repeat
    WriteLn;
    WriteLn('=== Diagnostic Réseau ===');
    WriteLn('1. Test de connectivité (ping)');
    WriteLn('2. Configuration réseau');
    WriteLn('3. Ports en écoute');
    WriteLn('4. Quitter');
    WriteLn;
    Write('Votre choix : ');
    ReadLn(Choix);
    WriteLn;

    // case sur des chaînes n'est pas supporté en FreePascal
    if Choix = '1' then
    begin
      Write('Hôte à tester : ');
      ReadLn(Choix);
      TestConnectivite(Choix);
    end
    else if Choix = '2' then
      AfficherConfiguration
    else if Choix = '3' then
      AfficherPortsOuverts
    else if Choix = '4' then
      Break
    else
      WriteLn('Choix invalide');

    WriteLn;
    WriteLn('Appuyez sur Entrée...');
    ReadLn;
  until False;
end;

begin
  MenuPrincipal;
end.
```

## Automatisation et scripts

### Script de configuration automatique

**Windows (Batch) :**

```batch
@echo off
echo === Configuration automatique du firewall ===  
echo.

REM Variables  
set APP_NAME=MonServeurPascal  
set APP_PATH=C:\Projets\MonServeur.exe  
set APP_PORT=8080

echo Configuration pour :  
echo   Application : %APP_NAME%  
echo   Chemin : %APP_PATH%  
echo   Port : %APP_PORT%  
echo.

REM Supprimer l'ancienne règle si elle existe  
netsh advfirewall firewall delete rule name="%APP_NAME%" >nul 2>&1

REM Créer la nouvelle règle  
echo Creation de la regle firewall...  
netsh advfirewall firewall add rule ^
  name="%APP_NAME%" ^
  dir=in ^
  action=allow ^
  program="%APP_PATH%" ^
  protocol=TCP ^
  localport=%APP_PORT% ^
  enable=yes

if %errorlevel% equ 0 (
    echo [OK] Regle creee avec succes
) else (
    echo [ERREUR] Echec de la creation de la regle
    pause
    exit /b 1
)

REM Vérifier la règle  
echo.  
echo Verification de la regle...  
netsh advfirewall firewall show rule name="%APP_NAME%"

echo.  
echo === Configuration terminee ===  
pause
```

**Linux (Bash) :**

```bash
#!/bin/bash

# Configuration automatique UFW

APP_NAME="MonServeurPascal"  
APP_PORT=8080

echo "=== Configuration automatique du firewall ==="  
echo ""  
echo "Configuration pour :"  
echo "  Application : $APP_NAME"  
echo "  Port : $APP_PORT"  
echo ""

# Vérifier si UFW est installé
if ! command -v ufw &> /dev/null; then
    echo "[ERREUR] UFW n'est pas installé"
    echo "Installation : sudo apt install ufw"
    exit 1
fi

# Vérifier si on est root
if [ "$EUID" -ne 0 ]; then
    echo "[ERREUR] Ce script doit être exécuté avec sudo"
    exit 1
fi

# Supprimer l'ancienne règle si elle existe
ufw delete allow $APP_PORT/tcp 2>/dev/null

# Créer la nouvelle règle
echo "Création de la règle firewall..."  
ufw allow $APP_PORT/tcp

if [ $? -eq 0 ]; then
    echo "[OK] Règle créée avec succès"
else
    echo "[ERREUR] Échec de la création de la règle"
    exit 1
fi

# Vérifier la règle
echo ""  
echo "Vérification de la règle..."  
ufw status | grep $APP_PORT

# Activer UFW si nécessaire
if ! ufw status | grep -q "Status: active"; then
    echo ""
    read -p "UFW n'est pas actif. L'activer maintenant ? (o/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Oo]$ ]]; then
        ufw enable
        echo "[OK] UFW activé"
    fi
fi

echo ""  
echo "=== Configuration terminée ==="
```

### Programme Pascal pour automatiser la configuration

```pascal
program ConfigReseau;

{$mode objfpc}{$H+}

uses
  SysUtils, Process;

function ExecuterCommande(const Cmd: String): Boolean;  
var
  Proc: TProcess;
  ExitCode: Integer;
begin
  Proc := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    Proc.Executable := 'cmd.exe';
    Proc.Parameters.Add('/c');
    {$ELSE}
    Proc.Executable := '/bin/bash';
    Proc.Parameters.Add('-c');
    {$ENDIF}
    Proc.Parameters.Add(Cmd);

    Proc.Options := [poWaitOnExit];
    Proc.Execute;

    ExitCode := Proc.ExitStatus;
    Result := ExitCode = 0;
  finally
    Proc.Free;
  end;
end;

procedure ConfigurerFirewall(const AppPath: String; Port: Word);  
var
  Commande: String;
begin
  WriteLn('=== Configuration du firewall ===');
  WriteLn('Application : ', AppPath);
  WriteLn('Port : ', Port);
  WriteLn;

  {$IFDEF WINDOWS}
  WriteLn('Configuration Windows Firewall...');
  Commande := Format(
    'netsh advfirewall firewall add rule name="MonApp" ' +
    'dir=in action=allow program="%s" protocol=TCP localport=%d',
    [AppPath, Port]
  );
  {$ELSE}
  WriteLn('Configuration UFW...');
  Commande := Format('sudo ufw allow %d/tcp', [Port]);
  {$ENDIF}

  if ExecuterCommande(Commande) then
    WriteLn('✓ Configuration réussie')
  else
    WriteLn('✗ Erreur de configuration');
end;

procedure VerifierConfiguration(Port: Word);  
var
  Commande: String;
begin
  WriteLn;
  WriteLn('Vérification de la configuration...');

  {$IFDEF WINDOWS}
  Commande := Format('netsh advfirewall firewall show rule name=all | findstr "%d"', [Port]);
  {$ELSE}
  Commande := Format('sudo ufw status | grep %d', [Port]);
  {$ENDIF}

  if ExecuterCommande(Commande) then
    WriteLn('✓ Port configuré')
  else
    WriteLn('⚠ Port non trouvé dans la configuration');
end;

var
  AppPath: String;
  Port: Word;
begin
  AppPath := ParamStr(0);  // Chemin de l'exécutable actuel
  Port := 8080;

  WriteLn('=== Configuration automatique réseau ===');
  WriteLn;

  {$IFNDEF WINDOWS}
  WriteLn('Note : Cette opération nécessite sudo');
  WriteLn;
  {$ENDIF}

  ConfigurerFirewall(AppPath, Port);
  VerifierConfiguration(Port);

  WriteLn;
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
end.
```

## Conclusion et ressources

### Récapitulatif

Dans ce tutoriel, nous avons couvert :

1. **Concepts fondamentaux** - Ports, protocoles, adresses IP
2. **Firewall Windows** - Configuration via GUI, netsh, PowerShell
3. **Firewall Linux** - UFW et iptables
4. **Tests et diagnostic** - Outils pour vérifier la configuration
5. **Configuration réseau** - Interfaces, routage, NAT
6. **Sécurité** - Rate limiting, validation, monitoring
7. **Dépannage** - Résolution de problèmes courants
8. **Automatisation** - Scripts de configuration

### Commandes essentielles à retenir

**Windows :**
```batch
netsh advfirewall firewall add rule name="..." dir=in action=allow protocol=TCP localport=8080  
netstat -an | findstr "8080"  
ipconfig /all
```

**Linux :**
```bash
sudo ufw allow 8080/tcp  
sudo ufw status  
netstat -tuln | grep 8080  
ip addr show
```

### Ressources complémentaires

**Documentation officielle :**
- Windows Firewall : https://docs.microsoft.com/en-us/windows/security/threat-protection/windows-firewall/
- UFW : https://help.ubuntu.com/community/UFW
- iptables : https://netfilter.org/documentation/

**Tutoriels et guides :**
- FreePascal Network Programming : https://wiki.freepascal.org/Networking
- Synapse Library : http://www.ararat.cz/synapse/
- Indy Components : https://www.indyproject.org/

**Outils utiles :**
- Wireshark - Analyse de paquets
- nmap - Scanner de ports
- iperf3 - Test de bande passante

### Bonnes pratiques finales

✅ **Toujours tester localement d'abord** (localhost/127.0.0.1)  
✅ **Documenter les ports utilisés** par votre application  
✅ **Logger les connexions** pour le débogage  
✅ **Utiliser des ports > 1024** pour éviter les droits admin  
✅ **Valider toutes les entrées** utilisateur  
✅ **Implémenter des timeouts** appropriés  
✅ **Monitorer régulièrement** les logs firewall  
✅ **Mettre à jour** le système et l'application  
✅ **Tester la configuration** après chaque changement  
✅ **Avoir un plan de secours** en cas de problème

### Mot de la fin

La configuration du firewall et du réseau est essentielle pour déployer des applications réseau sécurisées et fonctionnelles. Bien que cela puisse sembler complexe au début, avec de la pratique et les bons outils, vous serez capable de configurer efficacement n'importe quel environnement Windows ou Linux pour vos applications FreePascal/Lazarus.

**Conseil final :** Créez des scripts de configuration automatiques pour vos applications. Cela vous fera gagner beaucoup de temps et évitera les erreurs de configuration manuelle lors du déploiement.

Bon développement réseau avec FreePascal ! 🔒🌐

⏭️ [Multithreading et Concurrence](/11-multithreading-concurrence/README.md)
