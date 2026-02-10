🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.3 Protocoles TCP/UDP bas niveau

## Introduction aux protocoles réseau

Avant d'utiliser des bibliothèques de haut niveau comme Synapse ou Indy, il est important de comprendre comment fonctionnent les protocoles TCP et UDP au niveau le plus bas. Cette connaissance vous permettra de créer des protocoles personnalisés, de déboguer des problèmes réseau et d'optimiser vos applications.

### Qu'est-ce qu'un protocole bas niveau ?

Un protocole bas niveau signifie que vous travaillez directement avec les **sockets système** sans couches d'abstraction supplémentaires. Vous avez un contrôle total sur :

- La création et configuration des sockets
- L'envoi et la réception des données octet par octet
- La gestion des erreurs au niveau système
- Les options de socket avancées
- Le comportement du réseau

### TCP vs UDP : Les bases

#### TCP (Transmission Control Protocol)

TCP est un protocole **orienté connexion** et **fiable** :

```
Client                          Serveur
  |                                |
  |--- SYN ----------------------->|  (Demande de connexion)
  |<-- SYN-ACK --------------------|  (Acceptation)
  |--- ACK ----------------------->|  (Confirmation)
  |                                |
  |=== Connexion établie ==========|
  |                                |
  |--- Données ------------------->|
  |<-- ACK ------------------------|  (Accusé de réception)
  |<-- Données --------------------|
  |--- ACK ----------------------->|
  |                                |
  |--- FIN ----------------------->|  (Fermeture)
  |<-- ACK ------------------------|
  |<-- FIN ------------------------|
  |--- ACK ----------------------->|
```

**Caractéristiques TCP :**
- ✅ Connexion établie avant l'échange de données
- ✅ Garantie de livraison des données
- ✅ Ordre des paquets préservé
- ✅ Contrôle de flux et de congestion
- ❌ Plus lent qu'UDP
- ❌ Surcharge (overhead) plus importante

**Cas d'usage TCP :**
- Navigation web (HTTP/HTTPS)
- Transfert de fichiers (FTP)
- Emails (SMTP, POP3, IMAP)
- SSH, Telnet
- Toute application nécessitant une livraison garantie

#### UDP (User Datagram Protocol)

UDP est un protocole **sans connexion** et **non fiable** :

```
Client                          Serveur
  |                                |
  |--- Datagramme 1 -------------->|  (Envoi direct)
  |--- Datagramme 2 -------------->|
  |--- Datagramme 3 -------------->|
  |                                |
  |<-- Datagramme A ---------------|
  |<-- Datagramme B ---------------|
```

**Caractéristiques UDP :**
- ✅ Pas de connexion nécessaire
- ✅ Très rapide et léger
- ✅ Faible latence
- ❌ Aucune garantie de livraison
- ❌ L'ordre peut être modifié
- ❌ Pas de contrôle de flux

**Cas d'usage UDP :**
- Streaming vidéo/audio (perte acceptable)
- Jeux en ligne (vitesse > fiabilité)
- DNS (requêtes simples)
- VoIP (téléphonie IP)
- Découverte de services (broadcast)

## Sockets en FreePascal

FreePascal fournit l'unité `Sockets` pour travailler directement avec les sockets système.

### Les unités nécessaires

```pascal
uses
  Sockets,    // Fonctions socket de base
  BaseUnix,   // Fonctions Unix (Linux)
  WinSock2;   // Fonctions Windows (selon l'OS)
```

### Structure de base d'un socket

Un socket est représenté par un **descripteur de fichier** (file descriptor) :

```pascal
type
  TSocket = Integer;  // Sous Linux
  TSocket = THandle;  // Sous Windows

const
  INVALID_SOCKET = -1;  // Socket invalide
```

### Adresses réseau

Les adresses réseau sont stockées dans des structures spéciales :

```pascal
type
  // Structure pour IPv4
  TInetSockAddr = packed record
    sin_family: Word;           // Famille d'adresse (AF_INET)
    sin_port: Word;             // Port (en network byte order)
    sin_addr: TInAddr;          // Adresse IP
    sin_zero: array[0..7] of Byte;  // Padding
  end;

  // Adresse IP
  TInAddr = packed record
    s_addr: Cardinal;  // Adresse en format réseau
  end;
```

## Client TCP bas niveau

Voici comment créer un client TCP en utilisant directement les sockets :

```pascal
program SimpleTCPClient;
{$mode objfpc}{$H+}

uses
  SysUtils, Sockets;

function CreateTCPClient(const Host: string; Port: Word): TSocket;  
var
  Sock: TSocket;
  Addr: TInetSockAddr;
  HostEntry: THostEntry;
begin
  Result := INVALID_SOCKET;

  // 1. Création du socket
  Sock := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Sock = INVALID_SOCKET then
  begin
    WriteLn('Erreur: Impossible de créer le socket');
    Exit;
  end;

  WriteLn('Socket créé: ', Sock);

  // 2. Résolution du nom d'hôte
  if not GetHostByName(Host, HostEntry) then
  begin
    WriteLn('Erreur: Impossible de résoudre ', Host);
    CloseSocket(Sock);
    Exit;
  end;

  // 3. Préparation de l'adresse
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);  // Conversion en network byte order
  Addr.sin_addr := HostEntry.Addr;

  // 4. Connexion
  WriteLn('Connexion à ', Host, ':', Port, '...');
  if fpConnect(Sock, @Addr, SizeOf(Addr)) < 0 then
  begin
    WriteLn('Erreur: Connexion impossible (', SocketError, ')');
    CloseSocket(Sock);
    Exit;
  end;

  WriteLn('Connecté avec succès !');
  Result := Sock;
end;

procedure SendData(Sock: TSocket; const Data: string);  
var
  Sent: Integer;
begin
  Sent := fpSend(Sock, @Data[1], Length(Data), 0);
  if Sent < 0 then
    WriteLn('Erreur d''envoi: ', SocketError)
  else
    WriteLn('Envoyé: ', Sent, ' octets');
end;

function ReceiveData(Sock: TSocket; MaxLen: Integer): string;  
var
  Buffer: array[0..4095] of Char;
  Received: Integer;
begin
  Result := '';

  Received := fpRecv(Sock, @Buffer, MaxLen, 0);
  if Received < 0 then
    WriteLn('Erreur de réception: ', SocketError)
  else if Received = 0 then
    WriteLn('Connexion fermée par le serveur')
  else
  begin
    SetLength(Result, Received);
    Move(Buffer, Result[1], Received);
    WriteLn('Reçu: ', Received, ' octets');
  end;
end;

var
  Sock: TSocket;
  Response: string;

begin
  // Exemple: connexion à un serveur echo
  Sock := CreateTCPClient('localhost', 7);

  if Sock <> INVALID_SOCKET then
  begin
    try
      // Envoi de données
      SendData(Sock, 'Bonjour serveur' + #13#10);

      // Réception de la réponse
      Response := ReceiveData(Sock, 1024);
      WriteLn('Réponse: ', Response);

    finally
      // Fermeture du socket
      CloseSocket(Sock);
      WriteLn('Socket fermé');
    end;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Explications détaillées

#### 1. Création du socket

```pascal
Sock := fpSocket(AF_INET, SOCK_STREAM, 0);
```

- **AF_INET** : Famille d'adresse IPv4 (AF_INET6 pour IPv6)
- **SOCK_STREAM** : Socket en mode flux (TCP)
- **0** : Protocole par défaut (TCP pour SOCK_STREAM)

#### 2. Résolution DNS

```pascal
GetHostByName(Host, HostEntry)
```

Convertit un nom de domaine (exemple: "google.com") en adresse IP.

#### 3. Conversion byte order

```pascal
sin_port := htons(Port);
```

- **htons** : Host TO Network Short (conversion 16 bits)
- **htonl** : Host TO Network Long (conversion 32 bits)
- **ntohs** : Network TO Host Short
- **ntohl** : Network TO Host Long

Les processeurs peuvent stocker les nombres différemment (big-endian vs little-endian). Le réseau utilise toujours le **big-endian**, d'où la nécessité de conversion.

## Serveur TCP bas niveau

Voici un serveur TCP simple :

```pascal
program SimpleTCPServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Sockets;

function CreateTCPServer(Port: Word): TSocket;  
var
  Sock: TSocket;
  Addr: TInetSockAddr;
  OptVal: Integer;
begin
  Result := INVALID_SOCKET;

  // 1. Création du socket
  Sock := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Sock = INVALID_SOCKET then
  begin
    WriteLn('Erreur: Impossible de créer le socket');
    Exit;
  end;

  WriteLn('Socket serveur créé: ', Sock);

  // 2. Option SO_REUSEADDR (permet de réutiliser l'adresse immédiatement)
  OptVal := 1;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

  // 3. Liaison (bind) à une adresse et un port
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr.s_addr := INADDR_ANY;  // Écoute sur toutes les interfaces

  if fpBind(Sock, @Addr, SizeOf(Addr)) < 0 then
  begin
    WriteLn('Erreur: Impossible de lier le port ', Port, ' (', SocketError, ')');
    CloseSocket(Sock);
    Exit;
  end;

  WriteLn('Socket lié au port ', Port);

  // 4. Mise en écoute
  if fpListen(Sock, 5) < 0 then  // Queue de 5 connexions en attente
  begin
    WriteLn('Erreur: Impossible de se mettre en écoute (', SocketError, ')');
    CloseSocket(Sock);
    Exit;
  end;

  WriteLn('Serveur en écoute sur le port ', Port);
  Result := Sock;
end;

procedure HandleClient(ClientSock: TSocket; const ClientAddr: TInetSockAddr);  
var
  Buffer: array[0..4095] of Char;
  Received: Integer;
  ClientIP: string;
begin
  // Conversion de l'adresse IP en chaîne
  ClientIP := NetAddrToStr(ClientAddr.sin_addr);
  WriteLn('Nouveau client: ', ClientIP, ':', ntohs(ClientAddr.sin_port));

  try
    // Lecture des données
    Received := fpRecv(ClientSock, @Buffer, SizeOf(Buffer) - 1, 0);

    if Received > 0 then
    begin
      Buffer[Received] := #0;  // Terminaison
      WriteLn('Reçu (', Received, ' octets): ', Buffer);

      // Renvoi des données (echo)
      fpSend(ClientSock, @Buffer, Received, 0);
      WriteLn('Données renvoyées au client');
    end
    else if Received = 0 then
      WriteLn('Client déconnecté')
    else
      WriteLn('Erreur de réception: ', SocketError);

  finally
    CloseSocket(ClientSock);
    WriteLn('Connexion client fermée');
  end;
end;

var
  ServerSock, ClientSock: TSocket;
  ClientAddr: TInetSockAddr;
  AddrLen: TSockLen;

begin
  ServerSock := CreateTCPServer(8080);

  if ServerSock <> INVALID_SOCKET then
  begin
    try
      WriteLn('En attente de connexions...');
      WriteLn('Appuyez sur Ctrl+C pour arrêter');
      WriteLn;

      // Boucle d'acceptation
      while True do
      begin
        AddrLen := SizeOf(ClientAddr);

        // Acceptation d'une connexion cliente
        ClientSock := fpAccept(ServerSock, @ClientAddr, @AddrLen);

        if ClientSock < 0 then
        begin
          WriteLn('Erreur d''acceptation: ', SocketError);
          Continue;
        end;

        // Traitement du client
        HandleClient(ClientSock, ClientAddr);
        WriteLn;
      end;

    finally
      CloseSocket(ServerSock);
      WriteLn('Serveur arrêté');
    end;
  end;
end.
```

### Fonctions principales du serveur

#### fpBind - Liaison du socket

```pascal
fpBind(Sock, @Addr, SizeOf(Addr))
```

Associe le socket à une adresse IP et un port spécifiques.

**INADDR_ANY** : Écoute sur toutes les interfaces réseau disponibles (0.0.0.0).

#### fpListen - Mise en écoute

```pascal
fpListen(Sock, BacklogSize)
```

- **BacklogSize** : Nombre de connexions en attente dans la queue
- Typiquement entre 5 et 128

#### fpAccept - Acceptation d'une connexion

```pascal
ClientSock := fpAccept(ServerSock, @ClientAddr, @AddrLen)
```

Bloque jusqu'à ce qu'un client se connecte. Retourne un nouveau socket pour communiquer avec ce client.

## Client UDP bas niveau

UDP est plus simple car il n'y a pas de connexion :

```pascal
program SimpleUDPClient;
{$mode objfpc}{$H+}

uses
  SysUtils, Sockets;

function CreateUDPSocket: TSocket;  
var
  Sock: TSocket;
begin
  // Création d'un socket UDP (SOCK_DGRAM)
  Sock := fpSocket(AF_INET, SOCK_DGRAM, 0);

  if Sock = INVALID_SOCKET then
    WriteLn('Erreur: Impossible de créer le socket UDP')
  else
    WriteLn('Socket UDP créé: ', Sock);

  Result := Sock;
end;

procedure SendUDPMessage(Sock: TSocket; const Host: string;
                         Port: Word; const Message: string);
var
  Addr: TInetSockAddr;
  HostEntry: THostEntry;
  Sent: Integer;
begin
  // Résolution de l'hôte
  if not GetHostByName(Host, HostEntry) then
  begin
    WriteLn('Erreur: Impossible de résoudre ', Host);
    Exit;
  end;

  // Préparation de l'adresse de destination
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr := HostEntry.Addr;

  // Envoi du datagramme
  Sent := fpSendTo(Sock, @Message[1], Length(Message), 0,
                   @Addr, SizeOf(Addr));

  if Sent < 0 then
    WriteLn('Erreur d''envoi UDP: ', SocketError)
  else
    WriteLn('UDP envoyé: ', Sent, ' octets à ', Host, ':', Port);
end;

function ReceiveUDPMessage(Sock: TSocket; MaxLen: Integer;
                           out FromAddr: TInetSockAddr): string;
var
  Buffer: array[0..4095] of Char;
  Received: Integer;
  AddrLen: TSockLen;
begin
  Result := '';
  AddrLen := SizeOf(FromAddr);

  // Réception d'un datagramme
  Received := fpRecvFrom(Sock, @Buffer, MaxLen, 0, @FromAddr, @AddrLen);

  if Received < 0 then
    WriteLn('Erreur de réception UDP: ', SocketError)
  else if Received > 0 then
  begin
    SetLength(Result, Received);
    Move(Buffer, Result[1], Received);
    WriteLn('UDP reçu: ', Received, ' octets de ',
            NetAddrToStr(FromAddr.sin_addr), ':', ntohs(FromAddr.sin_port));
  end;
end;

var
  Sock: TSocket;
  Response: string;
  FromAddr: TInetSockAddr;

begin
  Sock := CreateUDPSocket;

  if Sock <> INVALID_SOCKET then
  begin
    try
      // Envoi d'un message UDP
      SendUDPMessage(Sock, 'localhost', 9000, 'Message UDP de test');

      // Attente optionnelle d'une réponse
      WriteLn('En attente de réponse (5 secondes max)...');

      // Configuration d'un timeout de réception
      SetSocketTimeout(Sock, 5000);  // 5 secondes

      Response := ReceiveUDPMessage(Sock, 1024, FromAddr);
      if Response <> '' then
        WriteLn('Réponse: ', Response);

    finally
      CloseSocket(Sock);
      WriteLn('Socket fermé');
    end;
  end;

  ReadLn;
end.
```

## Serveur UDP bas niveau

```pascal
program SimpleUDPServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Sockets;

function CreateUDPServer(Port: Word): TSocket;  
var
  Sock: TSocket;
  Addr: TInetSockAddr;
begin
  Result := INVALID_SOCKET;

  // Création du socket UDP
  Sock := fpSocket(AF_INET, SOCK_DGRAM, 0);
  if Sock = INVALID_SOCKET then
  begin
    WriteLn('Erreur: Impossible de créer le socket UDP');
    Exit;
  end;

  WriteLn('Socket UDP créé: ', Sock);

  // Liaison au port
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr.s_addr := INADDR_ANY;

  if fpBind(Sock, @Addr, SizeOf(Addr)) < 0 then
  begin
    WriteLn('Erreur: Impossible de lier le port ', Port, ' (', SocketError, ')');
    CloseSocket(Sock);
    Exit;
  end;

  WriteLn('Socket UDP lié au port ', Port);
  Result := Sock;
end;

procedure RunUDPServer(Sock: TSocket);  
var
  Buffer: array[0..4095] of Char;
  Received: Integer;
  ClientAddr: TInetSockAddr;
  AddrLen: TSockLen;
  ClientIP: string;
  ClientPort: Word;
begin
  WriteLn('Serveur UDP en écoute...');
  WriteLn('Appuyez sur Ctrl+C pour arrêter');
  WriteLn;

  while True do
  begin
    AddrLen := SizeOf(ClientAddr);

    // Réception d'un datagramme
    Received := fpRecvFrom(Sock, @Buffer, SizeOf(Buffer) - 1, 0,
                          @ClientAddr, @AddrLen);

    if Received > 0 then
    begin
      Buffer[Received] := #0;

      ClientIP := NetAddrToStr(ClientAddr.sin_addr);
      ClientPort := ntohs(ClientAddr.sin_port);

      WriteLn('Reçu de ', ClientIP, ':', ClientPort, ' (', Received, ' octets)');
      WriteLn('Données: ', Buffer);

      // Renvoi d'une réponse (echo)
      fpSendTo(Sock, @Buffer, Received, 0, @ClientAddr, AddrLen);
      WriteLn('Réponse envoyée');
      WriteLn;
    end
    else if Received < 0 then
      WriteLn('Erreur de réception: ', SocketError);
  end;
end;

var
  Sock: TSocket;

begin
  Sock := CreateUDPServer(9000);

  if Sock <> INVALID_SOCKET then
  begin
    try
      RunUDPServer(Sock);
    finally
      CloseSocket(Sock);
      WriteLn('Serveur arrêté');
    end;
  end;
end.
```

## Options de socket avancées

### Définir des timeouts

```pascal
procedure SetSocketTimeout(Sock: TSocket; TimeoutMS: Integer);  
var
  {$IFDEF WINDOWS}
  Timeout: DWORD;
  {$ELSE}
  Timeout: TTimeVal;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Windows utilise des millisecondes
  Timeout := TimeoutMS;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
  fpSetSockOpt(Sock, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
  {$ELSE}
  // Linux/Unix utilise une structure timeval
  Timeout.tv_sec := TimeoutMS div 1000;
  Timeout.tv_usec := (TimeoutMS mod 1000) * 1000;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
  fpSetSockOpt(Sock, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
  {$ENDIF}
end;
```

### Socket non-bloquant

```pascal
procedure SetNonBlocking(Sock: TSocket);  
var
  Flags: Integer;
begin
  {$IFDEF WINDOWS}
  Flags := 1;
  ioctlsocket(Sock, FIONBIO, @Flags);
  {$ELSE}
  Flags := fpFcntl(Sock, F_GETFL, 0);
  fpFcntl(Sock, F_SETFL, Flags or O_NONBLOCK);
  {$ENDIF}
end;
```

Avec un socket non-bloquant, les fonctions `fpRecv`, `fpSend`, `fpAccept` retournent immédiatement même s'il n'y a pas de données.

### Activer TCP_NODELAY (désactiver l'algorithme de Nagle)

```pascal
procedure SetTCPNoDelay(Sock: TSocket);  
var
  OptVal: Integer;
begin
  OptVal := 1;
  fpSetSockOpt(Sock, IPPROTO_TCP, TCP_NODELAY, @OptVal, SizeOf(OptVal));
end;
```

L'algorithme de Nagle regroupe les petits paquets pour optimiser la bande passante, mais ajoute de la latence. Désactivez-le pour les applications temps réel.

### Activer SO_KEEPALIVE

```pascal
procedure SetKeepAlive(Sock: TSocket);  
var
  OptVal: Integer;
begin
  OptVal := 1;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_KEEPALIVE, @OptVal, SizeOf(OptVal));
end;
```

Permet de détecter les connexions mortes en envoyant périodiquement des paquets de test.

### Définir la taille des buffers

```pascal
procedure SetBufferSizes(Sock: TSocket; SendBufSize, RecvBufSize: Integer);  
begin
  fpSetSockOpt(Sock, SOL_SOCKET, SO_SNDBUF, @SendBufSize, SizeOf(SendBufSize));
  fpSetSockOpt(Sock, SOL_SOCKET, SO_RCVBUF, @RecvBufSize, SizeOf(RecvBufSize));
end;
```

### Broadcast UDP

```pascal
procedure EnableBroadcast(Sock: TSocket);  
var
  OptVal: Integer;
begin
  OptVal := 1;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_BROADCAST, @OptVal, SizeOf(OptVal));
end;

// Envoi en broadcast
procedure SendBroadcast(Sock: TSocket; Port: Word; const Message: string);  
var
  Addr: TInetSockAddr;
begin
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr.s_addr := INADDR_BROADCAST;  // 255.255.255.255

  fpSendTo(Sock, @Message[1], Length(Message), 0, @Addr, SizeOf(Addr));
end;
```

## Multiplexage avec Select

`select` permet de surveiller plusieurs sockets simultanément :

```pascal
program SelectExample;
{$mode objfpc}{$H+}

uses
  SysUtils, Sockets, BaseUnix;

function WaitForData(Sock: TSocket; TimeoutMS: Integer): Boolean;  
var
  ReadFDs: TFDSet;
  Timeout: TTimeVal;
  Res: Integer;
begin
  // Initialisation du set de descripteurs
  fpFD_ZERO(ReadFDs);
  fpFD_SET(Sock, ReadFDs);

  // Configuration du timeout
  Timeout.tv_sec := TimeoutMS div 1000;
  Timeout.tv_usec := (TimeoutMS mod 1000) * 1000;

  // Attente de données avec timeout
  Res := fpSelect(Sock + 1, @ReadFDs, nil, nil, @Timeout);

  Result := (Res > 0) and (fpFD_ISSET(Sock, ReadFDs) = 1);
end;

procedure MultiSocketServer(Port: Word);  
var
  ServerSock, ClientSock: TSocket;
  ClientAddr: TInetSockAddr;
  AddrLen: TSockLen;
  ReadFDs, ActiveFDs: TFDSet;
  MaxFD, i: Integer;
  Clients: array[0..63] of TSocket;
  ClientCount: Integer;
  Buffer: array[0..1023] of Char;
  Received: Integer;
begin
  // Initialisation
  ClientCount := 0;
  for i := 0 to High(Clients) do
    Clients[i] := INVALID_SOCKET;

  // Création du serveur
  ServerSock := fpSocket(AF_INET, SOCK_STREAM, 0);
  // ... (code de bind et listen omis pour la clarté)

  WriteLn('Serveur multi-socket démarré');

  // Boucle principale
  while True do
  begin
    // Préparation des descripteurs à surveiller
    fpFD_ZERO(ActiveFDs);
    fpFD_SET(ServerSock, ActiveFDs);
    MaxFD := ServerSock;

    // Ajout des clients connectés
    for i := 0 to ClientCount - 1 do
    begin
      if Clients[i] <> INVALID_SOCKET then
      begin
        fpFD_SET(Clients[i], ActiveFDs);
        if Clients[i] > MaxFD then
          MaxFD := Clients[i];
      end;
    end;

    // Attente d'activité sur un socket
    if fpSelect(MaxFD + 1, @ActiveFDs, nil, nil, nil) > 0 then
    begin
      // Nouvelle connexion ?
      if fpFD_ISSET(ServerSock, ActiveFDs) = 1 then
      begin
        AddrLen := SizeOf(ClientAddr);
        ClientSock := fpAccept(ServerSock, @ClientAddr, @AddrLen);

        if ClientSock <> INVALID_SOCKET then
        begin
          WriteLn('Nouveau client connecté');
          Clients[ClientCount] := ClientSock;
          Inc(ClientCount);
        end;
      end;

      // Données d'un client ?
      for i := 0 to ClientCount - 1 do
      begin
        if (Clients[i] <> INVALID_SOCKET) and
           (fpFD_ISSET(Clients[i], ActiveFDs) = 1) then
        begin
          Received := fpRecv(Clients[i], @Buffer, SizeOf(Buffer), 0);

          if Received <= 0 then
          begin
            WriteLn('Client déconnecté');
            CloseSocket(Clients[i]);
            Clients[i] := INVALID_SOCKET;
          end
          else
          begin
            Buffer[Received] := #0;
            WriteLn('Reçu du client ', i, ': ', Buffer);

            // Echo au client
            fpSend(Clients[i], @Buffer, Received, 0);
          end;
        end;
      end;
    end;
  end;
end;

begin
  MultiSocketServer(8080);
end.
```

### Avantages de Select

- ✅ Gestion de multiples sockets dans un seul thread
- ✅ Évite le polling actif (économise le CPU)
- ✅ Permet des timeouts précis
- ✅ Portable entre systèmes

### Limitations de Select

- ❌ Limité à 1024 sockets sous Linux (FD_SETSIZE)
- ❌ Performance dégradée avec beaucoup de sockets
- ❌ Doit reconstruire le set à chaque itération

Pour des serveurs haute performance, considérez **epoll** (Linux) ou **IOCP** (Windows).

## Gestion des erreurs réseau

### Codes d'erreur communs

```pascal
procedure DisplaySocketError(ErrorCode: Integer);  
begin
  case ErrorCode of
    {$IFDEF WINDOWS}
    WSAECONNREFUSED:
      WriteLn('Connexion refusée par le serveur');
    WSAETIMEDOUT:
      WriteLn('Timeout de connexion');
    WSAECONNRESET:
      WriteLn('Connexion réinitialisée par le pair');
    WSAEADDRINUSE:
      WriteLn('Adresse déjà utilisée');
    WSAEACCES:
      WriteLn('Permission refusée');
    {$ELSE}
    ESysECONNREFUSED:
      WriteLn('Connexion refusée par le serveur');
    ESysETIMEDOUT:
      WriteLn('Timeout de connexion');
    ESysECONNRESET:
      WriteLn('Connexion réinitialisée par le pair');
    ESysEADDRINUSE:
      WriteLn('Adresse déjà utilisée');
    ESysEACCES:
      WriteLn('Permission refusée');
    ESysEPIPE:
      WriteLn('Broken pipe (socket fermé)');
    {$ENDIF}
  else
    WriteLn('Erreur socket: ', ErrorCode);
  end;
end;

// Utilisation
if fpConnect(Sock, @Addr, SizeOf(Addr)) < 0 then  
begin
  DisplaySocketError(SocketError);
  CloseSocket(Sock);
end;
```

### Gestion robuste des erreurs

```pascal
function SafeSend(Sock: TSocket; const Data: string): Boolean;  
var
  TotalSent, Sent: Integer;
  Remaining: Integer;
  Ptr: PChar;
begin
  Result := False;
  TotalSent := 0;
  Remaining := Length(Data);
  Ptr := @Data[1];

  while Remaining > 0 do
  begin
    Sent := fpSend(Sock, Ptr, Remaining, 0);

    if Sent < 0 then
    begin
      // Erreur
      if SocketError = EAGAIN then
      begin
        // Buffer plein, réessayer
        Sleep(10);
        Continue;
      end
      else
      begin
        WriteLn('Erreur d''envoi: ', SocketError);
        Exit;
      end;
    end
    else if Sent = 0 then
    begin
      WriteLn('Socket fermé durant l''envoi');
      Exit;
    end;

    Inc(TotalSent, Sent);
    Inc(Ptr, Sent);
    Dec(Remaining, Sent);
  end;

  Result := True;
  WriteLn('Envoyé: ', TotalSent, ' octets');
end;

function SafeRecv(Sock: TSocket; MaxLen: Integer; out Data: string): Boolean;  
var
  Buffer: array[0..4095] of Char;
  Received: Integer;
begin
  Result := False;
  Data := '';

  Received := fpRecv(Sock, @Buffer, Min(MaxLen, SizeOf(Buffer)), 0);

  if Received < 0 then
  begin
    if SocketError = EAGAIN then
    begin
      // Pas de données disponibles (socket non-bloquant)
      Result := True; // Pas une erreur fatale
    end
    else
      WriteLn('Erreur de réception: ', SocketError);
  end
  else if Received = 0 then
  begin
    WriteLn('Connexion fermée par le pair');
  end
  else
  begin
    SetLength(Data, Received);
    Move(Buffer, Data[1], Received);
    Result := True;
  end;
end;
```

## Protocole personnalisé simple

Créons un protocole de commandes simple :

```pascal
program CustomProtocolServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Sockets;

const
  // Codes de commande
  CMD_ECHO = 1;
  CMD_TIME = 2;
  CMD_QUIT = 3;
  CMD_PING = 4;

  // Codes de réponse
  RESP_OK = 0;
  RESP_ERROR = 1;

type
  // En-tête de paquet (8 octets)
  TPacketHeader = packed record
    Command: Byte;      // Code de commande
    Length: Word;       // Longueur des données
    Reserved: Byte;     // Réservé
    Checksum: Cardinal; // Somme de contrôle
  end;

function CalculateChecksum(const Data: string): Cardinal;  
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Data) do
    Result := Result + Ord(Data[i]);
end;

function SendPacket(Sock: TSocket; Command: Byte; const Data: string): Boolean;  
var
  Header: TPacketHeader;
  Sent: Integer;
begin
  Result := False;

  // Construction de l'en-tête
  Header.Command := Command;
  Header.Length := Length(Data);
  Header.Reserved := 0;
  Header.Checksum := CalculateChecksum(Data);

  // Envoi de l'en-tête
  Sent := fpSend(Sock, @Header, SizeOf(Header), 0);
  if Sent <> SizeOf(Header) then
  begin
    WriteLn('Erreur: Envoi partiel de l''en-tête');
    Exit;
  end;

  // Envoi des données si présentes
  if Length(Data) > 0 then
  begin
    Sent := fpSend(Sock, @Data[1], Length(Data), 0);
    if Sent <> Length(Data) then
    begin
      WriteLn('Erreur: Envoi partiel des données');
      Exit;
    end;
  end;

  Result := True;
end;

function ReceivePacket(Sock: TSocket; out Command: Byte;
                       out Data: string): Boolean;
var
  Header: TPacketHeader;
  Received: Integer;
  Buffer: array[0..4095] of Char;
  Checksum: Cardinal;
begin
  Result := False;
  Data := '';

  // Réception de l'en-tête
  Received := fpRecv(Sock, @Header, SizeOf(Header), 0);
  if Received <> SizeOf(Header) then
  begin
    WriteLn('Erreur: Réception partielle de l''en-tête');
    Exit;
  end;

  Command := Header.Command;

  // Réception des données si présentes
  if Header.Length > 0 then
  begin
    if Header.Length > SizeOf(Buffer) then
    begin
      WriteLn('Erreur: Données trop volumineuses');
      Exit;
    end;

    Received := fpRecv(Sock, @Buffer, Header.Length, 0);
    if Received <> Header.Length then
    begin
      WriteLn('Erreur: Réception partielle des données');
      Exit;
    end;

    SetLength(Data, Received);
    Move(Buffer, Data[1], Received);

    // Vérification de la somme de contrôle
    Checksum := CalculateChecksum(Data);
    if Checksum <> Header.Checksum then
    begin
      WriteLn('Erreur: Checksum invalide (reçu: ', Header.Checksum,
              ', calculé: ', Checksum, ')');
      Exit;
    end;
  end;

  Result := True;
end;

procedure HandleClientProtocol(ClientSock: TSocket);  
var
  Command: Byte;
  Data, Response: string;
  KeepRunning: Boolean;
begin
  KeepRunning := True;

  while KeepRunning do
  begin
    if not ReceivePacket(ClientSock, Command, Data) then
    begin
      WriteLn('Erreur de réception du paquet');
      Break;
    end;

    WriteLn('Commande reçue: ', Command);
    if Data <> '' then
      WriteLn('Données: ', Data);

    case Command of
      CMD_ECHO:
        begin
          Response := 'Echo: ' + Data;
          SendPacket(ClientSock, RESP_OK, Response);
        end;

      CMD_TIME:
        begin
          Response := DateTimeToStr(Now);
          SendPacket(ClientSock, RESP_OK, Response);
        end;

      CMD_PING:
        begin
          SendPacket(ClientSock, RESP_OK, 'PONG');
        end;

      CMD_QUIT:
        begin
          SendPacket(ClientSock, RESP_OK, 'Goodbye');
          KeepRunning := False;
        end;

    else
      SendPacket(ClientSock, RESP_ERROR, 'Unknown command');
    end;
  end;
end;

// Serveur principal (code similaire aux exemples précédents)
var
  ServerSock, ClientSock: TSocket;
  ClientAddr: TInetSockAddr;
  AddrLen: TSockLen;

begin
  // ... Création et liaison du serveur ...

  WriteLn('Serveur de protocole personnalisé démarré');

  while True do
  begin
    AddrLen := SizeOf(ClientAddr);
    ClientSock := fpAccept(ServerSock, @ClientAddr, @AddrLen);

    if ClientSock >= 0 then
    begin
      WriteLn('Client connecté');
      HandleClientProtocol(ClientSock);
      CloseSocket(ClientSock);
      WriteLn('Client déconnecté');
    end;
  end;
end.
```

### Client pour le protocole personnalisé

```pascal
program CustomProtocolClient;
{$mode objfpc}{$H+}

uses
  SysUtils, Sockets;

// Constantes et fonctions identiques au serveur
// (CMD_*, RESP_*, SendPacket, ReceivePacket)

procedure TestProtocol(Sock: TSocket);  
var
  Command: Byte;
  Response: string;
begin
  // Test PING
  WriteLn('Test PING...');
  if SendPacket(Sock, CMD_PING, '') then
  begin
    if ReceivePacket(Sock, Command, Response) then
      WriteLn('Réponse: ', Response);
  end;

  // Test ECHO
  WriteLn('Test ECHO...');
  if SendPacket(Sock, CMD_ECHO, 'Bonjour serveur!') then
  begin
    if ReceivePacket(Sock, Command, Response) then
      WriteLn('Réponse: ', Response);
  end;

  // Test TIME
  WriteLn('Test TIME...');
  if SendPacket(Sock, CMD_TIME, '') then
  begin
    if ReceivePacket(Sock, Command, Response) then
      WriteLn('Heure du serveur: ', Response);
  end;

  // Déconnexion
  WriteLn('Envoi QUIT...');
  SendPacket(Sock, CMD_QUIT, '');
  ReceivePacket(Sock, Command, Response);
  WriteLn('Réponse: ', Response);
end;

var
  Sock: TSocket;
  Addr: TInetSockAddr;
  HostEntry: THostEntry;

begin
  Sock := fpSocket(AF_INET, SOCK_STREAM, 0);

  if Sock <> INVALID_SOCKET then
  begin
    try
      // Connexion
      GetHostByName('localhost', HostEntry);
      FillChar(Addr, SizeOf(Addr), 0);
      Addr.sin_family := AF_INET;
      Addr.sin_port := htons(8080);
      Addr.sin_addr := HostEntry.Addr;

      if fpConnect(Sock, @Addr, SizeOf(Addr)) >= 0 then
      begin
        WriteLn('Connecté au serveur');
        TestProtocol(Sock);
      end
      else
        WriteLn('Erreur de connexion');

    finally
      CloseSocket(Sock);
    end;
  end;

  ReadLn;
end.
```

## Communication multicast UDP

Le multicast permet d'envoyer des données à un groupe d'hôtes :

```pascal
program MulticastSender;
{$mode objfpc}{$H+}

uses
  SysUtils, Sockets;

const
  MULTICAST_GROUP = '239.255.255.250';  // Groupe multicast
  MULTICAST_PORT = 5353;

procedure SendMulticast(const Message: string);  
var
  Sock: TSocket;
  Addr: TInetSockAddr;
  TTL: Integer;
begin
  Sock := fpSocket(AF_INET, SOCK_DGRAM, 0);

  if Sock <> INVALID_SOCKET then
  begin
    try
      // Configuration du TTL multicast
      TTL := 32;  // Nombre de sauts réseau
      fpSetSockOpt(Sock, IPPROTO_IP, IP_MULTICAST_TTL, @TTL, SizeOf(TTL));

      // Adresse du groupe multicast
      FillChar(Addr, SizeOf(Addr), 0);
      Addr.sin_family := AF_INET;
      Addr.sin_port := htons(MULTICAST_PORT);
      Addr.sin_addr.s_addr := inet_addr(MULTICAST_GROUP);

      // Envoi
      WriteLn('Envoi multicast: ', Message);
      fpSendTo(Sock, @Message[1], Length(Message), 0, @Addr, SizeOf(Addr));

    finally
      CloseSocket(Sock);
    end;
  end;
end;

begin
  SendMulticast('Message multicast de test');
  ReadLn;
end.
```

### Récepteur multicast

```pascal
program MulticastReceiver;
{$mode objfpc}{$H+}

uses
  SysUtils, Sockets;

const
  MULTICAST_GROUP = '239.255.255.250';
  MULTICAST_PORT = 5353;

type
  TIPMReq = packed record
    imr_multiaddr: TInAddr;  // Adresse du groupe
    imr_interface: TInAddr;  // Interface locale
  end;

procedure ReceiveMulticast;  
var
  Sock: TSocket;
  Addr: TInetSockAddr;
  MReq: TIPMReq;
  OptVal: Integer;
  Buffer: array[0..1023] of Char;
  Received: Integer;
  FromAddr: TInetSockAddr;
  AddrLen: TSockLen;
begin
  Sock := fpSocket(AF_INET, SOCK_DGRAM, 0);

  if Sock <> INVALID_SOCKET then
  begin
    try
      // Option SO_REUSEADDR pour partager le port
      OptVal := 1;
      fpSetSockOpt(Sock, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

      // Liaison au port multicast
      FillChar(Addr, SizeOf(Addr), 0);
      Addr.sin_family := AF_INET;
      Addr.sin_port := htons(MULTICAST_PORT);
      Addr.sin_addr.s_addr := INADDR_ANY;

      if fpBind(Sock, @Addr, SizeOf(Addr)) < 0 then
      begin
        WriteLn('Erreur bind: ', SocketError);
        Exit;
      end;

      // Rejoindre le groupe multicast
      MReq.imr_multiaddr.s_addr := inet_addr(MULTICAST_GROUP);
      MReq.imr_interface.s_addr := INADDR_ANY;

      if fpSetSockOpt(Sock, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                      @MReq, SizeOf(MReq)) < 0 then
      begin
        WriteLn('Erreur lors de la jonction au groupe: ', SocketError);
        Exit;
      end;

      WriteLn('En écoute sur le groupe multicast ', MULTICAST_GROUP, ':', MULTICAST_PORT);
      WriteLn('Appuyez sur Ctrl+C pour arrêter');
      WriteLn;

      // Réception
      while True do
      begin
        AddrLen := SizeOf(FromAddr);
        Received := fpRecvFrom(Sock, @Buffer, SizeOf(Buffer) - 1, 0,
                              @FromAddr, @AddrLen);

        if Received > 0 then
        begin
          Buffer[Received] := #0;
          WriteLn('Reçu de ', NetAddrToStr(FromAddr.sin_addr), ': ', Buffer);
        end;
      end;

      // Quitter le groupe (si on sort de la boucle)
      fpSetSockOpt(Sock, IPPROTO_IP, IP_DROP_MEMBERSHIP, @MReq, SizeOf(MReq));

    finally
      CloseSocket(Sock);
    end;
  end;
end;

begin
  ReceiveMulticast;
end.
```

## Raw Sockets (sockets bruts)

Les raw sockets permettent d'accéder directement aux protocoles IP :

```pascal
{$IFDEF LINUX}
program PingICMP;
{$mode objfpc}{$H+}

uses
  SysUtils, Sockets, BaseUnix;

type
  TICMPHeader = packed record
    icmp_type: Byte;      // Type de message ICMP
    icmp_code: Byte;      // Code
    icmp_checksum: Word;  // Somme de contrôle
    icmp_id: Word;        // Identifiant
    icmp_seq: Word;       // Numéro de séquence
  end;

function CalculateICMPChecksum(const Data: array of Byte; Len: Integer): Word;  
var
  Sum: Cardinal;
  i: Integer;
begin
  Sum := 0;

  // Somme par paires de 16 bits
  i := 0;
  while i < Len - 1 do
  begin
    Sum := Sum + (Data[i] shl 8 + Data[i + 1]);
    Inc(i, 2);
  end;

  // Octet restant si longueur impaire
  if i < Len then
    Sum := Sum + (Data[i] shl 8);

  // Ajout des débordements
  while (Sum shr 16) > 0 do
    Sum := (Sum and $FFFF) + (Sum shr 16);

  Result := not Word(Sum);
end;

function SendPing(const Host: string; Timeout: Integer): Boolean;  
var
  Sock: TSocket;
  Addr: TInetSockAddr;
  HostEntry: THostEntry;
  ICMPPacket: array[0..63] of Byte;
  ICMPHeader: ^TICMPHeader;
  Sent, Received: Integer;
  ReplyBuffer: array[0..1023] of Byte;
  StartTime, EndTime: TDateTime;
  TimeDiff: Double;
begin
  Result := False;

  // Création d'un raw socket (nécessite les privilèges root)
  Sock := fpSocket(AF_INET, SOCK_RAW, IPPROTO_ICMP);

  if Sock = INVALID_SOCKET then
  begin
    WriteLn('Erreur: Raw socket (nécessite sudo/root)');
    Exit;
  end;

  try
    // Résolution de l'hôte
    if not GetHostByName(Host, HostEntry) then
    begin
      WriteLn('Erreur: Impossible de résoudre ', Host);
      Exit;
    end;

    // Préparation de l'adresse
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_addr := HostEntry.Addr;

    // Construction du paquet ICMP Echo Request
    FillChar(ICMPPacket, SizeOf(ICMPPacket), 0);
    ICMPHeader := @ICMPPacket[0];

    ICMPHeader^.icmp_type := 8;  // Echo Request
    ICMPHeader^.icmp_code := 0;
    ICMPHeader^.icmp_id := htons(GetProcessID);
    ICMPHeader^.icmp_seq := htons(1);

    // Données du paquet (optionnel)
    FillChar(ICMPPacket[SizeOf(TICMPHeader)],
             SizeOf(ICMPPacket) - SizeOf(TICMPHeader), $AA);

    // Calcul du checksum
    ICMPHeader^.icmp_checksum := 0;
    ICMPHeader^.icmp_checksum :=
      CalculateICMPChecksum(ICMPPacket, SizeOf(ICMPPacket));

    // Envoi
    WriteLn('Envoi ping à ', Host, ' (', NetAddrToStr(Addr.sin_addr), ')');
    StartTime := Now;

    Sent := fpSendTo(Sock, @ICMPPacket, SizeOf(ICMPPacket), 0,
                     @Addr, SizeOf(Addr));

    if Sent < 0 then
    begin
      WriteLn('Erreur d''envoi: ', SocketError);
      Exit;
    end;

    // Configuration du timeout
    SetSocketTimeout(Sock, Timeout);

    // Réception de la réponse
    Received := fpRecv(Sock, @ReplyBuffer, SizeOf(ReplyBuffer), 0);
    EndTime := Now;

    if Received > 0 then
    begin
      TimeDiff := (EndTime - StartTime) * 24 * 3600 * 1000; // en ms
      WriteLn('Réponse de ', Host, ' : temps=', TimeDiff:0:2, ' ms');
      Result := True;
    end
    else
      WriteLn('Timeout ou erreur de réception');

  finally
    CloseSocket(Sock);
  end;
end;

begin
  if ParamCount = 0 then
  begin
    WriteLn('Usage: sudo ./PingICMP <host>');
    WriteLn('Exemple: sudo ./PingICMP google.com');
  end
  else
  begin
    if SendPing(ParamStr(1), 5000) then
      WriteLn('Ping réussi')
    else
      WriteLn('Ping échoué');
  end;

  ReadLn;
end.
{$ENDIF}
```

**Note importante** : Les raw sockets nécessitent des privilèges élevés (root sous Linux, administrateur sous Windows).

## Optimisations de performance

### Utilisation de sendfile (Linux)

`sendfile` permet de transférer des fichiers efficacement sans copie en espace utilisateur :

```pascal
{$IFDEF LINUX}
function SendFileOptimized(Sock: TSocket; const FileName: string): Boolean;  
var
  FileHandle: Integer;
  FileSize, Sent: Int64;
  Offset: Int64;
begin
  Result := False;

  // Ouverture du fichier
  FileHandle := fpOpen(FileName, O_RDONLY);
  if FileHandle < 0 then
  begin
    WriteLn('Erreur d''ouverture du fichier');
    Exit;
  end;

  try
    // Taille du fichier
    FileSize := fpLSeek(FileHandle, 0, SEEK_END);
    fpLSeek(FileHandle, 0, SEEK_SET);

    WriteLn('Envoi de ', FileName, ' (', FileSize, ' octets)...');

    // Transfert avec sendfile (zero-copy)
    Offset := 0;
    Sent := fpSendFile(Sock, FileHandle, @Offset, FileSize);

    if Sent = FileSize then
    begin
      WriteLn('Fichier envoyé avec succès');
      Result := True;
    end
    else
      WriteLn('Erreur d''envoi: ', Sent, '/', FileSize);

  finally
    fpClose(FileHandle);
  end;
end;
{$ENDIF}
```

### Zero-copy avec splice (Linux)

```pascal
{$IFDEF LINUX}
// Transfert de données entre deux sockets sans copie
function SpliceData(SourceSock, DestSock: TSocket; Length: Integer): Integer;  
var
  PipeFD: array[0..1] of Integer;
  Spliced: Integer;
begin
  // Création d'un pipe
  if fppipe(PipeFD) < 0 then
  begin
    WriteLn('Erreur création pipe');
    Exit(-1);
  end;

  try
    // Source -> Pipe
    Spliced := fpSplice(SourceSock, nil, PipeFD[1], nil, Length,
                        SPLICE_F_MOVE or SPLICE_F_MORE);

    if Spliced > 0 then
    begin
      // Pipe -> Destination
      Result := fpSplice(PipeFD[0], nil, DestSock, nil, Spliced,
                        SPLICE_F_MOVE or SPLICE_F_MORE);
    end
    else
      Result := Spliced;

  finally
    fpClose(PipeFD[0]);
    fpClose(PipeFD[1]);
  end;
end;
{$ENDIF}
```

### Utilisation de MSG_MORE

Indique au kernel qu'il y aura plus de données (optimisation TCP) :

```pascal
procedure SendMultiplePackets(Sock: TSocket);  
var
  i: Integer;
  Data: string;
begin
  for i := 1 to 10 do
  begin
    Data := 'Paquet ' + IntToStr(i);

    if i < 10 then
      // Indique qu'il y aura plus de données
      fpSend(Sock, @Data[1], Length(Data), MSG_MORE)
    else
      // Dernier paquet, envoi immédiat
      fpSend(Sock, @Data[1], Length(Data), 0);
  end;
end;
```

## Détection de fermeture de connexion

### Méthode 1 : Lecture avec timeout

```pascal
function IsSocketConnected(Sock: TSocket): Boolean;  
var
  ReadFDs: TFDSet;
  Timeout: TTimeVal;
  Buffer: Byte;
begin
  Result := True;

  fpFD_ZERO(ReadFDs);
  fpFD_SET(Sock, ReadFDs);

  Timeout.tv_sec := 0;
  Timeout.tv_usec := 0;

  if fpSelect(Sock + 1, @ReadFDs, nil, nil, @Timeout) > 0 then
  begin
    // Données disponibles, essayons de lire
    if fpRecv(Sock, @Buffer, 1, MSG_PEEK) <= 0 then
      Result := False;  // Socket fermé
  end;
end;
```

### Méthode 2 : Option SO_ERROR

```pascal
function CheckSocketError(Sock: TSocket): Integer;  
var
  Error: Integer;
  Len: TSockLen;
begin
  Len := SizeOf(Error);
  if fpGetSockOpt(Sock, SOL_SOCKET, SO_ERROR, @Error, @Len) = 0 then
    Result := Error
  else
    Result := -1;
end;
```

## Différences Windows/Linux

### Gestion des includes

```pascal
uses
  Sockets,
  {$IFDEF WINDOWS}
  WinSock2, Windows
  {$ELSE}
  BaseUnix, Unix
  {$ENDIF};
```

### Fermeture de socket

```pascal
procedure CloseSocketPortable(var Sock: TSocket);  
begin
  if Sock <> INVALID_SOCKET then
  begin
    {$IFDEF WINDOWS}
    CloseSocket(Sock);
    {$ELSE}
    fpClose(Sock);
    {$ENDIF}
    Sock := INVALID_SOCKET;
  end;
end;
```

### Codes d'erreur

```pascal
function GetLastSocketError: Integer;  
begin
  {$IFDEF WINDOWS}
  Result := WSAGetLastError;
  {$ELSE}
  Result := fpgeterrno;
  {$ENDIF}
end;

function SocketErrorToString(ErrorCode: Integer): string;  
begin
  {$IFDEF WINDOWS}
  case ErrorCode of
    WSAECONNREFUSED: Result := 'Connexion refusée';
    WSAETIMEDOUT: Result := 'Timeout';
    WSAECONNRESET: Result := 'Connexion réinitialisée';
    WSAEADDRINUSE: Result := 'Adresse déjà utilisée';
    WSAEWOULDBLOCK: Result := 'Opération bloquerait';
    WSAENOTCONN: Result := 'Socket non connecté';
  else
    Result := 'Erreur ' + IntToStr(ErrorCode);
  end;
  {$ELSE}
  case ErrorCode of
    ESysECONNREFUSED: Result := 'Connexion refusée';
    ESysETIMEDOUT: Result := 'Timeout';
    ESysECONNRESET: Result := 'Connexion réinitialisée';
    ESysEADDRINUSE: Result := 'Adresse déjà utilisée';
    ESysEWOULDBLOCK: Result := 'Opération bloquerait';
    ESysENOTCONN: Result := 'Socket non connecté';
    ESysEPIPE: Result := 'Broken pipe';
  else
    Result := 'Erreur ' + IntToStr(ErrorCode);
  end;
  {$ENDIF}
end;
```

### Initialisation Winsock (Windows uniquement)

```pascal
{$IFDEF WINDOWS}
function InitWinsock: Boolean;  
var
  WSAData: TWSAData;
begin
  Result := WSAStartup(MakeWord(2, 2), WSAData) = 0;
  if not Result then
    WriteLn('Erreur initialisation Winsock');
end;

procedure CleanupWinsock;  
begin
  WSACleanup;
end;
{$ENDIF}

// Utilisation dans le programme principal
begin
  {$IFDEF WINDOWS}
  if not InitWinsock then
    Exit;
  try
    // Votre code réseau ici
  finally
    CleanupWinsock;
  end;
  {$ELSE}
  // Pas d'initialisation nécessaire sous Linux
  // Votre code réseau ici
  {$ENDIF}
end.
```

### Timeout portable

```pascal
procedure SetSocketTimeoutPortable(Sock: TSocket; TimeoutMS: Integer);  
var
  {$IFDEF WINDOWS}
  Timeout: DWORD;
  {$ELSE}
  Timeout: TTimeVal;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  Timeout := TimeoutMS;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
  fpSetSockOpt(Sock, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
  {$ELSE}
  Timeout.tv_sec := TimeoutMS div 1000;
  Timeout.tv_usec := (TimeoutMS mod 1000) * 1000;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
  fpSetSockOpt(Sock, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
  {$ENDIF}
end;
```

## Serveur performant avec époll (Linux)

Époll est l'alternative moderne à select pour Linux, bien plus efficace :

```pascal
{$IFDEF LINUX}
program HighPerformanceServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Sockets, BaseUnix, Linux;

const
  MAX_EVENTS = 1024;

type
  PClientData = ^TClientData;
  TClientData = record
    Socket: TSocket;
    Buffer: array[0..4095] of Char;
    BufferPos: Integer;
  end;

function CreateEpollServer(Port: Word): TSocket;  
var
  Sock: TSocket;
  Addr: TInetSockAddr;
  OptVal: Integer;
begin
  Result := INVALID_SOCKET;

  Sock := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Sock = INVALID_SOCKET then
    Exit;

  // Configuration du socket
  OptVal := 1;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

  // Socket non-bloquant
  fpFcntl(Sock, F_SETFL, fpFcntl(Sock, F_GETFL, 0) or O_NONBLOCK);

  // Liaison
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr.s_addr := INADDR_ANY;

  if fpBind(Sock, @Addr, SizeOf(Addr)) < 0 then
  begin
    CloseSocket(Sock);
    Exit;
  end;

  // Écoute
  if fpListen(Sock, 128) < 0 then
  begin
    CloseSocket(Sock);
    Exit;
  end;

  Result := Sock;
  WriteLn('Serveur époll créé sur le port ', Port);
end;

procedure RunEpollServer(ListenSock: TSocket);  
var
  EpollFD: Integer;
  Event: TEpollEvent;
  Events: array[0..MAX_EVENTS - 1] of TEpollEvent;
  NumEvents, i: Integer;
  ClientSock: TSocket;
  ClientAddr: TInetSockAddr;
  AddrLen: TSockLen;
  ClientData: PClientData;
  Received: Integer;
begin
  // Création de l'instance époll
  EpollFD := epoll_create1(0);
  if EpollFD < 0 then
  begin
    WriteLn('Erreur epoll_create1');
    Exit;
  end;

  try
    // Ajout du socket d'écoute à époll
    FillChar(Event, SizeOf(Event), 0);
    Event.Events := EPOLLIN or EPOLLET;  // Edge-triggered
    Event.Data.fd := ListenSock;

    if epoll_ctl(EpollFD, EPOLL_CTL_ADD, ListenSock, @Event) < 0 then
    begin
      WriteLn('Erreur epoll_ctl ADD');
      Exit;
    end;

    WriteLn('Serveur époll en écoute...');
    WriteLn('Appuyez sur Ctrl+C pour arrêter');
    WriteLn;

    // Boucle principale
    while True do
    begin
      // Attente d'événements
      NumEvents := epoll_wait(EpollFD, @Events[0], MAX_EVENTS, -1);

      if NumEvents < 0 then
      begin
        if fpgeterrno = ESysEINTR then
          Continue;  // Interruption par signal
        WriteLn('Erreur epoll_wait: ', fpgeterrno);
        Break;
      end;

      // Traitement des événements
      for i := 0 to NumEvents - 1 do
      begin
        // Nouvelle connexion ?
        if Events[i].Data.fd = ListenSock then
        begin
          while True do
          begin
            AddrLen := SizeOf(ClientAddr);
            ClientSock := fpAccept(ListenSock, @ClientAddr, @AddrLen);

            if ClientSock < 0 then
            begin
              if (fpgeterrno = ESysEAGAIN) or (fpgeterrno = ESysEWOULDBLOCK) then
                Break;  // Plus de connexions en attente
              WriteLn('Erreur accept: ', fpgeterrno);
              Break;
            end;

            // Configuration du socket client en non-bloquant
            fpFcntl(ClientSock, F_SETFL,
                    fpFcntl(ClientSock, F_GETFL, 0) or O_NONBLOCK);

            // Allocation des données client
            New(ClientData);
            ClientData^.Socket := ClientSock;
            ClientData^.BufferPos := 0;

            // Ajout à époll
            FillChar(Event, SizeOf(Event), 0);
            Event.Events := EPOLLIN or EPOLLET;  // Edge-triggered
            Event.Data.ptr := ClientData;

            if epoll_ctl(EpollFD, EPOLL_CTL_ADD, ClientSock, @Event) < 0 then
            begin
              WriteLn('Erreur epoll_ctl ADD client');
              Dispose(ClientData);
              CloseSocket(ClientSock);
            end
            else
              WriteLn('Nouveau client: ', NetAddrToStr(ClientAddr.sin_addr));
          end;
        end
        else
        begin
          // Données d'un client
          ClientData := PClientData(Events[i].Data.ptr);

          // Lecture des données
          while True do
          begin
            Received := fpRecv(ClientData^.Socket,
                              @ClientData^.Buffer[ClientData^.BufferPos],
                              SizeOf(ClientData^.Buffer) - ClientData^.BufferPos,
                              0);

            if Received < 0 then
            begin
              if (fpgeterrno = ESysEAGAIN) or (fpgeterrno = ESysEWOULDBLOCK) then
                Break;  // Plus de données disponibles

              // Erreur ou déconnexion
              WriteLn('Client déconnecté (erreur)');
              epoll_ctl(EpollFD, EPOLL_CTL_DEL, ClientData^.Socket, nil);
              CloseSocket(ClientData^.Socket);
              Dispose(ClientData);
              Break;
            end
            else if Received = 0 then
            begin
              // Connexion fermée proprement
              WriteLn('Client déconnecté');
              epoll_ctl(EpollFD, EPOLL_CTL_DEL, ClientData^.Socket, nil);
              CloseSocket(ClientData^.Socket);
              Dispose(ClientData);
              Break;
            end
            else
            begin
              // Données reçues - Echo
              fpSend(ClientData^.Socket, @ClientData^.Buffer[ClientData^.BufferPos],
                    Received, 0);
            end;
          end;
        end;
      end;
    end;

  finally
    fpClose(EpollFD);
  end;
end;

var
  ListenSock: TSocket;

begin
  ListenSock := CreateEpollServer(8080);

  if ListenSock <> INVALID_SOCKET then
  begin
    try
      RunEpollServer(ListenSock);
    finally
      CloseSocket(ListenSock);
    end;
  end;
end.
{$ENDIF}
```

## Serveur IOCP (Windows)

IOCP (Input/Output Completion Ports) est le mécanisme haute performance de Windows :

```pascal
{$IFDEF WINDOWS}
program IOCPServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Windows, WinSock2;

type
  PPerIOData = ^TPerIOData;
  TPerIOData = record
    Overlapped: TOverlapped;
    WSABuf: TWSABUF;
    Buffer: array[0..4095] of Char;
    BytesRecv: DWORD;
    BytesSend: DWORD;
  end;

  PPerHandleData = ^TPerHandleData;
  TPerHandleData = record
    Socket: TSocket;
  end;

function CreateIOCPServer(Port: Word): TSocket;  
var
  Sock: TSocket;
  Addr: TSockAddrIn;
  OptVal: Integer;
begin
  Result := INVALID_SOCKET;

  Sock := socket(AF_INET, SOCK_STREAM, 0);
  if Sock = INVALID_SOCKET then
    Exit;

  OptVal := 1;
  setsockopt(Sock, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr.s_addr := INADDR_ANY;

  if bind(Sock, @Addr, SizeOf(Addr)) = SOCKET_ERROR then
  begin
    closesocket(Sock);
    Exit;
  end;

  if listen(Sock, SOMAXCONN) = SOCKET_ERROR then
  begin
    closesocket(Sock);
    Exit;
  end;

  Result := Sock;
  WriteLn('Serveur IOCP créé sur le port ', Port);
end;

procedure WorkerThread(CompletionPort: THandle); stdcall;  
var
  BytesTransferred: DWORD;
  PerHandleData: PPerHandleData;
  PerIOData: PPerIOData;
  Flags: DWORD;
begin
  while True do
  begin
    // Attente d'une opération I/O complétée
    if not GetQueuedCompletionStatus(CompletionPort, BytesTransferred,
                                     ULONG_PTR(PerHandleData),
                                     POverlapped(PerIOData), INFINITE) then
    begin
      WriteLn('Erreur GetQueuedCompletionStatus');
      Continue;
    end;

    // Traitement de la requête
    if BytesTransferred = 0 then
    begin
      // Connexion fermée
      WriteLn('Client déconnecté');
      closesocket(PerHandleData^.Socket);
      Dispose(PerHandleData);
      Dispose(PerIOData);
      Continue;
    end;

    // Echo des données reçues
    PerIOData^.BytesRecv := BytesTransferred;
    PerIOData^.BytesSend := 0;
    PerIOData^.WSABuf.buf := @PerIOData^.Buffer[0];
    PerIOData^.WSABuf.len := PerIOData^.BytesRecv;

    Flags := 0;
    WSASend(PerHandleData^.Socket, @PerIOData^.WSABuf, 1,
            PerIOData^.BytesSend, Flags, @PerIOData^.Overlapped, nil);

    // Préparation de la prochaine réception
    New(PerIOData);
    FillChar(PerIOData^, SizeOf(TPerIOData), 0);
    PerIOData^.WSABuf.buf := @PerIOData^.Buffer[0];
    PerIOData^.WSABuf.len := SizeOf(PerIOData^.Buffer);

    Flags := 0;
    WSARecv(PerHandleData^.Socket, @PerIOData^.WSABuf, 1,
            PerIOData^.BytesRecv, Flags, @PerIOData^.Overlapped, nil);
  end;
end;

procedure RunIOCPServer(ListenSock: TSocket);  
var
  CompletionPort: THandle;
  i: Integer;
  ClientSock: TSocket;
  ClientAddr: TSockAddrIn;
  AddrLen: Integer;
  PerHandleData: PPerHandleData;
  PerIOData: PPerIOData;
  Flags: DWORD;
  ThreadID: DWORD;
begin
  // Création du completion port
  CompletionPort := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  if CompletionPort = 0 then
  begin
    WriteLn('Erreur CreateIoCompletionPort');
    Exit;
  end;

  try
    // Création des threads workers
    for i := 0 to 3 do  // 4 threads
      CloseHandle(CreateThread(nil, 0, @WorkerThread, CompletionPort, 0, ThreadID));

    WriteLn('Serveur IOCP démarré avec 4 threads workers');
    WriteLn('En attente de connexions...');

    // Acceptation des connexions
    while True do
    begin
      AddrLen := SizeOf(ClientAddr);
      ClientSock := accept(ListenSock, @ClientAddr, @AddrLen);

      if ClientSock = INVALID_SOCKET then
      begin
        WriteLn('Erreur accept: ', WSAGetLastError);
        Continue;
      end;

      WriteLn('Nouveau client connecté');

      // Association du socket au completion port
      New(PerHandleData);
      PerHandleData^.Socket := ClientSock;

      if CreateIoCompletionPort(ClientSock, CompletionPort,
                               ULONG_PTR(PerHandleData), 0) = 0 then
      begin
        WriteLn('Erreur CreateIoCompletionPort pour client');
        closesocket(ClientSock);
        Dispose(PerHandleData);
        Continue;
      end;

      // Première opération de lecture asynchrone
      New(PerIOData);
      FillChar(PerIOData^, SizeOf(TPerIOData), 0);
      PerIOData^.WSABuf.buf := @PerIOData^.Buffer[0];
      PerIOData^.WSABuf.len := SizeOf(PerIOData^.Buffer);

      Flags := 0;
      if WSARecv(ClientSock, @PerIOData^.WSABuf, 1, PerIOData^.BytesRecv,
                 Flags, @PerIOData^.Overlapped, nil) = SOCKET_ERROR then
      begin
        if WSAGetLastError <> WSA_IO_PENDING then
        begin
          WriteLn('Erreur WSARecv: ', WSAGetLastError);
          closesocket(ClientSock);
          Dispose(PerHandleData);
          Dispose(PerIOData);
        end;
      end;
    end;

  finally
    CloseHandle(CompletionPort);
  end;
end;

var
  WSAData: TWSAData;
  ListenSock: TSocket;

begin
  // Initialisation Winsock
  if WSAStartup(MakeWord(2, 2), WSAData) <> 0 then
  begin
    WriteLn('Erreur WSAStartup');
    Exit;
  end;

  try
    ListenSock := CreateIOCPServer(8080);

    if ListenSock <> INVALID_SOCKET then
    begin
      try
        RunIOCPServer(ListenSock);
      finally
        closesocket(ListenSock);
      end;
    end;

  finally
    WSACleanup;
  end;
end.
{$ENDIF}
```

## Bonnes pratiques réseau bas niveau

### 1. Toujours vérifier les valeurs de retour

```pascal
Result := fpSend(Sock, @Data[1], Length(Data), 0);  
if Result < 0 then
  // Gérer l'erreur
else if Result < Length(Data) then
  // Envoi partiel, continuer
```

### 2. Gérer les envois partiels

```pascal
function SendAll(Sock: TSocket; const Data: string): Boolean;  
var
  TotalSent, Sent: Integer;
  Ptr: PChar;
begin
  Result := False;
  TotalSent := 0;
  Ptr := @Data[1];

  while TotalSent < Length(Data) do
  begin
    Sent := fpSend(Sock, Ptr, Length(Data) - TotalSent, 0);

    if Sent <= 0 then
      Exit;

    Inc(TotalSent, Sent);
    Inc(Ptr, Sent);
  end;

  Result := True;
end;
```

### 3. Utiliser des buffers appropriés

```pascal
const
  RECV_BUFFER_SIZE = 8192;  // 8 KB - bon compromis
  SEND_BUFFER_SIZE = 16384; // 16 KB
```

### 4. Définir des timeouts

```pascal
// Toujours définir des timeouts pour éviter les blocages
SetSocketTimeoutPortable(Sock, 30000); // 30 secondes
```

### 5. Nettoyer les ressources

```pascal
procedure CleanupSocket(var Sock: TSocket);  
begin
  if Sock <> INVALID_SOCKET then
  begin
    // Désactiver les envois (shutdown)
    {$IFDEF WINDOWS}
    shutdown(Sock, SD_BOTH);
    {$ELSE}
    fpShutdown(Sock, SHUT_RDWR);
    {$ENDIF}

    // Fermer le socket
    CloseSocketPortable(Sock);
  end;
end;
```

### 6. Valider les données reçues

```pascal
function ValidateReceivedData(const Data: string): Boolean;  
begin
  // Vérifications de sécurité
  Result := (Length(Data) > 0) and (Length(Data) < 1048576); // Max 1 MB

  // Autres validations selon votre protocole
  // - Vérifier les caractères autorisés
  // - Valider la structure des données
  // - Vérifier les checksums
end;
```

### 7. Logger les erreurs réseau

```pascal
procedure LogSocketError(const Context: string; ErrorCode: Integer);  
begin
  WriteLn('[', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), '] ',
          Context, ': ', SocketErrorToString(ErrorCode));
end;
```

## Cas d'usage pratiques

### Transfert de fichiers

```pascal
procedure SendFile(Sock: TSocket; const FileName: string);  
var
  F: File of Byte;
  Buffer: array[0..8191] of Byte;
  BytesRead: Integer;
begin
  Assign(F, FileName);
  Reset(F);
  try
    while not Eof(F) do
    begin
      BlockRead(F, Buffer, SizeOf(Buffer), BytesRead);
      if BytesRead > 0 then
        SendAll(Sock, Copy(Buffer, 0, BytesRead));
    end;
  finally
    Close(F);
  end;
end;
```

### Heartbeat (Keep-Alive applicatif)

```pascal
type
  THeartbeatThread = class(TThread)
  private
    FSock: TSocket;
    FInterval: Integer; // en secondes
  protected
    procedure Execute; override;
  public
    constructor Create(ASock: TSocket; AInterval: Integer);
  end;

constructor THeartbeatThread.Create(ASock: TSocket; AInterval: Integer);  
begin
  inherited Create(False);
  FSock := ASock;
  FInterval := AInterval;
  FreeOnTerminate := True;
end;

procedure THeartbeatThread.Execute;  
const
  HEARTBEAT = 'PING';
begin
  while not Terminated do
  begin
    if fpSend(FSock, @HEARTBEAT[1], Length(HEARTBEAT), 0) < 0 then
      Break; // Connexion perdue

    Sleep(FInterval * 1000);
  end;
end;
```

## Débogage réseau

### Outils utiles

**Sous Linux :**
```bash
# Voir les connexions actives
netstat -an | grep 8080  
ss -an | grep 8080

# Capturer le trafic réseau
sudo tcpdump -i lo port 8080

# Analyser avec Wireshark
wireshark

# Tester un port
nc -zv localhost 8080  
telnet localhost 8080
```

**Sous Windows :**
```cmd
# Voir les connexions actives
netstat -an | findstr 8080

# Tester un port
telnet localhost 8080  
Test-NetConnection -ComputerName localhost -Port 8080
```

### Traces de débogage

```pascal
{$DEFINE DEBUG_NETWORK}

procedure DebugLog(const Msg: string);  
begin
  {$IFDEF DEBUG_NETWORK}
  WriteLn('[DEBUG] ', FormatDateTime('hh:nn:ss.zzz', Now), ' ', Msg);
  {$ENDIF}
end;

// Utilisation
DebugLog('Connexion établie avec ' + ClientIP);  
DebugLog('Envoi de ' + IntToStr(DataLen) + ' octets');
```

## Conclusion

La programmation réseau bas niveau offre un contrôle total sur les communications mais nécessite une attention particulière aux détails. Voici un résumé des points clés :

### Points essentiels à retenir

1. **TCP vs UDP** : Choisissez selon vos besoins (fiabilité vs performance)
2. **Gestion des erreurs** : Toujours vérifier les valeurs de retour
3. **Timeouts** : Éviter les blocages infinis
4. **Buffers** : Dimensionner correctement pour la performance
5. **Portabilité** : Utiliser des directives de compilation conditionnelle
6. **Sécurité** : Valider toutes les données reçues
7. **Performance** : Utiliser époll/IOCP pour haute performance

### Quand utiliser les sockets bas niveau ?

✅ **Utilisez les sockets bas niveau pour :**
- Protocoles personnalisés nécessitant un contrôle total
- Applications haute performance (jeux, trading)
- Apprentissage approfondi des réseaux
- Optimisations spécifiques

❌ **Préférez des bibliothèques de haut niveau pour :**
- Protocoles standards (HTTP, FTP, SMTP)
- Développement rapide d'applications
- Projets nécessitant SSL/TLS
- Applications d'entreprise classiques

### Progression de l'apprentissage

1. **Niveau débutant** : Utilisez Synapse ou Indy
2. **Niveau intermédiaire** : Comprenez les concepts TCP/UDP
3. **Niveau avancé** : Maîtrisez les sockets bas niveau
4. **Niveau expert** : Optimisez avec époll/IOCP

Les sockets bas niveau sont un outil puissant dans votre boîte à outils de développeur réseau. Ils vous permettent de comprendre réellement ce qui se passe "sous le capot" et d'optimiser vos applications réseau au maximum de leurs capacités.

⏭️ [Serveurs HTTP/HTTPS personnalisés](/10-programmation-reseau-avancee/04-serveurs-http-https-personnalises.md)
