🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.11 P2P et protocoles décentralisés

## Introduction au Peer-to-Peer (P2P)

### Qu'est-ce que le P2P ?

Le P2P (Peer-to-Peer ou pair-à-pair) est une architecture réseau où chaque participant (appelé "peer" ou "pair") agit à la fois comme client et serveur. Il n'y a pas de serveur central unique.

**Analogie simple :** Imaginez un groupe d'amis qui s'échangent des livres directement entre eux, sans passer par une bibliothèque centrale. Chacun peut emprunter et prêter des livres aux autres. C'est ça le P2P !

### Architecture Client-Serveur vs P2P

**Client-Serveur (traditionnel) :**
```
    Client 1 ────┐
    Client 2 ────┼──→ SERVEUR CENTRAL
    Client 3 ────┘
```

**P2P (décentralisé) :**
```
    Peer 1 ←──→ Peer 2
       ↕           ↕
    Peer 3 ←──→ Peer 4
```

### Avantages du P2P

✅ **Pas de point unique de défaillance** - Si un pair tombe, les autres continuent  
✅ **Scalabilité naturelle** - Plus il y a de pairs, plus le réseau est puissant  
✅ **Économie de bande passante** - Distribution de la charge  
✅ **Résistance à la censure** - Difficile de bloquer un réseau décentralisé  
✅ **Coûts réduits** - Pas besoin d'infrastructure serveur massive

### Inconvénients du P2P

❌ **Complexité accrue** - Plus difficile à programmer  
❌ **Sécurité délicate** - Chaque pair doit être sécurisé  
❌ **Découverte de pairs** - Comment trouver les autres pairs ?  
❌ **Données inconsistantes** - Synchronisation plus difficile  
❌ **NAT et firewalls** - Problèmes de connectivité

### Exemples d'applications P2P

**Bien connus :**
- **BitTorrent** - Partage de fichiers
- **Bitcoin** - Cryptomonnaie
- **IPFS** - Système de fichiers distribué
- **WebRTC** - Communication vidéo directe
- **Skype** (anciennement) - VoIP P2P

**Moins connus :**
- **Freenet** - Réseau anonyme
- **RetroShare** - Communication sécurisée
- **Tox** - Messagerie instantanée
- **GNUnet** - Framework P2P

## Types de réseaux P2P

### 1. P2P pur (non structuré)

Tous les pairs sont égaux, pas de hiérarchie.

**Exemple : Gnutella (ancien)**
```
Pair A ←→ Pair B ←→ Pair C
  ↕        ↕         ↕
Pair D ←→ Pair E ←→ Pair F
```

**Avantages :**
- Très simple
- Très résilient
- Totalement décentralisé

**Inconvénients :**
- Recherche inefficace (flood du réseau)
- Difficile de trouver du contenu rare

### 2. P2P hybride

Utilise des serveurs pour certaines fonctions (découverte, index).

**Exemple : BitTorrent avec trackers**
```
        Tracker (index)
           ↓  ↓  ↓
Pair A ←→ Pair B ←→ Pair C
```

**Avantages :**
- Plus efficace pour la découverte
- Équilibre entre décentralisation et performance

**Inconvénients :**
- Point de défaillance partiel (le tracker)
- Moins décentralisé

### 3. P2P structuré (DHT)

Utilise une table de hachage distribuée (Distributed Hash Table).

**Exemple : Kademlia (BitTorrent moderne)**
```
ID node: 101010...
  ↓
Table de routage structurée
  ↓
Recherche en O(log n)
```

**Avantages :**
- Recherche très efficace
- Scalable
- Pas de serveur central

**Inconvénients :**
- Plus complexe à implémenter
- Maintenance de la structure

### 4. Super-peers

Certains pairs avec plus de ressources deviennent des "super-peers".

```
  Pair A ──→ Super-peer 1 ←─→ Super-peer 2 ←── Pair D
  Pair B ──┘       ↕              ↕          └── Pair E
  Pair C ──────────┘              └──────────── Pair F
```

**Avantages :**
- Meilleure performance
- Utilisation efficace des ressources

**Inconvénients :**
- Moins égalitaire
- Super-peers peuvent devenir des cibles

## Concepts fondamentaux

### 1. Identification des pairs (Peer ID)

Chaque pair doit avoir un identifiant unique :

```pascal
type
  TPeerID = record
    Value: array[0..19] of Byte;  // 160 bits (SHA-1)
  end;

function GeneratePeerID: TPeerID;
var
  i: Integer;
begin
  Randomize;
  for i := 0 to High(Result.Value) do
    Result.Value[i] := Random(256);
end;

function PeerIDToString(const ID: TPeerID): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(ID.Value) do
    Result := Result + IntToHex(ID.Value[i], 2);
end;
```

### 2. Découverte de pairs

**Méthodes principales :**

**a) Bootstrap nodes (nœuds d'amorçage)**
```pascal
const
  BOOTSTRAP_NODES: array[0..2] of String = (
    'bootstrap1.example.com:6881',
    'bootstrap2.example.com:6881',
    'bootstrap3.example.com:6881'
  );

procedure ConnecterBootstrap;
var
  i: Integer;
begin
  for i := 0 to High(BOOTSTRAP_NODES) do
  begin
    try
      ConnecterPair(BOOTSTRAP_NODES[i]);
      WriteLn('Connecté à ', BOOTSTRAP_NODES[i]);
    except
      on E: Exception do
        WriteLn('Échec connexion à ', BOOTSTRAP_NODES[i]);
    end;
  end;
end;
```

**b) Broadcasting local**
```pascal
procedure BroadcastDecouverte;
var
  Socket: TSocket;
  Broadcast: TSockAddr;
  Message: String;
begin
  Socket := fpSocket(AF_INET, SOCK_DGRAM, 0);
  try
    // Activer le broadcast
    fpSetSockOpt(Socket, SOL_SOCKET, SO_BROADCAST, @True, SizeOf(Boolean));

    // Préparer l'adresse de broadcast
    Broadcast.sin_family := AF_INET;
    Broadcast.sin_port := htons(6881);
    Broadcast.sin_addr.s_addr := INADDR_BROADCAST;

    // Envoyer le message
    Message := 'P2P_DISCOVERY:' + MyPeerID;
    fpSendTo(Socket, @Message[1], Length(Message), 0, @Broadcast, SizeOf(Broadcast));
  finally
    fpClose(Socket);
  end;
end;
```

**c) Peer Exchange (PEX)**
```pascal
procedure DemanderPairs(Peer: TPeer);
var
  Request: String;
  Response: String;
  PeerList: TStringList;
begin
  // Demander la liste de pairs connus
  Request := 'GET_PEERS';
  Response := EnvoyerRequete(Peer, Request);

  // Parser la réponse
  PeerList := TStringList.Create;
  try
    PeerList.Text := Response;
    AjouterPairsConnus(PeerList);
  finally
    PeerList.Free;
  end;
end;
```

### 3. NAT Traversal (traversée de NAT)

Le NAT (Network Address Translation) pose problème en P2P car il bloque les connexions entrantes.

**Solutions :**

**a) STUN (Session Traversal Utilities for NAT)**

Découvrir son adresse IP publique :

```pascal
unit STUNClient;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Sockets;

type
  TSTUNClient = class
  private
    FStunServer: String;
    FStunPort: Word;
  public
    constructor Create(const Server: String = 'stun.l.google.com'; Port: Word = 19302);

    function GetPublicAddress(out IP: String; out Port: Word): Boolean;
  end;

implementation

constructor TSTUNClient.Create(const Server: String; Port: Word);
begin
  FStunServer := Server;
  FStunPort := Port;
end;

function TSTUNClient.GetPublicAddress(out IP: String; out Port: Word): Boolean;
var
  Socket: TSocket;
  Request: array[0..19] of Byte;
  Response: array[0..1023] of Byte;
  BytesReceived: Integer;
begin
  Result := False;

  Socket := fpSocket(AF_INET, SOCK_DGRAM, 0);
  try
    // Construire la requête STUN
    FillChar(Request, SizeOf(Request), 0);
    Request[0] := $00;  // Binding Request
    Request[1] := $01;
    // ... (détails du protocole STUN)

    // Envoyer la requête
    // fpSendTo(Socket, @Request, SizeOf(Request), ...);

    // Recevoir la réponse
    // BytesReceived := fpRecv(Socket, @Response, SizeOf(Response), 0);

    // Parser la réponse pour extraire l'IP et le port public
    // ...

    Result := True;
  finally
    fpClose(Socket);
  end;
end;

end.
```

**b) UPnP (Universal Plug and Play)**

Demander au routeur d'ouvrir un port :

```pascal
procedure OuvrirPortUPnP(Port: Word);
var
  XML: String;
begin
  // Rechercher le routeur UPnP
  // ...

  // Construire la requête SOAP
  XML :=
    '<?xml version="1.0"?>' +
    '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
    '<s:Body>' +
    '<u:AddPortMapping xmlns:u="urn:schemas-upnp-org:service:WANIPConnection:1">' +
    '<NewRemoteHost></NewRemoteHost>' +
    '<NewExternalPort>' + IntToStr(Port) + '</NewExternalPort>' +
    '<NewProtocol>TCP</NewProtocol>' +
    '<NewInternalPort>' + IntToStr(Port) + '</NewInternalPort>' +
    '<NewInternalClient>' + GetLocalIP + '</NewInternalClient>' +
    '<NewEnabled>1</NewEnabled>' +
    '<NewPortMappingDescription>P2P App</NewPortMappingDescription>' +
    '<NewLeaseDuration>0</NewLeaseDuration>' +
    '</u:AddPortMapping>' +
    '</s:Body>' +
    '</s:Envelope>';

  // Envoyer au routeur
  // ...
end;
```

**c) Hole Punching**

Technique pour établir une connexion directe malgré le NAT :

```
Pair A (NAT) ←→ Serveur relais ←→ Pair B (NAT)
      ↓                               ↓
      └──────── Connexion directe ────┘
           (après hole punching)
```

### 4. Protocoles de communication

**Format de message simple :**

```pascal
type
  TMessageType = (
    mtPing,           // Test de connexion
    mtPong,           // Réponse au ping
    mtGetPeers,       // Demande de pairs
    mtPeers,          // Liste de pairs
    mtGetData,        // Demande de données
    mtData,           // Envoi de données
    mtDisconnect      // Déconnexion
  );

  TMessage = packed record
    Version: Byte;
    MessageType: TMessageType;
    Length: Word;
    PeerID: TPeerID;
    Payload: array[0..1023] of Byte;
  end;

function CreerMessage(MsgType: TMessageType; const Payload: String): TMessage;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Version := 1;
  Result.MessageType := MsgType;
  Result.Length := Length(Payload);
  Result.PeerID := MonPeerID;

  if Length(Payload) > 0 then
    Move(Payload[1], Result.Payload[0], Min(Length(Payload), 1024));
end;
```

## Implémentation d'un réseau P2P simple

### Architecture de base

```pascal
unit SimpleP2P;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets, SyncObjs, Contnrs;

type
  TPeer = class;
  TP2PNetwork = class;

  // Événements
  TPeerConnectedEvent = procedure(Peer: TPeer) of object;
  TPeerDisconnectedEvent = procedure(Peer: TPeer) of object;
  TMessageReceivedEvent = procedure(Peer: TPeer; const Data: String) of object;

  // Représente un pair distant
  TPeer = class
  private
    FSocket: TSocket;
    FAddress: String;
    FPort: Word;
    FConnected: Boolean;
    FLastSeen: TDateTime;
  public
    constructor Create(Socket: TSocket; const Address: String; Port: Word);
    destructor Destroy; override;

    procedure Send(const Data: String);
    procedure Disconnect;

    property Address: String read FAddress;
    property Port: Word read FPort;
    property Connected: Boolean read FConnected;
    property LastSeen: TDateTime read FLastSeen write FLastSeen;
  end;

  // Réseau P2P
  TP2PNetwork = class
  private
    FListenPort: Word;
    FListenSocket: TSocket;
    FPeers: TObjectList;
    FLock: TCriticalSection;
    FRunning: Boolean;
    FListenThread: TThread;

    FOnPeerConnected: TPeerConnectedEvent;
    FOnPeerDisconnected: TPeerDisconnectedEvent;
    FOnMessageReceived: TMessageReceivedEvent;

    procedure ListenProc;
    procedure HandlePeer(Peer: TPeer);
  public
    constructor Create(ListenPort: Word = 6881);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    function ConnectToPeer(const Address: String; Port: Word): TPeer;
    procedure Broadcast(const Data: String);
    function GetPeerCount: Integer;

    property OnPeerConnected: TPeerConnectedEvent read FOnPeerConnected write FOnPeerConnected;
    property OnPeerDisconnected: TPeerDisconnectedEvent read FOnPeerDisconnected write FOnPeerDisconnected;
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
  end;

implementation

{ TPeer }

constructor TPeer.Create(Socket: TSocket; const Address: String; Port: Word);
begin
  inherited Create;
  FSocket := Socket;
  FAddress := Address;
  FPort := Port;
  FConnected := True;
  FLastSeen := Now;
end;

destructor TPeer.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TPeer.Send(const Data: String);
var
  BytesSent: Integer;
begin
  if not FConnected then
    Exit;

  BytesSent := fpSend(FSocket, @Data[1], Length(Data), 0);
  if BytesSent <= 0 then
  begin
    FConnected := False;
    raise Exception.Create('Erreur lors de l''envoi');
  end;
end;

procedure TPeer.Disconnect;
begin
  if FConnected then
  begin
    fpShutdown(FSocket, 2);
    fpClose(FSocket);
    FConnected := False;
  end;
end;

{ TP2PNetwork }

constructor TP2PNetwork.Create(ListenPort: Word);
begin
  inherited Create;
  FListenPort := ListenPort;
  FPeers := TObjectList.Create(True);
  FLock := TCriticalSection.Create;
  FRunning := False;
end;

destructor TP2PNetwork.Destroy;
begin
  Stop;
  FPeers.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TP2PNetwork.Start;
var
  Addr: TSockAddr;
begin
  if FRunning then
    Exit;

  // Créer le socket d'écoute
  FListenSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if FListenSocket < 0 then
    raise Exception.Create('Impossible de créer le socket');

  // Configurer pour réutiliser l'adresse
  fpSetSockOpt(FListenSocket, SOL_SOCKET, SO_REUSEADDR, @True, SizeOf(Boolean));

  // Bind
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(FListenPort);
  Addr.sin_addr.s_addr := INADDR_ANY;

  if fpBind(FListenSocket, @Addr, SizeOf(Addr)) < 0 then
    raise Exception.Create('Impossible de bind le port');

  // Listen
  if fpListen(FListenSocket, 10) < 0 then
    raise Exception.Create('Impossible d''écouter');

  FRunning := True;

  // Lancer le thread d'écoute
  FListenThread := TThread.CreateAnonymousThread(@ListenProc);
  FListenThread.Start;

  WriteLn('Réseau P2P démarré sur le port ', FListenPort);
end;

procedure TP2PNetwork.Stop;
begin
  if not FRunning then
    Exit;

  FRunning := False;

  // Fermer le socket d'écoute
  fpShutdown(FListenSocket, 2);
  fpClose(FListenSocket);

  // Attendre la fin du thread
  if Assigned(FListenThread) then
  begin
    FListenThread.Terminate;
    FListenThread.WaitFor;
    FListenThread.Free;
  end;

  // Déconnecter tous les pairs
  FLock.Enter;
  try
    FPeers.Clear;
  finally
    FLock.Leave;
  end;

  WriteLn('Réseau P2P arrêté');
end;

procedure TP2PNetwork.ListenProc;
var
  ClientSocket: TSocket;
  ClientAddr: TSockAddr;
  AddrLen: TSockLen;
  Peer: TPeer;
  PeerThread: TThread;
begin
  while FRunning do
  begin
    AddrLen := SizeOf(ClientAddr);
    ClientSocket := fpAccept(FListenSocket, @ClientAddr, @AddrLen);

    if ClientSocket < 0 then
      Continue;

    // Créer un objet Peer
    Peer := TPeer.Create(ClientSocket, NetAddrToStr(ClientAddr.sin_addr),
                         ntohs(ClientAddr.sin_port));

    FLock.Enter;
    try
      FPeers.Add(Peer);
    finally
      FLock.Leave;
    end;

    // Notifier
    if Assigned(FOnPeerConnected) then
      FOnPeerConnected(Peer);

    // Gérer ce pair dans un thread séparé
    PeerThread := TThread.CreateAnonymousThread(procedure begin HandlePeer(Peer); end);
    PeerThread.FreeOnTerminate := True;
    PeerThread.Start;
  end;
end;

procedure TP2PNetwork.HandlePeer(Peer: TPeer);
var
  Buffer: array[0..4095] of Byte;
  BytesReceived: Integer;
  Data: String;
begin
  try
    while Peer.Connected and FRunning do
    begin
      BytesReceived := fpRecv(Peer.FSocket, @Buffer, SizeOf(Buffer), 0);

      if BytesReceived <= 0 then
        Break;

      SetString(Data, PAnsiChar(@Buffer), BytesReceived);
      Peer.LastSeen := Now;

      // Notifier de la réception
      if Assigned(FOnMessageReceived) then
        FOnMessageReceived(Peer, Data);
    end;
  finally
    // Déconnexion
    Peer.Disconnect;

    FLock.Enter;
    try
      FPeers.Remove(Peer);
    finally
      FLock.Leave;
    end;

    if Assigned(FOnPeerDisconnected) then
      FOnPeerDisconnected(Peer);
  end;
end;

function TP2PNetwork.ConnectToPeer(const Address: String; Port: Word): TPeer;
var
  Socket: TSocket;
  Addr: TSockAddr;
  PeerThread: TThread;
begin
  Socket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Socket < 0 then
    raise Exception.Create('Impossible de créer le socket');

  // Préparer l'adresse
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr := StrToNetAddr(Address);

  // Connecter
  if fpConnect(Socket, @Addr, SizeOf(Addr)) < 0 then
  begin
    fpClose(Socket);
    raise Exception.CreateFmt('Impossible de se connecter à %s:%d', [Address, Port]);
  end;

  // Créer l'objet Peer
  Result := TPeer.Create(Socket, Address, Port);

  FLock.Enter;
  try
    FPeers.Add(Result);
  finally
    FLock.Leave;
  end;

  // Notifier
  if Assigned(FOnPeerConnected) then
    FOnPeerConnected(Result);

  // Gérer ce pair dans un thread
  PeerThread := TThread.CreateAnonymousThread(procedure begin HandlePeer(Result); end);
  PeerThread.FreeOnTerminate := True;
  PeerThread.Start;
end;

procedure TP2PNetwork.Broadcast(const Data: String);
var
  i: Integer;
  Peer: TPeer;
begin
  FLock.Enter;
  try
    for i := 0 to FPeers.Count - 1 do
    begin
      Peer := TPeer(FPeers[i]);
      try
        Peer.Send(Data);
      except
        // Ignorer les erreurs d'envoi
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TP2PNetwork.GetPeerCount: Integer;
begin
  FLock.Enter;
  try
    Result := FPeers.Count;
  finally
    FLock.Leave;
  end;
end;

end.
```

### Exemple d'utilisation

```pascal
program TestP2P;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  SimpleP2P;

var
  Network: TP2PNetwork;
  Command: String;
  Parts: TStringList;

procedure OnPeerConnected(Peer: TPeer);
begin
  WriteLn('✓ Pair connecté : ', Peer.Address, ':', Peer.Port);
end;

procedure OnPeerDisconnected(Peer: TPeer);
begin
  WriteLn('✗ Pair déconnecté : ', Peer.Address, ':', Peer.Port);
end;

procedure OnMessageReceived(Peer: TPeer; const Data: String);
begin
  WriteLn('[', Peer.Address, '] ', Data);
end;

begin
  WriteLn('=== Réseau P2P Simple ===');
  WriteLn;

  Network := TP2PNetwork.Create(6881);
  try
    // Configurer les callbacks
    Network.OnPeerConnected := @OnPeerConnected;
    Network.OnPeerDisconnected := @OnPeerDisconnected;
    Network.OnMessageReceived := @OnMessageReceived;

    // Démarrer
    Network.Start;

    WriteLn('Commandes disponibles :');
    WriteLn('  connect <ip> <port>  - Se connecter à un pair');
    WriteLn('  send <message>       - Envoyer un message à tous');
    WriteLn('  peers                - Afficher les pairs connectés');
    WriteLn('  quit                 - Quitter');
    WriteLn;

    repeat
      Write('> ');
      ReadLn(Command);

      if Command = '' then
        Continue;

      Parts := TStringList.Create;
      try
        Parts.Delimiter := ' ';
        Parts.StrictDelimiter := True;
        Parts.DelimitedText := Command;

        if LowerCase(Parts[0]) = 'connect' then
        begin
          if Parts.Count >= 3 then
          begin
            try
              Network.ConnectToPeer(Parts[1], StrToInt(Parts[2]));
              WriteLn('Connexion en cours...');
            except
              on E: Exception do
                WriteLn('Erreur : ', E.Message);
            end;
          end
          else
            WriteLn('Usage : connect <ip> <port>');
        end
        else if LowerCase(Parts[0]) = 'send' then
        begin
          if Parts.Count >= 2 then
          begin
            Delete(Command, 1, 5);  // Enlever "send "
            Network.Broadcast(Command);
            WriteLn('Message envoyé');
          end
          else
            WriteLn('Usage : send <message>');
        end
        else if LowerCase(Parts[0]) = 'peers' then
          WriteLn('Pairs connectés : ', Network.GetPeerCount)
        else if LowerCase(Parts[0]) = 'quit' then
          Break
        else
          WriteLn('Commande inconnue');
      finally
        Parts.Free;
      end;

    until False;

    Network.Stop;

  finally
    Network.Free;
  end;
end.
```

## Protocole BitTorrent simplifié

BitTorrent est l'un des protocoles P2P les plus populaires. Voici une version simplifiée pour comprendre les concepts.

### Concepts BitTorrent

**1. Torrent file (.torrent)**
- Contient les métadonnées du fichier
- Hash SHA-1 de chaque morceau
- URL du tracker
- Taille des morceaux

**2. Tracker**
- Serveur central qui liste les pairs
- Optionnel (DHT peut le remplacer)

**3. Pieces (morceaux)**
- Le fichier est divisé en morceaux
- Taille typique : 256 KB ou 512 KB
- Chaque morceau a un hash pour validation

**4. Peers**
- **Leechers** : Téléchargent (pas encore complet)
- **Seeders** : Partagent (fichier complet)

### Structure d'un fichier .torrent

```pascal
type
  TTorrentFile = record
    Announce: String;        // URL du tracker
    Name: String;            // Nom du fichier
    Length: Int64;           // Taille totale
    PieceLength: Integer;    // Taille d'un morceau
    Pieces: TBytes;          // Hashes SHA-1 des morceaux (20 bytes chacun)
  end;

function ParseTorrentFile(const Filename: String): TTorrentFile;
// Parsing du format Bencode (format BitTorrent)
// Format: d8:announce...e
begin
  // Implémentation du parser Bencode
  // ...
end;
```

### Protocole de communication BitTorrent

**Messages principaux :**

```pascal
type
  TBTMessageType = (
    btChoke = 0,          // Bloquer le pair
    btUnchoke = 1,        // Débloquer le pair
    btInterested = 2,     // Intéressé par des morceaux
    btNotInterested = 3,  // Plus intéressé
    btHave = 4,           // Annonce possession d'un morceau
    btBitfield = 5,       // Envoie le bitmap complet
    btRequest = 6,        // Demande un morceau
    btPiece = 7,          // Envoi d'un morceau
    btCancel = 8          // Annule une demande
  );

  TBTMessage = packed record
    Length: LongWord;          // Taille du message
    MessageType: Byte;         // Type de message
    Payload: array of Byte;    // Données
  end;

function CreateBTMessage(MsgType: TBTMessageType; const Payload: TBytes): TBTMessage;
begin
  Result.Length := htonl(1 + Length(Payload));
  Result.MessageType := Ord(MsgType);
  Result.Payload := Payload;
end;
```

### Bitmap des morceaux

```pascal
type
  TBitfield = class
  private
    FBits: TBytes;
    FPieceCount: Integer;
  public
    constructor Create(PieceCount: Integer);

    procedure SetPiece(Index: Integer; Value: Boolean);
    function HasPiece(Index: Integer): Boolean;
    function GetCompletionPercent: Double;
    function GetMissingPieces: TIntegerDynArray;

    function ToBytes: TBytes;
    procedure FromBytes(const Data: TBytes);
  end;

implementation

constructor TBitfield.Create(PieceCount: Integer);
var
  ByteCount: Integer;
begin
  inherited Create;
  FPieceCount := PieceCount;

  // Calculer le nombre d'octets nécessaires
  ByteCount := (PieceCount + 7) div 8;
  SetLength(FBits, ByteCount);
  FillChar(FBits[0], ByteCount, 0);
end;

procedure TBitfield.SetPiece(Index: Integer; Value: Boolean);
var
  ByteIndex, BitIndex: Integer;
begin
  if (Index < 0) or (Index >= FPieceCount) then
    raise Exception.Create('Index de morceau invalide');

  ByteIndex := Index div 8;
  BitIndex := Index mod 8;

  if Value then
    FBits[ByteIndex] := FBits[ByteIndex] or (1 shl (7 - BitIndex))
  else
    FBits[ByteIndex] := FBits[ByteIndex] and not (1 shl (7 - BitIndex));
end;

function TBitfield.HasPiece(Index: Integer): Boolean;
var
  ByteIndex, BitIndex: Integer;
begin
  if (Index < 0) or (Index >= FPieceCount) then
    Exit(False);

  ByteIndex := Index div 8;
  BitIndex := Index mod 8;

  Result := (FBits[ByteIndex] and (1 shl (7 - BitIndex))) <> 0;
end;

function TBitfield.GetCompletionPercent: Double;
var
  i, Count: Integer;
begin
  Count := 0;
  for i := 0 to FPieceCount - 1 do
    if HasPiece(i) then
      Inc(Count);

  Result := (Count * 100.0) / FPieceCount;
end;

function TBitfield.GetMissingPieces: TIntegerDynArray;
var
  i, Count: Integer;
begin
  Count := 0;
  SetLength(Result, FPieceCount);

  for i := 0 to FPieceCount - 1 do
  begin
    if not HasPiece(i) then
    begin
      Result[Count] := i;
      Inc(Count);
    end;
  end;

  SetLength(Result, Count);
end;

function TBitfield.ToBytes: TBytes;
begin
  Result := Copy(FBits, 0, Length(FBits));
end;

procedure TBitfield.FromBytes(const Data: TBytes);
begin
  FBits := Copy(Data, 0, Length(Data));
end;

end.
```

### Stratégie de téléchargement

**1. Rarest First (le plus rare d'abord)**

Télécharger d'abord les morceaux les plus rares pour maximiser leur disponibilité :

```pascal
function TrouverMorceauPlusRare(const PeerBitfields: array of TBitfield;
                                const LocalBitfield: TBitfield): Integer;
var
  i, j, MinCount, Count: Integer;
begin
  Result := -1;
  MinCount := MaxInt;

  // Pour chaque morceau manquant
  for i := 0 to LocalBitfield.FPieceCount - 1 do
  begin
    if LocalBitfield.HasPiece(i) then
      Continue;

    // Compter combien de pairs l'ont
    Count := 0;
    for j := 0 to High(PeerBitfields) do
      if PeerBitfields[j].HasPiece(i) then
        Inc(Count);

    // Garder le plus rare
    if (Count > 0) and (Count < MinCount) then
    begin
      MinCount := Count;
      Result := i;
    end;
  end;
end;
```

**2. Random First (aléatoire au début)**

Au début, télécharger des morceaux aléatoires pour avoir quelque chose à partager rapidement :

```pascal
function ChoisirMorceauAleatoire(const PeerBitfield, LocalBitfield: TBitfield): Integer;
var
  MissingPieces: TIntegerDynArray;
  i: Integer;
  Available: TIntegerDynArray;
  Count: Integer;
begin
  // Obtenir les morceaux manquants
  MissingPieces := LocalBitfield.GetMissingPieces;

  // Filtrer ceux disponibles chez ce pair
  Count := 0;
  SetLength(Available, Length(MissingPieces));

  for i := 0 to High(MissingPieces) do
  begin
    if PeerBitfield.HasPiece(MissingPieces[i]) then
    begin
      Available[Count] := MissingPieces[i];
      Inc(Count);
    end;
  end;

  if Count = 0 then
    Exit(-1);

  // Choisir aléatoirement
  Result := Available[Random(Count)];
end;
```

**3. Endgame Mode**

À la fin, demander les derniers morceaux à plusieurs pairs :

```pascal
procedure ActiverEndgameMode(Torrent: TTorrent);
var
  MissingPieces: TIntegerDynArray;
  i, j: Integer;
begin
  MissingPieces := Torrent.Bitfield.GetMissingPieces;

  // Demander chaque morceau manquant à tous les pairs
  for i := 0 to High(MissingPieces) do
  begin
    for j := 0 to Torrent.Peers.Count - 1 do
    begin
      if TPeer(Torrent.Peers[j]).Bitfield.HasPiece(MissingPieces[i]) then
        RequestPiece(TPeer(Torrent.Peers[j]), MissingPieces[i]);
    end;
  end;
end;
```

## DHT (Distributed Hash Table)

La DHT remplace le tracker centralisé par un système distribué.

### Principe de fonctionnement

**1. Espace des ID (160 bits avec SHA-1)**

```
0000...0000 ←──────────────────────→ FFFF...FFFF
     ↑        ↑        ↑        ↑
   Node A   Node B   Node C   Node D
```

**2. Distance XOR**

La distance entre deux ID est calculée avec XOR :

```pascal
function CalculerDistance(const ID1, ID2: TPeerID): TPeerID;
var
  i: Integer;
begin
  for i := 0 to High(ID1.Value) do
    Result.Value[i] := ID1.Value[i] xor ID2.Value[i];
end;

function CompareDistance(const ID1, ID2: TPeerID): Integer;
var
  i: Integer;
begin
  for i := 0 to High(ID1.Value) do
  begin
    if ID1.Value[i] < ID2.Value[i] then
      Exit(-1)
    else if ID1.Value[i] > ID2.Value[i] then
      Exit(1);
  end;
  Result := 0;
end;
```

### Kademlia (algorithme DHT de BitTorrent)

**Table de routage :**

```pascal
type
  TKBucket = class
  private
    FNodes: TList;
    FMaxSize: Integer;
  public
    constructor Create(MaxSize: Integer = 8);

    procedure AddNode(const NodeID: TPeerID; const Address: String; Port: Word);
    procedure RemoveNode(const NodeID: TPeerID);
    function FindNode(const NodeID: TPeerID): Boolean;
  end;

  TRoutingTable = class
  private
    FBuckets: array[0..159] of TKBucket;  // 160 buckets (un par bit)
    FLocalID: TPeerID;
  public
    constructor Create(const LocalID: TPeerID);
    destructor Destroy; override;

    procedure AddNode(const NodeID: TPeerID; const Address: String; Port: Word);
    function FindClosestNodes(const TargetID: TPeerID; Count: Integer): TArray<TNode>;
  end;

implementation

constructor TRoutingTable.Create(const LocalID: TPeerID);
var
  i: Integer;
begin
  inherited Create;
  FLocalID := LocalID;

  for i := 0 to High(FBuckets) do
    FBuckets[i] := TKBucket.Create;
end;

destructor TRoutingTable.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FBuckets) do
    FBuckets[i].Free;

  inherited Destroy;
end;

procedure TRoutingTable.AddNode(const NodeID: TPeerID; const Address: String; Port: Word);
var
  Distance: TPeerID;
  BucketIndex: Integer;
  i: Integer;
begin
  Distance := CalculerDistance(FLocalID, NodeID);

  // Trouver le premier bit à 1 (le bucket approprié)
  BucketIndex := -1;
  for i := 0 to 19 do  // 20 octets
  begin
    if Distance.Value[i] <> 0 then
    begin
      // Trouver la position du bit le plus significatif
      BucketIndex := i * 8;
      if Distance.Value[i] and $80 <> 0 then Inc(BucketIndex, 0)
      else if Distance.Value[i] and $40 <> 0 then Inc(BucketIndex, 1)
      else if Distance.Value[i] and $20 <> 0 then Inc(BucketIndex, 2)
      else if Distance.Value[i] and $10 <> 0 then Inc(BucketIndex, 3)
      else if Distance.Value[i] and $08 <> 0 then Inc(BucketIndex, 4)
      else if Distance.Value[i] and $04 <> 0 then Inc(BucketIndex, 5)
      else if Distance.Value[i] and $02 <> 0 then Inc(BucketIndex, 6)
      else if Distance.Value[i] and $01 <> 0 then Inc(BucketIndex, 7);
      Break;
    end;
  end;

  if BucketIndex >= 0 then
    FBuckets[BucketIndex].AddNode(NodeID, Address, Port);
end;

function TRoutingTable.FindClosestNodes(const TargetID: TPeerID; Count: Integer): TArray<TNode>;
// Trouver les Count nodes les plus proches de TargetID
begin
  // Implémentation : parcourir les buckets et trier par distance
  // ...
end;
```

### Requêtes DHT

**Types de requêtes Kademlia :**

```pascal
type
  TDHTQueryType = (
    dqPing,           // Vérifier si un node est vivant
    dqFindNode,       // Trouver des nodes proches d'un ID
    dqGetPeers,       // Trouver les pairs qui ont un torrent
    dqAnnouncePeer    // Annoncer qu'on a un torrent
  );

  TDHTQuery = record
    QueryType: TDHTQueryType;
    TransactionID: String;
    QueryingNodeID: TPeerID;
    TargetID: TPeerID;
    Token: String;  // Pour announce_peer
  end;

function CreerRequeteFindNode(const NodeID, TargetID: TPeerID): String;
var
  Query: TDHTQuery;
begin
  // Format: Bencode
  // d1:ad2:id20:<node_id>6:target20:<target_id>e1:q9:find_node1:t2:aa1:y1:qe

  Result := 'd1:ad2:id20:' + PeerIDToString(NodeID) +
            '6:target20:' + PeerIDToString(TargetID) +
            'e1:q9:find_node1:t2:aa1:y1:qe';
end;
```

## Blockchain simplifié

La blockchain est un registre distribué décentralisé.

### Concepts de base

**1. Block (bloc)**

```pascal
type
  TBlock = class
  private
    FIndex: Integer;
    FTimestamp: TDateTime;
    FData: String;
    FPreviousHash: String;
    FHash: String;
    FNonce: Integer;
  public
    constructor Create(Index: Integer; const Data, PreviousHash: String);

    function CalculateHash: String;
    procedure MineBlock(Difficulty: Integer);

    property Index: Integer read FIndex;
    property Timestamp: TDateTime read FTimestamp;
    property Data: String read FData;
    property PreviousHash: String read FPreviousHash;
    property Hash: String read FHash;
  end;

implementation

uses
  mormot.core.crypto;

constructor TBlock.Create(Index: Integer; const Data, PreviousHash: String);
begin
  inherited Create;
  FIndex := Index;
  FTimestamp := Now;
  FData := Data;
  FPreviousHash := PreviousHash;
  FNonce := 0;
  FHash := CalculateHash;
end;

function TBlock.CalculateHash: String;
var
  Input: String;
  Hash: TSha256Digest;
begin
  Input := IntToStr(FIndex) +
           DateTimeToStr(FTimestamp) +
           FData +
           FPreviousHash +
           IntToStr(FNonce);

  Hash := SHA256(Input);
  Result := SHA256DigestToString(Hash);
end;

procedure TBlock.MineBlock(Difficulty: Integer);
var
  Target: String;
begin
  // Créer une chaîne de '0' pour la difficulté
  Target := StringOfChar('0', Difficulty);

  // Incrémenter le nonce jusqu'à ce que le hash commence par Target
  while Copy(FHash, 1, Difficulty) <> Target do
  begin
    Inc(FNonce);
    FHash := CalculateHash;
  end;

  WriteLn('Bloc miné : ', FHash);
end;
```

**2. Blockchain**

```pascal
type
  TBlockchain = class
  private
    FChain: TObjectList;
    FDifficulty: Integer;
  public
    constructor Create(Difficulty: Integer = 4);
    destructor Destroy; override;

    function GetLatestBlock: TBlock;
    procedure AddBlock(const Data: String);
    function IsValid: Boolean;

    property Chain: TObjectList read FChain;
  end;

implementation

constructor TBlockchain.Create(Difficulty: Integer);
var
  GenesisBlock: TBlock;
begin
  inherited Create;
  FChain := TObjectList.Create(True);
  FDifficulty := Difficulty;

  // Créer le bloc genesis (premier bloc)
  GenesisBlock := TBlock.Create(0, 'Genesis Block', '0');
  FChain.Add(GenesisBlock);
end;

destructor TBlockchain.Destroy;
begin
  FChain.Free;
  inherited Destroy;
end;

function TBlockchain.GetLatestBlock: TBlock;
begin
  Result := TBlock(FChain[FChain.Count - 1]);
end;

procedure TBlockchain.AddBlock(const Data: String);
var
  NewBlock: TBlock;
  LatestBlock: TBlock;
begin
  LatestBlock := GetLatestBlock;
  NewBlock := TBlock.Create(LatestBlock.Index + 1, Data, LatestBlock.Hash);

  WriteLn('Mining bloc ', NewBlock.Index, '...');
  NewBlock.MineBlock(FDifficulty);

  FChain.Add(NewBlock);
end;

function TBlockchain.IsValid: Boolean;
var
  i: Integer;
  CurrentBlock, PreviousBlock: TBlock;
begin
  // Vérifier chaque bloc (sauf le genesis)
  for i := 1 to FChain.Count - 1 do
  begin
    CurrentBlock := TBlock(FChain[i]);
    PreviousBlock := TBlock(FChain[i - 1]);

    // Vérifier le hash du bloc
    if CurrentBlock.Hash <> CurrentBlock.CalculateHash then
    begin
      WriteLn('Hash invalide pour le bloc ', CurrentBlock.Index);
      Exit(False);
    end;

    // Vérifier le lien avec le bloc précédent
    if CurrentBlock.PreviousHash <> PreviousBlock.Hash then
    begin
      WriteLn('Lien invalide pour le bloc ', CurrentBlock.Index);
      Exit(False);
    end;
  end;

  Result := True;
end;
```

**3. Utilisation**

```pascal
program TestBlockchain;

var
  Blockchain: TBlockchain;
begin
  Blockchain := TBlockchain.Create(4);  // Difficulté 4
  try
    WriteLn('=== Blockchain Simple ===');
    WriteLn;

    WriteLn('Ajout de blocs...');
    Blockchain.AddBlock('Transaction 1: Alice -> Bob: 10 BTC');
    Blockchain.AddBlock('Transaction 2: Bob -> Charlie: 5 BTC');
    Blockchain.AddBlock('Transaction 3: Charlie -> Alice: 3 BTC');

    WriteLn;
    WriteLn('Blockchain valide ? ', Blockchain.IsValid);
    WriteLn;

    WriteLn('Contenu de la blockchain :');
    for i := 0 to Blockchain.Chain.Count - 1 do
    begin
      Block := TBlock(Blockchain.Chain[i]);
      WriteLn('Bloc ', Block.Index);
      WriteLn('  Timestamp : ', DateTimeToStr(Block.Timestamp));
      WriteLn('  Data : ', Block.Data);
      WriteLn('  Hash : ', Block.Hash);
      WriteLn('  Previous : ', Block.PreviousHash);
      WriteLn;
    end;

  finally
    Blockchain.Free;
  end;
end.
```

## Consensus distribué

Dans un réseau décentralisé, les pairs doivent se mettre d'accord sur l'état du système.

### Proof of Work (PoW)

**Principe :** Résoudre un problème mathématique difficile.

```pascal
type
  TProofOfWork = class
  public
    class function FindProof(LastProof: Integer): Integer;
    class function ValidateProof(LastProof, Proof: Integer): Boolean;
  end;

class function TProofOfWork.FindProof(LastProof: Integer): Integer;
begin
  Result := 0;
  while not ValidateProof(LastProof, Result) do
    Inc(Result);
end;

class function TProofOfWork.ValidateProof(LastProof, Proof: Integer): Boolean;
var
  Guess: String;
  Hash: TSha256Digest;
begin
  // Le hash de "LastProof+Proof" doit commencer par "0000"
  Guess := IntToStr(LastProof) + IntToStr(Proof);
  Hash := SHA256(Guess);
  Result := Copy(SHA256DigestToString(Hash), 1, 4) = '0000';
end;
```

### Proof of Stake (PoS)

**Principe :** Les validateurs sont choisis selon leur "stake" (participation).

```pascal
type
  TValidator = class
  private
    FAddress: String;
    FStake: Int64;
  public
    property Address: String read FAddress;
    property Stake: Int64 read FStake;
  end;

function ChoisirValidateur(Validators: TObjectList): TValidator;
var
  TotalStake: Int64;
  RandomValue: Int64;
  CurrentSum: Int64;
  i: Integer;
begin
  // Calculer le total des stakes
  TotalStake := 0;
  for i := 0 to Validators.Count - 1 do
    TotalStake := TotalStake + TValidator(Validators[i]).Stake;

  // Choisir aléatoirement selon le poids
  RandomValue := Random(TotalStake);
  CurrentSum := 0;

  for i := 0 to Validators.Count - 1 do
  begin
    CurrentSum := CurrentSum + TValidator(Validators[i]).Stake;
    if CurrentSum >= RandomValue then
      Exit(TValidator(Validators[i]));
  end;

  Result := nil;
end;
```

### Consensus de Raft (pour systèmes fermés)

**États d'un nœud :**

```pascal
type
  TNodeState = (nsFollower, nsCandidate, nsLeader);

  TRaftNode = class
  private
    FState: TNodeState;
    FCurrentTerm: Integer;
    FVotedFor: String;
    FLog: TList;
    FCommitIndex: Integer;
    FLastApplied: Integer;
  public
    procedure StartElection;
    procedure ReceiveVoteRequest(Term: Integer; CandidateID: String);
    procedure ReceiveVoteResponse(Granted: Boolean);
    procedure BecomeLeader;
  end;

procedure TRaftNode.StartElection;
begin
  FState := nsCandidate;
  Inc(FCurrentTerm);
  FVotedFor := MyNodeID;

  // Envoyer des demandes de vote à tous les autres nœuds
  BroadcastVoteRequest(FCurrentTerm, MyNodeID);
end;

procedure TRaftNode.ReceiveVoteRequest(Term: Integer; CandidateID: String);
begin
  if Term > FCurrentTerm then
  begin
    FCurrentTerm := Term;
    FState := nsFollower;
    FVotedFor := '';
  end;

  // Accorder le vote si on n'a pas encore voté
  if (FVotedFor = '') or (FVotedFor = CandidateID) then
  begin
    FVotedFor := CandidateID;
    SendVoteResponse(CandidateID, True);
  end;
end;
```

## Sécurité dans les réseaux P2P

### 1. Attaque Sybil

Un attaquant crée de nombreux faux nœuds pour contrôler le réseau.

**Défenses :**

```pascal
type
  TAntiSybil = class
  private
    FTrustedNodes: TStringList;
    FNodeReputations: TDictionary<String, Integer>;
  public
    function ValidateNode(const NodeID: String): Boolean;
    procedure UpdateReputation(const NodeID: String; GoodBehavior: Boolean);
  end;

function TAntiSybil.ValidateNode(const NodeID: String): Boolean;
var
  Reputation: Integer;
begin
  // Vérifier si le nœud est de confiance
  if FTrustedNodes.IndexOf(NodeID) >= 0 then
    Exit(True);

  // Vérifier la réputation
  if FNodeReputations.TryGetValue(NodeID, Reputation) then
    Result := Reputation >= 50  // Seuil de réputation
  else
    Result := False;  // Nouveau nœud non validé
end;

procedure TAntiSybil.UpdateReputation(const NodeID: String; GoodBehavior: Boolean);
var
  Reputation: Integer;
begin
  if not FNodeReputations.TryGetValue(NodeID, Reputation) then
    Reputation := 0;

  if GoodBehavior then
    Inc(Reputation, 1)
  else
    Dec(Reputation, 5);

  FNodeReputations.AddOrSetValue(NodeID, Reputation);
end;
```

### 2. Eclipse Attack

Isoler un nœud en contrôlant toutes ses connexions.

**Défense :**

```pascal
procedure DiversifierConnexions(Network: TP2PNetwork);
var
  Subnets: TDictionary<String, Integer>;
  Peer: TPeer;
  Subnet: String;
  Count: Integer;
begin
  Subnets := TDictionary<String, Integer>.Create;
  try
    // Compter les pairs par sous-réseau
    for Peer in Network.Peers do
    begin
      Subnet := Copy(Peer.Address, 1, LastDelimiter('.', Peer.Address));
      if Subnets.TryGetValue(Subnet, Count) then
        Subnets[Subnet] := Count + 1
      else
        Subnets.Add(Subnet, 1);
    end;

    // Vérifier la diversité
    for Subnet in Subnets.Keys do
    begin
      Count := Subnets[Subnet];
      if Count > Network.GetPeerCount div 2 then
        WriteLn('ALERTE : Trop de pairs dans le sous-réseau ', Subnet);
    end;
  finally
    Subnets.Free;
  end;
end;
```

### 3. Man-in-the-Middle

**Protection avec chiffrement :**

```pascal
uses
  mormot.core.crypto;

type
  TSecureP2PPeer = class(TPeer)
  private
    FSharedSecret: TBytes;
    FEncrypted: Boolean;
  public
    procedure EstablishEncryption;
    procedure SendEncrypted(const Data: String);
    function ReceiveDecrypted: String;
  end;

procedure TSecureP2PPeer.EstablishEncryption;
var
  MyPrivateKey, MyPublicKey: TECCPrivateKey;
  PeerPublicKey: TECCPublicKey;
begin
  // Générer une paire de clés
  ECCKeyPair(MyPrivateKey, MyPublicKey);

  // Échanger les clés publiques
  Send(ECCPublicKeyToBase64(MyPublicKey));
  PeerPublicKey := ECCPublicKeyFromBase64(Receive);

  // Calculer le secret partagé (ECDH)
  FSharedSecret := ECDH(MyPrivateKey, PeerPublicKey);
  FEncrypted := True;

  WriteLn('Connexion chiffrée établie');
end;

procedure TSecureP2PPeer.SendEncrypted(const Data: String);
var
  Encrypted: TBytes;
begin
  if not FEncrypted then
    raise Exception.Create('Connexion non chiffrée');

  // Chiffrer avec AES-256
  Encrypted := AES256Encrypt(Data, FSharedSecret);
  Send(Base64Encode(Encrypted));
end;

function TSecureP2PPeer.ReceiveDecrypted: String;
var
  Encrypted, Decrypted: TBytes;
begin
  if not FEncrypted then
    raise Exception.Create('Connexion non chiffrée');

  Encrypted := Base64Decode(Receive);
  Decrypted := AES256Decrypt(Encrypted, FSharedSecret);
  Result := BytesToString(Decrypted);
end;
```

## Différences Windows / Linux

### Gestion des sockets

**Windows :**
```pascal
{$IFDEF WINDOWS}
uses
  WinSock2;

procedure InitialiserSocketsWindows;
var
  WSAData: TWSAData;
begin
  if WSAStartup(MakeWord(2, 2), WSAData) <> 0 then
    raise Exception.Create('Erreur initialisation WinSock');
end;

procedure NettoyerSocketsWindows;
begin
  WSACleanup;
end;
{$ENDIF}
```

**Linux :**
```pascal
{$IFDEF UNIX}
uses
  BaseUnix, Sockets;

// Pas d'initialisation nécessaire sous Linux

procedure ConfigurerSocketLinux(Socket: TSocket);
var
  Flag: Integer;
begin
  // Configurer en mode non-bloquant
  Flag := fpFcntl(Socket, F_GETFL, 0);
  fpFcntl(Socket, F_SETFL, Flag or O_NONBLOCK);

  // Désactiver l'algorithme de Nagle
  Flag := 1;
  fpSetSockOpt(Socket, IPPROTO_TCP, TCP_NODELAY, @Flag, SizeOf(Flag));
end;
{$ENDIF}
```

### Multithreading

**Configuration selon l'OS :**

```pascal
program P2PMultiPlateforme;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,  // Nécessaire pour les threads sous Unix
  {$ENDIF}
  Classes, SysUtils, SimpleP2P;

begin
  {$IFDEF WINDOWS}
  WriteLn('Plateforme : Windows');
  InitialiserSocketsWindows;
  {$ENDIF}

  {$IFDEF UNIX}
  WriteLn('Plateforme : Linux/Unix');
  {$ENDIF}

  // Code commun
  // ...

  {$IFDEF WINDOWS}
  NettoyerSocketsWindows;
  {$ENDIF}
end.
```

### Pare-feu et permissions

**Windows :**
```batch
REM Ouvrir le port dans le pare-feu Windows
netsh advfirewall firewall add rule ^
  name="P2P Application" ^
  dir=in action=allow protocol=TCP localport=6881

netsh advfirewall firewall add rule ^
  name="P2P Application UDP" ^
  dir=in action=allow protocol=UDP localport=6881
```

**Linux/Ubuntu :**
```bash
# Ouvrir les ports avec UFW
sudo ufw allow 6881/tcp
sudo ufw allow 6881/udp
sudo ufw reload

# Ou avec iptables
sudo iptables -A INPUT -p tcp --dport 6881 -j ACCEPT
sudo iptables -A INPUT -p udp --dport 6881 -j ACCEPT
sudo iptables-save > /etc/iptables/rules.v4
```

### Configuration réseau spécifique

**Vérifier l'IP publique :**

```pascal
function ObtenirIPPublique: String;
{$IFDEF WINDOWS}
var
  HTTP: THTTPClient;
{$ENDIF}
{$IFDEF UNIX}
var
  Output: String;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  HTTP := THTTPClient.Create;
  try
    Result := HTTP.Get('https://api.ipify.org');
  finally
    HTTP.Free;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Utiliser curl
  RunCommand('curl', ['-s', 'https://api.ipify.org'], Output);
  Result := Trim(Output);
  {$ENDIF}
end;
```

## Applications pratiques P2P

### 1. Chat P2P décentralisé

```pascal
unit P2PChat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleP2P;

type
  TP2PChat = class
  private
    FNetwork: TP2PNetwork;
    FUsername: String;

    procedure OnPeerConnected(Peer: TPeer);
    procedure OnMessageReceived(Peer: TPeer; const Data: String);
  public
    constructor Create(const Username: String; Port: Word = 6881);
    destructor Destroy; override;

    procedure SendMessage(const Message: String);
    procedure ConnectToPeer(const Address: String; Port: Word);
  end;

implementation

uses
  mormot.core.json;

constructor TP2PChat.Create(const Username: String; Port: Word);
begin
  inherited Create;
  FUsername := Username;

  FNetwork := TP2PNetwork.Create(Port);
  FNetwork.OnPeerConnected := @OnPeerConnected;
  FNetwork.OnMessageReceived := @OnMessageReceived;
  FNetwork.Start;

  WriteLn('Chat P2P démarré. Pseudo : ', FUsername);
end;

destructor TP2PChat.Destroy;
begin
  FNetwork.Free;
  inherited Destroy;
end;

procedure TP2PChat.OnPeerConnected(Peer: TPeer);
var
  WelcomeMsg: String;
begin
  WriteLn('✓ Pair connecté : ', Peer.Address);

  // Envoyer notre pseudo
  WelcomeMsg := Format('{"type":"hello","username":"%s"}', [FUsername]);
  Peer.Send(WelcomeMsg);
end;

procedure TP2PChat.OnMessageReceived(Peer: TPeer; const Data: String);
var
  JSON: TDocVariantData;
  MsgType, Username, Message: String;
begin
  // Parser le JSON
  JSON.InitJSON(Data);

  MsgType := JSON.U['type'];

  // case sur des chaînes n'est pas supporté en FreePascal
  if MsgType = 'hello' then
  begin
    Username := JSON.U['username'];
    WriteLn('[SYSTÈME] ', Username, ' a rejoint le chat');
  end
  else if MsgType = 'message' then
  begin
    Username := JSON.U['username'];
    Message := JSON.U['message'];
    WriteLn('[', Username, '] ', Message);
  end
  else if MsgType = 'bye' then
  begin
    Username := JSON.U['username'];
    WriteLn('[SYSTÈME] ', Username, ' a quitté le chat');
  end;
end;

procedure TP2PChat.SendMessage(const Message: String);
var
  JSON: String;
begin
  JSON := Format('{"type":"message","username":"%s","message":"%s"}',
                 [FUsername, Message]);
  FNetwork.Broadcast(JSON);
end;

procedure TP2PChat.ConnectToPeer(const Address: String; Port: Word);
begin
  FNetwork.ConnectToPeer(Address, Port);
end;

end.
```

**Programme principal :**

```pascal
program ChatP2PApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, P2PChat;

var
  Chat: TP2PChat;
  Input: String;
  Parts: TStringList;
begin
  Write('Votre pseudo : ');
  ReadLn(Input);

  Chat := TP2PChat.Create(Input);
  try
    WriteLn;
    WriteLn('Commandes :');
    WriteLn('  /connect <ip> <port> - Se connecter à un pair');
    WriteLn('  /quit                - Quitter');
    WriteLn('  Tout autre texte     - Envoyer un message');
    WriteLn;

    repeat
      Write('> ');
      ReadLn(Input);

      if Input = '' then
        Continue;

      if Input[1] = '/' then
      begin
        Parts := TStringList.Create;
        try
          Parts.Delimiter := ' ';
          Parts.StrictDelimiter := True;
          Parts.DelimitedText := Input;

          if LowerCase(Parts[0]) = '/connect' then
          begin
            if Parts.Count >= 3 then
              Chat.ConnectToPeer(Parts[1], StrToInt(Parts[2]))
            else
              WriteLn('Usage : /connect <ip> <port>');
          end
          else if LowerCase(Parts[0]) = '/quit' then
            Break
          else
            WriteLn('Commande inconnue');
        finally
          Parts.Free;
        end;
      end
      else
      begin
        // Envoyer le message
        Chat.SendMessage(Input);
      end;

    until False;

  finally
    Chat.Free;
  end;
end.
```

### 2. Partage de fichiers P2P simple

```pascal
unit P2PFileShare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, SimpleP2P;

type
  TSharedFile = class
  private
    FFilename: String;
    FHash: String;
    FSize: Int64;
  public
    property Filename: String read FFilename;
    property Hash: String read FHash;
    property Size: Int64 read FSize;
  end;

  TP2PFileShare = class
  private
    FNetwork: TP2PNetwork;
    FSharedFiles: TObjectList;
    FDownloadFolder: String;

    procedure OnMessageReceived(Peer: TPeer; const Data: String);
    procedure HandleFileListRequest(Peer: TPeer);
    procedure HandleFileRequest(Peer: TPeer; const Hash: String);
    procedure HandleFileChunk(Peer: TPeer; const Hash: String;
                             ChunkIndex: Integer; const Data: TBytes);
  public
    constructor Create(const DownloadFolder: String; Port: Word = 6881);
    destructor Destroy; override;

    procedure ShareFile(const Filename: String);
    procedure RequestFile(Peer: TPeer; const Hash: String);
    procedure ListSharedFiles;
  end;

implementation

uses
  mormot.core.crypto,
  mormot.core.json;

constructor TP2PFileShare.Create(const DownloadFolder: String; Port: Word);
begin
  inherited Create;
  FDownloadFolder := DownloadFolder;
  FSharedFiles := TObjectList.Create(True);

  ForceDirectories(FDownloadFolder);

  FNetwork := TP2PNetwork.Create(Port);
  FNetwork.OnMessageReceived := @OnMessageReceived;
  FNetwork.Start;

  WriteLn('Partage de fichiers P2P démarré');
  WriteLn('Dossier de téléchargement : ', FDownloadFolder);
end;

destructor TP2PFileShare.Destroy;
begin
  FSharedFiles.Free;
  FNetwork.Free;
  inherited Destroy;
end;

procedure TP2PFileShare.ShareFile(const Filename: String);
var
  SharedFile: TSharedFile;
  FileStream: TFileStream;
  Hash: TSha256Digest;
begin
  if not FileExists(Filename) then
  begin
    WriteLn('Fichier non trouvé : ', Filename);
    Exit;
  end;

  SharedFile := TSharedFile.Create;
  SharedFile.FFilename := ExtractFileName(Filename);

  // Calculer le hash du fichier
  FileStream := TFileStream.Create(Filename, fmOpenRead);
  try
    SharedFile.FSize := FileStream.Size;
    Hash := SHA256Stream(FileStream);
    SharedFile.FHash := SHA256DigestToString(Hash);
  finally
    FileStream.Free;
  end;

  FSharedFiles.Add(SharedFile);

  WriteLn('Fichier partagé : ', SharedFile.Filename);
  WriteLn('  Hash : ', SharedFile.Hash);
  WriteLn('  Taille : ', SharedFile.Size, ' octets');
end;

procedure TP2PFileShare.OnMessageReceived(Peer: TPeer; const Data: String);
var
  JSON: TDocVariantData;
  MsgType: String;
begin
  JSON.InitJSON(Data);
  MsgType := JSON.U['type'];

  if MsgType = 'list_request' then
    HandleFileListRequest(Peer)
  else if MsgType = 'file_request' then
    HandleFileRequest(Peer, JSON.U['hash'])
  else if MsgType = 'file_chunk' then
    HandleFileChunk(Peer, JSON.U['hash'], JSON.I['chunk'],
                   Base64ToBin(JSON.U['data']));
end;

procedure TP2PFileShare.HandleFileListRequest(Peer: TPeer);
var
  JSON: TDocVariantData;
  Files: TDocVariantData;
  i: Integer;
  SharedFile: TSharedFile;
begin
  Files.InitArray([]);

  for i := 0 to FSharedFiles.Count - 1 do
  begin
    SharedFile := TSharedFile(FSharedFiles[i]);
    Files.AddItem(_ObjFast([
      'filename', SharedFile.Filename,
      'hash', SharedFile.Hash,
      'size', SharedFile.Size
    ]));
  end;

  JSON.InitObject(['type', 'file_list', 'files', Variant(Files)]);
  Peer.Send(JSON.ToJson);
end;

procedure TP2PFileShare.HandleFileRequest(Peer: TPeer; const Hash: String);
const
  CHUNK_SIZE = 65536;  // 64 KB
var
  i: Integer;
  SharedFile: TSharedFile;
  FileStream: TFileStream;
  ChunkData: array[0..CHUNK_SIZE-1] of Byte;
  BytesRead: Integer;
  ChunkIndex: Integer;
  JSON: TDocVariantData;
begin
  // Trouver le fichier
  SharedFile := nil;
  for i := 0 to FSharedFiles.Count - 1 do
  begin
    if TSharedFile(FSharedFiles[i]).Hash = Hash then
    begin
      SharedFile := TSharedFile(FSharedFiles[i]);
      Break;
    end;
  end;

  if SharedFile = nil then
  begin
    WriteLn('Fichier demandé non trouvé : ', Hash);
    Exit;
  end;

  // Envoyer le fichier par morceaux
  FileStream := TFileStream.Create(
    FDownloadFolder + PathDelim + SharedFile.Filename, fmOpenRead);
  try
    ChunkIndex := 0;
    repeat
      BytesRead := FileStream.Read(ChunkData, CHUNK_SIZE);
      if BytesRead > 0 then
      begin
        JSON.InitObject([
          'type', 'file_chunk',
          'hash', Hash,
          'chunk', ChunkIndex,
          'data', BinToBase64(@ChunkData, BytesRead)
        ]);

        Peer.Send(JSON.ToJson);
        Inc(ChunkIndex);
      end;
    until BytesRead = 0;

    WriteLn('Fichier envoyé : ', SharedFile.Filename, ' (', ChunkIndex, ' morceaux)');
  finally
    FileStream.Free;
  end;
end;

procedure TP2PFileShare.HandleFileChunk(Peer: TPeer; const Hash: String;
                                       ChunkIndex: Integer; const Data: TBytes);
var
  Filename: String;
  FileStream: TFileStream;
begin
  Filename := FDownloadFolder + PathDelim + Hash + '.tmp';

  if ChunkIndex = 0 then
    FileStream := TFileStream.Create(Filename, fmCreate)
  else
    FileStream := TFileStream.Create(Filename, fmOpenWrite);

  try
    FileStream.Seek(0, soEnd);
    FileStream.Write(Data[0], Length(Data));
  finally
    FileStream.Free;
  end;

  WriteLn('Chunk ', ChunkIndex, ' reçu (', Length(Data), ' octets)');
end;

procedure TP2PFileShare.RequestFile(Peer: TPeer; const Hash: String);
var
  JSON: TDocVariantData;
begin
  JSON.InitObject(['type', 'file_request', 'hash', Hash]);
  Peer.Send(JSON.ToJson);
  WriteLn('Demande de téléchargement envoyée : ', Hash);
end;

procedure TP2PFileShare.ListSharedFiles;
var
  i: Integer;
  SharedFile: TSharedFile;
begin
  WriteLn('=== Fichiers partagés ===');
  for i := 0 to FSharedFiles.Count - 1 do
  begin
    SharedFile := TSharedFile(FSharedFiles[i]);
    WriteLn(i + 1, '. ', SharedFile.Filename);
    WriteLn('   Hash : ', SharedFile.Hash);
    WriteLn('   Taille : ', SharedFile.Size, ' octets');
  end;
end;

end.
```

### 3. Système de vote décentralisé

```pascal
unit P2PVoting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, SimpleP2P;

type
  TVote = class
  private
    FVoterID: String;
    FCandidate: String;
    FTimestamp: TDateTime;
    FSignature: String;
  public
    property VoterID: String read FVoterID;
    property Candidate: String read FCandidate;
    property Timestamp: TDateTime read FTimestamp;
    property Signature: String read FSignature;
  end;

  TP2PVoting = class
  private
    FNetwork: TP2PNetwork;
    FVotes: TObjectList;
    FMyVoterID: String;

    procedure OnMessageReceived(Peer: TPeer; const Data: String);
    function VerifyVote(Vote: TVote): Boolean;
  public
    constructor Create(const VoterID: String; Port: Word = 6881);
    destructor Destroy; override;

    procedure CastVote(const Candidate: String);
    procedure TallyVotes;
  end;

implementation

uses
  mormot.core.crypto,
  mormot.core.json;

constructor TP2PVoting.Create(const VoterID: String; Port: Word);
begin
  inherited Create;
  FMyVoterID := VoterID;
  FVotes := TObjectList.Create(True);

  FNetwork := TP2PNetwork.Create(Port);
  FNetwork.OnMessageReceived := @OnMessageReceived;
  FNetwork.Start;

  WriteLn('Système de vote P2P démarré');
  WriteLn('ID votant : ', FMyVoterID);
end;

destructor TP2PVoting.Destroy;
begin
  FVotes.Free;
  FNetwork.Free;
  inherited Destroy;
end;

procedure TP2PVoting.CastVote(const Candidate: String);
var
  Vote: TVote;
  JSON: TDocVariantData;
  ToSign: String;
  Hash: TSha256Digest;
begin
  Vote := TVote.Create;
  Vote.FVoterID := FMyVoterID;
  Vote.FCandidate := Candidate;
  Vote.FTimestamp := Now;

  // Signer le vote (simplifié - en pratique utiliser une vraie signature)
  ToSign := Vote.VoterID + Vote.Candidate + DateTimeToStr(Vote.Timestamp);
  Hash := SHA256(ToSign);
  Vote.FSignature := SHA256DigestToString(Hash);

  FVotes.Add(Vote);

  // Diffuser le vote
  JSON.InitObject([
    'type', 'vote',
    'voter_id', Vote.VoterID,
    'candidate', Vote.Candidate,
    'timestamp', DateTimeToStr(Vote.Timestamp),
    'signature', Vote.Signature
  ]);

  FNetwork.Broadcast(JSON.ToJson);
  WriteLn('Vote enregistré pour : ', Candidate);
end;

procedure TP2PVoting.OnMessageReceived(Peer: TPeer; const Data: String);
var
  JSON: TDocVariantData;
  Vote: TVote;
  i: Integer;
  Duplicate: Boolean;
begin
  JSON.InitJSON(Data);

  if JSON.U['type'] = 'vote' then
  begin
    Vote := TVote.Create;
    Vote.FVoterID := JSON.U['voter_id'];
    Vote.FCandidate := JSON.U['candidate'];
    Vote.FTimestamp := StrToDateTime(JSON.U['timestamp']);
    Vote.FSignature := JSON.U['signature'];

    // Vérifier le vote
    if not VerifyVote(Vote) then
    begin
      WriteLn('Vote invalide ignoré');
      Vote.Free;
      Exit;
    end;

    // Vérifier les doublons (un votant ne peut voter qu'une fois)
    Duplicate := False;
    for i := 0 to FVotes.Count - 1 do
    begin
      if TVote(FVotes[i]).VoterID = Vote.VoterID then
      begin
        Duplicate := True;
        Break;
      end;
    end;

    if Duplicate then
    begin
      WriteLn('Vote en double ignoré de : ', Vote.VoterID);
      Vote.Free;
    end
    else
    begin
      FVotes.Add(Vote);
      WriteLn('Vote reçu : ', Vote.VoterID, ' → ', Vote.Candidate);
    end;
  end;
end;

function TP2PVoting.VerifyVote(Vote: TVote): Boolean;
var
  ToVerify: String;
  Hash: TSha256Digest;
  ExpectedSignature: String;
begin
  ToVerify := Vote.VoterID + Vote.Candidate + DateTimeToStr(Vote.Timestamp);
  Hash := SHA256(ToVerify);
  ExpectedSignature := SHA256DigestToString(Hash);

  Result := ExpectedSignature = Vote.Signature;
end;

procedure TP2PVoting.TallyVotes;
var
  Tally: TDictionary<String, Integer>;
  i, Count: Integer;
  Vote: TVote;
  Candidate: String;
begin
  Tally := TDictionary<String, Integer>.Create;
  try
    // Compter les votes
    for i := 0 to FVotes.Count - 1 do
    begin
      Vote := TVote(FVotes[i]);

      if Tally.TryGetValue(Vote.Candidate, Count) then
        Tally[Vote.Candidate] := Count + 1
      else
        Tally.Add(Vote.Candidate, 1);
    end;

    // Afficher les résultats
    WriteLn;
    WriteLn('=== Résultats du vote ===');
    WriteLn('Total des votes : ', FVotes.Count);
    WriteLn;

    for Candidate in Tally.Keys do
    begin
      Count := Tally[Candidate];
      WriteLn(Candidate, ' : ', Count, ' votes (',
              (Count * 100.0 / FVotes.Count):0:1, '%)');
    end;
  finally
    Tally.Free;
  end;
end;

end.
```

## Optimisation et performance

### 1. Connection pooling

```pascal
type
  TConnectionPool = class
  private
    FAvailablePeers: TThreadList;
    FBusyPeers: TThreadList;
    FMaxConnections: Integer;
  public
    constructor Create(MaxConnections: Integer = 50);
    destructor Destroy; override;

    function AcquirePeer: TPeer;
    procedure ReleasePeer(Peer: TPeer);
  end;

constructor TConnectionPool.Create(MaxConnections: Integer);
begin
  inherited Create;
  FMaxConnections := MaxConnections;
  FAvailablePeers := TThreadList.Create;
  FBusyPeers := TThreadList.Create;
end;

function TConnectionPool.AcquirePeer: TPeer;
var
  List: TList;
begin
  List := FAvailablePeers.LockList;
  try
    if List.Count > 0 then
    begin
      Result := TPeer(List[0]);
      List.Delete(0);
      FBusyPeers.Add(Result);
    end
    else
      Result := nil;
  finally
    FAvailablePeers.UnlockList;
  end;
end;

procedure TConnectionPool.ReleasePeer(Peer: TPeer);
begin
  FBusyPeers.Remove(Peer);
  FAvailablePeers.Add(Peer);
end;
```

### 2. Compression des messages

```pascal
uses
  mormot.core.zip;

function CompresserMessage(const Data: String): TBytes;
var
  Compressed: RawByteString;
begin
  Compressed := AlgoGZFast.Compress(Data);
  Result := BytesOf(Compressed);
end;

function DecompresserMessage(const Data: TBytes): String;
var
  Decompressed: RawByteString;
begin
  Decompressed := AlgoGZFast.Decompress(Data);
  Result := UTF8ToString(Decompressed);
end;
```

### 3. Rate limiting

```pascal
type
  TRateLimiter = class
  private
    FRequests: TDictionary<String, TList<TDateTime>>;
    FMaxPerMinute: Integer;
  public
    constructor Create(MaxPerMinute: Integer = 60);
    destructor Destroy; override;

    function AllowRequest(const PeerID: String): Boolean;
  end;

function TRateLimiter.AllowRequest(const PeerID: String): Boolean;
var
  Timestamps: TList<TDateTime>;
  Now: TDateTime;
  i: Integer;
begin
  Now := SysUtils.Now;

  if not FRequests.TryGetValue(PeerID, Timestamps) then
  begin
    Timestamps := TList<TDateTime>.Create;
    FRequests.Add(PeerID, Timestamps);
  end;

  // Retirer les timestamps de plus d'une minute
  for i := Timestamps.Count - 1 downto 0 do
  begin
    if MinutesBetween(Now, Timestamps[i]) > 1 then
      Timestamps.Delete(i);
  end;

  // Vérifier la limite
  if Timestamps.Count >= FMaxPerMinute then
    Exit(False);

  Timestamps.Add(Now);
  Result := True;
end;
```

## Tests et validation

### Tests unitaires

```pascal
unit TestsP2P;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry,
  SimpleP2P;

type
  TTestP2P = class(TTestCase)
  published
    procedure TestPeerID;
    procedure TestBitfield;
    procedure TestMessageEncoding;
    procedure TestBlockchain;
  end;

implementation

procedure TTestP2P.TestPeerID;
var
  ID1, ID2: TPeerID;
begin
  ID1 := GeneratePeerID;
  ID2 := GeneratePeerID;

  AssertFalse('Les IDs doivent être différents',
              CompareMem(@ID1, @ID2, SizeOf(TPeerID)));
end;

procedure TTestP2P.TestBitfield;
var
  Bitfield: TBitfield;
begin
  Bitfield := TBitfield.Create(100);
  try
    // Tester l'initialisation
    AssertFalse('Le morceau 0 ne doit pas être marqué', Bitfield.HasPiece(0));

    // Marquer un morceau
    Bitfield.SetPiece(50, True);
    AssertTrue('Le morceau 50 doit être marqué', Bitfield.HasPiece(50));

    // Tester la complétion
    AssertEquals('Complétion attendue', 1.0, Bitfield.GetCompletionPercent, 0.01);
  finally
    Bitfield.Free;
  end;
end;

procedure TTestP2P.TestMessageEncoding;
var
  Original, Decoded: String;
  Encoded: TBytes;
begin
  Original := 'Message de test';

  // Encoder/Décoder
  Encoded := CompresserMessage(Original);
  Decoded := DecompresserMessage(Encoded);

  AssertEquals('Le message doit être identique', Original, Decoded);
  AssertTrue('La compression doit réduire la taille',
            Length(Encoded) < Length(Original));
end;

procedure TTestP2P.TestBlockchain;
var
  Blockchain: TBlockchain;
begin
  Blockchain := TBlockchain.Create(2);  // Difficulté faible pour tests
  try
    Blockchain.AddBlock('Test 1');
    Blockchain.AddBlock('Test 2');

    AssertTrue('La blockchain doit être valide', Blockchain.IsValid);
    AssertEquals('Nombre de blocs', 3, Blockchain.Chain.Count);  // Genesis + 2
  finally
    Blockchain.Free;
  end;
end;

initialization
  RegisterTest(TTestP2P);

end.
```

## Conclusion et recommandations

### Récapitulatif

Dans ce tutoriel, nous avons couvert :

1. **Introduction au P2P** - Concepts, avantages, inconvénients
2. **Types de réseaux** - Pur, hybride, structuré (DHT), super-peers
3. **Concepts fondamentaux** - Peer ID, découverte, NAT traversal
4. **Implémentation** - Réseau P2P simple en Pascal
5. **BitTorrent** - Protocole populaire simplifié
6. **DHT et Kademlia** - Tables de hachage distribuées
7. **Blockchain** - Registre distribué
8. **Consensus** - PoW, PoS, Raft
9. **Sécurité** - Attaques et défenses
10. **Applications pratiques** - Chat, partage de fichiers, vote

### Avantages du P2P

✅ **Décentralisation** - Pas de point unique de défaillance  
✅ **Scalabilité** - Performance augmente avec le nombre de pairs  
✅ **Résilience** - Résistant à la censure et aux pannes  
✅ **Économie** - Pas besoin de serveurs coûteux  
✅ **Confidentialité** - Données distribuées, pas centralisées

### Inconvénients et défis

❌ **Complexité** - Plus difficile à implémenter que client-serveur  
❌ **Sécurité** - Chaque pair est une surface d'attaque  
❌ **NAT/Firewall** - Problèmes de connectivité  
❌ **Consistance** - Difficile de maintenir un état cohérent  
❌ **Découverte** - Trouver les pairs n'est pas trivial

### Quand utiliser le P2P ?

**✅ Utilisez P2P quand :**
- Décentralisation est prioritaire
- Coûts serveur sont un problème
- Résilience est critique
- Bande passante peut être distribuée
- Censure est une menace

**❌ Préférez client-serveur quand :**
- Contrôle centralisé nécessaire
- Sécurité stricte requise
- Simplicité prioritaire
- État cohérent critique
- Ressources clients limitées

### Recommandations pour FreePascal

### Meilleures pratiques

**1. Utilisez des bibliothèques éprouvées**
   - mORMot pour la sérialisation
   - Synapse pour les sockets
   - Indy pour le réseau

**2. Gérez correctement les threads**
   - Un thread par pair pour la réception
   - Thread pool pour le traitement
   - Synchronisation avec TCriticalSection

**3. Implémentez le NAT traversal**
   - STUN pour découvrir l'IP publique
   - UPnP pour ouvrir les ports
   - Hole punching en dernier recours

**4. Sécurisez les communications**
   - Chiffrement bout-en-bout
   - Authentification des pairs
   - Validation des données

**5. Testez extensivement**
   - Tests unitaires pour les protocoles
   - Tests de charge pour la scalabilité
   - Tests réseau avec latence simulée
   - Tests multi-plateformes (Windows/Linux)

### Architecture avancée modulaire

```pascal
unit P2PStack;

{$mode objfpc}{$H+}

interface

type
  // Couche transport
  ITransportLayer = interface
    ['{11111111-2222-3333-4444-555555555555}']
    function Connect(const Address: String; Port: Word): Boolean;
    procedure Send(const Data: TBytes);
    function Receive(out Data: TBytes): Boolean;
    procedure Disconnect;
  end;

  // Couche découverte
  IDiscoveryLayer = interface
    ['{22222222-3333-4444-5555-666666666666}']
    procedure Bootstrap(const Seeds: array of String);
    function FindPeers(Count: Integer): TArray<TPeerInfo>;
    procedure Announce(const InfoHash: String);
  end;

  // Couche protocole
  IProtocolLayer = interface
    ['{33333333-4444-5555-6666-777777777777}']
    function EncodeMessage(const Msg: TMessage): TBytes;
    function DecodeMessage(const Data: TBytes): TMessage;
    function ValidateMessage(const Msg: TMessage): Boolean;
  end;

  // Couche application
  IApplicationLayer = interface
    ['{44444444-5555-6666-7777-888888888888}']
    procedure OnPeerConnected(const PeerID: String);
    procedure OnPeerDisconnected(const PeerID: String);
    procedure OnDataReceived(const PeerID: String; const Data: TBytes);
  end;

  // Stack P2P complet
  TP2PStack = class
  private
    FTransport: ITransportLayer;
    FDiscovery: IDiscoveryLayer;
    FProtocol: IProtocolLayer;
    FApplication: IApplicationLayer;
  public
    constructor Create(Transport: ITransportLayer;
                      Discovery: IDiscoveryLayer;
                      Protocol: IProtocolLayer;
                      Application: IApplicationLayer);

    procedure Start;
    procedure Stop;

    property Transport: ITransportLayer read FTransport;
    property Discovery: IDiscoveryLayer read FDiscovery;
    property Protocol: IProtocolLayer read FProtocol;
    property Application: IApplicationLayer read FApplication;
  end;

implementation

constructor TP2PStack.Create(Transport: ITransportLayer;
                            Discovery: IDiscoveryLayer;
                            Protocol: IProtocolLayer;
                            Application: IApplicationLayer);
begin
  inherited Create;
  FTransport := Transport;
  FDiscovery := Discovery;
  FProtocol := Protocol;
  FApplication := Application;
end;

procedure TP2PStack.Start;
begin
  WriteLn('Démarrage du stack P2P...');

  // Initialiser les couches dans l'ordre
  FDiscovery.Bootstrap(BOOTSTRAP_NODES);
  FTransport.Connect(/* ... */);

  WriteLn('Stack P2P démarré');
end;

procedure TP2PStack.Stop;
begin
  WriteLn('Arrêt du stack P2P...');
  FTransport.Disconnect;
  WriteLn('Stack P2P arrêté');
end;

end.
```

## Métriques et monitoring

### Tableau de bord des métriques

```pascal
unit P2PMetrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TP2PMetrics = class
  private
    FLock: TCriticalSection;
    FPeerCount: Integer;
    FMessagesSent: Int64;
    FMessagesReceived: Int64;
    FBytesUploaded: Int64;
    FBytesDownloaded: Int64;
    FConnectionAttempts: Integer;
    FFailedConnections: Integer;
    FStartTime: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    procedure IncrementPeerCount;
    procedure DecrementPeerCount;
    procedure RecordMessageSent(Size: Integer);
    procedure RecordMessageReceived(Size: Integer);
    procedure RecordConnectionAttempt(Success: Boolean);

    function GetStats: String;
    function GetUptime: String;
    function GetUploadRate: Double;
    function GetDownloadRate: Double;
  end;

implementation

constructor TP2PMetrics.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FStartTime := Now;
end;

destructor TP2PMetrics.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TP2PMetrics.IncrementPeerCount;
begin
  FLock.Enter;
  try
    Inc(FPeerCount);
  finally
    FLock.Leave;
  end;
end;

procedure TP2PMetrics.DecrementPeerCount;
begin
  FLock.Enter;
  try
    Dec(FPeerCount);
  finally
    FLock.Leave;
  end;
end;

procedure TP2PMetrics.RecordMessageSent(Size: Integer);
begin
  FLock.Enter;
  try
    Inc(FMessagesSent);
    Inc(FBytesUploaded, Size);
  finally
    FLock.Leave;
  end;
end;

procedure TP2PMetrics.RecordMessageReceived(Size: Integer);
begin
  FLock.Enter;
  try
    Inc(FMessagesReceived);
    Inc(FBytesDownloaded, Size);
  finally
    FLock.Leave;
  end;
end;

procedure TP2PMetrics.RecordConnectionAttempt(Success: Boolean);
begin
  FLock.Enter;
  try
    Inc(FConnectionAttempts);
    if not Success then
      Inc(FFailedConnections);
  finally
    FLock.Leave;
  end;
end;

function TP2PMetrics.GetStats: String;
begin
  FLock.Enter;
  try
    Result := Format(
      '=== Statistiques P2P ===' + sLineBreak +
      'Pairs connectés : %d' + sLineBreak +
      'Messages envoyés : %d' + sLineBreak +
      'Messages reçus : %d' + sLineBreak +
      'Upload : %s' + sLineBreak +
      'Download : %s' + sLineBreak +
      'Taux upload : %.2f KB/s' + sLineBreak +
      'Taux download : %.2f KB/s' + sLineBreak +
      'Tentatives connexion : %d' + sLineBreak +
      'Échecs connexion : %d (%.1f%%)' + sLineBreak +
      'Uptime : %s',
      [FPeerCount,
       FMessagesSent,
       FMessagesReceived,
       FormatBytes(FBytesUploaded),
       FormatBytes(FBytesDownloaded),
       GetUploadRate,
       GetDownloadRate,
       FConnectionAttempts,
       FFailedConnections,
       (FFailedConnections * 100.0 / Max(1, FConnectionAttempts)),
       GetUptime]
    );
  finally
    FLock.Leave;
  end;
end;

function TP2PMetrics.GetUptime: String;
var
  Duration: TDateTime;
  Days, Hours, Minutes: Integer;
begin
  Duration := Now - FStartTime;
  Days := Trunc(Duration);
  Hours := HourOf(Duration);
  Minutes := MinuteOf(Duration);

  if Days > 0 then
    Result := Format('%d jours, %d heures, %d minutes', [Days, Hours, Minutes])
  else if Hours > 0 then
    Result := Format('%d heures, %d minutes', [Hours, Minutes])
  else
    Result := Format('%d minutes', [Minutes]);
end;

function TP2PMetrics.GetUploadRate: Double;
var
  Seconds: Double;
begin
  Seconds := SecondsBetween(Now, FStartTime);
  if Seconds > 0 then
    Result := (FBytesUploaded / 1024) / Seconds
  else
    Result := 0;
end;

function TP2PMetrics.GetDownloadRate: Double;
var
  Seconds: Double;
begin
  Seconds := SecondsBetween(Now, FStartTime);
  if Seconds > 0 then
    Result := (FBytesDownloaded / 1024) / Seconds
  else
    Result := 0;
end;

function FormatBytes(Bytes: Int64): String;
begin
  if Bytes < 1024 then
    Result := Format('%d B', [Bytes])
  else if Bytes < 1024 * 1024 then
    Result := Format('%.2f KB', [Bytes / 1024])
  else if Bytes < 1024 * 1024 * 1024 then
    Result := Format('%.2f MB', [Bytes / (1024 * 1024)])
  else
    Result := Format('%.2f GB', [Bytes / (1024 * 1024 * 1024)]);
end;

end.
```

## Déploiement en production

### Configuration pour la production

```pascal
unit P2PConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TP2PConfig = class
  private
    FIni: TIniFile;
    FListenPort: Word;
    FMaxPeers: Integer;
    FBootstrapNodes: TStringList;
    FDataDirectory: String;
    FEnableUPnP: Boolean;
    FEnableEncryption: Boolean;
    FLogLevel: String;
  public
    constructor Create(const ConfigFile: String);
    destructor Destroy; override;

    procedure SaveDefault;

    property ListenPort: Word read FListenPort;
    property MaxPeers: Integer read FMaxPeers;
    property BootstrapNodes: TStringList read FBootstrapNodes;
    property DataDirectory: String read FDataDirectory;
    property EnableUPnP: Boolean read FEnableUPnP;
    property EnableEncryption: Boolean read FEnableEncryption;
    property LogLevel: String read FLogLevel;
  end;

implementation

constructor TP2PConfig.Create(const ConfigFile: String);
var
  i: Integer;
  NodeList: String;
begin
  inherited Create;

  FBootstrapNodes := TStringList.Create;

  if FileExists(ConfigFile) then
  begin
    FIni := TIniFile.Create(ConfigFile);

    FListenPort := FIni.ReadInteger('Network', 'ListenPort', 6881);
    FMaxPeers := FIni.ReadInteger('Network', 'MaxPeers', 50);
    FEnableUPnP := FIni.ReadBool('Network', 'EnableUPnP', True);

    FDataDirectory := FIni.ReadString('Storage', 'DataDirectory', 'data');

    FEnableEncryption := FIni.ReadBool('Security', 'EnableEncryption', True);

    FLogLevel := FIni.ReadString('Logging', 'LogLevel', 'INFO');

    NodeList := FIni.ReadString('Bootstrap', 'Nodes', '');
    FBootstrapNodes.CommaText := NodeList;
  end
  else
  begin
    // Valeurs par défaut
    FListenPort := 6881;
    FMaxPeers := 50;
    FEnableUPnP := True;
    FDataDirectory := 'data';
    FEnableEncryption := True;
    FLogLevel := 'INFO';

    FBootstrapNodes.Add('bootstrap1.example.com:6881');
    FBootstrapNodes.Add('bootstrap2.example.com:6881');

    SaveDefault;
  end;
end;

destructor TP2PConfig.Destroy;
begin
  FIni.Free;
  FBootstrapNodes.Free;
  inherited Destroy;
end;

procedure TP2PConfig.SaveDefault;
begin
  FIni := TIniFile.Create('p2p.ini');
  try
    FIni.WriteInteger('Network', 'ListenPort', FListenPort);
    FIni.WriteInteger('Network', 'MaxPeers', FMaxPeers);
    FIni.WriteBool('Network', 'EnableUPnP', FEnableUPnP);

    FIni.WriteString('Storage', 'DataDirectory', FDataDirectory);

    FIni.WriteBool('Security', 'EnableEncryption', FEnableEncryption);

    FIni.WriteString('Logging', 'LogLevel', FLogLevel);

    FIni.WriteString('Bootstrap', 'Nodes', FBootstrapNodes.CommaText);

    FIni.UpdateFile;
  finally
    FIni.Free;
  end;
end;

end.
```

### Service système

**Fichier systemd (Linux) - `/etc/systemd/system/p2pnode.service` :**

```ini
[Unit]
Description=P2P Node Service
After=network.target

[Service]
Type=simple
User=p2puser
Group=p2puser
WorkingDirectory=/opt/p2pnode
ExecStart=/opt/p2pnode/p2pnode
Restart=always
RestartSec=10

# Sécurité
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=/opt/p2pnode/data /var/log/p2pnode

# Limites de ressources
LimitNOFILE=65536
MemoryLimit=1G

[Install]
WantedBy=multi-user.target
```

**Installation et gestion :**

```bash
# Copier l'exécutable
sudo cp p2pnode /opt/p2pnode/
sudo chown -R p2puser:p2puser /opt/p2pnode

# Activer et démarrer le service
sudo systemctl enable p2pnode
sudo systemctl start p2pnode

# Vérifier le statut
sudo systemctl status p2pnode

# Voir les logs
sudo journalctl -u p2pnode -f
```

**Service Windows :**

```pascal
program P2PNodeService;

{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

uses
  {$IFDEF MSWINDOWS}
  Windows, ServiceManager,
  {$ENDIF}
  SysUtils, P2PNode;

{$IFDEF MSWINDOWS}
type
  TP2PService = class(TService)
  private
    FNode: TP2PNode;
  protected
    function DoStart: Boolean; override;
    function DoStop: Boolean; override;
  end;

function TP2PService.DoStart: Boolean;
begin
  FNode := TP2PNode.Create;
  FNode.Start;
  Result := True;
end;

function TP2PService.DoStop: Boolean;
begin
  FNode.Stop;
  FNode.Free;
  Result := True;
end;
{$ENDIF}

begin
  {$IFDEF MSWINDOWS}
  if ParamCount > 0 then
  begin
    if ParamStr(1) = '/install' then
    begin
      InstallService('P2PNode', 'P2P Node Service',
                    ParamStr(0), SERVICE_AUTO_START);
      WriteLn('Service installé');
    end
    else if ParamStr(1) = '/uninstall' then
    begin
      UninstallService('P2PNode');
      WriteLn('Service désinstallé');
    end;
  end
  else
    RunService(TP2PService);
  {$ELSE}
  // Mode console sous Linux
  WriteLn('Mode console - Ctrl+C pour arrêter');
  // Lancer le node...
  {$ENDIF}
end.
```

## Cas d'usage avancés

### 1. CDN décentralisé

```pascal
type
  TDecentralizedCDN = class
  private
    FNetwork: TP2PNetwork;
    FCache: TDictionary<String, TBytes>;
  public
    procedure PublishContent(const URL: String; const Content: TBytes);
    function RetrieveContent(const URL: String): TBytes;
    procedure InvalidateCache(const URL: String);
  end;

procedure TDecentralizedCDN.PublishContent(const URL: String; const Content: TBytes);
var
  Hash: String;
  Peers: TArray<TPeer>;
  i: Integer;
begin
  // Calculer le hash du contenu
  Hash := SHA256DigestToString(SHA256(Content));

  // Stocker localement
  FCache.AddOrSetValue(URL, Content);

  // Distribuer aux pairs
  Peers := FNetwork.GetRandomPeers(10);
  for i := 0 to High(Peers) do
  begin
    // Envoyer une copie à chaque pair
    SendCDNContent(Peers[i], URL, Hash, Content);
  end;

  WriteLn('Contenu publié : ', URL, ' (', Length(Content), ' octets)');
end;

function TDecentralizedCDN.RetrieveContent(const URL: String): TBytes;
begin
  // Vérifier le cache local
  if FCache.TryGetValue(URL, Result) then
  begin
    WriteLn('Cache hit : ', URL);
    Exit;
  end;

  // Demander aux pairs
  WriteLn('Cache miss : ', URL, ' - demande aux pairs');
  Result := RequestFromPeers(URL);

  // Mettre en cache
  if Length(Result) > 0 then
    FCache.Add(URL, Result);
end;
```

### 2. Base de données distribuée

```pascal
type
  TDistributedDB = class
  private
    FNetwork: TP2PNetwork;
    FLocalStore: TDictionary<String, String>;
    FDHT: TKademliaDHT;
  public
    procedure Put(const Key, Value: String);
    function Get(const Key: String): String;
    procedure Delete(const Key: String);
    function Query(const Condition: String): TArray<TKeyValue>;
  end;

procedure TDistributedDB.Put(const Key, Value: String);
var
  Hash: TPeerID;
  Nodes: TArray<TNode>;
  i: Integer;
begin
  // Hasher la clé
  Hash := SHA1(Key);

  // Trouver les nodes responsables (les 3 plus proches)
  Nodes := FDHT.FindClosestNodes(Hash, 3);

  // Stocker localement
  FLocalStore.AddOrSetValue(Key, Value);

  // Répliquer
  for i := 0 to High(Nodes) do
    SendStoreRequest(Nodes[i], Key, Value);

  WriteLn('Donnée stockée : ', Key);
end;

function TDistributedDB.Get(const Key: String): String;
var
  Hash: TPeerID;
  Nodes: TArray<TNode>;
  i: Integer;
begin
  // Vérifier le stockage local
  if FLocalStore.TryGetValue(Key, Result) then
    Exit;

  // Chercher dans la DHT
  Hash := SHA1(Key);
  Nodes := FDHT.FindClosestNodes(Hash, 3);

  for i := 0 to High(Nodes) do
  begin
    Result := SendGetRequest(Nodes[i], Key);
    if Result <> '' then
    begin
      // Mettre en cache localement
      FLocalStore.Add(Key, Result);
      Exit;
    end;
  end;

  Result := '';  // Non trouvé
end;
```

### 3. Système de messagerie décentralisée

```pascal
type
  TDecentralizedMessaging = class
  private
    FNetwork: TP2PNetwork;
    FInbox: TObjectList;
    FMyKeys: TKeyPair;
  public
    procedure SendMessage(const RecipientPubKey: String; const Message: String);
    function ReceiveMessages: TArray<TMessage>;
    procedure BroadcastMessage(const Message: String);
  end;

procedure TDecentralizedMessaging.SendMessage(const RecipientPubKey: String;
                                              const Message: String);
var
  Encrypted: TBytes;
  Envelope: TMessageEnvelope;
begin
  // Chiffrer le message avec la clé publique du destinataire
  Encrypted := RSAEncrypt(Message, RecipientPubKey);

  // Créer l'enveloppe
  Envelope.Sender := MyPublicKey;
  Envelope.Recipient := RecipientPubKey;
  Envelope.Timestamp := Now;
  Envelope.EncryptedData := Encrypted;
  Envelope.Signature := SignMessage(Envelope, FMyKeys.PrivateKey);

  // Router vers le destinataire via la DHT
  RouteMessage(Envelope);

  WriteLn('Message envoyé à ', Copy(RecipientPubKey, 1, 8), '...');
end;

function TDecentralizedMessaging.ReceiveMessages: TArray<TMessage>;
var
  i: Integer;
  Envelope: TMessageEnvelope;
  Decrypted: String;
begin
  SetLength(Result, FInbox.Count);

  for i := 0 to FInbox.Count - 1 do
  begin
    Envelope := TMessageEnvelope(FInbox[i]);

    // Vérifier la signature
    if not VerifySignature(Envelope) then
      Continue;

    // Déchiffrer
    Decrypted := RSADecrypt(Envelope.EncryptedData, FMyKeys.PrivateKey);

    Result[i].Sender := Envelope.Sender;
    Result[i].Timestamp := Envelope.Timestamp;
    Result[i].Content := Decrypted;
  end;

  FInbox.Clear;
end;
```

## Ressources complémentaires

### Livres essentiels

**Fondamentaux P2P :**
1. "Peer-to-Peer Systems and Applications" - Ralf Steinmetz & Klaus Wehrle
2. "Designing Data-Intensive Applications" - Martin Kleppmann
3. "Distributed Systems" - Andrew S. Tanenbaum

**Blockchain et cryptomonnaies :**
1. "Mastering Bitcoin" - Andreas M. Antonopoulos
2. "Mastering Ethereum" - Andreas M. Antonopoulos & Gavin Wood
3. "The Bitcoin Standard" - Saifedean Ammous

**Sécurité :**
1. "Applied Cryptography" - Bruce Schneier
2. "Cryptography Engineering" - Ferguson, Schneier & Kohno

### Sites web et communautés

**Documentation officielle :**
- BitTorrent Protocol : http://www.bittorrent.org/beps/bep_0003.html
- Kademlia Paper : https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf
- IPFS Docs : https://docs.ipfs.io/
- Ethereum : https://ethereum.org/en/developers/docs/

**Forums et communautés :**
- Reddit r/P2P
- Stack Overflow tag `peer-to-peer`
- GitHub topics : p2p, distributed-systems
- Lazarus/FreePascal Forum : https://forum.lazarus.freepascal.org/

### Outils de développement

**Analyse réseau :**
- Wireshark - Capture et analyse de paquets
- tcpdump - Ligne de commande
- netstat - Connexions actives

**Simulation :**
- Mininet - Simulation de réseaux
- Docker/Podman - Conteneurs pour tests
- VirtualBox/QEMU - Machines virtuelles

**Monitoring :**
- Prometheus + Grafana - Métriques
- ELK Stack - Logs centralisés
- Nagios/Zabbix - Surveillance

## Exercices suggérés pour approfondir

1. **Implémenter un tracker BitTorrent simple**
   - Gérer l'enregistrement des pairs
   - Retourner la liste des pairs pour un torrent
   - Implémenter les statistiques

2. **Créer une DHT Kademlia fonctionnelle**
   - Table de routage avec k-buckets
   - Recherche itérative
   - Stockage et récupération de valeurs

3. **Développer un chat P2P chiffré**
   - Échange de clés Diffie-Hellman
   - Chiffrement bout-en-bout
   - Groupes de discussion

4. **Construire une blockchain simple**
   - Proof of Work
   - Validation de chaîne
   - Consensus distribué

5. **Implémenter le NAT traversal**
   - Client STUN
   - UPnP IGD
   - Hole punching UDP

## Conclusion finale

### Points clés à retenir

Le développement P2P avec FreePascal/Lazarus offre :

✅ **Performance** - Pas de goulot d'étranglement serveur  
✅ **Résilience** - Tolérance aux pannes  
✅ **Scalabilité** - Croissance organique  
✅ **Économie** - Infrastructure minimale  
✅ **Liberté** - Décentralisation et autonomie

### Défis à relever

❌ **Complexité technique** - Architecture plus difficile  
❌ **NAT/Firewalls** - Connectivité problématique  
❌ **Sécurité** - Surface d'attaque distribuée  
❌ **Consistance** - État synchronisé difficile  
❌ **Découverte** - Bootstrap nécessaire

### L'avenir du P2P

Le P2P est plus pertinent que jamais avec :
- Web3 et blockchain
- IPFS et stockage décentralisé
- Applications résistantes à la censure
- IoT et edge computing
- 5G et bande passante accrue

### Mot de la fin

Les protocoles P2P et décentralisés représentent un paradigme fondamental en informatique distribuée. Bien que plus complexes à implémenter que les architectures client-serveur traditionnelles, ils offrent des avantages uniques en termes de résilience, scalabilité et décentralisation.

FreePascal/Lazarus, avec ses bibliothèques réseau robustes comme Synapse et mORMot, ses capacités multi-threading, et sa portabilité Windows/Linux, constitue une excellente plateforme pour développer des applications P2P professionnelles.

**Conseil final :** Commencez par des protocoles simples (chat P2P), comprenez bien les fondamentaux (découverte, NAT traversal, sécurité), puis progressez vers des architectures plus complexes (DHT, blockchain). La maîtrise du P2P ouvre les portes vers des systèmes véritablement décentralisés et résilients.

Bon développement P2P avec FreePascal ! 🌐🔗

⏭️ [Firewall et configuration réseau par OS](/10-programmation-reseau-avancee/12-firewall-configuration-reseau-par-os.md)
