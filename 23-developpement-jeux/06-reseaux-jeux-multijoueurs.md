🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.6 Réseaux pour jeux multijoueurs

## Introduction au multijoueur en réseau

Le développement de jeux multijoueurs en réseau est l'un des domaines les plus excitants et les plus complexes du game development. Permettre à plusieurs joueurs de jouer ensemble, qu'ils soient dans la même pièce ou à l'autre bout du monde, transforme complètement l'expérience de jeu.

### Types de jeux multijoueurs

**Local (même machine)** :
- Split-screen : Plusieurs joueurs sur un écran divisé
- Hot-seat : Les joueurs jouent à tour de rôle
- Pas de réseau nécessaire

**En réseau local (LAN)** :
- Joueurs sur le même réseau local
- Latence très faible (< 5ms)
- Pas besoin de serveur internet
- Idéal pour les LAN parties

**En ligne (Internet)** :
- Joueurs partout dans le monde
- Latence variable (20-200ms+)
- Nécessite des serveurs ou P2P
- Défis techniques importants

### Architectures réseau

#### Architecture Client-Serveur

```
        Serveur
          |
    ----------------
    |      |       |
Client1 Client2 Client3
```

**Avantages** :
- Autorité centrale (anti-triche)
- État du jeu cohérent
- Facile à synchroniser

**Inconvénients** :
- Coût des serveurs
- Point de défaillance unique
- Latence serveur → client

#### Architecture Peer-to-Peer (P2P)

```
Client1 ←→ Client2
   ↕          ↕
Client3 ←→ Client4
```

**Avantages** :
- Pas de serveur nécessaire
- Latence directe entre joueurs
- Gratuit (pas de coûts serveur)

**Inconvénients** :
- Difficile à synchroniser
- Vulnérable à la triche
- Problème du "host advantage"

### Protocoles réseau

#### TCP vs UDP

**TCP (Transmission Control Protocol)** :
- ✅ Fiable : Tous les paquets arrivent dans l'ordre
- ✅ Gère la retransmission automatique
- ❌ Plus lent
- ❌ Head-of-line blocking

**UDP (User Datagram Protocol)** :
- ✅ Rapide et léger
- ✅ Pas de garantie de livraison
- ✅ Idéal pour jeux en temps réel
- ❌ Paquets peuvent être perdus ou arriver dans le désordre

**Recommandation** :
- **Jeux en temps réel** (FPS, racing) → UDP
- **Jeux au tour par tour** → TCP
- **Chat, lobby** → TCP
- **Positions des joueurs** → UDP

## Bibliothèques réseau pour FreePascal

### Synapse - Bibliothèque complète

```pascal
uses
  blcksock, synsock;

type
  TGameServer = class
  private
    FSocket: TTCPBlockSocket;
  public
    constructor Create(Port: Integer);
    procedure Listen;
    procedure SendData(const Data: string);
    function ReceiveData: string;
  end;

constructor TGameServer.Create(Port: Integer);
begin
  FSocket := TTCPBlockSocket.Create;
  FSocket.CreateSocket;
  FSocket.SetLinger(True, 10);
  FSocket.Bind('0.0.0.0', IntToStr(Port));
  FSocket.Listen;
end;

procedure TGameServer.Listen;
var
  ClientSocket: TSocket;
begin
  WriteLn('Serveur en écoute sur le port...');

  ClientSocket := FSocket.Accept;

  if FSocket.LastError = 0 then
    WriteLn('Client connecté !')
  else
    WriteLn('Erreur : ', FSocket.LastErrorDesc);
end;

function TGameServer.ReceiveData: string;
begin
  Result := FSocket.RecvPacket(5000); // Timeout 5 secondes

  if FSocket.LastError <> 0 then
    WriteLn('Erreur réception : ', FSocket.LastErrorDesc);
end;

procedure TGameServer.SendData(const Data: string);
begin
  FSocket.SendString(Data + #13#10);
end;
```

### lNet - Alternative moderne

```pascal
uses
  lNet, lNetComponents;

type
  TGameClient = class
  private
    FTCP: TLTCPComponent;
    procedure OnConnect(aSocket: TLSocket);
    procedure OnReceive(aSocket: TLSocket);
    procedure OnError(const msg: string; aSocket: TLSocket);
  public
    constructor Create;
    procedure Connect(const Host: string; Port: Word);
    procedure SendMessage(const Msg: string);
  end;

constructor TGameClient.Create;
begin
  FTCP := TLTCPComponent.Create(nil);
  FTCP.OnConnect := @OnConnect;
  FTCP.OnReceive := @OnReceive;
  FTCP.OnError := @OnError;
end;

procedure TGameClient.OnConnect(aSocket: TLSocket);
begin
  WriteLn('Connecté au serveur !');
end;

procedure TGameClient.OnReceive(aSocket: TLSocket);
var
  Data: string;
begin
  if FTCP.GetMessage(Data) > 0 then
    WriteLn('Reçu : ', Data);
end;

procedure TGameClient.Connect(const Host: string; Port: Word);
begin
  FTCP.Connect(Host, Port);
end;

procedure TGameClient.SendMessage(const Msg: string);
begin
  FTCP.SendMessage(Msg + #13#10);
end;
```

## Concepts fondamentaux

### Sérialisation des données

Convertir les données du jeu en bytes pour l'envoi réseau :

```pascal
type
  TPlayerPosition = packed record
    PlayerID: Integer;
    X, Y, Z: Single;
    Rotation: Single;
    Timestamp: Cardinal;
  end;

// Sérialisation binaire
function SerializePosition(const Pos: TPlayerPosition): TBytes;
begin
  SetLength(Result, SizeOf(TPlayerPosition));
  Move(Pos, Result[0], SizeOf(TPlayerPosition));
end;

// Désérialisation
function DeserializePosition(const Data: TBytes): TPlayerPosition;
begin
  if Length(Data) >= SizeOf(TPlayerPosition) then
    Move(Data[0], Result, SizeOf(TPlayerPosition));
end;

// Sérialisation JSON (plus lisible, plus lourd)
uses
  fpjson, jsonparser;

function PositionToJSON(const Pos: TPlayerPosition): string;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.Add('playerID', Pos.PlayerID);
    JSON.Add('x', Pos.X);
    JSON.Add('y', Pos.Y);
    JSON.Add('z', Pos.Z);
    JSON.Add('rotation', Pos.Rotation);
    JSON.Add('timestamp', Integer(Pos.Timestamp));
    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

function JSONToPosition(const JSONStr: string): TPlayerPosition;
var
  JSON: TJSONObject;
begin
  JSON := GetJSON(JSONStr) as TJSONObject;
  try
    Result.PlayerID := JSON.Get('playerID', 0);
    Result.X := JSON.Get('x', 0.0);
    Result.Y := JSON.Get('y', 0.0);
    Result.Z := JSON.Get('z', 0.0);
    Result.Rotation := JSON.Get('rotation', 0.0);
    Result.Timestamp := Cardinal(JSON.Get('timestamp', 0));
  finally
    JSON.Free;
  end;
end;
```

### Types de messages

```pascal
type
  TMessageType = (
    mtPlayerJoin,      // Un joueur rejoint
    mtPlayerLeave,     // Un joueur quitte
    mtPlayerMove,      // Mouvement d'un joueur
    mtPlayerAction,    // Action (tir, saut, etc.)
    mtGameState,       // État complet du jeu
    mtChatMessage,     // Message de chat
    mtPing,           // Ping pour mesurer la latence
    mtPong            // Réponse au ping
  );

  TNetworkMessage = packed record
    MsgType: TMessageType;
    DataSize: Word;
    Data: array[0..1023] of Byte;
  end;

function CreateMessage(MsgType: TMessageType; const Data: TBytes): TNetworkMessage;
begin
  Result.MsgType := MsgType;
  Result.DataSize := Length(Data);

  if Result.DataSize > Length(Result.Data) then
    Result.DataSize := Length(Result.Data);

  Move(Data[0], Result.Data[0], Result.DataSize);
end;

procedure HandleMessage(const Msg: TNetworkMessage);
begin
  case Msg.MsgType of
    mtPlayerJoin:
      OnPlayerJoin(Msg);
    mtPlayerLeave:
      OnPlayerLeave(Msg);
    mtPlayerMove:
      OnPlayerMove(Msg);
    mtPlayerAction:
      OnPlayerAction(Msg);
    mtChatMessage:
      OnChatMessage(Msg);
  end;
end;
```

## Serveur de jeu

### Serveur TCP simple

```pascal
type
  TClient = class
    Socket: TTCPBlockSocket;
    PlayerID: Integer;
    PlayerName: string;
    LastPing: Cardinal;
  end;

  TGameServer = class
  private
    FListenSocket: TTCPBlockSocket;
    FClients: TList<TClient>;
    FRunning: Boolean;
    FNextPlayerID: Integer;
  public
    constructor Create(Port: Integer);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure BroadcastMessage(const Msg: TNetworkMessage);
    procedure HandleClient(Client: TClient);
  end;

constructor TGameServer.Create(Port: Integer);
begin
  FClients := TList<TClient>.Create;
  FNextPlayerID := 1;

  FListenSocket := TTCPBlockSocket.Create;
  FListenSocket.CreateSocket;
  FListenSocket.SetLinger(True, 10);
  FListenSocket.Bind('0.0.0.0', IntToStr(Port));
  FListenSocket.Listen;

  WriteLn('Serveur démarré sur le port ', Port);
end;

procedure TGameServer.Start;
var
  ClientSocket: TSocket;
  Client: TClient;
begin
  FRunning := True;

  while FRunning do
  begin
    // Accepter les nouvelles connexions (non-bloquant)
    if FListenSocket.CanRead(100) then
    begin
      ClientSocket := FListenSocket.Accept;

      if FListenSocket.LastError = 0 then
      begin
        Client := TClient.Create;
        Client.Socket := TTCPBlockSocket.Create;
        Client.Socket.Socket := ClientSocket;
        Client.PlayerID := FNextPlayerID;
        Inc(FNextPlayerID);

        FClients.Add(Client);

        WriteLn('Nouveau client connecté : ID ', Client.PlayerID);

        // Lancer un thread pour gérer ce client
        TThread.CreateAnonymousThread(procedure
        begin
          HandleClient(Client);
        end).Start;
      end;
    end;

    // Mettre à jour la logique du serveur
    UpdateGameLogic;

    Sleep(10);
  end;
end;

procedure TGameServer.HandleClient(Client: TClient);
var
  Data: string;
  Msg: TNetworkMessage;
begin
  while FRunning and (Client.Socket.LastError = 0) do
  begin
    // Lire les données du client
    Data := Client.Socket.RecvPacket(100);

    if Client.Socket.LastError = 0 then
    begin
      if Length(Data) > 0 then
      begin
        // Désérialiser le message
        Move(Data[1], Msg, Min(Length(Data), SizeOf(Msg)));

        // Traiter le message
        ProcessClientMessage(Client, Msg);
      end;
    end
    else
    begin
      WriteLn('Client ', Client.PlayerID, ' déconnecté');
      FClients.Remove(Client);
      Client.Free;
      Break;
    end;
  end;
end;

procedure TGameServer.BroadcastMessage(const Msg: TNetworkMessage);
var
  Client: TClient;
  Data: TBytes;
begin
  SetLength(Data, SizeOf(Msg));
  Move(Msg, Data[0], SizeOf(Msg));

  for Client in FClients do
  begin
    Client.Socket.SendBuffer(@Data[0], Length(Data));
  end;
end;
```

### Serveur UDP pour jeux rapides

```pascal
uses
  blcksock;

type
  TUDPGameServer = class
  private
    FSocket: TUDPBlockSocket;
    FClients: TDictionary<string, TClient>; // IP:Port → Client
  public
    constructor Create(Port: Integer);
    procedure Update;
    procedure SendToClient(const Address: string; const Msg: TNetworkMessage);
  end;

constructor TUDPGameServer.Create(Port: Integer);
begin
  FClients := TDictionary<string, TClient>.Create;

  FSocket := TUDPBlockSocket.Create;
  FSocket.CreateSocket;
  FSocket.Bind('0.0.0.0', IntToStr(Port));

  WriteLn('Serveur UDP démarré sur le port ', Port);
end;

procedure TUDPGameServer.Update;
var
  Data: string;
  RemoteIP: string;
  RemotePort: Integer;
  Msg: TNetworkMessage;
  ClientKey: string;
begin
  // Lire les données (non-bloquant)
  if FSocket.CanRead(0) then
  begin
    Data := FSocket.RecvPacket(1000);
    RemoteIP := FSocket.GetRemoteSinIP;
    RemotePort := FSocket.GetRemoteSinPort;

    if FSocket.LastError = 0 then
    begin
      ClientKey := RemoteIP + ':' + IntToStr(RemotePort);

      // Désérialiser le message
      if Length(Data) >= SizeOf(TMessageType) then
      begin
        Move(Data[1], Msg, Min(Length(Data), SizeOf(Msg)));

        // Traiter le message
        ProcessUDPMessage(ClientKey, Msg);
      end;
    end;
  end;
end;

procedure TUDPGameServer.SendToClient(const Address: string;
                                     const Msg: TNetworkMessage);
var
  IP: string;
  Port: Integer;
  Data: TBytes;
  ColonPos: Integer;
begin
  ColonPos := Pos(':', Address);
  if ColonPos > 0 then
  begin
    IP := Copy(Address, 1, ColonPos - 1);
    Port := StrToIntDef(Copy(Address, ColonPos + 1, Length(Address)), 0);

    SetLength(Data, SizeOf(Msg));
    Move(Msg, Data[0], SizeOf(Msg));

    FSocket.SendBufferTo(IP, Port, @Data[0], Length(Data));
  end;
end;
```

## Client de jeu

### Client TCP

```pascal
type
  TGameClient = class
  private
    FSocket: TTCPBlockSocket;
    FConnected: Boolean;
    FPlayerID: Integer;
    FReceiveThread: TThread;
  public
    constructor Create;
    destructor Destroy; override;
    function Connect(const Host: string; Port: Integer): Boolean;
    procedure Disconnect;
    procedure SendMessage(const Msg: TNetworkMessage);
    procedure Update;
  end;

constructor TGameClient.Create;
begin
  FSocket := TTCPBlockSocket.Create;
  FConnected := False;
end;

function TGameClient.Connect(const Host: string; Port: Integer): Boolean;
begin
  WriteLn('Connexion à ', Host, ':', Port);

  FSocket.CreateSocket;
  FSocket.Connect(Host, IntToStr(Port));

  Result := FSocket.LastError = 0;
  FConnected := Result;

  if Result then
  begin
    WriteLn('Connecté !');

    // Lancer le thread de réception
    FReceiveThread := TThread.CreateAnonymousThread(procedure
    var
      Data: string;
      Msg: TNetworkMessage;
    begin
      while FConnected do
      begin
        Data := FSocket.RecvPacket(100);

        if (FSocket.LastError = 0) and (Length(Data) > 0) then
        begin
          Move(Data[1], Msg, Min(Length(Data), SizeOf(Msg)));
          HandleServerMessage(Msg);
        end
        else if FSocket.LastError <> 0 then
        begin
          WriteLn('Connexion perdue');
          FConnected := False;
        end;
      end;
    end);

    FReceiveThread.Start;
  end
  else
    WriteLn('Erreur de connexion : ', FSocket.LastErrorDesc);
end;

procedure TGameClient.SendMessage(const Msg: TNetworkMessage);
var
  Data: TBytes;
begin
  if FConnected then
  begin
    SetLength(Data, SizeOf(Msg));
    Move(Msg, Data[0], SizeOf(Msg));

    FSocket.SendBuffer(@Data[0], Length(Data));
  end;
end;
```

### Client UDP

```pascal
type
  TUDPGameClient = class
  private
    FSocket: TUDPBlockSocket;
    FServerIP: string;
    FServerPort: Integer;
    FConnected: Boolean;
  public
    constructor Create;
    function Connect(const Host: string; Port: Integer): Boolean;
    procedure SendMessage(const Msg: TNetworkMessage);
    procedure Update;
  end;

constructor TUDPGameClient.Create;
begin
  FSocket := TUDPBlockSocket.Create;
  FSocket.CreateSocket;
  FSocket.Bind('0.0.0.0', '0'); // Port aléatoire
  FConnected := False;
end;

function TUDPGameClient.Connect(const Host: string; Port: Integer): Boolean;
begin
  FServerIP := Host;
  FServerPort := Port;
  FConnected := True;

  // Envoyer un message de connexion
  SendMessage(CreateMessage(mtPlayerJoin, []));

  Result := True;
end;

procedure TUDPGameClient.SendMessage(const Msg: TNetworkMessage);
var
  Data: TBytes;
begin
  if FConnected then
  begin
    SetLength(Data, SizeOf(Msg));
    Move(Msg, Data[0], SizeOf(Msg));

    FSocket.SendBufferTo(FServerIP, FServerPort, @Data[0], Length(Data));
  end;
end;

procedure TUDPGameClient.Update;
var
  Data: string;
  Msg: TNetworkMessage;
begin
  // Recevoir les messages du serveur
  while FSocket.CanRead(0) do
  begin
    Data := FSocket.RecvPacket(1000);

    if (FSocket.LastError = 0) and (Length(Data) > 0) then
    begin
      Move(Data[1], Msg, Min(Length(Data), SizeOf(Msg)));
      HandleServerMessage(Msg);
    end;
  end;
end;
```

## Synchronisation et prédiction

### Interpolation des positions

Pour des mouvements fluides malgré la latence :

```pascal
type
  TPositionSnapshot = record
    Position: TVector3;
    Rotation: Single;
    Timestamp: Cardinal;
  end;

  TNetworkPlayer = class
  private
    FSnapshots: array[0..9] of TPositionSnapshot;
    FSnapshotCount: Integer;
    FCurrentIndex: Integer;
  public
    procedure AddSnapshot(const Pos: TVector3; Rot: Single; Time: Cardinal);
    function GetInterpolatedPosition(CurrentTime: Cardinal): TVector3;
  end;

procedure TNetworkPlayer.AddSnapshot(const Pos: TVector3; Rot: Single;
                                    Time: Cardinal);
begin
  FCurrentIndex := (FCurrentIndex + 1) mod Length(FSnapshots);

  FSnapshots[FCurrentIndex].Position := Pos;
  FSnapshots[FCurrentIndex].Rotation := Rot;
  FSnapshots[FCurrentIndex].Timestamp := Time;

  if FSnapshotCount < Length(FSnapshots) then
    Inc(FSnapshotCount);
end;

function TNetworkPlayer.GetInterpolatedPosition(CurrentTime: Cardinal): TVector3;
var
  RenderTime: Cardinal;
  I, From, To_: Integer;
  Alpha: Single;
begin
  // Rendre 100ms dans le passé pour avoir du buffer
  RenderTime := CurrentTime - 100;

  // Trouver les deux snapshots encadrants
  From := -1;
  To_ := -1;

  for I := 0 to FSnapshotCount - 1 do
  begin
    if FSnapshots[I].Timestamp <= RenderTime then
      From := I
    else if FSnapshots[I].Timestamp > RenderTime then
    begin
      To_ := I;
      Break;
    end;
  end;

  // Interpoler
  if (From >= 0) and (To_ >= 0) then
  begin
    Alpha := (RenderTime - FSnapshots[From].Timestamp) /
             (FSnapshots[To_].Timestamp - FSnapshots[From].Timestamp);

    Result.X := Lerp(FSnapshots[From].Position.X,
                    FSnapshots[To_].Position.X, Alpha);
    Result.Y := Lerp(FSnapshots[From].Position.Y,
                    FSnapshots[To_].Position.Y, Alpha);
    Result.Z := Lerp(FSnapshots[From].Position.Z,
                    FSnapshots[To_].Position.Z, Alpha);
  end
  else if From >= 0 then
    Result := FSnapshots[From].Position
  else if FSnapshotCount > 0 then
    Result := FSnapshots[0].Position;
end;

function Lerp(A, B, T: Single): Single;
begin
  Result := A + (B - A) * T;
end;
```

### Prédiction côté client

Pour compenser la latence :

```pascal
type
  TClientPrediction = class
  private
    FPendingInputs: TQueue<TPlayerInput>;
    FLastAckedInput: Integer;
  public
    procedure SendInput(const Input: TPlayerInput);
    procedure OnServerUpdate(const State: TPlayerState; AckedInput: Integer);
    procedure PredictPosition(var Player: TPlayer);
  end;

procedure TClientPrediction.SendInput(const Input: TPlayerInput);
begin
  // Envoyer au serveur
  SendToServer(Input);

  // Garder en mémoire pour réconciliation
  FPendingInputs.Enqueue(Input);

  // Appliquer immédiatement côté client (prédiction)
  ApplyInput(LocalPlayer, Input);
end;

procedure TClientPrediction.OnServerUpdate(const State: TPlayerState;
                                          AckedInput: Integer);
var
  Input: TPlayerInput;
begin
  // Le serveur a traité jusqu'à AckedInput
  FLastAckedInput := AckedInput;

  // Supprimer les inputs confirmés
  while (FPendingInputs.Count > 0) and
        (FPendingInputs.Peek.SequenceNumber <= AckedInput) do
    FPendingInputs.Dequeue;

  // Remettre le joueur à la position confirmée par le serveur
  LocalPlayer.Position := State.Position;
  LocalPlayer.Velocity := State.Velocity;

  // Ré-appliquer les inputs non confirmés (réconciliation)
  for Input in FPendingInputs do
    ApplyInput(LocalPlayer, Input);
end;
```

## Gestion de la latence

### Mesure du ping

```pascal
type
  TPingManager = class
  private
    FPingSentTime: Cardinal;
    FCurrentPing: Integer;
    FPingHistory: array[0..9] of Integer;
    FPingIndex: Integer;
  public
    procedure SendPing;
    procedure OnPong;
    function GetAveragePing: Integer;
    function GetCurrentPing: Integer;
  end;

procedure TPingManager.SendPing;
var
  Msg: TNetworkMessage;
begin
  FPingSentTime := SDL_GetTicks;
  Msg := CreateMessage(mtPing, []);
  Client.SendMessage(Msg);
end;

procedure TPingManager.OnPong;
var
  Ping: Integer;
begin
  Ping := SDL_GetTicks - FPingSentTime;
  FCurrentPing := Ping;

  // Ajouter à l'historique
  FPingHistory[FPingIndex] := Ping;
  FPingIndex := (FPingIndex + 1) mod Length(FPingHistory);

  WriteLn('Ping : ', Ping, ' ms');
end;

function TPingManager.GetAveragePing: Integer;
var
  I, Sum: Integer;
begin
  Sum := 0;
  for I := 0 to High(FPingHistory) do
    Sum := Sum + FPingHistory[I];

  Result := Sum div Length(FPingHistory);
end;
```

### Compensation de lag (Lag compensation)

Pour les jeux de tir :

```pascal
type
  TLagCompensation = class
  private
    FPlayerHistory: array of array[0..99] of TPlayerSnapshot;
  public
    procedure RecordSnapshot(PlayerID: Integer; const Snapshot: TPlayerSnapshot);
    function GetPlayerPositionAt(PlayerID: Integer; Time: Cardinal): TVector3;
    function RewindAndRaycast(ShooterID: Integer; const Ray: TRay): THitResult;
  end;

function TLagCompensation.RewindAndRaycast(ShooterID: Integer;
                                          const Ray: TRay): THitResult;
var
  ShooterPing: Integer;
  RewindTime: Cardinal;
  I: Integer;
  PlayerPos: TVector3;
begin
  // Obtenir le ping du tireur
  ShooterPing := GetPlayerPing(ShooterID);

  // Rembobiner le temps de ping/2 (aller-retour)
  RewindTime := SDL_GetTicks - (ShooterPing div 2);

  // Replacer tous les joueurs à leur position d'il y a Ping/2 ms
  for I := 0 to Players.Count - 1 do
  begin
    if I <> ShooterID then
    begin
      PlayerPos := GetPlayerPositionAt(I, RewindTime);
      TempPlayers[I].Position := PlayerPos;
    end;
  end;

  // Effectuer le raycast sur les positions "remontées dans le temps"
  Result := PerformRaycast(Ray, TempPlayers);

  // Le serveur valide ainsi ce que le tireur voyait à son écran
end;
```

## Gestion des déconnexions

### Détection de timeout

```pascal
type
  TConnectionManager = class
  private
    FLastReceivedTime: array of Cardinal;
    FTimeout: Cardinal;
  public
    constructor Create(TimeoutMs: Cardinal);
    procedure UpdateLastReceived(PlayerID: Integer);
    function IsTimedOut(PlayerID: Integer): Boolean;
    procedure HandleDisconnect(PlayerID: Integer);
  end;

constructor TConnectionManager.Create(TimeoutMs: Cardinal);
begin
  FTimeout := TimeoutMs;
end;

procedure TConnectionManager.UpdateLastReceived(PlayerID: Integer);
begin
  if PlayerID < Length(FLastReceivedTime) then
    FLastReceivedTime[PlayerID] := SDL_GetTicks;
end;

function TConnectionManager.IsTimedOut(PlayerID: Integer): Boolean;
var
  TimeSinceLastMsg: Cardinal;
begin
  if PlayerID >= Length(FLastReceivedTime) then
  begin
    Result := True;
    Exit;
  end;

  TimeSinceLastMsg := SDL_GetTicks - FLastReceivedTime[PlayerID];
  Result := TimeSinceLastMsg > FTimeout;
end;

procedure TConnectionManager.HandleDisconnect(PlayerID: Integer);
var
  Msg: TNetworkMessage;
begin
  WriteLn('Joueur ', PlayerID, ' déconnecté (timeout)');

  // Notifier les autres joueurs
  Msg := CreatePlayerLeaveMessage(PlayerID);
  Server.BroadcastMessage(Msg);

  // Retirer le joueur du jeu
  RemovePlayer(PlayerID);
end;
```

### Reconnexion automatique

```pascal
type
  TReconnectManager = class
  private
    FReconnectAttempts: Integer;
    FMaxAttempts: Integer;
    FRetryDelay: Cardinal;
    FLastAttemptTime: Cardinal;
  public
    constructor Create(MaxAttempts: Integer; RetryDelayMs: Cardinal);
    procedure AttemptReconnect;
    function ShouldRetry: Boolean;
  end;

procedure TReconnectManager.AttemptReconnect;
begin
  if SDL_GetTicks - FLastAttemptTime < FRetryDelay then
    Exit;

  Inc(FReconnectAttempts);
  FLastAttemptTime := SDL_GetTicks;

  WriteLn('Tentative de reconnexion ', FReconnectAttempts, '/', FMaxAttempts);

  if Client.Connect(ServerHost, ServerPort) then
  begin
    WriteLn('Reconnexion réussie !');
    FReconnectAttempts := 0;
  end
  else if FReconnectAttempts >= FMaxAttempts then
  begin
    WriteLn('Impossible de se reconnecter');
    ShowDisconnectedMessage;
  end;
end;
```

## Anti-triche

### Validation serveur-side

Le principe d'or : **Ne jamais faire confiance au client**.

```pascal
type
  TServerValidator = class
  private
    FMaxSpeed: Single;
    FMaxJumpHeight: Single;
  public
    function ValidateMovement(PlayerID: Integer; const NewPos: TVector3): Boolean;
    function ValidateAction(PlayerID: Integer; Action: TGameAction): Boolean;
    function ValidateShotHit(ShooterID, TargetID: Integer): Boolean;
  end;

function TServerValidator.ValidateMovement(PlayerID: Integer;
                                          const NewPos: TVector3): Boolean;
var
  Player: TPlayer;
  OldPos: TVector3;
  Distance: Single;
  MaxDistance: Single;
  TimeDelta: Single;
begin
  Player := GetPlayer(PlayerID);
  OldPos := Player.Position;

  // Calculer la distance parcourue
  Distance := CalculateDistance(OldPos, NewPos);

  // Calculer la distance maximale possible
  TimeDelta := (SDL_GetTicks - Player.LastUpdateTime) / 1000.0;
  MaxDistance := FMaxSpeed * TimeDelta * 1.1; // 10% de marge

  Result := Distance <= MaxDistance;

  if not Result then
  begin
    WriteLn('Mouvement invalide du joueur ', PlayerID);
    WriteLn('Distance : ', Distance:0:2, ' Max : ', MaxDistance:0:2);

    // Corriger la position côté serveur
    SendPositionCorrection(PlayerID, OldPos);
  end;
end;

function TServerValidator.ValidateAction(PlayerID: Integer;
                                        Action: TGameAction): Boolean;
var
  Player: TPlayer;
  TimeSinceLastAction: Cardinal;
const
  MIN_ACTION_INTERVAL = 100; // ms entre actions
begin
  Player := GetPlayer(PlayerID);

  case Action of
    gaFire:
    begin
      TimeSinceLastAction := SDL_GetTicks - Player.LastFireTime;
      Result := TimeSinceLastAction >= Player.Weapon.FireRate;

      if Result then
        Player.LastFireTime := SDL_GetTicks
      else
        WriteLn('Tir trop rapide du joueur ', PlayerID);
    end;

    gaJump:
    begin
      Result := Player.IsOnGround;
      if not Result then
        WriteLn('Saut aérien invalide du joueur ', PlayerID);
    end;

    else
      Result := True;
  end;
end;

function TServerValidator.ValidateShotHit(ShooterID, TargetID: Integer): Boolean;
var
  Shooter, Target: TPlayer;
  Distance: Single;
  MaxRange: Single;
  Ray: TRay;
begin
  Shooter := GetPlayer(ShooterID);
  Target := GetPlayer(TargetID);

  // Vérifier la distance
  Distance := CalculateDistance(Shooter.Position, Target.Position);
  MaxRange := Shooter.Weapon.Range;

  if Distance > MaxRange then
  begin
    WriteLn('Tir hors de portée');
    Result := False;
    Exit;
  end;

  // Vérifier la ligne de vue avec lag compensation
  Ray := CreateRay(Shooter.Position, Shooter.AimDirection);
  Result := LagCompensation.RewindAndRaycast(ShooterID, Ray).HitPlayerID = TargetID;

  if not Result then
    WriteLn('Ligne de vue invalide');
end;
```

### Détection de patterns suspects

```pascal
type
  TCheatDetector = class
  private
    FPlayerStats: array of record
      Accuracy: Single;
      HeadshotRatio: Single;
      ReactionTimes: array[0..99] of Cardinal;
      SpeedViolations: Integer;
    end;
  public
    procedure RecordShot(PlayerID: Integer; Hit: Boolean; Headshot: Boolean);
    procedure RecordReactionTime(PlayerID: Integer; Time: Cardinal);
    function IsSuspicious(PlayerID: Integer): Boolean;
  end;

procedure TCheatDetector.RecordShot(PlayerID: Integer; Hit: Boolean;
                                   Headshot: Boolean);
var
  Stats: PPlayerStats;
begin
  Stats := @FPlayerStats[PlayerID];

  // Mettre à jour la précision
  if Hit then
    Stats.Accuracy := Stats.Accuracy * 0.95 + 0.05
  else
    Stats.Accuracy := Stats.Accuracy * 0.95;

  // Mettre à jour le ratio headshot
  if Hit and Headshot then
    Stats.HeadshotRatio := Stats.HeadshotRatio * 0.95 + 0.05
  else if Hit then
    Stats.HeadshotRatio := Stats.HeadshotRatio * 0.95;
end;

function TCheatDetector.IsSuspicious(PlayerID: Integer): Boolean;
var
  Stats: PPlayerStats;
  AvgReactionTime: Single;
  I, Sum: Integer;
begin
  Stats := @FPlayerStats[PlayerID];
  Result := False;

  // Précision surhumaine (> 95%)
  if Stats.Accuracy > 0.95 then
  begin
    WriteLn('Joueur ', PlayerID, ' : Précision suspecte ', Stats.Accuracy:0:2);
    Result := True;
  end;

  // Ratio headshot surhumain (> 80%)
  if Stats.HeadshotRatio > 0.80 then
  begin
    WriteLn('Joueur ', PlayerID, ' : Ratio headshot suspect ', Stats.HeadshotRatio:0:2);
    Result := True;
  end;

  // Temps de réaction trop rapide (< 100ms constant)
  Sum := 0;
  for I := 0 to High(Stats.ReactionTimes) do
    Sum := Sum + Stats.ReactionTimes[I];
  AvgReactionTime := Sum / Length(Stats.ReactionTimes);

  if AvgReactionTime < 100 then
  begin
    WriteLn('Joueur ', PlayerID, ' : Temps de réaction suspect ', AvgReactionTime:0:0, 'ms');
    Result := True;
  end;

  // Trop de violations de vitesse
  if Stats.SpeedViolations > 10 then
  begin
    WriteLn('Joueur ', PlayerID, ' : Trop de violations de vitesse');
    Result := True;
  end;
end;
```

### Cryptage et sécurité

```pascal
uses
  DCPcrypt2, DCPsha256, DCPrijndael;

type
  TSecureNetwork = class
  private
    FCipher: TDCP_rijndael;
    FKey: array[0..31] of Byte;
  public
    constructor Create(const Password: string);
    destructor Destroy; override;
    function Encrypt(const Data: TBytes): TBytes;
    function Decrypt(const Data: TBytes): TBytes;
    function ComputeHash(const Data: TBytes): string;
  end;

constructor TSecureNetwork.Create(const Password: string);
var
  Hash: TDCP_sha256;
begin
  // Générer une clé de 256 bits depuis le mot de passe
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Password);
    Hash.Final(FKey);
  finally
    Hash.Free;
  end;

  // Initialiser le cipher
  FCipher := TDCP_rijndael.Create(nil);
  FCipher.Init(FKey, SizeOf(FKey) * 8, nil);
end;

function TSecureNetwork.Encrypt(const Data: TBytes): TBytes;
begin
  SetLength(Result, Length(Data));
  if Length(Data) > 0 then
    FCipher.EncryptCBC(Data[0], Result[0], Length(Data));
end;

function TSecureNetwork.Decrypt(const Data: TBytes): TBytes;
begin
  SetLength(Result, Length(Data));
  if Length(Data) > 0 then
    FCipher.DecryptCBC(Data[0], Result[0], Length(Data));
end;

function TSecureNetwork.ComputeHash(const Data: TBytes): string;
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
  I: Integer;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.Update(Data[0], Length(Data));
    Hash.Final(Digest);

    Result := '';
    for I := 0 to High(Digest) do
      Result := Result + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

// Utilisation
procedure SendSecureMessage(const Msg: TNetworkMessage);
var
  Data, Encrypted: TBytes;
  Hash: string;
begin
  // Sérialiser
  SetLength(Data, SizeOf(Msg));
  Move(Msg, Data[0], SizeOf(Msg));

  // Calculer le hash
  Hash := SecureNet.ComputeHash(Data);

  // Crypter
  Encrypted := SecureNet.Encrypt(Data);

  // Envoyer : Hash + Encrypted Data
  SendToServer(Hash + '|' + BytesToString(Encrypted));
end;
```

## Optimisations réseau

### Compression des données

```pascal
uses
  zstream;

function CompressData(const Data: TBytes): TBytes;
var
  Input, Output: TMemoryStream;
  Compressor: TCompressionStream;
begin
  Input := TMemoryStream.Create;
  Output := TMemoryStream.Create;
  try
    Input.Write(Data[0], Length(Data));
    Input.Position := 0;

    Compressor := TCompressionStream.Create(clMax, Output);
    try
      Compressor.CopyFrom(Input, Input.Size);
    finally
      Compressor.Free;
    end;

    SetLength(Result, Output.Size);
    Output.Position := 0;
    Output.Read(Result[0], Output.Size);
  finally
    Input.Free;
    Output.Free;
  end;
end;

function DecompressData(const Data: TBytes): TBytes;
var
  Input, Output: TMemoryStream;
  Decompressor: TDecompressionStream;
begin
  Input := TMemoryStream.Create;
  Output := TMemoryStream.Create;
  try
    Input.Write(Data[0], Length(Data));
    Input.Position := 0;

    Decompressor := TDecompressionStream.Create(Input);
    try
      Output.CopyFrom(Decompressor, 0);
    finally
      Decompressor.Free;
    end;

    SetLength(Result, Output.Size);
    Output.Position := 0;
    Output.Read(Result[0], Output.Size);
  finally
    Input.Free;
    Output.Free;
  end;
end;
```

### Delta compression

Envoyer seulement ce qui a changé :

```pascal
type
  TPlayerDelta = record
    PlayerID: Integer;
    ChangedFields: Byte; // Bitmask : position, rotation, health, etc.
    NewPosition: TVector3;
    NewRotation: Single;
    NewHealth: Integer;
  end;

function CreateDeltaUpdate(const Old, New: TPlayer): TPlayerDelta;
const
  FLAG_POSITION = 1;
  FLAG_ROTATION = 2;
  FLAG_HEALTH = 4;
begin
  Result.PlayerID := New.ID;
  Result.ChangedFields := 0;

  // Position changée ?
  if (Old.Position.X <> New.Position.X) or
     (Old.Position.Y <> New.Position.Y) or
     (Old.Position.Z <> New.Position.Z) then
  begin
    Result.ChangedFields := Result.ChangedFields or FLAG_POSITION;
    Result.NewPosition := New.Position;
  end;

  // Rotation changée ?
  if Old.Rotation <> New.Rotation then
  begin
    Result.ChangedFields := Result.ChangedFields or FLAG_ROTATION;
    Result.NewRotation := New.Rotation;
  end;

  // Santé changée ?
  if Old.Health <> New.Health then
  begin
    Result.ChangedFields := Result.ChangedFields or FLAG_HEALTH;
    Result.NewHealth := New.Health;
  end;
end;

procedure ApplyDelta(var Player: TPlayer; const Delta: TPlayerDelta);
const
  FLAG_POSITION = 1;
  FLAG_ROTATION = 2;
  FLAG_HEALTH = 4;
begin
  if (Delta.ChangedFields and FLAG_POSITION) <> 0 then
    Player.Position := Delta.NewPosition;

  if (Delta.ChangedFields and FLAG_ROTATION) <> 0 then
    Player.Rotation := Delta.NewRotation;

  if (Delta.ChangedFields and FLAG_HEALTH) <> 0 then
    Player.Health := Delta.NewHealth;
end;
```

### Limiter la fréquence d'envoi (Throttling)

```pascal
type
  TUpdateThrottler = class
  private
    FLastUpdateTime: array of Cardinal;
    FUpdateRate: Cardinal; // ms entre updates
  public
    constructor Create(UpdateRateMs: Cardinal);
    function ShouldSendUpdate(PlayerID: Integer): Boolean;
    procedure MarkUpdated(PlayerID: Integer);
  end;

constructor TUpdateThrottler.Create(UpdateRateMs: Cardinal);
begin
  FUpdateRate := UpdateRateMs;
end;

function TUpdateThrottler.ShouldSendUpdate(PlayerID: Integer): Boolean;
var
  CurrentTime: Cardinal;
begin
  CurrentTime := SDL_GetTicks;

  if PlayerID >= Length(FLastUpdateTime) then
    SetLength(FLastUpdateTime, PlayerID + 1);

  Result := (CurrentTime - FLastUpdateTime[PlayerID]) >= FUpdateRate;
end;

procedure TUpdateThrottler.MarkUpdated(PlayerID: Integer);
begin
  if PlayerID >= Length(FLastUpdateTime) then
    SetLength(FLastUpdateTime, PlayerID + 1);

  FLastUpdateTime[PlayerID] := SDL_GetTicks;
end;

// Utilisation
var
  Throttler: TUpdateThrottler;

procedure BroadcastPlayerPositions;
var
  I: Integer;
  Player: TPlayer;
  Msg: TNetworkMessage;
begin
  for I := 0 to Players.Count - 1 do
  begin
    Player := Players[I];

    // Envoyer update seulement tous les 50ms
    if Throttler.ShouldSendUpdate(Player.ID) then
    begin
      Msg := CreatePositionUpdateMessage(Player);
      BroadcastMessage(Msg);
      Throttler.MarkUpdated(Player.ID);
    end;
  end;
end;
```

### Priorisation des messages

```pascal
type
  TMessagePriority = (mpCritical, mpHigh, mpNormal, mpLow);

  TPriorityQueue = class
  private
    FQueues: array[TMessagePriority] of TQueue<TNetworkMessage>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const Msg: TNetworkMessage; Priority: TMessagePriority);
    function Dequeue: TNetworkMessage;
    function HasMessages: Boolean;
  end;

constructor TPriorityQueue.Create;
var
  P: TMessagePriority;
begin
  for P := Low(TMessagePriority) to High(TMessagePriority) do
    FQueues[P] := TQueue<TNetworkMessage>.Create;
end;

function TPriorityQueue.Dequeue: TNetworkMessage;
var
  P: TMessagePriority;
begin
  // Traiter les priorités dans l'ordre
  for P := mpCritical to mpLow do
  begin
    if FQueues[P].Count > 0 then
    begin
      Result := FQueues[P].Dequeue;
      Exit;
    end;
  end;

  // Aucun message
  FillChar(Result, SizeOf(Result), 0);
end;

// Utilisation
procedure SendGameMessage(const Msg: TNetworkMessage);
var
  Priority: TMessagePriority;
begin
  case Msg.MsgType of
    mtPlayerJoin, mtPlayerLeave:
      Priority := mpCritical;
    mtPlayerAction, mtPlayerDamage:
      Priority := mpHigh;
    mtPlayerMove:
      Priority := mpNormal;
    mtChatMessage:
      Priority := mpLow;
    else
      Priority := mpNormal;
  end;

  MessageQueue.Enqueue(Msg, Priority);
end;
```

## Matchmaking et lobbies

### Serveur de matchmaking simple

```pascal
type
  TMatchmakingServer = class
  private
    FWaitingPlayers: TList<TPlayerInfo>;
    FActiveGames: TList<TGameSession>;
    FMinPlayers: Integer;
    FMaxPlayers: Integer;
  public
    constructor Create(MinPlayers, MaxPlayers: Integer);
    procedure AddPlayer(const Player: TPlayerInfo);
    procedure RemovePlayer(PlayerID: Integer);
    procedure Update;
  end;

procedure TMatchmakingServer.AddPlayer(const Player: TPlayerInfo);
begin
  WriteLn('Joueur ', Player.Name, ' en attente de partie');
  FWaitingPlayers.Add(Player);

  // Vérifier si on peut créer une partie
  if FWaitingPlayers.Count >= FMinPlayers then
    TryCreateGame;
end;

procedure TMatchmakingServer.TryCreateGame;
var
  Game: TGameSession;
  I: Integer;
begin
  if FWaitingPlayers.Count < FMinPlayers then
    Exit;

  Game := TGameSession.Create;

  // Ajouter les joueurs
  for I := 0 to Min(FMaxPlayers, FWaitingPlayers.Count) - 1 do
  begin
    Game.AddPlayer(FWaitingPlayers[0]);
    FWaitingPlayers.Delete(0);
  end;

  FActiveGames.Add(Game);

  WriteLn('Nouvelle partie créée avec ', Game.PlayerCount, ' joueurs');

  // Notifier les joueurs
  Game.Start;
end;
```

### Système de lobby

```pascal
type
  TGameLobby = class
  private
    FLobbyID: Integer;
    FHost: TPlayerInfo;
    FPlayers: TList<TPlayerInfo>;
    FMaxPlayers: Integer;
    FGameStarted: Boolean;
    FSettings: TGameSettings;
  public
    constructor Create(const Host: TPlayerInfo; MaxPlayers: Integer);
    function AddPlayer(const Player: TPlayerInfo): Boolean;
    procedure RemovePlayer(PlayerID: Integer);
    procedure UpdateSettings(const Settings: TGameSettings);
    procedure StartGame;
    procedure SendLobbyState;
  end;

function TGameLobby.AddPlayer(const Player: TPlayerInfo): Boolean;
begin
  Result := False;

  if FGameStarted then
  begin
    WriteLn('Partie déjà commencée');
    Exit;
  end;

  if FPlayers.Count >= FMaxPlayers then
  begin
    WriteLn('Lobby complet');
    Exit;
  end;

  FPlayers.Add(Player);
  WriteLn('Joueur ', Player.Name, ' a rejoint le lobby');

  // Notifier tous les joueurs
  SendLobbyState;

  Result := True;
end;

procedure TGameLobby.SendLobbyState;
var
  Msg: TNetworkMessage;
  Data: TBytes;
  I: Integer;
  Player: TPlayerInfo;
begin
  // Créer le message d'état du lobby
  // Format : [PlayerCount][Player1Info][Player2Info]...[Settings]

  // Envoyer à tous les joueurs du lobby
  for Player in FPlayers do
    SendToPlayer(Player.ID, Msg);
end;

procedure TGameLobby.StartGame;
begin
  if FPlayers.Count < 2 then
  begin
    WriteLn('Pas assez de joueurs pour commencer');
    Exit;
  end;

  FGameStarted := True;
  WriteLn('Démarrage de la partie !');

  // Créer la session de jeu
  CreateGameSession(FPlayers, FSettings);
end;
```

## NAT Traversal et P2P

### NAT Punchthrough avec serveur relais

```pascal
type
  TNATPunchthrough = class
  private
    FRelayServer: TGameClient;
  public
    function ConnectP2P(const PeerID: Integer): Boolean;
    procedure SendHolePunchPacket(const PeerAddress: string; Port: Integer);
  end;

function TNATPunchthrough.ConnectP2P(const PeerID: Integer): Boolean;
var
  MyPublicIP: string;
  MyPublicPort: Integer;
  PeerIP: string;
  PeerPort: Integer;
  Socket: TUDPBlockSocket;
begin
  // 1. Se connecter au serveur relais
  FRelayServer.Connect(RELAY_SERVER, RELAY_PORT);

  // 2. Demander notre adresse publique
  FRelayServer.SendMessage(CreateMessage(mtRequestPublicAddress, []));
  MyPublicIP := WaitForPublicAddress;

  // 3. Demander l'adresse du peer
  FRelayServer.SendMessage(CreatePeerAddressRequest(PeerID));
  PeerIP := WaitForPeerAddress(PeerID);

  // 4. Envoyer des paquets UDP pour percer le NAT
  Socket := TUDPBlockSocket.Create;
  try
    Socket.CreateSocket;

    // Envoyer plusieurs paquets pour maximiser les chances
    SendHolePunchPacket(PeerIP, PeerPort);
    Sleep(100);
    SendHolePunchPacket(PeerIP, PeerPort);
    Sleep(100);
    SendHolePunchPacket(PeerIP, PeerPort);

    // 5. Attendre la réponse du peer
    Result := WaitForPeerResponse(Socket, 5000);

    if Result then
      WriteLn('Connexion P2P établie !')
    else
      WriteLn('Échec de la connexion P2P, utilisation du relais');
  finally
    Socket.Free;
  end;
end;

procedure TNATPunchthrough.SendHolePunchPacket(const PeerAddress: string;
                                               Port: Integer);
var
  Socket: TUDPBlockSocket;
  Data: TBytes;
begin
  Socket := TUDPBlockSocket.Create;
  try
    Socket.CreateSocket;
    Socket.Bind('0.0.0.0', '0');

    // Envoyer un paquet vide pour ouvrir le port NAT
    SetLength(Data, 1);
    Data[0] := Ord('P'); // 'P' pour Punch

    Socket.SendBufferTo(PeerAddress, Port, @Data[0], Length(Data));
  finally
    Socket.Free;
  end;
end;
```

### Serveur relais (Relay)

Pour les connexions P2P impossibles :

```pascal
type
  TRelayServer = class
  private
    FConnections: TDictionary<Integer, TClient>;
  public
    procedure RelayMessage(FromID, ToID: Integer; const Msg: TNetworkMessage);
    procedure HandleRelayRequest(Client: TClient; const Msg: TNetworkMessage);
  end;

procedure TRelayServer.RelayMessage(FromID, ToID: Integer;
                                   const Msg: TNetworkMessage);
var
  TargetClient: TClient;
begin
  if FConnections.TryGetValue(ToID, TargetClient) then
  begin
    // Ajouter l'ID de l'expéditeur au message
    SendToClient(TargetClient, Msg);
  end
  else
    WriteLn('Client cible ', ToID, ' introuvable');
end;

procedure TRelayServer.HandleRelayRequest(Client: TClient;
                                         const Msg: TNetworkMessage);
var
  TargetID: Integer;
begin
  // Extraire l'ID cible du message
  Move(Msg.Data[0], TargetID, SizeOf(Integer));

  WriteLn('Relais de ', Client.PlayerID, ' vers ', TargetID);

  // Transférer le message
  RelayMessage(Client.PlayerID, TargetID, Msg);
end;
```

## Tests et debugging réseau

### Simulation de lag

```pascal
type
  TLagSimulator = class
  private
    FMinLag: Integer;
    FMaxLag: Integer;
    FPacketLoss: Single; // 0.0 à 1.0
    FDelayedMessages: TList<record
      Msg: TNetworkMessage;
      SendTime: Cardinal;
    end>;
  public
    constructor Create(MinLagMs, MaxLagMs: Integer; PacketLossPercent: Single);
    procedure SendWithLag(const Msg: TNetworkMessage);
    procedure Update;
  end;

constructor TLagSimulator.Create(MinLagMs, MaxLagMs: Integer;
                                PacketLossPercent: Single);
begin
  FMinLag := MinLagMs;
  FMaxLag := MaxLagMs;
  FPacketLoss := PacketLossPercent / 100.0;
  FDelayedMessages := TList.Create;
end;

procedure TLagSimulator.SendWithLag(const Msg: TNetworkMessage);
var
  Delay: Integer;
  DelayedMsg: TDelayedMessage;
begin
  // Simuler la perte de paquets
  if Random < FPacketLoss then
  begin
    WriteLn('Paquet perdu (simulé)');
    Exit;
  end;

  // Simuler le lag
  Delay := FMinLag + Random(FMaxLag - FMinLag);

  DelayedMsg.Msg := Msg;
  DelayedMsg.SendTime := SDL_GetTicks + Delay;

  FDelayedMessages.Add(DelayedMsg);
end;

procedure TLagSimulator.Update;
var
  I: Integer;
  CurrentTime: Cardinal;
begin
  CurrentTime := SDL_GetTicks;

  I := 0;
  while I < FDelayedMessages.Count do
  begin
    if CurrentTime >= FDelayedMessages[I].SendTime then
    begin
      // Le délai est écoulé, envoyer le message
      ActuallySendMessage(FDelayedMessages[I].Msg);
      FDelayedMessages.Delete(I);
    end
    else
      Inc(I);
  end;
end;
```

### Logger réseau

```pascal
type
  TNetworkLogger = class
  private
    FLogFile: TextFile;
    FEnabled: Boolean;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure LogSent(const Msg: TNetworkMessage);
    procedure LogReceived(const Msg: TNetworkMessage);
    procedure LogEvent(const Event: string);
  end;

constructor TNetworkLogger.Create(const FileName: string);
begin
  AssignFile(FLogFile, FileName);
  Rewrite(FLogFile);
  FEnabled := True;

  WriteLn(FLogFile, '=== Network Log Started ===');
  WriteLn(FLogFile, 'Time: ', DateTimeToStr(Now));
end;

procedure TNetworkLogger.LogSent(const Msg: TNetworkMessage);
begin
  if not FEnabled then Exit;

  WriteLn(FLogFile, Format('[%d] SENT %s Size:%d',
    [SDL_GetTicks, MessageTypeToString(Msg.MsgType), Msg.DataSize]));
  Flush(FLogFile);
end;

procedure TNetworkLogger.LogReceived(const Msg: TNetworkMessage);
begin
  if not FEnabled then Exit;

  WriteLn(FLogFile, Format('[%d] RECV %s Size:%d',
    [SDL_GetTicks, MessageTypeToString(Msg.MsgType), Msg.DataSize]));
  Flush(FLogFile);
end;
```

### Statistiques réseau

```pascal
type
  TNetworkStats = class
  private
    FBytesSent: Int64;
    FBytesReceived: Int64;
    FMessagesSent: Integer;
    FMessagesReceived: Integer;
    FStartTime: Cardinal;
  public
    procedure RecordSent(Bytes: Integer);
    procedure RecordReceived(Bytes: Integer);
    function GetUploadRate: Single; // KB/s
    function GetDownloadRate: Single; // KB/s
    procedure PrintStats;
  end;

procedure TNetworkStats.RecordSent(Bytes: Integer);
begin
  Inc(FBytesSent, Bytes);
  Inc(FMessagesSent);
end;

procedure TNetworkStats.RecordReceived(Bytes: Integer);
begin
  Inc(FBytesReceived, Bytes);
  Inc(FMessagesReceived);
end;

function TNetworkStats.GetUploadRate: Single;
var
  ElapsedSeconds: Single;
begin
  ElapsedSeconds := (SDL_GetTicks - FStartTime) / 1000.0;
  if ElapsedSeconds > 0 then
    Result := (FBytesSent / 1024.0) / ElapsedSeconds
  else
    Result := 0;
end;

procedure TNetworkStats.PrintStats;
begin
  WriteLn('=== Network Statistics ===');
  WriteLn('Uptime: ', (SDL_GetTicks - FStartTime) / 1000:0:1, ' seconds');
  WriteLn('Sent: ', FBytesSent, ' bytes (', FMessagesSent, ' messages)');
  WriteLn('Received: ', FBytesReceived, ' bytes (', FMessagesReceived, ' messages)');
  WriteLn('Upload: ', GetUploadRate:0:2, ' KB/s');
  WriteLn('Download: ', GetDownloadRate:0:2, ' KB/s');
  WriteLn('=========================');
end;
```

## Exemples de jeux complets

### Jeu de tir simple (Client-Serveur)

```pascal
program SimpleShooterClient;

uses
  SDL2, blcksock, SysUtils;

type
  TShooterClient = class
  private
    FSocket: TUDPBlockSocket;
    FServerIP: string;
    FServerPort: Integer;
    FMyPlayerID: Integer;
    FPlayers: array[0..9] of TPlayer;
  public
    constructor Create;
    procedure Connect(const Host: string; Port: Integer);
    procedure SendInput(const Input: TPlayerInput);
    procedure Update;
    procedure Render;
  end;

constructor TShooterClient.Create;
begin
  FSocket := TUDPBlockSocket.Create;
  FSocket.CreateSocket;
  FSocket.Bind('0.0.0.0', '0');
end;

procedure TShooterClient.Connect(const Host: string; Port: Integer);
var
  JoinMsg: TNetworkMessage;
begin
  FServerIP := Host;
  FServerPort := Port;

  // Envoyer le message de connexion
  JoinMsg := CreateMessage(mtPlayerJoin, []);
  FSocket.SendBufferTo(Host, Port, @JoinMsg, SizeOf(JoinMsg));

  WriteLn('Connexion au serveur ', Host, ':', Port);
end;

procedure TShooterClient.SendInput(const Input: TPlayerInput);
var
  Msg: TNetworkMessage;
  Data: TBytes;
begin
  // Sérialiser l'input
  SetLength(Data, SizeOf(TPlayerInput));
  Move(Input, Data[0], SizeOf(TPlayerInput));

  // Créer et envoyer le message
  Msg := CreateMessage(mtPlayerMove, Data);
  FSocket.SendBufferTo(FServerIP, FServerPort, @Msg, SizeOf(Msg));
end;

procedure TShooterClient.Update;
var
  Data: string;
  Msg: TNetworkMessage;
  PlayerState: TPlayerState;
begin
  // Recevoir les updates du serveur
  while FSocket.CanRead(0) do
  begin
    Data := FSocket.RecvPacket(1000);

    if Length(Data) >= SizeOf(TNetworkMessage) then
    begin
      Move(Data[1], Msg, SizeOf(TNetworkMessage));

      case Msg.MsgType of
        mtPlayerMove:
        begin
          // Mettre à jour la position des autres joueurs
          Move(Msg.Data[0], PlayerState, SizeOf(TPlayerState));
          UpdatePlayer(PlayerState);
        end;

        mtPlayerJoin:
        begin
          // Nouveau joueur
          Move(Msg.Data[0], FMyPlayerID, SizeOf(Integer));
          WriteLn('Mon ID : ', FMyPlayerID);
        end;
      end;
    end;
  end;
end;

procedure TShooterClient.Render;
var
  I: Integer;
begin
  // Dessiner tous les joueurs
  for I := 0 to High(FPlayers) do
  begin
    if FPlayers[I].Active then
    begin
      if FPlayers[I].ID = FMyPlayerID then
        DrawPlayer(FPlayers[I], clBlue)  // Moi en bleu
      else
        DrawPlayer(FPlayers[I], clRed);  // Les autres en rouge
    end;
  end;
end;

// Boucle principale
var
  Client: TShooterClient;
  Input: TPlayerInput;
  Running: Boolean;
  Event: TSDL_Event;

begin
  SDL_Init(SDL_INIT_EVERYTHING);

  Client := TShooterClient.Create;
  Client.Connect('127.0.0.1', 7777);

  Running := True;

  while Running do
  begin
    // Gérer les événements
    while SDL_PollEvent(@Event) <> 0 do
    begin
      if Event.type_ = SDL_QUIT then
        Running := False;
    end;

    // Capturer l'input local
    Input := CapturePlayerInput;

    // Envoyer au serveur
    Client.SendInput(Input);

    // Appliquer localement (prédiction)
    ApplyInputToLocalPlayer(Input);

    // Recevoir les updates
    Client.Update;

    // Dessiner
    Client.Render;

    SDL_Delay(16); // ~60 FPS
  end;

  Client.Free;
  SDL_Quit;
end.
```

### Serveur de jeu simple

```pascal
program SimpleShooterServer;

uses
  blcksock, SysUtils, Classes;

type
  TShooterServer = class
  private
    FSocket: TUDPBlockSocket;
    FClients: TList<TClientInfo>;
    FNextPlayerID: Integer;
  public
    constructor Create(Port: Integer);
    procedure Update;
    procedure BroadcastGameState;
  end;

constructor TShooterServer.Create(Port: Integer);
begin
  FSocket := TUDPBlockSocket.Create;
  FSocket.CreateSocket;
  FSocket.Bind('0.0.0.0', IntToStr(Port));

  FClients := TList<TClientInfo>.Create;
  FNextPlayerID := 1;

  WriteLn('Serveur démarré sur le port ', Port);
end;

procedure TShooterServer.Update;
var
  Data: string;
  Msg: TNetworkMessage;
  RemoteIP: string;
  RemotePort: Integer;
  ClientKey: string;
  Client: TClientInfo;
  PlayerInput: TPlayerInput;
begin
  // Recevoir les messages des clients
  while FSocket.CanRead(0) do
  begin
    Data := FSocket.RecvPacket(1000);
    RemoteIP := FSocket.GetRemoteSinIP;
    RemotePort := FSocket.GetRemoteSinPort;
    ClientKey := RemoteIP + ':' + IntToStr(RemotePort);

    if Length(Data) >= SizeOf(TNetworkMessage) then
    begin
      Move(Data[1], Msg, SizeOf(TNetworkMessage));

      case Msg.MsgType of
        mtPlayerJoin:
        begin
          // Nouveau client
          Client.IP := RemoteIP;
          Client.Port := RemotePort;
          Client.PlayerID := FNextPlayerID;
          Inc(FNextPlayerID);

          FClients.Add(Client);

          WriteLn('Nouveau joueur : ', Client.PlayerID);

          // Envoyer son ID au client
          SendPlayerID(RemoteIP, RemotePort, Client.PlayerID);
        end;

        mtPlayerMove:
        begin
          // Input du joueur
          Move(Msg.Data[0], PlayerInput, SizeOf(TPlayerInput));

          // Trouver le client
          Client := FindClient(ClientKey);
          if Client <> nil then
          begin
            // Valider et appliquer l'input
            if ValidateInput(Client, PlayerInput) then
            begin
              ApplyInput(Client, PlayerInput);
              Client.LastUpdateTime := GetTickCount64;
            end;
          end;
        end;
      end;
    end;
  end;

  // Mettre à jour la logique du jeu
  UpdateGameLogic;

  // Envoyer l'état du jeu à tous les clients
  BroadcastGameState;
end;

procedure TShooterServer.BroadcastGameState;
var
  Client: TClientInfo;
  Msg: TNetworkMessage;
  GameState: TGameState;
begin
  // Préparer l'état du jeu
  GameState := GetCurrentGameState;

  // Créer le message
  Msg := CreateGameStateMessage(GameState);

  // Envoyer à tous les clients
  for Client in FClients do
  begin
    FSocket.SendBufferTo(Client.IP, Client.Port, @Msg, SizeOf(Msg));
  end;
end;

// Boucle principale du serveur
var
  Server: TShooterServer;
  Running: Boolean;

begin
  Server := TShooterServer.Create(7777);
  Running := True;

  WriteLn('Serveur en attente de joueurs...');
  WriteLn('Appuyez sur Ctrl+C pour arrêter');

  while Running do
  begin
    Server.Update;
    Sleep(16); // ~60 ticks/seconde
  end;

  Server.Free;
end.
```

### Jeu au tour par tour (TCP)

```pascal
type
  TTurnBasedGame = class
  private
    FCurrentTurn: Integer;
    FPlayers: array[0..3] of TPlayer;
    FPlayerCount: Integer;
  public
    procedure ProcessTurn(PlayerID: Integer; const Action: TGameAction);
    procedure NextTurn;
    procedure SendTurnNotification;
  end;

procedure TTurnBasedGame.ProcessTurn(PlayerID: Integer;
                                    const Action: TGameAction);
begin
  if PlayerID <> FCurrentTurn then
  begin
    WriteLn('Pas votre tour !');
    Exit;
  end;

  // Valider et appliquer l'action
  if ValidateAction(PlayerID, Action) then
  begin
    ApplyAction(PlayerID, Action);

    // Notifier tous les joueurs
    BroadcastAction(PlayerID, Action);

    // Passer au tour suivant
    NextTurn;
  end;
end;

procedure TTurnBasedGame.NextTurn;
begin
  FCurrentTurn := (FCurrentTurn + 1) mod FPlayerCount;

  WriteLn('Tour du joueur ', FCurrentTurn);

  // Notifier le joueur que c'est son tour
  SendTurnNotification;
end;

procedure TTurnBasedGame.SendTurnNotification;
var
  Msg: TNetworkMessage;
begin
  Msg := CreateMessage(mtYourTurn, []);
  SendToPlayer(FCurrentTurn, Msg);
end;
```

## Déploiement et production

### Configuration serveur dédié

```pascal
program DedicatedServer;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes;

const
  SERVER_PORT = 7777;
  MAX_PLAYERS = 32;
  TICK_RATE = 60; // Updates par seconde

var
  Server: TGameServer;
  Running: Boolean;

procedure HandleSignal(Signal: Integer); cdecl;
begin
  WriteLn('Signal reçu : ', Signal);
  Running := False;
end;

begin
  WriteLn('=== Serveur Dédié ===');
  WriteLn('Port : ', SERVER_PORT);
  WriteLn('Max joueurs : ', MAX_PLAYERS);
  WriteLn('=====================');

  {$IFDEF UNIX}
  // Gérer SIGINT et SIGTERM sur Linux
  Signal(SIGINT, @HandleSignal);
  Signal(SIGTERM, @HandleSignal);
  {$ENDIF}

  Server := TGameServer.Create(SERVER_PORT);
  Running := True;

  try
    while Running do
    begin
      Server.Update;
      Sleep(1000 div TICK_RATE);
    end;
  finally
    WriteLn('Arrêt du serveur...');
    Server.Free;
  end;
end.
```

### Service systemd (Linux)

Créer `/etc/systemd/system/gameserver.service` :

```ini
[Unit]
Description=Game Server
After=network.target

[Service]
Type=simple
User=gameserver
WorkingDirectory=/opt/gameserver
ExecStart=/opt/gameserver/server
Restart=on-failure
RestartSec=10

[Install]
WantedBy=multi-user.target
```

Commandes :
```bash
# Activer le service
sudo systemctl enable gameserver

# Démarrer
sudo systemctl start gameserver

# Statut
sudo systemctl status gameserver

# Logs
sudo journalctl -u gameserver -f
```

### Docker pour le serveur

`Dockerfile` :
```dockerfile
FROM ubuntu:22.04

# Installer les dépendances
RUN apt-get update && apt-get install -y \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

# Créer un utilisateur non-root
RUN useradd -m -s /bin/bash gameserver

# Copier le serveur
COPY --chown=gameserver:gameserver server /opt/gameserver/
COPY --chown=gameserver:gameserver config.ini /opt/gameserver/

USER gameserver
WORKDIR /opt/gameserver

EXPOSE 7777/udp

CMD ["./server"]
```

`docker-compose.yml` :
```yaml
version: '3.8'

services:
  gameserver:
    build: .
    ports:
      - "7777:7777/udp"
    restart: unless-stopped
    environment:
      - MAX_PLAYERS=32
      - TICK_RATE=60
    volumes:
      - ./logs:/opt/gameserver/logs
```

Commandes :
```bash
# Build
docker-compose build

# Démarrer
docker-compose up -d

# Logs
docker-compose logs -f

# Arrêter
docker-compose down
```

## Monitoring et métriques

### Prometheus metrics

```pascal
type
  TServerMetrics = class
  private
    FActiveConnections: Integer;
    FTotalMessages: Int64;
    FMessageRate: Single;
  public
    procedure RecordMessage;
    procedure RecordConnection;
    procedure RecordDisconnection;
    function GetMetrics: string;
  end;

function TServerMetrics.GetMetrics: string;
begin
  Result := '# HELP active_connections Number of active connections'#10 +
            '# TYPE active_connections gauge'#10 +
            'active_connections ' + IntToStr(FActiveConnections) + #10 +
            '# HELP total_messages Total messages processed'#10 +
            '# TYPE total_messages counter'#10 +
            'total_messages ' + IntToStr(FTotalMessages) + #10 +
            '# HELP message_rate Messages per second'#10 +
            '# TYPE message_rate gauge'#10 +
            'message_rate ' + FloatToStr(FMessageRate) + #10;
end;

// Serveur HTTP simple pour exposer les métriques
procedure ServeMetrics(Port: Integer);
var
  HTTPServer: THTTPServer;
begin
  HTTPServer := THTTPServer.Create(Port);
  HTTPServer.OnRequest := procedure(Request: THTTPRequest; Response: THTTPResponse)
  begin
    if Request.Path = '/metrics' then
    begin
      Response.ContentType := 'text/plain';
      Response.Content := Metrics.GetMetrics;
    end;
  end;
  HTTPServer.Start;
end;
```

### Health check endpoint

```pascal
type
  THealthCheck = class
  public
    function IsHealthy: Boolean;
    function GetStatus: string;
  end;

function THealthCheck.IsHealthy: Boolean;
begin
  Result := (Server <> nil) and
            (Server.IsRunning) and
            (Server.GetActiveConnections < MAX_PLAYERS) and
            (Server.GetAveragePing < 200);
end;

function THealthCheck.GetStatus: string;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.Add('healthy', IsHealthy);
    JSON.Add('uptime', Server.GetUptime);
    JSON.Add('players', Server.GetActiveConnections);
    JSON.Add('max_players', MAX_PLAYERS);
    JSON.Add('average_ping', Server.GetAveragePing);
    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;
```

## Ressources et documentation

### Bibliothèques réseau FreePascal

**Synapse** :
- Site : http://synapse.ararat.cz/
- Documentation complète
- TCP, UDP, HTTP, FTP, SMTP, etc.
- Très stable et éprouvée

**lNet** :
- GitHub : https://github.com/almindor/lnet
- Moderne et orientée événements
- Support epoll/kqueue
- Bonne performance

**Indy pour Lazarus** :
- Port d'Indy (Delphi)
- Nombreux protocoles
- Bonne documentation

### Tutoriels et articles

**Networking fondamental** :
- "Gaffer On Games" : https://gafferongames.com/
- "Gabriel Gambetta" : https://www.gabrielgambetta.com/client-server-game-architecture.html
- "Source Multiplayer Networking" (Valve)

**Livres recommandés** :
- "Multiplayer Game Programming" - Joshua Glazer & Sanjay Madhav
- "Real-Time Rendering" - Tomas Akenine-Möller
- "Network Programming for Games" - Glenn Fiedler

### Communautés

- **Forum FreePascal** : freepascal.org/forum
- **Lazarus Forum** : forum.lazarus.freepascal.org
- **Reddit** : r/gamedev, r/gamenetworking
- **Discord** : Serveurs dédiés au game dev

## Conseils et bonnes pratiques

### Checklist de développement

**Phase de conception** :
- [ ] Choisir l'architecture (client-serveur vs P2P)
- [ ] Définir le protocole (TCP vs UDP)
- [ ] Estimer la bande passante nécessaire
- [ ] Planifier la gestion des déconnexions
- [ ] Concevoir le système anti-triche

**Phase de développement** :
- [ ] Implémenter le protocole réseau
- [ ] Créer le système de sérialisation
- [ ] Développer la prédiction côté client
- [ ] Implémenter l'interpolation
- [ ] Ajouter la lag compensation
- [ ] Crypter les communications sensibles

**Phase de test** :
- [ ] Tester avec lag simulé
- [ ] Tester la perte de paquets
- [ ] Tester les reconnexions
- [ ] Tester avec beaucoup de joueurs
- [ ] Tester la sécurité

**Phase de déploiement** :
- [ ] Configurer le serveur dédié
- [ ] Mettre en place le monitoring
- [ ] Configurer les sauvegardes
- [ ] Documenter l'infrastructure
- [ ] Planifier la scalabilité

### Erreurs courantes à éviter

❌ **Ne pas faire** :
- Faire confiance aux données du client
- Envoyer trop de données trop souvent
- Utiliser TCP pour tout
- Oublier la validation serveur-side
- Ignorer la latence dans le gameplay
- Stocker des mots de passe en clair
- Ne pas tester avec du lag réel

✅ **Faire** :
- Valider toutes les actions côté serveur
- Optimiser la bande passante (delta, compression)
- Utiliser UDP pour les données temps-réel
- Implémenter la prédiction côté client
- Concevoir pour 100-200ms de latence
- Crypter les communications
- Tester dans des conditions réelles

### Optimisation de la bande passante

**Règles d'or** :
1. N'envoyez que ce qui a changé (delta)
2. Compressez les données volumineuses
3. Utilisez des types de données compacts
4. Limitez la fréquence d'envoi
5. Priorisez les messages importants

**Exemple de budget réseau** :
```
Upload par joueur (60 Hz) :
- Position : 12 bytes (3 floats)
- Rotation : 4 bytes (1 float)
- Actions : 1 byte (flags)
- Total : ~17 bytes × 60 = ~1 KB/s

Download par joueur (30 Hz) :
- 31 autres joueurs × 17 bytes × 30 Hz = ~16 KB/s
- État du jeu : ~5 KB/s
- Total : ~21 KB/s

Bande passante totale : ~22 KB/s (~176 Kbps)
```

## Conclusion

Le développement de jeux multijoueurs en réseau est un défi technique complexe mais gratifiant. FreePascal offre d'excellentes bibliothèques (Synapse, lNet) pour créer des jeux en ligne robustes et performants.

### Points clés à retenir

**Architecture** :
- Client-serveur pour autorité et anti-triche
- P2P pour réduire les coûts et la latence
- Hybride possible avec serveur relais

**Protocoles** :
- UDP pour données temps-réel (positions)
- TCP pour données importantes (connexion, chat)
- Sérialisation binaire pour performance

**Synchronisation** :
- Prédiction côté client pour réactivité
- Interpolation pour mouvements fluides
- Lag compensation pour équité

**Sécurité** :
- Ne jamais faire confiance au client
- Valider toutes les actions serveur-side
- Crypter les communications sensibles
- Détecter les patterns suspects

### Prochaines étapes

1. **Commencez simple** : Chat ou jeu au tour par tour
2. **Ajoutez la complexité** : Prédiction, interpolation
3. **Testez rigoureusement** : Lag, déconnexions, charge
4. **Optimisez** : Bande passante, CPU, mémoire
5. **Déployez** : Serveur dédié, monitoring, scaling

Le multijoueur ajoute une dimension sociale incroyable aux jeux. Avec les connaissances de ce chapitre, vous avez les bases pour créer des expériences multijoueurs mémorables avec FreePascal !

**Bon développement et que vos serveurs tournent toujours ! 🌐🎮**

---

## Annexe : Template serveur complet

```pascal
unit GameServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock;

type
  TGameServer = class
  private
    FSocket: TUDPBlockSocket;
    FClients: TList;
    FRunning: Boolean;
    FPort: Integer;
  public
    constructor Create(Port: Integer);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Update;
    property Running: Boolean read FRunning;
  end;

implementation

constructor TGameServer.Create(Port: Integer);
begin
  FPort := Port;
  FClients := TList.Create;
  FSocket := TUDPBlockSocket.Create;
  FSocket.CreateSocket;
  FSocket.Bind('0.0.0.0', IntToStr(Port));
end;

destructor TGameServer.Destroy;
begin
  Stop;
  FSocket.Free;
  FClients.Free;
  inherited;
end;

procedure TGameServer.Start;
begin
  FRunning := True;
  WriteLn('Serveur démarré sur le port ', FPort);
end;

procedure TGameServer.Stop;
begin
  FRunning := False;
  WriteLn('Serveur arrêté');
end;

procedure TGameServer.Update;
begin
  // Recevoir et traiter les messages
  // Mettre à jour la logique du jeu
  // Envoyer les updates aux clients
end;

end.
```

Utilisez ce template comme base pour vos propres serveurs de jeu ! 🚀

⏭️ [Scripting et modding](/23-developpement-jeux/07-scripting-modding.md)
