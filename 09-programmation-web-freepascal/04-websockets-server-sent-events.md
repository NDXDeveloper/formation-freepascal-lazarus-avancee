🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.4 WebSockets et Server-Sent Events

## Introduction

Les applications web modernes nécessitent souvent une **communication en temps réel** entre le serveur et les clients. Contrairement au HTTP classique où le client doit demander des informations (modèle requête-réponse), les technologies temps réel permettent au serveur d'envoyer des données aux clients instantanément.

### Communication HTTP classique vs Temps réel

**HTTP classique (polling)** :
```
Client → Serveur : Y a-t-il des nouveautés ?  
Serveur → Client : Non
[attente 5 secondes]
Client → Serveur : Y a-t-il des nouveautés ?  
Serveur → Client : Non
[attente 5 secondes]
Client → Serveur : Y a-t-il des nouveautés ?  
Serveur → Client : Oui, voici les données !
```

**Problèmes du polling** :
- Nombreuses requêtes inutiles
- Latence (délai entre l'événement et sa réception)
- Surcharge serveur
- Gaspillage de bande passante

**Communication temps réel** :
```
Client ↔ Serveur : Connexion établie
[connexion maintenue]
Serveur → Client : Nouvelles données disponibles !  
Serveur → Client : Autre événement !
[la connexion reste ouverte]
```

### Deux technologies principales

#### WebSockets

**WebSocket** est un protocole de communication **bidirectionnelle** full-duplex sur une seule connexion TCP.

**Caractéristiques** :
- ✅ Communication bidirectionnelle (client ↔ serveur)
- ✅ Connexion persistante
- ✅ Faible latence
- ✅ Peu d'overhead (après établissement)
- ✅ Support binaire et texte

**Cas d'usage** :
- Chat en temps réel
- Jeux multijoueurs
- Applications collaboratives (éditeurs partagés)
- Trading en temps réel
- Notifications instantanées

#### Server-Sent Events (SSE)

**SSE** est une technologie simple pour le flux de données **unidirectionnel** du serveur vers le client.

**Caractéristiques** :
- ✅ Simple à implémenter
- ✅ Fonctionne sur HTTP standard
- ✅ Reconnexion automatique
- ✅ ID d'événements pour reprendre où on s'est arrêté
- ⚠️ Unidirectionnel uniquement (serveur → client)
- ⚠️ Texte uniquement (pas de binaire)

**Cas d'usage** :
- Flux d'actualités
- Notifications push
- Mises à jour de tableaux de bord
- Progression de tâches
- Cours de bourse en temps réel

### Comparaison WebSocket vs SSE

| Aspect | WebSocket | SSE |
|--------|-----------|-----|
| **Direction** | Bidirectionnelle ↔ | Unidirectionnelle → |
| **Protocole** | ws:// ou wss:// | HTTP/HTTPS |
| **Complexité** | Moyenne | Simple |
| **Format données** | Texte + Binaire | Texte uniquement |
| **Reconnexion** | Manuelle | Automatique |
| **Support navigateurs** | Excellent | Très bon |
| **Overhead** | Faible | Moyen |
| **Firewall** | Parfois bloqué | Passe partout |

**Règle générale** :
- **WebSocket** : Quand vous avez besoin de communication bidirectionnelle
- **SSE** : Quand seul le serveur envoie des données

## WebSockets avec FreePascal

### Comprendre le protocole WebSocket

WebSocket commence par un **handshake HTTP** classique, puis passe à un protocole WebSocket persistant.

**Handshake (effectué par le navigateur)** :

```http
GET /chat HTTP/1.1  
Host: example.com  
Upgrade: websocket  
Connection: Upgrade  
Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==  
Sec-WebSocket-Version: 13
```

**Réponse du serveur** :

```http
HTTP/1.1 101 Switching Protocols  
Upgrade: websocket  
Connection: Upgrade  
Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
```

Après cet échange, la connexion reste ouverte et les deux parties peuvent s'envoyer des **frames** WebSocket.

### Installation de la bibliothèque WebSocket

Pour FreePascal, nous utiliserons **fphttpwebsocket** (inclus dans fpWeb).

**Vérification** :

```bash
# La bibliothèque est incluse dans FreePascal
# Pas d'installation supplémentaire nécessaire
```

### Serveur WebSocket simple

**Programme complet** : `websocket_server.lpr`

```pascal
program WebSocketServer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fphttpapp, httpdefs, httproute,
  fpwebsocket, websocketserver;

type
  // Gestionnaire de connexion WebSocket
  TMyWebSocketHandler = class(TWebSocketHandler)
  public
    procedure DoHandleMessage(ASocket: TWebSocketConnection; AMessage: TWebSocketMessage); override;
    procedure DoConnect(ASocket: TWebSocketConnection); override;
    procedure DoDisconnect(ASocket: TWebSocketConnection); override;
  end;

procedure TMyWebSocketHandler.DoConnect(ASocket: TWebSocketConnection);  
begin
  WriteLn(Format('[%s] Client connecté', [FormatDateTime('hh:nn:ss', Now)]));

  // Message de bienvenue
  ASocket.SendText('{"type":"welcome","message":"Connecté au serveur WebSocket"}');
end;

procedure TMyWebSocketHandler.DoDisconnect(ASocket: TWebSocketConnection);  
begin
  WriteLn(Format('[%s] Client déconnecté', [FormatDateTime('hh:nn:ss', Now)]));
end;

procedure TMyWebSocketHandler.DoHandleMessage(ASocket: TWebSocketConnection;
                                               AMessage: TWebSocketMessage);
var
  ReceivedText: string;
  Response: string;
begin
  // Message reçu du client
  if AMessage.MessageType = mtString then
  begin
    ReceivedText := AMessage.AsString;
    WriteLn(Format('[%s] Reçu: %s', [FormatDateTime('hh:nn:ss', Now), ReceivedText]));

    // Répondre au client
    Response := Format('{"type":"echo","original":"%s","timestamp":"%s"}',
                      [ReceivedText, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
    ASocket.SendText(Response);
  end;
end;

var
  WSHandler: TMyWebSocketHandler;

begin
  // Créer le gestionnaire WebSocket
  WSHandler := TMyWebSocketHandler.Create;

  // Enregistrer la route WebSocket
  HTTPRouter.RegisterRoute('/ws', WSHandler);

  // Configuration du serveur
  Application.Port := 8080;

  WriteLn('Serveur WebSocket démarré sur ws://localhost:8080/ws');
  WriteLn('Appuyez sur Ctrl+C pour arrêter');

  // Démarrer
  Application.Run;
end.
```

### Client HTML/JavaScript pour tester

**Créer** : `websocket_client.html`

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>Client WebSocket</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            max-width: 800px;
            margin: 50px auto;
            padding: 20px;
        }
        #messages {
            height: 400px;
            border: 1px solid #ccc;
            padding: 10px;
            overflow-y: auto;
            background: #f5f5f5;
            margin-bottom: 10px;
        }
        .message {
            padding: 5px;
            margin: 5px 0;
            border-radius: 3px;
        }
        .sent { background: #e3f2fd; text-align: right; }
        .received { background: #fff3e0; }
        .system { background: #e8f5e9; font-style: italic; }
        input[type="text"] {
            width: 70%;
            padding: 10px;
            font-size: 14px;
        }
        button {
            padding: 10px 20px;
            font-size: 14px;
            cursor: pointer;
        }
        #status {
            padding: 10px;
            margin-bottom: 10px;
            border-radius: 5px;
            font-weight: bold;
        }
        .connected { background: #c8e6c9; color: #2e7d32; }
        .disconnected { background: #ffcdd2; color: #c62828; }
    </style>
</head>
<body>
    <h1>Client WebSocket</h1>

    <div id="status" class="disconnected">Déconnecté</div>

    <div id="messages"></div>

    <input type="text" id="messageInput" placeholder="Tapez un message..." />
    <button onclick="sendMessage()">Envoyer</button>
    <button onclick="connect()">Connecter</button>
    <button onclick="disconnect()">Déconnecter</button>

    <script>
        let ws = null;
        const messagesDiv = document.getElementById('messages');
        const statusDiv = document.getElementById('status');
        const messageInput = document.getElementById('messageInput');

        function addMessage(text, className) {
            const div = document.createElement('div');
            div.className = 'message ' + className;
            div.textContent = text;
            messagesDiv.appendChild(div);
            messagesDiv.scrollTop = messagesDiv.scrollHeight;
        }

        function updateStatus(connected) {
            if (connected) {
                statusDiv.textContent = 'Connecté';
                statusDiv.className = 'connected';
            } else {
                statusDiv.textContent = 'Déconnecté';
                statusDiv.className = 'disconnected';
            }
        }

        function connect() {
            if (ws && ws.readyState === WebSocket.OPEN) {
                addMessage('Déjà connecté', 'system');
                return;
            }

            ws = new WebSocket('ws://localhost:8080/ws');

            ws.onopen = function() {
                addMessage('Connexion établie', 'system');
                updateStatus(true);
            };

            ws.onmessage = function(event) {
                addMessage('← ' + event.data, 'received');
            };

            ws.onerror = function(error) {
                addMessage('Erreur: ' + error, 'system');
            };

            ws.onclose = function() {
                addMessage('Connexion fermée', 'system');
                updateStatus(false);
            };
        }

        function disconnect() {
            if (ws) {
                ws.close();
                ws = null;
            }
        }

        function sendMessage() {
            if (!ws || ws.readyState !== WebSocket.OPEN) {
                addMessage('Non connecté !', 'system');
                return;
            }

            const message = messageInput.value.trim();
            if (message === '') return;

            ws.send(message);
            addMessage('→ ' + message, 'sent');
            messageInput.value = '';
        }

        messageInput.addEventListener('keypress', function(e) {
            if (e.key === 'Enter') {
                sendMessage();
            }
        });

        // Connexion automatique au chargement
        connect();
    </script>
</body>
</html>
```

**Tester** :

```bash
# Terminal 1 : Démarrer le serveur
./websocket_server

# Terminal 2 ou navigateur : Ouvrir websocket_client.html
firefox websocket_client.html
```

### Application Chat multi-utilisateurs

**Serveur de chat** : `chat_server.lpr`

```pascal
program ChatServer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fpjson,
  fphttpapp, httpdefs, httproute,
  fpwebsocket, websocketserver;

type
  // Gestion des utilisateurs connectés
  TChatUser = class
    Socket: TWebSocketConnection;
    Username: string;
    ConnectedAt: TDateTime;
  end;

  TChatHandler = class(TWebSocketHandler)
  private
    FUsers: TList;
    procedure BroadcastMessage(const Msg: string; ExceptSocket: TWebSocketConnection = nil);
    procedure SendUserList(ASocket: TWebSocketConnection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoConnect(ASocket: TWebSocketConnection); override;
    procedure DoDisconnect(ASocket: TWebSocketConnection); override;
    procedure DoHandleMessage(ASocket: TWebSocketConnection; AMessage: TWebSocketMessage); override;
  end;

constructor TChatHandler.Create;  
begin
  inherited Create;
  FUsers := TList.Create;
end;

destructor TChatHandler.Destroy;  
var
  i: Integer;
begin
  for i := 0 to FUsers.Count - 1 do
    TChatUser(FUsers[i]).Free;
  FUsers.Free;
  inherited;
end;

procedure TChatHandler.BroadcastMessage(const Msg: string; ExceptSocket: TWebSocketConnection);  
var
  i: Integer;
  User: TChatUser;
begin
  for i := 0 to FUsers.Count - 1 do
  begin
    User := TChatUser(FUsers[i]);
    if (ExceptSocket = nil) or (User.Socket <> ExceptSocket) then
      User.Socket.SendText(Msg);
  end;
end;

procedure TChatHandler.SendUserList(ASocket: TWebSocketConnection);  
var
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  i: Integer;
  User: TChatUser;
begin
  JSONArray := TJSONArray.Create;
  try
    for i := 0 to FUsers.Count - 1 do
    begin
      User := TChatUser(FUsers[i]);
      JSONArray.Add(User.Username);
    end;

    JSONObj := TJSONObject.Create;
    try
      JSONObj.Add('type', 'userlist');
      JSONObj.Add('users', JSONArray);

      ASocket.SendText(JSONObj.AsJSON);
    finally
      JSONObj.Free;
    end;
  except
    JSONArray.Free;
    raise;
  end;
end;

procedure TChatHandler.DoConnect(ASocket: TWebSocketConnection);  
var
  User: TChatUser;
begin
  WriteLn(Format('[%s] Nouvelle connexion', [FormatDateTime('hh:nn:ss', Now)]));

  User := TChatUser.Create;
  User.Socket := ASocket;
  User.Username := 'User' + IntToStr(Random(9999));
  User.ConnectedAt := Now;

  FUsers.Add(User);

  // Assigner l'objet utilisateur à la connexion
  ASocket.Data := User;
end;

procedure TChatHandler.DoDisconnect(ASocket: TWebSocketConnection);  
var
  User: TChatUser;
  JSONObj: TJSONObject;
  i: Integer;
begin
  User := TChatUser(ASocket.Data);
  if User = nil then Exit;

  WriteLn(Format('[%s] %s déconnecté', [FormatDateTime('hh:nn:ss', Now), User.Username]));

  // Notifier les autres utilisateurs
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('type', 'user_left');
    JSONObj.Add('username', User.Username);
    BroadcastMessage(JSONObj.AsJSON);
  finally
    JSONObj.Free;
  end;

  // Retirer de la liste
  for i := 0 to FUsers.Count - 1 do
    if TChatUser(FUsers[i]) = User then
    begin
      FUsers.Delete(i);
      User.Free;
      Break;
    end;
end;

procedure TChatHandler.DoHandleMessage(ASocket: TWebSocketConnection;
                                       AMessage: TWebSocketMessage);
var
  User: TChatUser;
  JSONData: TJSONData;
  JSONObj, ResponseObj: TJSONObject;
  MsgType, Content, NewUsername: string;
begin
  if AMessage.MessageType <> mtString then Exit;

  User := TChatUser(ASocket.Data);
  if User = nil then Exit;

  try
    JSONData := GetJSON(AMessage.AsString);
    try
      if not (JSONData is TJSONObject) then Exit;

      JSONObj := TJSONObject(JSONData);
      MsgType := JSONObj.Get('type', '');

      if MsgType = 'set_username' then
      begin
        NewUsername := JSONObj.Get('username', '');
        if (NewUsername <> '') and (Length(NewUsername) <= 20) then
        begin
          // Notifier le changement de nom
          ResponseObj := TJSONObject.Create;
          try
            ResponseObj.Add('type', 'username_changed');
            ResponseObj.Add('old_username', User.Username);
            ResponseObj.Add('new_username', NewUsername);
            BroadcastMessage(ResponseObj.AsJSON);
          finally
            ResponseObj.Free;
          end;

          User.Username := NewUsername;
        end;
      end
      else if MsgType = 'message' then
      begin
        Content := JSONObj.Get('content', '');
        if Content <> '' then
        begin
          // Diffuser le message à tous
          ResponseObj := TJSONObject.Create;
          try
            ResponseObj.Add('type', 'message');
            ResponseObj.Add('username', User.Username);
            ResponseObj.Add('content', Content);
            ResponseObj.Add('timestamp', FormatDateTime('hh:nn:ss', Now));

            BroadcastMessage(ResponseObj.AsJSON);
          finally
            ResponseObj.Free;
          end;
        end;
      end
      else if MsgType = 'get_users' then
        SendUserList(ASocket);
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur: ', E.Message);
  end;
end;

var
  ChatHandler: TChatHandler;

begin
  Randomize;

  ChatHandler := TChatHandler.Create;

  HTTPRouter.RegisterRoute('/chat', ChatHandler);

  Application.Port := 8080;

  WriteLn('Serveur de chat démarré sur ws://localhost:8080/chat');
  WriteLn('Appuyez sur Ctrl+C pour arrêter');

  Application.Run;
end.
```

### Client de chat

**Créer** : `chat_client.html`

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>Chat WebSocket</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            font-family: Arial, sans-serif;
            display: flex;
            height: 100vh;
            background: #f0f0f0;
        }
        #sidebar {
            width: 250px;
            background: #2c3e50;
            color: white;
            padding: 20px;
        }
        #sidebar h2 {
            margin-bottom: 15px;
            font-size: 18px;
        }
        #userList {
            list-style: none;
        }
        #userList li {
            padding: 8px;
            margin: 5px 0;
            background: #34495e;
            border-radius: 3px;
        }
        #chatArea {
            flex: 1;
            display: flex;
            flex-direction: column;
        }
        #header {
            background: #3498db;
            color: white;
            padding: 20px;
        }
        #messages {
            flex: 1;
            padding: 20px;
            overflow-y: auto;
            background: white;
        }
        .message {
            padding: 10px;
            margin: 10px 0;
            border-radius: 5px;
            max-width: 70%;
        }
        .message.own {
            background: #e3f2fd;
            margin-left: auto;
            text-align: right;
        }
        .message.other {
            background: #f5f5f5;
        }
        .message .username {
            font-weight: bold;
            margin-bottom: 5px;
            color: #3498db;
        }
        .message .time {
            font-size: 11px;
            color: #999;
            margin-top: 5px;
        }
        .system-message {
            text-align: center;
            color: #999;
            font-style: italic;
            margin: 10px 0;
        }
        #inputArea {
            padding: 20px;
            background: #ecf0f1;
            display: flex;
            gap: 10px;
        }
        #messageInput {
            flex: 1;
            padding: 12px;
            border: 1px solid #bdc3c7;
            border-radius: 5px;
            font-size: 14px;
        }
        button {
            padding: 12px 24px;
            background: #3498db;
            color: white;
            border: none;
            border-radius: 5px;
            cursor: pointer;
            font-size: 14px;
        }
        button:hover {
            background: #2980b9;
        }
        #usernameDialog {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(0,0,0,0.5);
            display: flex;
            align-items: center;
            justify-content: center;
        }
        #usernameDialog > div {
            background: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.2);
        }
    </style>
</head>
<body>
    <div id="usernameDialog">
        <div>
            <h2>Choisissez votre pseudo</h2>
            <input type="text" id="usernameInput" placeholder="Votre pseudo..." />
            <button onclick="setUsername()">Rejoindre le chat</button>
        </div>
    </div>

    <div id="sidebar">
        <h2>Utilisateurs en ligne (<span id="userCount">0</span>)</h2>
        <ul id="userList"></ul>
    </div>

    <div id="chatArea">
        <div id="header">
            <h1>Chat en temps réel</h1>
            <p>Bienvenue, <span id="currentUsername"></span></p>
        </div>

        <div id="messages"></div>

        <div id="inputArea">
            <input type="text" id="messageInput" placeholder="Tapez votre message..." />
            <button onclick="sendMessage()">Envoyer</button>
        </div>
    </div>

    <script>
        let ws = null;
        let myUsername = '';

        const messagesDiv = document.getElementById('messages');
        const messageInput = document.getElementById('messageInput');
        const userList = document.getElementById('userList');
        const usernameDialog = document.getElementById('usernameDialog');
        const usernameInput = document.getElementById('usernameInput');

        function addSystemMessage(text) {
            const div = document.createElement('div');
            div.className = 'system-message';
            div.textContent = text;
            messagesDiv.appendChild(div);
            messagesDiv.scrollTop = messagesDiv.scrollHeight;
        }

        function addMessage(username, content, time, isOwn) {
            const div = document.createElement('div');
            div.className = 'message ' + (isOwn ? 'own' : 'other');

            if (!isOwn) {
                const userSpan = document.createElement('div');
                userSpan.className = 'username';
                userSpan.textContent = username;
                div.appendChild(userSpan);
            }

            const contentDiv = document.createElement('div');
            contentDiv.textContent = content;
            div.appendChild(contentDiv);

            const timeSpan = document.createElement('div');
            timeSpan.className = 'time';
            timeSpan.textContent = time;
            div.appendChild(timeSpan);

            messagesDiv.appendChild(div);
            messagesDiv.scrollTop = messagesDiv.scrollHeight;
        }

        function updateUserList(users) {
            userList.innerHTML = '';
            users.forEach(user => {
                const li = document.createElement('li');
                li.textContent = user;
                userList.appendChild(li);
            });
            document.getElementById('userCount').textContent = users.length;
        }

        function connect() {
            ws = new WebSocket('ws://localhost:8080/chat');

            ws.onopen = function() {
                addSystemMessage('Connecté au serveur');

                // Définir le pseudo
                ws.send(JSON.stringify({
                    type: 'set_username',
                    username: myUsername
                }));

                // Demander la liste des utilisateurs
                ws.send(JSON.stringify({ type: 'get_users' }));
            };

            ws.onmessage = function(event) {
                const data = JSON.parse(event.data);

                switch(data.type) {
                    case 'message':
                        addMessage(data.username, data.content, data.timestamp,
                                  data.username === myUsername);
                        break;

                    case 'userlist':
                        updateUserList(data.users);
                        break;

                    case 'username_changed':
                        addSystemMessage(`${data.old_username} s'appelle maintenant ${data.new_username}`);
                        ws.send(JSON.stringify({ type: 'get_users' }));
                        break;

                    case 'user_left':
                        addSystemMessage(`${data.username} a quitté le chat`);
                        ws.send(JSON.stringify({ type: 'get_users' }));
                        break;
                }
            };

            ws.onclose = function() {
                addSystemMessage('Déconnecté du serveur');
            };
        }

        function setUsername() {
            myUsername = usernameInput.value.trim();
            if (myUsername === '') {
                alert('Veuillez entrer un pseudo');
                return;
            }

            document.getElementById('currentUsername').textContent = myUsername;
            usernameDialog.style.display = 'none';
            connect();
        }

        function sendMessage() {
            if (!ws || ws.readyState !== WebSocket.OPEN) {
                addSystemMessage('Non connecté !');
                return;
            }

            const message = messageInput.value.trim();
            if (message === '') return;

            ws.send(JSON.stringify({
                type: 'message',
                content: message
            }));

            messageInput.value = '';
        }

        messageInput.addEventListener('keypress', function(e) {
            if (e.key === 'Enter') {
                sendMessage();
            }
        });

        usernameInput.addEventListener('keypress', function(e) {
            if (e.key === 'Enter') {
                setUsername();
            }
        });

        usernameInput.focus();
    </script>
</body>
</html>
```

**Tester** :

1. Compiler et lancer le serveur :
```bash
fpc chat_server.lpr
./chat_server
```

2. Ouvrir plusieurs onglets avec `chat_client.html`
3. Choisir des pseudos différents
4. Discuter en temps réel !

## Server-Sent Events (SSE)

### Comprendre SSE

SSE utilise une connexion HTTP normale qui reste ouverte. Le serveur envoie des événements au format texte :

```
data: Premier message\n\n  
data: Deuxième message\n\n  
data: {"type":"update","value":42}\n\n
```

**Format d'un événement** :

```
event: custom_event  
id: 123  
data: Contenu du message  
data: Ligne 2 du contenu  
retry: 10000

```

- `event` : Type d'événement (optionnel)
- `id` : ID pour reprendre après déconnexion
- `data` : Contenu (peut être sur plusieurs lignes)
- `retry` : Délai de reconnexion en ms
- Ligne vide (`\n\n`) pour terminer l'événement

### Serveur SSE simple

**Programme** : `sse_server.lpr`

```pascal
program SSEServer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fphttpapp, httpdefs, httproute;

procedure HandleSSE(ARequest: TRequest; AResponse: TResponse);  
var
  i: Integer;
  EventData: string;
begin
  // Headers SSE obligatoires
  AResponse.ContentType := 'text/event-stream';
  AResponse.SetCustomHeader('Cache-Control', 'no-cache');
  AResponse.SetCustomHeader('Connection', 'keep-alive');
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');

  WriteLn(Format('[%s] Client SSE connecté', [FormatDateTime('hh:nn:ss', Now)]));

  // Envoyer des événements toutes les 2 secondes
  for i := 1 to 10 do
  begin
    // Format SSE : data: ... suivi de deux retours à la ligne
    EventData := Format('data: {"count":%d,"time":"%s"}' + #10#10,
                       [i, FormatDateTime('hh:nn:ss', Now)]);

    // Envoyer l'événement
    AResponse.Content := AResponse.Content + EventData;
    AResponse.SendContent;
    AResponse.Content := ''; // Vider pour le prochain événement

    WriteLn(Format('[%s] Événement %d envoyé', [FormatDateTime('hh:nn:ss', Now), i]));

    // Attendre 2 secondes
    Sleep(2000);
  end;

  // Message de fin
  EventData := 'data: {"type":"end","message":"Flux terminé"}' + #10#10;
  AResponse.Content := AResponse.Content + EventData;
  AResponse.SendContent;

  WriteLn(Format('[%s] Client SSE déconnecté', [FormatDateTime('hh:nn:ss', Now)]));
end;

// Page HTML de démonstration
procedure HandleHome(ARequest: TRequest; AResponse: TResponse);  
begin
  AResponse.Content :=
    '<!DOCTYPE html>' +
    '<html><head><title>Demo SSE</title></head><body>' +
    '<h1>Serveur SSE en fonctionnement</h1>' +
    '<p>Ouvrir <a href="/sse">sse_client.html</a> pour tester</p>' +
    '</body></html>';
end;

begin
  // Enregistrer les routes
  HTTPRouter.Route('/', @HandleHome);
  HTTPRouter.Route('/events', @HandleSSE);

  // Configuration du serveur
  Application.Port := 8080;

  WriteLn('Serveur SSE démarré sur http://localhost:8080');
  WriteLn('Endpoint SSE : http://localhost:8080/events');
  WriteLn('Appuyez sur Ctrl+C pour arrêter');

  Application.Run;
end.
```

### Client HTML pour SSE

**Créer** : `sse_client.html`

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>Client Server-Sent Events</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            max-width: 800px;
            margin: 50px auto;
            padding: 20px;
        }
        #status {
            padding: 15px;
            margin-bottom: 20px;
            border-radius: 5px;
            font-weight: bold;
        }
        .connected { background: #c8e6c9; color: #2e7d32; }
        .disconnected { background: #ffcdd2; color: #c62828; }
        .connecting { background: #fff9c4; color: #f57f17; }

        #events {
            border: 1px solid #ccc;
            padding: 15px;
            height: 400px;
            overflow-y: auto;
            background: #f5f5f5;
            margin-bottom: 15px;
        }
        .event {
            padding: 10px;
            margin: 5px 0;
            background: white;
            border-left: 4px solid #3498db;
            border-radius: 3px;
        }
        .event .time {
            color: #999;
            font-size: 12px;
        }
        .event .data {
            margin-top: 5px;
            font-family: monospace;
        }
        button {
            padding: 10px 20px;
            margin: 5px;
            font-size: 14px;
            cursor: pointer;
            border: none;
            border-radius: 5px;
        }
        .btn-connect { background: #4caf50; color: white; }
        .btn-disconnect { background: #f44336; color: white; }
        .btn-clear { background: #9e9e9e; color: white; }
    </style>
</head>
<body>
    <h1>Client Server-Sent Events</h1>

    <div id="status" class="disconnected">Déconnecté</div>

    <div>
        <button class="btn-connect" onclick="connect()">Connecter</button>
        <button class="btn-disconnect" onclick="disconnect()">Déconnecter</button>
        <button class="btn-clear" onclick="clearEvents()">Effacer</button>
    </div>

    <h2>Événements reçus :</h2>
    <div id="events"></div>

    <script>
        let eventSource = null;
        const eventsDiv = document.getElementById('events');
        const statusDiv = document.getElementById('status');

        function updateStatus(status) {
            statusDiv.className = status;
            switch(status) {
                case 'connected':
                    statusDiv.textContent = '✓ Connecté au flux SSE';
                    break;
                case 'connecting':
                    statusDiv.textContent = '⟳ Connexion en cours...';
                    break;
                case 'disconnected':
                    statusDiv.textContent = '✗ Déconnecté';
                    break;
            }
        }

        function addEvent(data, type = 'message') {
            const div = document.createElement('div');
            div.className = 'event';

            const time = new Date().toLocaleTimeString();
            div.innerHTML = `
                <div class="time">${time} - Type: ${type}</div>
                <div class="data">${JSON.stringify(data, null, 2)}</div>
            `;

            eventsDiv.insertBefore(div, eventsDiv.firstChild);
        }

        function connect() {
            if (eventSource) {
                console.log('Déjà connecté');
                return;
            }

            updateStatus('connecting');

            // Créer la connexion SSE
            eventSource = new EventSource('http://localhost:8080/events');

            // Événement : connexion établie
            eventSource.onopen = function() {
                updateStatus('connected');
                console.log('Connexion SSE établie');
            };

            // Événement : message reçu
            eventSource.onmessage = function(event) {
                try {
                    const data = JSON.parse(event.data);
                    addEvent(data, 'message');
                    console.log('Message reçu:', data);
                } catch(e) {
                    addEvent(event.data, 'text');
                }
            };

            // Événement : erreur
            eventSource.onerror = function(error) {
                console.error('Erreur SSE:', error);

                if (eventSource.readyState === EventSource.CLOSED) {
                    updateStatus('disconnected');
                    eventSource = null;
                } else {
                    updateStatus('connecting');
                }
            };
        }

        function disconnect() {
            if (eventSource) {
                eventSource.close();
                eventSource = null;
                updateStatus('disconnected');
                console.log('Connexion SSE fermée');
            }
        }

        function clearEvents() {
            eventsDiv.innerHTML = '';
        }

        // Connexion automatique au chargement
        window.addEventListener('load', connect);

        // Déconnexion propre avant de quitter
        window.addEventListener('beforeunload', disconnect);
    </script>
</body>
</html>
```

**Tester** :

```bash
# Compiler et lancer
fpc sse_server.lpr
./sse_server

# Ouvrir dans le navigateur
firefox sse_client.html
```

### SSE avec événements personnalisés

Les SSE permettent de définir différents **types d'événements**.

**Serveur avec événements typés** :

```pascal
procedure HandleTypedSSE(ARequest: TRequest; AResponse: TResponse);  
var
  EventData: string;
  i: Integer;
begin
  AResponse.ContentType := 'text/event-stream';
  AResponse.SetCustomHeader('Cache-Control', 'no-cache');
  AResponse.SetCustomHeader('Connection', 'keep-alive');
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');

  // Événement de type "welcome"
  EventData := 'event: welcome' + #10 +
               'data: {"message":"Bienvenue sur le flux SSE"}' + #10#10;
  AResponse.Content := EventData;
  AResponse.SendContent;
  AResponse.Content := '';

  Sleep(1000);

  // Événements de type "update" avec ID
  for i := 1 to 5 do
  begin
    EventData := Format('event: update' + #10 +
                       'id: %d' + #10 +
                       'data: {"value":%d,"status":"ok"}' + #10#10,
                       [i, Random(100)]);

    AResponse.Content := EventData;
    AResponse.SendContent;
    AResponse.Content := '';

    Sleep(2000);
  end;

  // Événement de type "alert"
  EventData := 'event: alert' + #10 +
               'data: {"level":"warning","message":"Attention!"}' + #10#10;
  AResponse.Content := EventData;
  AResponse.SendContent;
  AResponse.Content := '';

  Sleep(1000);

  // Événement de fin
  EventData := 'event: end' + #10 +
               'data: {"message":"Flux terminé"}' + #10#10;
  AResponse.Content := EventData;
  AResponse.SendContent;
end;
```

**Client JavaScript pour événements typés** :

```javascript
const eventSource = new EventSource('http://localhost:8080/events');

// Écouter l'événement "welcome"
eventSource.addEventListener('welcome', function(event) {
    const data = JSON.parse(event.data);
    console.log('Bienvenue:', data.message);
});

// Écouter l'événement "update"
eventSource.addEventListener('update', function(event) {
    const data = JSON.parse(event.data);
    console.log('Mise à jour:', data);

    // L'ID est accessible via event.lastEventId
    console.log('ID événement:', event.lastEventId);
});

// Écouter l'événement "alert"
eventSource.addEventListener('alert', function(event) {
    const data = JSON.parse(event.data);
    alert(`${data.level.toUpperCase()}: ${data.message}`);
});

// Écouter l'événement "end"
eventSource.addEventListener('end', function(event) {
    console.log('Flux terminé');
    eventSource.close();
});
```

### Application temps réel : Dashboard de monitoring

**Serveur de monitoring** : `monitor_server.lpr`

```pascal
program MonitorServer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fpjson,
  fphttpapp, httpdefs, httproute;

type
  TSystemStats = record
    CPUUsage: Double;
    MemoryUsed: Int64;
    MemoryTotal: Int64;
    DiskUsed: Int64;
    DiskTotal: Int64;
    ActiveConnections: Integer;
  end;

function GetSystemStats: TSystemStats;  
begin
  // Simulation de statistiques système
  // En production, utiliser les API système appropriées
  Result.CPUUsage := Random * 100;
  Result.MemoryUsed := Random(8000) + 2000;
  Result.MemoryTotal := 16384;
  Result.DiskUsed := Random(100000) + 50000;
  Result.DiskTotal := 500000;
  Result.ActiveConnections := Random(50) + 10;
end;

function StatsToJSON(const Stats: TSystemStats): string;  
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('cpu_usage', FormatFloat('0.00', Stats.CPUUsage));
    JSONObj.Add('memory_used', Stats.MemoryUsed);
    JSONObj.Add('memory_total', Stats.MemoryTotal);
    JSONObj.Add('memory_percent', FormatFloat('0.0', (Stats.MemoryUsed / Stats.MemoryTotal) * 100));
    JSONObj.Add('disk_used', Stats.DiskUsed);
    JSONObj.Add('disk_total', Stats.DiskTotal);
    JSONObj.Add('disk_percent', FormatFloat('0.0', (Stats.DiskUsed / Stats.DiskTotal) * 100));
    JSONObj.Add('active_connections', Stats.ActiveConnections);
    JSONObj.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));

    Result := JSONObj.AsJSON;
  finally
    JSONObj.Free;
  end;
end;

procedure HandleMonitoring(ARequest: TRequest; AResponse: TResponse);  
var
  Stats: TSystemStats;
  EventData: string;
  Counter: Integer;
begin
  AResponse.ContentType := 'text/event-stream';
  AResponse.SetCustomHeader('Cache-Control', 'no-cache');
  AResponse.SetCustomHeader('Connection', 'keep-alive');
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');

  WriteLn('[Monitoring] Client connecté');

  Counter := 0;

  // Boucle infinie d'envoi de statistiques
  while True do
  begin
    try
      Stats := GetSystemStats;

      EventData := Format('event: stats' + #10 +
                         'id: %d' + #10 +
                         'data: %s' + #10#10,
                         [Counter, StatsToJSON(Stats)]);

      AResponse.Content := EventData;
      AResponse.SendContent;
      AResponse.Content := '';

      Inc(Counter);

      // Envoyer toutes les 3 secondes
      Sleep(3000);
    except
      on E: Exception do
      begin
        WriteLn('[Monitoring] Erreur: ', E.Message);
        Break;
      end;
    end;
  end;

  WriteLn('[Monitoring] Client déconnecté');
end;

// Page HTML intégrée
procedure HandleDashboard(ARequest: TRequest; AResponse: TResponse);  
const
  HTML =
    '<!DOCTYPE html>' +
    '<html><head><meta charset="utf-8"><title>Dashboard Monitoring</title>' +
    '<style>' +
    'body{font-family:Arial,sans-serif;margin:0;padding:20px;background:#1a1a1a;color:#fff;}' +
    'h1{text-align:center;color:#4caf50;}' +
    '.container{max-width:1200px;margin:0 auto;}' +
    '.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:20px;margin-top:30px;}' +
    '.card{background:#2d2d2d;padding:25px;border-radius:10px;box-shadow:0 4px 6px rgba(0,0,0,0.3);}' +
    '.card h2{margin:0 0 15px;font-size:18px;color:#4caf50;}' +
    '.metric{font-size:36px;font-weight:bold;margin:10px 0;}' +
    '.label{color:#999;font-size:14px;}' +
    '.progress{height:20px;background:#1a1a1a;border-radius:10px;overflow:hidden;margin:10px 0;}' +
    '.progress-bar{height:100%;background:linear-gradient(90deg,#4caf50,#8bc34a);transition:width 0.5s;}' +
    '.timestamp{text-align:center;color:#666;margin-top:20px;}' +
    '#status{padding:10px;text-align:center;border-radius:5px;margin-bottom:20px;}' +
    '.connected{background:#2e7d32;}.disconnected{background:#c62828;}' +
    '</style></head><body>' +
    '<div class="container">' +
    '<h1>📊 Dashboard de Monitoring Temps Réel</h1>' +
    '<div id="status" class="disconnected">Déconnecté</div>' +
    '<div class="grid">' +
    '<div class="card">' +
    '<h2>CPU</h2>' +
    '<div class="metric" id="cpuValue">0%</div>' +
    '<div class="progress"><div class="progress-bar" id="cpuBar" style="width:0%"></div></div>' +
    '</div>' +
    '<div class="card">' +
    '<h2>Mémoire</h2>' +
    '<div class="metric" id="memValue">0 MB</div>' +
    '<div class="label" id="memPercent">0%</div>' +
    '<div class="progress"><div class="progress-bar" id="memBar" style="width:0%"></div></div>' +
    '</div>' +
    '<div class="card">' +
    '<h2>Disque</h2>' +
    '<div class="metric" id="diskValue">0 MB</div>' +
    '<div class="label" id="diskPercent">0%</div>' +
    '<div class="progress"><div class="progress-bar" id="diskBar" style="width:0%"></div></div>' +
    '</div>' +
    '<div class="card">' +
    '<h2>Connexions</h2>' +
    '<div class="metric" id="connValue">0</div>' +
    '<div class="label">Connexions actives</div>' +
    '</div>' +
    '</div>' +
    '<div class="timestamp" id="timestamp">Dernière mise à jour: -</div>' +
    '</div>' +
    '<script>' +
    'const es=new EventSource("/monitoring");' +
    'es.addEventListener("stats",e=>{' +
    'const d=JSON.parse(e.data);' +
    'document.getElementById("status").className="connected";' +
    'document.getElementById("status").textContent="✓ Connecté";' +
    'document.getElementById("cpuValue").textContent=d.cpu_usage+"%";' +
    'document.getElementById("cpuBar").style.width=d.cpu_usage+"%";' +
    'document.getElementById("memValue").textContent=d.memory_used+" MB";' +
    'document.getElementById("memPercent").textContent=d.memory_percent+"% utilisé";' +
    'document.getElementById("memBar").style.width=d.memory_percent+"%";' +
    'document.getElementById("diskValue").textContent=d.disk_used+" MB";' +
    'document.getElementById("diskPercent").textContent=d.disk_percent+"% utilisé";' +
    'document.getElementById("diskBar").style.width=d.disk_percent+"%";' +
    'document.getElementById("connValue").textContent=d.active_connections;' +
    'document.getElementById("timestamp").textContent="Dernière mise à jour: "+new Date(d.timestamp).toLocaleTimeString();' +
    '});' +
    'es.onerror=()=>{' +
    'document.getElementById("status").className="disconnected";' +
    'document.getElementById("status").textContent="✗ Déconnecté";' +
    '};' +
    '</script></body></html>';
begin
  AResponse.Content := HTML;
end;

begin
  Randomize;

  HTTPRouter.Route('/', @HandleDashboard);
  HTTPRouter.Route('/monitoring', @HandleMonitoring);

  Application.Port := 8080;

  WriteLn('Dashboard de monitoring démarré sur http://localhost:8080');
  WriteLn('Appuyez sur Ctrl+C pour arrêter');

  Application.Run;
end.
```

**Tester** :

```bash
fpc monitor_server.lpr
./monitor_server
# Ouvrir http://localhost:8080 dans le navigateur
```

Vous verrez un dashboard qui se met à jour automatiquement toutes les 3 secondes !

### Gestion de la reconnexion automatique

Un avantage majeur de SSE est la **reconnexion automatique**.

**Côté serveur - définir le délai de reconnexion** :

```pascal
procedure HandleSSEWithRetry(ARequest: TRequest; AResponse: TResponse);  
var
  EventData: string;
begin
  AResponse.ContentType := 'text/event-stream';
  AResponse.SetCustomHeader('Cache-Control', 'no-cache');

  // Définir le délai de reconnexion (en millisecondes)
  EventData := 'retry: 5000' + #10#10;  // 5 secondes
  AResponse.Content := EventData;
  AResponse.SendContent;
  AResponse.Content := '';

  // Suite des événements...
end;
```

**Côté client - reprendre après déconnexion** :

```javascript
let lastEventId = 0;

const eventSource = new EventSource('http://localhost:8080/events');

eventSource.addEventListener('message', function(event) {
    lastEventId = event.lastEventId;
    console.log('Événement ID:', lastEventId);
    // Traiter les données...
});

eventSource.onerror = function() {
    console.log('Reconnexion automatique...');
    // EventSource se reconnecte automatiquement
    // et envoie le dernier ID reçu dans le header Last-Event-ID
};
```

**Serveur qui reprend depuis le dernier ID** :

```pascal
procedure HandleSSEWithResume(ARequest: TRequest; AResponse: TResponse);  
var
  LastEventID: Integer;
  StartFrom: Integer;
  i: Integer;
  EventData: string;
begin
  AResponse.ContentType := 'text/event-stream';
  AResponse.SetCustomHeader('Cache-Control', 'no-cache');

  // Récupérer le dernier ID reçu par le client
  LastEventID := StrToIntDef(ARequest.GetCustomHeader('Last-Event-ID'), 0);

  WriteLn(Format('Client demande reprise depuis ID: %d', [LastEventID]));

  // Reprendre depuis l'événement suivant
  StartFrom := LastEventID + 1;

  for i := StartFrom to 100 do
  begin
    EventData := Format('id: %d' + #10 +
                       'data: {"event_id":%d,"value":"Event %d"}' + #10#10,
                       [i, i, i]);

    AResponse.Content := EventData;
    AResponse.SendContent;
    AResponse.Content := '';

    Sleep(1000);
  end;
end;
```

## Comparaison pratique : Quand utiliser quoi ?

### Cas d'usage WebSocket

**1. Chat en temps réel**
- Besoin : Communication bidirectionnelle instantanée
- Raison : Les utilisateurs envoient ET reçoivent des messages

**2. Jeux multijoueurs**
- Besoin : Synchronisation en temps réel, faible latence
- Raison : Actions des joueurs doivent être transmises instantanément

**3. Éditeurs collaboratifs**
- Besoin : Modifications synchronisées entre utilisateurs
- Raison : Chaque modification doit être diffusée aux autres

**4. Trading/Finance**
- Besoin : Ordres d'achat/vente + réception des cours
- Raison : Bidirectionnalité avec latence minimale

### Cas d'usage SSE

**1. Flux d'actualités**
- Besoin : Recevoir les nouvelles au fur et à mesure
- Raison : Unidirectionnel, reconnexion automatique

**2. Notifications**
- Besoin : Alertes push depuis le serveur
- Raison : Pas besoin de réponse immédiate

**3. Tableaux de bord**
- Besoin : Métriques qui se mettent à jour
- Raison : Données qui coulent du serveur vers le client

**4. Progression de tâches**
- Besoin : Suivi d'un téléchargement, compilation, etc.
- Raison : Le serveur informe de l'avancement

### Tableau décisionnel

| Besoin | WebSocket | SSE | Polling HTTP |
|--------|-----------|-----|--------------|
| Communication bidirectionnelle | ✅ | ❌ | ⚠️ |
| Serveur → Client uniquement | ✅ | ✅ | ✅ |
| Reconnexion automatique | ⚠️ Manuel | ✅ Auto | ✅ Auto |
| Transmission binaire | ✅ | ❌ | ✅ |
| Simplicité d'implémentation | ⚠️ | ✅ | ✅ |
| Compatible vieux navigateurs | ⚠️ | ⚠️ | ✅ |
| Passe tous les firewalls | ⚠️ | ✅ | ✅ |
| Performance | ✅✅✅ | ✅✅ | ❌ |

## Production et bonnes pratiques

### Sécurité

**1. Authentification pour WebSocket**

```pascal
procedure TMyWebSocketHandler.DoConnect(ASocket: TWebSocketConnection);  
var
  Token: string;
begin
  // Récupérer le token depuis les paramètres
  Token := ASocket.Request.QueryFields.Values['token'];

  if not ValidateToken(Token) then
  begin
    WriteLn('Connexion refusée : token invalide');
    ASocket.Close;
    Exit;
  end;

  // Token valide, continuer
  WriteLn('Connexion autorisée');
end;
```

**2. CORS pour SSE**

```pascal
procedure HandleSSE(ARequest: TRequest; AResponse: TResponse);  
begin
  // Headers CORS
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', 'https://myapp.com');
  AResponse.SetCustomHeader('Access-Control-Allow-Credentials', 'true');

  // Headers SSE
  AResponse.ContentType := 'text/event-stream';
  // ...
end;
```

**3. Validation des messages**

```pascal
procedure TMyWebSocketHandler.DoHandleMessage(ASocket: TWebSocketConnection;
                                               AMessage: TWebSocketMessage);
var
  JSONData: TJSONData;
begin
  try
    // Valider que c'est du JSON
    JSONData := GetJSON(AMessage.AsString);
    try
      // Valider la structure
      if not (JSONData is TJSONObject) then
      begin
        ASocket.SendText('{"error":"Invalid JSON structure"}');
        Exit;
      end;

      // Traiter...
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      ASocket.SendText(Format('{"error":"%s"}', [E.Message]));
  end;
end;
```

### Gestion des ressources

**Limiter le nombre de connexions** :

```pascal
var
  MaxConnections: Integer = 100;
  CurrentConnections: Integer = 0;

procedure TMyWebSocketHandler.DoConnect(ASocket: TWebSocketConnection);  
begin
  if CurrentConnections >= MaxConnections then
  begin
    WriteLn('Limite de connexions atteinte');
    ASocket.SendText('{"error":"Server full"}');
    ASocket.Close;
    Exit;
  end;

  Inc(CurrentConnections);
  WriteLn(Format('Connexions actives: %d/%d', [CurrentConnections, MaxConnections]));
end;

procedure TMyWebSocketHandler.DoDisconnect(ASocket: TWebSocketConnection);  
begin
  Dec(CurrentConnections);
  WriteLn(Format('Connexions actives: %d/%d', [CurrentConnections, MaxConnections]));
end;
```

### Heartbeat / Keep-alive

**WebSocket ping/pong** :

```pascal
type
  TWebSocketHandlerWithHeartbeat = class(TWebSocketHandler)
  private
    FHeartbeatTimer: TTimer;
    procedure OnHeartbeat(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoConnect(ASocket: TWebSocketConnection); override;
  end;

constructor TWebSocketHandlerWithHeartbeat.Create;  
begin
  inherited;
  FHeartbeatTimer := TTimer.Create(nil);
  FHeartbeatTimer.Interval := 30000; // 30 secondes
  FHeartbeatTimer.OnTimer := @OnHeartbeat;
  FHeartbeatTimer.Enabled := True;
end;

procedure TWebSocketHandlerWithHeartbeat.OnHeartbeat(Sender: TObject);  
var
  i: Integer;
begin
  // Envoyer un ping à tous les clients
  for i := 0 to Connections.Count - 1 do
  begin
    try
      Connections[i].SendPing;
    except
      WriteLn('Client ', i, ' ne répond plus');
    end;
  end;
end;
```

**SSE keep-alive** :

```pascal
procedure HandleSSEWithKeepAlive(ARequest: TRequest; AResponse: TResponse);  
var
  LastActivity: TDateTime;
  EventData: string;
  Counter: Integer;
begin
  AResponse.ContentType := 'text/event-stream';
  AResponse.SetCustomHeader('Cache-Control', 'no-cache');
  AResponse.SetCustomHeader('Connection', 'keep-alive');

  LastActivity := Now;
  Counter := 0;

  while True do
  begin
    try
      // Envoyer un commentaire keep-alive toutes les 15 secondes
      if (Now - LastActivity) > (15 / (24 * 60 * 60)) then
      begin
        // Les commentaires en SSE commencent par ":"
        EventData := ': keep-alive' + #10#10;
        AResponse.Content := EventData;
        AResponse.SendContent;
        AResponse.Content := '';
        LastActivity := Now;
      end;

      // Envoyer des données réelles toutes les 30 secondes
      if Counter mod 30 = 0 then
      begin
        EventData := Format('data: {"timestamp":"%s"}' + #10#10,
                           [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
        AResponse.Content := EventData;
        AResponse.SendContent;
        AResponse.Content := '';
        LastActivity := Now;
      end;

      Inc(Counter);
      Sleep(1000);
    except
      Break;
    end;
  end;
end;
```

### Gestion des erreurs robuste

**WebSocket avec gestion d'erreurs** :

```pascal
type
  TRobustWebSocketHandler = class(TWebSocketHandler)
  public
    procedure DoHandleMessage(ASocket: TWebSocketConnection; AMessage: TWebSocketMessage); override;
    procedure DoHandleError(ASocket: TWebSocketConnection; AError: Exception); override;
  end;

procedure TRobustWebSocketHandler.DoHandleMessage(ASocket: TWebSocketConnection;
                                                   AMessage: TWebSocketMessage);
begin
  try
    // Traitement du message
    ProcessMessage(ASocket, AMessage);
  except
    on E: Exception do
    begin
      // Logger l'erreur
      WriteLn(Format('[ERROR] %s: %s', [E.ClassName, E.Message]));

      // Informer le client
      try
        ASocket.SendText(Format('{"error":"%s"}', [E.Message]));
      except
        // Si on ne peut pas envoyer, fermer la connexion
        ASocket.Close;
      end;
    end;
  end;
end;

procedure TRobustWebSocketHandler.DoHandleError(ASocket: TWebSocketConnection; AError: Exception);  
begin
  WriteLn(Format('[WebSocket Error] %s', [AError.Message]));

  // Tenter de fermer proprement
  try
    ASocket.Close;
  except
    // Ignorer les erreurs de fermeture
  end;
end;
```

### Logging structuré

**Système de logging pour WebSocket/SSE** :

```pascal
type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TLogger = class
  private
    FLogFile: TextFile;
    FEnabled: Boolean;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Log(Level: TLogLevel; const Msg: string);
    procedure LogConnection(const ClientIP: string; Connected: Boolean);
    procedure LogMessage(const ClientIP, Msg: string);
    procedure LogError(const ErrorMsg: string);
  end;

constructor TLogger.Create(const FileName: string);  
begin
  inherited Create;
  AssignFile(FLogFile, FileName);
  try
    if FileExists(FileName) then
      Append(FLogFile)
    else
      Rewrite(FLogFile);
    FEnabled := True;
  except
    FEnabled := False;
  end;
end;

destructor TLogger.Destroy;  
begin
  if FEnabled then
    CloseFile(FLogFile);
  inherited;
end;

procedure TLogger.Log(Level: TLogLevel; const Msg: string);  
const
  LevelStr: array[TLogLevel] of string = ('DEBUG', 'INFO', 'WARN', 'ERROR');
var
  LogLine: string;
begin
  if not FEnabled then Exit;

  LogLine := Format('[%s] [%s] %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    LevelStr[Level],
    Msg
  ]);

  WriteLn(LogLine);

  try
    WriteLn(FLogFile, LogLine);
    Flush(FLogFile);
  except
    // Ignorer les erreurs d'écriture
  end;
end;

procedure TLogger.LogConnection(const ClientIP: string; Connected: Boolean);  
begin
  if Connected then
    Log(llInfo, Format('Client connected: %s', [ClientIP]))
  else
    Log(llInfo, Format('Client disconnected: %s', [ClientIP]));
end;

procedure TLogger.LogMessage(const ClientIP, Msg: string);  
begin
  Log(llDebug, Format('Message from %s: %s', [ClientIP, Msg]));
end;

procedure TLogger.LogError(const ErrorMsg: string);  
begin
  Log(llError, ErrorMsg);
end;

var
  GlobalLogger: TLogger;

// Utilisation
begin
  GlobalLogger := TLogger.Create('websocket.log');
  try
    // ... application
  finally
    GlobalLogger.Free;
  end;
end.
```

## Reverse Proxy avec Nginx

Pour la production, utilisez Nginx comme reverse proxy devant votre application FreePascal.

### Configuration Nginx pour WebSocket

**`/etc/nginx/sites-available/websocket-app`** :

```nginx
map $http_upgrade $connection_upgrade {
    default upgrade;
    '' close;
}

upstream websocket_backend {
    server 127.0.0.1:8080;
    keepalive 32;
}

server {
    listen 80;
    server_name ws.example.com;

    # Redirection HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name ws.example.com;

    # SSL
    ssl_certificate /etc/letsencrypt/live/ws.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/ws.example.com/privkey.pem;

    # WebSocket
    location /ws {
        proxy_pass http://websocket_backend;
        proxy_http_version 1.1;

        # Headers WebSocket obligatoires
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;

        # Headers standards
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # Timeouts
        proxy_connect_timeout 7d;
        proxy_send_timeout 7d;
        proxy_read_timeout 7d;
    }

    # Fichiers statiques
    location / {
        root /var/www/websocket-app;
        try_files $uri $uri/ =404;
    }
}
```

### Configuration Nginx pour SSE

**`/etc/nginx/sites-available/sse-app`** :

```nginx
upstream sse_backend {
    server 127.0.0.1:8080;
    keepalive 32;
}

server {
    listen 443 ssl http2;
    server_name sse.example.com;

    # SSL
    ssl_certificate /etc/letsencrypt/live/sse.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/sse.example.com/privkey.pem;

    # SSE endpoint
    location /events {
        proxy_pass http://sse_backend;
        proxy_http_version 1.1;

        # Headers SSE importants
        proxy_set_header Connection '';
        proxy_buffering off;
        proxy_cache off;

        # Headers standards
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;

        # Timeouts longs pour SSE
        proxy_read_timeout 24h;
        proxy_connect_timeout 24h;

        # Types MIME
        types {
            text/event-stream sse;
        }
    }

    # Application web
    location / {
        root /var/www/sse-app;
        try_files $uri $uri/ =404;
    }
}
```

**Activer** :

```bash
sudo ln -s /etc/nginx/sites-available/websocket-app /etc/nginx/sites-enabled/  
sudo nginx -t  
sudo systemctl reload nginx
```

## Déploiement avec systemd

### Service systemd pour WebSocket

**`/etc/systemd/system/websocket-app.service`** :

```ini
[Unit]
Description=WebSocket Application FreePascal  
After=network.target

[Service]
Type=simple  
User=www-data  
Group=www-data  
WorkingDirectory=/var/www/websocket-app

# Exécutable
ExecStart=/var/www/websocket-app/websocket_server

# Redémarrage automatique
Restart=always  
RestartSec=5

# Variables d'environnement
Environment="APP_ENV=production"  
Environment="APP_PORT=8080"

# Limites
LimitNOFILE=65536  
MemoryLimit=512M

# Logging
StandardOutput=journal  
StandardError=journal  
SyslogIdentifier=websocket-app

[Install]
WantedBy=multi-user.target
```

**Activer et démarrer** :

```bash
sudo systemctl daemon-reload  
sudo systemctl enable websocket-app  
sudo systemctl start websocket-app  
sudo systemctl status websocket-app

# Logs
sudo journalctl -u websocket-app -f
```

## Monitoring et métriques

### Endpoint de métriques

```pascal
type
  TMetrics = record
    TotalConnections: Int64;
    ActiveConnections: Integer;
    MessagesSent: Int64;
    MessagesReceived: Int64;
    Errors: Int64;
    Uptime: Int64;
  end;

var
  AppMetrics: TMetrics;
  AppStartTime: TDateTime;

procedure InitMetrics;  
begin
  FillChar(AppMetrics, SizeOf(AppMetrics), 0);
  AppStartTime := Now;
end;

procedure HandleMetricsEndpoint(ARequest: TRequest; AResponse: TResponse);  
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('total_connections', AppMetrics.TotalConnections);
    JSONObj.Add('active_connections', AppMetrics.ActiveConnections);
    JSONObj.Add('messages_sent', AppMetrics.MessagesSent);
    JSONObj.Add('messages_received', AppMetrics.MessagesReceived);
    JSONObj.Add('errors', AppMetrics.Errors);
    JSONObj.Add('uptime_seconds', Round((Now - AppStartTime) * 86400));
    JSONObj.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));

    AResponse.Content := JSONObj.AsJSON;
    AResponse.ContentType := 'application/json';
  finally
    JSONObj.Free;
  end;
end;

// Dans le handler WebSocket
procedure TMetricsWebSocketHandler.DoConnect(ASocket: TWebSocketConnection);  
begin
  Inc(AppMetrics.TotalConnections);
  Inc(AppMetrics.ActiveConnections);
  inherited;
end;

procedure TMetricsWebSocketHandler.DoDisconnect(ASocket: TWebSocketConnection);  
begin
  Dec(AppMetrics.ActiveConnections);
  inherited;
end;

procedure TMetricsWebSocketHandler.DoHandleMessage(ASocket: TWebSocketConnection;
                                                    AMessage: TWebSocketMessage);
begin
  Inc(AppMetrics.MessagesReceived);
  inherited;
  Inc(AppMetrics.MessagesSent);
end;
```

### Script de monitoring

**`monitor.sh`** :

```bash
#!/bin/bash

API_URL="http://localhost:8080"

while true; do
    clear
    echo "=== Monitoring WebSocket App ==="
    echo "Date: $(date '+%Y-%m-%d %H:%M:%S')"
    echo ""

    # Récupérer les métriques
    METRICS=$(curl -s $API_URL/metrics)

    if [ $? -eq 0 ]; then
        echo "✓ Application opérationnelle"
        echo ""
        echo "$METRICS" | jq '.'
    else
        echo "✗ Application non disponible"
    fi

    echo ""
    echo "--- Connexions actives ---"
    netstat -an | grep :8080 | grep ESTABLISHED | wc -l

    echo ""
    echo "--- Mémoire ---"
    ps aux | grep websocket_server | grep -v grep | awk '{print "RSS: " $6/1024 " MB"}'

    echo ""
    echo "Rafraîchissement dans 5 secondes... (Ctrl+C pour arrêter)"
    sleep 5
done
```

## Tests de charge

### Test WebSocket avec websocat

**Installation** :

```bash
# Ubuntu
sudo apt install websocat

# ou depuis les sources
cargo install websocat
```

**Test simple** :

```bash
# Connexion et envoi de message
echo '{"type":"test","message":"Hello"}' | websocat ws://localhost:8080/ws

# Mode interactif
websocat ws://localhost:8080/ws
```

**Test de charge avec script** :

```bash
#!/bin/bash

# Nombre de connexions simultanées
CONNECTIONS=100

echo "Démarrage de $CONNECTIONS connexions WebSocket..."

for i in $(seq 1 $CONNECTIONS); do
    (
        while true; do
            echo "{\"client\":$i,\"message\":\"Test message\"}"
            sleep 1
        done | websocat ws://localhost:8080/ws
    ) &
done

wait
```

### Test SSE avec Apache Bench

```bash
# Test de charge SSE
ab -n 1000 -c 10 -t 60 http://localhost:8080/events

# Options:
# -n 1000 : 1000 requêtes
# -c 10   : 10 connexions simultanées
# -t 60   : Timeout de 60 secondes
```

## Dépannage courant

### WebSocket

**Problème : Connexion refuse**

```
WebSocket connection to 'ws://localhost:8080/ws' failed
```

**Solutions** :
1. Vérifier que le serveur est démarré
2. Vérifier le port (firewall)
3. Vérifier le chemin de la route

**Problème : Connexion se ferme immédiatement**

**Solutions** :
1. Vérifier les logs serveur
2. Problème d'authentification ?
3. Limite de connexions atteinte ?

**Problème : Messages non reçus**

**Solutions** :
1. Vérifier la sérialisation JSON
2. Vérifier les logs d'erreur
3. Tester avec websocat

### SSE

**Problème : Pas de données reçues**

**Solutions** :
1. Vérifier Content-Type: `text/event-stream`
2. Vérifier le format des événements (`data: ...\n\n`)
3. Désactiver le buffering

**Problème : Reconnexion en boucle**

**Solutions** :
1. Vérifier que le serveur ne ferme pas immédiatement
2. Vérifier les headers CORS
3. Augmenter les timeouts

## Exemple complet : Application de notification

### Serveur de notifications

**`notification_server.lpr`** :

```pascal
program NotificationServer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Math, fpjson, fgl,
  fphttpapp, httpdefs, httproute,
  fpwebsocket, websocketserver;

type
  TNotification = record
    ID: Integer;
    Title: string;
    Message: string;
    Priority: string;
    Timestamp: TDateTime;
  end;

  TNotificationList = specialize TFPGList<TNotification>;

  TNotificationHandler = class(TWebSocketHandler)
  private
    FNotifications: TNotificationList;
    FNextID: Integer;
    procedure BroadcastNotification(const Notif: TNotification);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoConnect(ASocket: TWebSocketConnection); override;
    procedure DoHandleMessage(ASocket: TWebSocketConnection; AMessage: TWebSocketMessage); override;
  end;

constructor TNotificationHandler.Create;  
begin
  inherited;
  FNotifications := TNotificationList.Create;
  FNextID := 1;
end;

destructor TNotificationHandler.Destroy;  
begin
  FNotifications.Free;
  inherited;
end;

procedure TNotificationHandler.BroadcastNotification(const Notif: TNotification);  
var
  JSONObj: TJSONObject;
  i: Integer;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('id', Notif.ID);
    JSONObj.Add('title', Notif.Title);
    JSONObj.Add('message', Notif.Message);
    JSONObj.Add('priority', Notif.Priority);
    JSONObj.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Notif.Timestamp));

    // Envoyer à tous les clients connectés
    for i := 0 to Connections.Count - 1 do
    begin
      try
        Connections[i].SendText(JSONObj.AsJSON);
      except
        WriteLn('Erreur envoi au client ', i);
      end;
    end;
  finally
    JSONObj.Free;
  end;
end;

procedure TNotificationHandler.DoConnect(ASocket: TWebSocketConnection);  
var
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  i: Integer;
begin
  WriteLn('Nouveau client connecté');

  // Envoyer les notifications récentes (dernières 10)
  JSONArray := TJSONArray.Create;
  try
    for i := Max(0, FNotifications.Count - 10) to FNotifications.Count - 1 do
    begin
      JSONObj := TJSONObject.Create;
      JSONObj.Add('id', FNotifications[i].ID);
      JSONObj.Add('title', FNotifications[i].Title);
      JSONObj.Add('message', FNotifications[i].Message);
      JSONObj.Add('priority', FNotifications[i].Priority);
      JSONObj.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FNotifications[i].Timestamp));
      JSONArray.Add(JSONObj);
    end;

    JSONObj := TJSONObject.Create;
    try
      JSONObj.Add('type', 'history');
      JSONObj.Add('notifications', JSONArray);
      ASocket.SendText(JSONObj.AsJSON);
    finally
      JSONObj.Free;
    end;
  except
    JSONArray.Free;
    raise;
  end;
end;

procedure TNotificationHandler.DoHandleMessage(ASocket: TWebSocketConnection;
                                                AMessage: TWebSocketMessage);
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  Notif: TNotification;
begin
  if AMessage.MessageType <> mtString then Exit;

  try
    JSONData := GetJSON(AMessage.AsString);
    try
      if not (JSONData is TJSONObject) then Exit;

      JSONObj := TJSONObject(JSONData);

      if JSONObj.Get('type', '') = 'create' then
      begin
        // Créer une nouvelle notification
        Notif.ID := FNextID;
        Inc(FNextID);
        Notif.Title := JSONObj.Get('title', 'Notification');
        Notif.Message := JSONObj.Get('message', '');
        Notif.Priority := JSONObj.Get('priority', 'normal');
        Notif.Timestamp := Now;

        FNotifications.Add(Notif);

        WriteLn(Format('Nouvelle notification #%d: %s', [Notif.ID, Notif.Title]));

        // Diffuser à tous
        BroadcastNotification(Notif);
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur: ', E.Message);
  end;
end;

var
  NotifHandler: TNotificationHandler;

begin
  NotifHandler := TNotificationHandler.Create;

  HTTPRouter.RegisterRoute('/notifications', NotifHandler);

  // Page HTML intégrée
  HTTPRouter.Route('/',
    procedure(ARequest: TRequest; AResponse: TResponse)
    begin
      AResponse.Content :=
        '<!DOCTYPE html><html><head><meta charset="utf-8">' +
        '<title>Notifications</title><style>' +
        'body{font-family:Arial;margin:0;padding:20px;background:#f0f0f0;}' +
        '.notif{background:white;padding:15px;margin:10px 0;border-radius:5px;' +
        'border-left:4px solid #4caf50;box-shadow:0 2px 4px rgba(0,0,0,0.1);}' +
        '.notif.high{border-left-color:#f44336;}' +
        '.notif h3{margin:0 0 5px;color:#333;}' +
        '.notif .time{color:#999;font-size:12px;}' +
        'input,button{padding:10px;margin:5px;font-size:14px;}' +
        '</style></head><body>' +
        '<h1>🔔 Système de Notifications</h1>' +
        '<div><input id="title" placeholder="Titre"/>' +
        '<input id="message" placeholder="Message"/>' +
        '<select id="priority"><option>normal</option><option>high</option></select>' +
        '<button onclick="send()">Envoyer</button></div>' +
        '<div id="notifs"></div>' +
        '<script>' +
        'const ws=new WebSocket("ws://"+location.host+"/notifications");' +
        'ws.onmessage=e=>{' +
        'const d=JSON.parse(e.data);' +
        'if(d.type==="history"){' +
        'd.notifications.forEach(n=>addNotif(n));' +
        '}else{addNotif(d);}};' +
        'function addNotif(n){' +
        'const div=document.createElement("div");' +
        'div.className="notif "+(n.priority==="high"?"high":"");' +
        'div.innerHTML=`<h3>${n.title}</h3><p>${n.message}</p>' +
        '<div class="time">${new Date(n.timestamp).toLocaleString()}</div>`;' +
        'document.getElementById("notifs").insertBefore(div,document.getElementById("notifs").firstChild);}' +
        'function send(){' +
        'ws.send(JSON.stringify({type:"create",title:document.getElementById("title").value,' +
        'message:document.getElementById("message").value,priority:document.getElementById("priority").value}));' +
        'document.getElementById("title").value="";document.getElementById("message").value="";}' +
        '</script></body></html>';
    end
  );

  Application.Port := 8080;

  WriteLn('Serveur de notifications démarré sur http://localhost:8080');
  WriteLn('Appuyez sur Ctrl+C pour arrêter');

  Application.Run;
end.
```

**Tester** :

```bash
fpc notification_server.lpr
./notification_server
# Ouvrir http://localhost:8080 dans plusieurs onglets
# Envoyer une notification depuis un onglet
# Elle apparaît instantanément dans tous les onglets !
```

## Conclusion

WebSockets et Server-Sent Events sont des technologies puissantes pour créer des applications web temps réel avec FreePascal :

### WebSockets
✅ **Communication bidirectionnelle** - Client ↔ Serveur  
✅ **Faible latence** - Idéal pour applications interactives  
✅ **Binaire et texte** - Flexibilité des données  
✅ **Cas d'usage** : Chat, jeux, collaboration temps réel

### Server-Sent Events
✅ **Simplicité** - Plus facile à implémenter  
✅ **Reconnexion automatique** - Robustesse native  
✅ **HTTP standard** - Passe tous les firewalls  
✅ **Cas d'usage** : Notifications, dashboards, flux d'actualités

### Points clés pour la production

🔐 **Sécurité** : Authentification, validation, CORS  
📊 **Monitoring** : Métriques, logs, health checks  
🚀 **Performance** : Keep-alive, gestion des ressources  
🔧 **Infrastructure** : Reverse proxy, systemd, SSL  
🐛 **Dépannage** : Tests de charge, logging structuré

FreePascal offre des outils robustes pour implémenter ces technologies efficacement, permettant de créer des applications web modernes et performantes avec une communication temps réel fluide.

Dans la section suivante, nous explorerons les **templates HTML** et la génération de contenu dynamique pour compléter notre boîte à outils de développement web avec FreePascal.

⏭️ [Templates et génération HTML](/09-programmation-web-freepascal/05-templates-generation-html.md)
