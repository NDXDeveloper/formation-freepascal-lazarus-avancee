🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.6 Clients et serveurs WebSocket

## Introduction aux WebSockets

### Qu'est-ce qu'un WebSocket ?

Un WebSocket est un protocole de communication bidirectionnel qui permet d'établir une connexion permanente entre un client et un serveur. Contrairement au HTTP classique où le client doit constamment demander des informations au serveur, le WebSocket permet au serveur d'envoyer des données au client à tout moment.

**Analogie simple :** Imaginez une conversation téléphonique (WebSocket) versus l'envoi de lettres (HTTP). Avec le téléphone, les deux personnes peuvent parler à tout moment. Avec les lettres, il faut attendre la réponse à chaque envoi.

### Pourquoi utiliser WebSocket ?

Les WebSockets sont particulièrement utiles pour :
- **Applications de chat en temps réel** : Les messages arrivent instantanément
- **Tableaux de bord** : Mise à jour automatique des données
- **Jeux multijoueurs** : Synchronisation rapide entre joueurs
- **Notifications en direct** : Alertes instantanées
- **Collaboration en temps réel** : Édition simultanée de documents

### Avantages par rapport au HTTP classique

| HTTP classique | WebSocket |
|----------------|-----------|
| Le client doit demander les données | Le serveur peut envoyer des données quand il veut |
| Connexion fermée après chaque requête | Connexion maintenue ouverte |
| Plus de consommation réseau (headers répétés) | Moins de données échangées |
| Latence plus élevée | Latence minimale |

## Comment fonctionne WebSocket ?

### Le processus de connexion (Handshake)

1. **Le client envoie une demande HTTP spéciale** avec des en-têtes particuliers
2. **Le serveur accepte** et transforme la connexion HTTP en connexion WebSocket
3. **La connexion reste ouverte** : client et serveur peuvent s'échanger des messages
4. **Fermeture** : L'un des deux peut fermer la connexion proprement

```
Client                          Serveur
  |                               |
  |---(1) HTTP Upgrade Request--->|
  |                               |
  |<--(2) HTTP 101 Switching------|
  |         Protocols             |
  |                               |
  |<==(3) Messages WebSocket=====>|
  |                               |
  |---(4) Close Frame------------>|
  |<--(5) Close Frame-------------|
```

### Structure d'un message WebSocket

Un message WebSocket peut contenir :
- **Du texte** (format UTF-8, souvent JSON)
- **Des données binaires** (images, fichiers, etc.)
- **Des frames de contrôle** (ping/pong, close)

## Bibliothèques WebSocket pour FreePascal

### Options disponibles

1. **fphttpwebsocket** (dans fpWeb) - Inclus avec FreePascal
2. **Synapse WebSocket** - Extension de la bibliothèque Synapse
3. **Brook Framework** - Framework web complet avec WebSocket
4. **WebSocketServer de Warleyalex** - Implémentation standalone

Pour ce tutoriel, nous utiliserons **fphttpwebsocket** car il est inclus par défaut.

## Créer un serveur WebSocket simple

### Installation et configuration

Aucune installation supplémentaire n'est nécessaire si vous avez FreePascal/Lazarus installé.

**Units nécessaires :**
```pascal
uses
  fphttpserver,      // Serveur HTTP de base
  fphttpwebsocket,   // Support WebSocket
  fpjson,            // Manipulation JSON
  SysUtils,          // Utilitaires système
  Classes;           // Classes de base
```

### Structure de base d'un serveur

```pascal
program SimpleWebSocketServer;

{$mode objfpc}{$H+}

uses
  fphttpserver, fphttpwebsocket, SysUtils, Classes;

type
  // Notre gestionnaire de connexions WebSocket
  TMonWebSocketHandler = class(TWebSocketHandler)
  protected
    // Appelé quand un client se connecte
    procedure DoHandleConnect; override;

    // Appelé quand un message arrive
    procedure DoHandleMessage; override;

    // Appelé quand un client se déconnecte
    procedure DoHandleDisconnect; override;
  end;

procedure TMonWebSocketHandler.DoHandleConnect;
begin
  WriteLn('Nouveau client connecté !');
  // Envoyer un message de bienvenue
  Send('Bienvenue sur le serveur WebSocket !');
end;

procedure TMonWebSocketHandler.DoHandleMessage;
var
  MessageRecu: String;
begin
  // Récupérer le message du client
  MessageRecu := Message;
  WriteLn('Message reçu : ', MessageRecu);

  // Renvoyer une réponse
  Send('Serveur dit : J''ai reçu votre message "' + MessageRecu + '"');
end;

procedure TMonWebSocketHandler.DoHandleDisconnect;
begin
  WriteLn('Client déconnecté');
end;

var
  Serveur: TFPHTTPServer;
begin
  // Créer le serveur
  Serveur := TFPHTTPServer.Create(nil);
  try
    // Configuration
    Serveur.Port := 8080;

    // Enregistrer notre gestionnaire WebSocket pour le chemin /ws
    Serveur.RegisterWebSocketHandler('/ws', TMonWebSocketHandler);

    WriteLn('Serveur WebSocket démarré sur le port 8080');
    WriteLn('Connectez-vous sur : ws://localhost:8080/ws');
    WriteLn('Appuyez sur Entrée pour arrêter...');

    // Démarrer le serveur (non-bloquant)
    Serveur.Active := True;

    // Attendre une touche
    ReadLn;

    WriteLn('Arrêt du serveur...');
  finally
    Serveur.Free;
  end;
end.
```

### Explication détaillée du code

**1. La classe TMonWebSocketHandler**

Cette classe hérite de `TWebSocketHandler` et définit comment le serveur réagit aux événements :

- `DoHandleConnect` : Exécuté quand un client se connecte
- `DoHandleMessage` : Exécuté quand un message arrive
- `DoHandleDisconnect` : Exécuté quand un client se déconnecte

**2. La méthode Send**

```pascal
Send('Mon message');
```

Cette méthode envoie un message texte au client connecté. Elle gère automatiquement le format WebSocket.

**3. La propriété Message**

```pascal
MessageRecu := Message;
```

Contient le contenu du dernier message reçu du client.

**4. Configuration du serveur**

```pascal
Serveur.Port := 8080;
Serveur.RegisterWebSocketHandler('/ws', TMonWebSocketHandler);
```

- On définit le port d'écoute
- On associe un chemin (`/ws`) à notre gestionnaire de WebSocket

## Créer un client WebSocket

### Client en HTML/JavaScript (pour tester)

Créez un fichier `test.html` :

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Test WebSocket</title>
</head>
<body>
    <h1>Client WebSocket de test</h1>

    <div>
        <button id="connectBtn">Se connecter</button>
        <button id="disconnectBtn" disabled>Se déconnecter</button>
    </div>

    <div>
        <input type="text" id="messageInput" placeholder="Tapez un message" />
        <button id="sendBtn" disabled>Envoyer</button>
    </div>

    <div>
        <h3>Messages :</h3>
        <div id="messages" style="border: 1px solid #ccc; padding: 10px; height: 300px; overflow-y: auto;"></div>
    </div>

    <script>
        let ws = null;
        const messagesDiv = document.getElementById('messages');

        // Fonction pour afficher un message
        function afficherMessage(msg, type) {
            const p = document.createElement('p');
            p.textContent = `[${type}] ${msg}`;
            p.style.color = type === 'ENVOYÉ' ? 'blue' : 'green';
            messagesDiv.appendChild(p);
            messagesDiv.scrollTop = messagesDiv.scrollHeight;
        }

        // Connexion
        document.getElementById('connectBtn').onclick = function() {
            ws = new WebSocket('ws://localhost:8080/ws');

            ws.onopen = function() {
                afficherMessage('Connecté au serveur !', 'SYSTÈME');
                document.getElementById('connectBtn').disabled = true;
                document.getElementById('disconnectBtn').disabled = false;
                document.getElementById('sendBtn').disabled = false;
            };

            ws.onmessage = function(event) {
                afficherMessage(event.data, 'REÇU');
            };

            ws.onerror = function(error) {
                afficherMessage('Erreur : ' + error, 'ERREUR');
            };

            ws.onclose = function() {
                afficherMessage('Déconnecté du serveur', 'SYSTÈME');
                document.getElementById('connectBtn').disabled = false;
                document.getElementById('disconnectBtn').disabled = true;
                document.getElementById('sendBtn').disabled = true;
            };
        };

        // Déconnexion
        document.getElementById('disconnectBtn').onclick = function() {
            if (ws) ws.close();
        };

        // Envoi de message
        document.getElementById('sendBtn').onclick = function() {
            const input = document.getElementById('messageInput');
            const message = input.value;
            if (message && ws) {
                ws.send(message);
                afficherMessage(message, 'ENVOYÉ');
                input.value = '';
            }
        };

        // Envoi avec la touche Entrée
        document.getElementById('messageInput').onkeypress = function(e) {
            if (e.key === 'Enter') {
                document.getElementById('sendBtn').click();
            }
        };
    </script>
</body>
</html>
```

### Client WebSocket en FreePascal

Voici un client console simple :

```pascal
program SimpleWebSocketClient;

{$mode objfpc}{$H+}

uses
  fphttpclient, fphttpwebsocket, SysUtils, Classes;

type
  TMonClientWebSocket = class(TWebSocketClient)
  protected
    procedure DoHandleMessage; override;
  end;

procedure TMonClientWebSocket.DoHandleMessage;
begin
  WriteLn('Message du serveur : ', Message);
end;

var
  Client: TMonClientWebSocket;
  Ligne: String;
begin
  Client := TMonClientWebSocket.Create(nil);
  try
    WriteLn('Connexion au serveur...');

    // Se connecter au serveur
    Client.Connect('ws://localhost:8080/ws');

    WriteLn('Connecté ! Tapez vos messages (ou "quit" pour quitter) :');

    // Boucle d'envoi de messages
    repeat
      ReadLn(Ligne);

      if Ligne <> 'quit' then
        Client.Send(Ligne);

    until Ligne = 'quit';

    WriteLn('Déconnexion...');
    Client.Disconnect;

  finally
    Client.Free;
  end;
end.
```

## Exemple avancé : Serveur de chat

### Serveur de chat multi-clients

```pascal
program ChatWebSocketServer;

{$mode objfpc}{$H+}

uses
  fphttpserver, fphttpwebsocket, fpjson, SysUtils, Classes, Contnrs;

type
  // Gestionnaire pour chaque connexion client
  TChatHandler = class(TWebSocketHandler)
  private
    FPseudo: String;
  protected
    procedure DoHandleConnect; override;
    procedure DoHandleMessage; override;
    procedure DoHandleDisconnect; override;
  public
    property Pseudo: String read FPseudo write FPseudo;
  end;

var
  // Liste globale de tous les clients connectés
  ClientsConnectes: TObjectList;

procedure DiffuserMessage(const Expediteur, Message: String);
var
  i: Integer;
  Client: TChatHandler;
begin
  // Envoyer le message à tous les clients
  for i := 0 to ClientsConnectes.Count - 1 do
  begin
    Client := TChatHandler(ClientsConnectes[i]);
    Client.Send(Format('[%s] %s', [Expediteur, Message]));
  end;
end;

procedure TChatHandler.DoHandleConnect;
begin
  // Ajouter ce client à la liste
  ClientsConnectes.Add(Self);

  WriteLn('Nouveau client connecté (Total : ', ClientsConnectes.Count, ')');

  // Demander le pseudo
  Send('Bienvenue ! Quel est votre pseudo ?');
end;

procedure TChatHandler.DoHandleMessage;
var
  Msg: String;
begin
  Msg := Message;

  // Si le pseudo n'est pas encore défini, c'est son premier message
  if FPseudo = '' then
  begin
    FPseudo := Msg;
    WriteLn('Nouveau pseudo : ', FPseudo);
    DiffuserMessage('SYSTÈME', FPseudo + ' a rejoint le chat');
  end
  else
  begin
    // Diffuser le message à tous
    WriteLn(Format('[%s] %s', [FPseudo, Msg]));
    DiffuserMessage(FPseudo, Msg);
  end;
end;

procedure TChatHandler.DoHandleDisconnect;
begin
  // Retirer ce client de la liste
  ClientsConnectes.Remove(Self);

  if FPseudo <> '' then
  begin
    WriteLn(FPseudo, ' s''est déconnecté');
    DiffuserMessage('SYSTÈME', FPseudo + ' a quitté le chat');
  end;

  WriteLn('Client déconnecté (Restants : ', ClientsConnectes.Count, ')');
end;

var
  Serveur: TFPHTTPServer;
begin
  // Initialiser la liste des clients
  ClientsConnectes := TObjectList.Create(False); // False = ne pas détruire les objets

  try
    Serveur := TFPHTTPServer.Create(nil);
    try
      Serveur.Port := 8080;
      Serveur.RegisterWebSocketHandler('/chat', TChatHandler);

      WriteLn('=== Serveur de Chat WebSocket ===');
      WriteLn('Port : 8080');
      WriteLn('URL : ws://localhost:8080/chat');
      WriteLn('Appuyez sur Entrée pour arrêter...');
      WriteLn;

      Serveur.Active := True;
      ReadLn;

    finally
      Serveur.Free;
    end;
  finally
    ClientsConnectes.Free;
  end;
end.
```

### Explication du serveur de chat

**1. Liste globale des clients**

```pascal
var
  ClientsConnectes: TObjectList;
```

Cette liste stocke tous les clients actuellement connectés. C'est indispensable pour diffuser les messages à tout le monde.

**2. Fonction de diffusion**

```pascal
procedure DiffuserMessage(const Expediteur, Message: String);
```

Cette fonction parcourt tous les clients et leur envoie le message. C'est le cœur du système de chat.

**3. Gestion du pseudo**

Lors de la première connexion, le serveur demande le pseudo. Le premier message reçu du client est considéré comme son pseudo.

**4. Messages système**

Les messages comme "X a rejoint" ou "X a quitté" sont envoyés par un expéditeur fictif appelé "SYSTÈME".

## Communication avec JSON

### Pourquoi utiliser JSON ?

JSON (JavaScript Object Notation) est un format de données structuré très pratique pour :
- Envoyer plusieurs informations en un seul message
- Typer les messages (chat, notification, commande, etc.)
- Faciliter l'analyse côté client et serveur

### Exemple avec messages JSON

**Serveur :**

```pascal
uses
  fpjson, jsonparser;

procedure TChatHandler.DoHandleMessage;
var
  MsgJSON: TJSONObject;
  TypeMsg, Contenu: String;
  Reponse: TJSONObject;
begin
  try
    // Parser le JSON reçu
    MsgJSON := TJSONObject(GetJSON(Message));
    try
      TypeMsg := MsgJSON.Get('type', '');
      Contenu := MsgJSON.Get('contenu', '');

      // case sur des chaînes n'est pas supporté en FreePascal
      if TypeMsg = 'message' then
      begin
        // Créer une réponse JSON
        Reponse := TJSONObject.Create;
        try
          Reponse.Add('type', 'message');
          Reponse.Add('expediteur', FPseudo);
          Reponse.Add('contenu', Contenu);
          Reponse.Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

          // Diffuser
          DiffuserMessage('', Reponse.AsJSON);
        finally
          Reponse.Free;
        end;
      end
      else if TypeMsg = 'ping' then
      begin
        // Répondre avec un pong
        Reponse := TJSONObject.Create;
        try
          Reponse.Add('type', 'pong');
          Send(Reponse.AsJSON);
        finally
          Reponse.Free;
        end;
      end;

    finally
      MsgJSON.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur parsing JSON : ', E.Message);
  end;
end;
```

**Client JavaScript :**

```javascript
// Envoyer un message
ws.send(JSON.stringify({
    type: 'message',
    contenu: 'Bonjour tout le monde !'
}));

// Recevoir un message
ws.onmessage = function(event) {
    const data = JSON.parse(event.data);

    if (data.type === 'message') {
        console.log(`[${data.timestamp}] ${data.expediteur}: ${data.contenu}`);
    }
};
```

## Gestion des erreurs et reconnexion

### Gestion côté serveur

```pascal
procedure TChatHandler.DoHandleMessage;
begin
  try
    // Traitement du message

  except
    on E: Exception do
    begin
      WriteLn('Erreur : ', E.Message);
      // Envoyer une erreur au client
      Send('{"type":"erreur","message":"' + E.Message + '"}');
    end;
  end;
end;
```

### Reconnexion automatique (client JavaScript)

```javascript
let ws;
let reconnectInterval = 1000; // 1 seconde

function connecter() {
    ws = new WebSocket('ws://localhost:8080/ws');

    ws.onopen = function() {
        console.log('Connecté !');
        reconnectInterval = 1000; // Reset
    };

    ws.onclose = function() {
        console.log('Déconnecté. Reconnexion dans ' + reconnectInterval + 'ms...');
        setTimeout(connecter, reconnectInterval);
        reconnectInterval = Math.min(reconnectInterval * 2, 30000); // Max 30 secondes
    };

    ws.onerror = function(error) {
        console.error('Erreur WebSocket:', error);
    };
}

connecter();
```

## Sécurité WebSocket

### WebSocket sécurisé (WSS)

Pour utiliser WebSocket sur SSL/TLS (wss:// au lieu de ws://), vous devez :

1. Avoir un certificat SSL
2. Configurer le serveur pour utiliser HTTPS
3. Les WebSockets utiliseront automatiquement SSL

```pascal
Serveur.UseSSL := True;
Serveur.CertificateFile := '/chemin/vers/certificat.pem';
Serveur.PrivateKeyFile := '/chemin/vers/cle.pem';
```

### Authentification

**Avec token dans l'URL :**

```javascript
// Client
const token = 'mon_token_secret';
ws = new WebSocket(`ws://localhost:8080/ws?token=${token}`);
```

```pascal
// Serveur
procedure TChatHandler.DoHandleConnect;
var
  Token: String;
begin
  // Récupérer le token depuis les paramètres de l'URL
  Token := GetQueryParam('token');

  if not ValiderToken(Token) then
  begin
    Send('{"type":"erreur","message":"Token invalide"}');
    Disconnect;
    Exit;
  end;

  // Token valide, continuer...
end;
```

### Limitation de débit (Rate Limiting)

```pascal
type
  TChatHandler = class(TWebSocketHandler)
  private
    FDernierMessage: TDateTime;
    FCompteurMessages: Integer;
  protected
    procedure DoHandleMessage; override;
  end;

procedure TChatHandler.DoHandleMessage;
var
  Maintenant: TDateTime;
  DeltaSecondes: Double;
begin
  Maintenant := Now;
  DeltaSecondes := (Maintenant - FDernierMessage) * 86400; // Convertir en secondes

  // Autoriser maximum 5 messages par seconde
  if DeltaSecondes < 1 then
  begin
    Inc(FCompteurMessages);
    if FCompteurMessages > 5 then
    begin
      Send('{"type":"erreur","message":"Trop de messages envoyés"}');
      Exit;
    end;
  end
  else
  begin
    FCompteurMessages := 1;
    FDernierMessage := Maintenant;
  end;

  // Traitement normal du message
end;
```

## Différences Windows / Ubuntu

### Chemins et configurations

**Windows :**
```pascal
{$IFDEF WINDOWS}
  Serveur.CertificateFile := 'C:\certs\certificat.pem';
  Serveur.PrivateKeyFile := 'C:\certs\cle.pem';
{$ENDIF}
```

**Ubuntu/Linux :**
```pascal
{$IFDEF UNIX}
  Serveur.CertificateFile := '/etc/ssl/certs/certificat.pem';
  Serveur.PrivateKeyFile := '/etc/ssl/private/cle.pem';
{$ENDIF}
```

### Pare-feu

**Windows :**
```batch
REM Ouvrir le port 8080 dans le pare-feu Windows
netsh advfirewall firewall add rule name="WebSocket Server" dir=in action=allow protocol=TCP localport=8080
```

**Ubuntu :**
```bash
# Ouvrir le port avec UFW
sudo ufw allow 8080/tcp
sudo ufw reload
```

### Services systèmes

Pour exécuter votre serveur WebSocket comme service :

**Windows (avec NSSM) :**
```batch
nssm install MonServeurWS "C:\chemin\vers\serveur.exe"
nssm start MonServeurWS
```

**Ubuntu (systemd) :**
```ini
# /etc/systemd/system/serveurws.service
[Unit]
Description=Serveur WebSocket
After=network.target

[Service]
Type=simple
User=www-data
ExecStart=/usr/local/bin/serveur
Restart=always

[Install]
WantedBy=multi-user.target
```

```bash
sudo systemctl enable serveurws
sudo systemctl start serveurws
```

## Débogage et tests

### Outils de test

**1. Extension navigateur :**
- "Simple WebSocket Client" (Chrome/Firefox)
- Permet de tester facilement sans écrire de code

**2. wscat (ligne de commande) :**
```bash
# Installation
npm install -g wscat

# Test
wscat -c ws://localhost:8080/ws
```

**3. Logs détaillés :**

```pascal
procedure TChatHandler.DoHandleMessage;
begin
  WriteLn('[', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), '] ',
          'Message de ', FPseudo, ' : ', Message);
  // ...
end;
```

### Problèmes courants

| Problème | Cause | Solution |
|----------|-------|----------|
| Connexion refusée | Serveur non démarré ou mauvais port | Vérifier que le serveur tourne et le bon port |
| Handshake échoue | En-têtes HTTP incorrects | Vérifier la version du protocole WebSocket |
| Messages non reçus | Problème d'encodage | Utiliser UTF-8 partout |
| Déconnexions fréquentes | Timeout réseau | Implémenter ping/pong |

### Ping/Pong pour maintenir la connexion

```pascal
// Serveur - envoyer un ping toutes les 30 secondes
procedure TMonServeur.TimerPing(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ClientsConnectes.Count - 1 do
    TChatHandler(ClientsConnectes[i]).SendPing;
end;
```

## Optimisation et performance

### Limiter la taille des messages

```pascal
procedure TChatHandler.DoHandleMessage;
const
  TAILLE_MAX = 10240; // 10 KB
begin
  if Length(Message) > TAILLE_MAX then
  begin
    Send('{"type":"erreur","message":"Message trop grand"}');
    Exit;
  end;

  // Traitement...
end;
```

### Compression des messages

Pour les gros volumes de données, activez la compression :

```pascal
Serveur.WebSocketCompression := True;
```

### Pool de threads

Pour gérer de nombreux clients simultanés :

```pascal
Serveur.ThreadPoolSize := 10; // 10 threads de traitement
```

## Conclusion

Les WebSockets sont un outil puissant pour créer des applications temps réel. Avec FreePascal/Lazarus, vous pouvez :

- Créer des serveurs WebSocket robustes
- Gérer plusieurs clients simultanément
- Échanger des données structurées avec JSON
- Déployer sur Windows et Ubuntu
- Intégrer avec des clients web modernes

Les concepts présentés ici constituent une base solide. Pour aller plus loin, explorez :
- Les frameworks comme Brook ou mORMot
- L'intégration avec des bases de données
- La mise en place de clusters de serveurs
- Les protocoles binaires personnalisés

**Ressources utiles :**
- Documentation FreePascal : https://www.freepascal.org/docs.html
- Wiki Lazarus : https://wiki.freepascal.org/
- Exemples WebSocket : https://github.com/search?q=freepascal+websocket

⏭️ [Protocoles binaires personnalisés](/10-programmation-reseau-avancee/07-protocoles-binaires-personnalises.md)
