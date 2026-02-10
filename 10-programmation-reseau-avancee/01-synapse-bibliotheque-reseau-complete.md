🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.1 Synapse - Bibliothèque réseau complète

## Introduction

Synapse est une bibliothèque réseau complète et gratuite pour FreePascal et Lazarus. Elle permet de gérer facilement les communications réseau sans dépendre de bibliothèques externes complexes. Synapse est particulièrement appréciée pour sa simplicité d'utilisation et sa portabilité entre Windows et Linux.

### Pourquoi choisir Synapse ?

- **Simple à utiliser** : API intuitive et bien documentée
- **Multi-plateforme** : Fonctionne sur Windows, Linux, macOS, BSD
- **Légère** : Pas de dépendances externes lourdes
- **Complète** : Support de nombreux protocoles (TCP, UDP, HTTP, SMTP, POP3, FTP, etc.)
- **Stable** : Utilisée dans de nombreux projets professionnels depuis des années
- **Open Source** : Code source disponible et modifiable

## Installation de Synapse

### Sur Windows

1. Téléchargez Synapse depuis le site officiel : http://www.ararat.cz/synapse/
2. Extrayez l'archive dans un dossier (par exemple : `C:\synapse\`)
3. Dans Lazarus, allez dans **Projet → Options du projet → Chemins du compilateur**
4. Ajoutez le chemin vers le dossier `source` de Synapse dans "Autres chemins de sources"

### Sur Ubuntu/Linux

```bash
# Méthode 1 : Installation via le gestionnaire de paquets Lazarus
# Ouvrez Lazarus → Paquets → Installer/Désinstaller des paquets
# Recherchez "synapse" et installez-le

# Méthode 2 : Installation manuelle
cd ~/  
wget http://www.ararat.cz/synapse/dl/synapse40.zip  
unzip synapse40.zip
# Puis configurez le chemin dans Lazarus comme pour Windows
```

### Vérification de l'installation

Créez un petit programme de test :

```pascal
program TestSynapse;

uses
  blcksock; // Unité de base de Synapse

begin
  WriteLn('Synapse est correctement installé !');
end.
```

Si le programme compile sans erreur, Synapse est bien installé.

## Concepts de base

### Les unités principales de Synapse

Synapse est organisée en plusieurs unités spécialisées :

- **blcksock** : Sockets TCP/UDP de base (unité principale)
- **httpsend** : Client HTTP/HTTPS
- **smtpsend** : Envoi d'emails (SMTP)
- **pop3send** : Réception d'emails (POP3)
- **ftpsend** : Client FTP
- **dnssend** : Résolution DNS
- **snmpsend** : Protocole SNMP
- **synautil** : Fonctions utilitaires

### Architecture de Synapse

Synapse utilise une approche **bloquante** (blocking sockets) par défaut, ce qui signifie que les opérations réseau attendent la fin de leur exécution. C'est plus simple à comprendre pour débuter, mais nécessite l'usage de threads pour des applications complexes.

```
Application
    ↓
Unités Synapse (httpsend, smtpsend, etc.)
    ↓
blcksock (couche socket de base)
    ↓
API système (WinSock sur Windows, BSD sockets sur Linux)
```

## Premier exemple : Client TCP simple

Voici un exemple de client TCP qui se connecte à un serveur et envoie/reçoit des données :

```pascal
program SimpleTCPClient;

uses
  SysUtils,
  blcksock; // Unité de base pour les sockets

var
  Socket: TTCPBlockSocket;
  Response: string;

begin
  // Création du socket
  Socket := TTCPBlockSocket.Create;
  try
    WriteLn('Connexion au serveur...');

    // Connexion au serveur (exemple : un serveur echo sur le port 7)
    Socket.Connect('localhost', '7');

    if Socket.LastError = 0 then
    begin
      WriteLn('Connecté avec succès !');

      // Envoi de données
      Socket.SendString('Bonjour serveur' + #13#10);
      WriteLn('Message envoyé.');

      // Réception de la réponse
      Response := Socket.RecvString(5000); // Timeout de 5 secondes
      WriteLn('Réponse reçue : ', Response);
    end
    else
      WriteLn('Erreur de connexion : ', Socket.LastErrorDesc);

  finally
    Socket.Free;
  end;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Explications du code

1. **TTCPBlockSocket** : C'est la classe principale pour les connexions TCP
2. **Connect** : Établit une connexion vers un serveur (adresse + port)
3. **LastError** : Contient le code d'erreur de la dernière opération (0 = succès)
4. **SendString** : Envoie une chaîne de caractères
5. **RecvString** : Reçoit des données avec un timeout (en millisecondes)

## Serveur TCP simple

Voici comment créer un serveur TCP basique :

```pascal
program SimpleTCPServer;

uses
  SysUtils,
  blcksock;

var
  ListenSocket, ClientSocket: TTCPBlockSocket;
  ReceivedData: string;

begin
  ListenSocket := TTCPBlockSocket.Create;
  try
    // Liaison du socket au port 8080
    ListenSocket.CreateSocket;
    ListenSocket.SetLinger(True, 10);
    ListenSocket.Bind('0.0.0.0', '8080'); // Écoute sur toutes les interfaces
    ListenSocket.Listen;

    WriteLn('Serveur en écoute sur le port 8080...');
    WriteLn('Appuyez sur Ctrl+C pour arrêter.');

    // Boucle d'acceptation des connexions
    while True do
    begin
      if ListenSocket.CanRead(1000) then
      begin
        ClientSocket := TTCPBlockSocket.Create;
        try
          ClientSocket.Socket := ListenSocket.Accept;
          WriteLn('Client connecté depuis : ', ClientSocket.GetRemoteSinIP);

          // Réception des données
          ReceivedData := ClientSocket.RecvString(5000);
          WriteLn('Données reçues : ', ReceivedData);

          // Renvoi des données (serveur echo)
          ClientSocket.SendString(ReceivedData);

        finally
          ClientSocket.Free;
        end;
      end;
    end;

  finally
    ListenSocket.Free;
  end;
end.
```

### Explications du code serveur

1. **Bind** : Associe le socket à une adresse IP et un port
2. **Listen** : Met le socket en mode écoute
3. **Accept** : Accepte une connexion entrante et retourne un nouveau socket
4. **CanRead** : Vérifie si des données sont disponibles (avec timeout)
5. **GetRemoteSinIP** : Obtient l'adresse IP du client connecté

## Client HTTP avec Synapse

L'unité `httpsend` simplifie considérablement les requêtes HTTP :

```pascal
program SimpleHTTPClient;

uses
  SysUtils, Classes,
  httpsend; // Unité HTTP de Synapse

var
  HTTP: THTTPSend;
  Response: TStringList;

begin
  HTTP := THTTPSend.Create;
  Response := TStringList.Create;
  try
    // Requête GET simple
    WriteLn('Téléchargement de la page...');

    if HTTP.HTTPMethod('GET', 'http://www.example.com') then
    begin
      // Lecture de la réponse
      Response.LoadFromStream(HTTP.Document);

      WriteLn('Code HTTP : ', HTTP.ResultCode);
      WriteLn('Taille : ', Response.Count, ' lignes');
      WriteLn('---');
      WriteLn(Response.Text);
    end
    else
      WriteLn('Erreur HTTP');

  finally
    Response.Free;
    HTTP.Free;
  end;

  ReadLn;
end.
```

### Requête POST avec données

```pascal
procedure EnvoyerDonneesPost;  
var
  HTTP: THTTPSend;
  PostData: TStringList;
begin
  HTTP := THTTPSend.Create;
  PostData := TStringList.Create;
  try
    // Préparation des données POST
    PostData.Add('nom=Dupont');
    PostData.Add('prenom=Jean');

    // Écriture dans le document HTTP
    PostData.SaveToStream(HTTP.Document);

    // Définition du type de contenu
    HTTP.MimeType := 'application/x-www-form-urlencoded';

    // Envoi de la requête POST
    if HTTP.HTTPMethod('POST', 'http://example.com/api/users') then
      WriteLn('Succès : ', HTTP.ResultCode)
    else
      WriteLn('Échec');

  finally
    PostData.Free;
    HTTP.Free;
  end;
end;
```

## Envoi d'emails avec SMTP

L'unité `smtpsend` permet d'envoyer des emails facilement :

```pascal
program SendEmail;

uses
  SysUtils,
  smtpsend,
  mimemess, // Pour créer le message MIME
  mimepart;

function EnvoyerEmail(const Destinataire, Sujet, Corps: string): Boolean;  
var
  SMTP: TSMTPSend;
  Msg: TMimeMess;
begin
  Result := False;

  SMTP := TSMTPSend.Create;
  Msg := TMimeMess.Create;
  try
    // Configuration du serveur SMTP
    SMTP.TargetHost := 'smtp.example.com';
    SMTP.TargetPort := '587'; // Port SMTP standard
    SMTP.Username := 'votre-email@example.com';
    SMTP.Password := 'votre-mot-de-passe';

    // Création du message
    Msg.Header.From := 'votre-email@example.com';
    Msg.Header.ToList.Add(Destinataire);
    Msg.Header.Subject := Sujet;
    Msg.Header.CharsetCode := 'UTF-8';

    // Corps du message
    with Msg.AddPartText(nil) do
    begin
      SetText(Corps);
      CharsetCode := 'UTF-8';
      EncodingCode := 'quoted-printable';
    end;

    Msg.EncodeMessage;

    // Connexion et envoi
    if SMTP.Login then
    begin
      if SMTP.MailFrom(Msg.Header.From, Length(Msg.Lines.Text)) then
      begin
        if SMTP.MailTo(Destinataire) then
        begin
          if SMTP.MailData(Msg.Lines) then
            Result := True;
        end;
      end;
      SMTP.Logout;
    end;

  finally
    Msg.Free;
    SMTP.Free;
  end;
end;

begin
  if EnvoyerEmail('destinataire@example.com',
                  'Test Synapse',
                  'Ceci est un email de test envoyé avec Synapse !') then
    WriteLn('Email envoyé avec succès !')
  else
    WriteLn('Erreur lors de l''envoi de l''email');

  ReadLn;
end.
```

## Gestion des erreurs

Synapse fournit des propriétés pour vérifier les erreurs :

```pascal
procedure ExempleGestionErreurs;  
var
  Socket: TTCPBlockSocket;
begin
  Socket := TTCPBlockSocket.Create;
  try
    Socket.Connect('serveur-inexistant.com', '80');

    // Vérification des erreurs
    if Socket.LastError <> 0 then
    begin
      WriteLn('Code d''erreur : ', Socket.LastError);
      WriteLn('Description : ', Socket.LastErrorDesc);

      // Codes d'erreur courants :
      // 0 = Pas d'erreur
      // 10060 = Timeout de connexion (Windows)
      // 110 = Timeout de connexion (Linux)
      // 10061 = Connexion refusée (Windows)
      // 111 = Connexion refusée (Linux)
    end;

  finally
    Socket.Free;
  end;
end;
```

## Timeouts et délais

Synapse permet de configurer les timeouts pour éviter que votre programme ne se bloque :

```pascal
procedure ExempleTimeouts;  
var
  Socket: TTCPBlockSocket;
begin
  Socket := TTCPBlockSocket.Create;
  try
    // Configuration des timeouts (en millisecondes)
    Socket.ConnectionTimeout := 5000;  // 5 secondes pour la connexion
    Socket.SendTimeout := 10000;       // 10 secondes pour l'envoi
    Socket.ReceiveTimeout := 10000;    // 10 secondes pour la réception

    Socket.Connect('www.example.com', '80');

    // Envoi avec timeout
    Socket.SendString('GET / HTTP/1.0' + #13#10#13#10);

    // Réception avec timeout
    WriteLn(Socket.RecvString(5000));

  finally
    Socket.Free;
  end;
end;
```

## Sockets UDP

Synapse supporte également les sockets UDP pour les communications sans connexion :

```pascal
program UDPExample;

uses
  SysUtils,
  blcksock;

// Émetteur UDP
procedure EnvoyerUDP(const Host, Port, Message: string);  
var
  Socket: TUDPBlockSocket;
begin
  Socket := TUDPBlockSocket.Create;
  try
    Socket.Connect(Host, Port);
    Socket.SendString(Message);
    WriteLn('Message UDP envoyé');
  finally
    Socket.Free;
  end;
end;

// Récepteur UDP
procedure RecevoirUDP(const Port: string);  
var
  Socket: TUDPBlockSocket;
  ReceivedData: string;
begin
  Socket := TUDPBlockSocket.Create;
  try
    Socket.CreateSocket;
    Socket.Bind('0.0.0.0', Port);
    WriteLn('En écoute sur le port UDP ', Port);

    while True do
    begin
      ReceivedData := Socket.RecvString(10000);
      if Socket.LastError = 0 then
        WriteLn('Reçu : ', ReceivedData)
      else
        Break;
    end;

  finally
    Socket.Free;
  end;
end;

begin
  // Exemple d'utilisation
  EnvoyerUDP('localhost', '9999', 'Message de test UDP');
  ReadLn;
end.
```

## Utilisation avec des threads

Pour créer des applications réseau non-bloquantes, utilisez des threads :

```pascal
type
  TClientThread = class(TThread)
  private
    FSocket: TTCPBlockSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TTCPBlockSocket);
    destructor Destroy; override;
  end;

constructor TClientThread.Create(ASocket: TTCPBlockSocket);  
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FSocket := ASocket;
end;

destructor TClientThread.Destroy;  
begin
  FSocket.Free;
  inherited;
end;

procedure TClientThread.Execute;  
var
  Data: string;
begin
  try
    // Traitement du client dans un thread séparé
    Data := FSocket.RecvString(30000);
    FSocket.SendString('Réponse : ' + Data);
  except
    on E: Exception do
      WriteLn('Erreur thread : ', E.Message);
  end;
end;

// Utilisation dans un serveur multi-thread
procedure ServeurMultiThread;  
var
  ListenSocket, ClientSocket: TTCPBlockSocket;
begin
  ListenSocket := TTCPBlockSocket.Create;
  try
    ListenSocket.CreateSocket;
    ListenSocket.Bind('0.0.0.0', '8080');
    ListenSocket.Listen;

    WriteLn('Serveur multi-thread en écoute...');

    while True do
    begin
      if ListenSocket.CanRead(1000) then
      begin
        ClientSocket := TTCPBlockSocket.Create;
        ClientSocket.Socket := ListenSocket.Accept;

        // Création d'un thread pour chaque client
        TClientThread.Create(ClientSocket);
      end;
    end;

  finally
    ListenSocket.Free;
  end;
end;
```

## Bonnes pratiques

### 1. Toujours libérer les ressources

```pascal
Socket := TTCPBlockSocket.Create;  
try
  // Votre code ici
finally
  Socket.Free; // Toujours dans un bloc finally
end;
```

### 2. Vérifier les erreurs

```pascal
Socket.Connect(Host, Port);  
if Socket.LastError <> 0 then  
begin
  // Gérer l'erreur
  Exit;
end;
```

### 3. Utiliser des timeouts appropriés

```pascal
Socket.ConnectionTimeout := 10000; // 10 secondes  
Socket.ReceiveTimeout := 30000;    // 30 secondes pour les opérations longues
```

### 4. Gérer les fins de ligne correctement

```pascal
// Windows utilise #13#10 (CRLF)
// Linux utilise #10 (LF)
// HTTP utilise toujours #13#10

Socket.SendString('Commande' + #13#10); // Standard pour les protocoles réseau
```

## Différences Windows/Linux

Synapse gère automatiquement la plupart des différences, mais voici quelques points à connaître :

### Codes d'erreur

Les codes d'erreur peuvent différer :

```pascal
// Windows : 10060 = Connection timeout
// Linux : 110 = Connection timeout

if (Socket.LastError = 10060) or (Socket.LastError = 110) then
  WriteLn('Timeout de connexion');
```

### Permissions sur Linux

Sur Linux, les ports < 1024 nécessitent les privilèges root :

```bash
# Pour exécuter un serveur sur le port 80 sans root
sudo setcap 'cap_net_bind_service=+ep' /chemin/vers/votre/programme
```

### Firewall

N'oubliez pas de configurer le firewall :

**Windows :**
```
netsh advfirewall firewall add rule name="Mon App" dir=in action=allow protocol=TCP localport=8080
```

**Ubuntu :**
```bash
sudo ufw allow 8080/tcp
```

## Ressources et documentation

- **Site officiel** : http://www.ararat.cz/synapse/
- **Documentation** : Incluse dans l'archive Synapse (dossier `doc`)
- **Forum FreePascal** : https://forum.lazarus.freepascal.org/
- **Exemples** : Dossier `examples` dans l'archive Synapse

## Conclusion

Synapse est une bibliothèque puissante et accessible qui permet de créer rapidement des applications réseau multi-plateformes avec FreePascal et Lazarus. Sa simplicité d'utilisation et sa stabilité en font un excellent choix pour les débutants comme pour les développeurs avancés.

Les concepts présentés dans ce chapitre constituent une base solide pour explorer des protocoles plus complexes et développer des applications réseau professionnelles.

⏭️ [Indy pour Lazarus](/10-programmation-reseau-avancee/02-indy-pour-lazarus.md)
