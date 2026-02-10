🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.2 Indy pour Lazarus

## Introduction

Indy (Internet Direct) est une bibliothèque réseau très populaire, initialement développée pour Delphi et portée sur Lazarus/FreePascal. Elle offre une approche orientée composants pour la programmation réseau, ce qui la rend particulièrement adaptée aux applications visuelles avec interface graphique.

### Qu'est-ce qu'Indy ?

Indy est un ensemble de composants réseau qui couvrent une large gamme de protocoles Internet. Contrairement à Synapse qui utilise principalement du code procédural, Indy adopte une approche **orientée objet** avec des composants que vous pouvez déposer directement sur vos formulaires Lazarus.

### Avantages d'Indy

- **Composants visuels** : Glissez-déposez des composants sur vos formulaires
- **Riche en protocoles** : Support natif de HTTP, FTP, SMTP, POP3, IMAP, NNTP, et bien d'autres
- **Événements intégrés** : Programmation pilotée par événements (OnConnect, OnReceive, etc.)
- **Support SSL/TLS** : Sécurisation facile des connexions
- **Documentation abondante** : Grande communauté Delphi/Lazarus
- **Architecture client/serveur** : Composants séparés pour clients et serveurs

### Indy vs Synapse

| Caractéristique | Indy | Synapse |
|----------------|------|---------|
| Approche | Composants visuels | Code procédural |
| Courbe d'apprentissage | Facile pour débutants | Moyenne |
| Interface graphique | Excellente | Nécessite plus de code |
| Légèreté | Plus lourd | Très léger |
| Documentation | Très riche | Bonne |
| Protocoles | Très nombreux | Nombreux |

## Installation d'Indy pour Lazarus

### Sur Windows

#### Méthode 1 : Via Online Package Manager (Recommandée)

1. Ouvrez Lazarus
2. Allez dans **Paquets → Gestionnaire de paquets en ligne**
3. Recherchez "**Indy**" ou "**IndyLaz**"
4. Sélectionnez le paquet **Indy10** pour Lazarus
5. Cliquez sur **Installer**
6. Lazarus va télécharger et compiler automatiquement
7. Redémarrez Lazarus après l'installation

#### Méthode 2 : Installation manuelle

```bash
# Téléchargez depuis GitHub
https://github.com/IndySockets/Indy

# Décompressez dans un dossier (ex: C:\Indy)
# Dans Lazarus :
# Paquets → Ouvrir un fichier paquet (.lpk)
# Naviguez vers : C:\Indy\Lib\Protocols\indylaz.lpk
# Cliquez sur "Compiler" puis "Utiliser → Installer"
```

### Sur Ubuntu/Linux

```bash
# Méthode 1 : Via apt (si disponible)
sudo apt-get install lazarus-indy

# Méthode 2 : Compilation depuis les sources
cd ~/  
git clone https://github.com/IndySockets/Indy.git  
cd Indy/Lib/Protocols

# Ouvrez Lazarus
# Paquets → Ouvrir un fichier paquet
# Sélectionnez : indylaz.lpk
# Compiler → Utiliser → Installer
```

### Vérification de l'installation

Après l'installation, vous devriez voir une nouvelle palette **Indy Clients**, **Indy Servers**, **Indy Misc** dans Lazarus :

```pascal
program TestIndy;
{$mode objfpc}{$H+}

uses
  IdHTTP; // Si cette unité compile, Indy est installé

begin
  WriteLn('Indy est correctement installé !');
end.
```

## Architecture d'Indy

### Structure des composants

Indy est organisé en plusieurs catégories :

```
Indy Clients (TIdHTTP, TIdFTP, TIdSMTP, etc.)
    ↓
Indy Servers (TIdHTTPServer, TIdTCPServer, etc.)
    ↓
Indy Misc (TIdSSLIOHandlerSocketOpenSSL, etc.)
    ↓
Couche TCP/IP de base
```

### Concepts clés

1. **IO Handlers** : Gèrent les entrées/sorties (standard, SSL, compression)
2. **Codecs** : Encodage/décodage (Base64, MIME, URL)
3. **Thread Management** : Gestion automatique des threads pour les serveurs
4. **Component Hierarchy** : Hiérarchie claire des composants

## Premier exemple : Client HTTP simple

### Version console

```pascal
program SimpleHTTPClient;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  IdHTTP;  // Client HTTP d'Indy

var
  HTTP: TIdHTTP;
  Response: string;

begin
  HTTP := TIdHTTP.Create(nil);
  try
    WriteLn('Téléchargement de la page...');

    // Requête GET simple
    Response := HTTP.Get('http://www.example.com');

    WriteLn('Code de réponse : ', HTTP.ResponseCode);
    WriteLn('Longueur : ', Length(Response), ' caractères');
    WriteLn('---');
    WriteLn(Copy(Response, 1, 500)); // Affiche les 500 premiers caractères

  finally
    HTTP.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Version graphique (avec formulaire)

Créez un nouveau projet d'application dans Lazarus :

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IdHTTP;  // Composant HTTP d'Indy

type
  TForm1 = class(TForm)
    ButtonTelecharger: TButton;
    EditURL: TEdit;
    MemoResultat: TMemo;
    Label1: TLabel;
    procedure ButtonTelechargerClick(Sender: TObject);
  private
    IdHTTP1: TIdHTTP;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

constructor TForm1.Create(TheOwner: TComponent);  
begin
  inherited Create(TheOwner);

  // Création du composant HTTP (méthode programmatique)
  IdHTTP1 := TIdHTTP.Create(Self);
end;

destructor TForm1.Destroy;  
begin
  IdHTTP1.Free;
  inherited Destroy;
end;

procedure TForm1.ButtonTelechargerClick(Sender: TObject);  
var
  Response: string;
begin
  MemoResultat.Clear;
  MemoResultat.Lines.Add('Téléchargement en cours...');
  Application.ProcessMessages;

  try
    // Téléchargement de la page
    Response := IdHTTP1.Get(EditURL.Text);

    // Affichage du résultat
    MemoResultat.Clear;
    MemoResultat.Lines.Add('Code HTTP : ' + IntToStr(IdHTTP1.ResponseCode));
    MemoResultat.Lines.Add('Taille : ' + IntToStr(Length(Response)) + ' octets');
    MemoResultat.Lines.Add('---');
    MemoResultat.Lines.Add(Response);

  except
    on E: Exception do
      MemoResultat.Lines.Add('Erreur : ' + E.Message);
  end;
end;

end.
```

### Utilisation des composants visuels

Vous pouvez également déposer un composant `TIdHTTP` directement sur votre formulaire :

1. Depuis la palette **Indy Clients**, glissez `TIdHTTP` sur votre formulaire
2. Nommez-le `IdHTTP1`
3. Utilisez-le directement sans créer d'instance

```pascal
procedure TForm1.ButtonClick(Sender: TObject);  
begin
  // Le composant IdHTTP1 existe déjà sur le formulaire
  Memo1.Text := IdHTTP1.Get('http://www.example.com');
end;
```

## Requêtes HTTP avancées

### GET avec paramètres

```pascal
procedure RequeteGETAvecParametres;  
var
  HTTP: TIdHTTP;
  URL: string;
begin
  HTTP := TIdHTTP.Create(nil);
  try
    // Construction de l'URL avec paramètres
    URL := 'http://api.example.com/search?q=lazarus&limit=10';

    // Envoi de la requête
    Memo1.Text := HTTP.Get(URL);

  finally
    HTTP.Free;
  end;
end;
```

### POST avec données

```pascal
procedure RequetePOST;  
var
  HTTP: TIdHTTP;
  PostData, Response: TStringStream;
begin
  HTTP := TIdHTTP.Create(nil);
  PostData := TStringStream.Create('');
  Response := TStringStream.Create('');
  try
    // Préparation des données POST
    PostData.WriteString('nom=Dupont&prenom=Jean&age=30');

    // Configuration du type de contenu
    HTTP.Request.ContentType := 'application/x-www-form-urlencoded';

    // Envoi de la requête POST
    HTTP.Post('http://api.example.com/users', PostData, Response);

    // Affichage de la réponse
    ShowMessage('Réponse : ' + Response.DataString);

  finally
    Response.Free;
    PostData.Free;
    HTTP.Free;
  end;
end;
```

### POST JSON

```pascal
procedure EnvoyerJSON;  
var
  HTTP: TIdHTTP;
  JSONData, Response: TStringStream;
begin
  HTTP := TIdHTTP.Create(nil);
  JSONData := TStringStream.Create('');
  Response := TStringStream.Create('');
  try
    // Création du JSON
    JSONData.WriteString('{"nom":"Dupont","prenom":"Jean","age":30}');

    // Configuration du type de contenu JSON
    HTTP.Request.ContentType := 'application/json';
    HTTP.Request.CharSet := 'utf-8';

    // Envoi
    HTTP.Post('http://api.example.com/users', JSONData, Response);

    ShowMessage('Réponse JSON : ' + Response.DataString);

  finally
    Response.Free;
    JSONData.Free;
    HTTP.Free;
  end;
end;
```

### Téléchargement de fichiers

```pascal
procedure TelechargerFichier(const URL, CheminLocal: string);  
var
  HTTP: TIdHTTP;
  FileStream: TFileStream;
begin
  HTTP := TIdHTTP.Create(nil);
  FileStream := TFileStream.Create(CheminLocal, fmCreate);
  try
    WriteLn('Téléchargement de : ', URL);

    // Téléchargement direct dans le fichier
    HTTP.Get(URL, FileStream);

    WriteLn('Fichier sauvegardé : ', CheminLocal);
    WriteLn('Taille : ', FileStream.Size, ' octets');

  finally
    FileStream.Free;
    HTTP.Free;
  end;
end;

// Utilisation
begin
  TelechargerFichier('http://example.com/file.zip', 'C:\temp\file.zip');
end;
```

### En-têtes HTTP personnalisés

```pascal
procedure RequeteAvecEnTetes;  
var
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create(nil);
  try
    // Ajout d'en-têtes personnalisés
    HTTP.Request.CustomHeaders.Add('X-API-Key: votre-cle-api');
    HTTP.Request.CustomHeaders.Add('X-Custom-Header: valeur');
    HTTP.Request.UserAgent := 'MonApplication/1.0';

    // Requête avec en-têtes
    Memo1.Text := HTTP.Get('http://api.example.com/data');

  finally
    HTTP.Free;
  end;
end;
```

## Client FTP avec Indy

```pascal
program SimpleFTPClient;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  IdFTP;

procedure ExempleFTP;  
var
  FTP: TIdFTP;
  FilesList: TStringList;
  i: Integer;
begin
  FTP := TIdFTP.Create(nil);
  FilesList := TStringList.Create;
  try
    // Configuration de la connexion
    FTP.Host := 'ftp.example.com';
    FTP.Username := 'utilisateur';
    FTP.Password := 'motdepasse';
    FTP.Port := 21; // Port FTP standard

    WriteLn('Connexion au serveur FTP...');
    FTP.Connect;

    if FTP.Connected then
    begin
      WriteLn('Connecté avec succès !');

      // Liste des fichiers du répertoire actuel
      FTP.List(FilesList, '', False);

      WriteLn('Fichiers sur le serveur :');
      for i := 0 to FilesList.Count - 1 do
        WriteLn('  ', FilesList[i]);

      // Téléchargement d'un fichier
      WriteLn('Téléchargement de fichier.txt...');
      FTP.Get('fichier.txt', 'C:\temp\fichier.txt', True);

      // Upload d'un fichier
      WriteLn('Envoi de document.pdf...');
      FTP.Put('C:\documents\document.pdf', 'document.pdf');

      // Déconnexion
      FTP.Disconnect;
      WriteLn('Déconnecté.');
    end;

  finally
    FilesList.Free;
    FTP.Free;
  end;
end;

begin
  ExempleFTP;
  ReadLn;
end.
```

### Gestion des événements FTP

```pascal
type
  TFTPForm = class(TForm)
    IdFTP1: TIdFTP;
    procedure IdFTP1Work(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure IdFTP1WorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure IdFTP1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
  end;

procedure TFTPForm.IdFTP1WorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  ProgressBar1.Max := AWorkCountMax;
  ProgressBar1.Position := 0;
  StatusBar1.SimpleText := 'Téléchargement en cours...';
end;

procedure TFTPForm.IdFTP1Work(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  ProgressBar1.Position := AWorkCount;
  Application.ProcessMessages; // Rafraîchir l'interface
end;

procedure TFTPForm.IdFTP1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);  
begin
  ProgressBar1.Position := ProgressBar1.Max;
  StatusBar1.SimpleText := 'Téléchargement terminé';
end;
```

## Envoi d'emails avec SMTP

```pascal
program SendEmailIndy;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  IdSMTP,           // Composant SMTP
  IdMessage,        // Message email
  IdText,           // Partie texte du message
  IdAttachmentFile, // Pièces jointes
  IdSSLOpenSSL;     // Support SSL/TLS

procedure EnvoyerEmail;  
var
  SMTP: TIdSMTP;
  Message: TIdMessage;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  SMTP := TIdSMTP.Create(nil);
  Message := TIdMessage.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Configuration SSL
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLHandler.SSLOptions.Mode := sslmClient;

    // Configuration du serveur SMTP
    SMTP.IOHandler := SSLHandler;
    SMTP.Host := 'smtp.gmail.com';
    SMTP.Port := 587;
    SMTP.Username := 'votre-email@gmail.com';
    SMTP.Password := 'votre-mot-de-passe';
    SMTP.UseTLS := utUseExplicitTLS;

    // Configuration du message
    Message.From.Address := 'votre-email@gmail.com';
    Message.From.Name := 'Votre Nom';
    Message.Recipients.EMailAddresses := 'destinataire@example.com';
    Message.Subject := 'Test email depuis Lazarus avec Indy';
    Message.ContentType := 'text/plain; charset=utf-8';

    // Corps du message
    with TIdText.Create(Message.MessageParts) do
    begin
      Body.Text := 'Bonjour,' + #13#10 + #13#10 +
                   'Ceci est un email de test envoyé avec Indy.' + #13#10 + #13#10 +
                   'Cordialement';
      ContentType := 'text/plain';
      CharSet := 'utf-8';
    end;

    // Ajout d'une pièce jointe (optionnel)
    // TIdAttachmentFile.Create(Message.MessageParts, 'C:\fichier.pdf');

    // Envoi
    WriteLn('Connexion au serveur SMTP...');
    SMTP.Connect;

    WriteLn('Authentification...');
    SMTP.Authenticate;

    WriteLn('Envoi du message...');
    SMTP.Send(Message);

    WriteLn('Email envoyé avec succès !');

    SMTP.Disconnect;

  finally
    SSLHandler.Free;
    Message.Free;
    SMTP.Free;
  end;
end;

begin
  try
    EnvoyerEmail;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  ReadLn;
end.
```

## Serveur TCP avec Indy

Indy facilite grandement la création de serveurs :

```pascal
program SimpleTCPServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  IdTCPServer,
  IdContext,
  IdGlobal;

type
  TMonServeur = class
  private
    FServer: TIdTCPServer;
    procedure OnExecute(AContext: TIdContext);
    procedure OnConnect(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Demarrer(Port: Integer);
    procedure Arreter;
  end;

constructor TMonServeur.Create;  
begin
  FServer := TIdTCPServer.Create(nil);
  FServer.OnExecute := @OnExecute;
  FServer.OnConnect := @OnConnect;
  FServer.OnDisconnect := @OnDisconnect;
end;

destructor TMonServeur.Destroy;  
begin
  FServer.Free;
  inherited;
end;

procedure TMonServeur.OnConnect(AContext: TIdContext);  
begin
  WriteLn('Client connecté : ', AContext.Binding.PeerIP);
end;

procedure TMonServeur.OnDisconnect(AContext: TIdContext);  
begin
  WriteLn('Client déconnecté : ', AContext.Binding.PeerIP);
end;

procedure TMonServeur.OnExecute(AContext: TIdContext);  
var
  ReceivedData, Response: string;
begin
  try
    // Lecture des données envoyées par le client
    ReceivedData := AContext.Connection.IOHandler.ReadLn;

    if ReceivedData <> '' then
    begin
      WriteLn('Reçu de ', AContext.Binding.PeerIP, ' : ', ReceivedData);

      // Traitement et réponse
      Response := 'Echo: ' + ReceivedData;
      AContext.Connection.IOHandler.WriteLn(Response);
    end;

  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end;

procedure TMonServeur.Demarrer(Port: Integer);  
begin
  FServer.DefaultPort := Port;
  FServer.Active := True;
  WriteLn('Serveur démarré sur le port ', Port);
end;

procedure TMonServeur.Arreter;  
begin
  FServer.Active := False;
  WriteLn('Serveur arrêté');
end;

var
  Serveur: TMonServeur;

begin
  Serveur := TMonServeur.Create;
  try
    Serveur.Demarrer(8080);
    WriteLn('Appuyez sur Entrée pour arrêter le serveur...');
    ReadLn;
    Serveur.Arreter;
  finally
    Serveur.Free;
  end;
end.
```

## Support SSL/TLS

### Configuration SSL pour HTTP

```pascal
uses
  IdHTTP,
  IdSSLOpenSSL;

procedure RequeteHTTPS;  
var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  HTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Configuration SSL
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLHandler.SSLOptions.Mode := sslmClient;

    // Association du handler SSL au HTTP
    HTTP.IOHandler := SSLHandler;

    // Requête HTTPS
    Memo1.Text := HTTP.Get('https://www.example.com');

  finally
    SSLHandler.Free;
    HTTP.Free;
  end;
end;
```

### Bibliothèques OpenSSL requises

**Sur Windows :**
- Téléchargez les DLL OpenSSL : `libeay32.dll` et `ssleay32.dll`
- Placez-les dans le répertoire de votre exécutable
- Ou téléchargez depuis : https://indy.fulgan.com/SSL/

**Sur Ubuntu/Linux :**
```bash
sudo apt-get install libssl-dev
# Les bibliothèques .so sont généralement déjà présentes
```

## Gestion des erreurs et exceptions

```pascal
procedure RequeteAvecGestionErreurs;  
var
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create(nil);
  try
    try
      HTTP.Get('http://www.example.com');

    except
      // Erreur HTTP (404, 500, etc.)
      on E: EIdHTTPProtocolException do
      begin
        ShowMessage('Erreur HTTP ' + IntToStr(E.ErrorCode) + ': ' + E.Message);
      end;

      // Erreur de connexion
      on E: EIdSocketError do
      begin
        ShowMessage('Erreur de connexion : ' + E.Message);
      end;

      // Erreur SSL
      on E: EIdOSSLUnderlyingCryptoError do
      begin
        ShowMessage('Erreur SSL : ' + E.Message);
      end;

      // Autres erreurs
      on E: Exception do
      begin
        ShowMessage('Erreur : ' + E.Message);
      end;
    end;

  finally
    HTTP.Free;
  end;
end;
```

## Timeouts et configuration

```pascal
procedure ConfigurerTimeouts;  
var
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create(nil);
  try
    // Configuration des timeouts (en millisecondes)
    HTTP.ConnectTimeout := 5000;  // 5 secondes pour la connexion
    HTTP.ReadTimeout := 30000;    // 30 secondes pour la lecture

    // Configuration de redirections
    HTTP.HandleRedirects := True;
    HTTP.RedirectMaximum := 5;

    // Configuration du proxy (si nécessaire)
    HTTP.ProxyParams.ProxyServer := 'proxy.example.com';
    HTTP.ProxyParams.ProxyPort := 8080;

    // Utilisation
    Memo1.Text := HTTP.Get('http://www.example.com');

  finally
    HTTP.Free;
  end;
end;
```

## Serveur HTTP simple

```pascal
program SimpleHTTPServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  IdHTTPServer,
  IdContext,
  IdCustomHTTPServer;

type
  TMonServeurHTTP = class
  private
    FServer: TIdHTTPServer;
    procedure OnCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Demarrer(Port: Integer);
  end;

constructor TMonServeurHTTP.Create;  
begin
  FServer := TIdHTTPServer.Create(nil);
  FServer.OnCommandGet := @OnCommandGet;
end;

destructor TMonServeurHTTP.Destroy;  
begin
  FServer.Free;
  inherited;
end;

procedure TMonServeurHTTP.OnCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  HTML: string;
begin
  WriteLn('Requête : ', ARequestInfo.URI, ' de ', ARequestInfo.RemoteIP);

  // Construction de la réponse HTML
  HTML := '<html><head><title>Serveur Indy</title></head>' +
          '<body><h1>Bonjour depuis Indy !</h1>' +
          '<p>Votre IP : ' + ARequestInfo.RemoteIP + '</p>' +
          '<p>URL demandée : ' + ARequestInfo.URI + '</p>' +
          '</body></html>';

  // Configuration de la réponse
  AResponseInfo.ContentType := 'text/html; charset=utf-8';
  AResponseInfo.ContentText := HTML;
  AResponseInfo.ResponseNo := 200; // Code HTTP 200 OK
end;

procedure TMonServeurHTTP.Demarrer(Port: Integer);  
begin
  FServer.DefaultPort := Port;
  FServer.Active := True;
  WriteLn('Serveur HTTP démarré sur http://localhost:', Port);
end;

var
  Serveur: TMonServeurHTTP;

begin
  Serveur := TMonServeurHTTP.Create;
  try
    Serveur.Demarrer(8080);
    WriteLn('Appuyez sur Entrée pour arrêter...');
    ReadLn;
  finally
    Serveur.Free;
  end;
end.
```

## WebSockets avec Indy

Indy supporte les WebSockets pour des communications bidirectionnelles en temps réel :

```pascal
uses
  IdWebSocketServer,
  IdWebSocketClient;

// Client WebSocket
procedure ClientWebSocket;  
var
  WS: TIdWebSocketClient;
begin
  WS := TIdWebSocketClient.Create(nil);
  try
    WS.Host := 'localhost';
    WS.Port := 8080;

    WS.Connect;
    WS.SendText('Bonjour serveur WebSocket !');

    WriteLn('Réponse : ', WS.ReceiveText);

  finally
    WS.Free;
  end;
end;
```

## Différences Windows/Linux

### Chemins de bibliothèques

**Windows :**
```pascal
// Les DLL doivent être dans le même répertoire que l'exécutable
// ou dans le PATH système
```

**Linux :**
```pascal
// Les .so sont généralement dans /usr/lib ou /usr/local/lib
// Vérifiez avec : ldconfig -p | grep ssl
```

### Permissions

**Linux nécessite des privilèges pour les ports < 1024 :**

```bash
# Méthode 1 : Exécuter avec sudo
sudo ./mon_serveur

# Méthode 2 : Donner les capabilities
sudo setcap 'cap_net_bind_service=+ep' ./mon_serveur
```

### Firewall

**Windows :**
```powershell
netsh advfirewall firewall add rule name="Mon Serveur" dir=in action=allow protocol=TCP localport=8080
```

**Ubuntu :**
```bash
sudo ufw allow 8080/tcp  
sudo ufw reload
```

## Bonnes pratiques

### 1. Toujours libérer les ressources

```pascal
HTTP := TIdHTTP.Create(nil);  
try
  // Votre code
finally
  HTTP.Free; // Toujours dans finally
end;
```

### 2. Utiliser les threads pour les opérations longues

```pascal
type
  TDownloadThread = class(TThread)
  private
    FForm: TForm1;
    FResultat: string;
    procedure MettreAJourInterface;
  protected
    procedure Execute; override;
  public
    constructor Create(AForm: TForm1);
  end;

constructor TDownloadThread.Create(AForm: TForm1);  
begin
  inherited Create(False); // Démarrage immédiat
  FreeOnTerminate := True;
  FForm := AForm;
end;

procedure TDownloadThread.Execute;  
var
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create(nil);
  try
    FResultat := HTTP.Get('http://example.com');

    // Mise à jour de l'interface dans le thread principal
    Synchronize(@MettreAJourInterface);
  finally
    HTTP.Free;
  end;
end;

procedure TDownloadThread.MettreAJourInterface;  
begin
  FForm.Memo1.Text := FResultat;
end;

procedure TForm1.ButtonClick(Sender: TObject);  
begin
  TDownloadThread.Create(Self);
end;
```

> **Note :** En mode ObjFPC, on utilise une classe de thread nommée plutôt que `TThread.CreateAnonymousThread(procedure ... end)` qui nécessite `{$modeswitch anonymousfunctions}` (FPC 3.3.1+).

### 3. Gérer les exceptions correctement

```pascal
try
  HTTP.Get(URL);
except
  on E: EIdHTTPProtocolException do
    ShowMessage('Erreur HTTP: ' + IntToStr(E.ErrorCode));
  on E: EIdSocketError do
    ShowMessage('Erreur réseau: ' + E.Message);
  on E: EIdConnClosedGracefully do
    ShowMessage('Connexion fermée normalement');
  on E: Exception do
    ShowMessage('Erreur générale: ' + E.Message);
end;
```

### 4. Configurer correctement les timeouts

```pascal
type
  TServeurAvecTimeout = class
  private
    FServer: TIdTCPServer;
    procedure OnExecute(AContext: TIdContext);
  public
    procedure Configurer;
  end;

procedure TServeurAvecTimeout.OnExecute(AContext: TIdContext);  
begin
  // Configuration du timeout par connexion
  AContext.Connection.IOHandler.ReadTimeout := 30000; // 30 secondes

  // ... traitement des données ...
end;

procedure TServeurAvecTimeout.Configurer;  
begin
  FServer := TIdTCPServer.Create(nil);

  // Timeouts pour éviter les blocages
  FServer.TerminateWaitTime := 5000; // 5 secondes

  // Assignation du gestionnaire d'événement
  FServer.OnExecute := @OnExecute;
end;
```

> **Note :** En mode ObjFPC, les événements `of object` doivent être assignés avec l'opérateur `@` vers une méthode nommée, pas une procédure anonyme.

### 5. Utiliser des pools de connexions

Pour optimiser les performances avec des connexions fréquentes :

```pascal
type
  TConnectionPool = class
  private
    FConnections: TThreadList;
    FMaxConnections: Integer;
  public
    constructor Create(AMaxConnections: Integer);
    destructor Destroy; override;
    function AcquireConnection: TIdHTTP;
    procedure ReleaseConnection(AConnection: TIdHTTP);
  end;

constructor TConnectionPool.Create(AMaxConnections: Integer);  
begin
  FConnections := TThreadList.Create;
  FMaxConnections := AMaxConnections;
end;

destructor TConnectionPool.Destroy;  
var
  List: TList;
  i: Integer;
begin
  List := FConnections.LockList;
  try
    for i := 0 to List.Count - 1 do
      TIdHTTP(List[i]).Free;
    List.Clear;
  finally
    FConnections.UnlockList;
  end;
  FConnections.Free;
  inherited;
end;

function TConnectionPool.AcquireConnection: TIdHTTP;  
var
  List: TList;
begin
  List := FConnections.LockList;
  try
    if List.Count > 0 then
    begin
      Result := TIdHTTP(List[List.Count - 1]);
      List.Delete(List.Count - 1);
    end
    else
      Result := TIdHTTP.Create(nil);
  finally
    FConnections.UnlockList;
  end;
end;

procedure TConnectionPool.ReleaseConnection(AConnection: TIdHTTP);  
var
  List: TList;
begin
  List := FConnections.LockList;
  try
    if List.Count < FMaxConnections then
      List.Add(AConnection)
    else
      AConnection.Free;
  finally
    FConnections.UnlockList;
  end;
end;
```

## Réception d'emails avec POP3

```pascal
program ReceiveEmailPOP3;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  IdPOP3,           // Composant POP3
  IdMessage,        // Message email
  IdSSLOpenSSL;     // Support SSL/TLS

procedure RecevoirEmails;  
var
  POP3: TIdPOP3;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  Message: TIdMessage;
  i, MessageCount: Integer;
begin
  POP3 := TIdPOP3.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Message := TIdMessage.Create(nil);
  try
    // Configuration SSL
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLHandler.SSLOptions.Mode := sslmClient;

    // Configuration du serveur POP3
    POP3.IOHandler := SSLHandler;
    POP3.Host := 'pop.gmail.com';
    POP3.Port := 995;
    POP3.Username := 'votre-email@gmail.com';
    POP3.Password := 'votre-mot-de-passe';
    POP3.UseTLS := utUseImplicitTLS;

    // Connexion
    WriteLn('Connexion au serveur POP3...');
    POP3.Connect;

    // Récupération du nombre de messages
    MessageCount := POP3.CheckMessages;
    WriteLn('Nombre de messages : ', MessageCount);

    // Lecture des messages
    for i := 1 to MessageCount do
    begin
      WriteLn('--- Message ', i, ' ---');

      // Récupération du message
      POP3.Retrieve(i, Message);

      WriteLn('De : ', Message.From.Text);
      WriteLn('Sujet : ', Message.Subject);
      WriteLn('Date : ', DateTimeToStr(Message.Date));
      WriteLn('Corps : ', Message.Body.Text);
      WriteLn;

      // Pour supprimer le message (optionnel)
      // POP3.Delete(i);
    end;

    // Déconnexion
    POP3.Disconnect;
    WriteLn('Déconnecté.');

  finally
    Message.Free;
    SSLHandler.Free;
    POP3.Free;
  end;
end;

begin
  try
    RecevoirEmails;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  ReadLn;
end.
```

### Gestion des pièces jointes

```pascal
procedure TraiterPiecesJointes(Message: TIdMessage);  
var
  i: Integer;
  Attachment: TIdAttachment;
  FileName: string;
begin
  WriteLn('Pièces jointes : ', Message.MessageParts.AttachmentCount);

  for i := 0 to Message.MessageParts.AttachmentCount - 1 do
  begin
    Attachment := Message.MessageParts.Attachments[i];
    FileName := ExtractFilePath(ParamStr(0)) + Attachment.FileName;

    WriteLn('Sauvegarde de : ', Attachment.FileName);
    Attachment.SaveToFile(FileName);
  end;
end;
```

## Client IMAP (plus avancé que POP3)

IMAP offre plus de fonctionnalités que POP3 (dossiers, flags, recherche, etc.) :

```pascal
program IMAPClient;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  IdIMAP4,
  IdMessage,
  IdSSLOpenSSL;

procedure ExempleIMAP;  
var
  IMAP: TIdIMAP4;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  Mailboxes: TStringList;
  Message: TIdMessage;
  i: Integer;
begin
  IMAP := TIdIMAP4.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Mailboxes := TStringList.Create;
  Message := TIdMessage.Create(nil);
  try
    // Configuration SSL
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLHandler.SSLOptions.Mode := sslmClient;

    // Configuration du serveur IMAP
    IMAP.IOHandler := SSLHandler;
    IMAP.Host := 'imap.gmail.com';
    IMAP.Port := 993;
    IMAP.Username := 'votre-email@gmail.com';
    IMAP.Password := 'votre-mot-de-passe';
    IMAP.UseTLS := utUseImplicitTLS;

    // Connexion
    WriteLn('Connexion au serveur IMAP...');
    IMAP.Connect;

    // Liste des boîtes mail
    WriteLn('Liste des dossiers :');
    IMAP.ListMailBoxes(Mailboxes);
    for i := 0 to Mailboxes.Count - 1 do
      WriteLn('  ', Mailboxes[i]);

    // Sélection de la boîte INBOX
    IMAP.SelectMailBox('INBOX');
    WriteLn('Messages dans INBOX : ', IMAP.MailBox.TotalMsgs);
    WriteLn('Messages non lus : ', IMAP.MailBox.UnseenMsgs);

    // Récupération du premier message
    if IMAP.MailBox.TotalMsgs > 0 then
    begin
      IMAP.Retrieve(1, Message);
      WriteLn('Premier message :');
      WriteLn('  Sujet : ', Message.Subject);
      WriteLn('  De : ', Message.From.Text);
    end;

    // Recherche de messages
    WriteLn('Recherche de messages non lus...');
    IMAP.SearchMailBox('UNSEEN', Mailboxes);
    WriteLn('Trouvés : ', Mailboxes.Count);

    // Déconnexion
    IMAP.Disconnect;

  finally
    Message.Free;
    Mailboxes.Free;
    SSLHandler.Free;
    IMAP.Free;
  end;
end;

begin
  try
    ExempleIMAP;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  ReadLn;
end.
```

## UDP avec Indy

Pour les communications sans connexion :

```pascal
program UDPExample;
{$mode objfpc}{$H+}

uses
  SysUtils,
  IdUDPClient,
  IdUDPServer,
  IdSocketHandle,
  IdGlobal;

// Émetteur UDP
procedure EnvoyerUDP;  
var
  UDP: TIdUDPClient;
begin
  UDP := TIdUDPClient.Create(nil);
  try
    UDP.Host := 'localhost';
    UDP.Port := 9000;

    UDP.Send('Message UDP de test');
    WriteLn('Message UDP envoyé');

  finally
    UDP.Free;
  end;
end;

// Récepteur UDP
type
  TMonServeurUDP = class
  private
    FServer: TIdUDPServer;
    procedure OnUDPRead(AThread: TIdUDPListenerThread;
      const AData: TIdBytes; ABinding: TIdSocketHandle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Demarrer(Port: Integer);
    procedure Arreter;
  end;

constructor TMonServeurUDP.Create;  
begin
  FServer := TIdUDPServer.Create(nil);
  FServer.OnUDPRead := @OnUDPRead;
end;

destructor TMonServeurUDP.Destroy;  
begin
  FServer.Free;
  inherited;
end;

procedure TMonServeurUDP.OnUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  ReceivedData: string;
begin
  ReceivedData := BytesToString(AData);
  WriteLn('Reçu de ', ABinding.PeerIP, ':', ABinding.PeerPort, ' : ', ReceivedData);

  // Réponse (optionnel)
  ABinding.SendTo(ABinding.PeerIP, ABinding.PeerPort,
    ToBytes('Accusé de réception'));
end;

procedure TMonServeurUDP.Demarrer(Port: Integer);  
begin
  FServer.DefaultPort := Port;
  FServer.Active := True;
  WriteLn('Serveur UDP démarré sur le port ', Port);
end;

procedure TMonServeurUDP.Arreter;  
begin
  FServer.Active := False;
end;

var
  Serveur: TMonServeurUDP;

begin
  Serveur := TMonServeurUDP.Create;
  try
    Serveur.Demarrer(9000);
    WriteLn('Appuyez sur Entrée pour arrêter...');
    ReadLn;
    Serveur.Arreter;
  finally
    Serveur.Free;
  end;
end.
```

## Encodage et décodage avec Indy

Indy fournit des outils pour encoder/décoder différents formats :

### Base64

```pascal
uses
  IdCoderMIME;

procedure ExempleBase64;  
var
  Encoder: TIdEncoderMIME;
  Decoder: TIdDecoderMIME;
  Original, Encoded, Decoded: string;
begin
  Encoder := TIdEncoderMIME.Create(nil);
  Decoder := TIdDecoderMIME.Create(nil);
  try
    Original := 'Texte à encoder';

    // Encodage
    Encoded := Encoder.EncodeString(Original);
    WriteLn('Encodé : ', Encoded);

    // Décodage
    Decoded := Decoder.DecodeString(Encoded);
    WriteLn('Décodé : ', Decoded);

  finally
    Decoder.Free;
    Encoder.Free;
  end;
end;
```

### URL Encoding

```pascal
uses
  IdURI;

procedure ExempleURLEncoding;  
var
  Original, Encoded, Decoded: string;
begin
  Original := 'Texte avec espaces & caractères spéciaux!';

  // Encodage URL
  Encoded := TIdURI.ParamsEncode(Original);
  WriteLn('Encodé : ', Encoded);
  // Résultat : Texte+avec+espaces+%26+caract%C3%A8res+sp%C3%A9ciaux%21

  // Décodage URL
  Decoded := TIdURI.URLDecode(Encoded);
  WriteLn('Décodé : ', Decoded);
end;
```

### Hachage MD5 et SHA

```pascal
uses
  IdHashMessageDigest,
  IdHashSHA;

procedure ExempleHachage;  
var
  MD5: TIdHashMessageDigest5;
  SHA1: TIdHashSHA1;
  SHA256: TIdHashSHA256;
  Texte: string;
begin
  Texte := 'Texte à hacher';

  // MD5
  MD5 := TIdHashMessageDigest5.Create;
  try
    WriteLn('MD5 : ', MD5.HashStringAsHex(Texte));
  finally
    MD5.Free;
  end;

  // SHA1
  SHA1 := TIdHashSHA1.Create;
  try
    WriteLn('SHA1 : ', SHA1.HashStringAsHex(Texte));
  finally
    SHA1.Free;
  end;

  // SHA256
  SHA256 := TIdHashSHA256.Create;
  try
    WriteLn('SHA256 : ', SHA256.HashStringAsHex(Texte));
  finally
    SHA256.Free;
  end;
end;
```

## API REST avec Indy

Créer une API REST simple avec Indy :

```pascal
program SimpleRESTAPI;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fpjson, jsonparser,
  IdHTTPServer,
  IdContext,
  IdCustomHTTPServer;

type
  TRESTServer = class
  private
    FServer: TIdHTTPServer;
    procedure OnCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure OnCommandPost(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    function GetUsers: TJSONObject;
    function CreateUser(const AName, AEmail: string): TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(Port: Integer);
  end;

constructor TRESTServer.Create;  
begin
  FServer := TIdHTTPServer.Create(nil);
  FServer.OnCommandGet := @OnCommandGet;
  FServer.OnCommandPost := @OnCommandPost;
end;

destructor TRESTServer.Destroy;  
begin
  FServer.Free;
  inherited;
end;

function TRESTServer.GetUsers: TJSONObject;  
var
  Response, UsersArray: TJSONObject;
  User: TJSONObject;
begin
  Response := TJSONObject.Create;
  UsersArray := TJSONObject.Create;

  try
    // Simulation de données utilisateurs
    User := TJSONObject.Create;
    User.Add('id', 1);
    User.Add('name', 'Jean Dupont');
    User.Add('email', 'jean@example.com');

    UsersArray.Add('users', User);
    Response.Add('data', UsersArray);
    Response.Add('success', True);

    Result := Response;
  except
    Response.Free;
    raise;
  end;
end;

function TRESTServer.CreateUser(const AName, AEmail: string): TJSONObject;  
var
  Response, User: TJSONObject;
begin
  Response := TJSONObject.Create;
  User := TJSONObject.Create;

  try
    User.Add('id', Random(1000));
    User.Add('name', AName);
    User.Add('email', AEmail);

    Response.Add('data', User);
    Response.Add('success', True);
    Response.Add('message', 'User created successfully');

    Result := Response;
  except
    Response.Free;
    raise;
  end;
end;

procedure TRESTServer.OnCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  JSONResponse: TJSONObject;
begin
  WriteLn('GET ', ARequestInfo.URI);

  // Gestion des routes
  if ARequestInfo.URI = '/api/users' then
  begin
    JSONResponse := GetUsers;
    try
      AResponseInfo.ContentType := 'application/json';
      AResponseInfo.ContentText := JSONResponse.AsJSON;
      AResponseInfo.ResponseNo := 200;
    finally
      JSONResponse.Free;
    end;
  end
  else
  begin
    // Route non trouvée
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := '{"error": "Route not found"}';
    AResponseInfo.ContentType := 'application/json';
  end;
end;

procedure TRESTServer.OnCommandPost(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  Name, Email: string;
  JSONResponse: TJSONObject;
begin
  WriteLn('POST ', ARequestInfo.URI);

  if ARequestInfo.URI = '/api/users' then
  begin
    try
      // Parse du JSON reçu
      JSONData := GetJSON(ARequestInfo.PostStream);

      if JSONData is TJSONObject then
      begin
        JSONObject := TJSONObject(JSONData);
        Name := JSONObject.Get('name', '');
        Email := JSONObject.Get('email', '');

        JSONResponse := CreateUser(Name, Email);
        try
          AResponseInfo.ContentType := 'application/json';
          AResponseInfo.ContentText := JSONResponse.AsJSON;
          AResponseInfo.ResponseNo := 201; // Created
        finally
          JSONResponse.Free;
        end;
      end;

    except
      on E: Exception do
      begin
        AResponseInfo.ResponseNo := 400;
        AResponseInfo.ContentText :=
          '{"error": "Invalid JSON: ' + E.Message + '"}';
        AResponseInfo.ContentType := 'application/json';
      end;
    end;
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := '{"error": "Route not found"}';
    AResponseInfo.ContentType := 'application/json';
  end;
end;

procedure TRESTServer.Start(Port: Integer);  
begin
  FServer.DefaultPort := Port;
  FServer.Active := True;
  WriteLn('API REST démarrée sur http://localhost:', Port);
  WriteLn('Endpoints disponibles :');
  WriteLn('  GET  /api/users');
  WriteLn('  POST /api/users');
end;

var
  Server: TRESTServer;

begin
  Randomize;
  Server := TRESTServer.Create;
  try
    Server.Start(8080);
    WriteLn('Appuyez sur Entrée pour arrêter...');
    ReadLn;
  finally
    Server.Free;
  end;
end.
```

### Client pour tester l'API

```pascal
procedure TesterAPIREST;  
var
  HTTP: TIdHTTP;
  JSONData: TStringStream;
  Response: string;
begin
  HTTP := TIdHTTP.Create(nil);
  JSONData := TStringStream.Create('');
  try
    // Test GET
    WriteLn('Test GET /api/users');
    Response := HTTP.Get('http://localhost:8080/api/users');
    WriteLn(Response);
    WriteLn;

    // Test POST
    WriteLn('Test POST /api/users');
    JSONData.WriteString('{"name":"Marie Curie","email":"marie@example.com"}');
    HTTP.Request.ContentType := 'application/json';

    Response := HTTP.Post('http://localhost:8080/api/users', JSONData);
    WriteLn(Response);

  finally
    JSONData.Free;
    HTTP.Free;
  end;
end;
```

## Serveur multi-thread avancé

Pour gérer de nombreuses connexions simultanées :

```pascal
type
  TMonServeurMultiThread = class
  private
    FServer: TIdTCPServer;
    FActiveConnections: Integer;
    procedure OnConnect(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
    procedure OnExecute(AContext: TIdContext);
    procedure OnException(AContext: TIdContext; AException: Exception);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(Port: Integer; MaxConnections: Integer = 100);
    property ActiveConnections: Integer read FActiveConnections;
  end;

constructor TMonServeurMultiThread.Create;  
begin
  FServer := TIdTCPServer.Create(nil);
  FServer.OnConnect := @OnConnect;
  FServer.OnDisconnect := @OnDisconnect;
  FServer.OnExecute := @OnExecute;
  FServer.OnException := @OnException;
  FActiveConnections := 0;
end;

destructor TMonServeurMultiThread.Destroy;  
begin
  if FServer.Active then
    FServer.Active := False;
  FServer.Free;
  inherited;
end;

procedure TMonServeurMultiThread.OnConnect(AContext: TIdContext);  
begin
  InterlockedIncrement(FActiveConnections);
  WriteLn('Client connecté : ', AContext.Binding.PeerIP,
          ' (Total: ', FActiveConnections, ')');
end;

procedure TMonServeurMultiThread.OnDisconnect(AContext: TIdContext);  
begin
  InterlockedDecrement(FActiveConnections);
  WriteLn('Client déconnecté : ', AContext.Binding.PeerIP,
          ' (Total: ', FActiveConnections, ')');
end;

procedure TMonServeurMultiThread.OnExecute(AContext: TIdContext);  
var
  Command: string;
begin
  try
    // Lecture non-bloquante avec timeout
    if AContext.Connection.IOHandler.InputBufferIsEmpty then
    begin
      AContext.Connection.IOHandler.CheckForDataOnSource(100);
      if AContext.Connection.IOHandler.InputBufferIsEmpty then
        Exit;
    end;

    Command := AContext.Connection.IOHandler.ReadLn;

    if Command <> '' then
    begin
      WriteLn('Commande de ', AContext.Binding.PeerIP, ' : ', Command);

      // Traitement de la commande
      if Command = 'PING' then
        AContext.Connection.IOHandler.WriteLn('PONG')
      else if Command = 'TIME' then
        AContext.Connection.IOHandler.WriteLn(DateTimeToStr(Now))
      else if Command = 'QUIT' then
        AContext.Connection.Disconnect
      else
        AContext.Connection.IOHandler.WriteLn('ERROR: Unknown command');
    end;

  except
    on E: Exception do
      WriteLn('Erreur dans OnExecute : ', E.Message);
  end;
end;

procedure TMonServeurMultiThread.OnException(AContext: TIdContext;
  AException: Exception);
begin
  WriteLn('Exception pour ', AContext.Binding.PeerIP, ' : ', AException.Message);
end;

procedure TMonServeurMultiThread.Start(Port: Integer; MaxConnections: Integer);  
begin
  FServer.DefaultPort := Port;
  FServer.MaxConnections := MaxConnections;
  FServer.Active := True;
  WriteLn('Serveur démarré sur le port ', Port);
  WriteLn('Connexions max : ', MaxConnections);
end;
```

## Monitoring et statistiques

Ajout de fonctionnalités de monitoring :

```pascal
type
  TServerStats = record
    StartTime: TDateTime;
    TotalConnections: Int64;
    TotalBytesSent: Int64;
    TotalBytesReceived: Int64;
    ActiveConnections: Integer;
  end;

type
  TMonitoredServer = class
  private
    FServer: TIdTCPServer;
    FStats: TServerStats;
    procedure OnExecute(AContext: TIdContext);
  public
    constructor Create;
    procedure Start(Port: Integer);
    function GetStats: string;
  end;

constructor TMonitoredServer.Create;  
begin
  FServer := TIdTCPServer.Create(nil);
  FServer.OnExecute := @OnExecute;

  // Initialisation des statistiques
  FStats.StartTime := Now;
  FStats.TotalConnections := 0;
  FStats.TotalBytesSent := 0;
  FStats.TotalBytesReceived := 0;
  FStats.ActiveConnections := 0;
end;

procedure TMonitoredServer.OnExecute(AContext: TIdContext);  
var
  Data: string;
  BytesReceived, BytesSent: Int64;
begin
  Data := AContext.Connection.IOHandler.ReadLn;
  BytesReceived := Length(Data);

  InterlockedExchangeAdd64(FStats.TotalBytesReceived, BytesReceived);

  // Réponse
  AContext.Connection.IOHandler.WriteLn('Echo: ' + Data);
  BytesSent := Length(Data) + 6; // "Echo: " + données

  InterlockedExchangeAdd64(FStats.TotalBytesSent, BytesSent);
end;

function TMonitoredServer.GetStats: string;  
var
  Uptime: TDateTime;
begin
  Uptime := Now - FStats.StartTime;

  Result := Format(
    'Statistiques du serveur:'#13#10 +
    '  Uptime: %s'#13#10 +
    '  Connexions totales: %d'#13#10 +
    '  Connexions actives: %d'#13#10 +
    '  Octets envoyés: %d'#13#10 +
    '  Octets reçus: %d',
    [
      FormatDateTime('hh:nn:ss', Uptime),
      FStats.TotalConnections,
      FStats.ActiveConnections,
      FStats.TotalBytesSent,
      FStats.TotalBytesReceived
    ]);
end;

procedure TMonitoredServer.Start(Port: Integer);  
begin
  FServer.DefaultPort := Port;
  FServer.Active := True;
end;
```

## Conclusion

Indy est une bibliothèque réseau puissante et polyvalente qui s'intègre parfaitement avec Lazarus. Ses principaux avantages sont :

### Points forts d'Indy

- **Composants visuels** : Idéal pour les applications avec interface graphique
- **Riche en fonctionnalités** : Support de nombreux protocoles standard
- **Gestion des threads** : Serveurs multi-thread automatiques
- **SSL/TLS intégré** : Sécurisation facile des communications
- **Événements** : Programmation intuitive basée sur les événements
- **Documentation** : Grande communauté et ressources abondantes

### Quand utiliser Indy ?

- Applications graphiques avec composants visuels
- Besoin de support pour de nombreux protocoles (SMTP, POP3, IMAP, FTP, etc.)
- Serveurs nécessitant la gestion de nombreuses connexions simultanées
- Projets nécessitant SSL/TLS sans complexité
- Migration depuis Delphi

### Comparaison rapide : Indy vs Synapse

**Utilisez Indy si** :
- Vous développez des applications graphiques
- Vous voulez des composants visuels
- Vous avez besoin de serveurs multi-thread
- Vous migrez depuis Delphi

**Utilisez Synapse si** :
- Vous préférez le code procédural
- Vous voulez une bibliothèque légère
- Vous développez des applications console
- Vous avez besoin de simplicité maximale

Les deux bibliothèques sont excellentes et le choix dépend de vos besoins et préférences. Elles sont toutes deux multi-plateformes et fonctionnent aussi bien sur Windows que sur Ubuntu/Linux.

## Ressources complémentaires

- **Site officiel** : http://www.indyproject.org/
- **Documentation** : http://www.indyproject.org/docsite/html/
- **GitHub** : https://github.com/IndySockets/Indy
- **Forum Lazarus** : https://forum.lazarus.freepascal.org/
- **Wiki FreePascal** : https://wiki.freepascal.org/Indy
- **Exemples** : Inclus dans l'installation d'Indy (dossier demos)

## Exemples pratiques avancés

### Serveur de chat multi-utilisateurs

Voici un exemple complet de serveur de chat utilisant Indy :

```pascal
program ChatServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes, syncobjs,
  IdTCPServer,
  IdContext,
  IdGlobal;

type
  TChatServer = class
  private
    FServer: TIdTCPServer;
    FClients: TThreadList;
    FLock: TCriticalSection;
    procedure OnConnect(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
    procedure OnExecute(AContext: TIdContext);
    procedure BroadcastMessage(const AMessage: string; ASender: TIdContext);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(Port: Integer);
    procedure Stop;
  end;

constructor TChatServer.Create;  
begin
  FServer := TIdTCPServer.Create(nil);
  FServer.OnConnect := @OnConnect;
  FServer.OnDisconnect := @OnDisconnect;
  FServer.OnExecute := @OnExecute;

  FClients := TThreadList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TChatServer.Destroy;  
begin
  Stop;
  FLock.Free;
  FClients.Free;
  FServer.Free;
  inherited;
end;

procedure TChatServer.OnConnect(AContext: TIdContext);  
var
  List: TList;
begin
  FLock.Enter;
  try
    List := FClients.LockList;
    try
      List.Add(AContext);
      WriteLn('Nouveau client connecté : ', AContext.Binding.PeerIP);
      WriteLn('Clients connectés : ', List.Count);
    finally
      FClients.UnlockList;
    end;

    // Message de bienvenue
    AContext.Connection.IOHandler.WriteLn('Bienvenue sur le serveur de chat !');
    AContext.Connection.IOHandler.WriteLn('Entrez votre pseudo :');
  finally
    FLock.Leave;
  end;
end;

procedure TChatServer.OnDisconnect(AContext: TIdContext);  
var
  List: TList;
  Username: string;
begin
  FLock.Enter;
  try
    Username := AContext.Data.ToString;

    List := FClients.LockList;
    try
      List.Remove(AContext);
      WriteLn('Client déconnecté : ', Username, ' (', AContext.Binding.PeerIP, ')');
      WriteLn('Clients connectés : ', List.Count);
    finally
      FClients.UnlockList;
    end;

    if Username <> '' then
      BroadcastMessage('[Système] ' + Username + ' a quitté le chat', nil);
  finally
    FLock.Leave;
  end;
end;

procedure TChatServer.OnExecute(AContext: TIdContext);  
var
  Message, Username: string;
begin
  try
    if AContext.Connection.IOHandler.InputBufferIsEmpty then
    begin
      AContext.Connection.IOHandler.CheckForDataOnSource(100);
      if AContext.Connection.IOHandler.InputBufferIsEmpty then
        Exit;
    end;

    Message := AContext.Connection.IOHandler.ReadLn;

    if Message <> '' then
    begin
      // Premier message = pseudo
      if AContext.Data = nil then
      begin
        AContext.Data := TObject(Message);
        Username := Message;
        WriteLn('Nouveau pseudo : ', Username);
        BroadcastMessage('[Système] ' + Username + ' a rejoint le chat', AContext);
        AContext.Connection.IOHandler.WriteLn('Vous êtes maintenant connecté en tant que : ' + Username);
      end
      else
      begin
        // Messages suivants
        Username := AContext.Data.ToString;
        WriteLn('[', Username, '] ', Message);
        BroadcastMessage('[' + Username + '] ' + Message, AContext);
      end;
    end;

  except
    on E: Exception do
      WriteLn('Erreur OnExecute : ', E.Message);
  end;
end;

procedure TChatServer.BroadcastMessage(const AMessage: string; ASender: TIdContext);  
var
  List: TList;
  i: Integer;
  Context: TIdContext;
begin
  FLock.Enter;
  try
    List := FClients.LockList;
    try
      for i := 0 to List.Count - 1 do
      begin
        Context := TIdContext(List[i]);

        // Ne pas renvoyer le message à l'expéditeur
        if Context <> ASender then
        begin
          try
            Context.Connection.IOHandler.WriteLn(AMessage);
          except
            on E: Exception do
              WriteLn('Erreur lors de l''envoi à ', Context.Binding.PeerIP, ' : ', E.Message);
          end;
        end;
      end;
    finally
      FClients.UnlockList;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TChatServer.Start(Port: Integer);  
begin
  FServer.DefaultPort := Port;
  FServer.Active := True;
  WriteLn('=== Serveur de Chat ===');
  WriteLn('Démarré sur le port : ', Port);
  WriteLn('En attente de connexions...');
  WriteLn;
end;

procedure TChatServer.Stop;  
begin
  if FServer.Active then
  begin
    FServer.Active := False;
    WriteLn('Serveur arrêté');
  end;
end;

var
  Server: TChatServer;

begin
  Server := TChatServer.Create;
  try
    Server.Start(8080);
    WriteLn('Appuyez sur Entrée pour arrêter le serveur...');
    ReadLn;
    Server.Stop;
  finally
    Server.Free;
  end;
end.
```

### Client de chat correspondant

```pascal
program ChatClient;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  IdTCPClient;

type
  TReaderThread = class(TThread)
  private
    FClient: TIdTCPClient;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TIdTCPClient);
  end;

constructor TReaderThread.Create(AClient: TIdTCPClient);  
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClient := AClient;
end;

procedure TReaderThread.Execute;  
var
  Message: string;
begin
  while not Terminated and FClient.Connected do
  begin
    try
      if not FClient.IOHandler.InputBufferIsEmpty then
      begin
        Message := FClient.IOHandler.ReadLn;
        WriteLn(Message);
      end;
      Sleep(50);
    except
      on E: Exception do
      begin
        WriteLn('Erreur de réception : ', E.Message);
        Break;
      end;
    end;
  end;
end;

procedure ChatClient;  
var
  Client: TIdTCPClient;
  ReaderThread: TReaderThread;
  Message: string;
begin
  Client := TIdTCPClient.Create(nil);
  try
    // Connexion au serveur
    Client.Host := 'localhost';
    Client.Port := 8080;

    WriteLn('Connexion au serveur de chat...');
    Client.Connect;
    WriteLn('Connecté !');
    WriteLn;

    // Démarrage du thread de lecture
    ReaderThread := TReaderThread.Create(Client);

    // Boucle d'envoi de messages
    while Client.Connected do
    begin
      ReadLn(Message);

      if Message = '/quit' then
        Break;

      if Message <> '' then
      begin
        try
          Client.IOHandler.WriteLn(Message);
        except
          on E: Exception do
          begin
            WriteLn('Erreur d''envoi : ', E.Message);
            Break;
          end;
        end;
      end;
    end;

    Client.Disconnect;
    WriteLn('Déconnecté.');

  finally
    Client.Free;
  end;
end;

begin
  try
    ChatClient;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Proxy HTTP avec Indy

Exemple d'un proxy HTTP simple :

```pascal
program SimpleHTTPProxy;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  IdHTTPServer,
  IdHTTP,
  IdContext,
  IdCustomHTTPServer;

type
  TProxyServer = class
  private
    FServer: TIdHTTPServer;
    FHTTPClient: TIdHTTP;
    procedure OnCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(Port: Integer);
  end;

constructor TProxyServer.Create;  
begin
  FServer := TIdHTTPServer.Create(nil);
  FServer.OnCommandGet := @OnCommandGet;
  FHTTPClient := TIdHTTP.Create(nil);
end;

destructor TProxyServer.Destroy;  
begin
  FHTTPClient.Free;
  FServer.Free;
  inherited;
end;

procedure TProxyServer.OnCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  URL, Content: string;
  ResponseStream: TMemoryStream;
begin
  // Récupération de l'URL demandée
  URL := ARequestInfo.Params.Values['url'];

  if URL = '' then
  begin
    AResponseInfo.ResponseNo := 400;
    AResponseInfo.ContentText :=
      '<html><body><h1>Proxy HTTP</h1>' +
      '<p>Usage: http://localhost:8080/?url=http://example.com</p>' +
      '</body></html>';
    Exit;
  end;

  WriteLn('Proxy: ', URL);

  try
    ResponseStream := TMemoryStream.Create;
    try
      // Récupération de la page via le proxy
      FHTTPClient.Get(URL, ResponseStream);

      // Renvoi du contenu
      ResponseStream.Position := 0;
      AResponseInfo.ContentStream := ResponseStream;
      AResponseInfo.ContentType := FHTTPClient.Response.ContentType;
      AResponseInfo.ResponseNo := FHTTPClient.ResponseCode;

      // Important: ne pas libérer ResponseStream ici
      // Il sera libéré automatiquement par Indy
      ResponseStream := nil;

    finally
      if Assigned(ResponseStream) then
        ResponseStream.Free;
    end;

  except
    on E: Exception do
    begin
      AResponseInfo.ResponseNo := 500;
      AResponseInfo.ContentText :=
        '<html><body><h1>Erreur Proxy</h1>' +
        '<p>' + E.Message + '</p></body></html>';
    end;
  end;
end;

procedure TProxyServer.Start(Port: Integer);  
begin
  FServer.DefaultPort := Port;
  FServer.Active := True;
  WriteLn('Proxy HTTP démarré sur le port ', Port);
  WriteLn('Usage: http://localhost:', Port, '/?url=http://example.com');
end;

var
  Proxy: TProxyServer;

begin
  Proxy := TProxyServer.Create;
  try
    Proxy.Start(8080);
    WriteLn('Appuyez sur Entrée pour arrêter...');
    ReadLn;
  finally
    Proxy.Free;
  end;
end.
```

## Téléchargement avec barre de progression

Application graphique avec barre de progression :

```pascal
unit DownloadForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  IdHTTP, IdSSLOpenSSL;

type
  TFormDownload = class(TForm)
    ButtonDownload: TButton;
    EditURL: TEdit;
    ProgressBar: TProgressBar;
    LabelStatus: TLabel;
    SaveDialog: TSaveDialog;
    procedure ButtonDownloadClick(Sender: TObject);
  private
    IdHTTP: TIdHTTP;
    SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
    procedure OnWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure OnWork(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure OnWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormDownload: TFormDownload;

implementation

{$R *.lfm}

constructor TFormDownload.Create(TheOwner: TComponent);  
begin
  inherited Create(TheOwner);

  // Création du client HTTP
  IdHTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

  // Configuration SSL
  SSLHandler.SSLOptions.Method := sslvTLSv1_2;
  SSLHandler.SSLOptions.Mode := sslmClient;
  IdHTTP.IOHandler := SSLHandler;

  // Configuration des événements de progression
  IdHTTP.OnWorkBegin := @OnWorkBegin;
  IdHTTP.OnWork := @OnWork;
  IdHTTP.OnWorkEnd := @OnWorkEnd;
end;

destructor TFormDownload.Destroy;  
begin
  SSLHandler.Free;
  IdHTTP.Free;
  inherited Destroy;
end;

procedure TFormDownload.OnWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  if AWorkMode = wmRead then
  begin
    ProgressBar.Max := AWorkCountMax;
    ProgressBar.Position := 0;
    LabelStatus.Caption := 'Téléchargement en cours...';
    Application.ProcessMessages;
  end;
end;

procedure TFormDownload.OnWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
var
  Percent: Integer;
begin
  if AWorkMode = wmRead then
  begin
    ProgressBar.Position := AWorkCount;

    if ProgressBar.Max > 0 then
    begin
      Percent := Round((AWorkCount / ProgressBar.Max) * 100);
      LabelStatus.Caption := Format('Téléchargement: %d%% (%s / %s)',
        [Percent,
         FormatFloat('#,##0', AWorkCount),
         FormatFloat('#,##0', ProgressBar.Max)]);
    end;

    Application.ProcessMessages;
  end;
end;

procedure TFormDownload.OnWorkEnd(ASender: TObject; AWorkMode: TWorkMode);  
begin
  if AWorkMode = wmRead then
  begin
    ProgressBar.Position := ProgressBar.Max;
    LabelStatus.Caption := 'Téléchargement terminé !';
    Application.ProcessMessages;
  end;
end;

procedure TFormDownload.ButtonDownloadClick(Sender: TObject);  
var
  FileStream: TFileStream;
  URL: string;
begin
  URL := EditURL.Text;

  if URL = '' then
  begin
    ShowMessage('Veuillez entrer une URL');
    Exit;
  end;

  if SaveDialog.Execute then
  begin
    ButtonDownload.Enabled := False;
    try
      FileStream := TFileStream.Create(SaveDialog.FileName, fmCreate);
      try
        LabelStatus.Caption := 'Connexion...';
        Application.ProcessMessages;

        IdHTTP.Get(URL, FileStream);

        ShowMessage('Fichier téléchargé avec succès !');

      finally
        FileStream.Free;
      end;

    except
      on E: Exception do
      begin
        ShowMessage('Erreur: ' + E.Message);
        LabelStatus.Caption := 'Erreur: ' + E.Message;
      end;
    end;

    ButtonDownload.Enabled := True;
  end;
end;

end.
```

## Client DNS

Résolution de noms de domaine avec Indy :

```pascal
program DNSClient;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  IdDNSResolver;

procedure ResoudreDNS(const Hostname: string);  
var
  DNS: TIdDNSResolver;
  i: Integer;
begin
  DNS := TIdDNSResolver.Create(nil);
  try
    // Configuration du serveur DNS (optionnel)
    // DNS.Host := '8.8.8.8'; // Google DNS

    WriteLn('Résolution DNS de : ', Hostname);
    WriteLn;

    // Résolution
    DNS.QueryRecords(Hostname, [qtA]); // Type A (IPv4)

    if DNS.QueryResult.Count > 0 then
    begin
      WriteLn('Adresses IPv4 :');
      for i := 0 to DNS.QueryResult.Count - 1 do
      begin
        if DNS.QueryResult[i].RecType = qtA then
          WriteLn('  ', DNS.QueryResult[i].IPAddress);
      end;
    end;

    // Résolution IPv6
    DNS.QueryRecords(Hostname, [qtAAAA]); // Type AAAA (IPv6)

    if DNS.QueryResult.Count > 0 then
    begin
      WriteLn('Adresses IPv6 :');
      for i := 0 to DNS.QueryResult.Count - 1 do
      begin
        if DNS.QueryResult[i].RecType = qtAAAA then
          WriteLn('  ', DNS.QueryResult[i].IPAddress);
      end;
    end;

    // Enregistrements MX (serveurs mail)
    DNS.QueryRecords(Hostname, [qtMX]);

    if DNS.QueryResult.Count > 0 then
    begin
      WriteLn('Serveurs mail (MX) :');
      for i := 0 to DNS.QueryResult.Count - 1 do
      begin
        if DNS.QueryResult[i].RecType = qtMX then
          WriteLn('  ', DNS.QueryResult[i].ExchangeServer,
                  ' (priorité: ', DNS.QueryResult[i].Preference, ')');
      end;
    end;

  finally
    DNS.Free;
  end;
end;

begin
  if ParamCount > 0 then
    ResoudreDNS(ParamStr(1))
  else
  begin
    WriteLn('Usage: DNSClient <hostname>');
    WriteLn('Exemple: DNSClient google.com');
  end;

  ReadLn;
end.
```

## Serveur de fichiers HTTP

Serveur pour partager des fichiers via HTTP :

```pascal
program FileServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  IdHTTPServer,
  IdContext,
  IdCustomHTTPServer;

type
  TFileServer = class
  private
    FServer: TIdHTTPServer;
    FRootDir: string;
    procedure OnCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    function GetContentType(const FileName: string): string;
    function GenerateDirectoryListing(const Path: string): string;
  public
    constructor Create(const ARootDir: string);
    destructor Destroy; override;
    procedure Start(Port: Integer);
  end;

constructor TFileServer.Create(const ARootDir: string);  
begin
  FRootDir := IncludeTrailingPathDelimiter(ARootDir);
  FServer := TIdHTTPServer.Create(nil);
  FServer.OnCommandGet := @OnCommandGet;
end;

destructor TFileServer.Destroy;  
begin
  FServer.Free;
  inherited;
end;

function TFileServer.GetContentType(const FileName: string): string;  
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));

  // case sur des chaînes n'est pas supporté en FreePascal
  if (Ext = '.html') or (Ext = '.htm') then
    Result := 'text/html'
  else if Ext = '.css' then
    Result := 'text/css'
  else if Ext = '.js' then
    Result := 'application/javascript'
  else if Ext = '.json' then
    Result := 'application/json'
  else if Ext = '.xml' then
    Result := 'application/xml'
  else if Ext = '.txt' then
    Result := 'text/plain'
  else if Ext = '.pdf' then
    Result := 'application/pdf'
  else if (Ext = '.jpg') or (Ext = '.jpeg') then
    Result := 'image/jpeg'
  else if Ext = '.png' then
    Result := 'image/png'
  else if Ext = '.gif' then
    Result := 'image/gif'
  else if Ext = '.svg' then
    Result := 'image/svg+xml'
  else if Ext = '.zip' then
    Result := 'application/zip'
  else if Ext = '.mp3' then
    Result := 'audio/mpeg'
  else if Ext = '.mp4' then
    Result := 'video/mp4'
  else
    Result := 'application/octet-stream';
end;

function TFileServer.GenerateDirectoryListing(const Path: string): string;  
var
  SearchRec: TSearchRec;
  HTML: TStringList;
  RelativePath: string;
begin
  HTML := TStringList.Create;
  try
    RelativePath := StringReplace(Path, FRootDir, '', [rfIgnoreCase]);

    HTML.Add('<html><head><title>Index de /' + RelativePath + '</title></head>');
    HTML.Add('<body><h1>Index de /' + RelativePath + '</h1><hr>');
    HTML.Add('<ul>');

    // Lien vers le répertoire parent
    if RelativePath <> '' then
      HTML.Add('<li><a href="../">[Répertoire parent]</a></li>');

    // Liste des répertoires
    if FindFirst(Path + '*', faDirectory, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
           ((SearchRec.Attr and faDirectory) = faDirectory) then
        begin
          HTML.Add('<li><a href="' + SearchRec.Name + '/">[' +
                   SearchRec.Name + ']</a></li>');
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;

    // Liste des fichiers
    if FindFirst(Path + '*', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        if ((SearchRec.Attr and faDirectory) = 0) then
        begin
          HTML.Add('<li><a href="' + SearchRec.Name + '">' +
                   SearchRec.Name + '</a> (' +
                   FormatFloat('#,##0', SearchRec.Size) + ' octets)</li>');
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;

    HTML.Add('</ul><hr></body></html>');
    Result := HTML.Text;

  finally
    HTML.Free;
  end;
end;

procedure TFileServer.OnCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  FullPath: string;
  FileStream: TFileStream;
begin
  // Construction du chemin complet
  FullPath := FRootDir + StringReplace(ARequestInfo.Document, '/',
              PathDelim, [rfReplaceAll]);

  // Sécurité: empêcher l'accès en dehors du répertoire racine
  if Copy(FullPath, 1, Length(FRootDir)) <> FRootDir then
  begin
    AResponseInfo.ResponseNo := 403;
    AResponseInfo.ContentText := '<html><body><h1>403 Forbidden</h1></body></html>';
    Exit;
  end;

  WriteLn('Requête: ', ARequestInfo.Document);

  // Vérification de l'existence
  if DirectoryExists(FullPath) then
  begin
    // Affichage du listing de répertoire
    AResponseInfo.ContentType := 'text/html; charset=utf-8';
    AResponseInfo.ContentText := GenerateDirectoryListing(
      IncludeTrailingPathDelimiter(FullPath));
    AResponseInfo.ResponseNo := 200;
  end
  else if FileExists(FullPath) then
  begin
    try
      // Envoi du fichier
      FileStream := TFileStream.Create(FullPath, fmOpenRead or fmShareDenyWrite);
      try
        AResponseInfo.ContentType := GetContentType(FullPath);
        AResponseInfo.ContentStream := FileStream;
        AResponseInfo.ResponseNo := 200;
        FileStream := nil; // Ne pas libérer, Indy s'en charge
      finally
        if Assigned(FileStream) then
          FileStream.Free;
      end;

    except
      on E: Exception do
      begin
        AResponseInfo.ResponseNo := 500;
        AResponseInfo.ContentText := '<html><body><h1>500 Internal Server Error</h1>' +
                                      '<p>' + E.Message + '</p></body></html>';
      end;
    end;
  end
  else
  begin
    // Fichier non trouvé
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := '<html><body><h1>404 Not Found</h1>' +
                                  '<p>Le fichier demandé n''existe pas.</p></body></html>';
  end;
end;

procedure TFileServer.Start(Port: Integer);  
begin
  FServer.DefaultPort := Port;
  FServer.Active := True;
  WriteLn('Serveur de fichiers démarré');
  WriteLn('Répertoire racine : ', FRootDir);
  WriteLn('URL : http://localhost:', Port);
end;

var
  Server: TFileServer;
  RootDir: string;

begin
  if ParamCount > 0 then
    RootDir := ParamStr(1)
  else
    RootDir := GetCurrentDir;

  WriteLn('=== Serveur de fichiers HTTP ===');

  Server := TFileServer.Create(RootDir);
  try
    Server.Start(8080);
    WriteLn('Appuyez sur Entrée pour arrêter...');
    ReadLn;
  finally
    Server.Free;
  end;
end.
```

## Conclusion finale

Indy est une bibliothèque réseau professionnelle et complète qui offre tout ce dont vous avez besoin pour développer des applications réseau sophistiquées avec FreePascal et Lazarus. Que vous développiez un simple client HTTP, un serveur multi-utilisateurs, ou une application de messagerie complexe, Indy fournit les outils nécessaires.

### Points clés à retenir

1. **Installation facile** via le gestionnaire de paquets de Lazarus
2. **Composants visuels** pour une intégration parfaite dans l'IDE
3. **Multi-thread natif** pour les serveurs hautes performances
4. **SSL/TLS intégré** pour des communications sécurisées
5. **Protocoles nombreux** : HTTP, SMTP, POP3, IMAP, FTP, DNS, etc.
6. **Cross-platform** : fonctionne sur Windows, Linux, macOS
7. **Documentation riche** et grande communauté

### Prochaines étapes

Maintenant que vous maîtrisez les bases d'Indy, vous pouvez :
- Explorer les protocoles plus avancés (NNTP, Telnet, IRC)
- Implémenter vos propres protocoles personnalisés
- Créer des applications client-serveur complexes
- Intégrer SSL/TLS dans toutes vos communications
- Développer des API REST professionnelles

Indy et Synapse (vue au chapitre précédent) sont complémentaires : utilisez Indy pour les applications graphiques complexes, et Synapse pour les solutions légères et les services en arrière-plan. Les deux sont d'excellents choix pour la programmation réseau multi-plateforme avec FreePascal.

⏭️ [Protocoles TCP/UDP bas niveau](/10-programmation-reseau-avancee/03-protocoles-tcp-udp-bas-niveau.md)
