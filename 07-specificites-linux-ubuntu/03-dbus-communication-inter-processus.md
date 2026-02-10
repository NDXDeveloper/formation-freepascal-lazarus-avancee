🔝 Retour au [Sommaire](/SOMMAIRE.md)

# D-Bus et Communication Inter-processus avec FreePascal/Lazarus

## Introduction : Qu'est-ce que D-Bus ?

D-Bus (Desktop Bus) est un système de communication inter-processus (IPC) largement utilisé sur les systèmes Linux/Ubuntu. Imaginez D-Bus comme un "bureau de poste" central où les applications peuvent s'envoyer des messages entre elles. C'est le moyen standard pour les applications Linux de communiquer, que ce soit pour :

- Notifier d'autres applications d'événements système
- Partager des données entre programmes
- Contrôler des services système
- Interagir avec l'environnement de bureau (GNOME, KDE, etc.)

### Pourquoi utiliser D-Bus ?

Avant D-Bus, chaque application devait créer sa propre méthode de communication, ce qui créait de la complexité et de l'incompatibilité. D-Bus standardise cette communication avec un protocole unique et simple.

## Architecture de D-Bus

D-Bus fonctionne avec trois composants principaux :

### 1. Le Bus Système (System Bus)
- Un seul bus système par machine
- Utilisé pour la communication avec les services système
- Nécessite des permissions pour certaines opérations
- Exemples : NetworkManager, systemd, hardware notifications

### 2. Le Bus de Session (Session Bus)
- Un bus par session utilisateur
- Pour la communication entre applications de l'utilisateur
- Plus permissif que le bus système
- Exemples : notifications desktop, lecteurs média, gestionnaires de fichiers

### 3. Les Applications
- **Fournisseurs de services** : exposent des méthodes que d'autres peuvent appeler
- **Clients** : utilisent les services exposés
- Une application peut être les deux à la fois

## Concepts Fondamentaux

### Adresses D-Bus

Chaque application sur D-Bus possède une adresse unique, comme une adresse email :

```
org.freedesktop.Notifications    # Service de notifications
org.mozilla.Firefox              # Firefox
com.example.MonApplication       # Votre application
```

### Chemins d'Objets

Les services exposent des objets via des chemins, similaires aux chemins de fichiers :

```
/org/freedesktop/Notifications
/com/example/MonApplication/MainWindow
```

### Interfaces

Les interfaces définissent les méthodes et signaux disponibles :

```
org.freedesktop.Notifications.Notify     # Méthode pour afficher une notification
org.example.MonApp.DoSomething          # Votre propre méthode
```

### Messages D-Bus

Il existe quatre types de messages :

1. **Method Calls** : Appels de méthodes (requête)
2. **Method Returns** : Réponses aux appels
3. **Signals** : Notifications broadcast (diffusion)
4. **Errors** : Messages d'erreur

## Installation et Configuration pour FreePascal

### Prérequis Ubuntu

D-Bus est généralement préinstallé sur Ubuntu. Vérifiez avec :

```bash
# Vérifier si D-Bus est actif
systemctl status dbus

# Installer les outils de développement
sudo apt-get install libdbus-1-dev
sudo apt-get install d-feet  # Outil graphique pour explorer D-Bus
```

### Installation du package pour Lazarus

1. Ouvrez Lazarus
2. Menu **Package** → **Online Package Manager**
3. Recherchez "dbus"
4. Installez le package **dbus** ou **python4lazarus** (qui inclut D-Bus)

Alternative manuelle :
```bash
# Cloner le repository
git clone https://github.com/fpcdbus/fpcdbus.git
```

Puis dans Lazarus :
- **Package** → **Open Package File (.lpk)**
- Ouvrir le fichier du package D-Bus
- Compiler et installer

## Premier Programme D-Bus

### Création d'un Client Simple

Voici un exemple simple qui envoie une notification desktop :

```pascal
program NotificationDemo;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, dbus;

const
  NOTIFICATIONS_SERVICE = 'org.freedesktop.Notifications';
  NOTIFICATIONS_PATH = '/org/freedesktop/Notifications';
  NOTIFICATIONS_INTERFACE = 'org.freedesktop.Notifications';

var
  Connection: PDBusConnection;
  Error: DBusError;
  Message, Reply: PDBusMessage;
  Args: DBusMessageIter;
  NotificationID: dbus_uint32_t;

begin
  // Initialiser l'erreur
  dbus_error_init(@Error);

  // Se connecter au bus de session
  Connection := dbus_bus_get(DBUS_BUS_SESSION, @Error);

  if Connection = nil then
  begin
    WriteLn('Erreur de connexion à D-Bus: ', Error.message);
    dbus_error_free(@Error);
    Exit;
  end;

  // Créer un message pour appeler la méthode Notify
  Message := dbus_message_new_method_call(
    NOTIFICATIONS_SERVICE,
    NOTIFICATIONS_PATH,
    NOTIFICATIONS_INTERFACE,
    'Notify'
  );

  if Message = nil then
  begin
    WriteLn('Erreur: impossible de créer le message');
    Exit;
  end;

  // Préparer les arguments
  dbus_message_iter_init_append(Message, @Args);

  // Nom de l'application
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @PChar('MonApp'));

  // ID de notification (0 pour nouvelle)
  NotificationID := 0;
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_UINT32, @NotificationID);

  // Icône
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @PChar('dialog-information'));

  // Titre
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @PChar('Bonjour D-Bus !'));

  // Corps du message
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @PChar('Ceci est ma première notification D-Bus'));

  // Envoyer le message et attendre la réponse
  Reply := dbus_connection_send_with_reply_and_block(Connection, Message, -1, @Error);

  if Reply = nil then
  begin
    WriteLn('Erreur lors de l''envoi: ', Error.message);
    dbus_error_free(@Error);
  end
  else
  begin
    WriteLn('Notification envoyée avec succès !');
    dbus_message_unref(Reply);
  end;

  // Nettoyer
  dbus_message_unref(Message);
  dbus_connection_unref(Connection);
end.
```

## Utilisation Simplifiée avec des Classes Helper

Pour simplifier l'utilisation, créons une classe helper :

```pascal
unit SimpleDBus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbus;

type
  TSimpleDBusClient = class
  private
    FConnection: PDBusConnection;
    FError: DBusError;
    FConnected: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect: Boolean;
    procedure Disconnect;

    function SendNotification(const ATitle, ABody: string): Boolean;
    function CallMethod(const AService, APath, AInterface, AMethod: string;
                       const AParams: array of Variant): Boolean;

    property Connected: Boolean read FConnected;
  end;

implementation

constructor TSimpleDBusClient.Create;
begin
  inherited Create;
  FConnected := False;
  dbus_error_init(@FError);
end;

destructor TSimpleDBusClient.Destroy;
begin
  if FConnected then
    Disconnect;
  inherited Destroy;
end;

function TSimpleDBusClient.Connect: Boolean;
begin
  Result := False;

  // Se connecter au bus de session
  FConnection := dbus_bus_get(DBUS_BUS_SESSION, @FError);

  if FConnection = nil then
  begin
    WriteLn('Erreur D-Bus: ', FError.message);
    dbus_error_free(@FError);
    Exit;
  end;

  FConnected := True;
  Result := True;
end;

procedure TSimpleDBusClient.Disconnect;
begin
  if FConnected and (FConnection <> nil) then
  begin
    dbus_connection_unref(FConnection);
    FConnection := nil;
    FConnected := False;
  end;
end;

function TSimpleDBusClient.SendNotification(const ATitle, ABody: string): Boolean;
var
  Message, Reply: PDBusMessage;
  Args: DBusMessageIter;
  ArrayIter, DictIter: DBusMessageIter;
  NotificationID: dbus_uint32_t;
  AppName, Icon: PChar;
  Timeout: dbus_int32_t;
begin
  Result := False;

  if not FConnected then
    Exit;

  // Créer le message
  Message := dbus_message_new_method_call(
    'org.freedesktop.Notifications',
    '/org/freedesktop/Notifications',
    'org.freedesktop.Notifications',
    'Notify'
  );

  if Message = nil then
    Exit;

  // Initialiser les arguments
  dbus_message_iter_init_append(Message, @Args);

  // Paramètres de la notification
  AppName := 'FreePascal App';
  NotificationID := 0;
  Icon := 'dialog-information';
  Timeout := 5000; // 5 secondes

  // Ajouter les arguments dans l'ordre
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @AppName);
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_UINT32, @NotificationID);
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @Icon);
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @PChar(ATitle));
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @PChar(ABody));

  // Actions (tableau vide)
  dbus_message_iter_open_container(@Args, DBUS_TYPE_ARRAY, 's', @ArrayIter);
  dbus_message_iter_close_container(@Args, @ArrayIter);

  // Hints (dictionnaire vide)
  dbus_message_iter_open_container(@Args, DBUS_TYPE_ARRAY, '{sv}', @DictIter);
  dbus_message_iter_close_container(@Args, @DictIter);

  // Timeout
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_INT32, @Timeout);

  // Envoyer et attendre la réponse
  Reply := dbus_connection_send_with_reply_and_block(FConnection, Message, -1, @FError);

  if Reply <> nil then
  begin
    Result := True;
    dbus_message_unref(Reply);
  end;

  dbus_message_unref(Message);
end;

end.
```

### Utilisation de la classe Helper

```pascal
program SimpleNotification;

uses
  SimpleDBus;

var
  Client: TSimpleDBusClient;
begin
  Client := TSimpleDBusClient.Create;
  try
    if Client.Connect then
    begin
      Client.SendNotification('Test D-Bus', 'Notification depuis FreePascal !');
      WriteLn('Notification envoyée');
    end
    else
      WriteLn('Impossible de se connecter à D-Bus');
  finally
    Client.Free;
  end;
end.
```

## Création d'un Service D-Bus

Créons maintenant un service que d'autres applications peuvent appeler :

```pascal
unit MyDBusService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbus;

type
  TMyDBusService = class
  private
    FConnection: PDBusConnection;
    FError: DBusError;
    FRunning: Boolean;

    function HandleMessage(AMessage: PDBusMessage): DBusHandlerResult;
  public
    constructor Create;
    destructor Destroy; override;

    function Start: Boolean;
    procedure Stop;
    procedure ProcessMessages;

    property Running: Boolean read FRunning;
  end;

const
  SERVICE_NAME = 'com.example.MyPascalService';
  OBJECT_PATH = '/com/example/MyPascalService';
  INTERFACE_NAME = 'com.example.MyPascalService.Interface';

implementation

constructor TMyDBusService.Create;
begin
  inherited Create;
  dbus_error_init(@FError);
  FRunning := False;
end;

destructor TMyDBusService.Destroy;
begin
  if FRunning then
    Stop;
  inherited Destroy;
end;

function TMyDBusService.Start: Boolean;
var
  Ret: Integer;
begin
  Result := False;

  // Se connecter au bus
  FConnection := dbus_bus_get(DBUS_BUS_SESSION, @FError);
  if FConnection = nil then
  begin
    WriteLn('Erreur connexion: ', FError.message);
    dbus_error_free(@FError);
    Exit;
  end;

  // Demander un nom sur le bus
  Ret := dbus_bus_request_name(FConnection, SERVICE_NAME,
                               DBUS_NAME_FLAG_REPLACE_EXISTING, @FError);

  if Ret <> DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then
  begin
    WriteLn('Impossible d''obtenir le nom du service');
    Exit;
  end;

  WriteLn('Service démarré: ', SERVICE_NAME);
  FRunning := True;
  Result := True;
end;

procedure TMyDBusService.Stop;
begin
  if FRunning and (FConnection <> nil) then
  begin
    dbus_connection_unref(FConnection);
    FConnection := nil;
    FRunning := False;
  end;
end;

procedure TMyDBusService.ProcessMessages;
var
  Message: PDBusMessage;
  Reply: PDBusMessage;
  Args: DBusMessageIter;
  Param: PChar;
  Response: string;
begin
  if not FRunning then
    Exit;

  // Lire les messages en attente
  dbus_connection_read_write(FConnection, 0);
  Message := dbus_connection_pop_message(FConnection);

  while Message <> nil do
  begin
    // Vérifier si c'est un appel de méthode
    if dbus_message_is_method_call(Message, INTERFACE_NAME, 'Echo') then
    begin
      // Lire le paramètre
      if dbus_message_iter_init(Message, @Args) then
      begin
        if dbus_message_iter_get_arg_type(@Args) = DBUS_TYPE_STRING then
        begin
          dbus_message_iter_get_basic(@Args, @Param);
          Response := 'Echo: ' + string(Param);

          // Créer la réponse
          Reply := dbus_message_new_method_return(Message);
          dbus_message_iter_init_append(Reply, @Args);
          dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @PChar(Response));

          // Envoyer la réponse
          dbus_connection_send(FConnection, Reply, nil);
          dbus_connection_flush(FConnection);

          dbus_message_unref(Reply);

          WriteLn('Message traité: ', string(Param));
        end;
      end;
    end;

    // Message suivant
    dbus_message_unref(Message);
    Message := dbus_connection_pop_message(FConnection);
  end;
end;

end.
```

## Communication Entre Applications FreePascal

Voici un exemple complet de deux applications communiquant via D-Bus :

### Application Serveur

```pascal
program DBusServer;

uses
  SysUtils, Crt, MyDBusService;

var
  Service: TMyDBusService;
begin
  WriteLn('Démarrage du serveur D-Bus...');

  Service := TMyDBusService.Create;
  try
    if Service.Start then
    begin
      WriteLn('Serveur en écoute. Appuyez sur une touche pour arrêter...');

      // Boucle principale
      while not KeyPressed do
      begin
        Service.ProcessMessages;
        Sleep(100); // Attendre 100ms
      end;
    end
    else
      WriteLn('Impossible de démarrer le service');
  finally
    Service.Free;
  end;
end.
```

### Application Cliente

```pascal
program DBusClient;

uses
  SysUtils, dbus;

function CallEchoMethod(const AMessage: string): string;
var
  Connection: PDBusConnection;
  Error: DBusError;
  Message, Reply: PDBusMessage;
  Args: DBusMessageIter;
  Response: PChar;
begin
  Result := '';
  dbus_error_init(@Error);

  // Connexion
  Connection := dbus_bus_get(DBUS_BUS_SESSION, @Error);
  if Connection = nil then
    Exit;

  // Créer l'appel
  Message := dbus_message_new_method_call(
    'com.example.MyPascalService',
    '/com/example/MyPascalService',
    'com.example.MyPascalService.Interface',
    'Echo'
  );

  // Ajouter le paramètre
  dbus_message_iter_init_append(Message, @Args);
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @PChar(AMessage));

  // Envoyer et attendre
  Reply := dbus_connection_send_with_reply_and_block(Connection, Message, 1000, @Error);

  if Reply <> nil then
  begin
    // Lire la réponse
    if dbus_message_iter_init(Reply, @Args) then
    begin
      dbus_message_iter_get_basic(@Args, @Response);
      Result := string(Response);
    end;
    dbus_message_unref(Reply);
  end;

  dbus_message_unref(Message);
  dbus_connection_unref(Connection);
end;

begin
  WriteLn('Envoi du message au serveur...');
  WriteLn('Réponse: ', CallEchoMethod('Bonjour serveur !'));
end.
```

## Signaux D-Bus

Les signaux permettent la diffusion d'événements à plusieurs applications :

### Émission d'un Signal

```pascal
procedure EmitSignal(Connection: PDBusConnection; const ASignalName, AData: string);
var
  Signal: PDBusMessage;
  Args: DBusMessageIter;
begin
  // Créer le signal
  Signal := dbus_message_new_signal(
    '/com/example/MyApp',
    'com.example.MyApp.Signals',
    ASignalName
  );

  if Signal = nil then
    Exit;

  // Ajouter les données
  dbus_message_iter_init_append(Signal, @Args);
  dbus_message_iter_append_basic(@Args, DBUS_TYPE_STRING, @PChar(AData));

  // Envoyer
  dbus_connection_send(Connection, Signal, nil);
  dbus_connection_flush(Connection);

  dbus_message_unref(Signal);
end;
```

### Réception de Signaux

```pascal
procedure ListenForSignals(Connection: PDBusConnection);
var
  Rule: string;
  Error: DBusError;
begin
  dbus_error_init(@Error);

  // S'abonner aux signaux
  Rule := 'type=''signal'',interface=''com.example.MyApp.Signals''';
  dbus_bus_add_match(Connection, PChar(Rule), @Error);
  dbus_connection_flush(Connection);

  if dbus_error_is_set(@Error) then
  begin
    WriteLn('Erreur: ', Error.message);
    dbus_error_free(@Error);
  end;
end;
```

## Outils de Débogage

### D-Feet - Explorateur D-Bus Graphique

D-Feet est un outil indispensable pour explorer et tester D-Bus :

```bash
# Installation
sudo apt-get install d-feet

# Lancement
d-feet
```

D-Feet permet de :
- Voir tous les services disponibles
- Explorer les interfaces et méthodes
- Tester les appels de méthodes
- Surveiller les signaux

### dbus-monitor - Surveillance en Ligne de Commande

```bash
# Surveiller tous les messages
dbus-monitor

# Filtrer par interface
dbus-monitor "interface='org.freedesktop.Notifications'"

# Surveiller le bus système
dbus-monitor --system
```

### busctl - Outil systemd

```bash
# Lister les services
busctl list

# Examiner un service
busctl introspect com.example.MyService /com/example/MyService

# Appeler une méthode
busctl call com.example.MyService /com/example/MyService \
  com.example.Interface Method s "paramètre"
```

## Bonnes Pratiques

### 1. Nommage

Utilisez la convention de nommage inversée des domaines :
- ✅ `com.monentreprise.MonApplication`
- ❌ `MyApp` (trop générique)

### 2. Gestion d'Erreurs

Toujours vérifier les erreurs D-Bus :

```pascal
if dbus_error_is_set(@Error) then
begin
  // Traiter l'erreur
  WriteLn('Erreur: ', Error.message);
  dbus_error_free(@Error);
  // Ne pas continuer !
  Exit;
end;
```

### 3. Libération de Mémoire

D-Bus utilise le comptage de références. Toujours libérer :

```pascal
// Messages
if Message <> nil then
  dbus_message_unref(Message);

// Connexions
if Connection <> nil then
  dbus_connection_unref(Connection);
```

### 4. Timeout

Définissez des timeouts appropriés pour éviter les blocages :

```pascal
// Timeout de 5 secondes (5000 ms)
Reply := dbus_connection_send_with_reply_and_block(Connection, Message, 5000, @Error);
```

### 5. Threading

D-Bus n'est pas thread-safe par défaut. Si vous utilisez des threads :

```pascal
// Dans le thread principal au démarrage
dbus_threads_init_default();
```

## Cas d'Usage Courants

### Intégration Desktop

```pascal
// Afficher une notification
SendNotification('Titre', 'Message');

// Ouvrir une URL dans le navigateur
CallMethod('org.freedesktop.portal.Desktop',
          '/org/freedesktop/portal/desktop',
          'org.freedesktop.portal.OpenURI',
          'OpenURI', ['', 'https://example.com', '']);

// Capture d'écran
CallMethod('org.gnome.Shell.Screenshot',
          '/org/gnome/Shell/Screenshot',
          'org.gnome.Shell.Screenshot',
          'Screenshot', [False, True, '/tmp/screenshot.png']);
```

### Communication avec systemd

```pascal
// Redémarrer un service
CallMethod('org.freedesktop.systemd1',
          '/org/freedesktop/systemd1',
          'org.freedesktop.systemd1.Manager',
          'RestartUnit', ['myservice.service', 'replace']);
```

### Contrôle du Lecteur Multimédia

```pascal
// Contrôler un lecteur compatible MPRIS
CallMethod('org.mpris.MediaPlayer2.vlc',
          '/org/mpris/MediaPlayer2',
          'org.mpris.MediaPlayer2.Player',
          'PlayPause', []);
```

## Résolution de Problèmes Courants

### Problème : "Connection refused"

**Solution** : Vérifiez que D-Bus est en cours d'exécution :
```bash
systemctl status dbus
ps aux | grep dbus-daemon
```

### Problème : "Name already in use"

**Solution** : Un autre processus utilise déjà ce nom. Utilisez un nom unique ou forcez le remplacement :
```pascal
dbus_bus_request_name(Connection, SERVICE_NAME,
                     DBUS_NAME_FLAG_REPLACE_EXISTING or
                     DBUS_NAME_FLAG_DO_NOT_QUEUE, @Error);
```

### Problème : Pas de réponse aux appels

**Solutions** :
1. Vérifiez le nom du service, le chemin et l'interface
2. Utilisez d-feet pour tester manuellement
3. Vérifiez les permissions (PolicyKit pour le bus système)

### Problème : Fuites mémoire

**Solution** : Utilisez valgrind pour détecter les fuites :
```bash
valgrind --leak-check=full ./monprogramme
```

## Conclusion

D-Bus est un outil puissant pour la communication inter-processus sur Linux. Avec FreePascal/Lazarus, vous pouvez :

- Créer des applications qui s'intègrent parfaitement au desktop Linux
- Faire communiquer vos applications entre elles
- Contrôler et surveiller les services système
- Recevoir des notifications d'événements système

Les concepts peuvent sembler complexes au début, mais avec la pratique, D-Bus devient un outil naturel pour créer des applications Linux modernes et bien intégrées.

## Ressources Supplémentaires

- [Spécification D-Bus](https://dbus.freedesktop.org/doc/dbus-specification.html)
- [Tutorial D-Bus officiel](https://dbus.freedesktop.org/doc/dbus-tutorial.html)
- [Wiki FreePascal D-Bus](https://wiki.freepascal.org/DBus)
- [Forum Lazarus](https://forum.lazarus.freepascal.org/)
- [Exemples de code](https://github.com/graemeg/fpGUI/tree/master/examples/dbus)

⏭️ [Configuration via fichiers texte](/07-specificites-linux-ubuntu/04-configuration-via-fichiers-texte.md)
