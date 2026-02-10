🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.5 D-Bus sous Linux

## Introduction

**D-Bus** (Desktop Bus) est un système de communication inter-processus (IPC) moderne et standardisé sous Linux et Unix. C'est l'équivalent Linux de COM sous Windows, mais avec une philosophie très différente : ouverture, simplicité et intégration desktop.

### Qu'est-ce que D-Bus ?

D-Bus est un **bus de messages** qui permet aux applications de communiquer entre elles de manière standardisée. Il remplace les anciennes méthodes IPC Unix (pipes, sockets, signaux) par une solution unifiée et orientée objet.

**Analogie** : Imaginez D-Bus comme un réseau de transport public où les applications sont des stations. Au lieu que chaque application crée sa propre route privée vers une autre, toutes utilisent le même réseau de bus public pour échanger des messages.

```
┌──────────────────────────────────────────────────────┐
│                    D-Bus Daemon                      │
│               (Bus de messages central)              │
└────┬──────┬──────┬──────┬──────┬──────┬──────────────┘
     │      │      │      │      │      │
  ┌──▼──┐ ┌─▼──┐ ┌─▼──┐ ┌─▼──┐ ┌─▼──┐ ┌─▼──┐
  │ App │ │ App│ │ App│ │ App│ │ App│ │ App│
  │  1  │ │ 2  │ │ 3  │ │ 4  │ │ 5  │ │ 6  │
  └─────┘ └────┘ └────┘ └────┘ └────┘ └────┘
```

### Les deux types de bus

#### System Bus (Bus Système)

Le **System Bus** est global à tout le système et persiste tant que la machine est allumée.

**Utilisé pour** :
- Services système (NetworkManager, BlueZ, systemd)
- Gestion du matériel (UPower, UDisks)
- Authentification (PolicyKit)
- Configuration système
- Événements matériels

**Caractéristiques** :
- Un seul bus par machine
- Nécessite souvent des privilèges root
- Démarre au boot
- Politique de sécurité stricte

**Exemple** : Quand vous insérez une clé USB, le kernel Linux détecte le matériel, UDisks envoie un message sur le System Bus, et votre gestionnaire de fichiers reçoit la notification pour afficher la clé.

#### Session Bus (Bus de Session)

Le **Session Bus** est privé à chaque session utilisateur.

**Utilisé pour** :
- Applications desktop (navigateurs, éditeurs)
- Notifications desktop
- Lecteurs multimédias
- Gestionnaires de fenêtres
- Applets et widgets

**Caractéristiques** :
- Un bus par session utilisateur connectée
- Pas de privilèges spéciaux requis
- Démarre avec la session
- Politique de sécurité plus souple

**Exemple** : Quand vous recevez une notification "Nouveau mail reçu", votre client email envoie un message sur le Session Bus, et le système de notification desktop l'affiche.

### Architecture D-Bus

#### Concepts fondamentaux

**1. Bus Name (Nom de bus)**

Identifie de manière unique un service sur le bus. Deux types :

- **Well-known name** : Nom lisible (ex: `org.freedesktop.NetworkManager`)
- **Unique name** : Attribué automatiquement (ex: `:1.42`)

```
org.freedesktop.Notifications
│         │            │
│         │            └─ Service (Notifications)
│         └─ Organisation (freedesktop)
└─ Domaine inversé (org)
```

**2. Object Path (Chemin d'objet)**

Identifie un objet spécifique exposé par un service. Similaire à un chemin de fichier.

```
/org/freedesktop/Notifications
│    │            │
│    │            └─ Objet Notifications
│    └─ Organisation
└─ Racine
```

**3. Interface**

Définit un ensemble de méthodes et signaux. Un objet peut implémenter plusieurs interfaces.

```
org.freedesktop.Notifications
    ├─ Notify (méthode)
    ├─ CloseNotification (méthode)
    ├─ GetCapabilities (méthode)
    └─ NotificationClosed (signal)
```

**4. Methods (Méthodes)**

Actions que vous pouvez demander à un service d'effectuer. Appels synchrones qui retournent une réponse.

**5. Signals (Signaux)**

Messages asynchrones émis par un service pour notifier des événements. Les clients s'y abonnent.

**6. Properties (Propriétés)**

Valeurs exposées par un service, accessibles en lecture/écriture.

### Pourquoi utiliser D-Bus depuis FreePascal ?

#### 1. Intégration desktop native

```pascal
// Afficher une notification native
DBusNotify('Mon Application', 'Traitement terminé !');
```

#### 2. Contrôle système

```pascal
// Éteindre l'ordinateur via systemd
DBusSystemdPowerOff;
```

#### 3. Communication inter-applications

```pascal
// Contrôler un lecteur multimédia
DBusMediaPlayerPlay('spotify');
```

#### 4. Surveillance d'événements

```pascal
// Être notifié des changements réseau
OnNetworkChanged := @MonHandlerReseau;
```

## Installation et prérequis

### Vérifier D-Bus

D-Bus est installé par défaut sur quasiment toutes les distributions Linux modernes.

```bash
# Vérifier si D-Bus est installé
which dbus-daemon

# Voir la version
dbus-daemon --version

# Vérifier que le bus système fonctionne
systemctl status dbus

# Vérifier que le bus de session fonctionne
echo $DBUS_SESSION_BUS_ADDRESS
```

### Installer les outils de développement

```bash
# Ubuntu/Debian
sudo apt install libdbus-1-dev dbus-x11

# Fedora/RHEL
sudo dnf install dbus-devel

# Arch Linux
sudo pacman -S dbus
```

### Outils de diagnostic

```bash
# Explorer les services disponibles
d-feet  # Interface graphique (à installer)

# Lister les services sur le bus système
dbus-send --system --print-reply \
  --dest=org.freedesktop.DBus \
  /org/freedesktop/DBus \
  org.freedesktop.DBus.ListNames

# Lister les services sur le bus de session
dbus-send --session --print-reply \
  --dest=org.freedesktop.DBus \
  /org/freedesktop/DBus \
  org.freedesktop.DBus.ListNames

# Surveiller tous les messages sur le bus
dbus-monitor --session  
dbus-monitor --system  # Nécessite root
```

### Packages FreePascal pour D-Bus

FreePascal n'a pas de binding D-Bus officiel dans la bibliothèque standard, mais plusieurs options existent :

1. **dbus-fpc** : Binding communautaire
2. **Utiliser directement libdbus** via bindings C
3. **Utiliser dbus-send** via Process

Pour ce tutoriel, nous allons créer nos propres bindings simples et montrer comment utiliser les outils en ligne de commande.

## Utilisation basique via dbus-send

Avant de créer des bindings complexes, voyons comment interagir avec D-Bus via la ligne de commande.

### Afficher une notification

```pascal
program NotificationSimple;

{$mode objfpc}{$H+}

uses
  Process, SysUtils;

function ExecuterCommande(const Cmd: string): string;  
var
  Proc: TProcess;
  Output: TStringList;
begin
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Proc.Executable := '/bin/bash';
    Proc.Parameters.Add('-c');
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

procedure AfficherNotification(const Titre, Message: string);  
var
  Cmd: string;
begin
  Cmd := Format(
    'dbus-send --session --print-reply ' +
    '--dest=org.freedesktop.Notifications ' +
    '/org/freedesktop/Notifications ' +
    'org.freedesktop.Notifications.Notify ' +
    'string:"Mon Application" ' +      // app_name
    'uint32:0 ' +                      // replaces_id
    'string:"" ' +                     // app_icon
    'string:"%s" ' +                   // summary (titre)
    'string:"%s" ' +                   // body (message)
    'array:string:"" ' +               // actions
    'dict:string:string:"" ' +         // hints
    'int32:5000',                      // expire_timeout (5 secondes)
    [Titre, Message]
  );

  ExecuterCommande(Cmd);
end;

begin
  WriteLn('Envoi d''une notification...');
  AfficherNotification('Hello D-Bus', 'Ceci est une notification depuis FreePascal !');
  WriteLn('Notification envoyée !');

  Sleep(6000); // Attendre pour voir la notification
end.
```

### Contrôler un lecteur multimédia (MPRIS)

```pascal
program ControleMediaPlayer;

{$mode objfpc}{$H+}

uses
  Process, SysUtils;

function ExecuterDBus(const Cmd: string): string;  
var
  Proc: TProcess;
  Output: TStringList;
begin
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Proc.Executable := '/bin/bash';
    Proc.Parameters.Add('-c');
    Proc.Parameters.Add(Cmd);
    Proc.Options := [poWaitOnExit, poUsePipes];
    Proc.Execute;

    Output.LoadFromStream(Proc.Output);
    Result := Trim(Output.Text);
  finally
    Output.Free;
    Proc.Free;
  end;
end;

function ListerLecteursMedia: TStringList;  
var
  Output: string;
begin
  Result := TStringList.Create;

  Output := ExecuterDBus(
    'dbus-send --session --print-reply ' +
    '--dest=org.freedesktop.DBus ' +
    '/org/freedesktop/DBus ' +
    'org.freedesktop.DBus.ListNames | ' +
    'grep "org.mpris.MediaPlayer2"'
  );

  Result.Text := Output;
end;

procedure MediaPlayerAction(const Player, Action: string);  
var
  Cmd: string;
begin
  Cmd := Format(
    'dbus-send --session --print-reply ' +
    '--dest=%s ' +
    '/org/mpris/MediaPlayer2 ' +
    'org.mpris.MediaPlayer2.Player.%s',
    [Player, Action]
  );

  ExecuterDBus(Cmd);
end;

function ObtenirStatutLecture(const Player: string): string;  
var
  Output: string;
begin
  Output := ExecuterDBus(Format(
    'dbus-send --session --print-reply ' +
    '--dest=%s ' +
    '/org/mpris/MediaPlayer2 ' +
    'org.freedesktop.DBus.Properties.Get ' +
    'string:"org.mpris.MediaPlayer2.Player" ' +
    'string:"PlaybackStatus"',
    [Player]
  ));

  // Parser la sortie pour extraire le statut
  if Pos('Playing', Output) > 0 then
    Result := 'Playing'
  else if Pos('Paused', Output) > 0 then
    Result := 'Paused'
  else if Pos('Stopped', Output) > 0 then
    Result := 'Stopped'
  else
    Result := 'Unknown';
end;

var
  Lecteurs: TStringList;
  Lecteur: string;
  Choix: Integer;
  i: Integer;

begin
  WriteLn('=== Contrôle de lecteur multimédia via D-Bus ===');
  WriteLn;

  // Lister les lecteurs disponibles
  Lecteurs := ListerLecteursMedia;
  try
    if Lecteurs.Count = 0 then
    begin
      WriteLn('Aucun lecteur multimédia détecté.');
      WriteLn('Lancez Spotify, VLC ou un autre lecteur compatible MPRIS.');
      Exit;
    end;

    WriteLn('Lecteurs détectés :');
    for i := 0 to Lecteurs.Count - 1 do
      WriteLn(Format('  %d. %s', [i + 1, Lecteurs[i]]));
    WriteLn;

    // Utiliser le premier lecteur trouvé
    Lecteur := Trim(Lecteurs[0]);
    WriteLn('Utilisation de : ', Lecteur);
    WriteLn;

    // Afficher le statut
    WriteLn('Statut actuel : ', ObtenirStatutLecture(Lecteur));
    WriteLn;

    // Menu de contrôle
    repeat
      WriteLn('Actions disponibles :');
      WriteLn('  1. Play');
      WriteLn('  2. Pause');
      WriteLn('  3. Stop');
      WriteLn('  4. Next (piste suivante)');
      WriteLn('  5. Previous (piste précédente)');
      WriteLn('  6. Afficher le statut');
      WriteLn('  0. Quitter');
      Write('Choix : ');
      ReadLn(Choix);

      case Choix of
        1: begin
             MediaPlayerAction(Lecteur, 'Play');
             WriteLn('▶ Play');
           end;
        2: begin
             MediaPlayerAction(Lecteur, 'Pause');
             WriteLn('⏸ Pause');
           end;
        3: begin
             MediaPlayerAction(Lecteur, 'Stop');
             WriteLn('⏹ Stop');
           end;
        4: begin
             MediaPlayerAction(Lecteur, 'Next');
             WriteLn('⏭ Piste suivante');
           end;
        5: begin
             MediaPlayerAction(Lecteur, 'Previous');
             WriteLn('⏮ Piste précédente');
           end;
        6: WriteLn('Statut : ', ObtenirStatutLecture(Lecteur));
      end;

      WriteLn;
    until Choix = 0;

  finally
    Lecteurs.Free;
  end;
end.
```

## Bindings D-Bus en FreePascal

Créons maintenant une interface FreePascal plus propre pour D-Bus.

### Unit de base pour D-Bus

```pascal
unit DBusHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  TDBusBusType = (dbtSystem, dbtSession);

  { Helper pour simplifier les appels D-Bus }
  TDBusHelper = class
  private
    class function ExecuteDBusCommand(const Cmd: string): string;
  public
    class function Call(
      BusType: TDBusBusType;
      const Destination: string;
      const ObjectPath: string;
      const InterfaceName: string;
      const MethodName: string;
      const Args: array of string
    ): string;

    class function GetProperty(
      BusType: TDBusBusType;
      const Destination: string;
      const ObjectPath: string;
      const InterfaceName: string;
      const PropertyName: string
    ): string;

    class function ListNames(BusType: TDBusBusType): TStringList;
  end;

  { Classe pour les notifications }
  TDBusNotifications = class
  public
    class function Notify(
      const AppName: string;
      const Summary: string;
      const Body: string;
      TimeoutMs: Integer = 5000
    ): Integer;

    class procedure Close(NotificationId: Integer);
  end;

implementation

{ TDBusHelper }

class function TDBusHelper.ExecuteDBusCommand(const Cmd: string): string;  
var
  Proc: TProcess;
  Output: TStringList;
begin
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Proc.Executable := '/bin/bash';
    Proc.Parameters.Add('-c');
    Proc.Parameters.Add(Cmd);
    Proc.Options := [poWaitOnExit, poUsePipes];
    Proc.Execute;

    Output.LoadFromStream(Proc.Output);
    Result := Trim(Output.Text);
  finally
    Output.Free;
    Proc.Free;
  end;
end;

class function TDBusHelper.Call(
  BusType: TDBusBusType;
  const Destination: string;
  const ObjectPath: string;
  const InterfaceName: string;
  const MethodName: string;
  const Args: array of string
): string;
var
  Cmd: string;
  BusTypeStr: string;
  ArgsStr: string;
  i: Integer;
begin
  if BusType = dbtSystem then
    BusTypeStr := '--system'
  else
    BusTypeStr := '--session';

  ArgsStr := '';
  for i := Low(Args) to High(Args) do
    ArgsStr := ArgsStr + ' ' + Args[i];

  Cmd := Format(
    'dbus-send %s --print-reply ' +
    '--dest=%s ' +
    '%s ' +
    '%s.%s%s',
    [BusTypeStr, Destination, ObjectPath, InterfaceName, MethodName, ArgsStr]
  );

  Result := ExecuteDBusCommand(Cmd);
end;

class function TDBusHelper.GetProperty(
  BusType: TDBusBusType;
  const Destination: string;
  const ObjectPath: string;
  const InterfaceName: string;
  const PropertyName: string
): string;
begin
  Result := Call(
    BusType,
    Destination,
    ObjectPath,
    'org.freedesktop.DBus.Properties',
    'Get',
    [Format('string:"%s"', [InterfaceName]), Format('string:"%s"', [PropertyName])]
  );
end;

class function TDBusHelper.ListNames(BusType: TDBusBusType): TStringList;  
var
  Output: string;
begin
  Result := TStringList.Create;

  Output := Call(
    BusType,
    'org.freedesktop.DBus',
    '/org/freedesktop/DBus',
    'org.freedesktop.DBus',
    'ListNames',
    []
  );

  // Parser la sortie (simplifié)
  Result.Text := Output;
end;

{ TDBusNotifications }

class function TDBusNotifications.Notify(
  const AppName: string;
  const Summary: string;
  const Body: string;
  TimeoutMs: Integer
): Integer;
var
  Output: string;
begin
  Output := TDBusHelper.Call(
    dbtSession,
    'org.freedesktop.Notifications',
    '/org/freedesktop/Notifications',
    'org.freedesktop.Notifications',
    'Notify',
    [
      Format('string:"%s"', [AppName]),
      'uint32:0',
      'string:""',
      Format('string:"%s"', [Summary]),
      Format('string:"%s"', [Body]),
      'array:string:""',
      'dict:string:string:""',
      Format('int32:%d', [TimeoutMs])
    ]
  );

  // Extraire l'ID de la notification (simplifié)
  Result := 0; // TODO: parser correctement
end;

class procedure TDBusNotifications.Close(NotificationId: Integer);  
begin
  TDBusHelper.Call(
    dbtSession,
    'org.freedesktop.Notifications',
    '/org/freedesktop/Notifications',
    'org.freedesktop.Notifications',
    'CloseNotification',
    [Format('uint32:%d', [NotificationId])]
  );
end;

end.
```

### Utilisation du helper

```pascal
program TestDBusHelper;

{$mode objfpc}{$H+}

uses
  SysUtils, Math, DBusHelper;

var
  Services: TStringList;
  i: Integer;

begin
  WriteLn('=== Test DBus Helper ===');
  WriteLn;

  // Test 1 : Notification simple
  WriteLn('Test 1 : Notification');
  TDBusNotifications.Notify(
    'Mon Application',
    'Test D-Bus',
    'Ceci est une notification via le helper FreePascal !'
  );
  WriteLn('Notification envoyée !');
  WriteLn;

  Sleep(3000);

  // Test 2 : Lister les services
  WriteLn('Test 2 : Services disponibles sur le bus de session');
  Services := TDBusHelper.ListNames(dbtSession);
  try
    WriteLn('Nombre de services : ', Services.Count);
    WriteLn('Quelques exemples :');
    for i := 0 to Min(9, Services.Count - 1) do
      WriteLn('  ', Services[i]);
  finally
    Services.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Services système courants

### NetworkManager

Surveiller l'état du réseau.

```pascal
program SurveillanceReseau;

{$mode objfpc}{$H+}

uses
  SysUtils, DBusHelper;

function ObtenirEtatReseau: string;  
var
  Output: string;
begin
  Output := TDBusHelper.GetProperty(
    dbtSystem,
    'org.freedesktop.NetworkManager',
    '/org/freedesktop/NetworkManager',
    'org.freedesktop.NetworkManager',
    'State'
  );

  // Parser l'état (simplifié)
  if Pos('70', Output) > 0 then  // NM_STATE_CONNECTED_GLOBAL
    Result := 'Connecté (Internet)'
  else if Pos('60', Output) > 0 then  // NM_STATE_CONNECTED_SITE
    Result := 'Connecté (Local)'
  else if Pos('50', Output) > 0 then  // NM_STATE_CONNECTED_LOCAL
    Result := 'Connecté (Limité)'
  else if Pos('20', Output) > 0 then  // NM_STATE_DISCONNECTED
    Result := 'Déconnecté'
  else
    Result := 'État inconnu';
end;

begin
  WriteLn('État du réseau : ', ObtenirEtatReseau);
end.
```

### UPower (Gestion de l'alimentation)

Surveiller la batterie.

```pascal
program InfoBatterie;

{$mode objfpc}{$H+}

uses
  SysUtils, DBusHelper;

procedure AfficherInfoBatterie;  
var
  Percentage, State, TimeToEmpty: string;
begin
  WriteLn('=== Informations Batterie ===');
  WriteLn;

  // Pourcentage
  Percentage := TDBusHelper.GetProperty(
    dbtSystem,
    'org.freedesktop.UPower',
    '/org/freedesktop/UPower/devices/battery_BAT0',
    'org.freedesktop.UPower.Device',
    'Percentage'
  );
  WriteLn('Charge : ', Percentage, '%');

  // État (charging, discharging, etc.)
  State := TDBusHelper.GetProperty(
    dbtSystem,
    'org.freedesktop.UPower',
    '/org/freedesktop/UPower/devices/battery_BAT0',
    'org.freedesktop.UPower.Device',
    'State'
  );
  Write('État : ');
  if Pos('2', State) > 0 then
    WriteLn('En charge')
  else if Pos('1', State) > 0 then
    WriteLn('Décharge')
  else
    WriteLn('Inconnu');

  // Temps restant
  TimeToEmpty := TDBusHelper.GetProperty(
    dbtSystem,
    'org.freedesktop.UPower',
    '/org/freedesktop/UPower/devices/battery_BAT0',
    'org.freedesktop.UPower.Device',
    'TimeToEmpty'
  );
  WriteLn('Temps restant : ', TimeToEmpty, ' secondes');
end;

begin
  try
    AfficherInfoBatterie;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### systemd

Contrôler les services système.

```pascal
program ControleSystemd;

{$mode objfpc}{$H+}

uses
  SysUtils, DBusHelper;

procedure ListerServices;  
var
  Output: string;
begin
  WriteLn('Liste des unités systemd :');

  Output := TDBusHelper.Call(
    dbtSystem,
    'org.freedesktop.systemd1',
    '/org/freedesktop/systemd1',
    'org.freedesktop.systemd1.Manager',
    'ListUnits',
    []
  );

  WriteLn(Output);
end;

procedure RedemarrerService(const ServiceName: string);  
begin
  WriteLn('Redémarrage de ', ServiceName, '...');

  TDBusHelper.Call(
    dbtSystem,
    'org.freedesktop.systemd1',
    '/org/freedesktop/systemd1',
    'org.freedesktop.systemd1.Manager',
    'RestartUnit',
    [
      Format('string:"%s"', [ServiceName]),
      'string:"replace"'
    ]
  );

  WriteLn('Service redémarré');
end;

begin
  // Note : Nécessite les permissions appropriées
  WriteLn('=== Contrôle systemd via D-Bus ===');
  WriteLn;
  WriteLn('Attention : Ces commandes nécessitent des droits root');
  WriteLn;

  // Exemple : redémarrer un service
  // RedemarrerService('ssh.service');

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Écouter des signaux D-Bus

Pour écouter les signaux (événements asynchrones), nous devons utiliser `dbus-monitor` ou une bibliothèque plus avancée.

### Surveiller les notifications

```pascal
program SurveillanceNotifications;

{$mode objfpc}{$H+}

uses
  Process, SysUtils, Classes;

procedure SurveillerNotifications;  
var
  Proc: TProcess;
  Line: string;
begin
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'dbus-monitor';
    Proc.Parameters.Add('--session');
    Proc.Parameters.Add('interface=org.freedesktop.Notifications');
    Proc.Options := [poUsePipes];

    WriteLn('Surveillance des notifications...');
    WriteLn('Appuyez sur Ctrl+C pour arrêter');
    WriteLn;

    Proc.Execute;

    while Proc.Running do
    begin
      if Proc.Output.NumBytesAvailable > 0 then
      begin
        SetLength(Line, Proc.Output.NumBytesAvailable);
        Proc.Output.Read(Line[1], Length(Line));
        Write(Line);
      end;
      Sleep(100);
    end;

  finally
    Proc.Free;
  end;
end;

begin
  try
    SurveillerNotifications;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end.
```

## Créer un service D-Bus

Pour exposer vos propres services sur D-Bus, vous devez implémenter un serveur D-Bus. C'est plus complexe et nécessite généralement une bibliothèque D-Bus complète comme libdbus.

### Squelette de serveur (conceptuel)

```pascal
// Note : Ceci est un exemple conceptuel
// Une implémentation complète nécessiterait libdbus ou GDBus

program ServeurDBus;

{$mode objfpc}{$H+}

type
  TMonService = class
  public
    // Méthodes exposées via D-Bus
    function Additionner(a, b: Integer): Integer;
    function ObtenirVersion: string;

    // Signaux
    procedure EmettreEvenement(const Message: string);
  end;

function TMonService.Additionner(a, b: Integer): Integer;  
begin
  Result := a + b;
  WriteLn('Appel de Additionner(', a, ', ', b, ') = ', Result);
end;

function TMonService.ObtenirVersion: string;  
begin
  Result := '1.0.0';
end;

procedure TMonService.EmettreEvenement(const Message: string);  
begin
  WriteLn('Émission d''événement : ', Message);
  // Émettre un signal D-Bus
end;

var
  Service: TMonService;

begin
  Service := TMonService.Create;
  try
    WriteLn('Service D-Bus démarré');
    WriteLn('Bus name : com.example.MonService');
    WriteLn('Object path : /com/example/MonService');
    WriteLn;

    // Boucle principale (conceptuel)
    WriteLn('En attente de requêtes...');
    WriteLn('(Implémentation D-Bus complète requise)');

    ReadLn;

  finally
    Service.Free;
  end;
end.
```

## Introspection D-Bus

L'introspection permet de découvrir les interfaces, méthodes et signaux exposés par un service.

### Examiner un service

```pascal
program IntrospectionDBus;

{$mode objfpc}{$H+}

uses
  SysUtils, DBusHelper;

procedure IntrospectionService(const BusName, ObjectPath: string);  
var
  XML: string;
begin
  WriteLn('=== Introspection de ', BusName, ' ===');
  WriteLn('Chemin objet : ', ObjectPath);
  WriteLn;

  XML := TDBusHelper.Call(
    dbtSession,
    BusName,
    ObjectPath,
    'org.freedesktop.DBus.Introspectable',
    'Introspect',
    []
  );

  WriteLn('XML d''introspection :');
  WriteLn(XML);
end;

begin
  // Exemple : introspection du service de notifications
  IntrospectionService(
    'org.freedesktop.Notifications',
    '/org/freedesktop/Notifications'
  );

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Parser le XML d'introspection

```pascal
program ParseIntrospection;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DOM, XMLRead;

procedure AfficherIntrospection(const BusName, ObjectPath: string);  
var
  XML: string;
  Doc: TXMLDocument;
  Node, ChildNode: TDOMNode;
  i, j: Integer;
begin
  // Obtenir le XML (simplifié - utiliser TDBusHelper en réel)
  XML := '<node>' +
    '<interface name="org.freedesktop.Notifications">' +
    '<method name="Notify">' +
    '<arg name="app_name" type="s" direction="in"/>' +
    '<arg name="id" type="u" direction="out"/>' +
    '</method>' +
    '<signal name="NotificationClosed">' +
    '<arg name="id" type="u"/>' +
    '</signal>' +
    '</interface>' +
    '</node>';

  // Parser le XML
  ReadXMLFragment(Doc, XML);
  try
    WriteLn('=== Interfaces disponibles ===');
    WriteLn;

    Node := Doc.DocumentElement.FirstChild;
    while Node <> nil do
    begin
      if Node.NodeName = 'interface' then
      begin
        WriteLn('Interface : ', TDOMElement(Node).GetAttribute('name'));

        // Lister les méthodes
        ChildNode := Node.FirstChild;
        while ChildNode <> nil do
        begin
          if ChildNode.NodeName = 'method' then
            WriteLn('  Méthode : ', TDOMElement(ChildNode).GetAttribute('name'))
          else if ChildNode.NodeName = 'signal' then
            WriteLn('  Signal : ', TDOMElement(ChildNode).GetAttribute('name'))
          else if ChildNode.NodeName = 'property' then
            WriteLn('  Propriété : ', TDOMElement(ChildNode).GetAttribute('name'));

          ChildNode := ChildNode.NextSibling;
        end;

        WriteLn;
      end;

      Node := Node.NextSibling;
    end;

  finally
    Doc.Free;
  end;
end;

begin
  AfficherIntrospection(
    'org.freedesktop.Notifications',
    '/org/freedesktop/Notifications'
  );
end.
```

## Applications pratiques

### Application de bureau avec notifications

```pascal
program AppAvecNotifications;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DBusHelper, Process;

type
  TMonApplication = class
  private
    FNomApp: string;
    procedure AfficherNotification(const Titre, Message: string;
      Urgence: Integer = 1);
  public
    constructor Create(const NomApp: string);
    procedure Demarrer;
  end;

constructor TMonApplication.Create(const NomApp: string);  
begin
  inherited Create;
  FNomApp := NomApp;
end;

procedure TMonApplication.AfficherNotification(const Titre, Message: string;
  Urgence: Integer);
var
  IconName: string;
begin
  // Choisir l'icône selon l'urgence
  case Urgence of
    0: IconName := 'dialog-information';
    1: IconName := 'dialog-warning';
    2: IconName := 'dialog-error';
  else
    IconName := '';
  end;

  TDBusHelper.Call(
    dbtSession,
    'org.freedesktop.Notifications',
    '/org/freedesktop/Notifications',
    'org.freedesktop.Notifications',
    'Notify',
    [
      Format('string:"%s"', [FNomApp]),
      'uint32:0',
      Format('string:"%s"', [IconName]),
      Format('string:"%s"', [Titre]),
      Format('string:"%s"', [Message]),
      'array:string:""',
      Format('dict:string:variant:"urgency",byte:%d', [Urgence]),
      'int32:5000'
    ]
  );
end;

procedure TMonApplication.Demarrer;  
var
  Choix: Integer;
begin
  WriteLn('=== ', FNomApp, ' ===');
  WriteLn;

  repeat
    WriteLn('Menu :');
    WriteLn('  1. Notification information');
    WriteLn('  2. Notification avertissement');
    WriteLn('  3. Notification erreur');
    WriteLn('  4. Simuler un long traitement');
    WriteLn('  0. Quitter');
    Write('Choix : ');
    ReadLn(Choix);
    WriteLn;

    case Choix of
      1: AfficherNotification('Information',
           'Ceci est une notification d''information', 0);
      2: AfficherNotification('Avertissement',
           'Attention : quelque chose nécessite votre attention', 1);
      3: AfficherNotification('Erreur',
           'Une erreur s''est produite !', 2);
      4: begin
           AfficherNotification('Traitement',
             'Début du traitement...', 0);

           WriteLn('Traitement en cours...');
           Sleep(3000);

           AfficherNotification('Terminé',
             'Le traitement est terminé avec succès !', 0);
         end;
    end;

  until Choix = 0;
end;

var
  App: TMonApplication;

begin
  App := TMonApplication.Create('Mon Application FreePascal');
  try
    App.Demarrer;
  finally
    App.Free;
  end;
end.
```

### Surveillance de l'état du système

```pascal
program SurveillanceSysteme;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DBusHelper;

type
  TInfoSysteme = record
    EtatReseau: string;
    PourcentageBatterie: Integer;
    EnCharge: Boolean;
    NombreNotifications: Integer;
  end;

function ObtenirInfoSysteme: TInfoSysteme;  
var
  Output: string;
begin
  // État réseau
  try
    Output := TDBusHelper.GetProperty(
      dbtSystem,
      'org.freedesktop.NetworkManager',
      '/org/freedesktop/NetworkManager',
      'org.freedesktop.NetworkManager',
      'State'
    );

    if Pos('70', Output) > 0 then
      Result.EtatReseau := 'Connecté'
    else
      Result.EtatReseau := 'Déconnecté';
  except
    Result.EtatReseau := 'Inconnu';
  end;

  // Batterie
  try
    Output := TDBusHelper.GetProperty(
      dbtSystem,
      'org.freedesktop.UPower',
      '/org/freedesktop/UPower/devices/battery_BAT0',
      'org.freedesktop.UPower.Device',
      'Percentage'
    );
    // Parser la sortie pour extraire le pourcentage
    Result.PourcentageBatterie := 0; // Simplification

    Output := TDBusHelper.GetProperty(
      dbtSystem,
      'org.freedesktop.UPower',
      '/org/freedesktop/UPower/devices/battery_BAT0',
      'org.freedesktop.UPower.Device',
      'State'
    );
    Result.EnCharge := Pos('2', Output) > 0; // 2 = charging
  except
    Result.PourcentageBatterie := -1;
    Result.EnCharge := False;
  end;

  Result.NombreNotifications := 0; // Simplification
end;

procedure AfficherInfoSysteme;  
var
  Info: TInfoSysteme;
begin
  Info := ObtenirInfoSysteme;

  WriteLn('╔════════════════════════════════════╗');
  WriteLn('║   INFORMATIONS SYSTÈME             ║');
  WriteLn('╠════════════════════════════════════╣');
  WriteLn('║ Réseau    : ', Info.EtatReseau:20, ' ║');

  if Info.PourcentageBatterie >= 0 then
  begin
    WriteLn('║ Batterie  : ', Info.PourcentageBatterie:3, '%':17, ' ║');
    if Info.EnCharge then
      WriteLn('║ État      : En charge':34, ' ║')
    else
      WriteLn('║ État      : Sur batterie':34, ' ║');
  end
  else
    WriteLn('║ Batterie  : Non disponible':34, ' ║');

  WriteLn('╚════════════════════════════════════╝');
end;

begin
  WriteLn('=== Surveillance du système via D-Bus ===');
  WriteLn;

  repeat
    AfficherInfoSysteme;
    WriteLn;
    WriteLn('Actualisation dans 5 secondes... (Ctrl+C pour quitter)');
    Sleep(5000);
    WriteLn;
  until False;
end.
```

### Contrôle de lecteur multimédia avancé

```pascal
program LecteurMediaAvance;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DBusHelper;

type
  TInfoPiste = record
    Titre: string;
    Artiste: string;
    Album: string;
    Duree: Integer;
  end;

  TLecteurMedia = class
  private
    FBusName: string;
    function ObtenirMetadata: TInfoPiste;
  public
    constructor Create(const BusName: string);

    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Next;
    procedure Previous;

    function ObtenirStatut: string;
    function ObtenirPosition: Integer;
    procedure DefinirPosition(Secondes: Integer);
    procedure DefinirVolume(Niveau: Double);

    procedure AfficherInfo;
  end;

constructor TLecteurMedia.Create(const BusName: string);  
begin
  inherited Create;
  FBusName := BusName;
end;

procedure TLecteurMedia.Play;  
begin
  TDBusHelper.Call(
    dbtSession,
    FBusName,
    '/org/mpris/MediaPlayer2',
    'org.mpris.MediaPlayer2.Player',
    'Play',
    []
  );
end;

procedure TLecteurMedia.Pause;  
begin
  TDBusHelper.Call(
    dbtSession,
    FBusName,
    '/org/mpris/MediaPlayer2',
    'org.mpris.MediaPlayer2.Player',
    'Pause',
    []
  );
end;

procedure TLecteurMedia.Stop;  
begin
  TDBusHelper.Call(
    dbtSession,
    FBusName,
    '/org/mpris/MediaPlayer2',
    'org.mpris.MediaPlayer2.Player',
    'Stop',
    []
  );
end;

procedure TLecteurMedia.Next;  
begin
  TDBusHelper.Call(
    dbtSession,
    FBusName,
    '/org/mpris/MediaPlayer2',
    'org.mpris.MediaPlayer2.Player',
    'Next',
    []
  );
end;

procedure TLecteurMedia.Previous;  
begin
  TDBusHelper.Call(
    dbtSession,
    FBusName,
    '/org/mpris/MediaPlayer2',
    'org.mpris.MediaPlayer2.Player',
    'Previous',
    []
  );
end;

function TLecteurMedia.ObtenirStatut: string;  
var
  Output: string;
begin
  Output := TDBusHelper.GetProperty(
    dbtSession,
    FBusName,
    '/org/mpris/MediaPlayer2',
    'org.mpris.MediaPlayer2.Player',
    'PlaybackStatus'
  );

  if Pos('Playing', Output) > 0 then
    Result := 'Lecture en cours'
  else if Pos('Paused', Output) > 0 then
    Result := 'En pause'
  else if Pos('Stopped', Output) > 0 then
    Result := 'Arrêté'
  else
    Result := 'Inconnu';
end;

function TLecteurMedia.ObtenirMetadata: TInfoPiste;  
var
  Output: string;
begin
  // Simplifié - nécessite un parsing XML/DBUS réel
  Result.Titre := 'Titre inconnu';
  Result.Artiste := 'Artiste inconnu';
  Result.Album := 'Album inconnu';
  Result.Duree := 0;
end;

function TLecteurMedia.ObtenirPosition: Integer;  
begin
  // Position en microsecondes, converti en secondes
  Result := 0; // Simplification
end;

procedure TLecteurMedia.DefinirPosition(Secondes: Integer);  
begin
  TDBusHelper.Call(
    dbtSession,
    FBusName,
    '/org/mpris/MediaPlayer2',
    'org.mpris.MediaPlayer2.Player',
    'Seek',
    [Format('int64:%d', [Int64(Secondes) * 1000000])]
  );
end;

procedure TLecteurMedia.DefinirVolume(Niveau: Double);  
begin
  // Volume entre 0.0 et 1.0
  if (Niveau < 0) or (Niveau > 1) then
    raise Exception.Create('Volume doit être entre 0.0 et 1.0');

  // SetProperty nécessite un appel plus complexe
  WriteLn('Définition du volume : ', Niveau:0:2);
end;

procedure TLecteurMedia.AfficherInfo;  
var
  Info: TInfoPiste;
begin
  Info := ObtenirMetadata;

  WriteLn('╔════════════════════════════════════════════╗');
  WriteLn('║          LECTEUR MULTIMÉDIA                ║');
  WriteLn('╠════════════════════════════════════════════╣');
  WriteLn('║ Statut  : ', ObtenirStatut:30, ' ║');
  WriteLn('║ Titre   : ', Info.Titre:30, ' ║');
  WriteLn('║ Artiste : ', Info.Artiste:30, ' ║');
  WriteLn('║ Album   : ', Info.Album:30, ' ║');
  WriteLn('╚════════════════════════════════════════════╝');
end;

var
  Lecteur: TLecteurMedia;
  Services: TStringList;
  BusName: string;
  i: Integer;
  Commande: string;

begin
  WriteLn('=== Contrôle lecteur multimédia ===');
  WriteLn;

  // Trouver un lecteur
  Services := TDBusHelper.ListNames(dbtSession);
  try
    BusName := '';
    for i := 0 to Services.Count - 1 do
    begin
      if Pos('org.mpris.MediaPlayer2', Services[i]) > 0 then
      begin
        BusName := Trim(Services[i]);
        Break;
      end;
    end;

    if BusName = '' then
    begin
      WriteLn('Aucun lecteur multimédia compatible MPRIS trouvé.');
      WriteLn('Lancez Spotify, VLC, Rhythmbox ou un autre lecteur.');
      Exit;
    end;

    WriteLn('Lecteur trouvé : ', BusName);
    WriteLn;

  finally
    Services.Free;
  end;

  Lecteur := TLecteurMedia.Create(BusName);
  try
    Lecteur.AfficherInfo;
    WriteLn;
    WriteLn('Commandes disponibles : play, pause, stop, next, prev, info, quit');

    repeat
      Write('> ');
      ReadLn(Commande);
      Commande := LowerCase(Trim(Commande));

      if Commande = 'play' then
        Lecteur.Play
      else if Commande = 'pause' then
        Lecteur.Pause
      else if Commande = 'stop' then
        Lecteur.Stop
      else if Commande = 'next' then
        Lecteur.Next
      else if (Commande = 'prev') or (Commande = 'previous') then
        Lecteur.Previous
      else if Commande = 'info' then
        Lecteur.AfficherInfo;

    until Commande = 'quit';

  finally
    Lecteur.Free;
  end;
end.
```

## Sécurité et permissions

### Politiques D-Bus

D-Bus utilise des fichiers de configuration pour contrôler l'accès aux services.

**Emplacement** :
- System bus : `/etc/dbus-1/system.d/`
- Session bus : `/etc/dbus-1/session.d/`

**Exemple de politique** (`/etc/dbus-1/system.d/com.example.MonService.conf`) :

```xml
<!DOCTYPE busconfig PUBLIC
 "-//freedesktop//DTD D-BUS Bus Configuration 1.0//EN"
 "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
<busconfig>
  <!-- Autoriser le service à posséder ce nom -->
  <policy user="root">
    <allow own="com.example.MonService"/>
  </policy>

  <!-- Autoriser tout le monde à appeler les méthodes -->
  <policy context="default">
    <allow send_destination="com.example.MonService"/>
    <allow receive_sender="com.example.MonService"/>
  </policy>

  <!-- Refuser l'accès à certaines méthodes sensibles -->
  <policy context="default">
    <deny send_destination="com.example.MonService"
          send_interface="com.example.MonService"
          send_member="MethodeSensible"/>
  </policy>
</busconfig>
```

### Vérifier les permissions

```pascal
program VerificationPermissions;

{$mode objfpc}{$H+}

uses
  SysUtils, BaseUnix;

function EstRoot: Boolean;  
begin
  Result := FpGetUID = 0;
end;

procedure TesterAccesSystemBus;  
var
  Output: string;
begin
  WriteLn('=== Test d''accès au System Bus ===');
  WriteLn;

  if EstRoot then
    WriteLn('✓ Exécuté en tant que root')
  else
    WriteLn('✗ PAS exécuté en tant que root');

  WriteLn;
  WriteLn('Tentative d''accès à systemd...');

  try
    Output := TDBusHelper.Call(
      dbtSystem,
      'org.freedesktop.systemd1',
      '/org/freedesktop/systemd1',
      'org.freedesktop.systemd1.Manager',
      'GetDefaultTarget',
      []
    );

    WriteLn('✓ Accès autorisé');
    WriteLn('Résultat : ', Output);
  except
    on E: Exception do
    begin
      WriteLn('✗ Accès refusé');
      WriteLn('Erreur : ', E.Message);
    end;
  end;
end;

begin
  TesterAccesSystemBus;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Débogage D-Bus

### Outils de diagnostic

#### 1. dbus-monitor

Surveiller tous les messages D-Bus en temps réel.

```bash
# Session bus
dbus-monitor --session

# System bus (nécessite root)
sudo dbus-monitor --system

# Filtrer par interface
dbus-monitor --session "interface='org.freedesktop.Notifications'"

# Filtrer par chemin
dbus-monitor --session "path='/org/freedesktop/Notifications'"
```

#### 2. d-feet

Interface graphique pour explorer D-Bus.

```bash
# Installation
sudo apt install d-feet

# Lancement
d-feet
```

**Fonctionnalités** :
- Explorer les services disponibles
- Voir les interfaces, méthodes et propriétés
- Tester les appels de méthodes
- Surveiller les signaux

#### 3. busctl

Outil moderne pour interagir avec D-Bus (systemd).

```bash
# Lister les services
busctl list

# Introspection
busctl introspect org.freedesktop.Notifications \
  /org/freedesktop/Notifications

# Appeler une méthode
busctl call org.freedesktop.Notifications \
  /org/freedesktop/Notifications \
  org.freedesktop.Notifications Notify \
  susssasa{sv}i "Test" 0 "" "Titre" "Message" 0 0 5000

# Surveiller
busctl monitor org.freedesktop.Notifications
```

### Tracer les appels depuis FreePascal

```pascal
program TraceDBus;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TDBusLogger = class
  private
    FLogFile: TextFile;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Log(const Message: string);
  end;

constructor TDBusLogger.Create(const FileName: string);  
begin
  AssignFile(FLogFile, FileName);
  Rewrite(FLogFile);
  Log('=== Début du traçage D-Bus ===');
end;

destructor TDBusLogger.Destroy;  
begin
  Log('=== Fin du traçage D-Bus ===');
  CloseFile(FLogFile);
  inherited;
end;

procedure TDBusLogger.Log(const Message: string);  
begin
  WriteLn(FLogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
    ' - ', Message);
  Flush(FLogFile);
  WriteLn(Message); // Aussi à l'écran
end;

var
  Logger: TDBusLogger;

begin
  Logger := TDBusLogger.Create('dbus_trace.log');
  try
    Logger.Log('Appel de notification');

    try
      TDBusNotifications.Notify(
        'Test Trace',
        'Notification',
        'Message de test'
      );
      Logger.Log('Notification envoyée avec succès');
    except
      on E: Exception do
        Logger.Log('ERREUR: ' + E.Message);
    end;

  finally
    Logger.Free;
  end;

  WriteLn('Log sauvegardé dans dbus_trace.log');
end.
```

## Bonnes pratiques

### ✅ À faire

#### 1. Utiliser les services standard

```pascal
// ✅ Bon : Utiliser le service de notifications standard
TDBusNotifications.Notify('Mon App', 'Titre', 'Message');

// ❌ Mauvais : Créer son propre système de notifications
```

#### 2. Gérer les erreurs gracieusement

```pascal
try
  // Appel D-Bus
except
  on E: Exception do
  begin
    // Ne pas crasher si D-Bus n'est pas disponible
    WriteLn('D-Bus non disponible : ', E.Message);
    // Utiliser un fallback
  end;
end;
```

#### 3. Respecter les standards Freedesktop

- Suivre les conventions de nommage
- Implémenter les interfaces standard
- Utiliser les types D-Bus appropriés

#### 4. Documenter les services exposés

```pascal
// Bien documenter votre service
{
  Service D-Bus : com.example.MonApp
  Object Path : /com/example/MonApp
  Interface : com.example.MonApp.Control

  Méthodes :
    - Start() : Démarre l'application
    - Stop() : Arrête l'application
    - GetStatus() → string : Retourne le statut

  Signaux :
    - StatusChanged(string newStatus)
}
```

#### 5. Tester sur plusieurs environnements

- GNOME (utilise D-Bus intensivement)
- KDE Plasma (aussi basé sur D-Bus)
- XFCE, LXDE (support partiel)
- Serveurs sans GUI

### ❌ À éviter

#### 1. Ne pas polluer le bus

```pascal
// ❌ Mauvais : Envoyer des messages en boucle
while True do
  TDBusNotifications.Notify('App', 'Spam', 'Message');

// ✅ Bon : Limiter les notifications
if TimeSinceLastNotif > 5000 then
  TDBusNotifications.Notify('App', 'Info', 'Message');
```

#### 2. Ne pas ignorer les timeouts

```pascal
// ✅ Gérer les appels qui peuvent bloquer
SetTimeout(5000); // 5 secondes max  
try
  DBusCall(...);
except
  on ETimeout: ...
end;
```

#### 3. Ne pas assumer que D-Bus est toujours disponible

```pascal
// ✅ Vérifier avant d'utiliser
if DBusEstDisponible then
  // Utiliser D-Bus
else
  // Fallback
```

## Alternatives à D-Bus

### Quand ne pas utiliser D-Bus

1. **Applications portables** : D-Bus est spécifique Linux/BSD
2. **Communication réseau** : D-Bus est local à la machine
3. **Haute performance** : Overhead de sérialisation
4. **Systèmes embarqués** : Peut être trop lourd

### Alternatives

#### 1. Sockets Unix

Plus léger, plus direct.

```pascal
// Socket Unix domain
Socket := socket(AF_UNIX, SOCK_STREAM, 0);
```

#### 2. Pipes nommés (FIFO)

Simple pour la communication unidirectionnelle.

```bash
mkfifo /tmp/monpipe
```

#### 3. Mémoire partagée

Le plus rapide pour des données volumineuses.

```pascal
// shm_open, mmap
```

#### 4. Message queues POSIX

Communication asynchrone avec files d'attente.

```pascal
// mq_open, mq_send, mq_receive
```

## Checklist de déploiement

Avant de distribuer une application utilisant D-Bus :

- [ ] Tester sur plusieurs distributions (Ubuntu, Fedora, Arch)
- [ ] Tester avec et sans environnement desktop
- [ ] Vérifier les permissions (System bus vs Session bus)
- [ ] Documenter les dépendances D-Bus
- [ ] Fournir un fallback si D-Bus n'est pas disponible
- [ ] Créer les fichiers de politique si nécessaire
- [ ] Tester les services persistent après déconnexion
- [ ] Vérifier la compatibilité avec systemd
- [ ] Logger les erreurs D-Bus pour le support
- [ ] Documenter les services D-Bus exposés

## Conclusion

D-Bus est le **standard de facto** pour la communication inter-processus sous Linux moderne. Il offre une intégration profonde avec le desktop et les services système.

### Points clés à retenir

✅ **Avantages de D-Bus** :
- Standard ouvert et bien documenté
- Intégration native Linux desktop
- Découverte de services automatique
- Support des signaux asynchrones
- Sécurité via politiques configurables
- Introspection complète

⚠️ **Limitations** :
- Spécifique Linux/Unix
- Courbe d'apprentissage
- Overhead de performance
- Complexité pour services simples

### Quand utiliser D-Bus depuis FreePascal ?

**Utilisez D-Bus si** :
- Vous ciblez Linux/BSD
- Vous voulez une intégration desktop native
- Vous devez interagir avec des services système
- Vous créez une application desktop moderne
- Vous voulez des notifications natives

**Évitez D-Bus si** :
- Vous visez la portabilité multi-plateforme
- Vous avez besoin de haute performance
- Vous développez pour systèmes embarqués
- La communication est simple (pipes suffisent)

### Ressources pour aller plus loin

**Documentation officielle** :
- D-Bus Specification : https://dbus.freedesktop.org/doc/dbus-specification.html
- D-Bus Tutorial : https://dbus.freedesktop.org/doc/dbus-tutorial.html
- Freedesktop.org Standards : https://www.freedesktop.org/wiki/Specifications/

**Outils** :
- d-feet : Interface graphique d'exploration D-Bus
- dbus-monitor : Surveillance des messages en temps réel
- busctl : Outil systemd pour D-Bus
- gdbus : Outil GLib pour D-Bus

**Spécifications importantes** :
- Desktop Notifications : https://specifications.freedesktop.org/notification-spec/
- MPRIS (Media Player) : https://specifications.freedesktop.org/mpris-spec/
- Secret Service API : https://specifications.freedesktop.org/secret-service/

**Communauté** :
- Forum Lazarus (section Linux)
- freedesktop.org mailing lists
- Stack Overflow (tags: dbus, linux-desktop)

### Exemples de projets utilisant D-Bus

**Applications desktop** :
- GNOME : Utilise D-Bus pour toute la communication entre composants
- KDE Plasma : Communication via D-Bus et Qt D-Bus
- Firefox : Notifications et intégration système via D-Bus
- Spotify : Contrôle via MPRIS (D-Bus)

**Services système** :
- NetworkManager : Gestion réseau
- BlueZ : Stack Bluetooth
- systemd : Init system et gestion de services
- UPower : Gestion de l'alimentation

### Bibliothèques D-Bus pour FreePascal

Bien que nous ayons créé nos propres helpers simples dans ce tutoriel, voici des options plus complètes :

#### 1. fpdbus (en développement)

Projet communautaire pour des bindings D-Bus natifs FreePascal.

**Avantages** :
- Intégration native
- Pas de dépendances externes
- Orienté objet

**État** : En développement actif

#### 2. Bindings via libdbus

Utiliser directement la bibliothèque C libdbus.

**Avantages** :
- Complet et mature
- Toutes les fonctionnalités D-Bus
- Bien documenté

**Inconvénients** :
- API C bas niveau
- Plus complexe à utiliser

#### 3. GDBus via GLib

Si vous utilisez déjà GTK dans votre application.

**Avantages** :
- Intégré à GTK
- API de haut niveau
- Très utilisé

**Inconvénients** :
- Dépendance GLib
- Pas natif FreePascal

### Feuille de route pour maîtriser D-Bus

#### Niveau débutant
1. ✅ Comprendre les concepts (bus, services, objets)
2. ✅ Utiliser dbus-send pour des appels simples
3. ✅ Afficher des notifications
4. ✅ Lister les services disponibles

#### Niveau intermédiaire
1. Créer des helpers FreePascal réutilisables
2. Contrôler des applications tierces (lecteurs média)
3. Interagir avec les services système (NetworkManager, UPower)
4. Parser l'introspection XML

#### Niveau avancé
1. Créer son propre service D-Bus
2. Implémenter des signaux asynchrones
3. Gérer les politiques de sécurité
4. Optimiser les performances

#### Niveau expert
1. Contribuer aux bindings FreePascal
2. Créer des services système complexes
3. Intégration profonde avec le desktop
4. Debugging avancé avec GDB

### Comparaison : D-Bus vs COM (Windows)

Pour mieux comprendre D-Bus si vous venez de Windows :

| Aspect | D-Bus (Linux) | COM (Windows) |
|--------|---------------|---------------|
| **Philosophie** | Bus de messages centralisé | Appels directs inter-objets |
| **Transport** | Socket Unix | RPC / Mémoire partagée |
| **Découverte** | Introspection dynamique | Registry + Type Libraries |
| **Identifiants** | Bus names (texte) | GUIDs (128 bits) |
| **Interfaces** | XML (introspectable) | IDL compilé |
| **Signaux** | Asynchrones natifs | Connection Points |
| **Sécurité** | Politiques XML | ACLs Windows |
| **Performance** | Overhead sérialisation | Appels directs plus rapides |
| **Portabilité** | Linux/BSD/Unix | Windows uniquement |

**En résumé** :
- **D-Bus** : Plus ouvert, découvrable, orienté messages
- **COM** : Plus rapide, plus complexe, orienté objets

### Anti-patterns à éviter

#### 1. Sur-utilisation de D-Bus

```pascal
// ❌ Mauvais : Utiliser D-Bus pour tout
procedure MettreAJourUI;  
begin
  DBusCall('com.example.App', 'UpdateUI'); // Inutile !
end;

// ✅ Bon : Appel direct dans le même processus
procedure MettreAJourUI;  
begin
  Form1.UpdateDisplay; // Direct
end;
```

**Règle** : D-Bus est pour la communication **inter-processus**, pas intra-processus.

#### 2. Ignorer les erreurs

```pascal
// ❌ Mauvais
DBusCall(...); // Et si ça échoue ?

// ✅ Bon
try
  DBusCall(...);
except
  on E: Exception do
  begin
    LogError('D-Bus error: ' + E.Message);
    // Fallback ou notification utilisateur
  end;
end;
```

#### 3. Bloquer l'interface utilisateur

```pascal
// ❌ Mauvais : Appel synchrone dans le thread UI
procedure TForm1.Button1Click(Sender: TObject);  
var
  Res: string;
begin
  Res := DBusLongOperation; // Bloque l'UI !
  ShowMessage(Res);
end;

// ✅ Bon : Asynchrone (utiliser un thread dédié)
// Note : les procédures anonymes nécessitent {$modeswitch anonymousfunctions}
// ou {$mode delphi} pour la syntaxe ci-dessous
procedure TForm1.Button1Click(Sender: TObject);  
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      Res: string;
    begin
      Res := DBusLongOperation;
      TThread.Synchronize(nil,
        procedure
        begin
          ShowMessage(Res);
        end
      );
    end
  ).Start;
end;
```

#### 4. Créer trop de services

```pascal
// ❌ Mauvais : Un service D-Bus par fonctionnalité
com.example.App.Database  
com.example.App.UI  
com.example.App.Network  
com.example.App.Config

// ✅ Bon : Un service principal avec interfaces
com.example.App
  ├─ /Database
  ├─ /UI
  ├─ /Network
  └─ /Config
```

### Patterns de conception D-Bus

#### Pattern 1 : Façade

Masquer la complexité D-Bus derrière une interface simple.

```pascal
type
  TSystemNotifier = class
  public
    class procedure Info(const Msg: string);
    class procedure Warning(const Msg: string);
    class procedure Error(const Msg: string);
  end;

class procedure TSystemNotifier.Info(const Msg: string);  
begin
  try
    TDBusNotifications.Notify('Mon App', 'Information', Msg);
  except
    // Fallback silencieux
    WriteLn('[INFO] ', Msg);
  end;
end;

// Utilisation simple
TSystemNotifier.Info('Traitement terminé');
```

#### Pattern 2 : Observateur (Signaux)

Recevoir des notifications d'événements système.

```pascal
type
  TNetworkObserver = class
  private
    FOnStateChanged: TNotifyEvent;
    procedure MonitorNetwork;
  public
    procedure Start;
    procedure Stop;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

procedure TNetworkObserver.MonitorNetwork;  
begin
  // Surveiller les signaux NetworkManager
  // StateChanged signal
end;
```

#### Pattern 3 : Proxy

Représenter un service distant localement.

```pascal
type
  TMediaPlayerProxy = class
  private
    FBusName: string;
  public
    constructor Create(const BusName: string);
    procedure Play;
    procedure Pause;
    function GetTitle: string;
    // ... autres méthodes
  end;

// Utilisation
var
  Player: TMediaPlayerProxy;
begin
  Player := TMediaPlayerProxy.Create('org.mpris.MediaPlayer2.spotify');
  Player.Play;
```

### Tests et validation

#### Tester sans D-Bus

```pascal
type
  IDBusService = interface
    procedure Notify(const Msg: string);
  end;

  TRealDBusService = class(TInterfacedObject, IDBusService)
    procedure Notify(const Msg: string);
  end;

  TMockDBusService = class(TInterfacedObject, IDBusService)
    procedure Notify(const Msg: string);
  end;

procedure TRealDBusService.Notify(const Msg: string);  
begin
  TDBusNotifications.Notify('App', 'Info', Msg);
end;

procedure TMockDBusService.Notify(const Msg: string);  
begin
  WriteLn('[MOCK] Notification: ', Msg);
end;

// Dans l'application
var
  DBusService: IDBusService;
begin
  {$IFDEF DEBUG}
  DBusService := TMockDBusService.Create;  // Mock pour tests
  {$ELSE}
  DBusService := TRealDBusService.Create;  // Réel en production
  {$ENDIF}
end;
```

#### Tests d'intégration

```pascal
program TestIntegrationDBus;

{$mode objfpc}{$H+}

uses
  SysUtils, DBusHelper;

var
  TestsPasses: Integer = 0;
  TestsTotal: Integer = 0;
  Services: TStringList;

procedure Test(const Nom: string; Resultat: Boolean);  
begin
  Inc(TestsTotal);
  Write('Test ', TestsTotal, ': ', Nom, ' ... ');
  if Resultat then
  begin
    WriteLn('✓ OK');
    Inc(TestsPasses);
  end
  else
    WriteLn('✗ ÉCHEC');
end;

begin
  WriteLn('=== Tests D-Bus ===');
  WriteLn;

  // Test 1 : Service de notifications disponible
  Test('Service notifications disponible',
    DBusServiceExists('org.freedesktop.Notifications'));

  // Test 2 : Envoyer une notification
  try
    TDBusNotifications.Notify('Test', 'Titre', 'Message');
    Test('Envoi de notification', True);
  except
    Test('Envoi de notification', False);
  end;

  // Test 3 : Lister les services
  Services := TDBusHelper.ListNames(dbtSession);
  try
    Test('Lister les services', Services.Count > 0);
  finally
    Services.Free;
  end;

  WriteLn;
  WriteLn('═══════════════════════════════');
  WriteLn('Résultats: ', TestsPasses, '/', TestsTotal, ' tests réussis');

  if TestsPasses = TestsTotal then
    Halt(0)  // Succès
  else
    Halt(1); // Échec
end.
```

### Intégration CI/CD

Pour tester automatiquement sur différents environnements :

```yaml
# .github/workflows/test-dbus.yml
name: Test D-Bus

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v2

    - name: Installer FreePascal
      run: sudo apt-get install -y fpc

    - name: Installer D-Bus
      run: sudo apt-get install -y dbus dbus-x11

    - name: Démarrer D-Bus session
      run: |
        eval $(dbus-launch --sh-syntax)
        echo "DBUS_SESSION_BUS_ADDRESS=$DBUS_SESSION_BUS_ADDRESS" >> $GITHUB_ENV

    - name: Compiler les tests
      run: fpc -o test_dbus test_dbus.pas

    - name: Exécuter les tests
      run: ./test_dbus
```

### Optimisations avancées

#### 1. Mise en cache des connexions

```pascal
// Note : nécessite {$mode delphi} pour TDictionary de Generics.Collections
type
  TDBusConnectionPool = class
  private
    FConnections: TDictionary<string, TDBusConnection>;
  public
    function GetConnection(const BusName: string): TDBusConnection;
    procedure ReleaseConnection(const BusName: string);
  end;
```

#### 2. Appels asynchrones

```pascal
// Note : nécessite {$mode delphi} ou {$modeswitch anonymousfunctions}
// TProc<string> n'existe pas en FPC standard, définir :
// type TStringProc = reference to procedure(const S: string);
procedure CallAsync(const Method: string; OnComplete: TStringProc);  
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      Res: string;
    begin
      Res := TDBusHelper.Call(...);
      TThread.Queue(nil,
        procedure
        begin
          OnComplete(Res);
        end
      );
    end
  ).Start;
end;
```

#### 3. Batching des appels

```pascal
// Au lieu de
for i := 1 to 100 do
  DBusNotify(Format('Message %d', [i]));

// Faire
DBusNotifyBatch([
  'Message 1',
  'Message 2',
  // ...
]);
```

## Conclusion finale

D-Bus est un outil puissant et essentiel pour créer des applications Linux modernes bien intégrées. En tant que développeur FreePascal, maîtriser D-Bus vous permet de :

🎯 **Créer des applications desktop natives** qui s'intègrent parfaitement à GNOME, KDE et autres environnements

🔧 **Interagir avec le système** via NetworkManager, systemd, UPower et autres services

🔔 **Communiquer efficacement** entre processus de manière standardisée

⚡ **Bénéficier de l'écosystème Linux** en réutilisant les services existants

Bien que la courbe d'apprentissage soit plus raide que pour des mécanismes IPC simples, **l'investissement en vaut la peine** pour toute application Linux sérieuse.

### Message clé

> D-Bus n'est pas juste une technologie IPC parmi d'autres sur Linux - c'est **LE** standard qui fait fonctionner le desktop Linux moderne. Le maîtriser, c'est maîtriser l'intégration système sous Linux.

### Prochaines étapes

Maintenant que vous avez les bases de D-Bus, vous pouvez :

1. **Explorer les services** : Utilisez `d-feet` pour découvrir ce qui est disponible
2. **Automatiser** : Créer des scripts pour contrôler votre environnement
3. **Intégrer** : Ajouter des notifications et contrôles système à vos applications
4. **Créer** : Exposer vos propres services D-Bus pour l'extensibilité
5. **Contribuer** : Participer aux bindings FreePascal D-Bus

---

## Pour aller plus loin

**Chapitres connexes du tutoriel** :
- **Chapitre 19.4** : COM/ActiveX sous Windows (équivalent Windows)
- **Chapitre 19.3** : Interfaçage avec Python (autre approche IPC)
- **Chapitre 10** : Programmation Réseau (pour IPC réseau)
- **Chapitre 11** : Multithreading (pour appels asynchrones)

**Projets pratiques suggérés** :
1. Créer un gestionnaire de notifications personnalisé
2. Contrôleur universel de lecteurs multimédia
3. Tableau de bord système (réseau, batterie, services)
4. Automatisation de tâches système via D-Bus
5. Intégration Spotify/VLC dans votre application

**Ressources en ligne** :
- 📚 D-Bus Tutorial officiel
- 🛠️ Exemples de code sur GitHub
- 💬 Forum Lazarus section Linux
- 📖 freedesktop.org specifications

Bon développement avec D-Bus et FreePascal sur Linux ! 🐧🚀

⏭️ [Java Native Interface (JNI)](/19-interoperabilite-bindings/06-java-native-interface-jni.md)
