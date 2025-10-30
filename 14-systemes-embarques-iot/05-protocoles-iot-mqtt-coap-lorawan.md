🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.5 Protocoles IoT (MQTT, CoAP, LoRaWAN)

## Introduction

L'Internet des Objets (IoT - Internet of Things) nécessite des protocoles de communication spécialement conçus pour les contraintes des objets connectés : faible consommation énergétique, bande passante limitée, connectivité intermittente, et ressources matérielles réduites. Dans cette section, nous allons explorer trois protocoles majeurs utilisés dans l'IoT et leur implémentation avec FreePascal/Lazarus.

## Vue d'ensemble des protocoles IoT

Avant de plonger dans les détails techniques, comprenons les différences fondamentales entre ces trois protocoles :

### MQTT (Message Queuing Telemetry Transport)
- **Type** : Protocole de messagerie publish/subscribe
- **Transport** : TCP/IP (port 1883, ou 8883 pour SSL/TLS)
- **Cas d'usage** : Télémétrie, domotique, capteurs connectés
- **Avantages** : Léger, fiable, largement supporté
- **Inconvénient** : Nécessite une connexion TCP permanente

### CoAP (Constrained Application Protocol)
- **Type** : Protocole web pour appareils contraints
- **Transport** : UDP (port 5683, ou 5684 pour DTLS)
- **Cas d'usage** : Réseaux de capteurs, appareils très contraints
- **Avantages** : Très léger, basé sur REST, économe en énergie
- **Inconvénient** : Moins fiable que TCP (UDP)

### LoRaWAN (Long Range Wide Area Network)
- **Type** : Protocole réseau longue portée basse consommation
- **Transport** : Radio LoRa (bandes ISM : 868 MHz en Europe, 915 MHz aux USA)
- **Cas d'usage** : Agriculture, ville intelligente, objets distants
- **Avantages** : Très longue portée (plusieurs km), très faible consommation
- **Inconvénient** : Débit très faible, latence élevée

## 1. MQTT (Message Queuing Telemetry Transport)

### 1.1 Principe de fonctionnement

MQTT est un protocole de messagerie basé sur le modèle publish/subscribe (publication/abonnement). Au lieu de communiquer directement, les clients passent par un broker (courtier) central.

**Architecture MQTT :**

```
[Capteur Température] ---publish---> [Broker MQTT] ---subscribe---> [Application Web]
[Capteur Humidité]    ---publish---> [Broker MQTT] ---subscribe---> [Application Mobile]
[Actionneur]          <--publish---- [Broker MQTT] <--publish----- [Dashboard]
```

**Concepts clés :**
- **Topic** : Canal thématique (ex: `maison/salon/temperature`)
- **QoS** (Quality of Service) : Niveau de garantie de livraison (0, 1 ou 2)
- **Retain** : Le broker conserve le dernier message pour les nouveaux abonnés
- **Last Will** : Message automatique en cas de déconnexion inattendue

### 1.2 Installation des bibliothèques MQTT

Pour utiliser MQTT avec FreePascal/Lazarus, nous utiliserons la bibliothèque **TMQTTClient** disponible via OPM (Online Package Manager) ou depuis GitHub.

**Installation sur Windows :**
1. Dans Lazarus : Package → Online Package Manager
2. Rechercher "MQTT"
3. Installer le package `mqtt_client`

**Installation sur Ubuntu :**
```bash
# Installer les dépendances
sudo apt-get install libssl-dev

# Dans Lazarus, même procédure via OPM
```

**Alternative : Installation manuelle**
```bash
git clone https://github.com/prof7bit/fpc-mqtt-client.git
```

### 1.3 Exemple complet : Client MQTT Publisher

Voici un exemple d'application qui publie des données de capteur via MQTT :

```pascal
unit MQTTPublisher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mqtt;

type
  { TTemperatureSensor }
  TTemperatureSensor = class
  private
    FMQTTClient: TMQTTClient;
    FBrokerHost: string;
    FBrokerPort: Integer;
    FConnected: Boolean;
    procedure OnConnect(Sender: TObject);
    procedure OnDisconnect(Sender: TObject);
    procedure OnError(Sender: TObject; const ErrorMsg: string);
  public
    constructor Create(const ABrokerHost: string; ABrokerPort: Integer);
    destructor Destroy; override;
    function Connect: Boolean;
    procedure Disconnect;
    procedure PublishTemperature(const Topic: string; Temperature: Double);
    property Connected: Boolean read FConnected;
  end;

implementation

{ TTemperatureSensor }

constructor TTemperatureSensor.Create(const ABrokerHost: string; ABrokerPort: Integer);
begin
  FBrokerHost := ABrokerHost;
  FBrokerPort := ABrokerPort;
  FConnected := False;

  // Créer le client MQTT
  FMQTTClient := TMQTTClient.Create;
  FMQTTClient.Host := FBrokerHost;
  FMQTTClient.Port := FBrokerPort;
  FMQTTClient.ClientID := 'FreePascal_Sensor_' + IntToStr(Random(10000));

  // Configurer les événements
  FMQTTClient.OnConnect := @OnConnect;
  FMQTTClient.OnDisconnect := @OnDisconnect;
  FMQTTClient.OnError := @OnError;

  // Configuration optionnelle
  FMQTTClient.KeepAlive := 60; // Ping toutes les 60 secondes
  FMQTTClient.CleanSession := True;
end;

destructor TTemperatureSensor.Destroy;
begin
  if FConnected then
    Disconnect;
  FMQTTClient.Free;
  inherited Destroy;
end;

function TTemperatureSensor.Connect: Boolean;
begin
  try
    FMQTTClient.Connect;
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Erreur de connexion : ', E.Message);
      Result := False;
    end;
  end;
end;

procedure TTemperatureSensor.Disconnect;
begin
  if FConnected then
  begin
    FMQTTClient.Disconnect;
    FConnected := False;
  end;
end;

procedure TTemperatureSensor.PublishTemperature(const Topic: string; Temperature: Double);
var
  Payload: string;
begin
  if not FConnected then
  begin
    WriteLn('Erreur : Non connecté au broker');
    Exit;
  end;

  // Formater la charge utile en JSON
  Payload := Format('{"temperature": %.2f, "timestamp": %d, "unit": "celsius"}',
                    [Temperature, DateTimeToUnix(Now)]);

  // Publier avec QoS 1 (au moins une fois)
  FMQTTClient.Publish(Topic, Payload, 1, False);
  WriteLn('Publié : ', Topic, ' = ', Payload);
end;

procedure TTemperatureSensor.OnConnect(Sender: TObject);
begin
  FConnected := True;
  WriteLn('Connecté au broker MQTT : ', FBrokerHost, ':', FBrokerPort);
end;

procedure TTemperatureSensor.OnDisconnect(Sender: TObject);
begin
  FConnected := False;
  WriteLn('Déconnecté du broker MQTT');
end;

procedure TTemperatureSensor.OnError(Sender: TObject; const ErrorMsg: string);
begin
  WriteLn('Erreur MQTT : ', ErrorMsg);
end;

end.
```

**Programme principal :**

```pascal
program PublishSensorData;

{$mode objfpc}{$H+}

uses
  SysUtils, MQTTPublisher;

var
  Sensor: TTemperatureSensor;
  Temperature: Double;
  i: Integer;

begin
  Randomize;

  // Créer le capteur avec connexion au broker
  // Utiliser un broker public pour les tests : test.mosquitto.org
  Sensor := TTemperatureSensor.Create('test.mosquitto.org', 1883);

  try
    // Connexion au broker
    if Sensor.Connect then
    begin
      WriteLn('Simulation de capteur de température...');
      WriteLn('Appuyez sur Ctrl+C pour arrêter');
      WriteLn;

      // Publier 10 mesures
      for i := 1 to 10 do
      begin
        // Simuler une température entre 18 et 25°C
        Temperature := 18.0 + Random * 7.0;

        // Publier sur le topic
        Sensor.PublishTemperature('maison/salon/temperature', Temperature);

        // Attendre 5 secondes
        Sleep(5000);
      end;
    end
    else
      WriteLn('Impossible de se connecter au broker');
  finally
    Sensor.Free;
  end;

  WriteLn('Programme terminé');
end.
```

### 1.4 Exemple : Client MQTT Subscriber

Voici maintenant un abonné qui reçoit les messages :

```pascal
unit MQTTSubscriber;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mqtt, fpjson, jsonparser;

type
  { TTemperatureMonitor }
  TTemperatureMonitor = class
  private
    FMQTTClient: TMQTTClient;
    FBrokerHost: string;
    FBrokerPort: Integer;
    procedure OnConnect(Sender: TObject);
    procedure OnMessage(Sender: TObject; const Topic: string; const Payload: string);
    procedure ProcessTemperature(const Topic, Payload: string);
  public
    constructor Create(const ABrokerHost: string; ABrokerPort: Integer);
    destructor Destroy; override;
    procedure Connect;
    procedure Subscribe(const Topic: string);
    procedure Run; // Boucle d'attente des messages
  end;

implementation

{ TTemperatureMonitor }

constructor TTemperatureMonitor.Create(const ABrokerHost: string; ABrokerPort: Integer);
begin
  FBrokerHost := ABrokerHost;
  FBrokerPort := ABrokerPort;

  FMQTTClient := TMQTTClient.Create;
  FMQTTClient.Host := FBrokerHost;
  FMQTTClient.Port := FBrokerPort;
  FMQTTClient.ClientID := 'FreePascal_Monitor_' + IntToStr(Random(10000));
  FMQTTClient.OnConnect := @OnConnect;
  FMQTTClient.OnMessage := @OnMessage;
end;

destructor TTemperatureMonitor.Destroy;
begin
  FMQTTClient.Free;
  inherited Destroy;
end;

procedure TTemperatureMonitor.Connect;
begin
  FMQTTClient.Connect;
end;

procedure TTemperatureMonitor.Subscribe(const Topic: string);
begin
  WriteLn('Abonnement au topic : ', Topic);
  FMQTTClient.Subscribe(Topic, 1); // QoS 1
end;

procedure TTemperatureMonitor.Run;
begin
  WriteLn('En attente de messages... (Ctrl+C pour arrêter)');
  // Boucle infinie pour recevoir les messages
  while True do
  begin
    FMQTTClient.ProcessMessages(100); // Timeout 100ms
    Sleep(10);
  end;
end;

procedure TTemperatureMonitor.OnConnect(Sender: TObject);
begin
  WriteLn('Connecté au broker MQTT');
end;

procedure TTemperatureMonitor.OnMessage(Sender: TObject; const Topic: string;
  const Payload: string);
begin
  WriteLn('Message reçu sur : ', Topic);
  ProcessTemperature(Topic, Payload);
end;

procedure TTemperatureMonitor.ProcessTemperature(const Topic, Payload: string);
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  Temperature: Double;
  Timestamp: Int64;
begin
  try
    // Parser le JSON
    JSONData := GetJSON(Payload);
    try
      if JSONData is TJSONObject then
      begin
        JSONObject := TJSONObject(JSONData);
        Temperature := JSONObject.Get('temperature', 0.0);
        Timestamp := JSONObject.Get('timestamp', 0);

        WriteLn(Format('  Température : %.2f°C', [Temperature]));
        WriteLn(Format('  Horodatage  : %s', [FormatDateTime('dd/mm/yyyy hh:nn:ss',
                UnixToDateTime(Timestamp))]));
        WriteLn;

        // Ici, vous pourriez déclencher des alertes, enregistrer en base, etc.
        if Temperature > 24.0 then
          WriteLn('⚠️  ALERTE : Température élevée !');
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur lors du parsing JSON : ', E.Message);
  end;
end;

end.
```

### 1.5 Configuration d'un broker MQTT local

Pour les tests et le développement, il est recommandé d'installer un broker MQTT local.

**Sur Windows avec Mosquitto :**

```bash
# Télécharger depuis https://mosquitto.org/download/
# Ou via Chocolatey :
choco install mosquitto

# Démarrer le service
net start mosquitto
```

**Sur Ubuntu :**

```bash
# Installation
sudo apt-get update
sudo apt-get install mosquitto mosquitto-clients

# Démarrer le service
sudo systemctl start mosquitto
sudo systemctl enable mosquitto

# Vérifier le statut
sudo systemctl status mosquitto
```

**Tester avec les outils en ligne de commande :**

```bash
# Terminal 1 : Abonné
mosquitto_sub -h localhost -t "test/topic" -v

# Terminal 2 : Éditeur
mosquitto_pub -h localhost -t "test/topic" -m "Hello MQTT!"
```

## 2. CoAP (Constrained Application Protocol)

### 2.1 Principe de fonctionnement

CoAP est souvent décrit comme "HTTP pour l'IoT". Il utilise une architecture REST (GET, POST, PUT, DELETE) mais sur UDP pour minimiser la consommation.

**Caractéristiques principales :**
- Architecture REST/RESTful
- Transport UDP (plus léger que TCP)
- Modèle requête/réponse avec observe (notifications push)
- Support du multicast
- Chiffrement avec DTLS (équivalent TLS pour UDP)

**Comparaison HTTP vs CoAP :**

```
HTTP                    CoAP
----                    ----
TCP (lourd)            UDP (léger)
Port 80/443            Port 5683/5684
Header texte           Header binaire (4 octets)
Méthodes verbales      Méthodes codées
```

### 2.2 Installation de la bibliothèque CoAP

Pour FreePascal, nous pouvons utiliser la bibliothèque **fcl-web** avec des composants CoAP ou créer notre propre implémentation.

**Exemple d'implémentation simple :**

```pascal
unit SimpleCOAP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock;

type
  { Types de messages CoAP }
  TCOAPMessageType = (mtConfirmable, mtNonConfirmable, mtAcknowledgement, mtReset);

  { Codes de méthode CoAP }
  TCOAPMethod = (cmEmpty, cmGET, cmPOST, cmPUT, cmDELETE);

  { Codes de réponse CoAP }
  TCOAPResponseCode = (
    rcEmpty = 0,
    rcCreated = 65,      // 2.01
    rcDeleted = 66,      // 2.02
    rcValid = 67,        // 2.03
    rcChanged = 68,      // 2.04
    rcContent = 69,      // 2.05
    rcBadRequest = 128,  // 4.00
    rcUnauthorized = 129,// 4.01
    rcNotFound = 132,    // 4.04
    rcMethodNotAllowed = 133 // 4.05
  );

  { TCOAPClient }
  TCOAPClient = class
  private
    FSocket: TUDPBlockSocket;
    FHost: string;
    FPort: Integer;
    FMessageID: Word;
    function BuildCOAPPacket(Method: TCOAPMethod; const URI: string;
      const Payload: string): string;
    function ParseCOAPResponse(const Data: string; out ResponseCode: TCOAPResponseCode;
      out Payload: string): Boolean;
  public
    constructor Create(const AHost: string; APort: Integer = 5683);
    destructor Destroy; override;
    function GET(const URI: string; out Response: string): Boolean;
    function POST(const URI: string; const Payload: string; out Response: string): Boolean;
    function PUT(const URI: string; const Payload: string): Boolean;
    function DELETE(const URI: string): Boolean;
  end;

implementation

{ TCOAPClient }

constructor TCOAPClient.Create(const AHost: string; APort: Integer);
begin
  FHost := AHost;
  FPort := APort;
  FMessageID := Random(65536);
  FSocket := TUDPBlockSocket.Create;
end;

destructor TCOAPClient.Destroy;
begin
  FSocket.Free;
  inherited Destroy;
end;

function TCOAPClient.BuildCOAPPacket(Method: TCOAPMethod; const URI: string;
  const Payload: string): string;
var
  Header: array[0..3] of Byte;
  Token: Byte;
  Options: string;
  UriPath: TStringList;
  i: Integer;
begin
  // Version (2 bits) = 01, Type (2 bits) = 00 (CON), Token Length (4 bits) = 1
  Header[0] := $41; // 01000001

  // Code (8 bits) - Méthode
  Header[1] := Ord(Method);

  // Message ID (16 bits) - Big Endian
  Inc(FMessageID);
  Header[2] := Hi(FMessageID);
  Header[3] := Lo(FMessageID);

  // Token (optionnel, 1 octet)
  Token := Random(256);

  // Construction du paquet
  Result := '';
  for i := 0 to 3 do
    Result := Result + Chr(Header[i]);
  Result := Result + Chr(Token);

  // Options - URI-Path
  UriPath := TStringList.Create;
  try
    UriPath.Delimiter := '/';
    UriPath.StrictDelimiter := True;
    UriPath.DelimitedText := URI;

    for i := 0 to UriPath.Count - 1 do
    begin
      if UriPath[i] <> '' then
      begin
        // Option 11 (Uri-Path) - Format : delta + length + value
        Options := Options + Chr($B0 + Length(UriPath[i])); // Delta=11, Length
        Options := Options + UriPath[i];
      end;
    end;
  finally
    UriPath.Free;
  end;

  Result := Result + Options;

  // Payload marker (0xFF) et payload si présent
  if Payload <> '' then
    Result := Result + Chr($FF) + Payload;
end;

function TCOAPClient.GET(const URI: string; out Response: string): Boolean;
var
  Packet: string;
  RecvData: string;
  ResponseCode: TCOAPResponseCode;
begin
  Result := False;
  Response := '';

  try
    // Construire le paquet CoAP GET
    Packet := BuildCOAPPacket(cmGET, URI, '');

    // Envoyer via UDP
    FSocket.Connect(FHost, IntToStr(FPort));
    FSocket.SendString(Packet);

    // Recevoir la réponse (timeout 5 secondes)
    RecvData := FSocket.RecvPacket(5000);

    if FSocket.LastError = 0 then
    begin
      Result := ParseCOAPResponse(RecvData, ResponseCode, Response);
      if Result then
        WriteLn('Code de réponse CoAP : ', Ord(ResponseCode));
    end
    else
      WriteLn('Erreur UDP : ', FSocket.LastErrorDesc);
  except
    on E: Exception do
      WriteLn('Erreur GET CoAP : ', E.Message);
  end;
end;

function TCOAPClient.POST(const URI: string; const Payload: string;
  out Response: string): Boolean;
var
  Packet: string;
  RecvData: string;
  ResponseCode: TCOAPResponseCode;
begin
  Result := False;
  Response := '';

  try
    Packet := BuildCOAPPacket(cmPOST, URI, Payload);
    FSocket.Connect(FHost, IntToStr(FPort));
    FSocket.SendString(Packet);
    RecvData := FSocket.RecvPacket(5000);

    if FSocket.LastError = 0 then
      Result := ParseCOAPResponse(RecvData, ResponseCode, Response);
  except
    on E: Exception do
      WriteLn('Erreur POST CoAP : ', E.Message);
  end;
end;

function TCOAPClient.ParseCOAPResponse(const Data: string;
  out ResponseCode: TCOAPResponseCode; out Payload: string): Boolean;
var
  PayloadStart: Integer;
begin
  Result := False;
  Payload := '';

  if Length(Data) < 4 then
    Exit;

  // Extraire le code de réponse (octet 1)
  ResponseCode := TCOAPResponseCode(Ord(Data[2]));

  // Chercher le marqueur de payload (0xFF)
  PayloadStart := Pos(Chr($FF), Data);
  if PayloadStart > 0 then
    Payload := Copy(Data, PayloadStart + 1, Length(Data) - PayloadStart);

  Result := True;
end;

// Implémentations simplifiées de PUT et DELETE
function TCOAPClient.PUT(const URI: string; const Payload: string): Boolean;
var
  Response: string;
begin
  Result := POST(URI, Payload, Response); // Simplifié
end;

function TCOAPClient.DELETE(const URI: string): Boolean;
var
  Response: string;
begin
  Result := GET(URI, Response); // Simplifié
end;

end.
```

**Programme d'exemple :**

```pascal
program COAPClientExample;

{$mode objfpc}{$H+}

uses
  SysUtils, SimpleCOAP;

var
  Client: TCOAPClient;
  Response: string;

begin
  // Créer un client CoAP
  Client := TCOAPClient.Create('coap.me', 5683);

  try
    WriteLn('=== Test CoAP Client ===');
    WriteLn;

    // GET sur une ressource publique
    WriteLn('GET /hello');
    if Client.GET('/hello', Response) then
      WriteLn('Réponse : ', Response)
    else
      WriteLn('Échec de la requête');

    WriteLn;

    // POST avec données
    WriteLn('POST /test avec payload');
    if Client.POST('/test', '{"sensor": "temp", "value": 23.5}', Response) then
      WriteLn('Réponse : ', Response);

  finally
    Client.Free;
  end;

  WriteLn;
  WriteLn('Programme terminé');
end.
```

### 2.3 Avantages et limitations de CoAP

**Avantages :**
- Très léger (header de 4 octets vs ~600 octets pour HTTP)
- Économe en énergie (UDP, pas de connexion persistante)
- Architecture REST familière
- Support du multicast (un message vers plusieurs destinataires)
- Observe pattern (équivalent de WebSocket)

**Limitations :**
- UDP = pas de garantie de livraison (nécessite implémentation de la fiabilité)
- Moins de support bibliothèque que MQTT
- Complexité accrue pour la gestion des erreurs réseau
- DTLS plus difficile à implémenter que TLS

## 3. LoRaWAN (Long Range Wide Area Network)

### 3.1 Principe de fonctionnement

LoRaWAN est un protocole de communication sans fil longue portée et basse consommation, idéal pour les capteurs alimentés par batterie devant fonctionner pendant des années.

**Architecture LoRaWAN :**

```
[Capteur LoRa] )))---radio---(((  [Gateway LoRa] ---Internet--- [Network Server]
                                                                        |
                                                                [Application Server]
```

**Caractéristiques :**
- **Portée** : 2-15 km en zone urbaine, jusqu'à 45 km en zone rurale
- **Débit** : 0.3 à 50 kbps (très lent)
- **Consommation** : Quelques microampères en veille
- **Fréquences** : Bandes ISM (868 MHz Europe, 915 MHz USA, 433 MHz Asie)
- **Classes de périphériques** :
  - **Classe A** : Bidirectionnel, faible consommation (capteurs)
  - **Classe B** : Fenêtres de réception programmées (semi-temps réel)
  - **Classe C** : Écoute continue (actionneurs, forte consommation)

### 3.2 Développement avec LoRaWAN sur FreePascal

Le développement LoRaWAN implique généralement deux parties :
1. Le firmware du capteur (souvent en C/C++ sur microcontrôleur)
2. L'application serveur (peut être en FreePascal)

**Architecture typique :**

```pascal
unit LoRaWANServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mqtt, fpjson, jsonparser, dateutils;

type
  { Structure des données LoRaWAN }
  TLoRaWANDevice = record
    DevEUI: string;        // Identifiant unique du périphérique
    AppEUI: string;        // Identifiant de l'application
    DevAddr: string;       // Adresse réseau
    LastSeen: TDateTime;   // Dernière activité
    BatteryLevel: Integer; // Niveau de batterie (0-255)
    RSSI: Integer;         // Puissance du signal
    SNR: Double;           // Signal-to-Noise Ratio
  end;

  { TLoRaWANHandler }
  TLoRaWANHandler = class
  private
    FMQTTClient: TMQTTClient;
    FDevices: array of TLoRaWANDevice;
    procedure OnMQTTMessage(Sender: TObject; const Topic: string; const Payload: string);
    procedure ProcessUplinkMessage(const JSONData: TJSONObject);
    procedure DecodePayload(const PayloadHex: string; out Temperature: Double;
      out Humidity: Double);
    function FindDeviceByEUI(const DevEUI: string): Integer;
  public
    constructor Create(const MQTTBroker: string; MQTTPort: Integer);
    destructor Destroy; override;
    procedure Connect;
    procedure SubscribeToGateway(const GatewayID: string);
    procedure ProcessMessages;
    procedure SendDownlink(const DevEUI: string; const Payload: string);
  end;

implementation

{ TLoRaWANHandler }

constructor TLoRaWANHandler.Create(const MQTTBroker: string; MQTTPort: Integer);
begin
  SetLength(FDevices, 0);

  FMQTTClient := TMQTTClient.Create;
  FMQTTClient.Host := MQTTBroker;
  FMQTTClient.Port := MQTTPort;
  FMQTTClient.ClientID := 'LoRaWAN_Server_' + IntToStr(Random(10000));
  FMQTTClient.OnMessage := @OnMQTTMessage;
end;

destructor TLoRaWANHandler.Destroy;
begin
  FMQTTClient.Free;
  inherited Destroy;
end;

procedure TLoRaWANHandler.Connect;
begin
  WriteLn('Connexion au broker MQTT...');
  FMQTTClient.Connect;
  WriteLn('Connecté');
end;

procedure TLoRaWANHandler.SubscribeToGateway(const GatewayID: string);
begin
  // S'abonner aux messages uplink de la gateway
  // Format typique : application/[appID]/device/[devEUI]/rx
  FMQTTClient.Subscribe('application/+/device/+/rx', 0);
  WriteLn('Abonné aux messages uplink LoRaWAN');
end;

procedure TLoRaWANHandler.OnMQTTMessage(Sender: TObject; const Topic: string;
  const Payload: string);
var
  JSONData: TJSONData;
begin
  WriteLn('Message reçu sur : ', Topic);

  try
    JSONData := GetJSON(Payload);
    try
      if JSONData is TJSONObject then
        ProcessUplinkMessage(TJSONObject(JSONData));
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur parsing JSON : ', E.Message);
  end;
end;

procedure TLoRaWANHandler.ProcessUplinkMessage(const JSONData: TJSONObject);
var
  DevEUI: string;
  DeviceIndex: Integer;
  PayloadHex: string;
  Temperature, Humidity: Double;
  RSSI: Integer;
  SNR: Double;
  RXInfo: TJSONArray;
  TXInfo: TJSONObject;
begin
  // Extraire les informations du périphérique
  DevEUI := JSONData.Get('devEUI', '');
  PayloadHex := JSONData.Get('data', ''); // Payload encodé en hexadécimal

  WriteLn('Traitement message de : ', DevEUI);

  // Extraire les informations radio
  if JSONData.Find('rxInfo') <> nil then
  begin
    RXInfo := JSONData.Arrays['rxInfo'];
    if RXInfo.Count > 0 then
    begin
      RSSI := TJSONObject(RXInfo[0]).Get('rssi', 0);
      SNR := TJSONObject(RXInfo[0]).Get('loRaSNR', 0.0);
      WriteLn(Format('  RSSI: %d dBm, SNR: %.1f dB', [RSSI, SNR]));
    end;
  end;

  // Extraire les informations de transmission
  if JSONData.Find('txInfo') <> nil then
  begin
    TXInfo := TJSONObject(JSONData.Objects['txInfo']);
    WriteLn('  Fréquence: ', TXInfo.Get('frequency', 0), ' Hz');
    WriteLn('  Data Rate: ', TXInfo.Get('dataRate', ''));
  end;

  // Décoder le payload
  if PayloadHex <> '' then
  begin
    DecodePayload(PayloadHex, Temperature, Humidity);
    WriteLn(Format('  Température: %.1f°C', [Temperature]));
    WriteLn(Format('  Humidité: %.1f%%', [Humidity]));

    // Mettre à jour ou ajouter le périphérique
    DeviceIndex := FindDeviceByEUI(DevEUI);
    if DeviceIndex = -1 then
    begin
      // Nouveau périphérique
      SetLength(FDevices, Length(FDevices) + 1);
      DeviceIndex := High(FDevices);
      FDevices[DeviceIndex].DevEUI := DevEUI;
      WriteLn('  Nouveau périphérique enregistré');
    end;

    // Mettre à jour les informations
    FDevices[DeviceIndex].LastSeen := Now;
    FDevices[DeviceIndex].RSSI := RSSI;
    FDevices[DeviceIndex].SNR := SNR;

    // Vérifier si une action est nécessaire
    if Temperature > 30.0 then
    begin
      WriteLn('⚠️  ALERTE: Température élevée détectée!');
      // Envoyer un downlink pour activer la ventilation par exemple
      SendDownlink(DevEUI, '01'); // Commande: activer ventilation
    end;
  end;

  WriteLn;
end;

procedure TLoRaWANHandler.DecodePayload(const PayloadHex: string;
  out Temperature: Double; out Humidity: Double);
var
  i: Integer;
  TempBytes, HumBytes: array[0..1] of Byte;
  TempInt, HumInt: SmallInt;
begin
  // Format typique: 4 octets (2 pour temp, 2 pour humidité)
  // Température: SmallInt signé (en dixièmes de degrés)
  // Humidité: Word non signé (en dixièmes de pourcent)

  Temperature := 0.0;
  Humidity := 0.0;

  if Length(PayloadHex) < 8 then
  begin
    WriteLn('Payload trop court');
    Exit;
  end;

  try
    // Convertir hex en bytes (Big Endian)
    // Température (octets 0-1)
    TempBytes[0] := StrToInt('$' + Copy(PayloadHex, 1, 2));
    TempBytes[1] := StrToInt('$' + Copy(PayloadHex, 3, 2));

    // Humidité (octets 2-3)
    HumBytes[0] := StrToInt('$' + Copy(PayloadHex, 5, 2));
    HumBytes[1] := StrToInt('$' + Copy(PayloadHex, 7, 2));

    // Reconstituer les valeurs (Big Endian)
    TempInt := SmallInt((TempBytes[0] shl 8) or TempBytes[1]);
    HumInt := SmallInt((HumBytes[0] shl 8) or HumBytes[1]);

    // Convertir en valeurs réelles
    Temperature := TempInt / 10.0;
    Humidity := HumInt / 10.0;
  except
    on E: Exception do
      WriteLn('Erreur décodage payload: ', E.Message);
  end;
end;

function TLoRaWANHandler.FindDeviceByEUI(const DevEUI: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FDevices) do
  begin
    if FDevices[i].DevEUI = DevEUI then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TLoRaWANHandler.ProcessMessages;
begin
  // Boucle de traitement des messages
  while True do
  begin
    FMQTTClient.ProcessMessages(100);
    Sleep(10);
  end;
end;

procedure TLoRaWANHandler.SendDownlink(const DevEUI: string; const Payload: string);
var
  DownlinkJSON: TJSONObject;
  DownlinkTopic: string;
  PayloadEncoded: string;
begin
  // Construire le message downlink
  DownlinkJSON := TJSONObject.Create;
  try
    // Encoder le payload en base64 (selon le network server)
    PayloadEncoded := EncodeStringBase64(Payload);

    DownlinkJSON.Add('confirmed', False); // Message non confirmé
    DownlinkJSON.Add('fPort', 1);
    DownlinkJSON.Add('data', PayloadEncoded);

    // Topic pour downlink: application/[appID]/device/[devEUI]/tx
    DownlinkTopic := 'application/1/device/' + DevEUI + '/tx';

    // Publier le downlink
    FMQTTClient.Publish(DownlinkTopic, DownlinkJSON.AsJSON, 0, False);

    WriteLn('Downlink envoyé vers ', DevEUI, ' : ', Payload);
  finally
    DownlinkJSON.Free;
  end;
end;

end.
```

### 3.3 Programme principal LoRaWAN

```pascal
program LoRaWANServerApp;

{$mode objfpc}{$H+}

uses
  SysUtils, LoRaWANServer;

var
  Server: TLoRaWANHandler;

begin
  WriteLn('=== Serveur LoRaWAN ===');
  WriteLn;

  // Créer le serveur avec connexion au broker MQTT
  // Le network server LoRaWAN publie généralement sur MQTT
  Server := TLoRaWANHandler.Create('localhost', 1883);

  try
    // Connexion
    Server.Connect;

    // S'abonner aux messages de toutes les gateways
    Server.SubscribeToGateway('*');

    WriteLn('Serveur démarré. En attente de messages LoRaWAN...');
    WriteLn('Appuyez sur Ctrl+C pour arrêter');
    WriteLn;

    // Boucle de traitement
    Server.ProcessMessages;

  finally
    Server.Free;
  end;
end.
```

### 3.4 Encodage de données pour LoRaWAN

Étant donné le débit très limité de LoRaWAN (maximum quelques centaines d'octets par jour selon la région), l'encodage efficace des données est crucial.

```pascal
unit LoRaWANEncoder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TLoRaPayloadEncoder }
  TLoRaPayloadEncoder = class
  public
    class function EncodeTemperature(Temp: Double): string;
    class function EncodeGPS(Latitude, Longitude: Double): string;
    class function EncodeBatteryLevel(BatteryPercent: Byte): string;
    class function EncodeMultiSensor(Temp, Humidity: Double;
      Battery: Byte): string;
    class function DecodeMultiSensor(const HexPayload: string;
      out Temp, Humidity: Double; out Battery: Byte): Boolean;
  end;

implementation

{ TLoRaPayloadEncoder }

class function TLoRaPayloadEncoder.EncodeTemperature(Temp: Double): string;
var
  TempInt: SmallInt;
begin
  // Encoder la température sur 2 octets (résolution 0.1°C)
  // Plage: -3276.8°C à +3276.7°C
  TempInt := Round(Temp * 10);
  Result := IntToHex(Hi(TempInt), 2) + IntToHex(Lo(TempInt), 2);
end;

class function TLoRaPayloadEncoder.EncodeGPS(Latitude, Longitude: Double): string;
var
  LatInt, LonInt: LongInt;
begin
  // Encoder GPS sur 8 octets (4 pour lat, 4 pour lon)
  // Résolution: environ 1cm
  LatInt := Round(Latitude * 1000000);
  LonInt := Round(Longitude * 1000000);

  Result := IntToHex((LatInt shr 24) and $FF, 2) +
            IntToHex((LatInt shr 16) and $FF, 2) +
            IntToHex((LatInt shr 8) and $FF, 2) +
            IntToHex(LatInt and $FF, 2) +
            IntToHex((LonInt shr 24) and $FF, 2) +
            IntToHex((LonInt shr 16) and $FF, 2) +
            IntToHex((LonInt shr 8) and $FF, 2) +
            IntToHex(LonInt and $FF, 2);
end;

class function TLoRaPayloadEncoder.EncodeBatteryLevel(BatteryPercent: Byte): string;
begin
  // Encoder le niveau de batterie sur 1 octet (0-100%)
  if BatteryPercent > 100 then
    BatteryPercent := 100;
  Result := IntToHex(BatteryPercent, 2);
end;

class function TLoRaPayloadEncoder.EncodeMultiSensor(Temp, Humidity: Double;
  Battery: Byte): string;
var
  TempInt, HumInt: SmallInt;
begin
  // Format compact: 5 octets total
  // 2 octets: température (-327.6 à +327.7°C, résolution 0.1°C)
  // 2 octets: humidité (0 à 100%, résolution 0.01%)
  // 1 octet: batterie (0 à 100%)

  TempInt := Round(Temp * 10);
  HumInt := Round(Humidity * 100);

  Result := IntToHex(Hi(TempInt), 2) + IntToHex(Lo(TempInt), 2) +
            IntToHex(Hi(HumInt), 2) + IntToHex(Lo(HumInt), 2) +
            EncodeBatteryLevel(Battery);
end;

class function TLoRaPayloadEncoder.DecodeMultiSensor(const HexPayload: string;
  out Temp, Humidity: Double; out Battery: Byte): Boolean;
var
  TempBytes, HumBytes: array[0..1] of Byte;
  TempInt, HumInt: SmallInt;
begin
  Result := False;
  Temp := 0.0;
  Humidity := 0.0;
  Battery := 0;

  // Vérifier la longueur (5 octets = 10 caractères hex)
  if Length(HexPayload) < 10 then
    Exit;

  try
    // Température (octets 0-1)
    TempBytes[0] := StrToInt('$' + Copy(HexPayload, 1, 2));
    TempBytes[1] := StrToInt('$' + Copy(HexPayload, 3, 2));
    TempInt := SmallInt((TempBytes[0] shl 8) or TempBytes[1]);
    Temp := TempInt / 10.0;

    // Humidité (octets 2-3)
    HumBytes[0] := StrToInt('$' + Copy(HexPayload, 5, 2));
    HumBytes[1] := StrToInt('$' + Copy(HexPayload, 7, 2));
    HumInt := SmallInt((HumBytes[0] shl 8) or HumBytes[1]);
    Humidity := HumInt / 100.0;

    // Batterie (octet 4)
    Battery := StrToInt('$' + Copy(HexPayload, 9, 2));

    Result := True;
  except
    on E: Exception do
      WriteLn('Erreur décodage: ', E.Message);
  end;
end;

end.
```

**Exemple d'utilisation de l'encodeur :**

```pascal
program TestLoRaEncoder;

{$mode objfpc}{$H+}

uses
  SysUtils, LoRaWANEncoder;

var
  Payload: string;
  Temp, Hum: Double;
  Bat: Byte;

begin
  WriteLn('=== Test Encodeur LoRaWAN ===');
  WriteLn;

  // Encoder des données
  WriteLn('Encodage:');
  Temp := 23.5;
  Hum := 67.8;
  Bat := 85;

  Payload := TLoRaPayloadEncoder.EncodeMultiSensor(Temp, Hum, Bat);
  WriteLn('  Température: ', Temp:0:1, '°C');
  WriteLn('  Humidité: ', Hum:0:1, '%');
  WriteLn('  Batterie: ', Bat, '%');
  WriteLn('  Payload: ', Payload, ' (', Length(Payload) div 2, ' octets)');
  WriteLn;

  // Décoder
  WriteLn('Décodage:');
  if TLoRaPayloadEncoder.DecodeMultiSensor(Payload, Temp, Hum, Bat) then
  begin
    WriteLn('  Température: ', Temp:0:1, '°C');
    WriteLn('  Humidité: ', Hum:0:1, '%');
    WriteLn('  Batterie: ', Bat, '%');
  end
  else
    WriteLn('  Erreur de décodage');
end.
```

### 3.5 Intégration avec The Things Network (TTN)

The Things Network est un réseau LoRaWAN communautaire gratuit. Voici comment s'y connecter :

**Configuration Windows/Ubuntu :**

```pascal
unit TTNIntegration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mqtt, fphttpclient, opensslsockets, fpjson, jsonparser;

type
  { TTTNClient }
  TTTNClient = class
  private
    FMQTTClient: TMQTTClient;
    FAppID: string;
    FAccessKey: string;
    FRegion: string; // 'eu1', 'nam1', 'au1', etc.
    procedure OnConnect(Sender: TObject);
    procedure OnMessage(Sender: TObject; const Topic: string; const Payload: string);
  public
    constructor Create(const AppID, AccessKey, Region: string);
    destructor Destroy; override;
    procedure Connect;
    procedure SubscribeToDevices;
    procedure Run;
    procedure SendDownlink(const DeviceID: string; const Port: Byte;
      const PayloadHex: string);
  end;

implementation

{ TTTNClient }

constructor TTTNClient.Create(const AppID, AccessKey, Region: string);
begin
  FAppID := AppID;
  FAccessKey := AccessKey;
  FRegion := Region;

  // Configuration du client MQTT pour TTN
  FMQTTClient := TMQTTClient.Create;
  FMQTTClient.Host := Format('%s.cloud.thethings.network', [FRegion]);
  FMQTTClient.Port := 1883;
  FMQTTClient.Username := FAppID;
  FMQTTClient.Password := FAccessKey;
  FMQTTClient.ClientID := 'FreePascal_TTN_' + IntToStr(Random(10000));
  FMQTTClient.OnConnect := @OnConnect;
  FMQTTClient.OnMessage := @OnMessage;

  WriteLn('Client TTN configuré pour région: ', FRegion);
end;

destructor TTTNClient.Destroy;
begin
  FMQTTClient.Free;
  inherited Destroy;
end;

procedure TTTNClient.Connect;
begin
  WriteLn('Connexion à The Things Network...');
  FMQTTClient.Connect;
end;

procedure TTTNClient.OnConnect(Sender: TObject);
begin
  WriteLn('✓ Connecté à TTN');
  SubscribeToDevices;
end;

procedure TTTNClient.SubscribeToDevices;
begin
  // S'abonner aux uplinks de tous les périphériques
  FMQTTClient.Subscribe('v3/' + FAppID + '/devices/+/up', 0);
  WriteLn('✓ Abonné aux messages uplink');
end;

procedure TTTNClient.OnMessage(Sender: TObject; const Topic: string;
  const Payload: string);
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  DeviceID, PayloadHex: string;
  FPort: Integer;
  RXMetadata: TJSONArray;
begin
  WriteLn('─────────────────────────────────');
  WriteLn('Message TTN reçu');

  try
    JSONData := GetJSON(Payload);
    try
      if JSONData is TJSONObject then
      begin
        JSONObj := TJSONObject(JSONData);

        // Extraire les informations de base
        DeviceID := JSONObj.Get('end_device_ids', TJSONObject.Create).Get('device_id', '');
        WriteLn('Périphérique: ', DeviceID);

        // Informations uplink
        if JSONObj.Find('uplink_message') <> nil then
        begin
          with TJSONObject(JSONObj.Objects['uplink_message']) do
          begin
            FPort := Get('f_port', 0);
            PayloadHex := Get('frm_payload', '');

            WriteLn('Port: ', FPort);
            WriteLn('Payload: ', PayloadHex);

            // Métadonnées radio
            if Find('rx_metadata') <> nil then
            begin
              RXMetadata := Arrays['rx_metadata'];
              if RXMetadata.Count > 0 then
              begin
                with TJSONObject(RXMetadata[0]) do
                begin
                  WriteLn('Gateway: ', Get('gateway_ids', TJSONObject.Create).Get('gateway_id', ''));
                  WriteLn('RSSI: ', Get('rssi', 0), ' dBm');
                  WriteLn('SNR: ', Get('snr', 0.0):0:1, ' dB');
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur traitement message: ', E.Message);
  end;

  WriteLn;
end;

procedure TTTNClient.Run;
begin
  WriteLn('Écoute des messages... (Ctrl+C pour arrêter)');
  while True do
  begin
    FMQTTClient.ProcessMessages(100);
    Sleep(10);
  end;
end;

procedure TTTNClient.SendDownlink(const DeviceID: string; const Port: Byte;
  const PayloadHex: string);
var
  DownlinkJSON: TJSONObject;
  DownlinkMessage: TJSONObject;
  Topic: string;
begin
  // Construire le message downlink pour TTN v3
  DownlinkJSON := TJSONObject.Create;
  try
    DownlinkMessage := TJSONObject.Create;
    DownlinkMessage.Add('f_port', Port);
    DownlinkMessage.Add('frm_payload', PayloadHex);
    DownlinkMessage.Add('priority', 'NORMAL');

    DownlinkJSON.Add('downlinks', TJSONArray.Create);
    TJSONArray(DownlinkJSON.Arrays['downlinks']).Add(DownlinkMessage);

    // Topic downlink TTN v3
    Topic := Format('v3/%s/devices/%s/down/push', [FAppID, DeviceID]);

    FMQTTClient.Publish(Topic, DownlinkJSON.AsJSON, 0, False);

    WriteLn('✓ Downlink envoyé vers ', DeviceID);
  finally
    DownlinkJSON.Free;
  end;
end;

end.
```

**Programme d'exemple TTN :**

```pascal
program TTNExample;

{$mode objfpc}{$H+}

uses
  SysUtils, TTNIntegration;

var
  Client: TTTNClient;

begin
  WriteLn('=== Client The Things Network ===');
  WriteLn;

  // Remplacer par vos identifiants TTN
  // Obtenez-les sur https://console.cloud.thethings.network/
  Client := TTTNClient.Create(
    'mon-application',           // Application ID
    'NNSXS.XXXXXXXXXXXXXXXXX',   // Access Key (API Key)
    'eu1'                         // Région (eu1, nam1, au1, etc.)
  );

  try
    Client.Connect;
    Client.Run;
  finally
    Client.Free;
  end;
end.
```

### 3.6 Optimisation de la consommation LoRaWAN

Pour maximiser la durée de vie de la batterie sur un périphérique LoRaWAN :

**Bonnes pratiques :**

1. **Réduire la fréquence d'envoi**
   - Envoyer seulement quand nécessaire
   - Utiliser des seuils de déclenchement
   - Exemple : envoyer uniquement si la température varie de plus de 0.5°C

2. **Optimiser la taille des messages**
   - Utiliser l'encodage binaire compact
   - Éviter JSON (trop verbeux)
   - Regrouper plusieurs mesures

3. **Adapter le Data Rate**
   - DR0 = longue portée mais lent (250 bps) et consommateur
   - DR5 = courte portée mais rapide (5470 bps) et économe
   - Laisser l'ADR (Adaptive Data Rate) gérer automatiquement

4. **Utiliser la classe A**
   - Classe A = écoute après TX seulement
   - Consommation typique : <100 µA en veille
   - Durée de vie batterie : plusieurs années

**Exemple de calcul de durée de vie :**

```pascal
function CalculateBatteryLife(BatteryCapacity_mAh: Integer;
  SleepCurrent_uA: Double;
  TXCurrent_mA: Double;
  TXTime_ms: Integer;
  MessagesPerDay: Integer): Integer;
var
  DailySleepCharge_mAh: Double;
  DailyTXCharge_mAh: Double;
  TotalDailyCharge_mAh: Double;
  LifeDays: Integer;
begin
  // Charge de veille par jour (µA * 24h)
  DailySleepCharge_mAh := (SleepCurrent_uA / 1000.0) * 24.0;

  // Charge TX par jour (mA * temps en heures * nombre de messages)
  DailyTXCharge_mAh := TXCurrent_mA * (TXTime_ms / 3600000.0) * MessagesPerDay;

  // Charge totale quotidienne
  TotalDailyCharge_mAh := DailySleepCharge_mAh + DailyTXCharge_mAh;

  // Durée de vie en jours
  LifeDays := Round(BatteryCapacity_mAh / TotalDailyCharge_mAh);

  Result := LifeDays;
end;

// Exemple d'utilisation
var
  LifeDays: Integer;
begin
  LifeDays := CalculateBatteryLife(
    2000,    // Batterie 2000 mAh (2x AA)
    5,       // 5 µA en veille
    120,     // 120 mA pendant TX
    5000,    // 5 secondes de TX
    24       // 24 messages par jour (1 par heure)
  );

  WriteLn('Durée de vie estimée: ', LifeDays, ' jours (',
          LifeDays div 365, ' ans)');
  // Résultat typique: environ 3-5 ans
end.
```

## 4. Comparaison et choix du protocole

### 4.1 Tableau comparatif

| Critère | MQTT | CoAP | LoRaWAN |
|---------|------|------|---------|
| **Portée** | Illimitée (Internet) | Illimitée (Internet) | 2-45 km (radio) |
| **Débit** | Élevé (Mbps) | Élevé (Mbps) | Très faible (bps-kbps) |
| **Latence** | Faible (<100ms) | Très faible (<50ms) | Élevée (1-10s) |
| **Consommation** | Moyenne | Faible | Très faible |
| **Infrastructure** | WiFi/4G/Ethernet | WiFi/4G/Ethernet | Gateway LoRa |
| **Coût** | Abonnement data | Abonnement data | Gratuit/abonnement |
| **Fiabilité** | Élevée (TCP) | Moyenne (UDP) | Moyenne |
| **Sécurité** | TLS | DTLS | Chiffrement AES-128 |

### 4.2 Critères de sélection

**Choisir MQTT si :**
- Connexion Internet disponible (WiFi, Ethernet, 4G)
- Besoin de fiabilité et de messages en temps réel
- Données fréquentes (secondes/minutes)
- Architecture publish/subscribe nécessaire
- Exemples : Domotique, supervision industrielle, télémétrie véhicule

**Choisir CoAP si :**
- Appareils très contraints en ressources
- Besoin d'une architecture REST familière
- Messages ponctuels sans connexion permanente
- Économie d'énergie sur WiFi
- Exemples : Capteurs sans fil, actionneurs basse consommation

**Choisir LoRaWAN si :**
- Longue portée nécessaire (plusieurs kilomètres)
- Pas de connexion Internet disponible
- Très faible consommation critique (années sur batterie)
- Données peu fréquentes (heures/jours)
- Faible quantité de données (<50 bytes)
- Exemples : Agriculture, compteurs, parking, tracking d'animaux

### 4.3 Architecture hybride

Il est courant de combiner plusieurs protocoles selon les besoins :

```
[Capteur LoRa] ─radio─> [Gateway LoRa] ─┐
                                         ├─MQTT─> [Broker] ─> [Application Web]
[Capteur WiFi] ─────────────────────────┘        [Broker] ─> [Base de données]
                                                  [Broker] ─> [Alertes]
```

**Exemple d'architecture complète :**

```pascal
program UnifiedIoTPlatform;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  MQTTPublisher,      // Pour MQTT
  SimpleCOAP,         // Pour CoAP
  LoRaWANServer;      // Pour LoRaWAN

type
  { TIoTPlatform }
  TIoTPlatform = class
  private
    FMQTTBroker: TMQTTClient;
    FLoRaWANHandler: TLoRaWANHandler;
    FCOAPDevices: TStringList;
    procedure ProcessMQTTData(const Topic, Data: string);
    procedure ProcessLoRaData(const DevEUI, Data: string);
    procedure ProcessCOAPData(const DeviceID, Data: string);
    procedure StoreInDatabase(const DeviceID, DataType: string; Value: Double);
    procedure CheckAlerts(const DeviceID: string; Value: Double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function GetDeviceStatus(const DeviceID: string): string;
    function GetAllDevices: TStringList;
  end;

implementation

uses
  fpjson, jsonparser, sqldb, sqlite3conn;

var
  DBConnection: TSQLite3Connection;
  DBTransaction: TSQLTransaction;

{ TIoTPlatform }

constructor TIoTPlatform.Create;
begin
  WriteLn('=== Plateforme IoT Unifiée ===');
  WriteLn('Initialisation...');

  // Initialiser la connexion à la base de données
  DBConnection := TSQLite3Connection.Create(nil);
  DBConnection.DatabaseName := 'iot_platform.db';

  DBTransaction := TSQLTransaction.Create(nil);
  DBTransaction.Database := DBConnection;
  DBConnection.Transaction := DBTransaction;

  try
    DBConnection.Open;
    WriteLn('✓ Base de données connectée');
  except
    on E: Exception do
      WriteLn('✗ Erreur base de données: ', E.Message);
  end;

  // Initialiser le broker MQTT
  FMQTTBroker := TMQTTClient.Create;
  FMQTTBroker.Host := 'localhost';
  FMQTTBroker.Port := 1883;
  FMQTTBroker.ClientID := 'IoT_Platform_' + IntToStr(Random(10000));
  WriteLn('✓ Client MQTT initialisé');

  // Initialiser le gestionnaire LoRaWAN
  FLoRaWANHandler := TLoRaWANHandler.Create('localhost', 1883);
  WriteLn('✓ Gestionnaire LoRaWAN initialisé');

  // Initialiser la liste des périphériques CoAP
  FCOAPDevices := TStringList.Create;
  WriteLn('✓ Gestionnaire CoAP initialisé');

  WriteLn;
end;

destructor TIoTPlatform.Destroy;
begin
  WriteLn('Arrêt de la plateforme...');

  FMQTTBroker.Free;
  FLoRaWANHandler.Free;
  FCOAPDevices.Free;

  if DBConnection.Connected then
    DBConnection.Close;

  DBTransaction.Free;
  DBConnection.Free;

  WriteLn('Plateforme arrêtée');
  inherited Destroy;
end;

procedure TIoTPlatform.Start;
begin
  WriteLn('Démarrage de la plateforme...');
  WriteLn;

  // Connexion au broker MQTT
  try
    FMQTTBroker.Connect;
    FMQTTBroker.Subscribe('sensors/+/+', 1); // sensors/[type]/[id]
    WriteLn('✓ MQTT: Connecté et abonné aux capteurs');
  except
    on E: Exception do
      WriteLn('✗ MQTT: Erreur de connexion: ', E.Message);
  end;

  // Démarrage du gestionnaire LoRaWAN
  try
    FLoRaWANHandler.Connect;
    FLoRaWANHandler.SubscribeToGateway('*');
    WriteLn('✓ LoRaWAN: Connecté et en écoute');
  except
    on E: Exception do
      WriteLn('✗ LoRaWAN: Erreur de connexion: ', E.Message);
  end;

  // CoAP: mode pull (interrogation périodique)
  WriteLn('✓ CoAP: Mode pull activé');

  WriteLn;
  WriteLn('=== Plateforme opérationnelle ===');
  WriteLn('En attente de données...');
  WriteLn;

  // Boucle principale
  while True do
  begin
    // Traiter les messages MQTT
    FMQTTBroker.ProcessMessages(10);

    // Traiter les messages LoRaWAN (via MQTT aussi)
    // FLoRaWANHandler traite automatiquement

    // Interroger périodiquement les périphériques CoAP
    // (à implémenter selon vos besoins)

    Sleep(10);
  end;
end;

procedure TIoTPlatform.Stop;
begin
  if FMQTTBroker.Connected then
    FMQTTBroker.Disconnect;

  WriteLn('Plateforme arrêtée');
end;

procedure TIoTPlatform.ProcessMQTTData(const Topic, Data: string);
var
  Parts: TStringList;
  SensorType, SensorID: string;
  JSONData: TJSONData;
  Value: Double;
begin
  WriteLn('[MQTT] ', Topic);

  // Parser le topic: sensors/[type]/[id]
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '/';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Topic;

    if Parts.Count >= 3 then
    begin
      SensorType := Parts[1];
      SensorID := Parts[2];

      // Parser les données JSON
      try
        JSONData := GetJSON(Data);
        try
          if JSONData is TJSONObject then
          begin
            Value := TJSONObject(JSONData).Get('value', 0.0);

            WriteLn('  Type: ', SensorType);
            WriteLn('  ID: ', SensorID);
            WriteLn('  Valeur: ', Value:0:2);

            // Stocker en base de données
            StoreInDatabase(SensorID, SensorType, Value);

            // Vérifier les alertes
            CheckAlerts(SensorID, Value);
          end;
        finally
          JSONData.Free;
        end;
      except
        on E: Exception do
          WriteLn('  Erreur parsing JSON: ', E.Message);
      end;
    end;
  finally
    Parts.Free;
  end;

  WriteLn;
end;

procedure TIoTPlatform.ProcessLoRaData(const DevEUI, Data: string);
var
  Temperature, Humidity: Double;
  Battery: Byte;
begin
  WriteLn('[LoRaWAN] ', DevEUI);

  // Décoder les données (format dépend de votre encodage)
  if TLoRaPayloadEncoder.DecodeMultiSensor(Data, Temperature, Humidity, Battery) then
  begin
    WriteLn('  Température: ', Temperature:0:1, '°C');
    WriteLn('  Humidité: ', Humidity:0:1, '%');
    WriteLn('  Batterie: ', Battery, '%');

    // Stocker en base
    StoreInDatabase(DevEUI, 'temperature', Temperature);
    StoreInDatabase(DevEUI, 'humidity', Humidity);
    StoreInDatabase(DevEUI, 'battery', Battery);

    // Vérifier les alertes
    CheckAlerts(DevEUI, Temperature);

    // Alerte batterie faible
    if Battery < 20 then
      WriteLn('  ⚠️  ALERTE: Batterie faible sur ', DevEUI);
  end;

  WriteLn;
end;

procedure TIoTPlatform.ProcessCOAPData(const DeviceID, Data: string);
begin
  WriteLn('[CoAP] ', DeviceID);
  WriteLn('  Data: ', Data);
  // Implémentation selon vos besoins
  WriteLn;
end;

procedure TIoTPlatform.StoreInDatabase(const DeviceID, DataType: string;
  Value: Double);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := DBConnection;
    Query.Transaction := DBTransaction;

    // Créer la table si elle n'existe pas
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS sensor_data (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  device_id TEXT NOT NULL,' +
      '  data_type TEXT NOT NULL,' +
      '  value REAL NOT NULL,' +
      '  timestamp DATETIME DEFAULT CURRENT_TIMESTAMP' +
      ')';
    Query.ExecSQL;

    // Insérer les données
    Query.SQL.Text :=
      'INSERT INTO sensor_data (device_id, data_type, value) ' +
      'VALUES (:device_id, :data_type, :value)';
    Query.Params.ParamByName('device_id').AsString := DeviceID;
    Query.Params.ParamByName('data_type').AsString := DataType;
    Query.Params.ParamByName('value').AsFloat := Value;
    Query.ExecSQL;

    DBTransaction.Commit;
  except
    on E: Exception do
    begin
      WriteLn('Erreur base de données: ', E.Message);
      DBTransaction.Rollback;
    end;
  end;

  Query.Free;
end;

procedure TIoTPlatform.CheckAlerts(const DeviceID: string; Value: Double);
begin
  // Exemple d'alertes simples
  if Value > 30.0 then
    WriteLn('  🔥 ALERTE: Température élevée sur ', DeviceID, ' (', Value:0:1, '°C)')
  else if Value < 10.0 then
    WriteLn('  ❄️  ALERTE: Température basse sur ', DeviceID, ' (', Value:0:1, '°C)');

  // Ici, vous pourriez envoyer des emails, SMS, notifications push, etc.
end;

function TIoTPlatform.GetDeviceStatus(const DeviceID: string): string;
var
  Query: TSQLQuery;
  Status: TJSONObject;
begin
  Query := TSQLQuery.Create(nil);
  Status := TJSONObject.Create;

  try
    Query.Database := DBConnection;
    Query.Transaction := DBTransaction;

    // Récupérer les dernières données du périphérique
    Query.SQL.Text :=
      'SELECT data_type, value, timestamp FROM sensor_data ' +
      'WHERE device_id = :device_id ' +
      'ORDER BY timestamp DESC LIMIT 10';
    Query.Params.ParamByName('device_id').AsString := DeviceID;
    Query.Open;

    Status.Add('device_id', DeviceID);
    Status.Add('last_seen', Query.FieldByName('timestamp').AsString);

    while not Query.EOF do
    begin
      Status.Add(
        Query.FieldByName('data_type').AsString,
        Query.FieldByName('value').AsFloat
      );
      Query.Next;
    end;

    Result := Status.AsJSON;
  finally
    Query.Free;
    Status.Free;
  end;
end;

function TIoTPlatform.GetAllDevices: TStringList;
var
  Query: TSQLQuery;
begin
  Result := TStringList.Create;
  Query := TSQLQuery.Create(nil);

  try
    Query.Database := DBConnection;
    Query.Transaction := DBTransaction;

    Query.SQL.Text :=
      'SELECT DISTINCT device_id FROM sensor_data ' +
      'ORDER BY device_id';
    Query.Open;

    while not Query.EOF do
    begin
      Result.Add(Query.FieldByName('device_id').AsString);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

var
  Platform: TIoTPlatform;

begin
  Randomize;

  Platform := TIoTPlatform.Create;
  try
    Platform.Start;
  finally
    Platform.Free;
  end;
end.
```

## 5. Sécurité dans l'IoT

### 5.1 Principes de sécurité

La sécurité est cruciale dans l'IoT car les objets connectés peuvent être :
- Exposés physiquement (accessibles aux attaquants)
- Contraints en ressources (chiffrement complexe difficile)
- Nombreux et difficiles à mettre à jour

**Principes fondamentaux :**

1. **Authentification** : Vérifier l'identité des périphériques
2. **Chiffrement** : Protéger les données en transit et au repos
3. **Intégrité** : Garantir que les données n'ont pas été modifiées
4. **Mise à jour sécurisée** : OTA (Over-The-Air) avec vérification
5. **Principe du moindre privilège** : Accès minimal nécessaire

### 5.2 Sécurisation MQTT avec TLS

```pascal
unit SecureMQTT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mqtt, opensslsockets;

type
  { TSecureMQTTClient }
  TSecureMQTTClient = class
  private
    FMQTTClient: TMQTTClient;
    FUsername: string;
    FPassword: string;
    FCACertFile: string;
    FClientCertFile: string;
    FClientKeyFile: string;
  public
    constructor Create(const BrokerHost: string; BrokerPort: Integer);
    destructor Destroy; override;
    procedure SetCredentials(const Username, Password: string);
    procedure SetTLSCertificates(const CACert, ClientCert, ClientKey: string);
    function Connect: Boolean;
    procedure Publish(const Topic, Payload: string);
  end;

implementation

{ TSecureMQTTClient }

constructor TSecureMQTTClient.Create(const BrokerHost: string; BrokerPort: Integer);
begin
  FMQTTClient := TMQTTClient.Create;
  FMQTTClient.Host := BrokerHost;
  FMQTTClient.Port := BrokerPort; // Typiquement 8883 pour MQTT over TLS
  FMQTTClient.ClientID := 'Secure_' + IntToStr(Random(10000));

  // Activer TLS
  FMQTTClient.UseSSL := True;
end;

destructor TSecureMQTTClient.Destroy;
begin
  FMQTTClient.Free;
  inherited Destroy;
end;

procedure TSecureMQTTClient.SetCredentials(const Username, Password: string);
begin
  FUsername := Username;
  FPassword := Password;
  FMQTTClient.Username := Username;
  FMQTTClient.Password := Password;
end;

procedure TSecureMQTTClient.SetTLSCertificates(const CACert, ClientCert,
  ClientKey: string);
begin
  FCACertFile := CACert;
  FClientCertFile := ClientCert;
  FClientKeyFile := ClientKey;

  // Configuration TLS
  FMQTTClient.SSLCACertFile := CACert;
  FMQTTClient.SSLCertFile := ClientCert;
  FMQTTClient.SSLKeyFile := ClientKey;
  FMQTTClient.SSLVersion := sslvTLSv1_2; // Utiliser TLS 1.2 minimum
end;

function TSecureMQTTClient.Connect: Boolean;
begin
  try
    WriteLn('Connexion sécurisée à ', FMQTTClient.Host, ':', FMQTTClient.Port);
    FMQTTClient.Connect;
    WriteLn('✓ Connexion TLS établie');
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('✗ Erreur connexion: ', E.Message);
      Result := False;
    end;
  end;
end;

procedure TSecureMQTTClient.Publish(const Topic, Payload: string);
begin
  FMQTTClient.Publish(Topic, Payload, 1, False);
end;

end.
```

**Génération de certificats pour tests (OpenSSL) :**

**Sur Windows :**
```bash
# Installer OpenSSL
choco install openssl

# Générer une autorité de certification (CA)
openssl req -new -x509 -days 365 -extensions v3_ca ^
  -keyout ca.key -out ca.crt

# Générer un certificat client
openssl genrsa -out client.key 2048
openssl req -new -key client.key -out client.csr
openssl x509 -req -in client.csr -CA ca.crt -CAkey ca.key ^
  -CAcreateserial -out client.crt -days 365
```

**Sur Ubuntu :**
```bash
# OpenSSL est généralement préinstallé

# Générer une autorité de certification (CA)
openssl req -new -x509 -days 365 -extensions v3_ca \
  -keyout ca.key -out ca.crt

# Générer un certificat client
openssl genrsa -out client.key 2048
openssl req -new -key client.key -out client.csr
openssl x509 -req -in client.csr -CA ca.crt -CAkey ca.key \
  -CAcreateserial -out client.crt -days 365
```

### 5.3 Sécurité LoRaWAN

LoRaWAN intègre plusieurs niveaux de sécurité :

**Clés de sécurité :**
- **AppKey** : Clé d'application (128 bits), utilisée pour la procédure de join
- **NwkSKey** : Clé de session réseau, pour l'intégrité des messages
- **AppSKey** : Clé de session application, pour le chiffrement du payload

**Activation :**
- **OTAA** (Over-The-Air Activation) : Recommandé, clés de session changent à chaque join
- **ABP** (Activation By Personalization) : Clés fixes, moins sécurisé

```pascal
type
  { TLoRaWANSecurity }
  TLoRaWANSecurity = class
  public
    class function GenerateDevEUI: string;
    class function GenerateAppKey: string;
    class function EncryptPayload(const Payload: string; const AppSKey: string): string;
    class function DecryptPayload(const EncryptedPayload: string;
      const AppSKey: string): string;
    class function CalculateMIC(const Message: string; const NwkSKey: string): string;
  end;

implementation

uses
  DCPrijndael, DCPsha256, DCPbase64;

{ TLoRaWANSecurity }

class function TLoRaWANSecurity.GenerateDevEUI: string;
var
  i: Integer;
  B: Byte;
begin
  // Générer un DevEUI de 8 octets (16 caractères hex)
  Result := '';
  for i := 1 to 8 do
  begin
    B := Random(256);
    Result := Result + IntToHex(B, 2);
  end;
end;

class function TLoRaWANSecurity.GenerateAppKey: string;
var
  i: Integer;
  B: Byte;
begin
  // Générer une AppKey de 16 octets (32 caractères hex)
  Result := '';
  for i := 1 to 16 do
  begin
    B := Random(256);
    Result := Result + IntToHex(B, 2);
  end;
end;

class function TLoRaWANSecurity.EncryptPayload(const Payload: string;
  const AppSKey: string): string;
var
  Cipher: TDCP_rijndael;
  Key: array[0..15] of Byte;
  i: Integer;
begin
  // Convertir la clé hex en bytes
  for i := 0 to 15 do
    Key[i] := StrToInt('$' + Copy(AppSKey, i * 2 + 1, 2));

  // Chiffrer avec AES-128
  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(Key, SizeOf(Key) * 8, nil);
    Result := Cipher.EncryptString(Payload);
  finally
    Cipher.Free;
  end;
end;

class function TLoRaWANSecurity.DecryptPayload(const EncryptedPayload: string;
  const AppSKey: string): string;
var
  Cipher: TDCP_rijndael;
  Key: array[0..15] of Byte;
  i: Integer;
begin
  // Convertir la clé hex en bytes
  for i := 0 to 15 do
    Key[i] := StrToInt('$' + Copy(AppSKey, i * 2 + 1, 2));

  // Déchiffrer avec AES-128
  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(Key, SizeOf(Key) * 8, nil);
    Result := Cipher.DecryptString(EncryptedPayload);
  finally
    Cipher.Free;
  end;
end;

class function TLoRaWANSecurity.CalculateMIC(const Message: string;
  const NwkSKey: string): string;
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
  i: Integer;
begin
  // Calculer le MIC (Message Integrity Code) avec CMAC
  // Simplifié ici avec SHA-256 pour l'exemple
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Message + NwkSKey);
    Hash.Final(Digest);

    // Prendre les 4 premiers octets
    Result := '';
    for i := 0 to 3 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
  end;
end;

end.
```

## 6. Bonnes pratiques et recommandations

### 6.1 Développement multi-plateforme

**Pour assurer la portabilité Windows/Ubuntu :**

```pascal
{$IFDEF WINDOWS}
const
  MQTT_CA_CERT = 'C:\certs\ca.crt';
  MQTT_CLIENT_CERT = 'C:\certs\client.crt';
  MQTT_CLIENT_KEY = 'C:\certs\client.key';
{$ENDIF}

{$IFDEF LINUX}
const
  MQTT_CA_CERT = '/etc/ssl/certs/ca.crt';
  MQTT_CLIENT_CERT = '/etc/ssl/certs/client.crt';
  MQTT_CLIENT_KEY = '/etc/ssl/private/client.key';
{$ENDIF}
```

### 6.2 Gestion des erreurs

```pascal
procedure ConnectWithRetry(Client: TMQTTClient; MaxRetries: Integer);
var
  Retry: Integer;
begin
  Retry := 0;
  while Retry < MaxRetries do
  begin
    try
      Client.Connect;
      WriteLn('✓ Connecté');
      Break;
    except
      on E: Exception do
      begin
        Inc(Retry);
        WriteLn(Format('Tentative %d/%d échouée: %s',
                [Retry, MaxRetries, E.Message]));
        if Retry < MaxRetries then
          Sleep(5000); // Attendre 5 secondes avant de réessayer
      end;
    end;
  end;

  if Retry >= MaxRetries then
    raise Exception.Create('Impossible de se connecter après ' +
          IntToStr(MaxRetries) + ' tentatives');
end;
```

### 6.3 Logging et monitoring

```pascal
unit IoTLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  { TIoTLogger }
  TIoTLogger = class
  private
    FLogFile: TextFile;
    FFileName: string;
    FMinLevel: TLogLevel;
  public
    constructor Create(const FileName: string; MinLevel: TLogLevel = llInfo);
    destructor Destroy; override;
    procedure Log(Level: TLogLevel; const Message: string);
    procedure Debug(const Message: string);
    procedure Info(const Message: string);
    procedure Warning(const Message: string);
    procedure Error(const Message: string);
    procedure Critical(const Message: string);
  end;

implementation

{ TIoTLogger }

constructor TIoTLogger.Create(const FileName: string; MinLevel: TLogLevel);
begin
  FFileName := FileName;
  FMinLevel := MinLevel;
  AssignFile(FLogFile, FFileName);

  if FileExists(FFileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
end;

destructor TIoTLogger.Destroy;
begin
  CloseFile(FLogFile);
  inherited Destroy;
end;

procedure TIoTLogger.Log(Level: TLogLevel; const Message: string);
const
  LevelStr: array[TLogLevel] of string =
    ('DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL');
var
  LogLine: string;
begin
  if Level >= FMinLevel then
  begin
    LogLine := Format('[%s] [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), LevelStr[Level], Message]);

    WriteLn(FLogFile, LogLine);
    Flush(FLogFile);
    WriteLn(LogLine); // Afficher aussi en console
  end;
end;

procedure TIoTLogger.Debug(const Message: string);
begin
  Log(llDebug, Message);
end;

procedure TIoTLogger.Info(const Message: string);
begin
  Log(llInfo, Message);
end;

procedure TIoTLogger.Warning(const Message: string);
begin
  Log(llWarning, Message);
end;

procedure TIoTLogger.Error(const Message: string);
begin
  Log(llError, Message);
end;

procedure TIoTLogger.Critical(const Message: string);
begin
  Log(llCritical, Message);
end;

end.
```

### 6.4 Configuration par fichier

```pascal
unit IoTConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  { TIoTConfig }
  TIoTConfig = class
  private
    FIniFile: TIniFile;
  public
    constructor Create(const ConfigFile: string);
    destructor Destroy; override;
    function GetMQTTBroker: string;
    function GetMQTTPort: Integer;
    function GetMQTTUsername: string;
    function GetMQTTPassword: string;
    function GetDatabasePath: string;
    function GetLogLevel: Integer;
    procedure SaveConfig;
  end;

implementation

{ TIoTConfig }

constructor TIoTConfig.Create(const ConfigFile: string);
begin
  FIniFile := TIniFile.Create(ConfigFile);

  // Valeurs par défaut si le fichier n'existe pas
  if not FileExists(ConfigFile) then
  begin
    FIniFile.WriteString('MQTT', 'Broker', 'localhost');
    FIniFile.WriteInteger('MQTT', 'Port', 1883);
    FIniFile.WriteString('MQTT', 'Username', '');
    FIniFile.WriteString('MQTT', 'Password', '');
    FIniFile.WriteString('Database', 'Path', 'iot_platform.db');
    FIniFile.WriteInteger('Logging', 'Level', 1); // Info
    FIniFile.UpdateFile;
  end;
end;

destructor TIoTConfig.Destroy;
begin
  FIniFile.Free;
  inherited Destroy;
end;

function TIoTConfig.GetMQTTBroker: string;
begin
  Result := FIniFile.ReadString('MQTT', 'Broker', 'localhost');
end;

function TIoTConfig.GetMQTTPort: Integer;
begin
  Result := FIniFile.ReadInteger('MQTT', 'Port', 1883);
end;

function TIoTConfig.GetMQTTUsername: string;
begin
  Result := FIniFile.ReadString('MQTT', 'Username', '');
end;

function TIoTConfig.GetMQTTPassword: string;
begin
  Result := FIniFile.ReadString('MQTT', 'Password', '');
end;

function TIoTConfig.GetDatabasePath: string;
begin
  Result := FIniFile.ReadString('Database', 'Path', 'iot_platform.db');
end;

function TIoTConfig.GetLogLevel: Integer;
begin
  Result := FIniFile.ReadInteger('Logging', 'Level', 1);
end;

procedure TIoTConfig.SaveConfig;
begin
  FIniFile.UpdateFile;
end;

end.
```

**Exemple de fichier de configuration (iot_config.ini) :**

```ini
[MQTT]
Broker=localhost
Port=1883
Username=iot_user
Password=secure_password
UseTLS=false
CACert=
ClientCert=
ClientKey=

[LoRaWAN]
NetworkServer=localhost
NetworkPort=1883
ApplicationID=my_app
AccessKey=NNSXS.XXXXXXXXXX
Region=eu1

[CoAP]
DefaultPort=5683
UseDTLS=false

[Database]
Type=sqlite
Path=iot_platform.db
Host=
Port=
Username=
Password=

[Logging]
Level=1
; 0=Debug, 1=Info, 2=Warning, 3=Error, 4=Critical
LogFile=iot_platform.log
MaxSizeMB=100

[Alerts]
EmailEnabled=false
SMTPServer=
SMTPPort=587
EmailFrom=
EmailTo=
SMSEnabled=false
SMSProvider=
SMSAPIKey=

[General]
DeviceTimeout=300
; Timeout en secondes avant de considérer un périphérique comme déconnecté
DataRetentionDays=90
; Nombre de jours de conservation des données
```

### 6.5 Tests unitaires pour protocoles IoT

```pascal
unit IoTProtocolTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  MQTTPublisher, LoRaWANEncoder, SimpleCOAP;

type
  { TIoTProtocolTests }
  TIoTProtocolTests = class(TTestCase)
  published
    procedure TestMQTTConnection;
    procedure TestLoRaWANEncoding;
    procedure TestLoRaWANDecoding;
    procedure TestCOAPRequest;
    procedure TestPayloadCompression;
  end;

implementation

{ TIoTProtocolTests }

procedure TIoTProtocolTests.TestMQTTConnection;
var
  Client: TTemperatureSensor;
begin
  Client := TTemperatureSensor.Create('test.mosquitto.org', 1883);
  try
    AssertTrue('MQTT Connection should succeed', Client.Connect);
    AssertTrue('Should be connected', Client.Connected);
  finally
    Client.Free;
  end;
end;

procedure TIoTProtocolTests.TestLoRaWANEncoding;
var
  Payload: string;
  Temp, Humidity: Double;
  Battery: Byte;
begin
  Temp := 23.5;
  Humidity := 65.3;
  Battery := 87;

  Payload := TLoRaPayloadEncoder.EncodeMultiSensor(Temp, Humidity, Battery);

  // Vérifier que le payload a la bonne taille (5 octets = 10 caractères hex)
  AssertEquals('Payload should be 10 characters', 10, Length(Payload));
end;

procedure TIoTProtocolTests.TestLoRaWANDecoding;
var
  Payload: string;
  TempIn, HumIn, TempOut, HumOut: Double;
  BatIn, BatOut: Byte;
  Success: Boolean;
begin
  TempIn := 23.5;
  HumIn := 65.3;
  BatIn := 87;

  // Encoder
  Payload := TLoRaPayloadEncoder.EncodeMultiSensor(TempIn, HumIn, BatIn);

  // Décoder
  Success := TLoRaPayloadEncoder.DecodeMultiSensor(Payload, TempOut, HumOut, BatOut);

  AssertTrue('Decoding should succeed', Success);
  AssertEquals('Temperature should match', TempIn, TempOut, 0.1);
  AssertEquals('Humidity should match', HumIn, HumOut, 0.1);
  AssertEquals('Battery should match', BatIn, BatOut);
end;

procedure TIoTProtocolTests.TestCOAPRequest;
var
  Client: TCOAPClient;
  Response: string;
begin
  Client := TCOAPClient.Create('coap.me', 5683);
  try
    AssertTrue('CoAP GET should succeed', Client.GET('/hello', Response));
    AssertTrue('Response should not be empty', Length(Response) > 0);
  finally
    Client.Free;
  end;
end;

procedure TIoTProtocolTests.TestPayloadCompression;
var
  Original, Compressed: string;
begin
  Original := 'This is a test payload with repeated data data data';
  // Ici, vous implémenteriez votre logique de compression
  // Par exemple avec zlib

  // Test basique : la compression devrait réduire la taille
  // AssertTrue('Compressed should be smaller', Length(Compressed) < Length(Original));
end;

initialization
  RegisterTest(TIoTProtocolTests);

end.
```

**Lancer les tests :**

```pascal
program RunIoTTests;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, consoletestrunner,
  IoTProtocolTests;

type
  { TIoTTestRunner }
  TIoTTestRunner = class(TTestRunner)
  protected
    // Configuration du runner de tests
  end;

var
  Application: TIoTTestRunner;

begin
  Application := TIoTTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'Tests IoT Protocols';
  Application.Run;
  Application.Free;
end.
```

## 7. Déploiement et production

### 7.1 Service Windows

Créer un service Windows pour votre application IoT :

```pascal
unit IoTWindowsService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp;

type
  { TIoTService }
  TIoTService = class(TDaemon)
  private
    FPlatform: TIoTPlatform;
  public
    function Start: Boolean; override;
    function Stop: Boolean; override;
    function Install: Boolean; override;
    function UnInstall: Boolean; override;
  end;

implementation

{ TIoTService }

function TIoTService.Start: Boolean;
begin
  WriteLn('Démarrage du service IoT...');

  try
    FPlatform := TIoTPlatform.Create;
    // Lancer dans un thread séparé
    TThread.CreateAnonymousThread(procedure
    begin
      FPlatform.Start;
    end).Start;

    Result := True;
    WriteLn('Service IoT démarré');
  except
    on E: Exception do
    begin
      WriteLn('Erreur démarrage: ', E.Message);
      Result := False;
    end;
  end;
end;

function TIoTService.Stop: Boolean;
begin
  WriteLn('Arrêt du service IoT...');

  try
    if Assigned(FPlatform) then
    begin
      FPlatform.Stop;
      FPlatform.Free;
      FPlatform := nil;
    end;

    Result := True;
    WriteLn('Service IoT arrêté');
  except
    on E: Exception do
    begin
      WriteLn('Erreur arrêt: ', E.Message);
      Result := False;
    end;
  end;
end;

function TIoTService.Install: Boolean;
begin
  // Installation du service Windows
  Result := inherited Install;
end;

function TIoTService.UnInstall: Boolean;
begin
  // Désinstallation du service Windows
  Result := inherited UnInstall;
end;

end.
```

**Programme principal du service :**

```pascal
program IoTService;

{$mode objfpc}{$H+}

uses
  SysUtils, DaemonApp, IoTWindowsService;

type
  { TIoTDaemonApp }
  TIoTDaemonApp = class(TDaemonApplication)
  protected
    procedure DoRun; override;
  end;

procedure TIoTDaemonApp.DoRun;
var
  Service: TIoTService;
begin
  Service := TIoTService.Create(Self);
  Service.Definition.DisplayName := 'IoT Platform Service';
  Service.Definition.Description := 'Service de gestion de la plateforme IoT multi-protocoles';
  Service.Definition.StartType := stAuto;

  if HasOption('install') then
  begin
    Service.Install;
    WriteLn('Service installé avec succès');
    Terminate;
  end
  else if HasOption('uninstall') then
  begin
    Service.UnInstall;
    WriteLn('Service désinstallé avec succès');
    Terminate;
  end
  else
    inherited DoRun;
end;

var
  Application: TIoTDaemonApp;

begin
  Application := TIoTDaemonApp.Create(nil);
  Application.Title := 'IoT Platform Service';
  Application.Run;
  Application.Free;
end.
```

**Installation sur Windows :**

```batch
REM Compiler le service
lazbuild IoTService.lpi

REM Installer le service
IoTService.exe --install

REM Démarrer le service
net start "IoT Platform Service"

REM Arrêter le service
net stop "IoT Platform Service"

REM Désinstaller le service
IoTService.exe --uninstall
```

### 7.2 Service systemd sur Ubuntu

Créer un fichier service systemd (`/etc/systemd/system/iot-platform.service`) :

```ini
[Unit]
Description=IoT Platform Multi-Protocol Service
After=network.target mosquitto.service
Wants=mosquitto.service

[Service]
Type=simple
User=iot
Group=iot
WorkingDirectory=/opt/iot-platform
ExecStart=/opt/iot-platform/iot_platform
Restart=on-failure
RestartSec=5s

# Limites de ressources
MemoryLimit=512M
CPUQuota=50%

# Sécurité
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=/var/log/iot-platform /var/lib/iot-platform

# Logging
StandardOutput=journal
StandardError=journal
SyslogIdentifier=iot-platform

[Install]
WantedBy=multi-user.target
```

**Installation sur Ubuntu :**

```bash
# Créer l'utilisateur système
sudo useradd -r -s /bin/false iot

# Créer les répertoires
sudo mkdir -p /opt/iot-platform
sudo mkdir -p /var/log/iot-platform
sudo mkdir -p /var/lib/iot-platform

# Copier l'exécutable
sudo cp iot_platform /opt/iot-platform/
sudo chmod +x /opt/iot-platform/iot_platform

# Copier la configuration
sudo cp iot_config.ini /opt/iot-platform/

# Définir les permissions
sudo chown -R iot:iot /opt/iot-platform
sudo chown -R iot:iot /var/log/iot-platform
sudo chown -R iot:iot /var/lib/iot-platform

# Copier le fichier service
sudo cp iot-platform.service /etc/systemd/system/

# Recharger systemd
sudo systemctl daemon-reload

# Activer le service au démarrage
sudo systemctl enable iot-platform

# Démarrer le service
sudo systemctl start iot-platform

# Vérifier le statut
sudo systemctl status iot-platform

# Voir les logs
sudo journalctl -u iot-platform -f
```

### 7.3 Conteneurisation avec Docker

**Dockerfile pour la plateforme IoT :**

```dockerfile
# Multi-stage build pour optimiser la taille

# Stage 1: Build
FROM ubuntu:22.04 AS builder

# Installer les dépendances de compilation
RUN apt-get update && apt-get install -y \
    fpc \
    lazarus \
    libssl-dev \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# Copier les sources
WORKDIR /build
COPY . .

# Compiler l'application
RUN lazbuild --build-mode=Release iot_platform.lpi

# Stage 2: Runtime
FROM ubuntu:22.04

# Installer uniquement les dépendances runtime
RUN apt-get update && apt-get install -y \
    libssl3 \
    libsqlite3-0 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Créer l'utilisateur non-root
RUN useradd -r -s /bin/false iot

# Créer les répertoires
RUN mkdir -p /app /data /logs && \
    chown -R iot:iot /app /data /logs

# Copier l'exécutable depuis le stage de build
COPY --from=builder /build/iot_platform /app/
COPY --from=builder /build/iot_config.ini /app/

# Définir l'utilisateur
USER iot

# Volume pour la persistance des données
VOLUME ["/data", "/logs"]

# Exposer les ports (si nécessaire pour API REST)
EXPOSE 8080

# Répertoire de travail
WORKDIR /app

# Point d'entrée
ENTRYPOINT ["./iot_platform"]
```

**docker-compose.yml pour un environnement complet :**

```yaml
version: '3.8'

services:
  mosquitto:
    image: eclipse-mosquitto:2
    container_name: iot-mosquitto
    ports:
      - "1883:1883"
      - "9001:9001"
    volumes:
      - ./mosquitto/config:/mosquitto/config
      - ./mosquitto/data:/mosquitto/data
      - ./mosquitto/log:/mosquitto/log
    restart: unless-stopped

  iot-platform:
    build: .
    container_name: iot-platform
    depends_on:
      - mosquitto
    environment:
      - MQTT_BROKER=mosquitto
      - MQTT_PORT=1883
      - DB_PATH=/data/iot_platform.db
      - LOG_PATH=/logs/iot_platform.log
    volumes:
      - iot-data:/data
      - iot-logs:/logs
    restart: unless-stopped

  grafana:
    image: grafana/grafana:latest
    container_name: iot-grafana
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin
    volumes:
      - grafana-data:/var/lib/grafana
    restart: unless-stopped

volumes:
  iot-data:
  iot-logs:
  grafana-data:
```

**Commandes Docker :**

```bash
# Construire l'image
docker build -t iot-platform:latest .

# Lancer avec docker-compose
docker-compose up -d

# Voir les logs
docker-compose logs -f iot-platform

# Arrêter
docker-compose down

# Arrêter et supprimer les volumes
docker-compose down -v
```

## 8. Monitoring et visualisation

### 8.1 Exposition de métriques Prometheus

```pascal
unit PrometheusMetrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver;

type
  { TPrometheusExporter }
  TPrometheusExporter = class
  private
    FHTTPServer: TFPHTTPServer;
    FDeviceCount: Integer;
    FMessageCount: Int64;
    FErrorCount: Int64;
    procedure HandleMetricsRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(Port: Integer);
    destructor Destroy; override;
    procedure Start;
    procedure IncrementDeviceCount;
    procedure IncrementMessageCount;
    procedure IncrementErrorCount;
  end;

implementation

{ TPrometheusExporter }

constructor TPrometheusExporter.Create(Port: Integer);
begin
  FDeviceCount := 0;
  FMessageCount := 0;
  FErrorCount := 0;

  FHTTPServer := TFPHTTPServer.Create(nil);
  FHTTPServer.Port := Port;
  FHTTPServer.OnRequest := @HandleMetricsRequest;
end;

destructor TPrometheusExporter.Destroy;
begin
  FHTTPServer.Free;
  inherited Destroy;
end;

procedure TPrometheusExporter.Start;
begin
  FHTTPServer.Active := True;
  WriteLn('Metrics endpoint started on port ', FHTTPServer.Port);
  WriteLn('Access metrics at http://localhost:', FHTTPServer.Port, '/metrics');
end;

procedure TPrometheusExporter.HandleMetricsRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Metrics: string;
begin
  if ARequest.URI = '/metrics' then
  begin
    // Format Prometheus
    Metrics := '# HELP iot_devices_total Total number of IoT devices' + LineEnding;
    Metrics := Metrics + '# TYPE iot_devices_total gauge' + LineEnding;
    Metrics := Metrics + 'iot_devices_total ' + IntToStr(FDeviceCount) + LineEnding;
    Metrics := Metrics + LineEnding;

    Metrics := Metrics + '# HELP iot_messages_total Total number of messages received' + LineEnding;
    Metrics := Metrics + '# TYPE iot_messages_total counter' + LineEnding;
    Metrics := Metrics + 'iot_messages_total ' + IntToStr(FMessageCount) + LineEnding;
    Metrics := Metrics + LineEnding;

    Metrics := Metrics + '# HELP iot_errors_total Total number of errors' + LineEnding;
    Metrics := Metrics + '# TYPE iot_errors_total counter' + LineEnding;
    Metrics := Metrics + 'iot_errors_total ' + IntToStr(FErrorCount) + LineEnding;

    AResponse.Content := Metrics;
    AResponse.ContentType := 'text/plain; version=0.0.4';
    AResponse.Code := 200;
  end
  else
  begin
    AResponse.Content := 'Not Found';
    AResponse.Code := 404;
  end;
end;

procedure TPrometheusExporter.IncrementDeviceCount;
begin
  Inc(FDeviceCount);
end;

procedure TPrometheusExporter.IncrementMessageCount;
begin
  Inc(FMessageCount);
end;

procedure TPrometheusExporter.IncrementErrorCount;
begin
  Inc(FErrorCount);
end;

end.
```

### 8.2 Configuration Prometheus

**prometheus.yml :**

```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'iot-platform'
    static_configs:
      - targets: ['localhost:9090']
```

### 8.3 Dashboard Grafana

Créer un tableau de bord dans Grafana pour visualiser :
- Nombre de périphériques connectés
- Nombre de messages par protocole (MQTT, CoAP, LoRaWAN)
- Taux d'erreurs
- Graphiques de température/humidité en temps réel
- État de la batterie des périphériques LoRaWAN

## 9. Ressources et documentation

### 9.1 Bibliothèques FreePascal/Lazarus pour IoT

**MQTT :**
- `fpc-mqtt-client` : https://github.com/prof7bit/fpc-mqtt-client
- `mqtt-client-lazarus` : Via OPM (Online Package Manager)

**CoAP :**
- Implémenter avec `synapse` ou `lnet`
- Référence RFC 7252 : https://tools.ietf.org/html/rfc7252

**LoRaWAN :**
- Intégration via MQTT avec network servers
- The Things Network : https://www.thethingsnetwork.org/
- ChirpStack : https://www.chirpstack.io/

**Utilitaires :**
- `Synapse` : Bibliothèque réseau complète
- `Indy` : Composants réseau
- `DCPCrypt` : Cryptographie

### 9.2 Outils de développement

**Brokers MQTT :**
- **Mosquitto** (Open Source) : https://mosquitto.org/
- **EMQX** (Enterprise) : https://www.emqx.io/
- **HiveMQ** (Cloud) : https://www.hivemq.com/

**Serveurs LoRaWAN :**
- **The Things Network** (Communautaire, gratuit)
- **ChirpStack** (Open Source, self-hosted)
- **Actility ThingPark** (Commercial)

**Outils de test :**
- **MQTT Explorer** : Client MQTT graphique
- **Copper (Cu)** : Plugin Firefox pour CoAP
- **Wireshark** : Analyse de trafic réseau

### 9.3 Documentation officielle

- **MQTT** : https://mqtt.org/
- **CoAP** : https://coap.technology/
- **LoRaWAN** : https://lora-alliance.org/
- **FreePascal** : https://www.freepascal.org/
- **Lazarus** : https://www.lazarus-ide.org/

## 10. Conclusion

### 10.1 Récapitulatif

Dans ce tutoriel, nous avons exploré trois protocoles majeurs de l'IoT :

**MQTT** - Le protocole polyvalent
- ✅ Idéal pour la télémétrie temps réel
- ✅ Architecture publish/subscribe flexible
- ✅ Large support et écosystème mature
- ⚠️ Nécessite connexion Internet stable

**CoAP** - Le protocole léger
- ✅ Très économe en ressources
- ✅ Architecture REST familière
- ✅ Parfait pour appareils contraints
- ⚠️ Moins fiable (UDP)

**LoRaWAN** - Le protocole longue portée
- ✅ Portée exceptionnelle (plusieurs km)
- ✅ Consommation ultra-faible
- ✅ Parfait pour objets distants
- ⚠️ Débit très limité

### 10.2 Points clés à retenir

1. **Choisir le bon protocole** selon les contraintes (portée, débit, consommation)
2. **Sécuriser les communications** (TLS/DTLS, authentification, chiffrement)
3. **Optimiser les données** (encodage binaire compact pour LoRaWAN)
4. **Gérer les erreurs** (reconnexion automatique, retry avec backoff)
5. **Logger et monitorer** (Prometheus, Grafana, logs structurés)
6. **Tester rigoureusement** (tests unitaires, tests d'intégration)

### 10.3 Pour aller plus loin

**Sujets avancés à explorer :**
- Edge Computing et traitement local
- Intelligence artificielle sur capteurs (TinyML)
- Gestion de flottes de milliers de périphériques
- Protocoles émergents (Thread, Matter, Zigbee)
- Blockchain pour l'IoT (traçabilité, sécurité)
- Jumeau numérique (Digital Twin)
- Protocoles industriels (OPC UA, Modbus)

**Projets pratiques suggérés :**
1. **Station météo connectée** (MQTT + Dashboard web)
2. **Tracker GPS longue autonomie** (LoRaWAN)
3. **Domotique légère** (CoAP + contrôle local)
4. **Surveillance agricole** (Capteurs LoRa + prédictions IA)
5. **Parking intelligent** (Capteurs + optimisation en temps réel)

### 10.4 Compatibilité Windows/Ubuntu

Tous les exemples de ce tutoriel sont compatibles avec Windows et Ubuntu grâce à :
- FreePascal/Lazarus multiplateforme natif
- Bibliothèques portables (Synapse, Indy)
- Directives de compilation conditionnelle
- Abstraction des chemins et configurations

**Différences à gérer :**

```pascal
{$IFDEF WINDOWS}
  // Chemins Windows
  DefaultConfigPath := 'C:\ProgramData\IoTPlatform\config.ini';
  DefaultLogPath := 'C:\ProgramData\IoTPlatform\logs\';
{$ENDIF}

{$IFDEF LINUX}
  // Chemins Linux
  DefaultConfigPath := '/etc/iot-platform/config.ini';
  DefaultLogPath := '/var/log/iot-platform/';
{$ENDIF}
```

Vous disposez maintenant d'une base solide pour développer des solutions IoT professionnelles avec FreePascal/Lazarus, que ce soit sur Windows ou Ubuntu !

---

**Bon développement IoT ! 🚀**

⏭️ [Communication série](/14-systemes-embarques-iot/06-communication-serie.md)
