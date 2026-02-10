🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.11 Edge Computing

## Introduction

L'**Edge Computing** (informatique en périphérie) consiste à **traiter les données localement**, au plus près de leur source, plutôt que de tout envoyer vers le cloud. C'est une révolution pour l'IoT et les systèmes embarqués.

### Paradigme traditionnel (Cloud Computing)

```
[Capteur] → [Internet] → [Cloud] → [Traitement] → [Internet] → [Actionneur]
              ↑ Lent           ↑ Coûteux          ↑ Latence
```

**Problèmes** :
- Latence élevée (100-500 ms)
- Coûts de bande passante
- Dépendance à la connexion Internet
- Problèmes de confidentialité (données sensibles)
- Scalabilité limitée (millions d'appareils)

### Nouveau paradigme (Edge Computing)

```
[Capteur] → [Traitement Local] → [Actionneur]
              ↑ Rapide (<10ms)

    ↓ (optionnel)

[Cloud] ← [Agrégation/ML/Analyses]
```

**Avantages** :
- ✓ Réponse en temps réel
- ✓ Fonctionnement sans connexion
- ✓ Réduction des coûts réseau
- ✓ Confidentialité des données
- ✓ Scalabilité massive

## Cas d'usage de l'Edge Computing

### 1. Surveillance industrielle

```pascal
// Détection d'anomalie locale sur machine industrielle
type
  TMachineState = record
    Temperature: Real;
    Vibration: Real;
    Pression: Real;
    RPM: Word;
  end;

var
  CurrentState: TMachineState;
  BaselineState: TMachineState;

function DetecterAnomalie: Boolean;  
const
  TEMP_THRESHOLD = 10.0;    // °C écart max
  VIB_THRESHOLD = 5.0;      // Unités
  PRESS_THRESHOLD = 2.0;    // Bar
begin
  Result := False;

  // Détection LOCALE (pas besoin du cloud)
  if Abs(CurrentState.Temperature - BaselineState.Temperature) > TEMP_THRESHOLD then
  begin
    UART_SendString('ALARME: Température anormale'#13#10);
    ActiverAlarme();
    Result := True;
  end;

  if Abs(CurrentState.Vibration - BaselineState.Vibration) > VIB_THRESHOLD then
  begin
    UART_SendString('ALARME: Vibration excessive'#13#10);
    ArreterMachine();  // Action IMMEDIATE
    Result := True;
  end;

  // Envoyer au cloud seulement si anomalie
  if Result then
    EnvoyerAnomalieCloud(CurrentState);
end;

// Cycle principal
begin
  while True do
  begin
    LireCapteurs(CurrentState);

    if DetecterAnomalie() then
      LogEvenement()  // Log local
    else
      // Fonctionnement normal, pas de communication cloud
      rien;

    Sleep(100);  // 100ms = réactivité temps réel
  end;
end;
```

**Bénéfice** : Arrêt machine en < 100ms au lieu de plusieurs secondes si traitement cloud.

### 2. Vision par ordinateur embarquée

```pascal
// Reconnaissance faciale sur Raspberry Pi
program ReconnaissanceFaciale;

type
  TVisage = record
    ID: Word;
    Nom: String[20];
    Caracteristiques: array[0..127] of Byte;  // Feature vector
  end;

const
  MAX_VISAGES = 100;

var
  VisagesConnus: array[0..MAX_VISAGES-1] of TVisage;
  NombreVisages: Word;

function CapturerImage: TImage;  
begin
  // Capture caméra locale
  Camera_Capture(@Result);
end;

function ExtraireCaracteristiques(const Img: TImage): TFeatureVector;  
begin
  // Traitement LOCAL de l'image
  // Extraction features (algorithme léger)
  Result := DetecterVisage_Lightweight(Img);
end;

function Identifier(const Features: TFeatureVector): Integer;  
var
  i: Integer;
  Distance, MinDistance: Real;
  BestMatch: Integer;
begin
  MinDistance := 999999.0;
  BestMatch := -1;

  // Comparaison LOCALE avec base de données embarquée
  for i := 0 to NombreVisages - 1 do
  begin
    Distance := CalculerDistance(Features, VisagesConnus[i].Caracteristiques);

    if Distance < MinDistance then
    begin
      MinDistance := Distance;
      BestMatch := i;
    end;
  end;

  if MinDistance < SEUIL_RECONNAISSANCE then
    Result := BestMatch
  else
    Result := -1;  // Inconnu
end;

// Programme principal
var
  Image: TImage;
  Features: TFeatureVector;
  PersonID: Integer;
begin
  ChargerBaseDonneesLocale();  // Visages connus en local

  while True do
  begin
    Image := CapturerImage();
    Features := ExtraireCaracteristiques(Image);
    PersonID := Identifier(Features);

    if PersonID >= 0 then
    begin
      UART_SendString('Bienvenue ' + VisagesConnus[PersonID].Nom);
      OuvrirPorte();  // Action immédiate

      // Log vers cloud de façon asynchrone (optionnel)
      EnvoyerLogCloud(PersonID, Now);
    end
    else
    begin
      UART_SendString('Accès refusé');
      // Envoyer photo vers cloud pour analyse approfondie
      EnvoyerImageCloud(Image);  // En arrière-plan
    end;
  end;
end.
```

**Bénéfice** : Ouverture de porte en < 500ms. Pas de dépendance Internet.

### 3. Véhicule autonome

```pascal
// Prise de décision locale critique
type
  TObstacle = record
    Distance: Real;      // mètres
    Angle: Real;         // degrés
    Vitesse: Real;       // m/s
    Type_: Byte;         // 0=piéton, 1=véhicule, 2=objet
  end;

function DetecterObstacles: array of TObstacle;  
var
  Obstacles: array[0..15] of TObstacle;
  Count: Byte;
begin
  Count := 0;

  // LIDAR, caméra, radar - traitement LOCAL
  LirePointsLIDAR();
  AnalyserImageCamera();

  // Fusion de capteurs
  for i := 0 to NombrePoints - 1 do
  begin
    if EstUnObstacle(Points[i]) then
    begin
      Obstacles[Count] := ClassifierObstacle(Points[i]);
      Inc(Count);
    end;
  end;

  SetLength(Result, Count);
  Move(Obstacles, Result[0], Count * SizeOf(TObstacle));
end;

procedure PrendreDecision(const Obstacles: array of TObstacle);  
var
  i: Integer;
  Danger: Boolean;
begin
  Danger := False;

  for i := 0 to High(Obstacles) do
  begin
    // Détection collision imminente
    if (Obstacles[i].Distance < 5.0) and  // < 5 mètres
       (Obstacles[i].Angle < 10.0) then   // devant
    begin
      // RÉACTION IMMÉDIATE (< 10ms)
      FreinageUrgence();
      Danger := True;
      Break;
    end;
  end;

  if not Danger then
    AjusterTrajectoire(Obstacles);
end;

// Boucle 100 Hz (10ms)
begin
  while True do
  begin
    Obstacles := DetecterObstacles();  // 5ms
    PrendreDecision(Obstacles);        // 3ms
    ActualiserCommandes();             // 2ms

    // Envoyer télémétrie cloud (basse priorité, asynchrone)
    if (FrameCount mod 100) = 0 then  // 1 fois par seconde
      EnvoyerTelemetrie();

    Inc(FrameCount);
  end;
end.
```

**Bénéfice** : Réaction en 10ms au lieu de 200-500ms si attente cloud = vies sauvées.

## Architecture Edge Computing

### Modèle à trois niveaux

```
┌─────────────────────────────────────────┐
│         CLOUD (Niveau 3)                │
│  - Big Data Analytics                   │
│  - Machine Learning training            │
│  - Stockage long terme                  │
│  - Tableaux de bord globaux             │
└────────────────┬────────────────────────┘
                 │ Internet
                 │ (agrégation périodique)
┌────────────────┴────────────────────────┐
│       FOG/EDGE (Niveau 2)               │
│  - Gateway                              │
│  - Agrégation locale                    │
│  - Pré-traitement                       │
│  - ML inference                         │
│  - Routage intelligent                  │
└────────────────┬────────────────────────┘
                 │ Local Network
                 │ (temps réel)
┌────────────────┴────────────────────────┐
│      EDGE DEVICES (Niveau 1)            │
│  - Capteurs                             │
│  - Actionneurs                          │
│  - Traitement minimal                   │
│  - Acquisition données                  │
└─────────────────────────────────────────┘
```

### Exemple d'implémentation

#### Niveau 1 : Edge Device (Capteur ESP32)

```pascal
program EdgeDevice;

{$MODE OBJFPC}

uses
  ESP32_WiFi, ESP32_ADC;

const
  GATEWAY_IP = '192.168.1.100';
  GATEWAY_PORT = 8080;

  SAMPLE_INTERVAL = 1000;  // 1 seconde

type
  TSensorData = packed record
    DeviceID: Word;
    Timestamp: LongWord;
    Temperature: SmallInt;  // x10 (25.7°C = 257)
    Humidity: Byte;         // 0-100%
    Battery: Byte;          // 0-100%
  end;

var
  Data: TSensorData;
  LocalThreshold: SmallInt = 300;  // 30°C

function LireTemperature: SmallInt;  
var
  ADCValue: Word;
begin
  ADCValue := ADC_Read(TEMP_CHANNEL);
  Result := ((ADCValue * 33) div 4095) * 10;  // Conversion
end;

procedure TraiterLocalement;  
begin
  // Décision EDGE : alarme locale si > seuil
  if Data.Temperature > LocalThreshold then
  begin
    GPIO_Set(ALARM_LED);
    Beep(1000, 100);  // Alarme sonore locale
  end
  else
    GPIO_Clear(ALARM_LED);
end;

procedure EnvoyerGateway;  
var
  Socket: Integer;
begin
  Socket := WiFi_Connect(GATEWAY_IP, GATEWAY_PORT);
  if Socket >= 0 then
  begin
    WiFi_Send(Socket, @Data, SizeOf(Data));
    WiFi_Close(Socket);
  end;
  // Sinon : stockage local pour renvoyer plus tard
end;

begin
  WiFi_Init('MonReseau', 'MotDePasse');
  Data.DeviceID := 1;

  while True do
  begin
    // Acquisition
    Data.Timestamp := GetTickCount() div 1000;
    Data.Temperature := LireTemperature();
    Data.Humidity := LireHumidite();
    Data.Battery := LireBatterie();

    // Traitement local IMMÉDIAT
    TraiterLocalement();

    // Envoi vers gateway (non-bloquant si possible)
    EnvoyerGateway();

    Sleep(SAMPLE_INTERVAL);
  end;
end.
```

#### Niveau 2 : Edge Gateway (Raspberry Pi)

```pascal
program EdgeGateway;

{$MODE OBJFPC}

uses
  Sockets, SQLite3, cThreads;

type
  TSensorData = packed record
    DeviceID: Word;
    Timestamp: LongWord;
    Temperature: SmallInt;
    Humidity: Byte;
    Battery: Byte;
  end;

  TAggregatedData = record
    HourTimestamp: LongWord;
    AvgTemperature: Real;
    MinTemperature: SmallInt;
    MaxTemperature: SmallInt;
    SampleCount: Word;
  end;

var
  LocalDB: PSQLite3;
  CloudQueue: TThreadList;

procedure InitDatabase;  
const
  SQL_CREATE =
    'CREATE TABLE IF NOT EXISTS sensor_data (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  device_id INTEGER,' +
    '  timestamp INTEGER,' +
    '  temperature REAL,' +
    '  humidity INTEGER,' +
    '  battery INTEGER' +
    ')';
begin
  sqlite3_open('edge_data.db', @LocalDB);
  sqlite3_exec(LocalDB, PChar(SQL_CREATE), nil, nil, nil);
end;

procedure StockerLocalement(const Data: TSensorData);  
var
  SQL: string;
begin
  SQL := Format(
    'INSERT INTO sensor_data (device_id, timestamp, temperature, humidity, battery) ' +
    'VALUES (%d, %d, %.1f, %d, %d)',
    [Data.DeviceID, Data.Timestamp, Data.Temperature / 10.0,
     Data.Humidity, Data.Battery]
  );

  sqlite3_exec(LocalDB, PChar(SQL), nil, nil, nil);
end;

function AnalyserAnomalie(const Data: TSensorData): Boolean;  
var
  SQL: string;
  Stmt: PSQLite3_Stmt;
  AvgTemp: Real;
begin
  // Calculer moyenne des 100 dernières mesures
  SQL := Format(
    'SELECT AVG(temperature) FROM sensor_data ' +
    'WHERE device_id = %d ' +
    'ORDER BY id DESC LIMIT 100',
    [Data.DeviceID]
  );

  sqlite3_prepare_v2(LocalDB, PChar(SQL), -1, @Stmt, nil);

  if sqlite3_step(Stmt) = SQLITE_ROW then
    AvgTemp := sqlite3_column_double(Stmt, 0)
  else
    AvgTemp := 25.0;  // Défaut

  sqlite3_finalize(Stmt);

  // Anomalie si écart > 5°C de la moyenne
  Result := Abs((Data.Temperature / 10.0) - AvgTemp) > 5.0;
end;

function AgregerDonnees: TAggregatedData;  
var
  SQL: string;
  Stmt: PSQLite3_Stmt;
begin
  // Agréger dernière heure
  SQL :=
    'SELECT ' +
    '  AVG(temperature), ' +
    '  MIN(temperature), ' +
    '  MAX(temperature), ' +
    '  COUNT(*) ' +
    'FROM sensor_data ' +
    'WHERE timestamp > ' + IntToStr(GetUnixTime() - 3600);

  sqlite3_prepare_v2(LocalDB, PChar(SQL), -1, @Stmt, nil);

  if sqlite3_step(Stmt) = SQLITE_ROW then
  begin
    Result.HourTimestamp := GetUnixTime();
    Result.AvgTemperature := sqlite3_column_double(Stmt, 0);
    Result.MinTemperature := Round(sqlite3_column_double(Stmt, 1) * 10);
    Result.MaxTemperature := Round(sqlite3_column_double(Stmt, 2) * 10);
    Result.SampleCount := sqlite3_column_int(Stmt, 3);
  end;

  sqlite3_finalize(Stmt);
end;

procedure EnvoyerCloud(const Aggregated: TAggregatedData);  
var
  JSON: string;
begin
  // Envoyer seulement données agrégées au cloud
  JSON := Format(
    '{"timestamp":%d,"avg_temp":%.2f,"min_temp":%.1f,"max_temp":%.1f,"samples":%d}',
    [Aggregated.HourTimestamp, Aggregated.AvgTemperature,
     Aggregated.MinTemperature / 10.0, Aggregated.MaxTemperature / 10.0,
     Aggregated.SampleCount]
  );

  HTTP_POST('https://api.moncloud.com/data', JSON);
end;

// Thread serveur pour recevoir des edge devices
procedure ThreadServeur;  
var
  ServerSocket, ClientSocket: Integer;
  Data: TSensorData;
  BytesRead: Integer;
  Aggregated: TAggregatedData;
begin
  ServerSocket := Socket_Listen(8080);

  while True do
  begin
    ClientSocket := Socket_Accept(ServerSocket);

    BytesRead := Socket_Receive(ClientSocket, @Data, SizeOf(Data));

    if BytesRead = SizeOf(Data) then
    begin
      // Stockage local
      StockerLocalement(Data);

      // Analyse locale
      if AnalyserAnomalie(Data) then
      begin
        WriteLn('Anomalie détectée sur device ', Data.DeviceID);
        // Notification locale immédiate
        EnvoyerNotificationLocale(Data);
      end;
    end;

    Socket_Close(ClientSocket);
  end;
end;

// Thread agrégation périodique
procedure ThreadAgregation;  
var
  Aggregated: TAggregatedData;
begin
  while True do
  begin
    Sleep(3600000);  // 1 heure

    Aggregated := AgregerDonnees();
    EnvoyerCloud(Aggregated);

    // Nettoyer données anciennes (> 7 jours)
    sqlite3_exec(LocalDB,
      PChar('DELETE FROM sensor_data WHERE timestamp < ' +
            IntToStr(GetUnixTime() - 604800)),
      nil, nil, nil);
  end;
end;

begin
  InitDatabase();

  BeginThread(@ThreadServeur);
  BeginThread(@ThreadAgregation);

  WriteLn('Edge Gateway démarré sur port 8080');

  // Boucle principale
  while True do
    Sleep(1000);
end.
```

**Résultat de cette architecture** :
- Edge devices réagissent en < 100ms
- Gateway agrège et analyse localement
- Cloud reçoit seulement données agrégées (réduction 99% trafic)
- Fonctionne même si connexion Internet coupée

## Techniques Edge Computing avec FreePascal

### 1. Machine Learning embarqué

#### Modèle pré-entraîné simple

```pascal
// Réseau de neurones simple pour classification
type
  TNeuralNetwork = record
    Weights1: array[0..3, 0..9] of Real;  // Couche 1: 4 inputs → 10 neurones
    Bias1: array[0..9] of Real;
    Weights2: array[0..9, 0..2] of Real;  // Couche 2: 10 → 3 classes
    Bias2: array[0..2] of Real;
  end;

var
  Model: TNeuralNetwork;

procedure ChargerModele;  
begin
  // Charger poids pré-entraînés depuis EEPROM/Flash
  EEPROM_ReadBlock(0, @Model, SizeOf(Model));
end;

function Sigmoid(x: Real): Real;  
begin
  Result := 1.0 / (1.0 + Exp(-x));
end;

function Predire(const Inputs: array of Real): Byte;  
var
  Hidden: array[0..9] of Real;
  Output: array[0..2] of Real;
  i, j: Integer;
  Sum, MaxVal: Real;
  MaxIdx: Byte;
begin
  // Couche cachée
  for i := 0 to 9 do
  begin
    Sum := Model.Bias1[i];
    for j := 0 to 3 do
      Sum := Sum + Inputs[j] * Model.Weights1[j, i];
    Hidden[i] := Sigmoid(Sum);
  end;

  // Couche sortie
  for i := 0 to 2 do
  begin
    Sum := Model.Bias2[i];
    for j := 0 to 9 do
      Sum := Sum + Hidden[j] * Model.Weights2[j, i];
    Output[i] := Sigmoid(Sum);
  end;

  // Trouver classe avec probabilité max
  MaxVal := Output[0];
  MaxIdx := 0;
  for i := 1 to 2 do
  begin
    if Output[i] > MaxVal then
    begin
      MaxVal := Output[i];
      MaxIdx := i;
    end;
  end;

  Result := MaxIdx;
end;

// Utilisation
var
  SensorInputs: array[0..3] of Real;
  Classe: Byte;
begin
  ChargerModele();

  // Lire capteurs
  SensorInputs[0] := LireTemperature() / 100.0;  // Normaliser
  SensorInputs[1] := LireHumidite() / 100.0;
  SensorInputs[2] := LirePression() / 1000.0;
  SensorInputs[3] := LireLuminosite() / 255.0;

  // Inférence LOCALE
  Classe := Predire(SensorInputs);

  case Classe of
    0: UART_SendString('Normal');
    1: UART_SendString('Attention');
    2: UART_SendString('Danger');
  end;
end;
```

### 2. Filtrage et traitement signal

```pascal
// Filtre Kalman simple pour fusion de capteurs
type
  TKalmanFilter = record
    x: Real;      // État estimé
    P: Real;      // Erreur estimée
    Q: Real;      // Bruit processus
    R: Real;      // Bruit mesure
  end;

procedure InitKalman(var KF: TKalmanFilter; Q, R: Real);  
begin
  KF.x := 0.0;
  KF.P := 1.0;
  KF.Q := Q;
  KF.R := R;
end;

function KalmanUpdate(var KF: TKalmanFilter; Measurement: Real): Real;  
var
  K: Real;  // Gain de Kalman
begin
  // Prédiction
  KF.P := KF.P + KF.Q;

  // Mise à jour
  K := KF.P / (KF.P + KF.R);
  KF.x := KF.x + K * (Measurement - KF.x);
  KF.P := (1.0 - K) * KF.P;

  Result := KF.x;
end;

// Utilisation : fusion accéléromètre + gyroscope
var
  FilterAngle: TKalmanFilter;
  AccelAngle, GyroAngle, FusedAngle: Real;

begin
  InitKalman(FilterAngle, 0.001, 0.1);

  while True do
  begin
    // Lire capteurs
    AccelAngle := LireAccelerometre();
    GyroAngle := GyroAngle + LireGyroscope() * DeltaTime;

    // Fusion locale avec Kalman
    FusedAngle := KalmanUpdate(FilterAngle, AccelAngle);

    // Utiliser angle fusionné
    AjusterStabilisation(FusedAngle);

    Sleep(10);
  end;
end;
```

### 3. Agrégation et compression de données

```pascal
// Agrégation statistique avant envoi
type
  TDataAggregator = record
    Count: Word;
    Sum: LongWord;
    Min: Word;
    Max: Word;
    SumSquares: QWord;  // Pour variance
  end;

procedure ResetAggregator(var Agg: TDataAggregator);  
begin
  Agg.Count := 0;
  Agg.Sum := 0;
  Agg.Min := 65535;
  Agg.Max := 0;
  Agg.SumSquares := 0;
end;

procedure AddValue(var Agg: TDataAggregator; Value: Word);  
begin
  Inc(Agg.Count);
  Inc(Agg.Sum, Value);
  Inc(Agg.SumSquares, QWord(Value) * Value);

  if Value < Agg.Min then Agg.Min := Value;
  if Value > Agg.Max then Agg.Max := Value;
end;

function GetAverage(const Agg: TDataAggregator): Word;  
begin
  if Agg.Count > 0 then
    Result := Agg.Sum div Agg.Count
  else
    Result := 0;
end;

function GetStdDev(const Agg: TDataAggregator): Real;  
var
  Mean, Variance: Real;
begin
  if Agg.Count > 0 then
  begin
    Mean := Agg.Sum / Agg.Count;
    Variance := (Agg.SumSquares / Agg.Count) - (Mean * Mean);
    Result := Sqrt(Variance);
  end
  else
    Result := 0.0;
end;

// Utilisation
var
  TempAggregator: TDataAggregator;
  Temp: Word;
  Minute: Word;

begin
  ResetAggregator(TempAggregator);
  Minute := 0;

  while True do
  begin
    // Lire température toutes les secondes
    Temp := LireTemperature();
    AddValue(TempAggregator, Temp);

    Inc(Minute);
    if Minute >= 60 then  // Toutes les minutes
    begin
      // Envoyer statistiques au lieu de 60 valeurs
      EnvoyerCloud(
        GetAverage(TempAggregator),
        TempAggregator.Min,
        TempAggregator.Max,
        Round(GetStdDev(TempAggregator))
      );

      ResetAggregator(TempAggregator);
      Minute := 0;
    end;

    Sleep(1000);
  end;
end;

// Résultat : 60 mesures → 4 valeurs (réduction 93% trafic)
```

### 4. Mise en cache et prédiction

```pascal
// Cache local pour réduire requêtes cloud
type
  TCacheEntry = record
    Key: String[32];
    Value: String[128];
    Timestamp: LongWord;
    TTL: LongWord;  // Time To Live (secondes)
  end;

const
  CACHE_SIZE = 32;

var
  Cache: array[0..CACHE_SIZE-1] of TCacheEntry;

function HashKey(const Key: string): Byte;  
var
  i: Integer;
  Hash: Byte;
begin
  Hash := 0;
  for i := 1 to Length(Key) do
    Hash := Hash xor Byte(Key[i]);
  Result := Hash mod CACHE_SIZE;
end;

function GetFromCache(const Key: string): string;  
var
  Index: Byte;
  Now: LongWord;
begin
  Index := HashKey(Key);
  Now := GetTickCount() div 1000;

  if (Cache[Index].Key = Key) and
     ((Now - Cache[Index].Timestamp) < Cache[Index].TTL) then
  begin
    Result := Cache[Index].Value;  // HIT !
  end
  else
  begin
    Result := '';  // MISS
  end;
end;

procedure PutInCache(const Key, Value: string; TTL: LongWord);  
var
  Index: Byte;
begin
  Index := HashKey(Key);

  Cache[Index].Key := Key;
  Cache[Index].Value := Value;
  Cache[Index].Timestamp := GetTickCount() div 1000;
  Cache[Index].TTL := TTL;
end;

function GetWeatherData(City: string): string;  
var
  Cached: string;
begin
  // Chercher dans cache local
  Cached := GetFromCache(City);

  if Cached <> '' then
  begin
    WriteLn('Cache HIT');
    Result := Cached;
  end
  else
  begin
    WriteLn('Cache MISS - requête cloud');
    Result := HTTP_GET('https://api.weather.com/' + City);

    // Mettre en cache pour 30 minutes
    PutInCache(City, Result, 1800);
  end;
end;

// Réduction drastique des appels API cloud
```

## Gestion de la connectivité

### Mode déconnecté (offline-first)

```pascal
type
  TQueuedMessage = record
    Timestamp: LongWord;
    Data: array[0..255] of Byte;
    Size: Word;
  end;

const
  QUEUE_SIZE = 100;

var
  MessageQueue: array[0..QUEUE_SIZE-1] of TQueuedMessage;
  QueueHead, QueueTail: Byte;
  IsConnected: Boolean;

procedure EnqueueMessage(const Data; Size: Word);  
var
  NextHead: Byte;
begin
  NextHead := (QueueHead + 1) mod QUEUE_SIZE;

  if NextHead <> QueueTail then  // Queue pas pleine
  begin
    MessageQueue[QueueHead].Timestamp := GetTickCount();
    Move(Data, MessageQueue[QueueHead].Data, Size);
    MessageQueue[QueueHead].Size := Size;
    QueueHead := NextHead;
  end
  else
  begin
    WriteLn('Erreur: Queue pleine, message perdu');
    // Ou : écraser message le plus ancien
  end;
end;

function DequeueMessage(var Data; var Size: Word): Boolean;  
begin
  if QueueHead <> QueueTail then  // Queue pas vide
  begin
    Move(MessageQueue[QueueTail].Data, Data, MessageQueue[QueueTail].Size);
    Size := MessageQueue[QueueTail].Size;
    QueueTail := (QueueTail + 1) mod QUEUE_SIZE;
    Result := True;
  end
  else
    Result := False;
end;

procedure TenterEnvoiCloud(const Data; Size: Word);  
begin
  if IsConnected then
  begin
    // Connexion disponible, envoyer immédiatement
    if HTTP_POST_Success('https://api.cloud.com/data', @Data, Size) then
      WriteLn('Envoyé avec succès')
    else
    begin
      WriteLn('Échec envoi, mise en queue');
      EnqueueMessage(Data, Size);
      IsConnected := False;  // Marquer comme déconnecté
    end;
  end
  else
  begin
    WriteLn('Mode déconnecté, mise en queue');
    EnqueueMessage(Data, Size);
  end;
end;

// Thread de synchronisation
procedure ThreadSynchronisation;  
var
  Data: array[0..255] of Byte;
  Size: Word;
begin
  while True do
  begin
    Sleep(5000);  // Vérifier toutes les 5 secondes

    // Tester connectivité
    IsConnected := TestConnectivite();

    if IsConnected then
    begin
      // Vider la queue
      while DequeueMessage(Data, Size) do
      begin
        if HTTP_POST_Success('https://api.cloud.com/data', @Data, Size) then
          WriteLn('Message en queue envoyé')
        else
        begin
          WriteLn('Échec, remise en queue');
          EnqueueMessage(Data, Size);
          Break;  // Arrêter et réessayer plus tard
        end;

        Sleep(100);  // Éviter de saturer
      end;
    end;
  end;
end;

// Utilisation
var
  SensorData: TSensorData;
begin
  BeginThread(@ThreadSynchronisation);

  while True do
  begin
    LireCapteurs(SensorData);

    // TOUJOURS fonctionnel, connecté ou non
    TenterEnvoiCloud(SensorData, SizeOf(SensorData));

    Sleep(1000);
  end;
end;
```

### Synchronisation intelligente

```pascal
// Prioriser les données importantes
type
  TPriority = (prLow, prMedium, prHigh, prCritical);

  TPrioritizedMessage = record
    Priority: TPriority;
    Timestamp: LongWord;
    Data: array[0..255] of Byte;
    Size: Word;
  end;

var
  PriorityQueues: array[TPriority] of array[0..31] of TPrioritizedMessage;
  QueueHeads, QueueTails: array[TPriority] of Byte;

procedure EnqueuePriority(const Data; Size: Word; Priority: TPriority);  
var
  NextHead: Byte;
begin
  NextHead := (QueueHeads[Priority] + 1) mod 32;

  if NextHead <> QueueTails[Priority] then
  begin
    with PriorityQueues[Priority][QueueHeads[Priority]] do
    begin
      Self.Priority := Priority;
      Timestamp := GetTickCount();
      Move(Data, Self.Data, Size);
      Self.Size := Size;
    end;
    QueueHeads[Priority] := NextHead;
  end
  else
  begin
    // Queue pleine pour cette priorité
    if Priority = prCritical then
    begin
      // Messages critiques : écraser le plus ancien
      QueueTails[Priority] := (QueueTails[Priority] + 1) mod 32;
      // Et ajouter le nouveau
      EnqueuePriority(Data, Size, Priority);
    end;
    // Sinon ignorer les messages de basse priorité
  end;
end;

function DequeuePriority(var Data; var Size: Word): Boolean;  
var
  p: TPriority;
begin
  // Traiter par ordre de priorité
  for p := prCritical downto prLow do
  begin
    if QueueHeads[p] <> QueueTails[p] then
    begin
      with PriorityQueues[p][QueueTails[p]] do
      begin
        Move(Self.Data, Data, Self.Size);
        Size := Self.Size;
      end;
      QueueTails[p] := (QueueTails[p] + 1) mod 32;
      Exit(True);
    end;
  end;

  Result := False;
end;

// Utilisation
begin
  // Données normales
  EnqueuePriority(TempData, SizeOf(TempData), prLow);

  // Alarme critique
  EnqueuePriority(AlarmData, SizeOf(AlarmData), prCritical);

  // Les alarmes seront toujours envoyées en premier
end;
```

## Sécurité en Edge Computing

### Chiffrement local des données

```pascal
uses
  DCPcrypt2, DCPrijndael;

type
  TSecureStorage = record
    Cipher: TDCP_rijndael;
    Key: array[0..31] of Byte;  // 256 bits
  end;

procedure InitSecureStorage(var Storage: TSecureStorage; const Password: string);  
var
  Hash: array[0..31] of Byte;
begin
  // Dériver clé depuis mot de passe
  SHA256(Password, @Hash);
  Move(Hash, Storage.Key, 32);

  Storage.Cipher := TDCP_rijndael.Create(nil);
  Storage.Cipher.Init(Storage.Key, 256, nil);
end;

procedure ChiffrerDonnees(var Storage: TSecureStorage;
                          const PlainData; var CipherData; Size: Integer);
begin
  Storage.Cipher.EncryptCBC(PlainData, CipherData, Size);
end;

procedure DechiffrerDonnees(var Storage: TSecureStorage;
                            const CipherData; var PlainData; Size: Integer);
begin
  Storage.Cipher.DecryptCBC(CipherData, PlainData, Size);
end;

// Utilisation : stocker données sensibles localement
var
  SecStorage: TSecureStorage;
  SensorData: TSensorData;
  EncryptedData: array[0..255] of Byte;
begin
  InitSecureStorage(SecStorage, 'MotDePasseSecret123');

  // Chiffrer avant stockage
  ChiffrerDonnees(SecStorage, SensorData, EncryptedData, SizeOf(SensorData));
  SaveToFlash(EncryptedData, SizeOf(EncryptedData));

  // Déchiffrer lors de la lecture
  LoadFromFlash(EncryptedData, SizeOf(EncryptedData));
  DechiffrerDonnees(SecStorage, EncryptedData, SensorData, SizeOf(SensorData));
end;
```

### Authentification et intégrité

```pascal
// HMAC pour vérifier intégrité des messages
uses
  DCPsha256;

type
  TAuthenticatedMessage = record
    Data: array[0..127] of Byte;
    DataSize: Word;
    HMAC: array[0..31] of Byte;  // SHA-256 hash
  end;

procedure CalculerHMAC(const Data; Size: Integer; const Key: string;
                       var HMAC: array of Byte);
var
  Hash: TDCP_sha256;
  KeyBytes: array[0..63] of Byte;
  i: Integer;
begin
  // Préparer clé
  FillChar(KeyBytes, SizeOf(KeyBytes), 0);
  Move(Key[1], KeyBytes, Min(Length(Key), 64));

  // HMAC = H(Key XOR opad, H(Key XOR ipad, message))
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;

    // ipad XOR
    for i := 0 to 63 do
      KeyBytes[i] := KeyBytes[i] xor $36;
    Hash.Update(KeyBytes, 64);
    Hash.Update(Data, Size);
    Hash.Final(HMAC);

    // opad XOR
    for i := 0 to 63 do
      KeyBytes[i] := (KeyBytes[i] xor $36) xor $5C;

    Hash.Init;
    Hash.Update(KeyBytes, 64);
    Hash.Update(HMAC, 32);
    Hash.Final(HMAC);
  finally
    Hash.Free;
  end;
end;

function VerifierHMAC(const Msg: TAuthenticatedMessage; const Key: string): Boolean;  
var
  CalculatedHMAC: array[0..31] of Byte;
  i: Integer;
begin
  CalculerHMAC(Msg.Data, Msg.DataSize, Key, CalculatedHMAC);

  Result := True;
  for i := 0 to 31 do
  begin
    if CalculatedHMAC[i] <> Msg.HMAC[i] then
    begin
      Result := False;
      Break;
    end;
  end;
end;

// Utilisation
var
  Msg: TAuthenticatedMessage;
  SecretKey: string = 'CléSecrètePartagée';
begin
  // Envoyer message
  Move(SensorData, Msg.Data, SizeOf(SensorData));
  Msg.DataSize := SizeOf(SensorData);
  CalculerHMAC(Msg.Data, Msg.DataSize, SecretKey, Msg.HMAC);
  EnvoyerMessage(Msg);

  // Recevoir et vérifier
  RecevoirMessage(Msg);
  if VerifierHMAC(Msg, SecretKey) then
    WriteLn('Message authentique')
  else
    WriteLn('ERREUR: Message altéré ou faux !');
end;
```

## Optimisation énergétique

### Sleep modes et wake-up

```pascal
// Gestion intelligente de l'énergie
type
  TPowerMode = (pmActive, pmLowPower, pmDeepSleep);

var
  CurrentPowerMode: TPowerMode;
  BatteryLevel: Byte;

procedure SetPowerMode(Mode: TPowerMode);  
begin
  case Mode of
    pmActive:
    begin
      // Tous périphériques actifs
      EnablePeripherals(UART or SPI or I2C or ADC);
      SetCPUFrequency(168000000);  // 168 MHz
    end;

    pmLowPower:
    begin
      // Désactiver périphériques non essentiels
      DisablePeripherals(SPI or I2C);
      SetCPUFrequency(48000000);  // 48 MHz
    end;

    pmDeepSleep:
    begin
      // Tout en veille sauf RTC et wake-up sources
      DisablePeripherals(UART or SPI or I2C or ADC);
      EnterStopMode();  // Réveil par interruption externe ou RTC
    end;
  end;

  CurrentPowerMode := Mode;
end;

procedure GererEnergie;  
begin
  BatteryLevel := LireBatterie();

  if BatteryLevel < 20 then
  begin
    // Mode économie d'énergie drastique
    SetPowerMode(pmDeepSleep);

    // Réveil toutes les 5 minutes pour mesure rapide
    RTC_SetAlarm(300);  // 5 minutes
  end
  else if BatteryLevel < 50 then
  begin
    // Mode économie modérée
    SetPowerMode(pmLowPower);
  end
  else
  begin
    // Mode normal
    SetPowerMode(pmActive);
  end;
end;

// Boucle principale optimisée
begin
  while True do
  begin
    GererEnergie();

    case CurrentPowerMode of
      pmActive:
      begin
        // Échantillonner fréquemment
        LireCapteurs();
        TraiterDonnees();
        Sleep(1000);  // 1 seconde
      end;

      pmLowPower:
      begin
        // Échantillonner moins souvent
        LireCapteurs();
        TraiterDonnees();
        Sleep(10000);  // 10 secondes
      end;

      pmDeepSleep:
      begin
        // Mesure rapide puis rendormir
        LireCapteurs();
        if AnomalieDetectee() then
        begin
          SetPowerMode(pmActive);  // Réveil complet
          EnvoyerAlarme();
        end;

        EnterDeepSleep();  // Réveil par RTC
      end;
    end;
  end;
end;
```

### Transmission adaptative

```pascal
// Adapter fréquence transmission selon batterie et contexte
type
  TTransmissionStrategy = record
    Interval: LongWord;      // Intervalle en ms
    Aggregation: Byte;       // Nombre d'échantillons à agréger
    Compression: Boolean;    // Activer compression
  end;

function DeterminerStrategie: TTransmissionStrategy;  
begin
  if BatteryLevel > 80 then
  begin
    // Batterie pleine : transmission fréquente
    Result.Interval := 10000;     // 10 secondes
    Result.Aggregation := 1;      // Pas d'agrégation
    Result.Compression := False;
  end
  else if BatteryLevel > 50 then
  begin
    // Batterie moyenne : agrégation modérée
    Result.Interval := 60000;     // 1 minute
    Result.Aggregation := 6;      // 6 échantillons
    Result.Compression := False;
  end
  else if BatteryLevel > 20 then
  begin
    // Batterie faible : forte agrégation
    Result.Interval := 300000;    // 5 minutes
    Result.Aggregation := 30;     // 30 échantillons
    Result.Compression := True;
  end
  else
  begin
    // Batterie critique : transmission minimale
    Result.Interval := 3600000;   // 1 heure
    Result.Aggregation := 360;    // 360 échantillons
    Result.Compression := True;
  end;
end;

var
  Strategy: TTransmissionStrategy;
  SampleBuffer: array[0..359] of Word;
  SampleCount: Word;

begin
  Strategy := DeterminerStrategie();
  SampleCount := 0;

  while True do
  begin
    // Toujours échantillonner (important pour le monitoring)
    SampleBuffer[SampleCount] := LireCapteur();
    Inc(SampleCount);

    if SampleCount >= Strategy.Aggregation then
    begin
      // Agréger
      Aggregated := AgregerEchantillons(SampleBuffer, SampleCount);

      // Compresser si nécessaire
      if Strategy.Compression then
        Aggregated := CompresserDonnees(Aggregated);

      // Envoyer
      EnvoyerCloud(Aggregated);

      SampleCount := 0;

      // Réévaluer stratégie
      Strategy := DeterminerStrategie();
    end;

    Sleep(Strategy.Interval div Strategy.Aggregation);
  end;
end;
```

## Protocoles légers pour Edge

### MQTT pour IoT

```pascal
// Client MQTT léger
uses
  MQTTClient;

type
  TEdgeMQTT = record
    Client: TMQTTClient;
    BrokerIP: string;
    ClientID: string;
    Connected: Boolean;
  end;

procedure InitMQTT(var MQTT: TEdgeMQTT; const Broker, ClientID: string);  
begin
  MQTT.BrokerIP := Broker;
  MQTT.ClientID := ClientID;
  MQTT.Client := TMQTTClient.Create;
  MQTT.Connected := False;
end;

function ConnectMQTT(var MQTT: TEdgeMQTT): Boolean;  
begin
  try
    MQTT.Client.Connect(MQTT.BrokerIP, 1883, MQTT.ClientID);
    MQTT.Connected := True;
    Result := True;
    WriteLn('Connecté à MQTT broker');
  except
    MQTT.Connected := False;
    Result := False;
    WriteLn('Échec connexion MQTT');
  end;
end;

procedure PublishMQTT(var MQTT: TEdgeMQTT; const Topic: string;
                      const Payload: string; QoS: Byte);
begin
  if not MQTT.Connected then
    if not ConnectMQTT(MQTT) then
      Exit;

  try
    MQTT.Client.Publish(Topic, Payload, QoS, False);
  except
    MQTT.Connected := False;
    WriteLn('Échec publication MQTT');
  end;
end;

procedure SubscribeMQTT(var MQTT: TEdgeMQTT; const Topic: string;
                        Callback: TMQTTMessageEvent);
begin
  if not MQTT.Connected then
    if not ConnectMQTT(MQTT) then
      Exit;

  MQTT.Client.Subscribe(Topic, 1);
  MQTT.Client.OnMessage := Callback;
end;

// Utilisation
var
  EdgeMQTT: TEdgeMQTT;
  Temp: Real;
  Payload: string;

procedure OnCommandReceived(const Topic, Message: string);  
begin
  WriteLn('Commande reçue: ', Topic, ' = ', Message);

  if Topic = 'device/1/command' then
  begin
    if Message = 'reset' then
      ResetDevice();
    if Message = 'sleep' then
      EnterSleepMode();
  end;
end;

begin
  InitMQTT(EdgeMQTT, '192.168.1.100', 'edge-device-001');
  ConnectMQTT(EdgeMQTT);

  // S'abonner aux commandes
  SubscribeMQTT(EdgeMQTT, 'device/1/command', @OnCommandReceived);

  while True do
  begin
    Temp := LireTemperature();

    // Publier données (QoS 0 = at most once, léger)
    Payload := Format('{"temp":%.1f,"time":%d}', [Temp, GetUnixTime()]);
    PublishMQTT(EdgeMQTT, 'sensor/temp', Payload, 0);

    Sleep(10000);
  end;
end;
```

### CoAP (Constrained Application Protocol)

```pascal
// CoAP est encore plus léger que HTTP pour IoT
type
  TCoapMessage = record
    Version: Byte;        // 2 bits
    MessageType: Byte;    // 2 bits (CON, NON, ACK, RST)
    TokenLength: Byte;    // 4 bits
    Code: Byte;           // 8 bits (méthode ou status)
    MessageID: Word;      // 16 bits
    Token: array[0..7] of Byte;
    Payload: array[0..255] of Byte;
    PayloadLength: Word;
  end;

function CreerCoapGET(const URI: string; MessageID: Word): TCoapMessage;  
begin
  FillChar(Result, SizeOf(Result), 0);

  Result.Version := 1;
  Result.MessageType := 0;  // CON (Confirmable)
  Result.Code := 1;         // GET
  Result.MessageID := MessageID;

  // Ajouter URI comme option (simplifié)
  Move(URI[1], Result.Payload, Length(URI));
  Result.PayloadLength := Length(URI);
end;

procedure EnvoyerCoap(const Msg: TCoapMessage; const ServerIP: string);  
var
  Socket: Integer;
  Buffer: array[0..511] of Byte;
  Size: Integer;
begin
  // Sérialiser message
  Size := SerializerCoap(Msg, Buffer);

  // Envoyer via UDP (CoAP utilise UDP port 5683)
  Socket := UDP_CreateSocket();
  UDP_SendTo(Socket, ServerIP, 5683, @Buffer, Size);
  UDP_CloseSocket(Socket);
end;

// Utilisation : GET d'une ressource
var
  Msg: TCoapMessage;
begin
  Msg := CreerCoapGET('/temperature', 1234);
  EnvoyerCoap(Msg, '192.168.1.100');
end;

// CoAP est ~10x plus léger que HTTP (overhead de 4 octets vs 100+)
```

## Edge Analytics

### Détection d'anomalies en temps réel

```pascal
// Algorithme simple de détection d'anomalie : Z-Score
type
  TStatistics = record
    Mean: Real;
    StdDev: Real;
    Count: LongWord;
    M2: Real;  // Pour calcul variance en ligne (Welford)
  end;

procedure InitStats(var Stats: TStatistics);  
begin
  Stats.Mean := 0.0;
  Stats.StdDev := 0.0;
  Stats.Count := 0;
  Stats.M2 := 0.0;
end;

procedure UpdateStats(var Stats: TStatistics; Value: Real);  
var
  Delta, Delta2: Real;
begin
  Inc(Stats.Count);

  // Algorithme de Welford pour variance en ligne
  Delta := Value - Stats.Mean;
  Stats.Mean := Stats.Mean + Delta / Stats.Count;
  Delta2 := Value - Stats.Mean;
  Stats.M2 := Stats.M2 + Delta * Delta2;

  if Stats.Count > 1 then
    Stats.StdDev := Sqrt(Stats.M2 / (Stats.Count - 1));
end;

function DetecterAnomalie(var Stats: TStatistics; Value: Real): Boolean;  
var
  ZScore: Real;
const
  THRESHOLD = 3.0;  // 3 écarts-types = 99.7% confiance
begin
  if Stats.Count < 10 then
  begin
    UpdateStats(Stats, Value);
    Exit(False);  // Pas assez de données pour décider
  end;

  // Calculer Z-Score
  if Stats.StdDev > 0 then
    ZScore := Abs((Value - Stats.Mean) / Stats.StdDev)
  else
    ZScore := 0;

  Result := (ZScore > THRESHOLD);

  if not Result then
    UpdateStats(Stats, Value);  // Valeur normale, mettre à jour stats
  // Sinon : ne pas polluer stats avec anomalie
end;

// Utilisation
var
  TempStats: TStatistics;
  Temp: Real;
begin
  InitStats(TempStats);

  while True do
  begin
    Temp := LireTemperature();

    if DetecterAnomalie(TempStats, Temp) then
    begin
      WriteLn('ANOMALIE: Température inhabituelle: ', Temp:0:1, '°C');
      WriteLn('  Moyenne: ', TempStats.Mean:0:1, '°C');
      WriteLn('  Écart-type: ', TempStats.StdDev:0:2);

      EnvoyerAlarme(Temp);
    end;

    Sleep(1000);
  end;
end;
```

### Pattern matching temporel

```pascal
// Détecter des patterns dans les séries temporelles
type
  TPattern = array[0..9] of Real;  // Pattern de 10 valeurs

function CorrelationPearson(const A, B: TPattern): Real;  
var
  i: Integer;
  MeanA, MeanB, SumXY, SumX2, SumY2: Real;
begin
  MeanA := 0;
  MeanB := 0;
  for i := 0 to 9 do
  begin
    MeanA := MeanA + A[i];
    MeanB := MeanB + B[i];
  end;
  MeanA := MeanA / 10;
  MeanB := MeanB / 10;

  SumXY := 0;
  SumX2 := 0;
  SumY2 := 0;
  for i := 0 to 9 do
  begin
    SumXY := SumXY + (A[i] - MeanA) * (B[i] - MeanB);
    SumX2 := SumX2 + Sqr(A[i] - MeanA);
    SumY2 := SumY2 + Sqr(B[i] - MeanB);
  end;

  if (SumX2 > 0) and (SumY2 > 0) then
    Result := SumXY / Sqrt(SumX2 * SumY2)
  else
    Result := 0;
end;

// Pattern de défaillance machine connu
const
  PATTERN_DEFAILLANCE: TPattern = (
    20.0, 22.0, 25.0, 30.0, 38.0, 50.0, 65.0, 80.0, 95.0, 110.0
  );  // Montée température progressive

var
  RecentValues: TPattern;
  ValueIndex: Byte = 0;

function DetecterPatternDefaillance(NewValue: Real): Boolean;  
var
  Correlation: Real;
begin
  // Ajouter nouvelle valeur
  RecentValues[ValueIndex] := NewValue;
  ValueIndex := (ValueIndex + 1) mod 10;

  // Comparer avec pattern connu
  Correlation := CorrelationPearson(RecentValues, PATTERN_DEFAILLANCE);

  Result := (Correlation > 0.85);  // 85% de similarité

  if Result then
    WriteLn('ALERTE: Pattern de défaillance détecté (corr=',
            Correlation:0:2, ')');
end;

// Détection prédictive de panne
begin
  while True do
  begin
    Temp := LireTemperature();

    if DetecterPatternDefaillance(Temp) then
    begin
      // Panne probable dans les prochaines minutes
      ArreterMachine();
      AppelerMaintenance();
    end;

    Sleep(10000);
  end;
end;
```

## Exemple complet : Smart Building Edge

```pascal
program SmartBuildingEdge;

{$MODE OBJFPC}
{$H+}

uses
  cThreads, Sockets, SysUtils, SQLite3, MQTTClient;

type
  // Données de capteur
  TSensorReading = packed record
    SensorID: Byte;
    RoomID: Byte;
    Temperature: SmallInt;  // x10
    Humidity: Byte;
    CO2: Word;              // ppm
    Occupancy: Boolean;
    LightLevel: Byte;
    Timestamp: LongWord;
  end;

  // Décision d'optimisation
  TOptimization = record
    RoomID: Byte;
    SetHVAC: Boolean;
    TargetTemp: SmallInt;
    SetLights: Boolean;
    LightLevel: Byte;
  end;

var
  LocalDB: PSQLite3;
  MQTT: TMQTTClient;
  Optimizations: array[0..99] of TOptimization;
  OptCount: Byte;

// === ACQUISITION ===
procedure TraiterCapteur(const Reading: TSensorReading);  
var
  SQL: string;
begin
  // Stockage local
  SQL := Format(
    'INSERT INTO readings (sensor_id, room_id, temp, humidity, co2, ' +
    'occupancy, light, timestamp) VALUES (%d,%d,%d,%d,%d,%d,%d,%d)',
    [Reading.SensorID, Reading.RoomID, Reading.Temperature,
     Reading.Humidity, Reading.CO2, Ord(Reading.Occupancy),
     Reading.LightLevel, Reading.Timestamp]
  );
  sqlite3_exec(LocalDB, PChar(SQL), nil, nil, nil);

  // Analyse locale immédiate
  AnalyserPiece(Reading.RoomID);
end;

// === ANALYSE LOCALE (EDGE) ===
procedure AnalyserPiece(RoomID: Byte);  
var
  SQL: string;
  Stmt: PSQLite3_Stmt;
  AvgTemp, AvgCO2: Real;
  Occupancy: Boolean;
  Opt: TOptimization;
begin
  // Moyenne dernières 5 minutes
  SQL := Format(
    'SELECT AVG(temp), AVG(co2), MAX(occupancy) ' +
    'FROM readings ' +
    'WHERE room_id = %d AND timestamp > %d',
    [RoomID, GetUnixTime() - 300]
  );

  sqlite3_prepare_v2(LocalDB, PChar(SQL), -1, @Stmt, nil);

  if sqlite3_step(Stmt) = SQLITE_ROW then
  begin
    AvgTemp := sqlite3_column_double(Stmt, 0);
    AvgCO2 := sqlite3_column_double(Stmt, 1);
    Occupancy := sqlite3_column_int(Stmt, 2) > 0;
  end
  else
  begin
    sqlite3_finalize(Stmt);
    Exit;
  end;

  sqlite3_finalize(Stmt);

  // === DÉCISION EDGE (LOCALE, TEMPS RÉEL) ===
  Opt.RoomID := RoomID;
  Opt.SetHVAC := False;
  Opt.SetLights := False;

  // Logique d'optimisation
  if Occupancy then
  begin
    // Pièce occupée : confort
    if AvgTemp > 230 then  // > 23°C
    begin
      Opt.SetHVAC := True;
      Opt.TargetTemp := 210;  // 21°C
      WriteLn('Pièce ', RoomID, ': Refroidissement activé');
    end
    else if AvgTemp < 190 then  // < 19°C
    begin
      Opt.SetHVAC := True;
      Opt.TargetTemp := 210;  // 21°C
      WriteLn('Pièce ', RoomID, ': Chauffage activé');
    end;

    // Ventilation si CO2 élevé
    if AvgCO2 > 1000 then
    begin
      ActiverVentilation(RoomID);
      WriteLn('Pièce ', RoomID, ': Ventilation activée (CO2: ', AvgCO2:0:0, ' ppm)');
    end;

    // Lumières auto
    Opt.SetLights := True;
    Opt.LightLevel := 80;  // 80% si occupé
  end
  else
  begin
    // Pièce vide : économie d'énergie
    Opt.SetHVAC := True;
    Opt.TargetTemp := 180;  // 18°C (économie)

    Opt.SetLights := True;
    Opt.LightLevel := 0;  // Éteindre lumières

    WriteLn('Pièce ', RoomID, ': Mode économie (inoccupée)');
  end;

  // Appliquer optimisation IMMÉDIATEMENT
  AppliquerOptimisation(Opt);

  // Stocker pour agrégation
  Optimizations[OptCount] := Opt;
  Inc(OptCount);
end;

// === ACTION LOCALE ===
procedure AppliquerOptimisation(const Opt: TOptimization);  
begin
  if Opt.SetHVAC then
  begin
    // Commande HVAC locale (Modbus, BACnet, etc.)
    SetHVACTarget(Opt.RoomID, Opt.TargetTemp);
  end;

  if Opt.SetLights then
  begin
    // Commande éclairage locale (DMX, DALI, etc.)
    SetLightLevel(Opt.RoomID, Opt.LightLevel);
  end;
end;

// === AGRÉGATION POUR CLOUD ===
procedure AgregerEtEnvoyerCloud;  
var
  i: Integer;
  JSON: string;
  TotalSavings: Real;
begin
  if OptCount = 0 then Exit;

  // Calculer économies d'énergie estimées
  TotalSavings := 0;
  for i := 0 to OptCount - 1 do
  begin
    if not Optimizations[i].SetHVAC then Continue;

    // Estimation simple : 1°C = 7% économie
    TotalSavings := TotalSavings +
      Abs(Optimizations[i].TargetTemp - 210) * 0.07;
  end;

  // Créer JSON agrégé
  JSON := Format(
    '{"timestamp":%d,"optimizations":%d,"estimated_savings":%.1f%%}',
    [GetUnixTime(), OptCount, TotalSavings]
  );

  // Envoyer au cloud via MQTT
  MQTT.Publish('building/edge/summary', JSON, 0, False);

  WriteLn('Envoyé au cloud: ', OptCount, ' optimisations, ',
          TotalSavings:0:1, '% économies');

  OptCount := 0;  // Reset compteur
end;

// === THREAD SERVEUR CAPTEURS ===
procedure ThreadServeurCapteurs;  
var
  ServerSocket, ClientSocket: Integer;
  Reading: TSensorReading;
  BytesRead: Integer;
begin
  ServerSocket := Socket_Listen(9000);
  WriteLn('Serveur capteurs démarré sur port 9000');

  while True do
  begin
    ClientSocket := Socket_Accept(ServerSocket);

    BytesRead := Socket_Receive(ClientSocket, @Reading, SizeOf(Reading));

    if BytesRead = SizeOf(Reading) then
    begin
      WriteLn('Capteur ', Reading.SensorID, ' (Pièce ', Reading.RoomID, '): ',
              Reading.Temperature / 10:0:1, '°C, ',
              Reading.Humidity, '%, ',
              Reading.CO2, ' ppm');

      // Traitement local IMMÉDIAT
      TraiterCapteur(Reading);
    end;

    Socket_Close(ClientSocket);
  end;
end;

// === THREAD AGRÉGATION PÉRIODIQUE ===
procedure ThreadAgregation;  
begin
  while True do
  begin
    Sleep(300000);  // 5 minutes

    AgregerEtEnvoyerCloud();
  end;
end;

// === THREAD NETTOYAGE BASE DE DONNÉES ===
procedure ThreadNettoyage;  
var
  SQL: string;
begin
  while True do
  begin
    Sleep(3600000);  // 1 heure

    // Garder seulement 24h de données locales
    SQL := Format(
      'DELETE FROM readings WHERE timestamp < %d',
      [GetUnixTime() - 86400]
    );

    sqlite3_exec(LocalDB, PChar(SQL), nil, nil, nil);

    WriteLn('Nettoyage DB: données > 24h supprimées');
  end;
end;

// === INITIALISATION ===
procedure InitDatabase;  
const
  SQL_CREATE =
    'CREATE TABLE IF NOT EXISTS readings (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  sensor_id INTEGER,' +
    '  room_id INTEGER,' +
    '  temp INTEGER,' +
    '  humidity INTEGER,' +
    '  co2 INTEGER,' +
    '  occupancy INTEGER,' +
    '  light INTEGER,' +
    '  timestamp INTEGER' +
    ')';

  SQL_INDEX =
    'CREATE INDEX IF NOT EXISTS idx_room_time ' +
    'ON readings(room_id, timestamp)';
begin
  sqlite3_open('smart_building.db', @LocalDB);
  sqlite3_exec(LocalDB, PChar(SQL_CREATE), nil, nil, nil);
  sqlite3_exec(LocalDB, PChar(SQL_INDEX), nil, nil, nil);

  WriteLn('Base de données initialisée');
end;

procedure InitMQTT;  
begin
  MQTT := TMQTTClient.Create;
  MQTT.Connect('192.168.1.100', 1883, 'smart-building-edge');

  WriteLn('Connecté au broker MQTT');
end;

// === PROGRAMME PRINCIPAL ===
begin
  WriteLn('=== Smart Building Edge Computing ===');
  WriteLn;

  InitDatabase();
  InitMQTT();

  OptCount := 0;

  // Lancer threads
  BeginThread(@ThreadServeurCapteurs);
  BeginThread(@ThreadAgregation);
  BeginThread(@ThreadNettoyage);

  WriteLn('Système démarré. Traitement local actif.');
  WriteLn('Appuyez sur Ctrl+C pour arrêter.');
  WriteLn;

  // Boucle principale
  while True do
    Sleep(1000);
end.
```

**Architecture du système** :

```
[Capteurs] → [Edge Gateway (Ce programme)] → [Optimisations locales]
                    ↓                              ↓
              [Base locale]                  [Actionneurs]
                    ↓
              [Agrégation]
                    ↓
            [Cloud (MQTT)] ← (Données agrégées seulement)
```

**Bénéfices** :
- Réaction en temps réel (< 1 seconde)
- Fonctionne même si connexion cloud perdue
- Réduction 95% du trafic vers cloud
- Économies d'énergie optimisées localement

## Déploiement et mise à jour OTA

### Over-The-Air (OTA) Updates

```pascal
// Mise à jour firmware à distance
type
  TFirmwareUpdate = record
    Version: Word;
    Size: LongWord;
    CRC32: LongWord;
    URL: String[128];
  end;

const
  CURRENT_VERSION = 102;  // v1.0.2
  FIRMWARE_PARTITION_ADDR = $08010000;  // Adresse Flash

var
  PendingUpdate: TFirmwareUpdate;
  UpdateAvailable: Boolean = False;

procedure CheckForUpdates;  
var
  Response: string;
  JSON: TJSONObject;
begin
  // Requête au serveur de mise à jour
  Response := HTTP_GET('https://updates.myiot.com/firmware/latest');

  JSON := ParseJSON(Response);
  try
    PendingUpdate.Version := JSON.GetInteger('version');
    PendingUpdate.Size := JSON.GetInteger('size');
    PendingUpdate.CRC32 := JSON.GetInteger('crc32');
    PendingUpdate.URL := JSON.GetString('url');

    if PendingUpdate.Version > CURRENT_VERSION then
    begin
      WriteLn('Mise à jour disponible: v', PendingUpdate.Version div 100, '.',
              (PendingUpdate.Version mod 100) div 10, '.',
              PendingUpdate.Version mod 10);
      UpdateAvailable := True;
    end;
  finally
    JSON.Free;
  end;
end;

function DownloadFirmware(const URL: string): Boolean;  
var
  Buffer: array[0..1023] of Byte;
  BytesRead, TotalRead: LongWord;
  FileHandle: Integer;
begin
  Result := False;
  TotalRead := 0;

  WriteLn('Téléchargement firmware...');

  FileHandle := FileCreate('firmware_new.bin');
  if FileHandle < 0 then Exit;

  try
    // Télécharger par blocs
    while TotalRead < PendingUpdate.Size do
    begin
      BytesRead := HTTP_DownloadChunk(URL, TotalRead, @Buffer, 1024);
      if BytesRead = 0 then Break;

      FileWrite(FileHandle, Buffer, BytesRead);
      Inc(TotalRead, BytesRead);

      // Progress
      Write(#13, 'Progrès: ', (TotalRead * 100) div PendingUpdate.Size, '%');
    end;

    WriteLn;
    Result := (TotalRead = PendingUpdate.Size);
  finally
    FileClose(FileHandle);
  end;
end;

function VerifyFirmware: Boolean;  
var
  FileHandle: Integer;
  Buffer: array[0..1023] of Byte;
  BytesRead: Integer;
  CRC: LongWord;
begin
  WriteLn('Vérification CRC32...');

  FileHandle := FileOpen('firmware_new.bin', fmOpenRead);
  if FileHandle < 0 then Exit(False);

  CRC := $FFFFFFFF;
  try
    repeat
      BytesRead := FileRead(FileHandle, Buffer, 1024);
      if BytesRead > 0 then
        CRC := UpdateCRC32(CRC, @Buffer, BytesRead);
    until BytesRead = 0;
  finally
    FileClose(FileHandle);
  end;

  CRC := not CRC;

  Result := (CRC = PendingUpdate.CRC32);

  if Result then
    WriteLn('CRC32 OK')
  else
    WriteLn('ERREUR: CRC32 incorrect !');
end;

procedure ApplyFirmwareUpdate;  
begin
  WriteLn('Installation mise à jour...');

  // Copier nouveau firmware vers partition de mise à jour
  FlashErase(FIRMWARE_PARTITION_ADDR, PendingUpdate.Size);
  FlashWriteFile(FIRMWARE_PARTITION_ADDR, 'firmware_new.bin');

  // Marquer pour boot sur nouvelle partition
  SetBootPartition(1);  // Partition alternative

  WriteLn('Mise à jour installée. Redémarrage...');
  Sleep(2000);

  SystemReset();
end;

procedure PerformOTAUpdate;  
begin
  if not UpdateAvailable then Exit;

  WriteLn('=== Démarrage mise à jour OTA ===');

  if DownloadFirmware(PendingUpdate.URL) then
  begin
    if VerifyFirmware() then
    begin
      ApplyFirmwareUpdate();
    end
    else
    begin
      WriteLn('Mise à jour annulée: vérification échouée');
      DeleteFile('firmware_new.bin');
    end;
  end
  else
  begin
    WriteLn('Mise à jour annulée: téléchargement échoué');
  end;

  UpdateAvailable := False;
end;

// Vérifier mises à jour toutes les 24h
begin
  while True do
  begin
    CheckForUpdates();

    if UpdateAvailable then
      PerformOTAUpdate();

    Sleep(86400000);  // 24 heures
  end;
end;
```

## Monitoring et observabilité

### Métriques système

```pascal
type
  TSystemMetrics = record
    CPUUsage: Byte;        // 0-100%
    MemoryUsage: Word;     // Octets
    DiskUsage: LongWord;   // Octets
    NetworkTX: LongWord;   // Octets envoyés
    NetworkRX: LongWord;   // Octets reçus
    Uptime: LongWord;      // Secondes
    Temperature: SmallInt; // CPU temp x10
  end;

var
  Metrics: TSystemMetrics;
  LastMetrics: TSystemMetrics;

procedure CollectMetrics;  
var
  CpuTime, IdleTime: LongWord;
begin
  // CPU usage (simplifié)
  CpuTime := GetProcessTime();
  IdleTime := GetIdleTime();
  Metrics.CPUUsage := ((CpuTime - IdleTime) * 100) div CpuTime;

  // Mémoire
  Metrics.MemoryUsage := GetMemoryUsage();

  // Disque
  Metrics.DiskUsage := GetDiskUsage('/data');

  // Réseau
  Metrics.NetworkTX := GetNetworkBytesTX();
  Metrics.NetworkRX := GetNetworkBytesRX();

  // Uptime
  Metrics.Uptime := GetTickCount() div 1000;

  // Température CPU
  Metrics.Temperature := ReadCPUTemperature();
end;

procedure PublishMetrics;  
var
  JSON: string;
begin
  JSON := Format(
    '{"cpu":%d,"mem":%d,"disk":%d,"net_tx":%d,"net_rx":%d,' +
    '"uptime":%d,"temp":%.1f}',
    [Metrics.CPUUsage, Metrics.MemoryUsage, Metrics.DiskUsage,
     Metrics.NetworkTX - LastMetrics.NetworkTX,
     Metrics.NetworkRX - LastMetrics.NetworkRX,
     Metrics.Uptime, Metrics.Temperature / 10.0]
  );

  MQTT.Publish('edge/metrics', JSON, 0, False);

  LastMetrics := Metrics;
end;

// Thread monitoring
procedure ThreadMonitoring;  
begin
  while True do
  begin
    CollectMetrics();
    PublishMetrics();

    // Alertes locales
    if Metrics.CPUUsage > 90 then
      WriteLn('ALERTE: CPU > 90%');

    if Metrics.Temperature > 800 then  // > 80°C
      WriteLn('ALERTE: Température élevée');

    Sleep(60000);  // Toutes les minutes
  end;
end;
```

### Logging structuré

```pascal
type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  TLogEntry = record
    Timestamp: LongWord;
    Level: TLogLevel;
    Component: String[32];
    Message: String[128];
  end;

const
  LOG_FILE = '/var/log/edge.log';
  MAX_LOG_SIZE = 10485760;  // 10 MB

var
  LogFile: TextFile;
  LogMutex: TRTLCriticalSection;

procedure InitLogging;  
begin
  InitCriticalSection(LogMutex);
  AssignFile(LogFile, LOG_FILE);

  if FileExists(LOG_FILE) then
    Append(LogFile)
  else
    Rewrite(LogFile);
end;

procedure Log(Level: TLogLevel; const Component, Message: string);  
const
  LevelStr: array[TLogLevel] of string = (
    'DEBUG', 'INFO', 'WARN', 'ERROR', 'CRIT'
  );
var
  Entry: TLogEntry;
  LogLine: string;
begin
  Entry.Timestamp := GetUnixTime();
  Entry.Level := Level;
  Entry.Component := Component;
  Entry.Message := Message;

  // Format: timestamp [LEVEL] Component: Message
  LogLine := Format('%d [%s] %s: %s',
    [Entry.Timestamp, LevelStr[Level], Component, Message]);

  EnterCriticalSection(LogMutex);
  try
    WriteLn(LogFile, LogLine);
    Flush(LogFile);

    // Aussi en console
    WriteLn(LogLine);

    // Rotation si trop gros
    if FileSize(LogFile) > MAX_LOG_SIZE then
    begin
      CloseFile(LogFile);
      RenameFile(LOG_FILE, LOG_FILE + '.old');
      Rewrite(LogFile);
    end;
  finally
    LeaveCriticalSection(LogMutex);
  end;

  // Envoyer erreurs critiques au cloud
  if Level >= llError then
  begin
    MQTT.Publish('edge/logs/error', LogLine, 1, False);
  end;
end;

// Utilisation
begin
  InitLogging();

  Log(llInfo, 'Main', 'Système démarré');
  Log(llDebug, 'Sensor', 'Lecture température: 23.5°C');
  Log(llWarning, 'Network', 'Latence élevée détectée');
  Log(llError, 'Database', 'Échec écriture SQLite');
  Log(llCritical, 'System', 'Mémoire insuffisante !');
end;
```

## Cas d'usage avancés

### Edge AI : Inférence de réseau de neurones

```pascal
// Modèle de classification d'images léger (MobileNet-like)
type
  TImageTensor = array[0..223, 0..223, 0..2] of Byte;  // 224x224 RGB
  TConvLayer = record
    Weights: array of Single;
    Bias: array of Single;
    OutputChannels: Word;
  end;

var
  Model: array[0..9] of TConvLayer;  // 10 couches

procedure LoadModel(const Filename: string);  
var
  f: File of Byte;
  i, j: Integer;
begin
  AssignFile(f, Filename);
  Reset(f);

  for i := 0 to 9 do
  begin
    // Charger poids et biais depuis fichier
    // (simplifié)
  end;

  CloseFile(f);
  WriteLn('Modèle chargé');
end;

function Classify(const Image: TImageTensor): Byte;  
var
  // Implémentation simplifiée
  x, y, c: Integer;
  MaxVal: Single;
  MaxIdx: Byte;
begin
  // Forward pass à travers les couches
  // ... (convolutions, pooling, activation)

  // Retourner classe avec score max
  Result := MaxIdx;
end;

// Utilisation : classification en temps réel
var
  Frame: TImageTensor;
  ClassID: Byte;
const
  ClassNames: array[0..9] of string = (
    'Personne', 'Voiture', 'Vélo', 'Chien', 'Chat',
    'Arbre', 'Bâtiment', 'Route', 'Ciel', 'Autre'
  );

begin
  LoadModel('mobilenet_edge.bin');

  while True do
  begin
    // Capturer image caméra
    Camera_CaptureFrame(Frame);

    // Classification LOCALE (pas de cloud)
    ClassID := Classify(Frame);

    WriteLn('Détecté: ', ClassNames[ClassID]);

    // Action basée sur classification
    if ClassID = 0 then  // Personne détectée
    begin
      ActiverEclairage();
      LogEvent('person_detected');
    end;

    Sleep(100);  // 10 FPS
  end;
end;
```

### Edge Data Pipeline

```pascal
// Pipeline de traitement de données en flux
type
  TDataPipeline = class
  private
    FStages: array of TPipelineStage;
  public
    procedure AddStage(Stage: TPipelineStage);
    procedure Process(Data: Pointer);
  end;

  TPipelineStage = class
  public
    function Execute(Data: Pointer): Pointer; virtual; abstract;
  end;

// Étape 1 : Filtrage
type
  TFilterStage = class(TPipelineStage)
    function Execute(Data: Pointer): Pointer; override;
  end;

function TFilterStage.Execute(Data: Pointer): Pointer;  
var
  Input: ^TSensorData;
  Output: ^TSensorData;
begin
  Input := Data;

  // Filtrer valeurs aberrantes
  if (Input^.Temperature > -400) and (Input^.Temperature < 1000) then
  begin
    New(Output);
    Output^ := Input^;
    Result := Output;
  end
  else
    Result := nil;  // Données rejetées
end;

// Étape 2 : Normalisation
type
  TNormalizeStage = class(TPipelineStage)
    function Execute(Data: Pointer): Pointer; override;
  end;

function TNormalizeStage.Execute(Data: Pointer): Pointer;  
var
  Input: ^TSensorData;
begin
  Input := Data;

  // Normaliser température (°C → 0-1)
  Input^.Temperature := Round((Input^.Temperature + 400) / 1400 * 1000);

  Result := Input;
end;

// Étape 3 : Agrégation
type
  TAggregateStage = class(TPipelineStage)
  private
    FBuffer: array[0..9] of TSensorData;
    FCount: Byte;
  public
    function Execute(Data: Pointer): Pointer; override;
  end;

function TAggregateStage.Execute(Data: Pointer): Pointer;  
var
  Input: ^TSensorData;
  Aggregated: ^TAggregatedData;
begin
  Input := Data;

  FBuffer[FCount] := Input^;
  Inc(FCount);

  if FCount >= 10 then
  begin
    // Créer agrégat
    New(Aggregated);
    Aggregated^.AvgTemp := CalculateAverage(FBuffer);
    Aggregated^.Count := FCount;

    FCount := 0;
    Result := Aggregated;
  end
  else
    Result := nil;  // Pas encore assez de données
end;

// Utilisation du pipeline
var
  Pipeline: TDataPipeline;
  SensorData: TSensorData;
  Output: Pointer;

begin
  Pipeline := TDataPipeline.Create;

  // Construire pipeline
  Pipeline.AddStage(TFilterStage.Create);
  Pipeline.AddStage(TNormalizeStage.Create);
  Pipeline.AddStage(TAggregateStage.Create);

  while True do
  begin
    LireCapteur(SensorData);

    // Traiter à travers le pipeline
    Output := Pipeline.Process(@SensorData);

    if Output <> nil then
    begin
      // Données agrégées disponibles
      EnvoyerCloud(Output);
      Dispose(Output);
    end;
  end;
end;
```

## Bonnes pratiques Edge Computing

### 1. Prioriser la robustesse

```pascal
// Toujours prévoir le mode dégradé
procedure TraiterDonnees(const Data: TSensorData);  
begin
  // Essayer traitement optimal
  try
    TraitementComplet(Data);
  except
    on E: Exception do
    begin
      Log(llWarning, 'Processing', 'Échec traitement complet: ' + E.Message);

      // Basculer sur traitement minimal
      try
        TraitementMinimal(Data);
      except
        Log(llError, 'Processing', 'Échec traitement minimal');

        // Au pire, juste logger
        LogRawData(Data);
      end;
    end;
  end;
end;
```

### 2. Optimiser pour la latence

```pascal
// Préférer approximation rapide à précision lente
function CalculRapideDistance(x1, y1, x2, y2: Real): Real;  
var
  dx, dy: Real;
begin
  dx := Abs(x2 - x1);
  dy := Abs(y2 - y1);

  // Approximation Manhattan (rapide)
  // Au lieu de Euclidienne (lent: Sqrt(dx² + dy²))
  Result := dx + dy;
end;

// ✓ Rapide : 2 soustractions + 2 valeurs absolues + 1 addition
// ✗ Lent : 2 soustractions + 2 carrés + 1 addition + 1 racine carrée
```

### 3. Gérer la mémoire

```pascal
// Réutiliser buffers au lieu d'allouer
var
  WorkBuffer: array[0..1023] of Byte;  // Buffer réutilisable

procedure TraiterMessage(const Msg: TMessage);  
begin
  // ✗ MAUVAIS
  // var LocalBuffer: array[0..1023] of Byte;  // Nouvelle allocation

  // ✓ BON
  FillChar(WorkBuffer, SizeOf(WorkBuffer), 0);

  // Utiliser WorkBuffer
  DecoderMessage(Msg, @WorkBuffer);
  TraiterBuffer(@WorkBuffer);
end;
```

### 4. Tester la résilience

```pascal
// Simuler pannes réseau
procedure TestResilience;  
var
  SimulatedFailure: Boolean;
begin
  SimulatedFailure := (Random(100) < 20);  // 20% échec

  if SimulatedFailure then
  begin
    Log(llDebug, 'Test', 'Simulation panne réseau');
    IsConnected := False;
  end;

  // Le système doit continuer à fonctionner
  TraiterDonnees();

  if SimulatedFailure then
    IsConnected := True;  // Restaurer
end;
```

## Conclusion

L'**Edge Computing** avec FreePascal permet de créer des systèmes :

✅ **Réactifs** : Traitement local en < 100ms  
✅ **Autonomes** : Fonctionnent sans connexion cloud  
✅ **Économiques** : Réduction 90-99% du trafic réseau  
✅ **Sécurisés** : Données sensibles restent locales  
✅ **Scalables** : Chaque edge device est indépendant

### Récapitulatif architecture Edge

```
Niveau Device (ESP32, STM32)
  ↓ Acquisition capteurs
  ↓ Décisions simples locales
  ↓ Envoi données filtrées

Niveau Gateway (Raspberry Pi, PC embarqué)
  ↓ Agrégation multi-devices
  ↓ Analytics locales
  ↓ ML inference
  ↓ Stockage temporaire
  ↓ Envoi synthèse au cloud

Niveau Cloud (Serveur distant)
  ↓ Big Data analytics
  ↓ ML training
  ↓ Tableaux de bord globaux
  ↓ Stockage long terme
```

### Quand utiliser Edge Computing ?

**✅ Edge Computing est idéal pour :**

| Critère | Description | Exemple |
|---------|-------------|---------|
| **Latence critique** | Réponse < 100ms requise | Freinage autonome, robotique industrielle |
| **Bande passante limitée** | Connexion lente ou coûteuse | IoT rural, sites isolés |
| **Confidentialité** | Données sensibles | Caméras de surveillance, données médicales |
| **Autonomie** | Doit fonctionner offline | Véhicules, drones, équipements mobiles |
| **Volume élevé** | Trop de données pour cloud | Vidéo 4K, capteurs haute fréquence |
| **Coûts cloud élevés** | Transfert ou stockage cher | Millions de devices IoT |

**❌ Cloud Computing reste meilleur pour :**

| Critère | Description | Exemple |
|---------|-------------|---------|
| **Calculs lourds** | Ressources edge insuffisantes | Training deep learning, simulations |
| **Données historiques** | Analyse longue durée | Tendances sur 5 ans, big data |
| **Centralisation** | Vue globale nécessaire | Dashboard entreprise, reporting |
| **Évolutivité illimitée** | Pics de charge imprévisibles | Application web grand public |
| **Pas de contrainte temps réel** | Latence acceptable | Rapports quotidiens, batch processing |

### Compromis Edge vs Cloud

```pascal
// Exemple : décider où traiter selon contexte
function DeterminerLieuTraitement(const Data: TSensorData): TProcessingLocation;  
var
  BatteryLow: Boolean;
  HighLatencyNeeded: Boolean;
  CloudAvailable: Boolean;
begin
  BatteryLow := (BatteryLevel < 30);
  HighLatencyNeeded := (Data.Priority = prCritical);
  CloudAvailable := IsConnected and (Ping < 100);

  // Décision locale si :
  if HighLatencyNeeded or BatteryLow or (not CloudAvailable) then
    Result := plEdge
  else if (Data.Size > 1024) and CloudAvailable then
    Result := plCloud  // Gros calcul → cloud
  else
    Result := plEdge;  // Par défaut : local
end;

procedure TraiterDonnees(const Data: TSensorData);  
begin
  case DeterminerLieuTraitement(Data) of
    plEdge:
    begin
      // Traitement local
      ResultatLocal := TraitementLeger(Data);
      ActionnerLocalement(ResultatLocal);
    end;

    plCloud:
    begin
      // Envoyer au cloud
      EnvoyerCloud(Data);
      // Attendre résultat (asynchrone)
      ResultatCloud := AttendrReponseCloud();
      ActionnerLocalement(ResultatCloud);
    end;
  end;
end;
```

## Patterns de conception Edge

### 1. Store and Forward

Stocker localement si connexion perdue, envoyer quand disponible.

```pascal
type
  TStoreAndForward = class
  private
    FQueue: TList;
    FMaxQueueSize: Integer;
    FConnected: Boolean;
  public
    procedure Store(const Data: Pointer);
    procedure Forward;
    procedure CheckConnection;
  end;

procedure TStoreAndForward.Store(const Data: Pointer);  
begin
  if FConnected then
  begin
    // Connexion OK, envoyer immédiatement
    SendToCloud(Data);
  end
  else
  begin
    // Pas de connexion, stocker
    if FQueue.Count < FMaxQueueSize then
    begin
      FQueue.Add(Data);
      Log(llInfo, 'Store', 'Données mises en queue');
    end
    else
    begin
      // Queue pleine, supprimer le plus ancien
      Dispose(FQueue[0]);
      FQueue.Delete(0);
      FQueue.Add(Data);
      Log(llWarning, 'Store', 'Queue pleine, anciennes données perdues');
    end;
  end;
end;

procedure TStoreAndForward.Forward;  
var
  i: Integer;
  Data: Pointer;
begin
  if not FConnected then Exit;

  // Vider la queue
  for i := FQueue.Count - 1 downto 0 do
  begin
    Data := FQueue[i];

    if SendToCloud(Data) then
    begin
      Dispose(Data);
      FQueue.Delete(i);
      Log(llInfo, 'Forward', 'Données en queue envoyées');
    end
    else
    begin
      // Échec, arrêter et réessayer plus tard
      FConnected := False;
      Break;
    end;
  end;
end;
```

### 2. Circuit Breaker

Éviter de surcharger un service distant défaillant.

```pascal
type
  TCircuitState = (csClose, csOpen, csHalfOpen);

  TCircuitBreaker = class
  private
    FState: TCircuitState;
    FFailureCount: Integer;
    FFailureThreshold: Integer;
    FTimeout: LongWord;
    FLastFailureTime: LongWord;
  public
    function Execute(Operation: TOperation): Boolean;
  end;

function TCircuitBreaker.Execute(Operation: TOperation): Boolean;  
var
  Now: LongWord;
begin
  Now := GetTickCount();

  case FState of
    csClose:
    begin
      // Circuit fermé, tenter opération
      try
        Result := Operation();
        FFailureCount := 0;  // Reset compteur si succès
      except
        Inc(FFailureCount);
        FLastFailureTime := Now;

        if FFailureCount >= FFailureThreshold then
        begin
          FState := csOpen;
          Log(llWarning, 'CircuitBreaker', 'Circuit ouvert');
        end;

        Result := False;
      end;
    end;

    csOpen:
    begin
      // Circuit ouvert, ne pas tenter
      if (Now - FLastFailureTime) > FTimeout then
      begin
        FState := csHalfOpen;
        Log(llInfo, 'CircuitBreaker', 'Tentative réouverture');
      end;

      Result := False;
    end;

    csHalfOpen:
    begin
      // Test de réouverture
      try
        Result := Operation();
        FState := csClose;
        FFailureCount := 0;
        Log(llInfo, 'CircuitBreaker', 'Circuit refermé');
      except
        FState := csOpen;
        FLastFailureTime := Now;
        Result := False;
      end;
    end;
  end;
end;

// Utilisation
var
  CloudBreaker: TCircuitBreaker;

function SendDataToCloud(Data: Pointer): Boolean;  
begin
  Result := CloudBreaker.Execute(
    function: Boolean
    begin
      Result := HTTP_POST('https://api.cloud.com/data', Data);
    end
  );

  if not Result then
  begin
    // Cloud indisponible, traiter localement
    ProcessLocally(Data);
  end;
end;
```

### 3. Event Sourcing

Stocker tous les événements plutôt que l'état.

```pascal
type
  TEventType = (etSensorReading, etAlarmTriggered, etActionTaken);

  TEvent = record
    EventID: LongWord;
    EventType: TEventType;
    Timestamp: LongWord;
    Data: array[0..255] of Byte;
    DataSize: Word;
  end;

var
  EventStore: array[0..999] of TEvent;
  EventCount: Word = 0;

procedure AppendEvent(EventType: TEventType; const Data; Size: Word);  
var
  Event: TEvent;
begin
  Event.EventID := EventCount;
  Event.EventType := EventType;
  Event.Timestamp := GetUnixTime();
  Move(Data, Event.Data, Size);
  Event.DataSize := Size;

  EventStore[EventCount mod 1000] := Event;
  Inc(EventCount);

  // Stocker sur SD/Flash
  WriteEventToDisk(Event);
end;

function ReplayEvents(FromEventID: LongWord): TSystemState;  
var
  i: LongWord;
  State: TSystemState;
begin
  // Reconstruire état depuis événements
  InitializeState(State);

  for i := FromEventID to EventCount - 1 do
  begin
    ApplyEvent(State, EventStore[i mod 1000]);
  end;

  Result := State;
end;

// Utilisation
begin
  // Enregistrer tous les événements
  AppendEvent(etSensorReading, SensorData, SizeOf(SensorData));
  AppendEvent(etAlarmTriggered, AlarmData, SizeOf(AlarmData));

  // Synchroniser avec cloud
  if Connected then
    SyncEventsSince(LastSyncedEventID);

  // Possibilité de replay complet
  CurrentState := ReplayEvents(0);
end;
```

### 4. Lambda Architecture

Combiner batch processing et stream processing.

```pascal
type
  TBatchLayer = class
    procedure ProcessBatch(const Events: array of TEvent);
  end;

  TSpeedLayer = class
    procedure ProcessStream(const Event: TEvent);
  end;

  TServingLayer = class
    function Query(const Request: string): string;
  end;

var
  BatchLayer: TBatchLayer;
  SpeedLayer: TSpeedLayer;
  ServingLayer: TServingLayer;

// Batch Layer : traitement lourd, périodique
procedure TBatchLayer.ProcessBatch(const Events: array of TEvent);  
var
  i: Integer;
  Aggregates: TAggregateData;
begin
  // Traiter toutes les données (lent mais complet)
  for i := Low(Events) to High(Events) do
  begin
    UpdateAggregates(Aggregates, Events[i]);
  end;

  // Stocker résultat
  SaveBatchView(Aggregates);
end;

// Speed Layer : temps réel
procedure TSpeedLayer.ProcessStream(const Event: TEvent);  
var
  RealtimeView: TRealtimeData;
begin
  // Mise à jour incrémentale rapide
  LoadRealtimeView(RealtimeView);
  UpdateIncremental(RealtimeView, Event);
  SaveRealtimeView(RealtimeView);
end;

// Serving Layer : combiner les vues
function TServingLayer.Query(const Request: string): string;  
var
  BatchView: TAggregateData;
  RealtimeView: TRealtimeData;
begin
  // Combiner batch (complet mais ancien) + realtime (récent)
  LoadBatchView(BatchView);
  LoadRealtimeView(RealtimeView);

  Result := MergeViews(BatchView, RealtimeView, Request);
end;

// Utilisation
begin
  // Événement arrive
  NewEvent := CreateEvent(SensorData);

  // Speed layer : traitement immédiat
  SpeedLayer.ProcessStream(NewEvent);

  // Batch layer : toutes les heures
  if (GetTickCount() mod 3600000) = 0 then
    BatchLayer.ProcessBatch(EventStore);

  // Query combinée
  Response := ServingLayer.Query('temperature_average');
end;
```

## Métriques de performance Edge

### KPIs essentiels

```pascal
type
  TEdgeMetrics = record
    // Latence
    AvgProcessingTime: LongWord;  // ms
    P95ProcessingTime: LongWord;  // 95e percentile
    MaxProcessingTime: LongWord;  // pire cas

    // Débit
    EventsPerSecond: Real;
    BytesPerSecond: LongWord;

    // Fiabilité
    SuccessRate: Real;            // 0-100%
    ErrorCount: LongWord;

    // Ressources
    CPUUsage: Byte;               // 0-100%
    MemoryUsage: LongWord;        // octets
    NetworkUsage: LongWord;       // octets/s

    // Efficacité
    LocalProcessingRate: Real;    // % traité localement
    CloudOffloadRate: Real;       // % envoyé au cloud
    DataReductionRatio: Real;     // compression ratio
  end;

var
  Metrics: TEdgeMetrics;
  ProcessingTimes: array[0..99] of LongWord;
  PTIndex: Byte = 0;

procedure RecordProcessingTime(Time: LongWord);  
var
  i: Integer;
  Sum: QWord;
  Sorted: array[0..99] of LongWord;
begin
  ProcessingTimes[PTIndex] := Time;
  PTIndex := (PTIndex + 1) mod 100;

  // Calculer moyenne
  Sum := 0;
  for i := 0 to 99 do
    Sum := Sum + ProcessingTimes[i];
  Metrics.AvgProcessingTime := Sum div 100;

  // Calculer P95
  Move(ProcessingTimes, Sorted, SizeOf(ProcessingTimes));
  QuickSort(Sorted, 0, 99);
  Metrics.P95ProcessingTime := Sorted[94];  // 95e percentile

  // Max
  Metrics.MaxProcessingTime := Sorted[99];
end;

procedure UpdateMetrics(Success: Boolean; BytesProcessed: LongWord);  
begin
  if Success then
  begin
    Inc(TotalSuccess);
    Inc(LocalProcessingCount);
  end
  else
    Inc(TotalErrors);

  Metrics.SuccessRate := (TotalSuccess * 100.0) / (TotalSuccess + TotalErrors);
  Metrics.ErrorCount := TotalErrors;

  // Calculs additionnels...
end;

procedure PrintMetricsReport;  
begin
  WriteLn('=== EDGE METRICS REPORT ===');
  WriteLn('Processing Time:');
  WriteLn('  Average: ', Metrics.AvgProcessingTime, ' ms');
  WriteLn('  P95: ', Metrics.P95ProcessingTime, ' ms');
  WriteLn('  Max: ', Metrics.MaxProcessingTime, ' ms');
  WriteLn;
  WriteLn('Throughput:');
  WriteLn('  Events/sec: ', Metrics.EventsPerSecond:0:2);
  WriteLn('  Bytes/sec: ', Metrics.BytesPerSecond);
  WriteLn;
  WriteLn('Reliability:');
  WriteLn('  Success rate: ', Metrics.SuccessRate:0:2, '%');
  WriteLn('  Errors: ', Metrics.ErrorCount);
  WriteLn;
  WriteLn('Efficiency:');
  WriteLn('  Local processing: ', Metrics.LocalProcessingRate:0:1, '%');
  WriteLn('  Cloud offload: ', Metrics.CloudOffloadRate:0:1, '%');
  WriteLn('  Data reduction: ', Metrics.DataReductionRatio:0:2, 'x');
end;
```

## Simulateur Edge Computing

```pascal
// Simulateur pour tester architecture Edge
program EdgeSimulator;

{$MODE OBJFPC}

uses
  SysUtils, DateUtils, Math;

type
  TDevice = record
    ID: Word;
    Location: string[32];
    Sensors: array[0..3] of Real;
    LastUpdate: TDateTime;
  end;

  TSimulationParams = record
    DeviceCount: Word;
    EventRate: Word;         // événements/seconde
    NetworkLatency: Word;    // ms
    NetworkReliability: Byte; // 0-100%
    EdgeProcessingTime: Word; // ms
    CloudProcessingTime: Word; // ms
  end;

var
  Devices: array[0..99] of TDevice;
  Params: TSimulationParams;

  EventsProcessed: LongWord;
  EventsLocal: LongWord;
  EventsCloud: LongWord;
  EventsFailed: LongWord;

procedure InitSimulation;  
var
  i: Integer;
begin
  Params.DeviceCount := 50;
  Params.EventRate := 100;
  Params.NetworkLatency := 50;
  Params.NetworkReliability := 95;
  Params.EdgeProcessingTime := 10;
  Params.CloudProcessingTime := 100;

  for i := 0 to Params.DeviceCount - 1 do
  begin
    Devices[i].ID := i;
    Devices[i].Location := Format('Zone-%d', [i div 10]);
    Devices[i].LastUpdate := Now;
  end;

  WriteLn('Simulation initialisée');
  WriteLn('  Devices: ', Params.DeviceCount);
  WriteLn('  Event rate: ', Params.EventRate, ' events/sec');
end;

procedure SimulateEvent(var Device: TDevice);  
var
  ProcessLocally: Boolean;
  Success: Boolean;
  Latency: Word;
begin
  // Simuler lecture capteurs
  Device.Sensors[0] := 20.0 + Random * 10.0;  // Température
  Device.Sensors[1] := 40.0 + Random * 30.0;  // Humidité
  Device.Sensors[2] := 1000.0 + Random * 100.0; // CO2
  Device.LastUpdate := Now;

  // Décider traitement local ou cloud
  ProcessLocally := (Random(100) < 80);  // 80% local

  if ProcessLocally then
  begin
    // Traitement Edge
    Sleep(Params.EdgeProcessingTime);
    Inc(EventsLocal);
    Success := True;
  end
  else
  begin
    // Traitement Cloud
    Success := (Random(100) < Params.NetworkReliability);

    if Success then
    begin
      Sleep(Params.NetworkLatency + Params.CloudProcessingTime);
      Inc(EventsCloud);
    end
    else
      Inc(EventsFailed);
  end;

  if Success then
    Inc(EventsProcessed);
end;

procedure RunSimulation(Duration: Integer);  
var
  StartTime, ElapsedTime: TDateTime;
  EventInterval: Integer;
  i: Integer;
begin
  WriteLn('Démarrage simulation (', Duration, ' secondes)...');

  StartTime := Now;
  EventInterval := 1000 div Params.EventRate;  // ms entre événements

  repeat
    for i := 0 to Params.DeviceCount - 1 do
    begin
      if Random(Params.DeviceCount) < Params.EventRate then
        SimulateEvent(Devices[i]);
    end;

    Sleep(EventInterval);

    ElapsedTime := Now;
  until SecondsBetween(ElapsedTime, StartTime) >= Duration;

  PrintSimulationResults;
end;

procedure PrintSimulationResults;  
var
  Total: LongWord;
  LocalPct, CloudPct, FailPct: Real;
begin
  Total := EventsProcessed + EventsFailed;

  if Total > 0 then
  begin
    LocalPct := (EventsLocal * 100.0) / Total;
    CloudPct := (EventsCloud * 100.0) / Total;
    FailPct := (EventsFailed * 100.0) / Total;
  end
  else
  begin
    LocalPct := 0;
    CloudPct := 0;
    FailPct := 0;
  end;

  WriteLn;
  WriteLn('=== RÉSULTATS SIMULATION ===');
  WriteLn('Total événements: ', Total);
  WriteLn('  Traités localement: ', EventsLocal, ' (', LocalPct:0:1, '%)');
  WriteLn('  Envoyés au cloud: ', EventsCloud, ' (', CloudPct:0:1, '%)');
  WriteLn('  Échecs: ', EventsFailed, ' (', FailPct:0:1, '%)');
  WriteLn;
  WriteLn('Efficacité:');
  WriteLn('  Taux de succès: ', ((EventsProcessed * 100.0) / Total):0:2, '%');
  WriteLn('  Latence moyenne edge: ', Params.EdgeProcessingTime, ' ms');
  WriteLn('  Latence moyenne cloud: ',
          Params.NetworkLatency + Params.CloudProcessingTime, ' ms');
  WriteLn('  Réduction latence: ',
          (((Params.NetworkLatency + Params.CloudProcessingTime) -
            Params.EdgeProcessingTime) * 100) div
          (Params.NetworkLatency + Params.CloudProcessingTime), '%');
end;

begin
  Randomize;

  InitSimulation();
  RunSimulation(60);  // 60 secondes

  WriteLn;
  WriteLn('Simulation terminée.');
end.
```

## Ressources et outils

### Frameworks et bibliothèques

**Pour FreePascal** :
- **Synapse** : Communication réseau (HTTP, MQTT, TCP/UDP)
- **Brook Framework** : Applications web/REST
- **mORMot** : ORM, SOA, services
- **Indy** : Composants réseau
- **fphttpclient** : Client HTTP natif

**Plateformes Edge** :
- **AWS IoT Greengrass** : Edge computing AWS
- **Azure IoT Edge** : Edge Microsoft
- **Google Cloud IoT Edge** : Edge Google
- **EdgeX Foundry** : Framework open-source
- **KubeEdge** : Kubernetes pour Edge

### Matériel recommandé

| Niveau | Matériel | RAM | CPU | Usage |
|--------|----------|-----|-----|-------|
| **Micro** | ESP32 | 520 Ko | 240 MHz | Capteurs simples |
| **Petit** | STM32H7 | 1 Mo | 480 MHz | Contrôle industriel |
| **Moyen** | Raspberry Pi 4 | 4-8 Go | 1.5 GHz quad | Gateway, ML léger |
| **Grand** | NVIDIA Jetson Nano | 4 Go | 1.43 GHz quad + GPU | Vision, Deep Learning |
| **Serveur** | Intel NUC | 16-64 Go | i5/i7 | Edge datacenter |

### Protocoles IoT

```pascal
// Comparaison protocoles
const
  PROTOCOLS: array[0..4] of record
    Name: string;
    Transport: string;
    Overhead: string;
    QoS: Boolean;
    Usage: string;
  end = (
    (Name: 'MQTT'; Transport: 'TCP'; Overhead: 'Faible'; QoS: True;
     Usage: 'IoT général, telemetry'),
    (Name: 'CoAP'; Transport: 'UDP'; Overhead: 'Très faible'; QoS: False;
     Usage: 'Devices contraints'),
    (Name: 'HTTP/REST'; Transport: 'TCP'; Overhead: 'Élevé'; QoS: False;
     Usage: 'APIs web, intégrations'),
    (Name: 'WebSocket'; Transport: 'TCP'; Overhead: 'Moyen'; QoS: False;
     Usage: 'Temps réel bidirectionnel'),
    (Name: 'LoRaWAN'; Transport: 'LoRa'; Overhead: 'Très faible'; QoS: False;
     Usage: 'Longue portée, batterie')
  );
```

### Outils de développement

```bash
# Monitoring edge devices
mosquitto_sub -h edge-gateway -t 'sensors/#' -v

# Debug MQTT
mosquitto_pub -h localhost -t 'test' -m 'hello'

# Analyser trafic réseau
tcpdump -i eth0 port 1883

# Profiling performance
valgrind --tool=callgrind ./edge_app
```

## Projet final : Station météo distribuée

```pascal
program StationMeteoDistribuee;

{
  Architecture complète Edge Computing :

  [Capteurs DHT22/BME280] → [ESP32 Nodes]
            ↓
    [Raspberry Pi Gateway]
      - Agrégation
      - ML prévisions locales
      - Stockage SQLite
            ↓
    [Cloud MQTT Broker]
      - Stockage long terme
      - Dashboard web
      - Alertes email/SMS
}

// Point d'entrée selon la plateforme
{$IFDEF ESP32}
  // Code pour ESP32 (node capteur)
  {$I station_node.inc}
{$ENDIF}

{$IFDEF RPI}
  // Code pour Raspberry Pi (gateway)
  {$I station_gateway.inc}
{$ENDIF}

{$IFDEF CLOUD}
  // Code pour serveur cloud
  {$I station_cloud.inc}
{$ENDIF}

begin
  WriteLn('Station Météo Distribuée - Edge Computing');
  WriteLn('Plateforme: ', {$I %FPCTARGETCPU%}, '-', {$I %FPCTARGETOS%});

  InitializePlatform();
  RunMainLoop();
end.
```

## Conclusion finale

L'**Edge Computing** révolutionne l'IoT et les systèmes embarqués en déplaçant l'intelligence au plus près des données. Avec **FreePascal**, vous disposez d'un outil puissant pour créer des solutions Edge :

### Avantages de FreePascal pour Edge

✅ **Multi-plateforme** : Un seul code pour ESP32, Raspberry Pi, PC  
✅ **Performant** : Code natif optimisé  
✅ **Lisible** : Syntaxe claire vs C/C++  
✅ **Robuste** : Typage fort, moins de bugs  
✅ **Complet** : Bibliothèques pour réseau, DB, ML  
✅ **Gratuit** : Open source, pas de licence

### Prochaines étapes

1. **Commencer petit** : Un capteur + un Raspberry Pi
2. **Ajouter intelligence** : Filtres, agrégation, ML
3. **Distribuer** : Plusieurs nodes → gateway
4. **Optimiser** : Latence, batterie, bande passante
5. **Sécuriser** : Chiffrement, authentification
6. **Scaler** : Des dizaines puis centaines de devices

### Ressources pour aller plus loin

📚 **Livres** :
- "Edge Computing: A Primer" - Thomas Plunkett
- "Practical IoT Hacking" - Fotios Chantzis
- "Building the Internet of Things" - Maciej Kranz

🌐 **Sites** :
- Eclipse IoT : iot.eclipse.org
- EdgeX Foundry : edgexfoundry.org
- MQTT.org : mqtt.org
- FreePascal Wiki Embedded : wiki.freepascal.org

💻 **Projets exemples** :
- Home Assistant : Domotique open-source
- Node-RED : Flow-based programming
- Grafana : Visualisation métriques

L'Edge Computing n'est pas juste une tendance, c'est le **futur de l'IoT**. Avec FreePascal, vous avez tous les outils pour en faire partie.

**Bonne programmation Edge ! 🚀**

⏭️ [Intelligence Artificielle et Machine Learning](/15-intelligence-artificielle-machine-learning/README.md)
