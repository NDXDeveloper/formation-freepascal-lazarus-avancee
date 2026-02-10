🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.5 Capteurs et périphériques mobiles

## Introduction

Les appareils mobiles modernes sont équipés d'une multitude de capteurs et périphériques qui permettent d'enrichir considérablement l'expérience utilisateur. De l'accéléromètre qui détecte les mouvements au GPS qui localise l'appareil, en passant par la caméra, le microphone et bien d'autres, ces capteurs ouvrent un monde de possibilités pour les applications mobiles.

Dans ce chapitre, nous explorerons comment accéder et utiliser ces capteurs avec FreePascal et LAMW pour créer des applications mobiles intelligentes et interactives.

### Capteurs disponibles sur mobile

**Capteurs de mouvement** :
- Accéléromètre : Détecte l'accélération sur 3 axes
- Gyroscope : Mesure la rotation
- Magnétomètre : Boussole digitale
- Détecteur de gravité : Orientation de l'appareil

**Capteurs de position** :
- GPS : Localisation précise
- Réseau cellulaire : Localisation approximative
- WiFi : Localisation en intérieur

**Capteurs environnementaux** :
- Lumière ambiante : Ajustement automatique de la luminosité
- Proximité : Détecte la présence d'objets proches
- Température : Mesure de la température ambiante
- Pression atmosphérique : Altitude et météo
- Humidité : Taux d'humidité de l'air

**Périphériques d'entrée/sortie** :
- Caméra (avant et arrière)
- Microphone
- Haut-parleurs
- Vibreur
- Flash LED
- Écran tactile

## Architecture d'accès aux capteurs

### Modèle de programmation

```
┌─────────────────────────────────────┐
│   Application FreePascal            │
├─────────────────────────────────────┤
│                                     │
│  ┌───────────────────────────────┐  │
│  │  Code LAMW (Pascal)           │  │
│  │  - Enregistrement capteurs    │  │
│  │  - Callbacks événements       │  │
│  │  - Traitement données         │  │
│  └──────────────┬────────────────┘  │
│                 │                   │
│                 │ JNI               │
│                 │                   │
│  ┌──────────────▼────────────────┐  │
│  │  Android Sensor Framework     │  │
│  │  - SensorManager              │  │
│  │  - SensorEventListener        │  │
│  └──────────────┬────────────────┘  │
│                 │                   │
└─────────────────┼───────────────────┘
                  │
        ┌─────────▼─────────┐
        │  Capteurs Matériel│
        └───────────────────┘
```

### Principes de base

**1. Enregistrement du capteur**
```pascal
// Démarrer l'écoute d'un capteur
jForm.StartSensor(stAccelerometer);
```

**2. Réception des événements**
```pascal
// Callback appelé à chaque nouvelle mesure
procedure OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
begin
  // Traiter les données
end;
```

**3. Désenregistrement**
```pascal
// Arrêter l'écoute (économie de batterie)
jForm.StopSensor(stAccelerometer);
```

## Capteurs de mouvement

### Accéléromètre

L'accéléromètre mesure l'accélération linéaire sur trois axes en m/s².

**Axes de référence** :
```
        Y (haut de l'appareil)
        ↑
        |
        |
        |
        +--------→ X (droite de l'appareil)
       /
      /
     ↙ Z (vers l'utilisateur)
```

**Utilisation de base** :
```pascal
type
  TFormMain = class(jForm)
  private
    FAccelX, FAccelY, FAccelZ: Single;
    procedure UpdateAccelerometer;
  end;

procedure TFormMain.FormCreate(Sender: TObject);  
begin
  // Démarrer l'accéléromètre
  jForm.StartSensor(stAccelerometer);
end;

procedure TFormMain.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
begin
  if SensorType = stAccelerometer then
  begin
    FAccelX := X;
    FAccelY := Y;
    FAccelZ := Z;
    UpdateAccelerometer;
  end;
end;

procedure TFormMain.UpdateAccelerometer;  
begin
  jTextViewX.Text := Format('X: %.2f m/s²', [FAccelX]);
  jTextViewY.Text := Format('Y: %.2f m/s²', [FAccelY]);
  jTextViewZ.Text := Format('Z: %.2f m/s²', [FAccelZ]);
end;
```

**Détection de secousse (shake)** :
```pascal
const
  SHAKE_THRESHOLD = 15.0;  // Seuil de détection

var
  LastShakeTime: TDateTime;

procedure TFormMain.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
var
  acceleration: Single;
begin
  if SensorType = stAccelerometer then
  begin
    // Calculer l'accélération totale
    acceleration := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z));

    // Soustraire la gravité (9.81 m/s²)
    acceleration := Abs(acceleration - 9.81);

    // Détecter la secousse
    if acceleration > SHAKE_THRESHOLD then
    begin
      // Éviter les détections multiples
      if (Now - LastShakeTime) * 24 * 3600 > 1.0 then  // 1 seconde
      begin
        OnShakeDetected;
        LastShakeTime := Now;
      end;
    end;
  end;
end;

procedure TFormMain.OnShakeDetected;  
begin
  ShowMessage('Secousse détectée !');
  jForm.Vibrate(100);

  // Actions possibles :
  // - Actualiser le contenu
  // - Annuler la dernière action
  // - Mélanger une playlist
  // - etc.
end;
```

**Détection d'orientation (portrait/paysage)** :
```pascal
type
  TDeviceOrientation = (doPortrait, doLandscape, doFaceUp, doFaceDown);

function DetectOrientation(X, Y, Z: Single): TDeviceOrientation;  
begin
  // Z positif = écran vers le haut
  if Z > 8 then
    Exit(doFaceUp);

  // Z négatif = écran vers le bas
  if Z < -8 then
    Exit(doFaceDown);

  // Portrait : Y dominant
  if Abs(Y) > Abs(X) then
    Result := doPortrait
  else
    Result := doLandscape;
end;

procedure TFormMain.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
var
  orientation: TDeviceOrientation;
begin
  if SensorType = stAccelerometer then
  begin
    orientation := DetectOrientation(X, Y, Z);

    case orientation of
      doPortrait:
        jTextView1.Text := 'Portrait';
      doLandscape:
        jTextView1.Text := 'Paysage';
      doFaceUp:
        jTextView1.Text := 'Écran vers le haut';
      doFaceDown:
        jTextView1.Text := 'Écran vers le bas';
    end;
  end;
end;
```

**Jeu avec contrôle par inclinaison** :
```pascal
type
  TFormGame = class(jForm)
  private
    FBallX, FBallY: Single;
    FBallRadius: Integer;
    procedure UpdateBallPosition;
    procedure DrawGame;
  end;

procedure TFormGame.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
const
  SENSITIVITY = 2.0;
begin
  if SensorType = stAccelerometer then
  begin
    // Déplacer la balle selon l'inclinaison
    FBallX := FBallX + X * SENSITIVITY;
    FBallY := FBallY - Y * SENSITIVITY;  // Y inversé

    // Limiter aux bords de l'écran
    if FBallX < FBallRadius then FBallX := FBallRadius;
    if FBallX > jCanvas1.Width - FBallRadius then
      FBallX := jCanvas1.Width - FBallRadius;

    if FBallY < FBallRadius then FBallY := FBallRadius;
    if FBallY > jCanvas1.Height - FBallRadius then
      FBallY := jCanvas1.Height - FBallRadius;

    DrawGame;
  end;
end;

procedure TFormGame.DrawGame;  
begin
  jCanvas1.Clear(colbrWhite);
  jCanvas1.DrawCircle(Round(FBallX), Round(FBallY), FBallRadius, colbrBlue);
  jCanvas1.Invalidate;
end;
```

### Gyroscope

Le gyroscope mesure la vitesse de rotation autour des trois axes en rad/s.

```pascal
type
  TFormMain = class(jForm)
  private
    FRotationX, FRotationY, FRotationZ: Single;
    FAngleX, FAngleY, FAngleZ: Single;
    FLastTime: TDateTime;
  end;

procedure TFormMain.FormCreate(Sender: TObject);  
begin
  jForm.StartSensor(stGyroscope);
  FLastTime := Now;
  FAngleX := 0;
  FAngleY := 0;
  FAngleZ := 0;
end;

procedure TFormMain.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
var
  currentTime: TDateTime;
  deltaTime: Single;
begin
  if SensorType = stGyroscope then
  begin
    currentTime := Now;
    deltaTime := (currentTime - FLastTime) * 24 * 3600;  // en secondes
    FLastTime := currentTime;

    // Vitesses de rotation actuelles
    FRotationX := X;
    FRotationY := Y;
    FRotationZ := Z;

    // Intégrer pour obtenir les angles
    FAngleX := FAngleX + X * deltaTime;
    FAngleY := FAngleY + Y * deltaTime;
    FAngleZ := FAngleZ + Z * deltaTime;

    UpdateDisplay;
  end;
end;

procedure TFormMain.UpdateDisplay;  
begin
  jTextView1.Text := Format('Rotation X: %.2f rad/s', [FRotationX]);
  jTextView2.Text := Format('Rotation Y: %.2f rad/s', [FRotationY]);
  jTextView3.Text := Format('Rotation Z: %.2f rad/s', [FRotationZ]);

  jTextView4.Text := Format('Angle X: %.1f°', [FAngleX * 180 / Pi]);
  jTextView5.Text := Format('Angle Y: %.1f°', [FAngleY * 180 / Pi]);
  jTextView6.Text := Format('Angle Z: %.1f°', [FAngleZ * 180 / Pi]);
end;
```

**Détection de geste de rotation** :
```pascal
const
  ROTATION_THRESHOLD = 2.0;  // rad/s

procedure TFormMain.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
begin
  if SensorType = stGyroscope then
  begin
    // Rotation rapide autour de Z (rotation dans le plan de l'écran)
    if Abs(Z) > ROTATION_THRESHOLD then
    begin
      if Z > 0 then
        OnRotateClockwise
      else
        OnRotateCounterClockwise;
    end;
  end;
end;

procedure TFormMain.OnRotateClockwise;  
begin
  // Rotation horaire détectée
  // Exemple : faire pivoter une image
  jImageView1.Rotation := jImageView1.Rotation + 90;
end;

procedure TFormMain.OnRotateCounterClockwise;  
begin
  // Rotation antihoraire détectée
  jImageView1.Rotation := jImageView1.Rotation - 90;
end;
```

### Magnétomètre (Boussole)

Le magnétomètre détecte le champ magnétique terrestre et permet de créer une boussole.

```pascal
type
  TFormCompass = class(jForm)
  private
    FMagneticX, FMagneticY, FMagneticZ: Single;
    FAzimuth: Single;
    procedure UpdateCompass;
    procedure DrawCompass;
  end;

procedure TFormCompass.FormCreate(Sender: TObject);  
begin
  jForm.StartSensor(stMagnetometer);
end;

procedure TFormCompass.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
begin
  if SensorType = stMagnetometer then
  begin
    FMagneticX := X;
    FMagneticY := Y;
    FMagneticZ := Z;
    UpdateCompass;
  end;
end;

procedure TFormCompass.UpdateCompass;  
var
  azimuthDegrees: Single;
  direction: string;
begin
  // Calculer l'azimut (angle par rapport au nord magnétique)
  FAzimuth := ArcTan2(FMagneticY, FMagneticX);

  // Convertir en degrés
  azimuthDegrees := FAzimuth * 180 / Pi;

  // Normaliser entre 0 et 360
  if azimuthDegrees < 0 then
    azimuthDegrees := azimuthDegrees + 360;

  // Déterminer la direction cardinale
  if (azimuthDegrees >= 337.5) or (azimuthDegrees < 22.5) then
    direction := 'Nord'
  else if (azimuthDegrees >= 22.5) and (azimuthDegrees < 67.5) then
    direction := 'Nord-Est'
  else if (azimuthDegrees >= 67.5) and (azimuthDegrees < 112.5) then
    direction := 'Est'
  else if (azimuthDegrees >= 112.5) and (azimuthDegrees < 157.5) then
    direction := 'Sud-Est'
  else if (azimuthDegrees >= 157.5) and (azimuthDegrees < 202.5) then
    direction := 'Sud'
  else if (azimuthDegrees >= 202.5) and (azimuthDegrees < 247.5) then
    direction := 'Sud-Ouest'
  else if (azimuthDegrees >= 247.5) and (azimuthDegrees < 292.5) then
    direction := 'Ouest'
  else
    direction := 'Nord-Ouest';

  jTextViewAzimuth.Text := Format('%.1f°', [azimuthDegrees]);
  jTextViewDirection.Text := direction;

  DrawCompass;
end;

procedure TFormCompass.DrawCompass;  
var
  centerX, centerY: Integer;
  radius: Integer;
  needleEndX, needleEndY: Single;
begin
  centerX := jCanvas1.Width div 2;
  centerY := jCanvas1.Height div 2;
  radius := Min(jCanvas1.Width, jCanvas1.Height) div 3;

  // Effacer
  jCanvas1.Clear(colbrWhite);

  // Cercle de la boussole
  jCanvas1.DrawCircle(centerX, centerY, radius, colbrBlack);

  // Marques cardinales
  jCanvas1.DrawText('N', centerX - 10, centerY - radius - 20,
                    colbrBlack, 20);
  jCanvas1.DrawText('S', centerX - 10, centerY + radius + 10,
                    colbrBlack, 20);
  jCanvas1.DrawText('E', centerX + radius + 10, centerY,
                    colbrBlack, 20);
  jCanvas1.DrawText('O', centerX - radius - 30, centerY,
                    colbrBlack, 20);

  // Aiguille de la boussole (pointant vers le nord)
  needleEndX := centerX + radius * 0.8 * Sin(FAzimuth);
  needleEndY := centerY - radius * 0.8 * Cos(FAzimuth);

  jCanvas1.DrawLine(centerX, centerY,
                    Round(needleEndX), Round(needleEndY),
                    colbrRed, 4);

  jCanvas1.Invalidate;
end;
```

**Fusion accéléromètre + magnétomètre** :
```pascal
// Pour une boussole plus précise, combiner les deux capteurs
var
  AccelData: array[0..2] of Single;
  MagneticData: array[0..2] of Single;

procedure TFormCompass.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
begin
  case SensorType of
    stAccelerometer:
    begin
      AccelData[0] := X;
      AccelData[1] := Y;
      AccelData[2] := Z;
    end;

    stMagnetometer:
    begin
      MagneticData[0] := X;
      MagneticData[1] := Y;
      MagneticData[2] := Z;
    end;
  end;

  // Calculer l'orientation en combinant les données
  CalculateOrientation(AccelData, MagneticData);
end;

procedure CalculateOrientation(const Accel, Magnetic: array of Single);  
var
  rotationMatrix: array[0..8] of Single;
  orientationAngles: array[0..2] of Single;
begin
  // Calculer la matrice de rotation à partir des données
  // (Algorithme complexe, simplifié ici)

  // Android fournit SensorManager.getRotationMatrix() et
  // SensorManager.getOrientation()
  // En Pascal, on peut appeler ces méthodes via JNI

  GetRotationMatrixFromVectors(rotationMatrix, Accel, Magnetic);
  GetOrientationFromMatrix(orientationAngles, rotationMatrix);

  // orientationAngles[0] = azimut
  // orientationAngles[1] = pitch (inclinaison avant/arrière)
  // orientationAngles[2] = roll (roulis gauche/droite)

  FAzimuth := orientationAngles[0];
  UpdateCompass;
end;
```

## Capteurs de position

### GPS et localisation

La localisation est l'un des capteurs les plus utiles pour les applications mobiles.

**Permissions nécessaires** :
```xml
<!-- AndroidManifest.xml -->
<uses-permission android:name="android.permission.ACCESS_FINE_LOCATION" />
<uses-permission android:name="android.permission.ACCESS_COARSE_LOCATION" />
```

**Utilisation de base** :
```pascal
type
  TFormLocation = class(jForm)
  private
    FLatitude, FLongitude: Double;
    FAltitude: Double;
    FAccuracy: Single;
    FSpeed: Single;
    procedure UpdateLocationDisplay;
  end;

procedure TFormLocation.FormCreate(Sender: TObject);  
begin
  // Vérifier la permission
  if not jForm.HasPermission('android.permission.ACCESS_FINE_LOCATION') then
  begin
    jForm.RequestPermission('android.permission.ACCESS_FINE_LOCATION', 100);
    Exit;
  end;

  // Démarrer les mises à jour de localisation
  // Paramètre : intervalle minimal en millisecondes
  jForm.StartLocationUpdates(5000);  // Toutes les 5 secondes
end;

procedure TFormLocation.OnPermissionResult(Permission: string;
                                           GrantResult: TPermissionResult);
begin
  if (Permission = 'android.permission.ACCESS_FINE_LOCATION') and
     (GrantResult = prGranted) then
  begin
    jForm.StartLocationUpdates(5000);
  end
  else
  begin
    ShowMessage('La permission de localisation est nécessaire');
  end;
end;

procedure TFormLocation.OnLocationChanged(Latitude, Longitude: Double;
                                         Accuracy: Single);
begin
  FLatitude := Latitude;
  FLongitude := Longitude;
  FAccuracy := Accuracy;

  UpdateLocationDisplay;
end;

procedure TFormLocation.UpdateLocationDisplay;  
begin
  jTextViewLat.Text := Format('Latitude: %.6f°', [FLatitude]);
  jTextViewLon.Text := Format('Longitude: %.6f°', [FLongitude]);
  jTextViewAccuracy.Text := Format('Précision: %.0f m', [FAccuracy]);
end;
```

**Obtenir l'altitude et la vitesse** :
```pascal
procedure TFormLocation.OnLocationChangedExtended(
  Latitude, Longitude: Double;
  Altitude: Double;
  Accuracy: Single;
  Speed: Single;
  Bearing: Single;
  Timestamp: Int64);
begin
  FLatitude := Latitude;
  FLongitude := Longitude;
  FAltitude := Altitude;
  FAccuracy := Accuracy;
  FSpeed := Speed;  // en m/s

  jTextViewAlt.Text := Format('Altitude: %.0f m', [FAltitude]);
  jTextViewSpeed.Text := Format('Vitesse: %.1f km/h', [FSpeed * 3.6]);
end;
```

**Calculer la distance entre deux points** :
```pascal
function CalculateDistance(Lat1, Lon1, Lat2, Lon2: Double): Double;  
const
  EARTH_RADIUS = 6371000;  // Rayon de la Terre en mètres
var
  dLat, dLon: Double;
  a, c: Double;
begin
  // Formule de Haversine
  dLat := (Lat2 - Lat1) * Pi / 180;
  dLon := (Lon2 - Lon1) * Pi / 180;

  a := Sin(dLat / 2) * Sin(dLat / 2) +
       Cos(Lat1 * Pi / 180) * Cos(Lat2 * Pi / 180) *
       Sin(dLon / 2) * Sin(dLon / 2);

  c := 2 * ArcTan2(Sqrt(a), Sqrt(1 - a));

  Result := EARTH_RADIUS * c;
end;

// Utilisation
procedure TFormLocation.CalculerDistanceDestination;  
var
  destinationLat, destinationLon: Double;
  distance: Double;
begin
  // Coordonnées de destination (exemple : Tour Eiffel)
  destinationLat := 48.8584;
  destinationLon := 2.2945;

  distance := CalculateDistance(FLatitude, FLongitude,
                                destinationLat, destinationLon);

  jTextView1.Text := Format('Distance: %.0f m', [distance]);
end;
```

**Suivi de trajet (tracking)** :
```pascal
type
  TLocationPoint = record
    Latitude, Longitude: Double;
    Timestamp: TDateTime;
    Altitude: Double;
  end;

  TFormTracking = class(jForm)
  private
    FTrackPoints: array of TLocationPoint;
    FTotalDistance: Double;
    FStartTime: TDateTime;
    FIsTracking: Boolean;
    procedure AddTrackPoint(const Point: TLocationPoint);
    procedure CalculateTrackStatistics;
  end;

procedure TFormTracking.StartTracking;  
begin
  SetLength(FTrackPoints, 0);
  FTotalDistance := 0;
  FStartTime := Now;
  FIsTracking := True;

  jForm.StartLocationUpdates(1000);  // Toutes les secondes

  jButtonStart.Enabled := False;
  jButtonStop.Enabled := True;
end;

procedure TFormTracking.OnLocationChanged(Latitude, Longitude: Double;
                                         Accuracy: Single);
var
  point: TLocationPoint;
  distance: Double;
begin
  if not FIsTracking then
    Exit;

  point.Latitude := Latitude;
  point.Longitude := Longitude;
  point.Timestamp := Now;
  point.Altitude := FAltitude;

  // Calculer la distance depuis le dernier point
  if Length(FTrackPoints) > 0 then
  begin
    distance := CalculateDistance(
      FTrackPoints[High(FTrackPoints)].Latitude,
      FTrackPoints[High(FTrackPoints)].Longitude,
      Latitude, Longitude
    );

    FTotalDistance := FTotalDistance + distance;
  end;

  AddTrackPoint(point);
  CalculateTrackStatistics;
end;

procedure TFormTracking.AddTrackPoint(const Point: TLocationPoint);  
begin
  SetLength(FTrackPoints, Length(FTrackPoints) + 1);
  FTrackPoints[High(FTrackPoints)] := Point;
end;

procedure TFormTracking.CalculateTrackStatistics;  
var
  duration: Double;
  avgSpeed: Double;
begin
  duration := (Now - FStartTime) * 24 * 3600;  // en secondes

  if duration > 0 then
    avgSpeed := FTotalDistance / duration  // m/s
  else
    avgSpeed := 0;

  jTextViewDistance.Text := Format('Distance: %.2f km',
                                    [FTotalDistance / 1000]);
  jTextViewDuration.Text := Format('Durée: %d:%2.2d',
                                   [Trunc(duration / 60),
                                    Trunc(duration) mod 60]);
  jTextViewSpeed.Text := Format('Vitesse moy: %.1f km/h',
                                [avgSpeed * 3.6]);
end;

procedure TFormTracking.StopTracking;  
begin
  FIsTracking := False;
  jForm.StopLocationUpdates;

  jButtonStart.Enabled := True;
  jButtonStop.Enabled := False;

  // Sauvegarder le trajet
  SaveTrack;
end;

procedure TFormTracking.SaveTrack;  
var
  fs: TFileStream;
  i: Integer;
  gpxContent: string;
begin
  // Sauvegarder au format GPX (GPS Exchange Format)
  gpxContent := '<?xml version="1.0"?>' + LineEnding +
                '<gpx version="1.1">' + LineEnding +
                '<trk><trkseg>' + LineEnding;

  for i := 0 to High(FTrackPoints) do
  begin
    gpxContent := gpxContent +
      Format('<trkpt lat="%.6f" lon="%.6f">' + LineEnding +
             '  <ele>%.1f</ele>' + LineEnding +
             '  <time>%s</time>' + LineEnding +
             '</trkpt>' + LineEnding,
             [FTrackPoints[i].Latitude,
              FTrackPoints[i].Longitude,
              FTrackPoints[i].Altitude,
              FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
                           FTrackPoints[i].Timestamp)]);
  end;

  gpxContent := gpxContent +
                '</trkseg></trk>' + LineEnding +
                '</gpx>';

  fs := TFileStream.Create(
    jForm.GetExternalStoragePath + '/track_' +
    FormatDateTime('yyyymmdd_hhnnss', Now) + '.gpx',
    fmCreate
  );
  try
    fs.WriteBuffer(gpxContent[1], Length(gpxContent));
  finally
    fs.Free;
  end;
end;
```

## Capteurs environnementaux

### Capteur de lumière ambiante

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin
  jForm.StartSensor(stLight);
end;

procedure TFormMain.OnLightSensorChanged(Luminosity: Single);  
begin
  // Luminosity en lux
  jTextView1.Text := Format('Luminosité: %.0f lux', [Luminosity]);

  // Ajuster automatiquement la luminosité de l'écran
  if Luminosity < 10 then
  begin
    // Très sombre - luminosité minimale
    jForm.SetScreenBrightness(0.1);
  end
  else if Luminosity < 100 then
  begin
    // Sombre - luminosité faible
    jForm.SetScreenBrightness(0.3);
  end
  else if Luminosity < 1000 then
  begin
    // Normal - luminosité moyenne
    jForm.SetScreenBrightness(0.6);
  end
  else
  begin
    // Très lumineux - luminosité maximale
    jForm.SetScreenBrightness(1.0);
  end;
end;
```

### Capteur de proximité

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin
  jForm.StartSensor(stProximity);
end;

procedure TFormMain.OnProximitySensorChanged(Distance: Single; IsNear: Boolean);  
begin
  if IsNear then
  begin
    // Objet proche détecté
    jTextView1.Text := 'Objet proche';

    // Éteindre l'écran (utile pendant un appel)
    jForm.SetScreenOn(False);
  end
  else
  begin
    // Pas d'objet proche
    jTextView1.Text := 'Aucun objet proche';

    // Rallumer l'écran
    jForm.SetScreenOn(True);
  end;

  jTextView2.Text := Format('Distance: %.1f cm', [Distance]);
end;
```

**Application : Gestionnaire d'appels** :
```pascal
type
  TFormPhone = class(jForm)
  private
    FInCall: Boolean;
    FScreenWasOn: Boolean;
  end;

procedure TFormPhone.OnCallStarted;  
begin
  FInCall := True;
  FScreenWasOn := jForm.IsScreenOn;

  // Activer le capteur de proximité
  jForm.StartSensor(stProximity);
end;

procedure TFormPhone.OnProximitySensorChanged(Distance: Single; IsNear: Boolean);  
begin
  if not FInCall then
    Exit;

  if IsNear then
  begin
    // Téléphone près de l'oreille - éteindre l'écran
    jForm.SetScreenOn(False);
  end
  else
  begin
    // Téléphone éloigné - rallumer l'écran
    if FScreenWasOn then
      jForm.SetScreenOn(True);
  end;
end;

procedure TFormPhone.OnCallEnded;  
begin
  FInCall := False;
  jForm.StopSensor(stProximity);

  // Restaurer l'état de l'écran
  if FScreenWasOn then
    jForm.SetScreenOn(True);
end;
```

### Capteur de pression atmosphérique

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin
  jForm.StartSensor(stPressure);
end;

procedure TFormMain.OnPressureSensorChanged(Pressure: Single);  
var
  altitude: Single;
begin
  jTextView1.Text := Format('Pression: %.2f hPa', [Pressure]);

  // Calculer l'altitude approximative
  // Formule barométrique internationale
  altitude := 44330 * (1 - Power(Pressure / 1013.25, 1/5.255));

  jTextView2.Text := Format('Altitude estimée: %.0f m', [altitude]);

  // Déterminer les conditions météo
  if Pressure < 980 then
    jTextView3.Text := 'Temps orageux'
  else if Pressure < 1013 then
    jTextView3.Text := 'Temps variable'
  else if Pressure < 1030 then
    jTextView3.Text := 'Temps stable'
  else
    jTextView3.Text := 'Temps très stable';
end;
```

**Détection de changement d'altitude** :
```pascal
var
  ReferencePressure: Single;
  LastAltitude: Single;

procedure TFormMain.CalibrateAltitude;  
begin
  // Étalonner au niveau actuel
  ReferencePressure := CurrentPressure;
  LastAltitude := 0;
end;

procedure TFormMain.OnPressureSensorChanged(Pressure: Single);  
var
  currentAltitude: Single;
  altitudeChange: Single;
begin
  // Calculer l'altitude relative
  currentAltitude := 44330 * (1 - Power(Pressure / ReferencePressure, 1/5.255));

  // Détecter changements significatifs
  altitudeChange := currentAltitude - LastAltitude;

  if Abs(altitudeChange) > 5 then  // Changement de plus de 5 mètres
  begin
    if altitudeChange > 0 then
      jTextView1.Text := Format('Montée de %.0f m', [altitudeChange])
    else
      jTextView1.Text := Format('Descente de %.0f m', [Abs(altitudeChange)]);

    LastAltitude := currentAltitude;
  end;
end;
```

### Capteur d'humidité et température

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin
  jForm.StartSensor(stHumidity);
  jForm.StartSensor(stTemperature);
end;

procedure TFormMain.OnHumiditySensorChanged(Humidity: Single);  
begin
  jTextView1.Text := Format('Humidité: %.1f%%', [Humidity]);

  // Évaluer le confort
  if Humidity < 30 then
    jTextView2.Text := 'Air très sec'
  else if Humidity < 40 then
    jTextView2.Text := 'Air sec'
  else if Humidity < 60 then
    jTextView2.Text := 'Humidité confortable'
  else if Humidity < 70 then
    jTextView2.Text := 'Air humide'
  else
    jTextView2.Text := 'Air très humide';
end;

procedure TFormMain.OnTemperatureSensorChanged(Temperature: Single);  
begin
  jTextView3.Text := Format('Température: %.1f°C', [Temperature]);

  // Conseil selon la température
  if Temperature < 10 then
    jTextView4.Text := 'Il fait froid'
  else if Temperature < 18 then
    jTextView4.Text := 'Température fraîche'
  else if Temperature < 25 then
    jTextView4.Text := 'Température agréable'
  else if Temperature < 30 then
    jTextView4.Text := 'Il fait chaud'
  else
    jTextView4.Text := 'Il fait très chaud';
end;
```

**Calcul de l'indice de confort** :
```pascal
function CalculateComfortIndex(Temperature, Humidity: Single): string;  
var
  heatIndex: Single;
begin
  // Calculer l'indice de chaleur (Heat Index)
  if Temperature >= 27 then
  begin
    heatIndex := -8.784695 +
                 1.61139411 * Temperature +
                 2.338549 * Humidity -
                 0.14611605 * Temperature * Humidity -
                 0.012308094 * Temperature * Temperature -
                 0.016424828 * Humidity * Humidity +
                 0.002211732 * Temperature * Temperature * Humidity +
                 0.00072546 * Temperature * Humidity * Humidity -
                 0.000003582 * Temperature * Temperature * Humidity * Humidity;

    if heatIndex > 54 then
      Result := 'Danger extrême'
    else if heatIndex > 41 then
      Result := 'Danger'
    else if heatIndex > 32 then
      Result := 'Prudence extrême'
    else if heatIndex > 27 then
      Result := 'Prudence'
    else
      Result := 'Confortable';
  end
  else
    Result := 'Confortable';
end;

procedure TFormMain.UpdateComfortIndex;  
begin
  jTextView5.Text := 'Confort: ' +
    CalculateComfortIndex(CurrentTemperature, CurrentHumidity);
end;
```

## Caméra

### Prendre une photo

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin
  // Vérifier la permission
  if not jForm.HasPermission('android.permission.CAMERA') then
  begin
    jForm.RequestPermission('android.permission.CAMERA', 100);
  end;
end;

procedure TFormMain.jButtonTakePhotoClick(Sender: TObject);  
var
  photoPath: string;
begin
  if not jForm.HasPermission('android.permission.CAMERA') then
  begin
    ShowMessage('Permission caméra requise');
    Exit;
  end;

  // Définir le chemin de sauvegarde
  photoPath := jForm.GetExternalStoragePath + '/DCIM/photo_' +
               FormatDateTime('yyyymmdd_hhnnss', Now) + '.jpg';

  // Prendre la photo
  jForm.TakePicture(photoPath);
end;

procedure TFormMain.OnActivityResult(RequestCode, ResultCode: Integer;
                                     Data: jObject);
begin
  if (RequestCode = CAMERA_REQUEST_CODE) and (ResultCode = RESULT_OK) then
  begin
    // Photo prise avec succès
    jImageView1.LoadFromFile(LastPhotoPath);
    ShowMessage('Photo sauvegardée : ' + LastPhotoPath);
  end
  else if ResultCode = RESULT_CANCELED then
  begin
    ShowMessage('Photo annulée');
  end;
end;
```

### Caméra personnalisée avec aperçu

```pascal
type
  TFormCamera = class(jForm)
    jCameraView1: jCameraView;
    jButtonCapture: jButton;
    jButtonSwitch: jButton;
  end;

procedure TFormCamera.FormCreate(Sender: TObject);  
begin
  // Configuration de la vue caméra
  jCameraView1.Width := jForm.Width;
  jCameraView1.Height := jForm.Height - 100;
  jCameraView1.CameraFacing := cfBack;  // Caméra arrière
  jCameraView1.Start;
end;

procedure TFormCamera.jButtonCaptureClick(Sender: TObject);  
begin
  // Capturer l'image
  jCameraView1.TakePicture;
end;

procedure TFormCamera.jCameraView1PictureTaken(Sender: TObject;
                                               const FilePath: string);
begin
  ShowMessage('Photo sauvegardée : ' + FilePath);

  // Afficher un aperçu
  jImageView1.LoadFromFile(FilePath);
  jImageView1.Visible := True;

  // Redémarrer la caméra après 2 secondes
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(2000);
      TThread.Synchronize(nil,
        procedure
        begin
          jImageView1.Visible := False;
          jCameraView1.Start;
        end);
    end
  ).Start;
end;

procedure TFormCamera.jButtonSwitchClick(Sender: TObject);  
begin
  // Basculer entre caméra avant et arrière
  jCameraView1.Stop;

  if jCameraView1.CameraFacing = cfBack then
    jCameraView1.CameraFacing := cfFront
  else
    jCameraView1.CameraFacing := cfBack;

  jCameraView1.Start;
end;
```

### Flash et modes caméra

```pascal
procedure TFormCamera.ConfigureCamera;  
begin
  // Mode flash
  jCameraView1.FlashMode := fmAuto;  // Auto, On, Off, Torch

  // Mode focus
  jCameraView1.FocusMode := focusAuto;  // Auto, Continuous, Manual

  // Résolution
  jCameraView1.SetPictureSize(1920, 1080);  // Full HD

  // Qualité JPEG
  jCameraView1.JPEGQuality := 90;  // 0-100

  // Zoom
  jCameraView1.SetZoom(2.0);  // Zoom x2
end;

procedure TFormCamera.jButtonFlashClick(Sender: TObject);  
begin
  // Basculer le flash
  case jCameraView1.FlashMode of
    fmOff:
    begin
      jCameraView1.FlashMode := fmAuto;
      jButtonFlash.Text := 'Flash: Auto';
    end;
    fmAuto:
    begin
      jCameraView1.FlashMode := fmOn;
      jButtonFlash.Text := 'Flash: On';
    end;
    fmOn:
    begin
      jCameraView1.FlashMode := fmOff;
      jButtonFlash.Text := 'Flash: Off';
    end;
  end;
end;

// Zoom avec geste pinch
procedure TFormCamera.jCameraView1Pinch(Sender: TObject; Scale: Single);  
var
  newZoom: Single;
begin
  newZoom := jCameraView1.Zoom * Scale;

  // Limiter le zoom
  if newZoom < 1.0 then newZoom := 1.0;
  if newZoom > jCameraView1.MaxZoom then newZoom := jCameraView1.MaxZoom;

  jCameraView1.SetZoom(newZoom);
end;
```

### Scanner de codes QR / codes-barres

```pascal
type
  TFormScanner = class(jForm)
    jBarcodeScanner1: jBarcodeScanner;
  end;

procedure TFormScanner.FormCreate(Sender: TObject);  
begin
  if not jForm.HasPermission('android.permission.CAMERA') then
  begin
    jForm.RequestPermission('android.permission.CAMERA', 100);
  end;
end;

procedure TFormScanner.jButtonScanClick(Sender: TObject);  
begin
  // Démarrer le scanner
  jBarcodeScanner1.Scan;
end;

procedure TFormScanner.jBarcodeScanner1Result(Sender: TObject;
                                              const BarcodeData: string;
                                              BarcodeFormat: TBarcodeFormat);
begin
  case BarcodeFormat of
    bfQRCode:
    begin
      jTextView1.Text := 'QR Code détecté';
      TraiterQRCode(BarcodeData);
    end;

    bfEAN13:
    begin
      jTextView1.Text := 'Code-barres EAN13';
      RechercherProduit(BarcodeData);
    end;

    bfCode128:
    begin
      jTextView1.Text := 'Code 128';
      TraiterCode128(BarcodeData);
    end;

    bfDataMatrix:
    begin
      jTextView1.Text := 'DataMatrix';
      TraiterDataMatrix(BarcodeData);
    end;
  end;

  jTextView2.Text := 'Données: ' + BarcodeData;

  // Vibration de confirmation
  jForm.Vibrate(100);
end;

procedure TFormScanner.TraiterQRCode(const data: string);  
begin
  // Vérifier si c'est une URL
  if (Pos('http://', data) = 1) or (Pos('https://', data) = 1) then
  begin
    if MessageDlg('Ouvrir le lien ?',
                  data,
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      jForm.OpenURL(data);
    end;
  end
  // Vérifier si c'est du texte
  else if Pos('WIFI:', data) = 1 then
  begin
    // Configuration WiFi
    ParseWiFiQRCode(data);
  end
  else
  begin
    // Texte simple
    ShowMessage(data);
  end;
end;

procedure TFormScanner.RechercherProduit(const ean: string);  
var
  url: string;
begin
  // Rechercher le produit dans une base de données
  url := 'https://world.openfoodfacts.org/api/v0/product/' + ean + '.json';

  jHttpClient1.URL := url;
  jHttpClient1.Method := hmGET;
  jHttpClient1.Execute;
end;

procedure TFormScanner.ParseWiFiQRCode(const data: string);  
var
  ssid, password, security: string;
begin
  // Format : WIFI:S:<SSID>;T:<WPA|WEP|>;P:<password>;;

  ssid := ExtractBetween(data, 'S:', ';');
  security := ExtractBetween(data, 'T:', ';');
  password := ExtractBetween(data, 'P:', ';');

  ShowMessage(Format('WiFi: %s%sType: %s%sPassword: %s',
                     [ssid, LineEnding, security, LineEnding, password]));
end;
```

## Microphone et audio

### Enregistrement audio

```pascal
type
  TFormRecorder = class(jForm)
    jAudioRecorder1: jAudioRecorder;
    jButtonRecord: jButton;
    jButtonStop: jButton;
    jButtonPlay: jButton;
  private
    FRecordingPath: string;
    FIsRecording: Boolean;
  end;

procedure TFormRecorder.FormCreate(Sender: TObject);  
begin
  // Vérifier la permission
  if not jForm.HasPermission('android.permission.RECORD_AUDIO') then
  begin
    jForm.RequestPermission('android.permission.RECORD_AUDIO', 100);
  end;

  FIsRecording := False;
end;

procedure TFormRecorder.jButtonRecordClick(Sender: TObject);  
begin
  if not jForm.HasPermission('android.permission.RECORD_AUDIO') then
  begin
    ShowMessage('Permission microphone requise');
    Exit;
  end;

  // Définir le chemin de sauvegarde
  FRecordingPath := jForm.GetExternalStoragePath + '/Recordings/audio_' +
                    FormatDateTime('yyyymmdd_hhnnss', Now) + '.3gp';

  // Configuration
  jAudioRecorder1.AudioSource := asVoiceRecognition;  // ou asMic, asCamcorder
  jAudioRecorder1.OutputFormat := ofThreeGPP;  // ou ofAMR_NB, ofAMR_WB, ofMPEG_4
  jAudioRecorder1.AudioEncoder := aeAMR_NB;  // ou aeAAC, aeAMR_WB
  jAudioRecorder1.OutputFile := FRecordingPath;

  // Démarrer l'enregistrement
  jAudioRecorder1.Start;

  FIsRecording := True;
  jButtonRecord.Enabled := False;
  jButtonStop.Enabled := True;
  jTextView1.Text := 'Enregistrement en cours...';
end;

procedure TFormRecorder.jButtonStopClick(Sender: TObject);  
begin
  if FIsRecording then
  begin
    jAudioRecorder1.Stop;
    FIsRecording := False;

    jButtonRecord.Enabled := True;
    jButtonStop.Enabled := False;
    jButtonPlay.Enabled := True;

    jTextView1.Text := 'Enregistrement terminé';
    ShowMessage('Audio sauvegardé : ' + FRecordingPath);
  end;
end;

procedure TFormRecorder.jButtonPlayClick(Sender: TObject);  
begin
  if FileExists(FRecordingPath) then
  begin
    jMediaPlayer1.LoadFromFile(FRecordingPath);
    jMediaPlayer1.Play;
  end;
end;
```

### Détection de niveau sonore

```pascal
type
  TFormSoundMeter = class(jForm)
  private
    FAudioRecorder: jAudioRecorder;
    FTimer: TTimer;
    procedure UpdateSoundLevel;
  end;

procedure TFormSoundMeter.FormCreate(Sender: TObject);  
begin
  // Configuration pour mesurer le niveau sonore
  FAudioRecorder := jAudioRecorder.Create(Self);
  FAudioRecorder.AudioSource := asMic;
  FAudioRecorder.OutputFormat := ofThreeGPP;
  FAudioRecorder.AudioEncoder := aeAMR_NB;
  FAudioRecorder.OutputFile := '/dev/null';  // On ne sauvegarde pas

  FAudioRecorder.Start;

  // Timer pour mise à jour
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 100;  // Toutes les 100ms
  FTimer.OnTimer := @UpdateSoundLevel;
  FTimer.Enabled := True;
end;

procedure TFormSoundMeter.UpdateSoundLevel;  
var
  amplitude: Integer;
  decibels: Single;
begin
  // Obtenir l'amplitude maximale
  amplitude := FAudioRecorder.MaxAmplitude;

  if amplitude > 0 then
  begin
    // Convertir en décibels
    decibels := 20 * Log10(amplitude / 32767);

    jTextView1.Text := Format('Niveau: %.1f dB', [decibels]);

    // Afficher visuellement
    jProgressBar1.Max := 100;
    jProgressBar1.Progress := Round((decibels + 60) / 60 * 100);  // Normaliser

    // Changer la couleur selon le niveau
    if decibels < -40 then
      jProgressBar1.BarColor := colbrGreen  // Calme
    else if decibels < -20 then
      jProgressBar1.BarColor := colbrYellow  // Modéré
    else
      jProgressBar1.BarColor := colbrRed;  // Fort
  end;
end;

destructor TFormSoundMeter.Destroy;  
begin
  FTimer.Enabled := False;
  FAudioRecorder.Stop;
  inherited Destroy;
end;
```

### Reconnaissance vocale

```pascal
type
  TFormVoiceRecognition = class(jForm)
    jSpeechRecognizer1: jSpeechRecognizer;
  end;

procedure TFormVoiceRecognition.jButtonListenClick(Sender: TObject);  
begin
  // Démarrer la reconnaissance vocale
  jSpeechRecognizer1.Language := 'fr-FR';  // Français
  jSpeechRecognizer1.Prompt := 'Parlez maintenant...';
  jSpeechRecognizer1.MaxResults := 5;
  jSpeechRecognizer1.Start;
end;

procedure TFormVoiceRecognition.jSpeechRecognizer1Results(Sender: TObject;
                                                          Results: TStringList);
var
  i: Integer;
  bestMatch: string;
begin
  if Results.Count > 0 then
  begin
    // Meilleure correspondance
    bestMatch := Results[0];
    jTextView1.Text := 'Vous avez dit : ' + bestMatch;

    // Afficher toutes les correspondances
    jListView1.Clear;
    for i := 0 to Results.Count - 1 do
      jListView1.AddItem(Results[i], '');

    // Traiter la commande vocale
    TraiterCommandeVocale(bestMatch);
  end
  else
  begin
    ShowMessage('Aucune parole détectée');
  end;
end;

procedure TFormVoiceRecognition.TraiterCommandeVocale(const commande: string);  
var
  cmd: string;
begin
  cmd := LowerCase(commande);

  if Pos('météo', cmd) > 0 then
    AfficherMeteo
  else if Pos('heure', cmd) > 0 then
    ShowMessage('Il est ' + FormatDateTime('hh:nn', Now))
  else if Pos('ouvrir', cmd) > 0 then
  begin
    if Pos('caméra', cmd) > 0 then
      OuvrirCamera
    else if Pos('galerie', cmd) > 0 then
      OuvrirGalerie;
  end
  else if Pos('appeler', cmd) > 0 then
  begin
    // Extraire le nom ou numéro
    AppelerContact(cmd);
  end;
end;
```

### Synthèse vocale (Text-to-Speech)

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin
  // Initialiser le TTS
  jTextToSpeech1.Language := 'fr-FR';
  jTextToSpeech1.Pitch := 1.0;  // Hauteur de la voix (0.5 - 2.0)
  jTextToSpeech1.SpeechRate := 1.0;  // Vitesse (0.5 - 2.0)
end;

procedure TFormMain.jButtonSpeakClick(Sender: TObject);  
var
  text: string;
begin
  text := jEditText1.Text;

  if text <> '' then
  begin
    jTextToSpeech1.Speak(text);
  end;
end;

procedure TFormMain.jTextToSpeech1Done(Sender: TObject);  
begin
  ShowMessage('Lecture terminée');
end;

// Lire un texte long en plusieurs parties
procedure TFormMain.LireTexteLong(const texte: string);  
var
  parts: TStringList;
  i: Integer;
begin
  parts := TStringList.Create;
  try
    // Diviser en phrases
    SplitString(texte, '.', parts);

    for i := 0 to parts.Count - 1 do
    begin
      if Trim(parts[i]) <> '' then
      begin
        jTextToSpeech1.Speak(parts[i]);

        // Attendre la fin avant la phrase suivante
        while jTextToSpeech1.IsSpeaking do
        begin
          Application.ProcessMessages;
          Sleep(100);
        end;
      end;
    end;
  finally
    parts.Free;
  end;
end;
```

## Vibreur

### Vibrations simples

```pascal
procedure TFormMain.VibrerCourt;  
begin
  // Vibration courte (50ms)
  jForm.Vibrate(50);
end;

procedure TFormMain.VibrerMoyen;  
begin
  // Vibration moyenne (200ms)
  jForm.Vibrate(200);
end;

procedure TFormMain.VibrerLong;  
begin
  // Vibration longue (500ms)
  jForm.Vibrate(500);
end;
```

### Patterns de vibration

```pascal
procedure TFormMain.VibrerPattern;  
var
  pattern: array of Integer;
begin
  // Pattern : [délai, vibration, délai, vibration, ...]
  // Toutes les valeurs en millisecondes

  SetLength(pattern, 6);
  pattern[0] := 0;     // Démarrer immédiatement
  pattern[1] := 100;   // Vibrer 100ms
  pattern[2] := 50;    // Pause 50ms
  pattern[3] := 100;   // Vibrer 100ms
  pattern[4] := 50;    // Pause 50ms
  pattern[5] := 200;   // Vibrer 200ms

  jForm.VibratePattern(pattern, -1);  // -1 = ne pas répéter
end;

procedure TFormMain.VibrerNotification;  
var
  pattern: array of Integer;
begin
  // Pattern de notification
  SetLength(pattern, 4);
  pattern[0] := 0;
  pattern[1] := 50;
  pattern[2] := 100;
  pattern[3] := 50;

  jForm.VibratePattern(pattern, -1);
end;

procedure TFormMain.VibrerAlarme;  
var
  pattern: array of Integer;
begin
  // Pattern d'alarme (répétitif)
  SetLength(pattern, 4);
  pattern[0] := 0;
  pattern[1] := 300;   // Vibration longue
  pattern[2] := 200;   // Pause courte
  pattern[3] := 300;   // Vibration longue

  jForm.VibratePattern(pattern, 0);  // Répéter indéfiniment à partir de l'index 0
end;

procedure TFormMain.ArreterVibration;  
begin
  jForm.CancelVibration;
end;
```

### Feedback haptique contextuel

```pascal
procedure ProvideFeedback(feedbackType: TFeedbackType);  
begin
  case feedbackType of
    ftClick:
      jForm.Vibrate(10);  // Très court pour un clic

    ftSuccess:
    begin
      // Deux vibrations courtes
      jForm.Vibrate(50);
      Sleep(50);
      jForm.Vibrate(50);
    end;

    ftError:
    begin
      // Trois vibrations rapides
      jForm.Vibrate(50);
      Sleep(30);
      jForm.Vibrate(50);
      Sleep(30);
      jForm.Vibrate(50);
    end;

    ftWarning:
      jForm.Vibrate(150);  // Une vibration moyenne

    ftLongPress:
      jForm.Vibrate(70);  // Vibration un peu plus longue
  end;
end;

// Utilisation
procedure TFormMain.jButton1Click(Sender: TObject);  
begin
  ProvideFeedback(ftClick);
  // ... action du bouton
end;

procedure TFormMain.OnSaveSuccess;  
begin
  ProvideFeedback(ftSuccess);
  ShowMessage('Sauvegarde réussie');
end;

procedure TFormMain.OnError;  
begin
  ProvideFeedback(ftError);
  ShowMessage('Une erreur s''est produite');
end;
```

## Flash / Lampe torche

```pascal
procedure TFormMain.AllumerLampe;  
begin
  if jForm.HasCameraFlash then
  begin
    jForm.SetFlashMode(fmTorch);  // Mode lampe torche
    jButtonFlash.Text := 'Éteindre';
  end
  else
  begin
    ShowMessage('Pas de flash disponible sur cet appareil');
  end;
end;

procedure TFormMain.EteindreLampe;  
begin
  jForm.SetFlashMode(fmOff);
  jButtonFlash.Text := 'Allumer';
end;

procedure TFormMain.jButtonFlashClick(Sender: TObject);  
begin
  if jForm.IsFlashOn then
    EteindreLampe
  else
    AllumerLampe;
end;
```

**Lampe torche avec contrôle d'intensité** :
```pascal
procedure TFormMain.ConfigurerIntensiteLampe;  
begin
  // Certains appareils supportent différents niveaux
  jForm.SetFlashBrightness(0.5);  // 50% de luminosité
end;

procedure TFormMain.jTrackBarIntensityChange(Sender: TObject);  
var
  intensity: Single;
begin
  intensity := jTrackBarIntensity.Position / 100.0;  // 0.0 - 1.0

  if jForm.IsFlashOn then
  begin
    jForm.SetFlashBrightness(intensity);
  end;

  jTextViewIntensity.Text := Format('%d%%', [Round(intensity * 100)]);
end;
```

**Effet stroboscope** :
```pascal
type
  TFormStrobe = class(jForm)
  private
    FStrobeTimer: TTimer;
    FStrobeActive: Boolean;
  end;

procedure TFormStrobe.StartStrobe(frequency: Integer);  
begin
  FStrobeActive := True;

  FStrobeTimer := TTimer.Create(Self);
  FStrobeTimer.Interval := 1000 div frequency;  // Fréquence en Hz
  FStrobeTimer.OnTimer := @OnStrobeTimer;
  FStrobeTimer.Enabled := True;
end;

procedure TFormStrobe.OnStrobeTimer(Sender: TObject);  
begin
  if jForm.IsFlashOn then
    jForm.SetFlashMode(fmOff)
  else
    jForm.SetFlashMode(fmTorch);
end;

procedure TFormStrobe.StopStrobe;  
begin
  FStrobeActive := False;

  if Assigned(FStrobeTimer) then
  begin
    FStrobeTimer.Enabled := False;
    FStrobeTimer.Free;
  end;

  jForm.SetFlashMode(fmOff);
end;

procedure TFormStrobe.jButtonStrobeClick(Sender: TObject);  
begin
  if FStrobeActive then
    StopStrobe
  else
    StartStrobe(10);  // 10 Hz
end;
```

**Signal SOS** :
```pascal
procedure TFormMain.EnvoyerSignalSOS;  
begin
  // S : 3 flashs courts
  // O : 3 flashs longs
  // S : 3 flashs courts

  TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
    begin
      // S (3 courts)
      for i := 1 to 3 do
      begin
        TThread.Synchronize(nil, procedure begin jForm.SetFlashMode(fmTorch); end);
        Sleep(200);
        TThread.Synchronize(nil, procedure begin jForm.SetFlashMode(fmOff); end);
        Sleep(200);
      end;

      Sleep(400);

      // O (3 longs)
      for i := 1 to 3 do
      begin
        TThread.Synchronize(nil, procedure begin jForm.SetFlashMode(fmTorch); end);
        Sleep(600);
        TThread.Synchronize(nil, procedure begin jForm.SetFlashMode(fmOff); end);
        Sleep(200);
      end;

      Sleep(400);

      // S (3 courts)
      for i := 1 to 3 do
      begin
        TThread.Synchronize(nil, procedure begin jForm.SetFlashMode(fmTorch); end);
        Sleep(200);
        TThread.Synchronize(nil, procedure begin jForm.SetFlashMode(fmOff); end);
        Sleep(200);
      end;
    end
  ).Start;
end;
```

## Combinaison de capteurs

### Application boussole avec réalité augmentée

```pascal
type
  TFormARCompass = class(jForm)
  private
    // Données des capteurs
    FAccelData: array[0..2] of Single;
    FMagneticData: array[0..2] of Single;
    FLatitude, FLongitude: Double;

    // Données calculées
    FAzimuth, FPitch, FRoll: Single;

    // Points d'intérêt
    FPOIs: array of TPOI;

    procedure UpdateOrientation;
    procedure DrawARView;
  end;

type
  TPOI = record
    Name: string;
    Latitude, Longitude: Double;
    Distance: Double;
    Bearing: Double;
  end;

procedure TFormARCompass.FormCreate(Sender: TObject);  
begin
  // Démarrer tous les capteurs nécessaires
  jForm.StartSensor(stAccelerometer);
  jForm.StartSensor(stMagnetometer);
  jForm.StartLocationUpdates(5000);

  // Démarrer la caméra en arrière-plan
  jCameraView1.CameraFacing := cfBack;
  jCameraView1.Start;

  // Initialiser les points d'intérêt
  InitializePOIs;
end;

procedure TFormARCompass.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
begin
  case SensorType of
    stAccelerometer:
    begin
      FAccelData[0] := X;
      FAccelData[1] := Y;
      FAccelData[2] := Z;
    end;

    stMagnetometer:
    begin
      FMagneticData[0] := X;
      FMagneticData[1] := Y;
      FMagneticData[2] := Z;
    end;
  end;

  UpdateOrientation;
end;

procedure TFormARCompass.OnLocationChanged(Latitude, Longitude: Double;
                                          Accuracy: Single);
begin
  FLatitude := Latitude;
  FLongitude := Longitude;

  // Recalculer les distances et directions vers les POIs
  UpdatePOIDistances;
end;

procedure TFormARCompass.UpdateOrientation;  
var
  rotationMatrix: array[0..8] of Single;
  orientationAngles: array[0..2] of Single;
begin
  // Calculer l'orientation à partir de l'accéléromètre et magnétomètre
  GetRotationMatrixFromVectors(rotationMatrix, FAccelData, FMagneticData);
  GetOrientationFromMatrix(orientationAngles, rotationMatrix);

  FAzimuth := orientationAngles[0];
  FPitch := orientationAngles[1];
  FRoll := orientationAngles[2];

  DrawARView;
end;

procedure TFormARCompass.DrawARView;  
var
  i: Integer;
  poi: TPOI;
  screenX, screenY: Integer;
  angleDiff: Single;
begin
  // Dessiner sur le canvas en superposition de la caméra
  jCanvas1.Clear(colbrTransparent);

  for i := 0 to High(FPOIs) do
  begin
    poi := FPOIs[i];

    // Calculer la différence d'angle entre la direction actuelle et le POI
    angleDiff := poi.Bearing - FAzimuth;

    // Normaliser l'angle entre -180 et 180
    while angleDiff > Pi do angleDiff := angleDiff - 2 * Pi;
    while angleDiff < -Pi do angleDiff := angleDiff + 2 * Pi;

    // Vérifier si le POI est dans le champ de vision (environ 60°)
    if Abs(angleDiff) < Pi / 3 then
    begin
      // Calculer la position à l'écran
      screenX := jCanvas1.Width div 2 + Round(angleDiff / (Pi / 3) * jCanvas1.Width / 2);
      screenY := jCanvas1.Height div 2;

      // Dessiner le POI
      DrawPOI(poi, screenX, screenY);
    end;
  end;

  jCanvas1.Invalidate;
end;

procedure TFormARCompass.DrawPOI(const poi: TPOI; X, Y: Integer);  
begin
  // Icône du POI
  jCanvas1.DrawCircle(X, Y, 20, colbrBlue);

  // Nom et distance
  jCanvas1.DrawText(poi.Name, X - 50, Y - 40, colbrWhite, 14);
  jCanvas1.DrawText(Format('%.1f km', [poi.Distance / 1000]),
                    X - 50, Y - 25, colbrWhite, 12);
end;

procedure TFormARCompass.UpdatePOIDistances;  
var
  i: Integer;
begin
  for i := 0 to High(FPOIs) do
  begin
    // Calculer la distance
    FPOIs[i].Distance := CalculateDistance(
      FLatitude, FLongitude,
      FPOIs[i].Latitude, FPOIs[i].Longitude
    );

    // Calculer le bearing (direction)
    FPOIs[i].Bearing := CalculateBearing(
      FLatitude, FLongitude,
      FPOIs[i].Latitude, FPOIs[i].Longitude
    );
  end;
end;

function CalculateBearing(Lat1, Lon1, Lat2, Lon2: Double): Double;  
var
  dLon: Double;
  y, x: Double;
begin
  dLon := (Lon2 - Lon1) * Pi / 180;

  y := Sin(dLon) * Cos(Lat2 * Pi / 180);
  x := Cos(Lat1 * Pi / 180) * Sin(Lat2 * Pi / 180) -
       Sin(Lat1 * Pi / 180) * Cos(Lat2 * Pi / 180) * Cos(dLon);

  Result := ArcTan2(y, x);
end;
```

### Détecteur de chutes (pour personnes âgées)

```pascal
type
  TFormFallDetector = class(jForm)
  private
    FAccelHistory: array[0..9] of Single;  // Historique des 10 dernières mesures
    FHistoryIndex: Integer;
    FFallDetected: Boolean;
    FEmergencyContact: string;

    function DetectFall: Boolean;
    procedure SendEmergencyAlert;
  end;

const
  FALL_THRESHOLD = 25.0;  // Seuil d'accélération pour détecter une chute
  FREE_FALL_THRESHOLD = 2.0;  // Seuil de chute libre

procedure TFormFallDetector.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
var
  totalAccel: Single;
  isFalling: Boolean;
begin
  if SensorType = stAccelerometer then
  begin
    // Calculer l'accélération totale
    totalAccel := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z));

    // Ajouter à l'historique
    FAccelHistory[FHistoryIndex] := totalAccel;
    FHistoryIndex := (FHistoryIndex + 1) mod Length(FAccelHistory);

    // Détecter une chute
    isFalling := DetectFall;

    if isFalling and not FFallDetected then
    begin
      FFallDetected := True;
      OnFallDetected;
    end;
  end;
end;

function TFormFallDetector.DetectFall: Boolean;  
var
  i: Integer;
  maxAccel, minAccel: Single;
begin
  Result := False;

  maxAccel := 0;
  minAccel := 999;

  // Analyser l'historique
  for i := 0 to High(FAccelHistory) do
  begin
    if FAccelHistory[i] > maxAccel then
      maxAccel := FAccelHistory[i];
    if FAccelHistory[i] < minAccel then
      minAccel := FAccelHistory[i];
  end;

  // Détecter :
  // 1. Chute libre (accélération très faible)
  // 2. Suivie d'un impact (accélération très forte)
  if (minAccel < FREE_FALL_THRESHOLD) and (maxAccel > FALL_THRESHOLD) then
    Result := True;
end;

procedure TFormFallDetector.OnFallDetected;  
begin
  // Vibration d'alerte
  jForm.Vibrate(1000);

  // Son d'alerte
  jMediaPlayer1.LoadFromAssets('alert.mp3');
  jMediaPlayer1.Play;

  // Afficher un dialogue de confirmation
  jDialogYN1.Title := 'ALERTE CHUTE';
  jDialogYN1.Msg := 'Une chute a été détectée. Êtes-vous en danger ?';
  jDialogYN1.Yes := 'Oui, alerter';
  jDialogYN1.No := 'Non, fausse alerte';
  jDialogYN1.Show;

  // Lancer un compte à rebours
  StartEmergencyCountdown(30);  // 30 secondes pour annuler
end;

procedure TFormFallDetector.StartEmergencyCountdown(seconds: Integer);  
var
  countdown: Integer;
begin
  countdown := seconds;

  TThread.CreateAnonymousThread(
    procedure
    begin
      while (countdown > 0) and FFallDetected do
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            jTextView1.Text := Format('Alerte dans %d secondes...', [countdown]);
          end);

        Sleep(1000);
        Dec(countdown);
      end;

      // Si toujours en état d'alerte, envoyer
      if FFallDetected then
      begin
        TThread.Synchronize(nil, procedure begin SendEmergencyAlert; end);
      end;
    end
  ).Start;
end;

procedure TFormFallDetector.SendEmergencyAlert;  
var
  message: string;
begin
  // Envoyer SMS d'urgence
  message := Format('ALERTE CHUTE détectée ! Position : %.6f, %.6f',
                    [FLatitude, FLongitude]);

  jForm.SendSMS(FEmergencyContact, message);

  // Appeler le contact d'urgence
  jForm.CallPhone(FEmergencyContact);

  // Envoyer notification avec localisation
  jNotificationManager1.Title := 'ALERTE CHUTE';
  jNotificationManager1.Message := 'Contact d''urgence alerté';
  jNotificationManager1.Notify(999);
end;

procedure TFormFallDetector.jDialogYN1Click(Sender: TObject; Button: TDlgResponse);  
begin
  if Button = dlgNo then
  begin
    // Fausse alerte - annuler
    FFallDetected := False;
    jTextView1.Text := 'Alerte annulée';
  end;
  // Si dlgYes, laisser le countdown continuer
end;
```

### Podomètre (compteur de pas)

```pascal
type
  TFormPedometer = class(jForm)
  private
    FStepCount: Integer;
    FLastStepTime: TDateTime;
    FCaloriesBurned: Single;
    FDistanceWalked: Single;  // en mètres

    FAccelMagnitude: Single;
    FLastAccelMagnitude: Single;
    FStepThreshold: Single;

    procedure DetectStep;
    procedure UpdateStatistics;
  end;

const
  STEP_THRESHOLD = 12.0;  // Seuil de détection de pas
  STEP_DELAY = 0.3;  // Délai minimum entre 2 pas (en secondes)
  STRIDE_LENGTH = 0.7;  // Longueur de foulée moyenne en mètres
  CALORIES_PER_STEP = 0.04;  // Calories par pas

procedure TFormPedometer.FormCreate(Sender: TObject);  
begin
  jForm.StartSensor(stAccelerometer);

  FStepCount := 0;
  FLastStepTime := Now;
  FStepThreshold := STEP_THRESHOLD;
end;

procedure TFormPedometer.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
begin
  if SensorType = stAccelerometer then
  begin
    FLastAccelMagnitude := FAccelMagnitude;
    FAccelMagnitude := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z));

    DetectStep;
  end;
end;

procedure TFormPedometer.DetectStep;  
var
  timeSinceLastStep: Double;
begin
  timeSinceLastStep := (Now - FLastStepTime) * 24 * 3600;

  // Détecter un pic d'accélération
  if (FAccelMagnitude > FStepThreshold) and
     (FLastAccelMagnitude < FStepThreshold) and
     (timeSinceLastStep > STEP_DELAY) then
  begin
    Inc(FStepCount);
    FLastStepTime := Now;

    UpdateStatistics;

    // Feedback haptique subtil
    if FStepCount mod 10 = 0 then  // Toutes les 10 pas
      jForm.Vibrate(20);
  end;
end;

procedure TFormPedometer.UpdateStatistics;  
begin
  // Calculer la distance
  FDistanceWalked := FStepCount * STRIDE_LENGTH;

  // Calculer les calories
  FCaloriesBurned := FStepCount * CALORIES_PER_STEP;

  // Afficher
  jTextViewSteps.Text := Format('%d pas', [FStepCount]);
  jTextViewDistance.Text := Format('%.2f km', [FDistanceWalked / 1000]);
  jTextViewCalories.Text := Format('%.0f kcal', [FCaloriesBurned]);

  // Objectif quotidien (10000 pas)
  jProgressBarGoal.Max := 10000;
  jProgressBarGoal.Progress := FStepCount;

  if FStepCount >= 10000 then
  begin
    if not FGoalReached then
    begin
      FGoalReached := True;
      OnGoalReached;
    end;
  end;
end;

procedure TFormPedometer.OnGoalReached;  
begin
  ShowMessage('Félicitations ! Vous avez atteint votre objectif de 10000 pas !');
  jForm.Vibrate(500);

  // Notification
  jNotificationManager1.Title := 'Objectif atteint !';
  jNotificationManager1.Message := '10000 pas aujourd''hui';
  jNotificationManager1.Notify(1);
end;

procedure TFormPedometer.ResetDailyStats;  
begin
  FStepCount := 0;
  FDistanceWalked := 0;
  FCaloriesBurned := 0;
  FGoalReached := False;
  UpdateStatistics;
end;
```

## Bonnes pratiques

### Gestion de l'énergie

```pascal
// Arrêter les capteurs quand l'app est en arrière-plan
procedure TFormMain.OnPause;  
begin
  // Arrêter les capteurs gourmands
  jForm.StopSensor(stAccelerometer);
  jForm.StopSensor(stGyroscope);
  jForm.StopSensor(stMagnetometer);
  jForm.StopLocationUpdates;

  // Sauvegarder l'état
  SaveState;
end;

procedure TFormMain.OnResume;  
begin
  // Redémarrer les capteurs nécessaires
  jForm.StartSensor(stAccelerometer);
  RestoreState;
end;
```

### Vérification de disponibilité

```pascal
function CheckSensorAvailability: Boolean;  
var
  availability: string;
begin
  Result := True;
  availability := '';

  if not jForm.HasSensor(stAccelerometer) then
  begin
    availability := availability + 'Accéléromètre non disponible' + LineEnding;
    Result := False;
  end;

  if not jForm.HasSensor(stGyroscope) then
  begin
    availability := availability + 'Gyroscope non disponible' + LineEnding;
    Result := False;
  end;

  if not jForm.HasSensor(stMagnetometer) then
  begin
    availability := availability + 'Magnétomètre non disponible' + LineEnding;
    Result := False;
  end;

  if not jForm.HasCameraFlash then
    availability := availability + 'Flash non disponible' + LineEnding;

  if availability <> '' then
    ShowMessage('Capteurs manquants:' + LineEnding + availability);
end;
```

### Filtrage des données bruitées

```pascal
type
  TLowPassFilter = class
  private
    FAlpha: Single;
    FFilteredValues: array[0..2] of Single;
  public
    constructor Create(AAlpha: Single);
    procedure Filter(var X, Y, Z: Single);
  end;

constructor TLowPassFilter.Create(AAlpha: Single);  
begin
  inherited Create;
  FAlpha := AAlpha;  // 0.0 - 1.0, plus petit = plus de filtrage
  FFilteredValues[0] := 0;
  FFilteredValues[1] := 0;
  FFilteredValues[2] := 0;
end;

procedure TLowPassFilter.Filter(var X, Y, Z: Single);  
begin
  // Filtre passe-bas simple
  FFilteredValues[0] := FAlpha * X + (1 - FAlpha) * FFilteredValues[0];
  FFilteredValues[1] := FAlpha * Y + (1 - FAlpha) * FFilteredValues[1];
  FFilteredValues[2] := FAlpha * Z + (1 - FAlpha) * FFilteredValues[2];

  X := FFilteredValues[0];
  Y := FFilteredValues[1];
  Z := FFilteredValues[2];
end;

// Utilisation
var
  AccelFilter: TLowPassFilter;

procedure TFormMain.FormCreate(Sender: TObject);  
begin
  AccelFilter := TLowPassFilter.Create(0.1);  // Filtrage fort
  jForm.StartSensor(stAccelerometer);
end;

procedure TFormMain.OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
begin
  if SensorType = stAccelerometer then
  begin
    // Filtrer les données
    AccelFilter.Filter(X, Y, Z);

    // Utiliser les valeurs filtrées
    UpdateDisplay(X, Y, Z);
  end;
end;
```

### Gestion des permissions runtime

```pascal
procedure TFormMain.RequestAllSensorsPermissions;  
var
  permissions: TStringList;
begin
  permissions := TStringList.Create;
  try
    // Localisation
    if not jForm.HasPermission('android.permission.ACCESS_FINE_LOCATION') then
      permissions.Add('android.permission.ACCESS_FINE_LOCATION');

    // Caméra
    if not jForm.HasPermission('android.permission.CAMERA') then
      permissions.Add('android.permission.CAMERA');

    // Microphone
    if not jForm.HasPermission('android.permission.RECORD_AUDIO') then
      permissions.Add('android.permission.RECORD_AUDIO');

    // Stockage
    if not jForm.HasPermission('android.permission.WRITE_EXTERNAL_STORAGE') then
      permissions.Add('android.permission.WRITE_EXTERNAL_STORAGE');

    if permissions.Count > 0 then
      jForm.RequestPermissions(permissions, 1000);
  finally
    permissions.Free;
  end;
end;

procedure TFormMain.OnMultiplePermissionsResult(Permissions: TStringList;
                                                Results: TPermissionResults);
var
  i: Integer;
  allGranted: Boolean;
  deniedPermissions: string;
begin
  allGranted := True;
  deniedPermissions := '';

  for i := 0 to Permissions.Count - 1 do
  begin
    if Results[i] <> prGranted then
    begin
      allGranted := False;
      deniedPermissions := deniedPermissions + Permissions[i] + LineEnding;
    end;
  end;

  if allGranted then
  begin
    InitializeSensors;
  end
  else
  begin
    ShowMessage('Permissions refusées:' + LineEnding + deniedPermissions);
  end;
end;
```

## Conclusion

Les capteurs et périphériques mobiles offrent des possibilités infinies pour créer des applications riches et interactives. En maîtrisant leur utilisation avec FreePascal et LAMW, vous pouvez développer :

### Applications possibles

**Santé et fitness** :
- Compteurs de pas et trackers d'activité
- Détecteurs de chutes pour personnes âgées
- Moniteurs de posture
- Applications de méditation (avec capteurs de respiration)

**Navigation et localisation** :
- Boussoles augmentées
- Guides touristiques avec réalité augmentée
- Trackers GPS
- Applications de géocaching

**Utilitaires** :
- Lampes torche avancées
- Niveaux à bulle
- Détecteurs de métaux (magnétomètre)
- Sonomètres

**Jeux** :
- Jeux contrôlés par inclinaison
- Jeux de réalité augmentée
- Jeux utilisant la caméra
- Expériences immersives

**Sécurité** :
- Systèmes d'alerte personnelle
- Détecteurs de mouvement
- Applications antivol
- Surveillance environnementale

### Points clés à retenir

✓ **Économie d'énergie** : Arrêtez les capteurs quand ils ne sont pas nécessaires  
✓ **Permissions** : Demandez uniquement ce dont vous avez besoin  
✓ **Filtrage** : Les données brutes sont souvent bruitées  
✓ **Combinaison** : Les capteurs sont plus puissants ensemble  
✓ **Tests réels** : Testez toujours sur de vrais appareils  
✓ **Feedback utilisateur** : Vibrations et sons pour confirmer les actions  
✓ **Gestion des erreurs** : Tous les appareils n'ont pas tous les capteurs

### Ressources complémentaires

- Documentation Android Sensor : developer.android.com/guide/topics/sensors
- LAMW Examples : Dossier sensors dans les exemples
- Forum Lazarus : Section Mobile Development

Avec une bonne maîtrise des capteurs mobiles, vos applications FreePascal peuvent rivaliser avec les meilleures applications natives en termes de fonctionnalités et d'expérience utilisateur !

**Bon développement avec les capteurs mobiles ! 📱🔬**

⏭️ [iOS avec FreePascal (expérimental)](/13-developpement-mobile-embarque/06-ios-freepascal-experimental.md)
