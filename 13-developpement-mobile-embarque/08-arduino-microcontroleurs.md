🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.8 Arduino et microcontrôleurs

## Introduction

FreePascal permet de programmer des **microcontrôleurs** comme l'Arduino, offrant une alternative au langage C/C++ traditionnellement utilisé. Cette approche combine la simplicité et la lisibilité de Pascal avec la puissance de la programmation embarquée.

Les microcontrôleurs sont des ordinateurs miniatures intégrés dans un seul circuit, conçus pour exécuter des tâches spécifiques. Ils sont au cœur de millions d'objets connectés, de robots, et de systèmes embarqués.

### Pourquoi utiliser FreePascal pour Arduino ?

**Avantages** :

✅ **Syntaxe claire et lisible** : Plus accessible que C/C++ pour les débutants  
✅ **Typage fort** : Moins d'erreurs de programmation  
✅ **Pas de pointeurs complexes** : Plus sûr et plus simple  
✅ **Code structuré** : Encourages les bonnes pratiques  
✅ **Performance** : Code natif compilé, aussi rapide que C  
✅ **Portabilité** : Le même code peut tourner sur PC et microcontrôleur

**Limitations** :

⚠️ **Bibliothèques limitées** : Moins de bibliothèques que l'écosystème Arduino C++  
⚠️ **Communauté plus restreinte** : Moins de ressources et d'exemples  
⚠️ **Documentation** : Moins abondante que pour Arduino IDE officiel  
⚠️ **Outils** : Configuration plus technique qu'Arduino IDE

## Architectures supportées

FreePascal supporte plusieurs architectures de microcontrôleurs :

| Architecture | Exemples | Support FPC | Difficulté |
|--------------|----------|-------------|------------|
| **AVR** | Arduino Uno, Mega, Nano | ✅ Excellent | ⭐⭐ Moyen |
| **ARM Cortex-M** | STM32, Teensy, Arduino Due | ✅ Bon | ⭐⭐⭐ Avancé |
| **ESP32** | ESP32-DevKit, ESP-WROOM | ⚠️ Expérimental | ⭐⭐⭐⭐ Expert |
| **PIC** | Microchip PIC16/18 | ⚠️ Limité | ⭐⭐⭐⭐ Expert |

> 💡 **Recommandation pour débuter** : Commencez avec un **Arduino Uno** (AVR), le support est excellent et la documentation abondante.

## Arduino AVR avec FreePascal

### Matériel requis

**Arduino compatible AVR** :
- **Arduino Uno** (ATmega328P) - Recommandé pour débuter
- **Arduino Nano** (ATmega328P) - Version compacte
- **Arduino Mega 2560** (ATmega2560) - Plus de mémoire et de broches

**Accessoires** :
- Câble USB A vers B (pour Uno/Mega) ou Mini-USB (pour Nano)
- Breadboard (plaque d'essai)
- LEDs et résistances (220Ω)
- Boutons poussoirs
- Capteurs divers (température, lumière, etc.)

### Installation de l'environnement

#### Sur Windows

```batch
# 1. Télécharger et installer FreePascal
# https://www.freepascal.org/download.html
# Version 3.2.2 ou supérieure

# 2. Télécharger AVRDude (outil de programmation)
# https://github.com/avrdudes/avrdude/releases
# Extraire dans C:\avrdude\

# 3. Ajouter au PATH système
# Panneau de configuration → Système → Variables d'environnement
# Ajouter : C:\FPC\3.2.2\bin\i386-win32
#          C:\avrdude\

# 4. Vérifier l'installation
fpc -version
avrdude -v
```

#### Sur Ubuntu/Linux

```bash
# 1. Installer FreePascal
sudo apt update
sudo apt install fpc -y

# 2. Installer AVRDude
sudo apt install avrdude -y

# 3. Ajouter l'utilisateur au groupe dialout (accès port série)
sudo usermod -a -G dialout $USER

# 4. Se déconnecter/reconnecter pour appliquer les changements

# 5. Vérifier l'installation
fpc -version
avrdude -v
```

### Configuration du compilateur pour AVR

Créer un fichier de configuration `fpc-avr.cfg` :

```ini
# Configuration FreePascal pour Arduino AVR

# Architecture cible
-Pavr
-Tembedded

# Microcontrôleur (Arduino Uno : ATmega328P)
-CpATMEGA328P

# Optimisations pour l'espace mémoire
-Os
-Xs

# Options de linkage
-WpATMEGA328P

# Répertoires des unités AVR
-Fu$FPCDIR/units/$fpctarget
-Fu$FPCDIR/units/$fpctarget/rtl

# Format de sortie
-Mobjfpc
-Sh

# Désactiver les features non supportées
-d-CPUAVR_16_REGS
```

## Premier programme : Clignotement de LED

### Le classique "Blink"

Créer un fichier `blink.pas` :

```pascal
program Blink;

{$mode objfpc}

uses
  avr_api;

const
  LED_PIN = 13;  // LED intégrée sur l'Arduino Uno

begin
  // Configurer la broche LED en sortie
  pinMode(LED_PIN, OUTPUT);

  // Boucle infinie
  repeat
    digitalWrite(LED_PIN, HIGH);  // Allumer la LED
    delay(1000);                   // Attendre 1 seconde (1000 ms)

    digitalWrite(LED_PIN, LOW);   // Éteindre la LED
    delay(1000);                   // Attendre 1 seconde
  until false;
end.
```

### Compilation pour Arduino Uno

```bash
# Compiler le programme
fpc -Pavr -CpATMEGA328P -Tembedded -Os blink.pas

# Cela génère un fichier blink.hex

# Identifier le port série (Windows)
# Gestionnaire de périphériques → Ports (COM et LPT) → Arduino Uno (COMx)

# Identifier le port série (Linux)
ls /dev/ttyUSB* /dev/ttyACM*
# Généralement /dev/ttyACM0 ou /dev/ttyUSB0

# Téléverser sur l'Arduino (Windows - remplacer COM3)
avrdude -c arduino -p atmega328p -P COM3 -b 115200 -U flash:w:blink.hex:i

# Téléverser sur l'Arduino (Linux)
avrdude -c arduino -p atmega328p -P /dev/ttyACM0 -b 115200 -U flash:w:blink.hex:i
```

**Résultat attendu** : La LED intégrée de l'Arduino doit clignoter toutes les secondes.

### Script d'automatisation

**Windows** (`compile-upload.bat`) :

```batch
@echo off
REM Script de compilation et téléversement pour Arduino

SET PROJECT=%1
SET PORT=COM3
SET MCU=atmega328p

if "%PROJECT%"=="" (
    echo Usage: compile-upload.bat fichier.pas
    exit /b 1
)

echo ========================================
echo  Compilation...
echo ========================================

fpc -Pavr -Cp%MCU:~3% -Tembedded -Os %PROJECT%

if %errorlevel% neq 0 (
    echo ERREUR: Compilation echouee
    exit /b 1
)

SET HEXFILE=%~n1.hex

echo.
echo ========================================
echo  Televerement sur %PORT%...
echo ========================================

avrdude -c arduino -p %MCU% -P %PORT% -b 115200 -U flash:w:%HEXFILE%:i

if %errorlevel% neq 0 (
    echo ERREUR: Televerement echoue
    exit /b 1
)

echo.
echo ========================================
echo  TERMINE !
echo ========================================
```

**Linux** (`compile-upload.sh`) :

```bash
#!/bin/bash
# Script de compilation et téléversement pour Arduino

PROJECT=$1
PORT=/dev/ttyACM0
MCU=atmega328p

if [ -z "$PROJECT" ]; then
    echo "Usage: ./compile-upload.sh fichier.pas"
    exit 1
fi

echo "========================================"
echo "  Compilation..."
echo "========================================"

fpc -Pavr -CpATMEGA328P -Tembedded -Os "$PROJECT"

if [ $? -ne 0 ]; then
    echo "ERREUR: Compilation échouée"
    exit 1
fi

HEXFILE="${PROJECT%.pas}.hex"

echo ""
echo "========================================"
echo "  Téléversement sur $PORT..."
echo "========================================"

avrdude -c arduino -p $MCU -P $PORT -b 115200 -U flash:w:$HEXFILE:i

if [ $? -ne 0 ]; then
    echo "ERREUR: Téléversement échoué"
    exit 1
fi

echo ""
echo "========================================"
echo "  TERMINÉ !"
echo "========================================"
```

Rendre le script exécutable :

```bash
chmod +x compile-upload.sh
./compile-upload.sh blink.pas
```

## Unité de base pour Arduino

Créer une unité `arduino.pas` pour simplifier le développement :

```pascal
unit arduino;

{$mode objfpc}

interface

uses
  avr_api;

// Constantes de broches Arduino Uno
const
  // Broches numériques
  D0  = 0;  D1  = 1;  D2  = 2;  D3  = 3;
  D4  = 4;  D5  = 5;  D6  = 6;  D7  = 7;
  D8  = 8;  D9  = 9;  D10 = 10; D11 = 11;
  D12 = 12; D13 = 13;

  // Broches analogiques (aussi utilisables en numérique)
  A0 = 14; A1 = 15; A2 = 16; A3 = 17;
  A4 = 18; A5 = 19;

  // LED intégrée
  LED_BUILTIN = 13;

  // États
  LOW  = 0;
  HIGH = 1;

  // Modes des broches
  INPUT        = 0;
  OUTPUT       = 1;
  INPUT_PULLUP = 2;

// Fonctions de base
procedure setup;
procedure loop;

// Fonctions utilitaires
procedure waitMs(ms: Word);
procedure waitUs(us: Word);
function millis: LongWord;

implementation

var
  millisCounter: LongWord = 0;

procedure setup;
begin
  // À redéfinir dans le programme principal
end;

procedure loop;
begin
  // À redéfinir dans le programme principal
end;

procedure waitMs(ms: Word);
begin
  delay(ms);
end;

procedure waitUs(us: Word);
begin
  delayMicroseconds(us);
end;

function millis: LongWord;
begin
  Result := millisCounter;
end;

end.
```

## Entrées et sorties numériques

### Lecture de bouton et contrôle de LED

```pascal
program ButtonLED;

{$mode objfpc}

uses
  arduino;

const
  BUTTON_PIN = 2;
  LED_PIN    = 13;

var
  buttonState: Byte;

begin
  // Configuration
  pinMode(BUTTON_PIN, INPUT_PULLUP);  // Bouton avec résistance pull-up
  pinMode(LED_PIN, OUTPUT);            // LED en sortie

  // Boucle principale
  repeat
    // Lire l'état du bouton (LOW = pressé avec pull-up)
    buttonState := digitalRead(BUTTON_PIN);

    if buttonState = LOW then
      digitalWrite(LED_PIN, HIGH)  // Bouton pressé → LED allumée
    else
      digitalWrite(LED_PIN, LOW);  // Bouton relâché → LED éteinte

  until false;
end.
```

### LED à intensité variable (PWM)

```pascal
program FadingLED;

{$mode objfpc}

uses
  arduino;

const
  LED_PIN = 9;  // Broche PWM (3, 5, 6, 9, 10, 11 sur Uno)

var
  brightness: Byte;
  fadeAmount: ShortInt;

begin
  pinMode(LED_PIN, OUTPUT);

  brightness := 0;
  fadeAmount := 5;

  repeat
    // Définir la luminosité (0-255)
    analogWrite(LED_PIN, brightness);

    // Modifier la luminosité
    brightness := brightness + fadeAmount;

    // Inverser la direction aux extrêmes
    if (brightness = 0) or (brightness = 255) then
      fadeAmount := -fadeAmount;

    delay(30);  // Attendre 30ms
  until false;
end.
```

## Entrées analogiques

### Lecture de capteur analogique

```pascal
program ReadAnalog;

{$mode objfpc}

uses
  arduino;

const
  SENSOR_PIN = A0;  // Broche analogique A0
  LED_PIN    = 13;

var
  sensorValue: Word;
  threshold: Word;

begin
  pinMode(LED_PIN, OUTPUT);
  threshold := 512;  // Seuil à mi-course (10 bits = 0-1023)

  repeat
    // Lire la valeur analogique (0-1023)
    sensorValue := analogRead(SENSOR_PIN);

    // Allumer la LED si la valeur dépasse le seuil
    if sensorValue > threshold then
      digitalWrite(LED_PIN, HIGH)
    else
      digitalWrite(LED_PIN, LOW);

    delay(100);  // Lecture toutes les 100ms
  until false;
end.
```

### Capteur de température LM35

Le LM35 est un capteur de température analogique simple : 10 mV par degré Celsius.

```pascal
program TemperatureLM35;

{$mode objfpc}

uses
  arduino;

const
  TEMP_SENSOR_PIN = A0;
  LED_COLD  = 10;  // LED bleue
  LED_WARM  = 11;  // LED jaune
  LED_HOT   = 12;  // LED rouge

var
  sensorValue: Word;
  voltage: Real;
  temperature: Real;

procedure updateLEDs(temp: Real);
begin
  // Tout éteindre
  digitalWrite(LED_COLD, LOW);
  digitalWrite(LED_WARM, LOW);
  digitalWrite(LED_HOT, LOW);

  // Allumer selon la température
  if temp < 20 then
    digitalWrite(LED_COLD, HIGH)
  else if temp < 25 then
    digitalWrite(LED_WARM, HIGH)
  else
    digitalWrite(LED_HOT, HIGH);
end;

begin
  // Configuration
  pinMode(LED_COLD, OUTPUT);
  pinMode(LED_WARM, OUTPUT);
  pinMode(LED_HOT, OUTPUT);

  repeat
    // Lire le capteur
    sensorValue := analogRead(TEMP_SENSOR_PIN);

    // Convertir en tension (0-5V sur 10 bits)
    voltage := (sensorValue * 5.0) / 1023.0;

    // Convertir en température (LM35 : 10mV/°C)
    temperature := voltage * 100.0;

    // Mettre à jour les LEDs
    updateLEDs(temperature);

    delay(500);  // Lecture toutes les 500ms
  until false;
end.
```

## Communication série

### Envoi de données via USB

```pascal
program SerialOutput;

{$mode objfpc}

uses
  arduino, serial;

var
  counter: Word;
  sensorValue: Word;

begin
  // Initialiser la communication série à 9600 bauds
  Serial.begin(9600);

  counter := 0;

  repeat
    // Lire un capteur
    sensorValue := analogRead(A0);

    // Envoyer les données
    Serial.print('Compteur: ');
    Serial.print(counter);
    Serial.print(' | Capteur: ');
    Serial.println(sensorValue);

    Inc(counter);
    delay(1000);  // Envoi toutes les secondes
  until false;
end.
```

**Visualisation sur PC** :

```bash
# Linux
sudo apt install screen
screen /dev/ttyACM0 9600

# Ou avec minicom
sudo apt install minicom
minicom -D /dev/ttyACM0 -b 9600

# Windows : utiliser PuTTY ou Arduino IDE Serial Monitor
```

### Réception de commandes

```pascal
program SerialControl;

{$mode objfpc}

uses
  arduino, serial;

const
  LED_PIN = 13;

var
  command: Char;

begin
  pinMode(LED_PIN, OUTPUT);
  Serial.begin(9600);

  Serial.println('Arduino pret. Commandes:');
  Serial.println('  1 = LED ON');
  Serial.println('  0 = LED OFF');

  repeat
    // Vérifier si des données sont disponibles
    if Serial.available > 0 then
    begin
      // Lire un caractère
      command := Serial.read;

      case command of
        '1':
        begin
          digitalWrite(LED_PIN, HIGH);
          Serial.println('LED allumee');
        end;

        '0':
        begin
          digitalWrite(LED_PIN, LOW);
          Serial.println('LED eteinte');
        end;

        else
          Serial.println('Commande inconnue');
      end;
    end;

    delay(10);
  until false;
end.
```

## Capteurs et modules

### Capteur ultrasonique HC-SR04 (distance)

```pascal
program UltrasonicSensor;

{$mode objfpc}

uses
  arduino;

const
  TRIG_PIN = 9;
  ECHO_PIN = 10;
  LED_PIN  = 13;

function measureDistance: Word;
var
  duration: LongWord;
  distance: Word;
begin
  // Envoyer une impulsion de 10µs sur TRIG
  digitalWrite(TRIG_PIN, LOW);
  delayMicroseconds(2);
  digitalWrite(TRIG_PIN, HIGH);
  delayMicroseconds(10);
  digitalWrite(TRIG_PIN, LOW);

  // Mesurer la durée de l'impulsion ECHO (en µs)
  duration := pulseIn(ECHO_PIN, HIGH);

  // Calculer la distance en cm
  // Vitesse du son : 340 m/s = 0.034 cm/µs
  // Distance = (durée * vitesse) / 2 (aller-retour)
  distance := (duration * 0.034) / 2;

  Result := distance;
end;

var
  dist: Word;

begin
  pinMode(TRIG_PIN, OUTPUT);
  pinMode(ECHO_PIN, INPUT);
  pinMode(LED_PIN, OUTPUT);

  Serial.begin(9600);
  Serial.println('Capteur ultrasonique HC-SR04');

  repeat
    dist := measureDistance;

    Serial.print('Distance: ');
    Serial.print(dist);
    Serial.println(' cm');

    // Allumer la LED si un objet est proche (< 20cm)
    if dist < 20 then
      digitalWrite(LED_PIN, HIGH)
    else
      digitalWrite(LED_PIN, LOW);

    delay(500);
  until false;
end.
```

### Servo-moteur

```pascal
program ServoControl;

{$mode objfpc}

uses
  arduino, servo;

const
  SERVO_PIN = 9;

var
  myServo: TServo;
  pos: Byte;

begin
  // Attacher le servo à la broche 9
  myServo.attach(SERVO_PIN);

  repeat
    // Balayage de 0° à 180°
    for pos := 0 to 180 do
    begin
      myServo.write(pos);
      delay(15);
    end;

    // Balayage de 180° à 0°
    for pos := 180 downto 0 do
    begin
      myServo.write(pos);
      delay(15);
    end;
  until false;
end.
```

## Interruptions

Les interruptions permettent de réagir immédiatement à des événements externes.

```pascal
program InterruptExample;

{$mode objfpc}

uses
  arduino, interrupts;

const
  BUTTON_PIN = 2;  // Broche d'interruption (2 ou 3 sur Uno)
  LED_PIN    = 13;

var
  ledState: Byte;
  counter: Word;

procedure buttonPressed; interrupt;
begin
  // Basculer l'état de la LED
  ledState := 1 - ledState;
  digitalWrite(LED_PIN, ledState);
  Inc(counter);
end;

begin
  pinMode(BUTTON_PIN, INPUT_PULLUP);
  pinMode(LED_PIN, OUTPUT);

  ledState := LOW;
  counter := 0;

  // Attacher l'interruption au bouton
  // FALLING = déclenchement sur front descendant
  attachInterrupt(digitalPinToInterrupt(BUTTON_PIN), @buttonPressed, FALLING);

  Serial.begin(9600);

  repeat
    Serial.print('Compteur de pressions: ');
    Serial.println(counter);
    delay(2000);
  until false;
end.
```

## Gestion de l'énergie

### Mode sommeil (sleep)

```pascal
program SleepMode;

{$mode objfpc}

uses
  arduino, avr_sleep;

const
  LED_PIN = 13;

procedure goToSleep;
begin
  // Configurer le mode de sommeil
  set_sleep_mode(SLEEP_MODE_PWR_DOWN);
  sleep_enable;

  // Entrer en sommeil
  sleep_cpu;

  // --- Le CPU est en sommeil ici ---
  // Il se réveillera sur interruption

  // Désactiver le sommeil après réveil
  sleep_disable;
end;

begin
  pinMode(LED_PIN, OUTPUT);

  repeat
    // Clignoter
    digitalWrite(LED_PIN, HIGH);
    delay(100);
    digitalWrite(LED_PIN, LOW);
    delay(100);

    // Dormir pendant 8 secondes
    goToSleep;
  until false;
end.
```

## Optimisation de la mémoire

Les microcontrôleurs ont une mémoire très limitée. Voici quelques techniques d'optimisation :

### Utiliser PROGMEM pour les constantes

```pascal
program OptimizeMemory;

{$mode objfpc}

uses
  arduino, progmem;

const
  // Stocker les chaînes en Flash (PROGMEM) au lieu de la RAM
  Message1: PChar = 'Bonjour depuis Arduino!'; {$IFDEF AVR} progmem; {$ENDIF}
  Message2: PChar = 'FreePascal sur AVR'; {$IFDEF AVR} progmem; {$ENDIF}

var
  buffer: array[0..50] of Char;

begin
  Serial.begin(9600);

  // Copier depuis PROGMEM vers RAM pour affichage
  strcpy_P(buffer, Message1);
  Serial.println(buffer);

  strcpy_P(buffer, Message2);
  Serial.println(buffer);

  repeat
    delay(1000);
  until false;
end.
```

### Vérifier l'utilisation de la mémoire

```bash
# Après compilation, vérifier la taille
avr-size blink.elf

# Sortie exemple:
#    text    data     bss     dec     hex filename
#    1234     100     456    1790     6fe blink.elf
#
# text = code program (Flash)
# data = variables initialisées (RAM)
# bss  = variables non initialisées (RAM)
```

## ARM Cortex-M (STM32)

Les microcontrôleurs STM32 sont plus puissants que les AVR et offrent plus de mémoire et de périphériques.

### Arduino Due (SAM3X8E - ARM Cortex-M3)

```pascal
program DueBlink;

{$mode objfpc}

uses
  sam3x8e;  // Unité spécifique pour Arduino Due

const
  LED_PIN = 13;

begin
  // Activer l'horloge pour le port B
  PMC_PCER0 := PMC_PCER0 or (1 shl ID_PIOB);

  // Configurer la broche 13 en sortie (PB27)
  PIOB_PER := (1 shl 27);   // Enable PIO
  PIOB_OER := (1 shl 27);   // Output Enable

  repeat
    PIOB_SODR := (1 shl 27);  // Set Output Data (HIGH)
    delay(1000);

    PIOB_CODR := (1 shl 27);  // Clear Output Data (LOW)
    delay(1000);
  until false;
end.
```

**Compilation pour Due** :

```bash
# Compiler pour ARM Cortex-M3
fpc -Parm -Tembedded -CpARMV7M -WpSAM3X8E due_blink.pas

# Téléverser (nécessite bossac)
bossac -e -w -v -b due_blink.bin
```

## Projet complet : Station météo Arduino

### Matériel

- Arduino Uno
- Capteur DHT22 (température et humidité)
- Écran LCD I2C 16x2
- Module horloge RTC DS1307
- Carte SD

### Code principal

```pascal
program WeatherStation;

{$mode objfpc}

uses
  arduino, dht, lcd_i2c, rtc_ds1307, sd_card;

const
  DHT_PIN = 2;
  LCD_ADDRESS = $27;

var
  dht: TDHT22;
  lcd: TLCD_I2C;
  rtc: TDS1307;
  temp: Real;
  humidity: Real;

procedure initializeDevices;
begin
  // Initialiser le capteur DHT22
  dht.init(DHT_PIN);

  // Initialiser l'écran LCD
  lcd.init(LCD_ADDRESS, 16, 2);
  lcd.backlight;
  lcd.clear;

  // Initialiser l'horloge RTC
  rtc.init;

  // Message de démarrage
  lcd.setCursor(0, 0);
  lcd.print('Station Meteo');
  delay(2000);
  lcd.clear;
end;

procedure displayData;
begin
  // Afficher sur l'écran LCD
  lcd.setCursor(0, 0);
  lcd.print('T:');
  lcd.print(temp:4:1);
  lcd.print('C H:');
  lcd.print(humidity:4:1);
  lcd.print('%');

  lcd.setCursor(0, 1);
  lcd.print(rtc.getTimeString);
end;

begin
  Serial.begin(9600);
  initializeDevices;

  repeat
    // Lire les données du capteur
    temp := dht.readTemperature;
    humidity := dht.readHumidity;

    // Vérifier si la lecture est valide
    if not isnan(temp) and not isnan(humidity) then
    begin
      // Afficher sur LCD
      displayData;

      // Envoyer sur le port série
      Serial.print(rtc.getTimeString);
      Serial.print(' - Temp: ');
      Serial.print(temp:4:1);
      Serial.print('C, Humidite: ');
      Serial.print(humidity:4:1);
      Serial.println('%');
    end
    else
    begin
      lcd.clear;
      lcd.print('Erreur lecture!');
    end;

    delay(2000);  // Mise à jour toutes les 2 secondes
  until false;
end.
```

## Dépannage et conseils

### Problèmes courants

**1. Erreur "Device not found"**
```bash
# Vérifier la connexion
avrdude -c arduino -p atmega328p -P /dev/ttyACM0 -v

# Vérifier les permissions (Linux)
sudo chmod 666 /dev/ttyACM0
# Ou ajouter l'utilisateur au groupe dialout (permanent)
sudo usermod -a -G dialout $USER
```

**2. Mémoire insuffisante**
```
- Utiliser des types plus petits (Byte au lieu de Integer)
- Stocker les constantes en PROGMEM
- Désactiver les fonctionnalités inutiles
- Compiler avec -Os (optimisation taille)
```

**3. Programme ne démarre pas**
```
- Vérifier que le bootloader est présent
- Utiliser le bon modèle de carte (-CpATMEGA328P)
- Vérifier la vitesse de communication (115200 bauds)
```

### Bonnes pratiques

1. **Commentez votre code** : Les microcontrôleurs sont difficiles à déboguer
2. **Utilisez Serial.print** : Pour le débogage
3. **Testez par étapes** : Validez chaque fonctionnalité séparément
4. **Gérez les erreurs** : Vérifiez les retours des fonctions
5. **Optimisez la mémoire** : Surveillez l'utilisation RAM/Flash

## Ressources et documentation

### Documentation officielle

- **FreePascal Wiki AVR** : https://wiki.freepascal.org/AVR_Embedded_Tutorial
- **FreePascal Wiki ARM Embedded** : https://wiki.freepascal.org/ARM_Embedded
- **Arduino Reference** : https://www.arduino.cc/reference/en/
- **AVR Libc Documentation** : https://www.nongnu.org/avr-libc/

### Bibliothèques et projets

- **embedded-fpc** : Collection de bibliothèques pour embedded
  - GitHub : https://github.com/FPK/fpc-avr
- **Exemples AVR** : Dans l'installation FPC
  - Dossier : `$FPCDIR/examples/embedded/`

### Communauté

- **Forum Lazarus Embedded** : https://forum.lazarus.freepascal.org/
- **AVR Freaks** : Communauté AVR générale (C/C++/ASM)
- **Reddit /r/embedded** : Discussions sur l'embarqué

### Outils utiles

| Outil | Description | Lien |
|-------|-------------|------|
| **AVRDude** | Programmeur AVR | https://github.com/avrdudes/avrdude |
| **Arduino IDE** | IDE officiel Arduino | https://www.arduino.cc/en/software |
| **PlatformIO** | IDE moderne pour embedded | https://platformio.org/ |
| **Fritzing** | Conception de circuits | https://fritzing.org/ |
| **Tinkercad Circuits** | Simulation Arduino en ligne | https://www.tinkercad.com/circuits |

## Comparaison : FreePascal vs Arduino C++

### Syntaxe comparative

**Clignotement LED en Arduino C++** :

```cpp
// Arduino C++
void setup() {
  pinMode(13, OUTPUT);
}

void loop() {
  digitalWrite(13, HIGH);
  delay(1000);
  digitalWrite(13, LOW);
  delay(1000);
}
```

**Clignotement LED en FreePascal** :

```pascal
// FreePascal
program Blink;
uses arduino;

begin
  pinMode(13, OUTPUT);

  repeat
    digitalWrite(13, HIGH);
    delay(1000);
    digitalWrite(13, LOW);
    delay(1000);
  until false;
end.
```

### Tableau comparatif

| Critère | FreePascal | Arduino C++ |
|---------|-----------|-------------|
| **Syntaxe** | Claire et structurée | Compacte mais complexe |
| **Typage** | Fort et strict | Faible, conversions implicites |
| **Pointeurs** | Optionnels | Omniprésents |
| **Bibliothèques** | Limitées | Très nombreuses |
| **Communauté** | Restreinte | Très large |
| **Documentation** | Moyenne | Excellente |
| **Courbe d'apprentissage** | Douce | Moyenne |
| **Performance** | Excellente | Excellente |
| **Débogage** | Difficile | Difficile |

### Quand choisir FreePascal pour Arduino ?

✅ **FreePascal est recommandé si** :
- Vous connaissez déjà Pascal/Delphi
- Vous voulez un code plus lisible et maintenable
- Vous développez un projet éducatif
- Vous préférez le typage fort
- Vous ne dépendez pas de bibliothèques Arduino spécifiques

❌ **Arduino C++ est préférable si** :
- Vous utilisez des bibliothèques Arduino tierces
- Vous voulez bénéficier de nombreux exemples en ligne
- Vous travaillez en équipe avec des développeurs Arduino
- Vous utilisez des shields ou modules complexes
- Vous débutez dans l'embarqué (plus de ressources)

## Projets avancés

### Projet 1 : Robot suiveur de ligne

**Matériel** :
- Arduino Uno
- Châssis robot 2 roues
- 2 moteurs DC avec driver L298N
- 3 capteurs infrarouge suiveur de ligne
- Batterie 9V

**Code principal** :

```pascal
program LineFollower;

{$mode objfpc}

uses
  arduino;

const
  // Capteurs IR
  SENSOR_LEFT   = A0;
  SENSOR_CENTER = A1;
  SENSOR_RIGHT  = A2;

  // Moteur gauche
  MOTOR_L_PWM = 5;
  MOTOR_L_IN1 = 6;
  MOTOR_L_IN2 = 7;

  // Moteur droit
  MOTOR_R_PWM = 10;
  MOTOR_R_IN1 = 8;
  MOTOR_R_IN2 = 9;

  // Vitesses
  SPEED_NORMAL = 150;
  SPEED_TURN   = 100;
  THRESHOLD    = 500;  // Seuil noir/blanc

type
  TMotorDirection = (mdForward, mdBackward, mdStop);

procedure setMotor(pwmPin, in1Pin, in2Pin: Byte; speed: Byte; direction: TMotorDirection);
begin
  case direction of
    mdForward:
    begin
      digitalWrite(in1Pin, HIGH);
      digitalWrite(in2Pin, LOW);
      analogWrite(pwmPin, speed);
    end;

    mdBackward:
    begin
      digitalWrite(in1Pin, LOW);
      digitalWrite(in2Pin, HIGH);
      analogWrite(pwmPin, speed);
    end;

    mdStop:
    begin
      digitalWrite(in1Pin, LOW);
      digitalWrite(in2Pin, LOW);
      analogWrite(pwmPin, 0);
    end;
  end;
end;

procedure moveForward(speed: Byte);
begin
  setMotor(MOTOR_L_PWM, MOTOR_L_IN1, MOTOR_L_IN2, speed, mdForward);
  setMotor(MOTOR_R_PWM, MOTOR_R_IN1, MOTOR_R_IN2, speed, mdForward);
end;

procedure turnLeft(speed: Byte);
begin
  setMotor(MOTOR_L_PWM, MOTOR_L_IN1, MOTOR_L_IN2, speed, mdBackward);
  setMotor(MOTOR_R_PWM, MOTOR_R_IN1, MOTOR_R_IN2, speed, mdForward);
end;

procedure turnRight(speed: Byte);
begin
  setMotor(MOTOR_L_PWM, MOTOR_L_IN1, MOTOR_L_IN2, speed, mdForward);
  setMotor(MOTOR_R_PWM, MOTOR_R_IN1, MOTOR_R_IN2, speed, mdBackward);
end;

procedure stopMotors;
begin
  setMotor(MOTOR_L_PWM, MOTOR_L_IN1, MOTOR_L_IN2, 0, mdStop);
  setMotor(MOTOR_R_PWM, MOTOR_R_IN1, MOTOR_R_IN2, 0, mdStop);
end;

var
  sensorLeft, sensorCenter, sensorRight: Word;
  onLine: Boolean;

begin
  // Configuration des broches
  pinMode(MOTOR_L_PWM, OUTPUT);
  pinMode(MOTOR_L_IN1, OUTPUT);
  pinMode(MOTOR_L_IN2, OUTPUT);
  pinMode(MOTOR_R_PWM, OUTPUT);
  pinMode(MOTOR_R_IN1, OUTPUT);
  pinMode(MOTOR_R_IN2, OUTPUT);

  Serial.begin(9600);
  Serial.println('Robot suiveur de ligne - FreePascal');

  delay(2000);  // Attendre 2 secondes avant de démarrer

  repeat
    // Lire les capteurs
    sensorLeft   := analogRead(SENSOR_LEFT);
    sensorCenter := analogRead(SENSOR_CENTER);
    sensorRight  := analogRead(SENSOR_RIGHT);

    // Détecter si on est sur la ligne (noir)
    onLine := (sensorCenter < THRESHOLD);

    // Logique de suivi
    if (sensorLeft < THRESHOLD) and (sensorCenter < THRESHOLD) and (sensorRight < THRESHOLD) then
    begin
      // Tous sur noir → avancer
      moveForward(SPEED_NORMAL);
      Serial.println('Avancer');
    end
    else if (sensorLeft < THRESHOLD) and (sensorCenter < THRESHOLD) then
    begin
      // Gauche + centre sur noir → tourner légèrement à gauche
      turnLeft(SPEED_TURN);
      Serial.println('Tourner gauche');
    end
    else if (sensorCenter < THRESHOLD) and (sensorRight < THRESHOLD) then
    begin
      // Centre + droite sur noir → tourner légèrement à droite
      turnRight(SPEED_TURN);
      Serial.println('Tourner droite');
    end
    else if sensorCenter < THRESHOLD then
    begin
      // Seulement centre sur noir → avancer
      moveForward(SPEED_NORMAL);
      Serial.println('Centre - Avancer');
    end
    else if sensorLeft < THRESHOLD then
    begin
      // Seulement gauche sur noir → tourner à gauche
      turnLeft(SPEED_TURN);
      Serial.println('Correction gauche');
    end
    else if sensorRight < THRESHOLD then
    begin
      // Seulement droite sur noir → tourner à droite
      turnRight(SPEED_TURN);
      Serial.println('Correction droite');
    end
    else
    begin
      // Aucun capteur sur noir → arrêt
      stopMotors;
      Serial.println('Ligne perdue - STOP');
    end;

    delay(50);  // Petit délai pour la stabilité
  until false;
end.
```

### Projet 2 : Thermomètre WiFi avec ESP8266

**Note** : Le support ESP8266/ESP32 avec FreePascal est expérimental. Voici un exemple conceptuel.

```pascal
program WiFiThermometer;

{$mode objfpc}

uses
  arduino, esp8266wifi, dht;

const
  WIFI_SSID     = 'VotreSSID';
  WIFI_PASSWORD = 'VotreMotDePasse';
  DHT_PIN       = 2;
  SERVER_PORT   = 80;

var
  dht: TDHT22;
  server: TESPWebServer;
  temp, humidity: Real;

procedure handleRoot;
var
  html: String;
begin
  html :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <title>Thermomètre WiFi</title>' +
    '  <meta charset="UTF-8">' +
    '  <meta http-equiv="refresh" content="5">' +
    '  <style>' +
    '    body { font-family: Arial; text-align: center; background: #f0f0f0; }' +
    '    .container { margin: 50px auto; padding: 30px; background: white; width: 400px; border-radius: 10px; }' +
    '    .value { font-size: 48px; color: #e74c3c; }' +
    '  </style>' +
    '</head>' +
    '<body>' +
    '  <div class="container">' +
    '    <h1>🌡️ Thermomètre WiFi</h1>' +
    '    <p>Température</p>' +
    '    <div class="value">' + FloatToStr(temp) + ' °C</div>' +
    '    <p>Humidité</p>' +
    '    <div class="value">' + FloatToStr(humidity) + ' %</div>' +
    '  </div>' +
    '</body>' +
    '</html>';

  server.send(200, 'text/html', html);
end;

procedure handleData;
var
  json: String;
begin
  json := '{"temperature":' + FloatToStr(temp) +
          ',"humidity":' + FloatToStr(humidity) + '}';

  server.send(200, 'application/json', json);
end;

begin
  Serial.begin(115200);
  Serial.println('Thermomètre WiFi - FreePascal');

  // Initialiser le DHT22
  dht.init(DHT_PIN);

  // Connexion WiFi
  WiFi.begin(WIFI_SSID, WIFI_PASSWORD);
  Serial.print('Connexion WiFi');

  while WiFi.status <> WL_CONNECTED do
  begin
    delay(500);
    Serial.print('.');
  end;

  Serial.println;
  Serial.print('Connecté ! IP: ');
  Serial.println(WiFi.localIP);

  // Configurer le serveur web
  server.on('/', @handleRoot);
  server.on('/data', @handleData);
  server.begin;

  Serial.println('Serveur web démarré');

  repeat
    // Lire le capteur
    temp := dht.readTemperature;
    humidity := dht.readHumidity;

    // Gérer les requêtes web
    server.handleClient;

    delay(100);
  until false;
end.
```

### Projet 3 : Enregistreur de données sur carte SD

```pascal
program DataLogger;

{$mode objfpc}

uses
  arduino, sd, rtc_ds1307;

const
  SD_CS_PIN = 10;
  DHT_PIN   = 2;
  LED_PIN   = 13;
  LOG_FILE  = 'datalog.txt';
  LOG_INTERVAL = 60000;  // 1 minute en millisecondes

var
  rtc: TDS1307;
  lastLogTime: LongWord;
  temp, humidity: Real;
  logFile: TFile;

function getTimestamp: String;
begin
  Result := rtc.getDate + ' ' + rtc.getTime;
end;

procedure logData;
var
  dataLine: String;
begin
  // Créer la ligne de données
  dataLine := getTimestamp + ',' +
              FloatToStrF(temp, ffFixed, 5, 2) + ',' +
              FloatToStrF(humidity, ffFixed, 5, 2);

  // Ouvrir le fichier en mode ajout
  logFile := SD.open(LOG_FILE, FILE_WRITE);

  if logFile then
  begin
    logFile.println(dataLine);
    logFile.close;

    // Clignoter la LED pour indiquer l'enregistrement
    digitalWrite(LED_PIN, HIGH);
    delay(100);
    digitalWrite(LED_PIN, LOW);

    Serial.println('Données enregistrées: ' + dataLine);
  end
  else
    Serial.println('Erreur ouverture fichier');
end;

begin
  Serial.begin(9600);
  pinMode(LED_PIN, OUTPUT);

  Serial.println('Enregistreur de données - FreePascal');

  // Initialiser la carte SD
  Serial.print('Initialisation carte SD...');
  if not SD.begin(SD_CS_PIN) then
  begin
    Serial.println('ERREUR!');
    while true do
      delay(1000);  // Bloquer si la carte SD n'est pas détectée
  end;
  Serial.println('OK');

  // Initialiser le RTC
  rtc.init;

  // Créer l'en-tête du fichier si nouveau
  if not SD.exists(LOG_FILE) then
  begin
    logFile := SD.open(LOG_FILE, FILE_WRITE);
    if logFile then
    begin
      logFile.println('Date,Heure,Temperature,Humidite');
      logFile.close;
    end;
  end;

  lastLogTime := 0;

  Serial.println('Enregistrement démarré');

  repeat
    // Vérifier si c'est le moment d'enregistrer
    if (millis - lastLogTime) >= LOG_INTERVAL then
    begin
      // Lire les capteurs
      temp := readTemperature;
      humidity := readHumidity;

      // Enregistrer
      logData;

      lastLogTime := millis;
    end;

    delay(100);
  until false;
end.
```

## Limitations et considérations

### Limitations matérielles

**Mémoire Flash (Programme)** :
- Arduino Uno : 32 KB (dont ~2 KB pour bootloader)
- Arduino Mega : 256 KB
- Arduino Due : 512 KB

**Mémoire SRAM (Variables)** :
- Arduino Uno : 2 KB
- Arduino Mega : 8 KB
- Arduino Due : 96 KB

**EEPROM** :
- Arduino Uno : 1 KB
- Arduino Mega : 4 KB
- Arduino Due : N/A (peut utiliser la Flash)

### Optimisations critiques

```pascal
// ❌ MAUVAIS - Consomme beaucoup de mémoire
var
  message: String;
begin
  message := 'Bonjour le monde!';
  Serial.println(message);
end;

// ✅ BON - Utilise moins de mémoire
begin
  Serial.println('Bonjour le monde!');
end;

// ✅ MEILLEUR - Stocke en Flash
const
  MESSAGE: PChar = 'Bonjour le monde!'; progmem;
begin
  Serial.println_P(MESSAGE);
end;
```

### Gestion des erreurs

```pascal
program SafeProgram;

{$mode objfpc}

uses
  arduino;

function safeDivide(a, b: Integer): Integer;
begin
  if b = 0 then
  begin
    Serial.println('ERREUR: Division par zéro!');
    Result := 0;
  end
  else
    Result := a div b;
end;

function safeAnalogRead(pin: Byte): Word;
begin
  if (pin >= A0) and (pin <= A5) then
    Result := analogRead(pin)
  else
  begin
    Serial.print('ERREUR: Broche analogique invalide: ');
    Serial.println(pin);
    Result := 0;
  end;
end;

begin
  Serial.begin(9600);

  // Utiliser les fonctions sécurisées
  Serial.println(safeDivide(10, 2));   // OK
  Serial.println(safeDivide(10, 0));   // Gère l'erreur

  Serial.println(safeAnalogRead(A0));  // OK
  Serial.println(safeAnalogRead(99));  // Gère l'erreur
end.
```

## Conclusion

FreePascal offre une alternative intéressante pour la programmation de microcontrôleurs Arduino, particulièrement pour :

### Points forts

✅ **Syntaxe claire** : Code plus lisible et maintenable  
✅ **Typage fort** : Moins d'erreurs à l'exécution  
✅ **Performances** : Équivalentes au C/C++  
✅ **Portabilité** : Code partageable avec d'autres plateformes  
✅ **Apprentissage** : Excellent pour l'enseignement

### Points à améliorer

⚠️ **Écosystème** : Moins de bibliothèques que Arduino C++  
⚠️ **Documentation** : Moins d'exemples et de tutoriels  
⚠️ **Outils** : Configuration plus complexe qu'Arduino IDE  
⚠️ **Communauté** : Plus restreinte

### Recommandations finales

1. **Pour débuter** : Commencez avec Arduino Uno et des projets simples
2. **Apprentissage** : Étudiez les datasheets des microcontrôleurs
3. **Développement** : Créez vos propres bibliothèques réutilisables
4. **Tests** : Testez chaque composant individuellement avant l'intégration
5. **Optimisation** : Surveillez l'utilisation de la mémoire avec `avr-size`
6. **Documentation** : Commentez abondamment votre code
7. **Communauté** : Partagez vos projets et bibliothèques

### Ressources pour aller plus loin

- **Datasheets** : Lisez les datasheets des microcontrôleurs
- **Forums** : Participez aux forums FreePascal et Arduino
- **Projets** : Étudiez des projets open source existants
- **Expérimentation** : N'ayez pas peur d'expérimenter !

> 💡 **Conseil final** : FreePascal pour Arduino est un excellent choix si vous maîtrisez déjà Pascal et voulez l'utiliser dans l'embarqué. Pour les débutants en électronique, commencez peut-être par Arduino C++ pour bénéficier de plus de ressources, puis migrez vers FreePascal une fois à l'aise.

**Bonne programmation embarquée avec FreePascal ! 🔌🤖**

⏭️ [Optimisation pour appareils mobiles](/13-developpement-mobile-embarque/09-optimisation-appareils-mobiles.md)
