🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.2 AVR et Arduino avec FreePascal

## Introduction aux microcontrôleurs AVR

### Qu'est-ce que l'AVR ?

AVR est une famille de microcontrôleurs 8 bits développée par Atmel (maintenant Microchip). Ces microcontrôleurs sont notamment utilisés dans les cartes Arduino, ce qui en fait l'une des plateformes les plus populaires pour l'apprentissage de l'électronique.

**Caractéristiques principales :**
- Architecture RISC (Reduced Instruction Set Computer) : instructions simples et rapides
- Mémoire Flash pour le programme (2 Ko à 256 Ko)
- RAM (SRAM) pour les variables (128 octets à 16 Ko)
- EEPROM pour stockage persistant (64 octets à 4 Ko)
- Nombreux périphériques intégrés (GPIO, timers, ADC, UART, SPI, I2C)
- Faible consommation électrique
- Large gamme de modèles

### Qu'est-ce qu'Arduino ?

Arduino n'est pas un microcontrôleur, mais une **plateforme de développement** qui comprend :

1. **Des cartes électroniques** : facilement utilisables avec des connecteurs standards
2. **Un bootloader** : petit programme préinstallé qui permet de programmer la carte via USB
3. **Un IDE** : environnement de développement simplifié
4. **Des bibliothèques** : code réutilisable pour capteurs, écrans, etc.
5. **Une communauté** : millions d'utilisateurs, tutoriels, projets partagés

**Avantage pour les débutants :** Arduino rend l'électronique accessible en masquant la complexité du matériel.

## Modèles AVR populaires dans Arduino

### ATmega328P (Arduino Uno, Nano)

Le plus populaire pour débuter :

**Spécifications :**
- **Flash** : 32 Ko (dont 0,5 Ko pour bootloader)
- **RAM** : 2 Ko
- **EEPROM** : 1 Ko
- **Fréquence** : 16 MHz
- **GPIO** : 23 broches (14 digitales + 6 analogiques)
- **Timers** : 3 (deux 8 bits, un 16 bits)
- **Communication** : UART, SPI, I2C

**Cartes Arduino utilisant ce chip :**
- Arduino Uno (la plus connue)
- Arduino Nano (version compacte)
- Arduino Pro Mini (sans USB intégré)

### ATmega2560 (Arduino Mega)

Pour projets plus complexes nécessitant plus d'E/S :

**Spécifications :**
- **Flash** : 256 Ko
- **RAM** : 8 Ko
- **EEPROM** : 4 Ko
- **GPIO** : 86 broches (54 digitales + 16 analogiques)
- **Timers** : 6
- **UART** : 4 ports série

**Quand l'utiliser ?**
- Nombreux capteurs/actionneurs
- Code volumineux
- Multiples communications série

### ATtiny85 (DigiSpark)

Microcontrôleur minimal pour projets compacts :

**Spécifications :**
- **Flash** : 8 Ko
- **RAM** : 512 octets
- **GPIO** : 6 broches
- **Format** : 8 broches (très petit)

**Usage typique :**
- Projets portables
- Wearables (vêtements électroniques)
- Capteurs autonomes

### ATmega32U4 (Arduino Leonardo, Pro Micro)

Avec USB natif :

**Particularité** : peut émuler clavier/souris USB  
**Usage** : interfaces HID (Human Interface Device), macropads, contrôleurs de jeu  

## Architecture matérielle AVR

### Organisation de la mémoire

```
┌─────────────────────────┐
│   Mémoire Flash         │  ← Programme (lecture seule après flash)
│   (0x0000 - 0x7FFF)     │     Exécution du code
└─────────────────────────┘

┌─────────────────────────┐
│   RAM (SRAM)            │  ← Variables, pile d'exécution
│   (0x0100 - 0x08FF)     │     Lecture/écriture rapide
└─────────────────────────┘     Volatile (perdue à l'extinction)

┌─────────────────────────┐
│   EEPROM                │  ← Données persistantes
│   (0x0000 - 0x03FF)     │     Survit à l'extinction
└─────────────────────────┘     Nombre d'écritures limité (~100 000)

┌─────────────────────────┐
│   Registres E/S         │  ← Configuration matérielle
│   (0x0020 - 0x00FF)     │     Contrôle des périphériques
└─────────────────────────┘
```

### Registres importants

Les registres sont des zones mémoire spéciales qui contrôlent le matériel. Voici les principaux :

#### GPIO (General Purpose Input/Output)

Chaque port GPIO a 3 registres :

```pascal
// Port B (broches digitales 8-13 sur Uno)
DDRB   // Data Direction Register : 0=entrée, 1=sortie
PORTB  // Port Output Register : valeur à écrire en sortie
PINB   // Port Input Register : lecture de l'état des broches

// Port C (broches analogiques A0-A5 sur Uno)
DDRC
PORTC
PINC

// Port D (broches digitales 0-7 sur Uno)
DDRD
PORTD
PIND
```

**Exemple concret :**

```pascal
// Configurer la broche 13 (LED intégrée) en sortie
// Pin 13 = bit 5 du Port B (PB5)
DDRB := DDRB or (1 shl 5);  // Mettre le bit 5 à 1

// Allumer la LED
PORTB := PORTB or (1 shl 5);

// Éteindre la LED
PORTB := PORTB and not (1 shl 5);
```

#### Timers/Compteurs

Les timers génèrent des interruptions périodiques ou mesurent le temps :

```pascal
TCCR0A  // Timer/Counter Control Register A
TCCR0B  // Timer/Counter Control Register B
TCNT0   // Timer/Counter Register (valeur actuelle)
OCR0A   // Output Compare Register A
TIMSK0  // Timer Interrupt Mask Register
```

#### UART (Communication série)

```pascal
UBRR0   // USART Baud Rate Register (vitesse)
UCSR0A  // USART Control and Status Register A
UCSR0B  // USART Control and Status Register B
UCSR0C  // USART Control and Status Register C
UDR0    // USART Data Register (données à envoyer/recevoir)
```

### Interruptions

Les interruptions permettent de réagir instantanément à des événements sans constamment vérifier (polling).

**Vecteurs d'interruption AVR :**

| Nom | Description |
|-----|-------------|
| `RESET_vect` | Reset du microcontrôleur |
| `INT0_vect`, `INT1_vect` | Interruption externe sur broche |
| `TIMER0_COMPA_vect` | Timer 0 Compare Match A |
| `TIMER1_COMPA_vect` | Timer 1 Compare Match A |
| `USART_RX_vect` | Réception série UART |
| `ADC_vect` | Conversion analogique terminée |

**Exemple d'utilisation :**

```pascal
var
  compteur: word = 0;

// Gestionnaire d'interruption Timer 1
procedure Timer1_ISR; interrupt; public name 'TIMER1_COMPA_vect';
begin
  Inc(compteur);  // Incrémenté automatiquement toutes les 1s
end;
```

## Installation et configuration

### Prérequis Windows

1. **FreePascal** : version 3.2.0 ou supérieure avec support AVR
2. **AVR Toolchain** :
   - Télécharger depuis https://blog.zakkemble.net/avr-gcc-builds/
   - Ou installer via Arduino IDE (les outils sont dans `arduino/hardware/tools/avr/`)
3. **AVRDude** : pour flasher le microcontrôleur (inclus dans Arduino IDE)

**Variables d'environnement à configurer :**

```batch
set PATH=%PATH%;C:\avr-gcc\bin
set PATH=%PATH%;C:\Arduino\hardware\tools\avr\bin
```

### Prérequis Ubuntu

Installation simple via APT :

```bash
# Installer le compilateur FreePascal avec support AVR
sudo apt update
sudo apt install fpc fpc-source

# Installer la toolchain AVR
sudo apt install binutils-avr gcc-avr avr-libc

# Installer AVRDude pour flasher
sudo apt install avrdude

# Optionnel : Arduino IDE pour référence
sudo apt install arduino
```

**Vérification de l'installation :**

```bash
fpc -i | grep avr        # Doit montrer le support AVR
avr-gcc --version        # Doit afficher la version
avrdude -?               # Doit lister les options
```

### Configuration du compilateur

Créer un fichier `fpc.cfg` personnalisé pour AVR :

```ini
# Configuration AVR pour FreePascal

# Processeur et cible
-Pavr
-Tembedded
-Wpatmega328p

# Optimisations
-O2
-CfSOFT

# Chemins de recherche
-Fu/usr/lib/fpc/$fpcversion/units/$fpctarget
-Fu/usr/lib/fpc/$fpcversion/units/$fpctarget/*
-Fu/usr/lib/fpc/$fpcversion/units/$fpctarget/rtl

# Génération du fichier HEX
-s
```

## Structure d'un programme AVR en FreePascal

### Programme minimal "Hello World" LED

Ce programme fait clignoter la LED intégrée d'un Arduino Uno (broche 13) :

```pascal
program BlinkLED;

{$mode objfpc}
{$H+}

// Pas d'unité spécifique nécessaire pour accès direct aux registres

const
  LED_PIN = 5;  // Pin 13 Arduino = PB5 (bit 5 du Port B)

// Délai approximatif (blocant)
procedure Delay_ms(ms: word);
var
  i, j: word;
begin
  for i := 0 to ms do
    for j := 0 to 1000 do
      asm nop; end;  // Instruction vide pour temporisation
end;

// Initialisation du matériel
procedure Setup;
begin
  // Configurer PB5 (LED) en sortie
  DDRB := DDRB or (1 shl LED_PIN);
end;

// Boucle principale
procedure Loop;
begin
  while true do
  begin
    // Allumer LED
    PORTB := PORTB or (1 shl LED_PIN);
    Delay_ms(1000);

    // Éteindre LED
    PORTB := PORTB and not (1 shl LED_PIN);
    Delay_ms(1000);
  end;
end;

// Point d'entrée
begin
  Setup;
  Loop;
end.
```

**Explication ligne par ligne :**

```pascal
{$mode objfpc}    // Mode Object Pascal moderne
{$H+}             // Chaînes longues (pas critique ici)

const
  LED_PIN = 5;    // Correspondance Arduino pin 13 ↔ AVR PB5
```

Le mappage des broches Arduino vers les ports AVR :
- Pin 13 = Port B, bit 5 (PB5)
- Pin 12 = PB4
- Pin 11 = PB3
- Etc.

```pascal
DDRB := DDRB or (1 shl LED_PIN);
```
Cette ligne configure la broche en **sortie** :
- `1 shl LED_PIN` : décale le bit 1 de LED_PIN positions (crée le masque 0b00100000)
- `DDRB or ...` : active le bit sans toucher les autres

```pascal
PORTB := PORTB or (1 shl LED_PIN);   // Met le bit à 1 (allume)
PORTB := PORTB and not (1 shl LED_PIN); // Met le bit à 0 (éteint)
```

### Utilisation des unités AVR

FreePascal fournit des unités qui facilitent l'accès aux registres :

```pascal
program BlinkLEDv2;

{$mode objfpc}
{$H+}

uses
  avr,         // Définitions générales AVR
  atmega328p,  // Registres spécifiques ATmega328P
  intrinsics;  // Fonctions intrinsèques (delay, etc.)

const
  LED_PORT = PORTB;
  LED_DDR  = DDRB;
  LED_PIN  = 5;

procedure Setup;
begin
  LED_DDR := LED_DDR or (1 shl LED_PIN);
end;

procedure Loop;
begin
  while true do
  begin
    LED_PORT := LED_PORT or (1 shl LED_PIN);
    avr_delay_ms(1000);  // Délai précis fourni par intrinsics

    LED_PORT := LED_PORT and not (1 shl LED_PIN);
    avr_delay_ms(1000);
  end;
end;

begin
  Setup;
  Loop;
end.
```

### Lecture d'entrée numérique (bouton)

```pascal
program BoutonLED;

uses
  atmega328p, intrinsics;

const
  LED_PIN    = 5;  // PB5 (pin 13)
  BUTTON_PIN = 2;  // PD2 (pin 2)

procedure Setup;
begin
  // LED en sortie
  DDRB := DDRB or (1 shl LED_PIN);

  // Bouton en entrée
  DDRD := DDRD and not (1 shl BUTTON_PIN);

  // Activer résistance pull-up interne
  PORTD := PORTD or (1 shl BUTTON_PIN);
end;

procedure Loop;
var
  buttonState: boolean;
begin
  while true do
  begin
    // Lire l'état du bouton (0 = pressé avec pull-up)
    buttonState := (PIND and (1 shl BUTTON_PIN)) = 0;

    if buttonState then
      PORTB := PORTB or (1 shl LED_PIN)   // Allumer
    else
      PORTB := PORTB and not (1 shl LED_PIN); // Éteindre

    avr_delay_ms(50);  // Anti-rebond simple
  end;
end;

begin
  Setup;
  Loop;
end.
```

**Concept important : Pull-up**

Les entrées numériques "flottent" si rien n'est connecté. Le pull-up tire la broche vers +5V :
- Bouton **non pressé** : broche à 5V (lecture = 1)
- Bouton **pressé** : broche à 0V (lecture = 0)

## Entrées analogiques (ADC)

L'ADC (Analog-to-Digital Converter) convertit une tension (0-5V) en valeur numérique (0-1023 sur 10 bits).

### Configuration de l'ADC

```pascal
program LectureAnalogique;

uses
  atmega328p, intrinsics;

const
  ADC_CHANNEL = 0;  // Pin A0

// Initialiser l'ADC
procedure ADC_Init;
begin
  // Référence de tension : AVCC (5V)
  ADMUX := (1 shl REFS0);

  // Activer ADC, prescaler = 128 (pour 16MHz → 125kHz ADC clock)
  ADCSRA := (1 shl ADEN) or (1 shl ADPS2) or (1 shl ADPS1) or (1 shl ADPS0);
end;

// Lire une valeur sur un canal
function ADC_Read(channel: byte): word;
begin
  // Sélectionner le canal (0-5 sur Arduino Uno)
  ADMUX := (ADMUX and $F0) or (channel and $0F);

  // Démarrer la conversion
  ADCSRA := ADCSRA or (1 shl ADSC);

  // Attendre la fin de conversion
  while (ADCSRA and (1 shl ADSC)) <> 0 do
    ;  // Boucle vide

  // Retourner le résultat (10 bits)
  Result := ADC;
end;

var
  valeur: word;

begin
  ADC_Init;

  while true do
  begin
    valeur := ADC_Read(ADC_CHANNEL);

    // valeur est entre 0 et 1023
    // 0 = 0V, 1023 = 5V
    // Exemple : si valeur = 512 → environ 2.5V

    avr_delay_ms(100);
  end;
end.
```

**Application pratique : allumer LED selon potentiomètre**

```pascal
var
  valeur: word;
  seuil: word = 512;  // Seuil à mi-course

begin
  ADC_Init;
  DDRB := DDRB or (1 shl 5);  // LED en sortie

  while true do
  begin
    valeur := ADC_Read(0);

    if valeur > seuil then
      PORTB := PORTB or (1 shl 5)   // Allumer
    else
      PORTB := PORTB and not (1 shl 5); // Éteindre

    avr_delay_ms(50);
  end;
end.
```

## Communication série (UART)

La communication série permet d'échanger des données avec un PC via USB.

### Configuration UART

```pascal
program CommunicationSerie;

uses
  atmega328p;

const
  F_CPU = 16000000;  // Fréquence horloge Arduino : 16 MHz
  BAUD  = 9600;      // Vitesse : 9600 bauds

  // Calcul du registre UBRR
  UBRR_VALUE = (F_CPU div (16 * BAUD)) - 1;

// Initialiser UART
procedure UART_Init;
begin
  // Configurer la vitesse
  UBRR0H := Hi(UBRR_VALUE);
  UBRR0L := Lo(UBRR_VALUE);

  // Activer émission et réception
  UCSR0B := (1 shl RXEN0) or (1 shl TXEN0);

  // Format : 8 bits de données, 1 bit de stop, pas de parité
  UCSR0C := (1 shl UCSZ01) or (1 shl UCSZ00);
end;

// Envoyer un octet
procedure UART_Transmit(data: byte);
begin
  // Attendre que le buffer soit vide
  while (UCSR0A and (1 shl UDRE0)) = 0 do
    ;

  // Envoyer la donnée
  UDR0 := data;
end;

// Recevoir un octet
function UART_Receive: byte;
begin
  // Attendre la réception
  while (UCSR0A and (1 shl RXC0)) = 0 do
    ;

  // Lire la donnée
  Result := UDR0;
end;

// Envoyer une chaîne
procedure UART_PrintString(const s: string);
var
  i: integer;
begin
  for i := 1 to Length(s) do
    UART_Transmit(Ord(s[i]));
end;

// Envoyer un saut de ligne
procedure UART_PrintLn(const s: string);
begin
  UART_PrintString(s);
  UART_Transmit(13);  // CR
  UART_Transmit(10);  // LF
end;

var
  compteur: word = 0;

begin
  UART_Init;

  UART_PrintLn('Arduino AVR avec FreePascal');
  UART_PrintLn('Demarrage...');

  while true do
  begin
    UART_PrintString('Compteur: ');
    // Conversion nombre → texte (simplifiée)
    UART_PrintLn('...');

    Inc(compteur);
    avr_delay_ms(1000);
  end;
end.
```

**Utilisation sur PC :**

**Windows :**
- Moniteur série Arduino IDE
- PuTTY sur le port COM approprié
- Terminal série au choix

**Ubuntu :**
```bash
# Identifier le port (souvent /dev/ttyUSB0 ou /dev/ttyACM0)
ls /dev/tty*

# Ouvrir avec screen
sudo screen /dev/ttyACM0 9600

# Ou avec minicom
sudo minicom -D /dev/ttyACM0 -b 9600
```

## Timers et PWM

### Timer en mode CTC (Clear Timer on Compare)

Permet de générer des interruptions périodiques précises :

```pascal
program TimerPrecis;

uses
  atmega328p;

var
  secondes: word = 0;

// Interruption Timer 1 Compare Match A
procedure Timer1_ISR; interrupt; public name 'TIMER1_COMPA_vect';
begin
  Inc(secondes);
  // Cette interruption est appelée exactement toutes les secondes
end;

procedure Timer1_Init;
const
  F_CPU = 16000000;
  PRESCALER = 256;
  COMPARE_VALUE = F_CPU div PRESCALER - 1;  // Pour 1 seconde
begin
  // Mode CTC (Clear Timer on Compare Match)
  TCCR1A := 0;
  TCCR1B := (1 shl WGM12) or (1 shl CS12);  // CTC + prescaler 256

  // Valeur de comparaison
  OCR1AH := Hi(COMPARE_VALUE);
  OCR1AL := Lo(COMPARE_VALUE);

  // Activer l'interruption Compare Match A
  TIMSK1 := (1 shl OCIE1A);

  // Activer les interruptions globalement
  SREG := SREG or (1 shl 7);  // Bit I (Interrupt Enable)
end;

begin
  Timer1_Init;
  DDRB := DDRB or (1 shl 5);  // LED en sortie

  while true do
  begin
    // La LED clignote toutes les secondes grâce à l'interruption
    if (secondes mod 2) = 0 then
      PORTB := PORTB or (1 shl 5)
    else
      PORTB := PORTB and not (1 shl 5);
  end;
end.
```

### PWM (Pulse Width Modulation)

Le PWM permet de varier l'intensité d'une LED ou la vitesse d'un moteur :

```pascal
program PWM_LED;

uses
  atmega328p;

const
  PWM_PIN = 6;  // Arduino pin 6 = Timer 0, OC0A

// Initialiser PWM sur Timer 0
procedure PWM_Init;
begin
  // Pin 6 en sortie
  DDRD := DDRD or (1 shl PWM_PIN);

  // Mode Fast PWM
  TCCR0A := (1 shl WGM01) or (1 shl WGM00) or (1 shl COM0A1);

  // Prescaler = 64
  TCCR0B := (1 shl CS01) or (1 shl CS00);

  // Rapport cyclique initial : 0 (éteint)
  OCR0A := 0;
end;

// Définir l'intensité (0-255)
procedure PWM_Set(value: byte);
begin
  OCR0A := value;
end;

var
  intensite: byte;
  montee: boolean = true;

begin
  PWM_Init;
  intensite := 0;

  // Effet de respiration (fade in/out)
  while true do
  begin
    PWM_Set(intensite);

    if montee then
    begin
      Inc(intensite, 5);
      if intensite >= 250 then
        montee := false;
    end
    else
    begin
      Dec(intensite, 5);
      if intensite <= 5 then
        montee := true;
    end;

    avr_delay_ms(30);
  end;
end.
```

**Broches PWM sur Arduino Uno :**
- Pin 3, 5, 6, 9, 10, 11 (timer 0, 1, 2)

## Compilation et flashage

### Compilation

**Ligne de commande Windows :**

```batch
fpc -Pavr -Tembedded -Wpatmega328p -O2 monprogramme.pas
avr-objcopy -O ihex monprogramme.elf monprogramme.hex
```

**Ligne de commande Ubuntu :**

```bash
fpc -Pavr -Tembedded -Wpatmega328p -O2 monprogramme.pas
avr-objcopy -O ihex monprogramme.elf monprogramme.hex
```

### Flashage avec AVRDude

**Arduino Uno (ATmega328P via USB) :**

**Windows :**
```batch
avrdude -p atmega328p -c arduino -P COM3 -b 115200 -U flash:w:monprogramme.hex:i
```

**Ubuntu :**
```bash
sudo avrdude -p atmega328p -c arduino -P /dev/ttyACM0 -b 115200 -U flash:w:monprogramme.hex:i
```

**Paramètres importants :**
- `-p atmega328p` : type de microcontrôleur
- `-c arduino` : protocole (bootloader Arduino)
- `-P COM3` ou `/dev/ttyACM0` : port série
- `-b 115200` : vitesse de communication
- `-U flash:w:monprogramme.hex:i` : écrire le fichier hex en Flash

**Arduino Mega (ATmega2560) :**
```bash
avrdude -p atmega2560 -c wiring -P /dev/ttyACM0 -b 115200 -U flash:w:monprogramme.hex:i
```

**Programmer externe (USBtinyISP, AVR Dragon) :**
```bash
avrdude -p atmega328p -c usbtiny -U flash:w:monprogramme.hex:i
```

### Script de build automatique

**build.sh (Ubuntu) :**

```bash
#!/bin/bash

PROGRAM="monprogramme"
MCU="atmega328p"
PORT="/dev/ttyACM0"

echo "Compilation..."
fpc -Pavr -Tembedded -Wp$MCU -O2 $PROGRAM.pas

if [ $? -eq 0 ]; then
    echo "Conversion en HEX..."
    avr-objcopy -O ihex $PROGRAM.elf $PROGRAM.hex

    echo "Flashage..."
    sudo avrdude -p $MCU -c arduino -P $PORT -b 115200 -U flash:w:$PROGRAM.hex:i

    echo "Terminé!"
else
    echo "Erreur de compilation!"
fi
```

**build.bat (Windows) :**

```batch
@echo off
set PROGRAM=monprogramme
set MCU=atmega328p
set PORT=COM3

echo Compilation...
fpc -Pavr -Tembedded -Wp%MCU% -O2 %PROGRAM%.pas

if %errorlevel%==0 (
    echo Conversion en HEX...
    avr-objcopy -O ihex %PROGRAM%.elf %PROGRAM%.hex

    echo Flashage...
    avrdude -p %MCU% -c arduino -P %PORT% -b 115200 -U flash:w:%PROGRAM%.hex:i

    echo Terminé!
) else (
    echo Erreur de compilation!
)
```

## Bibliothèques et helpers

### Créer une bibliothèque réutilisable

**unit_gpio.pas :**

```pascal
unit unit_gpio;

{$mode objfpc}
{$H+}

interface

uses
  atmega328p;

// Modes de broches
type
  TPinMode = (pmInput, pmOutput, pmInputPullup);
  TPinState = (psLow, psHigh);

// Fonctions publiques
procedure pinMode(pin: byte; mode: TPinMode);
procedure digitalWrite(pin: byte; state: TPinState);
function digitalRead(pin: byte): TPinState;
procedure togglePin(pin: byte);

implementation

// Mappage pin Arduino → (Port, Bit)
type
  TPinMapping = record
    DDR: ^byte;   // Registre direction
    PORT: ^byte;  // Registre sortie
    PIN: ^byte;   // Registre lecture
    bit: byte;    // Numéro de bit
  end;

const
  PinMap: array[0..13] of TPinMapping = (
    (DDR: @DDRD; PORT: @PORTD; PIN: @PIND; bit: 0),  // Pin 0 (RX)
    (DDR: @DDRD; PORT: @PORTD; PIN: @PIND; bit: 1),  // Pin 1 (TX)
    (DDR: @DDRD; PORT: @PORTD; PIN: @PIND; bit: 2),  // Pin 2
    (DDR: @DDRD; PORT: @PORTD; PIN: @PIND; bit: 3),  // Pin 3 (PWM)
    (DDR: @DDRD; PORT: @PORTD; PIN: @PIND; bit: 4),  // Pin 4
    (DDR: @DDRD; PORT: @PORTD; PIN: @PIND; bit: 5),  // Pin 5 (PWM)
    (DDR: @DDRD; PORT: @PORTD; PIN: @PIND; bit: 6),  // Pin 6 (PWM)
    (DDR: @DDRD; PORT: @PORTD; PIN: @PIND; bit: 7),  // Pin 7
    (DDR: @DDRB; PORT: @PORTB; PIN: @PINB; bit: 0),  // Pin 8
    (DDR: @DDRB; PORT: @PORTB; PIN: @PINB; bit: 1),  // Pin 9 (PWM)
    (DDR: @DDRB; PORT: @PORTB; PIN: @PINB; bit: 2),  // Pin 10 (PWM)
    (DDR: @DDRB; PORT: @PORTB; PIN: @PINB; bit: 3),  // Pin 11 (PWM)
    (DDR: @DDRB; PORT: @PORTB; PIN: @PINB; bit: 4),  // Pin 12
    (DDR: @DDRB; PORT: @PORTB; PIN: @PINB; bit: 5)   // Pin 13 (LED)
  );

procedure pinMode(pin: byte; mode: TPinMode);
var
  mapping: TPinMapping;
begin
  if pin > 13 then Exit;
  mapping := PinMap[pin];

  case mode of
    pmOutput:
      mapping.DDR^ := mapping.DDR^ or (1 shl mapping.bit);
    pmInput:
      begin
        mapping.DDR^ := mapping.DDR^ and not (1 shl mapping.bit);
        mapping.PORT^ := mapping.PORT^ and not (1 shl mapping.bit);
      end;
    pmInputPullup:
      begin
        mapping.DDR^ := mapping.DDR^ and not (1 shl mapping.bit);
        mapping.PORT^ := mapping.PORT^ or (1 shl mapping.bit);
      end;
  end;
end;

procedure digitalWrite(pin: byte; state: TPinState);
var
  mapping: TPinMapping;
begin
  if pin > 13 then Exit;
  mapping := PinMap[pin];

  if state = psHigh then
    mapping.PORT^ := mapping.PORT^ or (1 shl mapping.bit)
  else
    mapping.PORT^ := mapping.PORT^ and not (1 shl mapping.bit);
end;

function digitalRead(pin: byte): TPinState;
var
  mapping: TPinMapping;
begin
  if pin > 13 then Exit(psLow);
  mapping := PinMap[pin];

  if (mapping.PIN^ and (1 shl mapping.bit)) <> 0 then
    Result := psHigh
  else
    Result := psLow;
end;

procedure togglePin(pin: byte);
var
  mapping: TPinMapping;
begin
  if pin > 13 then Exit;
  mapping := PinMap[pin];

  // Toggle en XOR
  mapping.PORT^ := mapping.PORT^ xor (1 shl mapping.bit);
end;

end.
```

**Utilisation de la bibliothèque :**

```pascal
program TestGPIO;

{$mode objfpc}

uses
  unit_gpio, intrinsics;

const
  LED_PIN = 13;
  BUTTON_PIN = 2;

begin
  // Configuration
  pinMode(LED_PIN, pmOutput);
  pinMode(BUTTON_PIN, pmInputPullup);

  // Boucle principale
  while true do
  begin
    if digitalRead(BUTTON_PIN) = psLow then
      digitalWrite(LED_PIN, psHigh)
    else
      digitalWrite(LED_PIN, psLow);

    avr_delay_ms(50);
  end;
end.
```

### Bibliothèque UART simplifiée

**unit_uart.pas :**

```pascal
unit unit_uart;

{$mode objfpc}
{$H+}

interface

const
  F_CPU = 16000000;

procedure UART_Init(baud: longword);
procedure UART_Write(data: byte);
function UART_Read: byte;
function UART_Available: boolean;
procedure UART_Print(const s: string);
procedure UART_PrintLn(const s: string);
procedure UART_PrintInt(value: integer);

implementation

uses
  atmega328p;

procedure UART_Init(baud: longword);
var
  ubrr: word;
begin
  ubrr := (F_CPU div (16 * baud)) - 1;

  UBRR0H := Hi(ubrr);
  UBRR0L := Lo(ubrr);

  UCSR0B := (1 shl RXEN0) or (1 shl TXEN0);
  UCSR0C := (1 shl UCSZ01) or (1 shl UCSZ00);
end;

procedure UART_Write(data: byte);
begin
  while (UCSR0A and (1 shl UDRE0)) = 0 do
    ;
  UDR0 := data;
end;

function UART_Read: byte;
begin
  while (UCSR0A and (1 shl RXC0)) = 0 do
    ;
  Result := UDR0;
end;

function UART_Available: boolean;
begin
  Result := (UCSR0A and (1 shl RXC0)) <> 0;
end;

procedure UART_Print(const s: string);
var
  i: integer;
begin
  for i := 1 to Length(s) do
    UART_Write(Ord(s[i]));
end;

procedure UART_PrintLn(const s: string);
begin
  UART_Print(s);
  UART_Write(13);  // CR
  UART_Write(10);  // LF
end;

procedure UART_PrintInt(value: integer);
var
  s: string;
begin
  Str(value, s);
  UART_Print(s);
end;

end.
```

**Exemple d'utilisation :**

```pascal
program TestUART;

uses
  unit_uart, unit_gpio, intrinsics;

var
  compteur: integer = 0;
  commande: char;

begin
  UART_Init(9600);
  pinMode(13, pmOutput);

  UART_PrintLn('Arduino FreePascal');
  UART_PrintLn('Tapez "1" pour allumer, "0" pour eteindre');

  while true do
  begin
    // Envoyer le compteur
    UART_Print('Compteur: ');
    UART_PrintInt(compteur);
    UART_PrintLn('');

    Inc(compteur);

    // Vérifier si des données sont disponibles
    if UART_Available then
    begin
      commande := Chr(UART_Read);

      case commande of
        '1': begin
               digitalWrite(13, psHigh);
               UART_PrintLn('LED ON');
             end;
        '0': begin
               digitalWrite(13, psLow);
               UART_PrintLn('LED OFF');
             end;
        't': begin
               togglePin(13);
               UART_PrintLn('LED TOGGLE');
             end;
      end;
    end;

    avr_delay_ms(1000);
  end;
end.
```

## Gestion avancée de l'EEPROM

L'EEPROM permet de sauvegarder des données qui persistent après extinction :

**unit_eeprom.pas :**

```pascal
unit unit_eeprom;

{$mode objfpc}
{$H+}

interface

procedure EEPROM_Write(address: word; data: byte);
function EEPROM_Read(address: word): byte;
procedure EEPROM_WriteBlock(address: word; const buffer: array of byte; size: word);
procedure EEPROM_ReadBlock(address: word; var buffer: array of byte; size: word);

implementation

uses
  atmega328p;

procedure EEPROM_Write(address: word; data: byte);
begin
  // Attendre la fin de l'écriture précédente
  while (EECR and (1 shl EEPE)) <> 0 do
    ;

  // Définir l'adresse et la donnée
  EEARH := Hi(address);
  EEARL := Lo(address);
  EEDR := data;

  // Séquence d'écriture (obligatoire)
  EECR := EECR or (1 shl EEMPE);  // Master Write Enable
  EECR := EECR or (1 shl EEPE);   // Write Enable
end;

function EEPROM_Read(address: word): byte;
begin
  // Attendre la fin de l'écriture en cours
  while (EECR and (1 shl EEPE)) <> 0 do
    ;

  // Définir l'adresse
  EEARH := Hi(address);
  EEARL := Lo(address);

  // Démarrer la lecture
  EECR := EECR or (1 shl EERE);

  // Retourner la donnée
  Result := EEDR;
end;

procedure EEPROM_WriteBlock(address: word; const buffer: array of byte; size: word);
var
  i: word;
begin
  for i := 0 to size - 1 do
    EEPROM_Write(address + i, buffer[i]);
end;

procedure EEPROM_ReadBlock(address: word; var buffer: array of byte; size: word);
var
  i: word;
begin
  for i := 0 to size - 1 do
    buffer[i] := EEPROM_Read(address + i);
end;

end.
```

**Exemple : sauvegarder un compteur :**

```pascal
program CompteurPersistant;

uses
  unit_eeprom, unit_uart, intrinsics;

const
  EEPROM_ADDR_COMPTEUR = 0;

var
  compteur: word;

begin
  UART_Init(9600);

  // Lire le compteur sauvegardé
  compteur := EEPROM_Read(EEPROM_ADDR_COMPTEUR);
  compteur := compteur or (EEPROM_Read(EEPROM_ADDR_COMPTEUR + 1) shl 8);

  UART_Print('Compteur restaure: ');
  UART_PrintInt(compteur);
  UART_PrintLn('');

  while true do
  begin
    Inc(compteur);

    UART_Print('Compteur: ');
    UART_PrintInt(compteur);
    UART_PrintLn('');

    // Sauvegarder toutes les 10 secondes
    if (compteur mod 10) = 0 then
    begin
      EEPROM_Write(EEPROM_ADDR_COMPTEUR, Lo(compteur));
      EEPROM_Write(EEPROM_ADDR_COMPTEUR + 1, Hi(compteur));
      UART_PrintLn('Sauvegarde...');
    end;

    avr_delay_ms(1000);
  end;
end.
```

**Note importante :** L'EEPROM a une durée de vie limitée (environ 100 000 écritures par cellule). Évitez d'écrire trop fréquemment.

## Interfaces avancées

### I2C (TWI sur AVR)

Le protocole I2C permet de connecter plusieurs périphériques (capteurs, écrans, etc.) sur seulement 2 fils.

**Exemple : lecture capteur de température DS1621**

```pascal
unit unit_i2c;

{$mode objfpc}

interface

procedure I2C_Init;
procedure I2C_Start;
procedure I2C_Stop;
procedure I2C_Write(data: byte);
function I2C_Read(ack: boolean): byte;

implementation

uses
  atmega328p;

const
  F_CPU = 16000000;
  I2C_FREQ = 100000;  // 100 kHz
  TWBR_VALUE = ((F_CPU div I2C_FREQ) - 16) div 2;

procedure I2C_Init;
begin
  TWBR := TWBR_VALUE;
  TWSR := 0;  // Prescaler = 1
  TWCR := (1 shl TWEN);  // Activer TWI
end;

procedure I2C_Start;
begin
  TWCR := (1 shl TWINT) or (1 shl TWSTA) or (1 shl TWEN);
  while (TWCR and (1 shl TWINT)) = 0 do
    ;
end;

procedure I2C_Stop;
begin
  TWCR := (1 shl TWINT) or (1 shl TWSTO) or (1 shl TWEN);
end;

procedure I2C_Write(data: byte);
begin
  TWDR := data;
  TWCR := (1 shl TWINT) or (1 shl TWEN);
  while (TWCR and (1 shl TWINT)) = 0 do
    ;
end;

function I2C_Read(ack: boolean): byte;
begin
  if ack then
    TWCR := (1 shl TWINT) or (1 shl TWEN) or (1 shl TWEA)
  else
    TWCR := (1 shl TWINT) or (1 shl TWEN);

  while (TWCR and (1 shl TWINT)) = 0 do
    ;

  Result := TWDR;
end;

end.
```

**Utilisation avec un capteur :**

```pascal
program TestI2C;

uses
  unit_i2c, unit_uart, intrinsics;

const
  DS1621_ADDR = $48;  // Adresse I2C du capteur
  CMD_START = $EE;
  CMD_READ_TEMP = $AA;

function LireTemperature: integer;
begin
  // Envoyer commande de lecture
  I2C_Start;
  I2C_Write((DS1621_ADDR shl 1) or 0);  // Adresse + Write
  I2C_Write(CMD_READ_TEMP);
  I2C_Stop;

  // Lire la température
  I2C_Start;
  I2C_Write((DS1621_ADDR shl 1) or 1);  // Adresse + Read
  Result := I2C_Read(true);  // Byte haut
  I2C_Read(false);           // Byte bas (ignoré pour simplifier)
  I2C_Stop;
end;

var
  temperature: integer;

begin
  UART_Init(9600);
  I2C_Init;

  // Démarrer la conversion continue
  I2C_Start;
  I2C_Write((DS1621_ADDR shl 1) or 0);
  I2C_Write(CMD_START);
  I2C_Stop;

  UART_PrintLn('Lecture temperature...');

  while true do
  begin
    temperature := LireTemperature;

    UART_Print('Temperature: ');
    UART_PrintInt(temperature);
    UART_PrintLn(' C');

    avr_delay_ms(2000);
  end;
end.
```

### SPI (Serial Peripheral Interface)

Le SPI est un protocole rapide pour communiquer avec des périphériques comme les cartes SD, écrans TFT, modules RF, etc.

**unit_spi.pas :**

```pascal
unit unit_spi;

{$mode objfpc}

interface

procedure SPI_Init;
function SPI_Transfer(data: byte): byte;

implementation

uses
  atmega328p, unit_gpio;

const
  PIN_MOSI = 11;
  PIN_MISO = 12;
  PIN_SCK  = 13;
  PIN_SS   = 10;

procedure SPI_Init;
begin
  // Configurer les broches
  pinMode(PIN_MOSI, pmOutput);
  pinMode(PIN_SCK, pmOutput);
  pinMode(PIN_SS, pmOutput);
  pinMode(PIN_MISO, pmInput);

  // SS haut (désactivé)
  digitalWrite(PIN_SS, psHigh);

  // Activer SPI, Master, Clock/16
  SPCR := (1 shl SPE) or (1 shl MSTR) or (1 shl SPR0);
end;

function SPI_Transfer(data: byte): byte;
begin
  SPDR := data;
  while (SPSR and (1 shl SPIF)) = 0 do
    ;
  Result := SPDR;
end;

end.
```

## Optimisation mémoire et performance

### Techniques d'optimisation

#### 1. Utiliser les types minimaux

```pascal
// Gaspillage
var
  compteur: integer;     // 4 octets (si 32 bits)
  flag: boolean;         // 1 octet
  valeurs: array[0..99] of integer;  // 400 octets

// Optimisé
var
  compteur: byte;        // 1 octet (si < 256)
  flag: boolean;         // 1 octet
  valeurs: array[0..99] of byte;     // 100 octets
```

#### 2. Constantes en Flash

```pascal
const
  Message: string = 'Erreur systeme';  // En Flash
  Table: array[0..15] of byte = (
    $00, $11, $22, $33, $44, $55, $66, $77,
    $88, $99, $AA, $BB, $CC, $DD, $EE, $FF
  );  // En Flash
```

#### 3. Éviter les allocations dynamiques

```pascal
// À ÉVITER
var
  liste: ^array of integer;
begin
  New(liste);
  // ...
  Dispose(liste);
end;

// PRÉFÉRER
var
  liste: array[0..9] of integer;  // Taille fixe
begin
  // Pas d'allocation
end;
```

#### 4. Optimiser les boucles

```pascal
// Moins efficace
for i := 0 to 99 do
  tableau[i] := 0;

// Plus efficace (avec FillChar)
FillChar(tableau, SizeOf(tableau), 0);

// Ou en assembleur inline
asm
  ldi r16, 0
  ldi r17, 100
@loop:
  st Z+, r16
  dec r17
  brne @loop
end;
```

#### 5. Variables globales vs locales

```pascal
// Variables globales : en RAM statique (rapide)
var
  compteur_global: byte;

// Variables locales : sur la pile (peut être plus lent)
procedure MaProc;
var
  compteur_local: byte;
begin
  // ...
end;
```

**Conseil :** Pour les variables fréquemment utilisées, préférez les variables globales sur AVR.

### Analyse de la mémoire

**Après compilation, vérifier :**

```bash
avr-size monprogramme.elf
```

**Sortie :**
```
   text    data     bss     dec     hex filename
   1456      12     234    1702     6a6 monprogramme.elf
```

- **text** : Code programme (en Flash)
- **data** : Variables initialisées (copiées de Flash vers RAM au démarrage)
- **bss** : Variables non initialisées (en RAM)
- **dec/hex** : Total

**Limites ATmega328P :**
- Flash : 32 Ko (32768 octets)
- RAM : 2 Ko (2048 octets)

Si vous dépassez, le programme ne fonctionnera pas correctement !

## Débogage sur AVR

### Techniques de débogage

#### 1. LED de debug

La méthode la plus simple :

```pascal
procedure DebugBlink(fois: byte);
var
  i: byte;
begin
  for i := 1 to fois do
  begin
    digitalWrite(13, psHigh);
    avr_delay_ms(200);
    digitalWrite(13, psLow);
    avr_delay_ms(200);
  end;
  avr_delay_ms(1000);
end;

// Utilisation
begin
  if erreur then
    DebugBlink(3);  // 3 clignotements = erreur type 3
end;
```

#### 2. UART debug

Envoyer des messages :

```pascal
procedure Debug(const msg: string; value: integer);
begin
  UART_Print('[DEBUG] ');
  UART_Print(msg);
  UART_Print(': ');
  UART_PrintInt(value);
  UART_PrintLn('');
end;

// Utilisation
begin
  valeur := ADC_Read(0);
  Debug('ADC', valeur);
end;
```

#### 3. Simulateur

**SimAVR** permet de simuler un AVR sans matériel :

```bash
# Ubuntu
sudo apt install simavr

# Simuler
simavr -m atmega328p -f 16000000 monprogramme.elf
```

#### 4. Debugging matériel (pour experts)

Avec un programmeur JTAG/debugWIRE :
- Points d'arrêt
- Inspection des variables
- Pas-à-pas

**Outils :** AVR Dragon, Atmel-ICE

## Projets complets

### Projet 1 : Thermostat simple

```pascal
program Thermostat;

uses
  unit_gpio, unit_uart, intrinsics, atmega328p;

const
  PIN_RELAY = 8;      // Relais chauffage
  PIN_SENSOR = 0;     // Capteur température (LM35)
  TEMP_SEUIL = 512;   // ~25°C (LM35 : 10mV/°C)

var
  temperature: word;
  chauffage_actif: boolean = false;

function LireTemperature: word;
var
  somme: longword;
  i: byte;
begin
  somme := 0;

  // Moyenner 8 lectures
  for i := 0 to 7 do
  begin
    somme := somme + ADC_Read(PIN_SENSOR);
    avr_delay_ms(10);
  end;

  Result := somme shr 3;  // Diviser par 8
end;

procedure ControlerChauffage;
begin
  temperature := LireTemperature;

  if temperature < TEMP_SEUIL then
  begin
    if not chauffage_actif then
    begin
      digitalWrite(PIN_RELAY, psHigh);
      chauffage_actif := true;
      UART_PrintLn('Chauffage ON');
    end;
  end
  else
  begin
    if chauffage_actif then
    begin
      digitalWrite(PIN_RELAY, psLow);
      chauffage_actif := false;
      UART_PrintLn('Chauffage OFF');
    end;
  end;
end;

procedure ADC_Init;
begin
  ADMUX := (1 shl REFS0);  // AVCC reference
  ADCSRA := (1 shl ADEN) or (1 shl ADPS2) or (1 shl ADPS1) or (1 shl ADPS0);
end;

begin
  UART_Init(9600);
  ADC_Init;
  pinMode(PIN_RELAY, pmOutput);

  UART_PrintLn('Thermostat FreePascal');
  UART_Print('Seuil: ');
  UART_PrintInt(TEMP_SEUIL);
  UART_PrintLn('');

  while true do
  begin
    ControlerChauffage;

    UART_Print('Temperature: ');
    UART_PrintInt(temperature);
    UART_PrintLn('');

    avr_delay_ms(5000);  // Vérifier toutes les 5 secondes
  end;
end.
```

### Projet 2 : Logger de données sur SD

Nécessite un module SD Card avec interface SPI.

```pascal
program DataLogger;

uses
  unit_spi, unit_gpio, unit_uart, intrinsics;

const
  PIN_CS_SD = 10;

var
  compteur: word = 0;

procedure SD_Select;
begin
  digitalWrite(PIN_CS_SD, psLow);
end;

procedure SD_Deselect;
begin
  digitalWrite(PIN_CS_SD, psHigh);
  SPI_Transfer($FF);  // Cycle horloge supplémentaire
end;

// Initialisation simplifiée de la carte SD
function SD_Init: boolean;
var
  i: byte;
  response: byte;
begin
  Result := false;

  // Attendre que la carte soit prête
  SD_Deselect;
  for i := 0 to 9 do
    SPI_Transfer($FF);

  // Envoyer CMD0 (reset)
  SD_Select;
  SPI_Transfer($40);  // CMD0
  SPI_Transfer($00);
  SPI_Transfer($00);
  SPI_Transfer($00);
  SPI_Transfer($00);
  SPI_Transfer($95);  // CRC

  // Attendre réponse
  for i := 0 to 7 do
  begin
    response := SPI_Transfer($FF);
    if response = $01 then
    begin
      Result := true;
      Break;
    end;
  end;

  SD_Deselect;
end;

begin
  UART_Init(9600);
  SPI_Init;
  pinMode(PIN_CS_SD, pmOutput);

  UART_PrintLn('Data Logger FreePascal');

  if SD_Init then
    UART_PrintLn('Carte SD initialisee')
  else
  begin
    UART_PrintLn('ERREUR: Carte SD non detectee');
    while true do;  // Arrêt
  end;

  while true do
  begin
    // Logger des données
    Inc(compteur);

    UART_Print('Log #');
    UART_PrintInt(compteur);
    UART_PrintLn('');

    // Ici : écriture sur SD (simplifié)

    avr_delay_ms(10000);  // Toutes les 10 secondes
  end;
end.
```

## Ressources et aller plus loin

### Documentation officielle

- **FreePascal AVR Wiki** : https://wiki.freepascal.org/AVR_Embedded
- **Atmel ATmega328P Datasheet** : spécifications complètes du microcontrôleur
- **Arduino Reference** : pour comparaison avec l'API Arduino C++

### Bibliothèques recommandées

- **FPCKit** : collection d'unités pour AVR
- **AVRPascal** : bibliothèques de la communauté
- **Arduino for FreePascal** : port des bibliothèques Arduino

### Forums et communauté

- **Forum FreePascal** : section Embedded Systems
- **AVR Freaks** : communauté AVR générale (C/C++ majoritaire)
- **Reddit r/freepascal** : discussions et aide

### Outils utiles

- **Fritzing** : dessiner des schémas électroniques
- **KiCad** : conception de PCB professionnels
- **PlatformIO** : environnement de développement multi-plateforme (avec support expérimental FPC)

### Livres et tutoriels

- **"Make: AVR Programming"** : excellent pour comprendre AVR (en C, mais concepts applicables)
- **Documentation FreePascal officielle** : chapitre sur l'embarqué
- **Tutoriels vidéo YouTube** : rechercher "FreePascal AVR" ou "Lazarus Arduino"

## Conclusion

FreePascal sur AVR/Arduino offre une alternative intéressante au C/C++ Arduino classique :

**Avantages confirmés :**
- Syntaxe Pascal claire et structurée
- Typage fort réduisant les erreurs
- Même langage du desktop à l'embarqué
- Bonnes performances (code compilé natif)
- Génération de code compact

**Défis à relever :**
- Écosystème moins mature que C/C++ Arduino
- Bibliothèques moins nombreuses
- Documentation parfois fragmentée
- Communauté plus restreinte
- Besoin de configuration initiale plus complexe

**Cas d'usage idéaux :**
- Développeurs Pascal souhaitant faire de l'électronique
- Projets nécessitant du code partagé entre PC et microcontrôleur
- Enseignement avec un langage structuré
- Prototypage rapide avec syntaxe lisible
- Migration d'anciens projets Turbo Pascal vers l'embarqué

**Comparaison finale :**

| Aspect | FreePascal AVR | Arduino C++ |
|--------|---------------|-------------|
| Syntaxe | Claire, structurée | Complexe (pointeurs, etc.) |
| Performance | Excellente | Excellente |
| Bibliothèques | Limitées (~50) | Très nombreuses (>5000) |
| IDE | Lazarus/éditeurs texte | Arduino IDE (simple) |
| Courbe d'apprentissage | Moyenne | Moyenne |
| Communauté | Petite mais active | Très grande |
| Documentation | Correcte | Excellente |
| Maturité | En développement | Très mature |
| Portabilité code | Excellente (PC↔AVR) | Moyenne |

### Recommandations finales

**Choisir FreePascal AVR si :**
- Vous connaissez déjà Pascal/Delphi
- Vous voulez un code propre et maintenable
- Vous développez en parallèle sur PC et microcontrôleur
- Vous aimez les défis techniques
- Vous contribuez à des projets open source

**Choisir Arduino C++ si :**
- Vous débutez en électronique
- Vous avez besoin de nombreuses bibliothèques
- Vous voulez une large communauté d'entraide
- Vous privilégiez la rapidité de développement
- Vous utilisez des shields/modules du commerce

## Annexes

### Annexe A : Table de mappage des broches Arduino Uno

| Pin Arduino | Port AVR | Bit | Fonction spéciale |
|-------------|----------|-----|-------------------|
| 0 | PORTD | 0 | RX (UART) |
| 1 | PORTD | 1 | TX (UART) |
| 2 | PORTD | 2 | INT0 |
| 3 | PORTD | 3 | INT1, PWM (OC2B) |
| 4 | PORTD | 4 | - |
| 5 | PORTD | 5 | PWM (OC0B) |
| 6 | PORTD | 6 | PWM (OC0A) |
| 7 | PORTD | 7 | - |
| 8 | PORTB | 0 | - |
| 9 | PORTB | 1 | PWM (OC1A) |
| 10 | PORTB | 2 | PWM (OC1B), SS (SPI) |
| 11 | PORTB | 3 | PWM (OC2A), MOSI (SPI) |
| 12 | PORTB | 4 | MISO (SPI) |
| 13 | PORTB | 5 | SCK (SPI), LED intégrée |
| A0 | PORTC | 0 | ADC0 |
| A1 | PORTC | 1 | ADC1 |
| A2 | PORTC | 2 | ADC2 |
| A3 | PORTC | 3 | ADC3 |
| A4 | PORTC | 4 | ADC4, SDA (I2C) |
| A5 | PORTC | 5 | ADC5, SCL (I2C) |

### Annexe B : Registres importants ATmega328P

#### Registres GPIO

```pascal
// Port B (pins 8-13)
DDRB  : byte at $24;   // Direction
PORTB : byte at $25;   // Sortie
PINB  : byte at $23;   // Lecture

// Port C (pins A0-A5)
DDRC  : byte at $27;
PORTC : byte at $28;
PINC  : byte at $26;

// Port D (pins 0-7)
DDRD  : byte at $2A;
PORTD : byte at $2B;
PIND  : byte at $29;
```

#### Registres Timer 0 (8 bits)

```pascal
TCCR0A : byte at $44;  // Control Register A
TCCR0B : byte at $45;  // Control Register B
TCNT0  : byte at $46;  // Counter value
OCR0A  : byte at $47;  // Output Compare A
OCR0B  : byte at $48;  // Output Compare B
TIMSK0 : byte at $6E;  // Interrupt Mask
TIFR0  : byte at $35;  // Interrupt Flags
```

#### Registres UART

```pascal
UCSR0A : byte at $C0;  // Status Register A
UCSR0B : byte at $C1;  // Control Register B
UCSR0C : byte at $C2;  // Control Register C
UBRR0L : byte at $C4;  // Baud Rate Low
UBRR0H : byte at $C5;  // Baud Rate High
UDR0   : byte at $C6;  // Data Register
```

#### Registres ADC

```pascal
ADMUX  : byte at $7C;  // Multiplexer Selection
ADCSRA : byte at $7A;  // Control and Status A
ADCSRB : byte at $7B;  // Control and Status B
ADCL   : byte at $78;  // Data Register Low
ADCH   : byte at $79;  // Data Register High
ADC    : word at $78;  // Data Register (16 bits)
```

### Annexe C : Bits de configuration importants

#### DDRB/C/D (Data Direction Register)

- `0` = Entrée (Input)
- `1` = Sortie (Output)

#### PORTB/C/D (Port Data Register)

- En **sortie** : `0` = LOW, `1` = HIGH
- En **entrée** : `0` = Pull-up désactivé, `1` = Pull-up activé

#### TCCR0A/B (Timer Control)

**Bits WGM (Waveform Generation Mode) :**
- `000` = Normal
- `001` = PWM Phase Correct
- `010` = CTC (Clear Timer on Compare)
- `011` = Fast PWM

**Bits CS (Clock Select) - Prescaler :**
- `000` = Arrêt
- `001` = clk/1 (pas de prescaler)
- `010` = clk/8
- `011` = clk/64
- `100` = clk/256
- `101` = clk/1024

#### UCSR0B (UART Control)

- `RXEN0` (bit 4) : Activer réception
- `TXEN0` (bit 3) : Activer transmission
- `RXCIE0` (bit 7) : Interruption RX
- `TXCIE0` (bit 6) : Interruption TX

#### ADCSRA (ADC Control)

- `ADEN` (bit 7) : Activer ADC
- `ADSC` (bit 6) : Démarrer conversion
- `ADATE` (bit 5) : Auto-trigger
- `ADIF` (bit 4) : Interrupt flag
- `ADIE` (bit 3) : Interrupt enable
- `ADPS[2:0]` (bits 0-2) : Prescaler (128 recommandé pour 16 MHz)

### Annexe D : Calculs utiles

#### Calcul du Baud Rate (UART)

```
UBRR = (F_CPU / (16 × Baud)) - 1

Exemples pour F_CPU = 16 MHz :
- 9600 baud   → UBRR = 103
- 19200 baud  → UBRR = 51
- 38400 baud  → UBRR = 25
- 57600 baud  → UBRR = 16
- 115200 baud → UBRR = 8
```

#### Calcul de fréquence Timer

```
Fréquence interruption = F_CPU / (Prescaler × (1 + OCR))

Exemple pour 1 Hz (1 seconde) avec F_CPU = 16 MHz :
Prescaler = 256
OCR = (16 000 000 / (256 × 1)) - 1 = 62499
```

#### Conversion ADC vers tension

```
Tension (V) = (ADC_value × Vref) / 1024

Avec Vref = 5V :
ADC = 0    → 0V
ADC = 512  → 2.5V
ADC = 1023 → 5V
```

#### Délai avec boucle

Pour un délai approximatif en cycles :

```pascal
// Chaque itération ≈ 4 cycles
for i := 0 to cycles div 4 do
  asm nop; end;

// Pour 1 ms à 16 MHz :
// 16 000 cycles par ms
// donc boucle de 4000 itérations
```

### Annexe E : Erreurs courantes et solutions

#### Erreur 1 : Programme ne démarre pas

**Symptômes :** LED ne clignote pas, rien ne se passe

**Causes possibles :**
- Mauvais fichier HEX flashé
- Mauvaise configuration des fuses
- Problème d'alimentation
- Mauvais type de microcontrôleur sélectionné

**Solutions :**
```bash
# Vérifier les fuses
avrdude -p atmega328p -c arduino -P /dev/ttyACM0 -U lfuse:r:-:h -U hfuse:r:-:h

# Fuses par défaut Arduino Uno :
# lfuse = 0xFF (horloge externe 16 MHz)
# hfuse = 0xDE (bootloader à 0x7E00)
# efuse = 0xFD
```

#### Erreur 2 : Dépassement de mémoire

**Symptômes :** Compilation échoue avec "out of memory"

**Solutions :**
- Réduire les tableaux
- Utiliser des types plus petits (byte au lieu d'integer)
- Mettre les constantes en Flash
- Éviter les chaînes longues
- Activer les optimisations : `-O2` ou `-O3`

#### Erreur 3 : UART ne fonctionne pas

**Causes possibles :**
- Mauvaise vitesse (baud rate)
- Câblage inversé (RX/TX)
- Pins 0/1 utilisées pour autre chose

**Solutions :**
```pascal
// Vérifier le calcul UBRR
const
  F_CPU = 16000000;
  BAUD = 9600;
  UBRR_VALUE = (F_CPU div (16 * BAUD)) - 1;  // Doit donner 103
```

#### Erreur 4 : Timer imprécis

**Causes :** Interruptions trop longues, horloge mal configurée

**Solutions :**
- Garder les ISR courtes (< 10 µs)
- Vérifier F_CPU correspond à l'horloge réelle
- Utiliser Timer 1 (16 bits) pour plus de précision

#### Erreur 5 : ADC lectures instables

**Solutions :**
```pascal
// Moyenner plusieurs lectures
function ADC_Read_Stable(channel: byte): word;
var
  i: byte;
  somme: longword;
begin
  somme := 0;
  for i := 0 to 7 do
  begin
    somme := somme + ADC_Read(channel);
    avr_delay_ms(2);  // Attendre entre lectures
  end;
  Result := somme shr 3;  // Diviser par 8
end;
```

### Annexe F : Makefile exemple

**Makefile pour automatiser la compilation et le flashage :**

```makefile
# Configuration
PROGRAM = monprogramme
MCU = atmega328p
F_CPU = 16000000
PORT = /dev/ttyACM0
BAUD = 115200

# Outils
FPC = fpc
OBJCOPY = avr-objcopy
AVRDUDE = avrdude

# Options de compilation
FPCFLAGS = -Pavr -Tembedded -Wp$(MCU) -O2 -dF_CPU=$(F_CPU)

# Cibles
all: $(PROGRAM).hex

$(PROGRAM).elf: $(PROGRAM).pas
	$(FPC) $(FPCFLAGS) $(PROGRAM).pas

$(PROGRAM).hex: $(PROGRAM).elf
	$(OBJCOPY) -O ihex $(PROGRAM).elf $(PROGRAM).hex

flash: $(PROGRAM).hex
	$(AVRDUDE) -p $(MCU) -c arduino -P $(PORT) -b $(BAUD) \
		-U flash:w:$(PROGRAM).hex:i

clean:
	rm -f $(PROGRAM).elf $(PROGRAM).hex $(PROGRAM).o
	rm -f *.ppu *.o link.res

monitor:
	screen $(PORT) 9600

.PHONY: all flash clean monitor
```

**Utilisation :**
```bash
make          # Compiler
make flash    # Flasher
make monitor  # Ouvrir moniteur série
make clean    # Nettoyer
```

### Annexe G : Configuration VSCode

**settings.json :**

```json
{
  "files.associations": {
    "*.pas": "pascal"
  },
  "pascal.formatter.engine": "ptop",
  "pascal.codeNavigation": "workspace",
  "terminal.integrated.shell.linux": "/bin/bash",
  "tasks": {
    "version": "2.0.0",
    "tasks": [
      {
        "label": "Build AVR",
        "type": "shell",
        "command": "fpc",
        "args": [
          "-Pavr",
          "-Tembedded",
          "-Wpatmega328p",
          "-O2",
          "${file}"
        ],
        "group": {
          "kind": "build",
          "isDefault": true
        }
      },
      {
        "label": "Flash AVR",
        "type": "shell",
        "command": "avrdude",
        "args": [
          "-p", "atmega328p",
          "-c", "arduino",
          "-P", "/dev/ttyACM0",
          "-b", "115200",
          "-U", "flash:w:${fileBasenameNoExtension}.hex:i"
        ]
      }
    ]
  }
}
```

### Annexe H : Schémas de câblage courants

#### Montage LED + résistance

```
Arduino Pin 13 ----[Résistance 220Ω]----[LED]----GND

Calcul résistance :
R = (Vcc - Vled) / I
R = (5V - 2V) / 0.020A = 150Ω (220Ω standard)
```

#### Montage bouton avec pull-up

```
                    +5V
                     |
                  [10kΩ]
                     |
Arduino Pin 2 -------+-------[Bouton]-----GND

Ou utiliser le pull-up interne (20-50kΩ)
```

#### Montage potentiomètre (ADC)

```
       +5V
        |
     [Pot 10kΩ]
    /     |     \
   /   Curseur   \
  |       |       |
  |   Arduino A0  |
  |               |
  +5V            GND
```

#### Montage capteur température LM35

```
LM35 (vu de face, plat vers soi) :

  +5V  Vout  GND
   |    |    |
   1    2    3
        |
    Arduino A0

Précision : 10mV/°C
0°C = 0V, 25°C = 250mV, 100°C = 1V
```

## Prochaines étapes

Maintenant que vous maîtrisez FreePascal sur AVR/Arduino, vous pouvez :

1. **Explorer ARM Cortex-M** (section 14.3) : microcontrôleurs plus puissants (STM32)
2. **Approfondir les protocoles** : I2C, SPI, OneWire en détail
3. **Créer des projets IoT** : ESP32 avec WiFi/Bluetooth
4. **Interfacer des capteurs complexes** : GPS, accéléromètres, écrans LCD/OLED
5. **Développer des bibliothèques** : contribuer à la communauté FreePascal
6. **Optimiser pour le temps réel** : systèmes critiques, RTOS

**N'oubliez pas :**
- Commencez simple (LED clignotante)
- Testez chaque partie séparément
- Documentez votre code
- Partagez vos projets avec la communauté
- Consultez toujours le datasheet du microcontrôleur

**Bon développement avec FreePascal sur AVR ! 🚀**

---

*Ce tutoriel fait partie de la formation "FreePascal/Lazarus - Niveau Développeur Avancé - Edition Multi-plateforme Windows/Ubuntu"*

*Prochaine section : 14.3 ARM Cortex-M et STM32*

⏭️ [ARM Cortex-M et STM32](/14-systemes-embarques-iot/03-arm-cortex-m-stm32.md)
