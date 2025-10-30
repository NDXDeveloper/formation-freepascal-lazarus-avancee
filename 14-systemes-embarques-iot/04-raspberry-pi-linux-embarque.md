🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.4 Raspberry Pi et Linux embarqué

## Introduction

Le **Raspberry Pi** représente une catégorie unique dans le monde de l'embarqué : c'est un **ordinateur complet miniaturisé** capable de faire tourner un système d'exploitation Linux complet, tout en offrant un accès direct au matériel via ses broches GPIO (General Purpose Input/Output).

Contrairement aux microcontrôleurs (Arduino, STM32) qui exécutent un seul programme directement sur le matériel, le Raspberry Pi dispose d'un **véritable système d'exploitation** avec multitâche, système de fichiers, réseau complet, et toute la puissance d'un Linux desktop.

### Qu'est-ce que le Raspberry Pi ?

Le Raspberry Pi est un **nano-ordinateur monocarté** développé par la Raspberry Pi Foundation, une fondation britannique à but non lucratif. Initialement conçu pour l'éducation, il est devenu une plateforme incontournable pour le prototypage, l'IoT, la domotique, et même l'industrie.

**Caractéristiques générales :**
- Processeur ARM (1-4 cœurs selon modèle)
- RAM de 512 Mo à 8 Go
- Stockage sur carte microSD
- Ports USB, Ethernet, WiFi/Bluetooth (modèles récents)
- Sortie vidéo HDMI
- Broches GPIO pour interfaçage matériel
- Consommation 5V via USB-C ou micro-USB
- Prix : 5€ à 75€ selon modèle

**Utilisation typique :**
```
┌─────────────────────────────────┐
│     Raspberry Pi                │
│  ┌───────────────────────────┐  │
│  │    Raspbian/Ubuntu OS     │  │
│  │   (Linux ARM complet)     │  │
│  └───────────────────────────┘  │
│  ┌─────────┐  ┌──────────────┐  │
│  │ Python  │  │  FreePascal  │  │
│  │ Node.js │  │     C/C++    │  │
│  └─────────┘  └──────────────┘  │
│  ┌───────────────────────────┐  │
│  │    GPIO / I2C / SPI       │  │
│  └───────────────────────────┘  │
└─────────────────────────────────┘
         ↓
    Capteurs, LEDs, moteurs...
```

### Différences avec les microcontrôleurs

| Aspect | Microcontrôleur (Arduino/STM32) | Raspberry Pi |
|--------|--------------------------------|--------------|
| **Type** | Microcontrôleur nu | Ordinateur complet |
| **OS** | Aucun (bare metal) | Linux complet |
| **Boot** | Instantané (< 1s) | Plusieurs secondes |
| **Puissance** | 16-480 MHz | 1-1.8 GHz (multi-cœurs) |
| **RAM** | 2-512 Ko | 512 Mo - 8 Go |
| **Stockage** | Flash embarquée | Carte SD (Go/To) |
| **Multitâche** | Non (ou RTOS) | Oui (Linux complet) |
| **Prix** | 2-10€ | 5-75€ |
| **Consommation** | mW | Watts |
| **Temps réel** | Excellent | Moyen (soft real-time) |
| **GPIO** | Direct, déterministe | Via kernel, non déterministe |
| **Réseau** | Nécessite module | Ethernet/WiFi intégré |
| **Interface** | Console série | HDMI, SSH, VNC |
| **Stockage données** | EEPROM limitée | Fichiers illimités |

**Quand utiliser Raspberry Pi plutôt qu'Arduino :**
- ✅ Besoin d'un OS complet (fichiers, réseau, multitâche)
- ✅ Interface graphique nécessaire (écran, clavier)
- ✅ Traitement de données complexe
- ✅ Serveur web ou applications réseau
- ✅ Caméra ou traitement vidéo
- ✅ Intelligence artificielle / Machine Learning
- ✅ Stockage de grandes quantités de données

**Quand utiliser Arduino/STM32 plutôt que Raspberry Pi :**
- ✅ Temps réel strict requis
- ✅ Très faible consommation nécessaire
- ✅ Environnement hostile (températures extrêmes)
- ✅ Coût minimal
- ✅ Application simple (clignoter LED, lire capteur)

## Modèles de Raspberry Pi

### Gamme complète

**Raspberry Pi 5 (2023) - Le plus puissant**
- CPU : Quad-core ARM Cortex-A76 @ 2.4 GHz
- RAM : 4 Go ou 8 Go LPDDR4X
- GPU : VideoCore VII
- Réseau : Gigabit Ethernet, WiFi 6, Bluetooth 5.0
- USB : 2× USB 3.0, 2× USB 2.0
- Vidéo : 2× micro-HDMI 4K@60fps
- GPIO : 40 broches
- Prix : ~60-80€
- **Usage** : Desktop, serveur, projets exigeants

**Raspberry Pi 4 Model B (2019) - Polyvalent**
- CPU : Quad-core ARM Cortex-A72 @ 1.5 GHz
- RAM : 2 Go, 4 Go ou 8 Go LPDDR4
- Réseau : Gigabit Ethernet, WiFi 5, Bluetooth 5.0
- USB : 2× USB 3.0, 2× USB 2.0
- Vidéo : 2× micro-HDMI 4K@60fps
- Prix : ~45-75€
- **Usage** : Excellent rapport qualité/prix, très populaire

**Raspberry Pi 3 Model B+ (2018) - Encore valable**
- CPU : Quad-core ARM Cortex-A53 @ 1.4 GHz
- RAM : 1 Go LPDDR2
- Réseau : Gigabit Ethernet, WiFi 4, Bluetooth 4.2
- Prix : ~35€
- **Usage** : Projets légers, apprentissage

**Raspberry Pi Zero 2 W (2021) - Compact**
- CPU : Quad-core ARM Cortex-A53 @ 1 GHz
- RAM : 512 Mo LPDDR2
- Réseau : WiFi 4, Bluetooth 4.2
- Taille : 65 × 30 mm (très petit !)
- Prix : ~15€
- **Usage** : Projets embarqués compacts, wearables

**Raspberry Pi Zero W (2017) - Mini budget**
- CPU : Single-core ARM11 @ 1 GHz
- RAM : 512 Mo
- Réseau : WiFi 4, Bluetooth 4.0
- Prix : ~10€
- **Usage** : Projets simples, caméra cachée, domotique basique

**Raspberry Pi Pico (2021) - Microcontrôleur**
- **Attention** : Ce n'est PAS un Raspberry Pi classique !
- CPU : RP2040 dual-core ARM Cortex-M0+ @ 133 MHz
- RAM : 264 Ko SRAM
- Stockage : 2 Mo Flash
- **Pas d'OS Linux** : programmation comme Arduino
- Prix : ~5€
- **Usage** : Remplaçant Arduino, très bon rapport qualité/prix

### Quel modèle choisir ?

**Pour débuter avec FreePascal :**
- **Raspberry Pi 4 (4 Go)** : excellent compromis
- **Raspberry Pi 5 (4 Go)** : si budget permet
- **Raspberry Pi 3 B+** : si petit budget

**Pour projets légers :**
- **Raspberry Pi Zero 2 W** : compact et suffisant

**À éviter pour FreePascal :**
- **Raspberry Pi Pico** : pas de Linux, programmation différente

## Installation du système d'exploitation

### Systèmes d'exploitation disponibles

**Raspberry Pi OS (recommandé pour débuter)**
- Basé sur Debian Linux
- Optimisé pour Raspberry Pi
- Interface graphique Pixel
- Nombreux logiciels pré-installés
- **3 versions :**
  - **Full** : tous les logiciels (LibreOffice, etc.) - ~2.5 Go
  - **Standard** : interface graphique + essentiels - ~1 Go
  - **Lite** : ligne de commande uniquement - ~400 Mo

**Ubuntu Server / Desktop**
- Ubuntu 22.04 LTS pour Raspberry Pi
- Plus proche d'un Ubuntu desktop classique
- Bon choix si vous connaissez déjà Ubuntu

**Autres distributions :**
- **DietPi** : ultra-léger, pour serveurs
- **RetroPie** : émulation jeux rétro
- **LibreELEC** : media center
- **Home Assistant OS** : domotique

**Pour FreePascal, recommandé :**
- **Raspberry Pi OS Standard** (interface graphique pour Lazarus)
- **Ubuntu Desktop** (si familier avec Ubuntu)

### Installation avec Raspberry Pi Imager

**Méthode la plus simple (recommandée) :**

**1. Télécharger Raspberry Pi Imager**

**Sur Windows :**
```
https://www.raspberrypi.com/software/
→ Télécharger pour Windows
→ Installer imager_x.x.x.exe
```

**Sur Ubuntu :**
```bash
sudo apt update
sudo apt install rpi-imager
```

**2. Préparer la carte microSD**

- Carte microSD de minimum 8 Go (16-32 Go recommandé)
- Classe 10 ou U1 minimum
- Marques recommandées : SanDisk, Samsung, Kingston

**3. Flasher l'OS**

1. Insérer carte SD dans PC (adaptateur USB si nécessaire)
2. Lancer Raspberry Pi Imager
3. **Choisir l'OS** :
   - "Raspberry Pi OS (64-bit)" pour Pi 4/5
   - "Raspberry Pi OS (32-bit)" pour Pi 3 et antérieurs
4. **Choisir le stockage** : sélectionner votre carte SD
5. **⚙️ Paramètres avancés** (roue dentée) :
   - ✅ Activer SSH
   - ✅ Définir nom d'utilisateur et mot de passe
   - ✅ Configurer WiFi (SSID et mot de passe)
   - ✅ Définir locale (Europe/Paris, fr_FR.UTF-8)
6. **Écrire** : lancer le flashage (5-10 minutes)
7. **Éjecter** la carte proprement

**4. Premier démarrage**

1. Insérer carte SD dans Raspberry Pi
2. Connecter :
   - Câble HDMI vers écran
   - Clavier/souris USB
   - Câble Ethernet (ou WiFi déjà configuré)
   - Alimentation 5V (dernier branchement)
3. Attendre boot (~30 secondes première fois)
4. Se connecter avec nom d'utilisateur configuré

### Configuration initiale

**Via interface graphique :**

Menu → Préférences → Raspberry Pi Configuration

**Via ligne de commande :**
```bash
sudo raspi-config
```

**Paramètres importants :**

**1. Mise à jour du système**
```bash
sudo apt update
sudo apt upgrade -y
```

**2. Activer SSH (si pas fait)**
```bash
sudo raspi-config
→ Interface Options → SSH → Enable
```

**3. Configurer timezone**
```bash
sudo raspi-config
→ Localisation Options → Timezone → Europe → Paris
```

**4. Étendre le système de fichiers (si besoin)**
```bash
sudo raspi-config
→ Advanced Options → Expand Filesystem
sudo reboot
```

**5. Changer mot de passe (sécurité)**
```bash
passwd
```

## Installation de FreePascal sur Raspberry Pi

### Installation depuis les dépôts

**Méthode 1 : Via apt (la plus simple)**

```bash
# Mettre à jour les paquets
sudo apt update

# Installer FreePascal
sudo apt install fpc

# Installer sources (utile pour développement)
sudo apt install fpc-source

# Vérifier installation
fpc -version
```

**Sortie attendue :**
```
Free Pascal Compiler version 3.2.2
```

**Installer Lazarus (IDE graphique) :**

```bash
# Installer Lazarus complet
sudo apt install lazarus

# Ou seulement l'IDE de base
sudo apt install lazarus-ide

# Vérifier
lazarus-ide --version
```

**Note :** Les versions dans les dépôts peuvent être anciennes. Pour Raspberry Pi OS, c'est généralement FPC 3.2.2, ce qui est suffisant.

### Installation depuis les sources (version récente)

Si vous voulez la dernière version de FreePascal :

```bash
# Dépendances pour compiler
sudo apt install build-essential binutils gdb

# Télécharger FPC (exemple 3.2.2)
cd /tmp
wget https://sourceforge.net/projects/freepascal/files/Linux/3.2.2/fpc-3.2.2.arm-linux-eabihf.tar

# Extraire
tar xf fpc-3.2.2.arm-linux-eabihf.tar
cd fpc-3.2.2.arm-linux-eabihf

# Installer
sudo ./install.sh

# Suivre les instructions (accepter chemins par défaut)
```

### Premier programme sur Raspberry Pi

**hello_pi.pas :**

```pascal
program HelloPi;

{$mode objfpc}{$H+}

uses
  SysUtils;

begin
  WriteLn('Hello from FreePascal on Raspberry Pi !');
  WriteLn('OS: ', GetEnvironmentVariable('OSTYPE'));
  WriteLn('Architecture: ', {$I %FPCTARGETCPU%});
  WriteLn('Compiled with FPC ', {$I %FPCVERSION%});
end.
```

**Compiler et exécuter :**

```bash
# Compiler
fpc hello_pi.pas

# Exécuter
./hello_pi
```

**Sortie :**
```
Hello from FreePascal on Raspberry Pi !
OS: linux-gnueabihf
Architecture: arm
Compiled with FPC 3.2.2
```

## Accès aux GPIO (General Purpose Input/Output)

### Qu'est-ce que les GPIO ?

Les GPIO sont des **broches programmables** sur le Raspberry Pi permettant de :
- Lire des signaux digitaux (boutons, capteurs)
- Écrire des signaux digitaux (LEDs, relais)
- Communiquer via protocoles (I2C, SPI, UART)
- Générer PWM (contrôle moteurs, intensité LED)

**Raspberry Pi dispose de 40 broches GPIO** (26 pour les anciens modèles).

### Schéma des broches (Raspberry Pi 4/5)

```
       3.3V  (1) (2)  5V
      GPIO2  (3) (4)  5V
      GPIO3  (5) (6)  GND
      GPIO4  (7) (8)  GPIO14 (TXD)
        GND  (9) (10) GPIO15 (RXD)
     GPIO17 (11) (12) GPIO18 (PWM)
     GPIO27 (13) (14) GND
     GPIO22 (15) (16) GPIO23
       3.3V (17) (18) GPIO24
     GPIO10 (19) (20) GND
      GPIO9 (21) (22) GPIO25
     GPIO11 (23) (24) GPIO8
        GND (25) (26) GPIO7
      GPIO0 (27) (28) GPIO1
      GPIO5 (29) (30) GND
      GPIO6 (31) (32) GPIO12
     GPIO13 (33) (34) GND
     GPIO19 (35) (36) GPIO16
     GPIO26 (37) (38) GPIO20
        GND (39) (40) GPIO21
```

**Légende :**
- **3.3V** : Alimentation 3.3V (50 mA max par broche)
- **5V** : Alimentation 5V (directe)
- **GND** : Masse (0V)
- **GPIOx** : Broches programmables (3.3V logic)

**⚠️ ATTENTION CRITIQUE :**
- Les GPIO fonctionnent en **3.3V** (pas 5V !)
- Appliquer 5V sur GPIO = **destruction immédiate**
- Courant max par broche : 16 mA
- Courant total tous GPIO : 50 mA

### Méthodes d'accès aux GPIO

**Il existe plusieurs façons d'accéder aux GPIO depuis FreePascal :**

**1. Via /sys/class/gpio (simple, pas root)**
- Accès fichiers dans `/sys/class/gpio`
- Pas besoin de droits root (avec configuration)
- Lent mais suffisant pour applications simples

**2. Via /dev/mem (rapide, nécessite root)**
- Accès direct aux registres matériels
- Très rapide
- Nécessite `sudo` ou droits spéciaux

**3. Via /dev/gpiomem (rapide, pas root)**
- Accès mémoire GPIO sans root
- Rapide comme /dev/mem
- Recommandé pour production

**4. Via bibliothèques (wiringPi, pigpio)**
- Bibliothèques C avec bindings Pascal
- Fonctionnalités avancées (PWM, timing)
- wiringPi est obsolète, pigpio recommandé

**5. Via Python + IPC**
- Utiliser bibliothèque Python GPIO
- Communiquer via sockets/pipes
- Pratique si bibliothèque Pascal manquante

### Méthode 1 : Accès via /sys/class/gpio

**La plus simple pour débuter.**

**Principe :**
```bash
# Exporter GPIO (le rendre accessible)
echo 17 > /sys/class/gpio/export

# Configurer en sortie
echo out > /sys/class/gpio/gpio17/direction

# Écrire valeur
echo 1 > /sys/class/gpio/gpio17/value  # HIGH
echo 0 > /sys/class/gpio/gpio17/value  # LOW

# Libérer GPIO
echo 17 > /sys/class/gpio/unexport
```

**En FreePascal :**

```pascal
program GPIO_Blink_Sys;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  GPIO_PIN = 17;  // GPIO17 (broche physique 11)

procedure WriteToFile(const FileName, Value: string);
var
  F: TextFile;
begin
  AssignFile(F, FileName);
  try
    Rewrite(F);
    WriteLn(F, Value);
  finally
    CloseFile(F);
  end;
end;

function ReadFromFile(const FileName: string): string;
var
  F: TextFile;
begin
  Result := '';
  AssignFile(F, FileName);
  try
    Reset(F);
    ReadLn(F, Result);
  finally
    CloseFile(F);
  end;
end;

procedure ExportGPIO(Pin: Integer);
begin
  if not DirectoryExists('/sys/class/gpio/gpio' + IntToStr(Pin)) then
    WriteToFile('/sys/class/gpio/export', IntToStr(Pin));

  Sleep(100);  // Attendre que le système crée les fichiers
end;

procedure UnexportGPIO(Pin: Integer);
begin
  WriteToFile('/sys/class/gpio/unexport', IntToStr(Pin));
end;

procedure SetDirection(Pin: Integer; Direction: string);
begin
  WriteToFile('/sys/class/gpio/gpio' + IntToStr(Pin) + '/direction', Direction);
end;

procedure SetValue(Pin: Integer; Value: Integer);
begin
  WriteToFile('/sys/class/gpio/gpio' + IntToStr(Pin) + '/value', IntToStr(Value));
end;

function GetValue(Pin: Integer): Integer;
var
  S: string;
begin
  S := ReadFromFile('/sys/class/gpio/gpio' + IntToStr(Pin) + '/value');
  Result := StrToIntDef(Trim(S), 0);
end;

begin
  WriteLn('Clignotement LED sur GPIO', GPIO_PIN);
  WriteLn('Ctrl+C pour arrêter');
  WriteLn;

  try
    // Exporter et configurer GPIO
    ExportGPIO(GPIO_PIN);
    SetDirection(GPIO_PIN, 'out');

    // Boucle de clignotement
    while True do
    begin
      WriteLn('LED ON');
      SetValue(GPIO_PIN, 1);
      Sleep(1000);

      WriteLn('LED OFF');
      SetValue(GPIO_PIN, 0);
      Sleep(1000);
    end;

  finally
    // Nettoyer
    UnexportGPIO(GPIO_PIN);
  end;
end.
```

**Câblage :**
```
Raspberry Pi          LED
GPIO17 (pin 11) ─────[Résistance 220Ω]─────[LED]─────GND (pin 9)
```

**Compiler et exécuter :**
```bash
fpc gpio_blink_sys.pas
./gpio_blink_sys
```

**Permissions :**

Si erreur de permission :

```bash
# Ajouter utilisateur au groupe gpio
sudo usermod -a -G gpio $USER

# Se déconnecter/reconnecter

# Ou donner accès direct (temporaire)
sudo chmod 666 /sys/class/gpio/export
sudo chmod 666 /sys/class/gpio/unexport
```

### Méthode 2 : Lecture d'un bouton

**Bouton avec pull-up interne :**

```pascal
program GPIO_Button;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  BUTTON_PIN = 23;  // GPIO23 (broche physique 16)
  LED_PIN = 17;     // GPIO17 (broche physique 11)

// ... (fonctions WriteToFile, ExportGPIO, etc. identiques) ...

begin
  WriteLn('Lecture bouton sur GPIO', BUTTON_PIN);
  WriteLn('LED sur GPIO', LED_PIN);
  WriteLn('Ctrl+C pour arrêter');
  WriteLn;

  try
    // Configurer bouton en entrée
    ExportGPIO(BUTTON_PIN);
    SetDirection(BUTTON_PIN, 'in');

    // Configurer LED en sortie
    ExportGPIO(LED_PIN);
    SetDirection(LED_PIN, 'out');

    // Boucle de lecture
    while True do
    begin
      if GetValue(BUTTON_PIN) = 0 then  // Bouton pressé (pull-up)
      begin
        WriteLn('Bouton pressé - LED ON');
        SetValue(LED_PIN, 1);
      end
      else
      begin
        SetValue(LED_PIN, 0);
      end;

      Sleep(50);  // Anti-rebond
    end;

  finally
    UnexportGPIO(BUTTON_PIN);
    UnexportGPIO(LED_PIN);
  end;
end.
```

**Câblage bouton :**
```
GPIO23 (pin 16) ─────[Bouton]─────GND (pin 14)
                 (pull-up interne activé)
```

**Note :** Le pull-up interne se configure via :
```bash
# Pas possible via /sys/class/gpio malheureusement
# Il faut utiliser /dev/gpiomem ou bibliothèque
```

## Cross-compilation depuis PC

L'une des forces de FreePascal est la possibilité de **compiler sur votre PC** pour le Raspberry Pi, sans avoir besoin de compiler sur le Pi lui-même (plus lent).

### Cross-compilation depuis Ubuntu

**1. Installer cross-compiler**

```bash
# Installer FPC avec support ARM
sudo apt install fpc fpc-source

# Installer binutils ARM
sudo apt install binutils-arm-linux-gnueabihf
```

**2. Compiler pour Raspberry Pi**

```pascal
// hello_cross.pas
program HelloCross;
begin
  WriteLn('Compilé en cross depuis Ubuntu !');
end.
```

**Commande de compilation :**

```bash
# Pour Raspberry Pi (ARM 32 bits)
fpc -Parm -Tlinux hello_cross.pas

# Ou spécifier plus précisément
fpc -Parm -CpARMV7A -Tlinux hello_cross.pas
```

**3. Transférer sur Raspberry Pi**

```bash
# Via SCP
scp hello_cross pi@192.168.1.100:/home/pi/

# Via SSH direct
ssh pi@192.168.1.100
./hello_cross
```

### Cross-compilation depuis Windows

**1. Installer cross-compiler**

Télécharger FPC pour Windows avec support ARM depuis :
```
https://www.freepascal.org/download.html
→ Windows → ARM Linux
```

Ou utiliser **fpcupdeluxe** qui simplifie l'installation de cross-compilers.

**2. Compiler**

```batch
fpc -Parm -Tlinux -CpARMV7A hello_cross.pas
```

**3. Transférer**

Utiliser **WinSCP** ou **FileZilla** pour transférer via SFTP.

## Projet complet : Station météo

**Matériel nécessaire :**
- Raspberry Pi 4 (ou 3)
- Capteur DHT22 (température + humidité)
- Écran OLED I2C 128x64 (SSD1306)
- Breadboard et fils

**Architecture :**

```
[Raspberry Pi] ←I2C→ [Écran OLED SSD1306]
       ↓
    GPIO4 ← [DHT22]
```

**Câblage DHT22 :**
```
DHT22 (vu de face) :
   ┌─────┐
   │  1  │ VCC → 3.3V (pin 1 Raspberry Pi)
   │  2  │ DATA → GPIO4 (pin 7) + résistance pull-up 10kΩ vers 3.3V
   │  3  │ NC (non connecté)
   │  4  │ GND → GND (pin 9)
   └─────┘
```

**Câblage OLED I2C :**
```
OLED:
- VCC → 3.3V (pin 1)
- GND → GND (pin 6)
- SDA → GPIO2 (pin 3)
- SCL → GPIO3 (pin 5)
```

### Configuration I2C

**Activer I2C sur Raspberry Pi :**

```bash
# Méthode 1 : Via raspi-config
sudo raspi-config
# → Interface Options → I2C → Enable

# Méthode 2 : Éditer config.boot
sudo nano /boot/config.txt
# Ajouter : dtparam=i2c_arm=on
# Sauvegarder et redémarrer

sudo reboot
```

**Installer outils I2C :**

```bash
sudo apt install i2c-tools

# Détecter périphériques I2C
sudo i2cdetect -y 1

# Sortie exemple :
#      0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
# 00:          -- -- -- -- -- -- -- -- -- -- -- -- --
# 10: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# 20: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# 30: -- -- -- -- -- -- -- -- -- -- -- -- 3c -- -- --
# 40: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# 50: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# 60: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# 70: -- -- -- -- -- -- -- --

# 0x3C = Adresse OLED SSD1306
```

### Code complet de la station météo

**Note :** Pour DHT22, nous utiliserons une approche simplifiée. En production, il faut une bibliothèque comme `pigpio` ou `wiringPi` pour le timing précis.

```pascal
program StationMeteo;

{$mode objfpc}{$H+}

uses
  SysUtils, Unix, BaseUnix, DateUtils;

const
  I2C_DEVICE = '/dev/i2c-1';
  OLED_ADDR = $3C;
  DHT22_PIN = 4;

type
  TDonneesMeteo = record
    Temperature: Real;
    Humidite: Real;
    Timestamp: TDateTime;
    Valide: Boolean;
  end;

// === Accès I2C ===

var
  I2C_FD: Integer = -1;

function I2C_Open: Boolean;
begin
  I2C_FD := fpOpen(I2C_DEVICE, O_RDWR);
  Result := I2C_FD >= 0;

  if not Result then
    WriteLn('Erreur ouverture I2C: ', fpgeterrno);
end;

procedure I2C_Close;
begin
  if I2C_FD >= 0 then
    fpClose(I2C_FD);
end;

function I2C_SetAddress(Addr: Byte): Boolean;
const
  I2C_SLAVE = $0703;  // ioctl constant
begin
  Result := fpioctl(I2C_FD, I2C_SLAVE, Pointer(PtrUInt(Addr))) >= 0;
end;

function I2C_Write(const Data: array of Byte): Boolean;
begin
  Result := fpWrite(I2C_FD, Data[0], Length(Data)) = Length(Data);
end;

// === Contrôle OLED SSD1306 simplifié ===

procedure OLED_Command(Cmd: Byte);
var
  Data: array[0..1] of Byte;
begin
  Data[0] := $00;  // Control byte pour commande
  Data[1] := Cmd;
  I2C_Write(Data);
end;

procedure OLED_Init;
begin
  I2C_SetAddress(OLED_ADDR);

  // Séquence d'initialisation SSD1306
  OLED_Command($AE);  // Display OFF
  OLED_Command($D5);  // Set display clock
  OLED_Command($80);
  OLED_Command($A8);  // Set multiplex
  OLED_Command($3F);
  OLED_Command($D3);  // Set display offset
  OLED_Command($00);
  OLED_Command($40);  // Set start line
  OLED_Command($8D);  // Charge pump
  OLED_Command($14);
  OLED_Command($20);  // Memory mode
  OLED_Command($00);  // Horizontal
  OLED_Command($A1);  // Segment remap
  OLED_Command($C8);  // COM scan direction
  OLED_Command($DA);  // COM pins
  OLED_Command($12);
  OLED_Command($81);  // Contrast
  OLED_Command($CF);
  OLED_Command($D9);  // Pre-charge
  OLED_Command($F1);
  OLED_Command($DB);  // VCOM detect
  OLED_Command($40);
  OLED_Command($A4);  // Display all on resume
  OLED_Command($A6);  // Normal display (not inverted)
  OLED_Command($AF);  // Display ON

  Sleep(100);
end;

procedure OLED_Clear;
var
  i: Integer;
  Data: array[0..16] of Byte;
begin
  I2C_SetAddress(OLED_ADDR);

  // Positionner au début
  OLED_Command($21);  // Column address
  OLED_Command($00);  // Start
  OLED_Command($7F);  // End
  OLED_Command($22);  // Page address
  OLED_Command($00);  // Start
  OLED_Command($07);  // End

  // Effacer (envoyer des 0)
  Data[0] := $40;  // Control byte pour données
  for i := 1 to 16 do
    Data[i] := $00;

  for i := 0 to 127 do  // 128x64 / 8 pages = 1024 bytes
  begin
    I2C_Write(Data);
  end;
end;

procedure OLED_DrawText(X, Y: Byte; const Text: string);
begin
  // Implémentation simplifiée
  // En réalité, il faut une police bitmap et écrire pixel par pixel
  WriteLn('OLED [', X, ',', Y, ']: ', Text);
end;

// === Lecture DHT22 ===

// ATTENTION: Lecture DHT22 nécessite timing précis (microsecondes)
// Cette fonction est SIMPLIFIÉE et peut ne pas fonctionner
// Pour production, utiliser bibliothèque comme pigpio

function LireDHT22(Pin: Integer): TDonneesMeteo;
var
  Data: array[0..4] of Byte;
  i, bit_idx: Integer;
  CheckSum: Byte;
begin
  Result.Valide := False;

  // En réalité, protocole DHT22 :
  // 1. Envoyer signal start (18ms LOW)
  // 2. Attendre réponse (80µs LOW + 80µs HIGH)
  // 3. Lire 40 bits (5 octets)
  // 4. Vérifier checksum

  // Pour ce tutoriel : SIMULATION
  WriteLn('Lecture DHT22 sur GPIO', Pin, '...');

  // Données simulées
  Result.Temperature := 20.0 + Random * 10.0;
  Result.Humidite := 50.0 + Random * 30.0;
  Result.Timestamp := Now;
  Result.Valide := True;

  WriteLn('  Température: ', Result.Temperature:0:1, ' °C');
  WriteLn('  Humidité   : ', Result.Humidite:0:1, ' %');
end;

// Fonction réelle nécessiterait :
// - Contrôle GPIO très précis (timing µs)
// - Lecture des transitions HIGH/LOW
// - Décodage des bits
// → Utiliser pigpio ou wiringPi en production

// === Programme principal ===

var
  Meteo: TDonneesMeteo;
  compteur: Integer;

begin
  WriteLn('=== Station Météo Raspberry Pi ===');
  WriteLn;

  Randomize;

  // Initialiser I2C
  if not I2C_Open then
  begin
    WriteLn('ERREUR: Impossible d''ouvrir I2C');
    WriteLn('Vérifier: sudo raspi-config → Interface Options → I2C → Enable');
    Exit;
  end;

  WriteLn('I2C ouvert');

  try
    // Initialiser OLED
    WriteLn('Initialisation OLED...');
    OLED_Init;
    OLED_Clear;
    Sleep(500);

    WriteLn('Démarrage des mesures (Ctrl+C pour arrêter)');
    WriteLn;

    compteur := 0;

    // Boucle principale
    while True do
    begin
      Inc(compteur);
      WriteLn('--- Mesure #', compteur, ' ---');

      // Lire capteur
      Meteo := LireDHT22(DHT22_PIN);

      if Meteo.Valide then
      begin
        // Afficher sur console
        WriteLn('Température: ', Meteo.Temperature:0:1, ' °C');
        WriteLn('Humidité   : ', Meteo.Humidite:0:1, ' %');
        WriteLn('Heure      : ', FormatDateTime('hh:nn:ss', Meteo.Timestamp));

        // Afficher sur OLED
        OLED_Clear;
        OLED_DrawText(0, 0, 'Station Meteo');
        OLED_DrawText(0, 16, Format('Temp: %.1f C', [Meteo.Temperature]));
        OLED_DrawText(0, 32, Format('Hum : %.1f %%', [Meteo.Humidite]));
        OLED_DrawText(0, 48, FormatDateTime('hh:nn:ss', Meteo.Timestamp));

        // Logger dans fichier
        // AppendToFile('meteo.log', Format('%s;%.1f;%.1f', [
        //   FormatDateTime('yyyy-mm-dd hh:nn:ss', Meteo.Timestamp),
        //   Meteo.Temperature, Meteo.Humidite]));
      end
      else
        WriteLn('ERREUR: Lecture capteur échouée');

      WriteLn;

      // Attendre 5 secondes
      Sleep(5000);
    end;

  finally
    I2C_Close;
    WriteLn('I2C fermé');
  end;
end.
```

**Pour vraie lecture DHT22, utiliser pigpio :**

```bash
# Installer pigpio
sudo apt install pigpio python3-pigpio

# Démarrer démon pigpio
sudo pigpiod

# Créer binding Pascal ou utiliser Python en subprocess
```

**Exemple avec Python pour DHT22 :**

```pascal
// Appeler script Python pour DHT22
function LireDHT22_Python(Pin: Integer): TDonneesMeteo;
var
  Process: TProcess;
  Output: TStringList;
  Temp, Hum: string;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python3';
    Process.Parameters.Add('read_dht22.py');
    Process.Parameters.Add(IntToStr(Pin));
    Process.Options := [poUsePipes, poWaitOnExit];
    Process.Execute;

    Output.LoadFromStream(Process.Output);

    if Output.Count >= 2 then
    begin
      Temp := Output[0];
      Hum := Output[1];

      Result.Temperature := StrToFloatDef(Temp, 0.0);
      Result.Humidite := StrToFloatDef(Hum, 0.0);
      Result.Timestamp := Now;
      Result.Valide := True;
    end
    else
      Result.Valide := False;
  finally
    Output.Free;
    Process.Free;
  end;
end;
```

**Script Python read_dht22.py :**

```python
#!/usr/bin/env python3
import sys
import Adafruit_DHT

pin = int(sys.argv[1])
humidity, temperature = Adafruit_DHT.read_retry(Adafruit_DHT.DHT22, pin)

if humidity and temperature:
    print(temperature)
    print(humidity)
else:
    sys.exit(1)
```

## Communication I2C avancée

### Lecture/écriture I2C générique

```pascal
unit UnitI2C;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Unix, BaseUnix;

type
  TI2C = class
  private
    FFD: Integer;
    FDeviceName: string;
  public
    constructor Create(const DeviceName: string = '/dev/i2c-1');
    destructor Destroy; override;

    function Open: Boolean;
    procedure Close;
    function SetAddress(Addr: Byte): Boolean;
    function Write(const Data: array of Byte): Boolean;
    function Read(var Data: array of Byte; Count: Integer): Boolean;
    function WriteRead(const WriteData: array of Byte; var ReadData: array of Byte; ReadCount: Integer): Boolean;

    property Connected: Boolean read (FFD >= 0);
  end;

implementation

const
  I2C_SLAVE = $0703;

constructor TI2C.Create(const DeviceName: string);
begin
  inherited Create;
  FFD := -1;
  FDeviceName := DeviceName;
end;

destructor TI2C.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TI2C.Open: Boolean;
begin
  FFD := fpOpen(FDeviceName, O_RDWR);
  Result := FFD >= 0;
end;

procedure TI2C.Close;
begin
  if FFD >= 0 then
  begin
    fpClose(FFD);
    FFD := -1;
  end;
end;

function TI2C.SetAddress(Addr: Byte): Boolean;
begin
  Result := fpioctl(FFD, I2C_SLAVE, Pointer(PtrUInt(Addr))) >= 0;
end;

function TI2C.Write(const Data: array of Byte): Boolean;
begin
  Result := fpWrite(FFD, Data[0], Length(Data)) = Length(Data);
end;

function TI2C.Read(var Data: array of Byte; Count: Integer): Boolean;
begin
  Result := fpRead(FFD, Data[0], Count) = Count;
end;

function TI2C.WriteRead(const WriteData: array of Byte; var ReadData: array of Byte; ReadCount: Integer): Boolean;
begin
  Result := Write(WriteData) and Read(ReadData, ReadCount);
end;

end.
```

### Exemple : Capteur BMP280 (pression/température)

```pascal
program TestBMP280;

{$mode objfpc}{$H+}

uses
  SysUtils, UnitI2C;

const
  BMP280_ADDR = $76;  // ou $77 selon câblage
  BMP280_ID_REG = $D0;
  BMP280_TEMP_REG = $FA;

var
  I2C: TI2C;
  Data: array[0..2] of Byte;
  ChipID: Byte;
  RawTemp: LongInt;
  Temperature: Real;

begin
  I2C := TI2C.Create('/dev/i2c-1');

  try
    if not I2C.Open then
    begin
      WriteLn('Erreur ouverture I2C');
      Exit;
    end;

    WriteLn('I2C ouvert');

    // Lire Chip ID
    I2C.SetAddress(BMP280_ADDR);
    Data[0] := BMP280_ID_REG;

    if I2C.WriteRead(Data, Data, 1) then
    begin
      ChipID := Data[0];
      WriteLn('Chip ID: $', IntToHex(ChipID, 2));

      if ChipID = $58 then
        WriteLn('BMP280 détecté !')
      else
        WriteLn('Chip ID incorrect');
    end;

    // Lire température (simplifié, sans calibration)
    Data[0] := BMP280_TEMP_REG;
    if I2C.WriteRead(Data, Data, 3) then
    begin
      RawTemp := (Data[0] shl 12) or (Data[1] shl 4) or (Data[2] shr 4);

      // Conversion simplifiée (vraie formule nécessite calibration)
      Temperature := RawTemp / 1000.0;

      WriteLn('Température brute: ', RawTemp);
      WriteLn('Température: ', Temperature:0:2, ' °C (approximation)');
    end;

  finally
    I2C.Free;
  end;
end.
```

## Communication SPI

### Activation SPI

```bash
sudo raspi-config
# → Interface Options → SPI → Enable

# Vérifier
ls /dev/spi*
# /dev/spidev0.0  /dev/spidev0.1
```

### Accès SPI depuis FreePascal

```pascal
unit UnitSPI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Unix, BaseUnix;

type
  TSPI = class
  private
    FFD: Integer;
    FDeviceName: string;
    FSpeed: LongWord;
    FMode: Byte;
  public
    constructor Create(const DeviceName: string = '/dev/spidev0.0');
    destructor Destroy; override;

    function Open: Boolean;
    procedure Close;
    function SetSpeed(Speed: LongWord): Boolean;
    function SetMode(Mode: Byte): Boolean;
    function Transfer(const TxData: array of Byte; var RxData: array of Byte; Len: Integer): Boolean;

    property Connected: Boolean read (FFD >= 0);
  end;

implementation

const
  SPI_IOC_MESSAGE_1 = $40206B00;
  SPI_IOC_WR_MODE = $40016B01;
  SPI_IOC_WR_MAX_SPEED_HZ = $40046B04;

type
  spi_ioc_transfer = record
    tx_buf: QWord;
    rx_buf: QWord;
    len: LongWord;
    speed_hz: LongWord;
    delay_usecs: Word;
    bits_per_word: Byte;
    cs_change: Byte;
    pad: LongWord;
  end;

constructor TSPI.Create(const DeviceName: string);
begin
  inherited Create;
  FFD := -1;
  FDeviceName := DeviceName;
  FSpeed := 1000000;  // 1 MHz par défaut
  FMode := 0;
end;

destructor TSPI.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TSPI.Open: Boolean;
begin
  FFD := fpOpen(FDeviceName, O_RDWR);
  Result := FFD >= 0;

  if Result then
  begin
    SetMode(FMode);
    SetSpeed(FSpeed);
  end;
end;

procedure TSPI.Close;
begin
  if FFD >= 0 then
  begin
    fpClose(FFD);
    FFD := -1;
  end;
end;

function TSPI.SetSpeed(Speed: LongWord): Boolean;
begin
  FSpeed := Speed;
  Result := fpioctl(FFD, SPI_IOC_WR_MAX_SPEED_HZ, @FSpeed) >= 0;
end;

function TSPI.SetMode(Mode: Byte): Boolean;
begin
  FMode := Mode;
  Result := fpioctl(FFD, SPI_IOC_WR_MODE, @FMode) >= 0;
end;

function TSPI.Transfer(const TxData: array of Byte; var RxData: array of Byte; Len: Integer): Boolean;
var
  transfer: spi_ioc_transfer;
begin
  FillChar(transfer, SizeOf(transfer), 0);

  transfer.tx_buf := QWord(@TxData[0]);
  transfer.rx_buf := QWord(@RxData[0]);
  transfer.len := Len;
  transfer.speed_hz := FSpeed;
  transfer.bits_per_word := 8;

  Result := fpioctl(FFD, SPI_IOC_MESSAGE_1, @transfer) >= 0;
end;

end.
```

### Exemple : Lecture ADC MCP3008 via SPI

```pascal
program TestMCP3008;

{$mode objfpc}{$H+}

uses
  SysUtils, UnitSPI;

function LireADC_MCP3008(SPI: TSPI; Canal: Byte): Word;
var
  TxData, RxData: array[0..2] of Byte;
begin
  // MCP3008 protocole : 3 octets
  TxData[0] := $01;  // Start bit
  TxData[1] := (8 + Canal) shl 4;  // Single-ended mode + canal
  TxData[2] := $00;

  if SPI.Transfer(TxData, RxData, 3) then
  begin
    // Résultat sur 10 bits dans les 2 derniers octets
    Result := ((RxData[1] and $03) shl 8) or RxData[2];
  end
  else
    Result := 0;
end;

var
  SPI: TSPI;
  i, canal: Integer;
  valeur: Word;
  tension: Real;

begin
  SPI := TSPI.Create('/dev/spidev0.0');

  try
    if not SPI.Open then
    begin
      WriteLn('Erreur ouverture SPI');
      Exit;
    end;

    WriteLn('MCP3008 ADC via SPI');
    WriteLn('Lecture des 8 canaux...');
    WriteLn;

    for i := 1 to 5 do
    begin
      WriteLn('--- Mesure #', i, ' ---');

      for canal := 0 to 7 do
      begin
        valeur := LireADC_MCP3008(SPI, canal);
        tension := (valeur / 1023.0) * 3.3;  // Vref = 3.3V

        WriteLn('Canal ', canal, ': ', valeur:4, ' (', tension:0:3, ' V)');
      end;

      WriteLn;
      Sleep(1000);
    end;

  finally
    SPI.Free;
  end;
end.
```

## Caméra Raspberry Pi

### Installation

```bash
# Activer caméra
sudo raspi-config
# → Interface Options → Camera → Enable

# Installer outils
sudo apt install libraspberrypi-bin

# Tester caméra
raspistill -o test.jpg
raspivid -o test.h264 -t 10000
```

### Capture d'image depuis FreePascal

```pascal
program CapturePhoto;

{$mode objfpc}{$H+}

uses
  SysUtils, Process;

procedure CaptureImage(const Fichier: string; Largeur: Integer = 1920; Hauteur: Integer = 1080);
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'raspistill';
    Proc.Parameters.Add('-o');
    Proc.Parameters.Add(Fichier);
    Proc.Parameters.Add('-w');
    Proc.Parameters.Add(IntToStr(Largeur));
    Proc.Parameters.Add('-h');
    Proc.Parameters.Add(IntToStr(Hauteur));
    Proc.Parameters.Add('-t');
    Proc.Parameters.Add('1');  // Timeout 1ms (instantané)
    Proc.Options := [poWaitOnExit];

    Proc.Execute;

    WriteLn('Photo capturée: ', Fichier);
  finally
    Proc.Free;
  end;
end;

var
  i: Integer;
  fichier: string;

begin
  WriteLn('Capture série de photos');

  for i := 1 to 10 do
  begin
    fichier := Format('photo_%3.3d.jpg', [i]);
    WriteLn('Capture #', i, '...');
    CaptureImage(fichier);
    Sleep(2000);  // Pause 2 secondes
  end;

  WriteLn('Terminé !');
end.
```

### Time-lapse

```pascal
program TimeLapse;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, Process;

const
  INTERVAL_SEC = 60;  // Une photo par minute
  DUREE_HEURES = 24;  // Pendant 24 heures

var
  compteur: Integer;
  debut, maintenant: TDateTime;
  fichier: string;
  Proc: TProcess;

begin
  WriteLn('Time-lapse: 1 photo/', INTERVAL_SEC, 's pendant ', DUREE_HEURES, 'h');

  compteur := 0;
  debut := Now;

  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'raspistill';
    Proc.Options := [poWaitOnExit];

    repeat
      Inc(compteur);
      maintenant := Now;

      fichier := Format('timelapse_%6.6d.jpg', [compteur]);

      WriteLn(FormatDateTime('hh:nn:ss', maintenant), ' - Photo #', compteur);

      Proc.Parameters.Clear;
      Proc.Parameters.Add('-o');
      Proc.Parameters.Add(fichier);
      Proc.Parameters.Add('-w');
      Proc.Parameters.Add('1920');
      Proc.Parameters.Add('-h');
      Proc.Parameters.Add('1080');
      Proc.Parameters.Add('-t');
      Proc.Parameters.Add('1');
      Proc.Execute;

      Sleep(INTERVAL_SEC * 1000);

    until HoursBetween(Now, debut) >= DUREE_HEURES;

  finally
    Proc.Free;
  end;

  WriteLn('Time-lapse terminé: ', compteur, ' photos');
  WriteLn('Créer vidéo avec: ffmpeg -framerate 30 -pattern_type glob -i "timelapse_*.jpg" -c:v libx264 timelapse.mp4');
end.
```

## Serveur web embarqué

### Serveur HTTP simple

```pascal
program ServeurWeb;

{$mode objfpc}{$H+}

uses
  SysUtils, Sockets, Classes;

const
  PORT = 8080;

var
  ServerSocket, ClientSocket: TSocket;
  Address: TInetSockAddr;
  Request, Response: string;
  Buffer: array[0..1023] of Char;
  BytesRead: Integer;

begin
  WriteLn('Serveur Web sur port ', PORT);

  // Créer socket
  ServerSocket := fpSocket(AF_INET, SOCK_STREAM, 0);

  if ServerSocket < 0 then
  begin
    WriteLn('Erreur création socket');
    Exit;
  end;

  // Bind
  Address.sin_family := AF_INET;
  Address.sin_port := htons(PORT);
  Address.sin_addr.s_addr := INADDR_ANY;

  if fpBind(ServerSocket, @Address, SizeOf(Address)) < 0 then
  begin
    WriteLn('Erreur bind');
    Exit;
  end;

  // Listen
  if fpListen(ServerSocket, 5) < 0 then
  begin
    WriteLn('Erreur listen');
    Exit;
  end;

  WriteLn('Serveur démarré sur http://localhost:', PORT);
  WriteLn('Ctrl+C pour arrêter');
  WriteLn;

  // Boucle principale
  while True do
  begin
    // Accept
    ClientSocket := fpAccept(ServerSocket, nil, nil);

    if ClientSocket < 0 then
      Continue;

    // Lire requête
    BytesRead := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer), 0);

    if BytesRead > 0 then
    begin
      SetString(Request, Buffer, BytesRead);
      WriteLn('Requête: ', Copy(Request, 1, Pos(#13#10, Request) - 1));

      // Générer réponse HTML
      Response :=
        'HTTP/1.1 200 OK'#13#10 +
        'Content-Type: text/html; charset=utf-8'#13#10 +
        'Connection: close'#13#10 +
        #13#10 +
        '<!DOCTYPE html>'#10 +
        '<html><head>'#10 +
        '<title>Raspberry Pi - FreePascal</title>'#10 +
        '<meta charset="utf-8">'#10 +
        '<style>'#10 +
        '  body { font-family: Arial; margin: 40px; background: #f0f0f0; }'#10 +
        '  .container { background: white; padding: 20px; border-radius: 8px; }'#10 +
        '  h1 { color: #c7053d; }'#10 +
        '</style>'#10 +
        '</head><body>'#10 +
        '<div class="container">'#10 +
        '<h1>🍓 Raspberry Pi - FreePascal</h1>'#10 +
        '<p>Serveur web embarqué fonctionnant sur Raspberry Pi</p>'#10 +
        '<p>Compilé avec Free Pascal</p>'#10 +
        '<p>Heure serveur: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '</p>'#10 +
        '</div></body></html>';

      // Envoyer réponse
      fpSend(ClientSocket, @Response[1], Length(Response), 0);
    end;

    // Fermer connexion client
    CloseSocket(ClientSocket);
  end;

  // Fermer socket serveur (jamais atteint avec while True)
  CloseSocket(ServerSocket);
end.
```

**Tester :**
```bash
# Compiler et lancer
fpc serveur_web.pas
./serveur_web

# Dans navigateur ou avec curl
curl http://localhost:8080
```

### Serveur API REST pour données GPIO

```pascal
program API_GPIO;

{$mode objfpc}{$H+}

uses
  SysUtils, Sockets, fpJSON, JSONParser;

const
  PORT = 8080;

// Fonctions GPIO simplifiées
procedure SetGPIO(Pin: Integer; Value: Boolean);
begin
  WriteLn('GPIO', Pin, ' := ', Value);
  // Implémentation réelle via /sys/class/gpio
end;

function GetGPIO(Pin: Integer): Boolean;
begin
  Result := Random(2) = 1;  // Simulation
end;

function HandleRequest(const Request: string): string;
var
  Method, Path, Body: string;
  JSON, ResponseJSON: TJSONObject;
  Pin: Integer;
  Value: Boolean;
  p: Integer;
begin
  // Parser requête HTTP basique
  p := Pos(' ', Request);
  Method := Copy(Request, 1, p - 1);
  Delete(Request, 1, p);
  p := Pos(' ', Request);
  Path := Copy(Request, 1, p - 1);

  WriteLn(Method, ' ', Path);

  ResponseJSON := TJSONObject.Create;

  try
    // Routes API
    if (Method = 'GET') and (Path = '/api/status') then
    begin
      // Status général
      ResponseJSON.Add('status', 'online');
      ResponseJSON.Add('time', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
      ResponseJSON.Add('uptime', GetTickCount64 div 1000);
    end
    else if (Method = 'GET') and (Pos('/api/gpio/', Path) = 1) then
    begin
      // Lire GPIO
      Pin := StrToIntDef(Copy(Path, 11, 10), -1);
      if Pin >= 0 then
      begin
        ResponseJSON.Add('pin', Pin);
        ResponseJSON.Add('value', GetGPIO(Pin));
      end
      else
        ResponseJSON.Add('error', 'Invalid pin');
    end
    else if (Method = 'POST') and (Pos('/api/gpio/', Path) = 1) then
    begin
      // Écrire GPIO
      Pin := StrToIntDef(Copy(Path, 11, 10), -1);

      // Parser body JSON (simplifié)
      // En réalité, extraire body de la requête HTTP
      JSON := GetJSON('{"value": true}') as TJSONObject;
      try
        Value := JSON.Get('value', False);
        SetGPIO(Pin, Value);

        ResponseJSON.Add('pin', Pin);
        ResponseJSON.Add('value', Value);
        ResponseJSON.Add('success', True);
      finally
        JSON.Free;
      end;
    end
    else
    begin
      ResponseJSON.Add('error', 'Not found');
    end;

    // Construire réponse HTTP
    Result :=
      'HTTP/1.1 200 OK'#13#10 +
      'Content-Type: application/json'#13#10 +
      'Access-Control-Allow-Origin: *'#13#10 +
      'Connection: close'#13#10 +
      #13#10 +
      ResponseJSON.AsJSON;

  finally
    ResponseJSON.Free;
  end;
end;

var
  ServerSocket, ClientSocket: TSocket;
  Address: TInetSockAddr;
  Request, Response: string;
  Buffer: array[0..4095] of Char;
  BytesRead: Integer;

begin
  WriteLn('API REST GPIO sur port ', PORT);

  ServerSocket := fpSocket(AF_INET, SOCK_STREAM, 0);

  Address.sin_family := AF_INET;
  Address.sin_port := htons(PORT);
  Address.sin_addr.s_addr := INADDR_ANY;

  fpBind(ServerSocket, @Address, SizeOf(Address));
  fpListen(ServerSocket, 5);

  WriteLn('API démarrée: http://localhost:', PORT, '/api/status');

  while True do
  begin
    ClientSocket := fpAccept(ServerSocket, nil, nil);

    if ClientSocket >= 0 then
    begin
      BytesRead := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer), 0);

      if BytesRead > 0 then
      begin
        SetString(Request, Buffer, BytesRead);
        Response := HandleRequest(Request);
        fpSend(ClientSocket, @Response[1], Length(Response), 0);
      end;

      CloseSocket(ClientSocket);
    end;
  end;

  CloseSocket(ServerSocket);
end.
```

**Tester l'API :**

```bash
# Status
curl http://localhost:8080/api/status

# Lire GPIO17
curl http://localhost:8080/api/gpio/17

# Écrire GPIO17
curl -X POST http://localhost:8080/api/gpio/17 -d '{"value": true}'
```

## Optimisation et bonnes pratiques

### Performance

**1. Compiler avec optimisations**

```bash
# Niveau 2 (recommandé)
fpc -O2 programme.pas

# Niveau 3 (maximum)
fpc -O3 programme.pas

# Optimisation taille
fpc -Os programme.pas
```

**2. Utiliser des types appropriés**

```pascal
// Éviter
var
  compteur: Int64;  // 8 octets, overkill pour simple compteur

// Préférer
var
  compteur: Integer;  // 4 octets suffisants
  petit: Byte;        // 1 octet si < 256
```

**3. Minimiser allocations dynamiques**

```pascal
// Moins efficace
var
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  // ...
  Liste.Free;
end;

// Plus efficace si taille connue
var
  Tableau: array[0..99] of string;
begin
  // Pas d'allocation dynamique
end;
```

### Gestion mémoire

**Raspberry Pi a de la RAM limitée (surtout Zero) :**

```bash
# Voir mémoire disponible
free -h

# Surveiller utilisation
htop
```

**Dans le code :**

```pascal
// Libérer rapidement les objets
var
  Obj: TObject;
begin
  Obj := TObject.Create;
  try
    // Utiliser
  finally
    Obj.Free;  // Toujours libérer
  end;
end;
```

### Démarrage automatique

**Créer service systemd :**

**/etc/systemd/system/mon-app.service :**

```ini
[Unit]
Description=Mon application FreePascal
After=network.target

[Service]
Type=simple
User=pi
WorkingDirectory=/home/pi/mon-app
ExecStart=/home/pi/mon-app/programme
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
```

**Activer :**

```bash
sudo systemctl daemon-reload
sudo systemctl enable mon-app.service
sudo systemctl start mon-app.service

# Vérifier status
sudo systemctl status mon-app.service

# Voir logs
sudo journalctl -u mon-app.service -f
```

### Logging

**Logger dans fichier et syslog :**

```pascal
program AvecLogging;

{$mode objfpc}{$H+}

uses
  SysUtils, Unix;

procedure LogMessage(const Msg: string);
var
  F: TextFile;
  LogFile: string;
begin
  LogFile := '/var/log/mon-app.log';

  try
    AssignFile(F, LogFile);
    if FileExists(LogFile) then
      Append(F)
    else
      Rewrite(F);

    WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' - ', Msg);
    CloseFile(F);
  except
    // Fallback sur stdout si erreur
    WriteLn('[LOG] ', Msg);
  end;

  // Aussi vers syslog
  FpSystem('logger -t mon-app "' + Msg + '"');
end;

begin
  LogMessage('Application démarrée');

  try
    // Code de l'application
    LogMessage('Traitement en cours...');

    // Simuler travail
    Sleep(5000);

    LogMessage('Traitement terminé');
  except
    on E: Exception do
      LogMessage('ERREUR: ' + E.Message);
  end;

  LogMessage('Application arrêtée');
end.
```

### Watchdog (surveillance)

**Redémarrer automatiquement si plantage :**

```pascal
program AvecWatchdog;

{$mode objfpc}{$H+}

uses
  SysUtils, BaseUnix, Unix;

var
  PidFile: string;

procedure CreerPidFile;
var
  F: TextFile;
begin
  PidFile := '/var/run/mon-app.pid';
  AssignFile(F, PidFile);
  Rewrite(F);
  WriteLn(F, FpGetPid);
  CloseFile(F);
end;

procedure SupprimerPidFile;
begin
  if FileExists(PidFile) then
    DeleteFile(PidFile);
end;

procedure SignalHandler(Signal: cint); cdecl;
begin
  WriteLn('Signal reçu: ', Signal);
  SupprimerPidFile;
  Halt(0);
end;

begin
  // Capturer signaux pour nettoyage propre
  FpSignal(SIGTERM, @SignalHandler);
  FpSignal(SIGINT, @SignalHandler);

  CreerPidFile;

  try
    WriteLn('Application démarrée (PID: ', FpGetPid, ')');

    // Boucle principale
    while True do
    begin
      // Travail...
      Sleep(1000);

      // Indiquer au watchdog que tout va bien
      // (écriture dans fichier ou socket)
    end;
  finally
    SupprimerPidFile;
  end;
end.
```

**Script watchdog externe (bash) :**

```bash
#!/bin/bash
# watchdog.sh

PIDFILE="/var/run/mon-app.pid"
PROGRAM="/home/pi/mon-app/programme"

while true; do
    if [ -f "$PIDFILE" ]; then
        PID=$(cat "$PIDFILE")

        if ! ps -p $PID > /dev/null; then
            echo "Process mort, redémarrage..."
            $PROGRAM &
        fi
    else
        echo "Démarrage initial..."
        $PROGRAM &
    fi

    sleep 10
done
```

## Sécurité

### Pare-feu (UFW)

```bash
# Installer UFW
sudo apt install ufw

# Autoriser SSH
sudo ufw allow 22/tcp

# Autoriser port web
sudo ufw allow 8080/tcp

# Activer
sudo ufw enable

# Vérifier
sudo ufw status
```

### Mise à jour automatique

```bash
# Installer unattended-upgrades
sudo apt install unattended-upgrades

# Configurer
sudo dpkg-reconfigure -plow unattended-upgrades
```

### Sécuriser SSH

```bash
# Éditer config SSH
sudo nano /etc/ssh/sshd_config

# Désactiver login root
PermitRootLogin no

# Utiliser clés SSH plutôt que mot de passe
PasswordAuthentication no

# Redémarrer SSH
sudo systemctl restart ssh
```

## Comparaison avec autres plateformes

### Raspberry Pi vs Arduino

| Aspect | Arduino (ATmega328P) | Raspberry Pi 4 |
|--------|---------------------|----------------|
| **CPU** | 16 MHz, 8 bits | 1.5 GHz quad-core, 64 bits |
| **RAM** | 2 Ko | 2-8 Go |
| **Stockage** | 32 Ko Flash | Carte SD (Go/To) |
| **OS** | Aucun | Linux complet |
| **Boot** | < 1s | 20-30s |
| **GPIO** | 20 broches | 40 broches |
| **Temps réel** | Excellent | Moyen |
| **Réseau** | Module externe | Gigabit Ethernet + WiFi |
| **Vidéo** | Non | HDMI 4K |
| **Prix** | ~25€ | ~45-75€ |
| **Consommation** | 0.2W | 3-7W |
| **Usage** | Contrôle simple | Traitement complexe |

**Choisir Arduino si :**
- Temps réel strict nécessaire
- Très faible consommation
- Application simple (capteur, LED)
- Coût minimal

**Choisir Raspberry Pi si :**
- OS complet nécessaire
- Réseau/Internet
- Traitement de données
- Interface graphique
- Caméra/vidéo

### Raspberry Pi vs PC industriel

| Aspect | Raspberry Pi | PC Industriel |
|--------|-------------|---------------|
| **Prix** | 45-75€ | 300-1000€+ |
| **Taille** | Carte de crédit | Grande |
| **Consommation** | 3-7W | 20-100W |
| **Fiabilité** | Moyenne | Élevée |
| **Température** | 0-50°C | -20 à +70°C |
| **Extensions** | GPIO, HAT | PCI, PCIe |
| **Durée vie** | 3-5 ans | 10+ ans |
| **Support** | Communautaire | Commercial |

## Ressources et documentation

### Documentation officielle

**Raspberry Pi :**
- Site officiel : https://www.raspberrypi.com
- Documentation : https://www.raspberrypi.com/documentation
- Forums : https://forums.raspberrypi.com

**FreePascal :**
- Wiki ARM : https://wiki.freepascal.org/ARM
- Forum : https://forum.lazarus.freepascal.org

### Bibliothèques recommandées

**wiringPi (obsolète mais exemples utiles) :**
- GPIO, I2C, SPI en C
- Bindings Pascal disponibles

**pigpio (recommandé) :**
- Bibliothèque moderne en C
- Timing précis, PWM matériel
- Daemon pour accès distant
- Créer bindings Pascal ou utiliser via subprocess

**Installation pigpio :**

```bash
sudo apt install pigpio python3-pigpio

# Démarrer daemon
sudo pigpiod

# Tester avec Python
python3 -c "import pigpio; pi=pigpio.pi(); pi.set_mode(17, pigpio.OUTPUT)"
```

### Projets communautaires

**GitHub :**
- Chercher : "freepascal raspberry pi"
- Exemples de projets GPIO, I2C, SPI

**Exemples intéressants :**
- Serveur domotique
- Station météo complète
- Caméra de surveillance
- Robot contrôlé via web
- Affichage LED matrix

## Projets avancés suggérés

### 1. Hub domotique complet

**Fonctionnalités :**
- Contrôle lumières (relais)
- Lecture capteurs (température, mouvement)
- Interface web responsive
- Application mobile (via API REST)
- Notifications push
- Programmation horaires
- Détection présence

**Technologies :**
- FreePascal backend
- SQLite pour données
- WebSocket pour temps réel
- API REST pour mobile

### 2. Caméra de surveillance intelligente

**Fonctionnalités :**
- Détection mouvement
- Enregistrement vidéo
- Reconnaissance faciale (OpenCV)
- Notifications
- Streaming web
- Stockage cloud

### 3. Robot autonome

**Fonctionnalités :**
- Moteurs et servos (PWM)
- Capteurs ultrason/infrarouge
- Caméra
- Contrôle web ou Bluetooth
- Navigation autonome
- Évitement obstacles

### 4. Station de monitoring environnemental

**Fonctionnalités :**
- Multi-capteurs (T°, humidité, CO2, particules)
- Graphiques temps réel
- Historique base de données
- Export données (CSV, JSON)
- Alertes configurables
- Dashboard web

### 5. Serveur media center

**Fonctionnalités :**
- Streaming audio/vidéo
- Bibliothèque multimédia
- Télécommande web
- Playlist automatiques
- Radio Internet
- Chromecast-like

## Conclusion

Le **Raspberry Pi avec FreePascal** offre une plateforme puissante pour créer des systèmes embarqués avec un véritable OS Linux, tout en conservant la simplicité et la clarté de Pascal.

**Points forts de cette combinaison :**

✅ **Puissance d'un PC** : multitâche, réseau, stockage illimité

✅ **Accès matériel** : GPIO, I2C, SPI, caméra

✅ **FreePascal natif** : performances excellentes sur ARM

✅ **Développement confortable** : IDE Lazarus, debugging

✅ **Écosystème Linux** : milliers de logiciels disponibles

✅ **Prix abordable** : 45-75€ pour Raspberry Pi 4

✅ **Communauté active** : documentation, exemples, support

**Cas d'usage idéaux :**
- 🏠 Domotique et IoT
- 📊 Acquisition et traitement de données
- 🌐 Serveurs embarqués (web, API)
- 📷 Vision par ordinateur
- 🤖 Robotique avancée
- 🎓 Apprentissage et prototypage

**Limites à connaître :**
- ⚠️ Pas de temps réel strict (soft real-time seulement)
- ⚠️ Consommation supérieure aux microcontrôleurs
- ⚠️ Boot plus lent (20-30 secondes)
- ⚠️ Carte SD sensible aux écritures intensives
- ⚠️ Peut nécessiter refroidissement sous charge

**Prochaines étapes suggérées :**
1. Maîtriser GPIO et protocoles (I2C, SPI)
2. Créer APIs REST pour contrôle distant
3. Intégrer bases de données (SQLite, PostgreSQL)
4. Explorer vision par ordinateur (OpenCV)
5. Développer applications IoT complètes

**Bon développement sur Raspberry Pi avec FreePascal ! 🍓🚀**

---

*Ce tutoriel fait partie du module 14 "Systèmes Embarqués et IoT" de la formation "FreePascal/Lazarus - Niveau Développeur Avancé - Edition Multi-plateforme Windows/Ubuntu"*

*Section précédente : 14.3 ARM Cortex-M et STM32*

*Prochaine section : 14.5 Protocoles IoT (MQTT, CoAP, LoRaWAN)*

⏭️ [Protocoles IoT (MQTT, CoAP, LoRaWAN)](/14-systemes-embarques-iot/05-protocoles-iot-mqtt-coap-lorawan.md)
