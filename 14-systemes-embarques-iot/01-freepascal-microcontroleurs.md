🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.1 FreePascal pour microcontrôleurs

## Introduction

FreePascal n'est pas seulement destiné au développement d'applications desktop ou web. Le compilateur supporte également la programmation de microcontrôleurs, ces petits composants électroniques qui contrôlent des objets du quotidien : machines à laver, thermostats, drones, robots, systèmes domotiques, etc.

### Qu'est-ce qu'un microcontrôleur ?

Un microcontrôleur est un ordinateur miniature intégré sur une seule puce électronique. Contrairement à un PC classique, il est conçu pour une tâche spécifique et possède généralement :

- **Un processeur (CPU)** : beaucoup plus simple qu'un processeur de PC
- **De la mémoire RAM** : très limitée (quelques Ko à quelques Mo)
- **De la mémoire Flash** : pour stocker le programme (quelques Ko à quelques centaines de Ko)
- **Des périphériques intégrés** : ports GPIO, convertisseurs analogique-numérique, timers, interfaces série, etc.

**Exemples de microcontrôleurs populaires :**
- Arduino (basé sur AVR ou ARM)
- STM32 (ARM Cortex-M)
- ESP8266/ESP32 (avec WiFi intégré)
- PIC (Microchip)
- Raspberry Pi Pico (RP2040)

## Pourquoi utiliser FreePascal pour les microcontrôleurs ?

### Avantages par rapport au C/C++

La plupart des développeurs de systèmes embarqués utilisent le langage C ou C++. FreePascal offre plusieurs avantages :

1. **Syntaxe plus claire et lisible** : moins d'erreurs de syntaxe, code plus maintenable
2. **Typage fort** : le compilateur détecte plus d'erreurs à la compilation
3. **Gestion de chaînes facilitée** : pas de manipulation de pointeurs complexes
4. **Orienté objet natif** : organisation du code plus structurée
5. **Même langage du desktop à l'embarqué** : réutilisation des compétences

### Limites à connaître

Il est important de comprendre les contraintes :

- **Ressources très limitées** : quelques Ko de RAM seulement
- **Pas de système d'exploitation** : le programme s'exécute directement sur le matériel
- **Pas d'allocation dynamique complexe** : éviter les `New()` répétés
- **Bibliothèques limitées** : pas de RTL complète comme sur PC
- **Débogage plus difficile** : pas de debugger visuel comme sur desktop

## Architectures supportées par FreePascal

FreePascal supporte plusieurs familles de microcontrôleurs via différentes cibles de compilation :

### 1. AVR (Arduino classique)

**Cible FPC** : `avr`

Les microcontrôleurs AVR équipent les Arduino Uno, Nano, Mega, etc.

**Caractéristiques** :
- Architecture 8 bits
- 2 à 256 Ko de Flash
- 0,5 à 8 Ko de RAM
- Faible consommation
- Idéal pour débuter

**Exemples de modèles** :
- ATmega328P (Arduino Uno)
- ATmega2560 (Arduino Mega)
- ATtiny85 (très compact)

### 2. ARM Cortex-M (STM32, etc.)

**Cible FPC** : `arm-embedded`

Les processeurs ARM Cortex-M sont très répandus dans l'industrie.

**Caractéristiques** :
- Architecture 32 bits
- 16 Ko à plusieurs Mo de Flash
- 4 Ko à plusieurs centaines de Ko de RAM
- Performances élevées
- Nombreux périphériques intégrés

**Exemples de familles** :
- STM32 (ST Microelectronics) : très populaire
- NXP LPC : utilisé en automobile
- Nordic nRF52 : Bluetooth Low Energy
- Raspberry Pi Pico (RP2040)

### 3. MIPS

**Cible FPC** : `mipsel-embedded`

Moins courant mais supporté, notamment pour certains microcontrôleurs PIC32.

### 4. Xtensa (ESP8266/ESP32)

**Support** : en développement

Les ESP32 avec WiFi/Bluetooth intégré sont très populaires pour l'IoT, mais le support FreePascal est encore expérimental.

## Architecture d'un programme embarqué

### Structure minimale

Contrairement à un programme desktop, un programme pour microcontrôleur n'a pas de fonction `main()` classique ni de système d'exploitation. La structure typique est :

```pascal
program MonProgramme;

{$mode objfpc}
{$H+}

uses
  // Unités spécifiques au microcontrôleur
  ;

// Variables globales
var
  compteur: byte;

// Procédures et fonctions
procedure Initialiser;
begin
  // Configuration des périphériques
end;

procedure BouclePrincipale;
begin
  // Code exécuté en boucle infinie
  while true do
  begin
    // Votre logique ici
  end;
end;

// Point d'entrée
begin
  Initialiser;
  BouclePrincipale;
end.
```

### Sections critiques d'un programme embarqué

#### 1. Vecteurs d'interruption

Les interruptions sont des événements matériels (timer qui expire, donnée reçue, etc.) qui suspendent l'exécution normale pour traiter l'événement.

```pascal
procedure GestionnaireTimer; interrupt; public name 'TIMER1_COMPA_vect';
begin
  // Code exécuté à chaque interruption du timer
  Inc(compteur);
end;
```

#### 2. Configuration des registres

Les microcontrôleurs sont contrôlés par des registres matériels (petites zones mémoire spéciales).

```pascal
// Exemple : configurer une broche en sortie (AVR)
DDRB := DDRB or (1 shl PB5);  // Pin 13 Arduino = PB5
```

#### 3. Boucle infinie obligatoire

Un microcontrôleur ne doit jamais "se terminer". Le programme principal contient toujours une boucle infinie :

```pascal
while true do
begin
  // Traitement
  Delay(1000);
end;
```

## Gestion de la mémoire

### Contraintes mémoire

Sur un ATmega328P (Arduino Uno), vous disposez de :
- **32 Ko de Flash** : pour le code programme
- **2 Ko de RAM** : pour les variables
- **1 Ko d'EEPROM** : pour les données persistantes

### Bonnes pratiques

#### Éviter l'allocation dynamique

```pascal
// À ÉVITER sur microcontrôleur
var
  liste: ^TArray;
begin
  New(liste);  // Allocation dynamique = risque de fragmentation
  // ...
  Dispose(liste);
end;

// PRÉFÉRER : allocation statique
var
  tableau: array[0..9] of integer;
begin
  // Pas d'allocation, mémoire réservée à la compilation
end;
```

#### Utiliser des types adaptés

```pascal
// Gaspillage mémoire
var
  compteur: integer;  // 32 bits sur ARM = 4 octets

// Optimisé
var
  compteur: byte;     // 8 bits = 1 octet (suffisant si < 256)
```

#### Constantes en Flash

Les chaînes de caractères et tableaux constants peuvent être placés en mémoire Flash plutôt qu'en RAM :

```pascal
const
  Message: string = 'Bonjour';  // Stocké en Flash

// Comparé à :
var
  Message: string = 'Bonjour';  // Stocké en RAM (gaspillage)
```

### Surveillance de l'utilisation mémoire

Le compilateur FreePascal affiche la taille du programme compilé :

```
Code size: 1456 bytes
Data size: 234 bytes
```

Surveillez que vous ne dépassez pas les capacités de votre microcontrôleur.

## Différences avec la programmation desktop

### Pas de système d'exploitation

- **Pas de multitâche préemptif** : votre programme a le contrôle total
- **Pas de gestion automatique des ressources** : vous gérez tout manuellement
- **Accès direct au matériel** : pas de couche d'abstraction

### Pas de bibliothèque standard complète

De nombreuses unités de la RTL FreePascal ne sont pas disponibles :

**Non disponibles** :
- `SysUtils` (partiellement)
- `Classes` (pas de TStringList, etc.)
- Gestion de fichiers
- Fonctions d'allocation dynamique complexes

**Disponibles** :
- Opérations mathématiques de base
- Manipulation de bits
- Types de base (integer, byte, boolean, etc.)

### Timing critique

Sur desktop, un délai de quelques millisecondes n'a pas d'importance. Sur microcontrôleur :

- **Les timings sont précis** : une attente de 1 µs sera vraiment de 1 µs
- **Les interruptions doivent être rapides** : quelques instructions seulement
- **Le polling actif consomme de l'énergie** : préférer les interruptions

## Compilation pour microcontrôleurs

### Installation des outils

#### Sur Windows

1. **Installer FreePascal** avec support embarqué
2. **Installer les binutils** pour l'architecture cible (avr-binutils, arm-none-eabi-gcc, etc.)
3. **Configurer les chemins** dans fpc.cfg

#### Sur Ubuntu

```bash
# Pour AVR
sudo apt install fpc binutils-avr gcc-avr avr-libc avrdude

# Pour ARM
sudo apt install fpc binutils-arm-none-eabi gcc-arm-none-eabi
```

### Commande de compilation

#### Pour AVR (Arduino)

```bash
fpc -Pavr -Tembedded -Wpatmega328p -O2 monprogramme.pas
```

**Explications** :
- `-Pavr` : processeur AVR
- `-Tembedded` : système cible = embarqué
- `-Wpatmega328p` : modèle spécifique de microcontrôleur
- `-O2` : niveau d'optimisation

#### Pour ARM (STM32)

```bash
fpc -Parm -Tembedded -Wpstm32f103c8 -O2 monprogramme.pas
```

### Flashage du microcontrôleur

Une fois compilé, le fichier `.hex` ou `.bin` doit être transféré sur le microcontrôleur :

#### AVR (Arduino)

```bash
avrdude -p atmega328p -c arduino -P /dev/ttyUSB0 -U flash:w:monprogramme.hex
```

**Sur Windows** : remplacer `/dev/ttyUSB0` par `COM3` (ou le port approprié)

#### ARM (STM32)

Utiliser un outil comme :
- **STM32CubeProgrammer** (Windows/Linux)
- **OpenOCD** (ligne de commande)
- **st-flash** (Linux)

```bash
st-flash write monprogramme.bin 0x8000000
```

## Unités et bibliothèques pour l'embarqué

### Unités RTL embarquées

FreePascal fournit des unités spécialisées pour l'embarqué :

#### `embedded` (générique)

Fonctions de base communes à tous les microcontrôleurs :

```pascal
uses
  embedded;

begin
  // Attente active
  Delay(1000);  // 1000 ms
  DelayMicroseconds(500);  // 500 µs
end;
```

#### Unités spécifiques AVR

```pascal
uses
  avr, atmega328p;

begin
  // Accès aux registres spécifiques AVR
  PORTB := $FF;
end;
```

#### Unités spécifiques ARM

```pascal
uses
  cortexm3, stm32f103;

begin
  // Configuration GPIO, timers, etc.
end;
```

### Bibliothèques tierces

Plusieurs projets fournissent des bibliothèques pour faciliter le développement :

#### **FPCKit**

Collection d'unités pour microcontrôleurs, incluant :
- Gestion des GPIO
- Communication série (UART)
- Timers et PWM
- I2C et SPI

#### **Arduino-for-FreePascal**

Port des bibliothèques Arduino en Pascal, permettant d'utiliser :
- `digitalWrite()`, `digitalRead()`
- `analogRead()`, `analogWrite()`
- `Serial.println()`

## Outils de développement

### IDE adaptés

#### Lazarus

Lazarus peut être utilisé, mais sans le designer visuel. Il faut :
- Créer un projet console
- Configurer les options de compilation pour embedded
- Utiliser uniquement l'éditeur de code

#### Arduino IDE avec plugin

Un plugin expérimental permet d'utiliser l'Arduino IDE avec FreePascal au lieu de C++.

#### Éditeurs de texte

Pour plus de simplicité, beaucoup utilisent :
- **VS Code** avec extension Pascal
- **Geany**
- **Notepad++** (Windows)
- **Sublime Text**

Avec compilation en ligne de commande.

### Débogage

Le débogage sur microcontrôleur est plus complexe que sur PC :

#### Méthodes disponibles

1. **UART série** : envoyer des messages de debug via le port série

```pascal
WriteLn('Valeur: ', compteur);
```

2. **LED de debug** : faire clignoter une LED pour indiquer l'état

```pascal
if erreur then
  LED_Allumer
else
  LED_Eteindre;
```

3. **JTAG/SWD** : débogage matériel avec une sonde (ARM uniquement)
   - Nécessite un programmeur spécial (ST-Link, J-Link)
   - Permet de mettre des points d'arrêt
   - Inspection des variables en temps réel

4. **Simulateur** : tester le code sans matériel
   - **SimAVR** pour AVR
   - **QEMU** pour ARM

## Exemples d'utilisation

### Domaines d'application

FreePascal sur microcontrôleur peut être utilisé pour :

#### Domotique
- Contrôle d'éclairage intelligent
- Gestion de chauffage
- Alarmes et capteurs

#### Robotique
- Contrôle de moteurs
- Lecture de capteurs (ultrason, infrarouge)
- Navigation autonome

#### IoT (Internet des objets)
- Stations météo connectées
- Capteurs environnementaux
- Objets connectés personnalisés

#### Instrumentation
- Acquisition de données
- Contrôle de processus industriels
- Mesure et monitoring

#### Apprentissage
- Projets éducatifs
- Prototypage rapide
- Découverte de l'électronique

## Comparaison avec d'autres solutions

### FreePascal vs Arduino (C++)

| Critère | FreePascal | Arduino C++ |
|---------|-----------|-------------|
| Syntaxe | Plus lisible | Syntaxe C complexe |
| Courbe d'apprentissage | Moyenne | Moyenne |
| Bibliothèques | Limitées | Très nombreuses |
| Communauté | Petite | Très grande |
| Performance | Similaire | Similaire |
| Maturité | En développement | Mature |

### FreePascal vs MicroPython

| Critère | FreePascal | MicroPython |
|---------|-----------|-------------|
| Performance | Excellente | Moyenne |
| Mémoire requise | Faible | Élevée |
| Développement | Compilation | Interprété |
| Temps réel | Oui | Limité |
| Facilité | Moyenne | Très facile |

## Ressources et documentation

### Documentation officielle

- **FreePascal Wiki** : section "Embedded Systems"
- **Documentation des cibles embedded** : registres, interruptions
- **Forum FreePascal** : section "Embedded"

### Projets exemples

Recherchez sur GitHub :
- "freepascal avr"
- "freepascal stm32"
- "lazarus embedded"

### Communauté

- **Forum FreePascal** : https://forum.lazarus.freepascal.org
- **Discord/IRC** : canaux dédiés à l'embarqué
- **Listes de diffusion** : fpc-devel pour questions techniques

## Conclusion

FreePascal pour microcontrôleurs est une alternative intéressante au C/C++ traditionnel, offrant :

**Points forts** :
- Syntaxe claire et sécurisée
- Même langage du desktop à l'embarqué
- Bonnes performances
- Support de multiples architectures

**Points faibles** :
- Écosystème moins mature qu'Arduino
- Bibliothèques limitées
- Documentation parfois incomplète
- Communauté plus restreinte

**Pour qui ?** :
- Développeurs FreePascal souhaitant faire de l'embarqué
- Projets nécessitant du code partagé entre desktop et embarqué
- Apprentissage de l'électronique avec un langage familier
- Prototypage rapide avec code propre

Dans les prochaines sections, nous explorerons en détail la programmation AVR (Arduino), ARM Cortex-M (STM32), et les différents protocoles de communication pour microcontrôleurs.

⏭️ [AVR et Arduino avec FreePascal](/14-systemes-embarques-iot/02-avr-arduino-freepascal.md)
