🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14. Systèmes Embarqués et IoT

## Introduction générale

Bienvenue dans le module consacré aux **systèmes embarqués et à l'Internet des Objets (IoT)** avec FreePascal et Lazarus. Ce module représente une extension fascinante de vos compétences en développement, vous permettant de programmer non seulement des ordinateurs classiques, mais aussi des **objets connectés** et des **systèmes autonomes**.

### Qu'est-ce qu'un système embarqué ?

Un système embarqué est un **système informatique spécialisé** intégré dans un appareil plus grand pour accomplir une tâche spécifique. Contrairement à un ordinateur généraliste, il est optimisé pour une fonction particulière.

**Exemples de systèmes embarqués dans votre quotidien :**

- **Electroménager** : machine à laver, four micro-ondes, réfrigérateur intelligent
- **Automobile** : ABS, système de navigation, contrôle moteur, aide au stationnement
- **Domotique** : thermostat intelligent, système d'alarme, éclairage connecté
- **Santé** : pacemaker, tensiomètre, glucomètre
- **Industriel** : automates programmables (PLC), robots, systèmes de surveillance
- **Loisirs** : console de jeu, drone, appareil photo numérique
- **Télécommunications** : routeur WiFi, téléphone portable, décodeur TV

**Caractéristiques communes :**
- Ressources limitées (mémoire, puissance de calcul)
- Fonctionnement autonome (pas d'écran/clavier dans certains cas)
- Contraintes temps réel (réponse dans un délai déterminé)
- Faible consommation électrique
- Fiabilité élevée (fonctionnement 24/7)
- Coût optimisé

### Qu'est-ce que l'IoT (Internet of Things) ?

L'**Internet des Objets** (ou **Internet of Things - IoT** en anglais) désigne l'interconnexion via Internet d'objets physiques équipés de capteurs, d'actionneurs et de capacités de communication.

**Définition simple :** Des objets du quotidien rendus "intelligents" par leur connexion à Internet et leur capacité à collecter, échanger et traiter des données.

**Exemples concrets d'IoT :**

1. **Maison connectée** :
   - Thermostat Nest qui apprend vos habitudes
   - Ampoules Philips Hue contrôlables par smartphone
   - Serrures connectées pour ouvrir à distance
   - Caméras de surveillance accessibles en ligne

2. **Ville intelligente (Smart City)** :
   - Capteurs de stationnement indiquant les places libres
   - Éclairage public qui s'adapte au trafic
   - Poubelles intelligentes signalant leur remplissage
   - Gestion optimisée du trafic routier

3. **Santé connectée** :
   - Montres intelligentes surveillant rythme cardiaque
   - Balance connectée transmettant données au médecin
   - Piluliers connectés rappelant la prise de médicaments

4. **Agriculture de précision** :
   - Capteurs d'humidité du sol
   - Stations météo locales
   - Drones surveillant les cultures
   - Systèmes d'irrigation automatisés

5. **Industrie 4.0** :
   - Maintenance prédictive des machines
   - Suivi en temps réel de la production
   - Optimisation logistique
   - Contrôle qualité automatisé

**Architecture typique IoT :**

```
[Capteurs] → [Microcontrôleur] → [Connectivité] → [Cloud] → [Application]
    ↓              ↓                    ↓             ↓            ↓
 Mesures      Traitement            WiFi/BLE/     Stockage    Visualisation
 physiques    local                 LoRa/4G       Analyse     Contrôle
```

### Pourquoi utiliser FreePascal pour l'embarqué et l'IoT ?

#### Avantages de FreePascal dans ce domaine

**1. Syntaxe claire et maintenable**

Comparaison avec le C (langage dominant dans l'embarqué) :

```c
// C traditionnel
volatile uint8_t *port = (uint8_t*)0x40011014;
*port |= (1 << 5);  // Syntaxe cryptique
```

```pascal
// FreePascal
GPIO_SetBits(GPIOC, GPIO_Pin_13);  // Intention claire
```

**2. Typage fort et sécurité**

Le compilateur FreePascal détecte de nombreuses erreurs à la compilation :

```pascal
var
  temperature: real;
  compteur: integer;

begin
  compteur := temperature;  // ERREUR détectée : types incompatibles
end;
```

En C, ce genre d'erreur peut passer inaperçu et causer des bugs difficiles à tracer.

**3. Portabilité du code**

Le même code FreePascal peut fonctionner sur :
- **Desktop** : Windows, Linux, macOS
- **Microcontrôleurs** : AVR (Arduino), ARM (STM32), MIPS
- **Systèmes embarqués** : Raspberry Pi, BeagleBone
- **Mobile** : Android (avec LAMW)

**Exemple de code multi-plateforme :**

```pascal
{$IFDEF MSWINDOWS}
  // Code spécifique Windows
{$ENDIF}
{$IFDEF UNIX}
  {$IFDEF CPUARM}
    // Code spécifique ARM (Raspberry Pi)
  {$ELSE}
    // Code Unix x86/x64
  {$ENDIF}
{$ENDIF}
```

**4. Orienté objet natif**

Organisation du code facilitée :

```pascal
type
  TCapteur = class
  private
    FValeur: real;
  public
    procedure Initialiser;
    function Lire: real;
    property Valeur: real read FValeur;
  end;

var
  CapteurTemperature: TCapteur;
  CapteurHumidite: TCapteur;
```

**5. Bibliothèques existantes**

Réutilisation de code :
- Algorithmes de traitement
- Structures de données
- Protocoles réseau
- Calculs scientifiques

**6. Même environnement de développement**

Développement sur PC avec Lazarus :
- Interface graphique pour tests et simulations
- Debugging confortable
- Cross-compilation vers la cible embarquée

**7. Performance**

FreePascal génère du code machine natif, aussi rapide que le C :
- Pas d'interpréteur (contrairement à Python)
- Optimisations du compilateur
- Accès direct au matériel

#### Comparaison avec d'autres langages

| Langage | Avantages | Inconvénients | Usage embarqué |
|---------|-----------|---------------|----------------|
| **C/C++** | Standard industrie, nombreuses libs | Syntaxe complexe, gestion mémoire manuelle | ⭐⭐⭐⭐⭐ Dominant |
| **FreePascal** | Syntaxe claire, typage fort, portable | Écosystème plus petit | ⭐⭐⭐⭐ Très bon |
| **Python** | Très facile, prototypage rapide | Lent, gourmand en mémoire | ⭐⭐⭐ Limité (MicroPython, RPi) |
| **Rust** | Sécurité mémoire, moderne | Courbe d'apprentissage élevée | ⭐⭐⭐ Émergent |
| **JavaScript** | Écosystème web, Node.js | Performance moyenne | ⭐⭐ Niche (IoT web) |
| **Ada** | Fiabilité critique, temps réel | Complexe, peu répandu | ⭐⭐ Aéronautique/militaire |

### Écosystème matériel couvert

Ce module vous apprendra à programmer différentes catégories de matériel :

#### 1. Microcontrôleurs (14.1-14.3)

**AVR (Arduino)**
- ATmega328P (Arduino Uno, Nano)
- ATmega2560 (Arduino Mega)
- ATtiny85 (DigiSpark)

**ARM Cortex-M (STM32)**
- STM32F103 (Blue Pill)
- STM32F401/F411 (Black Pill)
- STM32F4 Discovery
- STM32 Nucleo

**Autres**
- ESP8266/ESP32 (WiFi intégré)
- nRF52 (Bluetooth Low Energy)

#### 2. Cartes de développement (14.4)

**Raspberry Pi**
- Pi Zero/Zero W (petit format)
- Pi 3/4 (usage général)
- Pi Pico (microcontrôleur RP2040)

**Autres plateformes Linux**
- BeagleBone Black
- Orange Pi
- Jetson Nano (IA embarquée)

#### 3. Modules de communication (14.5, 14.10)

**Sans fil courte portée**
- Bluetooth / BLE
- WiFi (ESP32, modules WiFi)
- NFC / RFID
- Zigbee

**Sans fil longue portée**
- LoRa / LoRaWAN
- Sigfox
- NB-IoT / LTE-M
- 4G/5G

**Filaire**
- Ethernet
- RS485
- CAN bus

#### 4. Capteurs et actionneurs (14.7)

**Capteurs environnementaux**
- Température (DS18B20, DHT22, BME280)
- Humidité
- Pression atmosphérique
- Qualité de l'air (CO2, particules)

**Capteurs de mouvement**
- PIR (détection présence)
- Accéléromètre (MPU6050)
- Gyroscope
- Magnétomètre (boussole)

**Capteurs de distance**
- Ultrason (HC-SR04)
- Infrarouge
- Laser (LIDAR)

**Actionneurs**
- Moteurs DC, pas-à-pas, servo
- Relais
- LED, afficheurs
- Buzzer, haut-parleur

### Protocoles de communication embarqués

Vous apprendrez à utiliser les protocoles essentiels :

#### Protocoles de bus (14.6)

**UART / Serial**
- Communication série asynchrone
- Simple, 2 fils (TX/RX)
- Vitesse : 9600-115200 bauds
- Usage : console debug, GPS, modules

**I2C (Inter-Integrated Circuit)**
- Bus série synchrone
- Multi-maître, multi-esclave
- 2 fils (SDA, SCL)
- Vitesse : 100 kHz - 3.4 MHz
- Usage : capteurs, écrans OLED, RTC

**SPI (Serial Peripheral Interface)**
- Bus série synchrone
- Maître-esclaves
- 4 fils (MOSI, MISO, SCK, CS)
- Vitesse : jusqu'à plusieurs MHz
- Usage : carte SD, écrans TFT, modules RF

**OneWire**
- Un seul fil de données
- Multi-capteurs sur même bus
- Usage : DS18B20 (température)

**CAN Bus**
- Robuste, anti-interférences
- Multi-maître
- Usage : automobile, industriel

#### Protocoles réseau IoT (14.5)

**MQTT (Message Queue Telemetry Transport)**
- Protocole publish/subscribe
- Léger, faible bande passante
- Broker central
- Usage : IoT, télémétrie

**CoAP (Constrained Application Protocol)**
- Équivalent HTTP pour objets contraints
- UDP
- Usage : réseaux maillés

**HTTP/HTTPS**
- Protocole web classique
- REST API
- Usage : intégration web

**WebSocket**
- Communication bidirectionnelle
- Temps réel
- Usage : dashboard en direct

### Architecture logicielle embarquée

#### Sans système d'exploitation (Bare Metal)

Programme s'exécutant directement sur le matériel :

```pascal
program BareMetal;

procedure Setup;
begin
  // Configuration matérielle
end;

begin
  Setup;

  // Boucle infinie
  while true do
  begin
    // Traitement
  end;
end.
```

**Caractéristiques :**
- Contrôle total du matériel
- Latence minimale
- Pas de surcharge système
- Complexité accrue

#### Avec RTOS (Real-Time Operating System)

Système d'exploitation temps réel pour multitâche :

```pascal
// Exemple conceptuel avec FreeRTOS

procedure TacheAffichage;
begin
  while true do
  begin
    AfficherDonnees;
    vTaskDelay(1000);  // Dormir 1s
  end;
end;

procedure TacheAcquisition;
begin
  while true do
  begin
    LireCapteurs;
    vTaskDelay(100);  // Dormir 100ms
  end;
end;

begin
  xTaskCreate(@TacheAffichage, ...);
  xTaskCreate(@TacheAcquisition, ...);
  vTaskStartScheduler;  // Démarrer l'ordonnanceur
end.
```

**Avantages RTOS :**
- Multitâche préemptif
- Gestion des priorités
- Synchronisation (mutex, sémaphores)
- Gestion mémoire

**RTOS populaires :**
- FreeRTOS (gratuit, très répandu)
- ChibiOS/RT
- Zephyr
- RT-Thread

#### Avec Linux embarqué

Système Linux complet sur carte type Raspberry Pi :

```pascal
program LinuxEmbarque;

uses
  Unix, BaseUnix;

begin
  // Utilisation API Linux standard
  WriteLn('Hello depuis Linux embarqué');

  // Accès GPIO via /sys/class/gpio
  // Ou bibliothèques spécialisées
end.
```

**Avantages Linux embarqué :**
- Environnement familier
- Nombreux outils disponibles
- Réseau complet (TCP/IP, WiFi, etc.)
- Stockage (filesystem)

### Contraintes spécifiques aux systèmes embarqués

#### 1. Ressources limitées

**Mémoire Flash (programme)**
- AVR : 2-256 Ko
- ARM : 16 Ko - plusieurs Mo
- Nécessité : code compact, pas de gaspillage

**RAM (variables)**
- AVR : 0.5-16 Ko
- ARM : 4 Ko - plusieurs Mo
- Nécessité : allocation statique privilégiée

**Techniques d'optimisation :**
```pascal
// Gaspillage
var
  message: string = 'Erreur système';  // En RAM

// Optimisé
const
  MESSAGE = 'Erreur système';  // En Flash
```

#### 2. Consommation électrique

Crucial pour objets sur batterie :

**Modes économie d'énergie :**
- Sleep : arrêt CPU, périphériques actifs
- Deep sleep : arrêt horloge, réveil par interruption
- Standby : tout éteint sauf RTC

**Stratégies :**
- Désactiver périphériques inutilisés
- Réduire fréquence CPU
- Utiliser interruptions plutôt que polling
- Optimiser protocoles radio

#### 3. Temps réel

Réponse dans un délai déterminé et prévisible :

**Types de contraintes :**
- **Hard real-time** : délai critique (airbag, ABS) - échec = catastrophe
- **Soft real-time** : délai souhaitable (vidéo) - échec = dégradation

**Techniques :**
- Interruptions pour événements critiques
- Priorités sur les tâches
- RTOS pour ordonnancement
- Éviter allocations dynamiques imprévisibles

#### 4. Fiabilité

Fonctionnement sans intervention humaine :

**Mécanismes de sûreté :**
- Watchdog timer : redémarre si plantage
- Vérifications CRC des données
- Redondance des capteurs critiques
- Mode dégradé en cas de défaut

```pascal
procedure ConfigurerWatchdog;
begin
  // Si le programme ne "nourrit" pas le watchdog
  // toutes les 5 secondes, reset automatique
  EnableWatchdog(5000);
end;

procedure BouclePrincipale;
begin
  while true do
  begin
    TraiterDonnees;
    ResetWatchdog;  // "Nourrir" le watchdog
  end;
end;
```

### Méthodologie de développement embarqué

#### Cycle de développement typique

1. **Spécifications**
   - Définir les fonctionnalités
   - Contraintes (budget, consommation, taille)
   - Choix du matériel

2. **Prototypage**
   - Breadboard / carte développement
   - Tests fonctionnels
   - Validation concept

3. **Développement**
   - Écriture code sur PC
   - Compilation croisée
   - Tests unitaires

4. **Test et debug**
   - Flashage microcontrôleur
   - Debug via UART ou JTAG
   - Mesures (oscilloscope, multimètre)

5. **Optimisation**
   - Performance
   - Consommation
   - Taille code

6. **Production**
   - PCB personnalisé
   - Boîtier
   - Certification (CE, FCC)

#### Outils nécessaires

**Logiciels**
- Lazarus / FreePascal
- Éditeur de texte
- Outils de compilation croisée
- Logiciel de flashage (avrdude, st-flash, OpenOCD)
- Terminal série (PuTTY, minicom, screen)
- Debugger (GDB, OpenOCD)

**Matériel**
- Carte de développement
- Programmeur (ST-Link, USBasp, etc.)
- Câbles USB
- Breadboard et fils
- Multimètre
- Oscilloscope (pour signaux rapides)
- Alimentation stabilisée

**Optionnel**
- Analyseur logique
- Sonde logique
- Fer à souder
- Composants électroniques de base

### Structure de ce module

Ce module 14 est organisé en 11 sections progressives :

**Fondations (14.1-14.3)**
- Introduction au développement embarqué avec FreePascal
- Programmation microcontrôleurs AVR (Arduino)
- Programmation ARM Cortex-M (STM32)

**Plateformes avancées (14.4)**
- Raspberry Pi et Linux embarqué
- Cross-compilation depuis Windows/Ubuntu

**Protocoles IoT (14.5)**
- MQTT, CoAP, LoRaWAN
- Architectures cloud et edge

**Communication (14.6)**
- Série, I2C, SPI, OneWire
- CAN bus industriel

**Interfaces matérielles (14.7)**
- GPIO avancé
- Capteurs et actionneurs
- Protocoles spécialisés

**Temps réel (14.8)**
- Concepts RTOS
- Timing critique
- Priorités et ordonnancement

**Drivers (14.9)**
- Développement pilotes
- Accès matériel bas niveau
- DMA et interruptions

**Optimisation (14.10)**
- Mémoire et performance
- Consommation électrique
- Code compact

**Edge computing (14.11)**
- Traitement local des données
- IA embarquée
- Architectures distribuées

### Prérequis

Pour aborder ce module efficacement, vous devriez maîtriser :

**Connaissances FreePascal/Lazarus :**
- ✅ Syntaxe de base (variables, boucles, conditions)
- ✅ Procédures et fonctions
- ✅ Types de données
- ✅ Gestion des fichiers
- ✅ Programmation orientée objet (souhaitable)

**Connaissances système :**
- ✅ Ligne de commande (Windows/Ubuntu)
- ✅ Compilation et édition de liens
- ✅ Concepts de base réseaux (IP, TCP/UDP)

**Électronique de base (utile mais pas obligatoire) :**
- Comprendre tension, courant, résistance (loi d'Ohm)
- Savoir lire un schéma électrique simple
- Utiliser un multimètre

**Ne vous inquiétez pas si vous n'êtes pas expert en électronique !** Ce module se concentre sur la programmation. Les notions électroniques nécessaires seront expliquées au fur et à mesure.

### Conseils pour réussir

1. **Commencer simple** : commencez par faire clignoter une LED avant d'attaquer des projets complexes

2. **Tester progressivement** : validez chaque fonction isolément avant d'intégrer

3. **Documentation** : consultez toujours les datasheets des composants

4. **Communauté** : n'hésitez pas à demander de l'aide sur les forums

5. **Sécurité** : respectez les tensions (3.3V vs 5V), risque de détruire le matériel

6. **Patience** : le debugging embarqué est plus difficile que sur PC (pas de debugger visuel)

7. **Prototypage** : utilisez un breadboard avant de souder

8. **Sauvegardez** : versionnez votre code (Git)

### Projets fil rouge

Tout au long de ce module, nous développerons plusieurs projets complets :

**Projet 1 : Station météo IoT**
- Capteurs température/humidité/pression
- Affichage LCD
- Envoi données MQTT vers cloud
- Dashboard web

**Projet 2 : Robot autonome**
- Moteurs et capteurs ultrason
- Navigation obstacle
- Contrôle à distance
- Caméra (Raspberry Pi)

**Projet 3 : Domotique intelligente**
- Contrôle éclairage
- Détection présence
- Programmation horaire
- Interface mobile

**Projet 4 : Datalogger industriel**
- Acquisition multi-capteurs
- Stockage carte SD
- Communication ModBus
- Interface SCADA

## Conclusion de l'introduction

Les **systèmes embarqués et l'IoT** représentent un domaine en pleine expansion, avec des milliards d'objets connectés déployés dans le monde. FreePascal vous offre un moyen **élégant et efficace** d'entrer dans cet univers, en combinant :

- 🎯 Syntaxe claire et maintenable
- ⚡ Performances natives
- 🔧 Portabilité multi-plateformes
- 🛡️ Sécurité du typage fort
- 📚 Réutilisation de compétences desktop

Que vous souhaitiez créer des objets connectés personnels, des systèmes industriels, ou simplement explorer ce domaine fascinant, ce module vous donnera les **bases solides** nécessaires.

**Prêt à programmer au-delà du PC ? C'est parti ! 🚀**

---

*Ce module fait partie de la formation "FreePascal/Lazarus - Niveau Développeur Avancé - Edition Multi-plateforme Windows/Ubuntu"*

*Prochaine section : 14.1 FreePascal pour microcontrôleurs*

⏭️ [FreePascal pour microcontrôleurs](/14-systemes-embarques-iot/01-freepascal-microcontroleurs.md)
