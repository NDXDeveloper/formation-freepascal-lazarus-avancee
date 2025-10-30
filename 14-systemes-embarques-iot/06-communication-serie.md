🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.6 Communication série

## Introduction

La **communication série** est l'une des méthodes les plus anciennes et les plus universelles pour faire communiquer des appareils électroniques. Malgré l'émergence de technologies plus modernes (USB, Ethernet, WiFi), elle reste **omniprésente** dans le monde de l'embarqué et de l'IoT en raison de sa simplicité, sa fiabilité et sa compatibilité universelle.

### Qu'est-ce que la communication série ?

La communication série consiste à transmettre des données **bit par bit** sur un seul fil (ou paire de fils), contrairement à la communication parallèle qui transmet plusieurs bits simultanément sur plusieurs fils.

**Analogie simple :**
- **Communication parallèle** : comme une autoroute à 8 voies, où 8 voitures (bits) passent en même temps
- **Communication série** : comme une route à une voie, où les voitures passent l'une après l'autre

**Visualisation :**

```
Parallèle (8 bits simultanés) :
  Bit 0 ═══════════════════════════> [0]
  Bit 1 ═══════════════════════════> [1]
  Bit 2 ═══════════════════════════> [1]
  Bit 3 ═══════════════════════════> [0]
  Bit 4 ═══════════════════════════> [1]
  Bit 5 ═══════════════════════════> [0]
  Bit 6 ═══════════════════════════> [1]
  Bit 7 ═══════════════════════════> [1]

Série (1 bit à la fois) :
  Ligne unique ═> [0][1][1][0][1][0][1][1] ═>
```

### Pourquoi la communication série ?

**Avantages :**

1. **Simplicité** : seulement 2-3 fils nécessaires (TX, RX, GND)
2. **Longue distance** : peut fonctionner sur plusieurs dizaines/centaines de mètres (avec RS-485)
3. **Coût faible** : peu de fils, connecteurs simples, composants bon marché
4. **Universalité** : supportée par tous les microcontrôleurs
5. **Fiabilité** : protocole éprouvé depuis des décennies
6. **Debugging** : idéale pour afficher des messages de debug
7. **Interopérabilité** : standard bien établi

**Inconvénients :**

1. **Vitesse limitée** : plus lente que le parallèle (mais suffisante pour la plupart des usages)
2. **Synchronisation** : nécessite accord sur la vitesse (baud rate)
3. **Câblage** : attention à l'ordre des fils (TX/RX)

### Histoire rapide

**1960s** : RS-232 standardisé pour connecter terminaux aux ordinateurs mainframe

**1980s** : Port série omniprésent sur PC (souris, modem, imprimante)

**1990s-2000s** : Déclin au profit de l'USB sur PC

**2010s-aujourd'hui** : Renaissance dans l'embarqué, IoT et prototypage (Arduino, Raspberry Pi)

**Évolution des vitesses :**
- 1960 : 110 bauds (10 caractères/seconde)
- 1980 : 9600 bauds (960 caractères/seconde)
- 2000 : 115200 bauds (11520 caractères/seconde)
- Aujourd'hui : jusqu'à 12 Mbauds sur certains systèmes

## Types de communication série

### UART (Universal Asynchronous Receiver-Transmitter)

**Le standard de fait pour la communication série.**

**Caractéristiques :**
- **Asynchrone** : pas de signal d'horloge partagé
- **Point à point** : 1 émetteur ↔ 1 récepteur
- **Full-duplex** : peut envoyer et recevoir simultanément
- **2 fils** : TX (transmission) et RX (réception)

**Principe de fonctionnement :**

```
Appareil A                    Appareil B
┌──────────┐                 ┌──────────┐
│   UART   │                 │   UART   │
│    TX ───┼─────────────────┼─→ RX     │
│    RX ←──┼─────────────────┼─── TX    │
│   GND ───┼─────────────────┼─── GND   │
└──────────┘                 └──────────┘
```

**Note importante :** TX d'un appareil se connecte au RX de l'autre, et vice-versa.

**Format d'une trame UART :**

```
   ┌─────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬────────┬──────┐
   │Start│ Bit0 │ Bit1 │ Bit2 │ Bit3 │ Bit4 │ Bit5 │ Bit6 │ Bit7 │ Parity │ Stop │
   │ (0) │      │      │      │      │      │      │      │      │ (opt)  │ (1)  │
   └─────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴────────┴──────┘

   Ligne au repos : niveau HAUT (1)
   Start bit : descend à BAS (0) pour indiquer début
   Bits de données : LSB en premier (bit 0 = poids faible)
   Parity bit : optionnel, pour détection d'erreur
   Stop bit(s) : retour à HAUT (1), peut être 1, 1.5 ou 2 bits
```

**Paramètres à configurer :**

1. **Baud rate** (vitesse) : nombre de bits/seconde
   - Standards : 9600, 19200, 38400, 57600, 115200 bauds
   - Les deux appareils doivent utiliser la même vitesse

2. **Bits de données** : nombre de bits par caractère
   - Généralement 8 bits (1 octet)
   - Parfois 7 bits (ASCII) ou 9 bits

3. **Parité** : vérification d'erreur
   - None (pas de parité) - le plus courant
   - Even (paire)
   - Odd (impaire)
   - Mark (toujours 1)
   - Space (toujours 0)

4. **Bits de stop** : fin de trame
   - 1 bit (standard)
   - 1.5 bits
   - 2 bits (connexions lentes)

**Notation courante : 9600-8-N-1**
- 9600 bauds
- 8 bits de données
- No parity (pas de parité)
- 1 bit de stop

### RS-232

**Standard électrique pour UART, historiquement utilisé sur PC.**

**Caractéristiques électriques :**
- **Niveaux de tension** : ±3V à ±25V
  - Logique 1 (MARK) : -3V à -25V
  - Logique 0 (SPACE) : +3V à +25V
- **Distance** : jusqu'à 15 mètres
- **Connecteur** : DB9 ou DB25

**Problème :** Les microcontrôleurs fonctionnent en **logique TTL** (0V/3.3V ou 0V/5V), pas en RS-232 (±12V).

**Solution :** Convertisseur de niveau (MAX232, FT232, etc.)

```
Microcontrôleur (TTL 0V/5V) ←→ [MAX232] ←→ PC (RS-232 ±12V)
```

**RS-232 est en déclin sur PC moderne** (remplacé par USB), mais le concept UART reste très utilisé.

### TTL Serial (UART logique TTL)

**Version moderne de l'UART, niveaux compatibles microcontrôleurs.**

**Caractéristiques :**
- **Niveaux de tension** :
  - 0V = logique 0
  - 3.3V ou 5V = logique 1
- **Pas de convertisseur** nécessaire entre microcontrôleurs
- **Omniprésent** : Arduino, Raspberry Pi, STM32, ESP32

**Attention aux niveaux de tension !**
- **5V tolerant** : un appareil 3.3V peut parfois accepter 5V en entrée
- **Non-tolerant** : risque de destruction si on applique 5V sur entrée 3.3V
- **Solution** : diviseur de tension ou convertisseur de niveau

```
Arduino 5V ←→ STM32 3.3V : DANGER !

Arduino 5V ←→ [Convertisseur niveau] ←→ STM32 3.3V : OK
```

### RS-485 / RS-422

**Standards pour communication série longue distance et multi-appareils.**

**RS-485 (half-duplex) :**
- **Différentiel** : utilise paire de fils torsadés (A et B)
- **Multi-point** : jusqu'à 32 appareils sur même bus
- **Distance** : jusqu'à 1200 mètres
- **Vitesse** : jusqu'à 10 Mbits/s sur courte distance
- **Robustesse** : résistant aux interférences électromagnétiques
- **Usage** : systèmes industriels, building automation, ModBus

**RS-422 (full-duplex) :**
- **Similaire à RS-485** mais full-duplex (2 paires de fils)
- **Point à multipoint** : 1 émetteur, jusqu'à 10 récepteurs

**Topologie RS-485 :**

```
┌──────────┐     ┌──────────┐     ┌──────────┐     ┌──────────┐
│ Appareil │─────│ Appareil │─────│ Appareil │─────│ Appareil │
│    1     │     │    2     │     │    3     │     │    4     │
└──────────┘     └──────────┘     └──────────┘     └──────────┘
      │                │                │                │
    ──┴────────────────┴────────────────┴────────────────┴──  Bus A
    ──┬────────────────┬────────────────┬────────────────┬──  Bus B
      │                │                │                │
   [120Ω]                                             [120Ω]
Terminaison                                       Terminaison
```

**Nécessite :**
- Convertisseur UART ↔ RS-485 (MAX485, MAX3485)
- Résistances de terminaison aux extrémités (120Ω)

### USB-Serial (USB CDC)

**Émulation d'un port série via USB.**

**Comment ça marche :**
- **Puce USB-Serial** : convertit USB ↔ UART (FT232, CH340, CP2102)
- **Côté PC** : apparaît comme port COM (Windows) ou /dev/ttyUSB (Linux)
- **Côté microcontrôleur** : UART standard

**Avantages :**
- Pas besoin de port série physique sur PC
- Alimentation via USB
- Vitesses élevées (jusqu'à 3 Mbauds)
- Plug and play (avec drivers)

**Exemples :**
- Arduino Uno : utilise ATmega16U2 ou CH340
- Blue Pill + ST-Link : émule port série USB
- FTDI cables : câbles USB ↔ TTL Serial

```
PC USB ←→ [Puce USB-Serial FT232] ←→ Microcontrôleur UART
```

## Applications de la communication série

### 1. Debugging et console

**Usage le plus courant pour le développeur.**

Afficher des messages de debug depuis un microcontrôleur :

```pascal
Serial_Print('Programme demarre');
Serial_Print('Temperature: ');
Serial_PrintInt(temperature);
Serial_PrintLn(' C');
```

**Avantages :**
- Simple à implémenter
- Aucun matériel supplémentaire
- Messages texte lisibles
- Interactif (peut envoyer commandes)

### 2. Communication inter-systèmes

**Faire communiquer deux microcontrôleurs ou un PC avec un microcontrôleur.**

**Exemples :**
- Arduino ↔ Raspberry Pi
- PC ↔ STM32 pour acquisition de données
- Microcontrôleur ↔ module GPS
- ESP32 ↔ capteur CO2

### 3. Configuration et contrôle

**Envoyer des commandes à un appareil.**

**Exemples :**
- Configurer un module WiFi par commandes AT
- Contrôler un module GPS
- Paramétrer un capteur
- Interface homme-machine simple

```
> SET_TEMP 25
< OK
> GET_STATUS
< STATUS: READY
```

### 4. Protocoles industriels

**ModBus RTU, NMEA, MIDI, DMX512.**

- **ModBus RTU** : protocole maître-esclave pour industrie (sur RS-485)
- **NMEA 0183** : protocoles GPS et marine
- **MIDI** : contrôle instruments de musique (31250 bauds)
- **DMX512** : contrôle éclairage scénique (250000 bauds)

### 5. Programmation et bootloader

**Flasher un microcontrôleur via UART.**

**Exemples :**
- Arduino bootloader : programme via USB-Serial
- STM32 bootloader ROM : flashage via UART
- ESP8266/ESP32 : flashage firmware via série

## Concepts fondamentaux

### Baud rate vs Bit rate

**Confusion courante !**

**Baud rate** : nombre de **changements de signal** par seconde (symboles/s)

**Bit rate** : nombre de **bits** transmis par seconde (bits/s)

Pour UART simple : **baud rate = bit rate** (1 symbole = 1 bit)

Pour modulations complexes : **bit rate peut être > baud rate** (1 symbole = plusieurs bits)

**Dans la pratique embarquée, on utilise "baud" pour "bits/seconde".**

### Calcul de débit effectif

Pour transmettre 1 octet à 9600 bauds (config 8-N-1) :

```
1 trame = 1 start bit + 8 data bits + 1 stop bit = 10 bits
Temps par octet = 10 bits / 9600 bauds = 1.04 ms
Débit effectif = 9600 / 10 = 960 octets/seconde = 0.94 Ko/s
```

**Pour 115200 bauds (8-N-1) :**
- Débit théorique : 115200 bits/s
- Débit effectif : 11520 octets/s ≈ 11.25 Ko/s

### Buffers et flux continu

**Les données ne sont pas instantanées !**

**Buffer d'émission (TX buffer) :**
- Stocke données en attente d'envoi
- Permet de continuer le programme pendant l'envoi

**Buffer de réception (RX buffer) :**
- Stocke données reçues avant leur traitement
- Évite de perdre des données si le programme est occupé

**Problème de débordement (overflow) :**
```pascal
// SI on reçoit beaucoup de données sans les lire :
while Serial_Available do
  // OUBLI de lire !
  DoSomethingElse;

// Le buffer RX se remplit et déborde → perte de données !
```

**Solution :**
```pascal
// Lire régulièrement le buffer
while Serial_Available do
begin
  c := Serial_ReadChar;
  ProcessChar(c);
end;
```

### Synchronisation et tolérance d'erreur

**Pas d'horloge partagée en UART asynchrone !**

Chaque appareil utilise sa propre horloge. Pour que ça fonctionne :

**1. Accord sur le baud rate**

Les deux appareils doivent être configurés à la même vitesse.

**2. Tolérance d'horloge**

Les horloges ne sont jamais parfaitement précises. Tolérance acceptable : **±2-5%**

**Exemple de problème :**
```
Émetteur : 9600 bauds (horloge +3% → 9888 bauds réels)
Récepteur : 9600 bauds (horloge -2% → 9408 bauds réels)
Différence : ~5% → risque d'erreur de bit !
```

**Start bit = synchronisation**

À chaque caractère, le start bit resynchronise le récepteur, limitant l'accumulation d'erreur.

### Flow control (contrôle de flux)

**Que faire si le récepteur est trop lent ?**

**Software flow control (XON/XOFF) :**
- Le récepteur envoie XOFF (caractère spécial) pour dire "arrête d'envoyer"
- Puis XON pour dire "tu peux reprendre"
- Peu utilisé en embarqué moderne

**Hardware flow control (RTS/CTS) :**
- Fils supplémentaires : RTS (Request To Send) et CTS (Clear To Send)
- Signaux électriques pour contrôler le flux
- Plus fiable que software

```
Émetteur                    Récepteur
  TX ───────────────────────→ RX
  RX ←─────────────────────── TX
 CTS ←─────────────────────── RTS  (Récepteur dit "prêt")
 RTS ───────────────────────→ CTS  (Émetteur demande permission)
```

**En pratique :** souvent non utilisé pour du simple debug/console.

## Comparaison avec d'autres protocoles

### UART vs I2C

| Aspect | UART | I2C |
|--------|------|-----|
| **Fils** | 2 (TX/RX) | 2 (SDA/SCL) |
| **Synchrone** | Non (asynchrone) | Oui (horloge SCL) |
| **Multi-appareils** | Non (1 à 1) | Oui (multi-maître/esclave) |
| **Adressage** | Non | Oui (adresses 7/10 bits) |
| **Vitesse** | Jusqu'à 12 Mbauds | 100 kHz - 3.4 MHz |
| **Distance** | Moyenne-longue | Courte (< 1m typique) |
| **Configuration** | Baud rate à définir | Horloge fournie par maître |
| **Usage typique** | Console, GPS, modules | Capteurs, EEPROM, RTC |

### UART vs SPI

| Aspect | UART | SPI |
|--------|------|-----|
| **Fils** | 2 (TX/RX) | 4 (MOSI/MISO/SCK/CS) |
| **Synchrone** | Non | Oui (horloge SCK) |
| **Full-duplex** | Oui | Oui |
| **Vitesse** | Moyenne | Très rapide (> 10 MHz) |
| **Multi-appareils** | Non | Oui (via CS multiple) |
| **Complexité** | Simple | Moyenne |
| **Usage typique** | Debug, GPS | Carte SD, écrans, capteurs rapides |

### Quand utiliser UART ?

**Choisir UART si :**
- ✅ Communication simple point-à-point
- ✅ Debugging et console
- ✅ Distance moyenne nécessaire
- ✅ Peu de fils disponibles
- ✅ Compatibilité avec équipements existants
- ✅ Vitesse modérée suffisante

**Préférer I2C si :**
- ✅ Plusieurs capteurs à connecter
- ✅ Économie de broches GPIO
- ✅ Courte distance
- ✅ Adressage nécessaire

**Préférer SPI si :**
- ✅ Très haute vitesse nécessaire
- ✅ Écrans, cartes SD
- ✅ Full-duplex essentiel

## Différences Windows vs Linux

### Architecture des ports série

**Windows :**
- **Nomenclature** : COM1, COM2, COM3, ..., COM256
- **API** : Win32 API (CreateFile, ReadFile, WriteFile)
- **Gestion** : Gestionnaire de périphériques
- **Drivers** : Installation parfois nécessaire (CH340, FT232)

**Linux :**
- **Nomenclature** : /dev/ttyS0 (série natif), /dev/ttyUSB0 (USB-Serial), /dev/ttyACM0 (CDC)
- **API** : termios (fichiers spéciaux)
- **Gestion** : udev
- **Drivers** : souvent intégrés au kernel

### Permissions et accès

**Windows :**
- Tout utilisateur peut accéder aux ports COM
- Risque : monopolisation du port par une application

**Linux :**
- Nécessite appartenance au groupe `dialout` ou droits root
- Commande : `sudo usermod -a -G dialout $USER` (puis redémarrer session)
- Ou : `sudo chmod 666 /dev/ttyUSB0` (temporaire)

### Énumération des ports

**Windows :**
```pascal
// Via registre ou API QueryDosDevice
// Ports : COM1-COM256
```

**Linux :**
```bash
# Lister ports série
ls /dev/tty*

# Ports USB-Serial
ls /dev/ttyUSB*
ls /dev/ttyACM*

# Informations détaillées
dmesg | grep tty
```

## Outils de test et debugging

### Moniteurs série

**Windows :**
- **PuTTY** : classique, très complet
- **Tera Term** : spécialisé série
- **RealTerm** : avancé (hex, macros)
- **Arduino IDE** : moniteur série intégré
- **HTerm** : léger et efficace

**Linux :**
- **minicom** : terminal série en console
- **screen** : simple et rapide
- **picocom** : léger
- **cutecom** : GUI
- **Arduino IDE** : moniteur série

**Multi-plateforme :**
- **CoolTerm** : GUI moderne
- **Serial Port Monitor** (Windows) : analyse trafic
- **Termite** : portable, macros

### Exemple d'utilisation

**PuTTY (Windows) :**
1. Lancer PuTTY
2. Sélectionner "Serial"
3. Port : COM3 (par exemple)
4. Speed : 115200
5. Connexion

**screen (Linux) :**
```bash
# Se connecter
screen /dev/ttyUSB0 115200

# Quitter : Ctrl+A puis K
```

**minicom (Linux) :**
```bash
# Configuration
sudo minicom -s

# Se connecter
minicom -D /dev/ttyUSB0 -b 115200

# Quitter : Ctrl+A puis X
```

### Analyseurs logiques

Pour débugger problèmes de timing :

**Matériel :**
- Saleae Logic Analyzer
- DSLogic
- Oscilloscope avec décodage UART

**Logiciel :**
- PulseView (sigrok)
- Saleae Logic Software

**Permet de :**
- Voir les bits transmis en temps réel
- Décoder automatiquement les trames
- Détecter erreurs de timing
- Mesurer baud rate réel

## Pièges courants et solutions

### Problème 1 : Rien ne se passe

**Causes possibles :**
- ❌ TX/RX inversés
- ❌ Baud rate différent
- ❌ Mauvaise masse (GND non connecté)
- ❌ Niveaux de tension incompatibles (5V vs 3.3V)
- ❌ Port série déjà utilisé par autre application

**Solutions :**
- ✅ Vérifier câblage (TX → RX et RX → TX)
- ✅ Confirmer même baud rate des deux côtés
- ✅ Connecter GND commun
- ✅ Utiliser convertisseur de niveau si nécessaire
- ✅ Fermer autres applications utilisant le port

### Problème 2 : Caractères bizarres (garbage)

**Causes :**
- ❌ Baud rate incorrect
- ❌ Configuration différente (8-N-1 vs 7-E-1)
- ❌ Problème d'horloge (quartz incorrect)

**Solutions :**
- ✅ Essayer vitesses courantes : 9600, 115200
- ✅ Vérifier config : 8 bits, no parity, 1 stop bit
- ✅ Tester avec autre appareil pour isoler le problème

### Problème 3 : Données perdues

**Causes :**
- ❌ Buffer overflow (pas lu assez vite)
- ❌ Vitesse trop élevée pour le traitement
- ❌ Buffer trop petit

**Solutions :**
- ✅ Lire buffer régulièrement
- ✅ Utiliser interruptions plutôt que polling
- ✅ Augmenter taille du buffer
- ✅ Réduire baud rate si nécessaire

### Problème 4 : Transmission instable

**Causes :**
- ❌ Câble trop long ou de mauvaise qualité
- ❌ Interférences électromagnétiques
- ❌ Connexion USB instable

**Solutions :**
- ✅ Utiliser câbles blindés
- ✅ Réduire longueur de câble
- ✅ Ajouter ferrites anti-parasites
- ✅ Passer à RS-485 pour longues distances

## Vue d'ensemble des sections suivantes

Ce chapitre 14.6 est organisé en plusieurs sous-sections spécialisées :

**14.6.1 Ports COM Windows**
- API Win32 pour communication série
- Énumération et ouverture de ports
- Gestion des erreurs Windows

**14.6.2 /dev/tty* Linux**
- termios et manipulation ports série
- udev et détection automatique
- Permissions et accès

**14.6.3 Unix Domain Sockets (Linux)**
- Communication inter-processus via sockets
- Alternative à la communication série locale

Chaque section fournira du code FreePascal fonctionnel, des exemples pratiques et des astuces spécifiques à chaque plateforme.

## Prérequis

Pour tirer le meilleur parti de ce chapitre :

**Connaissances nécessaires :**
- ✅ Bases de FreePascal (types, procédures, fichiers)
- ✅ Manipulation de chaînes de caractères
- ✅ Gestion d'erreurs (try-except)
- ✅ Utilisation de la ligne de commande (Windows/Linux)

**Matériel utile (mais pas obligatoire) :**
- Microcontrôleur avec UART (Arduino, STM32, ESP32)
- Adaptateur USB-Serial (FTDI, CH340)
- Breadboard et fils
- Multimètre (pour vérifier tensions)

**Logiciels recommandés :**
- FreePascal/Lazarus installé
- Moniteur série (PuTTY, minicom, screen)
- Drivers USB-Serial (si nécessaire)

## Conclusion de l'introduction

La communication série reste un **outil fondamental** pour tout développeur travaillant avec des systèmes embarqués et IoT. Sa simplicité, sa fiabilité et son universalité en font le choix idéal pour :

- 🐛 **Debugging** : afficher messages depuis un microcontrôleur
- 🔗 **Communication** : faire dialoguer des appareils
- ⚙️ **Configuration** : paramétrer des modules
- 📊 **Acquisition** : récupérer données de capteurs

Avec FreePascal, vous disposez d'un langage **clair et portable** pour implémenter des communications série robustes, que ce soit sur Windows ou Linux. Les sections suivantes vous guideront pas à pas dans l'utilisation concrète des ports série sur ces deux plateformes.

**Prêt à faire parler vos appareils ? C'est parti ! 🚀**

---

*Ce chapitre fait partie du module 14 "Systèmes Embarqués et IoT" de la formation "FreePascal/Lazarus - Niveau Développeur Avancé - Edition Multi-plateforme Windows/Ubuntu"*

*Prochaine section : 14.6.1 Ports COM Windows*

⏭️ [Ports COM Windows](/14-systemes-embarques-iot/06.1-ports-com-windows.md)
