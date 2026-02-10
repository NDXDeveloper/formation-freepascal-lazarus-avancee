🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13. Développement Mobile et Embarqué

## Introduction

Le développement mobile et embarqué représente l'un des domaines les plus dynamiques et en expansion de l'informatique moderne. Des smartphones que nous utilisons quotidiennement aux systèmes embarqués qui contrôlent nos appareils domestiques, en passant par l'Internet des Objets (IoT), ces technologies façonnent notre monde connecté.

FreePascal et Lazarus, traditionnellement associés au développement d'applications de bureau, offrent aujourd'hui des possibilités remarquables pour créer des applications mobiles et embarquées performantes. Ce chapitre explore comment tirer parti de vos compétences Pascal pour conquérir ces nouveaux territoires technologiques.

## Pourquoi développer en mobile et embarqué avec FreePascal ?

### Avantages du Pascal pour le mobile et l'embarqué

**1. Performance native**

FreePascal compile directement en code machine natif pour chaque architecture cible :
- **ARM** : Processeurs dominants sur mobile (Android, iOS)
- **ARM Cortex-M** : Microcontrôleurs pour l'embarqué
- **x86/x64** : Émulateurs et certains appareils
- **MIPS, PowerPC** : Architectures spécialisées

Cette compilation native offre des performances comparables, voire supérieures, aux langages traditionnels du mobile comme Java ou Kotlin.

**2. Réutilisation du code**

```
┌─────────────────────────────────────────┐
│     Code Pascal partagé                 │
│     (Logique métier, algorithmes)       │
└──────────────┬──────────────────────────┘
               │
     ┌─────────┴─────────┬─────────────┐
     │                   │             │
┌────▼─────┐      ┌──────▼───┐   ┌─────▼───┐
│ Windows  │      │  Linux   │   │ Android │
│ Desktop  │      │  Desktop │   │  Mobile │
└──────────┘      └──────────┘   └─────────┘
     │                   │             │
┌────▼─────┐      ┌──────▼───┐   ┌─────▼────┐
│   iOS    │      │Raspberry │   │  ESP32   │
│  Mobile  │      │    Pi    │   │Embarqué  │
└──────────┘      └──────────┘   └──────────┘
```

Un même code métier peut être partagé entre :
- Applications desktop (Windows, Linux, macOS)
- Applications mobiles (Android, expérimental iOS)
- Systèmes embarqués (Raspberry Pi, Arduino, microcontrôleurs)

**3. Faible empreinte mémoire**

Les applications FreePascal sont particulièrement adaptées aux systèmes à ressources limitées :
- **Pas de machine virtuelle** : Contrairement à Java ou .NET
- **Pas de garbage collector** : Gestion mémoire déterministe
- **Exécutables compacts** : Quelques Ko à quelques Mo selon la complexité
- **Démarrage instantané** : Pas de JIT ou d'interprétation

**4. Développement cross-platform depuis un seul poste**

Avec FreePascal, vous pouvez développer et compiler pour différentes cibles depuis votre machine de développement principale :

```
┌────────────────────────────────────┐
│  PC de développement               │
│  (Windows ou Linux)                │
│                                    │
│  ┌──────────────────────────┐      │
│  │   IDE Lazarus            │      │
│  │   + FreePascal           │      │
│  │   + Cross-compilers      │      │
│  └──────────────────────────┘      │
│              │                     │
│    Compilation croisée             │
│              │                     │
└──────────────┼─────────────────────┘
               │
    ┌──────────┴──────────┬──────────┐
    │                     │          │
┌───▼────┐         ┌──────▼──┐  ┌────▼───┐
│ ARM32  │         │  ARM64  │  │  x86   │
│Android │         │  iOS    │  │Émulat. │
└────────┘         └─────────┘  └────────┘
```

## Panorama des plateformes supportées

### Plateformes mobiles

#### Android

**État du support** : ⭐⭐⭐⭐⭐ Excellent

- **Outil principal** : LAMW (Lazarus Android Module Wizard)
- **Architectures** : armeabi-v7a, arm64-v8a, x86, x86_64
- **API Android** : Accès complet via JNI (Java Native Interface)
- **Maturité** : Production-ready, nombreuses applications publiées

**Versions Android supportées** :
- Minimum : Android 4.1 (API 16)
- Recommandé : Android 5.0+ (API 21+)
- Optimal : Android 8.0+ (API 26+)

#### iOS

**État du support** : ⭐⭐⭐☆☆ Expérimental

- **Outil** : PasCocoa / Custom Drawn
- **Architectures** : arm64, x86_64 (simulateur)
- **Limitations** : Support limité, communauté plus restreinte
- **Complexité** : Nécessite un Mac pour la signature et le déploiement

**Note** : Le support iOS est fonctionnel mais moins mature que Android. Convient surtout aux projets expérimentaux ou aux développeurs prêts à contribuer à l'écosystème.

### Plateformes embarquées

#### Raspberry Pi

**État du support** : ⭐⭐⭐⭐⭐ Excellent

- **Systèmes** : Raspberry Pi OS (anciennement Raspbian), Ubuntu
- **Architectures** : ARM32 (Pi 1-3), ARM64 (Pi 3-4-5)
- **Interface graphique** : LCL complète avec GTK/Qt
- **GPIO** : Accès direct aux broches via wiringPi ou sysfs

**Cas d'usage** :
- Domotique et contrôle maison
- Affichage d'informations (kiosque)
- Serveurs légers
- Projets éducatifs
- Prototypage IoT

#### Arduino et AVR

**État du support** : ⭐⭐⭐⭐☆ Bon

- **Microcontrôleurs** : AVR (ATmega, ATtiny)
- **Compilation** : Cross-compilation depuis PC
- **Bibliothèques** : Accès aux périphériques (UART, SPI, I2C, ADC)
- **Taille code** : Optimisé pour mémoire limitée (quelques Ko)

**Avantages** :
- Alternative à C/C++ pour Arduino
- Syntaxe Pascal plus lisible
- Débogage facilité
- Réutilisation de code desktop

#### ARM Cortex-M (STM32, LPC, etc.)

**État du support** : ⭐⭐⭐⭐☆ Bon

- **Familles** : STM32, LPC, SAM, Kinetis
- **IDE** : Embedded FreePascal ou Lazarus
- **RTOS** : Support pour FreeRTOS
- **Périphériques** : Accès bas niveau complet

**Applications** :
- Systèmes temps réel
- Dispositifs médicaux
- Automatisation industrielle
- Produits électroniques grand public

#### ESP32 / ESP8266

**État du support** : ⭐⭐⭐☆☆ Émergent

- **Capacités** : WiFi, Bluetooth intégrés
- **Mémoire** : Plus généreuse que les AVR traditionnels
- **Communauté** : Support communautaire croissant
- **SDK** : Intégration avec ESP-IDF

**Idéal pour** :
- Projets IoT connectés
- Capteurs sans fil
- Contrôle à distance
- Home automation

## Architecture du développement mobile avec FreePascal

### Modèle hybride (cas d'Android avec LAMW)

```
┌─────────────────────────────────────────────┐
│           Application Android               │
├─────────────────────────────────────────────┤
│                                             │
│  ┌──────────────────────────────────────┐   │
│  │     Couche Interface (Java/Kotlin)   │   │
│  │  - Activities Android                │   │
│  │  - Views et Layouts                  │   │
│  │  - Services système                  │   │
│  └────────────┬─────────────────────────┘   │
│               │                             │
│               │ JNI (Java Native Interface) │
│               │                             │
│  ┌────────────▼─────────────────────────┐   │
│  │    Couche Logique (Pascal)           │   │
│  │  - Algorithmes métier                │   │
│  │  - Traitement de données             │   │
│  │  - Base de données                   │   │
│  │  - Calculs complexes                 │   │
│  └──────────────────────────────────────┘   │
│                                             │
│  Compilation native ARM32/ARM64             │
└─────────────────────────────────────────────┘
```

**Avantages de ce modèle** :
- Interface native Android (look & feel optimal)
- Performance native pour la logique métier
- Accès complet aux API Android
- Réutilisation du code Pascal existant

### Modèle natif pur (cas de l'embarqué)

```
┌──────────────────────────────────────┐
│    Application Embarquée             │
│    (100% FreePascal)                 │
├──────────────────────────────────────┤
│                                      │
│  ┌────────────────────────────────┐  │
│  │  Code Application Pascal       │  │
│  │  - Interface utilisateur       │  │
│  │  - Logique métier              │  │
│  │  - Gestion périphériques       │  │
│  └────────────┬───────────────────┘  │
│               │                      │
│  ┌────────────▼───────────────────┐  │
│  │  RTL FreePascal                │  │
│  │  (Runtime Library)             │  │
│  └────────────┬───────────────────┘  │
│               │                      │
│  ┌────────────▼──────────────────┐   │
│  │  Accès Matériel Direct        │   │
│  │  - Registres microcontrôleur  │   │
│  │  - Interruptions              │   │
│  │  - Périphériques (GPIO, etc.) │   │
│  └───────────────────────────────┘   │
│                                      │
└──────────────────────────────────────┘
```

**Caractéristiques** :
- Contrôle total sur le matériel
- Pas de système d'exploitation (bare metal possible)
- Empreinte mémoire minimale
- Déterminisme parfait

## Différences clés avec le développement desktop

### Contraintes matérielles

#### Mémoire limitée

**Desktop** :
- RAM : 4 GB - 64 GB
- Stockage : 256 GB - plusieurs TB
- Mémoire virtuelle illimitée

**Mobile/Embarqué** :
- RAM : 512 MB - 8 GB (mobile), 2 KB - 512 KB (microcontrôleur)
- Stockage : 8 GB - 512 GB (mobile), 8 KB - 1 MB (microcontrôleur)
- Pas ou peu de mémoire virtuelle

**Implications** :
- Optimisation critique de la mémoire
- Réutilisation de buffers
- Structures de données compactes
- Pas de gaspillage autorisé

#### Puissance de calcul

**Desktop** :
- CPU : 2-16+ cœurs à 2-5 GHz
- GPU dédié puissant
- Refroidissement actif

**Mobile/Embarqué** :
- Mobile : 4-8 cœurs à 1-3 GHz, GPU intégré
- Embarqué : 1 cœur à 8-200 MHz, pas de GPU
- Refroidissement passif ou inexistant

**Implications** :
- Algorithmes optimisés obligatoires
- Calculs à virgule fixe au lieu de flottante
- Éviter les boucles coûteuses
- Utilisation judicieuse du multithreading

#### Autonomie énergétique

**Desktop** :
- Alimentation secteur permanente
- Consommation : 50-500W

**Mobile/Embarqué** :
- Batterie : 2000-5000 mAh (mobile), quelques jours à années (embarqué)
- Consommation critique pour l'autonomie

**Implications** :
- Modes veille et économie d'énergie
- Réduction de la fréquence CPU quand possible
- Désactivation des périphériques inutilisés
- Optimisation des réveils et activités

### Interface utilisateur

#### Écrans tactiles

**Desktop** :
- Souris et clavier précis
- Multi-fenêtrage
- Écrans larges (15"-32"+)

**Mobile** :
- Écran tactile principal
- Gestes multi-touch
- Écrans petits (4"-7")
- Rotation portrait/paysage

**Implications pour le développement** :
- Boutons et zones tactiles suffisamment grandes (44x44 dp minimum)
- Interface adaptative à l'orientation
- Gestes intuitifs (swipe, pinch, long press)
- Pas de survol souris (hover)

#### Fragments d'écran limités

**Principes de conception mobile** :
- **Une action principale par écran**
- **Navigation simple et claire**
- **Hiérarchie visuelle évidente**
- **Minimiser le texte**
- **Priorité au contenu**

```
┌─────────────────────┐
│   Barre de titre    │  ← Concise
├─────────────────────┤
│                     │
│   Contenu           │  ← Focus principal
│   Principal         │
│                     │
│   [Action]          │  ← Bouton visible
│                     │
├─────────────────────┤
│  Navigation         │  ← 3-5 items max
└─────────────────────┘
```

### Cycle de vie des applications

#### Desktop

```
Lancement → Exécution → Fermeture
```

- Application maître de son destin
- Fermeture explicite par l'utilisateur
- État prévisible

#### Mobile

```
Lancement → Active → Pause → Stop → Destruction
     ↑         ↓                ↓
     └─────────┴────────────────┘
           (Peut être tué à tout moment)
```

- Système peut suspendre/tuer l'application
- Interruptions fréquentes (appels, notifications)
- Doit sauvegarder l'état régulièrement
- Restauration automatique nécessaire

**Événements à gérer** :
- **onCreate** : Initialisation
- **onStart** : Application visible
- **onResume** : Application interactive
- **onPause** : Perte du focus (appel, notification)
- **onStop** : Application en arrière-plan
- **onDestroy** : Nettoyage

### Connectivité variable

#### Desktop

- Connexion stable (Ethernet/WiFi)
- Bande passante élevée
- Latence faible

#### Mobile/Embarqué

- Connexions intermittentes (3G/4G/5G/WiFi)
- Bande passante variable
- Latence imprévisible
- Modes avion

**Stratégies de développement** :
- Mode hors ligne obligatoire
- Synchronisation intelligente
- Mise en cache agressive
- Gestion des erreurs réseau
- Indicateurs de connexion pour l'utilisateur

### Sécurité et permissions

#### Desktop

- Accès relativement libre aux ressources
- Utilisateur fait confiance aux applications installées
- Antivirus comme protection

#### Mobile/Embarqué

- Modèle de permissions strict
- Sandboxing des applications
- Demande explicite à l'utilisateur
- Révocation possible à tout moment

**Permissions typiques Android** :
- Internet
- Localisation
- Caméra
- Microphone
- Stockage
- Contacts
- Téléphone

**Règles** :
- Demander uniquement ce qui est nécessaire
- Expliquer pourquoi
- Gérer les refus gracieusement
- Permissions runtime (Android 6+)

## Outils et environnement de développement

### Configuration requise

#### Pour développer sur Android (LAMW)

**Logiciels** :
- Lazarus 2.0+ avec FreePascal 3.2+
- Java JDK 8 ou 11
- Android SDK (API 21 minimum recommandé)
- Android NDK r21 ou r23
- Ant ou Gradle (build system)

**Matériel** :
- 8 GB RAM minimum (16 GB recommandé)
- 20 GB espace disque libre
- Appareil Android ou émulateur pour les tests

**Systèmes hôtes** :
- ✓ Windows 10/11
- ✓ Ubuntu 18.04+ / Linux Mint
- ✓ macOS (avec quelques limitations)

#### Pour développer sur embarqué

**Logiciels** :
- FreePascal avec support de la cible (AVR, ARM)
- Outils de flash (avrdude, openocd, stlink)
- Drivers USB pour programmateurs
- Terminal série pour débogage

**Matériel** :
- Carte de développement cible
- Programmateur/debugger (selon la plateforme)
- Câbles de connexion

### Émulateurs et simulateurs

#### Android

**Android Virtual Device (AVD)** :
- Fourni par Android Studio
- Émulation complète
- Lent sur les machines anciennes
- Support x86 plus rapide que ARM

**Alternatives** :
- **Genymotion** : Plus rapide, version gratuite limitée
- **BlueStacks** : Orienté jeux
- **Appareil physique** : Toujours préférable pour les tests réels

#### iOS

**Simulateur iOS** :
- Nécessite macOS et Xcode
- Très rapide (natif)
- Ne simule pas certaines fonctionnalités (notifications push)

#### Embarqué

**QEMU** :
- Émulation de nombreuses architectures ARM
- Utile pour tests basiques
- Pas d'émulation des périphériques spécifiques

**Proteus** :
- Simulation électronique avec microcontrôleurs
- Visualisation graphique
- Commercial

**Matériel réel** :
- Souvent plus simple et moins cher
- Tests réalistes
- Débogage plus complexe

## Stratégies de développement

### Approche progressive

**Phase 1 : Prototype desktop**
```
Développer et tester la logique sur desktop
↓
Validation des algorithmes
↓
Interface utilisateur basique
```

**Phase 2 : Portage de la logique**
```
Isoler le code métier
↓
Créer une bibliothèque partagée
↓
Tester sur la plateforme cible
```

**Phase 3 : Interface native**
```
Développer l'UI spécifique à la plateforme
↓
Intégrer avec la logique portée
↓
Tests utilisateur
```

**Phase 4 : Optimisation**
```
Profiling et identification des goulots
↓
Optimisation mémoire et performance
↓
Tests de batterie et réseau
```

### Séparation des préoccupations

**Architecture en couches** :

```
┌──────────────────────────────────┐
│  Présentation (UI)               │  ← Spécifique plateforme
├──────────────────────────────────┤
│  Logique Métier                  │  ← Partagé
├──────────────────────────────────┤
│  Accès Données                   │  ← Partagé avec adaptation
├──────────────────────────────────┤
│  Services Système                │  ← Abstraction par plateforme
└──────────────────────────────────┘
```

**Exemple de structure de projet** :

```
MonProjet/
├── Common/              ← Code partagé
│   ├── Business/
│   │   ├── Models.pas
│   │   └── Services.pas
│   ├── Data/
│   │   └── Database.pas
│   └── Utils/
│       └── Helpers.pas
├── Desktop/             ← Application desktop
│   ├── Forms/
│   └── Main.lpr
├── Android/             ← Application Android
│   ├── jni/
│   ├── src/
│   └── AndroidApp.lpr
└── Embedded/            ← Firmware embarqué
    ├── Hardware/
    └── Firmware.lpr
```

### Tests et débogage

**Tests unitaires** :
- Tester la logique métier sur desktop d'abord
- Framework FPCUnit
- Automatisation avec CI/CD

**Tests d'intégration** :
- Tester sur appareils réels
- Multiples configurations (écrans, versions OS)
- Différentes conditions réseau

**Débogage** :
- Logs structurés et traçables
- Remote debugging pour mobile
- JTAG/SWD pour embarqué
- Outils de monitoring système

## Défis courants et solutions

### Gestion de la fragmentation (Android)

**Problème** : Des milliers de combinaisons appareil/OS

**Solutions** :
- Cibler les versions API populaires
- Tester sur appareils représentatifs
- Utiliser des bibliothèques de support
- Interface responsive
- Tester avec différentes tailles d'écran

### Performance sur matériel limité

**Problème** : Ressources contraintes

**Solutions** :
- Profiling systématique
- Algorithmes optimisés (complexité)
- Structures de données compactes
- Lazy loading et pagination
- Cache intelligent
- Compilation avec optimisations activées

### Débogage sur embarqué

**Problème** : Outils limités, pas d'interface graphique

**Solutions** :
- UART pour logs série
- LED pour signaux visuels
- Breakpoints logiciels
- JTAG/SWD pour debugging hardware
- Émulation partielle sur desktop

### Mises à jour et maintenance

**Problème** : Applications déployées difficiles à mettre à jour

**Solutions** :
- **Mobile** : Déploiement via stores avec mises à jour automatiques
- **Embarqué** : OTA (Over The Air) updates si connecté
- **Versioning** : Gestion stricte des versions
- **Compatibilité** : Préserver les formats de données

## Écosystème et communauté

### Ressources d'apprentissage

**Documentation officielle** :
- Wiki FreePascal : Section Embedded
- Forum Lazarus : Mobile Development
- GitHub : Exemples et projets open source

**Communautés actives** :
- Forum Lazarus / Free Pascal
- Reddit : r/freepascal
- Telegram / Discord : Groupes dédiés
- YouTube : Tutoriels vidéo

**Projets exemplaires** :
- LAMW Demo Projects
- Embedded FreePascal Examples
- Castle Game Engine (jeux multi-plateformes)

### Contribution et écosystème

**Comment contribuer** :
- Rapporter des bugs
- Écrire de la documentation
- Créer des tutoriels
- Développer des packages
- Partager vos projets

**Packages populaires** :
- **LAMW** : Android development
- **mORMot** : Framework serveur léger pour IoT
- **Synapse** : Réseau pour systèmes embarqués
- **BGRABitmap** : Graphiques pour Raspberry Pi

## Perspectives d'avenir

### Tendances émergentes

**WebAssembly** :
- FreePascal peut compiler en WASM
- Applications web avec performance native
- Réutilisation du code Pascal dans le navigateur

**Edge Computing** :
- Intelligence artificielle sur embarqué
- Traitement local des données
- Réduction de la latence

**5G et IoT** :
- Connectivité omniprésente
- Objets ultra-connectés
- Nouveaux cas d'usage

**IA embarquée** :
- TensorFlow Lite sur mobile
- Modèles optimisés pour ARM
- Inférence en temps réel

### Opportunités professionnelles

**Secteurs demandeurs** :
- Domotique et smart home
- Industrie 4.0
- Santé connectée (e-health)
- Automobile (systèmes embarqués)
- Agriculture de précision
- Objets connectés grand public

**Compétences valorisées** :
- Développement cross-platform
- Optimisation pour ressources limitées
- Connaissance des protocoles IoT
- Sécurité embarquée
- Gestion de l'énergie

## Conclusion de l'introduction

Le développement mobile et embarqué avec FreePascal ouvre un monde de possibilités pour les développeurs Pascal. Que ce soit pour créer des applications Android grand public, programmer des microcontrôleurs pour l'IoT, ou développer des systèmes embarqués critiques, FreePascal offre les outils et la performance nécessaires.

Les sections suivantes de ce chapitre détailleront :
- **LAMW** pour le développement Android
- **Architecture Android** et intégration JNI
- **Custom Drawn** pour interfaces portables
- **Développement pour iOS** (expérimental)
- **Raspberry Pi** et Linux embarqué
- **Arduino et microcontrôleurs**
- **Optimisations** spécifiques au mobile et à l'embarqué
- **Distribution** sur les stores et déploiement

Préparez-vous à explorer comment transformer vos compétences Pascal en applications mobiles modernes et systèmes embarqués performants !

---

**Points clés à retenir** :

✓ FreePascal compile nativement pour ARM, permettant des applications performantes  
✓ Le code métier peut être partagé entre desktop, mobile et embarqué  
✓ LAMW offre un support mature pour Android  
✓ Les contraintes matérielles nécessitent une approche optimisée  
✓ L'écosystème FreePascal/Lazarus s'étend constamment vers de nouvelles plateformes  
✓ La communauté est active et accueillante pour les nouveaux développeurs

**Prochaine section** : 13.1 LAMW - Lazarus Android Module Wizard

⏭️ [LAMW - Lazarus Android Module Wizard](/13-developpement-mobile-embarque/01-lamw-lazarus-android-module-wizard.md)
