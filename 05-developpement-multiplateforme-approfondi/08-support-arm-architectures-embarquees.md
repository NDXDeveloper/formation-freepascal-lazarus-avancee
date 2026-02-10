🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Support ARM et Architectures Embarquées avec FreePascal/Lazarus

## Introduction

Le processeur ARM est aujourd'hui l'architecture la plus répandue dans le monde des systèmes embarqués. On le trouve dans les smartphones, les tablettes, les Raspberry Pi, et de nombreux autres appareils IoT (Internet des Objets). FreePascal offre un excellent support pour ces architectures, permettant de développer des applications performantes pour ces plateformes.

## Qu'est-ce que l'architecture ARM ?

### Comprendre les bases

ARM (Advanced RISC Machine) est une architecture de processeur différente de celle que vous trouvez habituellement dans votre PC (x86/x64). Les principales différences sont :

- **Consommation réduite** : Les processeurs ARM consomment beaucoup moins d'énergie
- **Architecture RISC** : Instructions plus simples mais plus rapides
- **Coût réduit** : Moins chers à produire, idéaux pour les appareils embarqués

### Les variantes ARM supportées par FreePascal

FreePascal supporte plusieurs versions de l'architecture ARM :

- **ARMv6** : Raspberry Pi Zero, Pi 1
- **ARMv7** : Raspberry Pi 2, nombreux smartphones
- **ARMv8/ARM64** : Raspberry Pi 3/4, smartphones modernes
- **ARM Cortex-M** : Microcontrôleurs (STM32, Arduino Due)

## Configuration de l'environnement de développement

### Installation sur Windows pour développement ARM

#### Étape 1 : Installer Lazarus standard

Téléchargez et installez Lazarus normalement depuis le site officiel. Cette installation servira de base pour le développement.

#### Étape 2 : Installer le compilateur croisé ARM

Le compilateur croisé permet de compiler sur Windows pour ARM :

```bash
# Télécharger le package cross-compile depuis :
# https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20arm/

# Ou utiliser fpcupdeluxe (outil graphique recommandé pour débutants)
# qui automatise l'installation des cross-compilateurs
```

#### Étape 3 : Configuration dans Lazarus

1. Ouvrir Lazarus
2. Aller dans **Projet → Options du projet**
3. Dans **Compilateur → Chemins** :
   - Définir le chemin vers le compilateur ARM
4. Dans **Compilateur → Configuration et Cible** :
   - OS cible : Linux (pour Raspberry Pi) ou Embedded (pour microcontrôleurs)
   - CPU cible : arm
   - Type de processeur : ARMV7 (selon votre cible)

### Installation sur Ubuntu pour développement ARM

#### Installation native sur Ubuntu

```bash
# Installer les outils de base
sudo apt update  
sudo apt install lazarus

# Installer le support ARM
sudo apt install fpc-source  
sudo apt install gcc-arm-linux-gnueabihf  
sudo apt install fp-compiler-arm-linux

# Pour ARM 64 bits
sudo apt install gcc-aarch64-linux-gnu
```

#### Configuration simple

```bash
# Vérifier l'installation
fpc -Parm -i

# Compiler un programme simple pour ARM
fpc -Parm -Tlinux monprogramme.pas
```

## Compilation croisée : Les concepts essentiels

### Qu'est-ce que la compilation croisée ?

La compilation croisée (cross-compilation) permet de compiler un programme sur une machine (votre PC) pour qu'il s'exécute sur une autre architecture (ARM). C'est essentiel car compiler directement sur un Raspberry Pi ou un microcontrôleur peut être très lent.

### Configuration de base pour Raspberry Pi

#### Fichier de configuration projet (.lpi)

Voici les paramètres essentiels à configurer dans votre projet Lazarus :

```xml
<CompilerOptions>
  <Target>
    <Filename Value="monapp"/>
  </Target>
  <CodeGeneration>
    <TargetCPU Value="arm"/>
    <TargetOS Value="linux"/>
    <TargetProcessor Value="ARMV7A"/>
  </CodeGeneration>
</CompilerOptions>
```

#### Code Pascal avec conditions de compilation

```pascal
program MonAppARM;

{$mode objfpc}{$H+}

uses
  SysUtils;

begin
  WriteLn('Application FreePascal sur ARM');

  {$IFDEF CPUARM}
    WriteLn('Exécution sur processeur ARM');
    {$IFDEF CPUARM32}
      WriteLn('Architecture ARM 32 bits');
    {$ENDIF}
    {$IFDEF CPUARM64}
      WriteLn('Architecture ARM 64 bits');
    {$ENDIF}
  {$ELSE}
    WriteLn('Exécution sur autre architecture');
  {$ENDIF}

  // Afficher des informations système
  WriteLn('Processeur : ', {$I %FPCTARGETCPU%});
  WriteLn('OS : ', {$I %FPCTARGETOS%});
end.
```

## Développement pour Raspberry Pi

### Configuration spécifique Raspberry Pi

Le Raspberry Pi est l'une des plateformes ARM les plus populaires pour apprendre le développement embarqué.

#### Identifier votre modèle de Raspberry Pi

| Modèle | Architecture | CPU FreePascal | Notes |
|--------|-------------|----------------|--------|
| Pi Zero/1 | ARMv6 | ARMV6 | Plus ancien, moins performant |
| Pi 2 | ARMv7 | ARMV7A | 32 bits, bon compromis |
| Pi 3 | ARMv8 | ARMV7A ou AARCH64 | Peut fonctionner en 32 ou 64 bits |
| Pi 4 | ARMv8 | ARMV7A ou AARCH64 | Le plus puissant |

#### Compilation depuis Windows

```pascal
// Script de compilation batch pour Windows
// Sauvegarder comme compile_rpi.bat

fpc -Parm -Tlinux -ARMV7A ^
    -FD"C:\lazarus\fpc\bin\arm-linux\" ^
    -XParm-linux-gnueabihf- ^
    -Fl"C:\lazarus\cross\lib\arm-linux\" ^
    monprogramme.pas
```

#### Compilation depuis Ubuntu

```bash
# Compilation simple pour Raspberry Pi
fpc -Parm -Tlinux monprogramme.pas

# Avec optimisations
fpc -Parm -Tlinux -O3 -CfVFPV3 monprogramme.pas
```

### Transfert et exécution sur Raspberry Pi

#### Méthode 1 : Via SSH/SCP

```bash
# Depuis Windows (avec WSL) ou Ubuntu
# Copier le fichier compilé
scp monprogramme pi@192.168.1.100:/home/pi/

# Se connecter et exécuter
ssh pi@192.168.1.100  
chmod +x monprogramme
./monprogramme
```

#### Méthode 2 : Via partage réseau

Configurer un dossier partagé entre votre PC et le Raspberry Pi pour faciliter les transferts.

## Développement pour microcontrôleurs

### Introduction aux microcontrôleurs ARM

Les microcontrôleurs ARM (comme les STM32, Arduino Due) sont différents des systèmes Linux embarqués :

- **Pas de système d'exploitation** : Le code s'exécute directement sur le matériel
- **Ressources limitées** : Quelques Ko de RAM, quelques centaines de Ko de Flash
- **Temps réel** : Contrôle précis du timing

### Configuration pour STM32

#### Installation des outils

```pascal
// Configuration de base pour STM32F103 (Blue Pill)
program STM32Blink;

{$mode objfpc}
{$inline on}

// Définir le microcontrôleur cible
{$define STM32F103X8}

uses
  cortexm3;  // Support Cortex-M3

const
  // Adresses des registres GPIO
  RCC_BASE = $40021000;
  GPIOC_BASE = $40011000;

  // Registres
  RCC_APB2ENR = RCC_BASE + $18;
  GPIOC_CRH = GPIOC_BASE + $04;
  GPIOC_ODR = GPIOC_BASE + $0C;

procedure DelayMs(ms: longword);  
var
  i: longword;
begin
  for i := 1 to ms * 1000 do
    asm nop end;
end;

begin
  // Activer l'horloge pour GPIOC
  PLongWord(RCC_APB2ENR)^ := PLongWord(RCC_APB2ENR)^ or (1 shl 4);

  // Configurer PC13 en sortie
  PLongWord(GPIOC_CRH)^ := $00200000;

  // Boucle principale : faire clignoter la LED
  while True do
  begin
    PLongWord(GPIOC_ODR)^ := PLongWord(GPIOC_ODR)^ xor (1 shl 13);
    DelayMs(500);
  end;
end.
```

### Compilation pour microcontrôleurs

```bash
# Compiler pour STM32
fpc -Parm -Tembedded -Cparmv7m -WpSTM32F103X8 monprogramme.pas

# Générer un fichier binaire pour flasher
arm-none-eabi-objcopy -O binary monprogramme.elf monprogramme.bin
```

## Optimisations spécifiques ARM

### Utilisation du FPU (Floating Point Unit)

Les processeurs ARM modernes incluent souvent une unité de calcul flottant :

```pascal
{$IFDEF CPUARM}
  {$IFDEF FPUVFPV2}
    // Raspberry Pi 1
    {$FPUTYPE VFPV2}
  {$ENDIF}

  {$IFDEF FPUVFPV3}
    // Raspberry Pi 2/3
    {$FPUTYPE VFPV3}
  {$ENDIF}

  {$IFDEF FPUVFPV4}
    // Processeurs plus récents
    {$FPUTYPE VFPV4}
  {$ENDIF}
{$ENDIF}
```

### Optimisations de compilation

```pascal
// Directives pour optimiser sur ARM
{$OPTIMIZATION ON}
{$OPTIMIZATION LEVEL3}        // Optimisation maximale
{$OPTIMIZATION REGVAR}         // Utiliser les registres
{$OPTIMIZATION PEEPHOLE}       // Optimisations peephole
{$OPTIMIZATION ASMCSE}         // Common subexpression elimination

// Pour les systèmes embarqués avec peu de RAM
{$OPTIMIZATION SIZE}           // Optimiser la taille plutôt que la vitesse
```

## Gestion de la mémoire sur systèmes embarqués

### Contraintes mémoire

Sur les systèmes embarqués ARM, la gestion de la mémoire est cruciale :

```pascal
program GestionMemoireARM;

{$mode objfpc}

const
  // Définir des limites pour systèmes embarqués
  MAX_BUFFER = 1024;  // Limiter les buffers

type
  // Utiliser des types compacts
  TCompactRecord = packed record
    Flag: Byte;      // 1 octet au lieu de Boolean (4 octets)
    Value: Word;     // 2 octets au lieu de Integer si suffisant
    Data: array[0..9] of Byte;  // Tableau fixe plutôt que dynamique
  end;

var
  // Variables globales statiques (pas d'allocation dynamique)
  Buffer: array[0..MAX_BUFFER-1] of Byte;

procedure ProcessData;  
var
  // Variables locales sur la pile
  temp: Word;
begin
  // Éviter les allocations dynamiques
  // Utiliser des buffers pré-alloués

  // Mauvais pour embarqué :
  // s := 'Hello ' + 'World';  // Allocation dynamique

  // Bon pour embarqué :
  // Utiliser des buffers fixes
end;

begin
  // Code principal minimaliste
  ProcessData;
end.
```

## Accès au matériel (GPIO, I2C, SPI)

### Accès aux GPIO sur Raspberry Pi

```pascal
unit GPIOControl;

{$mode objfpc}{$H+}

interface

type
  TGPIOPin = 0..53;  // Raspberry Pi a jusqu'à 54 pins GPIO
  TPinMode = (pmInput, pmOutput);
  TPinValue = (pvLow, pvHigh);

procedure InitGPIO;  
procedure SetPinMode(pin: TGPIOPin; mode: TPinMode);  
procedure WritePin(pin: TGPIOPin; value: TPinValue);  
function ReadPin(pin: TGPIOPin): TPinValue;  
procedure CleanupGPIO;

implementation

uses
  BaseUnix, SysUtils;

var
  gpio_mem: PByte;

const
  BCM2708_PERI_BASE = $20000000;  // Pour Pi 1
  // BCM2708_PERI_BASE = $3F000000;  // Pour Pi 2/3
  // BCM2708_PERI_BASE = $FE000000;  // Pour Pi 4

  GPIO_BASE = BCM2708_PERI_BASE + $200000;

  // Registres GPIO
  GPFSEL0 = 0;    // Function Select
  GPSET0 = 28;    // Pin Output Set
  GPCLR0 = 40;    // Pin Output Clear
  GPLEV0 = 52;    // Pin Level

procedure InitGPIO;  
var
  mem_fd: Integer;
begin
  // Ouvrir /dev/mem pour accès direct au matériel
  mem_fd := fpOpen('/dev/mem', O_RDWR or O_SYNC);
  if mem_fd < 0 then
    raise Exception.Create('Erreur ouverture /dev/mem (exécuter en sudo)');

  // Mapper la mémoire GPIO
  gpio_mem := PByte(fpMmap(nil, 4096,
    PROT_READ or PROT_WRITE,
    MAP_SHARED,
    mem_fd,
    GPIO_BASE));

  fpClose(mem_fd);
end;

procedure SetPinMode(pin: TGPIOPin; mode: TPinMode);  
var
  reg, shift: Integer;
  val: LongWord;
begin
  reg := pin div 10;
  shift := (pin mod 10) * 3;

  val := PLongWord(gpio_mem + GPFSEL0 + reg * 4)^;
  val := val and not (7 shl shift);  // Clear 3 bits

  if mode = pmOutput then
    val := val or (1 shl shift);  // Set as output

  PLongWord(gpio_mem + GPFSEL0 + reg * 4)^ := val;
end;

procedure WritePin(pin: TGPIOPin; value: TPinValue);  
begin
  if value = pvHigh then
    PLongWord(gpio_mem + GPSET0)^ := 1 shl pin
  else
    PLongWord(gpio_mem + GPCLR0)^ := 1 shl pin;
end;

function ReadPin(pin: TGPIOPin): TPinValue;  
begin
  if (PLongWord(gpio_mem + GPLEV0)^ and (1 shl pin)) <> 0 then
    Result := pvHigh
  else
    Result := pvLow;
end;

procedure CleanupGPIO;  
begin
  if gpio_mem <> nil then
    fpMunmap(gpio_mem, 4096);
end;

end.
```

## Débogage d'applications ARM

### Débogage à distance

#### Configuration du débogage distant depuis Windows

1. Installer gdb-multiarch sur le Raspberry Pi :
```bash
sudo apt install gdbserver
```

2. Lancer gdbserver sur le Pi :
```bash
gdbserver :2345 monprogramme
```

3. Dans Lazarus sur Windows :
   - **Outils → Options → Débogueur**
   - Type : GNU remote debugger (gdbserver)
   - Hôte : IP_du_raspberry_pi
   - Port : 2345

### Messages de débogage simples

Pour les systèmes sans débogueur, utiliser des messages :

```pascal
procedure DebugLog(const msg: string);  
begin
  {$IFDEF RASPBERRY_PI}
    // Écrire dans un fichier log
    Append(LogFile);
    WriteLn(LogFile, DateTimeToStr(Now), ': ', msg);
    Close(LogFile);
  {$ENDIF}

  {$IFDEF EMBEDDED}
    // Sur microcontrôleur, utiliser l'UART
    SendUART(msg + #13#10);
  {$ENDIF}
end;
```

## Conseils et bonnes pratiques

### 1. Toujours tester sur la cible réelle

Le comportement peut différer entre l'émulation et le matériel réel, surtout pour :
- Le timing précis
- L'accès au matériel
- La gestion mémoire

### 2. Gérer les différences d'endianness

ARM peut être little-endian ou big-endian :

```pascal
function SwapEndian32(value: LongWord): LongWord;  
begin
  {$IFDEF ENDIAN_BIG}
    Result := value;
  {$ELSE}
    Result := ((value and $FF) shl 24) or
              ((value and $FF00) shl 8) or
              ((value and $FF0000) shr 8) or
              ((value and $FF000000) shr 24);
  {$ENDIF}
end;
```

### 3. Optimiser pour la batterie

Sur systèmes embarqués alimentés par batterie :

```pascal
procedure EnterLowPowerMode;  
begin
  {$IFDEF CPUARM}
    // Utiliser les instructions WFI (Wait For Interrupt)
    asm
      wfi
    end;
  {$ENDIF}
end;
```

### 4. Gérer les limitations de pile (stack)

Les systèmes embarqués ont souvent une pile limitée :

```pascal
{$MAXSTACKSIZE 4096}  // Limiter la taille de la pile

procedure ProcessLargeData;  
var
  // Éviter les grandes variables locales
  // buffer: array[0..10000] of Byte;  // MAUVAIS !
begin
  // Utiliser des allocations dynamiques si nécessaire
  // ou des variables globales
end;
```

## Ressources et outils utiles

### Outils de développement

- **FPCUpDeluxe** : Installation facile des cross-compilateurs
- **ST-Link** : Programmateur pour STM32
- **OpenOCD** : Débogage pour microcontrôleurs
- **Raspberry Pi Imager** : Préparation des cartes SD

### Documentation

- Wiki FreePascal ARM : https://wiki.freepascal.org/ARM
- Forum Lazarus : https://forum.lazarus.freepascal.org/
- Documentation Raspberry Pi : https://www.raspberrypi.org/documentation/

### Bibliothèques utiles

- **PascalIO** : Accès GPIO pour Raspberry Pi
- **fp-rpi-hal** : Hardware Abstraction Layer pour Pi
- **PiGPIO** : Contrôle avancé des GPIO

## Conclusion

Le développement ARM avec FreePascal/Lazarus ouvre la porte à un monde passionnant de systèmes embarqués et d'IoT. Que ce soit pour un Raspberry Pi exécutant Linux ou un microcontrôleur STM32, FreePascal offre les outils nécessaires pour créer des applications performantes et optimisées.

Les points clés à retenir :

1. **Compilation croisée** : Développez confortablement sur PC, exécutez sur ARM
2. **Optimisation mémoire** : Cruciale pour les systèmes embarqués
3. **Accès matériel** : FreePascal permet un contrôle direct du hardware
4. **Portabilité** : Un même code peut cibler différentes plateformes ARM

Avec ces bases, vous êtes prêt à explorer le monde du développement embarqué avec FreePascal !

⏭️ [Gestion unifiée de la configuration](/05-developpement-multiplateforme-approfondi/09-gestion-unifiee-configuration.md)
