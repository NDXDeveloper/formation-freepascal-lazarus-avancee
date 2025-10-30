🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.3 ARM Cortex-M et STM32

## Introduction aux microcontrôleurs ARM

### Qu'est-ce qu'ARM ?

ARM (Advanced RISC Machine) n'est pas un fabricant de puces, mais une entreprise qui **conçoit des architectures de processeurs** sous licence. Les fabricants comme STMicroelectronics, NXP, Nordic, etc. intègrent ces cœurs ARM dans leurs microcontrôleurs.

**Différence fondamentale avec AVR :**
- **AVR** : architecture 8 bits, propriétaire (Microchip/Atmel)
- **ARM** : architecture 32 bits, standard universel

### La famille ARM Cortex-M

ARM propose plusieurs familles de processeurs. Pour les microcontrôleurs, la famille **Cortex-M** est la plus utilisée :

| Modèle | Caractéristiques | Usage typique |
|--------|------------------|---------------|
| **Cortex-M0/M0+** | Simple, faible consommation, 32 bits | Capteurs IoT, wearables |
| **Cortex-M3** | Performance moyenne, DSP basique | Applications générales |
| **Cortex-M4** | DSP et FPU intégrés | Audio, traitement signal |
| **Cortex-M7** | Haute performance, double FPU | Graphiques, calculs intensifs |
| **Cortex-M33** | Sécurité matérielle (TrustZone) | IoT sécurisé, paiement |

**FPU (Floating Point Unit)** : unité de calcul en virgule flottante matérielle (beaucoup plus rapide qu'en logiciel)

**DSP (Digital Signal Processing)** : instructions spécialisées pour le traitement du signal

### Pourquoi choisir ARM plutôt qu'AVR ?

**Avantages ARM Cortex-M :**

1. **Architecture 32 bits** : calculs plus rapides, adressage jusqu'à 4 Go
2. **Performance supérieure** : jusqu'à 400+ MHz vs 16-20 MHz pour AVR
3. **Plus de mémoire** : jusqu'à plusieurs Mo de Flash et RAM
4. **Périphériques avancés** : DMA, USB, Ethernet, écrans TFT, etc.
5. **FPU matérielle** : calculs flottants très rapides (M4/M7)
6. **Prix compétitifs** : certains STM32 coûtent moins de 1€
7. **Standard industriel** : utilisé dans automobile, médical, aérospatial

**Quand rester sur AVR :**
- Projets très simples (clignotement LED, lecture capteur basique)
- Budget ultra-serré (< 0,50€)
- Consommation minimale absolue
- Vous connaissez déjà bien AVR

## STM32 : la famille ARM de STMicroelectronics

### Pourquoi STM32 ?

STMicroelectronics fabrique une vaste gamme de microcontrôleurs ARM appelés **STM32**. C'est l'une des familles les plus populaires au monde.

**Points forts STM32 :**
- **Gamme très large** : des modèles pour tous les besoins
- **Documentation excellente** : datasheets, reference manuals, exemples
- **Outils gratuits** : STM32CubeMX, STM32CubeIDE
- **Support communautaire** : forums actifs, nombreux tutoriels
- **Prix abordables** : de 0,80€ à quelques euros
- **Disponibilité** : distributeurs mondiaux (DigiKey, Mouser, etc.)

### Familles STM32 principales

STM32 est divisé en séries selon le cœur ARM et les performances :

#### STM32F0 (Cortex-M0)

**Caractéristiques :**
- 48 MHz maximum
- 16-256 Ko Flash
- 4-32 Ko RAM
- Prix très bas (~1€)

**Usage :** Remplacement AVR avec plus de puissance

**Exemple :** STM32F030F4 (20 broches, 16 Ko Flash, 4 Ko RAM)

#### STM32F1 (Cortex-M3)

**Caractéristiques :**
- 72 MHz
- 16-1024 Ko Flash
- 6-96 Ko RAM
- Très populaire

**Usage :** Applications générales, prototypage

**Exemple :** STM32F103C8T6 (célèbre "Blue Pill", 64 Ko Flash, 20 Ko RAM)

#### STM32F4 (Cortex-M4F)

**Caractéristiques :**
- 168-180 MHz
- 512 Ko - 2 Mo Flash
- 128-384 Ko RAM
- FPU simple précision
- DSP

**Usage :** Audio, traitement d'images, moteurs

**Exemple :** STM32F407VGT6 (1 Mo Flash, 192 Ko RAM, nombreux périphériques)

#### STM32F7 (Cortex-M7F)

**Caractéristiques :**
- 216 MHz
- 512 Ko - 2 Mo Flash
- 256-512 Ko RAM
- Double FPU (simple + double précision)
- Cache instruction/données

**Usage :** IHM graphiques, calculs complexes

#### STM32H7 (Cortex-M7F)

**Caractéristiques :**
- 480 MHz (le plus rapide)
- Jusqu'à 2 Mo Flash
- Jusqu'à 1 Mo RAM
- Ethernet, USB High Speed, écrans HD

**Usage :** Applications haut de gamme, edge computing

#### STM32L (Cortex-M0+/M3/M4)

**Caractéristiques :**
- Optimisés pour **ultra-basse consommation**
- Modes veille avancés (µA)
- Fonctionnement sur batterie pendant des années

**Usage :** IoT, capteurs sans fil, wearables

#### STM32G (Cortex-M4)

**Caractéristiques :**
- Famille récente, optimisée coût/performance
- Bon compromis pour applications industrielles

### Quelle carte pour débuter ?

#### Blue Pill (STM32F103C8T6)

**La carte la plus populaire pour débuter**

**Spécifications :**
- Cortex-M3 à 72 MHz
- 64 Ko Flash (parfois 128 Ko non documenté)
- 20 Ko RAM
- 37 GPIO
- USB, UART, SPI, I2C
- Prix : ~2-3€

**Avantages :**
- Très bon rapport qualité/prix
- Communauté énorme
- Nombreux exemples
- Compatible Arduino IDE (avec bootloader)

**Inconvénients :**
- Beaucoup de clones de qualité variable
- Pas de debugger intégré (nécessite ST-Link externe)

#### Black Pill (STM32F401/F411)

**Version améliorée de la Blue Pill**

**Spécifications :**
- Cortex-M4F à 84-100 MHz
- 256-512 Ko Flash
- 64-128 Ko RAM
- FPU
- USB Type-C

**Avantages :**
- Plus puissant que Blue Pill
- FPU pour calculs flottants
- USB-C moderne

#### Nucleo-64 (diverses variantes)

**Cartes officielles STMicroelectronics**

**Spécifications :**
- Format Arduino compatible
- ST-Link V2 intégré (programmation + debug)
- Différents modèles : F030, F103, F401, F411, L476, etc.
- Prix : ~10-15€

**Avantages :**
- Qualité professionnelle
- Debugger intégré
- Documentation officielle
- Support garanti

**Recommandation débutant :** Nucleo-F103RB ou Nucleo-F411RE

#### Discovery (diverses variantes)

**Cartes de développement avec périphériques**

**Exemples :**
- **STM32F4-Discovery** : écran LCD, accéléromètre, audio
- **STM32F7-Discovery** : écran tactile, caméra
- Prix : 15-30€

**Usage :** Projets complexes, démonstrations

## Architecture matérielle ARM Cortex-M

### Organisation de la mémoire

Contrairement à AVR avec ses ports mémoire séparés, ARM utilise un **espace d'adressage unifié** :

```
0x0000 0000 - 0x1FFF FFFF   Flash / Code
0x2000 0000 - 0x3FFF FFFF   SRAM (RAM)
0x4000 0000 - 0x5FFF FFFF   Périphériques
0xE000 0000 - 0xE00F FFFF   Système (NVIC, SysTick, etc.)
```

**Avantage :** Tous les périphériques sont accessibles comme de la mémoire normale.

### Registres et périphériques

Les périphériques sont contrôlés via des registres mappés en mémoire :

```pascal
// Exemple : activer une LED sur GPIOC pin 13
GPIOC_ODR := GPIOC_ODR or (1 shl 13);

// GPIOC_ODR est un registre à l'adresse 0x40011014
```

**Différence avec AVR :**
- **AVR** : registres dans l'espace I/O (instructions spéciales `IN`/`OUT`)
- **ARM** : registres mappés en mémoire (instructions load/store normales)

### NVIC (Nested Vectored Interrupt Controller)

Le contrôleur d'interruptions ARM est beaucoup plus sophistiqué qu'AVR :

**Caractéristiques :**
- Jusqu'à 240+ sources d'interruption
- **16 niveaux de priorité** (vs 1 seul sur AVR)
- **Interruptions imbriquées** : une interruption prioritaire peut en interrompre une autre
- Vecteurs d'interruption en table

**Exemple de priorités :**
```
Priorité 0 (la plus haute) : Interruption critique temps réel
Priorité 1 : Communication réseau importante
Priorité 5 : Timer périodique
Priorité 10 : Bouton utilisateur
Priorité 15 (la plus basse) : Tâche de fond
```

### SysTick Timer

Timer système intégré à tous les Cortex-M, utilisé pour :
- Générer une base de temps (tick)
- Implémenter un RTOS (Real-Time Operating System)
- Créer des délais précis

```pascal
// Configuration SysTick pour 1 ms
SysTick_LOAD := (72000000 div 1000) - 1;  // 72 MHz / 1000 Hz
SysTick_VAL := 0;
SysTick_CTRL := $07;  // Enable, interrupt, core clock
```

## Installation et configuration

### Prérequis Windows

1. **FreePascal** : version 3.2.0+ avec support ARM embedded
2. **ARM Toolchain** : `gcc-arm-none-eabi`
   - Télécharger depuis https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-rm
   - Ou via MSYS2 : `pacman -S mingw-w64-x86_64-arm-none-eabi-gcc`
3. **ST-Link Utility** : pour flasher les STM32
   - Télécharger depuis st.com
   - Ou utiliser `stlink-tools` open source
4. **Drivers ST-Link** : pour que Windows reconnaisse le programmeur

**Installation rapide (chocolatey) :**

```batch
choco install gcc-arm-embedded
choco install stlink
```

### Prérequis Ubuntu

Installation très simple via APT :

```bash
# FreePascal avec support ARM
sudo apt update
sudo apt install fpc fpc-source

# Toolchain ARM
sudo apt install gcc-arm-none-eabi binutils-arm-none-eabi

# Outils de flash
sudo apt install stlink-tools openocd

# Optionnel : STM32CubeMX pour configuration graphique
# Télécharger depuis st.com (fichier .deb ou .zip)
```

**Vérification de l'installation :**

```bash
fpc -i | grep arm              # Support ARM ?
arm-none-eabi-gcc --version    # Toolchain présente ?
st-info --probe                # ST-Link détecté ?
```

### Configuration du compilateur

**Créer un fichier fpc-stm32.cfg :**

```ini
# Configuration FreePascal pour STM32

# Processeur et cible
-Parm
-Tembedded
-Cparmv7m
-Wpstm32f103xb

# Sous-architecture Cortex-M3
-XParm-none-eabi-

# Optimisations
-O2
-CfSOFT

# Génération de code
-g
-gl

# Chemins
-Fu/usr/lib/fpc/$fpcversion/units/$fpctarget
-Fu/usr/lib/fpc/$fpcversion/units/$fpctarget/rtl
```

**Options importantes :**

- `-Parm` : Processeur ARM
- `-Tembedded` : Cible embedded (pas d'OS)
- `-Cparmv7m` : Architecture ARMv7-M (Cortex-M3/M4)
- `-Wpstm32f103xb` : Modèle spécifique (Blue Pill)

**Pour d'autres modèles :**
- STM32F030 : `-Wpstm32f030x6` (Cortex-M0)
- STM32F401 : `-Wpstm32f401xc` (Cortex-M4)
- STM32F407 : `-Wpstm32f407xx` (Cortex-M4)
- STM32F746 : `-Wpstm32f746xx` (Cortex-M7)

## Structure d'un programme STM32

### Programme minimal "Blink LED"

Sur une Blue Pill, la LED intégrée est sur PC13 (Port C, pin 13).

```pascal
program BlinkSTM32;

{$mode objfpc}
{$H+}

uses
  stm32f10x_rcc,   // Reset and Clock Control
  stm32f10x_gpio;  // General Purpose I/O

const
  LED_PORT = GPIOC;
  LED_PIN  = 13;

// Délai approximatif
procedure Delay_ms(ms: longword);
var
  i, j: longword;
begin
  for i := 0 to ms do
    for j := 0 to 7200 do  // Calibré pour 72 MHz
      asm nop; end;
end;

// Configuration du système
procedure SystemInit;
begin
  // Configuration horloge système à 72 MHz (fait automatiquement)
  // La fonction SystemInit() est appelée avant le main
end;

// Configuration GPIO
procedure GPIO_Setup;
var
  GPIO_InitStruct: TGPIO_InitTypeDef;
begin
  // Activer l'horloge du port GPIOC
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOC, ENABLE);

  // Configurer PC13 en sortie push-pull
  GPIO_InitStruct.GPIO_Pin := GPIO_Pin_13;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_Out_PP;
  GPIO_InitStruct.GPIO_Speed := GPIO_Speed_2MHz;
  GPIO_Init(GPIOC, @GPIO_InitStruct);
end;

begin
  GPIO_Setup;

  // Boucle infinie
  while true do
  begin
    // Allumer LED (logique inversée sur Blue Pill)
    GPIO_ResetBits(LED_PORT, GPIO_Pin_13);
    Delay_ms(1000);

    // Éteindre LED
    GPIO_SetBits(LED_PORT, GPIO_Pin_13);
    Delay_ms(1000);
  end;
end.
```

**Explications :**

```pascal
uses
  stm32f10x_rcc,   // Gestion des horloges
  stm32f10x_gpio;  // Contrôle des GPIO
```

FreePascal fournit des unités qui correspondent aux bibliothèques STM32 standard.

```pascal
RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOC, ENABLE);
```

**Important :** Sur STM32, les périphériques doivent être **activés explicitement** via leur horloge. Sinon, ils ne fonctionnent pas. C'est pour économiser l'énergie.

```pascal
GPIO_InitStruct.GPIO_Mode := GPIO_Mode_Out_PP;
```

**Modes GPIO disponibles :**
- `GPIO_Mode_Out_PP` : Sortie push-pull (standard)
- `GPIO_Mode_Out_OD` : Sortie open-drain (I2C, etc.)
- `GPIO_Mode_IN_FLOATING` : Entrée flottante
- `GPIO_Mode_IPU` : Entrée avec pull-up
- `GPIO_Mode_IPD` : Entrée avec pull-down
- `GPIO_Mode_AF_PP` : Fonction alternative push-pull (UART, SPI, etc.)

```pascal
GPIO_ResetBits(GPIOC, GPIO_Pin_13);  // Mettre à 0 (allumer sur Blue Pill)
GPIO_SetBits(GPIOC, GPIO_Pin_13);    // Mettre à 1 (éteindre)
```

**Note :** Sur Blue Pill, la LED est câblée en logique inversée (0 = allumé, 1 = éteint).

### Lecture d'un bouton

```pascal
program BoutonLED;

uses
  stm32f10x_rcc, stm32f10x_gpio;

procedure GPIO_Setup;
var
  GPIO_InitStruct: TGPIO_InitTypeDef;
begin
  // Activer horloges GPIOA et GPIOC
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOA or RCC_APB2Periph_GPIOC, ENABLE);

  // LED sur PC13 (sortie)
  GPIO_InitStruct.GPIO_Pin := GPIO_Pin_13;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_Out_PP;
  GPIO_InitStruct.GPIO_Speed := GPIO_Speed_2MHz;
  GPIO_Init(GPIOC, @GPIO_InitStruct);

  // Bouton sur PA0 (entrée avec pull-down)
  GPIO_InitStruct.GPIO_Pin := GPIO_Pin_0;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_IPD;
  GPIO_Init(GPIOA, @GPIO_InitStruct);
end;

procedure Delay_ms(ms: longword);
var
  i, j: longword;
begin
  for i := 0 to ms do
    for j := 0 to 7200 do
      asm nop; end;
end;

var
  buttonState: boolean;

begin
  GPIO_Setup;

  while true do
  begin
    // Lire l'état du bouton
    buttonState := GPIO_ReadInputDataBit(GPIOA, GPIO_Pin_0) = Bit_SET;

    // Contrôler la LED
    if buttonState then
      GPIO_ResetBits(GPIOC, GPIO_Pin_13)  // Allumer
    else
      GPIO_SetBits(GPIOC, GPIO_Pin_13);   // Éteindre

    Delay_ms(50);  // Anti-rebond
  end;
end.
```

## Entrées analogiques (ADC)

STM32 possède des ADC 12 bits (0-4095) beaucoup plus précis que les AVR 10 bits (0-1023).

### Configuration ADC

```pascal
program LectureADC;

uses
  stm32f10x_rcc, stm32f10x_gpio, stm32f10x_adc;

const
  ADC_CHANNEL = ADC_Channel_0;  // PA0

procedure ADC_Setup;
var
  GPIO_InitStruct: TGPIO_InitTypeDef;
  ADC_InitStruct: TADC_InitTypeDef;
begin
  // Activer horloges
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOA or RCC_APB2Periph_ADC1, ENABLE);

  // Configurer PA0 en entrée analogique
  GPIO_InitStruct.GPIO_Pin := GPIO_Pin_0;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_AIN;  // Analog Input
  GPIO_Init(GPIOA, @GPIO_InitStruct);

  // Configuration ADC
  ADC_InitStruct.ADC_Mode := ADC_Mode_Independent;
  ADC_InitStruct.ADC_ScanConvMode := DISABLE;
  ADC_InitStruct.ADC_ContinuousConvMode := DISABLE;
  ADC_InitStruct.ADC_ExternalTrigConv := ADC_ExternalTrigConv_None;
  ADC_InitStruct.ADC_DataAlign := ADC_DataAlign_Right;
  ADC_InitStruct.ADC_NbrOfChannel := 1;
  ADC_Init(ADC1, @ADC_InitStruct);

  // Activer ADC
  ADC_Cmd(ADC1, ENABLE);

  // Calibration ADC (nécessaire sur STM32F1)
  ADC_ResetCalibration(ADC1);
  while ADC_GetResetCalibrationStatus(ADC1) = SET do;
  ADC_StartCalibration(ADC1);
  while ADC_GetCalibrationStatus(ADC1) = SET do;
end;

function ADC_Read(channel: byte): word;
begin
  // Configurer le canal
  ADC_RegularChannelConfig(ADC1, channel, 1, ADC_SampleTime_55Cycles5);

  // Démarrer conversion
  ADC_SoftwareStartConvCmd(ADC1, ENABLE);

  // Attendre fin de conversion
  while ADC_GetFlagStatus(ADC1, ADC_FLAG_EOC) = RESET do;

  // Lire le résultat
  Result := ADC_GetConversionValue(ADC1);
end;

var
  valeur: word;

begin
  ADC_Setup;

  while true do
  begin
    valeur := ADC_Read(ADC_CHANNEL);

    // valeur entre 0 et 4095
    // 0 = 0V, 4095 = 3.3V (sur STM32)
    // Tension = (valeur * 3.3) / 4095

    // Utiliser la valeur...
  end;
end.
```

**Différences avec AVR :**
- **Résolution** : 12 bits (4096 valeurs) vs 10 bits AVR
- **Tension référence** : 3.3V sur STM32 vs 5V sur Arduino
- **Calibration** : nécessaire sur STM32F1 pour précision maximale
- **Temps échantillonnage** : configurable (1.5 à 239.5 cycles)

## Communication série (USART)

### Configuration USART

STM32 possède plusieurs USART (USART1, USART2, etc.). Sur Blue Pill :
- USART1 : PA9 (TX), PA10 (RX)
- USART2 : PA2 (TX), PA3 (RX)
- USART3 : PB10 (TX), PB11 (RX)

```pascal
program TestUSART;

uses
  stm32f10x_rcc, stm32f10x_gpio, stm32f10x_usart;

procedure USART_Setup;
var
  GPIO_InitStruct: TGPIO_InitTypeDef;
  USART_InitStruct: TUSART_InitTypeDef;
begin
  // Activer horloges
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOA or RCC_APB2Periph_USART1, ENABLE);

  // Configurer PA9 (TX) en sortie AF push-pull
  GPIO_InitStruct.GPIO_Pin := GPIO_Pin_9;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_AF_PP;
  GPIO_InitStruct.GPIO_Speed := GPIO_Speed_50MHz;
  GPIO_Init(GPIOA, @GPIO_InitStruct);

  // Configurer PA10 (RX) en entrée flottante
  GPIO_InitStruct.GPIO_Pin := GPIO_Pin_10;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_IN_FLOATING;
  GPIO_Init(GPIOA, @GPIO_InitStruct);

  // Configuration USART
  USART_InitStruct.USART_BaudRate := 115200;
  USART_InitStruct.USART_WordLength := USART_WordLength_8b;
  USART_InitStruct.USART_StopBits := USART_StopBits_1;
  USART_InitStruct.USART_Parity := USART_Parity_No;
  USART_InitStruct.USART_HardwareFlowControl := USART_HardwareFlowControl_None;
  USART_InitStruct.USART_Mode := USART_Mode_Rx or USART_Mode_Tx;
  USART_Init(USART1, @USART_InitStruct);

  // Activer USART
  USART_Cmd(USART1, ENABLE);
end;

procedure USART_SendChar(c: char);
begin
  while USART_GetFlagStatus(USART1, USART_FLAG_TXE) = RESET do;
  USART_SendData(USART1, Ord(c));
end;

procedure USART_SendString(const s: string);
var
  i: integer;
begin
  for i := 1 to Length(s) do
    USART_SendChar(s[i]);
end;

procedure USART_SendLn(const s: string);
begin
  USART_SendString(s);
  USART_SendChar(#13);
  USART_SendChar(#10);
end;

function USART_ReceiveChar: char;
begin
  while USART_GetFlagStatus(USART1, USART_FLAG_RXNE) = RESET do;
  Result := Chr(USART_ReceiveData(USART1));
end;

var
  compteur: longword = 0;

begin
  USART_Setup;

  USART_SendLn('STM32 FreePascal');
  USART_SendLn('Blue Pill Demo');

  while true do
  begin
    USART_SendString('Compteur: ');
    // Conversion nombre → chaîne à implémenter
    USART_SendLn('');

    Inc(compteur);

    // Délai (à améliorer avec timer)
    Delay_ms(1000);
  end;
end.
```

## Timers avancés

STM32 possède de nombreux timers beaucoup plus sophistiqués qu'AVR :

### Types de timers

| Timer | Type | Résolution | Fonctionnalités |
|-------|------|------------|-----------------|
| TIM1, TIM8 | Avancé | 16 bits | PWM complémentaire, dead-time |
| TIM2-TIM5 | Général | 32 bits (F1: 16 bits) | 4 canaux PWM/capture |
| TIM6, TIM7 | Basique | 16 bits | Interruption simple, DAC trigger |
| TIM9-TIM14 | Général | 16 bits | 1-2 canaux PWM |

### Timer en mode interruption

Créer une interruption toutes les secondes :

```pascal
program TimerInterrupt;

uses
  stm32f10x_rcc, stm32f10x_tim, stm32f10x_misc, stm32f10x_gpio;

var
  secondes: longword = 0;

// Gestionnaire d'interruption Timer 2
procedure TIM2_IRQHandler; public name 'TIM2_IRQHandler';
begin
  if TIM_GetITStatus(TIM2, TIM_IT_Update) <> RESET then
  begin
    TIM_ClearITPendingBit(TIM2, TIM_IT_Update);

    // Code exécuté toutes les secondes
    Inc(secondes);

    // Faire clignoter LED
    GPIO_ToggleBits(GPIOC, GPIO_Pin_13);
  end;
end;

procedure Timer_Setup;
var
  TIM_TimeBaseStruct: TTIM_TimeBaseInitTypeDef;
  NVIC_InitStruct: TNVIC_InitTypeDef;
begin
  // Activer horloge TIM2
  RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM2, ENABLE);

  // Configuration timer pour 1 Hz (1 seconde)
  // APB1 clock = 36 MHz
  // Prescaler = 36000 → 1 kHz
  // Period = 1000 → 1 Hz
  TIM_TimeBaseStruct.TIM_Period := 999;  // 1000 - 1
  TIM_TimeBaseStruct.TIM_Prescaler := 35999;  // 36000 - 1
  TIM_TimeBaseStruct.TIM_ClockDivision := TIM_CKD_DIV1;
  TIM_TimeBaseStruct.TIM_CounterMode := TIM_CounterMode_Up;
  TIM_TimeBaseInit(TIM2, @TIM_TimeBaseStruct);

  // Activer interruption
  TIM_ITConfig(TIM2, TIM_IT_Update, ENABLE);

  // Configurer NVIC (priorité interruption)
  NVIC_InitStruct.NVIC_IRQChannel := TIM2_IRQn;
  NVIC_InitStruct.NVIC_IRQChannelPreemptionPriority := 0;
  NVIC_InitStruct.NVIC_IRQChannelSubPriority := 1;
  NVIC_InitStruct.NVIC_IRQChannelCmd := ENABLE;
  NVIC_Init(@NVIC_InitStruct);

  // Démarrer le timer
  TIM_Cmd(TIM2, ENABLE);
end;

procedure GPIO_Setup;
var
  GPIO_InitStruct: TGPIO_InitTypeDef;
begin
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOC, ENABLE);

  GPIO_InitStruct.GPIO_Pin := GPIO_Pin_13;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_Out_PP;
  GPIO_InitStruct.GPIO_Speed := GPIO_Speed_2MHz;
  GPIO_Init(GPIOC, @GPIO_InitStruct);
end;

begin
  GPIO_Setup;
  Timer_Setup;

  // Boucle infinie - le timer gère le clignotement
  while true do
  begin
    // Ici, on peut faire autre chose
    // L'interruption s'occupe du clignotement
  end;
end.
```

**Explication du calcul de fréquence :**

```
Fréquence timer = Horloge / ((Prescaler + 1) × (Period + 1))

Pour 1 Hz avec horloge 36 MHz :
1 Hz = 36 000 000 / ((35999 + 1) × (999 + 1))
1 Hz = 36 000 000 / (36000 × 1000)
1 Hz = 36 000 000 / 36 000 000
1 Hz = 1 ✓
```

**Calcul inverse pour une fréquence donnée :**

```pascal
// Pour obtenir une fréquence de 10 kHz avec horloge 36 MHz
// Prescaler = 72 → horloge devient 500 kHz
// Period = 50 → 500 kHz / 50 = 10 kHz

TIM_TimeBaseStruct.TIM_Prescaler := 71;  // 72 - 1
TIM_TimeBaseStruct.TIM_Period := 49;     // 50 - 1
```

### PWM (Pulse Width Modulation)

Le PWM sur STM32 est très flexible et supporte jusqu'à 4 canaux par timer.

**Exemple : varier l'intensité d'une LED**

```pascal
program PWM_Demo;

uses
  stm32f10x_rcc, stm32f10x_gpio, stm32f10x_tim;

// LED sur PA1 (TIM2_CH2)
const
  PWM_PORT = GPIOA;
  PWM_PIN = GPIO_Pin_1;

procedure PWM_Setup;
var
  GPIO_InitStruct: TGPIO_InitTypeDef;
  TIM_TimeBaseStruct: TTIM_TimeBaseInitTypeDef;
  TIM_OCInitStruct: TTIM_OCInitTypeDef;
begin
  // Activer horloges
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOA, ENABLE);
  RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM2, ENABLE);

  // Configurer PA1 en fonction alternative (PWM)
  GPIO_InitStruct.GPIO_Pin := PWM_PIN;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_AF_PP;
  GPIO_InitStruct.GPIO_Speed := GPIO_Speed_50MHz;
  GPIO_Init(PWM_PORT, @GPIO_InitStruct);

  // Configuration timer base : fréquence PWM = 1 kHz
  TIM_TimeBaseStruct.TIM_Prescaler := 71;    // 72 MHz / 72 = 1 MHz
  TIM_TimeBaseStruct.TIM_Period := 999;      // 1 MHz / 1000 = 1 kHz
  TIM_TimeBaseStruct.TIM_ClockDivision := TIM_CKD_DIV1;
  TIM_TimeBaseStruct.TIM_CounterMode := TIM_CounterMode_Up;
  TIM_TimeBaseInit(TIM2, @TIM_TimeBaseStruct);

  // Configuration PWM canal 2
  TIM_OCInitStruct.TIM_OCMode := TIM_OCMode_PWM1;
  TIM_OCInitStruct.TIM_OutputState := TIM_OutputState_Enable;
  TIM_OCInitStruct.TIM_Pulse := 0;  // Rapport cyclique initial : 0%
  TIM_OCInitStruct.TIM_OCPolarity := TIM_OCPolarity_High;
  TIM_OC2Init(TIM2, @TIM_OCInitStruct);

  TIM_OC2PreloadConfig(TIM2, TIM_OCPreload_Enable);
  TIM_ARRPreloadConfig(TIM2, ENABLE);

  // Démarrer le timer
  TIM_Cmd(TIM2, ENABLE);
end;

// Définir le rapport cyclique (0-1000 = 0-100%)
procedure PWM_SetDutyCycle(value: word);
begin
  if value > 1000 then
    value := 1000;
  TIM_SetCompare2(TIM2, value);
end;

procedure Delay_ms(ms: longword);
var
  i, j: longword;
begin
  for i := 0 to ms do
    for j := 0 to 7200 do
      asm nop; end;
end;

var
  intensite: word;
  montee: boolean;

begin
  PWM_Setup;

  intensite := 0;
  montee := true;

  // Effet de respiration (fade in/out)
  while true do
  begin
    PWM_SetDutyCycle(intensite);

    if montee then
    begin
      intensite := intensite + 10;
      if intensite >= 1000 then
        montee := false;
    end
    else
    begin
      intensite := intensite - 10;
      if intensite <= 0 then
        montee := true;
    end;

    Delay_ms(20);
  end;
end.
```

**Broches PWM sur STM32F103 (Blue Pill) :**

| Timer | Canal | Broche | Pin Arduino équivalent |
|-------|-------|--------|------------------------|
| TIM2 | CH1 | PA0 | - |
| TIM2 | CH2 | PA1 | - |
| TIM2 | CH3 | PA2 | - |
| TIM2 | CH4 | PA3 | - |
| TIM3 | CH1 | PA6 | - |
| TIM3 | CH2 | PA7 | - |
| TIM3 | CH3 | PB0 | - |
| TIM3 | CH4 | PB1 | - |

## DMA (Direct Memory Access)

Le DMA est une fonctionnalité très puissante de STM32 qui permet de **transférer des données sans intervention du CPU**.

### Pourquoi utiliser le DMA ?

**Sans DMA :**
```
CPU lit ADC → CPU écrit en mémoire → CPU lit ADC → CPU écrit en mémoire...
(le CPU est occupé en permanence)
```

**Avec DMA :**
```
DMA copie automatiquement ADC → mémoire en arrière-plan
(le CPU peut faire autre chose)
```

**Applications typiques :**
- Acquisition ADC multi-canaux
- Transmission UART rapide
- Transfert SPI/I2C
- Génération de signaux PWM complexes
- Traitement audio/vidéo

### Exemple : ADC multi-canaux avec DMA

```pascal
program ADC_DMA;

uses
  stm32f10x_rcc, stm32f10x_gpio, stm32f10x_adc, stm32f10x_dma;

const
  NUM_CHANNELS = 4;

var
  ADC_Values: array[0..NUM_CHANNELS-1] of word;

procedure DMA_Setup;
var
  DMA_InitStruct: TDMA_InitTypeDef;
begin
  // Activer horloge DMA1
  RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);

  // Configuration DMA Canal 1 (lié à ADC1)
  DMA_DeInit(DMA1_Channel1);

  DMA_InitStruct.DMA_PeripheralBaseAddr := longword(@ADC1^.DR);
  DMA_InitStruct.DMA_MemoryBaseAddr := longword(@ADC_Values[0]);
  DMA_InitStruct.DMA_DIR := DMA_DIR_PeripheralSRC;  // Périphérique → Mémoire
  DMA_InitStruct.DMA_BufferSize := NUM_CHANNELS;
  DMA_InitStruct.DMA_PeripheralInc := DMA_PeripheralInc_Disable;
  DMA_InitStruct.DMA_MemoryInc := DMA_MemoryInc_Enable;
  DMA_InitStruct.DMA_PeripheralDataSize := DMA_PeripheralDataSize_HalfWord;
  DMA_InitStruct.DMA_MemoryDataSize := DMA_MemoryDataSize_HalfWord;
  DMA_InitStruct.DMA_Mode := DMA_Mode_Circular;  // Mode circulaire (répétition)
  DMA_InitStruct.DMA_Priority := DMA_Priority_High;
  DMA_InitStruct.DMA_M2M := DMA_M2M_Disable;
  DMA_Init(DMA1_Channel1, @DMA_InitStruct);

  // Activer DMA
  DMA_Cmd(DMA1_Channel1, ENABLE);
end;

procedure ADC_Setup;
var
  GPIO_InitStruct: TGPIO_InitTypeDef;
  ADC_InitStruct: TADC_InitTypeDef;
begin
  // Activer horloges
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOA or RCC_APB2Periph_ADC1, ENABLE);

  // Configurer PA0-PA3 en entrées analogiques
  GPIO_InitStruct.GPIO_Pin := GPIO_Pin_0 or GPIO_Pin_1 or GPIO_Pin_2 or GPIO_Pin_3;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_AIN;
  GPIO_Init(GPIOA, @GPIO_InitStruct);

  // Configuration ADC en mode scan avec DMA
  ADC_InitStruct.ADC_Mode := ADC_Mode_Independent;
  ADC_InitStruct.ADC_ScanConvMode := ENABLE;  // Scanner plusieurs canaux
  ADC_InitStruct.ADC_ContinuousConvMode := ENABLE;  // Conversion continue
  ADC_InitStruct.ADC_ExternalTrigConv := ADC_ExternalTrigConv_None;
  ADC_InitStruct.ADC_DataAlign := ADC_DataAlign_Right;
  ADC_InitStruct.ADC_NbrOfChannel := NUM_CHANNELS;
  ADC_Init(ADC1, @ADC_InitStruct);

  // Configurer les canaux (PA0-PA3 = ADC_Channel_0 à 3)
  ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 1, ADC_SampleTime_55Cycles5);
  ADC_RegularChannelConfig(ADC1, ADC_Channel_1, 2, ADC_SampleTime_55Cycles5);
  ADC_RegularChannelConfig(ADC1, ADC_Channel_2, 3, ADC_SampleTime_55Cycles5);
  ADC_RegularChannelConfig(ADC1, ADC_Channel_3, 4, ADC_SampleTime_55Cycles5);

  // Activer DMA pour ADC
  ADC_DMACmd(ADC1, ENABLE);

  // Activer ADC
  ADC_Cmd(ADC1, ENABLE);

  // Calibration
  ADC_ResetCalibration(ADC1);
  while ADC_GetResetCalibrationStatus(ADC1) = SET do;
  ADC_StartCalibration(ADC1);
  while ADC_GetCalibrationStatus(ADC1) = SET do;

  // Démarrer les conversions
  ADC_SoftwareStartConvCmd(ADC1, ENABLE);
end;

procedure Delay_ms(ms: longword);
var
  i, j: longword;
begin
  for i := 0 to ms do
    for j := 0 to 7200 do
      asm nop; end;
end;

var
  i: integer;

begin
  DMA_Setup;
  ADC_Setup;

  // Les valeurs ADC sont automatiquement mises à jour par DMA
  while true do
  begin
    // Utiliser les valeurs ADC_Values[0..3]
    // Elles sont constamment rafraîchies en arrière-plan

    // Exemple : afficher via UART (à implémenter)
    // for i := 0 to NUM_CHANNELS-1 do
    //   UART_PrintInt(ADC_Values[i]);

    Delay_ms(1000);
  end;
end.
```

**Avantages du DMA :**
- CPU libre pour autres tâches
- Transferts très rapides
- Latence réduite
- Consommation réduite (CPU peut dormir)

## SPI avec DMA

Exemple : communication avec un capteur/écran SPI haute vitesse.

```pascal
program SPI_DMA;

uses
  stm32f10x_rcc, stm32f10x_gpio, stm32f10x_spi, stm32f10x_dma;

const
  BUFFER_SIZE = 128;

var
  TX_Buffer: array[0..BUFFER_SIZE-1] of byte;
  RX_Buffer: array[0..BUFFER_SIZE-1] of byte;

procedure SPI_DMA_Setup;
var
  GPIO_InitStruct: TGPIO_InitTypeDef;
  SPI_InitStruct: TSPI_InitTypeDef;
  DMA_InitStruct: TDMA_InitTypeDef;
begin
  // Activer horloges
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOA or RCC_APB2Periph_SPI1, ENABLE);
  RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);

  // Configurer broches SPI1 : PA5 (SCK), PA6 (MISO), PA7 (MOSI)
  // SCK et MOSI en sortie AF
  GPIO_InitStruct.GPIO_Pin := GPIO_Pin_5 or GPIO_Pin_7;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_AF_PP;
  GPIO_InitStruct.GPIO_Speed := GPIO_Speed_50MHz;
  GPIO_Init(GPIOA, @GPIO_InitStruct);

  // MISO en entrée
  GPIO_InitStruct.GPIO_Pin := GPIO_Pin_6;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_IN_FLOATING;
  GPIO_Init(GPIOA, @GPIO_InitStruct);

  // NSS (CS) en sortie normale (PA4)
  GPIO_InitStruct.GPIO_Pin := GPIO_Pin_4;
  GPIO_InitStruct.GPIO_Mode := GPIO_Mode_Out_PP;
  GPIO_Init(GPIOA, @GPIO_InitStruct);
  GPIO_SetBits(GPIOA, GPIO_Pin_4);  // CS haut (désactivé)

  // Configuration SPI
  SPI_InitStruct.SPI_Direction := SPI_Direction_2Lines_FullDuplex;
  SPI_InitStruct.SPI_Mode := SPI_Mode_Master;
  SPI_InitStruct.SPI_DataSize := SPI_DataSize_8b;
  SPI_InitStruct.SPI_CPOL := SPI_CPOL_Low;
  SPI_InitStruct.SPI_CPHA := SPI_CPHA_1Edge;
  SPI_InitStruct.SPI_NSS := SPI_NSS_Soft;
  SPI_InitStruct.SPI_BaudRatePrescaler := SPI_BaudRatePrescaler_4;  // 72 MHz / 4 = 18 MHz
  SPI_InitStruct.SPI_FirstBit := SPI_FirstBit_MSB;
  SPI_Init(SPI1, @SPI_InitStruct);

  // Configuration DMA TX (SPI1_TX = DMA1 Channel 3)
  DMA_InitStruct.DMA_PeripheralBaseAddr := longword(@SPI1^.DR);
  DMA_InitStruct.DMA_MemoryBaseAddr := longword(@TX_Buffer[0]);
  DMA_InitStruct.DMA_DIR := DMA_DIR_PeripheralDST;  // Mémoire → Périphérique
  DMA_InitStruct.DMA_BufferSize := BUFFER_SIZE;
  DMA_InitStruct.DMA_PeripheralInc := DMA_PeripheralInc_Disable;
  DMA_InitStruct.DMA_MemoryInc := DMA_MemoryInc_Enable;
  DMA_InitStruct.DMA_PeripheralDataSize := DMA_PeripheralDataSize_Byte;
  DMA_InitStruct.DMA_MemoryDataSize := DMA_MemoryDataSize_Byte;
  DMA_InitStruct.DMA_Mode := DMA_Mode_Normal;
  DMA_InitStruct.DMA_Priority := DMA_Priority_High;
  DMA_InitStruct.DMA_M2M := DMA_M2M_Disable;
  DMA_Init(DMA1_Channel3, @DMA_InitStruct);

  // Activer SPI
  SPI_Cmd(SPI1, ENABLE);
end;

procedure SPI_TransmitDMA(data: PByte; size: word);
begin
  // Désactiver DMA
  DMA_Cmd(DMA1_Channel3, DISABLE);

  // Mettre à jour l'adresse et la taille
  DMA1_Channel3^.CMAR := longword(data);
  DMA1_Channel3^.CNDTR := size;

  // Activer DMA
  SPI_I2S_DMACmd(SPI1, SPI_I2S_DMAReq_Tx, ENABLE);
  DMA_Cmd(DMA1_Channel3, ENABLE);

  // Activer CS (PA4 = low)
  GPIO_ResetBits(GPIOA, GPIO_Pin_4);

  // Attendre fin du transfert
  while DMA_GetFlagStatus(DMA1_FLAG_TC3) = RESET do;
  DMA_ClearFlag(DMA1_FLAG_TC3);

  // Attendre que SPI soit vraiment terminé
  while SPI_I2S_GetFlagStatus(SPI1, SPI_I2S_FLAG_BSY) = SET do;

  // Désactiver CS
  GPIO_SetBits(GPIOA, GPIO_Pin_4);

  // Désactiver DMA
  SPI_I2S_DMACmd(SPI1, SPI_I2S_DMAReq_Tx, DISABLE);
end;

var
  i: integer;

begin
  SPI_DMA_Setup;

  // Remplir le buffer de test
  for i := 0 to BUFFER_SIZE-1 do
    TX_Buffer[i] := i;

  while true do
  begin
    // Envoyer le buffer via SPI avec DMA
    SPI_TransmitDMA(@TX_Buffer[0], BUFFER_SIZE);

    Delay_ms(100);
  end;
end.
```

## USB Device

STM32F103 peut fonctionner comme périphérique USB (clé USB, clavier, souris, port série virtuel, etc.).

### USB CDC (Communication Device Class) - Port série virtuel

Permet de créer un port série USB (comme un Arduino Uno avec CH340).

```pascal
// Configuration USB CDC nécessite plusieurs fichiers
// Utiliser la bibliothèque USB STM32 fournie par ST

// Structure de base :
program USB_CDC;

uses
  stm32f10x_rcc,
  stm32f10x_gpio,
  usb_init,     // Initialisation USB
  usb_core,     // Cœur USB
  usb_desc,     // Descripteurs
  usb_pwr;      // Gestion alimentation

procedure USB_Setup;
begin
  // Configuration horloge USB (48 MHz requis)
  RCC_USBCLKConfig(RCC_USBCLKSource_PLLCLK_1Div5);
  RCC_APB1PeriphClockCmd(RCC_APB1Periph_USB, ENABLE);

  // Configuration broches USB (PA11 = D-, PA12 = D+)
  // (Configuration automatique par la bibliothèque USB)

  // Initialisation stack USB
  USB_Init;
end;

// Callbacks pour envoyer/recevoir des données
procedure USB_CDC_Send(const data: array of byte; len: word);
begin
  // Envoyer via endpoint IN
  // (implémentation dépend de la bibliothèque)
end;

function USB_CDC_Receive(var data: array of byte): word;
begin
  // Recevoir depuis endpoint OUT
  // Retourner le nombre d'octets reçus
end;

begin
  USB_Setup;

  while true do
  begin
    // Echo : renvoyer ce qui est reçu
    // if USB_CDC_DataAvailable then
    //   USB_CDC_Send(buffer, len);
  end;
end.
```

**Note :** L'implémentation USB complète est complexe. Il est recommandé d'utiliser une bibliothèque existante comme :
- **libopencm3-pascal** : port Pascal de libopencm3
- **ChibiOS/RT** : RTOS avec support USB
- **STM32 USB Library** : bibliothèque officielle ST (à porter en Pascal)

## Compilation et flashage

### Compilation pour STM32

**Commande de base :**

```bash
fpc -Parm -Tembedded -Cparmv7m -Wpstm32f103xb -O2 monprogramme.pas
```

**Options importantes :**

| Option | Description |
|--------|-------------|
| `-Parm` | Architecture ARM |
| `-Tembedded` | Cible embarquée |
| `-Cparmv7m` | Cortex-M3/M4 (ARMv7-M) |
| `-Cparmv6m` | Cortex-M0/M0+ (ARMv6-M) |
| `-Wpstm32f103xb` | Modèle spécifique |
| `-O2` ou `-O3` | Optimisations |
| `-g` | Symboles de debug |
| `-al` | Générer listing assembleur |

**Pour différents modèles :**

```bash
# STM32F030 (Cortex-M0)
fpc -Parm -Tembedded -Cparmv6m -Wpstm32f030x6 -O2 prog.pas

# STM32F401 (Cortex-M4)
fpc -Parm -Tembedded -Cparmv7em -Wpstm32f401xc -O2 prog.pas

# STM32F407 (Cortex-M4 avec FPU)
fpc -Parm -Tembedded -Cparmv7em -Wpstm32f407xx -O2 -CfSOFT prog.pas

# STM32F746 (Cortex-M7 avec FPU double précision)
fpc -Parm -Tembedded -Cparmv7em -Wpstm32f746xx -O2 prog.pas
```

**Générer le fichier binaire :**

```bash
# Convertir ELF en BIN
arm-none-eabi-objcopy -O binary monprogramme.elf monprogramme.bin

# Ou en HEX
arm-none-eabi-objcopy -O ihex monprogramme.elf monprogramme.hex
```

### Flashage avec ST-Link

ST-Link est le programmeur/debugger standard pour STM32.

#### Utiliser st-flash (ligne de commande)

**Ubuntu :**
```bash
# Installer
sudo apt install stlink-tools

# Flasher
st-flash write monprogramme.bin 0x8000000

# Effacer
st-flash erase

# Informations
st-info --probe
```

**Windows :**
```batch
rem Télécharger st-flash depuis https://github.com/stlink-org/stlink

rem Flasher
st-flash.exe write monprogramme.bin 0x8000000
```

**Note :** `0x8000000` est l'adresse de départ de la Flash sur STM32 (sauf bootloader spécial).

#### Utiliser OpenOCD

OpenOCD supporte de nombreux debuggers (ST-Link, J-Link, etc.).

**Configuration OpenOCD (stm32f103.cfg) :**

```tcl
source [find interface/stlink.cfg]
source [find target/stm32f1x.cfg]

# Flasher
init
reset halt
flash write_image erase monprogramme.bin 0x08000000
reset run
shutdown
```

**Commande :**

```bash
# Ubuntu
openocd -f stm32f103.cfg

# Ou directement
openocd -f interface/stlink.cfg -f target/stm32f1x.cfg \
  -c "program monprogramme.bin 0x08000000 verify reset exit"
```

#### Utiliser STM32CubeProgrammer (GUI)

**Windows / Ubuntu avec interface graphique :**

1. Télécharger depuis st.com
2. Installer (disponible en .deb pour Ubuntu)
3. Lancer STM32CubeProgrammer
4. Connecter : ST-Link, sélectionner le fichier .bin/.hex
5. Cliquer "Download"

### Bootloader UART (sans ST-Link)

STM32F103 possède un bootloader ROM qui permet de programmer via UART.

**Prérequis :**
- Jumper BOOT0 = 1 (connecté à 3.3V)
- BOOT1 = 0 (GND)
- Reset

**Utiliser stm32flash :**

```bash
# Ubuntu
sudo apt install stm32flash

# Flasher via UART
stm32flash -w monprogramme.bin -v -g 0x0 /dev/ttyUSB0

# Windows (COM3)
stm32flash.exe -w monprogramme.bin -v -g 0x0 COM3
```

**Après flashage :**
- Remettre BOOT0 = 0
- Reset

### Script de build automatique

**Makefile pour STM32F103 :**

```makefile
PROGRAM = monprogramme
MCU = stm32f103xb
CPU = armv7m

FPC = fpc
OBJCOPY = arm-none-eabi-objcopy
STFLASH = st-flash
SIZE = arm-none-eabi-size

FPCFLAGS = -Parm -Tembedded -Cp$(CPU) -Wp$(MCU) -O2 -g

all: $(PROGRAM).bin

$(PROGRAM).elf: $(PROGRAM).pas
	@echo "===== Compilation ====="
	$(FPC) $(FPCFLAGS) $(PROGRAM).pas

$(PROGRAM).bin: $(PROGRAM).elf
	@echo "===== Conversion BIN ====="
	$(OBJCOPY) -O binary $(PROGRAM).elf $(PROGRAM).bin
	@echo "===== Taille ====="
	$(SIZE) $(PROGRAM).elf

$(PROGRAM).hex: $(PROGRAM).elf
	$(OBJCOPY) -O ihex $(PROGRAM).elf $(PROGRAM).hex

flash: $(PROGRAM).bin
	@echo "===== Flashage ====="
	$(STFLASH) write $(PROGRAM).bin 0x8000000

erase:
	@echo "===== Effacement ====="
	$(STFLASH) erase

reset:
	@echo "===== Reset ====="
	$(STFLASH) reset

info:
	@echo "===== Informations ST-Link ====="
	st-info --probe

clean:
	@echo "===== Nettoyage ====="
	rm -f $(PROGRAM).elf $(PROGRAM).bin $(PROGRAM).hex
	rm -f *.o *.ppu link.res

.PHONY: all flash erase reset info clean
```

**Utilisation :**

```bash
make              # Compiler et générer .bin
make flash        # Flasher sur STM32
make clean        # Nettoyer les fichiers
make info         # Voir infos ST-Link
make erase        # Effacer la Flash
```

### Script build_flash.sh pour Ubuntu

```bash
#!/bin/bash

PROGRAM="monprogramme"
MCU="stm32f103xb"
CPU="armv7m"

echo "===== Compilation ====="
fpc -Parm -Tembedded -Cp$CPU -Wp$MCU -O2 -g $PROGRAM.pas

if [ $? -ne 0 ]; then
    echo "ERREUR: Compilation échouée"
    exit 1
fi

echo ""
echo "===== Conversion BIN ====="
arm-none-eabi-objcopy -O binary $PROGRAM.elf $PROGRAM.bin

echo ""
echo "===== Taille du programme ====="
arm-none-eabi-size $PROGRAM.elf

echo ""
echo "===== Flashage ====="
st-flash write $PROGRAM.bin 0x8000000

if [ $? -ne 0 ]; then
    echo "ERREUR: Flashage échoué"
    exit 1
fi

echo ""
echo "===== Terminé ! ====="
```

**Rendre exécutable :**

```bash
chmod +x build_flash.sh
./build_flash.sh
```

## Debugging avec GDB et OpenOCD

### Configuration OpenOCD

**Fichier openocd.cfg :**

```tcl
# Interface ST-Link V2/V3
source [find interface/stlink.cfg]

# Cible STM32F103
source [find target/stm32f1x.cfg]

# Vitesse JTAG/SWD
adapter speed 1000

# Configuration debugging
init
reset init
```

### Lancer OpenOCD (Terminal 1)

```bash
# Ubuntu
openocd -f openocd.cfg

# Windows
openocd.exe -f openocd.cfg
```

**Sortie attendue :**

```
Open On-Chip Debugger 0.11.0
Info : Listening on port 6666 for tcl connections
Info : Listening on port 4444 for telnet connections
Info : clock speed 1000 kHz
Info : STLINK V2J29S7 (API v2) VID:PID 0483:3748
Info : Target voltage: 3.245V
Info : stm32f1x.cpu: hardware has 6 breakpoints, 4 watchpoints
Info : starting gdb server for stm32f1x.cpu on 3333
Info : Listening on port 3333 for gdb connections
```

### Debugging avec GDB (Terminal 2)

```bash
arm-none-eabi-gdb monprogramme.elf
```

**Dans GDB :**

```gdb
# Connexion à OpenOCD
(gdb) target extended-remote localhost:3333

# Charger le programme
(gdb) load

# Afficher le code source
(gdb) list

# Point d'arrêt au début
(gdb) break main

# Point d'arrêt dans une fonction
(gdb) break GPIO_Setup

# Lancer l'exécution
(gdb) continue

# Exécution pas à pas
(gdb) step      # Entre dans les fonctions
(gdb) next      # Passe à la ligne suivante

# Afficher une variable
(gdb) print compteur
(gdb) print/x GPIOC->ODR  # En hexadécimal

# Afficher un registre
(gdb) info registers
(gdb) print $r0

# Examiner la mémoire
(gdb) x/10x 0x40011000  # GPIOC base

# Modifier une variable
(gdb) set variable compteur = 42

# Continuer l'exécution
(gdb) continue

# Reset et relance
(gdb) monitor reset halt

# Quitter
(gdb) quit
```

### Script GDB automatique

**debug.gdb :**

```gdb
# Connexion
target extended-remote localhost:3333

# Charger le programme
load

# Points d'arrêt
break main
break TIM2_IRQHandler

# Commandes automatiques au break
commands 1
  print secondes
  continue
end

# Lancer
continue
```

**Utilisation :**

```bash
arm-none-eabi-gdb -x debug.gdb monprogramme.elf
```

## Bibliothèques et frameworks

### libopencm3

Bibliothèque C open source pour STM32, utilisable avec FreePascal via bindings.

**Installation :**

```bash
# Cloner libopencm3
git clone https://github.com/libopencm3/libopencm3.git
cd libopencm3
make

# Les bibliothèques sont dans lib/
```

**Utilisation avec FreePascal :** Nécessite de créer des bindings Pascal ou utiliser C interop.

### ChibiOS/RT

RTOS (Real-Time Operating System) complet avec multitâche préemptif.

**Caractéristiques :**
- Multitâche avec priorités
- Synchronisation (mutex, sémaphores)
- Pilotes matériels (GPIO, UART, SPI, I2C, USB, etc.)
- Gestion mémoire
- Shell et ligne de commande

**Note :** Principalement en C, mais peut être interfacé avec FreePascal.

### FreeRTOS

Autre RTOS très populaire, plus léger que ChibiOS.

**Utilisations typiques :**
- Gérer plusieurs tâches simultanées
- Communication inter-tâches
- Gestion événements temps réel

### Bibliothèques Pascal natives

**Embedded Pascal RTL :**
- Unités de base pour GPIO, UART, SPI, I2C
- Intégration avec FPC standard
- Support multi-plateformes (AVR, ARM, MIPS)

**Arduino-STM32 en Pascal :**
- Port Pascal des fonctions Arduino
- `digitalWrite()`, `analogRead()`, etc.
- Simplifie le développement

## Projets d'exemple complets

### Projet 1 : Station météo avec affichage OLED

**Matériel nécessaire :**
- Blue Pill (STM32F103C8T6)
- Capteur DHT22 (température + humidité)
- Écran OLED 128x64 I2C (SSD1306)

**Structure du projet :**

```
meteo/
├── meteo.pas          # Programme principal
├── unit_dht22.pas     # Pilote DHT22
├── unit_ssd1306.pas   # Pilote OLED
├── unit_i2c.pas       # Communication I2C
├── Makefile
└── openocd.cfg
```

**meteo.pas (simplifié) :**

```pascal
program StationMeteo;

uses
  stm32f10x_rcc, stm32f10x_gpio,
  unit_i2c, unit_ssd1306, unit_dht22;

var
  temperature: real;
  humidite: real;

procedure Delay_ms(ms: longword);
var
  i, j: longword;
begin
  for i := 0 to ms do
    for j := 0 to 7200 do
      asm nop; end;
end;

procedure Setup;
begin
  // Initialiser I2C pour OLED
  I2C_Init;

  // Initialiser écran OLED
  SSD1306_Init;
  SSD1306_Clear;
  SSD1306_DrawString(0, 0, 'Station Meteo', 1);
  SSD1306_Display;

  // Initialiser DHT22
  DHT22_Init;

  Delay_ms(2000);
end;

procedure AfficherDonnees;
var
  ligne: string;
begin
  SSD1306_Clear;

  // Titre
  SSD1306_DrawString(0, 0, 'Station Meteo', 1);

  // Température
  Str(temperature:0:1, ligne);
  SSD1306_DrawString(0, 16, 'Temp: ' + ligne + ' C', 1);

  // Humidité
  Str(humidite:0:1, ligne);
  SSD1306_DrawString(0, 32, 'Hum : ' + ligne + ' %', 1);

  SSD1306_Display;
end;

begin
  Setup;

  while true do
  begin
    // Lire le capteur
    if DHT22_Read(temperature, humidite) then
    begin
      AfficherDonnees;
    end
    else
    begin
      SSD1306_Clear;
      SSD1306_DrawString(0, 16, 'Erreur lecture', 1);
      SSD1306_Display;
    end;

    // Attendre 2 secondes
    Delay_ms(2000);
  end;
end.
```

### Projet 2 : Datalogger avec carte SD

**Matériel :**
- STM32F103 Blue Pill
- Module carte SD SPI
- Capteur température DS18B20 (OneWire)
- RTC DS3231 (I2C) pour horodatage

**Fonctionnalités :**
- Logger température toutes les 10 secondes
- Sauvegarder sur carte SD en CSV
- Horodatage avec RTC
- LED indiquant l'activité

**Structure fichier CSV :**

```csv
Date,Heure,Temperature
2025-10-04,14:30:00,23.5
2025-10-04,14:30:10,23.6
2025-10-04,14:30:20,23.4
```

### Projet 3 : Contrôleur de moteur pas-à-pas

**Matériel :**
- STM32F401 Black Pill (84 MHz, FPU)
- Driver moteur A4988 ou TMC2208
- Moteur pas-à-pas NEMA 17
- Interface UART pour commandes

**Fonctionnalités :**
- Contrôle vitesse et position
- Accélération/décélération progressive
- Commandes via port série
- Retour position en temps réel

**Exemple de contrôle :**

```pascal
program MoteurPasAPas;

uses
  stm32f4xx_rcc, stm32f4xx_gpio, stm32f4xx_tim;

const
  PIN_STEP = GPIO_Pin_0;  // PA0
  PIN_DIR  = GPIO_Pin_1;  // PA1
  PIN_EN   = GPIO_Pin_2;  // PA2

var
  position: longint = 0;
  vitesse: word = 1000;  // Hz

// Générer des pulses step via timer
procedure Timer_GeneratePulse;
begin
  // Toggle pin STEP
  GPIO_ToggleBits(GPIOA, PIN_STEP);
end;

procedure Avancer(pas: longint);
begin
  GPIO_ResetBits(GPIOA, PIN_DIR);  // Direction forward
  // Générer "pas" pulses
  // ... implémentation avec timer
  position := position + pas;
end;

procedure Reculer(pas: longint);
begin
  GPIO_SetBits(GPIOA, PIN_DIR);  // Direction backward
  // Générer "pas" pulses
  position := position - pas;
end;

procedure ReglerVitesse(nouvelleVitesse: word);
begin
  vitesse := nouvelleVitesse;
  // Ajuster fréquence timer
end;

begin
  // Initialisation...

  while true do
  begin
    // Traiter commandes UART
    // Contrôler le moteur
  end;
end.
```

## Optimisation et bonnes pratiques

### Optimisation mémoire

**1. Utiliser les sections mémoire appropriées**

```pascal
// Variables en RAM rapide (CCM sur F4/F7)
var
  buffer_rapide: array[0..255] of byte; section '.ccmram';

// Constantes en Flash
const
  TableSinus: array[0..359] of word = (...);  // Automatiquement en Flash

// Variables non initialisées (économise Flash)
var
  gros_buffer: array[0..1023] of byte; section '.bss';
```

**2. Éviter l'allocation dynamique excessive**

```pascal
// À ÉVITER
var
  liste: TList;
begin
  liste := TList.Create;  // Allocation dynamique
  // ...
  liste.Free;
end;

// PRÉFÉRER
var
  tableau: array[0..99] of TRecord;  // Taille fixe
  compteur: integer;
begin
  // Utiliser tableau et compteur
end;
```

**3. Utiliser des pools mémoire**

```pascal
type
  TMemoryPool = object
    buffer: array[0..1023] of byte;
    index: word;
    procedure Init;
    function Alloc(size: word): pointer;
  end;

var
  pool: TMemoryPool;

procedure TMemoryPool.Init;
begin
  index := 0;
end;

function TMemoryPool.Alloc(size: word): pointer;
begin
  if index + size <= 1024 then
  begin
    Result := @buffer[index];
    Inc(index, size);
  end
  else
    Result := nil;
end;
```

### Optimisation performance

**1. Utiliser le cache (F7/H7)**

```pascal
// Activer cache instruction et données
SCB_EnableICache;
SCB_EnableDCache;
```

**2. Optimiser les boucles critiques**

```pascal
// Moins optimisé
for i := 0 to 999 do
  tableau[i] := CalculComplexe(i);

// Plus optimisé
p := @tableau[0];
for i := 0 to 999 do
begin
  p^ := CalculComplexe(i);
  Inc(p);
end;

// Encore mieux avec assembleur inline si besoin
asm
  // Code ARM optimisé
end;
```

**3. Utiliser le FPU sur M4/M7**

```pascal
// S'assurer que FPU est activé
// Les calculs flottants utilisent FPU matérielle

var
  a, b, c: single;  // Précision simple = FPU

begin
  c := a * b + Sin(a);  // Calculs rapides avec FPU
end;
```

**4. Vectoriser avec SIMD (M7)**

Cortex-M7 supporte des instructions SIMD pour traiter plusieurs données simultanément.

```pascal
// Exemple conceptuel (nécessite assembleur)
// Ajouter 4 bytes simultanément
asm
  ldr r0, =source1
  ldr r1, =source2
  ldr r2, =destination

  ldr r3, [r0]
  ldr r4, [r1]
  sadd8 r5, r3, r4  // Addition SIMD 4x8 bits
  str r5, [r2]
end;
```

### Gestion de l'énergie

**1. Modes basse consommation**

```pascal
// Mode Sleep : arrêt CPU, périphériques actifs
procedure EnterSleepMode;
begin
  __WFI;  // Wait For Interrupt
end;

// Mode Stop : arrêt horloge système
procedure EnterStopMode;
begin
  PWR_EnterSTOPMode(PWR_Regulator_LowPower, PWR_STOPEntry_WFI);
end;

// Mode Standby : tout éteint sauf RTC
procedure EnterStandbyMode;
begin
  PWR_EnterSTANDBYMode;
end;
```

**2. Désactiver périphériques inutilisés**

```pascal
// Désactiver horloge des périphériques non utilisés
RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, DISABLE);
RCC_APB2PeriphClockCmd(RCC_APB2Periph_SPI1, DISABLE);
```

**3. Réduire fréquence horloge**

```pascal
// Passer à 8 MHz au lieu de 72 MHz
SystemCoreClockUpdate;
RCC_HCLKConfig(RCC_SYSCLK_Div8);
```

## Comparaison AVR vs ARM Cortex-M

### Tableau comparatif

| Aspect | AVR (ATmega328P) | ARM Cortex-M3 (STM32F103) | ARM Cortex-M4 (STM32F401) |
|--------|------------------|---------------------------|---------------------------|
| **Architecture** | 8 bits | 32 bits | 32 bits |
| **Fréquence** | 16-20 MHz | 72 MHz | 84 MHz |
| **Flash** | 32 Ko | 64-512 Ko | 256-512 Ko |
| **RAM** | 2 Ko | 20 Ko | 64-96 Ko |
| **FPU** | Non | Non | Oui (simple précision) |
| **DMA** | Non | 7 canaux | 16 streams |
| **Timers** | 3 (8/16 bits) | 11 (16/32 bits) | 14 (16/32 bits) |
| **UART** | 1 | 3 | 6 |
| **USB** | Non (via externe) | USB 2.0 FS | USB 2.0 FS/OTG |
| **Prix** | ~2€ | ~2-3€ | ~3-4€ |
| **Consommation** | Très faible | Faible | Moyenne |
| **Complexité** | Simple | Moyenne | Moyenne-élevée |

### Quand utiliser ARM plutôt qu'AVR ?

**Choisir ARM Cortex-M si :**
- ✅ Besoin de calculs flottants (FPU sur M4/M7)
- ✅ Multiples périphériques simultanés (DMA)
- ✅ Interface USB native
- ✅ Réseau (Ethernet sur certains modèles)
- ✅ Écrans graphiques (mémoire suffisante)
- ✅ Traitement signal (DSP sur M4/M7)
- ✅ Plus de 2 Ko RAM nécessaires
- ✅ Plusieurs ports série simultanés
- ✅ Évolutivité future du projet

**Rester sur AVR si :**
- ✅ Projet très simple (LED, capteur basique)
- ✅ Ultra-basse consommation critique
- ✅ Budget très serré (< 1€)
- ✅ Vous connaissez déjà bien AVR
- ✅ Compatibilité Arduino shields
- ✅ Pas besoin de 32 bits

### Migration AVR → ARM

**Changements principaux :**

| AVR | ARM Cortex-M |
|-----|--------------|
| `PORTB := $FF;` | `GPIO_SetBits(GPIOB, 0xFFFF);` |
| `DDRB := $FF;` | `GPIO_Init(...)` avec config |
| Registres direct | Périphériques mappés |
| 1 niveau interruption | 16 niveaux priorité |
| `avr_delay_ms()` | `Delay_ms()` custom ou SysTick |
| UART simple | USART configurables |
| PWM 8 bits | PWM 16 bits |

## Ressources et documentation

### Documentation officielle

**STMicroelectronics :**
- **Datasheets** : spécifications électriques de chaque modèle
- **Reference Manuals** : description complète des périphériques (1000+ pages)
- **Programming Manuals** : instructions ARM, registres système
- **Application Notes** : guides pratiques (USB, DMA, low-power, etc.)
- **Errata sheets** : bugs connus et workarounds

**ARM :**
- **ARM Cortex-M Technical Reference Manuals**
- **ARMv7-M Architecture Reference Manual**

### Outils recommandés

**IDE et éditeurs :**
- **Lazarus** : avec configuration embedded
- **VS Code** : avec extensions Pascal + Cortex-Debug
- **STM32CubeIDE** : IDE officiel ST (basé Eclipse, C/C++)
- **Geany** : léger et rapide

**Utilitaires :**
- **STM32CubeMX** : configuration graphique broches/horloges
- **STM32CubeProgrammer** : flashage et debug
- **ST-Link Utility** : programme Windows pour ST-Link

### Bibliothèques Pascal

**Embedded FreePascal RTL :**
- https://github.com/fpc/FPCSource/tree/main/rtl/embedded
- Unités standard pour STM32

**STM32-Pascal :**
- Bindings Pascal pour bibliothèques ST
- GPIO, UART, SPI, I2C, ADC, Timer

**libopencm3-pascal** (expérimental) :
- Port Pascal de libopencm3

### Communauté et forums

**Forums FreePascal :**
- https://forum.lazarus.freepascal.org
- Section "Embedded Systems"

**Forums STM32 :**
- https://community.st.com
- Très actif, réponses rapides

**Reddit :**
- r/freepascal
- r/stm32
- r/embedded

### Livres et tutoriels

**Livres ARM recommandés :**
- "The Definitive Guide to ARM Cortex-M3/M4" - Joseph Yiu
- "Mastering STM32" - Carmine Noviello (C, mais concepts applicables)

**Tutoriels vidéo :**
- YouTube : "STM32 tutorial" (adapter de C vers Pascal)
- Chaînes recommandées : Phil's Lab, Controllers Tech

### Projets open source

**Exemples STM32 Pascal :**
- GitHub : chercher "freepascal stm32" ou "lazarus embedded stm32"
- Projets communautaires avec code source complet

## Conclusion

Les microcontrôleurs ARM Cortex-M, notamment la famille STM32, représentent une **évolution majeure** par rapport aux AVR 8 bits :

**Points clés à retenir :**

1. **Architecture 32 bits** : calculs plus rapides, plus de mémoire adressable
2. **Performances** : 72-480 MHz vs 16-20 MHz sur AVR
3. **Périphériques avancés** : DMA, USB, Ethernet, multi-UART/SPI/I2C
4. **FPU matérielle** : calculs flottants très rapides sur M4/M7
5. **Écosystème riche** : documentation, outils, communauté
6. **Prix compétitif** : STM32F103 ~2€, équivalent Arduino Uno

**FreePascal sur ARM Cortex-M offre :**
- ✅ Syntaxe Pascal claire et structurée
- ✅ Support natif multiplateforme
- ✅ Performances excellentes (code compilé)
- ✅ Mêmes concepts du desktop à l'embarqué
- ⚠️ Écosystème moins mature que C/C++
- ⚠️ Bibliothèques moins nombreuses
- ⚠️ Configuration initiale plus complexe

**Cas d'usage idéaux :**
- Projets nécessitant calculs complexes
- Interfaces graphiques (écrans TFT)
- Traitement audio/signal
- Communication réseau
- Acquisition données multi-canaux
- Migration code desktop vers embarqué

**Prochaines étapes :**
- Section 14.4 : Raspberry Pi et Linux embarqué
- Section 14.5 : Protocoles IoT (MQTT, CoAP, LoRaWAN)
- Section 14.6 : Communication série avancée
- Section 14.7 : GPIO et interfaces matérielles

**Bon développement avec FreePascal sur STM32 ! 🚀**

---

*Ce tutoriel fait partie de la formation "FreePascal/Lazarus - Niveau Développeur Avancé - Edition Multi-plateforme Windows/Ubuntu"*

⏭️ [Raspberry Pi et Linux embarqué](/14-systemes-embarques-iot/04-raspberry-pi-linux-embarque.md)
