🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.9 Drivers et accès matériel direct

## Introduction

Les **drivers** (pilotes) sont des programmes qui permettent au système d'exploitation et aux applications de communiquer avec le matériel (hardware). L'**accès matériel direct** consiste à manipuler directement les composants électroniques via des registres, des ports d'entrée/sortie, ou de la mémoire mappée.

Cette section explore comment FreePascal peut interagir avec le matériel, depuis les microcontrôleurs jusqu'aux PC modernes, sur Windows et Linux.

## Architecture matérielle : les bases

### Qu'est-ce qu'un registre matériel ?

Un **registre** est une petite zone mémoire intégrée directement dans un périphérique matériel. En écrivant ou lisant des valeurs spécifiques, on contrôle le comportement du composant.

```
Exemple : registre GPIO d'un microcontrôleur  
Adresse : 0x40020000

Bit 0 : État de la broche PA0 (0=bas, 1=haut)  
Bit 1 : État de la broche PA1
...
Bit 15 : État de la broche PA15

Pour allumer une LED sur PA5, on écrit 1 au bit 5 :  
Registre[0x40020000] := Registre[0x40020000] OR (1 shl 5);
```

### Memory-Mapped I/O (MMIO)

Les registres matériels sont souvent **mappés en mémoire** : ils apparaissent comme des adresses mémoire normales.

```pascal
// Exemple conceptuel
const
  GPIO_BASE = $40020000;  // Adresse de base du GPIO

type
  PGPIO_Registers = ^TGPIO_Registers;
  TGPIO_Registers = record
    MODER   : LongWord;  // Mode (entrée/sortie)
    OTYPER  : LongWord;  // Type de sortie
    OSPEEDR : LongWord;  // Vitesse
    PUPDR   : LongWord;  // Pull-up/pull-down
    IDR     : LongWord;  // Données en entrée
    ODR     : LongWord;  // Données en sortie
    BSRR    : LongWord;  // Set/reset atomique
    // ...
  end;

var
  GPIOA: PGPIO_Registers = PGPIO_Registers(GPIO_BASE);

// Allumer LED sur PA5
GPIOA^.BSRR := (1 shl 5);  // Set bit 5

// Éteindre LED sur PA5
GPIOA^.BSRR := (1 shl (5 + 16));  // Reset bit 5
```

### Port I/O (x86/x64)

Sur PC x86/x64, certains périphériques utilisent des **ports I/O** séparés de la mémoire.

```pascal
// Instructions spéciales IN/OUT
function InPortB(Port: Word): Byte; assembler;  
asm
  mov dx, Port
  in  al, dx
end;

procedure OutPortB(Port: Word; Value: Byte); assembler;  
asm
  mov dx, Port
  mov al, Value
  out dx, al
end;

// Exemple : lecture d'un port série
var
  Donnee: Byte;
begin
  Donnee := InPortB($3F8);  // Port COM1 base
end;
```

## Accès matériel sur microcontrôleurs

### Environnement bare-metal

Sur microcontrôleur (ARM, AVR, PIC...), votre programme FreePascal s'exécute **directement sur le silicium**, sans système d'exploitation.

```pascal
{$MODE OBJFPC}
program BareMetalLED;

// Configuration pour STM32F103 (ARM Cortex-M3)
const
  // Adresses des registres RCC (Reset and Clock Control)
  RCC_BASE   = $40021000;
  RCC_APB2ENR = RCC_BASE + $18;

  // Adresses des registres GPIOC
  GPIOC_BASE = $40011000;
  GPIOC_CRH  = GPIOC_BASE + $04;
  GPIOC_ODR  = GPIOC_BASE + $0C;

  // Configuration
  LED_PIN = 13;  // PC13 sur Blue Pill

type
  PLongWord = ^LongWord;

var
  RCC_APB2ENR_Ptr: PLongWord = PLongWord(RCC_APB2ENR);
  GPIOC_CRH_Ptr: PLongWord = PLongWord(GPIOC_CRH);
  GPIOC_ODR_Ptr: PLongWord = PLongWord(GPIOC_ODR);

procedure InitGPIO;  
begin
  // Activer l'horloge du GPIOC
  RCC_APB2ENR_Ptr^ := RCC_APB2ENR_Ptr^ or (1 shl 4);

  // Configurer PC13 en sortie push-pull 2MHz
  GPIOC_CRH_Ptr^ := (GPIOC_CRH_Ptr^ and (not ($F shl 20))) or ($2 shl 20);
end;

procedure LED_On;  
begin
  GPIOC_ODR_Ptr^ := GPIOC_ODR_Ptr^ or (1 shl LED_PIN);
end;

procedure LED_Off;  
begin
  GPIOC_ODR_Ptr^ := GPIOC_ODR_Ptr^ and (not (1 shl LED_PIN));
end;

procedure Delay(Count: LongWord);  
var
  i: LongWord;
begin
  for i := 0 to Count do
    asm nop end;
end;

begin
  InitGPIO;

  // Boucle principale : clignotement LED
  while True do
  begin
    LED_On;
    Delay(1000000);
    LED_Off;
    Delay(1000000);
  end;
end.
```

### GPIO (General Purpose Input/Output)

Les **GPIO** sont des broches numériques configurables en entrée ou sortie.

```pascal
// Driver GPIO générique conceptuel
type
  TGPIOMode = (gmInput, gmOutput, gmAlternate, gmAnalog);
  TGPIOPull = (gpNone, gpPullUp, gpPullDown);
  TGPIOSpeed = (gsLow, gsMedium, gsHigh, gsVeryHigh);

procedure GPIO_Config(Pin: Byte; Mode: TGPIOMode;
                      Pull: TGPIOPull; Speed: TGPIOSpeed);
var
  RegValue: LongWord;
  Offset: Byte;
begin
  // Calculer position du bit dans le registre
  Offset := (Pin mod 16) * 2;

  // Configurer le mode
  RegValue := GPIOA^.MODER;
  RegValue := RegValue and (not ($3 shl Offset));
  RegValue := RegValue or (Ord(Mode) shl Offset);
  GPIOA^.MODER := RegValue;

  // Configurer pull-up/down
  Offset := (Pin mod 16) * 2;
  RegValue := GPIOA^.PUPDR;
  RegValue := RegValue and (not ($3 shl Offset));
  RegValue := RegValue or (Ord(Pull) shl Offset);
  GPIOA^.PUPDR := RegValue;

  // Etc...
end;

// Utilisation
procedure Main;  
begin
  GPIO_Config(5, gmOutput, gpNone, gsMedium);  // PA5 en sortie
  GPIO_Config(0, gmInput, gpPullUp, gsLow);    // PA0 en entrée
end;
```

### UART (Serial Port)

L'UART permet la communication série asynchrone.

```pascal
const
  USART1_BASE = $40013800;

type
  PUSART_Registers = ^TUSART_Registers;
  TUSART_Registers = record
    SR   : LongWord;  // Status Register
    DR   : LongWord;  // Data Register
    BRR  : LongWord;  // Baud Rate Register
    CR1  : LongWord;  // Control Register 1
    CR2  : LongWord;  // Control Register 2
    CR3  : LongWord;  // Control Register 3
    GTPR : LongWord;  // Guard Time and Prescaler
  end;

var
  USART1: PUSART_Registers = PUSART_Registers(USART1_BASE);

procedure UART_Init(BaudRate: LongWord);  
var
  SystemClock: LongWord;
  Divider: LongWord;
begin
  SystemClock := 72000000;  // 72 MHz pour STM32F103

  // Calculer le diviseur de fréquence
  Divider := (SystemClock + (BaudRate div 2)) div BaudRate;
  USART1^.BRR := Divider;

  // Activer TX et RX, puis activer l'USART
  USART1^.CR1 := (1 shl 13) or (1 shl 3) or (1 shl 2);
end;

procedure UART_SendChar(c: Char);  
begin
  // Attendre que le registre de transmission soit vide
  while (USART1^.SR and (1 shl 7)) = 0 do ;

  USART1^.DR := Ord(c);
end;

procedure UART_SendString(s: string);  
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    UART_SendChar(s[i]);
end;

function UART_ReceiveChar: Char;  
begin
  // Attendre qu'un caractère soit reçu
  while (USART1^.SR and (1 shl 5)) = 0 do ;

  Result := Char(USART1^.DR and $FF);
end;

// Utilisation
begin
  UART_Init(115200);
  UART_SendString('Hello from FreePascal!'#13#10);

  while True do
  begin
    c := UART_ReceiveChar();
    UART_SendChar(c);  // Echo
  end;
end;
```

### ADC (Analog to Digital Converter)

L'ADC convertit des tensions analogiques en valeurs numériques.

```pascal
const
  ADC1_BASE = $40012400;

type
  PADC_Registers = ^TADC_Registers;
  TADC_Registers = record
    SR    : LongWord;  // Status Register
    CR1   : LongWord;  // Control Register 1
    CR2   : LongWord;  // Control Register 2
    SMPR1 : LongWord;  // Sample Time Register 1
    SMPR2 : LongWord;  // Sample Time Register 2
    // ...
    SQR1  : LongWord;  // Regular Sequence Register 1
    SQR2  : LongWord;
    SQR3  : LongWord;
    DR    : LongWord;  // Data Register
  end;

var
  ADC1: PADC_Registers = PADC_Registers(ADC1_BASE);

procedure ADC_Init;  
begin
  // Activer l'ADC
  ADC1^.CR2 := ADC1^.CR2 or (1 shl 0);

  // Attendre la stabilisation
  Delay(1000);

  // Calibration
  ADC1^.CR2 := ADC1^.CR2 or (1 shl 3);
  while (ADC1^.CR2 and (1 shl 3)) <> 0 do ;
end;

function ADC_Read(Channel: Byte): Word;  
begin
  // Sélectionner le canal
  ADC1^.SQR3 := Channel and $1F;

  // Démarrer la conversion
  ADC1^.CR2 := ADC1^.CR2 or (1 shl 0);

  // Attendre la fin de conversion
  while (ADC1^.SR and (1 shl 1)) = 0 do ;

  // Lire le résultat (12 bits)
  Result := ADC1^.DR and $FFF;
end;

// Convertir en voltage (0-3.3V)
function ADC_ToVoltage(Value: Word): Real;  
begin
  Result := (Value * 3.3) / 4095.0;
end;

// Utilisation
var
  Temperature: Word;
  Voltage: Real;
begin
  ADC_Init;

  while True do
  begin
    Temperature := ADC_Read(16);  // Canal 16 = capteur interne
    Voltage := ADC_ToVoltage(Temperature);
    UART_SendString('Temp sensor: ' + FloatToStr(Voltage) + 'V'#13#10);
    Delay(10000000);
  end;
end;
```

### Timers et PWM

Les timers génèrent des événements périodiques ou du PWM (Pulse Width Modulation).

```pascal
const
  TIM2_BASE = $40000000;

type
  PTimer_Registers = ^TTimer_Registers;
  TTimer_Registers = record
    CR1   : LongWord;  // Control Register 1
    CR2   : LongWord;
    SMCR  : LongWord;  // Slave Mode Control
    DIER  : LongWord;  // DMA/Interrupt Enable
    SR    : LongWord;  // Status Register
    EGR   : LongWord;  // Event Generation
    CCMR1 : LongWord;  // Capture/Compare Mode 1
    CCMR2 : LongWord;
    CCER  : LongWord;  // Capture/Compare Enable
    CNT   : LongWord;  // Counter
    PSC   : LongWord;  // Prescaler
    ARR   : LongWord;  // Auto-Reload Register
    // ...
    CCR1  : LongWord;  // Capture/Compare Register 1
    CCR2  : LongWord;
    CCR3  : LongWord;
    CCR4  : LongWord;
  end;

var
  TIM2: PTimer_Registers = PTimer_Registers(TIM2_BASE);

procedure PWM_Init(Frequency: LongWord);  
var
  SystemClock, Prescaler, Period: LongWord;
begin
  SystemClock := 72000000;  // 72 MHz

  // Calculer prescaler et period pour obtenir la fréquence voulue
  Prescaler := 72 - 1;  // Division par 72 → 1 MHz
  Period := (SystemClock div (Prescaler + 1)) div Frequency - 1;

  TIM2^.PSC := Prescaler;
  TIM2^.ARR := Period;

  // Configurer canal 1 en mode PWM
  TIM2^.CCMR1 := (TIM2^.CCMR1 and $FF00) or $60;  // PWM mode 1
  TIM2^.CCER := TIM2^.CCER or $1;  // Activer sortie

  // Démarrer le timer
  TIM2^.CR1 := TIM2^.CR1 or $1;
end;

procedure PWM_SetDutyCycle(Percent: Byte);  
var
  DutyValue: LongWord;
begin
  if Percent > 100 then Percent := 100;

  DutyValue := (LongWord(TIM2^.ARR) * Percent) div 100;
  TIM2^.CCR1 := DutyValue;
end;

// Utilisation : variation progressive de luminosité LED
var
  Brightness: Byte;
begin
  PWM_Init(1000);  // 1 kHz

  while True do
  begin
    // Montée
    for Brightness := 0 to 100 do
    begin
      PWM_SetDutyCycle(Brightness);
      Delay(50000);
    end;

    // Descente
    for Brightness := 100 downto 0 do
    begin
      PWM_SetDutyCycle(Brightness);
      Delay(50000);
    end;
  end;
end;
```

### Interruptions matérielles

Les interruptions permettent au matériel de signaler des événements de façon asynchrone.

```pascal
var
  CompteurTicks: LongWord = 0;

// Gestionnaire d'interruption du Timer
procedure TIM2_IRQHandler; interrupt;  
begin
  // Vérifier le flag d'interruption
  if (TIM2^.SR and $1) <> 0 then
  begin
    // Incrémenter compteur
    Inc(CompteurTicks);

    // Acquitter l'interruption (clear flag)
    TIM2^.SR := TIM2^.SR and (not $1);
  end;
end;

procedure Timer_EnableInterrupt;  
begin
  // Activer l'interruption de mise à jour
  TIM2^.DIER := TIM2^.DIER or $1;

  // Configurer NVIC (Nested Vectored Interrupt Controller)
  // ... code spécifique ARM Cortex-M
end;

// Dans le programme principal
begin
  Timer_EnableInterrupt;

  while True do
  begin
    if CompteurTicks >= 1000 then  // Toutes les secondes
    begin
      CompteurTicks := 0;
      LED_Toggle();
    end;
  end;
end;
```

## Accès matériel sur Windows

### Limitations modernes

Sur Windows moderne (Vista et ultérieur), l'accès direct au matériel depuis l'espace utilisateur est **fortement restreint** pour des raisons de sécurité et stabilité.

```pascal
// ✗ NE FONCTIONNE PLUS sur Windows moderne
procedure OutPortB(Port: Word; Value: Byte); assembler;  
asm
  mov dx, Port
  mov al, Value
  out dx, al  // ← Exception : accès privilégié requis !
end;
```

### Solutions pour Windows

#### 1. Drivers kernel-mode

Pour un accès matériel réel, il faut développer un **driver en mode noyau** (kernel driver), typiquement en C avec le WDK (Windows Driver Kit).

FreePascal peut ensuite communiquer avec ce driver via des appels DeviceIoControl.

```pascal
uses
  Windows;

const
  IOCTL_CUSTOM_READ_PORT = $222000;
  IOCTL_CUSTOM_WRITE_PORT = $222004;

function OpenDriver(DriverName: string): THandle;  
begin
  Result := CreateFile(
    PChar('\\.\' + DriverName),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
end;

function ReadPortViaDriver(hDriver: THandle; Port: Word): Byte;  
var
  BytesReturned: DWORD;
  InBuffer: Word;
  OutBuffer: Byte;
begin
  InBuffer := Port;

  if not DeviceIoControl(
    hDriver,
    IOCTL_CUSTOM_READ_PORT,
    @InBuffer,
    SizeOf(InBuffer),
    @OutBuffer,
    SizeOf(OutBuffer),
    BytesReturned,
    nil
  ) then
    raise Exception.Create('DeviceIoControl failed');

  Result := OutBuffer;
end;

// Utilisation
var
  hDriver: THandle;
  Value: Byte;
begin
  hDriver := OpenDriver('MyHardwareDriver');
  try
    Value := ReadPortViaDriver(hDriver, $3F8);
  finally
    CloseHandle(hDriver);
  end;
end;
```

#### 2. WinRing0 / InpOut32

Bibliothèques tierces qui fournissent un driver générique pour accès ports/mémoire.

```pascal
// Utilisation de InpOut32.dll
function Inp32(PortAddress: Word): Byte; stdcall; external 'inpout32.dll';  
procedure Out32(PortAddress: Word; Data: Byte); stdcall; external 'inpout32.dll';

// Maintenant fonctionnel (avec le driver InpOut32)
begin
  Out32($378, $FF);  // Écrire sur port parallèle
  Value := Inp32($378);
end;
```

**Attention** : nécessite l'installation du driver et privilèges administrateur.

#### 3. Périphériques USB/série via API

Pour du matériel moderne (Arduino, instruments...), utilisez les API de communication.

```pascal
uses
  Windows;

var
  hSerial: THandle;
  DCB: TDCB;

procedure OpenSerialPort(PortName: string; BaudRate: DWORD);  
begin
  hSerial := CreateFile(
    PChar(PortName),  // 'COM3'
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    0,
    0
  );

  if hSerial = INVALID_HANDLE_VALUE then
    raise Exception.Create('Cannot open ' + PortName);

  // Configurer paramètres série
  FillChar(DCB, SizeOf(DCB), 0);
  DCB.DCBlength := SizeOf(DCB);

  if not GetCommState(hSerial, DCB) then
    raise Exception.Create('GetCommState failed');

  DCB.BaudRate := BaudRate;
  DCB.ByteSize := 8;
  DCB.Parity := NOPARITY;
  DCB.StopBits := ONESTOPBIT;

  if not SetCommState(hSerial, DCB) then
    raise Exception.Create('SetCommState failed');
end;

procedure WriteSerial(Data: string);  
var
  BytesWritten: DWORD;
begin
  if not WriteFile(hSerial, Data[1], Length(Data), BytesWritten, nil) then
    raise Exception.Create('WriteFile failed');
end;

function ReadSerial(MaxBytes: Integer): string;  
var
  Buffer: array[0..255] of Char;
  BytesRead: DWORD;
begin
  if ReadFile(hSerial, Buffer, MaxBytes, BytesRead, nil) then
  begin
    SetLength(Result, BytesRead);
    Move(Buffer, Result[1], BytesRead);
  end
  else
    Result := '';
end;

// Utilisation : communication avec Arduino
begin
  OpenSerialPort('COM3', 115200);
  try
    WriteSerial('LED ON'#13#10);
    Sleep(1000);
    Response := ReadSerial(256);
    WriteLn('Arduino says: ', Response);
  finally
    CloseHandle(hSerial);
  end;
end;
```

## Accès matériel sur Linux/Ubuntu

### Accès direct avec permissions

Linux permet l'accès direct à `/dev/mem` et `/dev/port` avec les bonnes permissions.

```pascal
uses
  BaseUnix, Unix;

var
  fdMem: Integer;
  GPIO_Map: Pointer;

const
  BCM2835_PERI_BASE = $3F000000;  // Raspberry Pi 3
  GPIO_BASE = BCM2835_PERI_BASE + $200000;
  BLOCK_SIZE = 4096;

procedure MapGPIOMemory;  
begin
  // Ouvrir /dev/mem (nécessite root ou capability)
  fdMem := FpOpen('/dev/mem', O_RDWR or O_SYNC);
  if fdMem < 0 then
    raise Exception.Create('Cannot open /dev/mem - need root?');

  // Mapper la mémoire GPIO
  GPIO_Map := FpMmap(
    nil,
    BLOCK_SIZE,
    PROT_READ or PROT_WRITE,
    MAP_SHARED,
    fdMem,
    GPIO_BASE
  );

  if GPIO_Map = MAP_FAILED then
    raise Exception.Create('mmap failed');
end;

procedure UnmapGPIOMemory;  
begin
  FpMunmap(GPIO_Map, BLOCK_SIZE);
  FpClose(fdMem);
end;

// Accès aux registres GPIO
type
  PGPIO = ^TGPIO;
  TGPIO = record
    GPFSEL: array[0..5] of LongWord;   // Function Select
    Reserved1: LongWord;
    GPSET: array[0..1] of LongWord;    // Pin Output Set
    Reserved2: LongWord;
    GPCLR: array[0..1] of LongWord;    // Pin Output Clear
    Reserved3: LongWord;
    GPLEV: array[0..1] of LongWord;    // Pin Level
    // ...
  end;

var
  GPIO: PGPIO;

procedure SetupGPIOPin(Pin: Byte; AsOutput: Boolean);  
var
  Reg, Shift: Byte;
  Value: LongWord;
begin
  Reg := Pin div 10;
  Shift := (Pin mod 10) * 3;

  Value := GPIO^.GPFSEL[Reg];
  Value := Value and (not (7 shl Shift));  // Clear

  if AsOutput then
    Value := Value or (1 shl Shift);  // Set as output

  GPIO^.GPFSEL[Reg] := Value;
end;

procedure SetGPIOPin(Pin: Byte; High: Boolean);  
var
  Reg, Bit: Byte;
begin
  Reg := Pin div 32;
  Bit := Pin mod 32;

  if High then
    GPIO^.GPSET[Reg] := (1 shl Bit)
  else
    GPIO^.GPCLR[Reg] := (1 shl Bit);
end;

// Utilisation sur Raspberry Pi
begin
  MapGPIOMemory;
  GPIO := PGPIO(GPIO_Map);

  try
    SetupGPIOPin(17, True);  // GPIO17 en sortie

    // Clignotement
    while True do
    begin
      SetGPIOPin(17, True);
      Sleep(500);
      SetGPIOPin(17, False);
      Sleep(500);
    end;
  finally
    UnmapGPIOMemory;
  end;
end;
```

### Fichiers sysfs GPIO (méthode recommandée)

Linux expose les GPIO via le système de fichiers `/sys/class/gpio`.

```pascal
uses
  SysUtils;

procedure ExportGPIO(Pin: Integer);  
var
  f: TextFile;
begin
  AssignFile(f, '/sys/class/gpio/export');
  Rewrite(f);
  WriteLn(f, Pin);
  CloseFile(f);
  Sleep(100);  // Attendre création
end;

procedure UnexportGPIO(Pin: Integer);  
var
  f: TextFile;
begin
  AssignFile(f, '/sys/class/gpio/unexport');
  Rewrite(f);
  WriteLn(f, Pin);
  CloseFile(f);
end;

procedure SetGPIODirection(Pin: Integer; IsOutput: Boolean);  
var
  f: TextFile;
  Path: string;
begin
  Path := Format('/sys/class/gpio/gpio%d/direction', [Pin]);
  AssignFile(f, Path);
  Rewrite(f);

  if IsOutput then
    WriteLn(f, 'out')
  else
    WriteLn(f, 'in');

  CloseFile(f);
end;

procedure WriteGPIO(Pin: Integer; Value: Boolean);  
var
  f: TextFile;
  Path: string;
begin
  Path := Format('/sys/class/gpio/gpio%d/value', [Pin]);
  AssignFile(f, Path);
  Rewrite(f);

  if Value then
    WriteLn(f, '1')
  else
    WriteLn(f, '0');

  CloseFile(f);
end;

function ReadGPIO(Pin: Integer): Boolean;  
var
  f: TextFile;
  Path, Line: string;
begin
  Path := Format('/sys/class/gpio/gpio%d/value', [Pin]);
  AssignFile(f, Path);
  Reset(f);
  ReadLn(f, Line);
  CloseFile(f);

  Result := (Line = '1');
end;

// Utilisation simple et portable
const
  LED_PIN = 17;
  BUTTON_PIN = 27;

begin
  // Configuration
  ExportGPIO(LED_PIN);
  ExportGPIO(BUTTON_PIN);

  SetGPIODirection(LED_PIN, True);   // Sortie
  SetGPIODirection(BUTTON_PIN, False); // Entrée

  try
    // Boucle principale
    while True do
    begin
      if ReadGPIO(BUTTON_PIN) then
        WriteGPIO(LED_PIN, True)
      else
        WriteGPIO(LED_PIN, False);

      Sleep(10);
    end;
  finally
    UnexportGPIO(LED_PIN);
    UnexportGPIO(BUTTON_PIN);
  end;
end;
```

### Accès au port série Linux

```pascal
uses
  BaseUnix, Termio;

var
  fdSerial: Integer;

procedure ConfigureSerialPort(Device: string; BaudRate: Cardinal);  
var
  tios: termios;
  Speed: Cardinal;
begin
  // Ouvrir le périphérique
  fdSerial := FpOpen(Device, O_RDWR or O_NOCTTY);
  if fdSerial < 0 then
    raise Exception.Create('Cannot open ' + Device);

  // Lire configuration actuelle
  TCGetAttr(fdSerial, tios);

  // Convertir baudrate
  case BaudRate of
    9600: Speed := B9600;
    19200: Speed := B19200;
    38400: Speed := B38400;
    57600: Speed := B57600;
    115200: Speed := B115200;
  else
    Speed := B9600;
  end;

  // Configurer vitesse
  CFSetISpeed(tios, Speed);
  CFSetOSpeed(tios, Speed);

  // 8N1 (8 bits, no parity, 1 stop bit)
  tios.c_cflag := tios.c_cflag and (not PARENB);  // Pas de parité
  tios.c_cflag := tios.c_cflag and (not CSTOPB);  // 1 stop bit
  tios.c_cflag := tios.c_cflag and (not CSIZE);
  tios.c_cflag := tios.c_cflag or CS8;            // 8 bits

  // Mode raw (pas de traitement)
  tios.c_lflag := 0;
  tios.c_oflag := 0;
  tios.c_iflag := 0;

  // Timeout et taille minimale
  tios.c_cc[VMIN] := 1;
  tios.c_cc[VTIME] := 0;

  // Appliquer la configuration
  TCSetAttr(fdSerial, TCSANOW, tios);

  // Vider les buffers
  TCFlush(fdSerial, TCIOFLUSH);
end;

procedure WriteSerial(Data: string);  
begin
  FpWrite(fdSerial, Data[1], Length(Data));
end;

function ReadSerial(MaxBytes: Integer): string;  
var
  Buffer: array[0..255] of Char;
  BytesRead: Integer;
begin
  BytesRead := FpRead(fdSerial, Buffer, MaxBytes);
  if BytesRead > 0 then
  begin
    SetLength(Result, BytesRead);
    Move(Buffer, Result[1], BytesRead);
  end
  else
    Result := '';
end;

procedure CloseSerial;  
begin
  FpClose(fdSerial);
end;

// Utilisation
begin
  ConfigureSerialPort('/dev/ttyUSB0', 115200);
  try
    WriteSerial('AT'#13#10);  // Commande AT pour modem
    Sleep(100);
    Response := ReadSerial(256);
    WriteLn('Response: ', Response);
  finally
    CloseSerial;
  end;
end;
```

### I2C sur Linux

Le bus I2C est couramment utilisé pour capteurs et périphériques.

```pascal
uses
  BaseUnix, Linux;

const
  I2C_SLAVE = $0703;  // ioctl pour définir l'adresse slave

var
  fdI2C: Integer;

procedure OpenI2C(Device: string; SlaveAddr: Byte);  
begin
  // Ouvrir le bus I2C
  fdI2C := FpOpen(Device, O_RDWR);
  if fdI2C < 0 then
    raise Exception.Create('Cannot open ' + Device);

  // Définir l'adresse du périphérique
  if FpIOCtl(fdI2C, I2C_SLAVE, Pointer(PtrUInt(SlaveAddr))) < 0 then
    raise Exception.Create('Cannot set I2C slave address');
end;

procedure WriteI2CReg(RegAddr, Value: Byte);  
var
  Buffer: array[0..1] of Byte;
begin
  Buffer[0] := RegAddr;
  Buffer[1] := Value;

  if FpWrite(fdI2C, Buffer, 2) <> 2 then
    raise Exception.Create('I2C write failed');
end;

function ReadI2CReg(RegAddr: Byte): Byte;  
var
  Buffer: Byte;
begin
  // Écrire l'adresse du registre
  if FpWrite(fdI2C, RegAddr, 1) <> 1 then
    raise Exception.Create('I2C write register address failed');

  // Lire la valeur
  if FpRead(fdI2C, Buffer, 1) <> 1 then
    raise Exception.Create('I2C read failed');

  Result := Buffer;
end;

procedure CloseI2C;  
begin
  FpClose(fdI2C);
end;

// Exemple : lecture température BME280
const
  BME280_ADDR = $76;
  BME280_TEMP_MSB = $FA;

var
  TempMSB, TempLSB, TempXLSB: Byte;
  TempRaw: Integer;
  Temperature: Real;
begin
  OpenI2C('/dev/i2c-1', BME280_ADDR);
  try
    // Lire les 3 registres de température
    TempMSB := ReadI2CReg(BME280_TEMP_MSB);
    TempLSB := ReadI2CReg(BME280_TEMP_MSB + 1);
    TempXLSB := ReadI2CReg(BME280_TEMP_MSB + 2);

    // Reconstituer la valeur 20 bits
    TempRaw := (TempMSB shl 12) or (TempLSB shl 4) or (TempXLSB shr 4);

    // Conversion (simplifiée)
    Temperature := TempRaw / 5120.0;

    WriteLn('Temperature: ', Temperature:0:2, ' °C');
  finally
    CloseI2C;
  end;
end;
```

### SPI sur Linux

Le bus SPI (Serial Peripheral Interface) est utilisé pour des communications rapides.

```pascal
uses
  BaseUnix, Linux;

const
  SPI_IOC_MAGIC = Ord('k');
  SPI_IOC_MESSAGE_1 = (SPI_IOC_MAGIC shl 8) or 0;

  SPI_MODE_0 = $00;
  SPI_MODE_1 = $01;
  SPI_MODE_2 = $02;
  SPI_MODE_3 = $03;

type
  spi_ioc_transfer = record
    tx_buf: QWord;        // Pointeur buffer TX
    rx_buf: QWord;        // Pointeur buffer RX
    len: Cardinal;        // Longueur
    speed_hz: Cardinal;   // Vitesse
    delay_usecs: Word;    // Délai entre transferts
    bits_per_word: Byte;  // Bits par mot
    cs_change: Byte;      // Changer CS après
    pad: Cardinal;
  end;

var
  fdSPI: Integer;

procedure OpenSPI(Device: string; Mode: Byte; Speed: Cardinal);  
var
  BitsPerWord: Byte;
begin
  fdSPI := FpOpen(Device, O_RDWR);
  if fdSPI < 0 then
    raise Exception.Create('Cannot open ' + Device);

  // Configurer le mode SPI
  if FpIOCtl(fdSPI, SPI_IOC_RD_MODE, @Mode) < 0 then
    raise Exception.Create('Cannot set SPI mode');

  // Configurer la vitesse
  if FpIOCtl(fdSPI, SPI_IOC_WR_MAX_SPEED_HZ, @Speed) < 0 then
    raise Exception.Create('Cannot set SPI speed');

  // Configurer bits par mot
  BitsPerWord := 8;
  if FpIOCtl(fdSPI, SPI_IOC_WR_BITS_PER_WORD, @BitsPerWord) < 0 then
    raise Exception.Create('Cannot set SPI bits per word');
end;

procedure SPITransfer(TxData: PByte; RxData: PByte; Len: Integer);  
var
  Transfer: spi_ioc_transfer;
begin
  FillChar(Transfer, SizeOf(Transfer), 0);

  Transfer.tx_buf := QWord(TxData);
  Transfer.rx_buf := QWord(RxData);
  Transfer.len := Len;
  Transfer.speed_hz := 1000000;  // 1 MHz
  Transfer.bits_per_word := 8;

  if FpIOCtl(fdSPI, SPI_IOC_MESSAGE_1, @Transfer) < 0 then
    raise Exception.Create('SPI transfer failed');
end;

procedure CloseSPI;  
begin
  FpClose(fdSPI);
end;

// Exemple : lecture d'un registre MCP3008 (ADC SPI)
function ReadMCP3008(Channel: Byte): Word;  
var
  TxBuf, RxBuf: array[0..2] of Byte;
begin
  // Commande de lecture MCP3008
  TxBuf[0] := $01;  // Start bit
  TxBuf[1] := ($80 or (Channel shl 4));  // Single-ended, channel
  TxBuf[2] := $00;

  SPITransfer(@TxBuf, @RxBuf, 3);

  // Extraire résultat 10 bits
  Result := ((RxBuf[1] and $03) shl 8) or RxBuf[2];
end;

begin
  OpenSPI('/dev/spidev0.0', SPI_MODE_0, 1000000);
  try
    for i := 0 to 7 do
    begin
      Value := ReadMCP3008(i);
      WriteLn('Channel ', i, ': ', Value);
    end;
  finally
    CloseSPI;
  end;
end;
```

## Communication USB

### LibUSB sur Linux et Windows

LibUSB est une bibliothèque multiplateforme pour accéder aux périphériques USB.

```pascal
uses
  libusb;  // Bindings FreePascal pour libusb

const
  VENDOR_ID = $1234;
  PRODUCT_ID = $5678;

var
  Context: Plibusb_context;
  DevHandle: Plibusb_device_handle;

procedure InitUSB;  
var
  r: Integer;
begin
  r := libusb_init(@Context);
  if r < 0 then
    raise Exception.Create('libusb_init failed');

  // Ouvrir le périphérique
  DevHandle := libusb_open_device_with_vid_pid(Context, VENDOR_ID, PRODUCT_ID);
  if DevHandle = nil then
    raise Exception.Create('Device not found');

  // Claim interface
  r := libusb_claim_interface(DevHandle, 0);
  if r < 0 then
    raise Exception.Create('Cannot claim interface');
end;

procedure WriteUSB(Endpoint: Byte; Data: PByte; Length: Integer);  
var
  Transferred: Integer;
  r: Integer;
begin
  r := libusb_bulk_transfer(
    DevHandle,
    Endpoint,
    Data,
    Length,
    @Transferred,
    1000  // Timeout 1s
  );

  if r < 0 then
    raise Exception.CreateFmt('USB write failed: %d', [r]);
end;

function ReadUSB(Endpoint: Byte; Data: PByte; MaxLength: Integer): Integer;  
var
  Transferred: Integer;
  r: Integer;
begin
  r := libusb_bulk_transfer(
    DevHandle,
    Endpoint or $80,  // IN endpoint
    Data,
    MaxLength,
    @Transferred,
    1000
  );

  if r < 0 then
    raise Exception.CreateFmt('USB read failed: %d', [r]);

  Result := Transferred;
end;

procedure CloseUSB;  
begin
  libusb_release_interface(DevHandle, 0);
  libusb_close(DevHandle);
  libusb_exit(Context);
end;

// Utilisation
var
  Buffer: array[0..63] of Byte;
  BytesRead: Integer;
begin
  InitUSB;
  try
    // Envoyer commande
    Buffer[0] := $01;  // Commande
    Buffer[1] := $FF;  // Paramètre
    WriteUSB($01, @Buffer, 2);

    // Lire réponse
    BytesRead := ReadUSB($81, @Buffer, 64);
    WriteLn('Received ', BytesRead, ' bytes');
  finally
    CloseUSB;
  end;
end;
```

## DMA (Direct Memory Access)

Le DMA permet aux périphériques de transférer des données directement en mémoire sans solliciter le CPU.

### DMA sur microcontrôleur (STM32)

```pascal
const
  DMA1_BASE = $40020000;
  DMA1_Channel1 = DMA1_BASE + $0008;

type
  PDMA_Channel = ^TDMA_Channel;
  TDMA_Channel = record
    CCR   : LongWord;  // Configuration
    CNDTR : LongWord;  // Number of data
    CPAR  : LongWord;  // Peripheral address
    CMAR  : LongWord;  // Memory address
  end;

var
  DMA_CH1: PDMA_Channel = PDMA_Channel(DMA1_Channel1);

procedure ConfigureDMA_ADC_to_Memory(var Buffer: array of Word);  
begin
  // Désactiver le canal DMA
  DMA_CH1^.CCR := 0;

  // Configurer les adresses
  DMA_CH1^.CPAR := LongWord(@ADC1^.DR);  // Source: registre ADC
  DMA_CH1^.CMAR := LongWord(@Buffer[0]); // Destination: RAM

  // Nombre de transferts
  DMA_CH1^.CNDTR := Length(Buffer);

  // Configuration
  // MINC: Memory increment
  // PSIZE: Peripheral size 16-bit
  // MSIZE: Memory size 16-bit
  // CIRC: Circular mode
  DMA_CH1^.CCR := (1 shl 7) or  // MINC
                  (1 shl 10) or // PSIZE = 16-bit
                  (1 shl 8) or  // MSIZE = 16-bit
                  (1 shl 5);    // CIRC

  // Activer le canal
  DMA_CH1^.CCR := DMA_CH1^.CCR or 1;
end;

// Utilisation : acquisition continue ADC vers buffer
var
  ADCBuffer: array[0..255] of Word;
begin
  ConfigureDMA_ADC_to_Memory(ADCBuffer);

  // L'ADC remplit automatiquement le buffer via DMA
  // Le CPU peut faire autre chose !

  while True do
  begin
    // Traiter les données
    for i := 0 to 255 do
      TraiterEchantillon(ADCBuffer[i]);

    Sleep(100);
  end;
end;
```

## Gestion d'interruptions avancée

### Priorités d'interruptions (ARM Cortex-M)

```pascal
const
  NVIC_BASE = $E000E100;

type
  PNVIC = ^TNVIC;
  TNVIC = record
    ISER: array[0..7] of LongWord;  // Interrupt Set Enable
    Reserved1: array[0..23] of LongWord;
    ICER: array[0..7] of LongWord;  // Interrupt Clear Enable
    Reserved2: array[0..23] of LongWord;
    ISPR: array[0..7] of LongWord;  // Interrupt Set Pending
    Reserved3: array[0..23] of LongWord;
    ICPR: array[0..7] of LongWord;  // Interrupt Clear Pending
    Reserved4: array[0..23] of LongWord;
    IABR: array[0..7] of LongWord;  // Interrupt Active Bit
    Reserved5: array[0..55] of LongWord;
    IP: array[0..239] of Byte;      // Interrupt Priority
  end;

var
  NVIC: PNVIC = PNVIC(NVIC_BASE);

procedure EnableIRQ(IRQn: Byte; Priority: Byte);  
var
  RegIndex, BitPos: Byte;
begin
  // Activer l'interruption
  RegIndex := IRQn div 32;
  BitPos := IRQn mod 32;
  NVIC^.ISER[RegIndex] := (1 shl BitPos);

  // Définir la priorité (0 = plus haute)
  NVIC^.IP[IRQn] := Priority shl 4;  // 4 bits de priorité
end;

procedure DisableIRQ(IRQn: Byte);  
var
  RegIndex, BitPos: Byte;
begin
  RegIndex := IRQn div 32;
  BitPos := IRQn mod 32;
  NVIC^.ICER[RegIndex] := (1 shl BitPos);
end;

// Utilisation
const
  USART1_IRQn = 37;
  TIM2_IRQn = 28;

begin
  EnableIRQ(USART1_IRQn, 1);  // Priorité haute
  EnableIRQ(TIM2_IRQn, 3);    // Priorité basse
end;
```

### Interruptions imbriquées

```pascal
var
  CompteurTIM2: LongWord = 0;
  CompteurUSART: LongWord = 0;

// Interruption de priorité basse
procedure TIM2_IRQHandler; interrupt;  
begin
  Inc(CompteurTIM2);

  // Cette interruption peut être préemptée par USART1
  // si USART1 a une priorité plus élevée

  // Traitement long...
  for i := 0 to 1000 do
    asm nop end;

  TIM2^.SR := 0;  // Clear flags
end;

// Interruption de priorité haute
procedure USART1_IRQHandler; interrupt;  
begin
  Inc(CompteurUSART);

  // Cette interruption préempte TIM2
  // Traitement rapide uniquement !

  ReceivedChar := USART1^.DR;
  BufferUSART[BufferIndex] := ReceivedChar;
  Inc(BufferIndex);
end;
```

## Watchdog et sécurité matérielle

### Watchdog Timer

Le watchdog redémarre le système si le logiciel se bloque.

```pascal
const
  IWDG_BASE = $40003000;

type
  PIWDG = ^TIWDG;
  TIWDG = record
    KR  : LongWord;  // Key Register
    PR  : LongWord;  // Prescaler Register
    RLR : LongWord;  // Reload Register
    SR  : LongWord;  // Status Register
  end;

var
  IWDG: PIWDG = PIWDG(IWDG_BASE);

procedure InitWatchdog(TimeoutMs: Word);  
var
  Prescaler, Reload: Byte;
begin
  // Activer l'accès aux registres
  IWDG^.KR := $5555;

  // Configurer prescaler (LSI = 40 kHz)
  // timeout = (Prescaler * Reload) / 40000
  Prescaler := 6;  // Division par 256
  Reload := (TimeoutMs * 40) div 256;

  IWDG^.PR := Prescaler;
  IWDG^.RLR := Reload;

  // Recharger et démarrer
  IWDG^.KR := $AAAA;  // Reload
  IWDG^.KR := $CCCC;  // Start
end;

procedure FeedWatchdog;  
begin
  IWDG^.KR := $AAAA;  // Reload counter
end;

// Utilisation
begin
  InitWatchdog(1000);  // Timeout 1 seconde

  while True do
  begin
    // Travail normal
    TraiterDonnees();

    // Rafraîchir le watchdog
    FeedWatchdog();

    // Si le programme se bloque, pas de feed
    // → Reset automatique après 1s
  end;
end;
```

## Debugging matériel

### JTAG et SWD

Les interfaces JTAG/SWD permettent le debugging matériel.

```pascal
// Configuration SWD pour debugging
// À compiler avec options de debug

{$IFDEF DEBUG}
procedure InitDebugInterface;  
const
  DBGMCU_BASE = $E0042000;
  DBGMCU_CR = DBGMCU_BASE + $04;
var
  CR: ^LongWord;
begin
  CR := Pointer(DBGMCU_CR);

  // Garder les timers actifs en debug
  CR^ := CR^ or (1 shl 0);  // DBG_TIM2_STOP
  CR^ := CR^ or (1 shl 1);  // DBG_TIM3_STOP
end;
{$ENDIF}
```

### Points d'arrêt matériels

```pascal
// Breakpoint en assembleur inline
procedure HardwareBreakpoint;  
begin
  {$IFDEF DEBUG}
  asm
    bkpt #0  // ARM breakpoint instruction
  end;
  {$ENDIF}
end;

// Dans le code
procedure FonctionCritique;  
begin
  HardwareBreakpoint();  // Le debugger s'arrête ici

  // Code à analyser
  CalculComplexe();
end;
```

## Optimisations matérielles

### Accès alignés en mémoire

```pascal
// ✗ MAUVAIS : accès non aligné (lent sur ARM)
var
  Buffer: array[0..10] of Byte;
  Value: ^LongWord;
begin
  Value := @Buffer[1];  // Adresse impaire !
  Value^ := $12345678;  // Peut causer exception ou lenteur
end;

// ✓ BON : accès aligné
var
  Buffer: array[0..10] of LongWord;  // Aligné automatiquement
begin
  Buffer[0] := $12345678;  // Rapide
end;

// Forcer l'alignement
type
  TAlignedData = record
    Padding: array[0..2] of Byte;
    Data: LongWord;
  end align 4;  // Alignement forcé sur 4 bytes
```

### Instructions atomiques

```pascal
// Opération atomique pour multi-threading
function AtomicIncrement(var Value: LongInt): LongInt; assembler;
{$IFDEF CPUARM}
asm  
retry:
  ldrex r1, [r0]      // Load exclusive
  add r1, r1, #1      // Increment
  strex r2, r1, [r0]  // Store exclusive
  cmp r2, #0
  bne retry           // Retry si échec
  mov r0, r1
end;
{$ENDIF}
```

### Cache et barrières mémoire

```pascal
procedure FlushCache;  
begin
  {$IFDEF CPUARM}
  asm
    dsb  // Data Synchronization Barrier
    isb  // Instruction Synchronization Barrier
  end;
  {$ENDIF}
end;

procedure InvalidateCache;  
begin
  {$IFDEF CPUARM}
  // Invalider le cache d'instructions
  asm
    mov r0, #0
    mcr p15, 0, r0, c7, c5, 0
  end;
  {$ENDIF}
end;
```

## Exemple complet : Logger de données

Programme complet qui lit des capteurs et enregistre sur carte SD.

```pascal
program DataLogger;

{$MODE OBJFPC}

uses
  GPIO_Unit, SPI_Unit, UART_Unit, SDCard_Unit;

const
  LED_STATUS = 13;
  BUTTON_START = 2;

  SD_CS_PIN = 10;

  LOG_INTERVAL = 1000;  // 1 seconde

var
  Temperature: Real;
  Humidity: Real;
  LogFile: TSDFile;
  IsLogging: Boolean = False;

procedure InitHardware;  
begin
  // GPIO
  GPIO_Init(LED_STATUS, gmOutput);
  GPIO_Init(BUTTON_START, gmInput, gpPullUp);

  // SPI pour carte SD
  SPI_Init(SPI_MODE_0, 4000000);

  // UART pour debug
  UART_Init(115200);
  UART_SendString('Data Logger v1.0'#13#10);
end;

procedure ReadSensors;  
begin
  // Simulé : lecture I2C de BME280
  Temperature := ReadBME280_Temperature();
  Humidity := ReadBME280_Humidity();
end;

procedure LogData;  
var
  Line: string;
  Timestamp: LongWord;
begin
  Timestamp := GetTickCount();

  Line := Format('%d,%.2f,%.2f'#13#10,
                 [Timestamp, Temperature, Humidity]);

  SDCard_WriteString(LogFile, Line);

  // LED blink pour indiquer activité
  GPIO_Toggle(LED_STATUS);
end;

// Interruption bouton
procedure EXTI2_IRQHandler; interrupt;  
begin
  IsLogging := not IsLogging;

  if IsLogging then
  begin
    UART_SendString('Logging started'#13#10);
    LogFile := SDCard_OpenFile('datalog.csv', fmCreate);
    SDCard_WriteString(LogFile, 'Time,Temp,Humidity'#13#10);
  end
  else
  begin
    UART_SendString('Logging stopped'#13#10);
    SDCard_CloseFile(LogFile);
  end;

  // Clear interrupt flag
  EXTI^.PR := (1 shl 2);
end;

// Programme principal
var
  LastLog: LongWord;
begin
  InitHardware;

  // Activer interruption bouton
  EnableIRQ(EXTI2_IRQn, 5);

  LastLog := GetTickCount();

  while True do
  begin
    if IsLogging then
    begin
      if (GetTickCount() - LastLog) >= LOG_INTERVAL then
      begin
        ReadSensors();
        LogData();
        LastLog := GetTickCount();

        // Debug UART
        UART_SendString(Format('T:%.1f H:%.1f'#13#10,
                               [Temperature, Humidity]));
      end;
    end;

    // Économie d'énergie
    WaitForInterrupt();
  end;
end.
```

## Bonnes pratiques

### 1. Toujours documenter les registres

```pascal
// ✓ BON
const
  // ADC1 Status Register (SR)
  // Bit 1: EOC - End of conversion
  // Bit 0: AWD - Analog watchdog flag
  ADC1_SR = $40012400;

// ✗ MAUVAIS
const
  ADC1_SR = $40012400;  // Quoi ? Pourquoi ?
```

### 2. Vérifier les résultats

```pascal
// ✓ BON
if FpOpen('/dev/i2c-1', O_RDWR) < 0 then  
begin
  WriteLn('Error: ', StrError(errno));
  Halt(1);
end;

// ✗ MAUVAIS
fd := FpOpen('/dev/i2c-1', O_RDWR);
// Pas de vérification = crash potentiel
```

### 3. Gérer les timeouts

```pascal
// ✓ BON
function WaitForFlag(Timeout: LongWord): Boolean;  
var
  StartTime: LongWord;
begin
  StartTime := GetTickCount();

  while (USART1^.SR and FLAG_TXE) = 0 do
  begin
    if (GetTickCount() - StartTime) > Timeout then
      Exit(False);  // Timeout !
  end;

  Result := True;
end;

// ✗ MAUVAIS
while (USART1^.SR and FLAG_TXE) = 0 do ;  // Peut bloquer indéfiniment
```

### 4. Protéger les sections critiques

```pascal
// ✓ BON
procedure UpdateSharedVariable;  
begin
  DisableInterrupts();
  try
    GlobalCounter := GlobalCounter + 1;
  finally
    EnableInterrupts();
  end;
end;
```

## Ressources et outils

### Documentation matérielle

- **Datasheets** : toujours lire la documentation du fabricant
- **Reference Manuals** : pour les microcontrôleurs (STM32, AVR...)
- **Application Notes** : exemples d'implémentation

### Outils de développement

- **OpenOCD** : debugging JTAG/SWD multi-plateforme
- **st-link** : programmation STM32
- **avrdude** : programmation AVR/Arduino
- **Logic Analyzer** : Saleae, PulseView pour analyser les signaux

### Simulateurs

- **Proteus** : simulation de circuits et microcontrôleurs
- **QEMU** : émulation ARM
- **SimulIDE** : simulateur open-source Arduino/AVR

## Conclusion

L'accès matériel direct avec FreePascal est possible et puissant, mais requiert :

1. **Compréhension du matériel** : lisez les datasheets et reference manuals
2. **Patience et rigueur** : une erreur peut bloquer ou endommager le matériel
3. **Outils appropriés** : debugger JTAG/SWD, analyseur logique, multimètre
4. **Tests progressifs** : commencez simple (LED), puis complexifiez
5. **Sécurité** : watchdog, timeouts, vérifications systématiques

### Tableau récapitulatif : Accès matériel par plateforme

| Plateforme | Accès direct | Méthode recommandée | Difficulté |
|------------|--------------|---------------------|------------|
| **Microcontrôleur ARM** | Oui (bare-metal) | MMIO, registres | Moyenne |
| **Microcontrôleur AVR** | Oui (bare-metal) | Port I/O | Moyenne |
| **Raspberry Pi Linux** | Oui (/dev/mem) | sysfs GPIO préféré | Facile |
| **PC Windows moderne** | Non (protégé) | Driver kernel ou InpOut32 | Difficile |
| **PC Linux** | Limité (root requis) | sysfs, /dev/* | Facile à moyenne |
| **USB (tous OS)** | Via API | LibUSB multiplateforme | Facile |

### Progresser étape par étape

**Niveau débutant** :
1. Clignoter une LED (GPIO basique)
2. Lire un bouton
3. Communication série (UART)
4. Affichage LCD

**Niveau intermédiaire** :
1. ADC et lecture de capteurs analogiques
2. PWM pour contrôle moteur/LED
3. Communication I2C et SPI
4. Interruptions simples

**Niveau avancé** :
1. DMA et transferts haute vitesse
2. Timers complexes et génération de signaux
3. Multiples interruptions imbriquées
4. Systèmes temps réel

**Niveau expert** :
1. Développement de drivers complets
2. Systèmes multi-processeurs
3. FPGA et co-processeurs
4. Certifications matérielles (automotive, médical)

## Projets pratiques suggérés

### Projet 1 : Station météo autonome

**Matériel** :
- Microcontrôleur STM32 ou ESP32
- Capteur BME280 (température, humidité, pression)
- Carte SD pour logging
- Écran LCD I2C
- Alimentation batterie + solaire

**Compétences mises en œuvre** :
- I2C pour capteur et écran
- SPI pour carte SD
- RTC pour horodatage
- Gestion d'énergie (sleep modes)
- Interruptions (réveil périodique)

```pascal
program WeatherStation;

type
  TWeatherData = record
    Timestamp: TDateTime;
    Temperature: Real;
    Humidity: Real;
    Pressure: Real;
  end;

procedure ReadAndLogWeather;  
var
  Data: TWeatherData;
begin
  Data.Timestamp := RTC_GetDateTime();
  Data.Temperature := BME280_ReadTemperature();
  Data.Humidity := BME280_ReadHumidity();
  Data.Pressure := BME280_ReadPressure();

  // Affichage LCD
  LCD_Clear();
  LCD_PrintLine(0, Format('%.1f C %.0f%%',
                         [Data.Temperature, Data.Humidity]));
  LCD_PrintLine(1, Format('%.0f hPa', [Data.Pressure]));

  // Sauvegarde SD
  LogToSD(Data);
end;

begin
  InitPeripherals();

  while True do
  begin
    ReadAndLogWeather();

    // Sleep 10 minutes
    SetAlarmWakeup(600);
    EnterSleepMode();
  end;
end.
```

### Projet 2 : Contrôleur CNC simple

**Matériel** :
- Microcontrôleur (Arduino Mega ou STM32)
- Drivers moteurs pas-à-pas (A4988)
- Fins de course (endstops)
- Interface série/USB

**Compétences** :
- Génération de signaux PWM précis
- Interruptions pour fins de course
- Parsing de G-code
- Synchronisation multi-axes

```pascal
program SimpleCNC;

const
  STEPS_PER_MM = 80;  // Calibration

type
  TAxis = (axisX, axisY, axisZ);

var
  CurrentPos: array[TAxis] of LongInt;
  TargetPos: array[TAxis] of LongInt;

procedure StepMotor(Axis: TAxis; Direction: Boolean);  
begin
  case Axis of
    axisX:
    begin
      GPIO_Write(DIR_X, Direction);
      GPIO_Pulse(STEP_X, 2);  // 2µs pulse
    end;
    axisY:
    begin
      GPIO_Write(DIR_Y, Direction);
      GPIO_Pulse(STEP_Y, 2);
    end;
    axisZ:
    begin
      GPIO_Write(DIR_Z, Direction);
      GPIO_Pulse(STEP_Z, 2);
    end;
  end;

  if Direction then
    Inc(CurrentPos[Axis])
  else
    Dec(CurrentPos[Axis]);
end;

procedure MoveLinear(X, Y, Z: Real; FeedRate: Real);  
var
  StepsX, StepsY, StepsZ: LongInt;
  TotalSteps, Step: LongInt;
  DelayUs: LongWord;
begin
  // Convertir en pas
  StepsX := Round(X * STEPS_PER_MM) - CurrentPos[axisX];
  StepsY := Round(Y * STEPS_PER_MM) - CurrentPos[axisY];
  StepsZ := Round(Z * STEPS_PER_MM) - CurrentPos[axisZ];

  TotalSteps := Max(Abs(StepsX), Max(Abs(StepsY), Abs(StepsZ)));

  // Calcul vitesse
  DelayUs := Round((60000000.0 / FeedRate) / STEPS_PER_MM);

  // Interpolation linéaire (Bresenham 3D)
  for Step := 0 to TotalSteps do
  begin
    if (Step * Abs(StepsX)) div TotalSteps >
       ((Step - 1) * Abs(StepsX)) div TotalSteps then
      StepMotor(axisX, StepsX > 0);

    if (Step * Abs(StepsY)) div TotalSteps >
       ((Step - 1) * Abs(StepsY)) div TotalSteps then
      StepMotor(axisY, StepsY > 0);

    if (Step * Abs(StepsZ)) div TotalSteps >
       ((Step - 1) * Abs(StepsZ)) div TotalSteps then
      StepMotor(axisZ, StepsZ > 0);

    DelayMicroseconds(DelayUs);
  end;
end;

procedure ParseGCode(Line: string);  
var
  X, Y, Z, F: Real;
begin
  if Pos('G0', Line) = 1 then  // Rapid move
  begin
    ExtractCoordinates(Line, X, Y, Z, F);
    MoveLinear(X, Y, Z, 3000);  // Rapid = 3000 mm/min
  end
  else if Pos('G1', Line) = 1 then  // Linear move
  begin
    ExtractCoordinates(Line, X, Y, Z, F);
    if F = 0 then F := 1000;  // Feed par défaut
    MoveLinear(X, Y, Z, F);
  end;
  // ... autres commandes G-code
end;

begin
  InitMotors();
  HomeAllAxes();  // Recherche origine avec endstops

  while True do
  begin
    if UART_DataAvailable() then
    begin
      GCodeLine := UART_ReadLine();
      ParseGCode(GCodeLine);
      UART_SendString('ok'#13#10);
    end;
  end;
end.
```

### Projet 3 : Enregistreur de données haute vitesse

**Matériel** :
- STM32F4 (168 MHz, DMA)
- ADC rapide externe (via SPI)
- SRAM externe ou carte SD rapide
- Interface USB pour téléchargement

**Compétences** :
- DMA pour acquisition sans CPU
- Double buffering
- Gestion mémoire optimale
- USB Mass Storage

```pascal
program HighSpeedLogger;

const
  BUFFER_SIZE = 4096;
  SAMPLE_RATE = 100000;  // 100 kHz

var
  Buffer1, Buffer2: array[0..BUFFER_SIZE-1] of Word;
  ActiveBuffer: PWord;
  IsBuffer1Active: Boolean;
  SamplesCollected: LongWord;

procedure DMA_ADC_Complete_IRQHandler; interrupt;  
begin
  // Buffer plein, switcher
  if IsBuffer1Active then
  begin
    ActiveBuffer := @Buffer2;
    IsBuffer1Active := False;

    // Traiter Buffer1 en background
    ProcessBuffer(@Buffer1, BUFFER_SIZE);
  end
  else
  begin
    ActiveBuffer := @Buffer1;
    IsBuffer1Active := True;

    ProcessBuffer(@Buffer2, BUFFER_SIZE);
  end;

  Inc(SamplesCollected, BUFFER_SIZE);

  // Clear interrupt flag
  DMA1^.LISR := DMA1^.LISR;
end;

procedure ProcessBuffer(Buf: PWord; Size: Integer);  
var
  i: Integer;
  Min, Max, Avg: Word;
  Sum: LongWord;
begin
  Min := 65535;
  Max := 0;
  Sum := 0;

  // Analyse statistique
  for i := 0 to Size - 1 do
  begin
    if Buf[i] < Min then Min := Buf[i];
    if Buf[i] > Max then Max := Buf[i];
    Sum := Sum + Buf[i];
  end;

  Avg := Sum div Size;

  // Sauvegarder sur SD (asynchrone)
  SDCard_WriteBufferAsync(Buf, Size * 2);

  // Envoyer stats par UART
  UART_Printf('Min:%u Max:%u Avg:%u Samples:%u'#13#10,
              Min, Max, Avg, SamplesCollected);
end;

procedure ConfigureHighSpeedADC;  
begin
  // ADC en mode continu
  ADC1^.CR2 := ADC1^.CR2 or ADC_CR2_CONT;

  // DMA en mode circulaire
  ConfigureDMA_ADC_to_Memory(Buffer1);
  DMA1_Channel1^.CCR := DMA1_Channel1^.CCR or DMA_CCR_CIRC;

  // Interruption à chaque buffer plein
  DMA1_Channel1^.CCR := DMA1_Channel1^.CCR or DMA_CCR_TCIE;
  EnableIRQ(DMA1_Channel1_IRQn, 0);  // Priorité max

  // Timer pour cadencer l'ADC
  ConfigureTimer_ADC_Trigger(SAMPLE_RATE);
end;

begin
  InitPeripherals();
  ConfigureHighSpeedADC();

  ActiveBuffer := @Buffer1;
  IsBuffer1Active := True;

  UART_SendString('High-speed logger ready'#13#10);
  UART_Printf('Sample rate: %u Hz'#13#10, SAMPLE_RATE);

  // Démarrer acquisition
  ADC1^.CR2 := ADC1^.CR2 or ADC_CR2_ADON;
  TIM2^.CR1 := TIM2^.CR1 or TIM_CR1_CEN;

  // Boucle principale légère
  while True do
  begin
    // Le vrai travail est fait par DMA et interruptions
    // CPU peut gérer interface utilisateur, USB, etc.

    if USB_CommandReceived() then
      HandleUSBCommand();

    WaitForInterrupt();  // Économie d'énergie
  end;
end.
```

## Debugging avancé

### Utilisation d'OpenOCD

OpenOCD (Open On-Chip Debugger) permet le debugging via JTAG/SWD.

**Configuration OpenOCD pour STM32** :

```bash
# Fichier: openocd.cfg
source [find interface/stlink.cfg]  
source [find target/stm32f1x.cfg]

# Activer semihosting (printf vers console PC)
arm semihosting enable
```

**Lancement** :
```bash
openocd -f openocd.cfg
```

**Dans FreePascal** :
```pascal
// Semihosting pour debug printf
procedure Debug_Print(s: string);  
var
  Args: array[0..2] of LongWord;
begin
  Args[0] := 1;  // STDOUT
  Args[1] := LongWord(@s[1]);
  Args[2] := Length(s);

  asm
    mov r0, #0x04  // SYS_WRITE
    ldr r1, Args
    bkpt #0xAB
  end;
end;

begin
  Debug_Print('Hello from semihosting!'#13#10);
end;
```

### Analyseur logique logiciel

Utiliser GPIO pour débugger le timing.

```pascal
var
  DebugPin: Byte = 12;

procedure DEBUG_PulseStart; inline;  
begin
  GPIO_Set(DebugPin);
end;

procedure DEBUG_PulseEnd; inline;  
begin
  GPIO_Clear(DebugPin);
end;

// Utilisation
procedure FonctionAChronometre;  
begin
  DEBUG_PulseStart;

  // Code à analyser
  CalculComplexe();

  DEBUG_PulseEnd;
  // Mesurer avec oscilloscope ou analyseur logique
end;
```

### Assertion matérielle

```pascal
procedure HardwareAssert(Condition: Boolean; Msg: string);  
begin
  if not Condition then
  begin
    // Désactiver interruptions
    DisableInterrupts();

    // Afficher erreur
    UART_SendString('ASSERT FAILED: ' + Msg + #13#10);

    // LED rouge fixe
    GPIO_Set(LED_ERROR);

    // Arrêt total
    while True do
      asm wfi end;  // Wait For Interrupt (bloqué)
  end;
end;

// Utilisation
procedure ConfigurerPeriph;  
begin
  HardwareAssert(RCC^.CR and RCC_CR_HSERDY <> 0,
                 'HSE not ready');

  // Suite de la configuration...
end;
```

## Sécurité et robustesse

### Protection contre les erreurs matérielles

```pascal
type
  THardwareError = (
    heNone,
    heOvercurrent,
    heOvertemperature,
    heUnderVoltage,
    heWatchdogReset,
    heCommunicationLost
  );

var
  LastError: THardwareError = heNone;

procedure CheckHardwareHealth;  
var
  Voltage, Current, Temperature: Real;
begin
  // Surveillance tension
  Voltage := ADC_ReadVoltage(CH_VSUPPLY);
  if Voltage < 3.0 then
  begin
    LastError := heUnderVoltage;
    EnterSafeMode();
  end;

  // Surveillance courant
  Current := ADC_ReadCurrent(CH_CURRENT);
  if Current > 2.0 then
  begin
    LastError := heOvercurrent;
    DisableOutputs();
    EnterSafeMode();
  end;

  // Surveillance température
  Temperature := ADC_ReadTemperature();
  if Temperature > 85.0 then
  begin
    LastError := heOvertemperature;
    ReducePerformance();
  end;
end;

procedure EnterSafeMode;  
begin
  // Désactiver sorties dangereuses
  DisableMotors();
  DisableHeaters();

  // LED d'erreur
  while True do
  begin
    GPIO_Toggle(LED_ERROR);
    Delay(200);
  end;
end;
```

### Redondance et récupération

```pascal
const
  MAX_RETRIES = 3;
  RETRY_DELAY = 100;

function CommunicationRobuste(Commande: Byte): Boolean;  
var
  Retry: Integer;
  Response: Byte;
begin
  for Retry := 1 to MAX_RETRIES do
  begin
    // Envoyer commande
    SPI_SendByte(Commande);

    // Attendre réponse avec timeout
    if WaitForSPIResponse(Response, 100) then
    begin
      if Response = ACK then
        Exit(True);  // Succès
    end;

    // Échec, réessayer
    Delay(RETRY_DELAY);
  end;

  // Échec définitif après tentatives
  LastError := heCommunicationLost;
  Result := False;
end;
```

## Optimisation finale

### Code critique en assembleur

```pascal
// Boucle ultra-rapide pour bit-banging
procedure FastBitBang(Data: Byte); assembler; nostackframe;  
asm
  // r0 contient Data
  mov r1, #8        // Compteur de bits

loop:
  lsls r0, r0, #1   // Shift left, bit dans Carry
  bcs send_one      // Branch if Carry Set

send_zero:
  // Envoyer 0
  ldr r2, =GPIOA_BSRR
  mov r3, #(1 shl 16)  // Reset bit 0
  str r3, [r2]
  b continue

send_one:
  // Envoyer 1
  ldr r2, =GPIOA_BSRR
  mov r3, #1           // Set bit 0
  str r3, [r2]

continue:
  // Délai précis (3 cycles)
  nop
  nop
  nop

  subs r1, #1
  bne loop
end;
```

### Utilisation de la FPU matérielle

```pascal
{$IFDEF CPUARMV7M}
procedure EnableFPU;  
const
  CPACR = $E000ED88;
var
  CPACRReg: ^LongWord;
begin
  CPACRReg := Pointer(CPACR);
  // Activer CP10 et CP11 (coprocesseurs FPU)
  CPACRReg^ := CPACRReg^ or ($F shl 20);
end;
{$ENDIF}

// Calculs flottants accélérés
function FastSqrt(x: Single): Single; inline;  
begin
  asm
    vsqrt.f32 s0, s0  // Instruction FPU matérielle
  end;
  Result := x;  // Compilateur optimise
end;
```

## Certification et normes

Pour les applications critiques (médical, automobile, aéronautique) :

### Normes importantes

- **ISO 26262** : Automotive (sécurité fonctionnelle)
- **DO-178C** : Avionique (logiciel embarqué)
- **IEC 62304** : Dispositifs médicaux
- **IEC 61508** : Sécurité fonctionnelle générale

### Pratiques de développement certifiable

```pascal
// Documentation exhaustive
{
  Module: Motor_Control
  Purpose: PWM control for DC motors
  Safety: SIL-2 certified
  Version: 1.2.3
  Author: John Doe
  Date: 2025-01-15

  FMEA Analysis:
  - Overcurrent → Shutdown within 10ms
  - Sensor failure → Safe state (motor off)
  - Communication loss → Watchdog reset
}

// Traçabilité complète
procedure SetMotorSpeed(Speed: Word); // REQ-MOT-001  
var
  PWMValue: Word;
begin
  // SAFETY-CHECK: Limite vitesse maximale
  if Speed > MAX_SAFE_SPEED then  // REQ-MOT-002
    Speed := MAX_SAFE_SPEED;

  // LOG: Enregistrer changement
  LogMotorCommand(Speed);  // REQ-LOG-001

  PWMValue := (Speed * PWM_MAX) div 100;
  TIM2^.CCR1 := PWMValue;
end;
```

## Ressources finales

### Livres recommandés

- **"The Definitive Guide to ARM Cortex-M"** - Joseph Yiu
- **"Mastering STM32"** - Carmine Noviello
- **"Making Embedded Systems"** - Elecia White
- **"Programming Embedded Systems"** - Michael Barr

### Sites web et communautés

- **FreePascal Embedded Wiki** : wiki.freepascal.org/embedded
- **Stack Overflow** : Tag [freepascal] + [embedded]
- **Forum Lazarus** : Section "Embedded"
- **GitHub** : Rechercher "freepascal embedded"

### Cours en ligne

- **edX** : Embedded Systems - Shape the World
- **Coursera** : Introduction to Embedded Systems
- **YouTube** : Chaînes spécialisées STM32, ARM

## Glossaire

**ADC** : Analog-to-Digital Converter - Convertisseur analogique-numérique

**BKPT** : Breakpoint - Point d'arrêt matériel

**DMA** : Direct Memory Access - Accès mémoire direct

**GPIO** : General Purpose Input/Output - Entrées/sorties génériques

**I2C** : Inter-Integrated Circuit - Bus de communication série

**ISR** : Interrupt Service Routine - Routine de service d'interruption

**JTAG** : Joint Test Action Group - Interface de test et debug

**MMIO** : Memory-Mapped I/O - Entrées/sorties mappées en mémoire

**NVIC** : Nested Vectored Interrupt Controller - Contrôleur d'interruptions

**PWM** : Pulse Width Modulation - Modulation de largeur d'impulsion

**SPI** : Serial Peripheral Interface - Interface périphérique série

**SWD** : Serial Wire Debug - Interface de débogage série

**UART** : Universal Asynchronous Receiver-Transmitter - Émetteur-récepteur asynchrone

**Watchdog** : Chien de garde - Timer de surveillance système

---

## Récapitulatif final

L'accès matériel direct avec FreePascal est un domaine technique mais extrêmement gratifiant. Vous avez maintenant les connaissances pour :

✓ Comprendre l'architecture matérielle (registres, MMIO, interruptions)  
✓ Programmer des microcontrôleurs (GPIO, UART, ADC, PWM, Timers)  
✓ Accéder au matériel sur PC (Windows via drivers, Linux via sysfs)  
✓ Utiliser les bus de communication (I2C, SPI, USB)  
✓ Gérer les interruptions et le DMA  
✓ Optimiser et déboguer le code embarqué  
✓ Créer des projets complets robustes et sûrs

**Conseils pour progresser** :

1. **Commencez simple** : LED qui clignote
2. **Construisez progressivement** : ajoutez des fonctionnalités une par une
3. **Testez systématiquement** : ne supposez jamais, vérifiez toujours
4. **Documentez** : votre vous-même futur vous remerciera
5. **Partagez** : contribuez à la communauté FreePascal embedded

Bon développement matériel avec FreePascal ! 🚀

⏭️ [Optimisation mémoire pour embarqué](/14-systemes-embarques-iot/10-optimisation-memoire-embarque.md)
