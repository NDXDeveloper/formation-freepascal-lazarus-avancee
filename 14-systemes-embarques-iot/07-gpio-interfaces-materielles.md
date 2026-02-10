🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.7 GPIO et interfaces matérielles

## Introduction

GPIO (General Purpose Input/Output) désigne les broches d'entrée/sortie à usage général présentes sur les ordinateurs monocartes comme le Raspberry Pi, les microcontrôleurs, et certains systèmes embarqués. Ces broches permettent de contrôler directement du matériel électronique : allumer des LED, lire des capteurs, contrôler des moteurs, etc.

Dans ce chapitre, nous allons apprendre à utiliser les GPIO avec FreePascal/Lazarus, principalement sous Linux (Raspberry Pi, Orange Pi, etc.) où ces fonctionnalités sont les plus accessibles, mais aussi sous Windows pour certains cas d'usage.

## 1. Concepts fondamentaux des GPIO

### 1.1 Qu'est-ce qu'un GPIO ?

Un GPIO est une broche électronique programmable qui peut être configurée comme :
- **Entrée (Input)** : Pour lire l'état d'un signal (bouton pressé, capteur actif)
- **Sortie (Output)** : Pour envoyer un signal (allumer une LED, activer un relais)

**Caractéristiques importantes :**
- **Tension** : Généralement 3.3V (Raspberry Pi) ou 5V (Arduino)
- **Courant** : Limité (quelques mA à quelques dizaines de mA par broche)
- **Numérotation** : Deux systèmes (BCM et physique sur Raspberry Pi)

### 1.2 États d'un GPIO

Un GPIO peut avoir deux états logiques :
- **HIGH (1)** : Tension présente (3.3V ou 5V selon le système)
- **LOW (0)** : Pas de tension (0V, masse)

**Modes de configuration :**
- **Output** : La broche génère un signal
- **Input** : La broche lit un signal externe
- **Pull-up** : Résistance interne tirant vers HIGH
- **Pull-down** : Résistance interne tirant vers LOW

### 1.3 Sécurité et précautions

⚠️ **ATTENTION : Les GPIO sont sensibles !**

- Ne jamais dépasser la tension maximale (3.3V ou 5V)
- Ne jamais tirer trop de courant (risque de griller la broche)
- Toujours utiliser des résistances de limitation
- Ne jamais court-circuiter deux broches
- Vérifier la documentation avant de connecter du matériel

## 2. Accès aux GPIO sous Linux (Raspberry Pi)

### 2.1 Architecture GPIO sous Linux

Sous Linux, l'accès aux GPIO se fait principalement via deux méthodes :

**1. Système de fichiers /sys/class/gpio (ancien, simple)**
```
/sys/class/gpio/
├── export          # Activer un GPIO
├── unexport        # Désactiver un GPIO
├── gpio17/         # GPIO numéro 17
│   ├── direction   # input ou output
│   ├── value       # 0 ou 1
│   └── edge        # rising, falling, both
```

**2. Interface /dev/gpiochip (moderne, recommandée)**
```
/dev/gpiochip0      # Contrôleur GPIO principal
/dev/gpiochip1      # Contrôleur GPIO secondaire (si présent)
```

### 2.2 Méthode simple : sysfs

Voici une implémentation basique utilisant le système de fichiers :

```pascal
unit SimpleGPIO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGPIODirection = (gdInput, gdOutput);
  TGPIOValue = (gvLow, gvHigh);
  TGPIOEdge = (geNone, geRising, geFalling, geBoth);

  { TSimpleGPIO }
  TSimpleGPIO = class
  private
    FPinNumber: Integer;
    FExported: Boolean;
    FDirection: TGPIODirection;
    function GetGPIOPath: string;
    function GetValuePath: string;
    function GetDirectionPath: string;
    function GetEdgePath: string;
    procedure WriteToFile(const FileName, Value: string);
    function ReadFromFile(const FileName: string): string;
  public
    constructor Create(PinNumber: Integer);
    destructor Destroy; override;

    procedure Export;
    procedure Unexport;
    procedure SetDirection(Direction: TGPIODirection);
    procedure SetValue(Value: TGPIOValue);
    function GetValue: TGPIOValue;
    procedure SetEdge(Edge: TGPIOEdge);

    property PinNumber: Integer read FPinNumber;
    property Exported: Boolean read FExported;
  end;

implementation

const
  GPIO_BASE_PATH = '/sys/class/gpio/';

{ TSimpleGPIO }

constructor TSimpleGPIO.Create(PinNumber: Integer);  
begin
  FPinNumber := PinNumber;
  FExported := False;
  FDirection := gdInput;
end;

destructor TSimpleGPIO.Destroy;  
begin
  if FExported then
    Unexport;
  inherited Destroy;
end;

function TSimpleGPIO.GetGPIOPath: string;  
begin
  Result := GPIO_BASE_PATH + 'gpio' + IntToStr(FPinNumber) + '/';
end;

function TSimpleGPIO.GetValuePath: string;  
begin
  Result := GetGPIOPath + 'value';
end;

function TSimpleGPIO.GetDirectionPath: string;  
begin
  Result := GetGPIOPath + 'direction';
end;

function TSimpleGPIO.GetEdgePath: string;  
begin
  Result := GetGPIOPath + 'edge';
end;

procedure TSimpleGPIO.WriteToFile(const FileName, Value: string);  
var
  F: TextFile;
begin
  try
    AssignFile(F, FileName);
    Rewrite(F);
    Write(F, Value);
    CloseFile(F);
  except
    on E: Exception do
      raise Exception.CreateFmt('Erreur écriture dans %s: %s', [FileName, E.Message]);
  end;
end;

function TSimpleGPIO.ReadFromFile(const FileName: string): string;  
var
  F: TextFile;
  Line: string;
begin
  Result := '';
  try
    AssignFile(F, FileName);
    Reset(F);
    ReadLn(F, Line);
    Result := Trim(Line);
    CloseFile(F);
  except
    on E: Exception do
      raise Exception.CreateFmt('Erreur lecture de %s: %s', [FileName, E.Message]);
  end;
end;

procedure TSimpleGPIO.Export;  
begin
  if FExported then
    Exit;

  try
    WriteToFile(GPIO_BASE_PATH + 'export', IntToStr(FPinNumber));
    FExported := True;

    // Attendre que le système crée les fichiers
    Sleep(100);

    WriteLn(Format('GPIO %d exporté', [FPinNumber]));
  except
    on E: Exception do
      WriteLn(Format('Erreur export GPIO %d: %s', [FPinNumber, E.Message]));
  end;
end;

procedure TSimpleGPIO.Unexport;  
begin
  if not FExported then
    Exit;

  try
    WriteToFile(GPIO_BASE_PATH + 'unexport', IntToStr(FPinNumber));
    FExported := False;
    WriteLn(Format('GPIO %d libéré', [FPinNumber]));
  except
    on E: Exception do
      WriteLn(Format('Erreur unexport GPIO %d: %s', [FPinNumber, E.Message]));
  end;
end;

procedure TSimpleGPIO.SetDirection(Direction: TGPIODirection);  
var
  DirStr: string;
begin
  if not FExported then
    raise Exception.Create('GPIO non exporté');

  if Direction = gdInput then
    DirStr := 'in'
  else
    DirStr := 'out';

  WriteToFile(GetDirectionPath, DirStr);
  FDirection := Direction;
  WriteLn(Format('GPIO %d configuré en %s', [FPinNumber, DirStr]));
end;

procedure TSimpleGPIO.SetValue(Value: TGPIOValue);  
var
  ValStr: string;
begin
  if not FExported then
    raise Exception.Create('GPIO non exporté');

  if FDirection <> gdOutput then
    raise Exception.Create('GPIO non configuré en sortie');

  if Value = gvLow then
    ValStr := '0'
  else
    ValStr := '1';

  WriteToFile(GetValuePath, ValStr);
end;

function TSimpleGPIO.GetValue: TGPIOValue;  
var
  ValStr: string;
begin
  if not FExported then
    raise Exception.Create('GPIO non exporté');

  ValStr := ReadFromFile(GetValuePath);

  if ValStr = '1' then
    Result := gvHigh
  else
    Result := gvLow;
end;

procedure TSimpleGPIO.SetEdge(Edge: TGPIOEdge);  
var
  EdgeStr: string;
begin
  if not FExported then
    raise Exception.Create('GPIO non exporté');

  case Edge of
    geNone: EdgeStr := 'none';
    geRising: EdgeStr := 'rising';
    geFalling: EdgeStr := 'falling';
    geBoth: EdgeStr := 'both';
  end;

  WriteToFile(GetEdgePath, EdgeStr);
  WriteLn(Format('GPIO %d edge configuré: %s', [FPinNumber, EdgeStr]));
end;

end.
```

### 2.3 Exemple pratique : Clignotement d'une LED

```pascal
program BlinkLED;

{$mode objfpc}{$H+}

uses
  SysUtils, SimpleGPIO;

var
  LED: TSimpleGPIO;
  i: Integer;

begin
  WriteLn('=== Clignotement LED ===');
  WriteLn('LED connectée sur GPIO 17 (broche physique 11)');
  WriteLn;

  // Créer l''objet GPIO pour la broche 17
  LED := TSimpleGPIO.Create(17);

  try
    // Exporter et configurer en sortie
    LED.Export;
    LED.SetDirection(gdOutput);

    WriteLn('Clignotement 10 fois...');
    WriteLn('Ctrl+C pour arrêter');
    WriteLn;

    // Faire clignoter 10 fois
    for i := 1 to 10 do
    begin
      WriteLn(Format('[%d/10] LED ON', [i]));
      LED.SetValue(gvHigh);
      Sleep(500); // 500ms allumée

      WriteLn(Format('[%d/10] LED OFF', [i]));
      LED.SetValue(gvLow);
      Sleep(500); // 500ms éteinte
    end;

    WriteLn;
    WriteLn('Terminé !');

  finally
    // Toujours libérer le GPIO
    LED.Free;
  end;
end.
```

**Schéma de connexion :**
```
Raspberry Pi         LED  
GPIO 17 (broche 11) ----[Résistance 220Ω]----(+)LED(-)---- GND (broche 6)
```

### 2.4 Exemple : Lecture d'un bouton

```pascal
program ReadButton;

{$mode objfpc}{$H+}

uses
  SysUtils, SimpleGPIO;

var
  Button: TSimpleGPIO;
  LED: TSimpleGPIO;
  ButtonState: TGPIOValue;
  PreviousState: TGPIOValue;

begin
  WriteLn('=== Lecture de bouton ===');
  WriteLn('Bouton sur GPIO 27 (broche physique 13)');
  WriteLn('LED sur GPIO 17 (broche physique 11)');
  WriteLn;

  Button := TSimpleGPIO.Create(27);
  LED := TSimpleGPIO.Create(17);

  try
    // Configurer le bouton en entrée avec pull-up
    Button.Export;
    Button.SetDirection(gdInput);

    // Configurer la LED en sortie
    LED.Export;
    LED.SetDirection(gdOutput);

    WriteLn('Appuyez sur le bouton pour allumer la LED');
    WriteLn('Ctrl+C pour arrêter');
    WriteLn;

    PreviousState := gvHigh;

    // Boucle de lecture
    while True do
    begin
      ButtonState := Button.GetValue;

      // Détecter un changement d''état
      if ButtonState <> PreviousState then
      begin
        if ButtonState = gvLow then
        begin
          WriteLn('Bouton PRESSÉ - LED ON');
          LED.SetValue(gvHigh);
        end
        else
        begin
          WriteLn('Bouton RELÂCHÉ - LED OFF');
          LED.SetValue(gvLow);
        end;

        PreviousState := ButtonState;
      end;

      Sleep(10); // Petit délai pour éviter de saturer le CPU
    end;

  finally
    Button.Free;
    LED.Free;
  end;
end.
```

**Schéma de connexion du bouton :**
```
Raspberry Pi                    Bouton  
GPIO 27 (broche 13) -----------o----o---------- GND (broche 14)
                                Bouton poussoir
```

## 3. PWM (Pulse Width Modulation)

### 3.1 Principe du PWM

Le PWM (Modulation de Largeur d'Impulsion) permet de simuler un signal analogique en variant rapidement entre HIGH et LOW. C'est utile pour :
- Varier la luminosité d'une LED
- Contrôler la vitesse d'un moteur
- Générer des signaux audio simples

**Paramètres du PWM :**
- **Fréquence** : Nombre de cycles par seconde (Hz)
- **Duty Cycle** : Pourcentage du temps à HIGH (0-100%)

```
Duty Cycle 25%:  ▁▁▁▁▔▁▁▁▁▔▁▁▁▁▔  
Duty Cycle 50%:  ▁▁▔▔▁▁▔▔▁▁▔▔▁▁▔▔  
Duty Cycle 75%:  ▁▔▔▔▁▔▔▔▁▔▔▔▁▔▔▔
```

### 3.2 PWM logiciel

```pascal
unit SoftwarePWM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleGPIO;

type
  { TPWMController }
  TPWMController = class
  private
    FGPIO: TSimpleGPIO;
    FThread: TThread;
    FFrequency: Integer;
    FDutyCycle: Integer; // 0-100
    FRunning: Boolean;
    procedure PWMLoop;
  public
    constructor Create(PinNumber: Integer);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure SetDutyCycle(Value: Integer);
    procedure SetFrequency(Value: Integer);
    property DutyCycle: Integer read FDutyCycle write SetDutyCycle;
    property Frequency: Integer read FFrequency write SetFrequency;
    property Running: Boolean read FRunning;
  end;

implementation

{ TPWMController }

constructor TPWMController.Create(PinNumber: Integer);  
begin
  FGPIO := TSimpleGPIO.Create(PinNumber);
  FGPIO.Export;
  FGPIO.SetDirection(gdOutput);

  FFrequency := 1000; // 1 kHz par défaut
  FDutyCycle := 50;   // 50% par défaut
  FRunning := False;
end;

destructor TPWMController.Destroy;  
begin
  Stop;
  FGPIO.Free;
  inherited Destroy;
end;

procedure TPWMController.PWMLoop;  
var
  PeriodMicros: Integer;
  HighMicros: Integer;
  LowMicros: Integer;
begin
  // Calculer les durées en microsecondes
  PeriodMicros := 1000000 div FFrequency;
  HighMicros := (PeriodMicros * FDutyCycle) div 100;
  LowMicros := PeriodMicros - HighMicros;

  while FRunning do
  begin
    if HighMicros > 0 then
    begin
      FGPIO.SetValue(gvHigh);
      // Note: Sleep fonctionne en millisecondes, pour plus de précision
      // il faudrait utiliser usleep ou une bibliothèque de timing
      if HighMicros >= 1000 then
        Sleep(HighMicros div 1000);
    end;

    if LowMicros > 0 then
    begin
      FGPIO.SetValue(gvLow);
      if LowMicros >= 1000 then
        Sleep(LowMicros div 1000);
    end;
  end;
end;

procedure TPWMController.Start;  
begin
  if FRunning then
    Exit;

  FRunning := True;

  FThread := TThread.CreateAnonymousThread(@PWMLoop);
  FThread.FreeOnTerminate := False;
  FThread.Start;

  WriteLn(Format('PWM démarré: %d Hz, %d%%', [FFrequency, FDutyCycle]));
end;

procedure TPWMController.Stop;  
begin
  if not FRunning then
    Exit;

  FRunning := False;

  if Assigned(FThread) then
  begin
    FThread.WaitFor;
    FThread.Free;
    FThread := nil;
  end;

  FGPIO.SetValue(gvLow);
  WriteLn('PWM arrêté');
end;

procedure TPWMController.SetDutyCycle(Value: Integer);  
begin
  if Value < 0 then
    Value := 0;
  if Value > 100 then
    Value := 100;

  FDutyCycle := Value;
  WriteLn(Format('Duty cycle: %d%%', [FDutyCycle]));
end;

procedure TPWMController.SetFrequency(Value: Integer);  
begin
  if Value < 1 then
    Value := 1;

  FFrequency := Value;
  WriteLn(Format('Fréquence: %d Hz', [FFrequency]));
end;

end.
```

### 3.3 Exemple : LED avec intensité variable

```pascal
program FadeLED;

{$mode objfpc}{$H+}

uses
  SysUtils, SoftwarePWM;

var
  PWM: TPWMController;
  Brightness: Integer;
  Direction: Integer;

begin
  WriteLn('=== LED avec effet de fondu ===');
  WriteLn('LED sur GPIO 17');
  WriteLn;

  PWM := TPWMController.Create(17);

  try
    PWM.Frequency := 1000; // 1 kHz
    PWM.Start;

    WriteLn('Effet de fondu pendant 30 secondes...');
    WriteLn('Ctrl+C pour arrêter');
    WriteLn;

    Brightness := 0;
    Direction := 1; // 1 = augmenter, -1 = diminuer

    // Boucle de variation progressive
    while True do
    begin
      PWM.DutyCycle := Brightness;

      // Changer la direction aux extrêmes
      if Brightness >= 100 then
        Direction := -1
      else if Brightness <= 0 then
        Direction := 1;

      Brightness := Brightness + (Direction * 5);

      Sleep(100); // Délai entre les changements
    end;

  finally
    PWM.Free;
  end;
end.
```

## 4. PWM matériel (Raspberry Pi)

Le Raspberry Pi possède des broches PWM matérielles qui offrent de meilleures performances :

**Broches PWM matérielles :**
- GPIO 12 (broche physique 32) - PWM0
- GPIO 13 (broche physique 33) - PWM1
- GPIO 18 (broche physique 12) - PWM0
- GPIO 19 (broche physique 35) - PWM1

```pascal
unit HardwarePWM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { THardwarePWM }
  THardwarePWM = class
  private
    FChip: Integer;
    FChannel: Integer;
    FEnabled: Boolean;
    function GetBasePath: string;
    procedure WriteValue(const FileName, Value: string);
    function ReadValue(const FileName: string): string;
  public
    constructor Create(Chip, Channel: Integer);
    destructor Destroy; override;
    procedure Export;
    procedure Unexport;
    procedure SetPeriod(NanoSeconds: Int64);
    procedure SetDutyCycle(NanoSeconds: Int64);
    procedure Enable;
    procedure Disable;
    procedure SetPercentage(Percent: Double; FrequencyHz: Integer = 1000);
  end;

implementation

{ THardwarePWM }

constructor THardwarePWM.Create(Chip, Channel: Integer);  
begin
  FChip := Chip;
  FChannel := Channel;
  FEnabled := False;
end;

destructor THardwarePWM.Destroy;  
begin
  if FEnabled then
    Disable;
  Unexport;
  inherited Destroy;
end;

function THardwarePWM.GetBasePath: string;  
begin
  Result := Format('/sys/class/pwm/pwmchip%d/pwm%d/', [FChip, FChannel]);
end;

procedure THardwarePWM.WriteValue(const FileName, Value: string);  
var
  F: TextFile;
  FullPath: string;
begin
  FullPath := GetBasePath + FileName;
  try
    AssignFile(F, FullPath);
    Rewrite(F);
    Write(F, Value);
    CloseFile(F);
  except
    on E: Exception do
      raise Exception.CreateFmt('Erreur écriture %s: %s', [FullPath, E.Message]);
  end;
end;

function THardwarePWM.ReadValue(const FileName: string): string;  
var
  F: TextFile;
  FullPath: string;
  Line: string;
begin
  FullPath := GetBasePath + FileName;
  Result := '';
  try
    AssignFile(F, FullPath);
    Reset(F);
    ReadLn(F, Line);
    Result := Trim(Line);
    CloseFile(F);
  except
    on E: Exception do
      raise Exception.CreateFmt('Erreur lecture %s: %s', [FullPath, E.Message]);
  end;
end;

procedure THardwarePWM.Export;  
var
  F: TextFile;
  ExportPath: string;
begin
  ExportPath := Format('/sys/class/pwm/pwmchip%d/export', [FChip]);
  try
    AssignFile(F, ExportPath);
    Rewrite(F);
    Write(F, IntToStr(FChannel));
    CloseFile(F);
    Sleep(100); // Attendre la création des fichiers
    WriteLn(Format('PWM%d:%d exporté', [FChip, FChannel]));
  except
    // Peut échouer si déjà exporté
  end;
end;

procedure THardwarePWM.Unexport;  
var
  F: TextFile;
  UnexportPath: string;
begin
  UnexportPath := Format('/sys/class/pwm/pwmchip%d/unexport', [FChip]);
  try
    AssignFile(F, UnexportPath);
    Rewrite(F);
    Write(F, IntToStr(FChannel));
    CloseFile(F);
    WriteLn(Format('PWM%d:%d libéré', [FChip, FChannel]));
  except
    // Ignorer les erreurs
  end;
end;

procedure THardwarePWM.SetPeriod(NanoSeconds: Int64);  
begin
  WriteValue('period', IntToStr(NanoSeconds));
end;

procedure THardwarePWM.SetDutyCycle(NanoSeconds: Int64);  
begin
  WriteValue('duty_cycle', IntToStr(NanoSeconds));
end;

procedure THardwarePWM.Enable;  
begin
  WriteValue('enable', '1');
  FEnabled := True;
  WriteLn('PWM activé');
end;

procedure THardwarePWM.Disable;  
begin
  WriteValue('enable', '0');
  FEnabled := False;
  WriteLn('PWM désactivé');
end;

procedure THardwarePWM.SetPercentage(Percent: Double; FrequencyHz: Integer);  
var
  PeriodNS: Int64;
  DutyCycleNS: Int64;
begin
  // Limiter le pourcentage
  if Percent < 0 then
    Percent := 0;
  if Percent > 100 then
    Percent := 100;

  // Calculer la période en nanosecondes
  PeriodNS := 1000000000 div FrequencyHz;
  DutyCycleNS := Round((PeriodNS * Percent) / 100.0);

  SetPeriod(PeriodNS);
  SetDutyCycle(DutyCycleNS);

  WriteLn(Format('PWM: %.1f%% à %d Hz', [Percent, FrequencyHz]));
end;

end.
```

**Exemple d'utilisation du PWM matériel :**

```pascal
program HardwarePWMExample;

{$mode objfpc}{$H+}

uses
  SysUtils, HardwarePWM;

var
  PWM: THardwarePWM;
  i: Integer;

begin
  WriteLn('=== PWM Matériel ===');
  WriteLn('LED sur GPIO 18 (broche physique 12)');
  WriteLn;

  // Créer PWM sur chip 0, channel 0 (GPIO 18)
  PWM := THardwarePWM.Create(0, 0);

  try
    PWM.Export;
    PWM.Enable;

    WriteLn('Variation progressive de 0% à 100%...');

    // Augmenter progressivement de 0 à 100%
    for i := 0 to 100 do
    begin
      PWM.SetPercentage(i, 1000); // 1 kHz
      Sleep(50);
    end;

    WriteLn('Maintien à 100% pendant 2 secondes...');
    Sleep(2000);

    WriteLn('Diminution progressive de 100% à 0%...');

    // Diminuer progressivement de 100 à 0%
    for i := 100 downto 0 do
    begin
      PWM.SetPercentage(i, 1000);
      Sleep(50);
    end;

    WriteLn('Terminé !');

  finally
    PWM.Disable;
    PWM.Free;
  end;
end.
```

## 5. Communication SPI (Serial Peripheral Interface)

### 5.1 Principe du SPI

SPI est un protocole de communication série synchrone utilisé pour communiquer avec des périphériques comme :
- Capteurs (température, pression, accéléromètre)
- Afficheurs (LCD, OLED)
- Mémoires (EEPROM, Flash)
- Cartes SD

**Signaux SPI :**
- **MOSI** (Master Out Slave In) : Données du maître vers l'esclave
- **MISO** (Master In Slave Out) : Données de l'esclave vers le maître
- **SCLK** (Serial Clock) : Horloge générée par le maître
- **CS/SS** (Chip Select/Slave Select) : Sélection de l'esclave

**Sur Raspberry Pi :**
- SPI0: MOSI=GPIO10, MISO=GPIO9, SCLK=GPIO11, CE0=GPIO8, CE1=GPIO7
- SPI1: MOSI=GPIO20, MISO=GPIO19, SCLK=GPIO21, CE0=GPIO18

### 5.2 Activer SPI sur Raspberry Pi

```bash
# Activer SPI via raspi-config
sudo raspi-config
# Interface Options → SPI → Yes

# Ou éditer /boot/config.txt
sudo nano /boot/config.txt
# Ajouter: dtparam=spi=on

# Redémarrer
sudo reboot

# Vérifier que SPI est activé
ls /dev/spi*
# Devrait afficher: /dev/spidev0.0  /dev/spidev0.1
```

### 5.3 Bibliothèque SPI pour FreePascal

```pascal
unit SimpleSPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix;

const
  SPI_IOC_MAGIC = 'k';
  SPI_IOC_MESSAGE_1 = $40206b00;

type
  TSPI_IOC_Transfer = packed record
    tx_buf: QWord;
    rx_buf: QWord;
    len: LongWord;
    speed_hz: LongWord;
    delay_usecs: Word;
    bits_per_word: Byte;
    cs_change: Byte;
    pad: LongWord;
  end;

  { TSPIDevice }
  TSPIDevice = class
  private
    FDevice: string;
    FHandle: Integer;
    FSpeed: LongWord;
    FMode: Byte;
    FBitsPerWord: Byte;
    function OpenDevice: Boolean;
    procedure CloseDevice;
  public
    constructor Create(const DevicePath: string);
    destructor Destroy; override;
    function SetMode(Mode: Byte): Boolean;
    function SetSpeed(SpeedHz: LongWord): Boolean;
    function SetBitsPerWord(Bits: Byte): Boolean;
    function Transfer(const TxData: array of Byte; var RxData: array of Byte; Len: Integer): Boolean;
    function WriteByte(Value: Byte): Boolean;
    function ReadByte: Byte;
    function WriteBytes(const Data: array of Byte): Boolean;
    function ReadBytes(Count: Integer): TBytes;
    property Device: string read FDevice;
    property Speed: LongWord read FSpeed;
    property Mode: Byte read FMode;
  end;

implementation

uses
  Unix;

const
  SPI_IOC_WR_MODE = $40016B01;
  SPI_IOC_RD_MODE = $80016B01;
  SPI_IOC_WR_BITS_PER_WORD = $40016B03;
  SPI_IOC_RD_BITS_PER_WORD = $80016B03;
  SPI_IOC_WR_MAX_SPEED_HZ = $40046B04;
  SPI_IOC_RD_MAX_SPEED_HZ = $80046B04;

{ TSPIDevice }

constructor TSPIDevice.Create(const DevicePath: string);  
begin
  FDevice := DevicePath;
  FHandle := -1;
  FSpeed := 1000000; // 1 MHz par défaut
  FMode := 0;
  FBitsPerWord := 8;

  if not OpenDevice then
    raise Exception.CreateFmt('Impossible d''ouvrir le périphérique SPI: %s', [FDevice]);
end;

destructor TSPIDevice.Destroy;  
begin
  CloseDevice;
  inherited Destroy;
end;

function TSPIDevice.OpenDevice: Boolean;  
begin
  FHandle := FpOpen(FDevice, O_RDWR);
  Result := FHandle >= 0;

  if Result then
  begin
    WriteLn(Format('SPI ouvert: %s (fd=%d)', [FDevice, FHandle]));
    // Configurer avec les valeurs par défaut
    SetMode(FMode);
    SetSpeed(FSpeed);
    SetBitsPerWord(FBitsPerWord);
  end
  else
    WriteLn(Format('Erreur ouverture SPI: %s (errno=%d)', [FDevice, fpgeterrno]));
end;

procedure TSPIDevice.CloseDevice;  
begin
  if FHandle >= 0 then
  begin
    FpClose(FHandle);
    WriteLn(Format('SPI fermé: %s', [FDevice]));
    FHandle := -1;
  end;
end;

function TSPIDevice.SetMode(Mode: Byte): Boolean;  
begin
  Result := False;
  if FHandle < 0 then
    Exit;

  if FpIOCtl(FHandle, SPI_IOC_WR_MODE, @Mode) >= 0 then
  begin
    FMode := Mode;
    WriteLn(Format('SPI mode: %d', [FMode]));
    Result := True;
  end;
end;

function TSPIDevice.SetSpeed(SpeedHz: LongWord): Boolean;  
begin
  Result := False;
  if FHandle < 0 then
    Exit;

  if FpIOCtl(FHandle, SPI_IOC_WR_MAX_SPEED_HZ, @SpeedHz) >= 0 then
  begin
    FSpeed := SpeedHz;
    WriteLn(Format('SPI vitesse: %d Hz', [FSpeed]));
    Result := True;
  end;
end;

function TSPIDevice.SetBitsPerWord(Bits: Byte): Boolean;  
begin
  Result := False;
  if FHandle < 0 then
    Exit;

  if FpIOCtl(FHandle, SPI_IOC_WR_BITS_PER_WORD, @Bits) >= 0 then
  begin
    FBitsPerWord := Bits;
    WriteLn(Format('SPI bits par mot: %d', [FBitsPerWord]));
    Result := True;
  end;
end;

function TSPIDevice.Transfer(const TxData: array of Byte; var RxData: array of Byte;
  Len: Integer): Boolean;
var
  Transfer: TSPI_IOC_Transfer;
begin
  Result := False;
  if FHandle < 0 then
    Exit;

  FillChar(Transfer, SizeOf(Transfer), 0);
  Transfer.tx_buf := QWord(@TxData[0]);
  Transfer.rx_buf := QWord(@RxData[0]);
  Transfer.len := Len;
  Transfer.speed_hz := FSpeed;
  Transfer.bits_per_word := FBitsPerWord;

  Result := FpIOCtl(FHandle, SPI_IOC_MESSAGE_1, @Transfer) >= 0;
end;

function TSPIDevice.WriteByte(Value: Byte): Boolean;  
var
  TxData, RxData: array[0..0] of Byte;
begin
  TxData[0] := Value;
  Result := Transfer(TxData, RxData, 1);
end;

function TSPIDevice.ReadByte: Byte;  
var
  TxData, RxData: array[0..0] of Byte;
begin
  TxData[0] := $00; // Envoyer 0 pour recevoir
  if Transfer(TxData, RxData, 1) then
    Result := RxData[0]
  else
    Result := 0;
end;

function TSPIDevice.WriteBytes(const Data: array of Byte): Boolean;  
var
  RxData: array of Byte;
begin
  SetLength(RxData, Length(Data));
  Result := Transfer(Data, RxData, Length(Data));
end;

function TSPIDevice.ReadBytes(Count: Integer): TBytes;  
var
  TxData: array of Byte;
  i: Integer;
begin
  SetLength(Result, Count);
  SetLength(TxData, Count);

  // Remplir TxData avec des zéros
  for i := 0 to Count - 1 do
    TxData[i] := $00;

  Transfer(TxData, Result, Count);
end;

end.
```

### 5.4 Exemple : Lecture d'un capteur MCP3008 (ADC SPI)

Le MCP3008 est un convertisseur analogique-numérique 10 bits avec 8 canaux, communiquant via SPI.

```pascal
unit MCP3008;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleSPI;

type
  { TMCP3008 }
  TMCP3008 = class
  private
    FSPIDevice: TSPIDevice;
  public
    constructor Create(const SPIDevice: string = '/dev/spidev0.0');
    destructor Destroy; override;
    function ReadChannel(Channel: Byte): Word;
    function ReadVoltage(Channel: Byte; VRef: Double = 3.3): Double;
  end;

implementation

{ TMCP3008 }

constructor TMCP3008.Create(const SPIDevice: string);  
begin
  FSPIDevice := TSPIDevice.Create(SPIDevice);
  FSPIDevice.SetSpeed(1000000); // 1 MHz
  FSPIDevice.SetMode(0);
  WriteLn('MCP3008 ADC initialisé');
end;

destructor TMCP3008.Destroy;  
begin
  FSPIDevice.Free;
  inherited Destroy;
end;

function TMCP3008.ReadChannel(Channel: Byte): Word;  
var
  TxData, RxData: array[0..2] of Byte;
begin
  // Limiter le canal à 0-7
  if Channel > 7 then
    Channel := 7;

  // Construire la commande MCP3008
  // Format: 1 | SGL/DIFF | D2 | D1 | D0 | X | X | X
  TxData[0] := $01; // Start bit
  TxData[1] := ($80 or (Channel shl 4)); // Single-ended, canal
  TxData[2] := $00;

  // Transférer les données
  if FSPIDevice.Transfer(TxData, RxData, 3) then
  begin
    // Extraire la valeur 10 bits
    Result := ((RxData[1] and $03) shl 8) or RxData[2];
  end
  else
    Result := 0;
end;

function TMCP3008.ReadVoltage(Channel: Byte; VRef: Double): Double;  
var
  RawValue: Word;
begin
  RawValue := ReadChannel(Channel);
  // Convertir la valeur 10 bits (0-1023) en tension
  Result := (RawValue * VRef) / 1023.0;
end;

end.
```

**Programme d'exemple avec MCP3008 :**

```pascal
program TestMCP3008;

{$mode objfpc}{$H+}

uses
  SysUtils, MCP3008;

var
  ADC: TMCP3008;
  Channel: Integer;
  Value: Word;
  Voltage: Double;

begin
  WriteLn('=== Test MCP3008 ADC ===');
  WriteLn;

  ADC := TMCP3008.Create('/dev/spidev0.0');

  try
    WriteLn('Lecture des 8 canaux...');
    WriteLn;

    // Lire tous les canaux
    for Channel := 0 to 7 do
    begin
      Value := ADC.ReadChannel(Channel);
      Voltage := ADC.ReadVoltage(Channel, 3.3);

      WriteLn(Format('Canal %d: %4d (%.3f V)', [Channel, Value, Voltage]));
      Sleep(100);
    end;

    WriteLn;
    WriteLn('Lecture continue du canal 0...');
    WriteLn('Ctrl+C pour arrêter');
    WriteLn;

    // Lecture continue d'un canal
    while True do
    begin
      Voltage := ADC.ReadVoltage(0, 3.3);
      WriteLn(Format('Canal 0: %.3f V', [Voltage]));
      Sleep(500);
    end;

  finally
    ADC.Free;
  end;
end.
```

**Schéma de connexion MCP3008 :**
```
MCP3008          Raspberry Pi  
VDD  ----------- 3.3V (broche 1)  
VREF ----------- 3.3V  
AGND ----------- GND (broche 6)  
DGND ----------- GND  
CLK  ----------- SCLK (GPIO 11, broche 23)  
DOUT ----------- MISO (GPIO 9, broche 21)  
DIN  ----------- MOSI (GPIO 10, broche 19)  
CS   ----------- CE0 (GPIO 8, broche 24)

CH0-CH7: Entrées analogiques (0-3.3V)
```

## 6. Communication I²C (Inter-Integrated Circuit)

### 6.1 Principe de l'I²C

I²C est un bus de communication série synchrone bidirectionnel utilisant seulement 2 fils :
- **SDA** (Serial Data) : Données bidirectionnelles
- **SCL** (Serial Clock) : Horloge générée par le maître

**Avantages de l'I²C :**
- Seulement 2 fils pour communiquer avec plusieurs périphériques
- Adressage sur 7 bits = jusqu'à 127 périphériques sur le même bus
- Protocole standardisé et largement utilisé

**Sur Raspberry Pi :**
- I2C1: SDA=GPIO2 (broche 3), SCL=GPIO3 (broche 5)
- I2C0: Réservé à l'EEPROM (HAT)

### 6.2 Activer I²C sur Raspberry Pi

```bash
# Activer I²C via raspi-config
sudo raspi-config
# Interface Options → I2C → Yes

# Ou éditer /boot/config.txt
sudo nano /boot/config.txt
# Ajouter: dtparam=i2c_arm=on

# Installer les outils I²C
sudo apt-get install i2c-tools

# Redémarrer
sudo reboot

# Vérifier que I²C est activé
ls /dev/i2c*
# Devrait afficher: /dev/i2c-1

# Scanner le bus I²C pour détecter les périphériques
sudo i2cdetect -y 1
```

### 6.3 Bibliothèque I²C pour FreePascal

```pascal
unit SimpleI2C;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix;

const
  I2C_SLAVE = $0703;

type
  { TI2CDevice }
  TI2CDevice = class
  private
    FBusNumber: Integer;
    FAddress: Byte;
    FHandle: Integer;
    FDevice: string;
    function OpenDevice: Boolean;
    procedure CloseDevice;
  public
    constructor Create(BusNumber: Integer; Address: Byte);
    destructor Destroy; override;
    function WriteByte(Value: Byte): Boolean;
    function ReadByte: Byte;
    function WriteBytes(const Data: array of Byte): Boolean;
    function ReadBytes(Count: Integer): TBytes;
    function WriteRegister(Register: Byte; Value: Byte): Boolean;
    function ReadRegister(Register: Byte): Byte;
    function WriteRegisterWord(Register: Byte; Value: Word): Boolean;
    function ReadRegisterWord(Register: Byte): Word;
    property BusNumber: Integer read FBusNumber;
    property Address: Byte read FAddress;
  end;

implementation

uses
  Unix;

{ TI2CDevice }

constructor TI2CDevice.Create(BusNumber: Integer; Address: Byte);  
begin
  FBusNumber := BusNumber;
  FAddress := Address;
  FDevice := Format('/dev/i2c-%d', [FBusNumber]);
  FHandle := -1;

  if not OpenDevice then
    raise Exception.CreateFmt('Impossible d''ouvrir le bus I²C: %s', [FDevice]);
end;

destructor TI2CDevice.Destroy;  
begin
  CloseDevice;
  inherited Destroy;
end;

function TI2CDevice.OpenDevice: Boolean;  
begin
  FHandle := FpOpen(FDevice, O_RDWR);
  Result := FHandle >= 0;

  if Result then
  begin
    // Définir l'adresse du périphérique esclave
    if FpIOCtl(FHandle, I2C_SLAVE, Pointer(PtrUInt(FAddress))) < 0 then
    begin
      CloseDevice;
      Result := False;
      WriteLn(Format('Erreur définition adresse I²C 0x%0.2X', [FAddress]));
    end
    else
      WriteLn(Format('I²C ouvert: %s, adresse 0x%0.2X', [FDevice, FAddress]));
  end
  else
    WriteLn(Format('Erreur ouverture I²C: %s (errno=%d)', [FDevice, fpgeterrno]));
end;

procedure TI2CDevice.CloseDevice;  
begin
  if FHandle >= 0 then
  begin
    FpClose(FHandle);
    WriteLn(Format('I²C fermé: %s', [FDevice]));
    FHandle := -1;
  end;
end;

function TI2CDevice.WriteByte(Value: Byte): Boolean;  
begin
  Result := False;
  if FHandle < 0 then
    Exit;

  Result := FpWrite(FHandle, Value, 1) = 1;
end;

function TI2CDevice.ReadByte: Byte;  
begin
  Result := 0;
  if FHandle < 0 then
    Exit;

  if FpRead(FHandle, Result, 1) <> 1 then
    Result := 0;
end;

function TI2CDevice.WriteBytes(const Data: array of Byte): Boolean;  
begin
  Result := False;
  if FHandle < 0 then
    Exit;

  Result := FpWrite(FHandle, Data[0], Length(Data)) = Length(Data);
end;

function TI2CDevice.ReadBytes(Count: Integer): TBytes;  
var
  BytesRead: Integer;
begin
  SetLength(Result, Count);

  if FHandle < 0 then
    Exit;

  BytesRead := FpRead(FHandle, Result[0], Count);
  if BytesRead <> Count then
    SetLength(Result, 0);
end;

function TI2CDevice.WriteRegister(Register: Byte; Value: Byte): Boolean;  
var
  Data: array[0..1] of Byte;
begin
  Data[0] := Register;
  Data[1] := Value;
  Result := WriteBytes(Data);
end;

function TI2CDevice.ReadRegister(Register: Byte): Byte;  
begin
  Result := 0;

  // Écrire l'adresse du registre
  if not WriteByte(Register) then
    Exit;

  // Lire la valeur
  Result := ReadByte;
end;

function TI2CDevice.WriteRegisterWord(Register: Byte; Value: Word): Boolean;  
var
  Data: array[0..2] of Byte;
begin
  Data[0] := Register;
  Data[1] := Hi(Value); // MSB first (Big Endian)
  Data[2] := Lo(Value);
  Result := WriteBytes(Data);
end;

function TI2CDevice.ReadRegisterWord(Register: Byte): Word;  
var
  Data: TBytes;
begin
  Result := 0;

  // Écrire l'adresse du registre
  if not WriteByte(Register) then
    Exit;

  // Lire 2 octets
  Data := ReadBytes(2);
  if Length(Data) = 2 then
    Result := (Data[0] shl 8) or Data[1]; // Big Endian
end;

end.
```

### 6.4 Exemple : Capteur de température BMP280 (I²C)

Le BMP280 est un capteur de pression et température barométrique communiquant via I²C.

```pascal
unit BMP280;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleI2C;

const
  BMP280_ADDRESS = $76; // Ou $77 selon le module

  // Registres BMP280
  BMP280_REG_CHIP_ID = $D0;
  BMP280_REG_RESET = $E0;
  BMP280_REG_STATUS = $F3;
  BMP280_REG_CTRL_MEAS = $F4;
  BMP280_REG_CONFIG = $F5;
  BMP280_REG_PRESS_MSB = $F7;
  BMP280_REG_TEMP_MSB = $FA;

  // Valeurs
  BMP280_CHIP_ID = $58;

type
  { TBMP280 }
  TBMP280 = class
  private
    FI2C: TI2CDevice;
    FCalibrationRead: Boolean;
    // Paramètres de calibration
    dig_T1: Word;
    dig_T2, dig_T3: SmallInt;
    dig_P1: Word;
    dig_P2, dig_P3, dig_P4, dig_P5, dig_P6, dig_P7, dig_P8, dig_P9: SmallInt;
    procedure ReadCalibration;
    function CompensateTemperature(adc_T: LongInt): Double;
    function CompensatePressure(adc_P: LongInt): Double;
  public
    constructor Create(BusNumber: Integer = 1; Address: Byte = BMP280_ADDRESS);
    destructor Destroy; override;
    function Initialize: Boolean;
    function ReadTemperature: Double; // en °C
    function ReadPressure: Double; // en hPa
    function ReadAltitude(SeaLevelPressure: Double = 1013.25): Double; // en mètres
  end;

implementation

{ TBMP280 }

constructor TBMP280.Create(BusNumber: Integer; Address: Byte);  
begin
  FI2C := TI2CDevice.Create(BusNumber, Address);
  FCalibrationRead := False;
  WriteLn('BMP280 initialisé');
end;

destructor TBMP280.Destroy;  
begin
  FI2C.Free;
  inherited Destroy;
end;

function TBMP280.Initialize: Boolean;  
var
  ChipID: Byte;
begin
  Result := False;

  // Vérifier l'ID du chip
  ChipID := FI2C.ReadRegister(BMP280_REG_CHIP_ID);
  if ChipID <> BMP280_CHIP_ID then
  begin
    WriteLn(Format('Erreur: BMP280 non détecté (ID=0x%0.2X, attendu 0x%0.2X)',
                   [ChipID, BMP280_CHIP_ID]));
    Exit;
  end;

  WriteLn('BMP280 détecté');

  // Lire les paramètres de calibration
  ReadCalibration;

  // Configurer le capteur
  // Mode normal, oversampling température et pression x2
  FI2C.WriteRegister(BMP280_REG_CTRL_MEAS, $57); // 01010111

  // Filtre off, standby 0.5ms
  FI2C.WriteRegister(BMP280_REG_CONFIG, $00);

  Result := True;
  WriteLn('BMP280 configuré');
end;

procedure TBMP280.ReadCalibration;  
var
  i: Integer;
  CalibData: TBytes;
begin
  // Lire les 24 octets de calibration (registres 0x88-0x9F)
  FI2C.WriteByte($88);
  CalibData := FI2C.ReadBytes(24);

  if Length(CalibData) = 24 then
  begin
    // Extraire les paramètres de calibration
    dig_T1 := CalibData[0] or (CalibData[1] shl 8);
    dig_T2 := SmallInt(CalibData[2] or (CalibData[3] shl 8));
    dig_T3 := SmallInt(CalibData[4] or (CalibData[5] shl 8));

    dig_P1 := CalibData[6] or (CalibData[7] shl 8);
    dig_P2 := SmallInt(CalibData[8] or (CalibData[9] shl 8));
    dig_P3 := SmallInt(CalibData[10] or (CalibData[11] shl 8));
    dig_P4 := SmallInt(CalibData[12] or (CalibData[13] shl 8));
    dig_P5 := SmallInt(CalibData[14] or (CalibData[15] shl 8));
    dig_P6 := SmallInt(CalibData[16] or (CalibData[17] shl 8));
    dig_P7 := SmallInt(CalibData[18] or (CalibData[19] shl 8));
    dig_P8 := SmallInt(CalibData[20] or (CalibData[21] shl 8));
    dig_P9 := SmallInt(CalibData[22] or (CalibData[23] shl 8));

    FCalibrationRead := True;
    WriteLn('Calibration lue');
  end
  else
    WriteLn('Erreur lecture calibration');
end;

function TBMP280.CompensateTemperature(adc_T: LongInt): Double;  
var
  var1, var2, T: Double;
begin
  var1 := (adc_T / 16384.0 - dig_T1 / 1024.0) * dig_T2;
  var2 := ((adc_T / 131072.0 - dig_T1 / 8192.0) *
           (adc_T / 131072.0 - dig_T1 / 8192.0)) * dig_T3;
  T := (var1 + var2) / 5120.0;
  Result := T;
end;

function TBMP280.CompensatePressure(adc_P: LongInt): Double;  
var
  var1, var2, p: Double;
  t_fine: Double;
  adc_T: LongInt;
begin
  // Pour calculer la pression, on a besoin de la température
  FI2C.WriteByte(BMP280_REG_TEMP_MSB);
  adc_T := (FI2C.ReadByte shl 12) or (FI2C.ReadByte shl 4) or (FI2C.ReadByte shr 4);

  t_fine := CompensateTemperature(adc_T) * 5120.0;

  var1 := (t_fine / 2.0) - 64000.0;
  var2 := var1 * var1 * dig_P6 / 32768.0;
  var2 := var2 + var1 * dig_P5 * 2.0;
  var2 := (var2 / 4.0) + (dig_P4 * 65536.0);
  var1 := (dig_P3 * var1 * var1 / 524288.0 + dig_P2 * var1) / 524288.0;
  var1 := (1.0 + var1 / 32768.0) * dig_P1;

  if var1 = 0.0 then
  begin
    Result := 0.0;
    Exit;
  end;

  p := 1048576.0 - adc_P;
  p := (p - (var2 / 4096.0)) * 6250.0 / var1;
  var1 := dig_P9 * p * p / 2147483648.0;
  var2 := p * dig_P8 / 32768.0;
  p := p + (var1 + var2 + dig_P7) / 16.0;

  Result := p / 100.0; // Convertir en hPa
end;

function TBMP280.ReadTemperature: Double;  
var
  Data: TBytes;
  adc_T: LongInt;
begin
  Result := 0.0;

  if not FCalibrationRead then
    Exit;

  // Lire les 3 octets de température
  FI2C.WriteByte(BMP280_REG_TEMP_MSB);
  Data := FI2C.ReadBytes(3);

  if Length(Data) = 3 then
  begin
    adc_T := (Data[0] shl 12) or (Data[1] shl 4) or (Data[2] shr 4);
    Result := CompensateTemperature(adc_T);
  end;
end;

function TBMP280.ReadPressure: Double;  
var
  Data: TBytes;
  adc_P: LongInt;
begin
  Result := 0.0;

  if not FCalibrationRead then
    Exit;

  // Lire les 3 octets de pression
  FI2C.WriteByte(BMP280_REG_PRESS_MSB);
  Data := FI2C.ReadBytes(3);

  if Length(Data) = 3 then
  begin
    adc_P := (Data[0] shl 12) or (Data[1] shl 4) or (Data[2] shr 4);
    Result := CompensatePressure(adc_P);
  end;
end;

function TBMP280.ReadAltitude(SeaLevelPressure: Double): Double;  
var
  Pressure: Double;
begin
  Pressure := ReadPressure;
  // Formule barométrique
  Result := 44330.0 * (1.0 - Power(Pressure / SeaLevelPressure, 0.1903));
end;

end.
```

**Programme d'exemple avec BMP280 :**

```pascal
program TestBMP280;

{$mode objfpc}{$H+}

uses
  SysUtils, BMP280;

var
  Sensor: TBMP280;
  Temperature, Pressure, Altitude: Double;

begin
  WriteLn('=== Test BMP280 ===');
  WriteLn;

  Sensor := TBMP280.Create(1, $76); // Bus I2C-1, adresse 0x76

  try
    if Sensor.Initialize then
    begin
      WriteLn('Lecture continue...');
      WriteLn('Ctrl+C pour arrêter');
      WriteLn;

      while True do
      begin
        Temperature := Sensor.ReadTemperature;
        Pressure := Sensor.ReadPressure;
        Altitude := Sensor.ReadAltitude(1013.25); // Pression niveau mer standard

        WriteLn(Format('Température: %.2f °C', [Temperature]));
        WriteLn(Format('Pression:    %.2f hPa', [Pressure]));
        WriteLn(Format('Altitude:    %.2f m', [Altitude]));
        WriteLn('─────────────────────────────');

        Sleep(2000); // Lecture toutes les 2 secondes
      end;
    end
    else
      WriteLn('Erreur initialisation BMP280');

  finally
    Sensor.Free;
  end;
end.
```

**Schéma de connexion BMP280 :**
```
BMP280           Raspberry Pi  
VCC  ----------- 3.3V (broche 1)  
GND  ----------- GND (broche 6)  
SDA  ----------- SDA (GPIO 2, broche 3)  
SCL  ----------- SCL (GPIO 3, broche 5)
```

## 7. Communication UART (Serial)

### 7.1 Principe de l'UART

UART (Universal Asynchronous Receiver/Transmitter) est un protocole de communication série asynchrone point-à-point utilisant :
- **TX** (Transmit) : Émission de données
- **RX** (Receive) : Réception de données
- **GND** : Masse commune

**Paramètres de configuration :**
- **Baud rate** : Vitesse en bits/seconde (9600, 115200, etc.)
- **Data bits** : Nombre de bits de données (7 ou 8)
- **Parity** : Parité (None, Even, Odd)
- **Stop bits** : Bits d'arrêt (1 ou 2)

**Sur Raspberry Pi :**
- UART0 (GPIO 14/15) : `/dev/serial0` ou `/dev/ttyAMA0`
- UART1 (mini-UART) : `/dev/serial1` ou `/dev/ttyS0`

### 7.2 Activer UART sur Raspberry Pi

```bash
# Désactiver la console série
sudo raspi-config
# Interface Options → Serial Port
# "Login shell accessible over serial?" → No
# "Serial port hardware enabled?" → Yes

# Ou éditer /boot/config.txt
sudo nano /boot/config.txt
# Ajouter:
# enable_uart=1
# dtoverlay=disable-bt  # Désactiver Bluetooth pour libérer UART0

# Redémarrer
sudo reboot

# Vérifier les ports disponibles
ls -l /dev/serial*
```

### 7.3 Bibliothèque UART pour FreePascal

```pascal
unit SimpleUART;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, termio;

type
  TBaudRate = (br9600, br19200, br38400, br57600, br115200, br230400);
  TParity = (pNone, pEven, pOdd);
  TStopBits = (sb1, sb2);

  { TUARTPort }
  TUARTPort = class
  private
    FDevice: string;
    FHandle: Integer;
    FBaudRate: TBaudRate;
    FDataBits: Integer;
    FParity: TParity;
    FStopBits: TStopBits;
    function OpenPort: Boolean;
    procedure ClosePort;
    function ConfigurePort: Boolean;
    function BaudRateToSpeed(Rate: TBaudRate): Cardinal;
  public
    constructor Create(const Device: string);
    destructor Destroy; override;
    function Open: Boolean;
    procedure Close;
    function WriteByte(Value: Byte): Boolean;
    function ReadByte(out Value: Byte; TimeoutMS: Integer = 1000): Boolean;
    function WriteString(const Text: string): Boolean;
    function ReadString(MaxLength: Integer; TimeoutMS: Integer = 1000): string;
    function WriteData(const Data: array of Byte; Count: Integer): Integer;
    function ReadData(var Data: array of Byte; MaxCount: Integer; TimeoutMS: Integer = 1000): Integer;
    function DataAvailable: Boolean;
    procedure Flush;

    property Device: string read FDevice;
    property BaudRate: TBaudRate read FBaudRate write FBaudRate;
    property DataBits: Integer read FDataBits write FDataBits;
    property Parity: TParity read FParity write FParity;
    property StopBits: TStopBits read FStopBits write FStopBits;
  end;

implementation

uses
  Unix;

{ TUARTPort }

constructor TUARTPort.Create(const Device: string);  
begin
  FDevice := Device;
  FHandle := -1;
  FBaudRate := br115200;
  FDataBits := 8;
  FParity := pNone;
  FStopBits := sb1;
end;

destructor TUARTPort.Destroy;  
begin
  Close;
  inherited Destroy;
end;

function TUARTPort.BaudRateToSpeed(Rate: TBaudRate): Cardinal;  
begin
  case Rate of
    br9600:   Result := B9600;
    br19200:  Result := B19200;
    br38400:  Result := B38400;
    br57600:  Result := B57600;
    br115200: Result := B115200;
    br230400: Result := B230400;
    else      Result := B115200;
  end;
end;

function TUARTPort.OpenPort: Boolean;  
begin
  FHandle := FpOpen(FDevice, O_RDWR or O_NOCTTY or O_NONBLOCK);
  Result := FHandle >= 0;

  if Result then
    WriteLn(Format('UART ouvert: %s (fd=%d)', [FDevice, FHandle]))
  else
    WriteLn(Format('Erreur ouverture UART: %s (errno=%d)', [FDevice, fpgeterrno]));
end;

procedure TUARTPort.ClosePort;  
begin
  if FHandle >= 0 then
  begin
    FpClose(FHandle);
    WriteLn(Format('UART fermé: %s', [FDevice]));
    FHandle := -1;
  end;
end;

function TUARTPort.ConfigurePort: Boolean;  
var
  Options: Termios;
  Speed: Cardinal;
begin
  Result := False;
  if FHandle < 0 then
    Exit;

  // Récupérer les options actuelles
  if tcgetattr(FHandle, @Options) < 0 then
  begin
    WriteLn('Erreur tcgetattr');
    Exit;
  end;

  // Définir la vitesse
  Speed := BaudRateToSpeed(FBaudRate);
  cfsetispeed(@Options, Speed);
  cfsetospeed(@Options, Speed);

  // Configuration des bits de contrôle
  Options.c_cflag := Options.c_cflag or (CLOCAL or CREAD);

  // Bits de données
  Options.c_cflag := Options.c_cflag and (not CSIZE);
  if FDataBits = 7 then
    Options.c_cflag := Options.c_cflag or CS7
  else
    Options.c_cflag := Options.c_cflag or CS8;

  // Parité
  case FParity of
    pNone:
      Options.c_cflag := Options.c_cflag and (not PARENB);
    pEven:
      begin
        Options.c_cflag := Options.c_cflag or PARENB;
        Options.c_cflag := Options.c_cflag and (not PARODD);
      end;
    pOdd:
      begin
        Options.c_cflag := Options.c_cflag or PARENB;
        Options.c_cflag := Options.c_cflag or PARODD;
      end;
  end;

  // Bits d'arrêt
  if FStopBits = sb2 then
    Options.c_cflag := Options.c_cflag or CSTOPB
  else
    Options.c_cflag := Options.c_cflag and (not CSTOPB);

  // Mode raw
  Options.c_lflag := Options.c_lflag and (not (ICANON or ECHO or ECHOE or ISIG));
  Options.c_iflag := Options.c_iflag and (not (IXON or IXOFF or IXANY));
  Options.c_oflag := Options.c_oflag and (not OPOST);

  // Timeouts (0 = non-bloquant)
  Options.c_cc[VMIN] := 0;
  Options.c_cc[VTIME] := 0;

  // Appliquer les options
  if tcsetattr(FHandle, TCSANOW, @Options) < 0 then
  begin
    WriteLn('Erreur tcsetattr');
    Exit;
  end;

  Result := True;
  WriteLn(Format('UART configuré: %d bauds, %d bits de données',
                 [Speed, FDataBits]));
end;

function TUARTPort.Open: Boolean;  
begin
  Result := OpenPort;
  if Result then
    Result := ConfigurePort;
end;

procedure TUARTPort.Close;  
begin
  ClosePort;
end;

function TUARTPort.WriteByte(Value: Byte): Boolean;  
begin
  Result := False;
  if FHandle < 0 then
    Exit;

  Result := FpWrite(FHandle, Value, 1) = 1;
end;

function TUARTPort.ReadByte(out Value: Byte; TimeoutMS: Integer): Boolean;  
var
  StartTime: QWord;
  BytesRead: Integer;
begin
  Result := False;
  Value := 0;

  if FHandle < 0 then
    Exit;

  StartTime := GetTickCount64;

  repeat
    BytesRead := FpRead(FHandle, Value, 1);
    if BytesRead = 1 then
    begin
      Result := True;
      Break;
    end;

    if BytesRead < 0 then
      Break;

    Sleep(1);
  until (GetTickCount64 - StartTime) >= TimeoutMS;
end;

function TUARTPort.WriteString(const Text: string): Boolean;  
begin
  Result := WriteData(PByte(Text)^, Length(Text)) = Length(Text);
end;

function TUARTPort.ReadString(MaxLength: Integer; TimeoutMS: Integer): string;  
var
  Buffer: array of Byte;
  BytesRead: Integer;
begin
  Result := '';
  SetLength(Buffer, MaxLength);

  BytesRead := ReadData(Buffer, MaxLength, TimeoutMS);
  if BytesRead > 0 then
  begin
    SetLength(Result, BytesRead);
    Move(Buffer[0], Result[1], BytesRead);
  end;
end;

function TUARTPort.WriteData(const Data: array of Byte; Count: Integer): Integer;  
begin
  Result := 0;
  if FHandle < 0 then
    Exit;

  Result := FpWrite(FHandle, Data[0], Count);
end;

function TUARTPort.ReadData(var Data: array of Byte; MaxCount: Integer;
  TimeoutMS: Integer): Integer;
var
  StartTime: QWord;
  TotalRead: Integer;
  BytesRead: Integer;
begin
  Result := 0;
  TotalRead := 0;

  if FHandle < 0 then
    Exit;

  StartTime := GetTickCount64;

  while TotalRead < MaxCount do
  begin
    BytesRead := FpRead(FHandle, Data[TotalRead], MaxCount - TotalRead);

    if BytesRead > 0 then
      TotalRead := TotalRead + BytesRead
    else if BytesRead < 0 then
      Break;

    if (GetTickCount64 - StartTime) >= TimeoutMS then
      Break;

    if BytesRead = 0 then
      Sleep(1);
  end;

  Result := TotalRead;
end;

function TUARTPort.DataAvailable: Boolean;  
var
  BytesAvail: Integer;
begin
  Result := False;
  if FHandle < 0 then
    Exit;

  if FpIOCtl(FHandle, FIONREAD, @BytesAvail) >= 0 then
    Result := BytesAvail > 0;
end;

procedure TUARTPort.Flush;  
begin
  if FHandle >= 0 then
    tcflush(FHandle, TCIOFLUSH);
end;

end.
```

### 7.4 Exemple : Communication avec un module GPS

```pascal
program GPSReader;

{$mode objfpc}{$H+}

uses
  SysUtils, SimpleUART;

type
  { TGPSParser }
  TGPSParser = class
  public
    class function ParseGGA(const Sentence: string; out Latitude, Longitude: Double;
      out Altitude: Double): Boolean;
    class function ParseRMC(const Sentence: string; out Latitude, Longitude: Double;
      out Speed: Double): Boolean;
  end;

{ TGPSParser }

class function TGPSParser.ParseGGA(const Sentence: string; out Latitude,
  Longitude, Altitude: Double): Boolean;
var
  Parts: TStringList;
  LatDeg, LonDeg: Integer;
  LatMin, LonMin: Double;
  LatDir, LonDir: Char;
begin
  Result := False;
  Latitude := 0;
  Longitude := 0;
  Altitude := 0;

  if Copy(Sentence, 1, 6) <> '$GPGGA' then
    Exit;

  Parts := TStringList.Create;
  try
    Parts.Delimiter := ',';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Sentence;

    if Parts.Count < 10 then
      Exit;

    // Parse latitude (format: DDMM.MMMM)
    if (Parts[2] <> '') and (Parts[3] <> '') then
    begin
      LatDeg := StrToIntDef(Copy(Parts[2], 1, 2), 0);
      LatMin := StrToFloatDef(Copy(Parts[2], 3, Length(Parts[2])), 0);
      Latitude := LatDeg + (LatMin / 60.0);

      LatDir := Parts[3][1];
      if LatDir = 'S' then
        Latitude := -Latitude;
    end;

    // Parse longitude (format: DDDMM.MMMM)
    if (Parts[4] <> '') and (Parts[5] <> '') then
    begin
      LonDeg := StrToIntDef(Copy(Parts[4], 1, 3), 0);
      LonMin := StrToFloatDef(Copy(Parts[4], 4, Length(Parts[4])), 0);
      Longitude := LonDeg + (LonMin / 60.0);

      LonDir := Parts[5][1];
      if LonDir = 'W' then
        Longitude := -Longitude;
    end;

    // Parse altitude
    if Parts[9] <> '' then
      Altitude := StrToFloatDef(Parts[9], 0);

    Result := True;

  finally
    Parts.Free;
  end;
end;

class function TGPSParser.ParseRMC(const Sentence: string; out Latitude,
  Longitude, Speed: Double): Boolean;
var
  Parts: TStringList;
  LatDeg, LonDeg: Integer;
  LatMin, LonMin: Double;
  LatDir, LonDir: Char;
begin
  Result := False;
  Latitude := 0;
  Longitude := 0;
  Speed := 0;

  if Copy(Sentence, 1, 6) <> '$GPRMC' then
    Exit;

  Parts := TStringList.Create;
  try
    Parts.Delimiter := ',';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Sentence;

    if Parts.Count < 8 then
      Exit;

    // Vérifier le statut (A = valide)
    if Parts[2] <> 'A' then
      Exit;

    // Parse latitude
    if (Parts[3] <> '') and (Parts[4] <> '') then
    begin
      LatDeg := StrToIntDef(Copy(Parts[3], 1, 2), 0);
      LatMin := StrToFloatDef(Copy(Parts[3], 3, Length(Parts[3])), 0);
      Latitude := LatDeg + (LatMin / 60.0);

      LatDir := Parts[4][1];
      if LatDir = 'S' then
        Latitude := -Latitude;
    end;

    // Parse longitude
    if (Parts[5] <> '') and (Parts[6] <> '') then
    begin
      LonDeg := StrToIntDef(Copy(Parts[5], 1, 3), 0);
      LonMin := StrToFloatDef(Copy(Parts[5], 4, Length(Parts[5])), 0);
      Longitude := LonDeg + (LonMin / 60.0);

      LonDir := Parts[6][1];
      if LonDir = 'W' then
        Longitude := -Longitude;
    end;

    // Parse vitesse (en nœuds)
    if Parts[7] <> '' then
      Speed := StrToFloatDef(Parts[7], 0) * 1.852; // Convertir en km/h

    Result := True;

  finally
    Parts.Free;
  end;
end;

var
  UART: TUARTPort;
  Line: string;
  Ch: Byte;
  Latitude, Longitude, Altitude, Speed: Double;

begin
  WriteLn('=== Lecteur GPS ===');
  WriteLn;

  UART := TUARTPort.Create('/dev/serial0');
  UART.BaudRate := br9600; // Vitesse typique pour modules GPS
  UART.DataBits := 8;
  UART.Parity := pNone;
  UART.StopBits := sb1;

  try
    if UART.Open then
    begin
      WriteLn('Port série ouvert');
      WriteLn('Lecture des données GPS...');
      WriteLn('Ctrl+C pour arrêter');
      WriteLn;

      Line := '';

      while True do
      begin
        if UART.ReadByte(Ch, 100) then
        begin
          if Ch = 10 then // Line Feed
          begin
            // Traiter la ligne complète
            if Copy(Line, 1, 6) = '$GPGGA' then
            begin
              if TGPSParser.ParseGGA(Line, Latitude, Longitude, Altitude) then
              begin
                WriteLn('GGA:');
                WriteLn(Format('  Position: %.6f, %.6f', [Latitude, Longitude]));
                WriteLn(Format('  Altitude: %.1f m', [Altitude]));
              end;
            end
            else if Copy(Line, 1, 6) = '$GPRMC' then
            begin
              if TGPSParser.ParseRMC(Line, Latitude, Longitude, Speed) then
              begin
                WriteLn('RMC:');
                WriteLn(Format('  Position: %.6f, %.6f', [Latitude, Longitude]));
                WriteLn(Format('  Vitesse:  %.1f km/h', [Speed]));
              end;
            end;

            Line := '';
          end
          else if Ch <> 13 then // Ignorer Carriage Return
            Line := Line + Chr(Ch);
        end;
      end;
    end
    else
      WriteLn('Erreur ouverture du port série');

  finally
    UART.Free;
  end;
end.
```

**Schéma de connexion GPS :**
```
Module GPS       Raspberry Pi  
VCC  ----------- 3.3V ou 5V (selon module)  
GND  ----------- GND (broche 6)  
TX   ----------- RX (GPIO 15, broche 10)  
RX   ----------- TX (GPIO 14, broche 8)
```

## 8. 1-Wire (Bus Dallas/Maxim)

### 8.1 Principe du 1-Wire

1-Wire est un bus de communication série utilisant un seul fil (+ masse) pour la communication et l'alimentation. Il est couramment utilisé pour les capteurs de température DS18B20.

**Caractéristiques :**
- Un seul fil de données (+ GND)
- Alimentation parasite possible
- Adressage unique de 64 bits
- Plusieurs périphériques sur le même bus

### 8.2 Activer 1-Wire sur Raspberry Pi

```bash
# Activer 1-Wire
sudo raspi-config
# Interface Options → 1-Wire → Yes

# Ou éditer /boot/config.txt
sudo nano /boot/config.txt
# Ajouter: dtoverlay=w1-gpio

# Redémarrer
sudo reboot

# Vérifier que 1-Wire est activé
ls /sys/bus/w1/devices/
# Devrait afficher: 28-xxxxxxxxxxxx (capteurs DS18B20)
```

### 8.3 Lecture de capteurs DS18B20

```pascal
unit DS18B20;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TDS18B20Sensor }
  TDS18B20Sensor = class
  private
    FSensorID: string;
    FDevicePath: string;
    function ReadTemperatureRaw: string;
  public
    constructor Create(const SensorID: string);
    function ReadTemperature: Double;
    property SensorID: string read FSensorID;
  end;

  { TDS18B20Manager }
  TDS18B20Manager = class
  public
    class function GetSensorList: TStringList;
    class function ScanSensors: Integer;
  end;

implementation

const
  W1_DEVICES_PATH = '/sys/bus/w1/devices/';

{ TDS18B20Sensor }

constructor TDS18B20Sensor.Create(const SensorID: string);  
begin
  FSensorID := SensorID;
  FDevicePath := W1_DEVICES_PATH + FSensorID + '/w1_slave';

  if not FileExists(FDevicePath) then
    raise Exception.CreateFmt('Capteur non trouvé: %s', [FSensorID]);
end;

function TDS18B20Sensor.ReadTemperatureRaw: string;  
var
  F: TextFile;
  Line: string;
begin
  Result := '';

  try
    AssignFile(F, FDevicePath);
    Reset(F);

    while not EOF(F) do
    begin
      ReadLn(F, Line);
      Result := Result + Line + LineEnding;
    end;

    CloseFile(F);
  except
    on E: Exception do
      WriteLn('Erreur lecture capteur: ', E.Message);
  end;
end;

function TDS18B20Sensor.ReadTemperature: Double;  
var
  RawData: string;
  Lines: TStringList;
  TempStr: string;
  TempPos: Integer;
  TempValue: Integer;
begin
  Result := 0.0;

  RawData := ReadTemperatureRaw;
  if RawData = '' then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := RawData;

    // Vérifier que la lecture est valide (YES dans la première ligne)
    if (Lines.Count >= 2) and (Pos('YES', Lines[0]) > 0) then
    begin
      // Extraire la température de la deuxième ligne
      TempPos := Pos('t=', Lines[1]);
      if TempPos > 0 then
      begin
        TempStr := Copy(Lines[1], TempPos + 2, Length(Lines[1]));
        TempValue := StrToIntDef(TempStr, 0);
        // La température est en millièmes de degrés
        Result := TempValue / 1000.0;
      end;
    end;

  finally
    Lines.Free;
  end;
end;

{ TDS18B20Manager }

class function TDS18B20Manager.GetSensorList: TStringList;  
var
  SearchRec: TSearchRec;
begin
  Result := TStringList.Create;

  if FindFirst(W1_DEVICES_PATH + '28-*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) <> 0 then
        Result.Add(SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

class function TDS18B20Manager.ScanSensors: Integer;  
var
  Sensors: TStringList;
begin
  Sensors := GetSensorList;
  try
    Result := Sensors.Count;
    WriteLn(Format('%d capteur(s) DS18B20 trouvé(s):', [Result]));
    for Result := 0 to Sensors.Count - 1 do
      WriteLn('  - ', Sensors[Result]);
  finally
    Sensors.Free;
  end;
end;

end.
```

**Programme d'exemple DS18B20 :**

```pascal
program TestDS18B20;

{$mode objfpc}{$H+}

uses
  SysUtils, DS18B20;

var
  Sensors: TStringList;
  Sensor: TDS18B20Sensor;
  Temperature: Double;
  i: Integer;

begin
  WriteLn('=== Test DS18B20 ===');
  WriteLn;

  // Scanner les capteurs disponibles
  TDS18B20Manager.ScanSensors;
  WriteLn;

  Sensors := TDS18B20Manager.GetSensorList;

  try
    if Sensors.Count = 0 then
    begin
      WriteLn('Aucun capteur DS18B20 détecté');
      WriteLn('Vérifiez:');
      WriteLn('  1. Le câblage (GPIO 4 par défaut)');
      WriteLn('  2. La configuration 1-Wire (dtoverlay=w1-gpio)');
      Exit;
    end;

    WriteLn('Lecture continue...');
    WriteLn('Ctrl+C pour arrêter');
    WriteLn;

    while True do
    begin
      for i := 0 to Sensors.Count - 1 do
      begin
        Sensor := TDS18B20Sensor.Create(Sensors[i]);
        try
          Temperature := Sensor.ReadTemperature;
          WriteLn(Format('Capteur %d (%s): %.3f °C',
                         [i + 1, Sensors[i], Temperature]));
        finally
          Sensor.Free;
        end;
      end;

      WriteLn('─────────────────────────────');
      Sleep(2000);
    end;

  finally
    Sensors.Free;
  end;
end.
```

**Schéma de connexion DS18B20 :**
```
DS18B20          Raspberry Pi  
VDD (rouge)  --- 3.3V (broche 1)  
GND (noir)   --- GND (broche 6)  
DATA (jaune) --- GPIO 4 (broche 7)
                 + résistance pull-up 4.7kΩ vers 3.3V
```

## 9. Accès GPIO sous Windows

### 9.1 Limitations Windows

Windows ne dispose pas d'interfaces GPIO natives comme Linux. Cependant, certaines solutions existent :

**Options disponibles :**
1. **Cartes USB-GPIO** (FT232H, CH341A)
2. **Arduino/ESP32** en tant que périphérique USB
3. **Windows IoT Core** (pour Raspberry Pi)
4. **Port parallèle (obsolète)**

### 9.2 Exemple avec carte USB FT232H

```pascal
unit FT232H_GPIO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$IFDEF WINDOWS}
type
  { TFT232H }
  TFT232H = class
  private
    FHandle: Pointer;
    FConnected: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Connect: Boolean;
    procedure Disconnect;
    function SetPinMode(Pin: Byte; IsOutput: Boolean): Boolean;
    function DigitalWrite(Pin: Byte; Value: Boolean): Boolean;
    function DigitalRead(Pin: Byte): Boolean;
  end;
{$ENDIF}

implementation

{$IFDEF WINDOWS}
// Note: Nécessite la bibliothèque libMPSSE ou libftdi
// À implémenter selon la bibliothèque choisie

{ TFT232H }

constructor TFT232H.Create;  
begin
  FHandle := nil;
  FConnected := False;
end;

destructor TFT232H.Destroy;  
begin
  Disconnect;
  inherited Destroy;
end;

function TFT232H.Connect: Boolean;  
begin
  // Implémentation dépendante de la bibliothèque
  // Exemple conceptuel:
  WriteLn('Tentative de connexion à FT232H...');

  // Charger la DLL
  // FHandle := LoadLibrary('ftd2xx.dll');

  // Initialiser le périphérique
  // ...

  FConnected := True; // Si succès
  Result := FConnected;

  if Result then
    WriteLn('FT232H connecté')
  else
    WriteLn('Erreur connexion FT232H');
end;

procedure TFT232H.Disconnect;  
begin
  if FConnected then
  begin
    // Fermer le périphérique
    // ...
    FConnected := False;
    WriteLn('FT232H déconnecté');
  end;
end;

function TFT232H.SetPinMode(Pin: Byte; IsOutput: Boolean): Boolean;  
begin
  Result := False;
  if not FConnected then
    Exit;

  // Configuration de la broche
  // ...

  Result := True;
end;

function TFT232H.DigitalWrite(Pin: Byte; Value: Boolean): Boolean;  
begin
  Result := False;
  if not FConnected then
    Exit;

  // Écriture sur la broche
  // ...

  Result := True;
end;

function TFT232H.DigitalRead(Pin: Byte): Boolean;  
begin
  Result := False;
  if not FConnected then
    Exit;

  // Lecture de la broche
  // ...
end;

{$ENDIF}

end.
```

### 9.3 Communication via Arduino comme pont USB

Une solution simple et portable est d'utiliser un Arduino comme interface :

**Sketch Arduino (Firmata) :**
```cpp
// Utiliser StandardFirmata.ino disponible dans l'IDE Arduino
// Fichier → Exemples → Firmata → StandardFirmata
```

**Côté FreePascal (Windows/Ubuntu) :**

```pascal
unit ArduinoBridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$IFDEF WINDOWS}Windows,{$ENDIF} BaseUnix, termio;

type
  { TArduinoBridge }
  TArduinoBridge = class
  private
    FSerialPort: string;
    FHandle: THandle;
    FConnected: Boolean;
    function OpenSerial: Boolean;
    procedure CloseSerial;
    function SendCommand(const Cmd: string): Boolean;
    function ReadResponse(TimeoutMS: Integer = 1000): string;
  public
    constructor Create(const PortName: string);
    destructor Destroy; override;
    function Connect: Boolean;
    procedure Disconnect;
    function SetPinMode(Pin: Byte; IsOutput: Boolean): Boolean;
    function DigitalWrite(Pin: Byte; Value: Boolean): Boolean;
    function DigitalRead(Pin: Byte): Boolean;
    function AnalogRead(Pin: Byte): Integer;
    function AnalogWrite(Pin: Byte; Value: Byte): Boolean; // PWM
  end;

implementation

{$IFDEF WINDOWS}
uses
  Registry;
{$ENDIF}

{ TArduinoBridge }

constructor TArduinoBridge.Create(const PortName: string);  
begin
  FSerialPort := PortName;
  {$IFDEF WINDOWS}
  // Windows: format "COM3", "COM4", etc.
  if Copy(FSerialPort, 1, 4) <> '\\.\' then
    FSerialPort := '\\.\' + FSerialPort;
  {$ENDIF}

  FHandle := INVALID_HANDLE_VALUE;
  FConnected := False;
end;

destructor TArduinoBridge.Destroy;  
begin
  Disconnect;
  inherited Destroy;
end;

function TArduinoBridge.OpenSerial: Boolean;
{$IFDEF WINDOWS}
var
  DCB: TDCB;
  Timeouts: TCommTimeouts;
{$ENDIF}
begin
  Result := False;

  {$IFDEF WINDOWS}
  // Ouverture du port série sous Windows
  FHandle := CreateFile(
    PChar(FSerialPort),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    0,
    0
  );

  if FHandle = INVALID_HANDLE_VALUE then
  begin
    WriteLn('Erreur ouverture port série: ', FSerialPort);
    Exit;
  end;

  // Configuration du port série
  FillChar(DCB, SizeOf(DCB), 0);
  DCB.DCBlength := SizeOf(DCB);

  if not GetCommState(FHandle, DCB) then
  begin
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
    Exit;
  end;

  DCB.BaudRate := 57600;
  DCB.ByteSize := 8;
  DCB.Parity := NOPARITY;
  DCB.StopBits := ONESTOPBIT;

  if not SetCommState(FHandle, DCB) then
  begin
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
    Exit;
  end;

  // Configuration des timeouts
  Timeouts.ReadIntervalTimeout := 50;
  Timeouts.ReadTotalTimeoutMultiplier := 10;
  Timeouts.ReadTotalTimeoutConstant := 100;
  Timeouts.WriteTotalTimeoutMultiplier := 10;
  Timeouts.WriteTotalTimeoutConstant := 100;

  SetCommTimeouts(FHandle, Timeouts);

  {$ELSE}
  // Ouverture du port série sous Linux/Unix
  FHandle := FpOpen(FSerialPort, O_RDWR or O_NOCTTY);

  if FHandle < 0 then
  begin
    WriteLn('Erreur ouverture port série: ', FSerialPort);
    Exit;
  end;

  // Configuration du port série (voir SimpleUART)
  // ...
  {$ENDIF}

  Result := True;
  Sleep(2000); // Attendre que l'Arduino redémarre
end;

procedure TArduinoBridge.CloseSerial;  
begin
  {$IFDEF WINDOWS}
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
  end;
  {$ELSE}
  if FHandle >= 0 then
  begin
    FpClose(FHandle);
    FHandle := -1;
  end;
  {$ENDIF}
end;

function TArduinoBridge.Connect: Boolean;  
begin
  Result := OpenSerial;
  if Result then
  begin
    FConnected := True;
    WriteLn('Arduino connecté sur ', FSerialPort);
  end;
end;

procedure TArduinoBridge.Disconnect;  
begin
  if FConnected then
  begin
    CloseSerial;
    FConnected := False;
    WriteLn('Arduino déconnecté');
  end;
end;

function TArduinoBridge.SendCommand(const Cmd: string): Boolean;  
var
  BytesWritten: {$IFDEF WINDOWS}DWORD{$ELSE}Integer{$ENDIF};
  CmdWithNewline: string;
begin
  Result := False;
  if not FConnected then
    Exit;

  CmdWithNewline := Cmd + #10; // Ajouter Line Feed

  {$IFDEF WINDOWS}
  Result := WriteFile(FHandle, CmdWithNewline[1], Length(CmdWithNewline),
                      BytesWritten, nil);
  {$ELSE}
  BytesWritten := FpWrite(FHandle, CmdWithNewline[1], Length(CmdWithNewline));
  Result := BytesWritten = Length(CmdWithNewline);
  {$ENDIF}
end;

function TArduinoBridge.ReadResponse(TimeoutMS: Integer): string;  
var
  Buffer: array[0..255] of Char;
  BytesRead: {$IFDEF WINDOWS}DWORD{$ELSE}Integer{$ENDIF};
  StartTime: QWord;
begin
  Result := '';
  if not FConnected then
    Exit;

  StartTime := GetTickCount64;

  repeat
    {$IFDEF WINDOWS}
    if ReadFile(FHandle, Buffer[0], SizeOf(Buffer), BytesRead, nil) then
    {$ELSE}
    BytesRead := FpRead(FHandle, Buffer[0], SizeOf(Buffer));
    if BytesRead > 0 then
    {$ENDIF}
    begin
      SetString(Result, Buffer, BytesRead);
      if Pos(#10, Result) > 0 then
        Break;
    end;

    Sleep(10);
  until (GetTickCount64 - StartTime) >= TimeoutMS;

  Result := Trim(Result);
end;

function TArduinoBridge.SetPinMode(Pin: Byte; IsOutput: Boolean): Boolean;  
var
  Cmd: string;
begin
  if IsOutput then
    Cmd := Format('M%dO', [Pin]) // Mode Output
  else
    Cmd := Format('M%dI', [Pin]); // Mode Input

  Result := SendCommand(Cmd);
  Sleep(10);
end;

function TArduinoBridge.DigitalWrite(Pin: Byte; Value: Boolean): Boolean;  
var
  Cmd: string;
begin
  if Value then
    Cmd := Format('W%dH', [Pin]) // Write High
  else
    Cmd := Format('W%dL', [Pin]); // Write Low

  Result := SendCommand(Cmd);
  Sleep(10);
end;

function TArduinoBridge.DigitalRead(Pin: Byte): Boolean;  
var
  Cmd, Response: string;
begin
  Result := False;

  Cmd := Format('R%d', [Pin]); // Read pin
  if SendCommand(Cmd) then
  begin
    Response := ReadResponse(500);
    Result := Response = '1';
  end;
end;

function TArduinoBridge.AnalogRead(Pin: Byte): Integer;  
var
  Cmd, Response: string;
begin
  Result := 0;

  Cmd := Format('A%d', [Pin]); // Analog Read
  if SendCommand(Cmd) then
  begin
    Response := ReadResponse(500);
    Result := StrToIntDef(Response, 0);
  end;
end;

function TArduinoBridge.AnalogWrite(Pin: Byte; Value: Byte): Boolean;  
var
  Cmd: string;
begin
  Cmd := Format('P%d:%d', [Pin, Value]); // PWM
  Result := SendCommand(Cmd);
  Sleep(10);
end;

end.
```

**Sketch Arduino simple (sans Firmata) :**

```cpp
// Sketch Arduino pour protocole simplifié
// À téléverser sur l'Arduino

void setup() {
  Serial.begin(57600);
}

void loop() {
  if (Serial.available() > 0) {
    String cmd = Serial.readStringUntil('\n');
    cmd.trim();

    char command = cmd.charAt(0);
    int pin = cmd.substring(1).toInt();

    switch (command) {
      case 'M': // Mode
        if (cmd.endsWith("O"))
          pinMode(pin, OUTPUT);
        else if (cmd.endsWith("I"))
          pinMode(pin, INPUT);
        break;

      case 'W': // Digital Write
        if (cmd.endsWith("H"))
          digitalWrite(pin, HIGH);
        else if (cmd.endsWith("L"))
          digitalWrite(pin, LOW);
        break;

      case 'R': // Digital Read
        Serial.println(digitalRead(pin));
        break;

      case 'A': // Analog Read
        Serial.println(analogRead(pin));
        break;

      case 'P': // PWM (format: P9:128)
        {
          int colonPos = cmd.indexOf(':');
          if (colonPos > 0) {
            int value = cmd.substring(colonPos + 1).toInt();
            analogWrite(pin, value);
          }
        }
        break;
    }
  }
}
```

**Programme de test multi-plateforme :**

```pascal
program TestArduinoBridge;

{$mode objfpc}{$H+}

uses
  SysUtils, ArduinoBridge;

var
  Arduino: TArduinoBridge;
  PortName: string;
  i: Integer;
  Value: Integer;

begin
  WriteLn('=== Test Arduino Bridge ===');
  WriteLn;

  // Configuration du port selon l'OS
  {$IFDEF WINDOWS}
  PortName := 'COM3'; // Adapter selon votre système
  WriteLn('Windows détecté - utilisation de ', PortName);
  {$ELSE}
  PortName := '/dev/ttyUSB0'; // ou /dev/ttyACM0
  WriteLn('Linux détecté - utilisation de ', PortName);
  {$ENDIF}

  Arduino := TArduinoBridge.Create(PortName);

  try
    if Arduino.Connect then
    begin
      WriteLn('Arduino connecté !');
      WriteLn;

      // Test 1: Clignotement LED (broche 13 - LED intégrée)
      WriteLn('Test 1: Clignotement LED interne (broche 13)');
      Arduino.SetPinMode(13, True); // Output

      for i := 1 to 5 do
      begin
        WriteLn('  LED ON');
        Arduino.DigitalWrite(13, True);
        Sleep(500);

        WriteLn('  LED OFF');
        Arduino.DigitalWrite(13, False);
        Sleep(500);
      end;

      WriteLn;

      // Test 2: Lecture analogique (A0)
      WriteLn('Test 2: Lecture analogique (A0)');
      WriteLn('Connectez un potentiomètre ou laissez flottant');
      WriteLn('Lecture pendant 10 secondes...');

      for i := 1 to 10 do
      begin
        Value := Arduino.AnalogRead(0); // A0
        WriteLn(Format('  A0 = %4d (%.2f V)', [Value, (Value * 5.0) / 1023.0]));
        Sleep(1000);
      end;

      WriteLn;
      WriteLn('Tests terminés');
    end
    else
      WriteLn('Impossible de se connecter à l''Arduino');

  finally
    Arduino.Free;
  end;
end.
```

## 10. Bonnes pratiques et sécurité

### 10.1 Gestion des erreurs et exceptions

```pascal
unit SafeGPIO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleGPIO;

type
  { TSafeGPIO }
  TSafeGPIO = class
  private
    FGPIO: TSimpleGPIO;
    FAutoCleanup: Boolean;
  public
    constructor Create(PinNumber: Integer; AutoCleanup: Boolean = True);
    destructor Destroy; override;
    function TrySetValue(Value: TGPIOValue): Boolean;
    function TryGetValue(out Value: TGPIOValue): Boolean;
    procedure SafeCleanup;
  end;

implementation

{ TSafeGPIO }

constructor TSafeGPIO.Create(PinNumber: Integer; AutoCleanup: Boolean);  
begin
  FAutoCleanup := AutoCleanup;

  try
    FGPIO := TSimpleGPIO.Create(PinNumber);
    FGPIO.Export;
  except
    on E: Exception do
    begin
      WriteLn('Erreur initialisation GPIO: ', E.Message);
      raise;
    end;
  end;
end;

destructor TSafeGPIO.Destroy;  
begin
  if FAutoCleanup then
    SafeCleanup;

  FGPIO.Free;
  inherited Destroy;
end;

function TSafeGPIO.TrySetValue(Value: TGPIOValue): Boolean;  
begin
  Result := False;
  try
    FGPIO.SetValue(Value);
    Result := True;
  except
    on E: Exception do
      WriteLn('Erreur SetValue: ', E.Message);
  end;
end;

function TSafeGPIO.TryGetValue(out Value: TGPIOValue): Boolean;  
begin
  Result := False;
  try
    Value := FGPIO.GetValue;
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Erreur GetValue: ', E.Message);
      Value := gvLow;
    end;
  end;
end;

procedure TSafeGPIO.SafeCleanup;  
begin
  try
    // S'assurer que la sortie est à LOW avant de libérer
    if FGPIO.Exported then
    begin
      FGPIO.SetValue(gvLow);
      Sleep(10);
    end;
  except
    // Ignorer les erreurs lors du cleanup
  end;
end;

end.
```

### 10.2 Protection matérielle

**Résistances de limitation :**
```pascal
const
  // Calculer la résistance pour une LED
  // R = (Vsource - Vled) / Iled
  // Exemple: (3.3V - 2.0V) / 0.02A = 65Ω → utiliser 220Ω (standard)
  LED_RESISTOR_3V3 = 220; // Ohms pour LED sur 3.3V
  LED_RESISTOR_5V = 470;  // Ohms pour LED sur 5V

  // Courant maximal par broche (exemple Raspberry Pi)
  MAX_PIN_CURRENT_MA = 16;  // 16 mA par broche
  MAX_TOTAL_CURRENT_MA = 50; // 50 mA total pour toutes les broches
```

### 10.3 Debouncing (anti-rebond) pour boutons

```pascal
unit DebouncedButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleGPIO;

type
  TButtonEvent = procedure(Sender: TObject; Pressed: Boolean) of object;

  { TDebouncedButton }
  TDebouncedButton = class
  private
    FGPIO: TSimpleGPIO;
    FDebounceTime: Integer; // en ms
    FLastState: TGPIOValue;
    FLastChangeTime: QWord;
    FOnStateChange: TButtonEvent;
    procedure CheckState;
  public
    constructor Create(PinNumber: Integer; DebounceTimeMS: Integer = 50);
    destructor Destroy; override;
    procedure Poll; // À appeler régulièrement
    function IsPressed: Boolean;
    property OnStateChange: TButtonEvent read FOnStateChange write FOnStateChange;
  end;

implementation

{ TDebouncedButton }

constructor TDebouncedButton.Create(PinNumber: Integer; DebounceTimeMS: Integer);  
begin
  FDebounceTime := DebounceTimeMS;
  FLastChangeTime := 0;

  FGPIO := TSimpleGPIO.Create(PinNumber);
  FGPIO.Export;
  FGPIO.SetDirection(gdInput);

  FLastState := FGPIO.GetValue;
end;

destructor TDebouncedButton.Destroy;  
begin
  FGPIO.Free;
  inherited Destroy;
end;

procedure TDebouncedButton.CheckState;  
var
  CurrentState: TGPIOValue;
  CurrentTime: QWord;
begin
  CurrentState := FGPIO.GetValue;
  CurrentTime := GetTickCount64;

  // Si l'état a changé et que le temps de debounce est écoulé
  if (CurrentState <> FLastState) and
     (CurrentTime - FLastChangeTime >= FDebounceTime) then
  begin
    FLastState := CurrentState;
    FLastChangeTime := CurrentTime;

    // Déclencher l'événement
    if Assigned(FOnStateChange) then
      FOnStateChange(Self, CurrentState = gvLow); // Bouton en pull-up
  end;
end;

procedure TDebouncedButton.Poll;  
begin
  CheckState;
end;

function TDebouncedButton.IsPressed: Boolean;  
begin
  Result := FLastState = gvLow; // Logique inversée avec pull-up
end;

end.
```

### 10.4 Gestion multi-plateforme complète

```pascal
unit CrossPlatformGPIO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF LINUX}
  , SimpleGPIO
  {$ENDIF}
  {$IFDEF WINDOWS}
  , ArduinoBridge
  {$ENDIF};

type
  { TCrossPlatformGPIO }
  TCrossPlatformGPIO = class
  private
    FPinNumber: Integer;
    {$IFDEF LINUX}
    FLinuxGPIO: TSimpleGPIO;
    {$ENDIF}
    {$IFDEF WINDOWS}
    FArduino: TArduinoBridge;
    {$ENDIF}
  public
    constructor Create(PinNumber: Integer);
    destructor Destroy; override;
    procedure SetAsOutput;
    procedure SetAsInput;
    procedure SetHigh;
    procedure SetLow;
    function Read: Boolean;
    class function GetPlatformInfo: string;
  end;

implementation

{ TCrossPlatformGPIO }

constructor TCrossPlatformGPIO.Create(PinNumber: Integer);  
begin
  FPinNumber := PinNumber;

  {$IFDEF LINUX}
  WriteLn('Initialisation GPIO Linux (sysfs)');
  FLinuxGPIO := TSimpleGPIO.Create(PinNumber);
  FLinuxGPIO.Export;
  {$ENDIF}

  {$IFDEF WINDOWS}
  WriteLn('Initialisation GPIO Windows (Arduino bridge)');
  FArduino := TArduinoBridge.Create('COM3'); // À configurer
  if not FArduino.Connect then
    raise Exception.Create('Impossible de se connecter à l''Arduino');
  {$ENDIF}
end;

destructor TCrossPlatformGPIO.Destroy;  
begin
  {$IFDEF LINUX}
  FLinuxGPIO.Free;
  {$ENDIF}

  {$IFDEF WINDOWS}
  FArduino.Free;
  {$ENDIF}

  inherited Destroy;
end;

procedure TCrossPlatformGPIO.SetAsOutput;  
begin
  {$IFDEF LINUX}
  FLinuxGPIO.SetDirection(gdOutput);
  {$ENDIF}

  {$IFDEF WINDOWS}
  FArduino.SetPinMode(FPinNumber, True);
  {$ENDIF}
end;

procedure TCrossPlatformGPIO.SetAsInput;  
begin
  {$IFDEF LINUX}
  FLinuxGPIO.SetDirection(gdInput);
  {$ENDIF}

  {$IFDEF WINDOWS}
  FArduino.SetPinMode(FPinNumber, False);
  {$ENDIF}
end;

procedure TCrossPlatformGPIO.SetHigh;  
begin
  {$IFDEF LINUX}
  FLinuxGPIO.SetValue(gvHigh);
  {$ENDIF}

  {$IFDEF WINDOWS}
  FArduino.DigitalWrite(FPinNumber, True);
  {$ENDIF}
end;

procedure TCrossPlatformGPIO.SetLow;  
begin
  {$IFDEF LINUX}
  FLinuxGPIO.SetValue(gvLow);
  {$ENDIF}

  {$IFDEF WINDOWS}
  FArduino.DigitalWrite(FPinNumber, False);
  {$ENDIF}
end;

function TCrossPlatformGPIO.Read: Boolean;  
begin
  {$IFDEF LINUX}
  Result := FLinuxGPIO.GetValue = gvHigh;
  {$ENDIF}

  {$IFDEF WINDOWS}
  Result := FArduino.DigitalRead(FPinNumber);
  {$ENDIF}
end;

class function TCrossPlatformGPIO.GetPlatformInfo: string;  
begin
  {$IFDEF LINUX}
  Result := 'Linux (GPIO natif via sysfs)';
  {$ENDIF}

  {$IFDEF WINDOWS}
  Result := 'Windows (via Arduino bridge)';
  {$ENDIF}

  {$IFDEF DARWIN}
  Result := 'macOS (non supporté nativement)';
  {$ENDIF}
end;

end.
```

## 11. Projets pratiques complets

### 11.1 Station météo complète

```pascal
program WeatherStation;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, BMP280, DS18B20, SimpleGPIO, fpjson;

type
  { TWeatherStation }
  TWeatherStation = class
  private
    FTempSensor: TDS18B20Sensor;
    FPressureSensor: TBMP280;
    FLEDStatus: TSimpleGPIO;
    FLogFile: TextFile;
    procedure LogData(const Data: TJSONObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadAndLog;
    procedure Run;
  end;

{ TWeatherStation }

constructor TWeatherStation.Create;  
var
  Sensors: TStringList;
begin
  WriteLn('Initialisation de la station météo...');

  // LED de statut
  FLEDStatus := TSimpleGPIO.Create(17);
  FLEDStatus.Export;
  FLEDStatus.SetDirection(gdOutput);
  FLEDStatus.SetValue(gvHigh); // LED allumée = initialisation

  // Capteur de température DS18B20
  Sensors := TDS18B20Manager.GetSensorList;
  try
    if Sensors.Count > 0 then
      FTempSensor := TDS18B20Sensor.Create(Sensors[0])
    else
      raise Exception.Create('Aucun capteur DS18B20 trouvé');
  finally
    Sensors.Free;
  end;

  // Capteur de pression/température BMP280
  FPressureSensor := TBMP280.Create;
  if not FPressureSensor.Initialize then
    raise Exception.Create('Erreur initialisation BMP280');

  // Fichier de log
  AssignFile(FLogFile, 'weather_log.csv');
  if not FileExists('weather_log.csv') then
  begin
    Rewrite(FLogFile);
    WriteLn(FLogFile, 'Timestamp,Temperature_DS18B20,Temperature_BMP280,Pressure,Altitude');
  end
  else
    Append(FLogFile);

  FLEDStatus.SetValue(gvLow); // LED éteinte = prêt
  WriteLn('Station météo prête');
end;

destructor TWeatherStation.Destroy;  
begin
  CloseFile(FLogFile);
  FTempSensor.Free;
  FPressureSensor.Free;
  FLEDStatus.Free;
  inherited Destroy;
end;

procedure TWeatherStation.LogData(const Data: TJSONObject);  
var
  LogLine: string;
begin
  // Écrire en CSV
  LogLine := Format('%s,%.2f,%.2f,%.2f,%.2f',
    [Data.Get('timestamp', ''),
     Data.Get('temp_ds18b20', 0.0),
     Data.Get('temp_bmp280', 0.0),
     Data.Get('pressure', 0.0),
     Data.Get('altitude', 0.0)]);

  WriteLn(FLogFile, LogLine);
  Flush(FLogFile);
end;

procedure TWeatherStation.ReadAndLog;  
var
  Data: TJSONObject;
  TempDS, TempBMP, Pressure, Altitude: Double;
begin
  // Faire clignoter la LED pendant la lecture
  FLEDStatus.SetValue(gvHigh);

  // Lire les capteurs
  TempDS := FTempSensor.ReadTemperature;
  TempBMP := FPressureSensor.ReadTemperature;
  Pressure := FPressureSensor.ReadPressure;
  Altitude := FPressureSensor.ReadAltitude;

  // Créer l'objet JSON
  Data := TJSONObject.Create;
  try
    Data.Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    Data.Add('temp_ds18b20', TempDS);
    Data.Add('temp_bmp280', TempBMP);
    Data.Add('pressure', Pressure);
    Data.Add('altitude', Altitude);

    // Afficher
    WriteLn('─────────────────────────────────────');
    WriteLn(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    WriteLn(Format('Température (DS18B20): %.2f °C', [TempDS]));
    WriteLn(Format('Température (BMP280):  %.2f °C', [TempBMP]));
    WriteLn(Format('Pression:              %.2f hPa', [Pressure]));
    WriteLn(Format('Altitude:              %.2f m', [Altitude]));

    // Logger dans le fichier
    LogData(Data);

  finally
    Data.Free;
  end;

  FLEDStatus.SetValue(gvLow);
end;

procedure TWeatherStation.Run;  
begin
  WriteLn('Station météo en fonctionnement');
  WriteLn('Lecture toutes les 5 minutes');
  WriteLn('Ctrl+C pour arrêter');
  WriteLn;

  while True do
  begin
    ReadAndLog;
    Sleep(5 * 60 * 1000); // 5 minutes
  end;
end;

var
  Station: TWeatherStation;

begin
  try
    Station := TWeatherStation.Create;
    try
      Station.Run;
    finally
      Station.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur: ', E.Message);
  end;
end.
```

### 11.2 Système d'arrosage automatique

```pascal
program AutoWatering;

{$mode objfpc}{$H+}

uses
  SysUtils, SimpleGPIO, MCP3008;

type
  { TWateringSystem }
  TWateringSystem = class
  private
    FADC: TMCP3008;
    FPump: TSimpleGPIO;
    FMoistureChannel: Byte;
    FDryThreshold: Integer;
    FWetThreshold: Integer;
    FPumpDuration: Integer; // en secondes
    FCheckInterval: Integer; // en secondes
    function GetMoistureLevel: Integer;
    function IsSoilDry: Boolean;
    procedure WaterPlant;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

{ TWateringSystem }

constructor TWateringSystem.Create;  
begin
  WriteLn('=== Système d''arrosage automatique ===');
  WriteLn;

  // Initialiser le convertisseur ADC
  FADC := TMCP3008.Create('/dev/spidev0.0');
  FMoistureChannel := 0; // Canal 0 pour le capteur d'humidité

  // Initialiser la pompe (relais)
  FPump := TSimpleGPIO.Create(17);
  FPump.Export;
  FPump.SetDirection(gdOutput);
  FPump.SetValue(gvLow); // Pompe éteinte au démarrage

  // Configuration
  FDryThreshold := 300;   // Seuil "sec" (ajuster selon le capteur)
  FWetThreshold := 600;   // Seuil "humide"
  FPumpDuration := 5;     // Arroser pendant 5 secondes
  FCheckInterval := 60;   // Vérifier toutes les minutes

  WriteLn('Configuration:');
  WriteLn(Format('  Seuil sec:       %d', [FDryThreshold]));
  WriteLn(Format('  Seuil humide:    %d', [FWetThreshold]));
  WriteLn(Format('  Durée arrosage:  %d secondes', [FPumpDuration]));
  WriteLn(Format('  Intervalle:      %d secondes', [FCheckInterval]));
  WriteLn;
end;

destructor TWateringSystem.Destroy;  
begin
  // S'assurer que la pompe est éteinte
  FPump.SetValue(gvLow);

  FADC.Free;
  FPump.Free;
  inherited Destroy;
end;

function TWateringSystem.GetMoistureLevel: Integer;  
begin
  Result := FADC.ReadChannel(FMoistureChannel);
end;

function TWateringSystem.IsSoilDry: Boolean;  
var
  Level: Integer;
begin
  Level := GetMoistureLevel;
  Result := Level < FDryThreshold;

  WriteLn(Format('[%s] Humidité: %d - ',
          [FormatDateTime('hh:nn:ss', Now), Level]), '');

  if Result then
    WriteLn('SOL SEC !')
  else if Level > FWetThreshold then
    WriteLn('Sol humide')
  else
    WriteLn('Sol OK');
end;

procedure TWateringSystem.WaterPlant;  
begin
  WriteLn('>>> ARROSAGE EN COURS <<<');

  // Activer la pompe
  FPump.SetValue(gvHigh);

  // Attendre la durée configurée
  Sleep(FPumpDuration * 1000);

  // Éteindre la pompe
  FPump.SetValue(gvLow);

  WriteLn('>>> ARROSAGE TERMINÉ <<<');
  WriteLn;
end;

procedure TWateringSystem.Run;  
begin
  WriteLn('Système d''arrosage démarré');
  WriteLn('Ctrl+C pour arrêter');
  WriteLn;

  while True do
  begin
    if IsSoilDry then
    begin
      WaterPlant;
      // Attendre 30 minutes après arrosage avant de revérifier
      WriteLn('Attente 30 minutes...');
      Sleep(30 * 60 * 1000);
    end
    else
    begin
      // Vérifier régulièrement
      Sleep(FCheckInterval * 1000);
    end;
  end;
end;

var
  System: TWateringSystem;

begin
  try
    System := TWateringSystem.Create;
    try
      System.Run;
    finally
      System.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur: ', E.Message);
  end;
end.
```

**Schéma de connexion du système d'arrosage :**
```
Capteur d'humidité:
  VCC → 3.3V
  GND → GND
  Signal → CH0 du MCP3008

Relais (pompe):
  VCC → 5V
  GND → GND
  IN → GPIO 17

Pompe à eau:
  + → Relais NO (Normalement Ouvert)
  - → Alimentation GND

ATTENTION: Utiliser une alimentation séparée pour la pompe !
```

### 11.3 Système de détection de mouvement avec notification

```pascal
program MotionDetector;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, SimpleGPIO, fphttpclient, opensslsockets, fpjson;

type
  { TMotionDetector }
  TMotionDetector = class
  private
    FPIR: TSimpleGPIO;
    FLED: TSimpleGPIO;
    FBuzzer: TSimpleGPIO;
    FLastDetection: TDateTime;
    FCooldownSeconds: Integer;
    FWebhookURL: string;
    procedure OnMotionDetected;
    procedure ActivateAlarm;
    procedure DeactivateAlarm;
    procedure SendNotification(const Message: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

{ TMotionDetector }

constructor TMotionDetector.Create;  
begin
  WriteLn('=== Détecteur de mouvement ===');
  WriteLn;

  // Capteur PIR
  FPIR := TSimpleGPIO.Create(27);
  FPIR.Export;
  FPIR.SetDirection(gdInput);

  // LED d'indication
  FLED := TSimpleGPIO.Create(17);
  FLED.Export;
  FLED.SetDirection(gdOutput);
  FLED.SetValue(gvLow);

  // Buzzer
  FBuzzer := TSimpleGPIO.Create(22);
  FBuzzer.Export;
  FBuzzer.SetDirection(gdOutput);
  FBuzzer.SetValue(gvLow);

  // Configuration
  FLastDetection := 0;
  FCooldownSeconds := 30; // Ne pas re-notifier avant 30 secondes

  // Webhook pour notifications (optionnel)
  // Exemple avec IFTTT, Pushover, ou serveur custom
  FWebhookURL := ''; // À configurer

  WriteLn('Système prêt');
  WriteLn('Temps de latence PIR: ~2 secondes');
  WriteLn;
end;

destructor TMotionDetector.Destroy;  
begin
  DeactivateAlarm;
  FPIR.Free;
  FLED.Free;
  FBuzzer.Free;
  inherited Destroy;
end;

procedure TMotionDetector.OnMotionDetected;  
var
  CurrentTime: TDateTime;
  TimeDiff: Double;
begin
  CurrentTime := Now;

  // Vérifier le cooldown
  if FLastDetection > 0 then
  begin
    TimeDiff := (CurrentTime - FLastDetection) * 86400; // en secondes
    if TimeDiff < FCooldownSeconds then
      Exit; // Encore en cooldown
  end;

  FLastDetection := CurrentTime;

  WriteLn('!!! MOUVEMENT DÉTECTÉ !!!');
  WriteLn('Timestamp: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', CurrentTime));

  ActivateAlarm;

  if FWebhookURL <> '' then
    SendNotification('Mouvement détecté !');

  // Alarme pendant 3 secondes
  Sleep(3000);

  DeactivateAlarm;

  WriteLn('Alarme désactivée - cooldown de ', FCooldownSeconds, ' secondes');
  WriteLn;
end;

procedure TMotionDetector.ActivateAlarm;  
begin
  FLED.SetValue(gvHigh);
  FBuzzer.SetValue(gvHigh);
end;

procedure TMotionDetector.DeactivateAlarm;  
begin
  FLED.SetValue(gvLow);
  FBuzzer.SetValue(gvLow);
end;

procedure TMotionDetector.SendNotification(const Message: string);  
var
  HTTPClient: TFPHTTPClient;
  Response: string;
  JSONData: TJSONObject;
begin
  if FWebhookURL = '' then
    Exit;

  HTTPClient := TFPHTTPClient.Create(nil);
  try
    JSONData := TJSONObject.Create;
    try
      JSONData.Add('message', Message);
      JSONData.Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

      HTTPClient.RequestBody := TStringStream.Create(JSONData.AsJSON);
      HTTPClient.AddHeader('Content-Type', 'application/json');

      try
        Response := HTTPClient.Post(FWebhookURL);
        WriteLn('Notification envoyée: ', Response);
      except
        on E: Exception do
          WriteLn('Erreur envoi notification: ', E.Message);
      end;

    finally
      JSONData.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;

procedure TMotionDetector.Run;  
var
  CurrentState, PreviousState: TGPIOValue;
begin
  WriteLn('Détection active...');
  WriteLn('Ctrl+C pour arrêter');
  WriteLn;

  // État initial
  PreviousState := FPIR.GetValue;

  while True do
  begin
    CurrentState := FPIR.GetValue;

    // Détecter un front montant (passage de LOW à HIGH)
    if (CurrentState = gvHigh) and (PreviousState = gvLow) then
      OnMotionDetected;

    PreviousState := CurrentState;
    Sleep(100); // Vérifier 10 fois par seconde
  end;
end;

var
  Detector: TMotionDetector;

begin
  try
    Detector := TMotionDetector.Create;
    try
      Detector.Run;
    finally
      Detector.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur: ', E.Message);
  end;
end.
```

## 12. Ressources et documentation

### 12.1 Documentation matérielle

**Raspberry Pi :**
- Pinout officiel : https://pinout.xyz/
- Documentation GPIO : https://www.raspberrypi.com/documentation/computers/raspberry-pi.html
- Forums : https://forums.raspberrypi.com/

**Protocoles de communication :**
- SPI : https://en.wikipedia.org/wiki/Serial_Peripheral_Interface
- I²C : https://www.i2c-bus.org/
- UART : https://www.circuitbasics.com/basics-uart-communication/
- 1-Wire : https://www.maximintegrated.com/en/products/ibutton-one-wire.html

**Datasheets utiles :**
- MCP3008 (ADC) : https://ww1.microchip.com/downloads/en/DeviceDoc/21295d.pdf
- BMP280 (Pression/Temp) : https://www.bosch-sensortec.com/media/boschsensortec/downloads/datasheets/bst-bmp280-ds001.pdf
- DS18B20 (Température) : https://datasheets.maximintegrated.com/en/ds/DS18B20.pdf

### 12.2 Bibliothèques FreePascal

**Pour GPIO et interfaces :**
- PascalIO : https://github.com/SAmeis/pascalio
- Free Pascal RTL : Documentation BaseUnix, Unix
- Lazarus CCR : Composants communautaires

**Tutoriels et exemples :**
- Wiki FreePascal : https://wiki.freepascal.org/
- Wiki Lazarus : https://wiki.lazarus.freepascal.org/
- GitHub : Rechercher "freepascal gpio" ou "lazarus raspberry"

### 12.3 Outils de développement

**Compilation croisée (Cross-compilation) :**
```bash
# Compiler depuis Ubuntu pour Raspberry Pi
fpc -Tlinux -Parm myprogram.pas

# Depuis Windows (avec FPC configuré)
fpc -Tlinux -Parm myprogram.pas
```

**Déploiement automatique :**
```bash
#!/bin/bash
# deploy.sh - Script de déploiement sur Raspberry Pi

# Compiler
lazbuild --build-mode=Release myproject.lpi

# Copier vers le Raspberry Pi
scp myproject pi@raspberrypi.local:/home/pi/

# Rendre exécutable
ssh pi@raspberrypi.local "chmod +x /home/pi/myproject"

# Lancer
ssh pi@raspberrypi.local "/home/pi/myproject"
```

### 12.4 Debugging à distance

**Configuration SSH pour debugging :**
```bash
# Sur le Raspberry Pi, installer gdbserver
sudo apt-get install gdbserver

# Lancer le programme avec gdbserver
gdbserver :2345 ./myprogram

# Sur la machine de développement (Ubuntu/Windows avec GDB)
# Configurer Lazarus pour debugging distant
# Run → Run Parameters → Host Application: /path/to/myprogram
# Debugger → Debugger Backend: GDB/MI
# Remote Debugging: Enable, Target: raspberrypi.local:2345
```

## 13. Sécurité et précautions finales

### 13.1 Checklist de sécurité matérielle

```
✓ Vérifier les tensions (3.3V vs 5V)
✓ Utiliser des résistances de limitation appropriées
✓ Ne jamais court-circuiter les broches
✓ Utiliser des optocoupleurs pour isoler les circuits de puissance
✓ Alimentation externe pour les charges importantes (moteurs, relais)
✓ Protection ESD (Electrostatic Discharge)
✓ Dissipateurs thermiques si nécessaire
✓ Fusibles ou protections de courant
```

### 13.2 Checklist de sécurité logicielle

```pascal
// Toujours nettoyer les GPIO en cas d'erreur
try
  GPIO.Export;
  GPIO.SetDirection(gdOutput);
  // ... utilisation ...
finally
  GPIO.SetValue(gvLow); // État sûr
  GPIO.Unexport;
end;

// Gérer les signaux système (SIGINT, SIGTERM)
procedure SignalHandler(sig: cint); cdecl;  
begin
  WriteLn('Signal reçu, nettoyage...');
  // Nettoyer les GPIO
  Halt(0);
end;

// Dans le programme principal:
FpSignal(SIGINT, @SignalHandler);  
FpSignal(SIGTERM, @SignalHandler);
```

### 13.3 Limites et contraintes

**Raspberry Pi GPIO :**
- Courant max par broche : 16 mA
- Courant total 3.3V : 50 mA
- Pas de protection contre les surcharges
- Pas de convertisseur ADC intégré
- Timing non déterministe (Linux n'est pas un RTOS)

**Solutions pour les limitations :**
- Utiliser des buffers/drivers externes pour courants élevés
- ADC externe (MCP3008, ADS1115) pour signaux analogiques
- Utiliser un microcontrôleur dédié pour timing critique
- Protection avec diodes, résistances, optocoupleurs

## 14. Conclusion

### 14.1 Récapitulatif

Dans ce chapitre, nous avons exploré en profondeur les interfaces matérielles et GPIO avec FreePascal/Lazarus :

**Interfaces abordées :**
- ✅ **GPIO** : Contrôle basique des entrées/sorties numériques
- ✅ **PWM** : Modulation de largeur d'impulsion (logicielle et matérielle)
- ✅ **SPI** : Communication série haute vitesse
- ✅ **I²C** : Bus de communication multi-périphériques
- ✅ **UART** : Communication série asynchrone
- ✅ **1-Wire** : Bus simple fil pour capteurs

**Plateformes couvertes :**
- ✅ **Linux/Raspberry Pi** : Accès natif via sysfs
- ✅ **Windows** : Via cartes USB ou Arduino bridge
- ✅ **Solutions cross-platform** : Abstraction multi-OS

**Projets pratiques réalisés :**
- 🌡️ Station météo complète
- 💧 Système d'arrosage automatique
- 🚨 Détecteur de mouvement avec notifications

### 14.2 Points clés à retenir

1. **Sécurité d'abord** : Toujours vérifier les tensions et courants
2. **Nettoyage systématique** : Libérer les GPIO après utilisation
3. **Gestion d'erreurs** : Try/Finally pour les ressources matérielles
4. **Documentation** : Consulter les datasheets des composants
5. **Tests progressifs** : Commencer simple (LED) avant les projets complexes

### 14.3 Pour aller plus loin

**Sujets avancés à explorer :**
- DMA (Direct Memory Access) pour GPIO haute vitesse
- Interruptions matérielles (edge detection)
- Protocoles industriels (Modbus RTU, CAN bus)
- Vision par ordinateur avec caméra
- Interfaces audio (I²S)
- Écrans (LCD, OLED, e-paper)
- Communication radio (NRF24L01, LoRa)

**Projets suggérés :**
1. **Robot autonome** (moteurs + capteurs ultrasons + caméra)
2. **Domotique complète** (contrôle multi-zones, interface web)
3. **Datalogger industriel** (acquisition multi-canaux, stockage SD)
4. **Console de jeux rétro** (boutons + écran + son)
5. **Station de recharge solaire** (MPPT + monitoring)

### 14.4 Différences Windows/Linux

**Résumé des approches :**

| Aspect | Linux (Raspberry Pi) | Windows |
|--------|---------------------|---------|
| **Accès GPIO** | Natif via /sys/class/gpio | Carte USB ou Arduino |
| **Complexité** | Simple | Moyenne (driver requis) |
| **Performance** | Bonne | Variable |
| **Coût** | Faible (Raspberry Pi) | Moyen (hardware additionnel) |
| **Flexibilité** | Élevée | Limitée |

**Recommandations :**
- **Développement/tests** : Utilisez l'émulation ou Arduino bridge sur Windows
- **Production** : Privilégiez Linux (Raspberry Pi, BeagleBone) pour l'accès direct
- **Portabilité** : Utilisez une couche d'abstraction comme `TCrossPlatformGPIO`

### 14.5 Ressources continues

**Communauté FreePascal/Lazarus :**
- Forum officiel : https://forum.lazarus.freepascal.org/
- Discord FreePascal
- Telegram groups

**Projets open source inspirants :**
- PXL (Platform eXtended Library) : https://github.com/yunkot/pxl
- Brook Framework : https://github.com/risoflora/brookframework
- Nombreux projets sur GitHub avec tag "freepascal embedded"

---

**Félicitations !** Vous disposez maintenant des connaissances nécessaires pour créer des projets IoT et embarqués professionnels avec FreePascal/Lazarus sur Windows et Ubuntu. Les GPIO et interfaces matérielles n'ont plus de secrets pour vous ! 🚀

**Bon développement et bons projets embarqués !** 🔧⚡

⏭️ [Temps réel et RTOS](/14-systemes-embarques-iot/08-temps-reel-rtos.md)
