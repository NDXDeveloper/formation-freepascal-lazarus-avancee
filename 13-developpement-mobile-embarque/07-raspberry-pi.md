🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.7 Raspberry Pi

## Introduction

Le **Raspberry Pi** est un nano-ordinateur monocartes (single-board computer) créé par la Fondation Raspberry Pi au Royaume-Uni. Depuis son lancement en 2012, il est devenu une plateforme incontournable pour l'apprentissage de la programmation, les projets embarqués, l'IoT (Internet des Objets) et le prototypage rapide.

**FreePascal et Lazarus** offrent un excellent support pour le Raspberry Pi, permettant de développer des applications performantes sur cette plateforme ARM avec le confort d'un langage structuré et d'un IDE moderne.

## Pourquoi utiliser FreePascal sur Raspberry Pi ?

### Avantages de FreePascal pour le Raspberry Pi

- **Performances natives** : Code compilé directement pour ARM, pas d'interpréteur
- **Faible consommation de ressources** : Idéal pour les capacités limitées du Raspberry Pi
- **Portabilité** : Le même code fonctionne sur Windows, Linux x86 et ARM
- **Pas de runtime lourd** : Contrairement à Java ou .NET
- **Accès direct au matériel** : GPIO, I2C, SPI, etc.
- **Syntaxe claire et lisible** : Parfait pour l'apprentissage et les projets éducatifs
- **Bibliothèques disponibles** : Accès aux bibliothèques Linux standard

### Cas d'usage typiques

- **Domotique** : Contrôle de systèmes de maison intelligente
- **Stations météo** : Collecte et affichage de données environnementales
- **Systèmes embarqués** : Contrôle de robots, drones, machines
- **Serveurs légers** : Web, FTP, bases de données
- **Affichage d'information** : Panneaux d'information, kiosques
- **IoT et capteurs** : Collecte et transmission de données
- **Apprentissage** : Enseignement de la programmation système

## Les modèles de Raspberry Pi

### Comparaison des modèles récents

| Modèle | CPU | RAM | Prix | Usage typique |
|--------|-----|-----|------|---------------|
| **Pi Zero 2 W** | ARM Cortex-A53 (4 cœurs, 1 GHz) | 512 MB | ~15€ | Projets ultra-compacts |
| **Pi 4 Model B** | ARM Cortex-A72 (4 cœurs, 1.5 GHz) | 2/4/8 GB | 35-75€ | Usage général, serveur |
| **Pi 5** | ARM Cortex-A76 (4 cœurs, 2.4 GHz) | 4/8 GB | 60-80€ | Performances maximales |
| **Pi 400** | ARM Cortex-A72 (4 cœurs, 1.8 GHz) | 4 GB | ~70€ | Ordinateur clavier intégré |

### Architecture ARM et FreePascal

Les Raspberry Pi utilisent des processeurs **ARM** avec différentes architectures :

- **ARMv6** : Raspberry Pi 1, Zero (FPC : `-Parm`)
- **ARMv7** : Raspberry Pi 2, 3 (32 bits) (FPC : `-Parmv7`)
- **ARMv8/AArch64** : Raspberry Pi 3, 4, 5 (64 bits) (FPC : `-Paarch64`)

FreePascal supporte nativement toutes ces architectures ARM.

## Systèmes d'exploitation compatibles

### Raspberry Pi OS (recommandé)

Anciennement appelé **Raspbian**, c'est le système officiel basé sur Debian :

```bash
# Versions disponibles
- Raspberry Pi OS (32 bits) : Pour tous les modèles
- Raspberry Pi OS (64 bits) : Pour Pi 3, 4, 5 uniquement
- Raspberry Pi OS Lite : Version sans interface graphique (headless)
```

**Avantages** :
- Optimisé pour le Raspberry Pi
- Support matériel complet
- Grande communauté
- Packages pré-configurés

### Ubuntu pour Raspberry Pi

Ubuntu propose des versions officielles pour Raspberry Pi :

```bash
# Versions disponibles
- Ubuntu Server (64 bits)
- Ubuntu Desktop (64 bits)
```

**Avantages** :
- Écosystème Ubuntu familier
- Support LTS (Long Term Support)
- Packages Ubuntu standards

### Autres systèmes

- **DietPi** : Système ultra-léger et optimisé
- **LibreELEC** : Pour centre multimédia
- **RetroPie** : Pour émulation de jeux rétro
- **Arch Linux ARM** : Pour utilisateurs avancés

> 💡 **Recommandation** : Pour débuter avec FreePascal, utilisez **Raspberry Pi OS (64 bits)** sur un Raspberry Pi 4 ou 5.

## Installation de FreePascal sur Raspberry Pi

### Méthode 1 : Installation depuis les dépôts (la plus simple)

```bash
# Mise à jour du système
sudo apt update  
sudo apt upgrade

# Installation de FreePascal et Lazarus
sudo apt install fpc lazarus

# Vérification de l'installation
fpc -version
```

Cette méthode installe généralement une version légèrement ancienne mais stable.

**Version typique installée** : FPC 3.2.0 ou 3.2.2

### Méthode 2 : Installation via FpcUpDeluxe

Pour obtenir la dernière version de FreePascal et Lazarus :

```bash
# Télécharger FpcUpDeluxe
wget https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/download/1.8.2/fpcupdeluxe-arm-linux  
chmod +x fpcupdeluxe-arm-linux

# Lancer FpcUpDeluxe
./fpcupdeluxe-arm-linux
```

FpcUpDeluxe télécharge et compile automatiquement les dernières versions de FPC et Lazarus.

### Méthode 3 : Compilation depuis les sources

Pour les utilisateurs avancés ou pour des besoins spécifiques :

```bash
# Installation des dépendances
sudo apt install build-essential subversion

# Téléchargement des sources FPC
svn checkout https://svn.freepascal.org/svn/fpc/tags/release_3_2_2 fpc-3.2.2  
cd fpc-3.2.2

# Compilation
make clean  
make all  
sudo make install

# Configuration
sudo ln -sf /usr/local/lib/fpc/3.2.2/ppcarm /usr/local/bin/ppcarm
```

> ⚠️ **Note** : La compilation peut prendre de 30 minutes à plusieurs heures selon le modèle de Raspberry Pi.

## Configuration de l'environnement

### Variables d'environnement

Ajoutez ces lignes à votre `~/.bashrc` :

```bash
# FreePascal
export PATH=$PATH:/usr/local/lib/fpc/3.2.2

# Configuration du compilateur
export FPC=/usr/local/bin/fpc

# Librairies
export FPCDIR=/usr/local/lib/fpc/3.2.2
```

Puis rechargez la configuration :

```bash
source ~/.bashrc
```

### Configuration de Lazarus pour Raspberry Pi

Si vous utilisez Lazarus avec interface graphique (uniquement sur Raspberry Pi OS Desktop) :

1. **Lancer Lazarus** :
   ```bash
   lazarus-ide
   ```

2. **Configurer le compilateur** :
   - Menu `Outils` → `Options`
   - Section `Compilateur FreePascal`
   - Vérifier le chemin vers `fpc`

3. **Configurer le débogueur** :
   - Menu `Outils` → `Options`
   - Section `Débogueur`
   - Sélectionner `GNU debugger (gdb)`

## Premier programme sur Raspberry Pi

### Programme console simple

Créez un fichier `hello_rpi.pas` :

```pascal
program HelloRaspberryPi;

{$mode objfpc}{$H+}

uses
  SysUtils;

begin
  WriteLn('=================================');
  WriteLn('  Bonjour depuis Raspberry Pi!   ');
  WriteLn('=================================');
  WriteLn;
  WriteLn('Architecture : ', {$I %FPCTARGET%});
  WriteLn('Système      : ', {$I %FPCTARGETOS%});
  WriteLn('Date         : ', DateTimeToStr(Now));
  WriteLn('Utilisateur  : ', GetEnvironmentVariable('USER'));
  WriteLn;
end.
```

### Compilation et exécution

```bash
# Compilation
fpc hello_rpi.pas

# Exécution
./hello_rpi
```

**Sortie attendue** :
```
=================================
  Bonjour depuis Raspberry Pi!
=================================

Architecture : aarch64-linux  
Système      : linux  
Date         : 03/10/2025 14:30:00  
Utilisateur  : pi
```

## Accès aux ressources système

### Informations matérielles

```pascal
program SystemInfo;

{$mode objfpc}{$H+}

uses
  SysUtils, Process;

function ExecuteCommand(const cmd: string): string;  
var
  process: TProcess;
  outputList: TStringList;
begin
  Result := '';
  process := TProcess.Create(nil);
  outputList := TStringList.Create;
  try
    process.CommandLine := cmd;
    process.Options := process.Options + [poWaitOnExit, poUsePipes];
    process.Execute;
    outputList.LoadFromStream(process.Output);
    Result := outputList.Text;
  finally
    outputList.Free;
    process.Free;
  end;
end;

begin
  WriteLn('===== Informations Système Raspberry Pi =====');
  WriteLn;

  WriteLn('Modèle:');
  Write(ExecuteCommand('cat /proc/device-tree/model'));
  WriteLn;

  WriteLn('CPU:');
  Write(ExecuteCommand('lscpu | grep "Model name"'));
  WriteLn;

  WriteLn('Mémoire:');
  Write(ExecuteCommand('free -h | grep Mem'));
  WriteLn;

  WriteLn('Température CPU:');
  Write(ExecuteCommand('vcgencmd measure_temp'));
  WriteLn;

  WriteLn('Stockage:');
  Write(ExecuteCommand('df -h | grep root'));
end.
```

### Lecture de la température du CPU

```pascal
program CPUTemperature;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

function GetCPUTemp: Double;  
var
  tempFile: TextFile;
  tempStr: string;
  tempValue: Integer;
begin
  Result := 0.0;
  if FileExists('/sys/class/thermal/thermal_zone0/temp') then
  begin
    AssignFile(tempFile, '/sys/class/thermal/thermal_zone0/temp');
    Reset(tempFile);
    try
      ReadLn(tempFile, tempStr);
      tempValue := StrToIntDef(tempStr, 0);
      Result := tempValue / 1000.0; // Conversion en degrés Celsius
    finally
      CloseFile(tempFile);
    end;
  end;
end;

begin
  WriteLn('Température CPU: ', GetCPUTemp:0:1, '°C');

  // Surveillance continue
  Write('Surveillance continue (Ctrl+C pour arrêter)...');
  WriteLn;
  repeat
    Write(#13, 'Température: ', GetCPUTemp:0:1, '°C  ');
    Sleep(1000); // Attendre 1 seconde
  until False;
end.
```

## Interface graphique sur Raspberry Pi

### Application LCL simple

Créez une application graphique légère avec Lazarus :

```pascal
program SimpleGUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // LCL widgetset
  Forms, Controls, StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    InfoLabel: TLabel;
    TempLabel: TLabel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    function GetCPUTemp: Double;
  end;

var
  MainForm: TMainForm;

procedure TMainForm.FormCreate(Sender: TObject);  
begin
  Caption := 'Raspberry Pi - FreePascal';
  Width := 400;
  Height := 200;
  Position := poScreenCenter;

  InfoLabel := TLabel.Create(Self);
  InfoLabel.Parent := Self;
  InfoLabel.Caption := 'Surveillance Raspberry Pi';
  InfoLabel.Left := 20;
  InfoLabel.Top := 20;
  InfoLabel.Font.Size := 14;

  TempLabel := TLabel.Create(Self);
  TempLabel.Parent := Self;
  TempLabel.Caption := 'Température: --°C';
  TempLabel.Left := 20;
  TempLabel.Top := 60;
  TempLabel.Font.Size := 12;

  Timer := TTimer.Create(Self);
  Timer.Interval := 1000;
  Timer.OnTimer := @TimerTimer;
  Timer.Enabled := True;
end;

function TMainForm.GetCPUTemp: Double;  
var
  tempFile: TextFile;
  tempStr: string;
begin
  Result := 0.0;
  if FileExists('/sys/class/thermal/thermal_zone0/temp') then
  begin
    AssignFile(tempFile, '/sys/class/thermal/thermal_zone0/temp');
    Reset(tempFile);
    try
      ReadLn(tempFile, tempStr);
      Result := StrToInt(tempStr) / 1000.0;
    finally
      CloseFile(tempFile);
    end;
  end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);  
begin
  TempLabel.Caption := Format('Température: %.1f°C', [GetCPUTemp]);
end;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

### Compilation de l'application graphique

```bash
# Compiler avec support LCL
lazbuild --build-mode=Release SimpleGUI.lpi

# Ou avec fpc directement (plus complexe)
fpc -Fu/usr/lib/lazarus/lcl/units/arm-linux \
    -Fu/usr/lib/lazarus/lcl/units/arm-linux/gtk2 \
    SimpleGUI.pas
```

## Optimisation pour Raspberry Pi

### Options de compilation recommandées

```bash
# Pour Raspberry Pi 3/4/5 (ARMv7/ARMv8)
fpc -O3 -CpARMV7A -CfVFPV3 monprogramme.pas

# Pour optimisation maximale
fpc -O4 -OoFASTMATH -CpARMV7A -CfVFPV3 monprogramme.pas
```

**Explications des options** :
- `-O3` ou `-O4` : Niveau d'optimisation (3 = élevé, 4 = maximum)
- `-CpARMV7A` : Cibler l'architecture ARMv7-A
- `-CfVFPV3` : Utiliser les instructions VFP (Floating Point) version 3
- `-OoFASTMATH` : Optimisations mathématiques agressives

### Fichier de configuration projet

Créez un fichier `fpc.cfg` dans votre projet :

```ini
# Configuration pour Raspberry Pi optimisé

# Architecture ARM
-CpARMV7A
-CfVFPV3

# Optimisations
-O3
-OoREGVAR
-OoSTACKFRAME
-OoPEEPHOLE

# Options de link
-Xs
-XX

# Debugging (à commenter pour la production)
#-g
#-gl

# Chemins des unités
-Fu/usr/lib/fpc/$fpcversion/units/$fpctarget
-Fu/usr/lib/lazarus/lcl/units/$fpctarget
```

## Accès distant et développement headless

### SSH (Secure Shell)

Activer SSH sur le Raspberry Pi :

```bash
# Sur le Raspberry Pi
sudo raspi-config
# Aller dans "Interface Options" → "SSH" → "Enable"

# Ou directement
sudo systemctl enable ssh  
sudo systemctl start ssh
```

Se connecter depuis un autre ordinateur :

```bash
# Depuis Windows, Linux ou macOS
ssh pi@raspberrypi.local
# Mot de passe par défaut : raspberry (à changer!)
```

### Transfert de fichiers avec SCP

```bash
# Envoyer un fichier vers le Raspberry Pi
scp monprogramme.pas pi@raspberrypi.local:/home/pi/

# Récupérer un fichier depuis le Raspberry Pi
scp pi@raspberrypi.local:/home/pi/resultat.txt ./
```

### Développement avec VS Code Remote

Visual Studio Code peut se connecter au Raspberry Pi via SSH :

1. Installer l'extension **Remote - SSH**
2. Se connecter au Raspberry Pi
3. Éditer et compiler directement sur le Pi

### Montage réseau (SSHFS)

Monter le système de fichiers du Raspberry Pi sur votre PC :

```bash
# Sur Ubuntu/Linux
sudo apt install sshfs  
mkdir ~/raspberry  
sshfs pi@raspberrypi.local:/home/pi ~/raspberry

# Sur Windows (avec WinFsp + SSHFS-Win)
# Utiliser l'interface graphique ou la commande
net use R: \\sshfs\pi@raspberrypi.local
```

## Gestion de l'alimentation et performances

### Surveillance de la consommation

```pascal
program PowerMonitor;

{$mode objfpc}{$H+}

uses
  SysUtils;

function GetThrottleStatus: string;  
var
  cmd: string;
begin
  // Commande vcgencmd pour lire l'état de throttling
  cmd := 'vcgencmd get_throttled';
  // Implémentation simplifiée
  Result := 'Voir documentation vcgencmd';
end;

function GetVoltage: string;  
var
  cmd: string;
begin
  cmd := 'vcgencmd measure_volts';
  Result := 'Voir documentation vcgencmd';
end;

begin
  WriteLn('État du système:');
  WriteLn('  Throttle: ', GetThrottleStatus);
  WriteLn('  Voltage : ', GetVoltage);
end.
```

### Overclocking (avancé)

Modifier `/boot/config.txt` pour overclocker (à vos risques) :

```ini
# Exemple pour Raspberry Pi 4
over_voltage=6  
arm_freq=2000
```

> ⚠️ **Attention** : L'overclocking peut réduire la durée de vie du Raspberry Pi et nécessite un refroidissement adéquat.

## Ressources et documentation

### Documentation officielle

- **Site officiel Raspberry Pi** : https://www.raspberrypi.com/
- **Documentation Raspberry Pi** : https://www.raspberrypi.com/documentation/
- **Forum Raspberry Pi** : https://forums.raspberrypi.com/

### FreePascal sur ARM

- **Wiki FreePascal ARM** : https://wiki.freepascal.org/ARM_Linux
- **Forum FreePascal** : https://forum.lazarus.freepascal.org/
- **Documentation FPC** : https://www.freepascal.org/docs.html

### Communauté francophone

- **Forum Lazarus francophone** : https://forum.lazarus.freepascal.org/index.php/board,8.0.html
- **Developpez.com Pascal** : Forums et tutoriels en français
- **Discord/Telegram FreePascal FR** : Communautés actives

## Limitations et considérations

### Limitations matérielles

- **RAM limitée** : 512 MB à 8 GB selon le modèle (attention aux fuites mémoire)
- **CPU modeste** : Projets lourds peuvent être lents
- **Stockage SD** : Plus lent qu'un SSD, usure possible
- **Pas de GPU puissant** : Graphiques 3D complexes limités
- **Refroidissement** : Throttling si température élevée

### Bonnes pratiques

1. **Optimiser le code** : Utiliser les options de compilation appropriées
2. **Libérer la mémoire** : Attention aux objets et structures dynamiques
3. **Éviter les boucles infinies** : Consommation CPU inutile
4. **Utiliser des threads avec parcimonie** : Capacités limitées
5. **Surveiller la température** : Utiliser un dissipateur thermique
6. **Alimentation stable** : Utiliser une alimentation de qualité (3A minimum pour Pi 4)

## Prochaines étapes

Dans les sections suivantes, nous verrons :

- **13.7.1 Cross-compilation depuis Windows** : Compiler sur PC pour Raspberry Pi
- **13.7.2 Développement natif sur Ubuntu ARM** : Développer directement sur le Pi

---

## Conclusion de l'introduction

Le **Raspberry Pi** est une excellente plateforme pour développer avec **FreePascal** des applications embarquées, des systèmes IoT et des projets éducatifs. Avec un support ARM mature, des performances natives et une consommation de ressources minimale, FreePascal est un choix judicieux pour le développement sur Raspberry Pi.

Que vous développiez directement sur le Raspberry Pi ou que vous utilisiez la cross-compilation depuis Windows ou Linux, FreePascal vous offre tous les outils nécessaires pour créer des applications performantes et portables.

> 💡 **Conseil** : Commencez par des projets simples (affichage de capteurs, serveur web basique) avant de vous lancer dans des applications complexes. Le Raspberry Pi est une plateforme idéale pour apprendre et expérimenter!

⏭️ [Cross-compilation depuis Windows](/13-developpement-mobile-embarque/07.1-cross-compilation-depuis-windows.md)
