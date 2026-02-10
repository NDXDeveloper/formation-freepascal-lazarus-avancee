🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.8 Configuration d'environnements de développement dual-boot/VM

## Introduction : Pourquoi plusieurs environnements ?

### Le défi du développement multi-plateforme

Développer des applications vraiment multi-plateformes nécessite de **tester sur chaque système cible**. Même si FreePascal/Lazarus permet la cross-compilation, rien ne remplace les tests sur l'OS réel :

```
Développement sur Windows → Compile pour Linux → Fonctionne ?
                                                    ↓
                                            Besoin de tester sur Linux réel !
```

### Les différentes approches

```
┌────────────────────────────────────────────┐
│        Stratégies Multi-OS                 │
├────────────────────────────────────────────┤
│                                            │
│  1. DUAL-BOOT                              │
│     → Deux OS sur la même machine          │
│     → Performance native                   │
│     → Redémarrage nécessaire               │
│                                            │
│  2. MACHINES VIRTUELLES (VM)               │
│     → OS invité dans OS hôte               │
│     → Plusieurs OS simultanés              │
│     → Performance réduite                  │
│                                            │
│  3. CONTENEURS (Docker)                    │
│     → Environnements isolés                │
│     → Léger et rapide                      │
│     → Linux uniquement*                    │
│                                            │
│  4. WSL (Windows Subsystem for Linux)      │
│     → Linux dans Windows                   │
│     → Intégration native                   │
│     → Windows 10/11 uniquement             │
│                                            │
└────────────────────────────────────────────┘
```

## Partie 1 : Configuration Dual-Boot

### Comprendre le dual-boot

Le dual-boot permet d'installer **deux systèmes d'exploitation** sur le même ordinateur et de choisir au démarrage :

```
Démarrage PC
     ↓
Menu de démarrage (GRUB/Bootloader)
     ├── Windows 11
     └── Ubuntu 22.04
```

### Prérequis pour le dual-boot

#### Matériel nécessaire

- **Espace disque** : Minimum 50 GB par OS (100 GB recommandé)
- **RAM** : 8 GB minimum (16 GB confortable)
- **UEFI/BIOS** : Mode UEFI recommandé pour OS modernes
- **Clé USB** : 8 GB pour créer les médias d'installation

#### Préparation du disque

```
Disque avant dual-boot (500 GB) :
┌────────────────────────────────┐
│     Windows (500 GB)           │
└────────────────────────────────┘

Disque après partitionnement :
┌─────────┬──────────┬───────────┐
│ Windows │  Ubuntu  │   Données │
│ 200 GB  │  150 GB  │   150 GB  │
└─────────┴──────────┴───────────┘
```

### Installation Windows + Linux

#### Étape 1 : Sauvegarder vos données

```powershell
# Windows - Créer une image système
wbadmin start backup -backupTarget:E: -include:C: -allCritical -quiet

# Ou utiliser des outils tiers
# - Clonezilla
# - Acronis True Image
# - Macrium Reflect
```

#### Étape 2 : Réduire la partition Windows

```powershell
# PowerShell Admin - Vérifier l'espace
Get-Volume

# Utiliser Gestion des disques
diskmgmt.msc

# Ou en ligne de commande
diskpart
> list volume
> select volume C
> shrink desired=150000  # 150 GB pour Linux
```

#### Étape 3 : Créer le média d'installation Linux

```bash
# Sur Linux existant
sudo dd if=ubuntu-22.04.iso of=/dev/sdb bs=4M status=progress

# Sur Windows - utiliser Rufus ou balenaEtcher
# 1. Télécharger Rufus
# 2. Sélectionner l'ISO Ubuntu
# 3. Mode UEFI/GPT
# 4. Créer la clé bootable
```

#### Étape 4 : Installation d'Ubuntu

Configuration des partitions recommandée :

```
Partitions Linux (150 GB total) :
├── /boot/efi  (512 MB)  - Partition EFI
├── /          (50 GB)   - Racine système
├── /home      (95 GB)   - Données utilisateur
└── swap       (4 GB)    - Mémoire virtuelle
```

#### Étape 5 : Configuration du bootloader (GRUB)

```bash
# Après installation, configurer GRUB
sudo nano /etc/default/grub

# Options importantes
GRUB_DEFAULT=0              # OS par défaut (0=premier)  
GRUB_TIMEOUT=10            # Délai en secondes  
GRUB_DISABLE_OS_PROBER=false  # Détection Windows

# Appliquer les changements
sudo update-grub

# Vérifier la détection de Windows
sudo os-prober
```

### Configuration Lazarus en dual-boot

#### Partage de projets entre OS

```bash
# Créer une partition de données NTFS commune
# (accessible en lecture/écriture par Windows et Linux)

# Sur Linux, monter automatiquement
sudo blkid  # Trouver UUID de la partition  
sudo nano /etc/fstab

# Ajouter ligne :
UUID=XXXX-XXXX /mnt/shared ntfs-3g defaults,uid=1000,gid=1000,umask=022 0 0

# Structure recommandée sur partition partagée :
/mnt/shared/
├── LazarusProjects/
│   ├── Project1/
│   ├── Project2/
│   └── Libraries/
├── Documentation/
└── Tools/
```

#### Synchronisation des configurations

Script `sync-lazarus-config.sh` :

```bash
#!/bin/bash
# Synchroniser config Lazarus entre OS

SHARED_DIR="/mnt/shared/LazarusConfig"  
LINUX_CONFIG="$HOME/.lazarus"  
WIN_CONFIG="/mnt/windows/Users/$USER/AppData/Local/lazarus"

# Fonction de synchronisation
sync_configs() {
    echo "Synchronisation des configurations..."

    # Sauvegarder config actuelle
    cp -r "$LINUX_CONFIG" "$SHARED_DIR/linux-backup-$(date +%Y%m%d)"

    # Synchroniser les fichiers importants
    rsync -av --exclude='*.compiled' \
              --exclude='lib/' \
              --exclude='backup/' \
              "$LINUX_CONFIG/" "$SHARED_DIR/current/"
}

# Menu
echo "1. Sauvegarder config Linux"  
echo "2. Restaurer depuis Windows"  
echo "3. Synchronisation bidirectionnelle"  
read -p "Choix : " choice

case $choice in
    1) sync_configs ;;
    2) rsync -av "$WIN_CONFIG/" "$LINUX_CONFIG/" ;;
    3) unison "$LINUX_CONFIG" "$WIN_CONFIG" ;;
esac
```

### Problèmes courants du dual-boot

#### Problème : Windows Fast Startup

```powershell
# Désactiver Fast Startup (cause des problèmes de montage)
powercfg /h off

# Ou via GUI :
# Panneau de configuration → Options d'alimentation
# → Choisir l'action du bouton d'alimentation
# → Décocher "Activer le démarrage rapide"
```

#### Problème : Heure décalée entre OS

```bash
# Linux utilise UTC, Windows utilise l'heure locale
# Solution : Configurer Windows pour UTC

# Sur Linux :
timedatectl set-local-rtc 1

# Ou sur Windows (Registry) :
# HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\TimeZoneInformation
# Ajouter DWORD "RealTimeIsUniversal" = 1
```

## Partie 2 : Machines Virtuelles (VM)

### Comprendre la virtualisation

Une machine virtuelle simule un ordinateur complet dans votre OS :

```
┌─────────────────────────────────────┐
│         OS Hôte (Windows)           │
├─────────────────────────────────────┤
│      Hyperviseur (VirtualBox)       │
├──────────────┬──────────────────────┤
│   VM Linux   │    VM macOS*         │
│   (Ubuntu)   │   (Hackintosh)       │
└──────────────┴──────────────────────┘
```

### Choix de l'hyperviseur

| Hyperviseur | Gratuit | Performance | Facilité | Plateformes |
|-------------|---------|-------------|----------|-------------|
| **VirtualBox** | ✅ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | Win/Lin/Mac |
| **VMware Player** | ✅* | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | Win/Lin |
| **VMware Workstation** | ❌ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | Win/Lin |
| **Hyper-V** | ✅** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | Windows Pro |
| **QEMU/KVM** | ✅ | ⭐⭐⭐⭐⭐ | ⭐⭐ | Linux |
| **Parallels** | ❌ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | macOS |

*Usage personnel uniquement
**Inclus dans Windows Pro/Enterprise

### Installation de VirtualBox

#### Sur Windows

```powershell
# Via Chocolatey
choco install virtualbox

# Ou téléchargement manuel
# https://www.virtualbox.org/wiki/Downloads
# VirtualBox-7.0.x-Win.exe

# Extension Pack (USB 3.0, RDP, etc.)
choco install virtualbox-guest-additions-guest.install
```

#### Sur Linux

```bash
# Ubuntu/Debian
sudo apt update  
sudo apt install virtualbox virtualbox-ext-pack

# Ajouter utilisateur au groupe vboxusers
sudo usermod -aG vboxusers $USER

# Redémarrer ou recharger les groupes
newgrp vboxusers
```

### Création d'une VM pour développement

#### Configuration recommandée pour VM de développement

```
VM Ubuntu pour Lazarus :
├── CPU : 2-4 cœurs
├── RAM : 4-8 GB
├── Disque : 50 GB (dynamique)
├── Vidéo : 128 MB, 3D activé
├── Réseau : NAT + Host-only
└── Dossiers partagés : Projets
```

#### Script de création automatique

```bash
#!/bin/bash
# create-dev-vm.sh - Créer VM de développement

VM_NAME="Ubuntu-Dev-Lazarus"  
VM_RAM=4096  
VM_VRAM=128  
VM_DISK=51200  
VM_CPUS=2

# Créer la VM
VBoxManage createvm --name "$VM_NAME" --ostype Ubuntu_64 --register

# Configuration matérielle
VBoxManage modifyvm "$VM_NAME" \
    --memory $VM_RAM \
    --vram $VM_VRAM \
    --cpus $VM_CPUS \
    --ioapic on \
    --boot1 dvd \
    --boot2 disk \
    --boot3 none \
    --audio none \
    --usb on \
    --usbehci on

# Créer et attacher disque
VBoxManage createhd --filename "$VM_NAME.vdi" --size $VM_DISK  
VBoxManage storagectl "$VM_NAME" --name "SATA" --add sata --controller IntelAhci  
VBoxManage storageattach "$VM_NAME" --storagectl "SATA" --port 0 --device 0 \
    --type hdd --medium "$VM_NAME.vdi"

# Attacher ISO Ubuntu
VBoxManage storageattach "$VM_NAME" --storagectl "SATA" --port 1 --device 0 \
    --type dvddrive --medium ubuntu-22.04-desktop-amd64.iso

# Configuration réseau
VBoxManage modifyvm "$VM_NAME" --nic1 nat  
VBoxManage modifyvm "$VM_NAME" --nic2 hostonly --hostonlyadapter2 vboxnet0

# Activer les fonctionnalités
VBoxManage modifyvm "$VM_NAME" --clipboard bidirectional  
VBoxManage modifyvm "$VM_NAME" --draganddrop bidirectional  
VBoxManage modifyvm "$VM_NAME" --accelerate3d on

echo "VM $VM_NAME créée avec succès !"
```

### Optimisation des performances VM

#### Activation de la virtualisation matérielle

```powershell
# Windows - Vérifier support virtualisation
systeminfo | findstr /i "virtualization"

# Si désactivé, activer dans BIOS/UEFI :
# - Intel : VT-x
# - AMD : AMD-V
# - Activer aussi VT-d/IOMMU si disponible
```

#### Configuration optimale VirtualBox

```bash
# Paravirtualisation
VBoxManage modifyvm "VM-Name" --paravirtprovider kvm  # Pour Linux  
VBoxManage modifyvm "VM-Name" --paravirtprovider hyperv  # Pour Windows

# Nested Paging (important !)
VBoxManage modifyvm "VM-Name" --nestedpaging on

# Large Pages
VBoxManage modifyvm "VM-Name" --largepages on

# Compilation JIT
VBoxManage modifyvm "VM-Name" --vtxvpid on  
VBoxManage modifyvm "VM-Name" --vtxux on
```

### Dossiers partagés et intégration

#### Configuration des dossiers partagés

```bash
# Créer un dossier partagé permanent
VBoxManage sharedfolder add "VM-Name" \
    --name "Projects" \
    --hostpath "C:\LazarusProjects" \
    --automount

# Dans la VM Linux, monter automatiquement
sudo nano /etc/fstab
# Ajouter :
Projects /home/user/SharedProjects vboxsf defaults,uid=1000,gid=1000 0 0

# Ou montage manuel
sudo mount -t vboxsf Projects /home/user/SharedProjects
```

#### Guest Additions pour intégration complète

```bash
# Dans la VM, installer Guest Additions
sudo apt update  
sudo apt install build-essential dkms linux-headers-$(uname -r)

# Menu VirtualBox : Périphériques → Insérer l'image CD des Additions invité
sudo mount /dev/cdrom /mnt  
sudo /mnt/VBoxLinuxAdditions.run

# Redémarrer
sudo reboot

# Vérifier
lsmod | grep vbox
```

## Partie 3 : Windows Subsystem for Linux (WSL)

### Comprendre WSL

WSL permet d'exécuter Linux **directement dans Windows** sans VM traditionnelle :

```
Windows 11
├── Applications Windows natives
├── WSL2
│   ├── Ubuntu
│   ├── Debian
│   └── Fedora
└── Intégration complète (fichiers, réseau, GPU)
```

### Installation de WSL2

```powershell
# PowerShell en administrateur

# Activer WSL et VM Platform
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart  
dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart

# Redémarrer

# Définir WSL2 par défaut
wsl --set-default-version 2

# Installer Ubuntu
wsl --install -d Ubuntu-22.04

# Ou depuis Microsoft Store
# Rechercher "Ubuntu" et installer

# Vérifier installation
wsl --list --verbose
```

### Configuration de Lazarus dans WSL

#### Installation de Lazarus

```bash
# Dans WSL Ubuntu
sudo apt update  
sudo apt upgrade

# Installer Lazarus et dépendances GUI
sudo apt install lazarus

# Pour GUI, installer X Server sur Windows :
# - VcXsrv
# - X410
# - WSLg (Windows 11 natif)

# Configuration pour VcXsrv
echo 'export DISPLAY=:0.0' >> ~/.bashrc  
source ~/.bashrc

# Tester
lazarus-ide &
```

#### Accès aux fichiers Windows depuis WSL

```bash
# Windows drives montés dans /mnt/
cd /mnt/c/Users/YourName/Documents/LazarusProjects

# Créer lien symbolique pour accès facile
ln -s /mnt/c/Users/YourName/Documents/LazarusProjects ~/WinProjects

# Attention aux permissions !
# Configurer dans /etc/wsl.conf :
sudo nano /etc/wsl.conf

[automount]
enabled = true  
options = "metadata,umask=22,fmask=11"
```

### WSLg (GUI natif Windows 11)

```powershell
# Windows 11 inclut WSLg (GUI support)
# Pas besoin de X Server externe !

# Vérifier support
wsl --version

# Si WSL version >= 1.0.0, WSLg inclus
# Applications Linux GUI s'affichent comme apps Windows
```

```bash
# Dans WSL, lancer directement
lazarus-ide

# Apparaît dans la barre des tâches Windows
# Menu Démarrer liste les apps Linux installées
```

## Partie 4 : Conteneurs Docker

### Docker pour développement Lazarus

Docker permet des environnements de développement **reproductibles** :

```dockerfile
# Dockerfile pour environnement Lazarus
FROM ubuntu:22.04

# Variables d'environnement
ENV DEBIAN_FRONTEND=noninteractive  
ENV TZ=Europe/Paris

# Installation des dépendances
RUN apt-get update && apt-get install -y \
    lazarus \
    fpc \
    fpc-source \
    git \
    make \
    wget \
    xvfb \
    x11vnc \
    && apt-get clean

# Utilisateur non-root
RUN useradd -m -s /bin/bash developer  
USER developer  
WORKDIR /home/developer

# Port VNC pour accès GUI
EXPOSE 5900

# Script de démarrage
COPY start.sh /home/developer/  
CMD ["/home/developer/start.sh"]
```

### Docker Compose pour environnement complet

```yaml
# docker-compose.yml
version: '3.8'

services:
  lazarus-dev:
    build: .
    container_name: lazarus-dev
    volumes:
      - ./projects:/home/developer/projects
      - lazarus-config:/home/developer/.lazarus
    ports:
      - "5900:5900"  # VNC
      - "8080:8080"  # App web si nécessaire
    environment:
      - DISPLAY=:99
    networks:
      - dev-network

  postgres:
    image: postgres:14
    container_name: dev-postgres
    environment:
      POSTGRES_DB: testdb
      POSTGRES_USER: developer
      POSTGRES_PASSWORD: devpass
    volumes:
      - postgres-data:/var/lib/postgresql/data
    networks:
      - dev-network

volumes:
  lazarus-config:
  postgres-data:

networks:
  dev-network:
```

### Utilisation pratique

```bash
# Construire et lancer
docker-compose up -d

# Accéder au conteneur
docker exec -it lazarus-dev bash

# Ou via VNC
# Installer VNC Viewer
# Connecter à localhost:5900

# Compiler un projet
docker exec lazarus-dev lazbuild /home/developer/projects/myapp.lpi

# Arrêter
docker-compose down
```

## Comparaison des approches

### Tableau comparatif détaillé

| Critère | Dual-Boot | VM | WSL | Docker |
|---------|-----------|-----|-----|--------|
| **Performance** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Facilité** | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Isolation** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Multi-OS simultané** | ❌ | ✅ | ⚠️ | ⚠️ |
| **Ressources** | Minimal | Élevé | Moyen | Faible |
| **GUI natif** | ✅ | ⚠️ | ⚠️ | ❌ |
| **Snapshots** | ❌ | ✅ | ❌ | ✅ |
| **Portabilité** | ❌ | ✅ | ❌ | ✅ |

### Recommandations par cas d'usage

#### Développeur solo, machine puissante
**→ Dual-boot**
- Performance maximale
- Test réel des OS
- Idéal pour applications natives

#### Équipe de développement
**→ VM ou Docker**
- Environnements standardisés
- Facile à distribuer
- Snapshots pour tests

#### Développement Windows avec tests Linux
**→ WSL**
- Intégration Windows excellente
- Pas de redémarrage
- Accès fichiers transparent

#### CI/CD et tests automatisés
**→ Docker**
- Reproductibilité parfaite
- Intégration pipelines
- Scalabilité

## Configuration de workflow multi-environnement

### Synchronisation avec Git

```bash
# Structure de projet multi-plateforme
MyProject/
├── .git/
├── src/               # Code source
├── build/
│   ├── windows/      # Build Windows
│   ├── linux/        # Build Linux
│   └── macos/        # Build macOS
├── scripts/
│   ├── build-windows.bat
│   ├── build-linux.sh
│   └── build-all.sh
└── .gitignore

# .gitignore
*.exe
*.o
*.ppu
*.compiled
lib/  
backup/  
build/*/
```

### Script de build universel

```bash
#!/bin/bash
# build-all.sh - Compilation multi-plateforme

PROJECT="myproject.lpi"

# Fonction de build
build_platform() {
    local PLATFORM=$1
    local OUTPUT_DIR="build/$PLATFORM"

    echo "Building for $PLATFORM..."
    mkdir -p "$OUTPUT_DIR"

    case $PLATFORM in
        windows)
            if command -v wine &> /dev/null; then
                wine lazbuild --os=win64 --cpu=x86_64 "$PROJECT"
            else
                echo "Wine not found, trying WSL..."
                wsl lazbuild --os=win64 --cpu=x86_64 "$PROJECT"
            fi
            ;;
        linux)
            lazbuild --os=linux --cpu=x86_64 "$PROJECT"
            ;;
        macos)
            if [[ "$OSTYPE" == "darwin"* ]]; then
                lazbuild --os=darwin --cpu=x86_64 "$PROJECT"
            else
                echo "macOS build requires macOS host"
            fi
            ;;
    esac

    # Copier les binaires
    find . -name "*.exe" -exec mv {} "$OUTPUT_DIR/" \;
    find . -name "$PROJECT" -executable -exec mv {} "$OUTPUT_DIR/" \;
}

# Build pour toutes les plateformes
for platform in windows linux macos; do
    build_platform $platform
done

echo "Build complete!"
```

### Tests automatisés multi-plateforme

```yaml
# .github/workflows/multi-platform-test.yml
name: Multi-Platform Build

on: [push, pull_request]

jobs:
  build-windows:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Lazarus
        run: choco install lazarus
      - name: Build
        run: lazbuild myproject.lpi
      - name: Test
        run: ./myproject.exe --test

  build-linux:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Lazarus
        run: |
          sudo apt update
          sudo apt install lazarus
      - name: Build
        run: lazbuild myproject.lpi
      - name: Test
        run: ./myproject --test

  build-macos:
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Lazarus
        run: brew install --cask lazarus
      - name: Build
        run: lazbuild myproject.lpi
      - name: Test
        run: ./myproject --test
```

## Outils et utilitaires

### Gestionnaires d'environnements

#### Vagrant pour VM automatisées

```ruby
# Vagrantfile
Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/jammy64"

  config.vm.provider "virtualbox" do |vb|
    vb.memory = "4096"
    vb.cpus = 2
  end

  config.vm.provision "shell", inline: <<-SHELL
    apt-get update
    apt-get install -y lazarus fpc
  SHELL

  config.vm.synced_folder "./projects", "/home/vagrant/projects"
end
```

```bash
# Utilisation
vagrant up        # Créer et démarrer VM  
vagrant ssh       # Se connecter  
vagrant halt      # Arrêter  
vagrant destroy   # Supprimer
```

#### Multipass pour VM légères

```bash
# Ubuntu/Linux
sudo snap install multipass

# Créer VM Ubuntu avec Lazarus
multipass launch --name lazarus-dev --cpus 2 --mem 4G --disk 20G

# Script d'installation
multipass exec lazarus-dev -- bash -c "
  sudo apt update
  sudo apt install -y lazarus
"

# Monter dossier local
multipass mount ./projects lazarus-dev:/home/ubuntu/projects

# Se connecter
multipass shell lazarus-dev
```

## Meilleures pratiques

### Organisation des projets

```
Development/
├── VMs/                    # Machines virtuelles
│   ├── Ubuntu-Dev/
│   └── Windows-Test/
├── Projects/               # Code source
│   ├── Shared/            # Projets partagés
│   └── Platform-Specific/ # Code spécifique OS
├── Tools/                  # Outils communs
└── Scripts/               # Automatisation
```

### Checklist de configuration

- [ ] **Virtualisation activée** dans BIOS/UEFI
- [ ] **Espace disque suffisant** (min 100 GB libre)
- [ ] **RAM suffisante** (min 16 GB pour VM confortables)
- [ ] **Sauvegardes configurées** avant dual-boot
- [ ] **Partitions préparées** pour dual-boot
- [ ] **Hyperviseur installé** pour VM
- [ ] **WSL2 activé** si Windows 10/11
- [ ] **Docker installé** si conteneurs
- [ ] **Git configuré** pour synchronisation
- [ ] **Scripts de build** multi-plateforme

### Conseils de sécurité

1. **Isolation** : Les VM offrent la meilleure isolation
2. **Snapshots** : Toujours avant modifications majeures
3. **Mises à jour** : Maintenir tous les OS à jour
4. **Antivirus** : Configurer exceptions pour dossiers dev
5. **Backups** : Sauvegarder régulièrement les VM

## Conclusion

### Récapitulatif

- **Dual-boot** : Performance native, idéal pour tests réels
- **VM** : Flexibilité maximale, plusieurs OS simultanés
- **WSL** : Intégration Windows/Linux excellente
- **Docker** : Reproductibilité et standardisation

### Stratégie recommandée

Pour un développement multi-plateforme optimal :

1. **Machine principale** : Votre OS préféré
2. **VM de test** : Pour les autres OS
3. **WSL/Docker** : Pour tests rapides
4. **CI/CD** : Tests automatisés sur toutes plateformes

### Le mot de la fin

Le développement multi-plateforme nécessite de tester sur les vraies plateformes. Choisissez la méthode qui correspond à vos besoins :
- **Performance** → Dual-boot
- **Flexibilité** → VM
- **Intégration** → WSL
- **Reproductibilité** → Docker

L'important est d'avoir un environnement de test pour chaque plateforme cible !

⏭️ [Contribution aux projets open source FreePascal/Lazarus](/01-introduction-freepascal-lazarus/09-contribution-projets-open-source.md)
