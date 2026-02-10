🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Scripts Bash et intégration shell pour applications FreePascal/Lazarus

## Introduction

Sous Linux/Ubuntu, le shell (ligne de commande) est au cœur du système. Intégrer votre application FreePascal/Lazarus avec le shell et créer des scripts Bash permet d'automatiser des tâches, de faciliter l'installation, et d'offrir une expérience utilisateur plus riche. Ce tutoriel vous apprendra à créer des scripts Bash et à intégrer votre application dans l'environnement shell Linux.

### Qu'est-ce que Bash ?

**Bash** (Bourne Again SHell) est l'interpréteur de commandes par défaut sur la plupart des distributions Linux. C'est :
- Un **interpréteur de commandes** : exécute les commandes que vous tapez
- Un **langage de script** : permet d'automatiser des séquences de commandes
- Un **environnement de travail** : gère les variables, processus, etc.

## Les bases des scripts Bash

### Structure d'un script Bash simple

```bash
#!/bin/bash
# Ceci est un commentaire

# Variables
NOM_APP="MonAppli"
VERSION="1.0.0"

# Afficher un message
echo "Bienvenue dans $NOM_APP version $VERSION"

# Exécuter une commande
ls -la

# Condition
if [ -f "config.txt" ]; then
    echo "Le fichier de configuration existe"
else
    echo "Fichier de configuration non trouvé"
fi
```

### Le shebang (#!)

La première ligne `#!/bin/bash` indique au système quel interpréteur utiliser. Variantes courantes :

```bash
#!/bin/bash          # Bash spécifiquement
#!/bin/sh           # Shell POSIX (plus portable)
#!/usr/bin/env bash # Trouve bash dans le PATH (recommandé)
```

### Rendre un script exécutable

```bash
# Créer le script
nano mon_script.sh

# Le rendre exécutable
chmod +x mon_script.sh

# L'exécuter
./mon_script.sh
```

## Scripts d'installation pour votre application Lazarus

### Script d'installation basique

Créez `install.sh` :

```bash
#!/usr/bin/env bash

# Configuration
APP_NAME="monappli"
APP_VERSION="1.0.0"
INSTALL_PREFIX="/usr/local"
CONFIG_DIR="$HOME/.config/$APP_NAME"

# Couleurs pour l'affichage
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Fonction pour afficher les messages
print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERREUR]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[ATTENTION]${NC} $1"
}

# Vérifier si on est root
check_root() {
    if [ "$EUID" -ne 0 ]; then
        print_error "Ce script doit être exécuté avec sudo"
        exit 1
    fi
}

# Vérifier les dépendances
check_dependencies() {
    print_info "Vérification des dépendances..."

    MISSING_DEPS=""

    # Liste des paquets requis
    DEPS="libgtk2.0-0 libglib2.0-0 libcairo2"

    for dep in $DEPS; do
        if ! dpkg -l | grep -q "^ii.*$dep"; then
            MISSING_DEPS="$MISSING_DEPS $dep"
        fi
    done

    if [ ! -z "$MISSING_DEPS" ]; then
        print_warning "Dépendances manquantes:$MISSING_DEPS"
        read -p "Installer les dépendances manquantes ? (o/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Oo]$ ]]; then
            apt-get update
            apt-get install -y $MISSING_DEPS
        else
            print_error "Installation annulée"
            exit 1
        fi
    else
        print_info "Toutes les dépendances sont installées"
    fi
}

# Installer l'application
install_app() {
    print_info "Installation de $APP_NAME..."

    # Créer les répertoires
    mkdir -p "$INSTALL_PREFIX/bin"
    mkdir -p "$INSTALL_PREFIX/share/applications"
    mkdir -p "$INSTALL_PREFIX/share/icons/hicolor/48x48/apps"
    mkdir -p "$INSTALL_PREFIX/share/doc/$APP_NAME"

    # Copier les fichiers
    cp "$APP_NAME" "$INSTALL_PREFIX/bin/" || {
        print_error "Échec de la copie de l'exécutable"
        exit 1
    }

    chmod 755 "$INSTALL_PREFIX/bin/$APP_NAME"

    # Installer l'icône
    if [ -f "$APP_NAME.png" ]; then
        cp "$APP_NAME.png" "$INSTALL_PREFIX/share/icons/hicolor/48x48/apps/"
    fi

    # Créer le fichier .desktop
    cat > "$INSTALL_PREFIX/share/applications/$APP_NAME.desktop" << EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=Mon Application
Comment=Application créée avec Lazarus
Exec=$INSTALL_PREFIX/bin/$APP_NAME
Icon=$APP_NAME
Categories=Utility;
Terminal=false
EOF

    # Copier la documentation
    if [ -f "README.md" ]; then
        cp README.md "$INSTALL_PREFIX/share/doc/$APP_NAME/"
    fi

    print_info "Installation terminée avec succès!"
}

# Créer la configuration utilisateur
setup_user_config() {
    print_info "Configuration de l'environnement utilisateur..."

    # Obtenir le nom de l'utilisateur qui a lancé sudo
    REAL_USER=${SUDO_USER:-$USER}
    REAL_HOME=$(getent passwd $REAL_USER | cut -d: -f6)

    # Créer le répertoire de configuration
    sudo -u $REAL_USER mkdir -p "$REAL_HOME/.config/$APP_NAME"

    # Créer un fichier de configuration par défaut
    if [ ! -f "$REAL_HOME/.config/$APP_NAME/config.ini" ]; then
        sudo -u $REAL_USER cat > "$REAL_HOME/.config/$APP_NAME/config.ini" << EOF
[General]
Version=$APP_VERSION
FirstRun=true

[Display]
Theme=default
Language=fr

[Paths]
DataDir=$REAL_HOME/.local/share/$APP_NAME
CacheDir=$REAL_HOME/.cache/$APP_NAME
EOF
        print_info "Fichier de configuration créé"
    fi
}

# Programme principal
main() {
    echo "========================================="
    echo "   Installation de $APP_NAME v$APP_VERSION"
    echo "========================================="
    echo

    check_root
    check_dependencies
    install_app
    setup_user_config

    echo
    print_info "Installation complète!"
    print_info "Lancez l'application avec: $APP_NAME"
    echo
}

# Lancer le programme principal
main "$@"
```

### Script de désinstallation

Créez `uninstall.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
INSTALL_PREFIX="/usr/local"

# Couleurs
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERREUR]${NC} $1"
}

# Vérifier les droits root
if [ "$EUID" -ne 0 ]; then
    print_error "Ce script doit être exécuté avec sudo"
    exit 1
fi

echo "Désinstallation de $APP_NAME..."

# Confirmation
read -p "Êtes-vous sûr de vouloir désinstaller $APP_NAME ? (o/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Oo]$ ]]; then
    echo "Désinstallation annulée"
    exit 0
fi

# Supprimer les fichiers
rm -f "$INSTALL_PREFIX/bin/$APP_NAME"
rm -f "$INSTALL_PREFIX/share/applications/$APP_NAME.desktop"
rm -f "$INSTALL_PREFIX/share/icons/hicolor/48x48/apps/$APP_NAME.png"
rm -rf "$INSTALL_PREFIX/share/doc/$APP_NAME"

# Proposer de supprimer la configuration utilisateur
read -p "Supprimer aussi la configuration utilisateur ? (o/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Oo]$ ]]; then
    REAL_USER=${SUDO_USER:-$USER}
    REAL_HOME=$(getent passwd $REAL_USER | cut -d: -f6)
    rm -rf "$REAL_HOME/.config/$APP_NAME"
    rm -rf "$REAL_HOME/.local/share/$APP_NAME"
    rm -rf "$REAL_HOME/.cache/$APP_NAME"
fi

print_info "Désinstallation terminée"
```

## Intégration avec le shell depuis FreePascal

### Exécuter des commandes shell depuis votre application

```pascal
uses
  Process, SysUtils;

// Méthode 1 : Exécution simple
procedure ExecuteShellCommand(const Command: string);
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.CommandLine := Command;
    Process.Options := Process.Options + [poWaitOnExit];
    Process.Execute;
  finally
    Process.Free;
  end;
end;

// Méthode 2 : Avec récupération de la sortie
function ExecuteShellCommandWithOutput(const Command: string): string;
var
  Process: TProcess;
  OutputStream: TStringStream;
  BytesRead: Integer;
  Buffer: array[0..2047] of byte;
begin
  Process := TProcess.Create(nil);
  OutputStream := TStringStream.Create('');
  try
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(Command);
    Process.Options := [poUsePipes, poStderrToOutPut];

    Process.Execute;

    while Process.Running do
    begin
      BytesRead := Process.Output.Read(Buffer, 2048);
      if BytesRead > 0 then
        OutputStream.Write(Buffer, BytesRead);
    end;

    // Lire ce qui reste
    repeat
      BytesRead := Process.Output.Read(Buffer, 2048);
      if BytesRead > 0 then
        OutputStream.Write(Buffer, BytesRead);
    until BytesRead = 0;

    Result := OutputStream.DataString;
  finally
    Process.Free;
    OutputStream.Free;
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);
var
  Output: string;
begin
  // Lister les fichiers
  Output := ExecuteShellCommandWithOutput('ls -la /home');
  Memo1.Lines.Text := Output;

  // Obtenir des infos système
  Output := ExecuteShellCommandWithOutput('uname -a');
  ShowMessage('Système: ' + Output);
end;
```

### Lire les variables d'environnement

```pascal
uses
  SysUtils;

procedure ReadEnvironmentVariables;
var
  HomeDir, UserName, Path: string;
begin
  // Variables système standard
  HomeDir := GetEnvironmentVariable('HOME');
  UserName := GetEnvironmentVariable('USER');
  Path := GetEnvironmentVariable('PATH');

  WriteLn('Dossier home: ', HomeDir);
  WriteLn('Utilisateur: ', UserName);
  WriteLn('PATH: ', Path);

  // Variable personnalisée
  if GetEnvironmentVariable('MONAPPLI_CONFIG') <> '' then
    WriteLn('Config: ', GetEnvironmentVariable('MONAPPLI_CONFIG'));
end;

// Définir une variable d'environnement (Linux)
procedure SetEnvironmentVar(const Name, Value: string);
begin
  {$IFDEF UNIX}
  fpSetEnv(PChar(Name), PChar(Value), 1); // unité unix
  {$ENDIF}
end;
```

## Scripts de lancement avancés

### Wrapper script avec détection d'environnement

Créez `monappli-launcher.sh` :

```bash
#!/usr/bin/env bash

# Détection de l'emplacement du script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_NAME="monappli"

# Configuration selon l'environnement
setup_environment() {
    # Détection de la distribution
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        DISTRO=$ID
        VERSION=$VERSION_ID
    else
        DISTRO="unknown"
        VERSION="unknown"
    fi

    # Configuration spécifique par distribution
    case "$DISTRO" in
        ubuntu|debian)
            export GTK_THEME="Adwaita"
            ;;
        fedora|rhel|centos)
            export QT_STYLE="fusion"
            ;;
        arch|manjaro)
            export GTK_USE_PORTAL=1
            ;;
    esac

    # Détection du serveur d'affichage
    if [ ! -z "$WAYLAND_DISPLAY" ]; then
        echo "Détecté: Wayland"
        export GDK_BACKEND=wayland
    elif [ ! -z "$DISPLAY" ]; then
        echo "Détecté: X11"
        export GDK_BACKEND=x11
    else
        echo "Attention: Pas de serveur d'affichage détecté"
    fi

    # Configuration de la locale
    if [ -z "$LANG" ]; then
        export LANG=fr_FR.UTF-8
    fi

    # Chemins des bibliothèques
    export LD_LIBRARY_PATH="$SCRIPT_DIR/lib:$LD_LIBRARY_PATH"
}

# Vérification des prérequis
check_requirements() {
    local missing_libs=""

    # Vérifier les bibliothèques critiques
    for lib in libgtk-x11-2.0.so.0 libglib-2.0.so.0; do
        if ! ldconfig -p | grep -q "$lib"; then
            missing_libs="$missing_libs $lib"
        fi
    done

    if [ ! -z "$missing_libs" ]; then
        echo "Bibliothèques manquantes:$missing_libs"
        echo "Installez les dépendances avec:"
        echo "  sudo apt-get install libgtk2.0-0 libglib2.0-0"
        exit 1
    fi
}

# Gestion des logs
setup_logging() {
    LOG_DIR="$HOME/.local/share/$APP_NAME/logs"
    mkdir -p "$LOG_DIR"

    # Rotation des logs
    if [ -f "$LOG_DIR/app.log" ]; then
        # Si le log dépasse 10MB, le faire tourner
        LOG_SIZE=$(stat -c%s "$LOG_DIR/app.log" 2>/dev/null || echo 0)
        if [ "$LOG_SIZE" -gt 10485760 ]; then
            mv "$LOG_DIR/app.log" "$LOG_DIR/app.log.old"
        fi
    fi

    # Définir le fichier de log pour l'application
    export MONAPPLI_LOG_FILE="$LOG_DIR/app.log"
}

# Gestion des arguments
parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --debug)
                export MONAPPLI_DEBUG=1
                echo "Mode debug activé"
                ;;
            --config)
                export MONAPPLI_CONFIG="$2"
                shift
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            --version|-v)
                echo "$APP_NAME version 1.0.0"
                exit 0
                ;;
            *)
                # Passer l'argument à l'application
                APP_ARGS="$APP_ARGS $1"
                ;;
        esac
        shift
    done
}

show_help() {
    cat << EOF
Usage: $0 [OPTIONS]

Options:
    --debug           Active le mode debug
    --config FILE     Utilise un fichier de configuration spécifique
    --help, -h        Affiche cette aide
    --version, -v     Affiche la version

Variables d'environnement:
    MONAPPLI_DEBUG    Active le mode debug (1/0)
    MONAPPLI_CONFIG   Chemin vers le fichier de configuration
    MONAPPLI_LOG_FILE Fichier de log

EOF
}

# Gestion des signaux
cleanup() {
    echo "Arrêt de l'application..."
    # Nettoyer les fichiers temporaires si nécessaire
    rm -f "/tmp/$APP_NAME-$USER.lock" 2>/dev/null
    exit
}

trap cleanup EXIT INT TERM

# Vérifier une seule instance
check_single_instance() {
    LOCK_FILE="/tmp/$APP_NAME-$USER.lock"

    if [ -f "$LOCK_FILE" ]; then
        PID=$(cat "$LOCK_FILE")
        if ps -p "$PID" > /dev/null 2>&1; then
            echo "L'application est déjà en cours d'exécution (PID: $PID)"
            exit 1
        else
            # PID obsolète, on supprime le lock
            rm -f "$LOCK_FILE"
        fi
    fi

    # Créer le lock avec notre PID
    echo $$ > "$LOCK_FILE"
}

# Programme principal
main() {
    setup_environment
    check_requirements
    setup_logging
    parse_arguments "$@"
    check_single_instance

    # Lancer l'application
    if [ -f "$SCRIPT_DIR/$APP_NAME" ]; then
        exec "$SCRIPT_DIR/$APP_NAME" $APP_ARGS
    else
        echo "Erreur: $APP_NAME introuvable dans $SCRIPT_DIR"
        exit 1
    fi
}

main "$@"
```

## Intégration avec les alias et fonctions Bash

### Créer des alias pour votre application

Ajoutez dans `~/.bashrc` ou créez `/etc/profile.d/monappli.sh` :

```bash
# Alias pour lancer l'application
alias monappli='/usr/local/bin/monappli'

# Alias avec options courantes
alias monappli-debug='monappli --debug --verbose'
alias monappli-config='monappli --config ~/.config/monappli/custom.ini'

# Fonction pour ouvrir un fichier avec l'application
monappli-open() {
    if [ -z "$1" ]; then
        echo "Usage: monappli-open <fichier>"
        return 1
    fi

    if [ ! -f "$1" ]; then
        echo "Fichier non trouvé: $1"
        return 1
    fi

    monappli --open "$1"
}

# Fonction de complétion automatique
_monappli_completion() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts="--help --version --debug --config --open"

    case "${prev}" in
        --config)
            # Compléter avec les fichiers .ini
            COMPREPLY=( $(compgen -f -X '!*.ini' -- ${cur}) )
            return 0
            ;;
        --open)
            # Compléter avec les fichiers du type supporté
            COMPREPLY=( $(compgen -f -X '!*.dat' -- ${cur}) )
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
            return 0
            ;;
    esac
}

# Activer la complétion
complete -F _monappli_completion monappli
```

## Scripts utilitaires pour votre application

### Script de mise à jour

Créez `update.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
UPDATE_URL="https://example.com/releases/latest"
CURRENT_VERSION_FILE="/usr/local/share/$APP_NAME/version"

# Obtenir la version actuelle
get_current_version() {
    if [ -f "$CURRENT_VERSION_FILE" ]; then
        cat "$CURRENT_VERSION_FILE"
    else
        echo "0.0.0"
    fi
}

# Vérifier la dernière version
check_latest_version() {
    # Télécharger les infos de version
    wget -q -O /tmp/latest_version.txt "$UPDATE_URL/version.txt"

    if [ $? -eq 0 ]; then
        cat /tmp/latest_version.txt
    else
        echo "Erreur de connexion"
        return 1
    fi
}

# Comparer les versions
version_greater() {
    # Comparer deux versions au format X.Y.Z
    IFS='.' read -ra VER1 <<< "$1"
    IFS='.' read -ra VER2 <<< "$2"

    for i in {0..2}; do
        if [ "${VER1[$i]:-0}" -gt "${VER2[$i]:-0}" ]; then
            return 0
        elif [ "${VER1[$i]:-0}" -lt "${VER2[$i]:-0}" ]; then
            return 1
        fi
    done

    return 1
}

# Télécharger et installer la mise à jour
download_and_install() {
    local version=$1
    local download_url="$UPDATE_URL/$APP_NAME-$version.tar.gz"

    echo "Téléchargement de la version $version..."
    wget -O /tmp/$APP_NAME-update.tar.gz "$download_url"

    if [ $? -ne 0 ]; then
        echo "Erreur lors du téléchargement"
        return 1
    fi

    # Créer une sauvegarde
    echo "Sauvegarde de la version actuelle..."
    cp /usr/local/bin/$APP_NAME /usr/local/bin/$APP_NAME.backup

    # Extraire et installer
    echo "Installation..."
    tar -xzf /tmp/$APP_NAME-update.tar.gz -C /tmp/

    if [ -f "/tmp/$APP_NAME-$version/install.sh" ]; then
        cd "/tmp/$APP_NAME-$version"
        sudo ./install.sh
    else
        sudo cp "/tmp/$APP_NAME-$version/$APP_NAME" /usr/local/bin/
    fi

    # Mettre à jour le fichier de version
    echo "$version" | sudo tee "$CURRENT_VERSION_FILE" > /dev/null

    echo "Mise à jour terminée!"
}

# Programme principal
main() {
    echo "=== Vérification des mises à jour pour $APP_NAME ==="

    CURRENT=$(get_current_version)
    echo "Version actuelle: $CURRENT"

    LATEST=$(check_latest_version)
    if [ $? -ne 0 ]; then
        echo "Impossible de vérifier les mises à jour"
        exit 1
    fi

    echo "Dernière version: $LATEST"

    if version_greater "$LATEST" "$CURRENT"; then
        echo "Une nouvelle version est disponible!"
        read -p "Voulez-vous mettre à jour ? (o/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Oo]$ ]]; then
            download_and_install "$LATEST"
        fi
    else
        echo "Votre application est à jour."
    fi
}

main "$@"
```

### Script de diagnostic

Créez `diagnostic.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
REPORT_FILE="diagnostic_$(date +%Y%m%d_%H%M%S).txt"

echo "Génération du rapport de diagnostic..." | tee $REPORT_FILE

# Informations système
{
    echo "=== INFORMATIONS SYSTÈME ==="
    echo "Date: $(date)"
    echo "Hostname: $(hostname)"
    echo "User: $USER"
    echo

    echo "=== OS ==="
    uname -a
    lsb_release -a 2>/dev/null || cat /etc/os-release
    echo

    echo "=== ENVIRONNEMENT ==="
    echo "DISPLAY: $DISPLAY"
    echo "WAYLAND_DISPLAY: $WAYLAND_DISPLAY"
    echo "LANG: $LANG"
    echo "PATH: $PATH"
    echo "LD_LIBRARY_PATH: $LD_LIBRARY_PATH"
    echo

    echo "=== APPLICATION ==="
    echo "Emplacement: $(which $APP_NAME 2>/dev/null)"
    echo "Version: $($APP_NAME --version 2>/dev/null || echo 'Non disponible')"
    echo

    echo "=== DÉPENDANCES ==="
    echo "Bibliothèques liées:"
    if [ -f "/usr/local/bin/$APP_NAME" ]; then
        ldd "/usr/local/bin/$APP_NAME"
    fi
    echo

    echo "=== CONFIGURATION ==="
    echo "Fichiers de configuration:"
    ls -la ~/.config/$APP_NAME/ 2>/dev/null || echo "Aucun fichier de configuration"
    echo

    echo "=== LOGS RÉCENTS ==="
    if [ -f "$HOME/.local/share/$APP_NAME/logs/app.log" ]; then
        echo "Dernières 20 lignes du log:"
        tail -20 "$HOME/.local/share/$APP_NAME/logs/app.log"
    else
        echo "Aucun log trouvé"
    fi
    echo

    echo "=== PROCESSUS ==="
    ps aux | grep -i $APP_NAME | grep -v grep
    echo

    echo "=== MÉMOIRE ==="
    free -h
    echo

    echo "=== DISQUE ==="
    df -h /home /tmp /var

} >> $REPORT_FILE 2>&1

echo "Rapport généré: $REPORT_FILE"
echo "Envoyez ce fichier au support technique si nécessaire."
```

## Communication entre Bash et FreePascal

### Passer des données via pipes

Script Bash qui communique avec l'application :

```bash
#!/bin/bash
# data-processor.sh

# Générer des données
generate_data() {
    echo "timestamp,value"
    for i in {1..10}; do
        echo "$(date +%s),$((RANDOM % 100))"
        sleep 1
    done
}

# Envoyer les données à l'application
generate_data | monappli --process-stdin
```

Code FreePascal pour lire depuis stdin :

```pascal
procedure ProcessStdinData;
var
  Line: string;
begin
  while not EOF do
  begin
    ReadLn(Line);
    ProcessLine(Line);
  end;
end;
```

### Utiliser des fichiers FIFO (named pipes)

Script Bash créant un FIFO :

```bash
#!/bin/bash

FIFO_PATH="/tmp/monappli.fifo"

# Créer le FIFO s'il n'existe pas
if [ ! -p "$FIFO_PATH" ]; then
    mkfifo "$FIFO_PATH"
fi

# Envoyer des commandes
echo "COMMAND:REFRESH" > "$FIFO_PATH"
echo "DATA:Hello from Bash" > "$FIFO_PATH"
```

Code FreePascal pour lire le FIFO :

```pascal
uses
  BaseUnix, SysUtils;

procedure ReadFromFIFO;
var
  FIFOFile: TextFile;
  Line: string;
  FIFOPath: string;
begin
  FIFOPath := '/tmp/monappli.fifo';

  // Créer le FIFO si nécessaire
  if not FileExists(FIFOPath) then
    FpMkFifo(FIFOPath, &666);

  AssignFile(FIFOFile, FIFOPath);
  Reset(FIFOFile);

  try
    while not EOF(FIFOFile) do
    begin
      ReadLn(FIFOFile, Line);
      ProcessCommand(Line);
    end;
  finally
    CloseFile(FIFOFile);
  end;
end;
```

## Scripts de déploiement et maintenance

### Script de backup automatique

Créez `backup.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
BACKUP_DIR="/var/backups/$APP_NAME"
CONFIG_DIR="$HOME/.config/$APP_NAME"
DATA_DIR="$HOME/.local/share/$APP_NAME"
RETENTION_DAYS=30

# Créer le répertoire de backup
sudo mkdir -p "$BACKUP_DIR"

# Nom du fichier de backup
BACKUP_FILE="$BACKUP_DIR/backup_$(date +%Y%m%d_%H%M%S).tar.gz"

echo "Création du backup..."

# Créer l'archive
tar -czf "$BACKUP_FILE" \
    "$CONFIG_DIR" \
    "$DATA_DIR" \
    2>/dev/null

if [ $? -eq 0 ]; then
    echo "Backup créé: $BACKUP_FILE"

    # Nettoyer les vieux backups
    echo "Nettoyage des backups de plus de $RETENTION_DAYS jours..."
    find "$BACKUP_DIR" -name "backup_*.tar.gz" -mtime +$RETENTION_DAYS -delete
else
    echo "Erreur lors de la création du backup"
    exit 1
fi

# Vérifier l'intégrité
tar -tzf "$BACKUP_FILE" > /dev/null 2>&1
if [ $? -eq 0 ]; then
    echo "Backup vérifié avec succès"

    # Créer un lien vers le dernier backup
    sudo ln -sf "$BACKUP_FILE" "$BACKUP_DIR/latest.tar.gz"
else
    echo "ATTENTION: Le backup semble corrompu!"
    exit 1
fi

# Afficher les statistiques
SIZE=$(du -h "$BACKUP_FILE" | cut -f1)
echo "Taille du backup: $SIZE"
echo "Backups existants:"
ls -lh "$BACKUP_DIR"/*.tar.gz | tail -5
```

### Script de restauration

Créez `restore.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
BACKUP_DIR="/var/backups/$APP_NAME"

# Couleurs
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

print_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
print_error() { echo -e "${RED}[ERREUR]${NC} $1"; }
print_warning() { echo -e "${YELLOW}[ATTENTION]${NC} $1"; }

# Lister les backups disponibles
list_backups() {
    echo "Backups disponibles:"
    echo "-------------------"

    if [ -d "$BACKUP_DIR" ]; then
        ls -1t "$BACKUP_DIR"/backup_*.tar.gz 2>/dev/null | head -10 | nl
    else
        print_error "Aucun backup trouvé"
        exit 1
    fi
}

# Restaurer un backup
restore_backup() {
    local backup_file="$1"

    if [ ! -f "$backup_file" ]; then
        print_error "Fichier de backup introuvable: $backup_file"
        exit 1
    fi

    print_warning "Cette opération va remplacer votre configuration actuelle!"
    read -p "Continuer ? (o/n) " -n 1 -r
    echo

    if [[ ! $REPLY =~ ^[Oo]$ ]]; then
        echo "Restauration annulée"
        exit 0
    fi

    # Créer un backup de sécurité de la config actuelle
    print_info "Sauvegarde de la configuration actuelle..."
    SAFETY_BACKUP="/tmp/${APP_NAME}_safety_$(date +%Y%m%d_%H%M%S).tar.gz"
    tar -czf "$SAFETY_BACKUP" \
        "$HOME/.config/$APP_NAME" \
        "$HOME/.local/share/$APP_NAME" \
        2>/dev/null

    print_info "Backup de sécurité créé: $SAFETY_BACKUP"

    # Extraire le backup
    print_info "Restauration en cours..."
    tar -xzf "$backup_file" -C / 2>/dev/null

    if [ $? -eq 0 ]; then
        print_info "Restauration terminée avec succès!"
        print_info "En cas de problème, votre ancienne config est dans: $SAFETY_BACKUP"
    else
        print_error "Erreur lors de la restauration"
        print_info "Restauration de la configuration précédente..."
        tar -xzf "$SAFETY_BACKUP" -C / 2>/dev/null
        exit 1
    fi
}

# Menu principal
main() {
    echo "==================================="
    echo "   Restauration de $APP_NAME"
    echo "==================================="
    echo

    list_backups
    echo
    echo "Options:"
    echo "  1-10 : Restaurer le backup correspondant"
    echo "  l    : Restaurer le dernier backup (latest)"
    echo "  f    : Spécifier un fichier de backup"
    echo "  q    : Quitter"
    echo

    read -p "Votre choix: " choice

    case $choice in
        [1-9]|10)
            backup_file=$(ls -1t "$BACKUP_DIR"/backup_*.tar.gz 2>/dev/null | sed -n "${choice}p")
            if [ -n "$backup_file" ]; then
                restore_backup "$backup_file"
            else
                print_error "Numéro invalide"
                exit 1
            fi
            ;;
        l)
            if [ -L "$BACKUP_DIR/latest.tar.gz" ]; then
                restore_backup "$BACKUP_DIR/latest.tar.gz"
            else
                print_error "Aucun lien 'latest' trouvé"
                exit 1
            fi
            ;;
        f)
            read -p "Chemin du fichier de backup: " backup_path
            restore_backup "$backup_path"
            ;;
        q)
            echo "Au revoir!"
            exit 0
            ;;
        *)
            print_error "Choix invalide"
            exit 1
            ;;
    esac
}

main "$@"
```

### Script de monitoring

Créez `monitor.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
LOG_FILE="$HOME/.local/share/$APP_NAME/monitor.log"
PID_FILE="/var/run/$APP_NAME.pid"
CHECK_INTERVAL=60  # secondes
MAX_MEMORY_MB=500
MAX_CPU_PERCENT=80

# Créer le répertoire de log si nécessaire
mkdir -p "$(dirname "$LOG_FILE")"

# Fonction de logging
log_message() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> "$LOG_FILE"
}

# Obtenir le PID de l'application
get_app_pid() {
    if [ -f "$PID_FILE" ]; then
        cat "$PID_FILE"
    else
        pgrep -x "$APP_NAME" | head -1
    fi
}

# Vérifier l'utilisation mémoire
check_memory() {
    local pid=$1
    if [ -z "$pid" ]; then
        return 1
    fi

    # Obtenir l'utilisation mémoire en MB
    local mem_kb=$(ps -p "$pid" -o rss= 2>/dev/null | tr -d ' ')
    if [ -z "$mem_kb" ]; then
        return 1
    fi

    local mem_mb=$((mem_kb / 1024))

    if [ $mem_mb -gt $MAX_MEMORY_MB ]; then
        log_message "ATTENTION: Utilisation mémoire élevée: ${mem_mb}MB (max: ${MAX_MEMORY_MB}MB)"
        return 2
    fi

    return 0
}

# Vérifier l'utilisation CPU
check_cpu() {
    local pid=$1
    if [ -z "$pid" ]; then
        return 1
    fi

    # Obtenir l'utilisation CPU
    local cpu=$(ps -p "$pid" -o %cpu= 2>/dev/null | tr -d ' ' | cut -d. -f1)
    if [ -z "$cpu" ]; then
        return 1
    fi

    if [ $cpu -gt $MAX_CPU_PERCENT ]; then
        log_message "ATTENTION: Utilisation CPU élevée: ${cpu}% (max: ${MAX_CPU_PERCENT}%)"
        return 2
    fi

    return 0
}

# Vérifier si l'application répond
check_responsive() {
    local pid=$1
    if [ -z "$pid" ]; then
        return 1
    fi

    # Envoyer un signal 0 (test si le processus existe)
    if kill -0 "$pid" 2>/dev/null; then
        return 0
    else
        return 1
    fi
}

# Redémarrer l'application si nécessaire
restart_app() {
    log_message "Redémarrage de l'application..."

    # Arrêter l'application
    if [ -f "$PID_FILE" ]; then
        kill $(cat "$PID_FILE") 2>/dev/null
        sleep 2
        kill -9 $(cat "$PID_FILE") 2>/dev/null
        rm -f "$PID_FILE"
    fi

    # Redémarrer
    nohup "$APP_NAME" > /dev/null 2>&1 &
    echo $! > "$PID_FILE"

    log_message "Application redémarrée (PID: $!)"
}

# Générer un rapport
generate_report() {
    local pid=$1
    local report_file="/tmp/${APP_NAME}_report_$(date +%Y%m%d_%H%M%S).txt"

    {
        echo "=== Rapport de Monitoring ==="
        echo "Date: $(date)"
        echo "Application: $APP_NAME"
        echo "PID: $pid"
        echo
        echo "=== État du Processus ==="
        ps -p "$pid" -f
        echo
        echo "=== Utilisation Ressources ==="
        ps -p "$pid" -o pid,ppid,%cpu,%mem,rss,vsz,stat,start,time,cmd
        echo
        echo "=== Fichiers Ouverts ==="
        lsof -p "$pid" 2>/dev/null | head -20
        echo
        echo "=== Connexions Réseau ==="
        lsof -p "$pid" -i 2>/dev/null
        echo
        echo "=== Threads ==="
        ps -p "$pid" -L -o pid,tid,psr,pcpu,pmem,stat,wchan:20,comm
        echo
        echo "=== Dernières entrées du log ==="
        tail -50 "$LOG_FILE"
    } > "$report_file"

    echo "$report_file"
}

# Boucle de monitoring principal
monitor_loop() {
    log_message "Démarrage du monitoring de $APP_NAME"

    while true; do
        PID=$(get_app_pid)

        if [ -z "$PID" ]; then
            log_message "ERREUR: Application non trouvée"

            # Tenter de redémarrer
            read -p "Redémarrer l'application ? (o/n) " -n 1 -r
            echo
            if [[ $REPLY =~ ^[Oo]$ ]]; then
                restart_app
            fi
        else
            # Vérifier l'état
            if ! check_responsive "$PID"; then
                log_message "ERREUR: L'application ne répond pas"
                restart_app
            fi

            # Vérifier les ressources
            check_memory "$PID"
            mem_status=$?

            check_cpu "$PID"
            cpu_status=$?

            # Si utilisation excessive des ressources
            if [ $mem_status -eq 2 ] || [ $cpu_status -eq 2 ]; then
                log_message "Génération d'un rapport..."
                report=$(generate_report "$PID")
                log_message "Rapport généré: $report"

                # Envoyer une alerte (email, notification, etc.)
                # send_alert "$report"
            fi
        fi

        sleep $CHECK_INTERVAL
    done
}

# Mode daemon
daemonize() {
    # Rediriger les sorties
    exec 1>>"$LOG_FILE"
    exec 2>&1

    # Se détacher du terminal
    if [ -t 0 ]; then
        exec 0</dev/null
    fi

    # Lancer en arrière-plan
    monitor_loop &

    # Sauvegarder le PID du monitor
    echo $! > "/var/run/${APP_NAME}_monitor.pid"
    echo "Monitor lancé en arrière-plan (PID: $!)"
}

# Affichage de l'aide
show_help() {
    cat << EOF
Usage: $0 [OPTION]

Options:
    start       Démarrer le monitoring en arrière-plan
    stop        Arrêter le monitoring
    status      Afficher l'état du monitoring
    report      Générer un rapport immédiat
    watch       Lancer le monitoring en premier plan
    help        Afficher cette aide

Configuration:
    CHECK_INTERVAL=$CHECK_INTERVAL secondes
    MAX_MEMORY_MB=$MAX_MEMORY_MB MB
    MAX_CPU_PERCENT=$MAX_CPU_PERCENT %

EOF
}

# Programme principal
case "${1:-watch}" in
    start)
        daemonize
        ;;
    stop)
        if [ -f "/var/run/${APP_NAME}_monitor.pid" ]; then
            kill $(cat "/var/run/${APP_NAME}_monitor.pid")
            rm -f "/var/run/${APP_NAME}_monitor.pid"
            echo "Monitoring arrêté"
        else
            echo "Monitoring non actif"
        fi
        ;;
    status)
        if [ -f "/var/run/${APP_NAME}_monitor.pid" ]; then
            PID=$(cat "/var/run/${APP_NAME}_monitor.pid")
            if kill -0 "$PID" 2>/dev/null; then
                echo "Monitoring actif (PID: $PID)"
            else
                echo "Monitoring inactif (PID obsolète)"
            fi
        else
            echo "Monitoring inactif"
        fi
        ;;
    report)
        PID=$(get_app_pid)
        if [ -n "$PID" ]; then
            report=$(generate_report "$PID")
            echo "Rapport généré: $report"
            cat "$report"
        else
            echo "Application non trouvée"
        fi
        ;;
    watch)
        monitor_loop
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        echo "Option invalide: $1"
        show_help
        exit 1
        ;;
esac
```

## Intégration avec systemd

### Créer un service systemd

Pour que votre application démarre automatiquement, créez `/etc/systemd/system/monappli.service` :

```ini
[Unit]
Description=Mon Application FreePascal/Lazarus
Documentation=https://example.com/docs
After=network.target

[Service]
Type=simple
User=monappli
Group=monappli
WorkingDirectory=/usr/local/share/monappli

# Variables d'environnement
Environment="DISPLAY=:0"
Environment="HOME=/var/lib/monappli"

# Commande de démarrage
ExecStart=/usr/local/bin/monappli --daemon

# Commande d'arrêt
ExecStop=/bin/kill -TERM $MAINPID

# Redémarrage automatique
Restart=on-failure
RestartSec=10

# Limites
LimitNOFILE=1024
LimitNPROC=512

# Logs
StandardOutput=journal
StandardError=journal

# Sécurité
PrivateTmp=true
NoNewPrivileges=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=/var/lib/monappli /var/log/monappli

[Install]
WantedBy=multi-user.target
```

### Script d'installation du service

Créez `install-service.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
SERVICE_FILE="/etc/systemd/system/${APP_NAME}.service"
APP_USER="monappli"

# Couleurs
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

print_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
print_error() { echo -e "${RED}[ERREUR]${NC} $1"; }

# Vérifier les droits root
if [ "$EUID" -ne 0 ]; then
    print_error "Ce script doit être exécuté avec sudo"
    exit 1
fi

# Créer l'utilisateur système
create_service_user() {
    if ! id "$APP_USER" &>/dev/null; then
        print_info "Création de l'utilisateur système $APP_USER..."
        useradd --system --home /var/lib/$APP_NAME --shell /bin/false $APP_USER

        # Créer les répertoires
        mkdir -p /var/lib/$APP_NAME
        mkdir -p /var/log/$APP_NAME

        # Définir les permissions
        chown -R $APP_USER:$APP_USER /var/lib/$APP_NAME
        chown -R $APP_USER:$APP_USER /var/log/$APP_NAME
    else
        print_info "L'utilisateur $APP_USER existe déjà"
    fi
}

# Installer le service
install_service() {
    print_info "Installation du service systemd..."

    # Copier le fichier de service
    cp $APP_NAME.service $SERVICE_FILE

    # Recharger systemd
    systemctl daemon-reload

    # Activer le service
    systemctl enable $APP_NAME

    print_info "Service installé et activé"
}

# Configurer les logs avec journald
configure_logging() {
    print_info "Configuration des logs..."

    # Créer la configuration journald
    mkdir -p /etc/systemd/journald.conf.d/

    cat > /etc/systemd/journald.conf.d/${APP_NAME}.conf << EOF
# Configuration journald pour $APP_NAME
[Journal]
# Conserver les logs pendant 30 jours
MaxRetentionSec=30d

# Taille maximale des logs
SystemMaxUse=100M
EOF

    # Redémarrer journald
    systemctl restart systemd-journald
}

# Script de rotation des logs personnalisés
setup_logrotate() {
    print_info "Configuration de logrotate..."

    cat > /etc/logrotate.d/$APP_NAME << EOF
/var/log/$APP_NAME/*.log {
    daily
    rotate 14
    compress
    delaycompress
    missingok
    notifempty
    create 0640 $APP_USER $APP_USER
    postrotate
        systemctl reload $APP_NAME 2>/dev/null || true
    endscript
}
EOF
}

# Menu principal
main() {
    echo "================================"
    echo "Installation du service $APP_NAME"
    echo "================================"
    echo

    create_service_user
    install_service
    configure_logging
    setup_logrotate

    echo
    print_info "Installation terminée!"
    echo
    echo "Commandes utiles:"
    echo "  systemctl start $APP_NAME    # Démarrer"
    echo "  systemctl stop $APP_NAME     # Arrêter"
    echo "  systemctl status $APP_NAME   # État"
    echo "  journalctl -u $APP_NAME -f   # Logs en temps réel"
    echo

    read -p "Démarrer le service maintenant ? (o/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Oo]$ ]]; then
        systemctl start $APP_NAME
        sleep 2
        systemctl status $APP_NAME
    fi
}

main "$@"
```

## Intégration avec cron

### Script de tâches planifiées

Créez `setup-cron.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
CRON_USER="$USER"

# Ajouter des tâches cron
setup_cron_jobs() {
    echo "Configuration des tâches planifiées..."

    # Créer un fichier temporaire pour les tâches cron
    CRON_FILE="/tmp/${APP_NAME}_cron"

    # Obtenir les tâches actuelles
    crontab -u $CRON_USER -l > "$CRON_FILE" 2>/dev/null || true

    # Vérifier si nos tâches existent déjà
    if grep -q "$APP_NAME" "$CRON_FILE"; then
        echo "Des tâches pour $APP_NAME existent déjà"
        read -p "Les remplacer ? (o/n) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Oo]$ ]]; then
            exit 0
        fi
        # Supprimer les anciennes tâches
        sed -i "/$APP_NAME/d" "$CRON_FILE"
    fi

    # Ajouter les nouvelles tâches
    cat >> "$CRON_FILE" << EOF

# === Tâches automatiques pour $APP_NAME ===

# Backup quotidien à 2h du matin
0 2 * * * /usr/local/bin/${APP_NAME}-backup.sh >> /var/log/${APP_NAME}/backup.log 2>&1

# Nettoyage des fichiers temporaires tous les lundis
0 3 * * 1 /usr/local/bin/${APP_NAME}-cleanup.sh >> /var/log/${APP_NAME}/cleanup.log 2>&1

# Vérification de mises à jour tous les jours à 9h
0 9 * * * /usr/local/bin/${APP_NAME}-update.sh --check >> /var/log/${APP_NAME}/update.log 2>&1

# Rapport de statistiques le premier jour du mois
0 0 1 * * /usr/local/bin/${APP_NAME}-stats.sh --monthly >> /var/log/${APP_NAME}/stats.log 2>&1

# Redémarrage préventif tous les dimanches à 4h
0 4 * * 0 systemctl restart $APP_NAME

# === Fin des tâches $APP_NAME ===
EOF

    # Installer le nouveau crontab
    crontab -u $CRON_USER "$CRON_FILE"

    # Nettoyer
    rm -f "$CRON_FILE"

    echo "Tâches cron installées pour l'utilisateur $CRON_USER"
    echo
    echo "Pour vérifier : crontab -l"
    echo "Pour éditer  : crontab -e"
}

# Script de nettoyage
create_cleanup_script() {
    cat > /usr/local/bin/${APP_NAME}-cleanup.sh << 'EOF'
#!/bin/bash

APP_NAME="monappli"
TEMP_DIRS=(
    "/tmp/${APP_NAME}_*"
    "/var/tmp/${APP_NAME}_*"
    "$HOME/.cache/$APP_NAME/temp/*"
)

LOG_FILE="/var/log/$APP_NAME/cleanup.log"

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Début du nettoyage"

total_freed=0

for pattern in "${TEMP_DIRS[@]}"; do
    for file in $pattern; do
        if [ -e "$file" ]; then
            # Vérifier l'âge du fichier (plus de 7 jours)
            if [ $(find "$file" -mtime +7 2>/dev/null | wc -l) -gt 0 ]; then
                size=$(du -sb "$file" 2>/dev/null | cut -f1)
                rm -rf "$file"
                total_freed=$((total_freed + size))
                echo "Supprimé: $file ($(numfmt --to=iec-i --suffix=B $size))"
            fi
        fi
    done
done

# Nettoyer les logs anciens
find /var/log/$APP_NAME -name "*.log" -mtime +30 -delete

echo "Espace libéré: $(numfmt --to=iec-i --suffix=B $total_freed)"
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Fin du nettoyage"
EOF

    chmod +x /usr/local/bin/${APP_NAME}-cleanup.sh
}

# Script de statistiques
create_stats_script() {
    cat > /usr/local/bin/${APP_NAME}-stats.sh << 'EOF'
#!/bin/bash

APP_NAME="monappli"
STATS_DIR="/var/lib/$APP_NAME/stats"
DB_PATH="$HOME/.local/share/$APP_NAME/database.db"

mkdir -p "$STATS_DIR"

generate_stats() {
    local period=$1
    local output_file="$STATS_DIR/stats_$(date +%Y%m%d_%H%M%S).json"

    # Collecter les statistiques
    {
        echo "{"
        echo "  \"timestamp\": \"$(date -Iseconds)\","
        echo "  \"period\": \"$period\","

        # Nombre de processus
        echo "  \"process_count\": $(pgrep -c $APP_NAME),"

        # Utilisation disque
        echo "  \"disk_usage\": {"
        echo "    \"config\": \"$(du -sb $HOME/.config/$APP_NAME 2>/dev/null | cut -f1)\","
        echo "    \"data\": \"$(du -sb $HOME/.local/share/$APP_NAME 2>/dev/null | cut -f1)\","
        echo "    \"cache\": \"$(du -sb $HOME/.cache/$APP_NAME 2>/dev/null | cut -f1)\""
        echo "  },"

        # Nombre de fichiers
        echo "  \"file_counts\": {"
        echo "    \"config\": $(find $HOME/.config/$APP_NAME -type f 2>/dev/null | wc -l),"
        echo "    \"data\": $(find $HOME/.local/share/$APP_NAME -type f 2>/dev/null | wc -l)"
        echo "  },"

        # Taille de la base de données
        if [ -f "$DB_PATH" ]; then
            echo "  \"database_size\": $(stat -c%s "$DB_PATH"),"
        fi

        # Uptime du service
        if systemctl is-active --quiet $APP_NAME; then
            echo "  \"service_uptime\": \"$(systemctl show $APP_NAME --property=ActiveEnterTimestamp --value)\","
        fi

        # Nombre d'erreurs dans les logs
        echo "  \"error_count\": $(journalctl -u $APP_NAME --since "1 month ago" | grep -c ERROR)"

        echo "}"
    } > "$output_file"

    echo "Statistiques générées: $output_file"

    # Optionnel : envoyer par email ou webhook
    # send_stats_report "$output_file"
}

case "${1:-daily}" in
    --daily)
        generate_stats "daily"
        ;;
    --weekly)
        generate_stats "weekly"
        ;;
    --monthly)
        generate_stats "monthly"
        # Archiver les anciennes stats
        tar -czf "$STATS_DIR/archive_$(date +%Y%m).tar.gz" \
            $STATS_DIR/stats_*.json \
            --remove-files 2>/dev/null || true
        ;;
    *)
        echo "Usage: $0 [--daily|--weekly|--monthly]"
        exit 1
        ;;
esac
EOF

    chmod +x /usr/local/bin/${APP_NAME}-stats.sh
}

# Programme principal
main() {
    echo "Configuration des tâches planifiées pour $APP_NAME"
    echo "================================================"
    echo

    # Créer les scripts
    create_cleanup_script
    create_stats_script

    # Configurer cron
    setup_cron_jobs

    echo
    echo "Configuration terminée!"
}

main "$@"
```

## Intégration avec le terminal

### Créer une commande personnalisée

Ajoutez dans `/usr/local/bin/monappli-cli` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
VERSION="1.0.0"
CONFIG_FILE="$HOME/.config/$APP_NAME/config.ini"

# Couleurs pour le terminal
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
NC='\033[0m'

# Fonction d'aide
show_help() {
    cat << EOF
${BOLD}$APP_NAME CLI v$VERSION${NC}

${BOLD}Usage:${NC}
  $(basename $0) [commande] [options]

${BOLD}Commandes:${NC}
  ${GREEN}start${NC}              Démarrer l'application
  ${GREEN}stop${NC}               Arrêter l'application
  ${GREEN}restart${NC}            Redémarrer l'application
  ${GREEN}status${NC}             Afficher l'état
  ${GREEN}config${NC} [key] [val] Gérer la configuration
  ${GREEN}log${NC} [n]            Afficher les n dernières lignes de log
  ${GREEN}clean${NC}              Nettoyer les fichiers temporaires
  ${GREEN}backup${NC}             Créer un backup
  ${GREEN}restore${NC} [file]     Restaurer depuis un backup
  ${GREEN}update${NC}             Vérifier les mises à jour
  ${GREEN}info${NC}               Informations système
  ${GREEN}shell${NC}              Ouvrir un shell interactif

${BOLD}Options:${NC}
  -h, --help         Afficher cette aide
  -v, --version      Afficher la version
  -q, --quiet        Mode silencieux
  -d, --debug        Mode debug

${BOLD}Exemples:${NC}
  $(basename $0) start            # Démarrer l'application
  $(basename $0) config theme dark # Changer le thème
  $(basename $0) log 100          # Voir les 100 dernières lignes

EOF
}

# Gestion de la configuration
manage_config() {
    local key=$1
    local value=$2

    if [ -z "$key" ]; then
        # Afficher toute la configuration
        if [ -f "$CONFIG_FILE" ]; then
            echo "${BOLD}Configuration actuelle:${NC}"
            cat "$CONFIG_FILE" | while IFS='=' read -r k v; do
                if [[ ! "$k" =~ ^[#\[] ]]; then
                    echo "  ${BLUE}$k${NC} = $v"
                fi
            done
        else
            echo "Aucun fichier de configuration trouvé"
        fi
    elif [ -z "$value" ]; then
        # Lire une valeur
        if [ -f "$CONFIG_FILE" ]; then
            grep "^$key=" "$CONFIG_FILE" | cut -d'=' -f2-
        fi
    else
        # Écrire une valeur
        if [ ! -f "$CONFIG_FILE" ]; then
            mkdir -p "$(dirname "$CONFIG_FILE")"
            touch "$CONFIG_FILE"
        fi

        # Remplacer ou ajouter la clé
        if grep -q "^$key=" "$CONFIG_FILE"; then
            sed -i "s/^$key=.*/$key=$value/" "$CONFIG_FILE"
            echo "${GREEN}✓${NC} Configuration mise à jour: $key = $value"
        else
            echo "$key=$value" >> "$CONFIG_FILE"
            echo "${GREEN}✓${NC} Configuration ajoutée: $key = $value"
        fi
    fi
}

# Afficher les logs
show_logs() {
    local lines=${1:-50}
    local log_file="$HOME/.local/share/$APP_NAME/app.log"

    if [ -f "$log_file" ]; then
        echo "${BOLD}Dernières $lines lignes de log:${NC}"
        tail -n "$lines" "$log_file" | while IFS= read -r line; do
            # Colorier selon le niveau
            if [[ "$line" =~ ERROR ]]; then
                echo -e "${RED}$line${NC}"
            elif [[ "$line" =~ WARNING ]]; then
                echo -e "${YELLOW}$line${NC}"
            elif [[ "$line" =~ INFO ]]; then
                echo -e "${GREEN}$line${NC}"
            else
                echo "$line"
            fi
        done
    else
        echo "Aucun fichier de log trouvé"
    fi
}

# Obtenir le statut de l'application
get_status() {
    local pid=$(pgrep -x "$APP_NAME" | head -1)

    if [ -n "$pid" ]; then
        echo -e "${GREEN}●${NC} $APP_NAME est en cours d'exécution (PID: $pid)"

        # Afficher les détails
        echo
        echo "${BOLD}Détails du processus:${NC}"
        ps -p "$pid" -o pid,user,%cpu,%mem,vsz,rss,start,time,cmd --no-headers

        # Utilisation mémoire
        local mem_kb=$(ps -p "$pid" -o rss= | tr -d ' ')
        local mem_mb=$((mem_kb / 1024))
        echo
        echo "${BOLD}Utilisation mémoire:${NC} ${mem_mb}MB"

        # Uptime
        local start_time=$(ps -p "$pid" -o lstart= | tr -s ' ')
        echo "${BOLD}Démarré:${NC} $start_time"

        # Fichiers ouverts
        local open_files=$(lsof -p "$pid" 2>/dev/null | wc -l)
        echo "${BOLD}Fichiers ouverts:${NC} $open_files"

        # Connexions réseau
        local connections=$(lsof -p "$pid" -i 2>/dev/null | wc -l)
        if [ $connections -gt 0 ]; then
            echo "${BOLD}Connexions réseau:${NC} $connections"
        fi
    else
        echo -e "${RED}●${NC} $APP_NAME n'est pas en cours d'exécution"
    fi
}

# Informations système
show_info() {
    echo "${BOLD}=== Informations Système ===${NC}"
    echo
    echo "${BLUE}OS:${NC} $(lsb_release -ds 2>/dev/null || cat /etc/os-release | grep PRETTY_NAME | cut -d'"' -f2)"
    echo "${BLUE}Kernel:${NC} $(uname -r)"
    echo "${BLUE}Architecture:${NC} $(uname -m)"
    echo "${BLUE}Hostname:${NC} $(hostname)"
    echo "${BLUE}Utilisateur:${NC} $USER"
    echo
    echo "${BOLD}=== Environnement ===${NC}"
    echo "${BLUE}DISPLAY:${NC} ${DISPLAY:-non défini}"
    echo "${BLUE}WAYLAND_DISPLAY:${NC} ${WAYLAND_DISPLAY:-non défini}"
    echo "${BLUE}SHELL:${NC} $SHELL"
    echo "${BLUE}LANG:${NC} $LANG"
    echo
    echo "${BOLD}=== Application ===${NC}"
    echo "${BLUE}Version:${NC} $VERSION"
    echo "${BLUE}Config:${NC} $CONFIG_FILE"
    echo "${BLUE}Données:${NC} $HOME/.local/share/$APP_NAME"
    echo "${BLUE}Cache:${NC} $HOME/.cache/$APP_NAME"

    # Espace disque utilisé
    if [ -d "$HOME/.local/share/$APP_NAME" ]; then
        local size=$(du -sh "$HOME/.local/share/$APP_NAME" 2>/dev/null | cut -f1)
        echo "${BLUE}Espace utilisé:${NC} $size"
    fi
}

# Shell interactif
interactive_shell() {
    echo "${BOLD}Shell interactif $APP_NAME${NC}"
    echo "Tapez 'help' pour l'aide, 'exit' pour quitter"
    echo

    while true; do
        # Prompt personnalisé
        read -p "${GREEN}$APP_NAME>${NC} " cmd args

        case "$cmd" in
            help)
                echo "Commandes disponibles:"
                echo "  status    - État de l'application"
                echo "  config    - Gérer la configuration"
                echo "  log       - Afficher les logs"
                echo "  exec      - Exécuter une commande dans l'app"
                echo "  clear     - Effacer l'écran"
                echo "  exit      - Quitter le shell"
                ;;
            status)
                get_status
                ;;
            config)
                manage_config $args
                ;;
            log)
                show_logs ${args:-20}
                ;;
            exec)
                # Envoyer une commande à l'application via DBus ou socket
                echo "Exécution: $args"
                echo "$args" | nc -U /tmp/$APP_NAME.socket 2>/dev/null || \
                    echo "Impossible de communiquer avec l'application"
                ;;
            clear)
                clear
                ;;
            exit|quit)
                echo "Au revoir!"
                break
                ;;
            "")
                # Ligne vide, ne rien faire
                ;;
            *)
                echo "Commande inconnue: $cmd (tapez 'help' pour l'aide)"
                ;;
        esac
    done
}

# Nettoyer les fichiers temporaires
clean_temp() {
    echo "Nettoyage des fichiers temporaires..."

    local freed=0
    local count=0

    # Répertoires à nettoyer
    for dir in "/tmp/${APP_NAME}_*" "$HOME/.cache/$APP_NAME/temp/*"; do
        for file in $dir; do
            if [ -e "$file" ]; then
                size=$(du -sb "$file" 2>/dev/null | cut -f1)
                rm -rf "$file"
                freed=$((freed + size))
                count=$((count + 1))
            fi
        done
    done

    if [ $count -gt 0 ]; then
        echo "${GREEN}✓${NC} $count fichiers supprimés"
        echo "${GREEN}✓${NC} $(numfmt --to=iec-i --suffix=B $freed) libérés"
    else
        echo "Aucun fichier temporaire trouvé"
    fi
}

# Traitement des commandes
process_command() {
    case "$1" in
        start)
            echo "Démarrage de $APP_NAME..."
            if pgrep -x "$APP_NAME" > /dev/null; then
                echo "${YELLOW}⚠${NC} $APP_NAME est déjà en cours d'exécution"
            else
                nohup "$APP_NAME" > /dev/null 2>&1 &
                sleep 1
                if pgrep -x "$APP_NAME" > /dev/null; then
                    echo "${GREEN}✓${NC} $APP_NAME démarré avec succès"
                else
                    echo "${RED}✗${NC} Échec du démarrage"
                fi
            fi
            ;;
        stop)
            echo "Arrêt de $APP_NAME..."
            pkill -x "$APP_NAME"
            sleep 1
            if pgrep -x "$APP_NAME" > /dev/null; then
                echo "${YELLOW}⚠${NC} Arrêt forcé..."
                pkill -9 -x "$APP_NAME"
            fi
            echo "${GREEN}✓${NC} $APP_NAME arrêté"
            ;;
        restart)
            $0 stop
            sleep 1
            $0 start
            ;;
        status)
            get_status
            ;;
        config)
            manage_config "$2" "$3"
            ;;
        log)
            show_logs "$2"
            ;;
        clean)
            clean_temp
            ;;
        backup)
            /usr/local/bin/${APP_NAME}-backup.sh
            ;;
        restore)
            /usr/local/bin/${APP_NAME}-restore.sh "$2"
            ;;
        update)
            /usr/local/bin/${APP_NAME}-update.sh
            ;;
        info)
            show_info
            ;;
        shell)
            interactive_shell
            ;;
        *)
            echo "Commande inconnue: $1"
            show_help
            exit 1
            ;;
    esac
}

# Programme principal
main() {
    # Options globales
    DEBUG=0
    QUIET=0

    # Parser les options
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help)
                show_help
                exit 0
                ;;
            -v|--version)
                echo "$APP_NAME CLI version $VERSION"
                exit 0
                ;;
            -d|--debug)
                DEBUG=1
                shift
                ;;
            -q|--quiet)
                QUIET=1
                shift
                ;;
            *)
                # C'est une commande
                process_command "$@"
                exit $?
                ;;
        esac
    done

    # Si aucune commande, afficher l'aide
    show_help
}

# Exécuter si ce n'est pas sourcé
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    main "$@"
fi
```

### Complétion automatique avancée

Créez `/etc/bash_completion.d/monappli` :

```bash
# Complétion Bash pour monappli-cli

_monappli_cli() {
    local cur prev words cword
    _init_completion || return

    # Commandes principales
    local commands="start stop restart status config log clean backup restore update info shell"

    # Options globales
    local options="-h --help -v --version -d --debug -q --quiet"

    # Premier argument : commande ou option
    if [ $cword -eq 1 ]; then
        COMPREPLY=( $(compgen -W "$commands $options" -- "$cur") )
        return
    fi

    # Complétion selon la commande
    case "${words[1]}" in
        config)
            if [ $cword -eq 2 ]; then
                # Lister les clés de configuration existantes
                if [ -f "$HOME/.config/monappli/config.ini" ]; then
                    local keys=$(grep -E '^[a-zA-Z]' "$HOME/.config/monappli/config.ini" | cut -d'=' -f1)
                    COMPREPLY=( $(compgen -W "$keys" -- "$cur") )
                fi
            elif [ $cword -eq 3 ]; then
                # Suggestions de valeurs selon la clé
                case "${words[2]}" in
                    theme)
                        COMPREPLY=( $(compgen -W "light dark auto" -- "$cur") )
                        ;;
                    language)
                        COMPREPLY=( $(compgen -W "fr en de es it" -- "$cur") )
                        ;;
                    loglevel)
                        COMPREPLY=( $(compgen -W "debug info warning error critical" -- "$cur") )
                        ;;
                esac
            fi
            ;;
        log)
            # Suggestions de nombre de lignes
            COMPREPLY=( $(compgen -W "10 20 50 100 200 500 1000" -- "$cur") )
            ;;
        restore)
            # Lister les fichiers de backup
            if [ -d "/var/backups/monappli" ]; then
                local backups=$(ls -1 /var/backups/monappli/*.tar.gz 2>/dev/null | xargs -n1 basename)
                COMPREPLY=( $(compgen -W "$backups" -- "$cur") )
            fi
            ;;
        *)
            # Pas de complétion supplémentaire
            ;;
    esac
}

# Enregistrer la fonction de complétion
complete -F _monappli_cli monappli-cli
complete -F _monappli_cli monappli

# Alias utiles
alias mac='monappli-cli'
alias ma='monappli-cli'
```

## Intégration avec les notifications système

### Script de notifications

Créez `monappli-notify.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
ICON_PATH="/usr/share/icons/hicolor/48x48/apps/monappli.png"

# Détection du système de notification
detect_notification_system() {
    if command -v notify-send &> /dev/null; then
        echo "libnotify"
    elif command -v kdialog &> /dev/null; then
        echo "kdialog"
    elif command -v zenity &> /dev/null; then
        echo "zenity"
    else
        echo "none"
    fi
}

# Envoyer une notification
send_notification() {
    local urgency=$1  # low, normal, critical
    local title=$2
    local message=$3
    local system=$(detect_notification_system)

    case "$system" in
        libnotify)
            notify-send -u "$urgency" -i "$ICON_PATH" "$title" "$message"
            ;;
        kdialog)
            case "$urgency" in
                critical)
                    kdialog --error "$message" --title "$title"
                    ;;
                low)
                    kdialog --passivepopup "$message" 5 --title "$title"
                    ;;
                *)
                    kdialog --msgbox "$message" --title "$title"
                    ;;
            esac
            ;;
        zenity)
            case "$urgency" in
                critical)
                    zenity --error --text="$message" --title="$title"
                    ;;
                low)
                    zenity --notification --text="$title: $message"
                    ;;
                *)
                    zenity --info --text="$message" --title="$title"
                    ;;
            esac
            ;;
        none)
            # Fallback sur le terminal
            echo "[$urgency] $title: $message"
            ;;
    esac
}

# Surveiller les événements et envoyer des notifications
monitor_and_notify() {
    local log_file="$HOME/.local/share/$APP_NAME/app.log"

    if [ ! -f "$log_file" ]; then
        echo "Fichier de log non trouvé"
        exit 1
    fi

    # Suivre le log en temps réel
    tail -f "$log_file" | while IFS= read -r line; do
        # Détecter les patterns importants
        if [[ "$line" =~ ERROR ]]; then
            send_notification "critical" "$APP_NAME - Erreur" \
                "Une erreur s'est produite. Consultez les logs."
        elif [[ "$line" =~ WARNING ]]; then
            send_notification "normal" "$APP_NAME - Attention" \
                "Un avertissement a été généré."
        elif [[ "$line" =~ "Update available" ]]; then
            send_notification "normal" "$APP_NAME - Mise à jour" \
                "Une nouvelle version est disponible."
        elif [[ "$line" =~ "Backup completed" ]]; then
            send_notification "low" "$APP_NAME - Backup" \
                "Sauvegarde terminée avec succès."
        fi
    done
}

# Utilisation depuis FreePascal
# L'application peut appeler ce script pour envoyer des notifications :
# ExecuteShellCommand('monappli-notify.sh normal "Titre" "Message"');

# Programme principal
case "$1" in
    monitor)
        monitor_and_notify
        ;;
    *)
        # Utilisation directe
        if [ $# -ge 3 ]; then
            send_notification "$1" "$2" "$3"
        else
            echo "Usage: $0 [urgency] [title] [message]"
            echo "       $0 monitor"
            echo ""
            echo "Urgency: low, normal, critical"
            exit 1
        fi
        ;;
esac
```

## Intégration avec le système de fichiers

### Script de surveillance de fichiers

Créez `monappli-watch.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
WATCH_DIR="$HOME/Documents/MonAppli"
PROCESSED_DIR="$WATCH_DIR/processed"
ERROR_DIR="$WATCH_DIR/errors"

# Vérifier les dépendances
check_dependencies() {
    if ! command -v inotifywait &> /dev/null; then
        echo "inotify-tools est requis. Installez avec:"
        echo "  sudo apt install inotify-tools"
        exit 1
    fi
}

# Initialiser les répertoires
init_directories() {
    mkdir -p "$WATCH_DIR"
    mkdir -p "$PROCESSED_DIR"
    mkdir -p "$ERROR_DIR"

    echo "Surveillance de: $WATCH_DIR"
    echo "Fichiers traités: $PROCESSED_DIR"
    echo "Erreurs: $ERROR_DIR"
}

# Traiter un fichier
process_file() {
    local file="$1"
    local filename=$(basename "$file")
    local extension="${filename##*.}"

    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Traitement de: $filename"

    # Traitement selon l'extension
    case "$extension" in
        txt|dat)
            # Traiter avec l'application
            if $APP_NAME --process "$file"; then
                mv "$file" "$PROCESSED_DIR/"
                echo "  ✓ Traité avec succès"
            else
                mv "$file" "$ERROR_DIR/"
                echo "  ✗ Erreur de traitement"
            fi
            ;;
        csv)
            # Import CSV
            if $APP_NAME --import-csv "$file"; then
                mv "$file" "$PROCESSED_DIR/"
                echo "  ✓ CSV importé"
            else
                mv "$file" "$ERROR_DIR/"
                echo "  ✗ Erreur d'import CSV"
            fi
            ;;
        json)
            # Validation et traitement JSON
            if jq empty "$file" 2>/dev/null; then
                if $APP_NAME --process-json "$file"; then
                    mv "$file" "$PROCESSED_DIR/"
                    echo "  ✓ JSON traité"
                else
                    mv "$file" "$ERROR_DIR/"
                    echo "  ✗ Erreur de traitement JSON"
                fi
            else
                mv "$file" "$ERROR_DIR/"
                echo "  ✗ JSON invalide"
            fi
            ;;
        *)
            echo "  ⚠ Type de fichier non supporté: $extension"
            ;;
    esac
}

# Surveiller le répertoire
watch_directory() {
    echo "Démarrage de la surveillance..."
    echo "Déposez des fichiers dans: $WATCH_DIR"
    echo "Appuyez sur Ctrl+C pour arrêter"
    echo

    # Traiter les fichiers existants
    for file in "$WATCH_DIR"/*; do
        if [ -f "$file" ]; then
            process_file "$file"
        fi
    done

    # Surveiller les nouveaux fichiers
    inotifywait -m -e create -e moved_to --format "%w%f" "$WATCH_DIR" | while read file; do
        # Attendre que le fichier soit complètement écrit
        sleep 1

        if [ -f "$file" ]; then
            process_file "$file"
        fi
    done
}

# Script de synchronisation
sync_with_remote() {
    local remote_host=$1
    local remote_dir=$2

    if [ -z "$remote_host" ] || [ -z "$remote_dir" ]; then
        echo "Usage: $0 sync <host> <remote_dir>"
        exit 1
    fi

    echo "Synchronisation avec $remote_host:$remote_dir"

    # Synchroniser avec rsync
    rsync -avz --progress \
        --exclude "processed/" \
        --exclude "errors/" \
        "$WATCH_DIR/" \
        "$remote_host:$remote_dir/"

    if [ $? -eq 0 ]; then
        echo "✓ Synchronisation terminée"
    else
        echo "✗ Erreur de synchronisation"
    fi
}

# Programme principal
check_dependencies
init_directories

case "${1:-watch}" in
    watch)
        watch_directory
        ;;
    sync)
        sync_with_remote "$2" "$3"
        ;;
    status)
        echo "Fichiers en attente: $(find "$WATCH_DIR" -maxdepth 1 -type f | wc -l)"
        echo "Fichiers traités: $(find "$PROCESSED_DIR" -type f | wc -l)"
        echo "Fichiers en erreur: $(find "$ERROR_DIR" -type f | wc -l)"
        ;;
    clean)
        read -p "Supprimer les fichiers traités et en erreur ? (o/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Oo]$ ]]; then
            rm -f "$PROCESSED_DIR"/*
            rm -f "$ERROR_DIR"/*
            echo "✓ Nettoyage effectué"
        fi
        ;;
    *)
        echo "Usage: $0 [watch|sync|status|clean]"
        exit 1
        ;;
esac
```

## Création de liens symboliques et raccourcis

### Script d'installation des raccourcis

Créez `setup-shortcuts.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
INSTALL_PREFIX="/usr/local"

# Créer les liens symboliques
create_symlinks() {
    echo "Création des liens symboliques..."

    # Lien principal
    ln -sf "$INSTALL_PREFIX/bin/$APP_NAME" "/usr/bin/$APP_NAME"

    # Liens pour les commandes CLI
    ln -sf "$INSTALL_PREFIX/bin/${APP_NAME}-cli" "/usr/bin/mac"
    ln -sf "$INSTALL_PREFIX/bin/${APP_NAME}-cli" "/usr/bin/monappli"

    # Lien pour le man page
    if [ -f "$INSTALL_PREFIX/share/man/man1/${APP_NAME}.1" ]; then
        ln -sf "$INSTALL_PREFIX/share/man/man1/${APP_NAME}.1" \
               "/usr/share/man/man1/${APP_NAME}.1"
        mandb  # Mettre à jour la base de données man
    fi

    echo "✓ Liens symboliques créés"
}

# Créer le fichier desktop pour tous les utilisateurs
create_desktop_entry() {
    echo "Création de l'entrée desktop..."

    cat > "/usr/share/applications/${APP_NAME}.desktop" << EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=Mon Application
GenericName=Application FreePascal/Lazarus
Comment=Ma super application
Exec=${INSTALL_PREFIX}/bin/${APP_NAME} %F
Icon=${APP_NAME}
Terminal=false
Categories=Utility;Development;
MimeType=application/x-monappli;
Actions=Debug;Config;

[Desktop Action Debug]
Name=Lancer en mode Debug
Exec=${INSTALL_PREFIX}/bin/${APP_NAME} --debug

[Desktop Action Config]
Name=Ouvrir la configuration
Exec=${INSTALL_PREFIX}/bin/${APP_NAME}-cli config
EOF

    # Mettre à jour la base de données desktop
    update-desktop-database

    echo "✓ Entrée desktop créée"
}

# Créer les associations de fichiers
create_mime_types() {
    echo "Configuration des types MIME..."

    # Créer le type MIME personnalisé
    cat > "/usr/share/mime/packages/${APP_NAME}.xml" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
    <mime-type type="application/x-monappli">
        <comment>Fichier MonAppli</comment>
        <comment xml:lang="fr">Fichier MonAppli</comment>
        <glob pattern="*.mapp"/>
        <glob pattern="*.monappli"/>
        <icon name="monappli"/>
    </mime-type>
</mime-info>
EOF

    # Mettre à jour la base de données MIME
    update-mime-database /usr/share/mime

    echo "✓ Types MIME configurés"
}

# Créer le manuel (man page)
create_man_page() {
    echo "Création du manuel..."

    mkdir -p "$INSTALL_PREFIX/share/man/man1"

    cat > "$INSTALL_PREFIX/share/man/man1/${APP_NAME}.1" << 'EOF'
.TH MONAPPLI 1 "2024" "Version 1.0.0" "Manuel de MonAppli"
.SH NOM
monappli \- Application FreePascal/Lazarus
.SH SYNOPSIS
.B monappli
[\fIOPTIONS\fR] [\fIFICHIER\fR]
.SH DESCRIPTION
MonAppli est une application développée avec FreePascal/Lazarus
qui fait des choses extraordinaires.
.SH OPTIONS
.TP
.B \-h, \-\-help
Afficher l'aide et quitter
.TP
.B \-v, \-\-version
Afficher la version et quitter
.TP
.B \-d, \-\-debug
Activer le mode debug
.TP
.B \-c, \-\-config \fIFICHIER\fR
Utiliser un fichier de configuration spécifique
.TP
.B \-\-process \fIFICHIER\fR
Traiter un fichier et quitter
.SH FICHIERS
.TP
.I ~/.config/monappli/config.ini
Fichier de configuration utilisateur
.TP
.I ~/.local/share/monappli/
Données de l'application
.TP
.I ~/.cache/monappli/
Cache de l'application
.SH VARIABLES D'ENVIRONNEMENT
.TP
.B MONAPPLI_CONFIG
Chemin vers un fichier de configuration alternatif
.TP
.B MONAPPLI_DEBUG
Si défini à 1, active le mode debug
.TP
.B MONAPPLI_LOG_LEVEL
Niveau de log (DEBUG, INFO, WARNING, ERROR, CRITICAL)
.SH EXEMPLES
.TP
Lancer l'application normalement :
.B monappli
.TP
Lancer en mode debug :
.B monappli --debug
.TP
Traiter un fichier :
.B monappli --process data.txt
.TP
Utiliser une configuration personnalisée :
.B monappli --config ~/custom.ini
.SH CODES DE RETOUR
.TP
.B 0
Succès
.TP
.B 1
Erreur générale
.TP
.B 2
Fichier non trouvé
.TP
.B 3
Erreur de configuration
.TP
.B 4
Erreur de permissions
.SH AUTEUR
Votre Nom <votre.email@example.com>
.SH BOGUES
Rapporter les bogues à https://github.com/username/monappli/issues
.SH VOIR AUSSI
.BR monappli-cli (1),
.BR monappli.conf (5)
.SH COPYRIGHT
Copyright © 2024 Votre Nom. License GPL version 3 ou ultérieure.
EOF

    # Compresser le man page
    gzip -f "$INSTALL_PREFIX/share/man/man1/${APP_NAME}.1"

    echo "✓ Manuel créé"
}

# Programme principal du setup
main() {
    if [ "$EUID" -ne 0 ]; then
        echo "Ce script doit être exécuté avec sudo"
        exit 1
    fi

    echo "Configuration des raccourcis pour $APP_NAME"
    echo "========================================="
    echo

    create_symlinks
    create_desktop_entry
    create_mime_types
    create_man_page

    echo
    echo "✓ Configuration terminée!"
    echo
    echo "Les utilisateurs peuvent maintenant :"
    echo "  - Lancer l'application depuis le menu"
    echo "  - Double-cliquer sur les fichiers .mapp"
    echo "  - Utiliser 'man monappli' pour l'aide"
    echo "  - Utiliser 'mac' comme raccourci CLI"
}

main "$@"
```

## Communication avancée entre Bash et FreePascal

### Socket Unix pour communication bidirectionnelle

Script Bash serveur (`monappli-socket-server.sh`) :

```bash
#!/usr/bin/env bash

SOCKET_PATH="/tmp/monappli.sock"
LOG_FILE="/var/log/monappli/socket.log"

# Nettoyer le socket existant
cleanup() {
    rm -f "$SOCKET_PATH"
    echo "[$(date)] Socket serveur arrêté" >> "$LOG_FILE"
    exit
}

trap cleanup EXIT INT TERM

# Démarrer le serveur
start_server() {
    rm -f "$SOCKET_PATH"
    echo "[$(date)] Socket serveur démarré" >> "$LOG_FILE"

    # Utiliser socat pour créer un serveur socket Unix
    if command -v socat &> /dev/null; then
        socat UNIX-LISTEN:$SOCKET_PATH,fork EXEC:"/usr/local/bin/monappli-handler.sh"
    else
        # Alternative avec nc (netcat)
        while true; do
            nc -lU "$SOCKET_PATH" | while read line; do
                echo "[$(date)] Reçu: $line" >> "$LOG_FILE"
                process_command "$line"
            done
        done
    fi
}

# Traiter les commandes
process_command() {
    local cmd=$1
    local response=""

    case "$cmd" in
        STATUS)
            if pgrep -x monappli > /dev/null; then
                response="RUNNING"
            else
                response="STOPPED"
            fi
            ;;
        RELOAD)
            pkill -HUP monappli
            response="OK"
            ;;
        STATS)
            local pid=$(pgrep -x monappli)
            if [ -n "$pid" ]; then
                local mem=$(ps -p $pid -o rss= | tr -d ' ')
                local cpu=$(ps -p $pid -o %cpu= | tr -d ' ')
                response="MEM:$mem CPU:$cpu"
            else
                response="ERROR:NOT_RUNNING"
            fi
            ;;
        *)
            response="ERROR:UNKNOWN_COMMAND"
            ;;
    esac

    echo "$response"
}

start_server
```

Code FreePascal pour communiquer avec le socket :

```pascal
uses
  Sockets, SysUtils, BaseUnix;

type
  TSocketClient = class
  private
    FSocketPath: string;
    FSocket: TSocket;
  public
    constructor Create(const SocketPath: string);
    destructor Destroy; override;
    function Connect: Boolean;
    function SendCommand(const Command: string): string;
    procedure Disconnect;
  end;

constructor TSocketClient.Create(const SocketPath: string);
begin
  FSocketPath := SocketPath;
  FSocket := fpSocket(AF_UNIX, SOCK_STREAM, 0);
end;

function TSocketClient.Connect: Boolean;
var
  Addr: TUnixSockAddr;
begin
  Result := False;

  if FSocket < 0 then
    Exit;

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sun_family := AF_UNIX;
  StrPCopy(Addr.sun_path, FSocketPath);

  Result := fpConnect(FSocket, @Addr, SizeOf(Addr)) = 0;
end;

function TSocketClient.SendCommand(const Command: string): string;
var
  Buffer: array[0..1023] of Char;
  BytesSent, BytesReceived: Integer;
  Cmd: AnsiString;
begin
  Result := '';

  Cmd := Command + #10;  // Ajouter newline
  BytesSent := fpSend(FSocket, PChar(Cmd)^, Length(Cmd), 0);

  if BytesSent > 0 then
  begin
    BytesReceived := fpRecv(FSocket, Buffer, SizeOf(Buffer) - 1, 0);
    if BytesReceived > 0 then
    begin
      Buffer[BytesReceived] := #0;
      Result := StrPas(Buffer);
    end;
  end;
end;

procedure TSocketClient.Disconnect;
begin
  if FSocket >= 0 then
  begin
    fpShutdown(FSocket, 2);
    CloseSocket(FSocket);
  end;
end;

// Utilisation
procedure TForm1.CheckApplicationStatus;
var
  Client: TSocketClient;
  Status: string;
begin
  Client := TSocketClient.Create('/tmp/monappli.sock');
  try
    if Client.Connect then
    begin
      Status := Client.SendCommand('STATUS');
      ShowMessage('État: ' + Status);

      // Obtenir les statistiques
      Status := Client.SendCommand('STATS');
      ShowMessage('Stats: ' + Status);
    end
    else
      ShowMessage('Impossible de se connecter au socket');
  finally
    Client.Free;
  end;
end;
```

### Utilisation de D-Bus pour la communication

Script Bash pour envoyer des messages D-Bus :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
DBUS_NAME="com.example.MonAppli"
DBUS_PATH="/com/example/MonAppli"
DBUS_INTERFACE="com.example.MonAppli"

# Envoyer une commande via D-Bus
send_dbus_command() {
    local method=$1
    shift
    local args="$@"

    if command -v dbus-send &> /dev/null; then
        dbus-send --session \
            --dest=$DBUS_NAME \
            --type=method_call \
            --print-reply \
            $DBUS_PATH \
            $DBUS_INTERFACE.$method \
            $args
    else
        echo "dbus-send non disponible"
        return 1
    fi
}

# Surveiller les signaux D-Bus
monitor_dbus_signals() {
    echo "Surveillance des signaux D-Bus pour $APP_NAME..."

    dbus-monitor --session "interface='$DBUS_INTERFACE'" | while read line; do
        if [[ "$line" =~ "member=StatusChanged" ]]; then
            echo "État changé détecté"
            # Déclencher une action
            /usr/local/bin/monappli-notify.sh normal "$APP_NAME" "État modifié"
        elif [[ "$line" =~ "member=Error" ]]; then
            echo "Erreur détectée"
            /usr/local/bin/monappli-notify.sh critical "$APP_NAME" "Une erreur s'est produite"
        fi
    done
}

# Exemples d'utilisation
case "$1" in
    reload)
        send_dbus_command "Reload"
        ;;
    set-config)
        send_dbus_command "SetConfig" "string:$2" "string:$3"
        ;;
    get-status)
        send_dbus_command "GetStatus"
        ;;
    monitor)
        monitor_dbus_signals
        ;;
    *)
        echo "Usage: $0 {reload|set-config|get-status|monitor}"
        exit 1
        ;;
esac
```

## Scripts de tests et validation

### Suite de tests automatisés

Créez `test-suite.sh` :

```bash
#!/usr/bin/env bash

APP_NAME="monappli"
TEST_DIR="/tmp/monappli-tests"
RESULTS_FILE="test-results-$(date +%Y%m%d_%H%M%S).txt"

# Couleurs
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Compteurs
TESTS_TOTAL=0
TESTS_PASSED=0
TESTS_FAILED=0

# Préparer l'environnement de test
setup_test_env() {
    echo "Préparation de l'environnement de test..."
    rm -rf "$TEST_DIR"
    mkdir -p "$TEST_DIR"
    cd "$TEST_DIR"

    # Créer des fichiers de test
    echo "test data" > test.txt
    echo "key1=value1" > test.ini
    echo '{"test": "data"}' > test.json
    echo "col1,col2,col3" > test.csv
    echo "1,2,3" >> test.csv
}

# Fonction de test générique
run_test() {
    local test_name=$1
    local command=$2
    local expected_exit_code=${3:-0}

    TESTS_TOTAL=$((TESTS_TOTAL + 1))

    echo -n "Test: $test_name... "

    # Exécuter la commande
    eval "$command" > /tmp/test_output.txt 2>&1
    local exit_code=$?

    # Vérifier le résultat
    if [ $exit_code -eq $expected_exit_code ]; then
        echo -e "${GREEN}✓ PASSÉ${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        echo "✓ $test_name" >> "$RESULTS_FILE"
    else
        echo -e "${RED}✗ ÉCHOUÉ${NC}"
        echo "  Commande: $command"
        echo "  Code attendu: $expected_exit_code, obtenu: $exit_code"
        echo "  Sortie:"
        cat /tmp/test_output.txt | sed 's/^/    /'
        TESTS_FAILED=$((TESTS_FAILED + 1))
        echo "✗ $test_name (code: $exit_code)" >> "$RESULTS_FILE"
    fi
}

# Tests de base
test_basic() {
    echo -e "\n${YELLOW}=== Tests de base ===${NC}"

    run_test "Vérification de l'installation" "which $APP_NAME"
    run_test "Affichage de la version" "$APP_NAME --version"
    run_test "Affichage de l'aide" "$APP_NAME --help"
    run_test "Option invalide" "$APP_NAME --invalid-option" 1
}

# Tests de démarrage/arrêt
test_lifecycle() {
    echo -e "\n${YELLOW}=== Tests du cycle de vie ===${NC}"

    # Arrêter si déjà en cours
    pkill -x $APP_NAME 2>/dev/null
    sleep 1

    run_test "Démarrage de l'application" "$APP_NAME --daemon"
    sleep 2
    run_test "Vérification du processus" "pgrep -x $APP_NAME"
    run_test "Arrêt de l'application" "pkill -x $APP_NAME"
    sleep 1
    run_test "Vérification de l'arrêt" "! pgrep -x $APP_NAME"
}

# Tests de traitement de fichiers
test_file_processing() {
    echo -e "\n${YELLOW}=== Tests de traitement de fichiers ===${NC}"

    run_test "Traitement fichier texte" "$APP_NAME --process test.txt"
    run_test "Import CSV" "$APP_NAME --import-csv test.csv"
    run_test "Traitement JSON" "$APP_NAME --process-json test.json"
    run_test "Fichier inexistant" "$APP_NAME --process nonexistent.txt" 2
}

# Tests de configuration
test_configuration() {
    echo -e "\n${YELLOW}=== Tests de configuration ===${NC}"

    run_test "Lecture configuration" "$APP_NAME-cli config"
    run_test "Écriture configuration" "$APP_NAME-cli config test_key test_value"
    run_test "Vérification écriture" "grep -q 'test_key=test_value' ~/.config/$APP_NAME/config.ini"
}

# Tests de performance
test_performance() {
    echo -e "\n${YELLOW}=== Tests de performance ===${NC}"

    # Créer un gros fichier
    dd if=/dev/urandom of=large_file.dat bs=1M count=10 2>/dev/null

    echo -n "Test: Traitement fichier volumineux... "
    start_time=$(date +%s%N)
    $APP_NAME --process large_file.dat > /dev/null 2>&1
    end_time=$(date +%s%N)
    elapsed_ms=$(( (end_time - start_time) / 1000000 ))

    if [ $elapsed_ms -lt 5000 ]; then
        echo -e "${GREEN}✓ PASSÉ${NC} ($elapsed_ms ms)"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${YELLOW}⚠ LENT${NC} ($elapsed_ms ms)"
    fi
    TESTS_TOTAL=$((TESTS_TOTAL + 1))
}

# Tests de robustesse
test_robustness() {
    echo -e "\n${YELLOW}=== Tests de robustesse ===${NC}"

    # Fichier corrompu
    echo -e "\x00\x01\x02\x03" > corrupted.dat
    run_test "Fichier corrompu" "$APP_NAME --process corrupted.dat" 1

    # Permissions insuffisantes
    touch readonly.txt
    chmod 000 readonly.txt
    run_test "Fichier sans permission" "$APP_NAME --process readonly.txt" 4
    chmod 644 readonly.txt

    # Espace disque (simulation)
    run_test "Espace disque insuffisant" "df -h / | grep -q '100%' && echo 'Plein' || echo 'OK'"
}

# Tests d'intégration
test_integration() {
    echo -e "\n${YELLOW}=== Tests d'intégration ===${NC}"

    run_test "Socket Unix disponible" "test -S /tmp/$APP_NAME.sock"
    run_test "Communication socket" "echo 'STATUS' | nc -U /tmp/$APP_NAME.sock"
    run_test "Journalisation active" "test -f ~/.local/share/$APP_NAME/app.log"
}

# Tests de sécurité
test_security() {
    echo -e "\n${YELLOW}=== Tests de sécurité ===${NC}"

    # Injection de commandes
    run_test "Protection injection SQL" "$APP_NAME --query \"'; DROP TABLE users; --\""

    # Path traversal
    run_test "Protection path traversal" "$APP_NAME --process ../../../etc/passwd" 1

    # Buffer overflow (test avec une chaîne très longue)
    long_string=$(python3 -c "print('A' * 10000)")
    run_test "Protection buffer overflow" "$APP_NAME --process \"$long_string\""
}

# Génération du rapport
generate_report() {
    echo -e "\n${YELLOW}=== Rapport de tests ===${NC}"
    echo "========================="
    echo "Tests total    : $TESTS_TOTAL"
    echo -e "Tests passés   : ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Tests échoués  : ${RED}$TESTS_FAILED${NC}"

    local success_rate=$(( TESTS_PASSED * 100 / TESTS_TOTAL ))
    echo -n "Taux de succès : "

    if [ $success_rate -ge 90 ]; then
        echo -e "${GREEN}${success_rate}%${NC}"
    elif [ $success_rate -ge 70 ]; then
        echo -e "${YELLOW}${success_rate}%${NC}"
    else
        echo -e "${RED}${success_rate}%${NC}"
    fi

    echo
    echo "Rapport détaillé : $RESULTS_FILE"

    # Code de retour selon le résultat
    if [ $TESTS_FAILED -eq 0 ]; then
        exit 0
    else
        exit 1
    fi
}

# Programme principal
main() {
    echo "================================"
    echo "Suite de tests pour $APP_NAME"
    echo "================================"

    setup_test_env

    test_basic
    test_lifecycle
    test_file_processing
    test_configuration
    test_performance
    test_robustness
    test_integration
    test_security

    generate_report
}

# Nettoyer en cas d'interruption
trap "rm -rf $TEST_DIR; exit" INT TERM

main "$@"
```

## Bonnes pratiques et conseils

### Script de vérification des bonnes pratiques

Créez `check-best-practices.sh` :

```bash
#!/usr/bin/env bash

# Vérifier les bonnes pratiques dans les scripts Bash

check_scripts() {
    local issues=0

    echo "Vérification des scripts Bash..."
    echo "================================"

    for script in *.sh; do
        if [ -f "$script" ]; then
            echo -n "Vérification de $script... "
            local script_issues=0

            # Vérifier le shebang
            if ! head -1 "$script" | grep -q "^#!/"; then
                echo -e "\n  ⚠ Pas de shebang"
                script_issues=$((script_issues + 1))
            fi

            # Vérifier set -e (arrêt sur erreur)
            if ! grep -q "set -e" "$script"; then
                echo -e "\n  ⚠ Pas de 'set -e'"
                script_issues=$((script_issues + 1))
            fi

            # Vérifier les variables non quotées
            if grep -E '\$[A-Za-z_][A-Za-z0-9_]*[^"]' "$script" | grep -v '#'; then
                echo -e "\n  ⚠ Variables non quotées détectées"
                script_issues=$((script_issues + 1))
            fi

            # Vérifier l'utilisation de shellcheck
            if command -v shellcheck &> /dev/null; then
                if ! shellcheck "$script" > /dev/null 2>&1; then
                    echo -e "\n  ⚠ Erreurs shellcheck détectées"
                    script_issues=$((script_issues + 1))
                fi
            fi

            if [ $script_issues -eq 0 ]; then
                echo "✓"
            else
                issues=$((issues + script_issues))
            fi
        fi
    done

    echo
    if [ $issues -eq 0 ]; then
        echo "✓ Tous les scripts respectent les bonnes pratiques"
    else
        echo "⚠ $issues problèmes détectés"
    fi
}

check_scripts
```

### Template de script Bash robuste

Créez `template.sh` comme modèle pour nouveaux scripts :

```bash
#!/usr/bin/env bash

#############################################################################
# Nom du script : template.sh
# Description   : Template pour scripts Bash robustes
# Auteur        : Votre Nom
# Version       : 1.0.0
# Date          : 2024-01-01
#############################################################################

# Options de sécurité
set -euo pipefail  # Arrêt sur erreur, variables non définies, pipe failures
IFS=$'\n\t'       # Séparateur de champs sûr

# Mode debug (décommenter si nécessaire)
# set -x

# Constants
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_NAME="$(basename "$0")"
readonly VERSION="1.0.0"

# Configuration
readonly CONFIG_FILE="${CONFIG_FILE:-$HOME/.config/app/config.ini}"
readonly LOG_FILE="${LOG_FILE:-/var/log/app.log}"

# Couleurs (désactiver si pas de TTY)
if [[ -t 1 ]]; then
    readonly RED='\033[0;31m'
    readonly GREEN='\033[0;32m'
    readonly YELLOW='\033[1;33m'
    readonly NC='\033[0m'
else
    readonly RED=''
    readonly GREEN=''
    readonly YELLOW=''
    readonly NC=''
fi

# Fonction de logging
log() {
    local level=$1
    shift
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] [$level] $*" | tee -a "$LOG_FILE"
}

log_info() { log "INFO" "$@"; }
log_error() { log "ERROR" "$@"; }
log_warning() { log "WARNING" "$@"; }

# Gestion des erreurs
error_exit() {
    log_error "$1"
    exit "${2:-1}"
}

# Nettoyage en sortie
cleanup() {
    log_info "Nettoyage..."
    # Ajouter les commandes de nettoyage ici
}

trap cleanup EXIT

# Vérification des dépendances
check_dependencies() {
    local deps=("bash" "grep" "sed")

    for cmd in "${deps[@]}"; do
        if ! command -v "$cmd" &> /dev/null; then
            error_exit "Dépendance manquante: $cmd"
        fi
    done
}

# Aide
show_help() {
    cat << EOF
Usage: $SCRIPT_NAME [OPTIONS]

Description du script

OPTIONS:
    -h, --help      Afficher cette aide
    -v, --version   Afficher la version
    -c, --config    Fichier de configuration
    -d, --debug     Mode debug

EXEMPLES:
    $SCRIPT_NAME
    $SCRIPT_NAME --config custom.conf

EOF
}

# Parser les arguments
parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            -v|--version)
                echo "$SCRIPT_NAME version $VERSION"
                exit 0
                ;;
            -c|--config)
                CONFIG_FILE="$2"
                shift 2
                ;;
            -d|--debug)
                set -x
                shift
                ;;
            -*)
                error_exit "Option inconnue: $1"
                ;;
            *)
                # Arguments positionnels
                ARGS+=("$1")
                shift
                ;;
        esac
    done
}

# Fonction principale
main() {
    check_dependencies
    parse_arguments "$@"

    log_info "Démarrage de $SCRIPT_NAME"

    # Logique principale ici

    log_info "Fin de $SCRIPT_NAME"
}

# Exécuter seulement si pas sourcé
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    main "$@"
fi
```

## Conclusion

L'intégration entre FreePascal/Lazarus et le shell Bash sous Linux offre des possibilités infinies pour :

- **Automatiser** le déploiement et la maintenance
- **Surveiller** et gérer votre application
- **Intégrer** parfaitement avec l'écosystème Linux
- **Communiquer** entre processus de manière robuste
- **Tester** et valider votre application

Les scripts Bash complètent parfaitement votre application Lazarus en fournissant :
- Installation et configuration automatisées
- Gestion du cycle de vie de l'application
- Monitoring et alertes
- Backup et restauration
- Tests automatisés
- Intégration système complète

En combinant la puissance de FreePascal pour l'application principale et la flexibilité de Bash pour l'intégration système, vous créez une solution professionnelle et maintenable sous Linux/Ubuntu.

⏭️ [Signaux Unix et gestion des processus](/07-specificites-linux-ubuntu/10-signaux-unix-gestion-processus.md)
