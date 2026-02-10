🔝 Retour au [Sommaire](/SOMMAIRE.md)

# AppImage et Flatpak pour applications FreePascal/Lazarus

## Introduction

Lorsque vous développez une application FreePascal/Lazarus pour Linux, vous voulez qu'elle fonctionne sur toutes les distributions (Ubuntu, Fedora, openSUSE, etc.). AppImage et Flatpak sont deux formats modernes qui permettent de distribuer votre application de manière universelle, sans se soucier des différences entre distributions.

### Comparaison rapide

| Aspect | AppImage | Flatpak | DEB/RPM classique |
|--------|----------|---------|-------------------|
| **Installation** | Pas nécessaire | Une fois | Requise |
| **Portabilité** | Excellent (1 fichier) | Bon | Spécifique distro |
| **Mises à jour** | Manuelle ou AppImageUpdate | Automatique | Automatique |
| **Isolation** | Aucune | Sandbox | Aucune |
| **Taille** | Plus gros (tout inclus) | Optimisé (partage libs) | Plus petit |
| **USB/Portable** | ✅ Parfait | ❌ Non | ❌ Non |

## AppImage : L'application portable universelle

### Qu'est-ce qu'un AppImage ?

Un AppImage est un fichier unique qui contient votre application et toutes ses dépendances. C'est comme avoir une application Windows portable, mais pour Linux. L'utilisateur télécharge le fichier, le rend exécutable, et c'est parti !

### Avantages d'AppImage

- **Un seul fichier** : Facile à distribuer et gérer
- **Aucune installation** : Double-clic et ça fonctionne
- **Portable** : Peut s'exécuter depuis une clé USB
- **Compatible** : Fonctionne sur (presque) toutes les distributions
- **Versions multiples** : L'utilisateur peut garder plusieurs versions
- **Pas de privilèges root** : L'utilisateur n'a pas besoin d'être administrateur

### Comment ça fonctionne ?

Un AppImage contient :
1. Votre application compilée
2. Toutes les bibliothèques nécessaires
3. Un petit système de fichiers (SquashFS)
4. Un lanceur (AppRun) qui configure l'environnement

Quand l'utilisateur lance l'AppImage, il monte temporairement ce système de fichiers et exécute l'application dans cet environnement isolé.

## Créer un AppImage pour votre application Lazarus

### Méthode 1 : Utiliser linuxdeploy (Recommandé)

#### Étape 1 : Préparer votre application

```bash
# Compiler votre application en mode Release
lazbuild --build-mode=Release monprojet.lpi

# Créer un dossier de travail
mkdir -p AppDir/usr/bin  
mkdir -p AppDir/usr/share/applications  
mkdir -p AppDir/usr/share/icons/hicolor/256x256/apps

# Copier l'exécutable
cp monappli AppDir/usr/bin/
```

#### Étape 2 : Créer le fichier .desktop

Créez `AppDir/usr/share/applications/monappli.desktop` :

```desktop
[Desktop Entry]
Type=Application  
Name=Mon Appli  
Comment=Ma super application FreePascal/Lazarus  
Exec=monappli  
Icon=monappli  
Categories=Utility;  
Terminal=false
```

#### Étape 3 : Ajouter l'icône

```bash
# Copier votre icône (256x256 pixels recommandé)
cp monappli.png AppDir/usr/share/icons/hicolor/256x256/apps/
```

#### Étape 4 : Télécharger linuxdeploy

```bash
# Télécharger linuxdeploy
wget https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage  
chmod +x linuxdeploy-x86_64.AppImage

# Télécharger le plugin GTK (si votre app utilise GTK)
wget https://raw.githubusercontent.com/linuxdeploy/linuxdeploy-plugin-gtk/master/linuxdeploy-plugin-gtk.sh  
chmod +x linuxdeploy-plugin-gtk.sh
```

#### Étape 5 : Créer l'AppImage

```bash
# Variables d'environnement pour le plugin GTK
export DEPLOY_GTK_VERSION=2  # ou 3 selon votre app

# Créer l'AppImage
./linuxdeploy-x86_64.AppImage \
  --appdir AppDir \
  --plugin gtk \
  --output appimage \
  --executable AppDir/usr/bin/monappli \
  --desktop-file AppDir/usr/share/applications/monappli.desktop \
  --icon-file AppDir/usr/share/icons/hicolor/256x256/apps/monappli.png

# Résultat : MonAppli-x86_64.AppImage
```

### Méthode 2 : Utiliser appimagetool directement

#### Étape 1 : Créer la structure AppDir complète

```bash
# Structure complète
mkdir -p AppDir  
cd AppDir

# Créer le script AppRun
cat > AppRun << 'EOF'
#!/bin/bash
HERE="$(dirname "$(readlink -f "${0}")")"  
export LD_LIBRARY_PATH="${HERE}/usr/lib:${LD_LIBRARY_PATH}"  
export PATH="${HERE}/usr/bin:${PATH}"  
exec "${HERE}/usr/bin/monappli" "$@"  
EOF

chmod +x AppRun

# Copier l'application et les bibliothèques
mkdir -p usr/bin usr/lib  
cp ../monappli usr/bin/

# Copier les bibliothèques nécessaires
# D'abord, identifier les dépendances
ldd ../monappli
```

#### Étape 2 : Copier les bibliothèques dépendantes

```bash
# Script pour copier automatiquement les dépendances
#!/bin/bash
copy_deps() {
    local binary=$1
    local dest=$2

    ldd "$binary" | grep "=> /" | awk '{print $3}' | while read lib; do
        if [ -f "$lib" ]; then
            cp -n "$lib" "$dest"
            # Copier récursivement les dépendances de la lib
            copy_deps "$lib" "$dest"
        fi
    done
}

copy_deps usr/bin/monappli usr/lib/
```

#### Étape 3 : Exclure les bibliothèques système

**Important** : Certaines bibliothèques système ne doivent PAS être incluses :

```bash
# Bibliothèques à NE PAS inclure :
# - libc.so.*
# - libdl.so.*
# - libpthread.so.*
# - libGL.so.*
# - libasound.so.*
# - libX11.so.* (et autres libs X11 de base)

# Supprimer ces libs si copiées
rm -f usr/lib/libc.so.* usr/lib/libdl.so.* usr/lib/libpthread.so.*
```

#### Étape 4 : Créer l'AppImage

```bash
# Télécharger appimagetool
wget https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage  
chmod +x appimagetool-x86_64.AppImage

# Créer l'AppImage
cd ..
./appimagetool-x86_64.AppImage AppDir MonAppli-x86_64.AppImage
```

### Script de build automatisé pour AppImage

Créez `build-appimage.sh` :

```bash
#!/bin/bash

APP_NAME="monappli"  
APP_VERSION="1.0.0"  
ARCH="x86_64"

# Nettoyer
rm -rf AppDir  
rm -f ${APP_NAME}-${ARCH}.AppImage

# Compiler l'application
lazbuild --build-mode=Release src/${APP_NAME}.lpi || exit 1

# Créer la structure
mkdir -p AppDir/usr/{bin,lib}  
mkdir -p AppDir/usr/share/{applications,icons/hicolor/256x256/apps}

# Copier les fichiers
cp src/${APP_NAME} AppDir/usr/bin/  
cp resources/${APP_NAME}.png AppDir/usr/share/icons/hicolor/256x256/apps/  
cp resources/${APP_NAME}.desktop AppDir/usr/share/applications/

# Télécharger linuxdeploy si nécessaire
if [ ! -f linuxdeploy-${ARCH}.AppImage ]; then
    wget -q https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-${ARCH}.AppImage
    chmod +x linuxdeploy-${ARCH}.AppImage
fi

# Créer l'AppImage
./linuxdeploy-${ARCH}.AppImage \
    --appdir AppDir \
    --output appimage \
    --executable AppDir/usr/bin/${APP_NAME} \
    --desktop-file AppDir/usr/share/applications/${APP_NAME}.desktop \
    --icon-file AppDir/usr/share/icons/hicolor/256x256/apps/${APP_NAME}.png

# Renommer avec version
mv ${APP_NAME}*.AppImage ${APP_NAME}-${APP_VERSION}-${ARCH}.AppImage

echo "AppImage créé : ${APP_NAME}-${APP_VERSION}-${ARCH}.AppImage"
```

### Mises à jour automatiques avec AppImageUpdate

Pour permettre les mises à jour automatiques :

1. Ajoutez les informations de mise à jour dans l'AppImage :

```bash
# Dans le fichier .desktop, ajoutez :
X-AppImage-UpdateInformation=gh-releases-zsync|votre-username|votre-repo|latest|MonAppli-*x86_64.AppImage.zsync
```

2. Générez le fichier zsync lors de la création :

```bash
# Installer zsync
sudo apt install zsync

# Après création de l'AppImage
zsyncmake MonAppli-x86_64.AppImage
```

3. L'utilisateur peut mettre à jour avec :

```bash
# Télécharger AppImageUpdate
wget https://github.com/AppImage/AppImageUpdate/releases/download/continuous/AppImageUpdate-x86_64.AppImage  
chmod +x AppImageUpdate-x86_64.AppImage

# Mettre à jour
./AppImageUpdate-x86_64.AppImage MonAppli-x86_64.AppImage
```

## Flatpak : L'application sandboxée moderne

### Qu'est-ce que Flatpak ?

Flatpak est un système de distribution d'applications qui offre :
- **Isolation** : Applications dans un bac à sable (sandbox)
- **Portabilité** : Fonctionne sur toutes les distributions
- **Sécurité** : Permissions contrôlées
- **Mises à jour** : Automatiques et incrémentales
- **Runtimes partagés** : Économise de l'espace disque

### Architecture Flatpak

```
Application Flatpak
    ├── Votre application
    ├── Runtime (bibliothèques partagées)
    └── Sandbox (isolation)
        ├── Système de fichiers limité
        ├── Réseau (si autorisé)
        └── Accès matériel contrôlé
```

## Créer un Flatpak pour votre application Lazarus

### Prérequis

```bash
# Installer Flatpak et les outils de build
sudo apt install flatpak flatpak-builder

# Ajouter le dépôt Flathub
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

# Installer le SDK FreePascal
flatpak install flathub org.freedesktop.Platform//23.08  
flatpak install flathub org.freedesktop.Sdk//23.08
```

### Étape 1 : Créer le manifeste Flatpak

Créez `com.example.MonAppli.yml` :

```yaml
app-id: com.example.MonAppli  
runtime: org.freedesktop.Platform  
runtime-version: '23.08'  
sdk: org.freedesktop.Sdk  
command: monappli

finish-args:
  # Permissions
  - --share=network              # Accès réseau
  - --socket=x11                  # Interface graphique X11
  - --socket=wayland             # Support Wayland
  - --filesystem=home            # Accès au dossier home
  - --device=dri                 # Accélération graphique
  - --socket=pulseaudio          # Son

modules:
  # Module FreePascal Compiler
  - name: fpc
    buildsystem: simple
    sources:
      - type: archive
        url: https://sourceforge.net/projects/freepascal/files/Linux/3.2.2/fpc-3.2.2-x86_64-linux.tar
        sha256: [hash_du_fichier]
    build-commands:
      - ./install.sh -d /app

  # Module Lazarus (si nécessaire pour la compilation)
  - name: lazarus
    buildsystem: simple
    sources:
      - type: archive
        url: https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64/Lazarus%203.0/lazarus-3.0-0.x86_64.rpm
        sha256: [hash_du_fichier]
    build-commands:
      - rpm2cpio lazarus*.rpm | cpio -idmv
      - cp -r usr/* /app/

  # Votre application
  - name: monappli
    buildsystem: simple
    sources:
      - type: git
        url: https://github.com/votre-username/monappli.git
        tag: v1.0.0

      # Ou depuis une archive locale
      # - type: archive
      #   path: ./monappli-src.tar.gz

    build-commands:
      # Compiler avec lazbuild
      - lazbuild --build-mode=Release src/monprojet.lpi

      # Installer l'application
      - install -Dm755 src/monappli /app/bin/monappli

      # Installer l'icône
      - install -Dm644 resources/monappli.png /app/share/icons/hicolor/256x256/apps/com.example.MonAppli.png

      # Installer le fichier desktop
      - install -Dm644 resources/monappli.desktop /app/share/applications/com.example.MonAppli.desktop

      # Installer les métadonnées
      - install -Dm644 resources/monappli.metainfo.xml /app/share/metainfo/com.example.MonAppli.metainfo.xml
```

### Étape 2 : Créer le fichier metainfo.xml

Créez `resources/monappli.metainfo.xml` :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<component type="desktop-application">
  <id>com.example.MonAppli</id>
  <metadata_license>CC0-1.0</metadata_license>
  <project_license>GPL-3.0+</project_license>
  <name>Mon Appli</name>
  <summary>Ma super application FreePascal/Lazarus</summary>

  <description>
    <p>
      Description détaillée de votre application.
      Elle peut être sur plusieurs paragraphes.
    </p>
    <p>
      Fonctionnalités principales :
    </p>
    <ul>
      <li>Fonctionnalité 1</li>
      <li>Fonctionnalité 2</li>
      <li>Fonctionnalité 3</li>
    </ul>
  </description>

  <launchable type="desktop-id">com.example.MonAppli.desktop</launchable>

  <screenshots>
    <screenshot type="default">
      <image>https://example.com/screenshot1.png</image>
      <caption>Fenêtre principale</caption>
    </screenshot>
  </screenshots>

  <url type="homepage">https://example.com</url>
  <url type="bugtracker">https://github.com/username/monappli/issues</url>

  <provides>
    <binary>monappli</binary>
  </provides>

  <releases>
    <release version="1.0.0" date="2024-01-01">
      <description>
        <p>Première version stable</p>
      </description>
    </release>
  </releases>

  <content_rating type="oars-1.1" />
</component>
```

### Étape 3 : Adapter le fichier .desktop

Modifiez votre `monappli.desktop` pour Flatpak :

```desktop
[Desktop Entry]
Type=Application  
Name=Mon Appli  
Comment=Ma super application FreePascal/Lazarus  
Exec=monappli  
Icon=com.example.MonAppli  
Categories=Utility;Development;  
Terminal=false  
StartupNotify=true
```

### Étape 4 : Construire le Flatpak

```bash
# Construire l'application
flatpak-builder --force-clean build-dir com.example.MonAppli.yml

# Tester localement
flatpak-builder --user --install --force-clean build-dir com.example.MonAppli.yml  
flatpak run com.example.MonAppli

# Créer un dépôt local
flatpak-builder --repo=repo --force-clean build-dir com.example.MonAppli.yml

# Créer un bundle pour distribution
flatpak build-bundle repo monappli.flatpak com.example.MonAppli
```

### Permissions Flatpak détaillées

Les permissions contrôlent ce que votre application peut faire :

```yaml
finish-args:
  # Graphique
  - --socket=x11                    # X11 (classique)
  - --socket=wayland                # Wayland (moderne)
  - --device=dri                    # Accélération GPU

  # Audio
  - --socket=pulseaudio            # Son via PulseAudio

  # Réseau
  - --share=network                # Accès réseau complet

  # Système de fichiers
  - --filesystem=home              # Tout le dossier home
  - --filesystem=home:ro           # Home en lecture seule
  - --filesystem=~/Documents       # Dossier spécifique
  - --filesystem=host              # Tout le système (déconseillé)

  # Périphériques
  - --device=all                   # Tous les périphériques
  - --device=kvm                   # Virtualisation

  # Services système
  - --socket=system-bus            # D-Bus système
  - --socket=session-bus           # D-Bus session

  # Variables d'environnement
  - --env=VARIABLE=valeur
```

### Script de build automatisé pour Flatpak

Créez `build-flatpak.sh` :

```bash
#!/bin/bash

APP_ID="com.example.MonAppli"  
MANIFEST="${APP_ID}.yml"

# Nettoyer
rm -rf build-dir repo *.flatpak

echo "Construction du Flatpak..."

# Construire
flatpak-builder --force-clean build-dir ${MANIFEST}

if [ $? -eq 0 ]; then
    echo "Build réussi !"

    # Créer le dépôt
    flatpak-builder --repo=repo --force-clean build-dir ${MANIFEST}

    # Créer le bundle
    flatpak build-bundle repo ${APP_ID}.flatpak ${APP_ID}

    echo "Flatpak créé : ${APP_ID}.flatpak"

    # Proposer l'installation locale
    read -p "Installer localement pour tester ? (o/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Oo]$ ]]; then
        flatpak-builder --user --install --force-clean build-dir ${MANIFEST}
        echo "Lancez avec : flatpak run ${APP_ID}"
    fi
else
    echo "Erreur lors du build !"
    exit 1
fi
```

## Publication et distribution

### Distribution d'AppImage

#### Via GitHub Releases

1. Créez une release sur GitHub
2. Uploadez votre AppImage
3. Ajoutez le fichier .zsync pour les mises à jour

#### Via votre site web

```html
<!-- Lien de téléchargement simple -->
<a href="MonAppli-1.0.0-x86_64.AppImage" download>
  Télécharger MonAppli (Linux, 64-bit)
</a>

<!-- Avec détection d'architecture -->
<script>
function downloadAppImage() {
  let arch = 'x86_64'; // Par défaut
  if (navigator.userAgent.includes('aarch64')) {
    arch = 'aarch64';
  }
  window.location.href = `MonAppli-1.0.0-${arch}.AppImage`;
}
</script>
```

#### AppImageHub

Soumettez votre AppImage sur https://appimage.github.io/ :

1. Fork le dépôt
2. Ajoutez votre application dans `data/`
3. Créez une Pull Request

### Distribution de Flatpak

#### Via Flathub (Recommandé)

Flathub est le store principal pour Flatpak :

1. **Préparez votre application** selon les guidelines Flathub
2. **Soumettez** sur https://github.com/flathub/flathub/
3. **Maintenez** votre manifeste dans le dépôt Flathub

#### Dépôt personnel

```bash
# Créer un dépôt
ostree init --repo=monappli-repo --mode=archive

# Ajouter l'application
flatpak-builder --repo=monappli-repo --force-clean build-dir com.example.MonAppli.yml

# Servir le dépôt (exemple avec Python)
cd monappli-repo  
python3 -m http.server 8080
```

Les utilisateurs peuvent ajouter votre dépôt :

```bash
flatpak remote-add --user monappli-repo http://votre-serveur.com:8080/monappli-repo  
flatpak install --user monappli-repo com.example.MonAppli
```

## Comparaison détaillée : AppImage vs Flatpak

### Quand choisir AppImage ?

**Utilisez AppImage si :**
- Vous voulez la **simplicité absolue** pour l'utilisateur
- L'application doit être **portable** (clé USB)
- Vous voulez distribuer **plusieurs versions**
- L'utilisateur n'a **pas les droits admin**
- Vous privilégiez la **compatibilité maximale**

**Cas d'usage idéaux :**
- Applications portables
- Outils de développement
- Versions beta/test
- Applications d'entreprise sur postes verrouillés

### Quand choisir Flatpak ?

**Utilisez Flatpak si :**
- La **sécurité** est importante (sandbox)
- Vous voulez des **mises à jour automatiques**
- L'application nécessite des **permissions spécifiques**
- Vous ciblez les **distributions modernes**
- Vous voulez être sur **Flathub**

**Cas d'usage idéaux :**
- Applications grand public
- Applications nécessitant de la sécurité
- Applications avec mises à jour fréquentes
- Applications complexes avec nombreuses dépendances

### Tableau comparatif technique

| Critère | AppImage | Flatpak |
|---------|----------|---------|
| **Taille typique** | 30-150 MB | 5-50 MB (+runtime partagé) |
| **Temps de démarrage** | Rapide | Légèrement plus lent |
| **RAM utilisée** | Normal | +10-20 MB (sandbox) |
| **Intégration desktop** | Manuelle | Automatique |
| **Thèmes GTK/Qt** | Suit le système | Peut différer |
| **Accès fichiers** | Complet | Contrôlé |
| **Support Wayland** | Variable | Excellent |

## Conseils et bonnes pratiques

### Pour les deux formats

1. **Testez sur plusieurs distributions**
   - Ubuntu LTS (20.04, 22.04)
   - Fedora dernière version
   - Debian stable
   - openSUSE Leap

2. **Optimisez la taille**
   - Compilez en Release avec optimisations
   - Strippez les symboles de debug
   - Compressez les ressources

3. **Documentez clairement**
   - Instructions d'installation
   - Permissions nécessaires
   - Configuration requise

### Pour AppImage spécifiquement

```bash
# Stripper les binaires pour réduire la taille
strip AppDir/usr/bin/monappli  
strip AppDir/usr/lib/*.so*

# Utiliser UPX pour compression (optionnel, peut causer des problèmes)
# upx --best AppDir/usr/bin/monappli
```

### Pour Flatpak spécifiquement

```yaml
# Optimisations dans le manifeste
build-options:
  cflags: "-O2"
  cxxflags: "-O2"
  strip: true
  no-debuginfo: true
```

## Dépannage courant

### Problèmes AppImage

**"Permission denied"**
```bash
chmod +x MonAppli.AppImage
```

**"FUSE is required"**
```bash
# Installer FUSE
sudo apt install fuse libfuse2

# Ou extraire et exécuter sans FUSE
./MonAppli.AppImage --appimage-extract
./squashfs-root/AppRun
```

**Bibliothèques manquantes**
```bash
# Vérifier les dépendances
./MonAppli.AppImage --appimage-mount
# Dans un autre terminal :
ldd /tmp/.mount_*/usr/bin/monappli
```

### Problèmes Flatpak

**"No remote refs found"**
```bash
# Mettre à jour les métadonnées
flatpak update --appstream
```

**Permissions insuffisantes**
```bash
# Modifier les permissions après installation
flatpak override --user --filesystem=home com.example.MonAppli
```

**Thème incorrect**
```bash
# Installer les thèmes Flatpak
flatpak install flathub org.gtk.Gtk3theme.Adwaita
```

## Scripts d'aide pour les utilisateurs

### Script d'installation AppImage

Créez `install-appimage.sh` pour vos utilisateurs :

```bash
#!/bin/bash

APPIMAGE_URL="https://github.com/user/repo/releases/latest/download/MonAppli.AppImage"  
INSTALL_DIR="$HOME/.local/bin"  
DESKTOP_DIR="$HOME/.local/share/applications"

# Créer les dossiers
mkdir -p "$INSTALL_DIR" "$DESKTOP_DIR"

# Télécharger
echo "Téléchargement de MonAppli..."  
wget -O "$INSTALL_DIR/MonAppli.AppImage" "$APPIMAGE_URL"  
chmod +x "$INSTALL_DIR/MonAppli.AppImage"

# Extraire l'icône et le .desktop
cd /tmp
"$INSTALL_DIR/MonAppli.AppImage" --appimage-extract *.desktop *.png

# Installer le .desktop
cp squashfs-root/*.desktop "$DESKTOP_DIR/"  
sed -i "s|Exec=.*|Exec=$INSTALL_DIR/MonAppli.AppImage|" "$DESKTOP_DIR"/*.desktop

# Nettoyer
rm -rf squashfs-root

echo "Installation terminée !"  
echo "Vous pouvez lancer MonAppli depuis votre menu d'applications"
```

### Script de vérification système

```bash
#!/bin/bash

echo "=== Vérification compatibilité AppImage/Flatpak ==="

# AppImage
echo -n "FUSE pour AppImage : "  
if command -v fusermount &> /dev/null; then
    echo "✓ Installé"
else
    echo "✗ Manquant (installer avec: sudo apt install fuse)"
fi

# Flatpak
echo -n "Flatpak : "  
if command -v flatpak &> /dev/null; then
    version=$(flatpak --version)
    echo "✓ Installé ($version)"
else
    echo "✗ Manquant (installer avec: sudo apt install flatpak)"
fi

# Architecture
echo "Architecture : $(uname -m)"

# Distribution
echo "Distribution : $(lsb_release -ds 2>/dev/null || echo "Inconnue")"
```

## Conclusion

AppImage et Flatpak résolvent le problème de la fragmentation Linux en offrant des formats de distribution universels. Chaque format a ses avantages :

- **AppImage** : Parfait pour la simplicité et la portabilité
- **Flatpak** : Idéal pour la sécurité et l'intégration moderne

Pour une application FreePascal/Lazarus, vous pouvez même proposer les deux formats ! Cela donne le choix à vos utilisateurs selon leurs préférences et besoins.

Le plus important est de tester votre application sur différentes distributions et de bien documenter le processus d'installation. Avec ces formats modernes, distribuer une application Linux devient aussi simple que sur Windows ou macOS !

⏭️ [Scripts Bash et intégration shell](/07-specificites-linux-ubuntu/09-scripts-bash-integration-shell.md)
