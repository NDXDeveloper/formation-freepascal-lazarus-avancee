🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Paquets DEB et PPA pour applications FreePascal/Lazarus

## Introduction

Lorsque vous développez une application avec FreePascal/Lazarus sous Ubuntu/Linux, vous devez la distribuer à vos utilisateurs. Les paquets DEB et les PPA (Personal Package Archives) sont les méthodes standard pour distribuer des logiciels sur les systèmes Debian/Ubuntu. Ce tutoriel vous guidera pas à pas dans la création et la distribution de votre application.

## Qu'est-ce qu'un paquet DEB ?

Un paquet DEB est le format de fichier utilisé par Debian, Ubuntu et leurs dérivés pour installer des logiciels. C'est l'équivalent des fichiers `.exe` ou `.msi` sous Windows, mais en mieux organisé !

### Avantages des paquets DEB

- **Installation simple** : Un double-clic ou une commande suffit
- **Gestion des dépendances** : Le système installe automatiquement les bibliothèques nécessaires
- **Désinstallation propre** : Tous les fichiers sont supprimés correctement
- **Mises à jour automatiques** : Via le gestionnaire de paquets du système
- **Intégration système** : Menus, icônes, associations de fichiers

## Structure d'un paquet DEB

Un paquet DEB contient deux parties principales :

1. **Les fichiers de votre application** : L'exécutable, les ressources, etc.
2. **Les métadonnées** : Informations sur le paquet (nom, version, description, dépendances)

### Organisation des fichiers

```
monappli-deb/
├── DEBIAN/
│   ├── control         (informations du paquet)
│   ├── postinst        (script après installation - optionnel)
│   ├── prerm           (script avant désinstallation - optionnel)
│   └── postrm          (script après désinstallation - optionnel)
├── usr/
│   ├── bin/
│   │   └── monappli    (votre exécutable)
│   └── share/
│       ├── applications/
│       │   └── monappli.desktop    (entrée du menu)
│       ├── icons/
│       │   └── hicolor/
│       │       └── 48x48/
│       │           └── apps/
│       │               └── monappli.png
│       └── doc/
│           └── monappli/
│               └── copyright
```

## Création d'un paquet DEB pour votre application Lazarus

### Étape 1 : Préparer votre application

Compilez d'abord votre application en mode Release :

```bash
# Dans Lazarus : Menu Projet > Options du projet
# Mode de compilation : Release
# Ou en ligne de commande :
lazbuild --build-mode=Release monprojet.lpi
```

### Étape 2 : Créer la structure des dossiers

```bash
# Créer le dossier principal
mkdir -p monappli-1.0.0

# Créer la structure Debian
mkdir -p monappli-1.0.0/DEBIAN

# Créer les dossiers pour l'application
mkdir -p monappli-1.0.0/usr/bin  
mkdir -p monappli-1.0.0/usr/share/applications  
mkdir -p monappli-1.0.0/usr/share/icons/hicolor/48x48/apps  
mkdir -p monappli-1.0.0/usr/share/doc/monappli
```

### Étape 3 : Créer le fichier control

Le fichier `DEBIAN/control` est le cœur du paquet. Créez-le avec ce contenu :

```
Package: monappli  
Version: 1.0.0  
Section: utils  
Priority: optional  
Architecture: amd64  
Depends: libgtk2.0-0, libglib2.0-0, libcairo2  
Maintainer: Votre Nom <votre.email@example.com>  
Description: Ma super application FreePascal/Lazarus
 Une description plus longue de votre application.
 Elle peut être sur plusieurs lignes.
 Chaque ligne doit commencer par un espace.
Homepage: https://monsite.com
```

#### Explication des champs importants :

- **Package** : Le nom de votre paquet (lettres minuscules, chiffres, tirets)
- **Version** : Version de votre application
- **Architecture** : `amd64` pour 64-bit, `i386` pour 32-bit, `all` pour indépendant
- **Depends** : Les bibliothèques requises (très important !)

### Étape 4 : Identifier les dépendances

Pour trouver les dépendances de votre application :

```bash
# Vérifier les bibliothèques liées
ldd monappli

# Résultat exemple :
# libgtk-x11-2.0.so.0 => /usr/lib/x86_64-linux-gnu/libgtk-x11-2.0.so.0
# libglib-2.0.so.0 => /usr/lib/x86_64-linux-gnu/libglib-2.0.so.0
```

Pour trouver le paquet correspondant à une bibliothèque :

```bash
dpkg -S libgtk-x11-2.0.so.0
# Résultat : libgtk2.0-0:amd64: /usr/lib/x86_64-linux-gnu/libgtk-x11-2.0.so.0
```

### Étape 5 : Créer le fichier desktop

Le fichier `.desktop` permet d'ajouter votre application au menu. Créez `usr/share/applications/monappli.desktop` :

```desktop
[Desktop Entry]
Version=1.0  
Type=Application  
Name=Mon Appli  
Comment=Ma super application FreePascal/Lazarus  
Exec=/usr/bin/monappli  
Icon=monappli  
Categories=Utility;Development;  
Terminal=false  
StartupNotify=true
```

### Étape 6 : Copier les fichiers

```bash
# Copier l'exécutable
cp monappli monappli-1.0.0/usr/bin/

# Copier l'icône
cp monappli.png monappli-1.0.0/usr/share/icons/hicolor/48x48/apps/

# Créer le fichier copyright
echo "Copyright 2024 Votre Nom  
License: GPL-3.0+" > monappli-1.0.0/usr/share/doc/monappli/copyright
```

### Étape 7 : Définir les permissions

```bash
# L'exécutable doit être exécutable
chmod 755 monappli-1.0.0/usr/bin/monappli

# Les dossiers doivent avoir les bonnes permissions
find monappli-1.0.0 -type d -exec chmod 755 {} \;  
find monappli-1.0.0 -type f -exec chmod 644 {} \;  
chmod 755 monappli-1.0.0/usr/bin/monappli
```

### Étape 8 : Construire le paquet DEB

```bash
# Construire le paquet
dpkg-deb --build monappli-1.0.0

# Cela crée : monappli-1.0.0.deb
```

### Étape 9 : Vérifier le paquet

```bash
# Vérifier la structure
dpkg-deb --contents monappli-1.0.0.deb

# Vérifier les informations
dpkg-deb --info monappli-1.0.0.deb

# Tester avec lintian (outil de vérification)
sudo apt install lintian  
lintian monappli-1.0.0.deb
```

## Scripts de packaging automatisé

Pour simplifier le processus, créez un script `build-deb.sh` :

```bash
#!/bin/bash

APP_NAME="monappli"  
VERSION="1.0.0"  
ARCH="amd64"

# Nettoyer
rm -rf ${APP_NAME}_${VERSION}_${ARCH}  
rm -f ${APP_NAME}_${VERSION}_${ARCH}.deb

# Créer la structure
mkdir -p ${APP_NAME}_${VERSION}_${ARCH}/{DEBIAN,usr/bin,usr/share/applications}  
mkdir -p ${APP_NAME}_${VERSION}_${ARCH}/usr/share/icons/hicolor/48x48/apps  
mkdir -p ${APP_NAME}_${VERSION}_${ARCH}/usr/share/doc/${APP_NAME}

# Copier les fichiers
cp ${APP_NAME} ${APP_NAME}_${VERSION}_${ARCH}/usr/bin/  
cp ${APP_NAME}.desktop ${APP_NAME}_${VERSION}_${ARCH}/usr/share/applications/  
cp ${APP_NAME}.png ${APP_NAME}_${VERSION}_${ARCH}/usr/share/icons/hicolor/48x48/apps/

# Créer le fichier control
cat > ${APP_NAME}_${VERSION}_${ARCH}/DEBIAN/control << EOF  
Package: ${APP_NAME}  
Version: ${VERSION}  
Section: utils  
Priority: optional  
Architecture: ${ARCH}  
Depends: libgtk2.0-0, libglib2.0-0  
Maintainer: Votre Nom <email@example.com>  
Description: Description courte
 Description longue
EOF

# Permissions
chmod 755 ${APP_NAME}_${VERSION}_${ARCH}/usr/bin/${APP_NAME}

# Construire
dpkg-deb --build ${APP_NAME}_${VERSION}_${ARCH}

echo "Paquet créé : ${APP_NAME}_${VERSION}_${ARCH}.deb"
```

## Les PPA (Personal Package Archives)

Un PPA est un dépôt de paquets personnel hébergé sur Launchpad (service d'Ubuntu). Il permet de distribuer facilement votre application et ses mises à jour.

### Avantages des PPA

- **Mises à jour automatiques** : Les utilisateurs reçoivent les nouvelles versions
- **Hébergement gratuit** : Launchpad héberge vos paquets
- **Compilation automatique** : Pour différentes versions d'Ubuntu et architectures
- **Signature des paquets** : Sécurité renforcée

### Configuration initiale pour créer un PPA

#### 1. Créer un compte Launchpad

Rendez-vous sur https://launchpad.net et créez un compte gratuit.

#### 2. Générer une clé GPG

```bash
# Installer GPG si nécessaire
sudo apt install gnupg

# Générer une clé
gpg --gen-key

# Suivre les instructions :
# - Nom réel : Votre Nom
# - Email : votre.email@example.com
# - Passphrase : un mot de passe sécurisé
```

#### 3. Envoyer la clé à Launchpad

```bash
# Lister vos clés pour obtenir l'ID
gpg --list-keys
# Chercher la ligne : pub   rsa3072 2024-01-01 [SC] [expire: 2026-01-01]
#                          XXXXXXXXXXXXXXXX

# Envoyer la clé au serveur Ubuntu
gpg --keyserver keyserver.ubuntu.com --send-keys XXXXXXXXXXXXXXXX
```

Puis, sur Launchpad, allez dans votre profil et ajoutez votre empreinte GPG.

#### 4. Créer le PPA

Sur Launchpad :
1. Allez dans votre profil
2. Cliquez sur "Create a new PPA"
3. Donnez-lui un nom (ex: `monappli-stable`)
4. Ajoutez une description

### Préparer un paquet source pour PPA

Les PPA nécessitent des paquets sources, pas des .deb binaires.

#### Structure d'un paquet source

```
monappli-1.0.0/
├── debian/
│   ├── changelog
│   ├── compat
│   ├── control
│   ├── copyright
│   ├── rules
│   └── source/
│       └── format
└── src/
    └── (votre code source)
```

#### Fichier debian/changelog

```
monappli (1.0.0-1ubuntu1) focal; urgency=low

  * Version initiale
  * Ajout de la fonctionnalité X

 -- Votre Nom <email@example.com>  Mon, 01 Jan 2024 12:00:00 +0100
```

**Important** : Le nom de distribution (`focal` pour Ubuntu 20.04, `jammy` pour 22.04) doit correspondre à votre cible.

#### Fichier debian/rules

C'est un Makefile qui compile votre application :

```makefile
#!/usr/bin/make -f

%:
	dh $@

override_dh_auto_build:
	lazbuild --build-mode=Release src/monprojet.lpi

override_dh_auto_install:
	install -D -m 755 src/monprojet debian/monappli/usr/bin/monappli
	install -D -m 644 data/monappli.desktop debian/monappli/usr/share/applications/monappli.desktop
	install -D -m 644 data/monappli.png debian/monappli/usr/share/icons/hicolor/48x48/apps/monappli.png
```

#### Fichier debian/compat

```
10
```

#### Fichier debian/source/format

```
3.0 (quilt)
```

### Construire et uploader vers le PPA

#### 1. Installer les outils nécessaires

```bash
sudo apt install devscripts dput debhelper
```

#### 2. Construire le paquet source

```bash
# Dans le dossier de votre application
cd monappli-1.0.0

# Construire le paquet source
debuild -S -sa

# Cela crée plusieurs fichiers dans le dossier parent :
# - monappli_1.0.0-1ubuntu1.dsc
# - monappli_1.0.0-1ubuntu1_source.changes
# - monappli_1.0.0.orig.tar.gz
```

#### 3. Uploader vers le PPA

```bash
# Configurer dput (une seule fois)
echo "[monappli-ppa]  
fqdn = ppa.launchpad.net  
method = ftp  
incoming = ~votre-username/ubuntu/monappli-stable/  
login = anonymous  
allow_unsigned_uploads = 0" > ~/.dput.cf

# Uploader
dput monappli-ppa monappli_1.0.0-1ubuntu1_source.changes
```

### Utilisation du PPA par les utilisateurs

Une fois le paquet compilé sur Launchpad (environ 30 minutes), vos utilisateurs peuvent l'installer :

```bash
# Ajouter le PPA
sudo add-apt-repository ppa:votre-username/monappli-stable  
sudo apt update

# Installer l'application
sudo apt install monappli
```

## Bonnes pratiques

### 1. Versionnement

Utilisez le versionnement sémantique :
- **1.0.0** : Version majeure.mineure.patch
- **1.0.0-1** : Le `-1` indique la révision du paquet (pas de l'application)
- **1.0.0-1ubuntu1** : Spécifique à Ubuntu

### 2. Tests

Toujours tester votre paquet :

```bash
# Installation
sudo dpkg -i monappli_1.0.0_amd64.deb
# ou
sudo apt install ./monappli_1.0.0_amd64.deb

# Vérifier l'installation
dpkg -l | grep monappli

# Tester l'application
monappli

# Désinstallation
sudo apt remove monappli
```

### 3. Documentation

Incluez toujours :
- Un fichier README dans `/usr/share/doc/monappli/`
- Un changelog détaillé
- Les informations de licence

### 4. Compatibilité

Pour supporter plusieurs versions d'Ubuntu :
- Utilisez des dépendances génériques quand possible
- Testez sur différentes versions
- Créez des branches dans votre PPA pour différentes versions

## Outils utiles

### CheckInstall

Alternative rapide pour créer des .deb simples :

```bash
sudo apt install checkinstall

# Après compilation
sudo checkinstall --pkgname=monappli --pkgversion=1.0.0
```

### Debreate

Interface graphique pour créer des paquets DEB :

```bash
sudo apt install debreate  
debreate
```

### Launchpad Recipe

Pour automatiser la construction depuis Git/Bazaar :
1. Hébergez votre code sur Launchpad ou GitHub
2. Créez une "recipe" sur Launchpad
3. Les builds se font automatiquement à chaque commit

## Dépannage courant

### Problème : "Dépendance non satisfaite"

**Solution** : Vérifiez que toutes les bibliothèques sont listées dans `Depends:` du fichier control.

### Problème : "Permission denied" lors de l'exécution

**Solution** : Assurez-vous que l'exécutable a les permissions 755.

### Problème : "Paquet cassé" après installation

**Solution** : Utilisez `apt --fix-broken install` et vérifiez les scripts post/pre installation.

### Problème : Upload PPA échoue

**Solutions** :
- Vérifiez votre signature GPG
- Assurez-vous que la version n'existe pas déjà
- Vérifiez le nom de distribution (focal, jammy, etc.)

## Conclusion

La création de paquets DEB et l'utilisation de PPA permettent une distribution professionnelle de vos applications FreePascal/Lazarus. Bien que le processus initial puisse sembler complexe, une fois configuré, il devient un moyen très efficace de distribuer et maintenir vos applications sous Ubuntu/Debian.

Les utilisateurs apprécient particulièrement :
- L'installation en un clic
- Les mises à jour automatiques
- L'intégration parfaite avec le système

Avec un script de build automatisé et un PPA configuré, publier une nouvelle version devient aussi simple que de faire un commit et d'exécuter un script !

⏭️ [AppImage et Flatpak](/07-specificites-linux-ubuntu/08-appimage-flatpak.md)
