🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7. Spécificités Linux/Ubuntu

## Introduction au développement FreePascal/Lazarus sous Linux

Bienvenue dans ce chapitre dédié au développement d'applications FreePascal/Lazarus sous Linux, avec un focus particulier sur Ubuntu, l'une des distributions les plus populaires. Si vous venez du monde Windows ou si vous débutez avec Linux, ce chapitre vous guidera à travers les particularités de cet environnement.

## Pourquoi développer pour Linux/Ubuntu ?

### Un écosystème en pleine expansion

Linux n'est plus seulement l'OS des serveurs et des développeurs. Aujourd'hui, il équipe :
- Des millions de postes de travail professionnels
- La majorité des serveurs web dans le monde
- Les systèmes embarqués et IoT
- Les supercalculateurs
- Les smartphones (Android est basé sur Linux)

Ubuntu, en particulier, s'est imposé comme une distribution de choix grâce à sa facilité d'utilisation et son support commercial via Canonical.

### Les avantages pour le développeur FreePascal

1. **Gratuité totale** : Aucune licence à payer, ni pour l'OS ni pour les outils de développement
2. **Performance native** : FreePascal sous Linux produit des binaires extrêmement performants
3. **Stabilité** : Les applications peuvent tourner pendant des mois sans redémarrage
4. **Transparence** : Tout le système est open source et documenté
5. **Intégration système** : Accès direct aux fonctionnalités puissantes du noyau Linux

## Les différences fondamentales avec Windows

Avant de plonger dans les aspects techniques, comprenons les différences philosophiques et pratiques entre Linux et Windows.

### Philosophie du système

**Windows** suit un modèle commercial où Microsoft contrôle l'expérience utilisateur de manière centralisée. Les choix sont faits pour vous, ce qui simplifie certaines choses mais limite la personnalisation.

**Linux** suit la philosophie Unix : "faire une chose et la faire bien". Le système est modulaire, chaque composant peut être remplacé ou modifié. Cette liberté demande plus de connaissances mais offre un contrôle total.

### Architecture système

| Aspect | Windows | Linux/Ubuntu |
|--------|---------|--------------|
| Noyau | Monolithique fermé | Modulaire open source |
| Interface graphique | Intégrée (Windows Shell) | Séparée (X11/Wayland + DE) |
| Gestion des paquets | Installateurs individuels | Gestionnaire centralisé (apt) |
| Configuration | Registre + fichiers | Fichiers texte uniquement |
| Sécurité | UAC + Antivirus | Permissions + SELinux/AppArmor |
| Shell par défaut | CMD/PowerShell | Bash |

### L'environnement de bureau

Contrairement à Windows qui impose son interface, Linux offre le choix :

- **GNOME** (défaut Ubuntu) : Interface moderne et épurée
- **KDE Plasma** : Riche en fonctionnalités, personnalisable
- **XFCE** : Léger et rapide
- **Unity** : L'ancienne interface d'Ubuntu
- **Cinnamon**, **MATE**, **Budgie**... et bien d'autres

Votre application FreePascal/Lazarus devra s'adapter à ces différents environnements.

## Ce que vous devez savoir en tant que développeur

### Le terminal est votre ami

Sous Linux, le terminal (console) n'est pas une relique du passé mais un outil puissant quotidien. Vous l'utiliserez pour :

```bash
# Installer FreePascal et Lazarus
sudo apt update  
sudo apt install fpc lazarus

# Compiler en ligne de commande
fpc monprogramme.pas

# Gérer les permissions
chmod +x monprogramme

# Surveiller votre application
ps aux | grep monprogramme
```

### Tout est fichier

Linux traite presque tout comme des fichiers :
- Les périphériques : `/dev/sda` (disque dur), `/dev/ttyUSB0` (port série)
- Les processus : `/proc/1234/` (informations sur le processus 1234)
- La configuration système : `/etc/` (fichiers texte éditables)

Cette approche uniforme simplifie beaucoup de tâches de programmation.

### La sensibilité à la casse

```pascal
// Sous Windows, ces trois lignes référencent le même fichier :
LoadFromFile('Config.ini');  
LoadFromFile('config.ini');  
LoadFromFile('CONFIG.INI');

// Sous Linux, ce sont trois fichiers différents !
// Soyez cohérent dans vos noms de fichiers
```

### Les bibliothèques partagées

Windows utilise des DLL, Linux utilise des bibliothèques partagées (.so) :

```pascal
{$IFDEF WINDOWS}
  const LibraryName = 'mylib.dll';
{$ENDIF}
{$IFDEF UNIX}
  const LibraryName = 'libmylib.so';
{$ENDIF}
```

## Ubuntu : La distribution de référence

### Pourquoi Ubuntu ?

Ubuntu s'est imposé comme référence pour plusieurs raisons :
1. **LTS (Long Term Support)** : Versions stables supportées 5 ans
2. **Grande communauté** : Solutions et aide facilement disponibles
3. **PPA (Personal Package Archives)** : Installation facile de logiciels tiers
4. **Compatibilité** : Base Debian = énorme catalogue de paquets
5. **Documentation** : Excellente documentation officielle et communautaire

### Les versions d'Ubuntu

- **Ubuntu Desktop** : Version standard avec GNOME
- **Ubuntu Server** : Sans interface graphique
- **Kubuntu** : Avec KDE
- **Xubuntu** : Avec XFCE
- **Lubuntu** : Version légère avec LXQt

Votre application FreePascal fonctionnera sur toutes ces variantes.

### Le cycle de développement Ubuntu

Ubuntu sort une nouvelle version tous les 6 mois (avril et octobre) :
- **Versions LTS** : Tous les 2 ans (20.04, 22.04, 24.04...)
- **Versions intermédiaires** : Support 9 mois seulement

Pour le développement, privilégiez les versions LTS.

## Préparer votre environnement de développement

### Installation de base

```bash
# Mettre à jour le système
sudo apt update && sudo apt upgrade

# Installer les outils de développement essentiels
sudo apt install build-essential git

# Installer FreePascal et Lazarus
sudo apt install fpc lazarus

# Outils supplémentaires utiles
sudo apt install gdb valgrind  # Débogage et profiling  
sudo apt install sqlite3 postgresql-client  # Bases de données
```

### Structure de travail recommandée

```
~/Development/                 # Votre dossier de développement
├── Projects/                  # Vos projets
│   ├── MonApp/
│   │   ├── src/              # Code source
│   │   ├── bin/              # Binaires compilés
│   │   ├── lib/              # Bibliothèques
│   │   └── docs/             # Documentation
│   └── AutreProjet/
├── Libraries/                 # Bibliothèques tierces
└── Tools/                     # Outils et scripts
```

### Configuration de Lazarus pour Linux

Après l'installation, configurez Lazarus pour Linux :

1. **Vérifier les chemins du compilateur**
   - Outils → Options → Environnement → Fichiers
   - Vérifier que FPC pointe vers `/usr/bin/fpc`

2. **Configurer le débogueur**
   - Outils → Options → Débogueur
   - Sélectionner GNU debugger (gdb)

3. **Définir les widgetsets**
   - Projet → Options du projet → Ajouts et priorités
   - Choisir gtk2 ou qt5 selon vos besoins

## Les outils indispensables

### Outils système

- **htop** : Moniteur de processus interactif
- **mc** : Gestionnaire de fichiers en console
- **tree** : Visualiser l'arborescence des dossiers
- **lsof** : Voir les fichiers ouverts par les processus
- **strace** : Tracer les appels système

### Outils de développement

- **git** : Contrôle de version
- **make** : Automatisation de compilation
- **pkg-config** : Gestion des dépendances de compilation
- **ldd** : Vérifier les dépendances des binaires
- **objdump** : Analyser les binaires

### Éditeurs et IDE alternatifs

Bien que Lazarus soit notre IDE principal, connaître ces outils est utile :
- **VS Code** : Avec extensions Pascal
- **vim/neovim** : Éditeur en terminal
- **Kate** : Éditeur KDE
- **Geany** : IDE léger supportant Pascal

## Les pièges à éviter

### 1. Les chemins de fichiers

```pascal
// MAUVAIS - Chemin Windows
ConfigFile := 'C:\Program Files\MonApp\config.ini';

// BON - Chemin portable
ConfigFile := GetAppConfigDir(False) + 'config.ini';
```

### 2. Les fins de ligne

Windows utilise CRLF (`\r\n`), Linux utilise LF (`\n`) :

```pascal
// Gérer les deux formats
Text := StringReplace(Text, #13#10, #10, [rfReplaceAll]); // CRLF → LF  
Text := StringReplace(Text, #13, #10, [rfReplaceAll]);    // CR → LF
```

### 3. L'encodage des caractères

Linux utilise UTF-8 par défaut :

```pascal
// S'assurer de l'encodage UTF-8
{$codepage utf8}

// Ou utiliser les fonctions de conversion
UTF8Text := SysToUTF8(SystemText);
```

### 4. Les exécutables

Sous Linux, un fichier doit être marqué exécutable :

```pascal
// Après création d'un script ou binaire
fpChmod('monscript.sh', &755); // rwxr-xr-x
```

## Les avantages spécifiques à exploiter

### 1. Les signaux Unix

Linux permet une communication inter-processus élégante :

```pascal
uses
  BaseUnix;

procedure SignalHandler(sig: longint); cdecl;  
begin
  case sig of
    SIGTERM: // Demande d'arrêt propre
      begin
        CleanupApplication;
        Halt(0);
      end;
    SIGHUP: // Rechargement de configuration
      ReloadConfig;
  end;
end;

// Installation du gestionnaire
FpSignal(SIGTERM, @SignalHandler);  
FpSignal(SIGHUP, @SignalHandler);
```

### 2. Les pipes et redirection

```pascal
// Exécuter une commande et récupérer sa sortie
uses
  Process;

var
  Output: string;
begin
  RunCommand('/bin/ls', ['-la'], Output);
  // Output contient le résultat de ls -la
end;
```

### 3. La puissance du shell

```pascal
// Utiliser la puissance de bash
RunCommand('/bin/bash', ['-c', 'find . -name "*.pas" | wc -l'], Output);
// Compte tous les fichiers .pas récursivement
```

## Ce qui vous attend dans ce chapitre

Dans les sections suivantes, nous explorerons en détail :

- **7.1** Le système de fichiers et les permissions (base de la sécurité Linux)
- **7.2** Les services systemd (faire tourner vos applications en arrière-plan)
- **7.3** D-Bus (communication entre applications)
- **7.4** La configuration par fichiers texte (la méthode Unix)
- **7.5** L'intégration avec les environnements de bureau
- **7.6** Les standards Freedesktop.org
- **7.7** La création de paquets DEB
- **7.8** Les formats modernes AppImage et Flatpak
- **7.9** L'automatisation avec les scripts Bash
- **7.10** La gestion des processus et signaux
- **7.11** X11 et Wayland (systèmes graphiques)
- **7.12** Les politiques de sécurité SELinux/AppArmor

Chaque section vous donnera les clés pour créer des applications FreePascal/Lazarus qui s'intègrent parfaitement dans l'écosystème Linux/Ubuntu, en respectant ses conventions et en exploitant ses forces.

## Ressources pour approfondir

### Documentation officielle
- [Ubuntu Documentation](https://help.ubuntu.com/)
- [FreePascal Wiki - Linux](https://wiki.freepascal.org/Linux)
- [Lazarus Wiki - Linux Programming](https://wiki.lazarus.freepascal.org/Linux_Programming_Tips)

### Livres recommandés
- "The Linux Programming Interface" - Michael Kerrisk
- "Ubuntu Linux Toolbox" - Christopher Negus

### Communautés
- Forum Ubuntu : [ubuntuforums.org](https://ubuntuforums.org/)
- Forum FreePascal : [forum.lazarus.freepascal.org](https://forum.lazarus.freepascal.org/)
- Stack Overflow : Tags [freepascal] et [lazarus]

Prêt à découvrir la puissance du développement FreePascal sous Linux ? Commençons par comprendre le système de fichiers et les permissions, la base de tout dans l'univers Unix/Linux...

⏭️ [Système de fichiers Linux et permissions](/07-specificites-linux-ubuntu/01-systeme-fichiers-linux-permissions.md)
