🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.4 Installation multi-plateforme

## Introduction : Une installation, plusieurs philosophies

### Pourquoi l'installation est importante

L'installation d'un environnement de développement peut sembler être une étape triviale, mais elle révèle beaucoup sur la philosophie d'un outil. Avec FreePascal/Lazarus, l'installation reflète les valeurs fondamentales du projet :

- **Simplicité** : Pas de processus complexe de plusieurs heures
- **Transparence** : Vous savez exactement ce qui est installé
- **Flexibilité** : Plusieurs méthodes selon vos besoins
- **Portabilité** : Même outil sur tous les systèmes
- **Liberté** : Aucune activation ou enregistrement

### Ce que vous allez installer

Quand vous installez Lazarus, vous obtenez en réalité **trois composants majeurs** :

```
┌──────────────────────────────────────────┐
│         Installation Lazarus             │
├──────────────────────────────────────────┤
│                                          │
│  1. FPC (FreePascal Compiler)            │
│     • Compilateur en ligne de commande   │
│     • RTL (Runtime Library)              │
│     • FCL (Free Component Library)       │
│                                          │
│  2. Lazarus IDE                          │
│     • Environnement de développement     │
│     • Concepteur de formulaires          │
│     • Éditeur de code                    │
│                                          │
│  3. Sources et outils                    │
│     • Code source FPC (optionnel)        │
│     • Code source Lazarus                │
│     • Débogueur GDB                      │
│     • Utilitaires divers                 │
│                                          │
└──────────────────────────────────────────┘
```

### Tailles et ressources nécessaires

Avant de commencer, voici ce dont vous aurez besoin :

#### Espace disque requis

| Composant | Windows | Linux | macOS |
|-----------|---------|-------|-------|
| Installation minimale | 400 MB | 350 MB | 400 MB |
| Installation complète | 1.5 GB | 1.2 GB | 1.4 GB |
| Avec sources | 2.5 GB | 2.0 GB | 2.3 GB |
| Espace de travail recommandé | +2 GB | +2 GB | +2 GB |

#### Configuration système minimale

**Pour développer confortablement** :
- **Processeur** : Tout CPU x86/x64 des 15 dernières années
- **RAM** : 2 GB minimum, 4 GB recommandé, 8 GB confortable
- **Système** :
  - Windows : XP SP3 ou plus récent (7/8/10/11)
  - Linux : Kernel 2.6+ avec GTK2/Qt5
  - macOS : 10.10+ (Yosemite ou plus récent)

**Pour compiler des projets** :
- Les besoins dépendent de la taille de vos projets
- Un projet simple : quelques MB de RAM
- Un gros projet : plusieurs centaines de MB

### Les différentes approches d'installation

#### 1. Installation standard (recommandée pour débuter)

**Avantages** :
- ✅ Simple et rapide
- ✅ Configuration automatique
- ✅ Intégration système complète
- ✅ Mises à jour faciles

**Inconvénients** :
- ❌ Nécessite droits administrateur
- ❌ Une seule version à la fois (par défaut)
- ❌ Moins flexible

**Pour qui ?** : Débutants, développeurs solo, usage standard

#### 2. Gestionnaires de paquets

**Avantages** :
- ✅ Intégration avec l'écosystème OS
- ✅ Mises à jour automatiques
- ✅ Gestion des dépendances
- ✅ Commande simple

**Inconvénients** :
- ❌ Versions parfois anciennes
- ❌ Moins de contrôle
- ❌ Dépend du mainteneur

**Pour qui ?** : Administrateurs système, intégration continue

#### 3. Installation portable

**Avantages** :
- ✅ Aucun droit admin requis
- ✅ Plusieurs versions côte à côte
- ✅ USB/réseau possible
- ✅ Isolation complète

**Inconvénients** :
- ❌ Configuration manuelle
- ❌ Pas d'intégration système
- ❌ Plus technique

**Pour qui ?** : Développeurs avancés, environnements restreints

#### 4. Compilation depuis les sources

**Avantages** :
- ✅ Dernière version de développement
- ✅ Optimisations personnalisées
- ✅ Compréhension profonde
- ✅ Modifications possibles

**Inconvénients** :
- ❌ Processus long
- ❌ Expertise requise
- ❌ Maintenance manuelle

**Pour qui ?** : Contributeurs, experts, besoins spécifiques

### Organisation des répertoires après installation

Comprendre où les fichiers sont installés vous aidera pour la suite :

#### Structure typique sur Windows
```
C:\lazarus\                   # Répertoire principal
├── fpc\                      # Compilateur FreePascal
│   ├── 3.2.2\               # Version FPC
│   │   ├── bin\             # Exécutables (fpc.exe, etc.)
│   │   ├── units\           # Unités compilées
│   │   └── source\          # Sources FPC (optionnel)
│   └── ...
├── ide\                      # IDE Lazarus
│   ├── lazarus.exe          # Exécutable principal
│   ├── lazbuild.exe         # Outil ligne de commande
│   └── ...
├── components\               # Composants additionnels
├── docs\                     # Documentation
├── examples\                 # Exemples de code
└── tools\                    # Outils divers
```

#### Structure typique sur Linux
```
/usr/                         # Installation système
├── bin/
│   ├── fpc                  # Compilateur
│   ├── lazarus              # IDE
│   └── lazbuild             # Build tool
├── lib/
│   ├── fpc/                 # Bibliothèques FPC
│   └── lazarus/             # Bibliothèques Lazarus
└── share/
    ├── fpcsrc/              # Sources FPC
    └── lazarus/             # Ressources Lazarus

~/.lazarus/                   # Configuration utilisateur
├── onlinepackagemanager/    # Packages téléchargés
├── environmentoptions.xml   # Options IDE
└── ...
```

#### Structure typique sur macOS
```
/Applications/
└── Lazarus/                  # Bundle application
    └── Contents/
        ├── MacOS/           # Exécutables
        ├── Resources/       # Ressources
        └── ...

~/Library/Preferences/
└── lazarus/                  # Configuration utilisateur
```

### Préparer son environnement

#### Avant l'installation

**1. Vérifier l'architecture de votre système**

Il est crucial de télécharger la bonne version :
- **32 bits (i386)** : Pour vieux systèmes ou compatibilité maximale
- **64 bits (x86_64)** : Recommandé pour systèmes modernes
- **ARM** : Pour Raspberry Pi et certains Mac M1/M2

Comment vérifier :
- **Windows** : Système → À propos → Type du système
- **Linux** : `uname -m` dans le terminal
- **macOS** : Menu  → À propos de ce Mac

**2. Désinstaller les anciennes versions**

Si vous avez déjà une version installée :
- Sauvegarder vos projets
- Noter vos packages personnalisés
- Exporter vos configurations (optionnel)
- Désinstaller proprement

**3. Fermer les antivirus surprotecteurs**

Certains antivirus peuvent :
- Ralentir énormément la compilation
- Bloquer la création d'exécutables
- Quarantiner faussement vos programmes

Solution : Ajouter une exception pour le dossier Lazarus.

#### Choisir sa version

**Version stable (recommandée)**
- Numérotation : 2.2.x, 3.0.x (nombres pairs)
- Testée extensivement
- Bugs connus documentés
- Support communautaire optimal

**Version de développement**
- Numérotation : 2.3.x, 3.1.x (nombres impairs)
- Dernières fonctionnalités
- Potentiellement instable
- Pour tests et contribution

**Version FPC vs Version Lazarus**
- Les versions sont indépendantes
- Lazarus 3.0 peut utiliser FPC 3.2.2
- Compatibilité généralement ascendante
- Vérifier la matrice de compatibilité

### Les sources d'installation officielles

#### Site officiel Lazarus

**https://www.lazarus-ide.org**

C'est LA source de référence :
- Toujours les dernières versions stables
- Miroirs multiples pour téléchargement rapide
- Documentation officielle
- Liens vers les forums

#### SourceForge

**https://sourceforge.net/projects/lazarus/**

Hébergement historique :
- Archives de toutes les versions
- Statistiques de téléchargement
- Miroirs mondiaux
- Versions développement (snapshots)

#### GitLab

**https://gitlab.com/freepascal.org/**

Pour les développeurs :
- Code source en temps réel
- Branches de développement
- Issue tracker
- Merge requests

### Configuration multi-plateforme

#### Le concept de cross-compilation

FreePascal/Lazarus permet la **cross-compilation** : compiler pour une plateforme différente de celle où vous développez.

**Exemples pratiques** :
- Développer sur Windows → Compiler pour Linux
- Développer sur Linux → Compiler pour Windows
- Développer sur macOS Intel → Compiler pour macOS ARM

**Avantages** :
- Un seul environnement de développement
- Tests sur multiples plateformes
- Build automatisé centralisé
- Gain de temps énorme

**Ce qu'il faut installer** :
1. Le compilateur croisé (cross-compiler)
2. Les bibliothèques de la plateforme cible
3. Les outils de liaison (linker)
4. Éventuellement un émulateur pour tester

#### Stratégies de développement multi-plateforme

**1. Machine unique avec VMs**
```
Machine principale (ex: Windows)
├── Lazarus natif Windows
├── VM Linux (VirtualBox/VMware)
│   └── Lazarus Linux pour tests
└── VM macOS (si légal)
    └── Lazarus macOS pour tests
```

**Avantages** : Tout sur une machine  
**Inconvénients** : Ressources importantes nécessaires  

**2. Multi-boot**
```
Même machine physique
├── Partition Windows + Lazarus
├── Partition Linux + Lazarus
└── Données partagées
```

**Avantages** : Performance native  
**Inconvénients** : Redémarrage nécessaire  

**3. Machines multiples**
```
├── PC Windows (développement principal)
├── Machine Linux (tests et build)
└── Mac (build et tests macOS)
    Synchronisation via Git
```

**Avantages** : Vraie parallélisation  
**Inconvénients** : Coût matériel  

**4. Cross-compilation pure**
```
Machine unique
└── Lazarus avec cross-compilers
    ├── Win → Linux
    ├── Win → macOS
    └── Win → ARM
```

**Avantages** : Plus simple et rapide  
**Inconvénients** : Tests natifs impossibles  

### Considérations réseau et proxy

#### Installation derrière un proxy d'entreprise

Beaucoup d'entreprises utilisent des proxys qui peuvent compliquer l'installation :

**Problèmes courants** :
- Téléchargement des installateurs bloqué
- Online Package Manager inaccessible
- Mises à jour impossibles

**Solutions** :

1. **Configuration proxy système**
   - Windows : Paramètres Internet Explorer
   - Linux : Variables d'environnement
   - macOS : Préférences Réseau

2. **Configuration Lazarus**
   ```
   Outils → Options → Environnement → Proxy
   ```

3. **Téléchargement hors ligne**
   - Télécharger depuis une connexion non filtrée
   - Transférer via USB
   - Utiliser les versions portables

#### Installation hors ligne

Pour les environnements sans Internet :

**Préparer un kit d'installation complet** :
1. Installateur Lazarus
2. Sources FPC
3. Packages essentiels
4. Documentation
5. Exemples et tutoriels

**Créer un miroir local** :
- Repository de packages
- Documentation wiki offline
- Serveur de mise à jour interne

### Gestion des permissions et sécurité

#### Permissions nécessaires

**Windows** :
- Admin pour installation système
- Utilisateur pour version portable
- Exceptions antivirus/firewall

**Linux** :
- root/sudo pour installation système
- Utilisateur pour ~/.local
- Accès aux ports pour débogage

**macOS** :
- Admin pour /Applications
- Gatekeeper peut bloquer
- Signature développeur nécessaire

#### Bonnes pratiques de sécurité

1. **Vérifier les checksums**
   - MD5/SHA fournis sur le site
   - Garantit l'intégrité du téléchargement

2. **Sources officielles uniquement**
   - Éviter les sites tiers
   - Méfiance avec les "repacks"

3. **Isolation des projets**
   - Un répertoire par projet
   - Permissions appropriées
   - Backups réguliers

### Problèmes courants et solutions

#### Problèmes de téléchargement

**Symptôme** : Téléchargement lent ou interrompu

**Solutions** :
- Utiliser un gestionnaire de téléchargement
- Essayer différents miroirs
- Télécharger aux heures creuses
- Utiliser torrent si disponible

#### Conflits avec d'autres logiciels

**Delphi installé** :
- Peut causer confusion PATH
- Solutions : Versions portables ou VMs

**Autres IDE Pascal** :
- Dev-Pascal, Turbo Pascal
- Généralement pas de conflit
- Attention aux associations fichiers

**Antivirus** :
- Faux positifs fréquents
- Ajouter exceptions
- Désactiver pendant compilation

#### Problèmes d'espace disque

**Installation échoue** :
- Vérifier espace disponible
- Nettoyer fichiers temporaires
- Installer sur autre partition
- Version portable sur USB

### Optimisations post-installation

#### Configuration initiale recommandée

Après l'installation, quelques réglages améliorent l'expérience :

**1. Chemins et répertoires**
```
Projets/
├── Sources/          # Vos codes sources
├── Binaires/         # Exécutables compilés
├── Packages/         # Composants tiers
├── Backup/           # Sauvegardes auto
└── Temp/            # Fichiers temporaires
```

**2. Variables d'environnement utiles**
- `FPC_DIR` : Répertoire FPC
- `LAZARUS_DIR` : Répertoire Lazarus
- `PATH` : Ajouter bin FPC/Lazarus

**3. Configuration IDE de base**
- Thème visuel confortable
- Raccourcis clavier familiers
- Auto-save activé
- Backup automatique

#### Outils complémentaires recommandés

**Éditeur externe** :
- Visual Studio Code avec extension Pascal
- Notepad++ pour éditions rapides
- Vim/Emacs pour les puristes

**Gestion versions** :
- Git (obligatoire aujourd'hui)
- TortoiseGit/GitKraken (GUI)
- Configuration .gitignore Pascal

**Documentation** :
- Télécharger docs offline
- Installer CHM viewer
- Bookmarker wiki Lazarus

**Débogage avancé** :
- GDB dernière version
- Valgrind (Linux)
- Process Monitor (Windows)

### Stratégie de mise à jour

#### Quand mettre à jour

**Mise à jour recommandée** :
- Bugs critiques corrigés
- Nouvelles fonctionnalités nécessaires
- Support nouvelle plateforme
- Fin de vie version actuelle

**Mise à jour déconseillée** :
- En plein projet critique
- Version .0 (attendre .1 ou .2)
- Juste avant deadline
- Si tout fonctionne parfaitement

#### Comment mettre à jour

**Méthode prudente** :
1. Backup complet configuration
2. Installation parallèle nouvelle version
3. Tests sur projets non critiques
4. Migration progressive
5. Désinstallation ancienne version

**Méthode directe** :
1. Backup projets et config
2. Désinstallation ancienne
3. Installation nouvelle
4. Restauration config
5. Recompilation projets

### Préparation pour le développement multi-plateforme

#### Check-list avant de commencer

**Configuration de base validée** :
- [ ] Lazarus démarre correctement
- [ ] Compilation programme simple OK
- [ ] Débogueur fonctionne
- [ ] Packages de base installés

**Pour le cross-platform** :
- [ ] Comprendre les différences OS
- [ ] Identifier composants portables
- [ ] Planifier architecture code
- [ ] Préparer environnements test

**Documentation prête** :
- [ ] Aide Lazarus accessible
- [ ] Wiki en favoris
- [ ] Forums identifiés
- [ ] Exemples téléchargés

#### Philosophie d'installation

L'installation de Lazarus reflète sa philosophie :

**Simplicité** : Pas de processus alambiqué  
**Transparence** : Vous savez ce qui est installé où  
**Flexibilité** : Multiple méthodes possibles  
**Liberté** : Aucune activation ni limitation  

Cette approche contraste avec les IDE commerciaux qui nécessitent souvent :
- Création de compte
- Activation en ligne
- Télémétrie obligatoire
- Installations monolithiques

### Prochaines étapes

Une fois ces concepts compris, nous allons voir en détail :

1. **Installation Windows** : Les trois méthodes principales
2. **Installation Ubuntu** : Du package manager à la compilation
3. **Installation macOS** : Spécificités Apple
4. **Configuration avancée** : Optimisations et personnalisations

Chaque plateforme a ses particularités, mais la beauté de Lazarus est que **l'expérience de développement reste cohérente** quelle que soit votre plateforme.

---

**Rappel important** : Une bonne installation est la fondation d'une expérience de développement agréable. Prenez le temps de bien faire les choses, cela vous évitera des frustrations futures !

⏭️ [Installation sur Windows (installer, chocolatey, portable)](/01-introduction-freepascal-lazarus/04.1-installation-windows.md)
