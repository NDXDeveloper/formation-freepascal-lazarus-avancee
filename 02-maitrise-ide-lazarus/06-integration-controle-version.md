🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.6 Intégration avec les systèmes de contrôle de version

## Introduction : Pourquoi le contrôle de version est indispensable

Imaginez que vous travaillez sur votre projet depuis des semaines. Un jour, vous faites une modification qui casse tout. Sans contrôle de version, vous êtes perdu. Avec un système de contrôle de version, vous pouvez revenir en arrière en quelques secondes.

Le contrôle de version, c'est comme avoir une machine à remonter le temps pour votre code. Mais c'est bien plus que ça :

**Avantages du contrôle de version :**
- 📅 **Historique complet** : Chaque modification est enregistrée
- 👥 **Travail en équipe** : Plusieurs développeurs sans conflits
- 🔄 **Branches** : Expérimentez sans risquer le code stable
- 💾 **Sauvegarde** : Votre code est en sécurité sur un serveur
- 📝 **Documentation** : Chaque changement est documenté
- 🐛 **Débogage** : Trouvez quand un bug a été introduit

## Les systèmes de contrôle de version supportés

### Vue d'ensemble

```
Systèmes supportés par Lazarus :
├── Git (Recommandé)
│   ├── Le plus populaire
│   ├── GitHub, GitLab, Bitbucket
│   └── Support excellent dans Lazarus
├── SVN (Subversion)
│   ├── Centralisé, simple
│   ├── SourceForge, Apache
│   └── Bon support Lazarus
├── Mercurial (Hg)
│   ├── Distribué comme Git
│   ├── Plus simple que Git
│   └── Support basique
└── CVS (Legacy)
    ├── Ancien, dépassé
    ├── Encore utilisé parfois
    └── Support minimal
```

### Git : Le standard moderne

**Git** est devenu le standard de l'industrie. Si vous ne savez pas lequel choisir, choisissez Git.

**Pourquoi Git ?**
- 🌍 **Universel** : Utilisé partout
- 🚀 **Rapide** : Opérations locales instantanées
- 🔧 **Puissant** : Branches, merge, rebase
- 🌐 **Plateformes** : GitHub, GitLab, Bitbucket
- 📚 **Ressources** : Documentation immense

### SVN : La simplicité centralisée

**SVN (Subversion)** est plus simple que Git mais moins flexible.

**Avantages de SVN :**
- 📚 **Plus simple** à apprendre
- 📁 **Numérotation** séquentielle des versions
- 🔒 **Verrouillage** de fichiers possible
- 📊 **Un seul dépôt** central

## Installation des outils

### Git

#### Sur Windows

```powershell
# Méthode 1 : Installateur officiel
# Télécharger depuis : https://git-scm.com/download/win
# Installer avec les options par défaut

# Méthode 2 : Chocolatey
choco install git

# Méthode 3 : winget
winget install --id Git.Git

# Vérification
git --version
# git version 2.42.0.windows.1
```

#### Sur Ubuntu/Linux

```bash
# Installation
sudo apt update  
sudo apt install git

# Configuration globale
git config --global user.name "Votre Nom"  
git config --global user.email "vous@email.com"

# Vérification
git --version
# git version 2.34.1
```

### SVN

#### Sur Windows

```powershell
# TortoiseSVN (Recommandé - Interface graphique)
# Télécharger : https://tortoisesvn.net/downloads.html

# Ou SlikSVN (Ligne de commande)
# Télécharger : https://www.sliksvn.com/download/

# Vérification
svn --version
```

#### Sur Ubuntu/Linux

```bash
# Installation
sudo apt install subversion

# Vérification
svn --version
# svn, version 1.14.1
```

## Configuration dans Lazarus

### Accéder aux options VCS

**Menu : Projet → Options du projet → Contrôle de version**

```
┌─ Options de contrôle de version ────────────────┐
│                                                 │
│ Système : [Git                            ▼]    │
│           ├── Git                               │
│           ├── SVN                               │
│           ├── Mercurial                         │
│           └── Aucun                             │
│                                                 │
│ ☑ Activer l'intégration VCS                    │
│ ☑ Afficher les icônes d'état dans l'éditeur    │
│ ☑ Auto-commit avant compilation                │
│                                                 │
│ [OK] [Annuler]                                  │
└─────────────────────────────────────────────────┘
```

### Configuration Git dans Lazarus

#### Chemin vers Git

**Outils → Options → Contrôle de version → Git**

```
Configuration Git :
├── Exécutable Git
│   ├── Windows : [C:\Program Files\Git\bin\git.exe]
│   └── Linux : [/usr/bin/git]
├── Options
│   ├── ☑ Afficher les modifications dans la marge
│   ├── ☑ Colorier les lignes modifiées
│   └── ☑ Auto-stage des fichiers modifiés
└── Interface
    ├── ☑ Utiliser l'interface intégrée
    └── ☐ Ouvrir GUI externe (GitKraken, SourceTree)
```

#### Configuration du projet pour Git

Dans le dossier de votre projet :

```bash
# Initialiser Git
git init

# Créer .gitignore pour Lazarus
cat > .gitignore << 'EOF'
# Lazarus / FreePascal
*.compiled
*.ppu
*.o
*.or
*.a
*.rsj
*.lps
*.bak*
backup/  
lib/
*.exe
*.dll
*.so
*.dylib

# Fichiers de configuration locaux
*.local
*.session

# Dossiers de build
bin/  
build/  
debug/  
release/

# IDE
.idea/
.vscode/
EOF

# Premier commit
git add .  
git commit -m "Initial commit - Projet Lazarus"
```

### Configuration SVN dans Lazarus

**Outils → Options → Contrôle de version → SVN**

```
Configuration SVN :
├── Exécutable SVN
│   ├── Windows : [C:\Program Files\TortoiseSVN\bin\svn.exe]
│   └── Linux : [/usr/bin/svn]
├── Options
│   ├── ☑ Verrouiller fichiers à l'édition
│   ├── ☑ Update avant commit
│   └── Timeout : [30] secondes
└── Authentification
    ├── ☐ Sauvegarder mot de passe
    └── ☑ Utiliser SSH key
```

## Utilisation depuis Lazarus

### Menu Contrôle de version

Une fois configuré, un nouveau menu apparaît :

**Menu : Outils → Contrôle de version** (ou clic droit dans l'éditeur)

```
Menu Contrôle de version :
├── 📥 Update/Pull (Ctrl+Alt+U)
├── 📤 Commit (Ctrl+Alt+C)
├── 📊 Diff (Ctrl+Alt+D)
├── 📜 Log/History (Ctrl+Alt+L)
├── 🔄 Revert (Ctrl+Alt+R)
├── 🌿 Branch (Ctrl+Alt+B)
├── 🔀 Merge
├── ➕ Add file
├── ❌ Ignore file
└── ⚙️ Settings
```

### Indicateurs visuels dans l'éditeur

Lazarus affiche l'état des lignes dans la marge :

```
Indicateurs de marge :
├── Vert    : Ligne ajoutée
├── Jaune   : Ligne modifiée
├── Rouge   : Ligne supprimée (vue diff)
├── Bleu    : Ligne dans un conflit
└── Gris    : Non versionné
```

**Exemple visuel :**
```pascal
  1 │   program MonProjet;
  2 │   uses SysUtils;
  3 │ + begin  // Ligne ajoutée (vert)
  4 │ ~ WriteLn('Modifié');  // Ligne modifiée (jaune)
  5 │   end.
```

### Fenêtre de commit

**Ctrl+Alt+C** ou **Outils → Contrôle de version → Commit**

```
┌─ Commit Changes ─────────────────────────────────┐
│ Message de commit :                              │
│ ┌───────────────────────────────────────────┐    │
│ │ Fix: Correction du bug de calcul          │    │
│ │                                            │   │
│ │ - Ajout de vérification des divisions     │    │
│ │ - Gestion des valeurs nulles              │    │
│ └───────────────────────────────────────────┘    │
│                                                  │
│ Fichiers modifiés :                              │
│ ☑ main.pas (M)
│ ☑ calculator.pas (M)
│ ☐ readme.md (M)
│ ☑ tests.pas (A)
│                                                  │
│ [Commit] [Commit & Push] [Annuler]               │
└──────────────────────────────────────────────────┘
```

### Fenêtre de diff

**Ctrl+Alt+D** pour voir les modifications :

```
┌─ Différences : calculator.pas ───────────────────┐
│ Ligne 45:                                        │
│ - Result := A / B;                               │
│ + if B <> 0 then                                 │
│ +   Result := A / B                              │
│ + else                                           │
│ +   raise Exception.Create('Division par zéro'); │
│                                                  │
│ [Appliquer] [Ignorer] [Fermer]                   │
└──────────────────────────────────────────────────┘
```

## Workflow Git dans Lazarus

### Workflow basique

```mermaid
Workflow Git simple :
1. Pull (récupérer les changements)
   ↓
2. Coder (faire vos modifications)
   ↓
3. Test (vérifier que ça marche)
   ↓
4. Commit (enregistrer localement)
   ↓
5. Push (envoyer au serveur)
```

### Créer et gérer des branches

#### Via l'interface Lazarus

**Outils → Contrôle de version → Branches**

```
┌─ Gestion des branches ───────────────────────────┐
│ Branches locales :                               │
│ ● main                                           │
│ ○ feature/nouveau-module                         │
│ ○ bugfix/correction-calcul                       │
│                                                  │
│ [Nouvelle] [Checkout] [Merge] [Supprimer]        │
│                                                  │
│ Nouvelle branche :                               │
│ Nom : [feature/___________]                      │
│ Basée sur : [main        ▼]                      │
│                                                  │
│ [Créer et basculer] [Annuler]                    │
└──────────────────────────────────────────────────┘
```

#### Via le terminal intégré

Lazarus peut ouvrir un terminal :

**Affichage → Fenêtres → Terminal**

```bash
# Créer une branche
git checkout -b feature/nouvelle-fonction

# Voir les branches
git branch -a

# Changer de branche
git checkout main

# Fusionner
git merge feature/nouvelle-fonction
```

### Résolution de conflits

Quand Git ne peut pas fusionner automatiquement :

```pascal
// Fichier avec conflit
procedure Calculate;  
begin
<<<<<<< HEAD
  Result := OldMethod;
=======
  Result := NewMethod;
>>>>>>> feature/nouveau-calcul
end;
```

**Résolution dans Lazarus :**

```
┌─ Résolution de conflit ───────────────────────────┐
│ Fichier : calculator.pas                          │
│                                                   │
│ Version locale (HEAD) :     │ Version distante :  │
│ Result := OldMethod;        │ Result := NewMethod;│
│                             │                     │
│ Version fusionnée :                               │
│ [Result := NewMethod;                      ]      │
│                                                   │
│ [Utiliser local] [Utiliser distant] [Appliquer]   │
└───────────────────────────────────────────────────┘
```

## Bonnes pratiques pour projets Lazarus

### Structure de projet recommandée

```
MonProjetLazarus/
├── .git/                 # Dossier Git
├── .gitignore           # Fichiers à ignorer
├── README.md            # Documentation
├── LICENSE              # Licence
├── projet.lpi           # Projet Lazarus (versionné)
├── projet.lpr           # Programme principal (versionné)
├── projet.lps           # Session (NON versionné)
├── src/                 # Sources
│   ├── main.pas
│   ├── forms/
│   │   └── mainform.pas
│   └── units/
│       └── utils.pas
├── res/                 # Ressources
│   ├── images/
│   └── icons/
├── docs/                # Documentation
├── tests/               # Tests unitaires
└── bin/                 # Exécutables (NON versionnés)
```

### Fichier .gitignore optimal

```gitignore
# === Lazarus / FreePascal ===
# Unités compilées
*.ppu
*.o
*.or
*.compiled

# Bibliothèques
*.a
*.so
*.dll
*.dylib

# Exécutables
*.exe
projet

# Fichiers de backup
*.bak
*~
backup/

# Fichiers de session locale
*.lps
*.local
*.session

# Dossiers de compilation
lib/  
units/  
bin/  
build/

# === OS Specific ===
# Windows
Thumbs.db  
Desktop.ini

# macOS
.DS_Store

# Linux
*~

# === IDE ===
.idea/
.vscode/
*.sublime-*
```

### Messages de commit

**Format recommandé :**

```
Type: Description courte (max 50 caractères)

Description détaillée si nécessaire.  
Expliquez POURQUOI, pas comment.

Fixes #123  // Référence au ticket
```

**Types de commits :**
```
feat:     Nouvelle fonctionnalité  
fix:      Correction de bug  
docs:     Documentation  
style:    Formatage (pas de changement de code)  
refactor: Refactoring du code  
test:     Ajout de tests  
chore:    Maintenance, config  
perf:     Amélioration des performances
```

**Exemples :**
```
feat: Ajout du module d'export PDF

fix: Correction division par zéro dans Calculator

docs: Mise à jour README avec instructions Ubuntu

refactor: Simplification de la classe TDataManager
```

## Intégration avec plateformes

### GitHub

#### Configuration SSH

```bash
# Générer une clé SSH
ssh-keygen -t ed25519 -C "votre@email.com"

# Windows : Ajouter à l'agent SSH
ssh-add ~/.ssh/id_ed25519

# Linux : Démarrer l'agent si nécessaire
eval "$(ssh-agent -s)"  
ssh-add ~/.ssh/id_ed25519

# Copier la clé publique
cat ~/.ssh/id_ed25519.pub
# Ajouter dans GitHub Settings → SSH Keys
```

#### Clone et push

```bash
# Cloner un projet
git clone git@github.com:username/projet-lazarus.git

# Ajouter un remote
git remote add origin git@github.com:username/projet.git

# Premier push
git push -u origin main
```

### GitLab

Configuration similaire à GitHub :

```bash
# GitLab utilise le même système SSH
git clone git@gitlab.com:username/projet.git

# CI/CD avec .gitlab-ci.yml
```

Exemple `.gitlab-ci.yml` pour Lazarus :

```yaml
image: ubuntu:latest

before_script:
  - apt-get update -qq
  - apt-get install -y fp-compiler lazarus

build:
  script:
    - lazbuild projet.lpi
  artifacts:
    paths:
      - bin/
```

### Bitbucket

```bash
# Clone HTTPS (plus simple)
git clone https://username@bitbucket.org/team/projet.git

# Ou SSH
git clone git@bitbucket.org:team/projet.git
```

## Outils graphiques complémentaires

### Interfaces graphiques Git

#### Windows

```
Outils GUI pour Windows :
├── TortoiseGit
│   ├── Intégration Explorer
│   ├── Interface simple
│   └── Gratuit
├── SourceTree
│   ├── Atlassian (Bitbucket)
│   ├── Complet
│   └── Gratuit
├── GitKraken
│   ├── Moderne, beau
│   ├── Cross-platform
│   └── Freemium
└── GitHub Desktop
    ├── Simple
    ├── GitHub focused
    └── Gratuit
```

#### Linux

```
Outils GUI pour Linux :
├── GitKraken
│   └── Le même que Windows
├── SmartGit
│   └── Complet, commercial
├── gitg
│   └── Simple, GTK
├── GitCola
│   └── Qt, léger
└── Lazarus Git GUI
    └── Package OPM
```

### Configuration avec TortoiseGit (Windows)

1. **Installer TortoiseGit**
2. **Clic droit dans l'explorateur** → Menu TortoiseGit
3. **Configurer dans Lazarus** :

```
Outils → Options → Contrôle de version
├── ☑ Utiliser outil externe
└── Chemin : [C:\Program Files\TortoiseGit\bin\TortoiseGitProc.exe]
```

## Automatisation et hooks

### Git hooks pour Lazarus

Créez `.git/hooks/pre-commit` :

```bash
#!/bin/bash
# Compile le projet avant commit

echo "Compilation du projet..."  
lazbuild projet.lpi

if [ $? -ne 0 ]; then
    echo "Erreur de compilation ! Commit annulé."
    exit 1
fi

echo "Compilation OK, commit autorisé."
```

Rendre exécutable :
```bash
chmod +x .git/hooks/pre-commit
```

### Hook de formatage

`.git/hooks/pre-commit` pour formater le code :

```bash
#!/bin/bash
# Formater les fichiers Pascal

for file in $(git diff --cached --name-only | grep "\.pas$")  
do
    echo "Formatage de $file"
    ptop -c ptop.cfg "$file" "$file.tmp"
    mv "$file.tmp" "$file"
    git add "$file"
done
```

## Workflow en équipe

### Stratégies de branches

#### Git Flow

```
Branches Git Flow :
├── main (production)
├── develop (développement)
├── feature/* (nouvelles fonctions)
├── release/* (préparation release)
├── hotfix/* (corrections urgentes)
└── bugfix/* (corrections develop)
```

#### GitHub Flow (plus simple)

```
Branches GitHub Flow :
├── main (toujours déployable)
└── feature/* (tout le reste)
```

### Pull Requests / Merge Requests

Workflow recommandé :

1. **Créer une branche** feature
2. **Développer** et commiter
3. **Pousser** la branche
4. **Créer PR/MR** sur GitHub/GitLab
5. **Review** par un collègue
6. **Merge** après approbation

### Protection de branches

Sur GitHub/GitLab, protégez `main` :

```
Règles de protection :
├── ☑ Require pull request reviews
├── ☑ Dismiss stale reviews
├── ☑ Require status checks
├── ☑ Include administrators
└── ☑ Restrict push access
```

## Dépannage courant

### Problèmes fréquents et solutions

#### "Git not found" dans Lazarus

```
Solution :
1. Vérifier le chemin dans Options
2. Windows : Ajouter Git au PATH système
3. Linux : sudo apt install git
4. Redémarrer Lazarus
```

#### Fichiers binaires dans Git

```bash
# Configurer Git LFS pour gros fichiers
git lfs track "*.exe"  
git lfs track "*.dll"  
git lfs track "resources/*.png"
```

#### Conflits de fin de ligne (CRLF/LF)

```bash
# Configuration globale
git config --global core.autocrlf true  # Windows  
git config --global core.autocrlf input # Linux/Mac

# Dans le projet (.gitattributes)
*.pas text eol=native
*.lpr text eol=native
*.lpi text eol=native
```

#### Lazarus modifie .lpi à chaque ouverture

```xml
<!-- Dans projet.lpi, fixer les valeurs -->
<CompilerOptions>
  <Version Value="11"/>
  <PathDelim Value="\"/>
  <Target>
    <Filename Value="projet"/>
  </Target>
</CompilerOptions>
```

## Scripts d'automatisation

### Script de backup Git (Windows)

`backup.bat` :
```batch
@echo off
echo === Backup Git du projet ===

REM Sauvegarder les modifications  
git add .  
git commit -m "Backup automatique %date% %time%"

REM Pousser vers le serveur  
git push origin main

echo Backup terminé !  
pause
```

### Script de synchronisation (Linux)

`sync.sh` :
```bash
#!/bin/bash

echo "=== Synchronisation du projet ==="

# Récupérer les changements
git pull origin main

# Compiler
lazbuild projet.lpi

if [ $? -eq 0 ]; then
    echo "✓ Projet synchronisé et compilé"
else
    echo "✗ Erreur de compilation"
    exit 1
fi
```

## Métriques et statistiques

### Visualiser l'historique

```bash
# Graphique des branches
git log --graph --oneline --all

# Statistiques par auteur
git shortlog -sn

# Activité récente
git log --since="1 week ago" --oneline

# Fichiers les plus modifiés
git log --pretty=format: --name-only | sort | uniq -c | sort -rg | head -10
```

### Dans Lazarus

**Outils → Contrôle de version → Statistiques**

```
┌─ Statistiques du projet ─────────────────────────┐
│ Commits total : 342                              │
│ Contributeurs : 5                                │
│ Dernière modification : il y a 2 heures          │
│                                                  │
│ Top contributeurs :                              │
│ 1. Alice : 156 commits                           │
│ 2. Bob : 98 commits                              │
│ 3. Charlie : 88 commits                          │
│                                                  │
│ Fichiers actifs :                                │
│ • main.pas : 45 modifications                    │
│ • calculator.pas : 38 modifications              │
└──────────────────────────────────────────────────┘
```

## Conclusion

L'intégration du contrôle de version dans Lazarus transforme votre façon de développer :

**Bénéfices immédiats :**
- 💾 Plus jamais de code perdu
- 🔄 Retour arrière facile
- 📝 Historique documenté
- 👥 Collaboration efficace
- 🔧 Expérimentation sans risque

**Points clés à retenir :**
- Git est le standard, apprenez-le
- Configurez .gitignore correctement
- Commitez souvent, avec de bons messages
- Utilisez les branches pour les nouvelles fonctionnalités
- L'interface Lazarus simplifie les opérations courantes

Le contrôle de version n'est pas optionnel en développement moderne. C'est un investissement qui paie dès le premier jour et devient indispensable avec le temps. Lazarus rend son utilisation simple et intuitive, permettant de se concentrer sur le code plutôt que sur l'outil.

⏭️ [Développement de plugins IDE](/02-maitrise-ide-lazarus/07-developpement-plugins-ide.md)
