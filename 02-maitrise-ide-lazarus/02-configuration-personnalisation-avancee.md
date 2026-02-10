🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.2 Configuration et personnalisation avancée de l'IDE Lazarus

## Introduction : Pourquoi personnaliser son IDE ?

Chaque développeur a ses propres habitudes, préférences et besoins. Lazarus comprend cela et offre une personnalisation poussée qui vous permet de créer **votre** environnement de développement idéal. Que vous préfériez un thème sombre pour reposer vos yeux, des raccourcis clavier spécifiques, ou une organisation particulière de vos outils, tout est possible.

## Accéder aux options de configuration

La porte d'entrée vers la personnalisation se trouve dans le menu :
**Outils** → **Options** (ou **Tools** → **Options** en anglais)

Vous pouvez aussi utiliser le raccourci : **Shift+Ctrl+O**

La fenêtre d'options s'organise en arborescence sur la gauche, avec les catégories principales :

```
Options
├── Environnement
├── Éditeur
├── Outils de code
├── Explorateur de code
├── Débogueur
├── Aide
└── Divers
```

## Configuration de l'environnement

### Général - Les bases de votre IDE

Dans **Environnement** → **Général**, vous trouvez les réglages fondamentaux :

#### Langue de l'interface
```
Langue : [Français (fr)     ▼]
```
Lazarus supporte plus de 30 langues. Le changement nécessite un redémarrage de l'IDE.

**💡 Astuce** : Si certaines traductions sont incomplètes, vous pouvez contribuer via le projet de traduction Lazarus !

#### Vérifications au démarrage
- ☑ **Vérifier si FPC est la bonne version** : S'assure que le compilateur est compatible
- ☑ **Vérifier les fichiers manquants des packages** : Détecte les problèmes de packages
- ☐ **Afficher la fenêtre de bienvenue** : Pour les débutants, c'est utile au début

#### Sauvegarde automatique
```
Options de sauvegarde :
├── Intervalle d'auto-sauvegarde : [5] minutes
├── Type de sauvegarde : [Créer un fichier backup (.bak)]
└── Emplacement : [Même dossier que l'original]
```

**🔒 Sécurité** : Activez toujours l'auto-sauvegarde ! Vous me remercierez après un crash inattendu.

### Fichiers - Gestion des projets récents

Dans **Environnement** → **Fichiers** :

```
Projets et fichiers récents :
├── Nombre maximum de projets récents : [10]
├── Nombre maximum de fichiers récents : [20]
├── Nombre maximum de packages récents : [10]
└── ☑ Ouvrir le dernier projet au démarrage
```

**Comportement à l'ouverture :**
- **Nouveau projet par défaut** : Application GUI, Console, ou Vide
- **Répertoire de travail** : Où Lazarus cherche/sauve par défaut

### Fenêtres - Organisation de l'espace de travail

**Environnement** → **Fenêtres** contrôle l'apparence et le comportement des fenêtres :

#### Fenêtres flottantes vs Ancrées
```
Style de fenêtrage :
○ Fenêtres flottantes (MDI) - Par défaut
● Fenêtre unique ancrée (SDI) - Style Visual Studio
○ Fenêtres flottantes avec barre des tâches
```

#### Comportement des fenêtres
- ☑ **Fenêtre d'édition reste au premier plan** : L'éditeur reste visible
- ☐ **Cacher l'IDE lors de l'exécution** : Masque Lazarus quand votre programme tourne
- ☑ **Centrer automatiquement les nouvelles fenêtres** : Plus propre visuellement

#### Transparence (Windows uniquement)
```
Transparence Alpha :
├── Éditeur : [255] (0=transparent, 255=opaque)
└── Object Inspector : [240]
```

### Bureaux nommés - Vos espaces de travail

**Environnement** → **Bureaux** permet de sauvegarder différentes configurations :

#### Créer un bureau personnalisé

1. Arrangez vos fenêtres comme souhaité
2. **Outils** → **Bureaux** → **Sauvegarder le bureau sous...**
3. Nommez-le selon l'usage :

```
Mes bureaux :
├── 📝 "Édition" - Éditeur maximisé, autres fenêtres minimales
├── 🎨 "Design" - Form Designer prominent, Object Inspector large
├── 🐛 "Débogage" - Toutes les fenêtres de debug visibles
└── 📱 "Mobile" - Configuration pour développement mobile
```

#### Basculement rapide
- **Alt+Shift+D** : Menu rapide de sélection de bureau
- Vous pouvez assigner des raccourcis spécifiques à chaque bureau

### Thèmes et apparence

Depuis Lazarus 2.0, le support des thèmes est natif :

#### Thème de l'IDE
**Environnement** → **Général** → **Thème** :

```
Thème : [Sombre (Dark)     ▼]
        ├── Défaut (Default)
        ├── Sombre (Dark)
        ├── Haute Visibilité
        └── Personnalisé...
```

#### Personnalisation des couleurs
Pour un contrôle total, allez dans **Environnement** → **Grille d'objets** :

```
Couleurs personnalisées :
├── Arrière-plan : [#1E1E1E]
├── Texte : [#D4D4D4]
├── Sélection : [#264F78]
├── Ligne de grille : [#3C3C3C]
└── Propriétés modifiées : [#FFE4B5]
```

## Configuration de l'éditeur de code

### Général - Comportement de base

**Éditeur** → **Général** définit le comportement fondamental :

#### Tabulations
```
Tabulations :
├── Largeur de tab : [2]
├── ● Insérer des espaces
├── ○ Garder les tabulations
└── ☑ Tabulation intelligente (Smart Tabs)
```

**📏 Convention** : En Pascal, la norme est 2 espaces d'indentation.

#### Options de saisie
- ☑ **Auto-indentation** : Maintient l'indentation de la ligne précédente
- ☑ **Indentation intelligente** : Ajuste selon les mots-clés (begin, end, etc.)
- ☑ **Backspace désindente** : Supprime l'indentation par blocs
- ☑ **Glisser-déposer** : Permet de déplacer du texte à la souris

#### Limites et guides
```
Marge droite :
├── Position : [80] caractères
├── Couleur : [Gris clair]
└── Style : [Ligne continue]
```

### Affichage - Apparence visuelle

**Éditeur** → **Affichage** contrôle l'aspect visuel :

#### Police de caractères
```
Police :
├── Nom : [Consolas] (Windows) / [Ubuntu Mono] (Linux)
├── Taille : [10] points
└── ☑ Anti-aliasing
```

**Polices recommandées pour le code :**
- **Windows** : Consolas, Cascadia Code, Fira Code
- **Linux** : Ubuntu Mono, Source Code Pro, JetBrains Mono
- **Avec ligatures** : Fira Code, Cascadia Code (pour → ≤ ≠ etc.)

#### Éléments visuels
```
Afficher :
├── ☑ Numéros de ligne
├── ☑ Repères de modification (barre jaune/verte)
├── ☑ Code folding (pliage de code)
├── ☐ Minimap (aperçu miniature)
├── ☑ Caractères spéciaux (espaces, tabs)
└── ☑ Bracket highlighting (parenthèses correspondantes)
```

### Couleurs - Schémas de coloration syntaxique

**Éditeur** → **Couleurs** offre une personnalisation complète :

#### Schémas prédéfinis
```
Schéma : [Twilight          ▼]
         ├── Default
         ├── Delphi Classic
         ├── Ocean
         ├── Twilight (sombre)
         ├── Monokai
         ├── Solarized Dark
         └── Solarized Light
```

#### Personnalisation par élément
Chaque élément du langage peut avoir sa propre couleur :

```
Élément          | Texte    | Fond     | Gras | Italique
-----------------|----------|----------|------|----------
Mots réservés    | #569CD6  | -        | ✓    | ✗  
Chaînes          | #CE9178  | -        | ✗    | ✗  
Nombres          | #B5CEA8  | -        | ✗    | ✗  
Commentaires     | #6A9955  | -        | ✗    | ✓  
Directives       | #C586C0  | -        | ✗    | ✗  
Symboles         | #D4D4D4  | -        | ✗    | ✗
```

#### Export/Import de schémas
Vous pouvez partager vos schémas :
- **Exporter** : Sauvegarde en XML
- **Importer** : Charge un schéma XML
- **Partage communautaire** : Wiki Lazarus a une section dédiée

### Complétion de code

**Éditeur** → **Complétion et Conseils** :

#### Auto-complétion
```
Déclenchement :
├── Délai : [500] millisecondes
├── ☑ Auto-invoquer après un point (.)
├── ☑ Auto-invoquer pour les identifiants
└── Nombre de caractères minimum : [3]
```

#### Paramètres de fonction
- ☑ **Afficher les conseils de paramètres** : Montre la signature des fonctions
- ☑ **Afficher les valeurs lors du débogage** : Survol pour voir les valeurs

#### Templates de code (Code Templates)

**Éditeur** → **Code Templates** permet de créer des raccourcis :

```pascal
// Tapez "forb" puis Ctrl+J génère :
for i := 0 to List.Count - 1 do  
begin
  |
end;
```

**Templates utiles prédefinis :**
- `classf` : Squelette de classe complète
- `tryf` : Bloc try...finally
- `trye` : Bloc try...except
- `forb` : Boucle for avec begin...end
- `ifb` : Structure if then begin...end

### Mappage des touches - Raccourcis clavier

**Éditeur** → **Mappage des touches** :

#### Schémas de raccourcis
```
Schéma : [Lazarus (défaut)  ▼]
         ├── Lazarus (défaut)
         ├── Classic (Turbo Pascal)
         ├── Delphi
         ├── Visual Studio
         └── Personnalisé
```

#### Personnalisation des raccourcis

Recherchez une commande et assignez votre raccourci :

```
Recherche : [Compiler]

Commande                    | Raccourci 1  | Raccourci 2
---------------------------|--------------|-------------
Compiler                   | Ctrl+F9      | -  
Compiler et Exécuter       | F9           | Shift+F9  
Construction rapide        | Shift+F9     | -
```

**⚠️ Conflits** : Lazarus détecte automatiquement les conflits de raccourcis et vous avertit.

### Mouse Actions - Actions de la souris

**Éditeur** → **Souris** :

#### Comportement du clic
```
Simple clic :
├── Gauche : Positionner le curseur
├── Milieu : Coller (Linux) / Aller à la déclaration
└── Droit : Menu contextuel
```

#### Double-clic et triple-clic
```
Double-clic : [Sélectionner le mot]  
Triple-clic : [Sélectionner la ligne]  
Quadruple-clic : [Sélectionner le paragraphe]
```

#### Molette de défilement
```
Molette :
├── Défiler : 3 lignes
├── Ctrl+Molette : Zoom
└── Shift+Molette : Défilement horizontal
```

## Configuration des outils de code

### Code Tools - Intelligence du code

**Outils de code** → **Général** :

#### Indentation automatique
```
Style d'indentation :
├── ● Style Lazarus/FPC
├── ○ Style Delphi
└── ○ GNU Pascal
```

#### Insertion automatique
- ☑ **Ajouter point-virgule à la fin** : Complète automatiquement
- ☑ **Compléter les assignations** : `Variable := |` devient `Variable := Value;`
- ☑ **Compléter les blocs** : Ajoute `end` après `begin`

### Explorateur de code

**Explorateur de code** → **Options** :

```
Affichage :
├── ☑ Suivre le curseur
├── ☑ Afficher les sections privées
├── ☑ Afficher les directives
└── Mode : [Catégorisé par type]
```

## Configuration du débogueur

### Backend de débogage

**Débogueur** → **Général** :

#### Windows
```
Type de débogueur : [GDB          ▼]
                    ├── GDB (GNU Debugger)
                    ├── LLDB
                    └── FpDebug (natif)

Chemin GDB : [C:\lazarus\mingw\x86_64-win64\bin\gdb.exe]
```

#### Linux
```
Type de débogueur : [GDB          ▼]  
Chemin GDB : [/usr/bin/gdb]
```

### Options de débogage

```
Comportement :
├── ☑ Afficher la fenêtre de débogage au démarrage
├── ☑ Réinitialiser après chaque exécution
├── ☐ Afficher les messages du débogueur
└── Timeout : [30] secondes
```

## Configuration spécifique par OS

### Windows - Particularités

#### Intégration Shell Windows
**Environnement** → **Options Windows** :

```
Intégration :
├── ☑ Associer les fichiers .pas/.pp
├── ☑ Associer les fichiers .lpr/.lpi
├── ☑ Menu contextuel "Ouvrir avec Lazarus"
└── ☐ Épingler à la barre des tâches
```

#### Antivirus et performances
```
Exclusions recommandées pour l'antivirus :
├── C:\lazarus\*
├── %TEMP%\*.ppu
├── Dossier de vos projets
└── *.exe compilés (pendant le développement)
```

### Linux/Ubuntu - Particularités

#### Intégration Desktop
**Environnement** → **Options Linux** :

```
Desktop :
├── ☑ Créer entrée .desktop
├── ☑ Intégration MIME types
└── Gestionnaire : [GNOME/KDE/XFCE]
```

#### Permissions et chemins
```
Chemins importants :
├── Config : ~/.lazarus/
├── Packages : ~/.lazarus/lib/
├── Templates : ~/.lazarus/templates/
└── Backups : ~/.lazarus/backup/
```

## Profils de configuration

### Créer des profils différents

Lazarus permet plusieurs configurations via `--primary-config-path` :

#### Windows (raccourci)
```batch
lazarus.exe --primary-config-path=C:\lazarus-profiles\web  
lazarus.exe --primary-config-path=C:\lazarus-profiles\mobile
```

#### Linux (alias bash)
```bash
alias lazarus-web='lazarus --primary-config-path=~/.lazarus-web'  
alias lazarus-mobile='lazarus --primary-config-path=~/.lazarus-mobile'
```

### Cas d'usage des profils

```
Profil "Développement Web" :
├── Packages web installés
├── Templates HTML/CSS
└── Raccourcis orientés web

Profil "Applications Desktop" :
├── Composants visuels complets
├── Thème adapté au design
└── Debugger optimisé

Profil "Embedded/IoT" :
├── Cross-compilation ARM
├── Outils série/GPIO
└── Optimisations taille
```

## Sauvegarde et restauration

### Sauvegarder sa configuration

#### Fichiers essentiels à sauvegarder

**Windows :**
```
%LOCALAPPDATA%\lazarus\
├── environmentoptions.xml    (config générale)
├── editoroptions.xml         (config éditeur)
├── codetoolsoptions.xml      (outils de code)
├── buildlazarus.xml          (config de build)
└── inputhistory.xml          (historique)
```

**Linux :**
```
~/.lazarus/
├── environmentoptions.xml
├── editoroptions.xml
├── codetoolsoptions.xml
├── buildlazarus.xml
└── inputhistory.xml
```

### Script de sauvegarde automatique

#### Windows (batch)
```batch
@echo off
set BACKUP_DIR=D:\LazarusBackup\%DATE:/=-%  
xcopy "%LOCALAPPDATA%\lazarus" "%BACKUP_DIR%" /E /I /Y  
echo Configuration sauvegardée dans %BACKUP_DIR%
```

#### Linux (bash)
```bash
#!/bin/bash
BACKUP_DIR=~/LazarusBackup/$(date +%Y%m%d)  
cp -r ~/.lazarus "$BACKUP_DIR"  
echo "Configuration sauvegardée dans $BACKUP_DIR"
```

## Optimisations de performance

### Mémoire et compilation

**Projet** → **Options du projet** → **Compilation et Liaison** :

```
Optimisations :
├── Niveau : [2 - Optimisations normales]
├── ☐ Optimiser pour la taille (-Os)
├── ☑ Optimiser pour le processeur (-Op3)
└── Unités précompilées : [Utiliser si disponible]
```

### Cache et fichiers temporaires

```
Gestion du cache :
├── Taille max du cache : [500] MB
├── Nettoyer automatiquement : [Après 30 jours]
└── Emplacement : [%TEMP%\lazarus-cache]
```

## Plugins et extensions recommandés

### Packages essentiels pour la productivité

1. **AnchorDocking** : Fenêtre unique style Visual Studio
2. **OnlinePackageManager** : Gestionnaire de packages en ligne
3. **PascalScript** : Scripting dans l'IDE
4. **EditorMacroScript** : Macros pour l'éditeur
5. **LazProfiler** : Profilage de performance

### Installation via OPM (Online Package Manager)

**Paquet** → **Online Package Manager** :

```
Packages disponibles :
├── 📦 BGRABitmap (graphismes avancés)
├── 📦 Indy (réseau)
├── 📦 ZEOS (base de données)
├── 📦 SynEdit (éditeur amélioré)
└── 📦 VirtualTreeView (arbres avancés)
```

## Raccourcis essentiels à connaître

### Navigation
- **Ctrl+Clic** : Aller à la déclaration
- **Ctrl+Shift+Haut/Bas** : Naviguer entre déclaration/implémentation
- **Alt+Gauche/Droite** : Historique de navigation
- **Ctrl+Shift+C** : Complétion de classe

### Édition
- **Ctrl+D** : Formater le code
- **Ctrl+Shift+D** : Dupliquer la ligne
- **Ctrl+Y** : Supprimer la ligne
- **Ctrl+/** : Commenter/Décommenter

### Débogage
- **F9** : Compiler et exécuter
- **F8** : Pas à pas principal
- **F7** : Pas à pas détaillé
- **F5** : Ajouter/Retirer breakpoint

## Résolution de problèmes courants

### L'IDE est lent

1. Désactiver les packages non utilisés
2. Réduire l'historique des fichiers récents
3. Désactiver la vérification en temps réel
4. Augmenter la mémoire pour le compilateur

### Les raccourcis ne fonctionnent pas

1. Vérifier les conflits dans le mappage des touches
2. Réinitialiser le schéma de raccourcis
3. Vérifier que l'éditeur a le focus

### La configuration est corrompue

1. Renommer le dossier de config
2. Lazarus créera une nouvelle config
3. Réimporter progressivement vos paramètres

## Bonnes pratiques

### Organisation de l'espace de travail

1. **Créez des bureaux nommés** pour différentes tâches
2. **Utilisez plusieurs moniteurs** si possible
3. **Gardez les outils fréquents visibles**
4. **Masquez ce qui n'est pas nécessaire**

### Maintenance régulière

- **Nettoyez le cache** mensuellement
- **Sauvegardez votre config** avant les mises à jour
- **Documentez vos personnalisations** pour pouvoir les reproduire
- **Partagez vos configs** avec votre équipe pour l'uniformité

## Conclusion

La personnalisation de Lazarus est un investissement qui paie sur le long terme. Un IDE bien configuré selon vos besoins peut facilement doubler votre productivité. N'hésitez pas à expérimenter avec les différentes options - vous pouvez toujours revenir en arrière grâce aux sauvegardes.

Les points essentiels à retenir :
- Commencez par les bases (thème, police, raccourcis)
- Créez des profils pour différents types de projets
- Sauvegardez régulièrement votre configuration
- Explorez progressivement les options avancées
- Adaptez l'IDE à votre workflow, pas l'inverse

Dans la section suivante (2.3), nous verrons comment étendre encore plus Lazarus avec le système de packages.

⏭️ [Création et gestion de packages](/02-maitrise-ide-lazarus/03-creation-gestion-packages.md)
