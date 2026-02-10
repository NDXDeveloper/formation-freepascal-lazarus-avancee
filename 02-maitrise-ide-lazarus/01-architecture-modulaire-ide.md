🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.1 Architecture modulaire de l'IDE Lazarus

## Introduction : Qu'est-ce qu'une architecture modulaire ?

Imaginez l'IDE Lazarus comme un ensemble de blocs de construction (modules) que vous pouvez assembler, réorganiser et personnaliser selon vos besoins. Contrairement à un logiciel monolithique où tout est figé, Lazarus est conçu pour être flexible et extensible. Chaque fenêtre, chaque panneau, chaque fonctionnalité est un module indépendant qui communique avec les autres.

## Les modules principaux de Lazarus

### 1. La fenêtre principale (Main IDE Bar)

C'est la barre de contrôle centrale de Lazarus, généralement positionnée en haut de votre écran. Elle contient :

- **Le menu principal** : Fichier, Édition, Recherche, Affichage, Projet, Exécuter, Paquet, Outils, Fenêtre, Aide
- **Les barres d'outils** : Boutons pour les actions fréquentes (Nouveau, Ouvrir, Sauvegarder, Compiler, Exécuter)
- **La barre de composants** : Les palettes d'outils organisées par onglets (Standard, Additional, Common Controls, etc.)

Cette fenêtre est le "cerveau" de l'IDE - si vous la fermez, Lazarus se ferme complètement.

**💡 Astuce débutant** : Si vous perdez une fenêtre, utilisez le menu **Fenêtre** de cette barre principale pour la retrouver et la réafficher.

### 2. L'éditeur de code source (Source Editor)

C'est là où vous écrivez votre code Pascal. Cet éditeur est basé sur **SynEdit**, un composant d'édition très puissant qui offre :

```pascal
// Exemple de ce que vous verrez dans l'éditeur
unit Unit1;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type
  TForm1 = class(TForm)
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;
```

**Caractéristiques du module éditeur :**
- **Onglets multiples** : Chaque fichier ouvert a son propre onglet
- **Coloration syntaxique** : Le code est coloré selon sa fonction (mots-clés en bleu, chaînes en rouge, etc.)
- **Code folding** : Possibilité de replier/déplier des sections de code
- **Numérotation des lignes** : Affichage optionnel des numéros de ligne
- **Minimap** : Vue miniature du code sur le côté (comme dans VS Code)

**🔍 Bon à savoir** : L'éditeur peut être divisé en plusieurs panneaux pour voir différentes parties du même fichier ou différents fichiers côte à côte.

### 3. Le concepteur de formulaires (Form Designer)

Quand vous créez une application graphique, ce module vous permet de dessiner visuellement vos fenêtres :

```
┌─────────────────────────────┐
│     Form1: TForm1           │
├─────────────────────────────┤
│                             │
│   [Button1]  [Edit1____]    │
│                             │
│   [Memo1                ]   │
│   [                     ]   │
│   [_____________________]   │
│                             │
└─────────────────────────────┘
```

**Fonctionnement du designer :**
- **Mode WYSIWYG** : Ce que vous voyez est ce que vous obtiendrez
- **Grille magnétique** : Aide à aligner les composants
- **Guides d'alignement** : Lignes bleues qui apparaissent pour aligner avec d'autres composants
- **Poignées de redimensionnement** : Les 8 petits carrés autour d'un composant sélectionné

### 4. L'inspecteur d'objets (Object Inspector)

Ce module affiche et permet de modifier les propriétés et événements du composant sélectionné :

```
┌─ Object Inspector ──────────┐
│ Form1: TForm1               │
├─────────────────────────────┤
│ Properties │ Events │       │
├─────────────────────────────┤
│ Caption    │ Form1          │
│ Color      │ clBtnFace      │
│ Height     │ 300            │
│ Left       │ 0              │
│ Name       │ Form1          │
│ Top        │ 0              │
│ Width      │ 400            │
│ ...                         │
└─────────────────────────────┘
```

**Structure de l'inspecteur :**
- **Onglet Propriétés** : Caractéristiques modifiables (taille, couleur, texte, etc.)
- **Onglet Événements** : Actions déclenchables (OnClick, OnCreate, OnClose, etc.)
- **Onglet Favoris** : Propriétés que vous utilisez fréquemment
- **Filtre de recherche** : Pour trouver rapidement une propriété

### 5. La fenêtre de messages (Messages)

Affiche les résultats de compilation, les erreurs, les avertissements :

```
┌─ Messages ──────────────────────────────────┐
│ Compile Project, Target: project1.exe       │
│ Success: Compiled successfully              │
│ 0 Errors, 0 Warnings, 2 Hints               │
│                                             │
│ Hint: Start of reading config file...       │
│ Hint: End of reading config file...         │
└─────────────────────────────────────────────┘
```

**Types de messages :**
- **❌ Erreurs** : Empêchent la compilation (en rouge)
- **⚠️ Avertissements** : Problèmes potentiels (en orange)
- **💡 Conseils (Hints)** : Suggestions d'amélioration (en bleu)
- **ℹ️ Notes** : Informations générales (en vert)

## Organisation des modules en fenêtres

### Mode fenêtres flottantes (par défaut)

Par défaut, Lazarus utilise des **fenêtres flottantes indépendantes**. Chaque module est dans sa propre fenêtre que vous pouvez déplacer librement sur votre écran :

```
┌──────────────┐     ┌─────────────────┐
│ Main IDE Bar │     │ Source Editor   │
└──────────────┘     └─────────────────┘

┌─────────────────┐  ┌─────────────────┐
│ Form Designer   │  │Object Inspector │
└─────────────────┘  └─────────────────┘
```

**Avantages :**
- Flexibilité maximale
- Excellent pour le multi-écrans
- Chaque fenêtre peut être sur un écran différent

**Inconvénients :**
- Peut sembler désorganisé pour les débutants
- Gestion des fenêtres plus complexe

### Mode fenêtre unique (avec AnchorDocking)

Vous pouvez transformer Lazarus en IDE à fenêtre unique (comme Visual Studio) en installant le package **AnchorDockingDsgn** :

1. Menu **Paquet** → **Installer/Désinstaller des paquets**
2. Chercher "anchordockingdsgn" dans la liste
3. L'ajouter et recompiler l'IDE

Résultat : Tous les modules sont ancrés dans une seule grande fenêtre :

```
┌────────────────────────────────────────────┐
│            Main IDE Bar                    │
├────────────┬───────────────────────────────┤
│            │                               │
│  Object    │      Source Editor            │
│ Inspector  │                               │
│            ├───────────────────────────────┤
│            │      Form Designer            │
│            │                               │
├────────────┴───────────────────────────────┤
│            Messages Window                 │
└────────────────────────────────────────────┘
```

## Modules additionnels importants

### L'explorateur de code (Code Explorer)

Accessible via **Affichage** → **Code Explorer**, ce module affiche la structure de votre code :

```
┌─ Code Explorer ─────────────┐
│ ▼ Unit1                     │
│   ▼ Types                   │
│     ▶ TForm1                │
│   ▼ Variables               │
│     Form1: TForm1           │
│   ▼ Procedures              │
│     FormCreate              │
│     Button1Click            │
└─────────────────────────────┘
```

### L'explorateur de projet (Project Inspector)

Géré via **Projet** → **Inspecteur de projet**, il montre tous les fichiers de votre projet :

```
┌─ Project Inspector ─────────┐
│ project1.lpi                │
│ ▼ Files                     │
│   • unit1.pas               │
│   • unit2.pas               │
│ ▼ Required Packages         │
│   • LCL                     │
│ ▼ Dependencies              │
└─────────────────────────────┘
```

### La fenêtre de débogage

Plusieurs sous-modules pour le débogage :

- **Points d'arrêt** (Breakpoints) : Liste des points où le code s'arrêtera
- **Pile d'appels** (Call Stack) : Hiérarchie des appels de fonctions
- **Variables locales** : Valeurs des variables dans le contexte actuel
- **Watches** : Variables que vous surveillez spécifiquement

## Communication entre les modules

Les modules ne sont pas isolés, ils communiquent constamment :

### Exemple de flux de communication

1. Vous **double-cliquez** sur un bouton dans le **Form Designer**
2. Le **Form Designer** envoie un message à l'**Object Inspector**
3. L'**Object Inspector** crée un gestionnaire d'événement OnClick
4. Il communique avec l'**Éditeur de code** qui s'ouvre au bon endroit
5. L'**Éditeur** génère automatiquement le squelette de la procédure

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin
  // Le curseur se place ici automatiquement
end;
```

### Le système de messages interne

Lazarus utilise un système de messages pour la communication :

- **Messages synchrones** : Le module attend une réponse (ex: sauvegarde avant compilation)
- **Messages asynchrones** : Notification sans attente (ex: mise à jour de l'affichage)
- **Broadcasting** : Un message envoyé à tous les modules (ex: changement de projet)

## Personnalisation de l'architecture

### Sauvegarde de la disposition (Desktop)

Lazarus permet de sauvegarder votre arrangement de fenêtres :

1. Arrangez vos fenêtres comme vous le souhaitez
2. Menu **Outils** → **Bureaux** → **Sauver le bureau sous...**
3. Donnez un nom (ex: "Développement", "Débogage", "Design")

Vous pouvez ensuite basculer rapidement entre différentes dispositions selon votre tâche.

### Configuration par type de projet

Lazarus peut mémoriser des dispositions différentes selon le type de projet :

- **Application GUI** : Form Designer visible, Object Inspector ouvert
- **Application Console** : Juste l'éditeur et les messages
- **Bibliothèque** : Focus sur l'explorateur de code

## Gestion de la mémoire et des ressources

Chaque module gère ses propres ressources :

### Isolation des modules

- **Crash protection** : Si un module rencontre un problème, les autres continuent
- **Gestion mémoire** : Chaque module libère sa mémoire indépendamment
- **Threads séparés** : Certains modules tournent dans leur propre thread

### Optimisation des performances

```
Module          | RAM utilisée | Thread | Priorité
----------------|--------------|--------|----------
Main IDE        | ~50 MB       | Main   | Haute  
Source Editor   | ~20-100 MB   | Main   | Haute  
Form Designer   | ~30 MB       | Main   | Moyenne  
Compilateur     | Variable     | Séparé | Basse  
Débogueur       | ~40 MB       | Séparé | Moyenne
```

## Modules spécifiques Windows vs Linux

### Sur Windows

- **Windows API Inspector** : Pour explorer les API Windows
- **Registry Editor** : Édition du registre Windows intégré
- **COM Type Library Editor** : Pour les composants COM/ActiveX

### Sur Linux/Ubuntu

- **GTK Inspector** : Pour déboguer les interfaces GTK
- **D-Bus Browser** : Explorer les services D-Bus
- **Terminal intégré** : Console Linux dans l'IDE

## Architecture des plugins et packages

Les modules peuvent être étendus via le système de packages :

### Structure d'un package

```
MonPackage.lpk
├── Sources
│   ├── moncomposant.pas
│   └── monmodule.pas
├── Ressources
│   └── icones.res
└── Installation
    └── register.pas
```

### Points d'extension

Lazarus expose des **points d'extension** (hooks) où les packages peuvent s'intégrer :

- **Menu IDE** : Ajouter des entrées de menu
- **Barres d'outils** : Nouveaux boutons
- **Éditeur** : Nouveaux raccourcis et fonctionnalités
- **Composants** : Nouveaux composants dans la palette

## Comprendre les fichiers de configuration

Chaque module stocke sa configuration :

### Windows
```
C:\Users\[Username]\AppData\Local\lazarus\
├── editoroptions.xml      (config éditeur)
├── environmentoptions.xml  (config environnement)
├── inputhistory.xml       (historique)
└── projectsessions/       (sessions projet)
```

### Linux/Ubuntu
```
~/.lazarus/
├── editoroptions.xml
├── environmentoptions.xml
├── inputhistory.xml
└── projectsessions/
```

## Diagnostic et résolution de problèmes

### Si un module ne s'affiche pas

1. Menu **Fenêtre** → Vérifier si le module est listé
2. Menu **Affichage** → Certains modules sont ici
3. Menu **Outils** → **Options** → **Fenêtres** → Réinitialiser les positions

### Si l'IDE semble lent

- Vérifier le nombre de packages installés
- Désactiver les modules non utilisés
- Augmenter la mémoire allouée au compilateur

### Réinitialisation complète

En cas de problème majeur, renommer le dossier de configuration :
- Windows : Renommer `%APPDATA%\lazarus` en `lazarus.bak`
- Linux : Renommer `~/.lazarus` en `.lazarus.bak`

Lazarus recréera une configuration propre au prochain démarrage.

## Conseils pour bien démarrer

### Pour les débutants venant d'autres IDEs

**Si vous venez de Visual Studio :**
- Installez AnchorDocking pour retrouver une fenêtre unique
- Utilisez le schéma de raccourcis Visual Studio (Outils → Options → Éditeur → Mappage des touches)

**Si vous venez de Delphi :**
- L'organisation est très similaire, vous serez en terrain connu
- Les raccourcis sont identiques par défaut

**Si vous venez de VS Code :**
- Gardez les fenêtres flottantes pour la flexibilité
- Utilisez Ctrl+Shift+F pour la recherche dans les fichiers

### Organisation recommandée pour débuter

1. **Écran principal** : Éditeur de code au centre
2. **À gauche** : Object Inspector
3. **À droite** : Form Designer (si application GUI)
4. **En bas** : Messages
5. **En haut** : Main IDE Bar

Cette disposition offre un bon équilibre entre visibilité et productivité.

## Conclusion

L'architecture modulaire de Lazarus est sa grande force. Elle permet une personnalisation totale tout en restant stable et performante. Au début, cette modularité peut sembler complexe, mais elle devient rapidement un atout majeur pour adapter l'IDE à votre façon de travailler.

Les points clés à retenir :
- Chaque fenêtre est un module indépendant
- Les modules communiquent via un système de messages
- Tout est personnalisable et extensible
- La configuration peut être sauvegardée et restaurée
- Les différences Windows/Linux sont minimes

Dans la prochaine section (2.2), nous verrons comment configurer et personnaliser ces modules pour créer votre environnement de développement idéal.

⏭️ [Configuration et personnalisation avancée](/02-maitrise-ide-lazarus/02-configuration-personnalisation-avancee.md)
