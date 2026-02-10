🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.5 Débogueur GDB et alternatives
## Introduction complète au débogage dans Lazarus

### Qu'est-ce que le débogage ?

Le débogage est l'art de trouver et corriger les erreurs (bugs) dans votre code. C'est comme être un détective : vous avez un crime (le bug), des indices (les symptômes), et vous devez mener l'enquête pour trouver le coupable (la ligne de code fautive).

Sans débogueur, vous êtes limité à des techniques rudimentaires comme ajouter des `ShowMessage()` partout dans votre code pour comprendre ce qui se passe. Avec un débogueur, vous pouvez :
- ⏸️ **Mettre le programme en pause** à n'importe quel moment
- 👀 **Examiner les valeurs** de toutes les variables
- 🚶 **Exécuter le code pas à pas** pour suivre son déroulement
- 📚 **Explorer la pile d'appels** pour comprendre comment vous êtes arrivé là
- 🔍 **Inspecter la mémoire** directement

### Pourquoi le débogage est-il crucial ?

Imaginez cette situation frustrante : votre programme plante, mais seulement parfois, et vous ne savez pas pourquoi. Ou pire : il donne des résultats incorrects sans message d'erreur. Sans débogueur, vous pourriez passer des heures à chercher à l'aveugle. Avec un débogueur, vous pouvez identifier le problème en quelques minutes.

**Statistiques révélatrices :**
- Les développeurs passent environ **50% de leur temps** à déboguer
- Un bug trouvé tôt coûte **100x moins cher** à corriger qu'un bug en production
- La majorité des bugs sont des erreurs simples facilement identifiables avec un débogueur

### Vue d'ensemble des débogueurs disponibles

Lazarus supporte plusieurs débogueurs, chacun avec ses forces et faiblesses :

```
Débogueurs supportés par Lazarus :
├── GDB (GNU Debugger)
│   ├── Le plus mature et universel
│   ├── Support complet sur toutes les plateformes
│   └── Parfois complexe à configurer
├── FpDebug
│   ├── Débogueur natif Pascal
│   ├── Plus rapide et léger que GDB
│   └── Encore en développement actif
├── LLDB
│   ├── Débogueur moderne du projet LLVM
│   ├── Excellent sur macOS
│   └── Support expérimental sur Windows/Linux
└── GDB Server
    ├── Pour le débogage distant
    ├── Embedded et IoT
    └── Applications sur serveur
```

### GDB : Le standard de l'industrie

**GDB (GNU Debugger)** est le débogueur le plus utilisé dans le monde open source. Créé en 1986 par Richard Stallman, il est devenu le standard de facto pour le débogage sur Linux et est largement supporté sur Windows et macOS.

#### Pourquoi GDB ?

**Points forts :**
- ✅ **Maturité** : Plus de 35 ans de développement
- ✅ **Universalité** : Fonctionne sur presque toutes les plateformes
- ✅ **Puissance** : Capacités de débogage très avancées
- ✅ **Standards** : Respecte les formats de debug DWARF
- ✅ **Communauté** : Énorme base d'utilisateurs et documentation

**Limitations :**
- ⚠️ **Configuration** : Peut être délicat à configurer sur Windows
- ⚠️ **Performance** : Parfois lent sur de gros projets
- ⚠️ **Interface** : Conçu pour la ligne de commande à l'origine
- ⚠️ **Pascal** : Pas optimisé spécifiquement pour Object Pascal

### FpDebug : Le futur du débogage Pascal

**FpDebug** est un débogueur développé spécifiquement pour FreePascal/Lazarus. C'est un projet relativement récent mais très prometteur.

#### Avantages de FpDebug

**Points forts :**
- 🚀 **Performance** : 2 à 5x plus rapide que GDB
- 🎯 **Spécialisé Pascal** : Comprend nativement les types Pascal
- 📦 **Intégré** : Pas de dépendance externe
- 🔧 **Simple** : Configuration minimale requise
- 💡 **Moderne** : Architecture conçue pour les IDEs modernes

**Limitations actuelles :**
- 🚧 **En développement** : Certaines fonctionnalités manquantes
- 📚 **Documentation** : Moins de ressources disponibles
- 🔌 **Compatibilité** : Support limité pour le code C/C++
- 🐛 **Stabilité** : Quelques bugs dans les cas complexes

### LLDB : L'alternative moderne

**LLDB** fait partie du projet LLVM et représente une approche moderne du débogage. Il est le débogueur par défaut sur macOS.

#### Caractéristiques de LLDB

**Points forts :**
- 🍎 **macOS** : Intégration native excellente
- ⚡ **Rapidité** : Architecture moderne et efficace
- 🔄 **Extensible** : API Python pour l'automatisation
- 🎨 **Interface** : Meilleure intégration IDE

**Limitations :**
- 🪟 **Windows** : Support expérimental seulement
- 🐧 **Linux** : Moins mature que GDB
- 📖 **Pascal** : Support basique pour l'instant

### Architecture du débogage dans Lazarus

Pour comprendre comment fonctionne le débogage, visualisons l'architecture :

```
┌─────────────────────────────────────────────────┐
│                IDE LAZARUS                      │
│  ┌───────────────────────────────────────────┐  │
│  │        Interface de débogage              │  │
│  │  (Points d'arrêt, Variables, Pile, etc.)  │  │
│  └───────────────┬───────────────────────────┘  │
│                  │                              │
│  ┌───────────────▼───────────────────────────┐  │
│  │        Couche d'abstraction               │  │
│  │         (Debug Interface)                 │  │
│  └───────────────┬───────────────────────────┘  │
└──────────────────┼──────────────────────────────┘
                   │
    ┌──────────────┼──────────────┐
    │              │              │
┌───▼───┐    ┌────▼────┐    ┌────▼────┐
│  GDB  │    │ FpDebug │    │  LLDB   │
└───┬───┘    └────┬────┘    └────┬────┘
    │             │              │
┌───▼─────────────▼──────────────▼────┐
│     Votre Application Compilée      │
│        (avec symboles de debug)     │
└─────────────────────────────────────┘
```

### Concepts fondamentaux du débogage

#### 1. Les symboles de débogage

Pour que le débogueur fonctionne, votre programme doit être compilé avec des **symboles de débogage**. Ces symboles créent une carte entre le code machine et votre code source.

```pascal
// Sans symboles : Le débogueur voit
0x00401234: mov eax, [ebp-4]
0x00401237: add eax, 1

// Avec symboles : Le débogueur peut montrer
Ligne 42: i := i + 1;
```

**Configuration dans Lazarus :**
- **Projet** → **Options du projet** → **Compilation et édition de liens**
- Cocher : ☑ **Générer les informations de débogage**
- Type : **Dwarf with sets (-gw -godwarfsets)**

#### 2. Les points d'arrêt (Breakpoints)

Un point d'arrêt est un marqueur qui dit au débogueur : "Arrête l'exécution ici". C'est votre outil principal pour examiner l'état du programme.

**Types de points d'arrêt :**
```
Points d'arrêt disponibles :
├── Point d'arrêt simple
│   └── S'arrête à une ligne spécifique
├── Point d'arrêt conditionnel
│   └── S'arrête seulement si condition vraie
├── Point d'arrêt sur données (Watchpoint)
│   └── S'arrête quand une variable change
├── Point d'arrêt sur exception
│   └── S'arrête quand une exception est levée
└── Point d'arrêt sur fonction
    └── S'arrête à l'entrée d'une fonction
```

#### 3. L'exécution pas à pas

Une fois arrêté, vous pouvez contrôler l'exécution :

- **Step Into (F7)** : Entre dans les fonctions appelées
- **Step Over (F8)** : Exécute la ligne sans entrer dans les fonctions
- **Step Out (Shift+F8)** : Sort de la fonction actuelle
- **Run to Cursor (F4)** : Continue jusqu'au curseur

```pascal
procedure Principal;  
begin
  Initialiser;      // F8 : exécute sans entrer
  x := Calculer(5); // F7 : entre dans Calculer
  Afficher(x);      // F8 : continue sans entrer
end;
```

#### 4. L'inspection des variables

Le débogueur permet d'examiner toutes les variables :

- **Variables locales** : Automatiquement affichées
- **Watches** : Variables que vous surveillez spécifiquement
- **Evaluate/Modify** : Examiner et modifier n'importe quelle expression
- **Inspector** : Vue détaillée des objets complexes

#### 5. La pile d'appels (Call Stack)

La pile d'appels montre comment vous êtes arrivé au point actuel :

```
Pile d'appels :
#0 TCalculateur.DiviserParZero (ligne 45)
#1 TCalculateur.Calculer (ligne 23)
#2 TForm1.Button1Click (ligne 67)
#3 TControl.Click (ligne 2847)
#4 TButton.Click (ligne 98)
#5 Main program (ligne 15)
```

### Modes de débogage

Lazarus offre plusieurs modes selon vos besoins :

#### Mode Debug standard

Configuration par défaut pour le développement :
```
Optimisation : Aucune (-O0)  
Symboles : Complets (-g)  
Assertions : Activées (-Sa)  
Vérifications : Toutes activées (-Cr -Co -Ct)
```

#### Mode Release avec infos

Pour déboguer en conditions réelles :
```
Optimisation : Niveau 2 (-O2)  
Symboles : Externes (-Xg)  
Assertions : Désactivées  
Vérifications : Désactivées
```

#### Mode Profiling

Pour analyser les performances :
```
Optimisation : Niveau 2 (-O2)  
Symboles : Complets (-g)  
Profiling : Activé (-pg)
```

### Fonctionnalités avancées du débogage

#### Débogage conditionnel

```pascal
{$IFDEF DEBUG}
  WriteLn('Variable X = ', X);
  Assert(X > 0, 'X doit être positif');
{$ENDIF}
```

#### Points d'arrêt conditionnels

Exemple : S'arrêter seulement quand i = 100
```
Condition : i = 100  
Hit Count : Ignorer les 5 premiers passages  
Action : Logger un message sans arrêter
```

#### Expressions de surveillance complexes

```
Watches possibles :
├── Variables simples : X, Y, Count
├── Expressions : X + Y * 2
├── Propriétés : Form1.Caption
├── Tableaux : MonTableau[i]
├── Pointeurs : P^.Next^.Value
└── Casts : TButton(Sender).Caption
```

### Préparation au débogage efficace

#### Configuration du projet

1. **Mode de compilation Debug :**
   ```
   Projet → Options → Mode de compilation → Debug
   ```

2. **Informations de débogage :**
   ```
   ☑ Générer infos de débogage (-g)
   ☑ Utiliser symboles externes (-Xg)
   ☑ Afficher numéros de ligne (-gl)
   ☑ Utiliser Heaptrc (détection fuites) (-gh)
   ```

3. **Optimisations désactivées :**
   ```
   Niveau : 0 (Aucune optimisation)
   // Les optimisations peuvent réorganiser le code
   ```

#### Organisation de l'espace de travail

Configuration recommandée des fenêtres pour le débogage :

```
┌──────────────────────────────────────────┐
│          Éditeur de code                 │
│          (Point actuel surligné)         │
├─────────────┬────────────────────────────┤
│  Variables  │      Watches               │
│   Locales   │   (Expressions surveillées)│
├─────────────┼────────────────────────────┤
│   Call      │     Breakpoints            │
│   Stack     │   (Points d'arrêt)         │
├─────────────┴────────────────────────────┤
│        Console de débogage               │
│      (Messages et sorties)               │
└──────────────────────────────────────────┘
```

### Techniques de débogage essentielles

#### 1. La méthode scientifique

```
Processus de débogage :
1. Observer le symptôme
2. Former une hypothèse
3. Prédire où regarder
4. Tester avec le débogueur
5. Analyser les résultats
6. Répéter si nécessaire
```

#### 2. Isolation du problème

- **Diviser pour régner** : Réduire la zone de recherche
- **Bissection** : Commenter la moitié du code problématique
- **Simplification** : Créer un cas de test minimal

#### 3. Points d'arrêt stratégiques

```pascal
// Au lieu de mettre des points d'arrêt partout :
procedure Complex;  
begin
  // Point d'arrêt ici pour vérifier l'entrée
  if not ValidateInput then Exit;

  ProcessData;

  // Point d'arrêt ici pour vérifier le résultat
  if not ValidateOutput then
    raise Exception.Create('Sortie invalide');
end;
```

### Erreurs courantes et leur débogage

#### Violation d'accès (Access Violation)

**Symptôme :** "Access violation at address..."

**Causes fréquentes :**
- Pointeur nil déréférencé
- Objet libéré utilisé
- Dépassement de tableau

**Technique de débogage :**
```pascal
// Activer la vérification des pointeurs
{$CHECKPOINTER ON}
// Utiliser les assertions
Assert(Assigned(MonObjet), 'Objet non créé');
```

#### Fuite mémoire

**Symptôme :** Mémoire qui augmente continuellement

**Outil :** HeapTrc
```
Options du projet → Compilation →
☑ Utiliser Heaptrc (détection fuites mémoire)
```

#### Boucle infinie

**Symptôme :** Programme qui ne répond plus

**Technique :**
1. Pause pendant l'exécution (Pause/Break)
2. Examiner la pile d'appels
3. Identifier la boucle
4. Ajouter un compteur avec point d'arrêt conditionnel

### Optimisation du workflow de débogage

#### Raccourcis essentiels

| Raccourci | Action | Utilisation |
|-----------|--------|-------------|
| **F5** | Toggle Breakpoint | Ajouter/Retirer point d'arrêt |
| **F7** | Step Into | Entrer dans la fonction |
| **F8** | Step Over | Ligne suivante |
| **F9** | Run | Lancer/Continuer |
| **Ctrl+F2** | Stop | Arrêter le débogage |
| **Ctrl+F7** | Evaluate | Évaluer expression |
| **Ctrl+F5** | Add Watch | Surveiller variable |
| **F4** | Run to Cursor | Exécuter jusqu'au curseur |

#### Configuration des fenêtres de débogage

**Menu Affichage → Fenêtres de débogage :**
- Variables locales (Ctrl+Alt+L)
- Watches (Ctrl+Alt+W)
- Pile d'appels (Ctrl+Alt+S)
- Points d'arrêt (Ctrl+Alt+B)
- Assembleur (Ctrl+Alt+D)
- Registres (Ctrl+Alt+R)
- Historique (Ctrl+Alt+H)

### Comparaison des débogueurs

| Caractéristique | GDB | FpDebug | LLDB |
|----------------|-----|---------|------|
| **Maturité** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Performance** | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Support Pascal** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Windows** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐ |
| **Linux** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| **macOS** | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Configuration** | ⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Documentation** | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐ |

### Choisir le bon débogueur

#### Utilisez GDB si :
- Vous avez besoin de stabilité maximale
- Vous déboguez du code mixte Pascal/C
- Vous travaillez sur Linux principalement
- Vous avez besoin de fonctionnalités avancées

#### Utilisez FpDebug si :
- Vous voulez les meilleures performances
- Vous travaillez uniquement en Pascal
- Vous préférez la simplicité de configuration
- Vous êtes sur Windows

#### Utilisez LLDB si :
- Vous développez sur macOS
- Vous avez besoin d'une API moderne
- Vous voulez des scripts Python
- Vous travaillez avec LLVM/Clang

### Préparer votre environnement

Avant de plonger dans la configuration spécifique de chaque plateforme, assurez-vous d'avoir :

1. **Lazarus installé** avec les outils de débogage
2. **Un projet de test** simple pour expérimenter
3. **Les privilèges administrateur** si nécessaire (surtout Windows)
4. **La documentation** à portée de main

### Ce qui vous attend

Dans les sections suivantes, nous allons explorer en détail :

- **2.5.1** : Configuration spécifique de GDB sur Windows
- **2.5.2** : Configuration spécifique de GDB sur Ubuntu/Linux
- **2.5.3** : Mise en place et utilisation des alternatives (LLDB, FpDebug)

Chaque section contiendra des instructions pas à pas, des solutions aux problèmes courants, et des conseils d'optimisation spécifiques à chaque plateforme.

Le débogage est une compétence essentielle qui distingue les développeurs amateurs des professionnels. Maîtriser ces outils transformera votre façon de développer et vous fera gagner un temps précieux dans la résolution des problèmes.

⏭️ [Configuration GDB sur Windows](/02-maitrise-ide-lazarus/05.1-configuration-gdb-windows.md)
