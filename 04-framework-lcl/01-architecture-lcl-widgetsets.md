🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.1 Architecture de la LCL et widgetsets

## Introduction pour les débutants

Imaginez que vous voulez créer une application qui fonctionne à la fois sur Windows et Linux. Le problème, c'est que ces systèmes d'exploitation "parlent" des langages différents pour créer des fenêtres et des boutons. C'est là que la LCL intervient : elle agit comme un traducteur universel qui permet à votre code de fonctionner partout.

## Qu'est-ce que l'architecture de la LCL ?

### Le concept de base

La LCL (Lazarus Component Library) est organisée comme un bâtiment à plusieurs étages :

**L'étage supérieur (ce que vous voyez)** : C'est là où vous travaillez quotidiennement. Vous placez des boutons, des zones de texte, des menus. Vous écrivez du code comme :
```pascal
Button1.Caption := 'Cliquez-moi';
Edit1.Text := 'Bonjour';
```

**L'étage intermédiaire (la traduction)** : C'est le widgetset, le système qui traduit vos commandes en instructions que chaque système d'exploitation comprend.

**L'étage inférieur (le système)** : C'est le système d'exploitation lui-même (Windows, Linux, macOS) qui dessine réellement les fenêtres sur votre écran.

### Pourquoi cette architecture en couches ?

Cette organisation en couches présente plusieurs avantages majeurs :

1. **Vous écrivez le code une seule fois** : Votre code reste identique, que vous compiliez pour Windows ou Linux
2. **L'application reste native** : Sur Windows, elle ressemble à une vraie application Windows ; sur Linux, à une vraie application Linux
3. **Les performances restent optimales** : Pas d'émulation, c'est le système qui fait le travail

## Comprendre les Widgetsets

### Qu'est-ce qu'un widgetset ?

Le terme "widget" désigne un élément d'interface (bouton, fenêtre, menu...). Un "widgetset" est donc l'ensemble du système qui gère ces éléments.

Pensez au widgetset comme à un adaptateur universel. Quand vous voyagez à l'étranger, vous utilisez un adaptateur pour brancher vos appareils. Le widgetset fait la même chose : il adapte votre code aux "prises" de chaque système d'exploitation.

### Les principaux widgetsets disponibles

#### Win32/Win64 (Windows)
C'est le widgetset pour Windows. Il communique directement avec Windows en utilisant l'API Win32 (pour Windows 32 bits) ou Win64 (pour Windows 64 bits).

**Caractéristiques** :
- Utilise les contrôles natifs de Windows
- Support complet des thèmes Windows
- Intégration parfaite avec l'explorateur et le système
- Le plus stable et complet pour Windows

**Quand l'utiliser** : Toujours sur Windows, sauf cas très particuliers.

#### GTK2 (Linux/Unix)
GTK (GIMP Toolkit) version 2 est une bibliothèque graphique très populaire sur Linux. C'est le widgetset par défaut sur Ubuntu et beaucoup d'autres distributions Linux.

**Caractéristiques** :
- Mature et stable
- Bien intégré avec GNOME et XFCE
- Léger et rapide
- Large compatibilité avec les anciennes distributions

**Quand l'utiliser** : Pour une compatibilité maximale sur Linux, notamment avec les systèmes plus anciens.

#### GTK3 (Linux/Unix moderne)
La version moderne de GTK, avec support des fonctionnalités récentes.

**Caractéristiques** :
- Interface moderne avec animations
- Support natif du HiDPI (écrans haute résolution)
- Meilleure intégration avec GNOME 3+
- Thèmes modernes et dark mode

**Quand l'utiliser** : Pour les applications Linux modernes ciblant les distributions récentes.

#### Qt5 (Multi-plateforme)
Qt est un autre framework graphique populaire, particulièrement dans l'environnement KDE.

**Caractéristiques** :
- Excellent rendu sur KDE Plasma
- Fonctionne aussi sur Windows et macOS
- Look moderne et cohérent
- Bon support du HiDPI

**Quand l'utiliser** : Pour les applications KDE ou quand vous voulez une apparence très cohérente entre plateformes.

#### Carbon (macOS ancien)
L'ancien widgetset pour macOS, maintenant déprécié.

**Note** : Apple a arrêté le support de Carbon. Ne l'utilisez que pour maintenir d'anciennes applications.

#### Cocoa (macOS moderne)
Le widgetset moderne pour macOS, utilisant l'API Cocoa d'Apple.

**Caractéristiques** :
- Support complet de macOS moderne
- Intégration native parfaite
- Support de toutes les fonctionnalités macOS
- Compatible avec les Mac Apple Silicon (M1/M2)

**Quand l'utiliser** : Toujours pour les nouvelles applications macOS.

#### Custom Drawn (Expérimental)
Un widgetset spécial qui dessine tout lui-même sans utiliser les contrôles du système.

**Caractéristiques** :
- Apparence 100% identique sur toutes les plateformes
- Utile pour les interfaces très personnalisées
- Plus lent que les widgetsets natifs
- Encore en développement

**Quand l'utiliser** : Pour des besoins très spécifiques où l'apparence doit être absolument identique partout.

## Comment fonctionne la traduction ?

### Un exemple concret : créer un bouton

Voyons ce qui se passe quand vous créez un bouton dans votre application :

```pascal
// Votre code (identique sur toutes les plateformes)
MonBouton := TButton.Create(Self);
MonBouton.Caption := 'Cliquer ici';
MonBouton.Left := 100;
MonBouton.Top := 50;
MonBouton.Parent := Form1;
```

**Sur Windows (widgetset Win32)** :
1. La LCL appelle le widgetset Win32
2. Le widgetset crée un bouton Windows avec `CreateWindow('BUTTON', ...)`
3. Windows dessine un vrai bouton Windows

**Sur Linux (widgetset GTK2)** :
1. La LCL appelle le widgetset GTK2
2. Le widgetset crée un bouton GTK avec `gtk_button_new()`
3. GTK dessine un bouton avec le thème Linux actuel

**Le résultat** : Votre code est identique, mais le bouton s'affiche dans le style natif de chaque système !

### La gestion des événements

Les événements (clics, frappes clavier...) suivent le chemin inverse :

1. L'utilisateur clique sur le bouton
2. Le système d'exploitation détecte le clic
3. Le widgetset reçoit l'information du système
4. Le widgetset traduit en événement LCL
5. Votre procédure `OnClick` est appelée

## Les classes importantes de l'architecture

### TWinControl
C'est la classe de base pour tous les contrôles qui ont une "fenêtre" système (handle). Tous les contrôles visuels en héritent.

```pascal
// TWinControl sait comment :
// - Se dessiner à l'écran
// - Recevoir les événements souris/clavier
// - Contenir d'autres contrôles
```

### TWSxxx (Classes Widgetset)
Pour chaque composant, il existe une classe "WS" (WidgetSet) qui fait la traduction :

- `TButton` → `TWSButton` → Implémentation spécifique (Win32, GTK, etc.)
- `TEdit` → `TWSEdit` → Implémentation spécifique
- `TForm` → `TWSForm` → Implémentation spécifique

### Les interfaces widgetset

Chaque widgetset implémente des interfaces communes :
```pascal
// Pseudo-code simplifié
IWidgetSet = interface
  function CreateButton: THandle;
  function CreateWindow: THandle;
  procedure DestroyWidget(Handle: THandle);
  // ... beaucoup d'autres méthodes
end;
```

## Choisir le bon widgetset

### Critères de sélection

**Pour une application Windows uniquement** :
- Utilisez Win32/Win64, c'est le choix évident

**Pour une application Linux uniquement** :
- GTK2 pour la compatibilité maximale
- GTK3 pour les fonctionnalités modernes
- Qt5 si vous ciblez KDE

**Pour une application vraiment multi-plateforme** :
- Utilisez le widgetset natif de chaque plateforme
- Testez sur chaque système cible
- Gérez les petites différences avec la compilation conditionnelle

### Comment spécifier le widgetset

Dans Lazarus, vous pouvez choisir le widgetset de plusieurs façons :

**Dans les options du projet** :
1. Menu Projet → Options du projet
2. Onglet "Additions et Overrides"
3. Définir LCLWidgetType (gtk2, qt5, win32, etc.)

**En ligne de commande** :
```bash
# Compiler pour GTK2
lazbuild MonProjet.lpi --ws=gtk2

# Compiler pour Qt5
lazbuild MonProjet.lpi --ws=qt5
```

**Dans le code source** (pour adapter le comportement, pas pour sélectionner le widgetset) :
```pascal
uses InterfaceBase;

// Le widgetset est choisi à la compilation (--ws=xxx),
// mais vous pouvez adapter le comportement à l'exécution :
case WidgetSet.LCLPlatform of
  lpGtk2, lpGtk3: WriteLn('Interface GTK');
  lpQT5:          WriteLn('Interface Qt5');
  lpWin32:        WriteLn('Interface Windows');
end;
```

## Gérer les différences entre widgetsets

### Les différences communes

Même si la LCL fait un excellent travail d'abstraction, certaines différences subsistent :

**Tailles et positions** :
- Un bouton peut être légèrement plus grand sur un système que sur l'autre
- Les polices par défaut varient

**Comportements** :
- L'ordre de tabulation peut différer
- Certains raccourcis clavier sont spécifiques à un OS

**Fonctionnalités** :
- Certaines propriétés peuvent ne pas être supportées partout
- Des effets visuels peuvent varier

### Techniques pour gérer ces différences

**Utiliser des layouts flexibles** :
```pascal
// Au lieu de positions fixes
Button1.Left := 100;

// Préférez l'alignement et les ancres
Button1.Align := alTop;
Button1.BorderSpacing.Around := 8;
```

**Tester le widgetset actuel** :
```pascal
uses InterfaceBase;

if WidgetSet.LCLPlatform = lpGtk2 then
begin
  // Code spécifique GTK2
end
else if WidgetSet.LCLPlatform = lpWin32 then
begin
  // Code spécifique Windows
end;
```

**Utiliser la compilation conditionnelle** :
```pascal
{$IFDEF WINDOWS}
  // Code uniquement pour Windows
  MonEdit.Height := 23;
{$ENDIF}

{$IFDEF LINUX}
  // Code uniquement pour Linux
  MonEdit.Height := 27;
{$ENDIF}
```

## Architecture interne détaillée

### Le cycle de vie d'un composant

1. **Création** : Allocation mémoire et initialisation Pascal
2. **Réalisation** : Création du widget natif via le widgetset
3. **Vie** : Interactions, événements, modifications
4. **Destruction** : Libération du widget natif puis de la mémoire

### La communication bidirectionnelle

La LCL maintient une synchronisation constante entre vos objets Pascal et les widgets natifs :

**Sens Pascal → Système** :
```pascal
Button1.Caption := 'Nouveau texte';
// 1. La propriété Caption est modifiée
// 2. SetCaption est appelé
// 3. Le widgetset est notifié
// 4. Le texte du bouton natif est changé
```

**Sens Système → Pascal** :
```pascal
// L'utilisateur redimensionne la fenêtre
// 1. Le système envoie un message de redimensionnement
// 2. Le widgetset reçoit le message
// 3. Les propriétés Width/Height sont mises à jour
// 4. L'événement OnResize est déclenché
```

## Optimisations et performances

### Comment la LCL reste rapide

**Mise en cache** : Les propriétés fréquemment lues sont cachées côté Pascal pour éviter les appels système coûteux.

**Mise à jour différée** : Plusieurs modifications successives sont regroupées en une seule mise à jour système.

**Création paresseuse** : Les widgets natifs ne sont créés que quand c'est vraiment nécessaire (quand le contrôle devient visible).

### Impact sur votre code

Pour des performances optimales :

```pascal
// Mauvais : provoque plusieurs rafraîchissements
Button1.Left := 10;
Button1.Top := 20;
Button1.Width := 100;
Button1.Height := 30;

// Bon : un seul rafraîchissement
Button1.SetBounds(10, 20, 100, 30);

// Ou utiliser BeginUpdate/EndUpdate pour les modifications multiples
ListView1.BeginUpdate;
try
  // Plusieurs modifications...
finally
  ListView1.EndUpdate;
end;
```

## Debugging et diagnostic

### Comprendre ce qui se passe

Pour déboguer les problèmes de widgetset :

**Activer les messages de débogage** :
```pascal
// Dans votre code
DebugLn('Mon message de debug');

// Compiler avec -dDEBUG_VERBOSE pour plus de détails
```

**Vérifier le widgetset actif** :
```pascal
ShowMessage('Widgetset: ' + WidgetSet.ClassName);
ShowMessage('Platform: ' + IntToStr(Ord(WidgetSet.LCLPlatform)));
```

## Points clés à retenir

1. **La LCL est organisée en couches** pour permettre la portabilité
2. **Les widgetsets sont les traducteurs** entre votre code et le système
3. **Chaque plateforme a son widgetset optimal** (Win32 pour Windows, GTK2/3 pour Linux, Cocoa pour macOS)
4. **Votre code reste identique**, le widgetset fait la traduction
5. **Les contrôles sont natifs**, pas émulés, d'où les bonnes performances
6. **Des différences mineures existent** entre plateformes, mais sont gérables
7. **L'architecture est optimisée** pour minimiser les appels système

Cette architecture en couches est ce qui rend Lazarus si puissant pour le développement multi-plateforme. Vous écrivez une fois, et votre application fonctionne partout avec une apparence native !

⏭️ [Composants visuels fondamentaux](/04-framework-lcl/02-composants-visuels-fondamentaux.md)
