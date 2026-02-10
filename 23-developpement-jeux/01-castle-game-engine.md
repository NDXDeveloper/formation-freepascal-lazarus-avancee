🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.1 Castle Game Engine

## Introduction

Castle Game Engine est un moteur de jeu 3D open source puissant et mature, spécialement conçu pour FreePascal et Lazarus. Il permet de créer des jeux 2D et 3D multiplateformes fonctionnant sur Windows, Linux, macOS, Android, iOS et même dans les navigateurs web.

### Pourquoi Castle Game Engine ?

- **Entièrement gratuit et open source** : Licence permissive (BSD/LGPL modifiée)
- **Multiplateforme** : Un seul code source pour toutes les plateformes
- **Puissant** : Support OpenGL moderne, physique 3D, audio spatial
- **Bien documenté** : Documentation complète et exemples nombreux
- **Actif** : Développement continu depuis 2006

## Installation

### Sur Windows

1. Téléchargez le package depuis [castle-engine.io](https://castle-engine.io)
2. Décompressez l'archive dans un dossier (exemple : `C:\castle-engine`)
3. Ajoutez le dossier `bin` au PATH Windows
4. Installez le package Lazarus :
   - Ouvrez Lazarus
   - Menu : **Package → Ouvrir un fichier de package**
   - Naviguez vers `castle-engine\packages\castle_components.lpk`
   - Cliquez sur **Compiler** puis **Utiliser → Installer**

### Sur Ubuntu/Linux

```bash
# Installation via APT (si disponible dans les dépôts)
sudo apt install castle-game-engine

# OU installation manuelle
cd ~/Downloads  
wget https://castle-engine.io/latest.zip  
unzip latest.zip -d ~/castle-engine  
cd ~/castle-engine
./compile.sh
```

Ajoutez au PATH dans `~/.bashrc` :
```bash
export PATH="$HOME/castle-engine/bin:$PATH"
```

## Premiers pas avec Castle Game Engine

### Structure d'un projet

Un projet Castle Game Engine typique contient :

```
MonJeu/
├── data/           # Ressources (modèles 3D, textures, sons)
├── code/           # Code source Pascal
├── CastleEngineManifest.xml  # Configuration du projet
└── MonJeu.lpi      # Projet Lazarus
```

### Créer votre premier projet

#### Via l'outil en ligne de commande

```bash
# Créer un nouveau projet 3D
castle-engine create-project --project=MyGame

# Compiler le projet
cd MyGame  
castle-engine compile

# Lancer le jeu
castle-engine run
```

#### Via Lazarus

1. Créez une nouvelle application Lazarus
2. Ajoutez la dépendance `castle_components` au projet
3. Ajoutez `CastleWindow` ou `CastleControl` sur votre formulaire
4. Commencez à coder !

## Concepts fondamentaux

### TCastleWindow vs TCastleControl

**TCastleWindow** : Fenêtre autonome gérée par Castle Engine
- Plus rapide et recommandé pour les jeux
- Ne dépend pas de la LCL

```pascal
program SimpleGame;  
uses CastleWindow, CastleScene, CastleViewport;

var
  Window: TCastleWindow;

begin
  Window := TCastleWindow.Create(Application);
  Window.Open;
  Application.Run;
end.
```

**TCastleControl** : Composant visuel intégrable dans une application LCL
- Utile pour les outils et éditeurs
- Peut être placé sur un formulaire Lazarus

### Le système de vues (Views)

Castle Engine utilise un système de vues pour organiser votre jeu :

```pascal
type
  TViewMain = class(TCastleView)
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
  end;

procedure TViewMain.Start;  
begin
  inherited;
  // Initialisation de la vue
end;

procedure TViewMain.Update(const SecondsPassed: Single;
                           var HandleInput: Boolean);
begin
  inherited;
  // Mise à jour à chaque frame
end;
```

### Hiérarchie de scène

Castle Engine utilise une hiérarchie de transformations :

```
TCastleViewport (la caméra et la vue 3D)
└── Items (TCastleTransform)
    ├── Scene1 (TCastleScene)
    ├── Scene2 (TCastleScene)
    └── Transform1
        └── ChildScene (TCastleScene)
```

## Charger et afficher un modèle 3D

### Formats supportés

Castle Engine supporte de nombreux formats :
- **glTF** (.gltf, .glb) - Recommandé, standard moderne
- **X3D** (.x3d, .x3dv) - Format natif très puissant
- **Collada** (.dae)
- **Wavefront** (.obj)
- **Spine** (pour l'animation 2D)
- Et bien d'autres...

### Code de base

```pascal
uses CastleViewport, CastleScene, CastleVectors, CastleTransform;

var
  Viewport: TCastleViewport;
  Scene: TCastleScene;

begin
  // Créer le viewport (vue 3D)
  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := True;
  Viewport.AutoCamera := True;
  Window.Controls.InsertFront(Viewport);

  // Charger et afficher un modèle 3D
  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/models/my_model.gltf');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := True; // Pour les animations

  Viewport.Items.Add(Scene);
end;
```

### Le préfixe `castle-data:/`

Castle Engine utilise un système d'URL pour accéder aux ressources :
- `castle-data:/` pointe vers le dossier `data/` de votre projet
- Cela fonctionne de manière identique sur toutes les plateformes

## Animation et interactions

### Lancer une animation

```pascal
// Lancer une animation par son nom
Scene.PlayAnimation('walk', paForceLooping);

// Arrêter toutes les animations
Scene.StopAnimation;

// Vérifier si une animation est en cours
if Scene.AnimationsList.Count > 0 then
  WriteLn('Animations disponibles : ', Scene.AnimationsList.Text);
```

### Détecter les clics sur les objets

```pascal
procedure TViewMain.Press(const Event: TInputPressRelease);  
var
  HitTransform: TCastleTransform;
begin
  inherited;

  if Event.IsMouseButton(buttonLeft) then
  begin
    HitTransform := Viewport.TransformUnderMouse;
    if HitTransform <> nil then
      WriteLn('Cliqué sur : ', HitTransform.Name);
  end;
end;
```

## Caméra et contrôles

### Types de caméra

**Navigation automatique** :
```pascal
Viewport.AutoCamera := True; // Castle Engine choisit automatiquement
```

**Navigation manuelle** :
```pascal
uses CastleWalkNavigation;

var
  Navigation: TCastleWalkNavigation;

Navigation := TCastleWalkNavigation.Create(Application);  
Navigation.Gravity := True; // Activation de la gravité  
Viewport.Navigation := Navigation;
```

**Types de navigation disponibles** :
- `TCastleWalkNavigation` : FPS (First Person Shooter)
- `TCastleExamineNavigation` : Orbite autour d'un objet
- `TCastle2DNavigation` : Pour les jeux 2D

## Audio et sons

Castle Engine intègre un système audio complet avec support du son 3D spatial.

### Jouer un son simple

```pascal
uses CastleSound;

// Son non-spatial (2D)
SoundEngine.LoadBuffer('castle-data:/sounds/click.wav');  
SoundEngine.Sound('castle-data:/sounds/click.wav');
```

### Son spatial 3D

```pascal
uses CastleSoundEngine;

var
  Sound: TCastleSound;

Sound := TCastleSound.Create(Application);  
Sound.URL := 'castle-data:/sounds/explosion.wav';  
Sound.Spatial := True;  
Sound.Position := Vector3(10, 0, 5); // Position dans l'espace 3D

SoundEngine.Play(Sound);
```

### Musique de fond

```pascal
// Musique en boucle
SoundEngine.LoopingChannel[0].URL := 'castle-data:/music/background.ogg';  
SoundEngine.LoopingChannel[0].Volume := 0.5; // 50% du volume
```

## Interface utilisateur (UI)

Castle Engine fournit des composants d'interface utilisateur modernes et personnalisables.

### Créer un menu simple

```pascal
uses CastleControls, CastleColors;

var
  ButtonStart: TCastleButton;
  LabelTitle: TCastleLabel;

// Titre
LabelTitle := TCastleLabel.Create(Application);  
LabelTitle.Caption := 'Mon Super Jeu';  
LabelTitle.Color := Yellow;  
LabelTitle.FontSize := 40;  
LabelTitle.Anchor(hpMiddle);  
LabelTitle.Anchor(vpTop, -100);  
Window.Controls.InsertFront(LabelTitle);

// Bouton
ButtonStart := TCastleButton.Create(Application);  
ButtonStart.Caption := 'Commencer';  
ButtonStart.OnClick := @ButtonStartClick;  
ButtonStart.Anchor(hpMiddle);  
ButtonStart.Anchor(vpMiddle);  
Window.Controls.InsertFront(ButtonStart);
```

### Gestion des événements

```pascal
procedure TViewMain.ButtonStartClick(Sender: TObject);  
begin
  // Changer de vue, démarrer le jeu, etc.
  Container.View := ViewGame;
end;
```

## Physique 3D

Castle Engine intègre un moteur physique complet avec détection de collisions et simulation dynamique.

### Activer la physique sur un objet

```pascal
uses CastleTransform;

var
  RigidBody: TCastleRigidBody;
  Collider: TCastleBoxCollider;

// Créer le corps physique
RigidBody := TCastleRigidBody.Create(Application);  
RigidBody.Dynamic := True; // Objet mobile  
RigidBody.Gravity := True;  
Scene.RigidBody := RigidBody;

// Ajouter un collider (forme de collision)
Collider := TCastleBoxCollider.Create(Application);  
Collider.Size := Vector3(2, 2, 2);  
RigidBody.Collider := Collider;
```

### Types de colliders

- `TCastleBoxCollider` : Boîte
- `TCastleSphereCollider` : Sphère
- `TCastlePlaneCollider` : Plan infini
- `TCastleMeshCollider` : Maillage complexe (mesh)
- `TCastleCapsuleCollider` : Capsule (pour personnages)

### Appliquer des forces

```pascal
// Impulsion instantanée
RigidBody.ApplyImpulse(Vector3(0, 10, 0), Vector3(0, 0, 0));

// Force continue
RigidBody.AddForce(Vector3(100, 0, 0));
```

## Systèmes de particules

Pour créer des effets visuels (feu, fumée, explosions, etc.).

```pascal
uses CastleTransform, CastleParticles;

var
  ParticleEffect: TCastleParticleEmitter;

ParticleEffect := TCastleParticleEmitter.Create(Application);  
ParticleEffect.Load('castle-data:/effects/explosion.castle-particle-emitter');  
ParticleEffect.Translation := Vector3(0, 5, 0);  
Viewport.Items.Add(ParticleEffect);
```

## Compilation multiplateforme

### Pour Windows (depuis Windows)

```bash
castle-engine compile --os=win64 --cpu=x86_64
```

### Pour Linux (depuis Windows ou Linux)

```bash
castle-engine compile --os=linux --cpu=x86_64
```

### Pour Android

```bash
# Configuration initiale
castle-engine auto-generate-textures  
castle-engine auto-generate-clean

# Compilation
castle-engine package --os=android --cpu=arm
```

### Configuration dans CastleEngineManifest.xml

```xml
<?xml version="1.0" encoding="utf-8"?>
<project name="MonJeu"
         game_units="GameInitialize"
         standalone_source="MonJeu.lpr">

  <package>
    <include path="code/" recursive="True" />
  </package>

  <compiler_options>
    <search_paths>
      <path value="code/" />
    </search_paths>
  </compiler_options>

  <icons>
    <icon path="data/icon.png" />
  </icons>

</project>
```

## Optimisation et bonnes pratiques

### Performance

1. **Utilisez le culling automatique** : Castle Engine élimine automatiquement les objets non visibles
2. **Optimisez vos modèles 3D** : Réduisez le nombre de polygones quand possible
3. **Utilisez des atlas de textures** : Regroupez plusieurs textures en une seule
4. **Activez le cache des shaders** : Réutilisation des programmes GPU

```pascal
// Activer le cache de scène
Scene.Cache := True;

// Préparer les ressources en avance
Scene.PrepareResources([prRenderSelf, prBoundingBox]);
```

### Organisation du code

```pascal
// Structure recommandée
program MyGame;

uses
  GameInitialize; // Unité principale d'initialisation

{$R *.res}

begin
  Application.MainWindow.OpenAndRun;
end.
```

```pascal
// GameInitialize.pas
unit GameInitialize;

interface

implementation

uses CastleWindow, CastleViewport, GameViewMain;

var
  Window: TCastleWindow;

initialization
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  Window.Container.View := ViewMain; // Vue principale
end.
```

## Ressources et documentation

### Documentation officielle

- Site officiel : [castle-engine.io](https://castle-engine.io)
- Manuel complet : [castle-engine.io/manual_intro.php](https://castle-engine.io/manual_intro.php)
- API Reference : [castle-engine.io/apidoc/html/](https://castle-engine.io/apidoc/html/)

### Exemples et tutoriels

Castle Engine inclut de nombreux exemples dans le dossier `examples/` :
- `3d_fps_game/` : Jeu FPS complet
- `2d_game/` : Jeu 2D plateforme
- `physics/` : Exemples de physique
- `3d_rendering_processing/` : Rendu avancé

### Communauté

- Forum : [castle-engine.io/forum](https://castle-engine.io/forum)
- Discord : Communauté active et réactive
- GitHub : [github.com/castle-engine/castle-engine](https://github.com/castle-engine/castle-engine)

## Conclusion

Castle Game Engine est un moteur mature et puissant qui permet de créer des jeux 3D professionnels avec FreePascal et Lazarus. Sa conception multiplateforme native et son intégration parfaite avec l'écosystème Pascal en font un excellent choix pour les développeurs Pascal souhaitant se lancer dans le développement de jeux.

Les concepts présentés dans ce tutoriel constituent les fondations nécessaires pour commencer à créer vos propres jeux. La documentation officielle et les nombreux exemples fournis vous permettront d'approfondir chaque aspect du moteur selon vos besoins spécifiques.

⏭️ [ZenGL et frameworks 2D](/23-developpement-jeux/02-zengl-frameworks-2d.md)
