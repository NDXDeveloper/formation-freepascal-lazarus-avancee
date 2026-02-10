🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23. Développement de Jeux avec FreePascal

## Introduction

Le développement de jeux vidéo avec FreePascal et Lazarus est une aventure passionnante qui combine créativité, logique et compétences techniques. Contrairement à l'idée reçue que Pascal serait obsolète pour les jeux, l'écosystème FreePascal offre des outils matures, performants et parfaitement adaptés à la création de jeux 2D et 3D multiplateformes.

### Pourquoi FreePascal pour les jeux ?

#### Avantages techniques

**Performance native**
- Compilation en code machine natif (pas de machine virtuelle)
- Performance comparable au C/C++
- Optimisations avancées du compilateur FPC
- Accès direct au matériel et aux API système

**Multiplateforme par nature**
- Un seul code source pour Windows, Linux, macOS
- Support Android et iOS
- Déploiement sur Raspberry Pi et systèmes embarqués
- Pas de frais de licence pour la cross-compilation

**Stabilité et fiabilité**
- Typage fort qui réduit les bugs
- Gestion mémoire explicite et contrôlée
- Pas de garbage collector imprévisible
- Débogage facilité par la clarté du code

**Écosystème mature**
- Bibliothèques graphiques éprouvées (OpenGL, SDL, etc.)
- Frameworks de jeux complets et gratuits
- Communauté active et réactive
- Documentation abondante

#### Avantages pratiques

**Courbe d'apprentissage douce**
- Syntaxe claire et lisible
- Concept de programmation structurée
- Transition facile vers la POO (Programmation Orientée Objet)
- Moins de pièges que C/C++

**Développement rapide**
- IDE Lazarus puissant et gratuit
- Compilation ultra-rapide
- Débogueur intégré efficace
- Pas de temps d'attente entre modifications

**Coût zéro**
- Pas de licence à payer
- Pas d'abonnement mensuel
- Distribution libre de vos jeux
- Open source : vous contrôlez tout

### L'écosystème du jeu vidéo en FreePascal

#### Moteurs et frameworks disponibles

**Pour les jeux 3D**
- **Castle Game Engine** : Moteur complet avec éditeur visuel
- **GLScene** : Bibliothèque 3D orientée composants
- **Bindings OpenGL** : Accès direct à OpenGL moderne
- **Vulkan bindings** : Pour le rendu haute performance

**Pour les jeux 2D**
- **ZenGL** : Framework léger et rapide
- **SDL2** : Bibliothèque multiplateforme éprouvée
- **Allegro** : API élégante et performante
- **Castle Engine 2D** : Support 2D et 3D hybride

**Bibliothèques complémentaires**
- **Box2D** : Physique 2D réaliste
- **Chipmunk** : Alternative à Box2D
- **OpenAL** : Audio 3D spatial
- **Newton Game Dynamics** : Physique 3D

#### Types de jeux réalisables

**Jeux 2D**
- Platformers (Mario-like, Metroidvania)
- Jeux d'arcade (shoot'em up, puzzle)
- RPG 2D (vue du dessus, isométrique)
- Jeux de gestion et stratégie
- Visual novels et jeux narratifs
- Jeux éducatifs

**Jeux 3D**
- FPS (First Person Shooter)
- TPS (Third Person Shooter)
- Jeux d'aventure 3D
- Simulateurs (vol, conduite, etc.)
- RPG 3D
- Jeux de stratégie 3D

**Jeux mobiles**
- Casual games
- Endless runners
- Puzzle games
- Jeux d'arcade adaptés au tactile

**Jeux web**
- Avec Pas2JS (transpilation vers JavaScript)
- WebAssembly avec FPC
- Jeux en navigateur multiplateformes

### Concepts fondamentaux du développement de jeux

#### La boucle de jeu (Game Loop)

Le cœur de tout jeu vidéo est sa boucle principale qui s'exécute en continu :

```pascal
program SimpleGameLoop;

var
  Running: Boolean;
  LastTime, CurrentTime, DeltaTime: Double;

procedure Initialize;  
begin
  // Initialiser les ressources, charger les assets
  LoadTextures;
  LoadSounds;
  InitializeEntities;
end;

procedure ProcessInput;  
begin
  // Gérer les entrées clavier, souris, manette
  if KeyPressed(KEY_ESCAPE) then
    Running := False;

  if KeyPressed(KEY_SPACE) then
    PlayerJump;
end;

procedure Update(dt: Double);  
begin
  // Mettre à jour la logique du jeu
  UpdatePlayer(dt);
  UpdateEnemies(dt);
  UpdatePhysics(dt);
  CheckCollisions;
  UpdateAnimations(dt);
end;

procedure Render;  
begin
  // Dessiner tout à l'écran
  ClearScreen;
  DrawBackground;
  DrawEntities;
  DrawUI;
  SwapBuffers;  // Afficher le frame
end;

procedure Cleanup;  
begin
  // Libérer les ressources
  FreeTextures;
  FreeSounds;
  CloseWindow;
end;

begin
  Initialize;
  Running := True;
  LastTime := GetTime;

  // BOUCLE PRINCIPALE
  while Running do
  begin
    // Calculer le temps écoulé (Delta Time)
    CurrentTime := GetTime;
    DeltaTime := CurrentTime - LastTime;
    LastTime := CurrentTime;

    ProcessInput;
    Update(DeltaTime);
    Render;
  end;

  Cleanup;
end.
```

**Composants essentiels** :
1. **Delta Time (dt)** : Temps écoulé depuis la dernière frame
2. **Input Processing** : Capture des actions du joueur
3. **Update** : Mise à jour de la logique du jeu
4. **Render** : Dessin à l'écran
5. **Frame Rate** : Nombre d'images par seconde (FPS)

#### Le Delta Time expliqué

Le Delta Time est crucial pour un jeu fluide sur différentes machines :

```pascal
// MAUVAIS : Vitesse dépend du framerate
procedure UpdateWrong;  
begin
  PlayerX := PlayerX + 5;  // 5 pixels par frame
  // Sur un PC rapide (120 FPS) : 600 pixels/seconde
  // Sur un PC lent (30 FPS) : 150 pixels/seconde
end;

// BON : Vitesse constante indépendante du framerate
procedure UpdateCorrect(dt: Double);  
const
  SPEED = 200;  // pixels par seconde
begin
  PlayerX := PlayerX + SPEED * dt;
  // Sur tous les PC : 200 pixels/seconde
end;
```

#### Gestion des états (State Management)

Un jeu est composé de plusieurs états (écrans) :

```
Menu Principal
    ↓
Sélection Niveau
    ↓
Jeu en cours ←→ Pause
    ↓
Game Over / Victoire
    ↓
Tableau des scores
    ↓
Retour Menu
```

**Implémentation de base** :

```pascal
type
  TGameState = (gsMenu, gsPlaying, gsPaused, gsGameOver);

var
  CurrentState: TGameState;

procedure Update(dt: Double);  
begin
  case CurrentState of
    gsMenu:
      UpdateMenu(dt);
    gsPlaying:
      UpdateGame(dt);
    gsPaused:
      UpdatePause(dt);
    gsGameOver:
      UpdateGameOver(dt);
  end;
end;

procedure Render;  
begin
  case CurrentState of
    gsMenu:
      DrawMenu;
    gsPlaying:
      DrawGame;
    gsPaused:
    begin
      DrawGame;      // Dessiner le jeu en arrière-plan
      DrawPauseMenu; // Menu de pause par-dessus
    end;
    gsGameOver:
      DrawGameOver;
  end;
end;
```

### Architecture d'un jeu

#### Organisation du code

```
MonJeu/
├── src/
│   ├── Main.pas              # Point d'entrée
│   ├── Game/
│   │   ├── GameLoop.pas      # Boucle principale
│   │   ├── GameState.pas     # Gestion des états
│   │   └── GameConfig.pas    # Configuration globale
│   ├── Entities/
│   │   ├── Entity.pas        # Classe de base
│   │   ├── Player.pas        # Joueur
│   │   ├── Enemy.pas         # Ennemis
│   │   └── Projectile.pas    # Projectiles
│   ├── Systems/
│   │   ├── Renderer.pas      # Système de rendu
│   │   ├── Physics.pas       # Physique et collisions
│   │   ├── Input.pas         # Gestion des entrées
│   │   └── Audio.pas         # Système audio
│   ├── Resources/
│   │   ├── TextureManager.pas
│   │   ├── SoundManager.pas
│   │   └── FontManager.pas
│   └── Utils/
│       ├── Math2D.pas        # Fonctions mathématiques
│       └── Types.pas         # Types personnalisés
└── data/
    ├── textures/
    ├── sounds/
    ├── music/
    └── levels/
```

#### Design patterns utiles

**1. Component Pattern (Composition)**

Au lieu d'hériter de classes complexes, on compose des entités :

```pascal
type
  TEntity = class
  private
    FComponents: TComponentList;
  public
    procedure AddComponent(Component: TComponent);
    function GetComponent(ComponentType: TClass): TComponent;
    procedure Update(dt: Double);
  end;

  TTransformComponent = class(TComponent)
    X, Y: Single;
    Rotation: Single;
  end;

  TSpriteComponent = class(TComponent)
    Texture: TTexture;
    Width, Height: Integer;
  end;

  TPhysicsComponent = class(TComponent)
    VelocityX, VelocityY: Single;
    Mass: Single;
  end;

// Créer une entité
var
  Player: TEntity;
begin
  Player := TEntity.Create;
  Player.AddComponent(TTransformComponent.Create);
  Player.AddComponent(TSpriteComponent.Create);
  Player.AddComponent(TPhysicsComponent.Create);
end;
```

**2. Object Pool Pattern**

Réutiliser les objets au lieu de les créer/détruire :

```pascal
type
  TBulletPool = class
  private
    FBullets: array[0..99] of TBullet;
  public
    function GetBullet: TBullet;
    procedure ReturnBullet(Bullet: TBullet);
  end;
```

**3. Observer Pattern**

Pour les événements du jeu :

```pascal
type
  IGameEventListener = interface
    procedure OnPlayerDied;
    procedure OnEnemyKilled(Points: Integer);
    procedure OnLevelComplete;
  end;

  TGameEventManager = class
  private
    FListeners: TList;
  public
    procedure Subscribe(Listener: IGameEventListener);
    procedure NotifyPlayerDied;
  end;
```

### Ressources et assets

#### Types de ressources

**Graphiques**
- **Sprites** : Images 2D (PNG, TGA avec transparence)
- **Sprite sheets** : Multiples sprites dans une image
- **Textures** : Pour objets 3D
- **Tilesets** : Tuiles pour créer des niveaux
- **Polices bitmap** : Texte stylisé

**Audio**
- **Effets sonores** : Courts (< 2 secondes)
  - Formats : WAV (non compressé), OGG (compressé)
- **Musiques** : Longues, en streaming
  - Formats : OGG, MP3, MOD, XM

**Données**
- **Niveaux** : JSON, XML, formats propriétaires
- **Dialogues** : Fichiers texte, JSON
- **Configuration** : INI, JSON, YAML

#### Gestion de la mémoire

**Chargement des ressources** :

```pascal
type
  TResourceManager = class
  private
    FTextures: TDictionary<string, TTexture>;
    FSounds: TDictionary<string, TSound>;
  public
    function LoadTexture(const FileName: string): TTexture;
    function GetTexture(const Name: string): TTexture;
    procedure UnloadAll;
  end;

// Utilisation
var
  ResMgr: TResourceManager;
  PlayerTexture: TTexture;
begin
  ResMgr := TResourceManager.Create;

  // Charger une fois
  ResMgr.LoadTexture('player.png');

  // Utiliser plusieurs fois
  PlayerTexture := ResMgr.GetTexture('player.png');

  // Libérer à la fin
  ResMgr.UnloadAll;
end;
```

### Performance et optimisation

#### Règles d'or

1. **Ne pas optimiser prématurément**
   - Faites fonctionner d'abord
   - Mesurez ensuite
   - Optimisez ce qui est lent

2. **Profiling avant optimisation**
   ```pascal
   var
     StartTime, EndTime: Int64;
   begin
     StartTime := GetTickCount64;

     // Code à mesurer
     UpdateEnemies;

     EndTime := GetTickCount64;
     WriteLn('UpdateEnemies: ', EndTime - StartTime, ' ms');
   end;
   ```

3. **Objectifs de performance**
   - **60 FPS** = 16.67 ms par frame (idéal)
   - **30 FPS** = 33.33 ms par frame (minimum)
   - **< 100 ms** de latence input (impératif)

#### Techniques d'optimisation courantes

**Culling (élimination des objets hors écran)**
```pascal
function IsVisible(X, Y, W, H: Single): Boolean;  
begin
  Result := (X + W >= CameraX) and
            (X <= CameraX + ScreenWidth) and
            (Y + H >= CameraY) and
            (Y <= CameraY + ScreenHeight);
end;
```

**Spatial partitioning**
- Quadtree pour grandes cartes 2D
- Octree pour mondes 3D
- Grid simple pour commencer

**Batch rendering**
- Regrouper les appels de dessin
- Minimiser les changements de texture
- Utiliser les sprite batchers

### Mathématiques pour les jeux

#### Vecteurs 2D

```pascal
type
  TVector2 = record
    X, Y: Single;

    class operator Add(const A, B: TVector2): TVector2;
    class operator Subtract(const A, B: TVector2): TVector2;
    class operator Multiply(const V: TVector2; S: Single): TVector2;

    function Length: Single;
    function Normalize: TVector2;
    function Dot(const Other: TVector2): Single;
  end;

// Utilisation
var
  Position, Velocity: TVector2;
  dt: Double;
begin
  // Mouvement
  Position := Position + Velocity * dt;

  // Distance entre deux points
  Distance := (TargetPos - Position).Length;

  // Direction normalisée
  Direction := (TargetPos - Position).Normalize;
end;
```

#### Fonctions utiles

```pascal
// Interpolation linéaire
function Lerp(A, B, T: Single): Single;  
begin
  Result := A + (B - A) * T;
end;

// Limiter une valeur
function Clamp(Value, Min, Max: Single): Single;  
begin
  if Value < Min then Result := Min
  else if Value > Max then Result := Max
  else Result := Value;
end;

// Angle entre deux points
function AngleBetween(X1, Y1, X2, Y2: Single): Single;  
begin
  Result := ArcTan2(Y2 - Y1, X2 - X1);
end;
```

### Outils de développement

#### Éditeurs graphiques

**Gratuits**
- **GIMP** : Édition d'images (Windows/Linux)
- **Krita** : Peinture digitale
- **Aseprite** : Pixel art (payant mais excellent)
- **Piskel** : Pixel art en ligne (gratuit)

**Payants**
- **Photoshop** : Standard professionnel
- **Affinity Photo** : Alternative à Photoshop

#### Éditeurs de niveaux

- **Tiled** : Éditeur de tilemaps (gratuit, open source)
- **LDtk** : Level designer moderne (gratuit)
- **Ogmo Editor** : Simple et efficace

#### Audio

- **Audacity** : Édition audio (gratuit)
- **LMMS** : Création musicale (gratuit)
- **Bfxr** : Générateur d'effets sonores 8-bit
- **ChipTone** : Effets sonores retro

#### Outils de test

- **Fraps** : Capture FPS (Windows)
- **OBS Studio** : Enregistrement vidéo (gratuit)
- **MangoHUD** : Overlay FPS (Linux)

### Workflow de développement

#### Phase 1 : Prototype (1-2 semaines)

1. Concept de base
2. Mécaniques principales
3. Test de fun
4. Graphismes placeholder
5. Validation du gameplay

#### Phase 2 : Développement (2-6 mois)

1. Assets graphiques
2. Système audio
3. Niveaux/contenu
4. UI/UX
5. Polish et juice

#### Phase 3 : Test et débogage (2-4 semaines)

1. Tests internes
2. Beta testing
3. Corrections de bugs
4. Optimisation
5. Tests multiplateformes

#### Phase 4 : Publication

1. Packaging
2. Page de vente
3. Marketing
4. Lancement
5. Support post-lancement

### Aspects légaux et commerciaux

#### Licences des assets

- **CC0** : Domaine public, utilisable sans restriction
- **CC-BY** : Attribution requise
- **Licences commerciales** : Vérifier les termes

#### Protection de votre jeu

- **Copyright automatique** : Dès la création
- **Licence open source** : Si vous partagez le code
- **Conditions d'utilisation** : Définir clairement

#### Monétisation

- **Premium** : Achat unique
- **Freemium** : Gratuit avec achats in-app
- **Ads** : Publicités
- **Donations** : Financement participatif

### Communauté et ressources

#### Forums et communautés

- **Forum FreePascal** : Support technique
- **Lazarus Forum** : Questions sur l'IDE
- **Reddit r/gamedev** : Communauté générale
- **Discord FreePascal** : Chat en temps réel

#### Sites de ressources gratuites

**Graphics**
- OpenGameArt.org
- Itch.io (assets)
- Kenney.nl
- Game-icons.net

**Audio**
- Freesound.org
- OpenGameArt.org
- Incompetech.com
- Purple Planet Music

#### Game Jams

Excellents pour apprendre et pratiquer :
- **Ludum Dare** : Le plus ancien
- **Global Game Jam** : Événement mondial
- **GMTK Game Jam** : Thématique créative
- **Itch.io jams** : Nombreuses options

### Prochaines sections

Maintenant que vous comprenez les fondamentaux du développement de jeux avec FreePascal, nous allons explorer en détail :

- **23.1** : Castle Game Engine (moteur 3D complet)
- **23.2** : ZenGL et frameworks 2D
- **23.3** : Physics engines
- **23.4** : Audio multiplateforme
- **23.5** : Et bien plus encore...

Chaque section vous donnera les connaissances pratiques pour créer vos propres jeux, du prototype à la publication finale.

---

**Conseil final** : Commencez petit ! Ne visez pas le prochain AAA. Créez d'abord un Pong, puis un Snake, puis un petit platformer. Chaque projet terminé vous apprendra plus que dix projets abandonnés. Le plus important est de **terminer vos projets** et d'**itérer** sur vos idées.

Bon développement ! 🎮

⏭️ [Castle Game Engine](/23-developpement-jeux/01-castle-game-engine.md)
