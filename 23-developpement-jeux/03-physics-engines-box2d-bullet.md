🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.3 Physics Engines (Box2D, Bullet)

## Introduction aux moteurs physiques

Les moteurs physiques sont des bibliothèques qui simulent les lois de la physique réelle dans les jeux vidéo : gravité, collisions, friction, rebonds, etc. Au lieu de programmer manuellement ces comportements complexes, vous utilisez un moteur physique qui fait les calculs pour vous.

### Pourquoi utiliser un moteur physique ?

**Avantages**
- **Réalisme instantané** : Comportements physiques crédibles sans expertise
- **Gain de temps** : Des années de développement déjà faites
- **Stabilité** : Code testé par des milliers de développeurs
- **Performance** : Optimisations avancées incluses
- **Complexité gérée** : Collisions complexes simplifiées

**Cas d'utilisation**
- Jeux de puzzle physiques (Angry Birds, Cut the Rope)
- Jeux de plateforme avec physique réaliste
- Simulateurs (véhicules, destruction)
- Jeux de sport (billard, golf, basketball)
- Ragdoll physics (personnages morts)

### Box2D vs Bullet : Quel moteur choisir ?

| Critère | Box2D | Bullet |
|---------|-------|--------|
| **Dimensions** | 2D uniquement | 3D (et 2D possible) |
| **Facilité** | ⭐⭐⭐⭐⭐ Très simple | ⭐⭐⭐ Plus complexe |
| **Performance** | ⭐⭐⭐⭐⭐ Excellent | ⭐⭐⭐⭐ Très bon |
| **Documentation** | ⭐⭐⭐⭐⭐ Excellente | ⭐⭐⭐⭐ Bonne |
| **Usage** | Jeux 2D, puzzles | Jeux 3D, simulations |
| **Licence** | Zlib (très permissive) | Zlib (très permissive) |

**Recommandation** :
- **Jeu 2D** → Box2D
- **Jeu 3D** → Bullet
- **Débutant** → Box2D (plus simple)

## Box2D - Physique 2D

Box2D est le moteur physique 2D le plus populaire au monde, utilisé dans des milliers de jeux (Angry Birds, Limbo, Crayon Physics).

### Installation de Box2D pour FreePascal

#### Sur Windows

1. **Télécharger les bindings Pascal**
   ```
   https://github.com/Kagamma/box2d-fp
   ```

2. **Installer dans votre projet**
   ```
   MonProjet/
   ├── box2d/
   │   ├── uBox2D.pas
   │   └── box2d.dll (Windows 32/64-bit)
   └── MonJeu.pas
   ```

3. **Configurer le projet Lazarus**
   - Ajouter le dossier `box2d/` aux chemins de recherche
   - Copier `box2d.dll` dans le dossier de sortie

#### Sur Ubuntu/Linux

```bash
# Installer les dépendances
sudo apt-get install libbox2d-dev

# Ou compiler depuis les sources
git clone https://github.com/erincatto/box2d  
cd box2d  
mkdir build && cd build  
cmake ..  
make  
sudo make install
```

### Concepts fondamentaux de Box2D

#### Le monde physique (World)

Le monde contient tous les objets physiques et simule la physique :

```pascal
uses
  uBox2D;

var
  World: b2World;
  Gravity: b2Vec2;

begin
  // Créer la gravité (X, Y)
  // Y négatif = vers le bas en coordonnées écran classiques
  Gravity := b2Vec2_Make(0.0, -10.0); // 10 m/s²

  // Créer le monde
  World := b2World_Create(Gravity);

  // Configurer (optionnel)
  b2World_SetAllowSleeping(World, True); // Optimisation

  // Dans la boucle de jeu
  while Running do
  begin
    // Simuler la physique
    // Paramètres : timeStep, velocityIterations, positionIterations
    b2World_Step(World, 1.0 / 60.0, 8, 3);

    // Dessiner et mettre à jour
    Render;
  end;

  // Nettoyer
  b2World_Destroy(World);
end;
```

**Paramètres de Step** :
- `timeStep` : Temps simulé (généralement 1/60 seconde)
- `velocityIterations` : Précision des vitesses (8 recommandé)
- `positionIterations` : Précision des positions (3 recommandé)

#### Les corps (Bodies)

Un corps est un objet dans le monde physique :

```pascal
type
  TBodyType = (btStatic, btKinematic, btDynamic);

var
  GroundBody: b2Body;
  PlayerBody: b2Body;
  BodyDef: b2BodyDef;

// Créer un corps statique (sol)
BodyDef := b2DefaultBodyDef;  
BodyDef.bodyType := b2_staticBody;  
BodyDef.position := b2Vec2_Make(0.0, -10.0);  
GroundBody := b2World_CreateBody(World, @BodyDef);

// Créer un corps dynamique (joueur)
BodyDef := b2DefaultBodyDef;  
BodyDef.bodyType := b2_dynamicBody;  
BodyDef.position := b2Vec2_Make(0.0, 4.0);  
BodyDef.angle := 0.0;  
PlayerBody := b2World_CreateBody(World, @BodyDef);
```

**Types de corps** :
- **Static** : Immobile, ne bouge jamais (murs, sol)
- **Kinematic** : Contrôlé manuellement (plateformes mobiles)
- **Dynamic** : Affecté par les forces et la gravité (joueur, objets)

#### Les formes (Shapes) et fixtures

Les formes définissent la géométrie des collisions :

```pascal
var
  GroundShape: b2Polygon;
  CircleShape: b2Circle;
  FixtureDef: b2FixtureDef;

// Créer une boîte pour le sol
b2Polygon_SetAsBox(@GroundShape, 50.0, 10.0);

FixtureDef := b2DefaultFixtureDef;  
FixtureDef.shape := @GroundShape;  
FixtureDef.density := 0.0;  
FixtureDef.friction := 0.3;

b2Body_CreateFixture(GroundBody, @FixtureDef);

// Créer un cercle pour le joueur
CircleShape.center := b2Vec2_Make(0.0, 0.0);  
CircleShape.radius := 0.5;

FixtureDef := b2DefaultFixtureDef;  
FixtureDef.shape := @CircleShape;  
FixtureDef.density := 1.0;      // Masse = densité × surface  
FixtureDef.friction := 0.3;     // 0 = glissant, 1 = rugueux  
FixtureDef.restitution := 0.5;  // 0 = pas de rebond, 1 = rebond total

b2Body_CreateFixture(PlayerBody, @FixtureDef);
```

**Propriétés physiques** :
- **Density** (densité) : Détermine la masse (kg/m²)
- **Friction** : Résistance au glissement (0-1)
- **Restitution** : Élasticité des rebonds (0-1)

### Formes disponibles dans Box2D

#### Boîte (Box)

```pascal
var
  BoxShape: b2Polygon;
begin
  // Boîte centrée de 2×2 mètres
  b2Polygon_SetAsBox(@BoxShape, 1.0, 1.0);

  // Boîte décalée et tournée
  b2Polygon_SetAsBox(@BoxShape, 1.0, 1.0,
                     b2Vec2_Make(0.5, 0.5), // centre
                     0.25 * Pi);             // angle
end;
```

#### Cercle (Circle)

```pascal
var
  CircleShape: b2Circle;
begin
  CircleShape.center := b2Vec2_Make(0.0, 0.0);
  CircleShape.radius := 1.0; // rayon en mètres
end;
```

#### Polygone convexe

```pascal
var
  PolyShape: b2Polygon;
  Vertices: array[0..4] of b2Vec2;
begin
  // Triangle
  Vertices[0] := b2Vec2_Make(-1.0, 0.0);
  Vertices[1] := b2Vec2_Make(1.0, 0.0);
  Vertices[2] := b2Vec2_Make(0.0, 2.0);

  b2Polygon_Set(@PolyShape, @Vertices[0], 3);
end;
```

**Important** : Les polygones doivent être :
- Convexes (pas de creux)
- Maximum 8 sommets
- Sens antihoraire

#### Chaîne (Chain) et Segment (Edge)

Pour les niveaux et terrains :

```pascal
var
  ChainShape: b2ChainShape;
  Points: array[0..9] of b2Vec2;
  I: Integer;
begin
  // Définir les points du terrain
  for I := 0 to 9 do
  begin
    Points[I].x := I * 2.0;
    Points[I].y := Sin(I * 0.5) * 2.0;
  end;

  // Créer la chaîne
  b2ChainShape_CreateChain(@ChainShape, @Points[0], 10);
end;
```

### Conversion entre unités

Box2D utilise le système MKS (mètres, kilogrammes, secondes). Il faut convertir pour l'affichage :

```pascal
const
  PIXELS_PER_METER = 32.0; // 1 mètre = 32 pixels
  METERS_PER_PIXEL = 1.0 / PIXELS_PER_METER;

// Convertir position Box2D vers écran
function B2ToScreen(const Pos: b2Vec2): TVector2;  
begin
  Result.X := Pos.x * PIXELS_PER_METER;
  Result.Y := ScreenHeight - (Pos.y * PIXELS_PER_METER); // Inverser Y
end;

// Convertir position écran vers Box2D
function ScreenToB2(const Pos: TVector2): b2Vec2;  
begin
  Result.x := Pos.X * METERS_PER_PIXEL;
  Result.y := (ScreenHeight - Pos.Y) * METERS_PER_PIXEL;
end;

// Exemple d'utilisation
procedure DrawBody(Body: b2Body);  
var
  Pos: b2Vec2;
  ScreenPos: TVector2;
  Angle: Single;
begin
  Pos := b2Body_GetPosition(Body);
  Angle := b2Body_GetAngle(Body);

  ScreenPos := B2ToScreen(Pos);

  DrawSprite(SpriteTexture, ScreenPos.X, ScreenPos.Y, Angle);
end;
```

### Appliquer des forces et impulsions

#### Force continue

```pascal
// Appliquer une force (pour accélération progressive)
var
  Force: b2Vec2;
begin
  Force := b2Vec2_Make(100.0, 0.0); // 100 Newtons vers la droite

  // Au centre de masse
  b2Body_ApplyForceToCenter(PlayerBody, Force, True);

  // À un point spécifique
  b2Body_ApplyForce(PlayerBody, Force,
                    b2Body_GetWorldCenter(PlayerBody), True);
end;
```

#### Impulsion (changement instantané de vitesse)

```pascal
// Faire sauter le joueur
procedure Jump;  
var
  Impulse: b2Vec2;
begin
  Impulse := b2Vec2_Make(0.0, 50.0); // Vers le haut
  b2Body_ApplyLinearImpulseToCenter(PlayerBody, Impulse, True);
end;

// Tir de projectile
procedure FireBullet(Direction: b2Vec2);  
var
  BulletBody: b2Body;
  Velocity: b2Vec2;
begin
  // Créer la balle
  BulletBody := CreateBullet(PlayerPos);

  // Définir la vitesse directement
  Velocity := b2Vec2_Make(Direction.x * 20.0, Direction.y * 20.0);
  b2Body_SetLinearVelocity(BulletBody, Velocity);
end;
```

#### Modifier directement la vitesse

```pascal
// Déplacement du joueur (platformer)
procedure MovePlayer(dt: Single);  
var
  Vel: b2Vec2;
const
  MOVE_SPEED = 5.0;
  MAX_SPEED = 10.0;
begin
  Vel := b2Body_GetLinearVelocity(PlayerBody);

  // Mouvement horizontal
  if KeyPressed(KEY_RIGHT) then
    Vel.x := Min(Vel.x + MOVE_SPEED, MAX_SPEED);
  if KeyPressed(KEY_LEFT) then
    Vel.x := Max(Vel.x - MOVE_SPEED, -MAX_SPEED);

  // Conserver la vitesse verticale
  b2Body_SetLinearVelocity(PlayerBody, Vel);
end;
```

### Détection de collisions

#### Callbacks de collision

```pascal
type
  TMyContactListener = class
    procedure BeginContact(Contact: b2Contact); virtual;
    procedure EndContact(Contact: b2Contact); virtual;
    procedure PreSolve(Contact: b2Contact; const OldManifold: b2Manifold); virtual;
    procedure PostSolve(Contact: b2Contact; const Impulse: b2ContactImpulse); virtual;
  end;

procedure TMyContactListener.BeginContact(Contact: b2Contact);  
var
  FixtureA, FixtureB: b2Fixture;
  BodyA, BodyB: b2Body;
  UserDataA, UserDataB: Pointer;
begin
  FixtureA := b2Contact_GetFixtureA(Contact);
  FixtureB := b2Contact_GetFixtureB(Contact);

  BodyA := b2Fixture_GetBody(FixtureA);
  BodyB := b2Fixture_GetBody(FixtureB);

  // Récupérer les données utilisateur
  UserDataA := b2Body_GetUserData(BodyA);
  UserDataB := b2Body_GetUserData(BodyB);

  // Vérifier le type de collision
  if (UserDataA = PlayerData) and (UserDataB = EnemyData) then
  begin
    PlayerHit;
  end;
end;

// Installer le listener
var
  ContactListener: TMyContactListener;
begin
  ContactListener := TMyContactListener.Create;
  b2World_SetContactListener(World, ContactListener);
end;
```

#### UserData pour identifier les objets

```pascal
type
  TGameObject = class
    Body: b2Body;
    ObjectType: (otPlayer, otEnemy, otBullet, otWall);
    Health: Integer;
  end;

// Associer l'objet au corps physique
var
  Player: TGameObject;
begin
  Player := TGameObject.Create;
  Player.ObjectType := otPlayer;
  Player.Health := 100;

  // Créer le corps
  Player.Body := CreatePlayerBody(World, 0, 5);

  // Lier l'objet au corps
  b2Body_SetUserData(Player.Body, Player);
end;

// Dans le callback de collision
procedure TMyContactListener.BeginContact(Contact: b2Contact);  
var
  ObjA, ObjB: TGameObject;
begin
  ObjA := TGameObject(b2Body_GetUserData(b2Contact_GetFixtureA(Contact).body));
  ObjB := TGameObject(b2Body_GetUserData(b2Contact_GetFixtureB(Contact).body));

  if (ObjA.ObjectType = otPlayer) and (ObjB.ObjectType = otEnemy) then
  begin
    ObjA.Health := ObjA.Health - 10;
  end;
end;
```

### Raycasting (lancer de rayons)

Utile pour la ligne de vue, tirs au fusil, etc.

```pascal
type
  TRaycastCallback = class
    HitPoint: b2Vec2;
    HitNormal: b2Vec2;
    HitBody: b2Body;
    Hit: Boolean;

    function ReportFixture(Fixture: b2Fixture; const Point: b2Vec2;
                          const Normal: b2Vec2; Fraction: Single): Single; virtual;
  end;

function TRaycastCallback.ReportFixture(Fixture: b2Fixture;
                                       const Point: b2Vec2;
                                       const Normal: b2Vec2;
                                       Fraction: Single): Single;
begin
  HitPoint := Point;
  HitNormal := Normal;
  HitBody := b2Fixture_GetBody(Fixture);
  Hit := True;

  // Retourner Fraction pour continuer jusqu'au premier hit
  // Retourner 1 pour continuer à travers tous les objets
  Result := Fraction;
end;

// Utilisation
var
  Callback: TRaycastCallback;
  Point1, Point2: b2Vec2;
begin
  Callback := TRaycastCallback.Create;
  Callback.Hit := False;

  Point1 := b2Vec2_Make(PlayerX, PlayerY);
  Point2 := b2Vec2_Make(TargetX, TargetY);

  b2World_RayCast(World, Callback, Point1, Point2);

  if Callback.Hit then
  begin
    // Quelque chose a été touché
    DrawLine(Point1, Callback.HitPoint);
  end
  else
  begin
    // Rien touché
    DrawLine(Point1, Point2);
  end;
end;
```

### Joints (articulations)

Les joints permettent de connecter des corps entre eux.

#### Distance Joint (ressort)

```pascal
var
  JointDef: b2DistanceJointDef;
  Joint: b2Joint;
begin
  b2DistanceJointDef_Initialize(@JointDef, BodyA, BodyB,
                                AnchorPointA, AnchorPointB);

  JointDef.length := 5.0;           // Distance au repos
  JointDef.frequencyHz := 4.0;      // Raideur
  JointDef.dampingRatio := 0.5;     // Amortissement

  Joint := b2World_CreateJoint(World, @JointDef);
end;
```

#### Revolute Joint (charnière)

```pascal
var
  JointDef: b2RevoluteJointDef;
begin
  b2RevoluteJointDef_Initialize(@JointDef, BodyA, BodyB, AnchorPoint);

  JointDef.enableLimit := True;
  JointDef.lowerAngle := -0.5 * Pi;  // -90 degrés
  JointDef.upperAngle := 0.5 * Pi;   // +90 degrés

  JointDef.enableMotor := True;
  JointDef.motorSpeed := 1.0;        // rad/s
  JointDef.maxMotorTorque := 10.0;   // Force du moteur

  b2World_CreateJoint(World, @JointDef);
end;
```

#### Prismatic Joint (piston)

```pascal
var
  JointDef: b2PrismaticJointDef;
  Axis: b2Vec2;
begin
  Axis := b2Vec2_Make(0.0, 1.0); // Axe vertical

  b2PrismaticJointDef_Initialize(@JointDef, BodyA, BodyB,
                                 AnchorPoint, Axis);

  JointDef.enableLimit := True;
  JointDef.lowerTranslation := -5.0;
  JointDef.upperTranslation := 2.5;

  b2World_CreateJoint(World, @JointDef);
end;
```

### Exemple complet : Platformer simple

```pascal
program SimplePlatformer;

uses
  uBox2D, Graphics;

const
  PIXELS_PER_METER = 32.0;

type
  TGame = class
  private
    FWorld: b2World;
    FGround: b2Body;
    FPlayer: b2Body;
    FPlayerOnGround: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Update(dt: Single);
    procedure Render;
    procedure HandleInput;
  end;

constructor TGame.Create;  
var
  Gravity: b2Vec2;
  BodyDef: b2BodyDef;
  GroundShape: b2Polygon;
  PlayerShape: b2Polygon;
  FixtureDef: b2FixtureDef;
begin
  // Créer le monde
  Gravity := b2Vec2_Make(0.0, -20.0);
  FWorld := b2World_Create(Gravity);

  // Créer le sol
  BodyDef := b2DefaultBodyDef;
  BodyDef.position := b2Vec2_Make(10.0, 1.0);
  FGround := b2World_CreateBody(FWorld, @BodyDef);

  b2Polygon_SetAsBox(@GroundShape, 10.0, 1.0);
  b2Body_CreateFixture(FGround, @GroundShape, 0.0);

  // Créer le joueur
  BodyDef := b2DefaultBodyDef;
  BodyDef.bodyType := b2_dynamicBody;
  BodyDef.position := b2Vec2_Make(5.0, 8.0);
  BodyDef.fixedRotation := True; // Empêcher la rotation
  FPlayer := b2World_CreateBody(FWorld, @BodyDef);

  b2Polygon_SetAsBox(@PlayerShape, 0.4, 0.8);

  FixtureDef := b2DefaultFixtureDef;
  FixtureDef.shape := @PlayerShape;
  FixtureDef.density := 1.0;
  FixtureDef.friction := 0.3;

  b2Body_CreateFixture(FPlayer, @FixtureDef);
end;

procedure TGame.HandleInput;  
var
  Vel: b2Vec2;
  Impulse: b2Vec2;
const
  MOVE_FORCE = 10.0;
  JUMP_IMPULSE = 8.0;
begin
  Vel := b2Body_GetLinearVelocity(FPlayer);

  // Mouvement horizontal
  if KeyPressed(KEY_RIGHT) then
    Vel.x := 5.0
  else if KeyPressed(KEY_LEFT) then
    Vel.x := -5.0
  else
    Vel.x := Vel.x * 0.8; // Friction

  b2Body_SetLinearVelocity(FPlayer, Vel);

  // Saut
  if KeyDown(KEY_SPACE) and FPlayerOnGround then
  begin
    Impulse := b2Vec2_Make(0.0, JUMP_IMPULSE);
    b2Body_ApplyLinearImpulseToCenter(FPlayer, Impulse, True);
  end;
end;

procedure TGame.Update(dt: Single);  
var
  Vel: b2Vec2;
begin
  // Simuler la physique
  b2World_Step(FWorld, dt, 8, 3);

  // Vérifier si le joueur est au sol (simplifié)
  Vel := b2Body_GetLinearVelocity(FPlayer);
  FPlayerOnGround := Abs(Vel.y) < 0.1;
end;

procedure TGame.Render;  
var
  Pos: b2Vec2;
  ScreenX, ScreenY: Integer;
begin
  // Dessiner le sol
  Pos := b2Body_GetPosition(FGround);
  ScreenX := Round(Pos.x * PIXELS_PER_METER);
  ScreenY := Round((15 - Pos.y) * PIXELS_PER_METER);
  DrawRect(ScreenX - 320, ScreenY - 32, 640, 64, clGreen);

  // Dessiner le joueur
  Pos := b2Body_GetPosition(FPlayer);
  ScreenX := Round(Pos.x * PIXELS_PER_METER);
  ScreenY := Round((15 - Pos.y) * PIXELS_PER_METER);
  DrawRect(ScreenX - 13, ScreenY - 26, 26, 52, clBlue);
end;

destructor TGame.Destroy;  
begin
  b2World_Destroy(FWorld);
  inherited;
end;
```

## Bullet - Physique 3D

Bullet Physics est un moteur physique 3D professionnel utilisé dans de nombreux jeux AAA et films (Grand Theft Auto, Red Dead Redemption).

### Installation de Bullet pour FreePascal

#### Sur Windows

```
1. Télécharger Bullet depuis https://github.com/bulletphysics/bullet3
2. Compiler la bibliothèque ou télécharger les binaires
3. Utiliser les bindings Pascal (rechercher "bullet-pas" ou créer les vôtres)
4. Copier les DLL nécessaires
```

#### Sur Ubuntu/Linux

```bash
sudo apt-get install libbullet-dev

# Ou compiler depuis les sources
git clone https://github.com/bulletphysics/bullet3  
cd bullet3  
mkdir build && cd build  
cmake ..  
make  
sudo make install
```

### Concepts fondamentaux de Bullet

#### Monde physique 3D

```pascal
type
  TBulletWorld = class
  private
    FCollisionConfiguration: btDefaultCollisionConfiguration;
    FDispatcher: btCollisionDispatcher;
    FBroadphase: btDbvtBroadphase;
    FSolver: btSequentialImpulseConstraintSolver;
    FDynamicsWorld: btDiscreteDynamicsWorld;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Step(TimeStep: Single);
  end;

constructor TBulletWorld.Create;  
var
  Gravity: btVector3;
begin
  // Configuration de collision
  FCollisionConfiguration := btDefaultCollisionConfiguration_new;

  // Dispatcher
  FDispatcher := btCollisionDispatcher_new(FCollisionConfiguration);

  // Broadphase (détection grossière des collisions)
  FBroadphase := btDbvtBroadphase_new;

  // Solveur de contraintes
  FSolver := btSequentialImpulseConstraintSolver_new;

  // Créer le monde dynamique
  FDynamicsWorld := btDiscreteDynamicsWorld_new(FDispatcher, FBroadphase,
                                                 FSolver, FCollisionConfiguration);

  // Définir la gravité
  Gravity.x := 0.0;
  Gravity.y := -9.81; // m/s²
  Gravity.z := 0.0;
  btDynamicsWorld_setGravity(FDynamicsWorld, Gravity);
end;

procedure TBulletWorld.Step(TimeStep: Single);  
begin
  btDynamicsWorld_stepSimulation(FDynamicsWorld, TimeStep, 10, 1.0/60.0);
end;
```

### Formes de collision 3D

#### Boîte (Box)

```pascal
var
  Shape: btBoxShape;
  HalfExtents: btVector3;
begin
  HalfExtents.x := 1.0; // demi-largeur
  HalfExtents.y := 1.0; // demi-hauteur
  HalfExtents.z := 1.0; // demi-profondeur

  Shape := btBoxShape_new(HalfExtents);
end;
```

#### Sphère

```pascal
var
  Shape: btSphereShape;
begin
  Shape := btSphereShape_new(1.0); // rayon
end;
```

#### Capsule (pour personnages)

```pascal
var
  Shape: btCapsuleShape;
begin
  Shape := btCapsuleShape_new(0.5, 2.0); // rayon, hauteur
end;
```

#### Mesh (maillage complexe)

```pascal
var
  Shape: btBvhTriangleMeshShape;
  TriangleMesh: btTriangleMesh;
  V0, V1, V2: btVector3;
begin
  TriangleMesh := btTriangleMesh_new;

  // Ajouter des triangles
  // Triangle 1
  V0 := btVector3_Make(-1, 0, -1);
  V1 := btVector3_Make(1, 0, -1);
  V2 := btVector3_Make(0, 2, 0);
  btTriangleMesh_addTriangle(TriangleMesh, V0, V1, V2, True);

  // Créer la forme
  Shape := btBvhTriangleMeshShape_new(TriangleMesh, True);
end;
```

### Corps rigides (Rigid Bodies)

```pascal
function CreateRigidBody(Mass: Single; Transform: btTransform;
                        Shape: btCollisionShape): btRigidBody;
var
  LocalInertia: btVector3;
  MotionState: btDefaultMotionState;
  RBInfo: btRigidBodyConstructionInfo;
begin
  LocalInertia := btVector3_Make(0, 0, 0);

  // Si la masse > 0, calculer l'inertie
  if Mass > 0.0 then
    btCollisionShape_calculateLocalInertia(Shape, Mass, LocalInertia);

  // Créer le motion state (état de mouvement)
  MotionState := btDefaultMotionState_new(Transform);

  // Créer les infos du corps rigide
  RBInfo := btRigidBodyConstructionInfo_new(Mass, MotionState,
                                            Shape, LocalInertia);

  // Propriétés physiques
  RBInfo.friction := 0.5;
  RBInfo.restitution := 0.3;

  // Créer le corps rigide
  Result := btRigidBody_new(RBInfo);
end;

// Exemple d'utilisation
var
  World: btDiscreteDynamicsWorld;
  GroundShape: btBoxShape;
  SphereShape: btSphereShape;
  GroundTransform, SphereTransform: btTransform;
  GroundBody, SphereBody: btRigidBody;

begin
  // Créer le sol (statique, masse = 0)
  GroundTransform := btTransform_getIdentity;
  GroundTransform.origin := btVector3_Make(0, -10, 0);

  GroundShape := btBoxShape_new(btVector3_Make(50, 1, 50));
  GroundBody := CreateRigidBody(0.0, GroundTransform, GroundShape);

  btDynamicsWorld_addRigidBody(World, GroundBody);

  // Créer une sphère (dynamique, masse = 1)
  SphereTransform := btTransform_getIdentity;
  SphereTransform.origin := btVector3_Make(0, 10, 0);

  SphereShape := btSphereShape_new(1.0);
  SphereBody := CreateRigidBody(1.0, SphereTransform, SphereShape);

  btDynamicsWorld_addRigidBody(World, SphereBody);
end;
```

### Récupérer la position et l'orientation

```pascal
procedure GetBodyTransform(Body: btRigidBody; out Position: TVector3;
                          out Rotation: TQuaternion);
var
  Transform: btTransform;
  Origin: btVector3;
  Quat: btQuaternion;
begin
  // Récupérer la transformation
  btRigidBody_getWorldTransform(Body, Transform);

  // Position
  Origin := btTransform_getOrigin(Transform);
  Position.X := Origin.x;
  Position.Y := Origin.y;
  Position.Z := Origin.z;

  // Rotation (quaternion)
  Quat := btTransform_getRotation(Transform);
  Rotation.X := Quat.x;
  Rotation.Y := Quat.y;
  Rotation.Z := Quat.z;
  Rotation.W := Quat.w;
end;

// Convertir quaternion en angles d'Euler (pour l'affichage)
function QuaternionToEuler(const Q: TQuaternion): TVector3;  
var
  SinrCosp, CosrCosp, SinyCosp, CosyCosp, SinpValue: Single;
begin
  // Roll (X)
  SinrCosp := 2.0 * (Q.W * Q.X + Q.Y * Q.Z);
  CosrCosp := 1.0 - 2.0 * (Q.X * Q.X + Q.Y * Q.Y);
  Result.X := ArcTan2(SinrCosp, CosrCosp);

  // Pitch (Y)
  SinpValue := 2.0 * (Q.W * Q.Y - Q.Z * Q.X);
  if Abs(SinpValue) >= 1.0 then
    Result.Y := Sign(SinpValue) * Pi / 2.0
  else
    Result.Y := ArcSin(SinpValue);

  // Yaw (Z)
  SinyCosp := 2.0 * (Q.W * Q.Z + Q.X * Q.Y);
  CosyCosp := 1.0 - 2.0 * (Q.Y * Q.Y + Q.Z * Q.Z);
  Result.Z := ArcTan2(SinyCosp, CosyCosp);
end;
```

### Appliquer des forces et impulsions 3D

```pascal
// Appliquer une force au centre
procedure ApplyForce(Body: btRigidBody; Force: btVector3);  
begin
  btRigidBody_activate(Body, True); // Réveiller l'objet
  btRigidBody_applyCentralForce(Body, Force);
end;

// Appliquer une impulsion
procedure ApplyImpulse(Body: btRigidBody; Impulse: btVector3);  
begin
  btRigidBody_activate(Body, True);
  btRigidBody_applyCentralImpulse(Body, Impulse);
end;

// Appliquer un couple (rotation)
procedure ApplyTorque(Body: btRigidBody; Torque: btVector3);  
begin
  btRigidBody_activate(Body, True);
  btRigidBody_applyTorque(Body, Torque);
end;

// Exemple : faire sauter un objet
procedure Jump(Body: btRigidBody);  
var
  Impulse: btVector3;
begin
  Impulse.x := 0;
  Impulse.y := 10.0; // Vers le haut
  Impulse.z := 0;

  ApplyImpulse(Body, Impulse);
end;

// Exemple : propulser vers l'avant
procedure MoveForward(Body: btRigidBody; Speed: Single);  
var
  Transform: btTransform;
  Forward, Force: btVector3;
begin
  btRigidBody_getWorldTransform(Body, Transform);

  // Obtenir la direction "avant" du corps
  Forward := btMatrix3x3_getColumn(Transform.basis, 2);

  // Créer la force
  Force.x := Forward.x * Speed;
  Force.y := Forward.y * Speed;
  Force.z := Forward.z * Speed;

  ApplyForce(Body, Force);
end;
```

### Raycasting 3D

```pascal
type
  TRaycastResult = record
    Hit: Boolean;
    HitPoint: TVector3;
    HitNormal: TVector3;
    HitBody: btRigidBody;
    HitFraction: Single;
  end;

function Raycast(World: btDynamicsWorld; const RayFrom, RayTo: TVector3): TRaycastResult;  
var
  From, To: btVector3;
  Callback: btCollisionWorld_ClosestRayResultCallback;
begin
  Result.Hit := False;

  From := btVector3_Make(RayFrom.X, RayFrom.Y, RayFrom.Z);
  To := btVector3_Make(RayTo.X, RayTo.Y, RayTo.Z);

  Callback := btCollisionWorld_ClosestRayResultCallback_new(From, To);

  btCollisionWorld_rayTest(World, From, To, Callback);

  if btCollisionWorld_RayResultCallback_hasHit(Callback) then
  begin
    Result.Hit := True;

    Result.HitPoint.X := Callback.hitPointWorld.x;
    Result.HitPoint.Y := Callback.hitPointWorld.y;
    Result.HitPoint.Z := Callback.hitPointWorld.z;

    Result.HitNormal.X := Callback.hitNormalWorld.x;
    Result.HitNormal.Y := Callback.hitNormalWorld.y;
    Result.HitNormal.Z := Callback.hitNormalWorld.z;

    Result.HitBody := Callback.collisionObject;
    Result.HitFraction := Callback.closestHitFraction;
  end;

  btCollisionWorld_ClosestRayResultCallback_delete(Callback);
end;

// Utilisation pour viser
procedure ShootRay(PlayerPos, CameraDir: TVector3);  
var
  RayEnd: TVector3;
  Result: TRaycastResult;
begin
  // Point de fin du rayon
  RayEnd.X := PlayerPos.X + CameraDir.X * 100.0;
  RayEnd.Y := PlayerPos.Y + CameraDir.Y * 100.0;
  RayEnd.Z := PlayerPos.Z + CameraDir.Z * 100.0;

  Result := Raycast(World, PlayerPos, RayEnd);

  if Result.Hit then
  begin
    WriteLn('Touché à ', Result.HitFraction * 100.0, ' mètres');
    // Appliquer des dégâts, afficher un impact, etc.
  end;
end;
```

### Contraintes et joints 3D

#### Hinge Constraint (charnière)

```pascal
var
  Hinge: btHingeConstraint;
  PivotA, PivotB: btVector3;
  AxisA, AxisB: btVector3;
begin
  // Point de pivot dans l'espace local de chaque corps
  PivotA := btVector3_Make(0, -1, 0);
  PivotB := btVector3_Make(0, 1, 0);

  // Axe de rotation
  AxisA := btVector3_Make(0, 0, 1);
  AxisB := btVector3_Make(0, 0, 1);

  Hinge := btHingeConstraint_new(BodyA, BodyB, PivotA, PivotB,
                                  AxisA, AxisB, False);

  // Limites d'angle
  btHingeConstraint_setLimit(Hinge, -Pi/4, Pi/4, 0.9, 0.3, 1.0);

  // Moteur
  btHingeConstraint_enableAngularMotor(Hinge, True, 1.0, 50.0);

  btDynamicsWorld_addConstraint(World, Hinge, True);
end;
```

#### Point to Point Constraint (pivot)

```pascal
var
  P2P: btPoint2PointConstraint;
  PivotA, PivotB: btVector3;
begin
  PivotA := btVector3_Make(0, 1, 0);
  PivotB := btVector3_Make(0, -1, 0);

  P2P := btPoint2PointConstraint_new(BodyA, BodyB, PivotA, PivotB);

  btDynamicsWorld_addConstraint(World, P2P, True);
end;
```

#### Generic 6DOF Constraint (6 degrés de liberté)

Le plus flexible, permet de configurer chaque axe :

```pascal
var
  Constraint: btGeneric6DofConstraint;
  FrameA, FrameB: btTransform;
  LinearLower, LinearUpper: btVector3;
  AngularLower, AngularUpper: btVector3;
begin
  FrameA := btTransform_getIdentity;
  FrameB := btTransform_getIdentity;

  Constraint := btGeneric6DofConstraint_new(BodyA, BodyB,
                                            FrameA, FrameB, True);

  // Limites linéaires (translation)
  LinearLower := btVector3_Make(-5, 0, -5);
  LinearUpper := btVector3_Make(5, 10, 5);
  btGeneric6DofConstraint_setLinearLowerLimit(Constraint, LinearLower);
  btGeneric6DofConstraint_setLinearUpperLimit(Constraint, LinearUpper);

  // Limites angulaires (rotation)
  AngularLower := btVector3_Make(-Pi/4, -Pi/2, 0);
  AngularUpper := btVector3_Make(Pi/4, Pi/2, 0);
  btGeneric6DofConstraint_setAngularLowerLimit(Constraint, AngularLower);
  btGeneric6DofConstraint_setAngularUpperLimit(Constraint, AngularUpper);

  btDynamicsWorld_addConstraint(World, Constraint, True);
end;
```

### Contrôleur de personnage (Character Controller)

Pour les personnages jouables, utilisez un contrôleur spécialisé :

```pascal
type
  TCharacterController = class
  private
    FGhostObject: btPairCachingGhostObject;
    FController: btKinematicCharacterController;
    FWorld: btDynamicsWorld;
  public
    constructor Create(World: btDynamicsWorld; Radius, Height: Single;
                      StartPos: TVector3);
    destructor Destroy; override;
    procedure SetWalkDirection(Direction: TVector3);
    procedure Jump;
    function GetPosition: TVector3;
    function IsOnGround: Boolean;
  end;

constructor TCharacterController.Create(World: btDynamicsWorld;
                                       Radius, Height: Single;
                                       StartPos: TVector3);
var
  Shape: btCapsuleShape;
  Transform: btTransform;
  StepHeight: Single;
begin
  FWorld := World;

  // Créer la forme capsule
  Shape := btCapsuleShape_new(Radius, Height);

  // Ghost object (objet fantôme pour le contrôleur)
  FGhostObject := btPairCachingGhostObject_new;

  Transform := btTransform_getIdentity;
  Transform.origin := btVector3_Make(StartPos.X, StartPos.Y, StartPos.Z);
  btCollisionObject_setWorldTransform(FGhostObject, Transform);

  btCollisionObject_setCollisionShape(FGhostObject, Shape);
  btCollisionObject_setCollisionFlags(FGhostObject,
    btCollisionObject_CF_CHARACTER_OBJECT);

  // Créer le contrôleur
  StepHeight := 0.35; // Hauteur maximale de marche
  FController := btKinematicCharacterController_new(FGhostObject,
                                                     Shape, StepHeight);

  // Configuration
  btKinematicCharacterController_setGravity(FController, 9.81);
  btKinematicCharacterController_setJumpSpeed(FController, 5.0);
  btKinematicCharacterController_setMaxSlope(FController, Pi / 4); // 45°

  // Ajouter au monde
  btDynamicsWorld_addCollisionObject(World, FGhostObject,
    btBroadphaseProxy_CF_CHARACTER_OBJECT,
    btBroadphaseProxy_CF_STATIC_OBJECT or
    btBroadphaseProxy_CF_DYNAMIC_OBJECT);

  btDynamicsWorld_addAction(World, FController);
end;

procedure TCharacterController.SetWalkDirection(Direction: TVector3);  
var
  WalkDir: btVector3;
begin
  WalkDir := btVector3_Make(Direction.X, Direction.Y, Direction.Z);
  btKinematicCharacterController_setWalkDirection(FController, WalkDir);
end;

procedure TCharacterController.Jump;  
begin
  btKinematicCharacterController_jump(FController);
end;

function TCharacterController.GetPosition: TVector3;  
var
  Transform: btTransform;
  Origin: btVector3;
begin
  Transform := btCollisionObject_getWorldTransform(FGhostObject);
  Origin := btTransform_getOrigin(Transform);

  Result.X := Origin.x;
  Result.Y := Origin.y;
  Result.Z := Origin.z;
end;

function TCharacterController.IsOnGround: Boolean;  
begin
  Result := btKinematicCharacterController_onGround(FController);
end;

// Utilisation dans la boucle de jeu
procedure UpdatePlayer(Controller: TCharacterController; dt: Single);  
var
  MoveDir: TVector3;
  Speed: Single;
begin
  Speed := 5.0;
  MoveDir := Vector3Zero;

  if KeyPressed(KEY_W) then
    MoveDir.Z := MoveDir.Z - 1;
  if KeyPressed(KEY_S) then
    MoveDir.Z := MoveDir.Z + 1;
  if KeyPressed(KEY_A) then
    MoveDir.X := MoveDir.X - 1;
  if KeyPressed(KEY_D) then
    MoveDir.X := MoveDir.X + 1;

  // Normaliser et appliquer la vitesse
  if (MoveDir.X <> 0) or (MoveDir.Z <> 0) then
  begin
    MoveDir := Normalize(MoveDir);
    MoveDir.X := MoveDir.X * Speed * dt;
    MoveDir.Z := MoveDir.Z * Speed * dt;
  end;

  Controller.SetWalkDirection(MoveDir);

  if KeyDown(KEY_SPACE) and Controller.IsOnGround then
    Controller.Jump;
end;
```

### Véhicules avec Bullet

Bullet inclut un système de véhicules réaliste :

```pascal
type
  TVehicle = class
  private
    FChassisBody: btRigidBody;
    FVehicleRaycaster: btDefaultVehicleRaycaster;
    FVehicle: btRaycastVehicle;
    FTuning: btVehicleTuning;
  public
    constructor Create(World: btDynamicsWorld; ChassisPos: TVector3);
    procedure AddWheel(Position: TVector3; IsFrontWheel: Boolean);
    procedure ApplyEngineForce(Force: Single; WheelIndex: Integer);
    procedure SetSteering(Steering: Single; WheelIndex: Integer);
    procedure UpdateVehicle;
  end;

constructor TVehicle.Create(World: btDynamicsWorld; ChassisPos: TVector3);  
var
  ChassisShape: btBoxShape;
  ChassisTransform: btTransform;
begin
  // Créer la forme du châssis
  ChassisShape := btBoxShape_new(btVector3_Make(1.0, 0.5, 2.0));

  ChassisTransform := btTransform_getIdentity;
  ChassisTransform.origin := btVector3_Make(ChassisPos.X, ChassisPos.Y, ChassisPos.Z);

  // Créer le corps rigide
  FChassisBody := CreateRigidBody(800.0, ChassisTransform, ChassisShape);
  btDynamicsWorld_addRigidBody(World, FChassisBody);

  // Créer le véhicule
  FTuning := btVehicleTuning_new;
  FVehicleRaycaster := btDefaultVehicleRaycaster_new(World);
  FVehicle := btRaycastVehicle_new(FTuning, FChassisBody, FVehicleRaycaster);

  btRaycastVehicle_setCoordinateSystem(FVehicle, 0, 1, 2); // X, Y, Z

  btDynamicsWorld_addVehicle(World, FVehicle);
end;

procedure TVehicle.AddWheel(Position: TVector3; IsFrontWheel: Boolean);  
var
  ConnectionPoint, WheelDirection, WheelAxle: btVector3;
  SuspensionRestLength, WheelRadius: Single;
  WheelFriction, SuspensionStiffness: Single;
begin
  ConnectionPoint := btVector3_Make(Position.X, Position.Y, Position.Z);
  WheelDirection := btVector3_Make(0, -1, 0);
  WheelAxle := btVector3_Make(-1, 0, 0);

  SuspensionRestLength := 0.6;
  WheelRadius := 0.4;
  WheelFriction := 1000.0;
  SuspensionStiffness := 20.0;

  btRaycastVehicle_addWheel(FVehicle, ConnectionPoint, WheelDirection,
                           WheelAxle, SuspensionRestLength, WheelRadius,
                           FTuning, IsFrontWheel);
end;

procedure TVehicle.ApplyEngineForce(Force: Single; WheelIndex: Integer);  
begin
  btRaycastVehicle_applyEngineForce(FVehicle, Force, WheelIndex);
end;

procedure TVehicle.SetSteering(Steering: Single; WheelIndex: Integer);  
begin
  btRaycastVehicle_setSteeringValue(FVehicle, Steering, WheelIndex);
end;
```

## Optimisation des moteurs physiques

### Sleeping (mise en veille)

Les objets immobiles sont automatiquement "endormis" pour économiser du CPU :

```pascal
// Configurer les seuils de mise en veille (Box2D)
b2World_SetAllowSleeping(World, True);

// Pour Bullet
btRigidBody_setSleepingThresholds(Body, 0.5, 0.5);

// Forcer le réveil
b2Body_SetAwake(Body, True);           // Box2D  
btRigidBody_activate(Body, True);      // Bullet
```

### Broadphase optimization

La "broadphase" détecte rapidement quels objets pourraient entrer en collision :

```pascal
// Box2D utilise automatiquement un arbre dynamique (AABB tree)

// Bullet offre plusieurs options
// - btDbvtBroadphase : Bon compromis (par défaut)
// - btAxisSweep3 : Rapide pour mondes limités
// - bt32BitAxisSweep3 : Pour plus d'objets

var
  WorldMin, WorldMax: btVector3;
  Broadphase: bt32BitAxisSweep3;
begin
  WorldMin := btVector3_Make(-1000, -1000, -1000);
  WorldMax := btVector3_Make(1000, 1000, 1000);

  Broadphase := bt32BitAxisSweep3_new(WorldMin, WorldMax, 65536);
end;
```

### Itérations de solveur

Plus d'itérations = plus précis mais plus lent :

```pascal
// Box2D
b2World_Step(World, dt, 8, 3); // 8 vélocité, 3 position

// Bullet
btContactSolverInfo_setNumIterations(
  btDynamicsWorld_getSolverInfo(World), 10);
```

### Collision filtering

Évitez les calculs inutiles avec des groupes de collision :

```pascal
// Box2D
const
  CATEGORY_PLAYER   = $0001;
  CATEGORY_ENEMY    = $0002;
  CATEGORY_BULLET   = $0004;
  CATEGORY_GROUND   = $0008;

var
  FixtureDef: b2FixtureDef;
begin
  FixtureDef.filter.categoryBits := CATEGORY_PLAYER;
  FixtureDef.filter.maskBits := CATEGORY_GROUND or CATEGORY_ENEMY;
  // Le joueur ne collisionne qu'avec le sol et les ennemis
end;

// Bullet
const
  COL_PLAYER = 1;
  COL_ENEMY = 2;
  COL_GROUND = 4;

btDynamicsWorld_addRigidBody(World, PlayerBody, COL_PLAYER,
                            COL_GROUND or COL_ENEMY);
```

### Fixed timestep

Utilisez toujours un pas de temps fixe pour la stabilité :

```pascal
const
  FIXED_TIMESTEP = 1.0 / 60.0; // 60 Hz

var
  Accumulator: Double;
  FrameTime: Double;

procedure GameLoop;  
begin
  FrameTime := GetDeltaTime;
  Accumulator := Accumulator + FrameTime;

  // Mettre à jour la physique avec des pas fixes
  while Accumulator >= FIXED_TIMESTEP do
  begin
    b2World_Step(World, FIXED_TIMESTEP, 8, 3);
    Accumulator := Accumulator - FIXED_TIMESTEP;
  end;

  // Interpoler le rendu si nécessaire
  Alpha := Accumulator / FIXED_TIMESTEP;
  RenderWithInterpolation(Alpha);
end;
```

## Débogage visuel

### Debug Draw pour Box2D

```pascal
type
  TDebugDraw = class
  private
    FWorld: b2World;
  public
    procedure DrawShape(Shape: b2Shape; Transform: b2Transform;
                       Color: TColor);
    procedure DrawWorld;
  end;

procedure TDebugDraw.DrawWorld;  
var
  Body: b2Body;
  Fixture: b2Fixture;
  Transform: b2Transform;
begin
  Body := b2World_GetBodyList(FWorld);

  while Body <> nil do
  begin
    Transform := b2Body_GetTransform(Body);
    Fixture := b2Body_GetFixtureList(Body);

    while Fixture <> nil do
    begin
      DrawShape(b2Fixture_GetShape(Fixture), Transform, clGreen);
      Fixture := b2Fixture_GetNext(Fixture);
    end;

    Body := b2Body_GetNext(Body);
  end;
end;

procedure TDebugDraw.DrawShape(Shape: b2Shape; Transform: b2Transform;
                              Color: TColor);
var
  ShapeType: Integer;
  Polygon: b2PolygonShape;
  Circle: b2CircleShape;
  I: Integer;
  Vertices: array[0..7] of TVector2;
begin
  ShapeType := b2Shape_GetType(Shape);

  case ShapeType of
    b2Shape_e_polygon:
    begin
      Polygon := b2PolygonShape(Shape);
      // Dessiner le polygone
      for I := 0 to b2PolygonShape_GetVertexCount(Polygon) - 1 do
      begin
        Vertices[I] := B2ToScreen(
          b2Transform_Multiply(Transform,
                              b2PolygonShape_GetVertex(Polygon, I)));
      end;
      DrawPolygon(Vertices, b2PolygonShape_GetVertexCount(Polygon), Color);
    end;

    b2Shape_e_circle:
    begin
      Circle := b2CircleShape(Shape);
      DrawCircle(B2ToScreen(Circle.center),
                Circle.radius * PIXELS_PER_METER, Color);
    end;
  end;
end;
```

### Debug Draw pour Bullet

```pascal
type
  TBulletDebugDrawer = class
  public
    procedure DrawLine(const From, To: btVector3; const Color: btVector3);
    procedure DrawContactPoint(const PointOnB, Normal: btVector3;
                              Distance: Single; LifeTime: Integer;
                              const Color: btVector3);
    procedure Draw3dText(const Location: btVector3; const Text: string);
    procedure SetDebugMode(Mode: Integer);
    function GetDebugMode: Integer;
  end;

procedure TBulletDebugDrawer.DrawLine(const From, To: btVector3;
                                     const Color: btVector3);
begin
  // Utiliser votre moteur de rendu 3D
  DrawLine3D(Vector3(From.x, From.y, From.z),
            Vector3(To.x, To.y, To.z),
            RGBColor(Color.x, Color.y, Color.z));
end;

// Activer le debug draw
var
  DebugDrawer: TBulletDebugDrawer;
begin
  DebugDrawer := TBulletDebugDrawer.Create;
  btDynamicsWorld_setDebugDrawer(World, DebugDrawer);

  DebugDrawer.SetDebugMode(
    DBG_DrawWireframe or
    DBG_DrawAabb or
    DBG_DrawContactPoints);

  // Dans la boucle de rendu
  btDynamicsWorld_debugDrawWorld(World);
end;
```

## Conseils et bonnes pratiques

### Échelle du monde

**Box2D recommande** :
- Objets entre 0.1 et 10 mètres
- Vitesses < 50 m/s
- Gravité standard : 9.81 m/s²

**Bullet recommande** :
- Objets entre 0.05 et 10 mètres
- Éviter les objets trop petits ou trop grands
- Ratio taille max/min < 100:1

### Éviter les tunneling (traversée d'objets)

Le tunneling se produit quand un objet rapide traverse un mur mince entre deux frames.

#### Solution 1 : Continuous Collision Detection (CCD)

**Box2D** :
```pascal
// Activer la détection continue pour un corps
b2Body_SetBullet(BulletBody, True);

// Le corps sera testé en continu contre les corps statiques
```

**Bullet** :
```pascal
// Activer CCD avec un seuil
btRigidBody_setCcdMotionThreshold(Body, 1.0);  
btRigidBody_setCcdSweptSphereRadius(Body, 0.2);

// Le corps utilise une sphère de 0.2m de rayon pour la détection
// Si déplacement > 1.0m par frame, CCD s'active
```

#### Solution 2 : Augmenter l'épaisseur des murs

```pascal
// MAUVAIS : Mur trop fin
b2Polygon_SetAsBox(@WallShape, 50.0, 0.1); // 50m × 0.2m

// BON : Mur épais
b2Polygon_SetAsBox(@WallShape, 50.0, 0.5); // 50m × 1m
```

#### Solution 3 : Limiter les vitesses

```pascal
// Box2D
procedure LimitVelocity(Body: b2Body; MaxSpeed: Single);  
var
  Vel: b2Vec2;
  Speed: Single;
begin
  Vel := b2Body_GetLinearVelocity(Body);
  Speed := Sqrt(Vel.x * Vel.x + Vel.y * Vel.y);

  if Speed > MaxSpeed then
  begin
    Vel.x := (Vel.x / Speed) * MaxSpeed;
    Vel.y := (Vel.y / Speed) * MaxSpeed;
    b2Body_SetLinearVelocity(Body, Vel);
  end;
end;

// Bullet
procedure LimitVelocity3D(Body: btRigidBody; MaxSpeed: Single);  
var
  Vel: btVector3;
  Speed: Single;
begin
  Vel := btRigidBody_getLinearVelocity(Body);
  Speed := Sqrt(Vel.x * Vel.x + Vel.y * Vel.y + Vel.z * Vel.z);

  if Speed > MaxSpeed then
  begin
    Vel.x := (Vel.x / Speed) * MaxSpeed;
    Vel.y := (Vel.y / Speed) * MaxSpeed;
    Vel.z := (Vel.z / Speed) * MaxSpeed;
    btRigidBody_setLinearVelocity(Body, Vel);
  end;
end;
```

### Performance : Quand utiliser des formes simples

**Hiérarchie de performance (du plus rapide au plus lent)** :

1. **Sphère** - Le plus rapide
2. **Capsule** - Excellent pour personnages
3. **Boîte** - Très rapide
4. **Cylindre** - Rapide
5. **Convex Hull** - Moyen (polygone convexe)
6. **Triangle Mesh** - Lent (seulement pour objets statiques)
7. **Concave Mesh** - Très lent (décomposer si possible)

```pascal
// BON : Utiliser des formes simples pour objets dynamiques
PlayerShape := btCapsuleShape_new(0.5, 2.0); // Capsule

// MAUVAIS : Mesh complexe pour objet dynamique
PlayerShape := btBvhTriangleMeshShape_new(ComplexMesh); // Trop lent !

// BON : Mesh seulement pour décor statique
TerrainShape := btBvhTriangleMeshShape_new(TerrainMesh);  
TerrainBody := CreateRigidBody(0.0, Transform, TerrainShape); // Masse = 0
```

### Compound Shapes (formes composées)

Pour des objets complexes, combinez des formes simples :

```pascal
// Créer une voiture avec forme composée
var
  CompoundShape: btCompoundShape;
  ChassisShape, WheelShape: btBoxShape;
  ChassisTransform, WheelTransform: btTransform;
begin
  CompoundShape := btCompoundShape_new;

  // Châssis principal
  ChassisShape := btBoxShape_new(btVector3_Make(2.0, 0.5, 4.0));
  ChassisTransform := btTransform_getIdentity;
  ChassisTransform.origin := btVector3_Make(0, 0.5, 0);
  btCompoundShape_addChildShape(CompoundShape, ChassisTransform, ChassisShape);

  // Cabine
  WheelShape := btBoxShape_new(btVector3_Make(1.5, 0.8, 2.0));
  WheelTransform := btTransform_getIdentity;
  WheelTransform.origin := btVector3_Make(0, 1.3, -0.5);
  btCompoundShape_addChildShape(CompoundShape, WheelTransform, WheelShape);

  // Utiliser la forme composée
  CarBody := CreateRigidBody(1000.0, StartTransform, CompoundShape);
end;
```

### Gestion de la mémoire

#### Libération correcte des ressources

**Box2D** :
```pascal
procedure CleanupBox2D;  
var
  Body, NextBody: b2Body;
begin
  // Détruire tous les corps
  Body := b2World_GetBodyList(World);
  while Body <> nil do
  begin
    NextBody := b2Body_GetNext(Body);
    b2World_DestroyBody(World, Body);
    Body := NextBody;
  end;

  // Détruire le monde
  b2World_Destroy(World);
end;
```

**Bullet** :
```pascal
procedure CleanupBullet;  
var
  I: Integer;
  Obj: btCollisionObject;
  Body: btRigidBody;
  MotionState: btMotionState;
  Shape: btCollisionShape;
begin
  // Supprimer tous les corps rigides
  for I := btDynamicsWorld_getNumCollisionObjects(World) - 1 downto 0 do
  begin
    Obj := btCollisionWorld_getCollisionObjectArray(World, I);
    Body := btRigidBody_upcast(Obj);

    if Body <> nil then
    begin
      MotionState := btRigidBody_getMotionState(Body);
      if MotionState <> nil then
        btMotionState_delete(MotionState);

      Shape := btCollisionObject_getCollisionShape(Obj);
      btCollisionShape_delete(Shape);
    end;

    btDynamicsWorld_removeCollisionObject(World, Obj);
    btCollisionObject_delete(Obj);
  end;

  // Détruire le monde et ses composants
  btDynamicsWorld_delete(World);
  btConstraintSolver_delete(Solver);
  btBroadphaseInterface_delete(Broadphase);
  btCollisionDispatcher_delete(Dispatcher);
  btCollisionConfiguration_delete(CollisionConfiguration);
end;
```

### Stabilité numérique

#### Éviter les objets déséquilibrés

```pascal
// MAUVAIS : Objet très déséquilibré
var
  Shape: btBoxShape;
begin
  Shape := btBoxShape_new(btVector3_Make(0.1, 10.0, 0.1));
  // Ratio 100:1 = instable !
end;

// BON : Proportions raisonnables
var
  Shape: btBoxShape;
begin
  Shape := btBoxShape_new(btVector3_Make(1.0, 3.0, 1.0));
  // Ratio 3:1 = stable
end;
```

#### Centre de masse approprié

```pascal
// Définir manuellement le centre de masse (Bullet)
var
  LocalInertia: btVector3;
  CenterOfMass: btVector3;
begin
  CenterOfMass := btVector3_Make(0, -0.5, 0); // Décaler vers le bas

  btCollisionShape_calculateLocalInertia(Shape, Mass, LocalInertia);

  // Le corps sera plus stable avec un centre de masse bas
end;
```

### Plateformes mobiles (Moving Platforms)

#### Méthode 1 : Corps cinématique

```pascal
// Box2D
var
  Platform: b2Body;
  BodyDef: b2BodyDef;
begin
  BodyDef := b2DefaultBodyDef;
  BodyDef.bodyType := b2_kinematicBody; // Pas affecté par la physique
  BodyDef.position := b2Vec2_Make(0, 5);
  Platform := b2World_CreateBody(World, @BodyDef);

  // Dans la boucle de jeu
  NewPos := CalculatePlatformPosition(Time);
  b2Body_SetTransform(Platform, NewPos, 0.0);

  // Calculer et définir la vélocité pour que les objets suivent
  Velocity := (NewPos.x - OldPos.x) / dt;
  b2Body_SetLinearVelocity(Platform, b2Vec2_Make(Velocity, 0));
end;
```

#### Méthode 2 : Motor Joint (recommandé)

```pascal
// Box2D avec Motor Joint
var
  PlatformJoint: b2MotorJoint;
  JointDef: b2MotorJointDef;
begin
  b2MotorJointDef_Initialize(@JointDef, GroundBody, Platform);

  JointDef.maxForce := 1000.0;
  JointDef.maxTorque := 1000.0;
  JointDef.correctionFactor := 0.3;

  PlatformJoint := b2World_CreateJoint(World, @JointDef);

  // Déplacer la plateforme
  TargetPos := CalculatePlatformPosition(Time);
  b2MotorJoint_SetLinearOffset(PlatformJoint, TargetPos);
end;
```

### One-Way Platforms (plateformes traversables)

Plateformes que l'on peut traverser par le bas mais pas par le haut :

```pascal
type
  TOneWayPlatformListener = class
  private
    FPlayerBody: b2Body;
    FPlatformBody: b2Body;
  public
    procedure PreSolve(Contact: b2Contact; const OldManifold: b2Manifold);
  end;

procedure TOneWayPlatformListener.PreSolve(Contact: b2Contact;
                                          const OldManifold: b2Manifold);
var
  FixtureA, FixtureB: b2Fixture;
  BodyA, BodyB: b2Body;
  PlayerVel: b2Vec2;
  PlatformPos, PlayerPos: b2Vec2;
begin
  FixtureA := b2Contact_GetFixtureA(Contact);
  FixtureB := b2Contact_GetFixtureB(Contact);

  BodyA := b2Fixture_GetBody(FixtureA);
  BodyB := b2Fixture_GetBody(FixtureB);

  // Vérifier si c'est le joueur et la plateforme
  if (BodyA = FPlayerBody) and (BodyB = FPlatformBody) then
  begin
    PlayerPos := b2Body_GetPosition(FPlayerBody);
    PlatformPos := b2Body_GetPosition(FPlatformBody);
    PlayerVel := b2Body_GetLinearVelocity(FPlayerBody);

    // Si le joueur est en dessous OU monte, désactiver la collision
    if (PlayerPos.y < PlatformPos.y) or (PlayerVel.y > 0) then
      b2Contact_SetEnabled(Contact, False);
  end;
end;
```

### Explosion et forces radiales

```pascal
// Appliquer une force d'explosion à tous les objets dans un rayon
procedure ApplyExplosion(World: b2World; Center: b2Vec2;
                        Radius, Force: Single);
var
  Body: b2Body;
  Pos, Direction: b2Vec2;
  Distance, Strength: Single;
  Impulse: b2Vec2;
begin
  Body := b2World_GetBodyList(World);

  while Body <> nil do
  begin
    if b2Body_GetType(Body) = b2_dynamicBody then
    begin
      Pos := b2Body_GetPosition(Body);

      // Calculer la direction et la distance
      Direction.x := Pos.x - Center.x;
      Direction.y := Pos.y - Center.y;
      Distance := Sqrt(Direction.x * Direction.x + Direction.y * Direction.y);

      // Si dans le rayon
      if Distance < Radius then
      begin
        // Normaliser la direction
        if Distance > 0.001 then
        begin
          Direction.x := Direction.x / Distance;
          Direction.y := Direction.y / Distance;
        end;

        // Force diminue avec la distance
        Strength := Force * (1.0 - Distance / Radius);

        // Appliquer l'impulsion
        Impulse.x := Direction.x * Strength;
        Impulse.y := Direction.y * Strength;

        b2Body_ApplyLinearImpulseToCenter(Body, Impulse, True);
      end;
    end;

    Body := b2Body_GetNext(Body);
  end;
end;

// Utilisation
ApplyExplosion(World, b2Vec2_Make(10, 5), 5.0, 100.0);
```

### Destruction d'objets en toute sécurité

**Important** : Ne jamais détruire un corps pendant un callback de collision !

```pascal
type
  TPhysicsManager = class
  private
    FWorld: b2World;
    FBodiesToDestroy: TList<b2Body>;
  public
    procedure MarkForDestruction(Body: b2Body);
    procedure DestroyMarkedBodies;
  end;

procedure TPhysicsManager.MarkForDestruction(Body: b2Body);  
begin
  if not FBodiesToDestroy.Contains(Body) then
    FBodiesToDestroy.Add(Body);
end;

procedure TPhysicsManager.DestroyMarkedBodies;  
var
  Body: b2Body;
begin
  for Body in FBodiesToDestroy do
    b2World_DestroyBody(FWorld, Body);

  FBodiesToDestroy.Clear;
end;

// Dans la boucle de jeu
procedure Update(dt: Single);  
begin
  b2World_Step(World, dt, 8, 3);

  // Détruire APRÈS la simulation
  PhysicsManager.DestroyMarkedBodies;
end;

// Dans un callback de collision
procedure OnCollision(BulletBody, EnemyBody: b2Body);  
begin
  // NE PAS faire : b2World_DestroyBody(World, BulletBody);

  // Faire :
  PhysicsManager.MarkForDestruction(BulletBody);
  PhysicsManager.MarkForDestruction(EnemyBody);
end;
```

### Filtrage de collisions avancé

#### Système de masques et catégories

```pascal
const
  // Catégories (que suis-je ?)
  CAT_PLAYER    = $0001;  // 0000 0001
  CAT_ENEMY     = $0002;  // 0000 0010
  CAT_BULLET_P  = $0004;  // 0000 0100 (balle joueur)
  CAT_BULLET_E  = $0008;  // 0000 1000 (balle ennemi)
  CAT_GROUND    = $0010;  // 0001 0000
  CAT_POWERUP   = $0020;  // 0010 0000
  CAT_SENSOR    = $0040;  // 0100 0000

  // Masques (avec quoi je collisionne ?)
  MASK_PLAYER   = CAT_GROUND or CAT_ENEMY or CAT_BULLET_E or CAT_POWERUP;
  MASK_ENEMY    = CAT_GROUND or CAT_PLAYER or CAT_BULLET_P;
  MASK_BULLET_P = CAT_ENEMY or CAT_GROUND;
  MASK_BULLET_E = CAT_PLAYER or CAT_GROUND;
  MASK_GROUND   = $FFFF; // Collisionne avec tout
  MASK_POWERUP  = CAT_PLAYER; // Uniquement avec le joueur
  MASK_SENSOR   = CAT_PLAYER; // Détection uniquement

procedure SetupCollisionFilters;  
var
  FixtureDef: b2FixtureDef;
begin
  // Joueur
  FixtureDef := b2DefaultFixtureDef;
  FixtureDef.filter.categoryBits := CAT_PLAYER;
  FixtureDef.filter.maskBits := MASK_PLAYER;
  b2Body_CreateFixture(PlayerBody, @FixtureDef);

  // Balle du joueur
  FixtureDef.filter.categoryBits := CAT_BULLET_P;
  FixtureDef.filter.maskBits := MASK_BULLET_P;
  b2Body_CreateFixture(PlayerBulletBody, @FixtureDef);

  // Capteur (trigger zone)
  FixtureDef.filter.categoryBits := CAT_SENSOR;
  FixtureDef.filter.maskBits := MASK_SENSOR;
  FixtureDef.isSensor := True; // Pas de collision physique
  b2Body_CreateFixture(TriggerBody, @FixtureDef);
end;
```

### Sensors (capteurs/triggers)

Les sensors détectent les collisions sans réaction physique :

```pascal
// Créer une zone de détection
var
  SensorBody: b2Body;
  SensorShape: b2Polygon;
  FixtureDef: b2FixtureDef;
begin
  BodyDef := b2DefaultBodyDef;
  BodyDef.bodyType := b2_staticBody;
  BodyDef.position := b2Vec2_Make(10, 5);
  SensorBody := b2World_CreateBody(World, @BodyDef);

  b2Polygon_SetAsBox(@SensorShape, 2.0, 2.0);

  FixtureDef := b2DefaultFixtureDef;
  FixtureDef.shape := @SensorShape;
  FixtureDef.isSensor := True; // IMPORTANT !

  b2Body_CreateFixture(SensorBody, @FixtureDef);
  b2Body_SetUserData(SensorBody, TriggerZoneData);
end;

// Détecter l'entrée/sortie
procedure TContactListener.BeginContact(Contact: b2Contact);  
var
  FixtureA, FixtureB: b2Fixture;
begin
  FixtureA := b2Contact_GetFixtureA(Contact);
  FixtureB := b2Contact_GetFixtureB(Contact);

  // Vérifier si l'une des fixtures est un sensor
  if b2Fixture_IsSensor(FixtureA) or b2Fixture_IsSensor(FixtureB) then
  begin
    WriteLn('Le joueur est entré dans la zone !');
    // Déclencher un événement, charger un niveau, etc.
  end;
end;

procedure TContactListener.EndContact(Contact: b2Contact);  
begin
  // Le joueur est sorti de la zone
end;
```

### Ragdoll Physics (corps articulé)

Créer un personnage avec des membres articulés :

```pascal
type
  TRagdoll = class
  private
    FHead, FTorso, FLeftArm, FRightArm: btRigidBody;
    FLeftLeg, FRightLeg: btRigidBody;
    FJoints: array of btTypedConstraint;
  public
    constructor Create(World: btDynamicsWorld; Position: TVector3);
    procedure SetPose(Animated: Boolean);
    procedure ApplyForceToLimb(Limb: btRigidBody; Force: btVector3);
  end;

constructor TRagdoll.Create(World: btDynamicsWorld; Position: TVector3);  
var
  Transform: btTransform;
  HeadShape: btSphereShape;
  TorsoShape: btCapsuleShape;
  LimbShape: btCapsuleShape;
  Joint: btHingeConstraint;
begin
  Transform := btTransform_getIdentity;

  // Tête
  Transform.origin := btVector3_Make(Position.X, Position.Y + 1.8, Position.Z);
  HeadShape := btSphereShape_new(0.15);
  FHead := CreateRigidBody(5.0, Transform, HeadShape);

  // Torse
  Transform.origin := btVector3_Make(Position.X, Position.Y + 1.2, Position.Z);
  TorsoShape := btCapsuleShape_new(0.2, 0.6);
  FTorso := CreateRigidBody(10.0, Transform, TorsoShape);

  // Bras gauche
  Transform.origin := btVector3_Make(Position.X - 0.4, Position.Y + 1.2, Position.Z);
  LimbShape := btCapsuleShape_new(0.08, 0.5);
  FLeftArm := CreateRigidBody(2.0, Transform, LimbShape);

  // Connecter tête au torse avec charnière
  Joint := btHingeConstraint_new(
    FTorso, FHead,
    btVector3_Make(0, 0.4, 0),
    btVector3_Make(0, -0.15, 0),
    btVector3_Make(1, 0, 0),
    btVector3_Make(1, 0, 0),
    False
  );
  btHingeConstraint_setLimit(Joint, -Pi/6, Pi/6, 0.9, 0.3, 1.0);
  btDynamicsWorld_addConstraint(World, Joint, True);

  SetLength(FJoints, Length(FJoints) + 1);
  FJoints[High(FJoints)] := Joint;

  // Répéter pour tous les membres...
end;
```

### Water/Buoyancy (flottabilité)

Simuler la flottabilité dans l'eau :

```pascal
type
  TWaterVolume = record
    Y: Single;           // Hauteur de la surface
    Density: Single;     // Densité de l'eau (1000 kg/m³)
    LinearDrag: Single;  // Résistance linéaire
    AngularDrag: Single; // Résistance angulaire
  end;

procedure ApplyBuoyancy(Body: btRigidBody; Water: TWaterVolume);  
var
  Transform: btTransform;
  Pos: btVector3;
  Volume, SubmergedVolume: Single;
  BuoyancyForce, DragForce: btVector3;
  Vel: btVector3;
  Mass: Single;
begin
  Transform := btRigidBody_getWorldTransform(Body);
  Pos := btTransform_getOrigin(Transform);

  // Vérifier si l'objet est dans l'eau
  if Pos.y < Water.Y then
  begin
    Mass := 1.0 / btRigidBody_getInvMass(Body);

    // Calculer le volume submergé (simplifié)
    SubmergedVolume := (Water.Y - Pos.y) * 1.0; // Approximation

    // Force de flottabilité (Principe d'Archimède)
    BuoyancyForce.x := 0;
    BuoyancyForce.y := Water.Density * SubmergedVolume * 9.81;
    BuoyancyForce.z := 0;

    btRigidBody_applyCentralForce(Body, BuoyancyForce);

    // Résistance de l'eau
    Vel := btRigidBody_getLinearVelocity(Body);

    DragForce.x := -Vel.x * Water.LinearDrag;
    DragForce.y := -Vel.y * Water.LinearDrag;
    DragForce.z := -Vel.z * Water.LinearDrag;

    btRigidBody_applyCentralForce(Body, DragForce);

    // Résistance angulaire
    Vel := btRigidBody_getAngularVelocity(Body);
    DragForce.x := -Vel.x * Water.AngularDrag;
    DragForce.y := -Vel.y * Water.AngularDrag;
    DragForce.z := -Vel.z * Water.AngularDrag;

    btRigidBody_applyTorque(Body, DragForce);
  end;
end;

// Dans la boucle de jeu
var
  Water: TWaterVolume;
  Body: btRigidBody;
begin
  Water.Y := 0.0;          // Surface au niveau 0
  Water.Density := 1000.0; // Eau
  Water.LinearDrag := 5.0;
  Water.AngularDrag := 1.0;

  // Appliquer à tous les objets dynamiques
  for Body in DynamicBodies do
    ApplyBuoyancy(Body, Water);
end;
```

## Exemples de jeux complets

### Angry Birds-like (Box2D)

```pascal
type
  TAngryBirdsGame = class
  private
    FWorld: b2World;
    FBird: b2Body;
    FStructure: TList<b2Body>;
    FLaunched: Boolean;
    FLaunchPower: Single;
  public
    constructor Create;
    procedure Launch(Direction: TVector2; Power: Single);
    procedure Update(dt: Single);
    procedure CheckVictory;
  end;

constructor TAngryBirdsGame.Create;  
var
  Gravity: b2Vec2;
  Block: b2Body;
  I: Integer;
begin
  // Créer le monde
  Gravity := b2Vec2_Make(0, -10);
  FWorld := b2World_Create(Gravity);

  // Créer le sol
  CreateGround(FWorld, 0, -5, 50, 1);

  // Créer l'oiseau
  FBird := CreateCircleBody(FWorld, -10, 2, 0.3, 1.0, True);

  // Créer une structure avec des blocs
  FStructure := TList<b2Body>.Create;
  for I := 0 to 9 do
  begin
    Block := CreateBoxBody(FWorld,
                          5 + (I mod 3) * 0.6,
                          -4 + (I div 3) * 0.6,
                          0.25, 0.25, 0.5, True);
    FStructure.Add(Block);
  end;

  FLaunched := False;
end;

procedure TAngryBirdsGame.Launch(Direction: TVector2; Power: Single);  
var
  Impulse: b2Vec2;
begin
  if not FLaunched then
  begin
    Impulse.x := Direction.X * Power;
    Impulse.y := Direction.Y * Power;

    b2Body_ApplyLinearImpulseToCenter(FBird, Impulse, True);
    FLaunched := True;
  end;
end;

procedure TAngryBirdsGame.CheckVictory;  
var
  Block: b2Body;
  AllFallen: Boolean;
begin
  AllFallen := True;

  for Block in FStructure do
  begin
    if b2Body_GetPosition(Block).y > -4.5 then
    begin
      AllFallen := False;
      Break;
    end;
  end;

  if AllFallen then
    WriteLn('Victoire !');
end;
```

### Platformer avec physique (Box2D)

Voir l'exemple complet dans la partie précédente.

### Jeu de billard (Bullet 3D)

```pascal
type
  TBilliardGame = class
  private
    FWorld: btDynamicsWorld;
    FTable: btRigidBody;
    FBalls: array[0..15] of btRigidBody;
    FCue: btRigidBody;
  public
    constructor Create;
    procedure ShootCueBall(Direction: TVector3; Power: Single);
    procedure Update(dt: Single);
    function AllBallsStopped: Boolean;
  end;

constructor TBilliardGame.Create;  
var
  I: Integer;
  BallShape: btSphereShape;
  TableShape: btBoxShape;
  Transform: btTransform;
  X, Z: Single;
begin
  // Créer le monde (gravité normale)
  CreateBulletWorld;

  // Table
  TableShape := btBoxShape_new(btVector3_Make(3.5, 0.1, 7.0));
  Transform := btTransform_getIdentity;
  Transform.origin := btVector3_Make(0, 0, 0);
  FTable := CreateRigidBody(0.0, Transform, TableShape);

  // Friction élevée pour la table
  btRigidBody_setFriction(FTable, 0.8);

  // Créer les boules
  BallShape := btSphereShape_new(0.0286); // Rayon standard d'une boule de billard

  // Boule blanche (cue ball)
  Transform.origin := btVector3_Make(0, 0.2, -5.0);
  FBalls[0] := CreateRigidBody(0.17, Transform, BallShape); // 170g

  // Configurer les propriétés des boules
  btRigidBody_setRestitution(FBalls[0], 0.95);  // Rebond élevé
  btRigidBody_setFriction(FBalls[0], 0.2);       // Friction faible
  btRigidBody_setRollingFriction(FBalls[0], 0.01); // Friction de roulement

  // Disposer les autres boules en triangle
  for I := 1 to 15 do
  begin
    // Calculer la position dans le triangle
    X := ((I - 1) mod 5) * 0.057;
    Z := ((I - 1) div 5) * 0.057;

    Transform.origin := btVector3_Make(X, 0.2, Z + 5.0);
    FBalls[I] := CreateRigidBody(0.17, Transform, BallShape);

    btRigidBody_setRestitution(FBalls[I], 0.95);
    btRigidBody_setFriction(FBalls[I], 0.2);
    btRigidBody_setRollingFriction(FBalls[I], 0.01);
  end;
end;

procedure TBilliardGame.ShootCueBall(Direction: TVector3; Power: Single);  
var
  Impulse: btVector3;
begin
  if AllBallsStopped then
  begin
    // Normaliser la direction
    Direction := Normalize(Direction);

    // Créer l'impulsion
    Impulse.x := Direction.X * Power;
    Impulse.y := 0; // Pas de composante verticale
    Impulse.z := Direction.Z * Power;

    // Appliquer au centre de la boule blanche
    btRigidBody_activate(FBalls[0], True);
    btRigidBody_applyCentralImpulse(FBalls[0], Impulse);
  end;
end;

function TBilliardGame.AllBallsStopped: Boolean;  
var
  I: Integer;
  Vel: btVector3;
  Speed: Single;
const
  SPEED_THRESHOLD = 0.01; // Vitesse minimale considérée comme arrêt
begin
  Result := True;

  for I := 0 to 15 do
  begin
    Vel := btRigidBody_getLinearVelocity(FBalls[I]);
    Speed := Sqrt(Vel.x * Vel.x + Vel.y * Vel.y + Vel.z * Vel.z);

    if Speed > SPEED_THRESHOLD then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TBilliardGame.Update(dt: Single);  
var
  I: Integer;
  Pos: btVector3;
  Vel: btVector3;
begin
  // Simuler la physique
  btDynamicsWorld_stepSimulation(FWorld, dt, 10, 1.0/60.0);

  // Appliquer une friction supplémentaire pour simuler le feutre
  for I := 0 to 15 do
  begin
    Vel := btRigidBody_getLinearVelocity(FBalls[I]);

    // Réduire progressivement la vitesse
    Vel.x := Vel.x * 0.98;
    Vel.z := Vel.z * 0.98;

    btRigidBody_setLinearVelocity(FBalls[I], Vel);

    // Vérifier si les boules tombent dans les poches
    Pos := btRigidBody_getPosition(FBalls[I]);
    if CheckPocketCollision(Pos) then
      HandleBallPocketed(I);
  end;
end;
```

### Jeu de construction/puzzle (Box2D)

Type "World of Goo" ou "Bridge Constructor" :

```pascal
type
  TConstructionGame = class
  private
    FWorld: b2World;
    FNodes: TList<b2Body>;
    FBeams: TList<b2DistanceJoint>;
    FGoal: TVector2;
    FBallBody: b2Body;
  public
    constructor Create;
    procedure AddNode(X, Y: Single);
    procedure ConnectNodes(NodeA, NodeB: b2Body);
    procedure SpawnBall;
    function CheckSuccess: Boolean;
    procedure Update(dt: Single);
  end;

constructor TConstructionGame.Create;  
var
  Gravity: b2Vec2;
begin
  Gravity := b2Vec2_Make(0, -10);
  FWorld := b2World_Create(Gravity);

  FNodes := TList<b2Body>.Create;
  FBeams := TList<b2DistanceJoint>.Create;

  // Créer le sol
  CreateGround(FWorld, 0, -10, 50, 1);

  // Définir l'objectif
  FGoal.X := 20;
  FGoal.Y := 5;
end;

procedure TConstructionGame.AddNode(X, Y: Single);  
var
  BodyDef: b2BodyDef;
  CircleShape: b2Circle;
  FixtureDef: b2FixtureDef;
  Node: b2Body;
begin
  BodyDef := b2DefaultBodyDef;
  BodyDef.bodyType := b2_dynamicBody;
  BodyDef.position := b2Vec2_Make(X, Y);

  Node := b2World_CreateBody(FWorld, @BodyDef);

  CircleShape.center := b2Vec2_Make(0, 0);
  CircleShape.radius := 0.2;

  FixtureDef := b2DefaultFixtureDef;
  FixtureDef.shape := @CircleShape;
  FixtureDef.density := 1.0;
  FixtureDef.friction := 0.3;

  b2Body_CreateFixture(Node, @FixtureDef);

  FNodes.Add(Node);
end;

procedure TConstructionGame.ConnectNodes(NodeA, NodeB: b2Body);  
var
  JointDef: b2DistanceJointDef;
  Joint: b2DistanceJoint;
  PosA, PosB: b2Vec2;
  Distance: Single;
begin
  PosA := b2Body_GetPosition(NodeA);
  PosB := b2Body_GetPosition(NodeB);

  b2DistanceJointDef_Initialize(@JointDef, NodeA, NodeB, PosA, PosB);

  // Calculer la distance
  Distance := Sqrt(Sqr(PosB.x - PosA.x) + Sqr(PosB.y - PosA.y));

  JointDef.length := Distance;
  JointDef.frequencyHz := 4.0;   // Rigidité de la poutre
  JointDef.dampingRatio := 0.5;  // Amortissement

  Joint := b2World_CreateJoint(FWorld, @JointDef);
  FBeams.Add(Joint);
end;

procedure TConstructionGame.SpawnBall;  
var
  BodyDef: b2BodyDef;
  CircleShape: b2Circle;
  FixtureDef: b2FixtureDef;
begin
  BodyDef := b2DefaultBodyDef;
  BodyDef.bodyType := b2_dynamicBody;
  BodyDef.position := b2Vec2_Make(0, 10);

  FBallBody := b2World_CreateBody(FWorld, @BodyDef);

  CircleShape.center := b2Vec2_Make(0, 0);
  CircleShape.radius := 0.5;

  FixtureDef := b2DefaultFixtureDef;
  FixtureDef.shape := @CircleShape;
  FixtureDef.density := 2.0;
  FixtureDef.friction := 0.5;
  FixtureDef.restitution := 0.3;

  b2Body_CreateFixture(FBallBody, @FixtureDef);
end;

function TConstructionGame.CheckSuccess: Boolean;  
var
  BallPos: b2Vec2;
  Distance: Single;
begin
  if FBallBody = nil then
  begin
    Result := False;
    Exit;
  end;

  BallPos := b2Body_GetPosition(FBallBody);

  // Calculer la distance à l'objectif
  Distance := Sqrt(Sqr(BallPos.x - FGoal.X) + Sqr(BallPos.y - FGoal.Y));

  Result := Distance < 1.0; // Dans un rayon de 1 mètre
end;

procedure TConstructionGame.Update(dt: Single);  
var
  Beam: b2DistanceJoint;
  Force: Single;
  MaxStress: Single;
begin
  b2World_Step(FWorld, dt, 8, 3);

  // Vérifier la contrainte sur les poutres
  MaxStress := 100.0; // Force maximale supportée

  for Beam in FBeams do
  begin
    // Obtenir la force de réaction du joint
    Force := b2Joint_GetReactionForce(Beam, 1.0 / dt);

    // Si trop de contrainte, casser la poutre
    if Force > MaxStress then
    begin
      b2World_DestroyJoint(FWorld, Beam);
      FBeams.Remove(Beam);
      WriteLn('Une poutre s''est cassée !');
    end;
  end;

  // Vérifier la victoire
  if CheckSuccess then
    WriteLn('Niveau réussi !');
end;
```

### Destructible terrain (2D pixel-perfect)

Inspiré de Worms ou Destructible environments :

```pascal
type
  TDestructibleTerrain = class
  private
    FWorld: b2World;
    FTerrainData: array of array of Boolean; // true = solide
    FWidth, FHeight: Integer;
    FChunks: TList<b2Body>;
    FPixelSize: Single;
  public
    constructor Create(Width, Height: Integer);
    procedure DestroyCircle(CenterX, CenterY, Radius: Integer);
    procedure RebuildPhysics;
    procedure Draw;
  end;

constructor TDestructibleTerrain.Create(Width, Height: Integer);  
var
  X, Y: Integer;
  Gravity: b2Vec2;
begin
  FWidth := Width;
  FHeight := Height;
  FPixelSize := 0.05; // 1 pixel = 5cm

  SetLength(FTerrainData, Height, Width);
  FChunks := TList<b2Body>.Create;

  Gravity := b2Vec2_Make(0, -10);
  FWorld := b2World_Create(Gravity);

  // Générer un terrain initial
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
    begin
      // Terrain simple : sol en bas
      if Y < Height div 3 then
        FTerrainData[Y, X] := True
      else
        FTerrainData[Y, X] := False;
    end;

  RebuildPhysics;
end;

procedure TDestructibleTerrain.DestroyCircle(CenterX, CenterY, Radius: Integer);  
var
  X, Y: Integer;
  Distance: Single;
begin
  for Y := Max(0, CenterY - Radius) to Min(FHeight - 1, CenterY + Radius) do
    for X := Max(0, CenterX - Radius) to Min(FWidth - 1, CenterX + Radius) do
    begin
      Distance := Sqrt(Sqr(X - CenterX) + Sqr(Y - CenterY));

      if Distance <= Radius then
        FTerrainData[Y, X] := False;
    end;

  // Reconstruire la physique
  RebuildPhysics;
end;

procedure TDestructibleTerrain.RebuildPhysics;  
var
  Chunk: b2Body;
  X, Y: Integer;
  ChunkWidth, ChunkHeight: Integer;
  BodyDef: b2BodyDef;
  BoxShape: b2Polygon;
  StartX, StartY, EndX, EndY: Integer;
begin
  // Détruire les chunks existants
  for Chunk in FChunks do
    b2World_DestroyBody(FWorld, Chunk);
  FChunks.Clear;

  // Algorithme simplifié : créer des rectangles pour les zones solides
  // (Un algorithme réel utiliserait marching squares ou similar)
  ChunkWidth := 32;
  ChunkHeight := 32;

  Y := 0;
  while Y < FHeight do
  begin
    X := 0;
    while X < FWidth do
    begin
      // Vérifier s'il y a du terrain solide ici
      if FTerrainData[Y, X] then
      begin
        // Trouver l'étendue du chunk
        StartX := X;
        StartY := Y;
        EndX := Min(X + ChunkWidth, FWidth);
        EndY := Min(Y + ChunkHeight, FHeight);

        // Créer un corps statique pour ce chunk
        BodyDef := b2DefaultBodyDef;
        BodyDef.bodyType := b2_staticBody;
        BodyDef.position := b2Vec2_Make(
          (StartX + EndX) * 0.5 * FPixelSize,
          (StartY + EndY) * 0.5 * FPixelSize
        );

        Chunk := b2World_CreateBody(FWorld, @BodyDef);

        b2Polygon_SetAsBox(@BoxShape,
          (EndX - StartX) * 0.5 * FPixelSize,
          (EndY - StartY) * 0.5 * FPixelSize
        );

        b2Body_CreateFixture(Chunk, @BoxShape, 0.0);
        FChunks.Add(Chunk);
      end;

      Inc(X, ChunkWidth);
    end;
    Inc(Y, ChunkHeight);
  end;
end;

procedure TDestructibleTerrain.Draw;  
var
  X, Y: Integer;
begin
  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
    begin
      if FTerrainData[Y, X] then
        DrawPixel(X, Y, clBrown);
    end;
end;
```

## Intégration avec les moteurs de jeu

### Intégration Box2D avec Castle Game Engine

```pascal
uses
  CastleVectors, CastleScene, CastleTransform, uBox2D;

type
  TPhysicsTransform = class(TCastleTransform)
  private
    FBody: b2Body;
  public
    procedure Update(const SecondsPassed: Single;
                    var RemoveMe: TRemoveType); override;
    property Body: b2Body read FBody write FBody;
  end;

procedure TPhysicsTransform.Update(const SecondsPassed: Single;
                                  var RemoveMe: TRemoveType);
var
  Pos: b2Vec2;
  Angle: Single;
begin
  inherited;

  if FBody <> nil then
  begin
    // Synchroniser la position depuis Box2D
    Pos := b2Body_GetPosition(FBody);
    Angle := b2Body_GetAngle(FBody);

    Translation := Vector3(
      Pos.x * PIXELS_PER_METER,
      Pos.y * PIXELS_PER_METER,
      0
    );

    Rotation := Vector4(0, 0, 1, Angle);
  end;
end;

// Utilisation
var
  PhysicsWorld: b2World;
  Body: b2Body;
  Transform: TPhysicsTransform;
  Scene: TCastleScene;
begin
  // Créer le corps physique
  Body := CreateDynamicCircle(PhysicsWorld, 5, 5, 0.5);

  // Créer l'objet visuel
  Transform := TPhysicsTransform.Create(Application);
  Transform.Body := Body;

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/models/ball.gltf');
  Transform.Add(Scene);

  Viewport.Items.Add(Transform);
end;
```

### Intégration Bullet avec OpenGL

```pascal
procedure DrawBulletBody(Body: btRigidBody);  
var
  Transform: btTransform;
  Origin: btVector3;
  Rotation: btMatrix3x3;
  Matrix: array[0..15] of GLfloat;
  Shape: btCollisionShape;
  ShapeType: Integer;
begin
  Transform := btRigidBody_getWorldTransform(Body);
  Origin := btTransform_getOrigin(Transform);
  Rotation := btTransform_getBasis(Transform);

  // Convertir en matrice OpenGL
  btTransform_getOpenGLMatrix(Transform, @Matrix[0]);

  glPushMatrix;
  glMultMatrixf(@Matrix[0]);

  // Dessiner selon le type de forme
  Shape := btCollisionObject_getCollisionShape(Body);
  ShapeType := btCollisionShape_getShapeType(Shape);

  case ShapeType of
    BOX_SHAPE_PROXYTYPE:
      DrawBox(btBoxShape(Shape));
    SPHERE_SHAPE_PROXYTYPE:
      DrawSphere(btSphereShape(Shape));
    CYLINDER_SHAPE_PROXYTYPE:
      DrawCylinder(btCylinderShape(Shape));
  end;

  glPopMatrix;
end;

procedure DrawBox(Shape: btBoxShape);  
var
  HalfExtents: btVector3;
begin
  HalfExtents := btBoxShape_getHalfExtentsWithMargin(Shape);

  glBegin(GL_QUADS);
    // Dessiner les 6 faces du cube
    // Face avant
    glNormal3f(0, 0, 1);
    glVertex3f(-HalfExtents.x, -HalfExtents.y, HalfExtents.z);
    glVertex3f(HalfExtents.x, -HalfExtents.y, HalfExtents.z);
    glVertex3f(HalfExtents.x, HalfExtents.y, HalfExtents.z);
    glVertex3f(-HalfExtents.x, HalfExtents.y, HalfExtents.z);
    // ... autres faces
  glEnd;
end;
```

## Ressources et apprentissage

### Documentation officielle

**Box2D** :
- Site officiel : https://box2d.org
- Manuel : https://box2d.org/documentation/
- Code source : https://github.com/erincatto/box2d
- Tutoriels : http://www.iforce2d.net/b2dtut/

**Bullet** :
- Site officiel : https://pybullet.org/wordpress/
- Wiki : https://github.com/bulletphysics/bullet3/wiki
- Code source : https://github.com/bulletphysics/bullet3
- Forum : https://pybullet.org/Bullet/phpBB3/

### Bindings Pascal

- **Box2D pour FreePascal** : https://github.com/Kagamma/box2d-fp
- **Bullet pour Pascal** : Chercher "bullet-pas" ou créer vos propres bindings
- **Alternative** : ZenGL inclut un wrapper pour Chipmunk (similaire à Box2D)

### Livres et tutoriels

**Livres recommandés** :
- "Game Physics Engine Development" - Ian Millington
- "Physics for Game Developers" - David M. Bourg
- "Real-Time Collision Detection" - Christer Ericson

**Tutoriels en ligne** :
- Box2D C++ tutorials (transposables en Pascal)
- iforce2d (excellent site sur Box2D)
- Bullet Physics tutorials
- Game Physics sur YouTube

### Communautés

- **Forum FreePascal** : Questions techniques
- **Lazarus Forum** : Intégration avec l'IDE
- **GameDev Stack Exchange** : Questions générales sur la physique
- **Reddit** : r/gamedev, r/freepascal

## Alternatives et outils complémentaires

### Autres moteurs physiques

**Chipmunk2D** :
- Léger et rapide
- Alternative à Box2D
- Licence MIT

**ODE (Open Dynamics Engine)** :
- Physique 3D
- Moins moderne que Bullet
- Bonne stabilité

**PhysX** :
- NVIDIA
- Très performant avec GPU
- Complexe à intégrer

### Éditeurs de niveaux avec physique

**R.U.B.E. (Really Useful Box2D Editor)** :
- Éditeur visuel pour Box2D
- Export JSON/autres formats
- Payant mais excellent
- https://www.iforce2d.net/rube/

**Physics Body Editor** :
- Gratuit
- Créer des formes de collision à partir d'images
- Export vers plusieurs formats

### Outils de debug

**Box2D Debug Draw** :
- Voir les formes de collision
- Afficher les forces et vitesses
- Visualiser les joints

**Bullet Debug Drawer** :
- Wireframe 3D
- AABB visualization
- Contact points

## Performances et optimisation avancées

### Profiling de la physique

```pascal
var
  PhysicsTime: Int64;

procedure MeasurePhysics;  
var
  StartTime, EndTime: Int64;
begin
  StartTime := GetTickCount64;

  // Simulation physique
  b2World_Step(World, 1.0/60.0, 8, 3);

  EndTime := GetTickCount64;
  PhysicsTime := EndTime - StartTime;

  if PhysicsTime > 16 then // Plus de 16ms = problème à 60 FPS
    WriteLn('Warning: Physics took ', PhysicsTime, ' ms');
end;
```

### Techniques d'optimisation avancées

**1. Spatial hashing pour broadphase personnalisée**
```pascal
type
  TSpatialHash = class
  private
    FCells: TDictionary<Integer, TList<b2Body>>;
    FCellSize: Single;
  public
    procedure Clear;
    procedure Insert(Body: b2Body);
    function Query(const Bounds: TRect): TList<b2Body>;
  end;
```

**2. Island splitting**
- Box2D et Bullet divisent automatiquement le monde en "îles"
- Les îles sans mouvement sont mises en veille
- Optimise drastiquement les calculs

**3. Simplification de mesh**
- Réduire les triangles des mesh complexes
- Utiliser des formes convexes approximatives
- Décomposition convexe pour formes concaves

**4. Time scaling**
```pascal
var
  TimeScale: Single = 1.0;

procedure UpdatePhysics(dt: Single);  
begin
  // Ralenti (slow motion)
  if KeyPressed(KEY_SHIFT) then
    TimeScale := 0.3
  else
    TimeScale := 1.0;

  b2World_Step(World, dt * TimeScale, 8, 3);
end;
```

## Conclusion

Les moteurs physiques Box2D et Bullet sont des outils puissants et essentiels pour créer des jeux avec des comportements physiques réalistes. Leur intégration avec FreePascal permet de développer des jeux professionnels performants.

### Points clés à retenir

**Box2D (2D)** :
- ✅ Parfait pour les jeux 2D
- ✅ Simple à apprendre
- ✅ Excellent pour puzzles et platformers
- ✅ Documentation exhaustive
- ⚠️ Uniquement 2D

**Bullet (3D)** :
- ✅ Moteur 3D complet
- ✅ Utilisé professionnellement
- ✅ Véhicules et ragdolls inclus
- ✅ Très performant
- ⚠️ Courbe d'apprentissage plus raide

### Recommandations finales

1. **Commencez simple** : Maîtrisez les bases avant les features avancées
2. **Testez constamment** : La physique peut être imprévisible
3. **Optimisez intelligemment** : Profiling avant optimisation
4. **Utilisez le debug draw** : Visualisez ce qui se passe
5. **Lisez la documentation** : Elle contient des années de connaissances
6. **Rejoignez la communauté** : Posez des questions, partagez vos solutions

### Prochaines étapes

Maintenant que vous maîtrisez les moteurs physiques, vous pouvez :

1. Créer un prototype de jeu simple avec physique
2. Expérimenter avec différents types de joints
3. Implémenter un système de destruction
4. Ajouter des véhicules ou ragdolls
5. Optimiser pour les performances
6. Publier votre premier jeu avec physique réaliste

La physique dans les jeux ouvre un monde de possibilités créatives. Que vous créiez un puzzle game relaxant, un platformer dynamique, ou une simulation de destruction spectaculaire, Box2D et Bullet vous donnent les outils nécessaires pour réaliser votre vision.

**Bon développement et amusez-vous avec la physique !** ⚛️🎮

---

## Ressources rapides

### Snippets utiles à garder sous la main

```pascal
// Créer un corps Box2D simple
function QuickBody2D(World: b2World; X, Y, W, H: Single;
                    Dynamic: Boolean): b2Body;
// Créer un corps Bullet simple
function QuickBody3D(World: btDynamicsWorld; X, Y, Z, Radius: Single;
                    Mass: Single): btRigidBody;
// Vérifier collision entre deux corps
function BodiesColliding(BodyA, BodyB: b2Body): Boolean;
// Appliquer une explosion
procedure Explode(Center: TVector; Radius, Force: Single);
```

### Checklist de débogage physique

- [ ] Les corps ont-ils une masse appropriée ?
- [ ] Les formes de collision sont-elles correctes ?
- [ ] Le timestep est-il fixe ?
- [ ] Les itérations sont-elles suffisantes ?
- [ ] Y a-t-il du tunneling ?
- [ ] Les filtres de collision sont-ils corrects ?
- [ ] Le debug draw est-il activé ?
- [ ] Les unités sont-elles appropriées (MKS) ?
- [ ] Les corps dorment-ils correctement ?
- [ ] Les contraintes sont-elles stables ?

⏭️ [Audio multi-plateforme](/23-developpement-jeux/04-audio-multiplateforme.md)
