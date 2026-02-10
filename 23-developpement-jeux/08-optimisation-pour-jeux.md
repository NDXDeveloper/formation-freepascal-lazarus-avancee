🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.8 Optimisation pour jeux

## Introduction

L'optimisation est l'art de rendre votre jeu plus rapide, plus fluide et moins gourmand en ressources. Un jeu bien optimisé offre une meilleure expérience utilisateur et peut fonctionner sur une plus grande variété de matériels.

### Pourquoi optimiser ?

- **Fluidité** : Maintenir 60 FPS (images par seconde) minimum
- **Portabilité** : Fonctionner sur des configurations modestes
- **Batterie** : Réduire la consommation sur ordinateurs portables et mobiles
- **Accessibilité** : Permettre à plus de joueurs de profiter de votre jeu
- **Professionnalisme** : Un jeu optimisé montre votre sérieux

### Quand optimiser ?

**Règle d'or** : "Premature optimization is the root of all evil" (Donald Knuth)

1. **D'abord** : Faites fonctionner votre jeu correctement
2. **Ensuite** : Mesurez les performances
3. **Puis** : Optimisez les vrais goulots d'étranglement
4. **Enfin** : Mesurez à nouveau pour vérifier l'amélioration

N'optimisez jamais sans mesurer ! Vous risquez de perdre du temps sur des parties qui ne posent pas de problème.

## Comprendre les performances

### Les deux ressources principales

#### Le CPU (Processeur)

Le CPU gère la **logique** du jeu :
- Calculs physiques
- Intelligence artificielle
- Gestion des entrées
- Logique de gameplay
- Scripts

#### Le GPU (Carte graphique)

Le GPU gère le **rendu** :
- Affichage des sprites et modèles
- Effets visuels
- Shaders
- Post-processing

### Mesurer les performances

#### FPS (Frames Per Second)

Les FPS mesurent combien d'images sont affichées par seconde.

```pascal
type
  TFPSCounter = class
  private
    FFrameCount: Integer;
    FLastTime: QWord;
    FFPS: Integer;
  public
    procedure Update;
    property FPS: Integer read FFPS;
  end;

procedure TFPSCounter.Update;
var
  CurrentTime: QWord;
  ElapsedMS: QWord;
begin
  Inc(FFrameCount);
  CurrentTime := GetTickCount64;
  ElapsedMS := CurrentTime - FLastTime;

  // Mettre à jour chaque seconde
  if ElapsedMS >= 1000 then
  begin
    FFPS := Round(FFrameCount * 1000 / ElapsedMS);
    FFrameCount := 0;
    FLastTime := CurrentTime;
  end;
end;
```

**Objectifs de FPS** :
- **60 FPS** : Idéal pour la plupart des jeux
- **30 FPS** : Minimum acceptable
- **120+ FPS** : Pour jeux compétitifs ou VR

#### Frame time

Le **frame time** est le temps nécessaire pour calculer et afficher une image.

```pascal
procedure MeasureFrameTime;
var
  StartTime, EndTime: QWord;
  FrameTimeMS: Double;
begin
  StartTime := GetTickCount64;

  // Votre boucle de jeu ici
  UpdateGame;
  RenderGame;

  EndTime := GetTickCount64;
  FrameTimeMS := EndTime - StartTime;

  WriteLn(Format('Frame time: %.2f ms', [FrameTimeMS]));
end;
```

**Pour 60 FPS**, vous disposez de **16,67 ms** par frame.

#### Profiling

Le **profiling** identifie où le temps est dépensé.

```pascal
type
  TProfiler = class
  private
    FSections: TStringList;
    FTimes: array of QWord;
  public
    procedure StartSection(const Name: string);
    procedure EndSection(const Name: string);
    procedure PrintReport;
  end;

// Utilisation
Profiler.StartSection('Physics');
UpdatePhysics;
Profiler.EndSection('Physics');

Profiler.StartSection('Rendering');
RenderScene;
Profiler.EndSection('Rendering');

Profiler.PrintReport;
```

## Optimisation CPU

### 1. Réduire les allocations mémoire

Les allocations mémoire fréquentes ralentissent le jeu.

#### Problème : Créer/détruire des objets en boucle

```pascal
// ❌ MAUVAIS : Allocation à chaque frame
procedure UpdateParticles;
var
  i: Integer;
  Particle: TParticle;
begin
  for i := 0 to 100 do
  begin
    Particle := TParticle.Create;  // Allocation !
    Particle.Update;
    Particle.Free;  // Désallocation !
  end;
end;
```

#### Solution : Object Pooling

```pascal
type
  TParticlePool = class
  private
    FPool: array of TParticle;
    FActiveCount: Integer;
  public
    constructor Create(MaxParticles: Integer);
    function GetParticle: TParticle;
    procedure ReleaseParticle(Particle: TParticle);
  end;

constructor TParticlePool.Create(MaxParticles: Integer);
var
  i: Integer;
begin
  SetLength(FPool, MaxParticles);
  for i := 0 to MaxParticles - 1 do
    FPool[i] := TParticle.Create;  // Création une seule fois
  FActiveCount := 0;
end;

function TParticlePool.GetParticle: TParticle;
begin
  if FActiveCount < Length(FPool) then
  begin
    Result := FPool[FActiveCount];
    Inc(FActiveCount);
  end
  else
    Result := nil;
end;

procedure TParticlePool.ReleaseParticle(Particle: TParticle);
begin
  // Remettre la particule dans le pool
  Particle.Reset;
  Dec(FActiveCount);
end;
```

### 2. Optimiser les boucles

#### Éviter les calculs répétitifs

```pascal
// ❌ MAUVAIS : Calcul répété
for i := 0 to EntityCount - 1 do
begin
  Distance := Sqrt(Sqr(Entities[i].X - Player.X) + Sqr(Entities[i].Y - Player.Y));
  if Distance < 100 then
    // ...
end;

// ✅ BON : Comparer les carrés (évite Sqrt)
SqrRange := 100 * 100;
for i := 0 to EntityCount - 1 do
begin
  SqrDistance := Sqr(Entities[i].X - Player.X) + Sqr(Entities[i].Y - Player.Y);
  if SqrDistance < SqrRange then
    // ...
end;
```

#### Sortir des boucles tôt

```pascal
// ❌ MAUVAIS : Continue même après avoir trouvé
Found := False;
for i := 0 to 1000 do
begin
  if Items[i].ID = SearchID then
    Found := True;
end;

// ✅ BON : Sort dès qu'on trouve
Found := False;
for i := 0 to 1000 do
begin
  if Items[i].ID = SearchID then
  begin
    Found := True;
    Break;  // Sortie anticipée
  end;
end;
```

### 3. Spatial partitioning

Pour les collisions et la recherche, divisez l'espace en grilles.

#### Sans optimisation

```pascal
// ❌ LENT : O(n²) - Vérifie toutes les paires
for i := 0 to EntityCount - 1 do
  for j := i + 1 to EntityCount - 1 do
    if CheckCollision(Entities[i], Entities[j]) then
      HandleCollision(Entities[i], Entities[j]);
```

#### Avec grille spatiale

```pascal
type
  TSpatialGrid = class
  private
    FCellSize: Integer;
    FCells: array of array of TList;
  public
    procedure AddEntity(Entity: TEntity);
    procedure GetNearbyEntities(X, Y: Integer; out Nearby: TList);
  end;

// Seuls les objets dans les cellules voisines sont vérifiés
Grid.GetNearbyEntities(Player.X, Player.Y, NearbyList);
for i := 0 to NearbyList.Count - 1 do
begin
  Enemy := TEnemy(NearbyList[i]);
  if CheckCollision(Player, Enemy) then
    HandleCollision(Player, Enemy);
end;
```

### 4. Update sélectif

Ne pas tout mettre à jour chaque frame.

```pascal
type
  TEntity = class
  private
    FUpdateInterval: Integer;  // Mettre à jour tous les X frames
    FFramesSinceUpdate: Integer;
  public
    procedure Update; virtual;
  end;

procedure TEntity.Update;
begin
  Inc(FFramesSinceUpdate);

  if FFramesSinceUpdate >= FUpdateInterval then
  begin
    // Mise à jour réelle
    DoActualUpdate;
    FFramesSinceUpdate := 0;
  end;
end;

// Utilisation
DistantEnemy.UpdateInterval := 5;   // Mise à jour tous les 5 frames
NearbyEnemy.UpdateInterval := 1;     // Mise à jour chaque frame
```

### 5. Inline et fonctions simples

Pour les fonctions appelées très souvent, utilisez `inline`.

```pascal
function Distance(X1, Y1, X2, Y2: Single): Single; inline;
begin
  Result := Sqrt(Sqr(X2 - X1) + Sqr(Y2 - Y1));
end;

function SqrDistance(X1, Y1, X2, Y2: Single): Single; inline;
begin
  Result := Sqr(X2 - X1) + Sqr(Y2 - Y1);
end;
```

### 6. Éviter les conversions de types

```pascal
// ❌ LENT : Conversions répétées
procedure DrawCircle(X, Y, Radius: Integer);
var
  Angle: Single;
  i: Integer;
begin
  for i := 0 to 360 do
  begin
    Angle := i * Pi / 180;  // Conversion à chaque fois
    DrawPixel(Round(X + Cos(Angle) * Radius),
              Round(Y + Sin(Angle) * Radius));
  end;
end;

// ✅ RAPIDE : Précalculer
var
  CosTable, SinTable: array[0..360] of Single;

procedure InitTrigTables;
var
  i: Integer;
begin
  for i := 0 to 360 do
  begin
    CosTable[i] := Cos(i * Pi / 180);
    SinTable[i] := Sin(i * Pi / 180);
  end;
end;

procedure DrawCircle(X, Y, Radius: Integer);
var
  i: Integer;
begin
  for i := 0 to 360 do
    DrawPixel(Round(X + CosTable[i] * Radius),
              Round(Y + SinTable[i] * Radius));
end;
```

## Optimisation GPU et rendu

### 1. Batch rendering

Regrouper les appels de dessin identiques.

```pascal
// ❌ MAUVAIS : Un appel par sprite
for i := 0 to 1000 do
begin
  SelectTexture(Sprites[i].Texture);
  DrawSprite(Sprites[i]);
end;

// ✅ BON : Grouper par texture
SortSpritesByTexture(Sprites);
CurrentTexture := nil;

for i := 0 to High(Sprites) do
begin
  if Sprites[i].Texture <> CurrentTexture then
  begin
    if CurrentTexture <> nil then
      FlushBatch;  // Dessiner tout d'un coup

    CurrentTexture := Sprites[i].Texture;
    SelectTexture(CurrentTexture);
  end;

  AddToBatch(Sprites[i]);
end;

FlushBatch;  // Dernière série
```

### 2. Culling (Élimination)

Ne dessiner que ce qui est visible.

#### Frustum culling

```pascal
function IsInView(Entity: TEntity; Camera: TCamera): Boolean;
begin
  Result := (Entity.X + Entity.Width > Camera.X) and
            (Entity.X < Camera.X + Camera.Width) and
            (Entity.Y + Entity.Height > Camera.Y) and
            (Entity.Y < Camera.Y + Camera.Height);
end;

// Utilisation
for i := 0 to EntityCount - 1 do
begin
  if IsInView(Entities[i], Camera) then
    DrawEntity(Entities[i]);
end;
```

#### Occlusion culling

Ne pas dessiner ce qui est caché derrière d'autres objets.

```pascal
procedure RenderWithOcclusion;
begin
  // Trier du plus proche au plus loin
  SortEntitiesByDepth(Entities);

  for i := 0 to High(Entities) do
  begin
    if not IsOccluded(Entities[i]) then
    begin
      DrawEntity(Entities[i]);
      MarkAsOccluder(Entities[i]);  // Peut masquer d'autres objets
    end;
  end;
end;
```

### 3. Level of Detail (LOD)

Utiliser des versions simplifiées pour les objets lointains.

```pascal
type
  TLODLevel = (lodHigh, lodMedium, lodLow);

function GetLODLevel(Entity: TEntity; Camera: TCamera): TLODLevel;
var
  Distance: Single;
begin
  Distance := CalculateDistance(Entity, Camera);

  if Distance < 200 then
    Result := lodHigh
  else if Distance < 500 then
    Result := lodMedium
  else
    Result := lodLow;
end;

procedure DrawEntity(Entity: TEntity; LOD: TLODLevel);
begin
  case LOD of
    lodHigh:   DrawHighDetail(Entity);
    lodMedium: DrawMediumDetail(Entity);
    lodLow:    DrawLowDetail(Entity);
  end;
end;
```

### 4. Texture atlases

Combiner plusieurs textures en une seule grande texture.

```pascal
type
  TTextureAtlas = class
  private
    FTexture: TTexture;
    FRegions: array of TRect;
  public
    function GetRegion(Index: Integer): TRect;
    procedure DrawFromAtlas(Index: Integer; X, Y: Integer);
  end;

procedure TTextureAtlas.DrawFromAtlas(Index: Integer; X, Y: Integer);
var
  SrcRect: TRect;
begin
  SrcRect := FRegions[Index];
  DrawTextureRegion(FTexture, SrcRect, X, Y);
end;
```

**Avantages** :
- Moins de changements de texture
- Meilleure utilisation de la mémoire GPU
- Chargement plus rapide

### 5. Optimisation des shaders

Pour les effets personnalisés, écrivez des shaders efficaces.

```glsl
// ❌ LENT : Calculs complexes par pixel
void fragment() {
    vec4 color = texture(TEXTURE, UV);
    float brightness = (color.r + color.g + color.b) / 3.0;

    // Effet coûteux appliqué à chaque pixel
    for (int i = 0; i < 10; i++) {
        brightness = sin(brightness * 3.14159);
    }

    COLOR = vec4(vec3(brightness), 1.0);
}

// ✅ RAPIDE : Calculs simplifiés
void fragment() {
    vec4 color = texture(TEXTURE, UV);
    float brightness = dot(color.rgb, vec3(0.299, 0.587, 0.114));
    COLOR = vec4(vec3(brightness), 1.0);
}
```

### 6. Réduire les overdraw

L'**overdraw** se produit quand un pixel est dessiné plusieurs fois.

```pascal
// Trier par profondeur (Z-order)
procedure SortByDepth(var Sprites: array of TSprite);
begin
  // Trier du plus loin au plus proche
  QuickSort(Sprites, @CompareDepth);
end;

// Dessiner d'arrière en avant
for i := 0 to High(Sprites) do
  DrawSprite(Sprites[i]);
```

Pour les jeux 2D, utilisez **Z-ordering** ou **layers**.

## Optimisation mémoire

### 1. Streaming de ressources

Charger et décharger les ressources selon les besoins.

```pascal
type
  TResourceManager = class
  private
    FLoadedTextures: TStringList;
    FMemoryLimit: Int64;
    FCurrentMemory: Int64;
  public
    function LoadTexture(const Filename: string): TTexture;
    procedure UnloadTexture(const Filename: string);
    procedure CheckMemoryLimit;
  end;

procedure TResourceManager.CheckMemoryLimit;
begin
  while FCurrentMemory > FMemoryLimit do
  begin
    // Décharger les textures les moins récemment utilisées
    UnloadLeastRecentlyUsed;
  end;
end;
```

### 2. Compression des données

Utiliser des formats compressés quand c'est possible.

```pascal
// PNG pour les textures (compression sans perte)
// OGG pour l'audio (compression avec perte acceptable)
// ZIP pour les packs de données

procedure LoadCompressedLevel(const Filename: string);
var
  Unzipper: TUnZipper;
  TempDir: string;
begin
  TempDir := GetTempDir + 'level_temp/';

  Unzipper := TUnZipper.Create;
  try
    Unzipper.FileName := Filename;
    Unzipper.OutputPath := TempDir;
    Unzipper.UnZipAllFiles;

    LoadLevelFromDirectory(TempDir);
  finally
    Unzipper.Free;
    DeleteDirectory(TempDir);
  end;
end;
```

### 3. Gestion du cache

Mettre en cache les calculs coûteux.

```pascal
type
  TPathfindingCache = class
  private
    FCache: TDictionary<string, TPath>;
  public
    function GetPath(StartX, StartY, EndX, EndY: Integer): TPath;
    procedure ClearCache;
  end;

function TPathfindingCache.GetPath(StartX, StartY, EndX, EndY: Integer): TPath;
var
  Key: string;
begin
  Key := Format('%d,%d-%d,%d', [StartX, StartY, EndX, EndY]);

  if FCache.ContainsKey(Key) then
    Result := FCache[Key]  // Chemin déjà calculé
  else
  begin
    Result := CalculatePathAStar(StartX, StartY, EndX, EndY);
    FCache.Add(Key, Result);
  end;
end;
```

## Optimisations audio

### 1. Limiter les sources audio

```pascal
const
  MAX_AUDIO_SOURCES = 32;

type
  TAudioManager = class
  private
    FActiveSources: array[0..MAX_AUDIO_SOURCES-1] of TAudioSource;
  public
    procedure PlaySound(Sound: TSound; Priority: Integer);
  end;

procedure TAudioManager.PlaySound(Sound: TSound; Priority: Integer);
var
  i, LowestPriorityIndex: Integer;
  LowestPriority: Integer;
begin
  // Chercher un slot libre
  for i := 0 to MAX_AUDIO_SOURCES - 1 do
  begin
    if not FActiveSources[i].IsPlaying then
    begin
      FActiveSources[i].Play(Sound);
      Exit;
    end;
  end;

  // Tous les slots occupés : remplacer le son de priorité la plus basse
  LowestPriority := MaxInt;
  for i := 0 to MAX_AUDIO_SOURCES - 1 do
  begin
    if FActiveSources[i].Priority < LowestPriority then
    begin
      LowestPriority := FActiveSources[i].Priority;
      LowestPriorityIndex := i;
    end;
  end;

  if Priority > LowestPriority then
    FActiveSources[LowestPriorityIndex].Play(Sound);
end;
```

### 2. Audio streaming

Pour la musique et les longs effets sonores.

```pascal
type
  TStreamingAudio = class
  private
    FBuffer: array[0..1] of TAudioBuffer;  // Double buffering
    FCurrentBuffer: Integer;
    FStream: TFileStream;
  public
    procedure Update;
    procedure FillBuffer(BufferIndex: Integer);
  end;

procedure TStreamingAudio.Update;
begin
  if BufferFinished(FCurrentBuffer) then
  begin
    FillBuffer(FCurrentBuffer);
    PlayBuffer(FCurrentBuffer);
    FCurrentBuffer := 1 - FCurrentBuffer;  // Alterner entre 0 et 1
  end;
end;
```

## Optimisations multi-plateforme

### Windows

```pascal
{$IFDEF WINDOWS}
// Désactiver le compositeur Windows pour réduire la latence
procedure DisableCompositor;
begin
  DwmEnableComposition(DWM_EC_DISABLECOMPOSITION);
end;

// Augmenter la priorité du processus
procedure SetHighPriority;
begin
  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
end;
{$ENDIF}
```

### Linux

```pascal
{$IFDEF LINUX}
// Utiliser des timers haute résolution
procedure SetHighResolutionTimer;
begin
  // Configuration spécifique Linux
  clock_gettime(CLOCK_MONOTONIC, @StartTime);
end;

// Désactiver le compositing (sur X11)
procedure DisableCompositing;
begin
  // Envoyer des commandes X11 pour désactiver le compositing
  system('xfwm4 --compositor=off');
end;
{$ENDIF}
```

## Profiling avancé

### Valgrind (Linux)

```bash
# Profiling de performance
valgrind --tool=callgrind ./MonJeu

# Analyse de mémoire
valgrind --leak-check=full ./MonJeu
```

### Outils Windows

```pascal
{$IFDEF WINDOWS}
// Utiliser QueryPerformanceCounter pour un timing précis
var
  Frequency, StartCount, EndCount: Int64;

procedure InitPreciseTiming;
begin
  QueryPerformanceFrequency(Frequency);
end;

procedure StartTiming;
begin
  QueryPerformanceCounter(StartCount);
end;

function GetElapsedMS: Double;
begin
  QueryPerformanceCounter(EndCount);
  Result := ((EndCount - StartCount) * 1000.0) / Frequency;
end;
{$ENDIF}
```

## Optimisations spécifiques aux jeux

### 1. Delta Time

Utiliser un **delta time** pour un gameplay indépendant du framerate.

```pascal
var
  LastTime, CurrentTime: QWord;
  DeltaTime: Single;

procedure UpdateGame;
begin
  CurrentTime := GetTickCount64;
  DeltaTime := (CurrentTime - LastTime) / 1000.0;  // En secondes
  LastTime := CurrentTime;

  // Utiliser DeltaTime dans les calculs
  Player.X := Player.X + Player.VelocityX * DeltaTime;
  Player.Y := Player.Y + Player.VelocityY * DeltaTime;
end;
```

### 2. Frame limiting

Limiter les FPS pour économiser les ressources.

```pascal
const
  TARGET_FPS = 60;
  FRAME_TIME = 1000 div TARGET_FPS;  // ~16.67 ms

procedure GameLoop;
var
  FrameStart, FrameEnd, SleepTime: QWord;
begin
  while Running do
  begin
    FrameStart := GetTickCount64;

    UpdateGame;
    RenderGame;

    FrameEnd := GetTickCount64;
    SleepTime := FRAME_TIME - (FrameEnd - FrameStart);

    if SleepTime > 0 then
      Sleep(SleepTime);
  end;
end;
```

### 3. VSync

Synchroniser avec le rafraîchissement de l'écran.

```pascal
// Avec OpenGL
procedure EnableVSync;
begin
  {$IFDEF WINDOWS}
  wglSwapIntervalEXT(1);  // 1 = VSync activé
  {$ENDIF}

  {$IFDEF LINUX}
  glXSwapIntervalEXT(Display, Window, 1);
  {$ENDIF}
end;
```

## Optimisation de l'IA

### 1. Simplifier les calculs

```pascal
// ❌ LENT : Pathfinding complet chaque frame
procedure UpdateEnemy;
begin
  Path := FindPath(Enemy.Position, Player.Position);
  Enemy.FollowPath(Path);
end;

// ✅ RAPIDE : Pathfinding par intervalles
procedure UpdateEnemy;
begin
  Inc(Enemy.FramesSincePathUpdate);

  if Enemy.FramesSincePathUpdate >= 30 then  // Tous les 30 frames
  begin
    Path := FindPath(Enemy.Position, Player.Position);
    Enemy.FramesSincePathUpdate := 0;
  end;

  Enemy.FollowPath(Path);
end;
```

### 2. Behaviour trees optimisés

```pascal
type
  TBehaviourNode = class
  private
    FLastEvaluation: Integer;
    FEvaluationInterval: Integer;
    FCachedResult: TNodeStatus;
  public
    function Evaluate: TNodeStatus; virtual;
  end;

function TBehaviourNode.Evaluate: TNodeStatus;
begin
  Inc(FLastEvaluation);

  if FLastEvaluation >= FEvaluationInterval then
  begin
    FCachedResult := DoActualEvaluation;
    FLastEvaluation := 0;
  end;

  Result := FCachedResult;
end;
```

## Optimisation physique

### 1. Sleep/Wake pour objets statiques

```pascal
type
  TPhysicsBody = class
  private
    FVelocity: TVector2;
    FIsAsleep: Boolean;
    FSleepTimer: Single;
  public
    procedure Update(DeltaTime: Single);
  end;

procedure TPhysicsBody.Update(DeltaTime: Single);
begin
  // Si l'objet bouge peu, le mettre en sommeil
  if (Abs(FVelocity.X) < 0.1) and (Abs(FVelocity.Y) < 0.1) then
  begin
    FSleepTimer := FSleepTimer + DeltaTime;

    if FSleepTimer > 2.0 then  // 2 secondes sans mouvement
      FIsAsleep := True;
  end
  else
  begin
    FSleepTimer := 0;
    FIsAsleep := False;
  end;

  // Ne pas calculer la physique pour les objets endormis
  if not FIsAsleep then
    UpdatePhysics(DeltaTime);
end;
```

### 2. Broad phase collision

Utiliser une détection en deux phases.

```pascal
// Phase 1 : Broad phase (AABB simple)
function AABBIntersect(A, B: TEntity): Boolean;
begin
  Result := (A.X < B.X + B.Width) and
            (A.X + A.Width > B.X) and
            (A.Y < B.Y + B.Height) and
            (A.Y + A.Height > B.Y);
end;

// Phase 2 : Narrow phase (collision précise)
procedure CheckCollisions;
var
  i, j: Integer;
begin
  for i := 0 to EntityCount - 1 do
    for j := i + 1 to EntityCount - 1 do
    begin
      // Broad phase rapide
      if AABBIntersect(Entities[i], Entities[j]) then
      begin
        // Narrow phase précise seulement si nécessaire
        if PreciseCollision(Entities[i], Entities[j]) then
          HandleCollision(Entities[i], Entities[j]);
      end;
    end;
end;
```

## Astuces générales

### 1. Précalculer ce qui ne change pas

```pascal
// Précalculer au démarrage
var
  SineWave: array[0..359] of Single;

procedure InitTables;
var
  i: Integer;
begin
  for i := 0 to 359 do
    SineWave[i] := Sin(i * Pi / 180);
end;
```

### 2. Utiliser des types appropriés

```pascal
// ❌ Trop précis pour le gameplay
var
  PlayerX: Double;

// ✅ Single suffit et est plus rapide
var
  PlayerX: Single;

// ✅ Integer pour les positions pixel
var
  PlayerX: Integer;
```

### 3. Éviter les divisions

```pascal
// ❌ LENT : Division
HalfWidth := Width / 2;

// ✅ RAPIDE : Multiplication
HalfWidth := Width * 0.5;
```

### 4. Fast inverse square root

Pour normaliser des vecteurs rapidement.

```pascal
function FastInvSqrt(x: Single): Single;
var
  i: Integer absolute x;
  xhalf: Single;
begin
  xhalf := 0.5 * x;
  i := $5f3759df - (i shr 1);
  Result := x * (1.5 - xhalf * x * x);
end;
```

## Debugging des performances

### 1. Affichage des stats en jeu

```pascal
procedure DrawDebugInfo(Canvas: TCanvas);
begin
  Canvas.TextOut(10, 10, Format('FPS: %d', [FPS]));
  Canvas.TextOut(10, 30, Format('Entities: %d', [EntityCount]));
  Canvas.TextOut(10, 50, Format('Draw calls: %d', [DrawCallCount]));
  Canvas.TextOut(10, 70, Format('Memory: %.2f MB', [GetMemoryUsage / 1024 / 1024]));
  Canvas.TextOut(10, 90, Format('Frame time: %.2f ms', [FrameTimeMS]));
  Canvas.TextOut(10, 110, Format('Update: %.2f ms', [UpdateTimeMS]));
  Canvas.TextOut(10, 130, Format('Render: %.2f ms', [RenderTimeMS]));
end;
```

### 2. Graphiques de performances

```pascal
type
  TPerformanceGraph = class
  private
    FHistory: array[0..99] of Single;  // 100 derniers frames
    FPosition: Integer;
    FMaxValue: Single;
  public
    procedure AddValue(Value: Single);
    procedure Draw(Canvas: TCanvas; X, Y, Width, Height: Integer);
  end;

procedure TPerformanceGraph.AddValue(Value: Single);
begin
  FHistory[FPosition] := Value;
  FPosition := (FPosition + 1) mod 100;

  if Value > FMaxValue then
    FMaxValue := Value;
end;

procedure TPerformanceGraph.Draw(Canvas: TCanvas; X, Y, Width, Height: Integer);
var
  i, BarX, BarHeight: Integer;
  Value: Single;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(X, Y, X + Width, Y + Height);

  Canvas.Pen.Color := clGreen;

  for i := 0 to 99 do
  begin
    Value := FHistory[(FPosition + i) mod 100];
    BarX := X + (i * Width div 100);
    BarHeight := Round((Value / FMaxValue) * Height);

    Canvas.Line(BarX, Y + Height, BarX, Y + Height - BarHeight);
  end;

  // Ligne de référence 60 FPS = 16.67 ms
  Canvas.Pen.Color := clRed;
  BarHeight := Round((16.67 / FMaxValue) * Height);
  Canvas.Line(X, Y + Height - BarHeight, X + Width, Y + Height - BarHeight);
end;
```

### 3. Mode de débogage conditionnel

```pascal
{$IFDEF DEBUG}
const
  ENABLE_DEBUG_DRAW = True;
  ENABLE_PROFILING = True;
{$ELSE}
const
  ENABLE_DEBUG_DRAW = False;
  ENABLE_PROFILING = False;
{$ENDIF}

procedure DrawEntity(Entity: TEntity);
begin
  // Rendu normal
  Entity.Draw;

  // Informations de debug uniquement en mode debug
  if ENABLE_DEBUG_DRAW then
  begin
    DrawBoundingBox(Entity);
    DrawVelocityVector(Entity);
    DrawAIState(Entity);
  end;
end;
```

## Optimisation réseau (jeux multijoueurs)

### 1. Interpolation client

Pour compenser la latence réseau.

```pascal
type
  TNetworkEntity = class
  private
    FServerPosition: TVector2;
    FDisplayPosition: TVector2;
    FServerTimestamp: QWord;
  public
    procedure UpdateFromServer(NewPos: TVector2; Timestamp: QWord);
    procedure Interpolate(DeltaTime: Single);
  end;

procedure TNetworkEntity.Interpolate(DeltaTime: Single);
const
  INTERPOLATION_SPEED = 10.0;
var
  Direction: TVector2;
  Distance: Single;
begin
  // Interpoler vers la position serveur
  Direction.X := FServerPosition.X - FDisplayPosition.X;
  Direction.Y := FServerPosition.Y - FDisplayPosition.Y;
  Distance := Sqrt(Sqr(Direction.X) + Sqr(Direction.Y));

  if Distance > 0.1 then
  begin
    Direction.X := Direction.X / Distance;
    Direction.Y := Direction.Y / Distance;

    FDisplayPosition.X := FDisplayPosition.X + Direction.X * INTERPOLATION_SPEED * DeltaTime;
    FDisplayPosition.Y := FDisplayPosition.Y + Direction.Y * INTERPOLATION_SPEED * DeltaTime;
  end
  else
    FDisplayPosition := FServerPosition;
end;
```

### 2. Client-side prediction

```pascal
type
  TPlayerController = class
  private
    FPredictedPosition: TVector2;
    FLastAcknowledgedInput: Integer;
    FPendingInputs: TList;
  public
    procedure SendInputToServer(Input: TPlayerInput);
    procedure OnServerUpdate(ServerPos: TVector2; LastProcessedInput: Integer);
    procedure PredictMovement(DeltaTime: Single);
  end;

procedure TPlayerController.OnServerUpdate(ServerPos: TVector2; LastProcessedInput: Integer);
var
  i: Integer;
  Input: TPlayerInput;
begin
  // Position confirmée par le serveur
  FPredictedPosition := ServerPos;

  // Rejouer les inputs non encore traités par le serveur
  for i := 0 to FPendingInputs.Count - 1 do
  begin
    Input := TPlayerInput(FPendingInputs[i]);
    if Input.ID > LastProcessedInput then
      ApplyInput(Input, FPredictedPosition);
  end;

  // Supprimer les inputs traités
  RemoveProcessedInputs(LastProcessedInput);
end;
```

### 3. Snapshot compression

```pascal
type
  TEntitySnapshot = packed record
    ID: Word;
    X: SmallInt;       // Position relative * 10 pour économiser
    Y: SmallInt;
    Angle: Byte;       // 0-255 au lieu de 0-360 degrés
    State: Byte;       // Flags d'état compressés
  end;

procedure CompressSnapshot(Entity: TEntity; out Snapshot: TEntitySnapshot);
begin
  Snapshot.ID := Entity.ID;
  Snapshot.X := Round(Entity.X * 10);  // Précision de 0.1
  Snapshot.Y := Round(Entity.Y * 10);
  Snapshot.Angle := Round(Entity.Angle * 255 / 360);

  // Compresser plusieurs booléens en un seul byte
  Snapshot.State := 0;
  if Entity.IsMoving then Snapshot.State := Snapshot.State or 1;
  if Entity.IsShooting then Snapshot.State := Snapshot.State or 2;
  if Entity.IsJumping then Snapshot.State := Snapshot.State or 4;
end;
```

### 4. Delta compression

N'envoyer que ce qui a changé.

```pascal
type
  TEntityDelta = record
    ID: Word;
    ChangedFields: Byte;  // Bits indiquant les champs modifiés
    X: SmallInt;
    Y: SmallInt;
    Angle: Byte;
  end;

const
  DELTA_X     = 1;
  DELTA_Y     = 2;
  DELTA_ANGLE = 4;

procedure CreateDelta(OldState, NewState: TEntity; out Delta: TEntityDelta);
begin
  Delta.ID := NewState.ID;
  Delta.ChangedFields := 0;

  if Abs(OldState.X - NewState.X) > 0.1 then
  begin
    Delta.ChangedFields := Delta.ChangedFields or DELTA_X;
    Delta.X := Round(NewState.X * 10);
  end;

  if Abs(OldState.Y - NewState.Y) > 0.1 then
  begin
    Delta.ChangedFields := Delta.ChangedFields or DELTA_Y;
    Delta.Y := Round(NewState.Y * 10);
  end;

  if Abs(OldState.Angle - NewState.Angle) > 1 then
  begin
    Delta.ChangedFields := Delta.ChangedFields or DELTA_ANGLE;
    Delta.Angle := Round(NewState.Angle * 255 / 360);
  end;
end;
```

## Optimisation du chargement

### 1. Écran de chargement avec progression

```pascal
type
  TLoadingScreen = class
  private
    FProgress: Single;
    FTotalSteps: Integer;
    FCurrentStep: Integer;
    FStatusText: string;
  public
    procedure StartLoading(TotalSteps: Integer);
    procedure NextStep(const Status: string);
    procedure Draw;
  end;

procedure LoadGameLevel(const Filename: string);
begin
  LoadingScreen.StartLoading(5);

  LoadingScreen.NextStep('Chargement des textures...');
  LoadTextures;

  LoadingScreen.NextStep('Chargement des sons...');
  LoadAudio;

  LoadingScreen.NextStep('Génération du terrain...');
  GenerateTerrain;

  LoadingScreen.NextStep('Création des entités...');
  SpawnEntities;

  LoadingScreen.NextStep('Finalisation...');
  InitializeGame;
end;
```

### 2. Chargement asynchrone

```pascal
type
  TAsyncLoader = class(TThread)
  private
    FResourcePath: string;
    FResource: TGameResource;
  protected
    procedure Execute; override;
  public
    constructor Create(const ResourcePath: string);
    property Resource: TGameResource read FResource;
  end;

procedure TAsyncLoader.Execute;
begin
  // Chargement dans un thread séparé
  FResource := LoadResourceFromFile(FResourcePath);
end;

// Utilisation
procedure StartAsyncLoad;
begin
  Loader := TAsyncLoader.Create('level_data.dat');
  Loader.Start;

  // Le jeu continue de tourner pendant le chargement
  while not Loader.Finished do
  begin
    UpdateMainMenu;
    RenderMainMenu;
  end;

  // Récupérer la ressource chargée
  LoadedLevel := Loader.Resource;
  Loader.Free;
end;
```

### 3. Streaming de niveau

Charger les parties du niveau à mesure que le joueur avance.

```pascal
type
  TLevelChunk = class
  private
    FX, FY: Integer;
    FLoaded: Boolean;
    FEntities: TList;
  public
    procedure Load;
    procedure Unload;
  end;

procedure TLevelManager.Update(PlayerX, PlayerY: Integer);
var
  ChunkX, ChunkY: Integer;
  i, j: Integer;
  Chunk: TLevelChunk;
begin
  ChunkX := PlayerX div CHUNK_SIZE;
  ChunkY := PlayerY div CHUNK_SIZE;

  // Charger les chunks proches
  for i := ChunkX - LOAD_RADIUS to ChunkX + LOAD_RADIUS do
    for j := ChunkY - LOAD_RADIUS to ChunkY + LOAD_RADIUS do
    begin
      Chunk := GetChunk(i, j);
      if not Chunk.Loaded then
        Chunk.Load;
    end;

  // Décharger les chunks lointains
  for i := 0 to FChunks.Count - 1 do
  begin
    Chunk := TLevelChunk(FChunks[i]);
    if (Abs(Chunk.X - ChunkX) > UNLOAD_RADIUS) or
       (Abs(Chunk.Y - ChunkY) > UNLOAD_RADIUS) then
      Chunk.Unload;
  end;
end;
```

## Optimisation de la compilation

### 1. Options de compilation pour performance

```pascal
{$mode objfpc}{$H+}
{$OPTIMIZATION LEVEL3}        // Optimisation maximale
{$SMARTLINK ON}               // Élimination du code mort
{$INLINE ON}                  // Activer l'inlining
{$RANGECHECKS OFF}            // Désactiver les vérifications (release uniquement)
{$OVERFLOWCHECKS OFF}         // Idem
{$ASSERTIONS OFF}             // Pas d'assertions en production
```

### 2. Profils de compilation

**Profile Debug** :
```pascal
{$IFDEF DEBUG}
  {$OPTIMIZATION OFF}
  {$DEBUGINFO ON}
  {$RANGECHECKS ON}
  {$ASSERTIONS ON}
{$ENDIF}
```

**Profile Release** :
```pascal
{$IFDEF RELEASE}
  {$OPTIMIZATION LEVEL3}
  {$DEBUGINFO OFF}
  {$RANGECHECKS OFF}
  {$ASSERTIONS OFF}
  {$INLINE ON}
{$ENDIF}
```

### 3. Link-time optimization

Sur Linux, utilisez LTO (Link Time Optimization) :

```bash
fpc -O3 -XX -CX MonJeu.pas
```

Options :
- `-O3` : Optimisation niveau 3
- `-XX` : Smart linking
- `-CX` : Smart linking avec strip

## Optimisation spécifique aux plateformes

### Windows

```pascal
{$IFDEF WINDOWS}
procedure OptimizeForWindows;
begin
  // Éviter l'endormissement
  SetThreadExecutionState(ES_CONTINUOUS or ES_SYSTEM_REQUIRED or ES_DISPLAY_REQUIRED);

  // Affinité CPU pour éviter les changements de core
  SetProcessAffinityMask(GetCurrentProcess, 1);

  // Priorité haute pour le thread de rendu
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_HIGHEST);

  // Désactiver les optimisations d'énergie
  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
end;
{$ENDIF}
```

### Linux

```pascal
{$IFDEF LINUX}
procedure OptimizeForLinux;
begin
  // Verrouiller les pages en mémoire (éviter le swap)
  mlockall(MCL_CURRENT or MCL_FUTURE);

  // Politique de scheduling temps-réel (nécessite sudo)
  // Attention : à utiliser avec précaution !
  SetScheduler(SCHED_FIFO, 99);

  // Désactiver le CPU frequency scaling
  system('cpupower frequency-set -g performance');
end;
{$ENDIF}
```

### Détection du matériel

```pascal
function DetectGPUVendor: string;
begin
  {$IFDEF WINDOWS}
  // Utiliser DirectX ou WMI
  Result := GetGPUVendorWindows;
  {$ENDIF}

  {$IFDEF LINUX}
  // Lire /sys ou utiliser lspci
  Result := GetGPUVendorLinux;
  {$ENDIF}
end;

procedure ApplyOptimalSettings;
var
  Vendor: string;
begin
  Vendor := DetectGPUVendor;

  if Vendor = 'NVIDIA' then
    EnableNvidiaOptimizations
  else if Vendor = 'AMD' then
    EnableAMDOptimizations
  else if Vendor = 'Intel' then
    EnableIntelOptimizations;
end;
```

## Options graphiques configurables

### 1. Menu de paramètres

```pascal
type
  TGraphicsSettings = record
    Resolution: TPoint;
    Fullscreen: Boolean;
    VSync: Boolean;
    AntiAliasing: Integer;      // 0, 2, 4, 8
    ShadowQuality: Integer;     // 0=off, 1=low, 2=medium, 3=high
    TextureQuality: Integer;    // 0=low, 1=medium, 2=high
    ParticleCount: Integer;     // Multiplicateur
    PostProcessing: Boolean;
    ViewDistance: Integer;      // En unités de jeu
  end;

procedure ApplyGraphicsSettings(Settings: TGraphicsSettings);
begin
  SetResolution(Settings.Resolution);
  SetFullscreen(Settings.Fullscreen);
  SetVSync(Settings.VSync);
  SetAntiAliasing(Settings.AntiAliasing);

  // Ajuster les limites basées sur les paramètres
  case Settings.ParticleCount of
    0: MaxParticles := 100;
    1: MaxParticles := 500;
    2: MaxParticles := 2000;
  end;
end;
```

### 2. Détection automatique

```pascal
function DetectOptimalSettings: TGraphicsSettings;
var
  GPUMemory: Int64;
  CPUCores: Integer;
begin
  GPUMemory := GetGPUMemory;
  CPUCores := GetCPUCoreCount;

  // Paramètres bas
  Result.Resolution := Point(1280, 720);
  Result.TextureQuality := 0;
  Result.ShadowQuality := 0;
  Result.ParticleCount := 0;

  // Améliorer si le matériel le permet
  if GPUMemory > 2048 then  // Plus de 2 GB
  begin
    Result.Resolution := Point(1920, 1080);
    Result.TextureQuality := 1;
  end;

  if GPUMemory > 4096 then  // Plus de 4 GB
  begin
    Result.TextureQuality := 2;
    Result.ShadowQuality := 2;
    Result.ParticleCount := 2;
  end;

  // Adapter selon le CPU
  if CPUCores >= 4 then
    Result.ParticleCount := Min(Result.ParticleCount + 1, 2);
end;
```

## Benchmark intégré

```pascal
type
  TBenchmark = class
  private
    FResults: record
      AverageFPS: Single;
      MinFPS: Integer;
      MaxFPS: Integer;
      FrameTimeP95: Single;  // 95e percentile
    end;
  public
    procedure Run;
    procedure SaveResults(const Filename: string);
    function GetRecommendedSettings: TGraphicsSettings;
  end;

procedure TBenchmark.Run;
var
  FrameTimes: array of Single;
  i: Integer;
  TotalTime: Single;
begin
  SetLength(FrameTimes, 1000);

  // Scène de test standardisée
  LoadBenchmarkScene;

  // Mesurer 1000 frames
  for i := 0 to 999 do
  begin
    StartTime := GetPreciseTime;

    UpdateScene;
    RenderScene;

    FrameTimes[i] := GetPreciseTime - StartTime;
  end;

  // Analyser les résultats
  TotalTime := 0;
  FResults.MinFPS := MaxInt;
  FResults.MaxFPS := 0;

  for i := 0 to 999 do
  begin
    TotalTime := TotalTime + FrameTimes[i];

    CurrentFPS := Round(1000 / FrameTimes[i]);
    if CurrentFPS < FResults.MinFPS then FResults.MinFPS := CurrentFPS;
    if CurrentFPS > FResults.MaxFPS then FResults.MaxFPS := CurrentFPS;
  end;

  FResults.AverageFPS := 1000 / (TotalTime / 1000);

  // Calculer le 95e percentile
  SortArray(FrameTimes);
  FResults.FrameTimeP95 := FrameTimes[949];  // 95% des frames
end;
```

## Cas pratiques d'optimisation

### Exemple 1 : Système de particules optimisé

```pascal
type
  TParticle = record
    Active: Boolean;
    X, Y: Single;
    VX, VY: Single;
    Life: Single;
    Color: Cardinal;
  end;

  TParticleSystem = class
  private
    FParticles: array of TParticle;
    FActiveCount: Integer;
  public
    procedure Update(DeltaTime: Single);
    procedure Render;
    procedure Emit(X, Y: Single; Count: Integer);
  end;

procedure TParticleSystem.Update(DeltaTime: Single);
var
  i: Integer;
  P: ^TParticle;
begin
  // Utiliser un pointeur pour éviter les copies
  for i := 0 to FActiveCount - 1 do
  begin
    P := @FParticles[i];

    if P^.Active then
    begin
      P^.X := P^.X + P^.VX * DeltaTime;
      P^.Y := P^.Y + P^.VY * DeltaTime;
      P^.Life := P^.Life - DeltaTime;

      if P^.Life <= 0 then
      begin
        // Swap avec la dernière particule active
        P^ := FParticles[FActiveCount - 1];
        Dec(FActiveCount);
        Dec(i);  // Retraiter cette position
      end;
    end;
  end;
end;

procedure TParticleSystem.Render;
var
  i: Integer;
  Batch: TVertexBatch;
begin
  Batch := TVertexBatch.Create;
  try
    // Ajouter toutes les particules au batch
    for i := 0 to FActiveCount - 1 do
    begin
      with FParticles[i] do
        Batch.AddQuad(X, Y, 2, 2, Color);
    end;

    // Un seul appel de dessin
    Batch.Draw;
  finally
    Batch.Free;
  end;
end;
```

### Exemple 2 : Cache de pathfinding

```pascal
type
  TPathCache = class
  private
    FCache: TDictionary<string, TPath>;
    FAccessTime: TDictionary<string, QWord>;
    FMaxEntries: Integer;
  public
    function GetPath(StartX, StartY, EndX, EndY: Integer): TPath;
    procedure PruneCache;
  end;

function TPathCache.GetPath(StartX, StartY, EndX, EndY: Integer): TPath;
var
  Key: string;
begin
  Key := Format('%d,%d-%d,%d', [StartX, StartY, EndX, EndY]);

  if FCache.ContainsKey(Key) then
  begin
    // Chemin en cache
    FAccessTime[Key] := GetTickCount64;
    Result := FCache[Key];
  end
  else
  begin
    // Calculer le chemin
    Result := CalculatePath(StartX, StartY, EndX, EndY);

    // Ajouter au cache
    if FCache.Count >= FMaxEntries then
      PruneCache;

    FCache.Add(Key, Result);
    FAccessTime.Add(Key, GetTickCount64);
  end;
end;

procedure TPathCache.PruneCache;
var
  OldestKey: string;
  OldestTime: QWord;
  Key: string;
begin
  // Supprimer l'entrée la moins récemment utilisée
  OldestTime := GetTickCount64;

  for Key in FAccessTime.Keys do
  begin
    if FAccessTime[Key] < OldestTime then
    begin
      OldestTime := FAccessTime[Key];
      OldestKey := Key;
    end;
  end;

  FCache.Remove(OldestKey);
  FAccessTime.Remove(OldestKey);
end;
```

### Exemple 3 : Frustum culling optimisé

```pascal
type
  TFrustum = record
    Left, Right, Top, Bottom: Single;
  end;

  TQuadTree = class
  private
    FBounds: TRect;
    FEntities: TList;
    FChildren: array[0..3] of TQuadTree;
    FDivided: Boolean;
  public
    procedure Insert(Entity: TEntity);
    procedure QueryFrustum(const Frustum: TFrustum; Results: TList);
    procedure Subdivide;
  end;

procedure TQuadTree.QueryFrustum(const Frustum: TFrustum; Results: TList);
var
  i: Integer;
  Entity: TEntity;
begin
  // Vérifier si ce nœud intersecte le frustum
  if not RectIntersectsFrustum(FBounds, Frustum) then
    Exit;

  // Ajouter les entités de ce nœud
  for i := 0 to FEntities.Count - 1 do
  begin
    Entity := TEntity(FEntities[i]);
    if EntityInFrustum(Entity, Frustum) then
      Results.Add(Entity);
  end;

  // Récursion sur les enfants
  if FDivided then
  begin
    for i := 0 to 3 do
      FChildren[i].QueryFrustum(Frustum, Results);
  end;
end;
```

## Checklist d'optimisation

### Avant de lancer le jeu

- [ ] Compiler en mode Release avec optimisations activées
- [ ] Vérifier qu'il n'y a pas de code de debug actif
- [ ] Tester sur la configuration minimale requise
- [ ] Mesurer l'utilisation mémoire
- [ ] Vérifier les fuites mémoire avec des outils appropriés
- [ ] Profiler les zones critiques

### Pendant le développement

- [ ] Utiliser object pooling pour les objets fréquents
- [ ] Implémenter le batch rendering
- [ ] Ajouter frustum et occlusion culling
- [ ] Utiliser spatial partitioning pour les collisions
- [ ] Optimiser les boucles critiques
- [ ] Mettre en cache les calculs coûteux
- [ ] Limiter le nombre d'allocations

### Tests de performance

- [ ] Mesurer les FPS dans différentes situations
- [ ] Tester avec le nombre maximum d'entités
- [ ] Vérifier les temps de chargement
- [ ] Tester sur Windows et Linux
- [ ] Mesurer l'utilisation CPU et GPU
- [ ] Identifier les goulots d'étranglement

## Ressources et outils

### Outils de profiling

**Windows** :
- Intel VTune Profiler
- AMD μProf
- Visual Studio Profiler

**Linux** :
- Valgrind (callgrind, cachegrind)
- perf
- gprof
- Heaptrack (analyse mémoire)

**Multi-plateforme** :
- Custom profiler intégré au jeu
- Console de debug avec stats

### Bibliothèques utiles

- **BGRABitmap** : Graphiques 2D rapides
- **Castle Game Engine** : Moteur optimisé
- **OpenGL** : Rendu matériel accéléré
- **Synapse** : Réseau optimisé

## Conclusion

L'optimisation est un processus continu qui nécessite :

1. **Mesure constante** : Toujours profiler avant d'optimiser
2. **Priorités claires** : Optimiser les goulots d'étranglement, pas tout
3. **Tests rigoureux** : Vérifier que les optimisations fonctionnent
4. **Documentation** : Noter ce qui a été optimisé et pourquoi
5. **Équilibre** : Trouver le bon compromis entre performance et qualité

### Points clés à retenir

✅ **CPU** : Réduire les allocations, optimiser les boucles, utiliser le caching  
✅ **GPU** : Batch rendering, culling, LOD, optimiser les shaders  
✅ **Mémoire** : Object pooling, streaming, compression  
✅ **Réseau** : Interpolation, prédiction, compression des données  
✅ **Multi-plateforme** : Tester sur Windows et Linux, adapter aux spécificités

L'optimisation prématurée est certes la racine de tous les maux, mais l'optimisation réfléchie et mesurée est la clé d'un jeu professionnel et agréable à jouer !

N'oubliez pas : un jeu qui tourne à 60 FPS stable sur du matériel modeste atteindra un public beaucoup plus large qu'un jeu magnifique mais qui rame. L'optimisation est un investissement qui vaut la peine.

⏭️ [Distribution Steam (Windows/Linux)](/23-developpement-jeux/09-distribution-steam-windows-linux.md)
