🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.2 ZenGL et frameworks 2D - Partie 2 (suite)

## Optimisations pour jeux 2D (suite)

### Pool d'objets (suite)

```pascal
type
  TBullet = record
    Active: Boolean;
    X, Y, VX, VY: Single;
  end;

var
  BulletPool: array[0..99] of TBullet;

function GetBullet: Integer;  
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(BulletPool) do
  begin
    if not BulletPool[I].Active then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure FireBullet(X, Y, VX, VY: Single);  
var
  Index: Integer;
begin
  Index := GetBullet;
  if Index >= 0 then
  begin
    BulletPool[Index].Active := True;
    BulletPool[Index].X := X;
    BulletPool[Index].Y := Y;
    BulletPool[Index].VX := VX;
    BulletPool[Index].VY := VY;
  end;
end;

procedure UpdateBullets(dt: Double);  
var
  I: Integer;
begin
  for I := 0 to High(BulletPool) do
  begin
    if BulletPool[I].Active then
    begin
      // Mettre à jour la position
      BulletPool[I].X := BulletPool[I].X + BulletPool[I].VX * dt;
      BulletPool[I].Y := BulletPool[I].Y + BulletPool[I].VY * dt;

      // Désactiver si hors écran
      if (BulletPool[I].X < 0) or (BulletPool[I].X > SCREEN_WIDTH) or
         (BulletPool[I].Y < 0) or (BulletPool[I].Y > SCREEN_HEIGHT) then
      begin
        BulletPool[I].Active := False;
      end;
    end;
  end;
end;

procedure DrawBullets;  
var
  I: Integer;
begin
  for I := 0 to High(BulletPool) do
  begin
    if BulletPool[I].Active then
      ssprite2d_Draw(texBullet, BulletPool[I].X, BulletPool[I].Y,
                     8, 8, 0);
  end;
end;
```

**Avantages du pool d'objets** :
- Pas d'allocation mémoire dynamique pendant le jeu
- Performance prévisible et constante
- Évite la fragmentation mémoire
- Idéal pour les objets créés/détruits fréquemment

### Spatial partitioning (quadtree)

Pour optimiser la détection de collisions sur de grandes cartes :

```pascal
type
  TQuadTreeNode = class
  private
    FBounds: TRect;
    FObjects: array of TGameObject;
    FChildren: array[0..3] of TQuadTreeNode;
    FDivided: Boolean;
    FCapacity: Integer;
  public
    constructor Create(X, Y, W, H: Single; Capacity: Integer = 4);
    destructor Destroy; override;
    procedure Subdivide;
    function Insert(Obj: TGameObject): Boolean;
    procedure Query(Range: TRect; var Found: TObjectList);
    procedure Clear;
  end;

constructor TQuadTreeNode.Create(X, Y, W, H: Single; Capacity: Integer);  
begin
  FBounds.X := X;
  FBounds.Y := Y;
  FBounds.W := W;
  FBounds.H := H;
  FCapacity := Capacity;
  FDivided := False;
  SetLength(FObjects, 0);
end;

procedure TQuadTreeNode.Subdivide;  
var
  HalfW, HalfH: Single;
begin
  HalfW := FBounds.W / 2;
  HalfH := FBounds.H / 2;

  // Créer 4 sous-régions
  FChildren[0] := TQuadTreeNode.Create(FBounds.X, FBounds.Y,
                                       HalfW, HalfH, FCapacity);
  FChildren[1] := TQuadTreeNode.Create(FBounds.X + HalfW, FBounds.Y,
                                       HalfW, HalfH, FCapacity);
  FChildren[2] := TQuadTreeNode.Create(FBounds.X, FBounds.Y + HalfH,
                                       HalfW, HalfH, FCapacity);
  FChildren[3] := TQuadTreeNode.Create(FBounds.X + HalfW, FBounds.Y + HalfH,
                                       HalfW, HalfH, FCapacity);

  FDivided := True;
end;

function TQuadTreeNode.Insert(Obj: TGameObject): Boolean;  
var
  I: Integer;
begin
  // Vérifier si l'objet est dans les limites
  if not CheckCollision(Obj.Bounds, FBounds) then
  begin
    Result := False;
    Exit;
  end;

  // Si capacité non atteinte, ajouter ici
  if (Length(FObjects) < FCapacity) and (not FDivided) then
  begin
    SetLength(FObjects, Length(FObjects) + 1);
    FObjects[High(FObjects)] := Obj;
    Result := True;
    Exit;
  end;

  // Sinon, subdiviser si nécessaire
  if not FDivided then
    Subdivide;

  // Insérer dans les enfants
  for I := 0 to 3 do
  begin
    if FChildren[I].Insert(Obj) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;
```

## Gestion d'états de jeu (Game States)

Un système d'états permet d'organiser les différentes parties du jeu (menu, gameplay, pause, game over).

```pascal
type
  TGameStateType = (gsMenu, gsPlaying, gsPaused, gsGameOver);

  TGameState = class
  public
    procedure Enter; virtual; abstract;
    procedure Exit; virtual; abstract;
    procedure Update(dt: Double); virtual; abstract;
    procedure Draw; virtual; abstract;
    procedure HandleInput; virtual; abstract;
  end;

  TGameStateManager = class
  private
    FStates: array[TGameStateType] of TGameState;
    FCurrentState: TGameState;
    FCurrentType: TGameStateType;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterState(StateType: TGameStateType; State: TGameState);
    procedure ChangeState(NewState: TGameStateType);
    procedure Update(dt: Double);
    procedure Draw;
    procedure HandleInput;
  end;

// État Menu
type
  TMenuState = class(TGameState)
  private
    FSelectedButton: Integer;
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure Update(dt: Double); override;
    procedure Draw; override;
    procedure HandleInput; override;
  end;

procedure TMenuState.Enter;  
begin
  FSelectedButton := 0;
  snd_Play(sndMenuMusic, True);
end;

procedure TMenuState.Exit;  
begin
  snd_Stop(sndMenuMusic);
end;

procedure TMenuState.Update(dt: Double);  
begin
  // Logique du menu
end;

procedure TMenuState.Draw;  
begin
  // Dessiner le menu
  text_Draw(fntMain, 300, 100, 'SUPER GAME');

  if FSelectedButton = 0 then
    text_Draw(fntMain, 300, 200, '> Start Game')
  else
    text_Draw(fntMain, 300, 200, '  Start Game');

  if FSelectedButton = 1 then
    text_Draw(fntMain, 300, 250, '> Options')
  else
    text_Draw(fntMain, 300, 250, '  Options');

  if FSelectedButton = 2 then
    text_Draw(fntMain, 300, 300, '> Quit')
  else
    text_Draw(fntMain, 300, 300, '  Quit');
end;

procedure TMenuState.HandleInput;  
begin
  if key_Down(K_UP) then
  begin
    Dec(FSelectedButton);
    if FSelectedButton < 0 then
      FSelectedButton := 2;
    snd_Play(sndMenuMove, False);
  end;

  if key_Down(K_DOWN) then
  begin
    Inc(FSelectedButton);
    if FSelectedButton > 2 then
      FSelectedButton := 0;
    snd_Play(sndMenuMove, False);
  end;

  if key_Down(K_ENTER) then
  begin
    case FSelectedButton of
      0: GameStateManager.ChangeState(gsPlaying);
      1: ShowOptions;
      2: app_Quit;
    end;
    snd_Play(sndMenuSelect, False);
  end;
end;

// État Jeu
type
  TPlayingState = class(TGameState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure Update(dt: Double); override;
    procedure Draw; override;
    procedure HandleInput; override;
  end;

procedure TPlayingState.Enter;  
begin
  // Initialiser le niveau
  LoadLevel(CurrentLevel);
  snd_Play(sndGameMusic, True);
end;

procedure TPlayingState.Exit;  
begin
  snd_Stop(sndGameMusic);
end;

procedure TPlayingState.Update(dt: Double);  
begin
  // Mettre à jour le jeu
  UpdatePlayer(dt);
  UpdateEnemies(dt);
  UpdateBullets(dt);
  CheckCollisions;

  // Vérifier fin de niveau
  if PlayerDead then
    GameStateManager.ChangeState(gsGameOver)
  else if LevelComplete then
    LoadNextLevel;
end;

procedure TPlayingState.Draw;  
begin
  DrawLevel;
  DrawPlayer;
  DrawEnemies;
  DrawBullets;
  DrawHUD;
end;

procedure TPlayingState.HandleInput;  
begin
  if key_Down(K_ESCAPE) then
    GameStateManager.ChangeState(gsPaused);
end;

// Gestionnaire d'états
constructor TGameStateManager.Create;  
begin
  FCurrentState := nil;
end;

procedure TGameStateManager.RegisterState(StateType: TGameStateType;
                                         State: TGameState);
begin
  FStates[StateType] := State;
end;

procedure TGameStateManager.ChangeState(NewState: TGameStateType);  
begin
  if FCurrentState <> nil then
    FCurrentState.Exit;

  FCurrentType := NewState;
  FCurrentState := FStates[NewState];

  if FCurrentState <> nil then
    FCurrentState.Enter;
end;

procedure TGameStateManager.Update(dt: Double);  
begin
  if FCurrentState <> nil then
    FCurrentState.Update(dt);
end;

procedure TGameStateManager.Draw;  
begin
  if FCurrentState <> nil then
    FCurrentState.Draw;
end;

procedure TGameStateManager.HandleInput;  
begin
  if FCurrentState <> nil then
    FCurrentState.HandleInput;
end;
```

## Sauvegarde et chargement

### Sauvegarder le score et les paramètres

```pascal
uses
  zgl_file,
  zgl_ini;

procedure SaveGameData;  
var
  Ini: zglTIniFile;
begin
  ini_LoadFromFile(Ini, 'game.ini');

  ini_WriteInt(Ini, 'Game', 'HighScore', HighScore);
  ini_WriteInt(Ini, 'Game', 'Level', CurrentLevel);
  ini_WriteBool(Ini, 'Settings', 'SoundEnabled', SoundEnabled);
  ini_WriteFloat(Ini, 'Settings', 'Volume', Volume);

  ini_SaveToFile(Ini, 'game.ini');
  ini_Free(Ini);
end;

procedure LoadGameData;  
var
  Ini: zglTIniFile;
begin
  if file_Exists('game.ini') then
  begin
    ini_LoadFromFile(Ini, 'game.ini');

    HighScore := ini_ReadInt(Ini, 'Game', 'HighScore', 0);
    CurrentLevel := ini_ReadInt(Ini, 'Game', 'Level', 1);
    SoundEnabled := ini_ReadBool(Ini, 'Settings', 'SoundEnabled', True);
    Volume := ini_ReadFloat(Ini, 'Settings', 'Volume', 1.0);

    ini_Free(Ini);
  end;
end;
```

### Sauvegarde binaire (pour états de jeu)

```pascal
type
  TSaveData = packed record
    Version: Integer;
    PlayerX, PlayerY: Single;
    PlayerHealth: Integer;
    CurrentLevel: Integer;
    Score: Integer;
    Inventory: array[0..9] of Integer;
  end;

procedure SaveGame(const FileName: string);  
var
  F: zglTFile;
  Data: TSaveData;
begin
  Data.Version := 1;
  Data.PlayerX := Player.X;
  Data.PlayerY := Player.Y;
  Data.PlayerHealth := Player.Health;
  Data.CurrentLevel := CurrentLevel;
  Data.Score := Score;
  // ... remplir le reste

  file_Open(F, FileName, FOM_CREATE);
  file_Write(F, Data, SizeOf(Data));
  file_Close(F);
end;

function LoadGame(const FileName: string): Boolean;  
var
  F: zglTFile;
  Data: TSaveData;
begin
  Result := False;

  if not file_Exists(FileName) then
    Exit;

  file_Open(F, FileName, FOM_OPENR);
  file_Read(F, Data, SizeOf(Data));
  file_Close(F);

  // Vérifier la version
  if Data.Version = 1 then
  begin
    Player.X := Data.PlayerX;
    Player.Y := Data.PlayerY;
    Player.Health := Data.PlayerHealth;
    CurrentLevel := Data.CurrentLevel;
    Score := Data.Score;
    Result := True;
  end;
end;
```

## Effets visuels avancés

### Shake de caméra

```pascal
type
  TCameraShake = record
    Active: Boolean;
    Intensity: Single;
    Duration: Single;
    Timer: Single;
  end;

var
  CameraShake: TCameraShake;

procedure StartCameraShake(Intensity, Duration: Single);  
begin
  CameraShake.Active := True;
  CameraShake.Intensity := Intensity;
  CameraShake.Duration := Duration;
  CameraShake.Timer := 0;
end;

procedure UpdateCameraShake(dt: Double);  
begin
  if CameraShake.Active then
  begin
    CameraShake.Timer := CameraShake.Timer + dt;

    if CameraShake.Timer >= CameraShake.Duration then
    begin
      CameraShake.Active := False;
      Camera.X := Camera.BaseX;
      Camera.Y := Camera.BaseY;
    end
    else
    begin
      // Ajouter un décalage aléatoire
      Camera.X := Camera.BaseX + (Random - 0.5) * CameraShake.Intensity;
      Camera.Y := Camera.BaseY + (Random - 0.5) * CameraShake.Intensity;
    end;
  end;
end;
```

### Effet de fondu (fade)

```pascal
type
  TFadeEffect = record
    Active: Boolean;
    FadeIn: Boolean;  // True = fade in, False = fade out
    Alpha: Byte;
    Speed: Single;
  end;

var
  Fade: TFadeEffect;

procedure StartFadeOut(Speed: Single);  
begin
  Fade.Active := True;
  Fade.FadeIn := False;
  Fade.Alpha := 0;
  Fade.Speed := Speed;
end;

procedure StartFadeIn(Speed: Single);  
begin
  Fade.Active := True;
  Fade.FadeIn := True;
  Fade.Alpha := 255;
  Fade.Speed := Speed;
end;

procedure UpdateFade(dt: Double);  
begin
  if Fade.Active then
  begin
    if Fade.FadeIn then
    begin
      Fade.Alpha := Fade.Alpha - Round(Fade.Speed * dt * 255);
      if Fade.Alpha <= 0 then
      begin
        Fade.Alpha := 0;
        Fade.Active := False;
      end;
    end
    else
    begin
      Fade.Alpha := Fade.Alpha + Round(Fade.Speed * dt * 255);
      if Fade.Alpha >= 255 then
      begin
        Fade.Alpha := 255;
        Fade.Active := False;
      end;
    end;
  end;
end;

procedure DrawFade;  
begin
  if Fade.Active or (Fade.Alpha > 0) then
  begin
    pr2d_Rect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT,
              $000000, Fade.Alpha, PR2D_FILL);
  end;
end;
```

### Effet de slow motion

```pascal
var
  TimeScale: Single = 1.0;
  TargetTimeScale: Single = 1.0;

procedure SetSlowMotion(Scale: Single; Duration: Single);  
begin
  TargetTimeScale := Scale;
  // Utiliser un timer pour revenir à la normale après Duration
end;

procedure Update(dt: Double);  
var
  AdjustedDt: Double;
begin
  // Interpoler vers la vitesse cible
  TimeScale := TimeScale + (TargetTimeScale - TimeScale) * 5.0 * dt;

  // Appliquer le time scale
  AdjustedDt := dt * TimeScale;

  // Mettre à jour avec le delta time ajusté
  UpdatePlayer(AdjustedDt);
  UpdateEnemies(AdjustedDt);
  UpdateBullets(AdjustedDt);
end;
```

## Pathfinding (recherche de chemin)

### Algorithme A* simple

```pascal
type
  TNode = record
    X, Y: Integer;
    G, H, F: Single;  // G = coût, H = heuristique, F = G + H
    Parent: ^TNode;
  end;

  TNodeList = array of TNode;

function Heuristic(X1, Y1, X2, Y2: Integer): Single;  
begin
  // Distance de Manhattan
  Result := Abs(X1 - X2) + Abs(Y1 - Y2);
end;

function IsWalkable(X, Y: Integer): Boolean;  
begin
  Result := (X >= 0) and (X < MAP_WIDTH) and
            (Y >= 0) and (Y < MAP_HEIGHT) and
            (GameMap[Y, X] = 0); // 0 = sol praticable
end;

function FindPath(StartX, StartY, EndX, EndY: Integer): TNodeList;  
var
  OpenList, ClosedList: TNodeList;
  Current: TNode;
  Neighbors: array[0..3] of record X, Y: Integer; end;
  I, LowestF: Integer;
begin
  SetLength(Result, 0);
  SetLength(OpenList, 0);
  SetLength(ClosedList, 0);

  // Ajouter le nœud de départ
  SetLength(OpenList, 1);
  OpenList[0].X := StartX;
  OpenList[0].Y := StartY;
  OpenList[0].G := 0;
  OpenList[0].H := Heuristic(StartX, StartY, EndX, EndY);
  OpenList[0].F := OpenList[0].H;
  OpenList[0].Parent := nil;

  // Définir les voisins (haut, bas, gauche, droite)
  Neighbors[0].X := 0; Neighbors[0].Y := -1;
  Neighbors[1].X := 0; Neighbors[1].Y := 1;
  Neighbors[2].X := -1; Neighbors[2].Y := 0;
  Neighbors[3].X := 1; Neighbors[3].Y := 0;

  while Length(OpenList) > 0 do
  begin
    // Trouver le nœud avec le F le plus bas
    LowestF := 0;
    for I := 1 to High(OpenList) do
      if OpenList[I].F < OpenList[LowestF].F then
        LowestF := I;

    Current := OpenList[LowestF];

    // Si on a atteint la destination
    if (Current.X = EndX) and (Current.Y = EndY) then
    begin
      // Reconstruire le chemin
      // ... (code de reconstruction)
      Break;
    end;

    // Déplacer vers la liste fermée
    SetLength(ClosedList, Length(ClosedList) + 1);
    ClosedList[High(ClosedList)] := Current;

    // Supprimer de la liste ouverte
    for I := LowestF to High(OpenList) - 1 do
      OpenList[I] := OpenList[I + 1];
    SetLength(OpenList, Length(OpenList) - 1);

    // Examiner les voisins
    for I := 0 to 3 do
    begin
      // ... (code d'examen des voisins)
    end;
  end;
end;
```

## Multijoueur local (split-screen)

```pascal
type
  TPlayer = record
    X, Y: Single;
    VX, VY: Single;
    Health: Integer;
    Score: Integer;
    Keys: record
      Up, Down, Left, Right, Fire: Integer; // Codes de touches
    end;
  end;

var
  Players: array[0..1] of TPlayer;

procedure InitPlayers;  
begin
  // Joueur 1 - Clavier
  Players[0].Keys.Up := K_W;
  Players[0].Keys.Down := K_S;
  Players[0].Keys.Left := K_A;
  Players[0].Keys.Right := K_D;
  Players[0].Keys.Fire := K_SPACE;
  Players[0].X := 100;
  Players[0].Y := 300;

  // Joueur 2 - Flèches
  Players[1].Keys.Up := K_UP;
  Players[1].Keys.Down := K_DOWN;
  Players[1].Keys.Left := K_LEFT;
  Players[1].Keys.Right := K_RIGHT;
  Players[1].Keys.Fire := K_CTRL_R;
  Players[1].X := 700;
  Players[1].Y := 300;
end;

procedure UpdatePlayer(var P: TPlayer; dt: Double);  
begin
  // Mouvement
  if key_Press(P.Keys.Left) then
    P.VX := -200;
  if key_Press(P.Keys.Right) then
    P.VX := 200;
  if key_Press(P.Keys.Up) then
    P.VY := -200;
  if key_Press(P.Keys.Down) then
    P.VY := 200;

  // Appliquer le mouvement
  P.X := P.X + P.VX * dt;
  P.Y := P.Y + P.VY * dt;

  // Friction
  P.VX := P.VX * 0.9;
  P.VY := P.VY * 0.9;

  // Tir
  if key_Down(P.Keys.Fire) then
    FireBullet(P.X, P.Y, P.VX * 2, P.VY * 2);
end;

procedure Update(dt: Double);  
begin
  UpdatePlayer(Players[0], dt);
  UpdatePlayer(Players[1], dt);
end;

procedure Draw;  
begin
  // Dessiner en split-screen vertical
  // Moitié gauche pour joueur 1
  scr_SetViewPort(0, 0, SCREEN_WIDTH div 2, SCREEN_HEIGHT);
  DrawGame(Players[0]);

  // Moitié droite pour joueur 2
  scr_SetViewPort(SCREEN_WIDTH div 2, 0, SCREEN_WIDTH div 2, SCREEN_HEIGHT);
  DrawGame(Players[1]);

  // Réinitialiser le viewport
  scr_SetViewPort(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT);

  // Ligne de séparation
  pr2d_Line(SCREEN_WIDTH div 2, 0,
            SCREEN_WIDTH div 2, SCREEN_HEIGHT,
            $FFFFFF, 255);
end;
```

## Support manette/gamepad

```pascal
uses
  zgl_joystick;

var
  Joystick: zglPJoystick;

procedure Init;  
begin
  // Initialiser le support des joysticks
  joy_Init;

  // Obtenir le premier joystick
  if joy_GetCount > 0 then
    Joystick := joy_Get(0);
end;

procedure Update(dt: Double);  
var
  AxisX, AxisY: Single;
begin
  if Joystick <> nil then
  begin
    // Lire les axes analogiques
    AxisX := joy_AxisPos(Joystick, JOY_AXIS_X);
    AxisY := joy_AxisPos(Joystick, JOY_AXIS_Y);

    // Zone morte (deadzone)
    if Abs(AxisX) < 0.2 then
      AxisX := 0;
    if Abs(AxisY) < 0.2 then
      AxisY := 0;

    // Déplacer le joueur
    Player.X := Player.X + AxisX * 200 * dt;
    Player.Y := Player.Y + AxisY * 200 * dt;

    // Boutons
    if joy_Down(Joystick, JOY_BUTTON_A) then
      PlayerJump;

    if joy_Down(Joystick, JOY_BUTTON_B) then
      PlayerFire;
  end;
end;
```

## Packaging et distribution

### Configuration pour Windows

```pascal
// Dans les options du projet
{$APPTYPE GUI}  // Pour une application sans console

{$R *.res}      // Inclure les ressources (icône, etc.)
```

Créer un fichier `.rc` pour les ressources :
```
1 ICON "icon.ico"
1 VERSIONINFO
FILEVERSION 1,0,0,0  
PRODUCTVERSION 1,0,0,0
{
  BLOCK "StringFileInfo"
  {
    BLOCK "040C04E4"
    {
      VALUE "CompanyName", "Votre Société"
      VALUE "FileDescription", "Mon Super Jeu"
      VALUE "FileVersion", "1.0.0.0"
      VALUE "ProductName", "Super Jeu"
      VALUE "ProductVersion", "1.0.0.0"
    }
  }
}
```

### Configuration pour Linux

```bash
#!/bin/bash
# Script de packaging Linux

# Compiler le jeu
fpc -O3 -XX -CX MonJeu.pas

# Créer la structure de dossiers
mkdir -p MonJeu/data  
mkdir -p MonJeu/lib

# Copier les fichiers
cp MonJeu MonJeu/  
cp -r data/* MonJeu/data/  
cp /usr/lib/libogg.so.0 MonJeu/lib/  
cp /usr/lib/libvorbis.so.0 MonJeu/lib/

# Créer un script de lancement
cat > MonJeu/run.sh << 'EOF'
#!/bin/bash
cd "$(dirname "$0")"  
export LD_LIBRARY_PATH=./lib:$LD_LIBRARY_PATH
./MonJeu
EOF

chmod +x MonJeu/run.sh

# Créer l'archive
tar -czf MonJeu-Linux.tar.gz MonJeu/
```

## Ressources et documentation

### Documentation ZenGL

- Site officiel : [zengl.org](http://zengl.org)
- Forum : [zengl.org/forum](http://zengl.org/forum)
- GitHub : [github.com/Seenkao/New-ZenGL](https://github.com/Seenkao/New-ZenGL)
- Wiki : Documentation et tutoriels communautaires

### Exemples inclus

ZenGL inclut de nombreux exemples dans le dossier `examples/` :
- `01 - Initialization` : Initialisation de base
- `02 - Textures` : Chargement et affichage de textures
- `03 - Sprites` : Animation de sprites
- `04 - Primitives` : Formes géométriques
- `05 - Text and Font` : Gestion du texte
- `06 - Sound` : Audio et musique
- `07 - Input` : Clavier, souris, joystick
- Et bien d'autres...

### Ressources pour assets

- **Graphiques gratuits** :
  - OpenGameArt.org
  - Itch.io (section assets)
  - Kenney.nl (assets de qualité gratuits)

- **Sons et musiques** :
  - Freesound.org
  - OpenGameArt.org
  - Incompetech.com (musiques libres)

- **Polices** :
  - Google Fonts
  - FontSquirrel.com
  - DaFont.com

### Outils recommandés

- **Éditeurs graphiques** :
  - GIMP (gratuit, Windows/Linux)
  - Aseprite (pixel art, payant)
  - Krita (gratuit, Windows/Linux)

- **Éditeurs de niveaux** :
  - Tiled Map Editor (tilemaps)
  - LDtk (level designer moderne)

- **Audio** :
  - Audacity (édition audio gratuite)
  - LMMS (création musicale gratuite)
  - sfxr/jfxr (générateurs d'effets sonores)

## Conclusion

ZenGL et les autres frameworks 2D pour FreePascal offrent une excellente base pour créer des jeux 2D performants et multiplateformes. Que vous choisissiez ZenGL pour sa simplicité, SDL2 pour sa maturité, ou Allegro pour son élégance, vous disposez d'outils puissants et bien documentés.

### Points clés à retenir

1. **Choisir le bon framework** selon vos besoins :
   - ZenGL : Simplicité et intégration Pascal native
   - SDL2 : Maturité et large communauté
   - Allegro : API moderne et bien pensée
   - Castle Engine : Pour le 2D et 3D combinés

2. **Optimisation** est essentielle :
   - Batch rendering pour réduire les appels de dessin
   - Culling pour éliminer les objets hors écran
   - Pool d'objets pour éviter les allocations fréquentes
   - Spatial partitioning pour optimiser les collisions

3. **Organisation du code** :
   - Système d'états pour gérer les écrans
   - Séparation logique/rendu/entrées
   - Gestion des ressources centralisée
   - Code modulaire et réutilisable

4. **Multiplateforme** dès le début :
   - Tester régulièrement sur Windows et Linux
   - Utiliser des chemins relatifs
   - Gérer les différences de configuration
   - Documenter les dépendances

### Prochaines étapes

Maintenant que vous maîtrisez les bases du développement 2D avec FreePascal, vous pouvez :

1. **Créer votre premier jeu complet** :
   - Commencer par un prototype simple
   - Ajouter progressivement des fonctionnalités
   - Tester et itérer régulièrement
   - Publier et recueillir des retours

2. **Explorer des concepts avancés** :
   - Shaders pour effets visuels personnalisés
   - Pathfinding et IA pour ennemis intelligents
   - Génération procédurale de niveaux
   - Système de dialogue et quêtes

3. **Apprendre des autres** :
   - Étudier le code source de jeux open source
   - Participer aux game jams
   - Rejoindre la communauté FreePascal
   - Contribuer aux projets existants

4. **Approfondir les domaines connexes** :
   - Animation avancée (bones, IK)
   - Physique 2D avec Box2D
   - Networking pour multijoueur en ligne
   - Intégration avec Steam/Itch.io

### Exemple de structure de projet complète

Voici une architecture recommandée pour un jeu 2D professionnel :

```
MonJeu/
├── src/
│   ├── Main.pas                 # Point d'entrée
│   ├── GameTypes.pas            # Types et constantes globales
│   ├── GameStates/
│   │   ├── StateManager.pas     # Gestionnaire d'états
│   │   ├── MenuState.pas        # État menu
│   │   ├── PlayState.pas        # État jeu
│   │   ├── PauseState.pas       # État pause
│   │   └── GameOverState.pas    # État game over
│   ├── Entities/
│   │   ├── Entity.pas           # Classe de base
│   │   ├── Player.pas           # Joueur
│   │   ├── Enemy.pas            # Ennemis
│   │   └── Bullet.pas           # Projectiles
│   ├── Systems/
│   │   ├── InputManager.pas     # Gestion des entrées
│   │   ├── AudioManager.pas     # Gestion audio
│   │   ├── ResourceManager.pas  # Chargement ressources
│   │   └── CollisionSystem.pas  # Détection collisions
│   ├── Utils/
│   │   ├── Math2D.pas           # Fonctions mathématiques
│   │   ├── Animation.pas        # Système d'animation
│   │   └── Particles.pas        # Système de particules
│   └── Level/
│       ├── Level.pas            # Gestion des niveaux
│       ├── TileMap.pas          # Tilemap
│       └── LevelLoader.pas      # Chargement depuis fichier
├── data/
│   ├── textures/               # Images et sprites
│   ├── sounds/                 # Effets sonores
│   ├── music/                  # Musiques
│   ├── fonts/                  # Polices de caractères
│   ├── levels/                 # Fichiers de niveaux
│   └── config.ini              # Configuration
├── lib/                        # Bibliothèques externes
│   ├── win32/                  # DLL Windows
│   └── linux64/                # .so Linux
├── build/                      # Fichiers compilés
├── docs/                       # Documentation
├── tools/                      # Outils de développement
└── README.md                   # Instructions
```

### Code template d'un jeu complet

```pascal
program SuperGame;

{$mode objfpc}{$H+}
{$I zglCustomConfig.cfg}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  zgl_application,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_render_2d,
  zgl_fx,
  zgl_primitives_2d,
  zgl_textures,
  zgl_textures_png,
  zgl_sprite_2d,
  zgl_text,
  zgl_font,
  zgl_sound,
  zgl_sound_ogg,
  zgl_keyboard,
  zgl_mouse,
  zgl_utils,
  zgl_types,
  GameTypes,
  StateManager,
  MenuState,
  PlayState,
  PauseState,
  GameOverState,
  ResourceManager,
  AudioManager,
  InputManager;

const
  SCREEN_WIDTH = 1280;
  SCREEN_HEIGHT = 720;
  GAME_TITLE = 'Mon Super Jeu';
  GAME_VERSION = '1.0.0';

var
  GameStateManager: TGameStateManager;
  FrameTime: Double;
  FPS: Integer;

procedure Init;  
begin
  // Initialiser les managers
  ResourceManager := TResourceManager.Create;
  AudioManager := TAudioManager.Create;
  InputManager := TInputManager.Create;

  // Charger les ressources
  ResourceManager.LoadAll;

  // Initialiser les états
  GameStateManager := TGameStateManager.Create;
  GameStateManager.RegisterState(gsMenu, TMenuState.Create);
  GameStateManager.RegisterState(gsPlaying, TPlayState.Create);
  GameStateManager.RegisterState(gsPaused, TPauseState.Create);
  GameStateManager.RegisterState(gsGameOver, TGameOverState.Create);

  // Démarrer avec le menu
  GameStateManager.ChangeState(gsMenu);

  // Démarrer la musique du menu
  AudioManager.PlayMusic('menu_music');
end;

procedure Draw;  
begin
  // Dessiner l'état actuel
  GameStateManager.Draw;

  // Afficher les FPS (mode debug)
  {$IFDEF DEBUG}
  text_Draw(fntMain, 10, 10, 'FPS: ' + u_IntToStr(FPS), $FFFFFF);
  {$ENDIF}
end;

procedure Update(dt: Double);  
begin
  FrameTime := dt;

  // Mettre à jour les managers
  InputManager.Update;
  AudioManager.Update(dt);

  // Mettre à jour l'état actuel
  GameStateManager.Update(dt);

  // Gérer la sortie
  if InputManager.KeyDown(K_ESCAPE) and
     (GameStateManager.CurrentType = gsMenu) then
    app_Quit;
end;

procedure Timer;  
begin
  FPS := zgl_Get(RENDER_FPS);
end;

procedure Quit;  
begin
  // Libérer les ressources
  GameStateManager.Free;
  ResourceManager.Free;
  AudioManager.Free;
  InputManager.Free;
end;

begin
  // Configuration de la fenêtre
  wnd_SetCaption(GAME_TITLE + ' v' + GAME_VERSION);
  wnd_ShowCursor(TRUE);

  // Configuration de l'écran
  scr_SetOptions(SCREEN_WIDTH, SCREEN_HEIGHT, REFRESH_MAXIMUM,
                 FALSE, FALSE);

  // Définir les callbacks
  app_InitProc := Init;
  app_DrawProc := Draw;
  app_UpdateProc := Update;
  app_ExitProc := Quit;

  // Timer pour calculer les FPS
  timer_Add(@Timer, 1000);

  // Lancer l'application
  app_Init;
  app_MainLoop;
  app_Free;
end.
```

### Conseils pour la publication

**Sur Windows** :
1. Compiler en mode Release avec optimisations (`-O3 -XX`)
2. Inclure toutes les DLL nécessaires
3. Créer un installeur avec Inno Setup ou NSIS
4. Signer l'exécutable si possible
5. Tester sur différentes versions de Windows

**Sur Linux** :
1. Compiler en mode statique si possible
2. Créer un script de lancement avec LD_LIBRARY_PATH
3. Fournir un .deb ou AppImage
4. Documenter les dépendances
5. Tester sur plusieurs distributions (Ubuntu, Fedora, Arch)

**Plateformes de distribution** :
- **Steam** : Large audience, support Windows/Linux
- **Itch.io** : Gratuit, flexible, indie-friendly
- **GOG** : Jeux DRM-free
- **Game Jolt** : Communauté indie active
- **GitHub Releases** : Pour les jeux open source

### Ressources d'apprentissage complémentaires

**Livres** :
- "Game Programming Patterns" de Robert Nystrom
- "The Art of Game Design" de Jesse Schell
- "Game Engine Architecture" de Jason Gregory

**Tutoriels en ligne** :
- LazyFoo (SDL2 tutorials, adaptables à ZenGL)
- GameFromScratch (concepts généraux)
- GDQuest (game design et développement)

**Communautés** :
- Forum FreePascal (freepascal.org/forum)
- Reddit r/freepascal
- Discord FreePascal
- Lazarus Forum

**Game Jams** :
- Ludum Dare
- Global Game Jam
- GMTK Game Jam
- Itch.io jams

### Performance : Benchmarks et objectifs

Pour un jeu 2D fluide, visez :
- **60 FPS minimum** sur le matériel cible
- **< 16.67 ms** par frame pour 60 FPS
- **< 33.33 ms** par frame pour 30 FPS (minimum acceptable)
- Chargement initial **< 5 secondes**
- Temps de réponse input **< 100 ms**

**Profiling** :
```pascal
procedure ProfileSection;  
var
  StartTime, EndTime: Double;
begin
  StartTime := timer_GetTicks;

  // Code à profiler
  UpdateEnemies(dt);

  EndTime := timer_GetTicks;
  WriteLn('UpdateEnemies: ', (EndTime - StartTime):0:3, ' ms');
end;
```

### Conclusion finale

Le développement de jeux 2D avec FreePascal et les frameworks comme ZenGL, SDL2 ou Allegro est une aventure passionnante et accessible. Vous disposez maintenant de tous les outils et connaissances nécessaires pour créer vos propres jeux, de simples prototypes à des projets ambitieux.

**Rappelez-vous** :
- Commencez petit et itérez
- Testez tôt et souvent
- La performance vient avec l'optimisation, pas la sur-optimisation prématurée
- Le plus important est de terminer vos projets
- Partagez votre travail et apprenez de la communauté

**N'oubliez pas** : Les meilleurs jeux ne sont pas toujours les plus techniquement impressionnants, mais ceux qui offrent une expérience mémorable aux joueurs. Concentrez-vous sur le gameplay, l'ergonomie et le plaisir de jeu avant tout.

Bon développement et amusez-vous bien ! 🎮

---

**Annexe : Checklist de lancement d'un jeu**

- [ ] Tous les bugs critiques corrigés
- [ ] Performance acceptable sur matériel cible
- [ ] Tutoriel ou instructions claires
- [ ] Sauvegarde/chargement fonctionnels
- [ ] Audio équilibré (volumes corrects)
- [ ] Testé sur Windows et Linux
- [ ] Package d'installation créé
- [ ] README avec instructions
- [ ] Licence définie
- [ ] Screenshots et trailer préparés
- [ ] Page de vente/présentation rédigée
- [ ] Testeurs beta consultés
- [ ] Backup du code source
- [ ] Plan de support post-lancement

⏭️ [Physics engines (Box2D, Bullet)](/23-developpement-jeux/03-physics-engines-box2d-bullet.md)
