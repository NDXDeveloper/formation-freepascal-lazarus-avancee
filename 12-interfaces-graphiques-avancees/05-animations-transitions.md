🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.5 Animations et transitions

## Introduction

Les animations et transitions donnent vie à vos interfaces graphiques. Une application avec des animations fluides et bien pensées se sent plus professionnelle, plus intuitive et plus agréable à utiliser. Cette section vous apprendra à créer des animations de qualité avec FreePascal et Lazarus, que ce soit pour des interfaces 2D ou des scènes 3D.

**Objectif** : Transformer des interfaces statiques en expériences dynamiques et engageantes.

---

## Qu'est-ce qu'une animation ?

### Définition

Une **animation** est une séquence d'images affichées rapidement pour créer l'illusion du mouvement. En programmation, cela signifie :
- Modifier progressivement des propriétés (position, taille, couleur, rotation, etc.)
- Rafraîchir l'affichage régulièrement (typiquement 30-60 fois par seconde)
- Créer des transitions fluides entre états

### Types d'animations

#### 1. Animations de propriétés
Modifier des valeurs numériques au fil du temps :
- Position : `X := X + VelocityX * DeltaTime`
- Rotation : `Angle := Angle + RotationSpeed * DeltaTime`
- Échelle : `Scale := Scale + ScaleSpeed * DeltaTime`
- Opacité : `Alpha := Alpha + FadeSpeed * DeltaTime`

#### 2. Animations par images (Sprites)
Afficher une séquence d'images pré-dessinées :
- Sprite sheets
- GIF animés
- Séquences d'images

#### 3. Animations procédurales
Calculer l'animation mathématiquement :
- Courbes de Bézier
- Fonctions sinusoïdales
- Algorithmes physiques

#### 4. Animations squelettiques (3D)
Pour personnages et modèles 3D :
- Bones (os) et joints
- Skinning et blend shapes
- Morphing

---

## Principes fondamentaux

### Frame Rate (Taux de rafraîchissement)

Le **frame rate** ou **FPS** (Frames Per Second) détermine la fluidité :

| FPS | Qualité | Usage |
|-----|---------|-------|
| **24** | Cinématographique | Films |
| **30** | Acceptable | Jeux consoles anciennes, applications légères |
| **60** | Fluide | Standard moderne |
| **120+** | Très fluide | Gaming compétitif, VR |

**En programmation** :
```pascal
// 60 FPS = 1 image toutes les 16.67 ms
Timer1.Interval := 16;  // ~60 FPS

// 30 FPS = 1 image toutes les 33.33 ms
Timer1.Interval := 33;  // ~30 FPS
```

### Delta Time

Le **delta time** est le temps écoulé depuis la dernière frame. C'est **essentiel** pour des animations fluides indépendantes du frame rate.

```pascal
var
  FLastTime: TDateTime;
  FDeltaTime: Double;

procedure UpdateDeltaTime;
var
  CurrentTime: TDateTime;
begin
  CurrentTime := Now;
  FDeltaTime := MilliSecondsBetween(CurrentTime, FLastTime) / 1000.0;
  FLastTime := CurrentTime;
end;

procedure UpdateAnimation;
begin
  UpdateDeltaTime;

  // Mouvement indépendant du FPS
  X := X + Speed * FDeltaTime;  // Speed en unités par seconde
end;
```

**Pourquoi c'est important** :
- Sans delta time : Vitesse dépend du FPS (incohérent)
- Avec delta time : Vitesse constante quel que soit le FPS

---

## Fonctions d'interpolation (Easing)

Les **fonctions d'interpolation** (ou **easing functions**) contrôlent la vitesse de l'animation au fil du temps. Elles rendent les animations plus naturelles.

### Interpolation linéaire (basique)

```pascal
function LinearInterpolation(AStart, AEnd, ATime: Double): Double;
begin
  Result := AStart + (AEnd - AStart) * ATime;
  // ATime = 0.0 → Result = AStart
  // ATime = 1.0 → Result = AEnd
end;
```

**Problème** : Les mouvements linéaires semblent robotiques et peu naturels.

### Types d'easing

#### 1. Ease In (Accélération)
Commence lentement, accélère :
```pascal
function EaseInQuad(t: Double): Double;
begin
  Result := t * t;
end;

function EaseInCubic(t: Double): Double;
begin
  Result := t * t * t;
end;
```

#### 2. Ease Out (Décélération)
Commence vite, ralentit :
```pascal
function EaseOutQuad(t: Double): Double;
begin
  Result := t * (2 - t);
end;

function EaseOutCubic(t: Double): Double;
var
  f: Double;
begin
  f := t - 1;
  Result := f * f * f + 1;
end;
```

#### 3. Ease In-Out (Accélération puis décélération)
Commence lentement, accélère au milieu, ralentit à la fin :
```pascal
function EaseInOutQuad(t: Double): Double;
begin
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := -1 + (4 - 2 * t) * t;
end;

function EaseInOutCubic(t: Double): Double;
begin
  if t < 0.5 then
    Result := 4 * t * t * t
  else
  begin
    t := 2 * t - 2;
    Result := 0.5 * t * t * t + 1;
  end;
end;
```

#### 4. Elastic (Rebond élastique)
```pascal
function EaseOutElastic(t: Double): Double;
const
  c4 = (2 * Pi) / 3;
begin
  if t = 0 then
    Result := 0
  else if t = 1 then
    Result := 1
  else
    Result := Power(2, -10 * t) * Sin((t * 10 - 0.75) * c4) + 1;
end;
```

#### 5. Bounce (Rebond)
```pascal
function EaseOutBounce(t: Double): Double;
const
  n1 = 7.5625;
  d1 = 2.75;
begin
  if t < 1 / d1 then
    Result := n1 * t * t
  else if t < 2 / d1 then
  begin
    t := t - 1.5 / d1;
    Result := n1 * t * t + 0.75;
  end
  else if t < 2.5 / d1 then
  begin
    t := t - 2.25 / d1;
    Result := n1 * t * t + 0.9375;
  end
  else
  begin
    t := t - 2.625 / d1;
    Result := n1 * t * t + 0.984375;
  end;
end;
```

### Visualisation des easing functions

```
Linear:      ────────────────────────────────
                                           ↗
EaseIn:      ___________________________/

EaseOut:     ────────────────╲_______________
                            ╲
EaseInOut:   _____________/─╲_______________
                        /     ╲
Elastic:     ─────╱╲╱╲╱────────────────────
                /  \/
Bounce:      ────────╱╲╱╲╱╲─────────────────
                    /  \/\
```

---

## Animation de propriétés simples

### Classe d'animation générique

```pascal
type
  TAnimationState = (asNotStarted, asRunning, asCompleted);

  TAnimation = class
  private
    FStartValue: Double;
    FEndValue: Double;
    FDuration: Double;
    FElapsedTime: Double;
    FCurrentValue: Double;
    FState: TAnimationState;
    FEasingFunction: function(t: Double): Double;
  public
    constructor Create(AStart, AEnd, ADuration: Double);
    procedure Update(ADeltaTime: Double);
    procedure Reset;
    procedure Start;

    property CurrentValue: Double read FCurrentValue;
    property State: TAnimationState read FState;
    property EasingFunction: function(t: Double): Double write FEasingFunction;
  end;

constructor TAnimation.Create(AStart, AEnd, ADuration: Double);
begin
  FStartValue := AStart;
  FEndValue := AEnd;
  FDuration := ADuration;
  FElapsedTime := 0;
  FCurrentValue := AStart;
  FState := asNotStarted;
  FEasingFunction := nil;  // Linéaire par défaut
end;

procedure TAnimation.Start;
begin
  FState := asRunning;
  FElapsedTime := 0;
  FCurrentValue := FStartValue;
end;

procedure TAnimation.Update(ADeltaTime: Double);
var
  t: Double;
begin
  if FState <> asRunning then
    Exit;

  FElapsedTime := FElapsedTime + ADeltaTime;

  if FElapsedTime >= FDuration then
  begin
    FCurrentValue := FEndValue;
    FState := asCompleted;
  end
  else
  begin
    t := FElapsedTime / FDuration;  // Normaliser 0.0 à 1.0

    // Appliquer l'easing
    if Assigned(FEasingFunction) then
      t := FEasingFunction(t);

    // Interpoler
    FCurrentValue := FStartValue + (FEndValue - FStartValue) * t;
  end;
end;

procedure TAnimation.Reset;
begin
  FElapsedTime := 0;
  FCurrentValue := FStartValue;
  FState := asNotStarted;
end;
```

### Utilisation

```pascal
type
  TForm1 = class(TForm)
  private
    FXAnimation: TAnimation;
    FOpacityAnimation: TAnimation;
    FLastTime: TDateTime;
  public
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLastTime := Now;

  // Animation de position X (0 à 400 en 2 secondes)
  FXAnimation := TAnimation.Create(0, 400, 2.0);
  FXAnimation.EasingFunction := @EaseInOutQuad;
  FXAnimation.Start;

  // Animation d'opacité (255 à 0 en 1.5 secondes)
  FOpacityAnimation := TAnimation.Create(255, 0, 1.5);
  FOpacityAnimation.EasingFunction := @EaseOutCubic;
  FOpacityAnimation.Start;

  Timer1.Interval := 16;  // ~60 FPS
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  CurrentTime: TDateTime;
  DeltaTime: Double;
begin
  CurrentTime := Now;
  DeltaTime := MilliSecondsBetween(CurrentTime, FLastTime) / 1000.0;
  FLastTime := CurrentTime;

  // Mettre à jour les animations
  FXAnimation.Update(DeltaTime);
  FOpacityAnimation.Update(DeltaTime);

  // Redessiner
  PaintBox1.Invalidate;

  // Relancer quand terminé (boucle)
  if FXAnimation.State = asCompleted then
  begin
    FXAnimation.Reset;
    FXAnimation.Start;
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  X: Integer;
  Alpha: Byte;
begin
  X := Round(FXAnimation.CurrentValue);
  Alpha := Round(FOpacityAnimation.CurrentValue);

  // Dessiner un rectangle animé
  PaintBox1.Canvas.Brush.Color := RGB(255, 0, 0);
  PaintBox1.Canvas.Brush.Style := bsSolid;

  // Note: L'alpha n'est pas directement supporté par TCanvas
  // Utilisez BGRABitmap pour la transparence

  PaintBox1.Canvas.Rectangle(X, 50, X + 100, 150);
end;
```

---

## Animations avec BGRABitmap

BGRABitmap offre un support complet de la transparence alpha, idéal pour les animations.

### Animation de fondu (Fade)

```pascal
uses
  BGRABitmap, BGRABitmapTypes;

type
  TFadeAnimation = class
  private
    FBitmap: TBGRABitmap;
    FAlpha: Double;
    FSpeed: Double;
    FFading: Boolean;
  public
    constructor Create(ABitmap: TBGRABitmap);
    procedure FadeIn(ASpeed: Double);
    procedure FadeOut(ASpeed: Double);
    procedure Update(ADeltaTime: Double);
    procedure Draw(ACanvas: TCanvas; X, Y: Integer);
  end;

constructor TFadeAnimation.Create(ABitmap: TBGRABitmap);
begin
  FBitmap := ABitmap;
  FAlpha := 255;
  FFading := False;
end;

procedure TFadeAnimation.FadeIn(ASpeed: Double);
begin
  FSpeed := ASpeed;
  FFading := True;
end;

procedure TFadeAnimation.FadeOut(ASpeed: Double);
begin
  FSpeed := -ASpeed;
  FFading := True;
end;

procedure TFadeAnimation.Update(ADeltaTime: Double);
begin
  if not FFading then
    Exit;

  FAlpha := FAlpha + FSpeed * ADeltaTime;

  // Limiter 0-255
  if FAlpha < 0 then
  begin
    FAlpha := 0;
    FFading := False;
  end
  else if FAlpha > 255 then
  begin
    FAlpha := 255;
    FFading := False;
  end;
end;

procedure TFadeAnimation.Draw(ACanvas: TCanvas; X, Y: Integer);
var
  Temp: TBGRABitmap;
begin
  Temp := FBitmap.Duplicate;
  try
    Temp.ApplyGlobalOpacity(Round(FAlpha));
    Temp.Draw(ACanvas, X, Y, False);
  finally
    Temp.Free;
  end;
end;
```

### Animation de rotation

```pascal
type
  TRotationAnimation = class
  private
    FOriginalBitmap: TBGRABitmap;
    FAngle: Double;
    FRotationSpeed: Double;
  public
    constructor Create(ABitmap: TBGRABitmap);
    procedure SetSpeed(ASpeed: Double);
    procedure Update(ADeltaTime: Double);
    procedure Draw(ACanvas: TCanvas; X, Y: Integer);
  end;

constructor TRotationAnimation.Create(ABitmap: TBGRABitmap);
begin
  FOriginalBitmap := ABitmap;
  FAngle := 0;
  FRotationSpeed := 0;
end;

procedure TRotationAnimation.SetSpeed(ASpeed: Double);
begin
  FRotationSpeed := ASpeed;  // Degrés par seconde
end;

procedure TRotationAnimation.Update(ADeltaTime: Double);
begin
  FAngle := FAngle + FRotationSpeed * ADeltaTime;

  // Normaliser l'angle 0-360
  while FAngle >= 360 do
    FAngle := FAngle - 360;
  while FAngle < 0 do
    FAngle := FAngle + 360;
end;

procedure TRotationAnimation.Draw(ACanvas: TCanvas; X, Y: Integer);
var
  Rotated: TBGRABitmap;
begin
  Rotated := FOriginalBitmap.RotateAffine(FAngle, 0, 0);
  try
    Rotated.Draw(ACanvas,
      X - (Rotated.Width - FOriginalBitmap.Width) div 2,
      Y - (Rotated.Height - FOriginalBitmap.Height) div 2,
      False);
  finally
    Rotated.Free;
  end;
end;
```

### Animation d'échelle (Zoom)

```pascal
type
  TScaleAnimation = class
  private
    FOriginalBitmap: TBGRABitmap;
    FScale: Double;
    FTargetScale: Double;
    FScaleSpeed: Double;
  public
    constructor Create(ABitmap: TBGRABitmap);
    procedure ScaleTo(ATargetScale, ADuration: Double);
    procedure Update(ADeltaTime: Double);
    procedure Draw(ACanvas: TCanvas; X, Y: Integer);
  end;

constructor TScaleAnimation.Create(ABitmap: TBGRABitmap);
begin
  FOriginalBitmap := ABitmap;
  FScale := 1.0;
  FTargetScale := 1.0;
end;

procedure TScaleAnimation.ScaleTo(ATargetScale, ADuration: Double);
begin
  FTargetScale := ATargetScale;
  if ADuration > 0 then
    FScaleSpeed := (ATargetScale - FScale) / ADuration
  else
    FScale := ATargetScale;
end;

procedure TScaleAnimation.Update(ADeltaTime: Double);
begin
  if Abs(FScale - FTargetScale) < 0.01 then
  begin
    FScale := FTargetScale;
    Exit;
  end;

  FScale := FScale + FScaleSpeed * ADeltaTime;

  // Vérifier si on a dépassé la cible
  if (FScaleSpeed > 0) and (FScale > FTargetScale) then
    FScale := FTargetScale
  else if (FScaleSpeed < 0) and (FScale < FTargetScale) then
    FScale := FTargetScale;
end;

procedure TScaleAnimation.Draw(ACanvas: TCanvas; X, Y: Integer);
var
  Scaled: TBGRABitmap;
  NewWidth, NewHeight: Integer;
begin
  NewWidth := Round(FOriginalBitmap.Width * FScale);
  NewHeight := Round(FOriginalBitmap.Height * FScale);

  if (NewWidth <= 0) or (NewHeight <= 0) then
    Exit;

  Scaled := FOriginalBitmap.Resample(NewWidth, NewHeight, rmFineResample);
  try
    Scaled.Draw(ACanvas,
      X - (NewWidth - FOriginalBitmap.Width) div 2,
      Y - (NewHeight - FOriginalBitmap.Height) div 2,
      False);
  finally
    Scaled.Free;
  end;
end;
```

---

## Animations de sprites (Sprite sheets)

### Gestion d'un sprite sheet

```pascal
type
  TSpriteAnimation = class
  private
    FSpriteSheet: TBGRABitmap;
    FFrameWidth: Integer;
    FFrameHeight: Integer;
    FFrameCount: Integer;
    FCurrentFrame: Integer;
    FFrameTime: Double;
    FElapsedTime: Double;
    FLoop: Boolean;
    FPlaying: Boolean;
  public
    constructor Create(const ASpriteSheetFile: string;
      AFrameWidth, AFrameHeight, AFrameCount: Integer;
      AFrameTime: Double);
    destructor Destroy; override;

    procedure Play(ALoop: Boolean = True);
    procedure Stop;
    procedure Reset;
    procedure Update(ADeltaTime: Double);
    procedure Draw(ACanvas: TCanvas; X, Y: Integer);

    property CurrentFrame: Integer read FCurrentFrame write FCurrentFrame;
    property Playing: Boolean read FPlaying;
  end;

constructor TSpriteAnimation.Create(const ASpriteSheetFile: string;
  AFrameWidth, AFrameHeight, AFrameCount: Integer; AFrameTime: Double);
begin
  FSpriteSheet := TBGRABitmap.Create(ASpriteSheetFile);
  FFrameWidth := AFrameWidth;
  FFrameHeight := AFrameHeight;
  FFrameCount := AFrameCount;
  FFrameTime := AFrameTime;
  FCurrentFrame := 0;
  FElapsedTime := 0;
  FLoop := True;
  FPlaying := False;
end;

destructor TSpriteAnimation.Destroy;
begin
  FSpriteSheet.Free;
  inherited;
end;

procedure TSpriteAnimation.Play(ALoop: Boolean);
begin
  FPlaying := True;
  FLoop := ALoop;
end;

procedure TSpriteAnimation.Stop;
begin
  FPlaying := False;
end;

procedure TSpriteAnimation.Reset;
begin
  FCurrentFrame := 0;
  FElapsedTime := 0;
end;

procedure TSpriteAnimation.Update(ADeltaTime: Double);
begin
  if not FPlaying then
    Exit;

  FElapsedTime := FElapsedTime + ADeltaTime;

  if FElapsedTime >= FFrameTime then
  begin
    FElapsedTime := FElapsedTime - FFrameTime;
    Inc(FCurrentFrame);

    if FCurrentFrame >= FFrameCount then
    begin
      if FLoop then
        FCurrentFrame := 0
      else
      begin
        FCurrentFrame := FFrameCount - 1;
        FPlaying := False;
      end;
    end;
  end;
end;

procedure TSpriteAnimation.Draw(ACanvas: TCanvas; X, Y: Integer);
var
  SourceRect: TRect;
  Frame: TBGRABitmap;
begin
  // Calculer la position de la frame dans le sprite sheet
  SourceRect := Rect(
    FCurrentFrame * FFrameWidth,
    0,
    (FCurrentFrame + 1) * FFrameWidth,
    FFrameHeight
  );

  // Extraire et dessiner la frame
  Frame := FSpriteSheet.GetPart(SourceRect) as TBGRABitmap;
  try
    Frame.Draw(ACanvas, X, Y, False);
  finally
    Frame.Free;
  end;
end;
```

### Utilisation

```pascal
var
  PlayerAnimation: TSpriteAnimation;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Sprite sheet avec 8 frames de 64x64 pixels, 0.1s par frame
  PlayerAnimation := TSpriteAnimation.Create('player_walk.png', 64, 64, 8, 0.1);
  PlayerAnimation.Play(True);  // Boucle
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  PlayerAnimation.Update(DeltaTime);
  PaintBox1.Invalidate;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PlayerAnimation.Draw(PaintBox1.Canvas, 100, 100);
end;
```

---

## Animations de particules

Les systèmes de particules créent des effets visuels complexes (feu, fumée, pluie, explosions).

### Système de particules simple

```pascal
type
  TParticle = record
    X, Y: Double;
    VX, VY: Double;  // Vélocité
    Life: Double;
    MaxLife: Double;
    Color: TBGRAPixel;
    Size: Double;
  end;

  TParticleSystem = class
  private
    FParticles: array of TParticle;
    FCount: Integer;
    FMaxParticles: Integer;
    FGravity: Double;
  public
    constructor Create(AMaxParticles: Integer);
    procedure Emit(AX, AY: Double; ACount: Integer);
    procedure Update(ADeltaTime: Double);
    procedure Draw(ABitmap: TBGRABitmap);

    property Gravity: Double read FGravity write FGravity;
  end;

constructor TParticleSystem.Create(AMaxParticles: Integer);
begin
  FMaxParticles := AMaxParticles;
  SetLength(FParticles, FMaxParticles);
  FCount := 0;
  FGravity := 98.0;  // Pixels par seconde²
end;

procedure TParticleSystem.Emit(AX, AY: Double; ACount: Integer);
var
  i: Integer;
  Angle, Speed: Double;
begin
  for i := 1 to ACount do
  begin
    if FCount >= FMaxParticles then
      Break;

    // Angle et vitesse aléatoires
    Angle := Random * 2 * Pi;
    Speed := 50 + Random * 100;

    FParticles[FCount].X := AX;
    FParticles[FCount].Y := AY;
    FParticles[FCount].VX := Cos(Angle) * Speed;
    FParticles[FCount].VY := Sin(Angle) * Speed;
    FParticles[FCount].Life := 1.0 + Random;  // 1-2 secondes
    FParticles[FCount].MaxLife := FParticles[FCount].Life;
    FParticles[FCount].Size := 2 + Random * 3;
    FParticles[FCount].Color := BGRA(
      200 + Random(56),
      100 + Random(100),
      0,
      255
    );

    Inc(FCount);
  end;
end;

procedure TParticleSystem.Update(ADeltaTime: Double);
var
  i: Integer;
begin
  i := 0;
  while i < FCount do
  begin
    with FParticles[i] do
    begin
      // Mise à jour position
      X := X + VX * ADeltaTime;
      Y := Y + VY * ADeltaTime;

      // Appliquer la gravité
      VY := VY + FGravity * ADeltaTime;

      // Durée de vie
      Life := Life - ADeltaTime;

      // Supprimer si morte
      if Life <= 0 then
      begin
        // Remplacer par la dernière particule
        FParticles[i] := FParticles[FCount - 1];
        Dec(FCount);
        Continue;  // Ne pas incrémenter i
      end;
    end;
    Inc(i);
  end;
end;

procedure TParticleSystem.Draw(ABitmap: TBGRABitmap);
var
  i: Integer;
  Alpha: Byte;
begin
  for i := 0 to FCount - 1 do
  begin
    with FParticles[i] do
    begin
      // Alpha proportionnel à la durée de vie
      Alpha := Round((Life / MaxLife) * 255);
      Color.alpha := Alpha;

      // Dessiner la particule
      ABitmap.FillEllipseAntialias(X, Y, Size, Size, Color);
    end;
  end;
end;
```

---

## Animations procédurales

### Mouvement sinusoïdal

```pascal
type
  TSineWaveAnimation = class
  private
    FAmplitude: Double;
    FFrequency: Double;
    FPhase: Double;
    FTime: Double;
  public
    constructor Create(AAmplitude, AFrequency: Double);
    procedure Update(ADeltaTime: Double);
    function GetValue: Double;
    procedure Reset;
  end;

constructor TSineWaveAnimation.Create(AAmplitude, AFrequency: Double);
begin
  FAmplitude := AAmplitude;
  FFrequency := AFrequency;
  FPhase := 0;
  FTime := 0;
end;

procedure TSineWaveAnimation.Update(ADeltaTime: Double);
begin
  FTime := FTime + ADeltaTime;
end;

function TSineWaveAnimation.GetValue: Double;
begin
  Result := FAmplitude * Sin(2 * Pi * FFrequency * FTime + FPhase);
end;

procedure TSineWaveAnimation.Reset;
begin
  FTime := 0;
end;

// Utilisation : mouvement oscillant
var
  WaveAnim: TSineWaveAnimation;
  BaseY: Integer;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WaveAnim := TSineWaveAnimation.Create(50, 0.5);  // Amplitude 50px, freq 0.5Hz
  BaseY := 200;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  WaveAnim.Update(DeltaTime);
  PaintBox1.Invalidate;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  Y: Integer;
begin
  Y := BaseY + Round(WaveAnim.GetValue);
  PaintBox1.Canvas.Ellipse(100, Y - 25, 150, Y + 25);
end;
```

### Courbes de Bézier animées

```pascal
function CubicBezier(P0, P1, P2, P3: TPointF; t: Double): TPointF;
var
  u, uu, uuu, tt, ttt: Double;
begin
  u := 1 - t;
  uu := u * u;
  uuu := uu * u;
  tt := t * t;
  ttt := tt * t;

  Result.x := uuu * P0.x + 3 * uu * t * P1.x + 3 * u * tt * P2.x + ttt * P3.x;
  Result.y := uuu * P0.y + 3 * uu * t * P1.y + 3 * u * tt * P2.y + ttt * P3.y;
end;

type
  TBezierAnimation = class
  private
    FP0, FP1, FP2, FP3: TPointF;
    FTime: Double;
    FDuration: Double;
  public
    constructor Create(const P0, P1, P2, P3: TPointF; ADuration: Double);
    procedure Update(ADeltaTime: Double);
    function GetPosition: TPointF;
    function IsComplete: Boolean;
  end;

constructor TBezierAnimation.Create(const P0, P1, P2, P3: TPointF; ADuration: Double);
begin
  FP0 := P0;
  FP1 := P1;
  FP2 := P2;
  FP3 := P3;
  FDuration := ADuration;
  FTime := 0;
end;

procedure TBezierAnimation.Update(ADeltaTime: Double);
begin
  FTime := FTime + ADeltaTime;
  if FTime > FDuration then
    FTime := FDuration;
end;

function TBezierAnimation.GetPosition: TPointF;
var
  t: Double;
begin
  if FDuration > 0 then
    t := FTime / FDuration
  else
    t := 1.0;

  Result := CubicBezier(FP0, FP1, FP2, FP3, t);
end;

function TBezierAnimation.IsComplete: Boolean;
begin
  Result := FTime >= FDuration;
end;

// Utilisation
var
  BezierAnim: TBezierAnimation;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BezierAnim := TBezierAnimation.Create(
    PointF(50, 300),    // Point de départ
    PointF(150, 50),    // Premier point de contrôle
    PointF(350, 50),    // Deuxième point de contrôle
    PointF(450, 300),   // Point d'arrivée
    3.0                 // Durée 3 secondes
  );
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  Pos: TPointF;
begin
  Pos := BezierAnim.GetPosition;

  // Dessiner l'objet animé
  PaintBox1.Canvas.Brush.Color := clRed;
  PaintBox1.Canvas.Ellipse(
    Round(Pos.x) - 10,
    Round(Pos.y) - 10,
    Round(Pos.x) + 10,
    Round(Pos.y) + 10
  );
end;
```

---

## Transitions d'interface

Les transitions entre écrans ou états donnent une sensation de continuité.

### Gestionnaire de transitions

```pascal
type
  TTransitionType = (ttSlideLeft, ttSlideRight, ttSlideUp, ttSlideDown,
                     ttFade, ttZoom, ttRotate);

  TScreenTransition = class
  private
    FSourceBitmap: TBGRABitmap;
    FTargetBitmap: TBGRABitmap;
    FTransitionType: TTransitionType;
    FProgress: Double;
    FDuration: Double;
    FTime: Double;
    FActive: Boolean;
  public
    constructor Create(ADuration: Double);
    destructor Destroy; override;

    procedure Start(ASource, ATarget: TBGRABitmap; AType: TTransitionType);
    procedure Update(ADeltaTime: Double);
    procedure Draw(ABitmap: TBGRABitmap);

    property Active: Boolean read FActive;
    property Progress: Double read FProgress;
  end;

constructor TScreenTransition.Create(ADuration: Double);
begin
  FDuration := ADuration;
  FActive := False;
  FSourceBitmap := nil;
  FTargetBitmap := nil;
end;

destructor TScreenTransition.Destroy;
begin
  // Ne pas libérer les bitmaps source/target (ils appartiennent à l'appelant)
  inherited;
end;

procedure TScreenTransition.Start(ASource, ATarget: TBGRABitmap; AType: TTransitionType);
begin
  FSourceBitmap := ASource;
  FTargetBitmap := ATarget;
  FTransitionType := AType;
  FTime := 0;
  FProgress := 0;
  FActive := True;
end;

procedure TScreenTransition.Update(ADeltaTime: Double);
begin
  if not FActive then
    Exit;

  FTime := FTime + ADeltaTime;
  FProgress := FTime / FDuration;

  if FProgress >= 1.0 then
  begin
    FProgress := 1.0;
    FActive := False;
  end;
end;

procedure TScreenTransition.Draw(ABitmap: TBGRABitmap);
var
  t: Double;
  OffsetX, OffsetY: Integer;
  Alpha: Byte;
  Scale: Double;
  Angle: Double;
  Temp: TBGRABitmap;
begin
  if (FSourceBitmap = nil) or (FTargetBitmap = nil) then
    Exit;

  // Appliquer easing
  t := EaseInOutQuad(FProgress);

  case FTransitionType of
    ttSlideLeft:
      begin
        OffsetX := Round(-ABitmap.Width * t);
        ABitmap.PutImage(OffsetX, 0, FSourceBitmap, dmSet);
        ABitmap.PutImage(ABitmap.Width + OffsetX, 0, FTargetBitmap, dmSet);
      end;

    ttSlideRight:
      begin
        OffsetX := Round(ABitmap.Width * t);
        ABitmap.PutImage(OffsetX, 0, FSourceBitmap, dmSet);
        ABitmap.PutImage(OffsetX - ABitmap.Width, 0, FTargetBitmap, dmSet);
      end;

    ttSlideUp:
      begin
        OffsetY := Round(-ABitmap.Height * t);
        ABitmap.PutImage(0, OffsetY, FSourceBitmap, dmSet);
        ABitmap.PutImage(0, ABitmap.Height + OffsetY, FTargetBitmap, dmSet);
      end;

    ttSlideDown:
      begin
        OffsetY := Round(ABitmap.Height * t);
        ABitmap.PutImage(0, OffsetY, FSourceBitmap, dmSet);
        ABitmap.PutImage(0, OffsetY - ABitmap.Height, FTargetBitmap, dmSet);
      end;

    ttFade:
      begin
        // Dessiner source avec alpha décroissant
        Temp := FSourceBitmap.Duplicate;
        try
          Alpha := Round((1 - t) * 255);
          Temp.ApplyGlobalOpacity(Alpha);
          ABitmap.PutImage(0, 0, Temp, dmDrawWithTransparency);
        finally
          Temp.Free;
        end;

        // Dessiner target avec alpha croissant
        Temp := FTargetBitmap.Duplicate;
        try
          Alpha := Round(t * 255);
          Temp.ApplyGlobalOpacity(Alpha);
          ABitmap.BlendImage(0, 0, Temp, boTransparent);
        finally
          Temp.Free;
        end;
      end;

    ttZoom:
      begin
        // Source qui rétrécit
        Scale := 1.0 - t;
        if Scale > 0.1 then
        begin
          Temp := FSourceBitmap.Resample(
            Round(FSourceBitmap.Width * Scale),
            Round(FSourceBitmap.Height * Scale),
            rmFineResample
          );
          try
            ABitmap.PutImage(
              (ABitmap.Width - Temp.Width) div 2,
              (ABitmap.Height - Temp.Height) div 2,
              Temp,
              dmSet
            );
          finally
            Temp.Free;
          end;
        end;

        // Target qui grandit par-dessus
        Scale := t;
        if Scale > 0.1 then
        begin
          Temp := FTargetBitmap.Resample(
            Round(FTargetBitmap.Width * Scale),
            Round(FTargetBitmap.Height * Scale),
            rmFineResample
          );
          try
            Temp.ApplyGlobalOpacity(Round(t * 255));
            ABitmap.BlendImage(
              (ABitmap.Width - Temp.Width) div 2,
              (ABitmap.Height - Temp.Height) div 2,
              Temp,
              boTransparent
            );
          finally
            Temp.Free;
          end;
        end;
      end;

    ttRotate:
      begin
        Angle := t * 90;  // Rotation de 0 à 90 degrés

        // Source qui tourne
        Temp := FSourceBitmap.RotateAffine(-Angle, 0, 0);
        try
          Temp.ApplyGlobalOpacity(Round((1 - t) * 255));
          ABitmap.PutImage(
            (ABitmap.Width - Temp.Width) div 2,
            (ABitmap.Height - Temp.Height) div 2,
            Temp,
            dmDrawWithTransparency
          );
        finally
          Temp.Free;
        end;

        // Target qui apparaît
        Temp := FTargetBitmap.RotateAffine(90 - Angle, 0, 0);
        try
          Temp.ApplyGlobalOpacity(Round(t * 255));
          ABitmap.BlendImage(
            (ABitmap.Width - Temp.Width) div 2,
            (ABitmap.Height - Temp.Height) div 2,
            Temp,
            boTransparent
          );
        finally
          Temp.Free;
        end;
      end;
  end;
end;
```

---

## Animations de composants LCL

### Animer la position d'un composant

```pascal
type
  TControlAnimation = class
  private
    FControl: TControl;
    FStartLeft, FStartTop: Integer;
    FEndLeft, FEndTop: Integer;
    FProgress: Double;
    FDuration: Double;
    FTime: Double;
    FActive: Boolean;
    FEasing: function(t: Double): Double;
  public
    constructor Create(AControl: TControl);
    procedure MoveTo(ALeft, ATop: Integer; ADuration: Double);
    procedure Update(ADeltaTime: Double);

    property Active: Boolean read FActive;
  end;

constructor TControlAnimation.Create(AControl: TControl);
begin
  FControl := AControl;
  FActive := False;
  FEasing := @EaseInOutQuad;
end;

procedure TControlAnimation.MoveTo(ALeft, ATop: Integer; ADuration: Double);
begin
  FStartLeft := FControl.Left;
  FStartTop := FControl.Top;
  FEndLeft := ALeft;
  FEndTop := ATop;
  FDuration := ADuration;
  FTime := 0;
  FProgress := 0;
  FActive := True;
end;

procedure TControlAnimation.Update(ADeltaTime: Double);
var
  t: Double;
begin
  if not FActive then
    Exit;

  FTime := FTime + ADeltaTime;
  FProgress := FTime / FDuration;

  if FProgress >= 1.0 then
  begin
    FProgress := 1.0;
    FActive := False;
  end;

  // Appliquer l'easing
  t := FEasing(FProgress);

  // Calculer et appliquer la nouvelle position
  FControl.Left := Round(FStartLeft + (FEndLeft - FStartLeft) * t);
  FControl.Top := Round(FStartTop + (FEndTop - FStartTop) * t);
end;

// Utilisation
var
  ButtonAnim: TControlAnimation;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ButtonAnim := TControlAnimation.Create(Button1);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // Animer Button1 vers une nouvelle position
  ButtonAnim.MoveTo(300, 200, 1.0);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  ButtonAnim.Update(DeltaTime);
end;
```

### Animer la taille d'un composant

```pascal
type
  TControlSizeAnimation = class
  private
    FControl: TControl;
    FStartWidth, FStartHeight: Integer;
    FEndWidth, FEndHeight: Integer;
    FProgress: Double;
    FDuration: Double;
    FTime: Double;
    FActive: Boolean;
  public
    constructor Create(AControl: TControl);
    procedure ResizeTo(AWidth, AHeight: Integer; ADuration: Double);
    procedure Update(ADeltaTime: Double);
  end;

constructor TControlSizeAnimation.Create(AControl: TControl);
begin
  FControl := AControl;
  FActive := False;
end;

procedure TControlSizeAnimation.ResizeTo(AWidth, AHeight: Integer; ADuration: Double);
begin
  FStartWidth := FControl.Width;
  FStartHeight := FControl.Height;
  FEndWidth := AWidth;
  FEndHeight := AHeight;
  FDuration := ADuration;
  FTime := 0;
  FProgress := 0;
  FActive := True;
end;

procedure TControlSizeAnimation.Update(ADeltaTime: Double);
var
  t: Double;
begin
  if not FActive then
    Exit;

  FTime := FTime + ADeltaTime;
  FProgress := FTime / FDuration;

  if FProgress >= 1.0 then
  begin
    FProgress := 1.0;
    FActive := False;
  end;

  t := EaseInOutQuad(FProgress);

  FControl.Width := Round(FStartWidth + (FEndWidth - FStartWidth) * t);
  FControl.Height := Round(FStartHeight + (FEndHeight - FStartHeight) * t);
end;
```

---

## Animations 3D avec OpenGL

### Rotation 3D animée

```pascal
type
  T3DRotationAnimation = class
  private
    FAngleX, FAngleY, FAngleZ: Double;
    FSpeedX, FSpeedY, FSpeedZ: Double;
  public
    constructor Create;
    procedure SetSpeed(ASpeedX, ASpeedY, ASpeedZ: Double);
    procedure Update(ADeltaTime: Double);
    procedure ApplyToOpenGL;
  end;

constructor T3DRotationAnimation.Create;
begin
  FAngleX := 0;
  FAngleY := 0;
  FAngleZ := 0;
  FSpeedX := 0;
  FSpeedY := 0;
  FSpeedZ := 0;
end;

procedure T3DRotationAnimation.SetSpeed(ASpeedX, ASpeedY, ASpeedZ: Double);
begin
  FSpeedX := ASpeedX;
  FSpeedY := ASpeedY;
  FSpeedZ := ASpeedZ;
end;

procedure T3DRotationAnimation.Update(ADeltaTime: Double);
begin
  FAngleX := FAngleX + FSpeedX * ADeltaTime;
  FAngleY := FAngleY + FSpeedY * ADeltaTime;
  FAngleZ := FAngleZ + FSpeedZ * ADeltaTime;

  // Normaliser les angles (0-360)
  while FAngleX >= 360 do FAngleX := FAngleX - 360;
  while FAngleY >= 360 do FAngleY := FAngleY - 360;
  while FAngleZ >= 360 do FAngleZ := FAngleZ - 360;
end;

procedure T3DRotationAnimation.ApplyToOpenGL;
begin
  glRotatef(FAngleX, 1.0, 0.0, 0.0);
  glRotatef(FAngleY, 0.0, 1.0, 0.0);
  glRotatef(FAngleZ, 0.0, 0.0, 1.0);
end;

// Utilisation dans une scène OpenGL
var
  RotAnim: T3DRotationAnimation;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RotAnim := T3DRotationAnimation.Create;
  RotAnim.SetSpeed(30, 45, 0);  // Rotation en degrés/seconde
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  glTranslatef(0, 0, -5);
  RotAnim.ApplyToOpenGL;

  // Dessiner un cube
  DrawCube;

  OpenGLControl1.SwapBuffers;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  RotAnim.Update(DeltaTime);
  OpenGLControl1.Invalidate;
end;
```

---

## Gestionnaire d'animations centralisé

Pour gérer plusieurs animations simultanément :

```pascal
type
  TAnimationManager = class
  private
    FAnimations: TList;
    FLastTime: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AAnimation: TObject);
    procedure Remove(AAnimation: TObject);
    procedure Update;
    procedure Clear;
  end;

constructor TAnimationManager.Create;
begin
  FAnimations := TList.Create;
  FLastTime := Now;
end;

destructor TAnimationManager.Destroy;
begin
  Clear;
  FAnimations.Free;
  inherited;
end;

procedure TAnimationManager.Add(AAnimation: TObject);
begin
  if FAnimations.IndexOf(AAnimation) < 0 then
    FAnimations.Add(AAnimation);
end;

procedure TAnimationManager.Remove(AAnimation: TObject);
begin
  FAnimations.Remove(AAnimation);
end;

procedure TAnimationManager.Update;
var
  CurrentTime: TDateTime;
  DeltaTime: Double;
  i: Integer;
  Anim: TAnimation;
begin
  CurrentTime := Now;
  DeltaTime := MilliSecondsBetween(CurrentTime, FLastTime) / 1000.0;
  FLastTime := CurrentTime;

  // Mettre à jour toutes les animations
  for i := FAnimations.Count - 1 downto 0 do
  begin
    if FAnimations[i] is TAnimation then
    begin
      Anim := TAnimation(FAnimations[i]);
      Anim.Update(DeltaTime);

      // Supprimer si terminée
      if Anim.State = asCompleted then
        FAnimations.Delete(i);
    end;
  end;
end;

procedure TAnimationManager.Clear;
var
  i: Integer;
begin
  for i := 0 to FAnimations.Count - 1 do
    TObject(FAnimations[i]).Free;

  FAnimations.Clear;
end;

// Utilisation globale
var
  AnimationManager: TAnimationManager;

procedure TForm1.FormCreate(Sender: TObject);
var
  Anim: TAnimation;
begin
  AnimationManager := TAnimationManager.Create;

  // Ajouter plusieurs animations
  Anim := TAnimation.Create(0, 400, 2.0);
  Anim.Start;
  AnimationManager.Add(Anim);

  Anim := TAnimation.Create(100, 300, 1.5);
  Anim.Start;
  AnimationManager.Add(Anim);

  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  AnimationManager.Update;
  PaintBox1.Invalidate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AnimationManager.Free;
end;
```

---

## Optimisation des animations

### Techniques d'optimisation

#### 1. Dirty rectangles

Ne redessiner que les zones modifiées :

```pascal
type
  TDirtyRect = record
    X, Y, Width, Height: Integer;
    Dirty: Boolean;
  end;

var
  DirtyRegions: array of TDirtyRect;

procedure MarkDirty(X, Y, Width, Height: Integer);
var
  i: Integer;
begin
  i := Length(DirtyRegions);
  SetLength(DirtyRegions, i + 1);

  DirtyRegions[i].X := X;
  DirtyRegions[i].Y := Y;
  DirtyRegions[i].Width := Width;
  DirtyRegions[i].Height := Height;
  DirtyRegions[i].Dirty := True;
end;

procedure RedrawDirtyRegions(ACanvas: TCanvas);
var
  i: Integer;
  R: TRect;
begin
  for i := 0 to High(DirtyRegions) do
  begin
    if DirtyRegions[i].Dirty then
    begin
      R := Rect(
        DirtyRegions[i].X,
        DirtyRegions[i].Y,
        DirtyRegions[i].X + DirtyRegions[i].Width,
        DirtyRegions[i].Y + DirtyRegions[i].Height
      );

      // Redessiner uniquement cette zone
      RedrawRegion(ACanvas, R);

      DirtyRegions[i].Dirty := False;
    end;
  end;

  SetLength(DirtyRegions, 0);
end;
```

#### 2. Object pooling

Réutiliser les objets au lieu de les recréer :

```pascal
type
  TParticlePool = class
  private
    FPool: array of TParticle;
    FActive: array of Boolean;
  public
    constructor Create(ASize: Integer);
    function Acquire: Integer;  // Retourne l'index
    procedure Release(AIndex: Integer);
  end;

constructor TParticlePool.Create(ASize: Integer);
begin
  SetLength(FPool, ASize);
  SetLength(FActive, ASize);
  FillChar(FActive[0], Length(FActive), 0);
end;

function TParticlePool.Acquire: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FPool) do
  begin
    if not FActive[i] then
    begin
      FActive[i] := True;
      Result := i;
      Exit;
    end;
  end;
end;

procedure TParticlePool.Release(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < Length(FActive)) then
    FActive[AIndex] := False;
end;
```

#### 3. Level of Detail (LOD)

Réduire la qualité pour les objets lointains :

```pascal
procedure DrawAnimatedObject(X, Y: Integer; Distance: Double);
var
  Detail: Integer;
begin
  if Distance < 100 then
    Detail := 3  // Haute qualité
  else if Distance < 300 then
    Detail := 2  // Qualité moyenne
  else
    Detail := 1; // Basse qualité

  case Detail of
    3: DrawHighQuality(X, Y);
    2: DrawMediumQuality(X, Y);
    1: DrawLowQuality(X, Y);
  end;
end;
```

---

## Débogage des animations

### Affichage du FPS

```pascal
type
  TFPSCounter = class
  private
    FFrameCount: Integer;
    FLastTime: TDateTime;
    FFPS: Integer;
  public
    constructor Create;
    procedure Update;
    function GetFPS: Integer;
  end;

constructor TFPSCounter.Create;
begin
  FFrameCount := 0;
  FFPS := 0;
  FLastTime := Now;
end;

procedure TFPSCounter.Update;
var
  CurrentTime: TDateTime;
  Elapsed: Int64;
begin
  Inc(FFrameCount);
  CurrentTime := Now;
  Elapsed := MilliSecondsBetween(CurrentTime, FLastTime);

  if Elapsed >= 1000 then
  begin
    FFPS := FFrameCount;
    FFrameCount := 0;
    FLastTime := CurrentTime;
  end;
end;

function TFPSCounter.GetFPS: Integer;
begin
  Result := FFPS;
end;

// Utilisation
var
  FPSCounter: TFPSCounter;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPSCounter := TFPSCounter.Create;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  FPSCounter.Update;
  Caption := Format('FPS: %d', [FPSCounter.GetFPS]);
end;
```

### Mode débogage visuel

```pascal
var
  DebugMode: Boolean = False;

procedure DrawDebugInfo(ACanvas: TCanvas);
begin
  if not DebugMode then
    Exit;

  // Afficher les bounding boxes
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Style := psDot;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(BoundingBox);

  // Afficher les vecteurs de vélocité
  ACanvas.Pen.Color := clBlue;
  ACanvas.Pen.Style := psSolid;
  ACanvas.MoveTo(X, Y);
  ACanvas.LineTo(Round(X + VelocityX * 10), Round(Y + VelocityY * 10));

  // Afficher les informations textuelles
  ACanvas.TextOut(10, 10, Format('Pos: %.1f, %.1f', [X, Y]));
  ACanvas.TextOut(10, 30, Format('Vel: %.1f, %.1f', [VelocityX, VelocityY]));
end;
```

---

## Bibliothèques d'animation tierces

### Considérer l'utilisation de bibliothèques

Pour des projets complexes, envisagez :

#### TAChart (inclus avec Lazarus)
- Animations de graphiques
- Transitions de données
- Simple d'utilisation

#### BGRAControls
- Composants animés prêts à l'emploi
- Effets visuels intégrés
- Basé sur BGRABitmap

#### Castle Game Engine
- Moteur de jeu complet
- Système d'animation 3D
- Gestion du son et physique

---

## Bonnes pratiques

### ✅ À faire

1. **Utiliser le delta time** pour des animations frame-rate independent
2. **Limiter le nombre d'animations** simultanées pour les performances
3. **Réutiliser les objets** (object pooling) au lieu de les recréer
4. **Tester sur différentes configurations** matérielles
5. **Préférer les easing functions** aux mouvements linéaires
6. **Mesurer les performances** avec un compteur FPS
7. **Nettoyer les animations terminées** pour éviter les fuites mémoire

### ❌ À éviter

1. ❌ Animer trop d'objets à la fois sans optimisation
2. ❌ Créer/détruire des objets dans la boucle d'animation
3. ❌ Oublier de limiter le delta time (peut causer des sauts)
4. ❌ Utiliser des fonctions coûteuses (sin, cos) sans cache
5. ❌ Redessiner tout l'écran si seule une petite zone change
6. ❌ Ignorer les performances sur du matériel ancien

---

## Ressources et inspiration

### Références d'easing

- **Easings.net** : https://easings.net/ (visualisations interactives)
- **Cubic-bezier.com** : Outil pour créer des courbes personnalisées
- **Robert Penner's Easing Functions** : Référence classique

### Inspiration visuelle

- **Dribbble** : https://dribbble.com/ (designs animés)
- **CodePen** : Exemples d'animations web
- **Motion Design** : Principes d'animation professionnelle

### Principes d'animation Disney

Les 12 principes classiques (adaptables au code) :
1. **Squash and Stretch** : Déformation pour l'impact
2. **Anticipation** : Mouvement préparatoire
3. **Staging** : Mise en scène claire
4. **Straight Ahead vs Pose to Pose** : Animation directe ou par étapes clés
5. **Follow Through and Overlapping Action** : Continuation du mouvement
6. **Slow In and Slow Out** : Accélération/décélération (= easing!)
7. **Arcs** : Mouvements naturels en arc
8. **Secondary Action** : Actions secondaires pour enrichir
9. **Timing** : Contrôle de la vitesse
10. **Exaggeration** : Amplification pour l'effet
11. **Solid Drawing** : Dessin cohérent
12. **Appeal** : Attractivité visuelle

---

## Exemple complet : Application d'animation

Voici une application complète démontrant plusieurs techniques :

```pascal
unit AnimationDemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BGRABitmap, BGRABitmapTypes, DateUtils, Math;

type
  TMainForm = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    Panel1: TPanel;
    ButtonBounce: TButton;
    ButtonFade: TButton;
    ButtonParticles: TButton;
    ButtonRotate: TButton;
    ButtonReset: TButton;
    LabelFPS: TLabel;
    CheckBoxDebug: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure ButtonBounceClick(Sender: TObject);
    procedure ButtonFadeClick(Sender: TObject);
    procedure ButtonParticlesClick(Sender: TObject);
    procedure ButtonRotateClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
  private
    FBuffer: TBGRABitmap;
    FLastTime: TDateTime;
    FDeltaTime: Double;
    FFrameCount: Integer;
    FFPS: Integer;
    FLastFPSUpdate: TDateTime;

    // Animation de rebond (balle)
    FBallY: Double;
    FBallVelocity: Double;
    FBallActive: Boolean;
    FBallGravity: Double;
    FBallBounce: Double;

    // Animation de fade
    FFadeAlpha: Double;
    FFadeDirection: Integer;
    FFadeActive: Boolean;

    // Système de particules
    FParticles: array of record
      X, Y: Double;
      VX, VY: Double;
      Life: Double;
      Color: TBGRAPixel;
      Size: Double;
    end;
    FParticleCount: Integer;

    // Animation de rotation
    FRotateAngle: Double;
    FRotateSpeed: Double;
    FRotateActive: Boolean;

    procedure UpdateDeltaTime;
    procedure UpdateFPS;
    procedure ResetAnimations;
    procedure UpdateBounce;
    procedure UpdateFade;
    procedure UpdateParticles;
    procedure UpdateRotation;
    procedure DrawBounce;
    procedure DrawFade;
    procedure DrawParticles;
    procedure DrawRotation;
    procedure DrawDebugInfo;
    procedure EmitParticles(AX, AY: Integer; Count: Integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

// Fonctions d'easing
function EaseOutBounce(t: Double): Double;
const
  n1 = 7.5625;
  d1 = 2.75;
begin
  if t < 1 / d1 then
    Result := n1 * t * t
  else if t < 2 / d1 then
  begin
    t := t - 1.5 / d1;
    Result := n1 * t * t + 0.75;
  end
  else if t < 2.5 / d1 then
  begin
    t := t - 2.25 / d1;
    Result := n1 * t * t + 0.9375;
  end
  else
  begin
    t := t - 2.625 / d1;
    Result := n1 * t * t + 0.984375;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBuffer := TBGRABitmap.Create(PaintBox1.Width, PaintBox1.Height);
  FLastTime := Now;
  FLastFPSUpdate := Now;
  FFrameCount := 0;
  FFPS := 0;

  ResetAnimations;

  Timer1.Interval := 16;  // ~60 FPS
  Timer1.Enabled := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FBuffer.Free;
end;

procedure TMainForm.ResetAnimations;
begin
  // Réinitialiser toutes les animations
  FBallY := 50;
  FBallVelocity := 0;
  FBallActive := False;
  FBallGravity := 980;  // pixels/s²
  FBallBounce := 0.7;   // Coefficient de rebond

  FFadeAlpha := 255;
  FFadeDirection := 0;
  FFadeActive := False;

  SetLength(FParticles, 500);
  FParticleCount := 0;

  FRotateAngle := 0;
  FRotateSpeed := 0;
  FRotateActive := False;
end;

procedure TMainForm.UpdateDeltaTime;
var
  CurrentTime: TDateTime;
begin
  CurrentTime := Now;
  FDeltaTime := MilliSecondsBetween(CurrentTime, FLastTime) / 1000.0;

  // Limiter le delta time pour éviter les sauts
  if FDeltaTime > 0.1 then
    FDeltaTime := 0.1;

  FLastTime := CurrentTime;
end;

procedure TMainForm.UpdateFPS;
var
  CurrentTime: TDateTime;
  Elapsed: Int64;
begin
  Inc(FFrameCount);
  CurrentTime := Now;
  Elapsed := MilliSecondsBetween(CurrentTime, FLastFPSUpdate);

  if Elapsed >= 1000 then
  begin
    FFPS := FFrameCount;
    FFrameCount := 0;
    FLastFPSUpdate := CurrentTime;
    LabelFPS.Caption := Format('FPS: %d', [FFPS]);
  end;
end;

procedure TMainForm.UpdateBounce;
var
  FloorY: Double;
begin
  if not FBallActive then
    Exit;

  FloorY := FBuffer.Height - 50;

  // Appliquer la gravité
  FBallVelocity := FBallVelocity + FBallGravity * FDeltaTime;

  // Mettre à jour la position
  FBallY := FBallY + FBallVelocity * FDeltaTime;

  // Collision avec le sol
  if FBallY >= FloorY then
  begin
    FBallY := FloorY;
    FBallVelocity := -FBallVelocity * FBallBounce;

    // Arrêter si vitesse trop faible
    if Abs(FBallVelocity) < 50 then
    begin
      FBallVelocity := 0;
      FBallActive := False;
    end;
  end;
end;

procedure TMainForm.UpdateFade;
begin
  if not FFadeActive then
    Exit;

  FFadeAlpha := FFadeAlpha + FFadeDirection * 255 * FDeltaTime;

  // Limiter 0-255
  if FFadeAlpha < 0 then
  begin
    FFadeAlpha := 0;
    FFadeDirection := 1;  // Inverser
  end
  else if FFadeAlpha > 255 then
  begin
    FFadeAlpha := 255;
    FFadeDirection := -1;  // Inverser
  end;
end;

procedure TMainForm.UpdateParticles;
var
  i: Integer;
begin
  i := 0;
  while i < FParticleCount do
  begin
    with FParticles[i] do
    begin
      // Mise à jour position
      X := X + VX * FDeltaTime;
      Y := Y + VY * FDeltaTime;

      // Gravité
      VY := VY + 400 * FDeltaTime;

      // Durée de vie
      Life := Life - FDeltaTime;

      // Supprimer si morte
      if Life <= 0 then
      begin
        FParticles[i] := FParticles[FParticleCount - 1];
        Dec(FParticleCount);
        Continue;
      end;
    end;
    Inc(i);
  end;
end;

procedure TMainForm.UpdateRotation;
begin
  if not FRotateActive then
    Exit;

  FRotateAngle := FRotateAngle + FRotateSpeed * FDeltaTime;

  // Normaliser 0-360
  while FRotateAngle >= 360 do
    FRotateAngle := FRotateAngle - 360;
end;

procedure TMainForm.DrawBounce;
var
  X: Integer;
begin
  X := FBuffer.Width div 2;

  // Dessiner la balle avec ombre
  FBuffer.FillEllipseAntialias(X, FBallY + 5, 25, 25,
    BGRA(0, 0, 0, 50));  // Ombre
  FBuffer.FillEllipseAntialias(X, FBallY, 25, 25,
    BGRA(255, 100, 100));  // Balle

  // Dessiner le sol
  FBuffer.DrawLineAntialias(0, FBuffer.Height - 50,
    FBuffer.Width, FBuffer.Height - 50, BGRA(100, 100, 100), 2);
end;

procedure TMainForm.DrawFade;
var
  CenterX, CenterY: Integer;
  Alpha: Byte;
begin
  CenterX := FBuffer.Width div 2;
  CenterY := FBuffer.Height div 2;
  Alpha := Round(FFadeAlpha);

  FBuffer.FillEllipseAntialias(CenterX, CenterY, 80, 80,
    BGRA(100, 200, 255, Alpha));
end;

procedure TMainForm.DrawParticles;
var
  i: Integer;
  Alpha: Byte;
begin
  for i := 0 to FParticleCount - 1 do
  begin
    with FParticles[i] do
    begin
      Alpha := Round((Life / 2.0) * 255);
      if Alpha > 255 then Alpha := 255;
      Color.alpha := Alpha;

      FBuffer.FillEllipseAntialias(X, Y, Size, Size, Color);
    end;
  end;
end;

procedure TMainForm.DrawRotation;
var
  CenterX, CenterY: Integer;
  Rotated: TBGRABitmap;
  Square: TBGRABitmap;
begin
  CenterX := FBuffer.Width div 2 + 150;
  CenterY := FBuffer.Height div 2;

  // Créer un carré
  Square := TBGRABitmap.Create(80, 80);
  try
    Square.Fill(BGRA(255, 200, 0));
    Square.Rectangle(0, 0, 80, 80, BGRABlack, dmSet);

    // Rotation
    Rotated := Square.RotateAffine(FRotateAngle, 0, 0);
    try
      FBuffer.PutImage(
        CenterX - Rotated.Width div 2,
        CenterY - Rotated.Height div 2,
        Rotated,
        dmDrawWithTransparency
      );
    finally
      Rotated.Free;
    end;
  finally
    Square.Free;
  end;
end;

procedure TMainForm.DrawDebugInfo;
begin
  if not CheckBoxDebug.Checked then
    Exit;

  FBuffer.FontName := 'Arial';
  FBuffer.FontHeight := 12;
  FBuffer.FontQuality := fqFineAntialiasing;

  FBuffer.TextOut(10, 10, Format('Delta Time: %.3f ms', [FDeltaTime * 1000]),
    BGRAWhite);
  FBuffer.TextOut(10, 30, Format('Particles: %d', [FParticleCount]),
    BGRAWhite);
  FBuffer.TextOut(10, 50, Format('Ball Y: %.1f', [FBallY]),
    BGRAWhite);
  FBuffer.TextOut(10, 70, Format('Fade Alpha: %.1f', [FFadeAlpha]),
    BGRAWhite);
  FBuffer.TextOut(10, 90, Format('Rotation: %.1f°', [FRotateAngle]),
    BGRAWhite);
end;

procedure TMainForm.EmitParticles(AX, AY: Integer; Count: Integer);
var
  i: Integer;
  Angle, Speed: Double;
begin
  for i := 1 to Count do
  begin
    if FParticleCount >= Length(FParticles) then
      Break;

    Angle := Random * 2 * Pi;
    Speed := 100 + Random * 200;

    FParticles[FParticleCount].X := AX;
    FParticles[FParticleCount].Y := AY;
    FParticles[FParticleCount].VX := Cos(Angle) * Speed;
    FParticles[FParticleCount].VY := Sin(Angle) * Speed - 200;  // Vers le haut
    FParticles[FParticleCount].Life := 1.0 + Random;
    FParticles[FParticleCount].Size := 3 + Random * 4;
    FParticles[FParticleCount].Color := BGRA(
      150 + Random(106),
      50 + Random(100),
      Random(100),
      255
    );

    Inc(FParticleCount);
  end;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  UpdateDeltaTime;

  // Mettre à jour toutes les animations
  UpdateBounce;
  UpdateFade;
  UpdateParticles;
  UpdateRotation;

  UpdateFPS;

  // Redessiner
  PaintBox1.Invalidate;
end;

procedure TMainForm.PaintBox1Paint(Sender: TObject);
begin
  // Fond
  FBuffer.Fill(BGRA(20, 20, 40));

  // Dessiner toutes les animations actives
  if FBallActive then
    DrawBounce;

  if FFadeActive then
    DrawFade;

  if FParticleCount > 0 then
    DrawParticles;

  if FRotateActive then
    DrawRotation;

  // Infos de debug
  DrawDebugInfo;

  // Afficher
  FBuffer.Draw(PaintBox1.Canvas, 0, 0, False);
end;

procedure TMainForm.ButtonBounceClick(Sender: TObject);
begin
  FBallY := 50;
  FBallVelocity := 0;
  FBallActive := True;
end;

procedure TMainForm.ButtonFadeClick(Sender: TObject);
begin
  FFadeActive := not FFadeActive;
  if FFadeActive then
    FFadeDirection := -1;  // Commencer par fade out
end;

procedure TMainForm.ButtonParticlesClick(Sender: TObject);
begin
  EmitParticles(FBuffer.Width div 2, FBuffer.Height div 2, 50);
end;

procedure TMainForm.ButtonRotateClick(Sender: TObject);
begin
  FRotateActive := not FRotateActive;
  if FRotateActive then
    FRotateSpeed := 90;  // 90 degrés par seconde
  else
    FRotateSpeed := 0;
end;

procedure TMainForm.ButtonResetClick(Sender: TObject);
begin
  ResetAnimations;
end;

end.
```

### Fichier .lfm correspondant

```
object MainForm: TMainForm
  Left = 300
  Height = 600
  Top = 200
  Width = 800
  Caption = 'Démonstration d''animations - FreePascal/Lazarus'
  OnCreate = FormCreate
  OnDestroy = FormDestroy

  object Panel1: TPanel
    Left = 0
    Height = 80
    Top = 0
    Width = 800
    Align = alTop
    ClientHeight = 80
    ClientWidth = 800

    object ButtonBounce: TButton
      Left = 10
      Height = 30
      Top = 10
      Width = 120
      Caption = 'Balle rebondissante'
      OnClick = ButtonBounceClick
    end

    object ButtonFade: TButton
      Left = 140
      Height = 30
      Top = 10
      Width = 120
      Caption = 'Fade In/Out'
      OnClick = ButtonFadeClick
    end

    object ButtonParticles: TButton
      Left = 270
      Height = 30
      Top = 10
      Width = 120
      Caption = 'Particules'
      OnClick = ButtonParticlesClick
    end

    object ButtonRotate: TButton
      Left = 400
      Height = 30
      Top = 10
      Width = 120
      Caption = 'Rotation'
      OnClick = ButtonRotateClick
    end

    object ButtonReset: TButton
      Left = 530
      Height = 30
      Top = 10
      Width = 120
      Caption = 'Réinitialiser'
      OnClick = ButtonResetClick
    end

    object LabelFPS: TLabel
      Left = 10
      Height = 15
      Top = 50
      Width = 50
      Caption = 'FPS: 0'
    end

    object CheckBoxDebug: TCheckBox
      Left = 140
      Height = 20
      Top = 50
      Width = 120
      Caption = 'Mode Debug'
    end
  end

  object PaintBox1: TPaintBox
    Left = 0
    Height = 520
    Top = 80
    Width = 800
    Align = alClient
    OnPaint = PaintBox1Paint
  end

  object Timer1: TTimer
    Enabled = False
    Interval = 16
    OnTimer = Timer1Timer
  end
end
```

---

## Points clés de l'exemple

### Techniques démontrées

1. **Physique simple** : Gravité et rebonds
2. **Interpolation** : Fade in/out avec inversion automatique
3. **Système de particules** : Émission et gestion
4. **Rotation** : Transformation affine avec BGRABitmap
5. **Delta time** : Animations indépendantes du frame rate
6. **Compteur FPS** : Mesure de performance
7. **Mode debug** : Affichage d'informations utiles

### Architecture propre

- **Séparation Update/Draw** : Logique séparée du rendu
- **Réutilisation** : Buffer BGRABitmap persistent
- **Gestion d'état** : Flags pour activer/désactiver
- **Extensibilité** : Facile d'ajouter de nouvelles animations

---

## Conclusion

Les animations et transitions sont essentielles pour créer des interfaces modernes et engageantes. Points clés à retenir :

✅ **Delta time** : Essentiel pour la cohérence  
✅ **Easing functions** : Rendent les animations naturelles  
✅ **BGRABitmap** : Parfait pour les animations 2D avancées  
✅ **Optimisation** : Dirty rectangles, pooling, LOD  
✅ **Gestionnaire centralisé** : Pour gérer plusieurs animations  
✅ **Debugging** : Compteur FPS et mode debug visuel  
✅ **Architecture propre** : Séparer update et draw

### Ce que vous avez appris

1. **Fondamentaux** :
   - Delta time et frame rate
   - Interpolation linéaire et easing
   - Boucle de rendu

2. **Techniques intermédiaires** :
   - Animations de propriétés
   - Sprites animés
   - Transitions d'écran

3. **Techniques avancées** :
   - Systèmes de particules
   - Animations procédurales (Bézier, sinus)
   - Optimisations de performance

4. **Outils pratiques** :
   - Classes d'animation réutilisables
   - Gestionnaire d'animations
   - Compteur FPS et debugging

### Pour aller plus loin

#### Ressources complémentaires

- **Tutoriels vidéo** : YouTube - Game programming animations
- **Livres** : "The Animator's Survival Kit"
- **Cours en ligne** : Motion design et animation principles

#### Projets suggérés

1. **Menu animé** avec transitions fluides
2. **Jeu simple** avec sprites et particules
3. **Visualisation de données** animée
4. **Économiseur d'écran** avec formes procédurales
5. **Éditeur d'animation** basique

#### Bibliothèques à explorer

- **TAChart** : Graphiques animés (inclus Lazarus)
- **BGRAControls** : Composants avec animations
- **Castle Game Engine** : Moteur complet avec animations 3D

### Prochaines sections

Maintenant que vous maîtrisez les animations, vous pouvez explorer :
- **12.6 Docking et interfaces modulaires** : Interfaces professionnelles
- **12.7 Éditeurs et designers intégrés** : Outils avancés
- **12.10 Vision par ordinateur** : Analyse d'images animées

### Message final

Les animations ne sont pas juste "cosmétiques" - elles guident l'utilisateur, donnent du feedback, et rendent l'expérience intuitive. Une animation bien faite est invisible mais améliore considérablement l'expérience utilisateur.

**Rappelez-vous** : Commencez simple, testez régulièrement les performances, et ajoutez de la complexité progressivement. Les meilleures animations sont celles qui servent l'utilisateur sans le distraire.

Bon développement d'animations ! 🎬✨

⏭️ [Docking et interfaces modulaires](/12-interfaces-graphiques-avancees/06-docking-interfaces-modulaires.md)
