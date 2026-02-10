🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.5 Input devices et contrôleurs

## Introduction

Les périphériques d'entrée (input devices) sont essentiels pour l'interaction entre le joueur et le jeu. Un bon système de gestion des entrées rend le jeu agréable à jouer, tandis qu'un mauvais système peut ruiner l'expérience. Dans ce chapitre, nous allons explorer comment gérer efficacement le clavier, la souris, les manettes de jeu et d'autres périphériques avec FreePascal.

### Types de périphériques d'entrée

**Périphériques standards** :
- **Clavier** : Touches, combinaisons, saisie de texte
- **Souris** : Mouvements, clics, molette
- **Manettes/Gamepads** : Boutons, sticks analogiques, gâchettes
- **Joysticks** : Pour simulateurs et jeux d'arcade

**Périphériques spécialisés** :
- **Volants de course** : Force feedback, pédales
- **Joysticks de vol** : Throttle, rudder
- **Manettes arcade** : Boutons arcade, sticks
- **Écrans tactiles** : Touch, multitouch, gestes

### Bibliothèques disponibles pour FreePascal

| Bibliothèque | Clavier | Souris | Gamepad | Multiplateforme |
|--------------|---------|--------|---------|-----------------|
| **SDL2** | ✅ | ✅ | ✅ | ✅ Excellent |
| **ZenGL** | ✅ | ✅ | ✅ | ✅ Bon |
| **Castle Engine** | ✅ | ✅ | ✅ | ✅ Excellent |
| **Allegro** | ✅ | ✅ | ✅ | ✅ Bon |
| **LCL (Lazarus)** | ✅ | ✅ | ⚠️ Limité | ⚠️ Moyen |

## Gestion du clavier

### Concepts de base

Il existe deux types d'événements clavier :
- **Key Down** : Quand une touche est pressée (une seule fois)
- **Key Press** : Tant que la touche reste enfoncée (continu)
- **Key Up** : Quand une touche est relâchée

### Avec SDL2

```pascal
uses
  SDL2;

type
  TInputManager = class
  private
    FKeyStates: array[0..SDL_NUM_SCANCODES-1] of Boolean;
    FPreviousKeyStates: array[0..SDL_NUM_SCANCODES-1] of Boolean;
  public
    procedure Update;
    function KeyPressed(Key: TSDL_Scancode): Boolean;
    function KeyDown(Key: TSDL_Scancode): Boolean;
    function KeyUp(Key: TSDL_Scancode): Boolean;
  end;

procedure TInputManager.Update;  
var
  KeyboardState: PUInt8;
  I: Integer;
begin
  // Sauvegarder l'état précédent
  for I := 0 to SDL_NUM_SCANCODES - 1 do
    FPreviousKeyStates[I] := FKeyStates[I];

  // Obtenir l'état actuel
  KeyboardState := SDL_GetKeyboardState(nil);

  for I := 0 to SDL_NUM_SCANCODES - 1 do
    FKeyStates[I] := KeyboardState[I] = 1;
end;

function TInputManager.KeyPressed(Key: TSDL_Scancode): Boolean;  
begin
  // Vrai tant que la touche est enfoncée
  Result := FKeyStates[Ord(Key)];
end;

function TInputManager.KeyDown(Key: TSDL_Scancode): Boolean;  
begin
  // Vrai seulement au moment de l'appui
  Result := FKeyStates[Ord(Key)] and not FPreviousKeyStates[Ord(Key)];
end;

function TInputManager.KeyUp(Key: TSDL_Scancode): Boolean;  
begin
  // Vrai seulement au moment du relâchement
  Result := not FKeyStates[Ord(Key)] and FPreviousKeyStates[Ord(Key)];
end;

// Utilisation dans le jeu
var
  Input: TInputManager;

procedure Update(dt: Single);  
begin
  Input.Update;

  // Mouvement continu (KeyPressed)
  if Input.KeyPressed(SDL_SCANCODE_LEFT) then
    PlayerX := PlayerX - Speed * dt;

  if Input.KeyPressed(SDL_SCANCODE_RIGHT) then
    PlayerX := PlayerX + Speed * dt;

  // Action unique (KeyDown)
  if Input.KeyDown(SDL_SCANCODE_SPACE) then
    PlayerJump;

  // Détection de relâchement (KeyUp)
  if Input.KeyUp(SDL_SCANCODE_LSHIFT) then
    StopSprinting;
end;
```

### Codes de touches SDL2

```pascal
const
  // Lettres
  SDL_SCANCODE_A = 4;
  SDL_SCANCODE_B = 5;
  // ... jusqu'à Z

  // Chiffres
  SDL_SCANCODE_1 = 30;
  SDL_SCANCODE_2 = 31;
  // ... jusqu'à 0

  // Flèches
  SDL_SCANCODE_RIGHT = 79;
  SDL_SCANCODE_LEFT = 80;
  SDL_SCANCODE_DOWN = 81;
  SDL_SCANCODE_UP = 82;

  // Touches spéciales
  SDL_SCANCODE_SPACE = 44;
  SDL_SCANCODE_RETURN = 40;  // Entrée
  SDL_SCANCODE_ESCAPE = 41;
  SDL_SCANCODE_LSHIFT = 225;
  SDL_SCANCODE_RSHIFT = 229;
  SDL_SCANCODE_LCTRL = 224;
  SDL_SCANCODE_RCTRL = 228;
  SDL_SCANCODE_LALT = 226;
  SDL_SCANCODE_RALT = 230;
```

### Gestion des modificateurs

```pascal
function IsModifierPressed(Modifier: TSDL_Keymod): Boolean;  
var
  State: TSDL_Keymod;
begin
  State := SDL_GetModState;
  Result := (State and Modifier) <> 0;
end;

// Utilisation
if Input.KeyDown(SDL_SCANCODE_S) then  
begin
  if IsModifierPressed(KMOD_CTRL) then
    SaveGame  // Ctrl+S
  else
    MoveDown; // S seul
end;
```

### Saisie de texte

Pour les champs de texte (nom de joueur, chat, etc.) :

```pascal
var
  TextInput: string;
  TextInputActive: Boolean;

procedure HandleTextInput(Event: TSDL_Event);  
begin
  if Event.type_ = SDL_TEXTINPUT then
  begin
    // Ajouter le caractère saisi
    TextInput := TextInput + Event.text.text;
  end
  else if Event.type_ = SDL_KEYDOWN then
  begin
    case Event.key.keysym.sym of
      SDLK_BACKSPACE:
        if Length(TextInput) > 0 then
          Delete(TextInput, Length(TextInput), 1);

      SDLK_RETURN:
      begin
        ProcessTextInput(TextInput);
        TextInput := '';
      end;
    end;
  end;
end;

procedure ActivateTextInput;  
begin
  TextInputActive := True;
  SDL_StartTextInput;
end;

procedure DeactivateTextInput;  
begin
  TextInputActive := False;
  SDL_StopTextInput;
end;
```

## Gestion de la souris

### Position et mouvements

```pascal
type
  TMouseState = record
    X, Y: Integer;           // Position actuelle
    DeltaX, DeltaY: Integer; // Déplacement depuis la dernière frame
    PreviousX, PreviousY: Integer;
  end;

var
  Mouse: TMouseState;

procedure UpdateMouse;  
var
  ButtonState: UInt32;
begin
  // Sauvegarder position précédente
  Mouse.PreviousX := Mouse.X;
  Mouse.PreviousY := Mouse.Y;

  // Obtenir position actuelle
  ButtonState := SDL_GetMouseState(@Mouse.X, @Mouse.Y);

  // Calculer le déplacement
  Mouse.DeltaX := Mouse.X - Mouse.PreviousX;
  Mouse.DeltaY := Mouse.Y - Mouse.PreviousY;
end;

// Utilisation pour une caméra FPS
procedure UpdateCamera(dt: Single);  
const
  SENSITIVITY = 0.1;
begin
  CameraYaw := CameraYaw + Mouse.DeltaX * SENSITIVITY;
  CameraPitch := CameraPitch - Mouse.DeltaY * SENSITIVITY;

  // Limiter le pitch (éviter de regarder trop haut/bas)
  if CameraPitch > 89.0 then CameraPitch := 89.0;
  if CameraPitch < -89.0 then CameraPitch := -89.0;
end;
```

### Boutons de la souris

```pascal
type
  TMouseButtons = record
    Left, Middle, Right: Boolean;
    PrevLeft, PrevMiddle, PrevRight: Boolean;
  end;

var
  MouseButtons: TMouseButtons;

procedure UpdateMouseButtons;  
var
  State: UInt32;
begin
  // Sauvegarder état précédent
  MouseButtons.PrevLeft := MouseButtons.Left;
  MouseButtons.PrevMiddle := MouseButtons.Middle;
  MouseButtons.PrevRight := MouseButtons.Right;

  // Lire état actuel
  State := SDL_GetMouseState(nil, nil);

  MouseButtons.Left := (State and SDL_BUTTON_LMASK) <> 0;
  MouseButtons.Middle := (State and SDL_BUTTON_MMASK) <> 0;
  MouseButtons.Right := (State and SDL_BUTTON_RMASK) <> 0;
end;

function MouseButtonDown(Button: Integer): Boolean;  
begin
  case Button of
    SDL_BUTTON_LEFT:
      Result := MouseButtons.Left and not MouseButtons.PrevLeft;
    SDL_BUTTON_MIDDLE:
      Result := MouseButtons.Middle and not MouseButtons.PrevMiddle;
    SDL_BUTTON_RIGHT:
      Result := MouseButtons.Right and not MouseButtons.PrevRight;
    else
      Result := False;
  end;
end;

// Utilisation
if MouseButtonDown(SDL_BUTTON_LEFT) then  
begin
  // Tirer, sélectionner, etc.
  Fire;
end;
```

### Molette de la souris

```pascal
procedure HandleMouseWheel(Event: TSDL_Event);  
var
  WheelY: Integer;
begin
  if Event.type_ = SDL_MOUSEWHEEL then
  begin
    WheelY := Event.wheel.y;

    if WheelY > 0 then
    begin
      // Molette vers le haut
      ZoomIn;
    end
    else if WheelY < 0 then
    begin
      // Molette vers le bas
      ZoomOut;
    end;
  end;
end;
```

### Capture et verrouillage de la souris

Utile pour les jeux FPS :

```pascal
procedure LockMouse;  
begin
  // Cacher le curseur
  SDL_ShowCursor(SDL_DISABLE);

  // Capturer la souris (empêcher de sortir de la fenêtre)
  SDL_SetRelativeMouseMode(SDL_TRUE);
end;

procedure UnlockMouse;  
begin
  SDL_ShowCursor(SDL_ENABLE);
  SDL_SetRelativeMouseMode(SDL_FALSE);
end;

// En mode relatif, utilisez directement les deltas
procedure HandleRelativeMouseMotion(Event: TSDL_Event);  
begin
  if Event.type_ = SDL_MOUSEMOTION then
  begin
    CameraYaw := CameraYaw + Event.motion.xrel * SENSITIVITY;
    CameraPitch := CameraPitch - Event.motion.yrel * SENSITIVITY;
  end;
end;
```

### Curseur personnalisé

```pascal
var
  CustomCursor: PSDL_Cursor;

procedure LoadCustomCursor(const FileName: string);  
var
  Surface: PSDL_Surface;
begin
  Surface := IMG_Load(PChar(FileName));
  if Surface <> nil then
  begin
    CustomCursor := SDL_CreateColorCursor(Surface, 0, 0); // Hot spot en (0,0)
    SDL_SetCursor(CustomCursor);
    SDL_FreeSurface(Surface);
  end;
end;

// Curseur système
procedure SetSystemCursor(CursorType: TSDL_SystemCursor);  
var
  Cursor: PSDL_Cursor;
begin
  Cursor := SDL_CreateSystemCursor(CursorType);
  SDL_SetCursor(Cursor);
  // SDL_SYSTEM_CURSOR_ARROW, SDL_SYSTEM_CURSOR_HAND, etc.
end;
```

## Manettes et gamepads

### Initialisation des manettes avec SDL2

```pascal
uses
  SDL2;

type
  TGamepadManager = class
  private
    FControllers: array[0..3] of PSDL_GameController;
    FNumControllers: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Update;
    function GetController(Index: Integer): PSDL_GameController;
    function GetControllerCount: Integer;
  end;

constructor TGamepadManager.Create;  
var
  I: Integer;
begin
  // Initialiser le sous-système joystick/gamepad
  if SDL_Init(SDL_INIT_GAMECONTROLLER) < 0 then
  begin
    WriteLn('Erreur initialisation SDL GameController');
    Exit;
  end;

  FNumControllers := 0;

  // Détecter et ouvrir les manettes connectées
  for I := 0 to SDL_NumJoysticks - 1 do
  begin
    if SDL_IsGameController(I) then
    begin
      FControllers[FNumControllers] := SDL_GameControllerOpen(I);

      if FControllers[FNumControllers] <> nil then
      begin
        WriteLn('Manette connectée : ',
                SDL_GameControllerName(FControllers[FNumControllers]));
        Inc(FNumControllers);
      end;
    end;
  end;
end;

destructor TGamepadManager.Destroy;  
var
  I: Integer;
begin
  for I := 0 to FNumControllers - 1 do
  begin
    if FControllers[I] <> nil then
      SDL_GameControllerClose(FControllers[I]);
  end;

  inherited;
end;
```

### Lecture des boutons

```pascal
function IsButtonPressed(Controller: PSDL_GameController;
                        Button: TSDL_GameControllerButton): Boolean;
begin
  Result := SDL_GameControllerGetButton(Controller, Button) = 1;
end;

// Utilisation
var
  Gamepad: PSDL_GameController;

procedure Update(dt: Single);  
begin
  Gamepad := GamepadManager.GetController(0);

  if Gamepad <> nil then
  begin
    // Bouton A (Cross sur PlayStation)
    if IsButtonPressed(Gamepad, SDL_CONTROLLER_BUTTON_A) then
      PlayerJump;

    // Bouton B (Circle sur PlayStation)
    if IsButtonPressed(Gamepad, SDL_CONTROLLER_BUTTON_B) then
      PlayerRoll;

    // Bouton X (Square sur PlayStation)
    if IsButtonPressed(Gamepad, SDL_CONTROLLER_BUTTON_X) then
      Attack;

    // Bouton Y (Triangle sur PlayStation)
    if IsButtonPressed(Gamepad, SDL_CONTROLLER_BUTTON_Y) then
      Interact;

    // Gâchettes (bumpers)
    if IsButtonPressed(Gamepad, SDL_CONTROLLER_BUTTON_LEFTSHOULDER) then
      BlockLeft;

    if IsButtonPressed(Gamepad, SDL_CONTROLLER_BUTTON_RIGHTSHOULDER) then
      BlockRight;

    // Boutons Start et Select
    if IsButtonPressed(Gamepad, SDL_CONTROLLER_BUTTON_START) then
      PauseGame;

    if IsButtonPressed(Gamepad, SDL_CONTROLLER_BUTTON_BACK) then
      OpenInventory;
  end;
end;
```

### Sticks analogiques

```pascal
function GetAxisValue(Controller: PSDL_GameController;
                     Axis: TSDL_GameControllerAxis): Single;
var
  Value: Int16;
const
  DEADZONE = 8000; // Zone morte pour éviter le drift
begin
  Value := SDL_GameControllerGetAxis(Controller, Axis);

  // Appliquer la zone morte
  if Abs(Value) < DEADZONE then
    Value := 0;

  // Normaliser entre -1.0 et 1.0
  Result := Value / 32767.0;
end;

// Utilisation pour le mouvement
procedure UpdatePlayerMovement(dt: Single);  
var
  LeftStickX, LeftStickY: Single;
  RightStickX, RightStickY: Single;
const
  MOVE_SPEED = 5.0;
  LOOK_SPEED = 100.0;
begin
  if Gamepad <> nil then
  begin
    // Stick gauche : Mouvement
    LeftStickX := GetAxisValue(Gamepad, SDL_CONTROLLER_AXIS_LEFTX);
    LeftStickY := GetAxisValue(Gamepad, SDL_CONTROLLER_AXIS_LEFTY);

    PlayerVelocityX := LeftStickX * MOVE_SPEED;
    PlayerVelocityY := -LeftStickY * MOVE_SPEED; // Inverser Y

    // Stick droit : Caméra
    RightStickX := GetAxisValue(Gamepad, SDL_CONTROLLER_AXIS_RIGHTX);
    RightStickY := GetAxisValue(Gamepad, SDL_CONTROLLER_AXIS_RIGHTY);

    CameraYaw := CameraYaw + RightStickX * LOOK_SPEED * dt;
    CameraPitch := CameraPitch - RightStickY * LOOK_SPEED * dt;
  end;
end;
```

### Gâchettes analogiques (Triggers)

```pascal
function GetTriggerValue(Controller: PSDL_GameController;
                        Trigger: TSDL_GameControllerAxis): Single;
var
  Value: Int16;
begin
  Value := SDL_GameControllerGetAxis(Controller, Trigger);

  // Les triggers vont de 0 à 32767
  Result := Value / 32767.0;

  // Clamper entre 0 et 1
  if Result < 0 then Result := 0;
  if Result > 1 then Result := 1;
end;

// Utilisation
procedure UpdateVehicle(dt: Single);  
var
  Throttle, Brake: Single;
begin
  if Gamepad <> nil then
  begin
    // Gâchette droite : Accélération
    Throttle := GetTriggerValue(Gamepad, SDL_CONTROLLER_AXIS_TRIGGERRIGHT);
    VehicleSpeed := VehicleSpeed + Throttle * ACCELERATION * dt;

    // Gâchette gauche : Freinage
    Brake := GetTriggerValue(Gamepad, SDL_CONTROLLER_AXIS_TRIGGERLEFT);
    VehicleSpeed := VehicleSpeed - Brake * BRAKE_FORCE * dt;
  end;
end;
```

### Vibrations (Rumble)

```pascal
procedure Vibrate(Controller: PSDL_GameController;
                 LowFreq, HighFreq: UInt16; DurationMs: UInt32);
begin
  if Controller <> nil then
  begin
    SDL_GameControllerRumble(Controller, LowFreq, HighFreq, DurationMs);
  end;
end;

// Exemples d'utilisation
procedure OnPlayerHit;  
begin
  // Vibration forte et courte
  Vibrate(Gamepad, 65535, 65535, 200); // 200ms
end;

procedure OnCarCrash;  
begin
  // Vibration progressive
  Vibrate(Gamepad, 40000, 20000, 500); // 500ms
end;

procedure OnEngineRunning;  
begin
  // Vibration légère continue
  Vibrate(Gamepad, 5000, 3000, 100); // Répéter toutes les 100ms
end;
```

### Détection de connexion/déconnexion

```pascal
procedure HandleControllerEvents(Event: TSDL_Event);  
begin
  case Event.type_ of
    SDL_CONTROLLERDEVICEADDED:
    begin
      WriteLn('Manette connectée');
      // Ouvrir la nouvelle manette
      GamepadManager.AddController(Event.cdevice.which);
    end;

    SDL_CONTROLLERDEVICEREMOVED:
    begin
      WriteLn('Manette déconnectée');
      // Fermer la manette déconnectée
      GamepadManager.RemoveController(Event.cdevice.which);
    end;

    SDL_CONTROLLERBUTTONDOWN:
    begin
      WriteLn('Bouton pressé : ', Event.cbutton.button);
    end;

    SDL_CONTROLLERBUTTONUP:
    begin
      WriteLn('Bouton relâché : ', Event.cbutton.button);
    end;
  end;
end;
```

## Mapping des touches (Key Binding)

### Système de configuration des touches

```pascal
type
  TGameAction = (gaUp, gaDown, gaLeft, gaRight, gaJump, gaFire, gaPause);

  TInputBinding = record
    Keyboard: TSDL_Scancode;
    GamepadButton: TSDL_GameControllerButton;
    MouseButton: Integer;
  end;

  TInputConfig = class
  private
    FBindings: array[TGameAction] of TInputBinding;
  public
    constructor Create;
    procedure SetKeyboardBinding(Action: TGameAction; Key: TSDL_Scancode);
    procedure SetGamepadBinding(Action: TGameAction; Button: TSDL_GameControllerButton);
    function IsActionPressed(Action: TGameAction): Boolean;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
  end;

constructor TInputConfig.Create;  
begin
  // Bindings par défaut
  FBindings[gaUp].Keyboard := SDL_SCANCODE_W;
  FBindings[gaUp].GamepadButton := SDL_CONTROLLER_BUTTON_DPAD_UP;

  FBindings[gaDown].Keyboard := SDL_SCANCODE_S;
  FBindings[gaDown].GamepadButton := SDL_CONTROLLER_BUTTON_DPAD_DOWN;

  FBindings[gaLeft].Keyboard := SDL_SCANCODE_A;
  FBindings[gaLeft].GamepadButton := SDL_CONTROLLER_BUTTON_DPAD_LEFT;

  FBindings[gaRight].Keyboard := SDL_SCANCODE_D;
  FBindings[gaRight].GamepadButton := SDL_CONTROLLER_BUTTON_DPAD_RIGHT;

  FBindings[gaJump].Keyboard := SDL_SCANCODE_SPACE;
  FBindings[gaJump].GamepadButton := SDL_CONTROLLER_BUTTON_A;

  FBindings[gaFire].Keyboard := SDL_SCANCODE_LCTRL;
  FBindings[gaFire].GamepadButton := SDL_CONTROLLER_BUTTON_RIGHTSHOULDER;
  FBindings[gaFire].MouseButton := SDL_BUTTON_LEFT;

  FBindings[gaPause].Keyboard := SDL_SCANCODE_ESCAPE;
  FBindings[gaPause].GamepadButton := SDL_CONTROLLER_BUTTON_START;
end;

function TInputConfig.IsActionPressed(Action: TGameAction): Boolean;  
var
  Binding: TInputBinding;
begin
  Binding := FBindings[Action];
  Result := False;

  // Vérifier clavier
  if InputManager.KeyPressed(Binding.Keyboard) then
    Result := True;

  // Vérifier gamepad
  if (Gamepad <> nil) and
     IsButtonPressed(Gamepad, Binding.GamepadButton) then
    Result := True;

  // Vérifier souris
  if (Binding.MouseButton > 0) and
     MouseButtonPressed(Binding.MouseButton) then
    Result := True;
end;

// Utilisation
if InputConfig.IsActionPressed(gaJump) then
  PlayerJump;

if InputConfig.IsActionPressed(gaFire) then
  PlayerFire;
```

### Interface de reconfiguration

```pascal
type
  TKeyBindingUI = class
  private
    FWaitingForInput: Boolean;
    FCurrentAction: TGameAction;
  public
    procedure StartBinding(Action: TGameAction);
    procedure HandleInput(Event: TSDL_Event);
    procedure Draw;
  end;

procedure TKeyBindingUI.StartBinding(Action: TGameAction);  
begin
  FWaitingForInput := True;
  FCurrentAction := Action;
end;

procedure TKeyBindingUI.HandleInput(Event: TSDL_Event);  
begin
  if not FWaitingForInput then
    Exit;

  case Event.type_ of
    SDL_KEYDOWN:
    begin
      InputConfig.SetKeyboardBinding(FCurrentAction, Event.key.keysym.scancode);
      FWaitingForInput := False;
    end;

    SDL_CONTROLLERBUTTONDOWN:
    begin
      InputConfig.SetGamepadBinding(FCurrentAction,
        TSDL_GameControllerButton(Event.cbutton.button));
      FWaitingForInput := False;
    end;

    SDL_MOUSEBUTTONDOWN:
    begin
      InputConfig.SetMouseBinding(FCurrentAction, Event.button.button);
      FWaitingForInput := False;
    end;
  end;
end;

procedure TKeyBindingUI.Draw;  
begin
  DrawText(100, 100, 'Configuration des touches');
  DrawText(100, 150, 'Déplacement Haut : ' + GetKeyName(InputConfig.GetBinding(gaUp)));
  DrawText(100, 180, 'Déplacement Bas : ' + GetKeyName(InputConfig.GetBinding(gaDown)));

  if FWaitingForInput then
    DrawText(300, 300, 'Appuyez sur une touche...');
end;
```

## Gestion multi-joueurs locale

### Support de plusieurs manettes

```pascal
type
  TPlayer = class
  private
    FControllerIndex: Integer;
    FUseKeyboard: Boolean;
  public
    property ControllerIndex: Integer read FControllerIndex;
    property UseKeyboard: Boolean read FUseKeyboard;
  end;

procedure UpdateMultiplePlayers(dt: Single);  
var
  I: Integer;
  Player: TPlayer;
  Controller: PSDL_GameController;
  LeftX, LeftY: Single;
begin
  for I := 0 to Players.Count - 1 do
  begin
    Player := Players[I];

    if Player.UseKeyboard then
    begin
      // Joueur au clavier
      if InputManager.KeyPressed(SDL_SCANCODE_LEFT) then
        Player.X := Player.X - Player.Speed * dt;
      if InputManager.KeyPressed(SDL_SCANCODE_RIGHT) then
        Player.X := Player.X + Player.Speed * dt;
    end
    else
    begin
      // Joueur à la manette
      Controller := GamepadManager.GetController(Player.ControllerIndex);
      if Controller <> nil then
      begin
        LeftX := GetAxisValue(Controller, SDL_CONTROLLER_AXIS_LEFTX);
        LeftY := GetAxisValue(Controller, SDL_CONTROLLER_AXIS_LEFTY);

        Player.X := Player.X + LeftX * Player.Speed * dt;
        Player.Y := Player.Y - LeftY * Player.Speed * dt;
      end;
    end;
  end;
end;
```

## Touch et écrans tactiles

### Détection des touches

```pascal
type
  TTouch = record
    ID: Int64;
    X, Y: Single;
    Pressure: Single;
  end;

var
  ActiveTouches: array[0..9] of TTouch;
  TouchCount: Integer;

procedure HandleTouchEvent(Event: TSDL_Event);  
var
  I: Integer;
  Touch: TTouch;
begin
  case Event.type_ of
    SDL_FINGERDOWN:
    begin
      // Nouvelle touche
      Touch.ID := Event.tfinger.fingerId;
      Touch.X := Event.tfinger.x * ScreenWidth;
      Touch.Y := Event.tfinger.y * ScreenHeight;
      Touch.Pressure := Event.tfinger.pressure;

      ActiveTouches[TouchCount] := Touch;
      Inc(TouchCount);
    end;

    SDL_FINGERMOTION:
    begin
      // Mouvement de la touche
      for I := 0 to TouchCount - 1 do
      begin
        if ActiveTouches[I].ID = Event.tfinger.fingerId then
        begin
          ActiveTouches[I].X := Event.tfinger.x * ScreenWidth;
          ActiveTouches[I].Y := Event.tfinger.y * ScreenHeight;
          Break;
        end;
      end;
    end;

    SDL_FINGERUP:
    begin
      // Fin de la touche
      for I := 0 to TouchCount - 1 do
      begin
        if ActiveTouches[I].ID = Event.tfinger.fingerId then
        begin
          // Supprimer la touche
          ActiveTouches[I] := ActiveTouches[TouchCount - 1];
          Dec(TouchCount);
          Break;
        end;
      end;
    end;
  end;
end;
```

### Boutons virtuels pour mobile

```pascal
type
  TVirtualButton = record
    X, Y, Radius: Single;
    Pressed: Boolean;
    TouchID: Int64;
  end;

var
  JumpButton: TVirtualButton;

procedure UpdateVirtualButtons;  
var
  I: Integer;
  Touch: TTouch;
  Distance: Single;
begin
  JumpButton.Pressed := False;

  for I := 0 to TouchCount - 1 do
  begin
    Touch := ActiveTouches[I];

    // Calculer la distance entre la touche et le bouton
    Distance := Sqrt(Sqr(Touch.X - JumpButton.X) + Sqr(Touch.Y - JumpButton.Y));

    if Distance <= JumpButton.Radius then
    begin
      JumpButton.Pressed := True;
      JumpButton.TouchID := Touch.ID;
      Break;
    end;
  end;

  // Utilisation
  if JumpButton.Pressed then
    PlayerJump;
end;

procedure DrawVirtualButtons;  
var
  Alpha: Byte;
begin
  // Transparence selon si pressé ou non
  if JumpButton.Pressed then
    Alpha := 200
  else
    Alpha := 100;

  DrawCircle(JumpButton.X, JumpButton.Y, JumpButton.Radius,
             RGBA(255, 255, 255, Alpha));
  DrawText(JumpButton.X - 20, JumpButton.Y - 10, 'JUMP');
end;
```

### Joystick virtuel (Virtual D-Pad)

```pascal
type
  TVirtualJoystick = record
    CenterX, CenterY: Single;
    Radius: Single;
    CurrentX, CurrentY: Single;
    Active: Boolean;
    TouchID: Int64;
  end;

var
  VirtualStick: TVirtualJoystick;

procedure InitVirtualJoystick;  
begin
  VirtualStick.CenterX := 100;
  VirtualStick.CenterY := ScreenHeight - 100;
  VirtualStick.Radius := 60;
  VirtualStick.Active := False;
end;

procedure UpdateVirtualJoystick;  
var
  I: Integer;
  Touch: TTouch;
  Distance: Single;
begin
  VirtualStick.Active := False;

  for I := 0 to TouchCount - 1 do
  begin
    Touch := ActiveTouches[I];

    // Vérifier si la touche est dans la zone du joystick
    Distance := Sqrt(Sqr(Touch.X - VirtualStick.CenterX) +
                    Sqr(Touch.Y - VirtualStick.CenterY));

    if Distance <= VirtualStick.Radius * 2 then
    begin
      VirtualStick.Active := True;
      VirtualStick.TouchID := Touch.ID;

      // Position du stick (limité au rayon)
      if Distance > VirtualStick.Radius then
      begin
        // Normaliser et limiter au rayon
        VirtualStick.CurrentX := VirtualStick.CenterX +
          (Touch.X - VirtualStick.CenterX) / Distance * VirtualStick.Radius;
        VirtualStick.CurrentY := VirtualStick.CenterY +
          (Touch.Y - VirtualStick.CenterY) / Distance * VirtualStick.Radius;
      end
      else
      begin
        VirtualStick.CurrentX := Touch.X;
        VirtualStick.CurrentY := Touch.Y;
      end;

      Break;
    end;
  end;

  if not VirtualStick.Active then
  begin
    // Retour au centre
    VirtualStick.CurrentX := VirtualStick.CenterX;
    VirtualStick.CurrentY := VirtualStick.CenterY;
  end;
end;

function GetVirtualStickAxis: TVector2;  
var
  DeltaX, DeltaY, Distance: Single;
begin
  if VirtualStick.Active then
  begin
    DeltaX := VirtualStick.CurrentX - VirtualStick.CenterX;
    DeltaY := VirtualStick.CurrentY - VirtualStick.CenterY;

    // Normaliser
    Result.X := DeltaX / VirtualStick.Radius;
    Result.Y := DeltaY / VirtualStick.Radius;

    // Clamper entre -1 et 1
    if Result.X > 1 then Result.X := 1;
    if Result.X < -1 then Result.X := -1;
    if Result.Y > 1 then Result.Y := 1;
    if Result.Y < -1 then Result.Y := -1;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

procedure DrawVirtualJoystick;  
begin
  // Base du joystick
  DrawCircle(VirtualStick.CenterX, VirtualStick.CenterY,
            VirtualStick.Radius, RGBA(100, 100, 100, 100));

  // Stick mobile
  DrawCircle(VirtualStick.CurrentX, VirtualStick.CurrentY,
            VirtualStick.Radius * 0.4, RGBA(200, 200, 200, 150));
end;

// Utilisation pour le mouvement
procedure UpdatePlayerWithVirtualStick(dt: Single);  
var
  Axis: TVector2;
const
  SPEED = 200;
begin
  Axis := GetVirtualStickAxis;

  PlayerX := PlayerX + Axis.X * SPEED * dt;
  PlayerY := PlayerY + Axis.Y * SPEED * dt;
end;
```

### Gestes tactiles

```pascal
type
  TGestureType = (gtNone, gtTap, gtDoubleTap, gtSwipeLeft, gtSwipeRight,
                  gtSwipeUp, gtSwipeDown, gtPinch, gtRotate);

  TGestureDetector = class
  private
    FTouchStartX, FTouchStartY: Single;
    FTouchStartTime: Cardinal;
    FLastTapTime: Cardinal;
    FInitialDistance: Single;
  public
    function DetectGesture(Event: TSDL_Event): TGestureType;
  end;

function TGestureDetector.DetectGesture(Event: TSDL_Event): TGestureType;  
var
  DeltaX, DeltaY, Distance: Single;
  DeltaTime: Cardinal;
const
  SWIPE_THRESHOLD = 100;
  TAP_THRESHOLD = 20;
  DOUBLE_TAP_TIME = 300; // ms
begin
  Result := gtNone;

  case Event.type_ of
    SDL_FINGERDOWN:
    begin
      FTouchStartX := Event.tfinger.x * ScreenWidth;
      FTouchStartY := Event.tfinger.y * ScreenHeight;
      FTouchStartTime := SDL_GetTicks;
    end;

    SDL_FINGERUP:
    begin
      DeltaX := (Event.tfinger.x * ScreenWidth) - FTouchStartX;
      DeltaY := (Event.tfinger.y * ScreenHeight) - FTouchStartY;
      Distance := Sqrt(Sqr(DeltaX) + Sqr(DeltaY));
      DeltaTime := SDL_GetTicks - FTouchStartTime;

      // Tap ou Double Tap
      if (Distance < TAP_THRESHOLD) and (DeltaTime < 200) then
      begin
        if (SDL_GetTicks - FLastTapTime) < DOUBLE_TAP_TIME then
          Result := gtDoubleTap
        else
          Result := gtTap;

        FLastTapTime := SDL_GetTicks;
      end
      // Swipe
      else if Distance > SWIPE_THRESHOLD then
      begin
        // Déterminer la direction
        if Abs(DeltaX) > Abs(DeltaY) then
        begin
          if DeltaX > 0 then
            Result := gtSwipeRight
          else
            Result := gtSwipeLeft;
        end
        else
        begin
          if DeltaY > 0 then
            Result := gtSwipeDown
          else
            Result := gtSwipeUp;
        end;
      end;
    end;
  end;
end;

// Utilisation
var
  GestureDetector: TGestureDetector;

procedure HandleGestures;  
var
  Gesture: TGestureType;
begin
  Gesture := GestureDetector.DetectGesture(Event);

  case Gesture of
    gtTap:
      WriteLn('Simple tap détecté');
    gtDoubleTap:
      ZoomIn;
    gtSwipeLeft:
      NextPage;
    gtSwipeRight:
      PreviousPage;
    gtSwipeUp:
      ScrollUp;
    gtSwipeDown:
      ScrollDown;
  end;
end;
```

## Périphériques spécialisés

### Volant de course avec force feedback

```pascal
uses
  SDL2;

type
  TRacingWheel = class
  private
    FJoystick: PSDL_Joystick;
    FHaptic: PSDL_Haptic;
    FWheelAxis: Integer;
    FThrottleAxis: Integer;
    FBrakeAxis: Integer;
  public
    constructor Create(DeviceIndex: Integer);
    destructor Destroy; override;
    function GetSteeringAngle: Single;
    function GetThrottle: Single;
    function GetBrake: Single;
    procedure SetForceFeedback(Force: Single);
  end;

constructor TRacingWheel.Create(DeviceIndex: Integer);  
begin
  FJoystick := SDL_JoystickOpen(DeviceIndex);

  if FJoystick <> nil then
  begin
    WriteLn('Volant détecté : ', SDL_JoystickName(FJoystick));

    // Initialiser le force feedback
    if SDL_JoystickIsHaptic(FJoystick) then
    begin
      FHaptic := SDL_HapticOpenFromJoystick(FJoystick);

      if FHaptic <> nil then
      begin
        SDL_HapticRumbleInit(FHaptic);
        WriteLn('Force feedback disponible');
      end;
    end;
  end;

  // Configuration des axes (peut varier selon le volant)
  FWheelAxis := 0;      // Généralement axe 0
  FThrottleAxis := 2;   // Généralement axe 2
  FBrakeAxis := 3;      // Généralement axe 3
end;

function TRacingWheel.GetSteeringAngle: Single;  
var
  Value: Int16;
begin
  Value := SDL_JoystickGetAxis(FJoystick, FWheelAxis);
  Result := Value / 32767.0; // Normaliser entre -1.0 et 1.0
end;

function TRacingWheel.GetThrottle: Single;  
var
  Value: Int16;
begin
  Value := SDL_JoystickGetAxis(FJoystick, FThrottleAxis);
  Result := (Value + 32767) / 65534.0; // Normaliser entre 0.0 et 1.0
end;

function TRacingWheel.GetBrake: Single;  
var
  Value: Int16;
begin
  Value := SDL_JoystickGetAxis(FJoystick, FBrakeAxis);
  Result := (Value + 32767) / 65534.0; // Normaliser entre 0.0 et 1.0
end;

procedure TRacingWheel.SetForceFeedback(Force: Single);  
var
  Strength: Single;
begin
  if FHaptic <> nil then
  begin
    Strength := Abs(Force);
    if Strength > 1.0 then Strength := 1.0;

    SDL_HapticRumblePlay(FHaptic, Strength, 100); // 100ms
  end;
end;

// Utilisation dans un jeu de course
procedure UpdateRacingGame(dt: Single);  
var
  Steering, Throttle, Brake: Single;
  TireFriction: Single;
begin
  if Wheel <> nil then
  begin
    Steering := Wheel.GetSteeringAngle;
    Throttle := Wheel.GetThrottle;
    Brake := Wheel.GetBrake;

    // Appliquer à la voiture
    Car.SteeringAngle := Steering * MAX_STEERING_ANGLE;
    Car.EngineForce := Throttle * MAX_ENGINE_FORCE;
    Car.BrakeForce := Brake * MAX_BRAKE_FORCE;

    // Force feedback basée sur la friction des pneus
    TireFriction := CalculateTireFriction(Car);
    Wheel.SetForceFeedback(TireFriction * Steering);
  end;
end;
```

### Joystick de vol (HOTAS)

```pascal
type
  TFlightStick = class
  private
    FJoystick: PSDL_Joystick;
    FPitchAxis: Integer;
    FRollAxis: Integer;
    FYawAxis: Integer;
    FThrottleAxis: Integer;
  public
    constructor Create(DeviceIndex: Integer);
    function GetPitch: Single;
    function GetRoll: Single;
    function GetYaw: Single;
    function GetThrottle: Single;
    function GetHatDirection: Integer;
  end;

function TFlightStick.GetPitch: Single;  
var
  Value: Int16;
const
  DEADZONE = 500;
begin
  Value := SDL_JoystickGetAxis(FJoystick, FPitchAxis);

  if Abs(Value) < DEADZONE then
    Value := 0;

  Result := Value / 32767.0;
end;

function TFlightStick.GetHatDirection: Integer;  
begin
  // Hat switch (mini joystick directionnel)
  Result := SDL_JoystickGetHat(FJoystick, 0);

  // SDL_HAT_UP, SDL_HAT_DOWN, SDL_HAT_LEFT, SDL_HAT_RIGHT
  // SDL_HAT_RIGHTUP, SDL_HAT_RIGHTDOWN, SDL_HAT_LEFTUP, SDL_HAT_LEFTDOWN
end;

// Utilisation dans un simulateur de vol
procedure UpdateFlightSim(dt: Single);  
var
  Pitch, Roll, Yaw, Throttle: Single;
begin
  if FlightStick <> nil then
  begin
    Pitch := FlightStick.GetPitch;
    Roll := FlightStick.GetRoll;
    Yaw := FlightStick.GetYaw;
    Throttle := FlightStick.GetThrottle;

    // Appliquer aux surfaces de contrôle
    Aircraft.Elevator := Pitch * MAX_ELEVATOR;
    Aircraft.Aileron := Roll * MAX_AILERON;
    Aircraft.Rudder := Yaw * MAX_RUDDER;
    Aircraft.Throttle := Throttle;

    // Hat switch pour la vue
    case FlightStick.GetHatDirection of
      SDL_HAT_UP: CameraLookUp;
      SDL_HAT_DOWN: CameraLookDown;
      SDL_HAT_LEFT: CameraLookLeft;
      SDL_HAT_RIGHT: CameraLookRight;
    end;
  end;
end;
```

## Input buffering et replay

### Système de buffer d'entrées

Utile pour les combos et les replays :

```pascal
type
  TInputFrame = record
    Time: Cardinal;
    KeyboardState: array[0..255] of Boolean;
    MouseX, MouseY: Integer;
    MouseButtons: Byte;
    GamepadState: array[0..15] of Boolean;
  end;

  TInputBuffer = class
  private
    FFrames: array[0..599] of TInputFrame; // 10 secondes à 60 FPS
    FCurrentFrame: Integer;
    FRecording: Boolean;
  public
    procedure StartRecording;
    procedure StopRecording;
    procedure RecordFrame(const Frame: TInputFrame);
    function GetFrame(Index: Integer): TInputFrame;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
  end;

procedure TInputBuffer.RecordFrame(const Frame: TInputFrame);  
begin
  if FRecording then
  begin
    FFrames[FCurrentFrame] := Frame;
    FCurrentFrame := (FCurrentFrame + 1) mod Length(FFrames);
  end;
end;

procedure TInputBuffer.SaveToFile(const FileName: string);  
var
  F: File of TInputFrame;
  I: Integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);

  for I := 0 to Length(FFrames) - 1 do
    Write(F, FFrames[I]);

  CloseFile(F);
end;

// Utilisation pour un replay
type
  TReplayPlayer = class
  private
    FInputBuffer: TInputBuffer;
    FCurrentFrame: Integer;
    FPlaying: Boolean;
  public
    procedure StartReplay;
    procedure Update;
    function GetCurrentInput: TInputFrame;
  end;

procedure TReplayPlayer.Update;  
begin
  if FPlaying then
  begin
    // Avancer d'une frame
    Inc(FCurrentFrame);

    if FCurrentFrame >= 600 then
    begin
      FPlaying := False;
      FCurrentFrame := 0;
    end;
  end;
end;

function TReplayPlayer.GetCurrentInput: TInputFrame;  
begin
  Result := FInputBuffer.GetFrame(FCurrentFrame);
end;
```

### Détection de combos

Pour les jeux de combat :

```pascal
type
  TComboInput = array of TGameAction;

  TCombo = record
    Name: string;
    Inputs: TComboInput;
    TimeWindow: Cardinal; // Temps maximum entre chaque input (ms)
  end;

  TComboDetector = class
  private
    FCombos: array of TCombo;
    FInputHistory: array[0..19] of record
      Action: TGameAction;
      Time: Cardinal;
    end;
    FHistoryIndex: Integer;
  public
    procedure AddCombo(const Combo: TCombo);
    procedure RecordInput(Action: TGameAction);
    function CheckCombos: string; // Retourne le nom du combo détecté
  end;

procedure TComboDetector.RecordInput(Action: TGameAction);  
begin
  FInputHistory[FHistoryIndex].Action := Action;
  FInputHistory[FHistoryIndex].Time := SDL_GetTicks;
  FHistoryIndex := (FHistoryIndex + 1) mod Length(FInputHistory);
end;

function TComboDetector.CheckCombos: string;  
var
  I, J, K: Integer;
  Combo: TCombo;
  Match: Boolean;
  HistPos: Integer;
  TimeDiff: Cardinal;
begin
  Result := '';

  for I := 0 to High(FCombos) do
  begin
    Combo := FCombos[I];
    Match := True;

    // Vérifier si les derniers inputs correspondent au combo
    HistPos := (FHistoryIndex - 1 + Length(FInputHistory)) mod Length(FInputHistory);

    for J := High(Combo.Inputs) downto 0 do
    begin
      if FInputHistory[HistPos].Action <> Combo.Inputs[J] then
      begin
        Match := False;
        Break;
      end;

      // Vérifier le timing
      if J > 0 then
      begin
        K := (HistPos - 1 + Length(FInputHistory)) mod Length(FInputHistory);
        TimeDiff := FInputHistory[HistPos].Time - FInputHistory[K].Time;

        if TimeDiff > Combo.TimeWindow then
        begin
          Match := False;
          Break;
        end;

        HistPos := K;
      end;
    end;

    if Match then
    begin
      Result := Combo.Name;
      Break;
    end;
  end;
end;

// Configuration des combos
procedure SetupCombos;  
var
  Hadouken: TCombo;
begin
  // Hadouken : Bas, Bas-Avant, Avant, Poing
  Hadouken.Name := 'Hadouken';
  SetLength(Hadouken.Inputs, 4);
  Hadouken.Inputs[0] := gaDown;
  Hadouken.Inputs[1] := gaDownRight;
  Hadouken.Inputs[2] := gaRight;
  Hadouken.Inputs[3] := gaPunch;
  Hadouken.TimeWindow := 500; // 500ms max entre chaque input

  ComboDetector.AddCombo(Hadouken);
end;

// Utilisation
procedure Update(dt: Single);  
var
  ComboName: string;
begin
  if InputConfig.IsActionDown(gaRight) then
    ComboDetector.RecordInput(gaRight);

  if InputConfig.IsActionDown(gaPunch) then
  begin
    ComboDetector.RecordInput(gaPunch);

    ComboName := ComboDetector.CheckCombos;
    if ComboName <> '' then
    begin
      WriteLn('Combo exécuté : ', ComboName);
      ExecuteSpecialMove(ComboName);
    end;
  end;
end;
```

## Accessibilité

### Options d'accessibilité pour les contrôles

```pascal
type
  TAccessibilityOptions = record
    AutoAim: Boolean;
    ToggleCrouch: Boolean;    // Toggle au lieu de maintenir
    ButtonHoldTime: Cardinal; // Temps pour considérer un bouton comme "maintenu"
    ReducedInputRequirement: Boolean; // Simplifier les combos
    VibrationIntensity: Single; // 0.0 à 1.0
  end;

var
  Accessibility: TAccessibilityOptions;

procedure InitAccessibility;  
begin
  Accessibility.AutoAim := False;
  Accessibility.ToggleCrouch := False;
  Accessibility.ButtonHoldTime := 100;
  Accessibility.ReducedInputRequirement := False;
  Accessibility.VibrationIntensity := 1.0;
end;

// Auto-aim pour joueurs avec difficultés de visée
function GetAutoAimTarget: TEnemy;  
var
  I: Integer;
  Enemy: TEnemy;
  Distance, ClosestDistance: Single;
  Angle, AngleDiff: Single;
  BestTarget: TEnemy;
const
  AUTO_AIM_ANGLE = 30; // Degrés
  AUTO_AIM_RANGE = 20; // Mètres
begin
  Result := nil;

  if not Accessibility.AutoAim then
    Exit;

  ClosestDistance := AUTO_AIM_RANGE;
  BestTarget := nil;

  for I := 0 to Enemies.Count - 1 do
  begin
    Enemy := Enemies[I];
    Distance := GetDistance(Player.Position, Enemy.Position);

    if Distance < ClosestDistance then
    begin
      Angle := GetAngleTo(Player.Position, Enemy.Position);
      AngleDiff := Abs(Angle - Player.AimAngle);

      if AngleDiff < AUTO_AIM_ANGLE then
      begin
        ClosestDistance := Distance;
        BestTarget := Enemy;
      end;
    end;
  end;

  Result := BestTarget;
end;

// Toggle crouch (appuyer une fois pour s'accroupir, encore pour se lever)
var
  CrouchToggled: Boolean = False;

procedure HandleCrouchInput;  
begin
  if Accessibility.ToggleCrouch then
  begin
    // Mode toggle
    if InputConfig.IsActionDown(gaCrouch) then
      CrouchToggled := not CrouchToggled;

    Player.Crouching := CrouchToggled;
  end
  else
  begin
    // Mode normal (maintenir)
    Player.Crouching := InputConfig.IsActionPressed(gaCrouch);
  end;
end;
```

### Remapping pour joueurs mono-main

```pascal
type
  TOneHandedLayout = (ohlNone, ohlLeftHand, ohlRightHand);

var
  OneHandedMode: TOneHandedLayout;

procedure SetupOneHandedControls(Layout: TOneHandedLayout);  
begin
  case Layout of
    ohlLeftHand:
    begin
      // Tout sur la main gauche
      InputConfig.SetKeyboardBinding(gaUp, SDL_SCANCODE_W);
      InputConfig.SetKeyboardBinding(gaDown, SDL_SCANCODE_S);
      InputConfig.SetKeyboardBinding(gaLeft, SDL_SCANCODE_A);
      InputConfig.SetKeyboardBinding(gaRight, SDL_SCANCODE_D);
      InputConfig.SetKeyboardBinding(gaJump, SDL_SCANCODE_LSHIFT);
      InputConfig.SetKeyboardBinding(gaFire, SDL_SCANCODE_LCTRL);
      InputConfig.SetKeyboardBinding(gaReload, SDL_SCANCODE_R);
      InputConfig.SetKeyboardBinding(gaInteract, SDL_SCANCODE_E);
    end;

    ohlRightHand:
    begin
      // Tout sur la main droite
      InputConfig.SetKeyboardBinding(gaUp, SDL_SCANCODE_UP);
      InputConfig.SetKeyboardBinding(gaDown, SDL_SCANCODE_DOWN);
      InputConfig.SetKeyboardBinding(gaLeft, SDL_SCANCODE_LEFT);
      InputConfig.SetKeyboardBinding(gaRight, SDL_SCANCODE_RIGHT);
      InputConfig.SetKeyboardBinding(gaJump, SDL_SCANCODE_RSHIFT);
      InputConfig.SetKeyboardBinding(gaFire, SDL_SCANCODE_RCTRL);
      InputConfig.SetKeyboardBinding(gaReload, SDL_SCANCODE_KP_0);
      InputConfig.SetKeyboardBinding(gaInteract, SDL_SCANCODE_KP_ENTER);
    end;
  end;
end;
```

## Debug et visualisation des entrées

### Affichage de l'état des contrôles

```pascal
procedure DrawInputDebugInfo;  
var
  Y: Integer;
  I: Integer;
  Controller: PSDL_GameController;
  AxisValue: Single;
begin
  Y := 10;

  // Clavier
  DrawText(10, Y, 'Clavier:'); Inc(Y, 20);
  if InputManager.KeyPressed(SDL_SCANCODE_W) then DrawText(20, Y, 'W'); Inc(Y, 15);
  if InputManager.KeyPressed(SDL_SCANCODE_A) then DrawText(20, Y, 'A'); Inc(Y, 15);
  if InputManager.KeyPressed(SDL_SCANCODE_S) then DrawText(20, Y, 'S'); Inc(Y, 15);
  if InputManager.KeyPressed(SDL_SCANCODE_D) then DrawText(20, Y, 'D'); Inc(Y, 15);

  // Souris
  Inc(Y, 10);
  DrawText(10, Y, Format('Souris: X=%d Y=%d', [Mouse.X, Mouse.Y])); Inc(Y, 20);
  DrawText(20, Y, Format('Delta: X=%d Y=%d', [Mouse.DeltaX, Mouse.DeltaY])); Inc(Y, 20);

  // Manette
  Controller := GamepadManager.GetController(0);
  if Controller <> nil then
  begin
    Inc(Y, 10);
    DrawText(10, Y, 'Manette:'); Inc(Y, 20);

    // Axes
    for I := 0 to 5 do
    begin
      AxisValue := GetAxisValue(Controller, TSDL_GameControllerAxis(I));
      DrawText(20, Y, Format('Axe %d: %.2f', [I, AxisValue]));
      DrawBar(150, Y, 100, 10, AxisValue);
      Inc(Y, 15);
    end;

    // Boutons
    Inc(Y, 10);
    if IsButtonPressed(Controller, SDL_CONTROLLER_BUTTON_A) then
      DrawText(20, Y, 'A pressé');
    Inc(Y, 15);
  end;
end;

procedure DrawBar(X, Y, Width, Height: Integer; Value: Single);  
var
  BarWidth: Integer;
  Color: TColor;
begin
  // Convertir -1..1 en 0..Width
  BarWidth := Round((Value + 1.0) / 2.0 * Width);

  if Value > 0.1 then
    Color := clGreen
  else if Value < -0.1 then
    Color := clRed
  else
    Color := clGray;

  DrawRect(X, Y, Width, Height, clBlack);
  DrawRect(X, Y, BarWidth, Height, Color);
end;
```

## Optimisation et bonnes pratiques

### Pool d'événements

```pascal
type
  TEventPool = class
  private
    FEvents: array[0..99] of TSDL_Event;
    FEventCount: Integer;
  public
    procedure Clear;
    procedure AddEvent(const Event: TSDL_Event);
    function GetEventCount: Integer;
    function GetEvent(Index: Integer): TSDL_Event;
  end;

// Traiter tous les événements d'un coup
procedure ProcessAllEvents;  
var
  Event: TSDL_Event;
  EventPool: TEventPool;
  I: Integer;
begin
  EventPool := TEventPool.Create;

  // Collecter tous les événements
  while SDL_PollEvent(@Event) <> 0 do
    EventPool.AddEvent(Event);

  // Traiter par type
  for I := 0 to EventPool.GetEventCount - 1 do
  begin
    Event := EventPool.GetEvent(I);

    case Event.type_ of
      SDL_KEYDOWN: HandleKeyDown(Event);
      SDL_KEYUP: HandleKeyUp(Event);
      SDL_MOUSEMOTION: HandleMouseMotion(Event);
      SDL_MOUSEBUTTONDOWN: HandleMouseButtonDown(Event);
      SDL_MOUSEBUTTONUP: HandleMouseButtonUp(Event);
      SDL_CONTROLLERBUTTONDOWN: HandleControllerButtonDown(Event);
      SDL_CONTROLLERBUTTONUP: HandleControllerButtonUp(Event);
      SDL_CONTROLLERAXISMOTION: HandleControllerAxis(Event);
    end;
  end;

  EventPool.Free;
end;
```

### Limiter le polling rate

Pour économiser du CPU, surtout sur mobile :

```pascal
var
  LastInputPollTime: Cardinal;

const
  INPUT_POLL_INTERVAL = 8; // Milliseconds (125 Hz)

procedure UpdateInput;  
var
  CurrentTime: Cardinal;
begin
  CurrentTime := SDL_GetTicks;

  // Ne mettre à jour que tous les X ms
  if (CurrentTime - LastInputPollTime) >= INPUT_POLL_INTERVAL then
  begin
    ProcessAllEvents;
    InputManager.Update;
    LastInputPollTime := CurrentTime;
  end;
end;
```

### Prédiction d'entrée pour le réseau

Pour les jeux multijoueurs en ligne :

```pascal
type
  TInputPrediction = class
  private
    FPredictedInputs: array[0..9] of TInputFrame;
    FConfirmedFrame: Integer;
  public
    function PredictNextInput(const LastInput: TInputFrame): TInputFrame;
    procedure ConfirmInput(Frame: Integer; const ActualInput: TInputFrame);
    function NeedsRollback: Boolean;
  end;

function TInputPrediction.PredictNextInput(const LastInput: TInputFrame): TInputFrame;  
begin
  // Prédiction simple : répéter la dernière entrée
  Result := LastInput;

  // Ou prédiction plus sophistiquée basée sur l'historique
end;

procedure TInputPrediction.ConfirmInput(Frame: Integer;
                                       const ActualInput: TInputFrame);
begin
  // Vérifier si notre prédiction était correcte
  if not CompareInputs(FPredictedInputs[Frame mod 10], ActualInput) then
  begin
    // Rollback nécessaire !
    WriteLn('Prédiction incorrecte au frame ', Frame);
  end;
end;
```

## Tests et validation

### Système de test automatique des entrées

```pascal
type
  TInputTest = class
  private
    FTestSequence: array of TInputFrame;
    FCurrentStep: Integer;
  public
    procedure LoadTestSequence(const FileName: string);
    procedure RunTest;
    function Validate: Boolean;
  end;

procedure TInputTest.LoadTestSequence(const FileName: string);  
var
  F: File of TInputFrame;
  Frame: TInputFrame;
begin
  AssignFile(F, FileName);
  Reset(F);

  SetLength(FTestSequence, 0);

  while not Eof(F) do
  begin
    Read(F, Frame);
    SetLength(FTestSequence, Length(FTestSequence) + 1);
    FTestSequence[High(FTestSequence)] := Frame;
  end;

  CloseFile(F);
end;

procedure TInputTest.RunTest;  
begin
  FCurrentStep := 0;

  while FCurrentStep < Length(FTestSequence) do
  begin
    // Simuler l'entrée
    SimulateInput(FTestSequence[FCurrentStep]);

    // Mettre à jour le jeu
    GameUpdate(1.0 / 60.0);

    Inc(FCurrentStep);
  end;
end;

// Test unitaire pour les combos
procedure TestComboDetection;  
var
  Detector: TComboDetector;
  ComboName: string;
begin
  Detector := TComboDetector.Create;

  // Configurer un combo simple
  SetupTestCombo(Detector);

  // Simuler les inputs
  Detector.RecordInput(gaDown);
  SDL_Delay(100);
  Detector.RecordInput(gaRight);
  SDL_Delay(100);
  Detector.RecordInput(gaPunch);

  // Vérifier
  ComboName := Detector.CheckCombos;

  if ComboName = 'TestCombo' then
    WriteLn('Test réussi !')
  else
    WriteLn('Test échoué !');

  Detector.Free;
end;
```

### Enregistrement de sessions de jeu

Pour le debug et l'amélioration :

```pascal
type
  TGameSession = class
  private
    FStartTime: Cardinal;
    FInputs: TList<TInputFrame>;
    FGameStates: TList<TGameState>;
  public
    procedure StartRecording;
    procedure StopRecording;
    procedure RecordInput(const Input: TInputFrame);
    procedure SaveSession(const FileName: string);
    procedure GenerateHeatmap; // Zones où le joueur regarde/clique
  end;

procedure TGameSession.GenerateHeatmap;  
var
  Heatmap: array[0..SCREEN_WIDTH-1, 0..SCREEN_HEIGHT-1] of Integer;
  I: Integer;
  Input: TInputFrame;
begin
  // Initialiser
  FillChar(Heatmap, SizeOf(Heatmap), 0);

  // Compter les positions de la souris
  for I := 0 to FInputs.Count - 1 do
  begin
    Input := FInputs[I];
    if (Input.MouseX >= 0) and (Input.MouseX < SCREEN_WIDTH) and
       (Input.MouseY >= 0) and (Input.MouseY < SCREEN_HEIGHT) then
    begin
      Inc(Heatmap[Input.MouseX, Input.MouseY]);
    end;
  end;

  // Sauvegarder la heatmap
  SaveHeatmapImage('heatmap.png', Heatmap);
end;
```

## Compatibilité multiplateforme

### Différences Windows/Linux

```pascal
{$IFDEF WINDOWS}
procedure InitInputWindows;  
begin
  // Configuration spécifique Windows
  SDL_SetHint(SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING, '1');
end;
{$ENDIF}

{$IFDEF LINUX}
procedure InitInputLinux;  
begin
  // Configuration spécifique Linux
  SDL_SetHint(SDL_HINT_LINUX_JOYSTICK_DEADZONES, '1');
end;
{$ENDIF}

procedure InitInput;  
begin
  SDL_Init(SDL_INIT_EVERYTHING);

  {$IFDEF WINDOWS}
  InitInputWindows;
  {$ENDIF}

  {$IFDEF LINUX}
  InitInputLinux;
  {$ENDIF}
end;
```

### Détection automatique du type de contrôle

```pascal
type
  TControlScheme = (csKeyboardMouse, csGamepad, csTouch);

var
  CurrentScheme: TControlScheme;
  LastInputTime: array[TControlScheme] of Cardinal;

procedure DetectControlScheme;  
begin
  // Détecter automatiquement ce que le joueur utilise
  if SDL_GetTicks - LastInputTime[csGamepad] < 1000 then
    CurrentScheme := csGamepad
  else if SDL_GetTicks - LastInputTime[csTouch] < 1000 then
    CurrentScheme := csTouch
  else
    CurrentScheme := csKeyboardMouse;

  // Adapter l'UI en conséquence
  UpdateUIForScheme(CurrentScheme);
end;

procedure HandleEvent(Event: TSDL_Event);  
begin
  case Event.type_ of
    SDL_KEYDOWN, SDL_MOUSEMOTION:
      LastInputTime[csKeyboardMouse] := SDL_GetTicks;

    SDL_CONTROLLERBUTTONDOWN, SDL_CONTROLLERAXISMOTION:
      LastInputTime[csGamepad] := SDL_GetTicks;

    SDL_FINGERDOWN:
      LastInputTime[csTouch] := SDL_GetTicks;
  end;
end;

procedure UpdateUIForScheme(Scheme: TControlScheme);  
begin
  case Scheme of
    csKeyboardMouse:
    begin
      ShowMouseCursor;
      HideVirtualButtons;
      ShowKeyboardPrompts;
    end;

    csGamepad:
    begin
      HideMouseCursor;
      HideVirtualButtons;
      ShowGamepadPrompts;
    end;

    csTouch:
    begin
      HideMouseCursor;
      ShowVirtualButtons;
      ShowTouchPrompts;
    end;
  end;
end;
```

## Exemples d'architectures complètes

### Architecture MVC pour les entrées

```pascal
type
  // Model
  TInputModel = class
  private
    FKeyStates: array[0..255] of Boolean;
    FMousePosition: TPoint;
    FGamepadStates: array[0..3] of TGamepadState;
  public
    procedure Update;
    function IsKeyPressed(Key: Integer): Boolean;
    function GetMousePosition: TPoint;
    function GetGamepadState(Index: Integer): TGamepadState;
  end;

  // View
  TInputView = class
  public
    procedure ShowControlsHint(const Text: string);
    procedure DrawCursor(X, Y: Integer);
    procedure DrawVirtualControls;
  end;

  // Controller
  TInputController = class
  private
    FModel: TInputModel;
    FView: TInputView;
    FConfig: TInputConfig;
  public
    constructor Create(Model: TInputModel; View: TInputView);
    procedure HandleInput;
    procedure UpdateGameLogic;
  end;

constructor TInputController.Create(Model: TInputModel; View: TInputView);  
begin
  FModel := Model;
  FView := View;
  FConfig := TInputConfig.Create;
end;

procedure TInputController.HandleInput;  
begin
  FModel.Update;

  // Convertir les inputs en actions de jeu
  if FConfig.IsActionPressed(gaJump) then
    GamePlayer.Jump;

  if FConfig.IsActionPressed(gaFire) then
    GamePlayer.Fire;
end;
```

### System de plugins pour contrôles personnalisés

```pascal
type
  IInputPlugin = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function GetName: string;
    procedure Initialize;
    procedure Update;
    function GetAxisValue(const AxisName: string): Single;
    function IsButtonPressed(const ButtonName: string): Boolean;
  end;

  TInputPluginManager = class
  private
    FPlugins: TList<IInputPlugin>;
  public
    procedure RegisterPlugin(Plugin: IInputPlugin);
    function FindPlugin(const Name: string): IInputPlugin;
    procedure UpdateAll;
  end;

// Plugin exemple : Support Arduino
type
  TArduinoInputPlugin = class(TInterfacedObject, IInputPlugin)
  private
    FSerialPort: TSerialPort;
  public
    function GetName: string;
    procedure Initialize;
    procedure Update;
    function GetAxisValue(const AxisName: string): Single;
    function IsButtonPressed(const ButtonName: string): Boolean;
  end;

procedure TArduinoInputPlugin.Initialize;  
begin
  FSerialPort := TSerialPort.Create('COM3', 9600);
  FSerialPort.Open;
end;

procedure TArduinoInputPlugin.Update;  
var
  Data: string;
begin
  if FSerialPort.DataAvailable then
  begin
    Data := FSerialPort.ReadLine;
    ParseArduinoData(Data);
  end;
end;
```

## Conseils professionnels

### Liste de vérification (Checklist)

**Fonctionnalités essentielles** :
- [ ] Support clavier + souris
- [ ] Support gamepad (au moins Xbox/PlayStation)
- [ ] Remapping des touches
- [ ] Sauvegarde de la configuration
- [ ] Deadzone configurable pour les sticks
- [ ] Sensibilité réglable
- [ ] Vibration désactivable
- [ ] Support multi-joueurs local

**Accessibilité** :
- [ ] Options toggle pour actions continues
- [ ] Temps de maintien ajustable
- [ ] Contrôles mono-main possibles
- [ ] Prompts visuels clairs
- [ ] Test avec différents périphériques

**Polish** :
- [ ] Transition fluide entre contrôles
- [ ] Feedback visuel des entrées
- [ ] Gestion de déconnexion de manette
- [ ] Curseur personnalisé cohérent
- [ ] Tutoriel interactif

### Performance

**Optimisations clés** :
```pascal
// Éviter de traiter les événements dans plusieurs endroits
procedure BadPractice;  
begin
  while SDL_PollEvent(@Event) <> 0 do
    HandleEvent1(Event);

  while SDL_PollEvent(@Event) <> 0 do  // ❌ Mauvais !
    HandleEvent2(Event);
end;

// Centraliser le traitement
procedure GoodPractice;  
begin
  while SDL_PollEvent(@Event) <> 0 do
  begin
    HandleEvent1(Event);
    HandleEvent2(Event);
  end;
end;

// Utiliser des tables de lookup plutôt que des if multiples
type
  TActionHandler = procedure;

var
  ActionHandlers: array[TGameAction] of TActionHandler;

procedure InitActionHandlers;  
begin
  ActionHandlers[gaJump] := @PlayerJump;
  ActionHandlers[gaFire] := @PlayerFire;
  ActionHandlers[gaReload] := @PlayerReload;
end;

procedure ProcessAction(Action: TGameAction);  
begin
  if Assigned(ActionHandlers[Action]) then
    ActionHandlers[Action];
end;
```

### Débogage courant

**Problèmes fréquents et solutions** :

```pascal
// Problème : Drift des sticks analogiques
function FixStickDrift(Value: Single; Deadzone: Single): Single;  
begin
  if Abs(Value) < Deadzone then
    Result := 0
  else
  begin
    // Rescale pour garder la plage complète
    if Value > 0 then
      Result := (Value - Deadzone) / (1.0 - Deadzone)
    else
      Result := (Value + Deadzone) / (1.0 - Deadzone);
  end;
end;

// Problème : Input lag
var
  InputLatency: array[0..99] of Cardinal;
  LatencyIndex: Integer;

procedure MeasureInputLatency;  
var
  InputTime, RenderTime: Cardinal;
begin
  InputTime := SDL_GetTicks;

  // Traiter l'input...

  RenderTime := SDL_GetTicks;

  InputLatency[LatencyIndex] := RenderTime - InputTime;
  LatencyIndex := (LatencyIndex + 1) mod Length(InputLatency);

  // Si latence > 16ms, investiguer
  if (RenderTime - InputTime) > 16 then
    WriteLn('High input latency detected: ', RenderTime - InputTime, 'ms');
end;

// Problème : Événements perdus
type
  TSafeEventQueue = class
  private
    FQueue: TThreadList<TSDL_Event>;
  public
    procedure Push(const Event: TSDL_Event);
    function Pop(out Event: TSDL_Event): Boolean;
  end;
```

## Ressources et documentation

### Documentation officielle

**SDL2** :
- Site : https://www.libsdl.org/
- Wiki : https://wiki.libsdl.org/
- API Reference : https://wiki.libsdl.org/APIByCategory

**Autres bibliothèques** :
- ZenGL : http://zengl.org
- Castle Engine : https://castle-engine.io
- Allegro : https://liballeg.org/

### Tutoriels et exemples

**Tutoriels recommandés** :
- LazyFoo (SDL2) : https://lazyfoo.net/tutorials/SDL/
- TwinklebearDev SDL2 : https://www.willusher.io/pages/sdl2/
- Game Input Programming (général) : Game Programming Patterns

**Code source de jeux open source** :
- Doom (id Software) : Excellent exemple de gestion input
- Quake III Arena : Input système sophistiqué
- 0 A.D. : RTS avec support manette

### Communautés

- **Forum FreePascal** : freepascal.org/forum
- **Reddit** : r/gamedev, r/freepascal
- **Discord** : Serveurs dédiés au game dev
- **Stack Overflow** : Tag "sdl-2" et "input"

### Standards et bonnes pratiques

**Standards de l'industrie** :
- Xbox Design Lab : Guidelines pour contrôles
- PlayStation Developer : Best practices
- Nintendo Developer Portal : Input guidelines
- Steam Input : Documentation API

## Conclusion

La gestion des périphériques d'entrée est un aspect crucial du développement de jeux vidéo. Un bon système d'input rend le jeu agréable à jouer, tandis qu'un mauvais système peut ruiner l'expérience, même si le gameplay est excellent.

### Points clés à retenir

**Essentiels** :
1. **Flexibilité** : Supportez plusieurs types de contrôles (clavier, souris, gamepad)
2. **Remapping** : Laissez les joueurs configurer leurs touches
3. **Feedback** : Donnez un retour visuel/haptique aux actions
4. **Accessibilité** : Pensez aux joueurs avec des besoins spécifiques
5. **Tests** : Testez avec de vrais périphériques, pas seulement en émulation

**Recommandations techniques** :
- Utilisez SDL2 pour la compatibilité multiplateforme
- Implémentez un système de deadzone configurable
- Séparez la détection d'input de la logique de jeu
- Enregistrez les sessions pour le debug
- Centralisez la gestion des événements

**Architecture** :
```
Input Devices → Input Manager → Input Config → Game Actions → Game Logic
```

### Erreurs à éviter

❌ **Ne pas faire** :
- Hardcoder les touches sans possibilité de remapping
- Ignorer les gamepad (très populaires sur PC maintenant)
- Oublier de gérer la déconnexion de périphériques
- Traiter les événements de manière non-déterministe
- Ne pas tester sur différents OS

✅ **Faire** :
- Offrir des presets (WASD, AZERTY, Arrows, etc.)
- Détecter automatiquement le type de contrôle
- Afficher les bons prompts (boutons Xbox vs PlayStation)
- Implémenter un buffer d'input pour les combos
- Permettre plusieurs contrôles simultanés (coop locale)

### Exemple de configuration idéale

```pascal
type
  TIdealInputSystem = class
  public
    // Détection automatique
    property AutoDetectDevice: Boolean;

    // Configuration
    property Bindings: TInputBindings;
    property DeadzoneSize: Single;
    property Sensitivity: Single;
    property VibrationEnabled: Boolean;

    // Accessibilité
    property ToggleMode: Boolean;
    property HoldTime: Cardinal;
    property AutoAim: Boolean;

    // Debug
    property ShowInputDebug: Boolean;
    property RecordSession: Boolean;

    // Méthodes
    procedure LoadConfig(const FileName: string);
    procedure SaveConfig(const FileName: string);
    procedure ResetToDefaults;
    function IsActionActive(Action: TGameAction): Boolean;
  end;
```

### Prochaines étapes

Maintenant que vous maîtrisez la gestion des entrées, vous pouvez :

1. **Implémenter un système complet** dans votre jeu
2. **Ajouter le support de périphériques exotiques** (HOTAS, volant, etc.)
3. **Créer un système de replay** pour le debug et l'esport
4. **Optimiser pour différentes plateformes** (PC, console, mobile)
5. **Implémenter du machine learning** pour adapter la difficulté selon le skill

### Ressources finales

**Bibliothèques recommandées** :
- SDL2 : Le standard pour FreePascal
- ZenGL : Alternative légère
- Castle Engine : Si vous utilisez déjà ce moteur

**Outils utiles** :
- Gamepad Tester : Tester vos manettes
- Input Recorder : Enregistrer des macros
- AntiMicro : Mapper gamepad → clavier (pour tests)

**Livres** :
- "Game Programming Patterns" - Robert Nystrom
- "Game Engine Architecture" - Jason Gregory
- "The Art of Game Design" - Jesse Schell

La gestion des entrées peut sembler simple au premier abord, mais c'est un domaine profond avec de nombreuses subtilités. Prenez le temps de bien faire les choses, testez avec de vrais joueurs, et votre jeu n'en sera que meilleur !

**Bon développement et que vos contrôles soient toujours réactifs ! 🎮**

---

## Annexe : Code template complet

```pascal
unit GameInput;

{$mode objfpc}{$H+}

interface

uses
  SDL2, Classes, SysUtils;

type
  TGameAction = (gaUp, gaDown, gaLeft, gaRight, gaJump, gaFire, gaPause);

  TGameInput = class
  private
    FKeyStates: array[0..SDL_NUM_SCANCODES-1] of Boolean;
    FPrevKeyStates: array[0..SDL_NUM_SCANCODES-1] of Boolean;
    FController: PSDL_GameController;
    FMouseX, FMouseY: Integer;
    FConfig: TInputConfig;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Update;
    function IsActionPressed(Action: TGameAction): Boolean;
    function IsActionDown(Action: TGameAction): Boolean;
    function IsActionUp(Action: TGameAction): Boolean;
    procedure LoadConfig(const FileName: string);
    procedure SaveConfig(const FileName: string);
  end;

implementation

constructor TGameInput.Create;  
begin
  SDL_Init(SDL_INIT_GAMECONTROLLER);

  if SDL_NumJoysticks > 0 then
    FController := SDL_GameControllerOpen(0);

  FConfig := TInputConfig.Create;
end;

destructor TGameInput.Destroy;  
begin
  if FController <> nil then
    SDL_GameControllerClose(FController);

  FConfig.Free;
  inherited;
end;

procedure TGameInput.Update;  
var
  KeyboardState: PUInt8;
  I: Integer;
begin
  // Sauvegarder l'état précédent
  for I := 0 to SDL_NUM_SCANCODES - 1 do
    FPrevKeyStates[I] := FKeyStates[I];

  // Lire l'état actuel
  KeyboardState := SDL_GetKeyboardState(nil);
  for I := 0 to SDL_NUM_SCANCODES - 1 do
    FKeyStates[I] := KeyboardState[I] = 1;

  // Souris
  SDL_GetMouseState(@FMouseX, @FMouseY);
end;

function TGameInput.IsActionPressed(Action: TGameAction): Boolean;  
begin
  Result := FConfig.IsActionPressed(Action, Self);
end;

end.
```

Ce code template peut servir de base pour n'importe quel projet de jeu avec FreePascal ! 🚀

⏭️ [Réseaux pour jeux multijoueurs](/23-developpement-jeux/06-reseaux-jeux-multijoueurs.md)
