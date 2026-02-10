🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.6 Apprentissage par renforcement

## Introduction à l'Apprentissage par Renforcement

L'apprentissage par renforcement (Reinforcement Learning - RL) est une branche de l'intelligence artificielle où un **agent** apprend à prendre des décisions en **interagissant** avec son environnement. Contrairement à l'apprentissage supervisé où on donne les bonnes réponses, l'agent apprend par **essai-erreur** en recevant des récompenses ou des pénalités.

### L'analogie de l'enfant qui apprend à marcher

Imaginez un bébé qui apprend à marcher :
- Il **essaie** de faire un pas (action)
- Il **observe** ce qui se passe (état de l'environnement)
- Il reçoit une **récompense** (réussit à avancer) ou une **pénalité** (tombe)
- Il **apprend** progressivement quelles actions fonctionnent le mieux

C'est exactement le principe de l'apprentissage par renforcement !

### Concepts clés

**Agent** : Le "cerveau" qui prend les décisions
- Exemple : Un robot, un personnage de jeu vidéo, un programme de trading

**Environnement** : Le monde dans lequel l'agent évolue
- Exemple : Un labyrinthe, un jeu d'échecs, un marché financier

**État (State)** : La situation actuelle de l'environnement
- Exemple : Position dans un labyrinthe, configuration d'un échiquier

**Action** : Ce que l'agent peut faire
- Exemple : Bouger à gauche/droite/haut/bas, déplacer une pièce

**Récompense (Reward)** : Un signal numérique indiquant si l'action était bonne
- Exemple : +10 pour atteindre l'objectif, -1 pour chaque mouvement, -100 pour un mur

**Politique (Policy)** : La stratégie de l'agent pour choisir ses actions
- Exemple : "Si je suis proche du but, aller vers lui"

### Applications concrètes

L'apprentissage par renforcement est utilisé dans :

🎮 **Jeux vidéo**
- AlphaGo (bat les champions de Go)
- OpenAI Five (Dota 2)
- Personnages non-joueurs intelligents

🤖 **Robotique**
- Robots qui apprennent à marcher
- Bras robotiques pour l'assemblage
- Drones autonomes

💰 **Finance**
- Trading algorithmique
- Gestion de portefeuille
- Optimisation de prix

🚗 **Véhicules autonomes**
- Conduite autonome
- Parking automatique
- Navigation

🏭 **Industrie**
- Optimisation de chaînes de production
- Gestion d'énergie
- Maintenance prédictive

---

## Le Cycle d'Apprentissage par Renforcement

```
         ┌─────────────────────────────────┐
         │                                 │
         │          AGENT                  │
         │      (Cerveau/IA)               │
         │                                 │
         └──────────┬──────────────────────┘
                    │                    ▲
                    │ Action             │ État + Récompense
                    ▼                    │
         ┌─────────────────────────────────┐
         │                                 │
         │      ENVIRONNEMENT              │
         │      (Monde/Jeu)                │
         │                                 │
         └─────────────────────────────────┘

1. L'agent observe l'état actuel
2. L'agent choisit une action
3. L'environnement change d'état
4. L'agent reçoit une récompense
5. L'agent met à jour sa connaissance
6. Répéter jusqu'à l'objectif
```

### Exemple simple : Le labyrinthe

```
S = Start (Départ)  
G = Goal (Objectif)
# = Mur
. = Chemin libre

┌─────────────┐
│ S . . # . . │
│ # . # # . # │
│ . . . . . . │
│ # # . # # . │
│ . . . . . G │
└─────────────┘

Récompenses :
- Atteindre G : +100
- Chaque mouvement : -1
- Toucher un mur : -10
```

L'agent va explorer le labyrinthe, recevoir des récompenses, et progressivement apprendre le chemin optimal.

---

## Implémentation de Base en FreePascal

### Structure de données fondamentale

```pascal
unit ReinforcementLearning;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  // État de l'environnement
  TState = Integer;

  // Action possible
  TAction = Integer;

  // Expérience vécue par l'agent
  TExperience = record
    State: TState;
    Action: TAction;
    Reward: Double;
    NextState: TState;
    Done: Boolean;  // Episode terminé ?
  end;

  // Interface pour un environnement
  IEnvironment = interface
    function Reset: TState;
    function Step(AAction: TAction; out AReward: Double; out ADone: Boolean): TState;
    function GetStateCount: Integer;
    function GetActionCount: Integer;
    function GetValidActions(AState: TState): TArray<TAction>;
  end;

  // Agent d'apprentissage par renforcement
  TRLAgent = class
  protected
    FStateCount: Integer;
    FActionCount: Integer;
    FLearningRate: Double;      // Alpha (α)
    FDiscountFactor: Double;    // Gamma (γ)
    FEpsilon: Double;           // Pour exploration vs exploitation
    FEpisode: Integer;
  public
    constructor Create(AStateCount, AActionCount: Integer);

    function ChooseAction(AState: TState): TAction; virtual; abstract;
    procedure Learn(const AExperience: TExperience); virtual; abstract;
    procedure Reset; virtual;

    property LearningRate: Double read FLearningRate write FLearningRate;
    property DiscountFactor: Double read FDiscountFactor write FDiscountFactor;
    property Epsilon: Double read FEpsilon write FEpsilon;
    property Episode: Integer read FEpisode;
  end;

implementation

constructor TRLAgent.Create(AStateCount, AActionCount: Integer);  
begin
  FStateCount := AStateCount;
  FActionCount := AActionCount;

  // Paramètres par défaut
  FLearningRate := 0.1;      // Vitesse d'apprentissage
  FDiscountFactor := 0.95;   // Importance du futur
  FEpsilon := 0.1;           // 10% d'exploration
  FEpisode := 0;
end;

procedure TRLAgent.Reset;  
begin
  Inc(FEpisode);
end;

end.
```

---

## Q-Learning : L'Algorithme Fondamental

Q-Learning est l'un des algorithmes de RL les plus populaires. Il apprend une **table Q** qui estime la "qualité" de chaque action dans chaque état.

### Principe

La table Q stocke pour chaque paire (état, action) une valeur qui représente la récompense totale attendue en prenant cette action.

**Formule de mise à jour :**
```
Q(s, a) ← Q(s, a) + α × [r + γ × max Q(s', a') - Q(s, a)]
                                    a'

Où :
- s = état actuel
- a = action choisie
- r = récompense reçue
- s' = nouvel état
- α = taux d'apprentissage (learning rate)
- γ = facteur d'actualisation (discount factor)
```

### Implémentation

```pascal
unit QLearning;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, ReinforcementLearning;

type
  TQTable = array of array of Double;

  TQLearningAgent = class(TRLAgent)
  private
    FQTable: TQTable;

    function GetMaxQ(AState: TState): Double;
    function GetBestAction(AState: TState): TAction;
  public
    constructor Create(AStateCount, AActionCount: Integer);
    destructor Destroy; override;

    function ChooseAction(AState: TState): TAction; override;
    procedure Learn(const AExperience: TExperience); override;
    procedure Reset; override;

    function GetQValue(AState: TState; AAction: TAction): Double;
    procedure SaveQTable(const AFileName: string);
    procedure LoadQTable(const AFileName: string);
    procedure PrintQTable;
  end;

implementation

constructor TQLearningAgent.Create(AStateCount, AActionCount: Integer);  
var
  i, j: Integer;
begin
  inherited Create(AStateCount, AActionCount);

  // Initialiser la table Q à zéro
  SetLength(FQTable, FStateCount, FActionCount);

  for i := 0 to FStateCount - 1 do
    for j := 0 to FActionCount - 1 do
      FQTable[i, j] := 0;
end;

destructor TQLearningAgent.Destroy;  
begin
  SetLength(FQTable, 0, 0);
  inherited;
end;

function TQLearningAgent.GetMaxQ(AState: TState): Double;  
var
  i: Integer;
begin
  Result := FQTable[AState, 0];

  for i := 1 to FActionCount - 1 do
    if FQTable[AState, i] > Result then
      Result := FQTable[AState, i];
end;

function TQLearningAgent.GetBestAction(AState: TState): TAction;  
var
  i: Integer;
  maxQ: Double;
begin
  Result := 0;
  maxQ := FQTable[AState, 0];

  for i := 1 to FActionCount - 1 do
  begin
    if FQTable[AState, i] > maxQ then
    begin
      maxQ := FQTable[AState, i];
      Result := i;
    end;
  end;
end;

function TQLearningAgent.ChooseAction(AState: TState): TAction;  
begin
  // Stratégie ε-greedy (epsilon-greedy)
  if Random < FEpsilon then
    // Exploration : action aléatoire
    Result := Random(FActionCount)
  else
    // Exploitation : meilleure action connue
    Result := GetBestAction(AState);
end;

procedure TQLearningAgent.Learn(const AExperience: TExperience);  
var
  currentQ, maxNextQ, target, delta: Double;
begin
  // Q(s,a) actuel
  currentQ := FQTable[AExperience.State, AExperience.Action];

  if AExperience.Done then
    // Si l'épisode est terminé, pas de valeur future
    maxNextQ := 0
  else
    // Sinon, prendre la meilleure valeur Q du prochain état
    maxNextQ := GetMaxQ(AExperience.NextState);

  // Valeur cible
  target := AExperience.Reward + FDiscountFactor * maxNextQ;

  // Erreur temporelle (TD error)
  delta := target - currentQ;

  // Mise à jour de Q
  FQTable[AExperience.State, AExperience.Action] :=
    currentQ + FLearningRate * delta;
end;

procedure TQLearningAgent.Reset;  
begin
  inherited Reset;

  // Décroissance de l'exploration au fil du temps
  FEpsilon := Max(0.01, FEpsilon * 0.995);
end;

function TQLearningAgent.GetQValue(AState: TState; AAction: TAction): Double;  
begin
  Result := FQTable[AState, AAction];
end;

procedure TQLearningAgent.SaveQTable(const AFileName: string);  
var
  f: TextFile;
  i, j: Integer;
begin
  AssignFile(f, AFileName);
  try
    Rewrite(f);

    WriteLn(f, FStateCount);
    WriteLn(f, FActionCount);

    for i := 0 to FStateCount - 1 do
      for j := 0 to FActionCount - 1 do
        WriteLn(f, FQTable[i, j]:0:6);

  finally
    CloseFile(f);
  end;
end;

procedure TQLearningAgent.LoadQTable(const AFileName: string);  
var
  f: TextFile;
  i, j, stateCount, actionCount: Integer;
  value: Double;
begin
  AssignFile(f, AFileName);
  try
    Reset(f);

    ReadLn(f, stateCount);
    ReadLn(f, actionCount);

    if (stateCount <> FStateCount) or (actionCount <> FActionCount) then
      raise Exception.Create('Dimensions de la table Q incompatibles');

    for i := 0 to FStateCount - 1 do
      for j := 0 to FActionCount - 1 do
      begin
        ReadLn(f, value);
        FQTable[i, j] := value;
      end;

  finally
    CloseFile(f);
  end;
end;

procedure TQLearningAgent.PrintQTable;  
var
  i, j: Integer;
begin
  WriteLn('Table Q:');
  Write('État\Action ');

  for j := 0 to FActionCount - 1 do
    Write(Format('A%d      ', [j]));
  WriteLn;

  for i := 0 to FStateCount - 1 do
  begin
    Write(Format('S%-10d ', [i]));
    for j := 0 to FActionCount - 1 do
      Write(Format('%7.2f ', [FQTable[i, j]]));
    WriteLn;
  end;
end;

end.
```

---

## Premier Exemple : Le Labyrinthe Simple

### Environnement du labyrinthe

```pascal
unit GridWorld;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReinforcementLearning;

type
  TCell = (ctEmpty, ctWall, ctGoal);
  TGrid = array of array of TCell;

  TDirection = (dirUp, dirDown, dirLeft, dirRight);

  TGridWorld = class(TInterfacedObject, IEnvironment)
  private
    FGrid: TGrid;
    FWidth, FHeight: Integer;
    FAgentX, FAgentY: Integer;
    FGoalX, FGoalY: Integer;
    FStartX, FStartY: Integer;
    FStepCount: Integer;
    FMaxSteps: Integer;

    function PositionToState(AX, AY: Integer): TState;
    procedure StateToPosition(AState: TState; out AX, AY: Integer);
    function IsValidPosition(AX, AY: Integer): Boolean;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;

    procedure SetWall(AX, AY: Integer);
    procedure SetGoal(AX, AY: Integer);
    procedure SetStart(AX, AY: Integer);

    function Reset: TState;
    function Step(AAction: TAction; out AReward: Double; out ADone: Boolean): TState;
    function GetStateCount: Integer;
    function GetActionCount: Integer;
    function GetValidActions(AState: TState): TArray<TAction>;

    procedure Render;
    procedure RenderWithAgent(AState: TState);

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

implementation

constructor TGridWorld.Create(AWidth, AHeight: Integer);  
var
  x, y: Integer;
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FMaxSteps := FWidth * FHeight * 10;

  SetLength(FGrid, FWidth, FHeight);

  for x := 0 to FWidth - 1 do
    for y := 0 to FHeight - 1 do
      FGrid[x, y] := ctEmpty;

  // Position de départ par défaut
  FStartX := 0;
  FStartY := 0;

  // Objectif par défaut
  FGoalX := FWidth - 1;
  FGoalY := FHeight - 1;
end;

destructor TGridWorld.Destroy;  
begin
  SetLength(FGrid, 0, 0);
  inherited;
end;

function TGridWorld.PositionToState(AX, AY: Integer): TState;  
begin
  Result := AY * FWidth + AX;
end;

procedure TGridWorld.StateToPosition(AState: TState; out AX, AY: Integer);  
begin
  AX := AState mod FWidth;
  AY := AState div FWidth;
end;

function TGridWorld.IsValidPosition(AX, AY: Integer): Boolean;  
begin
  Result := (AX >= 0) and (AX < FWidth) and
            (AY >= 0) and (AY < FHeight) and
            (FGrid[AX, AY] <> ctWall);
end;

procedure TGridWorld.SetWall(AX, AY: Integer);  
begin
  if (AX >= 0) and (AX < FWidth) and (AY >= 0) and (AY < FHeight) then
    FGrid[AX, AY] := ctWall;
end;

procedure TGridWorld.SetGoal(AX, AY: Integer);  
begin
  FGoalX := AX;
  FGoalY := AY;
  FGrid[AX, AY] := ctGoal;
end;

procedure TGridWorld.SetStart(AX, AY: Integer);  
begin
  FStartX := AX;
  FStartY := AY;
end;

function TGridWorld.Reset: TState;  
begin
  FAgentX := FStartX;
  FAgentY := FStartY;
  FStepCount := 0;
  Result := PositionToState(FAgentX, FAgentY);
end;

function TGridWorld.Step(AAction: TAction; out AReward: Double; out ADone: Boolean): TState;  
var
  newX, newY: Integer;
begin
  Inc(FStepCount);
  newX := FAgentX;
  newY := FAgentY;

  // Déplacer selon l'action
  case TDirection(AAction) of
    dirUp:    Dec(newY);
    dirDown:  Inc(newY);
    dirLeft:  Dec(newX);
    dirRight: Inc(newX);
  end;

  // Vérifier si le mouvement est valide
  if IsValidPosition(newX, newY) then
  begin
    FAgentX := newX;
    FAgentY := newY;
    AReward := -1; // Pénalité pour chaque mouvement
  end
  else
  begin
    // Collision avec un mur ou sortie de la grille
    AReward := -10;
  end;

  // Vérifier si on a atteint l'objectif
  if (FAgentX = FGoalX) and (FAgentY = FGoalY) then
  begin
    AReward := 100;
    ADone := True;
  end
  else if FStepCount >= FMaxSteps then
  begin
    // Trop de pas, arrêter l'épisode
    ADone := True;
  end
  else
    ADone := False;

  Result := PositionToState(FAgentX, FAgentY);
end;

function TGridWorld.GetStateCount: Integer;  
begin
  Result := FWidth * FHeight;
end;

function TGridWorld.GetActionCount: Integer;  
begin
  Result := 4; // Haut, Bas, Gauche, Droite
end;

function TGridWorld.GetValidActions(AState: TState): TArray<TAction>;  
var
  x, y: Integer;
  actions: array of TAction;
  count: Integer;
begin
  StateToPosition(AState, x, y);
  SetLength(actions, 4);
  count := 0;

  // Vérifier chaque direction
  if IsValidPosition(x, y - 1) then
  begin
    actions[count] := Ord(dirUp);
    Inc(count);
  end;

  if IsValidPosition(x, y + 1) then
  begin
    actions[count] := Ord(dirDown);
    Inc(count);
  end;

  if IsValidPosition(x - 1, y) then
  begin
    actions[count] := Ord(dirLeft);
    Inc(count);
  end;

  if IsValidPosition(x + 1, y) then
  begin
    actions[count] := Ord(dirRight);
    Inc(count);
  end;

  SetLength(actions, count);
  Result := actions;
end;

procedure TGridWorld.Render;  
var
  x, y: Integer;
begin
  WriteLn('┌', StringOfChar('─', FWidth * 2), '┐');

  for y := 0 to FHeight - 1 do
  begin
    Write('│');
    for x := 0 to FWidth - 1 do
    begin
      case FGrid[x, y] of
        ctEmpty: Write('. ');
        ctWall:  Write('# ');
        ctGoal:  Write('G ');
      end;
    end;
    WriteLn('│');
  end;

  WriteLn('└', StringOfChar('─', FWidth * 2), '┘');
end;

procedure TGridWorld.RenderWithAgent(AState: TState);  
var
  x, y, agentX, agentY: Integer;
begin
  StateToPosition(AState, agentX, agentY);

  WriteLn('┌', StringOfChar('─', FWidth * 2), '┐');

  for y := 0 to FHeight - 1 do
  begin
    Write('│');
    for x := 0 to FWidth - 1 do
    begin
      if (x = agentX) and (y = agentY) then
        Write('A ')  // Agent
      else
      begin
        case FGrid[x, y] of
          ctEmpty: Write('. ');
          ctWall:  Write('# ');
          ctGoal:  Write('G ');
        end;
      end;
    end;
    WriteLn('│');
  end;

  WriteLn('└', StringOfChar('─', FWidth * 2), '┘');
end;

end.
```

### Programme d'entraînement

```pascal
program TrainMaze;

{$mode objfpc}{$H+}

uses
  SysUtils, ReinforcementLearning, QLearning, GridWorld;

procedure TrainAgent(AAgent: TQLearningAgent; AEnv: TGridWorld; AEpisodes: Integer);  
var
  episode, step: Integer;
  state, nextState: TState;
  action: TAction;
  reward, totalReward: Double;
  done: Boolean;
  experience: TExperience;
  successCount: Integer;
begin
  WriteLn('===================================');
  WriteLn('  Entraînement de l''agent         ');
  WriteLn('===================================');
  WriteLn;

  successCount := 0;

  for episode := 1 to AEpisodes do
  begin
    state := AEnv.Reset;
    AAgent.Reset;
    totalReward := 0;
    step := 0;

    repeat
      // Choisir une action
      action := AAgent.ChooseAction(state);

      // Exécuter l'action
      nextState := AEnv.Step(action, reward, done);

      // Créer l'expérience
      experience.State := state;
      experience.Action := action;
      experience.Reward := reward;
      experience.NextState := nextState;
      experience.Done := done;

      // Apprendre
      AAgent.Learn(experience);

      totalReward := totalReward + reward;
      state := nextState;
      Inc(step);

    until done;

    if reward = 100 then
      Inc(successCount);

    // Afficher la progression
    if episode mod 100 = 0 then
      WriteLn(Format('Épisode %4d | Récompense: %6.1f | Steps: %3d | Succès: %3d%% | Epsilon: %.3f',
        [episode, totalReward, step, (successCount * 100) div episode, AAgent.Epsilon]));
  end;

  WriteLn;
  WriteLn('Entraînement terminé!');
  WriteLn(Format('Taux de succès final: %d%%', [(successCount * 100) div AEpisodes]));
end;

procedure TestAgent(AAgent: TQLearningAgent; AEnv: TGridWorld);  
var
  state: TState;
  action: TAction;
  reward: Double;
  done: Boolean;
  step: Integer;
const
  ActionNames: array[0..3] of string = ('Haut', 'Bas', 'Gauche', 'Droite');
begin
  WriteLn;
  WriteLn('===================================');
  WriteLn('  Test de l''agent entraîné        ');
  WriteLn('===================================');
  WriteLn;

  // Désactiver l'exploration pour le test
  AAgent.Epsilon := 0;

  state := AEnv.Reset;
  step := 0;

  AEnv.RenderWithAgent(state);
  WriteLn;

  repeat
    action := AAgent.ChooseAction(state);
    WriteLn(Format('Step %d: Action = %s', [step + 1, ActionNames[action]]));

    state := AEnv.Step(action, reward, done);
    Inc(step);

    AEnv.RenderWithAgent(state);
    WriteLn(Format('Récompense: %.1f', [reward]));
    WriteLn;

    {$IFDEF WINDOWS}
    if step < 20 then // Limiter l'affichage
      ReadLn;
    {$ENDIF}

  until done or (step >= 100);

  if reward = 100 then
    WriteLn('✓ Objectif atteint!')
  else
    WriteLn('✗ Échec');
end;

var
  gridWorld: TGridWorld;
  agent: TQLearningAgent;

begin
  WriteLn('========================================');
  WriteLn('  Apprentissage par Renforcement       ');
  WriteLn('  Q-Learning dans un labyrinthe        ');
  WriteLn('========================================');
  WriteLn;

  Randomize;

  // Créer l'environnement (grille 5x5)
  gridWorld := TGridWorld.Create(5, 5);
  try
    // Définir le labyrinthe
    gridWorld.SetStart(0, 0);
    gridWorld.SetGoal(4, 4);

    gridWorld.SetWall(1, 0);
    gridWorld.SetWall(1, 1);
    gridWorld.SetWall(1, 2);
    gridWorld.SetWall(3, 1);
    gridWorld.SetWall(3, 2);
    gridWorld.SetWall(3, 3);

    WriteLn('Labyrinthe:');
    WriteLn('S = Start, G = Goal, # = Mur');
    WriteLn;
    gridWorld.Render;
    WriteLn;

    // Créer l'agent
    agent := TQLearningAgent.Create(
      gridWorld.GetStateCount,
      gridWorld.GetActionCount
    );
    try
      agent.LearningRate := 0.1;
      agent.DiscountFactor := 0.95;
      agent.Epsilon := 0.3;

      // Entraîner
      TrainAgent(agent, gridWorld, 1000);

      // Sauvegarder
      agent.SaveQTable('maze_qtable.txt');
      WriteLn('Table Q sauvegardée dans maze_qtable.txt');

      // Tester
      TestAgent(agent, gridWorld);

    finally
      agent.Free;
    end;
  finally
    gridWorld.Free;
  end;

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Exploration vs Exploitation

Un dilemme fondamental en RL : faut-il **explorer** de nouvelles actions ou **exploiter** ce qu'on sait déjà ?

### Stratégie ε-greedy (Epsilon-Greedy)

C'est la stratégie la plus simple et courante :

```pascal
function EpsilonGreedyAction(AState: TState; AEpsilon: Double): TAction;  
begin
  if Random < AEpsilon then
    // Exploration : action aléatoire
    Result := Random(FActionCount)
  else
    // Exploitation : meilleure action connue
    Result := GetBestAction(AState);
end;
```

**Décroissance de ε au fil du temps** :

```pascal
// Commencer avec beaucoup d'exploration
FEpsilon := 1.0;

// À chaque épisode, réduire l'exploration
procedure UpdateEpsilon;  
begin
  // Décroissance exponentielle
  FEpsilon := FEpsilon * 0.995;

  // Minimum de 1% d'exploration
  if FEpsilon < 0.01 then
    FEpsilon := 0.01;
end;
```

### Autres stratégies d'exploration

**Softmax / Boltzmann**
```pascal
function SoftmaxAction(AState: TState; ATemperature: Double): TAction;  
var
  i: Integer;
  probabilities: array of Double;
  sumExp, randomValue, cumulative: Double;
begin
  SetLength(probabilities, FActionCount);
  sumExp := 0;

  // Calculer e^(Q/T) pour chaque action
  for i := 0 to FActionCount - 1 do
  begin
    probabilities[i] := Exp(FQTable[AState, i] / ATemperature);
    sumExp := sumExp + probabilities[i];
  end;

  // Normaliser pour obtenir des probabilités
  for i := 0 to FActionCount - 1 do
    probabilities[i] := probabilities[i] / sumExp;

  // Sélectionner une action selon ces probabilités
  randomValue := Random;
  cumulative := 0;
  Result := 0;

  for i := 0 to FActionCount - 1 do
  begin
    cumulative := cumulative + probabilities[i];
    if randomValue <= cumulative then
    begin
      Result := i;
      Break;
    end;
  end;

  SetLength(probabilities, 0);
end;
```

**Température** :
- Haute température → plus d'exploration (actions équiprobables)
- Basse température → plus d'exploitation (favorise la meilleure action)

### Stratégie UCB (Upper Confidence Bound)

Explore les actions peu essayées :

```pascal
type
  TUCBAgent = class(TQLearningAgent)
  private
    FActionCounts: array of array of Integer;
    FTotalSteps: Integer;
    FC: Double; // Constante d'exploration
  public
    constructor Create(AStateCount, AActionCount: Integer);
    function ChooseAction(AState: TState): TAction; override;
  end;

function TUCBAgent.ChooseAction(AState: TState): TAction;  
var
  i: Integer;
  bestValue, value, exploration: Double;
begin
  Inc(FTotalSteps);
  Result := 0;
  bestValue := -Infinity;

  for i := 0 to FActionCount - 1 do
  begin
    if FActionCounts[AState, i] = 0 then
    begin
      // Action jamais essayée → priorité maximale
      Result := i;
      Exit;
    end;

    // UCB : Q(s,a) + C * sqrt(ln(t) / N(s,a))
    exploration := FC * Sqrt(Ln(FTotalSteps) / FActionCounts[AState, i]);
    value := FQTable[AState, i] + exploration;

    if value > bestValue then
    begin
      bestValue := value;
      Result := i;
    end;
  end;

  Inc(FActionCounts[AState, Result]);
end;
```

---

## SARSA : Alternative à Q-Learning

SARSA (State-Action-Reward-State-Action) est similaire à Q-Learning mais met à jour différemment.

### Différence clé

**Q-Learning** (off-policy) :
```
Q(s,a) ← Q(s,a) + α[r + γ max Q(s',a') - Q(s,a)]
                           a'
```

**SARSA** (on-policy) :
```
Q(s,a) ← Q(s,a) + α[r + γ Q(s',a') - Q(s,a)]
```

SARSA utilise l'action **réellement choisie** au prochain état, pas la meilleure.

### Implémentation

```pascal
unit SARSAAgent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, ReinforcementLearning;

type
  TSARSAAgent = class(TRLAgent)
  private
    FQTable: array of array of Double;
    FNextAction: TAction;

    function GetBestAction(AState: TState): TAction;
  public
    constructor Create(AStateCount, AActionCount: Integer);
    destructor Destroy; override;

    function ChooseAction(AState: TState): TAction; override;
    procedure Learn(const AExperience: TExperience); override;
    function GetQValue(AState: TState; AAction: TAction): Double;
  end;

implementation

constructor TSARSAAgent.Create(AStateCount, AActionCount: Integer);  
var
  i, j: Integer;
begin
  inherited Create(AStateCount, AActionCount);

  SetLength(FQTable, FStateCount, FActionCount);

  for i := 0 to FStateCount - 1 do
    for j := 0 to FActionCount - 1 do
      FQTable[i, j] := 0;

  FNextAction := -1;
end;

destructor TSARSAAgent.Destroy;  
begin
  SetLength(FQTable, 0, 0);
  inherited;
end;

function TSARSAAgent.GetBestAction(AState: TState): TAction;  
var
  i: Integer;
  maxQ: Double;
begin
  Result := 0;
  maxQ := FQTable[AState, 0];

  for i := 1 to FActionCount - 1 do
  begin
    if FQTable[AState, i] > maxQ then
    begin
      maxQ := FQTable[AState, i];
      Result := i;
    end;
  end;
end;

function TSARSAAgent.ChooseAction(AState: TState): TAction;  
begin
  if Random < FEpsilon then
    Result := Random(FActionCount)
  else
    Result := GetBestAction(AState);

  FNextAction := Result;
end;

procedure TSARSAAgent.Learn(const AExperience: TExperience);  
var
  currentQ, nextQ, target, delta: Double;
  nextAction: TAction;
begin
  currentQ := FQTable[AExperience.State, AExperience.Action];

  if AExperience.Done then
    nextQ := 0
  else
  begin
    // Choisir la prochaine action (pour SARSA)
    nextAction := ChooseAction(AExperience.NextState);
    nextQ := FQTable[AExperience.NextState, nextAction];
  end;

  target := AExperience.Reward + FDiscountFactor * nextQ;
  delta := target - currentQ;

  FQTable[AExperience.State, AExperience.Action] :=
    currentQ + FLearningRate * delta;
end;

function TSARSAAgent.GetQValue(AState: TState; AAction: TAction): Double;  
begin
  Result := FQTable[AState, AAction];
end;

end.
```

**Q-Learning vs SARSA** :
- Q-Learning apprend la politique **optimale** (risquée)
- SARSA apprend la politique qu'il **suit réellement** (prudente)

---

## Deep Q-Network (DQN) - Concepts

Pour les environnements avec trop d'états pour une table Q (jeux vidéo, robotique), on utilise un **réseau de neurones** pour approximer Q.

### Architecture

```
État (pixels, capteurs)
         ↓
    [Réseau de Neurones]
         ↓
Q(s, action1), Q(s, action2), ...
```

### Améliorations clés du DQN

**1. Experience Replay**

Stocker les expériences et apprendre sur des échantillons aléatoires :

```pascal
unit ExperienceReplay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReinforcementLearning, Generics.Collections;

type
  TReplayBuffer = class
  private
    FBuffer: specialize TList<TExperience>;
    FMaxSize: Integer;
  public
    constructor Create(AMaxSize: Integer);
    destructor Destroy; override;

    procedure Add(const AExperience: TExperience);
    function Sample(ABatchSize: Integer): specialize TList<TExperience>;
    function Count: Integer;
  end;

implementation

constructor TReplayBuffer.Create(AMaxSize: Integer);  
begin
  FMaxSize := AMaxSize;
  FBuffer := specialize TList<TExperience>.Create;
end;

destructor TReplayBuffer.Destroy;  
begin
  FBuffer.Free;
  inherited;
end;

procedure TReplayBuffer.Add(const AExperience: TExperience);  
begin
  if FBuffer.Count >= FMaxSize then
    FBuffer.Delete(0); // Supprimer la plus ancienne

  FBuffer.Add(AExperience);
end;

function TReplayBuffer.Sample(ABatchSize: Integer): specialize TList<TExperience>;  
var
  i, index: Integer;
begin
  Result := specialize TList<TExperience>.Create;

  for i := 1 to Min(ABatchSize, FBuffer.Count) do
  begin
    index := Random(FBuffer.Count);
    Result.Add(FBuffer[index]);
  end;
end;

function TReplayBuffer.Count: Integer;  
begin
  Result := FBuffer.Count;
end;

end.
```

**2. Target Network**

Utiliser un réseau séparé pour les valeurs cibles (stabilité) :

```pascal
type
  TDQNAgent = class
  private
    FQNetwork: TNeuralNetwork;      // Réseau principal
    FTargetNetwork: TNeuralNetwork; // Réseau cible
    FReplayBuffer: TReplayBuffer;
    FUpdateFrequency: Integer;
    FStepCount: Integer;

    procedure UpdateTargetNetwork;
  public
    constructor Create;
    procedure Learn;
  end;

procedure TDQNAgent.UpdateTargetNetwork;  
begin
  // Copier les poids du réseau principal vers le réseau cible
  FTargetNetwork.CopyWeightsFrom(FQNetwork);
end;

procedure TDQNAgent.Learn;  
var
  batch: TList<TExperience>;
  exp: TExperience;
  target: Double;
begin
  if FReplayBuffer.Count < 32 then
    Exit; // Pas assez d'expériences

  // Échantillonner un batch
  batch := FReplayBuffer.Sample(32);
  try
    for exp in batch do
    begin
      // Calculer la cible avec le réseau cible
      if exp.Done then
        target := exp.Reward
      else
        target := exp.Reward +
                  FDiscountFactor * FTargetNetwork.GetMaxQ(exp.NextState);

      // Entraîner le réseau principal
      FQNetwork.Train(exp.State, exp.Action, target);
    end;
  finally
    batch.Free;
  end;

  Inc(FStepCount);

  // Mettre à jour le réseau cible périodiquement
  if FStepCount mod FUpdateFrequency = 0 then
    UpdateTargetNetwork;
end;
```

---

## Policy Gradient - Approche Différente

Au lieu d'apprendre Q, apprendre directement la **politique** (probabilité de chaque action).

### REINFORCE Algorithm

```pascal
unit PolicyGradient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, ReinforcementLearning;

type
  TTrajectory = record
    States: array of TState;
    Actions: array of TAction;
    Rewards: array of Double;
  end;

  TPolicyNetwork = class
  private
    FWeights: array of array of Double;
    FStateSize: Integer;
    FActionCount: Integer;
    FLearningRate: Double;

    function Softmax(const ALogits: array of Double): TArray<Double>;
  public
    constructor Create(AStateSize, AActionCount: Integer);
    destructor Destroy; override;

    function GetActionProbabilities(AState: TState): TArray<Double>;
    function SampleAction(AState: TState): TAction;
    procedure Update(const ATrajectory: TTrajectory);
  end;

implementation

constructor TPolicyNetwork.Create(AStateSize, AActionCount: Integer);  
var
  i, j: Integer;
begin
  FStateSize := AStateSize;
  FActionCount := AActionCount;
  FLearningRate := 0.01;

  SetLength(FWeights, FStateSize, FActionCount);

  // Initialisation aléatoire
  for i := 0 to FStateSize - 1 do
    for j := 0 to FActionCount - 1 do
      FWeights[i, j] := (Random - 0.5) * 0.1;
end;

destructor TPolicyNetwork.Destroy;  
begin
  SetLength(FWeights, 0, 0);
  inherited;
end;

function TPolicyNetwork.Softmax(const ALogits: array of Double): TArray<Double>;  
var
  i: Integer;
  maxLogit, sumExp: Double;
begin
  SetLength(Result, Length(ALogits));

  // Trouver le max pour la stabilité numérique
  maxLogit := ALogits[0];
  for i := 1 to High(ALogits) do
    if ALogits[i] > maxLogit then
      maxLogit := ALogits[i];

  // Calculer exp et somme
  sumExp := 0;
  for i := 0 to High(ALogits) do
  begin
    Result[i] := Exp(ALogits[i] - maxLogit);
    sumExp := sumExp + Result[i];
  end;

  // Normaliser
  for i := 0 to High(Result) do
    Result[i] := Result[i] / sumExp;
end;

function TPolicyNetwork.GetActionProbabilities(AState: TState): TArray<Double>;  
var
  i: Integer;
  logits: array of Double;
begin
  SetLength(logits, FActionCount);

  // Calculer les logits (produit scalaire simplifié)
  for i := 0 to FActionCount - 1 do
    logits[i] := FWeights[AState, i];

  Result := Softmax(logits);
  SetLength(logits, 0);
end;

function TPolicyNetwork.SampleAction(AState: TState): TAction;  
var
  probs: TArray<Double>;
  randomValue, cumulative: Double;
  i: Integer;
begin
  probs := GetActionProbabilities(AState);

  randomValue := Random;
  cumulative := 0;
  Result := 0;

  for i := 0 to High(probs) do
  begin
    cumulative := cumulative + probs[i];
    if randomValue <= cumulative then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TPolicyNetwork.Update(const ATrajectory: TTrajectory);  
var
  i, t: Integer;
  returns: array of Double;
  G: Double;
  probs: TArray<Double>;
  gradient: Double;
begin
  SetLength(returns, Length(ATrajectory.Rewards));

  // Calculer les retours (returns)
  G := 0;
  for t := High(ATrajectory.Rewards) downto 0 do
  begin
    G := ATrajectory.Rewards[t] + 0.99 * G;
    returns[t] := G;
  end;

  // Mise à jour des poids
  for t := 0 to High(ATrajectory.States) do
  begin
    probs := GetActionProbabilities(ATrajectory.States[t]);

    // Gradient : ∇log π(a|s) * G
    for i := 0 to FActionCount - 1 do
    begin
      if i = ATrajectory.Actions[t] then
        gradient := (1 - probs[i]) * returns[t]
      else
        gradient := -probs[i] * returns[t];

      FWeights[ATrajectory.States[t], i] :=
        FWeights[ATrajectory.States[t], i] + FLearningRate * gradient;
    end;
  end;

  SetLength(returns, 0);
end;

end.
```

---

## Actor-Critic - Meilleur des Deux Mondes

Combiner Policy Gradient (Actor) et Value Function (Critic).

### Architecture

```
         État
           ↓
    ┌──────────────┐
    │   Critique   │ → Valeur V(s)
    └──────────────┘
           ↓
    ┌──────────────┐
    │    Acteur    │ → Politique π(a|s)
    └──────────────┘
```

### Implémentation simplifiée

```pascal
unit ActorCritic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, ReinforcementLearning;

type
  TActorCriticAgent = class(TRLAgent)
  private
    // Actor : Politique (comme Policy Gradient)
    FActorWeights: array of array of Double;

    // Critic : Fonction de valeur
    FCriticWeights: array of Double;

    FActorLR: Double;
    FCriticLR: Double;

    function GetStateValue(AState: TState): Double;
    function GetActionProbabilities(AState: TState): TArray<Double>;
    procedure UpdateActor(AState: TState; AAction: TAction; AAdvantage: Double);
    procedure UpdateCritic(AState: TState; ATDError: Double);
  public
    constructor Create(AStateCount, AActionCount: Integer);
    destructor Destroy; override;

    function ChooseAction(AState: TState): TAction; override;
    procedure Learn(const AExperience: TExperience); override;
  end;

implementation

constructor TActorCriticAgent.Create(AStateCount, AActionCount: Integer);  
var
  i, j: Integer;
begin
  inherited Create(AStateCount, AActionCount);

  FActorLR := 0.01;
  FCriticLR := 0.1;

  // Initialiser l'acteur
  SetLength(FActorWeights, FStateCount, FActionCount);
  for i := 0 to FStateCount - 1 do
    for j := 0 to FActionCount - 1 do
      FActorWeights[i, j] := (Random - 0.5) * 0.1;

  // Initialiser le critique
  SetLength(FCriticWeights, FStateCount);
  for i := 0 to FStateCount - 1 do
    FCriticWeights[i] := 0;
end;

destructor TActorCriticAgent.Destroy;  
begin
  SetLength(FActorWeights, 0, 0);
  SetLength(FCriticWeights, 0);
  inherited;
end;

function TActorCriticAgent.GetStateValue(AState: TState): Double;  
begin
  Result := FCriticWeights[AState];
end;

function TActorCriticAgent.GetActionProbabilities(AState: TState): TArray<Double>;  
var
  i: Integer;
  sumExp, maxWeight: Double;
begin
  SetLength(Result, FActionCount);

  // Trouver max
  maxWeight := FActorWeights[AState, 0];
  for i := 1 to FActionCount - 1 do
    if FActorWeights[AState, i] > maxWeight then
      maxWeight := FActorWeights[AState, i];

  // Softmax
  sumExp := 0;
  for i := 0 to FActionCount - 1 do
  begin
    Result[i] := Exp(FActorWeights[AState, i] - maxWeight);
    sumExp := sumExp + Result[i];
  end;

  for i := 0 to FActionCount - 1 do
    Result[i] := Result[i] / sumExp;
end;

function TActorCriticAgent.ChooseAction(AState: TState): TAction;  
var
  probs: TArray<Double>;
  randomValue, cumulative: Double;
  i: Integer;
begin
  probs := GetActionProbabilities(AState);

  randomValue := Random;
  cumulative := 0;
  Result := 0;

  for i := 0 to High(probs) do
  begin
    cumulative := cumulative + probs[i];
    if randomValue <= cumulative then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TActorCriticAgent.UpdateActor(AState: TState; AAction: TAction;
                                         AAdvantage: Double);
var
  i: Integer;
  probs: TArray<Double>;
  gradient: Double;
begin
  probs := GetActionProbabilities(AState);

  for i := 0 to FActionCount - 1 do
  begin
    if i = AAction then
      gradient := (1 - probs[i]) * AAdvantage
    else
      gradient := -probs[i] * AAdvantage;

    FActorWeights[AState, i] :=
      FActorWeights[AState, i] + FActorLR * gradient;
  end;
end;

procedure TActorCriticAgent.UpdateCritic(AState: TState; ATDError: Double);  
begin
  FCriticWeights[AState] :=
    FCriticWeights[AState] + FCriticLR * ATDError;
end;

procedure TActorCriticAgent.Learn(const AExperience: TExperience);  
var
  currentValue, nextValue, tdError, advantage: Double;
begin
  // Valeur de l'état actuel
  currentValue := GetStateValue(AExperience.State);

  if AExperience.Done then
    nextValue := 0
  else
    nextValue := GetStateValue(AExperience.NextState);

  // Erreur TD (Temporal Difference)
  tdError := AExperience.Reward + FDiscountFactor * nextValue - currentValue;

  // L'avantage est l'erreur TD
  advantage := tdError;

  // Mettre à jour le critique
  UpdateCritic(AExperience.State, tdError);

  // Mettre à jour l'acteur
  UpdateActor(AExperience.State, AExperience.Action, advantage);
end;

end.
```

---

## Exemple Avancé : Jeu Simple

### Cart-Pole (Perche sur Chariot)

Un problème classique de RL : équilibrer une perche sur un chariot mobile.

```pascal
unit CartPole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, ReinforcementLearning;

type
  TCartPoleState = record
    X: Double;          // Position du chariot
    XDot: Double;       // Vitesse du chariot
    Theta: Double;      // Angle de la perche
    ThetaDot: Double;   // Vitesse angulaire
  end;

  TCartPoleEnv = class(TInterfacedObject, IEnvironment)
  private
    FState: TCartPoleState;
    FGravity: Double;
    FMassCart: Double;
    FMassPole: Double;
    FLength: Double;
    FForceMagnitude: Double;
    FTau: Double;
    FStepCount: Integer;
    FMaxSteps: Integer;

    function StateToDiscrete(const AState: TCartPoleState): TState;
    procedure UpdatePhysics(AForce: Double);
  public
    constructor Create;

    function Reset: TState;
    function Step(AAction: TAction; out AReward: Double; out ADone: Boolean): TState;
    function GetStateCount: Integer;
    function GetActionCount: Integer;
    function GetValidActions(AState: TState): TArray<TAction>;

    procedure Render;
    function IsDone: Boolean;
  end;

implementation

constructor TCartPoleEnv.Create;  
begin
  FGravity := 9.8;
  FMassCart := 1.0;
  FMassPole := 0.1;
  FLength := 0.5;
  FForceMagnitude := 10.0;
  FTau := 0.02;  // Pas de temps
  FMaxSteps := 500;
end;

function TCartPoleEnv.Reset: TState;  
begin
  // État initial aléatoire
  FState.X := (Random - 0.5) * 0.1;
  FState.XDot := (Random - 0.5) * 0.1;
  FState.Theta := (Random - 0.5) * 0.1;
  FState.ThetaDot := (Random - 0.5) * 0.1;
  FStepCount := 0;

  Result := StateToDiscrete(FState);
end;

procedure TCartPoleEnv.UpdatePhysics(AForce: Double);  
var
  cosTheta, sinTheta: Double;
  temp, thetaAcc, xAcc: Double;
  totalMass: Double;
begin
  cosTheta := Cos(FState.Theta);
  sinTheta := Sin(FState.Theta);
  totalMass := FMassCart + FMassPole;

  temp := (AForce + FMassPole * FLength * FState.ThetaDot * FState.ThetaDot * sinTheta) / totalMass;

  thetaAcc := (FGravity * sinTheta - cosTheta * temp) /
              (FLength * (4/3 - FMassPole * cosTheta * cosTheta / totalMass));

  xAcc := temp - FMassPole * FLength * thetaAcc * cosTheta / totalMass;

  // Intégration d'Euler
  FState.X := FState.X + FTau * FState.XDot;
  FState.XDot := FState.XDot + FTau * xAcc;
  FState.Theta := FState.Theta + FTau * FState.ThetaDot;
  FState.ThetaDot := FState.ThetaDot + FTau * thetaAcc;
end;

function TCartPoleEnv.IsDone: Boolean;  
begin
  Result := (Abs(FState.X) > 2.4) or           // Chariot sort
            (Abs(FState.Theta) > 12 * Pi / 180) or // Perche tombe
            (FStepCount >= FMaxSteps);
end;

function TCartPoleEnv.Step(AAction: TAction; out AReward: Double;
                            out ADone: Boolean): TState;
var
  force: Double;
begin
  Inc(FStepCount);

  // Action 0 = gauche, Action 1 = droite
  if AAction = 0 then
    force := -FForceMagnitude
  else
    force := FForceMagnitude;

  UpdatePhysics(force);

  ADone := IsDone;

  if ADone then
    AReward := 0
  else
    AReward := 1;  // +1 pour chaque pas où la perche reste debout

  Result := StateToDiscrete(FState);
end;

function TCartPoleEnv.StateToDiscrete(const AState: TCartPoleState): TState;  
var
  xBin, xDotBin, thetaBin, thetaDotBin: Integer;
const
  NUM_BINS = 6;
begin
  // Discrétiser l'espace d'états continu
  xBin := Trunc((AState.X + 2.4) / 4.8 * NUM_BINS);
  xBin := Max(0, Min(NUM_BINS - 1, xBin));

  xDotBin := Trunc((AState.XDot + 3) / 6 * NUM_BINS);
  xDotBin := Max(0, Min(NUM_BINS - 1, xDotBin));

  thetaBin := Trunc((AState.Theta + 0.3) / 0.6 * NUM_BINS);
  thetaBin := Max(0, Min(NUM_BINS - 1, thetaBin));

  thetaDotBin := Trunc((AState.ThetaDot + 2) / 4 * NUM_BINS);
  thetaDotBin := Max(0, Min(NUM_BINS - 1, thetaDotBin));

  // Combiner en un seul état
  Result := xBin +
            xDotBin * NUM_BINS +
            thetaBin * NUM_BINS * NUM_BINS +
            thetaDotBin * NUM_BINS * NUM_BINS * NUM_BINS;
end;

function TCartPoleEnv.GetStateCount: Integer;  
const
  NUM_BINS = 6;
begin
  Result := NUM_BINS * NUM_BINS * NUM_BINS * NUM_BINS;
end;

function TCartPoleEnv.GetActionCount: Integer;  
begin
  Result := 2;  // Gauche ou Droite
end;

function TCartPoleEnv.GetValidActions(AState: TState): TArray<TAction>;  
begin
  SetLength(Result, 2);
  Result[0] := 0;
  Result[1] := 1;
end;

procedure TCartPoleEnv.Render;  
var
  i, cartPos, poleX, poleY: Integer;
  line: string;
const
  WIDTH = 50;
begin
  cartPos := Round((FState.X + 2.4) / 4.8 * WIDTH);
  cartPos := Max(0, Min(WIDTH - 1, cartPos));

  poleX := Round(Sin(FState.Theta) * 10);
  poleY := Round(Cos(FState.Theta) * 10);

  WriteLn;
  WriteLn('┌', StringOfChar('─', WIDTH), '┐');

  // Dessiner la perche (simplifié)
  line := StringOfChar(' ', WIDTH);
  if (cartPos + poleX >= 0) and (cartPos + poleX < WIDTH) then
    line[cartPos + poleX + 1] := '|';
  WriteLn('│', line, '│');

  // Dessiner le chariot
  line := StringOfChar(' ', WIDTH);
  line[cartPos + 1] := '█';
  WriteLn('│', line, '│');

  WriteLn('└', StringOfChar('─', WIDTH), '┘');
  WriteLn(Format('X: %.2f | Angle: %.2f° | Steps: %d',
    [FState.X, FState.Theta * 180 / Pi, FStepCount]));
end;

end.
```

### Programme d'entraînement Cart-Pole

```pascal
program TrainCartPole;

{$mode objfpc}{$H+}

uses
  SysUtils, QLearning, CartPole, ReinforcementLearning;

var
  env: TCartPoleEnv;
  agent: TQLearningAgent;
  episode, step: Integer;
  state, nextState: TState;
  action: TAction;
  reward, totalReward: Double;
  done: Boolean;
  experience: TExperience;
  successCount, avgSteps: Integer;

begin
  WriteLn('=========================================');
  WriteLn('  Cart-Pole - Apprentissage par RL      ');
  WriteLn('=========================================');
  WriteLn;

  Randomize;

  env := TCartPoleEnv.Create;
  agent := TQLearningAgent.Create(
    env.GetStateCount,
    env.GetActionCount
  );

  try
    agent.LearningRate := 0.1;
    agent.DiscountFactor := 0.99;
    agent.Epsilon := 0.5;

    WriteLn('Entraînement en cours...');
    WriteLn;
    WriteLn('Episode | Steps | Reward | Epsilon');
    WriteLn('-------------------------------------');

    successCount := 0;
    avgSteps := 0;

    for episode := 1 to 1000 do
    begin
      state := env.Reset;
      agent.Reset;
      totalReward := 0;
      step := 0;

      repeat
        action := agent.ChooseAction(state);
        nextState := env.Step(action, reward, done);

        experience.State := state;
        experience.Action := action;
        experience.Reward := reward;
        experience.NextState := nextState;
        experience.Done := done;

        agent.Learn(experience);

        totalReward := totalReward + reward;
        state := nextState;
        Inc(step);

      until done;

      avgSteps := avgSteps + step;

      if step >= 200 then
        Inc(successCount);

      if episode mod 50 = 0 then
      begin
        WriteLn(Format('%7d | %5d | %6.0f | %.3f',
          [episode, step, totalReward, agent.Epsilon]));
        avgSteps := avgSteps div 50;
        WriteLn(Format('  → Moyenne sur 50: %d steps, Succès: %d%%',
          [avgSteps, (successCount * 100) div 50]));
        avgSteps := 0;
        successCount := 0;
      end;
    end;

    WriteLn;
    WriteLn('Entraînement terminé!');
    WriteLn;
    WriteLn('Test de l''agent entraîné (epsilon=0)...');

    agent.Epsilon := 0;
    state := env.Reset;

    repeat
      env.Render;
      action := agent.ChooseAction(state);
      state := env.Step(action, reward, done);

      {$IFDEF WINDOWS}
      Sleep(50);
      {$ELSE}
      Sleep(50);
      {$ENDIF}

    until done;

    WriteLn;
    if reward > 0 then
      WriteLn('✓ Succès!')
    else
      WriteLn('✗ Échec');

  finally
    agent.Free;
    env.Free;
  end;

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Multi-Armed Bandit - Cas Particulier

Le problème du bandit manchot est un cas simplifié de RL avec un seul état.

### Contexte

Imaginez un casino avec plusieurs machines à sous :
- Chaque machine a un taux de gain différent (inconnu)
- Objectif : maximiser les gains totaux
- Dilemme : explorer (tester toutes les machines) vs exploiter (jouer à la meilleure)

### Implémentation

```pascal
unit MultiArmedBandit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TBandit = class
  private
    FArms: Integer;
    FTrueValues: array of Double;
    FEstimatedValues: array of Double;
    FActionCounts: array of Integer;
    FTotalReward: Double;
    FSteps: Integer;
  public
    constructor Create(AArms: Integer);
    destructor Destroy; override;

    function PullArm(AArm: Integer): Double;
    procedure UpdateEstimate(AArm: Integer; AReward: Double);

    function EpsilonGreedy(AEpsilon: Double): Integer;
    function UCB(AC: Double): Integer;
    function ThompsonSampling: Integer;

    procedure PrintStats;

    property TotalReward: Double read FTotalReward;
    property Steps: Integer read FSteps;
  end;

implementation

constructor TBandit.Create(AArms: Integer);  
var
  i: Integer;
begin
  FArms := AArms;

  SetLength(FTrueValues, FArms);
  SetLength(FEstimatedValues, FArms);
  SetLength(FActionCounts, FArms);

  // Initialiser avec des valeurs aléatoires
  for i := 0 to FArms - 1 do
  begin
    FTrueValues[i] := Random;  // Valeur vraie entre 0 et 1
    FEstimatedValues[i] := 0;
    FActionCounts[i] := 0;
  end;

  FTotalReward := 0;
  FSteps := 0;
end;

destructor TBandit.Destroy;  
begin
  SetLength(FTrueValues, 0);
  SetLength(FEstimatedValues, 0);
  SetLength(FActionCounts, 0);
  inherited;
end;

function TBandit.PullArm(AArm: Integer): Double;  
begin
  Inc(FSteps);

  // Récompense : valeur vraie + bruit gaussien
  Result := FTrueValues[AArm] + RandG(0, 0.1);

  FTotalReward := FTotalReward + Result;
  Inc(FActionCounts[AArm]);
end;

procedure TBandit.UpdateEstimate(AArm: Integer; AReward: Double);  
var
  n: Integer;
begin
  n := FActionCounts[AArm];

  // Moyenne incrémentale
  FEstimatedValues[AArm] := FEstimatedValues[AArm] +
                            (AReward - FEstimatedValues[AArm]) / n;
end;

function TBandit.EpsilonGreedy(AEpsilon: Double): Integer;  
var
  i, bestArm: Integer;
  maxValue: Double;
begin
  if Random < AEpsilon then
  begin
    // Exploration
    Result := Random(FArms);
  end
  else
  begin
    // Exploitation
    bestArm := 0;
    maxValue := FEstimatedValues[0];

    for i := 1 to FArms - 1 do
    begin
      if FEstimatedValues[i] > maxValue then
      begin
        maxValue := FEstimatedValues[i];
        bestArm := i;
      end;
    end;

    Result := bestArm;
  end;
end;

function TBandit.UCB(AC: Double): Integer;  
var
  i, bestArm: Integer;
  maxValue, ucbValue: Double;
begin
  bestArm := 0;
  maxValue := -Infinity;

  for i := 0 to FArms - 1 do
  begin
    if FActionCounts[i] = 0 then
    begin
      Result := i;
      Exit;
    end;

    ucbValue := FEstimatedValues[i] +
                AC * Sqrt(Ln(FSteps) / FActionCounts[i]);

    if ucbValue > maxValue then
    begin
      maxValue := ucbValue;
      bestArm := i;
    end;
  end;

  Result := bestArm;
end;

function TBandit.ThompsonSampling: Integer;  
var
  i, bestArm: Integer;
  samples: array of Double;
  maxSample: Double;
begin
  SetLength(samples, FArms);

  bestArm := 0;
  maxSample := -Infinity;

  for i := 0 to FArms - 1 do
  begin
    // Échantillon d'une distribution (simplifié)
    samples[i] := FEstimatedValues[i] + RandG(0, 1.0 / Max(1, FActionCounts[i]));

    if samples[i] > maxSample then
    begin
      maxSample := samples[i];
      bestArm := i;
    end;
  end;

  Result := bestArm;
  SetLength(samples, 0);
end;

procedure TBandit.PrintStats;  
var
  i, bestArm: Integer;
  regret: Double;
begin
  WriteLn('Statistiques:');
  WriteLn('-------------');

  bestArm := 0;
  for i := 1 to FArms - 1 do
    if FTrueValues[i] > FTrueValues[bestArm] then
      bestArm := i;

  WriteLn(Format('Steps: %d', [FSteps]));
  WriteLn(Format('Récompense totale: %.2f', [FTotalReward]));
  WriteLn(Format('Récompense moyenne: %.3f', [FTotalReward / FSteps]));
  WriteLn;

  WriteLn('Bras | Valeur vraie | Valeur estimée | Tirages | %');
  WriteLn('--------------------------------------------------------');

  for i := 0 to FArms - 1 do
  begin
    Write(Format('%4d | %12.3f | %14.3f | %7d | %3d%%',
      [i, FTrueValues[i], FEstimatedValues[i], FActionCounts[i],
       (FActionCounts[i] * 100) div Max(1, FSteps)]));

    if i = bestArm then
      WriteLn(' ← Meilleur')
    else
      WriteLn;
  end;

  // Regret = récompense optimale - récompense obtenue
  regret := FTrueValues[bestArm] * FSteps - FTotalReward;
  WriteLn;
  WriteLn(Format('Regret: %.2f', [regret]));
end;

end.
```

### Test des stratégies

```pascal
program TestBandit;

{$mode objfpc}{$H+}

uses
  SysUtils, MultiArmedBandit;

procedure TestStrategy(const AName: string; AProc: TProcedure);  
begin
  WriteLn;
  WriteLn('========================================');
  WriteLn('  Stratégie: ', AName);
  WriteLn('========================================');
  AProc();
end;

var
  bandit: TBandit;
  i, arm: Integer;
  reward: Double;

begin
  Randomize;

  WriteLn('=========================================');
  WriteLn('  Multi-Armed Bandit                    ');
  WriteLn('=========================================');

  // Test Epsilon-Greedy
  TestStrategy('Epsilon-Greedy (ε=0.1)',
    procedure
    begin
      bandit := TBandit.Create(10);
      try
        for i := 1 to 1000 do
        begin
          arm := bandit.EpsilonGreedy(0.1);
          reward := bandit.PullArm(arm);
          bandit.UpdateEstimate(arm, reward);
        end;

        bandit.PrintStats;
      finally
        bandit.Free;
      end;
    end
  );

  // Test UCB
  TestStrategy('UCB (c=2.0)',
    procedure
    begin
      bandit := TBandit.Create(10);
      try
        for i := 1 to 1000 do
        begin
          arm := bandit.UCB(2.0);
          reward := bandit.PullArm(arm);
          bandit.UpdateEstimate(arm, reward);
        end;

        bandit.PrintStats;
      finally
        bandit.Free;
      end;
    end
  );

  // Test Thompson Sampling
  TestStrategy('Thompson Sampling',
    procedure
    begin
      bandit := TBandit.Create(10);
      try
        for i := 1 to 1000 do
        begin
          arm := bandit.ThompsonSampling;
          reward := bandit.PullArm(arm);
          bandit.UpdateEstimate(arm, reward);
        end;

        bandit.PrintStats;
      finally
        bandit.Free;
      end;
    end
  );

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Visualisation avec Lazarus

### Interface pour observer l'apprentissage

```pascal
unit RLVisualization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ReinforcementLearning, QLearning, GridWorld;

type
  TFormRL = class(TForm)
    ButtonStart: TButton;
    ButtonStep: TButton;
    ButtonReset: TButton;
    PaintBoxGrid: TPaintBox;
    PaintBoxQValues: TPaintBox;
    LabelEpisode: TLabel;
    LabelReward: TLabel;
    TrackBarSpeed: TTrackBar;
    Timer1: TTimer;
    CheckBoxAutoRun: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStepClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBoxGridPaint(Sender: TObject);
    procedure PaintBoxQValuesPaint(Sender: TObject);
  private
    FEnv: TGridWorld;
    FAgent: TQLearningAgent;
    FCurrentState: TState;
    FEpisode: Integer;
    FTotalReward: Double;
    FDone: Boolean;

    procedure DrawGrid;
    procedure DrawQValues;
    procedure PerformStep;
  public
  end;

var
  FormRL: TFormRL;

implementation

{$R *.lfm}

procedure TFormRL.FormCreate(Sender: TObject);  
begin
  FEnv := TGridWorld.Create(8, 8);

  // Configuration du labyrinthe
  FEnv.SetStart(0, 0);
  FEnv.SetGoal(7, 7);
  FEnv.SetWall(3, 0);
  FEnv.SetWall(3, 1);
  FEnv.SetWall(3, 2);
  FEnv.SetWall(3, 3);
  FEnv.SetWall(5, 4);
  FEnv.SetWall(5, 5);
  FEnv.SetWall(5, 6);

  FAgent := TQLearningAgent.Create(
    FEnv.GetStateCount,
    FEnv.GetActionCount
  );

  FAgent.LearningRate := 0.1;
  FAgent.DiscountFactor := 0.95;
  FAgent.Epsilon := 0.3;

  FCurrentState := FEnv.Reset;
  FEpisode := 0;
  FTotalReward := 0;
  FDone := False;

  Timer1.Interval := 100;
  Timer1.Enabled := False;
end;

procedure TFormRL.FormDestroy(Sender: TObject);  
begin
  FAgent.Free;
  FEnv.Free;
end;

procedure TFormRL.ButtonStartClick(Sender: TObject);  
begin
  Timer1.Enabled := not Timer1.Enabled;

  if Timer1.Enabled then
    ButtonStart.Caption := 'Pause'
  else
    ButtonStart.Caption := 'Démarrer';
end;

procedure TFormRL.ButtonStepClick(Sender: TObject);  
begin
  PerformStep;
end;

procedure TFormRL.ButtonResetClick(Sender: TObject);  
begin
  FCurrentState := FEnv.Reset;
  FAgent.Reset;
  FEpisode := 0;
  FTotalReward := 0;
  FDone := False;

  PaintBoxGrid.Invalidate;
  PaintBoxQValues.Invalidate;
end;

procedure TFormRL.Timer1Timer(Sender: TObject);  
begin
  PerformStep;
end;

procedure TFormRL.PerformStep;  
var
  action: TAction;
  nextState: TState;
  reward: Double;
  experience: TExperience;
begin
  if FDone then
  begin
    // Nouvel épisode
    FCurrentState := FEnv.Reset;
    FAgent.Reset;
    Inc(FEpisode);
    FTotalReward := 0;
    FDone := False;
  end;

  action := FAgent.ChooseAction(FCurrentState);
  nextState := FEnv.Step(action, reward, FDone);

  experience.State := FCurrentState;
  experience.Action := action;
  experience.Reward := reward;
  experience.NextState := nextState;
  experience.Done := FDone;

  FAgent.Learn(experience);

  FTotalReward := FTotalReward + reward;
  FCurrentState := nextState;

  // Mise à jour de l'interface
  LabelEpisode.Caption := Format('Épisode: %d | ε: %.3f',
    [FEpisode, FAgent.Epsilon]);
  LabelReward.Caption := Format('Récompense: %.1f', [FTotalReward]);

  PaintBoxGrid.Invalidate;

  if FDone then
    PaintBoxQValues.Invalidate;
end;

procedure TFormRL.PaintBoxGridPaint(Sender: TObject);  
var
  cellWidth, cellHeight: Integer;
  x, y, agentX, agentY: Integer;
  rect: TRect;
begin
  cellWidth := PaintBoxGrid.Width div FEnv.Width;
  cellHeight := PaintBoxGrid.Height div FEnv.Height;

  // Extraire la position de l'agent
  agentX := FCurrentState mod FEnv.Width;
  agentY := FCurrentState div FEnv.Width;

  // Dessiner la grille
  for y := 0 to FEnv.Height - 1 do
  begin
    for x := 0 to FEnv.Width - 1 do
    begin
      rect := Rect(x * cellWidth, y * cellHeight,
                   (x + 1) * cellWidth, (y + 1) * cellHeight);

      // Couleur de fond selon le type de case
      if (x = agentX) and (y = agentY) then
        PaintBoxGrid.Canvas.Brush.Color := clLime  // Agent
      else if FEnv.FGrid[x, y] = ctGoal then
        PaintBoxGrid.Canvas.Brush.Color := clYellow  // Objectif
      else if FEnv.FGrid[x, y] = ctWall then
        PaintBoxGrid.Canvas.Brush.Color := clBlack  // Mur
      else
        PaintBoxGrid.Canvas.Brush.Color := clWhite;  // Vide

      PaintBoxGrid.Canvas.FillRect(rect);
      PaintBoxGrid.Canvas.Rectangle(rect);
    end;
  end;
end;

procedure TFormRL.PaintBoxQValuesPaint(Sender: TObject);  
var
  x, y, state, bestAction: Integer;
  cellWidth, cellHeight: Integer;
  maxQ: Double;
  rect: TRect;
  brightness: Byte;
  color: TColor;
begin
  cellWidth := PaintBoxQValues.Width div FEnv.Width;
  cellHeight := PaintBoxQValues.Height div FEnv.Height;

  // Dessiner les valeurs Q
  for y := 0 to FEnv.Height - 1 do
  begin
    for x := 0 to FEnv.Width - 1 do
    begin
      state := y * FEnv.Width + x;
      rect := Rect(x * cellWidth, y * cellHeight,
                   (x + 1) * cellWidth, (y + 1) * cellHeight);

      // Trouver la meilleure action et sa valeur Q
      maxQ := -Infinity;
      bestAction := 0;

      for action := 0 to FEnv.GetActionCount - 1 do
      begin
        if FAgent.GetQValue(state, action) > maxQ then
        begin
          maxQ := FAgent.GetQValue(state, action);
          bestAction := action;
        end;
      end;

      // Colorier selon la valeur Q (plus c'est élevé, plus c'est brillant)
      brightness := Min(255, Max(0, Round(maxQ * 2.55)));
      color := RGB(brightness, brightness, brightness);

      PaintBoxQValues.Canvas.Brush.Color := color;
      PaintBoxQValues.Canvas.FillRect(rect);
      PaintBoxQValues.Canvas.Rectangle(rect);

      // Dessiner une flèche pour indiquer la direction
      if maxQ > 0 then
      begin
        PaintBoxQValues.Canvas.Pen.Color := clRed;
        DrawArrow(rect, bestAction);
      end;
    end;
  end;
end;

procedure DrawArrow(const ARect: TRect; ADirection: Integer);  
var
  cx, cy: Integer;
begin
  cx := (ARect.Left + ARect.Right) div 2;
  cy := (ARect.Top + ARect.Bottom) div 2;

  case ADirection of
    0: // Haut
      begin
        PaintBoxQValues.Canvas.MoveTo(cx, cy);
        PaintBoxQValues.Canvas.LineTo(cx, ARect.Top + 5);
      end;
    1: // Bas
      begin
        PaintBoxQValues.Canvas.MoveTo(cx, cy);
        PaintBoxQValues.Canvas.LineTo(cx, ARect.Bottom - 5);
      end;
    2: // Gauche
      begin
        PaintBoxQValues.Canvas.MoveTo(cx, cy);
        PaintBoxQValues.Canvas.LineTo(ARect.Left + 5, cy);
      end;
    3: // Droite
      begin
        PaintBoxQValues.Canvas.MoveTo(cx, cy);
        PaintBoxQValues.Canvas.LineTo(ARect.Right - 5, cy);
      end;
  end;
end;

end.
```

---

## Gestion Multi-Plateforme

### Chemins et Ressources

```pascal
unit RLPlatformUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

function GetModelPath: string;  
function GetLogPath: string;  
procedure SaveModel(const AFileName: string; AAgent: TRLAgent);  
procedure LoadModel(const AFileName: string; AAgent: TRLAgent);

implementation

function GetModelPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := ExtractFilePath(ParamStr(0)) + 'models\';
  {$ENDIF}
  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + '/.rl_app/models/';
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetLogPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := ExtractFilePath(ParamStr(0)) + 'logs\';
  {$ENDIF}
  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + '/.rl_app/logs/';
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

procedure SaveModel(const AFileName: string; AAgent: TRLAgent);  
var
  fullPath: string;
begin
  fullPath := GetModelPath + AFileName;

  if AAgent is TQLearningAgent then
    TQLearningAgent(AAgent).SaveQTable(fullPath);
end;

procedure LoadModel(const AFileName: string; AAgent: TRLAgent);  
var
  fullPath: string;
begin
  fullPath := GetModelPath + AFileName;

  if FileExists(fullPath) and (AAgent is TQLearningAgent) then
    TQLearningAgent(AAgent).LoadQTable(fullPath);
end;

end.
```

---

## Conseils Pratiques et Débogage

### 1. Problèmes courants et solutions

**L'agent n'apprend pas**
```pascal
procedure DiagnoseAgent(AAgent: TQLearningAgent; AEnv: IEnvironment);  
begin
  WriteLn('=== Diagnostic ===');
  WriteLn('Learning rate: ', AAgent.LearningRate:0:3);
  WriteLn('Discount factor: ', AAgent.DiscountFactor:0:3);
  WriteLn('Epsilon: ', AAgent.Epsilon:0:3);
  WriteLn;

  // Vérifier si les Q-values changent
  WriteLn('Exemple de Q-values:');
  for i := 0 to 4 do
  begin
    Write('État ', i, ': ');
    for j := 0 to AEnv.GetActionCount - 1 do
      Write(Format('%.2f ', [AAgent.GetQValue(i, j)]));
    WriteLn;
  end;

  // Recommandations
  if AAgent.LearningRate < 0.01 then
    WriteLn('⚠ Learning rate trop faible');
  if AAgent.Epsilon < 0.05 then
    WriteLn('⚠ Pas assez d''exploration');
end;
```

**Convergence lente**
- Augmenter le learning rate (0.1 → 0.5)
- Augmenter l'exploration initiale (ε = 0.5)
- Réduire plus lentement epsilon
- Vérifier la structure des récompenses

**Instabilité**
- Réduire le learning rate
- Utiliser experience replay
- Ajouter un target network
- Normaliser les récompenses

### 2. Logging et Métriques

```pascal
unit RLMetrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRLMetrics = class
  private
    FEpisodeRewards: array of Double;
    FEpisodeLengths: array of Integer;
    FLogFile: TextFile;
  public
    constructor Create(const ALogFileName: string);
    destructor Destroy; override;

    procedure RecordEpisode(AReward: Double; ALength: Integer);
    function GetAverageReward(ALastN: Integer): Double;
    function GetAverageLength(ALastN: Integer): Double;
    procedure PrintSummary;
    procedure SaveToCSV(const AFileName: string);
  end;

implementation

constructor TRLMetrics.Create(const ALogFileName: string);  
begin
  SetLength(FEpisodeRewards, 0);
  SetLength(FEpisodeLengths, 0);

  AssignFile(FLogFile, ALogFileName);
  Rewrite(FLogFile);
  WriteLn(FLogFile, 'Episode,Reward,Length,AvgReward100,AvgLength100');
end;

destructor TRLMetrics.Destroy;  
begin
  CloseFile(FLogFile);
  SetLength(FEpisodeRewards, 0);
  SetLength(FEpisodeLengths, 0);
  inherited;
end;

procedure TRLMetrics.RecordEpisode(AReward: Double; ALength: Integer);  
var
  episode: Integer;
begin
  episode := Length(FEpisodeRewards);

  SetLength(FEpisodeRewards, episode + 1);
  SetLength(FEpisodeLengths, episode + 1);

  FEpisodeRewards[episode] := AReward;
  FEpisodeLengths[episode] := ALength;

  // Logger dans le fichier
  WriteLn(FLogFile, Format('%d,%.2f,%d,%.2f,%.2f',
    [episode + 1, AReward, ALength,
     GetAverageReward(100),
     GetAverageLength(100)]));
  Flush(FLogFile);
end;

function TRLMetrics.GetAverageReward(ALastN: Integer): Double;  
var
  i, count, start: Integer;
  sum: Double;
begin
  count := Length(FEpisodeRewards);
  if count = 0 then
  begin
    Result := 0;
    Exit;
  end;

  start := Max(0, count - ALastN);
  sum := 0;

  for i := start to count - 1 do
    sum := sum + FEpisodeRewards[i];

  Result := sum / (count - start);
end;

function TRLMetrics.GetAverageLength(ALastN: Integer): Double;  
var
  i, count, start: Integer;
  sum: Integer;
begin
  count := Length(FEpisodeLengths);
  if count = 0 then
  begin
    Result := 0;
    Exit;
  end;

  start := Max(0, count - ALastN);
  sum := 0;

  for i := start to count - 1 do
    sum := sum + FEpisodeLengths[i];

  Result := sum / (count - start);
end;

procedure TRLMetrics.PrintSummary;  
var
  i, totalEpisodes: Integer;
  bestReward, worstReward: Double;
begin
  totalEpisodes := Length(FEpisodeRewards);

  if totalEpisodes = 0 then
  begin
    WriteLn('Aucun épisode enregistré');
    Exit;
  end;

  bestReward := FEpisodeRewards[0];
  worstReward := FEpisodeRewards[0];

  for i := 1 to totalEpisodes - 1 do
  begin
    if FEpisodeRewards[i] > bestReward then
      bestReward := FEpisodeRewards[i];
    if FEpisodeRewards[i] < worstReward then
      worstReward := FEpisodeRewards[i];
  end;

  WriteLn('=== Résumé de l''entraînement ===');
  WriteLn(Format('Épisodes: %d', [totalEpisodes]));
  WriteLn(Format('Meilleure récompense: %.2f', [bestReward]));
  WriteLn(Format('Pire récompense: %.2f', [worstReward]));
  WriteLn(Format('Moyenne (100 derniers): %.2f', [GetAverageReward(100)]));
  WriteLn(Format('Longueur moyenne (100 derniers): %.2f', [GetAverageLength(100)]));
end;

procedure TRLMetrics.SaveToCSV(const AFileName: string);  
var
  f: TextFile;
  i: Integer;
begin
  AssignFile(f, AFileName);
  try
    Rewrite(f);
    WriteLn(f, 'Episode,Reward,Length');

    for i := 0 to High(FEpisodeRewards) do
      WriteLn(f, Format('%d,%.2f,%d',
        [i + 1, FEpisodeRewards[i], FEpisodeLengths[i]]));
  finally
    CloseFile(f);
  end;
end;

end.
```

---

## Comparaison des Algorithmes

### Tableau récapitulatif

| Algorithme | Type | Complexité | Convergence | Applications |
|------------|------|------------|-------------|--------------|
| **Q-Learning** | Value-based, Off-policy | O(S×A) | Garantie (sous conditions) | Grilles, jeux simples |
| **SARSA** | Value-based, On-policy | O(S×A) | Plus prudent | Environnements risqués |
| **DQN** | Value-based, Off-policy | O(N) réseau | Bonne sur gros états | Jeux Atari, robotique |
| **Policy Gradient** | Policy-based | O(N) réseau | Lente mais stable | Actions continues |
| **Actor-Critic** | Hybride | O(2N) réseaux | Meilleure | Contrôle continu |
| **A3C** | Hybride, Parallèle | O(N×Workers) | Très rapide | Production moderne |

### Quand utiliser quel algorithme ?

**Q-Learning / SARSA**
- ✅ Espace d'états discret et petit (<10000 états)
- ✅ Facile à implémenter et déboguer
- ✅ Garanties théoriques
- ❌ Ne scale pas pour grands espaces

**DQN**
- ✅ Grands espaces d'états (images, capteurs)
- ✅ États continus
- ✅ Prouvé sur Atari
- ❌ Instable sans tricks (replay, target network)
- ❌ Seulement actions discrètes

**Policy Gradient / Actor-Critic**
- ✅ Actions continues (robotique, contrôle)
- ✅ Environnements stochastiques
- ✅ Plus stable que DQN
- ❌ Convergence lente
- ❌ High variance

---

## Projet Complet : Tic-Tac-Toe avec RL

### Environnement Tic-Tac-Toe

```pascal
unit TicTacToeEnv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ReinforcementLearning;

type
  TPlayer = (pNone, pX, pO);
  TBoard = array[0..2, 0..2] of TPlayer;

  TTicTacToeEnv = class(TInterfacedObject, IEnvironment)
  private
    FBoard: TBoard;
    FCurrentPlayer: TPlayer;

    function BoardToState: TState;
    function CheckWinner: TPlayer;
    function IsFull: Boolean;
  public
    constructor Create;

    function Reset: TState;
    function Step(AAction: TAction; out AReward: Double; out ADone: Boolean): TState;
    function GetStateCount: Integer;
    function GetActionCount: Integer;
    function GetValidActions(AState: TState): TArray<TAction>;

    procedure Render;
    function IsValidMove(APosition: Integer): Boolean;
    procedure MakeMove(APosition: Integer; APlayer: TPlayer);
  end;

implementation

constructor TTicTacToeEnv.Create;  
begin
  Reset;
end;

function TTicTacToeEnv.Reset: TState;  
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      FBoard[i, j] := pNone;

  FCurrentPlayer := pX;
  Result := BoardToState;
end;

function TTicTacToeEnv.BoardToState: TState;  
var
  i, j: Integer;
begin
  Result := 0;

  for i := 0 to 2 do
    for j := 0 to 2 do
    begin
      Result := Result * 3 + Ord(FBoard[i, j]);
    end;
end;

function TTicTacToeEnv.CheckWinner: TPlayer;  
var
  i: Integer;
begin
  // Lignes
  for i := 0 to 2 do
  begin
    if (FBoard[i, 0] = FBoard[i, 1]) and
       (FBoard[i, 1] = FBoard[i, 2]) and
       (FBoard[i, 0] <> pNone) then
      Exit(FBoard[i, 0]);
  end;

  // Colonnes
  for i := 0 to 2 do
  begin
    if (FBoard[0, i] = FBoard[1, i]) and
       (FBoard[1, i] = FBoard[2, i]) and
       (FBoard[0, i] <> pNone) then
      Exit(FBoard[0, i]);
  end;

  // Diagonales
  if (FBoard[0, 0] = FBoard[1, 1]) and
     (FBoard[1, 1] = FBoard[2, 2]) and
     (FBoard[0, 0] <> pNone) then
    Exit(FBoard[0, 0]);

  if (FBoard[0, 2] = FBoard[1, 1]) and
     (FBoard[1, 1] = FBoard[2, 0]) and
     (FBoard[0, 2] <> pNone) then
    Exit(FBoard[0, 2]);

  Result := pNone;
end;

function TTicTacToeEnv.IsFull: Boolean;  
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      if FBoard[i, j] = pNone then
        Exit(False);
  Result := True;
end;

function TTicTacToeEnv.IsValidMove(APosition: Integer): Boolean;  
var
  row, col: Integer;
begin
  row := APosition div 3;
  col := APosition mod 3;
  Result := (row >= 0) and (row <= 2) and
            (col >= 0) and (col <= 2) and
            (FBoard[row, col] = pNone);
end;

procedure TTicTacToeEnv.MakeMove(APosition: Integer; APlayer: TPlayer);  
var
  row, col: Integer;
begin
  row := APosition div 3;
  col := APosition mod 3;
  FBoard[row, col] := APlayer;
end;

function TTicTacToeEnv.Step(AAction: TAction; out AReward: Double;
                             out ADone: Boolean): TState;
var
  winner: TPlayer;
begin
  if not IsValidMove(AAction) then
  begin
    AReward := -10;  // Mouvement invalide
    ADone := True;
    Result := BoardToState;
    Exit;
  end;

  MakeMove(AAction, FCurrentPlayer);
  winner := CheckWinner;

  if winner = pX then
  begin
    AReward := 1;
    ADone := True;
  end
  else if winner = pO then
  begin
    AReward := -1;
    ADone := True;
  end
  else if IsFull then
  begin
    AReward := 0.5;  // Match nul
    ADone := True;
  end
  else
  begin
    AReward := 0;
    ADone := False;

    // Changer de joueur
    if FCurrentPlayer = pX then
      FCurrentPlayer := pO
    else
      FCurrentPlayer := pX;
  end;

  Result := BoardToState;
end;

function TTicTacToeEnv.GetStateCount: Integer;  
begin
  Result := 19683;  // 3^9 états possibles
end;

function TTicTacToeEnv.GetActionCount: Integer;  
begin
  Result := 9;  // 9 positions
end;

function TTicTacToeEnv.GetValidActions(AState: TState): TArray<TAction>;  
var
  i, count: Integer;
  actions: array[0..8] of TAction;
begin
  count := 0;

  for i := 0 to 8 do
  begin
    if IsValidMove(i) then
    begin
      actions[count] := i;
      Inc(count);
    end;
  end;

  SetLength(Result, count);
  for i := 0 to count - 1 do
    Result[i] := actions[i];
end;

procedure TTicTacToeEnv.Render;  
var
  i, j: Integer;
  symbols: array[TPlayer] of Char = ('.', 'X', 'O');
begin
  WriteLn;
  WriteLn('  0 1 2');

  for i := 0 to 2 do
  begin
    Write(i, ' ');
    for j := 0 to 2 do
    begin
      Write(symbols[FBoard[i, j]]);
      if j < 2 then Write('|');
    end;
    WriteLn;

    if i < 2 then
      WriteLn('  -----');
  end;
  WriteLn;
end;

end.
```

### Programme d'entraînement Self-Play

```pascal
program TrainTicTacToe;

{$mode objfpc}{$H+}

uses
  SysUtils, QLearning, TicTacToeEnv, ReinforcementLearning, RLMetrics;

procedure SelfPlay(AAgent: TQLearningAgent; AEnv: TTicTacToeEnv;
                   AEpisodes: Integer; AMetrics: TRLMetrics);
var
  episode, step: Integer;
  state, nextState: TState;
  action: TAction;
  reward, totalReward: Double;
  done: Boolean;
  experience: TExperience;
  validActions: TArray<TAction>;
  wins, losses, draws: Integer;
begin
  wins := 0;
  losses := 0;
  draws := 0;

  WriteLn('Entraînement Self-Play...');
  WriteLn;

  for episode := 1 to AEpisodes do
  begin
    state := AEnv.Reset;
    AAgent.Reset;
    totalReward := 0;
    step := 0;

    repeat
      validActions := AEnv.GetValidActions(state);

      if Length(validActions) = 0 then
        Break;

      // Choisir parmi les actions valides
      if Random < AAgent.Epsilon then
        action := validActions[Random(Length(validActions))]
      else
      begin
        action := AAgent.ChooseAction(state);
        // Vérifier si l'action est valide
        if not AEnv.IsValidMove(action) then
          action := validActions[Random(Length(validActions))];
      end;

      nextState := AEnv.Step(action, reward, done);

      experience.State := state;
      experience.Action := action;
      experience.Reward := reward;
      experience.NextState := nextState;
      experience.Done := done;

      AAgent.Learn(experience);

      totalReward := totalReward + reward;
      state := nextState;
      Inc(step);

    until done;

    // Statistiques
    if reward > 0.9 then
      Inc(wins)
    else if reward < -0.9 then
      Inc(losses)
    else
      Inc(draws);

    AMetrics.RecordEpisode(totalReward, step);

    if episode mod 1000 = 0 then
    begin
      WriteLn(Format('Épisode %d: V=%d D=%d L=%d | ε=%.3f | Avg=%.2f',
        [episode, wins, draws, losses, AAgent.Epsilon,
         AMetrics.GetAverageReward(100)]));
      wins := 0;
      losses := 0;
      draws := 0;
    end;
  end;
end;

procedure PlayAgainstHuman(AAgent: TQLearningAgent; AEnv: TTicTacToeEnv);  
var
  state: TState;
  action, humanMove: TAction;
  reward: Double;
  done: Boolean;
  validActions: TArray<TAction>;
  i: Integer;
begin
  WriteLn;
  WriteLn('=== Jouer contre l''IA ===');
  WriteLn('Vous êtes O, l''IA est X');
  WriteLn;

  AAgent.Epsilon := 0;  // Pas d'exploration
  state := AEnv.Reset;

  repeat
    AEnv.Render;

    // Tour de l'IA (X)
    validActions := AEnv.GetValidActions(state);
    action := AAgent.ChooseAction(state);

    if not AEnv.IsValidMove(action) then
      action := validActions[Random(Length(validActions))];

    WriteLn('IA joue en position ', action);
    state := AEnv.Step(action, reward, done);

    if done then Break;

    AEnv.Render;

    // Tour du joueur (O)
    Write('Votre coup (0-8): ');
    ReadLn(humanMove);

    if not AEnv.IsValidMove(humanMove) then
    begin
      WriteLn('Mouvement invalide!');
      Continue;
    end;

    state := AEnv.Step(humanMove, reward, done);

  until done;

  AEnv.Render;

  if reward > 0.9 then
    WriteLn('L''IA a gagné!')
  else if reward < -0.9 then
    WriteLn('Vous avez gagné!')
  else
    WriteLn('Match nul!');
end;

var
  env: TTicTacToeEnv;
  agent: TQLearningAgent;
  metrics: TRLMetrics;
  choice: Char;

begin
  WriteLn('=========================================');
  WriteLn('  Tic-Tac-Toe avec Q-Learning           ');
  WriteLn('=========================================');
  WriteLn;

  Randomize;

  env := TTicTacToeEnv.Create;
  agent := TQLearningAgent.Create(
    env.GetStateCount,
    env.GetActionCount
  );
  metrics := TRLMetrics.Create('tictactoe_training.csv');

  try
    agent.LearningRate := 0.3;
    agent.DiscountFactor := 0.9;
    agent.Epsilon := 0.5;

    // Entraînement
    SelfPlay(agent, env, 10000, metrics);

    WriteLn;
    metrics.PrintSummary;
    metrics.SaveToCSV('tictactoe_results.csv');

    // Sauvegarder le modèle
    agent.SaveQTable('tictactoe_model.txt');
    WriteLn('Modèle sauvegardé dans tictactoe_model.txt');

    // Jouer contre l'humain
    WriteLn;
    Write('Voulez-vous jouer contre l''IA? (o/n): ');
    ReadLn(choice);

    if LowerCase(choice) = 'o' then
      PlayAgainstHuman(agent, env);

  finally
    metrics.Free;
    agent.Free;
    env.Free;
  end;

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Ressources et Documentation

### Livres recommandés

📚 **Fondamentaux**
- "Reinforcement Learning: An Introduction" - Sutton & Barto (LA référence)
- "Deep Reinforcement Learning Hands-On" - Maxim Lapan
- "Grokking Deep Reinforcement Learning" - Miguel Morales

📚 **Avancé**
- "Algorithms for Reinforcement Learning" - Csaba Szepesvári
- "Deep Reinforcement Learning" - Aske Plaat

### Cours en ligne

🎓 **Gratuits**
- David Silver's RL Course (UCL/DeepMind)
- Stanford CS234: Reinforcement Learning
- UC Berkeley CS285: Deep Reinforcement Learning

### Bibliothèques et Frameworks

**Python (pour référence)**
- OpenAI Gym - Environnements standards
- Stable Baselines3 - Implémentations prêtes à l'emploi
- RLlib - RL distribué

**Pour FreePascal**
- Créer vos propres environnements
- Interfacer avec des bibliothèques C/C++ (via FFI)
- Utiliser les algorithmes implémentés dans ce tutoriel

### Environnements de test

**Classiques**
- Cart-Pole
- Mountain Car
- Lunar Lander
- Atari Games

**À implémenter en Pascal**
- Grilles simples (comme nos exemples)
- Jeux de plateau (échecs, dames)
- Simulations robotiques simples

---

## Conclusion

### Ce que vous avez appris

Au cours de ce tutoriel, vous avez découvert :

✅ **Concepts fondamentaux**
- Agent, environnement, état, action, récompense
- Politique et fonction de valeur
- Exploration vs exploitation

✅ **Algorithmes principaux**
- Q-Learning (table Q)
- SARSA (on-policy)
- Policy Gradient
- Actor-Critic
- Multi-Armed Bandit

✅ **Implémentation pratique**
- Structure de données en FreePascal
- Environnements (GridWorld, Cart-Pole, Tic-Tac-Toe)
- Agents d'apprentissage
- Métriques et logging

✅ **Applications concrètes**
- Navigation dans un labyrinthe
- Équilibrage de perche
- Jeu de stratégie

✅ **Optimisation**
- Experience replay
- Target networks
- Stratégies d'exploration
- Débogage et diagnostic

### Avantages de FreePascal pour le RL

**Performance**
- Code natif compilé très rapide
- Pas de GC qui ralentit l'entraînement
- Idéal pour simulations rapides

**Déploiement**
- Exécutables standalone
- Pas de dépendances Python
- Parfait pour systèmes embarqués

**Apprentissage**
- Comprendre les algorithmes en profondeur
- Contrôle total du code
- Débogage facilité

### Limitations et solutions

**Limitations de l'approche table Q**
- Ne scale pas pour grands espaces d'états
- Solution : Utiliser des réseaux de neurones (DQN)
- Alternative : Approximation de fonction

**Manque de bibliothèques**
- Moins d'environnements préfabriqués que Python
- Solution : Créer vos propres environnements
- Alternative : Interfacer avec des libs C++

**Visualisation**
- Moins d'outils que TensorBoard
- Solution : Interface Lazarus personnalisée
- Alternative : Exporter vers CSV et utiliser Python pour viz

### Prochaines étapes

**Pour aller plus loin :**

1. **Implémenter d'autres algorithmes**
   - A3C (Asynchronous Advantage Actor-Critic)
   - PPO (Proximal Policy Optimization)
   - SAC (Soft Actor-Critic)

2. **Environnements plus complexes**
   - Jeux multijoueurs
   - Simulations physiques réalistes
   - Problèmes de contrôle continu

3. **Deep RL avec réseaux de neurones**
   - Intégrer une bibliothèque de NN
   - Implémenter DQN complet
   - Traiter des images (CNN)

4. **Applications pratiques**
   - Robotique (simulation puis réel)
   - Trading algorithmique
   - Optimisation de processus industriels
   - Personnalisation de jeux

### Message final

L'apprentissage par renforcement est l'une des branches les plus excitantes de l'IA. Avec FreePascal/Lazarus, vous disposez d'outils performants pour :

- **Apprendre** les concepts fondamentaux
- **Expérimenter** rapidement avec différents algorithmes
- **Déployer** des solutions dans des environnements contraints
- **Comprendre** en profondeur ce qui se passe sous le capot

**Points clés à retenir :**
- Le RL apprend par essai-erreur
- L'équilibre exploration/exploitation est crucial
- Les récompenses guident l'apprentissage
- La patience est nécessaire (convergence lente)
- FreePascal est excellent pour prototyper et déployer

**L'apprentissage par renforcement transforme des problèmes impossibles en problèmes solvables !**

Continuez à expérimenter, à apprendre de vos échecs (comme un agent RL !), et à construire des systèmes intelligents qui s'améliorent avec le temps.

Bon entraînement et bonne optimisation ! 🤖🎮🚀

---

*Fin du tutoriel 15.6 - Apprentissage par renforcement*

**Prochaine étape recommandée :** Intégrer le RL avec d'autres techniques (réseaux de neurones, algorithmes génétiques) pour des systèmes hybrides encore plus puissants !

⏭️ [Intégration avec Python](/15-intelligence-artificielle-machine-learning/07-integration-python.md)
