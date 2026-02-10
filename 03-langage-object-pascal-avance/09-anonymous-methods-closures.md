🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Anonymous methods et closures en FreePascal/Lazarus

## Introduction : Qu'est-ce qu'une méthode anonyme ?

Une méthode anonyme est une fonction ou procédure sans nom que vous pouvez définir directement là où vous en avez besoin. C'est comme écrire une recette de cuisine sur un post-it au lieu de créer un livre de recettes complet.

Imaginez que vous voulez trier une liste. Au lieu de créer une fonction de comparaison séparée avec un nom, vous pouvez écrire la logique de comparaison directement dans l'appel de tri :

```pascal
// Méthode traditionnelle
function CompareNumbers(A, B: Integer): Integer;
begin
  Result := A - B;
end;
List.Sort(@CompareNumbers);

// Avec méthode anonyme
List.Sort(function(A, B: Integer): Integer
          begin
            Result := A - B;
          end);
```

Une **closure** est une méthode anonyme qui "capture" des variables de son environnement. C'est comme si la fonction emportait avec elle un sac à dos contenant les variables dont elle a besoin.

## Syntaxe de base des méthodes anonymes

### Déclaration de types pour méthodes anonymes

```pascal
{$mode objfpc}
{$modeswitch anonymousfunctions}  // Activer les fonctions anonymes
{$modeswitch functionreferences}   // Activer les références de fonctions

uses
  SysUtils;

type
  // Types de référence pour méthodes anonymes
  TSimpleProc = reference to procedure;
  TSimpleFunc = reference to function: Integer;
  TIntegerProc = reference to procedure(Value: Integer);
  TIntegerFunc = reference to function(Value: Integer): Integer;
  TCompareFunc = reference to function(const A, B: Integer): Integer;
  TPredicateFunc = reference to function(Value: Integer): Boolean;

  // Types génériques
  generic TAction<T> = reference to procedure(const Item: T);
  generic TFunc<T, TResult> = reference to function(const Item: T): TResult;
  generic TPredicate<T> = reference to function(const Item: T): Boolean;
```

### Création et utilisation simple

```pascal
procedure SimpleAnonymousExample;
var
  MyProc: TSimpleProc;
  MyFunc: TSimpleFunc;
  DoubleFunc: TIntegerFunc;
begin
  // Méthode anonyme sans paramètres
  MyProc := procedure
            begin
              WriteLn('Bonjour depuis une méthode anonyme !');
            end;

  // Appel de la méthode
  MyProc();

  // Fonction anonyme retournant une valeur
  MyFunc := function: Integer
            begin
              Result := Random(100);
            end;

  WriteLn('Nombre aléatoire : ', MyFunc());

  // Fonction avec paramètre
  DoubleFunc := function(Value: Integer): Integer
                begin
                  Result := Value * 2;
                end;

  WriteLn('10 * 2 = ', DoubleFunc(10));
end;

// Passage de méthodes anonymes en paramètres
procedure RepeatAction(Count: Integer; Action: TSimpleProc);
var
  I: Integer;
begin
  for I := 1 to Count do
    Action();
end;

procedure UseRepeatAction;
var
  Counter: Integer;
begin
  Counter := 0;

  RepeatAction(5, procedure
                  begin
                    Inc(Counter);
                    WriteLn('Exécution ', Counter);
                  end);

  WriteLn('Total d''exécutions : ', Counter);
end;
```

## Closures : Capture de variables

### Comprendre la capture de variables

```pascal
procedure ClosureBasicExample;
var
  OuterValue: Integer;
  MyFunc: TIntegerFunc;
begin
  OuterValue := 10;

  // Cette méthode anonyme "capture" OuterValue
  MyFunc := function(X: Integer): Integer
            begin
              Result := X + OuterValue;  // Utilise la variable externe
            end;

  WriteLn('5 + 10 = ', MyFunc(5));  // Affiche 15

  // Modifier la variable capturée
  OuterValue := 20;
  WriteLn('5 + 20 = ', MyFunc(5));  // Affiche 25
end;

// Exemple plus complexe de closure
function CreateMultiplier(Factor: Integer): TIntegerFunc;
begin
  // La closure capture 'Factor'
  Result := function(Value: Integer): Integer
            begin
              Result := Value * Factor;
            end;
end;

procedure UseMultiplier;
var
  Double, Triple: TIntegerFunc;
begin
  Double := CreateMultiplier(2);
  Triple := CreateMultiplier(3);

  WriteLn('10 * 2 = ', Double(10));  // 20
  WriteLn('10 * 3 = ', Triple(10));  // 30

  // Chaque closure a sa propre copie de Factor
end;

// Closure avec plusieurs variables capturées
procedure MultipleCaptures;
var
  Name: string;
  Age: Integer;
  PrintInfo: TSimpleProc;
begin
  Name := 'Alice';
  Age := 25;

  PrintInfo := procedure
               begin
                 WriteLn('Nom : ', Name);
                 WriteLn('Âge : ', Age);
                 WriteLn('Dans 10 ans : ', Age + 10);
               end;

  PrintInfo();

  // Modification des variables
  Name := 'Bob';
  Age := 30;

  WriteLn('--- Après modification ---');
  PrintInfo();  // Utilise les nouvelles valeurs
end;
```

### Piège des closures en boucles

```pascal
procedure LoopClosureProblem;
var
  Funcs: array[0..4] of TIntegerFunc;
  I, J: Integer;
begin
  // PROBLÈME : Toutes les closures partagent la même variable I
  for I := 0 to 4 do
  begin
    Funcs[I] := function(X: Integer): Integer
                begin
                  Result := X + I;  // I est capturé par référence
                end;
  end;

  // Toutes afficheront la même valeur (5) !
  for J := 0 to 4 do
    WriteLn('Func[', J, '](10) = ', Funcs[J](10));  // Tous affichent 15
end;

procedure LoopClosureSolution;
var
  Funcs: array[0..4] of TIntegerFunc;
  I, J: Integer;

  function MakeClosure(Value: Integer): TIntegerFunc;
  begin
    // Value est local à MakeClosure, donc chaque closure a sa copie
    Result := function(X: Integer): Integer
              begin
                Result := X + Value;
              end;
  end;

begin
  // SOLUTION : Utiliser une fonction helper
  for I := 0 to 4 do
    Funcs[I] := MakeClosure(I);

  // Maintenant chaque fonction a sa propre valeur
  for J := 0 to 4 do
    WriteLn('Func[', J, '](10) = ', Funcs[J](10));  // 10, 11, 12, 13, 14
end;
```

## Méthodes anonymes avec collections

### Traitement de listes

```pascal
type
  TIntegerList = class
  private
    FItems: array of Integer;
    FCount: Integer;
  public
    procedure Add(Value: Integer);
    procedure Clear;

    // Méthodes utilisant des méthodes anonymes
    procedure ForEach(Action: TIntegerProc);
    function Find(Predicate: TPredicateFunc): Integer;
    function FindAll(Predicate: TPredicateFunc): TIntegerList;
    function Map(Transform: TIntegerFunc): TIntegerList;
    function Reduce(Accumulator: TCompareFunc; Initial: Integer): Integer;
    procedure Sort(Compare: TCompareFunc);

    property Count: Integer read FCount;
  end;

procedure TIntegerList.Add(Value: Integer);
begin
  if FCount = Length(FItems) then
    SetLength(FItems, FCount * 2 + 4);
  FItems[FCount] := Value;
  Inc(FCount);
end;

procedure TIntegerList.Clear;
begin
  FCount := 0;
  SetLength(FItems, 0);
end;

procedure TIntegerList.ForEach(Action: TIntegerProc);
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    Action(FItems[I]);
end;

function TIntegerList.Find(Predicate: TPredicateFunc): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if Predicate(FItems[I]) then
    begin
      Result := FItems[I];
      Break;
    end;
end;

function TIntegerList.FindAll(Predicate: TPredicateFunc): TIntegerList;
var
  I: Integer;
begin
  Result := TIntegerList.Create;
  for I := 0 to FCount - 1 do
    if Predicate(FItems[I]) then
      Result.Add(FItems[I]);
end;

function TIntegerList.Map(Transform: TIntegerFunc): TIntegerList;
var
  I: Integer;
begin
  Result := TIntegerList.Create;
  for I := 0 to FCount - 1 do
    Result.Add(Transform(FItems[I]));
end;

function TIntegerList.Reduce(Accumulator: TCompareFunc; Initial: Integer): Integer;
var
  I: Integer;
begin
  Result := Initial;
  for I := 0 to FCount - 1 do
    Result := Accumulator(Result, FItems[I]);
end;

procedure TIntegerList.Sort(Compare: TCompareFunc);
var
  I, J, Temp: Integer;
begin
  // Simple bubble sort pour la démo
  for I := 0 to FCount - 2 do
    for J := 0 to FCount - I - 2 do
      if Compare(FItems[J], FItems[J + 1]) > 0 then
      begin
        Temp := FItems[J];
        FItems[J] := FItems[J + 1];
        FItems[J + 1] := Temp;
      end;
end;

// Utilisation
procedure UseListWithClosures;
var
  List, Evens, Doubled: TIntegerList;
  Sum, Product: Integer;
  Threshold: Integer;
begin
  List := TIntegerList.Create;
  try
    // Ajouter des éléments
    List.Add(5);
    List.Add(2);
    List.Add(8);
    List.Add(3);
    List.Add(7);
    List.Add(1);

    // ForEach : afficher tous les éléments
    WriteLn('Éléments de la liste :');
    List.ForEach(procedure(Value: Integer)
                 begin
                   Write(Value, ' ');
                 end);
    WriteLn;

    // Find : trouver le premier élément > 5
    WriteLn('Premier élément > 5 : ',
            List.Find(function(Value: Integer): Boolean
                      begin
                        Result := Value > 5;
                      end));

    // FindAll : trouver tous les nombres pairs
    Evens := List.FindAll(function(Value: Integer): Boolean
                          begin
                            Result := (Value mod 2) = 0;
                          end);
    WriteLn('Nombres pairs :');
    Evens.ForEach(procedure(Value: Integer)
                  begin
                    Write(Value, ' ');
                  end);
    WriteLn;

    // Map : doubler tous les éléments
    Doubled := List.Map(function(Value: Integer): Integer
                        begin
                          Result := Value * 2;
                        end);
    WriteLn('Éléments doublés :');
    Doubled.ForEach(procedure(Value: Integer)
                    begin
                      Write(Value, ' ');
                    end);
    WriteLn;

    // Reduce : calculer la somme
    Sum := List.Reduce(function(A, B: Integer): Integer
                       begin
                         Result := A + B;
                       end, 0);
    WriteLn('Somme : ', Sum);

    // Reduce : calculer le produit
    Product := List.Reduce(function(A, B: Integer): Integer
                           begin
                             Result := A * B;
                           end, 1);
    WriteLn('Produit : ', Product);

    // Sort : tri croissant
    List.Sort(function(A, B: Integer): Integer
              begin
                Result := A - B;
              end);
    WriteLn('Liste triée (croissant) :');
    List.ForEach(procedure(Value: Integer)
                 begin
                   Write(Value, ' ');
                 end);
    WriteLn;

    // Sort avec closure capturant une variable
    Threshold := 4;
    List.Sort(function(A, B: Integer): Integer
              begin
                // Mettre les valeurs < Threshold en premier
                if (A < Threshold) and (B >= Threshold) then
                  Result := -1
                else if (A >= Threshold) and (B < Threshold) then
                  Result := 1
                else
                  Result := A - B;
              end);
    WriteLn('Liste triée (< ', Threshold, ' en premier) :');
    List.ForEach(procedure(Value: Integer)
                 begin
                   Write(Value, ' ');
                 end);
    WriteLn;

  finally
    List.Free;
    Evens.Free;
    Doubled.Free;
  end;
end;
```

## Callbacks et gestionnaires d'événements

### Système d'événements avec closures

```pascal
type
  TEventHandler = reference to procedure(Sender: TObject);
  TMouseEventHandler = reference to procedure(X, Y: Integer; Button: Integer);
  TKeyEventHandler = reference to procedure(Key: Char);

  TButton = class
  private
    FCaption: string;
    FOnClick: TEventHandler;
    FEnabled: Boolean;
  public
    constructor Create(const ACaption: string);

    procedure Click;

    property Caption: string read FCaption write FCaption;
    property OnClick: TEventHandler read FOnClick write FOnClick;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

constructor TButton.Create(const ACaption: string);
begin
  FCaption := ACaption;
  FEnabled := True;
end;

procedure TButton.Click;
begin
  if FEnabled and Assigned(FOnClick) then
  begin
    WriteLn('[', FCaption, '] clicked');
    FOnClick(Self);
  end;
end;

// Gestionnaire d'événements multiple
type
  TMulticastEvent = class
  private
    FHandlers: array of TEventHandler;
  public
    procedure Add(Handler: TEventHandler);
    procedure Remove(Handler: TEventHandler);
    procedure Clear;
    procedure Trigger(Sender: TObject);
  end;

procedure TMulticastEvent.Add(Handler: TEventHandler);
var
  Len: Integer;
begin
  Len := Length(FHandlers);
  SetLength(FHandlers, Len + 1);
  FHandlers[Len] := Handler;
end;

procedure TMulticastEvent.Clear;
begin
  SetLength(FHandlers, 0);
end;

procedure TMulticastEvent.Trigger(Sender: TObject);
var
  Handler: TEventHandler;
begin
  for Handler in FHandlers do
    if Assigned(Handler) then
      Handler(Sender);
end;

// Utilisation
procedure UseEventHandlers;
var
  Button1, Button2: TButton;
  ClickCount: Integer;
  LastClicked: string;
  MultiEvent: TMulticastEvent;
begin
  Button1 := TButton.Create('OK');
  Button2 := TButton.Create('Cancel');
  MultiEvent := TMulticastEvent.Create;
  try
    ClickCount := 0;
    LastClicked := '';

    // Handler avec closure capturant des variables
    Button1.OnClick := procedure(Sender: TObject)
                       begin
                         Inc(ClickCount);
                         LastClicked := TButton(Sender).Caption;
                         WriteLn('Click #', ClickCount, ' sur ', LastClicked);
                       end;

    // Handler différent pour Button2
    Button2.OnClick := procedure(Sender: TObject)
                       begin
                         WriteLn('Annulation !');
                         Button1.Enabled := False;
                       end;

    // Événements multiples
    MultiEvent.Add(procedure(Sender: TObject)
                   begin
                     WriteLn('Handler 1 appelé');
                   end);

    MultiEvent.Add(procedure(Sender: TObject)
                   begin
                     WriteLn('Handler 2 appelé');
                   end);

    // Simulation de clics
    Button1.Click;
    Button1.Click;
    Button2.Click;
    Button1.Click; // Ne fait rien car désactivé

    WriteLn('--- Déclenchement multi-événement ---');
    MultiEvent.Trigger(nil);

  finally
    Button1.Free;
    Button2.Free;
    MultiEvent.Free;
  end;
end;
```

## Programmation asynchrone avec closures

### Simulation de tâches asynchrones

```pascal
type
  TAsyncCallback = reference to procedure(Result: Variant);
  TAsyncErrorCallback = reference to procedure(Error: string);

  TAsyncTask = class
  private
    FOnComplete: TAsyncCallback;
    FOnError: TAsyncErrorCallback;
  public
    procedure Execute; virtual; abstract;
    procedure Then_(Callback: TAsyncCallback);
    procedure Catch(ErrorCallback: TAsyncErrorCallback);
  protected
    procedure Complete(Result: Variant);
    procedure Error(const Msg: string);
  end;

procedure TAsyncTask.Then_(Callback: TAsyncCallback);
begin
  FOnComplete := Callback;
end;

procedure TAsyncTask.Catch(ErrorCallback: TAsyncErrorCallback);
begin
  FOnError := ErrorCallback;
end;

procedure TAsyncTask.Complete(Result: Variant);
begin
  if Assigned(FOnComplete) then
    FOnComplete(Result);
end;

procedure TAsyncTask.Error(const Msg: string);
begin
  if Assigned(FOnError) then
    FOnError(Msg);
end;

// Tâche de chargement simulée
type
  TLoadDataTask = class(TAsyncTask)
  private
    FUrl: string;
  public
    constructor Create(const AUrl: string);
    procedure Execute; override;
  end;

constructor TLoadDataTask.Create(const AUrl: string);
begin
  FUrl := AUrl;
end;

procedure TLoadDataTask.Execute;
begin
  WriteLn('Chargement depuis ', FUrl, '...');

  // Simulation d'un délai
  Sleep(100);

  if Random(10) > 2 then
    Complete('Données chargées depuis ' + FUrl)
  else
    Error('Erreur de connexion à ' + FUrl);
end;

// Promise-like pattern
type
  TPromise = class
  private
    FValue: Variant;
    FIsResolved: Boolean;
    FIsRejected: Boolean;
    FError: string;
    FThenCallbacks: array of TAsyncCallback;
    FCatchCallbacks: array of TAsyncErrorCallback;
  public
    procedure Resolve(Value: Variant);
    procedure Reject(const Error: string);
    function Then_(Callback: TAsyncCallback): TPromise;
    function Catch(ErrorCallback: TAsyncErrorCallback): TPromise;
  end;

procedure TPromise.Resolve(Value: Variant);
var
  Callback: TAsyncCallback;
begin
  if FIsResolved or FIsRejected then
    Exit;

  FValue := Value;
  FIsResolved := True;

  for Callback in FThenCallbacks do
    if Assigned(Callback) then
      Callback(FValue);
end;

procedure TPromise.Reject(const Error: string);
var
  ErrorCallback: TAsyncErrorCallback;
begin
  if FIsResolved or FIsRejected then
    Exit;

  FError := Error;
  FIsRejected := True;

  for ErrorCallback in FCatchCallbacks do
    if Assigned(ErrorCallback) then
      ErrorCallback(FError);
end;

function TPromise.Then_(Callback: TAsyncCallback): TPromise;
var
  Len: Integer;
begin
  Result := Self;

  if FIsResolved then
    Callback(FValue)
  else if not FIsRejected then
  begin
    Len := Length(FThenCallbacks);
    SetLength(FThenCallbacks, Len + 1);
    FThenCallbacks[Len] := Callback;
  end;
end;

function TPromise.Catch(ErrorCallback: TAsyncErrorCallback): TPromise;
var
  Len: Integer;
begin
  Result := Self;

  if FIsRejected then
    ErrorCallback(FError)
  else if not FIsResolved then
  begin
    Len := Length(FCatchCallbacks);
    SetLength(FCatchCallbacks, Len + 1);
    FCatchCallbacks[Len] := ErrorCallback;
  end;
end;

// Utilisation
procedure UseAsyncPatterns;
var
  Task: TLoadDataTask;
  Promise: TPromise;
  DataCache: string;
begin
  DataCache := '';

  // Pattern callback simple
  Task := TLoadDataTask.Create('http://api.example.com/data');
  try
    Task.Then_(procedure(Result: Variant)
               begin
                 DataCache := string(Result);
                 WriteLn('Succès : ', DataCache);
               end);

    Task.Catch(procedure(Error: string)
               begin
                 WriteLn('Erreur : ', Error);
               end);

    Task.Execute;
  finally
    Task.Free;
  end;

  // Pattern Promise
  Promise := TPromise.Create;
  try
    Promise
      .Then_(procedure(Result: Variant)
             begin
               WriteLn('Promise résolue : ', string(Result));
             end)
      .Catch(procedure(Error: string)
             begin
               WriteLn('Promise rejetée : ', Error);
             end);

    // Simulation d'une opération asynchrone
    if Random(2) = 0 then
      Promise.Resolve('Opération réussie')
    else
      Promise.Reject('Opération échouée');

  finally
    Promise.Free;
  end;
end;
```

## Générateurs et itérateurs

### Implémentation de générateurs avec closures

```pascal
type
  TIntegerGenerator = reference to function: Integer;
  TGeneratorState = reference to function(out Value: Integer): Boolean;

// Générateur de séquence
function CreateRangeGenerator(Start, Stop, Step: Integer): TGeneratorState;
var
  Current: Integer;
begin
  Current := Start;

  Result := function(out Value: Integer): Boolean
            begin
              if (Step > 0) and (Current <= Stop) then
              begin
                Value := Current;
                Current := Current + Step;
                Result := True;
              end
              else if (Step < 0) and (Current >= Stop) then
              begin
                Value := Current;
                Current := Current + Step;
                Result := True;
              end
              else
                Result := False;
            end;
end;

// Générateur de Fibonacci
function CreateFibonacciGenerator(MaxValue: Integer): TGeneratorState;
var
  Prev, Curr: Integer;
begin
  Prev := 0;
  Curr := 1;

  Result := function(out Value: Integer): Boolean
            var
              Next: Integer;
            begin
              if Curr <= MaxValue then
              begin
                Value := Curr;
                Next := Prev + Curr;
                Prev := Curr;
                Curr := Next;
                Result := True;
              end
              else
                Result := False;
            end;
end;

// Générateur avec filtre
function CreateFilteredGenerator(Source: TGeneratorState;
                                Filter: TPredicateFunc): TGeneratorState;
begin
  Result := function(out Value: Integer): Boolean
            begin
              Result := False;
              while Source(Value) do
              begin
                if Filter(Value) then
                begin
                  Result := True;
                  Break;
                end;
              end;
            end;
end;

// Générateur avec transformation
function CreateMappedGenerator(Source: TGeneratorState;
                              Transform: TIntegerFunc): TGeneratorState;
begin
  Result := function(out Value: Integer): Boolean
            var
              SourceValue: Integer;
            begin
              Result := Source(SourceValue);
              if Result then
                Value := Transform(SourceValue);
            end;
end;

// Utilisation
procedure UseGenerators;
var
  Gen: TGeneratorState;
  Value: Integer;
  Count: Integer;
begin
  WriteLn('Génération de 1 à 10 par pas de 2 :');
  Gen := CreateRangeGenerator(1, 10, 2);
  while Gen(Value) do
    Write(Value, ' ');
  WriteLn;

  WriteLn('Nombres de Fibonacci < 100 :');
  Gen := CreateFibonacciGenerator(100);
  while Gen(Value) do
    Write(Value, ' ');
  WriteLn;

  WriteLn('Nombres pairs de 1 à 20 :');
  Gen := CreateFilteredGenerator(
    CreateRangeGenerator(1, 20, 1),
    function(X: Integer): Boolean
    begin
      Result := (X mod 2) = 0;
    end
  );
  while Gen(Value) do
    Write(Value, ' ');
  WriteLn;

  WriteLn('Carrés des nombres de 1 à 5 :');
  Gen := CreateMappedGenerator(
    CreateRangeGenerator(1, 5, 1),
    function(X: Integer): Integer
    begin
      Result := X * X;
    end
  );
  while Gen(Value) do
    Write(Value, ' ');
  WriteLn;

  // Générateur complexe : carrés des nombres pairs < 10
  WriteLn('Carrés des nombres pairs < 10 :');
  Gen := CreateMappedGenerator(
    CreateFilteredGenerator(
      CreateRangeGenerator(1, 10, 1),
      function(X: Integer): Boolean
      begin
        Result := (X mod 2) = 0;
      end
    ),
    function(X: Integer): Integer
    begin
      Result := X * X;
    end
  );

  Count := 0;
  while Gen(Value) do
  begin
    Write(Value, ' ');
    Inc(Count);
  end;
  WriteLn('(', Count, ' valeurs)');
end;
```

## Mémoïsation avec closures

### Cache automatique des résultats

```pascal
type
  TIntToIntFunc = reference to function(N: Integer): Integer;

// Créer une version mémoïsée d'une fonction
function Memoize(Func: TIntToIntFunc): TIntToIntFunc;
var
  Cache: array of record
    Key: Integer;
    Value: Integer;
    Valid: Boolean;
  end;
begin
  SetLength(Cache, 100); // Cache simple de taille fixe

  Result := function(N: Integer): Integer
            var
              I: Integer;
              Hash: Integer;
            begin
              // Recherche dans le cache
              Hash := N mod Length(Cache);

              if Cache[Hash].Valid and (Cache[Hash].Key = N) then
              begin
                WriteLn('  [Cache hit pour ', N, ']');
                Result := Cache[Hash].Value;
              end
              else
              begin
                WriteLn('  [Calcul pour ', N, ']');
                Result := Func(N);
                Cache[Hash].Key := N;
                Cache[Hash].Value := Result;
                Cache[Hash].Valid := True;
              end;
            end;
end;

// Fibonacci avec mémoïsation
function CreateMemoizedFibonacci: TIntToIntFunc;
var
  FibFunc: TIntToIntFunc;
begin
  FibFunc := nil; // Déclaration préalable pour la récursion

  FibFunc := function(N: Integer): Integer
             begin
               if N <= 1 then
                 Result := N
               else
                 Result := FibFunc(N - 1) + FibFunc(N - 2);
             end;

  Result := Memoize(FibFunc);
end;

// Factorielle avec mémoïsation
function CreateMemoizedFactorial: TIntToIntFunc;
var
  FactFunc: TIntToIntFunc;
begin
  FactFunc := function(N: Integer): Integer
              var
                I: Integer;
              begin
                Result := 1;
                for I := 2 to N do
                  Result := Result * I;
              end;

  Result := Memoize(FactFunc);
end;

// Utilisation
procedure UseMemoization;
var
  Fib, Fact: TIntToIntFunc;
begin
  WriteLn('=== Fibonacci avec mémoïsation ===');
  Fib := CreateMemoizedFibonacci;

  WriteLn('Fib(5) = ', Fib(5));
  WriteLn('Fib(5) = ', Fib(5));  // Utilise le cache
  WriteLn('Fib(6) = ', Fib(6));
  WriteLn('Fib(4) = ', Fib(4));  // Déjà en cache

  WriteLn;
  WriteLn('=== Factorielle avec mémoïsation ===');
  Fact := CreateMemoizedFactorial;

  WriteLn('Fact(5) = ', Fact(5));
  WriteLn('Fact(5) = ', Fact(5));  // Cache hit
  WriteLn('Fact(6) = ', Fact(6));
  WriteLn('Fact(4) = ', Fact(4));  // Cache hit
end;
```

## Patterns fonctionnels avancés

### Currying et application partielle

```pascal
type
  TBinaryOp = reference to function(A, B: Integer): Integer;
  TUnaryOp = reference to function(X: Integer): Integer;
  TCurriedOp = reference to function(A: Integer): TUnaryOp;

// Transformer une fonction binaire en fonction curryfiée
function Curry(Op: TBinaryOp): TCurriedOp;
begin
  Result := function(A: Integer): TUnaryOp
            begin
              Result := function(B: Integer): Integer
                        begin
                          Result := Op(A, B);
                        end;
            end;
end;

// Application partielle
function PartialApply(Op: TBinaryOp; FirstArg: Integer): TUnaryOp;
begin
  Result := function(SecondArg: Integer): Integer
            begin
              Result := Op(FirstArg, SecondArg);
            end;
end;

// Composition de fonctions
function Compose(F, G: TUnaryOp): TUnaryOp;
begin
  Result := function(X: Integer): Integer
            begin
              Result := F(G(X));
            end;
end;

// Pipeline de fonctions
type
  TPipeline = class
  private
    FOperations: array of TUnaryOp;
  public
    procedure Add(Op: TUnaryOp);
    function Execute(Value: Integer): Integer;
    function ToPipeline: TUnaryOp;
  end;

procedure TPipeline.Add(Op: TUnaryOp);
var
  Len: Integer;
begin
  Len := Length(FOperations);
  SetLength(FOperations, Len + 1);
  FOperations[Len] := Op;
end;

function TPipeline.Execute(Value: Integer): Integer;
var
  Op: TUnaryOp;
begin
  Result := Value;
  for Op in FOperations do
    Result := Op(Result);
end;

function TPipeline.ToPipeline: TUnaryOp;
var
  Ops: array of TUnaryOp;
  I: Integer;
begin
  // Copier les opérations pour la closure
  SetLength(Ops, Length(FOperations));
  for I := 0 to High(FOperations) do
    Ops[I] := FOperations[I];

  Result := function(X: Integer): Integer
            var
              Op: TUnaryOp;
            begin
              Result := X;
              for Op in Ops do
                Result := Op(Result);
            end;
end;

// Utilisation
procedure UseFunctionalPatterns;
var
  Add, Multiply: TBinaryOp;
  CurriedAdd: TCurriedOp;
  Add5, MultiplyBy3, Double: TUnaryOp;
  Pipeline: TPipeline;
  ProcessData: TUnaryOp;
begin
  // Opérations de base
  Add := function(A, B: Integer): Integer
         begin
           Result := A + B;
         end;

  Multiply := function(A, B: Integer): Integer
              begin
                Result := A * B;
              end;

  // Currying
  CurriedAdd := Curry(Add);
  Add5 := CurriedAdd(5);  // Fonction qui ajoute 5
  WriteLn('10 + 5 = ', Add5(10));

  // Application partielle
  MultiplyBy3 := PartialApply(Multiply, 3);
  WriteLn('7 * 3 = ', MultiplyBy3(7));

  // Composition
  Double := PartialApply(Multiply, 2);
  ProcessData := Compose(Add5, Double);  // (x * 2) + 5
  WriteLn('(10 * 2) + 5 = ', ProcessData(10));

  // Pipeline
  Pipeline := TPipeline.Create;
  try
    Pipeline.Add(Double);       // ×2
    Pipeline.Add(Add5);         // +5
    Pipeline.Add(MultiplyBy3);  // ×3

    WriteLn('Pipeline(4) = ((4 * 2) + 5) * 3 = ', Pipeline.Execute(4));

    // Convertir en fonction
    ProcessData := Pipeline.ToPipeline;
    WriteLn('ProcessData(4) = ', ProcessData(4));
  finally
    Pipeline.Free;
  end;
end;
```

### Monades simples avec closures

```pascal
type
  // Maybe monade pour gérer les valeurs optionnelles
  generic TMaybe<T> = record
  private
    FHasValue: Boolean;
    FValue: T;
  public
    class function Some(Value: T): TMaybe<T>; static;
    class function None: TMaybe<T>; static;

    function Map<U>(Transform: specialize TFunc<T, U>): specialize TMaybe<U>;
    function FlatMap<U>(Transform: specialize TFunc<T, specialize TMaybe<U>>): specialize TMaybe<U>;
    function OrElse(DefaultValue: T): T;

    property HasValue: Boolean read FHasValue;
    property Value: T read FValue;
  end;

  TMaybeInt = specialize TMaybe<Integer>;
  TIntToMaybeInt = reference to function(X: Integer): TMaybeInt;

class function TMaybe<T>.Some(Value: T): TMaybe<T>;
begin
  Result.FHasValue := True;
  Result.FValue := Value;
end;

class function TMaybe<T>.None: TMaybe<T>;
begin
  Result.FHasValue := False;
  Result.FValue := Default(T);
end;

function TMaybe<T>.Map<U>(Transform: specialize TFunc<T, U>): specialize TMaybe<U>;
begin
  if FHasValue then
    Result := specialize TMaybe<U>.Some(Transform(FValue))
  else
    Result := specialize TMaybe<U>.None;
end;

function TMaybe<T>.OrElse(DefaultValue: T): T;
begin
  if FHasValue then
    Result := FValue
  else
    Result := DefaultValue;
end;

// Exemple d'utilisation avec division sûre
function SafeDivide(A, B: Integer): TMaybeInt;
begin
  if B <> 0 then
    Result := TMaybeInt.Some(A div B)
  else
    Result := TMaybeInt.None;
end;

// Chaînage d'opérations
procedure UseMaybeMonad;
var
  Result: TMaybeInt;
  Value: Integer;
begin
  // Division sûre
  Result := SafeDivide(10, 2);
  if Result.HasValue then
    WriteLn('10 / 2 = ', Result.Value)
  else
    WriteLn('Division impossible');

  // Division par zéro
  Result := SafeDivide(10, 0);
  Value := Result.OrElse(-1);
  WriteLn('10 / 0 = ', Value, ' (valeur par défaut)');

  // Chaînage avec Map
  Result := SafeDivide(20, 4)
    .Map<Integer>(function(X: Integer): Integer
                  begin
                    Result := X * 2;
                  end);

  if Result.HasValue then
    WriteLn('(20 / 4) * 2 = ', Result.Value);
end;
```

## Gestion de l'état avec closures

### Créer des objets avec closures

```pascal
type
  TCounterInterface = record
    Increment: TSimpleProc;
    Decrement: TSimpleProc;
    GetValue: TSimpleFunc;
    Reset: TSimpleProc;
  end;

// Factory pour créer un compteur encapsulé
function CreateCounter(InitialValue: Integer = 0): TCounterInterface;
var
  Value: Integer;
begin
  Value := InitialValue;

  Result.Increment := procedure
                       begin
                         Inc(Value);
                       end;

  Result.Decrement := procedure
                       begin
                         Dec(Value);
                       end;

  Result.GetValue := function: Integer
                      begin
                        Result := Value;
                      end;

  Result.Reset := procedure
                   begin
                     Value := InitialValue;
                   end;
end;

// État partagé entre plusieurs closures
type
  TBankAccount = record
    Deposit: TIntegerProc;
    Withdraw: TIntegerFunc;
    GetBalance: TSimpleFunc;
    GetHistory: reference to function: string;
  end;

// Nécessite : uses Classes;  (pour TStringList)
function CreateBankAccount(InitialBalance: Integer): TBankAccount;
var
  Balance: Integer;
  History: TStringList;
begin
  Balance := InitialBalance;
  History := TStringList.Create;
  History.Add(Format('Compte créé avec %d€', [InitialBalance]));

  Result.Deposit := procedure(Amount: Integer)
                     begin
                       if Amount > 0 then
                       begin
                         Inc(Balance, Amount);
                         History.Add(Format('Dépôt: +%d€ (Solde: %d€)',
                                          [Amount, Balance]));
                       end;
                     end;

  Result.Withdraw := function(Amount: Integer): Integer
                      begin
                        if (Amount > 0) and (Amount <= Balance) then
                        begin
                          Dec(Balance, Amount);
                          History.Add(Format('Retrait: -%d€ (Solde: %d€)',
                                           [Amount, Balance]));
                          Result := Amount;
                        end
                        else
                        begin
                          History.Add(Format('Retrait refusé: %d€ (Solde insuffisant: %d€)',
                                           [Amount, Balance]));
                          Result := 0;
                        end;
                      end;

  Result.GetBalance := function: Integer
                        begin
                          Result := Balance;
                        end;

  Result.GetHistory := function: string
                        begin
                          Result := History.Text;
                        end;
end;

// Utilisation
procedure UseStatefulClosures;
var
  Counter1, Counter2: TCounterInterface;
  Account: TBankAccount;
  Withdrawn: Integer;
begin
  // Compteurs indépendants
  Counter1 := CreateCounter(0);
  Counter2 := CreateCounter(100);

  Counter1.Increment();
  Counter1.Increment();
  Counter2.Decrement();

  WriteLn('Counter1 : ', Counter1.GetValue());  // 2
  WriteLn('Counter2 : ', Counter2.GetValue());  // 99

  Counter1.Reset();
  WriteLn('Counter1 après reset : ', Counter1.GetValue());  // 0

  // Compte bancaire
  WriteLn;
  WriteLn('=== Compte bancaire ===');
  Account := CreateBankAccount(1000);

  Account.Deposit(500);
  WriteLn('Solde après dépôt : ', Account.GetBalance(), '€');

  Withdrawn := Account.Withdraw(200);
  WriteLn('Retiré : ', Withdrawn, '€');
  WriteLn('Solde : ', Account.GetBalance(), '€');

  Withdrawn := Account.Withdraw(2000);  // Échec
  WriteLn('Tentative de retrait de 2000€ : ', Withdrawn, '€ retirés');

  WriteLn;
  WriteLn('Historique :');
  WriteLn(Account.GetHistory());
end;
```

## Décorateurs et middleware

### Pattern décorateur avec closures

```pascal
type
  TRequestHandler = reference to function(const Request: string): string;

// Décorateur de logging
function WithLogging(Handler: TRequestHandler): TRequestHandler;
begin
  Result := function(const Request: string): string
            begin
              WriteLn('[LOG] Requête : ', Request);
              Result := Handler(Request);
              WriteLn('[LOG] Réponse : ', Result);
            end;
end;

// Décorateur d'authentification
function WithAuth(Handler: TRequestHandler; const Token: string): TRequestHandler;
begin
  Result := function(const Request: string): string
            begin
              if Pos('AUTH:' + Token, Request) > 0 then
                Result := Handler(Request)
              else
                Result := 'ERREUR: Non autorisé';
            end;
end;

// Décorateur de cache
function WithCache(Handler: TRequestHandler): TRequestHandler;
var
  Cache: TStringList;
begin
  Cache := TStringList.Create;

  Result := function(const Request: string): string
            var
              Index: Integer;
            begin
              Index := Cache.IndexOfName(Request);
              if Index >= 0 then
              begin
                WriteLn('[CACHE] Hit pour : ', Request);
                Result := Cache.ValueFromIndex[Index];
              end
              else
              begin
                WriteLn('[CACHE] Miss pour : ', Request);
                Result := Handler(Request);
                Cache.Add(Request + '=' + Result);
              end;
            end;
end;

// Décorateur de limitation de débit
function WithRateLimit(Handler: TRequestHandler; MaxCalls: Integer): TRequestHandler;
var
  CallCount: Integer;
begin
  CallCount := 0;

  Result := function(const Request: string): string
            begin
              Inc(CallCount);
              if CallCount <= MaxCalls then
                Result := Handler(Request)
              else
                Result := 'ERREUR: Limite de débit atteinte';
            end;
end;

// Middleware chain
type
  TMiddlewareChain = class
  private
    FHandler: TRequestHandler;
  public
    constructor Create(Handler: TRequestHandler);
    function Use(Middleware: reference to function(Handler: TRequestHandler): TRequestHandler): TMiddlewareChain;
    function Build: TRequestHandler;
  end;

constructor TMiddlewareChain.Create(Handler: TRequestHandler);
begin
  FHandler := Handler;
end;

function TMiddlewareChain.Use(Middleware: reference to function(Handler: TRequestHandler): TRequestHandler): TMiddlewareChain;
begin
  FHandler := Middleware(FHandler);
  Result := Self;
end;

function TMiddlewareChain.Build: TRequestHandler;
begin
  Result := FHandler;
end;

// Utilisation
procedure UseDecorators;
var
  BaseHandler, DecoratedHandler: TRequestHandler;
  Chain: TMiddlewareChain;
  Response: string;
begin
  // Handler de base
  BaseHandler := function(const Request: string): string
                 begin
                   Result := 'Réponse à : ' + Request;
                 end;

  // Décoration manuelle
  DecoratedHandler := WithLogging(WithAuth(BaseHandler, 'SECRET'));

  Response := DecoratedHandler('AUTH:SECRET Obtenir données');
  WriteLn('Résultat : ', Response);
  WriteLn;

  Response := DecoratedHandler('Obtenir données');  // Échec auth
  WriteLn('Résultat : ', Response);
  WriteLn;

  // Utilisation de la chaîne de middleware
  Chain := TMiddlewareChain.Create(BaseHandler);
  try
    DecoratedHandler := Chain
      .Use(function(H: TRequestHandler): TRequestHandler
           begin
             Result := WithLogging(H);
           end)
      .Use(function(H: TRequestHandler): TRequestHandler
           begin
             Result := WithCache(H);
           end)
      .Use(function(H: TRequestHandler): TRequestHandler
           begin
             Result := WithRateLimit(H, 3);
           end)
      .Build;

    WriteLn('=== Test avec chaîne de middleware ===');
    WriteLn('Appel 1:');
    Response := DecoratedHandler('Test1');
    WriteLn;

    WriteLn('Appel 2 (cache hit):');
    Response := DecoratedHandler('Test1');
    WriteLn;

    WriteLn('Appel 3:');
    Response := DecoratedHandler('Test2');
    WriteLn;

    WriteLn('Appel 4 (limite atteinte):');
    Response := DecoratedHandler('Test3');
    WriteLn('Résultat : ', Response);

  finally
    Chain.Free;
  end;
end;
```

## Gestion des ressources avec closures

### RAII pattern avec closures

```pascal
type
  TResourceAction = reference to procedure(Resource: TObject);
  TFileAction = reference to procedure(F: TextFile);

// Gestion automatique de ressources
procedure WithResource(ResourceClass: TClass; Action: TResourceAction);
var
  Resource: TObject;
begin
  Resource := ResourceClass.Create;
  try
    Action(Resource);
  finally
    Resource.Free;
  end;
end;

// Gestion de fichiers
procedure WithFile(const FileName: string; Action: TFileAction);
var
  F: TextFile;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    Action(F);
  finally
    CloseFile(F);
  end;
end;

// Transaction avec rollback automatique
type
  TTransaction = class
  private
    FCommitted: Boolean;
    FRollbackAction: TSimpleProc;
  public
    constructor Create(RollbackAction: TSimpleProc);
    destructor Destroy; override;
    procedure Commit;
  end;

constructor TTransaction.Create(RollbackAction: TSimpleProc);
begin
  FCommitted := False;
  FRollbackAction := RollbackAction;
end;

destructor TTransaction.Destroy;
begin
  if not FCommitted and Assigned(FRollbackAction) then
  begin
    WriteLn('Transaction rollback...');
    FRollbackAction();
  end;
  inherited;
end;

procedure TTransaction.Commit;
begin
  FCommitted := True;
  WriteLn('Transaction committed');
end;

procedure WithTransaction(Action: reference to procedure(Trans: TTransaction));
var
  Trans: TTransaction;
  OriginalData: string;
begin
  OriginalData := 'État initial';

  Trans := TTransaction.Create(
    procedure
    begin
      WriteLn('Restauration : ', OriginalData);
    end
  );

  try
    Action(Trans);
  finally
    Trans.Free;
  end;
end;

// Utilisation
procedure UseResourceManagement;
begin
  // Gestion automatique de TStringList
  WithResource(TStringList,
    procedure(Resource: TObject)
    var
      List: TStringList;
    begin
      List := TStringList(Resource);
      List.Add('Ligne 1');
      List.Add('Ligne 2');
      WriteLn('Liste créée avec ', List.Count, ' lignes');
    end
  );

  // Gestion de fichier
  WithFile('test.txt',
    procedure(F: TextFile)
    begin
      WriteLn(F, 'Contenu du fichier');
      WriteLn(F, 'Ligne 2');
      WriteLn('Fichier écrit');
    end
  );

  // Transaction avec succès
  WriteLn('=== Transaction réussie ===');
  WithTransaction(
    procedure(Trans: TTransaction)
    begin
      WriteLn('Exécution de la transaction...');
      // Opérations...
      Trans.Commit;
    end
  );

  // Transaction avec échec (rollback automatique)
  WriteLn;
  WriteLn('=== Transaction échouée ===');
  WithTransaction(
    procedure(Trans: TTransaction)
    begin
      WriteLn('Exécution de la transaction...');
      // Simulation d'erreur - pas de Commit
      WriteLn('Erreur simulée !');
    end
  );
end;
```

## Limitations et considérations

### Gestion de la mémoire

```pascal
// ATTENTION : Les closures peuvent créer des références circulaires
type
  TNode = class
  private
    FValue: Integer;
    FOnProcess: TSimpleProc;
  public
    property Value: Integer read FValue write FValue;
    property OnProcess: TSimpleProc read FOnProcess write FOnProcess;
  end;

procedure MemoryLeakExample;
var
  Node: TNode;
begin
  Node := TNode.Create;
  try
    Node.Value := 42;

    // PROBLÈME : La closure capture Node, créant une référence circulaire
    Node.OnProcess := procedure
                      begin
                        WriteLn('Valeur : ', Node.Value);
                      end;

    // La closure maintient une référence à Node
    // Node maintient une référence à la closure
    // => Fuite mémoire potentielle
  finally
    Node.Free;  // Ne libère pas complètement la mémoire
  end;
end;

// SOLUTION : Utiliser une référence faible ou éviter la capture
procedure MemoryLeakSolution;
var
  Node: TNode;
  NodeValue: Integer;
begin
  Node := TNode.Create;
  try
    Node.Value := 42;
    NodeValue := Node.Value;  // Copier la valeur

    // La closure capture NodeValue, pas Node
    Node.OnProcess := procedure
                      begin
                        WriteLn('Valeur : ', NodeValue);
                      end;
  finally
    Node.Free;  // Libération correcte
  end;
end;
```

### Performance

```pascal
// Les closures ont un coût en performance
procedure PerformanceConsiderations;
var
  DirectCall, ClosureCall: Int64;  // Nécessite : uses DateUtils;  (pour MilliSecondsBetween)
  StartTime: TDateTime;
  I: Integer;
  Func: TIntegerFunc;
begin
  // Appel direct de fonction
  StartTime := Now;
  for I := 1 to 1000000 do
    Sqr(I);
  DirectCall := MilliSecondsBetween(Now, StartTime);

  // Appel via closure
  Func := function(X: Integer): Integer
          begin
            Result := X * X;
          end;

  StartTime := Now;
  for I := 1 to 1000000 do
    Func(I);
  ClosureCall := MilliSecondsBetween(Now, StartTime);

  WriteLn('Appel direct : ', DirectCall, ' ms');
  WriteLn('Appel closure : ', ClosureCall, ' ms');
  WriteLn('Ratio : ', (ClosureCall / DirectCall):0:2, 'x plus lent');
end;
```

## Conclusion

Les méthodes anonymes et les closures sont des outils puissants en FreePascal/Lazarus qui apportent des capacités de programmation fonctionnelle au langage. Voici les points essentiels à retenir :

### Avantages des méthodes anonymes et closures

1. **Code plus concis** : Pas besoin de déclarer des fonctions séparées
2. **Encapsulation** : Les closures créent des portées privées
3. **Flexibilité** : Permettent des patterns fonctionnels avancés
4. **Callbacks naturels** : Idéal pour les gestionnaires d'événements
5. **État local** : Capture de variables pour maintenir un état

### Cas d'usage recommandés

✅ **Utilisez les closures pour** :
- Callbacks et gestionnaires d'événements
- Opérations sur collections (map, filter, reduce)
- Patterns fonctionnels (currying, composition)
- Encapsulation d'état local
- Middleware et décorateurs
- Gestion de ressources (RAII)

### Limitations et pièges

⚠️ **Attention à** :
- Références circulaires et fuites mémoire
- Performance (overhead par rapport aux fonctions directes)
- Capture de variables en boucles
- Complexité du code si surutilisé
- Débogage plus difficile

### Bonnes pratiques

1. **Simplicité** : Gardez les closures courtes et simples
2. **Documentation** : Commentez l'intention des closures complexes
3. **Mémoire** : Évitez les captures circulaires
4. **Performance** : Mesurez l'impact dans les sections critiques
5. **Lisibilité** : N'abusez pas - parfois une fonction nommée est plus claire

### Configuration requise

Pour utiliser les méthodes anonymes en FreePascal :

```pascal
{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}
```

Les méthodes anonymes et closures modernisent significativement FreePascal, permettant d'écrire du code plus expressif et plus flexible. Utilisées judicieusement, elles améliorent la qualité et la maintenabilité du code.

⏭️ [Coroutines et programmation asynchrone](/03-langage-object-pascal-avance/10-coroutines-programmation-asynchrone.md)
