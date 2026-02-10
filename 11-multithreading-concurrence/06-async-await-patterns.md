🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.6 Async/Await patterns

## Introduction à la programmation asynchrone

La **programmation asynchrone** permet d'exécuter des opérations longues sans bloquer le programme principal. C'est particulièrement utile pour les interfaces graphiques et les opérations d'entrée/sortie (I/O).

### Qu'est-ce que Async/Await ?

**Async/Await** est un pattern de programmation qui simplifie l'écriture de code asynchrone en le faisant ressembler à du code synchrone.

**Analogie** : Imaginez que vous commandez un café. Au lieu d'attendre devant le comptoir (bloquant), vous recevez un numéro et allez vous asseoir. Quand votre café est prêt, on vous appelle (callback). Async/Await vous permet d'écrire "commandez un café, attendez qu'il soit prêt, puis buvez-le" de manière naturelle, même si en coulisse c'est asynchrone.

### Programmation synchrone vs asynchrone

#### Code synchrone (bloquant)

```pascal
// ❌ Bloque l'interface pendant 5 secondes
procedure ButtonClick(Sender: TObject);
begin
  Label1.Caption := 'Téléchargement...';
  DownloadFile('http://example.com/file.zip'); // BLOQUE ICI 5 secondes
  Label1.Caption := 'Terminé !';
  // L'interface est gelée pendant le téléchargement
end;
```

#### Code asynchrone (non-bloquant)

```pascal
// ✅ N'bloque pas l'interface
procedure ButtonClick(Sender: TObject);
begin
  Label1.Caption := 'Téléchargement...';

  // Lancer le téléchargement en arrière-plan
  DownloadFileAsync('http://example.com/file.zip',
    procedure // Callback appelé quand c'est terminé
    begin
      Label1.Caption := 'Terminé !';
    end
  );

  // L'interface reste réactive immédiatement
end;
```

### Quand utiliser l'asynchrone ?

**Utilisez l'asynchrone pour :**
- ✅ Opérations réseau (HTTP, FTP, WebSockets)
- ✅ Accès disque (lecture/écriture de gros fichiers)
- ✅ Accès base de données (requêtes longues)
- ✅ Opérations utilisateur (attendre une saisie)
- ✅ Calculs longs qui ne doivent pas bloquer l'UI

**N'utilisez PAS l'asynchrone pour :**
- ❌ Opérations très rapides (< 10ms)
- ❌ Code qui doit s'exécuter dans un ordre strict
- ❌ Calculs CPU-intensifs (utilisez le parallélisme à la place)

## Concepts fondamentaux

### Callbacks (Rappels)

Un **callback** est une fonction appelée quand une opération asynchrone se termine.

```pascal
type
  TAsyncCallback = procedure(Success: Boolean; const Data: string);

procedure DownloadAsync(const URL: string; Callback: TAsyncCallback);
var
  Thread: TThread;
begin
  Thread := TThread.CreateAnonymousThread(
    procedure
    var
      Data: string;
      Success: Boolean;
    begin
      try
        Data := HTTPGet(URL);
        Success := True;
      except
        Data := '';
        Success := False;
      end;

      // Appeler le callback dans le thread principal
      TThread.Synchronize(nil,
        procedure
        begin
          Callback(Success, Data);
        end
      );
    end
  );

  Thread.FreeOnTerminate := True;
  Thread.Start;
end;

// Utilisation
procedure TForm1.ButtonClick(Sender: TObject);
begin
  DownloadAsync('http://example.com/data.json',
    procedure(Success: Boolean; const Data: string)
    begin
      if Success then
        ShowMessage('Données : ' + Data)
      else
        ShowMessage('Erreur de téléchargement');
    end
  );
end;
```

### Promises/Futures

Une **Promise** (ou **Future**) représente le résultat futur d'une opération asynchrone.

```pascal
unit AsyncFuture;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TFutureState = (fsWaiting, fsCompleted, fsFailed);

  TFuture<T> = class
  private
    FState: TFutureState;
    FValue: T;
    FError: string;
    FEvent: TEvent;
    FCS: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetValue(const Value: T);
    procedure SetError(const Error: string);

    function Wait(Timeout: Cardinal = INFINITE): Boolean;
    function GetValue: T;
    function IsCompleted: Boolean;
    function IsFailed: Boolean;

    property State: TFutureState read FState;
    property Error: string read FError;
  end;

implementation

{ TFuture<T> }

constructor TFuture<T>.Create;
begin
  inherited Create;
  FState := fsWaiting;
  FEvent := TEvent.Create(nil, True, False, '');
  FCS := TCriticalSection.Create;
end;

destructor TFuture<T>.Destroy;
begin
  FEvent.Free;
  FCS.Free;
  inherited;
end;

procedure TFuture<T>.SetValue(const Value: T);
begin
  FCS.Enter;
  try
    FValue := Value;
    FState := fsCompleted;
    FEvent.SetEvent;
  finally
    FCS.Leave;
  end;
end;

procedure TFuture<T>.SetError(const Error: string);
begin
  FCS.Enter;
  try
    FError := Error;
    FState := fsFailed;
    FEvent.SetEvent;
  finally
    FCS.Leave;
  end;
end;

function TFuture<T>.Wait(Timeout: Cardinal): Boolean;
begin
  Result := FEvent.WaitFor(Timeout) = wrSignaled;
end;

function TFuture<T>.GetValue: T;
begin
  Wait;

  FCS.Enter;
  try
    if FState = fsFailed then
      raise Exception.Create(FError);
    Result := FValue;
  finally
    FCS.Leave;
  end;
end;

function TFuture<T>.IsCompleted: Boolean;
begin
  FCS.Enter;
  try
    Result := FState = fsCompleted;
  finally
    FCS.Leave;
  end;
end;

function TFuture<T>.IsFailed: Boolean;
begin
  FCS.Enter;
  try
    Result := FState = fsFailed;
  finally
    FCS.Leave;
  end;
end;

end.
```

### Utilisation des Futures

```pascal
uses
  AsyncFuture;

type
  TStringFuture = specialize TFuture<string>;

function DownloadAsync(const URL: string): TStringFuture;
begin
  Result := TStringFuture.Create;

  TThread.CreateAnonymousThread(
    procedure
    var
      Data: string;
    begin
      try
        Data := HTTPGet(URL);
        Result.SetValue(Data);
      except
        on E: Exception do
          Result.SetError(E.Message);
      end;
    end
  ).Start;
end;

// Utilisation
var
  Future: TStringFuture;
  Data: string;
begin
  Future := DownloadAsync('http://example.com/data.json');
  try
    // Faire autre chose pendant le téléchargement...
    WriteLn('Téléchargement en cours...');

    // Attendre le résultat (bloque jusqu'à la fin)
    Data := Future.GetValue;
    WriteLn('Données reçues : ', Data);
  finally
    Future.Free;
  end;
end;
```

## Implémentation d'Async/Await en FreePascal

FreePascal ne dispose pas d'un support natif Async/Await comme C# ou JavaScript, mais nous pouvons implémenter un pattern similaire.

> **Note** : Les unités de ce chapitre utilisent `{$mode delphi}` car elles font un usage intensif des types `reference to procedure/function` (procédures anonymes) et des types génériques. En mode ObjFPC, les procédures anonymes nécessiteraient `{$modeswitch anonymousfunctions}` (FPC 3.3.1+) et les génériques le mot-clé `generic`.

### Task asynchrone de base

```pascal
unit AsyncTask;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils;

type
  TAsyncProc = reference to procedure;
  TAsyncFunc<T> = reference to function: T;
  TAsyncCallback<T> = reference to procedure(const Value: T);
  TAsyncErrorCallback = reference to procedure(const Error: Exception);

  TTask<T> = class
  private
    FThread: TThread;
    FCompleted: Boolean;
    FResult: T;
    FError: Exception;
    FOnSuccess: TAsyncCallback<T>;
    FOnError: TAsyncErrorCallback;
    FCS: TCriticalSection;

    procedure Execute(Func: TAsyncFunc<T>);
  public
    constructor Create(Func: TAsyncFunc<T>);
    destructor Destroy; override;

    function &Then(Callback: TAsyncCallback<T>): TTask<T>;
    function Catch(ErrorHandler: TAsyncErrorCallback): TTask<T>;
    function Wait(Timeout: Cardinal = INFINITE): Boolean;
    function GetResult: T;

    property Completed: Boolean read FCompleted;
  end;

implementation

{ TTask<T> }

constructor TTask<T>.Create(Func: TAsyncFunc<T>);
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FCompleted := False;
  FError := nil;

  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      Execute(Func);
    end
  );

  FThread.FreeOnTerminate := True;
  FThread.Start;
end;

destructor TTask<T>.Destroy;
begin
  Wait;
  FCS.Free;
  if Assigned(FError) then
    FError.Free;
  inherited;
end;

procedure TTask<T>.Execute(Func: TAsyncFunc<T>);
begin
  try
    FResult := Func();

    FCS.Enter;
    try
      FCompleted := True;
    finally
      FCS.Leave;
    end;

    // Appeler le callback de succès dans le thread principal
    if Assigned(FOnSuccess) then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnSuccess(FResult);
        end
      );
  except
    on E: Exception do
    begin
      FCS.Enter;
      try
        FError := Exception.Create(E.Message);
        FCompleted := True;
      finally
        FCS.Leave;
      end;

      // Appeler le callback d'erreur dans le thread principal
      if Assigned(FOnError) then
        TThread.Synchronize(nil,
          procedure
          begin
            FOnError(FError);
          end
        );
    end;
  end;
end;

function TTask<T>.&Then(Callback: TAsyncCallback<T>): TTask<T>;
begin
  FOnSuccess := Callback;
  Result := Self;
end;

function TTask<T>.Catch(ErrorHandler: TAsyncErrorCallback): TTask<T>;
begin
  FOnError := ErrorHandler;
  Result := Self;
end;

function TTask<T>.Wait(Timeout: Cardinal): Boolean;
var
  StartTime: TDateTime;
begin
  StartTime := Now;

  while not FCompleted do
  begin
    Sleep(10);
    if (Timeout <> INFINITE) and
       (MilliSecondsBetween(Now, StartTime) > Timeout) then
      Exit(False);
  end;

  Result := True;
end;

function TTask<T>.GetResult: T;
begin
  Wait;

  FCS.Enter;
  try
    if Assigned(FError) then
      raise Exception.Create(FError.Message);
    Result := FResult;
  finally
    FCS.Leave;
  end;
end;

end.
```

### Utilisation du pattern Async/Await

```pascal
uses
  AsyncTask;

type
  TStringTask = specialize TTask<string>;
  TIntegerTask = specialize TTask<Integer>;

// Fonction asynchrone
function DownloadDataAsync(const URL: string): TStringTask;
begin
  Result := TStringTask.Create(
    function: string
    begin
      Sleep(2000); // Simuler un téléchargement
      Result := 'Données depuis ' + URL;
    end
  );
end;

// Utilisation avec chaînage
procedure TForm1.ButtonDownloadClick(Sender: TObject);
begin
  DownloadDataAsync('http://example.com/api/users')
    .&Then(
      procedure(const Data: string)
      begin
        Memo1.Lines.Add('Reçu : ' + Data);
        ShowMessage('Téléchargement terminé !');
      end
    )
    .Catch(
      procedure(const Error: Exception)
      begin
        ShowMessage('Erreur : ' + Error.Message);
      end
    );

  // L'interface reste réactive !
  ShowMessage('Téléchargement lancé en arrière-plan');
end;
```

## Patterns asynchrones courants

### 1. Sequential Async (Async séquentiel)

Exécuter plusieurs opérations asynchrones l'une après l'autre.

```pascal
type
  TAsyncChain = class
  public
    class function Sequential<T>(Tasks: array of TTask<T>): TTask<T>;
  end;

class function TAsyncChain.Sequential<T>(Tasks: array of TTask<T>): TTask<T>;
begin
  Result := TTask<T>.Create(
    function: T
    var
      i: Integer;
    begin
      for i := 0 to High(Tasks) do
      begin
        Tasks[i].Wait;
        if i = High(Tasks) then
          Result := Tasks[i].GetResult;
      end;
    end
  );
end;

// Utilisation
procedure ExecuteSequential;
var
  Task1, Task2, Task3: TStringTask;
begin
  Task1 := DownloadDataAsync('http://api1.com');
  Task2 := DownloadDataAsync('http://api2.com');
  Task3 := DownloadDataAsync('http://api3.com');

  TAsyncChain.Sequential<string>([Task1, Task2, Task3])
    .&Then(
      procedure(const FinalResult: string)
      begin
        WriteLn('Toutes les tâches terminées');
      end
    );
end;
```

### 2. Parallel Async (Async parallèle)

Exécuter plusieurs opérations asynchrones simultanément.

```pascal
type
  TAsyncParallel = class
  public
    class function WhenAll<T>(Tasks: array of TTask<T>): TTask<T>;
    class function WhenAny<T>(Tasks: array of TTask<T>): TTask<T>;
  end;

class function TAsyncParallel.WhenAll<T>(Tasks: array of TTask<T>): TTask<T>;
begin
  Result := TTask<T>.Create(
    function: T
    var
      i: Integer;
    begin
      // Attendre que toutes les tâches se terminent
      for i := 0 to High(Tasks) do
        Tasks[i].Wait;

      // Retourner le résultat de la dernière tâche
      Result := Tasks[High(Tasks)].GetResult;
    end
  );
end;

class function TAsyncParallel.WhenAny<T>(Tasks: array of TTask<T>): TTask<T>;
begin
  Result := TTask<T>.Create(
    function: T
    var
      Completed: Boolean;
      i: Integer;
    begin
      // Attendre que l'une des tâches se termine
      Completed := False;
      while not Completed do
      begin
        for i := 0 to High(Tasks) do
          if Tasks[i].Completed then
          begin
            Result := Tasks[i].GetResult;
            Completed := True;
            Break;
          end;

        if not Completed then
          Sleep(10);
      end;
    end
  );
end;

// Utilisation
procedure ExecuteParallel;
var
  Tasks: array[0..2] of TStringTask;
begin
  Tasks[0] := DownloadDataAsync('http://api1.com');
  Tasks[1] := DownloadDataAsync('http://api2.com');
  Tasks[2] := DownloadDataAsync('http://api3.com');

  // Attendre que toutes se terminent
  TAsyncParallel.WhenAll<string>(Tasks)
    .&Then(
      procedure(const Result: string)
      begin
        WriteLn('Tous les téléchargements terminés');
      end
    );
end;
```

### 3. Retry pattern (Nouvelle tentative)

Réessayer une opération en cas d'échec.

```pascal
type
  TAsyncRetry = class
  public
    class function WithRetry<T>(
      Func: TAsyncFunc<T>;
      MaxRetries: Integer;
      DelayMs: Integer): TTask<T>;
  end;

class function TAsyncRetry.WithRetry<T>(
  Func: TAsyncFunc<T>;
  MaxRetries: Integer;
  DelayMs: Integer): TTask<T>;
begin
  Result := TTask<T>.Create(
    function: T
    var
      Attempt: Integer;
      Success: Boolean;
      LastError: string;
    begin
      Success := False;

      for Attempt := 1 to MaxRetries do
      begin
        try
          Result := Func();
          Success := True;
          Break;
        except
          on E: Exception do
          begin
            LastError := E.Message;
            WriteLn(Format('Tentative %d/%d échouée : %s',
              [Attempt, MaxRetries, E.Message]));

            if Attempt < MaxRetries then
              Sleep(DelayMs);
          end;
        end;
      end;

      if not Success then
        raise Exception.Create('Échec après ' + IntToStr(MaxRetries) +
          ' tentatives. Dernière erreur : ' + LastError);
    end
  );
end;

// Utilisation
var
  Task: TStringTask;
begin
  Task := TAsyncRetry.WithRetry<string>(
    function: string
    begin
      // Opération qui peut échouer
      Result := DownloadUnreliableAPI();
    end,
    3,      // 3 tentatives maximum
    1000    // 1 seconde entre les tentatives
  );

  Task
    .&Then(
      procedure(const Data: string)
      begin
        WriteLn('Succès : ', Data);
      end
    )
    .Catch(
      procedure(const Error: Exception)
      begin
        WriteLn('Échec définitif : ', Error.Message);
      end
    );
end;
```

### 4. Timeout pattern

Abandonner une opération si elle prend trop de temps.

```pascal
type
  TAsyncTimeout = class
  public
    class function WithTimeout<T>(
      Task: TTask<T>;
      TimeoutMs: Cardinal): TTask<T>;
  end;

class function TAsyncTimeout.WithTimeout<T>(
  Task: TTask<T>;
  TimeoutMs: Cardinal): TTask<T>;
begin
  Result := TTask<T>.Create(
    function: T
    begin
      if not Task.Wait(TimeoutMs) then
        raise Exception.Create('Opération annulée : timeout dépassé');

      Result := Task.GetResult;
    end
  );
end;

// Utilisation
var
  Task: TStringTask;
  TaskWithTimeout: TStringTask;
begin
  Task := DownloadDataAsync('http://slow-api.com');

  TaskWithTimeout := TAsyncTimeout.WithTimeout<string>(Task, 5000); // 5 secondes max

  TaskWithTimeout
    .&Then(
      procedure(const Data: string)
      begin
        WriteLn('Données reçues dans les temps');
      end
    )
    .Catch(
      procedure(const Error: Exception)
      begin
        WriteLn('Timeout ou erreur : ', Error.Message);
      end
    );
end;
```

### 5. Cache asynchrone

Mettre en cache les résultats d'opérations asynchrones.

```pascal
unit AsyncCache;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, AsyncTask;

type
  TCacheEntry<T> = record
    Value: T;
    Timestamp: TDateTime;
  end;

  TAsyncCache<TKey, TValue> = class
  private
    FCache: specialize TDictionary<TKey, TCacheEntry<TValue>>;
    FCS: TCriticalSection;
    FTTLSeconds: Integer;
  public
    constructor Create(TTLSeconds: Integer = 300);
    destructor Destroy; override;

    function GetOrFetch(
      const Key: TKey;
      FetchFunc: TAsyncFunc<TValue>): specialize TTask<TValue>;
  end;

implementation

uses
  DateUtils;

{ TAsyncCache<TKey, TValue> }

constructor TAsyncCache<TKey, TValue>.Create(TTLSeconds: Integer);
begin
  inherited Create;
  FCache := specialize TDictionary<TKey, TCacheEntry<TValue>>.Create;
  FCS := TCriticalSection.Create;
  FTTLSeconds := TTLSeconds;
end;

destructor TAsyncCache<TKey, TValue>.Destroy;
begin
  FCache.Free;
  FCS.Free;
  inherited;
end;

function TAsyncCache<TKey, TValue>.GetOrFetch(
  const Key: TKey;
  FetchFunc: TAsyncFunc<TValue>): specialize TTask<TValue>;
type
  TValueTask = specialize TTask<TValue>;
var
  Entry: TCacheEntry<TValue>;
  Found: Boolean;
begin
  FCS.Enter;
  try
    Found := FCache.TryGetValue(Key, Entry);

    // Vérifier si l'entrée est valide
    if Found and (SecondsBetween(Now, Entry.Timestamp) < FTTLSeconds) then
    begin
      // Retourner la valeur en cache
      Result := TValueTask.Create(
        function: TValue
        begin
          Result := Entry.Value;
        end
      );
      Exit;
    end;
  finally
    FCS.Leave;
  end;

  // Pas en cache ou expiré : récupérer
  Result := TValueTask.Create(
    function: TValue
    begin
      Result := FetchFunc();

      // Mettre en cache
      FCS.Enter;
      try
        Entry.Value := Result;
        Entry.Timestamp := Now;
        FCache.AddOrSetValue(Key, Entry);
      finally
        FCS.Leave;
      end;
    end
  );
end;

end.
```

### Utilisation du cache asynchrone

```pascal
uses
  AsyncCache, AsyncTask;

type
  TUserCache = specialize TAsyncCache<Integer, string>;
  TStringTask = specialize TTask<string>;

var
  UserCache: TUserCache;

function FetchUserFromAPI(UserID: Integer): string;
begin
  Sleep(1000); // Simuler un appel API lent
  Result := Format('User #%d data', [UserID]);
end;

procedure LoadUser(UserID: Integer);
var
  Task: TStringTask;
begin
  Task := UserCache.GetOrFetch(UserID,
    function: string
    begin
      WriteLn('Récupération depuis l''API...');
      Result := FetchUserFromAPI(UserID);
    end
  );

  Task.&Then(
    procedure(const UserData: string)
    begin
      WriteLn('Données utilisateur : ', UserData);
    end
  );
end;

begin
  UserCache := TUserCache.Create(60); // Cache de 60 secondes
  try
    LoadUser(123); // Premier appel : va à l'API
    Sleep(100);
    LoadUser(123); // Deuxième appel : depuis le cache
  finally
    UserCache.Free;
  end;
end;
```

## Gestion des erreurs asynchrones

### Try-Catch asynchrone

```pascal
function SafeAsyncOperation<T>(Func: TAsyncFunc<T>): specialize TTask<T>;
type
  TResultTask = specialize TTask<T>;
begin
  Result := TResultTask.Create(
    function: T
    begin
      try
        Result := Func();
      except
        on E: Exception do
        begin
          // Logger l'erreur
          WriteLn('Erreur asynchrone : ', E.Message);

          // Relancer ou retourner une valeur par défaut
          raise;
        end;
      end;
    end
  );
end;
```

### Agrégation d'erreurs multiples

```pascal
type
  TAggregateException = class(Exception)
  private
    FExceptions: TList;
  public
    constructor Create(Exceptions: array of Exception);
    destructor Destroy; override;

    function GetExceptionCount: Integer;
    function GetException(Index: Integer): Exception;
  end;

function WhenAllWithErrors<T>(Tasks: array of TTask<T>): TTask<T>;
type
  TResultTask = specialize TTask<T>;
begin
  Result := TResultTask.Create(
    function: T
    var
      i: Integer;
      Errors: array of Exception;
      ErrorCount: Integer;
    begin
      ErrorCount := 0;
      SetLength(Errors, 0);

      // Attendre toutes les tâches
      for i := 0 to High(Tasks) do
      begin
        Tasks[i].Wait;

        // Collecter les erreurs
        if Tasks[i].IsFailed then
        begin
          SetLength(Errors, ErrorCount + 1);
          Errors[ErrorCount] := Exception.Create(Tasks[i].Error);
          Inc(ErrorCount);
        end;
      end;

      // Si des erreurs, les agréger
      if ErrorCount > 0 then
        raise TAggregateException.Create(Errors);

      // Sinon, retourner le dernier résultat
      Result := Tasks[High(Tasks)].GetResult;
    end
  );
end;
```

## Intégration avec l'interface graphique

### Pattern pour mise à jour d'UI

```pascal
type
  TAsyncUI = class
  public
    class procedure UpdateLabel(
      ALabel: TLabel;
      const NewText: string);

    class procedure UpdateProgress(
      AProgressBar: TProgressBar;
      Position: Integer);

    class procedure EnableControl(
      AControl: TControl;
      Enabled: Boolean);
  end;

class procedure TAsyncUI.UpdateLabel(ALabel: TLabel; const NewText: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      ALabel.Caption := NewText;
    end
  );
end;

class procedure TAsyncUI.UpdateProgress(AProgressBar: TProgressBar; Position: Integer);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      AProgressBar.Position := Position;
    end
  );
end;

class procedure TAsyncUI.EnableControl(AControl: TControl; Enabled: Boolean);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      AControl.Enabled := Enabled;
    end
  );
end;
```

### Exemple complet avec interface

```pascal
procedure TForm1.ButtonDownloadClick(Sender: TObject);
var
  Task: TStringTask;
begin
  // Désactiver le bouton
  TAsyncUI.EnableControl(ButtonDownload, False);
  TAsyncUI.UpdateLabel(LabelStatus, 'Téléchargement en cours...');
  ProgressBar1.Position := 0;

  Task := TStringTask.Create(
    function: string
    var
      i: Integer;
    begin
      // Simuler un téléchargement avec progression
      for i := 1 to 100 do
      begin
        Sleep(50);
        TAsyncUI.UpdateProgress(ProgressBar1, i);
      end;

      Result := 'Données téléchargées avec succès';
    end
  );

  Task
    .&Then(
      procedure(const Data: string)
      begin
        TAsyncUI.UpdateLabel(LabelStatus, 'Terminé !');
        TAsyncUI.EnableControl(ButtonDownload, True);
        Memo1.Lines.Add(Data);
      end
    )
    .Catch(
      procedure(const Error: Exception)
      begin
        TAsyncUI.UpdateLabel(LabelStatus, 'Erreur');
        TAsyncUI.EnableControl(ButtonDownload, True);
        ShowMessage('Erreur : ' + Error.Message);
      end
    );
end;
```

## Différences Windows/Ubuntu

Les patterns Async/Await fonctionnent de manière identique sur Windows et Ubuntu, mais il existe quelques considérations.

### Threading sur Windows

```pascal
{$IFDEF WINDOWS}
uses
  Windows;

procedure OptimizeForWindows;
begin
  // Windows utilise son thread pool natif
  // Pas de configuration spéciale nécessaire
end;
{$ENDIF}
```

### Threading sur Linux

```pascal
{$IFDEF LINUX}
uses
  BaseUnix;

procedure OptimizeForLinux;
begin
  // Linux utilise pthreads
  // Considérer les limites du système
  // ulimit -u pour voir le nombre max de threads
end;
{$ENDIF}
```

## Bonnes pratiques

### 1. Toujours libérer les ressources

```pascal
// ❌ MAUVAIS - Fuite mémoire
procedure BadAsync;
begin
  TStringTask.Create(
    function: string
    begin
      Result := 'Data';
    end
  ); // Pas de Free !
end;

// ✅ BON - Avec gestion mémoire
procedure GoodAsync;
var
  Task: TStringTask;
begin
  Task := TStringTask.Create(
    function: string
    begin
      Result := 'Data';
    end
  );

  try
    Task
      .&Then(
        procedure(const Data: string)
        begin
          WriteLn(Data);
        end
      );

    Task.Wait; // Attendre la fin
  finally
    Task.Free; // TOUJOURS libérer
  end;
end;

// ✅ ENCORE MIEUX - Avec auto-destruction
type
  TAutoTask<T> = class(TTask<T>)
  public
    constructor CreateAuto(Func: TAsyncFunc<T>);
  end;

constructor TAutoTask<T>.CreateAuto(Func: TAsyncFunc<T>);
begin
  inherited Create(Func);

  // S'auto-détruire après exécution
  FThread.OnTerminate := procedure(Sender: TObject)
  begin
    Free;
  end;
end;
```

### 2. Éviter les deadlocks

```pascal
// ❌ DANGER - Deadlock possible
procedure DeadlockRisk;
var
  Task: TStringTask;
begin
  Task := TStringTask.Create(
    function: string
    begin
      TThread.Synchronize(nil, // Attend le thread principal
        procedure
        begin
          // Code
        end
      );
      Result := 'Done';
    end
  );

  Task.Wait; // Attend le thread worker - DEADLOCK !
end;

// ✅ CORRECT - Pas de synchronisation bloquante
procedure NoDeadlock;
var
  Task: TStringTask;
begin
  Task := TStringTask.Create(
    function: string
    begin
      Result := DoWork(); // Pas de Synchronize dans le Wait
    end
  );

  Task
    .&Then(
      procedure(const Data: string)
      begin
        // Mise à jour UI ici, pas dans le thread worker
        UpdateUI(Data);
      end
    );
end;
```

### 3. Gestion appropriée des exceptions

```pascal
// ❌ MAUVAIS - Exception non gérée
procedure BadExceptionHandling;
var
  Task: TStringTask;
begin
  Task := TStringTask.Create(
    function: string
    begin
      raise Exception.Create('Erreur'); // Non capturée !
    end
  );

  WriteLn(Task.GetResult); // Va crasher l'application
end;

// ✅ BON - Exceptions gérées
procedure GoodExceptionHandling;
var
  Task: TStringTask;
begin
  Task := TStringTask.Create(
    function: string
    begin
      try
        Result := RiskyOperation();
      except
        on E: Exception do
        begin
          WriteLn('Erreur dans le thread : ', E.Message);
          raise; // Relancer pour que le callback Catch l'attrape
        end;
      end;
    end
  );

  Task
    .&Then(
      procedure(const Data: string)
      begin
        WriteLn('Succès : ', Data);
      end
    )
    .Catch(
      procedure(const Error: Exception)
      begin
        WriteLn('Erreur capturée : ', Error.Message);
      end
    );
end;
```

### 4. Annulation de tâches

```pascal
unit CancellableTask;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TCancellationToken = class
  private
    FCancelled: Boolean;
    FCS: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Cancel;
    function IsCancelled: Boolean;
    procedure ThrowIfCancelled;
  end;

  TCancellableTask<T> = class
  private
    FThread: TThread;
    FToken: TCancellationToken;
    FResult: T;
    FCompleted: Boolean;
  public
    constructor Create(Func: TAsyncFunc<T>; Token: TCancellationToken);

    function Wait: Boolean;
    function GetResult: T;
    procedure Cancel;
  end;

implementation

{ TCancellationToken }

constructor TCancellationToken.Create;
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FCancelled := False;
end;

destructor TCancellationToken.Destroy;
begin
  FCS.Free;
  inherited;
end;

procedure TCancellationToken.Cancel;
begin
  FCS.Enter;
  try
    FCancelled := True;
  finally
    FCS.Leave;
  end;
end;

function TCancellationToken.IsCancelled: Boolean;
begin
  FCS.Enter;
  try
    Result := FCancelled;
  finally
    FCS.Leave;
  end;
end;

procedure TCancellationToken.ThrowIfCancelled;
begin
  if IsCancelled then
    raise Exception.Create('Opération annulée');
end;

{ TCancellableTask<T> }

constructor TCancellableTask<T>.Create(Func: TAsyncFunc<T>; Token: TCancellationToken);
begin
  inherited Create;
  FToken := Token;
  FCompleted := False;

  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        FResult := Func();
        FCompleted := True;
      except
        FCompleted := True;
        raise;
      end;
    end
  );

  FThread.FreeOnTerminate := True;
  FThread.Start;
end;

function TCancellableTask<T>.Wait: Boolean;
begin
  while not FCompleted and not FToken.IsCancelled do
    Sleep(10);

  Result := FCompleted and not FToken.IsCancelled;
end;

function TCancellableTask<T>.GetResult: T;
begin
  Wait;
  FToken.ThrowIfCancelled;
  Result := FResult;
end;

procedure TCancellableTask<T>.Cancel;
begin
  FToken.Cancel;
end;

end.
```

### Utilisation de l'annulation

```pascal
uses
  CancellableTask;

type
  TStringCancellableTask = specialize TCancellableTask<string>;

procedure DownloadWithCancellation;
var
  Token: TCancellationToken;
  Task: TStringCancellableTask;
  i: Integer;
begin
  Token := TCancellationToken.Create;
  try
    Task := TStringCancellableTask.Create(
      function: string
      var
        j: Integer;
      begin
        for j := 1 to 100 do
        begin
          Token.ThrowIfCancelled; // Vérifier l'annulation
          Sleep(100);
          WriteLn('Progression : ', j, '%');
        end;
        Result := 'Téléchargement terminé';
      end,
      Token
    );

    // Simuler l'attente de 2 secondes puis annulation
    Sleep(2000);
    WriteLn('Annulation demandée...');
    Task.Cancel;

    try
      WriteLn(Task.GetResult);
    except
      on E: Exception do
        WriteLn('Tâche annulée : ', E.Message);
    end;
  finally
    Token.Free;
  end;
end;
```

### 5. Éviter le callback hell

```pascal
// ❌ MAUVAIS - Callback hell (pyramide de la mort)
procedure CallbackHell;
begin
  DownloadAsync('http://api1.com',
    procedure(Data1: string)
    begin
      ProcessAsync(Data1,
        procedure(Processed: string)
        begin
          SaveAsync(Processed,
            procedure(Result: Boolean)
            begin
              if Result then
                NotifyAsync('Success',
                  procedure
                  begin
                    WriteLn('Tout est terminé');
                  end
                );
            end
          );
        end
      );
    end
  );
end;

// ✅ BON - Chaînage avec Then
procedure CleanChaining;
begin
  DownloadAsync('http://api1.com')
    .&Then(
      function(const Data: string): string
      begin
        Result := ProcessData(Data);
      end
    )
    .&Then(
      function(const Processed: string): Boolean
      begin
        Result := SaveData(Processed);
      end
    )
    .&Then(
      procedure(const Success: Boolean)
      begin
        if Success then
          NotifyUser('Terminé avec succès');
      end
    );
end;
```

## Patterns avancés

### Debounce (Anti-rebond)

Éviter d'exécuter trop fréquemment une opération coûteuse.

```pascal
unit AsyncDebounce;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, ExtCtrls; // ExtCtrls pour TTimer

type
  { TProc n'existe pas en FreePascal - définition locale }
  TProc = reference to procedure;

  TDebouncer = class
  private
    FDelay: Integer;
    FTimer: TTimer;
    FAction: TProc;
    FCS: TCriticalSection;
    procedure DoTimer(Sender: TObject);
  public
    constructor Create(DelayMs: Integer);
    destructor Destroy; override;

    procedure Trigger(Action: TProc);
  end;

implementation

constructor TDebouncer.Create(DelayMs: Integer);
begin
  inherited Create;
  FDelay := DelayMs;
  FCS := TCriticalSection.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := DoTimer;
end;

destructor TDebouncer.Destroy;
begin
  FTimer.Free;
  FCS.Free;
  inherited;
end;

procedure TDebouncer.DoTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  if Assigned(FAction) then
    FAction();
end;

procedure TDebouncer.Trigger(Action: TProc);
begin
  FCS.Enter;
  try
    FAction := Action;
    FTimer.Enabled := False;
    FTimer.Interval := FDelay;
    FTimer.Enabled := True;
  finally
    FCS.Leave;
  end;
end;

end.
```

### Utilisation du debounce

```pascal
uses
  AsyncDebounce;

var
  SearchDebouncer: TDebouncer;

procedure TForm1.EditSearchChange(Sender: TObject);
begin
  // Attendre 500ms après la dernière frappe avant de chercher
  SearchDebouncer.Trigger(
    procedure
    begin
      PerformSearch(EditSearch.Text);
    end
  );
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SearchDebouncer := TDebouncer.Create(500); // 500ms de délai
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SearchDebouncer.Free;
end;
```

### Throttle (Limitation de fréquence)

Limiter le nombre d'exécutions dans un intervalle de temps.

```pascal
unit AsyncThrottle;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TProc = reference to procedure;

  TThrottler = class
  private
    FInterval: Integer;
    FLastExecution: TDateTime;
    FPending: Boolean;
  public
    constructor Create(IntervalMs: Integer);

    procedure Execute(Action: TProc);
  end;

implementation

constructor TThrottler.Create(IntervalMs: Integer);
begin
  inherited Create;
  FInterval := IntervalMs;
  FLastExecution := 0;
  FPending := False;
end;

procedure TThrottler.Execute(Action: TProc);
var
  Elapsed: Integer;
begin
  Elapsed := MilliSecondsBetween(Now, FLastExecution);

  if Elapsed >= FInterval then
  begin
    // Exécuter immédiatement
    Action();
    FLastExecution := Now;
    FPending := False;
  end
  else if not FPending then
  begin
    // Programmer l'exécution après le délai
    FPending := True;
    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(FInterval - Elapsed);
        Action();
        FLastExecution := Now;
        FPending := False;
      end
    ).Start;
  end;
end;

end.
```

### Rate Limiting (Limitation du débit)

Limiter le nombre de requêtes par période.

```pascal
unit AsyncRateLimit;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections;

type
  TProc = reference to procedure;

  TRateLimiter = class
  private
    FMaxRequests: Integer;
    FTimeWindow: Integer;
    FRequestTimes: TQueue<TDateTime>;
    FCS: TCriticalSection;
  public
    constructor Create(MaxRequests: Integer; TimeWindowMs: Integer);
    destructor Destroy; override;

    function TryExecute(Action: TProc): Boolean;
    procedure ExecuteOrWait(Action: TProc);
  end;

implementation

uses
  DateUtils;

constructor TRateLimiter.Create(MaxRequests: Integer; TimeWindowMs: Integer);
begin
  inherited Create;
  FMaxRequests := MaxRequests;
  FTimeWindow := TimeWindowMs;
  FRequestTimes := TQueue<TDateTime>.Create;
  FCS := TCriticalSection.Create;
end;

destructor TRateLimiter.Destroy;
begin
  FRequestTimes.Free;
  FCS.Free;
  inherited;
end;

function TRateLimiter.TryExecute(Action: TProc): Boolean;
var
  Now: TDateTime;
  Cutoff: TDateTime;
begin
  Now := SysUtils.Now;
  Cutoff := IncMilliSecond(Now, -FTimeWindow);

  FCS.Enter;
  try
    // Supprimer les anciennes requêtes
    while (FRequestTimes.Count > 0) and (FRequestTimes.Peek < Cutoff) do
      FRequestTimes.Dequeue;

    // Vérifier si on peut exécuter
    if FRequestTimes.Count < FMaxRequests then
    begin
      FRequestTimes.Enqueue(Now);
      Result := True;
      Action();
    end
    else
      Result := False;
  finally
    FCS.Leave;
  end;
end;

procedure TRateLimiter.ExecuteOrWait(Action: TProc);
var
  Executed: Boolean;
begin
  Executed := False;

  while not Executed do
  begin
    Executed := TryExecute(Action);
    if not Executed then
      Sleep(100); // Attendre un peu avant de réessayer
  end;
end;

end.
```

### Utilisation du rate limiter

```pascal
uses
  AsyncRateLimit;

var
  APILimiter: TRateLimiter;

procedure CallAPI(const Endpoint: string);
begin
  // Limiter à 10 requêtes par seconde
  APILimiter.ExecuteOrWait(
    procedure
    begin
      HTTPGet('http://api.com/' + Endpoint);
    end
  );
end;

begin
  APILimiter := TRateLimiter.Create(10, 1000); // 10 req/sec
  try
    // Ces appels seront automatiquement limités
    CallAPI('users');
    CallAPI('posts');
    CallAPI('comments');
  finally
    APILimiter.Free;
  end;
end;
```

## Cas d'usage réels

### 1. Téléchargement de fichier avec progression

```pascal
unit AsyncDownloader;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient;

type
  TProgressCallback = reference to procedure(BytesReceived, TotalBytes: Int64);
  TCompleteCallback = reference to procedure(Success: Boolean; const FileName: string);

  TAsyncDownloader = class
  public
    class procedure DownloadFile(
      const URL: string;
      const DestFile: string;
      OnProgress: TProgressCallback;
      OnComplete: TCompleteCallback);
  end;

implementation

class procedure TAsyncDownloader.DownloadFile(
  const URL: string;
  const DestFile: string;
  OnProgress: TProgressCallback;
  OnComplete: TCompleteCallback);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      Client: TFPHTTPClient;
      Stream: TFileStream;
      Success: Boolean;
    begin
      Success := False;
      Client := TFPHTTPClient.Create(nil);
      Stream := nil;

      try
        Stream := TFileStream.Create(DestFile, fmCreate);

        // Note : OnDataReceived est un event of object, on ne peut pas
        // y assigner une procédure anonyme. On utilise le téléchargement
        // direct et on met à jour la progression après.
        Client.Get(URL, Stream);
        Success := True;
      except
        on E: Exception do
        begin
          WriteLn('Erreur de téléchargement : ', E.Message);
          Success := False;
        end;
      end;

      // Callback de fin
      if Assigned(OnComplete) then
        TThread.Synchronize(nil,
          procedure
          begin
            OnComplete(Success, DestFile);
          end
        );

      Stream.Free;
      Client.Free;
    end
  ).Start;
end;

end.
```

### Utilisation du téléchargeur

```pascal
procedure TForm1.ButtonDownloadClick(Sender: TObject);
begin
  TAsyncDownloader.DownloadFile(
    'http://example.com/largefile.zip',
    'C:\Temp\download.zip',

    // Callback de progression
    procedure(BytesReceived, TotalBytes: Int64)
    begin
      ProgressBar1.Max := TotalBytes;
      ProgressBar1.Position := BytesReceived;
      LabelStatus.Caption := Format('Téléchargé : %d / %d octets',
        [BytesReceived, TotalBytes]);
    end,

    // Callback de fin
    procedure(Success: Boolean; const FileName: string)
    begin
      if Success then
        ShowMessage('Téléchargement terminé : ' + FileName)
      else
        ShowMessage('Échec du téléchargement');

      ButtonDownload.Enabled := True;
    end
  );

  ButtonDownload.Enabled := False;
end;
```

### 2. Recherche incrémentale asynchrone

```pascal
unit AsyncSearch;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TSearchResult = record
    Title: string;
    Description: string;
    Score: Double;
  end;

  TSearchResults = specialize TList<TSearchResult>;
  TSearchCallback = reference to procedure(const Results: TSearchResults);

  TAsyncSearch = class
  private
    class var FCurrentSearch: TThread;
  public
    class procedure Search(
      const Query: string;
      OnResults: TSearchCallback);

    class procedure CancelSearch;
  end;

implementation

class procedure TAsyncSearch.Search(
  const Query: string;
  OnResults: TSearchCallback);
begin
  // Annuler la recherche précédente
  CancelSearch;

  // Lancer une nouvelle recherche
  FCurrentSearch := TThread.CreateAnonymousThread(
    procedure
    var
      Results: TSearchResults;
      Item: TSearchResult;
    begin
      Sleep(500); // Simuler une recherche réseau

      if TThread.CurrentThread.Terminated then
        Exit;

      // Simuler des résultats
      Results := TSearchResults.Create;
      try
        Item.Title := 'Résultat 1';
        Item.Description := 'Description pour : ' + Query;
        Item.Score := 0.95;
        Results.Add(Item);

        Item.Title := 'Résultat 2';
        Item.Description := 'Autre résultat pour : ' + Query;
        Item.Score := 0.87;
        Results.Add(Item);

        // Retourner les résultats
        if not TThread.CurrentThread.Terminated then
          TThread.Synchronize(nil,
            procedure
            begin
              OnResults(Results);
            end
          );
      finally
        Results.Free;
      end;
    end
  );

  FCurrentSearch.FreeOnTerminate := True;
  FCurrentSearch.Start;
end;

class procedure TAsyncSearch.CancelSearch;
begin
  if Assigned(FCurrentSearch) then
  begin
    FCurrentSearch.Terminate;
    FCurrentSearch := nil;
  end;
end;

end.
```

### 3. Queue de tâches asynchrones

```pascal
unit AsyncQueue;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections;

type
  TAsyncTask = reference to procedure;

  TAsyncTaskQueue = class;

  { Thread worker dédié (CreateAnonymousThread ne peut
    pas prendre une méthode d'objet) }
  TQueueWorkerThread = class(TThread)
  private
    FQueue: TAsyncTaskQueue;
  protected
    procedure Execute; override;
  public
    constructor Create(AQueue: TAsyncTaskQueue);
  end;

  TAsyncTaskQueue = class
  private
    FQueue: TThreadedQueue<TAsyncTask>;
    FWorkers: array of TQueueWorkerThread;
    FRunning: Boolean;
  public
    constructor Create(WorkerCount: Integer = 4);
    destructor Destroy; override;

    procedure Enqueue(Task: TAsyncTask);
    procedure Start;
    procedure Stop;
    function GetQueueSize: Integer;
  end;

implementation

constructor TAsyncTaskQueue.Create(WorkerCount: Integer);
var
  i: Integer;
begin
  inherited Create;
  FQueue := TThreadedQueue<TAsyncTask>.Create(1000, INFINITE, INFINITE);
  SetLength(FWorkers, WorkerCount);
  FRunning := False;
end;

destructor TAsyncTaskQueue.Destroy;
begin
  Stop;
  FQueue.Free;
  inherited;
end;

{ TQueueWorkerThread }

constructor TQueueWorkerThread.Create(AQueue: TAsyncTaskQueue);
begin
  inherited Create(False);
  FQueue := AQueue;
  FreeOnTerminate := False;
end;

procedure TQueueWorkerThread.Execute;
var
  Task: TAsyncTask;
begin
  while FQueue.FRunning do
  begin
    if FQueue.FQueue.PopItem(Task) = wrSignaled then
    begin
      try
        Task();
      except
        on E: Exception do
          WriteLn('Erreur dans la tâche : ', E.Message);
      end;
    end;
  end;
end;

{ TAsyncTaskQueue }

procedure TAsyncTaskQueue.Enqueue(Task: TAsyncTask);
begin
  FQueue.PushItem(Task);
end;

procedure TAsyncTaskQueue.Start;
var
  i: Integer;
begin
  FRunning := True;

  for i := 0 to High(FWorkers) do
    FWorkers[i] := TQueueWorkerThread.Create(Self);
end;

procedure TAsyncTaskQueue.Stop;
var
  i: Integer;
begin
  FRunning := False;

  for i := 0 to High(FWorkers) do
  begin
    if Assigned(FWorkers[i]) then
    begin
      FWorkers[i].WaitFor;
      FWorkers[i].Free;
    end;
  end;
end;

function TAsyncTaskQueue.GetQueueSize: Integer;
begin
  Result := FQueue.TotalItemsPushed - FQueue.TotalItemsPopped;
end;

end.
```

### Utilisation de la queue de tâches

```pascal
uses
  AsyncQueue;

var
  TaskQueue: TAsyncTaskQueue;

procedure ProcessFiles;
var
  Files: TStringList;
  i: Integer;
begin
  Files := TStringList.Create;
  try
    FindAllFiles(Files, 'C:\Data', '*.txt', True);

    for i := 0 to Files.Count - 1 do
    begin
      TaskQueue.Enqueue(
        procedure
        var
          FileName: string;
        begin
          FileName := Files[i];
          WriteLn('Traitement de ', FileName);
          ProcessFile(FileName);
        end
      );
    end;
  finally
    Files.Free;
  end;
end;

begin
  TaskQueue := TAsyncTaskQueue.Create(4); // 4 workers
  try
    TaskQueue.Start;

    ProcessFiles;

    // Attendre que toutes les tâches soient terminées
    while TaskQueue.GetQueueSize > 0 do
      Sleep(100);

  finally
    TaskQueue.Free;
  end;
end;
```

## Tests et debugging

### Simuler des opérations asynchrones

```pascal
unit AsyncMock;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAsyncMock = class
  public
    class function SimulateDelay<T>(
      const Value: T;
      DelayMs: Integer): specialize TTask<T>;

    class function SimulateError<T>(
      const ErrorMsg: string;
      DelayMs: Integer): specialize TTask<T>;
  end;

implementation

class function TAsyncMock.SimulateDelay<T>(
  const Value: T;
  DelayMs: Integer): specialize TTask<T>;
type
  TResultTask = specialize TTask<T>;
begin
  Result := TResultTask.Create(
    function: T
    begin
      Sleep(DelayMs);
      Result := Value;
    end
  );
end;

class function TAsyncMock.SimulateError<T>(
  const ErrorMsg: string;
  DelayMs: Integer): specialize TTask<T>;
type
  TResultTask = specialize TTask<T>;
begin
  Result := TResultTask.Create(
    function: T
    begin
      Sleep(DelayMs);
      raise Exception.Create(ErrorMsg);
    end
  );
end;

end.
```

### Tests unitaires

```pascal
procedure TestAsyncOperation;
var
  Task: TStringTask;
  Result: string;
begin
  WriteLn('Test : Opération asynchrone réussie');

  Task := TAsyncMock.SimulateDelay<string>('Test réussi', 100);
  try
    Result := Task.GetResult;
    Assert(Result = 'Test réussi', 'Résultat incorrect');
    WriteLn('✓ Test réussi');
  finally
    Task.Free;
  end;
end;

procedure TestAsyncError;
var
  Task: TStringTask;
  ErrorCaught: Boolean;
begin
  WriteLn('Test : Gestion d''erreur');

  Task := TAsyncMock.SimulateError<string>('Erreur simulée', 100);
  ErrorCaught := False;

  try
    Task.GetResult;
  except
    on E: Exception do
    begin
      ErrorCaught := True;
      Assert(Pos('Erreur simulée', E.Message) > 0, 'Message d''erreur incorrect');
    end;
  end;

  Assert(ErrorCaught, 'Exception non levée');
  WriteLn('✓ Test réussi');

  Task.Free;
end;
```

## Résumé

Les **patterns Async/Await** simplifient grandement la programmation asynchrone :

**Concepts clés :**
- **Callbacks** : Fonctions appelées à la fin d'une opération
- **Promises/Futures** : Représentations du résultat futur
- **Tasks** : Encapsulation d'opérations asynchrones
- **Chaînage** : Composer plusieurs opérations avec `Then`

**Patterns essentiels :**
- Sequential/Parallel execution
- Retry with backoff
- Timeout handling
- Cancellation tokens
- Debounce/Throttle
- Rate limiting

**Bonnes pratiques :**
1. ✅ Toujours libérer les ressources
2. ✅ Gérer les exceptions correctement
3. ✅ Éviter les deadlocks (pas de Synchronize dans Wait)
4. ✅ Utiliser l'annulation pour les longues opérations
5. ✅ Éviter le callback hell avec le chaînage

**Cas d'usage :**
- Téléchargements avec progression
- Recherche incrémentale
- Appels API avec retry
- Traitement de fichiers en arrière-plan
- Opérations base de données

Bien que FreePascal n'ait pas de support natif Async/Await comme C# ou JavaScript, les patterns présentés ici permettent d'écrire du code asynchrone propre et maintenable !

⏭️ [Différences de scheduling Windows/Linux](/11-multithreading-concurrence/07-differences-scheduling-windows-linux.md)
