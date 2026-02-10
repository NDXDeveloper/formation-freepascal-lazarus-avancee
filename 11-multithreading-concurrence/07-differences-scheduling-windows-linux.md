🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.7 Différences de scheduling Windows/Linux (Partie 2)

## Bonnes pratiques multi-plateformes (suite)

### 1. Ne pas dépendre de priorités absolues (suite)

```pascal
// ❌ MAUVAIS - Dépendant de la plateforme
{$IFDEF WINDOWS}
SetThreadPriority(Handle, 15); // Valeur Windows spécifique
{$ENDIF}

// ✅ BON - Abstraction portable
type
  TAppPriority = (apBackground, apNormal, apImportant);

procedure SetApplicationPriority(Priority: TAppPriority);  
begin
  case Priority of
    apBackground:
    begin
      {$IFDEF WINDOWS}
      SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_BELOW_NORMAL);
      {$ENDIF}
      {$IFDEF LINUX}
      FpNice(10);
      {$ENDIF}
    end;
    apNormal:
    begin
      {$IFDEF WINDOWS}
      SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_NORMAL);
      {$ENDIF}
      {$IFDEF LINUX}
      FpNice(0);
      {$ENDIF}
    end;
    apImportant:
    begin
      {$IFDEF WINDOWS}
      SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_ABOVE_NORMAL);
      {$ENDIF}
      {$IFDEF LINUX}
      if FpNice(-5) = -1 then
        WriteLn('Attention : privilèges insuffisants pour priorité élevée');
      {$ENDIF}
    end;
  end;
end;
```

### 2. Gérer les échecs de changement de priorité

```pascal
function TrySetHighPriority: Boolean;  
begin
  Result := True;

  {$IFDEF WINDOWS}
  // Windows : généralement fonctionne toujours
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_HIGHEST);
  {$ENDIF}

  {$IFDEF LINUX}
  // Linux : peut échouer sans privilèges
  if FpNice(-10) = -1 then
  begin
    Result := False;
    WriteLn('Impossible d''augmenter la priorité.');
    WriteLn('Lancez avec sudo ou ajustez les limites (ulimit).');
  end;
  {$ENDIF}
end;

// Utilisation
procedure ConfigureApplication;  
begin
  if not TrySetHighPriority then
  begin
    WriteLn('L''application fonctionnera avec la priorité par défaut.');
    // Continuer quand même avec priorité normale
  end;
end;
```

### 3. Tester sur les deux plateformes

```pascal
unit CrossPlatformTest;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TPlatformTest = class
  public
    class procedure TestScheduling;
    class procedure TestContextSwitchCost;
    class procedure TestPriorityEffects;
  end;

implementation

class procedure TPlatformTest.TestScheduling;  
var
  StartTime: TDateTime;
  Iterations: Int64;
begin
  WriteLn('=== Test de scheduling ===');
  {$IFDEF WINDOWS}
  WriteLn('Plateforme : Windows');
  {$ENDIF}
  {$IFDEF LINUX}
  WriteLn('Plateforme : Linux');
  {$ENDIF}

  StartTime := Now;
  Iterations := 0;

  // Boucle pendant 1 seconde
  while MilliSecondsBetween(Now, StartTime) < 1000 do
  begin
    Inc(Iterations);
    if Iterations mod 10000 = 0 then
      Sleep(0); // Yield
  end;

  WriteLn('Itérations effectuées : ', Iterations);
  WriteLn('Context switches estimés : ', Iterations div 10000);
end;

class procedure TPlatformTest.TestContextSwitchCost;  
var
  Thread1, Thread2: TThread;
  StartTime, EndTime: TDateTime;
  Counter1, Counter2: Int64;
begin
  WriteLn('=== Coût des changements de contexte ===');

  Counter1 := 0;
  Counter2 := 0;

  StartTime := Now;

  Thread1 := TThread.CreateAnonymousThread(
    procedure
    begin
      while MilliSecondsBetween(Now, StartTime) < 1000 do
      begin
        InterlockedExchangeAdd64(Counter1, 1);
        Sleep(0);
      end;
    end
  );

  Thread2 := TThread.CreateAnonymousThread(
    procedure
    begin
      while MilliSecondsBetween(Now, StartTime) < 1000 do
      begin
        InterlockedExchangeAdd64(Counter2, 1);
        Sleep(0);
      end;
    end
  );

  Thread1.FreeOnTerminate := False;
  Thread2.FreeOnTerminate := False;
  Thread1.Start;
  Thread2.Start;
  Thread1.WaitFor;
  Thread2.WaitFor;

  EndTime := Now;

  WriteLn('Thread 1 : ', Counter1, ' itérations');
  WriteLn('Thread 2 : ', Counter2, ' itérations');
  WriteLn('Total : ', Counter1 + Counter2);
  WriteLn('Context switches/sec : ', (Counter1 + Counter2));

  Thread1.Free;
  Thread2.Free;
end;

class procedure TPlatformTest.TestPriorityEffects;  
var
  HighPriority, LowPriority: TThread;
  CountHigh, CountLow: Int64;
  StartTime: TDateTime;
begin
  WriteLn('=== Impact des priorités ===');

  CountHigh := 0;
  CountLow := 0;
  StartTime := Now;

  HighPriority := TThread.CreateAnonymousThread(
    procedure
    begin
      while MilliSecondsBetween(Now, StartTime) < 2000 do
        InterlockedExchangeAdd64(CountHigh, 1);
    end
  );
  HighPriority.Priority := tpHigher;

  LowPriority := TThread.CreateAnonymousThread(
    procedure
    begin
      while MilliSecondsBetween(Now, StartTime) < 2000 do
        InterlockedExchangeAdd64(CountLow, 1);
    end
  );
  LowPriority.Priority := tpLower;

  HighPriority.FreeOnTerminate := False;
  LowPriority.FreeOnTerminate := False;
  HighPriority.Start;
  LowPriority.Start;
  HighPriority.WaitFor;
  LowPriority.WaitFor;

  WriteLn('Thread haute priorité : ', CountHigh, ' itérations');
  WriteLn('Thread basse priorité : ', CountLow, ' itérations');
  WriteLn('Ratio : ', (CountHigh / CountLow):0:2, ':1');

  {$IFDEF WINDOWS}
  WriteLn('Windows : ratio typiquement 2-4:1');
  {$ENDIF}
  {$IFDEF LINUX}
  WriteLn('Linux : ratio typiquement 3-8:1');
  {$ENDIF}

  HighPriority.Free;
  LowPriority.Free;
end;

end.
```

### 4. Documentation des comportements spécifiques

```pascal
{
  Configuration du scheduling multi-plateforme

  WINDOWS :
  - Les priorités sont facilement modifiables
  - Priority boost automatique activé par défaut
  - Quantum typique : 10-15ms
  - Bon pour la réactivité desktop

  LINUX :
  - Augmenter la priorité nécessite CAP_SYS_NICE ou root
  - Pas de priority boost
  - Quantum dynamique : 6-24ms
  - Excellent pour le débit serveur

  RECOMMANDATIONS :
  - Utiliser TThread.Priority pour la portabilité
  - Tester sur les deux plateformes
  - Ne pas compter sur des valeurs de priorité absolues
  - Gérer les échecs de changement de priorité sous Linux
}
```

## Cas d'usage spécifiques

### Application temps réel

```pascal
unit RealtimeApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF LINUX}, BaseUnix, Linux{$ENDIF};

type
  TRealtimeThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

implementation

constructor TRealtimeThread.Create;  
begin
  inherited Create(True);

  {$IFDEF WINDOWS}
  // Windows : Utiliser la classe de priorité temps réel
  Priority := tpTimeCritical;
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  {$ENDIF}

  {$IFDEF LINUX}
  // Linux : Utiliser SCHED_FIFO (nécessite privilèges)
  // SetSchedulingPolicy(SCHED_FIFO, 80);
  WriteLn('ATTENTION : Nécessite CAP_SYS_NICE ou root pour temps réel');
  WriteLn('Lancez avec : sudo setcap cap_sys_nice=eip votre_programme');
  {$ENDIF}
end;

procedure TRealtimeThread.Execute;  
var
  LastTime, CurrentTime: QWord;
  Delta: Int64;
const
  TARGET_INTERVAL = 1; // 1ms
begin
  LastTime := GetTickCount64;

  while not Terminated do
  begin
    // Travail critique temps réel
    ProcessRealtimeData();

    // Vérifier la latence
    CurrentTime := GetTickCount64;
    Delta := CurrentTime - LastTime;

    if Delta > TARGET_INTERVAL * 2 then
      WriteLn('ALERTE : Latence dépassée (', Delta, ' ms)');

    // Attendre l'intervalle
    Sleep(TARGET_INTERVAL);
    LastTime := CurrentTime;
  end;
end;

end.
```

### Serveur haute performance

```pascal
unit HighPerfServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF};

type
  TServerThread = class(TThread)
  private
    FWorkerID: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(WorkerID: Integer);
  end;

implementation

constructor TServerThread.Create(WorkerID: Integer);  
begin
  inherited Create(True);
  FWorkerID := WorkerID;

  {$IFDEF WINDOWS}
  // Windows : Priorité élevée pour les workers
  Priority := tpHigher;

  // Affinité CPU pour réduire les migrations
  SetThreadAffinityMask(Handle, 1 shl (WorkerID mod TThread.ProcessorCount));
  {$ENDIF}

  {$IFDEF LINUX}
  // Linux : SCHED_BATCH pour optimiser le débit
  // SetSchedulingPolicy(SCHED_BATCH, 0);

  // Affinité CPU
  // PinThreadToCoreLinux(WorkerID mod TThread.ProcessorCount);
  {$ENDIF}
end;

procedure TServerThread.Execute;  
begin
  while not Terminated do
  begin
    // Traiter les requêtes
    ProcessClientRequests();
    Sleep(1);
  end;
end;

end.
```

### Application interactive (UI)

```pascal
unit InteractiveApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF LINUX}, BaseUnix{$ENDIF};

type
  TUIThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TBackgroundWorker = class(TThread)
  protected
    procedure Execute; override;
  end;

implementation

{ TUIThread }

procedure TUIThread.Execute;  
begin
  {$IFDEF WINDOWS}
  // Windows : Priorité normale avec priority boost
  Priority := tpNormal;
  // Priority boost activé par défaut pour la réactivité
  {$ENDIF}

  {$IFDEF LINUX}
  // Linux : Nice value 0 (normal)
  FpNice(0);
  {$ENDIF}

  while not Terminated do
  begin
    // Traitement UI
    ProcessUIEvents();
    Sleep(10);
  end;
end;

{ TBackgroundWorker }

procedure TBackgroundWorker.Execute;  
begin
  {$IFDEF WINDOWS}
  // Windows : Basse priorité pour ne pas gêner l'UI
  Priority := tpLower;
  {$ENDIF}

  {$IFDEF LINUX}
  // Linux : Nice value élevé (basse priorité)
  FpNice(10);
  {$ENDIF}

  while not Terminated do
  begin
    // Travail en arrière-plan
    ProcessBackgroundTasks();
    Sleep(100);
  end;
end;

end.
```

## Problèmes courants et solutions

### 1. Inversion de priorité

**Problème :** Un thread de basse priorité bloque un thread de haute priorité.

```pascal
// Scénario problématique
var
  SharedResource: TCriticalSection;

// Thread haute priorité
procedure HighPriorityThread;  
begin
  SharedResource.Enter;
  try
    // Bloqué si un thread de basse priorité détient le verrou !
  finally
    SharedResource.Leave;
  end;
end;

// Thread basse priorité
procedure LowPriorityThread;  
begin
  SharedResource.Enter;
  try
    Sleep(1000); // Bloque le thread haute priorité pendant 1 seconde !
  finally
    SharedResource.Leave;
  end;
end;
```

**Solution :**

```pascal
// 1. Utiliser des verrous courts
procedure BetterLowPriorityThread;  
begin
  SharedResource.Enter;
  try
    // Minimiser le temps dans le verrou
    QuickOperation();
  finally
    SharedResource.Leave;
  end;
end;

// 2. Utiliser le priority inheritance (Windows automatique)
{$IFDEF WINDOWS}
// Windows gère automatiquement le priority inheritance
// pour les critical sections
{$ENDIF}

// 3. Éviter de partager des ressources entre priorités différentes
```

### 2. CPU starvation (Famine CPU)

**Problème :** Les threads de basse priorité ne s'exécutent jamais.

```pascal
// Thread haute priorité qui monopolise le CPU
procedure GreedyThread;  
begin
  while not Terminated do
  begin
    // Boucle serrée sans Sleep - monopolise le CPU !
    DoWork();
  end;
end;
```

**Solution :**

```pascal
// Ajouter des yields réguliers
procedure PoliteThread;  
begin
  while not Terminated do
  begin
    DoWork();

    // Donner une chance aux autres threads
    Sleep(0); // ou Sleep(1)

    // Ou utiliser TThread.Yield (FPC 3.2+)
    // TThread.Yield;
  end;
end;

// Ou utiliser un délai approprié
procedure BetterThread;  
begin
  while not Terminated do
  begin
    DoWork();
    Sleep(10); // 10ms entre les itérations
  end;
end;
```

### 3. Contention excessive

**Problème :** Trop de threads se battent pour les ressources.

```pascal
// Trop de threads
// Note : CreateAnonymousThread avec procédures anonymes nécessite
// {$modeswitch anonymousfunctions} en mode ObjFPC (FPC 3.3.1+)
var
  Threads: array[0..99] of TThread; // 100 threads !
  i: Integer;

begin
  for i := 0 to 99 do
  begin
    Threads[i] := TThread.CreateAnonymousThread(
      procedure
      begin
        DoWork();
      end
    );
    Threads[i].Start;
  end;

  // Overhead énorme de context switching !
end;
```

**Solution :**

```pascal
// Limiter au nombre de cœurs
var
  OptimalThreadCount: Integer;
  Threads: array of TThread;
  i: Integer;

begin
  OptimalThreadCount := TThread.ProcessorCount;
  SetLength(Threads, OptimalThreadCount);

  for i := 0 to OptimalThreadCount - 1 do
  begin
    Threads[i] := TThread.CreateAnonymousThread(
      procedure
      begin
        DoWork();
      end
    );
    Threads[i].Start;
  end;

  // Ou utiliser un thread pool
end;
```

### 4. Affinité CPU incorrecte

**Problème :** Forcer l'affinité peut nuire aux performances.

```pascal
// ❌ MAUVAIS - Tous les threads sur le même cœur
for i := 0 to 7 do  
begin
  Thread := CreateThread();
  SetThreadAffinityMask(Thread.Handle, $01); // Tous sur CPU 0 !
end;
```

**Solution :**

```pascal
// ✅ BON - Répartir sur différents cœurs
var
  CoreCount: Integer;

begin
  CoreCount := TThread.ProcessorCount;

  for i := 0 to 7 do
  begin
    Thread := CreateThread();

    // Répartir équitablement
    {$IFDEF WINDOWS}
    SetThreadAffinityMask(Thread.Handle, 1 shl (i mod CoreCount));
    {$ENDIF}

    {$IFDEF LINUX}
    // PinThreadToCoreLinux(i mod CoreCount);
    {$ENDIF}
  end;
end;

// Ou mieux : laisser l'OS gérer l'affinité
for i := 0 to 7 do  
begin
  Thread := CreateThread();
  // Pas d'affinité = L'OS optimise automatiquement
end;
```

## Recommandations finales

### Checklist de compatibilité multi-plateforme

- [ ] Tester sur Windows ET Linux
- [ ] Utiliser TThread.Priority plutôt que les API natives
- [ ] Gérer les échecs de changement de priorité
- [ ] Documenter les comportements spécifiques
- [ ] Ne pas compter sur le priority boost (Windows only)
- [ ] Limiter le nombre de threads au nombre de cœurs
- [ ] Ajouter des Sleep(0) dans les boucles serrées
- [ ] Éviter l'affinité CPU sauf cas particuliers
- [ ] Mesurer les performances sur les deux plateformes

### Quand optimiser le scheduling

**Optimisez SI :**
- ✅ L'application est CPU-bound
- ✅ Profiling montre des problèmes de context switching
- ✅ Besoin de temps réel strict
- ✅ Serveur haute performance

**N'optimisez PAS SI :**
- ❌ L'application est I/O-bound
- ❌ Pas de problème de performance mesuré
- ❌ Code qui fonctionne déjà bien
- ❌ Application simple sans contraintes temps réel

### Code template portable

```pascal
unit PortableScheduling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF LINUX}, BaseUnix{$ENDIF};

type
  TAppThreadPriority = (atpBackground, atpNormal, atpImportant, atpCritical);

  TPortableThread = class(TThread)
  private
    FAppPriority: TAppThreadPriority;
    procedure ApplyPlatformSettings;
  protected
    procedure Execute; override;
  public
    constructor Create(Priority: TAppThreadPriority);
  end;

implementation

constructor TPortableThread.Create(Priority: TAppThreadPriority);  
begin
  inherited Create(True);
  FAppPriority := Priority;
end;

procedure TPortableThread.ApplyPlatformSettings;  
begin
  case FAppPriority of
    atpBackground:
    begin
      {$IFDEF WINDOWS}
      Priority := tpLower;
      {$ENDIF}
      {$IFDEF LINUX}
      FpNice(10);
      {$ENDIF}
    end;

    atpNormal:
    begin
      {$IFDEF WINDOWS}
      Priority := tpNormal;
      {$ENDIF}
      {$IFDEF LINUX}
      FpNice(0);
      {$ENDIF}
    end;

    atpImportant:
    begin
      {$IFDEF WINDOWS}
      Priority := tpHigher;
      {$ENDIF}
      {$IFDEF LINUX}
      if FpNice(-5) = -1 then
        WriteLn('Privilèges insuffisants, utilisation priorité normale');
      {$ENDIF}
    end;

    atpCritical:
    begin
      {$IFDEF WINDOWS}
      Priority := tpTimeCritical;
      SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
      {$ENDIF}
      {$IFDEF LINUX}
      if FpNice(-20) = -1 then
        WriteLn('ERREUR : Privilèges root requis pour priorité critique');
      {$ENDIF}
    end;
  end;
end;

procedure TPortableThread.Execute;  
begin
  ApplyPlatformSettings;

  // Votre code ici
  while not Terminated do
  begin
    // Travail du thread
    Sleep(10);
  end;
end;

end.
```

## Résumé

Les différences de scheduling entre Windows et Linux sont significatives :

**Windows :**
- **Modèle** : Priorités absolues (0-31)
- **Forces** : Réactivité, facilité de configuration
- **Faiblesses** : Scalabilité sur gros systèmes
- **Idéal pour** : Applications desktop interactives

**Linux :**
- **Modèle** : CFS avec nice values (-20 à +19)
- **Forces** : Équité, scalabilité, débit
- **Faiblesses** : Nécessite privilèges pour haute priorité
- **Idéal pour** : Serveurs, systèmes embarqués

**Conseils clés :**
1. **Utilisez les abstractions** (TThread.Priority)
2. **Testez sur les deux plateformes**
3. **Documentez les comportements spécifiques**
4. **Gérez les échecs de changement de priorité**
5. **Optimisez seulement si nécessaire**

La compréhension du scheduling vous permet d'écrire des applications multi-threadées performantes et portables sur Windows et Linux !

⏭️ [Acteurs et passage de messages](/11-multithreading-concurrence/08-acteurs-passage-messages.md)
