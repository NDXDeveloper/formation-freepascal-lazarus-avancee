🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.7 Tests de charge et stress

## Introduction

Les tests de charge et de stress permettent de vérifier comment votre application se comporte dans des conditions extrêmes ou inhabituelles.

### Différence entre les types de tests

**Tests de charge (Load Testing) :**
- Simulent une utilisation **normale mais intensive**
- Exemple : 100 utilisateurs connectés simultanément
- Objectif : Vérifier que l'application fonctionne correctement sous charge normale maximale

**Tests de stress (Stress Testing) :**
- Poussent l'application **au-delà de ses limites**
- Exemple : 10 000 utilisateurs connectés simultanément
- Objectif : Trouver le point de rupture et observer le comportement en cas de surcharge

**Analogie simple :**
- **Test de charge** = Vérifier qu'un pont peut supporter 1000 voitures (capacité prévue)
- **Test de stress** = Mettre 5000 voitures pour voir quand le pont cède (limite absolue)

## Pourquoi faire des tests de charge et stress ?

### Raisons principales

1. **Identifier les limites** : Connaître la capacité maximale de l'application
2. **Prévenir les pannes** : Détecter les problèmes avant la mise en production
3. **Optimiser les ressources** : Savoir si le serveur est suffisant
4. **Améliorer la stabilité** : S'assurer que l'application ne plante pas sous charge
5. **Planifier la croissance** : Anticiper les besoins futurs

### Symptômes d'une application qui ne supporte pas la charge

- Temps de réponse très longs
- Messages d'erreur "Out of memory"
- Plantages aléatoires
- Perte de connexions
- Corruption de données
- Blocages (deadlocks)

## Test de charge basique avec threads

### Principe

Créer plusieurs threads qui simulent des utilisateurs effectuant des opérations en parallèle.

### Exemple 1 : Test de charge sur un calcul

```pascal
program LoadTestCalculation;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, SyncObjs;

type
  TWorkerThread = class(TThread)
  private
    FThreadID: Integer;
    FIterations: Integer;
    FStartTime: QWord;
    FEndTime: QWord;
    FErrors: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ThreadID, Iterations: Integer);
    property ThreadID: Integer read FThreadID;
    property Errors: Integer read FErrors;
    property ExecutionTime: QWord read FEndTime;
  end;

constructor TWorkerThread.Create(ThreadID, Iterations: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FThreadID := ThreadID;
  FIterations := Iterations;
  FErrors := 0;
end;

procedure TWorkerThread.Execute;
var
  i, j, result: Integer;
begin
  FStartTime := GetTickCount64;

  try
    // Simulation de travail intensif
    for i := 1 to FIterations do
    begin
      result := 0;
      for j := 1 to 10000 do
        result := result + (i * j) mod 1000;
    end;
  except
    on E: Exception do
      Inc(FErrors);
  end;

  FEndTime := GetTickCount64 - FStartTime;
end;

const
  NB_THREADS = 50;      // Nombre d'utilisateurs simultanés
  NB_OPERATIONS = 100;  // Nombre d'opérations par utilisateur

var
  threads: array of TWorkerThread;
  i: Integer;
  startTime, endTime: QWord;
  totalTime, minTime, maxTime, avgTime: QWord;
  totalErrors: Integer;

begin
  WriteLn('=== Test de charge - Calculs intensifs ===');
  WriteLn('Threads:    ', NB_THREADS);
  WriteLn('Opérations: ', NB_OPERATIONS, ' par thread');
  WriteLn;
  WriteLn('Démarrage du test...');

  SetLength(threads, NB_THREADS);
  startTime := GetTickCount64;

  // Créer et démarrer tous les threads
  for i := 0 to NB_THREADS - 1 do
    threads[i] := TWorkerThread.Create(i + 1, NB_OPERATIONS);

  // Attendre la fin de tous les threads
  for i := 0 to NB_THREADS - 1 do
    threads[i].WaitFor;

  endTime := GetTickCount64;
  totalTime := endTime - startTime;

  // Calculer les statistiques
  minTime := threads[0].ExecutionTime;
  maxTime := threads[0].ExecutionTime;
  avgTime := 0;
  totalErrors := 0;

  for i := 0 to NB_THREADS - 1 do
  begin
    if threads[i].ExecutionTime < minTime then
      minTime := threads[i].ExecutionTime;
    if threads[i].ExecutionTime > maxTime then
      maxTime := threads[i].ExecutionTime;
    avgTime := avgTime + threads[i].ExecutionTime;
    totalErrors := totalErrors + threads[i].Errors;
  end;

  avgTime := avgTime div NB_THREADS;

  // Afficher les résultats
  WriteLn;
  WriteLn('=== Résultats ===');
  WriteLn('Temps total:           ', totalTime, ' ms');
  WriteLn('Temps min par thread:  ', minTime, ' ms');
  WriteLn('Temps max par thread:  ', maxTime, ' ms');
  WriteLn('Temps moyen par thread:', avgTime, ' ms');
  WriteLn('Erreurs totales:       ', totalErrors);
  WriteLn('Débit:                 ', (NB_THREADS * NB_OPERATIONS * 1000) div totalTime, ' op/sec');

  if totalErrors > 0 then
    WriteLn('⚠ ATTENTION: Des erreurs ont été détectées!')
  else
    WriteLn('✓ Test réussi sans erreur');

  // Libérer les threads
  for i := 0 to NB_THREADS - 1 do
    threads[i].Free;
end.
```

**Sortie typique :**
```
=== Test de charge - Calculs intensifs ===
Threads:    50
Opérations: 100 par thread

Démarrage du test...

=== Résultats ===
Temps total:           3245 ms
Temps min par thread:  2890 ms
Temps max par thread:  3210 ms
Temps moyen par thread:3050 ms
Erreurs totales:       0
Débit:                 1540 op/sec
✓ Test réussi sans erreur
```

## Test de charge sur base de données

### Exemple 2 : Connexions multiples simultanées

```pascal
program LoadTestDatabase;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, sqldb, sqlite3conn, SyncObjs;

type
  TDBWorkerThread = class(TThread)
  private
    FThreadID: Integer;
    FQueries: Integer;
    FErrors: Integer;
    FSuccesses: Integer;
    FTotalTime: QWord;
  protected
    procedure Execute; override;
  public
    constructor Create(ThreadID, Queries: Integer);
    property ThreadID: Integer read FThreadID;
    property Errors: Integer read FErrors;
    property Successes: Integer read FSuccesses;
    property TotalTime: QWord read FTotalTime;
  end;

constructor TDBWorkerThread.Create(ThreadID, Queries: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FThreadID := ThreadID;
  FQueries := Queries;
  FErrors := 0;
  FSuccesses := 0;
end;

procedure TDBWorkerThread.Execute;
var
  conn: TSQLite3Connection;
  trans: TSQLTransaction;
  query: TSQLQuery;
  i: Integer;
  startTime: QWord;
begin
  startTime := GetTickCount64;

  // Chaque thread a sa propre connexion
  conn := TSQLite3Connection.Create(nil);
  trans := TSQLTransaction.Create(nil);
  query := TSQLQuery.Create(nil);

  try
    conn.DatabaseName := 'load_test.db';
    trans.DataBase := conn;
    conn.Transaction := trans;
    query.DataBase := conn;

    conn.Open;
    trans.Active := True;

    // Effectuer plusieurs requêtes
    for i := 1 to FQueries do
    begin
      try
        // INSERT
        query.SQL.Text := 'INSERT INTO test_table (thread_id, iteration, data) VALUES (:tid, :iter, :data)';
        query.ParamByName('tid').AsInteger := FThreadID;
        query.ParamByName('iter').AsInteger := i;
        query.ParamByName('data').AsString := Format('Data from thread %d iteration %d', [FThreadID, i]);
        query.ExecSQL;
        trans.Commit;

        // SELECT
        query.SQL.Text := 'SELECT COUNT(*) FROM test_table WHERE thread_id = :tid';
        query.ParamByName('tid').AsInteger := FThreadID;
        query.Open;
        query.Close;

        Inc(FSuccesses);
      except
        on E: Exception do
          Inc(FErrors);
      end;
    end;

  finally
    query.Free;
    trans.Free;
    conn.Free;
  end;

  FTotalTime := GetTickCount64 - startTime;
end;

procedure InitDatabase;
var
  conn: TSQLite3Connection;
  trans: TSQLTransaction;
  query: TSQLQuery;
begin
  conn := TSQLite3Connection.Create(nil);
  trans := TSQLTransaction.Create(nil);
  query := TSQLQuery.Create(nil);

  try
    conn.DatabaseName := 'load_test.db';
    trans.DataBase := conn;
    conn.Transaction := trans;
    query.DataBase := conn;

    conn.Open;
    trans.Active := True;

    // Créer la table
    query.SQL.Text := 'DROP TABLE IF EXISTS test_table';
    query.ExecSQL;

    query.SQL.Text := 'CREATE TABLE test_table (id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
                      'thread_id INTEGER, iteration INTEGER, data TEXT, created_at DATETIME DEFAULT CURRENT_TIMESTAMP)';
    query.ExecSQL;

    trans.Commit;
    WriteLn('✓ Base de données initialisée');
  finally
    query.Free;
    trans.Free;
    conn.Free;
  end;
end;

const
  NB_THREADS = 20;    // Nombre de connexions simultanées
  NB_QUERIES = 50;    // Nombre de requêtes par connexion

var
  threads: array of TDBWorkerThread;
  i: Integer;
  startTime, totalTime: QWord;
  totalErrors, totalSuccesses: Integer;
  minTime, maxTime, avgTime: QWord;

begin
  WriteLn('=== Test de charge - Base de données ===');
  WriteLn('Connexions simultanées: ', NB_THREADS);
  WriteLn('Requêtes par connexion: ', NB_QUERIES);
  WriteLn;

  InitDatabase;
  WriteLn;
  WriteLn('Démarrage du test...');

  SetLength(threads, NB_THREADS);
  startTime := GetTickCount64;

  // Créer et démarrer tous les threads
  for i := 0 to NB_THREADS - 1 do
    threads[i] := TDBWorkerThread.Create(i + 1, NB_QUERIES);

  // Attendre la fin
  for i := 0 to NB_THREADS - 1 do
  begin
    threads[i].WaitFor;
    Write('.');
  end;

  WriteLn;
  totalTime := GetTickCount64 - startTime;

  // Calculer les statistiques
  totalErrors := 0;
  totalSuccesses := 0;
  minTime := threads[0].TotalTime;
  maxTime := 0;
  avgTime := 0;

  for i := 0 to NB_THREADS - 1 do
  begin
    totalErrors := totalErrors + threads[i].Errors;
    totalSuccesses := totalSuccesses + threads[i].Successes;
    if threads[i].TotalTime < minTime then
      minTime := threads[i].TotalTime;
    if threads[i].TotalTime > maxTime then
      maxTime := threads[i].TotalTime;
    avgTime := avgTime + threads[i].TotalTime;
  end;

  avgTime := avgTime div NB_THREADS;

  // Afficher les résultats
  WriteLn;
  WriteLn('=== Résultats ===');
  WriteLn('Temps total:              ', totalTime, ' ms');
  WriteLn('Temps min par connexion:  ', minTime, ' ms');
  WriteLn('Temps max par connexion:  ', maxTime, ' ms');
  WriteLn('Temps moyen par connexion:', avgTime, ' ms');
  WriteLn('Requêtes réussies:        ', totalSuccesses);
  WriteLn('Requêtes échouées:        ', totalErrors);
  WriteLn('Débit:                    ', (totalSuccesses * 1000) div totalTime, ' req/sec');
  WriteLn('Taux de réussite:         ', (totalSuccesses * 100) div (totalSuccesses + totalErrors), '%');

  if totalErrors > 0 then
    WriteLn('⚠ ATTENTION: ', totalErrors, ' erreurs détectées')
  else
    WriteLn('✓ Test réussi sans erreur');

  // Libérer
  for i := 0 to NB_THREADS - 1 do
    threads[i].Free;
end.
```

## Test de serveur Web

### Exemple 3 : Serveur HTTP simple avec test de charge

**Serveur (à exécuter en premier) :**

```pascal
program SimpleHTTPServer;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fphttpserver, httpdefs;

type
  TMyHTTPServer = class(TFPHTTPServer)
  private
    FRequestCount: Integer;
  protected
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
                          var AResponse: TFPHTTPConnectionResponse); override;
  public
    constructor Create(AOwner: TComponent); override;
    property RequestCount: Integer read FRequestCount;
  end;

constructor TMyHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRequestCount := 0;
end;

procedure TMyHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
                                      var AResponse: TFPHTTPConnectionResponse);
var
  responseData: string;
begin
  Inc(FRequestCount);

  // Simulation de traitement
  Sleep(10); // 10ms de traitement

  responseData := Format('{"status":"ok","request_id":%d,"timestamp":"%s"}',
                         [FRequestCount, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);

  AResponse.ContentType := 'application/json';
  AResponse.Content := responseData;
  AResponse.Code := 200;
  AResponse.CodeText := 'OK';
end;

var
  server: TMyHTTPServer;

begin
  server := TMyHTTPServer.Create(nil);
  try
    server.Port := 8080;
    server.Active := True;

    WriteLn('Serveur HTTP démarré sur le port 8080');
    WriteLn('Appuyez sur Entrée pour arrêter...');
    ReadLn;

    WriteLn('Nombre total de requêtes traitées: ', server.RequestCount);
  finally
    server.Free;
  end;
end.
```

**Client de test de charge :**

```pascal
program HTTPLoadTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fphttpclient;

type
  THTTPClientThread = class(TThread)
  private
    FThreadID: Integer;
    FRequests: Integer;
    FSuccesses: Integer;
    FErrors: Integer;
    FTotalTime: QWord;
    FServerURL: string;
  protected
    procedure Execute; override;
  public
    constructor Create(ThreadID, Requests: Integer; const ServerURL: string);
    property Successes: Integer read FSuccesses;
    property Errors: Integer read FErrors;
    property TotalTime: QWord read FTotalTime;
  end;

constructor THTTPClientThread.Create(ThreadID, Requests: Integer; const ServerURL: string);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FThreadID := ThreadID;
  FRequests := Requests;
  FServerURL := ServerURL;
  FSuccesses := 0;
  FErrors := 0;
end;

procedure THTTPClientThread.Execute;
var
  client: TFPHTTPClient;
  i: Integer;
  response: string;
  startTime: QWord;
begin
  startTime := GetTickCount64;
  client := TFPHTTPClient.Create(nil);
  try
    for i := 1 to FRequests do
    begin
      try
        response := client.Get(FServerURL);
        if client.ResponseStatusCode = 200 then
          Inc(FSuccesses)
        else
          Inc(FErrors);
      except
        on E: Exception do
          Inc(FErrors);
      end;
    end;
  finally
    client.Free;
  end;

  FTotalTime := GetTickCount64 - startTime;
end;

const
  SERVER_URL = 'http://localhost:8080/';
  NB_CLIENTS = 50;      // Nombre de clients simultanés
  REQUESTS_PER_CLIENT = 20;

var
  threads: array of THTTPClientThread;
  i: Integer;
  startTime, totalTime: QWord;
  totalSuccesses, totalErrors: Integer;
  minTime, maxTime, avgTime: QWord;

begin
  WriteLn('=== Test de charge HTTP ===');
  WriteLn('URL:                ', SERVER_URL);
  WriteLn('Clients simultanés: ', NB_CLIENTS);
  WriteLn('Requêtes par client:', REQUESTS_PER_CLIENT);
  WriteLn('Total requêtes:     ', NB_CLIENTS * REQUESTS_PER_CLIENT);
  WriteLn;
  WriteLn('Assurez-vous que le serveur est démarré sur le port 8080');
  WriteLn('Appuyez sur Entrée pour commencer...');
  ReadLn;

  SetLength(threads, NB_CLIENTS);
  startTime := GetTickCount64;

  // Créer et démarrer tous les threads clients
  for i := 0 to NB_CLIENTS - 1 do
    threads[i] := THTTPClientThread.Create(i + 1, REQUESTS_PER_CLIENT, SERVER_URL);

  // Attendre la fin
  WriteLn('Test en cours');
  for i := 0 to NB_CLIENTS - 1 do
  begin
    threads[i].WaitFor;
    Write('.');
    if (i + 1) mod 10 = 0 then
      WriteLn(' ', i + 1, '/', NB_CLIENTS);
  end;

  WriteLn;
  totalTime := GetTickCount64 - startTime;

  // Statistiques
  totalSuccesses := 0;
  totalErrors := 0;
  minTime := threads[0].TotalTime;
  maxTime := 0;
  avgTime := 0;

  for i := 0 to NB_CLIENTS - 1 do
  begin
    totalSuccesses := totalSuccesses + threads[i].Successes;
    totalErrors := totalErrors + threads[i].Errors;
    if threads[i].TotalTime < minTime then
      minTime := threads[i].TotalTime;
    if threads[i].TotalTime > maxTime then
      maxTime := threads[i].TotalTime;
    avgTime := avgTime + threads[i].TotalTime;
  end;

  avgTime := avgTime div NB_CLIENTS;

  WriteLn;
  WriteLn('=== Résultats ===');
  WriteLn('Temps total:           ', totalTime, ' ms');
  WriteLn('Temps min par client:  ', minTime, ' ms');
  WriteLn('Temps max par client:  ', maxTime, ' ms');
  WriteLn('Temps moyen par client:', avgTime, ' ms');
  WriteLn('Requêtes réussies:     ', totalSuccesses);
  WriteLn('Requêtes échouées:     ', totalErrors);
  WriteLn('Débit:                 ', (totalSuccesses * 1000) div totalTime, ' req/sec');
  WriteLn('Latence moyenne:       ', totalTime div (totalSuccesses + totalErrors), ' ms/req');
  WriteLn('Taux de réussite:      ', (totalSuccesses * 100) div (totalSuccesses + totalErrors), '%');

  if totalErrors > 0 then
    WriteLn('⚠ ', totalErrors, ' requêtes ont échoué')
  else
    WriteLn('✓ Toutes les requêtes ont réussi');

  for i := 0 to NB_CLIENTS - 1 do
    threads[i].Free;
end.
```

## Test de stress : Trouver la limite

### Exemple 4 : Test de stress progressif

```pascal
program StressTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TStressWorker = class(TThread)
  private
    FID: Integer;
    FCompleted: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(ID: Integer);
    property Completed: Boolean read FCompleted;
  end;

constructor TStressWorker.Create(ID: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FID := ID;
  FCompleted := False;
end;

procedure TStressWorker.Execute;
var
  i, j, sum: Integer;
begin
  try
    sum := 0;
    for i := 1 to 1000 do
      for j := 1 to 1000 do
        sum := sum + (i * j) mod 1000;
    FCompleted := True;
  except
    FCompleted := False;
  end;
end;

var
  nbThreads: Integer;
  threads: array of TStressWorker;
  i, completed, failed: Integer;
  startTime, endTime: QWord;
  continueTest: Boolean;

begin
  WriteLn('=== Test de stress progressif ===');
  WriteLn('Ce test va augmenter progressivement la charge');
  WriteLn('jusqu''à trouver la limite du système.');
  WriteLn;

  nbThreads := 10;
  continueTest := True;

  while continueTest do
  begin
    WriteLn('Test avec ', nbThreads, ' threads...');

    SetLength(threads, nbThreads);
    startTime := GetTickCount64;

    // Créer les threads
    for i := 0 to nbThreads - 1 do
      threads[i] := TStressWorker.Create(i + 1);

    // Attendre la fin
    for i := 0 to nbThreads - 1 do
      threads[i].WaitFor;

    endTime := GetTickCount64;

    // Compter les succès/échecs
    completed := 0;
    failed := 0;
    for i := 0 to nbThreads - 1 do
    begin
      if threads[i].Completed then
        Inc(completed)
      else
        Inc(failed);
      threads[i].Free;
    end;

    WriteLn('  Complétés: ', completed, '/', nbThreads);
    WriteLn('  Temps:     ', endTime - startTime, ' ms');
    WriteLn('  Débit:     ', (completed * 1000) div (endTime - startTime), ' threads/sec');

    // Vérifier si on peut continuer
    if failed > 0 then
    begin
      WriteLn;
      WriteLn('⚠ Limite atteinte à ', nbThreads, ' threads');
      WriteLn('  ', failed, ' threads ont échoué');
      continueTest := False;
    end
    else if (endTime - startTime) > 30000 then // Plus de 30 secondes
    begin
      WriteLn;
      WriteLn('⚠ Temps d''exécution trop long (> 30s)');
      WriteLn('  Limite pratique atteinte');
      continueTest := False;
    end
    else
    begin
      // Augmenter la charge
      nbThreads := nbThreads + 10;
      WriteLn('  ✓ Réussi, augmentation à ', nbThreads, ' threads');
      WriteLn;

      // Sécurité : arrêter à 1000 threads
      if nbThreads > 1000 then
      begin
        WriteLn('Limite de sécurité atteinte (1000 threads)');
        continueTest := False;
      end;
    end;
  end;

  WriteLn;
  WriteLn('=== Test terminé ===');
  WriteLn('Capacité maximale: ', nbThreads - 10, ' threads');
end.
```

## Surveillance des ressources système

### Exemple 5 : Monitoring pendant le test

```pascal
program LoadTestWithMonitoring;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

{$IFDEF WINDOWS}
function GetMemoryUsage: QWord;
var
  memStatus: TMemoryStatusEx;
begin
  memStatus.dwLength := SizeOf(TMemoryStatusEx);
  GlobalMemoryStatusEx(memStatus);
  Result := memStatus.ullTotalPhys - memStatus.ullAvailPhys;
end;
{$ENDIF}

{$IFDEF UNIX}
function GetMemoryUsage: QWord;
var
  statFile: TextFile;
  line: string;
  value: QWord;
begin
  Result := 0;
  try
    AssignFile(statFile, '/proc/self/status');
    Reset(statFile);
    while not EOF(statFile) do
    begin
      ReadLn(statFile, line);
      if Pos('VmRSS:', line) = 1 then
      begin
        Delete(line, 1, 6);
        line := Trim(line);
        Delete(line, Pos(' ', line), Length(line));
        value := StrToQWordDef(line, 0);
        Result := value * 1024; // Convertir KB en bytes
        Break;
      end;
    end;
    CloseFile(statFile);
  except
    Result := 0;
  end;
end;
{$ENDIF}

type
  TMonitorThread = class(TThread)
  private
    FInterval: Integer;
    FMaxMemory: QWord;
    FSamples: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(IntervalMs: Integer);
    property MaxMemory: QWord read FMaxMemory;
    property Samples: Integer read FSamples;
  end;

constructor TMonitorThread.Create(IntervalMs: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FInterval := IntervalMs;
  FMaxMemory := 0;
  FSamples := 0;
end;

procedure TMonitorThread.Execute;
var
  currentMemory: QWord;
begin
  while not Terminated do
  begin
    currentMemory := GetMemoryUsage;
    if currentMemory > FMaxMemory then
      FMaxMemory := currentMemory;
    Inc(FSamples);
    Sleep(FInterval);
  end;
end;

type
  TWorkerThread = class(TThread)
  private
    FData: array of Integer;
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

constructor TWorkerThread.Create;
begin
  inherited Create(False);
  FreeOnTerminate := False;
end;

procedure TWorkerThread.Execute;
var
  i, j: Integer;
begin
  // Allouer de la mémoire
  SetLength(FData, 1000000);

  // Faire des calculs
  for i := 0 to High(FData) do
    FData[i] := Random(1000);

  for j := 1 to 100 do
    for i := 0 to High(FData) do
      FData[i] := FData[i] * 2 mod 1000;
end;

const
  NB_WORKERS = 20;

var
  monitor: TMonitorThread;
  workers: array of TWorkerThread;
  i: Integer;
  startTime: QWord;
  startMemory, endMemory: QWord;

begin
  WriteLn('=== Test de charge avec monitoring ===');
  WriteLn('Threads de travail: ', NB_WORKERS);
  WriteLn;

  startMemory := GetMemoryUsage;
  WriteLn('Mémoire initiale: ', startMemory div 1024 div 1024, ' MB');
  WriteLn;
  WriteLn('Démarrage du test...');

  // Démarrer le monitoring
  monitor := TMonitorThread.Create(100); // Échantillonner toutes les 100ms

  startTime := GetTickCount64;
  SetLength(workers, NB_WORKERS);

  // Créer les workers
  for i := 0 to NB_WORKERS - 1 do
    workers[i] := TWorkerThread.Create;

  // Attendre la fin
  for i := 0 to NB_WORKERS - 1 do
  begin
    workers[i].WaitFor;
    Write('.');
  end;

  WriteLn;

  // Arrêter le monitoring
  monitor.Terminate;
  monitor.WaitFor;

  endMemory := GetMemoryUsage;

  WriteLn;
  WriteLn('=== Résultats ===');
  WriteLn('Temps d''exécution:      ', GetTickCount64 - startTime, ' ms');
  WriteLn('Mémoire finale:         ', endMemory div 1024 div 1024, ' MB');
  WriteLn('Mémoire maximale:       ', monitor.MaxMemory div 1024 div 1024, ' MB');
  WriteLn('Augmentation mémoire:   ', (endMemory - startMemory) div 1024 div 1024, ' MB');
  WriteLn('Échantillons collectés: ', monitor.Samples);

  // Libérer
  for i := 0 to NB_WORKERS - 1 do
    workers[i].Free;
  monitor.Free;
end.
```

## Test de fuite mémoire sous charge

### Exemple 6 : Détection de fuites mémoire

```pascal
program MemoryLeakTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TLeakyThread = class(TThread)
  private
    FLeaks: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(CauseLeaks: Boolean);
  end;

constructor TLeakyThread.Create(CauseLeaks: Boolean);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FLeaks := CauseLeaks;
end;

procedure TLeakyThread.Execute;
var
  i: Integer;
  list: TStringList;
begin
  for i := 1 to 1000 do
  begin
    list := TStringList.Create;
    list.Add('Test ' + IntToStr(i));

    if not FLeaks then
      list.Free;  // Si FLeaks = True, on ne libère pas = fuite mémoire
  end;
end;

function GetCurrentMemory: QWord;
begin
  Result := GetHeapStatus.TotalAllocated;
end;

procedure RunTest(const TestName: string; CauseLeaks: Boolean);
const
  NB_THREADS = 10;
var
  threads: array of TLeakyThread;
  i: Integer;
  memBefore, memAfter: QWord;
begin
  WriteLn('=== ', TestName, ' ===');

  memBefore := GetCurrentMemory;
  WriteLn('Mémoire avant: ', memBefore div 1024, ' KB');

  SetLength(threads, NB_THREADS);

  for i := 0 to NB_THREADS - 1 do
    threads[i] := TLeakyThread.Create(CauseLeaks);

  for i := 0 to NB_THREADS - 1 do
    threads[i].WaitFor;

  for i := 0 to NB_THREADS - 1 do
    threads[i].Free;

  memAfter := GetCurrentMemory;
  WriteLn('Mémoire après: ', memAfter div 1024, ' KB');
  WriteLn('Différence:    ', (memAfter - memBefore) div 1024, ' KB');

  if (memAfter - memBefore) > 1024 * 10 then // Plus de 10 KB
    WriteLn('⚠ FUITE MÉMOIRE DÉTECTÉE!')
  else
    WriteLn('✓ Pas de fuite détectée');

  WriteLn;
end;

begin
  WriteLn('=== Test de détection de fuites mémoire ===');
  WriteLn;

  RunTest('Test sans fuite', False);
  RunTest('Test avec fuite', True);
end.
```

## Test de deadlock et conditions de course

### Exemple 7 : Détection de deadlock

```pascal
program DeadlockTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, SyncObjs;

var
  Lock1, Lock2: TCriticalSection;
  DeadlockDetected: Boolean;

type
  TThread1 = class(TThread)
  protected
    procedure Execute; override;
  end;

  TThread2 = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThread1.Execute;
begin
  try
    WriteLn('Thread 1: Tentative de verrouillage Lock1...');
    Lock1.Enter;
    WriteLn('Thread 1: Lock1 acquis');

    Sleep(100); // Attendre pour créer la condition de deadlock

    WriteLn('Thread 1: Tentative de verrouillage Lock2...');
    if Lock2.TryEnter then
    begin
      WriteLn('Thread 1: Lock2 acquis');
      Lock2.Leave;
    end
    else
    begin
      WriteLn('Thread 1: Impossible d''acquérir Lock2 - DEADLOCK détecté');
      DeadlockDetected := True;
    end;

    Lock1.Leave;
    WriteLn('Thread 1: Terminé');
  except
    on E: Exception do
      WriteLn('Thread 1 erreur: ', E.Message);
  end;
end;

procedure TThread2.Execute;
begin
  try
    WriteLn('Thread 2: Tentative de verrouillage Lock2...');
    Lock2.Enter;
    WriteLn('Thread 2: Lock2 acquis');

    Sleep(100); // Attendre pour créer la condition de deadlock

    WriteLn('Thread 2: Tentative de verrouillage Lock1...');
    if Lock1.TryEnter then
    begin
      WriteLn('Thread 2: Lock1 acquis');
      Lock1.Leave;
    end
    else
    begin
      WriteLn('Thread 2: Impossible d''acquérir Lock1 - DEADLOCK détecté');
      DeadlockDetected := True;
    end;

    Lock2.Leave;
    WriteLn('Thread 2: Terminé');
  except
    on E: Exception do
      WriteLn('Thread 2 erreur: ', E.Message);
  end;
end;

var
  t1: TThread1;
  t2: TThread2;

begin
  WriteLn('=== Test de détection de deadlock ===');
  WriteLn('Ce test crée intentionnellement une situation de deadlock');
  WriteLn;

  Lock1 := TCriticalSection.Create;
  Lock2 := TCriticalSection.Create;
  DeadlockDetected := False;

  try
    t1 := TThread1.Create(False);
    t2 := TThread2.Create(False);

    t1.WaitFor;
    t2.WaitFor;

    WriteLn;
    if DeadlockDetected then
      WriteLn('⚠ DEADLOCK DÉTECTÉ - Le code nécessite une correction')
    else
      WriteLn('✓ Aucun deadlock détecté');

    t1.Free;
    t2.Free;
  finally
    Lock1.Free;
    Lock2.Free;
  end;
end.
```

## Génération de rapport de test de charge

### Exemple 8 : Rapport HTML complet

```pascal
program LoadTestReport;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TTestResult = record
    TestName: string;
    Threads: Integer;
    Duration: QWord;
    Successes: Integer;
    Failures: Integer;
    Throughput: Double;
    AvgLatency: Double;
  end;

procedure GenerateHTMLReport(const Results: array of TTestResult; const FileName: string);
var
  html: TStringList;
  i: Integer;
  totalTests, totalSuccesses, totalFailures: Integer;
begin
  html := TStringList.Create;
  try
    html.Add('<!DOCTYPE html>');
    html.Add('<html>');
    html.Add('<head>');
    html.Add('  <meta charset="UTF-8">');
    html.Add('  <title>Rapport de Test de Charge</title>');
    html.Add('  <style>');
    html.Add('    body {');
    html.Add('      font-family: Arial, sans-serif;');
    html.Add('      margin: 20px;');
    html.Add('      background-color: #f5f5f5;');
    html.Add('    }');
    html.Add('    .container {');
    html.Add('      max-width: 1200px;');
    html.Add('      margin: 0 auto;');
    html.Add('      background-color: white;');
    html.Add('      padding: 20px;');
    html.Add('      border-radius: 8px;');
    html.Add('      box-shadow: 0 2px 4px rgba(0,0,0,0.1);');
    html.Add('    }');
    html.Add('    h1 {');
    html.Add('      color: #333;');
    html.Add('      border-bottom: 3px solid #4CAF50;');
    html.Add('      padding-bottom: 10px;');
    html.Add('    }');
    html.Add('    table {');
    html.Add('      width: 100%;');
    html.Add('      border-collapse: collapse;');
    html.Add('      margin-top: 20px;');
    html.Add('    }');
    html.Add('    th, td {');
    html.Add('      border: 1px solid #ddd;');
    html.Add('      padding: 12px;');
    html.Add('      text-align: left;');
    html.Add('    }');
    html.Add('    th {');
    html.Add('      background-color: #4CAF50;');
    html.Add('      color: white;');
    html.Add('    }');
    html.Add('    tr:nth-child(even) {');
    html.Add('      background-color: #f2f2f2;');
    html.Add('    }');
    html.Add('    .success { color: #4CAF50; font-weight: bold; }');
    html.Add('    .failure { color: #f44336; font-weight: bold; }');
    html.Add('    .summary {');
    html.Add('      background-color: #e3f2fd;');
    html.Add('      padding: 15px;');
    html.Add('      border-radius: 5px;');
    html.Add('      margin: 20px 0;');
    html.Add('    }');
    html.Add('    .metric {');
    html.Add('      display: inline-block;');
    html.Add('      margin: 10px 20px;');
    html.Add('    }');
    html.Add('    .metric-label {');
    html.Add('      font-size: 14px;');
    html.Add('      color: #666;');
    html.Add('    }');
    html.Add('    .metric-value {');
    html.Add('      font-size: 24px;');
    html.Add('      font-weight: bold;');
    html.Add('      color: #333;');
    html.Add('    }');
    html.Add('  </style>');
    html.Add('</head>');
    html.Add('<body>');
    html.Add('  <div class="container">');
    html.Add('    <h1>Rapport de Test de Charge</h1>');
    html.Add('    <p>Généré le: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '</p>');

    // Calculer les statistiques globales
    totalTests := Length(Results);
    totalSuccesses := 0;
    totalFailures := 0;
    for i := 0 to High(Results) do
    begin
      totalSuccesses := totalSuccesses + Results[i].Successes;
      totalFailures := totalFailures + Results[i].Failures;
    end;

    // Résumé
    html.Add('    <div class="summary">');
    html.Add('      <h2>Résumé Global</h2>');
    html.Add('      <div class="metric">');
    html.Add('        <div class="metric-label">Tests exécutés</div>');
    html.Add('        <div class="metric-value">' + IntToStr(totalTests) + '</div>');
    html.Add('      </div>');
    html.Add('      <div class="metric">');
    html.Add('        <div class="metric-label">Opérations réussies</div>');
    html.Add('        <div class="metric-value success">' + IntToStr(totalSuccesses) + '</div>');
    html.Add('      </div>');
    html.Add('      <div class="metric">');
    html.Add('        <div class="metric-label">Opérations échouées</div>');
    html.Add('        <div class="metric-value failure">' + IntToStr(totalFailures) + '</div>');
    html.Add('      </div>');
    html.Add('      <div class="metric">');
    html.Add('        <div class="metric-label">Taux de réussite</div>');
    html.Add('        <div class="metric-value">' +
             FormatFloat('0.00', (totalSuccesses * 100.0) / (totalSuccesses + totalFailures)) + '%</div>');
    html.Add('      </div>');
    html.Add('    </div>');

    // Tableau détaillé
    html.Add('    <h2>Résultats Détaillés</h2>');
    html.Add('    <table>');
    html.Add('      <tr>');
    html.Add('        <th>Test</th>');
    html.Add('        <th>Threads</th>');
    html.Add('        <th>Durée (ms)</th>');
    html.Add('        <th>Réussites</th>');
    html.Add('        <th>Échecs</th>');
    html.Add('        <th>Débit (op/s)</th>');
    html.Add('        <th>Latence moy. (ms)</th>');
    html.Add('      </tr>');

    for i := 0 to High(Results) do
    begin
      html.Add('      <tr>');
      html.Add('        <td>' + Results[i].TestName + '</td>');
      html.Add('        <td>' + IntToStr(Results[i].Threads) + '</td>');
      html.Add('        <td>' + IntToStr(Results[i].Duration) + '</td>');
      html.Add('        <td class="success">' + IntToStr(Results[i].Successes) + '</td>');
      html.Add('        <td class="failure">' + IntToStr(Results[i].Failures) + '</td>');
      html.Add('        <td>' + FormatFloat('0.00', Results[i].Throughput) + '</td>');
      html.Add('        <td>' + FormatFloat('0.00', Results[i].AvgLatency) + '</td>');
      html.Add('      </tr>');
    end;

    html.Add('    </table>');
    html.Add('  </div>');
    html.Add('</body>');
    html.Add('</html>');

    html.SaveToFile(FileName);
    WriteLn('Rapport généré: ', FileName);
  finally
    html.Free;
  end;
end;

var
  results: array[0..2] of TTestResult;

begin
  // Simuler des résultats de test
  results[0].TestName := 'Test léger (10 threads)';
  results[0].Threads := 10;
  results[0].Duration := 1523;
  results[0].Successes := 1000;
  results[0].Failures := 0;
  results[0].Throughput := 656.73;
  results[0].AvgLatency := 15.23;

  results[1].TestName := 'Test modéré (50 threads)';
  results[1].Threads := 50;
  results[1].Duration := 3847;
  results[1].Successes := 4950;
  results[1].Failures := 50;
  results[1].Throughput := 1286.34;
  results[1].AvgLatency := 38.47;

  results[2].TestName := 'Test intensif (100 threads)';
  results[2].Threads := 100;
  results[2].Duration := 8234;
  results[2].Successes := 9500;
  results[2].Failures := 500;
  results[2].Throughput := 1154.08;
  results[2].AvgLatency := 82.34;

  GenerateHTMLReport(results, 'load_test_report.html');
end.
```

## Outils externes pour tests de charge

### 1. Apache Bench (ab) - Pour serveurs HTTP

**Installation :**

```bash
# Ubuntu
sudo apt-get install apache2-utils

# Windows (via Apache)
# Télécharger Apache et utiliser ab.exe dans le dossier bin
```

**Utilisation :**

```bash
# Test simple : 1000 requêtes, 10 simultanées
ab -n 1000 -c 10 http://localhost:8080/

# Avec timeout personnalisé
ab -n 5000 -c 50 -t 30 http://localhost:8080/api/endpoint

# Avec headers HTTP
ab -n 1000 -c 10 -H "Authorization: Bearer token123" http://localhost:8080/
```

**Lecture des résultats :**

```
Concurrency Level:      10
Time taken for tests:   5.234 seconds
Complete requests:      1000
Failed requests:        0
Total transferred:      234000 bytes
Requests per second:    191.06 [#/sec] (mean)
Time per request:       52.34 [ms] (mean)
```

### 2. wrk - Outil moderne de benchmarking HTTP

**Installation :**

```bash
# Ubuntu
sudo apt-get install wrk

# Ou compiler depuis les sources
git clone https://github.com/wg/wrk.git
cd wrk
make
```

**Utilisation :**

```bash
# Test basique : 10 threads, 100 connexions, 30 secondes
wrk -t10 -c100 -d30s http://localhost:8080/

# Avec script Lua pour requêtes personnalisées
wrk -t4 -c100 -d30s -s script.lua http://localhost:8080/
```

**Script Lua exemple (script.lua) :**

```lua
wrk.method = "POST"
wrk.body   = '{"username":"test","password":"pass123"}'
wrk.headers["Content-Type"] = "application/json"
```

### 3. JMeter - Outil complet GUI/CLI

**Installation :**

```bash
# Ubuntu
sudo apt-get install jmeter

# Windows
# Télécharger depuis https://jmeter.apache.org/
```

**Utilisation en ligne de commande :**

```bash
# Exécuter un plan de test
jmeter -n -t test_plan.jmx -l results.jtl

# Générer un rapport HTML
jmeter -g results.jtl -o rapport_html/
```

## Stratégies de test de charge

### 1. Test de montée en charge (Ramp-Up)

Augmenter progressivement le nombre d'utilisateurs.

```pascal
program RampUpTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TWorker = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TWorker.Execute;
var
  i, sum: Integer;
begin
  sum := 0;
  for i := 1 to 100000 do
    sum := sum + i;
end;

procedure RampUpTest(StartThreads, EndThreads, StepSize, StepDurationMs: Integer);
var
  currentThreads: Integer;
  threads: array of TWorker;
  i: Integer;
  stepStart, stepDuration: QWord;
begin
  WriteLn('=== Test de montée en charge ===');
  WriteLn(Format('De %d à %d threads par pas de %d', [StartThreads, EndThreads, StepSize]));
  WriteLn;

  currentThreads := StartThreads;

  while currentThreads <= EndThreads do
  begin
    WriteLn(Format('Étape: %d threads...', [currentThreads]));
    stepStart := GetTickCount64;

    SetLength(threads, currentThreads);

    // Créer et démarrer
    for i := 0 to currentThreads - 1 do
      threads[i] := TWorker.Create(False);

    // Attendre
    for i := 0 to currentThreads - 1 do
      threads[i].WaitFor;

    stepDuration := GetTickCount64 - stepStart;
    WriteLn(Format('  Complété en %d ms', [stepDuration]));

    // Libérer
    for i := 0 to currentThreads - 1 do
      threads[i].Free;

    // Pause entre les étapes
    if currentThreads < EndThreads then
    begin
      WriteLn(Format('  Pause de %d ms...', [StepDurationMs]));
      Sleep(StepDurationMs);
    end;

    currentThreads := currentThreads + StepSize;
    WriteLn;
  end;

  WriteLn('Test de montée en charge terminé');
end;

begin
  RampUpTest(10, 100, 10, 1000); // De 10 à 100 threads, +10 chaque seconde
end.
```

### 2. Test de pic (Spike Test)

Augmentation soudaine de la charge.

```pascal
program SpikeTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TWorker = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TWorker.Execute;
var
  i, sum: Integer;
begin
  sum := 0;
  for i := 1 to 50000 do
    sum := sum + i;
end;

procedure ExecutePhase(const PhaseName: string; NbThreads: Integer);
var
  threads: array of TWorker;
  i: Integer;
  startTime, duration: QWord;
begin
  WriteLn('Phase: ', PhaseName, ' (', NbThreads, ' threads)');
  startTime := GetTickCount64;

  SetLength(threads, NbThreads);

  for i := 0 to NbThreads - 1 do
    threads[i] := TWorker.Create(False);

  for i := 0 to NbThreads - 1 do
    threads[i].WaitFor;

  duration := GetTickCount64 - startTime;
  WriteLn('  Durée: ', duration, ' ms');
  WriteLn('  Débit: ', (NbThreads * 1000) div duration, ' threads/sec');
  WriteLn;

  for i := 0 to NbThreads - 1 do
    threads[i].Free;
end;

begin
  WriteLn('=== Test de pic (Spike Test) ===');
  WriteLn('Simulation d''une charge normale suivie d''un pic soudain');
  WriteLn;

  ExecutePhase('Charge normale', 20);
  Sleep(1000);

  ExecutePhase('Charge normale', 20);
  Sleep(1000);

  WriteLn('!!! PIC DE CHARGE !!!');
  ExecutePhase('PIC', 200);
  Sleep(1000);

  ExecutePhase('Retour à la normale', 20);

  WriteLn('Test de pic terminé');
end.
```

### 3. Test d'endurance (Soak Test)

Charge constante pendant une longue période.

```pascal
program SoakTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DateUtils;

type
  TWorker = class(TThread)
  private
    FRunning: Boolean;
  protected
    procedure Execute; override;
  public
    procedure Stop;
    property Running: Boolean read FRunning;
  end;

procedure TWorker.Execute;
var
  i, sum: Integer;
begin
  FRunning := True;
  while not Terminated do
  begin
    sum := 0;
    for i := 1 to 10000 do
      sum := sum + i;
    Sleep(10); // Petite pause pour ne pas saturer
  end;
  FRunning := False;
end;

procedure TWorker.Stop;
begin
  Terminate;
end;

const
  NB_WORKERS = 30;
  TEST_DURATION_MINUTES = 5; // 5 minutes pour le test

var
  workers: array of TWorker;
  i: Integer;
  startTime, currentTime, elapsedMinutes: TDateTime;
  lastReport: TDateTime;

begin
  WriteLn('=== Test d''endurance (Soak Test) ===');
  WriteLn('Durée du test: ', TEST_DURATION_MINUTES, ' minutes');
  WriteLn('Threads actifs: ', NB_WORKERS);
  WriteLn;
  WriteLn('Démarrage du test...');
  WriteLn('(Appuyez sur Ctrl+C pour arrêter prématurément)');
  WriteLn;

  SetLength(workers, NB_WORKERS);

  // Démarrer tous les workers
  for i := 0 to NB_WORKERS - 1 do
    workers[i] := TWorker.Create(False);

  startTime := Now;
  lastReport := Now;

  // Boucle de surveillance
  while MinutesBetween(Now, startTime) < TEST_DURATION_MINUTES do
  begin
    Sleep(1000);

    // Rapport toutes les 30 secondes
    if SecondsBetween(Now, lastReport) >= 30 then
    begin
      elapsedMinutes := MinutesBetween(Now, startTime);
      WriteLn(Format('[%d min] Test en cours... %d/%d workers actifs',
              [elapsedMinutes, NB_WORKERS, NB_WORKERS]));
      lastReport := Now;
    end;
  end;

  WriteLn;
  WriteLn('Arrêt des workers...');

  // Arrêter tous les workers
  for i := 0 to NB_WORKERS - 1 do
    workers[i].Stop;

  for i := 0 to NB_WORKERS - 1 do
    workers[i].WaitFor;

  for i := 0 to NB_WORKERS - 1 do
    workers[i].Free;

  WriteLn;
  WriteLn('=== Test d''endurance terminé ===');
  WriteLn('Durée totale: ', MinutesBetween(Now, startTime), ' minutes');
  WriteLn('✓ Le système a supporté la charge continue');
end.
```

## Bonnes pratiques pour tests de charge

### 1. Checklist avant le test

```
□ Environnement de test isolé (pas de production!)
□ Données de test représentatives
□ Charge cible définie (ex: 1000 utilisateurs simultanés)
□ Critères de succès définis (ex: temps de réponse < 500ms)
□ Monitoring en place (CPU, mémoire, disque, réseau)
□ Plan de secours en cas de problème
□ Sauvegarde avant le test
```

### 2. Métriques importantes à surveiller

**Performance :**
- Temps de réponse (min, max, moyenne, percentiles)
- Débit (requêtes par seconde)
- Latence réseau

**Ressources système :**
- Utilisation CPU (%)
- Utilisation mémoire (MB/GB)
- I/O disque (lectures/écritures par seconde)
- Bande passante réseau (Mbps)

**Fiabilité :**
- Taux d'erreur (%)
- Nombre de timeouts
- Connexions refusées

**Exemple de surveillance système (Ubuntu) :**

```bash
#!/bin/bash
# monitor.sh - Script de surveillance pendant les tests

LOG_FILE="system_monitor.log"
INTERVAL=5  # Échantillonner toutes les 5 secondes

echo "Démarrage de la surveillance système..." | tee -a $LOG_FILE
echo "Timestamp,CPU%,Memory%,DiskIO,NetRX,NetTX" | tee -a $LOG_FILE

while true; do
  TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")
  CPU=$(top -bn1 | grep "Cpu(s)" | awk '{print $2}')
  MEM=$(free | grep Mem | awk '{printf "%.1f", $3/$2 * 100.0}')
  DISK_READ=$(iostat -d -x 1 2 | tail -1 | awk '{print $4}')
  NET_RX=$(cat /sys/class/net/eth0/statistics/rx_bytes)
  NET_TX=$(cat /sys/class/net/eth0/statistics/tx_bytes)

  echo "$TIMESTAMP,$CPU,$MEM,$DISK_READ,$NET_RX,$NET_TX" | tee -a $LOG_FILE

  sleep $INTERVAL
done
```

### 3. Interpréter les résultats

**Signes de bon comportement :**
✅ Temps de réponse stable sous charge  
✅ Utilisation CPU linéaire avec la charge  
✅ Mémoire stable (pas de fuite)  
✅ Taux d'erreur < 1%  
✅ Dégradation gracieuse sous forte charge

**Signes de problème :**
⚠️ Temps de réponse qui augmente exponentiellement  
⚠️ Utilisation mémoire qui croît sans cesse  
⚠️ CPU à 100% avec peu de charge  
⚠️ Beaucoup d'erreurs ou de timeouts  
⚠️ Plantage du système sous charge

**Exemple d'analyse :**

```
Charge: 50 utilisateurs
- Temps de réponse moyen: 120ms ✓
- CPU: 45% ✓
- Mémoire: 2.3 GB stable ✓
- Erreurs: 0% ✓

Charge: 100 utilisateurs
- Temps de réponse moyen: 145ms ✓
- CPU: 78% ✓
- Mémoire: 2.3 GB stable ✓
- Erreurs: 0.2% ✓

Charge: 200 utilisateurs
- Temps de réponse moyen: 3500ms ⚠️
- CPU: 98% ⚠️
- Mémoire: 2.8 GB ✓
- Erreurs: 15% ⚠️

Conclusion: Limite du système entre 100 et 200 utilisateurs
Action: Optimiser le code ou augmenter les ressources serveur
```

## Tests de charge automatisés avec CI/CD

### Intégration GitLab CI

**`.gitlab-ci.yml` :**

```yaml
stages:
  - build
  - test
  - load-test

build:
  stage: build
  script:
    - fpc -O3 mon_serveur.pas
    - fpc -O3 client_test.pas
  artifacts:
    paths:
      - mon_serveur
      - client_test
    expire_in: 1 hour

unit-tests:
  stage: test
  script:
    - ./run_unit_tests.sh

load-test:
  stage: load-test
  only:
    - merge_requests
    - main
  script:
    - echo "Démarrage du serveur..."
    - ./mon_serveur &
    - SERVER_PID=$!
    - sleep 5
    - echo "Exécution des tests de charge..."
    - ./client_test --threads=50 --requests=1000 > load_test_results.txt
    - kill $SERVER_PID
    - echo "Analyse des résultats..."
    - python analyze_results.py load_test_results.txt
  artifacts:
    reports:
      junit: load_test_report.xml
    paths:
      - load_test_results.txt
  allow_failure: false
```

### Script d'analyse des résultats (Python)

**`analyze_results.py` :**

```python
#!/usr/bin/env python3
import sys
import re

def analyze_load_test(filename):
    with open(filename, 'r') as f:
        content = f.read()

    # Extraire les métriques
    success_match = re.search(r'Requêtes réussies:\s+(\d+)', content)
    failure_match = re.search(r'Requêtes échouées:\s+(\d+)', content)
    avg_latency_match = re.search(r'Latence moyenne:\s+([\d.]+)\s+ms', content)

    if not all([success_match, failure_match, avg_latency_match]):
        print("❌ Impossible de parser les résultats")
        sys.exit(1)

    successes = int(success_match.group(1))
    failures = int(failure_match.group(1))
    avg_latency = float(avg_latency_match.group(1))

    print(f"\n=== Analyse des résultats ===")
    print(f"Succès: {successes}")
    print(f"Échecs: {failures}")
    print(f"Latence moyenne: {avg_latency} ms")

    # Critères de validation
    error_rate = (failures / (successes + failures)) * 100 if (successes + failures) > 0 else 100

    print(f"\n=== Validation ===")

    # Taux d'erreur acceptable: < 1%
    if error_rate > 1.0:
        print(f"❌ ÉCHEC: Taux d'erreur trop élevé ({error_rate:.2f}% > 1%)")
        sys.exit(1)
    else:
        print(f"✅ Taux d'erreur acceptable: {error_rate:.2f}%")

    # Latence acceptable: < 500ms
    if avg_latency > 500:
        print(f"❌ ÉCHEC: Latence trop élevée ({avg_latency} ms > 500 ms)")
        sys.exit(1)
    else:
        print(f"✅ Latence acceptable: {avg_latency} ms")

    print(f"\n✅ Tous les tests de charge sont passés avec succès!")
    sys.exit(0)

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python analyze_results.py <fichier_resultats>")
        sys.exit(1)

    analyze_load_test(sys.argv[1])
```

### Intégration GitHub Actions

**`.github/workflows/load-test.yml` :**

```yaml
name: Load Testing

on:
  pull_request:
  push:
    branches: [main]

jobs:
  load-test:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Install FreePascal
      run: |
        sudo apt-get update
        sudo apt-get install -y fpc

    - name: Build applications
      run: |
        fpc -O3 mon_serveur.pas
        fpc -O3 client_test.pas

    - name: Run load tests
      run: |
        ./mon_serveur &
        SERVER_PID=$!
        sleep 5
        ./client_test --threads=50 --requests=1000 | tee load_test_results.txt
        kill $SERVER_PID

    - name: Analyze results
      run: |
        python analyze_results.py load_test_results.txt

    - name: Upload results
      uses: actions/upload-artifact@v3
      if: always()
      with:
        name: load-test-results
        path: load_test_results.txt
```

## Scénarios de test réalistes

### Exemple complet : E-commerce sous charge

```pascal
program ECommerceLoadTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fphttpclient;

type
  TUserScenario = (usNavigate, usSearch, usAddToCart, usCheckout);

  TShopperThread = class(TThread)
  private
    FThreadID: Integer;
    FBaseURL: string;
    FActionsPerformed: Integer;
    FErrors: Integer;
  protected
    procedure Execute; override;
    procedure PerformAction(Scenario: TUserScenario);
  public
    constructor Create(ThreadID: Integer; const BaseURL: string);
    property ActionsPerformed: Integer read FActionsPerformed;
    property Errors: Integer read FErrors;
  end;

constructor TShopperThread.Create(ThreadID: Integer; const BaseURL: string);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FThreadID := ThreadID;
  FBaseURL := BaseURL;
  FActionsPerformed := 0;
  FErrors := 0;
end;

procedure TShopperThread.PerformAction(Scenario: TUserScenario);
var
  client: TFPHTTPClient;
  url: string;
begin
  client := TFPHTTPClient.Create(nil);
  try
    case Scenario of
      usNavigate:
        url := FBaseURL + '/products';
      usSearch:
        url := FBaseURL + '/search?q=laptop';
      usAddToCart:
        url := FBaseURL + '/cart/add/12345';
      usCheckout:
        url := FBaseURL + '/checkout';
    end;

    try
      client.Get(url);
      if client.ResponseStatusCode = 200 then
        Inc(FActionsPerformed)
      else
        Inc(FErrors);
    except
      Inc(FErrors);
    end;
  finally
    client.Free;
  end;
end;

procedure TShopperThread.Execute;
var
  i: Integer;
begin
  // Simuler un parcours utilisateur réaliste
  for i := 1 to 10 do
  begin
    // Navigation (50% du temps)
    if Random(100) < 50 then
      PerformAction(usNavigate);

    Sleep(Random(1000) + 500); // Entre 500ms et 1500ms

    // Recherche (30% du temps)
    if Random(100) < 30 then
      PerformAction(usSearch);

    Sleep(Random(1000) + 500);

    // Ajout au panier (15% du temps)
    if Random(100) < 15 then
      PerformAction(usAddToCart);

    Sleep(Random(500) + 200);

    // Paiement (5% du temps)
    if Random(100) < 5 then
      PerformAction(usCheckout);

    Sleep(Random(2000) + 1000); // Plus long entre les achats
  end;
end;

const
  BASE_URL = 'http://localhost:8080';
  NB_CONCURRENT_USERS = 100;

var
  shoppers: array of TShopperThread;
  i: Integer;
  startTime, totalTime: QWord;
  totalActions, totalErrors: Integer;

begin
  WriteLn('=== Test de charge E-commerce ===');
  WriteLn('URL: ', BASE_URL);
  WriteLn('Utilisateurs simultanés: ', NB_CONCURRENT_USERS);
  WriteLn;
  WriteLn('Simulation de comportement utilisateur réaliste...');
  WriteLn;

  Randomize;
  SetLength(shoppers, NB_CONCURRENT_USERS);
  startTime := GetTickCount64;

  // Créer les utilisateurs
  for i := 0 to NB_CONCURRENT_USERS - 1 do
    shoppers[i] := TShopperThread.Create(i + 1, BASE_URL);

  // Attendre la fin
  for i := 0 to NB_CONCURRENT_USERS - 1 do
  begin
    shoppers[i].WaitFor;
    if (i + 1) mod 10 = 0 then
      WriteLn('Progression: ', i + 1, '/', NB_CONCURRENT_USERS, ' utilisateurs terminés');
  end;

  totalTime := GetTickCount64 - startTime;

  // Statistiques
  totalActions := 0;
  totalErrors := 0;

  for i := 0 to NB_CONCURRENT_USERS - 1 do
  begin
    totalActions := totalActions + shoppers[i].ActionsPerformed;
    totalErrors := totalErrors + shoppers[i].Errors;
  end;

  WriteLn;
  WriteLn('=== Résultats ===');
  WriteLn('Durée totale:        ', totalTime div 1000, ' secondes');
  WriteLn('Actions effectuées:  ', totalActions);
  WriteLn('Erreurs:             ', totalErrors);
  WriteLn('Taux de réussite:    ', ((totalActions - totalErrors) * 100) div totalActions, '%');
  WriteLn('Actions/seconde:     ', (totalActions * 1000) div totalTime);
  WriteLn('Utilisateurs/sec:    ', (NB_CONCURRENT_USERS * 1000) div totalTime);

  if totalErrors > (totalActions div 20) then // Plus de 5% d'erreurs
    WriteLn('⚠️ ATTENTION: Taux d''erreur élevé!')
  else
    WriteLn('✅ Test réussi');

  // Libérer
  for i := 0 to NB_CONCURRENT_USERS - 1 do
    shoppers[i].Free;
end.
```

## Optimisations après tests de charge

### Problèmes courants et solutions

#### 1. Problème : Temps de réponse lents

**Diagnostic :**
```pascal
// Ajouter du profiling
var
  bench: TBenchmark;
begin
  bench := TBenchmark.Create;
  try
    bench.Start;
    ProcessRequest(request);
    WriteLn('Traitement: ', bench.Stop:0:2, ' ms');
  finally
    bench.Free;
  end;
end;
```

**Solutions :**
- Mettre en cache les données fréquemment accédées
- Optimiser les requêtes SQL (index, EXPLAIN)
- Utiliser des connexions persistantes
- Paralléliser les tâches indépendantes

#### 2. Problème : Fuite mémoire

**Diagnostic :**
```pascal
{$IFDEF DEBUG}
uses
  HeapTrc;
{$ENDIF}

begin
  {$IFDEF DEBUG}
  SetHeapTraceOutput('heap.log');
  {$ENDIF}

  // Code de l'application
end.
```

**Solutions :**
- Toujours libérer les objets créés (`try...finally`)
- Utiliser l'interfaces avec comptage de références
- Vérifier les listes et collections
- Utiliser Valgrind (Linux) pour détecter les fuites

#### 3. Problème : Deadlocks

**Diagnostic :**
```pascal
// Ajouter des timeouts
if not Lock.TryEnter(5000) then // 5 secondes
begin
  WriteLn('TIMEOUT: Possible deadlock détecté');
  Exit;
end;
```

**Solutions :**
- Toujours acquérir les locks dans le même ordre
- Utiliser `TryEnter` avec timeout
- Réduire la durée de verrouillage
- Éviter les locks imbriqués quand possible

#### 4. Problème : Saturation CPU

**Diagnostic :**
```bash
# Linux
top -H -p <PID>  # Voir les threads

# Windows (PowerShell)
Get-Process -Id <PID> | Select-Object -Property *
```

**Solutions :**
- Ajouter des `Sleep()` dans les boucles intensives
- Utiliser des événements plutôt que du polling
- Optimiser les algorithmes (O(n²) → O(n log n))
- Distribuer la charge sur plusieurs threads

## Checklist finale pour tests de charge

### Avant le test

- [ ] Environnement de test séparé de la production
- [ ] Données de test représentatives chargées
- [ ] Objectifs de performance définis et documentés
- [ ] Scripts de test préparés et validés
- [ ] Outils de monitoring installés et configurés
- [ ] Plan de rollback en cas de problème
- [ ] Équipe informée du planning du test

### Pendant le test

- [ ] Surveiller en temps réel : CPU, RAM, disque, réseau
- [ ] Logger toutes les erreurs et timeouts
- [ ] Noter tout comportement anormal
- [ ] Prendre des snapshots à intervalles réguliers
- [ ] Documenter les observations

### Après le test

- [ ] Analyser les logs et métriques
- [ ] Générer un rapport avec graphiques
- [ ] Identifier les goulots d'étranglement
- [ ] Documenter les problèmes trouvés
- [ ] Planifier les optimisations nécessaires
- [ ] Archiver les résultats pour comparaison future
- [ ] Communiquer les résultats à l'équipe

## Exemple de rapport final

```
========================================
  RAPPORT DE TEST DE CHARGE
========================================

Date: 2025-10-06
Application: Serveur E-commerce v2.3
Testeur: Équipe QA

1. CONFIGURATION DU TEST
   - Type: Test de montée en charge
   - Durée: 30 minutes
   - Utilisateurs: 10 → 500 (par paliers de 50)
   - Environnement: Ubuntu 22.04, 16GB RAM, 8 cores

2. RÉSULTATS GLOBAUX
   ✅ Requêtes totales:     45,230
   ✅ Requêtes réussies:    44,987 (99.46%)
   ⚠️  Requêtes échouées:      243 (0.54%)
   ✅ Débit moyen:           1,507 req/sec
   ✅ Latence moyenne:       127 ms
   ⚠️  Latence max:          3,245 ms (à 500 users)

3. PERFORMANCE PAR PALIER

   50 users:
   - Latence: 45ms (✅)
   - CPU: 23% (✅)
   - Erreurs: 0% (✅)

   200 users:
   - Latence: 98ms (✅)
   - CPU: 67% (✅)
   - Erreurs: 0.1% (✅)

   500 users:
   - Latence: 1,234ms (⚠️)
   - CPU: 98% (⚠️)
   - Erreurs: 2.3% (❌)

4. GOULOTS D'ÉTRANGLEMENT IDENTIFIÉS
   ❌ Base de données: Connexions saturées à 300+ users
   ⚠️  CPU: Saturation à 500 users
   ✅ Mémoire: Stable (4.2 GB max)
   ✅ Réseau: Pas de saturation

5. RECOMMANDATIONS
   1. Augmenter le pool de connexions DB (50 → 200)
   2. Ajouter un cache Redis pour produits populaires
   3. Optimiser la requête de recherche (index manquant)
   4. Considérer load balancing pour > 300 users

6. CONCLUSION
   ✅ Système stable jusqu'à 200 utilisateurs
   ⚠️  Dégradation acceptable jusqu'à 300 utilisateurs
   ❌ Limite atteinte à 500 utilisateurs

   Recommandation: Optimisations nécessaires avant
   déploiement si > 300 utilisateurs attendus.

========================================
```

## Conclusion

Les tests de charge et de stress sont essentiels pour garantir la fiabilité de vos applications FreePascal/Lazarus en conditions réelles.

**Points clés à retenir :**

✅ **Testez tôt et régulièrement** : Ne pas attendre la mise en production  
✅ **Simulez des scénarios réalistes** : Pas seulement des charges synthétiques  
✅ **Surveillez tout** : CPU, mémoire, disque, réseau, logs  
✅ **Automatisez** : Intégrez les tests dans votre CI/CD  
✅ **Documentez** : Gardez un historique des résultats  
✅ **Agissez** : Utilisez les résultats pour optimiser

**Différences Windows/Ubuntu :**

- **Windows** : Outils PowerShell, Perfmon, Resource Monitor
- **Ubuntu** : Scripts bash, top, htop, vmstat, iostat
- Les concepts restent identiques sur les deux plateformes
- FreePascal permet d'écrire des tests portables facilement

**Outils recommandés :**

| Cas d'usage | Windows | Ubuntu | Multi-plateforme |
|-------------|---------|--------|------------------|
| Tests HTTP | Apache Bench | wrk, Apache Bench | Custom Pascal |
| Monitoring | Perfmon | top, htop | Custom Pascal |
| Profiling | Intel VTune | gprof, Valgrind | FPC profiler |
| Automation | PowerShell | bash | GitLab CI |

**Prochaines étapes :**

1. Créer une suite de tests de charge pour votre application
2. Définir les critères de performance acceptables
3. Mettre en place le monitoring système
4. Intégrer les tests dans votre pipeline CI/CD
5. Établir une baseline de performance
6. Tester régulièrement et comparer avec la baseline

N'oubliez pas : **"You can't improve what you don't measure"** - Mesurez, testez, optimisez !

⏭️ [Fuzzing et tests aléatoires](/18-tests-qualite-code/08-fuzzing-tests-aleatoires.md)
