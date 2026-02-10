🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.10 Réplication et synchronisation

## Introduction

La réplication et la synchronisation de bases de données sont des concepts essentiels pour les applications modernes qui nécessitent haute disponibilité, sauvegarde automatique, ou distribution des données entre plusieurs sites.

Dans ce chapitre, nous allons explorer comment mettre en place ces mécanismes avec FreePascal/Lazarus, sur Windows et Ubuntu.

## Qu'est-ce que la réplication ?

### Analogie simple

Imaginez une bibliothèque principale à Paris qui possède 10 000 livres. Vous voulez ouvrir des succursales à Lyon et Marseille :

**Sans réplication :** Chaque bibliothèque a ses propres livres. Si un livre est ajouté à Paris, il n'apparaît pas automatiquement à Lyon ou Marseille.

**Avec réplication :** Quand un livre est ajouté à Paris (serveur maître), il est automatiquement copié vers Lyon et Marseille (serveurs esclaves). Toutes les bibliothèques ont les mêmes livres.

C'est exactement ce que fait la **réplication de base de données** !

## Pourquoi utiliser la réplication ?

### 1. Haute disponibilité

Si votre serveur principal tombe en panne, un serveur de secours peut prendre le relais immédiatement.

```
Serveur Principal (Windows)  →  Serveur Secours (Ubuntu)
        ↓ panne
   ✅ Bascule automatique
```

### 2. Répartition de charge

Distribuer les lectures sur plusieurs serveurs pour améliorer les performances.

```
           Serveur Maître (écritures)
                   ↓
    ┌──────────────┼──────────────┐
    ↓              ↓              ↓
Serveur 1      Serveur 2      Serveur 3
(lectures)     (lectures)     (lectures)
```

### 3. Sauvegarde en temps réel

Avoir une copie à jour en permanence dans un autre datacenter.

### 4. Distribution géographique

Des serveurs dans différentes régions pour réduire la latence.

```
Paris (Europe) ←→ New York (USA) ←→ Tokyo (Asie)
```

## Types de réplication

### 1. Réplication Maître-Esclave (Master-Slave)

Le modèle le plus courant et le plus simple.

**Caractéristiques :**
- Un serveur **maître** (master) accepte toutes les écritures
- Un ou plusieurs serveurs **esclaves** (slaves) répliquent les données
- Les esclaves sont en **lecture seule**

**Schéma :**

```
    MAÎTRE (Windows/Ubuntu)
    ├─ Écritures (INSERT, UPDATE, DELETE)
    └─ Lectures (SELECT)
           │
           ↓ Réplication
    ┌──────┴──────┐
    ↓             ↓
ESCLAVE 1     ESCLAVE 2
(lectures)    (lectures)
```

**Avantages :**
- Simple à mettre en place
- Lectures distribuées
- Bonne performance

**Inconvénients :**
- Un seul point d'écriture
- Si le maître tombe, pas d'écriture possible (sauf basculement manuel)

### 2. Réplication Maître-Maître (Master-Master)

Deux serveurs qui peuvent tous deux accepter les écritures.

**Schéma :**

```
MAÎTRE 1 (Windows) ←→ MAÎTRE 2 (Ubuntu)
    ↕                      ↕
Écritures             Écritures
Lectures              Lectures
```

**Avantages :**
- Haute disponibilité
- Répartition des écritures

**Inconvénients :**
- Risque de conflits
- Configuration plus complexe
- Nécessite une gestion des conflits

### 3. Réplication en cascade

Les esclaves peuvent eux-mêmes avoir des esclaves.

**Schéma :**

```
      MAÎTRE
         ↓
    ESCLAVE 1
    ┌────┴────┐
    ↓         ↓
ESCLAVE 2  ESCLAVE 3
```

**Utilité :** Réduire la charge sur le maître quand il y a beaucoup d'esclaves.

## Configuration de la réplication PostgreSQL

### Sur le serveur maître (Windows ou Ubuntu)

#### Étape 1 : Modifier postgresql.conf

```ini
# Sur Windows : C:\Program Files\PostgreSQL\15\data\postgresql.conf
# Sur Ubuntu : /etc/postgresql/15/main/postgresql.conf

# Activer la réplication
wal_level = replica
max_wal_senders = 10
max_replication_slots = 10
wal_keep_size = 1GB

# Activer l'archivage (recommandé)
archive_mode = on
archive_command = 'cp %p /var/lib/postgresql/15/archive/%f'  # Ubuntu
# archive_command = 'copy "%p" "C:\\PostgreSQL\\archive\\%f"'  # Windows
```

#### Étape 2 : Modifier pg_hba.conf

```ini
# Autoriser les connexions de réplication
# Sur Windows : C:\Program Files\PostgreSQL\15\data\pg_hba.conf
# Sur Ubuntu : /etc/postgresql/15/main/pg_hba.conf

# Ajouter cette ligne (adapter l'IP selon votre réseau)
host    replication     replicator      192.168.1.0/24      md5
```

#### Étape 3 : Créer un utilisateur de réplication

```sql
-- Se connecter à PostgreSQL en tant que superutilisateur
CREATE ROLE replicator WITH REPLICATION LOGIN PASSWORD 'mot_de_passe_securise';
```

#### Étape 4 : Redémarrer PostgreSQL

**Ubuntu :**
```bash
sudo systemctl restart postgresql
```

**Windows (PowerShell en admin) :**
```powershell
Restart-Service postgresql-x64-15
```

### Sur le serveur esclave (Windows ou Ubuntu)

#### Étape 1 : Arrêter PostgreSQL

**Ubuntu :**
```bash
sudo systemctl stop postgresql
```

**Windows (PowerShell) :**
```powershell
Stop-Service postgresql-x64-15
```

#### Étape 2 : Supprimer les données existantes

**Ubuntu :**
```bash
sudo rm -rf /var/lib/postgresql/15/main/*
```

**Windows (PowerShell) :**
```powershell
Remove-Item "C:\Program Files\PostgreSQL\15\data\*" -Recurse -Force
```

#### Étape 3 : Copier les données du maître

**Ubuntu :**
```bash
sudo -u postgres pg_basebackup -h 192.168.1.10 -D /var/lib/postgresql/15/main \
  -U replicator -P -v -R -X stream -C -S replica1
```

**Windows (PowerShell) :**
```powershell
pg_basebackup.exe -h 192.168.1.10 -D "C:\Program Files\PostgreSQL\15\data" `
  -U replicator -P -v -R -X stream -C -S replica1
```

**Paramètres :**
- `-h` : Adresse du serveur maître
- `-D` : Répertoire de destination
- `-U` : Utilisateur de réplication
- `-P` : Afficher la progression
- `-v` : Mode verbeux
- `-R` : Créer automatiquement standby.signal
- `-X stream` : Streamer les WAL
- `-C` : Créer le slot de réplication
- `-S` : Nom du slot

#### Étape 4 : Configurer postgresql.conf de l'esclave

```ini
# Sur l'esclave
hot_standby = on
```

#### Étape 5 : Démarrer PostgreSQL sur l'esclave

**Ubuntu :**
```bash
sudo systemctl start postgresql
```

**Windows :**
```powershell
Start-Service postgresql-x64-15
```

### Vérification de la réplication

**Sur le maître :**

```sql
-- Voir les connexions de réplication actives
SELECT * FROM pg_stat_replication;
```

**Sur l'esclave :**

```sql
-- Vérifier le statut
SELECT pg_is_in_recovery();  -- Doit retourner true
```

## Gestion de la réplication en FreePascal

### Classe de monitoring de réplication

```pascal
unit ReplicationMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, PQConnection;

type
  TReplicationStatus = record
    ClientAddr: String;
    State: String;
    SentLSN: String;
    WriteLSN: String;
    FlushLSN: String;
    ReplayLSN: String;
    SyncPriority: Integer;
    SyncState: String;
    Lag: Integer;  // en secondes
  end;

  TReplicationMonitor = class
  private
    FConnection: TSQLConnection;
    function IsMaster: Boolean;
  public
    constructor Create(AConnection: TSQLConnection);

    function GetReplicationStatus: TArray<TReplicationStatus>;
    function GetReplicationLag: Integer;
    function IsReplicaHealthy: Boolean;
    procedure PrintStatus;
  end;

implementation

constructor TReplicationMonitor.Create(AConnection: TSQLConnection);
begin
  FConnection := AConnection;
end;

function TReplicationMonitor.IsMaster: Boolean;
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := 'SELECT pg_is_in_recovery()';
    Query.Open;
    Result := not Query.Fields[0].AsBoolean;
    Query.Close;
  finally
    Query.Free;
  end;
end;

function TReplicationMonitor.GetReplicationStatus: TArray<TReplicationStatus>;
var
  Query: TSQLQuery;
  Status: TReplicationStatus;
  StatusList: array of TReplicationStatus;
begin
  SetLength(StatusList, 0);

  if not IsMaster then
  begin
    WriteLn('Ce serveur est un esclave, pas de status de réplication');
    Exit(StatusList);
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT ' +
      '  client_addr::text, ' +
      '  state, ' +
      '  sent_lsn::text, ' +
      '  write_lsn::text, ' +
      '  flush_lsn::text, ' +
      '  replay_lsn::text, ' +
      '  sync_priority, ' +
      '  sync_state, ' +
      '  EXTRACT(EPOCH FROM (now() - write_time))::integer as lag ' +
      'FROM pg_stat_replication';

    Query.Open;
    while not Query.EOF do
    begin
      Status.ClientAddr := Query.FieldByName('client_addr').AsString;
      Status.State := Query.FieldByName('state').AsString;
      Status.SentLSN := Query.FieldByName('sent_lsn').AsString;
      Status.WriteLSN := Query.FieldByName('write_lsn').AsString;
      Status.FlushLSN := Query.FieldByName('flush_lsn').AsString;
      Status.ReplayLSN := Query.FieldByName('replay_lsn').AsString;
      Status.SyncPriority := Query.FieldByName('sync_priority').AsInteger;
      Status.SyncState := Query.FieldByName('sync_state').AsString;
      Status.Lag := Query.FieldByName('lag').AsInteger;

      SetLength(StatusList, Length(StatusList) + 1);
      StatusList[High(StatusList)] := Status;

      Query.Next;
    end;
    Query.Close;
  finally
    Query.Free;
  end;

  Result := StatusList;
end;

function TReplicationMonitor.GetReplicationLag: Integer;
var
  Query: TSQLQuery;
begin
  Result := -1;

  if IsMaster then
    Exit;  // Le maître n'a pas de lag

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT EXTRACT(EPOCH FROM (now() - pg_last_xact_replay_timestamp()))::integer';

    Query.Open;
    if not Query.EOF then
      Result := Query.Fields[0].AsInteger;
    Query.Close;
  finally
    Query.Free;
  end;
end;

function TReplicationMonitor.IsReplicaHealthy: Boolean;
var
  Lag: Integer;
  Status: string;
begin
  Result := False;

  if IsMaster then
  begin
    // Vérifier que les esclaves sont connectés
    Status := GetReplicationStatus;
    Result := Length(Status) > 0;
  end
  else
  begin
    // Vérifier le lag de réplication
    Lag := GetReplicationLag;
    Result := (Lag >= 0) and (Lag < 10);  // Moins de 10 secondes de retard
  end;
end;

procedure TReplicationMonitor.PrintStatus;
var
  Status: TArray<TReplicationStatus>;
  S: TReplicationStatus;
  Lag: Integer;
begin
  if IsMaster then
  begin
    WriteLn('=== Statut du MAÎTRE ===');
    WriteLn;
    Status := GetReplicationStatus;

    if Length(Status) = 0 then
    begin
      WriteLn('Aucun esclave connecté');
      Exit;
    end;

    WriteLn('Esclaves connectés: ', Length(Status));
    WriteLn;

    for S in Status do
    begin
      WriteLn('Esclave: ', S.ClientAddr);
      WriteLn('  État: ', S.State);
      WriteLn('  Sync: ', S.SyncState);
      WriteLn('  Lag: ', S.Lag, ' secondes');
      WriteLn('  LSN envoyé: ', S.SentLSN);
      WriteLn('  LSN rejoué: ', S.ReplayLSN);
      WriteLn;
    end;
  end
  else
  begin
    WriteLn('=== Statut de l''ESCLAVE ===');
    WriteLn;
    Lag := GetReplicationLag;

    if Lag >= 0 then
    begin
      WriteLn('Lag de réplication: ', Lag, ' secondes');
      if Lag < 5 then
        WriteLn('Status: ✅ Excellent')
      else if Lag < 30 then
        WriteLn('Status: ⚠️  Acceptable')
      else
        WriteLn('Status: ❌ Problème de réplication');
    end
    else
      WriteLn('Impossible de déterminer le lag');
  end;
end;

end.
```

### Utilisation du monitoring

```pascal
program MonitorReplication;

{$mode objfpc}{$H+}

uses
  SysUtils, PQConnection, ReplicationMonitor;

var
  Connection: TPQConnection;
  Transaction: TSQLTransaction;
  Monitor: TReplicationMonitor;

begin
  Connection := TPQConnection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := 'mydb';
    Connection.HostName := 'localhost';
    Connection.UserName := 'postgres';
    Connection.Password := 'password';

    Connection.Open;

    Monitor := TReplicationMonitor.Create(Connection);
    try
      Monitor.PrintStatus;

      if not Monitor.IsReplicaHealthy then
        WriteLn('⚠️  ATTENTION : Problème de réplication détecté');
    finally
      Monitor.Free;
    end;
  finally
    Connection.Close;
    Connection.Free;
    Transaction.Free;
  end;
end.
```

## Basculement automatique (Failover)

Le basculement automatique permet de promouvoir un esclave en maître si le maître tombe en panne.

### Outil de basculement

```pascal
unit FailoverManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, Process;

type
  TFailoverManager = class
  private
    FMasterHost: String;
    FReplicaHost: String;
    FDatabaseName: String;
    FUserName: String;
    FPassword: String;

    function TestConnection(const Host: String): Boolean;
    function PromoteReplica: Boolean;
    procedure NotifyClients;
  public
    constructor Create(const AMasterHost, AReplicaHost, ADatabaseName,
      AUserName, APassword: String);

    function CheckMasterHealth: Boolean;
    function PerformFailover: Boolean;
  end;

implementation

constructor TFailoverManager.Create(const AMasterHost, AReplicaHost,
  ADatabaseName, AUserName, APassword: String);
begin
  FMasterHost := AMasterHost;
  FReplicaHost := AReplicaHost;
  FDatabaseName := ADatabaseName;
  FUserName := AUserName;
  FPassword := APassword;
end;

function TFailoverManager.TestConnection(const Host: String): Boolean;
var
  Connection: TSQLConnector;
begin
  Result := False;
  Connection := TSQLConnector.Create(nil);
  try
    Connection.ConnectorType := 'PostgreSQL';
    Connection.HostName := Host;
    Connection.DatabaseName := FDatabaseName;
    Connection.UserName := FUserName;
    Connection.Password := FPassword;

    try
      Connection.Open;
      Result := Connection.Connected;
      Connection.Close;
    except
      Result := False;
    end;
  finally
    Connection.Free;
  end;
end;

function TFailoverManager.CheckMasterHealth: Boolean;
begin
  Result := TestConnection(FMasterHost);

  if not Result then
    WriteLn('⚠️  Le maître ne répond pas!');
end;

function TFailoverManager.PromoteReplica: Boolean;
var
  Process: TProcess;
  Output: String;
begin
  Result := False;

  WriteLn('Promotion de l''esclave en maître...');

  Process := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    Process.Executable := 'psql.exe';
    {$ELSE}
    Process.Executable := 'psql';
    {$ENDIF}

    Process.Parameters.Add('-h');
    Process.Parameters.Add(FReplicaHost);
    Process.Parameters.Add('-U');
    Process.Parameters.Add(FUserName);
    Process.Parameters.Add('-d');
    Process.Parameters.Add(FDatabaseName);
    Process.Parameters.Add('-c');
    Process.Parameters.Add('SELECT pg_promote()');

    Process.Options := [poWaitOnExit, poUsePipes];

    try
      Process.Execute;

      SetLength(Output, Process.Output.NumBytesAvailable);
      Process.Output.Read(Output[1], Length(Output));

      Result := Process.ExitStatus = 0;

      if Result then
        WriteLn('✅ Esclave promu en maître avec succès')
      else
        WriteLn('❌ Échec de la promotion: ', Output);
    except
      on E: Exception do
        WriteLn('❌ Erreur lors de la promotion: ', E.Message);
    end;
  finally
    Process.Free;
  end;
end;

procedure TFailoverManager.NotifyClients;
begin
  // Ici, vous pourriez :
  // - Envoyer des emails
  // - Mettre à jour un fichier de configuration
  // - Notifier un load balancer
  // - Envoyer des alertes

  WriteLn('📧 Notification envoyée aux administrateurs');
  WriteLn('🔄 Mise à jour de la configuration...');
end;

function TFailoverManager.PerformFailover: Boolean;
begin
  Result := False;

  WriteLn('=== DÉBUT DU BASCULEMENT ===');
  WriteLn;

  // 1. Vérifier que le maître est vraiment en panne
  WriteLn('1. Vérification de l''état du maître...');
  if CheckMasterHealth then
  begin
    WriteLn('✅ Le maître répond, basculement annulé');
    Exit(False);
  end;

  // 2. Vérifier que l'esclave est accessible
  WriteLn('2. Vérification de l''état de l''esclave...');
  if not TestConnection(FReplicaHost) then
  begin
    WriteLn('❌ L''esclave n''est pas accessible, basculement impossible');
    Exit(False);
  end;

  // 3. Promouvoir l'esclave
  WriteLn('3. Promotion de l''esclave...');
  if not PromoteReplica then
  begin
    WriteLn('❌ Échec de la promotion');
    Exit(False);
  end;

  // 4. Notifier les clients
  WriteLn('4. Notification...');
  NotifyClients;

  WriteLn;
  WriteLn('=== BASCULEMENT TERMINÉ ===');
  WriteLn('Nouveau maître: ', FReplicaHost);

  Result := True;
end;

end.
```

### Daemon de surveillance

```pascal
program FailoverDaemon;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, DateUtils, FailoverManager;

const
  CHECK_INTERVAL = 30;  // Vérifier toutes les 30 secondes

var
  Manager: TFailoverManager;
  FailureCount: Integer;
  LastCheck: TDateTime;

procedure CheckAndFailover;
begin
  if not Manager.CheckMasterHealth then
  begin
    Inc(FailureCount);
    WriteLn(Format('[%s] Échec de connexion au maître (#%d)',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), FailureCount]));

    // Basculer après 3 échecs consécutifs (90 secondes)
    if FailureCount >= 3 then
    begin
      WriteLn('⚠️  BASCULEMENT AUTOMATIQUE DÉCLENCHÉ');
      if Manager.PerformFailover then
      begin
        WriteLn('✅ Basculement réussi, arrêt du daemon');
        Halt(0);
      end
      else
      begin
        WriteLn('❌ Échec du basculement, nouvelle tentative dans 30s');
      end;
    end;
  end
  else
  begin
    if FailureCount > 0 then
      WriteLn('✅ Le maître répond à nouveau');
    FailureCount := 0;
  end;
end;

begin
  WriteLn('=== Daemon de Basculement Automatique ===');
  WriteLn('Surveillance du maître toutes les ', CHECK_INTERVAL, ' secondes');
  WriteLn;

  Manager := TFailoverManager.Create(
    '192.168.1.10',  // Maître
    '192.168.1.11',  // Réplica
    'mydb',
    'postgres',
    'password'
  );

  try
    FailureCount := 0;
    LastCheck := Now;

    while True do
    begin
      if SecondsBetween(Now, LastCheck) >= CHECK_INTERVAL then
      begin
        CheckAndFailover;
        LastCheck := Now;
      end;

      Sleep(1000);  // Dormir 1 seconde
    end;
  finally
    Manager.Free;
  end;
end.
```

## Synchronisation bidirectionnelle

Pour les applications nécessitant une synchronisation entre plusieurs bases (par exemple, une base centrale et des bases locales), voici une approche simple.

### Architecture de synchronisation

```
  Base Centrale (Cloud)
         ↕️
  Synchronisation
         ↕️
  ┌──────┴──────┐
  ↓             ↓
Base Locale 1  Base Locale 2
(Windows)      (Ubuntu)
```

### Gestionnaire de synchronisation

```pascal
unit SyncManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DateUtils, fgl;

type
  TSyncDirection = (sdUpload, sdDownload, sdBidirectional);

  TSyncRecord = record
    TableName: String;
    LastSync: TDateTime;
    Direction: TSyncDirection;
  end;

  TSyncRecordList = specialize TFPGList<TSyncRecord>;

  TSyncManager = class
  private
    FLocalConnection: TSQLConnection;
    FRemoteConnection: TSQLConnection;
    FSyncRecords: TSyncRecordList;

    function GetLastSyncTime(const TableName: String): TDateTime;
    procedure SetLastSyncTime(const TableName: String; SyncTime: TDateTime);
    procedure SyncTableUp(const TableName: String);
    procedure SyncTableDown(const TableName: String);
  public
    constructor Create(ALocalConnection, ARemoteConnection: TSQLConnection);
    destructor Destroy; override;

    procedure RegisterTable(const TableName: String; Direction: TSyncDirection);
    procedure SyncAll;
    procedure SyncTable(const TableName: String);

    property SyncRecords: TSyncRecordList read FSyncRecords;
  end;

implementation

constructor TSyncManager.Create(ALocalConnection, ARemoteConnection: TSQLConnection);
begin
  FLocalConnection := ALocalConnection;
  FRemoteConnection := ARemoteConnection;
  FSyncRecords := TSyncRecordList.Create;
end;

destructor TSyncManager.Destroy;
begin
  FSyncRecords.Free;
  inherited;
end;

procedure TSyncManager.RegisterTable(const TableName: String; Direction: TSyncDirection);
var
  SyncRec: TSyncRecord;
begin
  SyncRec.TableName := TableName;
  SyncRec.LastSync := GetLastSyncTime(TableName);
  SyncRec.Direction := Direction;

  FSyncRecords.Add(SyncRec);
end;

function TSyncManager.GetLastSyncTime(const TableName: String): TDateTime;
var
  Query: TSQLQuery;
begin
  Result := 0;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FLocalConnection;
    Query.SQL.Text :=
      'SELECT last_sync FROM sync_status WHERE table_name = :table_name';
    Query.Params.ParamByName('table_name').AsString := TableName;

    try
      Query.Open;
      if not Query.EOF then
        Result := Query.FieldByName('last_sync').AsDateTime;
      Query.Close;
    except
      // Table sync_status n'existe pas encore
      Result := 0;
    end;
  finally
    Query.Free;
  end;
end;

procedure TSyncManager.SetLastSyncTime(const TableName: String; SyncTime: TDateTime);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FLocalConnection;

    // Créer la table si elle n'existe pas
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS sync_status (' +
      '  table_name VARCHAR(100) PRIMARY KEY, ' +
      '  last_sync TIMESTAMP' +
      ')';
    Query.ExecSQL;

    // Mettre à jour ou insérer
    Query.SQL.Text :=
      'INSERT INTO sync_status (table_name, last_sync) ' +
      'VALUES (:table_name, :last_sync) ' +
      'ON CONFLICT (table_name) DO UPDATE SET last_sync = :last_sync';
    Query.Params.ParamByName('table_name').AsString := TableName;
    Query.Params.ParamByName('last_sync').AsDateTime := SyncTime;
    Query.ExecSQL;

    FLocalConnection.Transaction.Commit;
  finally
    Query.Free;
  end;
end;

procedure TSyncManager.SyncTableUp(const TableName: String);
var
  LocalQuery, RemoteQuery: TSQLQuery;
  LastSync: TDateTime;
  InsertCount, UpdateCount: Integer;
begin
  WriteLn('📤 Synchronisation montante: ', TableName);

  LastSync := GetLastSyncTime(TableName);
  InsertCount := 0;
  UpdateCount := 0;

  LocalQuery := TSQLQuery.Create(nil);
  RemoteQuery := TSQLQuery.Create(nil);
  try
    LocalQuery.Database := FLocalConnection;
    RemoteQuery.Database := FRemoteConnection;

    // Récupérer les enregistrements modifiés localement
    LocalQuery.SQL.Text := Format(
      'SELECT * FROM %s WHERE updated_at > :last_sync',
      [TableName]
    );
    LocalQuery.Params.ParamByName('last_sync').AsDateTime := LastSync;
    LocalQuery.Open;

    FRemoteConnection.Transaction.StartTransaction;
    try
      while not LocalQuery.EOF do
      begin
        // Essayer d'insérer ou mettre à jour sur le serveur distant
        RemoteQuery.SQL.Text := Format(
          'INSERT INTO %s (id, data, updated_at) ' +
          'VALUES (:id, :data, :updated_at) ' +
          'ON CONFLICT (id) DO UPDATE SET data = :data, updated_at = :updated_at',
          [TableName]
        );

        RemoteQuery.Params.ParamByName('id').AsInteger :=
          LocalQuery.FieldByName('id').AsInteger;
        RemoteQuery.Params.ParamByName('data').AsString :=
          LocalQuery.FieldByName('data').AsString;
        RemoteQuery.Params.ParamByName('updated_at').AsDateTime :=
          LocalQuery.FieldByName('updated_at').AsDateTime;

        RemoteQuery.ExecSQL;
        Inc(UpdateCount);

        LocalQuery.Next;
      end;

      FRemoteConnection.Transaction.Commit;
      WriteLn(Format('  ✅ %d enregistrement(s) synchronisé(s)', [UpdateCount]));

      // Mettre à jour le timestamp de synchronisation
      SetLastSyncTime(TableName, Now);
    except
      on E: Exception do
      begin
        FRemoteConnection.Transaction.Rollback;
        WriteLn('  ❌ Erreur: ', E.Message);
        raise;
      end;
    end;

    LocalQuery.Close;
  finally
    LocalQuery.Free;
    RemoteQuery.Free;
  end;
end;

procedure TSyncManager.SyncTableDown(const TableName: String);
var
  LocalQuery, RemoteQuery: TSQLQuery;
  LastSync: TDateTime;
  UpdateCount: Integer;
begin
  WriteLn('📥 Synchronisation descendante: ', TableName);

  LastSync := GetLastSyncTime(TableName);
  UpdateCount := 0;

  LocalQuery := TSQLQuery.Create(nil);
  RemoteQuery := TSQLQuery.Create(nil);
  try
    LocalQuery.Database := FLocalConnection;
    RemoteQuery.Database := FRemoteConnection;

    // Récupérer les enregistrements modifiés sur le serveur distant
    RemoteQuery.SQL.Text := Format(
      'SELECT * FROM %s WHERE updated_at > :last_sync',
      [TableName]
    );
    RemoteQuery.Params.ParamByName('last_sync').AsDateTime := LastSync;
    RemoteQuery.Open;

    FLocalConnection.Transaction.StartTransaction;
    try
      while not RemoteQuery.EOF do
      begin
        // Insérer ou mettre à jour localement
        LocalQuery.SQL.Text := Format(
          'INSERT INTO %s (id, data, updated_at) ' +
          'VALUES (:id, :data, :updated_at) ' +
          'ON CONFLICT (id) DO UPDATE SET data = :data, updated_at = :updated_at',
          [TableName]
        );

        LocalQuery.Params.ParamByName('id').AsInteger :=
          RemoteQuery.FieldByName('id').AsInteger;
        LocalQuery.Params.ParamByName('data').AsString :=
          RemoteQuery.FieldByName('data').AsString;
        LocalQuery.Params.ParamByName('updated_at').AsDateTime :=
          RemoteQuery.FieldByName('updated_at').AsDateTime;

        LocalQuery.ExecSQL;
        Inc(UpdateCount);

        RemoteQuery.Next;
      end;

      FLocalConnection.Transaction.Commit;
      WriteLn(Format('  ✅ %d enregistrement(s) synchronisé(s)', [UpdateCount]));

      // Mettre à jour le timestamp de synchronisation
      SetLastSyncTime(TableName, Now);
    except
      on E: Exception do
      begin
        FLocalConnection.Transaction.Rollback;
        WriteLn('  ❌ Erreur: ', E.Message);
        raise;
      end;
    end;

    RemoteQuery.Close;
  finally
    LocalQuery.Free;
    RemoteQuery.Free;
  end;
end;

procedure TSyncManager.SyncTable(const TableName: String);
var
  i: Integer;
  SyncRec: TSyncRecord;
begin
  // Trouver la configuration de synchronisation pour cette table
  for i := 0 to FSyncRecords.Count - 1 do
  begin
    SyncRec := FSyncRecords[i];
    if SyncRec.TableName = TableName then
    begin
      case SyncRec.Direction of
        sdUpload:
          SyncTableUp(TableName);
        sdDownload:
          SyncTableDown(TableName);
        sdBidirectional:
        begin
          SyncTableDown(TableName);  // D'abord télécharger
          SyncTableUp(TableName);    // Puis envoyer
        end;
      end;
      Exit;
    end;
  end;

  WriteLn('⚠️  Table non enregistrée pour synchronisation: ', TableName);
end;

procedure TSyncManager.SyncAll;
var
  SyncRec: TSyncRecord;
  StartTime: TDateTime;
  Duration: Integer;
begin
  WriteLn('=== DÉBUT DE LA SYNCHRONISATION ===');
  WriteLn('Date: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  WriteLn;

  StartTime := Now;

  for SyncRec in FSyncRecords do
  begin
    try
      SyncTable(SyncRec.TableName);
    except
      on E: Exception do
        WriteLn('❌ Échec synchronisation ', SyncRec.TableName, ': ', E.Message);
    end;
  end;

  Duration := MilliSecondsBetween(Now, StartTime);

  WriteLn;
  WriteLn('=== SYNCHRONISATION TERMINÉE ===');
  WriteLn(Format('Durée: %d ms', [Duration]));
end;

end.
```

### Utilisation du SyncManager

```pascal
program SyncApp;

{$mode objfpc}{$H+}

uses
  SysUtils, PQConnection, SyncManager;

var
  LocalConn, RemoteConn: TPQConnection;
  LocalTrans, RemoteTrans: TSQLTransaction;
  Sync: TSyncManager;

begin
  // Connexion locale (Windows ou Ubuntu)
  LocalConn := TPQConnection.Create(nil);
  LocalTrans := TSQLTransaction.Create(nil);
  LocalConn.Transaction := LocalTrans;
  LocalConn.DatabaseName := 'local_db';
  LocalConn.HostName := 'localhost';
  LocalConn.UserName := 'postgres';
  LocalConn.Password := 'password';

  // Connexion distante (Cloud)
  RemoteConn := TPQConnection.Create(nil);
  RemoteTrans := TSQLTransaction.Create(nil);
  RemoteConn.Transaction := RemoteTrans;
  RemoteConn.DatabaseName := 'cloud_db';
  RemoteConn.HostName := 'cloud.example.com';
  RemoteConn.UserName := 'postgres';
  RemoteConn.Password := 'password';

  try
    LocalConn.Open;
    RemoteConn.Open;

    Sync := TSyncManager.Create(LocalConn, RemoteConn);
    try
      // Configurer les tables à synchroniser
      Sync.RegisterTable('clients', sdBidirectional);
      Sync.RegisterTable('produits', sdDownload);  // Seulement du cloud vers local
      Sync.RegisterTable('logs', sdUpload);        // Seulement du local vers cloud

      // Lancer la synchronisation
      Sync.SyncAll;
    finally
      Sync.Free;
    end;
  finally
    LocalConn.Close;
    RemoteConn.Close;
    LocalConn.Free;
    RemoteConn.Free;
    LocalTrans.Free;
    RemoteTrans.Free;
  end;
end.
```

## Gestion des conflits

Lors d'une synchronisation bidirectionnelle, des conflits peuvent survenir quand le même enregistrement est modifié des deux côtés.

### Stratégies de résolution de conflits

#### 1. Last Write Wins (Le dernier gagne)

La modification la plus récente écrase les autres.

```pascal
type
  TConflictResolution = (crLastWriteWins, crFirstWriteWins, crManual);

procedure TSyncManager.SyncTableBidirectional(const TableName: String;
  Resolution: TConflictResolution);
var
  LocalQuery, RemoteQuery: TSQLQuery;
  ConflictCount: Integer;
begin
  ConflictCount := 0;

  // Détecter les conflits
  LocalQuery := TSQLQuery.Create(nil);
  RemoteQuery := TSQLQuery.Create(nil);
  try
    LocalQuery.Database := FLocalConnection;
    RemoteQuery.Database := FRemoteConnection;

    // Trouver les enregistrements modifiés des deux côtés
    LocalQuery.SQL.Text := Format(
      'SELECT l.id, l.updated_at as local_time, r.updated_at as remote_time ' +
      'FROM %s l ' +
      'INNER JOIN %s r ON l.id = r.id ' +
      'WHERE l.updated_at > :last_sync AND r.updated_at > :last_sync',
      [TableName, TableName]
    );

    // Résoudre selon la stratégie
    case Resolution of
      crLastWriteWins:
      begin
        // Garder la version avec le timestamp le plus récent
        WriteLn('⚠️  Résolution des conflits: dernière écriture gagne');
      end;

      crFirstWriteWins:
      begin
        // Garder la version avec le timestamp le plus ancien
        WriteLn('⚠️  Résolution des conflits: première écriture gagne');
      end;

      crManual:
      begin
        // Demander à l'utilisateur
        WriteLn('⚠️  Conflit détecté, intervention manuelle requise');
      end;
    end;
  finally
    LocalQuery.Free;
    RemoteQuery.Free;
  end;
end;
```

#### 2. Version avec numéro de version

Ajouter un champ `version` qui s'incrémente à chaque modification.

```sql
-- Structure de table avec versioning
CREATE TABLE clients (
    id SERIAL PRIMARY KEY,
    nom VARCHAR(100),
    email VARCHAR(255),
    version INTEGER DEFAULT 1,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Trigger pour incrémenter la version
CREATE OR REPLACE FUNCTION increment_version()
RETURNS TRIGGER AS $$
BEGIN
    NEW.version := OLD.version + 1;
    NEW.updated_at := NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER clients_version_trigger
BEFORE UPDATE ON clients
FOR EACH ROW
EXECUTE FUNCTION increment_version();
```

**Synchronisation avec versioning :**

```pascal
procedure SyncWithVersioning(LocalConn, RemoteConn: TSQLConnection;
  const TableName: String; RecordID: Integer);
var
  LocalQuery, RemoteQuery: TSQLQuery;
  LocalVersion, RemoteVersion: Integer;
begin
  LocalQuery := TSQLQuery.Create(nil);
  RemoteQuery := TSQLQuery.Create(nil);
  try
    LocalQuery.Database := LocalConn;
    RemoteQuery.Database := RemoteConn;

    // Récupérer les versions
    LocalQuery.SQL.Text := Format(
      'SELECT version FROM %s WHERE id = :id', [TableName]
    );
    LocalQuery.Params.ParamByName('id').AsInteger := RecordID;
    LocalQuery.Open;
    LocalVersion := LocalQuery.FieldByName('version').AsInteger;
    LocalQuery.Close;

    RemoteQuery.SQL.Text := Format(
      'SELECT version FROM %s WHERE id = :id', [TableName]
    );
    RemoteQuery.Params.ParamByName('id').AsInteger := RecordID;
    RemoteQuery.Open;
    RemoteVersion := RemoteQuery.FieldByName('version').AsInteger;
    RemoteQuery.Close;

    // Synchroniser la version la plus récente
    if LocalVersion > RemoteVersion then
    begin
      WriteLn(Format('  Local plus récent (v%d > v%d) - envoi vers serveur',
        [LocalVersion, RemoteVersion]));
      // Copier local → remote
    end
    else if RemoteVersion > LocalVersion then
    begin
      WriteLn(Format('  Serveur plus récent (v%d > v%d) - téléchargement',
        [RemoteVersion, LocalVersion]));
      // Copier remote → local
    end
    else
      WriteLn('  Versions identiques (v', LocalVersion, ') - aucune action');
  finally
    LocalQuery.Free;
    RemoteQuery.Free;
  end;
end;
```

## Synchronisation incrémentale avec journalisation

Pour de meilleures performances, utilisez une table de journal qui enregistre tous les changements.

### Table de journal (Change Log)

```sql
CREATE TABLE change_log (
    id SERIAL PRIMARY KEY,
    table_name VARCHAR(100) NOT NULL,
    record_id INTEGER NOT NULL,
    operation VARCHAR(10) NOT NULL,  -- INSERT, UPDATE, DELETE
    data JSONB,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    synced BOOLEAN DEFAULT FALSE
);

CREATE INDEX idx_change_log_synced ON change_log(synced, created_at);
```

### Triggers pour capturer les changements

```sql
-- Fonction générique pour logger les changements
CREATE OR REPLACE FUNCTION log_changes()
RETURNS TRIGGER AS $$
BEGIN
    IF (TG_OP = 'DELETE') THEN
        INSERT INTO change_log (table_name, record_id, operation, data)
        VALUES (TG_TABLE_NAME, OLD.id, 'DELETE', row_to_json(OLD)::jsonb);
        RETURN OLD;
    ELSIF (TG_OP = 'UPDATE') THEN
        INSERT INTO change_log (table_name, record_id, operation, data)
        VALUES (TG_TABLE_NAME, NEW.id, 'UPDATE', row_to_json(NEW)::jsonb);
        RETURN NEW;
    ELSIF (TG_OP = 'INSERT') THEN
        INSERT INTO change_log (table_name, record_id, operation, data)
        VALUES (TG_TABLE_NAME, NEW.id, 'INSERT', row_to_json(NEW)::jsonb);
        RETURN NEW;
    END IF;
END;
$$ LANGUAGE plpgsql;

-- Appliquer le trigger sur les tables à synchroniser
CREATE TRIGGER clients_change_log
AFTER INSERT OR UPDATE OR DELETE ON clients
FOR EACH ROW EXECUTE FUNCTION log_changes();

CREATE TRIGGER produits_change_log
AFTER INSERT OR UPDATE OR DELETE ON produits
FOR EACH ROW EXECUTE FUNCTION log_changes();
```

### Synchronisation basée sur le journal

```pascal
unit ChangeLogSync;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, fpjson, jsonparser;

type
  TChangeLogSync = class
  private
    FLocalConnection: TSQLConnection;
    FRemoteConnection: TSQLConnection;

    procedure ApplyChange(Connection: TSQLConnection;
      const TableName, Operation: String; RecordID: Integer; Data: TJSONObject);
  public
    constructor Create(ALocalConnection, ARemoteConnection: TSQLConnection);

    function SyncChanges(Direction: String): Integer;
    procedure MarkAsSynced(ChangeIDs: array of Integer);
  end;

implementation

constructor TChangeLogSync.Create(ALocalConnection, ARemoteConnection: TSQLConnection);
begin
  FLocalConnection := ALocalConnection;
  FRemoteConnection := ARemoteConnection;
end;

procedure TChangeLogSync.ApplyChange(Connection: TSQLConnection;
  const TableName, Operation: String; RecordID: Integer; Data: TJSONObject);
var
  Query: TSQLQuery;
  SQL: String;
  i: Integer;
  FieldName: String;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;

    case Operation of
      'INSERT':
      begin
        // Construire INSERT depuis JSON
        SQL := Format('INSERT INTO %s (', [TableName]);
        for i := 0 to Data.Count - 1 do
        begin
          if i > 0 then SQL := SQL + ', ';
          SQL := SQL + Data.Names[i];
        end;
        SQL := SQL + ') VALUES (';
        for i := 0 to Data.Count - 1 do
        begin
          if i > 0 then SQL := SQL + ', ';
          SQL := SQL + ':' + Data.Names[i];
        end;
        SQL := SQL + ')';

        Query.SQL.Text := SQL;

        // Assigner les paramètres
        for i := 0 to Data.Count - 1 do
        begin
          FieldName := Data.Names[i];
          Query.Params.ParamByName(FieldName).AsString :=
            Data.Items[i].AsString;
        end;
      end;

      'UPDATE':
      begin
        // Construire UPDATE depuis JSON
        SQL := Format('UPDATE %s SET ', [TableName]);
        for i := 0 to Data.Count - 1 do
        begin
          FieldName := Data.Names[i];
          if FieldName <> 'id' then
          begin
            if i > 0 then SQL := SQL + ', ';
            SQL := SQL + FieldName + ' = :' + FieldName;
          end;
        end;
        SQL := SQL + ' WHERE id = :id';

        Query.SQL.Text := SQL;

        // Assigner les paramètres
        for i := 0 to Data.Count - 1 do
        begin
          FieldName := Data.Names[i];
          Query.Params.ParamByName(FieldName).AsString :=
            Data.Items[i].AsString;
        end;
      end;

      'DELETE':
      begin
        Query.SQL.Text := Format('DELETE FROM %s WHERE id = :id', [TableName]);
        Query.Params.ParamByName('id').AsInteger := RecordID;
      end;
    end;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TChangeLogSync.SyncChanges(Direction: String): Integer;
var
  SourceConn, TargetConn: TSQLConnection;
  Query: TSQLQuery;
  ChangeIDs: array of Integer;
  ChangeCount: Integer;
  TableName, Operation: String;
  RecordID, ChangeID: Integer;
  JSONData: TJSONObject;
  JSONParser: TJSONParser;
begin
  Result := 0;

  // Déterminer la direction
  if Direction = 'up' then
  begin
    SourceConn := FLocalConnection;
    TargetConn := FRemoteConnection;
    WriteLn('📤 Synchronisation montante des changements...');
  end
  else
  begin
    SourceConn := FRemoteConnection;
    TargetConn := FLocalConnection;
    WriteLn('📥 Synchronisation descendante des changements...');
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := SourceConn;

    // Récupérer les changements non synchronisés
    Query.SQL.Text :=
      'SELECT id, table_name, record_id, operation, data ' +
      'FROM change_log ' +
      'WHERE synced = FALSE ' +
      'ORDER BY created_at';

    Query.Open;
    ChangeCount := 0;
    SetLength(ChangeIDs, 0);

    TargetConn.Transaction.StartTransaction;
    try
      while not Query.EOF do
      begin
        ChangeID := Query.FieldByName('id').AsInteger;
        TableName := Query.FieldByName('table_name').AsString;
        RecordID := Query.FieldByName('record_id').AsInteger;
        Operation := Query.FieldByName('operation').AsString;

        // Parser le JSON
        JSONParser := TJSONParser.Create(Query.FieldByName('data').AsString);
        try
          JSONData := TJSONObject(JSONParser.Parse);
          try
            // Appliquer le changement
            ApplyChange(TargetConn, TableName, Operation, RecordID, JSONData);

            // Marquer pour synchronisation
            SetLength(ChangeIDs, Length(ChangeIDs) + 1);
            ChangeIDs[High(ChangeIDs)] := ChangeID;

            Inc(ChangeCount);
          finally
            JSONData.Free;
          end;
        finally
          JSONParser.Free;
        end;

        Query.Next;
      end;

      TargetConn.Transaction.Commit;

      // Marquer comme synchronisé
      if Length(ChangeIDs) > 0 then
        MarkAsSynced(ChangeIDs);

      WriteLn(Format('  ✅ %d changement(s) synchronisé(s)', [ChangeCount]));
      Result := ChangeCount;
    except
      on E: Exception do
      begin
        TargetConn.Transaction.Rollback;
        WriteLn('  ❌ Erreur: ', E.Message);
        raise;
      end;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TChangeLogSync.MarkAsSynced(ChangeIDs: array of Integer);
var
  Query: TSQLQuery;
  i: Integer;
  IDList: String;
begin
  if Length(ChangeIDs) = 0 then
    Exit;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FLocalConnection;

    // Construire la liste d'IDs
    IDList := '';
    for i := 0 to High(ChangeIDs) do
    begin
      if i > 0 then IDList := IDList + ',';
      IDList := IDList + IntToStr(ChangeIDs[i]);
    end;

    Query.SQL.Text := 'UPDATE change_log SET synced = TRUE WHERE id IN (' + IDList + ')';
    Query.ExecSQL;
    FLocalConnection.Transaction.Commit;
  finally
    Query.Free;
  end;
end;

end.
```

## Synchronisation en mode déconnecté (Offline-First)

Pour les applications mobiles ou les environnements avec connexion intermittente.

### Architecture Offline-First

```
1. Modification locale → Enregistrée dans SQLite local
2. En arrière-plan → Tentative de synchronisation
3. Si hors ligne → Mise en file d'attente
4. Connexion rétablie → Synchronisation automatique
```

### Gestionnaire de file d'attente

```pascal
unit OfflineQueueManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, fgl;

type
  TQueuedOperation = record
    ID: Integer;
    TableName: String;
    Operation: String;
    RecordID: Integer;
    Data: String;
    Attempts: Integer;
    LastAttempt: TDateTime;
  end;

  TOperationQueue = specialize TFPGList<TQueuedOperation>;

  TOfflineQueueManager = class
  private
    FLocalConnection: TSQLConnection;
    FRemoteConnection: TSQLConnection;
    FQueue: TOperationQueue;
    FMaxRetries: Integer;

    procedure LoadQueue;
    function IsOnline: Boolean;
  public
    constructor Create(ALocalConnection, ARemoteConnection: TSQLConnection);
    destructor Destroy; override;

    procedure QueueOperation(const TableName, Operation: String;
      RecordID: Integer; const Data: String);
    function ProcessQueue: Integer;
    procedure ClearProcessedOperations;

    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
  end;

implementation

constructor TOfflineQueueManager.Create(ALocalConnection, ARemoteConnection: TSQLConnection);
begin
  FLocalConnection := ALocalConnection;
  FRemoteConnection := ARemoteConnection;
  FQueue := TOperationQueue.Create;
  FMaxRetries := 3;

  LoadQueue;
end;

destructor TOfflineQueueManager.Destroy;
begin
  FQueue.Free;
  inherited;
end;

procedure TOfflineQueueManager.LoadQueue;
var
  Query: TSQLQuery;
  Op: TQueuedOperation;
begin
  FQueue.Clear;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FLocalConnection;

    // Créer la table de file d'attente si elle n'existe pas
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS sync_queue (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      '  table_name VARCHAR(100), ' +
      '  operation VARCHAR(10), ' +
      '  record_id INTEGER, ' +
      '  data TEXT, ' +
      '  attempts INTEGER DEFAULT 0, ' +
      '  last_attempt TIMESTAMP, ' +
      '  processed BOOLEAN DEFAULT FALSE' +
      ')';
    Query.ExecSQL;

    // Charger les opérations en attente
    Query.SQL.Text :=
      'SELECT * FROM sync_queue WHERE processed = FALSE ORDER BY id';
    Query.Open;

    while not Query.EOF do
    begin
      Op.ID := Query.FieldByName('id').AsInteger;
      Op.TableName := Query.FieldByName('table_name').AsString;
      Op.Operation := Query.FieldByName('operation').AsString;
      Op.RecordID := Query.FieldByName('record_id').AsInteger;
      Op.Data := Query.FieldByName('data').AsString;
      Op.Attempts := Query.FieldByName('attempts').AsInteger;

      if not Query.FieldByName('last_attempt').IsNull then
        Op.LastAttempt := Query.FieldByName('last_attempt').AsDateTime
      else
        Op.LastAttempt := 0;

      FQueue.Add(Op);
      Query.Next;
    end;

    Query.Close;

    WriteLn(Format('%d opération(s) en file d''attente', [FQueue.Count]));
  finally
    Query.Free;
  end;
end;

function TOfflineQueueManager.IsOnline: Boolean;
begin
  try
    FRemoteConnection.Open;
    Result := FRemoteConnection.Connected;
  except
    Result := False;
  end;
end;

procedure TOfflineQueueManager.QueueOperation(const TableName, Operation: String;
  RecordID: Integer; const Data: String);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FLocalConnection;
    Query.SQL.Text :=
      'INSERT INTO sync_queue (table_name, operation, record_id, data) ' +
      'VALUES (:table_name, :operation, :record_id, :data)';

    Query.Params.ParamByName('table_name').AsString := TableName;
    Query.Params.ParamByName('operation').AsString := Operation;
    Query.Params.ParamByName('record_id').AsInteger := RecordID;
    Query.Params.ParamByName('data').AsString := Data;

    Query.ExecSQL;
    FLocalConnection.Transaction.Commit;

    WriteLn('📝 Opération mise en file d''attente: ', Operation, ' sur ', TableName);

    // Recharger la file
    LoadQueue;
  finally
    Query.Free;
  end;
end;

function TOfflineQueueManager.ProcessQueue: Integer;
var
  i: Integer;
  Op: TQueuedOperation;
  Query: TSQLQuery;
  Success: Boolean;
  ProcessedCount: Integer;
begin
  Result := 0;

  if not IsOnline then
  begin
    WriteLn('❌ Hors ligne - synchronisation impossible');
    Exit;
  end;

  WriteLn('🔄 Traitement de la file d''attente...');
  ProcessedCount := 0;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FLocalConnection;

    for i := 0 to FQueue.Count - 1 do
    begin
      Op := FQueue[i];

      // Vérifier le nombre de tentatives
      if Op.Attempts >= FMaxRetries then
      begin
        WriteLn(Format('  ⏭️  Opération %d ignorée (trop de tentatives)', [Op.ID]));
        Continue;
      end;

      Success := False;
      try
        // Appliquer l'opération sur le serveur distant
        // (logique similaire à ApplyChange)

        Success := True;
        Inc(ProcessedCount);

        // Marquer comme traitée
        Query.SQL.Text :=
          'UPDATE sync_queue SET processed = TRUE WHERE id = :id';
        Query.Params.ParamByName('id').AsInteger := Op.ID;
        Query.ExecSQL;

        WriteLn(Format('  ✅ Opération %d traitée avec succès', [Op.ID]));
      except
        on E: Exception do
        begin
          // Incrémenter le compteur de tentatives
          Query.SQL.Text :=
            'UPDATE sync_queue SET attempts = attempts + 1, last_attempt = :now ' +
            'WHERE id = :id';
          Query.Params.ParamByName('id').AsInteger := Op.ID;
          Query.Params.ParamByName('now').AsDateTime := Now;
          Query.ExecSQL;

          WriteLn(Format('  ❌ Échec opération %d (tentative %d/%d): %s',
            [Op.ID, Op.Attempts + 1, FMaxRetries, E.Message]));
        end;
      end;

      FLocalConnection.Transaction.Commit;
    end;

    WriteLn(Format('  📊 %d/%d opération(s) traitée(s)',
      [ProcessedCount, FQueue.Count]));
    Result := ProcessedCount;
  finally
    Query.Free;
  end;

  // Recharger la file
  LoadQueue;
end;

procedure TOfflineQueueManager.ClearProcessedOperations;
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FLocalConnection;
    Query.SQL.Text := 'DELETE FROM sync_queue WHERE processed = TRUE';
    Query.ExecSQL;
    FLocalConnection.Transaction.Commit;

    WriteLn('🗑️  Opérations traitées supprimées');
    LoadQueue;
  finally
    Query.Free;
  end;
end;

end.
```

### Service de synchronisation en arrière-plan

```pascal
program SyncService;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, DateUtils, SQLite3Conn, PQConnection,
  OfflineQueueManager, SyncManager;

const
  SYNC_INTERVAL = 60;  // Synchroniser toutes les 60 secondes

var
  LocalConn: TSQLite3Connection;
  RemoteConn: TPQConnection;
  QueueManager: TOfflineQueueManager;
  LastSync: TDateTime;
  Running: Boolean;

procedure PerformSync;
begin
  WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] Tentative de synchronisation...');

  try
    if QueueManager.ProcessQueue > 0 then
    begin
      // Nettoyer les anciennes opérations après succès
      QueueManager.ClearProcessedOperations;
    end;

    LastSync := Now;
  except
    on E: Exception do
      WriteLn('Erreur synchronisation: ', E.Message);
  end;
end;

procedure HandleSignal(Signal: Integer); cdecl;
begin
  WriteLn('Signal reçu: ', Signal, ' - Arrêt du service');
  Running := False;
end;

begin
  WriteLn('=== Service de Synchronisation ===');
  WriteLn('Démarrage...');

  // Configuration des connexions
  LocalConn := TSQLite3Connection.Create(nil);
  LocalConn.DatabaseName := 'local.db';

  RemoteConn := TPQConnection.Create(nil);
  RemoteConn.DatabaseName := 'cloud_db';
  RemoteConn.HostName := 'cloud.example.com';
  RemoteConn.UserName := 'postgres';
  RemoteConn.Password := 'password';

  try
    LocalConn.Open;

    QueueManager := TOfflineQueueManager.Create(LocalConn, RemoteConn);
    try
      QueueManager.MaxRetries := 5;

      {$IFDEF UNIX}
      // Gérer les signaux Unix pour arrêt propre
      Signal(SIGTERM, @HandleSignal);
      Signal(SIGINT, @HandleSignal);
      {$ENDIF}

      Running := True;
      LastSync := Now;

      WriteLn('Service démarré. Synchronisation toutes les ', SYNC_INTERVAL, ' secondes');
      WriteLn('Appuyez sur Ctrl+C pour arrêter');
      WriteLn;

      while Running do
      begin
        if SecondsBetween(Now, LastSync) >= SYNC_INTERVAL then
          PerformSync;

        Sleep(1000);  // Dormir 1 seconde
      end;

      WriteLn('Arrêt propre du service...');
    finally
      QueueManager.Free;
    end;
  finally
    LocalConn.Close;
    LocalConn.Free;
    RemoteConn.Free;
  end;

  WriteLn('Service arrêté');
end.
```

## Configuration multi-OS de la réplication

### Détection automatique de l'environnement

```pascal
unit SyncConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TSyncConfig = class
  private
    FConfigFile: String;
    FIniFile: TIniFile;

    function GetConfigPath: String;
    function DetectOS: String;
  public
    constructor Create;
    destructor Destroy; override;

    function GetLocalDBPath: String;
    function GetRemoteHost: String;
    function GetRemotePort: Integer;
    function GetSyncInterval: Integer;

    procedure SaveConfig;
    procedure LoadConfig;
  end;

implementation

constructor TSyncConfig.Create;
begin
  FConfigFile := GetConfigPath;
  FIniFile := TIniFile.Create(FConfigFile);
  LoadConfig;
end;

destructor TSyncConfig.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TSyncConfig.GetConfigPath: String;
begin
  {$IFDEF WINDOWS}
  Result := ExtractFilePath(ParamStr(0)) + 'sync_config.ini';
  {$ELSE}
  // Sur Linux/Ubuntu, utiliser le répertoire de configuration utilisateur
  Result := GetEnvironmentVariable('HOME') + '/.config/syncapp/sync_config.ini';

  // Créer le répertoire si nécessaire
  if not DirectoryExists(ExtractFilePath(Result)) then
    ForceDirectories(ExtractFilePath(Result));
  {$ENDIF}
end;

function TSyncConfig.DetectOS: String;
begin
  {$IFDEF WINDOWS}
  Result := 'Windows';
  {$ELSE}
  {$IFDEF LINUX}
  Result := 'Linux';
  {$ELSE}
  {$IFDEF DARWIN}
  Result := 'macOS';
  {$ELSE}
  Result := 'Unknown';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

function TSyncConfig.GetLocalDBPath: String;
begin
  {$IFDEF WINDOWS}
  Result := FIniFile.ReadString('Local', 'DatabasePath',
    ExtractFilePath(ParamStr(0)) + 'data\local.db');
  {$ELSE}
  Result := FIniFile.ReadString('Local', 'DatabasePath',
    GetEnvironmentVariable('HOME') + '/.local/share/syncapp/local.db');
  {$ENDIF}
end;

function TSyncConfig.GetRemoteHost: String;
begin
  Result := FIniFile.ReadString('Remote', 'Host', 'localhost');
end;

function TSyncConfig.GetRemotePort: Integer;
begin
  Result := FIniFile.ReadInteger('Remote', 'Port', 5432);
end;

function TSyncConfig.GetSyncInterval: Integer;
begin
  Result := FIniFile.ReadInteger('Sync', 'IntervalSeconds', 60);
end;

procedure TSyncConfig.SaveConfig;
begin
  FIniFile.WriteString('System', 'OS', DetectOS);
  FIniFile.WriteString('System', 'LastUpdate',
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  FIniFile.UpdateFile;
end;

procedure TSyncConfig.LoadConfig;
begin
  // Charger ou créer la configuration par défaut
  if not FileExists(FConfigFile) then
  begin
    // Configuration par défaut
    FIniFile.WriteString('Local', 'DatabasePath', GetLocalDBPath);
    FIniFile.WriteString('Remote', 'Host', 'cloud.example.com');
    FIniFile.WriteInteger('Remote', 'Port', 5432);
    FIniFile.WriteString('Remote', 'Database', 'cloud_db');
    FIniFile.WriteString('Remote', 'Username', 'postgres');
    FIniFile.WriteString('Remote', 'Password', '');  // À configurer
    FIniFile.WriteInteger('Sync', 'IntervalSeconds', 60);
    FIniFile.WriteBool('Sync', 'AutoStart', True);
    FIniFile.WriteString('System', 'OS', DetectOS);
    SaveConfig;

    WriteLn('Configuration créée: ', FConfigFile);
    WriteLn('⚠️  N''oubliez pas de configurer le mot de passe distant!');
  end;
end;

end.
```

### Exemple de fichier de configuration (sync_config.ini)

```ini
[Local]
DatabasePath=local.db

[Remote]
Host=cloud.example.com
Port=5432
Database=cloud_db
Username=postgres
Password=mot_de_passe_securise

[Sync]
IntervalSeconds=60
AutoStart=True
MaxRetries=5
TimeoutSeconds=30

[Tables]
; Format: TableName=Direction (up/down/bidirectional)
clients=bidirectional
produits=down
commandes=bidirectional
logs=up

[System]
OS=Linux
LastUpdate=2024-01-15 10:30:00
```

## Installation comme service système

### Service Windows

```pascal
program SyncServiceWin;

{$mode objfpc}{$H+}

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, JwaSvc, ServiceManager;

const
  SERVICE_NAME = 'SyncService';
  SERVICE_DISPLAY = 'Service de Synchronisation BDD';

type
  TSyncService = class(TService)
  private
    FRunning: Boolean;
  protected
    procedure ServiceStart; override;
    procedure ServiceStop; override;
    procedure ServiceExecute; override;
  end;

procedure TSyncService.ServiceStart;
begin
  FRunning := True;
  LogMessage('Service démarré', EVENTLOG_INFORMATION_TYPE);
end;

procedure TSyncService.ServiceStop;
begin
  FRunning := False;
  LogMessage('Service arrêté', EVENTLOG_INFORMATION_TYPE);
end;

procedure TSyncService.ServiceExecute;
begin
  while FRunning do
  begin
    // Logique de synchronisation
    try
      // Effectuer la synchronisation
      Sleep(60000);  // Attendre 60 secondes
    except
      on E: Exception do
        LogMessage('Erreur: ' + E.Message, EVENTLOG_ERROR_TYPE);
    end;
  end;
end;

var
  Service: TSyncService;

begin
  Service := TSyncService.Create(nil);
  try
    Service.Name := SERVICE_NAME;
    Service.DisplayName := SERVICE_DISPLAY;
    Service.Run;
  finally
    Service.Free;
  end;
end.
```

**Installation du service Windows :**

```batch
REM Compiler le programme
fpc SyncServiceWin.pas

REM Installer le service (en administrateur)
sc create SyncService binPath= "C:\Path\To\SyncServiceWin.exe"
sc description SyncService "Service de synchronisation de base de données"

REM Démarrer le service
sc start SyncService

REM Arrêter le service
sc stop SyncService

REM Désinstaller le service
sc delete SyncService
```

### Service systemd (Ubuntu)

**Fichier de service : /etc/systemd/system/syncservice.service**

```ini
[Unit]
Description=Service de Synchronisation BDD
After=network.target postgresql.service

[Service]
Type=simple
User=syncuser
WorkingDirectory=/opt/syncapp
ExecStart=/opt/syncapp/syncservice
Restart=on-failure
RestartSec=30

# Logs
StandardOutput=journal
StandardError=journal
SyslogIdentifier=syncservice

# Sécurité
PrivateTmp=true
NoNewPrivileges=true

[Install]
WantedBy=multi-user.target
```

**Installation sur Ubuntu :**

```bash
# Copier l'exécutable
sudo mkdir -p /opt/syncapp
sudo cp syncservice /opt/syncapp/
sudo chmod +x /opt/syncapp/syncservice

# Copier le fichier de service
sudo cp syncservice.service /etc/systemd/system/

# Créer l'utilisateur dédié
sudo useradd -r -s /bin/false syncuser
sudo chown -R syncuser:syncuser /opt/syncapp

# Recharger systemd
sudo systemctl daemon-reload

# Activer le service au démarrage
sudo systemctl enable syncservice

# Démarrer le service
sudo systemctl start syncservice

# Vérifier le statut
sudo systemctl status syncservice

# Voir les logs
sudo journalctl -u syncservice -f
```

## Monitoring et alertes

### Système d'alertes

```pascal
unit SyncAlerts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  TAlertLevel = (alInfo, alWarning, alError, alCritical);

  TSyncAlerts = class
  private
    FEmailEnabled: Boolean;
    FEmailRecipient: String;
    FLogFile: String;

    procedure SendEmail(const Subject, Body: String);
    procedure LogAlert(Level: TAlertLevel; const Message: String);
  public
    constructor Create(const ALogFile: String);

    procedure Alert(Level: TAlertLevel; const Message: String);
    procedure AlertSyncFailure(Attempts: Integer);
    procedure AlertHighLag(LagSeconds: Integer);
    procedure AlertDiskSpace(PercentFree: Integer);

    property EmailEnabled: Boolean read FEmailEnabled write FEmailEnabled;
    property EmailRecipient: String read FEmailRecipient write FEmailRecipient;
  end;

implementation

constructor TSyncAlerts.Create(const ALogFile: String);
begin
  FLogFile := ALogFile;
  FEmailEnabled := False;
end;

procedure TSyncAlerts.LogAlert(Level: TAlertLevel; const Message: String);
var
  F: TextFile;
  LevelStr: String;
begin
  case Level of
    alInfo: LevelStr := 'INFO';
    alWarning: LevelStr := 'WARN';
    alError: LevelStr := 'ERROR';
    alCritical: LevelStr := 'CRITICAL';
  end;

  AssignFile(F, FLogFile);
  try
    if FileExists(FLogFile) then
      Append(F)
    else
      Rewrite(F);

    WriteLn(F, Format('[%s] [%s] %s', [
      FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
      LevelStr,
      Message
    ]));

    CloseFile(F);
  except
    // Ignorer les erreurs de log
  end;
end;

procedure TSyncAlerts.SendEmail(const Subject, Body: String);
var
  Process: TProcess;
begin
  if not FEmailEnabled then
    Exit;

  Process := TProcess.Create(nil);
  try
    {$IFDEF UNIX}
    // Utiliser mail sur Linux
    Process.Executable := 'mail';
    Process.Parameters.Add('-s');
    Process.Parameters.Add(Subject);
    Process.Parameters.Add(FEmailRecipient);
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;
    Process.Input.Write(Body[1], Length(Body));
    {$ENDIF}

    {$IFDEF WINDOWS}
    // Sur Windows, utiliser un script PowerShell ou un outil externe
    // Exemple avec PowerShell (nécessite configuration SMTP)
    Process.Executable := 'powershell.exe';
    Process.Parameters.Add('-Command');
    Process.Parameters.Add(Format(
      'Send-MailMessage -To "%s" -Subject "%s" -Body "%s" ' +
      '-SmtpServer "smtp.example.com" -From "sync@example.com"',
      [FEmailRecipient, Subject, Body]
    ));
    Process.Execute;
    {$ENDIF}
  finally
    Process.Free;
  end;
end;

procedure TSyncAlerts.Alert(Level: TAlertLevel; const Message: String);
begin
  LogAlert(Level, Message);

  // Envoyer un email pour les erreurs critiques
  if (Level in [alError, alCritical]) and FEmailEnabled then
  begin
    SendEmail(
      Format('Alerte Sync: %s', [Message]),
      Format('Une alerte de synchronisation a été déclenchée:%s%s%s%s' +
             'Timestamp: %s',
        [sLineBreak, sLineBreak, Message, sLineBreak,
         FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)])
    );
  end;
end;

procedure TSyncAlerts.AlertSyncFailure(Attempts: Integer);
begin
  Alert(alError, Format('Échec de synchronisation après %d tentatives', [Attempts]));
end;

procedure TSyncAlerts.AlertHighLag(LagSeconds: Integer);
begin
  if LagSeconds > 300 then  // Plus de 5 minutes
    Alert(alCritical, Format('Lag de réplication élevé: %d secondes', [LagSeconds]))
  else if LagSeconds > 60 then  // Plus de 1 minute
    Alert(alWarning, Format('Lag de réplication: %d secondes', [LagSeconds]));
end;

procedure TSyncAlerts.AlertDiskSpace(PercentFree: Integer);
begin
  if PercentFree < 10 then
    Alert(alCritical, Format('Espace disque critique: %d%% libre', [PercentFree]))
  else if PercentFree < 20 then
    Alert(alWarning, Format('Espace disque faible: %d%% libre', [PercentFree]));
end;

end.
```

### Dashboard de monitoring

```pascal
unit SyncDashboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls,
  SQLDB, ReplicationMonitor;

type
  TDashboardForm = class(TForm)
    StatusLabel: TLabel;
    LagLabel: TLabel;
    QueueLabel: TLabel;
    LastSyncLabel: TLabel;
    RefreshTimer: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
  private
    FConnection: TSQLConnection;
    FMonitor: TReplicationMonitor;

    procedure UpdateStatus;
    function GetQueueSize: Integer;
  end;

implementation

procedure TDashboardForm.FormCreate(Sender: TObject);
begin
  Caption := 'Dashboard de Synchronisation';
  Width := 600;
  Height := 400;

  // Créer les labels
  StatusLabel := TLabel.Create(Self);
  StatusLabel.Parent := Self;
  StatusLabel.Left := 20;
  StatusLabel.Top := 20;
  StatusLabel.Caption := 'Statut: Initialisation...';
  StatusLabel.Font.Size := 12;

  LagLabel := TLabel.Create(Self);
  LagLabel.Parent := Self;
  LagLabel.Left := 20;
  LagLabel.Top := 60;
  LagLabel.Caption := 'Lag: --';

  QueueLabel := TLabel.Create(Self);
  QueueLabel.Parent := Self;
  QueueLabel.Left := 20;
  QueueLabel.Top := 100;
  QueueLabel.Caption := 'File d''attente: --';

  LastSyncLabel := TLabel.Create(Self);
  LastSyncLabel.Parent := Self;
  LastSyncLabel.Left := 20;
  LastSyncLabel.Top := 140;
  LastSyncLabel.Caption := 'Dernière sync: --';

  // Timer pour rafraîchir
  RefreshTimer := TTimer.Create(Self);
  RefreshTimer.Interval := 5000;  // 5 secondes
  RefreshTimer.OnTimer := @RefreshTimerTimer;
  RefreshTimer.Enabled := True;

  // Initialiser le monitoring
  FMonitor := TReplicationMonitor.Create(FConnection);

  UpdateStatus;
end;

procedure TDashboardForm.UpdateStatus;
var
  Lag, QueueSize: Integer;
begin
  try
    Lag := FMonitor.GetReplicationLag;
    QueueSize := GetQueueSize;

    // Mettre à jour l'interface
    if FMonitor.IsReplicaHealthy then
    begin
      StatusLabel.Caption := 'Statut: ✅ En ligne';
      StatusLabel.Font.Color := clGreen;
    end
    else
    begin
      StatusLabel.Caption := 'Statut: ❌ Problème détecté';
      StatusLabel.Font.Color := clRed;
    end;

    if Lag >= 0 then
      LagLabel.Caption := Format('Lag: %d secondes', [Lag])
    else
      LagLabel.Caption := 'Lag: N/A';

    QueueLabel.Caption := Format('File d''attente: %d opération(s)', [QueueSize]);

    LastSyncLabel.Caption := 'Dernière sync: ' +
      FormatDateTime('hh:nn:ss', Now);
  except
    on E: Exception do
      StatusLabel.Caption := 'Statut: ❌ Erreur - ' + E.Message;
  end;
end;

function TDashboardForm.GetQueueSize: Integer;
var
  Query: TSQLQuery;
begin
  Result := 0;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := 'SELECT COUNT(*) FROM sync_queue WHERE processed = FALSE';
    Query.Open;
    Result := Query.Fields[0].AsInteger;
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TDashboardForm.RefreshTimerTimer(Sender: TObject);
begin
  UpdateStatus;
end;

end.
```

## Bonnes pratiques de réplication

### 1. Surveillance continue

✅ **À faire :**
- Monitorer le lag de réplication en permanence
- Configurer des alertes automatiques
- Logger tous les événements importants
- Vérifier régulièrement l'espace disque

### 2. Sécurité

✅ **À faire :**
- Utiliser SSL/TLS pour les connexions
- Chiffrer les mots de passe dans la configuration
- Limiter les accès réseau (firewall)
- Utiliser des utilisateurs dédiés avec droits minimaux

```pascal
// Exemple de connexion sécurisée
Connection.Params.Add('sslmode=require');
Connection.Params.Add('sslcert=/path/to/client-cert.pem');
Connection.Params.Add('sslkey=/path/to/client-key.pem');
Connection.Params.Add('sslrootcert=/path/to/ca-cert.pem');
```

### 3. Performance

✅ **À faire :**
- Utiliser la compression pour les données volumineuses
- Optimiser les index sur les colonnes de synchronisation
- Limiter la taille des transactions
- Utiliser des slots de réplication

### 4. Gestion des erreurs

✅ **À faire :**
- Implémenter des mécanismes de retry
- Logger toutes les erreurs
- Avoir un plan de récupération
- Tester régulièrement les scénarios de panne

### 5. Documentation

✅ **À faire :**
- Documenter l'architecture de réplication
- Maintenir un runbook pour les incidents
- Former l'équipe aux procédures
- Documenter tous les changements

## Conclusion

La réplication et la synchronisation sont des mécanismes puissants pour assurer la disponibilité et la cohérence des données dans vos applications FreePascal/Lazarus, que ce soit sur Windows ou Ubuntu.

### Points clés à retenir

1. **Choisir le bon type de réplication** selon vos besoins (maître-esclave, maître-maître, etc.)
2. **Surveiller en permanence** le lag et la santé de la réplication
3. **Gérer les conflits** avec une stratégie claire
4. **Tester régulièrement** les procédures de basculement
5. **Sécuriser** toutes les connexions de réplication
6. **Documenter** l'architecture et les procédures
7. **Automatiser** la surveillance et les alertes

Avec les outils et techniques présentés dans ce chapitre, vous êtes maintenant capable de mettre en place une infrastructure de réplication robuste et fiable pour vos applications professionnelles.

⏭️ [NoSQL avec MongoDB et Redis](/08-bases-donnees-orm-multiplatefomes/11-nosql-mongodb-redis.md)
