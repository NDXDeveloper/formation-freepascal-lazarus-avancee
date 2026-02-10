🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Connexions Natives Identiques sur Windows et Linux/Ubuntu

## Introduction

L'un des grands atouts de FreePascal/Lazarus est sa capacité à offrir une expérience de développement identique pour les bases de données, que vous soyez sur Windows ou Linux/Ubuntu. Cette section explore comment établir des connexions natives aux principaux systèmes de gestion de bases de données (SGBD) tout en maintenant un code source unique et portable.

## Qu'est-ce qu'une connexion native ?

### Définition

Une connexion native utilise les bibliothèques officielles fournies par l'éditeur de la base de données pour communiquer directement avec le serveur. C'est l'opposé d'une connexion générique (comme ODBC) qui passe par une couche d'abstraction supplémentaire.

### Avantages des connexions natives

1. **Performance optimale** : Communication directe sans intermédiaire
2. **Fonctionnalités complètes** : Accès à toutes les capacités spécifiques du SGBD
3. **Stabilité** : Bibliothèques maintenues par l'éditeur officiel
4. **Support** : Documentation et aide directement de la source
5. **Sécurité** : Utilisation des mécanismes de sécurité natifs

### Comparaison avec les alternatives

```
Connexion Native :  
Application → Bibliothèque native → Base de données
(Plus rapide, plus direct)

Connexion ODBC :  
Application → Driver ODBC → Bibliothèque native → Base de données
(Plus générique, mais plus lent)

Connexion via middleware :  
Application → Serveur intermédiaire → Base de données
(Pour architectures distribuées)
```

## Le défi de la portabilité

### Différences entre systèmes

Chaque système d'exploitation gère différemment les bibliothèques :

#### Windows
- Utilise des fichiers **.dll** (Dynamic Link Library)
- Recherche dans des chemins spécifiques (System32, dossier de l'application, PATH)
- Gestion des versions parfois complexe
- Architecture 32/64 bits à considérer

#### Linux/Ubuntu
- Utilise des fichiers **.so** (Shared Object)
- Système de gestion de paquets pour l'installation
- Liens symboliques pour les versions
- Permissions et droits d'accès à gérer

### La solution FreePascal/Lazarus

FreePascal résout ces différences en :

1. **Abstrayant le chargement des bibliothèques** : Le même code charge la .dll ou .so appropriée
2. **Unifiant les interfaces** : Mêmes composants et propriétés sur tous les OS
3. **Gérant automatiquement les chemins** : Recherche intelligente des bibliothèques
4. **Normalisant les types de données** : Conversion automatique selon la plateforme

## Architecture des connexions natives

### Structure générale

```
┌─────────────────────────────────────┐
│     Votre Application Lazarus       │
├─────────────────────────────────────┤
│         Composant SQLdb             │
│    (TPQConnection, TMySQLConn...)   │
├─────────────────────────────────────┤
│      Couche d'abstraction FPC       │
│   (Chargement dynamique unifié)     │
├─────────────────────────────────────┤
│     Bibliothèque Cliente Native     │
│   Windows: .dll  |  Linux: .so      │
├─────────────────────────────────────┤
│      Serveur de Base de Données     │
└─────────────────────────────────────┘
```

### Composants de connexion disponibles

FreePascal/Lazarus fournit des composants spécialisés pour chaque SGBD :

```pascal
// PostgreSQL
TPQConnection      // Version générique  
TPQConnection9_6   // Version spécifique 9.6+

// MySQL/MariaDB
TMySQL40Connection // MySQL 4.0  
TMySQL41Connection // MySQL 4.1  
TMySQL50Connection // MySQL 5.0  
TMySQL51Connection // MySQL 5.1  
TMySQL55Connection // MySQL 5.5  
TMySQL56Connection // MySQL 5.6  
TMySQL57Connection // MySQL 5.7  
TMySQL80Connection // MySQL 8.0

// SQLite
TSQLite3Connection // SQLite version 3

// Firebird
TIBConnection      // Firebird/InterBase

// Microsoft SQL Server
TMSSQLConnection   // Via FreeTDS ou native

// Oracle
TOracleConnection  // Oracle Database
```

## Préparation de l'environnement

### Installation des bibliothèques clientes

#### Sur Windows

Les bibliothèques clientes doivent être obtenues et placées correctement :

1. **Téléchargement** : Depuis le site officiel du SGBD
2. **Architecture** : Choisir 32 ou 64 bits selon votre application
3. **Emplacement** :
   - Dans le dossier de l'application (recommandé)
   - Dans Windows\System32 (64 bits) ou SysWOW64 (32 bits)
   - Dans un dossier du PATH système

#### Sur Linux/Ubuntu

Installation via le gestionnaire de paquets :

```bash
# Mise à jour des dépôts
sudo apt update

# Installation des clients (exemples)
sudo apt install libpq-dev          # PostgreSQL  
sudo apt install libmysqlclient-dev # MySQL  
sudo apt install libsqlite3-dev     # SQLite  
sudo apt install firebird-dev       # Firebird
```

### Vérification de l'installation

#### Créer un programme de test simple

```pascal
program TestConnexion;  
uses
  SysUtils, sqldb;

var
  Conn: TSQLConnection;
begin
  try
    // Tester le chargement de la bibliothèque
    Conn := TPQConnection.Create(nil);
    WriteLn('Bibliothèque PostgreSQL chargée avec succès');
    Conn.Free;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end.
```

## Configuration multi-plateforme

### Utilisation de directives conditionnelles

```pascal
unit DatabaseConfig;

interface

uses
  Classes, SysUtils, sqldb;

function GetLibraryPath: string;  
function CreateConnection(AType: string): TSQLConnection;

implementation

function GetLibraryPath: string;  
begin
  {$IFDEF WINDOWS}
    Result := ExtractFilePath(ParamStr(0)) + 'libs\';
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF LINUX}
      Result := '/usr/lib/';
    {$ENDIF}
    {$IFDEF DARWIN}
      Result := '/usr/local/lib/';
    {$ENDIF}
  {$ENDIF}
end;

function CreateConnection(AType: string): TSQLConnection;  
begin
  if AType = 'postgresql' then
    Result := TPQConnection.Create(nil)
  else if AType = 'mysql' then
    Result := TMySQL57Connection.Create(nil)
  else if AType = 'sqlite' then
    Result := TSQLite3Connection.Create(nil)
  else if AType = 'firebird' then
    Result := TIBConnection.Create(nil)
  else
    raise Exception.Create('Type de base non supporté');
end;
```

### Fichiers de configuration

Utiliser des fichiers de configuration pour gérer les différences :

```ini
; config.ini
[Database]
Type=postgresql  
Host=localhost  
Port=5432  
Database=mabase  
Username=user  
Password=pass

[Windows]
LibraryPath=.\libs\libpq.dll

[Linux]
LibraryPath=/usr/lib/x86_64-linux-gnu/libpq.so.5
```

```pascal
uses
  IniFiles;

procedure ConfigureConnection(AConnection: TSQLConnection);  
var
  Ini: TIniFile;
  OS: string;
begin
  Ini := TIniFile.Create('config.ini');
  try
    // Configuration commune
    AConnection.HostName := Ini.ReadString('Database', 'Host', 'localhost');
    AConnection.DatabaseName := Ini.ReadString('Database', 'Database', '');
    AConnection.UserName := Ini.ReadString('Database', 'Username', '');
    AConnection.Password := Ini.ReadString('Database', 'Password', '');

    // Configuration spécifique à l'OS
    {$IFDEF WINDOWS}
    OS := 'Windows';
    {$ELSE}
    OS := 'Linux';
    {$ENDIF}

    // Si nécessaire, spécifier le chemin de la bibliothèque
    if Ini.ValueExists(OS, 'LibraryPath') then
      AConnection.Params.Add('LibraryLocation=' +
        Ini.ReadString(OS, 'LibraryPath', ''));
  finally
    Ini.Free;
  end;
end;
```

## Gestion des erreurs de connexion

### Erreurs communes et solutions

#### Bibliothèque introuvable

```pascal
procedure TryConnect(AConnection: TSQLConnection);  
begin
  try
    AConnection.Connected := True;
  except
    on E: Exception do
    begin
      if Pos('library', LowerCase(E.Message)) > 0 then
      begin
        {$IFDEF WINDOWS}
        ShowMessage('Bibliothèque cliente non trouvée.' + LineEnding +
                   'Vérifiez que le fichier .dll est présent dans :' + LineEnding +
                   '- Le dossier de l''application' + LineEnding +
                   '- Le PATH système');
        {$ENDIF}
        {$IFDEF UNIX}
        ShowMessage('Bibliothèque cliente non trouvée.' + LineEnding +
                   'Installez le paquet client avec :' + LineEnding +
                   'sudo apt install [nom-du-paquet-client]');
        {$ENDIF}
      end
      else
        ShowMessage('Erreur de connexion : ' + E.Message);
    end;
  end;
end;
```

#### Versions incompatibles

```pascal
function GetCompatibleConnection(AVersion: string): TSQLConnection;  
begin
  // Pour MySQL, choisir la bonne version
  if Pos('5.7', AVersion) > 0 then
    Result := TMySQL57Connection.Create(nil)
  else if Pos('5.6', AVersion) > 0 then
    Result := TMySQL56Connection.Create(nil)
  else if Pos('8.0', AVersion) > 0 then
    Result := TMySQL80Connection.Create(nil)
  else
    raise Exception.Create('Version MySQL non supportée : ' + AVersion);
end;
```

## Propriétés communes à toutes les connexions

### Propriétés essentielles

Toutes les connexions natives partagent ces propriétés de base :

```pascal
type
  TDatabaseConfig = record
    HostName: string;      // Serveur (localhost, IP, nom DNS)
    Port: Integer;         // Port du serveur
    DatabaseName: string;  // Nom de la base
    UserName: string;      // Utilisateur
    Password: string;      // Mot de passe
    CharSet: string;       // Jeu de caractères (UTF8, etc.)
    Role: string;          // Rôle (Firebird)
    Params: TStrings;      // Paramètres supplémentaires
  end;
```

### Configuration unifiée

```pascal
procedure ConfigureAnyConnection(AConn: TSQLConnection; const AConfig: TDatabaseConfig);  
begin
  AConn.HostName := AConfig.HostName;
  AConn.DatabaseName := AConfig.DatabaseName;
  AConn.UserName := AConfig.UserName;
  AConn.Password := AConfig.Password;

  // Port (si différent du défaut)
  if AConfig.Port > 0 then
  begin
    if AConn is TPQConnection then
      TPQConnection(AConn).Port := AConfig.Port
    else if AConn is TMySQLConnection then
      TMySQLConnection(AConn).Port := AConfig.Port;
  end;

  // Charset
  if AConfig.CharSet <> '' then
    AConn.CharSet := AConfig.CharSet;

  // Paramètres additionnels
  if Assigned(AConfig.Params) then
    AConn.Params.Assign(AConfig.Params);
end;
```

## Optimisation des connexions

### Pool de connexions

Pour les applications multi-utilisateurs, réutiliser les connexions :

```pascal
type
  TConnectionPool = class
  private
    FConnections: TList;
    FConnectionType: string;
    FConfig: TDatabaseConfig;
    FMaxConnections: Integer;
    FCriticalSection: TCriticalSection;
  public
    constructor Create(AType: string; const AConfig: TDatabaseConfig; AMax: Integer);
    destructor Destroy; override;
    function GetConnection: TSQLConnection;
    procedure ReleaseConnection(AConn: TSQLConnection);
  end;

function TConnectionPool.GetConnection: TSQLConnection;  
var
  i: Integer;
begin
  FCriticalSection.Enter;
  try
    // Chercher une connexion disponible
    for i := 0 to FConnections.Count - 1 do
    begin
      Result := TSQLConnection(FConnections[i]);
      if not Result.Connected then
      begin
        Result.Connected := True;
        Exit;
      end;
    end;

    // Créer une nouvelle connexion si limite non atteinte
    if FConnections.Count < FMaxConnections then
    begin
      Result := CreateConnection(FConnectionType);
      ConfigureAnyConnection(Result, FConfig);
      Result.Connected := True;
      FConnections.Add(Result);
    end
    else
      raise Exception.Create('Pool de connexions saturé');
  finally
    FCriticalSection.Leave;
  end;
end;
```

### Connexions persistantes vs temporaires

```pascal
type
  TConnectionStrategy = (csAlwaysConnected, csConnectOnDemand, csConnectionPool);

  TDatabaseManager = class
  private
    FStrategy: TConnectionStrategy;
    FConnection: TSQLConnection;
    FLastAccess: TDateTime;
    FTimeout: Integer; // Minutes
  public
    function GetConnection: TSQLConnection;
    procedure CheckTimeout;
  end;

function TDatabaseManager.GetConnection: TSQLConnection;  
begin
  case FStrategy of
    csAlwaysConnected:
      begin
        if not FConnection.Connected then
          FConnection.Connected := True;
        Result := FConnection;
      end;

    csConnectOnDemand:
      begin
        FConnection.Connected := True;
        Result := FConnection;
        // Se déconnectera après utilisation
      end;

    csConnectionPool:
      Result := Pool.GetConnection;
  end;

  FLastAccess := Now;
end;
```

## Sécurité des connexions

### Chiffrement des communications

#### Configuration SSL/TLS

```pascal
procedure ConfigureSSL(AConnection: TSQLConnection);  
begin
  {$IFDEF WINDOWS}
    AConnection.Params.Add('sslmode=require');
    AConnection.Params.Add('sslcert=' + GetAppDir + 'certs\client-cert.pem');
    AConnection.Params.Add('sslkey=' + GetAppDir + 'certs\client-key.pem');
    AConnection.Params.Add('sslrootcert=' + GetAppDir + 'certs\ca-cert.pem');
  {$ENDIF}

  {$IFDEF UNIX}
    AConnection.Params.Add('sslmode=require');
    AConnection.Params.Add('sslcert=/etc/ssl/certs/client-cert.pem');
    AConnection.Params.Add('sslkey=/etc/ssl/private/client-key.pem');
    AConnection.Params.Add('sslrootcert=/etc/ssl/certs/ca-cert.pem');
  {$ENDIF}
end;
```

### Stockage sécurisé des identifiants

```pascal
uses
  {$IFDEF WINDOWS}
  Windows, WinCrypt,
  {$ENDIF}
  {$IFDEF UNIX}
  LibSecret, // ou autre keyring
  {$ENDIF}
  Base64;

function SecurePassword(const APassword: string; AEncrypt: Boolean): string;  
begin
  {$IFDEF WINDOWS}
  if AEncrypt then
    Result := EncryptWindowsDPAPI(APassword)
  else
    Result := DecryptWindowsDPAPI(APassword);
  {$ENDIF}

  {$IFDEF UNIX}
  if AEncrypt then
    Result := StoreInKeyring('MyApp', 'DBPassword', APassword)
  else
    Result := GetFromKeyring('MyApp', 'DBPassword');
  {$ENDIF}
end;
```

## Tests et diagnostics

### Vérification de la disponibilité

```pascal
function TestConnection(AType: string): Boolean;  
var
  Conn: TSQLConnection;
  StartTime: TDateTime;
  Duration: Double;
begin
  Result := False;
  Conn := CreateConnection(AType);
  try
    WriteLn('Test de connexion ' + AType + '...');

    // Test de chargement de la bibliothèque
    try
      Conn.HostName := 'localhost';
      WriteLn('✓ Bibliothèque chargée');
    except
      on E: Exception do
      begin
        WriteLn('✗ Échec du chargement : ' + E.Message);
        Exit;
      end;
    end;

    // Test de connexion
    StartTime := Now;
    try
      Conn.Connected := True;
      Duration := (Now - StartTime) * 24 * 3600;
      WriteLn(Format('✓ Connexion établie en %.3f secondes', [Duration]));
      Result := True;
    except
      on E: Exception do
        WriteLn('✗ Échec de connexion : ' + E.Message);
    end;

  finally
    Conn.Free;
  end;
end;
```

### Logging des opérations

```pascal
type
  TConnectionLogger = class
  private
    FLogFile: TextFile;
  public
    procedure LogConnection(AConn: TSQLConnection; ASuccess: Boolean);
    procedure LogQuery(ASQL: string; ADuration: Double);
    procedure LogError(AError: string);
  end;

procedure TConnectionLogger.LogConnection(AConn: TSQLConnection; ASuccess: Boolean);  
var
  LogEntry: string;
begin
  LogEntry := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' | ';
  LogEntry := LogEntry + AConn.ClassName + ' | ';
  LogEntry := LogEntry + AConn.HostName + ':' + AConn.DatabaseName + ' | ';
  if ASuccess then
    LogEntry := LogEntry + 'SUCCESS'
  else
    LogEntry := LogEntry + 'FAILED';

  WriteLn(FLogFile, LogEntry);
  Flush(FLogFile);
end;
```

## Préparation pour les sections suivantes

Cette introduction aux connexions natives pose les bases pour comprendre comment FreePascal/Lazarus unifie l'accès aux différentes bases de données. Les sections suivantes détailleront chaque type de connexion spécifique :

- **PostgreSQL** : Le SGBD open source professionnel
- **MySQL/MariaDB** : Les bases populaires du web
- **SQLite** : La base embarquée légère
- **Firebird** : La solution complète et portable

Chaque section approfondira les spécificités, optimisations et bonnes pratiques propres à chaque SGBD, tout en conservant cette approche unifiée multi-plateforme qui fait la force de FreePascal/Lazarus.

⏭️ [PostgreSQL](/08-bases-donnees-orm-multiplatefomes/02.1-postgresql.md)
