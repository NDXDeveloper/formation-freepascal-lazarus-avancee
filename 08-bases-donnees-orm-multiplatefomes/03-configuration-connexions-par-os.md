🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.3 Configuration des connexions par OS

## Introduction

L'un des grands avantages de FreePascal et Lazarus est leur capacité à créer des applications véritablement multi-plateformes. Cependant, lorsqu'il s'agit de connexions aux bases de données, certaines différences entre Windows et Linux/Ubuntu nécessitent une attention particulière.

Ce chapitre vous guidera à travers les spécificités de configuration des connexions bases de données selon le système d'exploitation, en vous donnant les outils pour créer des applications qui fonctionnent de manière transparente sur les deux plateformes.

## Pourquoi des configurations différentes ?

### Différences fondamentales entre Windows et Linux

Les systèmes d'exploitation Windows et Linux gèrent différemment plusieurs aspects cruciaux pour les connexions bases de données :

| Aspect | Windows | Linux/Ubuntu |
|--------|---------|--------------|
| **Bibliothèques dynamiques** | `.dll` (Dynamic Link Library) | `.so` (Shared Object) |
| **Chemins système** | `C:\Windows\System32\` | `/usr/lib/`, `/usr/local/lib/` |
| **Séparateurs de chemin** | `\` (backslash) | `/` (slash) |
| **Sensibilité à la casse** | Non (fichiers) | Oui (tout) |
| **Variables d'environnement** | `%PATH%` | `$PATH`, `$LD_LIBRARY_PATH` |
| **Gestionnaire de paquets** | MSI, exe, chocolatey | apt, snap, dpkg |

### Impact sur les connexions bases de données

Ces différences affectent directement :

1. **Le chargement des pilotes clients** (drivers)
2. **La localisation des bibliothèques**
3. **Les chemins de connexion** (sockets, pipes nommés)
4. **Les mécanismes d'authentification**
5. **Les permissions et sécurité**

## Philosophies de gestion différentes

### Approche Windows

Windows favorise une approche **décentralisée et auto-contenue** :

```
MonApplication\
├── MonApp.exe
├── libpq.dll          ← Bibliothèque incluse avec l'app
├── libmysql.dll
├── config.ini
└── data\
```

**Avantages :**
- Applications autonomes (tout dans un dossier)
- Pas de dépendances système
- Installation facile (copier-coller)
- Versions multiples possibles simultanément

**Inconvénients :**
- Duplication des bibliothèques
- Mises à jour manuelles
- Gestion de sécurité par application

### Approche Linux

Linux privilégie une approche **centralisée et partagée** :

```
/usr/bin/monapp          ← Exécutable
/usr/lib/libpq.so        ← Bibliothèque système partagée
/etc/monapp/config.ini   ← Configuration système
/var/lib/monapp/data/    ← Données
```

**Avantages :**
- Bibliothèques partagées entre applications
- Mises à jour centralisées via gestionnaire de paquets
- Sécurité gérée au niveau système
- Économie d'espace disque

**Inconvénients :**
- Dépendances système obligatoires
- Risques de conflits de versions
- Installation plus complexe

## Stratégies de développement multi-plateforme

### Stratégie 1 : Détection automatique au runtime

Cette approche détecte le système d'exploitation et adapte automatiquement la configuration.

```pascal
uses
  SysUtils, SQLdb, PQConnection
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, Unix{$ENDIF};

type
  TSystemeOS = (osWindows, osLinux, osMacOS, osAutre);

function DetecterOS: TSystemeOS;  
begin
  {$IFDEF WINDOWS}
  Result := osWindows;
  {$ENDIF}
  {$IFDEF LINUX}
  Result := osLinux;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := osMacOS;
  {$ENDIF}
  {$IFNDEF WINDOWS}{$IFNDEF LINUX}{$IFNDEF DARWIN}
  Result := osAutre;
  {$ENDIF}{$ENDIF}{$ENDIF}
end;

procedure ConfigurerConnexionBD(Connection: TPQConnection);  
var
  OS: TSystemeOS;
  CheminBibliotheque: string;
begin
  OS := DetecterOS;

  case OS of
    osWindows:
      begin
        // Configuration Windows
        CheminBibliotheque := ExtractFilePath(ParamStr(0)) + 'libs\libpq.dll';
        Connection.Params.Values['ClientLibrary'] := CheminBibliotheque;
      end;

    osLinux:
      begin
        // Configuration Linux
        CheminBibliotheque := 'libpq.so.5'; // Recherche système automatique
        Connection.Params.Values['ClientLibrary'] := CheminBibliotheque;
      end;

    osMacOS:
      begin
        // Configuration macOS
        CheminBibliotheque := 'libpq.dylib';
        Connection.Params.Values['ClientLibrary'] := CheminBibliotheque;
      end;
  end;

  // Configuration commune
  Connection.HostName := 'localhost';
  Connection.DatabaseName := 'ma_base';
  Connection.UserName := 'user';
end;
```

### Stratégie 2 : Fichiers de configuration par OS

Utiliser des fichiers de configuration spécifiques à chaque plateforme.

**Structure de projet :**
```
MonProjet\
├── src\
│   └── main.pas
└── config\
    ├── windows.ini
    ├── linux.ini
    └── macos.ini
```

**Contenu de `windows.ini` :**
```ini
[Database]
Type=PostgreSQL  
Library=libs\libpq.dll  
Host=localhost  
Port=5432  
Database=ma_base  
User=postgres

[Paths]
Data=.\data\  
Logs=.\logs\  
Temp=%TEMP%\monapp\
```

**Contenu de `linux.ini` :**
```ini
[Database]
Type=PostgreSQL  
Library=libpq.so.5  
Host=localhost  
Port=5432  
Database=ma_base  
User=postgres

[Paths]
Data=/var/lib/monapp/data/  
Logs=/var/log/monapp/  
Temp=/tmp/monapp/
```

**Code pour charger la configuration appropriée :**

```pascal
uses
  SysUtils, IniFiles;

type
  TConfigurationBD = record
    TypeBD: string;
    Bibliotheque: string;
    Hote: string;
    Port: Integer;
    NomBase: string;
    Utilisateur: string;
  end;

function ChargerConfiguration: TConfigurationBD;  
var
  IniFile: TIniFile;
  CheminConfig: string;
begin
  // Déterminer le fichier de configuration selon l'OS
  {$IFDEF WINDOWS}
  CheminConfig := ExtractFilePath(ParamStr(0)) + 'config\windows.ini';
  {$ENDIF}
  {$IFDEF LINUX}
  CheminConfig := '/etc/monapp/linux.ini';
  {$ENDIF}
  {$IFDEF DARWIN}
  CheminConfig := ExtractFilePath(ParamStr(0)) + 'config/macos.ini';
  {$ENDIF}

  // Charger les valeurs
  IniFile := TIniFile.Create(CheminConfig);
  try
    Result.TypeBD := IniFile.ReadString('Database', 'Type', 'PostgreSQL');
    Result.Bibliotheque := IniFile.ReadString('Database', 'Library', '');
    Result.Hote := IniFile.ReadString('Database', 'Host', 'localhost');
    Result.Port := IniFile.ReadInteger('Database', 'Port', 5432);
    Result.NomBase := IniFile.ReadString('Database', 'Database', '');
    Result.Utilisateur := IniFile.ReadString('Database', 'User', '');
  finally
    IniFile.Free;
  end;
end;
```

### Stratégie 3 : Abstraction complète

Créer une couche d'abstraction qui cache complètement les différences OS.

```pascal
unit GestionnaireBD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB;

type
  // Interface abstraite pour la gestion des connexions
  IGestionnaireBD = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function ObtenirConnexion: TSQLConnection;
    procedure Connecter;
    procedure Deconnecter;
    function EstConnecte: Boolean;
  end;

  // Factory pour créer le gestionnaire approprié
  TGestionnaireBDFactory = class
  public
    class function Creer(const TypeBD: string): IGestionnaireBD;
  end;

implementation

uses
  {$IFDEF WINDOWS}
  GestionnaireBD.Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  GestionnaireBD.Linux,
  {$ENDIF}
  PQConnection, MySQLConnection;

class function TGestionnaireBDFactory.Creer(const TypeBD: string): IGestionnaireBD;  
begin
  {$IFDEF WINDOWS}
  Result := TGestionnaireBDWindows.Create(TypeBD);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := TGestionnaireBDLinux.Create(TypeBD);
  {$ENDIF}
end;

end.
```

**Utilisation dans l'application :**

```pascal
var
  GestionnaireBD: IGestionnaireBD;
begin
  // Création automatique selon l'OS
  GestionnaireBD := TGestionnaireBDFactory.Creer('PostgreSQL');

  // Utilisation identique sur tous les OS
  GestionnaireBD.Connecter;
  try
    // ... requêtes SQL ...
  finally
    GestionnaireBD.Deconnecter;
  end;
end;
```

## Gestion des chemins multi-plateformes

### Problème des séparateurs

```pascal
// ❌ MAUVAIS - Hardcodé pour Windows
CheminBD := 'C:\Programmes\MonApp\data\base.db';

// ✅ BON - Compatible multi-plateforme
CheminBD := ExtractFilePath(ParamStr(0)) + 'data' + PathDelim + 'base.db';
```

### Fonction utilitaire universelle

```pascal
uses
  SysUtils;

function ConstruireChemin(const Elements: array of string): string;  
var
  i: Integer;
begin
  Result := '';
  for i := Low(Elements) to High(Elements) do
  begin
    if i > Low(Elements) then
      Result := Result + PathDelim;
    Result := Result + Elements[i];
  end;
end;

// Utilisation
var
  CheminConfig: string;
begin
  CheminConfig := ConstruireChemin([
    ExtractFilePath(ParamStr(0)),
    'config',
    'database.ini'
  ]);
  // Windows: C:\MonApp\config\database.ini
  // Linux:   /opt/monapp/config/database.ini
end;
```

## Variables d'environnement spécifiques

### Windows - PATH pour les DLLs

```pascal
uses
  Windows, SysUtils;

procedure AjouterCheminDLL;  
var
  CheminDLLs: string;
begin
  CheminDLLs := ExtractFilePath(ParamStr(0)) + 'libs';
  SetEnvironmentVariable('PATH',
    PChar(CheminDLLs + ';' + GetEnvironmentVariable('PATH')));
end;
```

### Linux - LD_LIBRARY_PATH pour les .so

```pascal
uses
  Unix, SysUtils;

procedure AjouterCheminSO;  
var
  CheminSO, PathActuel: string;
begin
  CheminSO := '/opt/monapp/lib';
  PathActuel := GetEnvironmentVariable('LD_LIBRARY_PATH');

  if PathActuel <> '' then
    fpSetEnv(PChar('LD_LIBRARY_PATH'), PChar(CheminSO + ':' + PathActuel), 1)
  else
    fpSetEnv(PChar('LD_LIBRARY_PATH'), PChar(CheminSO), 1);
end;
```

## Tests et validation multi-plateformes

### Liste de vérification avant déploiement

```pascal
unit TestsMultiPlateforme;

{$mode objfpc}{$H+}

interface

type
  TResultatTest = (rtReussi, rtEchec, rtAvertissement);

  TTest = record
    Nom: string;
    Resultat: TResultatTest;
    Message: string;
  end;

function ExecuterTestsConnexion: TArray<TTest>;

implementation

uses
  SysUtils, Classes, SQLDB;

function TesterPresenceBibliotheque: TTest;  
begin
  Result.Nom := 'Présence bibliothèque BD';

  {$IFDEF WINDOWS}
  if FileExists(ExtractFilePath(ParamStr(0)) + 'libs\libpq.dll') then
  begin
    Result.Resultat := rtReussi;
    Result.Message := 'libpq.dll trouvée';
  end
  else
  begin
    Result.Resultat := rtEchec;
    Result.Message := 'libpq.dll manquante';
  end;
  {$ENDIF}

  {$IFDEF LINUX}
  // Sous Linux, on teste le chargement dynamique
  if FileExists('/usr/lib/x86_64-linux-gnu/libpq.so.5') or
     FileExists('/usr/lib/libpq.so.5') then
  begin
    Result.Resultat := rtReussi;
    Result.Message := 'libpq.so.5 trouvée';
  end
  else
  begin
    Result.Resultat := rtAvertissement;
    Result.Message := 'libpq.so.5 non trouvée (peut être ailleurs)';
  end;
  {$ENDIF}
end;

function TesterPermissionsFichiers: TTest;  
var
  TestFile: TextFile;
  CheminTest: string;
begin
  Result.Nom := 'Permissions fichiers';

  CheminTest := ExtractFilePath(ParamStr(0)) + 'test.tmp';

  try
    AssignFile(TestFile, CheminTest);
    Rewrite(TestFile);
    WriteLn(TestFile, 'test');
    CloseFile(TestFile);
    DeleteFile(CheminTest);

    Result.Resultat := rtReussi;
    Result.Message := 'Lecture/écriture OK';
  except
    on E: Exception do
    begin
      Result.Resultat := rtEchec;
      Result.Message := 'Erreur: ' + E.Message;
    end;
  end;
end;

function ExecuterTestsConnexion: TArray<TTest>;  
begin
  SetLength(Result, 2);
  Result[0] := TesterPresenceBibliotheque;
  Result[1] := TesterPermissionsFichiers;
end;

end.
```

## Bonnes pratiques de développement cross-platform

### 1. Utiliser les constantes système

```pascal
uses
  SysUtils;

const
  {$IFDEF WINDOWS}
  NOM_BIBLIOTHEQUE_PQ = 'libpq.dll';
  SEPARATEUR_PATH = ';';
  {$ENDIF}
  {$IFDEF LINUX}
  NOM_BIBLIOTHEQUE_PQ = 'libpq.so.5';
  SEPARATEUR_PATH = ':';
  {$ENDIF}
```

### 2. Centraliser la logique spécifique à l'OS

```pascal
unit PlatformeUtils;

interface

type
  TInfoPlateforme = record
    NomOS: string;
    Version: string;
    Architecture: string;
    SeparateurChemin: Char;
    ExtensionBibliotheque: string;
  end;

function ObtenirInfoPlateforme: TInfoPlateforme;  
function CheminBibliothequesParDefaut: string;  
function CheminConfigurationParDefaut: string;

implementation

// Implémentations spécifiques...

end.
```

### 3. Documenter les différences

Dans votre code, commentez clairement les sections spécifiques :

```pascal
procedure ConfigurerConnexion;  
begin
  // Configuration commune à tous les OS
  Connection.HostName := 'localhost';
  Connection.Port := 5432;

  {$IFDEF WINDOWS}
  // Spécifique Windows: utilisation de Named Pipes possible
  // Connection.Params.Add('protocol=pipe');
  {$ENDIF}

  {$IFDEF LINUX}
  // Spécifique Linux: utilisation de Unix Domain Sockets
  // Connection.Params.Add('host=/var/run/postgresql');
  {$ENDIF}
end;
```

## Conclusion de cette section

La configuration des connexions bases de données multi-plateformes nécessite une bonne compréhension des différences entre Windows et Linux. Les points essentiels à retenir :

1. **Les bibliothèques ont des extensions différentes** : `.dll` vs `.so`
2. **Les chemins système suivent des conventions différentes**
3. **Il existe plusieurs stratégies** pour gérer ces différences
4. **L'abstraction et les tests** sont vos meilleurs alliés

Dans les sections suivantes, nous détaillerons précisément :
- **8.3.1** : Comment gérer les chemins de bibliothèques sous Windows
- **8.3.2** : Comment gérer les chemins de bibliothèques sous Linux

Ces connaissances vous permettront de créer des applications véritablement portables, capables de s'adapter automatiquement à leur environnement d'exécution.

⏭️ [Chemins de bibliothèques Windows (.dll)](/08-bases-donnees-orm-multiplatefomes/03.1-chemins-bibliotheques-windows-dll.md)
