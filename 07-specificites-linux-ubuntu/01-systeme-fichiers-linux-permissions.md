🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.1 Système de fichiers Linux et permissions

## Introduction

Lorsque vous développez des applications FreePascal/Lazarus pour Linux/Ubuntu, comprendre le système de fichiers et les permissions est essentiel. Contrairement à Windows où ces concepts sont moins visibles, Linux impose une gestion stricte des droits d'accès qui affecte directement vos applications.

## Structure du système de fichiers Linux

### Arborescence standard

Linux organise tous les fichiers dans une arborescence unique partant de la racine `/`. Voici les répertoires principaux que vos applications peuvent rencontrer :

```
/                   # Racine du système
├── home           # Répertoires personnels des utilisateurs
│   └── username   # Votre dossier personnel (~)
├── etc            # Fichiers de configuration système
├── usr            # Applications et bibliothèques
│   ├── bin        # Programmes exécutables
│   ├── lib        # Bibliothèques partagées
│   └── share      # Données partagées
├── var            # Données variables
│   └── log        # Fichiers de logs
├── tmp            # Fichiers temporaires
├── opt            # Logiciels optionnels
└── mnt            # Points de montage
```

### Chemins dans FreePascal

En FreePascal, vous devez adapter vos chemins selon l'OS :

```pascal
uses
  SysUtils;

var
  ConfigPath: string;
begin
  {$IFDEF UNIX}
    // Sur Linux, utiliser les chemins standards
    ConfigPath := GetEnvironmentVariable('HOME') + '/.config/monapp/';
  {$ELSE}
    // Sur Windows
    ConfigPath := GetEnvironmentVariable('APPDATA') + '\MonApp\';
  {$ENDIF}

  // Créer le répertoire s'il n'existe pas
  ForceDirectories(ConfigPath);
end;
```

### Différences avec Windows

| Aspect | Linux | Windows |
|--------|-------|---------|
| Séparateur de chemin | `/` (slash) | `\` (antislash) |
| Racine | `/` unique | `C:\`, `D:\`, etc. |
| Sensible à la casse | Oui | Non |
| Fichiers cachés | Commencent par `.` | Attribut caché |
| Chemins utilisateur | `/home/username` | `C:\Users\username` |

## Les permissions Linux

### Concept de base

Chaque fichier et dossier sous Linux possède :
- **Un propriétaire** (user/owner)
- **Un groupe** (group)
- **Des permissions** pour trois catégories d'utilisateurs

### Les trois catégories d'utilisateurs

1. **Owner (u)** : Le propriétaire du fichier
2. **Group (g)** : Les membres du groupe associé
3. **Others (o)** : Tous les autres utilisateurs

### Les trois types de permissions

Pour chaque catégorie, on peut définir trois permissions :

1. **Read (r)** - Lecture
   - Fichier : lire le contenu
   - Dossier : lister le contenu

2. **Write (w)** - Écriture
   - Fichier : modifier le contenu
   - Dossier : créer/supprimer des fichiers

3. **Execute (x)** - Exécution
   - Fichier : exécuter comme programme
   - Dossier : traverser (entrer dans le dossier)

### Représentation des permissions

Les permissions s'affichent sous forme de 9 caractères :

```
rwxrwxrwx
│││││││││
│││││││└└└─ Others (autres) : r=lecture, w=écriture, x=exécution
│││││└└────── Group (groupe)
│└└────────── Owner (propriétaire)
```

Exemple concret :
```
-rw-r--r--  = fichier lisible par tous, modifiable par le propriétaire
drwxr-xr-x  = dossier accessible par tous, modifiable par le propriétaire
-rwx------  = programme exécutable uniquement par le propriétaire
```

### Notation octale

Les permissions peuvent aussi s'exprimer en octal :
- Read (r) = 4
- Write (w) = 2
- Execute (x) = 1

On additionne pour chaque catégorie :
```
755 = rwxr-xr-x  (7=4+2+1, 5=4+1, 5=4+1)
644 = rw-r--r--  (6=4+2, 4=4, 4=4)
600 = rw-------  (6=4+2, 0=0, 0=0)
```

## Gestion des permissions en FreePascal

### Lire les permissions d'un fichier

```pascal
uses
  BaseUnix, SysUtils;

function GetFilePermissions(const FileName: string): string;  
var
  Info: Stat;
  Mode: Integer;
begin
  if FpStat(FileName, Info) = 0 then
  begin
    Mode := Info.st_mode;
    Result := '';

    // Permissions du propriétaire
    if (Mode and S_IRUSR) <> 0 then Result := Result + 'r' else Result := Result + '-';
    if (Mode and S_IWUSR) <> 0 then Result := Result + 'w' else Result := Result + '-';
    if (Mode and S_IXUSR) <> 0 then Result := Result + 'x' else Result := Result + '-';

    // Permissions du groupe
    if (Mode and S_IRGRP) <> 0 then Result := Result + 'r' else Result := Result + '-';
    if (Mode and S_IWGRP) <> 0 then Result := Result + 'w' else Result := Result + '-';
    if (Mode and S_IXGRP) <> 0 then Result := Result + 'x' else Result := Result + '-';

    // Permissions des autres
    if (Mode and S_IROTH) <> 0 then Result := Result + 'r' else Result := Result + '-';
    if (Mode and S_IWOTH) <> 0 then Result := Result + 'w' else Result := Result + '-';
    if (Mode and S_IXOTH) <> 0 then Result := Result + 'x' else Result := Result + '-';
  end
  else
    Result := 'Erreur lecture permissions';
end;
```

### Modifier les permissions

```pascal
uses
  BaseUnix, SysUtils;

function SetFilePermissions(const FileName: string; Permissions: Integer): Boolean;  
begin
  {$IFDEF UNIX}
    Result := FpChmod(FileName, Permissions) = 0;
  {$ELSE}
    // Sous Windows, utiliser les attributs de fichier
    Result := True; // Permissions simplifiées sous Windows
  {$ENDIF}
end;

// Exemples d'utilisation
begin
  // Rendre un fichier exécutable par le propriétaire (755)
  SetFilePermissions('/home/user/monscript.sh', &755);

  // Fichier privé lisible/écrivable uniquement par le propriétaire (600)
  SetFilePermissions('/home/user/donnees_privees.txt', &600);

  // Fichier de configuration lisible par tous (644)
  SetFilePermissions('/home/user/.config/monapp/config.ini', &644);
end;
```

### Vérifier les permissions avant d'agir

```pascal
uses
  SysUtils;

procedure SafeWriteToFile(const FileName, Content: string);  
var
  F: TextFile;
begin
  // Vérifier si on peut écrire dans le fichier
  if not FileExists(FileName) then
  begin
    // Le fichier n'existe pas, vérifier le dossier parent
    if not DirectoryExists(ExtractFileDir(FileName)) then
    begin
      WriteLn('Erreur : Le dossier n''existe pas');
      Exit;
    end;
  end
  else if FileIsReadOnly(FileName) then
  begin
    WriteLn('Erreur : Le fichier est en lecture seule');
    Exit;
  end;

  try
    AssignFile(F, FileName);
    Rewrite(F);
    Write(F, Content);
    CloseFile(F);
    WriteLn('Fichier écrit avec succès');
  except
    on E: Exception do
      WriteLn('Erreur d''écriture : ', E.Message);
  end;
end;
```

## Gestion des propriétaires et groupes

### Obtenir les informations de propriété

```pascal
uses
  BaseUnix, Users, Grp;

procedure GetFileOwnership(const FileName: string);  
var
  Info: Stat;
  UserInfo: PPasswd;
  GroupInfo: PGroup;
begin
  if FpStat(FileName, Info) = 0 then
  begin
    // Récupérer les informations du propriétaire
    UserInfo := GetPwUid(Info.st_uid);
    if UserInfo <> nil then
      WriteLn('Propriétaire : ', UserInfo^.pw_name);

    // Récupérer les informations du groupe
    GroupInfo := GetGrGid(Info.st_gid);
    if GroupInfo <> nil then
      WriteLn('Groupe : ', GroupInfo^.gr_name);
  end;
end;
```

### Changer le propriétaire (nécessite les droits root)

```pascal
uses
  BaseUnix;

function ChangeOwner(const FileName: string; UID, GID: Integer): Boolean;  
begin
  {$IFDEF UNIX}
    Result := FpChown(FileName, UID, GID) = 0;
  {$ELSE}
    Result := True; // Pas de concept équivalent sous Windows
  {$ENDIF}
end;
```

## Permissions spéciales

### Le bit setuid (4000)

Permet à un programme de s'exécuter avec les droits de son propriétaire :

```pascal
// Définir le bit setuid
SetFilePermissions('/usr/bin/monprogramme', &4755); // 4755 = setuid + rwxr-xr-x
```

### Le bit setgid (2000)

Pour les fichiers : exécution avec les droits du groupe  
Pour les dossiers : nouveaux fichiers héritent du groupe

```pascal
// Définir le bit setgid sur un dossier
SetFilePermissions('/var/shared/projet', &2775); // 2775 = setgid + rwxrwxr-x
```

### Le sticky bit (1000)

Sur les dossiers, empêche la suppression de fichiers par d'autres utilisateurs :

```pascal
// Définir le sticky bit (typique pour /tmp)
SetFilePermissions('/var/tmp/partage', &1777); // 1777 = sticky + rwxrwxrwx
```

## Bonnes pratiques pour vos applications

### 1. Répertoires de configuration

```pascal
function GetAppConfigDir: string;  
begin
  {$IFDEF UNIX}
    // Respecter les standards XDG
    Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
    if Result = '' then
      Result := GetEnvironmentVariable('HOME') + '/.config';
    Result := Result + '/monapp/';
  {$ELSE}
    Result := GetAppConfigDir(False);
  {$ENDIF}
  ForceDirectories(Result);
end;
```

### 2. Répertoires de données

```pascal
function GetAppDataDir: string;  
begin
  {$IFDEF UNIX}
    Result := GetEnvironmentVariable('XDG_DATA_HOME');
    if Result = '' then
      Result := GetEnvironmentVariable('HOME') + '/.local/share';
    Result := Result + '/monapp/';
  {$ELSE}
    Result := GetAppConfigDir(False);
  {$ENDIF}
  ForceDirectories(Result);
end;
```

### 3. Fichiers temporaires

```pascal
function CreateTempFile: string;  
begin
  {$IFDEF UNIX}
    Result := GetTempDir + 'monapp_' + IntToStr(GetProcessID) + '.tmp';
  {$ELSE}
    Result := GetTempFileName;
  {$ENDIF}
end;
```

### 4. Gestion sécurisée des permissions

```pascal
procedure CreateSecureConfigFile(const FileName, Content: string);  
var
  F: TextFile;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  Write(F, Content);
  CloseFile(F);

  {$IFDEF UNIX}
    // Permissions restrictives pour les fichiers sensibles
    SetFilePermissions(FileName, &600); // rw-------
  {$ENDIF}
end;
```

## Cas d'usage courants

### Installation d'une application

```pascal
procedure InstallApplication;  
begin
  {$IFDEF UNIX}
    // Binaire exécutable
    CopyFile('monapp', '/usr/local/bin/monapp');
    SetFilePermissions('/usr/local/bin/monapp', &755);

    // Fichiers de configuration système
    CopyFile('monapp.conf', '/etc/monapp.conf');
    SetFilePermissions('/etc/monapp.conf', &644);

    // Données partagées
    ForceDirectories('/usr/share/monapp');
    SetFilePermissions('/usr/share/monapp', &755);
  {$ENDIF}
end;
```

### Création de logs

```pascal
procedure WriteLog(const Message: string);  
var
  LogFile: string;
  F: TextFile;
begin
  {$IFDEF UNIX}
    LogFile := '/var/log/monapp.log';
    // S'assurer que le fichier est accessible en écriture
    if not FileExists(LogFile) then
    begin
      // Créer avec les bonnes permissions
      AssignFile(F, LogFile);
      Rewrite(F);
      CloseFile(F);
      SetFilePermissions(LogFile, &666); // rw-rw-rw-
    end;
  {$ELSE}
    LogFile := GetAppConfigDir(False) + 'monapp.log';
  {$ENDIF}

  // Écrire dans le log
  AssignFile(F, LogFile);
  Append(F);
  WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' - ', Message);
  CloseFile(F);
end;
```

## Conseils pour le développement cross-platform

1. **Toujours utiliser les constantes PathDelim et DirectorySeparator** au lieu de '/' ou '\'

2. **Tester les permissions avant d'agir** pour éviter les exceptions

3. **Respecter les conventions de chaque OS** pour l'emplacement des fichiers

4. **Documenter les permissions requises** pour votre application

5. **Prévoir des fallbacks** si certains répertoires ne sont pas accessibles

6. **Utiliser la compilation conditionnelle** pour gérer les différences OS

Cette compréhension du système de fichiers et des permissions Linux vous permettra de créer des applications FreePascal/Lazarus robustes et respectueuses des conventions de chaque plateforme.

⏭️ [Services systemd](/07-specificites-linux-ubuntu/02-services-systemd.md)
