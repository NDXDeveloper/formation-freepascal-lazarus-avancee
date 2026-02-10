🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.12 Directives de compilation conditionnelle multi-OS

## Table des matières
1. [Introduction : Qu'est-ce que la compilation conditionnelle ?](#introduction)
2. [Les bases de la compilation conditionnelle](#bases)
3. [Directives système prédéfinies](#directives-systeme)
4. [Détection du système d'exploitation](#detection-os)
5. [Détection de l'architecture processeur](#detection-cpu)
6. [Gestion des chemins et séparateurs](#chemins)
7. [Interfaces utilisateur multi-plateformes](#interfaces)
8. [Gestion des processus et services](#processus)
9. [Accès au système de fichiers](#fichiers)
10. [Réseau et communications](#reseau)
11. [Bibliothèques et liens dynamiques](#bibliotheques)
12. [Gestion des différences de comportement](#comportement)
13. [Organisation du code multi-plateforme](#organisation)
14. [Techniques avancées](#techniques-avancees)
15. [Débogage et tests multi-plateformes](#debogage)
16. [Bonnes pratiques et pièges à éviter](#bonnes-pratiques)

## 1. Introduction : Qu'est-ce que la compilation conditionnelle ? {#introduction}

### Le concept expliqué simplement

Imaginez que vous écrivez une lettre qui doit être lue par des personnes parlant différentes langues. Vous pourriez écrire : "Si le lecteur parle français, lire ce paragraphe, sinon si le lecteur parle anglais, lire cet autre paragraphe". La compilation conditionnelle fonctionne exactement de la même manière, mais pour différents systèmes d'exploitation.

La compilation conditionnelle permet d'inclure ou d'exclure des portions de code selon certaines conditions **au moment de la compilation**. C'est essentiel pour créer des programmes qui fonctionnent sur Windows ET Linux/Ubuntu avec le même code source.

### Pourquoi est-ce nécessaire ?

Les systèmes d'exploitation ont des différences fondamentales :

```pascal
// ❌ PROBLÈME : Ce code ne fonctionne QUE sur Windows
uses Windows;  // Unité Windows n'existe pas sur Linux !

procedure LireRegistre;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  Reg.OpenKey('HKEY_CURRENT_USER\Software\MonApp', False);
  // ... Le registre Windows n'existe pas sur Linux !
end;
```

```pascal
// ✅ SOLUTION : Compilation conditionnelle
procedure SauverConfiguration(const Cle, Valeur: string);
begin
  {$IFDEF WINDOWS}
    // Code pour Windows : utilise le registre
    SauverDansRegistre(Cle, Valeur);
  {$ENDIF}

  {$IFDEF UNIX}
    // Code pour Linux : utilise un fichier de config
    SauverDansFichierConfig(Cle, Valeur);
  {$ENDIF}
end;
```

### Comment ça fonctionne ?

Le compilateur FreePascal lit votre code et :
1. Détecte sur quel système il compile
2. Active automatiquement certains symboles (WINDOWS, LINUX, etc.)
3. Inclut ou ignore le code selon ces symboles
4. Produit un exécutable spécifique à la plateforme

```
Code source unique (.pas)
         ↓
    Compilation
    ↙        ↘
Sur Windows    Sur Linux
    ↓            ↓
 prog.exe    prog (ELF)
```

## 2. Les bases de la compilation conditionnelle {#bases}

### La directive {$IFDEF}

La forme la plus simple de compilation conditionnelle :

```pascal
{$IFDEF SYMBOLE}
  // Code compilé UNIQUEMENT si SYMBOLE est défini
{$ENDIF}
```

**Exemple concret :**
```pascal
program MonProgramme;

begin
  WriteLn('Bonjour depuis FreePascal !');

  {$IFDEF WINDOWS}
    WriteLn('Vous êtes sur Windows');
  {$ENDIF}

  {$IFDEF LINUX}
    WriteLn('Vous êtes sur Linux');
  {$ENDIF}

  WriteLn('Fin du programme');
end.
```

### La directive {$IFNDEF}

L'inverse de {$IFDEF} - compile si le symbole N'EST PAS défini :

```pascal
{$IFNDEF SYMBOLE}
  // Code compilé si SYMBOLE n'est PAS défini
{$ENDIF}
```

**Exemple :**
```pascal
{$IFNDEF WINDOWS}
  WriteLn('Vous n''êtes PAS sur Windows');
  WriteLn('Donc probablement sur Linux, Mac ou autre');
{$ENDIF}
```

### La directive {$ELSE}

Permet de créer une alternative :

```pascal
{$IFDEF WINDOWS}
  WriteLn('Code Windows');
{$ELSE}
  WriteLn('Code pour tous les autres systèmes');
{$ENDIF}
```

### La directive {$ELSEIF} ou {$ELSEDEF}

Pour tester plusieurs conditions :

```pascal
{$IFDEF WINDOWS}
  WriteLn('Windows détecté');
{$ELSEIF DEFINED(LINUX)}
  WriteLn('Linux détecté');
{$ELSEIF DEFINED(DARWIN)}
  WriteLn('macOS détecté');
{$ELSE}
  WriteLn('Système non reconnu');
{$ENDIF}
```

### Définir ses propres symboles

Vous pouvez créer vos propres symboles :

```pascal
// Définir un symbole
{$DEFINE MON_SYMBOLE}

// Plus loin dans le code
{$IFDEF MON_SYMBOLE}
  WriteLn('Mon symbole est actif');
{$ENDIF}

// Supprimer un symbole
{$UNDEF MON_SYMBOLE}
```

**Définir depuis la ligne de commande :**
```bash
# Windows
fpc -dMODE_DEBUG programme.pas

# Linux
fpc -dMODE_DEBUG programme.pas
```

## 3. Directives système prédéfinies {#directives-systeme}

FreePascal définit automatiquement de nombreux symboles selon l'environnement :

### Symboles de système d'exploitation

```pascal
// Systèmes d'exploitation principaux
{$IFDEF WINDOWS}     // Tous les Windows (XP, 7, 8, 10, 11)
{$IFDEF WIN32}       // Windows 32 bits
{$IFDEF WIN64}       // Windows 64 bits
{$IFDEF MSWINDOWS}   // Alias pour WINDOWS

{$IFDEF UNIX}        // Tous les systèmes Unix-like
{$IFDEF LINUX}       // Linux (incluant Ubuntu, Debian, etc.)
{$IFDEF FREEBSD}     // FreeBSD
{$IFDEF NETBSD}      // NetBSD
{$IFDEF OPENBSD}     // OpenBSD
{$IFDEF SUNOS}       // Solaris/SunOS
{$IFDEF DARWIN}      // macOS
{$IFDEF ANDROID}     // Android
{$IFDEF AIX}         // IBM AIX

{$IFDEF HAIKU}       // Haiku OS
{$IFDEF BEOS}        // BeOS (historique)
```

### Symboles d'architecture processeur

```pascal
// Architectures processeur
{$IFDEF CPU32}       // Processeur 32 bits (toute architecture)
{$IFDEF CPU64}       // Processeur 64 bits (toute architecture)

{$IFDEF CPUI386}     // Intel 386 et compatibles (32 bits x86)
{$IFDEF CPUX86_64}   // AMD64/Intel 64 bits (x86-64)
{$IFDEF CPUAMD64}    // Alias pour CPUX86_64

{$IFDEF CPUARM}      // ARM 32 bits
{$IFDEF CPUAARCH64}  // ARM 64 bits

{$IFDEF CPUPOWERPC}  // PowerPC
{$IFDEF CPUPOWERPC64}// PowerPC 64 bits

{$IFDEF CPUMIPS}     // MIPS
{$IFDEF CPUSPARC}    // SPARC
{$IFDEF CPUM68K}     // Motorola 68000
```

### Symboles de compilateur et version

```pascal
// Version du compilateur
{$IFDEF VER3}        // FreePascal version 3.x
{$IFDEF VER3_0}      // FreePascal version 3.0.x
{$IFDEF VER3_2}      // FreePascal version 3.2.x

// Mode de compilation
{$IFDEF FPC}         // Compilateur FreePascal (toujours défini)
{$IFDEF DEBUG}       // Mode debug (si activé)
{$IFDEF RELEASE}     // Mode release (si activé)

// Fonctionnalités
{$IFDEF UNICODE}     // Support Unicode activé
{$IFDEF CPUSTRINGOPS}// Opérations string optimisées CPU
```

### Vérifier les symboles disponibles

```pascal
program AfficherSymboles;

begin
  WriteLn('=== Informations système ===');

  // Système d'exploitation
  Write('OS: ');
  {$IFDEF WINDOWS}WriteLn('Windows');{$ENDIF}
  {$IFDEF LINUX}WriteLn('Linux');{$ENDIF}
  {$IFDEF DARWIN}WriteLn('macOS');{$ENDIF}
  {$IFDEF FREEBSD}WriteLn('FreeBSD');{$ENDIF}

  // Architecture
  Write('Architecture: ');
  {$IFDEF CPU32}Write('32-bit ');{$ENDIF}
  {$IFDEF CPU64}Write('64-bit ');{$ENDIF}
  {$IFDEF CPUI386}WriteLn('x86');{$ENDIF}
  {$IFDEF CPUX86_64}WriteLn('x86-64');{$ENDIF}
  {$IFDEF CPUARM}WriteLn('ARM');{$ENDIF}
  {$IFDEF CPUAARCH64}WriteLn('ARM64');{$ENDIF}

  // Version FPC
  WriteLn('Compilateur: FreePascal ', {$I %FPCVERSION%});
  WriteLn('Target: ', {$I %FPCTARGETOS%});
  WriteLn('Date compilation: ', {$I %DATE%}, ' ', {$I %TIME%});
end.
```

## 4. Détection du système d'exploitation {#detection-os}

### Approche basique : IFDEF simple

```pascal
unit OSDetection;

interface

function GetOSName: string;
function IsWindows: Boolean;
function IsLinux: Boolean;
function IsUnix: Boolean;

implementation

function GetOSName: string;
begin
  {$IFDEF WINDOWS}
    Result := 'Windows';
  {$ENDIF}

  {$IFDEF LINUX}
    Result := 'Linux';
  {$ENDIF}

  {$IFDEF DARWIN}
    Result := 'macOS';
  {$ENDIF}

  {$IFDEF FREEBSD}
    Result := 'FreeBSD';
  {$ENDIF}

  {$IFDEF ANDROID}
    Result := 'Android';
  {$ENDIF}

  {$IFNDEF WINDOWS}{$IFNDEF LINUX}{$IFNDEF DARWIN}{$IFNDEF FREEBSD}{$IFNDEF ANDROID}
    Result := 'Unknown OS';
  {$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
end;

function IsWindows: Boolean;
begin
  {$IFDEF WINDOWS}
    Result := True;
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

function IsLinux: Boolean;
begin
  {$IFDEF LINUX}
    Result := True;
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

function IsUnix: Boolean;
begin
  {$IFDEF UNIX}
    Result := True;
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

end.
```

### Détection détaillée avec informations système

```pascal
unit SystemInfo;

interface

type
  TSystemInfo = record
    OSName: string;
    OSVersion: string;
    Architecture: string;
    ProcessorCount: Integer;
    UserName: string;
    ComputerName: string;
    HomeDirectory: string;
    TempDirectory: string;
  end;

function GetSystemInfo: TSystemInfo;

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, BaseUnix, Unix{$ENDIF};

function GetSystemInfo: TSystemInfo;
{$IFDEF WINDOWS}
var
  VersionInfo: TOSVersionInfo;
  ComputerNameBuf: array[0..255] of Char;
  Size: DWORD;
{$ENDIF}
{$IFDEF UNIX}
var
  UName: UtsName;
{$ENDIF}
begin
  // Nom de l'OS
  {$IFDEF WINDOWS}
    Result.OSName := 'Windows';

    // Version Windows
    VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
    if GetVersionEx(VersionInfo) then
      Result.OSVersion := Format('%d.%d Build %d',
        [VersionInfo.dwMajorVersion,
         VersionInfo.dwMinorVersion,
         VersionInfo.dwBuildNumber])
    else
      Result.OSVersion := 'Unknown';

    // Nom de l'ordinateur
    Size := SizeOf(ComputerNameBuf);
    if GetComputerName(ComputerNameBuf, Size) then
      Result.ComputerName := ComputerNameBuf
    else
      Result.ComputerName := 'Unknown';
  {$ENDIF}

  {$IFDEF LINUX}
    Result.OSName := 'Linux';

    // Information système Unix
    if FpUname(UName) = 0 then
    begin
      Result.OSVersion := Format('%s %s',
        [UName.Release, UName.Version]);
      Result.ComputerName := UName.NodeName;
    end
    else
    begin
      Result.OSVersion := 'Unknown';
      Result.ComputerName := 'Unknown';
    end;
  {$ENDIF}

  {$IFDEF DARWIN}
    Result.OSName := 'macOS';
    if FpUname(UName) = 0 then
    begin
      Result.OSVersion := Format('%s %s',
        [UName.Release, UName.Version]);
      Result.ComputerName := UName.NodeName;
    end;
  {$ENDIF}

  // Architecture
  {$IFDEF CPU32}
    Result.Architecture := '32-bit';
  {$ENDIF}
  {$IFDEF CPU64}
    Result.Architecture := '64-bit';
  {$ENDIF}

  {$IFDEF CPUI386}
    Result.Architecture := Result.Architecture + ' x86';
  {$ENDIF}
  {$IFDEF CPUX86_64}
    Result.Architecture := Result.Architecture + ' x86-64';
  {$ENDIF}
  {$IFDEF CPUARM}
    Result.Architecture := Result.Architecture + ' ARM';
  {$ENDIF}

  // Informations communes (cross-platform)
  // Note : .ToInteger est une méthode Delphi. En FPC, utiliser StrToIntDef
  Result.ProcessorCount := StrToIntDef(GetEnvironmentVariable('NUMBER_OF_PROCESSORS'), 1);

  Result.UserName := GetEnvironmentVariable('USER');
  if Result.UserName = '' then
    Result.UserName := GetEnvironmentVariable('USERNAME');

  Result.HomeDirectory := GetEnvironmentVariable('HOME');
  if Result.HomeDirectory = '' then
    Result.HomeDirectory := GetEnvironmentVariable('USERPROFILE');

  Result.TempDirectory := GetTempDir;
end;

end.
```

## 5. Détection de l'architecture processeur {#detection-cpu}

### Détection simple de l'architecture

```pascal
unit CPUDetection;

interface

type
  TCPUInfo = record
    Bits: Integer;           // 32 ou 64
    Architecture: string;    // x86, x86-64, ARM, etc.
    Endianness: string;      // Little-endian ou Big-endian
    HasSSE: Boolean;
    HasSSE2: Boolean;
    HasSSE3: Boolean;
    HasAVX: Boolean;
    HasAVX2: Boolean;
  end;

function GetCPUInfo: TCPUInfo;
function IsLittleEndian: Boolean;

implementation

function IsLittleEndian: Boolean;
var
  n: Integer;
begin
  n := 1;
  Result := PByte(@n)^ = 1;
end;

function GetCPUInfo: TCPUInfo;
begin
  // Bits du processeur
  {$IFDEF CPU32}
    Result.Bits := 32;
  {$ENDIF}
  {$IFDEF CPU64}
    Result.Bits := 64;
  {$ENDIF}

  // Architecture
  {$IFDEF CPUI386}
    Result.Architecture := 'x86 (i386)';
  {$ELSEIF DEFINED(CPUX86_64)}
    Result.Architecture := 'x86-64 (AMD64)';
  {$ELSEIF DEFINED(CPUARM)}
    Result.Architecture := 'ARM 32-bit';
  {$ELSEIF DEFINED(CPUAARCH64)}
    Result.Architecture := 'ARM 64-bit (AArch64)';
  {$ELSEIF DEFINED(CPUPOWERPC)}
    Result.Architecture := 'PowerPC';
  {$ELSEIF DEFINED(CPUPOWERPC64)}
    Result.Architecture := 'PowerPC 64-bit';
  {$ELSEIF DEFINED(CPUMIPS)}
    Result.Architecture := 'MIPS';
  {$ELSEIF DEFINED(CPUSPARC)}
    Result.Architecture := 'SPARC';
  {$ELSE}
    Result.Architecture := 'Unknown';
  {$ENDIF}

  // Endianness
  if IsLittleEndian then
    Result.Endianness := 'Little-endian'
  else
    Result.Endianness := 'Big-endian';

  // Capacités SIMD (x86/x64 seulement)
  {$IF DEFINED(CPUI386) OR DEFINED(CPUX86_64)}
    {$IFDEF HASSSE}
      Result.HasSSE := True;
    {$ELSE}
      Result.HasSSE := False;
    {$ENDIF}

    {$IFDEF HASSSE2}
      Result.HasSSE2 := True;
    {$ELSE}
      Result.HasSSE2 := False;
    {$ENDIF}

    {$IFDEF HASSSE3}
      Result.HasSSE3 := True;
    {$ELSE}
      Result.HasSSE3 := False;
    {$ENDIF}

    {$IFDEF HASAVX}
      Result.HasAVX := True;
    {$ELSE}
      Result.HasAVX := False;
    {$ENDIF}

    {$IFDEF HASAVX2}
      Result.HasAVX2 := True;
    {$ELSE}
      Result.HasAVX2 := False;
    {$ENDIF}
  {$ENDIF}
end;

end.
```

### Optimisations conditionnelles selon l'architecture

```pascal
unit OptimizedFunctions;

interface

function FastMemCopy(Dest, Source: Pointer; Size: NativeInt): Boolean;
function FastSum(const Values: array of Single): Single;

implementation

function FastMemCopy(Dest, Source: Pointer; Size: NativeInt): Boolean;
begin
  {$IF DEFINED(CPUX86_64) AND DEFINED(HASAVX)}
    // Version optimisée AVX pour x86-64
    Result := MemCopyAVX(Dest, Source, Size);
  {$ELSEIF DEFINED(CPUX86_64) AND DEFINED(HASSSE2)}
    // Version optimisée SSE2 pour x86-64
    Result := MemCopySSE2(Dest, Source, Size);
  {$ELSEIF DEFINED(CPUARM) AND DEFINED(HASNEON)}
    // Version optimisée NEON pour ARM
    Result := MemCopyNEON(Dest, Source, Size);
  {$ELSE}
    // Version générique portable
    Move(Source^, Dest^, Size);
    Result := True;
  {$ENDIF}
end;

function FastSum(const Values: array of Single): Single;
var
  i: Integer;
begin
  Result := 0;

  {$IF DEFINED(CPUX86_64) AND DEFINED(HASSSE)}
    // Somme vectorisée SSE pour x86-64
    // Traite 4 valeurs simultanément
    i := 0;
    while i <= High(Values) - 3 do
    begin
      // Code SSE ici
      Inc(i, 4);
    end;
    // Traiter les éléments restants
    while i <= High(Values) do
    begin
      Result := Result + Values[i];
      Inc(i);
    end;
  {$ELSEIF DEFINED(CPUARM) AND DEFINED(HASNEON)}
    // Version NEON pour ARM
    // Code NEON ici
  {$ELSE}
    // Version scalaire standard
    for i := 0 to High(Values) do
      Result := Result + Values[i];
  {$ENDIF}
end;

end.
```

## 6. Gestion des chemins et séparateurs {#chemins}

### Différences fondamentales entre OS

```pascal
unit CrossPlatformPaths;

interface

const
  // Note : Les constantes PathDelim, PathSeparator et LineEnding sont
  // déjà définies dans l'unité System de FPC. Ici nous définissons des
  // constantes complémentaires pour l'exemple.
  {$IFDEF WINDOWS}
    ExeExtension = '.exe';
    LibExtension = '.dll';
  {$ENDIF}

  {$IFDEF UNIX}
    ExeExtension = '';
    {$IFDEF DARWIN}
      LibExtension = '.dylib';
    {$ELSE}
      LibExtension = '.so';
    {$ENDIF}
  {$ENDIF}

  // Rappel des constantes FPC intégrées (System) :
  // - PathDelim      : séparateur de répertoire (\ ou /)
  // - PathSeparator  : séparateur de PATH (;  ou :)
  // - LineEnding     : fin de ligne (#13#10 ou #10)

function GetConfigPath: string;
function GetDataPath: string;
function GetTempPath: string;
function GetExecutablePath: string;
function BuildPath(const Parts: array of string): string;
function NormalizePath(const Path: string): string;

implementation

uses
  SysUtils;

function GetConfigPath: string;
begin
  {$IFDEF WINDOWS}
    // Windows : %APPDATA%\MonApp
    Result := GetEnvironmentVariable('APPDATA');
    if Result = '' then
      Result := GetEnvironmentVariable('USERPROFILE') + '\AppData\Roaming';
    Result := Result + '\MonApp';
  {$ENDIF}

  {$IFDEF LINUX}
    // Linux : ~/.config/monapp
    Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
    if Result = '' then
      Result := GetEnvironmentVariable('HOME') + '/.config';
    Result := Result + '/monapp';
  {$ENDIF}

  {$IFDEF DARWIN}
    // macOS : ~/Library/Preferences/MonApp
    Result := GetEnvironmentVariable('HOME') + '/Library/Preferences/MonApp';
  {$ENDIF}

  // Créer le dossier s'il n'existe pas
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetDataPath: string;
begin
  {$IFDEF WINDOWS}
    // Windows : %LOCALAPPDATA%\MonApp
    Result := GetEnvironmentVariable('LOCALAPPDATA');
    if Result = '' then
      Result := GetEnvironmentVariable('USERPROFILE') + '\AppData\Local';
    Result := Result + '\MonApp';
  {$ENDIF}

  {$IFDEF LINUX}
    // Linux : ~/.local/share/monapp
    Result := GetEnvironmentVariable('XDG_DATA_HOME');
    if Result = '' then
      Result := GetEnvironmentVariable('HOME') + '/.local/share';
    Result := Result + '/monapp';
  {$ENDIF}

  {$IFDEF DARWIN}
    // macOS : ~/Library/Application Support/MonApp
    Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/MonApp';
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetTempPath: string;
begin
  {$IFDEF WINDOWS}
    Result := GetEnvironmentVariable('TEMP');
    if Result = '' then
      Result := GetEnvironmentVariable('TMP');
    if Result = '' then
      Result := 'C:\Temp';
  {$ENDIF}

  {$IFDEF UNIX}
    Result := GetEnvironmentVariable('TMPDIR');
    if Result = '' then
      Result := '/tmp';
  {$ENDIF}

  Result := IncludeTrailingPathDelimiter(Result);
end;

function GetExecutablePath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function BuildPath(const Parts: array of string): string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(Parts) to High(Parts) do
  begin
    if i > Low(Parts) then
      Result := Result + PathDelim;
    Result := Result + Parts[i];
  end;
end;

function NormalizePath(const Path: string): string;
begin
  Result := Path;

  {$IFDEF WINDOWS}
    // Windows : remplacer / par \
    Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
    // Gérer les chemins UNC
    if (Length(Result) > 2) and (Result[1] = '\') and (Result[2] = '\') then
      ; // Chemin UNC, ne rien faire
  {$ENDIF}

  {$IFDEF UNIX}
    // Unix : remplacer \ par /
    Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
    // Expansion du tilde
    if (Length(Result) > 0) and (Result[1] = '~') then
      Result := GetEnvironmentVariable('HOME') + Copy(Result, 2, MaxInt);
  {$ENDIF}

  // Supprimer les doubles séparateurs
  Result := StringReplace(Result, PathDelim + PathDelim,
                          PathDelim, [rfReplaceAll]);
end;

end.
```

### Exemple d'utilisation des chemins

```pascal
program TestPaths;

uses
  CrossPlatformPaths, SysUtils;

var
  ConfigFile: string;
  DataFile: string;
  LogFile: string;

begin
  WriteLn('=== Test des chemins multi-plateformes ===');

  // Fichier de configuration
  ConfigFile := GetConfigPath + PathDelim + 'settings.ini';
  WriteLn('Config: ', ConfigFile);

  // Fichier de données
  DataFile := BuildPath([GetDataPath, 'data', 'database.db']);
  WriteLn('Data: ', DataFile);

  // Fichier de log temporaire
  LogFile := GetTempPath + 'monapp_' + FormatDateTime('yyyymmdd', Now) + '.log';
  WriteLn('Log: ', LogFile);

  // Chemin de l'exécutable
  WriteLn('Executable: ', GetExecutablePath);

  // Test de normalisation
  {$IFDEF WINDOWS}
    WriteLn('Normalisé: ', NormalizePath('C:/Users/Test/Documents'));
    // Affiche : C:\Users\Test\Documents
  {$ENDIF}

  {$IFDEF UNIX}
    WriteLn('Normalisé: ', NormalizePath('~/Documents\Test'));
    // Affiche : /home/user/Documents/Test
  {$ENDIF}
end.
```

## 7. Interfaces utilisateur multi-plateformes {#interfaces}

### Gestion des boîtes de dialogue

```pascal
unit CrossPlatformDialogs;

interface

function ShowMessage(const Msg: string): Boolean;
function ShowError(const Msg: string): Boolean;
function ShowQuestion(const Question: string): Boolean;
function InputBox(const Caption, Prompt, Default: string): string;

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF LINUX}, Process{$ENDIF}
    {$IFDEF DARWIN}, MacOSAll{$ENDIF}
  {$ENDIF};

function ShowMessage(const Msg: string): Boolean;
{$IF DEFINED(LINUX) OR DEFINED(DARWIN)}
var
  Output: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    MessageBox(0, PChar(Msg), 'Information', MB_OK or MB_ICONINFORMATION);
    Result := True;
  {$ENDIF}

  {$IFDEF LINUX}
    // Utiliser zenity si disponible
    if FileExists('/usr/bin/zenity') then
    begin
      Result := RunCommand('/usr/bin/zenity',
        ['--info', '--text=' + Msg], Output);
    end
    // Sinon utiliser kdialog (KDE)
    else if FileExists('/usr/bin/kdialog') then
    begin
      Result := RunCommand('/usr/bin/kdialog',
        ['--msgbox', Msg], Output);
    end
    // Sinon afficher dans le terminal
    else
    begin
      WriteLn('[INFO] ', Msg);
      Result := True;
    end;
  {$ENDIF}

  {$IFDEF DARWIN}
    // Utiliser osascript pour macOS
    Result := RunCommand('/usr/bin/osascript',
      ['-e', 'display dialog "' + Msg + '" buttons {"OK"} default button 1'],
      Output);
  {$ENDIF}
end;

function ShowError(const Msg: string): Boolean;
{$IF DEFINED(LINUX) OR DEFINED(DARWIN)}
var
  Output: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    MessageBox(0, PChar(Msg), 'Erreur', MB_OK or MB_ICONERROR);
    Result := True;
  {$ENDIF}

  {$IFDEF LINUX}
    if FileExists('/usr/bin/zenity') then
    begin
      Result := RunCommand('/usr/bin/zenity',
        ['--error', '--text=' + Msg], Output);
    end
    else if FileExists('/usr/bin/kdialog') then
    begin
      Result := RunCommand('/usr/bin/kdialog',
        ['--error', Msg], Output);
    end
    else
    begin
      WriteLn('[ERROR] ', Msg);
      Result := True;
    end;
  {$ENDIF}

  {$IFDEF DARWIN}
    Result := RunCommand('/usr/bin/osascript',
      ['-e', 'display dialog "' + Msg + '" buttons {"OK"} default button 1 with icon stop'],
      Output);
  {$ENDIF}
end;

function ShowQuestion(const Question: string): Boolean;
{$IFDEF WINDOWS}
var
  Response: Integer;
{$ENDIF}
{$IF DEFINED(LINUX) OR DEFINED(DARWIN)}
var
  Output: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    Response := MessageBox(0, PChar(Question), 'Question',
                          MB_YESNO or MB_ICONQUESTION);
    Result := Response = IDYES;
  {$ENDIF}

  {$IFDEF LINUX}
    if FileExists('/usr/bin/zenity') then
    begin
      Result := RunCommand('/usr/bin/zenity',
        ['--question', '--text=' + Question], Output);
    end
    else if FileExists('/usr/bin/kdialog') then
    begin
      Result := RunCommand('/usr/bin/kdialog',
        ['--yesno', Question], Output);
    end
    else
    begin
      Write('[QUESTION] ', Question, ' (O/N) ? ');
      ReadLn(Output);
      Result := (UpperCase(Output) = 'O') or (UpperCase(Output) = 'Y');
    end;
  {$ENDIF}

  {$IFDEF DARWIN}
    if RunCommand('/usr/bin/osascript',
      ['-e', 'button returned of (display dialog "' + Question +
       '" buttons {"Non", "Oui"} default button 2)'], Output) then
      Result := Pos('Oui', Output) > 0
    else
      Result := False;
  {$ENDIF}
end;

function InputBox(const Caption, Prompt, Default: string): string;
{$IFDEF WINDOWS}
var
  Buffer: array[0..255] of Char;
{$ENDIF}
begin
  Result := Default;

  {$IFDEF WINDOWS}
    StrPCopy(Buffer, Default);
    if InputQuery(Caption, Prompt, Buffer) then
      Result := Buffer
    else
      Result := Default;
  {$ENDIF}

  {$IFDEF LINUX}
    if FileExists('/usr/bin/zenity') then
    begin
      RunCommand('/usr/bin/zenity',
        ['--entry', '--title=' + Caption, '--text=' + Prompt,
         '--entry-text=' + Default], Result);
    end
    else if FileExists('/usr/bin/kdialog') then
    begin
      RunCommand('/usr/bin/kdialog',
        ['--inputbox', Prompt, Default, '--title', Caption], Result);
    end
    else
    begin
      Write(Prompt, ' [', Default, ']: ');
      ReadLn(Result);
      if Result = '' then
        Result := Default;
    end;
  {$ENDIF}

  {$IFDEF DARWIN}
    RunCommand('/usr/bin/osascript',
      ['-e', 'text returned of (display dialog "' + Prompt +
       '" default answer "' + Default + '" with title "' + Caption + '")'],
      Result);
  {$ENDIF}
end;

end.
```

### Notification système

```pascal
unit SystemNotifications;

interface

procedure ShowNotification(const Title, Message: string);
procedure ShowTrayIcon(const Hint: string);
procedure PlaySystemSound(const SoundType: string);

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}, Windows, ShellAPI{$ENDIF}
  {$IFDEF LINUX}, Process{$ENDIF}
  {$IFDEF DARWIN}, MacOSAll{$ENDIF};

procedure ShowNotification(const Title, Message: string);
{$IFDEF WINDOWS}
var
  NotifyData: TNotifyIconData;
{$ENDIF}
{$IFDEF LINUX}
var
  s: string;
{$ENDIF}
{$IFDEF DARWIN}
var
  s: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    // Notification Windows via API système
    FillChar(NotifyData, SizeOf(NotifyData), 0);
    NotifyData.cbSize := SizeOf(NotifyData);
    NotifyData.uFlags := NIF_INFO;
    StrPCopy(NotifyData.szInfoTitle, Title);
    StrPCopy(NotifyData.szInfo, Message);
    NotifyData.dwInfoFlags := NIIF_INFO;
    Shell_NotifyIcon(NIM_MODIFY, @NotifyData);
  {$ENDIF}

  {$IFDEF LINUX}
    // Utiliser notify-send sur Linux
    if FileExists('/usr/bin/notify-send') then
    begin
      RunCommand('/usr/bin/notify-send', [Title, Message], s);
    end
    else
    begin
      WriteLn('[NOTIFICATION] ', Title, ': ', Message);
    end;
  {$ENDIF}

  {$IFDEF DARWIN}
    // Notification macOS via osascript
    RunCommand('/usr/bin/osascript',
      ['-e', 'display notification "' + Message +
       '" with title "' + Title + '"'], s);
  {$ENDIF}
end;

procedure ShowTrayIcon(const Hint: string);
{$IFDEF WINDOWS}
var
  NotifyData: TNotifyIconData;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    FillChar(NotifyData, SizeOf(NotifyData), 0);
    NotifyData.cbSize := SizeOf(NotifyData);
    NotifyData.hWnd := Application.MainForm.Handle;
    NotifyData.uID := 1;
    NotifyData.uFlags := NIF_ICON or NIF_TIP;
    NotifyData.hIcon := Application.Icon.Handle;
    StrPCopy(NotifyData.szTip, Hint);
    Shell_NotifyIcon(NIM_ADD, @NotifyData);
  {$ENDIF}

  {$IFDEF LINUX}
    // Linux : utiliser libappindicator ou systray
    WriteLn('[TRAY] ', Hint);
  {$ENDIF}

  {$IFDEF DARWIN}
    // macOS : nécessite des API spécifiques Cocoa
    WriteLn('[TRAY] ', Hint);
  {$ENDIF}
end;

procedure PlaySystemSound(const SoundType: string);
{$IFDEF LINUX}
var
  s: string;
{$ENDIF}
{$IFDEF DARWIN}
var
  s: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    if SoundType = 'error' then
      MessageBeep(MB_ICONERROR)
    else if SoundType = 'warning' then
      MessageBeep(MB_ICONWARNING)
    else if SoundType = 'info' then
      MessageBeep(MB_ICONINFORMATION)
    else
      MessageBeep(MB_OK);
  {$ENDIF}

  {$IFDEF LINUX}
    // Jouer un son avec paplay ou aplay
    if FileExists('/usr/bin/paplay') then
    begin
      if SoundType = 'error' then
        RunCommand('/usr/bin/paplay',
          ['/usr/share/sounds/freedesktop/stereo/dialog-error.oga'], s)
      else if SoundType = 'warning' then
        RunCommand('/usr/bin/paplay',
          ['/usr/share/sounds/freedesktop/stereo/dialog-warning.oga'], s)
      else
        RunCommand('/usr/bin/paplay',
          ['/usr/share/sounds/freedesktop/stereo/dialog-information.oga'], s);
    end
    else if FileExists('/usr/bin/aplay') then
    begin
      // Utiliser aplay avec des fichiers WAV système
      RunCommand('/usr/bin/aplay',
        ['/usr/share/sounds/alsa/Front_Center.wav'], s);
    end;
  {$ENDIF}

  {$IFDEF DARWIN}
    // Son système macOS
    if SoundType = 'error' then
      RunCommand('/usr/bin/afplay',
        ['/System/Library/Sounds/Basso.aiff'], s)
    else if SoundType = 'warning' then
      RunCommand('/usr/bin/afplay',
        ['/System/Library/Sounds/Hero.aiff'], s)
    else
      RunCommand('/usr/bin/afplay',
        ['/System/Library/Sounds/Glass.aiff'], s);
  {$ENDIF}
end;

end.
```

## 8. Gestion des processus et services {#processus}

### Lancement de processus externes

```pascal
unit ProcessManagement;

interface

type
  TProcessInfo = record
    PID: Integer;
    Name: string;
    Running: Boolean;
  end;

  TProcessInfoArray = array of TProcessInfo;

function ExecuteProcess(const Command: string;
                        const Args: array of string;
                        WaitForExit: Boolean = True): Integer;
function GetProcessList: TProcessInfoArray;
function IsProcessRunning(const ProcessName: string): Boolean;
function KillProcess(PID: Integer): Boolean;
function GetCurrentProcessID: Integer;

implementation

uses
  SysUtils, Classes, Generics.Collections  // Nécessite : uses Generics.Collections pour TList<>
  {$IFDEF WINDOWS}, Windows, TlHelp32{$ENDIF}
  {$IFDEF UNIX}, Process, BaseUnix{$ENDIF};

function ExecuteProcess(const Command: string;
                        const Args: array of string;
                        WaitForExit: Boolean = True): Integer;
{$IFDEF WINDOWS}
var
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
  i: Integer;
{$ENDIF}
{$IFDEF UNIX}
var
  AProcess: TProcess;
  i: Integer;
{$ENDIF}
begin
  Result := -1;

  {$IFDEF WINDOWS}
    // Construction de la ligne de commande
    CmdLine := '"' + Command + '"';
    for i := Low(Args) to High(Args) do
      CmdLine := CmdLine + ' "' + Args[i] + '"';

    FillChar(StartInfo, SizeOf(StartInfo), 0);
    FillChar(ProcInfo, SizeOf(ProcInfo), 0);
    StartInfo.cb := SizeOf(StartInfo);

    if CreateProcess(nil, PChar(CmdLine), nil, nil, False,
                     NORMAL_PRIORITY_CLASS, nil, nil,
                     StartInfo, ProcInfo) then
    begin
      if WaitForExit then
      begin
        WaitForSingleObject(ProcInfo.hProcess, INFINITE);
        GetExitCodeProcess(ProcInfo.hProcess, DWORD(Result));
      end
      else
        Result := ProcInfo.dwProcessId;

      CloseHandle(ProcInfo.hProcess);
      CloseHandle(ProcInfo.hThread);
    end;
  {$ENDIF}

  {$IFDEF UNIX}
    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable := Command;
      for i := Low(Args) to High(Args) do
        AProcess.Parameters.Add(Args[i]);

      if WaitForExit then
      begin
        AProcess.Options := [poWaitOnExit];
        AProcess.Execute;
        Result := AProcess.ExitStatus;
      end
      else
      begin
        AProcess.Execute;
        Result := AProcess.ProcessID;
      end;
    finally
      AProcess.Free;
    end;
  {$ENDIF}
end;

function GetProcessList: TProcessInfoArray;
{$IFDEF WINDOWS}
var
  Snapshot: THandle;
  ProcessEntry: TProcessEntry32;
  List: specialize TList<TProcessInfo>;
  Info: TProcessInfo;
{$ENDIF}
{$IFDEF UNIX}
var
  Output: TStringList;
  OutputStr: string;
  Line: string;
  Info: TProcessInfo;
  List: specialize TList<TProcessInfo>;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    List := specialize TList<TProcessInfo>.Create;
    try
      Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
      if Snapshot <> INVALID_HANDLE_VALUE then
      begin
        ProcessEntry.dwSize := SizeOf(ProcessEntry);
        if Process32First(Snapshot, ProcessEntry) then
        begin
          repeat
            Info.PID := ProcessEntry.th32ProcessID;
            Info.Name := ProcessEntry.szExeFile;
            Info.Running := True;
            List.Add(Info);
          until not Process32Next(Snapshot, ProcessEntry);
        end;
        CloseHandle(Snapshot);
      end;
      Result := List.ToArray;
    finally
      List.Free;
    end;
  {$ENDIF}

  {$IFDEF UNIX}
    List := specialize TList<TProcessInfo>.Create;
    Output := TStringList.Create;
    try
      // Utiliser ps pour lister les processus
      RunCommand('/bin/ps', ['aux'], OutputStr);
      Output.Text := OutputStr;

      for Line in Output do
      begin
        // Parser la sortie de ps
        // Format : USER PID %CPU %MEM VSZ RSS TTY STAT START TIME COMMAND
        if Pos('PID', Line) = 0 then  // Ignorer l'en-tête
        begin
          Info.PID := StrToIntDef(ExtractWord(2, Line, [' ']), 0);
          Info.Name := ExtractWord(11, Line, [' ']);
          Info.Running := True;
          if Info.PID > 0 then
            List.Add(Info);
        end;
      end;

      Result := List.ToArray;
    finally
      Output.Free;
      List.Free;
    end;
  {$ENDIF}
end;

function IsProcessRunning(const ProcessName: string): Boolean;
var
  ProcessList: TProcessInfoArray;
  Process: TProcessInfo;
begin
  Result := False;
  ProcessList := GetProcessList;

  for Process in ProcessList do
  begin
    if Pos(LowerCase(ProcessName), LowerCase(Process.Name)) > 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function KillProcess(PID: Integer): Boolean;
{$IFDEF WINDOWS}
var
  ProcessHandle: THandle;
{$ENDIF}
begin
  Result := False;

  {$IFDEF WINDOWS}
    ProcessHandle := OpenProcess(PROCESS_TERMINATE, False, PID);
    if ProcessHandle <> 0 then
    begin
      Result := TerminateProcess(ProcessHandle, 0);
      CloseHandle(ProcessHandle);
    end;
  {$ENDIF}

  {$IFDEF UNIX}
    Result := FpKill(PID, SIGTERM) = 0;
    if not Result then
      Result := FpKill(PID, SIGKILL) = 0;  // Force kill si nécessaire
  {$ENDIF}
end;

function GetCurrentProcessID: Integer;
begin
  {$IFDEF WINDOWS}
    Result := GetCurrentProcessId;
  {$ENDIF}

  {$IFDEF UNIX}
    Result := FpGetPid;
  {$ENDIF}
end;

end.
```

### Gestion des services système

```pascal
unit ServiceManagement;

interface

type
  TServiceStatus = (ssStopped, ssStarting, ssRunning, ssStopping);

function InstallService(const ServiceName, DisplayName, ExePath: string): Boolean;
function UninstallService(const ServiceName: string): Boolean;
function StartService(const ServiceName: string): Boolean;
function StopService(const ServiceName: string): Boolean;
function GetServiceStatus(const ServiceName: string): TServiceStatus;

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}, Windows, WinSvc{$ENDIF}
  {$IFDEF LINUX}, Process{$ENDIF};

{$IFDEF WINDOWS}
function InstallService(const ServiceName, DisplayName, ExePath: string): Boolean;
var
  SCManager, Service: SC_HANDLE;
begin
  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CREATE_SERVICE);
  if SCManager <> 0 then
  begin
    Service := CreateService(SCManager, PChar(ServiceName), PChar(DisplayName),
                            SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS,
                            SERVICE_AUTO_START, SERVICE_ERROR_NORMAL,
                            PChar(ExePath), nil, nil, nil, nil, nil);
    Result := Service <> 0;
    if Service <> 0 then
      CloseServiceHandle(Service);
    CloseServiceHandle(SCManager);
  end;
end;

function UninstallService(const ServiceName: string): Boolean;
var
  SCManager, Service: SC_HANDLE;
begin
  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager <> 0 then
  begin
    Service := OpenService(SCManager, PChar(ServiceName), DELETE);
    if Service <> 0 then
    begin
      Result := DeleteService(Service);
      CloseServiceHandle(Service);
    end;
    CloseServiceHandle(SCManager);
  end;
end;
{$ENDIF}

{$IFDEF LINUX}
function InstallService(const ServiceName, DisplayName, ExePath: string): Boolean;
var
  ServiceFile: TStringList;
  ServicePath: string;
  Output: string;
begin
  // Créer un fichier de service systemd
  ServicePath := '/etc/systemd/system/' + ServiceName + '.service';
  ServiceFile := TStringList.Create;
  try
    ServiceFile.Add('[Unit]');
    ServiceFile.Add('Description=' + DisplayName);
    ServiceFile.Add('After=network.target');
    ServiceFile.Add('');
    ServiceFile.Add('[Service]');
    ServiceFile.Add('Type=simple');
    ServiceFile.Add('ExecStart=' + ExePath);
    ServiceFile.Add('Restart=always');
    ServiceFile.Add('RestartSec=10');
    ServiceFile.Add('');
    ServiceFile.Add('[Install]');
    ServiceFile.Add('WantedBy=multi-user.target');

    // Nécessite les droits root
    ServiceFile.SaveToFile(ServicePath);

    // Recharger systemd
    Result := RunCommand('/bin/systemctl', ['daemon-reload'], Output);

    // Activer le service
    if Result then
      Result := RunCommand('/bin/systemctl', ['enable', ServiceName], Output);
  finally
    ServiceFile.Free;
  end;
end;

function UninstallService(const ServiceName: string): Boolean;
var
  Output: string;
begin
  // Arrêter le service
  StopService(ServiceName);

  // Désactiver le service
  Result := RunCommand('/bin/systemctl', ['disable', ServiceName], Output);

  // Supprimer le fichier de service
  if Result then
    DeleteFile('/etc/systemd/system/' + ServiceName + '.service');

  // Recharger systemd
  RunCommand('/bin/systemctl', ['daemon-reload'], Output);
end;
{$ENDIF}

function StartService(const ServiceName: string): Boolean;
{$IFDEF WINDOWS}
var
  SCManager, Service: SC_HANDLE;
{$ENDIF}
{$IFDEF LINUX}
var
  Output: string;
{$ENDIF}
begin
  Result := False;

  {$IFDEF WINDOWS}
    SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
    if SCManager <> 0 then
    begin
      Service := OpenService(SCManager, PChar(ServiceName), SERVICE_START);
      if Service <> 0 then
      begin
        Result := WinSvc.StartService(Service, 0, nil);
        CloseServiceHandle(Service);
      end;
      CloseServiceHandle(SCManager);
    end;
  {$ENDIF}

  {$IFDEF LINUX}
    Result := RunCommand('/bin/systemctl', ['start', ServiceName], Output);
  {$ENDIF}
end;

function StopService(const ServiceName: string): Boolean;
{$IFDEF WINDOWS}
var
  SCManager, Service: SC_HANDLE;
  Status: SERVICE_STATUS;
{$ENDIF}
{$IFDEF LINUX}
var
  Output: string;
{$ENDIF}
begin
  Result := False;

  {$IFDEF WINDOWS}
    SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
    if SCManager <> 0 then
    begin
      Service := OpenService(SCManager, PChar(ServiceName), SERVICE_STOP);
      if Service <> 0 then
      begin
        Result := ControlService(Service, SERVICE_CONTROL_STOP, Status);
        CloseServiceHandle(Service);
      end;
      CloseServiceHandle(SCManager);
    end;
  {$ENDIF}

  {$IFDEF LINUX}
    Result := RunCommand('/bin/systemctl', ['stop', ServiceName], Output);
  {$ENDIF}
end;

function GetServiceStatus(const ServiceName: string): TServiceStatus;
{$IFDEF WINDOWS}
var
  SCManager, Service: SC_HANDLE;
  Status: SERVICE_STATUS;
{$ENDIF}
{$IFDEF LINUX}
var
  Output: string;
{$ENDIF}
begin
  Result := ssStopped;

  {$IFDEF WINDOWS}
    SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
    if SCManager <> 0 then
    begin
      Service := OpenService(SCManager, PChar(ServiceName), SERVICE_QUERY_STATUS);
      if Service <> 0 then
      begin
        if QueryServiceStatus(Service, Status) then
        begin
          case Status.dwCurrentState of
            SERVICE_STOPPED: Result := ssStopped;
            SERVICE_START_PENDING: Result := ssStarting;
            SERVICE_RUNNING: Result := ssRunning;
            SERVICE_STOP_PENDING: Result := ssStopping;
          end;
        end;
        CloseServiceHandle(Service);
      end;
      CloseServiceHandle(SCManager);
    end;
  {$ENDIF}

  {$IFDEF LINUX}
    if RunCommand('/bin/systemctl', ['is-active', ServiceName], Output) then
    begin
      if Pos('active', Output) > 0 then
        Result := ssRunning
      else if Pos('activating', Output) > 0 then
        Result := ssStarting
      else if Pos('deactivating', Output) > 0 then
        Result := ssStopping
      else
        Result := ssStopped;
    end;
  {$ENDIF}
end;

end.
```

## 9. Accès au système de fichiers {#fichiers}

### Gestion des permissions et attributs

```pascal
unit FileSystemAccess;

interface

type
  TFilePermissions = record
    {$IFDEF UNIX}
    Owner: record
      Read, Write, Execute: Boolean;
    end;
    Group: record
      Read, Write, Execute: Boolean;
    end;
    Other: record
      Read, Write, Execute: Boolean;
    end;
    {$ENDIF}
    {$IFDEF WINDOWS}
    ReadOnly: Boolean;
    Hidden: Boolean;
    System: Boolean;
    Archive: Boolean;
    {$ENDIF}
  end;

function GetFilePermissions(const FileName: string): TFilePermissions;
function SetFilePermissions(const FileName: string;
                           const Permissions: TFilePermissions): Boolean;
function IsFileHidden(const FileName: string): Boolean;
function SetFileHidden(const FileName: string; Hidden: Boolean): Boolean;
function GetFileOwner(const FileName: string): string;

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, BaseUnix, Users, Grp{$ENDIF};

function GetFilePermissions(const FileName: string): TFilePermissions;
{$IFDEF UNIX}
var
  StatInfo: Stat;
{$ENDIF}
{$IFDEF WINDOWS}
var
  Attrs: DWORD;
{$ENDIF}
begin
  FillChar(Result, SizeOf(Result), 0);

  {$IFDEF UNIX}
    if FpStat(FileName, StatInfo) = 0 then
    begin
      // Permissions du propriétaire
      Result.Owner.Read := (StatInfo.st_mode and S_IRUSR) <> 0;
      Result.Owner.Write := (StatInfo.st_mode and S_IWUSR) <> 0;
      Result.Owner.Execute := (StatInfo.st_mode and S_IXUSR) <> 0;

      // Permissions du groupe
      Result.Group.Read := (StatInfo.st_mode and S_IRGRP) <> 0;
      Result.Group.Write := (StatInfo.st_mode and S_IWGRP) <> 0;
      Result.Group.Execute := (StatInfo.st_mode and S_IXGRP) <> 0;

      // Permissions des autres
      Result.Other.Read := (StatInfo.st_mode and S_IROTH) <> 0;
      Result.Other.Write := (StatInfo.st_mode and S_IWOTH) <> 0;
      Result.Other.Execute := (StatInfo.st_mode and S_IXOTH) <> 0;
    end;
  {$ENDIF}

  {$IFDEF WINDOWS}
    Attrs := GetFileAttributes(PChar(FileName));
    if Attrs <> INVALID_FILE_ATTRIBUTES then
    begin
      Result.ReadOnly := (Attrs and FILE_ATTRIBUTE_READONLY) <> 0;
      Result.Hidden := (Attrs and FILE_ATTRIBUTE_HIDDEN) <> 0;
      Result.System := (Attrs and FILE_ATTRIBUTE_SYSTEM) <> 0;
      Result.Archive := (Attrs and FILE_ATTRIBUTE_ARCHIVE) <> 0;
    end;
  {$ENDIF}
end;

function SetFilePermissions(const FileName: string;
                           const Permissions: TFilePermissions): Boolean;
{$IFDEF UNIX}
var
  Mode: TMode;
{$ENDIF}
{$IFDEF WINDOWS}
var
  Attrs: DWORD;
{$ENDIF}
begin
  Result := False;

  {$IFDEF UNIX}
    Mode := 0;

    // Construire le mode Unix
    if Permissions.Owner.Read then Mode := Mode or S_IRUSR;
    if Permissions.Owner.Write then Mode := Mode or S_IWUSR;
    if Permissions.Owner.Execute then Mode := Mode or S_IXUSR;

    if Permissions.Group.Read then Mode := Mode or S_IRGRP;
    if Permissions.Group.Write then Mode := Mode or S_IWGRP;
    if Permissions.Group.Execute then Mode := Mode or S_IXGRP;

    if Permissions.Other.Read then Mode := Mode or S_IROTH;
    if Permissions.Other.Write then Mode := Mode or S_IWOTH;
    if Permissions.Other.Execute then Mode := Mode or S_IXOTH;

    Result := FpChmod(FileName, Mode) = 0;
  {$ENDIF}

  {$IFDEF WINDOWS}
    Attrs := 0;

    if Permissions.ReadOnly then Attrs := Attrs or FILE_ATTRIBUTE_READONLY;
    if Permissions.Hidden then Attrs := Attrs or FILE_ATTRIBUTE_HIDDEN;
    if Permissions.System then Attrs := Attrs or FILE_ATTRIBUTE_SYSTEM;
    if Permissions.Archive then Attrs := Attrs or FILE_ATTRIBUTE_ARCHIVE;

    if Attrs = 0 then
      Attrs := FILE_ATTRIBUTE_NORMAL;

    Result := SetFileAttributes(PChar(FileName), Attrs);
  {$ENDIF}
end;

function IsFileHidden(const FileName: string): Boolean;
{$IFDEF WINDOWS}
var
  Attrs: DWORD;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    Attrs := GetFileAttributes(PChar(FileName));
    Result := (Attrs <> INVALID_FILE_ATTRIBUTES) and
              ((Attrs and FILE_ATTRIBUTE_HIDDEN) <> 0);
  {$ENDIF}

  {$IFDEF UNIX}
    // Sous Unix, les fichiers cachés commencent par un point
    Result := (ExtractFileName(FileName) <> '') and
              (ExtractFileName(FileName)[1] = '.');
  {$ENDIF}
end;

function SetFileHidden(const FileName: string; Hidden: Boolean): Boolean;
{$IFDEF WINDOWS}
var
  Attrs: DWORD;
{$ENDIF}
{$IFDEF UNIX}
var
  NewName, OldName: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    Attrs := GetFileAttributes(PChar(FileName));
    if Attrs <> INVALID_FILE_ATTRIBUTES then
    begin
      if Hidden then
        Attrs := Attrs or FILE_ATTRIBUTE_HIDDEN
      else
        Attrs := Attrs and not FILE_ATTRIBUTE_HIDDEN;

      Result := SetFileAttributes(PChar(FileName), Attrs);
    end
    else
      Result := False;
  {$ENDIF}

  {$IFDEF UNIX}
    // Sous Unix, on ne peut pas vraiment cacher un fichier
    // On peut juste le renommer avec un point au début
    if Hidden and not IsFileHidden(FileName) then
    begin
      NewName := ExtractFilePath(FileName) + '.' + ExtractFileName(FileName);
      Result := RenameFile(FileName, NewName);
    end
    else if not Hidden and IsFileHidden(FileName) then
    begin
      OldName := ExtractFileName(FileName);
      if (Length(OldName) > 1) and (OldName[1] = '.') then
      begin
        NewName := ExtractFilePath(FileName) + Copy(OldName, 2, MaxInt);
        Result := RenameFile(FileName, NewName);
      end
      else
        Result := False;
    end
    else
      Result := True;
  {$ENDIF}
end;

function GetFileOwner(const FileName: string): string;
{$IFDEF UNIX}
var
  StatInfo: Stat;
  PwdEntry: PPasswd;
{$ENDIF}
{$IFDEF WINDOWS}
var
  SecurityDesc: PSECURITY_DESCRIPTOR;
  Owner: PSID;
  OwnerDefaulted: BOOL;
  Name: array[0..255] of Char;
  Domain: array[0..255] of Char;
  NameLen, DomainLen: DWORD;
  Use: SID_NAME_USE;
{$ENDIF}
begin
  Result := 'Unknown';

  {$IFDEF UNIX}
    if FpStat(FileName, StatInfo) = 0 then
    begin
      PwdEntry := GetPwUid(StatInfo.st_uid);
      if PwdEntry <> nil then
        Result := PwdEntry^.pw_name;
    end;
  {$ENDIF}

  {$IFDEF WINDOWS}
    if GetNamedSecurityInfo(PChar(FileName), SE_FILE_OBJECT,
                           OWNER_SECURITY_INFORMATION, @Owner, nil,
                           nil, nil, SecurityDesc) = ERROR_SUCCESS then
    begin
      NameLen := SizeOf(Name);
      DomainLen := SizeOf(Domain);

      if LookupAccountSid(nil, Owner, Name, NameLen,
                          Domain, DomainLen, Use) then
        Result := string(Domain) + '\' + string(Name);

      LocalFree(HLOCAL(SecurityDesc));
    end;
  {$ENDIF}
end;

end.
```

## 10. Réseau et communications {#reseau}

La gestion du réseau multi-plateforme en FreePascal peut s'appuyer sur les unités `Sockets`, `SSockets`, `fphttpclient` ou `Synapse`/`lNet` pour une abstraction plus complète. Les principales différences entre plateformes concernent l'initialisation de Winsock sur Windows (`WSAStartup`/`WSACleanup`) qui n'est pas nécessaire sur Unix, ainsi que les options de sockets et les chemins vers les certificats SSL.

## 11. Bibliothèques et liens dynamiques {#bibliotheques}

### Chargement dynamique de bibliothèques

```pascal
unit DynamicLibraries;

interface

type
  TLibHandle = {$IFDEF WINDOWS}HMODULE{$ELSE}Pointer{$ENDIF};

  TDynamicLibrary = class
  private
    FHandle: TLibHandle;
    FLibraryName: string;
    FLoaded: Boolean;
  public
    constructor Create(const LibraryName: string);
    destructor Destroy; override;

    function LoadLibrary: Boolean;
    procedure UnloadLibrary;
    function GetProcAddress(const ProcName: string): Pointer;

    property Loaded: Boolean read FLoaded;
    property LibraryName: string read FLibraryName;
  end;

function GetSystemLibraryPath: string;
function GetLibraryExtension: string;
function LibraryExists(const LibraryName: string): Boolean;
function FindLibrary(const LibraryName: string): string;

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, dl, dynlibs{$ENDIF};

function GetSystemLibraryPath: string;
begin
  {$IFDEF WINDOWS}
    // Windows : dossier système
    SetLength(Result, MAX_PATH);
    GetSystemDirectory(PChar(Result), MAX_PATH);
    Result := IncludeTrailingPathDelimiter(Trim(Result));
  {$ENDIF}

  {$IFDEF LINUX}
    // Linux : plusieurs chemins possibles
    if DirectoryExists('/usr/lib64') then
      Result := '/usr/lib64/'
    else if DirectoryExists('/usr/lib') then
      Result := '/usr/lib/'
    else
      Result := '/lib/';
  {$ENDIF}

  {$IFDEF DARWIN}
    // macOS : chemins système
    Result := '/usr/lib/';
  {$ENDIF}
end;

function GetLibraryExtension: string;
begin
  {$IFDEF WINDOWS}
    Result := '.dll';
  {$ENDIF}

  {$IFDEF LINUX}
    Result := '.so';
  {$ENDIF}

  {$IFDEF DARWIN}
    Result := '.dylib';
  {$ENDIF}

  {$IFDEF FREEBSD}
    Result := '.so';
  {$ENDIF}
end;

function LibraryExists(const LibraryName: string): Boolean;
var
  FullPath: string;
begin
  // Vérifier avec l'extension appropriée
  FullPath := LibraryName;
  if ExtractFileExt(FullPath) = '' then
    FullPath := FullPath + GetLibraryExtension;

  Result := FileExists(FullPath);

  // Si non trouvé, chercher dans le chemin système
  if not Result then
  begin
    FullPath := GetSystemLibraryPath + ExtractFileName(FullPath);
    Result := FileExists(FullPath);
  end;
end;

function FindLibrary(const LibraryName: string): string;
const
  {$IFDEF WINDOWS}
  SearchPaths: array[0..3] of string = (
    '',  // Répertoire courant
    'C:\Windows\System32\',
    'C:\Windows\SysWOW64\',
    'C:\Windows\'
  );
  {$ENDIF}

  {$IFDEF LINUX}
  SearchPaths: array[0..5] of string = (
    '',  // Répertoire courant
    '/usr/local/lib/',
    '/usr/lib/',
    '/usr/lib64/',
    '/lib/',
    '/lib64/'
  );
  {$ENDIF}

  {$IFDEF DARWIN}
  SearchPaths: array[0..4] of string = (
    '',  // Répertoire courant
    '/usr/local/lib/',
    '/usr/lib/',
    '/System/Library/Frameworks/',
    '/Library/Frameworks/'
  );
  {$ENDIF}
var
  Path: string;
  FullName: string;
begin
  Result := '';

  // Ajouter l'extension si nécessaire
  if Pos('.', LibraryName) = 0 then
    FullName := LibraryName + GetLibraryExtension
  else
    FullName := LibraryName;

  // Chercher dans les chemins standards
  for Path in SearchPaths do
  begin
    if FileExists(Path + FullName) then
    begin
      Result := Path + FullName;
      Exit;
    end;
  end;

  {$IFDEF LINUX}
  // Sur Linux, essayer aussi avec 'lib' préfixe
  if Pos('lib', FullName) <> 1 then
  begin
    FullName := 'lib' + FullName;
    for Path in SearchPaths do
    begin
      if FileExists(Path + FullName) then
      begin
        Result := Path + FullName;
        Exit;
      end;
    end;
  end;
  {$ENDIF}
end;

{ TDynamicLibrary }

constructor TDynamicLibrary.Create(const LibraryName: string);
begin
  inherited Create;
  FLibraryName := LibraryName;
  FHandle := 0;
  FLoaded := False;
end;

destructor TDynamicLibrary.Destroy;
begin
  if FLoaded then
    UnloadLibrary;
  inherited;
end;

function TDynamicLibrary.LoadLibrary: Boolean;
var
  LibPath: string;
begin
  if FLoaded then
  begin
    Result := True;
    Exit;
  end;

  // Trouver le chemin complet de la bibliothèque
  LibPath := FindLibrary(FLibraryName);
  if LibPath = '' then
    LibPath := FLibraryName;

  {$IFDEF WINDOWS}
    FHandle := Windows.LoadLibrary(PChar(LibPath));
    FLoaded := FHandle <> 0;
  {$ENDIF}

  {$IFDEF UNIX}
    FHandle := dlopen(PChar(LibPath), RTLD_LAZY);
    FLoaded := FHandle <> nil;
  {$ENDIF}

  Result := FLoaded;
end;

procedure TDynamicLibrary.UnloadLibrary;
begin
  if not FLoaded then
    Exit;

  {$IFDEF WINDOWS}
    FreeLibrary(FHandle);
  {$ENDIF}

  {$IFDEF UNIX}
    dlclose(FHandle);
  {$ENDIF}

  FHandle := 0;
  FLoaded := False;
end;

function TDynamicLibrary.GetProcAddress(const ProcName: string): Pointer;
begin
  Result := nil;
  if not FLoaded then
    Exit;

  {$IFDEF WINDOWS}
    Result := Windows.GetProcAddress(FHandle, PChar(ProcName));
  {$ENDIF}

  {$IFDEF UNIX}
    Result := dlsym(FHandle, PChar(ProcName));
  {$ENDIF}
end;

end.
```

### Exemple d'utilisation de bibliothèques dynamiques

```pascal
program TestDynamicLib;

uses
  SysUtils, DynamicLibraries;

type
  // Types de fonctions pour les bibliothèques mathématiques
  TSqrtFunc = function(x: Double): Double; cdecl;
  TCosFunc = function(x: Double): Double; cdecl;

var
  MathLib: TDynamicLibrary;
  MySqrt: TSqrtFunc;
  MyCos: TCosFunc;

begin
  WriteLn('=== Test de chargement dynamique de bibliothèque ===');

  // Créer l'objet bibliothèque
  {$IFDEF WINDOWS}
    MathLib := TDynamicLibrary.Create('msvcrt');  // Bibliothèque C runtime
  {$ENDIF}
  {$IFDEF LINUX}
    MathLib := TDynamicLibrary.Create('m');  // libm.so - bibliothèque mathématique
  {$ENDIF}
  {$IFDEF DARWIN}
    MathLib := TDynamicLibrary.Create('libm');  // Bibliothèque mathématique
  {$ENDIF}

  try
    // Charger la bibliothèque
    if MathLib.LoadLibrary then
    begin
      WriteLn('Bibliothèque chargée avec succès');

      // Obtenir les adresses des fonctions
      MySqrt := TSqrtFunc(MathLib.GetProcAddress('sqrt'));
      MyCos := TCosFunc(MathLib.GetProcAddress('cos'));

      if Assigned(MySqrt) then
        WriteLn('Racine carrée de 16 = ', MySqrt(16):0:2)
      else
        WriteLn('Fonction sqrt non trouvée');

      if Assigned(MyCos) then
        WriteLn('Cosinus de 0 = ', MyCos(0):0:2)
      else
        WriteLn('Fonction cos non trouvée');
    end
    else
      WriteLn('Impossible de charger la bibliothèque');

  finally
    MathLib.Free;
  end;

  WriteLn('Appuyez sur Entrée pour terminer...');
  ReadLn;
end.
```

## 12. Gestion des différences de comportement {#comportement}

### Gestion des fins de ligne

```pascal
unit LineEndingHandling;

interface

const
  // Note : La constante LineEnding est déjà définie dans l'unité System.
  // On définit ici SystemLineEndingSize comme information complémentaire.
  {$IFDEF WINDOWS}
    SystemLineEndingSize = 2;  // CRLF (#13#10)
  {$ENDIF}
  {$IFDEF UNIX}
    SystemLineEndingSize = 1;  // LF (#10)
  {$ENDIF}

function NormalizeLineEndings(const Text: string): string;
function ConvertToSystemLineEndings(const Text: string): string;
function ConvertToUnixLineEndings(const Text: string): string;
function ConvertToWindowsLineEndings(const Text: string): string;
function DetectLineEndingStyle(const Text: string): string;

implementation

uses
  SysUtils, StrUtils;

function DetectLineEndingStyle(const Text: string): string;
var
  HasCRLF, HasLF, HasCR: Boolean;
begin
  HasCRLF := Pos(#13#10, Text) > 0;
  HasLF := Pos(#10, Text) > 0;
  HasCR := Pos(#13, Text) > 0;

  if HasCRLF then
    Result := 'Windows (CRLF)'
  else if HasLF then
    Result := 'Unix (LF)'
  else if HasCR then
    Result := 'Mac Classic (CR)'
  else
    Result := 'No line endings detected';
end;

function NormalizeLineEndings(const Text: string): string;
begin
  // Convertir tout vers le format système actuel
  Result := ConvertToSystemLineEndings(Text);
end;

function ConvertToSystemLineEndings(const Text: string): string;
begin
  {$IFDEF WINDOWS}
    Result := ConvertToWindowsLineEndings(Text);
  {$ENDIF}
  {$IFDEF UNIX}
    Result := ConvertToUnixLineEndings(Text);
  {$ENDIF}
end;

function ConvertToUnixLineEndings(const Text: string): string;
begin
  // D'abord remplacer CRLF par LF
  Result := StringReplace(Text, #13#10, #10, [rfReplaceAll]);
  // Puis remplacer CR seul par LF
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function ConvertToWindowsLineEndings(const Text: string): string;
begin
  // D'abord convertir en Unix
  Result := ConvertToUnixLineEndings(Text);
  // Puis remplacer LF par CRLF
  Result := StringReplace(Result, #10, #13#10, [rfReplaceAll]);
end;

end.
```

### Gestion de la sensibilité à la casse

```pascal
unit CaseSensitivity;

interface

function CompareFilenames(const File1, File2: string): Integer;
function SameFilename(const File1, File2: string): Boolean;
function FindFileIgnoreCase(const Directory, FileName: string): string;
function NormalizeFilename(const FileName: string): string;

implementation

uses
  SysUtils, Classes;

function CompareFilenames(const File1, File2: string): Integer;
begin
  {$IFDEF WINDOWS}
    // Windows : insensible à la casse
    Result := CompareText(File1, File2);
  {$ENDIF}

  {$IFDEF UNIX}
    // Unix/Linux : sensible à la casse
    Result := CompareStr(File1, File2);
  {$ENDIF}
end;

function SameFilename(const File1, File2: string): Boolean;
begin
  Result := CompareFilenames(File1, File2) = 0;
end;

function FindFileIgnoreCase(const Directory, FileName: string): string;
var
  SearchRec: TSearchRec;
  SearchPath: string;
begin
  Result := '';

  {$IFDEF WINDOWS}
    // Windows : recherche directe (insensible à la casse)
    SearchPath := IncludeTrailingPathDelimiter(Directory) + FileName;
    if FileExists(SearchPath) then
      Result := SearchPath;
  {$ENDIF}

  {$IFDEF UNIX}
    // Unix/Linux : parcourir le répertoire
    SearchPath := IncludeTrailingPathDelimiter(Directory);
    if FindFirst(SearchPath + '*', faAnyFile, SearchRec) = 0 then
    begin
      try
        repeat
          if CompareText(SearchRec.Name, FileName) = 0 then
          begin
            Result := SearchPath + SearchRec.Name;
            Break;
          end;
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;
    end;
  {$ENDIF}
end;

function NormalizeFilename(const FileName: string): string;
begin
  Result := FileName;

  {$IFDEF WINDOWS}
    // Windows : normaliser les séparateurs
    Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
    // Pas de changement de casse
  {$ENDIF}

  {$IFDEF UNIX}
    // Unix/Linux : normaliser les séparateurs
    Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
    // La casse est importante, ne pas modifier
  {$ENDIF}
end;

end.
```

### Gestion des encodages de caractères

```pascal
unit CharacterEncoding;

interface

function GetSystemEncoding: string;
function ConvertToUTF8(const Text: string): string;
function ConvertFromUTF8(const Text: string): string;
function IsUTF8Valid(const Text: string): Boolean;
function GetBOMType(const Buffer: array of Byte): string;

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF};

function GetSystemEncoding: string;
{$IFDEF WINDOWS}
var
  CodePage: UINT;
{$ENDIF}
{$IFDEF UNIX}
var
  Lang: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    CodePage := GetACP;  // Get ANSI Code Page
    case CodePage of
      1252: Result := 'Windows-1252 (Western European)';
      1251: Result := 'Windows-1251 (Cyrillic)';
      1250: Result := 'Windows-1250 (Central European)';
      932:  Result := 'Shift-JIS (Japanese)';
      936:  Result := 'GBK (Simplified Chinese)';
      65001: Result := 'UTF-8';
    else
      Result := 'CP' + IntToStr(CodePage);
    end;
  {$ENDIF}

  {$IFDEF UNIX}
    // Unix/Linux : généralement UTF-8
    Lang := GetEnvironmentVariable('LANG');
    if Pos('UTF-8', UpperCase(Lang)) > 0 then
      Result := 'UTF-8'
    else if Pos('UTF8', UpperCase(Lang)) > 0 then
      Result := 'UTF-8'
    else if Lang <> '' then
      Result := Lang
    else
      Result := 'UTF-8 (assumed)';
  {$ENDIF}
end;

function ConvertToUTF8(const Text: string): string;
begin
  {$IFDEF FPC_HAS_CPSTRING}
    Result := UTF8Encode(Text);
  {$ELSE}
    Result := Text;  // Assume already UTF-8
  {$ENDIF}
end;

function ConvertFromUTF8(const Text: string): string;
begin
  {$IFDEF FPC_HAS_CPSTRING}
    Result := UTF8Decode(Text);
  {$ELSE}
    Result := Text;  // Assume already in system encoding
  {$ENDIF}
end;

function IsUTF8Valid(const Text: string): Boolean;
var
  i, Len: Integer;
  c: Byte;
  BytesToFollow: Integer;
begin
  Result := True;
  i := 1;
  Len := Length(Text);

  while i <= Len do
  begin
    c := Ord(Text[i]);

    // ASCII character (0-127)
    if c <= 127 then
    begin
      Inc(i);
      Continue;
    end;

    // Determine number of bytes in UTF-8 sequence
    if (c and $E0) = $C0 then
      BytesToFollow := 1
    else if (c and $F0) = $E0 then
      BytesToFollow := 2
    else if (c and $F8) = $F0 then
      BytesToFollow := 3
    else
    begin
      Result := False;  // Invalid UTF-8 start byte
      Exit;
    end;

    // Check following bytes
    Inc(i);
    while (BytesToFollow > 0) and (i <= Len) do
    begin
      c := Ord(Text[i]);
      if (c and $C0) <> $80 then
      begin
        Result := False;  // Invalid UTF-8 continuation byte
        Exit;
      end;
      Inc(i);
      Dec(BytesToFollow);
    end;

    if BytesToFollow > 0 then
    begin
      Result := False;  // Incomplete UTF-8 sequence
      Exit;
    end;
  end;
end;

function GetBOMType(const Buffer: array of Byte): string;
begin
  Result := 'No BOM';

  if Length(Buffer) >= 3 then
  begin
    // UTF-8 BOM
    if (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
    begin
      Result := 'UTF-8';
      Exit;
    end;
  end;

  if Length(Buffer) >= 2 then
  begin
    // UTF-16 LE BOM
    if (Buffer[0] = $FF) and (Buffer[1] = $FE) then
    begin
      Result := 'UTF-16 LE';
      Exit;
    end;

    // UTF-16 BE BOM
    if (Buffer[0] = $FE) and (Buffer[1] = $FF) then
    begin
      Result := 'UTF-16 BE';
      Exit;
    end;
  end;

  if Length(Buffer) >= 4 then
  begin
    // UTF-32 LE BOM
    if (Buffer[0] = $FF) and (Buffer[1] = $FE) and
       (Buffer[2] = $00) and (Buffer[3] = $00) then
    begin
      Result := 'UTF-32 LE';
      Exit;
    end;

    // UTF-32 BE BOM
    if (Buffer[0] = $00) and (Buffer[1] = $00) and
       (Buffer[2] = $FE) and (Buffer[3] = $FF) then
    begin
      Result := 'UTF-32 BE';
      Exit;
    end;
  end;
end;

end.
```

## 13. Organisation du code multi-plateforme {#organisation}

### Structure de projet recommandée

```pascal
// Structure de répertoires suggérée :
{
  MonProjet/
    ├── src/  
    │   ├── common/          // Code partagé  
    │   │   ├── utils.pas  
    │   │   └── types.pas  
    │   ├── platform/        // Code spécifique plateforme  
    │   │   ├── windows/  
    │   │   │   ├── winapi.pas  
    │   │   │   └── registry.pas  
    │   │   ├── linux/  
    │   │   │   ├── unixapi.pas  
    │   │   │   └── dbus.pas  
    │   │   └── darwin/  
    │   │       └── cocoa.pas  
    │   └── main.pas  
    ├── tests/  
    ├── docs/  
    └── build/  
        ├── win32/  
        ├── win64/  
        ├── linux_x64/  
        └── darwin_x64/
}
```

### Unité d'abstraction de plateforme

```pascal
unit PlatformAbstraction;

interface

uses
  Classes, SysUtils;

type
  // Interface commune pour toutes les plateformes
  IPlatformService = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function GetPlatformName: string;
    function GetUserDataPath: string;
    function GetConfigPath: string;
    function GetTempPath: string;
    function ExecuteCommand(const Cmd: string; const Args: array of string): Integer;
    function ShowMessage(const Msg: string): Boolean;
    function GetSystemInfo: string;
  end;

  // Classe de base abstraite
  TPlatformService = class(TInterfacedObject, IPlatformService)
  protected
    function GetPlatformName: string; virtual; abstract;
    function GetUserDataPath: string; virtual; abstract;
    function GetConfigPath: string; virtual; abstract;
    function GetTempPath: string; virtual; abstract;
    function ExecuteCommand(const Cmd: string; const Args: array of string): Integer; virtual; abstract;
    function ShowMessage(const Msg: string): Boolean; virtual; abstract;
    function GetSystemInfo: string; virtual; abstract;
  end;

  {$IFDEF WINDOWS}
  TWindowsPlatformService = class(TPlatformService)
  protected
    function GetPlatformName: string; override;
    function GetUserDataPath: string; override;
    function GetConfigPath: string; override;
    function GetTempPath: string; override;
    function ExecuteCommand(const Cmd: string; const Args: array of string): Integer; override;
    function ShowMessage(const Msg: string): Boolean; override;
    function GetSystemInfo: string; override;
  end;
  {$ENDIF}

  {$IFDEF LINUX}
  TLinuxPlatformService = class(TPlatformService)
  protected
    function GetPlatformName: string; override;
    function GetUserDataPath: string; override;
    function GetConfigPath: string; override;
    function GetTempPath: string; override;
    function ExecuteCommand(const Cmd: string; const Args: array of string): Integer; override;
    function ShowMessage(const Msg: string): Boolean; override;
    function GetSystemInfo: string; override;
  end;
  {$ENDIF}

  {$IFDEF DARWIN}
  TMacOSPlatformService = class(TPlatformService)
  protected
    function GetPlatformName: string; override;
    function GetUserDataPath: string; override;
    function GetConfigPath: string; override;
    function GetTempPath: string; override;
    function ExecuteCommand(const Cmd: string; const Args: array of string): Integer; override;
    function ShowMessage(const Msg: string): Boolean; override;
    function GetSystemInfo: string; override;
  end;
  {$ENDIF}

// Factory pour créer le service approprié
function CreatePlatformService: IPlatformService;

// Instance globale (singleton)
var
  PlatformService: IPlatformService;

implementation

uses
  {$IFDEF WINDOWS}Windows, ShellAPI{$ENDIF}
  {$IFDEF LINUX}Process, BaseUnix{$ENDIF}
  {$IFDEF DARWIN}MacOSAll{$ENDIF};

function CreatePlatformService: IPlatformService;
begin
  {$IFDEF WINDOWS}
    Result := TWindowsPlatformService.Create;
  {$ENDIF}

  {$IFDEF LINUX}
    Result := TLinuxPlatformService.Create;
  {$ENDIF}

  {$IFDEF DARWIN}
    Result := TMacOSPlatformService.Create;
  {$ENDIF}

  {$IFNDEF WINDOWS}{$IFNDEF LINUX}{$IFNDEF DARWIN}
    raise Exception.Create('Plateforme non supportée');
  {$ENDIF}{$ENDIF}{$ENDIF}
end;

{$IFDEF WINDOWS}
{ TWindowsPlatformService }

function TWindowsPlatformService.GetPlatformName: string;
begin
  Result := 'Windows';
end;

function TWindowsPlatformService.GetUserDataPath: string;
begin
  Result := GetEnvironmentVariable('LOCALAPPDATA');
  if Result = '' then
    Result := GetEnvironmentVariable('APPDATA');
end;

function TWindowsPlatformService.GetConfigPath: string;
begin
  Result := GetEnvironmentVariable('APPDATA');
end;

function TWindowsPlatformService.GetTempPath: string;
begin
  Result := GetEnvironmentVariable('TEMP');
  if Result = '' then
    Result := GetEnvironmentVariable('TMP');
end;

function TWindowsPlatformService.ExecuteCommand(const Cmd: string;
  const Args: array of string): Integer;
var
  CmdLine: string;
  i: Integer;
begin
  CmdLine := Cmd;
  for i := Low(Args) to High(Args) do
    CmdLine := CmdLine + ' ' + Args[i];

  Result := WinExec(PChar(CmdLine), SW_HIDE);
end;

function TWindowsPlatformService.ShowMessage(const Msg: string): Boolean;
begin
  MessageBox(0, PChar(Msg), 'Information', MB_OK);
  Result := True;
end;

function TWindowsPlatformService.GetSystemInfo: string;
var
  VersionInfo: TOSVersionInfo;
begin
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  if GetVersionEx(VersionInfo) then
    Result := Format('Windows %d.%d Build %d',
      [VersionInfo.dwMajorVersion, VersionInfo.dwMinorVersion,
       VersionInfo.dwBuildNumber])
  else
    Result := 'Windows (version unknown)';
end;
{$ENDIF}

{$IFDEF LINUX}
{ TLinuxPlatformService }

function TLinuxPlatformService.GetPlatformName: string;
begin
  Result := 'Linux';
end;

function TLinuxPlatformService.GetUserDataPath: string;
begin
  Result := GetEnvironmentVariable('XDG_DATA_HOME');
  if Result = '' then
    Result := GetEnvironmentVariable('HOME') + '/.local/share';
end;

function TLinuxPlatformService.GetConfigPath: string;
begin
  Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if Result = '' then
    Result := GetEnvironmentVariable('HOME') + '/.config';
end;

function TLinuxPlatformService.GetTempPath: string;
begin
  Result := '/tmp';
end;

function TLinuxPlatformService.ExecuteCommand(const Cmd: string;
  const Args: array of string): Integer;
var
  Process: TProcess;
  i: Integer;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := Cmd;
    for i := Low(Args) to High(Args) do
      Process.Parameters.Add(Args[i]);
    Process.Execute;
    Process.WaitOnExit;
    Result := Process.ExitStatus;
  finally
    Process.Free;
  end;
end;

function TLinuxPlatformService.ShowMessage(const Msg: string): Boolean;
var
  Output: string;
begin
  Result := RunCommand('/usr/bin/zenity', ['--info', '--text=' + Msg], Output);
  if not Result then
  begin
    WriteLn(Msg);
    Result := True;
  end;
end;

function TLinuxPlatformService.GetSystemInfo: string;
var
  Output: string;
begin
  if RunCommand('/bin/uname', ['-a'], Output) then
    Result := Trim(Output)
  else
    Result := 'Linux (unknown version)';
end;
{$ENDIF}

{$IFDEF DARWIN}
{ TMacOSPlatformService }

function TMacOSPlatformService.GetPlatformName: string;
begin
  Result := 'macOS';
end;

function TMacOSPlatformService.GetUserDataPath: string;
begin
  Result := GetEnvironmentVariable('HOME') + '/Library/Application Support';
end;

function TMacOSPlatformService.GetConfigPath: string;
begin
  Result := GetEnvironmentVariable('HOME') + '/Library/Preferences';
end;

function TMacOSPlatformService.GetTempPath: string;
begin
  Result := GetEnvironmentVariable('TMPDIR');
  if Result = '' then
    Result := '/tmp';
end;

function TMacOSPlatformService.ExecuteCommand(const Cmd: string;
  const Args: array of string): Integer;
var
  Output: string;
begin
  if RunCommand(Cmd, Args, Output) then
    Result := 0
  else
    Result := -1;
end;

function TMacOSPlatformService.ShowMessage(const Msg: string): Boolean;
var
  Output: string;
begin
  Result := RunCommand('/usr/bin/osascript',
    ['-e', 'display dialog "' + Msg + '"'], Output);
end;

function TMacOSPlatformService.GetSystemInfo: string;
var
  Output: string;
begin
  if RunCommand('/usr/bin/sw_vers', [], Output) then
    Result := Trim(Output)
  else
    Result := 'macOS (unknown version)';
end;
{$ENDIF}

initialization
  PlatformService := CreatePlatformService;

finalization
  PlatformService := nil;

end.
```

## 14. Techniques avancées {#techniques-avancees}

### Macros et génération de code conditionnelle

```pascal
unit AdvancedConditionals;

interface

// Définir des macros complexes
{$MACRO ON}

// Macro pour le niveau de log
{$IFDEF DEBUG}
  {$DEFINE LOG_LEVEL := 3}  // Verbose
{$ELSE}
  {$IFDEF RELEASE}
    {$DEFINE LOG_LEVEL := 1}  // Errors only
  {$ELSE}
    {$DEFINE LOG_LEVEL := 2}  // Warnings
  {$ENDIF}
{$ENDIF}

// Macro pour les fonctionnalités
{$DEFINE FEATURE_NETWORKING := TRUE}
{$DEFINE FEATURE_DATABASE := TRUE}
{$DEFINE FEATURE_ENCRYPTION := FALSE}

// Combinaisons de conditions
{$IF DEFINED(WINDOWS) AND DEFINED(CPU64)}
  {$DEFINE WIN64_BUILD}
{$ENDIF}

{$IF DEFINED(LINUX) AND (DEFINED(CPUARM) OR DEFINED(CPUAARCH64))}
  {$DEFINE LINUX_ARM}
{$ENDIF}

type
  TLogLevel = (llError = 1, llWarning = 2, llInfo = 3, llDebug = 4);

  TAdvancedLogger = class
  private
    FLogLevel: TLogLevel;
  public
    constructor Create;
    procedure Log(Level: TLogLevel; const Msg: string);

    {$IF DECLARED(LOG_LEVEL)}
    procedure LogDebug(const Msg: string); inline;
    {$ENDIF}
  end;

  // Types conditionnels basés sur la plateforme
  {$IFDEF WINDOWS}
  TSystemHandle = THandle;
  {$ENDIF}
  {$IFDEF UNIX}
  TSystemHandle = Integer;
  {$ENDIF}

  // Record avec champs conditionnels
  TSystemInfo = record
    OSName: string;
    OSVersion: string;
    {$IFDEF WINDOWS}
    WindowsBuild: Integer;
    IsWindowsServer: Boolean;
    {$ENDIF}
    {$IFDEF LINUX}
    KernelVersion: string;
    Distribution: string;
    {$ENDIF}
    {$IFDEF DARWIN}
    DarwinVersion: string;
    MacOSCodename: string;
    {$ENDIF}
  end;

implementation

constructor TAdvancedLogger.Create;
begin
  inherited;
  {$IF DECLARED(LOG_LEVEL)}
    FLogLevel := TLogLevel(LOG_LEVEL);
  {$ELSE}
    FLogLevel := llError;
  {$ENDIF}
end;

procedure TAdvancedLogger.Log(Level: TLogLevel; const Msg: string);
begin
  if Level <= FLogLevel then
  begin
    case Level of
      llError:   Write('[ERROR] ');
      llWarning: Write('[WARN]  ');
      llInfo:    Write('[INFO]  ');
      llDebug:   Write('[DEBUG] ');
    end;
    WriteLn(Msg);
  end;
end;

{$IF DECLARED(LOG_LEVEL)}
procedure TAdvancedLogger.LogDebug(const Msg: string);
begin
  {$IF LOG_LEVEL >= 3}
    Log(llDebug, Msg);
  {$ELSE}
    // Ne rien faire en mode release
  {$ENDIF}
end;
{$ENDIF}

end.
```

### Inclusion conditionnelle de fichiers

```pascal
unit ConditionalIncludes;

interface

// Inclure différents fichiers selon la plateforme
{$IFDEF WINDOWS}
  {$I windows_constants.inc}
  {$I windows_types.inc}
{$ENDIF}

{$IFDEF LINUX}
  {$I linux_constants.inc}
  {$I linux_types.inc}
{$ENDIF}

{$IFDEF DARWIN}
  {$I darwin_constants.inc}
  {$I darwin_types.inc}
{$ENDIF}

// Fichier commun
{$I common_definitions.inc}

// Inclusion conditionnelle basée sur les fonctionnalités
{$IFDEF FEATURE_NETWORKING}
  {$I networking.inc}
{$ENDIF}

{$IFDEF FEATURE_DATABASE}
  {$I database.inc}
{$ENDIF}

implementation

// Inclure l'implémentation appropriée
{$IFDEF WINDOWS}
  {$I windows_implementation.inc}
{$ELSEIF DEFINED(LINUX)}
  {$I linux_implementation.inc}
{$ELSEIF DEFINED(DARWIN)}
  {$I darwin_implementation.inc}
{$ELSE}
  {$MESSAGE ERROR 'Plateforme non supportée'}
{$ENDIF}

end.
```

### Compilation conditionnelle avec messages

```pascal
unit CompilerMessages;

interface

// Messages au compilateur
{$IFDEF DEBUG}
  {$MESSAGE HINT 'Compilation en mode DEBUG'}
{$ENDIF}

{$IFDEF DEPRECATED_CODE}
  {$MESSAGE WARN 'Code déprécié inclus dans la compilation'}
{$ENDIF}

{$IFNDEF TESTED}
  {$MESSAGE WARN 'Ce code n''a pas été testé sur cette plateforme'}
{$ENDIF}

// Vérification de version
{$IF FPC_FULLVERSION < 30200}
  {$MESSAGE ERROR 'FreePascal 3.2.0 ou supérieur requis'}
{$ENDIF}

// Avertissements pour combinaisons non testées
{$IF DEFINED(WIN64) AND DEFINED(FEATURE_EXPERIMENTAL)}
  {$MESSAGE WARN 'Fonctionnalités expérimentales non testées sur Win64'}
{$ENDIF}

// Assertions de compilation
{$IF SIZEOF(Pointer) = 4}
  {$MESSAGE HINT 'Compilation 32 bits détectée'}
{$ELSEIF SIZEOF(Pointer) = 8}
  {$MESSAGE HINT 'Compilation 64 bits détectée'}
{$ELSE}
  {$MESSAGE ERROR 'Architecture non supportée'}
{$ENDIF}

implementation

end.
```

## 15. Débogage et tests multi-plateformes {#debogage}

### Outils de débogage conditionnels

```pascal
unit CrossPlatformDebug;

interface

procedure DebugOutput(const Msg: string);
procedure DebugBreak;
procedure AssertWithMessage(Condition: Boolean; const Msg: string);
procedure DumpMemory(P: Pointer; Size: Integer);
procedure TraceCall(const ProcName: string);
procedure CheckHeap;

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF LINUX}, BaseUnix{$ENDIF};

{$IFDEF DEBUG}
var
  TraceIndent: Integer = 0;
{$ENDIF}

procedure DebugOutput(const Msg: string);
{$IFDEF LOG_TO_FILE}
var
  F: TextFile;
{$ENDIF}
begin
  {$IFDEF DEBUG}
    {$IFDEF WINDOWS}
      OutputDebugString(PChar(Msg));
    {$ENDIF}

    {$IFDEF LINUX}
      WriteLn(ErrOutput, Msg);
    {$ENDIF}

    {$IFDEF DARWIN}
      WriteLn(ErrOutput, Msg);
    {$ENDIF}

    // Aussi écrire dans un fichier de log
    {$IFDEF LOG_TO_FILE}
    AssignFile(F, 'debug.log');
    if FileExists('debug.log') then
      Append(F)
    else
      Rewrite(F);
    WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), ' ', Msg);
    CloseFile(F);
    {$ENDIF}
  {$ENDIF}
end;

procedure DebugBreak;
begin
  {$IFDEF DEBUG}
    {$IFDEF WINDOWS}
      Windows.DebugBreak;
    {$ENDIF}

    {$IFDEF LINUX}
      // Générer SIGTRAP pour le débogueur
      FpKill(FpGetPid, SIGTRAP);
    {$ENDIF}

    {$IFDEF DARWIN}
      // Utiliser __builtin_trap() sur macOS
      asm
        int 3
      end;
    {$ENDIF}
  {$ENDIF}
end;

procedure AssertWithMessage(Condition: Boolean; const Msg: string);
begin
  if not Condition then
  begin
    DebugOutput('ASSERTION FAILED: ' + Msg);
    {$IFDEF DEBUG}
      DebugBreak;
    {$ELSE}
      raise Exception.Create('Assertion failed: ' + Msg);
    {$ENDIF}
  end;
end;

procedure DumpMemory(P: Pointer; Size: Integer);
var
  i, j: Integer;
  Line: string;
  B: PByte;
begin
  {$IFDEF DEBUG}
  B := PByte(P);
  DebugOutput(Format('Memory dump at %p, size: %d bytes', [P, Size]));

  i := 0;
  while i < Size do
  begin
    Line := Format('%08X: ', [i]);

    // Hex values
    j := 0;
    while (j < 16) and (i + j < Size) do
    begin
      Line := Line + Format('%02X ', [B[i + j]]);
      Inc(j);
    end;

    // Padding
    while j < 16 do
    begin
      Line := Line + '   ';
      Inc(j);
    end;

    Line := Line + ' ';

    // ASCII representation
    j := 0;
    while (j < 16) and (i + j < Size) do
    begin
      if (B[i + j] >= 32) and (B[i + j] < 127) then
        Line := Line + Chr(B[i + j])
      else
        Line := Line + '.';
      Inc(j);
    end;

    DebugOutput(Line);
    Inc(i, 16);
  end;
  {$ENDIF}
end;

procedure TraceCall(const ProcName: string);
begin
  {$IFDEF TRACE_CALLS}
    DebugOutput(StringOfChar(' ', TraceIndent * 2) + '-> ' + ProcName);
    Inc(TraceIndent);
  {$ENDIF}
end;

procedure CheckHeap;
begin
  {$IFDEF HEAPTRC}
    {$IFDEF WINDOWS}
      if not HeapValidate(GetProcessHeap, 0, nil) then
        DebugOutput('HEAP CORRUPTION DETECTED!');
    {$ENDIF}

    {$IFDEF LINUX}
      // Utiliser les fonctions de vérification du heap
      DebugOutput('Heap check performed');
    {$ENDIF}
  {$ENDIF}
end;

end.
```

### Tests unitaires multi-plateformes

```pascal
unit CrossPlatformTests;

interface

uses
  FPCUnit, TestRegistry;

type
  TCrossPlatformTest = class(TTestCase)
  published
    procedure TestPathSeparator;
    procedure TestFilePermissions;
    procedure TestProcessExecution;
    procedure TestNetworking;
    procedure TestEncoding;
  end;

implementation

uses
  SysUtils, Classes;

procedure TCrossPlatformTest.TestPathSeparator;
var
  Path: string;
begin
  {$IFDEF WINDOWS}
    Path := 'C:\Users\Test\Documents';
    AssertEquals('Path separator', '\', PathDelim);
    AssertTrue('Valid Windows path', Pos(':\', Path) > 0);
  {$ENDIF}

  {$IFDEF UNIX}
    Path := '/home/test/documents';
    AssertEquals('Path separator', '/', PathDelim);
    AssertTrue('Valid Unix path', Path[1] = '/');
  {$ENDIF}
end;

procedure TCrossPlatformTest.TestFilePermissions;
var
  TestFile: string;
begin
  TestFile := GetTempDir + 'test' + IntToStr(Random(10000)) + '.txt';

  try
    // Créer un fichier test
    with TFileStream.Create(TestFile, fmCreate) do
    begin
      WriteAnsiString('Test');
      Free;
    end;

    {$IFDEF WINDOWS}
      // Tester les attributs Windows
      AssertTrue('File exists', FileExists(TestFile));
      FileSetAttr(TestFile, faReadOnly);
      AssertTrue('Read-only set', (FileGetAttr(TestFile) and faReadOnly) <> 0);
    {$ENDIF}

    {$IFDEF UNIX}
      // Tester les permissions Unix
      // Note : nécessite uses BaseUnix
      AssertTrue('File exists', FileExists(TestFile));
      FpChmod(TestFile, &644);  // rw-r--r--
    {$ENDIF}

  finally
    DeleteFile(TestFile);
  end;
end;

procedure TCrossPlatformTest.TestProcessExecution;
var
  Output: string;
  ExitCode: Integer;
begin
  {$IFDEF WINDOWS}
    ExitCode := ExecuteProcess('cmd.exe', ['/c', 'echo', 'test']);
    AssertEquals('Process executed', 0, ExitCode);
  {$ENDIF}

  {$IFDEF LINUX}
    ExitCode := ExecuteProcess('/bin/echo', ['test']);
    AssertEquals('Process executed', 0, ExitCode);
  {$ENDIF}

  {$IFDEF DARWIN}
    ExitCode := ExecuteProcess('/bin/echo', ['test']);
    AssertEquals('Process executed', 0, ExitCode);
  {$ENDIF}
end;

procedure TCrossPlatformTest.TestNetworking;
{$IFDEF WINDOWS}
var
  WSAData: TWSAData;
  Host: PHostEnt;
{$ENDIF}
{$IFDEF UNIX}
var
  Host: PHostEnt;
{$ENDIF}
begin
  // Tester la résolution DNS
  {$IFDEF WINDOWS}
    AssertEquals('Winsock init', 0, WSAStartup($0202, WSAData));
    try
      Host := gethostbyname('localhost');
      AssertNotNull('Localhost resolved', Host);
    finally
      WSACleanup;
    end;
  {$ENDIF}

  {$IFDEF UNIX}
    Host := gethostbyname('localhost');
    AssertNotNull('Localhost resolved', Host);
  {$ENDIF}
end;

procedure TCrossPlatformTest.TestEncoding;
var
  TestStr: string;
  UTF8Str: UTF8String;
  Decoded: UnicodeString;
begin
  TestStr := 'Test éàü';

  {$IFDEF FPC_HAS_CPSTRING}
    UTF8Str := UTF8Encode(TestStr);
    AssertTrue('UTF8 encoding', Length(UTF8Str) > 0);

    Decoded := UTF8Decode(UTF8Str);
    AssertEquals('Round trip', TestStr, string(Decoded));
  {$ENDIF}
end;

initialization
  RegisterTest(TCrossPlatformTest);

end.
```

## 16. Bonnes pratiques et pièges à éviter {#bonnes-pratiques}

### Bonnes pratiques

1. **Toujours utiliser des constantes pour les différences OS**
```pascal
// ✅ BON
const
  {$IFDEF WINDOWS}
  CONFIG_FILE = 'config.ini';
  {$ELSE}
  CONFIG_FILE = '.config';
  {$ENDIF}

// ❌ MAUVAIS
procedure LoadConfig;
begin
  {$IFDEF WINDOWS}
  LoadFromFile('config.ini');
  {$ELSE}
  LoadFromFile('.config');
  {$ENDIF}
end;
```

2. **Centraliser les différences dans des unités séparées**
```pascal
// ✅ BON : Unit PlatformSpecific.pas
unit PlatformSpecific;
interface
function GetConfigPath: string;
implementation
// Toute la logique conditionnelle ici
end.

// Utilisation simple dans le code principal
uses PlatformSpecific;
begin
  ConfigPath := GetConfigPath;  // Pas de IFDEF ici
end;
```

3. **Tester les fonctionnalités, pas l'OS**
```pascal
// ✅ BON
{$IFDEF HAS_REGISTRY}
  UseRegistry;
{$ELSE}
  UseConfigFile;
{$ENDIF}

// ❌ MAUVAIS
{$IFDEF WINDOWS}
  UseRegistry;  // Et si c'est Wine sur Linux?
{$ENDIF}
```

4. **Documenter les différences de comportement**
```pascal
{
  GetTempPath : Retourne le chemin temporaire
  - Windows : %TEMP% ou C:\Temp
  - Linux : /tmp
  - macOS : $TMPDIR ou /tmp
  Note : Le chemin retourné inclut toujours le séparateur final
}
function GetTempPath: string;
```

### Pièges courants à éviter

1. **Oublier de tester toutes les plateformes**
```pascal
// ❌ DANGER : Code non testé
{$IFDEF WINDOWS}
  DoWindowsStuff;
{$ENDIF}
{$IFDEF LINUX}
  DoLinuxStuff;
{$ENDIF}
// Oups! Et macOS? FreeBSD?
```

2. **Suppositions sur les chemins**
```pascal
// ❌ MAUVAIS
FileName := 'C:\Data\' + Name;  // Ne marche que sur Windows

// ✅ BON
FileName := GetDataPath + PathDelim + Name;
```

3. **Mélanger la logique métier et le code plateforme**
```pascal
// ❌ MAUVAIS
procedure CalculateTotal;
begin
  Total := Price * Quantity;
  {$IFDEF WINDOWS}
    SaveToRegistry(Total);
  {$ELSE}
    SaveToFile(Total);
  {$ENDIF}
end;

// ✅ BON
procedure CalculateTotal;
begin
  Total := Price * Quantity;
  SaveTotal(Total);  // Déléguer à une fonction spécifique
end;
```

4. **Négliger les différences de sensibilité à la casse**
```pascal
// ❌ PROBLÈME
if FileName = 'CONFIG.INI' then  // Windows OK, Linux KO

// ✅ SOLUTION
if CompareFilenames(FileName, 'CONFIG.INI') = 0 then
```

5. **Assumptions sur les fins de ligne**
```pascal
// ❌ MAUVAIS
Text := 'Ligne 1'#13#10'Ligne 2';  // CRLF Windows seulement

// ✅ BON
Text := 'Ligne 1' + LineEnding + 'Ligne 2';
```

### Checklist de validation multi-plateforme

```pascal
unit PlatformChecklist;

interface

procedure ValidatePlatformCode;

implementation

procedure ValidatePlatformCode;
var
  TestStr: string;
  {$IFDEF WINDOWS}
  WSAData: TWSAData;
  {$ENDIF}
begin
  // 1. Vérifier les chemins
  Assert(DirectoryExists(GetConfigPath), 'Config path must exist');
  Assert(DirectoryExists(GetTempPath), 'Temp path must exist');

  // 2. Vérifier les permissions
  {$IFDEF UNIX}
  Assert(GetEnvironmentVariable('USER') <> '', 'USER env var required');
  {$ENDIF}
  {$IFDEF WINDOWS}
  Assert(GetEnvironmentVariable('USERNAME') <> '', 'USERNAME required');
  {$ENDIF}

  // 3. Vérifier les encodages
  TestStr := 'Test éàü';
  Assert(Length(UTF8Encode(TestStr)) > Length(TestStr), 'UTF8 encoding');

  // 4. Vérifier les bibliothèques système
  {$IFDEF WINDOWS}
  Assert(LoadLibrary('kernel32.dll') <> 0, 'kernel32.dll required');
  {$ENDIF}
  {$IFDEF LINUX}
  Assert(FileExists('/lib/libc.so.6') or FileExists('/lib64/libc.so.6'),
         'libc required');
  {$ENDIF}

  // 5. Vérifier la pile réseau
  {$IFDEF WINDOWS}
  Assert(WSAStartup($0202, WSAData) = 0, 'Winsock required');
  WSACleanup;
  {$ENDIF}

  WriteLn('Platform validation passed');
end;

end.
```

## Exemple complet : Application multi-plateforme

### Programme principal avec gestion complète

```pascal
program CrossPlatformApp;

{$MODE OBJFPC}{$H+}

// Définitions globales selon les options de compilation
{$IFDEF DEBUG}
  {$ASSERTIONS ON}
  {$RANGECHECKS ON}
  {$OVERFLOWCHECKS ON}
{$ELSE}
  {$ASSERTIONS OFF}
  {$RANGECHECKS OFF}
  {$OVERFLOWCHECKS OFF}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,  // Nécessaire pour le threading sur Unix
  {$ENDIF}
  SysUtils, Classes,
  // Unités spécifiques plateforme
  {$IFDEF WINDOWS}
  Windows, Registry,
  {$ENDIF}
  {$IFDEF LINUX}
  BaseUnix, Unix,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}
  // Unités communes
  PlatformAbstraction,
  CrossPlatformDialogs,
  FileSystemAccess;

const
  APP_NAME = 'CrossPlatformDemo';
  APP_VERSION = '1.0.0';

  // Configuration spécifique OS
  {$IFDEF WINDOWS}
  CONFIG_SUBDIR = 'CrossPlatformDemo';
  LOG_EXTENSION = '.log';
  {$ENDIF}

  {$IFDEF UNIX}
  CONFIG_SUBDIR = '.crossplatformdemo';
  LOG_EXTENSION = '.log';
  {$ENDIF}

type
  TCrossPlatformApp = class
  private
    FConfigPath: string;
    FDataPath: string;
    FLogFile: TextFile;
    FPlatform: IPlatformService;

    procedure InitializePaths;
    procedure InitializeLogging;
    procedure LoadConfiguration;
    procedure SaveConfiguration;
    procedure ShowSystemInfo;
    procedure RunPlatformTests;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run;
  end;

{ TCrossPlatformApp }

constructor TCrossPlatformApp.Create;
begin
  inherited Create;

  // Créer le service plateforme
  FPlatform := CreatePlatformService;

  // Initialiser les chemins
  InitializePaths;

  // Initialiser le logging
  InitializeLogging;

  // Charger la configuration
  LoadConfiguration;
end;

destructor TCrossPlatformApp.Destroy;
begin
  SaveConfiguration;

  {$I-}
  CloseFile(FLogFile);
  {$I+}

  FPlatform := nil;
  inherited;
end;

procedure TCrossPlatformApp.InitializePaths;
begin
  // Obtenir les chemins selon l'OS
  {$IFDEF WINDOWS}
    FConfigPath := GetEnvironmentVariable('APPDATA');
    if FConfigPath = '' then
      FConfigPath := GetEnvironmentVariable('USERPROFILE');
    FConfigPath := FConfigPath + '\' + CONFIG_SUBDIR;

    FDataPath := GetEnvironmentVariable('LOCALAPPDATA');
    if FDataPath = '' then
      FDataPath := FConfigPath;
    FDataPath := FDataPath + '\' + CONFIG_SUBDIR;
  {$ENDIF}

  {$IFDEF LINUX}
    FConfigPath := GetEnvironmentVariable('XDG_CONFIG_HOME');
    if FConfigPath = '' then
      FConfigPath := GetEnvironmentVariable('HOME') + '/.config';
    FConfigPath := FConfigPath + '/' + CONFIG_SUBDIR;

    FDataPath := GetEnvironmentVariable('XDG_DATA_HOME');
    if FDataPath = '' then
      FDataPath := GetEnvironmentVariable('HOME') + '/.local/share';
    FDataPath := FDataPath + '/' + CONFIG_SUBDIR;
  {$ENDIF}

  {$IFDEF DARWIN}
    FConfigPath := GetEnvironmentVariable('HOME') +
                   '/Library/Preferences/' + CONFIG_SUBDIR;
    FDataPath := GetEnvironmentVariable('HOME') +
                 '/Library/Application Support/' + CONFIG_SUBDIR;
  {$ENDIF}

  // Créer les répertoires s'ils n'existent pas
  if not DirectoryExists(FConfigPath) then
    ForceDirectories(FConfigPath);
  if not DirectoryExists(FDataPath) then
    ForceDirectories(FDataPath);
end;

procedure TCrossPlatformApp.InitializeLogging;
var
  LogFileName: string;
begin
  LogFileName := FDataPath + PathDelim +
                 FormatDateTime('yyyy-mm-dd', Now) + LOG_EXTENSION;

  AssignFile(FLogFile, LogFileName);
  {$I-}
  if FileExists(LogFileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
  {$I+}

  if IOResult = 0 then
  begin
    WriteLn(FLogFile, '=== Application started at ',
            FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' ===');
    WriteLn(FLogFile, 'Platform: ', FPlatform.GetPlatformName);
    WriteLn(FLogFile, 'Version: ', APP_VERSION);
    WriteLn(FLogFile, 'Config path: ', FConfigPath);
    WriteLn(FLogFile, 'Data path: ', FDataPath);
    Flush(FLogFile);
  end;
end;

procedure TCrossPlatformApp.LoadConfiguration;
var
  ConfigFile: string;
  Ini: TStringList;
begin
  ConfigFile := FConfigPath + PathDelim + 'settings.conf';

  if FileExists(ConfigFile) then
  begin
    Ini := TStringList.Create;
    try
      Ini.LoadFromFile(ConfigFile);
      WriteLn(FLogFile, 'Configuration loaded from ', ConfigFile);

      // Traiter la configuration selon l'OS
      {$IFDEF WINDOWS}
      // Les fins de ligne sont CRLF
      Ini.Text := ConvertToWindowsLineEndings(Ini.Text);
      {$ENDIF}

      {$IFDEF UNIX}
      // Les fins de ligne sont LF
      Ini.Text := ConvertToUnixLineEndings(Ini.Text);
      {$ENDIF}

    finally
      Ini.Free;
    end;
  end
  else
  begin
    WriteLn(FLogFile, 'No configuration file found, using defaults');
  end;
end;

procedure TCrossPlatformApp.SaveConfiguration;
var
  ConfigFile: string;
  Ini: TStringList;
begin
  ConfigFile := FConfigPath + PathDelim + 'settings.conf';

  Ini := TStringList.Create;
  try
    Ini.Add('# Configuration file for ' + APP_NAME);
    Ini.Add('# Generated on ' + DateTimeToStr(Now));
    Ini.Add('');
    Ini.Add('version=' + APP_VERSION);
    Ini.Add('platform=' + FPlatform.GetPlatformName);
    Ini.Add('last_run=' + DateTimeToStr(Now));

    // Sauvegarder avec les fins de ligne appropriées
    {$IFDEF WINDOWS}
    Ini.LineBreak := #13#10;
    {$ELSE}
    Ini.LineBreak := #10;
    {$ENDIF}

    Ini.SaveToFile(ConfigFile);
    WriteLn(FLogFile, 'Configuration saved to ', ConfigFile);

  finally
    Ini.Free;
  end;
end;

procedure TCrossPlatformApp.ShowSystemInfo;
begin
  WriteLn('=== System Information ===');
  WriteLn('Platform: ', FPlatform.GetPlatformName);
  WriteLn('System Info: ', FPlatform.GetSystemInfo);

  // Informations spécifiques à l'OS
  {$IFDEF WINDOWS}
    WriteLn('Windows Directory: ', GetEnvironmentVariable('WINDIR'));
    WriteLn('Program Files: ', GetEnvironmentVariable('PROGRAMFILES'));
    WriteLn('Processor: ', GetEnvironmentVariable('PROCESSOR_IDENTIFIER'));
  {$ENDIF}

  {$IFDEF LINUX}
    WriteLn('Distribution: ', GetEnvironmentVariable('DISTRIB_DESCRIPTION'));
    WriteLn('Desktop: ', GetEnvironmentVariable('XDG_CURRENT_DESKTOP'));
    WriteLn('Shell: ', GetEnvironmentVariable('SHELL'));
  {$ENDIF}

  {$IFDEF DARWIN}
    WriteLn('User: ', GetEnvironmentVariable('USER'));
    WriteLn('Shell: ', GetEnvironmentVariable('SHELL'));
  {$ENDIF}

  WriteLn('Home Directory: ', GetEnvironmentVariable('HOME'));
  if GetEnvironmentVariable('HOME') = '' then
    WriteLn('Home Directory: ', GetEnvironmentVariable('USERPROFILE'));

  WriteLn('Temp Directory: ', FPlatform.GetTempPath);
  WriteLn('Config Path: ', FConfigPath);
  WriteLn('Data Path: ', FDataPath);
  WriteLn;
end;

procedure TCrossPlatformApp.RunPlatformTests;
var
  TestFile: string;
  ExitCode: Integer;
begin
  WriteLn('=== Running Platform Tests ===');

  // Test 1 : Création de fichier
  Write('Testing file creation... ');
  TestFile := FPlatform.GetTempPath + 'test_' +
              IntToStr(Random(10000)) + '.txt';
  try
    with TFileStream.Create(TestFile, fmCreate) do
    begin
      WriteAnsiString('Test content');
      Free;
    end;

    if FileExists(TestFile) then
    begin
      WriteLn('OK');
      DeleteFile(TestFile);
    end
    else
      WriteLn('FAILED');
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;

  // Test 2 : Exécution de commande
  Write('Testing command execution... ');
  {$IFDEF WINDOWS}
  ExitCode := FPlatform.ExecuteCommand('cmd.exe', ['/c', 'echo test']);
  {$ELSE}
  ExitCode := FPlatform.ExecuteCommand('/bin/echo', ['test']);
  {$ENDIF}
  if ExitCode = 0 then
    WriteLn('OK')
  else
    WriteLn('FAILED (exit code: ', ExitCode, ')');

  // Test 3 : Boîte de dialogue
  Write('Testing message dialog... ');
  if FPlatform.ShowMessage('Test message from ' + APP_NAME) then
    WriteLn('OK')
  else
    WriteLn('FAILED');

  WriteLn;
end;

procedure TCrossPlatformApp.Run;
begin
  WriteLn('========================================');
  WriteLn('   ', APP_NAME, ' v', APP_VERSION);
  WriteLn('   Cross-Platform Demonstration');
  WriteLn('========================================');
  WriteLn;

  ShowSystemInfo;

  WriteLn('Press Enter to run platform tests...');
  ReadLn;

  RunPlatformTests;

  WriteLn('Application completed successfully.');
  WriteLn('Log file: ', FDataPath + PathDelim +
          FormatDateTime('yyyy-mm-dd', Now) + LOG_EXTENSION);
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end;

// Programme principal
var
  App: TCrossPlatformApp;
begin
  {$IFDEF DEBUG}
    WriteLn('[DEBUG MODE ENABLED]');
  {$ENDIF}

  Randomize;

  App := TCrossPlatformApp.Create;
  try
    App.Run;
  finally
    App.Free;
  end;
end.
```

## Conclusion

Ce guide complet sur les directives de compilation conditionnelle multi-OS vous donne tous les outils nécessaires pour créer des applications FreePascal/Lazarus vraiment portables.

### Points clés à retenir :

1. **Planifiez dès le départ** : Concevez votre architecture en pensant multi-plateforme
2. **Abstraire les différences** : Utilisez des interfaces et des classes pour isoler le code spécifique
3. **Testez sur toutes les plateformes** : Ne supposez jamais que le code fonctionnera ailleurs
4. **Documentez les spécificités** : Les futurs développeurs vous remercieront
5. **Utilisez les outils appropriés** : Profitez des capacités de FreePascal

Avec ces techniques, vous pouvez créer des applications qui fonctionnent de manière transparente sur Windows, Linux/Ubuntu, macOS et d'autres systèmes, tout en maintenant une base de code unique et maintenable.

⏭️ [Framework LCL](/04-framework-lcl/README.md)
