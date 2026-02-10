🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Compilation conditionnelle avancée en FreePascal/Lazarus
## Développement multi-plateforme Windows/Linux

## Introduction

Imaginez que vous écrivez une lettre qui doit être lue différemment selon qui la reçoit. Pour votre ami français, certains passages seraient en français, pour votre ami anglais, ces mêmes passages seraient en anglais. La compilation conditionnelle fonctionne exactement comme ça : elle permet d'inclure ou d'exclure des parties de code selon le système d'exploitation, la version du compilateur, ou d'autres conditions.

C'est l'outil le plus puissant pour créer un code source unique qui compile et fonctionne parfaitement sur Windows, Linux, et d'autres plateformes. Au lieu d'avoir plusieurs versions de votre programme, vous avez un seul code intelligent qui s'adapte automatiquement.

## Comprendre la compilation conditionnelle

### Le principe de base

La compilation conditionnelle permet au compilateur de :
- **Inclure** certaines parties de code seulement dans certaines conditions
- **Exclure** d'autres parties selon le contexte
- **Adapter** le comportement selon la plateforme, l'architecture, ou des options personnalisées

### Pourquoi est-ce essentiel ?

```pascal
// ❌ SANS compilation conditionnelle - Ce code ne compile que sur Windows
uses
  Windows, Registry;  // Unités Windows uniquement

procedure LireRegistre;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  // ... code Windows spécifique
end;

// ✅ AVEC compilation conditionnelle - Compile partout
uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows, Registry  // Inclus seulement sur Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix          // Inclus seulement sur Unix/Linux
  {$ENDIF}
  ;
```

## Les directives de base

### IFDEF - Si défini

La directive la plus courante vérifie si un symbole est défini :

```pascal
{$IFDEF WINDOWS}
  // Ce code n'est compilé que sur Windows
  ShowMessage('Vous êtes sur Windows');
{$ENDIF}

{$IFDEF LINUX}
  // Ce code n'est compilé que sur Linux
  WriteLn('Vous êtes sur Linux');
{$ENDIF}

{$IFDEF DARWIN}
  // Ce code n'est compilé que sur macOS
  WriteLn('Vous êtes sur macOS');
{$ENDIF}
```

### IFNDEF - Si non défini

L'inverse de IFDEF :

```pascal
{$IFNDEF WINDOWS}
  // Ce code est compilé sur TOUT sauf Windows
  WriteLn('Vous n''êtes pas sur Windows');
{$ENDIF}

{$IFNDEF FPC}
  // Code pour les compilateurs autres que FreePascal
  ShowMessage('Compilateur non-FreePascal détecté');
{$ENDIF}
```

### ELSE - Alternative

Permet de gérer le cas contraire :

```pascal
{$IFDEF WINDOWS}
  const PathSeparator = '\';
  const LineBreak = #13#10;
{$ELSE}
  const PathSeparator = '/';
  const LineBreak = #10;
{$ENDIF}
```

### ELSEIF - Conditions multiples

Pour tester plusieurs conditions en cascade :

```pascal
{$IFDEF WINDOWS}
  WriteLn('Système : Windows');
{$ELSEIF DEFINED(LINUX)}
  WriteLn('Système : Linux');
{$ELSEIF DEFINED(DARWIN)}
  WriteLn('Système : macOS');
{$ELSE}
  WriteLn('Système : Autre');
{$ENDIF}
```

## Symboles prédéfinis par FreePascal

### Symboles de plateforme

```pascal
program AfficherPlateforme;

begin
  Write('Plateforme détectée : ');

  {$IFDEF WINDOWS}
    WriteLn('Windows');
    {$IFDEF WIN32}
      WriteLn('  Architecture : 32 bits');
    {$ENDIF}
    {$IFDEF WIN64}
      WriteLn('  Architecture : 64 bits');
    {$ENDIF}
  {$ENDIF}

  {$IFDEF UNIX}
    WriteLn('Unix/Linux');
    {$IFDEF LINUX}
      WriteLn('  Distribution : Linux');
    {$ENDIF}
    {$IFDEF FREEBSD}
      WriteLn('  Distribution : FreeBSD');
    {$ENDIF}
    {$IFDEF DARWIN}
      WriteLn('  Distribution : macOS/Darwin');
    {$ENDIF}
  {$ENDIF}

  {$IFDEF ANDROID}
    WriteLn('Android');
  {$ENDIF}
end.
```

### Symboles d'architecture

```pascal
procedure AfficherArchitecture;
begin
  WriteLn('Architecture du processeur :');

  {$IFDEF CPU32}
    WriteLn('  32 bits');
  {$ENDIF}

  {$IFDEF CPU64}
    WriteLn('  64 bits');
  {$ENDIF}

  {$IFDEF CPUI386}
    WriteLn('  Intel x86');
  {$ENDIF}

  {$IFDEF CPUX86_64}
    WriteLn('  Intel/AMD x64');
  {$ENDIF}

  {$IFDEF CPUARM}
    WriteLn('  ARM');
    {$IFDEF CPUARM32}
      WriteLn('    ARM 32 bits');
    {$ENDIF}
    {$IFDEF CPUAARCH64}
      WriteLn('    ARM 64 bits');
    {$ENDIF}
  {$ENDIF}

  // Endianness
  {$IFDEF ENDIAN_LITTLE}
    WriteLn('  Little-endian');
  {$ENDIF}

  {$IFDEF ENDIAN_BIG}
    WriteLn('  Big-endian');
  {$ENDIF}
end;
```

### Symboles du compilateur

```pascal
procedure InfoCompilateur;
begin
  {$IFDEF FPC}
    WriteLn('Compilateur : FreePascal');
    WriteLn('Version : ', {$I %FPCVERSION%});

    // Version spécifique
    {$IF FPC_VERSION >= 3}
      WriteLn('FreePascal 3.0 ou supérieur');
    {$ENDIF}

    {$IF (FPC_VERSION = 3) and (FPC_RELEASE >= 2)}
      WriteLn('FreePascal 3.2.x ou supérieur');
    {$ENDIF}
  {$ENDIF}

  {$IFDEF VER3_2}
    WriteLn('Version exacte : 3.2.x');
  {$ENDIF}

  // Mode de compilation
  {$IFDEF DEBUG}
    WriteLn('Mode : DEBUG');
  {$ELSE}
    WriteLn('Mode : RELEASE');
  {$ENDIF}
end;
```

## Définir ses propres symboles

### Au niveau du code source

```pascal
// Définir un symbole
{$DEFINE MON_SYMBOLE}

// Utiliser le symbole
{$IFDEF MON_SYMBOLE}
  WriteLn('Mon symbole est défini');
{$ENDIF}

// Supprimer la définition
{$UNDEF MON_SYMBOLE}

{$IFDEF MON_SYMBOLE}
  WriteLn('Ce texte ne sera pas compilé');
{$ENDIF}
```

### Au niveau du projet

```pascal
// Définir des symboles pour tout le projet
{$DEFINE VERSION_PREMIUM}
{$DEFINE SUPPORT_MULTILANGUE}
{$DEFINE MAX_USERS_100}

type
  TApplication = class
  private
    {$IFDEF VERSION_PREMIUM}
    FFeaturesPremium: Boolean;
    {$ENDIF}

    {$IFDEF SUPPORT_MULTILANGUE}
    FLangue: string;
    {$ENDIF}
  public
    constructor Create;
    procedure Executer;
  end;

constructor TApplication.Create;
begin
  {$IFDEF VERSION_PREMIUM}
  FFeaturesPremium := True;
  WriteLn('Version Premium activée');
  {$ELSE}
  WriteLn('Version Standard');
  {$ENDIF}

  {$IFDEF SUPPORT_MULTILANGUE}
  FLangue := 'fr-FR';
  {$ENDIF}
end;
```

### Via les options de compilation

Dans Lazarus : Projet → Options du projet → Options du compilateur → Définitions personnalisées

Ou en ligne de commande :
```bash
fpc -dDEBUG -dVERSION_PREMIUM monprogramme.pas
```

## Techniques avancées

### Conditions complexes avec IF

```pascal
{$IF DEFINED(WINDOWS) and DEFINED(CPU64)}
  // Code spécifique Windows 64 bits
  WriteLn('Windows 64 bits détecté');
{$ELSEIF DEFINED(LINUX) and DEFINED(CPUARM)}
  // Code spécifique Linux ARM (Raspberry Pi par exemple)
  WriteLn('Linux ARM détecté (Raspberry Pi ?)');
{$ENDIF}

// Vérification de version
{$IF (FPC_VERSION > 3) or ((FPC_VERSION = 3) and (FPC_RELEASE >= 2))}
  // Code nécessitant FreePascal 3.2 ou supérieur
  WriteLn('Version FreePascal suffisante');
{$ELSE}
  {$ERROR Ce programme nécessite FreePascal 3.2 ou supérieur}
{$ENDIF}
```

### Macros et valeurs calculées

```pascal
{$MACRO ON}

// Définir des macros
{$DEFINE VERSION_MAJEURE := 1}
{$DEFINE VERSION_MINEURE := 5}
{$DEFINE VERSION_BUILD := 1234}

const
  VERSION_COMPLETE = IntToStr(VERSION_MAJEURE) + '.' +
                    IntToStr(VERSION_MINEURE) + '.' +
                    IntToStr(VERSION_BUILD);

// Utilisation conditionnelle basée sur les valeurs
{$IF VERSION_MAJEURE >= 2}
  // Fonctionnalités de la version 2.x
  {$DEFINE NOUVELLE_INTERFACE}
{$ENDIF}

{$IF (VERSION_MAJEURE = 1) and (VERSION_MINEURE < 5)}
  {$MESSAGE WARN 'Version obsolète, mise à jour recommandée'}
{$ENDIF}
```

### Inclusion conditionnelle de fichiers

```pascal
// Fichier de configuration selon la plateforme
{$IFDEF WINDOWS}
  {$I config_windows.inc}
{$ENDIF}

{$IFDEF LINUX}
  {$I config_linux.inc}
{$ENDIF}

{$IFDEF DARWIN}
  {$I config_macos.inc}
{$ENDIF}

// Inclusion avec test d'existence
{$I-} // Désactiver les erreurs I/O
{$I config_local.inc}
{$I+} // Réactiver les erreurs I/O

{$IF IOResult <> 0}
  // Le fichier n'existe pas, utiliser config par défaut
  {$I config_default.inc}
{$ENDIF}
```

## Gestion multi-plateforme des API système

### Abstraction des appels système

```pascal
unit SystemePortable;

interface

procedure ObtenirInfoSysteme(out OS, Architecture: string);
function ObtenirNomMachine: string;
function ObtenirNomUtilisateur: string;
function ObtenirRepertoireTemp: string;
procedure ExecuterCommande(const Commande: string);

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows, Registry
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix, BaseUnix
  {$ENDIF}
  ;

procedure ObtenirInfoSysteme(out OS, Architecture: string);
begin
  {$IFDEF WINDOWS}
    OS := 'Windows';
    {$IFDEF WIN32}
      Architecture := '32 bits';
    {$ELSE}
      Architecture := '64 bits';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
    OS := 'Linux';
    {$IFDEF CPU32}
      Architecture := '32 bits';
    {$ELSE}
      Architecture := '64 bits';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF DARWIN}
    OS := 'macOS';
    Architecture := '64 bits'; // macOS moderne est 64 bits
  {$ENDIF}
end;

function ObtenirNomMachine: string;
{$IFDEF WINDOWS}
var
  buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  taille: DWORD;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    taille := SizeOf(buffer);
    if GetComputerName(buffer, taille) then
      Result := StrPas(buffer)
    else
      Result := 'Unknown';
  {$ENDIF}

  {$IFDEF UNIX}
    Result := GetHostName;
  {$ENDIF}
end;

function ObtenirNomUtilisateur: string;
begin
  {$IFDEF WINDOWS}
    Result := GetEnvironmentVariable('USERNAME');
  {$ENDIF}

  {$IFDEF UNIX}
    Result := GetEnvironmentVariable('USER');
  {$ENDIF}

  if Result = '' then
    Result := 'Utilisateur';
end;

function ObtenirRepertoireTemp: string;
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

procedure ExecuterCommande(const Commande: string);
begin
  {$IFDEF WINDOWS}
    WinExec(PChar('cmd /c ' + Commande), SW_HIDE);
  {$ENDIF}

  {$IFDEF UNIX}
    fpSystem(PChar('/bin/sh -c "' + Commande + '"'));
  {$ENDIF}
end;

end.
```

### Gestion des processus

```pascal
unit ProcessusPortable;

interface

type
  TProcessInfo = record
    PID: Integer;
    Nom: string;
    Memoire: Int64;
  end;

function LancerProcessus(const Programme: string;
                        const Parametres: array of string): Boolean;
function TuerProcessus(PID: Integer): Boolean;
function ProcessusExiste(PID: Integer): Boolean;
function ListerProcessus: TArray<TProcessInfo>;

implementation

uses
  SysUtils, Classes, Process
  {$IFDEF WINDOWS}
  , Windows, TlHelp32
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix, BaseUnix
  {$ENDIF}
  ;

function LancerProcessus(const Programme: string;
                        const Parametres: array of string): Boolean;
var
  proc: TProcess;
  i: Integer;
begin
  Result := False;
  proc := TProcess.Create(nil);
  try
    proc.Executable := Programme;

    for i := 0 to High(Parametres) do
      proc.Parameters.Add(Parametres[i]);

    {$IFDEF WINDOWS}
    proc.ShowWindow := swoHide;
    proc.Options := proc.Options + [poNewConsole];
    {$ENDIF}

    {$IFDEF UNIX}
    proc.Options := proc.Options + [poNewProcessGroup];
    {$ENDIF}

    try
      proc.Execute;
      Result := proc.Running;
    except
      on E: Exception do
        WriteLn('Erreur lancement : ', E.Message);
    end;
  finally
    proc.Free;
  end;
end;

function TuerProcessus(PID: Integer): Boolean;
{$IFDEF WINDOWS}
var
  hProcess: THandle;
{$ENDIF}
begin
  Result := False;

  {$IFDEF WINDOWS}
  hProcess := OpenProcess(PROCESS_TERMINATE, False, PID);
  if hProcess <> 0 then
  begin
    Result := TerminateProcess(hProcess, 0);
    CloseHandle(hProcess);
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  Result := fpKill(PID, SIGTERM) = 0;
  if not Result then
    Result := fpKill(PID, SIGKILL) = 0; // Force si nécessaire
  {$ENDIF}
end;

function ProcessusExiste(PID: Integer): Boolean;
{$IFDEF WINDOWS}
var
  hProcess: THandle;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, PID);
  Result := hProcess <> 0;
  if Result then
    CloseHandle(hProcess);
  {$ENDIF}

  {$IFDEF UNIX}
  // Envoyer signal 0 pour tester l'existence
  Result := fpKill(PID, 0) = 0;
  {$ENDIF}
end;

function ListerProcessus: TArray<TProcessInfo>;
{$IFDEF WINDOWS}
var
  snapshot: THandle;
  processEntry: TProcessEntry32;
  liste: TList<TProcessInfo>;
  info: TProcessInfo;
{$ENDIF}
{$IFDEF UNIX}
var
  sr: TSearchRec;
  pid: Integer;
  statusFile: TextFile;
  ligne: string;
  liste: TList<TProcessInfo>;
  info: TProcessInfo;
{$ENDIF}
begin
  SetLength(Result, 0);

  {$IFDEF WINDOWS}
  liste := TList<TProcessInfo>.Create;
  try
    snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if snapshot <> INVALID_HANDLE_VALUE then
    begin
      try
        processEntry.dwSize := SizeOf(processEntry);
        if Process32First(snapshot, processEntry) then
        begin
          repeat
            info.PID := processEntry.th32ProcessID;
            info.Nom := StrPas(processEntry.szExeFile);
            info.Memoire := 0; // Nécessite APIs supplémentaires
            liste.Add(info);
          until not Process32Next(snapshot, processEntry);
        end;
      finally
        CloseHandle(snapshot);
      end;
    end;

    Result := liste.ToArray;
  finally
    liste.Free;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  liste := TList<TProcessInfo>.Create;
  try
    if FindFirst('/proc/[0-9]*', faDirectory, sr) = 0 then
    begin
      repeat
        if TryStrToInt(sr.Name, pid) then
        begin
          info.PID := pid;

          // Lire le nom du processus
          if FileExists('/proc/' + sr.Name + '/status') then
          begin
            AssignFile(statusFile, '/proc/' + sr.Name + '/status');
            Reset(statusFile);
            try
              ReadLn(statusFile, ligne);
              // Format: "Name:   processname"
              if Pos('Name:', ligne) = 1 then
              begin
                Delete(ligne, 1, 5);
                info.Nom := Trim(ligne);
              end;
            finally
              CloseFile(statusFile);
            end;
          end;

          info.Memoire := 0; // Nécessite parsing de /proc/[pid]/stat
          liste.Add(info);
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

    Result := liste.ToArray;
  finally
    liste.Free;
  end;
  {$ENDIF}
end;

end.
```

## Gestion des interfaces graphiques

### Adaptation de l'interface selon l'OS

```pascal
unit InterfacePortable;

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs;

type
  TFormPortable = class(TForm)
  private
    procedure AdapterInterface;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

constructor TFormPortable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AdapterInterface;
end;

procedure TFormPortable.AdapterInterface;
begin
  // Adaptation de la police
  {$IFDEF WINDOWS}
    Font.Name := 'Segoe UI';
    Font.Size := 9;
  {$ENDIF}

  {$IFDEF LINUX}
    Font.Name := 'Ubuntu';
    Font.Size := 10;
  {$ENDIF}

  {$IFDEF DARWIN}
    Font.Name := 'San Francisco';
    Font.Size := 13;
  {$ENDIF}

  // Adaptation des marges et espacements
  {$IFDEF WINDOWS}
    BorderStyle := bsSizeable;
    BorderIcons := [biSystemMenu, biMinimize, biMaximize];
  {$ENDIF}

  {$IFDEF LINUX}
    BorderStyle := bsSizeable;
    // Certains gestionnaires de fenêtres Linux ont des comportements différents
    {$IFDEF LCLGTK2}
      // Spécifique GTK2
    {$ENDIF}
    {$IFDEF LCLQT5}
      // Spécifique Qt5
    {$ENDIF}
  {$ENDIF}

  {$IFDEF DARWIN}
    BorderStyle := bsSizeable;
    // macOS a des conventions d'interface spécifiques
    Menu := nil; // Le menu est dans la barre de menu système
  {$ENDIF}

  // Boutons selon les conventions OS
  {$IFDEF WINDOWS}
    // Sur Windows : OK à gauche, Annuler à droite
  {$ENDIF}

  {$IFDEF DARWIN}
    // Sur macOS : Annuler à gauche, OK à droite
    // Inverser l'ordre des boutons si nécessaire
  {$ENDIF}
end;

end.
```

### Dialogues système natifs

```pascal
procedure OuvrirFichierNatif(out NomFichier: string);
var
  dialog: TOpenDialog;
begin
  dialog := TOpenDialog.Create(nil);
  try
    // Configuration commune
    dialog.Title := 'Sélectionner un fichier';
    dialog.Filter := 'Tous les fichiers (*.*)|*.*';

    // Adaptations spécifiques
    {$IFDEF WINDOWS}
    dialog.Filter := dialog.Filter + '|Fichiers texte (*.txt)|*.txt' +
                    '|Documents Word (*.docx)|*.docx';
    dialog.Options := dialog.Options + [ofFileMustExist, ofPathMustExist];
    {$ENDIF}

    {$IFDEF LINUX}
    dialog.Filter := dialog.Filter + '|Fichiers texte (*.txt)|*.txt' +
                    '|Documents LibreOffice (*.odt)|*.odt';
    // Sur Linux, possibilité d'afficher les fichiers cachés
    dialog.Options := dialog.Options + [ofShowHidden];
    {$ENDIF}

    {$IFDEF DARWIN}
    // macOS utilise des UTI (Uniform Type Identifiers)
    dialog.Filter := 'Documents|public.text;public.data';
    {$ENDIF}

    if dialog.Execute then
      NomFichier := dialog.FileName
    else
      NomFichier := '';
  finally
    dialog.Free;
  end;
end;
```

## Gestion des bibliothèques externes

### Chargement dynamique de DLL/SO

```pascal
unit BibliothequePortable;

interface

type
  TFonctionExterne = function(Param: Integer): Integer; cdecl;

function ChargerBibliotheque: Boolean;
procedure LibererBibliotheque;
function AppelerFonction(Valeur: Integer): Integer;

implementation

uses
  SysUtils, DynLibs;

var
  HandleBib: TLibHandle = 0;
  FonctionPtr: TFonctionExterne = nil;

function ChargerBibliotheque: Boolean;
const
  {$IFDEF WINDOWS}
  NOM_BIBLIOTHEQUE = 'malib.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  NOM_BIBLIOTHEQUE = 'libmalib.so';
  {$ENDIF}
  {$IFDEF DARWIN}
  NOM_BIBLIOTHEQUE = 'libmalib.dylib';
  {$ENDIF}
var
  cheminBib: string;
begin
  Result := False;

  // Chercher la bibliothèque dans différents emplacements
  {$IFDEF WINDOWS}
  // Windows cherche dans : répertoire exe, System32, PATH
  cheminBib := ExtractFilePath(ParamStr(0)) + NOM_BIBLIOTHEQUE;
  if not FileExists(cheminBib) then
    cheminBib := NOM_BIBLIOTHEQUE; // Laisser Windows chercher
  {$ENDIF}

  {$IFDEF LINUX}
  // Linux cherche dans : LD_LIBRARY_PATH, /usr/lib, /usr/local/lib
  cheminBib := './' + NOM_BIBLIOTHEQUE;
  if not FileExists(cheminBib) then
    cheminBib := '/usr/local/lib/' + NOM_BIBLIOTHEQUE;
  if not FileExists(cheminBib) then
    cheminBib := '/usr/lib/' + NOM_BIBLIOTHEQUE;
  if not FileExists(cheminBib) then
    cheminBib := NOM_BIBLIOTHEQUE; // Laisser le système chercher
  {$ENDIF}

  {$IFDEF DARWIN}
  // macOS cherche dans : DYLD_LIBRARY_PATH, /usr/local/lib, @rpath
  cheminBib := ExtractFilePath(ParamStr(0)) + NOM_BIBLIOTHEQUE;
  if not FileExists(cheminBib) then
    cheminBib := '/usr/local/lib/' + NOM_BIBLIOTHEQUE;
  if not FileExists(cheminBib) then
    cheminBib := NOM_BIBLIOTHEQUE;
  {$ENDIF}

  // Charger la bibliothèque
  HandleBib := LoadLibrary(cheminBib);

  if HandleBib <> 0 then
  begin
    // Obtenir l'adresse de la fonction
    FonctionPtr := TFonctionExterne(GetProcedureAddress(HandleBib, 'MaFonction'));

    {$IFDEF WINDOWS}
    // Sur Windows, essayer avec décoration de nom si échec
    if not Assigned(FonctionPtr) then
      FonctionPtr := TFonctionExterne(GetProcedureAddress(HandleBib, '_MaFonction@4'));
    {$ENDIF}

    Result := Assigned(FonctionPtr);

    if not Result then
    begin
      WriteLn('Erreur : Fonction non trouvée dans la bibliothèque');
      FreeLibrary(HandleBib);
      HandleBib := 0;
    end;
  end
  else
    WriteLn('Erreur chargement bibliothèque : ', GetLoadErrorStr);
end;

procedure LibererBibliotheque;
begin
  if HandleBib <> 0 then
  begin
    FreeLibrary(HandleBib);
    HandleBib := 0;
    FonctionPtr := nil;
  end;
end;

function AppelerFonction(Valeur: Integer): Integer;
begin
  if Assigned(FonctionPtr) then
    Result := FonctionPtr(Valeur)
  else
  begin
    WriteLn('Erreur : Bibliothèque non chargée');
    Result := -1;
  end;
end;

end.
```

## Messages et directives de compilation

### Messages informatifs

```pascal
{$MESSAGE 'Compilation pour ' + {$I %FPCTARGETCPU%}}
{$MESSAGE 'Système cible : ' + {$I %FPCTARGETOS%}}

{$IFDEF DEBUG}
  {$MESSAGE HINT 'Mode DEBUG activé - Performance réduite'}
{$ENDIF}

{$IFDEF RELEASE}
  {$MESSAGE 'Mode RELEASE - Optimisations activées'}
{$ENDIF}

// Avertissements
{$IF FPC_VERSION < 3}
  {$MESSAGE WARN 'Version de FPC obsolète, mise à jour recommandée'}
{$ENDIF}

// Erreurs
{$IFNDEF WINDOWS}
  {$IFNDEF LINUX}
    {$ERROR Plateforme non supportée}
  {$ENDIF}
{$ENDIF}
```

### Notes et TODO

```pascal
{$IFDEF EXPERIMENTAL}
  {$MESSAGE 'Fonctionnalités expérimentales activées'}
  {$NOTE 'À tester en production avant déploiement'}
{$ENDIF}

{$TODO 'Implémenter la gestion des erreurs pour Linux'}

{$IFDEF DEPRECATED_FEATURES}
  {$WARNING 'Utilisation de fonctionnalités obsolètes'}
{$ENDIF}
```

## Exemple complet : Application portable

```pascal
program ApplicationPortable;

{$MODE OBJFPC}{$H+}
{$MACRO ON}

// Configuration globale
{$DEFINE VERSION := '1.0.0'}
{$DEFINE APP_NAME := 'MonApp'}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils;

// Inclure les configurations spécifiques
{$IFDEF WINDOWS}
  {$I config_win.inc}
{$ENDIF}

{$IFDEF LINUX}
  {$I config_linux.inc}
{$ENDIF}

{$IFDEF DARWIN}
  {$I config_mac.inc}
{$ENDIF}

type
  TApplication = class
  private
    FPlateforme: string;
    FArchitecture: string;
    FVersionOS: string;
    FCheminDonnees: string;

    procedure InitialiserPlateforme;
    procedure ConfigurerChemins;
    procedure ChargerConfiguration;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Executer;
    procedure AfficherInfos;

    property Plateforme: string read FPlateforme;
    property Architecture: string read FArchitecture;
  end;

constructor TApplication.Create;
begin
  InitialiserPlateforme;
  ConfigurerChemins;
  ChargerConfiguration;

  WriteLn('=== ', APP_NAME, ' v', VERSION, ' ===');
  WriteLn('Initialisation terminée');
end;

destructor TApplication.Destroy;
begin
  WriteLn('Fermeture de l''application');
  inherited;
end;

procedure TApplication.InitialiserPlateforme;
begin
  // Détection de la plateforme
  {$IFDEF WINDOWS}
    FPlateforme := 'Windows';
    {$IFDEF WIN32}
      FArchitecture := '32-bit';
    {$ENDIF}
    {$IFDEF WIN64}
      FArchitecture := '64-bit';
    {$ENDIF}

    // Version Windows
    {$IFDEF WINDOWS7_UP}
      FVersionOS := '7 ou supérieur';
    {$ELSE}
      FVersionOS := 'XP/Vista';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
    FPlateforme := 'Linux';
    {$IFDEF CPU32}
      FArchitecture := '32-bit';
    {$ENDIF}
    {$IFDEF CPU64}
      FArchitecture := '64-bit';
    {$ENDIF}
    {$IFDEF CPUARM}
      FArchitecture := 'ARM';
    {$ENDIF}

    // Distribution Linux (approximative)
    if FileExists('/etc/debian_version') then
      FVersionOS := 'Debian/Ubuntu'
    else if FileExists('/etc/redhat-release') then
      FVersionOS := 'RedHat/Fedora'
    else if FileExists('/etc/arch-release') then
      FVersionOS := 'Arch Linux'
    else
      FVersionOS := 'Distribution inconnue';
  {$ENDIF}

  {$IFDEF DARWIN}
    FPlateforme := 'macOS';
    {$IFDEF CPUX86_64}
      FArchitecture := 'Intel 64-bit';
    {$ENDIF}
    {$IFDEF CPUAARCH64}
      FArchitecture := 'Apple Silicon (M1/M2)';
    {$ENDIF}
    FVersionOS := 'macOS';
  {$ENDIF}

  {$IFDEF BSD}
    FPlateforme := 'BSD';
    FArchitecture := 'Unix';
    FVersionOS := 'BSD variant';
  {$ENDIF}
end;

procedure TApplication.ConfigurerChemins;
var
  baseDir: string;
begin
  // Obtenir le répertoire de base selon l'OS
  {$IFDEF WINDOWS}
    // Windows : AppData
    baseDir := GetEnvironmentVariable('APPDATA');
    if baseDir = '' then
      baseDir := GetEnvironmentVariable('USERPROFILE') + '\AppData\Roaming';
    FCheminDonnees := baseDir + '\' + APP_NAME + '\';
  {$ENDIF}

  {$IFDEF UNIX}
    // Unix/Linux : dossier caché dans home
    baseDir := GetEnvironmentVariable('HOME');
    if baseDir = '' then
      baseDir := ExpandFileName('~');

    {$IFDEF DARWIN}
      // macOS : Library/Application Support
      FCheminDonnees := baseDir + '/Library/Application Support/' + APP_NAME + '/';
    {$ELSE}
      // Linux : .config
      if DirectoryExists(baseDir + '/.config') then
        FCheminDonnees := baseDir + '/.config/' + LowerCase(APP_NAME) + '/'
      else
        FCheminDonnees := baseDir + '/.' + LowerCase(APP_NAME) + '/';
    {$ENDIF}
  {$ENDIF}

  // Créer le répertoire si nécessaire
  if not DirectoryExists(FCheminDonnees) then
  begin
    {$IFDEF DEBUG}
    WriteLn('Création du répertoire : ', FCheminDonnees);
    {$ENDIF}
    ForceDirectories(FCheminDonnees);
  end;
end;

procedure TApplication.ChargerConfiguration;
var
  configFile: string;
  config: TStringList;
begin
  // Nom du fichier de configuration selon l'OS
  {$IFDEF WINDOWS}
    configFile := FCheminDonnees + 'config.ini';
  {$ELSE}
    configFile := FCheminDonnees + 'config.conf';
  {$ENDIF}

  if FileExists(configFile) then
  begin
    config := TStringList.Create;
    try
      config.LoadFromFile(configFile);

      {$IFDEF DEBUG}
      WriteLn('Configuration chargée : ', config.Count, ' ligne(s)');
      {$ENDIF}

      // Traiter la configuration
      // ...
    finally
      config.Free;
    end;
  end
  else
  begin
    {$IFDEF DEBUG}
    WriteLn('Aucune configuration trouvée, utilisation des valeurs par défaut');
    {$ENDIF}

    // Créer une configuration par défaut
    config := TStringList.Create;
    try
      config.Add('# Configuration ' + APP_NAME);
      config.Add('version=' + VERSION);
      config.Add('plateforme=' + FPlateforme);

      {$IFDEF WINDOWS}
      config.Add('# Configuration Windows');
      config.Add('use_registry=true');
      {$ENDIF}

      {$IFDEF UNIX}
      config.Add('# Configuration Unix');
      config.Add('use_dotfiles=true');
      {$ENDIF}

      config.SaveToFile(configFile);
    finally
      config.Free;
    end;
  end;
end;

procedure TApplication.AfficherInfos;
begin
  WriteLn('Informations système :');
  WriteLn('  Plateforme : ', FPlateforme);
  WriteLn('  Architecture : ', FArchitecture);
  WriteLn('  Version OS : ', FVersionOS);
  WriteLn('  Répertoire données : ', FCheminDonnees);

  // Informations de compilation
  WriteLn;
  WriteLn('Informations de compilation :');
  WriteLn('  Compilateur : FreePascal ', {$I %FPCVERSION%});
  WriteLn('  Date : ', {$I %DATE%}, ' ', {$I %TIME%});

  {$IFDEF DEBUG}
  WriteLn('  Mode : DEBUG');
  {$ELSE}
  WriteLn('  Mode : RELEASE');
  {$ENDIF}

  // Fonctionnalités activées
  WriteLn;
  WriteLn('Fonctionnalités :');

  {$IFDEF ENABLE_NETWORK}
  WriteLn('  ✓ Support réseau');
  {$ELSE}
  WriteLn('  ✗ Support réseau désactivé');
  {$ENDIF}

  {$IFDEF ENABLE_PLUGINS}
  WriteLn('  ✓ Support des plugins');
  {$ELSE}
  WriteLn('  ✗ Support des plugins désactivé');
  {$ENDIF}

  {$IFDEF ENABLE_SCRIPTING}
  WriteLn('  ✓ Support du scripting');
  {$ELSE}
  WriteLn('  ✗ Support du scripting désactivé');
  {$ENDIF}
end;

procedure TApplication.Executer;
begin
  WriteLn;
  WriteLn('Exécution de l''application...');

  // Code spécifique à la plateforme
  {$IFDEF WINDOWS}
    WriteLn('Initialisation des API Windows...');
    // Initialiser COM si nécessaire
    // CoInitialize(nil);
  {$ENDIF}

  {$IFDEF LINUX}
    WriteLn('Initialisation de l''environnement Linux...');
    // Configurer les signaux Unix
    // signal(SIGINT, @HandleSignal);
  {$ENDIF}

  {$IFDEF DARWIN}
    WriteLn('Initialisation de l''environnement macOS...');
    // Initialiser les frameworks macOS si nécessaire
  {$ENDIF}

  // Boucle principale (simplifiée)
  WriteLn('Application en cours d''exécution...');
  WriteLn('Appuyez sur Entrée pour quitter');
  ReadLn;

  // Nettoyage spécifique
  {$IFDEF WINDOWS}
    // CoUninitialize;
  {$ENDIF}
end;

// Programme principal
var
  app: TApplication;
begin
  {$IFDEF DEBUG}
  // En mode debug, afficher plus d'informations
  WriteLn('========================================');
  WriteLn('MODE DEBUG ACTIVÉ');
  WriteLn('========================================');
  {$ENDIF}

  app := TApplication.Create;
  try
    app.AfficherInfos;
    app.Executer;
  finally
    app.Free;
  end;
end.
```

## Fichiers d'inclusion (.inc)

### config_win.inc - Configuration Windows

```pascal
// config_win.inc - Configuration spécifique Windows

{$IFDEF WINDOWS}

// Définitions Windows
{$DEFINE USE_REGISTRY}
{$DEFINE ENABLE_COM}
{$DEFINE WINDOWS_INSTALLER}

// Versions Windows supportées
{$IFDEF WIN32_WINNT_WINXP}
  {$DEFINE WINXP_UP}
{$ENDIF}

{$IFDEF WIN32_WINNT_VISTA}
  {$DEFINE VISTA_UP}
{$ENDIF}

{$IFDEF WIN32_WINNT_WIN7}
  {$DEFINE WINDOWS7_UP}
{$ENDIF}

{$IFDEF WIN32_WINNT_WIN8}
  {$DEFINE WINDOWS8_UP}
{$ENDIF}

{$IFDEF WIN32_WINNT_WIN10}
  {$DEFINE WINDOWS10_UP}
{$ENDIF}

// Configuration des chemins Windows
const
  PATH_SEPARATOR = '\';
  LINE_ENDING = #13#10;
  EXEC_EXTENSION = '.exe';
  LIB_EXTENSION = '.dll';

// Messages Windows
{$MESSAGE 'Configuration Windows chargée'}

{$IFDEF CPU64}
  {$MESSAGE 'Windows 64-bit détecté'}
{$ELSE}
  {$MESSAGE 'Windows 32-bit détecté'}
{$ENDIF}

{$ENDIF}
```

### config_linux.inc - Configuration Linux

```pascal
// config_linux.inc - Configuration spécifique Linux

{$IFDEF LINUX}

// Définitions Linux
{$DEFINE USE_DOTFILES}
{$DEFINE ENABLE_DBUS}
{$DEFINE LINUX_PACKAGE}

// Détection du système d'init
{$DEFINE SYSTEMD}  // Par défaut pour les distributions modernes
// {$DEFINE SYSVINIT}  // Pour les anciennes distributions

// Configuration des chemins Linux
const
  PATH_SEPARATOR = '/';
  LINE_ENDING = #10;
  EXEC_EXTENSION = '';
  LIB_EXTENSION = '.so';

// Détection du gestionnaire de paquets
{$IF FILEEXISTS('/usr/bin/apt-get')}
  {$DEFINE PACKAGE_MANAGER_APT}
  {$MESSAGE 'Gestionnaire de paquets : APT (Debian/Ubuntu)'}
{$ELSEIF FILEEXISTS('/usr/bin/yum')}
  {$DEFINE PACKAGE_MANAGER_YUM}
  {$MESSAGE 'Gestionnaire de paquets : YUM (RedHat/Fedora)'}
{$ELSEIF FILEEXISTS('/usr/bin/pacman')}
  {$DEFINE PACKAGE_MANAGER_PACMAN}
  {$MESSAGE 'Gestionnaire de paquets : Pacman (Arch)'}
{$ENDIF}

// Messages Linux
{$MESSAGE 'Configuration Linux chargée'}

{$IFDEF CPUARM}
  {$MESSAGE 'Linux ARM détecté (Raspberry Pi?)'}
{$ENDIF}

{$ENDIF}
```

### config_mac.inc - Configuration macOS

```pascal
// config_mac.inc - Configuration spécifique macOS

{$IFDEF DARWIN}

// Définitions macOS
{$DEFINE USE_FRAMEWORKS}
{$DEFINE ENABLE_SANDBOX}
{$DEFINE MACOS_BUNDLE}

// Configuration des chemins macOS
const
  PATH_SEPARATOR = '/';
  LINE_ENDING = #10;
  EXEC_EXTENSION = '';
  LIB_EXTENSION = '.dylib';

// Architecture
{$IFDEF CPUX86_64}
  {$DEFINE INTEL_MAC}
  {$MESSAGE 'macOS Intel détecté'}
{$ENDIF}

{$IFDEF CPUAARCH64}
  {$DEFINE APPLE_SILICON}
  {$MESSAGE 'macOS Apple Silicon (M1/M2) détecté'}
{$ENDIF}

// Messages macOS
{$MESSAGE 'Configuration macOS chargée'}

{$ENDIF}
```

## Patterns avancés de compilation conditionnelle

### Pattern 1 : Factory selon la plateforme

```pascal
unit FactoryPortable;

interface

type
  IService = interface
    procedure Executer;
    function ObtenirNom: string;
  end;

function CreerService: IService;

implementation

{$IFDEF WINDOWS}
type
  TServiceWindows = class(TInterfacedObject, IService)
  public
    procedure Executer;
    function ObtenirNom: string;
  end;

procedure TServiceWindows.Executer;
begin
  WriteLn('Exécution du service Windows');
  // Code spécifique Windows
end;

function TServiceWindows.ObtenirNom: string;
begin
  Result := 'Service Windows';
end;
{$ENDIF}

{$IFDEF LINUX}
type
  TServiceLinux = class(TInterfacedObject, IService)
  public
    procedure Executer;
    function ObtenirNom: string;
  end;

procedure TServiceLinux.Executer;
begin
  WriteLn('Exécution du service Linux');
  // Code spécifique Linux
end;

function TServiceLinux.ObtenirNom: string;
begin
  Result := 'Service Linux';
end;
{$ENDIF}

{$IFDEF DARWIN}
type
  TServiceMacOS = class(TInterfacedObject, IService)
  public
    procedure Executer;
    function ObtenirNom: string;
  end;

procedure TServiceMacOS.Executer;
begin
  WriteLn('Exécution du service macOS');
  // Code spécifique macOS
end;

function TServiceMacOS.ObtenirNom: string;
begin
  Result := 'Service macOS';
end;
{$ENDIF}

function CreerService: IService;
begin
  {$IFDEF WINDOWS}
    Result := TServiceWindows.Create;
  {$ENDIF}

  {$IFDEF LINUX}
    Result := TServiceLinux.Create;
  {$ENDIF}

  {$IFDEF DARWIN}
    Result := TServiceMacOS.Create;
  {$ENDIF}

  {$IFNDEF WINDOWS}
    {$IFNDEF LINUX}
      {$IFNDEF DARWIN}
        {$ERROR Plateforme non supportée pour le service}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

end.
```

### Pattern 2 : Configuration en cascade

```pascal
unit ConfigurationAvancee;

interface

const
  // Configuration par défaut
  DEFAULT_TIMEOUT = 30;
  DEFAULT_BUFFER_SIZE = 8192;
  DEFAULT_MAX_CONNECTIONS = 100;

  // Override selon la plateforme
  {$IFDEF WINDOWS}
    TIMEOUT = 60;  // Windows a besoin de plus de temps
    {$IFDEF DEBUG}
      BUFFER_SIZE = 4096;  // Plus petit en debug pour tester
    {$ELSE}
      BUFFER_SIZE = 16384;  // Plus grand en production
    {$ENDIF}
  {$ELSE}
    TIMEOUT = DEFAULT_TIMEOUT;
    BUFFER_SIZE = DEFAULT_BUFFER_SIZE;
  {$ENDIF}

  // Configuration selon l'architecture
  {$IFDEF CPU64}
    MAX_MEMORY = Int64(8) * 1024 * 1024 * 1024;  // 8 GB
    {$IFDEF CPUX86_64}
      USE_SSE = True;
      USE_AVX = True;
    {$ENDIF}
  {$ELSE}
    MAX_MEMORY = 2 * 1024 * 1024 * 1024;  // 2 GB pour 32-bit
    USE_SSE = False;
    USE_AVX = False;
  {$ENDIF}

  // Configuration selon les features
  {$IFDEF ENABLE_ENCRYPTION}
    USE_TLS = True;
    MIN_TLS_VERSION = '1.2';
    {$IFDEF ENABLE_STRONG_ENCRYPTION}
      MIN_TLS_VERSION = '1.3';
      USE_AES256 = True;
    {$ELSE}
      USE_AES256 = False;
    {$ENDIF}
  {$ELSE}
    USE_TLS = False;
    MIN_TLS_VERSION = '';
    USE_AES256 = False;
  {$ENDIF}

implementation

end.
```

### Pattern 3 : Gestion d'erreurs adaptative

```pascal
unit GestionErreursPortable;

interface

type
  EApplicationError = class(Exception)
  private
    FCodeErreur: Integer;
    FDetailsSysteme: string;
  public
    constructor Create(const Msg: string; CodeErreur: Integer = 0);

    property CodeErreur: Integer read FCodeErreur;
    property DetailsSysteme: string read FDetailsSysteme;
  end;

procedure GererErreurSysteme(E: Exception);
procedure LogErreur(const Message: string; Niveau: Integer = 0);

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF}
  ;

constructor EApplicationError.Create(const Msg: string; CodeErreur: Integer);
begin
  inherited Create(Msg);
  FCodeErreur := CodeErreur;

  {$IFDEF WINDOWS}
  // Obtenir le message d'erreur Windows
  if CodeErreur = 0 then
    FCodeErreur := GetLastError;

  var buffer: array[0..255] of Char;
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, FCodeErreur,
                0, buffer, SizeOf(buffer), nil);
  FDetailsSysteme := StrPas(buffer);
  {$ENDIF}

  {$IFDEF UNIX}
  // Obtenir le message d'erreur Unix
  if CodeErreur = 0 then
    FCodeErreur := fpGetErrno;
  FDetailsSysteme := SysErrorMessage(FCodeErreur);
  {$ENDIF}
end;

procedure GererErreurSysteme(E: Exception);
begin
  {$IFDEF DEBUG}
  // En debug, afficher toutes les infos
  WriteLn('ERREUR DEBUG : ', E.ClassName);
  WriteLn('  Message : ', E.Message);
  if E is EApplicationError then
  begin
    WriteLn('  Code : ', EApplicationError(E).CodeErreur);
    WriteLn('  Détails : ', EApplicationError(E).DetailsSysteme);
  end;
  WriteLn('  Stack trace :');
  // Afficher la pile d'appels si disponible
  {$ENDIF}

  {$IFDEF RELEASE}
  // En production, log dans un fichier
  LogErreur(E.Message, 3);
  {$ENDIF}

  {$IFDEF WINDOWS}
  // Sur Windows, possibilité d'utiliser le journal d'événements
  // ReportEventLog(E.Message);
  {$ENDIF}

  {$IFDEF LINUX}
  // Sur Linux, utiliser syslog
  // openlog('monapp', LOG_PID, LOG_USER);
  // syslog(LOG_ERR, PChar(E.Message));
  // closelog;
  {$ENDIF}
end;

procedure LogErreur(const Message: string; Niveau: Integer);
var
  logFile: string;
  f: TextFile;
begin
  // Déterminer le fichier de log selon l'OS
  {$IFDEF WINDOWS}
    logFile := GetEnvironmentVariable('TEMP') + '\monapp.log';
  {$ENDIF}

  {$IFDEF UNIX}
    {$IFDEF DARWIN}
      logFile := GetEnvironmentVariable('HOME') + '/Library/Logs/monapp.log';
    {$ELSE}
      logFile := '/var/log/monapp.log';
      // Si pas les droits, utiliser /tmp
      if not FileExists('/var/log') then
        logFile := '/tmp/monapp.log';
    {$ENDIF}
  {$ENDIF}

  AssignFile(f, logFile);
  try
    if FileExists(logFile) then
      Append(f)
    else
      Rewrite(f);

    WriteLn(f, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' [', Niveau, '] ', Message);

  finally
    CloseFile(f);
  end;
end;

end.
```

## Bonnes pratiques

### 1. Organisation du code conditionnel

```pascal
// ✅ BON : Regrouper par fonctionnalité
unit MonUnite;

interface

// Déclarations communes
type
  TMonType = class
    procedure MethodeCommune;
    {$IFDEF WINDOWS}
    procedure MethodeWindows;
    {$ENDIF}
    {$IFDEF LINUX}
    procedure MethodeLinux;
    {$ENDIF}
  end;

implementation

// Implémentation commune
procedure TMonType.MethodeCommune;
begin
  // Code commun à toutes les plateformes
end;

// Implémentations spécifiques regroupées
{$IFDEF WINDOWS}
procedure TMonType.MethodeWindows;
begin
  // Code Windows
end;
{$ENDIF}

{$IFDEF LINUX}
procedure TMonType.MethodeLinux;
begin
  // Code Linux
end;
{$ENDIF}

// ❌ MAUVAIS : Mélanger les conditions
procedure MauvaiseOrganisation;
begin
  {$IFDEF WINDOWS}
  DoSomething;
  {$ENDIF}
  CommonCode;
  {$IFDEF LINUX}
  DoSomethingElse;
  {$ENDIF}
  MoreCommonCode;
  {$IFDEF WINDOWS}
  WindowsSpecific;
  {$ENDIF}
  // Difficile à lire et maintenir !
end;
```

### 2. Utiliser des constantes nommées

```pascal
// ✅ BON : Constantes explicites
const
  {$IFDEF WINDOWS}
  PLATFORM_NAME = 'Windows';
  CONFIG_DIR = 'AppData\Roaming\';
  {$ENDIF}

  {$IFDEF LINUX}
  PLATFORM_NAME = 'Linux';
  CONFIG_DIR = '.config/';
  {$ENDIF}

// Utilisation
WriteLn('Plateforme : ', PLATFORM_NAME);
configPath := GetHomeDir + CONFIG_DIR + 'monapp/';

// ❌ MAUVAIS : Valeurs magiques répétées
{$IFDEF WINDOWS}
path := home + '\AppData\Roaming\monapp\';
{$ENDIF}
{$IFDEF LINUX}
path := home + '/.config/monapp/';
{$ENDIF}
```

### 3. Centraliser les définitions

```pascal
// ✅ BON : Fichier central platform.inc
{$I platform.inc}

// platform.inc contient toutes les détections
{$IFDEF WINDOWS}
  {$DEFINE PLATFORM_WINDOWS}
  {$IFDEF WIN64}
    {$DEFINE PLATFORM_64BIT}
  {$ENDIF}
{$ENDIF}

// Utilisation simple
{$IFDEF PLATFORM_WINDOWS}
  // Code Windows
{$ENDIF}

{$IFDEF PLATFORM_64BIT}
  // Code 64 bits
{$ENDIF}
```

### 4. Documenter les conditions

```pascal
// ✅ BON : Documentation claire
{$IFDEF ENABLE_EXPERIMENTAL}
  { Fonctionnalité expérimentale : nouveau système de cache
    Activé avec : -dENABLE_EXPERIMENTAL
    Status : En test, ne pas utiliser en production
    Dépendances : Nécessite FPC 3.2+ }
  procedure NouveauCache;
  begin
    // ...
  end;
{$ENDIF}
```

## Débogage de la compilation conditionnelle

### Vérifier les symboles actifs

```pascal
procedure AfficherSymbolesActifs;
begin
  WriteLn('=== Symboles de compilation actifs ===');

  {$IFDEF WINDOWS}
  WriteLn('✓ WINDOWS');
  {$ENDIF}

  {$IFDEF WIN32}
  WriteLn('✓ WIN32');
  {$ENDIF}

  {$IFDEF WIN64}
  WriteLn('✓ WIN64');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('✓ LINUX');
  {$ENDIF}

  {$IFDEF UNIX}
  WriteLn('✓ UNIX');
  {$ENDIF}

  {$IFDEF CPU32}
  WriteLn('✓ CPU32');
  {$ENDIF}

  {$IFDEF CPU64}
  WriteLn('✓ CPU64');
  {$ENDIF}

  {$IFDEF DEBUG}
  WriteLn('✓ DEBUG');
  {$ENDIF}

  {$IFDEF RELEASE}
  WriteLn('✓ RELEASE');
  {$ENDIF}
end;
```

## Tableau récapitulatif des symboles

| Catégorie | Symbole | Description | Exemple d'usage |
|-----------|---------|-------------|-----------------|
| **OS** | WINDOWS | Windows toutes versions | Appels API Windows |
| | LINUX | Linux | Appels système Linux |
| | DARWIN | macOS | Frameworks macOS |
| | UNIX | Unix/Linux/macOS | Code POSIX |
| | BSD | FreeBSD, OpenBSD | Spécificités BSD |
| **Architecture** | CPU32 | 32 bits | Limites mémoire |
| | CPU64 | 64 bits | Types 64 bits |
| | CPUI386 | x86 | Instructions x86 |
| | CPUX86_64 | x64/AMD64 | SSE/AVX |
| | CPUARM | ARM tous | Code ARM |
| | CPUAARCH64 | ARM 64 | Apple M1/M2 |
| **Compilateur** | FPC | FreePascal | vs Delphi |
| | VER3_2 | FPC 3.2.x | Features 3.2 |
| | DEBUG | Mode debug | Logs détaillés |
| | RELEASE | Mode release | Optimisations |

## Conclusion

La compilation conditionnelle est l'outil fondamental pour créer des applications FreePascal/Lazarus véritablement multi-plateformes. Elle permet d'avoir un code source unique qui s'adapte automatiquement à Windows, Linux, macOS et autres systèmes.

Points clés à retenir :

1. **Utilisez les symboles prédéfinis** pour détecter la plateforme automatiquement
2. **Organisez votre code** en regroupant les parties conditionnelles
3. **Créez des abstractions** pour isoler le code spécifique à chaque OS
4. **Testez sur toutes les plateformes** cibles avec différentes configurations
5. **Documentez vos conditions** pour faciliter la maintenance
6. **Centralisez les définitions** dans des fichiers .inc réutilisables

Avec ces techniques, vous pouvez maintenir un seul code source tout en exploitant les spécificités de chaque plateforme, créant ainsi des applications performantes et natives sur tous les systèmes d'exploitation.

⏭️ [Ressources et icônes multi-plateformes](/05-developpement-multiplateforme-approfondi/05-ressources-icones-multiplatefomes.md)
