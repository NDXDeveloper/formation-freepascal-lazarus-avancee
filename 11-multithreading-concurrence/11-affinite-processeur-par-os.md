🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.11 Affinité processeur par OS

## Introduction

L'**affinité processeur** (ou *CPU affinity*) est une fonctionnalité qui permet de définir sur quels cœurs de processeur un thread ou un processus peut s'exécuter. Cette technique est particulièrement utile pour :

- **Optimiser les performances** en réduisant les migrations de threads entre cœurs
- **Améliorer l'utilisation du cache** CPU en gardant les données proches du cœur d'exécution
- **Contrôler la charge** en répartissant manuellement le travail sur les cœurs disponibles
- **Isoler des tâches critiques** sur des cœurs dédiés

## Concepts de base

### Qu'est-ce qu'un cœur de processeur ?

Les processeurs modernes possèdent plusieurs **cœurs** (cores), chacun capable d'exécuter des instructions indépendamment. Un processeur quad-core possède 4 cœurs, un octa-core en possède 8, etc.

### Qu'est-ce que l'affinité ?

Par défaut, le système d'exploitation décide librement sur quel cœur exécuter chaque thread. L'affinité permet de **restreindre** cette décision en spécifiant explicitement quels cœurs peuvent être utilisés.

### Pourquoi contrôler l'affinité ?

```
Sans affinité :
Thread A : Cœur 0 → Cœur 2 → Cœur 1 → Cœur 3
           (migrations fréquentes = perte de cache)

Avec affinité :
Thread A : Cœur 0 → Cœur 0 → Cœur 0 → Cœur 0
           (pas de migration = cache préservé)
```

## Différences entre Windows et Linux

### Architecture système

**Windows** utilise la notion de :
- **Masque d'affinité** (affinity mask) : un nombre binaire où chaque bit représente un cœur
- **Groupes de processeurs** pour les systèmes avec plus de 64 cœurs

**Linux** utilise :
- **CPU sets** : ensembles de cœurs représentés par des structures de données
- **Masques de bits** similaires à Windows mais avec une API différente

### Numérotation des cœurs

Les deux systèmes numérotent les cœurs à partir de **0**, mais peuvent les organiser différemment selon l'architecture (physique vs logique, HyperThreading, etc.).

## Implémentation sous Windows

### API Windows pour l'affinité

Windows propose plusieurs fonctions dans l'API Win32 :

```pascal
uses
  Windows;

type
  DWORD_PTR = NativeUInt;

function SetThreadAffinityMask(
  hThread: THandle;           // Handle du thread
  dwThreadAffinityMask: DWORD_PTR  // Masque d'affinité
): DWORD_PTR; stdcall; external kernel32;

function SetProcessAffinityMask(
  hProcess: THandle;          // Handle du processus
  dwProcessAffinityMask: DWORD_PTR  // Masque d'affinité
): BOOL; stdcall; external kernel32;
```

### Comprendre le masque d'affinité

Le masque d'affinité est un nombre binaire où chaque bit représente un cœur :

```
Bit 0 (valeur 1)   = Cœur 0
Bit 1 (valeur 2)   = Cœur 1
Bit 2 (valeur 4)   = Cœur 2
Bit 3 (valeur 8)   = Cœur 3
...

Exemples :
1  (0001) = Cœur 0 uniquement
3  (0011) = Cœurs 0 et 1
5  (0101) = Cœurs 0 et 2
15 (1111) = Cœurs 0, 1, 2 et 3
```

### Exemple pratique Windows

```pascal
program AffinityWindows;

{$mode objfpc}{$H+}

uses
  Windows, SysUtils, Classes;

type
  TWorkerThread = class(TThread)
  private
    FCoreNumber: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ACoreNumber: Integer);
  end;

constructor TWorkerThread.Create(ACoreNumber: Integer);
begin
  inherited Create(True); // Créé suspendu
  FCoreNumber := ACoreNumber;
  FreeOnTerminate := True;
end;

procedure TWorkerThread.Execute;
var
  Mask: DWORD_PTR;
  OldMask: DWORD_PTR;
  i: Integer;
  Sum: Int64;
begin
  // Définir l'affinité sur un cœur spécifique
  Mask := 1 shl FCoreNumber; // Décalage de bit pour sélectionner le cœur
  OldMask := SetThreadAffinityMask(GetCurrentThread, Mask);

  if OldMask = 0 then
  begin
    WriteLn('Erreur lors de la définition de l''affinité pour le cœur ', FCoreNumber);
    Exit;
  end;

  WriteLn(Format('Thread exécuté sur le cœur %d', [FCoreNumber]));

  // Travail intensif pour tester
  Sum := 0;
  for i := 1 to 100000000 do
    Sum := Sum + i;

  WriteLn(Format('Cœur %d terminé. Somme: %d', [FCoreNumber, Sum]));
end;

var
  Threads: array of TWorkerThread;
  SysInfo: SYSTEM_INFO;
  i: Integer;
begin
  // Obtenir le nombre de processeurs
  GetSystemInfo(SysInfo);
  WriteLn(Format('Nombre de processeurs détectés : %d', [SysInfo.dwNumberOfProcessors]));

  // Créer un thread par cœur
  SetLength(Threads, SysInfo.dwNumberOfProcessors);

  for i := 0 to High(Threads) do
  begin
    Threads[i] := TWorkerThread.Create(i);
    Threads[i].Start;
  end;

  // Attendre que tous les threads se terminent
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Obtenir des informations système sous Windows

```pascal
procedure GetCPUInfo;
var
  SysInfo: SYSTEM_INFO;
  ProcessAffinityMask, SystemAffinityMask: DWORD_PTR;
begin
  GetSystemInfo(SysInfo);

  WriteLn('=== Informations CPU Windows ===');
  WriteLn('Nombre de processeurs : ', SysInfo.dwNumberOfProcessors);
  WriteLn('Type de processeur : ', SysInfo.dwProcessorType);
  WriteLn('Niveau de processeur : ', SysInfo.wProcessorLevel);

  // Obtenir les masques d'affinité
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask) then
  begin
    WriteLn('Masque d''affinité du processus : ', BinStr(ProcessAffinityMask, 64));
    WriteLn('Masque d''affinité du système : ', BinStr(SystemAffinityMask, 64));
  end;
end;
```

## Implémentation sous Linux

### API Linux pour l'affinité

Linux utilise les fonctions POSIX avec l'extension GNU :

```pascal
uses
  BaseUnix, Unix;

type
  cpu_set_t = record
    __bits: array[0..15] of QWord; // Peut contenir jusqu'à 1024 cœurs
  end;

// Macros C converties en fonctions Pascal
procedure CPU_ZERO(var CpuSet: cpu_set_t); inline;
procedure CPU_SET(cpu: Integer; var CpuSet: cpu_set_t); inline;
function CPU_ISSET(cpu: Integer; const CpuSet: cpu_set_t): Boolean; inline;

// Fonctions système
function sched_setaffinity(pid: pid_t; cpusetsize: size_t;
  const mask: cpu_set_t): cint; cdecl; external 'c';

function sched_getaffinity(pid: pid_t; cpusetsize: size_t;
  var mask: cpu_set_t): cint; cdecl; external 'c';
```

### Implémentation des macros CPU_*

```pascal
procedure CPU_ZERO(var CpuSet: cpu_set_t);
begin
  FillChar(CpuSet, SizeOf(CpuSet), 0);
end;

procedure CPU_SET(cpu: Integer; var CpuSet: cpu_set_t);
var
  idx, bit: Integer;
begin
  idx := cpu div 64;
  bit := cpu mod 64;
  CpuSet.__bits[idx] := CpuSet.__bits[idx] or (QWord(1) shl bit);
end;

function CPU_ISSET(cpu: Integer; const CpuSet: cpu_set_t): Boolean;
var
  idx, bit: Integer;
begin
  idx := cpu div 64;
  bit := cpu mod 64;
  Result := (CpuSet.__bits[idx] and (QWord(1) shl bit)) <> 0;
end;
```

### Exemple pratique Linux

```pascal
program AffinityLinux;

{$mode objfpc}{$H+}

uses
  BaseUnix, Unix, SysUtils, Classes, ctypes;

type
  cpu_set_t = record
    __bits: array[0..15] of QWord;
  end;

procedure CPU_ZERO(var CpuSet: cpu_set_t); inline;
begin
  FillChar(CpuSet, SizeOf(CpuSet), 0);
end;

procedure CPU_SET(cpu: Integer; var CpuSet: cpu_set_t); inline;
var
  idx, bit: Integer;
begin
  idx := cpu div 64;
  bit := cpu mod 64;
  CpuSet.__bits[idx] := CpuSet.__bits[idx] or (QWord(1) shl bit);
end;

function sched_setaffinity(pid: pid_t; cpusetsize: size_t;
  const mask: cpu_set_t): cint; cdecl; external 'c';

type
  TWorkerThread = class(TThread)
  private
    FCoreNumber: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ACoreNumber: Integer);
  end;

constructor TWorkerThread.Create(ACoreNumber: Integer);
begin
  inherited Create(True);
  FCoreNumber := ACoreNumber;
  FreeOnTerminate := True;
end;

procedure TWorkerThread.Execute;
var
  CpuSet: cpu_set_t;
  i: Integer;
  Sum: Int64;
begin
  // Initialiser le CPU set
  CPU_ZERO(CpuSet);
  CPU_SET(FCoreNumber, CpuSet);

  // Définir l'affinité (0 = thread actuel)
  if sched_setaffinity(0, SizeOf(CpuSet), CpuSet) = -1 then
  begin
    WriteLn('Erreur lors de la définition de l''affinité pour le cœur ', FCoreNumber);
    Exit;
  end;

  WriteLn(Format('Thread exécuté sur le cœur %d', [FCoreNumber]));

  // Travail intensif
  Sum := 0;
  for i := 1 to 100000000 do
    Sum := Sum + i;

  WriteLn(Format('Cœur %d terminé. Somme: %d', [FCoreNumber, Sum]));
end;

var
  Threads: array of TWorkerThread;
  NumCores: Integer;
  i: Integer;
begin
  // Obtenir le nombre de cœurs (méthode simple)
  NumCores := sysconf(_SC_NPROCESSORS_ONLN);
  WriteLn(Format('Nombre de processeurs détectés : %d', [NumCores]));

  SetLength(Threads, NumCores);

  for i := 0 to High(Threads) do
  begin
    Threads[i] := TWorkerThread.Create(i);
    Threads[i].Start;
  end;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Obtenir des informations système sous Linux

```pascal
function GetCPUCount: Integer;
begin
  Result := sysconf(_SC_NPROCESSORS_ONLN); // Cœurs en ligne
end;

function GetCPUCountConfigured: Integer;
begin
  Result := sysconf(_SC_NPROCESSORS_CONF); // Cœurs configurés
end;

procedure ShowCPUAffinity;
var
  CpuSet: cpu_set_t;
  i: Integer;
begin
  if sched_getaffinity(0, SizeOf(CpuSet), CpuSet) = 0 then
  begin
    WriteLn('=== Affinité actuelle ===');
    for i := 0 to GetCPUCount - 1 do
    begin
      if CPU_ISSET(i, CpuSet) then
        Write(i, ' ');
    end;
    WriteLn;
  end;
end;
```

## Code multi-plateforme unifié

### Création d'une unité portable

```pascal
unit CPUAffinity;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix, Unix, ctypes
  {$ENDIF};

type
  {$IFDEF UNIX}
  TCpuSetBits = record
    __bits: array[0..15] of QWord;
  end;
  {$ENDIF}

  TCPUSet = record
    {$IFDEF WINDOWS}
    Mask: NativeUInt;
    {$ENDIF}
    {$IFDEF UNIX}
    CpuSet: TCpuSetBits;
    {$ENDIF}
  end;

function GetCPUCount: Integer;
function SetThreadAffinity(ThreadID: TThreadID; CoreNumber: Integer): Boolean;
function SetThreadAffinitySet(ThreadID: TThreadID; const CpuSet: TCPUSet): Boolean;
procedure InitCPUSet(var CpuSet: TCPUSet);
procedure AddCPUToSet(var CpuSet: TCPUSet; CoreNumber: Integer);
function GetCurrentThreadAffinity(var CpuSet: TCPUSet): Boolean;

implementation

{$IFDEF UNIX}
function sched_setaffinity(pid: pid_t; cpusetsize: size_t;
  const mask: TCpuSetBits): cint; cdecl; external 'c' name 'sched_setaffinity';
function sched_getaffinity(pid: pid_t; cpusetsize: size_t;
  var mask: TCpuSetBits): cint; cdecl; external 'c' name 'sched_getaffinity';
{$ENDIF}

function GetCPUCount: Integer;
{$IFDEF WINDOWS}
var
  SysInfo: SYSTEM_INFO;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := sysconf(_SC_NPROCESSORS_ONLN);
  {$ENDIF}
end;

function SetThreadAffinity(ThreadID: TThreadID; CoreNumber: Integer): Boolean;
{$IFDEF WINDOWS}
var
  Mask: NativeUInt;
{$ENDIF}
{$IFDEF UNIX}
var
  CpuSet: TCPUSet;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Mask := NativeUInt(1) shl CoreNumber;
  Result := SetThreadAffinityMask(ThreadID, Mask) <> 0;
  {$ENDIF}
  {$IFDEF UNIX}
  InitCPUSet(CpuSet);
  AddCPUToSet(CpuSet, CoreNumber);
  Result := sched_setaffinity(0, SizeOf(CpuSet.CpuSet), CpuSet.CpuSet) = 0;
  {$ENDIF}
end;

procedure InitCPUSet(var CpuSet: TCPUSet);
begin
  {$IFDEF WINDOWS}
  CpuSet.Mask := 0;
  {$ENDIF}
  {$IFDEF UNIX}
  FillChar(CpuSet.CpuSet, SizeOf(CpuSet.CpuSet), 0);
  {$ENDIF}
end;

procedure AddCPUToSet(var CpuSet: TCPUSet; CoreNumber: Integer);
{$IFDEF UNIX}
var
  idx, bit: Integer;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  CpuSet.Mask := CpuSet.Mask or (NativeUInt(1) shl CoreNumber);
  {$ENDIF}
  {$IFDEF UNIX}
  idx := CoreNumber div 64;
  bit := CoreNumber mod 64;
  CpuSet.CpuSet.__bits[idx] := CpuSet.CpuSet.__bits[idx] or (QWord(1) shl bit);
  {$ENDIF}
end;

function SetThreadAffinitySet(ThreadID: TThreadID; const CpuSet: TCPUSet): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := SetThreadAffinityMask(ThreadID, CpuSet.Mask) <> 0;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := sched_setaffinity(0, SizeOf(CpuSet.CpuSet), CpuSet.CpuSet) = 0;
  {$ENDIF}
end;

function GetCurrentThreadAffinity(var CpuSet: TCPUSet): Boolean;
{$IFDEF WINDOWS}
var
  ProcessMask, SystemMask: NativeUInt;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Result := GetProcessAffinityMask(GetCurrentProcess, ProcessMask, SystemMask);
  if Result then
    CpuSet.Mask := ProcessMask;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := sched_getaffinity(0, SizeOf(CpuSet.CpuSet), CpuSet.CpuSet) = 0;
  {$ENDIF}
end;

end.
```

### Utilisation de l'unité portable

```pascal
program MultiPlatformAffinity;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, CPUAffinity;

type
  TWorkerThread = class(TThread)
  private
    FCoreNumber: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ACoreNumber: Integer);
  end;

constructor TWorkerThread.Create(ACoreNumber: Integer);
begin
  inherited Create(True);
  FCoreNumber := ACoreNumber;
  FreeOnTerminate := True;
end;

procedure TWorkerThread.Execute;
var
  i: Integer;
  Sum: Int64;
begin
  // Utilisation de notre API portable
  if not SetThreadAffinity(ThreadID, FCoreNumber) then
  begin
    WriteLn('Erreur d''affinité pour le cœur ', FCoreNumber);
    Exit;
  end;

  WriteLn(Format('Thread sur cœur %d démarré', [FCoreNumber]));

  Sum := 0;
  for i := 1 to 100000000 do
    Sum := Sum + i;

  WriteLn(Format('Cœur %d terminé', [FCoreNumber]));
end;

var
  Threads: array of TWorkerThread;
  NumCores, i: Integer;
begin
  NumCores := GetCPUCount;
  WriteLn(Format('Système avec %d cœurs détectés', [NumCores]));

  SetLength(Threads, NumCores);

  for i := 0 to High(Threads) do
  begin
    Threads[i] := TWorkerThread.Create(i);
    Threads[i].Start;
  end;

  WriteLn('Appuyez sur Entrée...');
  ReadLn;
end.
```

## Cas d'usage avancés

### 1. Répartition manuelle de charge

```pascal
// Séparer les threads I/O des threads de calcul
procedure ConfigureThreadPool;
var
  CpuSet: TCPUSet;
begin
  // Threads I/O sur les cœurs 0-1
  InitCPUSet(CpuSet);
  AddCPUToSet(CpuSet, 0);
  AddCPUToSet(CpuSet, 1);
  SetThreadAffinitySet(IOThreadID, CpuSet);

  // Threads de calcul sur les cœurs 2-7
  InitCPUSet(CpuSet);
  for i := 2 to 7 do
    AddCPUToSet(CpuSet, i);
  SetThreadAffinitySet(ComputeThreadID, CpuSet);
end;
```

### 2. Isolation de tâches critiques

```pascal
// Dédier un cœur à une tâche temps-réel
procedure SetupRealTimeTask;
var
  RTCore: Integer;
begin
  RTCore := GetCPUCount - 1; // Dernier cœur
  SetThreadAffinity(RealTimeThreadID, RTCore);

  // Exclure ce cœur des autres threads
  // (nécessite des privilèges administrateur)
end;
```

### 3. Optimisation NUMA

```pascal
// Sur les systèmes NUMA, garder les threads près de leur mémoire
procedure OptimizeForNUMA;
begin
  // Socket 0 : cœurs 0-7
  // Socket 1 : cœurs 8-15

  // Threads travaillant sur le même bloc de données
  // doivent être sur le même socket
  SetThreadAffinity(DataThread1, 0);
  SetThreadAffinity(DataThread2, 1);
end;
```

## Considérations importantes

### Performance vs Flexibilité

**Avantages** de l'affinité :
- Meilleure utilisation du cache CPU
- Latence réduite pour les données chaudes
- Prédictibilité accrue

**Inconvénients** :
- Perte de flexibilité du scheduler
- Risque de déséquilibre de charge
- Complexité de gestion

### Quand utiliser l'affinité ?

✅ **Utilisez l'affinité quand :**
- Vous avez des threads avec beaucoup de données partagées
- Vous développez des applications temps-réel
- Vous faites du calcul haute performance (HPC)
- Vous optimisez pour une architecture matérielle spécifique

❌ **N'utilisez PAS l'affinité quand :**
- Votre application est simple et bien parallélisée
- La charge de travail est dynamique et imprévisible
- Vous n'avez pas de problème de performance identifié
- Vous visez la portabilité maximale

### Privilèges requis

**Windows** : Aucun privilège spécial pour les threads du processus

**Linux** :
- Aucun privilège pour les threads du processus
- Privilèges root pour modifier l'affinité d'autres processus
- Capacité `CAP_SYS_NICE` pour certaines opérations avancées

## Outils de diagnostic

### Windows

```bash
# PowerShell : voir l'affinité d'un processus
Get-Process | Select-Object Name, ProcessorAffinity

# Définir l'affinité via PowerShell
$process = Get-Process -Name "MonApp"
$process.ProcessorAffinity = 0x0F  # Cœurs 0-3
```

### Linux

```bash
# Voir l'affinité d'un processus
taskset -p <PID>

# Lancer un programme avec une affinité spécifique
taskset -c 0,1,2 ./monprogramme

# Modifier l'affinité d'un processus en cours
taskset -p -c 0,1 <PID>
```

## Résumé

L'affinité processeur est un outil puissant pour optimiser les performances des applications multi-threadées. Les principales différences entre Windows et Linux résident dans l'API utilisée (masques simples vs CPU sets), mais le concept reste identique.

En créant une couche d'abstraction portable comme l'unité `CPUAffinity` présentée ici, vous pouvez gérer l'affinité de manière uniforme sur les deux systèmes tout en bénéficiant des optimisations spécifiques à chaque plateforme.

**Points clés à retenir :**
- L'affinité lie un thread à des cœurs spécifiques
- Elle améliore l'utilisation du cache mais réduit la flexibilité
- Windows utilise des masques binaires simples
- Linux utilise des structures cpu_set_t plus complexes
- Une abstraction multi-plateforme est possible et recommandée
- À n'utiliser que lorsqu'un besoin réel d'optimisation est identifié

⏭️ [Débogage d'applications concurrentes](/11-multithreading-concurrence/12-debogage-applications-concurrentes.md)
