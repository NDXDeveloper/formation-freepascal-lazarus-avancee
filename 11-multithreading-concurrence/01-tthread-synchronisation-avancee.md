🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.1 TThread et synchronisation avancée

## Introduction au multithreading

Le multithreading permet à votre application d'exécuter plusieurs tâches simultanément. Imaginez que vous devez télécharger des fichiers tout en permettant à l'utilisateur de continuer à utiliser votre interface graphique : c'est exactement ce que permettent les threads.

En FreePascal/Lazarus, la classe **TThread** est l'outil principal pour créer et gérer des threads.

## Qu'est-ce qu'un thread ?

Un **thread** (ou fil d'exécution) est une unité d'exécution indépendante au sein d'un processus. Votre programme principal s'exécute dans un thread appelé "thread principal" ou "main thread". Vous pouvez créer des threads supplémentaires pour effectuer des tâches en arrière-plan.

### Pourquoi utiliser les threads ?

- **Réactivité de l'interface** : L'interface utilisateur reste fluide pendant les opérations longues
- **Performance** : Exploitation des processeurs multicœurs
- **Séparation des responsabilités** : Isoler les tâches complexes du code principal

### Les défis du multithreading

- **Synchronisation** : Coordonner l'accès aux ressources partagées
- **Race conditions** : Éviter que plusieurs threads modifient les mêmes données simultanément
- **Deadlocks** : Éviter que des threads se bloquent mutuellement

## Création d'un thread simple avec TThread

### Structure de base

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Déclaration de notre classe thread
  TMonThread = class(TThread)
  protected
    procedure Execute; override;
  end;

implementation

procedure TMonThread.Execute;  
begin
  // Code qui s'exécute dans le thread
  // ATTENTION : Ne pas accéder directement à l'interface graphique ici !

  while not Terminated do
  begin
    // Votre traitement ici
    Sleep(100); // Pause de 100 millisecondes
  end;
end;

end.
```

### Créer et démarrer un thread

```pascal
var
  MonThread: TMonThread;
begin
  // Créer le thread (suspendu au départ)
  MonThread := TMonThread.Create(True);

  // Configuration optionnelle
  MonThread.FreeOnTerminate := True; // Le thread se libère automatiquement

  // Démarrer le thread
  MonThread.Start;
end;
```

## Synchronisation avec le thread principal

### Le problème

Les composants visuels de Lazarus (LCL) ne sont **pas thread-safe**. Cela signifie que vous ne pouvez pas modifier l'interface graphique directement depuis un thread secondaire.

```pascal
// ❌ INCORRECT - Ne faites JAMAIS cela !
procedure TMonThread.Execute;  
begin
  Label1.Caption := 'Ceci va planter !'; // DANGER !
end;
```

### La solution : Synchronize

La méthode `Synchronize` permet d'exécuter du code dans le thread principal de manière sécurisée.

```pascal
type
  TMonThread = class(TThread)
  private
    FMessage: string;
    procedure MettreAJourInterface;
  protected
    procedure Execute; override;
  end;

procedure TMonThread.MettreAJourInterface;  
begin
  // Ce code s'exécute dans le thread principal
  // On peut modifier l'interface en toute sécurité
  Form1.Label1.Caption := FMessage;
end;

procedure TMonThread.Execute;  
begin
  while not Terminated do
  begin
    // Traitement dans le thread secondaire
    FMessage := 'Traitement en cours... ' + TimeToStr(Now);

    // Synchronisation avec le thread principal
    Synchronize(@MettreAJourInterface);

    Sleep(1000);
  end;
end;
```

### Queue : synchronisation non-bloquante

Contrairement à `Synchronize` qui attend que le thread principal exécute le code, `Queue` place la demande dans une file d'attente et continue immédiatement.

```pascal
procedure TMonThread.Execute;  
begin
  // Queue ne bloque pas le thread
  Queue(@MettreAJourInterface);

  // Le thread continue immédiatement sans attendre
  // que MettreAJourInterface soit exécuté
end;
```

**Quand utiliser quoi ?**
- **Synchronize** : Quand vous devez attendre que l'interface soit mise à jour
- **Queue** : Pour les mises à jour non critiques qui peuvent être retardées

## Sections critiques (TCriticalSection)

Quand plusieurs threads doivent accéder à la même ressource (variable, liste, fichier), il faut protéger cet accès avec une **section critique**.

### Exemple sans protection (problématique)

```pascal
var
  Compteur: Integer = 0; // Variable partagée

// Thread 1 et Thread 2 incrémentent le compteur
procedure TMonThread.Execute;  
begin
  Inc(Compteur); // ❌ Pas thread-safe !
end;
```

**Problème** : Les deux threads peuvent lire et écrire `Compteur` en même temps, causant des résultats imprévisibles.

### Solution avec TCriticalSection

```pascal
uses
  Classes, SyncObjs;

var
  Compteur: Integer = 0;
  CS: TCriticalSection;

procedure TMonThread.Execute;  
begin
  CS.Enter; // Verrouiller
  try
    Inc(Compteur); // ✅ Protégé
  finally
    CS.Leave; // Déverrouiller TOUJOURS dans finally
  end;
end;

// Dans l'initialisation de votre programme
initialization
  CS := TCriticalSection.Create;

finalization
  CS.Free;
```

### Règles d'or pour les sections critiques

1. **Toujours utiliser try...finally** pour garantir le déverrouillage
2. **Minimiser le code** dans la section critique (performances)
3. **Éviter les appels bloquants** dans une section critique (risque de deadlock)

## Mutex et sémaphores

### TMutex : synchronisation inter-processus

Un **Mutex** (Mutual Exclusion) permet de synchroniser non seulement entre threads, mais aussi entre processus différents.

```pascal
uses
  SyncObjs;

var
  Mutex: TMutex;

begin
  Mutex := TMutex.Create(nil, False, 'MonApplicationUnique');

  if Mutex.WaitFor(0) = wrSignaled then
  begin
    try
      // Zone protégée
      ShowMessage('Accès exclusif accordé');
    finally
      Mutex.Release;
    end;
  end
  else
    ShowMessage('Une autre instance est déjà lancée');

  Mutex.Free;
end;
```

### TSemaphore : limiter les accès concurrents

Un **sémaphore** permet de limiter le nombre de threads pouvant accéder simultanément à une ressource.

```pascal
var
  Semaphore: TSemaphore;

begin
  // Autoriser maximum 3 accès simultanés
  Semaphore := TSemaphore.Create(nil, 3, 3, 'MonSemaphore');

  // Demander l'accès
  if Semaphore.WaitFor(5000) = wrSignaled then // Timeout 5 secondes
  begin
    try
      // Maximum 3 threads peuvent être ici simultanément
      FaireTraitement();
    finally
      Semaphore.Release; // Libérer un slot
    end;
  end;

  Semaphore.Free;
end;
```

## TEvent : signalisation entre threads

**TEvent** permet à un thread de signaler un événement à d'autres threads.

```pascal
uses
  SyncObjs;

var
  Event: TEvent;

// Thread producteur
procedure TProducteur.Execute;  
begin
  // Faire un traitement long
  Sleep(5000);

  // Signaler que c'est terminé
  Event.SetEvent;
end;

// Thread consommateur
procedure TConsommateur.Execute;  
begin
  // Attendre le signal
  Event.WaitFor(INFINITE);

  // Le signal a été reçu, continuer
  TraiterResultat();
end;

// Création
begin
  Event := TEvent.Create(nil, True, False, '');
  // Paramètres : (SecurityAttributes, ManualReset, InitialState, Name)
end;
```

### Modes d'événement

- **ManualReset = True** : L'événement reste signalé jusqu'à `ResetEvent`
- **ManualReset = False** : L'événement se réinitialise automatiquement après un `WaitFor`

## Exemple complet : téléchargeur de fichiers

Voici un exemple pratique combinant plusieurs concepts.

```pascal
unit DownloadThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fphttpclient;

type
  TDownloadThread = class(TThread)
  private
    FURL: string;
    FDestination: string;
    FProgress: Integer;
    FStatus: string;
    FCS: TCriticalSection;

    procedure UpdateProgress;
    procedure UpdateStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(const AURL, ADestination: string);
    destructor Destroy; override;

    function GetProgress: Integer;
    function GetStatus: string;
  end;

implementation

constructor TDownloadThread.Create(const AURL, ADestination: string);  
begin
  inherited Create(True); // Créer suspendu

  FURL := AURL;
  FDestination := ADestination;
  FProgress := 0;
  FStatus := 'En attente';
  FCS := TCriticalSection.Create;

  FreeOnTerminate := True;
end;

destructor TDownloadThread.Destroy;  
begin
  FCS.Free;
  inherited;
end;

procedure TDownloadThread.UpdateProgress;  
begin
  // Mise à jour de l'interface (dans le thread principal)
  Form1.ProgressBar1.Position := FProgress;
end;

procedure TDownloadThread.UpdateStatus;  
begin
  // Mise à jour de l'interface (dans le thread principal)
  Form1.LabelStatus.Caption := FStatus;
end;

procedure TDownloadThread.Execute;  
var
  Client: TFPHTTPClient;
  Stream: TFileStream;
begin
  FCS.Enter;
  try
    FStatus := 'Téléchargement en cours...';
  finally
    FCS.Leave;
  end;
  Synchronize(@UpdateStatus);

  Client := TFPHTTPClient.Create(nil);
  Stream := TFileStream.Create(FDestination, fmCreate);
  try
    // Simulation de progression
    for FProgress := 0 to 100 do
    begin
      if Terminated then Break;

      Sleep(50); // Simulation

      // Mise à jour périodique de l'interface
      if FProgress mod 10 = 0 then
        Synchronize(@UpdateProgress);
    end;

    FCS.Enter;
    try
      if Terminated then
        FStatus := 'Annulé'
      else
        FStatus := 'Téléchargement terminé';
    finally
      FCS.Leave;
    end;
    Synchronize(@UpdateStatus);

  finally
    Stream.Free;
    Client.Free;
  end;
end;

function TDownloadThread.GetProgress: Integer;  
begin
  FCS.Enter;
  try
    Result := FProgress;
  finally
    FCS.Leave;
  end;
end;

function TDownloadThread.GetStatus: string;  
begin
  FCS.Enter;
  try
    Result := FStatus;
  finally
    FCS.Leave;
  end;
end;

end.
```

### Utilisation du thread de téléchargement

```pascal
var
  DownloadThread: TDownloadThread;

procedure TForm1.ButtonStartClick(Sender: TObject);  
begin
  DownloadThread := TDownloadThread.Create(
    'http://example.com/fichier.zip',
    'C:\Temp\fichier.zip'
  );
  DownloadThread.Start;
end;

procedure TForm1.ButtonCancelClick(Sender: TObject);  
begin
  if Assigned(DownloadThread) then
    DownloadThread.Terminate;
end;
```

## Bonnes pratiques

### 1. Toujours gérer la terminaison proprement

```pascal
procedure TMonThread.Execute;  
begin
  while not Terminated do
  begin
    // Vérifier régulièrement Terminated
    if Terminated then Break;

    // Votre code
  end;
end;
```

### 2. Ne jamais accéder à l'interface directement

```pascal
// ❌ INCORRECT
procedure TMonThread.Execute;  
begin
  Form1.Edit1.Text := 'Texte'; // DANGER !
end;

// ✅ CORRECT
procedure TMonThread.Execute;  
begin
  Synchronize(@MettreAJourEdit);
end;
```

### 3. Protéger toutes les données partagées

```pascal
// ❌ INCORRECT
var
  ListePartagee: TStringList;

procedure TMonThread.Execute;  
begin
  ListePartagee.Add('Item'); // Pas thread-safe !
end;

// ✅ CORRECT
var
  ListePartagee: TStringList;
  CS: TCriticalSection;

procedure TMonThread.Execute;  
begin
  CS.Enter;
  try
    ListePartagee.Add('Item');
  finally
    CS.Leave;
  end;
end;
```

### 4. Éviter les deadlocks

```pascal
// ❌ Risque de deadlock
CS1.Enter;
  CS2.Enter;
    // Code
  CS2.Leave;
CS1.Leave;

// Dans un autre thread, ordre inversé :
CS2.Enter;  // Deadlock possible !
  CS1.Enter;
    // Code
  CS1.Leave;
CS2.Leave;

// ✅ CORRECT : Toujours verrouiller dans le même ordre
CS1.Enter;
  CS2.Enter;
    // Code
  CS2.Leave;
CS1.Leave;
```

### 5. Libérer les ressources correctement

```pascal
procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);  
begin
  // Demander l'arrêt du thread
  if Assigned(MonThread) then
  begin
    MonThread.Terminate;
    MonThread.WaitFor; // Attendre que le thread se termine
    MonThread.Free;
  end;
end;
```

## Différences Windows/Ubuntu

### Gestion des threads

Les deux systèmes utilisent des threads natifs :
- **Windows** : Threads Win32
- **Ubuntu/Linux** : Threads POSIX (pthreads)

FreePascal/Lazarus abstrait ces différences, votre code fonctionne identiquement sur les deux plateformes.

### Priorités des threads

```pascal
// Définir la priorité (identique sur Windows et Linux)
MonThread.Priority := tpHigher;

// Valeurs disponibles :
// tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest, tpTimeCritical
```

**Note** : Sur Linux, certaines priorités peuvent nécessiter des privilèges root.

### Affinité processeur

```pascal
{$IFDEF WINDOWS}
uses
  Windows;

// Affecter le thread au CPU 0
SetThreadAffinityMask(ThreadHandle, 1);
{$ENDIF}

{$IFDEF LINUX}
uses
  BaseUnix, Unix;

// Affecter le thread au CPU 0
// Code spécifique Linux avec pthread_setaffinity_np
{$ENDIF}
```

## Résumé

- **TThread** est la classe de base pour créer des threads
- **Synchronize** et **Queue** permettent d'interagir avec l'interface graphique
- **TCriticalSection** protège l'accès aux ressources partagées
- **TMutex** permet la synchronisation inter-processus
- **TSemaphore** limite les accès concurrents
- **TEvent** permet la signalisation entre threads

Le multithreading est puissant mais nécessite de la rigueur. Suivez toujours les bonnes pratiques pour éviter les bugs difficiles à reproduire et à corriger.

## Ressources complémentaires

- Documentation officielle FreePascal : https://www.freepascal.org/docs.html
- Wiki Lazarus sur les threads : https://wiki.freepascal.org/Multithreaded_Application_Tutorial
- Forums Lazarus : https://forum.lazarus.freepascal.org/

⏭️ [Thread pools et workers](/11-multithreading-concurrence/02-thread-pools-workers.md)
