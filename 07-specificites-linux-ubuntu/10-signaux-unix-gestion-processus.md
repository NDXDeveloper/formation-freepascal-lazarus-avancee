🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Signaux Unix et Gestion des Processus avec FreePascal/Lazarus

## Introduction : Qu'est-ce qu'un signal Unix ?

Imaginez que vous travaillez sur votre ordinateur et que vous avez besoin d'interrompre un programme qui ne répond plus. Sous Linux/Ubuntu, quand vous appuyez sur `Ctrl+C` dans un terminal, vous envoyez en réalité un **signal** au programme pour lui demander de s'arrêter. Les signaux sont le système de communication fondamental entre le système d'exploitation et les programmes sous Unix/Linux.

Un signal est comme une notification urgente envoyée à un programme. C'est un mécanisme simple mais puissant qui permet :
- D'arrêter proprement un programme
- De le mettre en pause ou de le reprendre
- De lui demander de recharger sa configuration
- De communiquer entre différents programmes

## Les signaux les plus courants

Avant de plonger dans le code, familiarisons-nous avec les signaux que vous rencontrerez le plus souvent :

### SIGTERM (Signal 15) - Demande d'arrêt poli
C'est le signal par défaut envoyé par la commande `kill`. Il demande poliment au programme de s'arrêter, en lui laissant le temps de sauvegarder ses données et de nettoyer ses ressources.

### SIGKILL (Signal 9) - Arrêt forcé
Le signal "nucléaire" qui termine immédiatement un programme sans lui laisser aucune chance de réagir. À utiliser uniquement en dernier recours car le programme ne peut pas sauvegarder son travail.

### SIGINT (Signal 2) - Interruption clavier
C'est ce qui se passe quand vous appuyez sur `Ctrl+C`. Le programme peut l'intercepter pour faire un arrêt propre.

### SIGHUP (Signal 1) - Raccrochage
Historiquement envoyé quand un terminal était déconnecté. Aujourd'hui, souvent utilisé pour demander à un programme de recharger sa configuration.

### SIGUSR1 et SIGUSR2 (Signaux 10 et 12) - Signaux utilisateur
Réservés pour votre propre usage. Vous pouvez les utiliser pour créer vos propres commandes personnalisées.

## Configuration de base dans FreePascal

Pour utiliser les signaux dans FreePascal, nous utilisons principalement deux unités :
- `BaseUnix` : pour les fonctions Unix de base
- `Unix` : pour les fonctions Unix de plus haut niveau

Voici la structure de base d'un programme gérant les signaux :

```pascal
program GestionSignaux;

{$mode objfpc}{$H+}

uses
  BaseUnix, Unix, SysUtils;

var
  TerminationRequested: Boolean = False;

// Procédure qui sera appelée quand un signal est reçu
procedure SignalHandler(sig: cint); cdecl;
begin
  case sig of
    SIGTERM, SIGINT: begin
      WriteLn('Signal d''arrêt reçu. Fermeture propre...');
      TerminationRequested := True;
    end;
    SIGHUP: begin
      WriteLn('Signal SIGHUP reçu. Rechargement de la configuration...');
      // Ici, vous rechargeriez votre configuration
    end;
  end;
end;

begin
  // Installation du gestionnaire de signaux
  FpSignal(SIGTERM, @SignalHandler);
  FpSignal(SIGINT, @SignalHandler);
  FpSignal(SIGHUP, @SignalHandler);

  WriteLn('Programme démarré. PID: ', FpGetPid);
  WriteLn('Appuyez sur Ctrl+C pour arrêter proprement');

  // Boucle principale du programme
  while not TerminationRequested do
  begin
    // Votre code principal ici
    Sleep(1000); // Pause d'une seconde
    Write('.');
  end;

  WriteLn;
  WriteLn('Programme terminé proprement');
end.
```

## Comprendre les processus Unix

### Qu'est-ce qu'un processus ?

Un processus est simplement un programme en cours d'exécution. Chaque processus a :
- Un **PID** (Process ID) : un numéro unique qui l'identifie
- Un processus parent (sauf pour init/systemd qui a le PID 1)
- Des ressources (mémoire, fichiers ouverts, etc.)
- Un état (en cours, en pause, zombie, etc.)

### Obtenir des informations sur les processus

```pascal
program InfoProcessus;

{$mode objfpc}{$H+}

uses
  Unix, BaseUnix, SysUtils;

begin
  WriteLn('=== Informations sur ce processus ===');
  WriteLn('PID (mon numéro) : ', FpGetPid);
  WriteLn('PPID (PID de mon parent) : ', FpGetPPid);
  WriteLn('UID (utilisateur) : ', FpGetUid);
  WriteLn('GID (groupe) : ', FpGetGid);
  WriteLn('Répertoire de travail : ', GetCurrentDir);
end.
```

## Créer des processus enfants

L'une des fonctionnalités puissantes d'Unix est la capacité de créer des processus enfants. FreePascal offre plusieurs moyens de le faire.

### Méthode simple avec TProcess

La classe `TProcess` de FreePascal simplifie grandement la création de processus :

```pascal
program LancerProcessus;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Process;

var
  Processus: TProcess;

begin
  Processus := TProcess.Create(nil);
  try
    // Configuration du processus
    Processus.Executable := 'ls';
    Processus.Parameters.Add('-la');
    Processus.Parameters.Add('/home');

    // Options pour capturer la sortie
    Processus.Options := [poWaitOnExit, poUsePipes];

    WriteLn('Lancement de la commande ls -la /home');
    Processus.Execute;

    // Lire et afficher la sortie
    WriteLn('Résultat :');
    WriteLn(Processus.Output.ReadAnsiString);

    WriteLn('Code de retour : ', Processus.ExitStatus);
  finally
    Processus.Free;
  end;
end.
```

### Méthode avancée avec Fork

Pour un contrôle plus fin, vous pouvez utiliser `FpFork` qui crée une copie exacte du processus :

```pascal
program ForkExemple;

{$mode objfpc}{$H+}

uses
  BaseUnix, Unix, SysUtils;

var
  pid: TPid;
  status: cint;

begin
  WriteLn('Processus parent, PID: ', FpGetPid);

  // Fork crée une copie du processus
  pid := FpFork;

  if pid = -1 then
  begin
    WriteLn('Erreur lors du fork!');
    Exit;
  end
  else if pid = 0 then
  begin
    // Code exécuté par le processus enfant
    WriteLn('Je suis le processus enfant, PID: ', FpGetPid);
    WriteLn('Mon parent a le PID: ', FpGetPPid);

    // L'enfant fait son travail
    Sleep(2000);
    WriteLn('Enfant : travail terminé');

    // Important : l'enfant doit terminer
    Halt(0);
  end
  else
  begin
    // Code exécuté par le processus parent
    WriteLn('Je suis le parent, j''ai créé l''enfant avec PID: ', pid);

    // Attendre que l'enfant termine
    WriteLn('Parent : j''attends que mon enfant termine...');
    FpWaitPid(pid, @status, 0);

    WriteLn('Parent : mon enfant a terminé avec le status ', status);
  end;
end.
```

## Envoyer des signaux à d'autres processus

Vous pouvez envoyer des signaux à d'autres processus (avec les bonnes permissions) :

```pascal
program EnvoyerSignal;

{$mode objfpc}{$H+}

uses
  BaseUnix, Unix, SysUtils;

var
  targetPid: Integer;

begin
  Write('Entrez le PID du processus cible : ');
  ReadLn(targetPid);

  WriteLn('Envoi d''un signal SIGUSR1 au processus ', targetPid);

  if FpKill(targetPid, SIGUSR1) = 0 then
    WriteLn('Signal envoyé avec succès')
  else
    WriteLn('Erreur : ', SysErrorMessage(fpgeterrno));
end.
```

## Gérer plusieurs signaux avec un tableau

Pour une application plus complexe, il est pratique d'organiser la gestion des signaux :

```pascal
program GestionnaireSignauxAvance;

{$mode objfpc}{$H+}

uses
  BaseUnix, Unix, SysUtils;

type
  TSignalInfo = record
    Signal: cint;
    Name: string;
    Count: Integer;
    Action: procedure;
  end;

var
  SignalStats: array[1..5] of TSignalInfo;
  Running: Boolean = True;
  i: Integer;

procedure ActionTerminate;
begin
  WriteLn('Arrêt demandé - sauvegarde en cours...');
  Running := False;
end;

procedure ActionReload;
begin
  WriteLn('Rechargement de la configuration...');
  // Code de rechargement ici
end;

procedure ActionInfo;
begin
  WriteLn('État du système :');
  WriteLn('- Mémoire utilisée : ...'); // À implémenter
  WriteLn('- Temps d''exécution : ...'); // À implémenter
end;

procedure SignalHandler(sig: cint); cdecl;
var
  i: Integer;
begin
  for i := Low(SignalStats) to High(SignalStats) do
  begin
    if SignalStats[i].Signal = sig then
    begin
      Inc(SignalStats[i].Count);
      WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] Signal ',
              SignalStats[i].Name, ' reçu (total: ',
              SignalStats[i].Count, ')');
      if Assigned(SignalStats[i].Action) then
        SignalStats[i].Action();
      Break;
    end;
  end;
end;

procedure InitializeSignals;
var
  i: Integer;
begin
  // Configuration du tableau de signaux
  SignalStats[1].Signal := SIGTERM;
  SignalStats[1].Name := 'SIGTERM';
  SignalStats[1].Action := @ActionTerminate;

  SignalStats[2].Signal := SIGINT;
  SignalStats[2].Name := 'SIGINT';
  SignalStats[2].Action := @ActionTerminate;

  SignalStats[3].Signal := SIGHUP;
  SignalStats[3].Name := 'SIGHUP';
  SignalStats[3].Action := @ActionReload;

  SignalStats[4].Signal := SIGUSR1;
  SignalStats[4].Name := 'SIGUSR1';
  SignalStats[4].Action := @ActionInfo;

  SignalStats[5].Signal := SIGUSR2;
  SignalStats[5].Name := 'SIGUSR2';
  SignalStats[5].Action := nil; // Pas d'action spécifique

  // Installation des gestionnaires
  for i := Low(SignalStats) to High(SignalStats) do
  begin
    FpSignal(SignalStats[i].Signal, @SignalHandler);
    SignalStats[i].Count := 0;
  end;
end;

begin
  InitializeSignals;

  WriteLn('=== Gestionnaire de signaux avancé ===');
  WriteLn('PID: ', FpGetPid);
  WriteLn('Signaux gérés :');
  WriteLn('- SIGTERM/SIGINT (Ctrl+C) : Arrêt propre');
  WriteLn('- SIGHUP : Rechargement configuration');
  WriteLn('- SIGUSR1 : Affichage informations');
  WriteLn('- SIGUSR2 : Signal personnalisé');
  WriteLn;
  WriteLn('Testez avec : kill -USR1 ', FpGetPid);

  while Running do
  begin
    Sleep(1000);
    // Travail principal de l'application
  end;

  WriteLn;
  WriteLn('=== Statistiques finales ===');
  for i := Low(SignalStats) to High(SignalStats) do
  begin
    if SignalStats[i].Count > 0 then
      WriteLn(SignalStats[i].Name, ' : reçu ', SignalStats[i].Count, ' fois');
  end;

  WriteLn('Arrêt complet');
end.
```

## Créer un daemon (service) simple

Un daemon est un processus qui tourne en arrière-plan. Voici un exemple basique :

```pascal
program SimpleDaemon;

{$mode objfpc}{$H+}

uses
  BaseUnix, Unix, SysUtils;

var
  LogFile: TextFile;
  Running: Boolean = True;

procedure WriteLog(const Msg: string);
begin
  WriteLn(LogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' - ', Msg);
  Flush(LogFile);
end;

procedure SignalHandler(sig: cint); cdecl;
begin
  case sig of
    SIGTERM, SIGINT: begin
      WriteLog('Signal d''arrêt reçu');
      Running := False;
    end;
    SIGHUP: begin
      WriteLog('Signal SIGHUP - rechargement');
    end;
  end;
end;

procedure Daemonize;
var
  pid: TPid;
begin
  // Premier fork
  pid := FpFork;
  if pid < 0 then
  begin
    WriteLn('Erreur fork');
    Halt(1);
  end;
  if pid > 0 then
    Halt(0); // Le parent termine

  // Nouvelle session
  FpSetsid;

  // Changement de répertoire
  ChDir('/');

  // Fermeture des descripteurs standards
  FpClose(0); // stdin
  FpClose(1); // stdout
  FpClose(2); // stderr
end;

begin
  // Devenir un daemon
  Daemonize;

  // Ouvrir le fichier de log
  AssignFile(LogFile, '/tmp/simple_daemon.log');
  Rewrite(LogFile);
  WriteLog('Daemon démarré, PID: ' + IntToStr(FpGetPid));

  // Installer les gestionnaires de signaux
  FpSignal(SIGTERM, @SignalHandler);
  FpSignal(SIGINT, @SignalHandler);
  FpSignal(SIGHUP, @SignalHandler);

  // Boucle principale
  while Running do
  begin
    WriteLog('Daemon en activité...');
    Sleep(5000); // Attendre 5 secondes
  end;

  WriteLog('Daemon arrêté');
  CloseFile(LogFile);
end.
```

## Bonnes pratiques et conseils

### 1. Sécurité des gestionnaires de signaux

Les gestionnaires de signaux doivent être **simples et rapides**. Évitez :
- Les allocations mémoire complexes
- Les appels système non réentrants
- Les opérations longues

Utilisez plutôt des variables globales simples comme flags :

```pascal
var
  ReloadConfig: Boolean = False;

procedure SignalHandler(sig: cint); cdecl;
begin
  if sig = SIGHUP then
    ReloadConfig := True; // Simple affectation
end;

// Dans la boucle principale
if ReloadConfig then
begin
  ReloadConfig := False;
  // Faire le rechargement complexe ici, pas dans le handler
  DoComplexReload;
end;
```

### 2. Gestion des processus zombies

Un processus zombie est un processus enfant terminé dont le parent n'a pas encore lu le code de sortie. Pour les éviter :

```pascal
procedure ReaperHandler(sig: cint); cdecl;
var
  status: cint;
begin
  // Récupérer tous les enfants terminés
  while FpWaitPid(-1, @status, WNOHANG) > 0 do
    ; // Rien à faire, on libère juste les ressources
end;

// Dans le programme principal
FpSignal(SIGCHLD, @ReaperHandler);
```

### 3. Masquage temporaire de signaux

Parfois, vous devez exécuter du code critique sans interruption :

```pascal
var
  NewMask, OldMask: TSigSet;

begin
  // Bloquer temporairement SIGINT
  FpSigEmptySet(NewMask);
  FpSigAddSet(NewMask, SIGINT);
  FpSigProcMask(SIG_BLOCK, @NewMask, @OldMask);

  // Code critique ici
  WriteLn('Cette opération ne peut pas être interrompue...');
  Sleep(3000);

  // Restaurer le masque original
  FpSigProcMask(SIG_SETMASK, @OldMask, nil);
end;
```

## Différences avec Windows

Il est important de noter que ce système de signaux est spécifique à Unix/Linux. Sous Windows :
- Il n'y a pas de signaux Unix
- On utilise des événements Windows ou des messages
- `Ctrl+C` est géré différemment (SetConsoleCtrlHandler)
- Les services Windows ont leur propre système

Pour du code portable, utilisez la compilation conditionnelle :

```pascal
{$IFDEF UNIX}
  // Code pour Linux/Unix
  FpSignal(SIGTERM, @SignalHandler);
{$ENDIF}
{$IFDEF WINDOWS}
  // Code pour Windows
  SetConsoleCtrlHandler(@WindowsHandler, True);
{$ENDIF}
```

## Debugging et surveillance

Pour déboguer les signaux et processus :

1. **Voir les processus** : `ps aux | grep votre_programme`
2. **Envoyer un signal** : `kill -TERM 12345` (où 12345 est le PID)
3. **Voir les signaux disponibles** : `kill -l`
4. **Tracer les signaux** : `strace -e signal ./votre_programme`

## Conclusion

Les signaux Unix sont un mécanisme puissant pour :
- Contrôler l'exécution des programmes
- Permettre la communication inter-processus
- Gérer proprement l'arrêt des applications
- Implémenter des fonctionnalités avancées (rechargement à chaud, etc.)

Avec FreePascal/Lazarus, vous avez accès à toute la puissance du système Unix tout en gardant la simplicité de la programmation Pascal. Commencez par des exemples simples et progressez vers des architectures plus complexes selon vos besoins.

N'oubliez pas que la gestion des signaux est un domaine où les erreurs peuvent être subtiles. Testez toujours votre code dans différents scénarios et assurez-vous que vos programmes se comportent correctement même en cas de signaux inattendus.

⏭️ [X11 et Wayland](/07-specificites-linux-ubuntu/11-x11-wayland.md)
