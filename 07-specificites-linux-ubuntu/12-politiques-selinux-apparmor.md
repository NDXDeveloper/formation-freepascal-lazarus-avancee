🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Politiques SELinux et AppArmor avec FreePascal/Lazarus

## Introduction : Qu'est-ce que SELinux et AppArmor ?

Imaginez que votre système Linux est comme un immeuble de bureaux. Normalement, les permissions Unix classiques (lecture, écriture, exécution) sont comme des clés qui ouvrent certaines portes. Mais que se passe-t-il si quelqu'un copie une clé ou si un programme malveillant obtient des permissions qu'il ne devrait pas avoir ?

C'est là qu'interviennent **SELinux** et **AppArmor**. Ce sont des systèmes de sécurité supplémentaires qui agissent comme des gardes de sécurité dans notre immeuble. Ils vérifient non seulement si vous avez la clé, mais aussi :
- **Qui** vous êtes vraiment
- **Pourquoi** vous voulez accéder à cette pièce
- **Ce que** vous avez le droit d'y faire
- **Quand** vous pouvez y accéder

### Différence entre SELinux et AppArmor

- **SELinux** (Security-Enhanced Linux) : Développé par la NSA et Red Hat, utilisé principalement sur Fedora, RHEL, CentOS
- **AppArmor** : Développé par Novell/SUSE, utilisé principalement sur Ubuntu, Debian, SUSE

Les deux ont le même objectif (sécuriser votre système) mais utilisent des approches différentes. AppArmor est généralement considéré comme plus simple à configurer, tandis que SELinux offre un contrôle plus granulaire.

## Pourquoi est-ce important pour un développeur FreePascal/Lazarus ?

Vos applications FreePascal/Lazarus peuvent être bloquées par ces systèmes de sécurité si elles :
- Tentent d'accéder à des fichiers dans des répertoires protégés
- Essaient d'ouvrir des ports réseau
- Veulent exécuter d'autres programmes
- Ont besoin d'accès à des ressources système spéciales

Il est donc crucial de comprendre comment ces systèmes fonctionnent et comment configurer vos applications pour qu'elles fonctionnent correctement avec eux.

## Vérifier quel système est actif

Avant de commencer, vérifions quel système de sécurité est actif sur votre machine :

```pascal
program VerifierSecurite;

{$mode objfpc}{$H+}

uses
  SysUtils, Process, Classes;

function ExecuterCommande(const Cmd: string; const Args: array of string): string;  
var
  Proc: TProcess;
  Output: TStringList;
begin
  Result := '';
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Proc.Executable := Cmd;
    Proc.Parameters.AddStrings(Args);
    Proc.Options := [poUsePipes, poWaitOnExit];

    try
      Proc.Execute;
      Output.LoadFromStream(Proc.Output);
      Result := Trim(Output.Text);
    except
      on E: Exception do
        Result := 'Erreur: ' + E.Message;
    end;
  finally
    Output.Free;
    Proc.Free;
  end;
end;

procedure VerifierSELinux;  
var
  Status: string;
begin
  WriteLn('=== Vérification de SELinux ===');

  // Vérifier si SELinux est installé
  if FileExists('/etc/selinux/config') then
  begin
    WriteLn('SELinux est installé');

    // Vérifier le status
    if FileExists('/usr/sbin/getenforce') then
    begin
      Status := ExecuterCommande('/usr/sbin/getenforce', []);
      WriteLn('Status SELinux : ', Status);

      if Status = 'Enforcing' then
        WriteLn('  → SELinux est ACTIF et applique les règles')
      else if Status = 'Permissive' then
        WriteLn('  → SELinux enregistre les violations mais ne bloque pas')
      else if Status = 'Disabled' then
        WriteLn('  → SELinux est désactivé')
      else
        WriteLn('  → Status inconnu');
    end
    else
      WriteLn('Commande getenforce non trouvée');
  end
  else
    WriteLn('SELinux n''est pas installé sur ce système');

  WriteLn;
end;

procedure VerifierAppArmor;  
var
  Status: string;
  ProfileCount: Integer;
  Lines: TStringList;
begin
  WriteLn('=== Vérification d''AppArmor ===');

  // Vérifier si AppArmor est installé
  if DirectoryExists('/etc/apparmor.d') then
  begin
    WriteLn('AppArmor est installé');

    // Vérifier le status
    if FileExists('/usr/sbin/aa-status') then
    begin
      Status := ExecuterCommande('/usr/sbin/aa-status', ['--enabled']);
      if Pos('Yes', Status) > 0 then
        WriteLn('Status AppArmor : ACTIF')
      else if Pos('No', Status) > 0 then
        WriteLn('Status AppArmor : INACTIF')
      else
      begin
        // Essayer systemctl
        Status := ExecuterCommande('systemctl', ['is-active', 'apparmor']);
        WriteLn('Status AppArmor (systemctl) : ', Status);
      end;
    end
    else
      WriteLn('Commande aa-status non trouvée');

    // Compter les profils actifs
    if FileExists('/sys/kernel/security/apparmor/profiles') then
    begin
      Status := ExecuterCommande('cat', ['/sys/kernel/security/apparmor/profiles']);
      ProfileCount := 0;
      Lines := TStringList.Create;
      try
        Lines.Text := Status;
        ProfileCount := Lines.Count;
        WriteLn('Nombre de profils chargés : ', ProfileCount);
      finally
        Lines.Free;
      end;
    end;
  end
  else
    WriteLn('AppArmor n''est pas installé sur ce système');

  WriteLn;
end;

begin
  WriteLn('Détection des systèmes de sécurité Linux');
  WriteLn('=========================================');
  WriteLn;

  VerifierSELinux;
  VerifierAppArmor;

  WriteLn('Note : Les deux systèmes peuvent être installés mais');
  WriteLn('       généralement un seul est actif à la fois.');
end.
```

## AppArmor : Configuration pour vos applications

AppArmor utilise des **profils** qui définissent ce que chaque application peut faire. Ces profils sont des fichiers texte simples.

### Structure d'un profil AppArmor

Voici un exemple de profil pour une application FreePascal/Lazarus :

```pascal
program CreerProfilAppArmor;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

procedure CreerProfilExemple(const NomApp: string; const CheminApp: string);  
var
  Profil: TStringList;
  NomFichier: string;
begin
  Profil := TStringList.Create;
  try
    // En-tête du profil
    Profil.Add('#include <tunables/global>');
    Profil.Add('');
    Profil.Add(CheminApp + ' {');
    Profil.Add('  #include <abstractions/base>');
    Profil.Add('');

    // Permissions de base
    Profil.Add('  # Permissions pour l''exécutable lui-même');
    Profil.Add('  ' + CheminApp + ' mr,');
    Profil.Add('');

    // Bibliothèques système
    Profil.Add('  # Bibliothèques système nécessaires');
    Profil.Add('  /lib/x86_64-linux-gnu/lib*.so* mr,');
    Profil.Add('  /usr/lib/x86_64-linux-gnu/lib*.so* mr,');
    Profil.Add('');

    // Fichiers de configuration
    Profil.Add('  # Fichiers de configuration');
    Profil.Add('  /etc/ld.so.cache r,');
    Profil.Add('  /etc/nsswitch.conf r,');
    Profil.Add('  /etc/passwd r,');
    Profil.Add('  /etc/group r,');
    Profil.Add('');

    // Répertoire home de l''utilisateur
    Profil.Add('  # Accès au répertoire home');
    Profil.Add('  owner @{HOME}/ r,');
    Profil.Add('  owner @{HOME}/.config/' + NomApp + '/ rw,');
    Profil.Add('  owner @{HOME}/.config/' + NomApp + '/** rw,');
    Profil.Add('');

    // Fichiers temporaires
    Profil.Add('  # Fichiers temporaires');
    Profil.Add('  /tmp/ r,');
    Profil.Add('  owner /tmp/' + NomApp + '_* rw,');
    Profil.Add('');

    // Réseau (si nécessaire)
    Profil.Add('  # Réseau (décommentez si nécessaire)');
    Profil.Add('  # network inet stream,');
    Profil.Add('  # network inet dgram,');
    Profil.Add('');

    // Fin du profil
    Profil.Add('}');

    // Sauvegarder le profil
    NomFichier := NomApp + '.apparmor';
    Profil.SaveToFile(NomFichier);

    WriteLn('Profil AppArmor créé : ', NomFichier);
    WriteLn;
    WriteLn('Pour l''installer :');
    WriteLn('1. Copiez le fichier : sudo cp ', NomFichier, ' /etc/apparmor.d/', NomApp);
    WriteLn('2. Rechargez AppArmor : sudo systemctl reload apparmor');
    WriteLn('3. Activez le profil : sudo aa-enforce /etc/apparmor.d/', NomApp);
  finally
    Profil.Free;
  end;
end;

var
  AppName, AppPath: string;

begin
  WriteLn('Générateur de profil AppArmor pour applications FreePascal/Lazarus');
  WriteLn('==================================================================');
  WriteLn;

  Write('Nom de votre application : ');
  ReadLn(AppName);

  Write('Chemin complet de l''exécutable : ');
  ReadLn(AppPath);

  if (AppName <> '') and (AppPath <> '') then
  begin
    CreerProfilExemple(AppName, AppPath);
  end
  else
    WriteLn('Informations manquantes');
end.
```

### Tester et déboguer avec AppArmor

```pascal
program TesterAppArmor;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils;

type
  TTestResult = record
    TestName: string;
    Success: Boolean;
    Error: string;
  end;

var
  TestResults: array of TTestResult;

procedure AjouterResultat(const Nom: string; Succes: Boolean; const Erreur: string = '');  
begin
  SetLength(TestResults, Length(TestResults) + 1);
  with TestResults[High(TestResults)] do
  begin
    TestName := Nom;
    Success := Succes;
    Error := Erreur;
  end;
end;

procedure TesterLecture(const Fichier: string);  
var
  F: TextFile;
  Ligne: string;
begin
  try
    AssignFile(F, Fichier);
    Reset(F);
    ReadLn(F, Ligne);
    CloseFile(F);
    AjouterResultat('Lecture ' + Fichier, True);
  except
    on E: Exception do
      AjouterResultat('Lecture ' + Fichier, False, E.Message);
  end;
end;

procedure TesterEcriture(const Fichier: string);  
var
  F: TextFile;
begin
  try
    AssignFile(F, Fichier);
    Rewrite(F);
    WriteLn(F, 'Test écriture : ', DateTimeToStr(Now));
    CloseFile(F);
    DeleteFile(Fichier);
    AjouterResultat('Écriture ' + Fichier, True);
  except
    on E: Exception do
      AjouterResultat('Écriture ' + Fichier, False, E.Message);
  end;
end;

procedure TesterReseau;  
var
  Reussi: Boolean;
begin
  // Test simple de résolution DNS
  try
    // Ici on pourrait tester une connexion réseau
    Reussi := True; // Simulation
    AjouterResultat('Accès réseau', Reussi);
  except
    on E: Exception do
      AjouterResultat('Accès réseau', False, E.Message);
  end;
end;

procedure AfficherRapport;  
var
  i: Integer;
  NbSucces, NbEchecs: Integer;
begin
  WriteLn;
  WriteLn('=== Rapport des tests de permissions ===');
  WriteLn;

  NbSucces := 0;
  NbEchecs := 0;

  for i := 0 to High(TestResults) do
  begin
    with TestResults[i] do
    begin
      if Success then
      begin
        WriteLn('[✓] ', TestName);
        Inc(NbSucces);
      end
      else
      begin
        WriteLn('[✗] ', TestName);
        if Error <> '' then
          WriteLn('    Erreur : ', Error);
        Inc(NbEchecs);
      end;
    end;
  end;

  WriteLn;
  WriteLn('Résumé : ', NbSucces, ' succès, ', NbEchecs, ' échecs');

  if NbEchecs > 0 then
  begin
    WriteLn;
    WriteLn('Si des tests ont échoué, vérifiez :');
    WriteLn('1. Les logs AppArmor : sudo journalctl -xe | grep apparmor');
    WriteLn('2. Le mode du profil : sudo aa-status');
    WriteLn('3. Passez en mode complain pour déboguer : sudo aa-complain /path/to/app');
  end;
end;

begin
  WriteLn('Test des permissions avec AppArmor');
  WriteLn('===================================');
  WriteLn;
  WriteLn('Ce programme teste différents accès pour vérifier');
  WriteLn('les restrictions AppArmor.');
  WriteLn;

  // Tests de lecture
  TesterLecture('/etc/passwd');
  TesterLecture('/etc/shadow'); // Devrait échouer
  TesterLecture('/proc/version');

  // Tests d'écriture
  TesterEcriture('/tmp/test_apparmor.txt');
  TesterEcriture('/etc/test.txt'); // Devrait échouer
  TesterEcriture(GetEnvironmentVariable('HOME') + '/test_apparmor.txt');

  // Test réseau
  TesterReseau;

  // Afficher le rapport
  AfficherRapport;
end.
```

## SELinux : Configuration pour vos applications

SELinux utilise un système de **contextes** et de **politiques**. Chaque fichier, processus et ressource a un contexte de sécurité.

### Comprendre les contextes SELinux

```pascal
program ComprendreSELinux;

{$mode objfpc}{$H+}

uses
  SysUtils, Process, Classes;

function ExecuterCommande(const Cmd: string; const Args: array of string): string;  
var
  Proc: TProcess;
  Output: TStringList;
begin
  Result := '';
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Proc.Executable := Cmd;
    Proc.Parameters.AddStrings(Args);
    Proc.Options := [poUsePipes, poWaitOnExit];

    try
      Proc.Execute;
      Output.LoadFromStream(Proc.Output);
      Result := Output.Text;
    except
      Result := 'Commande non disponible';
    end;
  finally
    Output.Free;
    Proc.Free;
  end;
end;

procedure AfficherContexte(const Chemin: string);  
var
  Contexte: string;
begin
  Write(Chemin, ' : ');

  if not FileExists('/usr/bin/ls') then
  begin
    WriteLn('ls non disponible');
    Exit;
  end;

  Contexte := ExecuterCommande('ls', ['-Z', Chemin]);
  WriteLn(Contexte);
end;

procedure ExpliquContexte;  
begin
  WriteLn('=== Structure d''un contexte SELinux ===');
  WriteLn;
  WriteLn('Format : user:role:type:level');
  WriteLn;
  WriteLn('Exemple : unconfined_u:object_r:user_home_t:s0');
  WriteLn('  - user (unconfined_u) : L''utilisateur SELinux');
  WriteLn('  - role (object_r) : Le rôle (object_r pour les fichiers)');
  WriteLn('  - type (user_home_t) : Le type (le plus important)');
  WriteLn('  - level (s0) : Le niveau de sécurité (MLS/MCS)');
  WriteLn;
end;

procedure VerifierContexteProcessus;  
var
  Contexte: string;
  F: TextFile;
begin
  WriteLn('=== Contexte du processus actuel ===');

  if FileExists('/proc/self/attr/current') then
  begin
    try
      AssignFile(F, '/proc/self/attr/current');
      Reset(F);
      ReadLn(F, Contexte);
      CloseFile(F);
      WriteLn('Contexte : ', Contexte);
    except
      WriteLn('Impossible de lire le contexte');
    end;
  end
  else
    WriteLn('SELinux ne semble pas actif');

  WriteLn;
end;

var
  Status: string;

begin
  WriteLn('Comprendre SELinux pour les développeurs');
  WriteLn('========================================');
  WriteLn;

  // Vérifier si SELinux est actif
  Status := ExecuterCommande('/usr/sbin/getenforce', []);
  WriteLn('Status SELinux : ', Trim(Status));
  WriteLn;

  if Pos('Disabled', Status) = 0 then
  begin
    ExpliquContexte;

    WriteLn('=== Contextes de fichiers importants ===');
    AfficherContexte('/etc/passwd');
    AfficherContexte('/tmp');
    AfficherContexte(GetEnvironmentVariable('HOME'));
    WriteLn;

    VerifierContexteProcessus;

    WriteLn('=== Commandes SELinux utiles ===');
    WriteLn('- getenforce : Voir le mode actuel');
    WriteLn('- setenforce 0/1 : Passer en permissive/enforcing (temporaire)');
    WriteLn('- ls -Z : Voir les contextes des fichiers');
    WriteLn('- ps -Z : Voir les contextes des processus');
    WriteLn('- chcon : Changer le contexte d''un fichier');
    WriteLn('- restorecon : Restaurer le contexte par défaut');
    WriteLn('- ausearch -m avc : Voir les violations SELinux');
  end
  else
  begin
    WriteLn('SELinux est désactivé sur ce système');
  end;
end.
```

### Créer une politique SELinux pour votre application

```pascal
program CreerPolitiqueSELinux;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

procedure CreerModuleSELinux(const NomApp: string);  
var
  TeFile, FcFile, IfFile: TStringList;
begin
  // Fichier .te (Type Enforcement)
  TeFile := TStringList.Create;
  FcFile := TStringList.Create;
  IfFile := TStringList.Create;

  try
    // === Fichier .te ===
    TeFile.Add('policy_module(' + NomApp + ', 1.0.0)');
    TeFile.Add('');
    TeFile.Add('########################################');
    TeFile.Add('# Déclarations');
    TeFile.Add('########################################');
    TeFile.Add('');
    TeFile.Add('type ' + NomApp + '_t;');
    TeFile.Add('type ' + NomApp + '_exec_t;');
    TeFile.Add('application_domain(' + NomApp + '_t, ' + NomApp + '_exec_t)');
    TeFile.Add('');
    TeFile.Add('type ' + NomApp + '_conf_t;');
    TeFile.Add('files_config_file(' + NomApp + '_conf_t)');
    TeFile.Add('');
    TeFile.Add('type ' + NomApp + '_log_t;');
    TeFile.Add('logging_log_file(' + NomApp + '_log_t)');
    TeFile.Add('');
    TeFile.Add('########################################');
    TeFile.Add('# Règles pour ' + NomApp);
    TeFile.Add('########################################');
    TeFile.Add('');
    TeFile.Add('# Permettre l''exécution');
    TeFile.Add('allow ' + NomApp + '_t ' + NomApp + '_exec_t:file execute_no_trans;');
    TeFile.Add('');
    TeFile.Add('# Accès aux fichiers de configuration');
    TeFile.Add('allow ' + NomApp + '_t ' + NomApp + '_conf_t:dir list_dir_perms;');
    TeFile.Add('allow ' + NomApp + '_t ' + NomApp + '_conf_t:file read_file_perms;');
    TeFile.Add('');
    TeFile.Add('# Accès aux logs');
    TeFile.Add('allow ' + NomApp + '_t ' + NomApp + '_log_t:dir add_entry_dir_perms;');
    TeFile.Add('allow ' + NomApp + '_t ' + NomApp + '_log_t:file { create_file_perms append_file_perms };');
    TeFile.Add('');
    TeFile.Add('# Accès réseau (si nécessaire)');
    TeFile.Add('# corenet_tcp_bind_generic_node(' + NomApp + '_t)');
    TeFile.Add('# corenet_tcp_bind_generic_port(' + NomApp + '_t)');
    TeFile.Add('');
    TeFile.Add('# Accès aux bibliothèques système');
    TeFile.Add('libs_use_ld_so(' + NomApp + '_t)');
    TeFile.Add('libs_use_shared_libs(' + NomApp + '_t)');

    TeFile.SaveToFile(NomApp + '.te');
    WriteLn('Fichier créé : ', NomApp, '.te');

    // === Fichier .fc (File Context) ===
    FcFile.Add('# Contextes de fichiers pour ' + NomApp);
    FcFile.Add('/usr/bin/' + NomApp + ' -- gen_context(system_u:object_r:' + NomApp + '_exec_t,s0)');
    FcFile.Add('/etc/' + NomApp + '(/.*)? gen_context(system_u:object_r:' + NomApp + '_conf_t,s0)');
    FcFile.Add('/var/log/' + NomApp + '(/.*)? gen_context(system_u:object_r:' + NomApp + '_log_t,s0)');

    FcFile.SaveToFile(NomApp + '.fc');
    WriteLn('Fichier créé : ', NomApp, '.fc');

    // === Fichier .if (Interface) ===
    IfFile.Add('## <summary>Politique pour ' + NomApp + '</summary>');
    IfFile.Add('');
    IfFile.Add('########################################');
    IfFile.Add('## <summary>');
    IfFile.Add('## Exécuter ' + NomApp + ' dans le domaine ' + NomApp + '.');
    IfFile.Add('## </summary>');
    IfFile.Add('## <param name="domain">');
    IfFile.Add('## <summary>');
    IfFile.Add('## Domaine autorisé à transitionner.');
    IfFile.Add('## </summary>');
    IfFile.Add('## </param>');
    IfFile.Add('#');
    IfFile.Add('interface(`' + NomApp + '_domtrans'',`');
    IfFile.Add('	gen_require(`');
    IfFile.Add('		type ' + NomApp + '_t, ' + NomApp + '_exec_t;');
    IfFile.Add('	'')');
    IfFile.Add('');
    IfFile.Add('	corecmd_search_bin($1)');
    IfFile.Add('	domtrans_pattern($1, ' + NomApp + '_exec_t, ' + NomApp + '_t)');
    IfFile.Add(''')');

    IfFile.SaveToFile(NomApp + '.if');
    WriteLn('Fichier créé : ', NomApp, '.if');

    WriteLn;
    WriteLn('=== Instructions pour compiler et installer ===');
    WriteLn('1. Compiler le module : make -f /usr/share/selinux/devel/Makefile');
    WriteLn('2. Installer : sudo semodule -i ', NomApp, '.pp');
    WriteLn('3. Appliquer les contextes : sudo restorecon -Rv /usr/bin/', NomApp);
    WriteLn('4. Vérifier : sudo semodule -l | grep ', NomApp);

  finally
    TeFile.Free;
    FcFile.Free;
    IfFile.Free;
  end;
end;

var
  AppName: string;

begin
  WriteLn('Générateur de politique SELinux pour FreePascal/Lazarus');
  WriteLn('========================================================');
  WriteLn;

  Write('Nom de votre application : ');
  ReadLn(AppName);

  if AppName <> '' then
  begin
    CreerModuleSELinux(AppName);
  end
  else
    WriteLn('Nom d''application requis');
end.
```

## Gestion des violations et débogage

### Analyser les violations AppArmor

```pascal
program AnalyserViolationsAppArmor;

{$mode objfpc}{$H+}

uses
  SysUtils, Process, Classes, RegExpr;

type
  TViolation = record
    Timestamp: TDateTime;
    Profile: string;
    Operation: string;
    Path: string;
    Denied: Boolean;
  end;

var
  Violations: array of TViolation;

procedure AnalyserLogs;  
var
  Proc: TProcess;
  Output: TStringList;
  i: Integer;
  RegEx: TRegExpr;
  V: TViolation;
begin
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;
  RegEx := TRegExpr.Create;

  try
    // Lire les logs système
    Proc.Executable := 'journalctl';
    Proc.Parameters.Add('-xe');
    Proc.Parameters.Add('--grep=apparmor');
    Proc.Parameters.Add('--since=today');
    Proc.Options := [poUsePipes, poWaitOnExit];

    WriteLn('Lecture des logs AppArmor...');

    try
      Proc.Execute;
      Output.LoadFromStream(Proc.Output);

      // Analyser chaque ligne
      RegEx.Expression := 'apparmor="([^"]*)".*operation="([^"]*)".*name="([^"]*)"';

      for i := 0 to Output.Count - 1 do
      begin
        if RegEx.Exec(Output[i]) then
        begin
          V.Denied := RegEx.Match[1] = 'DENIED';
          V.Operation := RegEx.Match[2];
          V.Path := RegEx.Match[3];
          V.Timestamp := Now; // Simplification

          SetLength(Violations, Length(Violations) + 1);
          Violations[High(Violations)] := V;
        end;
      end;

    except
      on E: Exception do
        WriteLn('Erreur lors de la lecture des logs : ', E.Message);
    end;

  finally
    RegEx.Free;
    Output.Free;
    Proc.Free;
  end;
end;

procedure AfficherRapport;  
var
  i: Integer;
  Start: Integer;
  Operations: TStringList;
begin
  if Length(Violations) = 0 then
  begin
    WriteLn('Aucune violation AppArmor détectée aujourd''hui');
    Exit;
  end;

  WriteLn;
  WriteLn('=== Violations AppArmor détectées ===');
  WriteLn('Total : ', Length(Violations), ' violations');
  WriteLn;

  Operations := TStringList.Create;
  try
    // Grouper par opération
    for i := 0 to High(Violations) do
    begin
      if Operations.IndexOf(Violations[i].Operation) = -1 then
        Operations.Add(Violations[i].Operation);
    end;

    WriteLn('Opérations bloquées :');
    for i := 0 to Operations.Count - 1 do
      WriteLn('  - ', Operations[i]);

  finally
    Operations.Free;
  end;

  WriteLn;
  WriteLn('Détails des violations (10 dernières) :');
  WriteLn('----------------------------------------');

  Start := Max(0, Length(Violations) - 10);
  for i := Start to High(Violations) do
  begin
    with Violations[i] do
    begin
      WriteLn('Opération : ', Operation);
      WriteLn('Fichier   : ', Path);
      WriteLn('Status    : ', IfThen(Denied, 'REFUSÉ', 'Permis'));
      WriteLn('---');
    end;
  end;
end;

procedure GenererSuggestions;  
var
  i: Integer;
  Perms: string;
  Chemins: TStringList;
begin
  if Length(Violations) = 0 then Exit;

  WriteLn;
  WriteLn('=== Suggestions pour corriger le profil ===');
  WriteLn;

  Chemins := TStringList.Create;
  try
    Chemins.Duplicates := dupIgnore;

    // Collecter les chemins uniques
    for i := 0 to High(Violations) do
    begin
      if Violations[i].Denied then
        Chemins.Add(Violations[i].Path);
    end;

    WriteLn('Ajoutez ces lignes à votre profil AppArmor :');
    WriteLn;

    for i := 0 to Chemins.Count - 1 do
    begin
      // Déterminer les permissions nécessaires
      Perms := 'r'; // Lecture par défaut

      // Vérifier si c'est un fichier de config
      if Pos('/etc/', Chemins[i]) = 1 then
        Perms := 'r'
      // Fichiers temporaires
      else if Pos('/tmp/', Chemins[i]) = 1 then
        Perms := 'rw'
      // Répertoire home
      else if Pos('/home/', Chemins[i]) = 1 then
        Perms := 'rw';

      WriteLn('  ', Chemins[i], ' ', Perms, ',');
    end;

  finally
    Chemins.Free;
  end;
end;

begin
  WriteLn('Analyseur de violations AppArmor');
  WriteLn('================================');
  WriteLn;

  AnalyserLogs;
  AfficherRapport;
  GenererSuggestions;

  WriteLn;
  WriteLn('Commandes utiles :');
  WriteLn('- Mode complain (logs sans bloquer) : sudo aa-complain /path/to/app');
  WriteLn('- Mode enforce (applique les règles) : sudo aa-enforce /path/to/app');
  WriteLn('- Recharger un profil : sudo apparmor_parser -r /etc/apparmor.d/profile');
end.
```

### Analyser les violations SELinux

```pascal
program AnalyserViolationsSELinux;

{$mode objfpc}{$H+}

uses
  SysUtils, Process, Classes, DateUtils;

type
  TAVCMessage = record
    Timestamp: TDateTime;
    SourceContext: string;
    TargetContext: string;
    TargetClass: string;
    Permission: string;
    Path: string;
    Comm: string; // Nom de la commande
    Pid: Integer;
  end;

var
  AVCMessages: array of TAVCMessage;

procedure LireAuditLog;  
var
  Proc: TProcess;
  Output: TStringList;
  i: Integer;
  Ligne: string;
  AVC: TAVCMessage;
  PosStart, PosEnd: Integer;
begin
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Utiliser ausearch pour chercher les messages AVC
    Proc.Executable := 'ausearch';
    Proc.Parameters.Add('-m');
    Proc.Parameters.Add('avc');
    Proc.Parameters.Add('--start');
    Proc.Parameters.Add('today');
    Proc.Options := [poUsePipes, poWaitOnExit];

    WriteLn('Recherche des violations SELinux...');

    try
      Proc.Execute;
      Output.LoadFromStream(Proc.Output);

      for i := 0 to Output.Count - 1 do
      begin
        Ligne := Output[i];

        // Parser les messages AVC (simplifié)
        if Pos('type=AVC', Ligne) > 0 then
        begin
          // Extraire les informations
          AVC.Timestamp := Now; // Simplification

          // Extraire la permission
          PosStart := Pos('denied', Ligne);
          if PosStart > 0 then
          begin
            PosStart := Pos('{', Ligne, PosStart);
            PosEnd := Pos('}', Ligne, PosStart);
            if (PosStart > 0) and (PosEnd > PosStart) then
              AVC.Permission := Copy(Ligne, PosStart + 1, PosEnd - PosStart - 1);
          end;

          // Extraire comm (nom du programme)
          PosStart := Pos('comm="', Ligne);
          if PosStart > 0 then
          begin
            PosStart := PosStart + 6;
            PosEnd := Pos('"', Ligne, PosStart);
            if PosEnd > PosStart then
              AVC.Comm := Copy(Ligne, PosStart, PosEnd - PosStart);
          end;

          // Extraire le path
          PosStart := Pos('path="', Ligne);
          if PosStart > 0 then
          begin
            PosStart := PosStart + 6;
            PosEnd := Pos('"', Ligne, PosStart);
            if PosEnd > PosStart then
              AVC.Path := Copy(Ligne, PosStart, PosEnd - PosStart);
          end;

          SetLength(AVCMessages, Length(AVCMessages) + 1);
          AVCMessages[High(AVCMessages)] := AVC;
        end;
      end;

    except
      on E: Exception do
      begin
        WriteLn('Erreur : ', E.Message);
        WriteLn('Note : ausearch nécessite les privilèges root');
        WriteLn('Essayez : sudo ', ParamStr(0));
      end;
    end;

  finally
    Output.Free;
    Proc.Free;
  end;
end;

procedure GenererPolitique;  
var
  i: Integer;
  Commandes: TStringList;
begin
  if Length(AVCMessages) = 0 then
  begin
    WriteLn('Aucune violation SELinux trouvée');
    Exit;
  end;

  WriteLn;
  WriteLn('=== Génération de règles SELinux ===');
  WriteLn;

  Commandes := TStringList.Create;
  try
    Commandes.Duplicates := dupIgnore;

    for i := 0 to High(AVCMessages) do
    begin
      if AVCMessages[i].Comm <> '' then
        Commandes.Add(AVCMessages[i].Comm);
    end;

    WriteLn('Programmes affectés :');
    for i := 0 to Commandes.Count - 1 do
      WriteLn('  - ', Commandes[i]);

    WriteLn;
    WriteLn('Pour générer une politique permettant ces actions :');
    WriteLn('1. Passez en mode permissive : sudo setenforce 0');
    WriteLn('2. Exécutez votre application');
    WriteLn('3. Générez la politique : sudo audit2allow -a -M monapp');
    WriteLn('4. Installez : sudo semodule -i monapp.pp');
    WriteLn('5. Repassez en mode enforcing : sudo setenforce 1');

  finally
    Commandes.Free;
  end;
end;

var
  Proc: TProcess;
  Status: string;
  Len: Integer;

begin
  WriteLn('Analyseur de violations SELinux');
  WriteLn('===============================');
  WriteLn;

  // Vérifier si SELinux est actif
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'getenforce';
    Proc.Options := [poUsePipes, poWaitOnExit];

    try
      Proc.Execute;
      SetLength(Status, 100);
      Len := Proc.Output.Read(Status[1], 100);
      SetLength(Status, Len);

      WriteLn('Status SELinux : ', Trim(Status));
      WriteLn;

      if Pos('Disabled', Status) > 0 then
      begin
        WriteLn('SELinux est désactivé');
        Exit;
      end;

    except
      WriteLn('getenforce non trouvé - SELinux non installé ?');
      Exit;
    end;

  finally
    Proc.Free;
  end;

  LireAuditLog;
  GenererPolitique;
end.
```

## Intégration dans vos applications FreePascal/Lazarus

### Détection et adaptation automatique

```pascal
program AdaptationSecurite;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs;

type
  TSecuritySystem = (ssNone, ssAppArmor, ssSELinux);

  TSecurityAwareForm = class(TForm)
    MemoLog: TMemo;
    ButtonCheckSecurity: TButton;
    ButtonAdaptBehavior: TButton;
    LabelStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonCheckSecurityClick(Sender: TObject);
    procedure ButtonAdaptBehaviorClick(Sender: TObject);
  private
    FSecuritySystem: TSecuritySystem;
    FIsRestricted: Boolean;
    procedure DetectSecuritySystem;
    procedure AdaptApplicationBehavior;
    procedure LogMessage(const Msg: string);
    function TryWriteFile(const Path: string): Boolean;
  end;

var
  SecurityAwareForm: TSecurityAwareForm;

procedure TSecurityAwareForm.FormCreate(Sender: TObject);  
begin
  Caption := 'Application consciente de la sécurité';
  Width := 600;
  Height := 400;
  Position := poScreenCenter;

  LabelStatus := TLabel.Create(Self);
  LabelStatus.Parent := Self;
  LabelStatus.Left := 10;
  LabelStatus.Top := 10;
  LabelStatus.Caption := 'Système de sécurité : Non détecté';
  LabelStatus.Font.Style := [fsBold];

  ButtonCheckSecurity := TButton.Create(Self);
  ButtonCheckSecurity.Parent := Self;
  ButtonCheckSecurity.Left := 10;
  ButtonCheckSecurity.Top := 40;
  ButtonCheckSecurity.Width := 150;
  ButtonCheckSecurity.Caption := 'Vérifier la sécurité';
  ButtonCheckSecurity.OnClick := @ButtonCheckSecurityClick;

  ButtonAdaptBehavior := TButton.Create(Self);
  ButtonAdaptBehavior.Parent := Self;
  ButtonAdaptBehavior.Left := 170;
  ButtonAdaptBehavior.Top := 40;
  ButtonAdaptBehavior.Width := 150;
  ButtonAdaptBehavior.Caption := 'Adapter le comportement';
  ButtonAdaptBehavior.OnClick := @ButtonAdaptBehaviorClick;

  MemoLog := TMemo.Create(Self);
  MemoLog.Parent := Self;
  MemoLog.Left := 10;
  MemoLog.Top := 80;
  MemoLog.Width := 580;
  MemoLog.Height := 300;
  MemoLog.ScrollBars := ssVertical;
  MemoLog.ReadOnly := True;

  DetectSecuritySystem;
end;

procedure TSecurityAwareForm.DetectSecuritySystem;  
var
  F: TextFile;
  Profile: string;
  Context: string;
begin
  FSecuritySystem := ssNone;
  FIsRestricted := False;

  // Vérifier AppArmor
  if DirectoryExists('/etc/apparmor.d') then
  begin
    if FileExists('/sys/kernel/security/apparmor/profiles') then
    begin
      FSecuritySystem := ssAppArmor;
      LogMessage('AppArmor détecté');

      // Vérifier si notre processus est confiné
      try
        AssignFile(F, '/proc/self/attr/current');
        Reset(F);
        ReadLn(F, Profile);
        CloseFile(F);

        if Pos('unconfined', Profile) = 0 then
        begin
          FIsRestricted := True;
          LogMessage('Application confinée par AppArmor : ' + Profile);
        end
        else
          LogMessage('Application non confinée (unconfined)');

      except
        // Ignore
      end;
    end;
  end
  // Vérifier SELinux
  else if FileExists('/etc/selinux/config') then
  begin
    FSecuritySystem := ssSELinux;
    LogMessage('SELinux détecté');

    // Vérifier le contexte
    try
      AssignFile(F, '/proc/self/attr/current');
      Reset(F);
      ReadLn(F, Context);
      CloseFile(F);

      if Pos('unconfined', Context) = 0 then
      begin
        FIsRestricted := True;
        LogMessage('Contexte SELinux : ' + Context);
      end;

    except
      // Ignore
    end;
  end
  else
  begin
    LogMessage('Aucun système de sécurité MAC détecté');
  end;

  // Mettre à jour le label
  case FSecuritySystem of
    ssNone: LabelStatus.Caption := 'Système de sécurité : Aucun';
    ssAppArmor: LabelStatus.Caption := 'Système de sécurité : AppArmor';
    ssSELinux: LabelStatus.Caption := 'Système de sécurité : SELinux';
  end;

  if FIsRestricted then
    LabelStatus.Caption := LabelStatus.Caption + ' (RESTREINT)'
  else if FSecuritySystem <> ssNone then
    LabelStatus.Caption := LabelStatus.Caption + ' (non restreint)';
end;

procedure TSecurityAwareForm.AdaptApplicationBehavior;  
var
  ConfigPath, LogPath, TempPath: string;
begin
  LogMessage('');
  LogMessage('=== Adaptation du comportement ===');

  // Déterminer les chemins appropriés selon le système
  case FSecuritySystem of
    ssNone:
    begin
      // Pas de restrictions particulières
      ConfigPath := GetEnvironmentVariable('HOME') + '/.config/monapp/';
      LogPath := GetEnvironmentVariable('HOME') + '/.local/share/monapp/';
      TempPath := '/tmp/';
    end;

    ssAppArmor:
    begin
      if FIsRestricted then
      begin
        // Utiliser des chemins qui sont probablement autorisés
        ConfigPath := GetEnvironmentVariable('HOME') + '/.config/';
        LogPath := '/tmp/';
        TempPath := '/tmp/';
        LogMessage('Mode restreint AppArmor - utilisation de chemins sûrs');
      end
      else
      begin
        ConfigPath := GetEnvironmentVariable('HOME') + '/.config/monapp/';
        LogPath := GetEnvironmentVariable('HOME') + '/.local/share/monapp/';
        TempPath := '/tmp/';
      end;
    end;

    ssSELinux:
    begin
      if FIsRestricted then
      begin
        // SELinux : utiliser les contextes appropriés
        ConfigPath := GetEnvironmentVariable('HOME') + '/';
        LogPath := '/tmp/';
        TempPath := '/tmp/';
        LogMessage('Mode SELinux - utilisation de chemins avec contextes appropriés');
      end
      else
      begin
        ConfigPath := GetEnvironmentVariable('HOME') + '/.config/monapp/';
        LogPath := GetEnvironmentVariable('HOME') + '/.local/share/monapp/';
        TempPath := '/tmp/';
      end;
    end;
  end;

  // Tester les accès
  LogMessage('');
  LogMessage('Test des accès :');

  // Test écriture config
  if TryWriteFile(ConfigPath + 'test.txt') then
    LogMessage('✓ Configuration : ' + ConfigPath)
  else
    LogMessage('✗ Configuration : ' + ConfigPath + ' (accès refusé)');

  // Test écriture logs
  if TryWriteFile(LogPath + 'test.log') then
    LogMessage('✓ Logs : ' + LogPath)
  else
    LogMessage('✗ Logs : ' + LogPath + ' (accès refusé)');

  // Test écriture temporaire
  if TryWriteFile(TempPath + 'monapp_' + IntToStr(GetProcessID) + '.tmp') then
    LogMessage('✓ Temporaire : ' + TempPath)
  else
    LogMessage('✗ Temporaire : ' + TempPath + ' (accès refusé)');

  LogMessage('');
  LogMessage('Adaptation terminée');
end;

function TSecurityAwareForm.TryWriteFile(const Path: string): Boolean;  
var
  F: TextFile;
begin
  Result := False;
  try
    // Créer le répertoire si nécessaire
    ForceDirectories(ExtractFileDir(Path));

    // Essayer d'écrire
    AssignFile(F, Path);
    Rewrite(F);
    WriteLn(F, 'Test écriture');
    CloseFile(F);

    // Nettoyer
    DeleteFile(Path);

    Result := True;
  except
    // Accès refusé
    Result := False;
  end;
end;

procedure TSecurityAwareForm.LogMessage(const Msg: string);  
begin
  MemoLog.Lines.Add(FormatDateTime('[hh:nn:ss] ', Now) + Msg);
end;

procedure TSecurityAwareForm.ButtonCheckSecurityClick(Sender: TObject);  
begin
  DetectSecuritySystem;
end;

procedure TSecurityAwareForm.ButtonAdaptBehaviorClick(Sender: TObject);  
begin
  AdaptApplicationBehavior;
end;

begin
  Application.Initialize;
  Application.CreateForm(TSecurityAwareForm, SecurityAwareForm);
  Application.Run;
end.
```

## Création d'un installateur compatible

### Script d'installation avec support SELinux/AppArmor

```pascal
program CreerInstallateur;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

procedure CreerScriptInstallation(const NomApp: string);  
var
  Script: TStringList;
begin
  Script := TStringList.Create;
  try
    Script.Add('#!/bin/bash');
    Script.Add('');
    Script.Add('# Script d''installation pour ' + NomApp);
    Script.Add('# Compatible SELinux et AppArmor');
    Script.Add('');
    Script.Add('APP_NAME="' + NomApp + '"');
    Script.Add('APP_PATH="/usr/local/bin/$APP_NAME"');
    Script.Add('CONFIG_PATH="/etc/$APP_NAME"');
    Script.Add('');
    Script.Add('# Vérifier les privilèges root');
    Script.Add('if [ "$EUID" -ne 0 ]; then');
    Script.Add('    echo "Ce script doit être exécuté en tant que root"');
    Script.Add('    exit 1');
    Script.Add('fi');
    Script.Add('');
    Script.Add('echo "Installation de $APP_NAME..."');
    Script.Add('');
    Script.Add('# Copier l''exécutable');
    Script.Add('cp $APP_NAME $APP_PATH');
    Script.Add('chmod 755 $APP_PATH');
    Script.Add('');
    Script.Add('# Créer les répertoires de configuration');
    Script.Add('mkdir -p $CONFIG_PATH');
    Script.Add('mkdir -p /var/log/$APP_NAME');
    Script.Add('');
    Script.Add('# Détecter et configurer SELinux');
    Script.Add('if command -v getenforce &> /dev/null; then');
    Script.Add('    SELINUX_STATUS=$(getenforce)');
    Script.Add('    if [ "$SELINUX_STATUS" != "Disabled" ]; then');
    Script.Add('        echo "Configuration de SELinux..."');
    Script.Add('        ');
    Script.Add('        # Définir le contexte pour l''exécutable');
    Script.Add('        chcon -t bin_t $APP_PATH');
    Script.Add('        ');
    Script.Add('        # Définir les contextes pour les répertoires');
    Script.Add('        semanage fcontext -a -t etc_t "$CONFIG_PATH(/.*)?"');
    Script.Add('        restorecon -Rv $CONFIG_PATH');
    Script.Add('        ');
    Script.Add('        semanage fcontext -a -t var_log_t "/var/log/$APP_NAME(/.*)?"');
    Script.Add('        restorecon -Rv /var/log/$APP_NAME');
    Script.Add('        ');
    Script.Add('        # Créer une politique si elle n''existe pas');
    Script.Add('        if [ -f "${APP_NAME}.pp" ]; then');
    Script.Add('            echo "Installation de la politique SELinux..."');
    Script.Add('            semodule -i ${APP_NAME}.pp');
    Script.Add('        fi');
    Script.Add('    fi');
    Script.Add('fi');
    Script.Add('');
    Script.Add('# Détecter et configurer AppArmor');
    Script.Add('if command -v aa-status &> /dev/null; then');
    Script.Add('    if systemctl is-active --quiet apparmor; then');
    Script.Add('        echo "Configuration d''AppArmor..."');
    Script.Add('        ');
    Script.Add('        # Copier le profil AppArmor s''il existe');
    Script.Add('        if [ -f "${APP_NAME}.apparmor" ]; then');
    Script.Add('            cp ${APP_NAME}.apparmor /etc/apparmor.d/${APP_NAME}');
    Script.Add('            ');
    Script.Add('            # Recharger AppArmor');
    Script.Add('            apparmor_parser -r /etc/apparmor.d/${APP_NAME}');
    Script.Add('            ');
    Script.Add('            # Mettre en mode complain pour commencer');
    Script.Add('            aa-complain $APP_PATH');
    Script.Add('            echo "Profil AppArmor installé en mode complain"');
    Script.Add('            echo "Pour activer : sudo aa-enforce $APP_PATH"');
    Script.Add('        fi');
    Script.Add('    fi');
    Script.Add('fi');
    Script.Add('');
    Script.Add('# Créer un utilisateur système (optionnel)');
    Script.Add('if ! id -u $APP_NAME &> /dev/null; then');
    Script.Add('    useradd -r -s /bin/false -d /var/lib/$APP_NAME $APP_NAME');
    Script.Add('    mkdir -p /var/lib/$APP_NAME');
    Script.Add('    chown -R $APP_NAME:$APP_NAME /var/lib/$APP_NAME');
    Script.Add('    chown -R $APP_NAME:$APP_NAME /var/log/$APP_NAME');
    Script.Add('fi');
    Script.Add('');
    Script.Add('echo "Installation terminée !"');
    Script.Add('echo ""');
    Script.Add('echo "Pour tester :"');
    Script.Add('echo "  $APP_PATH"');
    Script.Add('echo ""');
    Script.Add('echo "Logs disponibles dans : /var/log/$APP_NAME/"');

    Script.SaveToFile('install_' + NomApp + '.sh');

    WriteLn('Script d''installation créé : install_', NomApp, '.sh');
    WriteLn('Rendez-le exécutable : chmod +x install_', NomApp, '.sh');

  finally
    Script.Free;
  end;
end;

procedure CreerScriptDesinstallation(const NomApp: string);  
var
  Script: TStringList;
begin
  Script := TStringList.Create;
  try
    Script.Add('#!/bin/bash');
    Script.Add('');
    Script.Add('# Script de désinstallation pour ' + NomApp);
    Script.Add('');
    Script.Add('APP_NAME="' + NomApp + '"');
    Script.Add('');
    Script.Add('if [ "$EUID" -ne 0 ]; then');
    Script.Add('    echo "Ce script doit être exécuté en tant que root"');
    Script.Add('    exit 1');
    Script.Add('fi');
    Script.Add('');
    Script.Add('echo "Désinstallation de $APP_NAME..."');
    Script.Add('');
    Script.Add('# Arrêter le service s''il existe');
    Script.Add('if systemctl is-active --quiet $APP_NAME; then');
    Script.Add('    systemctl stop $APP_NAME');
    Script.Add('    systemctl disable $APP_NAME');
    Script.Add('fi');
    Script.Add('');
    Script.Add('# Supprimer les fichiers');
    Script.Add('rm -f /usr/local/bin/$APP_NAME');
    Script.Add('rm -rf /etc/$APP_NAME');
    Script.Add('rm -rf /var/log/$APP_NAME');
    Script.Add('rm -rf /var/lib/$APP_NAME');
    Script.Add('');
    Script.Add('# Nettoyer SELinux');
    Script.Add('if command -v semodule &> /dev/null; then');
    Script.Add('    semodule -r $APP_NAME 2>/dev/null');
    Script.Add('    semanage fcontext -d "$CONFIG_PATH(/.*)?" 2>/dev/null');
    Script.Add('    semanage fcontext -d "/var/log/$APP_NAME(/.*)?" 2>/dev/null');
    Script.Add('fi');
    Script.Add('');
    Script.Add('# Nettoyer AppArmor');
    Script.Add('if [ -f /etc/apparmor.d/$APP_NAME ]; then');
    Script.Add('    rm -f /etc/apparmor.d/$APP_NAME');
    Script.Add('    systemctl reload apparmor 2>/dev/null');
    Script.Add('fi');
    Script.Add('');
    Script.Add('# Supprimer l''utilisateur système');
    Script.Add('if id -u $APP_NAME &> /dev/null; then');
    Script.Add('    userdel $APP_NAME');
    Script.Add('fi');
    Script.Add('');
    Script.Add('echo "Désinstallation terminée"');

    Script.SaveToFile('uninstall_' + NomApp + '.sh');

    WriteLn('Script de désinstallation créé : uninstall_', NomApp, '.sh');

  finally
    Script.Free;
  end;
end;

var
  AppName: string;

begin
  WriteLn('Générateur de scripts d''installation');
  WriteLn('====================================');
  WriteLn;

  Write('Nom de votre application : ');
  ReadLn(AppName);

  if AppName <> '' then
  begin
    CreerScriptInstallation(AppName);
    CreerScriptDesinstallation(AppName);

    WriteLn;
    WriteLn('Scripts créés avec succès !');
    WriteLn;
    WriteLn('Pour une installation complète, créez aussi :');
    WriteLn('- ', AppName, '.apparmor : Profil AppArmor');
    WriteLn('- ', AppName, '.pp : Module SELinux compilé');
  end;
end.
```

## Bonnes pratiques et conseils

### Principes de sécurité pour vos applications

1. **Principe du moindre privilège** : Votre application ne doit demander que les permissions strictement nécessaires.

2. **Séparation des données** : Utilisez des répertoires différents pour :
   - Configuration : `/etc/votre-app/` ou `~/.config/votre-app/`
   - Données : `/var/lib/votre-app/` ou `~/.local/share/votre-app/`
   - Logs : `/var/log/votre-app/` ou `~/.local/share/votre-app/logs/`
   - Temporaire : `/tmp/votre-app-XXXXX/`

3. **Gestion des erreurs de sécurité** : Toujours prévoir un comportement de secours quand l'accès est refusé :

```pascal
program GestionErreursSecurite;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TSecureFileHandler = class
  private
    FPrimaryPath: string;
    FFallbackPath: string;
    FLastError: string;
    function TryPath(const Path: string): Boolean;
  public
    constructor Create(const PrimaryPath, FallbackPath: string);
    function SaveData(const Data: string): Boolean;
    function LoadData: string;
    property LastError: string read FLastError;
  end;

constructor TSecureFileHandler.Create(const PrimaryPath, FallbackPath: string);  
begin
  FPrimaryPath := PrimaryPath;
  FFallbackPath := FallbackPath;
  FLastError := '';
end;

function TSecureFileHandler.TryPath(const Path: string): Boolean;  
begin
  Result := False;
  try
    ForceDirectories(ExtractFileDir(Path));
    Result := DirectoryExists(ExtractFileDir(Path));
  except
    on E: Exception do
      FLastError := E.Message;
  end;
end;

function TSecureFileHandler.SaveData(const Data: string): Boolean;  
var
  Path: string;
  F: TextFile;
begin
  Result := False;
  FLastError := '';

  // Essayer le chemin principal
  if TryPath(FPrimaryPath) then
    Path := FPrimaryPath
  else
  begin
    WriteLn('Chemin principal inaccessible : ', FPrimaryPath);
    WriteLn('Raison probable : restrictions SELinux/AppArmor');
    WriteLn('Utilisation du chemin de secours : ', FFallbackPath);

    // Essayer le chemin de secours
    if TryPath(FFallbackPath) then
      Path := FFallbackPath
    else
    begin
      FLastError := 'Aucun chemin accessible pour l''écriture';
      Exit(False);
    end;
  end;

  // Sauvegarder les données
  try
    AssignFile(F, Path);
    Rewrite(F);
    Write(F, Data);
    CloseFile(F);
    Result := True;
    WriteLn('Données sauvegardées dans : ', Path);
  except
    on E: Exception do
    begin
      FLastError := E.Message;
      WriteLn('Erreur lors de la sauvegarde : ', E.Message);
    end;
  end;
end;

function TSecureFileHandler.LoadData: string;  
var
  Path: string;
  F: TextFile;
begin
  Result := '';
  FLastError := '';

  // Déterminer quel fichier existe
  if FileExists(FPrimaryPath) then
    Path := FPrimaryPath
  else if FileExists(FFallbackPath) then
    Path := FFallbackPath
  else
  begin
    FLastError := 'Aucun fichier de données trouvé';
    Exit;
  end;

  // Charger les données
  try
    AssignFile(F, Path);
    Reset(F);
    ReadLn(F, Result);
    CloseFile(F);
    WriteLn('Données chargées depuis : ', Path);
  except
    on E: Exception do
    begin
      FLastError := E.Message;
      WriteLn('Erreur lors du chargement : ', E.Message);
    end;
  end;
end;

var
  Handler: TSecureFileHandler;
  Data: string;

begin
  WriteLn('Démonstration de gestion sécurisée des fichiers');
  WriteLn('===============================================');
  WriteLn;

  // Créer un gestionnaire avec chemin principal et de secours
  Handler := TSecureFileHandler.Create(
    '/etc/monapp/config.txt',     // Chemin principal (peut être bloqué)
    GetEnvironmentVariable('HOME') + '/.config/monapp/config.txt'  // Secours
  );

  try
    // Tenter de sauvegarder
    Data := 'Configuration de test : ' + DateTimeToStr(Now);
    if Handler.SaveData(Data) then
      WriteLn('Sauvegarde réussie')
    else
      WriteLn('Échec de sauvegarde : ', Handler.LastError);

    WriteLn;

    // Tenter de charger
    Data := Handler.LoadData;
    if Data <> '' then
      WriteLn('Données récupérées : ', Data)
    else
      WriteLn('Échec de chargement : ', Handler.LastError);

  finally
    Handler.Free;
  end;
end.
```

### 4. Documentation des besoins de sécurité

Créez toujours un fichier documentant les besoins de votre application :

```pascal
program GenererDocSecurite;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

procedure GenererREADMESecurite(const NomApp: string);  
var
  Doc: TStringList;
begin
  Doc := TStringList.Create;
  try
    Doc.Add('# Configuration de sécurité pour ' + NomApp);
    Doc.Add('');
    Doc.Add('## Permissions requises');
    Doc.Add('');
    Doc.Add('Cette application nécessite les accès suivants :');
    Doc.Add('');
    Doc.Add('### Fichiers et répertoires');
    Doc.Add('');
    Doc.Add('| Chemin | Permission | Raison |');
    Doc.Add('|--------|------------|--------|');
    Doc.Add('| ~/.config/' + NomApp + '/ | Lecture/Écriture | Stockage de la configuration utilisateur |');
    Doc.Add('| /tmp/' + NomApp + '_* | Lecture/Écriture | Fichiers temporaires |');
    Doc.Add('| /var/log/' + NomApp + '/ | Écriture | Journalisation (optionnel) |');
    Doc.Add('');
    Doc.Add('### Réseau (si applicable)');
    Doc.Add('');
    Doc.Add('- Connexions TCP sortantes sur le port 443 (HTTPS)');
    Doc.Add('- Résolution DNS');
    Doc.Add('');
    Doc.Add('## Configuration AppArmor');
    Doc.Add('');
    Doc.Add('### Installation du profil');
    Doc.Add('');
    Doc.Add('```bash');
    Doc.Add('# Copier le profil');
    Doc.Add('sudo cp ' + NomApp + '.apparmor /etc/apparmor.d/' + NomApp);
    Doc.Add('');
    Doc.Add('# Recharger AppArmor');
    Doc.Add('sudo systemctl reload apparmor');
    Doc.Add('');
    Doc.Add('# Mode complain (logs sans bloquer) - recommandé au début');
    Doc.Add('sudo aa-complain /usr/local/bin/' + NomApp);
    Doc.Add('');
    Doc.Add('# Mode enforce (applique les règles) - après tests');
    Doc.Add('sudo aa-enforce /usr/local/bin/' + NomApp);
    Doc.Add('```');
    Doc.Add('');
    Doc.Add('### Dépannage AppArmor');
    Doc.Add('');
    Doc.Add('Si l''application est bloquée :');
    Doc.Add('');
    Doc.Add('1. Vérifiez les logs :');
    Doc.Add('   ```bash');
    Doc.Add('   sudo journalctl -xe | grep apparmor');
    Doc.Add('   ```');
    Doc.Add('');
    Doc.Add('2. Passez temporairement en mode complain :');
    Doc.Add('   ```bash');
    Doc.Add('   sudo aa-complain /usr/local/bin/' + NomApp);
    Doc.Add('   ```');
    Doc.Add('');
    Doc.Add('3. Générez les règles manquantes :');
    Doc.Add('   ```bash');
    Doc.Add('   sudo aa-logprof');
    Doc.Add('   ```');
    Doc.Add('');
    Doc.Add('## Configuration SELinux');
    Doc.Add('');
    Doc.Add('### Installation de la politique');
    Doc.Add('');
    Doc.Add('```bash');
    Doc.Add('# Compiler le module');
    Doc.Add('make -f /usr/share/selinux/devel/Makefile');
    Doc.Add('');
    Doc.Add('# Installer');
    Doc.Add('sudo semodule -i ' + NomApp + '.pp');
    Doc.Add('');
    Doc.Add('# Appliquer les contextes');
    Doc.Add('sudo restorecon -Rv /usr/local/bin/' + NomApp);
    Doc.Add('```');
    Doc.Add('');
    Doc.Add('### Dépannage SELinux');
    Doc.Add('');
    Doc.Add('1. Vérifiez les violations :');
    Doc.Add('   ```bash');
    Doc.Add('   sudo ausearch -m avc -ts recent');
    Doc.Add('   ```');
    Doc.Add('');
    Doc.Add('2. Mode permissif temporaire :');
    Doc.Add('   ```bash');
    Doc.Add('   sudo setenforce 0');
    Doc.Add('   # Tester l''application');
    Doc.Add('   sudo setenforce 1');
    Doc.Add('   ```');
    Doc.Add('');
    Doc.Add('3. Générer une politique depuis les logs :');
    Doc.Add('   ```bash');
    Doc.Add('   sudo audit2allow -a -M ' + NomApp + '_fix');
    Doc.Add('   sudo semodule -i ' + NomApp + '_fix.pp');
    Doc.Add('   ```');
    Doc.Add('');
    Doc.Add('## Tests de sécurité');
    Doc.Add('');
    Doc.Add('Utilisez le script de test fourni :');
    Doc.Add('');
    Doc.Add('```bash');
    Doc.Add('./test_security.sh');
    Doc.Add('```');
    Doc.Add('');
    Doc.Add('Ce script vérifie :');
    Doc.Add('- La détection du système de sécurité actif');
    Doc.Add('- Les permissions sur les fichiers requis');
    Doc.Add('- Les connexions réseau si nécessaires');
    Doc.Add('- Le comportement en cas de restrictions');

    Doc.SaveToFile('SECURITY_' + UpperCase(NomApp) + '.md');

    WriteLn('Documentation créée : SECURITY_', UpperCase(NomApp), '.md');

  finally
    Doc.Free;
  end;
end;

var
  AppName: string;

begin
  Write('Nom de votre application : ');
  ReadLn(AppName);

  if AppName <> '' then
    GenererDocSecurite(AppName);
end.
```

### 5. Tests automatisés de compatibilité

```pascal
program TestsCompatibiliteSecurite;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Process;

type
  TTestSuite = class
  private
    FAppName: string;
    FAppPath: string;
    FResults: TStringList;
    procedure RunTest(const TestName, Command: string; const Args: array of string);
    procedure TestFileAccess;
    procedure TestNetworkAccess;
    procedure TestProcessCreation;
  public
    constructor Create(const AppName, AppPath: string);
    destructor Destroy; override;
    procedure RunAllTests;
    procedure SaveReport(const FileName: string);
  end;

constructor TTestSuite.Create(const AppName, AppPath: string);  
begin
  FAppName := AppName;
  FAppPath := AppPath;
  FResults := TStringList.Create;
  FResults.Add('Test de compatibilité sécurité pour ' + FAppName);
  FResults.Add('Date : ' + DateTimeToStr(Now));
  FResults.Add('');
end;

destructor TTestSuite.Destroy;  
begin
  FResults.Free;
  inherited;
end;

procedure TTestSuite.RunTest(const TestName, Command: string;
  const Args: array of string);
var
  Proc: TProcess;
  Output: TStringList;
  Success: Boolean;
begin
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Proc.Executable := Command;
    Proc.Parameters.AddStrings(Args);
    Proc.Options := [poUsePipes, poWaitOnExit];

    Success := False;
    try
      Proc.Execute;
      Output.LoadFromStream(Proc.Output);
      Success := (Proc.ExitStatus = 0);
    except
      on E: Exception do
        Output.Add('Erreur : ' + E.Message);
    end;

    FResults.Add('[' + IfThen(Success, 'PASS', 'FAIL') + '] ' + TestName);
    if not Success then
    begin
      FResults.Add('  Sortie : ' + Output.Text);
    end;

  finally
    Output.Free;
    Proc.Free;
  end;
end;

procedure TTestSuite.TestFileAccess;  
begin
  FResults.Add('');
  FResults.Add('=== Tests d''accès aux fichiers ===');

  // Test lecture /etc/passwd (devrait réussir)
  RunTest('Lecture /etc/passwd', 'cat', ['/etc/passwd']);

  // Test écriture /tmp (devrait réussir)
  RunTest('Écriture dans /tmp', 'touch', ['/tmp/test_' + FAppName]);

  // Test écriture /etc (devrait échouer)
  RunTest('Écriture dans /etc (doit échouer)', 'touch', ['/etc/test_' + FAppName]);
end;

procedure TTestSuite.TestNetworkAccess;  
begin
  FResults.Add('');
  FResults.Add('=== Tests d''accès réseau ===');

  // Test résolution DNS
  RunTest('Résolution DNS', 'nslookup', ['google.com']);

  // Test connexion HTTP
  RunTest('Connexion HTTP', 'curl', ['-I', 'http://example.com']);
end;

procedure TTestSuite.TestProcessCreation;  
begin
  FResults.Add('');
  FResults.Add('=== Tests de création de processus ===');

  // Test exécution commande simple
  RunTest('Exécution echo', 'echo', ['test']);

  // Test exécution avec pipe
  RunTest('Pipe command', 'sh', ['-c', 'echo test | wc -l']);
end;

procedure TTestSuite.RunAllTests;  
begin
  FResults.Add('Début des tests...');
  FResults.Add('');

  TestFileAccess;
  TestNetworkAccess;
  TestProcessCreation;

  FResults.Add('');
  FResults.Add('Tests terminés');
end;

procedure TTestSuite.SaveReport(const FileName: string);  
begin
  FResults.SaveToFile(FileName);
  WriteLn('Rapport sauvegardé dans : ', FileName);
end;

var
  Suite: TTestSuite;
  AppName: string;

begin
  WriteLn('Tests de compatibilité avec les politiques de sécurité');
  WriteLn('======================================================');
  WriteLn;

  Write('Nom de l''application à tester : ');
  ReadLn(AppName);

  if AppName = '' then
    AppName := 'monapp';

  Suite := TTestSuite.Create(AppName, '/usr/local/bin/' + AppName);
  try
    Suite.RunAllTests;
    Suite.SaveReport('security_test_' + AppName + '.txt');

    WriteLn;
    WriteLn('Consultez le rapport pour voir les résultats détaillés');

  finally
    Suite.Free;
  end;
end.
```

## Ressources et outils utiles

### Commandes essentielles

#### Pour AppArmor :
```bash
# Status général
sudo aa-status

# Mettre un profil en mode complain (log only)
sudo aa-complain /path/to/program

# Mettre un profil en mode enforce
sudo aa-enforce /path/to/program

# Désactiver un profil
sudo aa-disable /path/to/program

# Générer un profil interactivement
sudo aa-genprof /path/to/program

# Analyser les logs et suggérer des règles
sudo aa-logprof

# Recharger tous les profils
sudo systemctl reload apparmor
```

#### Pour SELinux :
```bash
# Status général
getenforce  
sestatus

# Changer le mode temporairement
sudo setenforce 0  # Permissive  
sudo setenforce 1  # Enforcing

# Voir le contexte d'un fichier
ls -Z /path/to/file

# Changer le contexte d'un fichier
sudo chcon -t type_t /path/to/file

# Restaurer les contextes par défaut
sudo restorecon -Rv /path/to/directory

# Voir les violations récentes
sudo ausearch -m avc -ts recent

# Générer une politique depuis les violations
sudo audit2allow -a -M mypolicy

# Installer une politique
sudo semodule -i mypolicy.pp

# Lister les modules
sudo semodule -l
```

### Outils graphiques

Pour ceux qui préfèrent une interface graphique :

- **Pour AppArmor** : `apparmor-utils` fournit des outils en ligne de commande, mais vous pouvez utiliser des outils comme YaST sur openSUSE
- **Pour SELinux** : `policycoreutils-gui` fournit `system-config-selinux`

### Documentation officielle

- **AppArmor** : https://gitlab.com/apparmor/apparmor/-/wikis/home
- **SELinux** : https://selinuxproject.org/page/Main_Page
- **Ubuntu AppArmor** : https://ubuntu.com/security/apparmor
- **Red Hat SELinux** : https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/8/html/using_selinux/

## Conclusion

SELinux et AppArmor sont des systèmes de sécurité puissants qui peuvent sembler intimidants au début, mais qui deviennent des alliés précieux une fois maîtrisés. Pour vos applications FreePascal/Lazarus :

### Points clés à retenir :

1. **Détection automatique** : Votre application doit détecter le système de sécurité actif et s'adapter
2. **Fallback gracieux** : Toujours prévoir des alternatives quand l'accès est refusé
3. **Documentation claire** : Documentez les besoins de sécurité de votre application
4. **Tests réguliers** : Testez sur des systèmes avec SELinux/AppArmor activés
5. **Mode apprentissage** : Commencez avec des politiques permissives, puis resserrez progressivement

### Stratégie de développement recommandée :

1. **Développement** : Commencez sans restrictions
2. **Test initial** : Activez AppArmor/SELinux en mode permissif
3. **Analyse des logs** : Identifiez les accès nécessaires
4. **Création de politique** : Générez une politique minimale
5. **Test en mode enforce** : Vérifiez que tout fonctionne
6. **Documentation** : Documentez la configuration pour les utilisateurs
7. **Distribution** : Incluez les profils de sécurité dans votre package

### Exemple final : Application complète avec support SELinux/AppArmor

```pascal
program ApplicationSecurisee;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TSecureApp = class
  private
    FConfigPath: string;
    FLogPath: string;
    FSecuritySystem: string;
    procedure DetectSecurity;
    procedure InitializePaths;
  public
    constructor Create;
    procedure Run;
  end;

constructor TSecureApp.Create;  
begin
  DetectSecurity;
  InitializePaths;
end;

procedure TSecureApp.DetectSecurity;  
begin
  if DirectoryExists('/etc/apparmor.d') and
     FileExists('/sys/kernel/security/apparmor/profiles') then
    FSecuritySystem := 'AppArmor'
  else if FileExists('/etc/selinux/config') then
    FSecuritySystem := 'SELinux'
  else
    FSecuritySystem := 'None';

  WriteLn('Système de sécurité détecté : ', FSecuritySystem);
end;

procedure TSecureApp.InitializePaths;  
begin
  // Adapter les chemins selon le système de sécurité
  if (FSecuritySystem = 'AppArmor') or (FSecuritySystem = 'SELinux') then
  begin
    // Utiliser des chemins compatibles avec les restrictions
    FConfigPath := GetEnvironmentVariable('HOME') + '/.config/monapp/';
    FLogPath := '/tmp/monapp_' + IntToStr(GetProcessID) + '/';
  end
  else
  begin
    // Pas de restrictions particulières
    FConfigPath := '/etc/monapp/';
    FLogPath := '/var/log/monapp/';
  end;

  WriteLn('Chemin de configuration : ', FConfigPath);
  WriteLn('Chemin des logs : ', FLogPath);
end;

procedure TSecureApp.Run;  
begin
  WriteLn('Application démarrée avec support ', FSecuritySystem);

  // Créer les répertoires si possible
  try
    ForceDirectories(FConfigPath);
    ForceDirectories(FLogPath);
    WriteLn('Répertoires créés avec succès');
  except
    on E: Exception do
    begin
      WriteLn('Impossible de créer les répertoires : ', E.Message);
      WriteLn('Utilisation de chemins alternatifs...');

      // Fallback vers /tmp qui est généralement accessible
      FConfigPath := '/tmp/monapp_config/';
      FLogPath := '/tmp/monapp_logs/';

      ForceDirectories(FConfigPath);
      ForceDirectories(FLogPath);
    end;
  end;

  // Votre logique d'application ici
  WriteLn('Application en cours d''exécution...');

  // Simulation de travail
  Sleep(2000);

  WriteLn('Application terminée');
end;

var
  App: TSecureApp;

begin
  App := TSecureApp.Create;
  try
    App.Run;
  finally
    App.Free;
  end;
end.
```

En suivant ces principes et en utilisant les outils fournis dans ce tutoriel, vos applications FreePascal/Lazarus pourront fonctionner harmonieusement avec SELinux et AppArmor, offrant ainsi une sécurité renforcée à vos utilisateurs tout en maintenant la fonctionnalité de votre logiciel.

⏭️ [Bases de Données et ORM Multi-plateformes](/08-bases-donnees-orm-multiplatefomes/README.md)
