🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.11 SELinux/AppArmor vs Windows Defender

## Introduction

Lors du développement d'applications FreePascal/Lazarus multi-plateformes, il est essentiel de comprendre les systèmes de sécurité natifs de chaque plateforme. Linux (Ubuntu) utilise principalement SELinux ou AppArmor, tandis que Windows utilise Windows Defender et d'autres mécanismes de sécurité. Ces systèmes peuvent affecter le fonctionnement de vos applications et nécessitent une configuration appropriée.

## Concepts fondamentaux

### Qu'est-ce qu'un système de contrôle d'accès obligatoire ?

Les systèmes de contrôle d'accès obligatoire (MAC - Mandatory Access Control) imposent des règles de sécurité que même les administrateurs ne peuvent contourner facilement. C'est une couche de sécurité supplémentaire au-dessus des permissions traditionnelles (lecture, écriture, exécution).

**Analogie simple :** Imaginez une entreprise où :
- **Permissions traditionnelles** : Vous avez une clé pour ouvrir une porte
- **MAC** : Même avec la clé, un garde vérifie votre badge et votre autorisation spécifique pour cette pièce à cette heure

### Philosophies différentes

| Aspect | Linux (SELinux/AppArmor) | Windows Defender |
|--------|--------------------------|------------------|
| **Approche** | Contrôle d'accès proactif | Détection de menaces réactive |
| **Principe** | Tout est interdit par défaut | Tout est permis sauf menaces détectées |
| **Configuration** | Politiques de sécurité | Signatures et heuristiques |
| **Granularité** | Très fine (par processus) | Globale (par fichier/comportement) |

## SELinux (Security-Enhanced Linux)

### Qu'est-ce que SELinux ?

SELinux est un module de sécurité du noyau Linux développé par la NSA. Il applique des politiques de contrôle d'accès obligatoire très strictes.

### Modes de fonctionnement

SELinux fonctionne en trois modes :

1. **Enforcing** : Les règles sont appliquées et les violations sont bloquées
2. **Permissive** : Les règles sont contrôlées mais pas appliquées (logging uniquement)
3. **Disabled** : SELinux est désactivé

```pascal
unit SELinuxChecker;

{$IFDEF LINUX}

interface

uses
  Classes, SysUtils, Process;

type
  TSELinuxMode = (slDisabled, slPermissive, slEnforcing, slUnknown);

  TSELinuxManager = class
  private
    function ExecuteCommand(const Cmd: string): string;
  public
    function IsInstalled: Boolean;
    function GetMode: TSELinuxMode;
    function GetStatus: string;
    function CheckContext(const FilePath: string): string;
  end;

implementation

function TSELinuxManager.ExecuteCommand(const Cmd: string): string;  
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := '';
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(Cmd);
    Process.Options := [poWaitOnExit, poUsePipes];

    Process.Execute;
    Output.LoadFromStream(Process.Output);
    Result := Trim(Output.Text);
  finally
    Output.Free;
    Process.Free;
  end;
end;

function TSELinuxManager.IsInstalled: Boolean;  
var
  Output: string;
begin
  Output := ExecuteCommand('which getenforce');
  Result := (Output <> '') and FileExists(Output);
end;

function TSELinuxManager.GetMode: TSELinuxMode;  
var
  Output: string;
begin
  Result := slUnknown;

  if not IsInstalled then
  begin
    Result := slDisabled;
    Exit;
  end;

  Output := LowerCase(Trim(ExecuteCommand('getenforce')));

  if Output = 'disabled' then
    Result := slDisabled
  else if Output = 'permissive' then
    Result := slPermissive
  else if Output = 'enforcing' then
    Result := slEnforcing;
end;

function TSELinuxManager.GetStatus: string;  
var
  Mode: TSELinuxMode;
begin
  Mode := GetMode;

  case Mode of
    slDisabled: Result := 'SELinux est désactivé';
    slPermissive: Result := 'SELinux est en mode permissif (avertissements uniquement)';
    slEnforcing: Result := 'SELinux est actif et applique les politiques';
    slUnknown: Result := 'État de SELinux inconnu';
  end;
end;

function TSELinuxManager.CheckContext(const FilePath: string): string;  
begin
  if not FileExists(FilePath) then
  begin
    Result := 'Fichier introuvable';
    Exit;
  end;

  Result := ExecuteCommand('ls -Z ' + FilePath);
end;

end.

{$ENDIF}
```

**Utilisation :**

```pascal
{$IFDEF LINUX}
var
  SELinux: TSELinuxManager;
  Mode: TSELinuxMode;
begin
  SELinux := TSELinuxManager.Create;
  try
    if SELinux.IsInstalled then
    begin
      WriteLn('SELinux Status: ', SELinux.GetStatus);

      Mode := SELinux.GetMode;
      if Mode = slEnforcing then
        WriteLn('ATTENTION : SELinux peut bloquer certaines opérations');

      // Vérifier le contexte d'un fichier
      WriteLn('Contexte : ', SELinux.CheckContext('/usr/local/bin/myapp'));
    end
    else
      WriteLn('SELinux n''est pas installé sur ce système');
  finally
    SELinux.Free;
  end;
end;
{$ENDIF}
```

### Contextes SELinux

SELinux utilise des "contextes" pour étiqueter les fichiers, processus et ressources :

```
user:role:type:level
```

**Exemple :**
```
system_u:object_r:bin_t:s0
```

- **user** : Utilisateur SELinux
- **role** : Rôle
- **type** : Type (le plus important)
- **level** : Niveau de sécurité

### Configuration pour vos applications

```pascal
unit SELinuxConfig;

{$IFDEF LINUX}

interface

uses
  Classes, SysUtils, Process;

type
  TSELinuxConfigurator = class
  private
    FAppPath: string;
    function RunCommand(const Cmd: string): Boolean;
  public
    constructor Create(const ApplicationPath: string);

    function SetExecutableContext: Boolean;
    function AllowNetworkAccess: Boolean;
    function AllowFileAccess(const Directory: string): Boolean;
    function CreateCustomPolicy(const PolicyName: string): Boolean;
    function GeneratePolicyFromDenials: string;
  end;

implementation

constructor TSELinuxConfigurator.Create(const ApplicationPath: string);  
begin
  inherited Create;
  FAppPath := ApplicationPath;
end;

function TSELinuxConfigurator.RunCommand(const Cmd: string): Boolean;  
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(Cmd);
    Process.Options := [poWaitOnExit];

    Process.Execute;
    Result := (Process.ExitStatus = 0);
  finally
    Process.Free;
  end;
end;

function TSELinuxConfigurator.SetExecutableContext: Boolean;  
var
  Cmd: string;
begin
  // Définir le contexte approprié pour un exécutable
  Cmd := Format('sudo chcon -t bin_t %s', [FAppPath]);
  Result := RunCommand(Cmd);

  if Result then
    WriteLn('Contexte SELinux défini pour : ', FAppPath)
  else
    WriteLn('Erreur : Impossible de définir le contexte (droits admin requis)');
end;

function TSELinuxConfigurator.AllowNetworkAccess: Boolean;  
var
  PolicyCmd: string;
begin
  // Créer une politique permettant l'accès réseau
  PolicyCmd := Format(
    'sudo setsebool -P httpd_can_network_connect 1',
    []
  );

  Result := RunCommand(PolicyCmd);

  if Result then
    WriteLn('Accès réseau autorisé')
  else
    WriteLn('Erreur configuration accès réseau');
end;

function TSELinuxConfigurator.AllowFileAccess(const Directory: string): Boolean;  
var
  Cmd: string;
begin
  // Autoriser l'accès à un répertoire spécifique
  Cmd := Format('sudo semanage fcontext -a -t usr_t "%s(/.*)?"', [Directory]);
  Result := RunCommand(Cmd);

  if Result then
  begin
    // Appliquer les changements
    RunCommand(Format('sudo restorecon -R %s', [Directory]));
    WriteLn('Accès autorisé au répertoire : ', Directory);
  end;
end;

function TSELinuxConfigurator.CreateCustomPolicy(const PolicyName: string): Boolean;  
var
  PolicyContent: TStringList;
  PolicyFile: string;
begin
  Result := False;
  PolicyFile := '/tmp/' + PolicyName + '.te';

  PolicyContent := TStringList.Create;
  try
    // Créer un module de politique basique
    PolicyContent.Add('module ' + PolicyName + ' 1.0;');
    PolicyContent.Add('');
    PolicyContent.Add('require {');
    PolicyContent.Add('    type unconfined_t;');
    PolicyContent.Add('    type bin_t;');
    PolicyContent.Add('    class file { read write execute };');
    PolicyContent.Add('};');
    PolicyContent.Add('');
    PolicyContent.Add('# Règles personnalisées');
    PolicyContent.Add('allow unconfined_t bin_t:file { read write execute };');

    PolicyContent.SaveToFile(PolicyFile);

    // Compiler et charger la politique
    if RunCommand(Format('checkmodule -M -m -o /tmp/%s.mod %s',
                        [PolicyName, PolicyFile])) then
    begin
      if RunCommand(Format('semodule_package -o /tmp/%s.pp -m /tmp/%s.mod',
                          [PolicyName, PolicyName])) then
      begin
        Result := RunCommand(Format('sudo semodule -i /tmp/%s.pp', [PolicyName]));
        if Result then
          WriteLn('Politique personnalisée chargée : ', PolicyName);
      end;
    end;
  finally
    PolicyContent.Free;
  end;
end;

function TSELinuxConfigurator.GeneratePolicyFromDenials: string;  
var
  Output: string;
  Process: TProcess;
  StringList: TStringList;
begin
  Result := '';

  // Analyser les refus SELinux et générer une politique
  Process := TProcess.Create(nil);
  StringList := TStringList.Create;
  try
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add('sudo ausearch -m avc -ts recent | audit2allow');
    Process.Options := [poWaitOnExit, poUsePipes];

    Process.Execute;
    StringList.LoadFromStream(Process.Output);
    Result := StringList.Text;

    if Result <> '' then
      WriteLn('Politique suggérée basée sur les refus récents :')
    else
      WriteLn('Aucun refus récent trouvé');
  finally
    StringList.Free;
    Process.Free;
  end;
end;

end.

{$ENDIF}
```

**Exemple d'utilisation :**

```pascal
{$IFDEF LINUX}
var
  SELinuxConfig: TSELinuxConfigurator;
  SuggestedPolicy: string;
begin
  SELinuxConfig := TSELinuxConfigurator.Create('/usr/local/bin/myapp');
  try
    WriteLn('Configuration SELinux pour l''application...');

    // Définir le bon contexte
    SELinuxConfig.SetExecutableContext;

    // Autoriser l'accès réseau si nécessaire
    if AppNeedsNetwork then
      SELinuxConfig.AllowNetworkAccess;

    // Autoriser l'accès à un répertoire de données
    SELinuxConfig.AllowFileAccess('/opt/myapp/data');

    // En cas de problèmes, générer une politique depuis les refus
    SuggestedPolicy := SELinuxConfig.GeneratePolicyFromDenials;
    if SuggestedPolicy <> '' then
      WriteLn(SuggestedPolicy);
  finally
    SELinuxConfig.Free;
  end;
end;
{$ENDIF}
```

## AppArmor

### Qu'est-ce qu'AppArmor ?

AppArmor (Application Armor) est un système de sécurité Linux plus simple que SELinux, utilisé par défaut sur Ubuntu. Il utilise des profils pour contrôler les capacités des programmes.

### Avantages d'AppArmor

- **Plus simple** que SELinux
- **Basé sur les chemins** plutôt que sur les contextes
- **Intégration Ubuntu** native
- **Profils lisibles** (texte simple)

```pascal
unit AppArmorManager;

{$IFDEF LINUX}

interface

uses
  Classes, SysUtils, Process;

type
  TAppArmorMode = (aamDisabled, aamComplain, aamEnforce, aamUnknown);

  TAppArmorManager = class
  private
    function ExecuteCommand(const Cmd: string): string;
    function ExecuteCommandWithStatus(const Cmd: string): Boolean;
  public
    function IsInstalled: Boolean;
    function IsEnabled: Boolean;
    function GetStatus: string;
    function GetProfileMode(const ProfileName: string): TAppArmorMode;
    function ListProfiles: TStringList;
  end;

implementation

function TAppArmorManager.ExecuteCommand(const Cmd: string): string;  
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := '';
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(Cmd);
    Process.Options := [poWaitOnExit, poUsePipes];

    Process.Execute;
    Output.LoadFromStream(Process.Output);
    Result := Trim(Output.Text);
  finally
    Output.Free;
    Process.Free;
  end;
end;

function TAppArmorManager.ExecuteCommandWithStatus(const Cmd: string): Boolean;  
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(Cmd);
    Process.Options := [poWaitOnExit];

    Process.Execute;
    Result := (Process.ExitStatus = 0);
  finally
    Process.Free;
  end;
end;

function TAppArmorManager.IsInstalled: Boolean;  
var
  Output: string;
begin
  Output := ExecuteCommand('which apparmor_status');
  Result := (Output <> '') and FileExists(Output);
end;

function TAppArmorManager.IsEnabled: Boolean;  
var
  Output: string;
begin
  if not IsInstalled then
  begin
    Result := False;
    Exit;
  end;

  Output := ExecuteCommand('sudo aa-status 2>&1');
  Result := Pos('apparmor module is loaded', LowerCase(Output)) > 0;
end;

function TAppArmorManager.GetStatus: string;  
begin
  if not IsInstalled then
    Result := 'AppArmor n''est pas installé'
  else if IsEnabled then
    Result := 'AppArmor est actif'
  else
    Result := 'AppArmor est installé mais désactivé';
end;

function TAppArmorManager.GetProfileMode(const ProfileName: string): TAppArmorMode;  
var
  Output: string;
begin
  Result := aamUnknown;

  if not IsEnabled then
  begin
    Result := aamDisabled;
    Exit;
  end;

  Output := LowerCase(ExecuteCommand('sudo aa-status | grep ' + ProfileName));

  if Pos('complain', Output) > 0 then
    Result := aamComplain
  else if Pos('enforce', Output) > 0 then
    Result := aamEnforce;
end;

function TAppArmorManager.ListProfiles: TStringList;  
var
  Output: string;
begin
  Result := TStringList.Create;

  if not IsEnabled then
  begin
    Result.Add('AppArmor n''est pas actif');
    Exit;
  end;

  Output := ExecuteCommand('sudo aa-status --profiled');
  Result.Text := Output;
end;

end.

{$ENDIF}
```

**Utilisation :**

```pascal
{$IFDEF LINUX}
var
  AppArmor: TAppArmorManager;
  Profiles: TStringList;
  i: Integer;
begin
  AppArmor := TAppArmorManager.Create;
  try
    WriteLn('Status AppArmor: ', AppArmor.GetStatus);

    if AppArmor.IsEnabled then
    begin
      Profiles := AppArmor.ListProfiles;
      try
        WriteLn('Profils chargés:');
        for i := 0 to Profiles.Count - 1 do
          WriteLn('  - ', Profiles[i]);
      finally
        Profiles.Free;
      end;
    end;
  finally
    AppArmor.Free;
  end;
end;
{$ENDIF}
```

### Création d'un profil AppArmor

```pascal
unit AppArmorProfileCreator;

{$IFDEF LINUX}

interface

uses
  Classes, SysUtils;

type
  TAppArmorProfileCreator = class
  private
    FAppPath: string;
    FProfileName: string;
    FProfile: TStringList;

    procedure AddHeader;
    procedure AddCapabilities;
    procedure AddFileRules;
    procedure AddNetworkRules;
  public
    constructor Create(const ApplicationPath: string);
    destructor Destroy; override;

    procedure AllowFileRead(const Path: string);
    procedure AllowFileWrite(const Path: string);
    procedure AllowFileExecute(const Path: string);
    procedure AllowNetwork(const Protocol: string = 'inet');
    procedure AllowCapability(const Capability: string);

    function SaveProfile: Boolean;
    function LoadProfile: Boolean;
    function SetProfileMode(Enforce: Boolean): Boolean;
  end;

implementation

uses
  Process;

constructor TAppArmorProfileCreator.Create(const ApplicationPath: string);  
begin
  inherited Create;
  FAppPath := ApplicationPath;
  FProfileName := StringReplace(FAppPath, '/', '.', [rfReplaceAll]);
  if FProfileName[1] = '.' then
    Delete(FProfileName, 1, 1);

  FProfile := TStringList.Create;
  AddHeader;
end;

destructor TAppArmorProfileCreator.Destroy;  
begin
  FProfile.Free;
  inherited;
end;

procedure TAppArmorProfileCreator.AddHeader;  
begin
  FProfile.Add('# AppArmor profile for ' + ExtractFileName(FAppPath));
  FProfile.Add('# Generated by FreePascal application');
  FProfile.Add('');
  FProfile.Add('#include <tunables/global>');
  FProfile.Add('');
  FProfile.Add(FAppPath + ' {');
  FProfile.Add('  #include <abstractions/base>');
  FProfile.Add('');
end;

procedure TAppArmorProfileCreator.AddCapabilities;  
begin
  FProfile.Add('  # Capabilities');
end;

procedure TAppArmorProfileCreator.AddFileRules;  
begin
  FProfile.Add('');
  FProfile.Add('  # File access rules');
end;

procedure TAppArmorProfileCreator.AddNetworkRules;  
begin
  FProfile.Add('');
  FProfile.Add('  # Network rules');
end;

procedure TAppArmorProfileCreator.AllowFileRead(const Path: string);  
begin
  FProfile.Add(Format('  %s r,', [Path]));
end;

procedure TAppArmorProfileCreator.AllowFileWrite(const Path: string);  
begin
  FProfile.Add(Format('  %s w,', [Path]));
end;

procedure TAppArmorProfileCreator.AllowFileExecute(const Path: string);  
begin
  FProfile.Add(Format('  %s ix,', [Path]));
end;

procedure TAppArmorProfileCreator.AllowNetwork(const Protocol: string);  
begin
  FProfile.Add(Format('  network %s,', [Protocol]));
end;

procedure TAppArmorProfileCreator.AllowCapability(const Capability: string);  
begin
  FProfile.Add(Format('  capability %s,', [Capability]));
end;

function TAppArmorProfileCreator.SaveProfile: Boolean;  
var
  ProfilePath: string;
begin
  // Fermer le profil
  FProfile.Add('}');

  ProfilePath := '/etc/apparmor.d/' + FProfileName;

  try
    FProfile.SaveToFile('/tmp/' + FProfileName);
    WriteLn('Profil temporaire créé : /tmp/', FProfileName);
    WriteLn('Pour l''installer : sudo cp /tmp/', FProfileName, ' ', ProfilePath);
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Erreur sauvegarde profil : ', E.Message);
      Result := False;
    end;
  end;
end;

function TAppArmorProfileCreator.LoadProfile: Boolean;  
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add('sudo apparmor_parser -r /etc/apparmor.d/' + FProfileName);
    Process.Options := [poWaitOnExit];

    Process.Execute;
    Result := (Process.ExitStatus = 0);

    if Result then
      WriteLn('Profil chargé avec succès')
    else
      WriteLn('Erreur chargement du profil (droits admin requis)');
  finally
    Process.Free;
  end;
end;

function TAppArmorProfileCreator.SetProfileMode(Enforce: Boolean): Boolean;  
var
  Process: TProcess;
  Cmd: string;
begin
  if Enforce then
    Cmd := 'sudo aa-enforce /etc/apparmor.d/' + FProfileName
  else
    Cmd := 'sudo aa-complain /etc/apparmor.d/' + FProfileName;

  Process := TProcess.Create(nil);
  try
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(Cmd);
    Process.Options := [poWaitOnExit];

    Process.Execute;
    Result := (Process.ExitStatus = 0);

    if Result then
    begin
      if Enforce then
        WriteLn('Profil en mode enforce (strict)')
      else
        WriteLn('Profil en mode complain (permissif)');
    end;
  finally
    Process.Free;
  end;
end;

end.

{$ENDIF}
```

**Exemple de création de profil :**

```pascal
{$IFDEF LINUX}
var
  ProfileCreator: TAppArmorProfileCreator;
begin
  ProfileCreator := TAppArmorProfileCreator.Create('/usr/local/bin/myapp');
  try
    WriteLn('Création d''un profil AppArmor...');

    // Autoriser l'exécution de l'application elle-même
    ProfileCreator.AllowFileExecute('/usr/local/bin/myapp');

    // Autoriser la lecture de la configuration
    ProfileCreator.AllowFileRead('/etc/myapp/**');
    ProfileCreator.AllowFileRead('/usr/local/share/myapp/**');

    // Autoriser l'écriture dans les logs
    ProfileCreator.AllowFileWrite('/var/log/myapp/*.log');

    // Autoriser l'écriture dans le répertoire de données
    ProfileCreator.AllowFileWrite('/var/lib/myapp/**');

    // Autoriser l'accès réseau TCP/IP
    ProfileCreator.AllowNetwork('inet');
    ProfileCreator.AllowNetwork('inet6');

    // Capacités nécessaires
    ProfileCreator.AllowCapability('net_bind_service');  // Pour bind sur port < 1024

    // Sauvegarder le profil
    if ProfileCreator.SaveProfile then
    begin
      WriteLn('Profil créé avec succès !');
      WriteLn('Prochaines étapes :');
      WriteLn('1. sudo cp /tmp/usr.local.bin.myapp /etc/apparmor.d/');
      WriteLn('2. sudo apparmor_parser -r /etc/apparmor.d/usr.local.bin.myapp');
      WriteLn('3. sudo aa-complain /etc/apparmor.d/usr.local.bin.myapp  (mode test)');
      WriteLn('4. sudo aa-enforce /etc/apparmor.d/usr.local.bin.myapp   (mode strict)');
    end;
  finally
    ProfileCreator.Free;
  end;
end;
{$ENDIF}
```

## Windows Defender

### Qu'est-ce que Windows Defender ?

Windows Defender est l'antivirus et système de protection intégré à Windows. Contrairement à SELinux/AppArmor, il fonctionne principalement par :

1. **Détection de signatures** : Reconnaissance de malwares connus
2. **Analyse heuristique** : Détection de comportements suspects
3. **Protection en temps réel** : Surveillance continue
4. **Contrôle d'applications** : SmartScreen et autres

```pascal
unit WindowsDefenderManager;

{$IFDEF WINDOWS}

interface

uses
  Windows, SysUtils, Classes, Registry, ActiveX, ComObj;

type
  TDefenderStatus = record
    AntivirusEnabled: Boolean;
    RealtimeProtectionEnabled: Boolean;
    BehaviorMonitorEnabled: Boolean;
    ProductStatus: string;
    DefinitionVersion: string;
  end;

  TWindowsDefenderManager = class
  private
    function GetWMIValue(const Query, Property: string): string;
  public
    function IsInstalled: Boolean;
    function GetStatus: TDefenderStatus;
    function IsFileExcluded(const FilePath: string): Boolean;
    function GetExclusionsList: TStringList;
  end;

implementation

function TWindowsDefenderManager.GetWMIValue(const Query, Property: string): string;  
var
  WMIService, WbemObject, PropValue: OleVariant;
  Enum: IEnumVaraint;
  TempValue: LongWord;
begin
  Result := '';

  try
    WMIService := GetObject('winmgmts:\\.\root\Microsoft\Windows\Defender');
    WbemObject := WMIService.ExecQuery(Query);
    Enum := IUnknown(WbemObject._NewEnum) as IEnumVariant;

    if Enum.Next(1, PropValue, TempValue) = S_OK then
    begin
      Result := VarToStr(PropValue.Properties_.Item(Property).Value);
    end;
  except
    on E: Exception do
      WriteLn('Erreur WMI : ', E.Message);
  end;
end;

function TWindowsDefenderManager.IsInstalled: Boolean;  
var
  Reg: TRegistry;
begin
  Result := False;

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Vérifier la présence de Windows Defender
    Result := Reg.KeyExists('SOFTWARE\Microsoft\Windows Defender');
  finally
    Reg.Free;
  end;
end;

function TWindowsDefenderManager.GetStatus: TDefenderStatus;  
var
  Query: string;
begin
  FillChar(Result, SizeOf(Result), 0);

  if not IsInstalled then
  begin
    Result.ProductStatus := 'Windows Defender n''est pas installé';
    Exit;
  end;

  try
    CoInitialize(nil);
    try
      Query := 'SELECT * FROM MSFT_MpComputerStatus';

      Result.AntivirusEnabled :=
        GetWMIValue(Query, 'AntivirusEnabled') = 'True';
      Result.RealtimeProtectionEnabled :=
        GetWMIValue(Query, 'RealTimeProtectionEnabled') = 'True';
      Result.BehaviorMonitorEnabled :=
        GetWMIValue(Query, 'BehaviorMonitorEnabled') = 'True';
      Result.DefinitionVersion :=
        GetWMIValue(Query, 'AntivirusSignatureVersion');

      if Result.AntivirusEnabled then
        Result.ProductStatus := 'Actif'
      else
        Result.ProductStatus := 'Inactif';

    finally
      CoUninitialize;
    end;
  except
    on E: Exception do
    begin
      Result.ProductStatus := 'Erreur : ' + E.Message;
    end;
  end;
end;

function TWindowsDefenderManager.IsFileExcluded(const FilePath: string): Boolean;  
var
  Exclusions: TStringList;
  i: Integer;
  NormalizedPath: string;
begin
  Result := False;
  NormalizedPath := UpperCase(ExpandFileName(FilePath));

  Exclusions := GetExclusionsList;
  try
    for i := 0 to Exclusions.Count - 1 do
    begin
      if Pos(UpperCase(Exclusions[i]), NormalizedPath) > 0 then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    Exclusions.Free;
  end;
end;

function TWindowsDefenderManager.GetExclusionsList: TStringList;  
var
  Reg: TRegistry;
  Values: TStringList;
  i: Integer;
begin
  Result := TStringList.Create;
  Values := TStringList.Create;

  try
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;

      // Lire les exclusions de chemins
      if Reg.OpenKey('SOFTWARE\Microsoft\Windows Defender\Exclusions\Paths', False) then
      begin
        Reg.GetValueNames(Values);
        for i := 0 to Values.Count - 1 do
          Result.Add(Values[i]);
        Reg.CloseKey;
      end;

      // Lire les exclusions d'extensions
      if Reg.OpenKey('SOFTWARE\Microsoft\Windows Defender\Exclusions\Extensions', False) then
      begin
        Values.Clear;
        Reg.GetValueNames(Values);
        for i := 0 to Values.Count - 1 do
          Result.Add('*.' + Values[i]);
        Reg.CloseKey;
      end;

    finally
      Reg.Free;
    end;
  finally
    Values.Free;
  end;
end;

end.

{$ENDIF}
```

**Utilisation :**

```pascal
{$IFDEF WINDOWS}
var
  Defender: TWindowsDefenderManager;
  Status: TDefenderStatus;
  Exclusions: TStringList;
  i: Integer;
begin
  Defender := TWindowsDefenderManager.Create;
  try
    if Defender.IsInstalled then
    begin
      Status := Defender.GetStatus;

      WriteLn('=== Windows Defender ===');
      WriteLn('État : ', Status.ProductStatus);
      WriteLn('Protection temps réel : ', Status.RealtimeProtectionEnabled);
      WriteLn('Surveillance comportement : ', Status.BehaviorMonitorEnabled);
      WriteLn('Version définitions : ', Status.DefinitionVersion);
      WriteLn('');

      // Vérifier si votre application est exclue
      if Defender.IsFileExcluded('C:\MyApp\myapp.exe') then
        WriteLn('L''application est dans les exclusions')
      else
        WriteLn('L''application n''est PAS exclue');

      // Lister toutes les exclusions
      Exclusions := Defender.GetExclusionsList;
      try
        if Exclusions.Count > 0 then
        begin
          WriteLn('');
          WriteLn('Exclusions configurées :');
          for i := 0 to Exclusions.Count - 1 do
            WriteLn('  - ', Exclusions[i]);
        end;
      finally
        Exclusions.Free;
      end;
    end
    else
      WriteLn('Windows Defender n''est pas installé');
  finally
    Defender.Free;
  end;
end;
{$ENDIF}
```

### Gestion des exclusions Windows Defender

```pascal
unit WindowsDefenderExclusions;

{$IFDEF WINDOWS}

interface

uses
  Windows, SysUtils, Classes, ShellAPI;

type
  TDefenderExclusionManager = class
  public
    function AddPathExclusion(const Path: string): Boolean;
    function RemovePathExclusion(const Path: string): Boolean;
    function AddProcessExclusion(const ProcessName: string): Boolean;
    function AddExtensionExclusion(const Extension: string): Boolean;
    function ShowExclusionInstructions(const AppPath: string): string;
  end;

implementation

function TDefenderExclusionManager.AddPathExclusion(const Path: string): Boolean;  
var
  Command: string;
begin
  // Utiliser PowerShell pour ajouter une exclusion
  Command := Format(
    'powershell.exe -Command "Add-MpPreference -ExclusionPath ''%s''"',
    [Path]
  );

  Result := ShellExecute(0, 'runas', 'cmd.exe',
    PChar('/c ' + Command), nil, SW_HIDE) > 32;

  if Result then
    WriteLn('Exclusion ajoutée : ', Path)
  else
    WriteLn('Erreur : Privilèges administrateur requis');
end;

function TDefenderExclusionManager.RemovePathExclusion(const Path: string): Boolean;  
var
  Command: string;
begin
  Command := Format(
    'powershell.exe -Command "Remove-MpPreference -ExclusionPath ''%s''"',
    [Path]
  );

  Result := ShellExecute(0, 'runas', 'cmd.exe',
    PChar('/c ' + Command), nil, SW_HIDE) > 32;
end;

function TDefenderExclusionManager.AddProcessExclusion(const ProcessName: string): Boolean;  
var
  Command: string;
begin
  Command := Format(
    'powershell.exe -Command "Add-MpPreference -ExclusionProcess ''%s''"',
    [ProcessName]
  );

  Result := ShellExecute(0, 'runas', 'cmd.exe',
    PChar('/c ' + Command), nil, SW_HIDE) > 32;
end;

function TDefenderExclusionManager.AddExtensionExclusion(const Extension: string): Boolean;  
var
  Command: string;
  CleanExt: string;
begin
  // Retirer le point si présent
  CleanExt := Extension;
  if CleanExt[1] = '.' then
    Delete(CleanExt, 1, 1);

  Command := Format(
    'powershell.exe -Command "Add-MpPreference -ExclusionExtension ''%s''"',
    [CleanExt]
  );

  Result := ShellExecute(0, 'runas', 'cmd.exe',
    PChar('/c ' + Command), nil, SW_HIDE) > 32;
end;

function TDefenderExclusionManager.ShowExclusionInstructions(const AppPath: string): string;  
var
  Instructions: TStringList;
begin
  Instructions := TStringList.Create;
  try
    Instructions.Add('=== Instructions pour exclure votre application ===');
    Instructions.Add('');
    Instructions.Add('Méthode 1 : Interface graphique');
    Instructions.Add('1. Ouvrir Windows Security (Sécurité Windows)');
    Instructions.Add('2. Aller dans "Protection contre les virus et menaces"');
    Instructions.Add('3. Cliquer sur "Gérer les paramètres"');
    Instructions.Add('4. Descendre jusqu''à "Exclusions"');
    Instructions.Add('5. Cliquer sur "Ajouter ou supprimer des exclusions"');
    Instructions.Add('6. Ajouter le chemin : ' + AppPath);
    Instructions.Add('');
    Instructions.Add('Méthode 2 : PowerShell (Admin requis)');
    Instructions.Add('Exécuter cette commande :');
    Instructions.Add('Add-MpPreference -ExclusionPath "' + AppPath + '"');
    Instructions.Add('');
    Instructions.Add('Méthode 3 : Ligne de commande');
    Instructions.Add('powershell -Command "Add-MpPreference -ExclusionPath ''' + AppPath + '''"');

    Result := Instructions.Text;
  finally
    Instructions.Free;
  end;
end;

end.

{$ENDIF}
```

### SmartScreen et réputation d'applications

Windows utilise SmartScreen pour vérifier la réputation des applications.

```pascal
unit WindowsSmartScreen;

{$IFDEF WINDOWS}

interface

uses
  Windows, SysUtils, Classes;

type
  TSmartScreenHelper = class
  public
    class function IsSmartScreenEnabled: Boolean;
    class function GetApplicationReputation(const AppPath: string): string;
    class function ShowSigningInstructions: string;
  end;

implementation

uses
  Registry;

class function TSmartScreenHelper.IsSmartScreenEnabled: Boolean;  
var
  Reg: TRegistry;
  Value: string;
begin
  Result := True; // Par défaut activé

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKey('SOFTWARE\Policies\Microsoft\Windows\System', False) then
    begin
      if Reg.ValueExists('EnableSmartScreen') then
      begin
        Value := Reg.ReadString('EnableSmartScreen');
        Result := (Value <> '0') and (Value <> 'Off');
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

class function TSmartScreenHelper.GetApplicationReputation(const AppPath: string): string;  
begin
  // Note : L'API de réputation n'est pas publiquement accessible
  // Cette fonction retourne des conseils généraux

  if FileExists(AppPath) then
  begin
    // Vérifier si le fichier est signé
    // (Implémentation simplifiée)
    Result := 'Pour améliorer la réputation :' + sLineBreak +
              '1. Signer numériquement l''exécutable' + sLineBreak +
              '2. Soumettre à Microsoft pour analyse' + sLineBreak +
              '3. Augmenter le nombre de téléchargements';
  end
  else
    Result := 'Fichier introuvable';
end;

class function TSmartScreenHelper.ShowSigningInstructions: string;  
var
  Instructions: TStringList;
begin
  Instructions := TStringList.Create;
  try
    Instructions.Add('=== Signature numérique Authenticode ===');
    Instructions.Add('');
    Instructions.Add('Pourquoi signer ?');
    Instructions.Add('- Éviter l''avertissement SmartScreen');
    Instructions.Add('- Garantir l''authenticité de votre application');
    Instructions.Add('- Améliorer la confiance des utilisateurs');
    Instructions.Add('');
    Instructions.Add('Comment obtenir un certificat ?');
    Instructions.Add('1. Acheter un certificat de signature de code auprès de :');
    Instructions.Add('   - DigiCert');
    Instructions.Add('   - Sectigo (Comodo)');
    Instructions.Add('   - GlobalSign');
    Instructions.Add('   Prix : 200-500€/an');
    Instructions.Add('');
    Instructions.Add('2. Signer votre exécutable avec signtool.exe :');
    Instructions.Add('   signtool sign /f certificate.pfx /p password /t http://timestamp.digicert.com myapp.exe');
    Instructions.Add('');
    Instructions.Add('3. Vérifier la signature :');
    Instructions.Add('   signtool verify /pa myapp.exe');
    Instructions.Add('');
    Instructions.Add('Alternative gratuite pour open source :');
    Instructions.Add('- Certum Open Source Code Signing (gratuit mais limité)');

    Result := Instructions.Text;
  finally
    Instructions.Free;
  end;
end;

end.

{$ENDIF}
```

## Comparaison pratique

### Tableau comparatif

| Critère | SELinux | AppArmor | Windows Defender |
|---------|---------|----------|------------------|
| **Plateforme** | Linux (toutes distros) | Linux (Ubuntu++) | Windows |
| **Complexité** | Très élevée | Moyenne | Faible |
| **Approche** | Contextes et étiquettes | Chemins de fichiers | Signatures + heuristiques |
| **Granularité** | Très fine | Fine | Globale |
| **Configuration** | Politiques complexes | Profils texte | Interface graphique |
| **Impact performance** | Moyen | Faible | Faible |
| **Courbe d'apprentissage** | Raide | Modérée | Douce |
| **Mode permissif** | Oui | Oui (complain) | Non |
| **Protection proactive** | Oui | Oui | Non (réactif) |

### Gestion unifiée multi-plateforme

```pascal
unit CrossPlatformSecurity;

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}, WindowsDefenderManager, WindowsDefenderExclusions{$ENDIF}
  {$IFDEF LINUX}, SELinuxChecker, AppArmorManager{$ENDIF};

type
  TSecuritySystem = (ssNone, ssSELinux, ssAppArmor, ssWindowsDefender);

  TSecurityInfo = record
    System: TSecuritySystem;
    Enabled: Boolean;
    Status: string;
    Recommendations: TStringList;
  end;

  TCrossPlatformSecurityManager = class
  private
    FSecurityInfo: TSecurityInfo;
    procedure DetectSecuritySystem;
  public
    constructor Create;
    destructor Destroy; override;

    function GetSecurityInfo: TSecurityInfo;
    function ConfigureForApplication(const AppPath: string): Boolean;
    function GetConfigurationInstructions: string;
  end;

implementation

constructor TCrossPlatformSecurityManager.Create;  
begin
  inherited Create;
  FSecurityInfo.Recommendations := TStringList.Create;
  DetectSecuritySystem;
end;

destructor TCrossPlatformSecurityManager.Destroy;  
begin
  FSecurityInfo.Recommendations.Free;
  inherited;
end;

procedure TCrossPlatformSecurityManager.DetectSecuritySystem;
{$IFDEF LINUX}
var
  SELinux: TSELinuxManager;
  AppArmor: TAppArmorManager;
{$ENDIF}
{$IFDEF WINDOWS}
var
  Defender: TWindowsDefenderManager;
  Status: TDefenderStatus;
{$ENDIF}
begin
  FSecurityInfo.System := ssNone;
  FSecurityInfo.Enabled := False;
  FSecurityInfo.Recommendations.Clear;

  {$IFDEF LINUX}
  // Vérifier SELinux en premier
  SELinux := TSELinuxManager.Create;
  try
    if SELinux.IsInstalled and (SELinux.GetMode <> slDisabled) then
    begin
      FSecurityInfo.System := ssSELinux;
      FSecurityInfo.Enabled := True;
      FSecurityInfo.Status := SELinux.GetStatus;

      FSecurityInfo.Recommendations.Add('Créer une politique SELinux personnalisée');
      FSecurityInfo.Recommendations.Add('Définir les contextes appropriés');
      FSecurityInfo.Recommendations.Add('Tester en mode permissif avant enforce');
    end;
  finally
    SELinux.Free;
  end;

  // Si pas SELinux, vérifier AppArmor
  if FSecurityInfo.System = ssNone then
  begin
    AppArmor := TAppArmorManager.Create;
    try
      if AppArmor.IsEnabled then
      begin
        FSecurityInfo.System := ssAppArmor;
        FSecurityInfo.Enabled := True;
        FSecurityInfo.Status := AppArmor.GetStatus;

        FSecurityInfo.Recommendations.Add('Créer un profil AppArmor');
        FSecurityInfo.Recommendations.Add('Tester en mode complain');
        FSecurityInfo.Recommendations.Add('Activer en mode enforce après validation');
      end;
    finally
      AppArmor.Free;
    end;
  end;
  {$ENDIF}

  {$IFDEF WINDOWS}
  Defender := TWindowsDefenderManager.Create;
  try
    if Defender.IsInstalled then
    begin
      FSecurityInfo.System := ssWindowsDefender;
      Status := Defender.GetStatus;
      FSecurityInfo.Enabled := Status.AntivirusEnabled;
      FSecurityInfo.Status := Status.ProductStatus;

      if Status.AntivirusEnabled then
      begin
        FSecurityInfo.Recommendations.Add('Signer numériquement l''application');
        FSecurityInfo.Recommendations.Add('Ajouter une exclusion si nécessaire');
        FSecurityInfo.Recommendations.Add('Construire une réputation SmartScreen');
      end;
    end;
  finally
    Defender.Free;
  end;
  {$ENDIF}

  if FSecurityInfo.System = ssNone then
    FSecurityInfo.Status := 'Aucun système de sécurité détecté';
end;

function TCrossPlatformSecurityManager.GetSecurityInfo: TSecurityInfo;  
begin
  Result := FSecurityInfo;
end;

function TCrossPlatformSecurityManager.ConfigureForApplication(
  const AppPath: string): Boolean;
begin
  Result := False;

  case FSecurityInfo.System of
    ssSELinux:
      begin
        {$IFDEF LINUX}
        WriteLn('Configuration SELinux pour : ', AppPath);
        WriteLn('Exécutez ces commandes :');
        WriteLn('  sudo chcon -t bin_t ', AppPath);
        WriteLn('  sudo semanage fcontext -a -t bin_t "', AppPath, '"');
        Result := True;
        {$ENDIF}
      end;

    ssAppArmor:
      begin
        {$IFDEF LINUX}
        WriteLn('Configuration AppArmor pour : ', AppPath);
        WriteLn('Créez un profil dans /etc/apparmor.d/');
        WriteLn('Utilisez aa-genprof pour générer automatiquement');
        Result := True;
        {$ENDIF}
      end;

    ssWindowsDefender:
      begin
        {$IFDEF WINDOWS}
        WriteLn('Configuration Windows Defender pour : ', AppPath);
        WriteLn('Ajoutez une exclusion via :');
        WriteLn('  PowerShell : Add-MpPreference -ExclusionPath "', AppPath, '"');
        Result := True;
        {$ENDIF}
      end;
  end;
end;

function TCrossPlatformSecurityManager.GetConfigurationInstructions: string;  
var
  Instructions: TStringList;
  i: Integer;
begin
  Instructions := TStringList.Create;
  try
    Instructions.Add('=== Configuration de sécurité recommandée ===');
    Instructions.Add('');
    Instructions.Add('Système détecté : ' +
      case FSecurityInfo.System of
        ssNone: 'Aucun';
        ssSELinux: 'SELinux';
        ssAppArmor: 'AppArmor';
        ssWindowsDefender: 'Windows Defender';
      end
    );
    Instructions.Add('État : ' + FSecurityInfo.Status);
    Instructions.Add('');

    if FSecurityInfo.Recommendations.Count > 0 then
    begin
      Instructions.Add('Recommandations :');
      for i := 0 to FSecurityInfo.Recommendations.Count - 1 do
        Instructions.Add('  ' + IntToStr(i + 1) + '. ' + FSecurityInfo.Recommendations[i]);
    end;

    Result := Instructions.Text;
  finally
    Instructions.Free;
  end;
end;

end.
```

**Utilisation du gestionnaire unifié :**

```pascal
var
  SecurityManager: TCrossPlatformSecurityManager;
  SecurityInfo: TSecurityInfo;
  i: Integer;
begin
  SecurityManager := TCrossPlatformSecurityManager.Create;
  try
    SecurityInfo := SecurityManager.GetSecurityInfo;

    WriteLn('=== Analyse de sécurité ===');
    WriteLn('Système : ',
      case SecurityInfo.System of
        ssNone: 'Aucun';
        ssSELinux: 'SELinux';
        ssAppArmor: 'AppArmor';
        ssWindowsDefender: 'Windows Defender';
      end
    );
    WriteLn('Actif : ', SecurityInfo.Enabled);
    WriteLn('État : ', SecurityInfo.Status);
    WriteLn('');

    if SecurityInfo.Recommendations.Count > 0 then
    begin
      WriteLn('Actions recommandées :');
      for i := 0 to SecurityInfo.Recommendations.Count - 1 do
        WriteLn('  ', i + 1, '. ', SecurityInfo.Recommendations[i]);
    end;

    WriteLn('');
    WriteLn(SecurityManager.GetConfigurationInstructions);

    // Configurer pour votre application
    SecurityManager.ConfigureForApplication(ParamStr(0));

  finally
    SecurityManager.Free;
  end;
end;
```

## Bonnes pratiques de développement

### 1. Conception sécurisée

```pascal
// ✅ Bon : Principe du moindre privilège
procedure SafeFileOperation(const FileName: string);  
begin
  // Ouvrir en lecture seule si possible
  if not NeedToWrite then
    OpenFile(FileName, fmOpenRead)
  else
    OpenFile(FileName, fmOpenReadWrite);
end;

// ❌ Mauvais : Toujours en lecture/écriture
procedure UnsafeFileOperation(const FileName: string);  
begin
  OpenFile(FileName, fmOpenReadWrite);  // Trop de droits
end;
```

### 2. Gestion des chemins

```pascal
// ✅ Bon : Chemins absolus et validation
function GetSafeDataPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + '\MyApp\';
  {$ENDIF}
  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + '/.config/myapp/';
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

// ❌ Mauvais : Chemins relatifs
function UnsafeDataPath: string;  
begin
  Result := 'data/';  // Peut causer des problèmes avec SELinux/AppArmor
end;
```

### 3. Logging pour le débogage

```pascal
unit SecurityAwareLogging;

interface

procedure LogSecurityEvent(const Event: string);  
procedure LogFileAccess(const FileName: string; Success: Boolean);

implementation

uses
  SysUtils;

procedure LogSecurityEvent(const Event: string);  
var
  LogFile: TextFile;
  LogPath: string;
begin
  {$IFDEF WINDOWS}
  LogPath := GetEnvironmentVariable('TEMP') + '\myapp_security.log';
  {$ENDIF}
  {$IFDEF LINUX}
  LogPath := '/var/log/myapp/security.log';
  {$ENDIF}

  try
    AssignFile(LogFile, LogPath);
    if FileExists(LogPath) then
      Append(LogFile)
    else
      Rewrite(LogFile);

    WriteLn(LogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' - ', Event);
    CloseFile(LogFile);
  except
    // Si impossible d'écrire, écrire sur stdout
    WriteLn('[SECURITY] ', Event);
  end;
end;

procedure LogFileAccess(const FileName: string; Success: Boolean);  
begin
  if Success then
    LogSecurityEvent('File access granted: ' + FileName)
  else
    LogSecurityEvent('File access DENIED: ' + FileName + ' - Check SELinux/AppArmor/Defender');
end;

end.
```

### 4. Détection et adaptation

```pascal
unit SecurityAdaptiveApp;

interface

type
  TSecurityAwareApp = class
  private
    FSecurityRestrictive: Boolean;
    procedure DetectSecurityLevel;
  public
    constructor Create;

    procedure InitializeWithSecurity;
    function CanAccessPath(const Path: string): Boolean;
    procedure RequestPermissions;
  end;

implementation

uses
  CrossPlatformSecurity;

constructor TSecurityAwareApp.Create;  
begin
  inherited Create;
  DetectSecurityLevel;
end;

procedure TSecurityAwareApp.DetectSecurityLevel;  
var
  SecurityMgr: TCrossPlatformSecurityManager;
  Info: TSecurityInfo;
begin
  SecurityMgr := TCrossPlatformSecurityManager.Create;
  try
    Info := SecurityMgr.GetSecurityInfo;

    // Considérer SELinux/AppArmor comme restrictifs
    FSecurityRestrictive := Info.Enabled and
      (Info.System in [ssSELinux, ssAppArmor]);

    if FSecurityRestrictive then
      WriteLn('Mode sécurité restrictif détecté - adaptation des chemins');
  finally
    SecurityMgr.Free;
  end;
end;

procedure TSecurityAwareApp.InitializeWithSecurity;  
begin
  if FSecurityRestrictive then
  begin
    // Utiliser des chemins plus conservateurs
    WriteLn('Utilisation de chemins standards uniquement');
    // Éviter /tmp, utiliser ~/.config ou %APPDATA%
  end
  else
  begin
    // Liberté normale
    WriteLn('Configuration standard');
  end;
end;

function TSecurityAwareApp.CanAccessPath(const Path: string): Boolean;  
begin
  // Tester l'accès avant de l'utiliser
  Result := DirectoryExists(Path) or FileExists(Path);

  if not Result and FSecurityRestrictive then
    WriteLn('Accès refusé (probablement par SELinux/AppArmor) : ', Path);
end;

procedure TSecurityAwareApp.RequestPermissions;  
begin
  {$IFDEF LINUX}
  WriteLn('Permissions requises :');
  WriteLn('- Lecture : /etc/myapp/');
  WriteLn('- Écriture : ~/.config/myapp/');
  WriteLn('- Réseau : TCP ports 8080');
  WriteLn('');
  WriteLn('Configurez AppArmor/SELinux en conséquence');
  {$ENDIF}

  {$IFDEF WINDOWS}
  WriteLn('Permissions Windows :');
  WriteLn('- Si bloqué par Defender, ajoutez une exclusion');
  WriteLn('- Signez l''application pour éviter SmartScreen');
  {$ENDIF}
end;

end.
```

## Débogage des problèmes de sécurité

### Linux : Diagnostic SELinux/AppArmor

```pascal
unit LinuxSecurityDiagnostic;

{$IFDEF LINUX}

interface

uses
  Classes, SysUtils, Process;

type
  TLinuxSecurityDiagnostic = class
  public
    procedure DiagnoseSELinux;
    procedure DiagnoseAppArmor;
    procedure CheckRecentDenials;
    procedure SuggestFixes;
  end;

implementation

procedure TLinuxSecurityDiagnostic.DiagnoseSELinux;  
var
  Process: TProcess;
  Output: TStringList;
begin
  WriteLn('=== Diagnostic SELinux ===');

  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    // Vérifier le mode
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add('getenforce 2>&1');
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    WriteLn('Mode SELinux : ', Trim(Output.Text));

    // Vérifier les refus récents
    Process.Parameters.Clear;
    Process.Parameters.Add('-c');
    Process.Parameters.Add('ausearch -m avc -ts recent 2>&1 | head -20');
    Process.Execute;

    Output.Clear;
    Output.LoadFromStream(Process.Output);

    if Output.Count > 0 then
    begin
      WriteLn('');
      WriteLn('Refus récents :');
      WriteLn(Output.Text);
    end
    else
      WriteLn('Aucun refus récent');

  finally
    Output.Free;
    Process.Free;
  end;
end;

procedure TLinuxSecurityDiagnostic.DiagnoseAppArmor;  
var
  Process: TProcess;
  Output: TStringList;
begin
  WriteLn('=== Diagnostic AppArmor ===');

  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add('sudo aa-status 2>&1');
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    WriteLn(Output.Text);

  finally
    Output.Free;
    Process.Free;
  end;
end;

procedure TLinuxSecurityDiagnostic.CheckRecentDenials;  
begin
  WriteLn('=== Vérification des refus ===');
  WriteLn('Consultez :');
  WriteLn('  SELinux : sudo ausearch -m avc -ts recent');
  WriteLn('  AppArmor : sudo journalctl | grep DENIED');
  WriteLn('  Kernel : sudo dmesg | grep -i denied');
end;

procedure TLinuxSecurityDiagnostic.SuggestFixes;  
begin
  WriteLn('=== Solutions courantes ===');
  WriteLn('');
  WriteLn('Si SELinux bloque :');
  WriteLn('  1. Mode permissif temporaire : sudo setenforce 0');
  WriteLn('  2. Générer politique : ausearch -m avc -ts recent | audit2allow -M mypolicy');
  WriteLn('  3. Charger politique : sudo semodule -i mypolicy.pp');
  WriteLn('  4. Contexte fichier : sudo chcon -t bin_t /path/to/app');
  WriteLn('');
  WriteLn('Si AppArmor bloque :');
  WriteLn('  1. Mode complain : sudo aa-complain /etc/apparmor.d/profile');
  WriteLn('  2. Désactiver profil : sudo aa-disable /etc/apparmor.d/profile');
  WriteLn('  3. Générer profil : sudo aa-genprof /path/to/app');
  WriteLn('  4. Mode enforce : sudo aa-enforce /etc/apparmor.d/profile');
end;

end.

{$ENDIF}
```

### Windows : Diagnostic Defender

```pascal
unit WindowsSecurityDiagnostic;

{$IFDEF WINDOWS}

interface

uses
  Windows, SysUtils, Classes, Registry;

type
  TWindowsSecurityDiagnostic = class
  public
    procedure DiagnoseDefender;
    procedure CheckQuarantine;
    procedure CheckThreatHistory;
    procedure SuggestFixes;
  end;

implementation

procedure TWindowsSecurityDiagnostic.DiagnoseDefender;  
var
  Reg: TRegistry;
  RealtimeEnabled: Boolean;
begin
  WriteLn('=== Diagnostic Windows Defender ===');

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Protection en temps réel
    if Reg.OpenKey('SOFTWARE\Microsoft\Windows Defender\Real-Time Protection', False) then
    begin
      if Reg.ValueExists('DisableRealtimeMonitoring') then
        RealtimeEnabled := Reg.ReadInteger('DisableRealtimeMonitoring') = 0
      else
        RealtimeEnabled := True;

      WriteLn('Protection temps réel : ', RealtimeEnabled);
      Reg.CloseKey;
    end;

    // Version des définitions
    if Reg.OpenKey('SOFTWARE\Microsoft\Windows Defender\Signature Updates', False) then
    begin
      if Reg.ValueExists('SignatureVersion') then
        WriteLn('Version signatures : ', Reg.ReadString('SignatureVersion'));
      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;
end;

procedure TWindowsSecurityDiagnostic.CheckQuarantine;  
var
  QuarantinePath: string;
begin
  WriteLn('');
  WriteLn('=== Vérification de la quarantaine ===');

  QuarantinePath := 'C:\ProgramData\Microsoft\Windows Defender\Quarantine\';

  if DirectoryExists(QuarantinePath) then
    WriteLn('Répertoire quarantaine : ', QuarantinePath)
  else
    WriteLn('Aucune quarantaine trouvée');

  WriteLn('Pour voir les éléments en quarantaine :');
  WriteLn('  1. Ouvrir Windows Security');
  WriteLn('  2. Protection contre les virus et menaces');
  WriteLn('  3. Historique de protection');
end;

procedure TWindowsSecurityDiagnostic.CheckThreatHistory;  
begin
  WriteLn('');
  WriteLn('=== Historique des menaces ===');
  WriteLn('Consultez :');
  WriteLn('  Event Viewer : Windows Logs → Application');
  WriteLn('  Filtrer source : Windows Defender');
  WriteLn('');
  WriteLn('Ligne de commande :');
  WriteLn('  Get-MpThreatDetection (PowerShell)');
end;

procedure TWindowsSecurityDiagnostic.SuggestFixes;  
begin
  WriteLn('');
  WriteLn('=== Solutions courantes ===');
  WriteLn('');
  WriteLn('Si votre application est bloquée :');
  WriteLn('  1. Ajouter une exclusion :');
  WriteLn('     Add-MpPreference -ExclusionPath "C:\Path\To\App"');
  WriteLn('');
  WriteLn('  2. Signer numériquement l''application');
  WriteLn('     signtool sign /f cert.pfx /p password app.exe');
  WriteLn('');
  WriteLn('  3. Soumettre à Microsoft pour analyse :');
  WriteLn('     https://www.microsoft.com/wdsi/filesubmission');
  WriteLn('');
  WriteLn('  4. Désactiver temporairement (NON recommandé) :');
  WriteLn('     Set-MpPreference -DisableRealtimeMonitoring $true');
end;

end.

{$ENDIF}
```

## Guide de résolution de problèmes

### Checklist de diagnostic

```pascal
unit SecurityTroubleshooting;

interface

uses
  Classes, SysUtils;

type
  TSecurityIssue = (
    siFileAccessDenied,
    siNetworkBlocked,
    siExecutionPrevented,
    siQuarantined,
    siPermissionError
  );

  TSecurityTroubleshooter = class
  private
    FIssue: TSecurityIssue;
    FSolutions: TStringList;
    procedure AnalyzeIssue;
  public
    constructor Create(Issue: TSecurityIssue);
    destructor Destroy; override;

    function GetSolutions: TStringList;
    procedure RunDiagnostics;
  end;

implementation

uses
  {$IFDEF LINUX}LinuxSecurityDiagnostic{$ENDIF}
  {$IFDEF WINDOWS}WindowsSecurityDiagnostic{$ENDIF};

constructor TSecurityTroubleshooter.Create(Issue: TSecurityIssue);  
begin
  inherited Create;
  FIssue := Issue;
  FSolutions := TStringList.Create;
  AnalyzeIssue;
end;

destructor TSecurityTroubleshooter.Destroy;  
begin
  FSolutions.Free;
  inherited;
end;

procedure TSecurityTroubleshooter.AnalyzeIssue;  
begin
  FSolutions.Clear;

  case FIssue of
    siFileAccessDenied:
      begin
        {$IFDEF LINUX}
        FSolutions.Add('Vérifier les permissions : ls -la fichier');
        FSolutions.Add('Vérifier SELinux : ls -Z fichier');
        FSolutions.Add('Vérifier AppArmor : sudo aa-status');
        FSolutions.Add('Logs kernel : sudo dmesg | grep -i denied');
        {$ENDIF}
        {$IFDEF WINDOWS}
        FSolutions.Add('Vérifier les permissions NTFS');
        FSolutions.Add('Vérifier Windows Defender');
        FSolutions.Add('Vérifier le contrôle de compte utilisateur (UAC)');
        {$ENDIF}
      end;

    siNetworkBlocked:
      begin
        {$IFDEF LINUX}
        FSolutions.Add('Vérifier firewall : sudo ufw status');
        FSolutions.Add('Vérifier SELinux boolean : getsebool httpd_can_network_connect');
        FSolutions.Add('Vérifier AppArmor profil réseau');
        {$ENDIF}
        {$IFDEF WINDOWS}
        FSolutions.Add('Vérifier Windows Firewall');
        FSolutions.Add('Vérifier Windows Defender Firewall');
        FSolutions.Add('Règles sortantes : Get-NetFirewallRule');
        {$ENDIF}
      end;

    siExecutionPrevented:
      begin
        {$IFDEF LINUX}
        FSolutions.Add('Bit exécution : chmod +x fichier');
        FSolutions.Add('SELinux execmod : sudo chcon -t bin_t fichier');
        FSolutions.Add('AppArmor : Ajouter règle ix dans profil');
        {$ENDIF}
        {$IFDEF WINDOWS}
        FSolutions.Add('Windows Defender SmartScreen');
        FSolutions.Add('Signer l''exécutable avec certificat');
        FSolutions.Add('Ajouter exclusion Defender');
        {$ENDIF}
      end;

    siQuarantined:
      begin
        {$IFDEF WINDOWS}
        FSolutions.Add('Restaurer depuis quarantaine Defender');
        FSolutions.Add('Ajouter exclusion avant restauration');
        FSolutions.Add('Soumettre faux positif à Microsoft');
        {$ENDIF}
      end;

    siPermissionError:
      begin
        FSolutions.Add('Exécuter avec privilèges élevés');
        FSolutions.Add('Vérifier appartenance aux groupes');
        {$IFDEF LINUX}
        FSolutions.Add('Capabilities : getcap fichier');
        {$ENDIF}
      end;
  end;
end;

function TSecurityTroubleshooter.GetSolutions: TStringList;  
begin
  Result := FSolutions;
end;

procedure TSecurityTroubleshooter.RunDiagnostics;
{$IFDEF LINUX}
var
  LinuxDiag: TLinuxSecurityDiagnostic;
{$ENDIF}
{$IFDEF WINDOWS}
var
  WinDiag: TWindowsSecurityDiagnostic;
{$ENDIF}
begin
  WriteLn('=== Diagnostic automatique ===');
  WriteLn('');

  {$IFDEF LINUX}
  LinuxDiag := TLinuxSecurityDiagnostic.Create;
  try
    LinuxDiag.DiagnoseSELinux;
    WriteLn('');
    LinuxDiag.DiagnoseAppArmor;
    WriteLn('');
    LinuxDiag.CheckRecentDenials;
  finally
    LinuxDiag.Free;
  end;
  {$ENDIF}

  {$IFDEF WINDOWS}
  WinDiag := TWindowsSecurityDiagnostic.Create;
  try
    WinDiag.DiagnoseDefender;
    WinDiag.CheckQuarantine;
    WinDiag.CheckThreatHistory;
  finally
    WinDiag.Free;
  end;
  {$ENDIF}
end;

end.
```

**Utilisation :**

```pascal
var
  Troubleshooter: TSecurityTroubleshooter;
  Solutions: TStringList;
  i: Integer;
begin
  // Diagnostiquer un problème d'accès fichier
  Troubleshooter := TSecurityTroubleshooter.Create(siFileAccessDenied);
  try
    WriteLn('Problème détecté : Accès fichier refusé');
    WriteLn('');

    // Exécuter les diagnostics
    Troubleshooter.RunDiagnostics;

    // Obtenir les solutions
    Solutions := Troubleshooter.GetSolutions;
    WriteLn('');
    WriteLn('Solutions suggérées :');
    for i := 0 to Solutions.Count - 1 do
      WriteLn('  ', i + 1, '. ', Solutions[i]);

  finally
    Troubleshooter.Free;
  end;
end;
```

## Recommandations finales

### Pour les développeurs

**Développement multi-plateforme :**

1. **Testez sur les deux plateformes** avec la sécurité activée
2. **Utilisez des chemins standard** pour éviter les problèmes
3. **Documentez les permissions requises** dans votre README
4. **Fournissez des scripts** de configuration SELinux/AppArmor
5. **Signez vos exécutables** Windows

**Exemple de documentation :**

````markdown
## Configuration de sécurité

### Linux (Ubuntu)

Si vous utilisez AppArmor, créez le profil suivant :

```bash
sudo nano /etc/apparmor.d/usr.local.bin.myapp
```

Contenu du profil :
````
/usr/local/bin/myapp {
  #include <abstractions/base>

  /usr/local/bin/myapp ix,
  /etc/myapp/** r,
  /var/lib/myapp/** rw,
  network inet,
}
```

Chargez le profil :
```bash
sudo apparmor_parser -r /etc/apparmor.d/usr.local.bin.myapp
```

### Windows

Si Windows Defender bloque l'application :

1. Téléchargez la version signée
2. OU ajoutez une exclusion :
   ```powershell
   Add-MpPreference -ExclusionPath "C:\Program Files\MyApp\"
   ```
```

### Pour les utilisateurs

**Installation sécurisée :**

```pascal
unit SecureInstaller;

interface

type
  TSecureInstaller = class
  public
    procedure CheckSecurityRequirements;
    procedure InstallWithSecurity;
    procedure ConfigureSecurity;
  end;

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}, WindowsDefenderExclusions{$ENDIF}
  {$IFDEF LINUX}, AppArmorProfileCreator{$ENDIF};

procedure TSecureInstaller.CheckSecurityRequirements;  
begin
  WriteLn('=== Vérification de la sécurité ===');

  {$IFDEF LINUX}
  WriteLn('Linux détecté');
  WriteLn('Vérification AppArmor/SELinux...');
  // Vérifier et informer
  {$ENDIF}

  {$IFDEF WINDOWS}
  WriteLn('Windows détecté');
  WriteLn('Vérification Windows Defender...');
  // Vérifier et informer
  {$ENDIF}
end;

procedure TSecureInstaller.InstallWithSecurity;  
begin
  WriteLn('Installation en cours...');

  // Copier les fichiers
  // ...

  WriteLn('Configuration de la sécurité...');
  ConfigureSecurity;
end;

procedure TSecureInstaller.ConfigureSecurity;  
begin
  {$IFDEF LINUX}
  WriteLn('Création du profil AppArmor...');
  // Créer le profil si nécessaire
  WriteLn('IMPORTANT : Exécutez ces commandes pour activer la sécurité :');
  WriteLn('  sudo apparmor_parser -r /etc/apparmor.d/usr.local.bin.myapp');
  {$ENDIF}

  {$IFDEF WINDOWS}
  WriteLn('Configuration Windows Defender...');
  WriteLn('Si l''application est bloquée, ajoutez une exclusion.');
  WriteLn('Ou téléchargez la version signée depuis notre site officiel.');
  {$ENDIF}
end;

end.
```

## Tableau récapitulatif

### Quand utiliser quoi ?

| Scénario | Linux (SELinux) | Linux (AppArmor) | Windows |
|----------|-----------------|------------------|---------|
| **Serveur haute sécurité** | ✓ Recommandé | Alternative | Defender + hardening |
| **Desktop Ubuntu** | - | ✓ Par défaut | N/A |
| **Application grand public** | Supporter | ✓ Supporter | ✓ Signer + exclusion |
| **Développement** | Mode permissif | Mode complain | Exclusions dev |
| **Production critique** | Mode enforcing | Mode enforce | Defender + monitoring |

### Niveaux d'effort

| Tâche | SELinux | AppArmor | Windows Defender |
|-------|---------|----------|------------------|
| Configuration initiale | ★★★★★ | ★★★☆☆ | ★☆☆☆☆ |
| Maintenance | ★★★★☆ | ★★☆☆☆ | ★☆☆☆☆ |
| Débogage | ★★★★★ | ★★★☆☆ | ★★☆☆☆ |
| Documentation | ★★★★☆ | ★★★☆☆ | ★★☆☆☆ |

## Conclusion

Les systèmes de sécurité Linux (SELinux/AppArmor) et Windows (Defender) ont des philosophies différentes mais le même objectif : protéger le système et les utilisateurs.

### Points clés à retenir

1. **Philosophie différente**
   - Linux : Contrôle d'accès proactif (tout interdit par défaut)
   - Windows : Détection réactive (tout permis sauf menaces)

2. **Complexité**
   - SELinux : Très complexe, très puissant
   - AppArmor : Équilibre simplicité/puissance
   - Windows Defender : Simple, moins granulaire

3. **Approche développeur**
   - Testez TOUJOURS avec la sécurité activée
   - Documentez les permissions requises
   - Fournissez des profils/exclusions prêts à l'emploi

4. **Multi-plateforme**
   - Utilisez des abstractions communes
   - Détectez le système de sécurité
   - Adaptez votre application en conséquence

### Checklist finale

**Pour le développement :**
```
[ ] Application testée avec SELinux enforcing
[ ] Application testée avec AppArmor enforce
[ ] Application testée avec Windows Defender actif
[ ] Profils SELinux/AppArmor créés et testés
[ ] Exécutable Windows signé numériquement
[ ] Documentation de sécurité rédigée
[ ] Scripts d'installation avec configuration sécurité
[ ] Logging des problèmes de sécurité implémenté
[ ] Tests d'intégration sur les deux plateformes
```

**Pour le déploiement :**
```
[ ] Instructions claires pour les administrateurs
[ ] Profils de sécurité inclus dans le package
[ ] Script de détection automatique du système
[ ] Documentation des exclusions nécessaires
[ ] Contact support pour problèmes de sécurité
```

### Ressources

**Linux :**
- SELinux : https://selinuxproject.org/
- AppArmor : https://apparmor.net/
- Documentation Ubuntu : https://ubuntu.com/security/certifications

**Windows :**
- Windows Defender : https://docs.microsoft.com/windows/security/
- Code Signing : https://docs.microsoft.com/windows/win32/seccrypto/
- SmartScreen : https://docs.microsoft.com/windows/security/threat-protection/

### Message final

Que vous développiez pour Linux ou Windows, comprendre les systèmes de sécurité est essentiel. Ne les voyez pas comme des obstacles mais comme des alliés qui protègent vos utilisateurs. Une application bien conçue cohabite harmonieusement avec SELinux, AppArmor ou Windows Defender.

**Règle d'or :** Si votre application est bloquée, ce n'est pas (toujours) la faute du système de sécurité - c'est l'occasion d'améliorer votre code et votre configuration !

⏭️ [Tests et Qualité du Code](/18-tests-qualite-code/README.md)
