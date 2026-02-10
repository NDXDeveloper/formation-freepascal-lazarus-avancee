🔝 Retour au [Sommaire](/SOMMAIRE.md)

# UAC et élévation de privilèges avec FreePascal/Lazarus sous Windows

## Introduction : Qu'est-ce que l'UAC ?

### Comprendre l'UAC (User Account Control)

L'**UAC (User Account Control)** ou **Contrôle de compte d'utilisateur** en français, est un mécanisme de sécurité introduit dans Windows Vista et présent dans toutes les versions ultérieures de Windows. Son rôle est de protéger votre système contre les modifications non autorisées.

**Principe de fonctionnement** :
- Par défaut, même les administrateurs travaillent avec des privilèges limités
- Quand une action nécessite des droits administrateur, Windows demande confirmation
- C'est la fameuse fenêtre qui demande "Voulez-vous autoriser cette application à apporter des modifications à votre appareil ?"

### Pourquoi l'UAC est important pour les développeurs

Votre application peut avoir besoin de privilèges élevés pour :
- Modifier des fichiers système
- Accéder au registre dans HKEY_LOCAL_MACHINE
- Installer des services Windows
- Modifier les paramètres réseau
- Accéder aux dossiers protégés (Program Files, Windows, etc.)
- Installer ou désinstaller des logiciels

### Les niveaux de privilèges

Windows distingue plusieurs niveaux de privilèges :

1. **Utilisateur standard** : Droits limités, ne peut pas modifier le système
2. **Administrateur (non élevé)** : A les droits admin mais fonctionne en mode limité par défaut
3. **Administrateur (élevé)** : Droits complets après confirmation UAC
4. **SYSTEM** : Le plus haut niveau, utilisé par Windows lui-même

## Concepts fondamentaux

### Le manifeste d'application

Le manifeste est un fichier XML qui indique à Windows comment votre application doit fonctionner vis-à-vis de l'UAC. Il définit le niveau de privilèges requis.

### Les niveaux d'exécution

Il existe trois niveaux d'exécution principaux :

```xml
<!-- asInvoker : Exécution avec les privilèges actuels de l'utilisateur -->
<requestedExecutionLevel level="asInvoker" uiAccess="false"/>

<!-- requireAdministrator : Demande toujours l'élévation -->
<requestedExecutionLevel level="requireAdministrator" uiAccess="false"/>

<!-- highestAvailable : Utilise le niveau le plus élevé disponible -->
<requestedExecutionLevel level="highestAvailable" uiAccess="false"/>
```

### L'icône du bouclier UAC

Windows affiche automatiquement une icône de bouclier sur les éléments qui nécessitent une élévation. C'est un indicateur visuel important pour l'utilisateur.

## Configuration du manifeste dans FreePascal/Lazarus

### Méthode 1 : Créer un fichier manifeste externe

Créez un fichier nommé `MonApplication.exe.manifest` :

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <assemblyIdentity
    version="1.0.0.0"
    processorArchitecture="*"
    name="MonEntreprise.MonApplication"
    type="win32"
  />
  <description>Mon Application FreePascal</description>

  <trustInfo xmlns="urn:schemas-microsoft-com:asm.v2">
    <security>
      <requestedPrivileges>
        <requestedExecutionLevel
          level="asInvoker"
          uiAccess="false"
        />
      </requestedPrivileges>
    </security>
  </trustInfo>

  <!-- Compatibilité Windows -->
  <compatibility xmlns="urn:schemas-microsoft-com:compatibility.v1">
    <application>
      <!-- Windows Vista -->
      <supportedOS Id="{e2011457-1546-43c5-a5fe-008deee3d3f0}"/>
      <!-- Windows 7 -->
      <supportedOS Id="{35138b9a-5d96-4fbd-8e2d-a2440225f93a}"/>
      <!-- Windows 8 -->
      <supportedOS Id="{4a2f28e3-53b9-4441-ba9c-d69d4a4a6e38}"/>
      <!-- Windows 8.1 -->
      <supportedOS Id="{1f676c76-80e1-4239-95bb-83d0f6d0da78}"/>
      <!-- Windows 10 et 11 -->
      <supportedOS Id="{8e0f7a12-bfb3-4fe8-b9a5-48fd50a15a9a}"/>
    </application>
  </compatibility>

  <!-- Configuration DPI pour les écrans haute résolution -->
  <application xmlns="urn:schemas-microsoft-com:asm.v3">
    <windowsSettings>
      <dpiAware xmlns="http://schemas.microsoft.com/SMI/2005/WindowsSettings">true</dpiAware>
      <dpiAwareness xmlns="http://schemas.microsoft.com/SMI/2016/WindowsSettings">PerMonitorV2</dpiAwareness>
    </windowsSettings>
  </application>
</assembly>
```

### Méthode 2 : Intégrer le manifeste dans l'exécutable

Dans Lazarus, utilisez les options du projet :

1. **Menu Projet** → **Options du projet**
2. **Onglet Options** → **Ressources**
3. Cochez **Utiliser le fichier manifeste**
4. Sélectionnez ou créez votre fichier manifeste

Ou directement dans le code source avec une directive :

```pascal
{$R MonManifeste.res}
```

Créez d'abord le fichier ressource avec :
```pascal
program CreerRessourceManifeste;

var
  F: TextFile;
begin
  AssignFile(F, 'MonManifeste.rc');
  Rewrite(F);
  WriteLn(F, '1 24 "MonApplication.exe.manifest"');
  CloseFile(F);

  // Compiler avec : windres MonManifeste.rc -o MonManifeste.res
end;
```

## Détection et vérification des privilèges

### Vérifier si l'application s'exécute avec des droits élevés

```pascal
uses
  Windows, SysUtils;

function EstExecuteEnTantQuAdmin: Boolean;
var
  TokenHandle: THandle;
  Elevation: TOKEN_ELEVATION;
  ReturnLength: DWORD;
begin
  Result := False;

  // Windows XP et antérieur n'ont pas l'UAC
  if Win32MajorVersion < 6 then
  begin
    Result := EstAdministrateur;
    Exit;
  end;

  // Ouvrir le token du processus
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
  try
    // Obtenir l'information d'élévation
    if GetTokenInformation(TokenHandle, TokenElevation, @Elevation,
                          SizeOf(Elevation), ReturnLength) then
      Result := Elevation.TokenIsElevated <> 0;
  finally
    CloseHandle(TokenHandle);
  end;
end;

function EstAdministrateur: Boolean;
var
  TokenHandle: THandle;
  TokenInformation: TOKEN_GROUPS;
  InfoBufferSize: DWORD;
  AdminSID: PSID;
  SIDAuthority: SID_IDENTIFIER_AUTHORITY;
  i: Integer;
begin
  Result := False;
  SIDAuthority := SECURITY_NT_AUTHORITY;

  // Créer le SID du groupe Administrateurs
  if not AllocateAndInitializeSid(SIDAuthority, 2,
    SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
    0, 0, 0, 0, 0, 0, AdminSID) then
    Exit;

  try
    // Ouvrir le token
    if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
    try
      // Obtenir la taille nécessaire
      GetTokenInformation(TokenHandle, TokenGroups, nil, 0, InfoBufferSize);
      GetMem(TokenInformation, InfoBufferSize);

      try
        // Obtenir les groupes
        if GetTokenInformation(TokenHandle, TokenGroups, TokenInformation,
                              InfoBufferSize, InfoBufferSize) then
        begin
          // Vérifier si le SID Admin est présent
          for i := 0 to TokenInformation.GroupCount - 1 do
          begin
            if EqualSid(TokenInformation.Groups[i].Sid, AdminSID) then
            begin
              Result := True;
              Break;
            end;
          end;
        end;
      finally
        FreeMem(TokenInformation);
      end;
    finally
      CloseHandle(TokenHandle);
    end;
  finally
    FreeSid(AdminSID);
  end;
end;
```

### Vérifier si l'UAC est activé

```pascal
uses
  Registry;

function UACEstActive: Boolean;
var
  Reg: TRegistry;
  EnableLUA: Integer;
begin
  Result := True;  // Par défaut, supposer que l'UAC est actif

  // Windows XP et antérieur n'ont pas l'UAC
  if Win32MajorVersion < 6 then
  begin
    Result := False;
    Exit;
  end;

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System') then
    begin
      try
        if Reg.ValueExists('EnableLUA') then
        begin
          EnableLUA := Reg.ReadInteger('EnableLUA');
          Result := EnableLUA <> 0;
        end;
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function ObtenirNiveauUAC: Integer;
var
  Reg: TRegistry;
begin
  Result := 2;  // Niveau par défaut

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System') then
    begin
      try
        if Reg.ValueExists('ConsentPromptBehaviorAdmin') then
          Result := Reg.ReadInteger('ConsentPromptBehaviorAdmin');
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;

  // Niveaux UAC :
  // 0 = Jamais notifier (UAC désactivé)
  // 1 = Notifier uniquement quand des programmes tentent d'apporter des modifications
  // 2 = Toujours notifier (par défaut)
  // 5 = Toujours notifier et attendre la réponse sur le bureau sécurisé
end;
```

## Demander l'élévation de privilèges

### Méthode 1 : Relancer l'application avec élévation

```pascal
uses
  Windows, ShellAPI, SysUtils;

function RelancerAvecElevation: Boolean;
var
  ExeName: string;
  Params: string;
  i: Integer;
begin
  Result := False;

  // Vérifier si déjà élevé
  if EstExecuteEnTantQuAdmin then
  begin
    ShowMessage('L''application a déjà les privilèges administrateur.');
    Exit;
  end;

  ExeName := ParamStr(0);

  // Reconstruire les paramètres
  Params := '';
  for i := 1 to ParamCount do
  begin
    if Pos(' ', ParamStr(i)) > 0 then
      Params := Params + '"' + ParamStr(i) + '" '
    else
      Params := Params + ParamStr(i) + ' ';
  end;

  // Ajouter un paramètre pour indiquer le redémarrage
  Params := Params + '/elevated';

  // Utiliser ShellExecute avec le verbe "runas"
  Result := ShellExecute(0, 'runas', PChar(ExeName), PChar(Params),
                        nil, SW_SHOWNORMAL) > 32;

  if Result then
  begin
    // Fermer l'instance actuelle
    Application.Terminate;
  end
  else
  begin
    ShowMessage('Impossible d''obtenir les privilèges administrateur.');
  end;
end;

// Utilisation dans le programme principal
begin
  // Vérifier si l'application nécessite l'élévation
  if not EstExecuteEnTantQuAdmin then
  begin
    if MessageDlg('Cette application nécessite des privilèges administrateur. ' +
                  'Voulez-vous la relancer avec élévation ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      RelancerAvecElevation;
      Exit;
    end;
  end;

  // Code principal de l'application...
end;
```

### Méthode 2 : Élever une action spécifique

```pascal
function ExecuterCommandeElevee(const Commande, Parametres: string): Boolean;
var
  SEI: TShellExecuteInfo;
begin
  FillChar(SEI, SizeOf(SEI), 0);
  SEI.cbSize := SizeOf(TShellExecuteInfo);
  SEI.Wnd := Application.Handle;
  SEI.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  SEI.lpVerb := 'runas';  // Verbe pour demander l'élévation
  SEI.lpFile := PChar(Commande);
  SEI.lpParameters := PChar(Parametres);
  SEI.lpDirectory := nil;
  SEI.nShow := SW_SHOWNORMAL;

  Result := ShellExecuteEx(@SEI);

  if not Result then
  begin
    case GetLastError of
      ERROR_CANCELLED:
        ShowMessage('L''utilisateur a annulé l''élévation.');
      ERROR_FILE_NOT_FOUND:
        ShowMessage('Fichier non trouvé : ' + Commande);
      else
        ShowMessage('Erreur lors de l''élévation : ' + SysErrorMessage(GetLastError));
    end;
  end;
end;

// Exemple d'utilisation
procedure ModifierFichierSysteme;
begin
  if not EstExecuteEnTantQuAdmin then
  begin
    // Lancer un outil externe avec élévation
    ExecuterCommandeElevee('notepad.exe', 'C:\Windows\System32\drivers\etc\hosts');
  end
  else
  begin
    // Nous avons déjà les privilèges, modifier directement
    // ... code de modification ...
  end;
end;
```

## Interface utilisateur et UAC

### Ajouter l'icône du bouclier UAC

```pascal
uses
  Windows, CommCtrl, Buttons;

const
  BCM_SETSHIELD = $160C;

procedure AjouterBouclierUAC(Button: TButton);
begin
  // Envoyer le message pour afficher le bouclier
  SendMessage(Button.Handle, BCM_SETSHIELD, 0, LPARAM(True));
end;

procedure AjouterBouclierSurBouton(Button: TBitBtn);
var
  Icon: TIcon;
  IconHandle: HICON;
begin
  // Méthode alternative : charger l'icône du bouclier
  Icon := TIcon.Create;
  try
    // Charger l'icône du bouclier système
    IconHandle := LoadIcon(0, IDI_SHIELD);
    if IconHandle <> 0 then
    begin
      Icon.Handle := IconHandle;
      Button.Glyph.Assign(Icon);
    end;
  finally
    Icon.Free;
  end;
end;

// Utilisation
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Vérifier si l'action nécessite l'élévation
  if not EstExecuteEnTantQuAdmin then
  begin
    AjouterBouclierUAC(BtnModifierSysteme);
    BtnModifierSysteme.Caption := 'Modifier (Admin requis)';
  end;
end;

procedure TForm1.BtnModifierSystemeClick(Sender: TObject);
begin
  if not EstExecuteEnTantQuAdmin then
  begin
    if MessageDlg('Cette action nécessite des privilèges administrateur. Continuer ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      RelancerAvecElevation;
    end;
  end
  else
  begin
    // Effectuer l'action privilégiée
    ModifierParametresSysteme;
  end;
end;
```

### Créer une interface adaptative

```pascal
type
  TMainForm = class(TForm)
    PanelAdmin: TPanel;
    PanelUser: TPanel;
    LabelStatut: TLabel;
    BtnElevation: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnElevationClick(Sender: TObject);
  private
    procedure AdapterInterface;
  end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AdapterInterface;
end;

procedure TMainForm.AdapterInterface;
begin
  if EstExecuteEnTantQuAdmin then
  begin
    // Mode administrateur
    Caption := Application.Title + ' [Administrateur]';
    LabelStatut.Caption := 'Exécution avec privilèges administrateur';
    LabelStatut.Font.Color := clGreen;

    PanelAdmin.Visible := True;
    PanelAdmin.Enabled := True;
    PanelUser.Visible := True;

    BtnElevation.Visible := False;
  end
  else
  begin
    // Mode utilisateur standard
    Caption := Application.Title + ' [Utilisateur standard]';
    LabelStatut.Caption := 'Exécution avec privilèges limités';
    LabelStatut.Font.Color := clMaroon;

    PanelAdmin.Visible := True;
    PanelAdmin.Enabled := False;  // Désactivé mais visible
    PanelUser.Visible := True;

    BtnElevation.Visible := True;
    AjouterBouclierUAC(BtnElevation);
    BtnElevation.Caption := 'Obtenir les droits administrateur';
  end;

  // Afficher des informations supplémentaires
  if UACEstActive then
    LabelStatut.Caption := LabelStatut.Caption + ' (UAC actif)'
  else
    LabelStatut.Caption := LabelStatut.Caption + ' (UAC désactivé)';
end;

procedure TMainForm.BtnElevationClick(Sender: TObject);
begin
  RelancerAvecElevation;
end;
```

## Gestion des opérations privilégiées

### Séparer les opérations selon les privilèges

```pascal
unit OperationsPrivilegiees;

interface

type
  TOperationPrivilegiee = class
  private
    FEstEleve: Boolean;
  public
    constructor Create;
    function PeutModifierSysteme: Boolean;
    function PeutAccederRegistreSystem: Boolean;
    function PeutInstallerService: Boolean;

    // Opérations
    function ModifierFichierSysteme(const Fichier: string): Boolean;
    function EcrireRegistreSystem(const Cle, Valeur: string): Boolean;
    function InstallerService(const NomService, CheminExe: string): Boolean;
  end;

implementation

uses
  Windows, SysUtils, Registry;

constructor TOperationPrivilegiee.Create;
begin
  FEstEleve := EstExecuteEnTantQuAdmin;
end;

function TOperationPrivilegiee.PeutModifierSysteme: Boolean;
begin
  Result := FEstEleve;
end;

function TOperationPrivilegiee.PeutAccederRegistreSystem: Boolean;
begin
  Result := FEstEleve;
end;

function TOperationPrivilegiee.PeutInstallerService: Boolean;
begin
  Result := FEstEleve;
end;

function TOperationPrivilegiee.ModifierFichierSysteme(const Fichier: string): Boolean;
begin
  Result := False;

  if not FEstEleve then
  begin
    ShowMessage('Privilèges administrateur requis pour modifier : ' + Fichier);
    Exit;
  end;

  try
    // Code de modification du fichier
    // ...
    Result := True;
  except
    on E: Exception do
      ShowMessage('Erreur lors de la modification : ' + E.Message);
  end;
end;

function TOperationPrivilegiee.EcrireRegistreSystem(const Cle, Valeur: string): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;

  if not FEstEleve then
  begin
    ShowMessage('Privilèges administrateur requis pour modifier le registre système');
    Exit;
  end;

  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKey(Cle, True) then
    begin
      try
        Reg.WriteString('MonApplication', Valeur);
        Result := True;
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function TOperationPrivilegiee.InstallerService(const NomService, CheminExe: string): Boolean;
var
  SCManager, Service: SC_HANDLE;
begin
  Result := False;

  if not FEstEleve then
  begin
    ShowMessage('Privilèges administrateur requis pour installer un service');
    Exit;
  end;

  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CREATE_SERVICE);
  if SCManager <> 0 then
  begin
    try
      Service := CreateService(
        SCManager,
        PChar(NomService),
        PChar(NomService),
        SERVICE_ALL_ACCESS,
        SERVICE_WIN32_OWN_PROCESS,
        SERVICE_AUTO_START,
        SERVICE_ERROR_NORMAL,
        PChar(CheminExe),
        nil,
        nil,
        nil,
        nil,
        nil
      );

      if Service <> 0 then
      begin
        CloseServiceHandle(Service);
        Result := True;
        ShowMessage('Service installé avec succès');
      end
      else
        ShowMessage('Erreur lors de l''installation du service : ' +
                   SysErrorMessage(GetLastError));
    finally
      CloseServiceHandle(SCManager);
    end;
  end
  else
    ShowMessage('Impossible d''ouvrir le gestionnaire de services');
end;

end.
```

## Communication entre processus élevés et non élevés

### Utiliser des pipes nommés pour la communication

```pascal
unit CommunicationElevee;

interface

uses
  Windows, SysUtils, Classes;

type
  TProcessusCommunication = class
  private
    FPipeHandle: THandle;
    FNomPipe: string;
  public
    constructor Create(const NomPipe: string);
    destructor Destroy; override;

    function CreerServeurPipe: Boolean;
    function ConnecterClientPipe: Boolean;
    function EnvoyerMessage(const Message: string): Boolean;
    function RecevoirMessage(out Message: string): Boolean;
  end;

implementation

constructor TProcessusCommunication.Create(const NomPipe: string);
begin
  FNomPipe := '\\.\pipe\' + NomPipe;
  FPipeHandle := INVALID_HANDLE_VALUE;
end;

destructor TProcessusCommunication.Destroy;
begin
  if FPipeHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FPipeHandle);
  inherited;
end;

function TProcessusCommunication.CreerServeurPipe: Boolean;
var
  SecurityAttr: TSecurityAttributes;
  SecurityDesc: TSecurityDescriptor;
begin
  // Créer un descripteur de sécurité permettant l'accès à tous
  InitializeSecurityDescriptor(@SecurityDesc, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@SecurityDesc, True, nil, False);

  SecurityAttr.nLength := SizeOf(TSecurityAttributes);
  SecurityAttr.lpSecurityDescriptor := @SecurityDesc;
  SecurityAttr.bInheritHandle := False;

  FPipeHandle := CreateNamedPipe(
    PChar(FNomPipe),
    PIPE_ACCESS_DUPLEX,
    PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
    1,  // Une seule instance
    4096,  // Taille buffer sortie
    4096,  // Taille buffer entrée
    0,  // Timeout par défaut
    @SecurityAttr
  );

  Result := FPipeHandle <> INVALID_HANDLE_VALUE;

  if Result then
  begin
    // Attendre la connexion d'un client
    ConnectNamedPipe(FPipeHandle, nil);
  end;
end;

function TProcessusCommunication.ConnecterClientPipe: Boolean;
begin
  FPipeHandle := CreateFile(
    PChar(FNomPipe),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    0,
    0
  );

  Result := FPipeHandle <> INVALID_HANDLE_VALUE;
end;

function TProcessusCommunication.EnvoyerMessage(const Message: string): Boolean;
var
  BytesWritten: DWORD;
  Buffer: AnsiString;
begin
  Result := False;

  if FPipeHandle = INVALID_HANDLE_VALUE then
    Exit;

  Buffer := AnsiString(Message);
  Result := WriteFile(FPipeHandle, Buffer[1], Length(Buffer), BytesWritten, nil);
end;

function TProcessusCommunication.RecevoirMessage(out Message: string): Boolean;
var
  Buffer: array[0..4095] of AnsiChar;
  BytesRead: DWORD;
begin
  Result := False;
  Message := '';

  if FPipeHandle = INVALID_HANDLE_VALUE then
    Exit;

  if ReadFile(FPipeHandle, Buffer, SizeOf(Buffer), BytesRead, nil) then
  begin
    SetString(Message, Buffer, BytesRead);
    Result := True;
  end;
end;

end.
```

### Exemple d'utilisation de la communication

```pascal
program ProcessusEleve;

uses
  CommunicationElevee;

var
  Comm: TProcessusCommunication;
  Message: string;
begin
  // Processus élevé agissant comme serveur
  Comm := TProcessusCommunication.Create('MonApplicationPipe');
  try
    WriteLn('Création du serveur pipe (processus élevé)...');

    if Comm.CreerServeurPipe then
    begin
      WriteLn('En attente de messages...');

      while Comm.RecevoirMessage(Message) do
      begin
        WriteLn('Reçu : ', Message);

        // Traiter le message
        if Message = 'MODIFIER_SYSTEME' then
        begin
          // Effectuer l'opération privilégiée
          WriteLn('Modification système en cours...');

          // Envoyer la réponse
          Comm.EnvoyerMessage('OK: Modification effectuée');
        end
        else if Message = 'QUIT' then
          Break;
      end;
    end;
  finally
    Comm.Free;
  end;
end;

program ProcessusNormal;

uses
  CommunicationElevee;

var
  Comm: TProcessusCommunication;
  Reponse: string;
begin
  // Processus normal agissant comme client
  Comm := TProcessusCommunication.Create('MonApplicationPipe');
  try
    WriteLn('Connexion au processus élevé...');

    if Comm.ConnecterClientPipe then
    begin
      WriteLn('Connecté !');

      // Demander une opération privilégiée
      Comm.EnvoyerMessage('MODIFIER_SYSTEME');

      if Comm.RecevoirMessage(Reponse) then
        WriteLn('Réponse : ', Reponse);

      // Terminer
      Comm.EnvoyerMessage('QUIT');
    end
    else
      WriteLn('Impossible de se connecter au processus élevé');
  finally
    Comm.Free;
  end;
end;
```

## Bonnes pratiques et sécurité

### 1. Principe du moindre privilège

```pascal
procedure ExecuterTache;
begin
  // Toujours vérifier si l'élévation est vraiment nécessaire
  if TacheNecessiteElevation then
  begin
    if not EstExecuteEnTantQuAdmin then
    begin
      // Demander l'élévation seulement si nécessaire
      if ConfirmerAvecUtilisateur then
        RelancerAvecElevation;
    end
    else
      ExecuterTachePrivilegiee;
  end
  else
    ExecuterTacheNormale;  // Pas besoin d'élévation
end;
```

### 2. Validation des entrées

```pascal
function ValiderCheminSecurise(const Chemin: string): Boolean;
begin
  Result := False;

  // Vérifier que le chemin ne contient pas de caractères dangereux
  if Pos('..', Chemin) > 0 then
    Exit;

  // Vérifier que le chemin est absolu
  if not ((Length(Chemin) >= 3) and (Chemin[2] = ':') and (Chemin[3] = '\')) then
    Exit;

  // Vérifier que le chemin existe
  if not DirectoryExists(ExtractFilePath(Chemin)) then
    Exit;

  // Vérifier qu'on n'accède pas à des zones sensibles sans raison
  if ContainsText(Chemin, '\System32\') or
     ContainsText(Chemin, '\Windows\') or
     ContainsText(Chemin, '\Program Files\') then
  begin
    // Demander confirmation supplémentaire
    Result := MessageDlg('Attention : vous allez modifier un fichier système. Continuer ?',
                        mtWarning, [mbYes, mbNo], 0) = mrYes;
  end
  else
    Result := True;
end;

function ValiderParametresSecurises(const Params: string): Boolean;
var
  ParamsInterdits: array[0..4] of string = ('&', '|', '>', '<', ';');
  i: Integer;
begin
  Result := True;

  // Vérifier l'absence de caractères shell dangereux
  for i := 0 to High(ParamsInterdits) do
  begin
    if Pos(ParamsInterdits[i], Params) > 0 then
    begin
      Result := False;
      ShowMessage('Paramètres invalides : caractères interdits détectés');
      Exit;
    end;
  end;

  // Limiter la longueur des paramètres
  if Length(Params) > 1024 then
  begin
    Result := False;
    ShowMessage('Paramètres trop longs');
  end;
end;
```

### 3. Journalisation des actions privilégiées

```pascal
unit JournalisationUAC;

interface

uses
  Windows, SysUtils, Classes;

type
  TJournalUAC = class
  private
    FNomFichier: string;
    FHandle: TextFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LogElevation(Reussie: Boolean; const Details: string);
    procedure LogActionPrivilegiee(const Action, Resultat: string);
    procedure LogErreurSecurite(const Erreur: string);
  end;

implementation

constructor TJournalUAC.Create;
var
  CheminLog: string;
begin
  // Stocker les logs dans un dossier sécurisé
  CheminLog := GetEnvironmentVariable('ProgramData') + '\MonApplication\Logs\';
  ForceDirectories(CheminLog);

  FNomFichier := CheminLog + 'UAC_' + FormatDateTime('yyyymmdd', Now) + '.log';

  AssignFile(FHandle, FNomFichier);
  if FileExists(FNomFichier) then
    Append(FHandle)
  else
    Rewrite(FHandle);
end;

destructor TJournalUAC.Destroy;
begin
  CloseFile(FHandle);
  inherited;
end;

procedure TJournalUAC.LogElevation(Reussie: Boolean; const Details: string);
var
  Statut: string;
begin
  if Reussie then
    Statut := 'SUCCÈS'
  else
    Statut := 'ÉCHEC';

  WriteLn(FHandle, Format('[%s] ÉLÉVATION UAC - %s - Utilisateur: %s - Détails: %s',
    [DateTimeToStr(Now), Statut, GetEnvironmentVariable('USERNAME'), Details]));
  Flush(FHandle);
end;

procedure TJournalUAC.LogActionPrivilegiee(const Action, Resultat: string);
begin
  WriteLn(FHandle, Format('[%s] ACTION PRIVILÉGIÉE - %s - Résultat: %s - PID: %d',
    [DateTimeToStr(Now), Action, Resultat, GetCurrentProcessId]));
  Flush(FHandle);
end;

procedure TJournalUAC.LogErreurSecurite(const Erreur: string);
begin
  WriteLn(FHandle, Format('[%s] ERREUR SÉCURITÉ - %s - Code: %d',
    [DateTimeToStr(Now), Erreur, GetLastError]));
  Flush(FHandle);

  // Optionnel : enregistrer aussi dans l'observateur d'événements Windows
  EnregistrerDansObservateurEvenements(Erreur);
end;

procedure EnregistrerDansObservateurEvenements(const Message: string);
var
  EventLog: THandle;
  Messages: array[0..0] of PChar;
begin
  EventLog := RegisterEventSource(nil, 'MonApplication');
  if EventLog <> 0 then
  begin
    Messages[0] := PChar(Message);
    ReportEvent(EventLog, EVENTLOG_WARNING_TYPE, 0, 0, nil,
                1, 0, @Messages, nil);
    DeregisterEventSource(EventLog);
  end;
end;

end.
```

### 4. Gestion sécurisée des tokens

```pascal
unit GestionTokens;

interface

uses
  Windows, SysUtils;

type
  TTokenManager = class
  private
    FTokenHandle: THandle;
    function ObtenirPrivilege(const NomPrivilege: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ActiverPrivilegeDebug: Boolean;
    function ActiverPrivilegeBackup: Boolean;
    function ActiverPrivilegeRestore: Boolean;
    function ActiverPrivilegeShutdown: Boolean;
    function DesactiverPrivilege(const NomPrivilege: string): Boolean;

    function ObtenirInfosToken: string;
    function ImpersonnerUtilisateur(const NomUtilisateur: string): Boolean;
    procedure RevenirIdentiteOriginale;
  end;

implementation

constructor TTokenManager.Create;
begin
  if not OpenProcessToken(GetCurrentProcess,
                          TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
                          FTokenHandle) then
    RaiseLastOSError;
end;

destructor TTokenManager.Destroy;
begin
  if FTokenHandle <> 0 then
    CloseHandle(FTokenHandle);
  inherited;
end;

function TTokenManager.ObtenirPrivilege(const NomPrivilege: string): Boolean;
var
  TokenPriv, PrevTokenPriv: TTokenPrivileges;
  ReturnLength: DWORD;
begin
  Result := False;

  // Rechercher le LUID du privilège
  if not LookupPrivilegeValue(nil, PChar(NomPrivilege), TokenPriv.Privileges[0].Luid) then
    Exit;

  TokenPriv.PrivilegeCount := 1;
  TokenPriv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

  // Ajuster les privilèges du token
  Result := AdjustTokenPrivileges(FTokenHandle, False, TokenPriv,
                                  SizeOf(PrevTokenPriv), PrevTokenPriv, ReturnLength);

  if Result and (GetLastError = ERROR_NOT_ALL_ASSIGNED) then
    Result := False;
end;

function TTokenManager.ActiverPrivilegeDebug: Boolean;
begin
  Result := ObtenirPrivilege('SeDebugPrivilege');
end;

function TTokenManager.ActiverPrivilegeBackup: Boolean;
begin
  Result := ObtenirPrivilege('SeBackupPrivilege');
end;

function TTokenManager.ActiverPrivilegeRestore: Boolean;
begin
  Result := ObtenirPrivilege('SeRestorePrivilege');
end;

function TTokenManager.ActiverPrivilegeShutdown: Boolean;
begin
  Result := ObtenirPrivilege('SeShutdownPrivilege');
end;

function TTokenManager.DesactiverPrivilege(const NomPrivilege: string): Boolean;
var
  TokenPriv: TTokenPrivileges;
begin
  Result := False;

  if not LookupPrivilegeValue(nil, PChar(NomPrivilege), TokenPriv.Privileges[0].Luid) then
    Exit;

  TokenPriv.PrivilegeCount := 1;
  TokenPriv.Privileges[0].Attributes := 0;  // Désactiver

  Result := AdjustTokenPrivileges(FTokenHandle, False, TokenPriv, 0, nil, PDWORD(nil)^);
end;

function TTokenManager.ObtenirInfosToken: string;
var
  TokenUser: ^TOKEN_USER;
  InfoLength: DWORD;
  AccountName, DomainName: array[0..255] of Char;
  AccountLen, DomainLen: DWORD;
  SidType: SID_NAME_USE;
begin
  Result := '';

  // Obtenir la taille nécessaire
  GetTokenInformation(FTokenHandle, TokenUser, nil, 0, InfoLength);
  GetMem(TokenUser, InfoLength);

  try
    if GetTokenInformation(FTokenHandle, TokenUser, TokenUser, InfoLength, InfoLength) then
    begin
      AccountLen := SizeOf(AccountName);
      DomainLen := SizeOf(DomainName);

      if LookupAccountSid(nil, TokenUser.User.Sid, AccountName, AccountLen,
                          DomainName, DomainLen, SidType) then
      begin
        Result := Format('Utilisateur: %s\%s', [DomainName, AccountName]);
      end;
    end;
  finally
    FreeMem(TokenUser);
  end;
end;

function TTokenManager.ImpersonnerUtilisateur(const NomUtilisateur: string): Boolean;
var
  Token: THandle;
begin
  Result := False;

  // Cette fonction nécessite des privilèges spéciaux
  if not EstExecuteEnTantQuAdmin then
    Exit;

  // Code d'impersonnation...
  // (Complexe, nécessite LogonUser et ImpersonateLoggedOnUser)
end;

procedure TTokenManager.RevenirIdentiteOriginale;
begin
  RevertToSelf;
end;

end.
```

## Cas d'usage avancés

### Créer un helper d'élévation

```pascal
program HelperElevation;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, ShellAPI, Classes;

type
  TElevationHelper = class
  private
    FCommande: string;
    FParametres: string;
    FDossierTravail: string;
    FAttendre: Boolean;
    FCodeSortie: Integer;

    procedure ParseCommandLine;
  public
    constructor Create;
    function Executer: Boolean;
    property CodeSortie: Integer read FCodeSortie;
  end;

constructor TElevationHelper.Create;
begin
  ParseCommandLine;
  FCodeSortie := 0;
end;

procedure TElevationHelper.ParseCommandLine;
var
  i: Integer;
  Param: string;
begin
  FAttendre := False;

  for i := 1 to ParamCount do
  begin
    Param := ParamStr(i);

    if Param = '/wait' then
      FAttendre := True
    else if Param = '/cmd' then
    begin
      if i < ParamCount then
      begin
        Inc(i);
        FCommande := ParamStr(i);
      end;
    end
    else if Param = '/params' then
    begin
      if i < ParamCount then
      begin
        Inc(i);
        FParametres := ParamStr(i);
      end;
    end
    else if Param = '/dir' then
    begin
      if i < ParamCount then
      begin
        Inc(i);
        FDossierTravail := ParamStr(i);
      end;
    end;
  end;
end;

function TElevationHelper.Executer: Boolean;
var
  SEI: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  Result := False;

  if FCommande = '' then
  begin
    WriteLn('Erreur : aucune commande spécifiée');
    FCodeSortie := 1;
    Exit;
  end;

  WriteLn('Demande d''élévation pour : ', FCommande);
  WriteLn('Paramètres : ', FParametres);

  FillChar(SEI, SizeOf(SEI), 0);
  SEI.cbSize := SizeOf(TShellExecuteInfo);
  SEI.fMask := SEE_MASK_NOCLOSEPROCESS;
  SEI.lpVerb := 'runas';
  SEI.lpFile := PChar(FCommande);
  SEI.lpParameters := PChar(FParametres);
  SEI.lpDirectory := PChar(FDossierTravail);
  SEI.nShow := SW_SHOWNORMAL;

  if ShellExecuteEx(@SEI) then
  begin
    Result := True;

    if FAttendre and (SEI.hProcess <> 0) then
    begin
      WriteLn('En attente de la fin du processus...');
      WaitForSingleObject(SEI.hProcess, INFINITE);

      if GetExitCodeProcess(SEI.hProcess, ExitCode) then
        FCodeSortie := ExitCode
      else
        FCodeSortie := -1;

      CloseHandle(SEI.hProcess);
      WriteLn('Processus terminé avec le code : ', FCodeSortie);
    end;
  end
  else
  begin
    FCodeSortie := GetLastError;
    WriteLn('Échec de l''élévation. Code erreur : ', FCodeSortie);
  end;
end;

// Programme principal
var
  Helper: TElevationHelper;
begin
  try
    Helper := TElevationHelper.Create;
    try
      if not Helper.Executer then
        ExitCode := Helper.CodeSortie;
    finally
      Helper.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur : ', E.Message);
      ExitCode := 999;
    end;
  end;
end.
```

### Gestionnaire de configuration avec élévation

```pascal
unit ConfigurationElevee;

interface

uses
  Windows, SysUtils, Classes, Registry, IniFiles;

type
  TConfigurationManager = class
  private
    FNeedElevation: Boolean;
    FConfigFile: string;
    FSystemConfig: Boolean;

    function GetConfigPath: string;
    function TenterEcriture(const Chemin: string): Boolean;
  public
    constructor Create(SystemeConfig: Boolean = False);

    function LireValeur(const Section, Cle: string; Defaut: string = ''): string;
    function EcrireValeur(const Section, Cle, Valeur: string): Boolean;

    function LireRegistre(const Cle, Valeur: string): string;
    function EcrireRegistre(const Cle, Nom, Valeur: string): Boolean;

    property NecessiteElevation: Boolean read FNeedElevation;
  end;

implementation

constructor TConfigurationManager.Create(SystemeConfig: Boolean);
begin
  FSystemConfig := SystemeConfig;
  FConfigFile := GetConfigPath;

  // Tester si on peut écrire dans la configuration
  FNeedElevation := SystemeConfig and not TenterEcriture(FConfigFile);
end;

function TConfigurationManager.GetConfigPath: string;
begin
  if FSystemConfig then
  begin
    // Configuration système (nécessite admin)
    Result := GetEnvironmentVariable('ProgramData') + '\MonApplication\config.ini';
  end
  else
  begin
    // Configuration utilisateur
    Result := GetEnvironmentVariable('APPDATA') + '\MonApplication\config.ini';
  end;

  ForceDirectories(ExtractFilePath(Result));
end;

function TConfigurationManager.TenterEcriture(const Chemin: string): Boolean;
var
  F: TextFile;
begin
  Result := False;
  try
    AssignFile(F, Chemin + '.test');
    Rewrite(F);
    CloseFile(F);
    DeleteFile(Chemin + '.test');
    Result := True;
  except
    // Pas de droits d'écriture
  end;
end;

function TConfigurationManager.LireValeur(const Section, Cle: string; Defaut: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FConfigFile);
  try
    Result := Ini.ReadString(Section, Cle, Defaut);
  finally
    Ini.Free;
  end;
end;

function TConfigurationManager.EcrireValeur(const Section, Cle, Valeur: string): Boolean;
var
  Ini: TIniFile;
begin
  Result := False;

  if FNeedElevation and not EstExecuteEnTantQuAdmin then
  begin
    ShowMessage('Cette opération nécessite des privilèges administrateur');

    // Lancer un processus élevé pour écrire la configuration
    if RelancerAvecElevation then
      Exit;
  end;

  try
    Ini := TIniFile.Create(FConfigFile);
    try
      Ini.WriteString(Section, Cle, Valeur);
      Result := True;
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur lors de l''écriture : ' + E.Message);
  end;
end;

function TConfigurationManager.LireRegistre(const Cle, Valeur: string): string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  try
    if FSystemConfig then
      Reg.RootKey := HKEY_LOCAL_MACHINE
    else
      Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKeyReadOnly(Cle) then
    begin
      try
        if Reg.ValueExists(Valeur) then
          Result := Reg.ReadString(Valeur);
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function TConfigurationManager.EcrireRegistre(const Cle, Nom, Valeur: string): Boolean;
var
  Reg: TRegistry;
  Access: DWORD;
begin
  Result := False;

  if FSystemConfig and not EstExecuteEnTantQuAdmin then
  begin
    ShowMessage('Modification du registre système : privilèges administrateur requis');
    Exit;
  end;

  if FSystemConfig then
    Access := KEY_WRITE
  else
    Access := KEY_WRITE;

  Reg := TRegistry.Create(Access);
  try
    if FSystemConfig then
      Reg.RootKey := HKEY_LOCAL_MACHINE
    else
      Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(Cle, True) then
    begin
      try
        Reg.WriteString(Nom, Valeur);
        Result := True;
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

end.
```

## Exemple complet d'application avec gestion UAC

```pascal
program ApplicationAvecUAC;

uses
  Forms, Windows, SysUtils, Dialogs;

{$R *.res}
{$R Manifeste.res}  // Manifeste avec requestedExecutionLevel="asInvoker"

type
  TMainForm = class(TForm)
    BtnOperationNormale: TButton;
    BtnOperationAdmin: TButton;
    MemoLog: TMemo;
    StatusBar: TStatusBar;

    procedure FormCreate(Sender: TObject);
    procedure BtnOperationNormaleClick(Sender: TObject);
    procedure BtnOperationAdminClick(Sender: TObject);
  private
    FJournal: TJournalUAC;
    FTokenManager: TTokenManager;
    FConfigManager: TConfigurationManager;

    procedure InitialiserApplication;
    procedure AfficherStatutUAC;
    procedure EffectuerOperationNormale;
    procedure EffectuerOperationPrivilegiee;
    function DemanderElevation: Boolean;
  end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InitialiserApplication;
end;

procedure TMainForm.InitialiserApplication;
begin
  // Créer les gestionnaires
  FJournal := TJournalUAC.Create;
  FConfigManager := TConfigurationManager.Create(True);  // Config système

  if EstExecuteEnTantQuAdmin then
  begin
    FTokenManager := TTokenManager.Create;
    MemoLog.Lines.Add('Application démarrée avec privilèges administrateur');
    MemoLog.Lines.Add('Token info : ' + FTokenManager.ObtenirInfosToken);
  end
  else
  begin
    MemoLog.Lines.Add('Application démarrée en mode utilisateur standard');
    FTokenManager := nil;
  end;

  // Afficher le statut UAC
  AfficherStatutUAC;

  // Configurer l'interface selon les privilèges
  if EstExecuteEnTantQuAdmin then
  begin
    Caption := Application.Title + ' [Administrateur]';
    BtnOperationAdmin.Enabled := True;

    // Pas besoin du bouclier, on est déjà admin
  end
  else
  begin
    Caption := Application.Title + ' [Standard]';
    BtnOperationAdmin.Enabled := True;

    // Ajouter le bouclier UAC au bouton
    AjouterBouclierUAC(BtnOperationAdmin);
  end;

  // Journaliser le démarrage
  FJournal.LogElevation(EstExecuteEnTantQuAdmin, 'Démarrage application');
end;

procedure TMainForm.AfficherStatutUAC;
var
  StatutText: string;
begin
  StatutText := 'UAC: ';

  if UACEstActive then
    StatutText := StatutText + 'Actif (Niveau ' + IntToStr(ObtenirNiveauUAC) + ')'
  else
    StatutText := StatutText + 'Désactivé';

  if EstExecuteEnTantQuAdmin then
    StatutText := StatutText + ' | Privilèges: Administrateur'
  else
    StatutText := StatutText + ' | Privilèges: Standard';

  StatusBar.SimpleText := StatutText;
end;

procedure TMainForm.BtnOperationNormaleClick(Sender: TObject);
begin
  EffectuerOperationNormale;
end;

procedure TMainForm.EffectuerOperationNormale;
var
  ConfigUser: TConfigurationManager;
  Valeur: string;
begin
  MemoLog.Lines.Add('--- Opération normale ---');

  try
    // Lire une configuration utilisateur
    MemoLog.Lines.Add('Lecture config utilisateur...');
    ConfigUser := TConfigurationManager.Create(False);
    try
      Valeur := ConfigUser.LireValeur('General', 'LastRun', 'Jamais');
      MemoLog.Lines.Add('Dernière exécution : ' + Valeur);

      // Écrire la date actuelle
      ConfigUser.EcrireValeur('General', 'LastRun', DateTimeToStr(Now));
      MemoLog.Lines.Add('Configuration mise à jour');
    finally
      ConfigUser.Free;
    end;

    FJournal.LogActionPrivilegiee('Opération normale', 'Succès');

  except
    on E: Exception do
    begin
      MemoLog.Lines.Add('Erreur : ' + E.Message);
      FJournal.LogErreurSecurite(E.Message);
    end;
  end;
end;

procedure TMainForm.BtnOperationAdminClick(Sender: TObject);
begin
  if not EstExecuteEnTantQuAdmin then
  begin
    if MessageDlg('Cette opération nécessite des privilèges administrateur. ' +
                  'Voulez-vous relancer l''application avec élévation ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if DemanderElevation then
      begin
        Application.Terminate;
        Exit;
      end;
    end;
  end
  else
    EffectuerOperationPrivilegiee;
end;

procedure TMainForm.EffectuerOperationPrivilegiee;
begin
  MemoLog.Lines.Add('--- Opération privilégiée ---');

  if not Assigned(FTokenManager) then
  begin
    MemoLog.Lines.Add('Erreur : Token manager non disponible');
    Exit;
  end;

  try
    // Activer des privilèges spéciaux
    if FTokenManager.ActiverPrivilegeBackup then
    begin
      MemoLog.Lines.Add('Privilège Backup activé');

      // Effectuer une opération nécessitant ce privilège
      // ...

      FTokenManager.DesactiverPrivilege('SeBackupPrivilege');
      MemoLog.Lines.Add('Privilège Backup désactivé');
    end;

    // Écrire dans la configuration système
    MemoLog.Lines.Add('Écriture config système...');
    FConfigManager.EcrireValeur('System', 'InstallDate', DateTimeToStr(Now));
    FConfigManager.EcrireRegistre('\SOFTWARE\MonApplication', 'Version', '1.0');

    MemoLog.Lines.Add('Opération privilégiée terminée avec succès');
    FJournal.LogActionPrivilegiee('Opération privilégiée', 'Succès');

  except
    on E: Exception do
    begin
      MemoLog.Lines.Add('Erreur : ' + E.Message);
      FJournal.LogErreurSecurite(E.Message);
    end;
  end;
end;

function TMainForm.DemanderElevation: Boolean;
var
  ExeName, Params: string;
begin
  ExeName := ParamStr(0);
  Params := '/elevated';

  FJournal.LogElevation(False, 'Demande élévation');

  Result := ShellExecute(0, 'runas', PChar(ExeName), PChar(Params),
                        nil, SW_SHOWNORMAL) > 32;

  if Result then
    FJournal.LogElevation(True, 'Élévation acceptée')
  else
    FJournal.LogElevation(False, 'Élévation refusée ou échouée');
end;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

## Dépannage et problèmes courants

### Problème : "L'opération demandée nécessite une élévation"

```pascal
procedure GererErreurElevation;
begin
  case GetLastError of
    ERROR_ELEVATION_REQUIRED:
      ShowMessage('Élévation requise. Relancez avec "Exécuter en tant qu''administrateur"');

    ERROR_CANCELLED:
      ShowMessage('L''utilisateur a annulé la demande d''élévation');

    ERROR_ACCESS_DENIED:
      ShowMessage('Accès refusé. Vérifiez vos privilèges');

    else
      ShowMessage('Erreur UAC : ' + SysErrorMessage(GetLastError));
  end;
end;
```

### Problème : Le manifeste n'est pas pris en compte

```pascal
procedure VerifierManifeste;
var
  ManifestPresent: Boolean;
  Handle: THandle;
begin
  // Vérifier si le manifeste est intégré
  Handle := FindResource(HInstance, MakeIntResource(1), RT_MANIFEST);
  ManifestPresent := Handle <> 0;

  if not ManifestPresent then
    ShowMessage('Attention : Aucun manifeste intégré détecté');
end;
```

### Problème : Détection incorrecte des privilèges

```pascal
function VerificationComplete: string;
var
  InfoAdmin, InfoEleve, InfoUAC: string;
begin
  if EstAdministrateur then
    InfoAdmin := 'Membre du groupe Administrateurs'
  else
    InfoAdmin := 'Utilisateur standard';

  if EstExecuteEnTantQuAdmin then
    InfoEleve := 'Processus élevé'
  else
    InfoEleve := 'Processus non élevé';

  if UACEstActive then
    InfoUAC := 'UAC actif'
  else
    InfoUAC := 'UAC inactif';

  Result := Format('%s | %s | %s', [InfoAdmin, InfoEleve, InfoUAC]);
end;
```

## Conclusion

L'UAC est un mécanisme de sécurité essentiel de Windows moderne. Pour créer des applications robustes et sécurisées avec FreePascal/Lazarus :

1. **Toujours utiliser un manifeste** pour déclarer les besoins de votre application
2. **Respecter le principe du moindre privilège** : ne demander l'élévation que si nécessaire
3. **Informer clairement l'utilisateur** quand des privilèges sont nécessaires
4. **Gérer les erreurs** liées à l'UAC de manière gracieuse
5. **Journaliser les actions privilégiées** pour la sécurité et le débogage
6. **Tester sur différentes configurations** UAC et versions de Windows
7. **Séparer les fonctionnalités** nécessitant l'élévation du reste de l'application
8. **Utiliser des mécanismes de communication** sécurisés entre processus élevés et non élevés

## Tableau récapitulatif des API UAC

### Fonctions principales

| Fonction | Description | Utilisation |
|----------|-------------|-------------|
| `ShellExecute` avec "runas" | Lance un programme avec élévation | Demande d'élévation simple |
| `ShellExecuteEx` | Version étendue avec plus d'options | Élévation avec attente du processus |
| `OpenProcessToken` | Ouvre le token d'un processus | Vérification des privilèges |
| `GetTokenInformation` | Obtient les infos du token | Vérifier si élevé |
| `AdjustTokenPrivileges` | Modifie les privilèges | Activer des privilèges spéciaux |
| `LookupPrivilegeValue` | Obtient le LUID d'un privilège | Préparation pour AdjustTokenPrivileges |

### Privilèges Windows importants

| Privilège | Nom système | Description |
|-----------|-------------|-------------|
| Debug | SeDebugPrivilege | Déboguer des processus |
| Backup | SeBackupPrivilege | Sauvegarder des fichiers système |
| Restore | SeRestorePrivilege | Restaurer des fichiers système |
| Shutdown | SeShutdownPrivilege | Arrêter le système |
| TakeOwnership | SeTakeOwnershipPrivilege | Prendre possession de fichiers |
| LoadDriver | SeLoadDriverPrivilege | Charger des drivers |
| SystemTime | SeSystemTimePrivilege | Modifier l'heure système |
| CreatePagefile | SeCreatePagefilePrivilege | Créer un fichier d'échange |

## Modèles de code réutilisables

### Module UAC complet

```pascal
unit UAC;

interface

uses
  Windows, SysUtils, ShellAPI, Registry, Classes;

type
  // Exception spécifique UAC
  EUACException = class(Exception);

  // Niveau d'exécution requis
  TExecutionLevel = (
    elAsInvoker,        // Privilèges actuels
    elHighestAvailable, // Plus haut disponible
    elRequireAdmin      // Admin obligatoire
  );

  // Gestionnaire UAC principal
  TUACManager = class
  private
    class var FInstance: TUACManager;
    FIsElevated: Boolean;
    FIsAdmin: Boolean;
    FUACEnabled: Boolean;
    FUACLevel: Integer;

    constructor CreateInstance;
    procedure DetectStatus;
  public
    class function Instance: TUACManager;
    class procedure FreeInstance;

    function RequestElevation(const Reason: string = ''): Boolean;
    function RunElevated(const Command, Params: string; Wait: Boolean = False): Boolean;
    function CanWriteToSystemFolders: Boolean;
    function CanModifyRegistry(RootKey: HKEY): Boolean;

    property IsElevated: Boolean read FIsElevated;
    property IsAdmin: Boolean read FIsAdmin;
    property UACEnabled: Boolean read FUACEnabled;
    property UACLevel: Integer read FUACLevel;
  end;

  // Helper pour les opérations privilégiées
  TPrivilegedOperation = class
  private
    FOperation: TProc;
    FDescription: string;
    FRequiresElevation: Boolean;
  public
    constructor Create(const Description: string; Operation: TProc;
                      RequiresElevation: Boolean = True);
    function Execute: Boolean;
  end;

  // Surveillance UAC
  TUACMonitor = class(TThread)
  private
    FOnStatusChanged: TNotifyEvent;
    FLastStatus: Boolean;
  protected
    procedure Execute; override;
  public
    property OnStatusChanged: TNotifyEvent read FOnStatusChanged write FOnStatusChanged;
  end;

implementation

{ TUACManager }

constructor TUACManager.CreateInstance;
begin
  inherited Create;
  DetectStatus;
end;

class function TUACManager.Instance: TUACManager;
begin
  if not Assigned(FInstance) then
    FInstance := TUACManager.CreateInstance;
  Result := FInstance;
end;

class procedure TUACManager.FreeInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TUACManager.DetectStatus;
var
  TokenHandle: THandle;
  Elevation: TOKEN_ELEVATION;
  ReturnLength: DWORD;
  Reg: TRegistry;
begin
  // Détection de l'élévation
  FIsElevated := False;
  if Win32MajorVersion >= 6 then
  begin
    if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
    try
      if GetTokenInformation(TokenHandle, TokenElevation, @Elevation,
                            SizeOf(Elevation), ReturnLength) then
        FIsElevated := Elevation.TokenIsElevated <> 0;
    finally
      CloseHandle(TokenHandle);
    end;
  end;

  // Détection admin
  FIsAdmin := EstAdministrateur;  // Utiliser la fonction définie précédemment

  // Détection UAC
  FUACEnabled := False;
  FUACLevel := 0;

  if Win32MajorVersion >= 6 then
  begin
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System') then
      begin
        try
          if Reg.ValueExists('EnableLUA') then
            FUACEnabled := Reg.ReadInteger('EnableLUA') <> 0;

          if Reg.ValueExists('ConsentPromptBehaviorAdmin') then
            FUACLevel := Reg.ReadInteger('ConsentPromptBehaviorAdmin');
        finally
          Reg.CloseKey;
        end;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

function TUACManager.RequestElevation(const Reason: string): Boolean;
var
  Msg: string;
begin
  Result := False;

  if FIsElevated then
  begin
    Result := True;
    Exit;
  end;

  if Reason <> '' then
    Msg := Reason + #13#10#13#10 + 'Voulez-vous relancer avec des privilèges administrateur ?'
  else
    Msg := 'Cette opération nécessite des privilèges administrateur. Continuer ?';

  if MessageBox(0, PChar(Msg), 'Élévation requise', MB_YESNO or MB_ICONQUESTION) = IDYES then
  begin
    Result := RunElevated(ParamStr(0), GetCommandLine, False);
    if Result then
      Halt(0);  // Fermer l'instance actuelle
  end;
end;

function TUACManager.RunElevated(const Command, Params: string; Wait: Boolean): Boolean;
var
  SEI: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  FillChar(SEI, SizeOf(SEI), 0);
  SEI.cbSize := SizeOf(TShellExecuteInfo);
  SEI.Wnd := 0;
  SEI.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;

  if Wait then
    SEI.fMask := SEI.fMask or SEE_MASK_NOCLOSEPROCESS;

  SEI.lpVerb := 'runas';
  SEI.lpFile := PChar(Command);
  SEI.lpParameters := PChar(Params);
  SEI.lpDirectory := nil;
  SEI.nShow := SW_SHOWNORMAL;

  Result := ShellExecuteEx(@SEI);

  if Result and Wait and (SEI.hProcess <> 0) then
  begin
    WaitForSingleObject(SEI.hProcess, INFINITE);
    GetExitCodeProcess(SEI.hProcess, ExitCode);
    CloseHandle(SEI.hProcess);
    Result := ExitCode = 0;
  end;
end;

function TUACManager.CanWriteToSystemFolders: Boolean;
begin
  Result := FIsElevated or not FUACEnabled;
end;

function TUACManager.CanModifyRegistry(RootKey: HKEY): Boolean;
begin
  if RootKey = HKEY_CURRENT_USER then
    Result := True
  else
    Result := FIsElevated or not FUACEnabled;
end;

{ TPrivilegedOperation }

constructor TPrivilegedOperation.Create(const Description: string;
  Operation: TProc; RequiresElevation: Boolean);
begin
  FDescription := Description;
  FOperation := Operation;
  FRequiresElevation := RequiresElevation;
end;

function TPrivilegedOperation.Execute: Boolean;
begin
  Result := False;

  if FRequiresElevation and not TUACManager.Instance.IsElevated then
  begin
    if not TUACManager.Instance.RequestElevation(FDescription) then
      Exit;
  end;

  try
    FOperation();
    Result := True;
  except
    on E: Exception do
      raise EUACException.CreateFmt('Échec de l''opération privilégiée : %s', [E.Message]);
  end;
end;

{ TUACMonitor }

procedure TUACMonitor.Execute;
begin
  while not Terminated do
  begin
    Sleep(1000);  // Vérifier toutes les secondes

    if TUACManager.Instance.IsElevated <> FLastStatus then
    begin
      FLastStatus := TUACManager.Instance.IsElevated;
      if Assigned(FOnStatusChanged) then
        Synchronize(procedure
        begin
          FOnStatusChanged(Self);
        end);
    end;
  end;
end;

initialization

finalization
  TUACManager.FreeInstance;

end.
```

### Utilisation du module UAC

```pascal
program ExempleUtilisationUAC;

uses
  UAC, SysUtils;

procedure OperationSimple;
begin
  WriteLn('Opération simple exécutée');
end;

procedure OperationPrivilegiee;
begin
  WriteLn('Modification du système...');
  // Code nécessitant l'élévation
end;

var
  Op: TPrivilegedOperation;
  UAC: TUACManager;
begin
  UAC := TUACManager.Instance;

  // Afficher le statut
  WriteLn('État UAC :');
  WriteLn('  - UAC activé : ', UAC.UACEnabled);
  WriteLn('  - Niveau UAC : ', UAC.UACLevel);
  WriteLn('  - Administrateur : ', UAC.IsAdmin);
  WriteLn('  - Élevé : ', UAC.IsElevated);
  WriteLn;

  // Exécuter une opération privilégiée
  Op := TPrivilegedOperation.Create(
    'Modification des paramètres système',
    OperationPrivilegiee,
    True
  );
  try
    if Op.Execute then
      WriteLn('Opération réussie')
    else
      WriteLn('Opération annulée ou échouée');
  finally
    Op.Free;
  end;

  ReadLn;
end.
```

## Checklist de sécurité UAC

### Avant le développement

- [ ] Déterminer quelles fonctionnalités nécessitent vraiment l'élévation
- [ ] Concevoir l'application pour minimiser les besoins d'élévation
- [ ] Prévoir une séparation entre fonctions admin et utilisateur
- [ ] Planifier la stratégie de manifeste (asInvoker, highestAvailable, requireAdministrator)

### Pendant le développement

- [ ] Implémenter la détection correcte des privilèges
- [ ] Ajouter les indicateurs visuels (icônes bouclier)
- [ ] Gérer tous les cas d'erreur UAC
- [ ] Implémenter la journalisation des actions privilégiées
- [ ] Valider toutes les entrées avant les opérations privilégiées
- [ ] Utiliser des mécanismes de communication sécurisés

### Tests

- [ ] Tester avec UAC désactivé
- [ ] Tester avec UAC au niveau maximum
- [ ] Tester en tant qu'utilisateur standard
- [ ] Tester en tant qu'administrateur (non élevé)
- [ ] Tester en tant qu'administrateur (élevé)
- [ ] Tester sur Windows 7, 8, 10 et 11
- [ ] Vérifier les messages d'erreur et leur clarté
- [ ] Tester l'annulation de l'élévation par l'utilisateur

### Déploiement

- [ ] Manifeste correctement intégré à l'exécutable
- [ ] Documentation des besoins en privilèges
- [ ] Instructions claires pour les utilisateurs
- [ ] Support technique préparé pour les problèmes UAC

## Ressources et documentation

### Documentation Microsoft officielle

- [User Account Control](https://docs.microsoft.com/windows/security/identity-protection/user-account-control/user-account-control-overview)
- [Application Manifests](https://docs.microsoft.com/windows/win32/sbscs/application-manifests)
- [Windows Security API](https://docs.microsoft.com/windows/win32/secauthz/authorization)

### Outils de développement

**Outils de diagnostic UAC :**
- **Process Explorer** : Voir les privilèges des processus
- **AccessChk** : Vérifier les permissions sur les fichiers/registre
- **Process Monitor** : Surveiller les accès refusés

**Éditeurs de manifeste :**
- **Manifest Tool (mt.exe)** : Outil Microsoft pour les manifestes
- **Resource Hacker** : Éditer les ressources des exécutables

### Exemples de manifestes pour différents cas

**Application standard (la plus courante) :**
```xml
<requestedExecutionLevel level="asInvoker" uiAccess="false"/>
```

**Utilitaire système :**
```xml
<requestedExecutionLevel level="highestAvailable" uiAccess="false"/>
```

**Installateur ou outil d'administration :**
```xml
<requestedExecutionLevel level="requireAdministrator" uiAccess="false"/>
```

**Application d'accessibilité (rare) :**
```xml
<requestedExecutionLevel level="asInvoker" uiAccess="true"/>
```

### Codes d'erreur UAC courants

| Code | Constante | Description |
|------|-----------|-------------|
| 740 | ERROR_ELEVATION_REQUIRED | L'opération requiert l'élévation |
| 1223 | ERROR_CANCELLED | L'utilisateur a annulé l'élévation |
| 5 | ERROR_ACCESS_DENIED | Accès refusé (peut être lié à UAC) |
| 1314 | ERROR_PRIVILEGE_NOT_HELD | Un privilège requis n'est pas détenu |

## Évolutions et alternatives

### Windows 10/11 et au-delà

Les versions récentes de Windows introduisent de nouvelles fonctionnalités :

1. **Windows Hello for Business** : Authentification biométrique
2. **Conditional Access** : Accès basé sur des conditions
3. **Windows Defender Application Control** : Contrôle plus granulaire

### Alternatives à l'élévation complète

1. **Services Windows** : Pour les tâches de fond privilégiées
2. **Tâches planifiées** : Exécution avec privilèges sans interaction
3. **COM Elevation Moniker** : Élévation de composants COM spécifiques
4. **Impersonation** : Emprunter temporairement une identité

```pascal
// Exemple de tâche planifiée pour éviter l'UAC
procedure CreerTachePlanifiee(const NomTache, Programme: string);
var
  TaskService: Variant;
  RootFolder: Variant;
  TaskDefinition: Variant;
  Trigger: Variant;
  Action: Variant;
begin
  TaskService := CreateOleObject('Schedule.Service');
  TaskService.Connect;

  RootFolder := TaskService.GetFolder('\');
  TaskDefinition := TaskService.NewTask(0);

  // Configurer pour s'exécuter avec les privilèges les plus élevés
  TaskDefinition.Principal.RunLevel := 1;  // TASK_RUNLEVEL_HIGHEST

  // Ajouter un déclencheur
  Trigger := TaskDefinition.Triggers.Create(1);  // TASK_TRIGGER_REGISTRATION

  // Ajouter l'action
  Action := TaskDefinition.Actions.Create(0);  // TASK_ACTION_EXEC
  Action.Path := Programme;

  // Enregistrer la tâche
  RootFolder.RegisterTaskDefinition(
    NomTache,
    TaskDefinition,
    6,  // TASK_CREATE_OR_UPDATE
    '',  // Utilisateur
    '',  // Mot de passe
    3,  // TASK_LOGON_INTERACTIVE_TOKEN
    ''
  );
end;
```

## Résumé final

La gestion de l'UAC est un aspect crucial du développement d'applications Windows modernes. Avec FreePascal/Lazarus, vous disposez de tous les outils nécessaires pour :

1. **Détecter** l'état des privilèges et de l'UAC
2. **Demander** l'élévation quand nécessaire
3. **Gérer** les opérations privilégiées de manière sécurisée
4. **Communiquer** clairement avec l'utilisateur
5. **Respecter** les bonnes pratiques de sécurité Windows

L'objectif est toujours de créer des applications qui :
- Fonctionnent correctement dans tous les environnements UAC
- Respectent le principe du moindre privilège
- Offrent une expérience utilisateur claire et cohérente
- Maintiennent la sécurité du système

En suivant les principes et exemples de ce tutoriel, vous pourrez créer des applications Windows professionnelles qui s'intègrent parfaitement avec les mécanismes de sécurité modernes de Windows tout en restant accessibles et faciles à utiliser.

⏭️ [Signature Authenticode](/06-specificites-windows/07-signature-authenticode.md)
