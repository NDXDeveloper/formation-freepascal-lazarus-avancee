🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Intégration native par plateforme dans FreePascal/Lazarus

## Introduction

L'intégration native signifie que votre application FreePascal/Lazarus s'intègre parfaitement avec le système d'exploitation, en utilisant ses fonctionnalités spécifiques pour offrir une expérience utilisateur naturelle. Plutôt que d'avoir une application qui semble "étrangère", vous créez une application qui se comporte exactement comme les autres applications natives de chaque système.

Ce tutoriel vous guidera pour intégrer votre application de manière native sur Windows et Linux/Ubuntu, en exploitant les capacités propres à chaque plateforme.

## Comprendre les différences d'intégration

### Ce qui rend une application "native"

**Sur Windows :**
- Icônes dans la zone de notification (system tray)
- Intégration avec le menu Démarrer et la barre des tâches
- Support des Jump Lists (listes de raccourcis)
- Notifications Windows modernes
- Association de fichiers dans l'Explorateur
- Intégration avec le registre Windows

**Sur Linux/Ubuntu :**
- Indicateurs d'application (AppIndicators)
- Intégration avec les bureaux GNOME/KDE/XFCE
- Notifications via libnotify
- Fichiers .desktop pour les lanceurs
- Intégration avec les menus systèmes
- Respect des thèmes système

## Zone de notification / System Tray

### Implémentation cross-platform de l'icône système

La zone de notification (Windows) ou la zone d'indicateurs (Linux) permet à votre application de rester accessible même lorsque sa fenêtre principale est fermée.

```pascal
unit SystemTrayIntegration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Menus, ExtCtrls, Graphics
  {$IFDEF WINDOWS}
  , Windows, ShellAPI
  {$ENDIF}
  {$IFDEF UNIX}
  , UnixType
  {$ENDIF};

type
  TSystemTrayManager = class
  private
    FTrayIcon: TTrayIcon;
    FPopupMenu: TPopupMenu;
    FMainForm: TForm;

    procedure CreateTrayMenu;
    procedure OnTrayClick(Sender: TObject);
    procedure OnTrayDblClick(Sender: TObject);
    procedure OnMenuShow(Sender: TObject);
    procedure OnMenuQuit(Sender: TObject);
    procedure SetupPlatformSpecific;
  public
    constructor Create(AMainForm: TForm);
    destructor Destroy; override;

    procedure ShowBalloonHint(const ATitle, AMessage: string;
                             AIconType: TBalloonFlags = bfInfo);
    procedure UpdateIcon(AIcon: TIcon);
    procedure SetVisible(AVisible: Boolean);
  end;

implementation

constructor TSystemTrayManager.Create(AMainForm: TForm);  
begin
  inherited Create;
  FMainForm := AMainForm;

  // Créer l'icône de la zone de notification
  FTrayIcon := TTrayIcon.Create(FMainForm);
  FTrayIcon.Visible := True;

  // Configurer les événements
  FTrayIcon.OnClick := @OnTrayClick;
  FTrayIcon.OnDblClick := @OnTrayDblClick;

  // Créer le menu contextuel
  CreateTrayMenu;
  FTrayIcon.PopUpMenu := FPopupMenu;

  // Configuration spécifique à la plateforme
  SetupPlatformSpecific;

  // Définir l'icône et le texte
  FTrayIcon.Icon := Application.Icon;
  FTrayIcon.Hint := Application.Title;
end;

destructor TSystemTrayManager.Destroy;  
begin
  FTrayIcon.Free;
  FPopupMenu.Free;
  inherited;
end;

procedure TSystemTrayManager.CreateTrayMenu;  
var
  MenuItem: TMenuItem;
begin
  FPopupMenu := TPopupMenu.Create(FMainForm);

  // Afficher/Masquer
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Afficher';
  MenuItem.OnClick := @OnMenuShow;
  MenuItem.Default := True; // Double-clic ouvrira l'application
  FPopupMenu.Items.Add(MenuItem);

  // Séparateur
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := '-';
  FPopupMenu.Items.Add(MenuItem);

  // Options spécifiques à l'OS
  {$IFDEF WINDOWS}
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Démarrage automatique';
  MenuItem.OnClick := @OnToggleAutoStart;
  FPopupMenu.Items.Add(MenuItem);
  {$ENDIF}

  {$IFDEF UNIX}
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Préférences';
  MenuItem.OnClick := @OnPreferences;
  FPopupMenu.Items.Add(MenuItem);
  {$ENDIF}

  // Séparateur
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := '-';
  FPopupMenu.Items.Add(MenuItem);

  // Quitter
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := 'Quitter';
  MenuItem.OnClick := @OnMenuQuit;
  FPopupMenu.Items.Add(MenuItem);
end;

procedure TSystemTrayManager.SetupPlatformSpecific;  
begin
  {$IFDEF WINDOWS}
  // Configuration spécifique Windows
  FTrayIcon.BalloonTitle := Application.Title;
  FTrayIcon.BalloonFlags := bfInfo;

  // Windows 10+ : utiliser les icônes modernes
  if (Win32MajorVersion >= 10) then
  begin
    // Adapter l'icône au thème (clair/sombre)
    // Code pour détecter le thème et adapter l'icône
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Configuration spécifique Linux
  // Les AppIndicators modernes nécessitent un menu
  if not Assigned(FTrayIcon.PopUpMenu) then
    FTrayIcon.PopUpMenu := FPopupMenu;

  // Définir la catégorie pour Unity/GNOME
  // Ceci aide le système à placer correctement l'icône
  {$ENDIF}
end;

procedure TSystemTrayManager.ShowBalloonHint(const ATitle, AMessage: string;
                                            AIconType: TBalloonFlags);
begin
  {$IFDEF WINDOWS}
  FTrayIcon.BalloonTitle := ATitle;
  FTrayIcon.BalloonHint := AMessage;
  FTrayIcon.BalloonFlags := AIconType;
  FTrayIcon.ShowBalloonHint;
  {$ENDIF}

  {$IFDEF UNIX}
  // Sur Linux, utiliser libnotify via un processus externe
  // ou une bibliothèque de notifications
  RunCommand('notify-send', [ATitle, AMessage], '');
  {$ENDIF}
end;

procedure TSystemTrayManager.OnTrayClick(Sender: TObject);  
begin
  // Simple clic : ne rien faire ou afficher un menu
  // Le comportement dépend de l'OS
  {$IFDEF UNIX}
  // Sur Linux, le clic gauche affiche généralement le menu
  if Assigned(FPopupMenu) then
    FPopupMenu.PopUp;
  {$ENDIF}
end;

procedure TSystemTrayManager.OnTrayDblClick(Sender: TObject);  
begin
  // Double-clic : restaurer la fenêtre principale
  OnMenuShow(Sender);
end;

procedure TSystemTrayManager.OnMenuShow(Sender: TObject);  
begin
  FMainForm.Show;
  FMainForm.WindowState := wsNormal;
  Application.BringToFront;
end;

procedure TSystemTrayManager.OnMenuQuit(Sender: TObject);  
begin
  Application.Terminate;
end;

procedure TSystemTrayManager.UpdateIcon(AIcon: TIcon);  
begin
  FTrayIcon.Icon := AIcon;
end;

procedure TSystemTrayManager.SetVisible(AVisible: Boolean);  
begin
  FTrayIcon.Visible := AVisible;
end;

end.
```

## Notifications natives

### Système de notifications cross-platform

Les notifications permettent d'informer l'utilisateur même quand l'application est en arrière-plan.

```pascal
unit NativeNotifications;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}
  , Windows, Registry
  {$ENDIF}
  {$IFDEF UNIX}
  , Process
  {$ENDIF};

type
  TNotificationType = (ntInfo, ntWarning, ntError, ntSuccess);

  TNativeNotification = class
  private
    FTitle: string;
    FMessage: string;
    FIconPath: string;
    FTimeout: Integer; // en millisecondes

    {$IFDEF WINDOWS}
    procedure ShowWindowsNotification;
    procedure ShowWindows10Toast;
    function IsWindows10OrNewer: Boolean;
    {$ENDIF}

    {$IFDEF UNIX}
    procedure ShowLinuxNotification;
    function GetNotificationIcon(AType: TNotificationType): string;
    {$ENDIF}
  public
    constructor Create;

    procedure Show(const ATitle, AMessage: string;
                  AType: TNotificationType = ntInfo);
    procedure ShowWithAction(const ATitle, AMessage, AActionLabel: string;
                           ACallback: TNotifyEvent);

    property Timeout: Integer read FTimeout write FTimeout;
  end;

implementation

constructor TNativeNotification.Create;  
begin
  inherited Create;
  FTimeout := 5000; // 5 secondes par défaut
end;

procedure TNativeNotification.Show(const ATitle, AMessage: string;
                                  AType: TNotificationType);
begin
  FTitle := ATitle;
  FMessage := AMessage;

  {$IFDEF WINDOWS}
  if IsWindows10OrNewer then
    ShowWindows10Toast
  else
    ShowWindowsNotification;
  {$ENDIF}

  {$IFDEF UNIX}
  ShowLinuxNotification;
  {$ENDIF}
end;

{$IFDEF WINDOWS}
procedure TNativeNotification.ShowWindowsNotification;  
var
  NotifyIconData: TNotifyIconData;
begin
  // Utiliser l'API Windows classique pour les notifications
  FillChar(NotifyIconData, SizeOf(NotifyIconData), 0);
  NotifyIconData.cbSize := SizeOf(NotifyIconData);
  NotifyIconData.hWnd := Application.MainForm.Handle;
  NotifyIconData.uFlags := NIF_INFO;

  StrPCopy(NotifyIconData.szInfoTitle, FTitle);
  StrPCopy(NotifyIconData.szInfo, FMessage);
  NotifyIconData.uTimeout := FTimeout;

  Shell_NotifyIcon(NIM_MODIFY, @NotifyIconData);
end;

procedure TNativeNotification.ShowWindows10Toast;  
begin
  // Windows 10+ : utiliser les notifications toast modernes
  // Ceci nécessite l'enregistrement de l'application dans le registre
  // et l'utilisation de COM pour WinRT

  // Pour simplifier, on peut utiliser PowerShell
  RunCommand('powershell',
    ['-Command',
     Format('[Windows.UI.Notifications.ToastNotificationManager, ' +
            'Windows.UI.Notifications, ContentType=WindowsRuntime] | Out-Null; ' +
            '$template = [Windows.UI.Notifications.ToastNotificationManager]::' +
            'GetTemplateContent([Windows.UI.Notifications.ToastTemplateType]::ToastText02); ' +
            '$textNodes = $template.GetElementsByTagName("text"); ' +
            '$textNodes.Item(0).InnerText = "%s"; ' +
            '$textNodes.Item(1).InnerText = "%s"; ' +
            '$toast = [Windows.UI.Notifications.ToastNotification]::new($template); ' +
            '[Windows.UI.Notifications.ToastNotificationManager]::' +
            'CreateToastNotifier("%s").Show($toast)',
            [FTitle, FMessage, Application.Title])],
    '');
end;

function TNativeNotification.IsWindows10OrNewer: Boolean;  
var
  Reg: TRegistry;
  Version: string;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows NT\CurrentVersion') then
    begin
      Version := Reg.ReadString('CurrentVersion');
      Result := (StrToFloatDef(Version, 0) >= 10.0);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;
{$ENDIF}

{$IFDEF UNIX}
procedure TNativeNotification.ShowLinuxNotification;  
var
  Process: TProcess;
  Icon: string;
begin
  Icon := GetNotificationIcon(ntInfo);

  Process := TProcess.Create(nil);
  try
    Process.Executable := 'notify-send';
    Process.Parameters.Add('--expire-time=' + IntToStr(FTimeout));

    if FileExists(Icon) then
      Process.Parameters.Add('--icon=' + Icon);

    Process.Parameters.Add(FTitle);
    Process.Parameters.Add(FMessage);

    Process.Options := [poNoConsole];
    Process.Execute;
  finally
    Process.Free;
  end;
end;

function TNativeNotification.GetNotificationIcon(AType: TNotificationType): string;  
begin
  // Utiliser les icônes standard du système
  case AType of
    ntInfo: Result := 'dialog-information';
    ntWarning: Result := 'dialog-warning';
    ntError: Result := 'dialog-error';
    ntSuccess: Result := 'emblem-default';
  else
    Result := 'dialog-information';
  end;
end;
{$ENDIF}

procedure TNativeNotification.ShowWithAction(const ATitle, AMessage,
                                            AActionLabel: string;
                                            ACallback: TNotifyEvent);
begin
  // Notifications avec actions (plus complexe)
  // Nécessite une implémentation plus avancée selon l'OS
  Show(ATitle, AMessage + ' (' + AActionLabel + ')');
end;

end.
```

## Association de fichiers

### Enregistrement des types de fichiers

L'association de fichiers permet à votre application d'être lancée automatiquement quand l'utilisateur double-clique sur certains types de fichiers.

```pascal
unit FileAssociation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}
  , Registry, Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , Process, FileUtil
  {$ENDIF};

type
  TFileAssociationManager = class
  private
    FApplicationPath: string;
    FApplicationName: string;

    {$IFDEF WINDOWS}
    procedure RegisterWindowsAssociation(const AExtension, ADescription: string;
                                        const AIconPath: string);
    procedure UnregisterWindowsAssociation(const AExtension: string);
    {$ENDIF}

    {$IFDEF UNIX}
    procedure CreateDesktopFile;
    procedure RegisterMimeType(const AExtension, AMimeType, ADescription: string);
    procedure UpdateMimeDatabase;
    {$ENDIF}
  public
    constructor Create;

    procedure RegisterExtension(const AExtension, ADescription: string;
                               const AIconPath: string = '');
    procedure UnregisterExtension(const AExtension: string);
    function IsRegistered(const AExtension: string): Boolean;
    procedure HandleOpenedFile(const AFileName: string);
  end;

implementation

constructor TFileAssociationManager.Create;  
begin
  inherited Create;
  FApplicationPath := ParamStr(0);
  FApplicationName := ExtractFileName(FApplicationPath);
  FApplicationName := ChangeFileExt(FApplicationName, '');
end;

procedure TFileAssociationManager.RegisterExtension(const AExtension,
                                                   ADescription: string;
                                                   const AIconPath: string);
begin
  {$IFDEF WINDOWS}
  RegisterWindowsAssociation(AExtension, ADescription, AIconPath);
  {$ENDIF}

  {$IFDEF UNIX}
  CreateDesktopFile;
  RegisterMimeType(AExtension, 'application/x-' + FApplicationName,
                   ADescription);
  {$ENDIF}
end;

{$IFDEF WINDOWS}
procedure TFileAssociationManager.RegisterWindowsAssociation(
  const AExtension, ADescription: string; const AIconPath: string);
var
  Reg: TRegistry;
  ExtKey, AppKey: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    // Créer la clé pour l'extension
    ExtKey := '\Software\Classes\' + AExtension;
    if Reg.OpenKey(ExtKey, True) then
    begin
      Reg.WriteString('', FApplicationName + '.Document');
      Reg.CloseKey;
    end;

    // Créer la clé pour le type de document
    AppKey := '\Software\Classes\' + FApplicationName + '.Document';
    if Reg.OpenKey(AppKey, True) then
    begin
      Reg.WriteString('', ADescription);
      Reg.CloseKey;
    end;

    // Définir l'icône
    if AIconPath <> '' then
    begin
      if Reg.OpenKey(AppKey + '\DefaultIcon', True) then
      begin
        Reg.WriteString('', AIconPath);
        Reg.CloseKey;
      end;
    end
    else
    begin
      // Utiliser l'icône de l'application
      if Reg.OpenKey(AppKey + '\DefaultIcon', True) then
      begin
        Reg.WriteString('', FApplicationPath + ',0');
        Reg.CloseKey;
      end;
    end;

    // Définir la commande d'ouverture
    if Reg.OpenKey(AppKey + '\shell\open\command', True) then
    begin
      Reg.WriteString('', '"' + FApplicationPath + '" "%1"');
      Reg.CloseKey;
    end;

    // Notifier Windows du changement
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

  finally
    Reg.Free;
  end;
end;

procedure TFileAssociationManager.UnregisterWindowsAssociation(
  const AExtension: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    // Supprimer l'association
    Reg.DeleteKey('\Software\Classes\' + AExtension);
    Reg.DeleteKey('\Software\Classes\' + FApplicationName + '.Document');

    // Notifier Windows
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

  finally
    Reg.Free;
  end;
end;
{$ENDIF}

{$IFDEF UNIX}
procedure TFileAssociationManager.CreateDesktopFile;  
var
  DesktopFile: TStringList;
  DesktopPath: string;
begin
  // Créer un fichier .desktop pour l'application
  DesktopPath := GetEnvironmentVariable('HOME') +
                 '/.local/share/applications/' +
                 FApplicationName + '.desktop';

  DesktopFile := TStringList.Create;
  try
    DesktopFile.Add('[Desktop Entry]');
    DesktopFile.Add('Version=1.0');
    DesktopFile.Add('Type=Application');
    DesktopFile.Add('Name=' + FApplicationName);
    DesktopFile.Add('Comment=' + Application.Title);
    DesktopFile.Add('Exec=' + FApplicationPath + ' %f');
    DesktopFile.Add('Icon=' + ChangeFileExt(FApplicationPath, ''));
    DesktopFile.Add('Terminal=false');
    DesktopFile.Add('Categories=Application;');
    DesktopFile.Add('MimeType=application/x-' + FApplicationName + ';');

    ForceDirectories(ExtractFilePath(DesktopPath));
    DesktopFile.SaveToFile(DesktopPath);

    // Rendre le fichier exécutable
    fpChmod(DesktopPath, &755);

  finally
    DesktopFile.Free;
  end;
end;

procedure TFileAssociationManager.RegisterMimeType(const AExtension,
                                                  AMimeType,
                                                  ADescription: string);
var
  MimeFile: TStringList;
  MimePath: string;
begin
  // Créer un fichier XML de définition MIME
  MimePath := GetEnvironmentVariable('HOME') +
              '/.local/share/mime/packages/' +
              FApplicationName + '.xml';

  MimeFile := TStringList.Create;
  try
    MimeFile.Add('<?xml version="1.0" encoding="UTF-8"?>');
    MimeFile.Add('<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">');
    MimeFile.Add('  <mime-type type="' + AMimeType + '">');
    MimeFile.Add('    <comment>' + ADescription + '</comment>');
    MimeFile.Add('    <glob pattern="*' + AExtension + '"/>');
    MimeFile.Add('  </mime-type>');
    MimeFile.Add('</mime-info>');

    ForceDirectories(ExtractFilePath(MimePath));
    MimeFile.SaveToFile(MimePath);

    // Mettre à jour la base de données MIME
    UpdateMimeDatabase;

  finally
    MimeFile.Free;
  end;
end;

procedure TFileAssociationManager.UpdateMimeDatabase;  
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'update-mime-database';
    Process.Parameters.Add(GetEnvironmentVariable('HOME') +
                          '/.local/share/mime');
    Process.Options := [poWaitOnExit, poNoConsole];
    Process.Execute;
  finally
    Process.Free;
  end;

  // Mettre à jour aussi la base de données des applications
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'update-desktop-database';
    Process.Parameters.Add(GetEnvironmentVariable('HOME') +
                          '/.local/share/applications');
    Process.Options := [poWaitOnExit, poNoConsole];
    Process.Execute;
  finally
    Process.Free;
  end;
end;
{$ENDIF}

function TFileAssociationManager.IsRegistered(const AExtension: string): Boolean;  
begin
  {$IFDEF WINDOWS}
  var
    Reg: TRegistry;
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Result := Reg.KeyExists('\Software\Classes\' + AExtension);
    finally
      Reg.Free;
    end;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Vérifier si le type MIME est enregistré
  Result := FileExists(GetEnvironmentVariable('HOME') +
                      '/.local/share/mime/packages/' +
                      FApplicationName + '.xml');
  {$ENDIF}
end;

procedure TFileAssociationManager.HandleOpenedFile(const AFileName: string);  
begin
  // Traiter le fichier ouvert
  if FileExists(AFileName) then
  begin
    // Charger le fichier dans l'application
    // Cette méthode sera appelée quand l'application est lancée
    // avec un fichier en paramètre
    ShowMessage('Ouverture du fichier : ' + AFileName);
  end;
end;

procedure TFileAssociationManager.UnregisterExtension(const AExtension: string);  
begin
  {$IFDEF WINDOWS}
  UnregisterWindowsAssociation(AExtension);
  {$ENDIF}

  {$IFDEF UNIX}
  // Supprimer les fichiers de configuration
  DeleteFile(GetEnvironmentVariable('HOME') +
            '/.local/share/mime/packages/' +
            FApplicationName + '.xml');
  UpdateMimeDatabase;
  {$ENDIF}
end;

end.
```

## Intégration au menu système

### Ajout au menu démarrer (Windows) et aux lanceurs (Linux)

```pascal
unit SystemMenuIntegration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil
  {$IFDEF WINDOWS}
  , Windows, ShlObj, ActiveX, ComObj
  {$ENDIF}
  {$IFDEF UNIX}
  , Process
  {$ENDIF};

type
  TSystemMenuManager = class
  private
    FApplicationName: string;
    FApplicationPath: string;
    FIconPath: string;

    {$IFDEF WINDOWS}
    procedure CreateWindowsShortcut(const ATargetPath, AShortcutPath: string;
                                   const ADescription: string = '');
    procedure AddToStartMenu;
    procedure AddToDesktop;
    procedure PinToTaskbar;
    {$ENDIF}

    {$IFDEF UNIX}
    procedure CreateLinuxLauncher;
    procedure AddToApplicationMenu;
    procedure AddToDesktop;
    procedure AddToDock;
    {$ENDIF}
  public
    constructor Create;

    procedure InstallShortcuts;
    procedure RemoveShortcuts;
    procedure CreateQuickLaunchEntry;
  end;

implementation

constructor TSystemMenuManager.Create;  
begin
  inherited Create;
  FApplicationPath := ParamStr(0);
  FApplicationName := ChangeFileExt(ExtractFileName(FApplicationPath), '');
  FIconPath := ChangeFileExt(FApplicationPath, '.ico');

  {$IFDEF UNIX}
  // Sur Linux, préférer les icônes PNG
  FIconPath := ChangeFileExt(FApplicationPath, '.png');
  {$ENDIF}
end;

procedure TSystemMenuManager.InstallShortcuts;  
begin
  {$IFDEF WINDOWS}
  AddToStartMenu;
  AddToDesktop;
  {$ENDIF}

  {$IFDEF UNIX}
  CreateLinuxLauncher;
  AddToApplicationMenu;
  {$ENDIF}
end;

{$IFDEF WINDOWS}
procedure TSystemMenuManager.CreateWindowsShortcut(const ATargetPath,
                                                  AShortcutPath: string;
                                                  const ADescription: string);
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  WideShortcutPath: WideString;
begin
  // Initialiser COM
  CoInitialize(nil);
  try
    // Créer l'objet ShellLink
    ShellLink := CreateComObject(CLSID_ShellLink) as IShellLink;
    PersistFile := ShellLink as IPersistFile;

    // Configurer le raccourci
    ShellLink.SetPath(PChar(ATargetPath));
    ShellLink.SetWorkingDirectory(PChar(ExtractFilePath(ATargetPath)));

    if ADescription <> '' then
      ShellLink.SetDescription(PChar(ADescription));

    if FileExists(FIconPath) then
      ShellLink.SetIconLocation(PChar(FIconPath), 0)
    else
      ShellLink.SetIconLocation(PChar(ATargetPath), 0);

    // Sauvegarder le raccourci
    WideShortcutPath := AShortcutPath;
    PersistFile.Save(PWideChar(WideShortcutPath), True);

  finally
    CoUninitialize;
  end;
end;

procedure TSystemMenuManager.AddToStartMenu;  
var
  StartMenuPath: array[0..MAX_PATH] of Char;
  ShortcutPath: string;
begin
  // Obtenir le chemin du menu Démarrer
  if SHGetFolderPath(0, CSIDL_PROGRAMS, 0, 0, StartMenuPath) = S_OK then
  begin
    ShortcutPath := IncludeTrailingPathDelimiter(StartMenuPath) +
                    FApplicationName + '.lnk';

    CreateWindowsShortcut(FApplicationPath, ShortcutPath, Application.Title);
  end;
end;

procedure TSystemMenuManager.AddToDesktop;  
var
  DesktopPath: array[0..MAX_PATH] of Char;
  ShortcutPath: string;
begin
  // Obtenir le chemin du bureau
  if SHGetFolderPath(0, CSIDL_DESKTOPDIRECTORY, 0, 0, DesktopPath) = S_OK then
  begin
    ShortcutPath := IncludeTrailingPathDelimiter(DesktopPath) +
                    FApplicationName + '.lnk';

    CreateWindowsShortcut(FApplicationPath, ShortcutPath, Application.Title);
  end;
end;

procedure TSystemMenuManager.PinToTaskbar;  
begin
  // Windows 10+ : épingler à la barre des tâches
  // Ceci est plus complexe car Microsoft a restreint l'API
  // Une approche est d'utiliser PowerShell
  RunCommand('powershell',
    ['-Command',
     Format('(New-Object -ComObject Shell.Application).Namespace("%s").ParseName("%s").InvokeVerb("taskbarpin")',
            [ExtractFilePath(FApplicationPath), ExtractFileName(FApplicationPath)])],
    '');
end;
{$ENDIF}

{$IFDEF UNIX}
procedure TSystemMenuManager.CreateLinuxLauncher;  
var
  DesktopFile: TStringList;
  DesktopPath: string;
  IconSourcePath: string;
  IconDestPath: string;
begin
  // Créer le fichier .desktop pour Linux
  DesktopPath := GetEnvironmentVariable('HOME') +
                 '/.local/share/applications/' +
                 FApplicationName + '.desktop';

  DesktopFile := TStringList.Create;
  try
    DesktopFile.Add('[Desktop Entry]');
    DesktopFile.Add('Version=1.1');
    DesktopFile.Add('Type=Application');
    DesktopFile.Add('Name=' + FApplicationName);
    DesktopFile.Add('GenericName=' + Application.Title);
    DesktopFile.Add('Comment=Application développée avec FreePascal/Lazarus');

    // Commande d'exécution
    DesktopFile.Add('Exec=' + FApplicationPath + ' %F');

    // Icône
    if FileExists(FIconPath) then
    begin
      // Copier l'icône dans le dossier des icônes
      IconDestPath := GetEnvironmentVariable('HOME') +
                     '/.local/share/icons/hicolor/48x48/apps/' +
                     FApplicationName + '.png';
      ForceDirectories(ExtractFilePath(IconDestPath));
      CopyFile(FIconPath, IconDestPath);
      DesktopFile.Add('Icon=' + FApplicationName);
    end
    else
      DesktopFile.Add('Icon=application-x-executable');

    // Autres propriétés
    DesktopFile.Add('Terminal=false');
    DesktopFile.Add('Categories=Application;Utility;');
    DesktopFile.Add('Keywords=' + FApplicationName + ';');

    // Actions supplémentaires (optionnel)
    DesktopFile.Add('Actions=new-window;preferences;');
    DesktopFile.Add('');
    DesktopFile.Add('[Desktop Action new-window]');
    DesktopFile.Add('Name=Nouvelle fenêtre');
    DesktopFile.Add('Exec=' + FApplicationPath + ' --new-window');
    DesktopFile.Add('');
    DesktopFile.Add('[Desktop Action preferences]');
    DesktopFile.Add('Name=Préférences');
    DesktopFile.Add('Exec=' + FApplicationPath + ' --preferences');

    // Créer les dossiers nécessaires
    ForceDirectories(ExtractFilePath(DesktopPath));

    // Sauvegarder le fichier
    DesktopFile.SaveToFile(DesktopPath);

    // Rendre le fichier exécutable
    fpChmod(DesktopPath, &755);

  finally
    DesktopFile.Free;
  end;
end;

procedure TSystemMenuManager.AddToApplicationMenu;  
var
  Process: TProcess;
begin
  // Mettre à jour la base de données des applications
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'update-desktop-database';
    Process.Parameters.Add(GetEnvironmentVariable('HOME') +
                          '/.local/share/applications');
    Process.Options := [poWaitOnExit, poNoConsole];

    try
      Process.Execute;
    except
      // Si la commande n'existe pas, ce n'est pas grave
    end;
  finally
    Process.Free;
  end;

  // Pour GNOME, rafraîchir le cache des icônes
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'gtk-update-icon-cache';
    Process.Parameters.Add('-f');
    Process.Parameters.Add('-t');
    Process.Parameters.Add(GetEnvironmentVariable('HOME') +
                          '/.local/share/icons/hicolor');
    Process.Options := [poWaitOnExit, poNoConsole];

    try
      Process.Execute;
    except
      // Commande optionnelle
    end;
  finally
    Process.Free;
  end;
end;

procedure TSystemMenuManager.AddToDesktop;  
var
  DesktopPath: string;
  SourcePath: string;
begin
  // Obtenir le chemin du bureau
  DesktopPath := GetEnvironmentVariable('HOME') + '/Desktop/';

  // Certains systèmes utilisent un nom localisé
  if not DirectoryExists(DesktopPath) then
    DesktopPath := GetEnvironmentVariable('HOME') + '/Bureau/';

  if DirectoryExists(DesktopPath) then
  begin
    SourcePath := GetEnvironmentVariable('HOME') +
                  '/.local/share/applications/' +
                  FApplicationName + '.desktop';

    if FileExists(SourcePath) then
    begin
      // Créer un lien sur le bureau
      CopyFile(SourcePath, DesktopPath + FApplicationName + '.desktop');

      // Rendre exécutable
      fpChmod(DesktopPath + FApplicationName + '.desktop', &755);

      // Sur certains environnements, marquer comme de confiance
      if FileExists('/usr/bin/gio') then
      begin
        RunCommand('/usr/bin/gio',
          ['set', DesktopPath + FApplicationName + '.desktop',
           'metadata::trusted', 'true'], '');
      end;
    end;
  end;
end;

procedure TSystemMenuManager.AddToDock;  
begin
  // L'ajout au dock dépend de l'environnement de bureau

  // Pour GNOME (via gsettings)
  if FileExists('/usr/bin/gsettings') then
  begin
    // Récupérer les favoris actuels et ajouter notre application
    // C'est complexe car il faut parser la liste existante
    // Exemple simplifié :
    RunCommand('/usr/bin/gsettings',
      ['set', 'org.gnome.shell', 'favorite-apps',
       '[''firefox.desktop'', ''' + FApplicationName + '.desktop'']'], '');
  end;

  // Pour Unity/Ubuntu
  if FileExists('/usr/bin/gsettings') then
  begin
    // Unity utilise un schéma différent
    RunCommand('/usr/bin/gsettings',
      ['set', 'com.canonical.Unity.Launcher', 'favorites',
       '[''application://' + FApplicationName + '.desktop'']'], '');
  end;
end;
{$ENDIF}

procedure TSystemMenuManager.RemoveShortcuts;  
begin
  {$IFDEF WINDOWS}
  var
    StartMenuPath: array[0..MAX_PATH] of Char;
    DesktopPath: array[0..MAX_PATH] of Char;
  begin
    // Supprimer du menu Démarrer
    if SHGetFolderPath(0, CSIDL_PROGRAMS, 0, 0, StartMenuPath) = S_OK then
      DeleteFile(IncludeTrailingPathDelimiter(StartMenuPath) +
                FApplicationName + '.lnk');

    // Supprimer du bureau
    if SHGetFolderPath(0, CSIDL_DESKTOPDIRECTORY, 0, 0, DesktopPath) = S_OK then
      DeleteFile(IncludeTrailingPathDelimiter(DesktopPath) +
                FApplicationName + '.lnk');
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  begin
    // Supprimer le fichier .desktop
    DeleteFile(GetEnvironmentVariable('HOME') +
              '/.local/share/applications/' +
              FApplicationName + '.desktop');

    // Supprimer l'icône
    DeleteFile(GetEnvironmentVariable('HOME') +
              '/.local/share/icons/hicolor/48x48/apps/' +
              FApplicationName + '.png');

    // Supprimer du bureau
    DeleteFile(GetEnvironmentVariable('HOME') + '/Desktop/' +
              FApplicationName + '.desktop');
    DeleteFile(GetEnvironmentVariable('HOME') + '/Bureau/' +
              FApplicationName + '.desktop');

    // Mettre à jour les bases de données
    AddToApplicationMenu; // Ceci rafraîchira les caches
  end;
  {$ENDIF}
end;

procedure TSystemMenuManager.CreateQuickLaunchEntry;  
begin
  {$IFDEF WINDOWS}
  var
    QuickLaunchPath: string;
  begin
    // Windows 7+ : épingler à la barre des tâches
    PinToTaskbar;

    // Windows XP/Vista : Quick Launch
    QuickLaunchPath := GetEnvironmentVariable('APPDATA') +
                      '\Microsoft\Internet Explorer\Quick Launch\';
    if DirectoryExists(QuickLaunchPath) then
    begin
      CreateWindowsShortcut(FApplicationPath,
                          QuickLaunchPath + FApplicationName + '.lnk',
                          Application.Title);
    end;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Sur Linux, ajouter au dock/panel
  AddToDock;
  {$ENDIF}
end;

end.
```

## Intégration avec les thèmes système

### Adaptation automatique aux thèmes

Pour que votre application s'intègre visuellement avec le système, elle doit respecter les préférences de thème de l'utilisateur.

```pascal
unit ThemeIntegration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls
  {$IFDEF WINDOWS}
  , Windows, Registry, Themes
  {$ENDIF}
  {$IFDEF UNIX}
  , Process, IniFiles
  {$ENDIF};

type
  TThemeStyle = (tsLight, tsDark, tsSystem);

  TThemeManager = class
  private
    FCurrentTheme: TThemeStyle;
    FOnThemeChange: TNotifyEvent;

    {$IFDEF WINDOWS}
    function GetWindowsTheme: TThemeStyle;
    function IsWindowsDarkMode: Boolean;
    procedure ApplyWindowsTheme;
    {$ENDIF}

    {$IFDEF UNIX}
    function GetLinuxTheme: TThemeStyle;
    function GetGTKTheme: string;
    procedure ApplyLinuxTheme;
    {$ENDIF}

    procedure UpdateApplicationColors;
  public
    constructor Create;

    procedure DetectSystemTheme;
    procedure ApplyTheme(ATheme: TThemeStyle);
    procedure RegisterThemeChangeHandler;

    property CurrentTheme: TThemeStyle read FCurrentTheme;
    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

implementation

constructor TThemeManager.Create;  
begin
  inherited Create;
  FCurrentTheme := tsSystem;
  DetectSystemTheme;
end;

procedure TThemeManager.DetectSystemTheme;  
begin
  {$IFDEF WINDOWS}
  FCurrentTheme := GetWindowsTheme;
  ApplyWindowsTheme;
  {$ENDIF}

  {$IFDEF UNIX}
  FCurrentTheme := GetLinuxTheme;
  ApplyLinuxTheme;
  {$ENDIF}
end;

{$IFDEF WINDOWS}
function TThemeManager.GetWindowsTheme: TThemeStyle;  
begin
  if IsWindowsDarkMode then
    Result := tsDark
  else
    Result := tsLight;
end;

function TThemeManager.IsWindowsDarkMode: Boolean;  
var
  Reg: TRegistry;
  LightTheme: Integer;
begin
  Result := False;

  // Windows 10+ : vérifier le thème dans le registre
  if Win32MajorVersion >= 10 then
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize') then
      begin
        try
          // 0 = Dark, 1 = Light
          LightTheme := Reg.ReadInteger('AppsUseLightTheme');
          Result := (LightTheme = 0);
        except
          Result := False;
        end;
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

procedure TThemeManager.ApplyWindowsTheme;  
begin
  // Activer les styles visuels Windows si disponibles
  if ThemeServices.ThemesEnabled then
  begin
    Application.UpdateFormatSettings := True;

    // Windows 10+ : support du dark mode
    if (Win32MajorVersion >= 10) and (FCurrentTheme = tsDark) then
    begin
      // Appliquer le thème sombre aux fenêtres
      // Note : nécessite des API Windows non documentées
      // ou l'utilisation de manifests spécifiques
    end;
  end;

  UpdateApplicationColors;
end;
{$ENDIF}

{$IFDEF UNIX}
function TThemeManager.GetLinuxTheme: TThemeStyle;  
var
  ThemeName: string;
begin
  ThemeName := LowerCase(GetGTKTheme);

  // Détecter si c'est un thème sombre
  if (Pos('dark', ThemeName) > 0) or
     (Pos('noir', ThemeName) > 0) or
     (Pos('black', ThemeName) > 0) then
    Result := tsDark
  else
    Result := tsLight;
end;

function TThemeManager.GetGTKTheme: string;  
var
  Process: TProcess;
  Output: TStringList;
  Ini: TIniFile;
begin
  Result := '';

  // Essayer de récupérer le thème via gsettings
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'gsettings';
    Process.Parameters.Add('get');
    Process.Parameters.Add('org.gnome.desktop.interface');
    Process.Parameters.Add('gtk-theme');
    Process.Options := [poWaitOnExit, poUsePipes, poNoConsole];

    try
      Process.Execute;
      Output.LoadFromStream(Process.Output);
      if Output.Count > 0 then
        Result := Trim(Output[0]);
    except
      // Méthode alternative : lire le fichier de configuration GTK
      if FileExists(GetEnvironmentVariable('HOME') + '/.config/gtk-3.0/settings.ini') then
      begin
        Ini := TIniFile.Create(GetEnvironmentVariable('HOME') +
                               '/.config/gtk-3.0/settings.ini');
        try
          Result := Ini.ReadString('Settings', 'gtk-theme-name', '');
        finally
          Ini.Free;
        end;
      end;
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TThemeManager.ApplyLinuxTheme;  
begin
  // Sur Linux, la LCL s'adapte généralement automatiquement
  // au thème GTK/Qt, mais on peut forcer certains paramètres

  UpdateApplicationColors;
end;
{$ENDIF}

procedure TThemeManager.UpdateApplicationColors;  
begin
  // Adapter les couleurs de l'application selon le thème
  case FCurrentTheme of
    tsDark:
      begin
        // Couleurs pour thème sombre
        Application.MainForm.Color := RGBToColor(30, 30, 30);
        Application.MainForm.Font.Color := clWhite;

        // Parcourir tous les contrôles et adapter leurs couleurs
        UpdateControlColors(Application.MainForm, True);
      end;

    tsLight:
      begin
        // Couleurs pour thème clair
        Application.MainForm.Color := clBtnFace;
        Application.MainForm.Font.Color := clBlack;

        UpdateControlColors(Application.MainForm, False);
      end;
  end;

  // Déclencher l'événement de changement
  if Assigned(FOnThemeChange) then
    FOnThemeChange(Self);
end;

procedure UpdateControlColors(AParent: TWinControl; ADarkMode: Boolean);  
var
  i: Integer;
  Control: TControl;
begin
  for i := 0 to AParent.ControlCount - 1 do
  begin
    Control := AParent.Controls[i];

    if ADarkMode then
    begin
      // Couleurs sombres
      if Control is TWinControl then
      begin
        TWinControl(Control).Color := RGBToColor(45, 45, 45);
        TWinControl(Control).Font.Color := clWhite;
      end;
    end
    else
    begin
      // Couleurs claires
      if Control is TWinControl then
      begin
        TWinControl(Control).Color := clWindow;
        TWinControl(Control).Font.Color := clWindowText;
      end;
    end;

    // Récursion pour les contrôles enfants
    if Control is TWinControl then
      UpdateControlColors(TWinControl(Control), ADarkMode);
  end;
end;

procedure TThemeManager.ApplyTheme(ATheme: TThemeStyle);  
begin
  FCurrentTheme := ATheme;

  if ATheme = tsSystem then
    DetectSystemTheme
  else
    UpdateApplicationColors;
end;

procedure TThemeManager.RegisterThemeChangeHandler;  
begin
  {$IFDEF WINDOWS}
  // Sur Windows, on peut surveiller les changements de registre
  // ou utiliser WM_SETTINGCHANGE
  {$ENDIF}

  {$IFDEF UNIX}
  // Sur Linux, on peut surveiller les changements de gsettings
  // via D-Bus ou des signaux
  {$ENDIF}
end;

end.
```

## Services système et démarrage automatique

### Configuration du démarrage automatique

```pascal
unit AutoStartManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}
  , Registry, Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , Process, BaseUnix
  {$ENDIF};

type
  TAutoStartManager = class
  private
    FApplicationName: string;
    FApplicationPath: string;

    {$IFDEF WINDOWS}
    procedure SetWindowsAutoStart(AEnable: Boolean);
    function GetWindowsAutoStart: Boolean;
    {$ENDIF}

    {$IFDEF UNIX}
    procedure SetLinuxAutoStart(AEnable: Boolean);
    function GetLinuxAutoStart: Boolean;
    procedure CreateSystemdService;
    {$ENDIF}
  public
    constructor Create;

    procedure EnableAutoStart;
    procedure DisableAutoStart;
    function IsAutoStartEnabled: Boolean;
    procedure CreateServiceEntry;
  end;

implementation

constructor TAutoStartManager.Create;  
begin
  inherited Create;
  FApplicationPath := ParamStr(0);
  FApplicationName := ChangeFileExt(ExtractFileName(FApplicationPath), '');
end;

procedure TAutoStartManager.EnableAutoStart;  
begin
  {$IFDEF WINDOWS}
  SetWindowsAutoStart(True);
  {$ENDIF}

  {$IFDEF UNIX}
  SetLinuxAutoStart(True);
  {$ENDIF}
end;

procedure TAutoStartManager.DisableAutoStart;  
begin
  {$IFDEF WINDOWS}
  SetWindowsAutoStart(False);
  {$ENDIF}

  {$IFDEF UNIX}
  SetLinuxAutoStart(False);
  {$ENDIF}
end;

function TAutoStartManager.IsAutoStartEnabled: Boolean;  
begin
  {$IFDEF WINDOWS}
  Result := GetWindowsAutoStart;
  {$ENDIF}

  {$IFDEF UNIX}
  Result := GetLinuxAutoStart;
  {$ENDIF}
end;

{$IFDEF WINDOWS}
procedure TAutoStartManager.SetWindowsAutoStart(AEnable: Boolean);  
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run', True) then
    begin
      if AEnable then
        Reg.WriteString(FApplicationName, '"' + FApplicationPath + '"')
      else
        Reg.DeleteValue(FApplicationName);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TAutoStartManager.GetWindowsAutoStart: Boolean;  
var
  Reg: TRegistry;
begin
  Result := False;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Run') then
    begin
      Result := Reg.ValueExists(FApplicationName);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;
{$ENDIF}

{$IFDEF UNIX}
procedure TAutoStartManager.SetLinuxAutoStart(AEnable: Boolean);  
var
  AutostartDir: string;
  DesktopFile: string;
  SourceFile: string;
begin
  // Dossier autostart de l'utilisateur
  AutostartDir := GetEnvironmentVariable('HOME') + '/.config/autostart/';
  ForceDirectories(AutostartDir);

  DesktopFile := AutostartDir + FApplicationName + '.desktop';
  SourceFile := GetEnvironmentVariable('HOME') +
                '/.local/share/applications/' +
                FApplicationName + '.desktop';

  if AEnable then
  begin
    // Copier le fichier .desktop dans autostart
    if FileExists(SourceFile) then
      CopyFile(SourceFile, DesktopFile)
    else
    begin
      // Créer un fichier .desktop minimal
      with TStringList.Create do
      try
        Add('[Desktop Entry]');
        Add('Type=Application');
        Add('Name=' + FApplicationName);
        Add('Exec=' + FApplicationPath);
        Add('Hidden=false');
        Add('NoDisplay=false');
        Add('X-GNOME-Autostart-enabled=true');
        SaveToFile(DesktopFile);
      finally
        Free;
      end;
    end;
  end
  else
  begin
    // Supprimer le fichier autostart
    if FileExists(DesktopFile) then
      DeleteFile(DesktopFile);
  end;
end;

function TAutoStartManager.GetLinuxAutoStart: Boolean;  
var
  DesktopFile: string;
begin
  DesktopFile := GetEnvironmentVariable('HOME') + '/.config/autostart/' +
                 FApplicationName + '.desktop';
  Result := FileExists(DesktopFile);
end;

procedure TAutoStartManager.CreateSystemdService;  
var
  ServiceFile: TStringList;
  ServicePath: string;
begin
  // Créer un service systemd pour l'utilisateur
  ServicePath := GetEnvironmentVariable('HOME') +
                 '/.config/systemd/user/' +
                 FApplicationName + '.service';

  ForceDirectories(ExtractFilePath(ServicePath));

  ServiceFile := TStringList.Create;
  try
    ServiceFile.Add('[Unit]');
    ServiceFile.Add('Description=' + Application.Title);
    ServiceFile.Add('After=graphical-session.target');
    ServiceFile.Add('');
    ServiceFile.Add('[Service]');
    ServiceFile.Add('Type=simple');
    ServiceFile.Add('ExecStart=' + FApplicationPath);
    ServiceFile.Add('Restart=on-failure');
    ServiceFile.Add('RestartSec=10');
    ServiceFile.Add('');
    ServiceFile.Add('[Install]');
    ServiceFile.Add('WantedBy=default.target');

    ServiceFile.SaveToFile(ServicePath);

    // Activer le service
    RunCommand('systemctl',
      ['--user', 'daemon-reload'], '');
    RunCommand('systemctl',
      ['--user', 'enable', FApplicationName + '.service'], '');
  finally
    ServiceFile.Free;
  end;
end;
{$ENDIF}

procedure TAutoStartManager.CreateServiceEntry;  
begin
  {$IFDEF WINDOWS}
  // Sur Windows, créer un service nécessite des privilèges admin
  // et l'utilisation de l'API Service Control Manager
  // Ceci est plus complexe et nécessite une implémentation dédiée
  {$ENDIF}

  {$IFDEF UNIX}
  // Sur Linux, créer un service systemd utilisateur
  CreateSystemdService;
  {$ENDIF}
end;

end.
```

## Utilisation complète dans une application

### Exemple d'intégration dans le formulaire principal

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  SystemTrayIntegration, NativeNotifications, FileAssociation,
  SystemMenuIntegration, ThemeIntegration, AutoStartManager;

type
  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuOptions: TMenuItem;
    MenuHelp: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuOptionsClick(Sender: TObject);
  private
    FTrayManager: TSystemTrayManager;
    FNotifications: TNativeNotification;
    FFileAssoc: TFileAssociationManager;
    FMenuManager: TSystemMenuManager;
    FThemeManager: TThemeManager;
    FAutoStart: TAutoStartManager;

    procedure InitializeNativeIntegration;
    procedure HandleCommandLineFile;
    procedure OnThemeChanged(Sender: TObject);
  public
    procedure ShowNotification(const ATitle, AMessage: string);
  end;

var
  FormMain: TFormMain;

implementation

procedure TFormMain.FormCreate(Sender: TObject);  
begin
  // Initialiser l'intégration native
  InitializeNativeIntegration;

  // Gérer les fichiers passés en paramètre
  HandleCommandLineFile;
end;

procedure TFormMain.InitializeNativeIntegration;  
begin
  // Créer le gestionnaire de zone de notification
  FTrayManager := TSystemTrayManager.Create(Self);

  // Créer le système de notifications
  FNotifications := TNativeNotification.Create;

  // Gestionnaire d'association de fichiers
  FFileAssoc := TFileAssociationManager.Create;

  // Vérifier si c'est la première exécution
  if not FFileAssoc.IsRegistered('.myext') then
  begin
    if MessageDlg('Première exécution',
                  'Voulez-vous associer les fichiers .myext avec cette application ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      FFileAssoc.RegisterExtension('.myext',
                                  'Fichier Mon Application',
                                  Application.Icon.FileName);
    end;
  end;

  // Gestionnaire de menu système
  FMenuManager := TSystemMenuManager.Create;

  // Gestionnaire de thème
  FThemeManager := TThemeManager.Create;
  FThemeManager.OnThemeChange := @OnThemeChanged;

  // Gestionnaire de démarrage automatique
  FAutoStart := TAutoStartManager.Create;

  // Afficher une notification de bienvenue
  ShowNotification('Bienvenue',
                  'L''application est prête et intégrée au système');
end;

procedure TFormMain.FormDestroy(Sender: TObject);  
begin
  // Libérer les ressources
  FTrayManager.Free;
  FNotifications.Free;
  FFileAssoc.Free;
  FMenuManager.Free;
  FThemeManager.Free;
  FAutoStart.Free;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);  
begin
  // Au lieu de fermer, minimiser dans la zone de notification
  {$IFDEF WINDOWS}
  if FTrayManager <> nil then
  begin
    CanClose := False;
    Hide;
    FTrayManager.ShowBalloonHint('Application minimisée',
                                 'L''application continue en arrière-plan',
                                 bfInfo);
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Sur Linux, demander confirmation
  if MessageDlg('Fermeture',
                'Minimiser dans la zone de notification ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    CanClose := False;
    Hide;
  end;
  {$ENDIF}
end;

procedure TFormMain.HandleCommandLineFile;  
begin
  // Si un fichier est passé en paramètre
  if ParamCount > 0 then
  begin
    if FileExists(ParamStr(1)) then
    begin
      // Ouvrir le fichier
      FFileAssoc.HandleOpenedFile(ParamStr(1));
    end;
  end;
end;

procedure TFormMain.ShowNotification(const ATitle, AMessage: string);  
begin
  if Assigned(FNotifications) then
    FNotifications.Show(ATitle, AMessage, ntInfo);
end;

procedure TFormMain.OnThemeChanged(Sender: TObject);  
begin
  // Réagir au changement de thème
  case FThemeManager.CurrentTheme of
    tsDark:
      ShowMessage('Thème sombre activé');
    tsLight:
      ShowMessage('Thème clair activé');
  end;
end;

procedure TFormMain.MenuOptionsClick(Sender: TObject);  
begin
  // Dialogue d'options avec intégration native
  with TFormOptions.Create(Self) do
  try
    // Charger les paramètres actuels
    CheckBoxAutoStart.Checked := FAutoStart.IsAutoStartEnabled;
    CheckBoxSystemTray.Checked := FTrayManager.IsVisible;
    CheckBoxNotifications.Checked := FNotifications.Enabled;

    // Afficher le dialogue
    if ShowModal = mrOK then
    begin
      // Appliquer les changements
      if CheckBoxAutoStart.Checked then
        FAutoStart.EnableAutoStart
      else
        FAutoStart.DisableAutoStart;

      FTrayManager.SetVisible(CheckBoxSystemTray.Checked);
      FNotifications.Enabled := CheckBoxNotifications.Checked;

      // Sauvegarder les préférences
      SavePreferences;
    end;
  finally
    Free;
  end;
end;

end.
```

## Formulaire d'options avec intégration native

### Création du dialogue de configuration

```pascal
unit OptionsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons;

type
  TFormOptions = class(TForm)
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabIntegration: TTabSheet;
    TabAppearance: TTabSheet;
    TabAdvanced: TTabSheet;

    // Onglet Général
    GroupBoxStartup: TGroupBox;
    CheckBoxAutoStart: TCheckBox;
    CheckBoxMinimizeOnStart: TCheckBox;
    CheckBoxCheckUpdates: TCheckBox;

    // Onglet Intégration
    GroupBoxSystemIntegration: TGroupBox;
    CheckBoxSystemTray: TCheckBox;
    CheckBoxNotifications: TCheckBox;
    LabelFileAssoc: TLabel;
    ListBoxFileTypes: TListBox;
    ButtonAddFileType: TButton;
    ButtonRemoveFileType: TButton;

    // Onglet Apparence
    GroupBoxTheme: TGroupBox;
    RadioButtonThemeSystem: TRadioButton;
    RadioButtonThemeLight: TRadioButton;
    RadioButtonThemeDark: TRadioButton;
    CheckBoxCustomColors: TCheckBox;

    // Onglet Avancé
    GroupBoxPlatformSpecific: TGroupBox;
    {$IFDEF WINDOWS}
    CheckBoxWindowsJumpList: TCheckBox;
    CheckBoxWindowsToast: TCheckBox;
    CheckBoxRunAsAdmin: TCheckBox;
    {$ENDIF}
    {$IFDEF UNIX}
    CheckBoxUnityIntegration: TCheckBox;
    CheckBoxAppIndicator: TCheckBox;
    ComboBoxDesktopEnvironment: TComboBox;
    {$ENDIF}

    // Boutons de dialogue
    PanelButtons: TPanel;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonApply: TBitBtn;

    procedure FormCreate(Sender: TObject);
    procedure ButtonAddFileTypeClick(Sender: TObject);
    procedure ButtonRemoveFileTypeClick(Sender: TObject);
    procedure RadioButtonThemeChange(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure CheckBoxAutoStartChange(Sender: TObject);
  private
    FFileAssociations: TStringList;
    procedure LoadCurrentSettings;
    procedure ApplySettings;
    procedure UpdatePlatformSpecificOptions;
  public
    property FileAssociations: TStringList read FFileAssociations;
  end;

implementation

uses
  FileAssociation, ThemeIntegration, AutoStartManager
  {$IFDEF WINDOWS}
  , Windows, Registry
  {$ENDIF}
  {$IFDEF UNIX}
  , Process
  {$ENDIF};

procedure TFormOptions.FormCreate(Sender: TObject);  
begin
  FFileAssociations := TStringList.Create;

  // Configurer l'interface selon la plateforme
  UpdatePlatformSpecificOptions;

  // Charger les paramètres actuels
  LoadCurrentSettings;

  // Ajuster la taille du dialogue
  {$IFDEF WINDOWS}
  Height := 450;
  Width := 550;
  {$ENDIF}

  {$IFDEF UNIX}
  // Linux a souvent besoin de plus d'espace
  Height := 480;
  Width := 580;
  {$ENDIF}
end;

procedure TFormOptions.LoadCurrentSettings;  
var
  Config: TIniFile;
  ConfigPath: string;
begin
  // Déterminer le chemin du fichier de configuration
  {$IFDEF WINDOWS}
  ConfigPath := GetEnvironmentVariable('APPDATA') + '\' +
                Application.Title + '\settings.ini';
  {$ENDIF}

  {$IFDEF UNIX}
  ConfigPath := GetEnvironmentVariable('HOME') + '/.config/' +
                LowerCase(Application.Title) + '/settings.ini';
  {$ENDIF}

  if not FileExists(ConfigPath) then
    Exit;

  Config := TIniFile.Create(ConfigPath);
  try
    // Général
    CheckBoxAutoStart.Checked := Config.ReadBool('General', 'AutoStart', False);
    CheckBoxMinimizeOnStart.Checked := Config.ReadBool('General', 'MinimizeOnStart', False);
    CheckBoxCheckUpdates.Checked := Config.ReadBool('General', 'CheckUpdates', True);

    // Intégration
    CheckBoxSystemTray.Checked := Config.ReadBool('Integration', 'SystemTray', True);
    CheckBoxNotifications.Checked := Config.ReadBool('Integration', 'Notifications', True);

    // Associations de fichiers
    Config.ReadSection('FileAssociations', FFileAssociations);
    ListBoxFileTypes.Items := FFileAssociations;

    // Apparence
    case Config.ReadInteger('Appearance', 'Theme', 0) of
      1: RadioButtonThemeLight.Checked := True;
      2: RadioButtonThemeDark.Checked := True;
    else
      RadioButtonThemeSystem.Checked := True;
    end;
    CheckBoxCustomColors.Checked := Config.ReadBool('Appearance', 'CustomColors', False);

    // Options spécifiques à la plateforme
    {$IFDEF WINDOWS}
    CheckBoxWindowsJumpList.Checked := Config.ReadBool('Windows', 'JumpList', True);
    CheckBoxWindowsToast.Checked := Config.ReadBool('Windows', 'ToastNotifications', True);
    CheckBoxRunAsAdmin.Checked := Config.ReadBool('Windows', 'RunAsAdmin', False);
    {$ENDIF}

    {$IFDEF UNIX}
    CheckBoxUnityIntegration.Checked := Config.ReadBool('Linux', 'UnityIntegration', True);
    CheckBoxAppIndicator.Checked := Config.ReadBool('Linux', 'AppIndicator', True);
    ComboBoxDesktopEnvironment.ItemIndex := Config.ReadInteger('Linux', 'DesktopEnvironment', 0);
    {$ENDIF}

  finally
    Config.Free;
  end;
end;

procedure TFormOptions.ApplySettings;  
var
  Config: TIniFile;
  ConfigPath: string;
  i: Integer;
begin
  // Créer le dossier de configuration si nécessaire
  {$IFDEF WINDOWS}
  ConfigPath := GetEnvironmentVariable('APPDATA') + '\' + Application.Title;
  ForceDirectories(ConfigPath);
  ConfigPath := ConfigPath + '\settings.ini';
  {$ENDIF}

  {$IFDEF UNIX}
  ConfigPath := GetEnvironmentVariable('HOME') + '/.config/' +
                LowerCase(Application.Title);
  ForceDirectories(ConfigPath);
  ConfigPath := ConfigPath + '/settings.ini';
  {$ENDIF}

  Config := TIniFile.Create(ConfigPath);
  try
    // Général
    Config.WriteBool('General', 'AutoStart', CheckBoxAutoStart.Checked);
    Config.WriteBool('General', 'MinimizeOnStart', CheckBoxMinimizeOnStart.Checked);
    Config.WriteBool('General', 'CheckUpdates', CheckBoxCheckUpdates.Checked);

    // Intégration
    Config.WriteBool('Integration', 'SystemTray', CheckBoxSystemTray.Checked);
    Config.WriteBool('Integration', 'Notifications', CheckBoxNotifications.Checked);

    // Associations de fichiers
    Config.EraseSection('FileAssociations');
    for i := 0 to ListBoxFileTypes.Items.Count - 1 do
      Config.WriteString('FileAssociations',
                        ListBoxFileTypes.Items[i],
                        'registered');

    // Apparence
    if RadioButtonThemeLight.Checked then
      Config.WriteInteger('Appearance', 'Theme', 1)
    else if RadioButtonThemeDark.Checked then
      Config.WriteInteger('Appearance', 'Theme', 2)
    else
      Config.WriteInteger('Appearance', 'Theme', 0);

    Config.WriteBool('Appearance', 'CustomColors', CheckBoxCustomColors.Checked);

    // Options spécifiques à la plateforme
    {$IFDEF WINDOWS}
    Config.WriteBool('Windows', 'JumpList', CheckBoxWindowsJumpList.Checked);
    Config.WriteBool('Windows', 'ToastNotifications', CheckBoxWindowsToast.Checked);
    Config.WriteBool('Windows', 'RunAsAdmin', CheckBoxRunAsAdmin.Checked);
    {$ENDIF}

    {$IFDEF UNIX}
    Config.WriteBool('Linux', 'UnityIntegration', CheckBoxUnityIntegration.Checked);
    Config.WriteBool('Linux', 'AppIndicator', CheckBoxAppIndicator.Checked);
    Config.WriteInteger('Linux', 'DesktopEnvironment', ComboBoxDesktopEnvironment.ItemIndex);
    {$ENDIF}

  finally
    Config.Free;
  end;
end;

procedure TFormOptions.UpdatePlatformSpecificOptions;  
begin
  {$IFDEF WINDOWS}
  // Afficher les options Windows
  GroupBoxPlatformSpecific.Caption := 'Options Windows';

  // Vérifier la version de Windows
  if Win32MajorVersion >= 10 then
  begin
    CheckBoxWindowsToast.Enabled := True;
    CheckBoxWindowsToast.Caption := 'Utiliser les notifications Windows 10+';
  end
  else
  begin
    CheckBoxWindowsToast.Enabled := False;
    CheckBoxWindowsToast.Caption := 'Notifications Windows 10+ (non disponible)';
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Afficher les options Linux
  GroupBoxPlatformSpecific.Caption := 'Options Linux/Unix';

  // Détecter l'environnement de bureau
  ComboBoxDesktopEnvironment.Items.Clear;
  ComboBoxDesktopEnvironment.Items.Add('Auto-détection');
  ComboBoxDesktopEnvironment.Items.Add('GNOME');
  ComboBoxDesktopEnvironment.Items.Add('KDE');
  ComboBoxDesktopEnvironment.Items.Add('XFCE');
  ComboBoxDesktopEnvironment.Items.Add('Unity');
  ComboBoxDesktopEnvironment.Items.Add('Autre');
  ComboBoxDesktopEnvironment.ItemIndex := 0;

  // Vérifier si Unity est disponible
  CheckBoxUnityIntegration.Enabled := FileExists('/usr/bin/unity');
  {$ENDIF}
end;

procedure TFormOptions.ButtonAddFileTypeClick(Sender: TObject);  
var
  Extension: string;
  Description: string;
  FileAssoc: TFileAssociationManager;
begin
  Extension := InputBox('Nouvelle association',
                       'Extension de fichier (ex: .txt):', '');

  if Extension = '' then
    Exit;

  // Ajouter le point si manquant
  if not Extension.StartsWith('.') then
    Extension := '.' + Extension;

  Description := InputBox('Description',
                         'Description du type de fichier:',
                         'Fichier ' + Application.Title);

  // Créer l'association
  FileAssoc := TFileAssociationManager.Create;
  try
    FileAssoc.RegisterExtension(Extension, Description);

    // Ajouter à la liste
    ListBoxFileTypes.Items.Add(Extension);

    ShowMessage('Association créée pour ' + Extension);
  finally
    FileAssoc.Free;
  end;
end;

procedure TFormOptions.ButtonRemoveFileTypeClick(Sender: TObject);  
var
  FileAssoc: TFileAssociationManager;
  Extension: string;
begin
  if ListBoxFileTypes.ItemIndex < 0 then
    Exit;

  Extension := ListBoxFileTypes.Items[ListBoxFileTypes.ItemIndex];

  if MessageDlg('Suppression',
                'Supprimer l''association pour ' + Extension + ' ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FileAssoc := TFileAssociationManager.Create;
    try
      FileAssoc.UnregisterExtension(Extension);

      // Retirer de la liste
      ListBoxFileTypes.Items.Delete(ListBoxFileTypes.ItemIndex);

      ShowMessage('Association supprimée');
    finally
      FileAssoc.Free;
    end;
  end;
end;

procedure TFormOptions.RadioButtonThemeChange(Sender: TObject);  
var
  ThemeManager: TThemeManager;
  NewTheme: TThemeStyle;
begin
  // Aperçu en temps réel du thème
  ThemeManager := TThemeManager.Create;
  try
    if RadioButtonThemeLight.Checked then
      NewTheme := tsLight
    else if RadioButtonThemeDark.Checked then
      NewTheme := tsDark
    else
      NewTheme := tsSystem;

    ThemeManager.ApplyTheme(NewTheme);
  finally
    ThemeManager.Free;
  end;
end;

procedure TFormOptions.CheckBoxAutoStartChange(Sender: TObject);  
begin
  // Avertir l'utilisateur des implications
  if CheckBoxAutoStart.Checked then
  begin
    {$IFDEF WINDOWS}
    ShowMessage('L''application sera ajoutée au registre Windows ' +
               'pour démarrer automatiquement à l''ouverture de session.');
    {$ENDIF}

    {$IFDEF UNIX}
    ShowMessage('Un fichier .desktop sera créé dans ~/.config/autostart/ ' +
               'pour démarrer l''application automatiquement.');
    {$ENDIF}
  end;
end;

procedure TFormOptions.ButtonApplyClick(Sender: TObject);  
begin
  ApplySettings;
  ShowMessage('Paramètres appliqués');
end;

end.
```

## Gestion avancée des droits et permissions

### Élévation de privilèges et permissions spéciales

```pascal
unit PermissionsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}
  , Windows, ShellAPI
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix, Users
  {$ENDIF};

type
  TPermissionsManager = class
  private
    {$IFDEF WINDOWS}
    function IsRunAsAdmin: Boolean;
    function RunElevated(const AProgram: string; AParameters: string = ''): Boolean;
    {$ENDIF}

    {$IFDEF UNIX}
    function IsRunAsRoot: Boolean;
    function HasSudoRights: Boolean;
    function RunWithSudo(const ACommand: string): Boolean;
    {$ENDIF}
  public
    function IsElevated: Boolean;
    function RequestElevation: Boolean;
    function RestartElevated: Boolean;
    function CheckPermission(const AResource: string): Boolean;
  end;

implementation

function TPermissionsManager.IsElevated: Boolean;  
begin
  {$IFDEF WINDOWS}
  Result := IsRunAsAdmin;
  {$ENDIF}

  {$IFDEF UNIX}
  Result := IsRunAsRoot;
  {$ENDIF}
end;

{$IFDEF WINDOWS}
function TPermissionsManager.IsRunAsAdmin: Boolean;  
var
  TokenHandle: THandle;
  TokenInformation: TOKEN_ELEVATION;
  ReturnLength: DWORD;
begin
  Result := False;

  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
  begin
    try
      if GetTokenInformation(TokenHandle, TokenElevation,
                           @TokenInformation, SizeOf(TokenInformation),
                           ReturnLength) then
      begin
        Result := TokenInformation.TokenIsElevated <> 0;
      end;
    finally
      CloseHandle(TokenHandle);
    end;
  end;
end;

function TPermissionsManager.RunElevated(const AProgram: string;
                                        AParameters: string): Boolean;
var
  ShellExecuteInfo: TShellExecuteInfo;
begin
  FillChar(ShellExecuteInfo, SizeOf(ShellExecuteInfo), 0);
  ShellExecuteInfo.cbSize := SizeOf(ShellExecuteInfo);
  ShellExecuteInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  ShellExecuteInfo.lpVerb := 'runas';
  ShellExecuteInfo.lpFile := PChar(AProgram);

  if AParameters <> '' then
    ShellExecuteInfo.lpParameters := PChar(AParameters);

  ShellExecuteInfo.nShow := SW_NORMAL;

  Result := ShellExecuteEx(@ShellExecuteInfo);

  if Result then
    CloseHandle(ShellExecuteInfo.hProcess);
end;
{$ENDIF}

{$IFDEF UNIX}
function TPermissionsManager.IsRunAsRoot: Boolean;  
begin
  Result := fpGetUID = 0;
end;

function TPermissionsManager.HasSudoRights: Boolean;  
var
  Output: string;
begin
  // Vérifier si l'utilisateur peut utiliser sudo
  Result := RunCommand('sudo', ['-n', 'true'], Output);
end;

function TPermissionsManager.RunWithSudo(const ACommand: string): Boolean;  
var
  Output: string;
begin
  // Utiliser pkexec pour une interface graphique
  if FileExists('/usr/bin/pkexec') then
    Result := RunCommand('/usr/bin/pkexec', [ACommand], Output)
  else
    // Utiliser gksudo ou kdesudo si disponible
    if FileExists('/usr/bin/gksudo') then
      Result := RunCommand('/usr/bin/gksudo', [ACommand], Output)
    else if FileExists('/usr/bin/kdesudo') then
      Result := RunCommand('/usr/bin/kdesudo', [ACommand], Output)
    else
      Result := False;
end;
{$ENDIF}

function TPermissionsManager.RequestElevation: Boolean;  
begin
  {$IFDEF WINDOWS}
  if not IsRunAsAdmin then
  begin
    if MessageDlg('Privilèges administrateur requis',
                  'Cette opération nécessite des privilèges administrateur. ' +
                  'Voulez-vous redémarrer l''application en tant qu''administrateur ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Result := RestartElevated;
    end
    else
      Result := False;
  end
  else
    Result := True;
  {$ENDIF}

  {$IFDEF UNIX}
  if not IsRunAsRoot then
  begin
    if MessageDlg('Privilèges root requis',
                  'Cette opération nécessite des privilèges root. ' +
                  'Voulez-vous utiliser sudo ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Result := RunWithSudo(ParamStr(0));
    end
    else
      Result := False;
  end
  else
    Result := True;
  {$ENDIF}
end;

function TPermissionsManager.RestartElevated: Boolean;  
begin
  {$IFDEF WINDOWS}
  Result := RunElevated(ParamStr(0));
  if Result then
    Application.Terminate;
  {$ENDIF}

  {$IFDEF UNIX}
  Result := RunWithSudo(ParamStr(0));
  if Result then
    Application.Terminate;
  {$ENDIF}
end;

function TPermissionsManager.CheckPermission(const AResource: string): Boolean;  
begin
  // Vérifier les permissions pour une ressource spécifique
  {$IFDEF WINDOWS}
  // Vérifier les ACL Windows
  Result := True; // Simplifié
  {$ENDIF}

  {$IFDEF UNIX}
  // Vérifier les permissions Unix
  Result := fpAccess(PChar(AResource), F_OK or R_OK or W_OK) = 0;
  {$ENDIF}
end;

end.
```

## Conseils et bonnes pratiques

### Points clés pour une intégration réussie

#### 1. **Respecter les conventions de chaque OS**

**Windows :**
- Utiliser le registre pour les paramètres
- Stocker les données dans %APPDATA%
- Respecter l'UAC (User Account Control)
- Suivre les guidelines de Microsoft

**Linux :**
- Utiliser les fichiers de configuration texte
- Respecter la hiérarchie XDG (freedesktop.org)
- Stocker dans ~/.config et ~/.local
- Suivre les conventions de l'environnement de bureau

#### 2. **Tester sur différentes versions**

Il est important de tester votre application sur :

**Windows :**
- Windows 7, 8.1, 10, 11
- Versions 32 et 64 bits
- Avec et sans droits administrateur
- Différentes langues système

**Linux :**
- Ubuntu LTS (20.04, 22.04)
- Debian stable
- Fedora
- Différents environnements (GNOME, KDE, XFCE)

#### 3. **Gérer les erreurs gracieusement**

```pascal
procedure SafeNativeOperation;  
begin
  try
    // Opération native
    PerformNativeOperation;
  except
    on E: Exception do
    begin
      {$IFDEF WINDOWS}
      LogError('Erreur Windows: ' + E.Message +
              ' (Code: ' + IntToStr(GetLastError) + ')');
      {$ENDIF}

      {$IFDEF UNIX}
      LogError('Erreur Unix: ' + E.Message +
              ' (Errno: ' + IntToStr(fpGetErrno) + ')');
      {$ENDIF}

      // Continuer avec une fonctionnalité dégradée
      UseFallbackMethod;
    end;
  end;
end;
```

### Architecture modulaire recommandée

```
Application/
├── Core/              # Logique métier pure
├── Platform/          # Abstraction plateforme
│   ├── Interfaces/    # Interfaces communes
│   ├── Windows/       # Implémentation Windows
│   └── Linux/         # Implémentation Linux
├── UI/                # Interface utilisateur
└── Native/            # Intégration native
    ├── Common/        # Code partagé
    ├── Windows/       # Spécifique Windows
    └── Linux/         # Spécifique Linux
```

### Checklist d'intégration native

- [ ] Icône dans la zone de notification
- [ ] Notifications système
- [ ] Association de fichiers
- [ ] Menu Démarrer / Lanceur d'applications
- [ ] Démarrage automatique
- [ ] Respect du thème système
- [ ] Raccourcis clavier système
- [ ] Intégration avec le gestionnaire de fichiers
- [ ] Support du glisser-déposer
- [ ] Clipboard système
- [ ] Internationalisation
- [ ] Accessibilité
- [ ] Mode sombre/clair
- [ ] High-DPI
- [ ] Multi-écrans

## Conclusion

L'intégration native est essentielle pour créer des applications professionnelles qui se sentent "chez elles" sur chaque plateforme. En utilisant les techniques présentées dans ce tutoriel, vous pouvez créer des applications FreePascal/Lazarus qui :

1. **S'intègrent naturellement** avec Windows et Linux
2. **Respectent les attentes** des utilisateurs de chaque OS
3. **Exploitent les fonctionnalités** spécifiques de chaque plateforme
4. **Maintiennent une base de code** unique et maintenable

L'effort supplémentaire pour implémenter ces intégrations natives sera récompensé par une meilleure expérience utilisateur et une adoption plus facile de votre application.

N'oubliez pas que l'intégration native est un processus continu. Les systèmes d'exploitation évoluent, de nouvelles fonctionnalités apparaissent, et les attentes des utilisateurs changent. Restez à jour avec les dernières guidelines et API de chaque plateforme pour maintenir votre application moderne et bien intégrée.

⏭️  [Support ARM et architectures embarquées](/05-developpement-multiplateforme-approfondi/08-support-arm-architectures-embarquees.md)
