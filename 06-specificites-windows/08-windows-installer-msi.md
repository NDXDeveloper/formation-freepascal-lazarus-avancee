🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Windows Installer (MSI) avec FreePascal/Lazarus

## Introduction : Qu'est-ce que Windows Installer ?

### Comprendre MSI

**Windows Installer (MSI)** est la technologie standard de Microsoft pour l'installation, la maintenance et la suppression de logiciels sous Windows. Un fichier MSI est comme une base de données qui contient toutes les instructions pour installer votre application.

### Différence entre MSI et les installateurs traditionnels

**Installateur traditionnel (EXE)** :
- Programme qui copie des fichiers
- Logique d'installation personnalisée
- Désinstallation manuelle

**Windows Installer (MSI)** :
- Base de données déclarative
- Gestion par Windows
- Transactions (peut annuler en cas d'erreur)
- Réparation automatique
- Installation silencieuse standardisée
- Gestion des mises à jour

### Avantages du format MSI

1. **Pour les utilisateurs** :
   - Installation/désinstallation propre
   - Réparation automatique des fichiers
   - Mises à jour intelligentes
   - Support natif de Windows

2. **Pour les entreprises** :
   - Déploiement via Group Policy
   - Installation silencieuse standardisée
   - Personnalisation via transforms (.mst)
   - Inventaire logiciel automatique

3. **Pour les développeurs** :
   - Gestion automatique du registre
   - Rollback en cas d'erreur
   - Détection des conflits
   - Séquençage automatique

## Concepts fondamentaux de MSI

### Structure d'un MSI

Un fichier MSI est une base de données contenant plusieurs tables :

```
MSI Database
├── File Table          (Fichiers à installer)
├── Component Table     (Groupes logiques de fichiers)
├── Feature Table       (Fonctionnalités sélectionnables)
├── Directory Table     (Structure des dossiers)
├── Registry Table      (Entrées registre)
├── Shortcut Table      (Raccourcis)
├── Property Table      (Variables et configuration)
└── ... autres tables
```

### Les composants essentiels

1. **Product Code** : GUID unique identifiant votre produit
2. **Package Code** : GUID unique pour chaque build MSI
3. **Upgrade Code** : GUID constant pour toutes les versions
4. **Version** : Numéro de version (Major.Minor.Build)

### Les niveaux de fonctionnalité

- **Feature** : Fonctionnalité visible par l'utilisateur
- **Component** : Unité atomique d'installation
- **File** : Fichier physique à installer

## Outils pour créer des MSI

### WiX Toolset (Windows Installer XML)

WiX est l'outil open source recommandé pour créer des MSI professionnels.

**Installation de WiX** :
1. Télécharger depuis https://wixtoolset.org
2. Installer WiX Toolset et l'extension Visual Studio (optionnel)
3. Ajouter au PATH : `C:\Program Files (x86)\WiX Toolset v3.11\bin`

### Autres outils

- **Advanced Installer** : Interface graphique (version gratuite disponible)
- **InstallShield** : Solution professionnelle (payante)
- **NSIS avec plugin MSI** : Alternative open source
- **Visual Studio Installer Projects** : Extension gratuite pour VS

## Création d'un MSI simple avec WiX

### Structure de base d'un fichier WiX

Créez `MonApplication.wxs` :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Wix xmlns="http://schemas.microsoft.com/wix/2006/wi">
  <!-- Définition du produit -->
  <Product Id="*"
           Name="Mon Application FreePascal"
           Language="1036"
           Codepage="1252"
           Version="1.0.0.0"
           Manufacturer="Mon Entreprise"
           UpgradeCode="12345678-1234-1234-1234-123456789012">

    <!-- Package MSI -->
    <Package InstallerVersion="200"
             Compressed="yes"
             InstallScope="perMachine"
             Description="Installation de Mon Application"
             Manufacturer="Mon Entreprise"
             Comments="Application développée avec FreePascal/Lazarus" />

    <!-- Empêcher le downgrade -->
    <MajorUpgrade DowngradeErrorMessage="Une version plus récente est déjà installée." />

    <!-- Inclure les fichiers cab dans le MSI -->
    <MediaTemplate EmbedCab="yes" />

    <!-- Structure des features -->
    <Feature Id="ProductFeature"
             Title="Mon Application"
             Level="1">
      <ComponentGroupRef Id="ProductComponents" />
      <ComponentGroupRef Id="Shortcuts" />
    </Feature>

    <!-- Icône pour Ajout/Suppression de programmes -->
    <Icon Id="MonApp.ico" SourceFile="MonApplication.ico"/>
    <Property Id="ARPPRODUCTICON" Value="MonApp.ico" />

  </Product>

  <!-- Structure des répertoires -->
  <Fragment>
    <Directory Id="TARGETDIR" Name="SourceDir">
      <!-- Program Files -->
      <Directory Id="ProgramFilesFolder">
        <Directory Id="CompanyFolder" Name="MonEntreprise">
          <Directory Id="INSTALLFOLDER" Name="MonApplication" />
        </Directory>
      </Directory>

      <!-- Menu Démarrer -->
      <Directory Id="ProgramMenuFolder">
        <Directory Id="ApplicationProgramsFolder" Name="Mon Application"/>
      </Directory>

      <!-- Bureau -->
      <Directory Id="DesktopFolder" Name="Desktop" />
    </Directory>
  </Fragment>

  <!-- Composants à installer -->
  <Fragment>
    <ComponentGroup Id="ProductComponents" Directory="INSTALLFOLDER">
      <!-- Exécutable principal -->
      <Component Id="MainExecutable" Guid="87654321-4321-4321-4321-210987654321">
        <File Id="MonApplication.exe"
              Name="MonApplication.exe"
              Source="$(var.SourceDir)\MonApplication.exe"
              KeyPath="yes">
          <Shortcut Id="DesktopShortcut"
                    Directory="DesktopFolder"
                    Name="Mon Application"
                    Description="Lancer Mon Application"
                    WorkingDirectory="INSTALLFOLDER"
                    Icon="MonApp.ico"
                    IconIndex="0"
                    Advertise="yes" />
        </File>
      </Component>

      <!-- DLLs et dépendances -->
      <Component Id="Dependencies" Guid="11111111-2222-3333-4444-555555555555">
        <File Id="sqlite3.dll" Name="sqlite3.dll" Source="$(var.SourceDir)\sqlite3.dll" />
      </Component>

      <!-- Fichiers de données -->
      <Component Id="DataFiles" Guid="66666666-7777-8888-9999-000000000000">
        <File Id="config.ini" Name="config.ini" Source="$(var.SourceDir)\config.ini" />
      </Component>
    </ComponentGroup>

    <!-- Raccourcis Menu Démarrer -->
    <ComponentGroup Id="Shortcuts" Directory="ApplicationProgramsFolder">
      <Component Id="ApplicationShortcut" Guid="AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE">
        <Shortcut Id="ApplicationStartMenuShortcut"
                  Name="Mon Application"
                  Description="Lancer Mon Application"
                  Target="[INSTALLFOLDER]MonApplication.exe"
                  WorkingDirectory="INSTALLFOLDER"
                  Icon="MonApp.ico"
                  IconIndex="0" />

        <Shortcut Id="UninstallProduct"
                  Name="Désinstaller Mon Application"
                  Description="Désinstalle Mon Application"
                  Target="[System64Folder]msiexec.exe"
                  Arguments="/x [ProductCode]" />

        <RemoveFolder Id="ApplicationProgramsFolder" On="uninstall"/>

        <RegistryValue Root="HKCU"
                       Key="Software\MonEntreprise\MonApplication"
                       Name="installed"
                       Type="integer"
                       Value="1"
                       KeyPath="yes"/>
      </Component>
    </ComponentGroup>
  </Fragment>
</Wix>
```

### Compilation du MSI

Créez `build.bat` :

```batch
@echo off
echo === Compilation du MSI ===

REM Variables  
set SOURCE_DIR=.\bin  
set WIX_DIR=C:\Program Files (x86)\WiX Toolset v3.11\bin

REM Compilation
"%WIX_DIR%\candle.exe" -dSourceDir=%SOURCE_DIR% MonApplication.wxs -o obj\

REM Liaison
"%WIX_DIR%\light.exe" -ext WixUIExtension obj\MonApplication.wixobj -o MonApplication.msi

REM Nettoyage  
del obj\*.wixobj  
del MonApplication.wixpdb

echo === MSI créé avec succès ===  
pause
```

## Interface utilisateur pour l'installateur

### Utiliser les UI prédéfinies de WiX

```xml
<!-- Ajouter dans Product, après MediaTemplate -->
<UIRef Id="WixUI_InstallDir" />
<UIRef Id="WixUI_ErrorProgressText" />

<!-- Propriété pour le répertoire d'installation -->
<Property Id="WIXUI_INSTALLDIR" Value="INSTALLFOLDER" />

<!-- Licence (optionnel) -->
<WixVariable Id="WixUILicenseRtf" Value="Licence.rtf" />

<!-- Bannières personnalisées -->
<WixVariable Id="WixUIBannerBmp" Value="banner.bmp" />
<WixVariable Id="WixUIDialogBmp" Value="dialog.bmp" />
```

### Créer une interface personnalisée

```xml
<UI>
  <!-- Dialogue de bienvenue -->
  <Dialog Id="WelcomeDlg" Width="370" Height="270" Title="Installation">
    <Control Id="Title" Type="Text" X="15" Y="6" Width="200" Height="15">
      <Text>{\WixUI_Font_Title}Bienvenue</Text>
    </Control>

    <Control Id="Description" Type="Text" X="25" Y="23" Width="280" Height="15">
      <Text>Cet assistant va installer Mon Application sur votre ordinateur.</Text>
    </Control>

    <Control Id="BottomLine" Type="Line" X="0" Y="234" Width="370" Height="0" />

    <Control Id="Next" Type="PushButton" X="236" Y="243" Width="56" Height="17" Default="yes">
      <Text>&Suivant</Text>
      <Publish Event="NewDialog" Value="InstallDirDlg">1</Publish>
    </Control>

    <Control Id="Cancel" Type="PushButton" X="304" Y="243" Width="56" Height="17" Cancel="yes">
      <Text>Annuler</Text>
      <Publish Event="SpawnDialog" Value="CancelDlg">1</Publish>
    </Control>
  </Dialog>

  <!-- Séquence d'installation -->
  <InstallUISequence>
    <Show Dialog="WelcomeDlg" Before="ProgressDlg">NOT Installed</Show>
  </InstallUISequence>
</UI>
```

## Actions personnalisées

### Exécuter du code pendant l'installation

```xml
<!-- Action personnalisée pour configurer l'application -->
<CustomAction Id="ConfigureApp"
              FileKey="MonApplication.exe"
              ExeCommand="/configure"
              Execute="deferred"
              Return="check"
              Impersonate="no" />

<!-- Action pour ouvrir une page web après installation -->
<CustomAction Id="LaunchWebsite"
              BinaryKey="WixCA"
              DllEntry="WixShellExec"
              Impersonate="yes" />
<Property Id="WixShellExecTarget" Value="https://monsite.com/merci" />

<!-- Séquencer les actions -->
<InstallExecuteSequence>
  <Custom Action="ConfigureApp" After="InstallFiles">NOT Installed</Custom>
  <Custom Action="LaunchWebsite" After="InstallFinalize">NOT Installed</Custom>
</InstallExecuteSequence>
```

### Action personnalisée en Pascal

Créez une DLL avec FreePascal :

```pascal
library CustomActions;

{$mode objfpc}{$H+}

uses
  Windows, SysUtils, Registry;

const
  ERROR_SUCCESS = 0;
  ERROR_INSTALL_FAILURE = 1603;

// Point d'entrée pour MSI
function ConfigureApplication(hInstall: MSIHANDLE): UINT; stdcall;  
var
  Reg: TRegistry;
  InstallDir: string;
  BufferSize: DWORD;
begin
  Result := ERROR_SUCCESS;

  try
    // Récupérer le répertoire d'installation depuis MSI
    SetLength(InstallDir, MAX_PATH);
    BufferSize := MAX_PATH;
    MsiGetProperty(hInstall, 'INSTALLFOLDER', @InstallDir[1], BufferSize);
    SetLength(InstallDir, BufferSize);

    // Configurer l'application
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey('\Software\MonEntreprise\MonApplication', True) then
      begin
        Reg.WriteString('InstallPath', InstallDir);
        Reg.WriteString('Version', '1.0.0');
        Reg.WriteDateTime('InstallDate', Now);
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;

    // Log dans MSI
    MsiLogMessage(hInstall, 'Configuration terminée avec succès');

  except
    on E: Exception do
    begin
      MsiLogMessage(hInstall, PChar('Erreur : ' + E.Message));
      Result := ERROR_INSTALL_FAILURE;
    end;
  end;
end;

exports
  ConfigureApplication;

begin  
end.
```

Référencer dans WiX :

```xml
<Binary Id="CustomActions.dll" SourceFile="CustomActions.dll" />

<CustomAction Id="ConfigureApp"
              BinaryKey="CustomActions.dll"
              DllEntry="ConfigureApplication"
              Execute="deferred"
              Return="check"
              Impersonate="no" />
```

## Gestion des versions et mises à jour

### Stratégie de versioning

```xml
<!-- Version actuelle -->
<Product Version="1.2.0.0" ... >

<!-- Code de mise à jour constant -->
<Product UpgradeCode="12345678-1234-1234-1234-123456789012">

<!-- Gestion des mises à jour -->
<Upgrade Id="12345678-1234-1234-1234-123456789012">
  <!-- Versions antérieures à désinstaller -->
  <UpgradeVersion Minimum="1.0.0.0"
                  Maximum="1.2.0.0"
                  Property="PREVIOUSVERSIONSINSTALLED"
                  IncludeMaximum="no" />

  <!-- Empêcher le downgrade -->
  <UpgradeVersion Minimum="1.2.0.0"
                  OnlyDetect="yes"
                  Property="NEWERVERSIONDETECTED"
                  IncludeMinimum="no" />
</Upgrade>

<!-- Séquence de mise à jour -->
<InstallExecuteSequence>
  <RemoveExistingProducts After="InstallInitialize" />
</InstallExecuteSequence>

<!-- Message d'erreur pour downgrade -->
<Condition Message="Une version plus récente est déjà installée.">
  NOT NEWERVERSIONDETECTED
</Condition>
```

### Patches et mises à jour mineures

Créez `patch.wxs` :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Wix xmlns="http://schemas.microsoft.com/wix/2006/wi">
  <Patch AllowRemoval="yes"
         Manufacturer="Mon Entreprise"
         MoreInfoURL="https://monsite.com"
         DisplayName="Mon Application - Mise à jour 1.2.1"
         Description="Correctifs et améliorations"
         Classification="Update">

    <Media Id="5000" Cabinet="patch.cab">
      <PatchBaseline Id="RTM">
        <Validate ProductId="yes"
                  ProductVersionOperator="LesserOrEqual"
                  ProductVersion="1.2.0.0"
                  ProductLanguage="yes"
                  UpgradeCode="yes" />
      </PatchBaseline>
    </Media>

    <PatchFamilyRef Id="MonAppPatchFamily" />
  </Patch>

  <Fragment>
    <PatchFamily Id="MonAppPatchFamily" Version="1.2.1.0" Supersede="yes">
      <PropertyRef Id="ProductVersion" />
      <ComponentRef Id="MainExecutable" />
    </PatchFamily>
  </Fragment>
</Wix>
```

## Conditions et détection de prérequis

### Vérifier les prérequis

```xml
<!-- Vérifier la version de Windows -->
<Condition Message="Cette application nécessite Windows 7 ou supérieur.">
  <![CDATA[Installed OR (VersionNT >= 601)]]>
</Condition>

<!-- Vérifier .NET Framework -->
<PropertyRef Id="NETFRAMEWORK45" />
<Condition Message="Cette application nécessite .NET Framework 4.5.">
  <![CDATA[Installed OR NETFRAMEWORK45]]>
</Condition>

<!-- Vérifier les droits administrateur -->
<Condition Message="Vous devez avoir les droits administrateur pour installer cette application.">
  <![CDATA[Privileged]]>
</Condition>

<!-- Rechercher un logiciel installé -->
<Property Id="SQLSERVERINSTALLED">
  <RegistrySearch Id="SqlServerSearch"
                  Root="HKLM"
                  Key="SOFTWARE\Microsoft\Microsoft SQL Server"
                  Name="InstalledInstances"
                  Type="raw" />
</Property>

<Condition Message="SQL Server doit être installé.">
  <![CDATA[Installed OR SQLSERVERINSTALLED]]>
</Condition>
```

### Installer des prérequis

```xml
<!-- Bootstrapper avec WiX Burn -->
<Bundle Name="Mon Application Bundle"
        Version="1.0.0.0"
        Manufacturer="Mon Entreprise"
        UpgradeCode="98765432-1234-1234-1234-210987654321">

  <BootstrapperApplicationRef Id="WixStandardBootstrapperApplication.RtfLicense" />

  <!-- Chain d'installation -->
  <Chain>
    <!-- Visual C++ Redistributables -->
    <ExePackage Id="VCRedist"
                SourceFile="vcredist_x64.exe"
                PerMachine="yes"
                Cache="no"
                Vital="yes"
                Compressed="yes"
                InstallCommand="/quiet /norestart"
                DetectCondition="VCREDISTINSTALLED" />

    <!-- .NET Framework -->
    <PackageGroupRef Id="NetFx45Web" />

    <!-- Notre MSI -->
    <MsiPackage Id="MonApplication"
                SourceFile="MonApplication.msi"
                Vital="yes"
                DisplayInternalUI="yes" />
  </Chain>
</Bundle>
```

## Propriétés et variables

### Propriétés système

```xml
<!-- Propriétés prédéfinies utilisables -->
<Property Id="USERNAME" />
<Property Id="COMPANYNAME" />
<Property Id="ALLUSERS" Value="1" />

<!-- Propriétés personnalisées -->
<Property Id="INSTALLDIR_VALID" Value="1" />
<Property Id="DATABASE_SERVER" Value="localhost" />
<Property Id="DATABASE_NAME" Value="MaDB" />

<!-- Propriété depuis le registre -->
<Property Id="OLDVERSION">
  <RegistrySearch Id="OldVersionSearch"
                  Root="HKLM"
                  Key="Software\MonEntreprise\MonApplication"
                  Name="Version"
                  Type="raw" />
</Property>

<!-- Propriété depuis une recherche de fichier -->
<Property Id="FILEEXISTS">
  <DirectorySearch Id="CheckFileDir" Path="[ProgramFilesFolder]">
    <FileSearch Id="CheckFile" Name="important.dll" />
  </DirectorySearch>
</Property>
```

### Utilisation dans l'interface

```xml
<Control Id="ServerEdit" Type="Edit" X="25" Y="100" Width="200" Height="17"
         Property="DATABASE_SERVER">
  <Text>{80}</Text>
</Control>

<Control Id="TestButton" Type="PushButton" X="240" Y="100" Width="56" Height="17">
  <Text>Tester</Text>
  <Publish Property="INSTALLDIR_VALID" Value="1">1</Publish>
  <Publish Event="DoAction" Value="TestConnection">1</Publish>
</Control>
```

## Journalisation et débogage

### Activer la journalisation

```batch
REM Installation avec log détaillé  
msiexec /i MonApplication.msi /l*v install.log

REM Options de log :  
REM   i - Status messages  
REM   w - Non-fatal warnings  
REM   e - Error messages  
REM   a - Start of actions  
REM   r - Action records  
REM   u - User requests  
REM   c - Initial UI parameters  
REM   m - Out-of-memory messages  
REM   o - Out-of-disk-space messages  
REM   p - Terminal properties  
REM   v - Verbose output  
REM   * - All information
```

### Analyser les logs

```pascal
program AnalyseurLogMSI;

uses
  SysUtils, Classes, StrUtils;

type
  TLogAnalyzer = class
  private
    FLogFile: TStringList;
    FErrors: TStringList;
    FWarnings: TStringList;
    FActions: TStringList;

    procedure ParseLine(const Line: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadLog(const FileName: string);
    procedure Analyze;
    procedure ShowReport;
  end;

constructor TLogAnalyzer.Create;  
begin
  FLogFile := TStringList.Create;
  FErrors := TStringList.Create;
  FWarnings := TStringList.Create;
  FActions := TStringList.Create;
end;

destructor TLogAnalyzer.Destroy;  
begin
  FLogFile.Free;
  FErrors.Free;
  FWarnings.Free;
  FActions.Free;
  inherited;
end;

procedure TLogAnalyzer.LoadLog(const FileName: string);  
begin
  if FileExists(FileName) then
    FLogFile.LoadFromFile(FileName)
  else
    raise Exception.Create('Fichier log non trouvé : ' + FileName);
end;

procedure TLogAnalyzer.ParseLine(const Line: string);  
begin
  // Erreurs
  if ContainsText(Line, 'Error') or ContainsText(Line, 'Failed') then
    FErrors.Add(Line)

  // Avertissements
  else if ContainsText(Line, 'Warning') then
    FWarnings.Add(Line)

  // Actions
  else if ContainsText(Line, 'Action start') or ContainsText(Line, 'Action ended') then
    FActions.Add(Line);
end;

procedure TLogAnalyzer.Analyze;  
var
  i: Integer;
  ActionStart, ActionEnd: TDateTime;
  InAction: Boolean;
  CurrentAction: string;
begin
  InAction := False;

  for i := 0 to FLogFile.Count - 1 do
  begin
    ParseLine(FLogFile[i]);

    // Analyser le temps d'exécution des actions
    if Pos('Action start', FLogFile[i]) > 0 then
    begin
      InAction := True;
      // Extraire l'heure et le nom de l'action
      CurrentAction := FLogFile[i];
    end
    else if InAction and (Pos('Action ended', FLogFile[i]) > 0) then
    begin
      // Calculer la durée
      InAction := False;
    end;
  end;
end;

procedure TLogAnalyzer.ShowReport;  
var
  i: Integer;
begin
  WriteLn('=== RAPPORT D''ANALYSE DU LOG MSI ===');
  WriteLn;

  WriteLn('Résumé :');
  WriteLn('  Lignes totales : ', FLogFile.Count);
  WriteLn('  Erreurs : ', FErrors.Count);
  WriteLn('  Avertissements : ', FWarnings.Count);
  WriteLn('  Actions : ', FActions.Count);
  WriteLn;

  if FErrors.Count > 0 then
  begin
    WriteLn('ERREURS DÉTECTÉES :');
    for i := 0 to Min(10, FErrors.Count - 1) do
      WriteLn('  - ', FErrors[i]);
    if FErrors.Count > 10 then
      WriteLn('  ... et ', FErrors.Count - 10, ' autres');
    WriteLn;
  end;

  if FWarnings.Count > 0 then
  begin
    WriteLn('AVERTISSEMENTS :');
    for i := 0 to Min(5, FWarnings.Count - 1) do
      WriteLn('  - ', FWarnings[i]);
    WriteLn;
  end;
end;

// Programme principal
var
  Analyzer: TLogAnalyzer;
begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: AnalyseurLogMSI.exe <fichier.log>');
    Exit;
  end;

  Analyzer := TLogAnalyzer.Create;
  try
    Analyzer.LoadLog(ParamStr(1));
    Analyzer.Analyze;
    Analyzer.ShowReport;
  finally
    Analyzer.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
end.
```

## Installation silencieuse et automatisation

### Paramètres d'installation silencieuse

```batch
REM Installation silencieuse de base  
msiexec /i MonApplication.msi /qn

REM Avec propriétés personnalisées  
msiexec /i MonApplication.msi /qn INSTALLDIR="C:\MonApp" DATABASE_SERVER="192.168.1.100"

REM Avec interface réduite  
msiexec /i MonApplication.msi /qb

REM Désinstallation silencieuse  
msiexec /x MonApplication.msi /qn

REM Réparation silencieuse  
msiexec /f MonApplication.msi /qn
```

### Script PowerShell d'installation

```powershell
# InstallApp.ps1
param(
    [string]$MsiPath = "MonApplication.msi",
    [string]$InstallDir = "C:\Program Files\MonApplication",
    [string]$LogPath = "install.log",
    [switch]$Silent = $false
)

function Test-Administrator {
    $currentUser = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($currentUser)
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

function Install-Application {
    Write-Host "Installation de l'application..." -ForegroundColor Green

    $arguments = @(
        "/i"
        "`"$MsiPath`""
        "INSTALLDIR=`"$InstallDir`""
        "/l*v"
        "`"$LogPath`""
    )

    if ($Silent) {
        $arguments += "/qn"
    } else {
        $arguments += "/qb"
    }

    $process = Start-Process -FilePath "msiexec.exe" -ArgumentList $arguments -Wait -PassThru

    return $process.ExitCode
}

# Vérifier les droits admin
if (-not (Test-Administrator)) {
    Write-Host "Ce script doit être exécuté en tant qu'administrateur" -ForegroundColor Red
    exit 1
}

# Vérifier que le MSI existe
if (-not (Test-Path $MsiPath)) {
    Write-Host "Fichier MSI non trouvé : $MsiPath" -ForegroundColor Red
    exit 1
}

# Installer
$exitCode = Install-Application

# Vérifier le résultat
switch ($exitCode) {
    0 { Write-Host "Installation réussie" -ForegroundColor Green }
    1603 { Write-Host "Erreur fatale lors de l'installation" -ForegroundColor Red }
    1619 { Write-Host "Le package MSI n'a pas pu être ouvert" -ForegroundColor Red }
    1638 { Write-Host "Une autre version est déjà installée" -ForegroundColor Yellow }
    3010 { Write-Host "Installation réussie, redémarrage requis" -ForegroundColor Yellow }
    default { Write-Host "Code de sortie : $exitCode" -ForegroundColor Yellow }
}

# Afficher le log si erreur
if ($exitCode -ne 0 -and $exitCode -ne 3010) {
    Write-Host "`nDernières lignes du log :" -ForegroundColor Yellow
    Get-Content $LogPath -Tail 20
}

exit $exitCode
```

## Intégration avec FreePascal/Lazarus

### Générer automatiquement le fichier WiX

```pascal
unit WiXGenerator;

interface

uses
  Classes, SysUtils, DOM, XMLWrite;

type
  TWiXGenerator = class
  private
    FProductName: string;
    FProductVersion: string;
    FManufacturer: string;
    FUpgradeCode: string;
    FFiles: TStringList;

    function GenerateGUID: string;
    function CreateProductElement(Doc: TXMLDocument): TDOMElement;
    function CreateDirectoryStructure(Doc: TXMLDocument): TDOMElement;
    function CreateComponentGroup(Doc: TXMLDocument): TDOMElement;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetProductInfo(const Name, Version, Manufacturer: string);
    procedure AddFile(const FileName, SourcePath: string);
    procedure AddDirectory(const DirName: string);
    function GenerateWiX(const OutputFile: string): Boolean;

    property ProductName: string read FProductName write FProductName;
    property ProductVersion: string read FProductVersion write FProductVersion;
    property Manufacturer: string read FManufacturer write FManufacturer;
    property UpgradeCode: string read FUpgradeCode write FUpgradeCode;
  end;

implementation

uses
  Windows, ActiveX;

constructor TWiXGenerator.Create;  
begin
  FFiles := TStringList.Create;
  FUpgradeCode := GenerateGUID;
end;

destructor TWiXGenerator.Destroy;  
begin
  FFiles.Free;
  inherited;
end;

function TWiXGenerator.GenerateGUID: string;  
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := GUIDToString(Guid);
end;

procedure TWiXGenerator.SetProductInfo(const Name, Version, Manufacturer: string);  
begin
  FProductName := Name;
  FProductVersion := Version;
  FManufacturer := Manufacturer;
end;

procedure TWiXGenerator.AddFile(const FileName, SourcePath: string);  
begin
  FFiles.Add(FileName + '=' + SourcePath);
end;

procedure TWiXGenerator.AddDirectory(const DirName: string);  
begin
  FFiles.Add('DIR:' + DirName);
end;

function TWiXGenerator.CreateProductElement(Doc: TXMLDocument): TDOMElement;  
var
  Product, Package, MajorUpgrade, MediaTemplate: TDOMElement;
begin
  // Créer l'élément Product
  Product := Doc.CreateElement('Product');
  Product.SetAttribute('Id', '*');
  Product.SetAttribute('Name', FProductName);
  Product.SetAttribute('Language', '1036'); // Français
  Product.SetAttribute('Version', FProductVersion);
  Product.SetAttribute('Manufacturer', FManufacturer);
  Product.SetAttribute('UpgradeCode', FUpgradeCode);

  // Package
  Package := Doc.CreateElement('Package');
  Package.SetAttribute('InstallerVersion', '200');
  Package.SetAttribute('Compressed', 'yes');
  Package.SetAttribute('InstallScope', 'perMachine');
  Package.SetAttribute('Description', 'Installation de ' + FProductName);
  Package.SetAttribute('Manufacturer', FManufacturer);
  Product.AppendChild(Package);

  // MajorUpgrade
  MajorUpgrade := Doc.CreateElement('MajorUpgrade');
  MajorUpgrade.SetAttribute('DowngradeErrorMessage',
    'Une version plus récente est déjà installée.');
  Product.AppendChild(MajorUpgrade);

  // MediaTemplate
  MediaTemplate := Doc.CreateElement('MediaTemplate');
  MediaTemplate.SetAttribute('EmbedCab', 'yes');
  Product.AppendChild(MediaTemplate);

  Result := Product;
end;

function TWiXGenerator.CreateDirectoryStructure(Doc: TXMLDocument): TDOMElement;  
var
  Fragment, Directory, SubDir1, SubDir2, InstallFolder: TDOMElement;
begin
  Fragment := Doc.CreateElement('Fragment');

  // Structure de base
  Directory := Doc.CreateElement('Directory');
  Directory.SetAttribute('Id', 'TARGETDIR');
  Directory.SetAttribute('Name', 'SourceDir');

  // Program Files
  SubDir1 := Doc.CreateElement('Directory');
  SubDir1.SetAttribute('Id', 'ProgramFilesFolder');

  // Dossier de l'entreprise
  SubDir2 := Doc.CreateElement('Directory');
  SubDir2.SetAttribute('Id', 'CompanyFolder');
  SubDir2.SetAttribute('Name', StringReplace(FManufacturer, ' ', '', [rfReplaceAll]));

  // Dossier de l'application
  InstallFolder := Doc.CreateElement('Directory');
  InstallFolder.SetAttribute('Id', 'INSTALLFOLDER');
  InstallFolder.SetAttribute('Name', StringReplace(FProductName, ' ', '', [rfReplaceAll]));

  SubDir2.AppendChild(InstallFolder);
  SubDir1.AppendChild(SubDir2);
  Directory.AppendChild(SubDir1);

  // Menu Démarrer
  SubDir1 := Doc.CreateElement('Directory');
  SubDir1.SetAttribute('Id', 'ProgramMenuFolder');

  SubDir2 := Doc.CreateElement('Directory');
  SubDir2.SetAttribute('Id', 'ApplicationProgramsFolder');
  SubDir2.SetAttribute('Name', FProductName);

  SubDir1.AppendChild(SubDir2);
  Directory.AppendChild(SubDir1);

  Fragment.AppendChild(Directory);
  Result := Fragment;
end;

function TWiXGenerator.CreateComponentGroup(Doc: TXMLDocument): TDOMElement;  
var
  Fragment, ComponentGroup, Component, FileElem: TDOMElement;
  i: Integer;
  FileName, SourcePath: string;
  Parts: TStringList;
begin
  Fragment := Doc.CreateElement('Fragment');

  ComponentGroup := Doc.CreateElement('ComponentGroup');
  ComponentGroup.SetAttribute('Id', 'ProductComponents');
  ComponentGroup.SetAttribute('Directory', 'INSTALLFOLDER');

  Parts := TStringList.Create;
  try
    for i := 0 to FFiles.Count - 1 do
    begin
      if Pos('DIR:', FFiles[i]) = 1 then
        Continue; // Ignorer les répertoires pour l'instant

      Parts.Clear;
      Parts.Delimiter := '=';
      Parts.DelimitedText := FFiles[i];

      if Parts.Count = 2 then
      begin
        FileName := Parts[0];
        SourcePath := Parts[1];

        // Créer le composant
        Component := Doc.CreateElement('Component');
        Component.SetAttribute('Id', 'Component_' +
          StringReplace(FileName, '.', '_', [rfReplaceAll]));
        Component.SetAttribute('Guid', GenerateGUID);

        // Créer l'élément File
        FileElem := Doc.CreateElement('File');
        FileElem.SetAttribute('Id', 'File_' +
          StringReplace(FileName, '.', '_', [rfReplaceAll]));
        FileElem.SetAttribute('Name', FileName);
        FileElem.SetAttribute('Source', SourcePath);
        FileElem.SetAttribute('KeyPath', 'yes');

        Component.AppendChild(FileElem);
        ComponentGroup.AppendChild(Component);
      end;
    end;
  finally
    Parts.Free;
  end;

  Fragment.AppendChild(ComponentGroup);
  Result := Fragment;
end;

function TWiXGenerator.GenerateWiX(const OutputFile: string): Boolean;  
var
  Doc: TXMLDocument;
  Root, Product, Feature, ComponentGroupRef: TDOMElement;
begin
  Result := False;

  try
    Doc := TXMLDocument.Create;
    try
      // Créer la racine Wix
      Root := Doc.CreateElement('Wix');
      Root.SetAttribute('xmlns', 'http://schemas.microsoft.com/wix/2006/wi');
      Doc.AppendChild(Root);

      // Créer Product
      Product := CreateProductElement(Doc);

      // Créer Feature
      Feature := Doc.CreateElement('Feature');
      Feature.SetAttribute('Id', 'ProductFeature');
      Feature.SetAttribute('Title', FProductName);
      Feature.SetAttribute('Level', '1');

      ComponentGroupRef := Doc.CreateElement('ComponentGroupRef');
      ComponentGroupRef.SetAttribute('Id', 'ProductComponents');
      Feature.AppendChild(ComponentGroupRef);

      Product.AppendChild(Feature);
      Root.AppendChild(Product);

      // Ajouter la structure de répertoires
      Root.AppendChild(CreateDirectoryStructure(Doc));

      // Ajouter les composants
      Root.AppendChild(CreateComponentGroup(Doc));

      // Sauvegarder
      WriteXMLFile(Doc, OutputFile);
      Result := True;

    finally
      Doc.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur génération WiX : ', E.Message);
  end;
end;
```

### Créer un assistant d'installation dans Lazarus

```pascal
unit InstallWizard;

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls,
  Buttons, Dialogs;

type
  TInstallWizardForm = class(TForm)
    PageControl: TPageControl;
    TabWelcome: TTabSheet;
    TabLicense: TTabSheet;
    TabDirectory: TTabSheet;
    TabComponents: TTabSheet;
    TabProgress: TTabSheet;
    TabFinish: TTabSheet;

    BtnBack: TBitBtn;
    BtnNext: TBitBtn;
    BtnCancel: TBitBtn;

    // Page Bienvenue
    LabelWelcome: TLabel;
    MemoWelcome: TMemo;
    ImageLogo: TImage;

    // Page Licence
    MemoLicense: TMemo;
    CheckAcceptLicense: TCheckBox;

    // Page Répertoire
    EditInstallDir: TEdit;
    BtnBrowse: TButton;
    LabelSpaceRequired: TLabel;
    LabelSpaceAvailable: TLabel;

    // Page Composants
    TreeViewComponents: TTreeView;
    LabelDescription: TLabel;
    MemoDescription: TMemo;

    // Page Progression
    ProgressBar: TProgressBar;
    ListBoxProgress: TListBox;

    // Page Fin
    CheckRunApp: TCheckBox;
    CheckViewReadme: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnBackClick(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure CheckAcceptLicenseClick(Sender: TObject);
    procedure TreeViewComponentsClick(Sender: TObject);

  private
    FCurrentPage: Integer;
    FInstallDir: string;
    FComponents: TStringList;
    FMSIPath: string;

    procedure UpdateButtons;
    procedure ChangePage(NewPage: Integer);
    procedure CalculateSpace;
    function ValidatePage: Boolean;
    procedure StartInstallation;
    procedure RunMSI;
    procedure Log(const Message: string);
  public
    property InstallDir: string read FInstallDir write FInstallDir;
  end;

implementation

uses
  FileUtil, Windows, ShellAPI, Process;

procedure TInstallWizardForm.FormCreate(Sender: TObject);  
var
  Node: TTreeNode;
begin
  FCurrentPage := 0;
  FComponents := TStringList.Create;

  // Initialiser les pages
  PageControl.ActivePageIndex := 0;
  PageControl.ShowTabs := False;

  // Charger la licence
  if FileExists('License.txt') then
    MemoLicense.Lines.LoadFromFile('License.txt');

  // Répertoire par défaut
  FInstallDir := 'C:\Program Files\' + Application.Title;
  EditInstallDir.Text := FInstallDir;

  // Calculer l'espace
  CalculateSpace;

  // Initialiser les composants
  with TreeViewComponents.Items do
  begin
    Clear;
    Node := Add(nil, 'Application principale');
    Node.StateIndex := 1;
    FComponents.AddObject('main', Node);

    Node := Add(nil, 'Documentation');
    Node.StateIndex := 1;
    FComponents.AddObject('docs', Node);

    Node := Add(nil, 'Exemples');
    Node.StateIndex := 0;
    FComponents.AddObject('samples', Node);
  end;

  UpdateButtons;
end;

procedure TInstallWizardForm.UpdateButtons;  
begin
  BtnBack.Enabled := FCurrentPage > 0;

  case FCurrentPage of
    0: // Bienvenue
      begin
        BtnNext.Caption := 'Suivant >';
        BtnNext.Enabled := True;
      end;

    1: // Licence
      begin
        BtnNext.Caption := 'Suivant >';
        BtnNext.Enabled := CheckAcceptLicense.Checked;
      end;

    2: // Répertoire
      begin
        BtnNext.Caption := 'Suivant >';
        BtnNext.Enabled := DirectoryExists(ExtractFilePath(EditInstallDir.Text));
      end;

    3: // Composants
      begin
        BtnNext.Caption := 'Installer';
        BtnNext.Enabled := True;
      end;

    4: // Progression
      begin
        BtnNext.Caption := 'Suivant >';
        BtnNext.Enabled := False;
        BtnBack.Enabled := False;
        BtnCancel.Enabled := False;
      end;

    5: // Fin
      begin
        BtnNext.Caption := 'Terminer';
        BtnNext.Enabled := True;
        BtnBack.Visible := False;
        BtnCancel.Visible := False;
      end;
  end;
end;

procedure TInstallWizardForm.ChangePage(NewPage: Integer);  
begin
  if (NewPage >= 0) and (NewPage < PageControl.PageCount) then
  begin
    FCurrentPage := NewPage;
    PageControl.ActivePageIndex := NewPage;
    UpdateButtons;

    // Actions spécifiques par page
    case NewPage of
      4: StartInstallation; // Lancer l'installation
    end;
  end;
end;

procedure TInstallWizardForm.BtnNextClick(Sender: TObject);  
begin
  if FCurrentPage = 5 then // Page de fin
  begin
    if CheckRunApp.Checked then
      ShellExecute(0, 'open', PChar(FInstallDir + '\MonApp.exe'), nil, nil, SW_SHOW);

    if CheckViewReadme.Checked then
      ShellExecute(0, 'open', PChar(FInstallDir + '\README.txt'), nil, nil, SW_SHOW);

    Close;
  end
  else
  begin
    if ValidatePage then
      ChangePage(FCurrentPage + 1);
  end;
end;

procedure TInstallWizardForm.BtnBackClick(Sender: TObject);  
begin
  ChangePage(FCurrentPage - 1);
end;

procedure TInstallWizardForm.BtnBrowseClick(Sender: TObject);  
var
  Dir: string;
begin
  Dir := EditInstallDir.Text;
  if SelectDirectory('Sélectionner le dossier d''installation', '', Dir) then
  begin
    EditInstallDir.Text := Dir;
    FInstallDir := Dir;
    CalculateSpace;
  end;
end;

procedure TInstallWizardForm.CheckAcceptLicenseClick(Sender: TObject);  
begin
  UpdateButtons;
end;

procedure TInstallWizardForm.TreeViewComponentsClick(Sender: TObject);  
var
  Node: TTreeNode;
begin
  Node := TreeViewComponents.Selected;
  if Node <> nil then
  begin
    // Basculer l'état
    if Node.StateIndex = 0 then
      Node.StateIndex := 1
    else
      Node.StateIndex := 0;

    // Afficher la description
    case Node.Index of
      0: MemoDescription.Text := 'L''application principale et ses dépendances.';
      1: MemoDescription.Text := 'Documentation complète au format PDF et HTML.';
      2: MemoDescription.Text := 'Exemples de code et projets de démonstration.';
    end;
  end;
end;

procedure TInstallWizardForm.CalculateSpace;  
var
  FreeSpace: Int64;
  RequiredSpace: Int64;
begin
  RequiredSpace := 50 * 1024 * 1024; // 50 MB

  FreeSpace := DiskFree(Ord(UpCase(EditInstallDir.Text[1])) - Ord('A') + 1);

  LabelSpaceRequired.Caption := Format('Espace requis : %d MB', [RequiredSpace div (1024*1024)]);
  LabelSpaceAvailable.Caption := Format('Espace disponible : %d MB', [FreeSpace div (1024*1024)]);

  if FreeSpace < RequiredSpace then
    LabelSpaceAvailable.Font.Color := clRed
  else
    LabelSpaceAvailable.Font.Color := clGreen;
end;

function TInstallWizardForm.ValidatePage: Boolean;  
begin
  Result := True;

  case FCurrentPage of
    1: // Licence
      if not CheckAcceptLicense.Checked then
      begin
        ShowMessage('Vous devez accepter la licence pour continuer.');
        Result := False;
      end;

    2: // Répertoire
      begin
        if not DirectoryExists(ExtractFilePath(EditInstallDir.Text)) then
        begin
          ShowMessage('Le répertoire parent n''existe pas.');
          Result := False;
        end;
        FInstallDir := EditInstallDir.Text;
      end;
  end;
end;

procedure TInstallWizardForm.StartInstallation;  
var
  Generator: TWiXGenerator;
  i: Integer;
begin
  Log('Démarrage de l''installation...');
  ProgressBar.Position := 0;

  // Créer le fichier WiX
  Log('Génération du fichier WiX...');
  Generator := TWiXGenerator.Create;
  try
    Generator.SetProductInfo(Application.Title, '1.0.0', 'Mon Entreprise');

    // Ajouter les fichiers selon les composants sélectionnés
    for i := 0 to TreeViewComponents.Items.Count - 1 do
    begin
      if TreeViewComponents.Items[i].StateIndex = 1 then
      begin
        case i of
          0: // Application principale
            begin
              Generator.AddFile('MonApp.exe', ExtractFilePath(ParamStr(0)) + 'MonApp.exe');
              Generator.AddFile('MonApp.dll', ExtractFilePath(ParamStr(0)) + 'MonApp.dll');
            end;
          1: // Documentation
            Generator.AddFile('Manuel.pdf', ExtractFilePath(ParamStr(0)) + 'Manuel.pdf');
          2: // Exemples
            Generator.AddDirectory('Exemples');
        end;
      end;
    end;

    FMSIPath := GetTempDir + 'MonApp.wxs';
    Generator.GenerateWiX(FMSIPath);

  finally
    Generator.Free;
  end;

  ProgressBar.Position := 25;

  // Compiler avec WiX
  Log('Compilation du MSI...');
  RunMSI;
end;

procedure TInstallWizardForm.RunMSI;  
var
  Process: TProcess;
  MSIFile: string;
begin
  // Compiler WiX -> MSI
  Process := TProcess.Create(nil);
  try
    // Candle
    Log('Exécution de candle.exe...');
    Process.Executable := 'candle.exe';
    Process.Parameters.Add(FMSIPath);
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    ProgressBar.Position := 50;

    // Light
    Log('Exécution de light.exe...');
    Process.Parameters.Clear;
    Process.Executable := 'light.exe';
    Process.Parameters.Add(ChangeFileExt(FMSIPath, '.wixobj'));
    Process.Parameters.Add('-o');
    MSIFile := GetTempDir + 'MonApp.msi';
    Process.Parameters.Add(MSIFile);
    Process.Execute;

    ProgressBar.Position := 75;

    // Installer le MSI
    Log('Installation du MSI...');
    Process.Parameters.Clear;
    Process.Executable := 'msiexec.exe';
    Process.Parameters.Add('/i');
    Process.Parameters.Add(MSIFile);
    Process.Parameters.Add('/qb');
    Process.Parameters.Add('INSTALLDIR=' + FInstallDir);
    Process.Execute;

    ProgressBar.Position := 100;
    Log('Installation terminée !');

    // Passer à la page de fin
    Application.ProcessMessages;
    Sleep(1000);
    ChangePage(5);

  finally
    Process.Free;
  end;
end;

procedure TInstallWizardForm.Log(const Message: string);  
begin
  ListBoxProgress.Items.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Message);
  ListBoxProgress.ItemIndex := ListBoxProgress.Count - 1;
  Application.ProcessMessages;
end;
```

## Gestion avancée des composants

### Composants optionnels et dépendances

```xml
<!-- Définir les features avec dépendances -->
<Feature Id="MainApplication"
         Title="Application principale"
         Level="1"
         Absent="disallow"
         Description="L'application principale (requis)">
  <ComponentRef Id="MainExecutable" />
  <ComponentRef Id="CoreLibraries" />
</Feature>

<Feature Id="Documentation"
         Title="Documentation"
         Level="1"
         Description="Manuel utilisateur et documentation technique">
  <ComponentRef Id="UserManual" />
  <ComponentRef Id="TechnicalDocs" />
</Feature>

<Feature Id="Examples"
         Title="Exemples"
         Level="2"
         Description="Exemples de code et projets de démonstration">
  <ComponentRef Id="SampleProjects" />
  <Condition Level="1">INSTALLEXAMPLES="1"</Condition>
</Feature>

<Feature Id="Plugins"
         Title="Plugins"
         Level="1">

  <Feature Id="Plugin_PDF"
           Title="Plugin PDF"
           Level="1">
    <ComponentRef Id="PDFPlugin" />
  </Feature>

  <Feature Id="Plugin_Excel"
           Title="Plugin Excel"
           Level="2">
    <ComponentRef Id="ExcelPlugin" />
    <!-- Dépendance : nécessite Office -->
    <Condition Level="0">NOT OFFICEINSTALLED</Condition>
  </Feature>
</Feature>
```

### Gestion des services Windows

```xml
<!-- Installer un service Windows -->
<Component Id="WindowsService" Guid="...">
  <File Id="MonService.exe" Name="MonService.exe" Source="MonService.exe" KeyPath="yes" />

  <ServiceInstall Id="MonServiceInstall"
                  Type="ownProcess"
                  Name="MonService"
                  DisplayName="Mon Service Application"
                  Description="Service de l'application Mon Application"
                  Start="auto"
                  Account="LocalSystem"
                  ErrorControl="normal" />

  <ServiceControl Id="StartService"
                  Start="install"
                  Stop="both"
                  Remove="uninstall"
                  Name="MonService"
                  Wait="yes" />
</Component>
```

Créer le service en FreePascal :

```pascal
program MonService;

{$mode objfpc}{$H+}

uses
  Windows, SysUtils, Classes, WinSvc;

type
  TMonService = class
  private
    FServiceName: string;
    FServiceStatus: SERVICE_STATUS;
    FServiceStatusHandle: SERVICE_STATUS_HANDLE;
    FStopEvent: THandle;

    procedure SetServiceStatus(State: DWORD);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure Execute;

    procedure Install;
    procedure Uninstall;
  end;

var
  Service: TMonService;
  ServiceTable: array[0..1] of TServiceTableEntry;

function ServiceCtrlHandler(Control: DWORD): DWORD; stdcall;  
begin
  Result := NO_ERROR;

  case Control of
    SERVICE_CONTROL_STOP, SERVICE_CONTROL_SHUTDOWN:
      begin
        Service.SetServiceStatus(SERVICE_STOP_PENDING);
        Service.Stop;
      end;

    SERVICE_CONTROL_PAUSE:
      Service.SetServiceStatus(SERVICE_PAUSED);

    SERVICE_CONTROL_CONTINUE:
      Service.SetServiceStatus(SERVICE_RUNNING);

    SERVICE_CONTROL_INTERROGATE:
      Service.SetServiceStatus(Service.FServiceStatus.dwCurrentState);
  end;
end;

procedure ServiceMain(ArgCount: DWORD; Args: PPChar); stdcall;  
begin
  Service.FServiceStatusHandle := RegisterServiceCtrlHandler(
    PChar(Service.FServiceName), @ServiceCtrlHandler);

  if Service.FServiceStatusHandle <> 0 then
  begin
    Service.SetServiceStatus(SERVICE_START_PENDING);
    Service.Start;
    Service.SetServiceStatus(SERVICE_RUNNING);
    Service.Execute;
  end;
end;

constructor TMonService.Create;  
begin
  FServiceName := 'MonService';
  FStopEvent := CreateEvent(nil, True, False, nil);

  FillChar(FServiceStatus, SizeOf(FServiceStatus), 0);
  FServiceStatus.dwServiceType := SERVICE_WIN32_OWN_PROCESS;
  FServiceStatus.dwCurrentState := SERVICE_START_PENDING;
  FServiceStatus.dwControlsAccepted := SERVICE_ACCEPT_STOP or SERVICE_ACCEPT_SHUTDOWN;
end;

destructor TMonService.Destroy;  
begin
  CloseHandle(FStopEvent);
  inherited;
end;

procedure TMonService.SetServiceStatus(State: DWORD);  
begin
  FServiceStatus.dwCurrentState := State;
  FServiceStatus.dwCheckPoint := 0;
  FServiceStatus.dwWaitHint := 0;

  Windows.SetServiceStatus(FServiceStatusHandle, @FServiceStatus);
end;

procedure TMonService.Start;  
begin
  // Initialisation du service
  // Charger la configuration, ouvrir les connexions, etc.
end;

procedure TMonService.Stop;  
begin
  SetEvent(FStopEvent);
end;

procedure TMonService.Execute;  
begin
  // Boucle principale du service
  while WaitForSingleObject(FStopEvent, 1000) = WAIT_TIMEOUT do
  begin
    // Travail du service
    // ...
  end;

  SetServiceStatus(SERVICE_STOPPED);
end;

procedure TMonService.Install;  
var
  SCManager, Service: SC_HANDLE;
  Path: string;
begin
  Path := ParamStr(0);

  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CREATE_SERVICE);
  if SCManager <> 0 then
  begin
    try
      Service := CreateService(
        SCManager,
        PChar(FServiceName),
        PChar('Mon Service Application'),
        SERVICE_ALL_ACCESS,
        SERVICE_WIN32_OWN_PROCESS,
        SERVICE_AUTO_START,
        SERVICE_ERROR_NORMAL,
        PChar(Path),
        nil, nil, nil, nil, nil);

      if Service <> 0 then
      begin
        WriteLn('Service installé avec succès');
        CloseServiceHandle(Service);
      end
      else
        WriteLn('Erreur installation : ', GetLastError);

    finally
      CloseServiceHandle(SCManager);
    end;
  end;
end;

procedure TMonService.Uninstall;  
var
  SCManager, Service: SC_HANDLE;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager <> 0 then
  begin
    try
      Service := OpenService(SCManager, PChar(FServiceName), SERVICE_ALL_ACCESS);
      if Service <> 0 then
      begin
        try
          if DeleteService(Service) then
            WriteLn('Service désinstallé')
          else
            WriteLn('Erreur désinstallation : ', GetLastError);
        finally
          CloseServiceHandle(Service);
        end;
      end;
    finally
      CloseServiceHandle(SCManager);
    end;
  end;
end;

begin
  Service := TMonService.Create;
  try
    if ParamCount > 0 then
    begin
      if ParamStr(1) = '/install' then
        Service.Install
      else if ParamStr(1) = '/uninstall' then
        Service.Uninstall
      else if ParamStr(1) = '/debug' then
      begin
        Service.Start;
        Service.Execute;
      end;
    end
    else
    begin
      ServiceTable[0].lpServiceName := PChar(Service.FServiceName);
      ServiceTable[0].lpServiceProc := @ServiceMain;
      ServiceTable[1].lpServiceName := nil;
      ServiceTable[1].lpServiceProc := nil;

      // Démarrer le service
      if not StartServiceCtrlDispatcher(ServiceTable[0]) then
        WriteLn('Erreur démarrage service : ', GetLastError);
    end;
  finally
    Service.Free;
  end;
end.
```

## Localisation et multi-langue

### Support multi-langue dans WiX

```xml
<!-- Fichier de localisation français : fr-FR.wxl -->
<?xml version="1.0" encoding="utf-8"?>
<WixLocalization Culture="fr-FR" xmlns="http://schemas.microsoft.com/wix/2006/localization">
  <String Id="ApplicationName">Mon Application</String>
  <String Id="Manufacturer">Mon Entreprise</String>
  <String Id="Description">Une application développée avec FreePascal</String>
  <String Id="Comments">Version française</String>
  <String Id="DowngradeError">Une version plus récente est déjà installée.</String>
  <String Id="Feature_Main">Application principale</String>
  <String Id="Feature_Docs">Documentation</String>
  <String Id="Feature_Examples">Exemples</String>
</WixLocalization>

<!-- Fichier de localisation anglais : en-US.wxl -->
<?xml version="1.0" encoding="utf-8"?>
<WixLocalization Culture="en-US" xmlns="http://schemas.microsoft.com/wix/2006/localization">
  <String Id="ApplicationName">My Application</String>
  <String Id="Manufacturer">My Company</String>
  <String Id="Description">An application developed with FreePascal</String>
  <String Id="Comments">English version</String>
  <String Id="DowngradeError">A newer version is already installed.</String>
  <String Id="Feature_Main">Main Application</String>
  <String Id="Feature_Docs">Documentation</String>
  <String Id="Feature_Examples">Examples</String>
</WixLocalization>
```

Utilisation dans le fichier WiX principal :

```xml
<Product Name="!(loc.ApplicationName)"
         Manufacturer="!(loc.Manufacturer)">

  <Package Description="!(loc.Description)"
           Comments="!(loc.Comments)" />

  <MajorUpgrade DowngradeErrorMessage="!(loc.DowngradeError)" />

  <Feature Id="MainFeature" Title="!(loc.Feature_Main)">
    <!-- ... -->
  </Feature>
</Product>
```

Compilation avec localisation :

```batch
@echo off
REM Compiler pour le français  
candle.exe MonApp.wxs -dLanguage=1036 -out obj\fr\  
light.exe -cultures:fr-FR -loc fr-FR.wxl obj\fr\MonApp.wixobj -out MonApp_FR.msi

REM Compiler pour l'anglais  
candle.exe MonApp.wxs -dLanguage=1033 -out obj\en\  
light.exe -cultures:en-US -loc en-US.wxl obj\en\MonApp.wixobj -out MonApp_EN.msi
```

### Détection automatique de la langue

```pascal
unit LanguageDetection;

interface

uses
  Windows, SysUtils, Registry;

type
  TLanguageInfo = record
    Code: Word;
    Name: string;
    Culture: string;
    MSIFile: string;
  end;

const
  SupportedLanguages: array[0..3] of TLanguageInfo = (
    (Code: 1033; Name: 'English'; Culture: 'en-US'; MSIFile: 'MonApp_EN.msi'),
    (Code: 1036; Name: 'Français'; Culture: 'fr-FR'; MSIFile: 'MonApp_FR.msi'),
    (Code: 1031; Name: 'Deutsch'; Culture: 'de-DE'; MSIFile: 'MonApp_DE.msi'),
    (Code: 1034; Name: 'Español'; Culture: 'es-ES'; MSIFile: 'MonApp_ES.msi')
  );

function GetSystemLanguage: Word;  
function GetMSIForLanguage(LangCode: Word): string;  
function InstallLocalizedMSI: Boolean;

implementation

function GetSystemLanguage: Word;  
begin
  Result := GetUserDefaultUILanguage;

  // Si non trouvé, utiliser la langue système
  if Result = 0 then
    Result := GetSystemDefaultUILanguage;

  // Par défaut : anglais
  if Result = 0 then
    Result := 1033;
end;

function GetMSIForLanguage(LangCode: Word): string;  
var
  i: Integer;
begin
  Result := SupportedLanguages[0].MSIFile; // Par défaut : anglais

  for i := 0 to High(SupportedLanguages) do
  begin
    if SupportedLanguages[i].Code = LangCode then
    begin
      Result := SupportedLanguages[i].MSIFile;
      Break;
    end;
  end;
end;

function InstallLocalizedMSI: Boolean;  
var
  LangCode: Word;
  MSIFile: string;
  Process: TProcess;
begin
  Result := False;

  // Détecter la langue
  LangCode := GetSystemLanguage;
  MSIFile := GetMSIForLanguage(LangCode);

  if not FileExists(MSIFile) then
  begin
    // Essayer avec l'anglais par défaut
    MSIFile := SupportedLanguages[0].MSIFile;
    if not FileExists(MSIFile) then
    begin
      ShowMessage('Aucun fichier d''installation trouvé');
      Exit;
    end;
  end;

  // Lancer l'installation
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'msiexec.exe';
    Process.Parameters.Add('/i');
    Process.Parameters.Add(MSIFile);
    Process.Options := [poWaitOnExit];
    Process.Execute;

    Result := Process.ExitStatus = 0;
  finally
    Process.Free;
  end;
end;

end.
```

## Déploiement en entreprise

### Transform files (.mst) pour personnalisation

Créer un fichier transform pour personnaliser l'installation :

```pascal
unit MSITransform;

interface

uses
  Windows, SysUtils, Classes, ActiveX, ComObj;

type
  TMSITransform = class
  private
    FDatabase: OleVariant;
    FTransformFile: string;

    procedure OpenDatabase(const MSIFile: string);
    procedure CreateTransform;
  public
    constructor Create(const MSIFile, TransformFile: string);
    destructor Destroy; override;

    procedure SetProperty(const Name, Value: string);
    procedure SetFeatureLevel(const FeatureName: string; Level: Integer);
    procedure AddRegistryEntry(Root: Integer; const Key, Name, Value: string);
    procedure GenerateTransform(const OriginalMSI: string);
  end;

implementation

constructor TMSITransform.Create(const MSIFile, TransformFile: string);  
begin
  CoInitialize(nil);
  FTransformFile := TransformFile;
  OpenDatabase(MSIFile);
end;

destructor TMSITransform.Destroy;  
begin
  FDatabase := Unassigned;
  CoUninitialize;
  inherited;
end;

procedure TMSITransform.OpenDatabase(const MSIFile: string);  
var
  Installer: OleVariant;
begin
  Installer := CreateOleObject('WindowsInstaller.Installer');
  FDatabase := Installer.OpenDatabase(MSIFile, 1); // msiOpenDatabaseModeTransact
end;

procedure TMSITransform.SetProperty(const Name, Value: string);  
var
  View, Record_: OleVariant;
  SQL: string;
begin
  // Vérifier si la propriété existe
  SQL := Format('SELECT * FROM Property WHERE Property = ''%s''', [Name]);
  View := FDatabase.OpenView(SQL);
  View.Execute;
  Record_ := View.Fetch;

  if VarIsEmpty(Record_) then
  begin
    // Insérer nouvelle propriété
    SQL := 'INSERT INTO Property (Property, Value) VALUES (?, ?)';
    View := FDatabase.OpenView(SQL);
    Record_ := FDatabase.CreateRecord(2);
    Record_.StringData[1] := Name;
    Record_.StringData[2] := Value;
    View.Execute(Record_);
  end
  else
  begin
    // Mettre à jour propriété existante
    SQL := Format('UPDATE Property SET Value = ''%s'' WHERE Property = ''%s''',
                  [Value, Name]);
    View := FDatabase.OpenView(SQL);
    View.Execute;
  end;
end;

procedure TMSITransform.SetFeatureLevel(const FeatureName: string; Level: Integer);  
var
  View: OleVariant;
  SQL: string;
begin
  SQL := Format('UPDATE Feature SET Level = %d WHERE Feature = ''%s''',
                [Level, FeatureName]);
  View := FDatabase.OpenView(SQL);
  View.Execute;
end;

procedure TMSITransform.AddRegistryEntry(Root: Integer; const Key, Name, Value: string);  
var
  View, Record_: OleVariant;
  SQL: string;
  RootStr: string;
begin
  case Root of
    0: RootStr := 'HKCR';
    1: RootStr := 'HKCU';
    2: RootStr := 'HKLM';
    3: RootStr := 'HKU';
    else RootStr := 'HKLM';
  end;

  SQL := 'INSERT INTO Registry (Registry, Root, Key, Name, Value, Component_) ' +
         'VALUES (?, ?, ?, ?, ?, ?)';
  View := FDatabase.OpenView(SQL);
  Record_ := FDatabase.CreateRecord(6);

  Record_.StringData[1] := 'Reg' + FormatDateTime('hhnnss', Now);
  Record_.IntegerData[2] := Root;
  Record_.StringData[3] := Key;
  Record_.StringData[4] := Name;
  Record_.StringData[5] := Value;
  Record_.StringData[6] := 'MainExecutable'; // Component existant

  View.Execute(Record_);
end;

procedure TMSITransform.GenerateTransform(const OriginalMSI: string);  
begin
  FDatabase.GenerateTransform(OriginalMSI, FTransformFile);
  FDatabase.CreateTransformSummaryInfo(OriginalMSI, FTransformFile, 0, 0);
end;

// Utilisation
procedure CreerTransformEntreprise;  
var
  Transform: TMSITransform;
begin
  Transform := TMSITransform.Create('MonApp_Modified.msi', 'Entreprise.mst');
  try
    // Personnaliser les propriétés
    Transform.SetProperty('INSTALLDIR', 'C:\Apps\MonEntreprise\MonApp');
    Transform.SetProperty('COMPANYNAME', 'Mon Entreprise SA');
    Transform.SetProperty('ALLUSERS', '1');

    // Désactiver les exemples
    Transform.SetFeatureLevel('Examples', 0);

    // Ajouter clé de registre entreprise
    Transform.AddRegistryEntry(2, 'SOFTWARE\MonEntreprise\MonApp',
                              'DeploymentType', 'Enterprise');

    // Générer le fichier transform
    Transform.GenerateTransform('MonApp.msi');

    WriteLn('Transform créé : Entreprise.mst');
  finally
    Transform.Free;
  end;
end;

end.
```

### Déploiement via Group Policy

Script pour préparer le déploiement GPO :

```pascal
program PrepareGPODeployment;

uses
  SysUtils, Classes, Windows, Registry;

procedure CreateDeploymentPackage(const MSIPath, TargetFolder: string);  
var
  AdminFolder: string;
  Files: TStringList;
begin
  AdminFolder := TargetFolder + '\MonApplication_GPO\';
  ForceDirectories(AdminFolder);

  // Copier le MSI
  CopyFile(PChar(MSIPath), PChar(AdminFolder + 'MonApplication.msi'), False);

  // Créer le fichier ZAP pour publication
  Files := TStringList.Create;
  try
    Files.Add('[Application]');
    Files.Add('FriendlyName=Mon Application');
    Files.Add('SetupCommand=\\serveur\partage\MonApplication_GPO\MonApplication.msi');
    Files.Add('DisplayVersion=1.0.0');
    Files.Add('Publisher=Mon Entreprise');
    Files.Add('URL=https://monentreprise.com');

    Files.SaveToFile(AdminFolder + 'MonApplication.zap');
  finally
    Files.Free;
  end;

  WriteLn('Package de déploiement créé dans : ', AdminFolder);
  WriteLn;
  WriteLn('Instructions pour l''administrateur :');
  WriteLn('1. Copier le dossier sur un partage réseau accessible');
  WriteLn('2. Dans la console GPO, créer une nouvelle stratégie');
  WriteLn('3. Configuration ordinateur > Stratégies > Paramètres logiciels');
  WriteLn('4. Installation de logiciels > Nouveau > Package');
  WriteLn('5. Sélectionner MonApplication.msi');
  WriteLn('6. Choisir "Attribué" pour installation automatique');
end;

begin
  if ParamCount < 2 then
  begin
    WriteLn('Usage: PrepareGPO.exe <MSI> <DossierCible>');
    Exit;
  end;

  CreateDeploymentPackage(ParamStr(1), ParamStr(2));
end.
```

## Outils de diagnostic et maintenance

### Vérificateur d'installation MSI

```pascal
program MSIChecker;

uses
  Windows, SysUtils, Classes, Registry, ComObj, ActiveX;

type
  TMSIChecker = class
  private
    FProductCode: string;
    FProductName: string;
    FVersion: string;
    FInstallLocation: string;
    FIsInstalled: Boolean;

    procedure CheckRegistry;
    procedure CheckFiles;
    procedure CheckServices;
  public
    constructor Create(const ProductCode: string);

    function RunDiagnostics: Boolean;
    procedure RepairInstallation;
    procedure ShowReport;
  end;

constructor TMSIChecker.Create(const ProductCode: string);  
begin
  FProductCode := ProductCode;
  FIsInstalled := False;
end;

procedure TMSIChecker.CheckRegistry;  
var
  Reg: TRegistry;
  Key: string;
begin
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Vérifier l'installation
    Key := 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\' + FProductCode;
    if Reg.OpenKeyReadOnly(Key) then
    begin
      FIsInstalled := True;
      FProductName := Reg.ReadString('DisplayName');
      FVersion := Reg.ReadString('DisplayVersion');
      FInstallLocation := Reg.ReadString('InstallLocation');
      Reg.CloseKey;
    end;

    // Vérifier les clés application
    Key := 'SOFTWARE\MonEntreprise\MonApplication';
    if not Reg.KeyExists(Key) then
      WriteLn('⚠ Clés de registre application manquantes');

  finally
    Reg.Free;
  end;
end;

procedure TMSIChecker.CheckFiles;  
var
  RequiredFiles: TStringList;
  i: Integer;
  FilePath: string;
begin
  if FInstallLocation = '' then
    Exit;

  RequiredFiles := TStringList.Create;
  try
    RequiredFiles.Add('MonApplication.exe');
    RequiredFiles.Add('MonApplication.dll');
    RequiredFiles.Add('config.ini');

    for i := 0 to RequiredFiles.Count - 1 do
    begin
      FilePath := FInstallLocation + '\' + RequiredFiles[i];
      if not FileExists(FilePath) then
        WriteLn('❌ Fichier manquant : ', RequiredFiles[i])
      else
        WriteLn('✓ Fichier présent : ', RequiredFiles[i]);
    end;

  finally
    RequiredFiles.Free;
  end;
end;

procedure TMSIChecker.CheckServices;  
var
  SCManager, Service: SC_HANDLE;
  ServiceStatus: SERVICE_STATUS;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCManager <> 0 then
  begin
    try
      Service := OpenService(SCManager, 'MonService', SERVICE_QUERY_STATUS);
      if Service <> 0 then
      begin
        try
          if QueryServiceStatus(Service, ServiceStatus) then
          begin
            case ServiceStatus.dwCurrentState of
              SERVICE_RUNNING: WriteLn('✓ Service en cours d''exécution');
              SERVICE_STOPPED: WriteLn('⚠ Service arrêté');
              SERVICE_PAUSED: WriteLn('⚠ Service en pause');
              else WriteLn('⚠ État du service inconnu');
            end;
          end;
        finally
          CloseServiceHandle(Service);
        end;
      end
      else
        WriteLn('❌ Service non installé');
    finally
      CloseServiceHandle(SCManager);
    end;
  end;
end;

function TMSIChecker.RunDiagnostics: Boolean;  
begin
  WriteLn('=== DIAGNOSTIC DE L''INSTALLATION ===');
  WriteLn;

  CheckRegistry;

  if FIsInstalled then
  begin
    WriteLn('✓ Produit installé : ', FProductName);
    WriteLn('  Version : ', FVersion);
    WriteLn('  Emplacement : ', FInstallLocation);
    WriteLn;

    WriteLn('Vérification des fichiers :');
    CheckFiles;
    WriteLn;

    WriteLn('Vérification des services :');
    CheckServices;
    WriteLn;

    Result := True;
  end
  else
  begin
    WriteLn('❌ Produit non installé');
    Result := False;
  end;
end;

procedure TMSIChecker.RepairInstallation;  
var
  Process: TProcess;
begin
  if not FIsInstalled then
  begin
    WriteLn('Le produit n''est pas installé');
    Exit;
  end;

  WriteLn('Lancement de la réparation...');

  Process := TProcess.Create(nil);
  try
    Process.Executable := 'msiexec.exe';
    Process.Parameters.Add('/f');
    Process.Parameters.Add(FProductCode);
    Process.Options := [poWaitOnExit];
    Process.Execute;

    if Process.ExitStatus = 0 then
      WriteLn('✓ Réparation terminée avec succès')
    else
      WriteLn('❌ Échec de la réparation');

  finally
    Process.Free;
  end;
end;

procedure TMSIChecker.ShowReport;  
var
  ReportFile: TextFile;
  FileName: string;
begin
  FileName := 'MSI_Diagnostic_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.txt';

  AssignFile(ReportFile, FileName);
  Rewrite(ReportFile);

  WriteLn(ReportFile, 'RAPPORT DE DIAGNOSTIC MSI');
  WriteLn(ReportFile, '========================');
  WriteLn(ReportFile);
  WriteLn(ReportFile, 'Date : ', DateTimeToStr(Now));
  WriteLn(ReportFile, 'Product Code : ', FProductCode);
  WriteLn(ReportFile, 'Product Name : ', FProductName);
  WriteLn(ReportFile, 'Version : ', FVersion);
  WriteLn(ReportFile, 'Install Location : ', FInstallLocation);
  WriteLn(ReportFile, 'Is Installed : ', FIsInstalled);

  CloseFile(ReportFile);

  WriteLn('Rapport sauvegardé : ', FileName);
end;

// Programme principal
var
  Checker: TMSIChecker;
  Choice: string;
begin
  CoInitialize(nil);
  try
    // Utiliser le ProductCode de votre application
    Checker := TMSIChecker.Create('{12345678-1234-1234-1234-123456789012}');
    try
      if Checker.RunDiagnostics then
      begin
        Write('Voulez-vous réparer l''installation ? (O/N) : ');
        ReadLn(Choice);

        if UpperCase(Choice) = 'O' then
          Checker.RepairInstallation;
      end;

      Checker.ShowReport;

    finally
      Checker.Free;
    end;
  finally
    CoUninitialize;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour terminer...');
  ReadLn;
end.
```

## Bonnes pratiques et recommandations

### Checklist de création MSI

**Avant de créer le MSI :**
- [ ] Définir le ProductCode unique
- [ ] Définir l'UpgradeCode (constant pour toutes les versions)
- [ ] Préparer tous les fichiers nécessaires
- [ ] Créer les icônes et images
- [ ] Rédiger la licence
- [ ] Définir la structure des features
- [ ] Planifier les prérequis

**Pendant la création :**
- [ ] Utiliser des composants stables (GUIDs constants)
- [ ] Définir les KeyPath correctement
- [ ] Gérer les versions correctement
- [ ] Inclure la désinstallation propre
- [ ] Ajouter les raccourcis nécessaires
- [ ] Configurer les services si nécessaire
- [ ] Implémenter les actions personnalisées

**Après la création :**
- [ ] Tester l'installation sur système propre
- [ ] Tester la désinstallation complète
- [ ] Tester la mise à jour depuis version antérieure
- [ ] Vérifier les logs d'installation
- [ ] Tester l'installation silencieuse
- [ ] Signer le MSI avec Authenticode
- [ ] Documenter les paramètres d'installation

### Erreurs courantes à éviter

1. **Ne jamais changer l'UpgradeCode** entre versions
2. **Toujours incrémenter la version** pour les mises à jour
3. **Ne pas modifier les Component GUIDs** pour les mêmes fichiers
4. **Éviter les chemins absolus** dans les sources
5. **Tester sur différentes versions de Windows**
6. **Gérer correctement les droits** (UAC, perMachine vs perUser)
7. **Nettoyer complètement** lors de la désinstallation

### Structure de projet recommandée

```
MonProjet/
├── src/                    # Code source FreePascal
│   ├── MonApp.lpr
│   └── ...
├── bin/                    # Fichiers compilés
│   ├── MonApp.exe
│   └── *.dll
├── installer/              # Fichiers d'installation
│   ├── wix/               # Fichiers WiX
│   │   ├── Product.wxs
│   │   ├── Components.wxs
│   │   └── UI.wxs
│   ├── resources/         # Ressources installer
│   │   ├── banner.bmp
│   │   ├── dialog.bmp
│   │   └── license.rtf
│   ├── localization/      # Fichiers de langue
│   │   ├── en-US.wxl
│   │   └── fr-FR.wxl
│   └── build.bat          # Script de compilation
├── tools/                  # Outils
│   └── MSIChecker.exe
└── docs/                   # Documentation
    └── INSTALL.md
```

## Conclusion

Windows Installer (MSI) est la technologie standard pour déployer des applications Windows professionnelles. Avec FreePascal/Lazarus, vous pouvez créer des installateurs MSI robustes en utilisant WiX Toolset et en développant vos propres outils.

**Points clés à retenir :**

1. **MSI est déclaratif** : Vous décrivez ce qui doit être installé, Windows s'occupe du comment
2. **Utilisez WiX** pour créer des MSI professionnels
3. **Automatisez** la génération des fichiers WiX depuis votre application
4. **Testez minutieusement** chaque aspect de l'installation
5. **Gérez les versions** correctement pour les mises à jour
6. **Personnalisez** selon les besoins (UI, actions, services)
7. **Documentez** le processus pour votre équipe

**Avantages du format MSI :**
- Installation/désinstallation propre et complète
- Support natif de Windows
- Déploiement en entreprise facilité
- Réparation automatique
- Gestion des mises à jour
- Installation silencieuse standardisée

**Pour aller plus loin :**
- Explorez les fonctionnalités avancées de WiX (Burn, Custom Actions)
- Intégrez la création MSI dans votre pipeline CI/CD
- Créez des transforms pour différents clients
- Développez des outils de diagnostic personnalisés
- Envisagez MSIX pour les déploiements Windows 10/11 modernes

Avec ces connaissances, vous êtes maintenant capable de créer des installateurs Windows professionnels pour vos applications FreePascal/Lazarus, offrant une expérience d'installation moderne et fiable à vos utilisateurs.

⏭️ [PowerShell et scripts système](/06-specificites-windows/09-powershell-scripts-systeme.md)
