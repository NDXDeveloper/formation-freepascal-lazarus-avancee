🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Signature Authenticode avec FreePascal/Lazarus sous Windows

## Introduction : Qu'est-ce qu'Authenticode ?

### Comprendre la signature numérique

**Authenticode** est la technologie de Microsoft pour signer numériquement les fichiers exécutables et les scripts. C'est comme apposer un sceau de cire sur une lettre : cela garantit que le fichier vient bien de vous et qu'il n'a pas été modifié depuis.

### Pourquoi signer vos applications ?

**Sans signature**, Windows affiche des avertissements effrayants :
- "Windows a protégé votre ordinateur" (SmartScreen)
- "Éditeur inconnu" dans l'UAC
- Les antivirus sont plus méfiants
- Les entreprises peuvent bloquer l'exécution

**Avec signature**, vous obtenez :
- La confiance des utilisateurs (votre nom apparaît)
- Moins d'avertissements de sécurité
- Une meilleure réputation auprès des antivirus
- La possibilité de distribuer en entreprise
- La preuve que le fichier n'a pas été modifié

### Types de certificats

Il existe plusieurs types de certificats pour la signature de code :

1. **Certificat standard** : Affiche votre nom/entreprise
2. **Certificat EV (Extended Validation)** : Validation renforcée, réputation immédiate
3. **Certificat auto-signé** : Pour tests uniquement, non reconnu publiquement

## Concepts fondamentaux

### La chaîne de confiance

```
Autorité racine (Microsoft Trusted Root)
    ↓
Autorité intermédiaire (CA)
    ↓
Votre certificat
    ↓
Votre application signée
```

### Les composants d'une signature

1. **Le hash** : Empreinte unique du fichier
2. **Le certificat** : Votre identité vérifiée
3. **Le timestamp** : Horodatage pour validité à long terme
4. **La signature** : Le hash chiffré avec votre clé privée

### Formats de certificats

- **.pfx/.p12** : Contient le certificat ET la clé privée (protégé par mot de passe)
- **.cer/.crt** : Certificat public uniquement
- **.pvk** : Clé privée séparée (ancien format)
- **.spc** : Software Publisher Certificate (ancien format)

## Obtenir un certificat de signature

### Option 1 : Acheter un certificat commercial

Les principaux fournisseurs :
- **DigiCert** : Leader du marché, certificats EV disponibles
- **Sectigo (ex-Comodo)** : Bon rapport qualité-prix
- **GlobalSign** : Service professionnel
- **Certum** : Option économique

Prix indicatifs : 70-500€/an selon le type

### Option 2 : Créer un certificat auto-signé (pour tests)

```powershell
# Dans PowerShell en tant qu'administrateur
$cert = New-SelfSignedCertificate -Type CodeSigningCert `
    -Subject "CN=Mon Entreprise, O=Mon Entreprise, C=FR" `
    -KeySpec Signature `
    -KeyLength 2048 `
    -KeyAlgorithm RSA `
    -HashAlgorithm SHA256 `
    -Provider "Microsoft Enhanced RSA and AES Cryptographic Provider" `
    -CertStoreLocation "Cert:\CurrentUser\My"

# Exporter avec clé privée
$pwd = ConvertTo-SecureString -String "MotDePasse123!" -Force -AsPlainText
Export-PfxCertificate -Cert $cert -FilePath "MonCertificat.pfx" -Password $pwd
```

### Option 3 : Utiliser makecert (méthode classique)

```batch
REM Créer un certificat racine
makecert -r -pe -n "CN=Mon CA Test" -ss CA -sr CurrentUser ^
    -a sha256 -cy authority -sky signature -sv MonCA.pvk MonCA.cer

REM Créer un certificat de signature
makecert -pe -n "CN=Mon Application" -a sha256 -cy end ^
    -sky signature -ic MonCA.cer -iv MonCA.pvk ^
    -sv MonApp.pvk MonApp.cer

REM Convertir en PFX
pvk2pfx -pvk MonApp.pvk -spc MonApp.cer -pfx MonApp.pfx -po MotDePasse123!
```

## Outils de signature

### SignTool (Outil officiel Microsoft)

SignTool fait partie du Windows SDK. Installation :
1. Télécharger le Windows SDK
2. Installer uniquement "Windows SDK Signing Tools for Desktop Apps"
3. Chemin typique : `C:\Program Files (x86)\Windows Kits\10\bin\[version]\x64\signtool.exe`

### Osslsigncode (Alternative open source)

Pour les environnements Linux/cross-compilation :
```bash
# Installation sur Ubuntu
sudo apt-get install osslsigncode

# Utilisation similaire à signtool
osslsigncode sign -pkcs12 certificat.pfx -pass motdepasse \
    -t http://timestamp.digicert.com \
    -in application.exe -out application-signee.exe
```

## Signer votre application FreePascal/Lazarus

### Étape 1 : Préparer l'application

```pascal
program MonApplication;

{$mode objfpc}{$H+}
{$APPTYPE GUI}

// Important : définir les informations de version
{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

uses
  Forms, SysUtils;

begin
  Application.Title := 'Mon Application';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

### Étape 2 : Ajouter les informations de version

Dans Lazarus :
1. **Projet** → **Options du projet**
2. **Informations de version** → Cocher "Inclure les informations de version"
3. Remplir tous les champs (important pour la signature)

Ou créer un fichier `version.rc` :
```rc
1 VERSIONINFO
FILEVERSION 1,0,0,0
PRODUCTVERSION 1,0,0,0
FILEFLAGSMASK 0x3F
FILEFLAGS 0x0
FILEOS 0x4
FILETYPE 0x1
FILESUBTYPE 0x0
BEGIN
    BLOCK "StringFileInfo"
    BEGIN
        BLOCK "040904E4"
        BEGIN
            VALUE "CompanyName", "Mon Entreprise"
            VALUE "FileDescription", "Description de mon application"
            VALUE "FileVersion", "1.0.0.0"
            VALUE "InternalName", "MonApp"
            VALUE "LegalCopyright", "Copyright (C) 2024 Mon Entreprise"
            VALUE "OriginalFilename", "MonApplication.exe"
            VALUE "ProductName", "Mon Application"
            VALUE "ProductVersion", "1.0.0.0"
        END
    END
    BLOCK "VarFileInfo"
    BEGIN
        VALUE "Translation", 0x409, 1252
    END
END
```

Compiler : `windres version.rc version.res`

### Étape 3 : Signature basique

```batch
@echo off
REM Signature simple
signtool sign /f "MonCertificat.pfx" /p "MotDePasse123!" "MonApplication.exe"

REM Vérifier la signature
signtool verify /pa "MonApplication.exe"
```

### Étape 4 : Signature avec timestamp

```batch
@echo off
REM Signature avec horodatage (recommandé)
signtool sign /f "MonCertificat.pfx" /p "MotDePasse123!" ^
    /t http://timestamp.digicert.com ^
    /d "Mon Application" ^
    "MonApplication.exe"

REM Alternatives de serveurs timestamp
REM Sectigo : http://timestamp.sectigo.com
REM GlobalSign : http://timestamp.globalsign.com/scripts/timstamp.dll
```

### Étape 5 : Signature SHA-256 (moderne)

```batch
@echo off
REM Signature SHA-256 avec timestamp RFC3161
signtool sign /f "MonCertificat.pfx" /p "MotDePasse123!" ^
    /fd sha256 ^
    /tr http://timestamp.digicert.com ^
    /td sha256 ^
    /d "Mon Application" ^
    /du "https://monsite.com" ^
    "MonApplication.exe"
```

## Automatiser la signature dans Lazarus

### Script de post-compilation

Créez `sign.bat` dans le dossier du projet :
```batch
@echo off
set SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe"
set CERT_PATH="C:\Certificats\MonCertificat.pfx"
set CERT_PASS=MotDePasse123!
set TIMESTAMP=http://timestamp.digicert.com

echo Signature de %1...
%SIGNTOOL% sign /f %CERT_PATH% /p %CERT_PASS% /t %TIMESTAMP% /d "Mon Application" %1

if %ERRORLEVEL% EQU 0 (
    echo Signature reussie !
    %SIGNTOOL% verify /pa %1
) else (
    echo Erreur lors de la signature !
    exit /b 1
)
```

Dans Lazarus :
1. **Projet** → **Options du projet**
2. **Compilateur** → **Commandes**
3. Dans "Exécuter après" : `sign.bat "$TargetFile()""`

### Intégration avec un script Pascal

```pascal
program SignatureHelper;

uses
  SysUtils, Process;

procedure SignerFichier(const CheminFichier: string);
var
  Process: TProcess;
  SignTool: string;
  CertPath: string;
  CertPass: string;
  Timestamp: string;
begin
  // Configuration
  SignTool := 'C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe';
  CertPath := 'C:\Certificats\MonCertificat.pfx';
  CertPass := 'MotDePasse123!';
  Timestamp := 'http://timestamp.digicert.com';

  if not FileExists(CheminFichier) then
  begin
    WriteLn('Erreur : Fichier non trouvé : ', CheminFichier);
    Exit;
  end;

  Process := TProcess.Create(nil);
  try
    Process.Executable := SignTool;
    Process.Parameters.Add('sign');
    Process.Parameters.Add('/f');
    Process.Parameters.Add(CertPath);
    Process.Parameters.Add('/p');
    Process.Parameters.Add(CertPass);
    Process.Parameters.Add('/t');
    Process.Parameters.Add(Timestamp);
    Process.Parameters.Add('/d');
    Process.Parameters.Add('Mon Application');
    Process.Parameters.Add(CheminFichier);

    Process.Options := [poWaitOnExit, poUsePipes];

    WriteLn('Signature en cours...');
    Process.Execute;

    if Process.ExitStatus = 0 then
      WriteLn('Signature réussie !')
    else
      WriteLn('Erreur lors de la signature. Code : ', Process.ExitStatus);

  finally
    Process.Free;
  end;
end;

begin
  if ParamCount > 0 then
    SignerFichier(ParamStr(1))
  else
    WriteLn('Usage: SignatureHelper.exe <fichier>');
end.
```

## Vérification de la signature

### Vérifier programmatiquement

```pascal
uses
  Windows, SysUtils, JwaWinTrust, JwaWinCrypt;

function VerifierSignature(const CheminFichier: string): Boolean;
var
  FileData: TWinTrustFileInfo;
  WinTrustData: TWinTrustData;
  Status: DWORD;
begin
  Result := False;

  // Initialiser la structure de fichier
  FillChar(FileData, SizeOf(FileData), 0);
  FileData.cbStruct := SizeOf(TWinTrustFileInfo);
  FileData.pcwszFilePath := PWideChar(WideString(CheminFichier));

  // Initialiser WinTrustData
  FillChar(WinTrustData, SizeOf(WinTrustData), 0);
  WinTrustData.cbStruct := SizeOf(TWinTrustData);
  WinTrustData.dwUIChoice := WTD_UI_NONE;
  WinTrustData.fdwRevocationChecks := WTD_REVOKE_NONE;
  WinTrustData.dwUnionChoice := WTD_CHOICE_FILE;
  WinTrustData.pFile := @FileData;
  WinTrustData.dwStateAction := WTD_STATEACTION_VERIFY;
  WinTrustData.dwProvFlags := WTD_SAFER_FLAG;

  // Vérifier la signature
  Status := WinVerifyTrust(0, WINTRUST_ACTION_GENERIC_VERIFY_V2, @WinTrustData);

  case Status of
    ERROR_SUCCESS:
      begin
        Result := True;
        WriteLn('Signature valide');
      end;
    TRUST_E_NOSIGNATURE:
      WriteLn('Fichier non signé');
    TRUST_E_EXPLICIT_DISTRUST:
      WriteLn('Certificat explicitement non fiable');
    TRUST_E_SUBJECT_NOT_TRUSTED:
      WriteLn('Sujet non fiable');
    CRYPT_E_SECURITY_SETTINGS:
      WriteLn('Erreur de paramètres de sécurité');
    else
      WriteLn('Erreur de vérification : ', Status);
  end;

  // Nettoyer
  WinTrustData.dwStateAction := WTD_STATEACTION_CLOSE;
  WinVerifyTrust(0, WINTRUST_ACTION_GENERIC_VERIFY_V2, @WinTrustData);
end;
```

### Obtenir les informations du certificat

```pascal
procedure AfficherInfosCertificat(const CheminFichier: string);
var
  hStore: HCERTSTORE;
  hMsg: HCRYPTMSG;
  pCertContext: PCCERT_CONTEXT;
  dwEncoding, dwContentType, dwFormatType: DWORD;
  SubjectSize: DWORD;
  Subject: string;
begin
  // Ouvrir le fichier signé
  if CryptQueryObject(
    CERT_QUERY_OBJECT_FILE,
    PWideChar(WideString(CheminFichier)),
    CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED_EMBED,
    CERT_QUERY_FORMAT_FLAG_BINARY,
    0,
    @dwEncoding,
    @dwContentType,
    @dwFormatType,
    @hStore,
    @hMsg,
    nil) then
  begin
    try
      // Énumérer les certificats
      pCertContext := CertEnumCertificatesInStore(hStore, nil);
      while pCertContext <> nil do
      begin
        // Obtenir le nom du sujet
        SubjectSize := CertGetNameString(
          pCertContext,
          CERT_NAME_SIMPLE_DISPLAY_TYPE,
          0,
          nil,
          nil,
          0);

        if SubjectSize > 0 then
        begin
          SetLength(Subject, SubjectSize);
          CertGetNameString(
            pCertContext,
            CERT_NAME_SIMPLE_DISPLAY_TYPE,
            0,
            nil,
            @Subject[1],
            SubjectSize);

          WriteLn('Signé par : ', Subject);
        end;

        // Certificat suivant
        pCertContext := CertEnumCertificatesInStore(hStore, pCertContext);
      end;
    finally
      if hStore <> nil then
        CertCloseStore(hStore, 0);
      if hMsg <> nil then
        CryptMsgClose(hMsg);
    end;
  end
  else
    WriteLn('Impossible de lire les informations de signature');
end;
```

## Double signature (SHA-1 et SHA-256)

Pour une compatibilité maximale (Windows 7 et versions récentes) :

```batch
@echo off
REM Première signature SHA-1
signtool sign /f "MonCertificat.pfx" /p "MotDePasse123!" ^
    /t http://timestamp.digicert.com ^
    /d "Mon Application" ^
    "MonApplication.exe"

REM Ajout signature SHA-256
signtool sign /f "MonCertificat.pfx" /p "MotDePasse123!" ^
    /as /fd sha256 ^
    /tr http://timestamp.digicert.com ^
    /td sha256 ^
    /d "Mon Application" ^
    "MonApplication.exe"

echo Double signature appliquee
```

## Signature de plusieurs fichiers

### Script batch pour signature en masse

```batch
@echo off
setlocal enabledelayedexpansion

set SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe"
set CERT="MonCertificat.pfx"
set PASS="MotDePasse123!"

echo === Signature de tous les executables ===

for %%f in (*.exe *.dll) do (
    echo.
    echo Signature de %%f...
    %SIGNTOOL% sign /f %CERT% /p %PASS% ^
        /t http://timestamp.digicert.com ^
        /d "Mon Application" "%%f"

    if !errorlevel! equ 0 (
        echo [OK] %%f signe avec succes
    ) else (
        echo [ERREUR] Echec de signature pour %%f
    )
)

echo.
echo === Verification des signatures ===
%SIGNTOOL% verify /pa *.exe *.dll

pause
```

### Outil Pascal pour signature en masse

```pascal
program SignatureMasse;

uses
  SysUtils, Classes, Process;

type
  TSignatureManager = class
  private
    FSignTool: string;
    FCertPath: string;
    FCertPass: string;
    FTimestamp: string;
    FLogFile: TextFile;

    procedure Log(const Message: string);
    function SignerUnFichier(const Fichier: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SignerDossier(const Dossier: string; const Masque: string = '*.exe');
    procedure VerifierSignatures(const Dossier: string);
  end;

constructor TSignatureManager.Create;
begin
  // Configuration
  FSignTool := 'C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe';
  FCertPath := ExtractFilePath(ParamStr(0)) + 'certificat.pfx';
  FCertPass := 'MotDePasse123!';
  FTimestamp := 'http://timestamp.digicert.com';

  // Créer fichier log
  AssignFile(FLogFile, 'signature.log');
  Rewrite(FLogFile);
  Log('=== Début de la session de signature ===');
  Log('Date : ' + DateTimeToStr(Now));
end;

destructor TSignatureManager.Destroy;
begin
  Log('=== Fin de la session ===');
  CloseFile(FLogFile);
  inherited;
end;

procedure TSignatureManager.Log(const Message: string);
begin
  WriteLn(FLogFile, '[' + TimeToStr(Now) + '] ' + Message);
  WriteLn(Message);  // Afficher aussi dans la console
end;

function TSignatureManager.SignerUnFichier(const Fichier: string): Boolean;
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := False;

  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := FSignTool;
    Process.Parameters.Add('sign');
    Process.Parameters.Add('/f');
    Process.Parameters.Add(FCertPath);
    Process.Parameters.Add('/p');
    Process.Parameters.Add(FCertPass);
    Process.Parameters.Add('/t');
    Process.Parameters.Add(FTimestamp);
    Process.Parameters.Add('/d');
    Process.Parameters.Add('Mon Application');
    Process.Parameters.Add(Fichier);

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);

    Result := Process.ExitStatus = 0;

    if Result then
      Log('[OK] ' + ExtractFileName(Fichier) + ' signé avec succès')
    else
    begin
      Log('[ERREUR] Échec signature ' + ExtractFileName(Fichier));
      Log('  Détails : ' + Output.Text);
    end;

  finally
    Output.Free;
    Process.Free;
  end;
end;

procedure TSignatureManager.SignerDossier(const Dossier: string; const Masque: string);
var
  SearchRec: TSearchRec;
  Chemin: string;
  NbFichiers, NbSucces: Integer;
begin
  Chemin := IncludeTrailingPathDelimiter(Dossier);
  NbFichiers := 0;
  NbSucces := 0;

  Log('Recherche dans : ' + Chemin + Masque);

  if FindFirst(Chemin + Masque, faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        Inc(NbFichiers);
        Log('');
        Log('Fichier ' + IntToStr(NbFichiers) + ': ' + SearchRec.Name);

        if SignerUnFichier(Chemin + SearchRec.Name) then
          Inc(NbSucces);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  Log('');
  Log('=== RÉSUMÉ ===');
  Log('Fichiers traités : ' + IntToStr(NbFichiers));
  Log('Signatures réussies : ' + IntToStr(NbSucces));
  Log('Échecs : ' + IntToStr(NbFichiers - NbSucces));
end;

procedure TSignatureManager.VerifierSignatures(const Dossier: string);
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := FSignTool;
    Process.Parameters.Add('verify');
    Process.Parameters.Add('/pa');
    Process.Parameters.Add(IncludeTrailingPathDelimiter(Dossier) + '*.exe');

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    Log('');
    Log('=== VÉRIFICATION DES SIGNATURES ===');
    Log(Output.Text);

  finally
    Output.Free;
    Process.Free;
  end;
end;

// Programme principal
var
  Manager: TSignatureManager;
  Dossier: string;
begin
  Manager := TSignatureManager.Create;
  try
    if ParamCount > 0 then
      Dossier := ParamStr(1)
    else
      Dossier := GetCurrentDir;

    Manager.SignerDossier(Dossier, '*.exe');
    Manager.SignerDossier(Dossier, '*.dll');
    Manager.VerifierSignatures(Dossier);

  finally
    Manager.Free;
  end;

  WriteLn('');
  WriteLn('Appuyez sur Entrée pour terminer...');
  ReadLn;
end.
```

## Gestion sécurisée des certificats

### Ne jamais faire

```pascal
// MAUVAIS : Mot de passe en dur dans le code
const
  CERT_PASSWORD = 'MotDePasse123!';  // NE JAMAIS FAIRE CELA !
```

### Bonnes pratiques

```pascal
unit GestionCertificat;

interface

uses
  Windows, SysUtils, Registry;

type
  TCertificatManager = class
  private
    FCertPath: string;
    FPassword: string;

    function LirePasswordSecurise: string;
    function ChiffrerPassword(const Pass: string): string;
    function DechiffrerPassword(const PassChiffre: string): string;
  public
    constructor Create;

    procedure ConfigurerCertificat(const Chemin, Password: string);
    function SignerFichier(const Fichier: string): Boolean;
    procedure EffacerPassword;
  end;

implementation

uses
  JwaCryptProt;  // Pour DPAPI

constructor TCertificatManager.Create;
begin
  FCertPath := '';
  FPassword := '';

  // Essayer de lire la configuration sauvegardée
  LirePasswordSecurise;
end;

function TCertificatManager.ChiffrerPassword(const Pass: string): string;
var
  DataIn, DataOut: DATA_BLOB;
  Description: PWideChar;
begin
  // Utiliser DPAPI pour chiffrer le mot de passe
  DataIn.pbData := @Pass[1];
  DataIn.cbData := Length(Pass) * SizeOf(Char);

  Description := 'Certificat Authenticode';

  if CryptProtectData(@DataIn, Description, nil, nil, nil,
                     CRYPTPROTECT_LOCAL_MACHINE, @DataOut) then
  begin
    SetLength(Result, DataOut.cbData);
    Move(DataOut.pbData^, Result[1], DataOut.cbData);
    LocalFree(HLOCAL(DataOut.pbData));
  end
  else
    raise Exception.Create('Erreur lors du chiffrement du mot de passe');
end;

function TCertificatManager.DechiffrerPassword(const PassChiffre: string): string;
var
  DataIn, DataOut: DATA_BLOB;
begin
  DataIn.pbData := @PassChiffre[1];
  DataIn.cbData := Length(PassChiffre);

  if CryptUnprotectData(@DataIn, nil, nil, nil, nil, 0, @DataOut) then
  begin
    SetLength(Result, DataOut.cbData div SizeOf(Char));
    Move(DataOut.pbData^, Result[1], DataOut.cbData);
    LocalFree(HLOCAL(DataOut.pbData));
  end
  else
    Result := '';
end;

function TCertificatManager.LirePasswordSecurise: string;
var
  Reg: TRegistry;
  PassChiffre: string;
begin
  Result := '';

  // Option 1 : Variable d'environnement
  Result := GetEnvironmentVariable('CERT_PASSWORD');
  if Result <> '' then
  begin
    FPassword := Result;
    Exit;
  end;

  // Option 2 : Registre avec chiffrement DPAPI
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('\Software\MonApplication\Signature') then
    begin
      if Reg.ValueExists('CertPath') then
        FCertPath := Reg.ReadString('CertPath');

      if Reg.ValueExists('CertPass') then
      begin
        PassChiffre := Reg.ReadString('CertPass');
        FPassword := DechiffrerPassword(PassChiffre);
      end;

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;

  // Option 3 : Demander à l'utilisateur
  if FPassword = '' then
  begin
    Write('Mot de passe du certificat : ');
    ReadLn(FPassword);
  end;
end;

procedure TCertificatManager.ConfigurerCertificat(const Chemin, Password: string);
var
  Reg: TRegistry;
  PassChiffre: string;
begin
  FCertPath := Chemin;
  FPassword := Password;

  // Sauvegarder de manière sécurisée
  PassChiffre := ChiffrerPassword(Password);

  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\MonApplication\Signature', True) then
    begin
      Reg.WriteString('CertPath', Chemin);
      Reg.WriteString('CertPass', PassChiffre);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TCertificatManager.EffacerPassword;
begin
  // Effacer le mot de passe de la mémoire
  if Length(FPassword) > 0 then
  begin
    FillChar(FPassword[1], Length(FPassword) * SizeOf(Char), 0);
    FPassword := '';
  end;
end;

function TCertificatManager.SignerFichier(const Fichier: string): Boolean;
begin
  // Implémenter la signature...
  Result := True;

  // Toujours effacer le mot de passe après utilisation
  EffacerPassword;
end;

end.
```

## Dépannage courant

### Problème : "Erreur : SignTool Error: No certificates were found"

**Solutions :**
- Vérifier le chemin du certificat
- Vérifier le mot de passe
- S'assurer que le certificat contient la clé privée

```batch
REM Vérifier le contenu du certificat
certutil -dump "MonCertificat.pfx"

REM Lister les certificats dans le magasin
certutil -store -user My

REM Installer le certificat dans le magasin (optionnel)
certutil -f -user -p "MotDePasse123!" -importPFX "MonCertificat.pfx"
```

### Problème : "The specified timestamp server could not be reached"

**Solution : Essayer différents serveurs de timestamp**

```pascal
unit TimestampServers;

interface

type
  TTimestampServer = record
    Name: string;
    URL: string;
    Protocol: string; // RFC3161 ou Authenticode
  end;

const
  TimestampServers: array[0..7] of TTimestampServer = (
    (Name: 'DigiCert'; URL: 'http://timestamp.digicert.com'; Protocol: 'Authenticode'),
    (Name: 'DigiCert SHA256'; URL: 'http://timestamp.digicert.com'; Protocol: 'RFC3161'),
    (Name: 'Sectigo'; URL: 'http://timestamp.sectigo.com'; Protocol: 'Authenticode'),
    (Name: 'Sectigo SHA256'; URL: 'http://timestamp.sectigo.com'; Protocol: 'RFC3161'),
    (Name: 'GlobalSign'; URL: 'http://timestamp.globalsign.com/scripts/timstamp.dll'; Protocol: 'Authenticode'),
    (Name: 'Certum'; URL: 'http://time.certum.pl'; Protocol: 'Authenticode'),
    (Name: 'Entrust'; URL: 'http://timestamp.entrust.net/TSS/RFC3161sha2TS'; Protocol: 'RFC3161'),
    (Name: 'Apple'; URL: 'http://timestamp.apple.com/ts01'; Protocol: 'RFC3161')
  );

function TrouverServeurDisponible: string;

implementation

uses
  Windows, WinInet, SysUtils;

function TestConnexionHTTP(const URL: string): Boolean;
var
  hInternet, hConnect: HINTERNET;
  Host, Path: string;
  Port: Integer;
  P: Integer;
begin
  Result := False;

  // Parser l'URL
  P := Pos('://', URL);
  if P > 0 then
    Host := Copy(URL, P + 3, MaxInt)
  else
    Host := URL;

  P := Pos('/', Host);
  if P > 0 then
  begin
    Path := Copy(Host, P, MaxInt);
    Host := Copy(Host, 1, P - 1);
  end
  else
    Path := '/';

  // Port par défaut
  Port := 80;
  P := Pos(':', Host);
  if P > 0 then
  begin
    Port := StrToIntDef(Copy(Host, P + 1, MaxInt), 80);
    Host := Copy(Host, 1, P - 1);
  end;

  // Tester la connexion
  hInternet := InternetOpen('SignTool', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if hInternet <> nil then
  begin
    try
      hConnect := InternetConnect(hInternet, PChar(Host), Port,
                                 nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
      if hConnect <> nil then
      begin
        InternetCloseHandle(hConnect);
        Result := True;
      end;
    finally
      InternetCloseHandle(hInternet);
    end;
  end;
end;

function TrouverServeurDisponible: string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to High(TimestampServers) do
  begin
    WriteLn('Test de ', TimestampServers[i].Name, '...');

    if TestConnexionHTTP(TimestampServers[i].URL) then
    begin
      Result := TimestampServers[i].URL;
      WriteLn('  [OK] Serveur disponible');
      Break;
    end
    else
      WriteLn('  [ÉCHEC] Serveur non accessible');
  end;

  if Result = '' then
    WriteLn('Aucun serveur de timestamp disponible !');
end;

end.
```

### Problème : "A certificate chain could not be built to a trusted root authority"

**Solution : Installer les certificats intermédiaires**

```pascal
procedure InstallerCertificatsIntermediaires;
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    // Télécharger et installer les certificats racine
    Process.Executable := 'certutil';
    Process.Parameters.Add('-syncWithWU');
    Process.Options := [poWaitOnExit];
    Process.Execute;

    WriteLn('Mise à jour des certificats racine effectuée');
  finally
    Process.Free;
  end;
end;

procedure VerifierChaineCertificat(const CheminCertificat: string);
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'certutil';
    Process.Parameters.Add('-verify');
    Process.Parameters.Add(CheminCertificat);
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    WriteLn(Output.Text);

    if Pos('certificate is valid', Output.Text) > 0 then
      WriteLn('Chaîne de certificat valide')
    else
      WriteLn('Problème avec la chaîne de certificat');

  finally
    Output.Free;
    Process.Free;
  end;
end;
```

### Problème : SmartScreen bloque toujours l'application

**Solution : Construire une réputation**

```pascal
unit SmartScreenReputation;

interface

type
  TReputationBuilder = class
  private
    FNombreTelechargements: Integer;
    FDatePremiereSIgnature: TDateTime;
  public
    procedure ConseilsPourReputation;
    function EstimationTempsReputation: Integer; // en jours
  end;

implementation

uses
  SysUtils, DateUtils;

procedure TReputationBuilder.ConseilsPourReputation;
begin
  WriteLn('=== Conseils pour améliorer la réputation SmartScreen ===');
  WriteLn;
  WriteLn('1. CERTIFICAT EV (Extended Validation)');
  WriteLn('   - Réputation immédiate avec un certificat EV');
  WriteLn('   - Plus cher mais évite l''attente');
  WriteLn;
  WriteLn('2. CERTIFICAT STANDARD');
  WriteLn('   - Nécessite du temps et des téléchargements');
  WriteLn('   - Généralement 3-4 semaines minimum');
  WriteLn('   - Besoin de plusieurs centaines de téléchargements');
  WriteLn;
  WriteLn('3. ACTIONS RECOMMANDÉES :');
  WriteLn('   - Toujours signer avec le même certificat');
  WriteLn('   - Utiliser un timestamp pour chaque signature');
  WriteLn('   - Maintenir les informations de version à jour');
  WriteLn('   - Distribuer via HTTPS uniquement');
  WriteLn('   - Soumettre à Microsoft Defender');
  WriteLn;
  WriteLn('4. SOUMISSION MANUELLE :');
  WriteLn('   - https://www.microsoft.com/wdsi/filesubmission');
  WriteLn('   - Catégorie : Software developer');
end;

function TReputationBuilder.EstimationTempsReputation: Integer;
begin
  // Estimation basée sur l'expérience
  if FNombreTelechargements < 100 then
    Result := 30  // Au moins 30 jours
  else if FNombreTelechargements < 500 then
    Result := 21  // 3 semaines
  else if FNombreTelechargements < 1000 then
    Result := 14  // 2 semaines
  else
    Result := 7;  // 1 semaine minimum

  // Ajuster selon l'ancienneté
  if DaysBetween(Now, FDatePremiereSIgnature) < 30 then
    Result := Result + 14;  // Certificat très récent
end;

end.
```

## Signature dans un environnement CI/CD

### GitHub Actions

```yaml
name: Build and Sign

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    runs-on: windows-latest

    steps:
    - uses: actions/checkout@v2

    - name: Setup Lazarus
      uses: gcarreno/setup-lazarus@v3
      with:
        lazarus-version: stable

    - name: Build Application
      run: |
        lazbuild MonProjet.lpi

    - name: Decode certificate
      run: |
        echo "${{ secrets.CERTIFICATE_BASE64 }}" | base64 -d > certificate.pfx

    - name: Sign executable
      run: |
        & "C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe" `
          sign /f certificate.pfx `
          /p "${{ secrets.CERTIFICATE_PASSWORD }}" `
          /t http://timestamp.digicert.com `
          /d "Mon Application" `
          MonApplication.exe

    - name: Verify signature
      run: |
        & "C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe" `
          verify /pa MonApplication.exe

    - name: Upload artifact
      uses: actions/upload-artifact@v2
      with:
        name: signed-application
        path: MonApplication.exe
```

### Script PowerShell pour CI/CD

```powershell
# SignApplication.ps1
param(
    [Parameter(Mandatory=$true)]
    [string]$FilePath,

    [Parameter(Mandatory=$true)]
    [string]$CertificateThumbprint,

    [string]$TimestampServer = "http://timestamp.digicert.com",

    [string]$Description = "Application"
)

# Fonction pour trouver SignTool
function Find-SignTool {
    $windowsKits = @(
        "${env:ProgramFiles(x86)}\Windows Kits\10\bin",
        "${env:ProgramFiles}\Windows Kits\10\bin",
        "${env:ProgramFiles(x86)}\Windows Kits\8.1\bin",
        "${env:ProgramFiles}\Windows Kits\8.1\bin"
    )

    foreach ($kit in $windowsKits) {
        if (Test-Path $kit) {
            $versions = Get-ChildItem $kit -Directory | Sort-Object Name -Descending
            foreach ($version in $versions) {
                $signtool = Join-Path $version.FullName "x64\signtool.exe"
                if (Test-Path $signtool) {
                    return $signtool
                }
            }
        }
    }

    throw "SignTool.exe not found"
}

# Trouver SignTool
$signTool = Find-SignTool
Write-Host "Using SignTool: $signTool"

# Signer le fichier
$arguments = @(
    "sign",
    "/sha1", $CertificateThumbprint,
    "/t", $TimestampServer,
    "/d", $Description,
    "/v",
    $FilePath
)

& $signTool $arguments

if ($LASTEXITCODE -ne 0) {
    throw "Signing failed with exit code $LASTEXITCODE"
}

Write-Host "Successfully signed: $FilePath"

# Vérifier la signature
& $signTool verify /pa $FilePath

if ($LASTEXITCODE -ne 0) {
    throw "Signature verification failed"
}

Write-Host "Signature verified successfully"
```

### Intégration avec Azure Key Vault

```pascal
unit AzureKeyVaultSigning;

interface

uses
  SysUtils, Classes, Process;

type
  TAzureKeyVaultSigner = class
  private
    FVaultName: string;
    FCertificateName: string;
    FClientId: string;
    FClientSecret: string;
    FTenantId: string;

    function GetAccessToken: string;
  public
    constructor Create(const VaultName, CertName: string);

    procedure ConfigureAuthentication(const ClientId, ClientSecret, TenantId: string);
    function SignerAvecKeyVault(const FichierASignier: string): Boolean;
  end;

implementation

uses
  fpjson, jsonparser, fphttpclient;

constructor TAzureKeyVaultSigner.Create(const VaultName, CertName: string);
begin
  FVaultName := VaultName;
  FCertificateName := CertName;
end;

procedure TAzureKeyVaultSigner.ConfigureAuthentication(
  const ClientId, ClientSecret, TenantId: string);
begin
  FClientId := ClientId;
  FClientSecret := ClientSecret;
  FTenantId := TenantId;
end;

function TAzureKeyVaultSigner.GetAccessToken: string;
var
  HTTP: TFPHTTPClient;
  Response: string;
  JSON: TJSONObject;
  PostData: TStringList;
begin
  Result := '';

  HTTP := TFPHTTPClient.Create(nil);
  PostData := TStringList.Create;
  try
    PostData.Add('grant_type=client_credentials');
    PostData.Add('client_id=' + FClientId);
    PostData.Add('client_secret=' + FClientSecret);
    PostData.Add('resource=https://vault.azure.net');

    Response := HTTP.FormPost(
      Format('https://login.microsoftonline.com/%s/oauth2/token', [FTenantId]),
      PostData
    );

    JSON := TJSONObject(GetJSON(Response));
    try
      Result := JSON.Get('access_token', '');
    finally
      JSON.Free;
    end;

  finally
    PostData.Free;
    HTTP.Free;
  end;
end;

function TAzureKeyVaultSigner.SignerAvecKeyVault(const FichierASignier: string): Boolean;
var
  Process: TProcess;
  Token: string;
begin
  Result := False;

  // Obtenir le token d'accès
  Token := GetAccessToken;
  if Token = '' then
  begin
    WriteLn('Erreur : impossible d''obtenir le token Azure');
    Exit;
  end;

  Process := TProcess.Create(nil);
  try
    // Utiliser AzureSignTool
    Process.Executable := 'AzureSignTool.exe';
    Process.Parameters.Add('sign');
    Process.Parameters.Add('-kvu');
    Process.Parameters.Add(Format('https://%s.vault.azure.net', [FVaultName]));
    Process.Parameters.Add('-kvc');
    Process.Parameters.Add(FCertificateName);
    Process.Parameters.Add('-kva');
    Process.Parameters.Add(Token);
    Process.Parameters.Add('-tr');
    Process.Parameters.Add('http://timestamp.digicert.com');
    Process.Parameters.Add('-td');
    Process.Parameters.Add('sha256');
    Process.Parameters.Add(FichierASignier);

    Process.Options := [poWaitOnExit];
    Process.Execute;

    Result := Process.ExitStatus = 0;

    if Result then
      WriteLn('Signature avec Azure Key Vault réussie')
    else
      WriteLn('Erreur lors de la signature avec Azure Key Vault');

  finally
    Process.Free;
  end;
end;

end.
```

## Signature de différents types de fichiers

### Signature de DLL

```pascal
procedure SignerDLL(const CheminDLL: string);
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'signtool.exe';
    Process.Parameters.Add('sign');
    Process.Parameters.Add('/f');
    Process.Parameters.Add('certificat.pfx');
    Process.Parameters.Add('/p');
    Process.Parameters.Add('password');
    Process.Parameters.Add('/fd');
    Process.Parameters.Add('sha256');
    Process.Parameters.Add('/tr');
    Process.Parameters.Add('http://timestamp.digicert.com');
    Process.Parameters.Add('/td');
    Process.Parameters.Add('sha256');
    Process.Parameters.Add('/d');
    Process.Parameters.Add('Ma Bibliothèque DLL');
    Process.Parameters.Add(CheminDLL);

    Process.Options := [poWaitOnExit];
    Process.Execute;

    if Process.ExitStatus = 0 then
      WriteLn('DLL signée avec succès')
    else
      WriteLn('Erreur lors de la signature de la DLL');

  finally
    Process.Free;
  end;
end;
```

### Signature de scripts PowerShell

```pascal
procedure SignerScriptPowerShell(const CheminScript: string);
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    // Utiliser Set-AuthenticodeSignature
    Process.Executable := 'powershell.exe';
    Process.Parameters.Add('-NoProfile');
    Process.Parameters.Add('-Command');
    Process.Parameters.Add(Format(
      '$cert = Get-ChildItem -Path Cert:\CurrentUser\My -CodeSigningCert; ' +
      'Set-AuthenticodeSignature -FilePath "%s" -Certificate $cert',
      [CheminScript]
    ));

    Process.Options := [poWaitOnExit];
    Process.Execute;

    if Process.ExitStatus = 0 then
      WriteLn('Script PowerShell signé')
    else
      WriteLn('Erreur lors de la signature du script');

  finally
    Process.Free;
  end;
end;
```

### Signature de packages MSI

```pascal
procedure SignerMSI(const CheminMSI: string);
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    // Signer le MSI
    Process.Executable := 'signtool.exe';
    Process.Parameters.Add('sign');
    Process.Parameters.Add('/f');
    Process.Parameters.Add('certificat.pfx');
    Process.Parameters.Add('/p');
    Process.Parameters.Add('password');
    Process.Parameters.Add('/d');
    Process.Parameters.Add('Mon Installateur');
    Process.Parameters.Add('/du');
    Process.Parameters.Add('https://monsite.com');
    Process.Parameters.Add('/t');
    Process.Parameters.Add('http://timestamp.digicert.com');
    Process.Parameters.Add(CheminMSI);

    Process.Options := [poWaitOnExit];
    Process.Execute;

    if Process.ExitStatus = 0 then
      WriteLn('Package MSI signé avec succès');

  finally
    Process.Free;
  end;
end;
```

## Outils et utilitaires

### Visualiseur de signature

```pascal
program VisualiserSignature;

uses
  Windows, SysUtils, JwaWinCrypt, DateUtils;

procedure AfficherDetailsSignature(const Fichier: string);
var
  hStore: HCERTSTORE;
  hMsg: HCRYPTMSG;
  pSignerInfo: PCMSG_SIGNER_INFO;
  dwSignerInfo: DWORD;
  pCertContext: PCCERT_CONTEXT;
  dwEncoding, dwContentType, dwFormatType: DWORD;
  SubjectName: array[0..255] of Char;
  IssuerName: array[0..255] of Char;
  SerialNumber: string;
  NotBefore, NotAfter: TSystemTime;
  i: Integer;
begin
  WriteLn('=== Informations de signature ===');
  WriteLn('Fichier : ', Fichier);
  WriteLn;

  // Requête sur le fichier signé
  if not CryptQueryObject(
    CERT_QUERY_OBJECT_FILE,
    PWideChar(WideString(Fichier)),
    CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED_EMBED,
    CERT_QUERY_FORMAT_FLAG_BINARY,
    0,
    @dwEncoding,
    @dwContentType,
    @dwFormatType,
    @hStore,
    @hMsg,
    nil) then
  begin
    WriteLn('Erreur : Fichier non signé ou signature invalide');
    Exit;
  end;

  try
    // Obtenir les informations du signataire
    CryptMsgGetParam(hMsg, CMSG_SIGNER_INFO_PARAM, 0, nil, @dwSignerInfo);
    GetMem(pSignerInfo, dwSignerInfo);
    try
      if CryptMsgGetParam(hMsg, CMSG_SIGNER_INFO_PARAM, 0, pSignerInfo, @dwSignerInfo) then
      begin
        // Trouver le certificat
        pCertContext := CertFindCertificateInStore(
          hStore,
          X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
          0,
          CERT_FIND_SUBJECT_CERT,
          pSignerInfo,
          nil
        );

        if pCertContext <> nil then
        begin
          // Nom du sujet
          CertGetNameString(pCertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE,
                           0, nil, SubjectName, SizeOf(SubjectName));
          WriteLn('Signé par : ', SubjectName);

          // Émetteur
          CertGetNameString(pCertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE,
                           CERT_NAME_ISSUER_FLAG, nil, IssuerName, SizeOf(IssuerName));
          WriteLn('Émis par : ', IssuerName);

          // Numéro de série
          SetLength(SerialNumber, pCertContext.pCertInfo.SerialNumber.cbData * 2);
          for i := 0 to pCertContext.pCertInfo.SerialNumber.cbData - 1 do
            SerialNumber := SerialNumber + IntToHex(pCertContext.pCertInfo.SerialNumber.pbData[i], 2);
          WriteLn('Numéro de série : ', SerialNumber);

          // Validité
          FileTimeToSystemTime(@pCertContext.pCertInfo.NotBefore, NotBefore);
          FileTimeToSystemTime(@pCertContext.pCertInfo.NotAfter, NotAfter);

          WriteLn('Valide du : ', DateToStr(SystemTimeToDateTime(NotBefore)));
          WriteLn('Valide jusqu''au : ', DateToStr(SystemTimeToDateTime(NotAfter)));

          // Algorithme de signature
          WriteLn('Algorithme : ', pCertContext.pCertInfo.SignatureAlgorithm.pszObjId);

          CertFreeCertificateContext(pCertContext);
        end;
      end;
    finally
      FreeMem(pSignerInfo);
    end;

    // Vérifier le timestamp
    WriteLn;
    WriteLn('=== Timestamp ===');
    VerifierTimestamp(hMsg);

  finally
    if hStore <> nil then
      CertCloseStore(hStore, 0);
    if hMsg <> nil then
      CryptMsgClose(hMsg);
  end;
end;

procedure VerifierTimestamp(hMsg: HCRYPTMSG);
var
  dwSize: DWORD;
  pTimeStamp: Pointer;
  TimeStampTime: FILETIME;
  SystemTime: TSystemTime;
begin
  // Vérifier la présence d'un timestamp
  if CryptMsgGetParam(hMsg, CMSG_SIGNER_UNAUTH_ATTR_PARAM, 0, nil, @dwSize) then
  begin
    GetMem(pTimeStamp, dwSize);
    try
      if CryptMsgGetParam(hMsg, CMSG_SIGNER_UNAUTH_ATTR_PARAM, 0, pTimeStamp, @dwSize) then
      begin
        // Analyser le timestamp (simplifié)
        WriteLn('Timestamp présent');
        // Le parsing complet nécessite plus de code...
      end;
    finally
      FreeMem(pTimeStamp);
    end;
  end
  else
    WriteLn('Pas de timestamp');
end;

begin
  if ParamCount > 0 then
    AfficherDetailsSignature(ParamStr(1))
  else
  begin
    WriteLn('Usage: VisualiserSignature.exe <fichier>');
    WriteLn;
    WriteLn('Exemple: VisualiserSignature.exe MonApp.exe');
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
end.
```

### Gestionnaire de certificats

```pascal
unit CertificateManager;

interface

uses
  Windows, SysUtils, Classes, Registry, DateUtils;

type
  TCertificateInfo = record
    SubjectName: string;
    IssuerName: string;
    SerialNumber: string;
    Thumbprint: string;
    NotBefore: TDateTime;
    NotAfter: TDateTime;
    IsValid: Boolean;
    DaysUntilExpiry: Integer;
  end;

  TCertificateStore = class
  private
    FCertificates: TList;

    function GetCertificateInfo(CertContext: PCCERT_CONTEXT): TCertificateInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromStore(StoreName: string = 'MY');
    procedure LoadFromFile(const FileName: string; const Password: string = '');

    function FindByThumbprint(const Thumbprint: string): TCertificateInfo;
    function FindBySubject(const Subject: string): TCertificateInfo;

    procedure ListCertificates;
    procedure CheckExpiration(DaysWarning: Integer = 30);
  end;

implementation

uses
  JwaWinCrypt;

constructor TCertificateStore.Create;
begin
  FCertificates := TList.Create;
end;

destructor TCertificateStore.Destroy;
begin
  FCertificates.Free;
  inherited;
end;

function TCertificateStore.GetCertificateInfo(CertContext: PCCERT_CONTEXT): TCertificateInfo;
var
  NameSize: DWORD;
  i: Integer;
begin
  // Nom du sujet
  NameSize := CertGetNameString(CertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE,
                                0, nil, nil, 0);
  SetLength(Result.SubjectName, NameSize);
  CertGetNameString(CertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE,
                   0, nil, @Result.SubjectName[1], NameSize);

  // Émetteur
  NameSize := CertGetNameString(CertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE,
                                CERT_NAME_ISSUER_FLAG, nil, nil, 0);
  SetLength(Result.IssuerName, NameSize);
  CertGetNameString(CertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE,
                   CERT_NAME_ISSUER_FLAG, nil, @Result.IssuerName[1], NameSize);

  // Dates de validité
  Result.NotBefore := FileTimeToDateTime(CertContext.pCertInfo.NotBefore);
  Result.NotAfter := FileTimeToDateTime(CertContext.pCertInfo.NotAfter);

  // Calculs
  Result.IsValid := (Now >= Result.NotBefore) and (Now <= Result.NotAfter);
  Result.DaysUntilExpiry := DaysBetween(Now, Result.NotAfter);

  // Thumbprint
  // (Simplification - le calcul réel est plus complexe)
  Result.Thumbprint := 'TODO: Calculate thumbprint';
end;

procedure TCertificateStore.LoadFromStore(StoreName: string);
var
  hStore: HCERTSTORE;
  pCertContext: PCCERT_CONTEXT;
  CertInfo: ^TCertificateInfo;
begin
  hStore := CertOpenSystemStore(0, PChar(StoreName));
  if hStore <> nil then
  begin
    try
      pCertContext := nil;
      repeat
        pCertContext := CertEnumCertificatesInStore(hStore, pCertContext);
        if pCertContext <> nil then
        begin
          New(CertInfo);
          CertInfo^ := GetCertificateInfo(pCertContext);
          FCertificates.Add(CertInfo);
        end;
      until pCertContext = nil;
    finally
      CertCloseStore(hStore, 0);
    end;
  end;
end;

procedure TCertificateStore.ListCertificates;
var
  i: Integer;
  Cert: ^TCertificateInfo;
begin
  WriteLn('=== Certificats disponibles ===');
  WriteLn;

  for i := 0 to FCertificates.Count - 1 do
  begin
    Cert := FCertificates[i];
    WriteLn(Format('%d. %s', [i + 1, Cert^.SubjectName]));
    WriteLn('   Émetteur : ', Cert^.IssuerName);
    WriteLn('   Valide du : ', DateToStr(Cert^.NotBefore), ' au ', DateToStr(Cert^.NotAfter));

    if Cert^.IsValid then
    begin
      if Cert^.DaysUntilExpiry <= 30 then
        WriteLn('   ⚠ ATTENTION : Expire dans ', Cert^.DaysUntilExpiry, ' jours !')
      else
        WriteLn('   ✓ Valide (expire dans ', Cert^.DaysUntilExpiry, ' jours)');
    end
    else
      WriteLn('   ✗ EXPIRÉ ou NON VALIDE');

    WriteLn('   Thumbprint : ', Cert^.Thumbprint);
    WriteLn;
  end;

  if FCertificates.Count = 0 then
    WriteLn('Aucun certificat trouvé');
end;

procedure TCertificateStore.CheckExpiration(DaysWarning: Integer);
var
  i: Integer;
  Cert: ^TCertificateInfo;
  HasWarning: Boolean;
begin
  HasWarning := False;

  WriteLn('=== Vérification expiration des certificats ===');
  WriteLn;

  for i := 0 to FCertificates.Count - 1 do
  begin
    Cert := FCertificates[i];

    if not Cert^.IsValid then
    begin
      WriteLn('❌ EXPIRÉ : ', Cert^.SubjectName);
      WriteLn('   Expiré depuis : ', DateToStr(Cert^.NotAfter));
      HasWarning := True;
    end
    else if Cert^.DaysUntilExpiry <= DaysWarning then
    begin
      WriteLn('⚠️  ATTENTION : ', Cert^.SubjectName);
      WriteLn('   Expire dans ', Cert^.DaysUntilExpiry, ' jours (',
              DateToStr(Cert^.NotAfter), ')');
      HasWarning := True;
    end;
  end;

  if not HasWarning then
    WriteLn('✅ Tous les certificats sont valides pour plus de ', DaysWarning, ' jours');
end;

procedure TCertificateStore.LoadFromFile(const FileName: string; const Password: string);
var
  hStore: HCERTSTORE;
  CertBlob: CRYPT_DATA_BLOB;
  FileStream: TFileStream;
  Buffer: array of Byte;
  pCertContext: PCCERT_CONTEXT;
  CertInfo: ^TCertificateInfo;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Buffer, FileStream.Size);
    FileStream.Read(Buffer[0], FileStream.Size);

    CertBlob.cbData := Length(Buffer);
    CertBlob.pbData := @Buffer[0];

    // Ouvrir le PFX
    hStore := PFXImportCertStore(@CertBlob, PWideChar(WideString(Password)),
                                 CRYPT_USER_KEYSET);
    if hStore <> nil then
    begin
      try
        pCertContext := nil;
        repeat
          pCertContext := CertEnumCertificatesInStore(hStore, pCertContext);
          if pCertContext <> nil then
          begin
            New(CertInfo);
            CertInfo^ := GetCertificateInfo(pCertContext);
            FCertificates.Add(CertInfo);
          end;
        until pCertContext = nil;
      finally
        CertCloseStore(hStore, 0);
      end;
    end
    else
      raise Exception.Create('Impossible d''ouvrir le certificat PFX');

  finally
    FileStream.Free;
  end;
end;

function TCertificateStore.FindByThumbprint(const Thumbprint: string): TCertificateInfo;
var
  i: Integer;
  Cert: ^TCertificateInfo;
begin
  FillChar(Result, SizeOf(Result), 0);

  for i := 0 to FCertificates.Count - 1 do
  begin
    Cert := FCertificates[i];
    if CompareText(Cert^.Thumbprint, Thumbprint) = 0 then
    begin
      Result := Cert^;
      Exit;
    end;
  end;
end;

function TCertificateStore.FindBySubject(const Subject: string): TCertificateInfo;
var
  i: Integer;
  Cert: ^TCertificateInfo;
begin
  FillChar(Result, SizeOf(Result), 0);

  for i := 0 to FCertificates.Count - 1 do
  begin
    Cert := FCertificates[i];
    if Pos(UpperCase(Subject), UpperCase(Cert^.SubjectName)) > 0 then
    begin
      Result := Cert^;
      Exit;
    end;
  end;
end;
```

## Application complète de gestion de signature

```pascal
program GestionnaireSignature;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils, Registry, Process,
  CertificateManager, TimestampServers;

type
  TSignatureApp = class
  private
    FCertStore: TCertificateStore;
    FConfig: TStringList;
    FLogFile: TextFile;

    procedure ChargerConfiguration;
    procedure SauvegarderConfiguration;
    procedure InitialiserLog;
    procedure Log(const Message: string);

    procedure MenuPrincipal;
    procedure GererCertificats;
    procedure SignerFichiers;
    procedure VerifierSignatures;
    procedure ConfigurerApplication;

    function ChoisirCertificat: string;
    function DemanderMotDePasse: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Executer;
  end;

constructor TSignatureApp.Create;
begin
  FCertStore := TCertificateStore.Create;
  FConfig := TStringList.Create;
  ChargerConfiguration;
  InitialiserLog;
end;

destructor TSignatureApp.Destroy;
begin
  SauvegarderConfiguration;
  CloseFile(FLogFile);
  FCertStore.Free;
  FConfig.Free;
  inherited;
end;

procedure TSignatureApp.ChargerConfiguration;
var
  ConfigFile: string;
begin
  ConfigFile := ChangeFileExt(ParamStr(0), '.ini');

  if FileExists(ConfigFile) then
    FConfig.LoadFromFile(ConfigFile)
  else
  begin
    // Configuration par défaut
    FConfig.Add('SignTool=C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe');
    FConfig.Add('Certificate=');
    FConfig.Add('Timestamp=http://timestamp.digicert.com');
    FConfig.Add('Description=Mon Application');
    FConfig.Add('URL=https://monsite.com');
    FConfig.Add('Algorithm=SHA256');
  end;
end;

procedure TSignatureApp.SauvegarderConfiguration;
var
  ConfigFile: string;
begin
  ConfigFile := ChangeFileExt(ParamStr(0), '.ini');
  FConfig.SaveToFile(ConfigFile);
end;

procedure TSignatureApp.InitialiserLog;
var
  LogFileName: string;
begin
  LogFileName := ChangeFileExt(ParamStr(0), '_' + FormatDateTime('yyyymmdd', Now) + '.log');
  AssignFile(FLogFile, LogFileName);

  if FileExists(LogFileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);

  Log('=== Session démarrée : ' + DateTimeToStr(Now) + ' ===');
end;

procedure TSignatureApp.Log(const Message: string);
begin
  WriteLn(FLogFile, '[' + TimeToStr(Now) + '] ' + Message);
  Flush(FLogFile);
end;

procedure TSignatureApp.MenuPrincipal;
var
  Choix: string;
begin
  repeat
    WriteLn;
    WriteLn('=================================');
    WriteLn(' GESTIONNAIRE DE SIGNATURE');
    WriteLn('=================================');
    WriteLn;
    WriteLn('1. Gérer les certificats');
    WriteLn('2. Signer des fichiers');
    WriteLn('3. Vérifier des signatures');
    WriteLn('4. Configuration');
    WriteLn('5. Quitter');
    WriteLn;
    Write('Votre choix : ');
    ReadLn(Choix);

    case Choix of
      '1': GererCertificats;
      '2': SignerFichiers;
      '3': VerifierSignatures;
      '4': ConfigurerApplication;
      '5': Break;
    else
      WriteLn('Choix invalide');
    end;
  until False;
end;

procedure TSignatureApp.GererCertificats;
var
  Choix: string;
  PfxFile: string;
  Password: string;
  Num: Integer;
begin
  repeat
    WriteLn;
    WriteLn('=== GESTION DES CERTIFICATS ===');
    WriteLn;
    WriteLn('1. Lister les certificats du magasin');
    WriteLn('2. Importer un certificat PFX');
    WriteLn('3. Vérifier les expirations');
    WriteLn('4. Afficher les détails d''un certificat');
    WriteLn('5. Retour');
    WriteLn;
    Write('Votre choix : ');
    ReadLn(Choix);

    case Choix of
      '1':
        begin
          FCertStore.LoadFromStore('MY');
          FCertStore.ListCertificates;
        end;

      '2':
        begin
          Write('Chemin du fichier PFX : ');
          ReadLn(PfxFile);

          if FileExists(PfxFile) then
          begin
            Password := DemanderMotDePasse;
            try
              FCertStore.LoadFromFile(PfxFile, Password);
              WriteLn('Certificat importé avec succès');
              Log('Certificat importé : ' + PfxFile);
            except
              on E: Exception do
              begin
                WriteLn('Erreur : ', E.Message);
                Log('Erreur import certificat : ' + E.Message);
              end;
            end;
          end
          else
            WriteLn('Fichier non trouvé');
        end;

      '3':
        begin
          FCertStore.LoadFromStore('MY');
          FCertStore.CheckExpiration(30);
        end;

      '4':
        begin
          FCertStore.LoadFromStore('MY');
          FCertStore.ListCertificates;
          Write('Numéro du certificat à examiner : ');
          ReadLn(Num);
          // Afficher les détails...
        end;

      '5': Break;
    end;
  until False;
end;

procedure TSignatureApp.SignerFichiers;
var
  Fichier: string;
  Certificat: string;
  Password: string;
  Process: TProcess;
  Timestamp: string;
begin
  WriteLn;
  WriteLn('=== SIGNATURE DE FICHIERS ===');
  WriteLn;

  Write('Fichier à signer (ou *.exe pour tous) : ');
  ReadLn(Fichier);

  if not FileExists(Fichier) and (Pos('*', Fichier) = 0) then
  begin
    WriteLn('Fichier non trouvé');
    Exit;
  end;

  // Choisir le certificat
  Certificat := ChoisirCertificat;
  if Certificat = '' then
  begin
    WriteLn('Aucun certificat sélectionné');
    Exit;
  end;

  Password := DemanderMotDePasse;

  // Tester les serveurs de timestamp
  WriteLn;
  WriteLn('Test des serveurs de timestamp...');
  Timestamp := TrouverServeurDisponible;
  if Timestamp = '' then
  begin
    WriteLn('Aucun serveur de timestamp disponible !');
    if MessageBox(0, 'Continuer sans timestamp ?', 'Avertissement', MB_YESNO) = IDNO then
      Exit;
  end;

  Process := TProcess.Create(nil);
  try
    Process.Executable := FConfig.Values['SignTool'];
    Process.Parameters.Add('sign');
    Process.Parameters.Add('/f');
    Process.Parameters.Add(Certificat);
    Process.Parameters.Add('/p');
    Process.Parameters.Add(Password);

    if Timestamp <> '' then
    begin
      Process.Parameters.Add('/tr');
      Process.Parameters.Add(Timestamp);
      Process.Parameters.Add('/td');
      Process.Parameters.Add('sha256');
    end;

    Process.Parameters.Add('/fd');
    Process.Parameters.Add(FConfig.Values['Algorithm']);
    Process.Parameters.Add('/d');
    Process.Parameters.Add(FConfig.Values['Description']);
    Process.Parameters.Add('/du');
    Process.Parameters.Add(FConfig.Values['URL']);
    Process.Parameters.Add(Fichier);

    Process.Options := [poWaitOnExit, poUsePipes];

    WriteLn;
    WriteLn('Signature en cours...');
    Process.Execute;

    if Process.ExitStatus = 0 then
    begin
      WriteLn('✅ Signature réussie !');
      Log('Fichier signé : ' + Fichier);

      // Vérifier immédiatement
      Process.Parameters.Clear;
      Process.Parameters.Add('verify');
      Process.Parameters.Add('/pa');
      Process.Parameters.Add(Fichier);
      Process.Execute;

      if Process.ExitStatus = 0 then
        WriteLn('✅ Vérification OK')
      else
        WriteLn('⚠️  Problème lors de la vérification');
    end
    else
    begin
      WriteLn('❌ Échec de la signature');
      Log('Échec signature : ' + Fichier);
    end;

  finally
    // Effacer le mot de passe de la mémoire
    FillChar(Password[1], Length(Password), 0);
    Process.Free;
  end;
end;

procedure TSignatureApp.VerifierSignatures;
var
  Dossier: string;
  SearchRec: TSearchRec;
  Process: TProcess;
  NbFichiers, NbSignes, NbValides: Integer;
begin
  WriteLn;
  WriteLn('=== VÉRIFICATION DES SIGNATURES ===');
  WriteLn;

  Write('Dossier à vérifier (vide = dossier actuel) : ');
  ReadLn(Dossier);

  if Dossier = '' then
    Dossier := GetCurrentDir;

  Dossier := IncludeTrailingPathDelimiter(Dossier);

  NbFichiers := 0;
  NbSignes := 0;
  NbValides := 0;

  Process := TProcess.Create(nil);
  try
    Process.Executable := FConfig.Values['SignTool'];
    Process.Options := [poWaitOnExit, poUsePipes];

    // Rechercher tous les exécutables
    if FindFirst(Dossier + '*.exe', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        Inc(NbFichiers);
        Write(SearchRec.Name, ' : ');

        Process.Parameters.Clear;
        Process.Parameters.Add('verify');
        Process.Parameters.Add('/pa');
        Process.Parameters.Add(Dossier + SearchRec.Name);

        Process.Execute;

        if Process.ExitStatus = 0 then
        begin
          Inc(NbSignes);
          Inc(NbValides);
          WriteLn('✅ Signé et valide');
        end
        else
        begin
          // Vérifier si signé mais non valide
          Process.Parameters.Clear;
          Process.Parameters.Add('verify');
          Process.Parameters.Add('/all');
          Process.Parameters.Add(Dossier + SearchRec.Name);
          Process.Execute;

          if Process.ExitStatus = 0 then
          begin
            Inc(NbSignes);
            WriteLn('⚠️  Signé mais non validé');
          end
          else
            WriteLn('❌ Non signé');
        end;

      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;

    // Vérifier aussi les DLL
    if FindFirst(Dossier + '*.dll', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        Inc(NbFichiers);
        Write(SearchRec.Name, ' : ');

        Process.Parameters.Clear;
        Process.Parameters.Add('verify');
        Process.Parameters.Add('/pa');
        Process.Parameters.Add(Dossier + SearchRec.Name);

        Process.Execute;

        if Process.ExitStatus = 0 then
        begin
          Inc(NbSignes);
          Inc(NbValides);
          WriteLn('✅ Signé et valide');
        end
        else
          WriteLn('❌ Non signé');

      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;

  finally
    Process.Free;
  end;

  WriteLn;
  WriteLn('=== RÉSUMÉ ===');
  WriteLn('Fichiers analysés : ', NbFichiers);
  WriteLn('Fichiers signés : ', NbSignes, ' (',
          Round(NbSignes * 100 / Max(NbFichiers, 1)), '%)');
  WriteLn('Signatures valides : ', NbValides, ' (',
          Round(NbValides * 100 / Max(NbFichiers, 1)), '%)');

  Log(Format('Vérification : %d fichiers, %d signés, %d valides',
            [NbFichiers, NbSignes, NbValides]));
end;

procedure TSignatureApp.ConfigurerApplication;
var
  Choix: string;
  Path: string;
  Input: string;
  Num: Integer;
  i: Integer;
  Desc: string;
  URL: string;
  Algo: string;
begin
  repeat
    WriteLn;
    WriteLn('=== CONFIGURATION ===');
    WriteLn;
    WriteLn('1. SignTool : ', FConfig.Values['SignTool']);
    WriteLn('2. Certificat : ', FConfig.Values['Certificate']);
    WriteLn('3. Timestamp : ', FConfig.Values['Timestamp']);
    WriteLn('4. Description : ', FConfig.Values['Description']);
    WriteLn('5. URL : ', FConfig.Values['URL']);
    WriteLn('6. Algorithme : ', FConfig.Values['Algorithm']);
    WriteLn('7. Tester la configuration');
    WriteLn('8. Retour');
    WriteLn;
    Write('Modifier (1-6) ou retour (8) : ');
    ReadLn(Choix);

    case Choix of
      '1':
        begin
          Write('Chemin de SignTool : ');
          ReadLn(Path);
          if FileExists(Path) then
            FConfig.Values['SignTool'] := Path
          else
            WriteLn('Fichier non trouvé');
        end;

      '2':
        begin
          Write('Chemin du certificat PFX : ');
          ReadLn(Path);
          if FileExists(Path) then
            FConfig.Values['Certificate'] := Path
          else
            WriteLn('Fichier non trouvé');
        end;

      '3':
        begin
          WriteLn('Serveurs de timestamp disponibles :');
          for i := 0 to High(TimestampServers) do
            WriteLn(i + 1, '. ', TimestampServers[i].Name, ' : ',
                   TimestampServers[i].URL);
          Write('Numéro ou URL personnalisée : ');
          ReadLn(Input);
          Num := StrToIntDef(Input, 0);
          if (Num > 0) and (Num <= Length(TimestampServers)) then
            FConfig.Values['Timestamp'] := TimestampServers[Num - 1].URL
          else
            FConfig.Values['Timestamp'] := Input;
        end;

      '4':
        begin
          Write('Description : ');
          ReadLn(Desc);
          FConfig.Values['Description'] := Desc;
        end;

      '5':
        begin
          Write('URL : ');
          ReadLn(URL);
          FConfig.Values['URL'] := URL;
        end;

      '6':
        begin
          WriteLn('1. SHA1 (ancien, compatible)');
          WriteLn('2. SHA256 (recommandé)');
          Write('Choix : ');
          ReadLn(Algo);
          if Algo = '1' then
            FConfig.Values['Algorithm'] := 'SHA1'
          else
            FConfig.Values['Algorithm'] := 'SHA256';
        end;

      '7':
        begin
          WriteLn('Test de la configuration...');

          // Test SignTool
          if FileExists(FConfig.Values['SignTool']) then
            WriteLn('✅ SignTool trouvé')
          else
            WriteLn('❌ SignTool non trouvé');

          // Test certificat
          if FileExists(FConfig.Values['Certificate']) then
            WriteLn('✅ Certificat trouvé')
          else
            WriteLn('⚠️  Certificat non configuré');

          // Test timestamp
          if TestConnexionHTTP(FConfig.Values['Timestamp']) then
            WriteLn('✅ Serveur timestamp accessible')
          else
            WriteLn('⚠️  Serveur timestamp non accessible');
        end;

      '8':
        begin
          SauvegarderConfiguration;
          Break;
        end;
    end;
  until False;
end;

function TSignatureApp.ChoisirCertificat: string;
begin
  Result := FConfig.Values['Certificate'];

  if Result = '' then
  begin
    Write('Chemin du certificat PFX : ');
    ReadLn(Result);

    if FileExists(Result) then
      FConfig.Values['Certificate'] := Result
    else
    begin
      WriteLn('Fichier non trouvé');
      Result := '';
    end;
  end;
end;

function TSignatureApp.DemanderMotDePasse: string;
begin
  Write('Mot de passe du certificat : ');
  // Dans une vraie application, masquer la saisie
  ReadLn(Result);
end;

procedure TSignatureApp.Executer;
begin
  WriteLn('Bienvenue dans le Gestionnaire de Signature Authenticode');
  WriteLn('Version 1.0 - FreePascal/Lazarus');
  WriteLn;

  Log('Application démarrée');

  try
    MenuPrincipal;
  except
    on E: Exception do
    begin
      WriteLn('Erreur fatale : ', E.Message);
      Log('Erreur fatale : ' + E.Message);
    end;
  end;

  Log('Application terminée');
  WriteLn;
  WriteLn('Au revoir !');
end;

// Programme principal
var
  App: TSignatureApp;
begin
  App := TSignatureApp.Create;
  try
    App.Executer;
  finally
    App.Free;
  end;
end.
```

## Checklist finale pour la signature

### Avant la signature

- [ ] Certificat de signature de code obtenu et valide
- [ ] Certificat installé dans le magasin Windows ou fichier PFX disponible
- [ ] Mot de passe du certificat stocké de manière sécurisée
- [ ] SignTool installé (Windows SDK)
- [ ] Informations de version définies dans l'application
- [ ] Description et URL de l'éditeur configurées

### Pendant la signature

- [ ] Utiliser SHA-256 pour les nouvelles signatures
- [ ] Toujours inclure un timestamp
- [ ] Tester plusieurs serveurs de timestamp si nécessaire
- [ ] Vérifier immédiatement après signature
- [ ] Journaliser toutes les opérations de signature

### Après la signature

- [ ] Vérifier la signature avec `signtool verify /pa`
- [ ] Tester sur différentes versions de Windows
- [ ] Vérifier que SmartScreen ne bloque pas (peut prendre du temps)
- [ ] Soumettre à Microsoft Defender si nécessaire
- [ ] Documenter le processus de signature

### Maintenance

- [ ] Surveiller l'expiration du certificat (rappel 60 jours avant)
- [ ] Planifier le renouvellement du certificat
- [ ] Maintenir à jour les serveurs de timestamp
- [ ] Archiver les anciennes versions signées
- [ ] Documenter les changements de certificat

## Résumé et bonnes pratiques

### Points essentiels à retenir

1. **La signature est essentielle** pour la distribution professionnelle
2. **Utilisez toujours un timestamp** pour la validité à long terme
3. **Protégez votre clé privée** comme un trésor
4. **Automatisez le processus** pour éviter les erreurs
5. **Vérifiez systématiquement** après signature
6. **Documentez tout** pour la maintenance

### Erreurs à éviter

❌ **Ne jamais** :
- Stocker le mot de passe en clair dans le code
- Partager votre certificat avec la clé privée
- Oublier le timestamp
- Ignorer les avertissements d'expiration
- Distribuer des binaires non signés en production

✅ **Toujours** :
- Utiliser des variables d'environnement ou un gestionnaire de secrets
- Sauvegarder votre certificat de manière sécurisée
- Tester la signature sur une machine propre
- Maintenir une documentation à jour
- Prévoir le renouvellement du certificat

### Évolution et tendances

**Tendances actuelles** :
- Migration vers SHA-256 uniquement
- Certificats EV pour la réputation immédiate
- Signature dans le cloud (Azure Key Vault, AWS KMS)
- Intégration CI/CD automatique
- Signature de conteneurs et packages

**Préparation pour l'avenir** :
- Se préparer aux certificats quantiques
- Adopter les nouvelles normes de timestamp
- Suivre les évolutions de Windows 11+
- Considérer la signature cross-platform

## Conclusion

La signature Authenticode est un élément crucial pour établir la confiance avec vos utilisateurs et assurer une distribution professionnelle de vos applications FreePascal/Lazarus sous Windows.

**Ce que vous avez appris** :
- Les concepts fondamentaux d'Authenticode
- Comment obtenir et gérer des certificats
- L'utilisation de SignTool et ses alternatives
- L'automatisation du processus de signature
- La résolution des problèmes courants
- Les bonnes pratiques de sécurité

**Prochaines étapes** :
1. Obtenir un certificat de signature de code
2. Mettre en place un processus de signature automatisé
3. Intégrer la signature dans votre pipeline CI/CD
4. Documenter votre processus pour l'équipe
5. Planifier la maintenance et le renouvellement

Avec ces connaissances, vous êtes maintenant capable de signer professionnellement vos applications FreePascal/Lazarus et de les distribuer en toute confiance sur Windows. La signature numérique n'est pas seulement une protection technique, c'est aussi une marque de professionnalisme et de respect pour vos utilisateurs.

⏭️ [Windows Installer (MSI)](/06-specificites-windows/08-windows-installer-msi.md)
