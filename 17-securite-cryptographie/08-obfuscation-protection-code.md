🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.8 Obfuscation et protection du code

## Introduction

L'obfuscation et la protection du code sont des techniques permettant de rendre plus difficile l'analyse, la rétro-ingénierie et la copie non autorisée de vos applications FreePascal/Lazarus. Bien qu'aucune protection ne soit absolue, ces méthodes augmentent considérablement le temps et les compétences nécessaires pour comprendre ou modifier votre code.

## Pourquoi protéger son code ?

### Motivations légitimes

- **Protection de la propriété intellectuelle** : Protéger des algorithmes propriétaires ou des innovations
- **Conformité aux licences** : Empêcher l'utilisation non autorisée de votre logiciel
- **Sécurité** : Rendre plus difficile la découverte de vulnérabilités
- **Protection contre la concurrence** : Préserver votre avantage commercial
- **Prévention du piratage** : Compliquer le contournement des mécanismes de licence

### Limites importantes

Il est crucial de comprendre que :
- Aucune protection n'est inviolable
- L'obfuscation ralentit mais n'empêche pas totalement la rétro-ingénierie
- Une protection trop agressive peut impacter les performances
- La maintenance de code obfusqué est plus complexe

## Concepts de base

### Qu'est-ce que l'obfuscation ?

L'obfuscation consiste à transformer le code ou l'exécutable pour le rendre difficile à comprendre tout en préservant sa fonctionnalité. C'est comme écrire un texte dans un langage codé : le message reste le même, mais il devient illisible pour qui n'a pas la clé.

### Différence entre compilation et obfuscation

```
Code source Pascal → Compilation → Exécutable binaire
                                          ↓
                                   Obfuscation
                                          ↓
                              Exécutable protégé
```

## Techniques d'obfuscation pour FreePascal

### 1. Strip des symboles de débogage

La méthode la plus simple et la plus efficace consiste à retirer tous les symboles de débogage de l'exécutable final.

**Configuration dans Lazarus :**

```
Project → Project Options → Compiler Options → Debugging
- Décocher "Generate debugging info for GDB"
- Décocher "Use LineInfo unit"
```

**Options du compilateur FPC :**

```pascal
// Dans les options de projet ou .lpi
-Xs     // Strip all symbols from executable
-XX     // Smart linking (enlève le code non utilisé)
```

**Ligne de commande pour strip manuel :**

```bash
# Windows
strip --strip-all monprogramme.exe

# Ubuntu/Linux
strip --strip-all monprogramme
```

**Impact :** Réduit la taille de l'exécutable de 30-70% et supprime les noms de fonctions, variables et informations de ligne.

### 2. Optimisation du compilateur

Les optimisations rendent le code machine plus difficile à analyser en réorganisant les instructions.

```pascal
// Options de compilation recommandées
-O3        // Niveau d'optimisation maximal
-OoREGVAR  // Optimisation des variables en registres
-OoLOOPUNROLL  // Déroulement des boucles
-CX        // Smart linking
```

**Configuration Lazarus :**

```
Project Options → Compiler Options → Code
- Optimization Level : Level 3
- Cocher "Smaller rather than faster (-Os)"
```

### 3. Renommage des symboles exportés

Si votre application exporte des fonctions (DLL/SO), renommez-les de manière non descriptive.

**Avant :**

```pascal
function CalculateLicenseKey(const UserName: string): string; stdcall;  
exports
  CalculateLicenseKey;
```

**Après :**

```pascal
function X7F2A9(const P1: string): string; stdcall;  
exports
  X7F2A9 name 'A1';
```

### 4. Chiffrement des chaînes de caractères

Les chaînes en clair dans l'exécutable révèlent beaucoup d'informations.

**Exemple simple de chiffrement XOR :**

```pascal
unit StringObfuscation;

interface

function DecryptString(const Encrypted: array of Byte): string;

implementation

const
  XOR_KEY = $A7; // Clé simple (à améliorer en production)

function DecryptString(const Encrypted: array of Byte): string;  
var
  i: Integer;
begin
  SetLength(Result, Length(Encrypted));
  for i := 0 to High(Encrypted) do
    Result[i + 1] := Chr(Encrypted[i] xor XOR_KEY);
end;

end.
```

**Utilisation :**

```pascal
// Au lieu de :
ShowMessage('Erreur de licence');

// Utiliser :
const
  MSG_ERROR: array[0..16] of Byte = (
    $C2, $D3, $D3, $D4, $DC, $D3, $A0, $C1, $C4, $A0,
    $CD, $C8, $C2, $C4, $C7, $C2, $C4
  );

ShowMessage(DecryptString(MSG_ERROR));
```

### 5. Obfuscation du flux de contrôle

Rendre la logique du programme plus difficile à suivre.

**Technique : Insertion de code mort**

```pascal
procedure ProcessData(Data: Integer);  
var
  Dummy: Integer;
begin
  // Code réel
  if Data > 100 then
    Result := Data * 2
  else
    Result := Data + 10;

  // Code mort qui ne s'exécute jamais mais complique l'analyse
  if Random(1000) = 999999 then
  begin
    Dummy := Data * 547;
    Sleep(Dummy);
  end;
end;
```

**Technique : Utilisation de pointeurs de fonction**

```pascal
type
  TOperationProc = procedure(var Value: Integer);

var
  Operations: array[0..3] of TOperationProc;

procedure Op1(var Value: Integer); begin Value := Value * 2; end;  
procedure Op2(var Value: Integer); begin Value := Value + 10; end;  
procedure Op3(var Value: Integer); begin Value := Value - 5; end;  
procedure Op4(var Value: Integer); begin Value := Value div 3; end;

procedure Initialize;  
begin
  // Ordre aléatoire ou basé sur un calcul complexe
  Operations[0] := @Op2;
  Operations[1] := @Op4;
  Operations[2] := @Op1;
  Operations[3] := @Op3;
end;

procedure ProcessValue(var Value: Integer; OpIndex: Integer);  
begin
  Operations[OpIndex](Value);
end;
```

### 6. Anti-débogage

Détecter et contrer les tentatives de débogage.

**Windows - Détection de débogueur :**

```pascal
{$IFDEF WINDOWS}
uses
  Windows;

function IsDebuggerPresent: Boolean;  
begin
  Result := Windows.IsDebuggerPresent;
end;

function CheckRemoteDebugger: Boolean;  
var
  IsDebugged: BOOL;
begin
  IsDebugged := False;
  CheckRemoteDebuggerPresent(GetCurrentProcess, @IsDebugged);
  Result := IsDebugged;
end;
{$ENDIF}
```

**Linux - Détection via /proc :**

```pascal
{$IFDEF LINUX}
function IsDebuggerPresent: Boolean;  
var
  StatusFile: TextFile;
  Line: string;
begin
  Result := False;
  try
    AssignFile(StatusFile, '/proc/self/status');
    Reset(StatusFile);
    while not Eof(StatusFile) do
    begin
      ReadLn(StatusFile, Line);
      if Pos('TracerPid:', Line) > 0 then
      begin
        Result := StrToIntDef(Copy(Line, Pos(':', Line) + 1, MaxInt), 0) <> 0;
        Break;
      end;
    end;
    CloseFile(StatusFile);
  except
    Result := False;
  end;
end;
{$ENDIF}
```

**Utilisation :**

```pascal
procedure ProtectedProcedure;  
begin
  if IsDebuggerPresent then
  begin
    // Comportement alternatif ou arrêt
    Halt(1);
  end;

  // Code sensible ici
end;
```

### 7. Vérification d'intégrité

Vérifier que l'exécutable n'a pas été modifié.

```pascal
unit IntegrityCheck;

interface

function VerifyExecutableIntegrity: Boolean;

implementation

uses
  SysUtils, Classes, md5;

function CalculateFileMD5(const FileName: string): string;  
var
  FileStream: TFileStream;
  MD5Digest: TMD5Digest;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    MD5Digest := MD5File(FileStream);
    Result := MD5Print(MD5Digest);
  finally
    FileStream.Free;
  end;
end;

function VerifyExecutableIntegrity: Boolean;  
const
  // Hash MD5 de l'exécutable original (à calculer et intégrer)
  EXPECTED_HASH = 'A1B2C3D4E5F6...';
var
  CurrentHash: string;
begin
  CurrentHash := CalculateFileMD5(ParamStr(0));
  Result := (CurrentHash = EXPECTED_HASH);
end;

end.
```

## Outils tiers pour FreePascal

### 1. UPX (Ultimate Packer for eXecutables)

UPX compresse l'exécutable et ajoute une couche de protection basique.

**Windows :**

```batch
upx --best --ultra-brute monprogramme.exe
```

**Ubuntu/Linux :**

```bash
sudo apt install upx-ucl  
upx --best --ultra-brute monprogramme
```

**Avantages :**
- Réduction de la taille de 50-70%
- Décompression automatique en mémoire
- Rend l'analyse statique plus difficile

**Inconvénients :**
- Peut être détecté par certains antivirus
- Facilement décompressable par des outils dédiés
- Légère pénalité au démarrage

### 2. Themida / WinLicense (Windows uniquement)

Protecteurs commerciaux professionnels offrant :
- Anti-débogage avancé
- Virtualisation de code
- Chiffrement de sections
- Gestion de licences intégrée

**Note :** Coûteux mais très efficace pour les applications commerciales critiques.

### 3. Outils open source

**Objdump et binutils :**

Utiles pour vérifier ce qui est visible dans votre exécutable :

```bash
# Lister les symboles
objdump -t monprogramme.exe

# Désassembler
objdump -d monprogramme.exe

# Afficher les chaînes
strings monprogramme.exe
```

## Protection des ressources

### 1. Compression et chiffrement des ressources

```pascal
unit ResourceProtection;

interface

function LoadProtectedResource(const ResourceName: string): TStream;

implementation

uses
  Classes, SysUtils, ZStream, Base64;

function LoadProtectedResource(const ResourceName: string): TStream;  
var
  ResStream: TResourceStream;
  DecompStream: TDecompressionStream;
  TempStream: TMemoryStream;
  Encrypted: TBytes;
  i: Integer;
const
  XOR_KEY = $5A;
begin
  Result := TMemoryStream.Create;
  try
    // Charger la ressource
    ResStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
    try
      TempStream := TMemoryStream.Create;
      try
        // Déchiffrer
        SetLength(Encrypted, ResStream.Size);
        ResStream.Read(Encrypted[0], ResStream.Size);
        for i := 0 to High(Encrypted) do
          Encrypted[i] := Encrypted[i] xor XOR_KEY;
        TempStream.Write(Encrypted[0], Length(Encrypted));
        TempStream.Position := 0;

        // Décompresser
        DecompStream := TDecompressionStream.Create(TempStream);
        try
          Result.CopyFrom(DecompStream, 0);
          Result.Position := 0;
        finally
          DecompStream.Free;
        end;
      finally
        TempStream.Free;
      end;
    finally
      ResStream.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
```

### 2. Stockage externe sécurisé

Plutôt que d'intégrer des données sensibles dans l'exécutable, utilisez des fichiers externes chiffrés.

```pascal
unit SecureStorage;

interface

type
  TSecureFile = class
  private
    FPassword: string;
    function Encrypt(const Data: TBytes): TBytes;
    function Decrypt(const Data: TBytes): TBytes;
  public
    constructor Create(const Password: string);
    procedure SaveToFile(const FileName: string; const Data: TBytes);
    function LoadFromFile(const FileName: string): TBytes;
  end;

implementation

uses
  SysUtils, Classes, DCPrijndael, DCPsha256;

constructor TSecureFile.Create(const Password: string);  
begin
  inherited Create;
  FPassword := Password;
end;

function TSecureFile.Encrypt(const Data: TBytes): TBytes;  
var
  Cipher: TDCP_rijndael;
  Hash: TDCP_sha256;
  Key: array[0..31] of Byte;
begin
  // Générer une clé depuis le mot de passe
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(FPassword);
    Hash.Final(Key);
  finally
    Hash.Free;
  end;

  // Chiffrer avec AES
  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(Key, SizeOf(Key) * 8, nil);
    SetLength(Result, Length(Data));
    Cipher.EncryptCBC(Data[0], Result[0], Length(Data));
  finally
    Cipher.Free;
  end;
end;

function TSecureFile.Decrypt(const Data: TBytes): TBytes;  
var
  Cipher: TDCP_rijndael;
  Hash: TDCP_sha256;
  Key: array[0..31] of Byte;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(FPassword);
    Hash.Final(Key);
  finally
    Hash.Free;
  end;

  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(Key, SizeOf(Key) * 8, nil);
    SetLength(Result, Length(Data));
    Cipher.DecryptCBC(Data[0], Result[0], Length(Data));
  finally
    Cipher.Free;
  end;
end;

procedure TSecureFile.SaveToFile(const FileName: string; const Data: TBytes);  
var
  FS: TFileStream;
  Encrypted: TBytes;
begin
  Encrypted := Encrypt(Data);
  FS := TFileStream.Create(FileName, fmCreate);
  try
    FS.Write(Encrypted[0], Length(Encrypted));
  finally
    FS.Free;
  end;
end;

function TSecureFile.LoadFromFile(const FileName: string): TBytes;  
var
  FS: TFileStream;
  Encrypted: TBytes;
begin
  FS := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Encrypted, FS.Size);
    FS.Read(Encrypted[0], FS.Size);
    Result := Decrypt(Encrypted);
  finally
    FS.Free;
  end;
end;

end.
```

## Stratégies multi-plateformes

### Approche unifiée Windows/Ubuntu

```pascal
unit CrossPlatformProtection;

interface

function GetSystemFingerprint: string;  
function IsRunningInVM: Boolean;  
function IsDebuggerAttached: Boolean;

implementation

uses
  SysUtils
  {$IFDEF WINDOWS}, Windows, Registry{$ENDIF}
  {$IFDEF UNIX}, Unix, Process{$ENDIF};

function GetSystemFingerprint: string;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
  MachineGuid: string;
{$ENDIF}
{$IFDEF UNIX}
var
  MachineID: string;
  F: TextFile;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Utiliser le GUID machine Windows
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('SOFTWARE\Microsoft\Cryptography', False) then
    begin
      MachineGuid := Reg.ReadString('MachineGuid');
      Result := MachineGuid;
    end;
  finally
    Reg.Free;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Utiliser /etc/machine-id sur Linux
  try
    AssignFile(F, '/etc/machine-id');
    Reset(F);
    ReadLn(F, MachineID);
    CloseFile(F);
    Result := MachineID;
  except
    Result := '';
  end;
  {$ENDIF}
end;

function IsRunningInVM: Boolean;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
begin
  Result := False;

  {$IFDEF WINDOWS}
  // Détecter VMware, VirtualBox, etc.
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('HARDWARE\DESCRIPTION\System\BIOS', False) then
    begin
      Result := (Pos('VBOX', UpperCase(Reg.ReadString('SystemManufacturer'))) > 0) or
                (Pos('VMWARE', UpperCase(Reg.ReadString('SystemManufacturer'))) > 0);
    end;
  finally
    Reg.Free;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Vérifier les fichiers système caractéristiques
  Result := FileExists('/proc/vz') or
            FileExists('/proc/xen') or
            DirectoryExists('/proc/xen');
  {$ENDIF}
end;

function IsDebuggerAttached: Boolean;  
begin
  {$IFDEF WINDOWS}
  Result := Windows.IsDebuggerPresent;
  {$ENDIF}

  {$IFDEF UNIX}
  // Implémentation Linux (voir exemple précédent)
  Result := False; // Simplifiée ici
  {$ENDIF}
end;

end.
```

## Bonnes pratiques

### 1. Approche en couches

Ne comptez jamais sur une seule technique. Combinez plusieurs méthodes :

```
Niveau 1 : Strip + Optimisation
     ↓
Niveau 2 : Obfuscation des chaînes
     ↓
Niveau 3 : Anti-débogage
     ↓
Niveau 4 : Vérification d'intégrité
     ↓
Niveau 5 : Protection commerciale (optionnel)
```

### 2. Balance protection/performance

```pascal
// Configuration pour développement
{$DEFINE DEBUG_MODE}

// Configuration pour production
{$UNDEF DEBUG_MODE}
{$DEFINE PROTECTED_MODE}

procedure CriticalFunction;  
begin
  {$IFDEF PROTECTED_MODE}
  if IsDebuggerAttached then Exit;
  if not VerifyIntegrity then Exit;
  {$ENDIF}

  // Code métier

  {$IFDEF DEBUG_MODE}
  WriteLn('Debug: Function executed');
  {$ENDIF}
end;
```

### 3. Protection ciblée

Ne protégez que les parties sensibles :

```pascal
unit MyApplication;

interface

// Fonction publique : pas de protection excessive
function GetVersion: string;

// Fonction sensible : protection maximale
function {$IFDEF PROTECTED}X7A2F{$ELSE}ValidateLicense{$ENDIF}
  (const Key: string): Boolean;

implementation

function GetVersion: string;  
begin
  Result := '1.0.0';
end;

function {$IFDEF PROTECTED}X7A2F{$ELSE}ValidateLicense{$ENDIF}
  (const Key: string): Boolean;
const
  // Chaîne obfusquée
  MASTER_KEY: array[0..15] of Byte = (...);
begin
  {$IFDEF PROTECTED}
  if IsDebuggerAttached then Exit(False);
  {$ENDIF}

  // Logique de validation
  Result := CheckKey(Key, DecryptString(MASTER_KEY));
end;

end.
```

### 4. Documentation de la protection

Gardez une documentation interne des protections appliquées :

```pascal
{*******************************************************************************
  Protection appliquée sur cette unité :

  [X] Strip des symboles
  [X] Optimisation -O3
  [X] Obfuscation des noms (ValidateLicense → X7A2F)
  [X] Chiffrement des constantes sensibles
  [X] Anti-débogage (IsDebuggerAttached)
  [ ] Virtualisation de code (trop lourd)

  Version de production : Les directives {$IFDEF PROTECTED} sont activées
  Version de développement : Les directives sont désactivées pour le debug

  Dernière mise à jour : 2025-01-15
*******************************************************************************}
```

## Limitations et considérations éthiques

### Respectez les lois

- Ne violez pas les droits des utilisateurs légitimes
- Respectez le RGPD et les lois sur la protection des données
- N'utilisez pas ces techniques pour masquer des logiciels malveillants
- Fournissez toujours une option de désinstallation claire

### Avertissements aux utilisateurs

Si votre protection inclut des mécanismes intrusifs :

```pascal
// Exemple : Détection de VM
if IsRunningInVM then  
begin
  if MessageDlg('Cette application a détecté un environnement virtualisé. ' +
                'Certaines fonctionnalités peuvent être limitées pour des ' +
                'raisons de sécurité. Continuer ?',
                mtWarning, [mbYes, mbNo], 0) = mrNo then
    Halt;
end;
```

### Impact sur la maintenance

Le code obfusqué est plus difficile à maintenir. Gardez toujours :
- Le code source original non obfusqué
- Des versions de développement sans protection
- Une documentation des modifications apportées

## Checklist de protection pour la production

Avant de déployer votre application :

```
[ ] Symboles de débogage retirés (strip)
[ ] Optimisations du compilateur activées (-O3)
[ ] Chaînes sensibles chiffrées
[ ] Constantes critiques obfusquées
[ ] Anti-débogage implémenté (si nécessaire)
[ ] Vérification d'intégrité activée
[ ] Ressources sensibles protégées
[ ] Tests de fonctionnement sur OS cibles (Windows ET Ubuntu)
[ ] Documentation de protection mise à jour
[ ] Sauvegarde du code source original
[ ] Licence et conditions d'utilisation vérifiées
```

## Conclusion

L'obfuscation et la protection du code ne sont pas des solutions miracles, mais des obstacles significatifs contre la rétro-ingénierie. Pour FreePascal/Lazarus, l'approche la plus pragmatique combine :

1. **Protection de base** : Strip + optimisation (toujours)
2. **Protection moyenne** : + obfuscation des chaînes + anti-débogage (applications commerciales)
3. **Protection avancée** : + outils tiers comme UPX ou Themida (applications critiques)

Rappelez-vous que la meilleure protection reste un bon modèle économique, un excellent support client et une valeur ajoutée réelle pour vos utilisateurs. La protection technique ne doit être qu'une couche supplémentaire, pas la seule ligne de défense.

**Note pour Windows/Ubuntu** : Testez toujours vos protections sur les deux plateformes. Certains mécanismes (comme les protections commerciales) sont spécifiques à Windows et nécessitent des alternatives sous Linux.

⏭️ [Sandboxing et isolation](/17-securite-cryptographie/09-sandboxing-isolation.md)
