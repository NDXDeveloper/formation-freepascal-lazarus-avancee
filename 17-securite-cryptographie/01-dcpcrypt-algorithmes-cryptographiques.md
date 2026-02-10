🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.1 DCPCrypt et algorithmes cryptographiques

## Introduction à la cryptographie

La cryptographie est l'art de protéger des informations en les transformant de manière à ce que seules les personnes autorisées puissent les lire. Dans le développement d'applications modernes, la cryptographie est essentielle pour :

- Protéger les mots de passe des utilisateurs
- Sécuriser les communications réseau
- Chiffrer des fichiers sensibles
- Garantir l'intégrité des données
- Authentifier les utilisateurs

## Qu'est-ce que DCPCrypt ?

DCPCrypt (DCP = Developer's Cryptographic Package) est une bibliothèque cryptographique open source pour FreePascal et Lazarus. Elle fournit des implémentations d'algorithmes de chiffrement standards et éprouvés.

### Avantages de DCPCrypt

- **Multi-plateforme** : Fonctionne identiquement sur Windows, Linux et macOS
- **Open source** : Code source disponible et auditable
- **Complète** : Inclut de nombreux algorithmes de chiffrement et de hachage
- **Simple d'utilisation** : API claire et bien documentée
- **Performante** : Implémentations optimisées en Object Pascal

### Installation de DCPCrypt

#### Sur Windows et Ubuntu

DCPCrypt peut être installé via l'OPM (Online Package Manager) de Lazarus :

1. Ouvrez Lazarus
2. Allez dans **Package → Online Package Manager**
3. Recherchez "DCPcrypt"
4. Cliquez sur **Install**
5. Reconstruisez l'IDE quand demandé

Vous pouvez aussi télécharger manuellement depuis : https://github.com/graemeg/dcpcrypt

## Concepts fondamentaux de cryptographie

### Chiffrement symétrique vs asymétrique

**Chiffrement symétrique** : La même clé est utilisée pour chiffrer et déchiffrer. Rapide mais nécessite un échange sécurisé de la clé.

- Exemples : AES, DES, Blowfish, Twofish

**Chiffrement asymétrique** : Deux clés différentes (publique et privée). La clé publique chiffre, la clé privée déchiffre. Plus lent mais résout le problème d'échange de clés.

- Exemples : RSA, DSA

### Fonctions de hachage

Les fonctions de hachage transforment des données de taille variable en une empreinte de taille fixe. Elles sont :

- **Unidirectionnelles** : Impossible de retrouver les données d'origine
- **Déterministes** : Même entrée = même sortie
- **Sensibles** : Un petit changement dans l'entrée change complètement le hachage

Exemples : MD5, SHA-1, SHA-256, SHA-512

**Note importante** : MD5 et SHA-1 sont aujourd'hui considérés comme faibles. Utilisez SHA-256 ou supérieur.

## Utilisation de DCPCrypt pour le hachage

### Exemple : Hacher un mot de passe avec SHA-256

```pascal
uses
  DCPsha256, DCPcrypt2;

function HashPassword(const Password: string): string;  
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
  i: Integer;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Password);
    Hash.Final(Digest);

    // Convertir en hexadécimal
    Result := '';
    for i := 0 to 31 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
  end;
end;
```

### Explication du code

1. **Init** : Initialise l'algorithme de hachage
2. **UpdateStr** : Ajoute les données à hacher (ici le mot de passe)
3. **Final** : Calcule le hash final et le stocke dans Digest
4. La boucle convertit les bytes en représentation hexadécimale

### Vérifier un mot de passe

```pascal
function VerifyPassword(const Password, StoredHash: string): Boolean;  
var
  ComputedHash: string;
begin
  ComputedHash := HashPassword(Password);
  Result := (ComputedHash = StoredHash);
end;
```

## Chiffrement symétrique avec AES

AES (Advanced Encryption Standard) est l'algorithme de chiffrement symétrique le plus utilisé aujourd'hui. Il est rapide, sécurisé et standardisé.

### Exemple : Chiffrer et déchiffrer une chaîne

```pascal
uses
  DCPrijndael, DCPcrypt2, SysUtils;

function EncryptString(const PlainText, Password: string): string;  
var
  Cipher: TDCP_rijndael;
  KeyData: array[0..31] of Byte;
  i: Integer;
  Data: string;
begin
  // Préparer la clé (dérivée du mot de passe)
  FillChar(KeyData, SizeOf(KeyData), 0);
  for i := 1 to Length(Password) do
    if i <= 32 then
      KeyData[i-1] := Ord(Password[i]);

  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(KeyData, SizeOf(KeyData) * 8, nil);

    // Chiffrer
    Data := PlainText;
    Cipher.EncryptString(Data);

    // Encoder en Base64 pour affichage/stockage
    Result := EncodeStringBase64(Data);
  finally
    Cipher.Free;
  end;
end;

function DecryptString(const CipherText, Password: string): string;  
var
  Cipher: TDCP_rijndael;
  KeyData: array[0..31] of Byte;
  i: Integer;
  Data: string;
begin
  // Préparer la clé
  FillChar(KeyData, SizeOf(KeyData), 0);
  for i := 1 to Length(Password) do
    if i <= 32 then
      KeyData[i-1] := Ord(Password[i]);

  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(KeyData, SizeOf(KeyData) * 8, nil);

    // Décoder Base64 puis déchiffrer
    Data := DecodeStringBase64(CipherText);
    Cipher.DecryptString(Data);

    Result := Data;
  finally
    Cipher.Free;
  end;
end;
```

### Utilisation

```pascal
var
  Original, Encrypted, Decrypted: string;
begin
  Original := 'Message secret à protéger';

  // Chiffrer
  Encrypted := EncryptString(Original, 'MotDePasseTresSecurise');
  ShowMessage('Chiffré : ' + Encrypted);

  // Déchiffrer
  Decrypted := DecryptString(Encrypted, 'MotDePasseTresSecurise');
  ShowMessage('Déchiffré : ' + Decrypted);
end;
```

## Chiffrement de fichiers

### Exemple : Chiffrer un fichier complet

```pascal
uses
  DCPrijndael, DCPcrypt2, Classes;

procedure EncryptFile(const SourceFile, DestFile, Password: string);  
var
  Cipher: TDCP_rijndael;
  Source, Dest: TFileStream;
  KeyData: array[0..31] of Byte;
  i: Integer;
begin
  // Préparer la clé
  FillChar(KeyData, SizeOf(KeyData), 0);
  for i := 1 to Length(Password) do
    if i <= 32 then
      KeyData[i-1] := Ord(Password[i]);

  Source := TFileStream.Create(SourceFile, fmOpenRead);
  Dest := TFileStream.Create(DestFile, fmCreate);
  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(KeyData, SizeOf(KeyData) * 8, nil);
    Cipher.EncryptStream(Source, Dest, Source.Size);
  finally
    Cipher.Free;
    Dest.Free;
    Source.Free;
  end;
end;

procedure DecryptFile(const SourceFile, DestFile, Password: string);  
var
  Cipher: TDCP_rijndael;
  Source, Dest: TFileStream;
  KeyData: array[0..31] of Byte;
  i: Integer;
begin
  // Préparer la clé
  FillChar(KeyData, SizeOf(KeyData), 0);
  for i := 1 to Length(Password) do
    if i <= 32 then
      KeyData[i-1] := Ord(Password[i]);

  Source := TFileStream.Create(SourceFile, fmOpenRead);
  Dest := TFileStream.Create(DestFile, fmCreate);
  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(KeyData, SizeOf(KeyData) * 8, nil);
    Cipher.DecryptStream(Source, Dest, Source.Size);
  finally
    Cipher.Free;
    Dest.Free;
    Source.Free;
  end;
end;
```

## Algorithmes disponibles dans DCPCrypt

### Algorithmes de chiffrement symétrique

| Algorithme | Unité | Taille de clé | Recommandation |
|------------|-------|---------------|----------------|
| **AES (Rijndael)** | DCPrijndael | 128, 192, 256 bits | ✅ Recommandé |
| **Blowfish** | DCPblowfish | 32-448 bits | ✅ Bon |
| **Twofish** | DCPtwofish | 128, 192, 256 bits | ✅ Bon |
| **DES** | DCPdes | 56 bits | ⚠️ Obsolète |
| **3DES** | DCPdes | 168 bits | ⚠️ Lent, préférer AES |
| **CAST128** | DCPcast128 | 40-128 bits | ✅ Correct |
| **CAST256** | DCPcast256 | 128, 160, 192, 224, 256 bits | ✅ Bon |

### Algorithmes de hachage

| Algorithme | Unité | Taille du hash | Recommandation |
|------------|-------|----------------|----------------|
| **SHA-256** | DCPsha256 | 256 bits | ✅ Recommandé |
| **SHA-512** | DCPsha512 | 512 bits | ✅ Recommandé |
| **SHA-1** | DCPsha1 | 160 bits | ⚠️ Faible, éviter |
| **MD5** | DCPmd5 | 128 bits | ❌ Non sécurisé |
| **RIPEMD-160** | DCPripemd160 | 160 bits | ✅ Correct |
| **Haval** | DCPhaval | 128-256 bits | ✅ Correct |

## Bonnes pratiques de sécurité

### 1. Dérivation de clé appropriée

Ne jamais utiliser directement un mot de passe comme clé de chiffrement. Utilisez une fonction de dérivation de clé (KDF) :

```pascal
uses
  DCPsha256;

function DeriveKey(const Password: string; SaltSize: Integer): string;  
var
  Hash: TDCP_sha256;
  Salt: string;
  i: Integer;
  Digest: array[0..31] of Byte;
begin
  // En production, utilisez un vrai salt aléatoire
  Salt := 'UnSaltUniquePourCetteApplication';

  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Password + Salt);

    // Multiples itérations pour ralentir les attaques
    for i := 1 to 10000 do
    begin
      Hash.Final(Digest);
      Hash.Init;
      Hash.Update(Digest, SizeOf(Digest));
    end;

    Hash.Final(Digest);

    Result := '';
    for i := 0 to 31 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
  end;
end;
```

### 2. Toujours utiliser un vecteur d'initialisation (IV)

Pour le chiffrement en mode CBC ou CTR, utilisez un IV aléatoire unique :

```pascal
var
  IV: array[0..15] of Byte;
  i: Integer;
begin
  // Générer un IV aléatoire
  Randomize;
  for i := 0 to 15 do
    IV[i] := Random(256);

  Cipher.Init(KeyData, SizeOf(KeyData) * 8, @IV);
end;
```

### 3. Ne jamais stocker les clés en clair

- Ne jamais coder en dur les mots de passe dans le code source
- Utiliser des gestionnaires de clés du système d'exploitation
- Effacer les clés de la mémoire après utilisation

### 4. Protéger les mots de passe avec du salage

```pascal
function HashPasswordWithSalt(const Password: string): string;  
var
  Hash: TDCP_sha256;
  Salt: string;
  Digest: array[0..31] of Byte;
  i: Integer;
begin
  // Générer un salt aléatoire (en pratique, utilisez un CSPRNG)
  Salt := '';
  for i := 1 to 16 do
    Salt := Salt + Chr(Random(256));

  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Salt + Password);
    Hash.Final(Digest);

    // Stocker : salt + hash
    Result := EncodeStringBase64(Salt);
    Result := Result + ':';
    for i := 0 to 31 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
  end;
end;
```

## Différences multi-plateformes

DCPCrypt fonctionne de manière identique sur Windows et Ubuntu/Linux. Les seuls points d'attention sont :

### Encodage des chaînes

Sur Linux, les chaînes peuvent utiliser UTF-8 par défaut. Assurez-vous de la cohérence :

```pascal
{$IFDEF UNIX}
  {$CODEPAGE UTF8}
{$ENDIF}
```

### Générateurs de nombres aléatoires

Pour la génération de sel ou d'IV, préférez utiliser les API système :

**Windows** :
```pascal
uses
  Windows;

procedure GetRandomBytes(var Buffer; Size: Integer);  
var
  hProv: THandle;
begin
  CryptAcquireContext(@hProv, nil, nil, PROV_RSA_FULL, 0);
  CryptGenRandom(hProv, Size, @Buffer);
  CryptReleaseContext(hProv, 0);
end;
```

**Linux** :
```pascal
procedure GetRandomBytes(var Buffer; Size: Integer);  
var
  F: File;
begin
  AssignFile(F, '/dev/urandom');
  Reset(F, 1);
  BlockRead(F, Buffer, Size);
  CloseFile(F);
end;
```

## Exemple complet : Application de chiffrement de texte

```pascal
unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, DCPrijndael, DCPsha256;

type
  TMainForm = class(TForm)
    MemoInput: TMemo;
    MemoOutput: TMemo;
    EditPassword: TEdit;
    BtnEncrypt: TButton;
    BtnDecrypt: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure BtnEncryptClick(Sender: TObject);
    procedure BtnDecryptClick(Sender: TObject);
  private
    function DeriveKeyFromPassword(const Password: string): string;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

function TMainForm.DeriveKeyFromPassword(const Password: string): string;  
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
  i: Integer;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Password);
    Hash.Final(Digest);

    SetLength(Result, 32);
    Move(Digest[0], Result[1], 32);
  finally
    Hash.Free;
  end;
end;

procedure TMainForm.BtnEncryptClick(Sender: TObject);  
var
  Cipher: TDCP_rijndael;
  Key: string;
  Data: string;
begin
  if EditPassword.Text = '' then
  begin
    ShowMessage('Veuillez entrer un mot de passe');
    Exit;
  end;

  Key := DeriveKeyFromPassword(EditPassword.Text);
  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(Key[1], Length(Key) * 8, nil);
    Data := MemoInput.Text;
    Cipher.EncryptString(Data);
    MemoOutput.Text := EncodeStringBase64(Data);
  finally
    Cipher.Free;
  end;
end;

procedure TMainForm.BtnDecryptClick(Sender: TObject);  
var
  Cipher: TDCP_rijndael;
  Key: string;
  Data: string;
begin
  if EditPassword.Text = '' then
  begin
    ShowMessage('Veuillez entrer un mot de passe');
    Exit;
  end;

  try
    Key := DeriveKeyFromPassword(EditPassword.Text);
    Cipher := TDCP_rijndael.Create(nil);
    try
      Cipher.Init(Key[1], Length(Key) * 8, nil);
      Data := DecodeStringBase64(MemoOutput.Text);
      Cipher.DecryptString(Data);
      MemoInput.Text := Data;
    finally
      Cipher.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur de déchiffrement : ' + E.Message);
  end;
end;

end.
```

## Ressources et documentation

- **Documentation DCPCrypt** : https://github.com/graemeg/dcpcrypt
- **Tutoriel FreePascal cryptographie** : https://wiki.freepascal.org/Cryptography
- **Standards cryptographiques (NIST)** : https://csrc.nist.gov/

## Conclusion

DCPCrypt offre une solution complète et portable pour implémenter la cryptographie dans vos applications FreePascal/Lazarus. Les principes fondamentaux restent les mêmes quelle que soit la plateforme, ce qui facilite le développement d'applications sécurisées multi-OS.

Points clés à retenir :

- Utilisez SHA-256 ou supérieur pour le hachage
- Préférez AES pour le chiffrement symétrique
- Ne stockez jamais les mots de passe en clair
- Utilisez toujours du salage pour les mots de passe
- Dérivez les clés correctement à partir des mots de passe
- Testez votre code de chiffrement/déchiffrement exhaustivement

⏭️ [TLS/SSL avec OpenSSL](/17-securite-cryptographie/02-tls-ssl-openssl.md)
