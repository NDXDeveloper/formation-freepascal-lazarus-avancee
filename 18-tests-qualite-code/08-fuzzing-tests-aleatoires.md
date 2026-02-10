🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.8 Fuzzing et tests aléatoires

## Introduction

Le **fuzzing** (ou test par injection de données aléatoires) est une technique de test qui consiste à envoyer des données invalides, inattendues ou aléatoires à un programme pour détecter des bugs, des failles de sécurité ou des comportements anormaux.

### Analogie simple

Imaginez que vous testez une serrure :
- **Tests classiques** : Vous essayez les bonnes clés
- **Fuzzing** : Vous essayez des tournevis, des trombones, de l'eau, du sable, etc. pour voir si la serrure résiste ou casse

### Pourquoi faire du fuzzing ?

1. **Découvrir des bugs cachés** : Trouver des cas limites non testés
2. **Améliorer la sécurité** : Détecter les vulnérabilités avant les attaquants
3. **Tester la robustesse** : Vérifier que le programme ne plante pas avec des entrées invalides
4. **Validation d'entrées** : S'assurer que toutes les entrées sont correctement validées
5. **Tests exhaustifs** : Couvrir des cas impossibles à imaginer manuellement

## Types de fuzzing

### 1. Fuzzing par mutation (Mutation-based)

Modifier des données valides pour créer des entrées invalides.

**Exemple :**
```
Donnée valide:    "John Doe"
Après mutation:   "John\x00Doe"      (caractère nul)
                  "John' OR '1'='1"  (injection SQL)
                  "John" + "D"*10000 (overflow)
```

### 2. Fuzzing par génération (Generation-based)

Créer des données complètement aléatoires selon des règles.

**Exemple :**
```
Email valide:     "user@example.com"
Génération:       "a@b"
                  "@@@@@"
                  "user@.com"
                  "user@@domain"
```

### 3. Fuzzing intelligent (Smart fuzzing)

Utiliser des connaissances du format pour créer des données presque valides.

**Exemple pour un parser JSON :**
```json
{"name": "John"}           // Valide
{"name": "John"            // Parenthèse manquante
{"name": 12345}            // Type incorrect
{name: "John"}             // Guillemets manquants
```

## Fuzzing basique en FreePascal

### Exemple 1 : Fuzzer simple pour fonction de calcul

```pascal
program SimpleFuzzer;

{$mode objfpc}{$H+}

uses
  SysUtils;

// Fonction à tester
function Diviser(a, b: Integer): Double;
begin
  Result := a / b;  // Bug potentiel: division par zéro
end;

procedure FuzzDiviser;
var
  i: Integer;
  a, b: Integer;
  resultat: Double;
  erreurs: Integer;
begin
  WriteLn('=== Fuzzing de la fonction Diviser ===');
  WriteLn;

  erreurs := 0;
  Randomize;

  for i := 1 to 1000 do
  begin
    // Générer des valeurs aléatoires
    a := Random(1000) - 500;  // -500 à 499
    b := Random(1000) - 500;

    try
      resultat := Diviser(a, b);

      // Vérifier le résultat
      if IsNan(resultat) or IsInfinite(resultat) then
      begin
        WriteLn('⚠️  Cas limite détecté: ', a, ' / ', b, ' = ', resultat:0:2);
        Inc(erreurs);
      end;

    except
      on E: Exception do
      begin
        WriteLn('❌ ERREUR avec a=', a, ' b=', b, ': ', E.Message);
        Inc(erreurs);
      end;
    end;
  end;

  WriteLn;
  WriteLn('Tests effectués: 1000');
  WriteLn('Erreurs trouvées: ', erreurs);

  if erreurs > 0 then
    WriteLn('⚠️  La fonction nécessite une amélioration!')
  else
    WriteLn('✓ Aucune erreur détectée');
end;

begin
  FuzzDiviser;
end.
```

**Sortie typique :**
```
=== Fuzzing de la fonction Diviser ===

❌ ERREUR avec a=123 b=0: Division by zero
❌ ERREUR avec a=-45 b=0: Division by zero
⚠️  Cas limite détecté: 500 / 1 = Inf

Tests effectués: 1000
Erreurs trouvées: 15
⚠️  La fonction nécessite une amélioration!
```

**Version corrigée :**

```pascal
function DiviserSecurise(a, b: Integer): Double;
begin
  if b = 0 then
    raise Exception.Create('Division par zéro interdite');

  Result := a / b;
end;
```

### Exemple 2 : Fuzzer pour parser de chaînes

```pascal
program StringParserFuzzer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

// Fonction à tester : Parser une liste de nombres séparés par des virgules
function ParseNumberList(const input: string): TStringList;
var
  parts: TStringList;
  i: Integer;
begin
  Result := TStringList.Create;

  // Bug potentiel : pas de validation
  parts := TStringList.Create;
  try
    parts.Delimiter := ',';
    parts.StrictDelimiter := True;
    parts.DelimitedText := input;
    for i := 0 to parts.Count - 1 do
      Result.Add(IntToStr(StrToInt(Trim(parts[i]))));
  finally
    parts.Free;
  end;
end;

procedure FuzzParser;
var
  testCases: array of string;
  i: Integer;
  result: TStringList;
  erreurs: Integer;
begin
  WriteLn('=== Fuzzing du parser de nombres ===');
  WriteLn;

  // Préparer des cas de test malveillants
  SetLength(testCases, 15);
  testCases[0] := '1,2,3';           // Valide
  testCases[1] := '';                // Vide
  testCases[2] := ',,,';             // Seulement virgules
  testCases[3] := '1,,3';            // Virgule double
  testCases[4] := 'abc,def';         // Texte au lieu de nombres
  testCases[5] := '1,2,';            // Virgule finale
  testCases[6] := ',1,2';            // Virgule initiale
  testCases[7] := '999999999999999'; // Très grand nombre
  testCases[8] := '1 2 3';           // Espaces au lieu de virgules
  testCases[9] := '1;2;3';           // Mauvais séparateur
  testCases[10] := '1.5,2.7';        // Nombres décimaux
  testCases[11] := '-1,-2,-3';       // Nombres négatifs
  testCases[12] := '0x10,0x20';      // Hexadécimal
  testCases[13] := '1e10,2e10';      // Notation scientifique
  testCases[14] := StringOfChar('1', 10000); // Très longue chaîne

  erreurs := 0;

  for i := 0 to High(testCases) do
  begin
    Write('Test ', i + 1:2, ': ');

    // Afficher un extrait de l'entrée
    if Length(testCases[i]) > 30 then
      Write('"', Copy(testCases[i], 1, 27), '..."')
    else
      Write('"', testCases[i], '"');

    try
      result := ParseNumberList(testCases[i]);
      try
        WriteLn(' → OK (', result.Count, ' éléments)');
      finally
        result.Free;
      end;
    except
      on E: Exception do
      begin
        WriteLn(' → ❌ ERREUR: ', E.Message);
        Inc(erreurs);
      end;
    end;
  end;

  WriteLn;
  WriteLn('Tests effectués: ', Length(testCases));
  WriteLn('Erreurs trouvées: ', erreurs);

  if erreurs > 0 then
    WriteLn('⚠️  Le parser doit être renforcé!')
  else
    WriteLn('✓ Parser robuste');
end;

begin
  FuzzParser;
end.
```

**Sortie :**
```
=== Fuzzing du parser de nombres ===

Test  1: "1,2,3" → OK (3 éléments)
Test  2: "" → ❌ ERREUR: '' is not a valid integer value
Test  3: ",,," → ❌ ERREUR: '' is not a valid integer value
Test  4: "1,,3" → ❌ ERREUR: '' is not a valid integer value
Test  5: "abc,def" → ❌ ERREUR: 'abc' is not a valid integer value
Test  6: "1,2," → ❌ ERREUR: '' is not a valid integer value
...

Tests effectués: 15
Erreurs trouvées: 12
⚠️  Le parser doit être renforcé!
```

**Version sécurisée :**

```pascal
function ParseNumberListSecure(const input: string): TStringList;
var
  parts: TStringList;
  i, num: Integer;
  part: string;
begin
  Result := TStringList.Create;

  if Trim(input) = '' then
    Exit;

  parts := TStringList.Create;
  try
    parts.Delimiter := ',';
    parts.StrictDelimiter := True;
    parts.DelimitedText := input;
    for i := 0 to parts.Count - 1 do
    begin
      part := Trim(parts[i]);
      if part = '' then
        Continue;  // Ignorer les parties vides

      if not TryStrToInt(part, num) then
        raise Exception.CreateFmt('Valeur invalide: "%s"', [part]);

      Result.Add(IntToStr(num));
    end;
  finally
    parts.Free;
  end;
end;
```

## Générateur de données aléatoires

### Exemple 3 : Générateur de chaînes aléatoires

```pascal
unit FuzzGenerators;

{$mode objfpc}{$H+}

interface

type
  TFuzzStringType = (
    fstAlphanumeric,    // Lettres et chiffres
    fstPrintable,       // Caractères imprimables
    fstBinary,          // Données binaires
    fstUnicode,         // Unicode
    fstSpecialChars,    // Caractères spéciaux
    fstSQLInjection,    // Tentatives d'injection SQL
    fstPathTraversal,   // Tentatives de path traversal
    fstOverflow         // Chaînes très longues
  );

function GenerateRandomString(StringType: TFuzzStringType; MinLen, MaxLen: Integer): string;
function GenerateRandomInteger(Min, Max: Integer): Integer;
function GenerateRandomFloat(Min, Max: Double): Double;
function GenerateRandomBoolean: Boolean;

implementation

uses
  SysUtils;

function GenerateRandomInteger(Min, Max: Integer): Integer;
begin
  Result := Random(Max - Min + 1) + Min;
end;

function GenerateRandomFloat(Min, Max: Double): Double;
begin
  Result := Min + Random * (Max - Min);
end;

function GenerateRandomBoolean: Boolean;
begin
  Result := Random(2) = 1;
end;

function GenerateRandomString(StringType: TFuzzStringType; MinLen, MaxLen: Integer): string;
const
  ALPHANUMERIC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  SPECIAL_CHARS = '!@#$%^&*()_+-=[]{}|;:''",.<>?/\~`';
  SQL_PATTERNS: array[0..4] of string = (
    ''' OR ''1''=''1',
    '; DROP TABLE users--',
    ''' UNION SELECT * FROM passwords--',
    'admin''--',
    '1'' OR ''1''=''1''--'
  );
  PATH_PATTERNS: array[0..3] of string = (
    '../../../etc/passwd',
    '..\..\..\..\windows\system32\config\sam',
    '....//....//....//etc/passwd',
    '%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd'
  );
var
  len, i: Integer;
  charset: string;
begin
  len := GenerateRandomInteger(MinLen, MaxLen);
  Result := '';

  case StringType of
    fstAlphanumeric:
    begin
      for i := 1 to len do
        Result := Result + ALPHANUMERIC[Random(Length(ALPHANUMERIC)) + 1];
    end;

    fstPrintable:
    begin
      for i := 1 to len do
        Result := Result + Chr(Random(95) + 32); // ASCII 32-126
    end;

    fstBinary:
    begin
      for i := 1 to len do
        Result := Result + Chr(Random(256)); // ASCII 0-255
    end;

    fstUnicode:
    begin
      // Quelques caractères Unicode courants
      for i := 1 to len do
        Result := Result + WideChar(Random($1000) + $80);
    end;

    fstSpecialChars:
    begin
      for i := 1 to len do
        Result := Result + SPECIAL_CHARS[Random(Length(SPECIAL_CHARS)) + 1];
    end;

    fstSQLInjection:
    begin
      Result := SQL_PATTERNS[Random(Length(SQL_PATTERNS))];
    end;

    fstPathTraversal:
    begin
      Result := PATH_PATTERNS[Random(Length(PATH_PATTERNS))];
    end;

    fstOverflow:
    begin
      len := MaxLen * 10; // 10x la taille max attendue
      Result := StringOfChar('A', len);
    end;
  end;
end;

end.
```

**Utilisation :**

```pascal
program TestFuzzGenerators;

{$mode objfpc}{$H+}

uses
  SysUtils, FuzzGenerators;

var
  i: Integer;
  s: string;

begin
  Randomize;

  WriteLn('=== Générateurs de fuzzing ===');
  WriteLn;

  WriteLn('Chaînes alphanumériques:');
  for i := 1 to 5 do
  begin
    s := GenerateRandomString(fstAlphanumeric, 5, 15);
    WriteLn('  ', s);
  end;
  WriteLn;

  WriteLn('Tentatives d''injection SQL:');
  for i := 1 to 3 do
  begin
    s := GenerateRandomString(fstSQLInjection, 0, 0);
    WriteLn('  ', s);
  end;
  WriteLn;

  WriteLn('Tentatives de path traversal:');
  for i := 1 to 3 do
  begin
    s := GenerateRandomString(fstPathTraversal, 0, 0);
    WriteLn('  ', s);
  end;
  WriteLn;

  WriteLn('Caractères spéciaux:');
  for i := 1 to 3 do
  begin
    s := GenerateRandomString(fstSpecialChars, 10, 20);
    WriteLn('  ', s);
  end;
end.
```

## Fuzzing de fichiers

### Exemple 4 : Fuzzer pour parser de fichiers

```pascal
program FileFuzzer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

// Fonction à tester : Parser un fichier de configuration
procedure LoadConfigFile(const FileName: string);
var
  lines: TStringList;
  i: Integer;
  line, key, value: string;
  pos: Integer;
begin
  lines := TStringList.Create;
  try
    lines.LoadFromFile(FileName);

    for i := 0 to lines.Count - 1 do
    begin
      line := Trim(lines[i]);

      // Ignorer les commentaires et lignes vides
      if (line = '') or (line[1] = '#') then
        Continue;

      // Bug potentiel : pas de vérification de la présence de '='
      pos := Pos('=', line);
      key := Copy(line, 1, pos - 1);
      value := Copy(line, pos + 1, Length(line));

      WriteLn('Config: ', key, ' = ', value);
    end;
  finally
    lines.Free;
  end;
end;

procedure GenerateFuzzedConfigFile(const FileName: string; TestCase: Integer);
var
  f: TextFile;
begin
  AssignFile(f, FileName);
  Rewrite(f);
  try
    case TestCase of
      1: // Fichier vide
        ;

      2: // Configuration valide
      begin
        WriteLn(f, 'server=localhost');
        WriteLn(f, 'port=8080');
        WriteLn(f, 'debug=true');
      end;

      3: // Ligne sans '='
      begin
        WriteLn(f, 'server=localhost');
        WriteLn(f, 'invalid_line');
        WriteLn(f, 'port=8080');
      end;

      4: // Multiples '='
      begin
        WriteLn(f, 'url=http://example.com/path?param=value');
      end;

      5: // Caractères spéciaux
      begin
        WriteLn(f, 'password=p@$$w0rd!#%');
        WriteLn(f, 'path=C:\Program Files\App');
      end;

      6: // Lignes très longues
      begin
        WriteLn(f, 'long_value=', StringOfChar('A', 10000));
      end;

      7: // Valeurs vides
      begin
        WriteLn(f, 'empty=');
        WriteLn(f, '=value_without_key');
      end;

      8: // Commentaires bizarres
      begin
        WriteLn(f, '# Commentaire normal');
        WriteLn(f, '#=#=');
        WriteLn(f, '### ');
      end;

      9: // Unicode
      begin
        WriteLn(f, 'unicode=Héllo Wörld 日本語');
      end;

      10: // Caractères de contrôle
      begin
        WriteLn(f, 'control=', #0, #1, #2, #13, #10);
      end;
    end;
  finally
    CloseFile(f);
  end;
end;

var
  i: Integer;
  testFile: string;

begin
  WriteLn('=== Fuzzing de parser de fichier de configuration ===');
  WriteLn;

  testFile := 'fuzz_config.txt';

  for i := 1 to 10 do
  begin
    WriteLn('Test case ', i, ':');

    try
      GenerateFuzzedConfigFile(testFile, i);

      try
        LoadConfigFile(testFile);
        WriteLn('  ✓ Pas d''erreur');
      except
        on E: Exception do
          WriteLn('  ❌ ERREUR: ', E.Message);
      end;

    finally
      if FileExists(testFile) then
        DeleteFile(testFile);
    end;

    WriteLn;
  end;
end.
```

## Fuzzing de protocoles réseau

### Exemple 5 : Fuzzer pour serveur HTTP

```pascal
program HTTPServerFuzzer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Sockets;

function SendHTTPRequest(const Host: string; Port: Word; const Request: string): string;
var
  sock: TSocket;
  addr: TInetSockAddr;
  buffer: array[0..1023] of Char;
  bytesRead: Integer;
begin
  Result := '';
  sock := fpSocket(AF_INET, SOCK_STREAM, 0);

  if sock < 0 then
    raise Exception.Create('Impossible de créer le socket');

  try
    addr.sin_family := AF_INET;
    addr.sin_port := htons(Port);
    addr.sin_addr := StrToNetAddr(Host);

    if fpConnect(sock, @addr, SizeOf(addr)) < 0 then
      raise Exception.Create('Connexion échouée');

    // Envoyer la requête
    if fpSend(sock, @Request[1], Length(Request), 0) < 0 then
      raise Exception.Create('Envoi échoué');

    // Lire la réponse
    bytesRead := fpRecv(sock, @buffer, SizeOf(buffer), 0);
    if bytesRead > 0 then
      SetString(Result, buffer, bytesRead);

  finally
    CloseSocket(sock);
  end;
end;

procedure FuzzHTTPServer(const Host: string; Port: Word);
var
  testCases: array of string;
  i: Integer;
  response: string;
  erreurs, crashes: Integer;
begin
  WriteLn('=== Fuzzing de serveur HTTP ===');
  WriteLn('Cible: ', Host, ':', Port);
  WriteLn;

  // Préparer les cas de test
  SetLength(testCases, 12);

  // Requêtes valides et invalides
  testCases[0] := 'GET / HTTP/1.1'#13#10'Host: localhost'#13#10#13#10;
  testCases[1] := 'GET /../../../etc/passwd HTTP/1.1'#13#10'Host: localhost'#13#10#13#10;
  testCases[2] := 'GET / HTTP/999.999'#13#10'Host: localhost'#13#10#13#10;
  testCases[3] := StringOfChar('A', 10000); // Requête énorme
  testCases[4] := 'INVALID_METHOD / HTTP/1.1'#13#10'Host: localhost'#13#10#13#10;
  testCases[5] := 'GET /'#0#0#0' HTTP/1.1'#13#10; // Caractères nuls
  testCases[6] := ''; // Requête vide
  testCases[7] := #13#10#13#10#13#10; // Seulement des retours à la ligne
  testCases[8] := 'GET / HTTP/1.1'#13#10'Host: ' + StringOfChar('x', 1000) + #13#10#13#10;
  testCases[9] := 'GET /<script>alert(1)</script> HTTP/1.1'#13#10'Host: localhost'#13#10#13#10;
  testCases[10] := 'GET / HTTP/1.1'#13#10'Header-Without-Value'#13#10#13#10;
  testCases[11] := 'POST / HTTP/1.1'#13#10'Content-Length: -1'#13#10#13#10;

  erreurs := 0;
  crashes := 0;

  for i := 0 to High(testCases) do
  begin
    Write('Test ', i + 1:2, ': ');

    try
      response := SendHTTPRequest(Host, Port, testCases[i]);

      if Length(response) > 0 then
        WriteLn('✓ Réponse reçue (', Length(response), ' bytes)')
      else
        WriteLn('⚠️  Pas de réponse');

    except
      on E: Exception do
      begin
        WriteLn('❌ ERREUR: ', E.Message);
        Inc(erreurs);

        // Vérifier si le serveur a planté
        Sleep(1000);
        try
          SendHTTPRequest(Host, Port, testCases[0]);
        except
          WriteLn('💥 CRASH: Le serveur ne répond plus!');
          Inc(crashes);
          Break;
        end;
      end;
    end;

    Sleep(100); // Pause entre les requêtes
  end;

  WriteLn;
  WriteLn('Tests effectués: ', i + 1);
  WriteLn('Erreurs: ', erreurs);
  WriteLn('Crashes: ', crashes);

  if crashes > 0 then
    WriteLn('💥 CRITIQUE: Le serveur a planté!')
  else if erreurs > 0 then
    WriteLn('⚠️  Vulnérabilités potentielles détectées')
  else
    WriteLn('✓ Serveur robuste');
end;

begin
  FuzzHTTPServer('127.0.0.1', 8080);
end.
```

## Tests basés sur les propriétés (Property-Based Testing)

### Exemple 6 : Vérification de propriétés mathématiques

```pascal
program PropertyBasedTesting;

{$mode objfpc}{$H+}

uses
  SysUtils;

// Fonction à tester
function Abs(x: Integer): Integer;
begin
  if x < 0 then
    Result := -x
  else
    Result := x;
end;

// Propriété 1: Abs(x) >= 0 pour tout x
function PropertyAbsAlwaysPositive: Boolean;
var
  i, x, result: Integer;
begin
  Result := True;
  for i := 1 to 1000 do
  begin
    x := Random(20000) - 10000; // -10000 à 9999
    result := Abs(x);
    if result < 0 then
    begin
      WriteLn('❌ Propriété violée: Abs(', x, ') = ', result, ' < 0');
      Exit(False);
    end;
  end;
end;

// Propriété 2: Abs(Abs(x)) = Abs(x) (idempotence)
function PropertyAbsIdempotent: Boolean;
var
  i, x, abs1, abs2: Integer;
begin
  Result := True;
  for i := 1 to 1000 do
  begin
    x := Random(20000) - 10000;
    abs1 := Abs(x);
    abs2 := Abs(abs1);
    if abs1 <> abs2 then
    begin
      WriteLn('❌ Propriété violée: Abs(Abs(', x, ')) ≠ Abs(', x, ')');
      Exit(False);
    end;
  end;
end;

// Propriété 3: Abs(-x) = Abs(x) (symétrie)
function PropertyAbsSymmetric: Boolean;
var
  i, x, absX, absNegX: Integer;
begin
  Result := True;
  for i := 1 to 1000 do
  begin
    x := Random(20000) - 10000;
    absX := Abs(x);
    absNegX := Abs(-x);
    if absX <> absNegX then
    begin
      WriteLn('❌ Propriété violée: Abs(', x, ') ≠ Abs(', -x, ')');
      Exit(False);
    end;
  end;
end;

// Propriété 4: Abs(x * y) = Abs(x) * Abs(y)
function PropertyAbsMultiplicative: Boolean;
var
  i, x, y: Integer;
  absXY, absXAbsY: Int64;
begin
  Result := True;
  for i := 1 to 1000 do
  begin
    x := Random(1000) - 500;
    y := Random(1000) - 500;
    absXY := Abs(x * y);
    absXAbsY := Int64(Abs(x)) * Int64(Abs(y));
    if absXY <> absXAbsY then
    begin
      WriteLn('❌ Propriété violée: Abs(', x, ' * ', y, ') ≠ Abs(', x, ') * Abs(', y, ')');
      Exit(False);
    end;
  end;
end;

begin
  WriteLn('=== Test basé sur les propriétés ===');
  WriteLn('Fonction testée: Abs(x)');
  WriteLn;

  Randomize;

  Write('Propriété 1 (toujours positif):     ');
  if PropertyAbsAlwaysPositive then
    WriteLn('✓ PASS')
  else
    WriteLn('❌ FAIL');

  Write('Propriété 2 (idempotence):          ');
  if PropertyAbsIdempotent then
    WriteLn('✓ PASS')
  else
    WriteLn('❌ FAIL');

  Write('Propriété 3 (symétrie):             ');
  if PropertyAbsSymmetric then
    WriteLn('✓ PASS')
  else
    WriteLn('❌ FAIL');

  Write('Propriété 4 (multiplicative):       ');
  if PropertyAbsMultiplicative then
    WriteLn('✓ PASS')
  else
    WriteLn('❌ FAIL');
end.
```

## Framework de fuzzing réutilisable

### Exemple 7 : Framework générique

```pascal
unit FuzzFramework;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants;

type
  TFuzzTestResult = (ftrPass, ftrFail, ftrCrash);

  TFuzzTestCase = record
    Input: string;
    ExpectedResult: TFuzzTestResult;
    ActualResult: TFuzzTestResult;
    ErrorMessage: string;
  end;

  TFuzzTestFunc = function(const Input: string): Boolean;

  TFuzzRunner = class
  private
    FTestCases: array of TFuzzTestCase;
    FTestFunction: TFuzzTestFunc;
    FTotalTests: Integer;
    FPassedTests: Integer;
    FFailedTests: Integer;
    FCrashedTests: Integer;
  public
    constructor Create(TestFunc: TFuzzTestFunc);
    procedure AddTestCase(const Input: string; Expected: TFuzzTestResult);
    procedure GenerateRandomTests(Count: Integer);
    procedure RunAllTests;
    procedure PrintReport;
    property TotalTests: Integer read FTotalTests;
    property PassedTests: Integer read FPassedTests;
    property FailedTests: Integer read FFailedTests;
    property CrashedTests: Integer read FCrashedTests;
  end;

implementation

uses
  FuzzGenerators;

constructor TFuzzRunner.Create(TestFunc: TFuzzTestFunc);
begin
  inherited Create;
  FTestFunction := TestFunc;
  FTotalTests := 0;
  FPassedTests := 0;
  FFailedTests := 0;
  FCrashedTests := 0;
end;

procedure TFuzzRunner.AddTestCase(const Input: string; Expected: TFuzzTestResult);
var
  idx: Integer;
begin
  idx := Length(FTestCases);
  SetLength(FTestCases, idx + 1);
  FTestCases[idx].Input := Input;
  FTestCases[idx].ExpectedResult := Expected;
end;

procedure TFuzzRunner.GenerateRandomTests(Count: Integer);
var
  i: Integer;
  testType: TFuzzStringType;
begin
  Randomize;
  for i := 1 to Count do
  begin
    // Choisir un type de test aléatoire
    testType := TFuzzStringType(Random(Ord(High(TFuzzStringType)) + 1));
    AddTestCase(GenerateRandomString(testType, 1, 100), ftrPass);
  end;
end;

procedure TFuzzRunner.RunAllTests;
var
  i: Integer;
  result: Boolean;
begin
  WriteLn('Exécution de ', Length(FTestCases), ' tests...');
  WriteLn;

  for i := 0 to High(FTestCases) do
  begin
    Inc(FTotalTests);

    try
      result := FTestFunction(FTestCases[i].Input);

      if result then
        FTestCases[i].ActualResult := ftrPass
      else
        FTestCases[i].ActualResult := ftrFail;

    except
      on E: Exception do
      begin
        FTestCases[i].ActualResult := ftrCrash;
        FTestCases[i].ErrorMessage := E.Message;
      end;
    end;

    // Compter les résultats
    case FTestCases[i].ActualResult of
      ftrPass: Inc(FPassedTests);
      ftrFail: Inc(FFailedTests);
      ftrCrash: Inc(FCrashedTests);
    end;

    // Afficher la progression
    if (i + 1) mod 100 = 0 then
      Write('.');
  end;

  WriteLn;
  WriteLn;
end;

procedure TFuzzRunner.PrintReport;
var
  i: Integer;
begin
  WriteLn('=== Rapport de Fuzzing ===');
  WriteLn;
  WriteLn('Tests exécutés: ', FTotalTests);
  WriteLn('Réussis:        ', FPassedTests, ' (', (FPassedTests * 100) div FTotalTests, '%)');
  WriteLn('Échoués:        ', FFailedTests, ' (', (FFailedTests * 100) div FTotalTests, '%)');
  WriteLn('Crashes:        ', FCrashedTests, ' (', (FCrashedTests * 100) div FTotalTests, '%)');
  WriteLn;

  // Afficher les crashes
  if FCrashedTests > 0 then
  begin
    WriteLn('=== Détails des crashes ===');
    for i := 0 to High(FTestCases) do
    begin
      if FTestCases[i].ActualResult = ftrCrash then
      begin
        WriteLn('Input: "', Copy(FTestCases[i].Input, 1, 50),
                IfThen(Length(FTestCases[i].Input) > 50, '..."', '"'));
        WriteLn('Erreur: ', FTestCases[i].ErrorMessage);
        WriteLn;
      end;
    end;
  end;

  // Verdict final
  if FCrashedTests > 0 then
    WriteLn('❌ CRITIQUE: Des crashes ont été détectés!')
  else if FFailedTests > (FTotalTests div 10) then
    WriteLn('⚠️  ATTENTION: Taux d''échec élevé')
  else
    WriteLn('✓ Tests passés avec succès');
end;

end.
```

**Utilisation du framework :**

```pascal
program UseFuzzFramework;

{$mode objfpc}{$H+}

uses
  SysUtils, FuzzFramework, FuzzGenerators;

// Fonction à tester : Validation d'email
function ValidateEmail(const email: string): Boolean;
var
  atPos, dotPos: Integer;
begin
  Result := False;

  // Vérifications basiques
  if Length(email) < 5 then
    Exit;

  atPos := Pos('@', email);
  if atPos <= 1 then
    Exit;

  dotPos := Pos('.', email);
  if dotPos <= atPos + 1 then
    Exit;

  if dotPos >= Length(email) then
    Exit;

  Result := True;
end;

// Wrapper pour le framework
function TestEmailValidation(const input: string): Boolean;
begin
  try
    Result := ValidateEmail(input);
  except
    raise; // Propager l'exception pour la détecter comme crash
  end;
end;

var
  fuzzer: TFuzzRunner;

begin
  WriteLn('=== Fuzzing de la validation d''email ===');
  WriteLn;

  fuzzer := TFuzzRunner.Create(@TestEmailValidation);
  try
    // Ajouter des cas de test manuels
    fuzzer.AddTestCase('user@example.com', ftrPass);
    fuzzer.AddTestCase('invalid', ftrPass);
    fuzzer.AddTestCase('', ftrPass);
    fuzzer.AddTestCase('@example.com', ftrPass);
    fuzzer.AddTestCase('user@', ftrPass);
    fuzzer.AddTestCase('user@.com', ftrPass);

    // Générer des tests aléatoires
    fuzzer.GenerateRandomTests(1000);

    // Exécuter
    fuzzer.RunAllTests;

    // Rapport
    fuzzer.PrintReport;

  finally
    fuzzer.Free;
  end;
end.
```

## Fuzzing de formats de fichiers

### Exemple 8 : Fuzzer pour parser XML

```pascal
program XMLFuzzer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DOM, XMLRead;

procedure TestXMLParser(const xmlContent: string);
var
  doc: TXMLDocument;
  stream: TStringStream;
begin
  stream := TStringStream.Create(xmlContent);
  try
    ReadXMLFile(doc, stream);
    try
      // Si on arrive ici, le parsing a réussi
      WriteLn('✓ XML valide');
    finally
      doc.Free;
    end;
  finally
    stream.Free;
  end;
end;

procedure FuzzXMLParser;
var
  testCases: array of string;
  i: Integer;
begin
  WriteLn('=== Fuzzing du parser XML ===');
  WriteLn;

  SetLength(testCases, 15);

  // XML valide
  testCases[0] := '<?xml version="1.0"?><root><item>Test</item></root>';

  // Balise non fermée
  testCases[1] := '<?xml version="1.0"?><root><item>Test</root>';

  // Balise orpheline fermante
  testCases[2] := '<?xml version="1.0"?><root></item></root>';

  // Attributs malformés
  testCases[3] := '<?xml version="1.0"?><root attr="value></root>';
  testCases[4] := '<?xml version="1.0"?><root attr=value"></root>';

  // Caractères spéciaux non échappés
  testCases[5] := '<?xml version="1.0"?><root><item>< & ></item></root>';

  // XML vide
  testCases[6] := '';

  // XML sans déclaration
  testCases[7] := '<root><item>Test</item></root>';

  // XML avec CDATA
  testCases[8] := '<?xml version="1.0"?><root><![CDATA[<test>]]></root>';

  // Entités non définies
  testCases[9] := '<?xml version="1.0"?><root>&undefined;</root>';

  // XML énorme
  testCases[10] := '<?xml version="1.0"?><root>' +
                   StringOfChar('a', 1000000) + '</root>';

  // Imbrication profonde
  testCases[11] := '<?xml version="1.0"?>' +
                   StringOfChar('<a>', 1000) + 'test' +
                   StringOfChar('</a>', 1000);

  // Encodage invalide
  testCases[12] := '<?xml version="1.0" encoding="UTF-999"?><root></root>';

  // Commentaires malformés
  testCases[13] := '<?xml version="1.0"?><root><!-- comment --></root>';
  testCases[14] := '<?xml version="1.0"?><root><!- invalid --></root>';

  for i := 0 to High(testCases) do
  begin
    Write('Test ', i + 1:2, ': ');

    try
      TestXMLParser(testCases[i]);
    except
      on E: Exception do
        WriteLn('❌ ERREUR: ', E.Message);
    end;
  end;
end;

begin
  FuzzXMLParser;
end.
```

## Fuzzing de sécurité

### Exemple 9 : Tests de sécurité communs

```pascal
program SecurityFuzzer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TSecurityTest = record
    Name: string;
    TestData: string;
    Category: string;
  end;

// Fonction à tester : Nettoyage de chaînes SQL
function SanitizeSQLInput(const input: string): string;
begin
  // Implémentation simple (potentiellement vulnérable)
  Result := StringReplace(input, '''', '''''', [rfReplaceAll]);
end;

procedure RunSecurityFuzz;
var
  tests: array of TSecurityTest;
  i: Integer;
  sanitized: string;
begin
  WriteLn('=== Tests de sécurité par fuzzing ===');
  WriteLn;

  // Préparer les tests
  SetLength(tests, 20);

  // Injection SQL
  tests[0].Name := 'SQL Injection - OR 1=1';
  tests[0].TestData := ''' OR ''1''=''1';
  tests[0].Category := 'SQL';

  tests[1].Name := 'SQL Injection - DROP TABLE';
  tests[1].TestData := '''; DROP TABLE users--';
  tests[1].Category := 'SQL';

  tests[2].Name := 'SQL Injection - UNION';
  tests[2].TestData := ''' UNION SELECT * FROM passwords--';
  tests[2].Category := 'SQL';

  tests[3].Name := 'SQL Injection - Commentaire';
  tests[3].TestData := 'admin''--';
  tests[3].Category := 'SQL';

  // XSS (Cross-Site Scripting)
  tests[4].Name := 'XSS - Script simple';
  tests[4].TestData := '<script>alert(1)</script>';
  tests[4].Category := 'XSS';

  tests[5].Name := 'XSS - Event handler';
  tests[5].TestData := '<img src=x onerror=alert(1)>';
  tests[5].Category := 'XSS';

  tests[6].Name := 'XSS - JavaScript URL';
  tests[6].TestData := 'javascript:alert(1)';
  tests[6].Category := 'XSS';

  // Path Traversal
  tests[7].Name := 'Path Traversal - Unix';
  tests[7].TestData := '../../../etc/passwd';
  tests[7].Category := 'Path';

  tests[8].Name := 'Path Traversal - Windows';
  tests[8].TestData := '..\..\..\windows\system32\config\sam';
  tests[8].Category := 'Path';

  tests[9].Name := 'Path Traversal - Encodé';
  tests[9].TestData := '%2e%2e%2f%2e%2e%2fetc%2fpasswd';
  tests[9].Category := 'Path';

  // Command Injection
  tests[10].Name := 'Command Injection - Pipe';
  tests[10].TestData := 'test | rm -rf /';
  tests[10].Category := 'Command';

  tests[11].Name := 'Command Injection - Semicolon';
  tests[11].TestData := 'test; cat /etc/passwd';
  tests[11].Category := 'Command';

  tests[12].Name := 'Command Injection - Backtick';
  tests[12].TestData := 'test`whoami`';
  tests[12].Category := 'Command';

  // LDAP Injection
  tests[13].Name := 'LDAP Injection';
  tests[13].TestData := '*)(uid=*))(|(uid=*';
  tests[13].Category := 'LDAP';

  // XML Injection
  tests[14].Name := 'XML Injection - Entity';
  tests[14].TestData := '<?xml version="1.0"?><!DOCTYPE foo [<!ENTITY xxe SYSTEM "file:///etc/passwd">]><foo>&xxe;</foo>';
  tests[14].Category := 'XML';

  // Buffer Overflow
  tests[15].Name := 'Buffer Overflow - Long string';
  tests[15].TestData := StringOfChar('A', 10000);
  tests[15].Category := 'Buffer';

  // Format String
  tests[16].Name := 'Format String';
  tests[16].TestData := '%s%s%s%s%s%s%s%s';
  tests[16].Category := 'Format';

  // Null Byte Injection
  tests[17].Name := 'Null Byte';
  tests[17].TestData := 'test'#0'.txt';
  tests[17].Category := 'Null';

  // Unicode/UTF-8
  tests[18].Name := 'Unicode Bypass';
  tests[18].TestData := '%c0%ae%c0%ae%c0%af'; // ../
  tests[18].Category := 'Unicode';

  // CRLF Injection
  tests[19].Name := 'CRLF Injection';
  tests[19].TestData := 'test'#13#10'Injected-Header: value';
  tests[19].Category := 'CRLF';

  // Exécuter les tests
  for i := 0 to High(tests) do
  begin
    WriteLn('[', tests[i].Category:8, '] ', tests[i].Name);
    Write('  Input:  "', Copy(tests[i].TestData, 1, 50));
    if Length(tests[i].TestData) > 50 then
      Write('...');
    WriteLn('"');

    try
      sanitized := SanitizeSQLInput(tests[i].TestData);
      Write('  Output: "', Copy(sanitized, 1, 50));
      if Length(sanitized) > 50 then
        Write('...');
      WriteLn('"');

      // Vérifier si l'entrée a été correctement nettoyée
      if tests[i].TestData = sanitized then
        WriteLn('  ⚠️  ATTENTION: Données non filtrées!')
      else
        WriteLn('  ✓ Données traitées');

    except
      on E: Exception do
        WriteLn('  ❌ CRASH: ', E.Message);
    end;

    WriteLn;
  end;
end;

begin
  RunSecurityFuzz;
end.
```

## Fuzzing avec couverture de code

### Exemple 10 : Mesure de couverture

```pascal
program CoverageFuzzer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TCoverageTracker = class
  private
    FLinesCovered: array of Boolean;
    FTotalLines: Integer;
  public
    constructor Create(TotalLines: Integer);
    procedure MarkLine(LineNumber: Integer);
    function GetCoverage: Double;
    procedure Reset;
  end;

constructor TCoverageTracker.Create(TotalLines: Integer);
begin
  inherited Create;
  FTotalLines := TotalLines;
  SetLength(FLinesCovered, TotalLines);
end;

procedure TCoverageTracker.MarkLine(LineNumber: Integer);
begin
  if (LineNumber >= 0) and (LineNumber < FTotalLines) then
    FLinesCovered[LineNumber] := True;
end;

function TCoverageTracker.GetCoverage: Double;
var
  i, covered: Integer;
begin
  covered := 0;
  for i := 0 to FTotalLines - 1 do
    if FLinesCovered[i] then
      Inc(covered);
  Result := (covered * 100.0) / FTotalLines;
end;

procedure TCoverageTracker.Reset;
var
  i: Integer;
begin
  for i := 0 to FTotalLines - 1 do
    FLinesCovered[i] := False;
end;

var
  coverage: TCoverageTracker;

// Fonction à tester avec instrumentation manuelle
function ProcessInput(const input: string): Integer;
begin
  coverage.MarkLine(1);
  Result := 0;

  coverage.MarkLine(2);
  if Length(input) = 0 then
  begin
    coverage.MarkLine(3);
    Exit(-1);
  end;

  coverage.MarkLine(4);
  if input[1] = 'A' then
  begin
    coverage.MarkLine(5);
    Result := 1;
  end
  else if input[1] = 'B' then
  begin
    coverage.MarkLine(6);
    Result := 2;
  end
  else
  begin
    coverage.MarkLine(7);
    Result := 3;
  end;

  coverage.MarkLine(8);
  if Length(input) > 5 then
  begin
    coverage.MarkLine(9);
    Result := Result * 10;
  end;

  coverage.MarkLine(10);
end;

procedure FuzzWithCoverage;
var
  i: Integer;
  input: string;
  result: Integer;
  previousCoverage, currentCoverage: Double;
begin
  WriteLn('=== Fuzzing avec mesure de couverture ===');
  WriteLn;

  coverage := TCoverageTracker.Create(11); // 11 lignes de code

  try
    Randomize;
    previousCoverage := 0;

    for i := 1 to 100 do
    begin
      // Générer une entrée aléatoire
      SetLength(input, Random(10));
      if Length(input) > 0 then
        input[1] := Chr(Random(26) + Ord('A'));

      // Tester
      result := ProcessInput(input);

      // Mesurer la couverture
      currentCoverage := coverage.GetCoverage;

      // Afficher si la couverture a augmenté
      if currentCoverage > previousCoverage then
      begin
        WriteLn('Test ', i:3, ': Input="', input, '" → Nouvelle couverture: ',
                currentCoverage:0:1, '%');
        previousCoverage := currentCoverage;
      end;

      // Arrêter si couverture complète
      if currentCoverage >= 100.0 then
      begin
        WriteLn;
        WriteLn('✓ Couverture complète atteinte après ', i, ' tests!');
        Break;
      end;
    end;

    WriteLn;
    WriteLn('Couverture finale: ', coverage.GetCoverage:0:1, '%');

  finally
    coverage.Free;
  end;
end;

begin
  FuzzWithCoverage;
end.
```

## Outils externes de fuzzing

### 1. AFL (American Fuzzy Lop) - Linux

**Installation sur Ubuntu :**

```bash
sudo apt-get install afl
```

**Compilation d'un programme pour AFL :**

```bash
# Compiler avec afl-fpc (wrapper pour FPC)
afl-fpc -O2 mon_programme.pas -o mon_programme_fuzz

# Créer les répertoires
mkdir input output

# Créer des entrées de départ
echo "test" > input/test1.txt

# Lancer AFL
afl-fuzz -i input -o output ./mon_programme_fuzz @@
```

**Explication :**
- `-i input` : Répertoire contenant les entrées initiales
- `-o output` : Répertoire pour les résultats
- `@@` : Remplacé par le nom du fichier d'entrée

### 2. Radamsa - Multi-plateforme

**Installation :**

```bash
# Ubuntu
sudo apt-get install radamsa

# Windows
# Télécharger depuis GitHub
```

**Utilisation :**

```bash
# Générer 100 mutations d'un fichier
for i in {1..100}; do
  radamsa input.txt > fuzz_$i.txt
done

# Tester chaque fichier généré
for f in fuzz_*.txt; do
  ./mon_programme "$f"
done
```

### 3. Honggfuzz - Multi-plateforme

```bash
# Installation Ubuntu
sudo apt-get install honggfuzz

# Utilisation
honggfuzz -i input_dir -o output_dir -- ./mon_programme ___FILE___
```

## Stratégies de fuzzing efficaces

### 1. Fuzzing guidé par dictionnaire

Utiliser un dictionnaire de mots-clés connus pour améliorer la couverture.

```pascal
program DictionaryFuzzer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  dictionary: TStringList;

procedure LoadDictionary;
begin
  dictionary := TStringList.Create;

  // Mots-clés SQL
  dictionary.Add('SELECT');
  dictionary.Add('INSERT');
  dictionary.Add('UPDATE');
  dictionary.Add('DELETE');
  dictionary.Add('DROP');
  dictionary.Add('WHERE');
  dictionary.Add('OR');
  dictionary.Add('AND');

  // Opérateurs
  dictionary.Add('=');
  dictionary.Add('''');
  dictionary.Add(';');
  dictionary.Add('--');

  // Patterns
  dictionary.Add('1=1');
  dictionary.Add('true');
  dictionary.Add('null');
end;

function GenerateDictionaryBasedInput: string;
var
  numTokens, i, idx: Integer;
begin
  Result := '';
  numTokens := Random(5) + 1;

  for i := 1 to numTokens do
  begin
    idx := Random(dictionary.Count);
    Result := Result + dictionary[idx];
    if i < numTokens then
      Result := Result + ' ';
  end;
end;

var
  i: Integer;
  input: string;

begin
  WriteLn('=== Fuzzing guidé par dictionnaire ===');
  WriteLn;

  Randomize;
  LoadDictionary;

  try
    WriteLn('Exemples d''entrées générées:');
    WriteLn;

    for i := 1 to 10 do
    begin
      input := GenerateDictionaryBasedInput;
      WriteLn(i:2, '. ', input);
    end;

  finally
    dictionary.Free;
  end;
end.
```

### 2. Fuzzing adaptatif

Adapter la stratégie en fonction des résultats.

```pascal
program AdaptiveFuzzer;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TFuzzStrategy = (fsRandom, fsBoundary, fsSpecialChars, fsMutation);

var
  strategySuccess: array[TFuzzStrategy] of Integer;
  strategyAttempts: array[TFuzzStrategy] of Integer;

function GetBestStrategy: TFuzzStrategy;
var
  s: TFuzzStrategy;
  bestScore: Double;
  currentScore: Double;
begin
  Result := fsRandom;
  bestScore := 0;

  for s := Low(TFuzzStrategy) to High(TFuzzStrategy) do
  begin
    if strategyAttempts[s] > 0 then
    begin
      currentScore := strategySuccess[s] / strategyAttempts[s];
      if currentScore > bestScore then
      begin
        bestScore := currentScore;
        Result := s;
      end;
    end;
  end;
end;

function GenerateInput(strategy: TFuzzStrategy): string;
begin
  case strategy of
    fsRandom:
      Result := StringOfChar(Chr(Random(256)), Random(20) + 1);
    fsBoundary:
      Result := IntToStr(Random(2) * MaxInt);
    fsSpecialChars:
      Result := '''<>&"' + #0 + #13 + #10;
    fsMutation:
      Result := 'valid' + Chr(Random(256));
  end;
end;

function TestInput(const input: string): Boolean;
begin
  // Simuler un test
  Result := Random(10) > 7; // 30% de succès
end;

var
  i: Integer;
  strategy: TFuzzStrategy;
  input: string;
  success: Boolean;

begin
  WriteLn('=== Fuzzing adaptatif ===');
  WriteLn;

  Randomize;

  // Initialiser
  for strategy := Low(TFuzzStrategy) to High(TFuzzStrategy) do
  begin
    strategySuccess[strategy] := 0;
    strategyAttempts[strategy] := 0;
  end;

  // Phase d'exploration: essayer toutes les stratégies
  WriteLn('Phase 1: Exploration...');
  for strategy := Low(TFuzzStrategy) to High(TFuzzStrategy) do
  begin
    for i := 1 to 10 do
    begin
      input := GenerateInput(strategy);
      success := TestInput(input);

      Inc(strategyAttempts[strategy]);
      if success then
        Inc(strategySuccess[strategy]);
    end;
  end;

  // Phase d'exploitation: utiliser la meilleure stratégie
  WriteLn('Phase 2: Exploitation...');
  strategy := GetBestStrategy;
  WriteLn('Meilleure stratégie: ', GetEnumName(TypeInfo(TFuzzStrategy), Ord(strategy)));
  WriteLn;

  for i := 1 to 50 do
  begin
    input := GenerateInput(strategy);
    success := TestInput(input);

    Inc(strategyAttempts[strategy]);
    if success then
    begin
      Inc(strategySuccess[strategy]);
      WriteLn('✓ Succès #', strategySuccess[strategy]);
    end;
  end;

  // Rapport final
  WriteLn;
  WriteLn('=== Statistiques ===');
  for strategy := Low(TFuzzStrategy) to High(TFuzzStrategy) do
  begin
    WriteLn(GetEnumName(TypeInfo(TFuzzStrategy), Ord(strategy)), ':');
    WriteLn('  Tentatives: ', strategyAttempts[strategy]);
    WriteLn('  Succès:     ', strategySuccess[strategy]);
    if strategyAttempts[strategy] > 0 then
      WriteLn('  Taux:       ', (strategySuccess[strategy] * 100) div strategyAttempts[strategy], '%');
    WriteLn;
  end;
end.
```

## Bonnes pratiques de fuzzing

### Checklist de fuzzing

```
□ Définir les objectifs (bugs, sécurité, robustesse)
□ Identifier les points d'entrée (fonctions, API, fichiers)
□ Créer un corpus d'entrées valides de départ
□ Choisir la stratégie (mutation, génération, hybride)
□ Instrumenter le code si possible (couverture)
□ Configurer les timeouts pour éviter les blocages
□ Logger tous les crashes et erreurs
□ Rejouer les cas problématiques pour confirmer
□ Prioriser les bugs trouvés par gravité
□ Corriger et retester
```

### Limites du fuzzing

**Ce que le fuzzing peut trouver :**
✅ Crashes et plantages  
✅ Débordements de buffer  
✅ Exceptions non gérées  
✅ Fuites mémoire  
✅ Boucles infinies (avec timeout)  
✅ Injections (SQL, XSS, etc.)  
✅ Problèmes de validation d'entrées  
✅ Erreurs de format

**Ce que le fuzzing ne peut PAS trouver facilement :**
❌ Bugs logiques complexes  
❌ Problèmes de concurrence (race conditions)  
❌ Bugs dépendant d'un état spécifique  
❌ Vulnérabilités nécessitant une séquence précise  
❌ Problèmes de permissions ou d'authentification  
❌ Bugs nécessitant un contexte métier

### Quand utiliser le fuzzing

**Utiliser le fuzzing pour :**
- Parsers (XML, JSON, CSV, etc.)
- Fonctions de validation d'entrées
- API publiques
- Code gérant des données non fiables
- Protocoles réseau
- Gestionnaires de fichiers

**Ne pas utiliser uniquement le fuzzing pour :**
- Logique métier complexe
- Algorithmes mathématiques précis
- Interface utilisateur
- Configuration système

## Intégration du fuzzing dans le CI/CD

### Script d'automatisation du fuzzing

**Script bash (Ubuntu) :**

```bash
#!/bin/bash
# fuzzing_ci.sh - Script d'intégration continue pour fuzzing

PROJECT_DIR="/chemin/vers/projet"
FUZZ_DIR="$PROJECT_DIR/fuzzing"
RESULTS_DIR="$FUZZ_DIR/results_$(date +%Y%m%d_%H%M%S)"
CORPUS_DIR="$FUZZ_DIR/corpus"
MAX_DURATION=600  # 10 minutes

echo "=== Fuzzing CI/CD ==="
echo "Projet: $PROJECT_DIR"
echo "Durée max: $MAX_DURATION secondes"
echo

# Créer les répertoires
mkdir -p "$RESULTS_DIR"
mkdir -p "$CORPUS_DIR"

# Compiler le programme de test
cd "$PROJECT_DIR"
echo "Compilation du programme de fuzzing..."
fpc -O2 fuzzer.pas -o "$FUZZ_DIR/fuzzer"

if [ $? -ne 0 ]; then
    echo "❌ Erreur de compilation"
    exit 1
fi

# Lancer le fuzzing
echo "Démarrage du fuzzing..."
timeout $MAX_DURATION "$FUZZ_DIR/fuzzer" > "$RESULTS_DIR/output.log" 2>&1

# Analyser les résultats
CRASHES=$(grep -c "CRASH" "$RESULTS_DIR/output.log")
ERRORS=$(grep -c "ERROR" "$RESULTS_DIR/output.log")

echo
echo "=== Résultats ==="
echo "Crashes détectés: $CRASHES"
echo "Erreurs détectées: $ERRORS"

# Extraire les cas problématiques
if [ $CRASHES -gt 0 ]; then
    echo
    echo "⚠️  Détails des crashes:"
    grep -A 5 "CRASH" "$RESULTS_DIR/output.log" > "$RESULTS_DIR/crashes.txt"
    cat "$RESULTS_DIR/crashes.txt"
fi

# Verdict
if [ $CRASHES -gt 0 ]; then
    echo
    echo "❌ ÉCHEC: Des crashes ont été détectés!"
    exit 1
elif [ $ERRORS -gt 10 ]; then
    echo
    echo "⚠️  ATTENTION: Beaucoup d'erreurs détectées"
    exit 1
else
    echo
    echo "✓ Fuzzing terminé sans problème majeur"
    exit 0
fi
```

**Script PowerShell (Windows) :**

```powershell
# fuzzing_ci.ps1

$ProjectDir = "C:\Projets\MonProjet"
$FuzzDir = "$ProjectDir\fuzzing"
$ResultsDir = "$FuzzDir\results_$(Get-Date -Format 'yyyyMMdd_HHmmss')"
$CorpusDir = "$FuzzDir\corpus"
$MaxDuration = 600  # 10 minutes en secondes

Write-Host "=== Fuzzing CI/CD ===" -ForegroundColor Green
Write-Host "Projet: $ProjectDir"
Write-Host "Durée max: $MaxDuration secondes"
Write-Host ""

# Créer les répertoires
New-Item -ItemType Directory -Force -Path $ResultsDir | Out-Null
New-Item -ItemType Directory -Force -Path $CorpusDir | Out-Null

# Compiler
Set-Location $ProjectDir
Write-Host "Compilation du programme de fuzzing..." -ForegroundColor Yellow
& "C:\lazarus\fpc\bin\x86_64-win64\fpc.exe" -O2 fuzzer.pas -o"$FuzzDir\fuzzer.exe"

if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Erreur de compilation" -ForegroundColor Red
    exit 1
}

# Lancer le fuzzing avec timeout
Write-Host "Démarrage du fuzzing..." -ForegroundColor Yellow
$job = Start-Job -ScriptBlock {
    param($FuzzerPath, $OutputFile)
    & $FuzzerPath > $OutputFile 2>&1
} -ArgumentList "$FuzzDir\fuzzer.exe", "$ResultsDir\output.log"

# Attendre avec timeout
$completed = Wait-Job $job -Timeout $MaxDuration
if (-not $completed) {
    Write-Host "⏱️  Timeout atteint, arrêt du fuzzing..." -ForegroundColor Yellow
    Stop-Job $job
}

Remove-Job $job

# Analyser les résultats
$content = Get-Content "$ResultsDir\output.log" -Raw
$crashes = ([regex]::Matches($content, "CRASH")).Count
$errors = ([regex]::Matches($content, "ERROR")).Count

Write-Host ""
Write-Host "=== Résultats ===" -ForegroundColor Green
Write-Host "Crashes détectés: $crashes"
Write-Host "Erreurs détectées: $errors"

# Extraire les crashes
if ($crashes -gt 0) {
    Write-Host ""
    Write-Host "⚠️  Détails des crashes:" -ForegroundColor Red
    Select-String -Path "$ResultsDir\output.log" -Pattern "CRASH" -Context 0,5 |
        Out-File "$ResultsDir\crashes.txt"
    Get-Content "$ResultsDir\crashes.txt"
}

# Verdict
if ($crashes -gt 0) {
    Write-Host ""
    Write-Host "❌ ÉCHEC: Des crashes ont été détectés!" -ForegroundColor Red
    exit 1
}
elseif ($errors -gt 10) {
    Write-Host ""
    Write-Host "⚠️  ATTENTION: Beaucoup d'erreurs détectées" -ForegroundColor Yellow
    exit 1
}
else {
    Write-Host ""
    Write-Host "✓ Fuzzing terminé sans problème majeur" -ForegroundColor Green
    exit 0
}
```

### Configuration GitLab CI

**`.gitlab-ci.yml` :**

```yaml
stages:
  - build
  - test
  - fuzz

build:
  stage: build
  script:
    - fpc -O2 mon_programme.pas
  artifacts:
    paths:
      - mon_programme
    expire_in: 1 hour

unit-tests:
  stage: test
  script:
    - ./run_unit_tests.sh
  dependencies:
    - build

fuzzing:
  stage: fuzz
  script:
    - fpc -O2 fuzzer.pas
    - chmod +x fuzzing_ci.sh
    - ./fuzzing_ci.sh
  artifacts:
    when: always
    paths:
      - fuzzing/results_*/
    reports:
      junit: fuzzing/results_*/report.xml
  allow_failure: true  # Ne pas bloquer le pipeline en cas d'échec
  only:
    - merge_requests
    - main
```

### Configuration GitHub Actions

**`.github/workflows/fuzzing.yml` :**

```yaml
name: Fuzzing Tests

on:
  pull_request:
  push:
    branches: [main]
  schedule:
    - cron: '0 2 * * *'  # Tous les jours à 2h du matin

jobs:
  fuzzing:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Install FreePascal
      run: |
        sudo apt-get update
        sudo apt-get install -y fpc

    - name: Build fuzzer
      run: |
        fpc -O2 fuzzer.pas

    - name: Run fuzzing tests
      run: |
        chmod +x fuzzing_ci.sh
        ./fuzzing_ci.sh
      timeout-minutes: 10

    - name: Upload results
      uses: actions/upload-artifact@v3
      if: always()
      with:
        name: fuzzing-results
        path: fuzzing/results_*/

    - name: Comment PR with results
      if: github.event_name == 'pull_request'
      uses: actions/github-script@v6
      with:
        script: |
          const fs = require('fs');
          const results = fs.readFileSync('fuzzing/results_latest/summary.txt', 'utf8');
          github.rest.issues.createComment({
            issue_number: context.issue.number,
            owner: context.repo.owner,
            repo: context.repo.repo,
            body: '## Fuzzing Results\n\n```\n' + results + '\n```'
          });
```

## Cas d'usage avancés

### Exemple 11 : Fuzzing de machine à états

```pascal
program StateMachineFuzzer;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TState = (stIdle, stConnecting, stConnected, stDisconnecting, stError);
  TEvent = (evConnect, evDisconnect, evSendData, evReceiveData, evTimeout, evError);

  TStateMachine = class
  private
    FCurrentState: TState;
    FTransitionCount: Integer;
  public
    constructor Create;
    function ProcessEvent(Event: TEvent): Boolean;
    property CurrentState: TState read FCurrentState;
    property TransitionCount: Integer read FTransitionCount;
  end;

constructor TStateMachine.Create;
begin
  inherited Create;
  FCurrentState := stIdle;
  FTransitionCount := 0;
end;

function TStateMachine.ProcessEvent(Event: TEvent): Boolean;
begin
  Result := True;
  Inc(FTransitionCount);

  case FCurrentState of
    stIdle:
      case Event of
        evConnect: FCurrentState := stConnecting;
        else Result := False;
      end;

    stConnecting:
      case Event of
        evReceiveData: FCurrentState := stConnected;
        evTimeout, evError: FCurrentState := stError;
        else Result := False;
      end;

    stConnected:
      case Event of
        evDisconnect: FCurrentState := stDisconnecting;
        evSendData, evReceiveData: ; // Rester connecté
        evError: FCurrentState := stError;
        else Result := False;
      end;

    stDisconnecting:
      case Event of
        evTimeout: FCurrentState := stIdle;
        evError: FCurrentState := stError;
        else Result := False;
      end;

    stError:
      case Event of
        evTimeout: FCurrentState := stIdle;
        else Result := False;
      end;
  end;
end;

procedure FuzzStateMachine;
const
  MAX_EVENTS = 1000;
var
  sm: TStateMachine;
  i, validTransitions, invalidTransitions, crashes: Integer;
  event: TEvent;
  result: Boolean;
begin
  WriteLn('=== Fuzzing de machine à états ===');
  WriteLn;

  Randomize;
  validTransitions := 0;
  invalidTransitions := 0;
  crashes := 0;

  for i := 1 to 100 do // 100 tests
  begin
    sm := TStateMachine.Create;
    try
      // Générer une séquence aléatoire d'événements
      while sm.TransitionCount < MAX_EVENTS do
      begin
        event := TEvent(Random(Ord(High(TEvent)) + 1));

        try
          result := sm.ProcessEvent(event);
          if result then
            Inc(validTransitions)
          else
            Inc(invalidTransitions);

        except
          on E: Exception do
          begin
            WriteLn('💥 CRASH au test ', i, ' après ', sm.TransitionCount, ' transitions');
            WriteLn('   État: ', GetEnumName(TypeInfo(TState), Ord(sm.CurrentState)));
            WriteLn('   Événement: ', GetEnumName(TypeInfo(TEvent), Ord(event)));
            WriteLn('   Erreur: ', E.Message);
            WriteLn;
            Inc(crashes);
            Break;
          end;
        end;
      end;

    finally
      sm.Free;
    end;

    if (i mod 10) = 0 then
      Write('.');
  end;

  WriteLn;
  WriteLn;
  WriteLn('=== Résultats ===');
  WriteLn('Transitions valides:   ', validTransitions);
  WriteLn('Transitions invalides: ', invalidTransitions);
  WriteLn('Crashes:               ', crashes);
  WriteLn('Taux de validité:      ', (validTransitions * 100) div (validTransitions + invalidTransitions), '%');

  if crashes > 0 then
    WriteLn('❌ ÉCHEC: La machine à états a planté!')
  else
    WriteLn('✓ Machine à états robuste');
end;

begin
  FuzzStateMachine;
end.
```

### Exemple 12 : Fuzzing de sérialiseur/désérialiseur

```pascal
program SerializerFuzzer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fpjson, jsonparser;

type
  TPerson = class
    Name: string;
    Age: Integer;
    Email: string;
  end;

// Fonction à tester : Désérialisation JSON
function DeserializePerson(const jsonStr: string): TPerson;
var
  json: TJSONObject;
  parser: TJSONParser;
begin
  Result := TPerson.Create;
  parser := TJSONParser.Create(jsonStr, []);
  try
    json := parser.Parse as TJSONObject;
    try
      Result.Name := json.Get('name', '');
      Result.Age := json.Get('age', 0);
      Result.Email := json.Get('email', '');
    finally
      json.Free;
    end;
  finally
    parser.Free;
  end;
end;

procedure FuzzJSONDeserializer;
var
  testCases: array of string;
  i: Integer;
  person: TPerson;
begin
  WriteLn('=== Fuzzing du désérialiseur JSON ===');
  WriteLn;

  SetLength(testCases, 20);

  // JSON valides
  testCases[0] := '{"name":"John","age":30,"email":"john@example.com"}';
  testCases[1] := '{"name":"","age":0,"email":""}';

  // JSON malformés
  testCases[2] := '{"name":"John","age":30,}'; // Virgule finale
  testCases[3] := '{"name":"John""age":30}'; // Virgule manquante
  testCases[4] := '{"name":"John","age":"thirty"}'; // Type incorrect
  testCases[5] := '{name:"John"}'; // Guillemets manquants
  testCases[6] := '{"name":"John}'; // Guillemet non fermé
  testCases[7] := ''; // Vide
  testCases[8] := '{}'; // Objet vide
  testCases[9] := '[]'; // Tableau au lieu d'objet
  testCases[10] := 'null'; // Null
  testCases[11] := '{"name":"John","age":-999}'; // Age négatif
  testCases[12] := '{"name":"John","age":999999999999}'; // Age énorme
  testCases[13] := '{"name":"' + StringOfChar('A', 10000) + '"}'; // Nom très long
  testCases[14] := '{"unknown":"value"}'; // Champs inconnus
  testCases[15] := '{"name":"John","age":30'; // Accolade manquante
  testCases[16] := '{{{"name":"John"}}}'; // Imbrication excessive
  testCases[17] := '{"name":null,"age":null}'; // Valeurs null
  testCases[18] := '{"name":"John\u0000","age":30}'; // Caractère null
  testCases[19] := '{"name":"<script>alert(1)</script>"}'; // XSS

  for i := 0 to High(testCases) do
  begin
    Write('Test ', i + 1:2, ': ');

    try
      person := DeserializePerson(testCases[i]);
      try
        WriteLn('✓ OK (Name="', person.Name, '", Age=', person.Age, ')');
      finally
        person.Free;
      end;
    except
      on E: Exception do
        WriteLn('❌ ERREUR: ', E.Message);
    end;
  end;
end;

begin
  FuzzJSONDeserializer;
end.
```

## Métriques et KPI du fuzzing

### Indicateurs de succès

**Métriques de couverture :**
- Couverture de code (% de lignes exécutées)
- Couverture de branches (% de chemins testés)
- Couverture d'états (pour machines à états)

**Métriques de découverte :**
- Nombre de bugs uniques trouvés
- Gravité des bugs (critique, élevée, moyenne, faible)
- Temps moyen pour trouver un bug
- Taux de faux positifs

**Métriques d'efficacité :**
- Nombre de tests exécutés par seconde
- Ratio nouveaux cas / tests totaux
- Temps de fuzzing total
- Ressources consommées (CPU, RAM)

### Rapport de fuzzing type

```
========================================
     RAPPORT DE FUZZING
========================================

Date: 2025-10-06 14:30:00
Composant testé: Parser XML v2.3
Durée: 4 heures 23 minutes
Stratégie: Mutation-based + Dictionary

1. STATISTIQUES GLOBALES
   Tests exécutés:        1,234,567
   Tests/seconde:         78
   Couverture de code:    87.3%
   Couverture branches:   72.1%

2. BUGS DÉCOUVERTS
   Total:                 5
   - Critiques:           2
   - Élevés:              1
   - Moyens:              2
   - Faibles:             0

3. DÉTAILS DES BUGS CRITIQUES

   BUG #1: Débordement de buffer
   Gravité: CRITIQUE
   Input: <root attr="[10000 x 'A']">
   Impact: Crash de l'application
   Stack: XMLParser.pas:142

   BUG #2: Division par zéro
   Gravité: CRITIQUE
   Input: <math>1/0</math>
   Impact: Exception non gérée
   Stack: MathEval.pas:87

4. COUVERTURE

   Fichiers testés:       12
   Fonctions couvertes:   145/178 (81.5%)
   Branches non testées:  23

   Fonctions problématiques:
   - ParseComplexAttribute (47% couverture)
   - ValidateSchema (62% couverture)

5. RECOMMANDATIONS

   ✓ Corriger les 2 bugs critiques immédiatement
   ✓ Ajouter validation de longueur pour attributs
   ✓ Gérer division par zéro dans évaluateur
   ✓ Augmenter tests pour fonctions < 70% couverture
   ✓ Réexécuter fuzzing après corrections

========================================
```

## Conclusion

Le fuzzing est une technique puissante pour améliorer la robustesse et la sécurité de vos applications FreePascal/Lazarus.

**Points clés à retenir :**

✅ **Complément essentiel** : Le fuzzing ne remplace pas les tests unitaires, il les complète  
✅ **Découverte automatique** : Trouve des bugs impossibles à imaginer manuellement  
✅ **Sécurité** : Particulièrement efficace pour détecter les vulnérabilités  
✅ **Facile à démarrer** : Peut commencer avec des scripts simples  
✅ **Évolutif** : Peut être sophistiqué avec outils spécialisés

**Stratégie recommandée :**

1. **Phase 1 : Basique (Semaine 1)**
   - Identifier 2-3 fonctions critiques
   - Créer un fuzzer simple avec données aléatoires
   - Exécuter manuellement et corriger les bugs trouvés

2. **Phase 2 : Intermédiaire (Semaine 2-3)**
   - Ajouter générateurs intelligents
   - Créer un corpus d'entrées intéressantes
   - Intégrer dans les tests automatisés

3. **Phase 3 : Avancé (Mois 1-2)**
   - Mesurer la couverture de code
   - Automatiser dans CI/CD
   - Utiliser outils externes (AFL, Honggfuzz)

4. **Phase 4 : Expert (Continu)**
   - Fuzzing guidé par feedback
   - Tests de sécurité systématiques
   - Contribution au corpus communautaire

**Différences Windows/Ubuntu :**

| Aspect | Windows | Ubuntu |
|--------|---------|--------|
| Scripts | PowerShell | Bash |
| Outils natifs | Peu d'outils | AFL, Radamsa, etc. |
| Performance | Similaire | Légèrement meilleur |
| Intégration CI | GitHub Actions, Azure | GitLab CI, Jenkins |

**Ressources complémentaires :**

- Documentation AFL: https://aflplus.plus/
- Radamsa: https://gitlab.com/akihe/radamsa
- OWASP Testing Guide: https://owasp.org/www-project-web-security-testing-guide/

**Prochaines étapes :**

1. Choisir une fonction critique de votre application
2. Créer un premier fuzzer simple (exemple 1 ou 2)
3. Exécuter pendant 1 heure et analyser les résultats
4. Corriger les bugs trouvés
5. Automatiser l'exécution régulière
6. Documenter les cas de test intéressants découverts

Le fuzzing transforme votre ordinateur en testeur infatigable qui travaille 24/7 pour améliorer la qualité de votre code. C'est un investissement qui paie rapidement !

**Citation finale :**

> "Fuzzing is like having a million monkeys with typewriters, except they're trying to break your code instead of writing Shakespeare."

N'attendez pas qu'un utilisateur (ou un attaquant) trouve les bugs - laissez le fuzzer les trouver pour vous ! 🔍🐛

⏭️ [CI/CD multi-plateforme](/18-tests-qualite-code/09-ci-cd-multiplateforme.md)
