🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 24.5 Preprocesseur et macros

## Introduction

Le préprocesseur est un composant du compilateur FreePascal qui traite le code source **avant** la compilation proprement dite. Il permet de :
- Inclure ou exclure du code selon des conditions
- Définir des constantes et des macros
- Adapter le code à différentes plateformes
- Créer des raccourcis syntaxiques

Pensez au préprocesseur comme un "éditeur automatique" qui modifie votre code avant que le compilateur ne le voie.

---

## Les directives de compilation

### Syntaxe de base

Les directives de préprocesseur commencent par `{$` et se terminent par `}`.

```pascal
{$DIRECTIVE}
{$DIRECTIVE paramètre}
{$DIRECTIVE ON}
{$DIRECTIVE OFF}
```

### Directives courantes

```pascal
{$MODE OBJFPC}        // Mode de compilation
{$H+}                 // Chaînes longues (AnsiString)
{$I+}                 // Vérification des E/S
{$R+}                 // Vérification des limites
{$Q+}                 // Vérification des débordements
{$WARN 5024 OFF}      // Désactiver un avertissement spécifique
```

---

## Compilation conditionnelle

### Le trio IF-IFDEF-IFNDEF

#### IFDEF - Si défini

```pascal
{$IFDEF WINDOWS}
  uses Windows;
{$ENDIF}

{$IFDEF DEBUG}
  WriteLn('Mode débogage activé');
{$ENDIF}
```

#### IFNDEF - Si non défini

```pascal
{$IFNDEF RELEASE}
  // Code pour les versions non-release
  WriteLn('Version de développement');
{$ENDIF}
```

#### IF avec expressions

```pascal
{$IF DEFINED(WINDOWS) OR DEFINED(LINUX)}
  // Code pour Windows OU Linux
{$ENDIF}

{$IF FPC_FULLVERSION >= 30200}
  // Code nécessitant FreePascal 3.2.0 ou supérieur
{$ENDIF}
```

### Structures complètes

```pascal
{$IFDEF WINDOWS}
  uses Windows, ShellAPI;
  const PathSeparator = '\';
{$ELSE}
  {$IFDEF LINUX}
    uses BaseUnix;
    const PathSeparator = '/';
  {$ELSE}
    {$ERROR Plateforme non supportée}
  {$ENDIF}
{$ENDIF}
```

### IFOPT - Tester des options de compilation

```pascal
{$IFOPT R+}
  // Code exécuté seulement si la vérification des limites est active
  WriteLn('Vérification des limites active');
{$ENDIF}

{$IFOPT Q+}
  // Code pour la vérification des débordements
{$ENDIF}
```

---

## Définition de symboles

### DEFINE et UNDEF

```pascal
// Définir un symbole
{$DEFINE MON_SYMBOLE}

{$IFDEF MON_SYMBOLE}
  WriteLn('MON_SYMBOLE est défini');
{$ENDIF}

// Annuler un symbole
{$UNDEF MON_SYMBOLE}

{$IFNDEF MON_SYMBOLE}
  WriteLn('MON_SYMBOLE n''est plus défini');
{$ENDIF}
```

### Symboles prédéfinis

FreePascal définit automatiquement de nombreux symboles :

```pascal
{$IFDEF FPC}
  WriteLn('Compilé avec FreePascal');
{$ENDIF}

{$IFDEF WINDOWS}
  WriteLn('Plateforme Windows');
{$ENDIF}

{$IFDEF LINUX}
  WriteLn('Plateforme Linux');
{$ENDIF}

{$IFDEF UNIX}
  WriteLn('Plateforme Unix (Linux, macOS, BSD...)');
{$ENDIF}

{$IFDEF CPU64}
  WriteLn('Architecture 64 bits');
{$ENDIF}

{$IFDEF CPU32}
  WriteLn('Architecture 32 bits');
{$ENDIF}

{$IFDEF DEBUG}
  WriteLn('Mode débogage');
{$ENDIF}
```

### Symboles de version

```pascal
program VersionCheck;

begin
  {$IF FPC_FULLVERSION >= 30200}
    WriteLn('FreePascal 3.2.0 ou supérieur');
  {$ELSE}
    WriteLn('Version FreePascal trop ancienne');
    WriteLn('Version actuelle : ', {$I %FPCVERSION%});
  {$ENDIF}
end.
```

---

## Les macros

### Activation des macros

Les macros doivent être activées explicitement :

```pascal
{$MACRO ON}
```

### DEFINE avec valeur

Contrairement aux symboles simples, les macros peuvent avoir des valeurs.

```pascal
{$MACRO ON}
{$DEFINE VERSION := '1.0.0'}
{$DEFINE AUTHOR := 'Jean Dupont'}
{$DEFINE MAX_ITEMS := 100}
```

### Utilisation des macros

Les macros sont substituées par leurs valeurs lors du préprocessing.

```pascal
{$MACRO ON}
{$DEFINE APP_VERSION := '2.5.1'}

program MonApp;

const
  Version = APP_VERSION;  // L'identifiant macro s'utilise directement

begin
  WriteLn('MonApp version ', Version);
end.
```

**Résultat après préprocessing :**
```pascal
program MonApp;

const
  Version = '2.5.1';  // Macro remplacée par sa valeur

begin
  WriteLn('MonApp version ', Version);
end.
```

> **Important :** En FPC, les macros sont de la substitution textuelle simple. L'identifiant macro s'utilise directement dans le code (pas entre accolades). `{NomMacro}` serait interprété comme un commentaire Pascal, pas comme une expansion de macro.

### Limites des macros FPC

> **Attention :** Contrairement au préprocesseur C, les macros FPC **ne supportent pas les paramètres** (`%1`, `%2` n'existent pas). Elles sont uniquement de la substitution textuelle simple. Pour du code générique ou paramétré, utilisez plutôt des **génériques**, des **fichiers d'inclusion** ou des **outils de génération de code** (voir section 24.4).

Les macros restent utiles pour des substitutions simples :

```pascal
{$MACRO ON}
{$DEFINE DB_ENGINE := SQLite}
{$DEFINE MAX_CONNECTIONS := 10}

const
  Engine = 'DB_ENGINE';       // Pas d'expansion dans les chaînes !
  MaxConn = MAX_CONNECTIONS;  // Expansion : MaxConn = 10
```

---

## Inclusion de fichiers

### INCLUDE ou I

Permet d'inclure le contenu d'un autre fichier.

**Fichier : `config.inc`**
```pascal
const
  DatabaseHost = 'localhost';
  DatabasePort = 5432;
  DatabaseName = 'mydb';
```

**Fichier principal :**
```pascal
program MyApp;

{$I config.inc}

begin
  WriteLn('Connexion à : ', DatabaseHost, ':', DatabasePort);
end.
```

### Inclusion conditionnelle

```pascal
{$IFDEF DEBUG}
  {$I debug_config.inc}
{$ELSE}
  {$I release_config.inc}
{$ENDIF}
```

### Fichiers d'inclusion pour multi-plateforme

**Fichier : `platform.inc`**
```pascal
{$IFDEF WINDOWS}
  {$I windows_specific.inc}
{$ENDIF}

{$IFDEF LINUX}
  {$I linux_specific.inc}
{$ENDIF}

{$IFDEF DARWIN}
  {$I macos_specific.inc}
{$ENDIF}
```

---

## Macros avancées

### Macros pour la compilation conditionnelle (cas réels)

Les macros FPC sont surtout utiles pour activer/désactiver du code :

```pascal
{$MACRO ON}

{$IFDEF DEBUG}
  {$DEFINE LOG_PREFIX := '[DEBUG] '}
{$ELSE}
  {$DEFINE LOG_PREFIX := ''}
{$ENDIF}

procedure MyProcedure;  
begin
  WriteLn(LOG_PREFIX, 'Début du traitement');

  try
    // Code...
  except
    on E: Exception do
      WriteLn('[ERROR] ', E.Message);
  end;

  WriteLn(LOG_PREFIX, 'Fin du traitement');
end;
```

### Génération de code répétitif via fichiers d'inclusion

Pour générer du code répétitif, les **fichiers d'inclusion** sont plus adaptés que les macros :

**Fichier : `customer_fields.inc`**
```pascal
  private
    FID: Integer;
    FName: string;
    FEmail: string;
    FAge: Integer;
  public
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
    property Age: Integer read FAge write FAge;
```

**Usage :**
```pascal
type
  TCustomer = class
  {$I customer_fields.inc}
  end;
```

---

## Directives d'information

### Obtenir des informations sur la compilation

```pascal
{$I %DATE%}        // Date de compilation
{$I %TIME%}        // Heure de compilation
{$I %FILE%}        // Nom du fichier actuel
{$I %LINE%}        // Numéro de ligne actuel
{$I %FPCVERSION%}  // Version de FreePascal
{$I %FPCTARGETCPU%} // Architecture CPU cible
{$I %FPCTARGETOS%}  // OS cible
```

**Exemple d'utilisation :**
```pascal
program BuildInfo;

const
  BuildDate = {$I %DATE%};
  BuildTime = {$I %TIME%};
  CompilerVersion = {$I %FPCVERSION%};
  TargetOS = {$I %FPCTARGETOS%};

begin
  WriteLn('Compilé le : ', BuildDate, ' à ', BuildTime);
  WriteLn('Compilateur : FreePascal ', CompilerVersion);
  WriteLn('Système cible : ', TargetOS);
end.
```

### Affichage de messages durant la compilation

```pascal
{$MESSAGE 'Compilation en cours...'}
{$WARNING 'Cette fonctionnalité est obsolète'}
{$ERROR 'Configuration invalide'}
{$FATAL 'Erreur critique - arrêt de la compilation'}
{$NOTE 'Pensez à optimiser cette section'}
{$HINT 'Utilisez plutôt la nouvelle API'}
```

**Exemple pratique :**
```pascal
{$IFNDEF FPC}
  {$FATAL Ce code nécessite FreePascal}
{$ENDIF}

{$IF FPC_FULLVERSION < 30200}
  {$ERROR FreePascal 3.2.0 ou supérieur requis}
{$ENDIF}

{$IFDEF DEBUG}
  {$MESSAGE 'Compilation en mode DEBUG'}
{$ENDIF}
```

---

## Cas d'usage pratiques

### 1. Configuration multi-environnement

```pascal
{$MACRO ON}

// Définir l'environnement
{$DEFINE ENV_DEVELOPMENT}
//{$DEFINE ENV_STAGING}
//{$DEFINE ENV_PRODUCTION}

{$IFDEF ENV_DEVELOPMENT}
  {$DEFINE DB_HOST := 'localhost'}
  {$DEFINE DB_PORT := '5432'}
  {$DEFINE DB_NAME := 'dev_db'}
  {$DEFINE DEBUG_ENABLED}
{$ENDIF}

{$IFDEF ENV_STAGING}
  {$DEFINE DB_HOST := 'staging.example.com'}
  {$DEFINE DB_PORT := '5432'}
  {$DEFINE DB_NAME := 'staging_db'}
{$ENDIF}

{$IFDEF ENV_PRODUCTION}
  {$DEFINE DB_HOST := 'prod.example.com'}
  {$DEFINE DB_PORT := '5432'}
  {$DEFINE DB_NAME := 'prod_db'}
  {$UNDEF DEBUG_ENABLED}
{$ENDIF}

const
  DatabaseHost = DB_HOST;
  DatabasePort = DB_PORT;
  DatabaseName = DB_NAME;

begin
  WriteLn('Connexion à : ', DatabaseHost, ':', DatabasePort, '/', DatabaseName);

  {$IFDEF DEBUG_ENABLED}
    WriteLn('Mode DEBUG actif');
  {$ENDIF}
end.
```

### 2. Compatibilité multi-versions

```pascal
program CompatibleCode;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

// Pour FreePascal 3.0.x et antérieur
{$IF FPC_FULLVERSION < 30200}
  type
    // Définir des types manquants
    NativeInt = Integer;
    NativeUInt = Cardinal;
{$ENDIF}

// Pour FreePascal 3.2.0 et supérieur
{$IF FPC_FULLVERSION >= 30200}
  {$DEFINE HAS_INLINE}
  {$DEFINE HAS_GENERICS}
{$ENDIF}

type
  TMyClass = class
    {$IFDEF HAS_INLINE}
    function GetValue: Integer; inline;
    {$ELSE}
    function GetValue: Integer;
    {$ENDIF}
  end;

function TMyClass.GetValue: Integer;  
begin
  Result := 42;
end;

begin
  WriteLn('Code compatible avec plusieurs versions de FPC');
end.
```

### 3. Profils de compilation

**Fichier : `profiles.inc`**
```pascal
// Profil DEBUG
{$IFDEF PROFILE_DEBUG}
  {$R+}  // Vérification des limites
  {$Q+}  // Vérification des débordements
  {$I+}  // Vérification des E/S
  {$D+}  // Informations de débogage
  {$ASSERTIONS ON}
  {$OPTIMIZATION OFF}
  {$DEFINE LOGGING}
  {$DEFINE PROFILING}
{$ENDIF}

// Profil RELEASE
{$IFDEF PROFILE_RELEASE}
  {$R-}  // Pas de vérification
  {$Q-}
  {$I-}
  {$D-}  // Pas d'infos debug
  {$ASSERTIONS OFF}
  {$OPTIMIZATION LEVEL3}
  {$UNDEF LOGGING}
  {$UNDEF PROFILING}
{$ENDIF}

// Profil TESTING
{$IFDEF PROFILE_TESTING}
  {$R+}
  {$Q+}
  {$I+}
  {$D+}
  {$ASSERTIONS ON}
  {$OPTIMIZATION LEVEL2}
  {$DEFINE LOGGING}
  {$DEFINE MOCK_SERVICES}
{$ENDIF}
```

**Utilisation :**
```pascal
program MyApp;

{$DEFINE PROFILE_DEBUG}
{$I profiles.inc}

begin
  {$IFDEF LOGGING}
    WriteLn('Logging activé');
  {$ENDIF}

  {$IFDEF MOCK_SERVICES}
    WriteLn('Services mockés pour les tests');
  {$ENDIF}
end.
```

### 4. Adaptation plateforme-spécifique

```pascal
{$MACRO ON}

// Définir les chemins selon la plateforme
{$IFDEF WINDOWS}
  {$DEFINE CONFIG_PATH := 'C:\ProgramData\MyApp\'}
  {$DEFINE TEMP_PATH := 'C:\Temp\'}
  {$DEFINE PATH_SEP := '\'}
  uses Windows;
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE CONFIG_PATH := '/etc/myapp/'}
  {$DEFINE TEMP_PATH := '/tmp/'}
  {$DEFINE PATH_SEP := '/'}
  uses BaseUnix;
{$ENDIF}

const
  ConfigDirectory = CONFIG_PATH;
  TempDirectory = TEMP_PATH;
  MyPathSep = PATH_SEP;

function GetFullPath(const FileName: string): string;  
begin
  Result := ConfigDirectory + FileName;
end;

begin
  WriteLn('Répertoire de configuration : ', ConfigDirectory);
  WriteLn('Fichier config : ', GetFullPath('app.conf'));
end.
```

### 5. Feature Flags (drapeaux de fonctionnalités)

```pascal
{$MACRO ON}

// Activer/désactiver des fonctionnalités
{$DEFINE FEATURE_TELEMETRY}
{$DEFINE FEATURE_AUTO_UPDATE}
//{$UNDEF FEATURE_BETA_FEATURES}

program FeatureFlagsDemo;

procedure InitializeApp;  
begin
  WriteLn('Initialisation de l''application...');

  {$IFDEF FEATURE_TELEMETRY}
  WriteLn('- Télémétrie activée');
  // InitTelemetry();
  {$ENDIF}

  {$IFDEF FEATURE_AUTO_UPDATE}
  WriteLn('- Mise à jour automatique activée');
  // InitAutoUpdater();
  {$ENDIF}

  {$IFDEF FEATURE_BETA_FEATURES}
  WriteLn('- Fonctionnalités bêta activées');
  // EnableBetaFeatures();
  {$ENDIF}
end;

begin
  InitializeApp;
end.
```

---

## Macros pour le code multi-plateforme

### Bibliothèques dynamiques

```pascal
{$MACRO ON}

{$IFDEF WINDOWS}
  {$DEFINE LIBEXT := '.dll'}
  {$DEFINE LIBPREFIX := ''}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE LIBEXT := '.so'}
  {$DEFINE LIBPREFIX := 'lib'}
{$ENDIF}

{$IFDEF DARWIN}
  {$DEFINE LIBEXT := '.dylib'}
  {$DEFINE LIBPREFIX := 'lib'}
{$ENDIF}

const
  SQLiteLibrary = LIBPREFIX + 'sqlite3' + LIBEXT;
  // Windows: 'sqlite3.dll'
  // Linux:   'libsqlite3.so'
  // macOS:   'libsqlite3.dylib'
```

### Gestion des chemins

```pascal
{$MACRO ON}

{$IFDEF WINDOWS}
  {$DEFINE HOME_DIR := GetEnvironmentVariable('USERPROFILE')}
  {$DEFINE APP_DATA := GetEnvironmentVariable('APPDATA')}
{$ENDIF}

{$IFDEF UNIX}
  {$DEFINE HOME_DIR := GetEnvironmentVariable('HOME')}
  {$DEFINE APP_DATA := GetEnvironmentVariable('HOME') + '/.config'}
{$ENDIF}

function GetConfigPath: string;  
begin
  Result := APP_DATA + PathDelim + 'MyApp' + PathDelim;
end;
```

---

## Optimisations avec les macros

### Élimination de code mort

```pascal
{$MACRO ON}
{$DEFINE OPTIMIZE_SIZE}

{$IFDEF OPTIMIZE_SIZE}
  {$DEFINE USE_INLINE := }
{$ELSE}
  {$DEFINE USE_INLINE := inline;}
{$ENDIF}

function Calculate(X: Integer): Integer; USE_INLINE  
begin
  {$IFNDEF OPTIMIZE_SIZE}
  WriteLn('Calcul avec X = ', X);
  {$ENDIF}
  Result := X * X;
end;
```

### Profilage conditionnel

```pascal
{$MACRO ON}

{$IFDEF ENABLE_PROFILING}
  {$DEFINE START_TIMER := StartTimer := GetTickCount64;}
  {$DEFINE STOP_TIMER := WriteLn('Temps écoulé: ', GetTickCount64 - StartTimer, ' ms');}
{$ELSE}
  {$DEFINE START_TIMER := }
  {$DEFINE STOP_TIMER := }
{$ENDIF}

procedure ProcessData;  
var
  StartTimer: QWord;
begin
  START_TIMER   // Expansion : StartTimer := GetTickCount64; (ou rien)

  // Traitement...

  STOP_TIMER    // Expansion : WriteLn(...) (ou rien)
end;
```

---

## Bonnes pratiques

### 1. Organisation des directives

**Structure recommandée :**
```pascal
program MyApp;

// 1. Mode et options de base
{$MODE OBJFPC}{$H+}
{$MACRO ON}

// 2. Activation des fonctionnalités
{$DEFINE FEATURE_LOGGING}
{$DEFINE FEATURE_CACHING}

// 3. Configuration selon l'environnement
{$IFDEF DEBUG}
  {$ASSERTIONS ON}
  {$R+}{$Q+}{$I+}
{$ELSE}
  {$ASSERTIONS OFF}
  {$R-}{$Q-}{$I-}
{$ENDIF}

// 4. Inclusions
{$I config.inc}

// 5. Code principal
begin
  // ...
end.
```

### 2. Documentation des macros

```pascal
{$MACRO ON}

// Macro pour activer/désactiver le traçage
// Actif uniquement en mode DEBUG
{$IFDEF DEBUG}
  {$DEFINE TRACE_PREFIX := '[TRACE] '}
{$ELSE}
  {$DEFINE TRACE_PREFIX := }
{$ENDIF}

// Utilisation :
procedure DoWork;  
begin
  {$IFDEF DEBUG}
  WriteLn(TRACE_PREFIX, 'Début de DoWork');
  {$ENDIF}
  // ...
end;
```

### 3. Fichiers d'inclusion séparés

**Fichier : `compiler_settings.inc`**
```pascal
{$MODE OBJFPC}
{$H+}
{$MACRO ON}
```

**Fichier : `debug_settings.inc`**
```pascal
{$ASSERTIONS ON}
{$R+}{$Q+}{$I+}
{$DEFINE LOGGING}
```

**Fichier : `release_settings.inc`**
```pascal
{$ASSERTIONS OFF}
{$R-}{$Q-}{$I-}
{$OPTIMIZATION LEVEL3}
```

**Usage :**
```pascal
program MyApp;

{$I compiler_settings.inc}

{$IFDEF DEBUG}
  {$I debug_settings.inc}
{$ELSE}
  {$I release_settings.inc}
{$ENDIF}

begin
  // ...
end.
```

### 4. Éviter les macros trop complexes

**❌ Mauvais - les macros FPC ne supportent pas les paramètres :**
```pascal
// Ceci ne fonctionne PAS en FPC (pas de macros paramétrées)
// {$DEFINE COMPLEX_MACRO := if %1 > %2 then WriteLn(%3)...}
```

**✅ Bon - utiliser des fonctions :**
```pascal
procedure CompareAndLog(A, B: Integer; Greater, Less, Equal: string);  
begin
  if A > B then WriteLn(Greater)
  else if A < B then WriteLn(Less)
  else WriteLn(Equal);
end;
```

---

## Débogage des macros

### Voir le résultat du préprocessing

**Option de compilation :**
```bash
fpc -Cn myprogram.pas
```

Cela génère un fichier `.ppu` contenant le code après préprocessing.

### Utiliser des messages de débogage

```pascal
{$MACRO ON}
{$DEFINE MY_MACRO := SomeValue}

{$MESSAGE 'MY_MACRO est défini'}

{$IFDEF DEBUG}
  {$MESSAGE 'Mode DEBUG activé'}
{$ENDIF}
```

### Tracer l'expansion des macros

```pascal
{$MACRO ON}

{$MESSAGE 'Début de la définition des macros'}
{$DEFINE VERSION := '1.0'}
{$MESSAGE 'VERSION définie'}

{$DEFINE APP_NAME := 'MonApp'}
{$MESSAGE 'APP_NAME définie'}

const
  FullName = APP_NAME + ' v' + VERSION;
{$MESSAGE 'Constante FullName créée'}
```

---

## Limitations et pièges à éviter

### 1. Les macros ne sont pas des fonctions

Les macros FPC ne supportent **pas les paramètres**. Elles ne font que de la substitution textuelle simple.

```pascal
{$MACRO ON}

// Les macros ne peuvent remplacer que des identifiants simples
{$DEFINE TWO := 2}
{$DEFINE PI_APPROX := 3.14159}

var
  X: Integer;
begin
  X := TWO * TWO;             // Devient : X := 2 * 2;
  WriteLn(PI_APPROX);         // Devient : WriteLn(3.14159);
end.
```

**Pour des calculs paramétrés, utilisez des fonctions :**
```pascal
function Square(X: Integer): Integer; inline;  
begin
  Result := X * X;
end;
```

### 2. Portée des définitions

Les `{$DEFINE}` sont **globaux** dans une unité et ses dépendances.

```pascal
// Unit1.pas
{$DEFINE MY_SYMBOL}

// Unit2.pas - qui utilise Unit1
{$IFDEF MY_SYMBOL}  // MY_SYMBOL est défini ici aussi!
  // ...
{$ENDIF}
```

### 3. Macros et chaînes de caractères

```pascal
{$MACRO ON}
{$DEFINE NAME := 'John'}

const
  Msg1 = 'Hello NAME';    // ❌ Pas d'expansion dans les chaînes littérales
  Msg2 = 'Hello ' + NAME; // ✅ Concaténation : 'Hello ' + 'John'
```

---

## Conclusion

Le préprocesseur et les macros de FreePascal sont des outils puissants qui permettent :

✅ **Compilation conditionnelle** pour le multi-plateforme  
✅ **Réduction de code répétitif** avec les macros  
✅ **Configuration flexible** selon l'environnement  
✅ **Optimisation** en éliminant le code inutile  
✅ **Compatibilité** avec différentes versions du compilateur

**Points clés à retenir :**
- Utiliser `{$IFDEF}` pour le code conditionnel
- Activer `{$MACRO ON}` pour les macros avec valeurs
- Documenter clairement toutes les macros
- Préférer les fonctions pour la logique complexe
- Tester le code sur toutes les plateformes cibles

**Prochaines étapes :**
1. Expérimenter avec les directives de base
2. Créer un système de configuration multi-environnement
3. Développer des macros réutilisables pour vos projets
4. Explorer les fichiers d'inclusion pour organiser votre code

⏭️ [Analyseurs syntaxiques (fcl-passrc)](/24-compilateur-outils-avances/06-analyseurs-syntaxiques-fcl-passrc.md)
