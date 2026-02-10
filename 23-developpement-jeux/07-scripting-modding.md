🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.7 Scripting et Modding

## Introduction

Le **scripting** et le **modding** sont des techniques qui permettent de rendre vos jeux et applications extensibles et personnalisables par les utilisateurs finaux, sans qu'ils aient besoin de recompiler le programme principal.

### Qu'est-ce que le scripting ?

Le scripting consiste à intégrer un langage de script (comme Lua, Python ou JavaScript) dans votre application FreePascal. Cela permet aux utilisateurs ou développeurs tiers d'écrire du code qui s'exécute dans votre programme, modifiant son comportement sans toucher au code source principal.

### Qu'est-ce que le modding ?

Le modding (modification) désigne la capacité pour les utilisateurs de personnaliser et d'étendre un jeu ou une application. Cela peut inclure :
- Nouveaux niveaux ou cartes
- Personnages et objets personnalisés
- Modifications des règles du jeu
- Nouveaux comportements et mécaniques
- Contenu graphique et sonore additionnel

## Pourquoi intégrer le scripting dans vos projets ?

### Avantages pour les développeurs

1. **Développement plus rapide** : Modifier un script est plus rapide que recompiler toute l'application
2. **Séparation des responsabilités** : La logique métier peut être dans les scripts, le moteur reste en Pascal
3. **Prototypage rapide** : Tester de nouvelles idées sans toucher au code principal
4. **Mises à jour faciles** : Patcher un jeu en modifiant simplement des scripts

### Avantages pour les utilisateurs

1. **Personnalisation** : Adapter le jeu à leurs préférences
2. **Créativité** : Créer leur propre contenu
3. **Communauté** : Partager des mods avec d'autres joueurs
4. **Durée de vie** : Le jeu reste intéressant plus longtemps grâce aux mods

## Langages de script populaires

### Lua

**Lua** est le langage de script le plus populaire dans l'industrie du jeu vidéo.

**Avantages** :
- Très léger et rapide
- Facile à apprendre
- Excellente intégration avec le C (et donc Pascal via FFI)
- Utilisé dans des jeux AAA (World of Warcraft, Angry Birds, etc.)
- Bibliothèques d'intégration matures pour FreePascal

**Inconvénients** :
- Syntaxe parfois déroutante pour les débutants
- Indexation des tableaux commence à 1 (pas 0)

### Python

**Python** est un langage très accessible et puissant.

**Avantages** :
- Syntaxe claire et lisible
- Énorme écosystème de bibliothèques
- Très populaire (beaucoup d'utilisateurs le connaissent)
- Python4Lazarus facilite l'intégration

**Inconvénients** :
- Plus lourd que Lua
- Plus lent à l'exécution
- Gestion de l'environnement Python peut être complexe

### JavaScript

**JavaScript** avec des moteurs comme V8 ou SpiderMonkey.

**Avantages** :
- Langage très connu
- Performances excellentes avec les moteurs modernes
- Bonne pour les jeux web

**Inconvénients** :
- Intégration plus complexe avec FreePascal
- Moteurs lourds

### PascalScript

**PascalScript** est un langage de script qui ressemble au Pascal.

**Avantages** :
- Syntaxe familière pour les développeurs Pascal
- Intégration native parfaite
- Léger et rapide
- Pas de dépendances externes

**Inconvénients** :
- Moins connu du grand public
- Communauté plus petite
- Moins de documentation

## Intégration de Lua dans FreePascal

Lua est le choix recommandé pour la plupart des projets de jeux. Voici comment l'intégrer.

### Installation des bibliothèques Lua

**Sur Windows** :
```bash
# Télécharger lua5.4.dll depuis luabinaries.sourceforge.net
# Placer le fichier .dll dans le dossier de votre application
```

**Sur Ubuntu** :
```bash
sudo apt-get install lua5.4 liblua5.4-dev
```

### Bibliothèques FreePascal pour Lua

Plusieurs options existent :

1. **lua-pas** : Binding direct de l'API C de Lua
2. **LuaJIT-pas** : Pour LuaJIT (version optimisée de Lua)
3. **Lua-Pas Wrapper** : Interface de plus haut niveau

### Exemple basique d'intégration Lua

```pascal
program LuaExample;

{$mode objfpc}{$H+}

uses
  lua54; // Binding Lua pour FreePascal

var
  L: Plua_State;
  script: string;
begin
  // Créer un état Lua
  L := lua_open();

  // Charger les bibliothèques standard
  luaL_openlibs(L);

  // Script Lua simple
  script := 'print("Bonjour depuis Lua!")';

  // Exécuter le script
  if luaL_dostring(L, PChar(script)) <> 0 then
  begin
    WriteLn('Erreur Lua : ', lua_tostring(L, -1));
    lua_pop(L, 1);
  end;

  // Nettoyer
  lua_close(L);
end.
```

### Exposer des fonctions Pascal à Lua

Pour permettre à Lua d'appeler des fonctions Pascal :

```pascal
function PascalFunction(L: Plua_State): Integer; cdecl;
var
  param: Integer;
begin
  // Récupérer le paramètre depuis Lua
  param := lua_tointeger(L, 1);

  // Faire un traitement
  WriteLn('Fonction Pascal appelée avec paramètre : ', param);

  // Retourner un résultat à Lua
  lua_pushinteger(L, param * 2);

  // Nombre de valeurs retournées
  Result := 1;
end;

procedure RegisterPascalFunctions(L: Plua_State);
begin
  // Enregistrer la fonction dans Lua
  lua_register(L, 'pascalFunction', @PascalFunction);
end;
```

Utilisation depuis Lua :
```lua
-- Appeler la fonction Pascal
resultat = pascalFunction(42)
print("Résultat : " .. resultat)  -- Affiche 84
```

### Passer des données complexes entre Pascal et Lua

#### Tables Lua vers Pascal

```pascal
procedure ReadLuaTable(L: Plua_State);
var
  key, value: string;
begin
  // La table doit être au sommet de la pile
  lua_pushnil(L);  // Premier clé

  while lua_next(L, -2) <> 0 do
  begin
    // Clé à l'index -2, valeur à -1
    key := lua_tostring(L, -2);
    value := lua_tostring(L, -1);

    WriteLn(Format('%s = %s', [key, value]));

    // Retirer la valeur, garder la clé pour next
    lua_pop(L, 1);
  end;
end;
```

#### Créer des objets Lua depuis Pascal

```pascal
procedure CreatePlayerObject(L: Plua_State; PlayerName: string; Health: Integer);
begin
  lua_newtable(L);

  // Ajouter le nom
  lua_pushstring(L, 'name');
  lua_pushstring(L, PChar(PlayerName));
  lua_settable(L, -3);

  // Ajouter la santé
  lua_pushstring(L, 'health');
  lua_pushinteger(L, Health);
  lua_settable(L, -3);

  // La table est maintenant au sommet de la pile
  lua_setglobal(L, 'player');
end;
```

## Architecture d'un système de modding

### Structure de fichiers recommandée

```
MonJeu/
├── MonJeu.exe (ou MonJeu sous Linux)
├── data/
│   ├── graphics/
│   ├── sounds/
│   └── levels/
├── scripts/
│   ├── core/           # Scripts système (ne pas modifier)
│   │   ├── engine.lua
│   │   └── utils.lua
│   └── game/           # Scripts de jeu (modifiables)
│       ├── player.lua
│       ├── enemies.lua
│       └── items.lua
└── mods/               # Dossier pour les mods des utilisateurs
    ├── SuperMod/
    │   ├── mod.json    # Métadonnées du mod
    │   ├── scripts/
    │   ├── graphics/
    │   └── sounds/
    └── AutreMod/
        └── ...
```

### Fichier de métadonnées pour les mods

Créer un format simple pour décrire les mods :

```json
{
  "name": "Super Mod",
  "version": "1.0.0",
  "author": "NomDuCreateur",
  "description": "Ajoute de nouveaux ennemis et objets",
  "dependencies": [],
  "compatible_game_version": "1.5.0",
  "load_order": 100,
  "entry_point": "scripts/init.lua"
}
```

### Gestionnaire de mods en Pascal

```pascal
type
  TMod = class
  private
    FName: string;
    FVersion: string;
    FAuthor: string;
    FEnabled: Boolean;
    FScriptPath: string;
  public
    property Name: string read FName;
    property Enabled: Boolean read FEnabled write FEnabled;
    function Load(LuaState: Plua_State): Boolean;
  end;

  TModManager = class
  private
    FMods: TList;
    FLuaState: Plua_State;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ScanModsDirectory(const Path: string);
    procedure LoadAllMods;
    procedure EnableMod(const ModName: string);
    procedure DisableMod(const ModName: string);
    function GetModList: TStringList;
  end;
```

## Système de hooks et événements

Les **hooks** (ou crochets) permettent aux scripts de réagir à des événements du jeu.

### Concept de base

```pascal
type
  TEventType = (etPlayerSpawn, etEnemyDeath, etItemPickup, etLevelStart);

  TScriptEvent = class
  private
    FEventType: TEventType;
    FCallbacks: TList;
  public
    procedure AddCallback(CallbackName: string);
    procedure Trigger(L: Plua_State; params: array of const);
  end;
```

### Exemple d'implémentation

```pascal
procedure TScriptEvent.Trigger(L: Plua_State; params: array of const);
var
  i: Integer;
  callbackName: string;
begin
  for i := 0 to FCallbacks.Count - 1 do
  begin
    callbackName := FCallbacks[i];

    // Récupérer la fonction Lua
    lua_getglobal(L, PChar(callbackName));

    if lua_isfunction(L, -1) then
    begin
      // Pousser les paramètres
      // (code pour convertir params en valeurs Lua)

      // Appeler la fonction
      if lua_pcall(L, Length(params), 0, 0) <> 0 then
      begin
        WriteLn('Erreur lors de l''appel du hook : ', lua_tostring(L, -1));
        lua_pop(L, 1);
      end;
    end;
  end;
end;
```

### Utilisation côté script Lua

```lua
-- Enregistrer un callback pour un événement
function onPlayerSpawn(x, y)
    print("Le joueur est apparu en position : " .. x .. ", " .. y)
    -- Code personnalisé du mod
end

-- Enregistrer le callback
registerHook("PlayerSpawn", "onPlayerSpawn")
```

## Rechargement à chaud (Hot Reloading)

Le **hot reloading** permet de modifier les scripts pendant que le jeu tourne.

### Implémentation simple

```pascal
type
  TScriptWatcher = class
  private
    FWatchedFiles: TStringList;
    FLastModified: TStringList;
    FLuaState: Plua_State;
  public
    procedure AddScript(const Filename: string);
    procedure CheckForChanges;
    procedure ReloadScript(const Filename: string);
  end;

procedure TScriptWatcher.CheckForChanges;
var
  i: Integer;
  currentTime: TDateTime;
  filename: string;
begin
  for i := 0 to FWatchedFiles.Count - 1 do
  begin
    filename := FWatchedFiles[i];
    currentTime := FileAge(filename);

    if currentTime <> StrToDateTime(FLastModified[i]) then
    begin
      WriteLn('Script modifié : ', filename);
      ReloadScript(filename);
      FLastModified[i] := DateTimeToStr(currentTime);
    end;
  end;
end;
```

### Utilisation multi-plateforme

**Sur Windows**, utilisez `FindFirstChangeNotification` pour surveiller les dossiers.

**Sur Linux**, utilisez `inotify` pour des notifications en temps réel.

```pascal
{$IFDEF WINDOWS}
  // Code Windows avec FindFirstChangeNotification
{$ENDIF}
{$IFDEF LINUX}
  // Code Linux avec inotify
{$ENDIF}
```

## Sécurité et sandboxing

Lorsque vous permettez l'exécution de scripts, la sécurité est cruciale.

### Limiter l'accès aux fonctions dangereuses

```pascal
procedure SandboxLuaEnvironment(L: Plua_State);
begin
  // Désactiver les fonctions dangereuses
  lua_pushnil(L);
  lua_setglobal(L, 'dofile');     // Empêcher le chargement de fichiers arbitraires

  lua_pushnil(L);
  lua_setglobal(L, 'loadfile');   // Idem

  lua_pushnil(L);
  lua_setglobal(L, 'require');    // Contrôler les imports

  // Limiter l'accès au système de fichiers
  lua_getglobal(L, 'io');
  lua_pushnil(L);
  lua_setfield(L, -2, 'popen');   // Empêcher l'exécution de commandes
  lua_pop(L, 1);
end;
```

### Limites de temps d'exécution

Pour éviter les scripts infinis :

```pascal
procedure SetExecutionTimeout(L: Plua_State; TimeoutMS: Integer);
begin
  // Utiliser lua_sethook pour interrompre après un certain temps
  lua_sethook(L, @TimeoutHook, LUA_MASKCOUNT, 100000);
end;

procedure TimeoutHook(L: Plua_State; ar: Plua_Debug); cdecl;
begin
  // Vérifier si le temps limite est dépassé
  if GetTickCount > StartTime + TimeoutMS then
    luaL_error(L, 'Script timeout exceeded');
end;
```

### Limites de mémoire

```pascal
procedure SetMemoryLimit(L: Plua_State; MaxMemoryMB: Integer);
begin
  // Configurer l'allocateur personnalisé avec limite
  lua_setallocf(L, @CustomAllocator, Pointer(MaxMemoryMB));
end;
```

## Formats de données pour le modding

### JSON pour la configuration

Les fichiers JSON sont parfaits pour les configurations de mods :

```pascal
uses
  fpjson, jsonparser;

function LoadModConfig(const Filename: string): TJSONObject;
var
  JSONData: TJSONData;
  FileContent: string;
begin
  FileContent := ReadFileToString(Filename);
  JSONData := GetJSON(FileContent);

  if JSONData is TJSONObject then
    Result := TJSONObject(JSONData)
  else
    Result := nil;
end;
```

### XML pour les données hiérarchiques

```pascal
uses
  DOM, XMLRead;

procedure LoadModDataFromXML(const Filename: string);
var
  Doc: TXMLDocument;
  RootNode: TDOMNode;
begin
  ReadXMLFile(Doc, Filename);
  try
    RootNode := Doc.DocumentElement;
    // Traiter les données
  finally
    Doc.Free;
  end;
end;
```

### Formats binaires personnalisés

Pour des performances optimales :

```pascal
type
  TModDataHeader = packed record
    Magic: array[0..3] of Char;  // 'MODD'
    Version: Word;
    DataSize: Cardinal;
  end;

procedure SaveModData(const Filename: string; Data: TModData);
var
  F: TFileStream;
  Header: TModDataHeader;
begin
  F := TFileStream.Create(Filename, fmCreate);
  try
    Header.Magic := 'MODD';
    Header.Version := 1;
    Header.DataSize := SizeOf(Data);

    F.Write(Header, SizeOf(Header));
    F.Write(Data, SizeOf(Data));
  finally
    F.Free;
  end;
end;
```

## Gestion des conflits entre mods

Lorsque plusieurs mods sont chargés, des conflits peuvent survenir.

### Ordre de chargement

```pascal
procedure TModManager.SortModsByLoadOrder;
begin
  FMods.Sort(@CompareModLoadOrder);
end;

function CompareModLoadOrder(Item1, Item2: Pointer): Integer;
var
  Mod1, Mod2: TMod;
begin
  Mod1 := TMod(Item1);
  Mod2 := TMod(Item2);
  Result := Mod1.LoadOrder - Mod2.LoadOrder;
end;
```

### Système de priorité

```pascal
type
  TResourceOverride = class
  private
    FResourcePath: string;
    FModName: string;
    FPriority: Integer;
  end;

function TModManager.GetResource(const Path: string): TResourceOverride;
var
  i: Integer;
  Override: TResourceOverride;
  BestOverride: TResourceOverride;
begin
  BestOverride := nil;

  for i := 0 to FOverrides.Count - 1 do
  begin
    Override := TResourceOverride(FOverrides[i]);

    if Override.ResourcePath = Path then
    begin
      if (BestOverride = nil) or (Override.Priority > BestOverride.Priority) then
        BestOverride := Override;
    end;
  end;

  Result := BestOverride;
end;
```

## Console de développement

Une console intégrée est essentielle pour tester les scripts.

### Interface de console basique

```pascal
type
  TDevConsole = class
  private
    FLuaState: Plua_State;
    FHistory: TStringList;
    FVisible: Boolean;
  public
    procedure ExecuteCommand(const Command: string);
    procedure Show;
    procedure Hide;
    procedure AddMessage(const Msg: string);
  end;

procedure TDevConsole.ExecuteCommand(const Command: string);
begin
  FHistory.Add(Command);

  // Exécuter comme code Lua
  if luaL_dostring(FLuaState, PChar(Command)) <> 0 then
  begin
    AddMessage('Erreur : ' + lua_tostring(FLuaState, -1));
    lua_pop(FLuaState, 1);
  end;
end;
```

### Commandes intégrées

```pascal
procedure RegisterConsoleCommands(L: Plua_State);
begin
  // Recharger tous les scripts
  lua_register(L, 'reload', @CmdReload);

  // Lister les mods chargés
  lua_register(L, 'listmods', @CmdListMods);

  // Informations de debug
  lua_register(L, 'debug', @CmdDebug);
end;
```

## Débogage des scripts

### Messages de debug

```lua
-- Dans les scripts Lua
function debug_print(message)
    if DEBUG_MODE then
        print("[DEBUG] " .. message)
    end
end
```

### Inspection de variables

```pascal
procedure InspectLuaVariable(L: Plua_State; const VarName: string);
begin
  lua_getglobal(L, PChar(VarName));

  case lua_type(L, -1) of
    LUA_TNIL: WriteLn(VarName, ' = nil');
    LUA_TBOOLEAN: WriteLn(VarName, ' = ', lua_toboolean(L, -1));
    LUA_TNUMBER: WriteLn(VarName, ' = ', lua_tonumber(L, -1));
    LUA_TSTRING: WriteLn(VarName, ' = "', lua_tostring(L, -1), '"');
    LUA_TTABLE: WriteLn(VarName, ' = table');
    LUA_TFUNCTION: WriteLn(VarName, ' = function');
  end;

  lua_pop(L, 1);
end;
```

## Distribution et partage de mods

### Empaquetage de mods

```pascal
procedure PackageMod(const ModPath, OutputFile: string);
var
  Zipper: TZipper;
begin
  Zipper := TZipper.Create;
  try
    Zipper.FileName := OutputFile;
    Zipper.AddDirectory(ModPath);
    Zipper.ZipAllFiles;
  finally
    Zipper.Free;
  end;
end;
```

### Installation automatique

```pascal
procedure InstallMod(const ModFile, ModsDirectory: string);
var
  UnZipper: TUnZipper;
  ModName: string;
begin
  ModName := ExtractFileName(ModFile);
  ModName := ChangeFileExt(ModName, '');

  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := ModFile;
    UnZipper.OutputPath := ModsDirectory + PathDelim + ModName;
    UnZipper.UnZipAllFiles;
  finally
    UnZipper.Free;
  end;
end;
```

### Plateforme de partage (Steam Workshop, Mod.io)

Pour intégrer avec des plateformes existantes :

**Steam Workshop** : Utiliser le Steamworks SDK  
**Mod.io** : API REST simple  

```pascal
procedure UploadModToModIO(const ModPath: string; APIKey: string);
var
  HTTPClient: TFPHTTPClient;
  FormData: TMultipartFormData;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    FormData := TMultipartFormData.Create;
    try
      FormData.AddFile('file', ModPath + '/mod.zip');
      FormData.AddField('api_key', APIKey);

      HTTPClient.FormPost('https://api.mod.io/v1/games/xxx/mods', FormData);
    finally
      FormData.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;
```

## Bonnes pratiques

### Documentation pour les moddeurs

Créez une documentation claire :

1. **Guide de démarrage** : Comment créer son premier mod
2. **Référence API** : Liste de toutes les fonctions disponibles
3. **Exemples** : Mods simples commentés
4. **FAQ** : Problèmes courants et solutions

### Versioning de l'API

```pascal
const
  MODDING_API_VERSION = '1.0.0';

procedure CheckModCompatibility(Mod: TMod);
begin
  if Mod.RequiredAPIVersion <> MODDING_API_VERSION then
    ShowWarning('Ce mod nécessite une version différente de l''API');
end;
```

### Performances

- **Limiter les appels script ↔ natif** : Ils ont un coût
- **Cacher les résultats** : Éviter de recalculer constamment
- **Précharger les ressources** : Au démarrage plutôt qu'à la demande
- **Profiler les scripts** : Identifier les goulots d'étranglement

### Communauté

- **Forums dédiés** : Pour que les moddeurs s'entraident
- **Discord/Chat** : Communication en temps réel
- **Concours de mods** : Stimuler la créativité
- **Mettre en avant** : Présenter les meilleurs mods

## Conclusion

Le scripting et le modding transforment votre jeu ou application d'un produit fermé en une plateforme vivante et évolutive. Avec FreePascal et Lazarus, vous disposez de tous les outils nécessaires pour créer un système de modding robuste et multi-plateforme.

Les clés du succès sont :
- Une API claire et bien documentée
- Des outils faciles à utiliser pour les moddeurs
- Un équilibre entre flexibilité et sécurité
- Une communauté engagée et soutenue

N'oubliez pas que les meilleurs mods viennent souvent de votre communauté - donnez-leur les moyens de créer et vous serez surpris des résultats !

⏭️ [Optimisation pour jeux](/23-developpement-jeux/08-optimisation-pour-jeux.md)
