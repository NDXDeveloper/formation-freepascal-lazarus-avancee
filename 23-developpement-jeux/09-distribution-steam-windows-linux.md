🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.9 Distribution Steam (Windows/Linux)

## Introduction

Steam est la plateforme de distribution de jeux la plus populaire au monde, avec plus de 120 millions d'utilisateurs actifs. Distribuer votre jeu sur Steam vous donne accès à un marché immense et à des outils puissants pour gérer votre communauté.

### Pourquoi distribuer sur Steam ?

**Avantages** :
- **Visibilité** : Des millions de joueurs potentiels
- **Infrastructure** : Téléchargement, mises à jour automatiques, sauvegarde cloud
- **Communauté** : Forums, guides, ateliers (Workshop)
- **Fonctionnalités** : Succès, classements, multijoueur
- **Paiement** : Gestion des transactions et de la TVA
- **Multi-plateforme** : Windows, Linux, macOS sur une seule page de store

**Inconvénients** :
- **Coût** : 100$ par jeu (remboursables après 1000$ de ventes)
- **Commission** : Steam prend 30% des ventes (réduit à 25% puis 20% après certains seuils)
- **Processus de validation** : Votre jeu doit être approuvé
- **Exigences techniques** : Intégration obligatoire du SDK Steamworks

### Prérequis

- Un jeu terminé et fonctionnel sur Windows et/ou Linux
- 100$ pour les frais d'inscription Steam Direct
- Des informations fiscales et bancaires
- Du matériel marketing (captures d'écran, vidéos, artwork)

## Créer un compte Steamworks

### 1. Inscription Steam Direct

1. Créez un compte Steam si vous n'en avez pas
2. Allez sur [partner.steamgames.com](https://partner.steamgames.com)
3. Connectez-vous et créez un compte Steamworks
4. Payez les 100$ de frais d'inscription

### 2. Vérification d'identité

Steam nécessite une vérification pour prévenir la fraude :
- Vérification de l'adresse email
- Confirmation de numéro de téléphone
- Informations bancaires (pour les paiements)
- Informations fiscales (formulaire W-8 ou W-9 selon votre pays)

### 3. Créer votre page d'application

Dans Steamworks :
1. Cliquez sur "Create new app"
2. Choisissez "Game"
3. Remplissez les informations de base
4. Vous obtiendrez un **App ID** unique

Conservez cet App ID, vous en aurez besoin pour l'intégration.

## Intégration du SDK Steamworks

### Télécharger le SDK

1. Depuis votre compte Steamworks, téléchargez le SDK
2. Extrayez l'archive dans un dossier de développement

Structure du SDK :
```
steamworks_sdk/
├── public/           # Headers C++
├── redistributable_bin/
│   ├── win64/       # steam_api64.dll
│   ├── linux64/     # libsteam_api.so
│   └── ...
├── tools/
└── sdk/
```

### Créer un binding Pascal

Le SDK Steamworks est en C++, nous devons créer des bindings pour Pascal.

#### Fichier steam_api.pas (version simplifiée)

```pascal
unit steam_api;

{$mode objfpc}{$H+}

interface

const
  {$IFDEF WINDOWS}
  STEAM_API_LIB = 'steam_api64.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  STEAM_API_LIB = 'libsteam_api.so';
  {$ENDIF}

type
  HSteamUser = Integer;
  HSteamPipe = Integer;
  AppId_t = Cardinal;

  ISteamClient = Pointer;
  ISteamUser = Pointer;
  ISteamFriends = Pointer;
  ISteamUtils = Pointer;
  ISteamUserStats = Pointer;

// Initialisation
function SteamAPI_Init: Boolean; cdecl; external STEAM_API_LIB;
procedure SteamAPI_Shutdown; cdecl; external STEAM_API_LIB;
procedure SteamAPI_RunCallbacks; cdecl; external STEAM_API_LIB;

// Obtenir les interfaces
function SteamAPI_GetHSteamUser: HSteamUser; cdecl; external STEAM_API_LIB;
function SteamAPI_GetHSteamPipe: HSteamPipe; cdecl; external STEAM_API_LIB;

// Interface User
function SteamAPI_ISteamUser_GetSteamID(SelfPtr: ISteamUser): UInt64; cdecl; external STEAM_API_LIB;
function SteamAPI_ISteamUser_BLoggedOn(SelfPtr: ISteamUser): Boolean; cdecl; external STEAM_API_LIB;

// Interface Friends
function SteamAPI_ISteamFriends_GetPersonaName(SelfPtr: ISteamFriends): PChar; cdecl; external STEAM_API_LIB;

// Interface UserStats (succès)
function SteamAPI_ISteamUserStats_SetAchievement(SelfPtr: ISteamUserStats; pchName: PChar): Boolean; cdecl; external STEAM_API_LIB;
function SteamAPI_ISteamUserStats_StoreStats(SelfPtr: ISteamUserStats): Boolean; cdecl; external STEAM_API_LIB;

implementation

end.
```

### Wrapper de haut niveau

Créons une interface plus facile à utiliser :

```pascal
unit SteamWrapper;

{$mode objfpc}{$H+}

interface

uses
  steam_api;

type
  TSteamAPI = class
  private
    FInitialized: Boolean;
    FAppID: AppId_t;
  public
    constructor Create(AppID: AppId_t);
    destructor Destroy; override;

    function Initialize: Boolean;
    procedure RunCallbacks;

    function GetUserName: string;
    function GetSteamID: UInt64;
    function IsLoggedOn: Boolean;

    // Succès
    function UnlockAchievement(const AchievementName: string): Boolean;

    property Initialized: Boolean read FInitialized;
  end;

var
  Steam: TSteamAPI;

implementation

constructor TSteamAPI.Create(AppID: AppId_t);
begin
  inherited Create;
  FAppID := AppID;
  FInitialized := False;
end;

destructor TSteamAPI.Destroy;
begin
  if FInitialized then
    SteamAPI_Shutdown;
  inherited;
end;

function TSteamAPI.Initialize: Boolean;
begin
  Result := SteamAPI_Init;
  FInitialized := Result;

  if not Result then
    WriteLn('Erreur : Impossible d''initialiser Steam API');
end;

procedure TSteamAPI.RunCallbacks;
begin
  if FInitialized then
    SteamAPI_RunCallbacks;
end;

function TSteamAPI.GetUserName: string;
var
  Friends: ISteamFriends;
begin
  if not FInitialized then
    Exit('');

  Friends := SteamInternal_FindOrCreateUserInterface(
    SteamAPI_GetHSteamUser,
    STEAMFRIENDS_INTERFACE_VERSION
  );

  Result := string(SteamAPI_ISteamFriends_GetPersonaName(Friends));
end;

function TSteamAPI.GetSteamID: UInt64;
var
  User: ISteamUser;
begin
  if not FInitialized then
    Exit(0);

  User := SteamInternal_FindOrCreateUserInterface(
    SteamAPI_GetHSteamUser,
    STEAMUSER_INTERFACE_VERSION
  );

  Result := SteamAPI_ISteamUser_GetSteamID(User);
end;

function TSteamAPI.IsLoggedOn: Boolean;
var
  User: ISteamUser;
begin
  if not FInitialized then
    Exit(False);

  User := SteamInternal_FindOrCreateUserInterface(
    SteamAPI_GetHSteamUser,
    STEAMUSER_INTERFACE_VERSION
  );

  Result := SteamAPI_ISteamUser_BLoggedOn(User);
end;

function TSteamAPI.UnlockAchievement(const AchievementName: string): Boolean;
var
  UserStats: ISteamUserStats;
begin
  if not FInitialized then
    Exit(False);

  UserStats := SteamInternal_FindOrCreateUserInterface(
    SteamAPI_GetHSteamUser,
    STEAMUSERSTATS_INTERFACE_VERSION
  );

  Result := SteamAPI_ISteamUserStats_SetAchievement(UserStats, PChar(AchievementName));

  if Result then
    SteamAPI_ISteamUserStats_StoreStats(UserStats);
end;

end.
```

### Utilisation dans votre jeu

```pascal
program MonJeu;

uses
  SteamWrapper;

const
  MY_APP_ID = 480; // Remplacez par votre App ID réel

begin
  // Créer l'instance Steam
  Steam := TSteamAPI.Create(MY_APP_ID);

  // Initialiser
  if not Steam.Initialize then
  begin
    WriteLn('Le jeu doit être lancé depuis Steam');
    Halt(1);
  end;

  WriteLn('Bienvenue ', Steam.GetUserName);

  // Boucle principale du jeu
  while GameRunning do
  begin
    // IMPORTANT : Appeler à chaque frame
    Steam.RunCallbacks;

    UpdateGame;
    RenderGame;

    // Débloquer un succès
    if PlayerWinsLevel then
      Steam.UnlockAchievement('ACH_WIN_ONE_GAME');
  end;

  // Nettoyage
  Steam.Free;
end.
```

## Fichier steam_appid.txt

Pour tester en développement, créez un fichier `steam_appid.txt` à côté de votre exécutable :

```
480
```

Remplacez 480 par votre App ID. Ce fichier indique au SDK quel jeu vous développez.

**⚠️ IMPORTANT** : Ne distribuez JAMAIS ce fichier avec votre jeu ! Il est uniquement pour le développement.

## Préparer les builds

### Build Windows

```pascal
// Compiler pour Windows 64-bit
fpc -Twin64 -O3 MonJeu.pas

// Structure du dossier
MonJeu_Windows/
├── MonJeu.exe
├── steam_api64.dll
├── data/
└── ...
```

### Build Linux

```pascal
// Compiler pour Linux 64-bit
fpc -Tlinux -O3 MonJeu.pas

// Structure du dossier
MonJeu_Linux/
├── MonJeu (exécutable)
├── libsteam_api.so
├── data/
└── ...
```

### Script de build multi-plateforme

```bash
#!/bin/bash
# build_all.sh

# Nettoyer
rm -rf builds/
mkdir -p builds/windows builds/linux

# Build Windows
echo "Building Windows version..."
fpc -Twin64 -O3 -FEbuilds/windows/ MonJeu.pas
cp redistributable_bin/win64/steam_api64.dll builds/windows/
cp -r data builds/windows/

# Build Linux
echo "Building Linux version..."
fpc -Tlinux -O3 -FEbuilds/linux/ MonJeu.pas
cp redistributable_bin/linux64/libsteam_api.so builds/linux/
cp -r data builds/linux/
chmod +x builds/linux/MonJeu

echo "Build complete!"
```

## Utiliser SteamPipe (upload des builds)

### 1. Installer les outils

Téléchargez les **Steamworks SDK Tools** depuis votre compte Steamworks.

### 2. Structure des fichiers de configuration

Créez un dossier `steamworks_upload/` :

```
steamworks_upload/
├── app_build_<appid>.vdf
├── depot_build_<depotid>_windows.vdf
├── depot_build_<depotid>_linux.vdf
├── content_windows/
│   └── (contenu Windows)
└── content_linux/
    └── (contenu Linux)
```

### 3. Fichier app_build

`app_build_480.vdf` (remplacez 480 par votre App ID) :

```vdf
"appbuild"
{
  "appid" "480"
  "desc" "Version 1.0.0" // Description de cette build
  "buildoutput" "..\output\" // Où stocker les logs
  "contentroot" ".\" // Dossier racine du contenu
  "setlive" "" // Branche (vide = default, "beta" pour beta, etc.)
  "preview" "0" // 1 pour tester sans uploader
  "local" "" // Chemin local optionnel

  "depots"
  {
    "481" // Depot ID Windows (exemple)
    {
      "file" "depot_build_481_windows.vdf"
    }
    "482" // Depot ID Linux (exemple)
    {
      "file" "depot_build_482_linux.vdf"
    }
  }
}
```

### 4. Fichiers depot

`depot_build_481_windows.vdf` :

```vdf
"DepotBuildConfig"
{
  "DepotID" "481"
  "ContentRoot" "..\builds\windows\"

  "FileMapping"
  {
    "LocalPath" "*"
    "DepotPath" "."
    "recursive" "1"
  }

  "FileExclusion" "*.pdb"
  "FileExclusion" "*.log"
}
```

`depot_build_482_linux.vdf` :

```vdf
"DepotBuildConfig"
{
  "DepotID" "482"
  "ContentRoot" "..\builds\linux\"

  "FileMapping"
  {
    "LocalPath" "*"
    "DepotPath" "."
    "recursive" "1"
  }

  "FileExclusion" "*.log"
}
```

### 5. Script d'upload

**Windows** (`upload.bat`) :

```batch
@echo off
cd steamworks_sdk\tools\ContentBuilder

steamcmd.exe +login votre_username +run_app_build ..\..\..\steamworks_upload\app_build_480.vdf +quit

pause
```

**Linux** (`upload.sh`) :

```bash
#!/bin/bash
cd steamworks_sdk/tools/ContentBuilder

./steamcmd.sh +login votre_username +run_app_build ../../../steamworks_upload/app_build_480.vdf +quit
```

### 6. Premier upload

```bash
# Lancer le script
./upload.sh

# Entrer votre mot de passe Steam
# Entrer le code Steam Guard si nécessaire
# Attendre la fin de l'upload
```

L'upload peut prendre du temps selon la taille de votre jeu.

## Configuration de la page Steam Store

### 1. Informations de base

Dans Steamworks, section "Store Presence" :

**Général** :
- Nom du jeu
- Description courte (300 caractères)
- Description complète
- Langues supportées
- Site web
- Lien vers les forums

**Prix** :
- Prix de base en USD (Steam convertit automatiquement)
- Prix par région (optionnel)
- Réductions éventuelles

### 2. Assets graphiques requis

**Header capsule** :
- Taille : 460 x 215 pixels
- Format : PNG ou JPG
- Utilisé : Page du store, bibliothèque

**Small capsule** :
- Taille : 231 x 87 pixels
- Utilisé : Recherche, recommandations

**Main capsule** :
- Taille : 616 x 353 pixels
- Utilisé : Page d'accueil Steam

**Hero capsule** :
- Taille : 1920 x 622 pixels (minimum)
- Utilisé : Grande bannière en haut de la page

**Library assets** :
- Hero : 3840 x 1240 pixels
- Logo : 1280 x 720 pixels (fond transparent)

**Captures d'écran** :
- Minimum : 5 captures
- Recommandé : 1920 x 1080 pixels
- Format : JPG ou PNG

**Trailer vidéo** :
- Au moins une vidéo
- YouTube ou upload direct
- 1080p recommandé

### 3. Exemple de structure marketing

```
marketing/
├── capsules/
│   ├── header_460x215.png
│   ├── small_231x87.png
│   ├── main_616x353.png
│   └── hero_1920x622.png
├── library/
│   ├── hero_3840x1240.png
│   └── logo_1280x720.png
├── screenshots/
│   ├── screenshot_01.jpg
│   ├── screenshot_02.jpg
│   ├── screenshot_03.jpg
│   ├── screenshot_04.jpg
│   └── screenshot_05.jpg
└── trailer/
    └── trailer_1080p.mp4
```

## Fonctionnalités Steamworks

### Succès (Achievements)

#### 1. Définir les succès dans Steamworks

Dans la section "Stats & Achievements" :
- Créez chaque succès
- Donnez un ID unique (ex: ACH_WIN_ONE_GAME)
- Ajoutez un nom et une description
- Uploadez une icône (64x64 pixels)

#### 2. Implémentation dans le code

```pascal
procedure UnlockAchievement(const AchievementID: string);
begin
  if Steam.Initialized then
  begin
    if Steam.UnlockAchievement(AchievementID) then
      WriteLn('Succès débloqué : ', AchievementID);
  end;
end;

// Dans votre jeu
if Player.Level >= 10 then
  UnlockAchievement('ACH_REACH_LEVEL_10');

if Player.CompletedAllLevels then
  UnlockAchievement('ACH_COMPLETE_GAME');
```

### Classements (Leaderboards)

```pascal
// Créer dans Steamworks d'abord

procedure SubmitScore(LeaderboardName: string; Score: Integer);
var
  UserStats: ISteamUserStats;
begin
  if not Steam.Initialized then Exit;

  UserStats := GetUserStatsInterface;

  // Soumettre le score
  SteamAPI_ISteamUserStats_UploadLeaderboardScore(
    UserStats,
    PChar(LeaderboardName),
    Score,
    nil, // Détails additionnels
    0    // Nombre de détails
  );
end;
```

### Sauvegarde Cloud

```pascal
function SaveToCloud(const Filename: string; Data: TBytes): Boolean;
var
  RemoteStorage: ISteamRemoteStorage;
begin
  if not Steam.Initialized then Exit(False);

  RemoteStorage := GetRemoteStorageInterface;

  Result := SteamAPI_ISteamRemoteStorage_FileWrite(
    RemoteStorage,
    PChar(Filename),
    @Data[0],
    Length(Data)
  );
end;

function LoadFromCloud(const Filename: string; out Data: TBytes): Boolean;
var
  RemoteStorage: ISteamRemoteStorage;
  Size: Integer;
begin
  if not Steam.Initialized then Exit(False);

  RemoteStorage := GetRemoteStorageInterface;

  Size := SteamAPI_ISteamRemoteStorage_GetFileSize(RemoteStorage, PChar(Filename));

  if Size > 0 then
  begin
    SetLength(Data, Size);
    Result := SteamAPI_ISteamRemoteStorage_FileRead(
      RemoteStorage,
      PChar(Filename),
      @Data[0],
      Size
    ) = Size;
  end
  else
    Result := False;
end;
```

### Overlay

L'overlay Steam s'affiche automatiquement (Shift+Tab par défaut).

Pour ouvrir des pages spécifiques :

```pascal
procedure OpenSteamOverlay(const URL: string);
var
  Friends: ISteamFriends;
begin
  if not Steam.Initialized then Exit;

  Friends := GetFriendsInterface;

  SteamAPI_ISteamFriends_ActivateGameOverlayToWebPage(
    Friends,
    PChar(URL)
  );
end;

// Exemples
OpenSteamOverlay('https://store.steampowered.com/app/480');
OpenSteamOverlay('steamcommunity://friends');
```

### DLC (Contenu téléchargeable)

#### Vérifier si un DLC est possédé

```pascal
function HasDLC(DLCAppID: AppId_t): Boolean;
var
  Apps: ISteamApps;
begin
  if not Steam.Initialized then Exit(False);

  Apps := GetAppsInterface;

  Result := SteamAPI_ISteamApps_BIsDlcInstalled(Apps, DLCAppID);
end;

// Dans votre jeu
if HasDLC(481) then
  EnableBonusContent;
```

## Tests et validation

### Tests en développement

1. **Mode hors ligne** : Testez sans connexion Steam
2. **Différents comptes** : Créez des comptes de test
3. **Différentes configurations** : Windows/Linux, différentes résolutions

### Checklist avant soumission

- [ ] Le jeu se lance et fonctionne correctement
- [ ] L'intégration Steam fonctionne
- [ ] Les succès se débloquent correctement
- [ ] Les sauvegardes cloud fonctionnent
- [ ] Testé sur Windows 64-bit
- [ ] Testé sur Linux (Ubuntu recommandé)
- [ ] Tous les assets marketing sont uploadés
- [ ] Les descriptions sont complètes et sans fautes
- [ ] Le prix est défini
- [ ] Les informations légales sont remplies

### Processus de révision Steam

1. **Soumission** : Cliquez sur "Prepare for Release"
2. **Validation automatique** : Steam vérifie les exigences basiques
3. **Révision humaine** : Peut prendre quelques jours
4. **Approbation ou refus** : Vous recevez un email

Raisons courantes de refus :
- Assets marketing de mauvaise qualité
- Jeu non fonctionnel
- Contenu inapproprié
- Violation des règles Steam

## Lancement et post-lancement

### Préparer le lancement

**2-3 semaines avant** :
- Créez une page "Coming Soon"
- Commencez le marketing (réseaux sociaux, forums)
- Contactez des streamers/youtubeurs

**1 semaine avant** :
- Envoyez des clés de presse
- Préparez du contenu pour le jour J
- Testez une dernière fois

**Jour du lancement** :
- Activez le jeu dans Steamworks
- Annoncez sur tous vos canaux
- Répondez aux premiers retours

### Mises à jour

#### Upload d'une mise à jour

```bash
# Même processus que l'upload initial
# Modifiez la description dans app_build.vdf
"desc" "Version 1.1.0 - Correction de bugs"

# Uploadez
./upload.sh
```

#### Branches de build

Créez des branches pour tester avant de déployer :

Dans `app_build.vdf` :
```vdf
"setlive" "beta" // Branche beta
```

Les joueurs peuvent choisir la branche dans les propriétés du jeu.

### Gérer les retours

**Steam Workshop** :
- Permettez aux joueurs de créer du contenu
- Intégration du Workshop dans votre jeu

**Forums Steam** :
- Répondez aux questions
- Recueillez les bugs
- Écoutez les suggestions

**Mises à jour régulières** :
- Corrigez les bugs rapidement
- Ajoutez du contenu
- Améliorez selon les retours

## Spécificités multi-plateforme

### Windows

```pascal
{$IFDEF WINDOWS}
procedure InitSteamWindows;
begin
  // Charger steam_api64.dll
  SteamLibHandle := LoadLibrary('steam_api64.dll');

  if SteamLibHandle = 0 then
  begin
    MessageBox(0, 'steam_api64.dll introuvable', 'Erreur', MB_OK);
    Halt(1);
  end;
end;
{$ENDIF}
```

### Linux

```pascal
{$IFDEF LINUX}
procedure InitSteamLinux;
begin
  // Charger libsteam_api.so
  SteamLibHandle := LoadLibrary('libsteam_api.so');

  if SteamLibHandle = NilHandle then
  begin
    WriteLn('Erreur : libsteam_api.so introuvable');
    WriteLn('Assurez-vous que la bibliothèque est dans le dossier du jeu');
    Halt(1);
  end;
end;
{$ENDIF}
```

### Script de lancement Linux

Créez `MonJeu.sh` :

```bash
#!/bin/bash

# Obtenir le dossier du script
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Ajouter le dossier au LD_LIBRARY_PATH pour trouver libsteam_api.so
export LD_LIBRARY_PATH="$SCRIPT_DIR:$LD_LIBRARY_PATH"

# Lancer le jeu
cd "$SCRIPT_DIR"
./MonJeu "$@"
```

Dans votre `depot_build_linux.vdf`, assurez-vous de marquer le script comme exécutable.

## Statistiques et analytics

### Dashboard Steamworks

Accédez aux statistiques détaillées :
- Ventes par jour/semaine/mois
- Joueurs actifs
- Temps de jeu moyen
- Taux de complétion des succès
- Régions des joueurs
- Revenus

### Utiliser les stats dans votre jeu

```pascal
procedure TrackGameplayStats;
var
  UserStats: ISteamUserStats;
begin
  // Incrémenter une statistique
  SteamAPI_ISteamUserStats_SetStat_Int(
    UserStats,
    'NumGamesPlayed',
    GamesPlayed
  );

  // Sauvegarder
  SteamAPI_ISteamUserStats_StoreStats(UserStats);
end;
```

## Conseils et bonnes pratiques

### Marketing

1. **Page store attractive** : Investissez dans de bons visuels
2. **Démo** : Proposez une démo si possible
3. **Réductions** : Participez aux soldes Steam
4. **Contenu régulier** : Ajoutez du contenu post-lancement

### Technique

1. **Testez intensivement** : Sur différentes configurations
2. **Performance** : Optimisez pour du matériel modeste
3. **Support Linux** : Même avec peu d'utilisateurs, c'est apprécié
4. **Logs** : Implémentez un système de logs pour déboguer

### Communauté

1. **Répondez rapidement** : Aux bugs et questions
2. **Soyez transparent** : Sur les mises à jour et problèmes
3. **Écoutez** : Les retours sont précieux
4. **Remerciez** : Votre communauté est votre force

## Ressources utiles

### Documentation officielle

- [Steamworks Documentation](https://partner.steamgames.com/doc/home)
- [Steamworks SDK](https://partner.steamgames.com/downloads/list)
- [Steam Community Guidelines](https://partner.steamgames.com/doc/gettingstarted/guidelines)

### Outils

- **SteamDB** : Statistiques et informations sur les jeux Steam
- **Steam Spy** : Analytics de ventes (moins précis depuis 2018)
- **Steam Calculator** : Estimer le prix optimal

### Communautés

- Forums Steamworks
- Reddit : r/gamedev, r/Steam
- Discord : Serveurs de développement de jeux

## Conclusion

Distribuer votre jeu FreePascal sur Steam est tout à fait possible et peut vous ouvrir les portes d'un marché immense. Bien que l'intégration du SDK nécessite un peu de travail initial avec les bindings C, une fois en place, vous avez accès à toutes les fonctionnalités puissantes de Steam.

### Points clés à retenir

✅ **Préparation** : Investissez du temps dans la page store et le marketing  
✅ **Multi-plateforme** : Supportez au moins Windows et Linux  
✅ **Intégration** : Testez soigneusement toutes les fonctionnalités Steam  
✅ **Qualité** : Un jeu bien fini vaut mieux qu'un jeu riche mais buggé  
✅ **Communauté** : Restez à l'écoute et mettez à jour régulièrement

Avec FreePascal et Lazarus, vous pouvez créer des jeux de qualité professionnelle et les distribuer sur la plus grande plateforme de jeux au monde. Bonne chance avec votre projet !

⏭️ [Réalité virtuelle et augmentée](/23-developpement-jeux/10-realite-virtuelle-augmentee.md)
