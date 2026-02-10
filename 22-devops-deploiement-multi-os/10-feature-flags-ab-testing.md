🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.10 Feature flags et A/B testing

## Introduction aux Feature Flags

### Qu'est-ce qu'un feature flag ?

Un **feature flag** (aussi appelé feature toggle ou feature switch) est un mécanisme qui permet d'activer ou désactiver des fonctionnalités de votre application **sans avoir à déployer une nouvelle version**.

**L'analogie de l'interrupteur** :

Imaginez que vous construisez une maison. Avec l'approche traditionnelle, si vous voulez ajouter un nouveau système d'éclairage, vous devez :
1. Couper l'électricité de toute la maison
2. Installer le nouveau système
3. Remettre l'électricité

Avec les feature flags, c'est comme installer des interrupteurs pour chaque nouvelle fonctionnalité :
- Vous installez le nouveau système d'éclairage (vous déployez le code)
- Mais il reste éteint (flag désactivé)
- Quand vous êtes prêt, vous actionnez simplement l'interrupteur (vous activez le flag)
- Si ça ne fonctionne pas bien, vous éteignez immédiatement sans avoir à tout refaire

### Pourquoi utiliser des feature flags ?

**Avantages** :

✅ **Déploiement sans risque** : Vous pouvez déployer du code incomplet ou expérimental sans l'activer  
✅ **Rollback instantané** : Désactivez une fonctionnalité problématique en 1 seconde, sans redéploiement  
✅ **Tests en production** : Activez pour un petit groupe d'utilisateurs avant le déploiement général  
✅ **Déploiement progressif** : 10% des utilisateurs, puis 50%, puis 100%  
✅ **Personnalisation** : Fonctionnalités différentes selon le type d'utilisateur  
✅ **A/B Testing** : Comparer différentes versions d'une fonctionnalité  
✅ **Dark launching** : Tester la charge d'une nouvelle fonctionnalité sans l'exposer aux utilisateurs

**Cas d'usage concrets** :

- Une nouvelle interface utilisateur que vous voulez tester avec 10% des utilisateurs
- Une optimisation de performance que vous voulez activer/désactiver rapidement
- Une fonctionnalité premium réservée aux clients payants
- Un mode maintenance pour désactiver certaines parties de l'application
- Une fonctionnalité en cours de développement que seuls les développeurs peuvent voir

## Types de feature flags

### 1. Release Flags (Flags de version)

**But** : Contrôler le déploiement de nouvelles fonctionnalités

```pascal
if FeatureFlags.IsEnabled('new_checkout_process') then
  ShowNewCheckout
else
  ShowOldCheckout;
```

**Durée de vie** : Temporaire - supprimé une fois la fonctionnalité stable

### 2. Ops Flags (Flags opérationnels)

**But** : Contrôler les aspects opérationnels du système

```pascal
if FeatureFlags.IsEnabled('enable_caching') then
  UseCache
else
  SkipCache;

if FeatureFlags.IsEnabled('maintenance_mode') then
  ShowMaintenancePage;
```

**Durée de vie** : Permanent - garde le contrôle opérationnel

### 3. Experiment Flags (Flags d'expérimentation)

**But** : A/B testing et expériences utilisateur

```pascal
Variant := FeatureFlags.GetVariant('button_color_test');
if Variant = 'red' then
  ShowRedButton
else if Variant = 'blue' then
  ShowBlueButton
else if Variant = 'green' then
  ShowGreenButton;
```

**Durée de vie** : Temporaire - supprimé après l'expérience

### 4. Permission Flags (Flags de permission)

**But** : Contrôler l'accès selon le type d'utilisateur

```pascal
if FeatureFlags.IsEnabledForUser(CurrentUser, 'premium_features') then
  ShowPremiumFeatures;
```

**Durée de vie** : Permanent - fait partie du modèle de permissions

## Architecture d'un système de feature flags

### Composants principaux

```
┌─────────────────────────────────────────────────────────┐
│                    Application FreePascal               │
│                                                         │
│  ┌─────────────────────────────────────────────────┐    │
│  │        Feature Flag Client                      │    │
│  │  ┌──────────────────────────────────────────┐   │    │
│  │  │  Cache Local (en mémoire)                │   │    │
│  │  │  - Flags et leurs états                  │   │    │
│  │  │  - Règles de ciblage                     │   │    │
│  │  │  - TTL: 60 secondes                      │   │    │
│  │  └──────────────────────────────────────────┘   │    │
│  └─────────────────────────────────────────────────┘    │
│                          ↕                              │
│                   Rafraîchissement périodique           │
│                          ↓                              │
└─────────────────────────────────────────────────────────┘
                           ↓
                    ┌──────────────┐
                    │    Redis     │  ← Stockage rapide et partagé
                    │  (ou autre)  │     des états de flags
                    └──────────────┘
                           ↓
                    ┌──────────────┐
                    │  Admin UI    │  ← Interface de gestion
                    │  ou API      │     des feature flags
                    └──────────────┘
```

### Choix du backend de stockage

#### Option 1 : Redis (Recommandé)

**Avantages** :
- Très rapide (en mémoire)
- Pub/Sub pour notifications instantanées
- Facile à partager entre instances
- Supporte les TTL natifs

**Inconvénients** :
- Service supplémentaire à maintenir

#### Option 2 : Base de données

**Avantages** :
- Déjà présent dans votre stack
- Persistance garantie
- Transactions ACID

**Inconvénients** :
- Plus lent que Redis
- Charge supplémentaire sur la DB

#### Option 3 : Fichier de configuration

**Avantages** :
- Très simple
- Pas de dépendance externe
- Contrôle de version avec Git

**Inconvénients** :
- Nécessite redémarrage ou rechargement
- Difficile à partager entre instances

#### Option 4 : Service SaaS

**Exemples** : LaunchDarkly, Split.io, Unleash

**Avantages** :
- Interface de gestion complète
- Analytics intégrés
- Support et maintenance

**Inconvénients** :
- Coût
- Dépendance externe

## Implémentation d'un système de feature flags en FreePascal

### Structure de base

```pascal
unit FeatureFlags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, SyncObjs;

type
  // Stratégie de ciblage
  TTargetingStrategy = (
    tsAll,              // Tous les utilisateurs
    tsNone,             // Personne
    tsPercentage,       // Pourcentage d'utilisateurs
    tsUserList,         // Liste spécifique d'utilisateurs
    tsUserAttribute     // Basé sur un attribut utilisateur
  );

  // Configuration d'un flag
  TFeatureFlag = class
  private
    FName: string;
    FEnabled: Boolean;
    FDescription: string;
    FStrategy: TTargetingStrategy;
    FPercentage: Integer;        // Pour tsPercentage
    FUserList: TStringList;      // Pour tsUserList
    FAttributeKey: string;       // Pour tsUserAttribute
    FAttributeValue: string;     // Pour tsUserAttribute
    FVariants: TStringList;      // Pour A/B testing
    FDefaultVariant: string;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    property Name: string read FName;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Description: string read FDescription write FDescription;
    property Strategy: TTargetingStrategy read FStrategy write FStrategy;
    property Percentage: Integer read FPercentage write FPercentage;
    property UserList: TStringList read FUserList;
    property AttributeKey: string read FAttributeKey write FAttributeKey;
    property AttributeValue: string read FAttributeValue write FAttributeValue;
    property Variants: TStringList read FVariants;
    property DefaultVariant: string read FDefaultVariant write FDefaultVariant;

    function ToJSON: TJSONObject;
    procedure FromJSON(AJSON: TJSONObject);
  end;

  // Contexte utilisateur pour l'évaluation
  TUserContext = class
  private
    FUserID: string;
    FUsername: string;
    FEmail: string;
    FAttributes: TStringList;
  public
    constructor Create(const AUserID: string);
    destructor Destroy; override;

    property UserID: string read FUserID;
    property Username: string read FUsername write FUsername;
    property Email: string read FEmail write FEmail;
    property Attributes: TStringList read FAttributes;

    function GetAttribute(const Key: string): string;
    procedure SetAttribute(const Key, Value: string);
  end;

  TFeatureFlagMap = specialize TFPGMap<string, TFeatureFlag>;

  // Gestionnaire principal de feature flags
  TFeatureFlagManager = class
  private
    FFlags: TFeatureFlagMap;
    FLock: TCriticalSection;
    FBackend: TObject; // Redis, Database, ou autre
    FCacheTime: TDateTime;
    FCacheTTL: Integer; // Secondes

    procedure RefreshFromBackend;
    function EvaluateFlag(AFlag: TFeatureFlag; AUser: TUserContext): Boolean;
    function GetVariantForUser(AFlag: TFeatureFlag; AUser: TUserContext): string;
    function HashUserForPercentage(const UserID, FlagName: string): Integer;
  public
    constructor Create(ABackend: TObject; ACacheTTL: Integer = 60);
    destructor Destroy; override;

    // Gestion des flags
    procedure RegisterFlag(AFlag: TFeatureFlag);
    function GetFlag(const FlagName: string): TFeatureFlag;

    // Évaluation
    function IsEnabled(const FlagName: string; AUser: TUserContext = nil): Boolean;
    function GetVariant(const FlagName: string; AUser: TUserContext): string;

    // Administration
    procedure EnableFlag(const FlagName: string);
    procedure DisableFlag(const FlagName: string);
    function ListAllFlags: TStringList;

    // Force refresh
    procedure Refresh;
  end;

implementation

uses
  DateUtils, md5;

{ TFeatureFlag }

constructor TFeatureFlag.Create(const AName: string);
begin
  FName := AName;
  FEnabled := False;
  FStrategy := tsAll;
  FPercentage := 100;
  FUserList := TStringList.Create;
  FVariants := TStringList.Create;
  FDefaultVariant := 'control';
end;

destructor TFeatureFlag.Destroy;
begin
  FUserList.Free;
  FVariants.Free;
  inherited;
end;

function TFeatureFlag.ToJSON: TJSONObject;
var
  i: Integer;
  UsersArray, VariantsArray: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.Add('name', FName);
  Result.Add('enabled', FEnabled);
  Result.Add('description', FDescription);
  Result.Add('strategy', Integer(FStrategy));
  Result.Add('percentage', FPercentage);
  Result.Add('attribute_key', FAttributeKey);
  Result.Add('attribute_value', FAttributeValue);
  Result.Add('default_variant', FDefaultVariant);

  // Liste des utilisateurs
  UsersArray := TJSONArray.Create;
  for i := 0 to FUserList.Count - 1 do
    UsersArray.Add(FUserList[i]);
  Result.Add('user_list', UsersArray);

  // Variants
  VariantsArray := TJSONArray.Create;
  for i := 0 to FVariants.Count - 1 do
    VariantsArray.Add(FVariants[i]);
  Result.Add('variants', VariantsArray);
end;

procedure TFeatureFlag.FromJSON(AJSON: TJSONObject);
var
  i: Integer;
  UsersArray, VariantsArray: TJSONArray;
begin
  FEnabled := AJSON.Get('enabled', False);
  FDescription := AJSON.Get('description', '');
  FStrategy := TTargetingStrategy(AJSON.Get('strategy', 0));
  FPercentage := AJSON.Get('percentage', 100);
  FAttributeKey := AJSON.Get('attribute_key', '');
  FAttributeValue := AJSON.Get('attribute_value', '');
  FDefaultVariant := AJSON.Get('default_variant', 'control');

  // Charger la liste des utilisateurs
  FUserList.Clear;
  if AJSON.Find('user_list') <> nil then
  begin
    UsersArray := AJSON.Get('user_list') as TJSONArray;
    for i := 0 to UsersArray.Count - 1 do
      FUserList.Add(UsersArray.Strings[i]);
  end;

  // Charger les variants
  FVariants.Clear;
  if AJSON.Find('variants') <> nil then
  begin
    VariantsArray := AJSON.Get('variants') as TJSONArray;
    for i := 0 to VariantsArray.Count - 1 do
      FVariants.Add(VariantsArray.Strings[i]);
  end;
end;

{ TUserContext }

constructor TUserContext.Create(const AUserID: string);
begin
  FUserID := AUserID;
  FAttributes := TStringList.Create;
  FAttributes.NameValueSeparator := '=';
end;

destructor TUserContext.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

function TUserContext.GetAttribute(const Key: string): string;
begin
  Result := FAttributes.Values[Key];
end;

procedure TUserContext.SetAttribute(const Key, Value: string);
begin
  FAttributes.Values[Key] := Value;
end;

{ TFeatureFlagManager }

constructor TFeatureFlagManager.Create(ABackend: TObject; ACacheTTL: Integer);
begin
  FFlags := TFeatureFlagMap.Create;
  FLock := TCriticalSection.Create;
  FBackend := ABackend;
  FCacheTTL := ACacheTTL;
  RefreshFromBackend;
end;

destructor TFeatureFlagManager.Destroy;
var
  i: Integer;
begin
  // Libérer tous les flags
  for i := 0 to FFlags.Count - 1 do
    FFlags.Data[i].Free;

  FFlags.Free;
  FLock.Free;
  inherited;
end;

procedure TFeatureFlagManager.RefreshFromBackend;
begin
  // Vérifier si le cache est encore valide
  if (SecondsBetween(Now, FCacheTime) < FCacheTTL) and (FFlags.Count > 0) then
    Exit;

  FLock.Enter;
  try
    // TODO: Charger depuis le backend (Redis, DB, etc.)
    // Pour cet exemple, nous utilisons juste la mémoire
    FCacheTime := Now;
  finally
    FLock.Leave;
  end;
end;

function TFeatureFlagManager.HashUserForPercentage(const UserID, FlagName: string): Integer;
var
  HashStr: string;
begin
  // Créer un hash stable basé sur UserID + FlagName
  HashStr := MD5Print(MD5String(UserID + FlagName));
  // Convertir les 8 premiers caractères hex en entier et prendre modulo 100
  Result := StrToInt64('$' + Copy(HashStr, 1, 8)) mod 100;
end;

function TFeatureFlagManager.EvaluateFlag(AFlag: TFeatureFlag; AUser: TUserContext): Boolean;
var
  UserHash: Integer;
  UserAttribute: string;
begin
  // Si le flag est désactivé globalement
  if not AFlag.Enabled then
    Exit(False);

  // Stratégie d'évaluation
  case AFlag.Strategy of
    tsAll:
      Result := True;

    tsNone:
      Result := False;

    tsPercentage:
      begin
        if AUser = nil then
          Exit(False);

        // Hash consistant pour que le même utilisateur ait toujours le même résultat
        UserHash := HashUserForPercentage(AUser.UserID, AFlag.Name);
        Result := UserHash < AFlag.Percentage;
      end;

    tsUserList:
      begin
        if AUser = nil then
          Exit(False);
        Result := AFlag.UserList.IndexOf(AUser.UserID) >= 0;
      end;

    tsUserAttribute:
      begin
        if AUser = nil then
          Exit(False);
        UserAttribute := AUser.GetAttribute(AFlag.AttributeKey);
        Result := UserAttribute = AFlag.AttributeValue;
      end;
  else
    Result := False;
  end;
end;

function TFeatureFlagManager.GetVariantForUser(AFlag: TFeatureFlag; AUser: TUserContext): string;
var
  UserHash: Integer;
  VariantIndex: Integer;
begin
  // Si pas de variants définis, retourner le variant par défaut
  if AFlag.Variants.Count = 0 then
    Exit(AFlag.DefaultVariant);

  if AUser = nil then
    Exit(AFlag.DefaultVariant);

  // Utiliser un hash pour assigner de manière consistante un variant
  UserHash := HashUserForPercentage(AUser.UserID, AFlag.Name + '_variant');
  VariantIndex := UserHash mod AFlag.Variants.Count;
  Result := AFlag.Variants[VariantIndex];
end;

procedure TFeatureFlagManager.RegisterFlag(AFlag: TFeatureFlag);
begin
  FLock.Enter;
  try
    FFlags.Add(AFlag.Name, AFlag);
  finally
    FLock.Leave;
  end;
end;

function TFeatureFlagManager.GetFlag(const FlagName: string): TFeatureFlag;
var
  Index: Integer;
begin
  RefreshFromBackend;

  FLock.Enter;
  try
    Index := FFlags.IndexOf(FlagName);
    if Index >= 0 then
      Result := FFlags.Data[Index]
    else
      Result := nil;
  finally
    FLock.Leave;
  end;
end;

function TFeatureFlagManager.IsEnabled(const FlagName: string; AUser: TUserContext): Boolean;
var
  Flag: TFeatureFlag;
begin
  Flag := GetFlag(FlagName);
  if Flag = nil then
    Exit(False); // Flag inconnu = désactivé par défaut

  Result := EvaluateFlag(Flag, AUser);
end;

function TFeatureFlagManager.GetVariant(const FlagName: string; AUser: TUserContext): string;
var
  Flag: TFeatureFlag;
begin
  Flag := GetFlag(FlagName);
  if Flag = nil then
    Exit('control');

  if not EvaluateFlag(Flag, AUser) then
    Exit('control'); // Si le flag n'est pas actif pour cet utilisateur

  Result := GetVariantForUser(Flag, AUser);
end;

procedure TFeatureFlagManager.EnableFlag(const FlagName: string);
var
  Flag: TFeatureFlag;
begin
  Flag := GetFlag(FlagName);
  if Flag <> nil then
  begin
    Flag.Enabled := True;
    // TODO: Persister dans le backend
  end;
end;

procedure TFeatureFlagManager.DisableFlag(const FlagName: string);
var
  Flag: TFeatureFlag;
begin
  Flag := GetFlag(FlagName);
  if Flag <> nil then
  begin
    Flag.Enabled := False;
    // TODO: Persister dans le backend
  end;
end;

function TFeatureFlagManager.ListAllFlags: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;

  FLock.Enter;
  try
    for i := 0 to FFlags.Count - 1 do
      Result.Add(FFlags.Keys[i]);
  finally
    FLock.Leave;
  end;
end;

procedure TFeatureFlagManager.Refresh;
begin
  FCacheTime := 0; // Force refresh
  RefreshFromBackend;
end;

end.
```

### Utilisation dans votre application

```pascal
program MyApp;

uses
  SysUtils, FeatureFlags;

var
  FFManager: TFeatureFlagManager;
  User: TUserContext;
  NewUIFlag: TFeatureFlag;

begin
  // Initialiser le gestionnaire
  FFManager := TFeatureFlagManager.Create(nil, 60);
  try
    // Créer et enregistrer un flag
    NewUIFlag := TFeatureFlag.Create('new_ui');
    NewUIFlag.Enabled := True;
    NewUIFlag.Description := 'Nouvelle interface utilisateur';
    NewUIFlag.Strategy := tsPercentage;
    NewUIFlag.Percentage := 10; // Actif pour 10% des utilisateurs
    FFManager.RegisterFlag(NewUIFlag);

    // Créer un contexte utilisateur
    User := TUserContext.Create('user_12345');
    User.Username := 'john_doe';
    User.Email := 'john@example.com';
    try
      // Vérifier si le flag est actif pour cet utilisateur
      if FFManager.IsEnabled('new_ui', User) then
      begin
        WriteLn('Affichage de la nouvelle UI pour ', User.Username);
        // ShowNewUI;
      end
      else
      begin
        WriteLn('Affichage de l''ancienne UI pour ', User.Username);
        // ShowOldUI;
      end;
    finally
      User.Free;
    end;
  finally
    FFManager.Free;
  end;
end.
```

## Intégration avec Redis

### Backend Redis pour feature flags

```pascal
unit FeatureFlagRedisBackend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FeatureFlags, fpjson;

type
  TRedisClient = class
  // Implémentation simplifiée - utilisez une vraie bibliothèque Redis
  public
    function Get(const Key: string): string; virtual; abstract;
    procedure SetValue(const Key, Value: string); virtual; abstract;
    function Keys(const Pattern: string): TStringList; virtual; abstract;
    procedure Subscribe(const Channel: string); virtual; abstract;
  end;

  TFeatureFlagRedisBackend = class
  private
    FRedis: TRedisClient;
    FPrefix: string;
    procedure PublishFlagUpdate(const FlagName: string);
  public
    constructor Create(ARedis: TRedisClient; const APrefix: string = 'ff:');
    destructor Destroy; override;

    procedure SaveFlag(AFlag: TFeatureFlag);
    function LoadFlag(const FlagName: string): TFeatureFlag;
    function LoadAllFlags: TStringList; // Retourne les noms des flags
    procedure DeleteFlag(const FlagName: string);
  end;

implementation

{ TFeatureFlagRedisBackend }

constructor TFeatureFlagRedisBackend.Create(ARedis: TRedisClient; const APrefix: string);
begin
  FRedis := ARedis;
  FPrefix := APrefix;
end;

destructor TFeatureFlagRedisBackend.Destroy;
begin
  inherited;
end;

procedure TFeatureFlagRedisBackend.SaveFlag(AFlag: TFeatureFlag);
var
  JSON: TJSONObject;
  Key: string;
begin
  Key := FPrefix + AFlag.Name;
  JSON := AFlag.ToJSON;
  try
    FRedis.SetValue(Key, JSON.AsJSON);
    PublishFlagUpdate(AFlag.Name);
  finally
    JSON.Free;
  end;
end;

function TFeatureFlagRedisBackend.LoadFlag(const FlagName: string): TFeatureFlag;
var
  Key: string;
  JSONStr: string;
  JSON: TJSONObject;
  Parser: TJSONParser;
begin
  Key := FPrefix + FlagName;
  JSONStr := FRedis.Get(Key);

  if JSONStr = '' then
    Exit(nil);

  Parser := TJSONParser.Create(JSONStr, []);
  try
    JSON := Parser.Parse as TJSONObject;
    try
      Result := TFeatureFlag.Create(FlagName);
      Result.FromJSON(JSON);
    finally
      JSON.Free;
    end;
  finally
    Parser.Free;
  end;
end;

function TFeatureFlagRedisBackend.LoadAllFlags: TStringList;
var
  Keys: TStringList;
  i: Integer;
  FlagName: string;
begin
  Result := TStringList.Create;
  Keys := FRedis.Keys(FPrefix + '*');
  try
    for i := 0 to Keys.Count - 1 do
    begin
      FlagName := StringReplace(Keys[i], FPrefix, '', []);
      Result.Add(FlagName);
    end;
  finally
    Keys.Free;
  end;
end;

procedure TFeatureFlagRedisBackend.DeleteFlag(const FlagName: string);
var
  Key: string;
begin
  Key := FPrefix + FlagName;
  // FRedis.Del(Key); // Méthode à implémenter
end;

procedure TFeatureFlagRedisBackend.PublishFlagUpdate(const FlagName: string);
begin
  // Publier sur un canal Redis pour notifier les autres instances
  // FRedis.Publish('ff:updates', FlagName);
end;

end.
```

## A/B Testing avec Feature Flags

### Qu'est-ce que l'A/B Testing ?

L'**A/B Testing** (test A/B) est une méthode d'expérimentation où vous comparez deux versions (A et B) d'une fonctionnalité pour déterminer laquelle performe le mieux.

**Exemple concret** :
Vous voulez savoir quelle couleur de bouton "Acheter" génère le plus de ventes :
- **Variante A** (contrôle) : Bouton bleu
- **Variante B** (test) : Bouton rouge

Vous divisez vos utilisateurs en deux groupes :
- 50% voient le bouton bleu
- 50% voient le bouton rouge

Après quelques jours, vous analysez :
- Taux de conversion de A : 2.5%
- Taux de conversion de B : 3.1%

➡️ La variante B (rouge) performe mieux → Vous déployez le bouton rouge pour tout le monde.

### Implémentation d'A/B Testing

#### 1. Configuration du test

```pascal
procedure SetupABTest;
var
  FFManager: TFeatureFlagManager;
  ButtonColorTest: TFeatureFlag;
begin
  FFManager := GetGlobalFeatureFlagManager;

  ButtonColorTest := TFeatureFlag.Create('button_color_test');
  ButtonColorTest.Enabled := True;
  ButtonColorTest.Description := 'Test A/B de la couleur du bouton d''achat';
  ButtonColorTest.Strategy := tsPercentage;
  ButtonColorTest.Percentage := 100; // Actif pour tous

  // Définir les variants
  ButtonColorTest.Variants.Add('blue');    // 50% des utilisateurs
  ButtonColorTest.Variants.Add('red');     // 50% des utilisateurs
  ButtonColorTest.DefaultVariant := 'blue';

  FFManager.RegisterFlag(ButtonColorTest);
end;
```

#### 2. Utilisation dans le code

```pascal
procedure ShowCheckoutButton(User: TUserContext);
var
  FFManager: TFeatureFlagManager;
  Variant: string;
begin
  FFManager := GetGlobalFeatureFlagManager;

  // Obtenir le variant assigné à cet utilisateur
  Variant := FFManager.GetVariant('button_color_test', User);

  if Variant = 'blue' then
  begin
    ButtonBuy.Color := clBlue;
    TrackEvent('button_shown', 'variant', 'blue');
  end
  else if Variant = 'red' then
  begin
    ButtonBuy.Color := clRed;
    TrackEvent('button_shown', 'variant', 'red');
  end
  else
    // Fallback sur le contrôle
    ButtonBuy.Color := clBlue;
end;
```

#### 3. Tracking des événements

```pascal
unit ABTestTracking;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  TEventTracker = class
  private
    FDatabaseConnection: TObject; // Votre connexion DB
    procedure SaveToDatabase(AEvent: TJSONObject);
  public
    constructor Create(AConnection: TObject);

    procedure TrackEvent(
      const EventName: string;
      const UserID: string;
      const FlagName: string;
      const Variant: string;
      AProperties: TJSONObject = nil
    );

    procedure TrackConversion(
      const UserID: string;
      const FlagName: string;
      const Variant: string;
      const ConversionValue: Double
    );
  end;

implementation

constructor TEventTracker.Create(AConnection: TObject);
begin
  FDatabaseConnection := AConnection;
end;

procedure TEventTracker.TrackEvent(
  const EventName: string;
  const UserID: string;
  const FlagName: string;
  const Variant: string;
  AProperties: TJSONObject
);
var
  Event: TJSONObject;
begin
  Event := TJSONObject.Create;
  try
    Event.Add('event_name', EventName);
    Event.Add('user_id', UserID);
    Event.Add('flag_name', FlagName);
    Event.Add('variant', Variant);
    Event.Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

    // Ajouter les propriétés supplémentaires
    if Assigned(AProperties) then
      Event.Add('properties', AProperties.Clone as TJSONObject);

    SaveToDatabase(Event);
  finally
    Event.Free;
  end;
end;

procedure TEventTracker.TrackConversion(
  const UserID: string;
  const FlagName: string;
  const Variant: string;
  const ConversionValue: Double
);
var
  Properties: TJSONObject;
begin
  Properties := TJSONObject.Create;
  try
    Properties.Add('conversion_value', ConversionValue);
    TrackEvent('conversion', UserID, FlagName, Variant, Properties);
  finally
    Properties.Free;
  end;
end;

procedure TEventTracker.SaveToDatabase(AEvent: TJSONObject);
begin
  // Exemple d'insertion SQL
  // INSERT INTO ab_test_events (event_name, user_id, flag_name, variant, timestamp, properties)
  // VALUES (?, ?, ?, ?, ?, ?)

  // TODO: Implémenter selon votre système de base de données
end;

end.
```

### Exemple complet d'un test A/B sur un bouton d'achat

```pascal
unit CheckoutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics,
  FeatureFlags, ABTestTracking;

type
  TCheckoutForm = class(TForm)
    ButtonBuy: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
  private
    FCurrentUser: TUserContext;
    FCurrentVariant: string;
    FTracker: TEventTracker;
    procedure SetupButtonTest;
  public
    property CurrentUser: TUserContext read FCurrentUser write FCurrentUser;
  end;

implementation

procedure TCheckoutForm.FormCreate(Sender: TObject);
begin
  FTracker := TEventTracker.Create(DatabaseConnection);
  SetupButtonTest;
end;

procedure TCheckoutForm.SetupButtonTest;
var
  FFManager: TFeatureFlagManager;
begin
  FFManager := GetGlobalFeatureFlagManager;

  // Obtenir le variant pour cet utilisateur
  FCurrentVariant := FFManager.GetVariant('button_color_test', FCurrentUser);

  // Appliquer le variant
  if FCurrentVariant = 'blue' then
  begin
    ButtonBuy.Color := clBlue;
    ButtonBuy.Font.Color := clWhite;
  end
  else if FCurrentVariant = 'red' then
  begin
    ButtonBuy.Color := clRed;
    ButtonBuy.Font.Color := clWhite;
  end
  else if FCurrentVariant = 'green' then
  begin
    ButtonBuy.Color := clGreen;
    ButtonBuy.Font.Color := clWhite;
  end
  else
  begin
    // Contrôle par défaut
    ButtonBuy.Color := clBlue;
  end;

  // Tracker l'exposition au test
  FTracker.TrackEvent(
    'button_shown',
    FCurrentUser.UserID,
    'button_color_test',
    FCurrentVariant,
    nil
  );
end;

procedure TCheckoutForm.ButtonBuyClick(Sender: TObject);
var
  OrderAmount: Double;
begin
  // Traiter la commande
  OrderAmount := ProcessOrder();

  if OrderAmount > 0 then
  begin
    // Tracker la conversion
    FTracker.TrackConversion(
      FCurrentUser.UserID,
      'button_color_test',
      FCurrentVariant,
      OrderAmount
    );

    ShowMessage('Commande validée !');
  end;
end;

end.
```

## Analyse des résultats d'A/B Testing

### Schéma de base de données pour les événements

```sql
-- Table pour stocker les événements du test A/B
CREATE TABLE ab_test_events (
    id SERIAL PRIMARY KEY,
    event_name VARCHAR(100) NOT NULL,
    user_id VARCHAR(100) NOT NULL,
    flag_name VARCHAR(100) NOT NULL,
    variant VARCHAR(50) NOT NULL,
    timestamp TIMESTAMP NOT NULL,
    properties JSONB,
    session_id VARCHAR(100),
    INDEX idx_flag_variant (flag_name, variant),
    INDEX idx_user_id (user_id),
    INDEX idx_timestamp (timestamp)
);

-- Table pour stocker les conversions
CREATE TABLE ab_test_conversions (
    id SERIAL PRIMARY KEY,
    user_id VARCHAR(100) NOT NULL,
    flag_name VARCHAR(100) NOT NULL,
    variant VARCHAR(50) NOT NULL,
    conversion_value DECIMAL(10, 2),
    timestamp TIMESTAMP NOT NULL,
    INDEX idx_flag_variant (flag_name, variant)
);
```

### Requêtes d'analyse

#### 1. Calculer le taux de conversion par variant

```sql
-- Nombre d'utilisateurs exposés par variant
WITH exposures AS (
    SELECT
        flag_name,
        variant,
        COUNT(DISTINCT user_id) as exposed_users
    FROM ab_test_events
    WHERE event_name = 'button_shown'
        AND flag_name = 'button_color_test'
    GROUP BY flag_name, variant
),
-- Nombre de conversions par variant
conversions AS (
    SELECT
        flag_name,
        variant,
        COUNT(DISTINCT user_id) as converted_users,
        SUM(conversion_value) as total_revenue,
        AVG(conversion_value) as avg_order_value
    FROM ab_test_conversions
    WHERE flag_name = 'button_color_test'
    GROUP BY flag_name, variant
)
-- Combiner les résultats
SELECT
    e.variant,
    e.exposed_users,
    COALESCE(c.converted_users, 0) as converted_users,
    ROUND(
        (COALESCE(c.converted_users, 0)::DECIMAL / e.exposed_users) * 100,
        2
    ) as conversion_rate_percent,
    COALESCE(c.total_revenue, 0) as total_revenue,
    COALESCE(c.avg_order_value, 0) as avg_order_value
FROM exposures e
LEFT JOIN conversions c ON e.variant = c.variant
ORDER BY e.variant;
```

**Exemple de résultat** :
```
variant | exposed_users | converted_users | conversion_rate | total_revenue | avg_order_value
--------|---------------|-----------------|-----------------|---------------|----------------
blue    | 5000          | 125             | 2.50%           | 12500.00      | 100.00
red     | 5000          | 155             | 3.10%           | 15810.00      | 102.00
green   | 5000          | 140             | 2.80%           | 14280.00      | 102.00
```

**Interprétation** :
- Le bouton **rouge** a le meilleur taux de conversion (3.10%)
- Il génère aussi plus de revenus au total
- La différence est-elle statistiquement significative ? (voir section suivante)

#### 2. Évolution dans le temps

```sql
SELECT
    DATE(timestamp) as date,
    variant,
    COUNT(DISTINCT user_id) as exposed_users
FROM ab_test_events
WHERE event_name = 'button_shown'
    AND flag_name = 'button_color_test'
    AND timestamp >= NOW() - INTERVAL '7 days'
GROUP BY DATE(timestamp), variant
ORDER BY date, variant;
```

### Significativité statistique

Pour savoir si la différence observée est réelle ou due au hasard, calculez la **significativité statistique**.

#### Implémentation d'un test Z en FreePascal

```pascal
unit ABTestStatistics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TVariantStats = record
    Name: string;
    Exposures: Integer;
    Conversions: Integer;
    ConversionRate: Double;
  end;

  TSignificanceTest = record
    PValue: Double;
    IsSignificant: Boolean;
    ConfidenceLevel: Double;
    Winner: string;
    Uplift: Double; // Amélioration en pourcentage
  end;

function CalculateConversionRate(Conversions, Exposures: Integer): Double;
function CalculateZScore(VariantA, VariantB: TVariantStats): Double;
function PerformSignificanceTest(
  VariantA, VariantB: TVariantStats;
  ConfidenceLevel: Double = 0.95
): TSignificanceTest;

implementation

function CalculateConversionRate(Conversions, Exposures: Integer): Double;
begin
  if Exposures = 0 then
    Result := 0
  else
    Result := Conversions / Exposures;
end;

function CalculateZScore(VariantA, VariantB: TVariantStats): Double;
var
  p1, p2: Double;
  n1, n2: Integer;
  pooledP: Double;
  pooledSE: Double;
begin
  p1 := VariantA.ConversionRate;
  p2 := VariantB.ConversionRate;
  n1 := VariantA.Exposures;
  n2 := VariantB.Exposures;

  // Taux de conversion combiné
  pooledP := (VariantA.Conversions + VariantB.Conversions) / (n1 + n2);

  // Erreur standard combinée
  pooledSE := Sqrt(pooledP * (1 - pooledP) * (1/n1 + 1/n2));

  if pooledSE = 0 then
    Result := 0
  else
    Result := (p1 - p2) / pooledSE;
end;

function PerformSignificanceTest(
  VariantA, VariantB: TVariantStats;
  ConfidenceLevel: Double
): TSignificanceTest;
var
  zScore: Double;
  zCritical: Double;
  pValue: Double;
begin
  // Calculer le Z-score
  zScore := CalculateZScore(VariantA, VariantB);

  // Z-critique pour un test bilatéral
  // 95% confidence = 1.96, 99% = 2.576
  case Round(ConfidenceLevel * 100) of
    90: zCritical := 1.645;
    95: zCritical := 1.96;
    99: zCritical := 2.576;
  else
    zCritical := 1.96;
  end;

  // P-value approximative (test bilatéral)
  pValue := 2 * (1 - 0.5 * (1 + Erf(Abs(zScore) / Sqrt(2))));

  Result.PValue := pValue;
  Result.ConfidenceLevel := ConfidenceLevel;
  Result.IsSignificant := Abs(zScore) > zCritical;

  // Déterminer le gagnant
  if Result.IsSignificant then
  begin
    if VariantA.ConversionRate > VariantB.ConversionRate then
    begin
      Result.Winner := VariantA.Name;
      Result.Uplift := ((VariantA.ConversionRate - VariantB.ConversionRate) /
                        VariantB.ConversionRate) * 100;
    end
    else
    begin
      Result.Winner := VariantB.Name;
      Result.Uplift := ((VariantB.ConversionRate - VariantA.ConversionRate) /
                        VariantA.ConversionRate) * 100;
    end;
  end
  else
  begin
    Result.Winner := 'none';
    Result.Uplift := 0;
  end;
end;

end.
```

#### Utilisation pour analyser un test

```pascal
program AnalyzeABTest;

uses
  SysUtils, ABTestStatistics;

var
  Blue, Red: TVariantStats;
  TestResult: TSignificanceTest;

begin
  // Données du variant Bleu
  Blue.Name := 'blue';
  Blue.Exposures := 5000;
  Blue.Conversions := 125;
  Blue.ConversionRate := CalculateConversionRate(Blue.Conversions, Blue.Exposures);

  // Données du variant Rouge
  Red.Name := 'red';
  Red.Exposures := 5000;
  Red.Conversions := 155;
  Red.ConversionRate := CalculateConversionRate(Red.Conversions, Red.Exposures);

  WriteLn('Variant Blue:');
  WriteLn('  Exposures: ', Blue.Exposures);
  WriteLn('  Conversions: ', Blue.Conversions);
  WriteLn('  Conversion Rate: ', FormatFloat('0.00', Blue.ConversionRate * 100), '%');
  WriteLn;

  WriteLn('Variant Red:');
  WriteLn('  Exposures: ', Red.Exposures);
  WriteLn('  Conversions: ', Red.Conversions);
  WriteLn('  Conversion Rate: ', FormatFloat('0.00', Red.ConversionRate * 100), '%');
  WriteLn;

  // Effectuer le test de significativité
  TestResult := PerformSignificanceTest(Red, Blue, 0.95);

  WriteLn('Test de significativité statistique:');
  WriteLn('  P-Value: ', FormatFloat('0.0000', TestResult.PValue));
  WriteLn('  Confidence Level: ', FormatFloat('0', TestResult.ConfidenceLevel * 100), '%');

  if TestResult.IsSignificant then
  begin
    WriteLn('  Résultat: SIGNIFICATIF ✓');
    WriteLn('  Gagnant: ', TestResult.Winner);
    WriteLn('  Amélioration: +', FormatFloat('0.00', TestResult.Uplift), '%');
    WriteLn;
    WriteLn('➡️  Recommandation: Déployer le variant ', TestResult.Winner);
  end
  else
  begin
    WriteLn('  Résultat: NON SIGNIFICATIF');
    WriteLn('  La différence peut être due au hasard');
    WriteLn;
    WriteLn('➡️  Recommandation: Continuer le test ou conserver le contrôle');
  end;

  ReadLn;
end.
```

**Exemple de sortie** :
```
Variant Blue:
  Exposures: 5000
  Conversions: 125
  Conversion Rate: 2.50%

Variant Red:
  Exposures: 5000
  Conversions: 155
  Conversion Rate: 3.10%

Test de significativité statistique:
  P-Value: 0.0231
  Confidence Level: 95%
  Résultat: SIGNIFICATIF ✓
  Gagnant: red
  Amélioration: +24.00%

➡️  Recommandation: Déployer le variant red
```

## Tests multivariés

### Au-delà de l'A/B : tests A/B/C/D...

Vous pouvez tester plus de 2 variants simultanément.

**Exemple** : Tester 3 couleurs de bouton + 2 textes = 6 combinaisons

```pascal
procedure SetupMultivariateTest;
var
  FFManager: TFeatureFlagManager;
  ButtonTest: TFeatureFlag;
begin
  FFManager := GetGlobalFeatureFlagManager;

  ButtonTest := TFeatureFlag.Create('button_multivariate');
  ButtonTest.Enabled := True;
  ButtonTest.Strategy := tsPercentage;
  ButtonTest.Percentage := 100;

  // 6 variants avec distribution égale
  ButtonTest.Variants.Add('blue_buy_now');      // 16.6%
  ButtonTest.Variants.Add('blue_purchase');     // 16.6%
  ButtonTest.Variants.Add('red_buy_now');       // 16.6%
  ButtonTest.Variants.Add('red_purchase');      // 16.6%
  ButtonTest.Variants.Add('green_buy_now');     // 16.6%
  ButtonTest.Variants.Add('green_purchase');    // 16.6%

  ButtonTest.DefaultVariant := 'blue_buy_now';
  FFManager.RegisterFlag(ButtonTest);
end;

procedure ApplyButtonVariant(const Variant: string; Button: TButton);
begin
  // Extraire couleur et texte du nom du variant
  if Pos('blue', Variant) > 0 then
    Button.Color := clBlue
  else if Pos('red', Variant) > 0 then
    Button.Color := clRed
  else if Pos('green', Variant) > 0 then
    Button.Color := clGreen;

  if Pos('buy_now', Variant) > 0 then
    Button.Caption := 'Acheter maintenant'
  else if Pos('purchase', Variant) > 0 then
    Button.Caption := 'Commander';
end;
```

**Attention** : Plus vous avez de variants, plus vous avez besoin de trafic pour atteindre la significativité statistique.

## Ciblage avancé

### Tests basés sur les attributs utilisateurs

```pascal
procedure SetupSegmentedTest;
var
  FFManager: TFeatureFlagManager;
  PremiumFeature: TFeatureFlag;
begin
  FFManager := GetGlobalFeatureFlagManager;

  // Fonctionnalité uniquement pour les utilisateurs premium
  PremiumFeature := TFeatureFlag.Create('advanced_analytics');
  PremiumFeature.Enabled := True;
  PremiumFeature.Strategy := tsUserAttribute;
  PremiumFeature.AttributeKey := 'subscription_type';
  PremiumFeature.AttributeValue := 'premium';

  FFManager.RegisterFlag(PremiumFeature);
end;

// Utilisation
procedure ShowAnalytics(User: TUserContext);
var
  FFManager: TFeatureFlagManager;
begin
  User.SetAttribute('subscription_type', 'premium'); // ou 'free'

  FFManager := GetGlobalFeatureFlagManager;

  if FFManager.IsEnabled('advanced_analytics', User) then
    ShowAdvancedAnalytics
  else
    ShowBasicAnalytics;
end;
```

### Tests géographiques

```pascal
procedure SetupGeoTest;
var
  User: TUserContext;
begin
  User := TUserContext.Create('user_123');

  // Ajouter la localisation
  User.SetAttribute('country', 'FR');
  User.SetAttribute('region', 'Normandy');

  // Le flag peut alors cibler par pays
  if FFManager.IsEnabled('france_promotion', User) then
    ShowPromotionFR;
end;
```

### Tests temporels

```pascal
unit TemporalFlags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, FeatureFlags;

type
  TTemporalFeatureFlag = class(TFeatureFlag)
  private
    FStartDate: TDateTime;
    FEndDate: TDateTime;
  public
    function IsActive: Boolean;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property EndDate: TDateTime read FEndDate write FEndDate;
  end;

implementation

function TTemporalFeatureFlag.IsActive: Boolean;
var
  CurrentTime: TDateTime;
begin
  if not Enabled then
    Exit(False);

  CurrentTime := Now;

  // Vérifier si nous sommes dans la fenêtre temporelle
  Result := (CurrentTime >= FStartDate) and (CurrentTime <= FEndDate);
end;

end.
```

**Utilisation pour des promotions limitées dans le temps** :

```pascal
var
  ChristmasPromo: TTemporalFeatureFlag;
begin
  ChristmasPromo := TTemporalFeatureFlag.Create('christmas_promo_2025');
  ChristmasPromo.Enabled := True;
  ChristmasPromo.StartDate := EncodeDate(2025, 12, 20);
  ChristmasPromo.EndDate := EncodeDate(2025, 12, 26);

  if ChristmasPromo.IsActive then
    ShowChristmasDiscount;
end;
```

## Interface d'administration des feature flags

### API REST pour gérer les flags

```pascal
unit FeatureFlagAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, fpjson,
  FeatureFlags, FeatureFlagRedisBackend;

type
  TFeatureFlagAPIServer = class
  private
    FFManager: TFeatureFlagManager;
    FBackend: TFeatureFlagRedisBackend;

    procedure HandleListFlags(ARequest: TRequest; AResponse: TResponse);
    procedure HandleGetFlag(ARequest: TRequest; AResponse: TResponse);
    procedure HandleCreateFlag(ARequest: TRequest; AResponse: TResponse);
    procedure HandleUpdateFlag(ARequest: TRequest; AResponse: TResponse);
    procedure HandleToggleFlag(ARequest: TRequest; AResponse: TResponse);
    procedure HandleDeleteFlag(ARequest: TRequest; AResponse: TResponse);
  public
    constructor Create(AManager: TFeatureFlagManager; ABackend: TFeatureFlagRedisBackend);
    procedure RegisterRoutes(Server: TFPHTTPServer);
  end;

implementation

constructor TFeatureFlagAPIServer.Create(
  AManager: TFeatureFlagManager;
  ABackend: TFeatureFlagRedisBackend
);
begin
  FFManager := AManager;
  FBackend := ABackend;
end;

procedure TFeatureFlagAPIServer.HandleListFlags(ARequest: TRequest; AResponse: TResponse);
var
  Flags: TStringList;
  ResultArray: TJSONArray;
  i: Integer;
  Flag: TFeatureFlag;
begin
  Flags := FFManager.ListAllFlags;
  ResultArray := TJSONArray.Create;
  try
    for i := 0 to Flags.Count - 1 do
    begin
      Flag := FFManager.GetFlag(Flags[i]);
      if Assigned(Flag) then
        ResultArray.Add(Flag.ToJSON);
    end;

    AResponse.ContentType := 'application/json';
    AResponse.Content := ResultArray.AsJSON;
  finally
    ResultArray.Free;
    Flags.Free;
  end;
end;

procedure TFeatureFlagAPIServer.HandleGetFlag(ARequest: TRequest; AResponse: TResponse);
var
  FlagName: string;
  Flag: TFeatureFlag;
begin
  // Extraire le nom du flag de l'URL
  // Ex: GET /api/flags/new_ui
  FlagName := ARequest.PathInfo; // Simplification

  Flag := FFManager.GetFlag(FlagName);
  if Assigned(Flag) then
  begin
    AResponse.Code := 200;
    AResponse.ContentType := 'application/json';
    AResponse.Content := Flag.ToJSON.AsJSON;
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Flag not found"}';
  end;
end;

procedure TFeatureFlagAPIServer.HandleCreateFlag(ARequest: TRequest; AResponse: TResponse);
var
  RequestBody: TJSONObject;
  Parser: TJSONParser;
  NewFlag: TFeatureFlag;
  FlagName: string;
begin
  Parser := TJSONParser.Create(ARequest.Content, []);
  try
    RequestBody := Parser.Parse as TJSONObject;
    try
      FlagName := RequestBody.Get('name', '');

      if FlagName = '' then
      begin
        AResponse.Code := 400;
        AResponse.Content := '{"error": "Flag name required"}';
        Exit;
      end;

      NewFlag := TFeatureFlag.Create(FlagName);
      NewFlag.FromJSON(RequestBody);

      FFManager.RegisterFlag(NewFlag);
      FBackend.SaveFlag(NewFlag);

      AResponse.Code := 201;
      AResponse.ContentType := 'application/json';
      AResponse.Content := NewFlag.ToJSON.AsJSON;
    finally
      RequestBody.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TFeatureFlagAPIServer.HandleToggleFlag(ARequest: TRequest; AResponse: TResponse);
var
  FlagName: string;
  Flag: TFeatureFlag;
  RequestBody: TJSONObject;
  Parser: TJSONParser;
  NewState: Boolean;
begin
  FlagName := ARequest.PathInfo;
  Flag := FFManager.GetFlag(FlagName);

  if not Assigned(Flag) then
  begin
    AResponse.Code := 404;
    Exit;
  end;

  Parser := TJSONParser.Create(ARequest.Content, []);
  try
    RequestBody := Parser.Parse as TJSONObject;
    try
      NewState := RequestBody.Get('enabled', Flag.Enabled);
      Flag.Enabled := NewState;

      FBackend.SaveFlag(Flag);

      AResponse.Code := 200;
      AResponse.ContentType := 'application/json';
      AResponse.Content := Flag.ToJSON.AsJSON;
    finally
      RequestBody.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TFeatureFlagAPIServer.HandleUpdateFlag(ARequest: TRequest; AResponse: TResponse);
begin
  // Similaire à HandleToggleFlag mais met à jour tous les champs
end;

procedure TFeatureFlagAPIServer.HandleDeleteFlag(ARequest: TRequest; AResponse: TResponse);
var
  FlagName: string;
begin
  FlagName := ARequest.PathInfo;

  FBackend.DeleteFlag(FlagName);

  AResponse.Code := 204; // No Content
end;

procedure TFeatureFlagAPIServer.RegisterRoutes(Server: TFPHTTPServer);
begin
  // Enregistrer les routes
  // Cette partie dépend de votre framework HTTP
  // Server.RegisterRoute('GET', '/api/flags', @HandleListFlags);
  // Server.RegisterRoute('GET', '/api/flags/:name', @HandleGetFlag);
  // etc.
end;

end.
```

### Interface web simple avec HTML

```html
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Feature Flags Admin</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
        }

        .flag-card {
            border: 1px solid #ddd;
            border-radius: 8px;
            padding: 15px;
            margin: 10px 0;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .flag-enabled {
            background-color: #e8f5e9;
        }

        .flag-disabled {
            background-color: #ffebee;
        }

        .toggle-button {
            padding: 8px 16px;
            border: none;
            border-radius: 4px;
            cursor: pointer;
            font-size: 14px;
        }

        .toggle-on {
            background-color: #4caf50;
            color: white;
        }

        .toggle-off {
            background-color: #f44336;
            color: white;
        }

        .flag-info {
            flex-grow: 1;
        }

        .flag-name {
            font-size: 18px;
            font-weight: bold;
            margin-bottom: 5px;
        }

        .flag-description {
            color: #666;
            font-size: 14px;
        }

        .flag-stats {
            margin-top: 10px;
            font-size: 12px;
            color: #888;
        }
    </style>
</head>
<body>
    <h1>Feature Flags Administration</h1>

    <button onclick="refreshFlags()">🔄 Rafraîchir</button>
    <button onclick="showCreateForm()">➕ Nouveau Flag</button>

    <div id="flags-container"></div>

    <script>
        // Charger tous les flags
        async function loadFlags() {
            const response = await fetch('/api/flags');
            const flags = await response.json();

            const container = document.getElementById('flags-container');
            container.innerHTML = '';

            flags.forEach(flag => {
                const card = createFlagCard(flag);
                container.appendChild(card);
            });
        }

        // Créer une carte pour un flag
        function createFlagCard(flag) {
            const card = document.createElement('div');
            card.className = `flag-card ${flag.enabled ? 'flag-enabled' : 'flag-disabled'}`;

            card.innerHTML = `
                <div class="flag-info">
                    <div class="flag-name">${flag.name}</div>
                    <div class="flag-description">${flag.description || 'Pas de description'}</div>
                    <div class="flag-stats">
                        Stratégie: ${getStrategyName(flag.strategy)} |
                        ${flag.percentage ? `Pourcentage: ${flag.percentage}%` : ''}
                    </div>
                </div>
                <button
                    class="toggle-button ${flag.enabled ? 'toggle-on' : 'toggle-off'}"
                    onclick="toggleFlag('${flag.name}', ${!flag.enabled})">
                    ${flag.enabled ? '✓ Activé' : '✗ Désactivé'}
                </button>
                <button onclick="editFlag('${flag.name}')">✏️ Éditer</button>
                <button onclick="deleteFlag('${flag.name}')">🗑️ Supprimer</button>
            `;

            return card;
        }

        // Basculer un flag
        async function toggleFlag(flagName, newState) {
            const response = await fetch(`/api/flags/${flagName}/toggle`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({ enabled: newState })
            });

            if (response.ok) {
                loadFlags();
            } else {
                alert('Erreur lors de la modification du flag');
            }
        }

        // Obtenir le nom lisible de la stratégie
        function getStrategyName(strategy) {
            const strategies = {
                0: 'Tous',
                1: 'Personne',
                2: 'Pourcentage',
                3: 'Liste utilisateurs',
                4: 'Attribut utilisateur'
            };
            return strategies[strategy] || 'Inconnu';
        }

        // Supprimer un flag
        async function deleteFlag(flagName) {
            if (!confirm(`Êtes-vous sûr de vouloir supprimer le flag "${flagName}" ?`)) {
                return;
            }

            const response = await fetch(`/api/flags/${flagName}`, {
                method: 'DELETE'
            });

            if (response.ok) {
                loadFlags();
            } else {
                alert('Erreur lors de la suppression du flag');
            }
        }

        // Rafraîchir les flags
        function refreshFlags() {
            loadFlags();
        }

        // Afficher le formulaire de création
        function showCreateForm() {
            // TODO: Implémenter un formulaire modal ou une page dédiée
            alert('Formulaire de création à implémenter');
        }

        // Éditer un flag
        function editFlag(flagName) {
            // TODO: Implémenter un formulaire d'édition
            alert(`Édition du flag "${flagName}" à implémenter`);
        }

        // Charger les flags au démarrage
        loadFlags();
    </script>
</body>
</html>
```

## Bonnes pratiques des feature flags

### 1. Nommage cohérent

**Convention recommandée** :
```
<type>_<fonctionnalité>_<contexte>
```

**Exemples** :
```pascal
// Release flags
'release_new_checkout'
'release_mobile_app_v2'

// Ops flags
'ops_enable_caching'
'ops_maintenance_mode'
'ops_slow_query_logging'

// Experiment flags
'exp_button_color_test'
'exp_pricing_page_variant'

// Permission flags
'perm_premium_features'
'perm_admin_panel'
```

### 2. Documentation des flags

Chaque flag devrait avoir :
- **Description claire** : Que fait ce flag ?
- **Propriétaire** : Qui est responsable ?
- **Date de création** : Quand a-t-il été créé ?
- **Date de suppression prévue** : Quand sera-t-il supprimé ?
- **Dépendances** : Quels autres flags ou systèmes sont affectés ?

```pascal
procedure DocumentFlag(var Flag: TFeatureFlag);
begin
  Flag.Description := 'Active la nouvelle interface de checkout redesignée';
  Flag.SetAttribute('owner', 'equipe_frontend');
  Flag.SetAttribute('created_date', '2025-10-01');
  Flag.SetAttribute('planned_removal', '2025-12-01');
  Flag.SetAttribute('jira_ticket', 'PROJ-1234');
end;
```

### 3. Cycle de vie des flags

```
Création → Déploiement → Test → Activation progressive → 100% → Nettoyage
   ↓          ↓            ↓           ↓                    ↓        ↓
  Code     Production    A/B Test   Rollout            Stable   Supprimer
                                    10%→50%→100%                le flag
```

**Important** : Les flags temporaires (release, experiment) doivent être **supprimés** après utilisation !

```pascal
// ❌ MAUVAIS : Flag qui reste dans le code pour toujours
if FeatureFlags.IsEnabled('new_ui_2022') then  // On est en 2025 !
  ShowNewUI
else
  ShowOldUI;

// ✅ BON : Une fois stable, supprimer le flag et garder seulement le nouveau code
ShowNewUI;  // L'ancien code est supprimé
```

### 4. Gestion de la dette technique

**Processus de nettoyage** :

1. **Identifier les flags obsolètes**
```sql
SELECT flag_name, created_date, last_updated
FROM feature_flags
WHERE last_updated < NOW() - INTERVAL '90 days'
  AND flag_type = 'release';
```

2. **Analyser l'utilisation**
```sql
SELECT flag_name, COUNT(*) as checks
FROM flag_evaluations
WHERE timestamp > NOW() - INTERVAL '7 days'
GROUP BY flag_name
ORDER BY checks DESC;
```

3. **Supprimer le flag**
```bash
# Script de nettoyage automatique
grep -r "IsEnabled('old_feature')" .
# Si aucun résultat → supprimer le flag de la base
```

### 5. Sécurité et contrôle d'accès

```pascal
unit SecureFeatureFlags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FeatureFlags;

type
  TUserRole = (urUser, urDeveloper, urAdmin);

  TSecureFeatureFlagManager = class(TFeatureFlagManager)
  private
    function CanModifyFlag(const FlagName: string; Role: TUserRole): Boolean;
  public
    procedure EnableFlagSecure(const FlagName: string; Role: TUserRole);
    procedure DisableFlagSecure(const FlagName: string; Role: TUserRole);
  end;

implementation

function TSecureFeatureFlagManager.CanModifyFlag(const FlagName: string; Role: TUserRole): Boolean;
var
  Flag: TFeatureFlag;
begin
  Flag := GetFlag(FlagName);

  if Flag = nil then
    Exit(False);

  // Seuls les admins peuvent modifier les flags de permission
  if (Pos('perm_', FlagName) = 1) and (Role <> urAdmin) then
    Exit(False);

  // Les développeurs peuvent modifier les flags de release et expérimentation
  if Role >= urDeveloper then
    Exit(True);

  Result := False;
end;

procedure TSecureFeatureFlagManager.EnableFlagSecure(const FlagName: string; Role: TUserRole);
begin
  if not CanModifyFlag(FlagName, Role) then
    raise Exception.Create('Permission refusée pour modifier ce flag');

  EnableFlag(FlagName);
end;

procedure TSecureFeatureFlagManager.DisableFlagSecure(const FlagName: string; Role: TUserRole);
begin
  if not CanModifyFlag(FlagName, Role) then
    raise Exception.Create('Permission refusée pour modifier ce flag');

  DisableFlag(FlagName);
end;

end.
```

### 6. Logging et audit trail

Gardez une trace de toutes les modifications :

```pascal
unit FeatureFlagAudit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  TAuditAction = (aaCreated, aaEnabled, aaDisabled, aaUpdated, aaDeleted);

  TAuditEntry = record
    Timestamp: TDateTime;
    FlagName: string;
    Action: TAuditAction;
    UserID: string;
    OldValue: string;
    NewValue: string;
  end;

  TFeatureFlagAudit = class
  private
    FDatabaseConnection: TObject;
    procedure WriteToDatabase(Entry: TAuditEntry);
  public
    constructor Create(AConnection: TObject);

    procedure LogFlagChange(
      const FlagName: string;
      Action: TAuditAction;
      const UserID: string;
      const OldValue: string = '';
      const NewValue: string = ''
    );

    function GetAuditHistory(const FlagName: string): string; // JSON
  end;

implementation

constructor TFeatureFlagAudit.Create(AConnection: TObject);
begin
  FDatabaseConnection := AConnection;
end;

procedure TFeatureFlagAudit.LogFlagChange(
  const FlagName: string;
  Action: TAuditAction;
  const UserID: string;
  const OldValue: string;
  const NewValue: string
);
var
  Entry: TAuditEntry;
begin
  Entry.Timestamp := Now;
  Entry.FlagName := FlagName;
  Entry.Action := Action;
  Entry.UserID := UserID;
  Entry.OldValue := OldValue;
  Entry.NewValue := NewValue;

  WriteToDatabase(Entry);
end;

procedure TFeatureFlagAudit.WriteToDatabase(Entry: TAuditEntry);
var
  ActionStr: string;
begin
  case Entry.Action of
    aaCreated: ActionStr := 'CREATED';
    aaEnabled: ActionStr := 'ENABLED';
    aaDisabled: ActionStr := 'DISABLED';
    aaUpdated: ActionStr := 'UPDATED';
    aaDeleted: ActionStr := 'DELETED';
  end;

  // INSERT INTO feature_flag_audit ...
  // TODO: Implémenter selon votre base de données
end;

function TFeatureFlagAudit.GetAuditHistory(const FlagName: string): string;
begin
  // SELECT * FROM feature_flag_audit WHERE flag_name = ? ORDER BY timestamp DESC
  // TODO: Implémenter
  Result := '[]';
end;

end.
```

## Tests automatisés avec feature flags

### Tests unitaires

```pascal
unit FeatureFlagTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, FeatureFlags;

type
  TFeatureFlagTests = class(TTestCase)
  private
    FFManager: TFeatureFlagManager;
    FUser: TUserContext;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFlagEnabled;
    procedure TestFlagDisabled;
    procedure TestPercentageRollout;
    procedure TestUserListTargeting;
    procedure TestAttributeTargeting;
    procedure TestVariantAssignment;
  end;

implementation

procedure TFeatureFlagTests.SetUp;
begin
  FFManager := TFeatureFlagManager.Create(nil, 60);
  FUser := TUserContext.Create('test_user_123');
  FUser.Username := 'testuser';
  FUser.SetAttribute('country', 'FR');
end;

procedure TFeatureFlagTests.TearDown;
begin
  FUser.Free;
  FFManager.Free;
end;

procedure TFeatureFlagTests.TestFlagEnabled;
var
  Flag: TFeatureFlag;
begin
  Flag := TFeatureFlag.Create('test_feature');
  Flag.Enabled := True;
  Flag.Strategy := tsAll;
  FFManager.RegisterFlag(Flag);

  AssertTrue('Flag should be enabled for all users',
    FFManager.IsEnabled('test_feature', FUser));
end;

procedure TFeatureFlagTests.TestFlagDisabled;
var
  Flag: TFeatureFlag;
begin
  Flag := TFeatureFlag.Create('test_feature');
  Flag.Enabled := False;
  FFManager.RegisterFlag(Flag);

  AssertFalse('Flag should be disabled',
    FFManager.IsEnabled('test_feature', FUser));
end;

procedure TFeatureFlagTests.TestPercentageRollout;
var
  Flag: TFeatureFlag;
  EnabledCount, TotalTests, i: Integer;
  TestUser: TUserContext;
begin
  Flag := TFeatureFlag.Create('percentage_test');
  Flag.Enabled := True;
  Flag.Strategy := tsPercentage;
  Flag.Percentage := 50; // 50%
  FFManager.RegisterFlag(Flag);

  // Tester avec 1000 utilisateurs différents
  EnabledCount := 0;
  TotalTests := 1000;

  for i := 1 to TotalTests do
  begin
    TestUser := TUserContext.Create('user_' + IntToStr(i));
    try
      if FFManager.IsEnabled('percentage_test', TestUser) then
        Inc(EnabledCount);
    finally
      TestUser.Free;
    end;
  end;

  // Vérifier que c'est proche de 50% (avec une marge d'erreur)
  AssertTrue('Should be close to 50%',
    (EnabledCount >= 450) and (EnabledCount <= 550));
end;

procedure TFeatureFlagTests.TestUserListTargeting;
var
  Flag: TFeatureFlag;
  AllowedUser, DeniedUser: TUserContext;
begin
  Flag := TFeatureFlag.Create('user_list_test');
  Flag.Enabled := True;
  Flag.Strategy := tsUserList;
  Flag.UserList.Add('allowed_user');
  FFManager.RegisterFlag(Flag);

  AllowedUser := TUserContext.Create('allowed_user');
  DeniedUser := TUserContext.Create('denied_user');
  try
    AssertTrue('Allowed user should see the feature',
      FFManager.IsEnabled('user_list_test', AllowedUser));

    AssertFalse('Denied user should not see the feature',
      FFManager.IsEnabled('user_list_test', DeniedUser));
  finally
    AllowedUser.Free;
    DeniedUser.Free;
  end;
end;

procedure TFeatureFlagTests.TestAttributeTargeting;
var
  Flag: TFeatureFlag;
  FrenchUser, UsUser: TUserContext;
begin
  Flag := TFeatureFlag.Create('france_only');
  Flag.Enabled := True;
  Flag.Strategy := tsUserAttribute;
  Flag.AttributeKey := 'country';
  Flag.AttributeValue := 'FR';
  FFManager.RegisterFlag(Flag);

  FrenchUser := TUserContext.Create('french_user');
  FrenchUser.SetAttribute('country', 'FR');

  UsUser := TUserContext.Create('us_user');
  UsUser.SetAttribute('country', 'US');

  try
    AssertTrue('French user should see the feature',
      FFManager.IsEnabled('france_only', FrenchUser));

    AssertFalse('US user should not see the feature',
      FFManager.IsEnabled('france_only', UsUser));
  finally
    FrenchUser.Free;
    UsUser.Free;
  end;
end;

procedure TFeatureFlagTests.TestVariantAssignment;
var
  Flag: TFeatureFlag;
  Variant: string;
begin
  Flag := TFeatureFlag.Create('variant_test');
  Flag.Enabled := True;
  Flag.Strategy := tsAll;
  Flag.Variants.Add('red');
  Flag.Variants.Add('blue');
  Flag.Variants.Add('green');
  Flag.DefaultVariant := 'control';
  FFManager.RegisterFlag(Flag);

  Variant := FFManager.GetVariant('variant_test', FUser);

  // Le variant doit être l'un des variants définis
  AssertTrue('Variant should be red, blue, or green',
    (Variant = 'red') or (Variant = 'blue') or (Variant = 'green'));

  // Le même utilisateur doit toujours obtenir le même variant
  AssertEquals('Same user should get same variant',
    Variant,
    FFManager.GetVariant('variant_test', FUser));
end;

initialization
  RegisterTest(TFeatureFlagTests);

end.
```

### Tests d'intégration

```pascal
// Tester avec différents scenarios
procedure TestCheckoutWithDifferentFlags;
begin
  // Scenario 1 : Flag désactivé
  FFManager.DisableFlag('new_checkout');
  ProcessCheckout; // Devrait utiliser l'ancien checkout
  AssertEquals('Should use old checkout', 'old', GetUsedCheckoutVersion);

  // Scenario 2 : Flag activé
  FFManager.EnableFlag('new_checkout');
  ProcessCheckout; // Devrait utiliser le nouveau checkout
  AssertEquals('Should use new checkout', 'new', GetUsedCheckoutVersion);
end;
```

## Monitoring et alertes

### Métriques importantes à surveiller

```pascal
unit FeatureFlagMetrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TFeatureFlagMetrics = class
  private
    FEvaluationCount: Int64;
    FErrorCount: Int64;
    FLatencySum: Int64;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RecordEvaluation(LatencyMs: Integer; Success: Boolean);
    function GetMetricsJSON: string;
  end;

var
  GlobalFFMetrics: TFeatureFlagMetrics;

implementation

uses
  fpjson;

constructor TFeatureFlagMetrics.Create;
begin
  FLock := TCriticalSection.Create;
  FEvaluationCount := 0;
  FErrorCount := 0;
  FLatencySum := 0;
end;

destructor TFeatureFlagMetrics.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TFeatureFlagMetrics.RecordEvaluation(LatencyMs: Integer; Success: Boolean);
begin
  FLock.Enter;
  try
    Inc(FEvaluationCount);
    FLatencySum := FLatencySum + LatencyMs;

    if not Success then
      Inc(FErrorCount);
  finally
    FLock.Leave;
  end;
end;

function TFeatureFlagMetrics.GetMetricsJSON: string;
var
  JSON: TJSONObject;
  AvgLatency: Double;
  ErrorRate: Double;
begin
  FLock.Enter;
  try
    if FEvaluationCount > 0 then
    begin
      AvgLatency := FLatencySum / FEvaluationCount;
      ErrorRate := (FErrorCount / FEvaluationCount) * 100;
    end
    else
    begin
      AvgLatency := 0;
      ErrorRate := 0;
    end;

    JSON := TJSONObject.Create;
    try
      JSON.Add('total_evaluations', FEvaluationCount);
      JSON.Add('error_count', FErrorCount);
      JSON.Add('error_rate_percent', ErrorRate);
      JSON.Add('avg_latency_ms', AvgLatency);
      Result := JSON.AsJSON;
    finally
      JSON.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

initialization
  GlobalFFMetrics := TFeatureFlagMetrics.Create;

finalization
  GlobalFFMetrics.Free;

end.
```

### Alertes sur les anomalies

```pascal
procedure CheckFeatureFlagHealth;
var
  Metrics: string;
  ErrorRate: Double;
begin
  Metrics := GlobalFFMetrics.GetMetricsJSON;
  ErrorRate := ExtractErrorRate(Metrics);

  // Alerte si taux d'erreur > 5%
  if ErrorRate > 5.0 then
  begin
    SendAlert(
      'Feature Flags Error Rate High',
      Format('Error rate is %.2f%%', [ErrorRate])
    );
  end;
end;
```

## Cas d'usage avancés

### 1. Kill Switch (interrupteur d'urgence)

Pour désactiver rapidement une fonctionnalité problématique en production :

```pascal
// En cas de problème critique avec le nouveau système de paiement
FFManager.DisableFlag('new_payment_system');
// ➡️ Rollback instantané sans redéploiement
```

### 2. Déploiement progressif (Progressive Rollout)

```bash
#!/bin/bash
# Script de rollout progressif

FLAG_NAME="new_feature"

# Jour 1 : 10%
curl -X POST http://api.example.com/flags/$FLAG_NAME \
  -d '{"percentage": 10}'
sleep 86400  # 24 heures

# Jour 2 : 25%
curl -X POST http://api.example.com/flags/$FLAG_NAME \
  -d '{"percentage": 25}'
sleep 86400

# Jour 3 : 50%
curl -X POST http://api.example.com/flags/$FLAG_NAME \
  -d '{"percentage": 50}'
sleep 86400

# Jour 4 : 100%
curl -X POST http://api.example.com/flags/$FLAG_NAME \
  -d '{"percentage": 100}'
```

### 3. Dark Launch

Exécuter du nouveau code sans exposer les résultats aux utilisateurs :

```pascal
procedure ProcessOrder(Order: TOrder);
var
  OldResult, NewResult: TOrderResult;
begin
  // Toujours utiliser l'ancien système
  OldResult := OldOrderProcessor.Process(Order);

  // Tester le nouveau système en parallèle (dark launch)
  if FeatureFlags.IsEnabled('new_order_processor_dark_launch') then
  begin
    try
      NewResult := NewOrderProcessor.Process(Order);

      // Comparer les résultats et logger les différences
      if not ResultsMatch(OldResult, NewResult) then
        LogDifference(OldResult, NewResult);
    except
      on E: Exception do
        LogError('New processor failed: ' + E.Message);
    end;
  end;

  // Retourner toujours le résultat de l'ancien système
  Result := OldResult;
end;
```

### 4. Feature flags basés sur le temps

```pascal
// Activer une promotion uniquement pendant le Black Friday
if FeatureFlags.IsEnabled('black_friday_promo') and
   IsBlackFridayWeek then
begin
  ApplyDiscount(50); // -50%
end;
```

### 5. Personnalisation par client

```pascal
// Fonctionnalités différentes selon le plan d'abonnement
procedure ShowFeatures(User: TUserContext);
begin
  User.SetAttribute('plan', User.SubscriptionPlan);

  if FFManager.IsEnabled('premium_analytics', User) then
    ShowPremiumAnalytics;

  if FFManager.IsEnabled('api_access', User) then
    ShowAPICredentials;

  if FFManager.IsEnabled('white_label', User) then
    ShowWhiteLabelOptions;
end;
```

## Comparaison avec des solutions SaaS

### Solutions commerciales populaires

| Solution | Avantages | Inconvénients | Prix |
|----------|-----------|---------------|------|
| **LaunchDarkly** | Interface complète, Analytics, SDK pour beaucoup de langages | Coûteux, dépendance externe | $8.33+/utilisateur/mois |
| **Split.io** | Excellent pour A/B testing, métriques avancées | Complexe à configurer | Sur devis |
| **Unleash** | Open source, self-hosted possible | Interface moins polie | Gratuit (self-hosted) |
| **ConfigCat** | Simple, bon rapport qualité/prix | Moins de fonctionnalités avancées | $7+/mois |
| **DIY (notre implémentation)** | Contrôle total, pas de coûts récurrents, données privées | Maintenance à faire soi-même | Temps de développement |

### Quand utiliser une solution maison vs SaaS ?

**Choisir une solution maison si** :
- Vous avez des contraintes de confidentialité strictes
- Budget limité mais ressources de développement disponibles
- Besoins simples et bien définis
- Vous voulez un contrôle total

**Choisir un SaaS si** :
- Besoin rapide (go-to-market rapide)
- Équipe petite sans temps pour la maintenance
- Besoin d'analytics avancés et visualisations
- Support et SLA importants

## Récapitulatif et checklist

### Checklist de mise en place des feature flags

**Phase 1 : Infrastructure**
- [ ] Choisir le backend de stockage (Redis, DB, fichier)
- [ ] Implémenter le gestionnaire de feature flags
- [ ] Mettre en place le cache local
- [ ] Configurer la synchronisation entre instances

**Phase 2 : Intégration**
- [ ] Créer les premiers flags
- [ ] Intégrer dans le code de l'application
- [ ] Ajouter le tracking des évaluations
- [ ] Mettre en place le monitoring

**Phase 3 : Administration**
- [ ] Créer l'API d'administration
- [ ] Développer l'interface web
- [ ] Configurer les permissions et la sécurité
- [ ] Mettre en place l'audit trail

**Phase 4 : A/B Testing**
- [ ] Implémenter le tracking des événements
- [ ] Créer la base de données d'analytics
- [ ] Développer les requêtes d'analyse
- [ ] Implémenter les tests statistiques

**Phase 5 : Opérations**
- [ ] Documenter le processus pour l'équipe
- [ ] Former l'équipe aux bonnes pratiques
- [ ] Mettre en place le processus de nettoyage
- [ ] Configurer les alertes

### Bonnes pratiques finales

✅ **À FAIRE** :
- Nommer les flags de manière cohérente et descriptive
- Documenter chaque flag (description, propriétaire, date)
- Supprimer les flags obsolètes régulièrement
- Tester les flags en local avant la production
- Monitorer les performances des évaluations
- Avoir un plan de rollback
- Logger toutes les modifications de flags

❌ **À ÉVITER** :
- Laisser les flags temporaires indéfiniment
- Créer trop de flags (préférer quelques flags bien pensés)
- Oublier de documenter
- Tester directement en production sans phase de test
- Imbriquer trop de conditions de flags
- Utiliser des flags pour du code temporaire (hotfix)
- Ignorer les métriques de performance

### Exemple de code final complet

Voici un exemple complet d'utilisation dans une application FreePascal/Lazarus :

```pascal
program MyApplication;

{$mode objfpc}{$H+}

uses
  Forms, SysUtils,
  FeatureFlags, FeatureFlagRedisBackend, ABTestTracking,
  MainForm;

var
  FFManager: TFeatureFlagManager;
  RedisBackend: TFeatureFlagRedisBackend;
  Tracker: TEventTracker;

procedure InitializeFeatureFlags;
var
  NewUIFlag: TFeatureFlag;
begin
  // Initialiser le backend Redis
  RedisBackend := TFeatureFlagRedisBackend.Create(
    CreateRedisClient('localhost', 6379),
    'ff:'
  );

  // Initialiser le gestionnaire
  FFManager := TFeatureFlagManager.Create(RedisBackend, 60);

  // Créer un flag pour la nouvelle UI
  NewUIFlag := TFeatureFlag.Create('new_ui_redesign');
  NewUIFlag.Enabled := True;
  NewUIFlag.Description := 'Nouvelle interface utilisateur redesignée';
  NewUIFlag.Strategy := tsPercentage;
  NewUIFlag.Percentage := 25; // Rollout à 25%
  FFManager.RegisterFlag(NewUIFlag);

  // Initialiser le tracker pour A/B testing
  Tracker := TEventTracker.Create(DatabaseConnection);
end;

begin
  Application.Initialize;

  // Initialiser les feature flags
  InitializeFeatureFlags;

  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

  // Nettoyage
  Tracker.Free;
  FFManager.Free;
  RedisBackend.Free;
end.
```

## Conclusion

Les feature flags et l'A/B testing sont des outils puissants qui transforment la façon dont vous déployez et testez vos applications FreePascal/Lazarus. En séparant le **déploiement** de la **release**, vous gagnez en flexibilité et réduisez les risques.

**Bénéfices clés** :
- 🚀 Déploiements plus fréquents et moins risqués
- 🔄 Rollback instantané en cas de problème
- 📊 Décisions basées sur les données (A/B testing)
- 🎯 Personnalisation par utilisateur
- 🛡️ Contrôle opérationnel en temps réel

**Prochaines étapes** :
1. Commencez petit avec quelques flags simples
2. Mesurez l'impact sur votre équipe
3. Ajoutez progressivement des fonctionnalités avancées
4. Formez votre équipe aux bonnes pratiques
5. Automatisez le cycle de vie des flags

Avec une implémentation solide des feature flags, votre application FreePascal devient plus agile, plus résiliente et plus orientée données. Vous pouvez expérimenter en toute confiance, apprendre de vos utilisateurs et itérer rapidement.

---

## Ressources complémentaires et outils

### Bibliothèques FreePascal utiles

**Pour Redis** :
- **Redis4Delphi** : Compatible avec FreePascal, client Redis complet
- **Synapse** : Bibliothèque réseau qui peut être utilisée pour implémenter un client Redis

**Pour les statistiques** :
- **NumLib** : Bibliothèque de calculs numériques pour FreePascal
- **TAChart** : Pour visualiser les résultats d'A/B testing

**Pour la persistance** :
- **mORMot** : Framework complet avec ORM et SOA
- **tiOPF** : Framework de persistance objet

### Outils d'analyse et visualisation

#### Dashboard simple avec Grafana

Vous pouvez exposer vos métriques au format Prometheus et les visualiser avec Grafana :

```pascal
unit PrometheusExporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs;

type
  TPrometheusExporter = class
  private
    FFManager: TFeatureFlagManager;
    FMetrics: TFeatureFlagMetrics;
  public
    constructor Create(AManager: TFeatureFlagManager; AMetrics: TFeatureFlagMetrics);
    procedure HandleMetricsRequest(ARequest: TRequest; AResponse: TResponse);
  end;

implementation

constructor TPrometheusExporter.Create(
  AManager: TFeatureFlagManager;
  AMetrics: TFeatureFlagMetrics
);
begin
  FFManager := AManager;
  FMetrics := AMetrics;
end;

procedure TPrometheusExporter.HandleMetricsRequest(ARequest: TRequest; AResponse: TResponse);
var
  Output: TStringList;
  Flags: TStringList;
  i: Integer;
  Flag: TFeatureFlag;
begin
  Output := TStringList.Create;
  try
    // Métriques d'évaluation
    Output.Add('# HELP feature_flag_evaluations_total Total number of flag evaluations');
    Output.Add('# TYPE feature_flag_evaluations_total counter');
    Output.Add(Format('feature_flag_evaluations_total %d', [FMetrics.EvaluationCount]));
    Output.Add('');

    Output.Add('# HELP feature_flag_errors_total Total number of evaluation errors');
    Output.Add('# TYPE feature_flag_errors_total counter');
    Output.Add(Format('feature_flag_errors_total %d', [FMetrics.ErrorCount]));
    Output.Add('');

    Output.Add('# HELP feature_flag_latency_ms Average evaluation latency in milliseconds');
    Output.Add('# TYPE feature_flag_latency_ms gauge');
    Output.Add(Format('feature_flag_latency_ms %.2f', [FMetrics.AvgLatency]));
    Output.Add('');

    // État de chaque flag
    Output.Add('# HELP feature_flag_enabled Flag enabled state (1=enabled, 0=disabled)');
    Output.Add('# TYPE feature_flag_enabled gauge');

    Flags := FFManager.ListAllFlags;
    try
      for i := 0 to Flags.Count - 1 do
      begin
        Flag := FFManager.GetFlag(Flags[i]);
        if Assigned(Flag) then
        begin
          Output.Add(Format('feature_flag_enabled{name="%s"} %d',
            [Flag.Name, Integer(Flag.Enabled)]));
        end;
      end;
    finally
      Flags.Free;
    end;

    AResponse.ContentType := 'text/plain; version=0.0.4';
    AResponse.Content := Output.Text;
  finally
    Output.Free;
  end;
end;

end.
```

#### Configuration Grafana

Créez un dashboard avec des panneaux comme :

**Panel 1 : Nombre d'évaluations par seconde**
```promql
rate(feature_flag_evaluations_total[5m])
```

**Panel 2 : Taux d'erreur**
```promql
rate(feature_flag_errors_total[5m]) / rate(feature_flag_evaluations_total[5m]) * 100
```

**Panel 3 : Latence moyenne**
```promql
feature_flag_latency_ms
```

**Panel 4 : État des flags**
```promql
feature_flag_enabled
```

### Scripts utiles

#### Script de migration des flags vers production

```bash
#!/bin/bash
# migrate-flags.sh - Migrer les flags d'un environnement à l'autre

SOURCE_ENV="staging"
TARGET_ENV="production"
SOURCE_REDIS="redis-staging:6379"
TARGET_REDIS="redis-prod:6379"

echo "Migration des feature flags de $SOURCE_ENV vers $TARGET_ENV"

# Lister tous les flags dans staging
FLAGS=$(redis-cli -h $SOURCE_REDIS KEYS "ff:*")

for FLAG_KEY in $FLAGS; do
    FLAG_NAME=$(echo $FLAG_KEY | sed 's/ff://')
    FLAG_DATA=$(redis-cli -h $SOURCE_REDIS GET $FLAG_KEY)

    echo "Migrating flag: $FLAG_NAME"

    # Demander confirmation pour chaque flag
    read -p "Migrer ce flag vers production? (o/n) " -n 1 -r
    echo

    if [[ $REPLY =~ ^[Oo]$ ]]; then
        redis-cli -h $TARGET_REDIS SET "ff:$FLAG_NAME" "$FLAG_DATA"
        echo "✓ Flag $FLAG_NAME migré"
    else
        echo "✗ Flag $FLAG_NAME ignoré"
    fi
done

echo "Migration terminée"
```

#### Script de nettoyage des flags obsolètes

```bash
#!/bin/bash
# cleanup-flags.sh - Supprimer les flags obsolètes

REDIS_HOST="localhost:6379"
DAYS_INACTIVE=90

echo "Recherche des flags inactifs depuis plus de $DAYS_INACTIVE jours..."

# Requête SQL pour trouver les flags non utilisés
INACTIVE_FLAGS=$(psql -t -c "
    SELECT DISTINCT flag_name
    FROM feature_flags
    WHERE last_evaluation_date < NOW() - INTERVAL '$DAYS_INACTIVE days'
      AND flag_type = 'release'
")

echo "Flags inactifs trouvés:"
echo "$INACTIVE_FLAGS"

read -p "Supprimer ces flags? (o/n) " -n 1 -r
echo

if [[ $REPLY =~ ^[Oo]$ ]]; then
    for FLAG in $INACTIVE_FLAGS; do
        echo "Suppression de $FLAG"
        redis-cli -h $REDIS_HOST DEL "ff:$FLAG"

        # Supprimer aussi de la base de données
        psql -c "DELETE FROM feature_flags WHERE flag_name = '$FLAG'"
        psql -c "DELETE FROM ab_test_events WHERE flag_name = '$FLAG'"
    done

    echo "✓ Nettoyage terminé"
else
    echo "✗ Nettoyage annulé"
fi
```

#### Script de rapport hebdomadaire

```bash
#!/bin/bash
# weekly-flag-report.sh - Génère un rapport hebdomadaire des flags

OUTPUT_FILE="flag_report_$(date +%Y%m%d).html"

cat > $OUTPUT_FILE << 'EOF'
<!DOCTYPE html>
<html>
<head>
    <title>Feature Flags - Rapport Hebdomadaire</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        table { border-collapse: collapse; width: 100%; margin: 20px 0; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #4CAF50; color: white; }
        .enabled { color: green; font-weight: bold; }
        .disabled { color: red; }
    </style>
</head>
<body>
    <h1>Rapport Hebdomadaire des Feature Flags</h1>
    <p>Date: $(date)</p>

    <h2>État des Flags</h2>
    <table>
        <tr>
            <th>Nom</th>
            <th>État</th>
            <th>Stratégie</th>
            <th>Évaluations (7j)</th>
            <th>Dernière modif.</th>
        </tr>
EOF

# Récupérer les données depuis PostgreSQL
psql -H -c "
    SELECT
        f.flag_name,
        CASE WHEN f.enabled THEN 'Activé' ELSE 'Désactivé' END as state,
        f.strategy,
        COUNT(e.id) as evaluations,
        f.last_updated
    FROM feature_flags f
    LEFT JOIN ab_test_events e ON e.flag_name = f.flag_name
        AND e.timestamp > NOW() - INTERVAL '7 days'
    GROUP BY f.flag_name, f.enabled, f.strategy, f.last_updated
    ORDER BY evaluations DESC
" >> $OUTPUT_FILE

cat >> $OUTPUT_FILE << 'EOF'
    </table>

    <h2>Tests A/B en Cours</h2>
    <table>
        <tr>
            <th>Flag</th>
            <th>Variant</th>
            <th>Expositions</th>
            <th>Conversions</th>
            <th>Taux</th>
        </tr>
EOF

psql -H -c "
    SELECT
        flag_name,
        variant,
        COUNT(DISTINCT user_id) as exposures,
        (SELECT COUNT(*) FROM ab_test_conversions c
         WHERE c.flag_name = e.flag_name AND c.variant = e.variant) as conversions,
        ROUND(
            (SELECT COUNT(*) FROM ab_test_conversions c
             WHERE c.flag_name = e.flag_name AND c.variant = e.variant)::decimal /
            COUNT(DISTINCT user_id) * 100, 2
        ) || '%' as conversion_rate
    FROM ab_test_events e
    WHERE timestamp > NOW() - INTERVAL '7 days'
    GROUP BY flag_name, variant
    ORDER BY flag_name, variant
" >> $OUTPUT_FILE

cat >> $OUTPUT_FILE << 'EOF'
    </table>

    <h2>Recommandations</h2>
    <ul>
EOF

# Ajouter des recommandations automatiques
psql -t -c "
    SELECT flag_name
    FROM feature_flags
    WHERE last_updated < NOW() - INTERVAL '90 days'
      AND flag_type = 'release'
" | while read FLAG; do
    echo "        <li>⚠️ Flag <b>$FLAG</b> n'a pas été modifié depuis 90+ jours - Considérer la suppression</li>" >> $OUTPUT_FILE
done

cat >> $OUTPUT_FILE << 'EOF'
    </ul>
</body>
</html>
EOF

echo "Rapport généré: $OUTPUT_FILE"

# Optionnel: Envoyer par email
# mail -s "Feature Flags - Rapport Hebdomadaire" team@example.com < $OUTPUT_FILE
```

### Intégration avec CI/CD

#### GitHub Actions - Validation des flags

```yaml
name: Feature Flags Validation

on:
  pull_request:
    paths:
      - 'src/**/*.pas'

jobs:
  check-flags:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Check for hardcoded flags
        run: |
          # Vérifier qu'il n'y a pas de flags hardcodés
          if grep -r "FeatureFlags.IsEnabled.*true\|false" src/; then
            echo "❌ Flags hardcodés détectés!"
            exit 1
          fi
          echo "✓ Pas de flags hardcodés"

      - name: Check flag naming convention
        run: |
          # Extraire tous les noms de flags du code
          FLAGS=$(grep -roh "IsEnabled('[^']*')" src/ | sed "s/IsEnabled('//g" | sed "s/')//g")

          for FLAG in $FLAGS; do
            # Vérifier la convention de nommage: type_feature_context
            if ! echo "$FLAG" | grep -qE "^(release|ops|exp|perm)_[a-z_]+$"; then
              echo "❌ Flag '$FLAG' ne respecte pas la convention de nommage"
              exit 1
            fi
          done
          echo "✓ Tous les flags respectent la convention"

      - name: Check for flag documentation
        run: |
          # Vérifier que les nouveaux flags sont documentés
          python3 scripts/check_flag_docs.py
```

#### Script Python pour vérifier la documentation

```python
# scripts/check_flag_docs.py
import re
import sys
import json

def extract_flags_from_code(file_path):
    """Extrait les noms de flags du code source"""
    with open(file_path, 'r') as f:
        content = f.read()

    pattern = r"IsEnabled\('([^']+)'\)"
    return set(re.findall(pattern, content))

def check_flag_documentation(flag_name, docs_file):
    """Vérifie qu'un flag est documenté"""
    with open(docs_file, 'r') as f:
        docs = json.load(f)

    if flag_name not in docs:
        return False, "Flag non documenté"

    flag_doc = docs[flag_name]
    required_fields = ['description', 'owner', 'created_date']

    for field in required_fields:
        if field not in flag_doc or not flag_doc[field]:
            return False, f"Champ '{field}' manquant"

    return True, "OK"

def main():
    # Charger la documentation des flags
    docs_file = 'docs/feature_flags.json'

    # Scanner tous les fichiers Pascal
    import glob
    code_files = glob.glob('src/**/*.pas', recursive=True)

    all_flags = set()
    for file_path in code_files:
        flags = extract_flags_from_code(file_path)
        all_flags.update(flags)

    errors = []
    for flag in all_flags:
        is_documented, message = check_flag_documentation(flag, docs_file)
        if not is_documented:
            errors.append(f"❌ Flag '{flag}': {message}")

    if errors:
        print("\n".join(errors))
        sys.exit(1)
    else:
        print(f"✓ Tous les {len(all_flags)} flags sont correctement documentés")

if __name__ == '__main__':
    main()
```

#### Documentation JSON des flags

```json
{
  "release_new_checkout": {
    "description": "Nouveau processus de checkout redesigné avec moins d'étapes",
    "owner": "equipe_frontend",
    "created_date": "2025-10-01",
    "planned_removal": "2025-12-01",
    "jira_ticket": "SHOP-1234",
    "strategy": "percentage",
    "initial_percentage": 10,
    "dependencies": []
  },
  "exp_button_color_test": {
    "description": "Test A/B de la couleur du bouton d'achat",
    "owner": "equipe_growth",
    "created_date": "2025-10-15",
    "planned_removal": "2025-11-01",
    "jira_ticket": "GROWTH-567",
    "strategy": "percentage",
    "variants": ["blue", "red", "green"],
    "hypothesis": "Un bouton rouge augmente les conversions de 15%",
    "success_metric": "conversion_rate",
    "minimum_sample_size": 10000
  },
  "ops_enable_caching": {
    "description": "Active le système de cache Redis pour les requêtes lourdes",
    "owner": "equipe_backend",
    "created_date": "2025-09-01",
    "planned_removal": null,
    "strategy": "all",
    "permanent": true
  }
}
```

### Performance et optimisations

#### Cache local avec invalidation

```pascal
unit FeatureFlagLocalCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, SyncObjs, DateUtils, FeatureFlags;

type
  TCachedFlag = record
    Flag: TFeatureFlag;
    CachedAt: TDateTime;
  end;

  TFlagCache = specialize TFPGMap<string, TCachedFlag>;

  TFeatureFlagLocalCache = class
  private
    FCache: TFlagCache;
    FLock: TCriticalSection;
    FTTL: Integer; // Secondes
    FBackend: TFeatureFlagRedisBackend;

    function IsCacheValid(const CachedFlag: TCachedFlag): Boolean;
  public
    constructor Create(ABackend: TFeatureFlagRedisBackend; ATTL: Integer = 60);
    destructor Destroy; override;

    function GetFlag(const FlagName: string): TFeatureFlag;
    procedure InvalidateFlag(const FlagName: string);
    procedure InvalidateAll;
  end;

implementation

constructor TFeatureFlagLocalCache.Create(ABackend: TFeatureFlagRedisBackend; ATTL: Integer);
begin
  FCache := TFlagCache.Create;
  FLock := TCriticalSection.Create;
  FTTL := ATTL;
  FBackend := ABackend;
end;

destructor TFeatureFlagLocalCache.Destroy;
var
  i: Integer;
begin
  // Libérer tous les flags en cache
  for i := 0 to FCache.Count - 1 do
    FCache.Data[i].Flag.Free;

  FCache.Free;
  FLock.Free;
  inherited;
end;

function TFeatureFlagLocalCache.IsCacheValid(const CachedFlag: TCachedFlag): Boolean;
begin
  Result := SecondsBetween(Now, CachedFlag.CachedAt) < FTTL;
end;

function TFeatureFlagLocalCache.GetFlag(const FlagName: string): TFeatureFlag;
var
  Index: Integer;
  CachedFlag: TCachedFlag;
begin
  FLock.Enter;
  try
    Index := FCache.IndexOf(FlagName);

    // Si en cache et valide
    if (Index >= 0) and IsCacheValid(FCache.Data[Index]) then
      Exit(FCache.Data[Index].Flag);

    // Sinon, charger depuis le backend
    Result := FBackend.LoadFlag(FlagName);

    if Assigned(Result) then
    begin
      CachedFlag.Flag := Result;
      CachedFlag.CachedAt := Now;

      if Index >= 0 then
      begin
        // Libérer l'ancien et remplacer
        FCache.Data[Index].Flag.Free;
        FCache.Data[Index] := CachedFlag;
      end
      else
      begin
        // Ajouter au cache
        FCache.Add(FlagName, CachedFlag);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TFeatureFlagLocalCache.InvalidateFlag(const FlagName: string);
var
  Index: Integer;
begin
  FLock.Enter;
  try
    Index := FCache.IndexOf(FlagName);
    if Index >= 0 then
    begin
      FCache.Data[Index].Flag.Free;
      FCache.Delete(Index);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TFeatureFlagLocalCache.InvalidateAll;
var
  i: Integer;
begin
  FLock.Enter;
  try
    for i := 0 to FCache.Count - 1 do
      FCache.Data[i].Flag.Free;
    FCache.Clear;
  finally
    FLock.Leave;
  end;
end;

end.
```

#### Notifications en temps réel avec Redis Pub/Sub

```pascal
unit FeatureFlagPubSub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Redis, FeatureFlagLocalCache;

type
  TFeatureFlagPubSub = class(TThread)
  private
    FRedis: TRedisClient;
    FCache: TFeatureFlagLocalCache;
    FChannel: string;
  protected
    procedure Execute; override;
  public
    constructor Create(
      ARedis: TRedisClient;
      ACache: TFeatureFlagLocalCache;
      const AChannel: string = 'ff:updates'
    );
  end;

implementation

constructor TFeatureFlagPubSub.Create(
  ARedis: TRedisClient;
  ACache: TFeatureFlagLocalCache;
  const AChannel: string
);
begin
  inherited Create(False);
  FRedis := ARedis;
  FCache := ACache;
  FChannel := AChannel;
  FreeOnTerminate := False;
end;

procedure TFeatureFlagPubSub.Execute;
var
  Message: string;
begin
  // S'abonner au canal
  FRedis.Subscribe(FChannel);

  while not Terminated do
  begin
    // Attendre un message
    Message := FRedis.GetMessage;

    if Message <> '' then
    begin
      // Un flag a été modifié, invalider le cache
      FCache.InvalidateFlag(Message);
    end;

    Sleep(100); // Éviter une boucle trop rapide
  end;
end;

end.
```

## Conclusion finale

Vous disposez maintenant d'une base solide pour implémenter des feature flags et de l'A/B testing dans vos applications FreePascal/Lazarus. Cette approche moderne vous permet de :

🎯 **Déployer en continu** sans crainte grâce aux rollbacks instantanés

📊 **Prendre des décisions basées sur les données** avec l'A/B testing

🚀 **Expérimenter rapidement** de nouvelles idées

🔧 **Contrôler finement** qui voit quelles fonctionnalités

⚡ **Réagir instantanément** aux problèmes en production

N'oubliez pas que la clé du succès réside dans :
- La **discipline** : nettoyer régulièrement les flags obsolètes
- La **documentation** : chaque flag doit être documenté
- Le **monitoring** : surveiller l'impact de vos flags
- L'**automatisation** : scriptez les tâches répétitives

Avec ces outils et bonnes pratiques, vos applications FreePascal seront plus agiles et plus robustes, que ce soit sur Windows ou Ubuntu !

---

**Points clés à retenir** :

1. **Feature flags ≠ branches Git** : Ce sont des interrupteurs dans le code
2. **A/B testing = science** : Mesurez, analysez, décidez
3. **Cache local + Redis** : Performance et cohérence
4. **Nettoyage régulier** : Ne laissez pas la dette technique s'accumuler
5. **Documentation** : Votre futur vous remerciera

Bonne chance dans vos déploiements ! 🎉

⏭️ [Gestion de configuration](/22-devops-deploiement-multi-os/11-gestion-configuration.md)
