🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.5 Dependency Injection et IoC Containers

## Introduction

La **Dependency Injection** (Injection de Dépendances) et l'**Inversion of Control** (Inversion de Contrôle) sont des patterns fondamentaux pour créer des applications modulaires, testables et maintenables.

### Qu'est-ce qu'une dépendance ?

Une dépendance est un objet dont une classe a besoin pour fonctionner.

```pascal
// TServiceCommande DÉPEND de IRepository
type
  TServiceCommande = class
  private
    FRepository: IRepository;  // ← Dépendance
  end;
```

### Le problème sans Dependency Injection

```pascal
// ❌ MAUVAIS : Dépendances créées en dur
type
  TServiceCommande = class
  private
    FRepository: TRepositorySQL;  // ← Couplage fort !
  public
    constructor Create;
  end;

constructor TServiceCommande.Create;  
begin
  inherited Create;
  // Création directe = couplage fort
  FRepository := TRepositorySQL.Create;  // ← Impossible à remplacer !
end;
```

**Problèmes :**
- ❌ Impossible de tester sans base de données
- ❌ Impossible de changer d'implémentation
- ❌ Couplage fort entre les classes
- ❌ Difficile à maintenir

### La solution : Dependency Injection

```pascal
// ✅ BON : Dépendances injectées
type
  TServiceCommande = class
  private
    FRepository: IRepository;  // ← Interface, pas implémentation
  public
    constructor Create(const ARepository: IRepository);  // ← Injection !
  end;

constructor TServiceCommande.Create(const ARepository: IRepository);  
begin
  inherited Create;
  FRepository := ARepository;  // ← Reçoit la dépendance de l'extérieur
end;

// Utilisation
var
  Repository: IRepository;
  Service: TServiceCommande;
begin
  // On décide quelle implémentation utiliser
  Repository := TRepositorySQL.Create(Connection);
  // ou
  Repository := TRepositoryMemoire.Create;
  // ou
  Repository := TMockRepository.Create;

  // Injection de la dépendance
  Service := TServiceCommande.Create(Repository);
end;
```

**Avantages :**
- ✅ Testable facilement (injection de mocks)
- ✅ Flexible (changement d'implémentation)
- ✅ Faible couplage
- ✅ Respect du principe SOLID

## Les trois types d'injection

### 1. Constructor Injection (Injection par constructeur)

**Le plus recommandé** : les dépendances sont obligatoires et immutables.

```pascal
type
  TServiceCommande = class
  private
    FRepository: IRepository;
    FLogger: ILogger;
  public
    constructor Create(const ARepository: IRepository;
                      const ALogger: ILogger);
  end;

constructor TServiceCommande.Create(const ARepository: IRepository;
  const ALogger: ILogger);
begin
  inherited Create;

  if not Assigned(ARepository) then
    raise Exception.Create('Repository requis');
  if not Assigned(ALogger) then
    raise Exception.Create('Logger requis');

  FRepository := ARepository;
  FLogger := ALogger;
end;
```

**Avantages :**
- Dépendances clairement visibles
- Objet toujours dans un état valide
- Immutabilité des dépendances

### 2. Property Injection (Injection par propriété)

Utile pour les **dépendances optionnelles**.

```pascal
type
  TServiceCommande = class
  private
    FRepository: IRepository;
    FCache: ICache;  // Optionnel
  public
    constructor Create(const ARepository: IRepository);

    property Cache: ICache read FCache write FCache;  // ← Injection optionnelle
  end;

constructor TServiceCommande.Create(const ARepository: IRepository);  
begin
  inherited Create;
  FRepository := ARepository;
  FCache := nil;  // Optionnel
end;

// Utilisation
var
  Service: TServiceCommande;
begin
  Service := TServiceCommande.Create(Repository);

  // Cache optionnel
  if CacheDisponible then
    Service.Cache := CacheInstance;
end;
```

### 3. Method Injection (Injection par méthode)

Pour les dépendances qui varient à chaque appel.

```pascal
type
  TServiceCommande = class
  public
    procedure TraiterCommande(const ACommande: TCommande;
                             const ANotificateur: INotificateur);
  end;

procedure TServiceCommande.TraiterCommande(const ACommande: TCommande;
  const ANotificateur: INotificateur);
begin
  // Traiter la commande
  // ...

  // Utiliser le notificateur passé en paramètre
  ANotificateur.Notifier('Commande traitée');
end;
```

## Inversion of Control (IoC)

L'**IoC** est le principe : au lieu que votre code crée ses dépendances, c'est un framework externe qui les fournit.

```
CONTRÔLE NORMAL (sans IoC)
┌─────────────────────────┐
│  MonService             │
│  ┌──────────────────┐   │
│  │ Je crée moi-même │   │
│  │ mes dépendances  │   │
│  └──────────────────┘   │
└─────────────────────────┘

INVERSION OF CONTROL (avec IoC)
┌─────────────────────────┐
│  Container IoC          │
│  ┌──────────────────┐   │
│  │ Je fournis les   │   │
│  │ dépendances      │   │
│  └────────┬─────────┘   │
└───────────┼─────────────┘
            │
            ▼
    ┌───────────────┐
    │  MonService   │
    │ (reçoit tout) │
    └───────────────┘
```

## IoC Container (Conteneur d'injection)

Un **conteneur IoC** est un framework qui :
1. Enregistre les types et leurs dépendances
2. Crée automatiquement les instances
3. Injecte automatiquement les dépendances
4. Gère le cycle de vie des objets

### Container simple pour FreePascal

```pascal
unit DI.Container;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  // Types de cycle de vie
  TLifetimeScope = (
    lsTransient,   // Nouvelle instance à chaque fois
    lsSingleton,   // Une seule instance partagée
    lsScoped       // Une instance par scope
  );

  // Factory pour créer des instances
  TFactoryFunc = function: IInterface;

  // Enregistrement d'un service
  TServiceRegistration = class
  private
    FInterfaceGUID: TGUID;
    FFactory: TFactoryFunc;
    FLifetime: TLifetimeScope;
    FSingletonInstance: IInterface;
  public
    constructor Create(const AGUID: TGUID; AFactory: TFactoryFunc;
                      ALifetime: TLifetimeScope);

    function CreateInstance: IInterface;

    property InterfaceGUID: TGUID read FInterfaceGUID;
    property Lifetime: TLifetimeScope read FLifetime;
  end;

  // Conteneur IoC
  TDIContainer = class
  private
    FServices: TObjectDictionary<string, TServiceRegistration>;
    FLock: TRTLCriticalSection;

    function GUIDToString(const AGUID: TGUID): string;
  public
    constructor Create;
    destructor Destroy; override;

    // Enregistrement
    procedure RegisterTransient(const AGUID: TGUID; AFactory: TFactoryFunc);
    procedure RegisterSingleton(const AGUID: TGUID; AFactory: TFactoryFunc);

    // Résolution
    function Resolve(const AGUID: TGUID): IInterface;
    function TryResolve(const AGUID: TGUID; out AService: IInterface): Boolean;

    // Vérification
    function IsRegistered(const AGUID: TGUID): Boolean;
  end;

implementation

constructor TServiceRegistration.Create(const AGUID: TGUID;
  AFactory: TFactoryFunc; ALifetime: TLifetimeScope);
begin
  inherited Create;
  FInterfaceGUID := AGUID;
  FFactory := AFactory;
  FLifetime := ALifetime;
  FSingletonInstance := nil;
end;

function TServiceRegistration.CreateInstance: IInterface;  
begin
  case FLifetime of
    lsTransient:
      // Nouvelle instance à chaque fois
      Result := FFactory();

    lsSingleton:
      begin
        // Créer une seule fois et réutiliser
        if not Assigned(FSingletonInstance) then
          FSingletonInstance := FFactory();
        Result := FSingletonInstance;
      end;

    lsScoped:
      // Pour simplifier, traité comme transient ici
      Result := FFactory();
  end;
end;

constructor TDIContainer.Create;  
begin
  inherited Create;
  FServices := TObjectDictionary<string, TServiceRegistration>.Create([doOwnsValues]);
  InitCriticalSection(FLock);
end;

destructor TDIContainer.Destroy;  
begin
  DoneCriticalSection(FLock);
  FServices.Free;
  inherited;
end;

function TDIContainer.GUIDToString(const AGUID: TGUID): string;  
begin
  Result := GUIDToString(AGUID);
end;

procedure TDIContainer.RegisterTransient(const AGUID: TGUID; AFactory: TFactoryFunc);  
var
  Registration: TServiceRegistration;
  Key: string;
begin
  EnterCriticalSection(FLock);
  try
    Key := GUIDToString(AGUID);
    Registration := TServiceRegistration.Create(AGUID, AFactory, lsTransient);
    FServices.AddOrSetValue(Key, Registration);

    WriteLn(Format('[Container] Transient enregistré: %s', [Key]));
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TDIContainer.RegisterSingleton(const AGUID: TGUID; AFactory: TFactoryFunc);  
var
  Registration: TServiceRegistration;
  Key: string;
begin
  EnterCriticalSection(FLock);
  try
    Key := GUIDToString(AGUID);
    Registration := TServiceRegistration.Create(AGUID, AFactory, lsSingleton);
    FServices.AddOrSetValue(Key, Registration);

    WriteLn(Format('[Container] Singleton enregistré: %s', [Key]));
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TDIContainer.Resolve(const AGUID: TGUID): IInterface;  
var
  Registration: TServiceRegistration;
  Key: string;
begin
  EnterCriticalSection(FLock);
  try
    Key := GUIDToString(AGUID);

    if not FServices.TryGetValue(Key, Registration) then
      raise Exception.CreateFmt('Service non enregistré: %s', [Key]);

    Result := Registration.CreateInstance;
    WriteLn(Format('[Container] Service résolu: %s', [Key]));
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TDIContainer.TryResolve(const AGUID: TGUID; out AService: IInterface): Boolean;  
var
  Registration: TServiceRegistration;
  Key: string;
begin
  EnterCriticalSection(FLock);
  try
    Key := GUIDToString(AGUID);

    Result := FServices.TryGetValue(Key, Registration);
    if Result then
      AService := Registration.CreateInstance;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TDIContainer.IsRegistered(const AGUID: TGUID): Boolean;  
var
  Key: string;
begin
  EnterCriticalSection(FLock);
  try
    Key := GUIDToString(AGUID);
    Result := FServices.ContainsKey(Key);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

end.
```

## Interfaces pour l'exemple

```pascal
unit Domain.Interfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Interface Repository
  IRepository = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure Sauvegarder(const AData: string);
    function Charger(const AId: string): string;
  end;

  // Interface Logger
  ILogger = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']
    procedure Log(const AMessage: string);
    procedure LogError(const AMessage: string);
  end;

  // Interface Notification
  INotificationService = interface
    ['{C3D4E5F6-A7B8-9012-CDEF-123456789012}']
    procedure Envoyer(const ADestinataire, AMessage: string);
  end;

  // Interface Service métier
  ICommandeService = interface
    ['{D4E5F6A7-B8C9-0123-DEF1-234567890123}']
    procedure CreerCommande(const AClientId: string);
    procedure ValiderCommande(const ACommandeId: string);
  end;

implementation

end.
```

## Implémentations concrètes

```pascal
unit Infrastructure.Implementations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Domain.Interfaces;

type
  // Repository en mémoire
  TRepositoryMemoire = class(TInterfacedObject, IRepository)
  private
    FData: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Sauvegarder(const AData: string);
    function Charger(const AId: string): string;
  end;

  // Logger console
  TConsoleLogger = class(TInterfacedObject, ILogger)
  public
    procedure Log(const AMessage: string);
    procedure LogError(const AMessage: string);
  end;

  // Service de notification
  TEmailNotificationService = class(TInterfacedObject, INotificationService)
  public
    procedure Envoyer(const ADestinataire, AMessage: string);
  end;

  // Service métier
  TCommandeService = class(TInterfacedObject, ICommandeService)
  private
    FRepository: IRepository;
    FLogger: ILogger;
    FNotification: INotificationService;
  public
    constructor Create(const ARepository: IRepository;
                      const ALogger: ILogger;
                      const ANotification: INotificationService);

    procedure CreerCommande(const AClientId: string);
    procedure ValiderCommande(const ACommandeId: string);
  end;

implementation

// TRepositoryMemoire

constructor TRepositoryMemoire.Create;  
begin
  inherited Create;
  FData := TStringList.Create;
  WriteLn('[RepositoryMemoire] Instance créée');
end;

destructor TRepositoryMemoire.Destroy;  
begin
  FData.Free;
  WriteLn('[RepositoryMemoire] Instance détruite');
  inherited;
end;

procedure TRepositoryMemoire.Sauvegarder(const AData: string);  
begin
  FData.Add(AData);
  WriteLn('[Repository] Données sauvegardées: ', AData);
end;

function TRepositoryMemoire.Charger(const AId: string): string;  
begin
  if FData.Count > 0 then
    Result := FData[0]
  else
    Result := '';
  WriteLn('[Repository] Données chargées: ', Result);
end;

// TConsoleLogger

procedure TConsoleLogger.Log(const AMessage: string);  
begin
  WriteLn('[INFO] ', AMessage);
end;

procedure TConsoleLogger.LogError(const AMessage: string);  
begin
  WriteLn('[ERROR] ', AMessage);
end;

// TEmailNotificationService

procedure TEmailNotificationService.Envoyer(const ADestinataire, AMessage: string);  
begin
  WriteLn(Format('[Email] Envoi à %s: %s', [ADestinataire, AMessage]));
end;

// TCommandeService

constructor TCommandeService.Create(const ARepository: IRepository;
  const ALogger: ILogger; const ANotification: INotificationService);
begin
  inherited Create;

  FRepository := ARepository;
  FLogger := ALogger;
  FNotification := ANotification;

  WriteLn('[CommandeService] Instance créée avec dépendances injectées');
end;

procedure TCommandeService.CreerCommande(const AClientId: string);  
begin
  FLogger.Log('Création commande pour client: ' + AClientId);

  // Logique métier
  FRepository.Sauvegarder('Commande-' + AClientId);

  FLogger.Log('Commande créée avec succès');
end;

procedure TCommandeService.ValiderCommande(const ACommandeId: string);  
begin
  FLogger.Log('Validation commande: ' + ACommandeId);

  // Logique métier
  FNotification.Envoyer('client@example.com', 'Commande validée: ' + ACommandeId);

  FLogger.Log('Commande validée avec succès');
end;

end.
```

## Configuration du conteneur

```pascal
unit Bootstrap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DI.Container,
  Domain.Interfaces,
  Infrastructure.Implementations;

type
  TBootstrap = class
  private
    FContainer: TDIContainer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ConfigurerServices;

    property Container: TDIContainer read FContainer;
  end;

implementation

constructor TBootstrap.Create;  
begin
  inherited Create;
  FContainer := TDIContainer.Create;
end;

destructor TBootstrap.Destroy;  
begin
  FContainer.Free;
  inherited;
end;

procedure TBootstrap.ConfigurerServices;  
begin
  WriteLn('=== Configuration du conteneur IoC ===');
  WriteLn;

  // Enregistrer les services d'infrastructure (Singletons)
  FContainer.RegisterSingleton(
    IRepository,
    function: IInterface
    begin
      Result := TRepositoryMemoire.Create;
    end
  );

  FContainer.RegisterSingleton(
    ILogger,
    function: IInterface
    begin
      Result := TConsoleLogger.Create;
    end
  );

  FContainer.RegisterSingleton(
    INotificationService,
    function: IInterface
    begin
      Result := TEmailNotificationService.Create;
    end
  );

  // Enregistrer les services métier (Transient)
  FContainer.RegisterTransient(
    ICommandeService,
    function: IInterface
    var
      Repository: IRepository;
      Logger: ILogger;
      Notification: INotificationService;
    begin
      // Résolution automatique des dépendances
      Repository := FContainer.Resolve(IRepository) as IRepository;
      Logger := FContainer.Resolve(ILogger) as ILogger;
      Notification := FContainer.Resolve(INotificationService) as INotificationService;

      Result := TCommandeService.Create(Repository, Logger, Notification);
    end
  );

  WriteLn('Configuration terminée');
  WriteLn;
end;

end.
```

## Programme principal

```pascal
program DIDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Bootstrap,
  Domain.Interfaces;

var
  Bootstrap: TBootstrap;
  Service1, Service2: ICommandeService;
begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Démonstration Dependency Injection       ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  try
    // Bootstrap de l'application
    Bootstrap := TBootstrap.Create;
    try
      Bootstrap.ConfigurerServices;

      WriteLn('--- Utilisation du conteneur ---');
      WriteLn;

      // Résoudre le service (première instance)
      WriteLn('1. Résolution du premier service...');
      Service1 := Bootstrap.Container.Resolve(ICommandeService) as ICommandeService;
      Service1.CreerCommande('client-123');
      WriteLn;

      // Résoudre le service (deuxième instance - Transient)
      WriteLn('2. Résolution du deuxième service...');
      Service2 := Bootstrap.Container.Resolve(ICommandeService) as ICommandeService;
      Service2.ValiderCommande('commande-456');
      WriteLn;

      WriteLn('3. Les services utilisent les mêmes singletons (Repository, Logger)');
      WriteLn('   mais sont des instances différentes (Transient)');
      WriteLn;

    finally
      Bootstrap.Free;
    end;

    WriteLn('═══════════════════════════════════════════');
    WriteLn('  Démonstration terminée                   ');
    WriteLn('═══════════════════════════════════════════');

  except
    on E: Exception do
      WriteLn('ERREUR: ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Auto-wiring (Câblage automatique)

Le **auto-wiring** résout automatiquement les dépendances en analysant les constructeurs.

```pascal
unit DI.AutoWiring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, RTTI,
  DI.Container;

type
  TAutoWiringContainer = class(TDIContainer)
  public
    // Enregistrement avec auto-wiring
    procedure RegisterType<TInterface: IInterface; TImplementation: class>();

    // Résolution avec auto-wiring
    function AutoResolve(const AGUID: TGUID): IInterface;
  end;

implementation

procedure TAutoWiringContainer.RegisterType<TInterface, TImplementation>();  
begin
  RegisterTransient(
    GetTypeData(TypeInfo(TInterface))^.Guid,
    function: IInterface
    begin
      // Créer l'instance avec résolution automatique des dépendances
      Result := AutoResolve(GetTypeData(TypeInfo(TInterface))^.Guid);
    end
  );
end;

function TAutoWiringContainer.AutoResolve(const AGUID: TGUID): IInterface;  
begin
  // Implémentation simplifiée
  // Dans un container réel, il faudrait :
  // 1. Trouver le type d'implémentation
  // 2. Analyser son constructeur avec RTTI
  // 3. Résoudre toutes les dépendances
  // 4. Invoquer le constructeur dynamiquement

  Result := Resolve(AGUID);
end;

end.
```

## Scopes (Portées)

Les **scopes** permettent de partager des instances dans un contexte limité (requête HTTP, transaction, etc.).

```pascal
unit DI.Scopes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  DI.Container;

type
  // Scope de cycle de vie
  TLifetimeScope = class
  private
    FParent: TLifetimeScope;
    FInstances: TDictionary<string, IInterface>;
    FContainer: TDIContainer;
  public
    constructor Create(AContainer: TDIContainer; AParent: TLifetimeScope);
    destructor Destroy; override;

    function Resolve(const AGUID: TGUID): IInterface;

    // Créer un scope enfant
    function BeginScope: TLifetimeScope;
  end;

implementation

constructor TLifetimeScope.Create(AContainer: TDIContainer; AParent: TLifetimeScope);  
begin
  inherited Create;
  FContainer := AContainer;
  FParent := AParent;
  FInstances := TDictionary<string, IInterface>.Create;
end;

destructor TLifetimeScope.Destroy;  
begin
  FInstances.Free;
  inherited;
end;

function TLifetimeScope.Resolve(const AGUID: TGUID): IInterface;  
var
  Key: string;
begin
  Key := GUIDToString(AGUID);

  // Chercher dans ce scope
  if FInstances.TryGetValue(Key, Result) then
    Exit;

  // Créer une nouvelle instance
  Result := FContainer.Resolve(AGUID);

  // Stocker dans ce scope
  FInstances.Add(Key, Result);
end;

function TLifetimeScope.BeginScope: TLifetimeScope;  
begin
  Result := TLifetimeScope.Create(FContainer, Self);
end;

end.
```

**Utilisation des scopes :**

```pascal
var
  RootScope, RequestScope: TLifetimeScope;
  Service1, Service2: ICommandeService;
begin
  RootScope := TLifetimeScope.Create(Container, nil);
  try
    // Simuler une requête HTTP
    RequestScope := RootScope.BeginScope;
    try
      // Dans ce scope, les instances sont partagées
      Service1 := RequestScope.Resolve(ICommandeService) as ICommandeService;
      Service2 := RequestScope.Resolve(ICommandeService) as ICommandeService;

      // Service1 et Service2 sont la même instance dans ce scope
    finally
      RequestScope.Free;
    end;

    // Nouvelle requête = nouveau scope
    RequestScope := RootScope.BeginScope;
    try
      // Nouvelles instances
    finally
      RequestScope.Free;
    end;
  finally
    RootScope.Free;
  end;
end;
```

## Décorateurs avec DI

Les **décorateurs** ajoutent des fonctionnalités sans modifier le code original.

```pascal
unit DI.Decorators;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Domain.Interfaces;

type
  // Décorateur : Logger automatique
  TLoggingRepositoryDecorator = class(TInterfacedObject, IRepository)
  private
    FInner: IRepository;
    FLogger: ILogger;
  public
    constructor Create(const AInner: IRepository; const ALogger: ILogger);

    procedure Sauvegarder(const AData: string);
    function Charger(const AId: string): string;
  end;

  // Décorateur : Cache
  TCachingRepositoryDecorator = class(TInterfacedObject, IRepository)
  private
    FInner: IRepository;
    FCache: TStringList;
  public
    constructor Create(const AInner: IRepository);
    destructor Destroy; override;

    procedure Sauvegarder(const AData: string);
    function Charger(const AId: string): string;
  end;

implementation

// TLoggingRepositoryDecorator

constructor TLoggingRepositoryDecorator.Create(const AInner: IRepository;
  const ALogger: ILogger);
begin
  inherited Create;
  FInner := AInner;
  FLogger := ALogger;
end;

procedure TLoggingRepositoryDecorator.Sauvegarder(const AData: string);  
begin
  FLogger.Log('Avant Sauvegarder: ' + AData);
  FInner.Sauvegarder(AData);
  FLogger.Log('Après Sauvegarder');
end;

function TLoggingRepositoryDecorator.Charger(const AId: string): string;  
begin
  FLogger.Log('Avant Charger: ' + AId);
  Result := FInner.Charger(AId);
  FLogger.Log('Après Charger: ' + Result);
end;

// TCachingRepositoryDecorator

constructor TCachingRepositoryDecorator.Create(const AInner: IRepository);  
begin
  inherited Create;
  FInner := AInner;
  FCache := TStringList.Create;
end;

destructor TCachingRepositoryDecorator.Destroy;  
begin
  FCache.Free;
  inherited;
end;

procedure TCachingRepositoryDecorator.Sauvegarder(const AData: string);  
begin
  FInner.Sauvegarder(AData);
  FCache.Add(AData);  // Mettre en cache
end;

function TCachingRepositoryDecorator.Charger(const AId: string): string;  
begin
  // Chercher dans le cache d'abord
  if FCache.Count > 0 then
  begin
    WriteLn('[Cache] Hit !');
    Result := FCache[0];
  end
  else
  begin
    WriteLn('[Cache] Miss');
    Result := FInner.Charger(AId);
    FCache.Add(Result);
  end;
end;

end.
```

**Configuration avec décorateurs :**

```pascal
// Enregistrement avec décorateurs empilés
Container.RegisterSingleton(
  IRepository,
  function: IInterface
  var
    Inner: IRepository;
    Logger: ILogger;
  begin
    // Repository de base
    Inner := TRepositoryMemoire.Create;

    // Ajouter le cache
    Inner := TCachingRepositoryDecorator.Create(Inner);

    // Ajouter le logging
    Logger := Container.Resolve(ILogger) as ILogger;
    Result := TLoggingRepositoryDecorator.Create(Inner, Logger);
  end
);
```

## Container avancé avec nommage

Enregistrer plusieurs implémentations de la même interface.

```pascal
unit DI.NamedContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  DI.Container;

type
  TNamedDIContainer = class(TDIContainer)
  private
    FNamedServices: TDictionary<string, TServiceRegistration>;
  public
    constructor Create;
    destructor Destroy; override;

    // Enregistrement avec nom
    procedure RegisterNamed(const AName: string; const AGUID: TGUID;
                           AFactory: TFactoryFunc; ALifetime: TLifetimeScope);

    // Résolution par nom
    function ResolveNamed(const AName: string): IInterface;
  end;

implementation

constructor TNamedDIContainer.Create;  
begin
  inherited Create;
  FNamedServices := TDictionary<string, TServiceRegistration>.Create([doOwnsValues]);
end;

destructor TNamedDIContainer.Destroy;  
begin
  FNamedServices.Free;
  inherited;
end;

procedure TNamedDIContainer.RegisterNamed(const AName: string;
  const AGUID: TGUID; AFactory: TFactoryFunc; ALifetime: TLifetimeScope);
var
  Registration: TServiceRegistration;
begin
  Registration := TServiceRegistration.Create(AGUID, AFactory, ALifetime);
  FNamedServices.AddOrSetValue(AName, Registration);

  WriteLn(Format('[Container] Service nommé enregistré: %s', [AName]));
end;

function TNamedDIContainer.ResolveNamed(const AName: string): IInterface;  
var
  Registration: TServiceRegistration;
begin
  if not FNamedServices.TryGetValue(AName, Registration) then
    raise Exception.CreateFmt('Service nommé "%s" non enregistré', [AName]);

  Result := Registration.CreateInstance;
  WriteLn(Format('[Container] Service nommé résolu: %s', [AName]));
end;

end.
```

**Utilisation :**

```pascal
var
  Container: TNamedDIContainer;
  RepoSQL, RepoMemory: IRepository;
begin
  Container := TNamedDIContainer.Create;
  try
    // Enregistrer plusieurs implémentations
    Container.RegisterNamed(
      'sql',
      IRepository,
      function: IInterface
      begin
        Result := TRepositorySQL.Create(Connection);
      end,
      lsSingleton
    );

    Container.RegisterNamed(
      'memory',
      IRepository,
      function: IInterface
      begin
        Result := TRepositoryMemoire.Create;
      end,
      lsSingleton
    );

    // Résoudre par nom
    RepoSQL := Container.ResolveNamed('sql') as IRepository;
    RepoMemory := Container.ResolveNamed('memory') as IRepository;

  finally
    Container.Free;
  end;
end;
```

## Validation des dépendances

Vérifier que toutes les dépendances peuvent être résolues au démarrage.

```pascal
unit DI.Validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DI.Container;

type
  TContainerValidator = class
  private
    FContainer: TDIContainer;
    FErrors: TStringList;
  public
    constructor Create(AContainer: TDIContainer);
    destructor Destroy; override;

    function Validate: Boolean;
    function GetErrors: TStringDynArray { de l'unité Types };
  end;

implementation

constructor TContainerValidator.Create(AContainer: TDIContainer);  
begin
  inherited Create;
  FContainer := AContainer;
  FErrors := TStringList.Create;
end;

destructor TContainerValidator.Destroy;  
begin
  FErrors.Free;
  inherited;
end;

function TContainerValidator.Validate: Boolean;  
var
  TestService: IInterface;
begin
  FErrors.Clear;
  Result := True;

  // Tenter de résoudre tous les services enregistrés
  // (Implémentation simplifiée)

  try
    // Tester IRepository
    if FContainer.IsRegistered(IRepository) then
    begin
      TestService := FContainer.Resolve(IRepository);
      WriteLn('[Validation] IRepository: OK');
    end;

    // Tester ILogger
    if FContainer.IsRegistered(ILogger) then
    begin
      TestService := FContainer.Resolve(ILogger);
      WriteLn('[Validation] ILogger: OK');
    end;

  except
    on E: Exception do
    begin
      FErrors.Add(E.Message);
      Result := False;
    end;
  end;
end;

function TContainerValidator.GetErrors: TStringDynArray { de l'unité Types };  
var
  i: Integer;
begin
  SetLength(Result, FErrors.Count);
  for i := 0 to FErrors.Count - 1 do
    Result[i] := FErrors[i];
end;

end.
```

## Intercepteurs (AOP - Aspect Oriented Programming)

Les **intercepteurs** permettent d'ajouter des comportements transversaux (logging, transactions, cache, etc.).

```pascal
unit DI.Interceptors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Domain.Interfaces;

type
  // Interface d'interception
  IInterceptor = interface
    ['{E5F6A7B8-C9D0-1234-E5F6-A7B8C9D0E1F2}']
    procedure BeforeCall(const AMethodName: string; const AParams: array of const);
    procedure AfterCall(const AMethodName: string; const AResult: Variant);
    procedure OnError(const AMethodName: string; const AException: Exception);
  end;

  // Intercepteur de logging
  TLoggingInterceptor = class(TInterfacedObject, IInterceptor)
  private
    FLogger: ILogger;
  public
    constructor Create(const ALogger: ILogger);

    procedure BeforeCall(const AMethodName: string; const AParams: array of const);
    procedure AfterCall(const AMethodName: string; const AResult: Variant);
    procedure OnError(const AMethodName: string; const AException: Exception);
  end;

  // Proxy avec interception
  TInterceptedRepository = class(TInterfacedObject, IRepository)
  private
    FInner: IRepository;
    FInterceptor: IInterceptor;
  public
    constructor Create(const AInner: IRepository; const AInterceptor: IInterceptor);

    procedure Sauvegarder(const AData: string);
    function Charger(const AId: string): string;
  end;

implementation

uses
  Variants;

// TLoggingInterceptor

constructor TLoggingInterceptor.Create(const ALogger: ILogger);  
begin
  inherited Create;
  FLogger := ALogger;
end;

procedure TLoggingInterceptor.BeforeCall(const AMethodName: string;
  const AParams: array of const);
begin
  FLogger.Log(Format('[Interceptor] AVANT %s', [AMethodName]));
end;

procedure TLoggingInterceptor.AfterCall(const AMethodName: string;
  const AResult: Variant);
begin
  FLogger.Log(Format('[Interceptor] APRÈS %s', [AMethodName]));
end;

procedure TLoggingInterceptor.OnError(const AMethodName: string;
  const AException: Exception);
begin
  FLogger.LogError(Format('[Interceptor] ERREUR dans %s: %s',
    [AMethodName, AException.Message]));
end;

// TInterceptedRepository

constructor TInterceptedRepository.Create(const AInner: IRepository;
  const AInterceptor: IInterceptor);
begin
  inherited Create;
  FInner := AInner;
  FInterceptor := AInterceptor;
end;

procedure TInterceptedRepository.Sauvegarder(const AData: string);  
begin
  FInterceptor.BeforeCall('Sauvegarder', [AData]);
  try
    FInner.Sauvegarder(AData);
    FInterceptor.AfterCall('Sauvegarder', Null);
  except
    on E: Exception do
    begin
      FInterceptor.OnError('Sauvegarder', E);
      raise;
    end;
  end;
end;

function TInterceptedRepository.Charger(const AId: string): string;  
begin
  FInterceptor.BeforeCall('Charger', [AId]);
  try
    Result := FInner.Charger(AId);
    FInterceptor.AfterCall('Charger', Result);
  except
    on E: Exception do
    begin
      FInterceptor.OnError('Charger', E);
      raise;
    end;
  end;
end;

end.
```

## Configuration par fichier

Configurer le conteneur via un fichier JSON.

```pascal
unit DI.Configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser,
  DI.Container;

type
  TContainerConfigurator = class
  private
    FContainer: TDIContainer;

    procedure LoadFromJSON(AJSON: TJSONObject);
  public
    constructor Create(AContainer: TDIContainer);

    procedure LoadFromFile(const AFileName: string);
  end;

implementation

constructor TContainerConfigurator.Create(AContainer: TDIContainer);  
begin
  inherited Create;
  FContainer := AContainer;
end;

procedure TContainerConfigurator.LoadFromJSON(AJSON: TJSONObject);  
var
  Services: TJSONArray;
  i: Integer;
  Service: TJSONObject;
  InterfaceName, ImplementationName, Lifetime: string;
begin
  Services := AJSON.Get('services', TJSONArray(nil));
  if not Assigned(Services) then
    Exit;

  for i := 0 to Services.Count - 1 do
  begin
    Service := Services.Objects[i];

    InterfaceName := Service.Get('interface', '');
    ImplementationName := Service.Get('implementation', '');
    Lifetime := Service.Get('lifetime', 'transient');

    WriteLn(Format('[Config] %s → %s (%s)',
      [InterfaceName, ImplementationName, Lifetime]));

    // Enregistrer dans le conteneur
    // (Nécessiterait une factory registry pour créer les instances par nom)
  end;
end;

procedure TContainerConfigurator.LoadFromFile(const AFileName: string);  
var
  FileContent: string;
  Parser: TJSONParser;
  JSON: TJSONObject;
  FileStream: TFileStream;
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('Fichier de configuration non trouvé: %s', [AFileName]);

  FileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    SetLength(FileContent, FileStream.Size);
    FileStream.Read(FileContent[1], FileStream.Size);

    Parser := TJSONParser.Create(FileContent, [joUTF8]);
    try
      JSON := Parser.Parse as TJSONObject;
      try
        LoadFromJSON(JSON);
      finally
        JSON.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

end.
```

**Fichier de configuration (config.json) :**

```json
{
  "services": [
    {
      "interface": "IRepository",
      "implementation": "TRepositorySQL",
      "lifetime": "singleton"
    },
    {
      "interface": "ILogger",
      "implementation": "TConsoleLogger",
      "lifetime": "singleton"
    },
    {
      "interface": "ICommandeService",
      "implementation": "TCommandeService",
      "lifetime": "transient"
    }
  ]
}
```

## Bonnes pratiques

### 1. Prefer Constructor Injection

```pascal
// ✅ BON : Dépendances dans le constructeur
type
  TService = class
  private
    FRepo: IRepository;
  public
    constructor Create(const ARepo: IRepository);
  end;

// ❌ MAUVAIS : Dépendances en propriété
type
  TService = class
  public
    Repository: IRepository;  // Peut être nil !
  end;
```

### 2. Dépendre des abstractions

```pascal
// ✅ BON : Dépendre d'une interface
constructor TService.Create(const ARepo: IRepository);

// ❌ MAUVAIS : Dépendre d'une implémentation
constructor TService.Create(const ARepo: TRepositorySQL);
```

### 3. Un seul conteneur par application

```pascal
// ✅ BON : Conteneur global
var
  GlobalContainer: TDIContainer;

initialization
  GlobalContainer := TDIContainer.Create;

finalization
  GlobalContainer.Free;

// ❌ MAUVAIS : Créer des conteneurs partout
procedure MaFonction;  
var
  Container: TDIContainer;  // ← Overhead inutile
begin
  Container := TDIContainer.Create;
  // ...
end;
```

### 4. Enregistrer tôt, résoudre tard

```pascal
// ✅ BON : Configuration au démarrage
procedure ConfigurerApplication;  
begin
  Container.RegisterSingleton(ILogger, @CreateLogger);
  Container.RegisterTransient(IService, @CreateService);
  // Toute la configuration ici
end;

// Résolution à l'utilisation
procedure TraiterRequete;  
var
  Service: IService;
begin
  Service := Container.Resolve(IService) as IService;
  Service.Traiter;
end;
```

### 5. Éviter le Service Locator

```pascal
// ❌ ANTI-PATTERN : Service Locator
type
  TMyClass = class
  public
    procedure DoSomething;
  end;

procedure TMyClass.DoSomething;  
var
  Logger: ILogger;
begin
  // Mauvais : dépendance cachée vers le conteneur
  Logger := GlobalContainer.Resolve(ILogger) as ILogger;
  Logger.Log('Something');
end;

// ✅ BON : Dependency Injection
type
  TMyClass = class
  private
    FLogger: ILogger;
  public
    constructor Create(const ALogger: ILogger);
    procedure DoSomething;
  end;

procedure TMyClass.DoSomething;  
begin
  // Bon : dépendance explicite
  FLogger.Log('Something');
end;
```

### 6. Valider la configuration

```pascal
procedure DemarrerApplication;  
var
  Validator: TContainerValidator;
begin
  ConfigurerServices(Container);

  // Valider avant de démarrer
  Validator := TContainerValidator.Create(Container);
  try
    if not Validator.Validate then
    begin
      WriteLn('ERREURS DE CONFIGURATION:');
      for Error in Validator.GetErrors do
        WriteLn('  - ', Error);
      Halt(1);
    end;
  finally
    Validator.Free;
  end;

  // Démarrer l'application
end;
```

## Comparaison avec d'autres patterns

| Pattern | Utilisation | Avantages | Inconvénients |
|---------|-------------|-----------|---------------|
| **New** | `obj := TClass.Create` | Simple | Couplage fort |
| **Factory** | `obj := Factory.Create` | Centralise création | Toujours du couplage |
| **Service Locator** | `obj := Locator.Get<T>` | Flexibilité | Dépendances cachées |
| **DI Container** | Injection automatique | Découplage total | Complexité initiale |

## Tests avec DI

La DI facilite énormément les tests en permettant d'injecter des mocks.

```pascal
unit Tests.ServiceCommande;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Domain.Interfaces;

type
  // Mock pour les tests
  TMockRepository = class(TInterfacedObject, IRepository)
  private
    FSaveCount: Integer;
    FLoadCount: Integer;
  public
    procedure Sauvegarder(const AData: string);
    function Charger(const AId: string): string;

    property SaveCount: Integer read FSaveCount;
    property LoadCount: Integer read FLoadCount;
  end;

  TMockLogger = class(TInterfacedObject, ILogger)
  private
    FMessages: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Log(const AMessage: string);
    procedure LogError(const AMessage: string);

    property Messages: TStringList read FMessages;
  end;

  // Tests
  TTestServiceCommande = class(TTestCase)
  private
    FService: ICommandeService;
    FMockRepo: TMockRepository;
    FMockLogger: TMockLogger;
  protected
    procedure SetUp; override;
  published
    procedure TestCreerCommande;
    procedure TestValiderCommande;
  end;

implementation

uses
  Infrastructure.Implementations;

// TMockRepository

procedure TMockRepository.Sauvegarder(const AData: string);  
begin
  Inc(FSaveCount);
end;

function TMockRepository.Charger(const AId: string): string;  
begin
  Inc(FLoadCount);
  Result := 'mock-data';
end;

// TMockLogger

constructor TMockLogger.Create;  
begin
  inherited Create;
  FMessages := TStringList.Create;
end;

destructor TMockLogger.Destroy;  
begin
  FMessages.Free;
  inherited;
end;

procedure TMockLogger.Log(const AMessage: string);  
begin
  FMessages.Add(AMessage);
end;

procedure TMockLogger.LogError(const AMessage: string);  
begin
  FMessages.Add('[ERROR] ' + AMessage);
end;

// TTestServiceCommande

procedure TTestServiceCommande.SetUp;  
var
  MockNotif: INotificationService;
begin
  // Créer les mocks
  FMockRepo := TMockRepository.Create;
  FMockLogger := TMockLogger.Create;
  MockNotif := TEmailNotificationService.Create;

  // Injecter les mocks
  FService := TCommandeService.Create(
    FMockRepo as IRepository,
    FMockLogger as ILogger,
    MockNotif
  );
end;

procedure TTestServiceCommande.TestCreerCommande;  
begin
  // Agir
  FService.CreerCommande('client-123');

  // Vérifier
  AssertEquals('Repository appelé 1 fois', 1, FMockRepo.SaveCount);
  AssertTrue('Logger utilisé', FMockLogger.Messages.Count > 0);
end;

procedure TTestServiceCommande.TestValiderCommande;  
begin
  // Agir
  FService.ValiderCommande('commande-456');

  // Vérifier
  AssertTrue('Logger utilisé', FMockLogger.Messages.Count > 0);
  AssertTrue('Message de validation',
    Pos('Validation', FMockLogger.Messages.Text) > 0);
end;

initialization
  RegisterTest(TTestServiceCommande);

end.
```

## Frameworks DI pour FreePascal

### Spring4D

**Spring4D** est un framework complet pour FreePascal/Delphi incluant un conteneur IoC puissant.

```pascal
uses
  Spring.Container;

var
  Container: TContainer;
begin
  Container := TContainer.Create;
  try
    // Enregistrement
    Container.RegisterType<IRepository, TRepositorySQL>.AsSingleton;
    Container.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
    Container.RegisterType<ICommandeService, TCommandeService>.AsTransient;

    // Auto-wiring automatique
    Container.Build;

    // Résolution
    Service := Container.Resolve<ICommandeService>;
  finally
    Container.Free;
  end;
end;
```

### mORMot

**mORMot** inclut aussi un système d'injection de dépendances.

```pascal
uses
  mormot.core.interfaces;

var
  Services: TServiceContainer;
begin
  Services := TServiceContainer.Create;
  try
    Services.AddSingleton(TypeInfo(IRepository), TRepositorySQL);
    Services.AddTransient(TypeInfo(ICommandeService), TCommandeService);

    Service := Services.Resolve(TypeInfo(ICommandeService)) as ICommandeService;
  finally
    Services.Free;
  end;
end;
```

## Quand utiliser Dependency Injection ?

### ✅ Utilisez DI quand :

- **Tests automatisés** : Besoin d'injecter des mocks
- **Flexibilité** : Plusieurs implémentations possibles
- **Modularité** : Composants remplaçables
- **Architecture en couches** : Séparation des responsabilités
- **Projet moyen/grand** : Plus de 10 classes
- **Équipe** : Plusieurs développeurs

### ❌ Évitez DI quand :

- **Script simple** : Quelques lignes de code
- **Prototype rapide** : Overhead inutile
- **Une seule implémentation** : Jamais de changement
- **Performances critiques** : Overhead inacceptable
- **Débutant** : Courbe d'apprentissage

## Conclusion

La **Dependency Injection** et les **IoC Containers** sont des outils puissants pour créer des applications modulaires, testables et maintenables.

### Points clés à retenir :

1. **DI = Injecter les dépendances** au lieu de les créer
2. **IoC Container** = Automatise l'injection
3. **Constructor Injection** = Le plus recommandé
4. **Dépendre d'abstractions** = Interfaces, pas implémentations
5. **Testabilité** = Principal avantage

### Bénéfices avec FreePascal :

- **Interfaces natives** : Support parfait
- **Typage fort** : Sécurité au compile-time
- **Performance** : Pas d'overhead runtime significatif
- **Frameworks disponibles** : Spring4D, mORMot
- **Architecture claire** : Code explicite

La DI demande un **investissement initial** en termes de configuration, mais elle paye rapidement en facilitant les tests, la maintenance et l'évolution du code. Elle est particulièrement précieuse dans les projets d'entreprise où la qualité et la maintenabilité sont critiques. 🎯

⏭️ [Plugin architectures portables](/21-architecture-logicielle-avancee/06-plugin-architectures-portables.md)
