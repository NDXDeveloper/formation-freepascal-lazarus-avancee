🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21. Architecture Logicielle Avancée

## Introduction

L'architecture logicielle est l'art de structurer un système pour qu'il soit **maintenable**, **évolutif**, **performant** et **compréhensible**. Dans ce chapitre avancé, nous explorerons les architectures modernes et leur implémentation avec FreePascal.

### Qu'est-ce que l'architecture logicielle ?

L'architecture logicielle définit :
- **La structure globale** du système (comment les composants sont organisés)
- **Les responsabilités** de chaque composant (qui fait quoi)
- **Les interactions** entre composants (comment ils communiquent)
- **Les décisions techniques** majeures (technologies, patterns, contraintes)

**Analogie :** Si votre code est une ville, l'architecture est le plan d'urbanisme qui définit où placer les quartiers résidentiels, commerciaux et industriels, et comment les relier par des routes.

### Pourquoi l'architecture est-elle cruciale ?

**Sans architecture claire :**
```
┌─────────────────────────────────┐
│                                 │
│    "Big Ball of Mud"            │
│    Code spaghetti               │
│    Tout dépend de tout          │
│    Impossible à maintenir       │
│                                 │
└─────────────────────────────────┘
```

**Avec une bonne architecture :**
```
┌──────────┐    ┌──────────┐    ┌──────────┐
│ Module A │───→│ Module B │───→│ Module C │
│ Clair    │    │ Isolé    │    │ Testable │
└──────────┘    └──────────┘    └──────────┘
```

### Les défis de l'architecture logicielle

1. **Complexité croissante** : Les applications modernes sont de plus en plus sophistiquées
2. **Évolution constante** : Les besoins métier changent régulièrement
3. **Scalabilité** : Supporter la croissance du nombre d'utilisateurs
4. **Maintenance** : Faciliter les modifications sans tout casser
5. **Performance** : Répondre rapidement même sous charge
6. **Coût** : Optimiser le temps de développement et les ressources

## Pourquoi FreePascal pour l'architecture avancée ?

### Atouts de FreePascal

**1. Typage fort et explicite**
```pascal
// Le compilateur détecte les erreurs de type
var
  Client: TClient;
  Commande: TCommande;
begin
  // ✓ Correct
  Client := TClient.Create('Jean Dupont');

  // ✗ Erreur de compilation
  Client := TCommande.Create; // Types incompatibles !
end;
```

**2. Interfaces pour l'abstraction**
```pascal
type
  IRepository<T> = interface
    ['{GUID-HERE}']
    procedure Sauvegarder(const AEntite: T);
    function Charger(const AId: string): T;
  end;

// Plusieurs implémentations possibles
type
  TRepositorySQL = class(TInterfacedObject, IRepository<TClient>)
    // Implémentation SQL
  end;

  TRepositoryMongoDB = class(TInterfacedObject, IRepository<TClient>)
    // Implémentation MongoDB
  end;
```

**3. Gestion mémoire maîtrisée**
```pascal
// Pas de garbage collector imprévisible
// Contrôle total sur l'allocation/libération
var
  Liste: TObjectList<TClient>;
begin
  Liste := TObjectList<TClient>.Create(True); // True = possède les objets
  try
    Liste.Add(TClient.Create('Jean'));
    Liste.Add(TClient.Create('Marie'));
    // Traitement...
  finally
    Liste.Free; // Libération déterministe
  end;
end;
```

**4. Performance native**
```pascal
// Compilation en code machine natif
// Pas de JIT, pas de VM
// = Démarrage instantané et performances prévisibles
```

**5. Portabilité Windows/Linux**
```pascal
{$IFDEF WINDOWS}
  // Code spécifique Windows
  UseUnit('Windows');
{$ENDIF}
{$IFDEF UNIX}
  // Code spécifique Unix/Linux
  UseUnit('BaseUnix');
{$ENDIF}
// Code commun portable
```

### Comparaison avec d'autres langages

| Aspect | FreePascal | Java | C# | Python | C++ |
|--------|-----------|------|-------|--------|-----|
| Typage | Fort, statique | Fort, statique | Fort, statique | Faible, dynamique | Fort, statique |
| Performance | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐⭐ |
| Mémoire | Manuelle/ARC | GC | GC | GC | Manuelle |
| Démarrage | Instantané | Lent (JVM) | Rapide (.NET) | Rapide | Instantané |
| Portabilité | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| Courbe d'apprentissage | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐ |

## Les grands principes architecturaux

### 1. Separation of Concerns (Séparation des préoccupations)

**Principe :** Chaque partie du code doit avoir une seule responsabilité claire.

```pascal
// ❌ Mauvais : tout mélangé
procedure TraiterCommande;
begin
  // Validation
  if Montant <= 0 then Exit;

  // Accès base de données
  Query.SQL.Text := 'INSERT INTO commandes...';
  Query.ExecSQL;

  // Envoi email
  SMTP.Send('Commande confirmée');

  // Logging
  WriteLn('Commande créée');
end;

// ✅ Bon : responsabilités séparées
type
  TValidateurCommande = class
    function Valider(const ACommande: TCommande): Boolean;
  end;

  TRepositoryCommande = class
    procedure Sauvegarder(const ACommande: TCommande);
  end;

  TServiceNotification = class
    procedure EnvoyerConfirmation(const ACommande: TCommande);
  end;

  TLogger = class
    procedure LoggerCommande(const ACommande: TCommande);
  end;

procedure TraiterCommande(const ACommande: TCommande);
var
  Validateur: TValidateurCommande;
  Repository: TRepositoryCommande;
  Notification: TServiceNotification;
  Logger: TLogger;
begin
  if not Validateur.Valider(ACommande) then Exit;
  Repository.Sauvegarder(ACommande);
  Notification.EnvoyerConfirmation(ACommande);
  Logger.LoggerCommande(ACommande);
end;
```

### 2. High Cohesion, Low Coupling (Forte cohésion, faible couplage)

**Cohésion :** Les éléments d'un module doivent être fortement liés entre eux.  
**Couplage :** Les modules doivent être le moins dépendants possible les uns des autres.  

```pascal
// ✅ Forte cohésion : tout ce qui concerne les clients ensemble
unit GestionClients;

type
  TClient = class
    // Données client
  end;

  TValidateurClient = class
    // Validation client
  end;

  TRepositoryClient = class
    // Persistance client
  end;

// ✅ Faible couplage : utiliser des interfaces
type
  IRepository<T> = interface
    procedure Sauvegarder(const AEntite: T);
  end;

type
  TServiceCommande = class
  private
    FRepositoryClient: IRepository<TClient>; // Dépend de l'interface, pas de l'implémentation
  public
    constructor Create(const ARepo: IRepository<TClient>);
  end;
```

### 3. DRY (Don't Repeat Yourself)

**Principe :** Ne pas dupliquer le code. Chaque connaissance doit avoir une représentation unique.

```pascal
// ❌ Mauvais : duplication
function CalculerTotalCommande1(ACommande: TCommande): Currency;
begin
  Result := ACommande.SousTotal * (1 + ACommande.TauxTVA / 100);
end;

function CalculerTotalCommande2(ACommande: TCommande): Currency;
begin
  Result := ACommande.SousTotal * (1 + ACommande.TauxTVA / 100); // Duplication !
end;

// ✅ Bon : une seule source de vérité
type
  TCommande = class
  private
    FSousTotal: Currency;
    FTauxTVA: Double;
  public
    function CalculerTotal: Currency;
  end;

function TCommande.CalculerTotal: Currency;
begin
  Result := FSousTotal * (1 + FTauxTVA / 100);
end;
```

### 4. SOLID Principles

**S - Single Responsibility Principle**
```pascal
// ❌ Mauvais : trop de responsabilités
type
  TClient = class
    procedure Sauvegarder; // Persistance
    procedure EnvoyerEmail; // Communication
    procedure ValiderDonnees; // Validation
  end;

// ✅ Bon : une seule responsabilité
type
  TClient = class
    // Uniquement les données et logique métier
  end;

  TRepositoryClient = class
    procedure Sauvegarder(const AClient: TClient);
  end;

  TServiceEmail = class
    procedure Envoyer(const AClient: TClient);
  end;
```

**O - Open/Closed Principle**
```pascal
// Ouvert à l'extension, fermé à la modification

type
  ICalculateurPrix = interface
    function Calculer(AMontant: Currency): Currency;
  end;

  // Extension par héritage/implémentation
  TCalculateurPrixStandard = class(TInterfacedObject, ICalculateurPrix)
    function Calculer(AMontant: Currency): Currency;
  end;

  TCalculateurPrixVIP = class(TInterfacedObject, ICalculateurPrix)
    function Calculer(AMontant: Currency): Currency;
  end;
```

**L - Liskov Substitution Principle**
```pascal
// Les sous-types doivent pouvoir remplacer leurs types de base

type
  TOiseau = class
    procedure Voler; virtual; abstract;
  end;

// ❌ Mauvais : un pingouin ne peut pas voler !
type
  TPingouin = class(TOiseau)
    procedure Voler; override;
    begin
      raise Exception.Create('Un pingouin ne vole pas !');
    end;
  end;

// ✅ Bon : meilleure hiérarchie
type
  TOiseau = class
  end;

  TOiseauVolant = class(TOiseau)
    procedure Voler; virtual; abstract;
  end;

  TAigle = class(TOiseauVolant)
    procedure Voler; override;
  end;

  TPingouin = class(TOiseau)
    procedure Nager;
  end;
```

**I - Interface Segregation Principle**
```pascal
// ❌ Mauvais : interface trop large
type
  IWorker = interface
    procedure Travailler;
    procedure Manger;
    procedure Dormir;
  end;

// ✅ Bon : interfaces ségrégées
type
  IWorker = interface
    procedure Travailler;
  end;

  IEater = interface
    procedure Manger;
  end;

  ISleeper = interface
    procedure Dormir;
  end;

// Une classe peut implémenter plusieurs interfaces
type
  THumain = class(TInterfacedObject, IWorker, IEater, ISleeper)
    // Implémente les trois
  end;

  TRobot = class(TInterfacedObject, IWorker)
    // Implémente seulement IWorker
  end;
```

**D - Dependency Inversion Principle**
```pascal
// Dépendre des abstractions, pas des implémentations

// ❌ Mauvais : dépendance directe
type
  TServiceCommande = class
  private
    FRepositorySQL: TRepositoryCommandeSQL; // Dépendance concrète
  end;

// ✅ Bon : dépendance inversée
type
  IRepositoryCommande = interface
    procedure Sauvegarder(const ACommande: TCommande);
  end;

  TServiceCommande = class
  private
    FRepository: IRepositoryCommande; // Dépendance abstraite
  public
    constructor Create(const ARepo: IRepositoryCommande);
  end;

// On peut maintenant injecter n'importe quelle implémentation
var
  Service: TServiceCommande;
begin
  Service := TServiceCommande.Create(TRepositoryCommandeSQL.Create);
  // ou
  Service := TServiceCommande.Create(TRepositoryCommandeMongoDB.Create);
end;
```

## Les architectures en couches

### Architecture traditionnelle 3-tiers

```
┌─────────────────────────────────┐
│     Présentation (UI)           │ ← Interface utilisateur
├─────────────────────────────────┤
│     Logique Métier (Business)   │ ← Règles métier
├─────────────────────────────────┤
│     Accès aux Données (Data)    │ ← Base de données
└─────────────────────────────────┘
```

**Exemple FreePascal :**

```pascal
// Couche Présentation
unit UI.FormCommande;

type
  TFormCommande = class(TForm)
  private
    FServiceCommande: TServiceCommande;
  public
    procedure BoutonCreerClick(Sender: TObject);
  end;

procedure TFormCommande.BoutonCreerClick(Sender: TObject);
var
  Commande: TCommande;
begin
  Commande := FServiceCommande.CreerCommande(
    EditClient.Text,
    GetProduitsSelectionnes
  );
  ShowMessage('Commande créée: ' + Commande.Numero);
end;

// Couche Logique Métier
unit Business.ServiceCommande;

type
  TServiceCommande = class
  private
    FRepository: IRepositoryCommande;
    FValidateur: TValidateurCommande;
  public
    function CreerCommande(const AClientId: string;
                          AProduits: TArray<TProduit>): TCommande;
  end;

function TServiceCommande.CreerCommande(const AClientId: string;
  AProduits: TArray<TProduit>): TCommande;
begin
  Result := TCommande.Create(AClientId);
  // Ajouter produits, valider, etc.

  if not FValidateur.Valider(Result) then
    raise EValidationException.Create('Commande invalide');

  FRepository.Sauvegarder(Result);
end;

// Couche Accès aux Données
unit Data.RepositoryCommande;

type
  TRepositoryCommande = class(TInterfacedObject, IRepositoryCommande)
  private
    FConnection: TSQLConnection;
  public
    procedure Sauvegarder(const ACommande: TCommande);
  end;

procedure TRepositoryCommande.Sauvegarder(const ACommande: TCommande);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := 'INSERT INTO commandes (id, client_id, ...) VALUES (...)';
    // Paramètres...
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;
```

### Architecture hexagonale (Ports et Adaptateurs)

```
         ┌─────────────────────┐
         │   Adaptateur UI     │
         │   (Web, Desktop)    │
         └──────────┬──────────┘
                    │
         ┌──────────▼──────────┐
         │      Port API       │
         ├─────────────────────┤
         │                     │
         │   Domaine Métier    │ ← Cœur isolé
         │   (Business Logic)  │
         │                     │
         ├─────────────────────┤
         │  Port Persistance   │
         └──────────┬──────────┘
                    │
         ┌──────────▼──────────┐
         │  Adaptateur DB      │
         │  (SQL, NoSQL)       │
         └─────────────────────┘
```

**Avantages :**
- Le domaine métier est complètement isolé
- Facile de changer de base de données
- Facile de changer d'interface utilisateur
- Tests unitaires simplifiés

### Clean Architecture

```
┌───────────────────────────────────────┐
│  Frameworks & Drivers (UI, DB, Web)  │ ← Externe
├───────────────────────────────────────┤
│  Interface Adapters (Controllers)     │ ← Adaptateurs
├───────────────────────────────────────┤
│  Application Business Rules (Use Cases)│ ← Cas d'usage
├───────────────────────────────────────┤
│  Enterprise Business Rules (Entities) │ ← Domaine
└───────────────────────────────────────┘
```

**Règle de dépendance :** Les couches internes ne connaissent pas les couches externes.

## Vue d'ensemble des architectures du chapitre

Dans ce chapitre, nous couvrirons les architectures suivantes :

### 21.1 Domain-Driven Design (DDD)
Modéliser le logiciel selon le domaine métier avec des concepts comme les entités, value objects, agrégats, et le langage ubiquitaire.

### 21.2 Microservices et architectures distribuées
Décomposer l'application en services autonomes qui communiquent via des APIs ou des messages.

### 21.3 Event Sourcing et CQRS
Stocker les changements comme une séquence d'événements et séparer les opérations de lecture et d'écriture.

### 21.4 Hexagonal Architecture
Isoler le domaine métier et le rendre indépendant des frameworks et de l'infrastructure.

### 21.5 Dependency Injection et IoC
Inverser les dépendances pour rendre le code plus testable et modulaire.

### 21.6 Plugin Architecture
Créer des systèmes extensibles où les fonctionnalités peuvent être ajoutées dynamiquement.

### 21.7 Message Bus et Event-Driven Architecture
Communiquer via des événements asynchrones pour découpler les composants.

### 21.8 Saga Pattern
Coordonner des transactions distribuées à travers plusieurs services.

### 21.9 API Gateway Patterns
Implémenter un point d'entrée unique pour gérer le routage, l'authentification et le rate limiting.

### 21.10 Service Mesh et Observabilité
Gérer la communication inter-services, la sécurité et l'observabilité dans les architectures microservices.

### 21.11 Architecture Cloud-Native
Concevoir des applications pour tirer parti des environnements cloud (scalabilité, résilience).

## Choix d'architecture : critères de décision

### Quand utiliser quelle architecture ?

| Critère | Monolithe | Microservices | Serverless |
|---------|-----------|---------------|------------|
| **Taille équipe** | 1-10 | 10+ | Variable |
| **Complexité métier** | Simple-Moyenne | Élevée | Simple |
| **Scalabilité** | Limitée | Excellente | Excellente |
| **Coût infrastructure** | Faible | Élevé | Variable |
| **Temps de développement** | Rapide | Lent | Rapide |
| **Maintenance** | Difficile à terme | Complexe | Simple |
| **Déploiement** | Simple | Complexe | Très simple |

### Questions à se poser

**1. Quelle est la taille de l'équipe ?**
- Petite équipe (< 5) → Monolithe modulaire
- Moyenne équipe (5-20) → Monolithe ou microservices limités
- Grande équipe (20+) → Microservices

**2. Quelle est la complexité métier ?**
- Simple → Architecture 3-tiers
- Complexe avec domaines distincts → DDD + Microservices
- Très complexe → DDD + Event Sourcing + CQRS

**3. Quels sont les besoins de scalabilité ?**
- Croissance modérée → Monolithe scalable
- Croissance importante → Microservices
- Pics imprévisibles → Serverless

**4. Quel est le budget infrastructure ?**
- Limité → Monolithe
- Confortable → Microservices
- À l'usage → Serverless

## Outils et concepts pour ce chapitre

### Patterns de conception utilisés

- **Repository Pattern** : Abstraction de l'accès aux données
- **Factory Pattern** : Création d'objets complexes
- **Strategy Pattern** : Algorithmes interchangeables
- **Observer Pattern** : Notification d'événements
- **Command Pattern** : Encapsulation d'actions
- **Adapter Pattern** : Adaptation d'interfaces
- **Facade Pattern** : Interface simplifiée

### Concepts clés

- **Inversion of Control (IoC)** : Inverser le contrôle de flux
- **Dependency Injection (DI)** : Injecter les dépendances
- **Interface Segregation** : Interfaces spécialisées
- **Bounded Context** : Frontières de contexte (DDD)
- **Aggregate Root** : Racine d'agrégat (DDD)
- **Event Sourcing** : Stockage par événements
- **CQRS** : Séparation commande/requête

## Préparation pour la suite

Pour tirer le meilleur parti de ce chapitre, assurez-vous de maîtriser :

**Prérequis techniques :**
- ✅ Programmation orientée objet en FreePascal
- ✅ Interfaces et polymorphisme
- ✅ Gestion mémoire et pointeurs
- ✅ Génériques et types avancés
- ✅ Accès aux bases de données
- ✅ Programmation réseau de base

**Prérequis conceptuels :**
- ✅ Principes SOLID
- ✅ Patterns de conception de base
- ✅ Architecture 3-tiers
- ✅ Tests unitaires
- ✅ Git et versioning

**Outils recommandés :**
- FreePascal 3.2+ et Lazarus 2.2+
- Docker pour la conteneurisation
- PostgreSQL ou MySQL
- Git pour le versioning
- Un éditeur JSON/YAML

## Méthodologie d'apprentissage

Pour chaque architecture présentée, nous suivrons cette structure :

1. **Concept** : Qu'est-ce que c'est et pourquoi ?
2. **Principes** : Les fondamentaux à comprendre
3. **Implémentation** : Code FreePascal complet
4. **Exemples** : Cas d'usage concrets
5. **Bonnes pratiques** : Ce qu'il faut faire/éviter
6. **Quand l'utiliser** : Critères de décision

## Conseil pour la pratique

**Approche progressive recommandée :**

1. **Commencez simple** : Implémentez une architecture 3-tiers basique
2. **Ajoutez la complexité graduellement** : Introduisez DDD, puis CQRS si nécessaire
3. **Refactorisez** : Améliorez votre code progressivement
4. **Testez** : Écrivez des tests pour valider vos choix
5. **Documentez** : Expliquez vos décisions architecturales
6. **Itérez** : L'architecture évolue avec le projet

**Ne cherchez pas la perfection** : Une architecture "assez bonne" qui répond aux besoins est mieux qu'une architecture "parfaite" qui ne verra jamais le jour.

## Conclusion de l'introduction

L'architecture logicielle n'est pas une science exacte mais un **art pragmatique**. Il n'y a pas de solution universelle, seulement des compromis éclairés basés sur votre contexte spécifique.

**FreePascal vous offre tous les outils** pour implémenter des architectures modernes et robustes. Sa performance native, son typage fort et sa portabilité en font un excellent choix pour les systèmes critiques.

Dans les sections suivantes, nous explorerons en détail chaque pattern architectural, avec des exemples concrets, du code complet et des recommandations pratiques pour vos projets.

**Prêt à plonger dans le monde fascinant de l'architecture logicielle avancée ?** Commençons par le Domain-Driven Design ! 🚀

⏭️ [Domain-Driven Design avec FreePascal](/21-architecture-logicielle-avancee/01-domain-driven-design-freepascal.md)
