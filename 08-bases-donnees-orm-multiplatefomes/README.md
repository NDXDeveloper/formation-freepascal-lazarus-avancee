🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Bases de Données et ORM Multi-plateformes avec FreePascal/Lazarus

## Introduction générale

Le développement d'applications modernes nécessite presque toujours de stocker et gérer des données de manière persistante. Que vous développiez un logiciel de gestion, une application web, ou même un jeu vidéo, vous aurez besoin d'interagir avec une base de données. FreePascal/Lazarus offre un écosystème riche et mature pour travailler avec les bases de données de manière efficace et portable entre Windows et Linux/Ubuntu.

## Qu'est-ce qu'une base de données ?

### Définition simple

Une base de données est comme un classeur numérique ultra-organisé qui stocke vos informations de manière structurée. Contrairement à des fichiers simples, elle permet :

- De retrouver instantanément une information parmi des millions d'enregistrements
- De maintenir la cohérence des données
- D'accéder aux mêmes données depuis plusieurs applications simultanément
- De sécuriser l'accès aux informations sensibles

### Types de bases de données supportées

FreePascal/Lazarus peut communiquer avec pratiquement toutes les bases de données majeures :

#### Bases de données serveur
- **PostgreSQL** : Base open source très puissante et fiable
- **MySQL/MariaDB** : Très populaire dans le monde web
- **Microsoft SQL Server** : Solution enterprise de Microsoft
- **Oracle** : Pour les applications d'entreprise critiques
- **Firebird** : Descendant open source d'InterBase

#### Bases de données embarquées
- **SQLite** : Base légère qui stocke tout dans un fichier unique
- **Firebird Embedded** : Version embarquée de Firebird
- **DBase** : Format historique mais toujours utilisé

## Pourquoi l'aspect multi-plateforme est crucial

### Le défi de la portabilité

Développer une application qui fonctionne identiquement sur Windows et Linux présente des défis spécifiques pour l'accès aux bases de données :

1. **Bibliothèques clientes différentes**
   - Windows utilise des fichiers .dll
   - Linux utilise des fichiers .so
   - Les chemins et noms peuvent varier

2. **Configuration système**
   - Les emplacements par défaut diffèrent
   - Les mécanismes d'authentification varient
   - Les ports et protocoles peuvent changer

3. **Performances et optimisations**
   - Chaque OS gère différemment les connexions réseau
   - La gestion de la mémoire varie
   - Les systèmes de cache sont distincts

### La solution FreePascal/Lazarus

FreePascal/Lazarus résout ces problèmes en proposant une couche d'abstraction qui masque les différences entre plateformes. Votre code reste identique que vous soyez sur Windows ou Ubuntu, et c'est le framework qui s'occupe des spécificités de chaque système.

## Qu'est-ce qu'un ORM ?

### ORM : Object-Relational Mapping

Un ORM (Mapping Objet-Relationnel en français) est une technique qui permet de manipuler une base de données comme si vous travailliez avec des objets Pascal normaux.

### Sans ORM (approche SQL traditionnelle)

```pascal
// Requête SQL directe
SQLQuery.SQL.Text := 'SELECT id, nom, email FROM clients WHERE age > 18';  
SQLQuery.Open;  
while not SQLQuery.EOF do  
begin
  id := SQLQuery.FieldByName('id').AsInteger;
  nom := SQLQuery.FieldByName('nom').AsString;
  email := SQLQuery.FieldByName('email').AsString;
  // Traitement...
  SQLQuery.Next;
end;
```

### Avec ORM (approche orientée objet)

```pascal
// Manipulation d'objets
ClientsList := TClientRepository.FindAll('age > 18');  
for Client in ClientsList do  
begin
  // Client est un objet avec des propriétés typées
  ShowMessage(Client.Nom + ' - ' + Client.Email);
end;
```

### Avantages de l'ORM

1. **Code plus lisible** : Vous travaillez avec des objets et propriétés, pas du SQL brut
2. **Sécurité accrue** : Protection automatique contre les injections SQL
3. **Portabilité** : Change de base de données sans modifier le code
4. **Productivité** : Moins de code répétitif à écrire
5. **Maintenabilité** : Modifications centralisées du modèle de données

### Inconvénients à considérer

1. **Performance** : Peut être moins optimal que du SQL optimisé à la main
2. **Courbe d'apprentissage** : Nécessite de maîtriser le framework ORM
3. **Flexibilité** : Requêtes très complexes parfois difficiles à exprimer

## L'écosystème FreePascal/Lazarus pour les bases de données

### Composants natifs

FreePascal/Lazarus fournit plusieurs systèmes intégrés :

1. **SQLdb** : Le framework principal pour l'accès SQL
   - Intégré nativement dans Lazarus
   - Support de multiples bases de données
   - Composants visuels pour le RAD

2. **ZEOS DBO** : Alternative puissante et mature
   - Support étendu de bases de données
   - Optimisations avancées
   - Grande communauté

3. **IBX for Lazarus** : Spécialisé Firebird/InterBase
   - Performances optimales avec Firebird
   - Fonctionnalités avancées spécifiques

### Frameworks ORM disponibles

1. **tiOPF** (Timeless Object Persistence Framework)
   - ORM complet et mature
   - Architecture en couches
   - Support de multiples bases

2. **mORMot** : Framework SOA et ORM
   - Très haute performance
   - Architecture REST intégrée
   - Fonctionnalités entreprise

3. **Brook Framework** : Pour applications web
   - ORM intégré
   - Orienté services REST
   - Idéal pour APIs

## Architecture en couches

### Organisation recommandée

Une application bien conçue sépare les responsabilités en couches :

```
┌─────────────────────────────┐
│   Interface Utilisateur      │  ← Formulaires, boutons, grilles
├─────────────────────────────┤
│   Logique Métier            │  ← Règles, calculs, validations
├─────────────────────────────┤
│   Couche d'Accès Données    │  ← ORM ou SQLdb
├─────────────────────────────┤
│   Base de Données           │  ← PostgreSQL, MySQL, SQLite...
└─────────────────────────────┘
```

### Avantages de cette architecture

1. **Séparation des préoccupations** : Chaque couche a sa responsabilité
2. **Testabilité** : Test indépendant de chaque couche
3. **Réutilisabilité** : La logique métier peut servir pour différentes interfaces
4. **Maintenance** : Modifications localisées sans impact global

## Choix de la base de données

### Critères de sélection

#### Pour une application desktop mono-utilisateur
- **SQLite** : Simple, aucune installation serveur requise
- **Firebird Embedded** : Plus de fonctionnalités que SQLite

#### Pour une application multi-utilisateurs en réseau local
- **Firebird** : Installation simple, peu de maintenance
- **PostgreSQL** : Robuste et complet
- **MySQL/MariaDB** : Populaire et bien documenté

#### Pour une application web
- **PostgreSQL** : Excellent pour les charges importantes
- **MySQL/MariaDB** : Standard de l'industrie web
- **MongoDB** (via mORMot) : Pour données non structurées

#### Pour l'entreprise
- **Oracle** : Si déjà en place dans l'entreprise
- **SQL Server** : Environnement Microsoft
- **PostgreSQL** : Alternative open source professionnelle

## Patterns et bonnes pratiques

### Repository Pattern

Centralise l'accès aux données dans des classes dédiées :

```pascal
type
  TClientRepository = class
  public
    function FindById(AId: Integer): TClient;
    function FindAll: TClientList;
    function Save(AClient: TClient): Boolean;
    function Delete(AClient: TClient): Boolean;
  end;
```

### Unit of Work

Gère les transactions et les modifications en lot :

```pascal
type
  TUnitOfWork = class
  private
    FNewObjects: TObjectList;
    FDirtyObjects: TObjectList;
    FRemovedObjects: TObjectList;
  public
    procedure RegisterNew(AObject: TObject);
    procedure RegisterDirty(AObject: TObject);
    procedure RegisterRemoved(AObject: TObject);
    procedure Commit;
    procedure Rollback;
  end;
```

### Data Transfer Objects (DTO)

Sépare les objets métier des objets de transfert :

```pascal
type
  // Objet métier avec logique
  TClient = class
  private
    FId: Integer;
    FNom: string;
    FCredit: Currency;
  public
    function PeutCommander: Boolean;
    procedure AjouterCredit(AMontant: Currency);
  end;

  // DTO pour le transfert
  TClientDTO = record
    Id: Integer;
    Nom: string;
    Credit: Currency;
  end;
```

## Gestion des connexions

### Pool de connexions

Pour les applications multi-utilisateurs, un pool évite de créer/détruire constamment des connexions :

```pascal
type
  TConnectionPool = class
  private
    FAvailableConnections: TQueue;
    FUsedConnections: TList;
    FMaxConnections: Integer;
  public
    function GetConnection: TSQLConnection;
    procedure ReleaseConnection(AConnection: TSQLConnection);
  end;
```

### Connexions paresseuses (Lazy Loading)

Ne créer la connexion que quand nécessaire :

```pascal
function TDataModule.GetConnection: TSQLConnection;  
begin
  if not Assigned(FConnection) then
  begin
    FConnection := CreateConnection;
    FConnection.Open;
  end;
  Result := FConnection;
end;
```

## Migration et versionnement de schéma

### Importance du versionnement

Votre base de données évolue avec votre application. Il faut pouvoir :

1. **Tracer les modifications** : Savoir quelle version du schéma est installée
2. **Migrer automatiquement** : Appliquer les changements nécessaires
3. **Revenir en arrière** : Annuler une migration problématique

### Système de migration simple

```pascal
type
  TMigration = class
  public
    Version: Integer;
    Description: string;
    procedure Up; virtual; abstract;    // Applique la migration
    procedure Down; virtual; abstract;  // Annule la migration
  end;

  TMigration_001_CreateClients = class(TMigration)
  public
    procedure Up; override;
    procedure Down; override;
  end;
```

## Performance et optimisation

### Principes clés

1. **N+1 Problem** : Éviter les requêtes multiples
   ```pascal
   // Mauvais : N+1 requêtes
   clients := GetAllClients;
   for client in clients do
     commandes := GetCommandesByClientId(client.Id);

   // Bon : 1 requête avec jointure
   clientsAvecCommandes := GetClientsWithCommandes;
   ```

2. **Lazy vs Eager Loading** : Charger les données au bon moment
3. **Cache** : Mémoriser les données fréquemment accédées
4. **Index** : Optimiser les colonnes utilisées dans WHERE et JOIN
5. **Pagination** : Ne charger que les données visibles

### Monitoring et profiling

- Activer les logs SQL pour voir les requêtes générées
- Mesurer les temps d'exécution
- Identifier les goulots d'étranglement
- Utiliser EXPLAIN pour analyser les plans d'exécution

## Sécurité

### Menaces principales

1. **Injection SQL** : Code malveillant dans les données
2. **Exposition de données** : Informations sensibles visibles
3. **Privilèges excessifs** : Utilisateurs avec trop de droits
4. **Connexions non sécurisées** : Données transmises en clair

### Bonnes pratiques de sécurité

1. **Toujours utiliser des requêtes paramétrées**
2. **Chiffrer les connexions** (SSL/TLS)
3. **Principe du moindre privilège** pour les comptes DB
4. **Hasher les mots de passe** (jamais en clair)
5. **Valider toutes les entrées** côté serveur
6. **Auditer les accès** aux données sensibles

## Préparation à l'apprentissage des composants

Avant d'aborder les composants techniques spécifiques comme SQLdb, il est important de comprendre :

1. **Le modèle relationnel** : Tables, colonnes, clés primaires et étrangères
2. **Le langage SQL** : SELECT, INSERT, UPDATE, DELETE au minimum
3. **Les transactions** : BEGIN, COMMIT, ROLLBACK
4. **Les types de données** : Correspondance entre types SQL et Pascal
5. **La normalisation** : Organisation efficace des données

Cette base théorique vous permettra d'aborder sereinement l'utilisation des composants techniques qui vont suivre dans les prochaines sections du cours.

⏭️ [SQLdb : architecture et composants](/08-bases-donnees-orm-multiplatefomes/01-sqldb-architecture-composants.md)
