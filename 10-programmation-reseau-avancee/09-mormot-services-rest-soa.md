🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.9 mORMot pour services REST/SOA

## Introduction à mORMot

### Qu'est-ce que mORMot ?

mORMot (Synopse mORMot) est un framework Open Source complet et puissant pour FreePascal et Delphi. C'est l'un des frameworks les plus performants pour créer des services web et des applications distribuées.

**mORMot signifie :**
- **m** : Modular (modulaire)
- **ORM** : Object-Relational Mapping (correspondance objet-relationnel)
- **ot** : Object Tree (arbre d'objets)

**Analogie simple :** Si vous construisez une maison, mORMot est comme une trousse complète d'outils professionnels plutôt que des outils basiques. Vous avez tout ce qu'il faut pour construire rapidement et solidement.

### Pourquoi utiliser mORMot ?

mORMot excelle dans plusieurs domaines :

1. **Performance exceptionnelle** - Parmi les serveurs HTTP les plus rapides
2. **Tout-en-un** - ORM, REST, SOA, authentification, JSON, etc.
3. **Multi-plateforme** - Windows, Linux, BSD, Android
4. **Documentation complète** - Plus de 1000 pages de documentation
5. **Communauté active** - Support et mises à jour régulières

### Comparaison avec d'autres solutions

| Critère | mORMot | fpWeb | Node.js | Spring Boot |
|---------|--------|-------|---------|-------------|
| Performance | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| Fonctionnalités | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| Simplicité | ⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| Documentation | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |

## Concepts fondamentaux

### REST (Representational State Transfer)

REST est un style d'architecture pour les services web basé sur HTTP.

**Principes de base :**

1. **Ressources** - Tout est une ressource (utilisateur, produit, commande)
2. **URLs** - Chaque ressource a une adresse unique
3. **Méthodes HTTP** - GET (lire), POST (créer), PUT (modifier), DELETE (supprimer)
4. **Sans état** - Chaque requête est indépendante

**Exemple simple :**
```
GET    /api/utilisateurs      -> Liste tous les utilisateurs
GET    /api/utilisateurs/42   -> Récupère l'utilisateur 42
POST   /api/utilisateurs      -> Crée un nouvel utilisateur
PUT    /api/utilisateurs/42   -> Modifie l'utilisateur 42
DELETE /api/utilisateurs/42   -> Supprime l'utilisateur 42
```

### SOA (Service-Oriented Architecture)

SOA est une architecture où les fonctionnalités sont exposées comme des services.

**Différence REST vs SOA :**

- **REST** : Orienté ressources (données)
- **SOA** : Orienté actions (opérations)

**Exemple SOA :**
```
POST /api/calculatrice/additionner     -> Additionner deux nombres
POST /api/email/envoyer                -> Envoyer un email
POST /api/rapport/generer              -> Générer un rapport
```

### ORM (Object-Relational Mapping)

L'ORM permet de manipuler la base de données avec des objets Pascal au lieu d'écrire du SQL.

**Sans ORM (SQL direct) :**
```pascal
Query.SQL.Text := 'SELECT * FROM Clients WHERE ID = 42';
Query.Open;
Nom := Query.FieldByName('Nom').AsString;
```

**Avec ORM (mORMot) :**
```pascal
Client := TOrmClient.Create(Database, 42);
Nom := Client.Nom;
```

## Installation de mORMot

### Installation sur Windows

**Méthode 1 : Téléchargement manuel**

1. Télécharger mORMot depuis GitHub :
   ```
   https://github.com/synopse/mORMot2
   ```

2. Extraire dans un dossier, par exemple :
   ```
   C:\Dev\mORMot2
   ```

3. Dans Lazarus, aller dans **Projet → Options du projet → Chemins du compilateur**

4. Ajouter dans "Autres unités" :
   ```
   C:\Dev\mORMot2\src\core
   C:\Dev\mORMot2\src\db
   C:\Dev\mORMot2\src\rest
   C:\Dev\mORMot2\src\net
   ```

**Méthode 2 : Via Git**

```batch
cd C:\Dev
git clone https://github.com/synopse/mORMot2.git
```

### Installation sur Ubuntu/Linux

**Via Git :**

```bash
cd ~/Dev
git clone https://github.com/synopse/mORMot2.git
```

**Configuration dans Lazarus :**

Dans **Outils → Options → Chemins du compilateur**, ajouter :
```
~/Dev/mORMot2/src/core
~/Dev/mORMot2/src/db
~/Dev/mORMot2/src/rest
~/Dev/mORMot2/src/net
```

### Vérification de l'installation

Créez un petit programme de test :

```pascal
program TestmORMot;

{$mode objfpc}{$H+}

uses
  mormot.core.base,
  mormot.core.json;

var
  JSON: RawUtf8;
begin
  JSON := ObjectToJson(TObject.Create);
  WriteLn('mORMot fonctionne ! JSON : ', JSON);
end.
```

Si le programme compile, mORMot est correctement installé.

## Premier serveur REST simple

### Créer un modèle de données

Commençons par définir nos objets métier (entités) :

```pascal
unit ModelUtilisateurs;

{$mode objfpc}{$H+}

interface

uses
  mormot.orm.core;

type
  // Définition d'un utilisateur
  TOrmUtilisateur = class(TOrm)
  private
    FNom: RawUtf8;
    FEmail: RawUtf8;
    FAge: Integer;
    FDateInscription: TDateTime;
  published
    // Les propriétés "published" sont automatiquement persistées
    property Nom: RawUtf8 read FNom write FNom;
    property Email: RawUtf8 read FEmail write FEmail;
    property Age: Integer read FAge write FAge;
    property DateInscription: TDateTime read FDateInscription write FDateInscription;
  end;

implementation

end.
```

**Explication :**

- `TOrm` : Classe de base pour tous les objets persistants dans mORMot
- `RawUtf8` : Type optimisé de mORMot pour les chaînes UTF-8
- `published` : Les propriétés marquées "published" sont automatiquement sauvegardées en base

### Créer le serveur REST

```pascal
program ServeurRestSimple;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  mormot.core.base,
  mormot.core.json,
  mormot.core.log,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.rest.http.server,
  mormot.db.raw.sqlite3,
  ModelUtilisateurs;

type
  // Notre serveur REST
  TMonServeurRest = class
  private
    FServeur: TRestHttpServer;
    FModel: TOrmModel;
    FRest: TRestServerDB;
  public
    constructor Create(const Port: String);
    destructor Destroy; override;
  end;

constructor TMonServeurRest.Create(const Port: String);
begin
  inherited Create;

  // 1. Créer le modèle de données
  FModel := TOrmModel.Create([TOrmUtilisateur], 'root');

  // 2. Créer le serveur REST avec base SQLite
  FRest := TRestServerDB.Create(FModel, 'utilisateurs.db');
  FRest.CreateMissingTables; // Créer les tables si elles n'existent pas

  // 3. Créer le serveur HTTP
  FServeur := TRestHttpServer.Create(Port, [FRest], '+', HTTP_DEFAULT_MODE);

  WriteLn('Serveur démarré sur le port ', Port);
  WriteLn('URL : http://localhost:', Port, '/root');
  WriteLn('Appuyez sur Entrée pour arrêter...');
end;

destructor TMonServeurRest.Destroy;
begin
  FServeur.Free;
  FRest.Free;
  FModel.Free;
  inherited Destroy;
end;

var
  Serveur: TMonServeurRest;
begin
  Serveur := TMonServeurRest.Create('8080');
  try
    ReadLn;
  finally
    Serveur.Free;
  end;
end.
```

**Explication détaillée :**

1. **TOrmModel** : Définit quelles classes font partie de notre modèle
2. **TRestServerDB** : Serveur REST avec base de données SQLite intégrée
3. **CreateMissingTables** : Crée automatiquement les tables en base
4. **TRestHttpServer** : Serveur HTTP qui expose notre REST

### Tester le serveur

Une fois le serveur démarré, vous pouvez tester avec :

**1. Navigateur web**

```
http://localhost:8080/root/Utilisateur
```

**2. Curl (ligne de commande)**

```bash
# Créer un utilisateur
curl -X POST http://localhost:8080/root/Utilisateur \
  -H "Content-Type: application/json" \
  -d '{"Nom":"Jean Dupont","Email":"jean@example.com","Age":30}'

# Lire tous les utilisateurs
curl http://localhost:8080/root/Utilisateur

# Lire un utilisateur spécifique (ID 1)
curl http://localhost:8080/root/Utilisateur/1

# Modifier un utilisateur
curl -X PUT http://localhost:8080/root/Utilisateur/1 \
  -H "Content-Type: application/json" \
  -d '{"Nom":"Jean Martin","Email":"jean@example.com","Age":31}'

# Supprimer un utilisateur
curl -X DELETE http://localhost:8080/root/Utilisateur/1
```

**3. Avec un client mORMot**

```pascal
program ClientRest;

{$mode objfpc}{$H+}

uses
  SysUtils,
  mormot.core.base,
  mormot.core.json,
  mormot.orm.core,
  mormot.rest.http.client,
  ModelUtilisateurs;

var
  Model: TOrmModel;
  Client: TRestHttpClient;
  Utilisateur: TOrmUtilisateur;
  Liste: TObjectList;
  i: Integer;
begin
  // Créer le modèle
  Model := TOrmModel.Create([TOrmUtilisateur], 'root');

  // Se connecter au serveur
  Client := TRestHttpClient.Create('localhost', '8080', Model);
  try
    // Créer un nouvel utilisateur
    Utilisateur := TOrmUtilisateur.Create;
    try
      Utilisateur.Nom := 'Alice Dubois';
      Utilisateur.Email := 'alice@example.com';
      Utilisateur.Age := 28;
      Utilisateur.DateInscription := Now;

      // Ajouter à la base
      if Client.Add(Utilisateur, True) <> 0 then
        WriteLn('Utilisateur créé avec ID : ', Utilisateur.ID)
      else
        WriteLn('Erreur lors de la création');
    finally
      Utilisateur.Free;
    end;

    // Lire tous les utilisateurs
    Liste := Client.RetrieveList(TOrmUtilisateur, '');
    if Liste <> nil then
    try
      WriteLn(Format('Nombre d''utilisateurs : %d', [Liste.Count]));
      for i := 0 to Liste.Count - 1 do
      begin
        Utilisateur := TOrmUtilisateur(Liste[i]);
        WriteLn(Format('  %d. %s (%s) - %d ans',
                [Utilisateur.ID, Utilisateur.Nom, Utilisateur.Email, Utilisateur.Age]));
      end;
    finally
      Liste.Free;
    end;

    // Lire un utilisateur spécifique
    Utilisateur := TOrmUtilisateur.Create(Client, 1);
    try
      if Utilisateur.ID <> 0 then
      begin
        WriteLn('Utilisateur trouvé : ', Utilisateur.Nom);

        // Modifier
        Utilisateur.Age := Utilisateur.Age + 1;
        if Client.Update(Utilisateur) then
          WriteLn('Utilisateur mis à jour');
      end;
    finally
      Utilisateur.Free;
    end;

  finally
    Client.Free;
    Model.Free;
  end;
end.
```

## Services SOA avec interface

### Définir une interface de service

mORMot permet de définir des services avec des interfaces Pascal :

```pascal
unit ServicesCalculatrice;

{$mode objfpc}{$H+}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  // Interface du service
  ICalculatrice = interface(IInvokable)
    ['{12345678-1234-1234-1234-123456789ABC}']

    // Opérations disponibles
    function Additionner(A, B: Double): Double;
    function Soustraire(A, B: Double): Double;
    function Multiplier(A, B: Double): Double;
    function Diviser(A, B: Double): Double;
    function Calculer(const Expression: RawUtf8): Double;
  end;

implementation

end.
```

**Explication :**

- `IInvokable` : Interface de base pour tous les services mORMot
- `['{GUID}']` : Identifiant unique de l'interface (générer avec Ctrl+Shift+G dans Lazarus)
- Les méthodes sont automatiquement exposées via REST

### Implémenter le service

```pascal
unit ServicesCalculatriceImpl;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  mormot.core.base,
  ServicesCalculatrice;

type
  // Implémentation du service
  TCalculatrice = class(TInterfacedObject, ICalculatrice)
  public
    function Additionner(A, B: Double): Double;
    function Soustraire(A, B: Double): Double;
    function Multiplier(A, B: Double): Double;
    function Diviser(A, B: Double): Double;
    function Calculer(const Expression: RawUtf8): Double;
  end;

implementation

function TCalculatrice.Additionner(A, B: Double): Double;
begin
  Result := A + B;
end;

function TCalculatrice.Soustraire(A, B: Double): Double;
begin
  Result := A - B;
end;

function TCalculatrice.Multiplier(A, B: Double): Double;
begin
  Result := A * B;
end;

function TCalculatrice.Diviser(A, B: Double): Double;
begin
  if B = 0 then
    raise Exception.Create('Division par zéro impossible');
  Result := A / B;
end;

function TCalculatrice.Calculer(const Expression: RawUtf8): Double;
begin
  // Implémentation simplifiée
  // En production, utiliser un vrai parseur d'expressions
  Result := 0; // À implémenter
end;

end.
```

### Serveur avec services SOA

```pascal
program ServeurSOA;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.http.server,
  ServicesCalculatrice,
  ServicesCalculatriceImpl;

var
  Model: TOrmModel;
  ServeurRest: TRestServerFullMemory;
  ServeurHttp: TRestHttpServer;
begin
  // Créer le modèle (vide pour ce serveur SOA pur)
  Model := TOrmModel.Create([], 'root');

  // Créer le serveur REST en mémoire
  ServeurRest := TRestServerFullMemory.Create(Model, False);
  try
    // Enregistrer le service
    ServeurRest.ServiceDefine(TCalculatrice, [ICalculatrice], sicShared);

    // Créer le serveur HTTP
    ServeurHttp := TRestHttpServer.Create('8080', [ServeurRest], '+', HTTP_DEFAULT_MODE);
    try
      WriteLn('Serveur SOA démarré sur le port 8080');
      WriteLn('Service disponible : Calculatrice');
      WriteLn('URL : http://localhost:8080/root/Calculatrice');
      WriteLn('Appuyez sur Entrée pour arrêter...');
      ReadLn;
    finally
      ServeurHttp.Free;
    end;
  finally
    ServeurRest.Free;
    Model.Free;
  end;
end.
```

**Paramètre sicShared :**

- `sicSingle` : Une instance unique pour tous les clients
- `sicShared` : Une instance par thread
- `sicPerSession` : Une instance par session utilisateur
- `sicPerUser` : Une instance par utilisateur
- `sicPerThread` : Une instance par thread

### Client pour le service SOA

```pascal
program ClientSOA;

{$mode objfpc}{$H+}

uses
  SysUtils,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.http.client,
  ServicesCalculatrice;

var
  Model: TOrmModel;
  Client: TRestHttpClient;
  Calculatrice: ICalculatrice;
  Resultat: Double;
begin
  Model := TOrmModel.Create([], 'root');
  Client := TRestHttpClient.Create('localhost', '8080', Model);
  try
    // Obtenir l'instance du service
    if Client.ServiceResolve(ICalculatrice, Calculatrice) then
    begin
      WriteLn('Service Calculatrice connecté');

      // Utiliser le service comme une interface locale
      Resultat := Calculatrice.Additionner(10, 5);
      WriteLn('10 + 5 = ', Resultat:0:2);

      Resultat := Calculatrice.Soustraire(10, 5);
      WriteLn('10 - 5 = ', Resultat:0:2);

      Resultat := Calculatrice.Multiplier(10, 5);
      WriteLn('10 * 5 = ', Resultat:0:2);

      Resultat := Calculatrice.Diviser(10, 5);
      WriteLn('10 / 5 = ', Resultat:0:2);

      try
        Resultat := Calculatrice.Diviser(10, 0);
      except
        on E: Exception do
          WriteLn('Erreur attendue : ', E.Message);
      end;
    end
    else
      WriteLn('Impossible de se connecter au service');

  finally
    Client.Free;
    Model.Free;
  end;
end.
```

**Magie de mORMot :** Le client appelle les méthodes comme si l'objet était local, mais en réalité les appels sont transmis via HTTP/JSON au serveur !

## Authentification et sécurité

### Ajouter l'authentification

```pascal
unit ModelUtilisateursAuth;

{$mode objfpc}{$H+}

interface

uses
  mormot.orm.core;

type
  // Utilisateur avec authentification
  TOrmAuthUser = class(TOrm)
  private
    FLogonName: RawUtf8;
    FPassword: RawUtf8;
    FNom: RawUtf8;
    FEmail: RawUtf8;
    FGroupe: RawUtf8;
  published
    property LogonName: RawUtf8 read FLogonName write FLogonName;
    property Password: RawUtf8 read FPassword write FPassword stored false;
    property Nom: RawUtf8 read FNom write FNom;
    property Email: RawUtf8 read FEmail write FEmail;
    property Groupe: RawUtf8 read FGroupe write FGroupe;
  end;

implementation

end.
```

### Serveur avec authentification

```pascal
program ServeurAvecAuth;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  mormot.core.base,
  mormot.core.crypto,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.http.server,
  mormot.orm.storage,
  ModelUtilisateursAuth;

var
  Model: TOrmModel;
  ServeurRest: TRestServerDB;
  ServeurHttp: TRestHttpServer;
  Admin: TOrmAuthUser;
begin
  Model := TOrmModel.Create([TOrmAuthUser], 'root');
  ServeurRest := TRestServerDB.Create(Model, 'auth.db');
  try
    ServeurRest.CreateMissingTables;

    // Activer l'authentification
    ServeurRest.HandleAuthentication := True;

    // Créer un utilisateur admin si n'existe pas
    Admin := TOrmAuthUser.CreateAndFillPrepare(ServeurRest.Orm,
      'LogonName=?', ['admin']);
    try
      if Admin.ID = 0 then
      begin
        Admin.LogonName := 'admin';
        Admin.Password := 'admin123';  // Sera hashé automatiquement
        Admin.Nom := 'Administrateur';
        Admin.Email := 'admin@example.com';
        Admin.Groupe := 'Admin';
        ServeurRest.Add(Admin, True);
        WriteLn('Utilisateur admin créé');
      end;
    finally
      Admin.Free;
    end;

    // Démarrer le serveur HTTP
    ServeurHttp := TRestHttpServer.Create('8080', [ServeurRest], '+', HTTP_DEFAULT_MODE);
    try
      WriteLn('Serveur avec authentification démarré');
      WriteLn('Utilisateur : admin');
      WriteLn('Mot de passe : admin123');
      ReadLn;
    finally
      ServeurHttp.Free;
    end;
  finally
    ServeurRest.Free;
    Model.Free;
  end;
end.
```

### Client avec authentification

```pascal
program ClientAvecAuth;

{$mode objfpc}{$H+}

uses
  SysUtils,
  mormot.core.base,
  mormot.orm.core,
  mormot.rest.http.client,
  ModelUtilisateursAuth;

var
  Model: TOrmModel;
  Client: TRestHttpClient;
  User: TOrmAuthUser;
begin
  Model := TOrmModel.Create([TOrmAuthUser], 'root');
  Client := TRestHttpClient.Create('localhost', '8080', Model);
  try
    // S'authentifier
    if Client.SetUser('admin', 'admin123') then
    begin
      WriteLn('Authentification réussie');
      WriteLn('Token de session : ', Client.SessionKey);

      // Maintenant on peut accéder aux données
      User := TOrmAuthUser.Create(Client, 1);
      try
        if User.ID <> 0 then
          WriteLn('Utilisateur : ', User.Nom)
        else
          WriteLn('Utilisateur non trouvé');
      finally
        User.Free;
      end;
    end
    else
      WriteLn('Échec de l''authentification');

  finally
    Client.Free;
    Model.Free;
  end;
end.
```

## Requêtes avancées

### Filtres et recherches

```pascal
// Recherche simple
Liste := Client.RetrieveList(TOrmUtilisateur, 'Age>?', [25]);

// Recherche avec tri
Liste := Client.RetrieveList(TOrmUtilisateur, '', [], 'Nom ASC');

// Recherche complexe
Liste := Client.RetrieveList(TOrmUtilisateur,
  'Age>=? AND Age<=? AND Nom LIKE ?',
  [18, 65, 'Jean%'],
  'Age DESC');

// Compter les résultats
Nombre := Client.TableRowCount(TOrmUtilisateur, 'Age>?', [30]);
WriteLn('Utilisateurs de plus de 30 ans : ', Nombre);
```

### Pagination

```pascal
var
  Page, PageSize, Total: Integer;
  Liste: TObjectList;
begin
  PageSize := 10;
  Page := 1;

  // Récupérer une page
  Liste := Client.RetrieveList(TOrmUtilisateur, '', [], '',
    (Page - 1) * PageSize, // Offset
    PageSize);              // Limit

  // Compter le total
  Total := Client.TableRowCount(TOrmUtilisateur);

  WriteLn(Format('Page %d/%d (%d éléments)',
    [Page, (Total + PageSize - 1) div PageSize, Total]));
end;
```

## JSON et sérialisation

### Conversion objet ↔ JSON

mORMot gère automatiquement la sérialisation JSON :

```pascal
var
  Utilisateur: TOrmUtilisateur;
  JSON: RawUtf8;
begin
  // Objet vers JSON
  Utilisateur := TOrmUtilisateur.Create;
  try
    Utilisateur.Nom := 'Test';
    Utilisateur.Email := 'test@example.com';
    Utilisateur.Age := 25;

    JSON := Utilisateur.GetJsonValues(True, True, ooSelect);
    WriteLn('JSON : ', JSON);
    // Résultat : {"ID":0,"Nom":"Test","Email":"test@example.com","Age":25}
  finally
    Utilisateur.Free;
  end;

  // JSON vers objet
  Utilisateur := TOrmUtilisateur.Create;
  try
    JSON := '{"Nom":"Nouveau","Email":"nouveau@example.com","Age":30}';
    Utilisateur.FillFrom(JSON);
    WriteLn('Nom : ', Utilisateur.Nom);
    WriteLn('Age : ', Utilisateur.Age);
  finally
    Utilisateur.Free;
  end;
end;
```

### JSON personnalisé

```pascal
uses
  mormot.core.json,
  mormot.core.variants;

var
  Doc: TDocVariantData;
  JSON: RawUtf8;
begin
  // Créer un JSON dynamique
  Doc.InitObject(['nom', 'Jean', 'age', 30, 'actif', True]);
  JSON := Doc.ToJson;
  WriteLn(JSON);
  // {"nom":"Jean","age":30,"actif":true}

  // Ajouter des propriétés
  Doc.AddValue('email', 'jean@example.com');
  Doc.AddValue('adresse', _ObjFast(['rue', '123 Main St', 'ville', 'Paris']));

  // Lire des propriétés
  WriteLn('Nom : ', Doc.U['nom']);
  WriteLn('Age : ', Doc.I['age']);
end;
```

## Performance et optimisation

### Batch operations (opérations par lot)

Pour insérer plusieurs enregistrements efficacement :

```pascal
var
  Batch: TRestBatch;
  i: Integer;
  User: TOrmUtilisateur;
begin
  Batch := TRestBatch.Create(Client.Orm, TOrmUtilisateur, 1000);
  try
    // Ajouter 1000 utilisateurs
    for i := 1 to 1000 do
    begin
      User := TOrmUtilisateur.Create;
      try
        User.Nom := Format('Utilisateur%d', [i]);
        User.Email := Format('user%d@example.com', [i]);
        User.Age := 20 + Random(50);
        Batch.Add(User, True);
      finally
        User.Free;
      end;
    end;

    // Envoyer tout en une seule fois
    if Client.BatchSend(Batch) = HTTP_SUCCESS then
      WriteLn('1000 utilisateurs créés en batch')
    else
      WriteLn('Erreur lors du batch');
  finally
    Batch.Free;
  end;
end;
```

**Avantage :** Environ 100x plus rapide que 1000 INSERT individuels !

### Cache côté client

```pascal
// Activer le cache
Client.CacheOrNil := TSynCache.Create;

// Les requêtes seront mises en cache
Liste1 := Client.RetrieveList(TOrmUtilisateur, '');  // Requête réelle vers le serveur
Liste2 := Client.RetrieveList(TOrmUtilisateur, '');  // Récupéré du cache (instantané)

// Vider le cache manuellement
Client.CacheOrNil.Flush;

// Désactiver le cache
Client.CacheOrNil.Free;
Client.CacheOrNil := nil;
```

**Quand utiliser le cache :**
- Données qui changent rarement
- Lectures fréquentes des mêmes données
- Pour réduire la charge réseau

**Quand ne pas utiliser le cache :**
- Données en temps réel
- Données qui changent fréquemment
- Applications critiques nécessitant toujours les dernières données

### Compression HTTP

Pour économiser la bande passante :

```pascal
// Côté serveur - activer la compression
ServeurHttp.Compression := [hcDeflate, hcGzip];

// Côté client - activer la compression
Client.Compression := [hcDeflate];
```

**Résultat :** Réduction de 60-90% de la taille des données transférées !

### Connexions persistantes

```pascal
// Maintenir la connexion ouverte entre les requêtes
Client.KeepAliveMS := 60000;  // 60 secondes

// Configuration du timeout
Client.Timeout := 30000;  // 30 secondes
```

## Validation et contraintes

### Validation personnalisée

Ajoutez de la validation dans vos classes ORM :

```pascal
unit ModelUtilisateursValidation;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.orm.core;

type
  TOrmUtilisateurValide = class(TOrm)
  private
    FNom: RawUtf8;
    FEmail: RawUtf8;
    FAge: Integer;

    function ValiderEmail(const Email: RawUtf8): Boolean;
  protected
    // Méthode appelée avant l'insertion/mise à jour
    function Validate(aRest: TRest; aRecord: TOrm;
                     aID: TID; aEvent: TOrmEvent): Boolean; override;
  published
    property Nom: RawUtf8 read FNom write FNom;
    property Email: RawUtf8 read FEmail write FEmail;
    property Age: Integer read FAge write FAge;
  end;

implementation

function TOrmUtilisateurValide.ValiderEmail(const Email: RawUtf8): Boolean;
begin
  // Validation simple d'email
  Result := (Pos('@', Email) > 0) and (Pos('.', Email) > Pos('@', Email));
end;

function TOrmUtilisateurValide.Validate(aRest: TRest; aRecord: TOrm;
                                        aID: TID; aEvent: TOrmEvent): Boolean;
begin
  Result := inherited Validate(aRest, aRecord, aID, aEvent);
  if not Result then
    Exit;

  // Vérifier le nom
  if Length(FNom) < 2 then
  begin
    aRest.InternalLog('Nom trop court (minimum 2 caractères)', sllWarning);
    Exit(False);
  end;

  // Vérifier l'email
  if not ValiderEmail(FEmail) then
  begin
    aRest.InternalLog('Format d''email invalide : %', [FEmail], sllWarning);
    Exit(False);
  end;

  // Vérifier l'âge
  if (FAge < 0) or (FAge > 150) then
  begin
    aRest.InternalLog('Âge invalide : %', [FAge], sllWarning);
    Exit(False);
  end;

  Result := True;
end;

end.
```

**Utilisation :**

```pascal
var
  User: TOrmUtilisateurValide;
begin
  User := TOrmUtilisateurValide.Create;
  try
    User.Nom := 'A';  // Trop court
    User.Email := 'invalide';  // Pas d'@
    User.Age := -5;  // Négatif

    if Client.Add(User, True) = 0 then
      WriteLn('Validation échouée (comme prévu)')
    else
      WriteLn('Validation réussie');
  finally
    User.Free;
  end;
end;
```

### Contraintes au niveau base de données

```pascal
// Créer un index unique sur l'email
ServeurRest.CreateSqlIndex(TOrmUtilisateur, 'Email', True);  // True = unique

// Créer un index sur un champ pour accélérer les recherches
ServeurRest.CreateSqlIndex(TOrmUtilisateur, 'Nom', False);
```

## Relations entre tables

### One-to-Many (Un-à-Plusieurs)

Exemple : Un client peut avoir plusieurs commandes.

```pascal
unit ModelCommandes;

{$mode objfpc}{$H+}

interface

uses
  mormot.orm.core;

type
  // Table Client
  TOrmClient = class(TOrm)
  private
    FNom: RawUtf8;
    FEmail: RawUtf8;
  published
    property Nom: RawUtf8 read FNom write FNom;
    property Email: RawUtf8 read FEmail write FEmail;
  end;

  // Table Commande (référence un Client)
  TOrmCommande = class(TOrm)
  private
    FClient: TRecordReference;  // Référence vers TOrmClient
    FNumero: RawUtf8;
    FMontant: Currency;
    FDateCommande: TDateTime;
  published
    property Client: TRecordReference read FClient write FClient;
    property Numero: RawUtf8 read FNumero write FNumero;
    property Montant: Currency read FMontant write FMontant;
    property DateCommande: TDateTime read FDateCommande write FDateCommande;
  end;

implementation

initialization
  // Déclarer la relation
  TOrmCommande.AddFilterOrValidate('Client', TSynValidateRest.Create(TOrmClient));

end.
```

**Utilisation :**

```pascal
var
  Client: TOrmClient;
  Commande: TOrmCommande;
  ClientID: TID;
begin
  // Créer un client
  Client := TOrmClient.Create;
  try
    Client.Nom := 'Entreprise ABC';
    Client.Email := 'contact@abc.com';
    ClientID := ServeurRest.Add(Client, True);
  finally
    Client.Free;
  end;

  // Créer une commande pour ce client
  Commande := TOrmCommande.Create;
  try
    Commande.Client := ClientID;  // Référencer le client
    Commande.Numero := 'CMD-001';
    Commande.Montant := 1250.50;
    Commande.DateCommande := Now;
    ServeurRest.Add(Commande, True);
  finally
    Commande.Free;
  end;

  // Récupérer toutes les commandes d'un client
  Liste := ServeurRest.RetrieveList(TOrmCommande, 'Client=?', [ClientID]);
end;
```

### Many-to-Many (Plusieurs-à-Plusieurs)

Exemple : Des étudiants s'inscrivent à plusieurs cours.

```pascal
unit ModelCoursEtudiants;

{$mode objfpc}{$H+}

interface

uses
  mormot.orm.core;

type
  TOrmEtudiant = class(TOrm)
  private
    FNom: RawUtf8;
    FPrenom: RawUtf8;
  published
    property Nom: RawUtf8 read FNom write FNom;
    property Prenom: RawUtf8 read FPrenom write FPrenom;
  end;

  TOrmCours = class(TOrm)
  private
    FTitre: RawUtf8;
    FCode: RawUtf8;
  published
    property Titre: RawUtf8 read FTitre write FTitre;
    property Code: RawUtf8 read FCode write FCode;
  end;

  // Table de liaison
  TOrmInscription = class(TOrm)
  private
    FEtudiant: TRecordReference;
    FCours: TRecordReference;
    FDateInscription: TDateTime;
    FNote: Integer;
  published
    property Etudiant: TRecordReference read FEtudiant write FEtudiant;
    property Cours: TRecordReference read FCours write FCours;
    property DateInscription: TDateTime read FDateInscription write FDateInscription;
    property Note: Integer read FNote write FNote;
  end;

implementation

initialization
  TOrmInscription.AddFilterOrValidate('Etudiant', TSynValidateRest.Create(TOrmEtudiant));
  TOrmInscription.AddFilterOrValidate('Cours', TSynValidateRest.Create(TOrmCours));

end.
```

**Récupérer tous les cours d'un étudiant :**

```pascal
function ObtenirCoursEtudiant(EtudiantID: TID): TObjectList;
var
  Inscriptions: TObjectList;
  i: Integer;
  Inscription: TOrmInscription;
  Cours: TOrmCours;
begin
  Result := TObjectList.Create(True);

  // Récupérer les inscriptions de l'étudiant
  Inscriptions := ServeurRest.RetrieveList(TOrmInscription, 'Etudiant=?', [EtudiantID]);
  try
    for i := 0 to Inscriptions.Count - 1 do
    begin
      Inscription := TOrmInscription(Inscriptions[i]);

      // Récupérer le cours correspondant
      Cours := TOrmCours.Create(ServeurRest, Inscription.Cours);
      if Cours.ID <> 0 then
        Result.Add(Cours)
      else
        Cours.Free;
    end;
  finally
    Inscriptions.Free;
  end;
end;
```

## Logging et monitoring

### Configuration des logs

```pascal
uses
  mormot.core.log;

var
  Logger: TSynLog;
begin
  // Configurer le niveau de log
  TSynLog.Family.Level := LOG_VERBOSE;  // Tout logger
  // ou
  TSynLog.Family.Level := [sllError, sllException];  // Uniquement erreurs

  // Configurer la destination des logs
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  // Dossier des logs
  {$IFDEF WINDOWS}
  TSynLog.Family.DestinationPath := 'C:\Logs\';
  {$ENDIF}
  {$IFDEF UNIX}
  TSynLog.Family.DestinationPath := '/var/log/monapp/';
  {$ENDIF}

  // Utilisation
  Logger := TSynLog.Add;
  Logger.Log(sllInfo, 'Application démarrée');
  Logger.Log(sllDebug, 'Variable X = %', [X]);
  Logger.Log(sllError, 'Erreur : %', [E.Message]);
end;
```

### Logs automatiques des requêtes

```pascal
// Activer le logging SQL
ServeurRest.LogFamily.Level := LOG_VERBOSE;

// Toutes les requêtes SQL seront loggées
// Exemple de sortie :
// 20250102 14:30:15.123 SQL SELECT * FROM Utilisateur WHERE Age>25
// 20250102 14:30:15.145 SQL INSERT INTO Utilisateur (Nom,Email) VALUES (?,?)
```

### Monitoring des performances

```pascal
uses
  mormot.core.perf;

var
  Timer: TPrecisionTimer;
  Stats: RawUtf8;
begin
  Timer.Start;

  // Code à mesurer
  for i := 1 to 1000 do
    Client.Retrieve(i, User);

  Stats := Timer.Stop;
  WriteLn('1000 récupérations en : ', Stats);
  WriteLn('Débit : ', Timer.PerSec(1000):0:0, ' ops/sec');
end;
```

## Déploiement multi-plateforme

### Configuration Windows

**Serveur en tant que service Windows :**

```pascal
program ServeurService;

{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.http.server;

type
  TMonService = class(TServiceSingle)
  protected
    FServeur: TRestHttpServer;
    function DoStart: Boolean; override;
    function DoStop: Boolean; override;
  end;

function TMonService.DoStart: Boolean;
var
  Model: TOrmModel;
  Rest: TRestServerDB;
begin
  Model := TOrmModel.Create([TOrmUtilisateur], 'root');
  Rest := TRestServerDB.Create(Model, 'data.db');
  Rest.CreateMissingTables;

  FServeur := TRestHttpServer.Create('8080', [Rest], '+', HTTP_DEFAULT_MODE);

  Result := True;
end;

function TMonService.DoStop: Boolean;
begin
  FServeur.Free;
  Result := True;
end;

begin
  TMonService.Run('MonServeurREST', 'Serveur REST mORMot');
end.
```

**Installation du service :**

```batch
REM Installer le service
ServeurService.exe /install

REM Démarrer le service
net start MonServeurREST

REM Arrêter le service
net stop MonServeurREST

REM Désinstaller le service
ServeurService.exe /uninstall
```

### Configuration Ubuntu/Linux

**Serveur en tant que service systemd :**

Créer le fichier `/etc/systemd/system/serveurrest.service` :

```ini
[Unit]
Description=Serveur REST mORMot
After=network.target

[Service]
Type=simple
User=www-data
Group=www-data
WorkingDirectory=/opt/monapp
ExecStart=/opt/monapp/serveur
Restart=always
RestartSec=10

# Limites de sécurité
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=/opt/monapp/data /var/log/monapp

[Install]
WantedBy=multi-user.target
```

**Commandes systemd :**

```bash
# Activer le service
sudo systemctl enable serveurrest

# Démarrer le service
sudo systemctl start serveurrest

# Voir le statut
sudo systemctl status serveurrest

# Voir les logs
sudo journalctl -u serveurrest -f

# Redémarrer le service
sudo systemctl restart serveurrest

# Arrêter le service
sudo systemctl stop serveurrest
```

### Configuration multi-plateforme dans le code

```pascal
program ServeurMultiPlateforme;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, BaseUnix,
  {$ENDIF}
  SysUtils,
  mormot.core.base,
  mormot.orm.core,
  mormot.rest.http.server;

const
  {$IFDEF WINDOWS}
  CHEMIN_BASE = 'C:\ProgramData\MonApp\';
  CHEMIN_LOGS = 'C:\ProgramData\MonApp\Logs\';
  {$ENDIF}
  {$IFDEF UNIX}
  CHEMIN_BASE = '/var/lib/monapp/';
  CHEMIN_LOGS = '/var/log/monapp/';
  {$ENDIF}

var
  Model: TOrmModel;
  Rest: TRestServerDB;
  Serveur: TRestHttpServer;
begin
  // Créer les dossiers si nécessaire
  ForceDirectories(CHEMIN_BASE);
  ForceDirectories(CHEMIN_LOGS);

  // Configuration des logs
  TSynLog.Family.DestinationPath := CHEMIN_LOGS;

  // Créer le serveur
  Model := TOrmModel.Create([TOrmUtilisateur], 'root');
  Rest := TRestServerDB.Create(Model, CHEMIN_BASE + 'data.db');
  Rest.CreateMissingTables;

  Serveur := TRestHttpServer.Create('8080', [Rest], '+', HTTP_DEFAULT_MODE);
  try
    WriteLn('Serveur démarré sur le port 8080');
    WriteLn('Base de données : ', CHEMIN_BASE, 'data.db');
    WriteLn('Logs : ', CHEMIN_LOGS);

    {$IFDEF UNIX}
    // Sous Unix, gérer les signaux proprement
    fpSignal(SIGTERM, @SignalHandler);
    fpSignal(SIGINT, @SignalHandler);
    {$ENDIF}

    WriteLn('Appuyez sur Ctrl+C pour arrêter...');

    {$IFDEF WINDOWS}
    ReadLn;
    {$ENDIF}
    {$IFDEF UNIX}
    while not Terminated do
      Sleep(1000);
    {$ENDIF}

  finally
    Serveur.Free;
    Rest.Free;
    Model.Free;
  end;
end.
```

## CORS et API publiques

### Activer CORS (Cross-Origin Resource Sharing)

Pour permettre aux applications web d'accéder à votre API :

```pascal
uses
  mormot.net.http;

begin
  // Créer le serveur
  Serveur := TRestHttpServer.Create('8080', [Rest], '+', HTTP_DEFAULT_MODE);

  // Configurer CORS
  Serveur.AccessControlAllowOrigin := '*';  // Autoriser tous les domaines
  // ou spécifique :
  // Serveur.AccessControlAllowOrigin := 'https://monsite.com';

  Serveur.AccessControlAllowMethods := 'GET,POST,PUT,DELETE,OPTIONS';
  Serveur.AccessControlAllowHeaders := 'Content-Type,Authorization';
end;
```

**Test avec JavaScript :**

```html
<!DOCTYPE html>
<html>
<head>
    <title>Test API mORMot</title>
</head>
<body>
    <h1>Test API mORMot</h1>
    <button onclick="chargerUtilisateurs()">Charger utilisateurs</button>
    <div id="resultat"></div>

    <script>
        function chargerUtilisateurs() {
            fetch('http://localhost:8080/root/Utilisateur')
                .then(response => response.json())
                .then(data => {
                    document.getElementById('resultat').innerHTML =
                        '<pre>' + JSON.stringify(data, null, 2) + '</pre>';
                })
                .catch(error => {
                    console.error('Erreur:', error);
                    document.getElementById('resultat').innerHTML =
                        'Erreur: ' + error;
                });
        }
    </script>
</body>
</html>
```

## Documentation automatique de l'API

### Générer la documentation Swagger/OpenAPI

```pascal
uses
  mormot.rest.swagger;

var
  Swagger: TSwaggerDefinition;
  JSON: RawUtf8;
begin
  // Générer la définition Swagger
  Swagger := TSwaggerDefinition.Create(Rest);
  try
    Swagger.Info.Title := 'Mon API REST';
    Swagger.Info.Description := 'API de gestion des utilisateurs';
    Swagger.Info.Version := '1.0.0';
    Swagger.Info.Contact.Name := 'Support';
    Swagger.Info.Contact.Email := 'support@example.com';

    // Générer le JSON
    JSON := Swagger.SaveToJson;
    WriteLn(JSON);

    // Sauvegarder dans un fichier
    FileFromString(JSON, 'swagger.json');
  finally
    Swagger.Free;
  end;
end;
```

**Accéder à la documentation :**

Une fois le fichier `swagger.json` généré, utilisez Swagger UI :

```
https://petstore.swagger.io/?url=http://localhost:8080/swagger.json
```

## Exemples pratiques complets

### API de gestion de tâches (Todo)

```pascal
unit ModelTodo;

{$mode objfpc}{$H+}

interface

uses
  mormot.orm.core;

type
  TStatutTache = (stNonCommencee, stEnCours, stTerminee);
  TPriorite = (prBasse, prNormale, prHaute, prUrgente);

  TOrmTache = class(TOrm)
  private
    FTitre: RawUtf8;
    FDescription: RawUtf8;
    FStatut: TStatutTache;
    FPriorite: TPriorite;
    FDateCreation: TDateTime;
    FDateEcheance: TDateTime;
    FTerminee: Boolean;
  published
    property Titre: RawUtf8 read FTitre write FTitre;
    property Description: RawUtf8 read FDescription write FDescription;
    property Statut: TStatutTache read FStatut write FStatut;
    property Priorite: TPriorite read FPriorite write FPriorite;
    property DateCreation: TDateTime read FDateCreation write FDateCreation;
    property DateEcheance: TDateTime read FDateEcheance write FDateEcheance;
    property Terminee: Boolean read FTerminee write FTerminee;
  end;

implementation

end.
```

**Service métier pour les tâches :**

```pascal
unit ServicesTodo;

{$mode objfpc}{$H+}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  ModelTodo;

type
  // Interface du service
  ITodoService = interface(IInvokable)
    ['{11111111-2222-3333-4444-555555555555}']

    function CreerTache(const Titre, Description: RawUtf8;
                       Priorite: TPriorite): TID;
    function ObtenirTache(ID: TID): TOrmTache;
    function ListerTaches(Statut: TStatutTache): TObjectList;
    function MarquerTerminee(ID: TID): Boolean;
    function SupprimerTache(ID: TID): Boolean;
    function CompterTaches(Statut: TStatutTache): Integer;
  end;

implementation

end.
```

**Implémentation du service :**

```pascal
unit ServicesTodoImpl;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.orm.core,
  mormot.rest.server,
  ServicesTodo,
  ModelTodo;

type
  TTodoService = class(TInjectableObjectRest, ITodoService)
  public
    function CreerTache(const Titre, Description: RawUtf8;
                       Priorite: TPriorite): TID;
    function ObtenirTache(ID: TID): TOrmTache;
    function ListerTaches(Statut: TStatutTache): TObjectList;
    function MarquerTerminee(ID: TID): Boolean;
    function SupprimerTache(ID: TID): Boolean;
    function CompterTaches(Statut: TStatutTache): Integer;
  end;

implementation

function TTodoService.CreerTache(const Titre, Description: RawUtf8;
                                 Priorite: TPriorite): TID;
var
  Tache: TOrmTache;
begin
  Tache := TOrmTache.Create;
  try
    Tache.Titre := Titre;
    Tache.Description := Description;
    Tache.Priorite := Priorite;
    Tache.Statut := stNonCommencee;
    Tache.DateCreation := Now;
    Tache.Terminee := False;

    Result := Server.Add(Tache, True);
  finally
    Tache.Free;
  end;
end;

function TTodoService.ObtenirTache(ID: TID): TOrmTache;
begin
  Result := TOrmTache.Create(Server, ID);
  if Result.ID = 0 then
    FreeAndNil(Result);
end;

function TTodoService.ListerTaches(Statut: TStatutTache): TObjectList;
begin
  Result := Server.RetrieveList(TOrmTache, 'Statut=?', [Ord(Statut)], 'Priorite DESC');
end;

function TTodoService.MarquerTerminee(ID: TID): Boolean;
var
  Tache: TOrmTache;
begin
  Tache := TOrmTache.Create(Server, ID);
  try
    if Tache.ID <> 0 then
    begin
      Tache.Terminee := True;
      Tache.Statut := stTerminee;
      Result := Server.Update(Tache);
    end
    else
      Result := False;
  finally
    Tache.Free;
  end;
end;

function TTodoService.SupprimerTache(ID: TID): Boolean;
begin
  Result := Server.Delete(TOrmTache, ID);
end;

function TTodoService.CompterTaches(Statut: TStatutTache): Integer;
begin
  Result := Server.TableRowCount(TOrmTache, 'Statut=?', [Ord(Statut)]);
end;

end.
```

**Serveur complet :**

```pascal
program ServeurTodo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  mormot.core.base,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.http.server,
  ModelTodo,
  ServicesTodo,
  ServicesTodoImpl;

var
  Model: TOrmModel;
  Rest: TRestServerDB;
  Http: TRestHttpServer;
begin
  Model := TOrmModel.Create([TOrmTache], 'api');
  Rest := TRestServerDB.Create(Model, 'todo.db');
  try
    Rest.CreateMissingTables;

    // Enregistrer le service
    Rest.ServiceDefine(TTodoService, [ITodoService], sicShared);

    // Démarrer le serveur HTTP
    Http := TRestHttpServer.Create('8080', [Rest], '+', HTTP_DEFAULT_MODE);
    try
      WriteLn('=== Serveur Todo API ===');
      WriteLn('URL REST : http://localhost:8080/api/Tache');
      WriteLn('URL Service : http://localhost:8080/api/TodoService');
      WriteLn('Appuyez sur Entrée pour arrêter...');
      ReadLn;
    finally
      Http.Free;
    end;
  finally
    Rest.Free;
    Model.Free;
  end;
end.
```

## Bonnes pratiques

### 1. Structure de projet

```
MonProjet/
├── src/
│   ├── models/           # Modèles ORM
│   │   ├── ModelUtilisateur.pas
│   │   └── ModelCommande.pas
│   ├── services/         # Interfaces de services
│   │   ├── IUserService.pas
│   │   └── IOrderService.pas
│   ├── impl/             # Implémentations
│   │   ├── UserServiceImpl.pas
│   │   └── OrderServiceImpl.pas
│   └── server.pas        # Point d'entrée
├── tests/                # Tests unitaires
├── docs/                 # Documentation
└── deploy/               # Scripts de déploiement
```

### 2. Gestion des erreurs

```pascal
type
  EApiException = class(Exception)
  private
    FStatusCode: Integer;
  public
    constructor Create(const Msg: String; StatusCode: Integer = 500);
    property StatusCode: Integer read FStatusCode;
  end;

// Dans les services
function TTodoService.ObtenirTache(ID: TID): TOrmTache;
begin
  Result := TOrmTache.Create(Server, ID);
  if Result.ID = 0 then
  begin
    FreeAndNil(Result);
    raise EApiException.Create('Tâche non trouvée', 404);
  end;
end;
```

### 3. Versionnement de l'API

```pascal
// Model v1
TOrmModel.Create([TOrmUtilisateurV1], 'api/v1');

// Model v2 (avec nouveaux champs)
TOrmModel.Create([TOrmUtilisateurV2], 'api/v2');

// Les deux versions peuvent coexister
```

### 4. Tests unitaires

```pascal
unit TestsTodo;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry,
  mormot.orm.core,
  mormot.rest.server,
  ModelTodo,
  ServicesTodo,
  ServicesTodoImpl;

type
  TTestTodoService = class(TTestCase)
  private
    FModel: TOrmModel;
    FRest: TRestServerDB;
    FService: ITodoService;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreerTache;
    procedure TestObtenirTache;
    procedure TestMarquerTerminee;
    procedure TestListerTaches;
    procedure TestSupprimerTache;
  end;

implementation

procedure TTestTodoService.SetUp;
begin
  // Créer une base de données en mémoire pour les tests
  FModel := TOrmModel.Create([TOrmTache], 'test');
  FRest := TRestServerDB.Create(FModel, ':memory:');
  FRest.CreateMissingTables;

  // Enregistrer le service
  FRest.ServiceDefine(TTodoService, [ITodoService], sicShared);

  // Obtenir l'instance du service
  FRest.ServiceResolve(ITodoService, FService);
end;

procedure TTestTodoService.TearDown;
begin
  FService := nil;
  FRest.Free;
  FModel.Free;
end;

procedure TTestTodoService.TestCreerTache;
var
  ID: TID;
  Tache: TOrmTache;
begin
  // Créer une tâche
  ID := FService.CreerTache('Test unitaire', 'Description test', prNormale);

  // Vérifier que l'ID est valide
  AssertTrue('ID doit être > 0', ID > 0);

  // Vérifier que la tâche existe
  Tache := TOrmTache.Create(FRest, ID);
  try
    AssertEquals('Titre incorrect', 'Test unitaire', Tache.Titre);
    AssertEquals('Statut incorrect', Ord(stNonCommencee), Ord(Tache.Statut));
    AssertFalse('Ne doit pas être terminée', Tache.Terminee);
  finally
    Tache.Free;
  end;
end;

procedure TTestTodoService.TestObtenirTache;
var
  ID: TID;
  Tache: TOrmTache;
begin
  // Créer une tâche
  ID := FService.CreerTache('Tâche à récupérer', 'Description', prHaute);

  // La récupérer via le service
  Tache := FService.ObtenirTache(ID);
  try
    AssertNotNull('Tâche ne doit pas être nil', Tache);
    AssertEquals('Titre incorrect', 'Tâche à récupérer', Tache.Titre);
    AssertEquals('Priorité incorrecte', Ord(prHaute), Ord(Tache.Priorite));
  finally
    Tache.Free;
  end;

  // Tester avec un ID invalide
  Tache := FService.ObtenirTache(99999);
  AssertNull('Tâche inexistante doit retourner nil', Tache);
end;

procedure TTestTodoService.TestMarquerTerminee;
var
  ID: TID;
  Tache: TOrmTache;
begin
  // Créer une tâche
  ID := FService.CreerTache('Tâche à terminer', 'Description', prNormale);

  // La marquer comme terminée
  AssertTrue('Marquage doit réussir', FService.MarquerTerminee(ID));

  // Vérifier le changement
  Tache := TOrmTache.Create(FRest, ID);
  try
    AssertTrue('Doit être marquée terminée', Tache.Terminee);
    AssertEquals('Statut doit être Terminee', Ord(stTerminee), Ord(Tache.Statut));
  finally
    Tache.Free;
  end;
end;

procedure TTestTodoService.TestListerTaches;
var
  Liste: TObjectList;
  i: Integer;
begin
  // Créer plusieurs tâches
  FService.CreerTache('Tâche 1', 'Desc 1', prBasse);
  FService.CreerTache('Tâche 2', 'Desc 2', prNormale);
  FService.CreerTache('Tâche 3', 'Desc 3', prHaute);

  // Lister les tâches non commencées
  Liste := FService.ListerTaches(stNonCommencee);
  try
    AssertEquals('Doit avoir 3 tâches', 3, Liste.Count);
  finally
    Liste.Free;
  end;

  // Marquer une tâche terminée
  FService.MarquerTerminee(1);

  // Lister à nouveau
  Liste := FService.ListerTaches(stNonCommencee);
  try
    AssertEquals('Doit avoir 2 tâches', 2, Liste.Count);
  finally
    Liste.Free;
  end;

  Liste := FService.ListerTaches(stTerminee);
  try
    AssertEquals('Doit avoir 1 tâche terminée', 1, Liste.Count);
  finally
    Liste.Free;
  end;
end;

procedure TTestTodoService.TestSupprimerTache;
var
  ID: TID;
  Tache: TOrmTache;
begin
  // Créer une tâche
  ID := FService.CreerTache('Tâche à supprimer', 'Description', prNormale);

  // La supprimer
  AssertTrue('Suppression doit réussir', FService.SupprimerTache(ID));

  // Vérifier qu'elle n'existe plus
  Tache := TOrmTache.Create(FRest, ID);
  try
    AssertEquals('ID doit être 0', 0, Tache.ID);
  finally
    Tache.Free;
  end;
end;

initialization
  RegisterTest(TTestTodoService);

end.
```

**Exécuter les tests :**

```pascal
program RunTests;

{$mode objfpc}{$H+}

uses
  fpcunit, testrunner, testregistry,
  TestsTodo;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
```

### 5. Configuration externalisée

```pascal
unit Configuration;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, IniFiles,
  mormot.core.base;

type
  TConfiguration = class
  private
    FIni: TIniFile;
    FPort: String;
    FDatabasePath: String;
    FLogLevel: String;
    FMaxConnections: Integer;
  public
    constructor Create(const FichierConfig: String);
    destructor Destroy; override;

    property Port: String read FPort;
    property DatabasePath: String read FDatabasePath;
    property LogLevel: String read FLogLevel;
    property MaxConnections: Integer read FMaxConnections;
  end;

implementation

constructor TConfiguration.Create(const FichierConfig: String);
begin
  inherited Create;

  if not FileExists(FichierConfig) then
    raise Exception.CreateFmt('Fichier de configuration non trouvé : %s', [FichierConfig]);

  FIni := TIniFile.Create(FichierConfig);

  // Lire les paramètres
  FPort := FIni.ReadString('Server', 'Port', '8080');
  FDatabasePath := FIni.ReadString('Database', 'Path', 'data.db');
  FLogLevel := FIni.ReadString('Logging', 'Level', 'INFO');
  FMaxConnections := FIni.ReadInteger('Server', 'MaxConnections', 100);
end;

destructor TConfiguration.Destroy;
begin
  FIni.Free;
  inherited Destroy;
end;

end.
```

**Fichier config.ini :**

```ini
[Server]
Port=8080
MaxConnections=100
Timeout=30000

[Database]
Path=data/myapp.db
BackupEnabled=true
BackupInterval=3600

[Logging]
Level=INFO
Path=logs/
MaxFileSize=10485760

[Security]
EnableAuth=true
TokenExpiration=3600
```

**Utilisation :**

```pascal
program ServeurAvecConfig;

uses
  Configuration;

var
  Config: TConfiguration;
  Serveur: TRestHttpServer;
begin
  Config := TConfiguration.Create('config.ini');
  try
    WriteLn('Configuration chargée :');
    WriteLn('  Port : ', Config.Port);
    WriteLn('  Base de données : ', Config.DatabasePath);
    WriteLn('  Niveau de log : ', Config.LogLevel);

    // Créer le serveur avec la config
    Serveur := TRestHttpServer.Create(Config.Port, [Rest], '+', HTTP_DEFAULT_MODE);
    // ...
  finally
    Config.Free;
  end;
end.
```

## Migration de données

### Script de migration

```pascal
unit MigrationDatabase;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  mormot.orm.core,
  mormot.rest.server;

type
  TMigration = class
  private
    FRest: TRestServer;
    FVersionActuelle: Integer;
  public
    constructor Create(Rest: TRestServer);

    procedure Migrer;
    procedure MigrerVers(Version: Integer);

    // Migrations spécifiques
    procedure Migration_V1_to_V2;
    procedure Migration_V2_to_V3;
  end;

implementation

constructor TMigration.Create(Rest: TRestServer);
begin
  inherited Create;
  FRest := Rest;

  // Lire la version actuelle depuis la base
  // (à implémenter selon votre système de versioning)
  FVersionActuelle := 1;
end;

procedure TMigration.Migrer;
const
  VERSION_CIBLE = 3;
var
  i: Integer;
begin
  WriteLn('Version actuelle de la base : ', FVersionActuelle);
  WriteLn('Version cible : ', VERSION_CIBLE);

  if FVersionActuelle >= VERSION_CIBLE then
  begin
    WriteLn('Base de données à jour');
    Exit;
  end;

  // Appliquer les migrations une par une
  for i := FVersionActuelle + 1 to VERSION_CIBLE do
  begin
    WriteLn('Migration vers version ', i, '...');
    MigrerVers(i);
  end;

  WriteLn('Migration terminée avec succès');
end;

procedure TMigration.MigrerVers(Version: Integer);
begin
  case Version of
    2: Migration_V1_to_V2;
    3: Migration_V2_to_V3;
  else
    raise Exception.CreateFmt('Version de migration inconnue : %d', [Version]);
  end;

  // Mettre à jour le numéro de version
  FVersionActuelle := Version;
  // Sauvegarder dans la base...
end;

procedure TMigration.Migration_V1_to_V2;
begin
  WriteLn('  Ajout de la colonne DateModification...');
  FRest.ExecuteDirect('ALTER TABLE Utilisateur ADD COLUMN DateModification INTEGER');

  WriteLn('  Mise à jour des dates...');
  FRest.ExecuteDirect('UPDATE Utilisateur SET DateModification = DateInscription');

  WriteLn('  Migration V1->V2 terminée');
end;

procedure TMigration.Migration_V2_to_V3;
begin
  WriteLn('  Création de la table Categories...');
  FRest.ExecuteDirect(
    'CREATE TABLE Categories (' +
    '  ID INTEGER PRIMARY KEY,' +
    '  Nom TEXT NOT NULL,' +
    '  Description TEXT' +
    ')'
  );

  WriteLn('  Ajout de catégories par défaut...');
  FRest.ExecuteDirect('INSERT INTO Categories (Nom) VALUES (''Général'')');
  FRest.ExecuteDirect('INSERT INTO Categories (Nom) VALUES (''Important'')');

  WriteLn('  Migration V2->V3 terminée');
end;

end.
```

## Monitoring et statistiques

### Collecte de métriques

```pascal
unit Metriques;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, SyncObjs,
  mormot.core.base,
  mormot.core.perf;

type
  TMetriques = class
  private
    FLock: TCriticalSection;
    FNbRequetes: Int64;
    FNbErreurs: Int64;
    FTempsTotal: Int64;
    FDebutPeriode: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    procedure EnregistrerRequete(TempsMs: Int64; Erreur: Boolean = False);
    procedure Reinitialiser;

    function ObtenirStatistiques: RawUtf8;
    function RequetesParSeconde: Double;
    function TempsReponsesMoyen: Double;
    function TauxErreur: Double;
  end;

var
  Metriques: TMetriques;

implementation

constructor TMetriques.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  Reinitialiser;
end;

destructor TMetriques.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TMetriques.EnregistrerRequete(TempsMs: Int64; Erreur: Boolean);
begin
  FLock.Enter;
  try
    Inc(FNbRequetes);
    Inc(FTempsTotal, TempsMs);
    if Erreur then
      Inc(FNbErreurs);
  finally
    FLock.Leave;
  end;
end;

procedure TMetriques.Reinitialiser;
begin
  FLock.Enter;
  try
    FNbRequetes := 0;
    FNbErreurs := 0;
    FTempsTotal := 0;
    FDebutPeriode := Now;
  finally
    FLock.Leave;
  end;
end;

function TMetriques.ObtenirStatistiques: RawUtf8;
var
  Doc: TDocVariantData;
begin
  FLock.Enter;
  try
    Doc.InitObject([
      'requetes', FNbRequetes,
      'erreurs', FNbErreurs,
      'requetes_par_sec', RequetesParSeconde,
      'temps_moyen_ms', TempsReponsesMoyen,
      'taux_erreur_pct', TauxErreur,
      'uptime_sec', SecondsBetween(Now, FDebutPeriode)
    ]);
    Result := Doc.ToJson;
  finally
    FLock.Leave;
  end;
end;

function TMetriques.RequetesParSeconde: Double;
var
  Duree: Double;
begin
  Duree := SecondsBetween(Now, FDebutPeriode);
  if Duree > 0 then
    Result := FNbRequetes / Duree
  else
    Result := 0;
end;

function TMetriques.TempsReponsesMoyen: Double;
begin
  if FNbRequetes > 0 then
    Result := FTempsTotal / FNbRequetes
  else
    Result := 0;
end;

function TMetriques.TauxErreur: Double;
begin
  if FNbRequetes > 0 then
    Result := (FNbErreurs * 100.0) / FNbRequetes
  else
    Result := 0;
end;

initialization
  Metriques := TMetriques.Create;

finalization
  Metriques.Free;

end.
```

**Endpoint de monitoring :**

```pascal
// Ajouter un endpoint pour les statistiques
type
  IMonitoringService = interface(IInvokable)
    ['{99999999-8888-7777-6666-555555555555}']
    function ObtenirStatistiques: RawUtf8;
    function ObtenirSante: RawUtf8;
  end;

type
  TMonitoringService = class(TInjectableObjectRest, IMonitoringService)
  public
    function ObtenirStatistiques: RawUtf8;
    function ObtenirSante: RawUtf8;
  end;

function TMonitoringService.ObtenirStatistiques: RawUtf8;
begin
  Result := Metriques.ObtenirStatistiques;
end;

function TMonitoringService.ObtenirSante: RawUtf8;
var
  Doc: TDocVariantData;
  MemUsage: Int64;
begin
  // Vérifications de santé
  MemUsage := GetHeapStatus.TotalAllocated;

  Doc.InitObject([
    'status', 'OK',
    'timestamp', DateTimeToStr(Now),
    'memoire_mb', MemUsage div (1024*1024),
    'base_connectee', Server.Orm.Connected,
    'version', '1.0.0'
  ]);

  Result := Doc.ToJson;
end;
```

**Accès :**
```
GET http://localhost:8080/api/Monitoring/ObtenirStatistiques
GET http://localhost:8080/api/Monitoring/ObtenirSante
```

## Sécurisation avancée

### HTTPS/TLS

```pascal
program ServeurHTTPS;

{$mode objfpc}{$H+}

uses
  SysUtils,
  mormot.core.base,
  mormot.net.sock,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.http.server;

var
  Model: TOrmModel;
  Rest: TRestServerDB;
  Serveur: TRestHttpServer;
begin
  Model := TOrmModel.Create([TOrmUtilisateur], 'root');
  Rest := TRestServerDB.Create(Model, 'data.db');
  try
    Rest.CreateMissingTables;

    // Créer le serveur HTTPS
    Serveur := TRestHttpServer.Create('443', [Rest], '+', useHttpApiRegisteringURI, 32);
    try
      // Configurer TLS/SSL
      {$IFDEF WINDOWS}
      // Windows : utiliser le certificat du magasin
      Serveur.HttpServer.RegisterCompress(CompressGzip);
      // Le certificat doit être installé via netsh :
      // netsh http add sslcert ipport=0.0.0.0:443 certhash=THUMBPRINT appid={GUID}
      {$ENDIF}

      {$IFDEF UNIX}
      // Linux : charger le certificat depuis des fichiers
      Serveur.UseSSL('/etc/ssl/certs/server.crt', '/etc/ssl/private/server.key');
      {$ENDIF}

      WriteLn('Serveur HTTPS démarré sur le port 443');
      WriteLn('URL : https://localhost/root');
      ReadLn;
    finally
      Serveur.Free;
    end;
  finally
    Rest.Free;
    Model.Free;
  end;
end.
```

### Rate Limiting (limitation de débit)

```pascal
unit RateLimiter;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Contnrs, SyncObjs,
  mormot.core.base;

type
  TRateLimiter = class
  private
    FLock: TCriticalSection;
    FRequetes: TStringList;  // IP -> Compteur
    FLimiteParMinute: Integer;
  public
    constructor Create(LimiteParMinute: Integer = 60);
    destructor Destroy; override;

    function EstAutorise(const IP: String): Boolean;
    procedure NettoyerAnciennesEntrees;
  end;

implementation

constructor TRateLimiter.Create(LimiteParMinute: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FRequetes := TStringList.Create;
  FLimiteParMinute := LimiteParMinute;
end;

destructor TRateLimiter.Destroy;
begin
  FRequetes.Free;
  FLock.Free;
  inherited Destroy;
end;

function TRateLimiter.EstAutorise(const IP: String): Boolean;
var
  Index: Integer;
  Compteur: Integer;
begin
  FLock.Enter;
  try
    Index := FRequetes.IndexOf(IP);

    if Index < 0 then
    begin
      // Première requête de cette IP
      FRequetes.AddObject(IP, TObject(PtrInt(1)));
      Result := True;
    end
    else
    begin
      // IP déjà vue
      Compteur := PtrInt(FRequetes.Objects[Index]);

      if Compteur < FLimiteParMinute then
      begin
        Inc(Compteur);
        FRequetes.Objects[Index] := TObject(PtrInt(Compteur));
        Result := True;
      end
      else
        Result := False;  // Limite atteinte
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRateLimiter.NettoyerAnciennesEntrees;
begin
  FLock.Enter;
  try
    // Réinitialiser tous les compteurs
    // À appeler toutes les minutes via un timer
    FRequetes.Clear;
  finally
    FLock.Leave;
  end;
end;

end.
```

**Intégration dans le serveur :**

```pascal
var
  RateLimiter: TRateLimiter;

// Dans l'initialisation
RateLimiter := TRateLimiter.Create(60);  // 60 requêtes par minute

// Dans le gestionnaire de requêtes
function VerifierRateLimit(const Context: THttpServerRequest): Boolean;
begin
  Result := RateLimiter.EstAutorise(Context.RemoteIP);
  if not Result then
  begin
    Context.OutStatus := HTTP_TOOMANYREQUESTS;
    Context.OutContent := '{"erreur":"Trop de requêtes. Réessayez plus tard."}';
  end;
end;
```

## Intégration avec d'autres services

### Envoi d'emails

```pascal
unit ServiceEmail;

{$mode objfpc}{$H+}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.net.client;

type
  IEmailService = interface(IInvokable)
    ['{AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE}']
    function EnvoyerEmail(const Destinataire, Sujet, Corps: RawUtf8): Boolean;
  end;

  TEmailService = class(TInterfacedObject, IEmailService)
  private
    FServeurSMTP: RawUtf8;
    FPort: Integer;
    FUtilisateur: RawUtf8;
    FMotDePasse: RawUtf8;
  public
    constructor Create(const ServeurSMTP: RawUtf8; Port: Integer;
                      const Utilisateur, MotDePasse: RawUtf8);

    function EnvoyerEmail(const Destinataire, Sujet, Corps: RawUtf8): Boolean;
  end;

implementation

uses
  mormot.net.smtp;

constructor TEmailService.Create(const ServeurSMTP: RawUtf8; Port: Integer;
                                 const Utilisateur, MotDePasse: RawUtf8);
begin
  inherited Create;
  FServeurSMTP := ServeurSMTP;
  FPort := Port;
  FUtilisateur := Utilisateur;
  FMotDePasse := MotDePasse;
end;

function TEmailService.EnvoyerEmail(const Destinataire, Sujet, Corps: RawUtf8): Boolean;
var
  SMTP: TSMTPConnection;
begin
  SMTP := TSMTPConnection.Create;
  try
    SMTP.Host := FServeurSMTP;
    SMTP.Port := IntToStr(FPort);
    SMTP.User := FUtilisateur;
    SMTP.Pass := FMotDePasse;

    Result := SMTP.SendEmail(
      FUtilisateur,        // From
      Destinataire,        // To
      Sujet,               // Subject
      Corps                // Body
    );
  finally
    SMTP.Free;
  end;
end;

end.
```

### Webhooks

```pascal
unit ServiceWebhook;

{$mode objfpc}{$H+}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.net.client;

type
  IWebhookService = interface(IInvokable)
    ['{FFFFFFFF-0000-1111-2222-333333333333}']
    function EnvoyerWebhook(const URL: RawUtf8; const Donnees: RawUtf8): Boolean;
  end;

  TWebhookService = class(TInterfacedObject, IWebhookService)
  public
    function EnvoyerWebhook(const URL: RawUtf8; const Donnees: RawUtf8): Boolean;
  end;

implementation

uses
  mormot.net.http;

function TWebhookService.EnvoyerWebhook(const URL: RawUtf8;
                                        const Donnees: RawUtf8): Boolean;
var
  Client: THttpClientSocket;
  Reponse: RawUtf8;
  Code: Integer;
begin
  Client := THttpClientSocket.Create;
  try
    // Envoyer les données en POST
    Code := Client.Post(URL, Donnees, 'application/json', Reponse);
    Result := (Code >= 200) and (Code < 300);
  finally
    Client.Free;
  end;
end;

end.
```

## Exemples de clients

### Client Python

```python
import requests
import json

class MormotClient:
    def __init__(self, base_url, username=None, password=None):
        self.base_url = base_url
        self.session = requests.Session()

        # Authentification si nécessaire
        if username and password:
            self.login(username, password)

    def login(self, username, password):
        """Authentification sur le serveur"""
        response = self.session.get(
            f"{self.base_url}/auth",
            params={"username": username, "password": password}
        )
        if response.status_code == 200:
            data = response.json()
            self.session.headers['Authorization'] = f"Bearer {data['sessionID']}"
            return True
        return False

    def get_all(self, table_name):
        """Récupérer tous les enregistrements d'une table"""
        response = self.session.get(f"{self.base_url}/{table_name}")
        return response.json()

    def get_by_id(self, table_name, record_id):
        """Récupérer un enregistrement par ID"""
        response = self.session.get(f"{self.base_url}/{table_name}/{record_id}")
        return response.json()

    def create(self, table_name, data):
        """Créer un nouvel enregistrement"""
        response = self.session.post(
            f"{self.base_url}/{table_name}",
            json=data
        )
        return response.json()

    def update(self, table_name, record_id, data):
        """Mettre à jour un enregistrement"""
        response = self.session.put(
            f"{self.base_url}/{table_name}/{record_id}",
            json=data
        )
        return response.status_code == 200

    def delete(self, table_name, record_id):
        """Supprimer un enregistrement"""
        response = self.session.delete(
            f"{self.base_url}/{table_name}/{record_id}"
        )
        return response.status_code == 200

# Utilisation
client = MormotClient("http://localhost:8080/root")

# Créer un utilisateur
user = client.create("Utilisateur", {
    "Nom": "Dupont",
    "Email": "dupont@example.com",
    "Age": 30
})
print(f"Utilisateur créé avec ID: {user['ID']}")

# Lire tous les utilisateurs
users = client.get_all("Utilisateur")
for u in users:
    print(f"{u['ID']}: {u['Nom']} ({u['Email']})")
```

### Client JavaScript/Node.js

```javascript
class MormotClient {
    constructor(baseUrl, username = null, password = null) {
        this.baseUrl = baseUrl;
        this.sessionId = null;

        if (username && password) {
            this.login(username, password);
        }
    }

    async login(username, password) {
        const response = await fetch(
            `${this.baseUrl}/auth?username=${username}&password=${password}`
        );
        const data = await response.json();
        this.sessionId = data.sessionID;
        return this.sessionId !== null;
    }

    async getAll(tableName) {
        const response = await fetch(`${this.baseUrl}/${tableName}`, {
            headers: this.getHeaders()
        });
        return await response.json();
    }

    async getById(tableName, id) {
        const response = await fetch(`${this.baseUrl}/${tableName}/${id}`, {
            headers: this.getHeaders()
        });
        return await response.json();
    }

    async create(tableName, data) {
        const response = await fetch(`${this.baseUrl}/${tableName}`, {
            method: 'POST',
            headers: {
                ...this.getHeaders(),
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(data)
        });
        return await response.json();
    }

    async update(tableName, id, data) {
        const response = await fetch(`${this.baseUrl}/${tableName}/${id}`, {
            method: 'PUT',
            headers: {
                ...this.getHeaders(),
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(data)
        });
        return response.ok;
    }

    async delete(tableName, id) {
        const response = await fetch(`${this.baseUrl}/${tableName}/${id}`, {
            method: 'DELETE',
            headers: this.getHeaders()
        });
        return response.ok;
    }

    getHeaders() {
        const headers = {};
        if (this.sessionId) {
            headers['Authorization'] = `Bearer ${this.sessionId}`;
        }
        return headers;
    }
}

// Utilisation
const client = new MormotClient('http://localhost:8080/root');

// Créer un utilisateur
const user = await client.create('Utilisateur', {
    Nom: 'Dupont',
    Email: 'dupont@example.com',
    Age: 30
});
console.log(`Utilisateur créé avec ID: ${user.ID}`);

// Lire tous les utilisateurs
const users = await client.getAll('Utilisateur');
users.forEach(u => {
    console.log(`${u.ID}: ${u.Nom} (${u.Email})`);
});

// Mettre à jour un utilisateur
await client.update('Utilisateur', 1, {
    Nom: 'Martin',
    Email: 'martin@example.com',
    Age: 31
});

// Supprimer un utilisateur
await client.delete('Utilisateur', 1);
```

### Client C# / .NET

```csharp
using System;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;

public class MormotClient
{
    private readonly HttpClient _httpClient;
    private readonly string _baseUrl;
    private string _sessionId;

    public MormotClient(string baseUrl, string username = null, string password = null)
    {
        _baseUrl = baseUrl;
        _httpClient = new HttpClient();

        if (username != null && password != null)
        {
            Login(username, password).Wait();
        }
    }

    public async Task<bool> Login(string username, string password)
    {
        var response = await _httpClient.GetAsync(
            $"{_baseUrl}/auth?username={username}&password={password}"
        );

        if (response.IsSuccessStatusCode)
        {
            var json = await response.Content.ReadAsStringAsync();
            var data = JsonSerializer.Deserialize<JsonElement>(json);
            _sessionId = data.GetProperty("sessionID").GetString();
            _httpClient.DefaultRequestHeaders.Add("Authorization", $"Bearer {_sessionId}");
            return true;
        }
        return false;
    }

    public async Task<T[]> GetAll<T>(string tableName)
    {
        var response = await _httpClient.GetAsync($"{_baseUrl}/{tableName}");
        var json = await response.Content.ReadAsStringAsync();
        return JsonSerializer.Deserialize<T[]>(json);
    }

    public async Task<T> GetById<T>(string tableName, int id)
    {
        var response = await _httpClient.GetAsync($"{_baseUrl}/{tableName}/{id}");
        var json = await response.Content.ReadAsStringAsync();
        return JsonSerializer.Deserialize<T>(json);
    }

    public async Task<T> Create<T>(string tableName, T data)
    {
        var json = JsonSerializer.Serialize(data);
        var content = new StringContent(json, Encoding.UTF8, "application/json");

        var response = await _httpClient.PostAsync($"{_baseUrl}/{tableName}", content);
        var responseJson = await response.Content.ReadAsStringAsync();
        return JsonSerializer.Deserialize<T>(responseJson);
    }

    public async Task<bool> Update<T>(string tableName, int id, T data)
    {
        var json = JsonSerializer.Serialize(data);
        var content = new StringContent(json, Encoding.UTF8, "application/json");

        var response = await _httpClient.PutAsync($"{_baseUrl}/{tableName}/{id}", content);
        return response.IsSuccessStatusCode;
    }

    public async Task<bool> Delete(string tableName, int id)
    {
        var response = await _httpClient.DeleteAsync($"{_baseUrl}/{tableName}/{id}");
        return response.IsSuccessStatusCode;
    }
}

// Modèle
public class Utilisateur
{
    public int ID { get; set; }
    public string Nom { get; set; }
    public string Email { get; set; }
    public int Age { get; set; }
}

// Utilisation
var client = new MormotClient("http://localhost:8080/root");

// Créer un utilisateur
var user = await client.Create("Utilisateur", new Utilisateur
{
    Nom = "Dupont",
    Email = "dupont@example.com",
    Age = 30
});
Console.WriteLine($"Utilisateur créé avec ID: {user.ID}");

// Lire tous les utilisateurs
var users = await client.GetAll<Utilisateur>("Utilisateur");
foreach (var u in users)
{
    Console.WriteLine($"{u.ID}: {u.Nom} ({u.Email})");
}
```

## Déploiement en production

### Docker

**Dockerfile pour le serveur :**

```dockerfile
# Dockerfile
FROM debian:bullseye-slim

# Installer les dépendances
RUN apt-get update && apt-get install -y \
    libsqlite3-0 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Créer l'utilisateur de l'application
RUN useradd -m -s /bin/bash appuser

# Créer les dossiers nécessaires
RUN mkdir -p /app/data /app/logs && \
    chown -R appuser:appuser /app

# Copier l'exécutable
COPY --chown=appuser:appuser serveur /app/serveur
RUN chmod +x /app/serveur

# Copier la configuration
COPY --chown=appuser:appuser config.ini /app/config.ini

# Passer à l'utilisateur non-root
USER appuser
WORKDIR /app

# Exposer le port
EXPOSE 8080

# Volumes pour les données persistantes
VOLUME ["/app/data", "/app/logs"]

# Commande de démarrage
CMD ["./serveur"]
```

**docker-compose.yml :**

```yaml
version: '3.8'

services:
  api:
    build: .
    container_name: mormot-api
    ports:
      - "8080:8080"
    volumes:
      - ./data:/app/data
      - ./logs:/app/logs
    environment:
      - TZ=Europe/Paris
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/root/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s

  # Reverse proxy Nginx (optionnel)
  nginx:
    image: nginx:alpine
    container_name: mormot-nginx
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
      - ./ssl:/etc/nginx/ssl:ro
    depends_on:
      - api
    restart: unless-stopped
```

**Configuration Nginx (nginx.conf) :**

```nginx
events {
    worker_connections 1024;
}

http {
    upstream mormot_backend {
        server api:8080;
    }

    # Redirection HTTP -> HTTPS
    server {
        listen 80;
        server_name api.example.com;
        return 301 https://$server_name$request_uri;
    }

    # HTTPS
    server {
        listen 443 ssl http2;
        server_name api.example.com;

        ssl_certificate /etc/nginx/ssl/server.crt;
        ssl_certificate_key /etc/nginx/ssl/server.key;
        ssl_protocols TLSv1.2 TLSv1.3;
        ssl_ciphers HIGH:!aNULL:!MD5;

        # Sécurité
        add_header Strict-Transport-Security "max-age=31536000" always;
        add_header X-Frame-Options "SAMEORIGIN" always;
        add_header X-Content-Type-Options "nosniff" always;

        # Rate limiting
        limit_req_zone $binary_remote_addr zone=api_limit:10m rate=10r/s;
        limit_req zone=api_limit burst=20 nodelay;

        location / {
            proxy_pass http://mormot_backend;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;

            # Timeouts
            proxy_connect_timeout 60s;
            proxy_send_timeout 60s;
            proxy_read_timeout 60s;
        }

        # Health check endpoint
        location /health {
            proxy_pass http://mormot_backend/root/health;
            access_log off;
        }
    }
}
```

**Commandes Docker :**

```bash
# Construire l'image
docker-compose build

# Démarrer les services
docker-compose up -d

# Voir les logs
docker-compose logs -f api

# Arrêter les services
docker-compose down

# Redémarrer
docker-compose restart api
```

### Script de déploiement automatisé

**deploy.sh (Linux) :**

```bash
#!/bin/bash

set -e  # Arrêter en cas d'erreur

echo "=== Déploiement de l'API mORMot ==="

# Variables
APP_NAME="mormot-api"
APP_DIR="/opt/$APP_NAME"
BACKUP_DIR="/backup/$APP_NAME"
BUILD_DIR="./build"

# Couleurs
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Fonction d'affichage
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Vérifier les droits sudo
if [ "$EUID" -ne 0 ]; then
    log_error "Ce script doit être exécuté avec sudo"
    exit 1
fi

# Sauvegarder la version actuelle
if [ -d "$APP_DIR" ]; then
    log_info "Sauvegarde de la version actuelle..."
    mkdir -p "$BACKUP_DIR"
    tar -czf "$BACKUP_DIR/backup-$(date +%Y%m%d-%H%M%S).tar.gz" -C "$APP_DIR" .
fi

# Arrêter le service
log_info "Arrêt du service..."
systemctl stop $APP_NAME || true

# Créer le dossier si nécessaire
mkdir -p "$APP_DIR"/{data,logs,config}

# Copier les nouveaux fichiers
log_info "Déploiement des nouveaux fichiers..."
cp "$BUILD_DIR/serveur" "$APP_DIR/"
cp "$BUILD_DIR/config.ini" "$APP_DIR/config/"
chmod +x "$APP_DIR/serveur"

# Changer les permissions
chown -R www-data:www-data "$APP_DIR"

# Migration de base de données
log_info "Vérification de la base de données..."
# Ajouter ici les commandes de migration si nécessaire

# Redémarrer le service
log_info "Démarrage du service..."
systemctl start $APP_NAME

# Vérifier que le service est bien démarré
sleep 3
if systemctl is-active --quiet $APP_NAME; then
    log_info "✓ Déploiement réussi !"
    log_info "  Service actif et fonctionnel"
else
    log_error "✗ Le service n'a pas démarré correctement"
    log_error "  Consultez les logs : journalctl -u $APP_NAME -n 50"
    exit 1
fi

# Afficher le statut
systemctl status $APP_NAME --no-pager

echo ""
log_info "=== Déploiement terminé ==="
```

**deploy.bat (Windows) :**

```batch
@echo off
setlocal enabledelayedexpansion

echo === Deploiement de l'API mORMot ===
echo.

REM Variables
set APP_NAME=mormot-api
set APP_DIR=C:\Services\%APP_NAME%
set BACKUP_DIR=C:\Backups\%APP_NAME%
set BUILD_DIR=.\build

REM Créer les dossiers
if not exist "%APP_DIR%" mkdir "%APP_DIR%"
if not exist "%APP_DIR%\data" mkdir "%APP_DIR%\data"
if not exist "%APP_DIR%\logs" mkdir "%APP_DIR%\logs"
if not exist "%BACKUP_DIR%" mkdir "%BACKUP_DIR%"

REM Sauvegarder la version actuelle
if exist "%APP_DIR%\serveur.exe" (
    echo [INFO] Sauvegarde de la version actuelle...
    set BACKUP_FILE=%BACKUP_DIR%\backup-%date:~-4%%date:~-7,2%%date:~-10,2%-%time:~0,2%%time:~3,2%.zip
    powershell Compress-Archive -Path "%APP_DIR%\*" -DestinationPath "!BACKUP_FILE!" -Force
)

REM Arrêter le service
echo [INFO] Arret du service...
net stop %APP_NAME% 2>nul

REM Copier les nouveaux fichiers
echo [INFO] Deploiement des nouveaux fichiers...
copy /Y "%BUILD_DIR%\serveur.exe" "%APP_DIR%\"
copy /Y "%BUILD_DIR%\config.ini" "%APP_DIR%\"

REM Démarrer le service
echo [INFO] Demarrage du service...
net start %APP_NAME%

REM Vérifier que le service est démarré
timeout /t 3 /nobreak >nul
sc query %APP_NAME% | find "RUNNING" >nul
if %errorlevel% equ 0 (
    echo [INFO] Deploiement reussi !
    echo [INFO]   Service actif et fonctionnel
) else (
    echo [ERROR] Le service n'a pas demarre correctement
    echo [ERROR]   Consultez les logs evenements Windows
    exit /b 1
)

echo.
echo === Deploiement termine ===
pause
```

## Monitoring et alertes

### Intégration Prometheus

```pascal
unit MetriquesPrometheus;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  mormot.core.base;

type
  TMetriquesPrometheus = class
  public
    class function GenererMetriques: RawUtf8;
  end;

implementation

uses
  Metriques;

class function TMetriquesPrometheus.GenererMetriques: RawUtf8;
var
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    // Format Prometheus
    Liste.Add('# HELP api_requetes_total Nombre total de requetes');
    Liste.Add('# TYPE api_requetes_total counter');
    Liste.Add(Format('api_requetes_total %d', [Metriques.NbRequetes]));

    Liste.Add('# HELP api_erreurs_total Nombre total d''erreurs');
    Liste.Add('# TYPE api_erreurs_total counter');
    Liste.Add(Format('api_erreurs_total %d', [Metriques.NbErreurs]));

    Liste.Add('# HELP api_temps_reponse_moyen_ms Temps de reponse moyen en ms');
    Liste.Add('# TYPE api_temps_reponse_moyen_ms gauge');
    Liste.Add(Format('api_temps_reponse_moyen_ms %.2f', [Metriques.TempsReponsesMoyen]));

    Liste.Add('# HELP api_taux_erreur_pct Taux d''erreur en pourcentage');
    Liste.Add('# TYPE api_taux_erreur_pct gauge');
    Liste.Add(Format('api_taux_erreur_pct %.2f', [Metriques.TauxErreur]));

    Result := StringToUtf8(Liste.Text);
  finally
    Liste.Free;
  end;
end;

end.
```

**Endpoint Prometheus :**

```pascal
// Ajouter dans le serveur
type
  IMetriquesService = interface(IInvokable)
    ['{DDDDDDDD-EEEE-FFFF-0000-111111111111}']
    function ObtenirMetriques: RawUtf8;
  end;

type
  TMetriquesService = class(TInterfacedObject, IMetriquesService)
  public
    function ObtenirMetriques: RawUtf8;
  end;

function TMetriquesService.ObtenirMetriques: RawUtf8;
begin
  Result := TMetriquesPrometheus.GenererMetriques;
end;
```

**Configuration Prometheus (prometheus.yml) :**

```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'mormot-api'
    static_configs:
      - targets: ['localhost:8080']
    metrics_path: '/root/Metriques/ObtenirMetriques'
```

### Grafana Dashboard

**Configuration datasource Prometheus dans Grafana :**

```json
{
  "name": "Prometheus",
  "type": "prometheus",
  "url": "http://prometheus:9090",
  "access": "proxy",
  "isDefault": true
}
```

**Exemple de requêtes pour dashboard :**

```
// Requêtes par seconde
rate(api_requetes_total[5m])

// Taux d'erreur
(api_erreurs_total / api_requetes_total) * 100

// Temps de réponse moyen
api_temps_reponse_moyen_ms

// Disponibilité (uptime)
up{job="mormot-api"}
```

## Conclusion et ressources

### Récapitulatif

Dans ce tutoriel, nous avons couvert :

1. **Introduction à mORMot** - Framework complet pour REST/SOA
2. **Installation** - Configuration sur Windows et Ubuntu
3. **Premier serveur REST** - Création d'API simples
4. **Services SOA** - Interfaces et implémentations
5. **Authentification** - Sécurisation des APIs
6. **ORM avancé** - Relations, validations, requêtes
7. **Performance** - Optimisations, cache, batch
8. **Déploiement** - Docker, services système
9. **Monitoring** - Logs, métriques, alertes
10. **Clients** - Exemples Python, JavaScript, C#

### Avantages de mORMot

✅ **Performance exceptionnelle** - Parmi les plus rapides du marché  
✅ **Complet** - ORM, REST, SOA, JSON tout intégré  
✅ **Multi-plateforme** - Windows, Linux, BSD, Android  
✅ **Bien documenté** - Documentation extensive  
✅ **Open Source** - Code source disponible  
✅ **Actif** - Communauté et mises à jour régulières

### Quand utiliser mORMot ?

**Idéal pour :**
- APIs REST haute performance
- Applications distribuées SOA
- Microservices
- Backends mobiles
- Systèmes temps réel

**Moins adapté pour :**
- Projets nécessitant beaucoup de dépendances externes
- Équipes débutantes (courbe d'apprentissage)
- Projets nécessitant un écosystème Node.js/Python

### Ressources officielles

**Documentation :**
- Site officiel : https://synopse.info/
- Documentation PDF : https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html
- GitHub : https://github.com/synopse/mORMot2

**Forums et communauté :**
- Forum officiel : https://synopse.info/forum/
- Stack Overflow (tag `mormot`)
- Groupe Telegram

**Exemples de code :**
- https://github.com/synopse/mORMot2/tree/master/ex
- https://github.com/synopse/mORMot2-Samples

### Livres et tutoriels

- **"mORMot in Action"** - Guide pratique complet
- **Blog d'Arnaud Bouchez** - Créateur de mORMot
- **Chaîne YouTube "mORMot Framework"**

### Projets open source utilisant mORMot

1. **RAD Studio IDE** - Embarcadero utilise mORMot
2. **Systèmes de monitoring** - Surveillance infrastructure
3. **APIs bancaires** - Transactions haute performance
4. **Plateformes IoT** - Collecte de données capteurs
5. **ERP/CRM** - Gestion d'entreprise

### Alternatives à considérer

Si mORMot ne convient pas à votre projet :

- **Brook Framework** - Plus simple, moins de fonctionnalités
- **Horse** - Framework Delphi/FreePascal minimaliste
- **fpWeb** - Intégré à FreePascal, basique
- **Node.js + Express** - JavaScript, très populaire
- **FastAPI** - Python, moderne et performant
- **Spring Boot** - Java, très complet

### Prochaines étapes

Après avoir maîtrisé mORMot, explorez :

1. **WebSockets avec mORMot** - Communication temps réel
2. **GraphQL avec mORMot** - Alternative à REST
3. **Microservices** - Architecture distribuée
4. **Message queues** - RabbitMQ, Redis avec mORMot
5. **Tests de charge** - Apache JMeter, k6
6. **CI/CD** - Automatisation complète

### Exemple complet final

Voici un serveur REST complet production-ready :

```pascal
program ServeurProductionReady;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, BaseUnix,
  {$ENDIF}
  SysUtils,
  mormot.core.base,
  mormot.core.log,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.http.server,
  mormot.db.raw.sqlite3,
  Configuration,
  ModelUtilisateurs,
  ServicesUtilisateurs;

var
  Config: TConfiguration;
  Model: TOrmModel;
  Rest: TRestServerDB;
  Http: TRestHttpServer;

procedure ConfigurerLogs;
begin
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := True;
  TSynLog.Family.DestinationPath := Config.LogPath;
end;

procedure ConfigurerServeur;
begin
  Model := TOrmModel.Create([TOrmUtilisateur], 'api');
  Rest := TRestServerDB.Create(Model, Config.DatabasePath);
  Rest.CreateMissingTables;

  // Authentification
  Rest.HandleAuthentication := True;

  // Services
  Rest.ServiceDefine(TUtilisateurService, [IUtilisateurService], sicShared);

  // Compression
  Http := TRestHttpServer.Create(Config.Port, [Rest], '+', HTTP_DEFAULT_MODE);
  Http.Compression := [hcDeflate, hcGzip];

  // CORS
  Http.AccessControlAllowOrigin := '*';
end;

begin
  try
    WriteLn('=== Démarrage du serveur mORMot ===');

    // Charger la configuration
    Config := TConfiguration.Create('config.ini');
    try
      WriteLn('Configuration chargée depuis config.ini');

      // Configurer les logs
      ConfigurerLogs;
      TSynLog.Add.Log(sllInfo, 'Serveur en cours de démarrage...');

      // Configurer et démarrer le serveur
      ConfigurerServeur;

      WriteLn('Serveur démarré avec succès');
      WriteLn('Port : ', Config.Port);
      WriteLn('Base de données : ', Config.DatabasePath);
      WriteLn('Logs : ', Config.LogPath);
      WriteLn('');
      WriteLn('URLs disponibles :');
      WriteLn('  - API : http://localhost:', Config.Port, '/api');
      WriteLn('  - Health : http://localhost:', Config.Port, '/api/health');
      WriteLn('  - Metrics : http://localhost:', Config.Port, '/api/metrics');
      WriteLn('');
      WriteLn('Appuyez sur Ctrl+C pour arrêter...');

      TSynLog.Add.Log(sllInfo, 'Serveur opérationnel');

      {$IFDEF UNIX}
      while not Terminated do
        Sleep(1000);
      {$ELSE}
      ReadLn;
      {$ENDIF}

      TSynLog.Add.Log(sllInfo, 'Arrêt du serveur demandé');
      WriteLn('Arrêt du serveur...');

    finally
      Http.Free;
      Rest.Free;
      Model.Free;
      Config.Free;
    end;

    TSynLog.Add.Log(sllInfo, 'Serveur arrêté proprement');
    WriteLn('Serveur arrêté');

  except
    on E: Exception do
    begin
      WriteLn('ERREUR FATALE : ', E.Message);
      TSynLog.Add.Log(sllError, 'Erreur fatale : %', [E.Message]);
      ExitCode := 1;
    end;
  end;
end.
```

### Mot de la fin

mORMot est un framework extrêmement puissant qui peut sembler complexe au premier abord, mais qui offre des possibilités immenses une fois maîtrisé. Il est particulièrement adapté pour les développeurs FreePascal/Lazarus qui souhaitent créer des applications professionnelles haute performance.

**Conseil final :** Commencez petit, expérimentez avec les exemples, et augmentez progressivement la complexité de vos projets. La communauté mORMot est très active et prête à aider !

Bon développement avec mORMot ! 🚀

⏭️ [gRPC et Protocol Buffers](/10-programmation-reseau-avancee/10-grpc-protocol-buffers.md)
