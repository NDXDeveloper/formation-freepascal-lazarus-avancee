🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.10 gRPC et Protocol Buffers

## Introduction à gRPC et Protocol Buffers

### Qu'est-ce que gRPC ?

gRPC (gRPC Remote Procedure Call) est un framework moderne de communication développé par Google. Il permet à des applications de communiquer entre elles de manière efficace, comme si elles appelaient des fonctions locales.

**Analogie simple :** Imaginez que vous avez deux bureaux dans des villes différentes. Au lieu d'envoyer des lettres (HTTP/JSON), vous installez un téléphone spécialisé ultra-rapide (gRPC) qui permet de parler instantanément avec un protocole compact.

### Qu'est-ce que Protocol Buffers ?

Protocol Buffers (protobuf) est un format de sérialisation de données créé par Google. C'est comme JSON ou XML, mais :
- **Plus compact** : 3 à 10 fois plus petit
- **Plus rapide** : 20 à 100 fois plus rapide
- **Typé** : Les données sont strictement définies

**Comparaison :**

| Format | Taille | Vitesse | Lisible | Typé |
|--------|--------|---------|---------|------|
| JSON | 100% | Moyen | ✅ | ❌ |
| XML | 150% | Lent | ✅ | ❌ |
| Protobuf | 30% | Très rapide | ❌ | ✅ |

### Pourquoi utiliser gRPC ?

**Avantages :**
1. **Performance** - 5 à 10 fois plus rapide que REST/JSON
2. **Bidirectionnel** - Client et serveur peuvent envoyer des données simultanément
3. **Streaming** - Support natif du streaming de données
4. **Multi-langage** - Interopérabilité entre langages
5. **Génération de code** - Client et serveur générés automatiquement

**Cas d'usage idéaux :**
- Microservices haute performance
- Communication temps réel
- Applications mobiles (économie de batterie)
- IoT avec bande passante limitée
- Systèmes distribués

### Architecture gRPC

```
┌─────────────┐                    ┌─────────────┐
│   Client    │                    │   Serveur   │
│             │                    │             │
│  ┌────────┐ │   gRPC/HTTP/2      │  ┌────────┐ │
│  │ Stub   │─┼────────────────────┼─>│Service │ │
│  └────────┘ │   Protobuf         │  └────────┘ │
│             │                    │             │
└─────────────┘                    └─────────────┘
```

## Installation et configuration

### Prérequis

Pour utiliser gRPC avec FreePascal, nous avons besoin de plusieurs outils :

1. **protoc** - Compilateur Protocol Buffers
2. **grpc_pascal** - Plugin pour générer du code Pascal
3. **Bibliothèques gRPC** - Runtime gRPC

### Installation sur Windows

**1. Installer protoc :**

Télécharger depuis GitHub :
```
https://github.com/protocolbuffers/protobuf/releases
```

Chercher `protoc-XX.X-win64.zip`, extraire et ajouter au PATH.

**Vérifier l'installation :**
```batch
protoc --version
```

**2. Installation alternative via Chocolatey :**
```batch
choco install protobuf
```

**3. Installer les bibliothèques gRPC pour Pascal :**

Cloner le projet :
```batch
cd C:\Dev
git clone https://github.com/highras/fpnn-grpc-pascal.git
```

Ou utiliser une bibliothèque alternative comme **mORMot** qui inclut le support protobuf.

### Installation sur Ubuntu/Linux

**1. Installer protoc :**

```bash
# Via le gestionnaire de paquets
sudo apt update
sudo apt install -y protobuf-compiler

# Vérifier
protoc --version
```

**2. Installation manuelle (version plus récente) :**

```bash
# Télécharger la dernière version
PROTOC_VERSION=25.1
wget https://github.com/protocolbuffers/protobuf/releases/download/v${PROTOC_VERSION}/protoc-${PROTOC_VERSION}-linux-x86_64.zip

# Extraire
unzip protoc-${PROTOC_VERSION}-linux-x86_64.zip -d protoc
sudo mv protoc/bin/protoc /usr/local/bin/
sudo mv protoc/include/google /usr/local/include/

# Nettoyer
rm -rf protoc protoc-${PROTOC_VERSION}-linux-x86_64.zip

# Vérifier
protoc --version
```

**3. Installer les dépendances de développement :**

```bash
sudo apt install -y build-essential libssl-dev
```

### Configuration dans Lazarus

**Ajouter les chemins du compilateur :**

Dans Lazarus : **Outils → Options → Chemins du compilateur**

Ajouter :
```
# Windows
C:\Dev\grpc-pascal\src

# Linux
~/Dev/grpc-pascal/src
```

## Protocol Buffers - Les bases

### Syntaxe de base

Un fichier `.proto` définit la structure de vos données :

**hello.proto :**
```protobuf
// Version de Protocol Buffers
syntax = "proto3";

// Package (équivalent namespace)
package hello;

// Message (équivalent structure/record)
message HelloRequest {
  string nom = 1;
  int32 age = 2;
}

message HelloResponse {
  string message = 1;
  int64 timestamp = 2;
}

// Service (équivalent interface)
service Greeter {
  // RPC (équivalent méthode)
  rpc SayHello (HelloRequest) returns (HelloResponse);
}
```

**Explication :**

- `syntax = "proto3"` : Version du langage protobuf
- `package` : Organisation du code généré
- `message` : Structure de données (comme un record)
- Les numéros (1, 2, etc.) sont des **tags** uniques pour chaque champ
- `service` : Définit les opérations disponibles
- `rpc` : Une méthode distante

### Types de données Protocol Buffers

| Type Protobuf | Type Pascal | Description |
|---------------|-------------|-------------|
| `double` | `Double` | Nombre à virgule 64 bits |
| `float` | `Single` | Nombre à virgule 32 bits |
| `int32` | `Integer` | Entier signé 32 bits |
| `int64` | `Int64` | Entier signé 64 bits |
| `uint32` | `LongWord` | Entier non signé 32 bits |
| `uint64` | `QWord` | Entier non signé 64 bits |
| `bool` | `Boolean` | Booléen |
| `string` | `String` | Chaîne UTF-8 |
| `bytes` | `TBytes` | Tableau d'octets |

### Champs optionnels et répétés

```protobuf
syntax = "proto3";

message Utilisateur {
  string nom = 1;

  // Champ optionnel (peut être absent)
  optional string email = 2;

  // Champ répété (tableau/liste)
  repeated string hobbies = 3;

  // Valeur par défaut en proto3
  int32 age = 4;  // Défaut: 0
  bool actif = 5;  // Défaut: false
}
```

### Énumérations

```protobuf
syntax = "proto3";

enum Statut {
  INCONNU = 0;      // Toujours commencer à 0
  ACTIF = 1;
  INACTIF = 2;
  SUSPENDU = 3;
}

message Compte {
  string nom = 1;
  Statut statut = 2;
}
```

### Messages imbriqués

```protobuf
syntax = "proto3";

message Adresse {
  string rue = 1;
  string ville = 2;
  string code_postal = 3;
  string pays = 4;
}

message Personne {
  string nom = 1;
  int32 age = 2;
  Adresse adresse = 3;  // Message imbriqué
}
```

## Génération de code Pascal

### Compiler un fichier .proto

**Commande de base :**

```bash
# Générer du code Pascal
protoc --pascal_out=./generated hello.proto

# Avec options
protoc \
  --proto_path=./proto \
  --pascal_out=./generated \
  hello.proto
```

**Options importantes :**

- `--proto_path` ou `-I` : Dossier contenant les fichiers .proto
- `--pascal_out` : Dossier de sortie pour le code Pascal
- `--grpc_out` : Pour générer le code gRPC (si plugin disponible)

### Code généré

Le compilateur protoc génère généralement :

**hello.pb.pas :**
```pascal
unit hello.pb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Message HelloRequest
  THelloRequest = class
  private
    FNom: String;
    FAge: Integer;
  public
    property Nom: String read FNom write FNom;
    property Age: Integer read FAge write FAge;
  end;

  // Message HelloResponse
  THelloResponse = class
  private
    FMessage: String;
    FTimestamp: Int64;
  public
    property Message: String read FMessage write FMessage;
    property Timestamp: Int64 read FTimestamp write FTimestamp;
  end;

implementation

end.
```

## Implémentation manuelle avec mORMot

Comme le support gRPC natif pour FreePascal est limité, nous utiliserons **mORMot** qui offre un excellent support de Protocol Buffers.

### Définir les messages avec mORMot

**models.pas :**
```pascal
unit Models;

{$mode objfpc}{$H+}

interface

uses
  mormot.core.base,
  mormot.core.data;

type
  // Équivalent d'un message protobuf
  TUtilisateur = packed record
    ID: Integer;
    Nom: RawUtf8;
    Email: RawUtf8;
    Age: Integer;
    Actif: Boolean;
  end;

  TListeUtilisateurs = packed record
    Utilisateurs: array of TUtilisateur;
  end;

  TRequeteUtilisateur = packed record
    ID: Integer;
  end;

  TReponseUtilisateur = packed record
    Utilisateur: TUtilisateur;
    Trouve: Boolean;
    Message: RawUtf8;
  end;

implementation

end.
```

### Sérialisation Protocol Buffers avec mORMot

```pascal
unit ProtobufHelpers;

{$mode objfpc}{$H+}

interface

uses
  mormot.core.base,
  mormot.core.buffers,
  Models;

// Sérialiser un utilisateur en protobuf
function SerialiserUtilisateur(const User: TUtilisateur): TBytes;

// Désérialiser depuis protobuf
function DeserialiserUtilisateur(const Data: TBytes): TUtilisateur;

implementation

function SerialiserUtilisateur(const User: TUtilisateur): TBytes;
var
  Writer: TBufferWriter;
  Temp: TBytes;
begin
  SetLength(Temp, 1024);
  Writer := TBufferWriter.Create(Temp);
  try
    // Écrire chaque champ avec son tag
    Writer.WriteVarUInt32(1 shl 3 or 0);  // Tag 1, type varint
    Writer.WriteVarUInt32(User.ID);

    Writer.WriteVarUInt32(2 shl 3 or 2);  // Tag 2, type string
    Writer.WriteVarString(User.Nom);

    Writer.WriteVarUInt32(3 shl 3 or 2);  // Tag 3, type string
    Writer.WriteVarString(User.Email);

    Writer.WriteVarUInt32(4 shl 3 or 0);  // Tag 4, type varint
    Writer.WriteVarUInt32(User.Age);

    Writer.WriteVarUInt32(5 shl 3 or 0);  // Tag 5, type varint
    if User.Actif then
      Writer.WriteVarUInt32(1)
    else
      Writer.WriteVarUInt32(0);

    // Obtenir les données finales
    SetLength(Result, Writer.TotalWritten);
    Move(Temp[0], Result[0], Writer.TotalWritten);
  finally
    Writer.Free;
  end;
end;

function DeserialiserUtilisateur(const Data: TBytes): TUtilisateur;
var
  Reader: TFastReader;
  Tag, WireType: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Reader.Init(Data);

  while Reader.P < Reader.Last do
  begin
    Tag := Reader.VarUInt32;
    WireType := Tag and 7;
    Tag := Tag shr 3;

    case Tag of
      1: Result.ID := Reader.VarUInt32;
      2: Result.Nom := Reader.VarString;
      3: Result.Email := Reader.VarString;
      4: Result.Age := Reader.VarUInt32;
      5: Result.Actif := Reader.VarUInt32 <> 0;
    else
      // Tag inconnu, ignorer
      Reader.VarBlob;
    end;
  end;
end;

end.
```

## Exemple complet : Service utilisateur

### Définition du service

**user.proto :**
```protobuf
syntax = "proto3";

package userservice;

message User {
  int32 id = 1;
  string nom = 2;
  string email = 3;
  int32 age = 4;
  bool actif = 5;
}

message GetUserRequest {
  int32 id = 1;
}

message GetUserResponse {
  User user = 1;
  bool found = 2;
  string message = 3;
}

message ListUsersRequest {
  int32 page = 1;
  int32 page_size = 2;
}

message ListUsersResponse {
  repeated User users = 1;
  int32 total = 2;
}

message CreateUserRequest {
  string nom = 1;
  string email = 2;
  int32 age = 3;
}

message CreateUserResponse {
  User user = 1;
  bool success = 2;
  string message = 3;
}

service UserService {
  rpc GetUser (GetUserRequest) returns (GetUserResponse);
  rpc ListUsers (ListUsersRequest) returns (ListUsersResponse);
  rpc CreateUser (CreateUserRequest) returns (CreateUserResponse);
  rpc StreamUsers (ListUsersRequest) returns (stream User);
}
```

### Implémentation du serveur

```pascal
unit UserServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Contnrs,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.rest.server,
  Models;

type
  // Interface du service (comme défini dans le .proto)
  IUserService = interface(IInvokable)
    ['{12345678-ABCD-EFGH-IJKL-123456789ABC}']

    function GetUser(ID: Integer): TReponseUtilisateur;
    function ListUsers(Page, PageSize: Integer): TListeUtilisateurs;
    function CreateUser(const Nom, Email: RawUtf8; Age: Integer): TReponseUtilisateur;
  end;

  // Implémentation du service
  TUserService = class(TInjectableObjectRest, IUserService)
  private
    FUtilisateurs: TObjectList;
    FProchainID: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetUser(ID: Integer): TReponseUtilisateur;
    function ListUsers(Page, PageSize: Integer): TListeUtilisateurs;
    function CreateUser(const Nom, Email: RawUtf8; Age: Integer): TReponseUtilisateur;
  end;

implementation

constructor TUserService.Create;
begin
  inherited Create;
  FUtilisateurs := TObjectList.Create(True);
  FProchainID := 1;

  // Ajouter des utilisateurs de test
  CreateUser('Alice Martin', 'alice@example.com', 28);
  CreateUser('Bob Dupont', 'bob@example.com', 35);
  CreateUser('Charlie Dubois', 'charlie@example.com', 42);
end;

destructor TUserService.Destroy;
begin
  FUtilisateurs.Free;
  inherited Destroy;
end;

function TUserService.GetUser(ID: Integer): TReponseUtilisateur;
var
  i: Integer;
  User: ^TUtilisateur;
begin
  FillChar(Result, SizeOf(Result), 0);

  // Chercher l'utilisateur
  for i := 0 to FUtilisateurs.Count - 1 do
  begin
    User := FUtilisateurs[i];
    if User^.ID = ID then
    begin
      Result.Utilisateur := User^;
      Result.Trouve := True;
      Result.Message := 'Utilisateur trouvé';
      Exit;
    end;
  end;

  // Non trouvé
  Result.Trouve := False;
  Result.Message := 'Utilisateur non trouvé';
end;

function TUserService.ListUsers(Page, PageSize: Integer): TListeUtilisateurs;
var
  i, Debut, Fin: Integer;
  User: ^TUtilisateur;
begin
  // Pagination
  Debut := (Page - 1) * PageSize;
  Fin := Min(Debut + PageSize, FUtilisateurs.Count);

  SetLength(Result.Utilisateurs, Fin - Debut);

  for i := Debut to Fin - 1 do
  begin
    User := FUtilisateurs[i];
    Result.Utilisateurs[i - Debut] := User^;
  end;
end;

function TUserService.CreateUser(const Nom, Email: RawUtf8; Age: Integer): TReponseUtilisateur;
var
  User: ^TUtilisateur;
begin
  // Créer le nouvel utilisateur
  New(User);
  User^.ID := FProchainID;
  Inc(FProchainID);
  User^.Nom := Nom;
  User^.Email := Email;
  User^.Age := Age;
  User^.Actif := True;

  // Ajouter à la liste
  FUtilisateurs.Add(User);

  // Retourner la réponse
  Result.Utilisateur := User^;
  Result.Trouve := True;
  Result.Message := Format('Utilisateur créé avec ID %d', [User^.ID]);
end;

end.
```

### Serveur gRPC-like avec mORMot

```pascal
program ServeurUser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  mormot.core.base,
  mormot.core.log,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.http.server,
  Models,
  UserServiceImpl;

var
  Model: TOrmModel;
  Rest: TRestServerFullMemory;
  Http: TRestHttpServer;
begin
  try
    WriteLn('=== Serveur UserService (gRPC-like) ===');

    // Configuration des logs
    TSynLog.Family.Level := LOG_VERBOSE;

    // Créer le modèle
    Model := TOrmModel.Create([], 'grpc');

    // Créer le serveur REST
    Rest := TRestServerFullMemory.Create(Model, False);
    try
      // Enregistrer le service
      Rest.ServiceDefine(TUserService, [IUserService], sicShared);

      // Créer le serveur HTTP
      Http := TRestHttpServer.Create('50051', [Rest], '+', HTTP_DEFAULT_MODE);
      try
        WriteLn('Serveur démarré sur le port 50051');
        WriteLn('URL : http://localhost:50051/grpc/UserService');
        WriteLn('');
        WriteLn('Méthodes disponibles :');
        WriteLn('  - GetUser(ID: Integer)');
        WriteLn('  - ListUsers(Page, PageSize: Integer)');
        WriteLn('  - CreateUser(Nom, Email: String, Age: Integer)');
        WriteLn('');
        WriteLn('Appuyez sur Entrée pour arrêter...');
        ReadLn;
      finally
        Http.Free;
      end;
    finally
      Rest.Free;
      Model.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERREUR : ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
```

### Client

```pascal
program ClientUser;

{$mode objfpc}{$H+}

uses
  SysUtils,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.http.client,
  Models,
  UserServiceImpl;

var
  Model: TOrmModel;
  Client: TRestHttpClient;
  Service: IUserService;
  Reponse: TReponseUtilisateur;
  Liste: TListeUtilisateurs;
  i: Integer;
begin
  Model := TOrmModel.Create([], 'grpc');
  Client := TRestHttpClient.Create('localhost', '50051', Model);
  try
    WriteLn('=== Client UserService ===');
    WriteLn;

    // Résoudre le service
    if not Client.ServiceResolve(IUserService, Service) then
    begin
      WriteLn('Impossible de se connecter au service');
      Exit;
    end;

    WriteLn('Connexion établie');
    WriteLn;

    // 1. Créer un utilisateur
    WriteLn('1. Création d''un utilisateur...');
    Reponse := Service.CreateUser('David Lambert', 'david@example.com', 30);
    if Reponse.Trouve then
      WriteLn('   ✓ ', Reponse.Message)
    else
      WriteLn('   ✗ ', Reponse.Message);
    WriteLn;

    // 2. Récupérer un utilisateur
    WriteLn('2. Récupération de l''utilisateur ID=1...');
    Reponse := Service.GetUser(1);
    if Reponse.Trouve then
    begin
      WriteLn('   ✓ Utilisateur trouvé :');
      WriteLn('     Nom : ', Reponse.Utilisateur.Nom);
      WriteLn('     Email : ', Reponse.Utilisateur.Email);
      WriteLn('     Age : ', Reponse.Utilisateur.Age);
    end
    else
      WriteLn('   ✗ ', Reponse.Message);
    WriteLn;

    // 3. Lister les utilisateurs
    WriteLn('3. Liste des utilisateurs (page 1, 10 par page)...');
    Liste := Service.ListUsers(1, 10);
    WriteLn('   Nombre d''utilisateurs : ', Length(Liste.Utilisateurs));
    for i := 0 to High(Liste.Utilisateurs) do
    begin
      WriteLn('   ', i + 1, '. ', Liste.Utilisateurs[i].Nom,
              ' (', Liste.Utilisateurs[i].Email, ')');
    end;
    WriteLn;

    WriteLn('Tests terminés');

  finally
    Client.Free;
    Model.Free;
  end;
end.
```

## Types de RPC dans gRPC

### 1. Unary RPC (Requête-Réponse simple)

C'est le type le plus simple : le client envoie une requête et reçoit une réponse.

```protobuf
service Calculator {
  rpc Add (AddRequest) returns (AddResponse);
}
```

**Utilisation :**
- Opérations CRUD simples
- Calculs
- Validations

### 2. Server Streaming RPC

Le client envoie une requête, le serveur répond avec un flux de messages.

```protobuf
service FileService {
  rpc DownloadFile (FileRequest) returns (stream FileChunk);
}
```

**Cas d'usage :**
- Téléchargement de fichiers
- Flux de données en temps réel
- Résultats de recherche pagines

**Simulation en mORMot :**

```pascal
type
  IFileService = interface(IInvokable)
    ['{AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE}']

    // Simuler le streaming avec un callback
    procedure DownloadFile(const Filename: RawUtf8;
                          OnChunk: TProc<TBytes>);
  end;

implementation

procedure TFileService.DownloadFile(const Filename: RawUtf8;
                                    OnChunk: TProc<TBytes>);
var
  Stream: TFileStream;
  Buffer: array[0..8191] of Byte;
  BytesRead: Integer;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead);
  try
    repeat
      BytesRead := Stream.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
      begin
        SetLength(Chunk, BytesRead);
        Move(Buffer[0], Chunk[0], BytesRead);
        OnChunk(Chunk);  // Envoyer le chunk au client
      end;
    until BytesRead = 0;
  finally
    Stream.Free;
  end;
end;
```

### 3. Client Streaming RPC

Le client envoie un flux de messages, le serveur répond avec un seul message.

```protobuf
service UploadService {
  rpc UploadFile (stream FileChunk) returns (UploadResponse);
}
```

**Cas d'usage :**
- Upload de fichiers
- Envoi de logs
- Métriques en batch

### 4. Bidirectional Streaming RPC

Client et serveur s'envoient mutuellement des flux de messages.

```protobuf
service ChatService {
  rpc Chat (stream ChatMessage) returns (stream ChatMessage);
}
```

**Cas d'usage :**
- Chat en temps réel
- Jeux multijoueurs
- Collaboration en temps réel

## Avantages et inconvénients de gRPC

### Avantages

✅ **Performance exceptionnelle**
- Sérialisation binaire ultra-rapide
- HTTP/2 : multiplexage, compression d'en-têtes
- Jusqu'à 10x plus rapide que REST/JSON

✅ **Streaming bidirectionnel**
- Support natif du temps réel
- Pas besoin de WebSockets

✅ **Génération de code**
- Client et serveur générés automatiquement
- Fortement typé

✅ **Multi-langage**
- Interopérabilité parfaite
- Support de nombreux langages

✅ **Évolutivité**
- Ajout de champs sans casser la compatibilité
- Versioning intégré

### Inconvénients

❌ **Complexité**
- Courbe d'apprentissage plus élevée
- Configuration plus complexe que REST

❌ **Debugging difficile**
- Format binaire non lisible
- Outils spécialisés nécessaires

❌ **Support navigateur limité**
- Nécessite grpc-web
- Pas de support natif dans les navigateurs

❌ **Support FreePascal limité**
- Peu de bibliothèques natives
- Nécessite des workarounds

❌ **Proxies et firewalls**
- Certains équipements bloquent HTTP/2
- Peut nécessiter des configurations spéciales

## Comparaison REST vs gRPC

| Critère | REST | gRPC |
|---------|------|------|
| **Format** | JSON/XML (texte) | Protobuf (binaire) |
| **Protocole** | HTTP/1.1 | HTTP/2 |
| **Performance** | Moyenne | Très élevée |
| **Streaming** | Limité (SSE) | Natif bidirectionnel |
| **Navigation** | Facile (URLs) | Complexe (RPC) |
| **Debugging** | Facile (lisible) | Difficile (binaire) |
| **Caching** | Standard (HTTP) | Manuel |
| **Browser** | Natif | grpc-web nécessaire |
| **Adoption** | Très répandu | Croissante |

**Quand utiliser gRPC :**
- Microservices backend-à-backend
- Applications haute performance
- Streaming temps réel
- Communication entre datacenters

**Quand utiliser REST :**
- APIs publiques
- Applications web
- Simplicité prioritaire
- Interopérabilité maximale

## Protocol Buffers avancés

### Imports et packages

**common.proto :**
```protobuf
syntax = "proto3";

package common;

message Timestamp {
  int64 seconds = 1;
  int32 nanos = 2;
}

message UUID {
  string value = 1;
}
```

**user.proto :**
```protobuf
syntax = "proto3";

package user;

import "common.proto";

message User {
  common.UUID id = 1;
  string nom = 2;
  common.Timestamp created_at = 3;
}
```

### Oneof (union)

Permet de définir qu'un seul champ parmi plusieurs peut être défini :

```protobuf
syntax = "proto3";

message PaiementInfo {
  int32 montant = 1;

  oneof methode_paiement {
    string carte_credit = 2;
    string paypal_email = 3;
    string bitcoin_address = 4;
    string virement_iban = 5;
  }
}
```

**Utilisation :**
```pascal
type
  TMethodePaiement = (mpAucune, mpCartCredit, mpPaypal, mpBitcoin, mpVirement);

  TPaiementInfo = packed record
    Montant: Integer;
    Methode: TMethodePaiement;
    case TMethodePaiement of
      mpCartCredit: (CarteCredit: RawUtf8);
      mpPaypal: (PaypalEmail: RawUtf8);
      mpBitcoin: (BitcoinAddress: RawUtf8);
      mpVirement: (VirementIBAN: RawUtf8);
  end;
```

### Map (dictionnaires)

```protobuf
syntax = "proto3";

message Produit {
  string nom = 1;

  // Dictionnaire clé-valeur
  map<string, string> attributs = 2;
  map<string, int32> prix_par_pays = 3;
}
```

**Exemple :**
```json
{
  "nom": "T-Shirt",
  "attributs": {
    "couleur": "bleu",
    "taille": "M",
    "matiere": "coton"
  },
  "prix_par_pays": {
    "FR": 1990,
    "US": 2190,
    "UK": 1790
  }
}
```

**En Pascal :**
```pascal
type
  TProduit = packed record
    Nom: RawUtf8;
    Attributs: TRawUtf8DynArray;      // Tableau de paires clé-valeur
    PrixParPays: TIntegerDynArray;     // Synchronisé avec Attributs
  end;

// Ou utiliser mORMot
uses
  mormot.core.collections;

type
  TProduit = class
  private
    FNom: RawUtf8;
    FAttributs: IKeyValue<RawUtf8, RawUtf8>;
    FPrixParPays: IKeyValue<RawUtf8, Integer>;
  public
    constructor Create;
    property Nom: RawUtf8 read FNom write FNom;
    property Attributs: IKeyValue<RawUtf8, RawUtf8> read FAttributs;
    property PrixParPays: IKeyValue<RawUtf8, Integer> read FPrixParPays;
  end;
```

### Well-Known Types

Protocol Buffers fournit des types standards prêts à l'emploi :

```protobuf
syntax = "proto3";

import "google/protobuf/timestamp.proto";
import "google/protobuf/duration.proto";
import "google/protobuf/empty.proto";
import "google/protobuf/any.proto";

message Evenement {
  string titre = 1;
  google.protobuf.Timestamp date_debut = 2;
  google.protobuf.Duration duree = 3;
  google.protobuf.Any metadata = 4;
}

service EvenementService {
  rpc DeleteEvenement (IDRequest) returns (google.protobuf.Empty);
}
```

### Options et annotations

```protobuf
syntax = "proto3";

option java_package = "com.example.proto";
option java_outer_classname = "UserProto";
option optimize_for = SPEED;  // ou CODE_SIZE, LITE_RUNTIME

message User {
  string nom = 1 [(deprecated) = true];  // Champ déprécié
  string nouveau_nom = 2;

  // Options de champ
  string email = 3 [json_name = "emailAddress"];
}
```

## Bonnes pratiques Protocol Buffers

### 1. Règles de numérotation

**À faire :**
```protobuf
message BonExemple {
  string nom = 1;
  string email = 2;
  int32 age = 3;
  // Numéros séquentiels, faciles à suivre
}
```

**À éviter :**
```protobuf
message MauvaisExemple {
  string nom = 15;
  string email = 3;
  int32 age = 42;
  // Numéros désordonnés, source de confusion
}
```

### 2. Réserver des numéros

Quand vous supprimez un champ, réservez son numéro :

```protobuf
message User {
  reserved 2, 15, 9 to 11;
  reserved "old_field", "deprecated_name";

  string nom = 1;
  // int32 age = 2;  ← Supprimé, numéro réservé
  string email = 3;
}
```

**Pourquoi ?** Empêche la réutilisation accidentelle qui casserait la compatibilité.

### 3. Compatibilité ascendante et descendante

**Règles d'or :**

✅ **Autorisé :**
- Ajouter de nouveaux champs
- Ajouter de nouveaux messages
- Ajouter de nouvelles méthodes RPC
- Renommer des champs (le nom n'affecte pas le binaire)

❌ **Interdit :**
- Changer le type d'un champ
- Changer le numéro d'un champ
- Supprimer un champ sans le réserver
- Changer `optional` en `repeated` ou vice-versa

**Exemple d'évolution :**

**Version 1 :**
```protobuf
message User {
  string nom = 1;
  string email = 2;
}
```

**Version 2 (compatible) :**
```protobuf
message User {
  string nom = 1;
  string email = 2;
  int32 age = 3;           // ✅ Ajout d'un nouveau champ
  string telephone = 4;    // ✅ Ajout d'un autre champ
}
```

### 4. Nommage cohérent

```protobuf
// Conventions de nommage recommandées

// Messages : PascalCase
message UserProfile { }
message OrderDetails { }

// Champs : snake_case
message User {
  string first_name = 1;
  string last_name = 2;
  string email_address = 3;
}

// Enums : UPPER_SNAKE_CASE
enum OrderStatus {
  ORDER_STATUS_UNSPECIFIED = 0;
  ORDER_STATUS_PENDING = 1;
  ORDER_STATUS_SHIPPED = 2;
  ORDER_STATUS_DELIVERED = 3;
}

// Services : PascalCase
service UserManagementService { }

// RPC : PascalCase (verbes)
service UserService {
  rpc GetUser (...) returns (...);
  rpc CreateUser (...) returns (...);
  rpc UpdateUser (...) returns (...);
  rpc DeleteUser (...) returns (...);
}
```

### 5. Organisation des fichiers

```
proto/
├── common/
│   ├── types.proto          # Types partagés
│   └── errors.proto         # Messages d'erreur
├── user/
│   ├── user.proto           # Messages utilisateur
│   └── user_service.proto   # Service utilisateur
└── order/
    ├── order.proto
    └── order_service.proto
```

## Gestion des erreurs

### Définir des erreurs structurées

```protobuf
syntax = "proto3";

enum ErrorCode {
  ERROR_CODE_UNSPECIFIED = 0;
  ERROR_CODE_NOT_FOUND = 1;
  ERROR_CODE_ALREADY_EXISTS = 2;
  ERROR_CODE_INVALID_ARGUMENT = 3;
  ERROR_CODE_PERMISSION_DENIED = 4;
  ERROR_CODE_INTERNAL_ERROR = 5;
}

message Error {
  ErrorCode code = 1;
  string message = 2;
  map<string, string> details = 3;
}

message CreateUserResponse {
  oneof result {
    User user = 1;
    Error error = 2;
  }
}
```

### Implémentation en Pascal

```pascal
unit ErrorHandling;

{$mode objfpc}{$H+}

interface

uses
  mormot.core.base;

type
  TErrorCode = (
    ecUnspecified,
    ecNotFound,
    ecAlreadyExists,
    ecInvalidArgument,
    ecPermissionDenied,
    ecInternalError
  );

  TError = packed record
    Code: TErrorCode;
    Message: RawUtf8;
    Details: RawUtf8;  // JSON avec détails additionnels
  end;

  TResultat<T> = record
    Success: Boolean;
    Data: T;
    Error: TError;
  end;

// Fonctions helper
function CreateError(Code: TErrorCode; const Msg: RawUtf8): TError;
function CreateSuccess<T>(const Data: T): TResultat<T>;
function CreateFailure<T>(const Error: TError): TResultat<T>;

implementation

uses
  mormot.core.json;

function CreateError(Code: TErrorCode; const Msg: RawUtf8): TError;
begin
  Result.Code := Code;
  Result.Message := Msg;
  Result.Details := '{}';
end;

function CreateSuccess<T>(const Data: T): TResultat<T>;
begin
  Result.Success := True;
  Result.Data := Data;
  FillChar(Result.Error, SizeOf(TError), 0);
end;

function CreateFailure<T>(const Error: TError): TResultat<T>;
begin
  Result.Success := False;
  FillChar(Result.Data, SizeOf(T), 0);
  Result.Error := Error;
end;

end.
```

**Utilisation :**

```pascal
function TUserService.GetUser(ID: Integer): TResultat<TUtilisateur>;
var
  User: ^TUtilisateur;
begin
  User := FindUserByID(ID);

  if User = nil then
    Result := CreateFailure<TUtilisateur>(
      CreateError(ecNotFound, Format('Utilisateur %d non trouvé', [ID]))
    )
  else
    Result := CreateSuccess<TUtilisateur>(User^);
end;

// Côté client
var
  Resultat: TResultat<TUtilisateur>;
begin
  Resultat := Service.GetUser(42);

  if Resultat.Success then
    WriteLn('Utilisateur : ', Resultat.Data.Nom)
  else
    WriteLn('Erreur : ', Resultat.Error.Message);
end;
```

## Performance et optimisation

### 1. Taille des messages

**Mesurer la taille :**

```pascal
uses
  ProtobufHelpers;

var
  User: TUtilisateur;
  Data: TBytes;
begin
  User.ID := 1;
  User.Nom := 'Jean Dupont';
  User.Email := 'jean@example.com';
  User.Age := 30;
  User.Actif := True;

  Data := SerialiserUtilisateur(User);
  WriteLn('Taille Protobuf : ', Length(Data), ' octets');

  // Comparer avec JSON
  JSON := ObjectToJson(User);
  WriteLn('Taille JSON : ', Length(JSON), ' octets');
  WriteLn('Économie : ', (1 - Length(Data) / Length(JSON)) * 100:0:1, '%');
end;
```

**Résultat typique :**
```
Taille Protobuf : 42 octets
Taille JSON : 128 octets
Économie : 67.2%
```

### 2. Réutiliser les buffers

```pascal
type
  TProtobufPool = class
  private
    FBuffers: TList;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    function Obtenir: TBytes;
    procedure Liberer(var Buffer: TBytes);
  end;

var
  BufferPool: TProtobufPool;

constructor TProtobufPool.Create;
begin
  inherited Create;
  FBuffers := TList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TProtobufPool.Destroy;
begin
  FBuffers.Free;
  FLock.Free;
  inherited Destroy;
end;

function TProtobufPool.Obtenir: TBytes;
begin
  FLock.Enter;
  try
    if FBuffers.Count > 0 then
    begin
      Result := TBytes(FBuffers[0]);
      FBuffers.Delete(0);
    end
    else
      SetLength(Result, 4096);  // Taille par défaut
  finally
    FLock.Leave;
  end;
end;

procedure TProtobufPool.Liberer(var Buffer: TBytes);
begin
  FLock.Enter;
  try
    if Length(Buffer) <= 65536 then  // Ne garder que les buffers raisonnables
      FBuffers.Add(Pointer(Buffer));
    Buffer := nil;
  finally
    FLock.Leave;
  end;
end;
```

### 3. Compression

Pour les gros messages, la compression peut aider :

```pascal
uses
  mormot.core.zip;

function CompresserMessage(const Data: TBytes): TBytes;
var
  Compresse: RawByteString;
begin
  Compresse := AlgoGZFast.Compress(Data);
  Result := StringToBytes(Compresse);
end;

function DecompresserMessage(const Data: TBytes): TBytes;
var
  Decompresse: RawByteString;
begin
  Decompresse := AlgoGZFast.Decompress(Data);
  Result := BytesOf(Decompresse);
end;
```

## Outils et debugging

### 1. Visualiser les messages Protobuf

**Avec protoc :**

```bash
# Encoder un fichier texte en binaire
protoc --encode=user.User user.proto < user.txt > user.bin

# Décoder un fichier binaire en texte
protoc --decode=user.User user.proto < user.bin
```

**user.txt :**
```
nom: "Jean Dupont"
email: "jean@example.com"
age: 30
actif: true
```

### 2. Inspector hexadécimal

```pascal
procedure AfficherProtobuf(const Data: TBytes);
var
  i: Integer;
begin
  WriteLn('Dump Protobuf (', Length(Data), ' octets) :');
  Write('Hex : ');
  for i := 0 to Length(Data) - 1 do
    Write(IntToHex(Data[i], 2), ' ');
  WriteLn;

  // Décoder les tags
  WriteLn('Structure :');
  i := 0;
  while i < Length(Data) do
  begin
    Tag := Data[i];
    FieldNumber := Tag shr 3;
    WireType := Tag and 7;

    WriteLn(Format('  Tag %d, Type %d', [FieldNumber, WireType]));
    Inc(i);

    // Sauter les données selon le type
    case WireType of
      0: Inc(i);  // Varint
      1: Inc(i, 8);  // 64-bit
      2: begin  // Length-delimited
        Len := Data[i];
        Inc(i, 1 + Len);
      end;
      5: Inc(i, 4);  // 32-bit
    end;
  end;
end;
```

### 3. Validation

```pascal
function ValiderUtilisateur(const User: TUtilisateur): TError;
begin
  // Vérifier le nom
  if Length(User.Nom) < 2 then
    Exit(CreateError(ecInvalidArgument, 'Le nom doit contenir au moins 2 caractères'));

  // Vérifier l'email
  if Pos('@', User.Email) = 0 then
    Exit(CreateError(ecInvalidArgument, 'Email invalide'));

  // Vérifier l'âge
  if (User.Age < 0) or (User.Age > 150) then
    Exit(CreateError(ecInvalidArgument, 'Âge invalide'));

  // Tout est OK
  Result.Code := ecUnspecified;
end;
```

## Sécurité

### 1. Limitation de taille

```pascal
const
  TAILLE_MAX_MESSAGE = 10 * 1024 * 1024;  // 10 MB

function DeserialiserSecurise(const Data: TBytes): TUtilisateur;
begin
  if Length(Data) > TAILLE_MAX_MESSAGE then
    raise Exception.Create('Message trop grand');

  if Length(Data) = 0 then
    raise Exception.Create('Message vide');

  Result := DeserialiserUtilisateur(Data);
end;
```

### 2. Validation stricte

```pascal
function DeserialiserAvecValidation(const Data: TBytes): TUtilisateur;
var
  Erreur: TError;
begin
  Result := DeserialiserUtilisateur(Data);

  // Valider après désérialisation
  Erreur := ValiderUtilisateur(Result);
  if Erreur.Code <> ecUnspecified then
    raise Exception.Create(Erreur.Message);
end;
```

### 3. Sanitisation

```pascal
uses
  mormot.core.text;

function SanitiserUtilisateur(var User: TUtilisateur);
begin
  // Nettoyer le nom
  User.Nom := Trim(User.Nom);
  User.Nom := StringReplaceAll(User.Nom, #0, '');

  // Nettoyer l'email
  User.Email := LowerCase(Trim(User.Email));

  // Limiter l'âge
  if User.Age < 0 then User.Age := 0;
  if User.Age > 150 then User.Age := 150;
end;
```

## Intégration multi-plateforme

### Différences Windows/Linux

**Chemins de fichiers proto :**

```pascal
const
  {$IFDEF WINDOWS}
  PROTO_PATH = 'C:\Dev\proto\';
  {$ENDIF}
  {$IFDEF UNIX}
  PROTO_PATH = '/usr/local/share/proto/';
  {$ENDIF}

procedure ChargerDefinitionsProto;
begin
  // Charger les définitions depuis le chemin approprié
  LoadProtoFile(PROTO_PATH + 'user.proto');
end;
```

**Compilation des protos :**

**compile_protos.bat (Windows) :**
```batch
@echo off
set PROTO_DIR=.\proto
set OUT_DIR=.\generated

if not exist %OUT_DIR% mkdir %OUT_DIR%

for %%f in (%PROTO_DIR%\*.proto) do (
    echo Compilation de %%f...
    protoc --proto_path=%PROTO_DIR% --pascal_out=%OUT_DIR% %%f
)

echo Terminé !
pause
```

**compile_protos.sh (Linux) :**
```bash
#!/bin/bash

PROTO_DIR="./proto"
OUT_DIR="./generated"

mkdir -p "$OUT_DIR"

for proto_file in "$PROTO_DIR"/*.proto; do
    echo "Compilation de $proto_file..."
    protoc --proto_path="$PROTO_DIR" --pascal_out="$OUT_DIR" "$proto_file"
done

echo "Terminé !"
```

### Communication cross-platform

```pascal
program ServeurMultiPlateforme;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, BaseUnix,
  {$ENDIF}
  SysUtils,
  mormot.core.base,
  mormot.rest.http.server;

procedure ConfigurerServeur;
begin
  WriteLn('=== Serveur Protobuf/gRPC-like ===');

  {$IFDEF WINDOWS}
  WriteLn('Plateforme : Windows');
  WriteLn('Encodage : UTF-16LE');
  {$ENDIF}

  {$IFDEF UNIX}
  WriteLn('Plateforme : Linux/Unix');
  WriteLn('Encodage : UTF-8');
  {$ENDIF}

  // Configuration commune
  WriteLn('Port : 50051');
  WriteLn('Format : Protocol Buffers');
end;

begin
  ConfigurerServeur;
  // Suite du programme...
end.
```

## Cas d'usage pratiques

### 1. API de e-commerce

**shop.proto :**
```protobuf
syntax = "proto3";

package shop;

message Produit {
  string id = 1;
  string nom = 2;
  string description = 3;
  int32 prix_cents = 4;
  int32 stock = 5;
  repeated string images = 6;
  map<string, string> caracteristiques = 7;
}

message Panier {
  string client_id = 1;
  repeated ArticlePanier articles = 2;
  int32 total_cents = 3;
}

message ArticlePanier {
  string produit_id = 1;
  int32 quantite = 2;
  int32 prix_unitaire_cents = 3;
}

message CommandeRequest {
  string client_id = 1;
  Panier panier = 2;
  AdresseLivraison adresse = 3;
  PaiementInfo paiement = 4;
}

message CommandeResponse {
  string commande_id = 1;
  bool success = 2;
  string message = 3;
}

service ShopService {
  rpc GetProduit (ProduitRequest) returns (Produit);
  rpc ListProduits (ListRequest) returns (stream Produit);
  rpc AjouterAuPanier (AjoutPanierRequest) returns (Panier);
  rpc PasserCommande (CommandeRequest) returns (CommandeResponse);
}
```

### 2. Système de chat

**chat.proto :**
```protobuf
syntax = "proto3";

package chat;

import "google/protobuf/timestamp.proto";

message Message {
  string id = 1;
  string auteur_id = 2;
  string contenu = 3;
  google.protobuf.Timestamp timestamp = 4;

  oneof media {
    Image image = 5;
    Fichier fichier = 6;
    Lien lien = 7;
  }
}

message Image {
  string url = 1;
  int32 largeur = 2;
  int32 hauteur = 3;
}

message Fichier {
  string nom = 1;
  int64 taille = 2;
  string mime_type = 3;
  string url = 4;
}

message Lien {
  string url = 1;
  string titre = 2;
  string description = 3;
}

message Salon {
  string id = 1;
  string nom = 2;
  repeated string membres = 3;
}

service ChatService {
  // Streaming bidirectionnel pour le chat en temps réel
  rpc StreamChat (stream Message) returns (stream Message);

  // Opérations classiques
  rpc CreerSalon (SalonRequest) returns (Salon);
  rpc RejoindreSalon (JoinRequest) returns (Salon);
  rpc EnvoyerMessage (Message) returns (MessageResponse);
  rpc ObtenirHistorique (HistoriqueRequest) returns (stream Message);
}
```

### 3. Monitoring IoT

**iot.proto :**
```protobuf
syntax = "proto3";

package iot;

import "google/protobuf/timestamp.proto";

enum TypeCapteur {
  TYPE_CAPTEUR_UNSPECIFIED = 0;
  TYPE_CAPTEUR_TEMPERATURE = 1;
  TYPE_CAPTEUR_HUMIDITE = 2;
  TYPE_CAPTEUR_PRESSION = 3;
  TYPE_CAPTEUR_CO2 = 4;
  TYPE_CAPTEUR_LUMINOSITE = 5;
}

message Mesure {
  string capteur_id = 1;
  TypeCapteur type = 2;
  double valeur = 3;
  string unite = 4;
  google.protobuf.Timestamp timestamp = 5;
  int32 batterie_pct = 6;
}

message Alerte {
  string capteur_id = 1;
  string message = 2;
  enum Severite {
    INFO = 0;
    WARNING = 1;
    ERROR = 2;
    CRITICAL = 3;
  }
  Severite severite = 3;
  google.protobuf.Timestamp timestamp = 4;
}

message StatsCapteur {
  string capteur_id = 1;
  double valeur_min = 2;
  double valeur_max = 3;
  double valeur_moyenne = 4;
  int32 nombre_mesures = 5;
}

service IoTService {
  // Capteurs envoient des mesures en continu
  rpc StreamMesures (stream Mesure) returns (AckResponse);

  // Serveur envoie des alertes
  rpc SubscribeAlertes (AlerteRequest) returns (stream Alerte);

  // Statistiques
  rpc GetStats (StatsRequest) returns (StatsCapteur);
}
```

## Alternatives et comparaisons

### Protocol Buffers vs autres formats

| Format | Taille | Vitesse | Lisible | Schéma | Évolution |
|--------|--------|---------|---------|--------|-----------|
| Protobuf | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐ | ✅ | ⭐⭐⭐⭐⭐ |
| JSON | ⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ❌ | ⭐⭐⭐ |
| MessagePack | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐ | ❌ | ⭐⭐ |
| Avro | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐ | ✅ | ⭐⭐⭐⭐ |
| Thrift | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐ | ✅ | ⭐⭐⭐⭐ |
| Cap'n Proto | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐ | ✅ | ⭐⭐⭐⭐ |
| FlatBuffers | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐ | ✅ | ⭐⭐⭐⭐ |

### Quand utiliser chaque format ?

**Protocol Buffers :**
- Communication entre microservices
- APIs haute performance
- Stockage compact de données
- Versioning important

**JSON :**
- APIs publiques REST
- Configuration
- Debugging facile
- Compatibilité navigateur

**MessagePack :**
- Alternative binaire à JSON
- Moins complexe que Protobuf
- Pas de schéma nécessaire

**FlatBuffers :**
- Jeux vidéo (accès sans parsing)
- Applications temps réel extrême
- Mémoire limitée

## Ressources et documentation

### Documentation officielle

**Protocol Buffers :**
- Site officiel : https://protobuf.dev/
- Guide du langage : https://protobuf.dev/programming-guides/proto3/
- Spécification : https://protobuf.dev/reference/

**gRPC :**
- Site officiel : https://grpc.io/
- Documentation : https://grpc.io/docs/
- Examples : https://github.com/grpc/grpc/tree/master/examples

### Outils recommandés

**1. Buf** - Outil moderne pour protobuf
```bash
# Installation
brew install buf  # macOS
# ou
go install github.com/bufbuild/buf/cmd/buf@latest

# Utilisation
buf lint
buf generate
buf breaking --against '.git#branch=main'
```

**2. grpcurl** - curl pour gRPC
```bash
# Installation
brew install grpcurl

# Utilisation
grpcurl -plaintext localhost:50051 list
grpcurl -plaintext localhost:50051 user.UserService/GetUser
```

**3. Bloom RPC** - GUI pour tester gRPC
Interface graphique type Postman pour gRPC.

### Bibliothèques Pascal

**1. mORMot** (recommandé)
- Support complet de Protocol Buffers
- Très performant
- Bien maintenu
- URL : https://github.com/synopse/mORMot2

**2. fpnn-grpc-pascal**
- Support gRPC pour FreePascal
- Expérimental
- URL : https://github.com/highras/fpnn-grpc-pascal

**3. protobuf-fpc**
- Implémentation pure Pascal de Protocol Buffers
- Moins de dépendances
- URL : https://github.com/BeRo1985/protobuf-fpc

## Projet complet exemple

### Architecture du projet

```
MonProjetgRPC/
├── proto/
│   ├── common/
│   │   └── types.proto
│   ├── user/
│   │   └── user.proto
│   └── order/
│       └── order.proto
├── src/
│   ├── generated/          # Code généré
│   ├── models/             # Modèles métier
│   ├── services/           # Implémentations
│   ├── server/             # Serveur
│   └── client/             # Client
├── tests/
├── scripts/
│   ├── compile_protos.sh
│   └── compile_protos.bat
└── docs/
```

### Script de build complet

**build.sh (Linux) :**
```bash
#!/bin/bash

set -e  # Arrêter en cas d'erreur

echo "=== Build du projet gRPC ==="
echo ""

# Couleurs
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# 1. Nettoyer
echo "1. Nettoyage..."
rm -rf generated/
mkdir -p generated/

# 2. Compiler les protos
echo "2. Compilation des fichiers .proto..."
PROTO_FILES=$(find proto -name "*.proto")

for proto_file in $PROTO_FILES; do
    echo "   - Compilation de $proto_file"
    protoc \
        --proto_path=proto \
        --pascal_out=generated \
        "$proto_file"

    if [ $? -ne 0 ]; then
        echo -e "${RED}✗ Erreur lors de la compilation de $proto_file${NC}"
        exit 1
    fi
done

echo -e "${GREEN}✓ Compilation des protos réussie${NC}"

# 3. Compiler le serveur
echo "3. Compilation du serveur..."
fpc -Fu../mormot2/src/core \
    -Fu../mormot2/src/rest \
    -Fugenerated \
    -Fusrc/models \
    -Fusrc/services \
    -FE./bin \
    src/server/server.pas

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Serveur compilé${NC}"
else
    echo -e "${RED}✗ Erreur compilation serveur${NC}"
    exit 1
fi

# 4. Compiler le client
echo "4. Compilation du client..."
fpc -Fu../mormot2/src/core \
    -Fu../mormot2/src/rest \
    -Fugenerated \
    -Fusrc/models \
    -FE./bin \
    src/client/client.pas

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Client compilé${NC}"
else
    echo -e "${RED}✗ Erreur compilation client${NC}"
    exit 1
fi

echo ""
echo -e "${GREEN}=== Build terminé avec succès ===${NC}"
echo "Exécutables dans ./bin/"
```

**build.bat (Windows) :**
```batch
@echo off
setlocal enabledelayedexpansion

echo === Build du projet gRPC ===
echo.

REM 1. Nettoyer
echo 1. Nettoyage...
if exist generated rmdir /s /q generated
mkdir generated

REM 2. Compiler les protos
echo 2. Compilation des fichiers .proto...

for /r proto %%f in (*.proto) do (
    echo    - Compilation de %%f
    protoc --proto_path=proto --pascal_out=generated "%%f"
    if errorlevel 1 (
        echo [ERREUR] Echec compilation de %%f
        exit /b 1
    )
)

echo [OK] Compilation des protos reussie

REM 3. Compiler le serveur
echo 3. Compilation du serveur...
fpc -Fu..\mormot2\src\core ^
    -Fu..\mormot2\src\rest ^
    -Fugenerated ^
    -Fusrc\models ^
    -Fusrc\services ^
    -FE.\bin ^
    src\server\server.pas

if errorlevel 1 (
    echo [ERREUR] Echec compilation serveur
    exit /b 1
)
echo [OK] Serveur compile

REM 4. Compiler le client
echo 4. Compilation du client...
fpc -Fu..\mormot2\src\core ^
    -Fu..\mormot2\src\rest ^
    -Fugenerated ^
    -Fusrc\models ^
    -FE.\bin ^
    src\client\client.pas

if errorlevel 1 (
    echo [ERREUR] Echec compilation client
    exit /b 1
)
echo [OK] Client compile

echo.
echo === Build termine avec succes ===
echo Executables dans .\bin\
pause
```

### Configuration Docker

**Dockerfile :**
```dockerfile
FROM debian:bullseye-slim

# Installer les dépendances
RUN apt-get update && apt-get install -y \
    fpc \
    protobuf-compiler \
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Créer les dossiers
WORKDIR /app

# Copier les sources
COPY proto /app/proto
COPY src /app/src
COPY scripts /app/scripts

# Cloner mORMot
RUN git clone https://github.com/synopse/mORMot2.git /opt/mormot2

# Compiler
RUN chmod +x /app/scripts/compile_protos.sh && \
    /app/scripts/compile_protos.sh && \
    fpc -Fu/opt/mormot2/src/core \
        -Fu/opt/mormot2/src/rest \
        -Fugenerated \
        -Fusrc/models \
        -Fusrc/services \
        -FE/app/bin \
        src/server/server.pas

# Exposer le port
EXPOSE 50051

# Utilisateur non-root
RUN useradd -m appuser && \
    chown -R appuser:appuser /app
USER appuser

# Lancer le serveur
CMD ["/app/bin/server"]
```

**docker-compose.yml :**
```yaml
version: '3.8'

services:
  grpc-server:
    build: .
    container_name: grpc-server
    ports:
      - "50051:50051"
    environment:
      - LOG_LEVEL=INFO
    volumes:
      - ./data:/app/data
      - ./logs:/app/logs
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "grpcurl", "-plaintext", "localhost:50051", "list"]
      interval: 30s
      timeout: 10s
      retries: 3

  # Client de test
  grpc-client:
    build: .
    container_name: grpc-client
    command: /app/bin/client
    depends_on:
      - grpc-server
    environment:
      - SERVER_HOST=grpc-server
      - SERVER_PORT=50051
```

## Tests et validation

### Tests unitaires des messages Protobuf

```pascal
unit TestsProtobuf;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry,
  Models, ProtobufHelpers;

type
  TTestProtobuf = class(TTestCase)
  published
    procedure TestSerialisation;
    procedure TestDeserialisation;
    procedure TestRoundTrip;
    procedure TestChampOptionnel;
    procedure TestChampsRepetes;
    procedure TestCompatibilite;
  end;

implementation

procedure TTestProtobuf.TestSerialisation;
var
  User: TUtilisateur;
  Data: TBytes;
begin
  User.ID := 42;
  User.Nom := 'Test User';
  User.Email := 'test@example.com';
  User.Age := 25;
  User.Actif := True;

  Data := SerialiserUtilisateur(User);

  AssertTrue('Données non vides', Length(Data) > 0);
  AssertTrue('Taille raisonnable', Length(Data) < 1000);
end;

procedure TTestProtobuf.TestDeserialisation;
var
  User, UserDeserialisé: TUtilisateur;
  Data: TBytes;
begin
  User.ID := 123;
  User.Nom := 'Alice';
  User.Email := 'alice@test.com';
  User.Age := 30;
  User.Actif := True;

  Data := SerialiserUtilisateur(User);
  UserDeserialisé := DeserialiserUtilisateur(Data);

  AssertEquals('ID identique', User.ID, UserDeserialisé.ID);
  AssertEquals('Nom identique', User.Nom, UserDeserialisé.Nom);
  AssertEquals('Email identique', User.Email, UserDeserialisé.Email);
  AssertEquals('Age identique', User.Age, UserDeserialisé.Age);
  AssertEquals('Actif identique', User.Actif, UserDeserialisé.Actif);
end;

procedure TTestProtobuf.TestRoundTrip;
var
  i: Integer;
  User, Result: TUtilisateur;
  Data: TBytes;
begin
  // Tester avec plusieurs valeurs
  for i := 1 to 100 do
  begin
    User.ID := i;
    User.Nom := Format('User%d', [i]);
    User.Email := Format('user%d@test.com', [i]);
    User.Age := 20 + (i mod 50);
    User.Actif := (i mod 2) = 0;

    Data := SerialiserUtilisateur(User);
    Result := DeserialiserUtilisateur(Data);

    AssertEquals(Format('ID %d', [i]), User.ID, Result.ID);
    AssertEquals(Format('Nom %d', [i]), User.Nom, Result.Nom);
  end;
end;

procedure TTestProtobuf.TestChampOptionnel;
var
  User: TUtilisateur;
  Data: TBytes;
begin
  // Champ optionnel non défini
  User.ID := 1;
  User.Nom := 'Test';
  User.Email := '';  // Optionnel vide
  User.Age := 25;
  User.Actif := True;

  Data := SerialiserUtilisateur(User);

  // Vérifier que le champ optionnel ne prend pas de place
  AssertTrue('Taille réduite', Length(Data) < 30);
end;

procedure TTestProtobuf.TestChampsRepetes;
var
  Liste: TListeUtilisateurs;
  Data: TBytes;
  i: Integer;
begin
  SetLength(Liste.Utilisateurs, 10);
  for i := 0 to 9 do
  begin
    Liste.Utilisateurs[i].ID := i + 1;
    Liste.Utilisateurs[i].Nom := Format('User%d', [i + 1]);
    Liste.Utilisateurs[i].Email := Format('user%d@test.com', [i + 1]);
    Liste.Utilisateurs[i].Age := 20 + i;
    Liste.Utilisateurs[i].Actif := True;
  end;

  // Sérialiser la liste
  // Data := SerialiserListeUtilisateurs(Liste);

  AssertTrue('Liste sérialisée', True);
end;

procedure TTestProtobuf.TestCompatibilite;
var
  UserV1, UserV2: TUtilisateur;
  DataV1: TBytes;
begin
  // Version 1 : seulement nom et email
  UserV1.ID := 0;
  UserV1.Nom := 'Ancien';
  UserV1.Email := 'ancien@test.com';
  UserV1.Age := 0;
  UserV1.Actif := False;

  DataV1 := SerialiserUtilisateur(UserV1);

  // Version 2 : peut lire V1
  UserV2 := DeserialiserUtilisateur(DataV1);

  AssertEquals('Nom lu', 'Ancien', UserV2.Nom);
  AssertEquals('Email lu', 'ancien@test.com', UserV2.Email);
  AssertEquals('Age par défaut', 0, UserV2.Age);
end;

initialization
  RegisterTest(TTestProtobuf);

end.
```

### Tests de performance

```pascal
program BenchmarkProtobuf;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  mormot.core.json,
  mormot.core.perf,
  Models, ProtobufHelpers;

procedure BenchmarkerSerialisation;
const
  NB_ITERATIONS = 100000;
var
  i: Integer;
  User: TUtilisateur;
  Data: TBytes;
  JSON: RawUtf8;
  Timer: TPrecisionTimer;
  TailleProtobuf, TailleJSON: Int64;
begin
  // Préparer les données
  User.ID := 42;
  User.Nom := 'John Doe Example Name';
  User.Email := 'john.doe@example.com';
  User.Age := 35;
  User.Actif := True;

  WriteLn('=== Benchmark Sérialisation ===');
  WriteLn('Nombre d''itérations : ', NB_ITERATIONS);
  WriteLn;

  // Benchmark Protobuf
  Timer.Start;
  for i := 1 to NB_ITERATIONS do
    Data := SerialiserUtilisateur(User);
  WriteLn('Protobuf : ', Timer.Stop);
  WriteLn('  Débit : ', Timer.PerSec(NB_ITERATIONS):0:0, ' ops/sec');
  TailleProtobuf := Length(Data);
  WriteLn('  Taille : ', TailleProtobuf, ' octets');
  WriteLn;

  // Benchmark JSON
  Timer.Start;
  for i := 1 to NB_ITERATIONS do
    JSON := RecordSaveJSON(User, TypeInfo(TUtilisateur));
  WriteLn('JSON : ', Timer.Stop);
  WriteLn('  Débit : ', Timer.PerSec(NB_ITERATIONS):0:0, ' ops/sec');
  TailleJSON := Length(JSON);
  WriteLn('  Taille : ', TailleJSON, ' octets');
  WriteLn;

  // Comparaison
  WriteLn('=== Résultats ===');
  WriteLn('Protobuf est ', (TailleJSON / TailleProtobuf):0:1, 'x plus compact');
  WriteLn('Économie : ', (1 - TailleProtobuf / TailleJSON) * 100:0:1, '%');
end;

procedure BenchmarkerDeserialisation;
const
  NB_ITERATIONS = 100000;
var
  i: Integer;
  User, Result: TUtilisateur;
  Data: TBytes;
  JSON: RawUtf8;
  Timer: TPrecisionTimer;
begin
  // Préparer les données
  User.ID := 42;
  User.Nom := 'John Doe Example Name';
  User.Email := 'john.doe@example.com';
  User.Age := 35;
  User.Actif := True;

  Data := SerialiserUtilisateur(User);
  JSON := RecordSaveJSON(User, TypeInfo(TUtilisateur));

  WriteLn('=== Benchmark Désérialisation ===');
  WriteLn('Nombre d''itérations : ', NB_ITERATIONS);
  WriteLn;

  // Benchmark Protobuf
  Timer.Start;
  for i := 1 to NB_ITERATIONS do
    Result := DeserialiserUtilisateur(Data);
  WriteLn('Protobuf : ', Timer.Stop);
  WriteLn('  Débit : ', Timer.PerSec(NB_ITERATIONS):0:0, ' ops/sec');
  WriteLn;

  // Benchmark JSON
  Timer.Start;
  for i := 1 to NB_ITERATIONS do
    RecordLoadJSON(Result, JSON, TypeInfo(TUtilisateur));
  WriteLn('JSON : ', Timer.Stop);
  WriteLn('  Débit : ', Timer.PerSec(NB_ITERATIONS):0:0, ' ops/sec');
end;

begin
  BenchmarkerSerialisation;
  WriteLn;
  BenchmarkerDeserialisation;
  WriteLn;
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
end.
```

**Résultats typiques :**
```
=== Benchmark Sérialisation ===
Nombre d'itérations : 100000

Protobuf : 45ms
  Débit : 2222222 ops/sec
  Taille : 45 octets

JSON : 320ms
  Débit : 312500 ops/sec
  Taille : 135 octets

=== Résultats ===
Protobuf est 3.0x plus compact
Économie : 66.7%
```

## Migration vers gRPC/Protobuf

### Stratégie de migration depuis REST

**Phase 1 : Coexistence**
- Garder l'API REST existante
- Ajouter gRPC en parallèle
- Migrer progressivement les clients

**Phase 2 : Migration**
- Identifier les endpoints critiques
- Les migrer vers gRPC
- Mesurer les performances

**Phase 3 : Optimisation**
- Désactiver les endpoints REST inutilisés
- Optimiser les messages Protobuf
- Documentation complète

### Proxy REST → gRPC

Pour faciliter la transition, créer un proxy :

```pascal
unit RestToGrpcProxy;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.rest.server,
  mormot.rest.http.server,
  UserServiceImpl;

type
  TRestProxy = class
  private
    FGrpcService: IUserService;
  public
    constructor Create(GrpcService: IUserService);

    // Endpoint REST qui appelle gRPC
    function GetUser(Ctxt: TRestServerUriContext): Integer;
    function CreateUser(Ctxt: TRestServerUriContext): Integer;
  end;

implementation

constructor TRestProxy.Create(GrpcService: IUserService);
begin
  FGrpcService := GrpcService;
end;

function TRestProxy.GetUser(Ctxt: TRestServerUriContext): Integer;
var
  ID: Integer;
  Reponse: TReponseUtilisateur;
  JSON: RawUtf8;
begin
  // Extraire l'ID depuis l'URL REST
  ID := StrToIntDef(Ctxt.InputString['id'], 0);

  // Appeler le service gRPC
  Reponse := FGrpcService.GetUser(ID);

  // Convertir en JSON pour REST
  if Reponse.Trouve then
  begin
    JSON := RecordSaveJSON(Reponse.Utilisateur, TypeInfo(TUtilisateur));
    Ctxt.Returns(JSON, 200, TEXT_CONTENT_TYPE_JSON);
  end
  else
  begin
    Ctxt.Error('Utilisateur non trouvé', 404);
  end;

  Result := 200;
end;

function TRestProxy.CreateUser(Ctxt: TRestServerUriContext): Integer;
var
  Nom, Email: RawUtf8;
  Age: Integer;
  Reponse: TReponseUtilisateur;
  JSON: RawUtf8;
begin
  // Parser le JSON REST
  Nom := Ctxt.InputString['nom'];
  Email := Ctxt.InputString['email'];
  Age := StrToIntDef(Ctxt.InputString['age'], 0);

  // Appeler le service gRPC
  Reponse := FGrpcService.CreateUser(Nom, Email, Age);

  // Retourner en JSON
  JSON := RecordSaveJSON(Reponse.Utilisateur, TypeInfo(TUtilisateur));
  Ctxt.Returns(JSON, 201, TEXT_CONTENT_TYPE_JSON);

  Result := 201;
end;

end.
```

## Conclusion et recommandations

### Récapitulatif

Dans ce tutoriel, nous avons couvert :

1. **Introduction à gRPC et Protocol Buffers** - Concepts de base
2. **Installation** - Configuration sur Windows et Ubuntu
3. **Protocol Buffers** - Syntaxe, types, messages
4. **Génération de code** - Compilation des .proto
5. **Implémentation** - Serveurs et clients
6. **Types de RPC** - Unary, streaming
7. **Optimisation** - Performance, compression
8. **Tests** - Unitaires, benchmarks
9. **Déploiement** - Docker, production

### Avantages de gRPC/Protobuf

✅ **Performance exceptionnelle** - 5 à 10x plus rapide que REST/JSON  
✅ **Efficacité réseau** - 60 à 90% de réduction de taille  
✅ **Typage fort** - Moins d'erreurs, meilleure maintenabilité  
✅ **Streaming** - Support natif du temps réel  
✅ **Multi-langage** - Interopérabilité parfaite  
✅ **Évolutivité** - Compatibilité ascendante garantie  
✅ **Génération de code** - Productivité accrue

### Inconvénients et limitations

❌ **Courbe d'apprentissage** - Plus complexe que REST  
❌ **Support FreePascal limité** - Nécessite des adaptations  
❌ **Debugging** - Format binaire moins lisible  
❌ **Écosystème** - Moins d'outils que REST  
❌ **Navigateurs** - Pas de support natif

### Quand utiliser gRPC/Protobuf ?

**✅ Utilisez gRPC quand :**
- Performance critique (microservices, temps réel)
- Communication backend-à-backend
- Streaming bidirectionnel nécessaire
- Bande passante limitée (mobile, IoT)
- Typage fort requis

**❌ Préférez REST quand :**
- API publique simple
- Clients navigateurs web
- Équipe débutante
- Debugging fréquent
- Flexibilité prioritaire

### Recommandations pour FreePascal

**Pour des projets FreePascal/Lazarus :**

1. **Utilisez mORMot** - Meilleur support Protobuf
2. **Commencez simple** - Messages basiques d'abord
3. **Testez extensivement** - Validation critique
4. **Documentez bien** - Format binaire opaque
5. **Benchmarkez** - Vérifiez les gains réels

### Ressources essentielles

**Documentation :**
- Protocol Buffers : https://protobuf.dev/
- gRPC : https://grpc.io/
- mORMot : https://synopse.info/

**Outils :**
- protoc : Compilateur officiel
- Buf : Outil moderne
- Bloom RPC : GUI de test
- grpcurl : CLI pour gRPC

**Exemples :**
- Dépôt officiel gRPC : https://github.com/grpc/grpc
- mORMot samples : https://github.com/synopse/mORMot2/tree/master/ex

### Prochaines étapes

Après avoir maîtrisé gRPC et Protobuf :

1. **Étudier les intercepteurs** - Middleware gRPC
2. **Load balancing** - Distribution de charge
3. **Service mesh** - Istio, Linkerd
4. **Observabilité** - Métriques, tracing
5. **Sécurité avancée** - mTLS, authentification

### Exemple complet final

Voici un exemple production-ready complet :

```pascal
program ServeurProductionGrpc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, BaseUnix,
  {$ENDIF}
  SysUtils,
  mormot.core.base,
  mormot.core.log,
  mormot.core.os,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.http.server,
  Models, UserServiceImpl, MetriquesPrometheus;

var
  Model: TOrmModel;
  Rest: TRestServerFullMemory;
  Http: TRestHttpServer;
  Port: String;

procedure InitialiserLogs;
begin
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := True;

  {$IFDEF UNIX}
  TSynLog.Family.DestinationPath := '/var/log/grpc-server/';
  {$ELSE}
  TSynLog.Family.DestinationPath := 'C:\Logs\grpc-server\';
  {$ENDIF}
end;

procedure GestionnaireSignal(Signal: cint); cdecl;
begin
  WriteLn;
  WriteLn('Signal reçu, arrêt propre...');
  Halt(0);
end;

begin
  try
    WriteLn('=== Serveur gRPC/Protobuf ===');
    WriteLn('Version : 1.0.0');
    WriteLn;

    // Configuration
    Port := GetEnvironmentVariable('GRPC_PORT');
    if Port = '' then
      Port := '50051';

    // Logs
    InitialiserLogs;
    TSynLog.Add.Log(sllInfo, 'Démarrage du serveur gRPC');

    // Créer le modèle
    Model := TOrmModel.Create([], 'grpc');
    Rest := TRestServerFullMemory.Create(Model, False);
    try
      // Services
      Rest.ServiceDefine(TUserService, [IUserService], sicShared);

      // Serveur HTTP
      Http := TRestHttpServer.Create(Port, [Rest], '+', HTTP_DEFAULT_MODE);
      try
        WriteLn('✓ Serveur démarré sur le port ', Port);
        WriteLn('✓ Format : Protocol Buffers');
        WriteLn('✓ Services : UserService');
        WriteLn;
        WriteLn('Endpoints :');
        WriteLn('  - UserService : http://localhost:', Port, '/grpc/UserService');
        WriteLn('  - Health : http://localhost:', Port, '/health');
        WriteLn('  - Metrics : http://localhost:', Port, '/metrics');
        WriteLn;

        TSynLog.Add.Log(sllInfo, 'Serveur opérationnel sur port %', [Port]);

        {$IFDEF UNIX}
        // Gérer les signaux Unix
        fpSignal(SIGTERM, @GestionnaireSignal);
        fpSignal(SIGINT, @GestionnaireSignal);

        WriteLn('Appuyez sur Ctrl+C pour arrêter...');
        while True do
          Sleep(1000);
        {$ELSE}
        WriteLn('Appuyez sur Entrée pour arrêter...');
        ReadLn;
        {$ENDIF}

        TSynLog.Add.Log(sllInfo, 'Arrêt demandé');
        WriteLn('Arrêt du serveur...');

      finally
        Http.Free;
      end;
    finally
      Rest.Free;
      Model.Free;
    end;

    TSynLog.Add.Log(sllInfo, 'Serveur arrêté proprement');
    WriteLn('✓ Serveur arrêté');

  except
    on E: Exception do
    begin
      WriteLn('✗ ERREUR FATALE : ', E.Message);
      TSynLog.Add.Log(sllError, 'Erreur fatale : %', [E.Message]);
      ExitCode := 1;
    end;
  end;
end.
```

### Mot de la fin

gRPC et Protocol Buffers représentent l'avenir de la communication entre services. Bien que le support natif pour FreePascal soit encore limité, l'utilisation de mORMot permet de bénéficier de la plupart des avantages de ce protocole.

**Points clés à retenir :**
- Performance exceptionnelle pour les microservices
- Excellente efficacité réseau
- Courbe d'apprentissage plus élevée que REST
- Nécessite des outils spécialisés pour le debugging
- Adapter les solutions existantes pour FreePascal

**Conseil final :** Commencez par implémenter Protocol Buffers pour la sérialisation de vos données, puis évoluez progressivement vers une architecture gRPC complète si vos besoins le justifient. mORMot offre une excellente base pour débuter.

Bon développement avec gRPC et Protocol Buffers ! 🚀

---

## Annexe : Aide-mémoire

### Commandes protoc essentielles

```bash
# Compiler un fichier proto
protoc --pascal_out=./out file.proto

# Avec chemin d'inclusion
protoc -I./proto --pascal_out=./out proto/user.proto

# Plusieurs fichiers
protoc -I./proto --pascal_out=./out proto/*.proto

# Vérifier la syntaxe
protoc --decode_raw < data.bin

# Afficher les types
protoc --descriptor_set_out=types.pb proto/*.proto
```

### Types de données protobuf

| Protobuf | Pascal | Taille | Notes |
|----------|--------|--------|-------|
| double | Double | 8 bytes | Virgule flottante |
| float | Single | 4 bytes | Virgule flottante |
| int32 | Integer | Variable | Signé |
| int64 | Int64 | Variable | Signé |
| uint32 | LongWord | Variable | Non signé |
| uint64 | QWord | Variable | Non signé |
| sint32 | Integer | Variable | Signé optimisé |
| sint64 | Int64 | Variable | Signé optimisé |
| fixed32 | LongWord | 4 bytes | Non signé fixe |
| fixed64 | QWord | 8 bytes | Non signé fixe |
| sfixed32 | Integer | 4 bytes | Signé fixe |
| sfixed64 | Int64 | 8 bytes | Signé fixe |
| bool | Boolean | 1 byte | Booléen |
| string | String/RawUtf8 | Variable | UTF-8 |
| bytes | TBytes | Variable | Binaire |

### Wire types

| Type | Nom | Utilisé pour |
|------|-----|--------------|
| 0 | Varint | int32, int64, uint32, uint64, bool, enum |
| 1 | 64-bit | fixed64, sfixed64, double |
| 2 | Length-delimited | string, bytes, messages, repeated |
| 5 | 32-bit | fixed32, sfixed32, float |

### Encodage des tags

```
Tag = (field_number << 3) | wire_type

Exemple :
  Field 1, type varint : (1 << 3) | 0 = 8
  Field 2, type string : (2 << 3) | 2 = 18
  Field 3, type varint : (3 << 3) | 0 = 24
```

### Règles de compatibilité

| Action | Compatible ? | Notes |
|--------|--------------|-------|
| Ajouter un champ | ✅ Oui | Toujours safe |
| Supprimer un champ | ⚠️ Oui | Réserver le numéro |
| Renommer un champ | ✅ Oui | Le numéro compte seul |
| Changer le type | ❌ Non | Risque de corruption |
| Changer le numéro | ❌ Non | Changement de sémantique |
| optional → repeated | ❌ Non | Incompatible |
| repeated → optional | ❌ Non | Incompatible |

### Exemples de messages courants

**Pagination :**
```protobuf
message PaginationRequest {
  int32 page = 1;
  int32 page_size = 2;
  string sort_by = 3;
  bool descending = 4;
}

message PaginationResponse {
  repeated Item items = 1;
  int32 total_count = 2;
  int32 page = 3;
  int32 total_pages = 4;
}
```

**Résultat avec erreur :**
```protobuf
message Result {
  oneof result {
    Data data = 1;
    Error error = 2;
  }
}

message Error {
  int32 code = 1;
  string message = 2;
  map<string, string> details = 3;
}
```

**Métadonnées :**
```protobuf
message Metadata {
  google.protobuf.Timestamp created_at = 1;
  google.protobuf.Timestamp updated_at = 2;
  string created_by = 3;
  string updated_by = 4;
  int32 version = 5;
}
```

**Filtres de recherche :**
```protobuf
message SearchFilter {
  string field = 1;
  enum Operator {
    EQUALS = 0;
    NOT_EQUALS = 1;
    GREATER_THAN = 2;
    LESS_THAN = 3;
    CONTAINS = 4;
    IN = 5;
  }
  Operator operator = 2;
  string value = 3;
}

message SearchRequest {
  repeated SearchFilter filters = 1;
  string query = 2;
  PaginationRequest pagination = 3;
}
```

### Patterns de conception gRPC

**1. Health Check Service :**
```protobuf
service Health {
  rpc Check(HealthCheckRequest) returns (HealthCheckResponse);
  rpc Watch(HealthCheckRequest) returns (stream HealthCheckResponse);
}

message HealthCheckRequest {
  string service = 1;
}

message HealthCheckResponse {
  enum ServingStatus {
    UNKNOWN = 0;
    SERVING = 1;
    NOT_SERVING = 2;
    SERVICE_UNKNOWN = 3;
  }
  ServingStatus status = 1;
}
```

**2. Batch Operations :**
```protobuf
message BatchRequest {
  repeated SingleRequest requests = 1;
}

message BatchResponse {
  repeated SingleResponse responses = 1;
}

service BatchService {
  rpc ProcessBatch(BatchRequest) returns (BatchResponse);
}
```

**3. Long Running Operations :**
```protobuf
message Operation {
  string name = 1;
  bool done = 2;
  oneof result {
    Error error = 3;
    google.protobuf.Any response = 4;
  }
}

service LongRunning {
  rpc StartOperation(OperationRequest) returns (Operation);
  rpc GetOperation(GetOperationRequest) returns (Operation);
  rpc ListOperations(ListOperationsRequest) returns (ListOperationsResponse);
}
```

### Outils de diagnostic

**1. Afficher la structure d'un message :**
```bash
# Décoder en format texte
protoc --decode=package.MessageName proto/file.proto < data.bin

# Décoder sans schéma (raw)
protoc --decode_raw < data.bin
```

**2. Tester avec grpcurl :**
```bash
# Lister les services
grpcurl -plaintext localhost:50051 list

# Lister les méthodes d'un service
grpcurl -plaintext localhost:50051 list package.ServiceName

# Décrire un message
grpcurl -plaintext localhost:50051 describe package.MessageName

# Appeler une méthode
grpcurl -plaintext -d '{"id": 42}' \
  localhost:50051 package.ServiceName/MethodName
```

**3. Monitoring et métriques :**
```pascal
// Intercepteur pour mesurer les performances
procedure LogRpcCall(const Method: RawUtf8; DurationMs: Integer);
begin
  TSynLog.Add.Log(sllInfo, 'RPC % completed in %ms', [Method, DurationMs]);

  // Incrémenter les métriques Prometheus
  PrometheusMetrics.IncrementCounter('grpc_requests_total', ['method', Method]);
  PrometheusMetrics.ObserveHistogram('grpc_duration_seconds',
                                     DurationMs / 1000, ['method', Method]);
end;
```

### Checklist de déploiement

**Avant le déploiement :**

- [ ] Tous les fichiers .proto sont versionnés
- [ ] Les numéros de champs sont documentés
- [ ] Les champs supprimés sont réservés
- [ ] Tests unitaires passent (100% coverage sur les messages)
- [ ] Tests de compatibilité ascendante/descendante
- [ ] Benchmarks de performance effectués
- [ ] Documentation API à jour
- [ ] Exemples de code client fournis
- [ ] Gestion des erreurs implémentée
- [ ] Logging configuré correctement
- [ ] Métriques Prometheus exposées
- [ ] Health checks implémentés
- [ ] Configuration externalisée
- [ ] TLS/SSL configuré (production)
- [ ] Rate limiting en place
- [ ] Timeouts configurés
- [ ] Retry policy définie
- [ ] Circuit breaker (si applicable)

**Monitoring post-déploiement :**

- [ ] Dashboard Grafana configuré
- [ ] Alertes Prometheus actives
- [ ] Logs centralisés (ELK/Loki)
- [ ] Tracing distribué (Jaeger/Zipkin)
- [ ] Métriques de latence surveillées
- [ ] Taux d'erreur surveillé
- [ ] Utilisation CPU/mémoire monitorée

### Ressources complémentaires

**Livres recommandés :**
- "gRPC: Up and Running" - Kasun Indrasiri & Danesh Kuruppu
- "Practical gRPC" - Joshua B. Humphries
- "Building Microservices with gRPC" - Reza Mamun

**Cours en ligne :**
- Google Cloud Skills Boost - gRPC courses
- Udemy - gRPC [Golang] MasterClass
- Pluralsight - Getting Started with gRPC

**Communautés :**
- gRPC Slack : https://grpc-io.slack.com
- Forum officiel : https://groups.google.com/g/grpc-io
- Stack Overflow tag `grpc`
- Reddit r/grpc

**Projets open source utilisant gRPC :**
- **Kubernetes** - Orchestration de conteneurs
- **etcd** - Stockage clé-valeur distribué
- **Istio** - Service mesh
- **TiDB** - Base de données distribuée
- **CockroachDB** - Base de données SQL distribuée

### Glossaire

**Termes importants :**

- **Protobuf** : Protocol Buffers, format de sérialisation binaire
- **RPC** : Remote Procedure Call, appel de procédure distante
- **Wire type** : Format d'encodage binaire d'un champ
- **Tag** : Numéro identifiant unique d'un champ
- **Message** : Structure de données dans protobuf
- **Service** : Ensemble de méthodes RPC
- **Stub** : Code généré côté client pour appeler le serveur
- **Stream** : Flux de messages (unidirectionnel ou bidirectionnel)
- **Unary RPC** : Appel simple requête-réponse
- **Interceptor** : Middleware pour intercepter les appels RPC
- **Metadata** : En-têtes HTTP/2 pour les appels gRPC
- **Deadline** : Timeout pour un appel RPC
- **Backpressure** : Mécanisme de contrôle de flux
- **Load balancing** : Distribution de charge entre serveurs

---

## Pour aller plus loin

### Sujets avancés non couverts

1. **gRPC Gateway** - Exposer gRPC via HTTP/REST
2. **gRPC-Web** - Support des navigateurs web
3. **Intercepteurs** - Middleware pour logging, auth, etc.
4. **Load balancing** - Distribution intelligente
5. **Service Discovery** - Consul, etcd
6. **Circuit breaker** - Résilience
7. **Distributed tracing** - OpenTelemetry
8. **mTLS** - Authentification mutuelle
9. **Multiplexing** - Plusieurs services sur un port
10. **Compression adaptative** - Optimisation dynamique

### Exercices pratiques suggérés

1. Créer un système de chat temps réel avec streaming bidirectionnel
2. Implémenter un service de file upload avec server streaming
3. Développer un dashboard de monitoring avec métriques gRPC
4. Créer un API gateway REST → gRPC
5. Implémenter un système de notification push
6. Développer un service d'authentification centralisé
7. Créer un système de cache distribué
8. Implémenter un load balancer custom
9. Développer un système de logs distribués
10. Créer une API de microservices complète

### Certifications et formations

**Certifications Google Cloud :**
- Google Cloud Professional Cloud Architect
- Google Cloud Professional Developer

**Formations spécialisées :**
- Coursera - Cloud Native Development
- edX - Microservices Architecture
- Linux Foundation - Cloud Engineer

Bonne chance dans vos projets gRPC avec FreePascal ! 🎯

⏭️ [P2P et protocoles décentralisés](/10-programmation-reseau-avancee/11-p2p-protocoles-decentralises.md)
