🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 26.6 Projets open source notables

## Introduction

L'écosystème FreePascal/Lazarus est enrichi par des centaines de projets open source de qualité exceptionnelle. Ces projets démontrent la puissance et la polyvalence de Pascal moderne, tout en offrant des solutions prêtes à l'emploi pour vos propres développements. Découvrir, utiliser et contribuer à ces projets est une excellente façon d'améliorer vos compétences et de participer à la communauté.

### Pourquoi s'intéresser aux projets open source ?

**Pour apprendre :**
- Lire du code de qualité professionnelle
- Découvrir des patterns et techniques avancées
- Comprendre l'architecture de grands projets
- Apprendre des erreurs et succès des autres
- Voir comment les problèmes complexes sont résolus

**Pour vos projets :**
- Réutiliser des composants éprouvés
- Gagner du temps de développement
- Bénéficier de la maintenance communautaire
- Éviter de réinventer la roue
- S'appuyer sur des solutions testées

**Pour contribuer :**
- Améliorer vos compétences en codant
- Collaborer avec d'autres développeurs
- Ajouter des fonctionnalités dont vous avez besoin
- Corriger des bugs que vous rencontrez
- Développer votre réputation professionnelle

### Comment évaluer un projet open source

Avant d'utiliser un projet, évaluez sa qualité :

**Critères de qualité :**

✅ **Documentation** : README clair, exemples, API documentée  
✅ **Activité** : Commits récents, issues résolues  
✅ **Communauté** : Nombre d'utilisateurs, contributeurs actifs  
✅ **Tests** : Suite de tests, CI/CD  
✅ **Licence** : Compatible avec vos besoins  
✅ **Maintenance** : Réponses aux issues, releases régulières  
✅ **Code** : Qualité, lisibilité, organisation

**Red flags :**

❌ Pas de commit depuis 2+ ans (projet abandonné)  
❌ Issues ouvertes sans réponse  
❌ Pas de documentation  
❌ Code non testé  
❌ Licence restrictive ou absente  
❌ Un seul contributeur sans successeur  
❌ Dépendances obsolètes ou non maintenues

## Frameworks et bibliothèques majeurs

### mORMot 2

**Catégorie** : Framework SOA/Microservices et ORM  
**Auteur** : Arnaud Bouchez et contributeurs  
**Licence** : GPL/LGPL/MPL tri-license  
**URL** : https://github.com/synopse/mORMot2  

**Description :**

mORMot (Synopse mORMot) est un framework complet pour développer des applications client-serveur, des services REST/SOA, et gérer la persistance objet-relationnel. C'est l'un des projets Pascal les plus ambitieux et professionnels.

**Fonctionnalités principales :**

- **ORM puissant** : Mapping objet-relationnel avec support multi-SGBD
- **REST/JSON** : Serveur et client REST haute performance
- **Microservices** : Architecture modulaire pour services distribués
- **Authentication** : JWT, OAuth, sessions sécurisées
- **Performance** : Optimisé pour vitesse et faible consommation mémoire
- **Cross-platform** : Windows, Linux, BSD, Android

**Exemple d'utilisation :**

```pascal
uses
  mormot.core.base,
  mormot.core.data,
  mormot.orm.core,
  mormot.rest.server;

type
  TOrmUser = class(TOrm)
  private
    fName: RawUtf8;
    fEmail: RawUtf8;
  published
    property Name: RawUtf8 read fName write fName;
    property Email: RawUtf8 read fEmail write fEmail;
  end;

// Créer un serveur REST
var
  Model: TOrmModel;
  Server: TRestServerDB;
begin
  Model := TOrmModel.Create([TOrmUser]);
  Server := TRestServerDB.Create(Model, 'data.db');
  Server.CreateMissingTables;

  // Le serveur REST est prêt !
end;
```

**Cas d'usage :**

- Applications enterprise (ERP, CRM)
- APIs REST haute performance
- Backends pour applications mobiles/web
- Systèmes distribués et microservices
- Applications nécessitant ORM sophistiqué

**Pourquoi c'est notable :**

- Performance exceptionnelle (benchmarks impressionnants)
- Architecture très professionnelle
- Documentation exhaustive (5000+ pages)
- Utilisé en production par de grandes entreprises
- Communauté active et support commercial disponible

### Castle Game Engine

**Catégorie** : Moteur de jeu 3D  
**Auteur** : Michalis Kamburelis et contributeurs  
**Licence** : LGPL/GPL (commercial-friendly)  
**URL** : https://castle-engine.io/  

**Description :**

Castle Game Engine est un moteur de jeu 3D complet pour créer des jeux multi-plateformes avec FreePascal. Il supporte Windows, Linux, macOS, Android, iOS et Nintendo Switch.

**Fonctionnalités principales :**

- **Rendu 3D moderne** : OpenGL/OpenGLES, shaders, PBR
- **Éditeur visuel** : Design de scènes, placement d'objets
- **Physique** : Intégration de moteurs physiques (Kraft)
- **Audio spatial** : Son 3D, effets sonores
- **Animation** : Squelettes, skinning, interpolation
- **Multi-plateforme** : Un code, plusieurs plateformes
- **Formats supportés** : glTF, X3D, VRML, Spine

**Exemple simple :**

```pascal
uses
  CastleWindow, CastleScene, CastleViewport,
  CastleVectors, CastleTransform;

var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  Scene: TCastleScene;

begin
  Window := TCastleWindow.Create(Application);

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := True;
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle.gltf');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := True;

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Window.OpenAndRun;
end.
```

**Cas d'usage :**

- Jeux 3D (FPS, RPG, aventure)
- Jeux 2D avec effets 3D
- Applications de visualisation 3D
- Prototypes rapides
- Serious games et simulations

**Pourquoi c'est notable :**

- Moteur complet et mature (20+ ans de développement)
- Excellente documentation et tutoriels
- Éditeur visuel professionnel
- Support commercial disponible
- Utilisé pour des jeux publiés commercialement

### Brook Framework

**Catégorie** : Framework Web  
**Auteur** : Silvio Clecio et contributeurs  
**Licence** : LGPL  
**URL** : https://github.com/risoflora/brookframework  

**Description :**

Brook est un framework web léger et rapide pour créer des applications web et APIs REST avec FreePascal. Philosophie minimaliste inspirée de Flask (Python) et Express (Node.js).

**Fonctionnalités principales :**

- **Routage flexible** : Routes RESTful avec paramètres
- **Middleware** : Chaîne de traitement des requêtes
- **Sessions** : Gestion des sessions HTTP
- **Template engine** : Génération HTML dynamique
- **Upload/Download** : Gestion de fichiers
- **WebSockets** : Communication bidirectionnelle
- **Performance** : Très rapide et faible empreinte mémoire

**Exemple d'API REST :**

```pascal
uses
  BrookApplication, BrookHTTPServer, BrookHTTPRouter;

type
  TUserRoute = class(TBrookHTTPRoute)
  public
    procedure Get; override;
    procedure Post; override;
  end;

procedure TUserRoute.Get;
begin
  Response
    .Json('{"users": [{"id": 1, "name": "John"}]}')
    .Status(200)
    .Send;
end;

procedure TUserRoute.Post;
var
  Name: string;
begin
  Name := Request.Params['name'];
  Response
    .Json(Format('{"created": true, "name": "%s"}', [Name]))
    .Status(201)
    .Send;
end;

begin
  BrookApp.Routes
    .Get('/users', TUserRoute)
    .Post('/users', TUserRoute);
  BrookApp.Run;
end.
```

**Cas d'usage :**

- APIs REST/RESTful
- Backends pour SPAs (React, Vue, Angular)
- Microservices
- Applications web légères
- Prototypes rapides

**Pourquoi c'est notable :**

- Très simple à apprendre et utiliser
- Performance exceptionnelle
- Code propre et bien organisé
- Active development
- Bon pour débuter avec le web en Pascal

## Bibliothèques graphiques et multimédia

### BGRABitmap

**Catégorie** : Graphiques 2D  
**Auteur** : Circular et contributeurs  
**Licence** : Modifiée LGPL  
**URL** : https://github.com/bgrabitmap/bgrabitmap  

**Description :**

BGRABitmap est une bibliothèque de graphiques 2D offrant des fonctionnalités avancées avec anti-aliasing, transparence alpha, et effets visuels. Souvent décrite comme "la bibliothèque graphique que Lazarus aurait dû avoir".

**Fonctionnalités principales :**

- **Anti-aliasing** : Lignes, courbes, polygones lisses
- **Transparence alpha** : Compositing avancé
- **Filtres** : Blur, sharpen, emboss, etc.
- **Textures** : Remplissage avec motifs et gradients
- **Transformations** : Rotation, scaling, perspective
- **Texte avancé** : Rendu de texte avec effets
- **Couches** : Layers avec modes de fusion
- **Performance** : Optimisations SSE2/AVX

**Exemple :**

```pascal
uses
  BGRABitmap, BGRABitmapTypes;

procedure DrawExample;
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(800, 600, BGRAWhite);
  try
    // Dessiner un cercle avec dégradé
    Bmp.GradientFill(0, 0, 800, 600,
      BGRA(100, 150, 200), BGRA(200, 100, 150),
      gtRadial, PointF(400, 300), PointF(600, 300),
      dmSet);

    // Cercle avec anti-aliasing
    Bmp.FillEllipseAntialias(400, 300, 150, 150,
      BGRA(255, 100, 100, 200));

    // Texte avec ombre
    Bmp.FontHeight := 48;
    Bmp.TextOut(300, 280, 'Hello BGRABitmap!',
      BGRA(255, 255, 255, 255), taCenter);

    Bmp.SaveToFile('output.png');
  finally
    Bmp.Free;
  end;
end;
```

**Cas d'usage :**

- Applications graphiques créatives
- Visualisation de données
- Jeux 2D
- Traitement d'images
- Génération de graphiques dynamiques
- Interfaces utilisateur personnalisées

**Pourquoi c'est notable :**

- Qualité professionnelle du rendu
- Très complète et bien documentée
- Performance excellente
- Utilisée dans de nombreux projets
- Alternative puissante à l'API Canvas standard

### TAChart

**Catégorie** : Graphiques et diagrammes  
**Auteur** : Luiz Américo et contributeurs  
**Licence** : LGPL (inclus dans Lazarus)  
**URL** : Dans Lazarus (components/tachart)  

**Description :**

TAChart est une bibliothèque de création de graphiques et diagrammes intégrée à Lazarus. Elle permet de créer facilement des visualisations de données professionnelles.

**Fonctionnalités principales :**

- **Types de graphiques** : Ligne, barre, aire, camembert, etc.
- **Multi-séries** : Plusieurs séries sur un graphique
- **Axes personnalisables** : Échelles, labels, grilles
- **Interactivité** : Zoom, pan, tooltips
- **Export** : PNG, SVG, PDF
- **Animation** : Transitions et effets
- **Performance** : Gestion de grands datasets

**Exemple :**

```pascal
uses
  TAGraph, TASeries, TATools;

procedure CreateChart(Chart: TChart);
var
  Series: TLineSeries;
  i: Integer;
begin
  Series := TLineSeries.Create(Chart);
  Series.Title := 'Sales 2024';

  // Ajouter des points
  for i := 1 to 12 do
    Series.AddXY(i, Random(100) + 50);

  Chart.AddSeries(Series);

  // Configuration
  Chart.Title.Text.Text := 'Monthly Sales';
  Chart.BottomAxis.Title.Caption := 'Month';
  Chart.LeftAxis.Title.Caption := 'Sales ($)';
  Chart.Legend.Visible := True;
end;
```

**Cas d'usage :**

- Tableaux de bord (dashboards)
- Rapports statistiques
- Applications scientifiques
- Outils de monitoring
- Applications de business intelligence

**Pourquoi c'est notable :**

- Intégré à Lazarus (pas de dépendance externe)
- Très facile à utiliser
- Qualité professionnelle
- Bien maintenu et documenté
- Nombreux exemples fournis

## Bibliothèques réseau et communication

### Synapse

**Catégorie** : Réseau et protocoles  
**Auteur** : Lukas Gebauer  
**Licence** : BSD-like (très permissive)  
**URL** : http://synapse.ararat.cz/  

**Description :**

Synapse est une bibliothèque réseau complète supportant de nombreux protocoles. C'est une alternative légère et portable à Indy pour les communications réseau.

**Protocoles supportés :**

- **HTTP/HTTPS** : Client et serveur
- **SMTP** : Envoi d'emails
- **POP3/IMAP** : Réception d'emails
- **FTP** : Transfert de fichiers
- **SNMP** : Monitoring réseau
- **DNS** : Résolution de noms
- **Telnet** : Connexions interactives
- **LDAP** : Annuaires
- **Et bien d'autres...**

**Exemple HTTP :**

```pascal
uses
  httpsend, ssl_openssl;

function DownloadPage(const URL: string): string;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    if HTTP.HTTPMethod('GET', URL) then
    begin
      SetLength(Result, HTTP.Document.Size);
      HTTP.Document.Read(Result[1], HTTP.Document.Size);
    end;
  finally
    HTTP.Free;
  end;
end;

// Usage
var
  HTML: string;
begin
  HTML := DownloadPage('https://example.com');
  WriteLn(HTML);
end;
```

**Exemple SMTP (envoi email) :**

```pascal
uses
  smtpsend, ssl_openssl, mimemess, mimepart;

procedure SendEmail(const ToAddr, Subject, Body: string);
var
  Msg: TMimeMess;
  Part: TMimePart;
begin
  Msg := TMimeMess.Create;
  try
    Msg.Header.From := 'sender@example.com';
    Msg.Header.ToList.Add(ToAddr);
    Msg.Header.Subject := Subject;

    Part := Msg.AddPartMultipart('text/plain', nil);
    Part.DecodedLines.Text := Body;

    if not SendToRaw('sender@example.com', ToAddr,
                     'smtp.example.com', Msg.Lines) then
      raise Exception.Create('Failed to send email');
  finally
    Msg.Free;
  end;
end;
```

**Cas d'usage :**

- Clients HTTP/HTTPS
- Applications email (client ou automatisation)
- Transfert de fichiers FTP
- Outils de monitoring réseau
- Clients pour protocoles legacy
- Scripts système et automation

**Pourquoi c'est notable :**

- Très légère (pas de dépendances lourdes)
- Portable (Windows, Linux, BSD, etc.)
- Stable et éprouvée
- Code simple et lisible
- Bien documentée
- Licence très permissive

### Indy (Internet Direct)

**Catégorie** : Composants réseau  
**Auteur** : Chad Z. Hower et équipe Indy  
**Licence** : Double license (BSD-like + commercial)  
**URL** : https://github.com/IndySockets/Indy  

**Description :**

Indy est une suite complète de composants réseau, portée de Delphi vers FreePascal/Lazarus. Plus lourde que Synapse mais offrant plus de fonctionnalités haut niveau.

**Fonctionnalités :**

- Composants visuels pour Lazarus
- Client/Serveur pour divers protocoles
- Support SSL/TLS natif
- Architecture orientée composants
- Thread pooling intégré

**Exemple serveur TCP :**

```pascal
uses
  IdTCPServer, IdContext;

type
  TMyServer = class
  private
    FServer: TIdTCPServer;
    procedure OnExecute(AContext: TIdContext);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
  end;

constructor TMyServer.Create;
begin
  FServer := TIdTCPServer.Create(nil);
  FServer.DefaultPort := 8080;
  FServer.OnExecute := @OnExecute;
end;

procedure TMyServer.OnExecute(AContext: TIdContext);
var
  Line: string;
begin
  Line := AContext.Connection.IOHandler.ReadLn;
  AContext.Connection.IOHandler.WriteLn('Echo: ' + Line);
end;

procedure TMyServer.Start;
begin
  FServer.Active := True;
  WriteLn('Server listening on port 8080');
end;
```

**Cas d'usage :**

- Serveurs TCP/UDP personnalisés
- Applications client/serveur
- Services réseau
- Migration depuis Delphi avec Indy

**Pourquoi c'est notable :**

- Port de la bibliothèque Delphi de référence
- Composants visuels pratiques
- Largement utilisée
- Compatible avec code Delphi existant

## Bases de données

### ZEOS Database Objects (ZDBC)

**Catégorie** : Accès base de données  
**Auteur** : Équipe ZEOS  
**Licence** : LGPL  
**URL** : https://sourceforge.net/projects/zeoslib/  

**Description :**

ZEOS est une suite de composants d'accès aux bases de données pour plusieurs SGBD. Alternative populaire à SQLdb avec support de nombreuses bases.

**SGBD supportés :**

- MySQL/MariaDB
- PostgreSQL
- SQLite
- Firebird/InterBase
- Oracle
- MS SQL Server
- DB2
- Sybase

**Fonctionnalités :**

- Connexion native (pas d'ODBC requis)
- Performance optimisée
- Composants visuels pour Lazarus
- Support transactions
- Stored procedures
- Unicode complet

**Exemple :**

```pascal
uses
  ZConnection, ZDataset;

var
  Connection: TZConnection;
  Query: TZQuery;

begin
  Connection := TZConnection.Create(nil);
  try
    Connection.Protocol := 'postgresql';
    Connection.HostName := 'localhost';
    Connection.Database := 'mydb';
    Connection.User := 'postgres';
    Connection.Password := 'secret';
    Connection.Connect;

    Query := TZQuery.Create(nil);
    try
      Query.Connection := Connection;
      Query.SQL.Text := 'SELECT * FROM users WHERE active = true';
      Query.Open;

      while not Query.Eof do
      begin
        WriteLn(Query.FieldByName('username').AsString);
        Query.Next;
      end;
    finally
      Query.Free;
    end;
  finally
    Connection.Free;
  end;
end.
```

**Cas d'usage :**

- Applications bases de données multi-SGBD
- Migration entre différentes bases
- Applications nécessitant performance optimale
- Projets avec composants visuels data-aware

**Pourquoi c'est notable :**

- Support de nombreux SGBD
- Performance excellente
- Composants matures et stables
- Grande communauté d'utilisateurs
- Alternative solide à SQLdb

### tiOPF (TechInsite Object Persistence Framework)

**Catégorie** : ORM et persistance  
**Auteur** : Graeme Geldenhuys et contributeurs  
**Licence** : BSD  
**URL** : https://github.com/graemeg/tiopf  

**Description :**

tiOPF est un framework de persistance objet qui sépare complètement la logique métier de la couche de persistance. Architecture Model-View-Controller (MVC) intégrée.

**Fonctionnalités :**

- Mapping objet-relationnel
- Support multi-SGBD
- Validations métier
- Architecture en couches
- Design patterns intégrés
- Tests unitaires inclus

**Exemple :**

```pascal
type
  TPerson = class(TtiObject)
  private
    FName: string;
    FEmail: string;
    FAge: Integer;
  published
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
    property Age: Integer read FAge write FAge;
  end;

var
  Person: TPerson;
  List: TtiObjectList;
begin
  // Créer et sauvegarder
  Person := TPerson.Create;
  Person.Name := 'John Doe';
  Person.Email := 'john@example.com';
  Person.Age := 30;
  Person.Save;

  // Charger une liste
  List := TtiObjectList.Create;
  GTIOPFManager.VisitorManager.Load(List, TPerson,
    'Age > 25', 'Name');
end;
```

**Cas d'usage :**

- Applications enterprise avec logique métier complexe
- Projets nécessitant séparation stricte des couches
- Applications multi-bases de données
- Projets avec forte orientation objet

**Pourquoi c'est notable :**

- Architecture propre et professionnelle
- Bien documenté avec nombreux exemples
- Tests unitaires complets
- Utilisé en production dans grandes entreprises

## Outils et utilitaires

### Pas2JS (Pascal to JavaScript Transpiler)

**Catégorie** : Transpileur  
**Auteur** : Mattias Gaertner et équipe FPC  
**Licence** : LGPL  
**URL** : http://wiki.freepascal.org/pas2js  

**Description :**

Pas2JS est un transpileur qui convertit du code Pascal en JavaScript, permettant d'utiliser Pascal pour le développement web frontend.

**Fonctionnalités :**

- Syntaxe Object Pascal complète
- Classes, interfaces, génériques
- Génération JavaScript moderne
- Source maps pour debug
- Intégration avec frameworks JS (React, Vue)
- Node.js backend possible

**Exemple :**

```pascal
// Pascal
program WebApp;

{$mode objfpc}

uses
  JS, Web;

procedure ButtonClick(Event: TJSMouseEvent);
begin
  document.getElementById('output').innerHTML :=
    'Hello from Pascal!';
end;

begin
  document.getElementById('myButton')
    .addEventListener('click', @ButtonClick);
end.
```

Génère du JavaScript qui s'exécute dans le navigateur.

**Cas d'usage :**

- Applications web frontend en Pascal
- SPAs (Single Page Applications)
- Migration de code Pascal vers le web
- Partage de logique entre serveur (FPC) et client (Pas2JS)

**Pourquoi c'est notable :**

- Permet d'utiliser Pascal pour le web
- Syntaxe familière pour développeurs Pascal
- Intégration avec écosystème JavaScript
- Maintenu par l'équipe FreePascal

### LazProfiler

**Catégorie** : Profiling et optimisation  
**Auteur** : Mattias Gaertner  
**Licence** : GPL  
**URL** : https://wiki.lazarus.freepascal.org/LazProfiler  

**Description :**

LazProfiler est un profileur pour identifier les goulots d'étranglement de performance dans vos applications FreePascal/Lazarus.

**Fonctionnalités :**

- Profiling instrumenté du code
- Mesure précise du temps d'exécution
- Comptage d'appels de fonctions
- Graphique de call graph
- Export des résultats
- Interface graphique intégrée

**Utilisation :**

1. Ajouter `-gp` aux options de compilation
2. Compiler et exécuter l'application
3. Un fichier `gmon.out` est généré
4. Analyser avec LazProfiler

**Cas d'usage :**

- Optimisation de performance
- Identification de fonctions lentes
- Analyse de complexité algorithmique
- Recherche de fuites de performance

**Pourquoi c'est notable :**

- Intégré à l'IDE Lazarus
- Facile à utiliser
- Résultats visuels clairs
- Essentiel pour optimisation

### FPCUnit (Free Pascal Unit Testing)

**Catégorie** : Tests unitaires  
**Auteur** : Équipe FreePascal  
**Licence** : LGPL  
**URL** : Inclus dans FreePascal (packages/fcl-fpcunit)  

**Description :**

FPCUnit est le framework de tests unitaires standard pour FreePascal, similaire à JUnit/NUnit.

**Fonctionnalités :**

- Assertions complètes
- Setup/TearDown automatiques
- Test suites et runners
- Rapports HTML/XML
- Intégration CI/CD
- Test fixtures

**Exemple :**

```pascal
unit TestCalculator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Calculator;

type
  TTestCalculator = class(TTestCase)
  published
    procedure TestAddition;
    procedure TestDivision;
    procedure TestDivisionByZero;
  end;

implementation

procedure TTestCalculator.TestAddition;
begin
  AssertEquals('2 + 3 = 5', 5, Add(2, 3));
  AssertEquals('0 + 0 = 0', 0, Add(0, 0));
  AssertEquals('-5 + 3 = -2', -2, Add(-5, 3));
end;

procedure TTestCalculator.TestDivision;
begin
  AssertEquals('10 / 2 = 5', 5.0, Divide(10, 2), 0.001);
end;

procedure TTestCalculator.TestDivisionByZero;
begin
  try
    Divide(10, 0);
    Fail('Should raise exception');
  except
    on E: EDivByZero do
      ; // Expected
  end;
end;

initialization
  RegisterTest(TTestCalculator);

end.
```

**Cas d'usage :**

- Tests unitaires systématiques
- TDD (Test-Driven Development)
- Régression testing
- CI/CD pipelines

**Pourquoi c'est notable :**

- Framework standard de FreePascal
- Architecture solide et éprouvée
- Bien intégré avec outils
- Nombreux exemples disponibles

## Applications complètes

### Double Commander

**Catégorie** : Gestionnaire de fichiers  
**Auteur** : Communauté (Alexander Koblov et autres)  
**Licence** : GPL  
**URL** : https://doublecmd.sourceforge.io/  

**Description :**

Double Commander est un gestionnaire de fichiers open source multi-plateforme à deux panneaux, inspiré de Total Commander.

**Fonctionnalités :**

- Interface à deux panneaux
- Support archives (ZIP, RAR, TAR, etc.)
- Éditeur/visionneuse intégrés
- Recherche avancée
- Opérations par lot
- Plugins et extensions
- Multilingue
- Portable

**Pourquoi c'est notable :**

- Application desktop complète en Pascal
- Qualité professionnelle
- Utilisée par des millions d'utilisateurs
- Démontre la maturité de Lazarus pour applications complexes
- Code source excellent pour apprendre

### Lazarus IDE lui-même

**Catégorie** : IDE  
**Auteur** : Équipe Lazarus  
**Licence** : GPL/LGPL  
**URL** : https://www.lazarus-ide.org/  

**Description :**

L'IDE Lazarus est lui-même écrit en FreePascal avec Lazarus ! C'est un excellent exemple de ce qui peut être accompli.

**Pourquoi étudier son code :**

- Architecture d'un IDE complet
- Gestion de plugins
- Éditeur de code avancé (SynEdit)
- Designer de formulaires
- Système de packages
- Patterns et techniques avancées

**Où trouver :**

Le code source est dans votre installation Lazarus ou sur GitLab.

### fpGUI Toolkit

**Catégorie** : Toolkit GUI  
**Auteur** : Graeme Geldenhuys  
**Licence** : LGPL modifiée  
**URL** : https://github.com/graemeg/fpGUI  

**Description :**

fpGUI est un toolkit d'interface graphique entièrement écrit en Pascal, sans dépendances externes (pas de GTK, Qt, ou Win32 API). Rendu direct via X11, GDI ou Agg.

**Fonctionnalités :**

- Indépendant des toolkits natifs
- Look & feel consistant sur toutes plateformes
- Widgets personnalisables
- Thèmes
- Pas de dépendances externes
- Très léger

**Exemple :**

```pascal
program fpGUIApp;

{$mode objfpc}{$H+}

uses
  fpg_base, fpg_main, fpg_form, fpg_button;

type
  TMainForm = class(TfpgForm)
  private
    Button: TfpgButton;
    procedure ButtonClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Title := 'fpGUI Example';
  SetPosition(100, 100, 400, 300);

  Button := TfpgButton.Create(Self);
  Button.Text := 'Click Me';
  Button.SetPosition(150, 120, 100, 30);
  Button.OnClick := @ButtonClicked;
end;

procedure TMainForm.ButtonClicked(Sender: TObject);
begin
  ShowMessage('Hello from fpGUI!');
end;

var
  Form: TMainForm;

begin
  fpgApplication.Initialize;
  Form := TMainForm.Create(nil);
  try
    Form.Show;
    fpgApplication.Run;
  finally
    Form.Free;
  end;
end.
```

**Cas d'usage :**

- Applications nécessitant look & feel consistant
- Systèmes embarqués sans toolkit standard
- Applications légères sans dépendances
- Prototypage d'interfaces custom

**Pourquoi c'est notable :**

- Approche unique (pas de wrapper)
- Très portable
- Performances excellentes
- Code Pascal pur

## Découvrir plus de projets

### Ressources pour trouver des projets

**1. GitHub / GitLab**

Recherche avancée :
```
language:Pascal stars:>50
language:Pascal topic:lazarus
language:Pascal topic:freepascal
```

Organisations notables :
- https://github.com/LongDirtyAnimAlf (FPC cross-compilers)
- https://github.com/circular-software (BGRABitmap)
- https://github.com/synopse (mORMot)

**2. Lazarus Online Package Manager (OPM)**

Dans Lazarus :
- Package → Online Package Manager
- Browse par catégorie
- Trier par popularité/date

**3. Lazarus-CCR (Code and Component Repository)**

- https://sourceforge.net/projects/lazarus-ccr/
- Centaines de packages communautaires
- Code examples et snippets

**4. FreePascal Wiki**

- https://wiki.freepascal.org/Projects
- Liste de projets catégorisés
- Liens et descriptions

**5. Awesome Pascal**

- https://github.com/Fr0sT-Brutal/awesome-pascal
- Liste curatée de ressources
- Projets, bibliothèques, outils

**6. Forums et communauté**

- Forum Lazarus : Section "Third Party"
- Reddit r/lazarus
- Stack Overflow tag:freepascal

### Évaluer un projet avant utilisation

**Checklist d'évaluation :**

```markdown
## Documentation
- [ ] README complet
- [ ] Exemples de code
- [ ] API documentée
- [ ] Changelog

## Activité
- [ ] Commit récent (<6 mois)
- [ ] Issues répondues
- [ ] Releases régulières

## Qualité
- [ ] Tests unitaires
- [ ] CI/CD configuré
- [ ] Code review visible
- [ ] Coding style cohérent

## Communauté
- [ ] Plusieurs contributeurs
- [ ] Forum/Discord actif
- [ ] Questions/réponses sur forums

## Compatibilité
- [ ] FPC version supportée
- [ ] Plateformes testées
- [ ] Dépendances claires

## Licence
- [ ] Licence spécifiée
- [ ] Compatible avec votre usage
- [ ] Dépendances vérifiées
```

## Utiliser un projet open source

### Installation typique

**1. Via OPM (Lazarus) :**

```
Package → Online Package Manager
→ Search for package
→ Install
→ Rebuild Lazarus
```

**2. Via Git :**

```bash
# Cloner le projet
git clone https://github.com/username/project.git

# Ouvrir le .lpk dans Lazarus
# Package → Open Package File → project.lpk
# Compile → Install
```

**3. Manuel :**

```bash
# Télécharger ZIP
wget https://github.com/user/project/archive/main.zip
unzip main.zip

# Copier dans répertoire Lazarus
cp -r project ~/.lazarus/packages/

# Ajouter au projet
# Project Inspector → Add → Requirement
```

### Intégration dans un projet

**Exemple complet :**

Utilisons BGRABitmap dans un projet.

**1. Installation :**

Via OPM : Installer "BGRABitmap"

**2. Créer un projet :**

```pascal
program ImageProcessor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes;

procedure ProcessImage(const InputFile, OutputFile: string);
var
  Bmp: TBGRABitmap;
begin
  // Charger
  Bmp := TBGRABitmap.Create(InputFile);
  try
    // Appliquer un flou
    BGRAReplace(Bmp, Bmp.FilterBlurRadial(10, rbFast));

    // Ajouter du texte
    Bmp.FontHeight := 48;
    Bmp.TextOut(50, 50, 'Processed!',
      ColorToBGRA(clWhite), taLeftJustify);

    // Sauvegarder
    Bmp.SaveToFile(OutputFile);
  finally
    Bmp.Free;
  end;
end;

begin
  if ParamCount < 2 then
  begin
    WriteLn('Usage: imageprocessor input.png output.png');
    Exit;
  end;

  ProcessImage(ParamStr(1), ParamStr(2));
  WriteLn('Done!');
end.
```

**3. Configuration du projet :**

Project Inspector → Required Packages → Add → BGRABitmap

**4. Compilation :**

```bash
lazbuild imageprocessor.lpi
./imageprocessor input.png output.png
```

### Gérer les versions

**Utiliser des versions stables :**

```bash
# Cloner
git clone https://github.com/user/project.git
cd project

# Lister les versions
git tag -l

# Checkout d'une version stable
git checkout v2.5.0

# Ou utiliser les releases GitHub
wget https://github.com/user/project/archive/refs/tags/v2.5.0.zip
```

**Dans votre projet :**

Documentez les versions utilisées :

```markdown
# Dependencies

- BGRABitmap 11.5.3
- Synapse 40.1
- mORMot2 2.0.stable
```

## Contribuer à un projet

### Pourquoi contribuer ?

**Avantages personnels :**
- Améliorer vos compétences
- Apprendre de code de qualité
- Résoudre vos propres bugs
- Ajouter les fonctionnalités dont vous avez besoin
- Portfolio professionnel

**Avantages communautaires :**
- Améliorer l'écosystème
- Aider d'autres utilisateurs
- Pérenniser les projets
- Créer des relations

### Types de contributions

**1. Rapporter des bugs :**

Template d'issue :

````markdown
**Describe the bug**
When I call `Function X` with parameter Y, I get error Z.

**To Reproduce**
```pascal
// Minimal code to reproduce
procedure Test;
begin
  MyFunction(InvalidParam);  // Crash here
end;
```

**Expected behavior**
Should return default value or raise exception.

**Environment**
- OS: Ubuntu 22.04
- FPC: 3.2.2
- Package version: 2.5.0

**Additional context**
Stack trace:
````
...
```
```

**2. Corriger des bugs :**

Processus :
1. Fork le projet
2. Créer une branche : `git checkout -b fix-issue-123`
3. Corriger le bug
4. Ajouter un test si possible
5. Commit : `git commit -m "Fix #123: Description"`
6. Push et créer une Pull Request

**3. Ajouter des fonctionnalités :**

Avant de coder :
- Ouvrir une issue pour discuter
- Vérifier que c'est accepté par les mainteneurs
- S'assurer que ça rentre dans le scope du projet

Puis :
- Développer avec tests
- Documenter (comments + README)
- Exemples d'utilisation
- PR avec description détaillée

**4. Améliorer la documentation :**

Très apprécié et souvent plus simple :
- Corriger des fautes
- Clarifier des explications
- Ajouter des exemples
- Traduire
- Créer des tutoriels

**5. Créer des exemples :**

```
examples/
├── basic/
│   └── hello_world.lpr
├── intermediate/
│   └── database_app.lpr
└── advanced/
    └── multi_threaded_server.lpr
```

**6. Tests et validation :**

- Tester sur différentes plateformes
- Rapporter les problèmes de compatibilité
- Valider les fixes proposés
- Participer aux bêta tests

### Bonnes pratiques de contribution

**Code :**

✅ Suivre le style du projet existant  
✅ Commits atomiques et bien décrits  
✅ Ajouter des tests si possible  
✅ Pas de reformatage massif (sauf si demandé)  
✅ Un seul sujet par PR

**Communication :**

✅ Être poli et respectueux  
✅ Accepter les critiques constructives  
✅ Répondre aux commentaires de review  
✅ Être patient (mainteneurs souvent bénévoles)  
✅ Remercier pour le temps accordé

**Processus :**

1. **Lire CONTRIBUTING.md** si existe
2. **Ouvrir une issue** pour discuter grandes fonctionnalités
3. **Fork et branch** pour vos modifications
4. **Tests locaux** avant de soumettre
5. **PR description** claire avec contexte
6. **Itérer** selon les retours

## Licences open source

### Principales licences

**GPL (General Public License) :**

- Code dérivé doit être GPL
- Source code obligatoire si distribution
- Modifications doivent être partagées
- Forte protection copyleft

**LGPL (Lesser GPL) :**

- Linking autorisé sans contamination
- Modifications de la bibliothèque doivent être partagées
- Code utilisant la bibliothèque peut rester propriétaire
- Souvent utilisée pour bibliothèques

**LGPL modifiée (FPC RTL) :**

- Variante pour FreePascal RTL
- Permet static linking sans contamination
- Très permissive
- FreePascal et Lazarus l'utilisent

**MIT / BSD :**

- Très permissives
- Usage commercial OK
- Pas d'obligation de partager modifications
- Attribution requise

**Apache 2.0 :**

- Permissive avec protection brevets
- Usage commercial OK
- Contributions sous même licence
- Populaire dans l'entreprise

### Compatibilité des licences

**Matrice de compatibilité simplifiée :**

| Votre projet | Peut utiliser | Ne peut pas utiliser |
|--------------|---------------|---------------------|
| Propriétaire | MIT, BSD, Apache, LGPL | GPL |
| GPL | Toutes | - |
| LGPL | MIT, BSD, Apache, LGPL | - |
| MIT/BSD | MIT, BSD, Apache | - |

**Attention :**

⚠️ Vérifier TOUTES les dépendances transitives  
⚠️ Certaines licences ont des conditions spécifiques  
⚠️ En cas de doute, consulter un juriste

### Choisir une licence pour votre projet

**Critères de choix :**

1. **Voulez-vous du copyleft fort ?**
   → GPL

2. **Bibliothèque pour usage large ?**
   → LGPL ou MIT

3. **Protéger contre brevets ?**
   → Apache 2.0

4. **Maximum de liberté ?**
   → MIT ou BSD

5. **Compatibilité Delphi/FPC ?**
   → LGPL modifiée

**Ajouter une licence :**

1. Fichier `LICENSE.txt` à la racine
2. Header dans chaque fichier source
3. Mention dans README.md

```pascal
{
  This file is part of MyProject.

  MyProject is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of
  the License, or (at your option) any later version.

  MyProject is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU Lesser General Public License for more details.
}
```

## Études de cas : Utilisation pratique des projets open source

Cette section présente des scénarios réels d'utilisation des projets open source présentés précédemment. L'objectif est de vous montrer comment ces bibliothèques s'intègrent dans des projets concrets et comment les choisir selon vos besoins.

### Étude de cas 1 : Application web REST complète

**Contexte :** Vous devez créer un backend REST pour une application mobile de gestion de tâches, avec authentification, base de données PostgreSQL, et déploiement sur Linux.

**Projets utilisés :**
- **Brook Framework** : Pour le serveur web et les routes REST
- **ZEOS** : Pour l'accès à PostgreSQL
- **DCPCrypt** : Pour le hashing des mots de passe
- **Synapse** : Pour l'envoi d'emails de notification

**Architecture :**

```
Client Mobile
    ↓
Brook REST API
    ↓
ZEOS ← → PostgreSQL
```

**Pourquoi ces choix ?**

- **Brook** : Léger, rapide, idéal pour APIs REST simples. Plus accessible que mORMot pour débuter.
- **ZEOS** : Support natif PostgreSQL, composants visuels si besoin de maintenance.
- **DCPCrypt** : Simple pour hashing de mots de passe, pas de dépendance externe.
- **Synapse** : Légère, parfaite pour fonctionnalités email basiques.

**Alternative plus complexe :**

Si le projet évoluait vers un système distribué avec microservices, vous pourriez migrer vers **mORMot 2** qui offre :
- ORM plus sophistiqué
- Support natif des microservices
- Authentification JWT intégrée
- Meilleures performances à grande échelle

**Leçon :** Commencez simple avec Brook, migrez vers mORMot si la complexité l'exige.

---

### Étude de cas 2 : Application de visualisation scientifique

**Contexte :** Logiciel desktop multi-plateforme pour visualiser des données scientifiques (graphiques, images, traitement).

**Projets utilisés :**
- **Lazarus LCL** : Interface graphique portable
- **TAChart** : Graphiques et courbes
- **BGRABitmap** : Traitement d'images et effets visuels
- **NumLib** : Calculs numériques (interpolation, régression)
- **SQLite** (via SQLdb) : Stockage local des données

**Architecture :**

```
Interface LCL
├── TAChart (graphiques)
├── BGRABitmap (images)
└── NumLib (calculs)
     ↓
   SQLite (données)
```

**Pourquoi ces choix ?**

- **LCL** : Portable Windows/Linux/macOS sans effort
- **TAChart** : Intégré à Lazarus, excellent pour graphiques 2D scientifiques
- **BGRABitmap** : Rendu de qualité, anti-aliasing, transformations
- **NumLib** : Bibliothèque FreePascal native, pas de binding externe
- **SQLite** : Base embarquée, pas de serveur à gérer

**Points d'attention :**

Pour visualisation 3D intensive, considérer **Castle Game Engine** ou OpenGL direct avec **fpGUI** pour le contrôle total.

**Leçon :** TAChart et BGRABitmap couvrent 90% des besoins de visualisation scientifique sans complexité.

---

### Étude de cas 3 : Service système multi-plateforme

**Contexte :** Daemon/Service qui surveille des fichiers et envoie des alertes (Windows Service + systemd sur Linux).

**Projets utilisés :**
- **Synapse** : Pour envoi HTTP/SMTP des alertes
- **fpTimer** : Gestion des intervalles
- **Composants natifs** : TService (Windows) / Daemon (Linux)

**Code spécifique plateforme :**

```pascal
{$IFDEF WINDOWS}
type
  TMonitorService = class(TService)
  // Utilisation de l'API Windows Service
{$ENDIF}

{$IFDEF LINUX}
type
  TMonitorDaemon = class(TDaemonApplication)
  // Utilisation de systemd
{$ENDIF}

// Code partagé
procedure SendAlert(const Message: string);
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    // Envoyer à un webhook
    HTTP.Document.Write(Message[1], Length(Message));
    HTTP.HTTPMethod('POST', 'https://api.alerts.com/webhook');
  finally
    HTTP.Free;
  end;
end;
```

**Pourquoi ces choix ?**

- **Synapse** : Pas de composants visuels nécessaires, parfait pour services
- **Compilation conditionnelle** : Code partagé maximum, spécifique quand nécessaire
- **Pas de bibliothèque lourde** : Service doit être léger

**Alternative :**

Pour monitoring complexe avec métriques, logs structurés, considérer l'intégration avec **mORMot** pour logging avancé et stockage.

**Leçon :** Les services système bénéficient de bibliothèques légères sans dépendances GUI.

---

### Étude de cas 4 : Migration d'application Delphi vers multi-plateforme

**Contexte :** Application Delphi Windows existante avec Indy + base de données, à porter sur Linux.

**Stratégie de migration :**

**Phase 1 - Évaluation :**
1. Identifier les composants Delphi utilisés
2. Vérifier la compatibilité FreePascal/Lazarus
3. Lister les API Windows spécifiques

**Phase 2 - Portage :**

| Composant Delphi | Remplacement FreePascal/Lazarus |
|------------------|--------------------------------|
| VCL (Forms, Controls) | **LCL** (compatible à 80-90%) |
| Indy (réseau) | **Indy pour FPC** ou **Synapse** |
| ADO/dbExpress | **ZEOS** ou **SQLdb** |
| Registry Windows | Abstraction multi-plateforme (INI ou XML) |
| WinAPI calls | **LCLIntf** ou compilation conditionnelle |

**Code avant (Delphi + Windows) :**

```pascal
uses
  Registry, Windows;

procedure SaveSetting(const Key, Value: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('Software\MyApp', True);
    Reg.WriteString(Key, Value);
  finally
    Reg.Free;
  end;
end;
```

**Code après (Multi-plateforme) :**

```pascal
uses
  IniFiles, SysUtils;

procedure SaveSetting(const Key, Value: string);
var
  Ini: TIniFile;
begin
  {$IFDEF WINDOWS}
  Ini := TIniFile.Create(GetAppConfigFile(False));
  {$ELSE}
  Ini := TIniFile.Create(GetUserDir + '.myapp.conf');
  {$ENDIF}
  try
    Ini.WriteString('Settings', Key, Value);
  finally
    Ini.Free;
  end;
end;
```

**Projets utiles pour migration :**

- **Lazarus IDE** : Convertisseur de projets Delphi (.dpr → .lpr, .dfm → .lfm)
- **fpGUI** : Alternative si la LCL ne suffit pas
- **JEDI Code Library** : Portée vers FPC, aide à la compatibilité

**Leçon :** 70% du code Delphi se porte sans modification, 20% nécessite des ajustements mineurs, 10% doit être réécrit.

---

### Étude de cas 5 : Jeu 2D indépendant multi-plateforme

**Contexte :** Développer un jeu 2D (platformer) pour Windows, Linux et possiblement Android.

**Projets utilisés :**
- **Castle Game Engine** : Moteur principal
- **OpenAL** (via Castle) : Audio
- **Physics Engine** intégré : Collisions
- **Pas2JS** (optionnel) : Version web du jeu

**Architecture :**

```
Castle Game Engine
├── Rendu 2D/3D
├── Gestion scènes
├── Input (clavier/souris/tactile)
├── Audio
└── Physique
```

**Pourquoi Castle plutôt que faire from scratch ?**

| Aspect | From Scratch | Castle Game Engine |
|--------|--------------|-------------------|
| Temps de développement | 6-12 mois | 1-3 mois |
| Portabilité | À implémenter | Incluse |
| Outils | À créer | Éditeur fourni |
| Communauté | Seul | Support et exemples |
| Mises à jour | Maintenance totale | Communauté maintient |

**Alternative pour projets simples :**

Pour jeux 2D très simples, **BGRABitmap + LCL** peut suffire :

```pascal
uses
  BGRABitmap, BGRABitmapTypes, Forms, ExtCtrls;

type
  TGameForm = class(TForm)
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FPlayerX, FPlayerY: Integer;
    FBuffer: TBGRABitmap;
  end;

procedure TGameForm.TimerTimer(Sender: TObject);
begin
  // Logique du jeu
  FPlayerX := FPlayerX + 5;
  Invalidate; // Déclenche FormPaint
end;

procedure TGameForm.FormPaint(Sender: TObject);
begin
  // Dessiner dans FBuffer
  FBuffer.Rectangle(FPlayerX, FPlayerY, FPlayerX+32, FPlayerY+32,
    BGRA(255, 0, 0), dmSet);

  // Afficher sur le formulaire
  FBuffer.Draw(Canvas, 0, 0, False);
end;
```

**Leçon :** Castle Game Engine évite de réinventer la roue pour jeux moyennement complexes. BGRABitmap suffit pour prototypes simples.

---

### Étude de cas 6 : Système de monitoring avec interface web

**Contexte :** Application qui collecte des métriques système et les affiche via interface web temps réel.

**Projets utilisés :**
- **fpWeb + FastCGI** : Backend web
- **Pas2JS** : Frontend (dashboard)
- **WebSockets** (Synapse) : Communication temps réel
- **SQLite** : Stockage métriques
- **TAChart** (côté serveur) : Génération de graphiques

**Architecture :**

```
Collecteur (thread)
    ↓
SQLite ← → fpWeb API ← → WebSocket
                              ↓
                    Frontend Pas2JS + Chart.js
```

**Pourquoi cette combinaison ?**

- **fpWeb** : Standard FreePascal, pas de dépendance externe
- **Pas2JS** : Un seul langage (Pascal) pour tout le projet
- **WebSocket via Synapse** : Updates temps réel sans polling
- **SQLite** : Historique des métriques, simple à déployer

**Alternative moderne :**

Utiliser **Brook Framework** + **JavaScript frontend** (React/Vue) si l'équipe préfère :

```pascal
// Brook endpoint
procedure TMetricsRoute.Get;
var
  Metrics: TJSONArray;
begin
  Metrics := CollectCurrentMetrics;
  Response
    .Json(Metrics.AsJSON)
    .Header('Access-Control-Allow-Origin', '*')
    .Send;
end;
```

**Leçon :** Pas2JS permet un développement full-stack Pascal, mais JavaScript reste une option valide côté client.

---

### Étude de cas 7 : Outil CLI pour automatisation DevOps

**Contexte :** Utilitaire en ligne de commande pour automatiser le déploiement sur plusieurs serveurs Linux.

**Projets utilisés :**
- **Synapse** : SSH et transferts de fichiers
- **fpJSON** : Parsing de configuration
- **FPCUnit** : Tests automatisés

**Exemple de structure :**

```pascal
program DeployTool;

uses
  SysUtils, Classes, fpjson, jsonparser,
  blcksock, ssl_openssl; // Synapse

type
  TServerConfig = record
    Host: string;
    User: string;
    KeyFile: string;
  end;

procedure DeployToServer(const Config: TServerConfig;
                         const LocalPath: string);
begin
  WriteLn('Deploying to ', Config.Host, '...');
  // Utiliser Synapse pour SSH/SCP
  // ...
  WriteLn('✓ Deployment successful');
end;

procedure LoadConfig(const Filename: string;
                     out Servers: array of TServerConfig);
var
  JSON: TJSONData;
  Arr: TJSONArray;
  i: Integer;
begin
  JSON := GetJSON(ReadFileAsString(Filename));
  try
    Arr := TJSONArray(JSON);
    SetLength(Servers, Arr.Count);
    for i := 0 to Arr.Count - 1 do
    begin
      with TJSONObject(Arr[i]) do
      begin
        Servers[i].Host := Get('host', '');
        Servers[i].User := Get('user', '');
        Servers[i].KeyFile := Get('keyfile', '');
      end;
    end;
  finally
    JSON.Free;
  end;
end;

begin
  if ParamCount < 2 then
  begin
    WriteLn('Usage: deploytool config.json /path/to/files');
    Exit;
  end;

  // Charger config et déployer
  // ...
end.
```

**Pourquoi ces choix ?**

- **Pas de GUI** : Outil CLI pur, compilation rapide
- **Synapse** : SSH natif sans dépendre d'outils externes
- **fpJSON** : Parsing JSON natif dans FPC
- **FPCUnit** : Tests des fonctions de déploiement

**Avantages FreePascal pour CLI :**

✅ Binaire statique sans dépendances  
✅ Démarrage instantané (vs Python/Ruby)  
✅ Cross-compilation facile (Linux/Windows)  
✅ Performance native  
✅ Déploiement simple (un seul fichier)

**Leçon :** FreePascal excelle pour outils CLI système, alternative crédible à Go/Rust avec syntaxe plus accessible.

---

## Matrice de sélection des projets

Pour vous aider à choisir rapidement les bons projets selon votre besoin :

### Par type de projet

| Type de projet | Projets recommandés | Niveau |
|----------------|---------------------|--------|
| **API REST simple** | Brook Framework | Débutant |
| **API REST enterprise** | mORMot 2 | Avancé |
| **Application desktop** | Lazarus LCL + composants | Débutant |
| **Jeu 2D/3D** | Castle Game Engine | Intermédiaire |
| **Application web** | fpWeb ou Brook + Pas2JS | Intermédiaire |
| **Outil CLI** | FreePascal pur + Synapse | Débutant |
| **Service/Daemon** | FreePascal pur + Synapse | Intermédiaire |
| **Graphiques avancés** | BGRABitmap | Intermédiaire |
| **Base de données** | ZEOS ou SQLdb | Débutant |
| **Calcul scientifique** | NumLib + TAChart | Intermédiaire |

### Par critère de performance

| Critère | Projets recommandés |
|---------|---------------------|
| **Vitesse maximale** | mORMot, Synapse, BGRABitmap |
| **Légèreté mémoire** | Synapse, Brook, fpWeb |
| **Startup rapide** | Synapse, Brook (vs Indy) |
| **Faible latence** | mORMot, Castle Game Engine |

### Par niveau de compétence

**Débutant (découverte FreePascal) :**
- Lazarus LCL
- Brook Framework
- Synapse
- TAChart
- SQLdb

**Intermédiaire (maîtrise Pascal) :**
- BGRABitmap
- Castle Game Engine
- ZEOS
- fpWeb
- Pas2JS

**Avancé (expert) :**
- mORMot 2
- tiOPF
- Contribution à Lazarus/FPC
- Développement de composants
- Bindings C/C++

---

## Checklist avant d'adopter un projet

Avant d'intégrer un projet open source dans votre code, vérifiez :

### Évaluation technique

- [ ] **Compatible avec ma version FPC ?** (vérifier 3.2.x vs 3.3.x)
- [ ] **Fonctionne sur mes plateformes cibles ?** (Win/Linux/macOS)
- [ ] **Dépendances acceptables ?** (bibliothèques externes, DLL/SO)
- [ ] **Performance adéquate ?** (benchmarks ou tests)
- [ ] **Exemples fonctionnels ?** (tester avant d'intégrer)
- [ ] **API stable ?** (breaking changes fréquents ?)

### Évaluation projet

- [ ] **Dernière release < 1 an ?** (projet actif)
- [ ] **Documentation claire ?** (README, wiki, exemples)
- [ ] **Issues répondues ?** (mainteneur réactif)
- [ ] **Tests présents ?** (qualité du code)
- [ ] **Plusieurs contributeurs ?** (pas de bus factor)
- [ ] **Licence compatible ?** (vérifier LGPL vs GPL vs MIT)

### Évaluation communauté

- [ ] **Forum/Discord actif ?** (support communautaire)
- [ ] **Tutoriels tiers ?** (popularité)
- [ ] **Utilisé en production ?** (projets connus qui l'utilisent)
- [ ] **Roadmap visible ?** (vision future)

### Risques à évaluer

⚠️ **Projet mono-contributeur** : Risque d'abandon  
⚠️ **Pas de release depuis 2+ ans** : Potentiellement mort  
⚠️ **Issues ouvertes non traitées** : Mainteneur débordé/parti  
⚠️ **Licence GPL pour usage commercial** : Incompatibilité juridique  
⚠️ **Dépendances obsolètes** : Maintenance future problématique

---

## Stratégies d'intégration progressive

### Approche 1 : Test isolé

Avant d'intégrer dans votre projet principal :

```
projet_test/
├── test_mormot.lpr        # Test isolé mORMot
├── test_bgrabitmap.lpr    # Test isolé BGRABitmap
└── benchmarks/
    ├── bench_synapse.lpr
    └── bench_indy.lpr
```

**Avantages :**
- Pas de pollution du projet principal
- Tests de performance comparatifs
- Apprentissage sans risque
- Facilite le choix final

### Approche 2 : Abstraction

Créer une couche d'abstraction pour faciliter le remplacement :

```pascal
// Abstraction
unit UDataAccess;

interface

type
  IDataAccess = interface
    procedure Connect(const Params: string);
    function ExecuteQuery(const SQL: string): TDataSet;
  end;

implementation

// Implémentation actuelle avec ZEOS
type
  TZeosDataAccess = class(TInterfacedObject, IDataAccess)
  private
    FConnection: TZConnection;
  public
    procedure Connect(const Params: string);
    function ExecuteQuery(const SQL: string): TDataSet;
  end;

// Facile à remplacer par SQLdb ou mORMot plus tard
```

**Avantages :**
- Migration facilitée
- Tests avec mocks possibles
- Plusieurs implémentations cohabitent
- Réduction du couplage

### Approche 3 : Feature flags

Pour tester en production sans tout casser :

```pascal
const
  USE_NEW_HTTP_LIBRARY = {$IFDEF DEBUG}True{$ELSE}False{$ENDIF};

procedure SendRequest(const URL: string);
begin
  if USE_NEW_HTTP_LIBRARY then
  begin
    // Nouvelle implémentation (Brook)
    SendRequestBrook(URL);
  end
  else
  begin
    // Ancienne implémentation (Indy)
    SendRequestIndy(URL);
  end;
end;
```

**Avantages :**
- Test progressif en production
- Rollback immédiat si problème
- Comparaison de performance A/B
- Adoption sans risque

---

## Contribuer efficacement

### Contributions faciles pour débuter

Si vous voulez contribuer mais ne savez pas par où commencer :

**1. Documentation (le plus facile) :**
- Corriger des fautes de frappe
- Traduire README en français
- Ajouter des exemples commentés
- Clarifier des sections confuses

**2. Tests (intermédiaire) :**
- Ajouter des cas de test manquants
- Tester sur votre plateforme (Windows/Linux)
- Reproduire et documenter des bugs
- Valider des correctifs proposés

**3. Bugs mineurs (intermédiaire) :**
- Corrections de typos dans le code
- Warning du compilateur à corriger
- Memory leaks évidents
- Bugs cosmétiques (UI)

**4. Fonctionnalités (avancé) :**
- Nouvelles fonctionnalités discutées dans issues
- Optimisations de performance
- Support de nouvelles plateformes
- Refactoring majeur

### Template de première contribution

```markdown
**Description**
This PR adds French translation to README.md

**Motivation**
As a French developer, I found the documentation hard to follow.
This translation will help French-speaking users.

**Checklist**
- [x] Translation is accurate
- [x] Links are updated
- [x] Formatting is preserved
- [x] No broken links

**Questions**
- Should I also translate the wiki pages?
- Do you want separate README.fr.md or inline?
```

### Progression dans la contribution

**Niveau 1 (mois 1-3) :** Documentation, traductions, tests  
**Niveau 2 (mois 3-6) :** Bugs mineurs, exemples, tutoriels  
**Niveau 3 (mois 6-12) :** Fonctionnalités, optimisations  
**Niveau 4 (an 1+) :** Architecture, maintenance, mentoring  

---

## Ressources pour aller plus loin

### Veille technologique

**Sites à suivre :**
- **Lazarus Forum - Third Party Announcements** : Nouveaux packages
- **Awesome Pascal GitHub** : Liste curatée mise à jour
- **r/freepascal Reddit** : Discussions communauté
- **FreePascal Wiki - Projects** : Catalogue officiel

**Newsletters et blogs :**
- Blog de Synopse (mORMot) : Articles techniques avancés
- Castle Game Engine Blog : Nouveautés moteur de jeu
- Lazarus Planet : Actualités écosystème

### Évaluer la santé d'un projet

**Métriques GitHub/GitLab à surveiller :**

```
✅ Commits last 3 months: > 10
✅ Issues response time: < 7 days
✅ Pull requests merged: > 5/month
✅ Stars: > 100 (pour popularité)
✅ Forks: > 20 (pour intérêt)
✅ Contributors: > 3 (pas de mono-maintainer)
✅ Releases: > 2/year (activité)
```

**Outils d'analyse :**
- **GitHub Insights** : Graphiques d'activité
- **Libraries.io** : Suivi de dépendances
- **OpenHub (anciennement Ohloh)** : Métriques projets open source

### Créer votre propre liste

Maintenez un fichier de référence personnel :

```markdown
# Mes projets FreePascal favoris

## Web
- [ ] Brook Framework (testé, OK)
- [ ] mORMot (à tester)
- [x] Synapse (en prod, excellent)

## Graphics
- [x] BGRABitmap (en prod, performant)
- [ ] TAChart (à tester pour dashboard)

## Notes
- Brook : Parfait pour APIs simples < 1000 req/s
- BGRABitmap : Attention memory avec grandes images
- Synapse : Préférer à Indy pour CLI tools
```

---

## Conclusion de la section 26.6

L'écosystème open source FreePascal/Lazarus est mature et riche. Les projets présentés couvrent la majorité des besoins de développement moderne :

**Points clés à retenir :**

1. **Ne réinventez pas la roue** : Il existe probablement déjà une bibliothèque pour votre besoin
2. **Commencez simple** : Brook avant mORMot, BGRABitmap avant OpenGL custom
3. **Évaluez avant d'adopter** : Licence, maintenance, communauté
4. **Contribuez quand possible** : Améliore l'écosystème et vos compétences
5. **Abstraisez vos dépendances** : Facilite les migrations futures

**Prochaines étapes :**

- Explorez les projets via Online Package Manager (OPM)
- Testez les exemples fournis avec les bibliothèques
- Participez aux forums pour poser vos questions
- Commencez par de petites contributions (documentation)
- Partagez vos propres packages si vous créez des outils réutilisables

**Ressources essentielles :**

- 🔗 Lazarus OPM : Via IDE Lazarus
- 🔗 Awesome Pascal : https://github.com/Fr0sT-Brutal/awesome-pascal
- 🔗 FreePascal Wiki : https://wiki.freepascal.org/Projects
- 🔗 Lazarus Forum : https://forum.lazarus.freepascal.org/

La maîtrise de ces projets open source vous permettra de développer des applications professionnelles multi-plateformes rapidement et efficacement. Dans la prochaine section (26.7), nous verrons comment migrer des projets Delphi existants vers FreePascal/Lazarus en tirant parti de ces bibliothèques.

⏭️ [Migration depuis Delphi](/26-communaute-ecosysteme/07-migration-depuis-delphi.md)
