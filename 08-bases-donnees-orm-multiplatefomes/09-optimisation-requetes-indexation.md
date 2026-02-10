🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.9 Optimisation des requêtes et indexation

## Introduction

L'optimisation des requêtes et l'indexation sont des compétences essentielles pour tout développeur travaillant avec des bases de données. Une application peut fonctionner parfaitement avec 100 enregistrements, mais devenir inutilisable avec 100 000 enregistrements si les requêtes ne sont pas optimisées.

Dans ce chapitre, nous allons explorer comment rendre vos applications FreePascal/Lazarus rapides et efficaces, que vous travailliez sur Windows ou Ubuntu.

## Qu'est-ce que l'optimisation de requêtes ?

### Analogie simple

Imaginez une bibliothèque avec 10 000 livres :

**Sans index :** Pour trouver un livre sur "les oiseaux", vous devez parcourir TOUS les livres un par un jusqu'à trouver ceux qui parlent d'oiseaux. Cela peut prendre des heures.

**Avec index :** La bibliothèque possède un catalogue organisé par sujet. Vous cherchez "oiseaux" dans le catalogue, qui vous indique exactement où se trouvent ces livres. Cela prend quelques secondes.

C'est exactement le rôle des **index** dans une base de données !

## Comprendre le problème de performance

### Exemple concret

Prenons une table `clients` avec 500 000 enregistrements :

```sql
CREATE TABLE clients (
    id INTEGER PRIMARY KEY,
    nom VARCHAR(100),
    prenom VARCHAR(100),
    email VARCHAR(255),
    ville VARCHAR(100),
    date_inscription DATE
);
```

**Requête lente (sans index) :**

```sql
SELECT * FROM clients WHERE email = 'jean.dupont@example.com';
```

Cette requête peut prendre plusieurs secondes car la base de données doit examiner les 500 000 lignes une par une (scan complet de table).

**Requête rapide (avec index) :**

```sql
CREATE INDEX idx_clients_email ON clients(email);

SELECT * FROM clients WHERE email = 'jean.dupont@example.com';
```

Avec l'index, la requête trouve le résultat en quelques millisecondes !

## Les index : concepts fondamentaux

### Qu'est-ce qu'un index ?

Un **index** est une structure de données supplémentaire qui permet d'accélérer les recherches dans une table. C'est comme une table des matières dans un livre.

### Types d'index courants

#### 1. Index simple (single column)

Index sur une seule colonne :

```sql
CREATE INDEX idx_clients_nom ON clients(nom);
```

Utilisé pour les recherches sur cette colonne :

```sql
SELECT * FROM clients WHERE nom = 'Dupont';
```

#### 2. Index composite (multi-column)

Index sur plusieurs colonnes dans un ordre spécifique :

```sql
CREATE INDEX idx_clients_nom_prenom ON clients(nom, prenom);
```

**Important :** L'ordre des colonnes dans l'index est crucial !

```sql
-- ✅ Utilise l'index (commence par nom)
SELECT * FROM clients WHERE nom = 'Dupont' AND prenom = 'Jean';

-- ✅ Utilise l'index (commence par nom)
SELECT * FROM clients WHERE nom = 'Dupont';

-- ❌ N'utilise PAS l'index (commence par prenom)
SELECT * FROM clients WHERE prenom = 'Jean';
```

**Règle :** Un index composite fonctionne de gauche à droite. C'est comme un annuaire téléphonique : trié par nom puis prénom, mais pas par prénom seul.

#### 3. Index unique

Garantit l'unicité des valeurs et accélère les recherches :

```sql
CREATE UNIQUE INDEX idx_clients_email_unique ON clients(email);
```

Empêche l'insertion de doublons :

```sql
-- Cette insertion échouera si l'email existe déjà
INSERT INTO clients (email) VALUES ('jean@example.com');
```

#### 4. Index de texte intégral (Full-Text)

Pour les recherches dans du texte (PostgreSQL, MySQL) :

```sql
-- PostgreSQL
CREATE INDEX idx_clients_description_fulltext  
ON clients USING gin(to_tsvector('french', description));

-- MySQL
CREATE FULLTEXT INDEX idx_clients_description ON clients(description);
```

Utilisé pour des recherches de mots :

```sql
-- PostgreSQL
SELECT * FROM clients  
WHERE to_tsvector('french', description) @@ to_tsquery('oiseaux');

-- MySQL
SELECT * FROM clients  
WHERE MATCH(description) AGAINST('oiseaux' IN NATURAL LANGUAGE MODE);
```

## Création et gestion des index en FreePascal

### Créer un index

```pascal
procedure CreerIndexEmail(Connection: TSQLConnection);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;
    Query.SQL.Text := 'CREATE INDEX idx_clients_email ON clients(email)';

    try
      Query.ExecSQL;
      Connection.Transaction.Commit;
      WriteLn('Index créé avec succès');
    except
      on E: Exception do
      begin
        Connection.Transaction.Rollback;
        WriteLn('Erreur création index: ', E.Message);
      end;
    end;
  finally
    Query.Free;
  end;
end;
```

### Supprimer un index

```pascal
procedure SupprimerIndex(Connection: TSQLConnection; const IndexName: String);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;

    // SQL compatible Windows/Ubuntu
    {$IFDEF POSTGRES}
    Query.SQL.Text := 'DROP INDEX IF EXISTS ' + IndexName;
    {$ELSE}
    Query.SQL.Text := 'DROP INDEX ' + IndexName;
    {$ENDIF}

    try
      Query.ExecSQL;
      Connection.Transaction.Commit;
      WriteLn('Index supprimé avec succès');
    except
      on E: Exception do
      begin
        Connection.Transaction.Rollback;
        WriteLn('Erreur suppression index: ', E.Message);
      end;
    end;
  finally
    Query.Free;
  end;
end;
```

### Lister les index existants

```pascal
procedure ListerIndex(Connection: TSQLConnection; const TableName: String);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;

    // PostgreSQL
    {$IFDEF POSTGRES}
    Query.SQL.Text :=
      'SELECT indexname, indexdef ' +
      'FROM pg_indexes ' +
      'WHERE tablename = :tablename';
    Query.Params.ParamByName('tablename').AsString := TableName;
    {$ENDIF}

    // MySQL
    {$IFDEF MYSQL}
    Query.SQL.Text := 'SHOW INDEX FROM ' + TableName;
    {$ENDIF}

    Query.Open;
    WriteLn('Index pour la table ', TableName, ':');
    while not Query.EOF do
    begin
      WriteLn('  - ', Query.FieldByName('indexname').AsString);
      Query.Next;
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;
```

## Analyser les performances des requêtes

### Plan d'exécution (EXPLAIN)

Le plan d'exécution vous montre COMMENT la base de données va exécuter votre requête.

```pascal
procedure AnalyserRequete(Connection: TSQLConnection; const SQL: String);  
var
  Query: TSQLQuery;
  ExplainSQL: String;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;

    // Ajouter EXPLAIN devant la requête
    {$IFDEF POSTGRES}
    ExplainSQL := 'EXPLAIN ANALYZE ' + SQL;
    {$ELSE}
    ExplainSQL := 'EXPLAIN ' + SQL;
    {$ENDIF}

    Query.SQL.Text := ExplainSQL;
    Query.Open;

    WriteLn('Plan d''exécution:');
    WriteLn('-------------------');
    while not Query.EOF do
    begin
      WriteLn(Query.Fields[0].AsString);
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;
```

### Exemple d'utilisation

```pascal
// Analyser une requête potentiellement lente
AnalyserRequete(Connection,
  'SELECT * FROM clients WHERE ville = ''Paris'' AND age > 25'
);
```

**Résultat typique (PostgreSQL) :**

```
Seq Scan on clients  (cost=0.00..10234.00 rows=5000 width=100)
  Filter: ((ville = 'Paris') AND (age > 25))
```

**Seq Scan** = Scan séquentiel (mauvais) : la base parcourt toute la table.

**Après création d'un index :**

```
Index Scan using idx_clients_ville_age on clients  (cost=0.42..823.21 rows=5000 width=100)
  Index Cond: ((ville = 'Paris') AND (age > 25))
```

**Index Scan** = Utilise l'index (bon) : la base accède directement aux données.

## Optimisation des requêtes : techniques essentielles

### 1. Utiliser WHERE au lieu de HAVING quand possible

❌ **Mauvais (lent) :**

```sql
SELECT ville, COUNT(*) as nb  
FROM clients  
GROUP BY ville  
HAVING ville = 'Paris';
```

✅ **Bon (rapide) :**

```sql
SELECT ville, COUNT(*) as nb  
FROM clients  
WHERE ville = 'Paris'  
GROUP BY ville;
```

**Raison :** `WHERE` filtre AVANT le regroupement, `HAVING` filtre APRÈS.

### 2. Éviter SELECT *

❌ **Mauvais :**

```sql
SELECT * FROM clients WHERE id = 123;
```

✅ **Bon :**

```sql
SELECT id, nom, prenom, email FROM clients WHERE id = 123;
```

**Raison :** Récupérer uniquement les colonnes nécessaires réduit la quantité de données transférées.

### 3. Utiliser LIMIT pour les grandes tables

```sql
-- Récupérer seulement les 100 premiers résultats
SELECT nom, prenom FROM clients  
WHERE ville = 'Paris'  
ORDER BY nom  
LIMIT 100;
```

### 4. Éviter les fonctions dans WHERE

❌ **Mauvais (l'index ne sera pas utilisé) :**

```sql
SELECT * FROM clients WHERE UPPER(nom) = 'DUPONT';
```

✅ **Bon :**

```sql
-- Stocker le nom en majuscules ou créer un index fonctionnel
CREATE INDEX idx_clients_nom_upper ON clients(UPPER(nom));

-- Ou mieux : normaliser en amont
SELECT * FROM clients WHERE nom = 'DUPONT';
```

### 5. Utiliser EXISTS au lieu de IN pour les sous-requêtes

❌ **Mauvais (peut être lent) :**

```sql
SELECT * FROM clients  
WHERE id IN (SELECT client_id FROM commandes WHERE montant > 1000);
```

✅ **Bon (généralement plus rapide) :**

```sql
SELECT * FROM clients c  
WHERE EXISTS (
    SELECT 1 FROM commandes o
    WHERE o.client_id = c.id AND o.montant > 1000
);
```

### 6. Utiliser les jointures appropriées

```pascal
// Requête optimisée avec jointure
procedure RechercherClientsAvecCommandes(Connection: TSQLConnection);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;

    // Jointure efficace avec index sur les clés étrangères
    Query.SQL.Text :=
      'SELECT c.nom, c.prenom, COUNT(o.id) as nb_commandes, SUM(o.montant) as total ' +
      'FROM clients c ' +
      'INNER JOIN commandes o ON c.id = o.client_id ' +
      'WHERE o.date_commande >= :date_debut ' +
      'GROUP BY c.id, c.nom, c.prenom ' +
      'HAVING SUM(o.montant) > :montant_min ' +
      'ORDER BY total DESC ' +
      'LIMIT 50';

    Query.Params.ParamByName('date_debut').AsDate := EncodeDate(2024, 1, 1);
    Query.Params.ParamByName('montant_min').AsFloat := 1000.00;

    Query.Open;
    while not Query.EOF do
    begin
      WriteLn(Format('%s %s: %d commandes, total: %.2f€', [
        Query.FieldByName('nom').AsString,
        Query.FieldByName('prenom').AsString,
        Query.FieldByName('nb_commandes').AsInteger,
        Query.FieldByName('total').AsFloat
      ]));
      Query.Next;
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;
```

## Stratégies d'indexation avancées

### Quand créer un index ?

✅ **Créez un index si :**

- La colonne est fréquemment utilisée dans WHERE
- La colonne est utilisée dans JOIN
- La colonne est utilisée dans ORDER BY
- La table contient beaucoup de lignes (>10 000)
- Les lectures sont plus fréquentes que les écritures

❌ **N'indexez PAS si :**

- La table est petite (<1 000 lignes)
- La colonne change fréquemment
- La colonne a peu de valeurs distinctes (ex: colonne booléenne)
- Les insertions/mises à jour sont très fréquentes

### Index couvrant (Covering Index)

Un index qui contient toutes les colonnes nécessaires à une requête :

```sql
-- Index couvrant pour cette requête spécifique
CREATE INDEX idx_clients_ville_nom_email ON clients(ville, nom, email);

-- Cette requête utilise UNIQUEMENT l'index (très rapide)
SELECT nom, email FROM clients WHERE ville = 'Paris';
```

### Index partiel (Partial Index)

Index seulement sur un sous-ensemble de données (PostgreSQL) :

```sql
-- Indexer seulement les clients actifs
CREATE INDEX idx_clients_actifs ON clients(email)  
WHERE statut = 'actif';

-- Cette requête utilise l'index partiel
SELECT * FROM clients WHERE email = 'test@example.com' AND statut = 'actif';
```

**Avantage :** Index plus petit et plus rapide.

### Index sur expressions

Index basé sur une fonction (PostgreSQL) :

```sql
-- Index pour les recherches insensibles à la casse
CREATE INDEX idx_clients_email_lower ON clients(LOWER(email));

-- Requête qui utilise l'index
SELECT * FROM clients WHERE LOWER(email) = 'jean@example.com';
```

## Pagination efficace

### Problème de OFFSET

❌ **Mauvais pour grande pagination :**

```sql
-- Page 1000 : la base doit lire et ignorer 999 000 lignes !
SELECT * FROM clients ORDER BY id LIMIT 1000 OFFSET 999000;
```

✅ **Bon (pagination par curseur) :**

```sql
-- Première page
SELECT * FROM clients ORDER BY id LIMIT 1000;

-- Pages suivantes : utiliser le dernier ID vu
SELECT * FROM clients WHERE id > :dernier_id_vu ORDER BY id LIMIT 1000;
```

**Implémentation en FreePascal :**

```pascal
type
  TClientPagination = class
  private
    FConnection: TSQLConnection;
    FQuery: TSQLQuery;
    FPageSize: Integer;
    FLastID: Integer;
  public
    constructor Create(AConnection: TSQLConnection; APageSize: Integer = 100);
    destructor Destroy; override;

    function GetFirstPage: Boolean;
    function GetNextPage: Boolean;
    function HasMorePages: Boolean;

    property Query: TSQLQuery read FQuery;
  end;

constructor TClientPagination.Create(AConnection: TSQLConnection; APageSize: Integer);  
begin
  FConnection := AConnection;
  FPageSize := APageSize;
  FLastID := 0;
  FQuery := TSQLQuery.Create(nil);
  FQuery.Database := FConnection;
end;

destructor TClientPagination.Destroy;  
begin
  FQuery.Free;
  inherited;
end;

function TClientPagination.GetFirstPage: Boolean;  
begin
  FLastID := 0;
  FQuery.Close;
  FQuery.SQL.Text :=
    'SELECT id, nom, prenom, email FROM clients ' +
    'ORDER BY id ' +
    'LIMIT :page_size';
  FQuery.Params.ParamByName('page_size').AsInteger := FPageSize;

  FQuery.Open;
  Result := not FQuery.EOF;

  if Result then
  begin
    FQuery.Last;
    FLastID := FQuery.FieldByName('id').AsInteger;
    FQuery.First;
  end;
end;

function TClientPagination.GetNextPage: Boolean;  
begin
  if FLastID = 0 then
  begin
    Result := GetFirstPage;
    Exit;
  end;

  FQuery.Close;
  FQuery.SQL.Text :=
    'SELECT id, nom, prenom, email FROM clients ' +
    'WHERE id > :last_id ' +
    'ORDER BY id ' +
    'LIMIT :page_size';
  FQuery.Params.ParamByName('last_id').AsInteger := FLastID;
  FQuery.Params.ParamByName('page_size').AsInteger := FPageSize;

  FQuery.Open;
  Result := not FQuery.EOF;

  if Result then
  begin
    FQuery.Last;
    FLastID := FQuery.FieldByName('id').AsInteger;
    FQuery.First;
  end;
end;

function TClientPagination.HasMorePages: Boolean;  
begin
  Result := FQuery.RecordCount = FPageSize;
end;

// Utilisation
procedure ChargerTousLesClients;  
var
  Pagination: TClientPagination;
begin
  Pagination := TClientPagination.Create(Connection, 1000);
  try
    if Pagination.GetFirstPage then
    begin
      repeat
        // Traiter la page courante
        while not Pagination.Query.EOF do
        begin
          WriteLn(Pagination.Query.FieldByName('nom').AsString);
          Pagination.Query.Next;
        end;

        // Passer à la page suivante
      until not Pagination.GetNextPage;
    end;
  finally
    Pagination.Free;
  end;
end;
```

## Optimisation des insertions massives

### Transactions groupées

❌ **Très lent (1000 transactions) :**

```pascal
for i := 1 to 1000 do  
begin
  Connection.Transaction.StartTransaction;
  Query.SQL.Text := 'INSERT INTO clients (nom) VALUES (:nom)';
  Query.Params.ParamByName('nom').AsString := 'Client' + IntToStr(i);
  Query.ExecSQL;
  Connection.Transaction.Commit;
end;
```

✅ **Rapide (1 transaction) :**

```pascal
Connection.Transaction.StartTransaction;  
try
  Query.SQL.Text := 'INSERT INTO clients (nom) VALUES (:nom)';
  for i := 1 to 1000 do
  begin
    Query.Params.ParamByName('nom').AsString := 'Client' + IntToStr(i);
    Query.ExecSQL;
  end;
  Connection.Transaction.Commit;
except
  Connection.Transaction.Rollback;
  raise;
end;
```

### Insertions par lot (Batch Insert)

✅ **Encore plus rapide (PostgreSQL) :**

```pascal
procedure InsertionMassive(Connection: TSQLConnection; NombreClients: Integer);  
var
  Query: TSQLQuery;
  SQL: TStringList;
  i, BatchSize: Integer;
begin
  BatchSize := 100; // Insérer par lots de 100
  SQL := TStringList.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;
    Connection.Transaction.StartTransaction;

    try
      for i := 1 to NombreClients do
      begin
        SQL.Add(Format('(''Client%d'', ''client%d@example.com'')', [i, i]));

        // Insérer tous les 100 ou à la fin
        if (SQL.Count = BatchSize) or (i = NombreClients) then
        begin
          Query.SQL.Text :=
            'INSERT INTO clients (nom, email) VALUES ' +
            SQL.CommaText;
          Query.ExecSQL;
          SQL.Clear;
        end;
      end;

      Connection.Transaction.Commit;
      WriteLn(Format('%d clients insérés avec succès', [NombreClients]));
    except
      on E: Exception do
      begin
        Connection.Transaction.Rollback;
        WriteLn('Erreur: ', E.Message);
      end;
    end;
  finally
    SQL.Free;
    Query.Free;
  end;
end;
```

### Désactiver temporairement les index

Pour des insertions massives, vous pouvez désactiver puis recréer les index :

```pascal
procedure ImportationMassive(Connection: TSQLConnection; const FichierCSV: String);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;

    Connection.Transaction.StartTransaction;
    try
      // 1. Supprimer les index (sauf PRIMARY KEY)
      Query.SQL.Text := 'DROP INDEX IF EXISTS idx_clients_email';
      Query.ExecSQL;
      Query.SQL.Text := 'DROP INDEX IF EXISTS idx_clients_ville';
      Query.ExecSQL;

      // 2. Importer les données
      ImporterDepuisCSV(FichierCSV);

      // 3. Recréer les index
      Query.SQL.Text := 'CREATE INDEX idx_clients_email ON clients(email)';
      Query.ExecSQL;
      Query.SQL.Text := 'CREATE INDEX idx_clients_ville ON clients(ville)';
      Query.ExecSQL;

      Connection.Transaction.Commit;
      WriteLn('Importation terminée avec succès');
    except
      on E: Exception do
      begin
        Connection.Transaction.Rollback;
        WriteLn('Erreur: ', E.Message);
      end;
    end;
  finally
    Query.Free;
  end;
end;
```

## Mise en cache des requêtes

### Cache applicatif simple

```pascal
unit QueryCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, fgl;

type
  TCachedResult = class
  private
    FData: TStringList;
    FTimestamp: TDateTime;
    FTTL: Integer; // Durée de vie en secondes
  public
    constructor Create(ATTL: Integer = 300); // 5 minutes par défaut
    destructor Destroy; override;

    function IsExpired: Boolean;
    property Data: TStringList read FData;
  end;

  TQueryCacheMap = specialize TFPGMap<String, TCachedResult>;

  TQueryCache = class
  private
    FCache: TQueryCacheMap;
    FDefaultTTL: Integer;
  public
    constructor Create(ADefaultTTL: Integer = 300);
    destructor Destroy; override;

    function Get(const Key: String): TCachedResult;
    procedure Put(const Key: String; Data: TStringList; TTL: Integer = -1);
    procedure Clear;
    procedure RemoveExpired;
  end;

implementation

{ TCachedResult }

constructor TCachedResult.Create(ATTL: Integer);  
begin
  FData := TStringList.Create;
  FTimestamp := Now;
  FTTL := ATTL;
end;

destructor TCachedResult.Destroy;  
begin
  FData.Free;
  inherited;
end;

function TCachedResult.IsExpired: Boolean;  
begin
  Result := (Now - FTimestamp) > (FTTL / 86400); // Convertir secondes en jours
end;

{ TQueryCache }

constructor TQueryCache.Create(ADefaultTTL: Integer);  
begin
  FCache := TQueryCacheMap.Create;
  FDefaultTTL := ADefaultTTL;
end;

destructor TQueryCache.Destroy;  
begin
  Clear;
  FCache.Free;
  inherited;
end;

function TQueryCache.Get(const Key: String): TCachedResult;  
var
  Index: Integer;
begin
  Index := FCache.IndexOf(Key);
  if Index >= 0 then
  begin
    Result := FCache.Data[Index];
    if Result.IsExpired then
    begin
      FCache.Delete(Index);
      Result.Free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

procedure TQueryCache.Put(const Key: String; Data: TStringList; TTL: Integer);  
var
  CachedResult: TCachedResult;
  Index: Integer;
begin
  if TTL = -1 then
    TTL := FDefaultTTL;

  Index := FCache.IndexOf(Key);
  if Index >= 0 then
  begin
    FCache.Data[Index].Free;
    FCache.Delete(Index);
  end;

  CachedResult := TCachedResult.Create(TTL);
  CachedResult.Data.Assign(Data);
  FCache.Add(Key, CachedResult);
end;

procedure TQueryCache.Clear;  
var
  i: Integer;
begin
  for i := 0 to FCache.Count - 1 do
    FCache.Data[i].Free;
  FCache.Clear;
end;

procedure TQueryCache.RemoveExpired;  
var
  i: Integer;
begin
  for i := FCache.Count - 1 downto 0 do
  begin
    if FCache.Data[i].IsExpired then
    begin
      FCache.Data[i].Free;
      FCache.Delete(i);
    end;
  end;
end;

end.
```

### Utilisation du cache

```pascal
var
  Cache: TQueryCache;

procedure RechercherVilleAvecCache(const Ville: String);  
var
  Query: TSQLQuery;
  CacheKey: String;
  CachedResult: TCachedResult;
  Results: TStringList;
begin
  CacheKey := 'ville_' + Ville;

  // Vérifier le cache
  CachedResult := Cache.Get(CacheKey);
  if CachedResult <> nil then
  begin
    WriteLn('Résultats depuis le cache:');
    WriteLn(CachedResult.Data.Text);
    Exit;
  end;

  // Pas en cache, exécuter la requête
  Query := TSQLQuery.Create(nil);
  Results := TStringList.Create;
  try
    Query.Database := Connection;
    Query.SQL.Text := 'SELECT nom, prenom FROM clients WHERE ville = :ville';
    Query.Params.ParamByName('ville').AsString := Ville;
    Query.Open;

    while not Query.EOF do
    begin
      Results.Add(Query.FieldByName('nom').AsString + ' ' +
                  Query.FieldByName('prenom').AsString);
      Query.Next;
    end;

    Query.Close;

    // Mettre en cache pour 5 minutes
    Cache.Put(CacheKey, Results, 300);

    WriteLn('Résultats depuis la base:');
    WriteLn(Results.Text);
  finally
    Results.Free;
    Query.Free;
  end;
end;

// Initialisation
Cache := TQueryCache.Create(300);
```

## Outils de monitoring des performances

### Classe de profilage des requêtes

```pascal
unit QueryProfiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TQueryProfile = record
    SQL: String;
    StartTime: TDateTime;
    EndTime: TDateTime;
    Duration: Integer; // en millisecondes
    RowCount: Integer;
  end;

  TQueryProfiler = class
  private
    FProfiles: array of TQueryProfile;
    FEnabled: Boolean;
  public
    constructor Create;

    procedure StartProfile(const SQL: String);
    procedure EndProfile(RowCount: Integer);
    procedure PrintStats;
    procedure SaveToFile(const FileName: String);
    procedure Clear;

    property Enabled: Boolean read FEnabled write FEnabled;
  end;

var
  GlobalProfiler: TQueryProfiler;

implementation

constructor TQueryProfiler.Create;  
begin
  FEnabled := True;
  SetLength(FProfiles, 0);
end;

procedure TQueryProfiler.StartProfile(const SQL: String);  
var
  Profile: TQueryProfile;
begin
  if not FEnabled then Exit;

  Profile.SQL := SQL;
  Profile.StartTime := Now;
  Profile.EndTime := 0;
  Profile.Duration := 0;
  Profile.RowCount := 0;

  SetLength(FProfiles, Length(FProfiles) + 1);
  FProfiles[High(FProfiles)] := Profile;
end;

procedure TQueryProfiler.EndProfile(RowCount: Integer);  
begin
  if not FEnabled then Exit;
  if Length(FProfiles) = 0 then Exit;

  with FProfiles[High(FProfiles)] do
  begin
    EndTime := Now;
    Duration := MilliSecondsBetween(EndTime, StartTime);
    RowCount := RowCount;
  end;
end;

procedure TQueryProfiler.PrintStats;  
var
  i: Integer;
  TotalDuration: Integer;
begin
  if Length(FProfiles) = 0 then
  begin
    WriteLn('Aucune requête profilée');
    Exit;
  end;

  WriteLn('=== Statistiques des requêtes ===');
  WriteLn;

  TotalDuration := 0;
  for i := 0 to High(FProfiles) do
  begin
    with FProfiles[i] do
    begin
      WriteLn(Format('Requête #%d:', [i + 1]));
      WriteLn('  SQL: ', Copy(SQL, 1, 80)); // Afficher les 80 premiers caractères
      WriteLn(Format('  Durée: %d ms', [Duration]));
      WriteLn(Format('  Lignes: %d', [RowCount]));
      WriteLn;

      TotalDuration := TotalDuration + Duration;
    end;
  end;

  WriteLn(Format('Total: %d requêtes en %d ms', [Length(FProfiles), TotalDuration]));
  WriteLn(Format('Moyenne: %.2f ms par requête', [TotalDuration / Length(FProfiles)]));
end;

procedure TQueryProfiler.SaveToFile(const FileName: String);  
var
  F: TextFile;
  i: Integer;
begin
  AssignFile(F, FileName);
  try
    Rewrite(F);

    WriteLn(F, 'Rapport de profilage des requêtes');
    WriteLn(F, 'Date: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    WriteLn(F, '');
    WriteLn(F, 'Requête;Durée (ms);Lignes');

    for i := 0 to High(FProfiles) do
    begin
      with FProfiles[i] do
      begin
        WriteLn(F, Format('"%s";%d;%d', [
          StringReplace(SQL, '"', '""', [rfReplaceAll]),
          Duration,
          RowCount
        ]));
      end;
    end;

    CloseFile(F);
    WriteLn('Rapport sauvegardé dans: ', FileName);
  except
    on E: Exception do
      WriteLn('Erreur sauvegarde rapport: ', E.Message);
  end;
end;

procedure TQueryProfiler.Clear;  
begin
  SetLength(FProfiles, 0);
end;

end.
```

### Utilisation du QueryProfiler

```pascal
uses
  QueryProfiler;

procedure ExecuterAvecProfiling;  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;

    // Profiler la première requête
    GlobalProfiler.StartProfile('SELECT * FROM clients WHERE ville = ''Paris''');
    Query.SQL.Text := 'SELECT * FROM clients WHERE ville = :ville';
    Query.Params.ParamByName('ville').AsString := 'Paris';
    Query.Open;
    GlobalProfiler.EndProfile(Query.RecordCount);
    Query.Close;

    // Profiler la deuxième requête
    GlobalProfiler.StartProfile('SELECT * FROM commandes JOIN clients...');
    Query.SQL.Text :=
      'SELECT c.nom, COUNT(o.id) as nb_commandes ' +
      'FROM clients c ' +
      'INNER JOIN commandes o ON c.id = o.client_id ' +
      'GROUP BY c.id, c.nom';
    Query.Open;
    GlobalProfiler.EndProfile(Query.RecordCount);
    Query.Close;

    // Afficher les statistiques
    GlobalProfiler.PrintStats;

    // Sauvegarder dans un fichier
    GlobalProfiler.SaveToFile('profiling_report.csv');
  finally
    Query.Free;
  end;
end;

initialization
  GlobalProfiler := TQueryProfiler.Create;

finalization
  GlobalProfiler.Free;
```

## Optimisation spécifique Windows vs Ubuntu

### Différences de performance selon l'OS

Les performances peuvent varier entre Windows et Ubuntu pour plusieurs raisons :

#### 1. Système de fichiers

**Windows (NTFS) :**
- Performances variables selon la fragmentation
- Antivirus peut ralentir les accès disque

**Ubuntu (ext4, btrfs) :**
- Généralement plus rapide pour les E/S
- Meilleure gestion des fichiers de grande taille

#### 2. Configuration mémoire partagée (PostgreSQL)

**Windows :**

```ini
# postgresql.conf sur Windows
shared_buffers = 256MB          # Limité par la mémoire système  
effective_cache_size = 1GB  
work_mem = 4MB  
maintenance_work_mem = 64MB
```

**Ubuntu :**

```ini
# postgresql.conf sur Ubuntu
shared_buffers = 512MB          # Peut être plus élevé  
effective_cache_size = 4GB      # Utilise mieux la mémoire système  
work_mem = 8MB  
maintenance_work_mem = 128MB
```

#### 3. Détection automatique de l'OS

```pascal
function GetOptimalConnectionParams: TConnectionParams;  
begin
  {$IFDEF WINDOWS}
  Result.MaxConnections := 50;
  Result.ConnectionTimeout := 30;
  Result.QueryTimeout := 60;
  {$ELSE} // Linux/Ubuntu
  Result.MaxConnections := 100;
  Result.ConnectionTimeout := 15;
  Result.QueryTimeout := 120;
  {$ENDIF}
end;
```

## Techniques avancées d'optimisation

### 1. Partitionnement de tables

Pour les très grandes tables (millions de lignes), le partitionnement peut améliorer considérablement les performances.

**Exemple : Partitionner par date**

```sql
-- Table parent (PostgreSQL)
CREATE TABLE commandes (
    id SERIAL,
    client_id INTEGER,
    montant DECIMAL(10,2),
    date_commande DATE
) PARTITION BY RANGE (date_commande);

-- Partitions par mois
CREATE TABLE commandes_2024_01 PARTITION OF commandes
    FOR VALUES FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE commandes_2024_02 PARTITION OF commandes
    FOR VALUES FROM ('2024-02-01') TO ('2024-03-01');

CREATE TABLE commandes_2024_03 PARTITION OF commandes
    FOR VALUES FROM ('2024-03-01') TO ('2024-04-01');

-- Index sur chaque partition
CREATE INDEX idx_commandes_2024_01_date ON commandes_2024_01(date_commande);  
CREATE INDEX idx_commandes_2024_02_date ON commandes_2024_02(date_commande);  
CREATE INDEX idx_commandes_2024_03_date ON commandes_2024_03(date_commande);
```

**Avantages :**
- Les requêtes sur une période spécifique n'accèdent qu'aux partitions concernées
- Maintenance plus facile (supprimer une partition = supprimer de vieilles données)
- Index plus petits et plus rapides

**Gestion en FreePascal :**

```pascal
procedure CreerPartitionMensuelle(Connection: TSQLConnection; Annee, Mois: Integer);  
var
  Query: TSQLQuery;
  DateDebut, DateFin: TDateTime;
  NomPartition: String;
begin
  DateDebut := EncodeDate(Annee, Mois, 1);
  DateFin := IncMonth(DateDebut, 1);

  NomPartition := Format('commandes_%d_%0.2d', [Annee, Mois]);

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;

    Connection.Transaction.StartTransaction;
    try
      // Créer la partition
      Query.SQL.Text := Format(
        'CREATE TABLE %s PARTITION OF commandes ' +
        'FOR VALUES FROM (''%s'') TO (''%s'')',
        [NomPartition,
         FormatDateTime('yyyy-mm-dd', DateDebut),
         FormatDateTime('yyyy-mm-dd', DateFin)]
      );
      Query.ExecSQL;

      // Créer l'index sur la partition
      Query.SQL.Text := Format(
        'CREATE INDEX idx_%s_date ON %s(date_commande)',
        [NomPartition, NomPartition]
      );
      Query.ExecSQL;

      Connection.Transaction.Commit;
      WriteLn('Partition créée: ', NomPartition);
    except
      on E: Exception do
      begin
        Connection.Transaction.Rollback;
        WriteLn('Erreur création partition: ', E.Message);
      end;
    end;
  finally
    Query.Free;
  end;
end;

// Créer automatiquement les partitions pour l'année
procedure CreerPartitionsAnnuelles(Connection: TSQLConnection; Annee: Integer);  
var
  Mois: Integer;
begin
  for Mois := 1 to 12 do
    CreerPartitionMensuelle(Connection, Annee, Mois);
end;
```

### 2. Vues matérialisées

Les vues matérialisées stockent physiquement le résultat d'une requête complexe.

**Création (PostgreSQL) :**

```sql
-- Vue matérialisée pour statistiques clients
CREATE MATERIALIZED VIEW stats_clients AS  
SELECT
    c.id,
    c.nom,
    c.prenom,
    COUNT(o.id) as nb_commandes,
    SUM(o.montant) as total_commandes,
    AVG(o.montant) as montant_moyen,
    MAX(o.date_commande) as derniere_commande
FROM clients c  
LEFT JOIN commandes o ON c.id = o.client_id  
GROUP BY c.id, c.nom, c.prenom;

-- Index sur la vue matérialisée
CREATE INDEX idx_stats_clients_id ON stats_clients(id);  
CREATE INDEX idx_stats_clients_total ON stats_clients(total_commandes);
```

**Rafraîchissement :**

```sql
-- Rafraîchir la vue (recalculer)
REFRESH MATERIALIZED VIEW stats_clients;

-- Rafraîchissement concurrent (sans bloquer les lectures)
REFRESH MATERIALIZED VIEW CONCURRENTLY stats_clients;
```

**Gestion en FreePascal :**

```pascal
procedure RafraichirVueMaterialisee(Connection: TSQLConnection;
  const NomVue: String; Concurrent: Boolean = False);
var
  Query: TSQLQuery;
  SQL: String;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;

    if Concurrent then
      SQL := 'REFRESH MATERIALIZED VIEW CONCURRENTLY ' + NomVue
    else
      SQL := 'REFRESH MATERIALIZED VIEW ' + NomVue;

    Query.SQL.Text := SQL;

    WriteLn('Rafraîchissement de ', NomVue, '...');
    Query.ExecSQL;
    Connection.Transaction.Commit;
    WriteLn('Vue matérialisée rafraîchie avec succès');
  finally
    Query.Free;
  end;
end;

// Rafraîchissement automatique périodique
procedure ConfigurerRafraichissementAuto(Connection: TSQLConnection);  
var
  Timer: TTimer;
begin
  Timer := TTimer.Create(nil);
  Timer.Interval := 3600000; // 1 heure
  Timer.OnTimer := @RafraichirVuesMaterialisees;
  Timer.Enabled := True;
end;

procedure RafraichirVuesMaterialisees(Sender: TObject);  
begin
  RafraichirVueMaterialisee(Connection, 'stats_clients', True);
  RafraichirVueMaterialisee(Connection, 'stats_produits', True);
end;
```

### 3. Requêtes parallèles

Certaines bases de données (PostgreSQL 9.6+) peuvent exécuter des requêtes en parallèle.

**Configuration (PostgreSQL) :**

```sql
-- Activer le parallélisme
SET max_parallel_workers_per_gather = 4;  
SET parallel_setup_cost = 100;  
SET parallel_tuple_cost = 0.1;

-- Forcer le parallélisme pour une requête
SET parallel_setup_cost = 0;  
SET parallel_tuple_cost = 0;
```

**En FreePascal :**

```pascal
procedure ActiverParallelisme(Connection: TSQLConnection; NbWorkers: Integer = 4);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Connection;

    Query.SQL.Text := Format('SET max_parallel_workers_per_gather = %d', [NbWorkers]);
    Query.ExecSQL;

    Query.SQL.Text := 'SET parallel_setup_cost = 100';
    Query.ExecSQL;

    Connection.Transaction.Commit;
    WriteLn('Parallélisme activé avec ', NbWorkers, ' workers');
  finally
    Query.Free;
  end;
end;
```

### 4. Analyse et maintenance automatique

```pascal
unit DatabaseMaintenance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB;

type
  TMaintenanceTask = (mtVacuum, mtAnalyze, mtReindex, mtUpdateStats);
  TMaintenanceTasks = set of TMaintenanceTask;

  TDatabaseMaintenance = class
  private
    FConnection: TSQLConnection;
    procedure ExecuteVacuum(const TableName: String);
    procedure ExecuteAnalyze(const TableName: String);
    procedure ExecuteReindex(const TableName: String);
  public
    constructor Create(AConnection: TSQLConnection);

    procedure PerformMaintenance(const TableName: String;
      Tasks: TMaintenanceTasks);
    procedure PerformFullMaintenance;
    procedure GetTableStats(const TableName: String);
  end;

implementation

constructor TDatabaseMaintenance.Create(AConnection: TSQLConnection);  
begin
  FConnection := AConnection;
end;

procedure TDatabaseMaintenance.ExecuteVacuum(const TableName: String);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    WriteLn('VACUUM sur ', TableName, '...');

    {$IFDEF POSTGRES}
    // PostgreSQL
    Query.SQL.Text := 'VACUUM ANALYZE ' + TableName;
    Query.ExecSQL;
    {$ELSE}
    {$IFDEF MYSQL}
    // MySQL
    Query.SQL.Text := 'OPTIMIZE TABLE ' + TableName;
    Query.ExecSQL;
    {$ENDIF}
    {$ENDIF}

    WriteLn('VACUUM terminé pour ', TableName);
  finally
    Query.Free;
  end;
end;

procedure TDatabaseMaintenance.ExecuteAnalyze(const TableName: String);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    WriteLn('ANALYZE sur ', TableName, '...');

    {$IFDEF POSTGRES}
    Query.SQL.Text := 'ANALYZE ' + TableName;
    {$ELSE}
    Query.SQL.Text := 'ANALYZE TABLE ' + TableName;
    {$ENDIF}

    Query.ExecSQL;
    WriteLn('ANALYZE terminé pour ', TableName);
  finally
    Query.Free;
  end;
end;

procedure TDatabaseMaintenance.ExecuteReindex(const TableName: String);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    WriteLn('REINDEX sur ', TableName, '...');

    {$IFDEF POSTGRES}
    Query.SQL.Text := 'REINDEX TABLE ' + TableName;
    Query.ExecSQL;
    {$ENDIF}

    WriteLn('REINDEX terminé pour ', TableName);
  finally
    Query.Free;
  end;
end;

procedure TDatabaseMaintenance.PerformMaintenance(const TableName: String;
  Tasks: TMaintenanceTasks);
begin
  if mtVacuum in Tasks then
    ExecuteVacuum(TableName);

  if mtAnalyze in Tasks then
    ExecuteAnalyze(TableName);

  if mtReindex in Tasks then
    ExecuteReindex(TableName);
end;

procedure TDatabaseMaintenance.PerformFullMaintenance;  
var
  Query: TSQLQuery;
  Tables: TStringList;
begin
  Query := TSQLQuery.Create(nil);
  Tables := TStringList.Create;
  try
    Query.Database := FConnection;

    // Récupérer la liste des tables
    {$IFDEF POSTGRES}
    Query.SQL.Text :=
      'SELECT tablename FROM pg_tables ' +
      'WHERE schemaname = ''public''';
    {$ELSE}
    Query.SQL.Text := 'SHOW TABLES';
    {$ENDIF}

    Query.Open;
    while not Query.EOF do
    begin
      Tables.Add(Query.Fields[0].AsString);
      Query.Next;
    end;
    Query.Close;

    // Maintenance sur toutes les tables
    WriteLn('Maintenance complète sur ', Tables.Count, ' tables...');
    for var TableName in Tables do
    begin
      PerformMaintenance(TableName, [mtVacuum, mtAnalyze, mtReindex]);
    end;

    WriteLn('Maintenance complète terminée');
  finally
    Tables.Free;
    Query.Free;
  end;
end;

procedure TDatabaseMaintenance.GetTableStats(const TableName: String);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    {$IFDEF POSTGRES}
    Query.SQL.Text :=
      'SELECT ' +
      '  schemaname, ' +
      '  tablename, ' +
      '  pg_size_pretty(pg_total_relation_size(schemaname||''.''||tablename)) as size, ' +
      '  n_live_tup as rows, ' +
      '  n_dead_tup as dead_rows ' +
      'FROM pg_stat_user_tables ' +
      'WHERE tablename = :tablename';
    Query.Params.ParamByName('tablename').AsString := TableName;

    Query.Open;
    if not Query.EOF then
    begin
      WriteLn('Statistiques pour ', TableName, ':');
      WriteLn('  Taille: ', Query.FieldByName('size').AsString);
      WriteLn('  Lignes: ', Query.FieldByName('rows').AsInteger);
      WriteLn('  Lignes mortes: ', Query.FieldByName('dead_rows').AsInteger);
    end;
    Query.Close;
    {$ENDIF}
  finally
    Query.Free;
  end;
end;

end.
```

### Utilisation de la maintenance

```pascal
var
  Maintenance: TDatabaseMaintenance;

begin
  Maintenance := TDatabaseMaintenance.Create(Connection);
  try
    // Statistiques avant maintenance
    Maintenance.GetTableStats('clients');

    // Maintenance ciblée
    Maintenance.PerformMaintenance('clients', [mtVacuum, mtAnalyze]);

    // Statistiques après maintenance
    Maintenance.GetTableStats('clients');

    // Ou maintenance complète
    // Maintenance.PerformFullMaintenance;
  finally
    Maintenance.Free;
  end;
end;
```

## Monitoring en production

### Classe de monitoring des requêtes lentes

```pascal
unit SlowQueryLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TSlowQueryLogger = class
  private
    FThresholdMS: Integer;
    FLogFile: String;
    procedure LogSlowQuery(const SQL: String; DurationMS: Integer);
  public
    constructor Create(const ALogFile: String; AThresholdMS: Integer = 1000);

    procedure CheckQuery(const SQL: String; StartTime: TDateTime);

    property ThresholdMS: Integer read FThresholdMS write FThresholdMS;
  end;

implementation

constructor TSlowQueryLogger.Create(const ALogFile: String; AThresholdMS: Integer);  
begin
  FLogFile := ALogFile;
  FThresholdMS := AThresholdMS;
end;

procedure TSlowQueryLogger.LogSlowQuery(const SQL: String; DurationMS: Integer);  
var
  F: TextFile;
begin
  AssignFile(F, FLogFile);
  try
    if FileExists(FLogFile) then
      Append(F)
    else
      Rewrite(F);

    WriteLn(F, '=== Requête lente détectée ===');
    WriteLn(F, 'Date: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    WriteLn(F, 'Durée: ', DurationMS, ' ms');
    WriteLn(F, 'SQL: ', SQL);
    WriteLn(F, '');

    CloseFile(F);
  except
    on E: Exception do
      WriteLn('Erreur logging: ', E.Message);
  end;
end;

procedure TSlowQueryLogger.CheckQuery(const SQL: String; StartTime: TDateTime);  
var
  DurationMS: Integer;
begin
  DurationMS := MilliSecondsBetween(Now, StartTime);

  if DurationMS >= FThresholdMS then
    LogSlowQuery(SQL, DurationMS);
end;

end.
```

### Wrapper automatique avec logging

```pascal
unit QueryExecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, SlowQueryLogger;

type
  TQueryExecutor = class
  private
    FConnection: TSQLConnection;
    FSlowQueryLogger: TSlowQueryLogger;
  public
    constructor Create(AConnection: TSQLConnection);
    destructor Destroy; override;

    function Execute(const SQL: String): Integer;
    function Open(const SQL: String): TSQLQuery;

    property SlowQueryLogger: TSlowQueryLogger read FSlowQueryLogger;
  end;

implementation

constructor TQueryExecutor.Create(AConnection: TSQLConnection);  
begin
  FConnection := AConnection;

  // Logger les requêtes > 1000ms sous Windows, > 500ms sous Ubuntu
  {$IFDEF WINDOWS}
  FSlowQueryLogger := TSlowQueryLogger.Create('slow_queries.log', 1000);
  {$ELSE}
  FSlowQueryLogger := TSlowQueryLogger.Create('slow_queries.log', 500);
  {$ENDIF}
end;

destructor TQueryExecutor.Destroy;  
begin
  FSlowQueryLogger.Free;
  inherited;
end;

function TQueryExecutor.Execute(const SQL: String): Integer;  
var
  Query: TSQLQuery;
  StartTime: TDateTime;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := SQL;

    StartTime := Now;
    Query.ExecSQL;
    Result := Query.RowsAffected;

    FSlowQueryLogger.CheckQuery(SQL, StartTime);
  finally
    Query.Free;
  end;
end;

function TQueryExecutor.Open(const SQL: String): TSQLQuery;  
var
  StartTime: TDateTime;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConnection;
  Result.SQL.Text := SQL;

  StartTime := Now;
  Result.Open;

  FSlowQueryLogger.CheckQuery(SQL, StartTime);
end;

end.
```

## Checklist d'optimisation

Voici une checklist pratique pour optimiser vos bases de données :

### Avant de déployer

- [ ] Tous les WHERE contiennent des colonnes indexées
- [ ] Les clés étrangères ont des index
- [ ] Les colonnes utilisées dans JOIN sont indexées
- [ ] Les colonnes utilisées dans ORDER BY sont indexées
- [ ] Aucun SELECT * dans le code de production
- [ ] Les requêtes N+1 ont été éliminées
- [ ] EXPLAIN a été exécuté sur les requêtes principales
- [ ] Les requêtes complexes utilisent des vues matérialisées
- [ ] Un système de cache est en place
- [ ] Un monitoring des requêtes lentes est actif

### Configuration de la base

- [ ] `shared_buffers` configuré (25% RAM sur Ubuntu, moins sur Windows)
- [ ] `effective_cache_size` configuré (50-75% RAM)
- [ ] `work_mem` configuré selon la charge
- [ ] Connexions poolées configurées
- [ ] Logs des requêtes lentes activés
- [ ] Auto-vacuum activé (PostgreSQL)
- [ ] Stratégie de backup en place

### En production

- [ ] Monitoring actif des performances
- [ ] Alertes sur requêtes lentes configurées
- [ ] Maintenance régulière planifiée (VACUUM, ANALYZE)
- [ ] Statistiques de tables à jour
- [ ] Index inutilisés identifiés et supprimés
- [ ] Croissance de la base surveillée
- [ ] Plans d'exécution vérifiés régulièrement

## Conclusion

L'optimisation des requêtes et l'indexation sont des compétences essentielles qui feront la différence entre une application lente et inutilisable, et une application rapide et agréable à utiliser.

### Points clés à retenir

1. **Les index sont vos amis** - Mais n'en abusez pas
2. **Mesurez avant d'optimiser** - Utilisez EXPLAIN et le profiling
3. **Optimisez les requêtes fréquentes** - 80% du temps est passé dans 20% des requêtes
4. **Testez sur des données réelles** - 100 lignes ne révèlent pas les problèmes
5. **Surveillez en production** - Les performances changent avec le temps
6. **Maintenez régulièrement** - VACUUM et ANALYZE sont essentiels
7. **Adaptez selon l'OS** - Windows et Ubuntu ont des caractéristiques différentes

Avec FreePascal/Lazarus et les techniques présentées dans ce chapitre, vous disposez de tous les outils nécessaires pour créer des applications performantes sur Windows et Ubuntu, capables de gérer efficacement de grandes quantités de données.

⏭️ [Réplication et synchronisation](/08-bases-donnees-orm-multiplatefomes/10-replication-synchronisation.md)
