🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 25.1 ERP complet multi-plateforme

## Introduction

Un ERP (Enterprise Resource Planning ou Progiciel de Gestion Intégré) est un système informatique qui permet de gérer l'ensemble des processus d'une entreprise : comptabilité, ventes, achats, stocks, ressources humaines, production, etc. Dans ce chapitre, nous allons explorer comment concevoir et développer un ERP complet avec FreePascal/Lazarus qui fonctionne aussi bien sur Windows que sur Ubuntu.

## Qu'est-ce qu'un ERP ?

Un ERP moderne comprend généralement les modules suivants :

- **Gestion commerciale** : devis, commandes, factures, clients
- **Comptabilité** : écritures comptables, bilan, compte de résultat
- **Gestion des stocks** : inventaire, mouvements, approvisionnements
- **Achats** : fournisseurs, commandes d'achat, réceptions
- **Ressources Humaines** : employés, paie, congés, formations
- **Production** : nomenclatures, ordres de fabrication, suivi
- **CRM** : relation client, opportunités commerciales, historique
- **Reporting et tableaux de bord** : indicateurs de performance

## Pourquoi FreePascal/Lazarus pour un ERP ?

FreePascal et Lazarus présentent plusieurs avantages pour développer un ERP :

1. **Multi-plateforme natif** : le même code source compile pour Windows et Linux
2. **Performance** : code natif compilé, pas d'interpréteur ou de machine virtuelle
3. **Interface riche** : composants GUI complets via la LCL
4. **Accès bases de données** : support natif de PostgreSQL, MySQL, SQLite, Firebird
5. **Gratuit et open source** : aucun coût de licence pour vous ou vos clients
6. **Déploiement simple** : exécutable autonome, pas de runtime complexe à installer

## Architecture globale d'un ERP multi-plateforme

### Architecture en couches

Pour un ERP maintenable et évolutif, nous adoptons une architecture en trois couches :

```
┌─────────────────────────────────────┐
│   Couche Présentation (UI)          │
│   - Formulaires LCL                 │
│   - Grilles de données              │
│   - Rapports et impressions         │
└─────────────────────────────────────┘
              ↕
┌─────────────────────────────────────┐
│   Couche Métier (Business Logic)    │
│   - Règles de gestion               │
│   - Calculs et validations          │
│   - Workflow                        │
└─────────────────────────────────────┘
              ↕
┌─────────────────────────────────────┐
│   Couche Données (Data Access)      │
│   - Accès base de données           │
│   - ORM ou SQL direct               │
│   - Cache et optimisations          │
└─────────────────────────────────────┘
```

### Organisation des modules

Structurez votre projet en modules indépendants :

```
MonERP/
├── Core/                  # Noyau commun
│   ├── Database/         # Accès données
│   ├── Common/           # Utilitaires partagés
│   └── Security/         # Authentification, droits
├── Modules/
│   ├── Sales/           # Ventes
│   ├── Purchases/       # Achats
│   ├── Accounting/      # Comptabilité
│   ├── Inventory/       # Stocks
│   ├── HR/              # Ressources Humaines
│   └── Production/      # Production
├── UI/                   # Interface utilisateur
│   ├── Forms/           # Formulaires
│   ├── Reports/         # Rapports
│   └── Common/          # Composants UI réutilisables
└── Platform/            # Spécificités plateforme
    ├── Windows/
    └── Linux/
```

## Gestion multi-plateforme : les points clés

### 1. Gestion des chemins de fichiers

Les chemins de fichiers sont différents entre Windows et Linux. Utilisez toujours les fonctions de la RTL :

```pascal
uses
  SysUtils, FileUtil;

function GetConfigPath: string;
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + PathDelim + 'MonERP' + PathDelim;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME') + PathDelim + '.monerp' + PathDelim;
  {$ENDIF}

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetDatabasePath: string;
begin
  Result := GetConfigPath + 'database' + PathDelim;
end;
```

**Points importants :**
- Utilisez `PathDelim` au lieu de `\` ou `/`
- Sur Windows, les données vont dans `%APPDATA%`
- Sur Linux, les données vont dans le home de l'utilisateur (dossier caché)

### 2. Connexion à la base de données

Choisissez une base de données multi-plateforme. Recommandations :

**PostgreSQL** : excellente pour les ERP, robuste, conforme ACID
```pascal
uses
  sqldb, pqconnection;

procedure TDataModule.ConnectDatabase;
begin
  Connection := TPQConnection.Create(nil);
  Connection.HostName := 'localhost';
  Connection.DatabaseName := 'monerp';
  Connection.UserName := 'erp_user';
  Connection.Password := GetPassword; // À sécuriser !

  {$IFDEF WINDOWS}
  // Spécifier le chemin de la DLL PostgreSQL si nécessaire
  Connection.Params.Add('ClientLib=libpq.dll');
  {$ENDIF}
  {$IFDEF UNIX}
  Connection.Params.Add('ClientLib=libpq.so');
  {$ENDIF}

  Connection.Open;
end;
```

**SQLite** : parfait pour les petites installations, sans serveur
```pascal
uses
  sqldb, sqlite3conn;

procedure TDataModule.ConnectSQLite;
begin
  Connection := TSQLite3Connection.Create(nil);
  Connection.DatabaseName := GetDatabasePath + 'monerp.db';
  Connection.Open;
end;
```

### 3. Interface utilisateur portable

La LCL s'occupe de l'adaptation automatique, mais quelques précautions :

```pascal
type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    procedure AdjustForPlatform;
  end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AdjustForPlatform;
end;

procedure TMainForm.AdjustForPlatform;
begin
  {$IFDEF WINDOWS}
  // Windows utilise Segoe UI par défaut
  Font.Name := 'Segoe UI';
  Font.Size := 9;
  {$ENDIF}

  {$IFDEF UNIX}
  // Linux utilise généralement des polices légèrement plus grandes
  Font.Name := 'Sans';
  Font.Size := 10;
  {$ENDIF}

  // Adaptation High-DPI
  if Screen.PixelsPerInch > 96 then
    ScaleFactor := Screen.PixelsPerInch / 96;
end;
```

### 4. Impression et rapports

Pour les rapports multi-plateformes, plusieurs solutions :

**Solution 1 : LazReport**
```pascal
uses
  LR_Class, LR_DBSet;

procedure TFactureForm.ImprimerFacture;
var
  Report: TfrReport;
begin
  Report := TfrReport.Create(nil);
  try
    Report.LoadFromFile(GetReportsPath + 'facture.lrf');
    Report.ShowReport;
  finally
    Report.Free;
  end;
end;
```

**Solution 2 : Génération PDF avec fpReport**
```pascal
uses
  fpreport, fpreportpdfexport;

procedure GenererPDF(const NomFichier: string);
var
  Report: TFPReport;
  Exporter: TFPReportExportPDF;
begin
  Report := TFPReport.Create(nil);
  Exporter := TFPReportExportPDF.Create(nil);
  try
    // Configurer le rapport
    ConfigurerRapport(Report);

    // Exporter
    Exporter.FileName := NomFichier;
    Exporter.Execute(Report);
  finally
    Exporter.Free;
    Report.Free;
  end;
end;
```

## Structure de la base de données

Voici un exemple de structure pour les tables principales :

```sql
-- Table des entreprises (multi-société)
CREATE TABLE companies (
    id SERIAL PRIMARY KEY,
    code VARCHAR(10) UNIQUE NOT NULL,
    name VARCHAR(100) NOT NULL,
    address TEXT,
    vat_number VARCHAR(20),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Table des utilisateurs
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    username VARCHAR(50) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    email VARCHAR(100),
    is_active BOOLEAN DEFAULT true,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Table des clients
CREATE TABLE customers (
    id SERIAL PRIMARY KEY,
    company_id INTEGER REFERENCES companies(id),
    code VARCHAR(20) UNIQUE NOT NULL,
    name VARCHAR(100) NOT NULL,
    address TEXT,
    phone VARCHAR(20),
    email VARCHAR(100),
    vat_number VARCHAR(20),
    payment_terms INTEGER DEFAULT 30,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Table des articles
CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    company_id INTEGER REFERENCES companies(id),
    code VARCHAR(30) UNIQUE NOT NULL,
    description VARCHAR(200) NOT NULL,
    unit_price DECIMAL(15,2) DEFAULT 0,
    vat_rate DECIMAL(5,2) DEFAULT 20.00,
    stock_quantity DECIMAL(15,3) DEFAULT 0,
    is_active BOOLEAN DEFAULT true,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Table des factures
CREATE TABLE invoices (
    id SERIAL PRIMARY KEY,
    company_id INTEGER REFERENCES companies(id),
    invoice_number VARCHAR(20) UNIQUE NOT NULL,
    customer_id INTEGER REFERENCES customers(id),
    invoice_date DATE NOT NULL,
    due_date DATE,
    total_ht DECIMAL(15,2) DEFAULT 0,
    total_vat DECIMAL(15,2) DEFAULT 0,
    total_ttc DECIMAL(15,2) DEFAULT 0,
    status VARCHAR(20) DEFAULT 'draft',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Table des lignes de facture
CREATE TABLE invoice_lines (
    id SERIAL PRIMARY KEY,
    invoice_id INTEGER REFERENCES invoices(id) ON DELETE CASCADE,
    product_id INTEGER REFERENCES products(id),
    description VARCHAR(200),
    quantity DECIMAL(15,3) NOT NULL,
    unit_price DECIMAL(15,2) NOT NULL,
    vat_rate DECIMAL(5,2) NOT NULL,
    line_total DECIMAL(15,2) NOT NULL,
    line_number INTEGER NOT NULL
);
```

## Implémentation des modules principaux

### Module de gestion des clients

```pascal
unit CustomerManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db;

type
  TCustomer = class
  private
    FId: Integer;
    FCode: string;
    FName: string;
    FAddress: string;
    FPhone: string;
    FEmail: string;
  public
    property Id: Integer read FId write FId;
    property Code: string read FCode write FCode;
    property Name: string read FName write FName;
    property Address: string read FAddress write FAddress;
    property Phone: string read FPhone write FPhone;
    property Email: string read FEmail write FEmail;

    function Save(Connection: TSQLConnection): Boolean;
    function Delete(Connection: TSQLConnection): Boolean;
    class function LoadById(Connection: TSQLConnection; AId: Integer): TCustomer;
  end;

implementation

function TCustomer.Save(Connection: TSQLConnection): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;

    if FId = 0 then
    begin
      // Insertion
      Query.SQL.Text :=
        'INSERT INTO customers (code, name, address, phone, email, company_id) ' +
        'VALUES (:code, :name, :address, :phone, :email, :company_id) ' +
        'RETURNING id';
    end
    else
    begin
      // Mise à jour
      Query.SQL.Text :=
        'UPDATE customers SET code = :code, name = :name, ' +
        'address = :address, phone = :phone, email = :email ' +
        'WHERE id = :id';
      Query.Params.ParamByName('id').AsInteger := FId;
    end;

    Query.Params.ParamByName('code').AsString := FCode;
    Query.Params.ParamByName('name').AsString := FName;
    Query.Params.ParamByName('address').AsString := FAddress;
    Query.Params.ParamByName('phone').AsString := FPhone;
    Query.Params.ParamByName('email').AsString := FEmail;
    Query.Params.ParamByName('company_id').AsInteger := 1; // À adapter

    Query.ExecSQL;

    if FId = 0 then
    begin
      Query.Open;
      FId := Query.Fields[0].AsInteger;
    end;

    Connection.Transaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      Connection.Transaction.Rollback;
      raise;
    end;
  end;
  Query.Free;
end;

class function TCustomer.LoadById(Connection: TSQLConnection; AId: Integer): TCustomer;
var
  Query: TSQLQuery;
begin
  Result := nil;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text := 'SELECT * FROM customers WHERE id = :id';
    Query.Params.ParamByName('id').AsInteger := AId;
    Query.Open;

    if not Query.EOF then
    begin
      Result := TCustomer.Create;
      Result.FId := Query.FieldByName('id').AsInteger;
      Result.FCode := Query.FieldByName('code').AsString;
      Result.FName := Query.FieldByName('name').AsString;
      Result.FAddress := Query.FieldByName('address').AsString;
      Result.FPhone := Query.FieldByName('phone').AsString;
      Result.FEmail := Query.FieldByName('email').AsString;
    end;
  finally
    Query.Free;
  end;
end;

function TCustomer.Delete(Connection: TSQLConnection): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text := 'DELETE FROM customers WHERE id = :id';
    Query.Params.ParamByName('id').AsInteger := FId;
    Query.ExecSQL;
    Connection.Transaction.Commit;
    Result := True;
  except
    Connection.Transaction.Rollback;
    raise;
  end;
  Query.Free;
end;

end.
```

### Module de facturation

```pascal
unit InvoiceManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, CustomerManager;

type
  TInvoiceLine = class
  private
    FProductId: Integer;
    FDescription: string;
    FQuantity: Double;
    FUnitPrice: Double;
    FVatRate: Double;
  public
    property ProductId: Integer read FProductId write FProductId;
    property Description: string read FDescription write FDescription;
    property Quantity: Double read FQuantity write FQuantity;
    property UnitPrice: Double read FUnitPrice write FUnitPrice;
    property VatRate: Double read FVatRate write FVatRate;

    function GetLineTotal: Double;
    function GetVatAmount: Double;
  end;

  TInvoice = class
  private
    FId: Integer;
    FInvoiceNumber: string;
    FCustomerId: Integer;
    FInvoiceDate: TDateTime;
    FDueDate: TDateTime;
    FLines: TList;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: Integer read FId write FId;
    property InvoiceNumber: string read FInvoiceNumber write FInvoiceNumber;
    property CustomerId: Integer read FCustomerId write FCustomerId;
    property InvoiceDate: TDateTime read FInvoiceDate write FInvoiceDate;
    property DueDate: TDateTime read FDueDate write FDueDate;
    property Lines: TList read FLines;

    procedure AddLine(ALine: TInvoiceLine);
    function GetTotalHT: Double;
    function GetTotalVAT: Double;
    function GetTotalTTC: Double;
    function Save(Connection: TSQLConnection): Boolean;
  end;

implementation

{ TInvoiceLine }

function TInvoiceLine.GetLineTotal: Double;
begin
  Result := FQuantity * FUnitPrice;
end;

function TInvoiceLine.GetVatAmount: Double;
begin
  Result := GetLineTotal * (FVatRate / 100);
end;

{ TInvoice }

constructor TInvoice.Create;
begin
  inherited Create;
  FLines := TList.Create;
  FInvoiceDate := Date;
  FDueDate := Date + 30; // 30 jours par défaut
end;

destructor TInvoice.Destroy;
var
  i: Integer;
begin
  for i := 0 to FLines.Count - 1 do
    TInvoiceLine(FLines[i]).Free;
  FLines.Free;
  inherited Destroy;
end;

procedure TInvoice.AddLine(ALine: TInvoiceLine);
begin
  FLines.Add(ALine);
end;

function TInvoice.GetTotalHT: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FLines.Count - 1 do
    Result := Result + TInvoiceLine(FLines[i]).GetLineTotal;
end;

function TInvoice.GetTotalVAT: Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FLines.Count - 1 do
    Result := Result + TInvoiceLine(FLines[i]).GetVatAmount;
end;

function TInvoice.GetTotalTTC: Double;
begin
  Result := GetTotalHT + GetTotalVAT;
end;

function TInvoice.Save(Connection: TSQLConnection): Boolean;
var
  Query: TSQLQuery;
  i: Integer;
  Line: TInvoiceLine;
begin
  Result := False;
  Query := TSQLQuery.Create(nil);
  try
    Connection.Transaction.StartTransaction;
    Query.DataBase := Connection;

    // Sauvegarder l'en-tête de facture
    if FId = 0 then
    begin
      Query.SQL.Text :=
        'INSERT INTO invoices (invoice_number, customer_id, invoice_date, ' +
        'due_date, total_ht, total_vat, total_ttc, company_id) ' +
        'VALUES (:number, :customer, :invdate, :duedate, :ht, :vat, :ttc, :company) ' +
        'RETURNING id';
    end
    else
    begin
      Query.SQL.Text :=
        'UPDATE invoices SET invoice_number = :number, customer_id = :customer, ' +
        'invoice_date = :invdate, due_date = :duedate, total_ht = :ht, ' +
        'total_vat = :vat, total_ttc = :ttc WHERE id = :id';
      Query.Params.ParamByName('id').AsInteger := FId;
    end;

    Query.Params.ParamByName('number').AsString := FInvoiceNumber;
    Query.Params.ParamByName('customer').AsInteger := FCustomerId;
    Query.Params.ParamByName('invdate').AsDate := FInvoiceDate;
    Query.Params.ParamByName('duedate').AsDate := FDueDate;
    Query.Params.ParamByName('ht').AsFloat := GetTotalHT;
    Query.Params.ParamByName('vat').AsFloat := GetTotalVAT;
    Query.Params.ParamByName('ttc').AsFloat := GetTotalTTC;
    Query.Params.ParamByName('company').AsInteger := 1;

    Query.ExecSQL;

    if FId = 0 then
    begin
      Query.Open;
      FId := Query.Fields[0].AsInteger;
    end;

    // Supprimer les anciennes lignes
    Query.Close;
    Query.SQL.Text := 'DELETE FROM invoice_lines WHERE invoice_id = :id';
    Query.Params.ParamByName('id').AsInteger := FId;
    Query.ExecSQL;

    // Insérer les nouvelles lignes
    Query.SQL.Text :=
      'INSERT INTO invoice_lines (invoice_id, product_id, description, ' +
      'quantity, unit_price, vat_rate, line_total, line_number) ' +
      'VALUES (:invoice, :product, :desc, :qty, :price, :vat, :total, :line)';

    for i := 0 to FLines.Count - 1 do
    begin
      Line := TInvoiceLine(FLines[i]);
      Query.Params.ParamByName('invoice').AsInteger := FId;
      Query.Params.ParamByName('product').AsInteger := Line.ProductId;
      Query.Params.ParamByName('desc').AsString := Line.Description;
      Query.Params.ParamByName('qty').AsFloat := Line.Quantity;
      Query.Params.ParamByName('price').AsFloat := Line.UnitPrice;
      Query.Params.ParamByName('vat').AsFloat := Line.VatRate;
      Query.Params.ParamByName('total').AsFloat := Line.GetLineTotal;
      Query.Params.ParamByName('line').AsInteger := i + 1;
      Query.ExecSQL;
    end;

    Connection.Transaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      Connection.Transaction.Rollback;
      raise;
    end;
  end;
  Query.Free;
end;

end.
```

## Gestion de la sécurité

### Authentification

```pascal
unit Security;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, sha1; // Pour le hachage

type
  TUser = class
  private
    FId: Integer;
    FUsername: string;
    FEmail: string;
  public
    property Id: Integer read FId;
    property Username: string read FUsername;
    property Email: string read FEmail;
  end;

  TSecurityManager = class
  public
    class function HashPassword(const Password: string): string;
    class function Authenticate(Connection: TSQLConnection;
      const Username, Password: string): TUser;
    class function ChangePassword(Connection: TSQLConnection;
      UserId: Integer; const NewPassword: string): Boolean;
  end;

implementation

class function TSecurityManager.HashPassword(const Password: string): string;
begin
  // Utiliser SHA256 ou mieux, bcrypt en production
  Result := SHA1Print(SHA1String(Password));
end;

class function TSecurityManager.Authenticate(Connection: TSQLConnection;
  const Username, Password: string): TUser;
var
  Query: TSQLQuery;
  HashedPassword: string;
begin
  Result := nil;
  HashedPassword := HashPassword(Password);

  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text :=
      'SELECT id, username, email FROM users ' +
      'WHERE username = :username AND password_hash = :hash AND is_active = true';
    Query.Params.ParamByName('username').AsString := Username;
    Query.Params.ParamByName('hash').AsString := HashedPassword;
    Query.Open;

    if not Query.EOF then
    begin
      Result := TUser.Create;
      Result.FId := Query.FieldByName('id').AsInteger;
      Result.FUsername := Query.FieldByName('username').AsString;
      Result.FEmail := Query.FieldByName('email').AsString;
    end;
  finally
    Query.Free;
  end;
end;

class function TSecurityManager.ChangePassword(Connection: TSQLConnection;
  UserId: Integer; const NewPassword: string): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text :=
      'UPDATE users SET password_hash = :hash WHERE id = :id';
    Query.Params.ParamByName('hash').AsString := HashPassword(NewPassword);
    Query.Params.ParamByName('id').AsInteger := UserId;
    Query.ExecSQL;
    Connection.Transaction.Commit;
    Result := True;
  except
    Connection.Transaction.Rollback;
  end;
  Query.Free;
end;

end.
```

## Configuration et déploiement

### Fichier de configuration multi-plateforme

```pascal
unit Configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TAppConfig = class
  private
    FIniFile: TIniFile;
    FConfigPath: string;
  public
    constructor Create;
    destructor Destroy; override;

    function GetDatabaseHost: string;
    function GetDatabaseName: string;
    function GetDatabaseUser: string;
    procedure SetDatabaseHost(const Value: string);

    property ConfigPath: string read FConfigPath;
  end;

implementation

constructor TAppConfig.Create;
begin
  inherited Create;

  {$IFDEF WINDOWS}
  FConfigPath := GetEnvironmentVariable('APPDATA') + PathDelim +
                 'MonERP' + PathDelim + 'config.ini';
  {$ENDIF}
  {$IFDEF UNIX}
  FConfigPath := GetEnvironmentVariable('HOME') + PathDelim +
                 '.monerp' + PathDelim + 'config.ini';
  {$ENDIF}

  ForceDirectories(ExtractFilePath(FConfigPath));
  FIniFile := TIniFile.Create(FConfigPath);
end;

destructor TAppConfig.Destroy;
begin
  FIniFile.Free;
  inherited Destroy;
end;

function TAppConfig.GetDatabaseHost: string;
begin
  Result := FIniFile.ReadString('Database', 'Host', 'localhost');
end;

function TAppConfig.GetDatabaseName: string;
begin
  Result := FIniFile.ReadString('Database', 'Name', 'monerp');
end;

function TAppConfig.GetDatabaseUser: string;
begin
  Result := FIniFile.ReadString('Database', 'User', 'erp_user');
end;

procedure TAppConfig.SetDatabaseHost(const Value: string);
begin
  FIniFile.WriteString('Database', 'Host', Value);
end;

end.
```

### Script de déploiement Windows

```batch
@echo off
echo Installation de MonERP pour Windows
echo ====================================

REM Créer les répertoires
mkdir "%APPDATA%\MonERP"
mkdir "%APPDATA%\MonERP\reports"
mkdir "%APPDATA%\MonERP\database"

REM Copier les fichiers
copy MonERP.exe "%APPDATA%\MonERP\"
copy *.dll "%APPDATA%\MonERP\"
copy reports\*.* "%APPDATA%\MonERP\reports\"

REM Créer un raccourci sur le bureau
echo Creating desktop shortcut...
powershell "$s=(New-Object -COM WScript.Shell).CreateShortcut('%userprofile%\Desktop\MonERP.lnk');$s.TargetPath='%APPDATA%\MonERP\MonERP.exe';$s.Save()"

echo Installation terminee!
pause
```

### Script de déploiement Linux

```bash
#!/bin/bash
echo "Installation de MonERP pour Linux"
echo "=================================="

# Créer les répertoires
mkdir -p ~/.monerp
mkdir -p ~/.monerp/reports
mkdir -p ~/.monerp/database

# Copier les fichiers
cp monerp ~/.monerp/
chmod +x ~/.monerp/monerp
cp reports/* ~/.monerp/reports/

# Créer un fichier .desktop
cat > ~/.local/share/applications/monerp.desktop << EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=MonERP
Comment=Système de gestion d'entreprise
Exec=$HOME/.monerp/monerp
Icon=$HOME/.monerp/icon.png
Terminal=false
Categories=Office;
EOF

echo "Installation terminée!"
echo "Lancez MonERP depuis le menu des applications"
```

## Optimisations et bonnes pratiques

### 1. Mise en cache des données

```pascal
unit DataCache;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TCachedItem = class
  private
    FData: TObject;
    FExpiry: TDateTime;
  public
    property Data: TObject read FData write FData;
    property Expiry: TDateTime read FExpiry write FExpiry;
  end;

  TDataCache = class
  private
    FCache: TDictionary<string, TCachedItem>;
    procedure CleanExpired;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Put(const Key: string; Data: TObject; DurationMinutes: Integer);
    function Get(const Key: string): TObject;
    procedure Clear;
    function Has(const Key: string): Boolean;
  end;

implementation

constructor TDataCache.Create;
begin
  inherited Create;
  FCache := TDictionary<string, TCachedItem>.Create;
end;

destructor TDataCache.Destroy;
begin
  Clear;
  FCache.Free;
  inherited Destroy;
end;

procedure TDataCache.Put(const Key: string; Data: TObject; DurationMinutes: Integer);
var
  Item: TCachedItem;
begin
  CleanExpired;

  if FCache.ContainsKey(Key) then
  begin
    Item := FCache[Key];
    Item.Data.Free;
  end
  else
  begin
    Item := TCachedItem.Create;
    FCache.Add(Key, Item);
  end;

  Item.Data := Data;
  Item.Expiry := Now + (DurationMinutes / (24 * 60));
end;

function TDataCache.Get(const Key: string): TObject;
var
  Item: TCachedItem;
begin
  Result := nil;

  if FCache.TryGetValue(Key, Item) then
  begin
    if Item.Expiry > Now then
      Result := Item.Data
    else
    begin
      // Élément expiré, le supprimer
      Item.Data.Free;
      FCache.Remove(Key);
      Item.Free;
    end;
  end;
end;

function TDataCache.Has(const Key: string): Boolean;
begin
  Result := Get(Key) <> nil;
end;

procedure TDataCache.Clear;
var
  Item: TCachedItem;
begin
  for Item in FCache.Values do
  begin
    Item.Data.Free;
    Item.Free;
  end;
  FCache.Clear;
end;

procedure TDataCache.CleanExpired;
var
  KeysToRemove: TStringList;
  Key: string;
  Item: TCachedItem;
begin
  KeysToRemove := TStringList.Create;
  try
    // Identifier les clés expirées
    for Key in FCache.Keys do
    begin
      Item := FCache[Key];
      if Item.Expiry <= Now then
        KeysToRemove.Add(Key);
    end;

    // Supprimer les éléments expirés
    for Key in KeysToRemove do
    begin
      Item := FCache[Key];
      Item.Data.Free;
      Item.Free;
      FCache.Remove(Key);
    end;
  finally
    KeysToRemove.Free;
  end;
end;

end.
```

**Utilisation du cache :**

```pascal
var
  Cache: TDataCache;
  Customer: TCustomer;
begin
  Cache := TDataCache.Create;
  try
    // Mettre en cache
    Customer := TCustomer.LoadById(Connection, 123);
    Cache.Put('customer_123', Customer, 15); // Cache pour 15 minutes

    // Récupérer du cache
    Customer := TCustomer(Cache.Get('customer_123'));
    if Customer = nil then
      Customer := TCustomer.LoadById(Connection, 123); // Recharger si absent
  finally
    Cache.Free;
  end;
end;
```

### 2. Pool de connexions

Pour améliorer les performances, surtout dans un contexte multi-utilisateurs :

```pascal
unit ConnectionPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, pqconnection, SyncObjs;

type
  TConnectionPool = class
  private
    FConnections: TList;
    FAvailable: TList;
    FLock: TCriticalSection;
    FMaxConnections: Integer;
    FConnectionString: string;

    function CreateConnection: TSQLConnection;
  public
    constructor Create(const AConnectionString: string; AMaxConnections: Integer = 10);
    destructor Destroy; override;

    function AcquireConnection: TSQLConnection;
    procedure ReleaseConnection(Connection: TSQLConnection);
  end;

implementation

constructor TConnectionPool.Create(const AConnectionString: string;
  AMaxConnections: Integer);
begin
  inherited Create;
  FConnectionString := AConnectionString;
  FMaxConnections := AMaxConnections;
  FConnections := TList.Create;
  FAvailable := TList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TConnectionPool.Destroy;
var
  i: Integer;
begin
  for i := 0 to FConnections.Count - 1 do
    TSQLConnection(FConnections[i]).Free;

  FConnections.Free;
  FAvailable.Free;
  FLock.Free;
  inherited Destroy;
end;

function TConnectionPool.CreateConnection: TSQLConnection;
var
  Conn: TPQConnection;
begin
  Conn := TPQConnection.Create(nil);
  // Parser la chaîne de connexion et configurer
  Conn.HostName := 'localhost';
  Conn.DatabaseName := 'monerp';
  Conn.UserName := 'erp_user';
  Conn.Password := 'password';
  Conn.Open;

  Result := Conn;
end;

function TConnectionPool.AcquireConnection: TSQLConnection;
begin
  FLock.Enter;
  try
    if FAvailable.Count > 0 then
    begin
      Result := TSQLConnection(FAvailable[FAvailable.Count - 1]);
      FAvailable.Delete(FAvailable.Count - 1);
    end
    else if FConnections.Count < FMaxConnections then
    begin
      Result := CreateConnection;
      FConnections.Add(Result);
    end
    else
    begin
      // Attendre qu'une connexion se libère
      // (version simplifiée, devrait utiliser un événement)
      FLock.Leave;
      Sleep(100);
      Result := AcquireConnection;
      Exit;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TConnectionPool.ReleaseConnection(Connection: TSQLConnection);
begin
  FLock.Enter;
  try
    if FAvailable.IndexOf(Connection) = -1 then
      FAvailable.Add(Connection);
  finally
    FLock.Leave;
  end;
end;

end.
```

### 3. Gestion des transactions

```pascal
unit TransactionHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb;

type
  TTransactionScope = class
  private
    FTransaction: TSQLTransaction;
    FCommitted: Boolean;
  public
    constructor Create(ATransaction: TSQLTransaction);
    destructor Destroy; override;

    procedure Commit;
    procedure Rollback;
  end;

implementation

constructor TTransactionScope.Create(ATransaction: TSQLTransaction);
begin
  inherited Create;
  FTransaction := ATransaction;
  FCommitted := False;
  FTransaction.StartTransaction;
end;

destructor TTransactionScope.Destroy;
begin
  if not FCommitted then
    FTransaction.Rollback;
  inherited Destroy;
end;

procedure TTransactionScope.Commit;
begin
  FTransaction.Commit;
  FCommitted := True;
end;

procedure TTransactionScope.Rollback;
begin
  FTransaction.Rollback;
  FCommitted := True;
end;

end.
```

**Utilisation :**

```pascal
var
  Scope: TTransactionScope;
begin
  Scope := TTransactionScope.Create(Connection.Transaction);
  try
    // Opérations de base de données
    Customer.Save(Connection);
    Invoice.Save(Connection);

    // Tout s'est bien passé
    Scope.Commit;
  except
    // En cas d'erreur, rollback automatique dans le destructeur
    raise;
  end;
  Scope.Free;
end;
```

## Interface utilisateur avancée

### Formulaire principal multi-onglets

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, StdCtrls;

type
  TfrmMain = class(TForm)
    MainMenu: TMainMenu;
    StatusBar: TStatusBar;
    PageControl: TPageControl;

    // Menus
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuSales: TMenuItem;
    mnuSalesCustomers: TMenuItem;
    mnuSalesInvoices: TMenuItem;
    mnuPurchases: TMenuItem;
    mnuInventory: TMenuItem;
    mnuReports: TMenuItem;
    mnuHelp: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuSalesCustomersClick(Sender: TObject);
    procedure mnuSalesInvoicesClick(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
  private
    FCurrentUser: TUser;
    procedure LoadUserPreferences;
    procedure SaveUserPreferences;
    procedure UpdateStatusBar;
  public
    property CurrentUser: TUser read FCurrentUser write FCurrentUser;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  CustomerForm, InvoiceForm, Configuration;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := 'MonERP - Système de Gestion Intégré';

  {$IFDEF WINDOWS}
  Font.Name := 'Segoe UI';
  Font.Size := 9;
  {$ENDIF}

  {$IFDEF UNIX}
  Font.Name := 'Sans';
  Font.Size := 10;
  {$ENDIF}

  LoadUserPreferences;
  UpdateStatusBar;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  SaveUserPreferences;
end;

procedure TfrmMain.LoadUserPreferences;
var
  Config: TAppConfig;
begin
  Config := TAppConfig.Create;
  try
    // Restaurer la position et taille de la fenêtre
    Left := Config.FIniFile.ReadInteger('Window', 'Left', 100);
    Top := Config.FIniFile.ReadInteger('Window', 'Top', 100);
    Width := Config.FIniFile.ReadInteger('Window', 'Width', 1024);
    Height := Config.FIniFile.ReadInteger('Window', 'Height', 768);

    if Config.FIniFile.ReadBool('Window', 'Maximized', False) then
      WindowState := wsMaximized;
  finally
    Config.Free;
  end;
end;

procedure TfrmMain.SaveUserPreferences;
var
  Config: TAppConfig;
begin
  Config := TAppConfig.Create;
  try
    if WindowState = wsNormal then
    begin
      Config.FIniFile.WriteInteger('Window', 'Left', Left);
      Config.FIniFile.WriteInteger('Window', 'Top', Top);
      Config.FIniFile.WriteInteger('Window', 'Width', Width);
      Config.FIniFile.WriteInteger('Window', 'Height', Height);
    end;

    Config.FIniFile.WriteBool('Window', 'Maximized', WindowState = wsMaximized);
  finally
    Config.Free;
  end;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  if Assigned(FCurrentUser) then
    StatusBar.Panels[0].Text := 'Utilisateur: ' + FCurrentUser.Username
  else
    StatusBar.Panels[0].Text := 'Non connecté';

  StatusBar.Panels[1].Text := 'Base de données: Connectée';
  StatusBar.Panels[2].Text := FormatDateTime('dd/mm/yyyy hh:nn', Now);
end;

procedure TfrmMain.mnuSalesCustomersClick(Sender: TObject);
var
  TabSheet: TTabSheet;
  CustomerForm: TfrmCustomers;
begin
  // Créer un nouvel onglet
  TabSheet := TTabSheet.Create(PageControl);
  TabSheet.PageControl := PageControl;
  TabSheet.Caption := 'Clients';

  // Créer le formulaire à l'intérieur de l'onglet
  CustomerForm := TfrmCustomers.Create(TabSheet);
  CustomerForm.Parent := TabSheet;
  CustomerForm.Align := alClient;
  CustomerForm.BorderStyle := bsNone;
  CustomerForm.Show;

  PageControl.ActivePage := TabSheet;
end;

procedure TfrmMain.mnuSalesInvoicesClick(Sender: TObject);
var
  TabSheet: TTabSheet;
  InvoiceForm: TfrmInvoices;
begin
  TabSheet := TTabSheet.Create(PageControl);
  TabSheet.PageControl := PageControl;
  TabSheet.Caption := 'Factures';

  InvoiceForm := TfrmInvoices.Create(TabSheet);
  InvoiceForm.Parent := TabSheet;
  InvoiceForm.Align := alClient;
  InvoiceForm.BorderStyle := bsNone;
  InvoiceForm.Show;

  PageControl.ActivePage := TabSheet;
end;

procedure TfrmMain.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

end.
```

### Formulaire de liste de clients avec recherche

```pascal
unit CustomerForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, Buttons, sqldb, db;

type
  TfrmCustomers = class(TForm)
    pnlTop: TPanel;
    edtSearch: TEdit;
    btnSearch: TButton;
    btnNew: TButton;
    btnEdit: TButton;
    btnDelete: TButton;

    gridCustomers: TStringGrid;

    DataSource: TDataSource;
    Query: TSQLQuery;

    procedure FormCreate(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure gridCustomersDblClick(Sender: TObject);
  private
    procedure LoadCustomers(const SearchTerm: string = '');
    procedure ConfigureGrid;
    function GetSelectedCustomerId: Integer;
  public
  end;

implementation

{$R *.lfm}

uses
  CustomerEditForm, DataModule;

procedure TfrmCustomers.FormCreate(Sender: TObject);
begin
  ConfigureGrid;
  LoadCustomers;
end;

procedure TfrmCustomers.ConfigureGrid;
begin
  gridCustomers.RowCount := 1;
  gridCustomers.ColCount := 6;
  gridCustomers.FixedRows := 1;
  gridCustomers.FixedCols := 0;

  // En-têtes
  gridCustomers.Cells[0, 0] := 'ID';
  gridCustomers.Cells[1, 0] := 'Code';
  gridCustomers.Cells[2, 0] := 'Nom';
  gridCustomers.Cells[3, 0] := 'Adresse';
  gridCustomers.Cells[4, 0] := 'Téléphone';
  gridCustomers.Cells[5, 0] := 'Email';

  // Largeurs de colonnes
  gridCustomers.ColWidths[0] := 50;
  gridCustomers.ColWidths[1] := 80;
  gridCustomers.ColWidths[2] := 200;
  gridCustomers.ColWidths[3] := 250;
  gridCustomers.ColWidths[4] := 120;
  gridCustomers.ColWidths[5] := 180;

  // Options
  gridCustomers.Options := gridCustomers.Options + [goRowSelect];
end;

procedure TfrmCustomers.LoadCustomers(const SearchTerm: string);
var
  Row: Integer;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := dmData.Connection;

    if SearchTerm <> '' then
    begin
      Query.SQL.Text :=
        'SELECT id, code, name, address, phone, email ' +
        'FROM customers ' +
        'WHERE LOWER(name) LIKE LOWER(:search) ' +
        '   OR LOWER(code) LIKE LOWER(:search) ' +
        'ORDER BY name';
      Query.Params.ParamByName('search').AsString := '%' + SearchTerm + '%';
    end
    else
    begin
      Query.SQL.Text :=
        'SELECT id, code, name, address, phone, email ' +
        'FROM customers ' +
        'ORDER BY name';
    end;

    Query.Open;

    // Remplir la grille
    gridCustomers.RowCount := Query.RecordCount + 1;
    Row := 1;

    while not Query.EOF do
    begin
      gridCustomers.Cells[0, Row] := Query.FieldByName('id').AsString;
      gridCustomers.Cells[1, Row] := Query.FieldByName('code').AsString;
      gridCustomers.Cells[2, Row] := Query.FieldByName('name').AsString;
      gridCustomers.Cells[3, Row] := Query.FieldByName('address').AsString;
      gridCustomers.Cells[4, Row] := Query.FieldByName('phone').AsString;
      gridCustomers.Cells[5, Row] := Query.FieldByName('email').AsString;

      Inc(Row);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TfrmCustomers.btnSearchClick(Sender: TObject);
begin
  LoadCustomers(edtSearch.Text);
end;

procedure TfrmCustomers.btnNewClick(Sender: TObject);
var
  EditForm: TfrmCustomerEdit;
begin
  EditForm := TfrmCustomerEdit.Create(Self);
  try
    if EditForm.ShowModal = mrOk then
      LoadCustomers;
  finally
    EditForm.Free;
  end;
end;

procedure TfrmCustomers.btnEditClick(Sender: TObject);
var
  EditForm: TfrmCustomerEdit;
  CustomerId: Integer;
begin
  CustomerId := GetSelectedCustomerId;
  if CustomerId = 0 then
  begin
    ShowMessage('Veuillez sélectionner un client');
    Exit;
  end;

  EditForm := TfrmCustomerEdit.Create(Self);
  try
    EditForm.LoadCustomer(CustomerId);
    if EditForm.ShowModal = mrOk then
      LoadCustomers;
  finally
    EditForm.Free;
  end;
end;

procedure TfrmCustomers.btnDeleteClick(Sender: TObject);
var
  CustomerId: Integer;
  Customer: TCustomer;
begin
  CustomerId := GetSelectedCustomerId;
  if CustomerId = 0 then
  begin
    ShowMessage('Veuillez sélectionner un client');
    Exit;
  end;

  if MessageDlg('Confirmation',
                'Voulez-vous vraiment supprimer ce client ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Customer := TCustomer.LoadById(dmData.Connection, CustomerId);
    try
      if Assigned(Customer) then
      begin
        Customer.Delete(dmData.Connection);
        LoadCustomers;
        ShowMessage('Client supprimé avec succès');
      end;
    finally
      Customer.Free;
    end;
  end;
end;

procedure TfrmCustomers.gridCustomersDblClick(Sender: TObject);
begin
  btnEditClick(Sender);
end;

function TfrmCustomers.GetSelectedCustomerId: Integer;
begin
  Result := 0;
  if (gridCustomers.Row > 0) and (gridCustomers.RowCount > 1) then
    Result := StrToIntDef(gridCustomers.Cells[0, gridCustomers.Row], 0);
end;

end.
```

## Génération de rapports

### Classe de base pour les rapports

```pascal
unit ReportBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Printers;

type
  TReportFormat = (rfPDF, rfHTML, rfExcel, rfPrint);

  TBaseReport = class
  protected
    FTitle: string;
    FCompanyName: string;
    FReportDate: TDateTime;

    procedure DrawHeader(Canvas: TCanvas; Width: Integer); virtual;
    procedure DrawFooter(Canvas: TCanvas; Width, Y: Integer); virtual;
    procedure DrawContent(Canvas: TCanvas; Width, StartY: Integer); virtual; abstract;
  public
    constructor Create;

    procedure Generate(Format: TReportFormat; const FileName: string = '');
    procedure Print;

    property Title: string read FTitle write FTitle;
    property CompanyName: string read FCompanyName write FCompanyName;
  end;

implementation

constructor TBaseReport.Create;
begin
  inherited Create;
  FReportDate := Now;
  FCompanyName := 'Ma Société';
end;

procedure TBaseReport.DrawHeader(Canvas: TCanvas; Width: Integer);
begin
  Canvas.Font.Size := 16;
  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(50, 50, FCompanyName);

  Canvas.Font.Size := 12;
  Canvas.Font.Style := [];
  Canvas.TextOut(50, 80, FTitle);

  Canvas.Font.Size := 10;
  Canvas.TextOut(Width - 200, 80,
    'Date: ' + FormatDateTime('dd/mm/yyyy', FReportDate));

  // Ligne de séparation
  Canvas.Pen.Width := 2;
  Canvas.Line(50, 110, Width - 50, 110);
end;

procedure TBaseReport.DrawFooter(Canvas: TCanvas; Width, Y: Integer);
begin
  Canvas.Font.Size := 8;
  Canvas.TextOut(50, Y,
    'Généré le ' + FormatDateTime('dd/mm/yyyy à hh:nn', Now));
  Canvas.TextOut(Width - 150, Y, 'Page 1');
end;

procedure TBaseReport.Generate(Format: TReportFormat; const FileName: string);
begin
  case Format of
    rfPrint: Print;
    rfPDF: ; // À implémenter dans les sous-classes
    rfHTML: ; // À implémenter dans les sous-classes
    rfExcel: ; // À implémenter dans les sous-classes
  end;
end;

procedure TBaseReport.Print;
var
  PrinterCanvas: TCanvas;
begin
  if not Printer.Printing then
  begin
    Printer.BeginDoc;
    try
      PrinterCanvas := Printer.Canvas;

      DrawHeader(PrinterCanvas, Printer.PageWidth);
      DrawContent(PrinterCanvas, Printer.PageWidth, 150);
      DrawFooter(PrinterCanvas, Printer.PageWidth, Printer.PageHeight - 100);

    finally
      Printer.EndDoc;
    end;
  end;
end;

end.
```

### Rapport de liste de clients

```pascal
unit CustomerListReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ReportBase, CustomerManager;

type
  TCustomerListReport = class(TBaseReport)
  private
    FCustomers: TList;
  protected
    procedure DrawContent(Canvas: TCanvas; Width, StartY: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadCustomers(Connection: TSQLConnection);
  end;

implementation

constructor TCustomerListReport.Create;
begin
  inherited Create;
  FCustomers := TList.Create;
  FTitle := 'Liste des Clients';
end;

destructor TCustomerListReport.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCustomers.Count - 1 do
    TCustomer(FCustomers[i]).Free;
  FCustomers.Free;
  inherited Destroy;
end;

procedure TCustomerListReport.LoadCustomers(Connection: TSQLConnection);
var
  Query: TSQLQuery;
  Customer: TCustomer;
  i: Integer;
begin
  // Vider la liste actuelle
  for i := 0 to FCustomers.Count - 1 do
    TCustomer(FCustomers[i]).Free;
  FCustomers.Clear;

  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text := 'SELECT * FROM customers ORDER BY name';
    Query.Open;

    while not Query.EOF do
    begin
      Customer := TCustomer.Create;
      Customer.Id := Query.FieldByName('id').AsInteger;
      Customer.Code := Query.FieldByName('code').AsString;
      Customer.Name := Query.FieldByName('name').AsString;
      Customer.Address := Query.FieldByName('address').AsString;
      Customer.Phone := Query.FieldByName('phone').AsString;
      Customer.Email := Query.FieldByName('email').AsString;

      FCustomers.Add(Customer);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TCustomerListReport.DrawContent(Canvas: TCanvas; Width, StartY: Integer);
var
  i, Y: Integer;
  Customer: TCustomer;
const
  LineHeight = 25;
begin
  Canvas.Font.Size := 10;
  Canvas.Font.Style := [fsBold];

  Y := StartY;

  // En-têtes de colonnes
  Canvas.TextOut(50, Y, 'Code');
  Canvas.TextOut(150, Y, 'Nom');
  Canvas.TextOut(350, Y, 'Téléphone');
  Canvas.TextOut(500, Y, 'Email');

  Canvas.Font.Style := [];
  Y := Y + LineHeight;

  // Ligne de séparation
  Canvas.Line(50, Y, Width - 50, Y);
  Y := Y + 10;

  // Données
  for i := 0 to FCustomers.Count - 1 do
  begin
    Customer := TCustomer(FCustomers[i]);

    Canvas.TextOut(50, Y, Customer.Code);
    Canvas.TextOut(150, Y, Customer.Name);
    Canvas.TextOut(350, Y, Customer.Phone);
    Canvas.TextOut(500, Y, Customer.Email);

    Y := Y + LineHeight;

    // Gérer le saut de page si nécessaire
    if Y > Canvas.Height - 150 then
    begin
      // Nouvelle page (à implémenter selon le contexte)
      Break;
    end;
  end;
end;

end.
```

## Logging et audit

### Système de logging multi-plateforme

```pascal
unit Logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);

  TLogger = class
  private
    FLogFile: TextFile;
    FLogPath: string;
    FLock: TCriticalSection;
    FMinLevel: TLogLevel;

    function GetLogFileName: string;
    function LogLevelToString(Level: TLogLevel): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Log(Level: TLogLevel; const Message: string);
    procedure Debug(const Message: string);
    procedure Info(const Message: string);
    procedure Warning(const Message: string);
    procedure Error(const Message: string);
    procedure Fatal(const Message: string);

    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
  end;

function GetLogger: TLogger;

implementation

var
  GlobalLogger: TLogger = nil;

function GetLogger: TLogger;
begin
  if GlobalLogger = nil then
    GlobalLogger := TLogger.Create;
  Result := GlobalLogger;
end;

constructor TLogger.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FMinLevel := llInfo;

  {$IFDEF WINDOWS}
  FLogPath := GetEnvironmentVariable('APPDATA') + PathDelim +
              'MonERP' + PathDelim + 'logs' + PathDelim;
  {$ENDIF}
  {$IFDEF UNIX}
  FLogPath := GetEnvironmentVariable('HOME') + PathDelim +
              '.monerp' + PathDelim + 'logs' + PathDelim;
  {$ENDIF}

  ForceDirectories(FLogPath);
end;

destructor TLogger.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

function TLogger.GetLogFileName: string;
begin
  Result := FLogPath + 'monerp_' +
            FormatDateTime('yyyymmdd', Date) + '.log';
end;

function TLogger.LogLevelToString(Level: TLogLevel): string;
begin
  case Level of
    llDebug: Result := 'DEBUG';
    llInfo: Result := 'INFO';
    llWarning: Result := 'WARNING';
    llError: Result := 'ERROR';
    llFatal: Result := 'FATAL';
  end;
end;

procedure TLogger.Log(Level: TLogLevel; const Message: string);
var
  LogLine: string;
begin
  if Level < FMinLevel then
    Exit;

  FLock.Enter;
  try
    AssignFile(FLogFile, GetLogFileName);
    if FileExists(GetLogFileName) then
      Append(FLogFile)
    else
      Rewrite(FLogFile);

    try
      LogLine := Format('[%s] [%s] %s', [
        FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
        LogLevelToString(Level),
        Message
      ]);

      WriteLn(FLogFile, LogLine);
      Flush(FLogFile);
    finally
      CloseFile(FLogFile);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TLogger.Debug(const Message: string);
begin
  Log(llDebug, Message);
end;

procedure TLogger.Info(const Message: string);
begin
  Log(llInfo, Message);
end;

procedure TLogger.Warning(const Message: string);
begin
  Log(llWarning, Message);
end;

procedure TLogger.Error(const Message: string);
begin
  Log(llError, Message);
end;

procedure TLogger.Fatal(const Message: string);
begin
  Log(llFatal, Message);
end;

initialization

finalization
  if Assigned(GlobalLogger) then
    GlobalLogger.Free;

end.
```

**Utilisation du logger :**

```pascal
uses
  Logger;

begin
  GetLogger.Info('Application démarrée');

  try
    // Code de l'application
    GetLogger.Debug('Connexion à la base de données...');
    ConnectDatabase;
    GetLogger.Info('Base de données connectée avec succès');
  except
    on E: Exception do
    begin
      GetLogger.Error('Erreur: ' + E.Message);
      raise;
    end;
  end;
end;
```

### Audit des modifications

```pascal
unit AuditLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db;

type
  TAuditAction = (aaInsert, aaUpdate, aaDelete, aaView);

  TAuditLog = class
  public
    class procedure LogAction(
      Connection: TSQLConnection;
      UserId: Integer;
      TableName: string;
      RecordId: Integer;
      Action: TAuditAction;
      const OldValue: string = '';
      const NewValue: string = ''
    );

    class function GetAuditTrail(
      Connection: TSQLConnection;
      TableName: string;
      RecordId: Integer
    ): TStringList;
  end;

implementation

uses
  Logger;

class procedure TAuditLog.LogAction(
  Connection: TSQLConnection;
  UserId: Integer;
  TableName: string;
  RecordId: Integer;
  Action: TAuditAction;
  const OldValue: string;
  const NewValue: string
);
var
  Query: TSQLQuery;
  ActionStr: string;
begin
  case Action of
    aaInsert: ActionStr := 'INSERT';
    aaUpdate: ActionStr := 'UPDATE';
    aaDelete: ActionStr := 'DELETE';
    aaView: ActionStr := 'VIEW';
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text :=
      'INSERT INTO audit_log (user_id, table_name, record_id, action, ' +
      'old_value, new_value, action_date) ' +
      'VALUES (:user_id, :table_name, :record_id, :action, ' +
      ':old_value, :new_value, CURRENT_TIMESTAMP)';

    Query.Params.ParamByName('user_id').AsInteger := UserId;
    Query.Params.ParamByName('table_name').AsString := TableName;
    Query.Params.ParamByName('record_id').AsInteger := RecordId;
    Query.Params.ParamByName('action').AsString := ActionStr;
    Query.Params.ParamByName('old_value').AsString := OldValue;
    Query.Params.ParamByName('new_value').AsString := NewValue;

    Query.ExecSQL;
    Connection.Transaction.Commit;

    GetLogger.Info(Format('Audit: %s sur %s (ID: %d) par utilisateur %d',
      [ActionStr, TableName, RecordId, UserId]));
  except
    on E: Exception do
    begin
      GetLogger.Error('Erreur lors de l''audit: ' + E.Message);
      Connection.Transaction.Rollback;
    end;
  end;
  Query.Free;
end;

class function TAuditLog.GetAuditTrail(
  Connection: TSQLConnection;
  TableName: string;
  RecordId: Integer
): TStringList;
var
  Query: TSQLQuery;
begin
  Result := TStringList.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text :=
      'SELECT al.action_date, al.action, u.username, al.old_value, al.new_value ' +
      'FROM audit_log al ' +
      'INNER JOIN users u ON al.user_id = u.id ' +
      'WHERE al.table_name = :table_name AND al.record_id = :record_id ' +
      'ORDER BY al.action_date DESC';

    Query.Params.ParamByName('table_name').AsString := TableName;
    Query.Params.ParamByName('record_id').AsInteger := RecordId;
    Query.Open;

    while not Query.EOF do
    begin
      Result.Add(Format('%s - %s par %s', [
        FormatDateTime('dd/mm/yyyy hh:nn:ss', Query.FieldByName('action_date').AsDateTime),
        Query.FieldByName('action').AsString,
        Query.FieldByName('username').AsString
      ]));
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
```

**Structure de table pour l'audit :**

```sql
CREATE TABLE audit_log (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(id),
    table_name VARCHAR(100) NOT NULL,
    record_id INTEGER NOT NULL,
    action VARCHAR(20) NOT NULL,
    old_value TEXT,
    new_value TEXT,
    action_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    ip_address VARCHAR(45)
);

CREATE INDEX idx_audit_table_record ON audit_log(table_name, record_id);
CREATE INDEX idx_audit_date ON audit_log(action_date);
CREATE INDEX idx_audit_user ON audit_log(user_id);
```

## Gestion des droits d'accès

### Système de permissions

```pascal
unit Permissions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb;

type
  TPermission = (
    // Gestion commerciale
    pmViewCustomers,
    pmCreateCustomers,
    pmEditCustomers,
    pmDeleteCustomers,
    pmViewInvoices,
    pmCreateInvoices,
    pmEditInvoices,
    pmDeleteInvoices,
    pmValidateInvoices,

    // Comptabilité
    pmViewAccounting,
    pmCreateEntries,
    pmEditEntries,
    pmCloseAccounting,

    // Stocks
    pmViewInventory,
    pmEditInventory,
    pmStockMovements,

    // Administration
    pmViewUsers,
    pmCreateUsers,
    pmEditUsers,
    pmDeleteUsers,
    pmManagePermissions,

    // Rapports
    pmViewReports,
    pmExportData
  );

  TPermissionSet = set of TPermission;

  TRole = class
  private
    FId: Integer;
    FName: string;
    FPermissions: TPermissionSet;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Permissions: TPermissionSet read FPermissions write FPermissions;

    function HasPermission(Permission: TPermission): Boolean;
    function Save(Connection: TSQLConnection): Boolean;
    class function LoadById(Connection: TSQLConnection; AId: Integer): TRole;
  end;

  TUserPermissions = class
  private
    FUserId: Integer;
    FRoles: TList;
  public
    constructor Create(AUserId: Integer);
    destructor Destroy; override;

    function HasPermission(Permission: TPermission): Boolean;
    procedure LoadRoles(Connection: TSQLConnection);

    property UserId: Integer read FUserId;
  end;

implementation

{ TRole }

function TRole.HasPermission(Permission: TPermission): Boolean;
begin
  Result := Permission in FPermissions;
end;

function TRole.Save(Connection: TSQLConnection): Boolean;
var
  Query: TSQLQuery;
  PermStr: string;
  P: TPermission;
begin
  Result := False;

  // Convertir les permissions en chaîne
  PermStr := '';
  for P := Low(TPermission) to High(TPermission) do
  begin
    if P in FPermissions then
    begin
      if PermStr <> '' then
        PermStr := PermStr + ',';
      PermStr := PermStr + IntToStr(Ord(P));
    end;
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;

    if FId = 0 then
    begin
      Query.SQL.Text :=
        'INSERT INTO roles (name, permissions) VALUES (:name, :perms) RETURNING id';
    end
    else
    begin
      Query.SQL.Text :=
        'UPDATE roles SET name = :name, permissions = :perms WHERE id = :id';
      Query.Params.ParamByName('id').AsInteger := FId;
    end;

    Query.Params.ParamByName('name').AsString := FName;
    Query.Params.ParamByName('perms').AsString := PermStr;

    Query.ExecSQL;

    if FId = 0 then
    begin
      Query.Open;
      FId := Query.Fields[0].AsInteger;
    end;

    Connection.Transaction.Commit;
    Result := True;
  except
    Connection.Transaction.Rollback;
    raise;
  end;
  Query.Free;
end;

class function TRole.LoadById(Connection: TSQLConnection; AId: Integer): TRole;
var
  Query: TSQLQuery;
  PermStr: string;
  PermList: TStringList;
  i: Integer;
begin
  Result := nil;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text := 'SELECT * FROM roles WHERE id = :id';
    Query.Params.ParamByName('id').AsInteger := AId;
    Query.Open;

    if not Query.EOF then
    begin
      Result := TRole.Create;
      Result.FId := Query.FieldByName('id').AsInteger;
      Result.FName := Query.FieldByName('name').AsString;

      // Charger les permissions
      PermStr := Query.FieldByName('permissions').AsString;
      PermList := TStringList.Create;
      try
        PermList.Delimiter := ',';
        PermList.DelimitedText := PermStr;

        Result.FPermissions := [];
        for i := 0 to PermList.Count - 1 do
          Include(Result.FPermissions, TPermission(StrToInt(PermList[i])));
      finally
        PermList.Free;
      end;
    end;
  finally
    Query.Free;
  end;
end;

{ TUserPermissions }

constructor TUserPermissions.Create(AUserId: Integer);
begin
  inherited Create;
  FUserId := AUserId;
  FRoles := TList.Create;
end;

destructor TUserPermissions.Destroy;
var
  i: Integer;
begin
  for i := 0 to FRoles.Count - 1 do
    TRole(FRoles[i]).Free;
  FRoles.Free;
  inherited Destroy;
end;

procedure TUserPermissions.LoadRoles(Connection: TSQLConnection);
var
  Query: TSQLQuery;
  Role: TRole;
  i: Integer;
begin
  // Vider les rôles actuels
  for i := 0 to FRoles.Count - 1 do
    TRole(FRoles[i]).Free;
  FRoles.Clear;

  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text :=
      'SELECT r.* FROM roles r ' +
      'INNER JOIN user_roles ur ON r.id = ur.role_id ' +
      'WHERE ur.user_id = :user_id';
    Query.Params.ParamByName('user_id').AsInteger := FUserId;
    Query.Open;

    while not Query.EOF do
    begin
      Role := TRole.LoadById(Connection, Query.FieldByName('id').AsInteger);
      if Assigned(Role) then
        FRoles.Add(Role);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TUserPermissions.HasPermission(Permission: TPermission): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to FRoles.Count - 1 do
  begin
    if TRole(FRoles[i]).HasPermission(Permission) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

end.
```

**Structure de tables pour les permissions :**

```sql
CREATE TABLE roles (
    id SERIAL PRIMARY KEY,
    name VARCHAR(50) UNIQUE NOT NULL,
    permissions TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE user_roles (
    user_id INTEGER REFERENCES users(id) ON DELETE CASCADE,
    role_id INTEGER REFERENCES roles(id) ON DELETE CASCADE,
    assigned_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (user_id, role_id)
);

-- Rôles par défaut
INSERT INTO roles (name, permissions) VALUES
    ('Administrateur', '0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23'),
    ('Commercial', '0,1,2,4,5,6,13,14,21'),
    ('Comptable', '9,10,11,12,21'),
    ('Magasinier', '13,14,15');
```

**Utilisation des permissions dans les formulaires :**

```pascal
procedure TfrmCustomers.FormCreate(Sender: TObject);
var
  Perms: TUserPermissions;
begin
  Perms := TUserPermissions.Create(CurrentUser.Id);
  try
    Perms.LoadRoles(dmData.Connection);

    // Activer/désactiver les boutons selon les permissions
    btnNew.Enabled := Perms.HasPermission(pmCreateCustomers);
    btnEdit.Enabled := Perms.HasPermission(pmEditCustomers);
    btnDelete.Enabled := Perms.HasPermission(pmDeleteCustomers);

    if not Perms.HasPermission(pmViewCustomers) then
    begin
      ShowMessage('Vous n''avez pas la permission de voir les clients');
      Close;
    end;
  finally
    Perms.Free;
  end;
end;
```

## Import/Export de données

### Export vers Excel

```pascal
unit DataExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpspreadsheet, xlsbiff8;

type
  TDataExporter = class
  public
    class function ExportToExcel(
      Query: TSQLQuery;
      const FileName: string;
      const SheetName: string = 'Data'
    ): Boolean;

    class function ExportToCSV(
      Query: TSQLQuery;
      const FileName: string;
      const Delimiter: Char = ';'
    ): Boolean;
  end;

implementation

class function TDataExporter.ExportToExcel(
  Query: TSQLQuery;
  const FileName: string;
  const SheetName: string
): Boolean;
var
  Workbook: TsWorkbook;
  Worksheet: TsWorksheet;
  Row, Col: Integer;
  Field: TField;
begin
  Result := False;

  try
    Workbook := TsWorkbook.Create;
    try
      Worksheet := Workbook.AddWorksheet(SheetName);

      // En-têtes de colonnes
      Col := 0;
      for Field in Query.Fields do
      begin
        Worksheet.WriteText(0, Col, Field.FieldName);
        // Mettre l'en-tête en gras
        Worksheet.WriteFont(0, Col, 'Arial', 10, [fssBold], scBlack);
        Inc(Col);
      end;

      // Données
      Row := 1;
      Query.First;
      while not Query.EOF do
      begin
        Col := 0;
        for Field in Query.Fields do
        begin
          case Field.DataType of
            ftInteger, ftSmallint, ftLargeint:
              Worksheet.WriteNumber(Row, Col, Field.AsInteger);
            ftFloat, ftCurrency:
              Worksheet.WriteNumber(Row, Col, Field.AsFloat);
            ftDate, ftDateTime:
              Worksheet.WriteDateTime(Row, Col, Field.AsDateTime);
            ftBoolean:
              Worksheet.WriteBoolValue(Row, Col, Field.AsBoolean);
          else
            Worksheet.WriteText(Row, Col, Field.AsString);
          end;
          Inc(Col);
        end;
        Inc(Row);
        Query.Next;
      end;

      // Ajuster la largeur des colonnes
      for Col := 0 to Query.FieldCount - 1 do
        Worksheet.WriteColWidth(Col, 15, suChars);

      // Sauvegarder
      Workbook.WriteToFile(FileName, sfExcel8, True);
      Result := True;
    finally
      Workbook.Free;
    end;
  except
    on E: Exception do
      raise Exception.Create('Erreur lors de l''export Excel: ' + E.Message);
  end;
end;

class function TDataExporter.ExportToCSV(
  Query: TSQLQuery;
  const FileName: string;
  const Delimiter: Char
): Boolean;
var
  Output: TextFile;
  Line: string;
  Field: TField;
  First: Boolean;
begin
  Result := False;

  try
    AssignFile(Output, FileName);
    Rewrite(Output);
    try
      // En-têtes
      First := True;
      Line := '';
      for Field in Query.Fields do
      begin
        if not First then
          Line := Line + Delimiter;
        Line := Line + Field.FieldName;
        First := False;
      end;
      WriteLn(Output, Line);

      // Données
      Query.First;
      while not Query.EOF do
      begin
        First := True;
        Line := '';
        for Field in Query.Fields do
        begin
          if not First then
            Line := Line + Delimiter;

          // Échapper les guillemets et ajouter des guillemets si nécessaire
          if Pos(Delimiter, Field.AsString) > 0 then
            Line := Line + '"' + StringReplace(Field.AsString, '"', '""', [rfReplaceAll]) + '"'
          else
            Line := Line + Field.AsString;

          First := False;
        end;
        WriteLn(Output, Line);
        Query.Next;
      end;

      Result := True;
    finally
      CloseFile(Output);
    end;
  except
    on E: Exception do
      raise Exception.Create('Erreur lors de l''export CSV: ' + E.Message);
  end;
end;

end.
```

### Import depuis Excel/CSV

```pascal
unit DataImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpspreadsheet, csvdocument;

type
  TImportResult = record
    Success: Boolean;
    RowsProcessed: Integer;
    RowsImported: Integer;
    Errors: TStringList;
  end;

  TDataImporter = class
  public
    class function ImportFromExcel(
      const FileName: string;
      Connection: TSQLConnection;
      const TableName: string;
      const ColumnMapping: array of string
    ): TImportResult;

    class function ImportFromCSV(
      const FileName: string;
      Connection: TSQLConnection;
      const TableName: string;
      const ColumnMapping: array of string;
      const Delimiter: Char = ';'
    ): TImportResult;
  end;

implementation

class function TDataImporter.ImportFromCSV(
  const FileName: string;
  Connection: TSQLConnection;
  const TableName: string;
  const ColumnMapping: array of string;
  const Delimiter: Char
): TImportResult;
var
  CSV: TCSVDocument;
  Query: TSQLQuery;
  Row, Col: Integer;
  SQL, Values: string;
  FieldName: string;
begin
  Result.Success := False;
  Result.RowsProcessed := 0;
  Result.RowsImported := 0;
  Result.Errors := TStringList.Create;

  CSV := TCSVDocument.Create;
  Query := TSQLQuery.Create(nil);
  try
    CSV.Delimiter := Delimiter;
    CSV.LoadFromFile(FileName);

    Query.DataBase := Connection;

    // Préparer la requête d'insertion
    SQL := 'INSERT INTO ' + TableName + ' (';
    for Col := 0 to High(ColumnMapping) do
    begin
      if Col > 0 then
        SQL := SQL + ', ';
      SQL := SQL + ColumnMapping[Col];
    end;
    SQL := SQL + ') VALUES (';

    // Commencer la transaction
    Connection.Transaction.StartTransaction;

    try
      // Parcourir les lignes (en commençant à 1 pour sauter l'en-tête)
      for Row := 1 to CSV.RowCount - 1 do
      begin
        Inc(Result.RowsProcessed);

        Values := SQL;
        for Col := 0 to High(ColumnMapping) do
        begin
          if Col > 0 then
            Values := Values + ', ';
          Values := Values + QuotedStr(CSV.Cells[Col, Row]);
        end;
        Values := Values + ')';

        try
          Query.SQL.Text := Values;
          Query.ExecSQL;
          Inc(Result.RowsImported);
        except
          on E: Exception do
            Result.Errors.Add(Format('Ligne %d: %s', [Row + 1, E.Message]));
        end;
      end;

      Connection.Transaction.Commit;
      Result.Success := (Result.Errors.Count = 0);
    except
      on E: Exception do
      begin
        Connection.Transaction.Rollback;
        Result.Errors.Add('Erreur globale: ' + E.Message);
      end;
    end;
  finally
    Query.Free;
    CSV.Free;
  end;
end;

function TBackupManager.RestoreBackup(const BackupFile: string): Boolean;
var
  PgRestorePath: string;
begin
  if not FileExists(BackupFile) then
  begin
    GetLogger.Error('Fichier de sauvegarde introuvable: ' + BackupFile);
    Exit(False);
  end;

  {$IFDEF WINDOWS}
  PgRestorePath := 'pg_restore.exe';
  {$ENDIF}
  {$IFDEF UNIX}
  PgRestorePath := 'pg_restore';
  {$ENDIF}

  GetLogger.Info('Démarrage de la restauration: ' + BackupFile);

  Result := ExecuteCommand(PgRestorePath, [
    '-h', 'localhost',
    '-U', FDatabaseUser,
    '-d', FDatabaseName,
    '-c',  // Clean (drop) database objects before recreating
    BackupFile
  ]);

  if Result then
    GetLogger.Info('Restauration réussie')
  else
    GetLogger.Error('Échec de la restauration');
end;

procedure TBackupManager.CleanOldBackups(DaysToKeep: Integer);
var
  SearchRec: TSearchRec;
  FileAge: TDateTime;
  CutoffDate: TDateTime;
  FilePath: string;
begin
  CutoffDate := Now - DaysToKeep;

  if FindFirst(FBackupPath + '*.backup', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      FilePath := FBackupPath + SearchRec.Name;
      FileAge := FileDateToDateTime(SysUtils.FileAge(FilePath));

      if FileAge < CutoffDate then
      begin
        GetLogger.Info('Suppression de l''ancienne sauvegarde: ' + SearchRec.Name);
        DeleteFile(FilePath);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

end.
```

### Planification automatique des sauvegardes

```pascal
unit ScheduledBackup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Backup;

type
  TScheduledBackup = class(TComponent)
  private
    FTimer: TTimer;
    FBackupManager: TBackupManager;
    FBackupHour: Integer;
    FBackupMinute: Integer;
    FLastBackup: TDateTime;
    FEnabled: Boolean;

    procedure OnTimerTick(Sender: TObject);
    procedure SetEnabled(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartScheduler;
    procedure StopScheduler;
    procedure ExecuteBackupNow;

    property BackupHour: Integer read FBackupHour write FBackupHour;
    property BackupMinute: Integer read FBackupMinute write FBackupMinute;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property LastBackup: TDateTime read FLastBackup;
  end;

implementation

uses
  Logger;

constructor TScheduledBackup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 60000; // Vérifier chaque minute
  FTimer.OnTimer := @OnTimerTick;
  FTimer.Enabled := False;

  FBackupManager := TBackupManager.Create('monerp', 'erp_user');

  // Sauvegarde par défaut à 2h du matin
  FBackupHour := 2;
  FBackupMinute := 0;
  FLastBackup := 0;
  FEnabled := False;
end;

destructor TScheduledBackup.Destroy;
begin
  FTimer.Free;
  FBackupManager.Free;
  inherited Destroy;
end;

procedure TScheduledBackup.OnTimerTick(Sender: TObject);
var
  CurrentHour, CurrentMinute, CurrentSecond, CurrentMS: Word;
begin
  if not FEnabled then
    Exit;

  DecodeTime(Now, CurrentHour, CurrentMinute, CurrentSecond, CurrentMS);

  // Vérifier si c'est l'heure de la sauvegarde
  if (CurrentHour = FBackupHour) and (CurrentMinute = FBackupMinute) then
  begin
    // Ne faire qu'une sauvegarde par jour
    if Trunc(FLastBackup) < Trunc(Now) then
    begin
      GetLogger.Info('Exécution de la sauvegarde planifiée');
      ExecuteBackupNow;
    end;
  end;
end;

procedure TScheduledBackup.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;

  FEnabled := AValue;
  FTimer.Enabled := AValue;

  if AValue then
    GetLogger.Info('Planificateur de sauvegarde activé')
  else
    GetLogger.Info('Planificateur de sauvegarde désactivé');
end;

procedure TScheduledBackup.StartScheduler;
begin
  Enabled := True;
end;

procedure TScheduledBackup.StopScheduler;
begin
  Enabled := False;
end;

procedure TScheduledBackup.ExecuteBackupNow;
begin
  GetLogger.Info('Début de la sauvegarde...');

  if FBackupManager.CreateBackup then
  begin
    FLastBackup := Now;
    GetLogger.Info('Sauvegarde terminée avec succès');

    // Nettoyer les anciennes sauvegardes (garder 30 jours)
    FBackupManager.CleanOldBackups(30);
  end
  else
    GetLogger.Error('La sauvegarde a échoué');
end;

end.
```

## Gestion des mises à jour

### Système de versioning de la base de données

```pascal
unit DatabaseVersion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb;

type
  TMigration = class
  private
    FVersion: Integer;
    FDescription: string;
    FUpScript: TStringList;
    FDownScript: TStringList;
  public
    constructor Create(AVersion: Integer; const ADescription: string);
    destructor Destroy; override;

    procedure AddUpStatement(const SQL: string);
    procedure AddDownStatement(const SQL: string);

    function Execute(Connection: TSQLConnection; Up: Boolean = True): Boolean;

    property Version: Integer read FVersion;
    property Description: string read FDescription;
  end;

  TDatabaseVersionManager = class
  private
    FMigrations: TList;

    procedure EnsureVersionTable(Connection: TSQLConnection);
    function GetCurrentVersion(Connection: TSQLConnection): Integer;
    procedure SetVersion(Connection: TSQLConnection; Version: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterMigration(Migration: TMigration);
    function Migrate(Connection: TSQLConnection; TargetVersion: Integer = -1): Boolean;
    function Rollback(Connection: TSQLConnection; TargetVersion: Integer): Boolean;
    function GetDatabaseVersion(Connection: TSQLConnection): Integer;
  end;

implementation

uses
  Logger;

{ TMigration }

constructor TMigration.Create(AVersion: Integer; const ADescription: string);
begin
  inherited Create;
  FVersion := AVersion;
  FDescription := ADescription;
  FUpScript := TStringList.Create;
  FDownScript := TStringList.Create;
end;

destructor TMigration.Destroy;
begin
  FUpScript.Free;
  FDownScript.Free;
  inherited Destroy;
end;

procedure TMigration.AddUpStatement(const SQL: string);
begin
  FUpScript.Add(SQL);
end;

procedure TMigration.AddDownStatement(const SQL: string);
begin
  FDownScript.Add(SQL);
end;

function TMigration.Execute(Connection: TSQLConnection; Up: Boolean): Boolean;
var
  Query: TSQLQuery;
  Scripts: TStringList;
  i: Integer;
begin
  Result := False;

  if Up then
  begin
    Scripts := FUpScript;
    GetLogger.Info(Format('Application de la migration %d: %s', [FVersion, FDescription]));
  end
  else
  begin
    Scripts := FDownScript;
    GetLogger.Info(Format('Annulation de la migration %d: %s', [FVersion, FDescription]));
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Connection.Transaction.StartTransaction;

    try
      for i := 0 to Scripts.Count - 1 do
      begin
        if Trim(Scripts[i]) <> '' then
        begin
          Query.SQL.Text := Scripts[i];
          Query.ExecSQL;
        end;
      end;

      Connection.Transaction.Commit;
      Result := True;
      GetLogger.Info('Migration appliquée avec succès');
    except
      on E: Exception do
      begin
        Connection.Transaction.Rollback;
        GetLogger.Error('Erreur lors de la migration: ' + E.Message);
      end;
    end;
  finally
    Query.Free;
  end;
end;

{ TDatabaseVersionManager }

constructor TDatabaseVersionManager.Create;
begin
  inherited Create;
  FMigrations := TList.Create;
end;

destructor TDatabaseVersionManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FMigrations.Count - 1 do
    TMigration(FMigrations[i]).Free;
  FMigrations.Free;
  inherited Destroy;
end;

procedure TDatabaseVersionManager.EnsureVersionTable(Connection: TSQLConnection);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS schema_version (' +
      '  version INTEGER PRIMARY KEY,' +
      '  applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,' +
      '  description VARCHAR(200)' +
      ')';
    Query.ExecSQL;
    Connection.Transaction.Commit;
  except
    Connection.Transaction.Rollback;
    raise;
  end;
  Query.Free;
end;

function TDatabaseVersionManager.GetCurrentVersion(Connection: TSQLConnection): Integer;
var
  Query: TSQLQuery;
begin
  Result := 0;

  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text := 'SELECT MAX(version) as version FROM schema_version';
    Query.Open;

    if not Query.EOF then
      Result := Query.FieldByName('version').AsInteger;
  finally
    Query.Free;
  end;
end;

procedure TDatabaseVersionManager.SetVersion(Connection: TSQLConnection; Version: Integer);
var
  Query: TSQLQuery;
  Migration: TMigration;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text :=
      'INSERT INTO schema_version (version, description) VALUES (:version, :desc)';

    // Trouver la description de la migration
    Migration := TMigration(FMigrations[Version - 1]);
    Query.Params.ParamByName('version').AsInteger := Version;
    Query.Params.ParamByName('desc').AsString := Migration.Description;

    Query.ExecSQL;
    Connection.Transaction.Commit;
  except
    Connection.Transaction.Rollback;
    raise;
  end;
  Query.Free;
end;

procedure TDatabaseVersionManager.RegisterMigration(Migration: TMigration);
begin
  FMigrations.Add(Migration);
end;

function TDatabaseVersionManager.Migrate(Connection: TSQLConnection;
  TargetVersion: Integer): Boolean;
var
  CurrentVersion: Integer;
  i: Integer;
  Migration: TMigration;
begin
  Result := False;

  EnsureVersionTable(Connection);
  CurrentVersion := GetCurrentVersion(Connection);

  if TargetVersion = -1 then
    TargetVersion := FMigrations.Count;

  GetLogger.Info(Format('Version actuelle: %d, Version cible: %d',
    [CurrentVersion, TargetVersion]));

  if CurrentVersion >= TargetVersion then
  begin
    GetLogger.Info('Base de données déjà à jour');
    Exit(True);
  end;

  // Appliquer les migrations
  for i := CurrentVersion to TargetVersion - 1 do
  begin
    Migration := TMigration(FMigrations[i]);

    if not Migration.Execute(Connection, True) then
    begin
      GetLogger.Error('Échec de la migration, arrêt du processus');
      Exit(False);
    end;

    SetVersion(Connection, Migration.Version);
  end;

  Result := True;
  GetLogger.Info('Toutes les migrations ont été appliquées avec succès');
end;

function TDatabaseVersionManager.Rollback(Connection: TSQLConnection;
  TargetVersion: Integer): Boolean;
var
  CurrentVersion: Integer;
  i: Integer;
  Migration: TMigration;
  Query: TSQLQuery;
begin
  Result := False;

  CurrentVersion := GetCurrentVersion(Connection);

  if CurrentVersion <= TargetVersion then
  begin
    GetLogger.Info('Aucun rollback nécessaire');
    Exit(True);
  end;

  // Annuler les migrations
  for i := CurrentVersion - 1 downto TargetVersion do
  begin
    Migration := TMigration(FMigrations[i]);

    if not Migration.Execute(Connection, False) then
    begin
      GetLogger.Error('Échec du rollback, arrêt du processus');
      Exit(False);
    end;

    // Supprimer l'entrée de version
    Query := TSQLQuery.Create(nil);
    try
      Query.DataBase := Connection;
      Query.SQL.Text := 'DELETE FROM schema_version WHERE version = :version';
      Query.Params.ParamByName('version').AsInteger := Migration.Version;
      Query.ExecSQL;
      Connection.Transaction.Commit;
    except
      Connection.Transaction.Rollback;
      raise;
    end;
    Query.Free;
  end;

  Result := True;
  GetLogger.Info('Rollback effectué avec succès');
end;

function TDatabaseVersionManager.GetDatabaseVersion(Connection: TSQLConnection): Integer;
begin
  EnsureVersionTable(Connection);
  Result := GetCurrentVersion(Connection);
end;

end.
```

### Exemple d'utilisation des migrations

```pascal
program MigrateDatabase;

{$mode objfpc}{$H+}

uses
  SysUtils, sqldb, pqconnection, DatabaseVersion;

var
  Connection: TPQConnection;
  VersionManager: TDatabaseVersionManager;
  Migration: TMigration;

begin
  Connection := TPQConnection.Create(nil);
  try
    Connection.HostName := 'localhost';
    Connection.DatabaseName := 'monerp';
    Connection.UserName := 'erp_user';
    Connection.Password := 'password';
    Connection.Open;

    VersionManager := TDatabaseVersionManager.Create;
    try
      // Migration 1: Ajout de la table des pays
      Migration := TMigration.Create(1, 'Ajout table countries');
      Migration.AddUpStatement(
        'CREATE TABLE countries (' +
        '  id SERIAL PRIMARY KEY,' +
        '  code VARCHAR(2) UNIQUE NOT NULL,' +
        '  name VARCHAR(100) NOT NULL' +
        ')'
      );
      Migration.AddDownStatement('DROP TABLE countries');
      VersionManager.RegisterMigration(Migration);

      // Migration 2: Ajout colonne pays aux clients
      Migration := TMigration.Create(2, 'Ajout colonne country_id aux clients');
      Migration.AddUpStatement(
        'ALTER TABLE customers ADD COLUMN country_id INTEGER REFERENCES countries(id)'
      );
      Migration.AddDownStatement(
        'ALTER TABLE customers DROP COLUMN country_id'
      );
      VersionManager.RegisterMigration(Migration);

      // Migration 3: Ajout des index
      Migration := TMigration.Create(3, 'Ajout des index de performance');
      Migration.AddUpStatement('CREATE INDEX idx_customers_name ON customers(name)');
      Migration.AddUpStatement('CREATE INDEX idx_invoices_date ON invoices(invoice_date)');
      Migration.AddUpStatement('CREATE INDEX idx_invoices_customer ON invoices(customer_id)');
      Migration.AddDownStatement('DROP INDEX idx_customers_name');
      Migration.AddDownStatement('DROP INDEX idx_invoices_date');
      Migration.AddDownStatement('DROP INDEX idx_invoices_customer');
      VersionManager.RegisterMigration(Migration);

      // Migration 4: Ajout du champ email_verified
      Migration := TMigration.Create(4, 'Ajout vérification email');
      Migration.AddUpStatement(
        'ALTER TABLE customers ADD COLUMN email_verified BOOLEAN DEFAULT false'
      );
      Migration.AddDownStatement('ALTER TABLE customers DROP COLUMN email_verified');
      VersionManager.RegisterMigration(Migration);

      // Exécuter toutes les migrations
      WriteLn('Version actuelle de la base: ', VersionManager.GetDatabaseVersion(Connection));

      if VersionManager.Migrate(Connection) then
        WriteLn('Migrations appliquées avec succès')
      else
        WriteLn('Erreur lors des migrations');

      WriteLn('Nouvelle version de la base: ', VersionManager.GetDatabaseVersion(Connection));

    finally
      VersionManager.Free;
    end;
  finally
    Connection.Free;
  end;
end.
```

## Notifications et alertes

### Système de notifications internes

```pascal
unit Notifications;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb;

type
  TNotificationType = (ntInfo, ntWarning, ntError, ntSuccess);

  TNotification = class
  private
    FId: Integer;
    FUserId: Integer;
    FTitle: string;
    FMessage: string;
    FNotificationType: TNotificationType;
    FCreatedAt: TDateTime;
    FRead: Boolean;
  public
    property Id: Integer read FId write FId;
    property UserId: Integer read FUserId write FUserId;
    property Title: string read FTitle write FTitle;
    property Message: string read FMessage write FMessage;
    property NotificationType: TNotificationType read FNotificationType write FNotificationType;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property IsRead: Boolean read FRead write FRead;

    function Save(Connection: TSQLConnection): Boolean;
    class function LoadForUser(Connection: TSQLConnection; UserId: Integer): TList;
    class function MarkAsRead(Connection: TSQLConnection; NotificationId: Integer): Boolean;
  end;

  TNotificationManager = class
  public
    class procedure SendNotification(
      Connection: TSQLConnection;
      UserId: Integer;
      const Title, Message: string;
      NotifType: TNotificationType
    );

    class function GetUnreadCount(Connection: TSQLConnection; UserId: Integer): Integer;
    class procedure SendToAllUsers(
      Connection: TSQLConnection;
      const Title, Message: string;
      NotifType: TNotificationType
    );
  end;

implementation

{ TNotification }

function TNotification.Save(Connection: TSQLConnection): Boolean;
var
  Query: TSQLQuery;
  TypeStr: string;
begin
  Result := False;

  case FNotificationType of
    ntInfo: TypeStr := 'info';
    ntWarning: TypeStr := 'warning';
    ntError: TypeStr := 'error';
    ntSuccess: TypeStr := 'success';
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;

    if FId = 0 then
    begin
      Query.SQL.Text :=
        'INSERT INTO notifications (user_id, title, message, notification_type) ' +
        'VALUES (:user_id, :title, :message, :type) RETURNING id';

      Query.Params.ParamByName('user_id').AsInteger := FUserId;
      Query.Params.ParamByName('title').AsString := FTitle;
      Query.Params.ParamByName('message').AsString := FMessage;
      Query.Params.ParamByName('type').AsString := TypeStr;

      Query.ExecSQL;
      Query.Open;
      FId := Query.Fields[0].AsInteger;
      FCreatedAt := Now;
    end
    else
    begin
      Query.SQL.Text :=
        'UPDATE notifications SET is_read = :read WHERE id = :id';
      Query.Params.ParamByName('id').AsInteger := FId;
      Query.Params.ParamByName('read').AsBoolean := FRead;
      Query.ExecSQL;
    end;

    Connection.Transaction.Commit;
    Result := True;
  except
    Connection.Transaction.Rollback;
  end;
  Query.Free;
end;

class function TNotification.LoadForUser(Connection: TSQLConnection;
  UserId: Integer): TList;
var
  Query: TSQLQuery;
  Notif: TNotification;
  TypeStr: string;
begin
  Result := TList.Create;

  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text :=
      'SELECT * FROM notifications WHERE user_id = :user_id ' +
      'ORDER BY created_at DESC LIMIT 50';
    Query.Params.ParamByName('user_id').AsInteger := UserId;
    Query.Open;

    while not Query.EOF do
    begin
      Notif := TNotification.Create;
      Notif.FId := Query.FieldByName('id').AsInteger;
      Notif.FUserId := Query.FieldByName('user_id').AsInteger;
      Notif.FTitle := Query.FieldByName('title').AsString;
      Notif.FMessage := Query.FieldByName('message').AsString;
      Notif.FCreatedAt := Query.FieldByName('created_at').AsDateTime;
      Notif.FRead := Query.FieldByName('is_read').AsBoolean;

      TypeStr := Query.FieldByName('notification_type').AsString;
      if TypeStr = 'info' then Notif.FNotificationType := ntInfo
      else if TypeStr = 'warning' then Notif.FNotificationType := ntWarning
      else if TypeStr = 'error' then Notif.FNotificationType := ntError
      else if TypeStr = 'success' then Notif.FNotificationType := ntSuccess;

      Result.Add(Notif);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

class function TNotification.MarkAsRead(Connection: TSQLConnection;
  NotificationId: Integer): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text := 'UPDATE notifications SET is_read = true WHERE id = :id';
    Query.Params.ParamByName('id').AsInteger := NotificationId;
    Query.ExecSQL;
    Connection.Transaction.Commit;
    Result := True;
  except
    Connection.Transaction.Rollback;
  end;
  Query.Free;
end;

{ TNotificationManager }

class procedure TNotificationManager.SendNotification(
  Connection: TSQLConnection;
  UserId: Integer;
  const Title, Message: string;
  NotifType: TNotificationType
);
var
  Notif: TNotification;
begin
  Notif := TNotification.Create;
  try
    Notif.UserId := UserId;
    Notif.Title := Title;
    Notif.Message := Message;
    Notif.NotificationType := NotifType;
    Notif.Save(Connection);
  finally
    Notif.Free;
  end;
end;

class function TNotificationManager.GetUnreadCount(Connection: TSQLConnection;
  UserId: Integer): Integer;
var
  Query: TSQLQuery;
begin
  Result := 0;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text :=
      'SELECT COUNT(*) as count FROM notifications ' +
      'WHERE user_id = :user_id AND is_read = false';
    Query.Params.ParamByName('user_id').AsInteger := UserId;
    Query.Open;

    if not Query.EOF then
      Result := Query.FieldByName('count').AsInteger;
  finally
    Query.Free;
  end;
end;

class procedure TNotificationManager.SendToAllUsers(
  Connection: TSQLConnection;
  const Title, Message: string;
  NotifType: TNotificationType
);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.SQL.Text := 'SELECT id FROM users WHERE is_active = true';
    Query.Open;

    while not Query.EOF do
    begin
      SendNotification(
        Connection,
        Query.FieldByName('id').AsInteger,
        Title,
        Message,
        NotifType
      );
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
```

**Structure de table pour les notifications :**

```sql
CREATE TABLE notifications (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(id) ON DELETE CASCADE,
    title VARCHAR(200) NOT NULL,
    message TEXT,
    notification_type VARCHAR(20) DEFAULT 'info',
    is_read BOOLEAN DEFAULT false,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_notifications_user ON notifications(user_id);
CREATE INDEX idx_notifications_read ON notifications(user_id, is_read);
```

## Conclusion et bonnes pratiques

### Check-list de développement ERP

Voici les points essentiels à vérifier lors du développement d'un ERP multi-plateforme :

#### 1. **Architecture**
- ✅ Séparation claire des couches (UI, métier, données)
- ✅ Modules indépendants et réutilisables
- ✅ Gestion centralisée de la configuration
- ✅ Système de plugins pour l'extensibilité

#### 2. **Base de données**
- ✅ Schéma normalisé et indexé
- ✅ Transactions ACID respectées
- ✅ Système de versioning (migrations)
- ✅ Sauvegardes automatiques régulières
- ✅ Procédures stockées pour la logique complexe

#### 3. **Sécurité**
- ✅ Authentification sécurisée (mots de passe hachés)
- ✅ Gestion fine des permissions
- ✅ Audit complet des actions
- ✅ Protection contre les injections SQL
- ✅ Chiffrement des données sensibles

#### 4. **Multi-plateforme**
- ✅ Utilisation de PathDelim pour les chemins
- ✅ Compilation conditionnelle pour les spécificités OS
- ✅ Tests sur Windows et Linux
- ✅ Scripts de déploiement pour chaque plateforme
- ✅ Documentation des dépendances par OS

#### 5. **Performance**
- ✅ Utilisation de cache pour les données fréquentes
- ✅ Pool de connexions base de données
- ✅ Requêtes optimisées avec EXPLAIN
- ✅ Pagination des résultats longs
- ✅ Chargement différé (lazy loading)

#### 6. **Expérience utilisateur**
- ✅ Interface intuitive et cohérente
- ✅ Raccourcis clavier
- ✅ Recherche rapide et filtres
- ✅ Messages d'erreur clairs
- ✅ Notifications et feedback visuel

#### 7. **Maintenance**
- ✅ Logging complet des erreurs
- ✅ Système de mise à jour automatique
- ✅ Documentation technique à jour
- ✅ Tests unitaires et d'intégration
- ✅ Monitoring des performances

### Recommandations finales

**Pour démarrer votre projet ERP :**

1. **Commencez petit** : Développez d'abord un module complet (par exemple, la gestion clients) avant d'ajouter d'autres fonctionnalités

2. **Testez régulièrement** : Compilez et testez sur Windows ET Linux dès le début du projet

3. **Documentez** : Maintenez une documentation technique et utilisateur au fur et à mesure

4. **Utilisez Git** : Versionnez votre code avec des commits réguliers et des branches pour les fonctionnalités

5. **Pensez évolutivité** : Concevez votre architecture pour pouvoir ajouter facilement de nouveaux modules

6. **Impliquez les utilisateurs** : Recueillez des retours réguliers et ajustez l'interface

7. **Préparez le déploiement** : Créez des scripts d'installation simples et testez-les

### Ressources supplémentaires

- **Forum FreePascal** : https://forum.lazarus.freepascal.org/
- **Wiki Lazarus** : https://wiki.lazarus.freepascal.org/
- **Documentation SQLdb** : https://wiki.freepascal.org/SQLdb
- **Exemples de code** : https://wiki.lazarus.freepascal.org/Code_Examples

### Le mot de la fin

Développer un ERP complet est un projet ambitieux qui demande du temps et de la rigueur. FreePascal et Lazarus offrent tous les outils nécessaires pour créer une solution professionnelle, performante et véritablement multi-plateforme. En suivant les bonnes pratiques présentées dans ce chapitre, vous avez les bases pour construire un système de gestion d'entreprise solide et évolutif.

⏭️ [Plateforme SaaS multi-tenant](/25-projets-complexes-etudes-cas/02-plateforme-saas-multi-tenant.md)
