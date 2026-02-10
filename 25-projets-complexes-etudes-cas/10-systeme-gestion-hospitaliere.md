🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 25.10 Système de gestion hospitalière

## Introduction

Un système de gestion hospitalière (SGH) est une application complexe qui coordonne l'ensemble des activités d'un établissement de santé. Ce type de projet représente un cas d'étude idéal pour un développeur avancé FreePascal/Lazarus car il combine de nombreux défis techniques : gestion de données sensibles, interfaces multi-utilisateurs, conformité réglementaire, performance et fiabilité.

Dans ce chapitre, nous allons concevoir un SGH complet et portable entre Windows et Ubuntu, en appliquant les meilleures pratiques de développement avancé.

## Vue d'ensemble du système

### Objectifs principaux

Un SGH doit gérer :

- **Dossiers patients** : informations démographiques, historique médical, allergies, traitements
- **Rendez-vous et planning** : consultations, chirurgies, examens
- **Personnel médical** : médecins, infirmières, techniciens, administratifs
- **Dossiers médicaux électroniques** (DME) : notes cliniques, prescriptions, résultats d'examens
- **Pharmacie** : stocks de médicaments, dispensation, interactions
- **Facturation** : actes médicaux, assurances, tiers payants
- **Laboratoire** : examens, résultats, analyses
- **Imagerie médicale** : radiographies, IRM, échographies
- **Statistiques et reporting** : indicateurs de performance, tableaux de bord

### Contraintes et exigences

Un SGH doit respecter des contraintes strictes :

- **Sécurité** : données médicales hautement confidentielles
- **Disponibilité** : fonctionnement 24/7 sans interruption
- **Conformité** : RGPD, normes médicales (HL7, DICOM)
- **Traçabilité** : audit complet de toutes les actions
- **Performance** : réponse rapide même avec des milliers de patients
- **Multi-utilisateurs** : accès concurrent de dizaines d'utilisateurs
- **Évolutivité** : adaptation aux besoins croissants

## Architecture globale du système

### Architecture logicielle

Pour un SGH robuste et maintenable, nous adoptons une **architecture en couches** :

```
┌─────────────────────────────────────────┐
│     Interface Utilisateur (LCL)         │
│  (Consultation, Saisie, Tableaux)       │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│      Couche Métier (Business Logic)     │
│  (Règles médicales, Validations)        │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│    Couche Accès aux Données (ORM)       │
│  (Persistance, Transactions)            │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│        Base de Données PostgreSQL       │
│  (Patients, Consultations, Examens)     │
└─────────────────────────────────────────┘
```

### Architecture technique multi-plateforme

Le système doit fonctionner de manière identique sur Windows et Ubuntu :

**Composants communs :**
- Lazarus LCL avec widgetset Win32 (Windows) ou GTK2/Qt5 (Ubuntu)
- PostgreSQL comme SGBD (identique sur les deux OS)
- OpenSSL pour le chiffrement (adaptations de chemins)
- SQLdb pour l'accès base de données

**Spécificités par OS :**

| Fonction | Windows | Ubuntu |
|----------|---------|--------|
| Service système | Windows Service | systemd unit |
| Authentification | Active Directory | LDAP/PAM |
| Impression | Windows Print Spooler | CUPS |
| Stockage documents | SMB/CIFS | NFS/Samba |
| Certificats SSL | Windows Certificate Store | /etc/ssl/certs |

### Choix de la base de données

Pour un SGH, nous recommandons **PostgreSQL** car :

- Gratuit et open source (pas de coûts de licence)
- Excellent support des transactions ACID
- Fonctionnalités avancées (JSON, recherche full-text, triggers complexes)
- Même comportement sur Windows et Ubuntu
- Performance éprouvée en production
- Support natif dans FreePascal via SQLdb

**Alternative** : Firebird est également excellent pour les petites structures.

## Modèle de données

### Structure de la base de données

Voici les tables principales du système (version simplifiée) :

```sql
-- Patients
CREATE TABLE patients (
    id_patient SERIAL PRIMARY KEY,
    numero_dossier VARCHAR(20) UNIQUE NOT NULL,
    nom VARCHAR(100) NOT NULL,
    prenom VARCHAR(100) NOT NULL,
    date_naissance DATE NOT NULL,
    sexe CHAR(1) CHECK (sexe IN ('M', 'F', 'A')),
    numero_secu VARCHAR(15),
    adresse TEXT,
    telephone VARCHAR(20),
    email VARCHAR(100),
    medecin_traitant VARCHAR(100),
    groupe_sanguin VARCHAR(5),
    allergies TEXT,
    date_creation TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    date_modification TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    actif BOOLEAN DEFAULT TRUE
);

-- Personnel médical
CREATE TABLE personnel (
    id_personnel SERIAL PRIMARY KEY,
    matricule VARCHAR(20) UNIQUE NOT NULL,
    nom VARCHAR(100) NOT NULL,
    prenom VARCHAR(100) NOT NULL,
    specialite VARCHAR(100),
    type_personnel VARCHAR(50), -- Médecin, Infirmier, Admin, etc.
    numero_ordre VARCHAR(20), -- Ordre des médecins
    email VARCHAR(100),
    telephone VARCHAR(20),
    actif BOOLEAN DEFAULT TRUE
);

-- Consultations
CREATE TABLE consultations (
    id_consultation SERIAL PRIMARY KEY,
    id_patient INTEGER REFERENCES patients(id_patient),
    id_medecin INTEGER REFERENCES personnel(id_personnel),
    date_consultation TIMESTAMP NOT NULL,
    motif TEXT,
    diagnostic TEXT,
    observations TEXT,
    prescription TEXT,
    duree_minutes INTEGER,
    statut VARCHAR(20), -- Planifiée, En cours, Terminée, Annulée
    date_creation TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Prescriptions médicales
CREATE TABLE prescriptions (
    id_prescription SERIAL PRIMARY KEY,
    id_consultation INTEGER REFERENCES consultations(id_consultation),
    id_patient INTEGER REFERENCES patients(id_patient),
    id_prescripteur INTEGER REFERENCES personnel(id_personnel),
    date_prescription TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    date_debut DATE,
    date_fin DATE,
    instructions TEXT,
    statut VARCHAR(20) -- Active, Terminée, Annulée
);

-- Détails prescriptions (médicaments)
CREATE TABLE prescription_details (
    id_detail SERIAL PRIMARY KEY,
    id_prescription INTEGER REFERENCES prescriptions(id_prescription),
    medicament VARCHAR(200) NOT NULL,
    dosage VARCHAR(100),
    frequence VARCHAR(100),
    duree VARCHAR(50),
    quantite INTEGER,
    instructions TEXT
);

-- Examens et analyses
CREATE TABLE examens (
    id_examen SERIAL PRIMARY KEY,
    id_patient INTEGER REFERENCES patients(id_patient),
    id_prescripteur INTEGER REFERENCES personnel(id_personnel),
    type_examen VARCHAR(100), -- Biologie, Radiologie, etc.
    description TEXT,
    date_prescription TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    date_realisation TIMESTAMP,
    resultats TEXT,
    fichiers_joints TEXT[], -- Chemins vers images/PDF
    statut VARCHAR(20), -- Prescrit, En attente, Réalisé
    urgent BOOLEAN DEFAULT FALSE
);

-- Rendez-vous
CREATE TABLE rendez_vous (
    id_rdv SERIAL PRIMARY KEY,
    id_patient INTEGER REFERENCES patients(id_patient),
    id_personnel INTEGER REFERENCES personnel(id_personnel),
    date_rdv TIMESTAMP NOT NULL,
    duree_minutes INTEGER DEFAULT 30,
    type_rdv VARCHAR(50), -- Consultation, Chirurgie, Examen
    statut VARCHAR(20), -- Confirmé, En attente, Annulé
    notes TEXT,
    salle VARCHAR(50)
);

-- Hospitalisations
CREATE TABLE hospitalisations (
    id_hospitalisation SERIAL PRIMARY KEY,
    id_patient INTEGER REFERENCES patients(id_patient),
    id_medecin_responsable INTEGER REFERENCES personnel(id_personnel),
    date_admission TIMESTAMP NOT NULL,
    date_sortie TIMESTAMP,
    service VARCHAR(100),
    chambre VARCHAR(20),
    motif_admission TEXT,
    diagnostic_principal TEXT,
    compte_rendu_sortie TEXT,
    statut VARCHAR(20) -- En cours, Sortie, Décédé
);

-- Facturation
CREATE TABLE factures (
    id_facture SERIAL PRIMARY KEY,
    id_patient INTEGER REFERENCES patients(id_patient),
    numero_facture VARCHAR(50) UNIQUE NOT NULL,
    date_facture DATE NOT NULL,
    montant_total DECIMAL(10,2),
    montant_paye DECIMAL(10,2) DEFAULT 0,
    statut VARCHAR(20), -- Non payée, Payée, Partielle
    mode_paiement VARCHAR(50),
    assurance VARCHAR(100),
    numero_assurance VARCHAR(50)
);

-- Lignes de facturation
CREATE TABLE facture_lignes (
    id_ligne SERIAL PRIMARY KEY,
    id_facture INTEGER REFERENCES factures(id_facture),
    code_acte VARCHAR(20),
    libelle VARCHAR(200),
    quantite INTEGER DEFAULT 1,
    prix_unitaire DECIMAL(10,2),
    montant DECIMAL(10,2)
);

-- Pharmacie - Stock médicaments
CREATE TABLE medicaments (
    id_medicament SERIAL PRIMARY KEY,
    code_cip VARCHAR(20) UNIQUE,
    nom_commercial VARCHAR(200) NOT NULL,
    dci VARCHAR(200), -- Dénomination Commune Internationale
    dosage VARCHAR(100),
    forme VARCHAR(50), -- Comprimé, Gélule, Sirop, etc.
    quantite_stock INTEGER DEFAULT 0,
    seuil_alerte INTEGER DEFAULT 10,
    prix_unitaire DECIMAL(10,2),
    date_peremption DATE,
    fabricant VARCHAR(100)
);

-- Audit et traçabilité
CREATE TABLE audit_log (
    id_log SERIAL PRIMARY KEY,
    date_action TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    id_utilisateur INTEGER REFERENCES personnel(id_personnel),
    action VARCHAR(50), -- INSERT, UPDATE, DELETE, SELECT
    table_name VARCHAR(100),
    record_id INTEGER,
    details JSONB, -- Anciennes et nouvelles valeurs
    adresse_ip VARCHAR(45)
);
```

### Indexation pour performance

Les index sont cruciaux pour un SGH performant :

```sql
-- Index sur les recherches fréquentes
CREATE INDEX idx_patients_nom ON patients(nom, prenom);  
CREATE INDEX idx_patients_numero ON patients(numero_dossier);  
CREATE INDEX idx_patients_date_naiss ON patients(date_naissance);  
CREATE INDEX idx_consultations_date ON consultations(date_consultation);  
CREATE INDEX idx_consultations_patient ON consultations(id_patient);  
CREATE INDEX idx_rdv_date ON rendez_vous(date_rdv);  
CREATE INDEX idx_rdv_personnel ON rendez_vous(id_personnel);  
CREATE INDEX idx_audit_date ON audit_log(date_action);  
CREATE INDEX idx_audit_user ON audit_log(id_utilisateur);

-- Index full-text pour recherche dans les notes
CREATE INDEX idx_consultations_fulltext ON consultations
    USING gin(to_tsvector('french', diagnostic || ' ' || observations));
```

## Implémentation FreePascal/Lazarus

### Classes métier principales

Créons les classes Pascal correspondant à notre modèle :

```pascal
unit UHospitalModel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  // Patient
  TPatient = class
  private
    FIdPatient: Integer;
    FNumeroDossier: string;
    FNom: string;
    FPrenom: string;
    FDateNaissance: TDate;
    FSexe: Char;
    FNumeroSecu: string;
    FAdresse: string;
    FTelephone: string;
    FEmail: string;
    FGroupeSanguin: string;
    FAllergies: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetAge: Integer;
    function GetNomComplet: string;
    function EstMineur: Boolean;

    property IdPatient: Integer read FIdPatient write FIdPatient;
    property NumeroDossier: string read FNumeroDossier write FNumeroDossier;
    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property DateNaissance: TDate read FDateNaissance write FDateNaissance;
    property Sexe: Char read FSexe write FSexe;
    property Allergies: TStringList read FAllergies;
  end;

  // Personnel médical
  TPersonnel = class
  private
    FIdPersonnel: Integer;
    FMatricule: string;
    FNom: string;
    FPrenom: string;
    FSpecialite: string;
    FTypePersonnel: string;
    FNumeroOrdre: string;
  public
    property IdPersonnel: Integer read FIdPersonnel write FIdPersonnel;
    property Matricule: string read FMatricule write FMatricule;
    property NomComplet: string read GetNomComplet;
    property Specialite: string read FSpecialite write FSpecialite;
    property TypePersonnel: string read FTypePersonnel write FTypePersonnel;
  end;

  // Consultation
  TConsultation = class
  private
    FIdConsultation: Integer;
    FPatient: TPatient;
    FMedecin: TPersonnel;
    FDateConsultation: TDateTime;
    FMotif: string;
    FDiagnostic: string;
    FObservations: string;
    FStatut: string;
  public
    constructor Create;
    destructor Destroy; override;

    function EstTerminee: Boolean;
    function GetDureeMinutes: Integer;

    property IdConsultation: Integer read FIdConsultation write FIdConsultation;
    property Patient: TPatient read FPatient write FPatient;
    property Medecin: TPersonnel read FMedecin write FMedecin;
    property DateConsultation: TDateTime read FDateConsultation write FDateConsultation;
    property Motif: string read FMotif write FMotif;
    property Diagnostic: string read FDiagnostic write FDiagnostic;
  end;

implementation

{ TPatient }

constructor TPatient.Create;  
begin
  inherited Create;
  FAllergies := TStringList.Create;
end;

destructor TPatient.Destroy;  
begin
  FAllergies.Free;
  inherited Destroy;
end;

function TPatient.GetAge: Integer;  
begin
  Result := YearsBetween(Now, FDateNaissance);
end;

function TPatient.GetNomComplet: string;  
begin
  Result := FNom + ' ' + FPrenom;
end;

function TPatient.EstMineur: Boolean;  
begin
  Result := GetAge < 18;
end;

{ TConsultation }

constructor TConsultation.Create;  
begin
  inherited Create;
  FDateConsultation := Now;
  FStatut := 'En cours';
end;

destructor TConsultation.Destroy;  
begin
  // Ne pas libérer Patient et Medecin car gérés ailleurs
  inherited Destroy;
end;

function TConsultation.EstTerminee: Boolean;  
begin
  Result := FStatut = 'Terminée';
end;

end.
```

### Couche d'accès aux données (DAO)

Créons une classe d'accès aux données pour les patients :

```pascal
unit UPatientDAO;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, UHospitalModel, UDatabaseConnection;

type
  TPatientDAO = class
  private
    FConnection: TDatabaseConnection;
    FQuery: TSQLQuery;
  public
    constructor Create(AConnection: TDatabaseConnection);
    destructor Destroy; override;

    function GetById(AId: Integer): TPatient;
    function FindByNumeroDossier(ANumero: string): TPatient;
    function Search(ANom, APrenom: string): TList;
    function Insert(APatient: TPatient): Integer;
    function Update(APatient: TPatient): Boolean;
    function Delete(AId: Integer): Boolean;
    function GetAllActifs: TList;
  end;

implementation

constructor TPatientDAO.Create(AConnection: TDatabaseConnection);  
begin
  inherited Create;
  FConnection := AConnection;
  FQuery := TSQLQuery.Create(nil);
  FQuery.Database := FConnection.Database;
  FQuery.Transaction := FConnection.Transaction;
end;

destructor TPatientDAO.Destroy;  
begin
  FQuery.Free;
  inherited Destroy;
end;

function TPatientDAO.GetById(AId: Integer): TPatient;  
begin
  Result := nil;
  FQuery.Close;
  FQuery.SQL.Text :=
    'SELECT * FROM patients WHERE id_patient = :id';
  FQuery.ParamByName('id').AsInteger := AId;
  FQuery.Open;

  if not FQuery.EOF then
  begin
    Result := TPatient.Create;
    Result.IdPatient := FQuery.FieldByName('id_patient').AsInteger;
    Result.NumeroDossier := FQuery.FieldByName('numero_dossier').AsString;
    Result.Nom := FQuery.FieldByName('nom').AsString;
    Result.Prenom := FQuery.FieldByName('prenom').AsString;
    Result.DateNaissance := FQuery.FieldByName('date_naissance').AsDateTime;
    Result.Sexe := FQuery.FieldByName('sexe').AsString[1];
    // ... autres champs
  end;

  FQuery.Close;
end;

function TPatientDAO.Search(ANom, APrenom: string): TList;  
var
  Patient: TPatient;
begin
  Result := TList.Create;
  FQuery.Close;
  FQuery.SQL.Text :=
    'SELECT * FROM patients ' +
    'WHERE UPPER(nom) LIKE UPPER(:nom) ' +
    'AND UPPER(prenom) LIKE UPPER(:prenom) ' +
    'AND actif = TRUE ' +
    'ORDER BY nom, prenom';
  FQuery.ParamByName('nom').AsString := '%' + ANom + '%';
  FQuery.ParamByName('prenom').AsString := '%' + APrenom + '%';
  FQuery.Open;

  while not FQuery.EOF do
  begin
    Patient := TPatient.Create;
    // Remplir les données...
    Result.Add(Patient);
    FQuery.Next;
  end;

  FQuery.Close;
end;

function TPatientDAO.Insert(APatient: TPatient): Integer;  
begin
  FQuery.Close;
  FQuery.SQL.Text :=
    'INSERT INTO patients ' +
    '(numero_dossier, nom, prenom, date_naissance, sexe, ' +
    ' adresse, telephone, email, groupe_sanguin) ' +
    'VALUES (:numero, :nom, :prenom, :date_naiss, :sexe, ' +
    '        :adresse, :tel, :email, :groupe) ' +
    'RETURNING id_patient';

  FQuery.ParamByName('numero').AsString := APatient.NumeroDossier;
  FQuery.ParamByName('nom').AsString := APatient.Nom;
  FQuery.ParamByName('prenom').AsString := APatient.Prenom;
  FQuery.ParamByName('date_naiss').AsDate := APatient.DateNaissance;
  FQuery.ParamByName('sexe').AsString := APatient.Sexe;
  // ... autres paramètres

  FQuery.Open;
  Result := FQuery.Fields[0].AsInteger;
  FConnection.Transaction.Commit;
  FQuery.Close;
end;

end.
```

### Gestion de connexion multi-plateforme

Créons une classe de connexion qui s'adapte à l'OS :

```pascal
unit UDatabaseConnection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, PQConnection;

type
  TDatabaseConnection = class
  private
    FConnection: TPQConnection;
    FTransaction: TSQLTransaction;
    FConnected: Boolean;

    function GetLibraryPath: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect(AHost, ADatabase, AUser, APassword: string; APort: Integer = 5432);
    procedure Disconnect;
    function IsConnected: Boolean;

    property Database: TPQConnection read FConnection;
    property Transaction: TSQLTransaction read FTransaction;
  end;

implementation

constructor TDatabaseConnection.Create;  
begin
  inherited Create;
  FConnection := TPQConnection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Database := FConnection;
  FConnection.Transaction := FTransaction;
  FConnected := False;
end;

destructor TDatabaseConnection.Destroy;  
begin
  Disconnect;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

function TDatabaseConnection.GetLibraryPath: string;  
begin
  {$IFDEF WINDOWS}
    // Sous Windows, chercher libpq.dll
    Result := 'libpq.dll';
    // Ou spécifier un chemin complet si besoin
    if not FileExists(Result) then
      Result := 'C:\Program Files\PostgreSQL\15\bin\libpq.dll';
  {$ENDIF}

  {$IFDEF UNIX}
    {$IFDEF LINUX}
      // Sous Ubuntu/Linux, utiliser libpq.so
      Result := 'libpq.so.5';
      // Le système trouve automatiquement dans /usr/lib
    {$ENDIF}
  {$ENDIF}
end;

procedure TDatabaseConnection.Connect(AHost, ADatabase, AUser, APassword: string;
  APort: Integer = 5432);
begin
  if FConnected then
    Exit;

  try
    // Définir la bibliothèque selon l'OS
    FConnection.DatabaseName := ADatabase;
    FConnection.HostName := AHost;
    FConnection.UserName := AUser;
    FConnection.Password := APassword;
    FConnection.Port := APort;

    // Options de connexion
    FConnection.Params.Add('client_encoding=UTF8');
    FConnection.Params.Add('application_name=SGH_Hopital');

    FConnection.Open;
    FConnected := True;
  except
    on E: Exception do
    begin
      raise Exception.Create('Erreur de connexion à la base de données: ' + E.Message);
    end;
  end;
end;

procedure TDatabaseConnection.Disconnect;  
begin
  if FConnected then
  begin
    if FTransaction.Active then
      FTransaction.Rollback;
    FConnection.Close;
    FConnected := False;
  end;
end;

function TDatabaseConnection.IsConnected: Boolean;  
begin
  Result := FConnected and FConnection.Connected;
end;

end.
```

## Interface utilisateur

### Formulaire principal

Le formulaire principal du SGH offre un menu avec accès aux différents modules :

```pascal
unit UMainForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, UDatabaseConnection;

type
  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    mnuPatients: TMenuItem;
    mnuPatientNouveau: TMenuItem;
    mnuPatientRecherche: TMenuItem;
    mnuConsultations: TMenuItem;
    mnuConsultationNouvelle: TMenuItem;
    mnuConsultationListe: TMenuItem;
    mnuRendezVous: TMenuItem;
    mnuRdvPlanning: TMenuItem;
    mnuPharmacie: TMenuItem;
    mnuPharmacieStock: TMenuItem;
    mnuPharmacieDispensation: TMenuItem;
    mnuFacturation: TMenuItem;
    mnuStatistiques: TMenuItem;
    mnuAdministration: TMenuItem;
    mnuAdminUtilisateurs: TMenuItem;
    mnuAdminParametres: TMenuItem;
    StatusBar1: TStatusBar;
    PanelInfo: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure mnuPatientNouveauClick(Sender: TObject);
    procedure mnuPatientRechercheClick(Sender: TObject);
  private
    FDatabase: TDatabaseConnection;
    FUtilisateurConnecte: string;
  public
    property Database: TDatabaseConnection read FDatabase;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  UPatientForm, UPatientSearchForm;

{$R *.lfm}

procedure TfrmMain.FormCreate(Sender: TObject);  
begin
  // Initialiser la connexion
  FDatabase := TDatabaseConnection.Create;

  try
    // Paramètres de connexion (à externaliser dans un fichier config)
    FDatabase.Connect('localhost', 'hopital_db', 'admin', 'password');
    StatusBar1.Panels[0].Text := 'Connecté à la base de données';
  except
    on E: Exception do
    begin
      ShowMessage('Impossible de se connecter à la base de données: ' + E.Message);
      Application.Terminate;
    end;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);  
begin
  FDatabase.Free;
end;

procedure TfrmMain.mnuPatientNouveauClick(Sender: TObject);  
var
  frmPatient: TfrmPatient;
begin
  frmPatient := TfrmPatient.Create(Self);
  try
    frmPatient.Database := FDatabase;
    frmPatient.ShowModal;
  finally
    frmPatient.Free;
  end;
end;

procedure TfrmMain.mnuPatientRechercheClick(Sender: TObject);  
var
  frmSearch: TfrmPatientSearch;
begin
  frmSearch := TfrmPatientSearch.Create(Self);
  try
    frmSearch.Database := FDatabase;
    frmSearch.ShowModal;
  finally
    frmSearch.Free;
  end;
end;

end.
```

### Formulaire de saisie patient

Interface pour créer ou modifier un patient :

```pascal
unit UPatientForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, EditBtn, UHospitalModel, UPatientDAO, UDatabaseConnection;

type
  TfrmPatient = class(TForm)
    edtNom: TEdit;
    edtPrenom: TEdit;
    edtNumDossier: TEdit;
    dtpDateNaissance: TDateEdit;
    cmbSexe: TComboBox;
    edtNumSecu: TEdit;
    memoAdresse: TMemo;
    edtTelephone: TEdit;
    edtEmail: TEdit;
    cmbGroupeSanguin: TComboBox;
    memoAllergies: TMemo;
    btnSauvegarder: TButton;
    btnAnnuler: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnSauvegarderClick(Sender: TObject);
    procedure btnAnnulerClick(Sender: TObject);
  private
    FDatabase: TDatabaseConnection;
    FPatient: TPatient;
    FPatientDAO: TPatientDAO;
    FModeEdition: Boolean;

    function ValiderFormulaire: Boolean;
    procedure ChargerPatient;
    procedure SauvegarderPatient;
  public
    property Database: TDatabaseConnection read FDatabase write FDatabase;
    property Patient: TPatient read FPatient write FPatient;
    property ModeEdition: Boolean read FModeEdition write FModeEdition;
  end;

implementation

{$R *.lfm}

procedure TfrmPatient.FormCreate(Sender: TObject);  
begin
  FPatient := TPatient.Create;

  // Initialiser les combos
  cmbSexe.Items.Clear;
  cmbSexe.Items.Add('Masculin');
  cmbSexe.Items.Add('Féminin');
  cmbSexe.Items.Add('Autre');

  cmbGroupeSanguin.Items.Clear;
  cmbGroupeSanguin.Items.AddStrings(['A+', 'A-', 'B+', 'B-',
    'AB+', 'AB-', 'O+', 'O-']);
end;

function TfrmPatient.ValiderFormulaire: Boolean;  
begin
  Result := True;

  // Validation nom/prénom
  if Trim(edtNom.Text) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    edtNom.SetFocus;
    Exit(False);
  end;

  if Trim(edtPrenom.Text) = '' then
  begin
    ShowMessage('Le prénom est obligatoire');
    edtPrenom.SetFocus;
    Exit(False);
  end;

  // Validation date de naissance
  if dtpDateNaissance.Date > Now then
  begin
    ShowMessage('La date de naissance ne peut pas être dans le futur');
    Exit(False);
  end;

  // Validation email (simple)
  if (Trim(edtEmail.Text) <> '') and
     (Pos('@', edtEmail.Text) = 0) then
  begin
    ShowMessage('Email invalide');
    Exit(False);
  end;
end;

procedure TfrmPatient.btnSauvegarderClick(Sender: TObject);  
begin
  if not ValiderFormulaire then
    Exit;

  FPatient.Nom := Trim(edtNom.Text);
  FPatient.Prenom := Trim(edtPrenom.Text);
  FPatient.NumeroDossier := Trim(edtNumDossier.Text);
  FPatient.DateNaissance := dtpDateNaissance.Date;

  case cmbSexe.ItemIndex of
    0: FPatient.Sexe := 'M';
    1: FPatient.Sexe := 'F';
    2: FPatient.Sexe := 'A';
  end;

  // Sauvegarder dans la base
  FPatientDAO := TPatientDAO.Create(FDatabase);
  try
    if FModeEdition then
      FPatientDAO.Update(FPatient)
    else
    begin
      FPatient.IdPatient := FPatientDAO.Insert(FPatient);
      ShowMessage('Patient créé avec succès. N° dossier: ' +
        FPatient.NumeroDossier);
    end;

    ModalResult := mrOK;
  finally
    FPatientDAO.Free;
  end;
end;

procedure TfrmPatient.btnAnnulerClick(Sender: TObject);  
begin
  ModalResult := mrCancel;
end;

end.
```

### Recherche de patients avec grille

```pascal
unit UPatientSearchForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  DBGrids, ExtCtrls, UPatientDAO, UDatabaseConnection;

type
  TfrmPatientSearch = class(TForm)
    edtNom: TEdit;
    edtPrenom: TEdit;
    btnRechercher: TButton;
    grdResultats: TStringGrid;
    btnOuvrir: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnRechercherClick(Sender: TObject);
    procedure grdResultatsDblClick(Sender: TObject);
  private
    FDatabase: TDatabaseConnection;
    FResultats: TList;

    procedure AfficherResultats;
  public
    property Database: TDatabaseConnection read FDatabase write FDatabase;
  end;

implementation

uses UHospitalModel, UPatientForm;

{$R *.lfm}

procedure TfrmPatientSearch.FormCreate(Sender: TObject);  
begin
  // Configuration de la grille
  grdResultats.ColCount := 7;
  grdResultats.RowCount := 1;
  grdResultats.Cells[0, 0] := 'N° Dossier';
  grdResultats.Cells[1, 0] := 'Nom';
  grdResultats.Cells[2, 0] := 'Prénom';
  grdResultats.Cells[3, 0] := 'Date naissance';
  grdResultats.Cells[4, 0] := 'Âge';
  grdResultats.Cells[5, 0] := 'Sexe';
  grdResultats.Cells[6, 0] := 'Téléphone';

  FResultats := TList.Create;
end;

procedure TfrmPatientSearch.btnRechercherClick(Sender: TObject);  
var
  DAO: TPatientDAO;
begin
  DAO := TPatientDAO.Create(FDatabase);
  try
    // Libérer anciens résultats
    FResultats.Clear;

    // Recherche
    FResultats := DAO.Search(edtNom.Text, edtPrenom.Text);

    // Afficher
    AfficherResultats;
  finally
    DAO.Free;
  end;
end;

procedure TfrmPatientSearch.AfficherResultats;  
var
  i: Integer;
  Patient: TPatient;
begin
  grdResultats.RowCount := FResultats.Count + 1;

  for i := 0 to FResultats.Count - 1 do
  begin
    Patient := TPatient(FResultats[i]);
    grdResultats.Cells[0, i + 1] := Patient.NumeroDossier;
    grdResultats.Cells[1, i + 1] := Patient.Nom;
    grdResultats.Cells[2, i + 1] := Patient.Prenom;
    grdResultats.Cells[3, i + 1] := DateToStr(Patient.DateNaissance);
    grdResultats.Cells[4, i + 1] := IntToStr(Patient.GetAge);
    grdResultats.Cells[5, i + 1] := Patient.Sexe;
    // Téléphone si disponible...
  end;
end;

end.
```

## Sécurité et authentification

### Système d'authentification

Un SGH doit avoir un système d'authentification robuste :

```pascal
unit UAuthenticationManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SHA1, UDatabaseConnection, SQLDB;

type
  TUserRole = (urAdmin, urMedecin, urInfirmier, urSecretaire, urLaborantin);

  TUtilisateur = class
  private
    FIdUtilisateur: Integer;
    FUsername: string;
    FNomComplet: string;
    FRole: TUserRole;
    FPermissions: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function APermission(APermission: string): Boolean;

    property IdUtilisateur: Integer read FIdUtilisateur write FIdUtilisateur;
    property Username: string read FUsername write FUsername;
    property NomComplet: string read FNomComplet write FNomComplet;
    property Role: TUserRole read FRole write FRole;
  end;

  TAuthenticationManager = class
  private
    FDatabase: TDatabaseConnection;
    FUtilisateurActuel: TUtilisateur;

    function HashPassword(APassword: string): string;
    function VerifyPassword(APassword, AHash: string): Boolean;
  public
    constructor Create(ADatabase: TDatabaseConnection);
    destructor Destroy; override;

    function Login(AUsername, APassword: string): Boolean;
    procedure Logout;
    function ChangePassword(AOldPassword, ANewPassword: string): Boolean;
    function IsLoggedIn: Boolean;

    property UtilisateurActuel: TUtilisateur read FUtilisateurActuel;
  end;

implementation

{ TUtilisateur }

constructor TUtilisateur.Create;  
begin
  inherited Create;
  FPermissions := TStringList.Create;
end;

destructor TUtilisateur.Destroy;  
begin
  FPermissions.Free;
  inherited Destroy;
end;

function TUtilisateur.APermission(APermission: string): Boolean;  
begin
  Result := FPermissions.IndexOf(APermission) >= 0;
end;

{ TAuthenticationManager }

constructor TAuthenticationManager.Create(ADatabase: TDatabaseConnection);  
begin
  inherited Create;
  FDatabase := ADatabase;
  FUtilisateurActuel := nil;
end;

destructor TAuthenticationManager.Destroy;  
begin
  if Assigned(FUtilisateurActuel) then
    FUtilisateurActuel.Free;
  inherited Destroy;
end;

function TAuthenticationManager.HashPassword(APassword: string): string;  
begin
  // Utiliser SHA256 en production, SHA1 ici pour simplicité
  Result := SHA1Print(SHA1String(APassword));
end;

function TAuthenticationManager.VerifyPassword(APassword, AHash: string): Boolean;  
begin
  Result := HashPassword(APassword) = AHash;
end;

function TAuthenticationManager.Login(AUsername, APassword: string): Boolean;  
var
  Query: TSQLQuery;
  PasswordHash: string;
begin
  Result := False;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabase.Database;
    Query.Transaction := FDatabase.Transaction;

    Query.SQL.Text :=
      'SELECT id_personnel, nom, prenom, password_hash, type_personnel ' +
      'FROM personnel ' +
      'WHERE username = :username AND actif = TRUE';
    Query.ParamByName('username').AsString := AUsername;
    Query.Open;

    if not Query.EOF then
    begin
      PasswordHash := Query.FieldByName('password_hash').AsString;

      if VerifyPassword(APassword, PasswordHash) then
      begin
        // Créer l'utilisateur
        FUtilisateurActuel := TUtilisateur.Create;
        FUtilisateurActuel.IdUtilisateur := Query.FieldByName('id_personnel').AsInteger;
        FUtilisateurActuel.Username := AUsername;
        FUtilisateurActuel.NomComplet :=
          Query.FieldByName('nom').AsString + ' ' +
          Query.FieldByName('prenom').AsString;

        // Charger les permissions...

        Result := True;

        // Logger la connexion
        LogAudit('LOGIN', AUsername);
      end;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TAuthenticationManager.Logout;  
begin
  if Assigned(FUtilisateurActuel) then
  begin
    LogAudit('LOGOUT', FUtilisateurActuel.Username);
    FreeAndNil(FUtilisateurActuel);
  end;
end;

function TAuthenticationManager.IsLoggedIn: Boolean;  
begin
  Result := Assigned(FUtilisateurActuel);
end;

end.
```

### Audit et traçabilité

Chaque action importante doit être tracée :

```pascal
unit UAuditManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UDatabaseConnection, SQLDB, fpjson;

type
  TAuditManager = class
  private
    FDatabase: TDatabaseConnection;
    FIdUtilisateur: Integer;
    FAdresseIP: string;
  public
    constructor Create(ADatabase: TDatabaseConnection);

    procedure LogAction(AAction, ATableName: string; ARecordId: Integer;
      ADetails: TJSONObject = nil);
    procedure LogAccesPatient(AIdPatient: Integer);
    procedure LogModificationPatient(AIdPatient: Integer;
      AOldValues, ANewValues: TJSONObject);

    property IdUtilisateur: Integer read FIdUtilisateur write FIdUtilisateur;
    property AdresseIP: string read FAdresseIP write FAdresseIP;
  end;

implementation

constructor TAuditManager.Create(ADatabase: TDatabaseConnection);  
begin
  inherited Create;
  FDatabase := ADatabase;
  FAdresseIP := '127.0.0.1'; // À améliorer
end;

procedure TAuditManager.LogAction(AAction, ATableName: string;
  ARecordId: Integer; ADetails: TJSONObject = nil);
var
  Query: TSQLQuery;
  DetailsJSON: string;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabase.Database;
    Query.Transaction := FDatabase.Transaction;

    if Assigned(ADetails) then
      DetailsJSON := ADetails.AsJSON
    else
      DetailsJSON := '{}';

    Query.SQL.Text :=
      'INSERT INTO audit_log ' +
      '(id_utilisateur, action, table_name, record_id, details, adresse_ip) ' +
      'VALUES (:user_id, :action, :table, :record, :details::jsonb, :ip)';

    Query.ParamByName('user_id').AsInteger := FIdUtilisateur;
    Query.ParamByName('action').AsString := AAction;
    Query.ParamByName('table').AsString := ATableName;
    Query.ParamByName('record').AsInteger := ARecordId;
    Query.ParamByName('details').AsString := DetailsJSON;
    Query.ParamByName('ip').AsString := FAdresseIP;

    Query.ExecSQL;
    FDatabase.Transaction.Commit;
  finally
    Query.Free;
  end;
end;

procedure TAuditManager.LogAccesPatient(AIdPatient: Integer);  
begin
  LogAction('SELECT', 'patients', AIdPatient);
end;

end.
```

## Gestion des droits d'accès

### Système de permissions

```pascal
unit UPermissionManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UAuthenticationManager;

type
  TPermissionManager = class
  private
    FUtilisateur: TUtilisateur;
  public
    constructor Create(AUtilisateur: TUtilisateur);

    function PeutCreerPatient: Boolean;
    function PeutModifierPatient: Boolean;
    function PeutSupprimerPatient: Boolean;
    function PeutConsulterDossierMedical(AIdPatient: Integer): Boolean;
    function PeutPrescrire: Boolean;
    function PeutAccederFacturation: Boolean;
    function PeutAccederAdministration: Boolean;
  end;

implementation

constructor TPermissionManager.Create(AUtilisateur: TUtilisateur);  
begin
  inherited Create;
  FUtilisateur := AUtilisateur;
end;

function TPermissionManager.PeutCreerPatient: Boolean;  
begin
  // Tous sauf laborantin peuvent créer des patients
  Result := FUtilisateur.Role in [urAdmin, urMedecin, urInfirmier, urSecretaire];
end;

function TPermissionManager.PeutModifierPatient: Boolean;  
begin
  Result := FUtilisateur.Role in [urAdmin, urMedecin, urSecretaire];
end;

function TPermissionManager.PeutConsulterDossierMedical(AIdPatient: Integer): Boolean;  
begin
  // Les médecins et infirmiers peuvent consulter les dossiers
  Result := FUtilisateur.Role in [urAdmin, urMedecin, urInfirmier];

  // TODO: Vérifier si le médecin est affecté à ce patient
end;

function TPermissionManager.PeutPrescrire: Boolean;  
begin
  // Seuls les médecins peuvent prescrire
  Result := FUtilisateur.Role in [urAdmin, urMedecin];
end;

function TPermissionManager.PeutAccederFacturation: Boolean;  
begin
  Result := FUtilisateur.Role in [urAdmin, urSecretaire];
end;

function TPermissionManager.PeutAccederAdministration: Boolean;  
begin
  Result := FUtilisateur.Role = urAdmin;
end;

end.
```

## Fonctionnalités avancées

### Module de prescriptions

```pascal
unit UPrescriptionManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UHospitalModel, UDatabaseConnection, SQLDB;

type
  TMedicament = record
    Code: string;
    Nom: string;
    Dosage: string;
    Forme: string;
  end;

  TLignePrescription = class
    Medicament: TMedicament;
    Dosage: string;
    Frequence: string;
    Duree: string;
    Quantite: Integer;
    Instructions: string;
  end;

  TPrescription = class
  private
    FIdPrescription: Integer;
    FPatient: TPatient;
    FMedecin: TPersonnel;
    FLignes: TList;
    FDatePrescription: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AjouterLigne(ALigne: TLignePrescription);
    function VerifierInteractions: TStringList;
    function VerifierAllergies: TStringList;

    property Lignes: TList read FLignes;
  end;

  TPrescriptionManager = class
  private
    FDatabase: TDatabaseConnection;

    function GetInteractionsMedicamenteuses(AMed1, AMed2: string): Boolean;
  public
    constructor Create(ADatabase: TDatabaseConnection);

    function CreerPrescription(APrescription: TPrescription): Integer;
    function GetPrescriptionsPatient(AIdPatient: Integer): TList;
    function VerifierContrindications(APatient: TPatient;
      AMedicament: string): TStringList;
  end;

implementation

{ TPrescription }

constructor TPrescription.Create;  
begin
  inherited Create;
  FLignes := TList.Create;
  FDatePrescription := Now;
end;

destructor TPrescription.Destroy;  
var
  i: Integer;
begin
  for i := 0 to FLignes.Count - 1 do
    TLignePrescription(FLignes[i]).Free;
  FLignes.Free;
  inherited Destroy;
end;

procedure TPrescription.AjouterLigne(ALigne: TLignePrescription);  
begin
  FLignes.Add(ALigne);
end;

function TPrescription.VerifierInteractions: TStringList;  
var
  i, j: Integer;
begin
  Result := TStringList.Create;

  // Vérifier les interactions entre médicaments
  for i := 0 to FLignes.Count - 2 do
    for j := i + 1 to FLignes.Count - 1 do
    begin
      // Appeler base de données d'interactions
      // Simplification ici
    end;
end;

function TPrescription.VerifierAllergies: TStringList;  
var
  i, j: Integer;
  Ligne: TLignePrescription;
begin
  Result := TStringList.Create;

  if not Assigned(FPatient) then
    Exit;

  // Vérifier chaque médicament contre les allergies
  for i := 0 to FLignes.Count - 1 do
  begin
    Ligne := TLignePrescription(FLignes[i]);
    for j := 0 to FPatient.Allergies.Count - 1 do
    begin
      if Pos(FPatient.Allergies[j], Ligne.Medicament.Nom) > 0 then
        Result.Add('ALLERGIE: ' + Ligne.Medicament.Nom +
          ' / ' + FPatient.Allergies[j]);
    end;
  end;
end;

end.
```

### Planning et rendez-vous

Un module de gestion du planning est essentiel :

```pascal
unit UPlanningManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, UHospitalModel, UDatabaseConnection, SQLDB;

type
  TRendezVous = class
  private
    FIdRdv: Integer;
    FPatient: TPatient;
    FPersonnel: TPersonnel;
    FDateRdv: TDateTime;
    FDureeMinutes: Integer;
    FTypeRdv: string;
    FStatut: string;
  public
    property IdRdv: Integer read FIdRdv write FIdRdv;
    property DateRdv: TDateTime read FDateRdv write FDateRdv;
    property DureeMinutes: Integer read FDureeMinutes write FDureeMinutes;
  end;

  TPlanningManager = class
  private
    FDatabase: TDatabaseConnection;
  public
    constructor Create(ADatabase: TDatabaseConnection);

    function GetDisponibilites(AIdPersonnel: Integer; ADate: TDate): TList;
    function CreerRendezVous(ARdv: TRendezVous): Boolean;
    function AnnulerRendezVous(AIdRdv: Integer): Boolean;
    function GetPlanningJour(AIdPersonnel: Integer; ADate: TDate): TList;
    function EstDisponible(AIdPersonnel: Integer; ADebut: TDateTime;
      ADuree: Integer): Boolean;
  end;

implementation

constructor TPlanningManager.Create(ADatabase: TDatabaseConnection);  
begin
  inherited Create;
  FDatabase := ADatabase;
end;

function TPlanningManager.EstDisponible(AIdPersonnel: Integer;
  ADebut: TDateTime; ADuree: Integer): Boolean;
var
  Query: TSQLQuery;
  AFin: TDateTime;
begin
  Result := True;
  AFin := IncMinute(ADebut, ADuree);

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabase.Database;
    Query.Transaction := FDatabase.Transaction;

    // Vérifier chevauchements
    Query.SQL.Text :=
      'SELECT COUNT(*) as nb FROM rendez_vous ' +
      'WHERE id_personnel = :id ' +
      'AND statut != ''Annulé'' ' +
      'AND (' +
      '  (date_rdv < :fin AND ' +
      '   date_rdv + (duree_minutes || '' minutes'')::interval > :debut)' +
      ')';

    Query.ParamByName('id').AsInteger := AIdPersonnel;
    Query.ParamByName('debut').AsDateTime := ADebut;
    Query.ParamByName('fin').AsDateTime := AFin;
    Query.Open;

    Result := Query.FieldByName('nb').AsInteger = 0;
    Query.Close;
  finally
    Query.Free;
  end;
end;

end.
```

## Conformité RGPD

### Respect de la vie privée

Le SGH doit être conforme au RGPD :

**Principes à implémenter :**

1. **Consentement explicite** : le patient doit accepter le traitement de ses données
2. **Droit d'accès** : le patient peut consulter ses données
3. **Droit à l'oubli** : possibilité de supprimer les données (avec contraintes légales médicales)
4. **Portabilité** : export des données en format standard
5. **Sécurité** : chiffrement, accès contrôlé
6. **Traçabilité** : qui accède à quoi, quand

```pascal
unit URGPDManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UDatabaseConnection, UHospitalModel;

type
  TRGPDManager = class
  private
    FDatabase: TDatabaseConnection;
  public
    constructor Create(ADatabase: TDatabaseConnection);

    function EnregistrerConsentement(AIdPatient: Integer;
      ATypeTraitement: string): Boolean;
    function ExporterDonneesPatient(AIdPatient: Integer): string; // JSON
    function AnonymiserPatient(AIdPatient: Integer): Boolean;
    function SupprimerDonneesPatient(AIdPatient: Integer): Boolean;
    function GetHistoriqueAcces(AIdPatient: Integer): TStringList;
  end;

implementation

uses fpjson, jsonparser;

constructor TRGPDManager.Create(ADatabase: TDatabaseConnection);  
begin
  inherited Create;
  FDatabase := ADatabase;
end;

function TRGPDManager.ExporterDonneesPatient(AIdPatient: Integer): string;  
var
  JSON: TJSONObject;
  Query: TSQLQuery;
begin
  JSON := TJSONObject.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabase.Database;
    Query.Transaction := FDatabase.Transaction;

    // Exporter données patient
    Query.SQL.Text := 'SELECT * FROM patients WHERE id_patient = :id';
    Query.ParamByName('id').AsInteger := AIdPatient;
    Query.Open;

    if not Query.EOF then
    begin
      JSON.Add('patient', TJSONObject.Create([
        'nom', Query.FieldByName('nom').AsString,
        'prenom', Query.FieldByName('prenom').AsString,
        'date_naissance', DateToStr(Query.FieldByName('date_naissance').AsDateTime)
        // etc.
      ]));
    end;

    // Exporter consultations, prescriptions, etc.
    // ...

    Result := JSON.FormatJSON;
  finally
    JSON.Free;
    Query.Free;
  end;
end;

function TRGPDManager.AnonymiserPatient(AIdPatient: Integer): Boolean;  
var
  Query: TSQLQuery;
begin
  Result := False;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabase.Database;
    Query.Transaction := FDatabase.Transaction;

    // Anonymiser les données personnelles
    Query.SQL.Text :=
      'UPDATE patients SET ' +
      '  nom = ''ANONYME'', ' +
      '  prenom = ''ANONYME'', ' +
      '  adresse = NULL, ' +
      '  telephone = NULL, ' +
      '  email = NULL, ' +
      '  numero_secu = NULL ' +
      'WHERE id_patient = :id';

    Query.ParamByName('id').AsInteger := AIdPatient;
    Query.ExecSQL;
    FDatabase.Transaction.Commit;
    Result := True;
  finally
    Query.Free;
  end;
end;

end.
```

## Déploiement multi-plateforme

### Configuration Windows

**Étapes de déploiement Windows :**

1. **Compilation** :
```bash
lazbuild --build-mode=Release SGHopital.lpi
```

2. **Dépendances** :
   - PostgreSQL client (libpq.dll)
   - OpenSSL (libeay32.dll, ssleay32.dll)
   - DLLs système

3. **Service Windows** :
```pascal
// Créer un service Windows
unit USGHService;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows, JwaSvc,
  {$ENDIF}
  Classes, SysUtils;

type
  TSGHService = class
  private
    {$IFDEF WINDOWS}
    FServiceStatus: SERVICE_STATUS;
    FServiceStatusHandle: SERVICE_STATUS_HANDLE;
    {$ENDIF}
  public
    procedure Start;
    procedure Stop;
    procedure Install;
    procedure Uninstall;
  end;

implementation

// Implémentation spécifique Windows...
```

4. **Installateur** (Inno Setup) :
```
[Setup]
AppName=Système Gestion Hospitalière  
AppVersion=1.0  
DefaultDirName={pf}\SGHopital  
DefaultGroupName=SGH

[Files]
Source: "SGHopital.exe"; DestDir: "{app}"  
Source: "libpq.dll"; DestDir: "{app}"  
Source: "config.ini"; DestDir: "{app}"; Flags: onlyifdoesntexist

[Icons]
Name: "{group}\SGH Hopital"; Filename: "{app}\SGHopital.exe"

[Run]
Filename: "{app}\SGHopital.exe"; Parameters: "--install-service"
```

### Configuration Ubuntu

**Étapes de déploiement Ubuntu :**

1. **Compilation** :
```bash
lazbuild --build-mode=Release SGHopital.lpi
```

2. **Dépendances** :
```bash
sudo apt install libpq5 libssl1.1 libgtk2.0-0
```

3. **Service systemd** :
Créer `/etc/systemd/system/sgh-hopital.service` :
```ini
[Unit]
Description=Système de Gestion Hospitalière  
After=postgresql.service network.target

[Service]
Type=simple  
User=sgh  
Group=sgh  
WorkingDirectory=/opt/sgh-hopital  
ExecStart=/opt/sgh-hopital/SGHopital --server-mode  
Restart=always  
RestartSec=10

[Install]
WantedBy=multi-user.target
```

Activer :
```bash
sudo systemctl enable sgh-hopital  
sudo systemctl start sgh-hopital
```

4. **Paquet DEB** :
Structure :
```
sgh-hopital_1.0-1/
├── DEBIAN/
│   ├── control
│   ├── postinst
│   └── prerm
├── opt/sgh-hopital/
│   ├── SGHopital
│   └── config.ini
└── etc/systemd/system/
    └── sgh-hopital.service
```

Fichier `control` :
```
Package: sgh-hopital  
Version: 1.0-1  
Section: medical  
Priority: optional  
Architecture: amd64  
Depends: libpq5, libssl1.1, libgtk2.0-0  
Maintainer: Votre Nom <email@example.com>  
Description: Système de Gestion Hospitalière
 Application complète de gestion d'établissement hospitalier
```

Créer le paquet :
```bash
dpkg-deb --build sgh-hopital_1.0-1
```

## Performance et optimisation

### Optimisation des requêtes

Pour un système utilisé par de nombreux utilisateurs simultanément :

```pascal
// Utiliser des requêtes préparées
procedure TPatientDAO.PrepareStatements;  
begin
  FQuerySelect.SQL.Text := 'SELECT * FROM patients WHERE id_patient = $1';
  FQuerySelect.Prepare;

  FQueryInsert.SQL.Text :=
    'INSERT INTO patients (nom, prenom, ...) VALUES ($1, $2, ...)';
  FQueryInsert.Prepare;
end;

// Utiliser des transactions pour les opérations groupées
procedure TConsultationManager.EnregistrerConsultationComplete(
  AConsultation: TConsultation; APrescription: TPrescription);
begin
  FDatabase.Transaction.StartTransaction;
  try
    // Enregistrer consultation
    SaveConsultation(AConsultation);

    // Enregistrer prescription
    SavePrescription(APrescription);

    // Créer les lignes de facturation
    CreateFacture(AConsultation);

    FDatabase.Transaction.Commit;
  except
    FDatabase.Transaction.Rollback;
    raise;
  end;
end;
```

### Mise en cache

Implémenter un système de cache pour les données fréquemment consultées :

```pascal
unit UCacheManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TCacheEntry = class
    Key: string;
    Data: TObject;
    Timestamp: TDateTime;
    HitCount: Integer;
  end;

  TCacheManager = class
  private
    FCache: TFPHashList;
    FMaxSize: Integer;
    FTTLSeconds: Integer;
  public
    constructor Create(AMaxSize: Integer = 1000; ATTL: Integer = 300);
    destructor Destroy; override;

    procedure Add(AKey: string; AData: TObject);
    function Get(AKey: string): TObject;
    procedure Clear;
    procedure Remove(AKey: string);
  end;

implementation

// Implémentation du cache...
```

### Connexion pool

Pour gérer efficacement les connexions multiples :

```pascal
unit UConnectionPool;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UDatabaseConnection, SyncObjs;

type
  TConnectionPool = class
  private
    FConnections: TList;
    FAvailableConnections: TList;
    FLock: TCriticalSection;
    FMaxConnections: Integer;
  public
    constructor Create(AMaxConnections: Integer = 10);
    destructor Destroy; override;

    function AcquireConnection: TDatabaseConnection;
    procedure ReleaseConnection(AConnection: TDatabaseConnection);
  end;

implementation

// Pool de connexions pour améliorer performance...
```

## Tests et validation

### Tests unitaires

```pascal
unit UPatientDAOTests;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  UPatientDAO, UDatabaseConnection, UHospitalModel;

type
  TPatientDAOTest = class(TTestCase)
  private
    FDatabase: TDatabaseConnection;
    FDAO: TPatientDAO;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInsertPatient;
    procedure TestUpdatePatient;
    procedure TestSearchPatient;
    procedure TestDeletePatient;
  end;

implementation

procedure TPatientDAOTest.SetUp;  
begin
  // Connexion base de test
  FDatabase := TDatabaseConnection.Create;
  FDatabase.Connect('localhost', 'hopital_test', 'test', 'test');
  FDAO := TPatientDAO.Create(FDatabase);
end;

procedure TPatientDAOTest.TearDown;  
begin
  FDAO.Free;
  FDatabase.Free;
end;

procedure TPatientDAOTest.TestInsertPatient;  
var
  Patient: TPatient;
  Id: Integer;
begin
  Patient := TPatient.Create;
  try
    Patient.Nom := 'DUPONT';
    Patient.Prenom := 'Jean';
    Patient.DateNaissance := EncodeDate(1980, 1, 15);
    Patient.Sexe := 'M';

    Id := FDAO.Insert(Patient);
    AssertTrue('ID doit être > 0', Id > 0);
  finally
    Patient.Free;
  end;
end;

initialization
  RegisterTest(TPatientDAOTest);

end.
```

## Conclusion

La réalisation d'un système de gestion hospitalière complet avec FreePascal et Lazarus est un projet ambitieux qui met en œuvre l'ensemble des compétences d'un développeur avancé :

**Points clés réussis :**
- Architecture multi-couches robuste et maintenable
- Portabilité Windows/Ubuntu totale
- Sécurité et conformité RGPD
- Performance optimisée pour usage intensif
- Interface utilisateur complète et ergonomique

**Défis techniques relevés :**
- Gestion de données sensibles avec traçabilité complète
- Système multi-utilisateurs concurrent
- Intégration avec systèmes externes (laboratoires, imagerie)
- Règles métier complexes (interactions médicamenteuses, allergies)
- Haute disponibilité et fiabilité

**Extensions possibles :**
- Module d'imagerie médicale (DICOM)
- Intégration HL7 pour échanges inter-établissements
- Application mobile pour personnel soignant
- Télémédecine et visioconférences
- Intelligence artificielle pour aide au diagnostic
- Blockchain pour traçabilité des prescriptions

Ce projet démontre la capacité de FreePascal/Lazarus à réaliser des applications professionnelles critiques, multi-plateformes et hautement sécurisées dans le domaine médical.

⏭️ [Communauté et Écosystème](/26-communaute-ecosysteme/README.md)
