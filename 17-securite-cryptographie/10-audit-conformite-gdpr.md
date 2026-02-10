🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.10 Audit et conformité GDPR

## Introduction

Le RGPD (Règlement Général sur la Protection des Données) ou GDPR (General Data Protection Regulation) est une réglementation européenne qui régit le traitement des données personnelles. Pour les développeurs FreePascal/Lazarus, implémenter la conformité GDPR et mettre en place des mécanismes d'audit est devenu essentiel, que vous développiez pour Windows ou Ubuntu.

### Qu'est-ce que le GDPR ?

Le GDPR est une loi européenne entrée en vigueur le 25 mai 2018 qui protège les données personnelles des citoyens de l'Union Européenne. Elle s'applique à toute organisation qui traite des données de résidents européens, peu importe où l'organisation est située.

**Données personnelles** : Toute information se rapportant à une personne physique identifiée ou identifiable :
- Nom, prénom
- Adresse email, numéro de téléphone
- Adresse IP
- Identifiants en ligne
- Données de localisation
- Données de santé, financières, etc.

## Principes fondamentaux du GDPR

### 1. Licéité du traitement

Le traitement des données doit reposer sur une base légale :
- **Consentement** : L'utilisateur a donné son accord explicite
- **Contrat** : Nécessaire pour exécuter un contrat
- **Obligation légale** : Imposé par la loi
- **Intérêt vital** : Protection de la vie d'une personne
- **Intérêt public** : Mission d'intérêt public
- **Intérêt légitime** : Intérêts légitimes de l'organisation

### 2. Minimisation des données

Ne collecter que les données strictement nécessaires à la finalité du traitement.

```pascal
// ❌ Mauvais : Collecte excessive
type
  TUserProfile = record
    Name: string;
    Email: string;
    Phone: string;
    Address: string;
    BirthDate: TDateTime;
    SocialSecurityNumber: string;  // Pas nécessaire !
    BankAccount: string;            // Pas nécessaire !
    FavoriteColor: string;          // Pas pertinent !
  end;

// ✅ Bon : Données minimales
type
  TUserProfile = record
    Email: string;        // Nécessaire pour se connecter
    DisplayName: string;  // Nécessaire pour l'affichage
  end;
```

### 3. Limitation de la conservation

Les données ne doivent être conservées que le temps nécessaire.

```pascal
const
  // Durées de conservation recommandées
  RETENTION_USER_ACCOUNT_DAYS = 1095;      // 3 ans
  RETENTION_SESSION_LOGS_DAYS = 90;        // 3 mois
  RETENTION_AUDIT_LOGS_DAYS = 2190;        // 6 ans (obligation légale)
  RETENTION_INACTIVE_ACCOUNT_DAYS = 730;   // 2 ans
```

### 4. Exactitude

Les données doivent être exactes et mises à jour.

### 5. Intégrité et confidentialité

Protection des données par des mesures techniques et organisationnelles appropriées.

### 6. Transparence

Les utilisateurs doivent être informés clairement de l'utilisation de leurs données.

## Droits des utilisateurs

Le GDPR accorde plusieurs droits aux utilisateurs que votre application doit respecter :

### 1. Droit d'accès

L'utilisateur peut demander une copie de ses données.

```pascal
unit GDPRDataAccess;

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  TDataExportFormat = (defJSON, defXML, defCSV);

  TGDPRDataExporter = class
  private
    FUserID: Integer;
    FDatabaseConnection: TConnection;  // Votre connexion DB
  public
    constructor Create(UserID: Integer; DBConnection: TConnection);

    function ExportUserData(Format: TDataExportFormat): string;
    function GenerateDataPortabilityFile(const OutputPath: string): Boolean;
  end;

implementation

constructor TGDPRDataExporter.Create(UserID: Integer; DBConnection: TConnection);
begin
  inherited Create;
  FUserID := UserID;
  FDatabaseConnection := DBConnection;
end;

function TGDPRDataExporter.ExportUserData(Format: TDataExportFormat): string;
var
  JSONData: TJSONObject;
  Query: TSQLQuery;
begin
  Result := '';

  JSONData := TJSONObject.Create;
  try
    // Informations de base
    Query := TSQLQuery.Create(nil);
    try
      Query.Database := FDatabaseConnection;
      Query.SQL.Text := 'SELECT * FROM users WHERE user_id = :userid';
      Query.ParamByName('userid').AsInteger := FUserID;
      Query.Open;

      if not Query.EOF then
      begin
        JSONData.Add('user_id', Query.FieldByName('user_id').AsInteger);
        JSONData.Add('email', Query.FieldByName('email').AsString);
        JSONData.Add('name', Query.FieldByName('name').AsString);
        JSONData.Add('created_at', Query.FieldByName('created_at').AsString);
        JSONData.Add('last_login', Query.FieldByName('last_login').AsString);
      end;

      Query.Close;
    finally
      Query.Free;
    end;

    // Historique des commandes
    Query := TSQLQuery.Create(nil);
    try
      Query.Database := FDatabaseConnection;
      Query.SQL.Text := 'SELECT * FROM orders WHERE user_id = :userid';
      Query.ParamByName('userid').AsInteger := FUserID;
      Query.Open;

      // Ajouter au JSON...

      Query.Close;
    finally
      Query.Free;
    end;

    // Autres données pertinentes...

    case Format of
      defJSON: Result := JSONData.FormatJSON;
      defXML:  Result := JSONToXML(JSONData);
      defCSV:  Result := JSONToCSV(JSONData);
    end;

  finally
    JSONData.Free;
  end;
end;

function TGDPRDataExporter.GenerateDataPortabilityFile(const OutputPath: string): Boolean;
var
  FileStream: TFileStream;
  JSONContent: string;
begin
  Result := False;

  try
    JSONContent := ExportUserData(defJSON);

    FileStream := TFileStream.Create(OutputPath, fmCreate);
    try
      FileStream.Write(JSONContent[1], Length(JSONContent));
      Result := True;
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur export données : ', E.Message);
  end;
end;

end.
```

### 2. Droit de rectification

L'utilisateur peut corriger ses données inexactes.

```pascal
unit GDPRDataRectification;

interface

uses
  Classes, SysUtils, sqldb;

type
  TDataRectificationRequest = record
    UserID: Integer;
    FieldName: string;
    OldValue: string;
    NewValue: string;
    RequestDate: TDateTime;
    ProcessedDate: TDateTime;
    Status: string;  // 'pending', 'approved', 'rejected'
  end;

  TGDPRRectificationManager = class
  private
    FDatabaseConnection: TConnection;
    function ValidateField(const FieldName: string): Boolean;
    function LogRectification(const Request: TDataRectificationRequest): Boolean;
  public
    constructor Create(DBConnection: TConnection);

    function RequestRectification(UserID: Integer; const FieldName, NewValue: string): Boolean;
    function ProcessRectification(const Request: TDataRectificationRequest): Boolean;
  end;

implementation

constructor TGDPRRectificationManager.Create(DBConnection: TConnection);
begin
  inherited Create;
  FDatabaseConnection := DBConnection;
end;

function TGDPRRectificationManager.ValidateField(const FieldName: string): Boolean;
const
  ALLOWED_FIELDS: array[0..4] of string = (
    'name', 'email', 'phone', 'address', 'preferences'
  );
var
  i: Integer;
begin
  Result := False;
  for i := Low(ALLOWED_FIELDS) to High(ALLOWED_FIELDS) do
  begin
    if FieldName = ALLOWED_FIELDS[i] then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TGDPRRectificationManager.RequestRectification(UserID: Integer;
  const FieldName, NewValue: string): Boolean;
var
  Request: TDataRectificationRequest;
begin
  Result := False;

  // Valider le champ
  if not ValidateField(FieldName) then
  begin
    WriteLn('Champ non autorisé pour rectification : ', FieldName);
    Exit;
  end;

  // Créer la demande
  Request.UserID := UserID;
  Request.FieldName := FieldName;
  Request.NewValue := NewValue;
  Request.RequestDate := Now;
  Request.Status := 'pending';

  // Enregistrer dans la base
  Result := LogRectification(Request);

  if Result then
    WriteLn('Demande de rectification enregistrée pour user_id: ', UserID);
end;

function TGDPRRectificationManager.LogRectification(
  const Request: TDataRectificationRequest): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'INSERT INTO rectification_requests ' +
      '(user_id, field_name, new_value, request_date, status) ' +
      'VALUES (:userid, :field, :value, :reqdate, :status)';

    Query.ParamByName('userid').AsInteger := Request.UserID;
    Query.ParamByName('field').AsString := Request.FieldName;
    Query.ParamByName('value').AsString := Request.NewValue;
    Query.ParamByName('reqdate').AsDateTime := Request.RequestDate;
    Query.ParamByName('status').AsString := Request.Status;

    Query.ExecSQL;
    Result := True;
  except
    on E: Exception do
      WriteLn('Erreur enregistrement rectification : ', E.Message);
  end;

  Query.Free;
end;

function TGDPRRectificationManager.ProcessRectification(
  const Request: TDataRectificationRequest): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;

  Query := TSQLQuery.Create(nil);
  try
    // Mettre à jour la donnée
    Query.Database := FDatabaseConnection;
    Query.SQL.Text := Format(
      'UPDATE users SET %s = :newvalue WHERE user_id = :userid',
      [Request.FieldName]
    );
    Query.ParamByName('newvalue').AsString := Request.NewValue;
    Query.ParamByName('userid').AsInteger := Request.UserID;
    Query.ExecSQL;

    // Marquer la demande comme traitée
    Query.SQL.Text :=
      'UPDATE rectification_requests SET status = ''approved'', ' +
      'processed_date = :procdate WHERE user_id = :userid AND field_name = :field';
    Query.ParamByName('procdate').AsDateTime := Now;
    Query.ParamByName('userid').AsInteger := Request.UserID;
    Query.ParamByName('field').AsString := Request.FieldName;
    Query.ExecSQL;

    Result := True;
  except
    on E: Exception do
      WriteLn('Erreur traitement rectification : ', E.Message);
  end;

  Query.Free;
end;

end.
```

### 3. Droit à l'effacement ("droit à l'oubli")

L'utilisateur peut demander la suppression de ses données.

```pascal
unit GDPRDataErasure;

interface

uses
  Classes, SysUtils, sqldb;

type
  TErasureMethod = (
    emSoftDelete,      // Marquage comme supprimé
    emHardDelete,      // Suppression physique
    emAnonymization    // Anonymisation
  );

  TGDPRDataEraser = class
  private
    FDatabaseConnection: TConnection;
    function AnonymizeUser(UserID: Integer): Boolean;
    function DeleteUserData(UserID: Integer): Boolean;
    function LogErasure(UserID: Integer; Method: TErasureMethod): Boolean;
  public
    constructor Create(DBConnection: TConnection);

    function EraseUserData(UserID: Integer; Method: TErasureMethod): Boolean;
    function CanEraseUser(UserID: Integer; out Reason: string): Boolean;
  end;

implementation

constructor TGDPRDataEraser.Create(DBConnection: TConnection);
begin
  inherited Create;
  FDatabaseConnection := DBConnection;
end;

function TGDPRDataEraser.CanEraseUser(UserID: Integer; out Reason: string): Boolean;
var
  Query: TSQLQuery;
  HasActiveOrders: Boolean;
  HasLegalObligation: Boolean;
begin
  Result := True;
  Reason := '';

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;

    // Vérifier les commandes en cours
    Query.SQL.Text :=
      'SELECT COUNT(*) as cnt FROM orders ' +
      'WHERE user_id = :userid AND status IN (''pending'', ''processing'')';
    Query.ParamByName('userid').AsInteger := UserID;
    Query.Open;

    HasActiveOrders := Query.FieldByName('cnt').AsInteger > 0;
    Query.Close;

    if HasActiveOrders then
    begin
      Result := False;
      Reason := 'L''utilisateur a des commandes en cours';
      Exit;
    end;

    // Vérifier obligations légales (ex: factures fiscales)
    Query.SQL.Text :=
      'SELECT COUNT(*) as cnt FROM invoices ' +
      'WHERE user_id = :userid AND created_date > :cutoff_date';
    Query.ParamByName('userid').AsInteger := UserID;
    // Obligation de conserver 10 ans en France
    Query.ParamByName('cutoff_date').AsDateTime := IncYear(Now, -10);
    Query.Open;

    HasLegalObligation := Query.FieldByName('cnt').AsInteger > 0;
    Query.Close;

    if HasLegalObligation then
    begin
      Result := False;
      Reason := 'Conservation requise pour obligations légales/comptables';
      Exit;
    end;

  finally
    Query.Free;
  end;
end;

function TGDPRDataEraser.EraseUserData(UserID: Integer;
  Method: TErasureMethod): Boolean;
var
  CanErase: Boolean;
  Reason: string;
begin
  Result := False;

  // Vérifier si l'effacement est possible
  CanErase := CanEraseUser(UserID, Reason);
  if not CanErase then
  begin
    WriteLn('Impossible d''effacer l''utilisateur : ', Reason);
    Exit;
  end;

  case Method of
    emSoftDelete:
      begin
        // Marquer comme supprimé sans effacer
        Result := SoftDeleteUser(UserID);
      end;

    emHardDelete:
      begin
        // Supprimer physiquement
        Result := DeleteUserData(UserID);
      end;

    emAnonymization:
      begin
        // Anonymiser les données
        Result := AnonymizeUser(UserID);
      end;
  end;

  if Result then
    LogErasure(UserID, Method);
end;

function TGDPRDataEraser.AnonymizeUser(UserID: Integer): Boolean;
var
  Query: TSQLQuery;
  AnonymousEmail, AnonymousName: string;
begin
  Result := False;

  // Générer des données anonymes
  AnonymousEmail := Format('deleted_%d@anonymized.local', [UserID]);
  AnonymousName := Format('Utilisateur supprimé %d', [UserID]);

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;

    // Anonymiser les données personnelles
    Query.SQL.Text :=
      'UPDATE users SET ' +
      'email = :email, ' +
      'name = :name, ' +
      'phone = NULL, ' +
      'address = NULL, ' +
      'birth_date = NULL, ' +
      'gdpr_anonymized = 1, ' +
      'anonymized_date = :anondate ' +
      'WHERE user_id = :userid';

    Query.ParamByName('email').AsString := AnonymousEmail;
    Query.ParamByName('name').AsString := AnonymousName;
    Query.ParamByName('anondate').AsDateTime := Now;
    Query.ParamByName('userid').AsInteger := UserID;

    Query.ExecSQL;
    Result := True;

    WriteLn('Utilisateur ', UserID, ' anonymisé avec succès');
  except
    on E: Exception do
      WriteLn('Erreur anonymisation : ', E.Message);
  end;

  Query.Free;
end;

function TGDPRDataEraser.DeleteUserData(UserID: Integer): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;

    // Supprimer dans l'ordre (contraintes FK)

    // 1. Sessions
    Query.SQL.Text := 'DELETE FROM sessions WHERE user_id = :userid';
    Query.ParamByName('userid').AsInteger := UserID;
    Query.ExecSQL;

    // 2. Préférences
    Query.SQL.Text := 'DELETE FROM user_preferences WHERE user_id = :userid';
    Query.ParamByName('userid').AsInteger := UserID;
    Query.ExecSQL;

    // 3. Logs non critiques
    Query.SQL.Text := 'DELETE FROM user_activity_logs WHERE user_id = :userid';
    Query.ParamByName('userid').AsInteger := UserID;
    Query.ExecSQL;

    // 4. Utilisateur principal
    Query.SQL.Text := 'DELETE FROM users WHERE user_id = :userid';
    Query.ParamByName('userid').AsInteger := UserID;
    Query.ExecSQL;

    Result := True;
    WriteLn('Utilisateur ', UserID, ' supprimé physiquement');

  except
    on E: Exception do
      WriteLn('Erreur suppression : ', E.Message);
  end;

  Query.Free;
end;

function TGDPRDataEraser.LogErasure(UserID: Integer;
  Method: TErasureMethod): Boolean;
var
  Query: TSQLQuery;
  MethodStr: string;
begin
  Result := False;

  case Method of
    emSoftDelete: MethodStr := 'soft_delete';
    emHardDelete: MethodStr := 'hard_delete';
    emAnonymization: MethodStr := 'anonymization';
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'INSERT INTO gdpr_erasure_log ' +
      '(user_id, erasure_method, erasure_date, performed_by) ' +
      'VALUES (:userid, :method, :erasdate, :performer)';

    Query.ParamByName('userid').AsInteger := UserID;
    Query.ParamByName('method').AsString := MethodStr;
    Query.ParamByName('erasdate').AsDateTime := Now;
    Query.ParamByName('performer').AsString := 'system';  // Ou ID admin

    Query.ExecSQL;
    Result := True;
  except
    on E: Exception do
      WriteLn('Erreur log effacement : ', E.Message);
  end;

  Query.Free;
end;

end.
```

### 4. Droit à la limitation du traitement

L'utilisateur peut demander de limiter l'utilisation de ses données.

```pascal
unit GDPRDataRestriction;

interface

uses
  Classes, SysUtils, sqldb;

type
  TRestrictionReason = (
    rrContestAccuracy,      // Conteste l'exactitude
    rrUnlawfulProcessing,   // Traitement illicite
    rrNoLongerNeeded,       // Données plus nécessaires
    rrObjection             // Opposition au traitement
  );

  TGDPRRestrictionManager = class
  private
    FDatabaseConnection: TConnection;
    function ApplyRestriction(UserID: Integer): Boolean;
    function LogRestriction(UserID: Integer; Reason: TRestrictionReason): Boolean;
  public
    constructor Create(DBConnection: TConnection);

    function RestrictProcessing(UserID: Integer; Reason: TRestrictionReason): Boolean;
    function LiftRestriction(UserID: Integer): Boolean;
    function IsRestricted(UserID: Integer): Boolean;
  end;

implementation

constructor TGDPRRestrictionManager.Create(DBConnection: TConnection);
begin
  inherited Create;
  FDatabaseConnection := DBConnection;
end;

function TGDPRRestrictionManager.RestrictProcessing(UserID: Integer;
  Reason: TRestrictionReason): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'UPDATE users SET ' +
      'processing_restricted = 1, ' +
      'restriction_date = :restdate ' +
      'WHERE user_id = :userid';

    Query.ParamByName('restdate').AsDateTime := Now;
    Query.ParamByName('userid').AsInteger := UserID;
    Query.ExecSQL;

    Result := LogRestriction(UserID, Reason);

    if Result then
      WriteLn('Traitement restreint pour user_id: ', UserID);
  except
    on E: Exception do
      WriteLn('Erreur restriction : ', E.Message);
  end;

  Query.Free;
end;

function TGDPRRestrictionManager.LiftRestriction(UserID: Integer): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'UPDATE users SET ' +
      'processing_restricted = 0, ' +
      'restriction_lifted_date = :liftdate ' +
      'WHERE user_id = :userid';

    Query.ParamByName('liftdate').AsDateTime := Now;
    Query.ParamByName('userid').AsInteger := UserID;
    Query.ExecSQL;

    Result := True;
    WriteLn('Restriction levée pour user_id: ', UserID);
  except
    on E: Exception do
      WriteLn('Erreur levée restriction : ', E.Message);
  end;

  Query.Free;
end;

function TGDPRRestrictionManager.IsRestricted(UserID: Integer): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'SELECT processing_restricted FROM users WHERE user_id = :userid';
    Query.ParamByName('userid').AsInteger := UserID;
    Query.Open;

    if not Query.EOF then
      Result := Query.FieldByName('processing_restricted').AsBoolean;

    Query.Close;
  finally
    Query.Free;
  end;
end;

function TGDPRRestrictionManager.LogRestriction(UserID: Integer;
  Reason: TRestrictionReason): Boolean;
var
  Query: TSQLQuery;
  ReasonStr: string;
begin
  case Reason of
    rrContestAccuracy: ReasonStr := 'contest_accuracy';
    rrUnlawfulProcessing: ReasonStr := 'unlawful_processing';
    rrNoLongerNeeded: ReasonStr := 'no_longer_needed';
    rrObjection: ReasonStr := 'objection';
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'INSERT INTO gdpr_restriction_log ' +
      '(user_id, reason, restriction_date) ' +
      'VALUES (:userid, :reason, :restdate)';

    Query.ParamByName('userid').AsInteger := UserID;
    Query.ParamByName('reason').AsString := ReasonStr;
    Query.ParamByName('restdate').AsDateTime := Now;

    Query.ExecSQL;
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Erreur log restriction : ', E.Message);
      Result := False;
    end;
  end;

  Query.Free;
end;

end.
```

### 5. Droit à la portabilité

L'utilisateur peut récupérer ses données dans un format structuré et les transférer.

### 6. Droit d'opposition

L'utilisateur peut s'opposer au traitement de ses données (marketing, profilage).

## Gestion du consentement

Le consentement doit être libre, spécifique, éclairé et univoque.

```pascal
unit GDPRConsent;

interface

uses
  Classes, SysUtils, sqldb;

type
  TConsentPurpose = (
    cpProcessing,        // Traitement des données
    cpMarketing,         // Marketing
    cpProfiling,         // Profilage
    cpThirdPartySharing, // Partage avec tiers
    cpAnalytics          // Analyse/statistiques
  );

  TConsentPurposes = set of TConsentPurpose;

  TConsentRecord = record
    UserID: Integer;
    Purpose: TConsentPurpose;
    Granted: Boolean;
    ConsentDate: TDateTime;
    WithdrawnDate: TDateTime;
    IPAddress: string;
    UserAgent: string;
  end;

  TGDPRConsentManager = class
  private
    FDatabaseConnection: TConnection;
    function LogConsent(const Consent: TConsentRecord): Boolean;
  public
    constructor Create(DBConnection: TConnection);

    function GrantConsent(UserID: Integer; Purpose: TConsentPurpose;
      const IPAddress, UserAgent: string): Boolean;
    function WithdrawConsent(UserID: Integer; Purpose: TConsentPurpose): Boolean;
    function HasConsent(UserID: Integer; Purpose: TConsentPurpose): Boolean;
    function GetConsentHistory(UserID: Integer): TStringList;
  end;

implementation

constructor TGDPRConsentManager.Create(DBConnection: TConnection);
begin
  inherited Create;
  FDatabaseConnection := DBConnection;
end;

function TGDPRConsentManager.GrantConsent(UserID: Integer;
  Purpose: TConsentPurpose; const IPAddress, UserAgent: string): Boolean;
var
  Consent: TConsentRecord;
begin
  Consent.UserID := UserID;
  Consent.Purpose := Purpose;
  Consent.Granted := True;
  Consent.ConsentDate := Now;
  Consent.IPAddress := IPAddress;
  Consent.UserAgent := UserAgent;

  Result := LogConsent(Consent);

  if Result then
    WriteLn('Consentement accordé pour user_id: ', UserID);
end;

function TGDPRConsentManager.WithdrawConsent(UserID: Integer;
  Purpose: TConsentPurpose): Boolean;
var
  Query: TSQLQuery;
  PurposeStr: string;
begin
  Result := False;

  case Purpose of
    cpProcessing: PurposeStr := 'processing';
    cpMarketing: PurposeStr := 'marketing';
    cpProfiling: PurposeStr := 'profiling';
    cpThirdPartySharing: PurposeStr := 'third_party_sharing';
    cpAnalytics: PurposeStr := 'analytics';
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'UPDATE consent_log SET ' +
      'withdrawn = 1, ' +
      'withdrawn_date = :withdate ' +
      'WHERE user_id = :userid AND purpose = :purpose AND withdrawn = 0';

    Query.ParamByName('withdate').AsDateTime := Now;
    Query.ParamByName('userid').AsInteger := UserID;
    Query.ParamByName('purpose').AsString := PurposeStr;

    Query.ExecSQL;
    Result := True;

    WriteLn('Consentement retiré pour user_id: ', UserID, ', purpose: ', PurposeStr);
  except
    on E: Exception do
      WriteLn('Erreur retrait consentement : ', E.Message);
  end;

  Query.Free;
end;

function TGDPRConsentManager.HasConsent(UserID: Integer;
  Purpose: TConsentPurpose): Boolean;
var
  Query: TSQLQuery;
  PurposeStr: string;
begin
  Result := False;

  case Purpose of
    cpProcessing: PurposeStr := 'processing';
    cpMarketing: PurposeStr := 'marketing';
    cpProfiling: PurposeStr := 'profiling';
    cpThirdPartySharing: PurposeStr := 'third_party_sharing';
    cpAnalytics: PurposeStr := 'analytics';
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'SELECT COUNT(*) as cnt FROM consent_log ' +
      'WHERE user_id = :userid AND purpose = :purpose ' +
      'AND withdrawn = 0';

    Query.ParamByName('userid').AsInteger := UserID;
    Query.ParamByName('purpose').AsString := PurposeStr;
    Query.Open;

    Result := Query.FieldByName('cnt').AsInteger > 0;
    Query.Close;
  finally
    Query.Free;
  end;
end;

function TGDPRConsentManager.GetConsentHistory(UserID: Integer): TStringList;
var
  Query: TSQLQuery;
begin
  Result := TStringList.Create;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'SELECT purpose, consent_date, withdrawn, withdrawn_date ' +
      'FROM consent_log WHERE user_id = :userid ' +
      'ORDER BY consent_date DESC';

    Query.ParamByName('userid').AsInteger := UserID;
    Query.Open;

    while not Query.EOF do
    begin
      Result.Add(Format('%s: %s (accordé: %s, retiré: %s)',
        [Query.FieldByName('purpose').AsString,
         DateTimeToStr(Query.FieldByName('consent_date').AsDateTime),
         DateTimeToStr(Query.FieldByName('consent_date').AsDateTime),
         DateTimeToStr(Query.FieldByName('withdrawn_date').AsDateTime)]));
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

function TGDPRConsentManager.LogConsent(const Consent: TConsentRecord): Boolean;
var
  Query: TSQLQuery;
  PurposeStr: string;
begin
  Result := False;

  case Consent.Purpose of
    cpProcessing: PurposeStr := 'processing';
    cpMarketing: PurposeStr := 'marketing';
    cpProfiling: PurposeStr := 'profiling';
    cpThirdPartySharing: PurposeStr := 'third_party_sharing';
    cpAnalytics: PurposeStr := 'analytics';
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'INSERT INTO consent_log ' +
      '(user_id, purpose, granted, consent_date, ip_address, user_agent) ' +
      'VALUES (:userid, :purpose, :granted, :consdate, :ip, :useragent)';

    Query.ParamByName('userid').AsInteger := Consent.UserID;
    Query.ParamByName('purpose').AsString := PurposeStr;
    Query.ParamByName('granted').AsBoolean := Consent.Granted;
    Query.ParamByName('consdate').AsDateTime := Consent.ConsentDate;
    Query.ParamByName('ip').AsString := Consent.IPAddress;
    Query.ParamByName('useragent').AsString := Consent.UserAgent;

    Query.ExecSQL;
    Result := True;
  except
    on E: Exception do
      WriteLn('Erreur log consentement : ', E.Message);
  end;

  Query.Free;
end;

end.
```

**Exemple d'utilisation du gestionnaire de consentement :**

```pascal
var
  ConsentManager: TGDPRConsentManager;
begin
  ConsentManager := TGDPRConsentManager.Create(DatabaseConnection);
  try
    // Accorder un consentement pour le marketing
    if ConsentManager.GrantConsent(
      UserID,
      cpMarketing,
      '192.168.1.100',
      'Mozilla/5.0...'
    ) then
      ShowMessage('Consentement enregistré');

    // Vérifier avant d'envoyer un email marketing
    if ConsentManager.HasConsent(UserID, cpMarketing) then
      SendMarketingEmail(UserID)
    else
      WriteLn('Utilisateur n''a pas consenti au marketing');

    // Retirer le consentement
    ConsentManager.WithdrawConsent(UserID, cpMarketing);
  finally
    ConsentManager.Free;
  end;
end;
```

## Système d'audit GDPR

Un système d'audit robuste est essentiel pour démontrer la conformité.

```pascal
unit GDPRAuditSystem;

interface

uses
  Classes, SysUtils, sqldb, DateUtils;

type
  TAuditEventType = (
    aetDataAccess,        // Accès aux données
    aetDataModification,  // Modification
    aetDataDeletion,      // Suppression
    aetConsentGranted,    // Consentement accordé
    aetConsentWithdrawn,  // Consentement retiré
    aetDataExport,        // Export de données
    aetSecurityBreach,    // Violation de sécurité
    aetAccessDenied       // Accès refusé
  );

  TAuditEvent = record
    EventID: Int64;
    EventType: TAuditEventType;
    UserID: Integer;
    PerformedBy: string;      // Utilisateur ou système
    EventDate: TDateTime;
    IPAddress: string;
    Details: string;
    DataBefore: string;       // État avant (si modif)
    DataAfter: string;        // État après (si modif)
    Severity: Integer;        // 1-5
  end;

  TGDPRAuditor = class
  private
    FDatabaseConnection: TConnection;
    FLogFilePath: string;
    procedure WriteToLogFile(const Event: TAuditEvent);
    function SanitizeForLog(const Data: string): string;
  public
    constructor Create(DBConnection: TConnection; const LogPath: string);

    function LogEvent(const Event: TAuditEvent): Boolean;
    function GetAuditTrail(UserID: Integer; StartDate, EndDate: TDateTime): TStringList;
    function GenerateAuditReport(StartDate, EndDate: TDateTime): string;
    function SearchAuditLog(const SearchTerm: string): TStringList;
  end;

implementation

constructor TGDPRAuditor.Create(DBConnection: TConnection; const LogPath: string);
begin
  inherited Create;
  FDatabaseConnection := DBConnection;
  FLogFilePath := LogPath;

  // Créer le répertoire de logs s'il n'existe pas
  if not DirectoryExists(ExtractFilePath(FLogFilePath)) then
    ForceDirectories(ExtractFilePath(FLogFilePath));
end;

function TGDPRAuditor.SanitizeForLog(const Data: string): string;
begin
  // Masquer les données sensibles dans les logs
  Result := Data;

  // Exemple : masquer les emails partiellement
  // user@example.com → u***@example.com

  // Masquer les numéros de carte
  // 1234-5678-9012-3456 → ****-****-****-3456

  // etc.
end;

function TGDPRAuditor.LogEvent(const Event: TAuditEvent): Boolean;
var
  Query: TSQLQuery;
  EventTypeStr: string;
begin
  Result := False;

  // Convertir le type d'événement en chaîne
  case Event.EventType of
    aetDataAccess: EventTypeStr := 'data_access';
    aetDataModification: EventTypeStr := 'data_modification';
    aetDataDeletion: EventTypeStr := 'data_deletion';
    aetConsentGranted: EventTypeStr := 'consent_granted';
    aetConsentWithdrawn: EventTypeStr := 'consent_withdrawn';
    aetDataExport: EventTypeStr := 'data_export';
    aetSecurityBreach: EventTypeStr := 'security_breach';
    aetAccessDenied: EventTypeStr := 'access_denied';
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'INSERT INTO gdpr_audit_log ' +
      '(event_type, user_id, performed_by, event_date, ip_address, ' +
      'details, data_before, data_after, severity) ' +
      'VALUES (:evtype, :userid, :performer, :evdate, :ip, ' +
      ':details, :databefore, :dataafter, :severity)';

    Query.ParamByName('evtype').AsString := EventTypeStr;
    Query.ParamByName('userid').AsInteger := Event.UserID;
    Query.ParamByName('performer').AsString := Event.PerformedBy;
    Query.ParamByName('evdate').AsDateTime := Event.EventDate;
    Query.ParamByName('ip').AsString := Event.IPAddress;
    Query.ParamByName('details').AsString := Event.Details;
    Query.ParamByName('databefore').AsString := SanitizeForLog(Event.DataBefore);
    Query.ParamByName('dataafter').AsString := SanitizeForLog(Event.DataAfter);
    Query.ParamByName('severity').AsInteger := Event.Severity;

    Query.ExecSQL;
    Result := True;

    // Écrire également dans un fichier de log
    WriteToLogFile(Event);

  except
    on E: Exception do
      WriteLn('Erreur log audit : ', E.Message);
  end;

  Query.Free;
end;

procedure TGDPRAuditor.WriteToLogFile(const Event: TAuditEvent);
var
  LogFile: TextFile;
  LogEntry: string;
begin
  try
    AssignFile(LogFile, FLogFilePath);

    if FileExists(FLogFilePath) then
      Append(LogFile)
    else
      Rewrite(LogFile);

    LogEntry := Format('[%s] User:%d Event:%d Performer:%s IP:%s Details:%s',
      [DateTimeToStr(Event.EventDate),
       Event.UserID,
       Ord(Event.EventType),
       Event.PerformedBy,
       Event.IPAddress,
       Event.Details]);

    WriteLn(LogFile, LogEntry);
    CloseFile(LogFile);
  except
    on E: Exception do
      WriteLn('Erreur écriture fichier log : ', E.Message);
  end;
end;

function TGDPRAuditor.GetAuditTrail(UserID: Integer;
  StartDate, EndDate: TDateTime): TStringList;
var
  Query: TSQLQuery;
begin
  Result := TStringList.Create;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'SELECT event_type, event_date, performed_by, details ' +
      'FROM gdpr_audit_log ' +
      'WHERE user_id = :userid ' +
      'AND event_date BETWEEN :startdate AND :enddate ' +
      'ORDER BY event_date DESC';

    Query.ParamByName('userid').AsInteger := UserID;
    Query.ParamByName('startdate').AsDateTime := StartDate;
    Query.ParamByName('enddate').AsDateTime := EndDate;
    Query.Open;

    while not Query.EOF do
    begin
      Result.Add(Format('%s | %s | %s | %s',
        [DateTimeToStr(Query.FieldByName('event_date').AsDateTime),
         Query.FieldByName('event_type').AsString,
         Query.FieldByName('performed_by').AsString,
         Query.FieldByName('details').AsString]));
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

function TGDPRAuditor.GenerateAuditReport(StartDate, EndDate: TDateTime): string;
var
  Query: TSQLQuery;
  Report: TStringList;
  TotalEvents, SecurityBreaches: Integer;
begin
  Report := TStringList.Create;
  try
    Report.Add('=== RAPPORT D''AUDIT GDPR ===');
    Report.Add(Format('Période : %s - %s',
      [DateToStr(StartDate), DateToStr(EndDate)]));
    Report.Add('');

    Query := TSQLQuery.Create(nil);
    try
      Query.Database := FDatabaseConnection;

      // Nombre total d'événements
      Query.SQL.Text :=
        'SELECT COUNT(*) as cnt FROM gdpr_audit_log ' +
        'WHERE event_date BETWEEN :startdate AND :enddate';
      Query.ParamByName('startdate').AsDateTime := StartDate;
      Query.ParamByName('enddate').AsDateTime := EndDate;
      Query.Open;
      TotalEvents := Query.FieldByName('cnt').AsInteger;
      Query.Close;

      Report.Add(Format('Nombre total d''événements : %d', [TotalEvents]));

      // Violations de sécurité
      Query.SQL.Text :=
        'SELECT COUNT(*) as cnt FROM gdpr_audit_log ' +
        'WHERE event_type = ''security_breach'' ' +
        'AND event_date BETWEEN :startdate AND :enddate';
      Query.ParamByName('startdate').AsDateTime := StartDate;
      Query.ParamByName('enddate').AsDateTime := EndDate;
      Query.Open;
      SecurityBreaches := Query.FieldByName('cnt').AsInteger;
      Query.Close;

      Report.Add(Format('Violations de sécurité : %d', [SecurityBreaches]));

      // Événements par type
      Report.Add('');
      Report.Add('Répartition par type d''événement :');
      Query.SQL.Text :=
        'SELECT event_type, COUNT(*) as cnt FROM gdpr_audit_log ' +
        'WHERE event_date BETWEEN :startdate AND :enddate ' +
        'GROUP BY event_type ORDER BY cnt DESC';
      Query.ParamByName('startdate').AsDateTime := StartDate;
      Query.ParamByName('enddate').AsDateTime := EndDate;
      Query.Open;

      while not Query.EOF do
      begin
        Report.Add(Format('  - %s : %d',
          [Query.FieldByName('event_type').AsString,
           Query.FieldByName('cnt').AsInteger]));
        Query.Next;
      end;
      Query.Close;

      // Utilisateurs les plus actifs
      Report.Add('');
      Report.Add('Top 10 utilisateurs (activité) :');
      Query.SQL.Text :=
        'SELECT user_id, COUNT(*) as cnt FROM gdpr_audit_log ' +
        'WHERE event_date BETWEEN :startdate AND :enddate ' +
        'GROUP BY user_id ORDER BY cnt DESC LIMIT 10';
      Query.ParamByName('startdate').AsDateTime := StartDate;
      Query.ParamByName('enddate').AsDateTime := EndDate;
      Query.Open;

      while not Query.EOF do
      begin
        Report.Add(Format('  - User %d : %d événements',
          [Query.FieldByName('user_id').AsInteger,
           Query.FieldByName('cnt').AsInteger]));
        Query.Next;
      end;
      Query.Close;

    finally
      Query.Free;
    end;

    Result := Report.Text;
  finally
    Report.Free;
  end;
end;

function TGDPRAuditor.SearchAuditLog(const SearchTerm: string): TStringList;
var
  Query: TSQLQuery;
begin
  Result := TStringList.Create;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'SELECT event_id, event_type, user_id, event_date, details ' +
      'FROM gdpr_audit_log ' +
      'WHERE details LIKE :searchterm ' +
      'OR performed_by LIKE :searchterm ' +
      'ORDER BY event_date DESC LIMIT 100';

    Query.ParamByName('searchterm').AsString := '%' + SearchTerm + '%';
    Query.Open;

    while not Query.EOF do
    begin
      Result.Add(Format('[%d] %s | User:%d | %s | %s',
        [Query.FieldByName('event_id').AsInteger,
         DateTimeToStr(Query.FieldByName('event_date').AsDateTime),
         Query.FieldByName('user_id').AsInteger,
         Query.FieldByName('event_type').AsString,
         Query.FieldByName('details').AsString]));
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

end.
```

**Exemple d'utilisation du système d'audit :**

```pascal
var
  Auditor: TGDPRAuditor;
  Event: TAuditEvent;
  AuditTrail: TStringList;
begin
  Auditor := TGDPRAuditor.Create(DatabaseConnection, 'C:\Logs\gdpr_audit.log');
  try
    // Logger un accès aux données
    Event.EventType := aetDataAccess;
    Event.UserID := 123;
    Event.PerformedBy := 'admin';
    Event.EventDate := Now;
    Event.IPAddress := '192.168.1.50';
    Event.Details := 'Consultation du profil utilisateur';
    Event.Severity := 2;

    Auditor.LogEvent(Event);

    // Obtenir l'historique d'audit pour un utilisateur
    AuditTrail := Auditor.GetAuditTrail(123, IncMonth(Now, -1), Now);
    try
      WriteLn('Historique d''audit :');
      WriteLn(AuditTrail.Text);
    finally
      AuditTrail.Free;
    end;

    // Générer un rapport
    WriteLn(Auditor.GenerateAuditReport(IncMonth(Now, -3), Now));
  finally
    Auditor.Free;
  end;
end;
```

## Notification de violation de données

En cas de violation de données, le GDPR impose de notifier les autorités dans les 72 heures.

```pascal
unit GDPRBreachNotification;

interface

uses
  Classes, SysUtils, sqldb;

type
  TBreachSeverity = (bsLow, bsMedium, bsHigh, bsCritical);

  TDataBreach = record
    BreachID: Integer;
    DetectedDate: TDateTime;
    ReportedDate: TDateTime;
    Severity: TBreachSeverity;
    AffectedUsers: Integer;
    DataTypes: string;        // Types de données concernées
    Description: string;
    MitigationSteps: string;
    AuthorityNotified: Boolean;
    UsersNotified: Boolean;
  end;

  TGDPRBreachManager = class
  private
    FDatabaseConnection: TConnection;
    function CalculateSeverity(AffectedUsers: Integer;
      const DataTypes: string): TBreachSeverity;
    function NotifyAuthority(const Breach: TDataBreach): Boolean;
    function NotifyAffectedUsers(const Breach: TDataBreach): Boolean;
  public
    constructor Create(DBConnection: TConnection);

    function ReportBreach(const Description, DataTypes: string;
      AffectedUsers: Integer): Integer;
    function GetBreachDetails(BreachID: Integer): TDataBreach;
    function UpdateBreachStatus(BreachID: Integer;
      const MitigationSteps: string): Boolean;
    function IsWithin72Hours(BreachID: Integer): Boolean;
  end;

implementation

constructor TGDPRBreachManager.Create(DBConnection: TConnection);
begin
  inherited Create;
  FDatabaseConnection := DBConnection;
end;

function TGDPRBreachManager.CalculateSeverity(AffectedUsers: Integer;
  const DataTypes: string): TBreachSeverity;
var
  HasSensitiveData: Boolean;
begin
  // Vérifier si des données sensibles sont concernées
  HasSensitiveData :=
    (Pos('password', LowerCase(DataTypes)) > 0) or
    (Pos('credit_card', LowerCase(DataTypes)) > 0) or
    (Pos('health', LowerCase(DataTypes)) > 0) or
    (Pos('social_security', LowerCase(DataTypes)) > 0);

  // Déterminer la gravité
  if HasSensitiveData or (AffectedUsers > 10000) then
    Result := bsCritical
  else if AffectedUsers > 1000 then
    Result := bsHigh
  else if AffectedUsers > 100 then
    Result := bsMedium
  else
    Result := bsLow;
end;

function TGDPRBreachManager.ReportBreach(const Description, DataTypes: string;
  AffectedUsers: Integer): Integer;
var
  Query: TSQLQuery;
  Severity: TBreachSeverity;
begin
  Result := -1;

  Severity := CalculateSeverity(AffectedUsers, DataTypes);

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'INSERT INTO data_breaches ' +
      '(detected_date, severity, affected_users, data_types, description) ' +
      'VALUES (:detdate, :severity, :affected, :datatypes, :description) ' +
      'RETURNING breach_id';

    Query.ParamByName('detdate').AsDateTime := Now;
    Query.ParamByName('severity').AsInteger := Ord(Severity);
    Query.ParamByName('affected').AsInteger := AffectedUsers;
    Query.ParamByName('datatypes').AsString := DataTypes;
    Query.ParamByName('description').AsString := Description;

    Query.Open;

    if not Query.EOF then
      Result := Query.FieldByName('breach_id').AsInteger;

    Query.Close;

    WriteLn(Format('Violation enregistrée (ID: %d, Gravité: %d, Utilisateurs: %d)',
      [Result, Ord(Severity), AffectedUsers]));

    // Si critique ou haute, notifier immédiatement
    if Severity in [bsHigh, bsCritical] then
    begin
      WriteLn('ALERTE : Violation critique détectée !');
      // Déclencher les procédures d'urgence
    end;

  except
    on E: Exception do
      WriteLn('Erreur enregistrement violation : ', E.Message);
  end;

  Query.Free;
end;

function TGDPRBreachManager.GetBreachDetails(BreachID: Integer): TDataBreach;
var
  Query: TSQLQuery;
begin
  FillChar(Result, SizeOf(Result), 0);

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'SELECT * FROM data_breaches WHERE breach_id = :breachid';
    Query.ParamByName('breachid').AsInteger := BreachID;
    Query.Open;

    if not Query.EOF then
    begin
      Result.BreachID := Query.FieldByName('breach_id').AsInteger;
      Result.DetectedDate := Query.FieldByName('detected_date').AsDateTime;
      Result.ReportedDate := Query.FieldByName('reported_date').AsDateTime;
      Result.Severity := TBreachSeverity(Query.FieldByName('severity').AsInteger);
      Result.AffectedUsers := Query.FieldByName('affected_users').AsInteger;
      Result.DataTypes := Query.FieldByName('data_types').AsString;
      Result.Description := Query.FieldByName('description').AsString;
      Result.MitigationSteps := Query.FieldByName('mitigation_steps').AsString;
      Result.AuthorityNotified := Query.FieldByName('authority_notified').AsBoolean;
      Result.UsersNotified := Query.FieldByName('users_notified').AsBoolean;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

function TGDPRBreachManager.IsWithin72Hours(BreachID: Integer): Boolean;
var
  Breach: TDataBreach;
  HoursSinceDetection: Int64;
begin
  Breach := GetBreachDetails(BreachID);
  HoursSinceDetection := HoursBetween(Now, Breach.DetectedDate);
  Result := HoursSinceDetection <= 72;

  if not Result then
    WriteLn('ATTENTION : Délai de 72h dépassé pour la violation ', BreachID);
end;

function TGDPRBreachManager.NotifyAuthority(const Breach: TDataBreach): Boolean;
begin
  // Implémentation de la notification à la CNIL (France) ou autorité compétente
  // Ceci devrait inclure :
  // - Génération d'un rapport détaillé
  // - Envoi par email ou formulaire web officiel
  // - Enregistrement de la confirmation

  WriteLn('Notification de l''autorité pour violation ', Breach.BreachID);
  Result := True;
end;

function TGDPRBreachManager.NotifyAffectedUsers(const Breach: TDataBreach): Boolean;
begin
  // Implémentation de la notification aux utilisateurs affectés
  // - Envoyer des emails individuels
  // - Expliquer la nature de la violation
  // - Recommandations (changement de mot de passe, etc.)

  WriteLn(Format('Notification de %d utilisateurs affectés',
    [Breach.AffectedUsers]));
  Result := True;
end;

function TGDPRBreachManager.UpdateBreachStatus(BreachID: Integer;
  const MitigationSteps: string): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'UPDATE data_breaches SET ' +
      'mitigation_steps = :steps, ' +
      'last_updated = :updated ' +
      'WHERE breach_id = :breachid';

    Query.ParamByName('steps').AsString := MitigationSteps;
    Query.ParamByName('updated').AsDateTime := Now;
    Query.ParamByName('breachid').AsInteger := BreachID;

    Query.ExecSQL;
    Result := True;
  except
    on E: Exception do
      WriteLn('Erreur mise à jour violation : ', E.Message);
  end;

  Query.Free;
end;

end.
```

## Schéma de base de données pour la conformité GDPR

Voici un exemple de structure de base de données supportant la conformité GDPR :

```sql
-- Table des utilisateurs avec champs GDPR
CREATE TABLE users (
    user_id INTEGER PRIMARY KEY AUTO_INCREMENT,
    email VARCHAR(255) UNIQUE NOT NULL,
    name VARCHAR(255),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_login TIMESTAMP,

    -- Champs GDPR
    gdpr_consent_date TIMESTAMP,
    gdpr_anonymized BOOLEAN DEFAULT FALSE,
    anonymized_date TIMESTAMP NULL,
    processing_restricted BOOLEAN DEFAULT FALSE,
    restriction_date TIMESTAMP NULL,
    restriction_lifted_date TIMESTAMP NULL,

    INDEX idx_email (email),
    INDEX idx_gdpr_anonymized (gdpr_anonymized)
);

-- Table des consentements
CREATE TABLE consent_log (
    consent_id INTEGER PRIMARY KEY AUTO_INCREMENT,
    user_id INTEGER NOT NULL,
    purpose VARCHAR(50) NOT NULL,  -- 'marketing', 'profiling', etc.
    granted BOOLEAN NOT NULL,
    consent_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    withdrawn BOOLEAN DEFAULT FALSE,
    withdrawn_date TIMESTAMP NULL,
    ip_address VARCHAR(45),
    user_agent TEXT,

    FOREIGN KEY (user_id) REFERENCES users(user_id),
    INDEX idx_user_purpose (user_id, purpose),
    INDEX idx_consent_date (consent_date)
);

-- Table d'audit GDPR
CREATE TABLE gdpr_audit_log (
    event_id BIGINT PRIMARY KEY AUTO_INCREMENT,
    event_type VARCHAR(50) NOT NULL,
    user_id INTEGER,
    performed_by VARCHAR(255),
    event_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    ip_address VARCHAR(45),
    details TEXT,
    data_before TEXT,
    data_after TEXT,
    severity INTEGER DEFAULT 1,

    INDEX idx_user_date (user_id, event_date),
    INDEX idx_event_type (event_type),
    INDEX idx_event_date (event_date)
);

-- Table des effacements
CREATE TABLE gdpr_erasure_log (
    erasure_id INTEGER PRIMARY KEY AUTO_INCREMENT,
    user_id INTEGER NOT NULL,
    erasure_method VARCHAR(50),  -- 'soft_delete', 'hard_delete', 'anonymization'
    erasure_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    performed_by VARCHAR(255),
    reason TEXT,

    INDEX idx_erasure_date (erasure_date),
    INDEX idx_user_id (user_id)
);

-- Table des restrictions de traitement
CREATE TABLE gdpr_restriction_log (
    restriction_id INTEGER PRIMARY KEY AUTO_INCREMENT,
    user_id INTEGER NOT NULL,
    reason VARCHAR(50),  -- 'contest_accuracy', 'unlawful_processing', etc.
    restriction_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    lifted_date TIMESTAMP NULL,

    FOREIGN KEY (user_id) REFERENCES users(user_id),
    INDEX idx_user_id (user_id),
    INDEX idx_restriction_date (restriction_date)
);

-- Table des violations de données
CREATE TABLE data_breaches (
    breach_id INTEGER PRIMARY KEY AUTO_INCREMENT,
    detected_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    reported_date TIMESTAMP NULL,
    severity INTEGER,  -- 0=Low, 1=Medium, 2=High, 3=Critical
    affected_users INTEGER,
    data_types TEXT,
    description TEXT,
    mitigation_steps TEXT,
    authority_notified BOOLEAN DEFAULT FALSE,
    users_notified BOOLEAN DEFAULT FALSE,
    last_updated TIMESTAMP,

    INDEX idx_detected_date (detected_date),
    INDEX idx_severity (severity)
);

-- Table de conservation des données
CREATE TABLE data_retention_policy (
    policy_id INTEGER PRIMARY KEY AUTO_INCREMENT,
    data_type VARCHAR(100) NOT NULL,
    retention_days INTEGER NOT NULL,
    legal_basis TEXT,
    description TEXT,
    created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    UNIQUE KEY uk_data_type (data_type)
);

-- Table des exports de données
CREATE TABLE data_exports (
    export_id INTEGER PRIMARY KEY AUTO_INCREMENT,
    user_id INTEGER NOT NULL,
    export_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    format VARCHAR(20),  -- 'json', 'xml', 'csv'
    file_path VARCHAR(500),
    file_size_bytes BIGINT,
    status VARCHAR(20) DEFAULT 'pending',  -- 'pending', 'completed', 'failed'

    FOREIGN KEY (user_id) REFERENCES users(user_id),
    INDEX idx_user_date (user_id, export_date)
);
```

## Politique de conservation des données

Automatiser la suppression des données après la période de conservation.

```pascal
unit GDPRDataRetention;

interface

uses
  Classes, SysUtils, sqldb;

type
  TRetentionPolicy = record
    DataType: string;
    RetentionDays: Integer;
    LegalBasis: string;
    Description: string;
  end;

  TGDPRRetentionManager = class
  private
    FDatabaseConnection: TConnection;
    function GetRetentionPolicy(const DataType: string): TRetentionPolicy;
  public
    constructor Create(DBConnection: TConnection);

    procedure DefineRetentionPolicy(const Policy: TRetentionPolicy);
    function CleanupExpiredData: Integer;
    function GetDataAge(UserID: Integer): Integer;
    procedure GenerateRetentionReport: string;
  end;

implementation

uses
  DateUtils;

constructor TGDPRRetentionManager.Create(DBConnection: TConnection);
begin
  inherited Create;
  FDatabaseConnection := DBConnection;
end;

procedure TGDPRRetentionManager.DefineRetentionPolicy(const Policy: TRetentionPolicy);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'INSERT INTO data_retention_policy ' +
      '(data_type, retention_days, legal_basis, description) ' +
      'VALUES (:datatype, :days, :basis, :desc) ' +
      'ON DUPLICATE KEY UPDATE ' +
      'retention_days = :days, legal_basis = :basis, description = :desc';

    Query.ParamByName('datatype').AsString := Policy.DataType;
    Query.ParamByName('days').AsInteger := Policy.RetentionDays;
    Query.ParamByName('basis').AsString := Policy.LegalBasis;
    Query.ParamByName('desc').AsString := Policy.Description;

    Query.ExecSQL;

    WriteLn(Format('Politique de conservation définie : %s (%d jours)',
      [Policy.DataType, Policy.RetentionDays]));
  finally
    Query.Free;
  end;
end;

function TGDPRRetentionManager.GetRetentionPolicy(const DataType: string): TRetentionPolicy;
var
  Query: TSQLQuery;
begin
  FillChar(Result, SizeOf(Result), 0);

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'SELECT * FROM data_retention_policy WHERE data_type = :datatype';
    Query.ParamByName('datatype').AsString := DataType;
    Query.Open;

    if not Query.EOF then
    begin
      Result.DataType := Query.FieldByName('data_type').AsString;
      Result.RetentionDays := Query.FieldByName('retention_days').AsInteger;
      Result.LegalBasis := Query.FieldByName('legal_basis').AsString;
      Result.Description := Query.FieldByName('description').AsString;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

function TGDPRRetentionManager.CleanupExpiredData: Integer;
var
  Query: TSQLQuery;
  Policy: TRetentionPolicy;
  CutoffDate: TDateTime;
  DeletedCount: Integer;
begin
  Result := 0;

  // Session logs
  Policy := GetRetentionPolicy('session_logs');
  if Policy.RetentionDays > 0 then
  begin
    CutoffDate := IncDay(Now, -Policy.RetentionDays);

    Query := TSQLQuery.Create(nil);
    try
      Query.Database := FDatabaseConnection;
      Query.SQL.Text :=
        'DELETE FROM session_logs WHERE created_date < :cutoff';
      Query.ParamByName('cutoff').AsDateTime := CutoffDate;
      Query.ExecSQL;

      DeletedCount := Query.RowsAffected;
      Result := Result + DeletedCount;

      WriteLn(Format('Logs de session nettoyés : %d enregistrements', [DeletedCount]));
    finally
      Query.Free;
    end;
  end;

  // User activity logs
  Policy := GetRetentionPolicy('activity_logs');
  if Policy.RetentionDays > 0 then
  begin
    CutoffDate := IncDay(Now, -Policy.RetentionDays);

    Query := TSQLQuery.Create(nil);
    try
      Query.Database := FDatabaseConnection;
      Query.SQL.Text :=
        'DELETE FROM user_activity_logs WHERE activity_date < :cutoff';
      Query.ParamByName('cutoff').AsDateTime := CutoffDate;
      Query.ExecSQL;

      DeletedCount := Query.RowsAffected;
      Result := Result + DeletedCount;

      WriteLn(Format('Logs d''activité nettoyés : %d enregistrements', [DeletedCount]));
    finally
      Query.Free;
    end;
  end;

  // Comptes inactifs
  Policy := GetRetentionPolicy('inactive_accounts');
  if Policy.RetentionDays > 0 then
  begin
    CutoffDate := IncDay(Now, -Policy.RetentionDays);

    Query := TSQLQuery.Create(nil);
    try
      Query.Database := FDatabaseConnection;
      Query.SQL.Text :=
        'UPDATE users SET gdpr_anonymized = 1, anonymized_date = :anondate ' +
        'WHERE last_login < :cutoff AND gdpr_anonymized = 0';
      Query.ParamByName('anondate').AsDateTime := Now;
      Query.ParamByName('cutoff').AsDateTime := CutoffDate;
      Query.ExecSQL;

      DeletedCount := Query.RowsAffected;
      Result := Result + DeletedCount;

      WriteLn(Format('Comptes inactifs anonymisés : %d utilisateurs', [DeletedCount]));
    finally
      Query.Free;
    end;
  end;

  WriteLn(Format('Nettoyage total : %d éléments traités', [Result]));
end;

function TGDPRRetentionManager.GetDataAge(UserID: Integer): Integer;
var
  Query: TSQLQuery;
begin
  Result := 0;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabaseConnection;
    Query.SQL.Text :=
      'SELECT DATEDIFF(NOW(), created_at) as age_days ' +
      'FROM users WHERE user_id = :userid';
    Query.ParamByName('userid').AsInteger := UserID;
    Query.Open;

    if not Query.EOF then
      Result := Query.FieldByName('age_days').AsInteger;

    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TGDPRRetentionManager.GenerateRetentionReport;
var
  Query: TSQLQuery;
  Report: TStringList;
begin
  Report := TStringList.Create;
  try
    Report.Add('=== RAPPORT DE CONSERVATION DES DONNÉES ===');
    Report.Add('');
    Report.Add('Politiques actives :');

    Query := TSQLQuery.Create(nil);
    try
      Query.Database := FDatabaseConnection;
      Query.SQL.Text :=
        'SELECT * FROM data_retention_policy ORDER BY data_type';
      Query.Open;

      while not Query.EOF do
      begin
        Report.Add(Format('  - %s : %d jours (%s)',
          [Query.FieldByName('data_type').AsString,
           Query.FieldByName('retention_days').AsInteger,
           Query.FieldByName('legal_basis').AsString]));
        Query.Next;
      end;

      Query.Close;

      Report.Add('');
      Report.Add('Statistiques des données :');

      // Compter les utilisateurs actifs vs inactifs
      Query.SQL.Text :=
        'SELECT ' +
        '  SUM(CASE WHEN last_login >= DATE_SUB(NOW(), INTERVAL 90 DAY) THEN 1 ELSE 0 END) as active, ' +
        '  SUM(CASE WHEN last_login < DATE_SUB(NOW(), INTERVAL 90 DAY) THEN 1 ELSE 0 END) as inactive, ' +
        '  SUM(CASE WHEN gdpr_anonymized = 1 THEN 1 ELSE 0 END) as anonymized ' +
        'FROM users';
      Query.Open;

      if not Query.EOF then
      begin
        Report.Add(Format('  Utilisateurs actifs (90 jours) : %d',
          [Query.FieldByName('active').AsInteger]));
        Report.Add(Format('  Utilisateurs inactifs : %d',
          [Query.FieldByName('inactive').AsInteger]));
        Report.Add(Format('  Utilisateurs anonymisés : %d',
          [Query.FieldByName('anonymized').AsInteger]));
      end;

      Query.Close;

    finally
      Query.Free;
    end;

    WriteLn(Report.Text);
  finally
    Report.Free;
  end;
end;

end.
```

## Checklist de conformité GDPR

Liste de vérification pour s'assurer de la conformité de votre application FreePascal/Lazarus :

```pascal
unit GDPRComplianceChecker;

interface

uses
  Classes, SysUtils;

type
  TComplianceItem = record
    ID: string;
    Category: string;
    Description: string;
    Compliant: Boolean;
    Notes: string;
  end;

  TGDPRComplianceChecker = class
  private
    FItems: array of TComplianceItem;
    procedure AddItem(const ID, Category, Description: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RunComplianceCheck;
    function GenerateComplianceReport: string;
    function GetComplianceScore: Double;
  end;

implementation

constructor TGDPRComplianceChecker.Create;
begin
  inherited Create;
  SetLength(FItems, 0);

  // Base légale
  AddItem('LEGAL_001', 'Base légale',
    'Une base légale est définie pour chaque traitement');
  AddItem('LEGAL_002', 'Base légale',
    'Le consentement est documenté et traçable');

  // Minimisation des données
  AddItem('DATA_001', 'Minimisation',
    'Seules les données nécessaires sont collectées');
  AddItem('DATA_002', 'Minimisation',
    'Les champs optionnels sont clairement marqués');

  // Droits des utilisateurs
  AddItem('RIGHTS_001', 'Droits',
    'Droit d''accès : export des données implémenté');
  AddItem('RIGHTS_002', 'Droits',
    'Droit de rectification : modification des données possible');
  AddItem('RIGHTS_003', 'Droits',
    'Droit à l''effacement : suppression/anonymisation implémentée');
  AddItem('RIGHTS_004', 'Droits',
    'Droit à la portabilité : export dans format structuré');
  AddItem('RIGHTS_005', 'Droits',
    'Droit d''opposition : mécanisme d''opt-out disponible');

  // Sécurité
  AddItem('SEC_001', 'Sécurité',
    'Chiffrement des données sensibles au repos');
  AddItem('SEC_002', 'Sécurité',
    'Chiffrement des communications (TLS/SSL)');
  AddItem('SEC_003', 'Sécurité',
    'Contrôle d''accès et authentification robuste');
  AddItem('SEC_004', 'Sécurité',
    'Journalisation des accès aux données personnelles');

  // Consentement
  AddItem('CONSENT_001', 'Consentement',
    'Recueil explicite du consentement');
  AddItem('CONSENT_002', 'Consentement',
    'Possibilité de retirer le consentement facilement');
  AddItem('CONSENT_003', 'Consentement',
    'Conservation de la preuve du consentement');

  // Conservation
  AddItem('RETENTION_001', 'Conservation',
    'Politique de conservation définie et documentée');
  AddItem('RETENTION_002', 'Conservation',
    'Suppression automatique des données expirées');

  // Transparence
  AddItem('TRANS_001', 'Transparence',
    'Politique de confidentialité claire et accessible');
  AddItem('TRANS_002', 'Transparence',
    'Information sur les finalités du traitement');
  AddItem('TRANS_003', 'Transparence',
    'Information sur les destinataires des données');

  // Violation
  AddItem('BREACH_001', 'Violations',
    'Procédure de notification des violations en place');
  AddItem('BREACH_002', 'Violations',
    'Registre des violations maintenu');

  // Sous-traitants
  AddItem('PROCESSOR_001', 'Sous-traitants',
    'Contrats DPA avec les sous-traitants');
  AddItem('PROCESSOR_002', 'Sous-traitants',
    'Vérification de la conformité des sous-traitants');

  // Documentation
  AddItem('DOC_001', 'Documentation',
    'Registre des activités de traitement maintenu');
  AddItem('DOC_002', 'Documentation',
    'Analyse d''impact (PIA) pour traitements à risque');
end;

destructor TGDPRComplianceChecker.Destroy;
begin
  SetLength(FItems, 0);
  inherited;
end;

procedure TGDPRComplianceChecker.AddItem(const ID, Category, Description: string);
var
  Idx: Integer;
begin
  Idx := Length(FItems);
  SetLength(FItems, Idx + 1);

  FItems[Idx].ID := ID;
  FItems[Idx].Category := Category;
  FItems[Idx].Description := Description;
  FItems[Idx].Compliant := False;
  FItems[Idx].Notes := '';
end;

procedure TGDPRComplianceChecker.RunComplianceCheck;
var
  i: Integer;
begin
  WriteLn('=== VÉRIFICATION DE CONFORMITÉ GDPR ===');
  WriteLn('');

  for i := 0 to High(FItems) do
  begin
    WriteLn(Format('[%s] %s', [FItems[i].ID, FItems[i].Description]));
    Write('Conforme ? (O/N) : ');

    // En production, ceci serait vérifié automatiquement
    // Ici, c'est une vérification manuelle

    WriteLn('');
  end;
end;

function TGDPRComplianceChecker.GenerateComplianceReport: string;
var
  Report: TStringList;
  i, CompliantCount, TotalCount: Integer;
  CurrentCategory: string;
begin
  Report := TStringList.Create;
  try
    Report.Add('========================================');
    Report.Add('   RAPPORT DE CONFORMITÉ GDPR');
    Report.Add('========================================');
    Report.Add('Date : ' + DateTimeToStr(Now));
    Report.Add('');

    CompliantCount := 0;
    TotalCount := Length(FItems);

    CurrentCategory := '';
    for i := 0 to High(FItems) do
    begin
      if FItems[i].Category <> CurrentCategory then
      begin
        CurrentCategory := FItems[i].Category;
        Report.Add('');
        Report.Add('--- ' + CurrentCategory + ' ---');
      end;

      Report.Add(Format('  [%s] %s : %s',
        [FItems[i].ID,
         IfThen(FItems[i].Compliant, '✓', '✗'),
         FItems[i].Description]));

      if FItems[i].Notes <> '' then
        Report.Add('       Note : ' + FItems[i].Notes);

      if FItems[i].Compliant then
        Inc(CompliantCount);
    end;

    Report.Add('');
    Report.Add('========================================');
    Report.Add(Format('Score de conformité : %.1f%% (%d/%d)',
      [GetComplianceScore, CompliantCount, TotalCount]));
    Report.Add('========================================');

    Result := Report.Text;
  finally
    Report.Free;
  end;
end;

function TGDPRComplianceChecker.GetComplianceScore: Double;
var
  i, CompliantCount: Integer;
begin
  CompliantCount := 0;

  for i := 0 to High(FItems) do
  begin
    if FItems[i].Compliant then
      Inc(CompliantCount);
  end;

  if Length(FItems) > 0 then
    Result := (CompliantCount / Length(FItems)) * 100.0
  else
    Result := 0.0;
end;

end.
```

## Gestion multi-plateforme de la conformité

Adaptation pour Windows et Ubuntu :

```pascal
unit GDPRPlatformSpecific;

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}, Windows, Registry{$ENDIF}
  {$IFDEF UNIX}, Unix{$ENDIF};

type
  TGDPRPlatformManager = class
  public
    class function GetSystemLocale: string;
    class function IsEURegion: Boolean;
    class function GetDataProtectionAuthority: string;
    class procedure StoreConsentLocally(UserID: Integer; Consent: Boolean);
    class function RetrieveLocalConsent(UserID: Integer): Boolean;
  end;

implementation

class function TGDPRPlatformManager.GetSystemLocale: string;
{$IFDEF WINDOWS}
var
  Buffer: array[0..255] of Char;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO3166CTRYNAME, Buffer, SizeOf(Buffer));
  Result := string(Buffer);
  {$ENDIF}

  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('LANG');
  if Pos('_', Result) > 0 then
    Result := Copy(Result, Pos('_', Result) + 1, 2)
  else
    Result := 'US';
  {$ENDIF}
end;

class function TGDPRPlatformManager.IsEURegion: Boolean;
const
  EU_COUNTRIES: array[0..26] of string = (
    'AT', 'BE', 'BG', 'HR', 'CY', 'CZ', 'DK', 'EE', 'FI', 'FR',
    'DE', 'GR', 'HU', 'IE', 'IT', 'LV', 'LT', 'LU', 'MT', 'NL',
    'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE'
  );
var
  Locale: string;
  i: Integer;
begin
  Result := False;
  Locale := UpperCase(GetSystemLocale);

  for i := Low(EU_COUNTRIES) to High(EU_COUNTRIES) do
  begin
    if Locale = EU_COUNTRIES[i] then
    begin
      Result := True;
      Break;
    end;
  end;
end;

class function TGDPRPlatformManager.GetDataProtectionAuthority: string;
var
  Locale: string;
begin
  Locale := UpperCase(GetSystemLocale);

  // Retourner l'autorité compétente selon le pays
  if Locale = 'FR' then
    Result := 'CNIL - Commission Nationale de l''Informatique et des Libertés'
  else if Locale = 'DE' then
    Result := 'BfDI - Bundesbeauftragte für den Datenschutz'
  else if Locale = 'UK' then
    Result := 'ICO - Information Commissioner''s Office'
  else if Locale = 'IT' then
    Result := 'Garante per la protezione dei dati personali'
  else if Locale = 'ES' then
    Result := 'AEPD - Agencia Española de Protección de Datos'
  else
    Result := 'EDPB - European Data Protection Board';
end;

class procedure TGDPRPlatformManager.StoreConsentLocally(UserID: Integer; Consent: Boolean);
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
{$IFDEF UNIX}
var
  ConfigFile: TextFile;
  ConfigPath: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Stocker dans le registre Windows
  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Software\MyApp\GDPR', True) then
    begin
      Reg.WriteBool(Format('Consent_%d', [UserID]), Consent);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Stocker dans un fichier de configuration Linux
  ConfigPath := GetEnvironmentVariable('HOME') + '/.config/myapp/gdpr.conf';

  if not DirectoryExists(ExtractFilePath(ConfigPath)) then
    ForceDirectories(ExtractFilePath(ConfigPath));

  AssignFile(ConfigFile, ConfigPath);
  try
    if FileExists(ConfigPath) then
      Append(ConfigFile)
    else
      Rewrite(ConfigFile);

    WriteLn(ConfigFile, Format('user_%d=%s',
      [UserID, IfThen(Consent, 'true', 'false')]));

    CloseFile(ConfigFile);
  except
    on E: Exception do
      WriteLn('Erreur stockage consentement : ', E.Message);
  end;
  {$ENDIF}
end;

class function TGDPRPlatformManager.RetrieveLocalConsent(UserID: Integer): Boolean;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
{$IFDEF UNIX}
var
  ConfigFile: TextFile;
  ConfigPath, Line: string;
{$ENDIF}
begin
  Result := False;

  {$IFDEF WINDOWS}
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Software\MyApp\GDPR', False) then
    begin
      if Reg.ValueExists(Format('Consent_%d', [UserID])) then
        Result := Reg.ReadBool(Format('Consent_%d', [UserID]));
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  ConfigPath := GetEnvironmentVariable('HOME') + '/.config/myapp/gdpr.conf';

  if FileExists(ConfigPath) then
  begin
    AssignFile(ConfigFile, ConfigPath);
    try
      Reset(ConfigFile);

      while not Eof(ConfigFile) do
      begin
        ReadLn(ConfigFile, Line);
        if Pos(Format('user_%d=', [UserID]), Line) > 0 then
        begin
          Result := Pos('true', Line) > 0;
          Break;
        end;
      end;

      CloseFile(ConfigFile);
    except
      on E: Exception do
        WriteLn('Erreur lecture consentement : ', E.Message);
    end;
  end;
  {$ENDIF}
end;

end.
```

## Bonnes pratiques pour la conformité GDPR

### 1. Privacy by Design

Intégrer la protection des données dès la conception.

```pascal
// ✅ Bon : Minimisation dès la conception
type
  TUserRegistration = record
    Email: string;        // Nécessaire pour l'authentification
    DisplayName: string;  // Nécessaire pour l'interface
    // Pas de collecte de données inutiles
  end;

// ❌ Mauvais : Collecte excessive
type
  TUserRegistration = record
    Email, Phone, Address, BirthDate,
    FavoriteSong, ShoeSize, PetName: string;
  end;
```

### 2. Privacy by Default

Paramètres par défaut respectueux de la vie privée.

```pascal
const
  DEFAULT_PRIVACY_SETTINGS: record
    AllowMarketing: Boolean = False;          // Opt-in requis
    AllowProfiling: Boolean = False;          // Opt-in requis
    AllowThirdPartySharing: Boolean = False;  // Opt-in requis
    AllowAnalytics: Boolean = True;           // Analytics anonymes OK
  end;
```

### 3. Transparence

Toujours informer l'utilisateur.

```pascal
procedure ShowPrivacyNotice;
begin
  ShowMessage(
    'Nous collectons votre adresse email pour :' + LineEnding +
    '- Vous permettre de vous connecter' + LineEnding +
    '- Vous envoyer des notifications importantes' + LineEnding +
    LineEnding +
    'Vos données sont conservées pendant 3 ans.' + LineEnding +
    'Vous pouvez les supprimer à tout moment.' + LineEnding +
    LineEnding +
    'En savoir plus : https://example.com/privacy'
  );
end;
```

### 4. Documentation

Maintenir un registre des traitements.

```pascal
unit GDPRProcessingRegistry;

interface

type
  TProcessingRecord = record
    ProcessingID: string;
    Name: string;
    Purpose: string;
    LegalBasis: string;
    DataCategories: string;
    Recipients: string;
    RetentionPeriod: string;
    SecurityMeasures: string;
    DPOContact: string;
  end;

const
  PROCESSING_REGISTRY: array[0..2] of TProcessingRecord = (
    (ProcessingID: 'PROC_001';
     Name: 'Gestion des comptes utilisateurs';
     Purpose: 'Authentification et gestion du compte';
     LegalBasis: 'Exécution du contrat';
     DataCategories: 'Identité, coordonnées';
     Recipients: 'Personnel interne uniquement';
     RetentionPeriod: '3 ans après dernière connexion';
     SecurityMeasures: 'Chiffrement, contrôle d''accès';
     DPOContact: 'dpo@example.com'),

    (ProcessingID: 'PROC_002';
     Name: 'Envoi de newsletters';
     Purpose: 'Communication marketing';
     LegalBasis: 'Consentement';
     DataCategories: 'Email, préférences';
     Recipients: 'Prestataire emailing (UE)';
     RetentionPeriod: 'Jusqu''au retrait du consentement';
     SecurityMeasures: 'TLS, liste de désabonnement';
     DPOContact: 'dpo@example.com'),

    (ProcessingID: 'PROC_003';
     Name: 'Analyse statistique';
     Purpose: 'Amélioration du service';
     LegalBasis: 'Intérêt légitime';
     DataCategories: 'Données d''usage anonymisées';
     Recipients: 'Personnel interne, prestataire analytics (UE)';
     RetentionPeriod: '1 an';
     SecurityMeasures: 'Anonymisation, agrégation';
     DPOContact: 'dpo@example.com')
  );

end.
```

## Outils d'automatisation de la conformité

### 1. Tâche planifiée de nettoyage

```pascal
unit GDPRScheduledTasks;

interface

uses
  Classes, SysUtils, DateUtils;

type
  TGDPRScheduler = class
  private
    FLastCleanup: TDateTime;
    FCleanupIntervalDays: Integer;
    procedure ExecuteCleanup;
    procedure ExecuteAuditReport;
    procedure CheckExpiredConsents;
  public
    constructor Create;

    procedure RunScheduledTasks;
    procedure SetCleanupInterval(Days: Integer);

    property LastCleanup: TDateTime read FLastCleanup;
  end;

implementation

uses
  GDPRDataRetention, GDPRAuditSystem, GDPRConsent;

constructor TGDPRScheduler.Create;
begin
  inherited Create;
  FCleanupIntervalDays := 7;  // Par défaut : hebdomadaire
  FLastCleanup := 0;
end;

procedure TGDPRScheduler.SetCleanupInterval(Days: Integer);
begin
  if Days > 0 then
    FCleanupIntervalDays := Days;
end;

procedure TGDPRScheduler.ExecuteCleanup;
var
  RetentionManager: TGDPRRetentionManager;
  DeletedCount: Integer;
begin
  WriteLn('[', DateTimeToStr(Now), '] Début du nettoyage GDPR...');

  RetentionManager := TGDPRRetentionManager.Create(DatabaseConnection);
  try
    DeletedCount := RetentionManager.CleanupExpiredData;
    WriteLn(Format('Nettoyage terminé : %d éléments traités', [DeletedCount]));
  finally
    RetentionManager.Free;
  end;

  FLastCleanup := Now;
end;

procedure TGDPRScheduler.ExecuteAuditReport;
var
  Auditor: TGDPRAuditor;
  Report: string;
  ReportFile: TextFile;
  ReportPath: string;
begin
  WriteLn('[', DateTimeToStr(Now), '] Génération du rapport d''audit...');

  Auditor := TGDPRAuditor.Create(DatabaseConnection, 'audit.log');
  try
    Report := Auditor.GenerateAuditReport(
      IncMonth(Now, -1),  // Dernier mois
      Now
    );

    // Sauvegarder le rapport
    ReportPath := Format('gdpr_audit_%s.txt',
      [FormatDateTime('yyyymmdd', Now)]);

    AssignFile(ReportFile, ReportPath);
    try
      Rewrite(ReportFile);
      Write(ReportFile, Report);
      CloseFile(ReportFile);

      WriteLn('Rapport sauvegardé : ', ReportPath);
    except
      on E: Exception do
        WriteLn('Erreur sauvegarde rapport : ', E.Message);
    end;
  finally
    Auditor.Free;
  end;
end;

procedure TGDPRScheduler.CheckExpiredConsents;
var
  Query: TSQLQuery;
  ExpiredCount: Integer;
begin
  WriteLn('[', DateTimeToStr(Now), '] Vérification des consentements expirés...');

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := DatabaseConnection;

    // Les consentements de plus de 2 ans doivent être renouvelés
    Query.SQL.Text :=
      'SELECT COUNT(*) as cnt FROM consent_log ' +
      'WHERE consent_date < :cutoff AND withdrawn = 0';
    Query.ParamByName('cutoff').AsDateTime := IncYear(Now, -2);
    Query.Open;

    ExpiredCount := Query.FieldByName('cnt').AsInteger;
    Query.Close;

    if ExpiredCount > 0 then
      WriteLn(Format('ATTENTION : %d consentements à renouveler', [ExpiredCount]));

  finally
    Query.Free;
  end;
end;

procedure TGDPRScheduler.RunScheduledTasks;
var
  DaysSinceLastCleanup: Integer;
begin
  // Vérifier si le nettoyage est nécessaire
  if FLastCleanup = 0 then
    DaysSinceLastCleanup := MaxInt
  else
    DaysSinceLastCleanup := DaysBetween(Now, FLastCleanup);

  if DaysSinceLastCleanup >= FCleanupIntervalDays then
  begin
    ExecuteCleanup;
  end;

  // Rapport d'audit mensuel (le 1er du mois)
  if DayOf(Now) = 1 then
  begin
    ExecuteAuditReport;
  end;

  // Vérification quotidienne des consentements
  CheckExpiredConsents;
end;

end.
```

**Configuration d'une tâche planifiée :**

**Windows (Task Scheduler) :**
```batch
REM Créer une tâche planifiée Windows
schtasks /create /tn "GDPR Cleanup" /tr "C:\MyApp\myapp.exe --gdpr-cleanup" /sc daily /st 02:00
```

**Linux (cron) :**
```bash
# Éditer le crontab
crontab -e

# Ajouter une ligne pour exécution quotidienne à 2h du matin
0 2 * * * /usr/local/bin/myapp --gdpr-cleanup >> /var/log/gdpr_cleanup.log 2>&1
```

### 2. Dashboard de conformité

```pascal
unit GDPRDashboard;

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, ComCtrls;

type
  TGDPRDashboardForm = class(TForm)
  private
    FComplianceScore: TLabel;
    FPendingRequests: TLabel;
    FRecentBreaches: TMemo;
    FStatistics: TListView;

    procedure UpdateDashboard;
    procedure LoadStatistics;
    procedure LoadPendingRequests;
    procedure LoadRecentBreaches;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshData;
  end;

implementation

uses
  GDPRComplianceChecker, GDPRDataErasure, GDPRBreachNotification;

constructor TGDPRDashboardForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := 'Tableau de bord GDPR';
  Width := 800;
  Height := 600;

  // Créer les composants visuels
  FComplianceScore := TLabel.Create(Self);
  FComplianceScore.Parent := Self;
  FComplianceScore.Left := 20;
  FComplianceScore.Top := 20;
  FComplianceScore.Font.Size := 16;
  FComplianceScore.Font.Style := [fsBold];

  FPendingRequests := TLabel.Create(Self);
  FPendingRequests.Parent := Self;
  FPendingRequests.Left := 20;
  FPendingRequests.Top := 60;

  FRecentBreaches := TMemo.Create(Self);
  FRecentBreaches.Parent := Self;
  FRecentBreaches.Left := 20;
  FRecentBreaches.Top := 100;
  FRecentBreaches.Width := 760;
  FRecentBreaches.Height := 150;
  FRecentBreaches.ReadOnly := True;

  FStatistics := TListView.Create(Self);
  FStatistics.Parent := Self;
  FStatistics.Left := 20;
  FStatistics.Top := 270;
  FStatistics.Width := 760;
  FStatistics.Height := 300;
  FStatistics.ViewStyle := vsReport;

  // Colonnes
  FStatistics.Columns.Add.Caption := 'Métrique';
  FStatistics.Columns.Add.Caption := 'Valeur';
  FStatistics.Columns[0].Width := 400;
  FStatistics.Columns[1].Width := 350;

  UpdateDashboard;
end;

procedure TGDPRDashboardForm.UpdateDashboard;
begin
  LoadStatistics;
  LoadPendingRequests;
  LoadRecentBreaches;
end;

procedure TGDPRDashboardForm.LoadStatistics;
var
  Query: TSQLQuery;
  Item: TListItem;
  ComplianceChecker: TGDPRComplianceChecker;
begin
  FStatistics.Items.Clear;

  // Score de conformité
  ComplianceChecker := TGDPRComplianceChecker.Create;
  try
    FComplianceScore.Caption := Format('Score de conformité : %.1f%%',
      [ComplianceChecker.GetComplianceScore]);
  finally
    ComplianceChecker.Free;
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := DatabaseConnection;

    // Nombre total d'utilisateurs
    Query.SQL.Text := 'SELECT COUNT(*) as cnt FROM users WHERE gdpr_anonymized = 0';
    Query.Open;
    Item := FStatistics.Items.Add;
    Item.Caption := 'Utilisateurs actifs';
    Item.SubItems.Add(Query.FieldByName('cnt').AsString);
    Query.Close;

    // Utilisateurs anonymisés
    Query.SQL.Text := 'SELECT COUNT(*) as cnt FROM users WHERE gdpr_anonymized = 1';
    Query.Open;
    Item := FStatistics.Items.Add;
    Item.Caption := 'Utilisateurs anonymisés';
    Item.SubItems.Add(Query.FieldByName('cnt').AsString);
    Query.Close;

    // Consentements marketing actifs
    Query.SQL.Text :=
      'SELECT COUNT(*) as cnt FROM consent_log ' +
      'WHERE purpose = ''marketing'' AND withdrawn = 0';
    Query.Open;
    Item := FStatistics.Items.Add;
    Item.Caption := 'Consentements marketing actifs';
    Item.SubItems.Add(Query.FieldByName('cnt').AsString);
    Query.Close;

    // Événements d'audit ce mois
    Query.SQL.Text :=
      'SELECT COUNT(*) as cnt FROM gdpr_audit_log ' +
      'WHERE event_date >= DATE_SUB(NOW(), INTERVAL 30 DAY)';
    Query.Open;
    Item := FStatistics.Items.Add;
    Item.Caption := 'Événements d''audit (30 jours)';
    Item.SubItems.Add(Query.FieldByName('cnt').AsString);
    Query.Close;

  finally
    Query.Free;
  end;
end;

procedure TGDPRDashboardForm.LoadPendingRequests;
var
  Query: TSQLQuery;
  PendingCount: Integer;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := DatabaseConnection;
    Query.SQL.Text :=
      'SELECT COUNT(*) as cnt FROM rectification_requests WHERE status = ''pending''';
    Query.Open;

    PendingCount := Query.FieldByName('cnt').AsInteger;

    if PendingCount > 0 then
      FPendingRequests.Caption := Format('⚠ %d demandes en attente', [PendingCount])
    else
      FPendingRequests.Caption := '✓ Aucune demande en attente';

    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TGDPRDashboardForm.LoadRecentBreaches;
var
  Query: TSQLQuery;
begin
  FRecentBreaches.Clear;
  FRecentBreaches.Lines.Add('Violations récentes :');
  FRecentBreaches.Lines.Add('');

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := DatabaseConnection;
    Query.SQL.Text :=
      'SELECT * FROM data_breaches ' +
      'ORDER BY detected_date DESC LIMIT 5';
    Query.Open;

    if Query.RecordCount = 0 then
      FRecentBreaches.Lines.Add('✓ Aucune violation enregistrée')
    else
    begin
      while not Query.EOF do
      begin
        FRecentBreaches.Lines.Add(Format('[%s] Gravité: %d - %s',
          [DateTimeToStr(Query.FieldByName('detected_date').AsDateTime),
           Query.FieldByName('severity').AsInteger,
           Query.FieldByName('description').AsString]));
        Query.Next;
      end;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TGDPRDashboardForm.RefreshData;
begin
  UpdateDashboard;
end;

end.
```

## Sanctions et risques de non-conformité

### Amendes GDPR

Le GDPR prévoit des sanctions importantes en cas de non-conformité :

- **Niveau 1** : Jusqu'à **10 millions d'euros** ou **2% du chiffre d'affaires annuel mondial** (le montant le plus élevé)
- **Niveau 2** : Jusqu'à **20 millions d'euros** ou **4% du chiffre d'affaires annuel mondial**

### Exemples de violations courantes

```pascal
unit GDPRViolationChecker;

interface

uses
  Classes, SysUtils;

type
  TViolationType = (
    vtNoLegalBasis,           // Pas de base légale
    vtExcessiveCollection,    // Collecte excessive
    vtNoConsent,              // Pas de consentement
    vtInsecureStorage,        // Stockage non sécurisé
    vtNoBreachNotification,   // Pas de notification de violation
    vtNoDataProtectionOfficer // Pas de DPO (si requis)
  );

  TGDPRViolationCheck = record
    ViolationType: TViolationType;
    Description: string;
    Severity: Integer;  // 1-5
    RecommendedAction: string;
  end;

function CheckCommonViolations: array of TGDPRViolationCheck;

implementation

function CheckCommonViolations: array of TGDPRViolationCheck;
begin
  SetLength(Result, 6);

  Result[0].ViolationType := vtNoLegalBasis;
  Result[0].Description := 'Traitement sans base légale identifiée';
  Result[0].Severity := 5;
  Result[0].RecommendedAction := 'Documenter la base légale pour chaque traitement';

  Result[1].ViolationType := vtExcessiveCollection;
  Result[1].Description := 'Collecte de données non nécessaires';
  Result[1].Severity := 4;
  Result[1].RecommendedAction := 'Réviser les formulaires, supprimer les champs inutiles';

  Result[2].ViolationType := vtNoConsent;
  Result[2].Description := 'Traitement marketing sans consentement';
  Result[2].Severity := 5;
  Result[2].RecommendedAction := 'Mettre en place un système de consentement explicite';

  Result[3].ViolationType := vtInsecureStorage;
  Result[3].Description := 'Données personnelles stockées en clair';
  Result[3].Severity := 5;
  Result[3].RecommendedAction := 'Implémenter le chiffrement des données sensibles';

  Result[4].ViolationType := vtNoBreachNotification;
  Result[4].Description := 'Violation non notifiée dans les 72h';
  Result[4].Severity := 5;
  Result[4].RecommendedAction := 'Établir une procédure de notification des violations';

  Result[5].ViolationType := vtNoDataProtectionOfficer;
  Result[5].Description := 'Absence de DPO (si requis)';
  Result[5].Severity := 3;
  Result[5].RecommendedAction := 'Nommer un DPO si le traitement à grande échelle';
end;

end.
```

## Ressources et aide

### Autorités de protection des données

**France - CNIL**
- Site : https://www.cnil.fr
- Tel : +33 1 53 73 22 22
- Formulaire de plainte en ligne

**Allemagne - BfDI**
- Site : https://www.bfdi.bund.de
- Email : poststelle@bfdi.bund.de

**Royaume-Uni - ICO**
- Site : https://ico.org.uk
- Tel : 0303 123 1113

**Union Européenne - EDPB**
- Site : https://edpb.europa.eu

### Documentation officielle

```pascal
const
  GDPR_RESOURCES: array[0..4] of record
    Name: string;
    URL: string;
    Description: string;
  end = (
    (Name: 'Texte officiel GDPR';
     URL: 'https://eur-lex.europa.eu/eli/reg/2016/679/oj';
     Description: 'Règlement (UE) 2016/679'),

    (Name: 'Guide CNIL';
     URL: 'https://www.cnil.fr/fr/rgpd-passer-a-laction';
     Description: 'Guide pratique de mise en conformité'),

    (Name: 'ICO Guide';
     URL: 'https://ico.org.uk/for-organisations/guide-to-data-protection/';
     Description: 'Guide complet en anglais'),

    (Name: 'EDPB Guidelines';
     URL: 'https://edpb.europa.eu/our-work-tools/general-guidance_en';
     Description: 'Lignes directrices européennes'),

    (Name: 'ISO 27701';
     URL: 'https://www.iso.org/standard/71670.html';
     Description: 'Norme ISO pour la gestion de la vie privée')
  );
```

## Conclusion

La conformité GDPR est un processus continu qui nécessite :

### Points essentiels à retenir

1. **Base légale** : Toujours identifier la base légale du traitement
2. **Consentement** : Obtenir et documenter le consentement explicite
3. **Droits des utilisateurs** : Implémenter tous les droits GDPR
4. **Sécurité** : Protéger les données par des mesures techniques appropriées
5. **Audit** : Tenir un journal d'audit complet et immuable
6. **Violations** : Procédure de notification dans les 72 heures
7. **Conservation** : Supprimer les données après la période de conservation
8. **Transparence** : Informer clairement les utilisateurs

### Architecture recommandée pour FreePascal/Lazarus

```
Application FreePascal/Lazarus
├── Units GDPR
│   ├── GDPRConsent.pas        (Gestion du consentement)
│   ├── GDPRDataAccess.pas     (Droit d'accès)
│   ├── GDPRDataErasure.pas    (Droit à l'oubli)
│   ├── GDPRAuditSystem.pas    (Système d'audit)
│   ├── GDPRRetention.pas      (Politique de conservation)
│   └── GDPRBreach.pas         (Notification de violations)
├── Base de données
│   ├── Tables utilisateurs avec champs GDPR
│   ├── Tables d'audit et de logs
│   ├── Tables de consentement
│   └── Tables de violations
└── Tâches planifiées
    ├── Nettoyage automatique
    ├── Rapports mensuels
    └── Vérification des consentements
```

### Checklist finale de déploiement

```
[ ] Politique de confidentialité rédigée et accessible
[ ] Base légale documentée pour chaque traitement
[ ] Système de consentement fonctionnel
[ ] Tous les droits GDPR implémentés (accès, rectification, effacement, etc.)
[ ] Chiffrement des données sensibles activé
[ ] Système d'audit en place et fonctionnel
[ ] Politique de conservation définie et automatisée
[ ] Procédure de notification de violation établie
[ ] Tests de conformité effectués
[ ] Formation de l'équipe sur le GDPR
[ ] DPO nommé (si requis)
[ ] Contrats DPA avec les sous-traitants
[ ] Documentation technique complète
[ ] Plan de réponse aux demandes des utilisateurs
[ ] Monitoring et alertes configurés
```

### Multi-plateforme (Windows/Ubuntu)

Les implémentations fournies dans ce chapitre fonctionnent sur les deux plateformes grâce à :

- **Directives de compilation** : `{$IFDEF WINDOWS}` et `{$IFDEF UNIX}`
- **Abstraction de la base de données** : SQLdb fonctionne de manière identique
- **Chemins de fichiers** : Utilisation de fonctions cross-platform
- **Logging** : Compatible avec les deux systèmes
- **Tâches planifiées** : Task Scheduler (Windows) et cron (Linux)

### Maintenance continue

La conformité GDPR n'est pas un état figé mais un processus :

1. **Revue trimestrielle** : Vérifier la conformité
2. **Mise à jour des politiques** : Adapter aux évolutions légales
3. **Formation continue** : Tenir l'équipe informée
4. **Audits réguliers** : Contrôler les accès et traitements
5. **Veille juridique** : Suivre les décisions et guidelines

### En cas de doute

Si vous n'êtes pas sûr de la conformité de votre application :

1. Consulter un avocat spécialisé en protection des données
2. Contacter votre autorité de protection des données nationale
3. Faire réaliser un audit GDPR par un expert
4. Nommer un DPO (Délégué à la Protection des Données)

**Rappel important** : Ce tutoriel fournit des orientations techniques, mais ne constitue pas un conseil juridique. Pour des questions légales spécifiques, consultez toujours un professionnel du droit.

La conformité GDPR peut sembler complexe, mais avec les bons outils et une approche méthodique, elle est tout à fait réalisable pour les applications FreePascal/Lazarus, que ce soit sous Windows ou Ubuntu. L'investissement dans la conformité protège à la fois vos utilisateurs et votre organisation.

⏭️ [SELinux/AppArmor vs Windows Defender](/17-securite-cryptographie/11-selinux-apparmor-vs-windows-defender.md)
