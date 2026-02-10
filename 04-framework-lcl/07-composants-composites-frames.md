🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.7 Composants composites et frames

## Introduction : Qu'est-ce qu'un composant composite ?

Imaginez que vous devez créer plusieurs fois la même combinaison de composants dans votre application. Par exemple, un champ de saisie avec son label, un bouton de validation et un indicateur d'erreur. Plutôt que de recréer cette combinaison à chaque fois, vous pouvez créer un **composant composite** qui regroupe tous ces éléments en un seul.

Un composant composite est comme un kit préfabriqué : tous les éléments sont assemblés et configurés une fois pour toutes, et vous pouvez réutiliser l'ensemble partout où vous en avez besoin.

Les **frames** (TFrame) sont l'outil principal de Lazarus pour créer des composants composites visuellement, comme vous créez un formulaire.

## Comprendre la différence entre Frame et Panel

### Panel vs Frame : Quand utiliser quoi ?

**TPanel** est un simple conteneur :
- Créé par code ou visuellement
- Pas de fichier de conception séparé
- Idéal pour organiser l'interface

**TFrame** est un mini-formulaire réutilisable :
- Conçu visuellement comme un formulaire
- Fichier .lfm séparé pour le design
- Peut contenir sa propre logique métier
- Réutilisable dans plusieurs formulaires

```pascal
// Panel simple - création par code
Panel1 := TPanel.Create(Self);  
Panel1.Parent := Form1;  
Button1 := TButton.Create(Panel1);  
Button1.Parent := Panel1;

// Frame - déjà conçu visuellement
Frame1 := TMyFrame.Create(Self);  
Frame1.Parent := Form1;
// Tous les composants du frame sont déjà créés !
```

## Créer votre premier Frame

### Étape 1 : Créer un nouveau Frame

Dans Lazarus :
1. Menu Fichier → Nouveau → Frame
2. Lazarus crée deux fichiers :
   - `Unit1.pas` : Le code
   - `Unit1.lfm` : Le design visuel

### Étape 2 : Concevoir le Frame

Créons un frame pour la saisie d'une adresse :

```pascal
unit AddressFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics;

type
  TAddressFrame = class(TFrame)
    // Composants créés visuellement
    lblStreet: TLabel;
    edtStreet: TEdit;
    lblCity: TLabel;
    edtCity: TEdit;
    lblPostalCode: TLabel;
    edtPostalCode: TEdit;
    lblCountry: TLabel;
    cboCountry: TComboBox;

    procedure edtPostalCodeKeyPress(Sender: TObject; var Key: char);
    procedure FrameResize(Sender: TObject);
  private
    FOnAddressChanged: TNotifyEvent;
    FRequired: Boolean;

    function GetAddress: string;
    procedure SetAddress(const Value: string);
    procedure SetRequired(Value: Boolean);
    procedure ValidateFields;
  public
    constructor Create(AOwner: TComponent); override;

    // Méthodes publiques
    procedure Clear;
    function IsValid: Boolean;
    function IsEmpty: Boolean;

    // Propriétés
    property Address: string read GetAddress write SetAddress;
    property Required: Boolean read FRequired write SetRequired;

    // Événements
    property OnAddressChanged: TNotifyEvent read FOnAddressChanged write FOnAddressChanged;
  end;

implementation

{$R *.lfm}

constructor TAddressFrame.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  // Initialisation du frame
  // Les composants sont créés automatiquement depuis le .lfm

  // Remplir la liste des pays
  cboCountry.Items.Clear;
  cboCountry.Items.Add('France');
  cboCountry.Items.Add('Belgique');
  cboCountry.Items.Add('Suisse');
  cboCountry.Items.Add('Canada');
  cboCountry.Items.Add('Luxembourg');

  // Configuration initiale
  FRequired := False;
end;

function TAddressFrame.GetAddress: string;  
begin
  Result := Format('%s, %s %s, %s',
    [edtStreet.Text, edtPostalCode.Text, edtCity.Text, cboCountry.Text]);
end;

procedure TAddressFrame.SetAddress(const Value: string);  
begin
  // Analyse simplifiée de l'adresse
  // Dans un vrai projet, utilisez un parser plus sophistiqué
  Clear;
  if Value <> '' then
  begin
    // Parser l'adresse...
    edtStreet.Text := ExtractDelimited(1, Value, [',']);
    // etc.
  end;
end;

procedure TAddressFrame.Clear;  
begin
  edtStreet.Clear;
  edtCity.Clear;
  edtPostalCode.Clear;
  cboCountry.ItemIndex := -1;
end;

function TAddressFrame.IsValid: Boolean;  
begin
  Result := True;

  if FRequired then
  begin
    Result := (Trim(edtStreet.Text) <> '') and
              (Trim(edtCity.Text) <> '') and
              (Trim(edtPostalCode.Text) <> '') and
              (cboCountry.ItemIndex >= 0);
  end;

  // Validation du code postal
  if Result and (edtPostalCode.Text <> '') then
  begin
    // Vérifier le format selon le pays
    if cboCountry.Text = 'France' then
      Result := Length(edtPostalCode.Text) = 5
    else if cboCountry.Text = 'Canada' then
      Result := Length(edtPostalCode.Text) = 6;
    // etc.
  end;
end;

function TAddressFrame.IsEmpty: Boolean;  
begin
  Result := (Trim(edtStreet.Text) = '') and
            (Trim(edtCity.Text) = '') and
            (Trim(edtPostalCode.Text) = '') and
            (cboCountry.ItemIndex < 0);
end;

procedure TAddressFrame.SetRequired(Value: Boolean);  
begin
  FRequired := Value;
  ValidateFields;
end;

procedure TAddressFrame.ValidateFields;  
begin
  // Indicateur visuel pour les champs requis
  if FRequired then
  begin
    lblStreet.Caption := 'Rue *';
    lblCity.Caption := 'Ville *';
    lblPostalCode.Caption := 'Code postal *';
    lblCountry.Caption := 'Pays *';
  end
  else
  begin
    lblStreet.Caption := 'Rue';
    lblCity.Caption := 'Ville';
    lblPostalCode.Caption := 'Code postal';
    lblCountry.Caption := 'Pays';
  end;
end;

procedure TAddressFrame.edtPostalCodeKeyPress(Sender: TObject; var Key: char);  
begin
  // Accepter uniquement les chiffres pour la France
  if cboCountry.Text = 'France' then
  begin
    if not (Key in ['0'..'9', #8, #13]) then
      Key := #0;
  end;
end;

procedure TAddressFrame.FrameResize(Sender: TObject);  
begin
  // Adapter la disposition selon la taille
  if Width < 300 then
  begin
    // Mode compact : labels au-dessus
    lblStreet.Left := 8;
    lblStreet.Top := 8;
    edtStreet.Left := 8;
    edtStreet.Top := 28;
    edtStreet.Width := Width - 16;
    // etc. pour les autres composants
  end
  else
  begin
    // Mode normal : labels à gauche
    lblStreet.Left := 8;
    lblStreet.Top := 12;
    edtStreet.Left := 100;
    edtStreet.Top := 8;
    edtStreet.Width := Width - 108;
    // etc.
  end;
end;
```

## Utiliser un Frame dans vos formulaires

### Méthode 1 : Insertion visuelle

1. Ouvrez votre formulaire dans le concepteur
2. Dans la palette de composants, onglet "Standard" → "Frame"
3. Cliquez sur le formulaire pour placer le frame
4. Dans l'inspecteur d'objets, propriété "FrameClass" → Choisir votre frame

### Méthode 2 : Création par code

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, AddressFrame;

type
  TFormMain = class(TForm)
    btnValidate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
  private
    FHomeAddress: TAddressFrame;
    FWorkAddress: TAddressFrame;
  public
    // Propriétés publiques
  end;

implementation

procedure TFormMain.FormCreate(Sender: TObject);  
begin
  // Créer le frame pour l'adresse personnelle
  FHomeAddress := TAddressFrame.Create(Self);
  FHomeAddress.Parent := Self;
  FHomeAddress.Name := 'HomeAddress';
  FHomeAddress.Left := 10;
  FHomeAddress.Top := 10;
  FHomeAddress.Width := 400;
  FHomeAddress.Height := 150;
  FHomeAddress.Required := True;

  // Créer le frame pour l'adresse professionnelle
  FWorkAddress := TAddressFrame.Create(Self);
  FWorkAddress.Parent := Self;
  FWorkAddress.Name := 'WorkAddress';
  FWorkAddress.Left := 10;
  FWorkAddress.Top := 170;
  FWorkAddress.Width := 400;
  FWorkAddress.Height := 150;
  FWorkAddress.Required := False;

  // Positionner le bouton de validation
  btnValidate.Top := 330;
end;

procedure TFormMain.btnValidateClick(Sender: TObject);  
begin
  // Valider les deux adresses
  if not FHomeAddress.IsValid then
  begin
    ShowMessage('L''adresse personnelle est incomplète');
    Exit;
  end;

  if FWorkAddress.Required and not FWorkAddress.IsValid then
  begin
    ShowMessage('L''adresse professionnelle est incomplète');
    Exit;
  end;

  // Traiter les données
  ShowMessage('Adresse personnelle : ' + FHomeAddress.Address + #13#10 +
              'Adresse professionnelle : ' + FWorkAddress.Address);
end;
```

## Créer des composants composites complexes

### Composant composite avec logique métier

```pascal
unit LoginFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Dialogs;

type
  TLoginState = (lsLoggedOut, lsLoggingIn, lsLoggedIn, lsError);

  TLoginFrame = class(TFrame)
    pnlLogin: TPanel;
    lblUsername: TLabel;
    edtUsername: TEdit;
    lblPassword: TLabel;
    edtPassword: TEdit;
    chkRemember: TCheckBox;
    btnLogin: TButton;
    btnCancel: TButton;
    pnlStatus: TPanel;
    lblStatus: TLabel;
    imgStatus: TImage;
    tmrTimeout: TTimer;

    procedure btnLoginClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure edtPasswordKeyPress(Sender: TObject; var Key: char);
    procedure tmrTimeoutTimer(Sender: TObject);
  private
    FState: TLoginState;
    FOnLogin: TNotifyEvent;
    FOnLogout: TNotifyEvent;
    FMaxAttempts: Integer;
    FAttemptCount: Integer;
    FTimeoutSeconds: Integer;

    procedure SetState(Value: TLoginState);
    procedure UpdateUI;
    procedure DoLogin;
    function ValidateCredentials(const Username, Password: string): Boolean;
    procedure LoadSavedCredentials;
    procedure SaveCredentials;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Méthodes publiques
    procedure Reset;
    procedure Logout;

    // Propriétés
    property State: TLoginState read FState;
    property MaxAttempts: Integer read FMaxAttempts write FMaxAttempts;
    property TimeoutSeconds: Integer read FTimeoutSeconds write FTimeoutSeconds;

    // Événements
    property OnLogin: TNotifyEvent read FOnLogin write FOnLogin;
    property OnLogout: TNotifyEvent read FOnLogout write FOnLogout;
  end;

implementation

uses
  IniFiles, MD5;

{$R *.lfm}

constructor TLoginFrame.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  FState := lsLoggedOut;
  FMaxAttempts := 3;
  FAttemptCount := 0;
  FTimeoutSeconds := 30;

  // Charger les identifiants sauvegardés
  LoadSavedCredentials;

  UpdateUI;
end;

destructor TLoginFrame.Destroy;  
begin
  // Nettoyer les ressources
  inherited Destroy;
end;

procedure TLoginFrame.SetState(Value: TLoginState);  
begin
  if FState <> Value then
  begin
    FState := Value;
    UpdateUI;
  end;
end;

procedure TLoginFrame.UpdateUI;  
begin
  case FState of
    lsLoggedOut:
      begin
        pnlLogin.Enabled := True;
        btnLogin.Caption := 'Connexion';
        btnLogin.Enabled := True;
        lblStatus.Caption := 'Veuillez vous connecter';
        lblStatus.Font.Color := clDefault;
        imgStatus.Visible := False;
        edtUsername.Color := clWindow;
        edtPassword.Color := clWindow;
      end;

    lsLoggingIn:
      begin
        pnlLogin.Enabled := False;
        btnLogin.Caption := 'Connexion...';
        lblStatus.Caption := 'Vérification en cours...';
        lblStatus.Font.Color := clBlue;
        imgStatus.Visible := True;
        // Afficher une animation de chargement
      end;

    lsLoggedIn:
      begin
        pnlLogin.Enabled := False;
        btnLogin.Caption := 'Connecté';
        lblStatus.Caption := Format('Bienvenue %s !', [edtUsername.Text]);
        lblStatus.Font.Color := clGreen;
        imgStatus.Visible := True;
        // Afficher une icône de succès

        if Assigned(FOnLogin) then
          FOnLogin(Self);
      end;

    lsError:
      begin
        pnlLogin.Enabled := True;
        btnLogin.Enabled := FAttemptCount < FMaxAttempts;

        if FAttemptCount >= FMaxAttempts then
        begin
          lblStatus.Caption := 'Trop de tentatives. Veuillez patienter...';
          tmrTimeout.Interval := FTimeoutSeconds * 1000;
          tmrTimeout.Enabled := True;
        end
        else
        begin
          lblStatus.Caption := Format('Échec. Tentative %d/%d',
            [FAttemptCount, FMaxAttempts]);
        end;

        lblStatus.Font.Color := clRed;
        edtPassword.Color := $FFE0E0;  // Rouge clair
        edtPassword.Clear;
        edtPassword.SetFocus;
      end;
  end;
end;

procedure TLoginFrame.btnLoginClick(Sender: TObject);  
begin
  DoLogin;
end;

procedure TLoginFrame.DoLogin;  
begin
  // Validation des champs
  if Trim(edtUsername.Text) = '' then
  begin
    ShowMessage('Veuillez entrer un nom d''utilisateur');
    edtUsername.SetFocus;
    Exit;
  end;

  if Trim(edtPassword.Text) = '' then
  begin
    ShowMessage('Veuillez entrer un mot de passe');
    edtPassword.SetFocus;
    Exit;
  end;

  SetState(lsLoggingIn);
  Application.ProcessMessages;

  // Simuler une vérification (remplacer par vraie authentification)
  Sleep(1000);  // Simule le délai réseau

  if ValidateCredentials(edtUsername.Text, edtPassword.Text) then
  begin
    FAttemptCount := 0;

    if chkRemember.Checked then
      SaveCredentials;

    SetState(lsLoggedIn);
  end
  else
  begin
    Inc(FAttemptCount);
    SetState(lsError);
  end;
end;

function TLoginFrame.ValidateCredentials(const Username, Password: string): Boolean;  
begin
  // Exemple simple - À remplacer par une vraie authentification
  Result := (Username = 'admin') and (Password = 'password');

  // Dans un vrai système :
  // - Hasher le mot de passe
  // - Vérifier contre une base de données
  // - Utiliser HTTPS pour la communication
end;

procedure TLoginFrame.LoadSavedCredentials;  
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create('login.ini');
  try
    edtUsername.Text := Ini.ReadString('Login', 'Username', '');
    chkRemember.Checked := Ini.ReadBool('Login', 'Remember', False);
    // Ne JAMAIS sauvegarder le mot de passe en clair !
  finally
    Ini.Free;
  end;
end;

procedure TLoginFrame.SaveCredentials;  
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create('login.ini');
  try
    Ini.WriteString('Login', 'Username', edtUsername.Text);
    Ini.WriteBool('Login', 'Remember', True);
    // Ne pas sauvegarder le mot de passe !
  finally
    Ini.Free;
  end;
end;

procedure TLoginFrame.edtPasswordKeyPress(Sender: TObject; var Key: char);  
begin
  if Key = #13 then  // Entrée
  begin
    Key := #0;
    DoLogin;
  end;
end;

procedure TLoginFrame.btnCancelClick(Sender: TObject);  
begin
  Reset;
end;

procedure TLoginFrame.Reset;  
begin
  edtPassword.Clear;
  FAttemptCount := 0;
  SetState(lsLoggedOut);
end;

procedure TLoginFrame.Logout;  
begin
  Reset;
  if Assigned(FOnLogout) then
    FOnLogout(Self);
end;

procedure TLoginFrame.tmrTimeoutTimer(Sender: TObject);  
begin
  tmrTimeout.Enabled := False;
  FAttemptCount := 0;
  SetState(lsLoggedOut);
end;
```

## Frames imbriqués et héritage

### Créer une hiérarchie de frames

```pascal
unit BaseFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics;

type
  // Frame de base avec fonctionnalités communes
  TBaseFrame = class(TFrame)
  private
    FModified: Boolean;
    FOnModified: TNotifyEvent;
  protected
    procedure DoModified; virtual;
    procedure SetModified(Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    // Méthodes virtuelles à surcharger
    procedure LoadData; virtual; abstract;
    procedure SaveData; virtual; abstract;
    function ValidateData: Boolean; virtual;
    procedure ClearData; virtual;

    property Modified: Boolean read FModified write SetModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

  // Frame dérivé pour les données client
  TCustomerFrame = class(TBaseFrame)
  private
    FCustomerID: Integer;
  protected
    procedure DoModified; override;
  public
    procedure LoadData; override;
    procedure SaveData; override;
    function ValidateData: Boolean; override;

    property CustomerID: Integer read FCustomerID write FCustomerID;
  end;

implementation

constructor TBaseFrame.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FModified := False;
end;

procedure TBaseFrame.DoModified;  
begin
  FModified := True;
  if Assigned(FOnModified) then
    FOnModified(Self);
end;

procedure TBaseFrame.SetModified(Value: Boolean);  
begin
  if FModified <> Value then
  begin
    FModified := Value;
    if FModified then
      DoModified;
  end;
end;

function TBaseFrame.ValidateData: Boolean;  
begin
  // Validation de base
  Result := True;
end;

procedure TBaseFrame.ClearData;  
begin
  // Réinitialisation de base
  FModified := False;
end;

{ TCustomerFrame }

procedure TCustomerFrame.DoModified;  
begin
  inherited DoModified;
  // Actions spécifiques au frame client
  Caption := Caption + ' *';  // Indicateur de modification
end;

procedure TCustomerFrame.LoadData;  
begin
  // Charger les données du client
  // SELECT * FROM Customers WHERE ID = FCustomerID
  Modified := False;
end;

procedure TCustomerFrame.SaveData;  
begin
  if not ValidateData then
    raise Exception.Create('Données invalides');

  // Sauvegarder les données
  // UPDATE Customers SET ... WHERE ID = FCustomerID
  Modified := False;
end;

function TCustomerFrame.ValidateData: Boolean;  
begin
  Result := inherited ValidateData;

  // Validations spécifiques au client
  // Result := Result and (CustomerName <> '');
end;
```

## Communication entre frames

### Pattern Médiateur pour la communication

```pascal
unit FrameMediator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TFrameMessage = class
  public
    Sender: TObject;
    MessageType: string;
    Data: Variant;
  end;

  TFrameMediator = class
  private
    FFrames: TObjectList;
    FMessages: TQueue;

    procedure ProcessMessages;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterFrame(Frame: TFrame);
    procedure UnregisterFrame(Frame: TFrame);

    procedure SendMessage(Sender: TObject; const MsgType: string;
                         const Data: Variant);
    procedure BroadcastMessage(const MsgType: string; const Data: Variant);
  end;

// Singleton global
function Mediator: TFrameMediator;

implementation

var
  GMediatorInstance: TFrameMediator = nil;

function Mediator: TFrameMediator;  
begin
  if not Assigned(GMediatorInstance) then
    GMediatorInstance := TFrameMediator.Create;
  Result := GMediatorInstance;
end;

constructor TFrameMediator.Create;  
begin
  FFrames := TObjectList.Create(False);  // Ne pas posséder les frames
  FMessages := TQueue.Create;
end;

destructor TFrameMediator.Destroy;  
begin
  FMessages.Free;
  FFrames.Free;
  inherited;
end;

procedure TFrameMediator.RegisterFrame(Frame: TFrame);  
begin
  if FFrames.IndexOf(Frame) = -1 then
    FFrames.Add(Frame);
end;

procedure TFrameMediator.UnregisterFrame(Frame: TFrame);  
begin
  FFrames.Remove(Frame);
end;

procedure TFrameMediator.SendMessage(Sender: TObject; const MsgType: string;
                                    const Data: Variant);
var
  Msg: TFrameMessage;
begin
  Msg := TFrameMessage.Create;
  Msg.Sender := Sender;
  Msg.MessageType := MsgType;
  Msg.Data := Data;

  FMessages.Push(Msg);
  ProcessMessages;
end;

procedure TFrameMediator.BroadcastMessage(const MsgType: string;
                                         const Data: Variant);
begin
  SendMessage(nil, MsgType, Data);
end;

procedure TFrameMediator.ProcessMessages;  
var
  Msg: TFrameMessage;
  i: Integer;
  Frame: TFrame;
begin
  while FMessages.Count > 0 do
  begin
    Msg := TFrameMessage(FMessages.Pop);
    try
      for i := 0 to FFrames.Count - 1 do
      begin
        Frame := TFrame(FFrames[i]);

        // Appeler une méthode HandleMessage si elle existe
        if Frame.MethodAddress('HandleMessage') <> nil then
        begin
          // Appeler HandleMessage(Msg)
          // Utiliser RTTI ou interface pour l'appel
        end;
      end;
    finally
      Msg.Free;
    end;
  end;
end;

initialization

finalization
  FreeAndNil(GMediatorInstance);
end.
```

## Optimisation et bonnes pratiques

### Gestion de la mémoire dans les frames

```pascal
unit OptimizedFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics;

type
  TOptimizedFrame = class(TFrame)
  private
    FResourcesLoaded: Boolean;
    FLazyLoadTimer: TTimer;
    FHeavyData: TObject;

    procedure LoadResources;
    procedure FreeResources;
    procedure LazyLoadTimerTimer(Sender: TObject);
  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure VisibleChanging; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure OptimizeForDisplay;
    procedure OptimizeForHidden;
  end;

implementation

constructor TOptimizedFrame.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  FResourcesLoaded := False;

  // Timer pour chargement différé
  FLazyLoadTimer := TTimer.Create(Self);
  FLazyLoadTimer.Interval := 100;
  FLazyLoadTimer.Enabled := False;
  FLazyLoadTimer.OnTimer := @LazyLoadTimerTimer;
end;

destructor TOptimizedFrame.Destroy;  
begin
  FreeResources;
  inherited Destroy;
end;

procedure TOptimizedFrame.SetParent(NewParent: TWinControl);  
begin
  if (NewParent <> nil) and not FResourcesLoaded then
  begin
    // Démarrer le chargement différé
    FLazyLoadTimer.Enabled := True;
  end
  else if NewParent = nil then
  begin
    // Libérer les ressources si plus de parent
    FreeResources;
  end;

  inherited SetParent(NewParent);
end;

procedure TOptimizedFrame.VisibleChanging;  
begin
  inherited;

  if Visible then
    OptimizeForDisplay
  else
    OptimizeForHidden;
end;

procedure TOptimizedFrame.LoadResources;  
begin
  if FResourcesLoaded then Exit;

  // Charger les ressources lourdes
  FHeavyData := TObject.Create;  // Exemple

  // Charger les images, données, etc.

  FResourcesLoaded := True;
end;

procedure TOptimizedFrame.FreeResources;  
begin
  if not FResourcesLoaded then Exit;

  FreeAndNil(FHeavyData);

  FResourcesLoaded := False;
end;

procedure TOptimizedFrame.LazyLoadTimerTimer(Sender: TObject);  
begin
  FLazyLoadTimer.Enabled := False;

  if Visible and (Parent <> nil) then
    LoadResources;
end;

procedure TOptimizedFrame.OptimizeForDisplay;  
begin
  // Activer les mises à jour visuelles
  DoubleBuffered := True;

  // Charger les ressources si nécessaire
  if not FResourcesLoaded then
    LoadResources;
end;

procedure TOptimizedFrame.OptimizeForHidden;  
begin
  // Désactiver les mises à jour inutiles
  DoubleBuffered := False;

  // Optionnel : libérer certaines ressources
  // FreeResources;  // Si on veut être très agressif
end;
```

### Pattern Factory pour la création de frames

```pascal
unit FrameFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type
  TFrameClass = class of TFrame;

  TFrameFactory = class
  private
    class var FRegistry: TStringList;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure RegisterFrame(const Name: string; FrameClass: TFrameClass);
    class function CreateFrame(const Name: string; Owner: TComponent): TFrame;
    class function FrameExists(const Name: string): Boolean;
  end;

implementation

class constructor TFrameFactory.Create;  
begin
  FRegistry := TStringList.Create;
  FRegistry.Sorted := True;
end;

class destructor TFrameFactory.Destroy;  
begin
  FRegistry.Free;
end;

class procedure TFrameFactory.RegisterFrame(const Name: string;
                                           FrameClass: TFrameClass);
begin
  FRegistry.AddObject(UpperCase(Name), TObject(FrameClass));
end;

class function TFrameFactory.CreateFrame(const Name: string;
                                        Owner: TComponent): TFrame;
var
  Index: Integer;
  FrameClass: TFrameClass;
begin
  Result := nil;
  Index := FRegistry.IndexOf(UpperCase(Name));

  if Index >= 0 then
  begin
    FrameClass := TFrameClass(FRegistry.Objects[Index]);
    Result := FrameClass.Create(Owner);
  end
  else
    raise Exception.CreateFmt('Frame "%s" non enregistré', [Name]);
end;

class function TFrameFactory.FrameExists(const Name: string): Boolean;  
begin
  Result := FRegistry.IndexOf(UpperCase(Name)) >= 0;
end;
```

### Utilisation du Factory Pattern

```pascal
unit MainApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls,
  FrameFactory, AddressFrame, LoginFrame;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    pnlFrameContainer: TPanel;
    cboFrameSelector: TComboBox;
    btnCreateFrame: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnCreateFrameClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    FCurrentFrame: TFrame;
    FFrameCache: TStringList;

    procedure LoadFrameIntoTab(const FrameName: string; TabSheet: TTabSheet);
    function GetOrCreateFrame(const FrameName: string): TFrame;
    procedure ClearCurrentFrame;
  public
    destructor Destroy; override;
  end;

implementation

procedure TMainForm.FormCreate(Sender: TObject);  
begin
  FFrameCache := TStringList.Create;
  FFrameCache.OwnsObjects := True;

  // Enregistrer tous les frames disponibles
  TFrameFactory.RegisterFrame('Address', TAddressFrame);
  TFrameFactory.RegisterFrame('Login', TLoginFrame);
  TFrameFactory.RegisterFrame('Customer', TCustomerFrame);
  TFrameFactory.RegisterFrame('Product', TProductFrame);

  // Remplir la liste de sélection
  cboFrameSelector.Items.Clear;
  cboFrameSelector.Items.Add('Address');
  cboFrameSelector.Items.Add('Login');
  cboFrameSelector.Items.Add('Customer');
  cboFrameSelector.Items.Add('Product');
  cboFrameSelector.ItemIndex := 0;
end;

destructor TMainForm.Destroy;  
begin
  FFrameCache.Free;
  inherited;
end;

procedure TMainForm.btnCreateFrameClick(Sender: TObject);  
var
  TabSheet: TTabSheet;
  FrameName: string;
begin
  FrameName := cboFrameSelector.Text;

  // Créer un nouvel onglet
  TabSheet := TTabSheet.Create(PageControl);
  TabSheet.PageControl := PageControl;
  TabSheet.Caption := FrameName;

  // Charger le frame dans l'onglet
  LoadFrameIntoTab(FrameName, TabSheet);

  // Activer le nouvel onglet
  PageControl.ActivePage := TabSheet;
end;

procedure TMainForm.LoadFrameIntoTab(const FrameName: string; TabSheet: TTabSheet);  
var
  Frame: TFrame;
begin
  Frame := GetOrCreateFrame(FrameName);
  Frame.Parent := TabSheet;
  Frame.Align := alClient;

  // Stocker une référence au frame dans le Tag du TabSheet
  TabSheet.Tag := PtrInt(Frame);
end;

function TMainForm.GetOrCreateFrame(const FrameName: string): TFrame;  
var
  Index: Integer;
  CacheKey: string;
begin
  CacheKey := UpperCase(FrameName);
  Index := FFrameCache.IndexOf(CacheKey);

  if Index >= 0 then
  begin
    // Récupérer depuis le cache
    Result := TFrame(FFrameCache.Objects[Index]);
  end
  else
  begin
    // Créer un nouveau frame
    Result := TFrameFactory.CreateFrame(FrameName, Self);
    FFrameCache.AddObject(CacheKey, Result);
  end;
end;

procedure TMainForm.ClearCurrentFrame;  
begin
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Parent := nil;
    FCurrentFrame := nil;
  end;
end;

procedure TMainForm.PageControlChange(Sender: TObject);  
var
  ActiveTab: TTabSheet;
begin
  ActiveTab := PageControl.ActivePage;
  if Assigned(ActiveTab) and (ActiveTab.Tag <> 0) then
  begin
    FCurrentFrame := TFrame(ActiveTab.Tag);

    // Notifier le frame qu'il est activé
    if FCurrentFrame.MethodAddress('OnActivate') <> nil then
    begin
      // Appeler OnActivate si elle existe
    end;
  end;
end;
```

## Frames dynamiques et configurables

### Frame configurable par XML

```pascal
unit ConfigurableFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, DOM, XMLRead;

type
  TConfigurableFrame = class(TFrame)
  private
    FConfigFile: string;
    FComponents: TStringList;

    procedure LoadConfiguration(const FileName: string);
    procedure CreateControlFromXML(Node: TDOMNode; Parent: TWinControl);
    function StringToControlClass(const ClassName: string): TControlClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromXML(const XMLFile: string);
    procedure SaveToXML(const XMLFile: string);

    function FindComponentByName(const Name: string): TComponent;
  end;

implementation

constructor TConfigurableFrame.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FComponents := TStringList.Create;
  FComponents.OwnsObjects := True;
end;

destructor TConfigurableFrame.Destroy;  
begin
  FComponents.Free;
  inherited;
end;

procedure TConfigurableFrame.LoadFromXML(const XMLFile: string);  
begin
  FConfigFile := XMLFile;
  LoadConfiguration(XMLFile);
end;

procedure TConfigurableFrame.LoadConfiguration(const FileName: string);  
var
  Doc: TXMLDocument;
  Root, Node: TDOMNode;
  i: Integer;
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('Fichier de configuration non trouvé : %s', [FileName]);

  ReadXMLFile(Doc, FileName);
  try
    Root := Doc.DocumentElement;

    if Root.NodeName <> 'Frame' then
      raise Exception.Create('Format XML invalide');

    // Lire les propriétés du frame
    if Root.Attributes.GetNamedItem('Width') <> nil then
      Width := StrToInt(Root.Attributes.GetNamedItem('Width').NodeValue);

    if Root.Attributes.GetNamedItem('Height') <> nil then
      Height := StrToInt(Root.Attributes.GetNamedItem('Height').NodeValue);

    // Créer les composants enfants
    Node := Root.FirstChild;
    while Assigned(Node) do
    begin
      if Node.NodeType = ELEMENT_NODE then
        CreateControlFromXML(Node, Self);
      Node := Node.NextSibling;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TConfigurableFrame.CreateControlFromXML(Node: TDOMNode; Parent: TWinControl);  
var
  ControlClass: TControlClass;
  Control: TControl;
  PropName, PropValue: string;
  i: Integer;
  Attr: TDOMNode;
begin
  // Obtenir la classe du contrôle
  ControlClass := StringToControlClass(Node.NodeName);
  if not Assigned(ControlClass) then
    Exit;

  // Créer le contrôle
  Control := ControlClass.Create(Self);
  Control.Parent := Parent;

  // Définir les propriétés depuis les attributs XML
  for i := 0 to Node.Attributes.Length - 1 do
  begin
    Attr := Node.Attributes.Item[i];
    PropName := Attr.NodeName;
    PropValue := Attr.NodeValue;

    // Définir les propriétés communes
    if PropName = 'Name' then
      Control.Name := PropValue
    else if PropName = 'Left' then
      Control.Left := StrToInt(PropValue)
    else if PropName = 'Top' then
      Control.Top := StrToInt(PropValue)
    else if PropName = 'Width' then
      Control.Width := StrToInt(PropValue)
    else if PropName = 'Height' then
      Control.Height := StrToInt(PropValue)
    else if PropName = 'Caption' then
    begin
      if Control is TButton then
        TButton(Control).Caption := PropValue
      else if Control is TLabel then
        TLabel(Control).Caption := PropValue;
    end;
  end;

  // Ajouter à la liste des composants
  FComponents.AddObject(Control.Name, Control);

  // Créer les enfants si c'est un conteneur
  if Control is TWinControl then
  begin
    Node := Node.FirstChild;
    while Assigned(Node) do
    begin
      if Node.NodeType = ELEMENT_NODE then
        CreateControlFromXML(Node, TWinControl(Control));
      Node := Node.NextSibling;
    end;
  end;
end;

function TConfigurableFrame.StringToControlClass(const ClassName: string): TControlClass;  
begin
  Result := nil;

  if ClassName = 'Button' then
    Result := TButton
  else if ClassName = 'Edit' then
    Result := TEdit
  else if ClassName = 'Label' then
    Result := TLabel
  else if ClassName = 'Panel' then
    Result := TPanel
  else if ClassName = 'CheckBox' then
    Result := TCheckBox
  else if ClassName = 'ComboBox' then
    Result := TComboBox
  else if ClassName = 'Memo' then
    Result := TMemo
  else if ClassName = 'ListBox' then
    Result := TListBox;
end;

function TConfigurableFrame.FindComponentByName(const Name: string): TComponent;  
var
  Index: Integer;
begin
  Index := FComponents.IndexOf(Name);
  if Index >= 0 then
    Result := TComponent(FComponents.Objects[Index])
  else
    Result := nil;
end;

procedure TConfigurableFrame.SaveToXML(const XMLFile: string);  
var
  Doc: TXMLDocument;
  Root, Node: TDOMNode;
  i: Integer;
  Control: TControl;
begin
  Doc := TXMLDocument.Create;
  try
    Root := Doc.CreateElement('Frame');
    Doc.AppendChild(Root);

    // Sauvegarder les propriétés du frame
    TDOMElement(Root).SetAttribute('Width', IntToStr(Width));
    TDOMElement(Root).SetAttribute('Height', IntToStr(Height));

    // Sauvegarder les composants
    for i := 0 to FComponents.Count - 1 do
    begin
      Control := TControl(FComponents.Objects[i]);
      Node := Doc.CreateElement(Control.ClassName);

      TDOMElement(Node).SetAttribute('Name', Control.Name);
      TDOMElement(Node).SetAttribute('Left', IntToStr(Control.Left));
      TDOMElement(Node).SetAttribute('Top', IntToStr(Control.Top));
      TDOMElement(Node).SetAttribute('Width', IntToStr(Control.Width));
      TDOMElement(Node).SetAttribute('Height', IntToStr(Control.Height));

      Root.AppendChild(Node);
    end;

    WriteXMLFile(Doc, XMLFile);
  finally
    Doc.Free;
  end;
end;
```

### Exemple de fichier de configuration XML

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Frame Width="400" Height="300">
  <Panel Name="HeaderPanel" Left="0" Top="0" Width="400" Height="50">
    <Label Name="TitleLabel" Left="10" Top="15" Width="100" Height="20"
           Caption="Formulaire dynamique"/>
  </Panel>

  <Label Name="NameLabel" Left="10" Top="70" Width="80" Height="20"
         Caption="Nom :"/>
  <Edit Name="NameEdit" Left="100" Top="70" Width="200" Height="25"/>

  <Label Name="EmailLabel" Left="10" Top="100" Width="80" Height="20"
         Caption="Email :"/>
  <Edit Name="EmailEdit" Left="100" Top="100" Width="200" Height="25"/>

  <CheckBox Name="NewsletterCheck" Left="100" Top="130" Width="200" Height="20"
            Caption="S'abonner à la newsletter"/>

  <Button Name="SubmitButton" Left="100" Top="160" Width="100" Height="30"
          Caption="Valider"/>
  <Button Name="CancelButton" Left="210" Top="160" Width="100" Height="30"
          Caption="Annuler"/>
</Frame>
```

## Frames et multi-threading

### Frame thread-safe

```pascal
unit ThreadSafeFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, SyncObjs;

type
  TDataUpdateEvent = procedure(const Data: string) of object;

  TWorkerThread = class(TThread)
  private
    FFrame: TThreadSafeFrame;
    FData: string;
    procedure UpdateUI;
  protected
    procedure Execute; override;
  public
    constructor Create(AFrame: TThreadSafeFrame);
  end;

  TThreadSafeFrame = class(TFrame)
    ProgressBar: TProgressBar;
    StatusLabel: TLabel;
    DataMemo: TMemo;
    StartButton: TButton;
    StopButton: TButton;

    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
  private
    FWorkerThread: TWorkerThread;
    FDataLock: TCriticalSection;
    FDataQueue: TStringList;
    FOnDataReceived: TDataUpdateEvent;
    FPendingProgress: Integer;
    FPendingStatus: string;

    procedure ProcessDataQueue;
    procedure DoUpdateProgress;
    procedure DoUpdateStatus;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddData(const Data: string);
    procedure UpdateProgress(Position: Integer);
    procedure UpdateStatus(const Status: string);

    property OnDataReceived: TDataUpdateEvent read FOnDataReceived write FOnDataReceived;
  end;

implementation

{ TWorkerThread }

constructor TWorkerThread.Create(AFrame: TThreadSafeFrame);  
begin
  inherited Create(True);
  FFrame := AFrame;
  FreeOnTerminate := False;
end;

procedure TWorkerThread.Execute;  
var
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    if Terminated then Break;

    // Simuler un travail
    Sleep(50);
    FData := Format('Donnée %d : %s', [i, DateTimeToStr(Now)]);

    // Mettre à jour l'interface de manière thread-safe
    Synchronize(@UpdateUI);
  end;
end;

procedure TWorkerThread.UpdateUI;  
begin
  FFrame.AddData(FData);
  // Pas de variable Position ici : utiliser le pourcentage calculé
  // La mise à jour du progrès se fait via AddData/ProcessDataQueue
end;

{ TThreadSafeFrame }

constructor TThreadSafeFrame.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  FDataLock := TCriticalSection.Create;
  FDataQueue := TStringList.Create;
  FWorkerThread := nil;
end;

destructor TThreadSafeFrame.Destroy;  
begin
  if Assigned(FWorkerThread) then
  begin
    FWorkerThread.Terminate;
    FWorkerThread.WaitFor;
    FWorkerThread.Free;
  end;

  FDataQueue.Free;
  FDataLock.Free;

  inherited;
end;

procedure TThreadSafeFrame.StartButtonClick(Sender: TObject);  
begin
  if Assigned(FWorkerThread) then
    Exit;

  DataMemo.Clear;
  ProgressBar.Position := 0;
  StatusLabel.Caption := 'Démarrage...';

  StartButton.Enabled := False;
  StopButton.Enabled := True;

  FWorkerThread := TWorkerThread.Create(Self);
  FWorkerThread.Start;
end;

procedure TThreadSafeFrame.StopButtonClick(Sender: TObject);  
begin
  if not Assigned(FWorkerThread) then
    Exit;

  FWorkerThread.Terminate;
  FWorkerThread.WaitFor;
  FreeAndNil(FWorkerThread);

  StartButton.Enabled := True;
  StopButton.Enabled := False;
  StatusLabel.Caption := 'Arrêté';
end;

procedure TThreadSafeFrame.AddData(const Data: string);  
begin
  FDataLock.Enter;
  try
    FDataQueue.Add(Data);
  finally
    FDataLock.Leave;
  end;

  // Traiter la file dans le thread principal
  if MainThreadID = GetCurrentThreadId then
    ProcessDataQueue
  else
    TThread.Synchronize(nil, @ProcessDataQueue);
end;

procedure TThreadSafeFrame.ProcessDataQueue;  
var
  Data: string;
begin
  FDataLock.Enter;
  try
    while FDataQueue.Count > 0 do
    begin
      Data := FDataQueue[0];
      FDataQueue.Delete(0);

      // Ajouter au memo
      DataMemo.Lines.Add(Data);

      // Déclencher l'événement
      if Assigned(FOnDataReceived) then
        FOnDataReceived(Data);
    end;
  finally
    FDataLock.Leave;
  end;
end;

procedure TThreadSafeFrame.DoUpdateProgress;  
begin
  ProgressBar.Position := FPendingProgress;
end;

procedure TThreadSafeFrame.DoUpdateStatus;  
begin
  StatusLabel.Caption := FPendingStatus;
end;

procedure TThreadSafeFrame.UpdateProgress(Position: Integer);  
begin
  if MainThreadID = GetCurrentThreadId then
    ProgressBar.Position := Position
  else
  begin
    FPendingProgress := Position;
    TThread.Queue(nil, @DoUpdateProgress);
  end;
end;

procedure TThreadSafeFrame.UpdateStatus(const Status: string);  
begin
  if MainThreadID = GetCurrentThreadId then
    StatusLabel.Caption := Status
  else
  begin
    FPendingStatus := Status;
    TThread.Queue(nil, @DoUpdateStatus);
  end;
end;
```

## Tests et validation des frames

### Framework de test pour frames

```pascal
unit FrameTestFramework;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, FPCUnit, TestRegistry;

type
  TFrameTestCase = class(TTestCase)
  private
    FTestFrame: TFrame;
    FTestForm: TForm;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    // Méthodes utilitaires pour les tests
    function CreateTestFrame(FrameClass: TFrameClass): TFrame;
    procedure SimulateClick(Control: TControl);
    procedure SimulateKeyPress(Control: TWinControl; Key: Char);
    procedure SimulateText(Edit: TEdit; const Text: string);
    function GetControlByName(const Name: string): TControl;
  public
    // Tests génériques pour tous les frames
    procedure TestFrameCreation;
    procedure TestFrameDestruction;
    procedure TestMemoryLeaks;
  end;

  TAddressFrameTest = class(TFrameTestCase)
  published
    procedure TestAddressValidation;
    procedure TestPostalCodeFormat;
    procedure TestRequiredFields;
    procedure TestClearFunction;
  end;

implementation

uses
  AddressFrame;

{ TFrameTestCase }

procedure TFrameTestCase.SetUp;  
begin
  // Créer un formulaire de test
  FTestForm := TForm.Create(nil);
  FTestForm.Width := 800;
  FTestForm.Height := 600;
  FTestFrame := nil;
end;

procedure TFrameTestCase.TearDown;  
begin
  FreeAndNil(FTestFrame);
  FreeAndNil(FTestForm);
end;

function TFrameTestCase.CreateTestFrame(FrameClass: TFrameClass): TFrame;  
begin
  Result := FrameClass.Create(FTestForm);
  Result.Parent := FTestForm;
  Result.Align := alClient;
  FTestFrame := Result;
end;

procedure TFrameTestCase.SimulateClick(Control: TControl);  
begin
  if Control is TButton then
    TButton(Control).Click
  else if Control is TCheckBox then
    TCheckBox(Control).Checked := not TCheckBox(Control).Checked;
end;

procedure TFrameTestCase.SimulateKeyPress(Control: TWinControl; Key: Char);  
var
  KeyEvent: Char;
begin
  KeyEvent := Key;
  if Control is TEdit then
    TEdit(Control).OnKeyPress(Control, KeyEvent);
end;

procedure TFrameTestCase.SimulateText(Edit: TEdit; const Text: string);  
begin
  Edit.Text := Text;
  if Assigned(Edit.OnChange) then
    Edit.OnChange(Edit);
end;

function TFrameTestCase.GetControlByName(const Name: string): TControl;  
begin
  Result := FTestFrame.FindComponent(Name) as TControl;
end;

procedure TFrameTestCase.TestFrameCreation;  
begin
  AssertNotNull('Frame doit être créé', FTestFrame);
  AssertTrue('Frame doit avoir un parent', FTestFrame.Parent = FTestForm);
end;

procedure TFrameTestCase.TestFrameDestruction;  
var
  Frame: TFrame;
begin
  Frame := TAddressFrame.Create(nil);
  try
    // Vérifier que la destruction ne lève pas d'exception
    Frame.Free;
    Frame := nil;
  except
    on E: Exception do
      Fail('Erreur lors de la destruction : ' + E.Message);
  end;

  AssertNull('Frame doit être nil après destruction', Frame);
end;

procedure TFrameTestCase.TestMemoryLeaks;  
var
  i: Integer;
  Frame: TFrame;
  InitialMemory, FinalMemory: PtrUInt;
begin
  InitialMemory := GetHeapStatus.TotalAllocated;

  for i := 1 to 100 do
  begin
    Frame := TAddressFrame.Create(nil);
    Frame.Free;
  end;

  FinalMemory := GetHeapStatus.TotalAllocated;

  // Tolérer une petite différence due aux allocations système
  AssertTrue('Pas de fuite mémoire significative',
    FinalMemory - InitialMemory < 10240);  // 10 KB de tolérance
end;

{ TAddressFrameTest }

procedure TAddressFrameTest.TestAddressValidation;  
var
  Frame: TAddressFrame;
begin
  Frame := CreateTestFrame(TAddressFrame) as TAddressFrame;

  // Test avec adresse vide
  AssertFalse('Adresse vide non requise doit être valide',
    Frame.Required and Frame.IsValid);

  // Remplir l'adresse
  SimulateText(Frame.edtStreet, '123 Rue de la Paix');
  SimulateText(Frame.edtCity, 'Paris');
  SimulateText(Frame.edtPostalCode, '75001');
  Frame.cboCountry.ItemIndex := 0;  // France

  Frame.Required := True;
  AssertTrue('Adresse complète doit être valide', Frame.IsValid);
end;

procedure TAddressFrameTest.TestPostalCodeFormat;  
var
  Frame: TAddressFrame;
  Key: Char;
begin
  Frame := CreateTestFrame(TAddressFrame) as TAddressFrame;

  Frame.cboCountry.ItemIndex := 0;  // France

  // Test avec des chiffres
  Key := '7';
  Frame.edtPostalCodeKeyPress(Frame.edtPostalCode, Key);
  AssertEquals('Les chiffres doivent être acceptés', '7', Key);

  // Test avec une lettre
  Key := 'A';
  Frame.edtPostalCodeKeyPress(Frame.edtPostalCode, Key);
  AssertEquals('Les lettres doivent être refusées', #0, Key);
end;

procedure TAddressFrameTest.TestRequiredFields;  
var
  Frame: TAddressFrame;
begin
  Frame := CreateTestFrame(TAddressFrame) as TAddressFrame;

  Frame.Required := True;

  // Vérifier les indicateurs visuels
  AssertTrue('Label rue doit avoir astérisque',
    Pos('*', Frame.lblStreet.Caption) > 0);
  AssertTrue('Label ville doit avoir astérisque',
    Pos('*', Frame.lblCity.Caption) > 0);

  Frame.Required := False;

  AssertFalse('Label rue ne doit pas avoir astérisque',
    Pos('*', Frame.lblStreet.Caption) > 0);
end;

procedure TAddressFrameTest.TestClearFunction;  
var
  Frame: TAddressFrame;
begin
  Frame := CreateTestFrame(TAddressFrame) as TAddressFrame;

  // Remplir les champs
  Frame.edtStreet.Text := 'Test';
  Frame.edtCity.Text := 'Test';
  Frame.edtPostalCode.Text := '12345';
  Frame.cboCountry.ItemIndex := 0;

  // Effacer
  Frame.Clear;

  // Vérifier que tout est vide
  AssertEquals('Rue doit être vide', '', Frame.edtStreet.Text);
  AssertEquals('Ville doit être vide', '', Frame.edtCity.Text);
  AssertEquals('Code postal doit être vide', '', Frame.edtPostalCode.Text);
  AssertEquals('Pays non sélectionné', -1, Frame.cboCountry.ItemIndex);

  AssertTrue('Frame doit être vide', Frame.IsEmpty);
end;

initialization
  RegisterTest(TAddressFrameTest);
end.
```

## Conseils et bonnes pratiques

### Organisation des frames dans un projet

```
MonProjet/
├── frames/
│   ├── base/
│   │   ├── BaseFrame.pas
│   │   └── BaseDataFrame.pas
│   ├── common/
│   │   ├── AddressFrame.pas
│   │   ├── LoginFrame.pas
│   │   └── SearchFrame.pas
│   └── business/
│       ├── CustomerFrame.pas
│       ├── ProductFrame.pas
│       └── OrderFrame.pas
├── forms/
│   └── MainForm.pas
└── tests/
    └── FrameTests.pas
```

### Checklist pour créer un bon frame

1. **Encapsulation** : Le frame doit être autonome et ne pas dépendre du formulaire parent
2. **Interface claire** : Propriétés et méthodes publiques bien définies
3. **Événements** : Exposer des événements pour la communication
4. **Validation** : Méthodes de validation intégrées
5. **Configuration** : Permettre la personnalisation via propriétés
6. **Performance** : Chargement différé des ressources lourdes
7. **Thread-safety** : Si utilisé avec des threads
8. **Tests** : Tests unitaires pour la logique métier
9. **Documentation** : Commentaires sur l'utilisation
10. **Réutilisabilité** : Éviter les dépendances spécifiques

## Conclusion

Les frames et composants composites sont des outils puissants pour :

- **Réutiliser** du code et des interfaces
- **Organiser** votre application en modules
- **Maintenir** plus facilement votre code
- **Tester** indépendamment chaque partie
- **Collaborer** en équipe (chacun sur ses frames)

Avec ces techniques, vous pouvez créer des applications modulaires, maintenables et professionnelles. Les frames sont la clé pour passer d'une application monolithique à une architecture composable et évolutive !

Points clés à retenir :

1. **Les frames sont des mini-formulaires réutilisables**
2. **Ils encapsulent interface ET logique**
3. **Utilisez l'héritage pour créer des hiérarchies**
4. **Les patterns (Factory, Mediator) améliorent l'architecture**
5. **Le thread-safety est crucial pour les applications modernes**
6. **Les tests garantissent la fiabilité**
7. **L'organisation en modules facilite la maintenance**

Maîtriser les frames, c'est maîtriser l'art de créer des applications modulaires et professionnelles avec Lazarus !

## Patterns avancés pour les frames

### Pattern MVP (Model-View-Presenter) avec frames

```pascal
unit MVPFramework;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type
  // Interfaces pour le pattern MVP
  IView = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure ShowData(const Data: string);
    procedure ShowError(const Error: string);
    procedure SetEnabled(Value: Boolean);
  end;

  IPresenter = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']
    procedure LoadData;
    procedure SaveData;
    procedure ValidateData;
  end;

  IModel = interface
    ['{C3D4E5F6-A7B8-9012-CDEF-123456789012}']
    function GetData: string;
    procedure SetData(const Value: string);
    function IsValid: Boolean;
  end;

  // Implémentation de base
  TBasePresenter = class(TInterfacedObject, IPresenter)
  protected
    FView: IView;
    FModel: IModel;
  public
    constructor Create(AView: IView; AModel: IModel);
    procedure LoadData; virtual;
    procedure SaveData; virtual;
    procedure ValidateData; virtual;
  end;

  // Frame de base implémentant IView
  TMVPFrame = class(TFrame, IView)
  private
    FPresenter: IPresenter;
  protected
    // IView implementation
    procedure ShowData(const Data: string); virtual; abstract;
    procedure ShowError(const Error: string); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
  public
    procedure SetPresenter(APresenter: IPresenter);
    property Presenter: IPresenter read FPresenter;
  end;

implementation

{ TBasePresenter }

constructor TBasePresenter.Create(AView: IView; AModel: IModel);  
begin
  inherited Create;
  FView := AView;
  FModel := AModel;
end;

procedure TBasePresenter.LoadData;  
begin
  try
    FView.SetEnabled(False);
    FView.ShowData(FModel.GetData);
    FView.SetEnabled(True);
  except
    on E: Exception do
      FView.ShowError(E.Message);
  end;
end;

procedure TBasePresenter.SaveData;  
begin
  if not FModel.IsValid then
  begin
    FView.ShowError('Données invalides');
    Exit;
  end;

  // Sauvegarder...
end;

procedure TBasePresenter.ValidateData;  
begin
  if FModel.IsValid then
    FView.ShowData('Validation réussie')
  else
    FView.ShowError('Validation échouée');
end;

{ TMVPFrame }

procedure TMVPFrame.SetPresenter(APresenter: IPresenter);  
begin
  FPresenter := APresenter;
end;

procedure TMVPFrame.ShowError(const Error: string);  
begin
  MessageDlg('Erreur', Error, mtError, [mbOK], 0);
end;

procedure TMVPFrame.SetEnabled(Value: Boolean);  
begin
  Enabled := Value;
end;
```

### Exemple concret avec MVP

```pascal
unit CustomerMVPFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, MVPFramework;

type
  // Model
  TCustomerModel = class(TInterfacedObject, IModel)
  private
    FName: string;
    FEmail: string;
    FPhone: string;
  public
    function GetData: string;
    procedure SetData(const Value: string);
    function IsValid: Boolean;

    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
    property Phone: string read FPhone write FPhone;
  end;

  // View (Frame)
  TCustomerFrame = class(TMVPFrame)
    edtName: TEdit;
    edtEmail: TEdit;
    edtPhone: TEdit;
    btnLoad: TButton;
    btnSave: TButton;
    btnValidate: TButton;

    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
  protected
    procedure ShowData(const Data: string); override;
  public
    procedure UpdateModel(Model: TCustomerModel);
  end;

  // Presenter
  TCustomerPresenter = class(TBasePresenter)
  private
    function GetCustomerModel: TCustomerModel;
  public
    procedure SaveData; override;
    property CustomerModel: TCustomerModel read GetCustomerModel;
  end;

implementation

uses
  StrUtils;

{ TCustomerModel }

function TCustomerModel.GetData: string;  
begin
  Result := Format('Nom: %s, Email: %s, Tél: %s', [FName, FEmail, FPhone]);
end;

procedure TCustomerModel.SetData(const Value: string);  
begin
  // Parser les données...
  FName := ExtractDelimited(1, Value, [',']);
  FEmail := ExtractDelimited(2, Value, [',']);
  FPhone := ExtractDelimited(3, Value, [',']);
end;

function TCustomerModel.IsValid: Boolean;  
begin
  Result := (FName <> '') and
            (Pos('@', FEmail) > 0) and
            (Length(FPhone) >= 10);
end;

{ TCustomerFrame }

procedure TCustomerFrame.ShowData(const Data: string);  
var
  Model: TCustomerModel;
begin
  // Afficher les données dans les champs
  if Presenter is TCustomerPresenter then
  begin
    Model := TCustomerPresenter(Presenter).CustomerModel;
    edtName.Text := Model.Name;
    edtEmail.Text := Model.Email;
    edtPhone.Text := Model.Phone;
  end;
end;

procedure TCustomerFrame.UpdateModel(Model: TCustomerModel);  
begin
  Model.Name := edtName.Text;
  Model.Email := edtEmail.Text;
  Model.Phone := edtPhone.Text;
end;

procedure TCustomerFrame.btnLoadClick(Sender: TObject);  
begin
  if Assigned(Presenter) then
    Presenter.LoadData;
end;

procedure TCustomerFrame.btnSaveClick(Sender: TObject);  
begin
  if Presenter is TCustomerPresenter then
  begin
    UpdateModel(TCustomerPresenter(Presenter).CustomerModel);
    Presenter.SaveData;
  end;
end;

procedure TCustomerFrame.btnValidateClick(Sender: TObject);  
begin
  if Presenter is TCustomerPresenter then
  begin
    UpdateModel(TCustomerPresenter(Presenter).CustomerModel);
    Presenter.ValidateData;
  end;
end;

{ TCustomerPresenter }

function TCustomerPresenter.GetCustomerModel: TCustomerModel;  
begin
  Result := FModel as TCustomerModel;
end;

procedure TCustomerPresenter.SaveData;  
begin
  // Logique métier spécifique
  if not CustomerModel.IsValid then
  begin
    FView.ShowError('Veuillez remplir tous les champs correctement');
    Exit;
  end;

  // Simuler la sauvegarde
  FView.ShowData('Client sauvegardé : ' + CustomerModel.GetData);
end;
```

## Gestion d'état dans les frames

### State Pattern pour les frames

```pascal
unit StateFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type
  TFrameState = class;

  // Contexte avec état
  TStatefulFrame = class(TFrame)
  private
    FState: TFrameState;
    FStateHistory: TList;

    procedure SetState(NewState: TFrameState);
  protected
    procedure SaveStateToHistory;
    procedure RestoreStateFromHistory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ChangeState(NewState: TFrameState);
    procedure HandleInput(const Input: string);
    procedure Undo;
    procedure Redo;

    property State: TFrameState read FState;
  end;

  // État abstrait
  TFrameState = class
  protected
    FContext: TStatefulFrame;
  public
    constructor Create(AContext: TStatefulFrame);

    procedure Enter; virtual;
    procedure Exit; virtual;
    procedure HandleInput(const Input: string); virtual; abstract;
    procedure Update; virtual;
  end;

  // États concrets
  TIdleState = class(TFrameState)
  public
    procedure HandleInput(const Input: string); override;
    procedure Enter; override;
  end;

  TLoadingState = class(TFrameState)
  private
    FLoadingTimer: TTimer;
    procedure OnLoadingComplete(Sender: TObject);
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleInput(const Input: string); override;
  end;

  TEditingState = class(TFrameState)
  public
    procedure HandleInput(const Input: string); override;
    procedure Enter; override;
  end;

  TErrorState = class(TFrameState)
  private
    FErrorMessage: string;
  public
    constructor Create(AContext: TStatefulFrame; const ErrorMsg: string);
    procedure Enter; override;
    procedure HandleInput(const Input: string); override;
  end;

implementation

{ TStatefulFrame }

constructor TStatefulFrame.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FStateHistory := TList.Create;

  // État initial
  FState := TIdleState.Create(Self);
  FState.Enter;
end;

destructor TStatefulFrame.Destroy;  
var
  i: Integer;
begin
  if Assigned(FState) then
    FState.Exit;

  FState.Free;

  for i := 0 to FStateHistory.Count - 1 do
    TFrameState(FStateHistory[i]).Free;

  FStateHistory.Free;

  inherited Destroy;
end;

procedure TStatefulFrame.SetState(NewState: TFrameState);  
begin
  if Assigned(FState) then
    FState.Exit;

  FState := NewState;

  if Assigned(FState) then
    FState.Enter;
end;

procedure TStatefulFrame.ChangeState(NewState: TFrameState);  
begin
  SaveStateToHistory;
  SetState(NewState);
end;

procedure TStatefulFrame.SaveStateToHistory;  
begin
  if Assigned(FState) then
    FStateHistory.Add(FState);

  // Limiter l'historique
  while FStateHistory.Count > 50 do
  begin
    TFrameState(FStateHistory[0]).Free;
    FStateHistory.Delete(0);
  end;
end;

procedure TStatefulFrame.RestoreStateFromHistory;  
begin
  if FStateHistory.Count > 0 then
  begin
    if Assigned(FState) then
      FState.Free;

    FState := TFrameState(FStateHistory[FStateHistory.Count - 1]);
    FStateHistory.Delete(FStateHistory.Count - 1);

    if Assigned(FState) then
      FState.Enter;
  end;
end;

procedure TStatefulFrame.HandleInput(const Input: string);  
begin
  if Assigned(FState) then
    FState.HandleInput(Input);
end;

procedure TStatefulFrame.Undo;  
begin
  RestoreStateFromHistory;
end;

procedure TStatefulFrame.Redo;  
begin
  // Implémenter la logique de redo
end;

{ TFrameState }

constructor TFrameState.Create(AContext: TStatefulFrame);  
begin
  inherited Create;
  FContext := AContext;
end;

procedure TFrameState.Enter;  
begin
  // Override dans les classes dérivées
end;

procedure TFrameState.Exit;  
begin
  // Override dans les classes dérivées
end;

procedure TFrameState.Update;  
begin
  // Override si nécessaire
end;

{ TIdleState }

procedure TIdleState.Enter;  
begin
  // Configuration pour l'état idle
  if Assigned(FContext) then
  begin
    FContext.Cursor := crDefault;
    FContext.Enabled := True;
  end;
end;

procedure TIdleState.HandleInput(const Input: string);  
begin
  if Input = 'LOAD' then
    FContext.ChangeState(TLoadingState.Create(FContext))
  else if Input = 'EDIT' then
    FContext.ChangeState(TEditingState.Create(FContext));
end;

{ TLoadingState }

procedure TLoadingState.Enter;  
begin
  FContext.Cursor := crHourGlass;
  FContext.Enabled := False;

  // Simuler le chargement
  FLoadingTimer := TTimer.Create(nil);
  FLoadingTimer.Interval := 2000;
  FLoadingTimer.OnTimer := @OnLoadingComplete;
  FLoadingTimer.Enabled := True;
end;

procedure TLoadingState.Exit;  
begin
  FLoadingTimer.Free;
  FContext.Cursor := crDefault;
  FContext.Enabled := True;
end;

procedure TLoadingState.HandleInput(const Input: string);  
begin
  if Input = 'CANCEL' then
  begin
    FLoadingTimer.Enabled := False;
    FContext.ChangeState(TIdleState.Create(FContext));
  end;
end;

procedure TLoadingState.OnLoadingComplete(Sender: TObject);  
begin
  FLoadingTimer.Enabled := False;
  FContext.ChangeState(TIdleState.Create(FContext));
end;

{ TEditingState }

procedure TEditingState.Enter;  
begin
  // Activer le mode édition
  FContext.Color := clInfoBk;
end;

procedure TEditingState.HandleInput(const Input: string);  
begin
  if Input = 'SAVE' then
    FContext.ChangeState(TIdleState.Create(FContext))
  else if Input = 'CANCEL' then
    FContext.ChangeState(TIdleState.Create(FContext));
end;

{ TErrorState }

constructor TErrorState.Create(AContext: TStatefulFrame; const ErrorMsg: string);  
begin
  inherited Create(AContext);
  FErrorMessage := ErrorMsg;
end;

procedure TErrorState.Enter;  
begin
  FContext.Color := $FFE0E0;  // Rouge clair
  ShowMessage('Erreur : ' + FErrorMessage);
end;

procedure TErrorState.HandleInput(const Input: string);  
begin
  if Input = 'RETRY' then
    FContext.ChangeState(TLoadingState.Create(FContext))
  else if Input = 'CANCEL' then
    FContext.ChangeState(TIdleState.Create(FContext));
end;
```

## Conclusion finale

Les frames et composants composites représentent l'évolution naturelle du développement d'interfaces avec Lazarus. Ils permettent de :

1. **Structurer** votre code de manière modulaire et réutilisable
2. **Encapsuler** la logique métier avec l'interface
3. **Faciliter** la maintenance et les tests
4. **Améliorer** la collaboration en équipe
5. **Optimiser** les performances avec le chargement différé
6. **Gérer** la complexité avec des patterns éprouvés

Avec ces outils et techniques, vous êtes maintenant capable de créer des applications professionnelles, modulaires et maintenables. Les frames ne sont pas juste des conteneurs visuels, mais de véritables composants architecturaux qui structurent votre application.

La maîtrise des frames est un passage obligé pour tout développeur Lazarus sérieux qui souhaite créer des applications évolutives et professionnelles !

⏭️ [Gestion avancée des événements](/04-framework-lcl/08-gestion-avancee-evenements.md)
