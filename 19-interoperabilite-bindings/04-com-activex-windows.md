🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.4 COM/ActiveX sous Windows

## Introduction

**COM (Component Object Model)** et **ActiveX** sont des technologies Microsoft qui permettent aux applications de communiquer entre elles et de partager des fonctionnalités, quelle que soit leur langage de programmation. Ces technologies sont au cœur de nombreuses applications Windows depuis les années 1990.

### Qu'est-ce que COM ?

COM est un **standard binaire** qui définit comment les composants logiciels interagissent. Imaginez COM comme un langage universel que tous les programmes Windows peuvent parler, qu'ils soient écrits en Pascal, C++, C#, VB, Python ou autre.

**Analogie** : Pensez à COM comme aux prises électriques standard. Peu importe le fabricant de votre appareil (lampe, ordinateur, téléviseur), tant qu'il a la bonne prise, il fonctionnera avec n'importe quelle prise murale standard.

### Qu'est-ce qu'ActiveX ?

**ActiveX** est une extension de COM spécialement conçue pour les contrôles visuels et les composants qui peuvent être intégrés dans des applications ou des pages web. Un contrôle ActiveX est essentiellement un composant COM avec une interface utilisateur.

**Exemples courants** :
- Lecteur Windows Media Player (intégré dans des applications)
- Contrôles de calendrier et date picker
- Composants de graphiques
- Adobe PDF Reader (contrôle ActiveX)
- Contrôles de grille de données avancés

### Pourquoi utiliser COM/ActiveX en FreePascal ?

#### 1. Automatisation d'applications Office

```pascal
// Automatiser Microsoft Excel
ExcelApp := CreateOleObject('Excel.Application');  
ExcelApp.Visible := True;  
ExcelApp.Workbooks.Add;  
ExcelApp.Cells[1, 1].Value := 'Hello from FreePascal!';
```

#### 2. Utilisation de composants Windows

- Internet Explorer (contrôle web browser)
- Windows Media Player
- Shell Windows (explorateur de fichiers)
- Contrôles système avancés

#### 3. Interopérabilité avec .NET

COM est le pont entre les applications natives et .NET, permettant d'utiliser des assemblies .NET depuis FreePascal.

#### 4. Accès aux services système

- WMI (Windows Management Instrumentation)
- ADSI (Active Directory Service Interfaces)
- Windows Script Host
- Services Windows

### Architecture COM

```
┌─────────────────────────────────────────────────────┐
│                  Application Cliente                │
│              (FreePascal, C++, C#, etc.)            │
└────────────────────┬────────────────────────────────┘
                     │
                     │ Appels via interface COM
                     │
         ┌───────────▼──────────────┐
         │     Couche COM/OLE       │
         │  (système d'exploitation)│
         └───────────┬──────────────┘
                     │
        ┌────────────┼────────────┐
        │            │            │
   ┌────▼───┐  ┌────▼───┐  ┌────▼───┐
   │ Serveur│  │ Serveur│  │ Serveur│
   │  COM 1 │  │  COM 2 │  │  COM 3 │
   │ (Excel)│  │  (WMP) │  │  (IE)  │
   └────────┘  └────────┘  └────────┘
```

### Concepts clés

#### GUID/UUID (Globally Unique Identifier)

Chaque composant COM est identifié par un **GUID** unique de 128 bits.

**Format** : `{00000000-0000-0000-0000-000000000000}`

**Exemple pour Excel** :
```pascal
const
  CLASS_Excel_Application = '{00024500-0000-0000-C000-000000000046}';
```

Les GUIDs garantissent qu'aucune collision ne peut se produire entre différents composants.

#### Interfaces

Une **interface** définit un contrat - un ensemble de méthodes que l'objet COM doit implémenter. Les interfaces COM dérivent toutes de `IUnknown`.

```pascal
type
  ICalculator = interface(IUnknown)
    ['{12345678-1234-1234-1234-123456789ABC}']
    function Add(a, b: Integer): Integer; stdcall;
    function Subtract(a, b: Integer): Integer; stdcall;
  end;
```

#### IUnknown : La mère de toutes les interfaces

Toutes les interfaces COM héritent de `IUnknown` qui fournit trois méthodes essentielles :

```pascal
IUnknown = interface
  function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  function _AddRef: Integer; stdcall;
  function _Release: Integer; stdcall;
end;
```

- **QueryInterface** : Demander une interface spécifique
- **AddRef** : Incrémenter le compteur de références
- **Release** : Décrémenter le compteur et détruire si zéro

#### IDispatch : Liaison tardive (Late Binding)

`IDispatch` permet l'**automation** - appeler des méthodes par leur nom (chaîne) plutôt que par des pointeurs de fonctions.

```pascal
// Early binding (compilation)
ExcelApp.Cells[1, 1].Value := 'Test';

// Late binding (runtime)
ExcelApp.Invoke('Cells', [1, 1]).Set('Value', 'Test');
```

### Types de serveurs COM

#### In-Process (DLL)

Le serveur COM est une DLL chargée dans l'espace mémoire du client.

**Avantages** :
- Très rapide (pas de marshalling)
- Accès direct à la mémoire partagée

**Inconvénients** :
- Crash du serveur = crash du client
- Même processus, même thread

#### Out-of-Process (EXE)

Le serveur COM est un processus séparé.

**Avantages** :
- Isolation (crash du serveur n'affecte pas le client)
- Peut être sur une machine distante (DCOM)

**Inconvénients** :
- Plus lent (marshalling des appels)
- Consommation mémoire plus élevée

## Utiliser des objets COM existants

### OLE Automation avec Excel

L'exemple classique : automatiser Microsoft Excel depuis FreePascal.

#### Exemple basique

```pascal
program ExcelBasic;

{$mode objfpc}{$H+}
{$APPTYPE GUI}

uses
  SysUtils, ComObj, Variants;

var
  ExcelApp: Variant;
  Workbook: Variant;
  Worksheet: Variant;
begin
  try
    // Créer une instance d'Excel
    ExcelApp := CreateOleObject('Excel.Application');

    // Rendre Excel visible
    ExcelApp.Visible := True;

    // Créer un nouveau classeur
    Workbook := ExcelApp.Workbooks.Add;

    // Obtenir la première feuille
    Worksheet := Workbook.Worksheets[1];

    // Écrire des données
    Worksheet.Cells[1, 1].Value := 'Nom';
    Worksheet.Cells[1, 2].Value := 'Âge';
    Worksheet.Cells[1, 3].Value := 'Ville';

    Worksheet.Cells[2, 1].Value := 'Alice';
    Worksheet.Cells[2, 2].Value := 25;
    Worksheet.Cells[2, 3].Value := 'Paris';

    Worksheet.Cells[3, 1].Value := 'Bob';
    Worksheet.Cells[3, 2].Value := 30;
    Worksheet.Cells[3, 3].Value := 'Lyon';

    // Formater le titre
    Worksheet.Range['A1:C1'].Font.Bold := True;
    Worksheet.Range['A1:C1'].Interior.Color := $00C0C0C0; // Gris clair

    // Ajuster la largeur des colonnes
    Worksheet.Columns['A:C'].AutoFit;

    WriteLn('Excel créé avec succès !');
    WriteLn('Fermez Excel pour continuer...');

  except
    on E: Exception do
    begin
      WriteLn('Erreur : ', E.Message);
      WriteLn('Assurez-vous que Microsoft Excel est installé.');
    end;
  end;

  ReadLn;
end.
```

#### Exemple avancé : Rapport avec graphique

```pascal
program ExcelRapport;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants;

procedure CreerRapportVentes;  
var
  Excel, Workbook, Sheet, Chart: Variant;
  i: Integer;
const
  Mois: array[1..12] of string = (
    'Janvier', 'Février', 'Mars', 'Avril', 'Mai', 'Juin',
    'Juillet', 'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre'
  );
  Ventes: array[1..12] of Integer = (
    15000, 18000, 22000, 19000, 25000, 28000,
    26000, 24000, 27000, 30000, 32000, 35000
  );
begin
  Excel := CreateOleObject('Excel.Application');
  Excel.Visible := True;

  Workbook := Excel.Workbooks.Add;
  Sheet := Workbook.Worksheets[1];
  Sheet.Name := 'Rapport Ventes';

  // En-têtes
  Sheet.Cells[1, 1].Value := 'RAPPORT DE VENTES 2024';
  Sheet.Range['A1:B1'].Merge;
  Sheet.Range['A1'].Font.Size := 16;
  Sheet.Range['A1'].Font.Bold := True;

  Sheet.Cells[3, 1].Value := 'Mois';
  Sheet.Cells[3, 2].Value := 'Ventes (€)';
  Sheet.Range['A3:B3'].Font.Bold := True;
  Sheet.Range['A3:B3'].Interior.Color := $00FFCC99; // Orange clair

  // Données
  for i := 1 to 12 do
  begin
    Sheet.Cells[3 + i, 1].Value := Mois[i];
    Sheet.Cells[3 + i, 2].Value := Ventes[i];
  end;

  // Formule total
  Sheet.Cells[16, 1].Value := 'TOTAL';
  Sheet.Cells[16, 1].Font.Bold := True;
  Sheet.Cells[16, 2].Formula := '=SUM(B4:B15)';
  Sheet.Cells[16, 2].Font.Bold := True;

  // Mise en forme des nombres
  Sheet.Range['B4:B16'].NumberFormat := '#,##0 €';

  // Bordures
  Sheet.Range['A3:B16'].Borders.LineStyle := 1;

  // Créer un graphique
  Chart := Sheet.ChartObjects.Add(250, 50, 400, 300).Chart;
  Chart.ChartType := 51; // xlColumnClustered
  Chart.SetSourceData(Sheet.Range['A3:B15']);
  Chart.HasTitle := True;
  Chart.ChartTitle.Text := 'Évolution des ventes 2024';

  // Ajuster les colonnes
  Sheet.Columns['A:B'].AutoFit;

  WriteLn('Rapport créé avec succès !');
end;

begin
  try
    CreerRapportVentes;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Automatisation de Word

```pascal
program WordBasic;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants;

procedure CreerDocument;  
var
  WordApp, Doc, Paragraph, Table: Variant;
begin
  // Créer une instance de Word
  WordApp := CreateOleObject('Word.Application');
  WordApp.Visible := True;

  // Créer un nouveau document
  Doc := WordApp.Documents.Add;

  // Ajouter du texte
  Paragraph := Doc.Paragraphs.Add;
  Paragraph.Range.Text := 'Rapport généré par FreePascal';
  Paragraph.Range.Font.Size := 18;
  Paragraph.Range.Font.Bold := True;
  Paragraph.Range.ParagraphFormat.Alignment := 1; // Centre

  // Ajouter un paragraphe
  Doc.Content.InsertParagraphAfter;
  Paragraph := Doc.Paragraphs.Add;
  Paragraph.Range.Text := 'Ceci est un exemple d''automatisation Word depuis FreePascal. ';
  Paragraph.Range.Text := Paragraph.Range.Text +
    'Vous pouvez créer des documents complexes, insérer des tableaux, ' +
    'des images et bien plus encore.';

  // Ajouter un tableau
  Doc.Content.InsertParagraphAfter;
  Table := Doc.Tables.Add(Doc.Paragraphs.Last.Range, 3, 2);
  Table.Borders.Enable := True;

  Table.Cell(1, 1).Range.Text := 'Élément';
  Table.Cell(1, 2).Range.Text := 'Valeur';
  Table.Cell(2, 1).Range.Text := 'CPU';
  Table.Cell(2, 2).Range.Text := '85%';
  Table.Cell(3, 1).Range.Text := 'RAM';
  Table.Cell(3, 2).Range.Text := '4.2 GB';

  // Formater le tableau
  Table.Rows[1].Range.Font.Bold := True;
  Table.Rows[1].Shading.BackgroundPatternColor := $00C0C0C0;

  WriteLn('Document Word créé avec succès !');
end;

begin
  try
    CreerDocument;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Automatisation d'Outlook

```pascal
program OutlookEmail;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants;

procedure EnvoyerEmail;  
var
  Outlook, MailItem: Variant;
begin
  // Créer une instance d'Outlook
  Outlook := CreateOleObject('Outlook.Application');

  // Créer un nouvel email
  MailItem := Outlook.CreateItem(0); // olMailItem = 0

  // Configurer l'email
  MailItem.Subject := 'Email envoyé depuis FreePascal';
  MailItem.To := 'destinataire@example.com';
  MailItem.CC := 'copie@example.com';
  MailItem.Body := 'Bonjour,' + #13#10 + #13#10 +
    'Ceci est un email envoyé automatiquement depuis une application FreePascal.' + #13#10 +
    'Cordialement,' + #13#10 +
    'Votre application';

  // Ajouter une pièce jointe (optionnel)
  // MailItem.Attachments.Add('C:\chemin\vers\fichier.pdf');

  // Afficher l'email (l'utilisateur peut modifier avant d'envoyer)
  MailItem.Display;

  // Ou envoyer directement (décommenter si nécessaire)
  // MailItem.Send;

  WriteLn('Email créé avec succès !');
end;

begin
  try
    EnvoyerEmail;
  except
    on E: Exception do
    begin
      WriteLn('Erreur : ', E.Message);
      WriteLn('Assurez-vous que Microsoft Outlook est installé et configuré.');
    end;
  end;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Internet Explorer / Edge

```pascal
program NavigateurCOM;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants, Windows;

procedure OuvrirPageWeb;  
var
  IE, Document, Links: Variant;
  Msg: TMsg;
begin
  // Créer une instance d'Internet Explorer
  IE := CreateOleObject('InternetExplorer.Application');

  // Configurer
  IE.Visible := True;
  IE.Navigate('https://www.freepascal.org');

  // Attendre que la page soit chargée
  while IE.Busy do
  begin
    Sleep(100);
    // Traiter les messages Windows pour éviter le blocage
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;

  WriteLn('Page chargée !');

  // Accéder au contenu HTML
  Document := IE.Document;
  WriteLn('Titre de la page : ', Document.Title);

  // Exemple : Récupérer tous les liens
  Links := Document.getElementsByTagName('a');
  WriteLn('Nombre de liens : ', Links.length);

  WriteLn('Appuyez sur Entrée pour fermer le navigateur...');
  ReadLn;

  IE.Quit;
end;

begin
  try
    OuvrirPageWeb;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end.
```

## Windows Management Instrumentation (WMI)

WMI permet d'obtenir des informations système et de gérer Windows via COM.

### Informations système

```pascal
program InfoSystemeWMI;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants, ActiveX;

procedure AfficherInfosSysteme;  
var
  Locator, Service, ObjectSet, Obj: Variant;
  oEnum: IEnumVariant;
  Value: OleVariant;
  Fetched: Cardinal;
begin
  CoInitialize(nil);
  try
    // Connexion à WMI
    Locator := CreateOleObject('WbemScripting.SWbemLocator');
    Service := Locator.ConnectServer('.', 'root\CIMV2');

    WriteLn('=== Informations Système ===');
    WriteLn;

    // Informations sur le système d'exploitation
    ObjectSet := Service.ExecQuery('SELECT * FROM Win32_OperatingSystem');
    oEnum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;

    while oEnum.Next(1, Value, Fetched) = S_OK do
    begin
      Obj := Value;
      WriteLn('Système : ', VarToStr(Obj.Caption));
      WriteLn('Version : ', VarToStr(Obj.Version));
      WriteLn('Architecture : ', VarToStr(Obj.OSArchitecture));
      WriteLn('Fabricant : ', VarToStr(Obj.Manufacturer));

      var TotalMem: Int64 := Obj.TotalVisibleMemorySize;
      var FreeMem: Int64 := Obj.FreePhysicalMemory;
      WriteLn('Mémoire totale : ', TotalMem div 1024, ' MB');
      WriteLn('Mémoire libre : ', FreeMem div 1024, ' MB');
      WriteLn('Mémoire utilisée : ', (TotalMem - FreeMem) div 1024, ' MB');
    end;

    WriteLn;
    WriteLn('=== Processeur ===');
    WriteLn;

    // Informations sur le processeur
    ObjectSet := Service.ExecQuery('SELECT * FROM Win32_Processor');
    oEnum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;

    while oEnum.Next(1, Value, Fetched) = S_OK do
    begin
      Obj := Value;
      WriteLn('Nom : ', VarToStr(Obj.Name));
      WriteLn('Fabricant : ', VarToStr(Obj.Manufacturer));
      WriteLn('Cœurs : ', VarToStr(Obj.NumberOfCores));
      WriteLn('Threads : ', VarToStr(Obj.NumberOfLogicalProcessors));
      WriteLn('Fréquence : ', VarToStr(Obj.MaxClockSpeed), ' MHz');
    end;

    WriteLn;
    WriteLn('=== Disques ===');
    WriteLn;

    // Informations sur les disques
    ObjectSet := Service.ExecQuery('SELECT * FROM Win32_LogicalDisk WHERE DriveType=3');
    oEnum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;

    while oEnum.Next(1, Value, Fetched) = S_OK do
    begin
      Obj := Value;
      WriteLn('Lecteur : ', VarToStr(Obj.DeviceID));
      WriteLn('  Type : ', VarToStr(Obj.FileSystem));

      var TotalSize: Int64 := Obj.Size div (1024 * 1024 * 1024);
      var FreeSize: Int64 := Obj.FreeSpace div (1024 * 1024 * 1024);
      WriteLn('  Taille : ', TotalSize, ' GB');
      WriteLn('  Libre : ', FreeSize, ' GB');
      WriteLn('  Utilisé : ', TotalSize - FreeSize, ' GB');
      WriteLn;
    end;

  finally
    CoUninitialize;
  end;
end;

begin
  try
    AfficherInfosSysteme;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Surveillance des processus

```pascal
program SurveillanceProcessus;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants, ActiveX;

procedure ListerProcessus;  
var
  Locator, Service, ObjectSet, Obj: Variant;
  oEnum: IEnumVariant;
  Value: OleVariant;
  Fetched: Cardinal;
  PID, Name: string;
  Memory: Int64;
begin
  CoInitialize(nil);
  try
    Locator := CreateOleObject('WbemScripting.SWbemLocator');
    Service := Locator.ConnectServer('.', 'root\CIMV2');

    ObjectSet := Service.ExecQuery('SELECT * FROM Win32_Process');
    oEnum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;

    WriteLn('=== Processus en cours ===');
    WriteLn;
    WriteLn(Format('%-8s %-40s %12s', ['PID', 'Nom', 'Mémoire']));
    WriteLn(StringOfChar('-', 65));

    while oEnum.Next(1, Value, Fetched) = S_OK do
    begin
      Obj := Value;
      PID := VarToStr(Obj.ProcessId);
      Name := VarToStr(Obj.Name);
      Memory := 0;

      try
        Memory := Obj.WorkingSetSize div 1024; // En Ko
      except
        // Certains processus peuvent ne pas retourner cette info
      end;

      WriteLn(Format('%-8s %-40s %9d KB', [PID, Name, Memory]));
    end;

  finally
    CoUninitialize;
  end;
end;

begin
  try
    ListerProcessus;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Contrôles ActiveX

### Intégrer un contrôle ActiveX dans Lazarus

Lazarus peut héberger des contrôles ActiveX directement dans vos formulaires.

#### Installation du support ActiveX

1. **Dans Lazarus** : Package → Open Package File
2. **Naviguer vers** : `lazarus/components/activex/activex.lpk`
3. **Compiler et installer** le package

#### Exemple : Lecteur Windows Media Player

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  OleControl, ComObj;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FMediaPlayer: Variant;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin
  try
    // Créer le contrôle Windows Media Player
    FMediaPlayer := CreateOleObject('WMPlayer.OCX');
    WriteLn('Windows Media Player chargé');
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);  
var
  OpenDialog: TOpenDialog;
begin
  // Ouvrir un fichier
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Fichiers média|*.mp3;*.mp4;*.avi;*.wmv|Tous les fichiers|*.*';
    if OpenDialog.Execute then
    begin
      FMediaPlayer.URL := OpenDialog.FileName;
      FMediaPlayer.controls.play;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);  
begin
  // Pause
  FMediaPlayer.controls.pause;
end;

procedure TForm1.Button3Click(Sender: TObject);  
begin
  // Stop
  FMediaPlayer.controls.stop;
end;

end.
```

### Navigateur web intégré

```pascal
unit WebBrowserForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComObj, Variants;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    EditURL: TEdit;
    ButtonGo: TButton;
    ButtonBack: TButton;
    ButtonForward: TButton;
    ButtonRefresh: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonGoClick(Sender: TObject);
    procedure ButtonBackClick(Sender: TObject);
    procedure ButtonForwardClick(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
  private
    FBrowser: Variant;
    FBrowserHandle: HWND;
    procedure CreateBrowser;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  Windows;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin
  CreateBrowser;
end;

procedure TForm1.CreateBrowser;  
begin
  try
    // Créer le contrôle Internet Explorer
    FBrowser := CreateOleObject('Shell.Explorer.2');

    // Initialiser avec une page
    FBrowser.Navigate('about:blank');

    WriteLn('Navigateur web créé');
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;

procedure TForm1.ButtonGoClick(Sender: TObject);  
begin
  if not VarIsEmpty(FBrowser) then
    FBrowser.Navigate(EditURL.Text);
end;

procedure TForm1.ButtonBackClick(Sender: TObject);  
begin
  if not VarIsEmpty(FBrowser) then
    FBrowser.GoBack;
end;

procedure TForm1.ButtonForwardClick(Sender: TObject);  
begin
  if not VarIsEmpty(FBrowser) then
    FBrowser.GoForward;
end;

procedure TForm1.ButtonRefreshClick(Sender: TObject);  
begin
  if not VarIsEmpty(FBrowser) then
    FBrowser.Refresh;
end;

procedure TForm1.FormDestroy(Sender: TObject);  
begin
  FBrowser := Unassigned;
end;

end.
```

## Créer un serveur COM en FreePascal

### Serveur COM simple (DLL)

```pascal
library SimpleComServer;

{$mode objfpc}{$H+}

uses
  ComObj, ComServ, SysUtils, Windows, ActiveX;

type
  // Interface ICalculator
  ICalculator = interface(IDispatch)
    ['{A1234567-1234-1234-1234-123456789ABC}']
    function Add(a, b: Integer): Integer; safecall;
    function Subtract(a, b: Integer): Integer; safecall;
    function Multiply(a, b: Integer): Integer; safecall;
    function Divide(a, b: Double): Double; safecall;
  end;

  // Implémentation
  TCalculator = class(TAutoObject, ICalculator)
  public
    function Add(a, b: Integer): Integer; safecall;
    function Subtract(a, b: Integer): Integer; safecall;
    function Multiply(a, b: Integer): Integer; safecall;
    function Divide(a, b: Double): Double; safecall;
  end;

  // Factory pour créer des instances
  TCalculatorFactory = class(TAutoObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

const
  // CLSID de notre objet COM
  CLASS_Calculator: TGUID = '{B2345678-2345-2345-2345-23456789ABCD}';

{ TCalculator }

function TCalculator.Add(a, b: Integer): Integer;  
begin
  Result := a + b;
end;

function TCalculator.Subtract(a, b: Integer): Integer;  
begin
  Result := a - b;
end;

function TCalculator.Multiply(a, b: Integer): Integer;  
begin
  Result := a * b;
end;

function TCalculator.Divide(a, b: Double): Double;  
begin
  if b = 0 then
    raise Exception.Create('Division par zéro');
  Result := a / b;
end;

{ TCalculatorFactory }

procedure TCalculatorFactory.UpdateRegistry(Register: Boolean);  
begin
  if Register then
  begin
    inherited UpdateRegistry(Register);
    // Enregistrer des informations supplémentaires si nécessaire
    CreateRegKey('SOFTWARE\MyCompany\Calculator', '', 'Calculator COM Server');
  end
  else
    inherited UpdateRegistry(Register);
end;

initialization
  // Enregistrer la factory
  TCalculatorFactory.Create(
    ComServer,
    TCalculator,
    CLASS_Calculator,
    'Calculator',
    'Simple Calculator COM Object',
    ciMultiInstance,
    tmApartment
  );

end.
```

### Enregistrement du serveur COM

Pour enregistrer le serveur COM :

```batch
REM Enregistrer  
regsvr32 SimpleComServer.dll

REM Désenregistrer  
regsvr32 /u SimpleComServer.dll
```

### Client pour tester le serveur COM

```pascal
program TestCalculator;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants;

var
  Calculator: Variant;
  Result: Variant;
begin
  try
    // Créer une instance de notre calculatrice COM
    Calculator := CreateOleObject('Calculator');

    WriteLn('=== Test Calculatrice COM ===');
    WriteLn;

    // Addition
    Result := Calculator.Add(10, 5);
    WriteLn('10 + 5 = ', Integer(Result));

    // Soustraction
    Result := Calculator.Subtract(10, 5);
    WriteLn('10 - 5 = ', Integer(Result));

    // Multiplication
    Result := Calculator.Multiply(10, 5);
    WriteLn('10 * 5 = ', Integer(Result));

    // Division
    Result := Calculator.Divide(10.0, 5.0);
    WriteLn('10 / 5 = ', Double(Result):0:2);

    // Test erreur
    WriteLn;
    WriteLn('Test division par zéro :');
    try
      Result := Calculator.Divide(10.0, 0.0);
    except
      on E: Exception do
        WriteLn('Erreur capturée : ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Serveur COM avancé avec gestion d'état

### Serveur de données avec stockage

```pascal
library DataComServer;

{$mode delphi}{$H+}

uses
  ComObj, ComServ, SysUtils, Windows, ActiveX, Classes, Generics.Collections;

type
  // Interface IDataStore
  IDataStore = interface(IDispatch)
    ['{C3456789-3456-3456-3456-3456789ABCDE}']
    procedure SetValue(const Key: WideString; const Value: OleVariant); safecall;
    function GetValue(const Key: WideString): OleVariant; safecall;
    procedure DeleteKey(const Key: WideString); safecall;
    function HasKey(const Key: WideString): WordBool; safecall;
    function GetKeys: OleVariant; safecall;
    procedure Clear; safecall;
    function GetCount: Integer; safecall;
  end;

  // Implémentation
  TDataStore = class(TAutoObject, IDataStore)
  private
    FData: TDictionary<string, Variant>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetValue(const Key: WideString; const Value: OleVariant); safecall;
    function GetValue(const Key: WideString): OleVariant; safecall;
    procedure DeleteKey(const Key: WideString); safecall;
    function HasKey(const Key: WideString): WordBool; safecall;
    function GetKeys: OleVariant; safecall;
    procedure Clear; safecall;
    function GetCount: Integer; safecall;
  end;

const
  CLASS_DataStore: TGUID = '{D4567890-4567-4567-4567-456789ABCDEF}';

{ TDataStore }

constructor TDataStore.Create;  
begin
  inherited Create;
  FData := TDictionary<string, Variant>.Create;
end;

destructor TDataStore.Destroy;  
begin
  FData.Free;
  inherited Destroy;
end;

procedure TDataStore.SetValue(const Key: WideString; const Value: OleVariant);  
begin
  FData.AddOrSetValue(Key, Value);
end;

function TDataStore.GetValue(const Key: WideString): OleVariant;  
begin
  if FData.ContainsKey(Key) then
    Result := FData[Key]
  else
    raise Exception.CreateFmt('Clé "%s" non trouvée', [Key]);
end;

procedure TDataStore.DeleteKey(const Key: WideString);  
begin
  if not FData.Remove(Key) then
    raise Exception.CreateFmt('Clé "%s" non trouvée', [Key]);
end;

function TDataStore.HasKey(const Key: WideString): WordBool;  
begin
  Result := FData.ContainsKey(Key);
end;

function TDataStore.GetKeys: OleVariant;  
var
  Keys: TArray<string>;
  i: Integer;
begin
  Keys := FData.Keys.ToArray;
  Result := VarArrayCreate([0, Length(Keys) - 1], varOleStr);

  for i := 0 to High(Keys) do
    Result[i] := Keys[i];
end;

procedure TDataStore.Clear;  
begin
  FData.Clear;
end;

function TDataStore.GetCount: Integer;  
begin
  Result := FData.Count;
end;

initialization
  TAutoObjectFactory.Create(
    ComServer,
    TDataStore,
    CLASS_DataStore,
    'DataStore',
    'Simple Data Store COM Object',
    ciMultiInstance,
    tmApartment
  );

end.
```

### Client pour le DataStore

```pascal
program TestDataStore;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants;

procedure TestDataStore;  
var
  Store: Variant;
  Keys: Variant;
  i: Integer;
begin
  // Créer l'instance
  Store := CreateOleObject('DataStore');

  WriteLn('=== Test DataStore COM ===');
  WriteLn;

  // Stocker des valeurs
  Store.SetValue('nom', 'Alice');
  Store.SetValue('age', 25);
  Store.SetValue('ville', 'Paris');
  Store.SetValue('actif', True);
  Store.SetValue('salaire', 45000.50);

  WriteLn('Données stockées : ', Store.GetCount);
  WriteLn;

  // Récupérer des valeurs
  WriteLn('Nom : ', string(Store.GetValue('nom')));
  WriteLn('Âge : ', Integer(Store.GetValue('age')));
  WriteLn('Ville : ', string(Store.GetValue('ville')));
  WriteLn('Actif : ', Boolean(Store.GetValue('actif')));
  WriteLn('Salaire : ', Double(Store.GetValue('salaire')):0:2, ' €');
  WriteLn;

  // Vérifier l'existence
  WriteLn('Clé "nom" existe : ', Boolean(Store.HasKey('nom')));
  WriteLn('Clé "inexistante" existe : ', Boolean(Store.HasKey('inexistante')));
  WriteLn;

  // Lister toutes les clés
  Keys := Store.GetKeys;
  WriteLn('Liste des clés :');
  for i := VarArrayLowBound(Keys, 1) to VarArrayHighBound(Keys, 1) do
    WriteLn('  - ', string(Keys[i]));
  WriteLn;

  // Supprimer une clé
  Store.DeleteKey('ville');
  WriteLn('Après suppression de "ville" : ', Store.GetCount, ' éléments');
  WriteLn;

  // Tout effacer
  Store.Clear;
  WriteLn('Après Clear : ', Store.GetCount, ' éléments');
end;

begin
  try
    TestDataStore;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Gestion des événements COM

### Serveur avec événements

```pascal
library EventComServer;

{$mode objfpc}{$H+}

uses
  ComObj, ComServ, SysUtils, Windows, ActiveX;

type
  // Interface pour les événements
  ITimerEvents = interface(IDispatch)
    ['{E5678901-5678-5678-5678-56789ABCDEF0}']
    procedure OnTick(Seconds: Integer); safecall;
    procedure OnAlarm(const Message: WideString); safecall;
  end;

  // Interface principale
  ITimer = interface(IDispatch)
    ['{F6789012-6789-6789-6789-6789ABCDEF01}']
    procedure Start; safecall;
    procedure Stop; safecall;
    procedure SetAlarm(Seconds: Integer; const Message: WideString); safecall;
    function GetIsRunning: WordBool; safecall;
  end;

  // Implémentation
  TTimer = class(TAutoObject, ITimer, IConnectionPointContainer)
  private
    FConnectionPoint: IConnectionPoint;
    FIsRunning: Boolean;
    FElapsed: Integer;
    FAlarmTime: Integer;
    FAlarmMessage: string;
    procedure FireOnTick;
    procedure FireOnAlarm;
  public
    constructor Create;

    // ITimer
    procedure Start; safecall;
    procedure Stop; safecall;
    procedure SetAlarm(Seconds: Integer; const Message: WideString); safecall;
    function GetIsRunning: WordBool; safecall;
  end;

const
  CLASS_Timer: TGUID = '{01234567-0123-0123-0123-0123456789AB}';

{ TTimer }

constructor TTimer.Create;  
begin
  inherited Create;
  FIsRunning := False;
  FElapsed := 0;
  FAlarmTime := -1;
end;

procedure TTimer.Start;  
begin
  FIsRunning := True;
  FElapsed := 0;

  // Simulation d'un timer
  while FIsRunning do
  begin
    Sleep(1000);
    Inc(FElapsed);
    FireOnTick;

    if (FAlarmTime > 0) and (FElapsed >= FAlarmTime) then
    begin
      FireOnAlarm;
      FAlarmTime := -1;
    end;
  end;
end;

procedure TTimer.Stop;  
begin
  FIsRunning := False;
end;

procedure TTimer.SetAlarm(Seconds: Integer; const Message: WideString);  
begin
  FAlarmTime := Seconds;
  FAlarmMessage := Message;
end;

function TTimer.GetIsRunning: WordBool;  
begin
  Result := FIsRunning;
end;

procedure TTimer.FireOnTick;  
var
  EventSink: ITimerEvents;
begin
  if Assigned(FConnectionPoint) then
  begin
    // Notifier tous les clients connectés
    // (implémentation simplifiée)
  end;
end;

procedure TTimer.FireOnAlarm;  
var
  EventSink: ITimerEvents;
begin
  if Assigned(FConnectionPoint) then
  begin
    // Notifier l'alarme
  end;
end;

initialization
  TAutoObjectFactory.Create(
    ComServer,
    TTimer,
    CLASS_Timer,
    'Timer',
    'Timer COM Object with Events',
    ciMultiInstance,
    tmApartment
  );

end.
```

## Accès aux services Windows

### Gestion des services

```pascal
program ServiceManager;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants, ActiveX;

procedure ListerServices;  
var
  Locator, Service, ObjectSet, Obj: Variant;
  oEnum: IEnumVariant;
  Value: OleVariant;
  Fetched: Cardinal;
  Count: Integer;
  Name, State, StartMode: string;
begin
  CoInitialize(nil);
  try
    Locator := CreateOleObject('WbemScripting.SWbemLocator');
    Service := Locator.ConnectServer('.', 'root\CIMV2');

    // Lister les services
    ObjectSet := Service.ExecQuery('SELECT * FROM Win32_Service');
    oEnum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;

    WriteLn('=== Services Windows ===');
    WriteLn;
    WriteLn(Format('%-30s %-12s %-10s', ['Nom', 'État', 'Démarrage']));
    WriteLn(StringOfChar('-', 70));

    Count := 0;
    while oEnum.Next(1, Value, Fetched) = S_OK do
    begin
      Obj := Value;

      Name := VarToStr(Obj.DisplayName);
      State := VarToStr(Obj.State);
      StartMode := VarToStr(Obj.StartMode);

      // Tronquer le nom s'il est trop long
      if Length(Name) > 30 then
        Name := Copy(Name, 1, 27) + '...';

      WriteLn(Format('%-30s %-12s %-10s', [Name, State, StartMode]));
      Inc(Count);
    end;

    WriteLn;
    WriteLn('Total : ', Count, ' services');

  finally
    CoUninitialize;
  end;
end;

procedure ArreterService(const ServiceName: string);  
var
  Locator, Service, ObjectSet, Obj: Variant;
  oEnum: IEnumVariant;
  Value: OleVariant;
  Fetched: Cardinal;
  ReturnValue: OleVariant;
begin
  CoInitialize(nil);
  try
    Locator := CreateOleObject('WbemScripting.SWbemLocator');
    Service := Locator.ConnectServer('.', 'root\CIMV2');

    ObjectSet := Service.ExecQuery(
      Format('SELECT * FROM Win32_Service WHERE Name="%s"', [ServiceName])
    );
    oEnum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;

    if oEnum.Next(1, Value, Fetched) = S_OK then
    begin
      Obj := Value;
      WriteLn('Arrêt du service : ', VarToStr(Obj.DisplayName));

      ReturnValue := Obj.StopService;

      case Integer(ReturnValue) of
        0: WriteLn('Service arrêté avec succès');
        1: WriteLn('Requête non supportée');
        2: WriteLn('Accès refusé');
        3: WriteLn('Service dépendant actif');
        5: WriteLn('Service ne peut pas accepter de commande');
        10: WriteLn('Service déjà arrêté');
      else
        WriteLn('Code de retour : ', Integer(ReturnValue));
      end;
    end
    else
      WriteLn('Service non trouvé : ', ServiceName);

  finally
    CoUninitialize;
  end;
end;

begin
  try
    ListerServices;

    WriteLn;
    WriteLn('Exemple d''arrêt de service :');
    WriteLn('(nécessite les droits administrateur)');
    // ArreterService('Spooler'); // Service d'impression

  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Active Directory (ADSI)

### Interroger Active Directory

```pascal
program ActiveDirectoryQuery;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants, ActiveX;

procedure ListerUtilisateursAD;  
var
  Connection, Command, RecordSet: Variant;
  Count: Integer;
  Name, Email, Phone: string;
begin
  CoInitialize(nil);
  try
    // Créer une connexion ADO
    Connection := CreateOleObject('ADODB.Connection');
    Connection.Provider := 'ADsDSOObject';
    Connection.Open('Active Directory Provider');

    // Créer une commande
    Command := CreateOleObject('ADODB.Command');
    Command.ActiveConnection := Connection;

    // Requête LDAP
    Command.CommandText :=
      '<LDAP://DC=votredomaine,DC=com>;' +
      '(&(objectClass=user)(objectCategory=person));' +
      'cn,mail,telephoneNumber;subtree';

    // Exécuter la requête
    RecordSet := Command.Execute;

    WriteLn('=== Utilisateurs Active Directory ===');
    WriteLn;
    WriteLn(Format('%-30s %-35s %-15s', ['Nom', 'Email', 'Téléphone']));
    WriteLn(StringOfChar('-', 85));

    Count := 0;
    while not RecordSet.EOF do
    begin
      Name := VarToStr(RecordSet.Fields['cn'].Value);
      Email := VarToStr(RecordSet.Fields['mail'].Value);
      Phone := VarToStr(RecordSet.Fields['telephoneNumber'].Value);

      WriteLn(Format('%-30s %-35s %-15s', [Name, Email, Phone]));

      RecordSet.MoveNext;
      Inc(Count);
    end;

    WriteLn;
    WriteLn('Total : ', Count, ' utilisateurs');

    RecordSet.Close;
    Connection.Close;

  finally
    CoUninitialize;
  end;
end;

begin
  try
    ListerUtilisateursAD;
  except
    on E: Exception do
    begin
      WriteLn('Erreur : ', E.Message);
      WriteLn;
      WriteLn('Note : Cet exemple nécessite :');
      WriteLn('  - Un environnement Active Directory');
      WriteLn('  - Des droits d''accès appropriés');
      WriteLn('  - Ajuster le domaine dans la requête LDAP');
    end;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Gestion des erreurs COM

### Codes d'erreur HRESULT

```pascal
program GestionErreursCOM;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants, Windows, ActiveX;

function HResultToString(HR: HRESULT): string;  
begin
  case HR of
    S_OK: Result := 'S_OK - Succès';
    S_FALSE: Result := 'S_FALSE - Succès (faux)';
    E_NOTIMPL: Result := 'E_NOTIMPL - Non implémenté';
    E_NOINTERFACE: Result := 'E_NOINTERFACE - Interface non supportée';
    E_POINTER: Result := 'E_POINTER - Pointeur invalide';
    E_ABORT: Result := 'E_ABORT - Opération annulée';
    E_FAIL: Result := 'E_FAIL - Échec non spécifié';
    E_UNEXPECTED: Result := 'E_UNEXPECTED - Échec catastrophique';
    E_ACCESSDENIED: Result := 'E_ACCESSDENIED - Accès refusé';
    E_HANDLE: Result := 'E_HANDLE - Handle invalide';
    E_OUTOFMEMORY: Result := 'E_OUTOFMEMORY - Mémoire insuffisante';
    E_INVALIDARG: Result := 'E_INVALIDARG - Argument invalide';
  else
    Result := Format('Code: $%x', [HR]);
  end;
end;

procedure TesterGestionErreurs;  
var
  Obj: Variant;
  TextFile: Variant;
begin
  WriteLn('=== Test gestion d''erreurs COM ===');
  WriteLn;

  // Test 1 : Objet inexistant
  WriteLn('Test 1 : Créer un objet inexistant');
  try
    Obj := CreateOleObject('Objet.Inexistant');
  except
    on E: EOleSysError do
    begin
      WriteLn('  Exception EOleSysError capturée');
      WriteLn('  Message : ', E.Message);
      WriteLn('  ErrorCode : ', HResultToString(E.ErrorCode));
    end;
    on E: Exception do
      WriteLn('  Exception : ', E.ClassName, ' - ', E.Message);
  end;
  WriteLn;

  // Test 2 : Méthode inexistante
  WriteLn('Test 2 : Appeler une méthode inexistante');
  try
    Obj := CreateOleObject('Scripting.FileSystemObject');
    Obj.MethodeInexistante;
  except
    on E: EOleException do
    begin
      WriteLn('  Exception EOleException capturée');
      WriteLn('  Message : ', E.Message);
    end;
    on E: Exception do
      WriteLn('  Exception : ', E.ClassName, ' - ', E.Message);
  end;
  WriteLn;

  // Test 3 : Paramètre invalide
  WriteLn('Test 3 : Paramètre invalide');
  try
    Obj := CreateOleObject('Scripting.FileSystemObject');
    TextFile := Obj.OpenTextFile('', 1); // Chemin vide
  except
    on E: Exception do
      WriteLn('  Exception : ', E.Message);
  end;
end;

begin
  CoInitialize(nil);
  try
    TesterGestionErreurs;
  finally
    CoUninitialize;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Gestion robuste avec try..finally

```pascal
procedure UtilisationRobusteCOM;  
var
  ExcelApp: Variant;
  Workbook: Variant;
  FileName: string;
begin
  ExcelApp := Unassigned;
  Workbook := Unassigned;

  try
    // Créer Excel
    ExcelApp := CreateOleObject('Excel.Application');
    ExcelApp.Visible := False;
    ExcelApp.DisplayAlerts := False;

    try
      // Créer un classeur
      Workbook := ExcelApp.Workbooks.Add;

      // Faire des opérations
      Workbook.Worksheets[1].Cells[1, 1].Value := 'Test';

      // Sauvegarder
      FileName := GetTempDir + 'test.xlsx';
      Workbook.SaveAs(FileName);

      WriteLn('Fichier créé : ', FileName);

    finally
      // Fermer le classeur si ouvert
      if not VarIsEmpty(Workbook) then
      begin
        try
          Workbook.Close(False); // False = ne pas sauvegarder
        except
          // Ignorer les erreurs de fermeture
        end;
      end;
    end;

  finally
    // Quitter Excel si ouvert
    if not VarIsEmpty(ExcelApp) then
    begin
      try
        ExcelApp.Quit;
      except
        // Ignorer les erreurs de fermeture
      end;

      // Libérer la référence
      ExcelApp := Unassigned;
    end;
  end;
end;
```

## Performance et optimisation

### Éviter les appels multiples

```pascal
// ❌ Mauvais : Appels répétés
for i := 1 to 1000 do
  ExcelApp.Worksheets[1].Cells[i, 1].Value := i;

// ✅ Bon : Utiliser un tableau
var Data: Variant;  
Data := VarArrayCreate([1, 1000], varVariant);  
for i := 1 to 1000 do
  Data[i] := i;

ExcelApp.Worksheets[1].Range['A1:A1000'].Value := Data;
```

### Cache des références

```pascal
// ❌ Mauvais : Récupérer à chaque fois
for i := 1 to 100 do
  ExcelApp.Worksheets[1].Cells[i, 1].Value := i;

// ✅ Bon : Mettre en cache
var Sheet: Variant;  
Sheet := ExcelApp.Worksheets[1];  
for i := 1 to 100 do
  Sheet.Cells[i, 1].Value := i;
```

### Désactiver les mises à jour d'écran

```pascal
procedure RemplirExcelRapide;  
var
  Excel, Sheet: Variant;
  i: Integer;
  StartTime, EndTime: TDateTime;
begin
  Excel := CreateOleObject('Excel.Application');
  Excel.Visible := False;

  Sheet := Excel.Workbooks.Add.Worksheets[1];

  // Désactiver les mises à jour
  Excel.ScreenUpdating := False;
  Excel.Calculation := -4135; // xlCalculationManual
  Excel.EnableEvents := False;

  try
    StartTime := Now;

    // Remplir rapidement
    for i := 1 to 10000 do
      Sheet.Cells[i, 1].Value := i;

    EndTime := Now;
    WriteLn('Temps : ', MilliSecondsBetween(EndTime, StartTime), ' ms');

  finally
    // Réactiver
    Excel.Calculation := -4105; // xlCalculationAutomatic
    Excel.ScreenUpdating := True;
    Excel.EnableEvents := True;

    Excel.Visible := True;
  end;
end;
```

## Débogage COM

### Outils de diagnostic

#### OleView (OLE/COM Object Viewer)

Outil Microsoft pour explorer les objets COM enregistrés.

**Utilisation** :
1. Rechercher "OleView" dans le SDK Windows
2. Naviguer dans l'arbre des objets COM
3. Examiner les interfaces disponibles
4. Tester les appels de méthodes

#### Registry Editor (regedit)

Les objets COM sont enregistrés dans le registre Windows.

**Emplacements clés** :
```
HKEY_CLASSES_ROOT\CLSID\
  {GUID de votre objet}\
    InprocServer32\     → DLL (in-process)
    LocalServer32\      → EXE (out-of-process)
    ProgID\             → Nom convivial
    TypeLib\            → Bibliothèque de types
```

**Exemple de recherche** :
```
HKEY_CLASSES_ROOT\Excel.Application\
  CLSID → {00024500-0000-0000-C000-000000000046}
```

#### Process Monitor (ProcMon)

Outil Sysinternals pour tracer les accès registre et fichiers.

**Filtres utiles** :
- Path contains "CLSID"
- Process Name is "votre_app.exe"
- Operation is "RegQueryValue"

#### Tracer les appels COM

```pascal
program TracerCOM;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants;

type
  TComTracer = class
  private
    FLogFile: TextFile;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Log(const Message: string);
  end;

constructor TComTracer.Create(const FileName: string);  
begin
  AssignFile(FLogFile, FileName);
  Rewrite(FLogFile);
  Log('=== Début du traçage COM ===');
end;

destructor TComTracer.Destroy;  
begin
  Log('=== Fin du traçage COM ===');
  CloseFile(FLogFile);
  inherited;
end;

procedure TComTracer.Log(const Message: string);  
begin
  WriteLn(FLogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), ' - ', Message);
  Flush(FLogFile);
end;

var
  Tracer: TComTracer;
  Excel: Variant;
  Workbook: Variant;

begin
  Tracer := TComTracer.Create('com_trace.log');
  try
    Tracer.Log('Création de l''objet Excel');
    try
      Excel := CreateOleObject('Excel.Application');
      Tracer.Log('Excel créé avec succès');
    except
      on E: Exception do
      begin
        Tracer.Log('ERREUR création Excel: ' + E.Message);
        Exit;
      end;
    end;

    Tracer.Log('Rendre Excel visible');
    Excel.Visible := True;

    Tracer.Log('Créer un classeur');
    Workbook := Excel.Workbooks.Add;

    Tracer.Log('Écrire dans une cellule');
    Excel.Worksheets[1].Cells[1, 1].Value := 'Test';

    Tracer.Log('Fermeture');
    Workbook.Close(False);
    Excel.Quit;

    Tracer.Log('Opérations terminées avec succès');

  finally
    Tracer.Free;
  end;

  WriteLn('Log sauvegardé dans com_trace.log');
end.
```

### Débogage des fuites mémoire

```pascal
program DebugFuitesCOM;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants, Windows;

procedure AfficherCompteurRef(const Obj: Variant);  
var
  Unk: IUnknown;
begin
  if not VarIsEmpty(Obj) then
  begin
    Unk := IUnknown(TVarData(Obj).VUnknown);
    WriteLn('Compteur de références : ', Unk._AddRef - 1);
    Unk._Release; // Compenser le AddRef du test
  end;
end;

procedure TestFuites;  
var
  Excel: Variant;
  Workbook: Variant;
begin
  WriteLn('Création Excel...');
  Excel := CreateOleObject('Excel.Application');
  AfficherCompteurRef(Excel);

  WriteLn('Création classeur...');
  Workbook := Excel.Workbooks.Add;
  AfficherCompteurRef(Excel);

  WriteLn('Libération classeur...');
  Workbook := Unassigned;
  AfficherCompteurRef(Excel);

  WriteLn('Fermeture Excel...');
  Excel.Quit;
  Excel := Unassigned;

  WriteLn('Terminé - vérifiez dans le gestionnaire de tâches');
  WriteLn('EXCEL.EXE ne devrait plus être en cours d''exécution');
end;

begin
  TestFuites;
  ReadLn;
end.
```

## Problèmes courants et solutions

### Problème 1 : "Invalid class string"

**Erreur** :
```
EOleSysError: Invalid class string
```

**Causes possibles** :
1. ProgID incorrect
2. Objet COM non enregistré
3. Application non installée

**Solutions** :
```pascal
procedure VerifierObjetCOM(const ProgID: string);  
var
  ClassID: TGUID;
begin
  try
    if CLSIDFromProgID(PWideChar(WideString(ProgID)), ClassID) = S_OK then
      WriteLn(ProgID, ' est enregistré')
    else
      WriteLn(ProgID, ' n''est PAS enregistré');
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end;

// Utilisation
VerifierObjetCOM('Excel.Application');  
VerifierObjetCOM('Word.Application');  
VerifierObjetCOM('Objet.Inexistant');
```

### Problème 2 : Excel reste en mémoire

**Cause** : Références COM non libérées

**Solution** :
```pascal
procedure FermerExcelProprement;  
var
  Excel, Workbook, Worksheet: Variant;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    Workbook := Excel.Workbooks.Add;
    try
      Worksheet := Workbook.Worksheets[1];

      // Faire des opérations
      Worksheet.Cells[1, 1].Value := 'Test';

      // Libérer les références dans l'ordre inverse
      Worksheet := Unassigned;

    finally
      if not VarIsEmpty(Workbook) then
      begin
        Workbook.Close(False);
        Workbook := Unassigned;
      end;
    end;

  finally
    if not VarIsEmpty(Excel) then
    begin
      Excel.Quit;
      Excel := Unassigned;
    end;
  end;
end;
```

### Problème 3 : Erreur "Access denied"

**Cause** : Droits insuffisants, DCOM mal configuré

**Solutions** :
1. Exécuter en tant qu'administrateur
2. Configurer DCOM (dcomcnfg)
3. Vérifier les permissions du registre

```pascal
function EstAdministrateur: Boolean;  
var
  TokenHandle: THandle;
  ReturnLength: Cardinal;
  TokenInformation: TOKEN_ELEVATION;
begin
  Result := False;
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
  begin
    try
      if GetTokenInformation(TokenHandle, TokenElevation,
         @TokenInformation, SizeOf(TokenInformation), ReturnLength) then
        Result := TokenInformation.TokenIsElevated <> 0;
    finally
      CloseHandle(TokenHandle);
    end;
  end;
end;

begin
  if EstAdministrateur then
    WriteLn('Application exécutée en tant qu''administrateur')
  else
    WriteLn('ATTENTION: Droits administrateur requis pour certaines opérations');
end;
```

### Problème 4 : Variant type mismatch

**Cause** : Conversion de type incorrecte

**Solution** :
```pascal
procedure GestionTypesSafe;  
var
  Excel: Variant;
  Value: Variant;
begin
  Excel := CreateOleObject('Excel.Application');
  Excel.Workbooks.Add;

  // Lire une valeur
  Value := Excel.Worksheets[1].Cells[1, 1].Value;

  // Vérifier le type avant conversion
  if VarIsNull(Value) or VarIsEmpty(Value) then
    WriteLn('Cellule vide')
  else if VarIsNumeric(Value) then
    WriteLn('Nombre : ', Double(Value):0:2)
  else if VarIsStr(Value) then
    WriteLn('Texte : ', string(Value))
  else if VarType(Value) = varDate then
    WriteLn('Date : ', VarToDateTime(Value))
  else if VarIsType(Value, varBoolean) then
    WriteLn('Booléen : ', Boolean(Value))
  else
    WriteLn('Type inconnu : ', VarType(Value));

  Excel.Quit;
end;
```

## Bonnes pratiques

### ✅ À faire

#### 1. Toujours utiliser try..finally

```pascal
var
  ComObj: Variant;
begin
  ComObj := CreateOleObject('...');
  try
    // Utilisation
  finally
    ComObj := Unassigned;
  end;
end;
```

#### 2. Libérer dans l'ordre inverse

```pascal
// Ordre de création
Excel := CreateOleObject('Excel.Application');  
Workbook := Excel.Workbooks.Add;  
Sheet := Workbook.Worksheets[1];

// Ordre de libération (inverse)
Sheet := Unassigned;  
Workbook.Close(False);  
Workbook := Unassigned;  
Excel.Quit;  
Excel := Unassigned;
```

#### 3. Désactiver les alertes et l'affichage

```pascal
Excel.DisplayAlerts := False;  
Excel.ScreenUpdating := False;  
Excel.Visible := False;
```

#### 4. Gérer les erreurs spécifiquement

```pascal
try
  // Code COM
except
  on E: EOleSysError do
    // Erreur système COM
  on E: EOleException do
    // Erreur applicative COM
  on E: EOleError do
    // Erreur générale COM
  on E: Exception do
    // Autres erreurs
end;
```

#### 5. Vérifier l'existence avant utilisation

```pascal
function ApplicationEstInstallee(const ProgID: string): Boolean;  
var
  ClassID: TGUID;
begin
  Result := CLSIDFromProgID(PWideChar(WideString(ProgID)), ClassID) = S_OK;
end;

if ApplicationEstInstallee('Excel.Application') then
  // Utiliser Excel
else
  ShowMessage('Microsoft Excel n''est pas installé');
```

### ❌ À éviter

#### 1. Ne pas libérer les objets COM

```pascal
// ❌ Mauvais
procedure Mauvais;  
var
  Excel: Variant;
begin
  Excel := CreateOleObject('Excel.Application');
  // Utilisation
  // Oubli de libérer → fuite mémoire
end;
```

#### 2. Utiliser des Variants pour tout

```pascal
// ❌ Mauvais
var
  Excel: Variant;
  i: Variant;
begin
  for i := 1 to 100 do  // Variant dans une boucle = lent
    Excel.Cells[i, 1] := i;
end;

// ✅ Bon
var
  Excel: Variant;
  i: Integer;
begin
  for i := 1 to 100 do
    Excel.Cells[i, 1] := i;
end;
```

#### 3. Ignorer les erreurs

```pascal
// ❌ Mauvais
try
  Excel.DoSomething;
except
  // Ignorer silencieusement
end;

// ✅ Bon
try
  Excel.DoSomething;
except
  on E: Exception do
  begin
    LogError(E.Message);
    raise;  // Propager si nécessaire
  end;
end;
```

#### 4. Ne pas tester l'installation

```pascal
// ❌ Mauvais
Excel := CreateOleObject('Excel.Application');  // Peut échouer

// ✅ Bon
if ApplicationEstInstallee('Excel.Application') then
  Excel := CreateOleObject('Excel.Application')
else
  raise Exception.Create('Excel non installé');
```

## Exemples pratiques avancés

### Génération de rapport PDF via Excel

```pascal
program ExcelToPDF;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants;

procedure CreerRapportPDF;  
var
  Excel, Workbook, Sheet: Variant;
  PDFFile: string;
begin
  Excel := CreateOleObject('Excel.Application');
  Excel.Visible := False;
  Excel.DisplayAlerts := False;

  try
    Workbook := Excel.Workbooks.Add;
    Sheet := Workbook.Worksheets[1];

    // Créer un rapport
    Sheet.Cells[1, 1].Value := 'RAPPORT MENSUEL';
    Sheet.Cells[1, 1].Font.Size := 16;
    Sheet.Cells[1, 1].Font.Bold := True;

    Sheet.Cells[3, 1].Value := 'Élément';
    Sheet.Cells[3, 2].Value := 'Janvier';
    Sheet.Cells[3, 3].Value := 'Février';
    Sheet.Cells[3, 4].Value := 'Mars';

    Sheet.Cells[4, 1].Value := 'Ventes';
    Sheet.Cells[4, 2].Value := 15000;
    Sheet.Cells[4, 3].Value := 18000;
    Sheet.Cells[4, 4].Value := 22000;

    Sheet.Cells[5, 1].Value := 'Coûts';
    Sheet.Cells[5, 2].Value := 8000;
    Sheet.Cells[5, 3].Value := 9500;
    Sheet.Cells[5, 4].Value := 11000;

    // Formater
    Sheet.Range['A3:D5'].Borders.LineStyle := 1;
    Sheet.Range['A3:D3'].Font.Bold := True;
    Sheet.Range['B4:D5'].NumberFormat := '#,##0 €';

    // Exporter en PDF
    PDFFile := ExtractFilePath(ParamStr(0)) + 'rapport.pdf';
    Workbook.ExportAsFixedFormat(0, PDFFile);  // 0 = xlTypePDF

    WriteLn('Rapport PDF créé : ', PDFFile);

    Workbook.Close(False);

  finally
    Excel.Quit;
    Excel := Unassigned;
  end;
end;

begin
  try
    CreerRapportPDF;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Fusion de courrier avec Word

```pascal
program FusionCourrier;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants, Classes;

procedure CreerLettresPersonnalisees;  
var
  Word, Doc: Variant;
  Destinataires: TStringList;
  i: Integer;
  OutputFile: string;
  Ligne, Nom, Email, Ville: string;
  P1, P2: Integer;
begin
  Destinataires := TStringList.Create;
  try
    // Liste de destinataires
    Destinataires.Add('Alice Dupont|alice@example.com|Paris');
    Destinataires.Add('Bob Martin|bob@example.com|Lyon');
    Destinataires.Add('Claire Dubois|claire@example.com|Marseille');

    Word := CreateOleObject('Word.Application');
    Word.Visible := False;

    try
      for i := 0 to Destinataires.Count - 1 do
      begin
        Ligne := Destinataires[i];
        P1 := Pos('|', Ligne);
        Nom := Copy(Ligne, 1, P1 - 1);
        Delete(Ligne, 1, P1);
        P2 := Pos('|', Ligne);
        Email := Copy(Ligne, 1, P2 - 1);
        Ville := Copy(Ligne, P2 + 1, Length(Ligne));

        // Créer un nouveau document
        Doc := Word.Documents.Add;

        // En-tête
        Doc.Content.Text := Format(
          'Madame, Monsieur %s,'#13#13 +
          'Nous avons le plaisir de vous inviter à notre événement qui se tiendra à %s.'#13#13 +
          'Pour confirmation, veuillez répondre à %s.'#13#13 +
          'Cordialement,'#13 +
          'L''équipe organisation',
          [Nom, Ville, Email]
        );

        // Formater
        Doc.Paragraphs[1].Range.Font.Size := 12;
        Doc.Paragraphs[1].Range.Font.Bold := True;

        // Sauvegarder
        OutputFile := Format('lettre_%s.docx', [StringReplace(Nom, ' ', '_', [rfReplaceAll])]);
        Doc.SaveAs(ExtractFilePath(ParamStr(0)) + OutputFile);
        Doc.Close;

        WriteLn('Lettre créée : ', OutputFile);
      end;

    finally
      Word.Quit;
      Word := Unassigned;
    end;

  finally
    Destinataires.Free;
  end;
end;

begin
  try
    CreerLettresPersonnalisees;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Automatisation complète : Excel + Word + Outlook

```pascal
program AutomatisationComplete;

{$mode objfpc}{$H+}

uses
  SysUtils, ComObj, Variants;

procedure GenererEtEnvoyerRapport;  
var
  Excel, Workbook, Sheet: Variant;
  Word, Doc: Variant;
  Outlook, Mail: Variant;
  ExcelFile, WordFile: string;
begin
  WriteLn('=== Automatisation complète Office ===');
  WriteLn;

  // 1. Créer le fichier Excel
  WriteLn('1. Création du rapport Excel...');
  Excel := CreateOleObject('Excel.Application');
  Excel.Visible := False;
  try
    Workbook := Excel.Workbooks.Add;
    Sheet := Workbook.Worksheets[1];

    Sheet.Cells[1, 1].Value := 'Données du rapport';
    Sheet.Cells[2, 1].Value := 'Total ventes';
    Sheet.Cells[2, 2].Value := 125000;

    ExcelFile := ExtractFilePath(ParamStr(0)) + 'rapport_data.xlsx';
    Workbook.SaveAs(ExcelFile);
    Workbook.Close(False);
  finally
    Excel.Quit;
    Excel := Unassigned;
  end;
  WriteLn('   Fichier Excel créé : ', ExcelFile);

  // 2. Créer le document Word
  WriteLn('2. Création du document Word...');
  Word := CreateOleObject('Word.Application');
  Word.Visible := False;
  try
    Doc := Word.Documents.Add;

    Doc.Content.Text :=
      'RAPPORT MENSUEL'#13#13 +
      'Veuillez trouver ci-joint les données détaillées du mois.'#13#13 +
      'Cordialement,';

    Doc.Paragraphs[1].Range.Font.Size := 16;
    Doc.Paragraphs[1].Range.Font.Bold := True;

    WordFile := ExtractFilePath(ParamStr(0)) + 'rapport_texte.docx';
    Doc.SaveAs(WordFile);
    Doc.Close;
  finally
    Word.Quit;
    Word := Unassigned;
  end;
  WriteLn('   Fichier Word créé : ', WordFile);

  // 3. Envoyer par email via Outlook
  WriteLn('3. Préparation de l''email...');
  Outlook := CreateOleObject('Outlook.Application');
  try
    Mail := Outlook.CreateItem(0); // olMailItem

    Mail.Subject := 'Rapport mensuel - ' + FormatDateTime('mmmm yyyy', Now);
    Mail.To := 'destinataire@example.com';
    Mail.Body :=
      'Bonjour,'#13#10#13#10 +
      'Veuillez trouver ci-joint le rapport mensuel.'#13#10#13#10 +
      'Cordialement,';

    // Ajouter les pièces jointes
    Mail.Attachments.Add(ExcelFile);
    Mail.Attachments.Add(WordFile);

    // Afficher l'email (l'utilisateur peut modifier avant d'envoyer)
    Mail.Display;
    // Ou envoyer directement : Mail.Send;

  finally
    Outlook := Unassigned;
  end;
  WriteLn('   Email préparé dans Outlook');

  WriteLn;
  WriteLn('Processus terminé avec succès !');
end;

begin
  try
    GenererEtEnvoyerRapport;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Sécurité et COM

### Considérations de sécurité

#### 1. Validation des entrées

```pascal
function SauvegarderExcelSecurise(const FileName: string): Boolean;  
var
  Excel, Workbook: Variant;
begin
  Result := False;

  // Valider le nom de fichier
  if Trim(FileName) = '' then
    raise Exception.Create('Nom de fichier vide');

  if not DirectoryExists(ExtractFilePath(FileName)) then
    raise Exception.Create('Répertoire inexistant');

  // Vérifier les caractères dangereux
  if Pos('..', FileName) > 0 then
    raise Exception.Create('Chemin invalide');

  Excel := CreateOleObject('Excel.Application');
  try
    Workbook := Excel.Workbooks.Add;
    Workbook.SaveAs(FileName);
    Workbook.Close(False);
    Result := True;
  finally
    Excel.Quit;
    Excel := Unassigned;
  end;
end;
```

#### 2. Gestion des macros

```pascal
procedure DesactiverMacros;  
var
  Excel: Variant;
  Workbook: Variant;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    // Désactiver les macros pour la sécurité
    Excel.AutomationSecurity := 3; // msoAutomationSecurityForceDisable

    // Ouvrir un fichier
    Workbook := Excel.Workbooks.Open('fichier.xlsx');

    // Traitement...

    Workbook.Close(False);
  finally
    Excel.Quit;
    Excel := Unassigned;
  end;
end;
```

#### 3. Timeout pour éviter les blocages

```pascal
function ExecuterAvecTimeout(const Code: TProcedure; TimeoutMs: Integer): Boolean;  
var
  Thread: TThread;
  StartTime: TDateTime;
begin
  Result := False;

  Thread := TThread.CreateAnonymousThread(Code);
  Thread.FreeOnTerminate := False;

  try
    Thread.Start;
    StartTime := Now;

    while not Thread.Finished do
    begin
      Sleep(100);
      if MilliSecondsBetween(Now, StartTime) > TimeoutMs then
      begin
        Thread.Terminate;
        raise Exception.Create('Timeout dépassé');
      end;
    end;

    Result := True;
  finally
    Thread.Free;
  end;
end;
```

## Alternatives à COM

### Quand éviter COM

1. **Performance critique** : COM a un overhead
2. **Applications portables** : COM est spécifique Windows
3. **Déploiement simplifié** : Dépendance aux applications installées
4. **Maintenance** : API COM peut changer entre versions

### Alternatives modernes

#### 1. Bibliothèques natives

```pascal
// Au lieu de COM Excel, utiliser fpspreadsheet
uses
  fpspreadsheet, xlsbiff8;

var
  Workbook: TsWorkbook;
  Sheet: TsWorksheet;
begin
  Workbook := TsWorkbook.Create;
  try
    Sheet := Workbook.AddWorksheet('Feuille1');
    Sheet.WriteText(0, 0, 'Hello');
    Sheet.WriteNumber(1, 0, 42);
    Workbook.WriteToFile('output.xlsx', sfOOXML);
  finally
    Workbook.Free;
  end;
end;
```

#### 2. API REST

```pascal
// Utiliser Microsoft Graph API au lieu de COM Outlook
uses
  fphttpclient, fpjson;

var
  Client: TFPHTTPClient;
  Response: string;
  JSON: TJSONObject;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + AccessToken);
    Response := Client.Get('https://graph.microsoft.com/v1.0/me/messages');
    JSON := GetJSON(Response) as TJSONObject;
    // Traiter les emails
  finally
    Client.Free;
  end;
end;
```

#### 3. Formats de fichiers directs

- **Excel** : fpspreadsheet, OpenXML SDK
- **Word** : docx4j (via Java), bibliothèques RTF
- **PDF** : fpReport, QuickReport, FastReport

## Checklist de déploiement

Avant de distribuer une application utilisant COM :

- [ ] Vérifier que les applications Office sont installées (ou documenter)
- [ ] Tester sur différentes versions d'Office (2013, 2016, 2019, 365)
- [ ] Gérer gracieusement l'absence d'applications
- [ ] Libérer toutes les références COM proprement
- [ ] Tester les fuites mémoire (Task Manager)
- [ ] Documenter les prérequis (versions Office minimum)
- [ ] Fournir un mode dégradé si COM non disponible
- [ ] Tester avec des droits utilisateur limités
- [ ] Vérifier la compatibilité 32/64 bits
- [ ] Logger les erreurs COM pour le support
- [ ] Prévoir des alternatives (export CSV, etc.)

## Conclusion

COM et ActiveX représentent des technologies matures et puissantes pour l'automatisation Windows. Bien que considérées comme "legacy" par certains, elles restent **largement utilisées et supportées** dans l'écosystème Windows professionnel.

### Points clés à retenir

✅ **Avantages de COM** :
- Automatisation complète d'Office (Excel, Word, Outlook)
- Accès aux services système Windows (WMI, ADSI)
- Standard bien établi et stable
- Pas de dépendances externes à distribuer
- Documentation abondante

⚠️ **Limitations** :
- Spécifique à Windows
- Dépendance aux applications installées
- Performance (overhead d'interopérabilité)
- Gestion mémoire délicate
- Évolution des versions Office

### Quand utiliser COM depuis FreePascal ?

**Utilisez COM si** :
- Vous ciblez uniquement Windows
- Vous devez interagir avec Office
- Vous avez besoin de WMI/services Windows
- Les utilisateurs ont Office installé
- L'automatisation est ponctuelle

**Évitez COM si** :
- Vous visez la portabilité multi-plateforme
- La performance est critique
- Vous voulez un déploiement simplifié
- Les utilisateurs peuvent ne pas avoir Office

### Ressources pour aller plus loin

**Documentation Microsoft** :
- COM Technical Overview
- Office VBA Object Model Reference
- WMI Reference

**Outils** :
- OleView (SDK Windows)
- Process Monitor (Sysinternals)
- COM+ Component Services (comexp.msc)

**Communauté** :
- Forum Lazarus (section Windows)
- Stack Overflow (tags: delphi, com, ole)
- Microsoft Docs

⏭️ [D-Bus sous Linux](/19-interoperabilite-bindings/05-dbus-linux.md)
