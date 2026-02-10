🔝 Retour au [Sommaire](/SOMMAIRE.md)

# COM/ActiveX et OLE avec FreePascal/Lazarus sous Windows

## Introduction : Qu'est-ce que COM, ActiveX et OLE ?

### Définitions simples

**COM (Component Object Model)** est une technologie Microsoft qui permet à différents programmes Windows de communiquer entre eux, même s'ils sont écrits dans des langages différents. Imaginez COM comme un "langage universel" que tous les programmes Windows peuvent comprendre pour échanger des données et des fonctionnalités.

**ActiveX** est une extension de COM qui permet de créer des composants réutilisables pouvant être intégrés dans différentes applications. Par exemple, un graphique Excel peut être intégré dans un document Word grâce à ActiveX.

**OLE (Object Linking and Embedding)** utilise COM pour permettre l'intégration d'objets d'une application dans une autre. C'est ce qui vous permet de copier un tableau Excel et de le coller dans Word tout en gardant ses fonctionnalités.

### Pourquoi utiliser COM/ActiveX ?

- **Automatisation** : Contrôler des applications Microsoft Office depuis votre programme
- **Réutilisation** : Utiliser des composants existants sans les recréer
- **Intégration** : Faire communiquer votre application avec d'autres logiciels Windows
- **Extension** : Créer des plugins pour d'autres applications

## Concepts fondamentaux

### Les interfaces COM

Une interface COM est comme un contrat qui définit quelles fonctions un objet doit fournir. Toutes les interfaces COM héritent de `IUnknown`, qui fournit trois méthodes essentielles :

```pascal
IUnknown = interface
  function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  function _AddRef: Integer; stdcall;
  function _Release: Integer; stdcall;
end;
```

- **QueryInterface** : Demande si l'objet supporte une interface spécifique
- **_AddRef** : Augmente le compteur de références (pour la gestion mémoire)
- **_Release** : Diminue le compteur de références

### Les GUIDs (Globally Unique Identifiers)

Chaque interface et classe COM possède un identifiant unique au monde (GUID). C'est une chaîne de 128 bits qui ressemble à ceci :
```pascal
'{00000000-0000-0000-C000-000000000046}'
```

### Le registre Windows et COM

Windows stocke les informations sur les composants COM dans le registre système. Quand vous demandez à utiliser un composant, Windows consulte le registre pour savoir où le trouver.

## Configuration de FreePascal/Lazarus pour COM

### Unités nécessaires

Pour utiliser COM dans FreePascal, vous devez inclure ces unités :

```pascal
uses
  Windows,    // Types Windows de base
  ActiveX,    // Support COM/ActiveX
  ComObj,     // Création et gestion d'objets COM
  Variants;   // Support des types Variant pour l'automatisation
```

### Initialisation de COM

Avant d'utiliser COM dans votre application, vous devez l'initialiser :

```pascal
program MonProgrammeCOM;

uses
  Windows, ActiveX, ComObj;

begin
  // Initialiser COM pour ce thread
  CoInitialize(nil);
  try
    // Votre code COM ici

  finally
    // Toujours libérer COM à la fin
    CoUninitialize;
  end;
end.
```

## Utilisation de composants COM existants

### Exemple 1 : Automatisation de Microsoft Word

Voici comment créer un document Word depuis votre application FreePascal :

```pascal
program AutomatiserWord;

uses
  SysUtils, ComObj, Variants, ActiveX;

var
  WordApp, Document: Variant;

begin
  CoInitialize(nil);
  try
    try
      // Créer une instance de Word
      WordApp := CreateOleObject('Word.Application');

      // Rendre Word visible (optionnel)
      WordApp.Visible := True;

      // Créer un nouveau document
      Document := WordApp.Documents.Add;

      // Ajouter du texte
      WordApp.Selection.TypeText('Bonjour depuis FreePascal !');
      WordApp.Selection.TypeParagraph; // Nouvelle ligne
      WordApp.Selection.TypeText('Ceci est créé automatiquement.');

      // Mettre en gras le premier paragraphe
      Document.Paragraphs.Item(1).Range.Font.Bold := True;

      // Sauvegarder le document
      Document.SaveAs('C:\MonDocument.docx');

      WriteLn('Document créé avec succès !');

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    CoUninitialize;
  end;

  ReadLn; // Pause pour voir le résultat
end.
```

### Exemple 2 : Automatisation d'Excel

Créons et remplissons une feuille Excel :

```pascal
program AutomatiserExcel;

uses
  SysUtils, ComObj, Variants, ActiveX;

var
  ExcelApp, Workbook, Sheet: Variant;
  i: Integer;

begin
  CoInitialize(nil);
  try
    try
      // Créer une instance d'Excel
      ExcelApp := CreateOleObject('Excel.Application');
      ExcelApp.Visible := True;

      // Créer un nouveau classeur
      Workbook := ExcelApp.Workbooks.Add;
      Sheet := Workbook.Worksheets[1];

      // Ajouter des en-têtes
      Sheet.Cells[1, 1].Value := 'Nom';
      Sheet.Cells[1, 2].Value := 'Âge';
      Sheet.Cells[1, 3].Value := 'Ville';

      // Formater les en-têtes
      Sheet.Range['A1:C1'].Font.Bold := True;
      Sheet.Range['A1:C1'].Interior.Color := $00FFFF; // Jaune

      // Ajouter des données
      Sheet.Cells[2, 1].Value := 'Jean Dupont';
      Sheet.Cells[2, 2].Value := 30;
      Sheet.Cells[2, 3].Value := 'Paris';

      Sheet.Cells[3, 1].Value := 'Marie Martin';
      Sheet.Cells[3, 2].Value := 25;
      Sheet.Cells[3, 3].Value := 'Lyon';

      // Ajuster la largeur des colonnes
      Sheet.Columns['A:C'].AutoFit;

      // Ajouter une formule
      Sheet.Cells[4, 2].Formula := '=AVERAGE(B2:B3)';
      Sheet.Cells[4, 1].Value := 'Moyenne âge :';

      WriteLn('Feuille Excel créée avec succès !');

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    CoUninitialize;
  end;

  ReadLn;
end.
```

## Utilisation avec la liaison précoce (Early Binding)

La liaison précoce est plus rapide et offre l'auto-complétion dans l'IDE. Pour l'utiliser, vous devez importer la bibliothèque de types.

### Importation d'une bibliothèque de types

Dans Lazarus :
1. Menu **Tools** → **Import Type Library**
2. Sélectionnez la bibliothèque (ex: Microsoft Word)
3. Générez l'unité Pascal

Exemple avec l'unité générée :

```pascal
uses
  Windows, ActiveX, ComObj,
  Word_TLB; // Unité générée par l'import

var
  WordApp: _Application;
  Doc: _Document;

begin
  CoInitialize(nil);
  try
    // Création avec liaison précoce
    WordApp := CoApplication.Create;
    WordApp.Visible := True;

    Doc := WordApp.Documents.Add(EmptyParam, EmptyParam,
                                  EmptyParam, EmptyParam);

    WordApp.Selection.TypeText('Texte avec liaison précoce');

  finally
    CoUninitialize;
  end;
end;
```

## Création de vos propres composants COM

### Étape 1 : Définir l'interface

```pascal
unit MonComposantCOM;

interface

uses
  Windows, ActiveX, Classes, ComObj;

type
  // Définir le GUID de l'interface
  IMonCalculateur = interface(IDispatch)
    ['{12345678-1234-1234-1234-123456789ABC}']
    function Additionner(a, b: Integer): Integer; safecall;
    function Multiplier(a, b: Integer): Integer; safecall;
    function GetDernierResultat: Integer; safecall;
    property DernierResultat: Integer read GetDernierResultat;
  end;

  // Classe qui implémente l'interface
  TMonCalculateur = class(TAutoObject, IMonCalculateur)
  private
    FDernierResultat: Integer;
  protected
    function Additionner(a, b: Integer): Integer; safecall;
    function Multiplier(a, b: Integer): Integer; safecall;
    function GetDernierResultat: Integer; safecall;
  end;

const
  CLASS_MonCalculateur: TGUID = '{87654321-4321-4321-4321-CBA987654321}';

implementation

uses ComServ;

function TMonCalculateur.Additionner(a, b: Integer): Integer;  
begin
  FDernierResultat := a + b;
  Result := FDernierResultat;
end;

function TMonCalculateur.Multiplier(a, b: Integer): Integer;  
begin
  FDernierResultat := a * b;
  Result := FDernierResultat;
end;

function TMonCalculateur.GetDernierResultat: Integer;  
begin
  Result := FDernierResultat;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TMonCalculateur,
    CLASS_MonCalculateur, ciMultiInstance, tmApartment);

end.
```

### Étape 2 : Enregistrer le composant

Pour utiliser votre composant COM, il doit être enregistré dans Windows :

```pascal
// Dans votre programme principal ou DLL
program EnregistrerComposant;

uses
  Windows, ComServ;

begin
  // L'enregistrement se fait automatiquement si compilé en DLL
  // avec les paramètres /regserver ou /unregserver
end.
```

### Étape 3 : Utiliser votre composant

```pascal
program UtiliserMonComposant;

uses
  ComObj, Variants, ActiveX;

var
  Calc: Variant;
  Resultat: Integer;

begin
  CoInitialize(nil);
  try
    // Créer une instance de votre composant
    Calc := CreateOleObject('MonProjet.MonCalculateur');

    // Utiliser les méthodes
    Resultat := Calc.Additionner(5, 3);
    WriteLn('5 + 3 = ', Resultat);

    Resultat := Calc.Multiplier(4, 7);
    WriteLn('4 × 7 = ', Resultat);

    WriteLn('Dernier résultat : ', Calc.DernierResultat);

  finally
    CoUninitialize;
  end;

  ReadLn;
end;
```

## Gestion des erreurs COM

### Codes de retour HRESULT

COM utilise des codes HRESULT pour indiquer le succès ou l'échec :

```pascal
var
  hr: HRESULT;

begin
  hr := SomeComFunction();

  if SUCCEEDED(hr) then
    WriteLn('Succès !')
  else if FAILED(hr) then
    WriteLn('Échec avec code : ', IntToHex(hr, 8));

  // Vérifications spécifiques
  case hr of
    S_OK: WriteLn('Tout va bien');
    S_FALSE: WriteLn('Opération réussie mais avec avertissement');
    E_NOTIMPL: WriteLn('Non implémenté');
    E_NOINTERFACE: WriteLn('Interface non supportée');
    E_FAIL: WriteLn('Échec général');
  end;
end;
```

### Gestion des exceptions

```pascal
try
  // Code COM
  WordApp := CreateOleObject('Word.Application');
except
  on E: EOleException do
  begin
    WriteLn('Erreur OLE : ', E.Message);
    WriteLn('Code erreur : ', E.ErrorCode);
    WriteLn('Source : ', E.Source);
  end;
  on E: Exception do
    WriteLn('Erreur générale : ', E.Message);
end;
```

## Utilisation d'ActiveX dans les interfaces graphiques

### Intégration d'un contrôle ActiveX dans une Form Lazarus

```pascal
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActiveX, ComObj;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FWebBrowser: Variant;
    procedure CreerNavigateur;
  public
  end;

var
  Form1: TForm1;

implementation

procedure TForm1.FormCreate(Sender: TObject);  
begin
  CoInitialize(nil);
  CreerNavigateur;
end;

procedure TForm1.FormDestroy(Sender: TObject);  
begin
  FWebBrowser := Unassigned;
  CoUninitialize;
end;

procedure TForm1.CreerNavigateur;  
var
  WebBrowserControl: TOleControl;
begin
  try
    // Créer un contrôle Internet Explorer
    FWebBrowser := CreateOleObject('Shell.Explorer');

    // Configuration du contrôle
    WebBrowserControl := TOleControl.Create(Self);
    WebBrowserControl.Parent := Self;
    WebBrowserControl.Align := alClient;
    WebBrowserControl.OleObject := FWebBrowser;

    // Naviguer vers une page
    FWebBrowser.Navigate('https://www.example.com');

  except
    on E: Exception do
      ShowMessage('Erreur création navigateur : ' + E.Message);
  end;
end;

end.
```

## OLE : Object Linking and Embedding

### Embedding (Incorporation) d'objets

L'incorporation permet d'intégrer complètement un objet dans votre application :

```pascal
program OLEEmbedding;

uses
  Windows, ActiveX, ComObj, Variants;

var
  Container: Variant;
  EmbeddedDoc: Variant;

begin
  CoInitialize(nil);
  try
    // Créer un conteneur OLE (exemple avec Word)
    Container := CreateOleObject('Word.Application');
    Container.Visible := True;

    // Créer un document
    Container.Documents.Add;

    // Incorporer un objet Excel dans le document Word
    EmbeddedDoc := Container.Selection.InlineShapes.AddOLEObject(
      ClassType := 'Excel.Sheet',
      FileName := '',
      LinkToFile := False,
      DisplayAsIcon := False
    );

    // L'objet Excel est maintenant incorporé dans Word
    // Les données sont stockées dans le document Word

    WriteLn('Objet Excel incorporé dans Word');

  finally
    CoUninitialize;
  end;

  ReadLn;
end;
```

### Linking (Liaison) d'objets

La liaison crée une référence vers un fichier externe :

```pascal
program OLELinking;

uses
  Windows, ActiveX, ComObj, Variants;

var
  WordApp, Doc: Variant;
  LinkedObject: Variant;

begin
  CoInitialize(nil);
  try
    // Créer Word
    WordApp := CreateOleObject('Word.Application');
    WordApp.Visible := True;
    Doc := WordApp.Documents.Add;

    // Lier un fichier Excel existant
    LinkedObject := Doc.InlineShapes.AddOLEObject(
      ClassType := '',
      FileName := 'C:\MonFichier.xlsx',
      LinkToFile := True,  // True = liaison, False = incorporation
      DisplayAsIcon := False
    );

    // Le fichier reste externe, seul le lien est dans Word
    WriteLn('Fichier Excel lié au document Word');

    // Mise à jour du lien
    LinkedObject.LinkFormat.Update;

  finally
    CoUninitialize;
  end;

  ReadLn;
end;
```

## Conseils et bonnes pratiques

### 1. Gestion de la mémoire

- **Toujours** appeler `CoInitialize`/`CoUninitialize` en paire
- Les objets COM utilisent le comptage de références automatique
- Assignez `Unassigned` aux variants COM quand vous avez fini

```pascal
var
  ExcelApp: Variant;
begin
  CoInitialize(nil);
  try
    ExcelApp := CreateOleObject('Excel.Application');
    try
      // Utilisation d'Excel
    finally
      ExcelApp := Unassigned; // Libération propre
    end;
  finally
    CoUninitialize;
  end;
end;
```

### 2. Vérification de la disponibilité

Vérifiez toujours si un composant COM est disponible :

```pascal
function ExcelDisponible: Boolean;  
var
  ExcelApp: Variant;
begin
  Result := False;
  try
    ExcelApp := CreateOleObject('Excel.Application');
    ExcelApp := Unassigned;
    Result := True;
  except
    // Excel n'est pas installé
  end;
end;
```

### 3. Performance

- Utilisez la liaison précoce quand possible (plus rapide)
- Minimisez les appels COM (regroupez les opérations)
- Évitez les boucles avec des appels COM individuels

```pascal
// Mauvais : lent
for i := 1 to 100 do
  Sheet.Cells[i, 1].Value := i;

// Bon : rapide
var
  Data: Variant;
begin
  Data := VarArrayCreate([1, 100, 1, 1], varVariant);
  for i := 1 to 100 do
    Data[i, 1] := i;
  Sheet.Range['A1:A100'].Value := Data;
end;
```

### 4. Thread Safety

COM a différents modèles de threading :

```pascal
// Initialisation pour Single Thread Apartment (STA)
CoInitialize(nil);

// Initialisation pour Multi Thread Apartment (MTA)
CoInitializeEx(nil, COINIT_MULTITHREADED);
```

La plupart des applications Office nécessitent STA.

## Dépannage courant

### Problème : "Class not registered"

**Solution** : Le composant COM n'est pas installé ou enregistré
```bash
regsvr32 moncomposant.dll
```

### Problème : "Interface not supported"

**Solution** : Vérifiez que vous utilisez la bonne interface
```pascal
if Supports(MonObjet, IMonInterface, MonInterface) then
  // Utiliser MonInterface
else
  ShowMessage('Interface non supportée');
```

### Problème : Excel/Word reste en mémoire

**Solution** : Fermez correctement l'application
```pascal
try
  // Votre code
finally
  if not VarIsEmpty(ExcelApp) then
  begin
    ExcelApp.Quit;
    ExcelApp := Unassigned;
  end;
end;
```

## Ressources supplémentaires

### Documentation officielle
- [MSDN COM Documentation](https://docs.microsoft.com/en-us/windows/win32/com/component-object-model--com--portal)
- [FreePascal COM Programming](https://wiki.freepascal.org/COM_Programming)

### Outils utiles
- **OLEView** : Pour explorer les composants COM installés
- **RegEdit** : Pour voir les enregistrements COM dans le registre
- **Type Library Viewer** : Dans Lazarus pour importer des bibliothèques

### Exemples de projets
- Automatisation Office complète
- Intégration avec des logiciels tiers
- Création de plugins pour applications existantes
- Serveurs COM pour exposer des fonctionnalités

## Conclusion

COM/ActiveX et OLE sont des technologies puissantes pour l'intégration d'applications sous Windows. Avec FreePascal/Lazarus, vous pouvez :
- Automatiser des applications Microsoft Office
- Créer vos propres composants réutilisables
- Intégrer des fonctionnalités d'autres applications
- Construire des solutions d'entreprise intégrées

La maîtrise de ces technologies ouvre de nombreuses possibilités pour créer des applications Windows professionnelles et bien intégrées à l'écosystème Microsoft.

⏭️ [Windows Shell et intégration Explorer](/06-specificites-windows/05-windows-shell-integration-explorer.md)
