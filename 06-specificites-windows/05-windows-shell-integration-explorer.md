🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Windows Shell et intégration Explorer avec FreePascal/Lazarus

## Introduction : Qu'est-ce que Windows Shell ?

### Comprendre Windows Shell

Le **Windows Shell** est l'interface utilisateur de Windows qui gère tout ce que vous voyez et utilisez au quotidien :
- Le bureau (Desktop)
- La barre des tâches
- Le menu Démarrer
- L'Explorateur de fichiers
- Les menus contextuels (clic droit)
- Les icônes et leurs associations
- Les notifications système

Quand vous développez une application Windows, l'intégration avec le Shell permet de rendre votre programme plus naturel et intuitif pour les utilisateurs.

### Pourquoi intégrer votre application au Shell ?

- **Expérience utilisateur native** : Votre application se comporte comme une partie intégrante de Windows
- **Productivité** : Les utilisateurs peuvent accéder à vos fonctionnalités depuis l'Explorer
- **Professionnalisme** : Une application bien intégrée paraît plus aboutie
- **Accessibilité** : Utilisation des fonctionnalités Windows standards

## Configuration de base

### Unités nécessaires

Pour travailler avec Windows Shell dans FreePascal/Lazarus :

```pascal
uses
  Windows,      // API Windows de base
  ShlObj,       // Objets Shell et interfaces
  ShellAPI,     // Fonctions Shell de haut niveau
  ActiveX,      // Pour COM (Shell utilise COM)
  ComObj,       // Objets COM
  Registry;     // Pour les associations de fichiers
```

### Initialisation COM pour Shell

Beaucoup de fonctionnalités Shell utilisent COM :

```pascal
program MonProgrammeShell;

uses
  Windows, ShlObj, ActiveX;

begin
  // Initialiser COM (requis pour certaines fonctions Shell)
  CoInitialize(nil);
  try
    // Votre code Shell ici

  finally
    CoUninitialize;
  end;
end;
```

## Opérations de base avec les fichiers et dossiers

### Ouvrir des fichiers et dossiers avec l'application par défaut

```pascal
uses
  ShellAPI, Windows;

procedure OuvrirFichier(const CheminFichier: string);  
begin
  // Ouvre le fichier avec l'application associée
  ShellExecute(0, 'open', PChar(CheminFichier), nil, nil, SW_SHOWNORMAL);
end;

procedure OuvrirDossier(const CheminDossier: string);  
begin
  // Ouvre le dossier dans l'Explorateur Windows
  ShellExecute(0, 'explore', PChar(CheminDossier), nil, nil, SW_SHOWNORMAL);
end;

procedure OuvrirURL(const URL: string);  
begin
  // Ouvre l'URL dans le navigateur par défaut
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

// Exemples d'utilisation
begin
  OuvrirFichier('C:\MesDocuments\rapport.pdf');
  OuvrirDossier('C:\Program Files');
  OuvrirURL('https://www.example.com');
end;
```

### Exécuter des programmes avec paramètres

```pascal
procedure LancerProgramme(const Programme, Parametres, DossierTravail: string);  
var
  InfoExec: TShellExecuteInfo;
begin
  FillChar(InfoExec, SizeOf(InfoExec), 0);
  InfoExec.cbSize := SizeOf(TShellExecuteInfo);
  InfoExec.fMask := SEE_MASK_NOCLOSEPROCESS;
  InfoExec.Wnd := 0;
  InfoExec.lpVerb := 'open';
  InfoExec.lpFile := PChar(Programme);
  InfoExec.lpParameters := PChar(Parametres);
  InfoExec.lpDirectory := PChar(DossierTravail);
  InfoExec.nShow := SW_SHOWNORMAL;

  if ShellExecuteEx(@InfoExec) then
  begin
    // Attendre la fin du programme si nécessaire
    WaitForSingleObject(InfoExec.hProcess, INFINITE);
    CloseHandle(InfoExec.hProcess);
  end;
end;

// Exemple : Lancer Notepad avec un fichier
begin
  LancerProgramme('notepad.exe', 'C:\monfichier.txt', 'C:\');
end;
```

## Menus contextuels de l'Explorateur

### Ajouter une entrée au menu contextuel pour tous les fichiers

```pascal
uses
  Registry, Windows;

procedure AjouterMenuContextuel(const NomCommande, Libelle, CheminProgramme: string);  
var
  Reg: TRegistry;
  Commande: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Créer l'entrée pour tous les fichiers
    if Reg.OpenKey('\*\shell\' + NomCommande, True) then
    begin
      Reg.WriteString('', Libelle);

      // Optionnel : ajouter une icône
      Reg.WriteString('Icon', CheminProgramme + ',0');

      Reg.CloseKey;
    end;

    // Créer la commande
    if Reg.OpenKey('\*\shell\' + NomCommande + '\command', True) then
    begin
      Commande := '"' + CheminProgramme + '" "%1"';
      Reg.WriteString('', Commande);
      Reg.CloseKey;
    end;

    WriteLn('Menu contextuel ajouté avec succès');

  finally
    Reg.Free;
  end;
end;

// Exemple d'utilisation
begin
  AjouterMenuContextuel(
    'MonApplication',
    'Ouvrir avec Mon Application',
    'C:\MonApp\MonApp.exe'
  );
end;
```

### Ajouter un menu contextuel pour des types de fichiers spécifiques

```pascal
procedure AjouterMenuPourExtension(const Extension, NomCommande, Libelle, Programme: string);  
var
  Reg: TRegistry;
  TypeFichier: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Trouver le type de fichier associé à l'extension
    if Reg.OpenKeyReadOnly('\' + Extension) then
    begin
      TypeFichier := Reg.ReadString('');
      Reg.CloseKey;

      if TypeFichier <> '' then
      begin
        // Ajouter le menu au type de fichier
        if Reg.OpenKey('\' + TypeFichier + '\shell\' + NomCommande, True) then
        begin
          Reg.WriteString('', Libelle);
          Reg.CloseKey;
        end;

        if Reg.OpenKey('\' + TypeFichier + '\shell\' + NomCommande + '\command', True) then
        begin
          Reg.WriteString('', '"' + Programme + '" "%1"');
          Reg.CloseKey;
        end;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

// Exemple : Ajouter un menu pour les fichiers .txt
begin
  AjouterMenuPourExtension(
    '.txt',
    'MonEditeur',
    'Éditer avec Mon Éditeur',
    'C:\MonEditeur\editeur.exe'
  );
end;
```

## Associations de fichiers

### Créer une nouvelle association de fichier

```pascal
procedure AssocierExtension(const Extension, Description, Programme, Icone: string);  
var
  Reg: TRegistry;
  TypeFichier: string;
begin
  TypeFichier := Copy(Extension, 2, Length(Extension)) + 'file';

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Créer l'extension
    if Reg.OpenKey('\' + Extension, True) then
    begin
      Reg.WriteString('', TypeFichier);
      Reg.CloseKey;
    end;

    // Créer le type de fichier
    if Reg.OpenKey('\' + TypeFichier, True) then
    begin
      Reg.WriteString('', Description);
      Reg.CloseKey;
    end;

    // Définir l'icône
    if Reg.OpenKey('\' + TypeFichier + '\DefaultIcon', True) then
    begin
      Reg.WriteString('', Icone);
      Reg.CloseKey;
    end;

    // Définir l'action par défaut
    if Reg.OpenKey('\' + TypeFichier + '\shell\open\command', True) then
    begin
      Reg.WriteString('', '"' + Programme + '" "%1"');
      Reg.CloseKey;
    end;

    // Notifier Windows du changement
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

    WriteLn('Association créée pour ', Extension);

  finally
    Reg.Free;
  end;
end;

// Exemple d'utilisation
begin
  AssocierExtension(
    '.monfichier',
    'Fichier de Mon Application',
    'C:\MonApp\MonApp.exe',
    'C:\MonApp\MonApp.exe,0'  // Icône index 0 dans l'exe
  );
end;
```

## Intégration dans la barre des tâches

### Icône dans la zone de notification (System Tray)

```pascal
unit SystemTrayUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, ShellAPI, Menus;

type
  TSystemTrayApp = class(TForm)
    PopupMenu1: TPopupMenu;
    MenuItemShow: TMenuItem;
    MenuItemExit: TMenuItem;
  private
    FIconData: TNotifyIconData;
    procedure AjouterIconeTray;
    procedure RetirerIconeTray;
    procedure WMTrayIcon(var Message: TMessage); message WM_USER + 1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

constructor TSystemTrayApp.Create(AOwner: TComponent);  
begin
  inherited;
  AjouterIconeTray;
end;

destructor TSystemTrayApp.Destroy;  
begin
  RetirerIconeTray;
  inherited;
end;

procedure TSystemTrayApp.AjouterIconeTray;  
begin
  FillChar(FIconData, SizeOf(FIconData), 0);
  FIconData.cbSize := SizeOf(TNotifyIconData);
  FIconData.Wnd := Handle;
  FIconData.uID := 1;
  FIconData.uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
  FIconData.uCallbackMessage := WM_USER + 1;
  FIconData.hIcon := Application.Icon.Handle;
  StrCopy(FIconData.szTip, 'Mon Application');

  Shell_NotifyIcon(NIM_ADD, @FIconData);
end;

procedure TSystemTrayApp.RetirerIconeTray;  
begin
  Shell_NotifyIcon(NIM_DELETE, @FIconData);
end;

procedure TSystemTrayApp.WMTrayIcon(var Message: TMessage);  
var
  Point: TPoint;
begin
  case Message.LParam of
    WM_LBUTTONDBLCLK:
      begin
        // Double-clic : afficher la fenêtre
        Show;
        WindowState := wsNormal;
        Application.BringToFront;
      end;
    WM_RBUTTONUP:
      begin
        // Clic droit : afficher le menu
        GetCursorPos(Point);
        PopupMenu1.Popup(Point.X, Point.Y);
      end;
  end;
end;

end.
```

### Notifications Windows (Toast Notifications)

```pascal
procedure AfficherNotification(const Titre, Message: string; TypeIcone: Integer = NIIF_INFO);  
var
  IconData: TNotifyIconData;
begin
  FillChar(IconData, SizeOf(IconData), 0);
  IconData.cbSize := SizeOf(TNotifyIconData);
  IconData.Wnd := Application.Handle;
  IconData.uID := 1;
  IconData.uFlags := NIF_INFO;
  IconData.dwInfoFlags := TypeIcone;
  StrCopy(IconData.szInfoTitle, PChar(Titre));
  StrCopy(IconData.szInfo, PChar(Message));

  Shell_NotifyIcon(NIM_MODIFY, @IconData);
end;

// Types d'icônes disponibles
begin
  AfficherNotification('Information', 'Opération terminée', NIIF_INFO);
  AfficherNotification('Attention', 'Vérifiez les paramètres', NIIF_WARNING);
  AfficherNotification('Erreur', 'Une erreur est survenue', NIIF_ERROR);
end;
```

## Dossiers spéciaux Windows

### Obtenir les chemins des dossiers système

```pascal
uses
  ShlObj, ActiveX;

function ObtenirDossierSpecial(CSIDL: Integer): string;  
var
  Chemin: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL, 0, SHGFP_TYPE_CURRENT, Chemin) = S_OK then
    Result := Chemin
  else
    Result := '';
end;

// Exemples d'utilisation
var
  Bureau, Documents, AppData, ProgramFiles: string;
begin
  Bureau := ObtenirDossierSpecial(CSIDL_DESKTOP);
  WriteLn('Bureau : ', Bureau);

  Documents := ObtenirDossierSpecial(CSIDL_PERSONAL);
  WriteLn('Mes Documents : ', Documents);

  AppData := ObtenirDossierSpecial(CSIDL_APPDATA);
  WriteLn('AppData : ', AppData);

  ProgramFiles := ObtenirDossierSpecial(CSIDL_PROGRAM_FILES);
  WriteLn('Program Files : ', ProgramFiles);
end;
```

### Liste des CSIDL les plus utiles

```pascal
const
  // Dossiers utilisateur
  CSIDL_DESKTOP            = $0000;  // Bureau
  CSIDL_PERSONAL          = $0005;  // Mes Documents
  CSIDL_FAVORITES         = $0006;  // Favoris
  CSIDL_STARTUP           = $0007;  // Démarrage
  CSIDL_RECENT            = $0008;  // Documents récents
  CSIDL_SENDTO            = $0009;  // Envoyer vers
  CSIDL_STARTMENU         = $000B;  // Menu Démarrer
  CSIDL_MYDOCUMENTS       = $000C;  // Mes Documents (alias)
  CSIDL_MYMUSIC           = $000D;  // Ma Musique
  CSIDL_MYVIDEO           = $000E;  // Mes Vidéos
  CSIDL_DESKTOP_DIRECTORY = $0010;  // Dossier Bureau

  // Dossiers système
  CSIDL_DRIVES            = $0011;  // Poste de travail
  CSIDL_NETWORK           = $0012;  // Réseau
  CSIDL_FONTS             = $0014;  // Polices
  CSIDL_TEMPLATES         = $0015;  // Modèles
  CSIDL_COMMON_STARTMENU  = $0016;  // Menu Démarrer commun
  CSIDL_COMMON_PROGRAMS   = $0017;  // Programmes communs
  CSIDL_COMMON_STARTUP    = $0018;  // Démarrage commun
  CSIDL_COMMON_DESKTOP    = $0019;  // Bureau commun
  CSIDL_APPDATA           = $001A;  // Application Data
  CSIDL_LOCAL_APPDATA     = $001C;  // Local AppData
  CSIDL_PROGRAM_FILES     = $0026;  // Program Files
  CSIDL_MYPICTURES        = $0027;  // Mes Images
  CSIDL_COMMON_DOCUMENTS  = $002E;  // Documents partagés
```

## Raccourcis Windows (.lnk)

### Créer un raccourci

```pascal
uses
  ShlObj, ActiveX, ComObj;

procedure CreerRaccourci(const CheminRaccourci, CheminCible, Description,
                        DossierTravail, Parametres: string; Icone: string = '');
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  WideCheminRaccourci: WideString;
begin
  CoInitialize(nil);
  try
    // Créer l'objet ShellLink
    ShellLink := CreateComObject(CLSID_ShellLink) as IShellLink;

    // Configurer le raccourci
    ShellLink.SetPath(PChar(CheminCible));
    ShellLink.SetDescription(PChar(Description));
    ShellLink.SetWorkingDirectory(PChar(DossierTravail));

    if Parametres <> '' then
      ShellLink.SetArguments(PChar(Parametres));

    if Icone <> '' then
      ShellLink.SetIconLocation(PChar(Icone), 0);

    // Sauvegarder le raccourci
    PersistFile := ShellLink as IPersistFile;
    WideCheminRaccourci := CheminRaccourci;
    PersistFile.Save(PWideChar(WideCheminRaccourci), True);

    WriteLn('Raccourci créé : ', CheminRaccourci);

  finally
    CoUninitialize;
  end;
end;

// Exemple : Créer un raccourci sur le bureau
var
  Bureau, CheminRaccourci: string;
begin
  Bureau := ObtenirDossierSpecial(CSIDL_DESKTOP);
  CheminRaccourci := Bureau + '\Mon Application.lnk';

  CreerRaccourci(
    CheminRaccourci,
    'C:\MonApp\MonApp.exe',
    'Raccourci vers Mon Application',
    'C:\MonApp',
    '--option1 --option2',
    'C:\MonApp\MonApp.exe'
  );
end;
```

### Lire les propriétés d'un raccourci

```pascal
function LireRaccourci(const CheminRaccourci: string): string;  
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  Cible: array[0..MAX_PATH] of Char;
  Donnees: TWin32FindData;
begin
  Result := '';
  CoInitialize(nil);
  try
    ShellLink := CreateComObject(CLSID_ShellLink) as IShellLink;
    PersistFile := ShellLink as IPersistFile;

    if PersistFile.Load(PWideChar(WideString(CheminRaccourci)), STGM_READ) = S_OK then
    begin
      ShellLink.GetPath(Cible, MAX_PATH, Donnees, SLGP_UNCPRIORITY);
      Result := Cible;
    end;
  finally
    CoUninitialize;
  end;
end;
```

## Boîtes de dialogue Shell

### Sélectionner un dossier

```pascal
uses
  ShlObj;

function SelectionnerDossier(const Titre: string; DossierInitial: string = ''): string;  
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  Chemin: array[0..MAX_PATH] of Char;
begin
  Result := '';

  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  BrowseInfo.hwndOwner := Application.Handle;
  BrowseInfo.lpszTitle := PChar(Titre);
  BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE;

  if DossierInitial <> '' then
    BrowseInfo.lParam := LPARAM(PChar(DossierInitial));

  ItemIDList := SHBrowseForFolder(BrowseInfo);
  if ItemIDList <> nil then
  begin
    if SHGetPathFromIDList(ItemIDList, Chemin) then
      Result := Chemin;
    CoTaskMemFree(ItemIDList);
  end;
end;

// Utilisation
var
  DossierChoisi: string;
begin
  DossierChoisi := SelectionnerDossier('Sélectionnez un dossier de destination');
  if DossierChoisi <> '' then
    ShowMessage('Dossier sélectionné : ' + DossierChoisi);
end;
```

### Boîte de dialogue "Exécuter"

```pascal
procedure AfficherDialogueExecuter;  
begin
  // Affiche la boîte de dialogue Exécuter de Windows
  ShellExecute(0, 'open', 'rundll32.exe',
    'shell32.dll,#61', nil, SW_SHOWNORMAL);
end;
```

## Opérations de fichiers avec Shell

### Copier, déplacer, supprimer avec l'interface Shell

```pascal
uses
  ShellAPI;

function OperationFichierShell(Operation: UINT; const Source, Destination: string;
                               Flags: FILEOP_FLAGS = 0): Boolean;
var
  FileOp: TSHFileOpStruct;
begin
  FillChar(FileOp, SizeOf(FileOp), 0);
  FileOp.Wnd := Application.Handle;
  FileOp.wFunc := Operation;
  FileOp.pFrom := PChar(Source + #0);
  FileOp.pTo := PChar(Destination + #0);
  FileOp.fFlags := Flags;

  Result := SHFileOperation(FileOp) = 0;
end;

// Copier un fichier avec progression
procedure CopierAvecProgression(const Source, Dest: string);  
begin
  OperationFichierShell(FO_COPY, Source, Dest, FOF_SIMPLEPROGRESS);
end;

// Déplacer un fichier
procedure DeplacerFichier(const Source, Dest: string);  
begin
  OperationFichierShell(FO_MOVE, Source, Dest, 0);
end;

// Supprimer vers la Corbeille
procedure SupprimerVersCorbeille(const Fichier: string);  
begin
  OperationFichierShell(FO_DELETE, Fichier, '', FOF_ALLOWUNDO);
end;

// Renommer un fichier
procedure RenommerFichier(const Ancien, Nouveau: string);  
begin
  OperationFichierShell(FO_RENAME, Ancien, Nouveau, 0);
end;
```

### Obtenir les informations d'un fichier

```pascal
uses
  ShellAPI, Graphics;

type
  TInfoFichier = record
    TypeFichier: string;
    Icone: TIcon;
    IndexIcone: Integer;
  end;

function ObtenirInfoFichier(const CheminFichier: string): TInfoFichier;  
var
  SHFileInfo: TSHFileInfo;
begin
  // Obtenir les informations du fichier
  SHGetFileInfo(PChar(CheminFichier), 0, SHFileInfo, SizeOf(SHFileInfo),
    SHGFI_TYPENAME or SHGFI_ICON or SHGFI_LARGEICON);

  Result.TypeFichier := SHFileInfo.szTypeName;
  Result.IndexIcone := SHFileInfo.iIcon;

  // Créer l'icône
  Result.Icone := TIcon.Create;
  Result.Icone.Handle := SHFileInfo.hIcon;
end;

// Utilisation
var
  Info: TInfoFichier;
begin
  Info := ObtenirInfoFichier('C:\monfichier.pdf');
  WriteLn('Type : ', Info.TypeFichier);
  // Utiliser Info.Icone pour l'affichage
  Info.Icone.Free;
end;
```

## Menu Démarrer et programmes

### Ajouter un programme au menu Démarrer

```pascal
procedure AjouterAuMenuDemarrer(const NomProgramme, CheminExe: string);  
var
  DossierMenuDemarrer, CheminRaccourci: string;
begin
  // Obtenir le dossier du menu Démarrer
  DossierMenuDemarrer := ObtenirDossierSpecial(CSIDL_PROGRAMS);

  // Créer le dossier du programme si nécessaire
  ForceDirectories(DossierMenuDemarrer + '\' + NomProgramme);

  // Créer le raccourci
  CheminRaccourci := DossierMenuDemarrer + '\' + NomProgramme +
                    '\' + NomProgramme + '.lnk';

  CreerRaccourci(
    CheminRaccourci,
    CheminExe,
    NomProgramme,
    ExtractFilePath(CheminExe),
    '',
    CheminExe
  );

  WriteLn('Programme ajouté au menu Démarrer');
end;
```

### Ajouter au démarrage automatique

```pascal
procedure AjouterAuDemarrage(const NomProgramme, CheminExe: string);  
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run', True) then
    begin
      Reg.WriteString(NomProgramme, CheminExe);
      Reg.CloseKey;
      WriteLn('Programme ajouté au démarrage automatique');
    end;
  finally
    Reg.Free;
  end;
end;
```

## Propriétés et dialogues système

### Afficher les propriétés d'un fichier

```pascal
procedure AfficherProprietesFichier(const CheminFichier: string);  
var
  SEI: TShellExecuteInfo;
begin
  FillChar(SEI, SizeOf(SEI), 0);
  SEI.cbSize := SizeOf(SEI);
  SEI.fMask := SEE_MASK_INVOKEIDLIST;
  SEI.lpVerb := 'properties';
  SEI.lpFile := PChar(CheminFichier);
  SEI.nShow := SW_SHOW;

  ShellExecuteEx(@SEI);
end;
```

### Ouvrir les paramètres système

```pascal
procedure OuvrirParametresSysteme(const Page: string = '');  
begin
  if Page = '' then
    ShellExecute(0, 'open', 'control', nil, nil, SW_SHOWNORMAL)
  else
    ShellExecute(0, 'open', 'control', PChar(Page), nil, SW_SHOWNORMAL);
end;

// Exemples
begin
  OuvrirParametresSysteme;  // Panneau de configuration
  OuvrirParametresSysteme('sysdm.cpl');  // Propriétés système
  OuvrirParametresSysteme('appwiz.cpl');  // Programmes et fonctionnalités
  OuvrirParametresSysteme('desk.cpl');  // Paramètres d'affichage
end;
```

## Drag and Drop (Glisser-Déposer)

### Accepter des fichiers glissés dans votre application

```pascal
unit DragDropUnit;

interface

uses
  Windows, Messages, Forms, Classes, ShellAPI;

type
  TFormDragDrop = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  end;

implementation

procedure TFormDragDrop.FormCreate(Sender: TObject);  
begin
  // Activer l'acceptation des fichiers
  DragAcceptFiles(Handle, True);
end;

procedure TFormDragDrop.FormDestroy(Sender: TObject);  
begin
  // Désactiver l'acceptation des fichiers
  DragAcceptFiles(Handle, False);
end;

procedure TFormDragDrop.WMDropFiles(var Msg: TWMDropFiles);  
var
  NombreFichiers, i: Integer;
  NomFichier: array[0..MAX_PATH] of Char;
  ListeFichiers: TStringList;
begin
  ListeFichiers := TStringList.Create;
  try
    // Obtenir le nombre de fichiers
    NombreFichiers := DragQueryFile(Msg.Drop, UINT(-1), nil, 0);

    // Récupérer chaque fichier
    for i := 0 to NombreFichiers - 1 do
    begin
      DragQueryFile(Msg.Drop, i, NomFichier, SizeOf(NomFichier));
      ListeFichiers.Add(NomFichier);
    end;

    // Traiter les fichiers
    for i := 0 to ListeFichiers.Count - 1 do
      WriteLn('Fichier reçu : ', ListeFichiers[i]);

  finally
    ListeFichiers.Free;
    DragFinish(Msg.Drop);
  end;
end;

end.
```

## Recherche Windows

### Lancer une recherche Windows

```pascal
procedure LancerRecherche(const DossierDepart: string = '');  
var
  Params: string;
begin
  if DossierDepart <> '' then
    Params := 'search-ms:displayname=Résultats&crumb=location:' + DossierDepart
  else
    Params := 'search-ms:';

  ShellExecute(0, 'open', PChar(Params), nil, nil, SW_SHOWNORMAL);
end;

// Recherche avec des critères spécifiques
procedure RechercherAvecCriteres(const Terme, DossierDepart: string);  
var
  RequeteRecherche: string;
begin
  RequeteRecherche := Format('search-ms:query=%s&crumb=location:%s',
    [Terme, DossierDepart]);
  ShellExecute(0, 'open', PChar(RequeteRecherche), nil, nil, SW_SHOWNORMAL);
end;

// Exemples d'utilisation
begin
  // Ouvrir la recherche Windows
  LancerRecherche;

  // Rechercher dans un dossier spécifique
  LancerRecherche('C:\MesDocuments');

  // Rechercher un terme spécifique
  RechercherAvecCriteres('*.pdf', 'C:\MesDocuments');
end;
```

### Utiliser l'indexation Windows Search

```pascal
uses
  ComObj, ActiveX, Variants;

procedure RechercherDansIndex(const Requete: string);  
var
  Connection, Recordset: Variant;
  SQL: string;
begin
  CoInitialize(nil);
  try
    // Créer la connexion à Windows Search
    Connection := CreateOleObject('ADODB.Connection');
    Connection.Open('Provider=Search.CollatorDSO;Extended Properties="Application=Windows"');

    // Construire la requête SQL
    SQL := 'SELECT System.ItemName, System.ItemPathDisplay, System.DateModified ' +
           'FROM SystemIndex ' +
           'WHERE CONTAINS(''"%s"'')';
    SQL := Format(SQL, [Requete]);

    // Exécuter la requête
    Recordset := Connection.Execute(SQL);

    // Parcourir les résultats
    while not Recordset.EOF do
    begin
      WriteLn('Fichier : ', Recordset.Fields['System.ItemName'].Value);
      WriteLn('Chemin : ', Recordset.Fields['System.ItemPathDisplay'].Value);
      WriteLn('Modifié : ', Recordset.Fields['System.DateModified'].Value);
      WriteLn('---');
      Recordset.MoveNext;
    end;

    Recordset.Close;
    Connection.Close;
  finally
    CoUninitialize;
  end;
end;
```

## Miniatures et prévisualisation

### Obtenir la miniature d'un fichier

```pascal
uses
  ShlObj, ActiveX, Graphics;

function ObtenirMiniature(const CheminFichier: string; Largeur, Hauteur: Integer): TBitmap;  
var
  ShellFolder: IShellFolder;
  ExtractImage: IExtractImage;
  ItemIDList: PItemIDList;
  Taille: TSize;
  Flags: DWORD;
  hBitmap: HBITMAP;
  CheminImage: array[0..MAX_PATH] of WideChar;
  Priorite: DWORD;
begin
  Result := nil;
  CoInitialize(nil);
  try
    // Obtenir le dossier parent et l'élément
    if SHGetDesktopFolder(ShellFolder) = S_OK then
    begin
      if ShellFolder.ParseDisplayName(0, nil, PWideChar(WideString(CheminFichier)),
         ULONG(0), ItemIDList, ULONG(0)) = S_OK then
      begin
        // Obtenir l'interface IExtractImage
        if ShellFolder.GetUIObjectOf(0, 1, ItemIDList, IExtractImage,
           nil, ExtractImage) = S_OK then
        begin
          // Configurer la taille
          Taille.cx := Largeur;
          Taille.cy := Hauteur;
          Flags := IEIFLAG_SCREEN or IEIFLAG_QUALITY;

          // Obtenir le chemin de l'image
          if ExtractImage.GetLocation(CheminImage, Length(CheminImage),
             Priorite, Taille, 32, Flags) = S_OK then
          begin
            // Extraire l'image
            if ExtractImage.Extract(hBitmap) = S_OK then
            begin
              Result := TBitmap.Create;
              Result.Handle := hBitmap;
            end;
          end;
        end;
        CoTaskMemFree(ItemIDList);
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

// Utilisation
var
  Miniature: TBitmap;
begin
  Miniature := ObtenirMiniature('C:\MonImage.jpg', 128, 128);
  if Assigned(Miniature) then
  begin
    // Utiliser la miniature (par exemple, l'afficher dans un TImage)
    Miniature.Free;
  end;
end;
```

### Gestionnaire de prévisualisation

```pascal
procedure AfficherPrevisualisation(const CheminFichier: string);  
var
  SEI: TShellExecuteInfo;
begin
  FillChar(SEI, SizeOf(SEI), 0);
  SEI.cbSize := SizeOf(SEI);
  SEI.fMask := SEE_MASK_INVOKEIDLIST;
  SEI.lpVerb := 'preview';
  SEI.lpFile := PChar(CheminFichier);
  SEI.nShow := SW_SHOW;

  if not ShellExecuteEx(@SEI) then
  begin
    // Si la prévisualisation n'est pas disponible, ouvrir normalement
    ShellExecute(0, 'open', PChar(CheminFichier), nil, nil, SW_SHOWNORMAL);
  end;
end;
```

## Jump Lists (Listes de raccourcis)

### Créer une Jump List pour Windows 7+

```pascal
uses
  ShlObj, PropSys, ObjectArray, ActiveX, ComObj;

procedure CreerJumpList;  
var
  DestList: ICustomDestinationList;
  RemovedItems: IObjectArray;
  Tasks: IObjectCollection;
  ShellLink: IShellLink;
  PropertyStore: IPropertyStore;
  PropVariant: TPropVariant;
begin
  CoInitialize(nil);
  try
    // Créer la liste de destinations
    DestList := CreateComObject(CLSID_DestinationList) as ICustomDestinationList;

    // Commencer une nouvelle session
    DestList.BeginList(UINT(10), IID_IObjectArray, RemovedItems);

    // Créer la collection de tâches
    Tasks := CreateComObject(CLSID_EnumerableObjectCollection) as IObjectCollection;

    // Créer une tâche (raccourci)
    ShellLink := CreateComObject(CLSID_ShellLink) as IShellLink;
    ShellLink.SetPath('C:\MonApp\MonApp.exe');
    ShellLink.SetArguments('--nouvelle-fenetre');
    ShellLink.SetIconLocation('C:\MonApp\MonApp.exe', 0);

    // Définir le titre de la tâche
    PropertyStore := ShellLink as IPropertyStore;
    InitPropVariantFromString('Nouvelle fenêtre', PropVariant);
    PropertyStore.SetValue(PKEY_Title, PropVariant);
    PropertyStore.Commit;

    // Ajouter la tâche à la collection
    Tasks.AddObject(ShellLink);

    // Ajouter les tâches à la Jump List
    DestList.AddUserTasks(Tasks);

    // Valider les changements
    DestList.CommitList;

    WriteLn('Jump List créée avec succès');
  finally
    CoUninitialize;
  end;
end;
```

## Bibliothèques Windows (Libraries)

### Créer et gérer une bibliothèque Windows

```pascal
uses
  ShlObj, KnownFolders, ShObjIdl, ActiveX, ComObj;

procedure CreerBibliotheque(const NomBibliotheque: string);  
var
  LibraryManager: IShellLibrary;
  SavedTo: IShellItem;
  hr: HRESULT;
begin
  CoInitialize(nil);
  try
    // Créer le gestionnaire de bibliothèque
    hr := CoCreateInstance(CLSID_ShellLibrary, nil, CLSCTX_INPROC_SERVER,
                           IID_IShellLibrary, LibraryManager);
    if SUCCEEDED(hr) then
    begin
      // Sauvegarder la bibliothèque
      hr := LibraryManager.SaveInKnownFolder(FOLDERID_Libraries,
                                             PWideChar(WideString(NomBibliotheque)),
                                             LSF_OVERWRITE_IF_EXISTS, SavedTo);
      if SUCCEEDED(hr) then
        WriteLn('Bibliothèque créée : ', NomBibliotheque);
    end;
  finally
    CoUninitialize;
  end;
end;

// Ajouter un dossier à une bibliothèque
procedure AjouterDossierBibliotheque(const NomBibliotheque, CheminDossier: string);  
var
  LibraryManager: IShellLibrary;
  FolderItem: IShellItem;
  hr: HRESULT;
begin
  CoInitialize(nil);
  try
    hr := CoCreateInstance(CLSID_ShellLibrary, nil, CLSCTX_INPROC_SERVER,
                          IID_IShellLibrary, LibraryManager);
    if SUCCEEDED(hr) then
    begin
      // Charger la bibliothèque existante
      hr := SHCreateItemFromParsingName(PWideChar(WideString(NomBibliotheque)),
                                       nil, IID_IShellItem, FolderItem);
      if SUCCEEDED(hr) then
      begin
        LibraryManager.LoadLibraryFromItem(FolderItem, STGM_READWRITE);

        // Créer l'élément pour le dossier
        hr := SHCreateItemFromParsingName(PWideChar(WideString(CheminDossier)),
                                         nil, IID_IShellItem, FolderItem);
        if SUCCEEDED(hr) then
        begin
          LibraryManager.AddFolder(FolderItem);
          LibraryManager.Commit;
          WriteLn('Dossier ajouté à la bibliothèque');
        end;
      end;
    end;
  finally
    CoUninitialize;
  end;
end;
```

## Gestion avancée du presse-papiers

### Formats de presse-papiers personnalisés

```pascal
uses
  Windows, Clipbrd;

var
  CF_MONFORMAT: UINT;

procedure InitialiserFormatsPersonnalises;  
begin
  // Enregistrer un format personnalisé
  CF_MONFORMAT := RegisterClipboardFormat('MonApplication.DonneesSpeciales');
end;

procedure CopierDonneesPersonnalisees(const Donnees: string);  
var
  hMem: HGLOBAL;
  pMem: Pointer;
begin
  OpenClipboard(0);
  try
    EmptyClipboard;

    // Allouer la mémoire
    hMem := GlobalAlloc(GMEM_MOVEABLE, Length(Donnees) + 1);
    pMem := GlobalLock(hMem);
    try
      // Copier les données
      Move(PChar(Donnees)^, pMem^, Length(Donnees) + 1);
    finally
      GlobalUnlock(hMem);
    end;

    // Placer dans le presse-papiers
    SetClipboardData(CF_MONFORMAT, hMem);
  finally
    CloseClipboard;
  end;
end;

function CollerDonneesPersonnalisees: string;  
var
  hMem: HGLOBAL;
  pMem: Pointer;
begin
  Result := '';
  OpenClipboard(0);
  try
    if IsClipboardFormatAvailable(CF_MONFORMAT) then
    begin
      hMem := GetClipboardData(CF_MONFORMAT);
      if hMem <> 0 then
      begin
        pMem := GlobalLock(hMem);
        try
          Result := PChar(pMem);
        finally
          GlobalUnlock(hMem);
        end;
      end;
    end;
  finally
    CloseClipboard;
  end;
end;
```

### Surveillance du presse-papiers

```pascal
unit ClipboardMonitor;

interface

uses
  Windows, Messages, Forms, Classes;

type
  TClipboardMonitor = class(TForm)
  private
    FNextClipboardViewer: HWND;
    procedure WMDrawClipboard(var Message: TWMDrawClipboard); message WM_DRAWCLIPBOARD;
    procedure WMChangeCBChain(var Message: TWMChangeCBChain); message WM_CHANGECBCHAIN;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    procedure OnClipboardChange; virtual;
  end;

implementation

procedure TClipboardMonitor.CreateWnd;  
begin
  inherited;
  // S'enregistrer comme observateur du presse-papiers
  FNextClipboardViewer := SetClipboardViewer(Handle);
end;

procedure TClipboardMonitor.DestroyWnd;  
begin
  // Se retirer de la chaîne d'observateurs
  ChangeClipboardChain(Handle, FNextClipboardViewer);
  inherited;
end;

procedure TClipboardMonitor.WMDrawClipboard(var Message: TWMDrawClipboard);  
begin
  // Le presse-papiers a changé
  OnClipboardChange;

  // Passer le message au suivant
  if FNextClipboardViewer <> 0 then
    SendMessage(FNextClipboardViewer, WM_DRAWCLIPBOARD, 0, 0);
end;

procedure TClipboardMonitor.WMChangeCBChain(var Message: TWMChangeCBChain);  
begin
  // Mettre à jour la chaîne si nécessaire
  if Message.Remove = FNextClipboardViewer then
    FNextClipboardViewer := Message.Next
  else if FNextClipboardViewer <> 0 then
    SendMessage(FNextClipboardViewer, WM_CHANGECBCHAIN, Message.Remove, Message.Next);
end;

procedure TClipboardMonitor.OnClipboardChange;  
begin
  WriteLn('Le presse-papiers a changé !');
  // Traiter le changement ici
end;

end.
```

## Intégration avec Windows Explorer avancée

### Extension de colonnes dans Explorer

```pascal
type
  IColumnProvider = interface(IUnknown)
    ['{E8025004-1C42-11D2-BE2C-00A0C9A83DA1}']
    function Initialize(const psci: LPCSHCOLUMNINIT): HRESULT; stdcall;
    function GetColumnInfo(dwIndex: DWORD; out psci: SHCOLUMNINFO): HRESULT; stdcall;
    function GetItemData(const pscid: LPCSHCOLUMNID; const pscd: LPCSHCOLUMNDATA;
                         out pvarData: VARIANT): HRESULT; stdcall;
  end;

  TMonColumnProvider = class(TComObject, IColumnProvider)
  public
    function Initialize(const psci: LPCSHCOLUMNINIT): HRESULT; stdcall;
    function GetColumnInfo(dwIndex: DWORD; out psci: SHCOLUMNINFO): HRESULT; stdcall;
    function GetItemData(const pscid: LPCSHCOLUMNID; const pscd: LPCSHCOLUMNDATA;
                         out pvarData: VARIANT): HRESULT; stdcall;
  end;

function TMonColumnProvider.GetColumnInfo(dwIndex: DWORD; out psci: SHCOLUMNINFO): HRESULT;  
begin
  if dwIndex = 0 then
  begin
    psci.scid.fmtid := StringToGUID('{12345678-1234-1234-1234-123456789ABC}');
    psci.scid.pid := 0;
    psci.vt := VT_BSTR;
    psci.fmt := LVCFMT_LEFT;
    psci.cChars := 30;
    psci.csFlags := SHCOLSTATE_TYPE_STR or SHCOLSTATE_ONBYDEFAULT;
    StrCopy(psci.wszTitle, 'Ma Colonne');
    StrCopy(psci.wszDescription, 'Description de ma colonne');
    Result := S_OK;
  end
  else
    Result := S_FALSE;
end;
```

### Namespace Extension (Extension d'espace de noms)

```pascal
type
  IShellFolder2 = interface(IShellFolder)
    ['{93F2F68C-1D1B-11D3-A30E-00C04F79ABD1}']
    // Méthodes supplémentaires pour les extensions d'espace de noms
  end;

  TMonNamespaceExtension = class(TComObject, IShellFolder, IPersistFolder, IShellFolder2)
  private
    FPidl: PItemIDList;
  public
    // Implémentation des méthodes IShellFolder
    function ParseDisplayName(hwnd: HWND; pbc: IBindCtx; pszDisplayName: POLESTR;
      out pchEaten: ULONG; out ppidl: PItemIDList;
      out pdwAttributes: ULONG): HResult; stdcall;
    function EnumObjects(hwnd: HWND; grfFlags: DWORD;
      out ppenumIDList: IEnumIDList): HResult; stdcall;
    // ... autres méthodes
  end;
```

## Icônes et overlays

### Créer un overlay d'icône

```pascal
type
  IShellIconOverlayIdentifier = interface(IUnknown)
    ['{0C6C4200-C589-11D0-999A-00C04FD655E1}']
    function IsMemberOf(pwszPath: LPCWSTR; dwAttrib: DWORD): HRESULT; stdcall;
    function GetOverlayInfo(pwszIconFile: LPWSTR; cchMax: Integer;
      out pIndex: Integer; out pdwFlags: DWORD): HRESULT; stdcall;
    function GetPriority(out pPriority: Integer): HRESULT; stdcall;
  end;

  TMonIconOverlay = class(TComObject, IShellIconOverlayIdentifier)
  public
    function IsMemberOf(pwszPath: LPCWSTR; dwAttrib: DWORD): HRESULT; stdcall;
    function GetOverlayInfo(pwszIconFile: LPWSTR; cchMax: Integer;
      out pIndex: Integer; out pdwFlags: DWORD): HRESULT; stdcall;
    function GetPriority(out pPriority: Integer): HRESULT; stdcall;
  end;

function TMonIconOverlay.IsMemberOf(pwszPath: LPCWSTR; dwAttrib: DWORD): HRESULT;  
var
  CheminFichier: string;
begin
  CheminFichier := pwszPath;
  // Vérifier si ce fichier doit avoir l'overlay
  if Pos('.monext', LowerCase(CheminFichier)) > 0 then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TMonIconOverlay.GetOverlayInfo(pwszIconFile: LPWSTR; cchMax: Integer;
  out pIndex: Integer; out pdwFlags: DWORD): HRESULT;
begin
  StrLCopy(pwszIconFile, 'C:\MonApp\overlay.ico', cchMax);
  pIndex := 0;
  pdwFlags := ISIOI_ICONFILE;
  Result := S_OK;
end;
```

## Barres d'outils et bandes Explorer

### Créer une barre d'outils Explorer

```pascal
type
  IDeskBand = interface(IDockingWindow)
    ['{EB0FE172-1A3A-11D0-89B3-00A0C90A90AC}']
    function GetBandInfo(dwBandID, dwViewMode: DWORD; var pdbi: DESKBANDINFO): HRESULT; stdcall;
  end;

  TMaBandeExplorer = class(TComObject, IDeskBand, IObjectWithSite, IPersistStream)
  private
    FSite: IUnknown;
    FHwndParent: HWND;
    FHwnd: HWND;
  public
    function GetBandInfo(dwBandID, dwViewMode: DWORD; var pdbi: DESKBANDINFO): HRESULT; stdcall;
    // Autres méthodes d'interface...
  end;

function TMaBandeExplorer.GetBandInfo(dwBandID, dwViewMode: DWORD;
  var pdbi: DESKBANDINFO): HRESULT;
begin
  if pdbi.dwMask and DBIM_MINSIZE <> 0 then
  begin
    pdbi.ptMinSize.x := 100;
    pdbi.ptMinSize.y := 30;
  end;

  if pdbi.dwMask and DBIM_MAXSIZE <> 0 then
  begin
    pdbi.ptMaxSize.x := -1;  // Pas de limite
    pdbi.ptMaxSize.y := 30;
  end;

  if pdbi.dwMask and DBIM_TITLE <> 0 then
    StrCopy(pdbi.wszTitle, 'Ma Bande Explorer');

  Result := S_OK;
end;
```

## Optimisation et performance

### Cache d'icônes système

```pascal
uses
  ShellAPI, CommCtrl;

var
  ImageListSystem: HIMAGELIST;

procedure InitialiserCacheIcones;  
var
  FileInfo: TSHFileInfo;
begin
  // Obtenir la liste d'images système
  ImageListSystem := SHGetFileInfo('C:\', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);

  // Utiliser cette liste pour afficher des icônes sans les recharger
  SendMessage(ListView1.Handle, LVM_SETIMAGELIST, LVSIL_SMALL, ImageListSystem);
end;

function ObtenirIndexIcone(const CheminFichier: string): Integer;  
var
  FileInfo: TSHFileInfo;
begin
  SHGetFileInfo(PChar(CheminFichier), 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  Result := FileInfo.iIcon;
end;
```

### Opérations asynchrones Shell

```pascal
procedure OperationAsynchrone(const Fichiers: TStringList);  
var
  FileOp: TSHFileOpStruct;
  Thread: TThread;
begin
  Thread := TThread.CreateAnonymousThread(
    procedure
    begin
      FillChar(FileOp, SizeOf(FileOp), 0);
      FileOp.Wnd := 0;
      FileOp.wFunc := FO_COPY;
      FileOp.pFrom := PChar(Fichiers.Text);
      FileOp.pTo := PChar('C:\Destination');
      FileOp.fFlags := FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR;

      if SHFileOperation(FileOp) = 0 then
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            ShowMessage('Copie terminée');
          end
        );
      end;
    end
  );
  Thread.Start;
end;
```

## Conseils et bonnes pratiques

### 1. Gestion des chemins longs

```pascal
function CheminLongWindows(const Chemin: string): string;  
begin
  // Préfixe pour supporter les chemins > 260 caractères
  if Length(Chemin) > MAX_PATH then
    Result := '\\?\' + Chemin
  else
    Result := Chemin;
end;
```

### 2. Vérification des versions Windows

```pascal
function SupportsJumpLists: Boolean;  
var
  VersionInfo: TOSVersionInfo;
begin
  VersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VersionInfo);
  // Jump Lists disponibles à partir de Windows 7
  Result := (VersionInfo.dwMajorVersion > 6) or
           ((VersionInfo.dwMajorVersion = 6) and (VersionInfo.dwMinorVersion >= 1));
end;
```

### 3. Notification des changements

```pascal
procedure NotifierChangementShell(TypeChangement: Longint);  
begin
  SHChangeNotify(TypeChangement, SHCNF_IDLIST or SHCNF_FLUSHNOWAIT, nil, nil);
end;

// Types de notifications
begin
  NotifierChangementShell(SHCNE_ASSOCCHANGED);  // Associations modifiées
  NotifierChangementShell(SHCNE_ALLEVENTS);      // Tous les événements
  NotifierChangementShell(SHCNE_UPDATEDIR);      // Dossier mis à jour
end;
```

### 4. Libération des ressources COM

```pascal
procedure LibererInterfaceCOM(var Intf);  
var
  IUnk: IUnknown;
begin
  if IUnknown(Intf) <> nil then
  begin
    IUnk := IUnknown(Intf);
    IUnknown(Intf) := nil;
    IUnk._Release;
  end;
end;
```

## Sécurité et permissions

### Vérifier les permissions d'accès

```pascal
function VerifierAccesFichier(const CheminFichier: string; AccesDemande: DWORD): Boolean;  
var
  SecurityDescriptor: PSecurityDescriptor;
  DesiredAccess: DWORD;
  GrantedAccess: DWORD;
  PrivilegeSet: TPrivilegeSet;
  PrivilegeSetLength: DWORD;
  AccessStatus: BOOL;
  hToken: THandle;
begin
  Result := False;

  // Obtenir le token du thread actuel
  if not OpenThreadToken(GetCurrentThread, TOKEN_QUERY, False, hToken) then
    OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken);

  try
    GetFileSecurity(PChar(CheminFichier), OWNER_SECURITY_INFORMATION or
                   GROUP_SECURITY_INFORMATION or DACL_SECURITY_INFORMATION,
                   nil, 0, PrivilegeSetLength);

    GetMem(SecurityDescriptor, PrivilegeSetLength);
    try
      if GetFileSecurity(PChar(CheminFichier), OWNER_SECURITY_INFORMATION or
                        GROUP_SECURITY_INFORMATION or DACL_SECURITY_INFORMATION,
                        SecurityDescriptor, PrivilegeSetLength, PrivilegeSetLength) then
      begin
        DesiredAccess := AccesDemande;
        PrivilegeSetLength := SizeOf(TPrivilegeSet);

        AccessCheck(SecurityDescriptor, hToken, DesiredAccess,
                   GENERIC_MAPPING(nil^), @PrivilegeSet,
                   PrivilegeSetLength, GrantedAccess, AccessStatus);

        Result := AccessStatus;
      end;
    finally
      FreeMem(SecurityDescriptor);
    end;
  finally
    CloseHandle(hToken);
  end;
end;

// Utilisation
begin
  if VerifierAccesFichier('C:\Fichier.txt', FILE_GENERIC_READ) then
    WriteLn('Accès en lecture autorisé')
  else
    WriteLn('Accès en lecture refusé');
end;
```

## Ressources et documentation

### APIs Windows Shell essentielles

- **SHGetKnownFolderPath** : Obtenir les chemins des dossiers spéciaux (Windows Vista+)
- **IFileOperation** : Interface moderne pour les opérations de fichiers (Windows Vista+)
- **ITaskbarList3** : Contrôle avancé de la barre des tâches (Windows 7+)
- **IApplicationDestinations** : Gestion des éléments récents et fréquents
- **IShellItem2** : Interface moderne pour les éléments Shell

### Outils de développement

- **ShellExView** : Visualiser toutes les extensions Shell installées
- **Process Monitor** : Surveiller les accès fichiers et registre
- **Spy++** : Analyser les messages Windows
- **Registry Monitor** : Surveiller les modifications du registre

### Registre Windows - Clés importantes

```pascal
const
  // Extensions Shell
  KEY_SHELL_EXTENSIONS = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved';

  // Associations de fichiers
  KEY_FILE_EXTS = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts';

  // Menu contextuel
  KEY_CONTEXT_MENU_HANDLERS = 'SOFTWARE\Classes\*\shellex\ContextMenuHandlers';

  // Overlay icons
  KEY_ICON_OVERLAY = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\ShellIconOverlayIdentifiers';
```

## Conclusion

L'intégration avec Windows Shell et Explorer permet de créer des applications qui s'intègrent parfaitement à l'environnement Windows. Les possibilités sont vastes :

- **Menus contextuels** pour ajouter des fonctionnalités directement dans l'Explorer
- **Associations de fichiers** pour que votre application gère des types de fichiers spécifiques
- **Icônes dans la zone de notification** pour un accès rapide
- **Jump Lists** pour améliorer la productivité des utilisateurs
- **Extensions Shell** pour personnaliser complètement l'expérience Explorer

## Exemples complets d'applications

### Exemple 1 : Gestionnaire de fichiers intégré

```pascal
program GestionnaireIntegre;

uses
  Windows, SysUtils, Classes, Registry, ShellAPI, ShlObj, ActiveX, ComObj;

type
  TMonGestionnaire = class
  private
    FExtensions: TStringList;
    FMenuContextuel: string;
    FIconeTray: TNotifyIconData;
    procedure EnregistrerAssociations;
    procedure CreerMenusContextuels;
    procedure InitialiserIconeTray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Installer;
    procedure Desinstaller;
    procedure TraiterFichier(const CheminFichier: string);
  end;

constructor TMonGestionnaire.Create;  
begin
  FExtensions := TStringList.Create;
  FExtensions.Add('.monext');
  FExtensions.Add('.dat');
  FMenuContextuel := 'MonGestionnaire';
end;

destructor TMonGestionnaire.Destroy;  
begin
  FExtensions.Free;
  inherited;
end;

procedure TMonGestionnaire.EnregistrerAssociations;  
var
  Reg: TRegistry;
  i: Integer;
  TypeFichier: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    for i := 0 to FExtensions.Count - 1 do
    begin
      TypeFichier := 'MonGestionnaire' + FExtensions[i];

      // Créer l'association
      if Reg.OpenKey('\' + FExtensions[i], True) then
      begin
        Reg.WriteString('', TypeFichier);
        Reg.CloseKey;
      end;

      // Définir le type
      if Reg.OpenKey('\' + TypeFichier, True) then
      begin
        Reg.WriteString('', 'Fichier Mon Gestionnaire');
        Reg.CloseKey;
      end;

      // Définir l'icône
      if Reg.OpenKey('\' + TypeFichier + '\DefaultIcon', True) then
      begin
        Reg.WriteString('', ParamStr(0) + ',1');
        Reg.CloseKey;
      end;

      // Définir l'action
      if Reg.OpenKey('\' + TypeFichier + '\shell\open\command', True) then
      begin
        Reg.WriteString('', '"' + ParamStr(0) + '" "%1"');
        Reg.CloseKey;
      end;
    end;

    // Notifier Windows
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

  finally
    Reg.Free;
  end;
end;

procedure TMonGestionnaire.CreerMenusContextuels;  
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Menu pour tous les fichiers
    if Reg.OpenKey('\*\shell\' + FMenuContextuel, True) then
    begin
      Reg.WriteString('', 'Traiter avec Mon Gestionnaire');
      Reg.WriteString('Icon', ParamStr(0) + ',0');
      Reg.CloseKey;
    end;

    if Reg.OpenKey('\*\shell\' + FMenuContextuel + '\command', True) then
    begin
      Reg.WriteString('', '"' + ParamStr(0) + '" --process "%1"');
      Reg.CloseKey;
    end;

    // Menu pour les dossiers
    if Reg.OpenKey('\Directory\shell\' + FMenuContextuel, True) then
    begin
      Reg.WriteString('', 'Scanner le dossier');
      Reg.WriteString('Icon', ParamStr(0) + ',0');
      Reg.CloseKey;
    end;

    if Reg.OpenKey('\Directory\shell\' + FMenuContextuel + '\command', True) then
    begin
      Reg.WriteString('', '"' + ParamStr(0) + '" --scan "%1"');
      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;
end;

procedure TMonGestionnaire.InitialiserIconeTray;  
begin
  FillChar(FIconeTray, SizeOf(FIconeTray), 0);
  FIconeTray.cbSize := SizeOf(TNotifyIconData);
  FIconeTray.Wnd := GetDesktopWindow;  // Dans une vraie app, utiliser la fenêtre principale
  FIconeTray.uID := 1;
  FIconeTray.uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
  FIconeTray.uCallbackMessage := WM_USER + 1;
  FIconeTray.hIcon := LoadIcon(HInstance, 'MAINICON');
  StrCopy(FIconeTray.szTip, 'Mon Gestionnaire - Actif');

  Shell_NotifyIcon(NIM_ADD, @FIconeTray);
end;

procedure TMonGestionnaire.Installer;  
begin
  WriteLn('Installation de Mon Gestionnaire...');
  EnregistrerAssociations;
  CreerMenusContextuels;
  InitialiserIconeTray;
  WriteLn('Installation terminée !');
end;

procedure TMonGestionnaire.Desinstaller;  
var
  Reg: TRegistry;
  i: Integer;
begin
  WriteLn('Désinstallation...');

  // Retirer l'icône de la zone de notification
  Shell_NotifyIcon(NIM_DELETE, @FIconeTray);

  // Nettoyer le registre
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Supprimer les associations
    for i := 0 to FExtensions.Count - 1 do
    begin
      Reg.DeleteKey('\' + FExtensions[i]);
      Reg.DeleteKey('\MonGestionnaire' + FExtensions[i]);
    end;

    // Supprimer les menus contextuels
    Reg.DeleteKey('\*\shell\' + FMenuContextuel);
    Reg.DeleteKey('\Directory\shell\' + FMenuContextuel);

  finally
    Reg.Free;
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  WriteLn('Désinstallation terminée');
end;

procedure TMonGestionnaire.TraiterFichier(const CheminFichier: string);  
var
  InfoFichier: TSHFileInfo;
  Extension: string;
begin
  // Obtenir les informations du fichier
  SHGetFileInfo(PChar(CheminFichier), 0, InfoFichier, SizeOf(InfoFichier),
    SHGFI_DISPLAYNAME or SHGFI_TYPENAME or SHGFI_ICON);

  WriteLn('Traitement du fichier : ', InfoFichier.szDisplayName);
  WriteLn('Type : ', InfoFichier.szTypeName);

  Extension := ExtractFileExt(CheminFichier);
  if FExtensions.IndexOf(Extension) >= 0 then
  begin
    WriteLn('Fichier reconnu, traitement spécial...');
    // Traitement spécifique
  end
  else
  begin
    WriteLn('Fichier non reconnu, traitement générique...');
    // Traitement générique
  end;

  // Libérer l'icône
  DestroyIcon(InfoFichier.hIcon);
end;

// Programme principal
var
  Gestionnaire: TMonGestionnaire;
  Commande: string;
begin
  CoInitialize(nil);
  try
    Gestionnaire := TMonGestionnaire.Create;
    try
      if ParamCount > 0 then
      begin
        Commande := ParamStr(1);

        if Commande = '--install' then
          Gestionnaire.Installer
        else if Commande = '--uninstall' then
          Gestionnaire.Desinstaller
        else if Commande = '--process' then
        begin
          if ParamCount > 1 then
            Gestionnaire.TraiterFichier(ParamStr(2));
        end
        else if Commande = '--scan' then
        begin
          if ParamCount > 1 then
            WriteLn('Scan du dossier : ', ParamStr(2));
        end
        else
          Gestionnaire.TraiterFichier(Commande);
      end
      else
      begin
        WriteLn('Usage:');
        WriteLn('  MonGestionnaire --install    : Installer l''intégration');
        WriteLn('  MonGestionnaire --uninstall  : Désinstaller');
        WriteLn('  MonGestionnaire fichier.ext  : Traiter un fichier');
      end;

    finally
      Gestionnaire.Free;
    end;
  finally
    CoUninitialize;
  end;

  if ParamCount = 0 then
    ReadLn;
end.
```

### Exemple 2 : Explorateur de dossiers spéciaux

```pascal
program ExplorateurDossiersSpeciaux;

uses
  Windows, SysUtils, ShellAPI, ShlObj, ActiveX;

type
  TDossierSpecial = record
    CSIDL: Integer;
    Nom: string;
    Description: string;
  end;

const
  DossiersSpeciaux: array[0..15] of TDossierSpecial = (
    (CSIDL: CSIDL_DESKTOP; Nom: 'Bureau'; Description: 'Bureau de l''utilisateur'),
    (CSIDL: CSIDL_PERSONAL; Nom: 'Documents'; Description: 'Mes Documents'),
    (CSIDL: CSIDL_MYPICTURES; Nom: 'Images'; Description: 'Mes Images'),
    (CSIDL: CSIDL_MYMUSIC; Nom: 'Musique'; Description: 'Ma Musique'),
    (CSIDL: CSIDL_MYVIDEO; Nom: 'Vidéos'; Description: 'Mes Vidéos'),
    (CSIDL: CSIDL_FAVORITES; Nom: 'Favoris'; Description: 'Favoris Internet'),
    (CSIDL: CSIDL_STARTUP; Nom: 'Démarrage'; Description: 'Programmes au démarrage'),
    (CSIDL: CSIDL_RECENT; Nom: 'Récents'; Description: 'Documents récents'),
    (CSIDL: CSIDL_SENDTO; Nom: 'SendTo'; Description: 'Menu Envoyer vers'),
    (CSIDL: CSIDL_STARTMENU; Nom: 'Menu Démarrer'; Description: 'Menu Démarrer'),
    (CSIDL: CSIDL_APPDATA; Nom: 'AppData'; Description: 'Données d''application'),
    (CSIDL: CSIDL_LOCAL_APPDATA; Nom: 'LocalAppData'; Description: 'Données locales'),
    (CSIDL: CSIDL_PROGRAM_FILES; Nom: 'Program Files'; Description: 'Programmes installés'),
    (CSIDL: CSIDL_WINDOWS; Nom: 'Windows'; Description: 'Dossier Windows'),
    (CSIDL: CSIDL_SYSTEM; Nom: 'System32'; Description: 'Fichiers système'),
    (CSIDL: CSIDL_FONTS; Nom: 'Polices'; Description: 'Polices de caractères')
  );

function ObtenirCheminSpecial(CSIDL: Integer): string;  
var
  Chemin: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL, 0, SHGFP_TYPE_CURRENT, Chemin) = S_OK then
    Result := Chemin
  else
    Result := 'Non disponible';
end;

procedure CreerRaccourciVers(const DossierSpecial: TDossierSpecial);  
var
  Chemin, CheminRaccourci, Bureau: string;
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
begin
  Chemin := ObtenirCheminSpecial(DossierSpecial.CSIDL);
  if Chemin = 'Non disponible' then
  begin
    WriteLn('Impossible de créer un raccourci pour : ', DossierSpecial.Nom);
    Exit;
  end;

  Bureau := ObtenirCheminSpecial(CSIDL_DESKTOP);
  CheminRaccourci := Bureau + '\' + DossierSpecial.Nom + '.lnk';

  CoInitialize(nil);
  try
    ShellLink := CreateComObject(CLSID_ShellLink) as IShellLink;

    ShellLink.SetPath(PChar(Chemin));
    ShellLink.SetDescription(PChar(DossierSpecial.Description));
    ShellLink.SetWorkingDirectory(PChar(Chemin));

    PersistFile := ShellLink as IPersistFile;
    PersistFile.Save(PWideChar(WideString(CheminRaccourci)), True);

    WriteLn('Raccourci créé : ', CheminRaccourci);

  finally
    CoUninitialize;
  end;
end;

procedure AfficherInfosDossier(const DossierSpecial: TDossierSpecial);  
var
  Chemin: string;
  SearchRec: TSearchRec;
  NbFichiers, NbDossiers: Integer;
  TailleTotale: Int64;
begin
  Chemin := ObtenirCheminSpecial(DossierSpecial.CSIDL);

  WriteLn('=====================================');
  WriteLn('Nom : ', DossierSpecial.Nom);
  WriteLn('Description : ', DossierSpecial.Description);
  WriteLn('Chemin : ', Chemin);

  if (Chemin <> 'Non disponible') and DirectoryExists(Chemin) then
  begin
    NbFichiers := 0;
    NbDossiers := 0;
    TailleTotale := 0;

    if FindFirst(Chemin + '\*.*', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          if (SearchRec.Attr and faDirectory) <> 0 then
            Inc(NbDossiers)
          else
          begin
            Inc(NbFichiers);
            Inc(TailleTotale, SearchRec.Size);
          end;
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;

    WriteLn('Contenu : ', NbFichiers, ' fichiers, ', NbDossiers, ' dossiers');
    WriteLn('Taille totale : ', TailleTotale div 1024, ' Ko');
  end;

  WriteLn('=====================================');
  WriteLn;
end;

// Programme principal
var
  i, Choix: Integer;
  Reponse: string;
begin
  WriteLn('Explorateur de Dossiers Spéciaux Windows');
  WriteLn('=========================================');
  WriteLn;

  repeat
    WriteLn('Menu :');
    WriteLn('1. Lister tous les dossiers spéciaux');
    WriteLn('2. Ouvrir un dossier spécial');
    WriteLn('3. Créer des raccourcis sur le bureau');
    WriteLn('4. Afficher les informations détaillées');
    WriteLn('5. Quitter');
    WriteLn;
    Write('Votre choix : ');
    ReadLn(Choix);
    WriteLn;

    case Choix of
      1:
        begin
          for i := 0 to High(DossiersSpeciaux) do
          begin
            WriteLn(Format('%2d. %-20s : %s',
              [i + 1, DossiersSpeciaux[i].Nom,
               ObtenirCheminSpecial(DossiersSpeciaux[i].CSIDL)]));
          end;
          WriteLn;
        end;

      2:
        begin
          WriteLn('Quel dossier ouvrir ?');
          for i := 0 to High(DossiersSpeciaux) do
            WriteLn(Format('%2d. %s', [i + 1, DossiersSpeciaux[i].Nom]));
          Write('Numéro : ');
          ReadLn(i);

          if (i >= 1) and (i <= Length(DossiersSpeciaux)) then
          begin
            ShellExecute(0, 'explore',
              PChar(ObtenirCheminSpecial(DossiersSpeciaux[i - 1].CSIDL)),
              nil, nil, SW_SHOWNORMAL);
          end;
        end;

      3:
        begin
          Write('Créer des raccourcis pour tous les dossiers ? (O/N) : ');
          ReadLn(Reponse);

          if UpperCase(Reponse) = 'O' then
          begin
            for i := 0 to High(DossiersSpeciaux) do
              CreerRaccourciVers(DossiersSpeciaux[i]);
          end;
        end;

      4:
        begin
          for i := 0 to High(DossiersSpeciaux) do
            AfficherInfosDossier(DossiersSpeciaux[i]);
        end;
    end;

  until Choix = 5;

  WriteLn('Au revoir !');
end.
```

### Exemple 3 : Moniteur d'activité Shell

```pascal
program MoniteurActiviteShell;

uses
  Windows, Messages, SysUtils, Classes, ShellAPI, ShlObj, ActiveX;

type
  TShellMonitor = class(TThread)
  private
    FNotifyHandle: THandle;
    FLogFile: TextFile;
    procedure LogEvent(const Message: string);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TShellMonitor.Create;  
var
  LogFileName: string;
begin
  inherited Create(False);

  // Créer le fichier log
  LogFileName := ExtractFilePath(ParamStr(0)) + 'ShellMonitor.log';
  AssignFile(FLogFile, LogFileName);
  Rewrite(FLogFile);
  WriteLn(FLogFile, 'Moniteur d''activité Shell démarré : ', DateTimeToStr(Now));
  WriteLn(FLogFile, '================================================');

  // Configurer la surveillance
  FNotifyHandle := 0;
end;

destructor TShellMonitor.Destroy;  
begin
  if FNotifyHandle <> 0 then
    FindCloseChangeNotification(FNotifyHandle);

  WriteLn(FLogFile, '================================================');
  WriteLn(FLogFile, 'Moniteur arrêté : ', DateTimeToStr(Now));
  CloseFile(FLogFile);

  inherited;
end;

procedure TShellMonitor.LogEvent(const Message: string);  
begin
  WriteLn(FLogFile, '[', TimeToStr(Now), '] ', Message);
  Flush(FLogFile);

  // Afficher aussi dans la console
  WriteLn('[', TimeToStr(Now), '] ', Message);
end;

procedure TShellMonitor.Execute;  
var
  PIDLDesktop: PItemIDList;
  NotifyInfo: TSHChangeNotifyEntry;
  NotifyID: Integer;
  WaitResult: DWORD;
  Msg: TMsg;
begin
  CoInitialize(nil);
  try
    // Obtenir le PIDL du bureau
    SHGetFolderLocation(0, CSIDL_DESKTOP, 0, 0, PIDLDesktop);

    // Configurer la surveillance
    NotifyInfo.pidl := PIDLDesktop;
    NotifyInfo.fRecursive := True;

    // S'enregistrer pour les notifications
    NotifyID := SHChangeNotifyRegister(
      GetDesktopWindow,
      SHCNF_TYPE or SHCNF_IDLIST,
      SHCNE_ALLEVENTS,
      WM_USER + 100,
      1,
      @NotifyInfo
    );

    if NotifyID <> 0 then
    begin
      LogEvent('Surveillance activée');

      // Surveiller aussi un dossier spécifique
      FNotifyHandle := FindFirstChangeNotification(
        PChar('C:\'),
        True,
        FILE_NOTIFY_CHANGE_FILE_NAME or
        FILE_NOTIFY_CHANGE_DIR_NAME or
        FILE_NOTIFY_CHANGE_SIZE
      );

      while not Terminated do
      begin
        WaitResult := WaitForSingleObject(FNotifyHandle, 1000);

        case WaitResult of
          WAIT_OBJECT_0:
            begin
              LogEvent('Changement détecté dans le système de fichiers');
              FindNextChangeNotification(FNotifyHandle);
            end;

          WAIT_TIMEOUT:
            begin
              // Traiter les messages Windows
              while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
              begin
                if Msg.message = WM_USER + 100 then
                  LogEvent('Notification Shell reçue');

                TranslateMessage(Msg);
                DispatchMessage(Msg);
              end;
            end;
        end;
      end;

      // Se désinscrire
      SHChangeNotifyDeregister(NotifyID);
    end;

    CoTaskMemFree(PIDLDesktop);

  finally
    CoUninitialize;
  end;
end;

// Programme principal
var
  Monitor: TShellMonitor;
  Commande: string;
begin
  WriteLn('Moniteur d''activité Windows Shell');
  WriteLn('==================================');
  WriteLn('Tapez "quit" pour arrêter');
  WriteLn;

  Monitor := TShellMonitor.Create;
  try
    repeat
      ReadLn(Commande);
    until LowerCase(Commande) = 'quit';

    Monitor.Terminate;
    Monitor.WaitFor;

  finally
    Monitor.Free;
  end;

  WriteLn('Moniteur arrêté.');
end.
```

## Résumé des points clés

### Ce que vous avez appris

1. **Fondamentaux du Shell**
   - Architecture et composants de Windows Shell
   - Utilisation des APIs ShellAPI et ShlObj
   - Gestion COM pour les fonctionnalités avancées

2. **Intégration avec l'Explorer**
   - Ajout de menus contextuels
   - Création d'associations de fichiers
   - Personnalisation des icônes et overlays

3. **Interactions système**
   - Accès aux dossiers spéciaux
   - Création de raccourcis
   - Notifications et zone système

4. **Fonctionnalités avancées**
   - Jump Lists et barres des tâches
   - Extensions Shell personnalisées
   - Surveillance des événements système

### Meilleures pratiques à retenir

1. **Toujours initialiser COM** quand nécessaire
2. **Notifier Windows** des changements (SHChangeNotify)
3. **Respecter les conventions** Windows pour l'expérience utilisateur
4. **Gérer les erreurs** et vérifier les codes de retour
5. **Libérer les ressources** (handles, interfaces COM)
6. **Tester sur différentes versions** de Windows
7. **Documenter les modifications** du registre

### Prochaines étapes

Pour approfondir vos connaissances :

1. **Explorer les APIs modernes** (Windows Runtime, UWP)
2. **Étudier les extensions Shell** plus complexes
3. **Implémenter des handlers** personnalisés
4. **Créer des Property Sheets** pour l'Explorer
5. **Développer des Band Objects** pour la barre des tâches

L'intégration Shell est un domaine vaste qui permet de créer des applications véritablement intégrées à Windows. Avec FreePascal/Lazarus, vous disposez de tous les outils nécessaires pour exploiter pleinement ces fonctionnalités et offrir une expérience utilisateur native et professionnelle.

⏭️ [UAC et élévation de privilèges](/06-specificites-windows/06-uac-elevation-privileges.md)
