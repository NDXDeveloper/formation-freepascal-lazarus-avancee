🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.3 Composants non-visuels et services

## Introduction aux composants non-visuels

Les composants non-visuels sont les héros invisibles de vos applications. Contrairement aux boutons et zones de texte que l'utilisateur voit, ces composants travaillent en coulisse pour gérer le temps, les fichiers, les données, les communications réseau et bien d'autres tâches essentielles.

Imaginez une pièce de théâtre : les composants visuels sont les acteurs sur scène, tandis que les composants non-visuels sont les techniciens dans les coulisses qui gèrent l'éclairage, le son et les changements de décor. Sans eux, le spectacle ne pourrait pas avoir lieu !

Dans Lazarus, ces composants apparaissent comme de petites icônes sur votre formulaire en mode conception, mais disparaissent complètement lors de l'exécution. Ils sont là pour fournir des services, pas pour être vus.

## Les timers et la gestion du temps

### TTimer - L'horloge de votre application

TTimer est probablement le composant non-visuel le plus utilisé. Il déclenche un événement à intervalles réguliers, comme un métronome.

```pascal
// Configuration de base
Timer1 := TTimer.Create(Self);  
Timer1.Interval := 1000;  // 1000 millisecondes = 1 seconde  
Timer1.Enabled := True;   // Démarre immédiatement

// L'événement qui se répète
procedure TForm1.Timer1Timer(Sender: TObject);  
begin
  // Ce code s'exécute toutes les secondes
  Label1.Caption := 'Heure : ' + TimeToStr(Now);
end;
```

**Utilisations courantes du TTimer** :

```pascal
// Horloge en temps réel
procedure TForm1.TimerClockTimer(Sender: TObject);  
begin
  StatusBar1.Panels[0].Text := FormatDateTime('hh:nn:ss', Now);
end;

// Animation simple
procedure TForm1.TimerAnimationTimer(Sender: TObject);  
begin
  Image1.Left := Image1.Left + 5;
  if Image1.Left > Form1.Width then
    Image1.Left := -Image1.Width;
end;

// Sauvegarde automatique
procedure TForm1.TimerAutoSaveTimer(Sender: TObject);  
begin
  if DocumentModified then
    SaveDocument;
end;

// Déconnexion après inactivité
procedure TForm1.TimerIdleTimer(Sender: TObject);  
begin
  Inc(FIdleTime);
  if FIdleTime > 300 then  // 5 minutes
  begin
    ShowMessage('Déconnexion pour inactivité');
    Logout;
  end;
end;
```

**Conseils importants pour TTimer** :

```pascal
// Éviter les conflits - désactiver pendant le traitement
procedure TForm1.Timer1Timer(Sender: TObject);  
begin
  Timer1.Enabled := False;  // Stop temporaire
  try
    // Traitement potentiellement long
    ProcessComplexData;
  finally
    Timer1.Enabled := True;  // Redémarrer
  end;
end;

// Timer à usage unique
procedure TForm1.StartCountdown;  
begin
  Timer1.Interval := 3000;  // 3 secondes
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);  
begin
  Timer1.Enabled := False;  // S'arrête après une fois
  ShowMessage('3 secondes écoulées !');
end;
```

### TIdleTimer - Détecter l'inactivité

Un timer spécialisé qui ne se déclenche que lorsque l'application est inactive.

```pascal
IdleTimer1.Enabled := True;  
IdleTimer1.Interval := 100;

procedure TForm1.IdleTimer1Timer(Sender: TObject);  
begin
  // Tâches de fond quand l'utilisateur n'interagit pas
  UpdateBackgroundTasks;
  CheckForUpdates;
end;
```

## Composants de dialogue

### Dialogues de fichiers

Ces composants affichent les boîtes de dialogue standard du système pour ouvrir ou sauvegarder des fichiers.

#### TOpenDialog - Ouvrir un fichier

```pascal
// Configuration
OpenDialog1 := TOpenDialog.Create(Self);  
OpenDialog1.Title := 'Choisir un fichier à ouvrir';  
OpenDialog1.InitialDir := GetCurrentDir;  
OpenDialog1.Filter := 'Fichiers texte|*.txt|Tous les fichiers|*.*';  
OpenDialog1.FilterIndex := 1;  // Premier filtre par défaut

// Utilisation
if OpenDialog1.Execute then  
begin
  // L'utilisateur a choisi un fichier
  Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
  Caption := 'Éditeur - ' + ExtractFileName(OpenDialog1.FileName);
end;

// Options utiles
OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];  
if OpenDialog1.Execute then  
begin
  // Plusieurs fichiers sélectionnés
  for i := 0 to OpenDialog1.Files.Count - 1 do
    ProcessFile(OpenDialog1.Files[i]);
end;
```

#### TSaveDialog - Sauvegarder un fichier

```pascal
SaveDialog1.Title := 'Enregistrer le document';  
SaveDialog1.Filter := 'Document Word|*.docx|Document texte|*.txt|PDF|*.pdf';  
SaveDialog1.DefaultExt := 'txt';  // Extension ajoutée automatiquement  
SaveDialog1.Options := SaveDialog1.Options + [ofOverwritePrompt];

if SaveDialog1.Execute then  
begin
  Memo1.Lines.SaveToFile(SaveDialog1.FileName);
  FCurrentFile := SaveDialog1.FileName;
  FModified := False;
end;
```

#### TSelectDirectoryDialog - Choisir un dossier

```pascal
SelectDirectoryDialog1.Title := 'Choisir le dossier de destination';  
SelectDirectoryDialog1.InitialDir := GetUserDir;

if SelectDirectoryDialog1.Execute then  
begin
  FBackupPath := SelectDirectoryDialog1.FileName;
  Label1.Caption := 'Dossier : ' + FBackupPath;
end;
```

### Dialogues de personnalisation

#### TColorDialog - Choisir une couleur

```pascal
ColorDialog1.Color := Label1.Font.Color;  // Couleur initiale

if ColorDialog1.Execute then  
begin
  Label1.Font.Color := ColorDialog1.Color;
  // Sauvegarder le choix
  Settings.TextColor := ColorToString(ColorDialog1.Color);
end;

// Avec couleurs personnalisées
ColorDialog1.CustomColors.Add('ColorA=' + IntToHex(clRed, 6));  
ColorDialog1.CustomColors.Add('ColorB=' + IntToHex(clBlue, 6));
```

#### TFontDialog - Choisir une police

```pascal
FontDialog1.Font := Memo1.Font;  // Police actuelle  
FontDialog1.Options := [fdEffects, fdTrueTypeOnly];  // Effets et TrueType seulement

if FontDialog1.Execute then  
begin
  Memo1.Font := FontDialog1.Font;
  SaveFontSettings(FontDialog1.Font);
end;

// Limiter les choix
FontDialog1.MinFontSize := 8;  
FontDialog1.MaxFontSize := 72;
```

### Dialogues de recherche

#### TFindDialog - Rechercher

```pascal
// Configuration
FindDialog1.FindText := 'recherche';  
FindDialog1.Options := [frDown, frMatchCase];

// Afficher
FindDialog1.Execute;

// Gérer la recherche
procedure TForm1.FindDialog1Find(Sender: TObject);  
var
  FoundAt: Integer;
  StartPos: Integer;
begin
  with Sender as TFindDialog do
  begin
    StartPos := Memo1.SelStart + Memo1.SelLength;
    FoundAt := Pos(FindText, Copy(Memo1.Text, StartPos + 1, MaxInt));

    if FoundAt > 0 then
    begin
      Memo1.SelStart := StartPos + FoundAt - 1;
      Memo1.SelLength := Length(FindText);
      Memo1.SetFocus;
    end
    else
      ShowMessage('Texte non trouvé');
  end;
end;
```

#### TReplaceDialog - Rechercher et remplacer

```pascal
ReplaceDialog1.FindText := 'ancien';  
ReplaceDialog1.ReplaceText := 'nouveau';

procedure TForm1.ReplaceDialog1Replace(Sender: TObject);  
begin
  with Sender as TReplaceDialog do
  begin
    if frReplaceAll in Options then
      Memo1.Text := StringReplace(Memo1.Text, FindText, ReplaceText, [rfReplaceAll])
    else if Memo1.SelText = FindText then
      Memo1.SelText := ReplaceText;
  end;
end;
```

## Composants de menu et d'action

### TMainMenu - Menu principal

Le menu principal de votre application, affiché en haut de la fenêtre.

```pascal
// Création par code (généralement fait visuellement)
MainMenu1 := TMainMenu.Create(Self);  
Form1.Menu := MainMenu1;

// Créer la structure
MenuFile := TMenuItem.Create(MainMenu1);  
MenuFile.Caption := '&Fichier';  
MainMenu1.Items.Add(MenuFile);

MenuNew := TMenuItem.Create(MenuFile);  
MenuNew.Caption := '&Nouveau' + #9 + 'Ctrl+N';  // #9 = Tab pour raccourci  
MenuNew.ShortCut := ShortCut(Word('N'), [ssCtrl]);  
MenuNew.OnClick := @MenuNewClick;  
MenuFile.Add(MenuNew);

// Séparateur
MenuSep := TMenuItem.Create(MenuFile);  
MenuSep.Caption := '-';  // Un tiret crée un séparateur  
MenuFile.Add(MenuSep);

// Menu avec image
MenuSave := TMenuItem.Create(MenuFile);  
MenuSave.Caption := '&Enregistrer';  
MenuSave.ImageIndex := 2;  // Si ImageList associée  
MenuSave.Enabled := False;  // Grisé au départ  
MenuFile.Add(MenuSave);
```

### TPopupMenu - Menu contextuel

Menu qui apparaît avec le clic droit.

```pascal
// Associer à un composant
ListBox1.PopupMenu := PopupMenu1;

// Ou afficher manuellement
procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    // Personnaliser avant d'afficher
    PopupMenu1.Items[0].Enabled := ListBox1.ItemIndex >= 0;
    PopupMenu1.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

// Menu contextuel dynamique
procedure TForm1.PopupMenu1Popup(Sender: TObject);  
begin
  // Adapter le menu au contexte
  MenuEdit.Visible := CanEdit;
  MenuDelete.Enabled := HasSelection;
end;
```

### TActionList - Centraliser les actions

TActionList centralise la logique des actions (nouveau, ouvrir, sauver...) utilisables depuis plusieurs endroits.

```pascal
// Créer une action
ActionNew := TAction.Create(ActionList1);  
ActionNew.Caption := '&Nouveau';  
ActionNew.ShortCut := ShortCut(Word('N'), [ssCtrl]);  
ActionNew.Hint := 'Créer un nouveau document';  
ActionNew.ImageIndex := 0;  
ActionNew.OnExecute := @ActionNewExecute;  
ActionNew.OnUpdate := @ActionNewUpdate;

// Associer l'action à plusieurs composants
Button1.Action := ActionNew;  
MenuItemNew.Action := ActionNew;  
ToolButton1.Action := ActionNew;
// Tous sont synchronisés automatiquement !

// Gérer l'action
procedure TForm1.ActionNewExecute(Sender: TObject);  
begin
  CreateNewDocument;
end;

// Mettre à jour l'état
procedure TForm1.ActionNewUpdate(Sender: TObject);  
begin
  TAction(Sender).Enabled := not FProcessing;
end;
```

## Composants de gestion de fichiers

### TFileListBox - Liste de fichiers

Affiche les fichiers d'un dossier.

```pascal
FileListBox1.Directory := 'C:\Documents';  
FileListBox1.Mask := '*.txt;*.doc';  // Filtre  
FileListBox1.ShowGlyphs := True;     // Icônes de fichiers

// Réagir à la sélection
procedure TForm1.FileListBox1Click(Sender: TObject);  
begin
  if FileListBox1.ItemIndex >= 0 then
    LoadFile(FileListBox1.FileName);
end;
```

### TDirectoryListBox - Liste de dossiers

Affiche l'arborescence des dossiers.

```pascal
DirectoryListBox1.Directory := GetCurrentDir;  
DirectoryListBox1.FileList := FileListBox1;  // Lier à une liste de fichiers

// Changement de dossier
procedure TForm1.DirectoryListBox1Change(Sender: TObject);  
begin
  CurrentPath := DirectoryListBox1.Directory;
  UpdateFileList;
end;
```

### TDriveComboBox - Sélection de lecteur

Permet de choisir un lecteur (Windows principalement).

```pascal
DriveComboBox1.DirList := DirectoryListBox1;  // Lier au dossier  
DriveComboBox1.Drive := 'C';

procedure TForm1.DriveComboBox1Change(Sender: TObject);  
begin
  DirectoryListBox1.Drive := DriveComboBox1.Drive;
end;
```

## Composants de données et de liaison

### TDataSource - Source de données

Fait le lien entre les composants de données et l'interface.

```pascal
DataSource1 := TDataSource.Create(Self);  
DataSource1.DataSet := SQLQuery1;  // Lier à une requête

// Connecter les contrôles
DBEdit1.DataSource := DataSource1;  
DBEdit1.DataField := 'Nom';

DBGrid1.DataSource := DataSource1;
```

### TImageList - Bibliothèque d'images

Stocke des images pour les menus, boutons, listes, etc.

```pascal
// Configuration
ImageList1.Width := 16;  
ImageList1.Height := 16;

// Ajouter des images
ImageList1.Add(Image1.Picture.Bitmap, nil);  // nil = pas de masque

// Ou charger depuis des fichiers
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromFile('icon1.bmp');
    ImageList1.Add(Bitmap, nil);
  finally
    Bitmap.Free;
  end;
end;

// Utiliser avec d'autres composants
ToolBar1.Images := ImageList1;  
TreeView1.Images := ImageList1;  
ListView1.SmallImages := ImageList1;
```

### TApplicationProperties - Propriétés de l'application

Configure le comportement global de l'application.

```pascal
ApplicationProperties1.Title := 'Mon Application';  
ApplicationProperties1.ShowMainForm := True;  
ApplicationProperties1.ShowButtonGlyphs := sbgAlways;

// Événements globaux
procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);  
begin
  // Appelé quand l'application est inactive
  StatusBar1.Panels[0].Text := 'Prêt';
  Done := True;  // Pas d'autres traitements idle
end;

procedure TForm1.ApplicationProperties1Hint(Sender: TObject);  
begin
  // Afficher les hints dans la barre de statut
  StatusBar1.SimpleText := Application.Hint;
end;

procedure TForm1.ApplicationProperties1Exception(Sender: TObject; E: Exception);  
begin
  // Gestion globale des exceptions
  LogError(E.Message);
  ShowMessage('Erreur : ' + E.Message);
end;
```

## Composants système

### TProcess - Exécuter des programmes

Lance des programmes externes ou des commandes système.

```pascal
Process1 := TProcess.Create(nil);  
try
  Process1.Executable := 'notepad.exe';
  Process1.Parameters.Add('document.txt');
  Process1.Options := [poWaitOnExit];  // Attendre la fin
  Process1.Execute;
finally
  Process1.Free;
end;

// Exécution avec capture de sortie
Process1.Executable := 'dir';  
Process1.Parameters.Add('/b');  
Process1.Options := [poUsePipes, poNoConsole];  
Process1.Execute;

// Lire la sortie
StringList := TStringList.Create;  
try
  StringList.LoadFromStream(Process1.Output);
  Memo1.Lines := StringList;
finally
  StringList.Free;
end;
```

### TRegistry - Accès au registre (Windows)

Lit et écrit dans le registre Windows.

```pascal
uses Registry;

var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    // Écrire
    if Reg.OpenKey('Software\MonApp', True) then
    begin
      Reg.WriteString('LastFile', FileName);
      Reg.WriteInteger('WindowWidth', Form1.Width);
      Reg.WriteBool('ShowToolbar', True);
      Reg.CloseKey;
    end;

    // Lire
    if Reg.OpenKey('Software\MonApp', False) then
    begin
      if Reg.ValueExists('LastFile') then
        FLastFile := Reg.ReadString('LastFile');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;
```

### TIniFile - Fichiers de configuration

Alternative portable au registre, utilise des fichiers INI.

```pascal
uses IniFiles;

var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create('config.ini');
  try
    // Écrire
    Ini.WriteString('General', 'Username', Edit1.Text);
    Ini.WriteInteger('Window', 'Width', Form1.Width);
    Ini.WriteBool('Options', 'AutoSave', CheckBox1.Checked);

    // Lire
    Edit1.Text := Ini.ReadString('General', 'Username', 'Utilisateur');
    Form1.Width := Ini.ReadInteger('Window', 'Width', 800);
    CheckBox1.Checked := Ini.ReadBool('Options', 'AutoSave', False);
  finally
    Ini.Free;
  end;
end;
```

### TXMLDocument - Traitement XML

Pour lire et écrire des fichiers XML.

```pascal
uses DOM, XMLRead, XMLWrite;

var
  Doc: TXMLDocument;
  RootNode, ChildNode: TDOMNode;
begin
  // Créer un document XML
  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement('Configuration');
    Doc.AppendChild(RootNode);

    ChildNode := Doc.CreateElement('Setting');
    TDOMElement(ChildNode).SetAttribute('name', 'Theme');
    TDOMElement(ChildNode).SetAttribute('value', 'Dark');
    RootNode.AppendChild(ChildNode);

    WriteXMLFile(Doc, 'config.xml');
  finally
    Doc.Free;
  end;

  // Lire un document XML
  ReadXMLFile(Doc, 'config.xml');
  try
    RootNode := Doc.DocumentElement;
    ChildNode := RootNode.FindNode('Setting');
    if ChildNode <> nil then
      ShowMessage(TDOMElement(ChildNode).GetAttribute('value'));
  finally
    Doc.Free;
  end;
end;
```

## Composants de communication

### Composants Internet de base

#### TIdHTTP - Client HTTP

```pascal
uses IdHTTP;

var
  HTTP: TIdHTTP;
  Response: string;
begin
  HTTP := TIdHTTP.Create(nil);
  try
    // GET simple
    Response := HTTP.Get('http://api.example.com/data');
    Memo1.Text := Response;

    // POST avec paramètres
    Params := TStringList.Create;
    try
      Params.Add('user=admin');
      Params.Add('pass=secret');
      Response := HTTP.Post('http://api.example.com/login', Params);
    finally
      Params.Free;
    end;
  finally
    HTTP.Free;
  end;
end;
```

#### TIdTCPClient/TIdTCPServer - Communication TCP

```pascal
// Client TCP
TCPClient := TIdTCPClient.Create(nil);  
try
  TCPClient.Host := '192.168.1.100';
  TCPClient.Port := 9000;
  TCPClient.Connect;

  TCPClient.IOHandler.WriteLn('Hello Server');
  Response := TCPClient.IOHandler.ReadLn;
finally
  TCPClient.Free;
end;

// Serveur TCP
TCPServer := TIdTCPServer.Create(nil);  
TCPServer.DefaultPort := 9000;  
TCPServer.OnExecute := @TCPServerExecute;  
TCPServer.Active := True;

procedure TForm1.TCPServerExecute(AContext: TIdContext);  
var
  Msg: string;
begin
  Msg := AContext.Connection.IOHandler.ReadLn;
  AContext.Connection.IOHandler.WriteLn('Echo: ' + Msg);
end;
```

## Composants de notification

### TTrayIcon - Icône dans la zone de notification

Affiche une icône dans la barre système (systray).

```pascal
TrayIcon1 := TTrayIcon.Create(Self);  
TrayIcon1.Icon.LoadFromFile('app.ico');  
TrayIcon1.Hint := 'Mon Application';  
TrayIcon1.PopupMenu := PopupMenuTray;  
TrayIcon1.Visible := True;

// Notification ballon (Windows)
TrayIcon1.BalloonTitle := 'Notification';  
TrayIcon1.BalloonHint := 'Nouveau message reçu !';  
TrayIcon1.BalloonFlags := bfInfo;  
TrayIcon1.ShowBalloonHint;

// Réagir au clic
procedure TForm1.TrayIcon1Click(Sender: TObject);  
begin
  Form1.WindowState := wsNormal;
  Form1.Show;
  Application.BringToFront;
end;

// Menu contextuel
procedure TForm1.MenuRestoreClick(Sender: TObject);  
begin
  Form1.Show;
  TrayIcon1.Visible := False;
end;
```

### TNotification - Notifications système

Pour les notifications modernes du système.

```pascal
Notification1 := TNotification.Create(Self);  
Notification1.Title := 'Rappel';  
Notification1.AlertBody := 'Réunion dans 10 minutes';  
Notification1.FireDate := Now + EncodeTime(0, 10, 0, 0);  
Notification1.Present;
```

## Composants de validation et de formatage

### TMaskEdit - Saisie avec masque

Force un format de saisie spécifique.

```pascal
// Masques courants
MaskEdit1.EditMask := '000-000-0000;1;_';  // Téléphone  
MaskEdit2.EditMask := '00/00/0000;1;_';    // Date  
MaskEdit3.EditMask := '>LL-00000;1;_';     // Code postal canadien

// Légende des masques :
// 0 = Chiffre obligatoire
// 9 = Chiffre optionnel
// L = Lettre obligatoire
// l = Lettre optionnelle
// A = Alphanumér obligatoire
// > = Majuscules
// < = Minuscules
// ;1 = Enregistrer les caractères littéraux
// ;0 = Ne pas enregistrer les caractères littéraux
// _ = Caractère de remplissage

// Récupérer la valeur
PhoneNumber := MaskEdit1.Text;  // Avec le masque  
PhoneNumber := MaskEdit1.EditText;  // Sans le masque
```

### TLabeledEdit - Zone de texte avec label

Combine un TLabel et un TEdit en un seul composant.

```pascal
LabeledEdit1.EditLabel.Caption := 'Nom d''utilisateur :';  
LabeledEdit1.LabelPosition := lpAbove;  // ou lpLeft, lpRight, lpBelow  
LabeledEdit1.LabelSpacing := 3;

// S'utilise comme un TEdit normal
UserName := LabeledEdit1.Text;
```

## Optimisation et bonnes pratiques

### Gestion de la mémoire

```pascal
// Toujours libérer les composants créés dynamiquement
Timer := TTimer.Create(nil);  // nil = pas de propriétaire auto  
try
  Timer.Interval := 1000;
  Timer.OnTimer := @TimerTick;
  Timer.Enabled := True;
  // Utilisation...
finally
  Timer.Free;  // Libération obligatoire
end;

// Avec un propriétaire, libération automatique
Timer := TTimer.Create(Self);  // Self = Form propriétaire  
Timer.Interval := 1000;
// Pas besoin de Free, détruit avec la Form
```

### Organisation des composants non-visuels

```pascal
// Grouper logiquement sur le formulaire
// Zone 1 : Timers
// Zone 2 : Dialogues
// Zone 3 : Menus et actions
// Zone 4 : Données

// Nommer clairement
TimerAutosave     // Pas Timer1  
OpenDialogImage   // Pas OpenDialog1  
ActionListMain    // Pas ActionList1

// Commenter leur rôle
TimerAutosave.Tag := 1;  // Sauvegarde toutes les 5 minutes
```

### Centralisation de la logique

```pascal
// Utiliser TActionList pour centraliser
// Au lieu de dupliquer le code dans plusieurs événements OnClick

// Mauvais
procedure TForm1.ButtonSaveClick(Sender: TObject);  
begin
  SaveDocument;
end;

procedure TForm1.MenuSaveClick(Sender: TObject);  
begin
  SaveDocument;
end;

// Bon
procedure TForm1.ActionSaveExecute(Sender: TObject);  
begin
  SaveDocument;
end;
// Et associer l'action aux deux composants
```

## Cycle de vie des composants non-visuels

### Création et initialisation

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin
  // Initialiser les composants non-visuels
  Timer1.Interval := 1000;
  Timer1.Enabled := False;  // Démarrage manuel plus tard

  // Configurer les dialogues
  OpenDialog1.InitialDir := GetDocumentsPath;
  OpenDialog1.Filter := BuildFilterString;

  // Préparer les actions
  UpdateActionStates;
end;
```

### Utilisation pendant l'exécution

```pascal
procedure TForm1.StartMonitoring;  
begin
  Timer1.Enabled := True;
  TrayIcon1.Visible := True;
  ApplicationProperties1.OnIdle := @ApplicationIdle;
end;

procedure TForm1.StopMonitoring;  
begin
  Timer1.Enabled := False;
  TrayIcon1.Visible := False;
  ApplicationProperties1.OnIdle := nil;
end;
```

### Nettoyage et libération

```pascal
procedure TForm1.FormDestroy(Sender: TObject);  
begin
  // Arrêter les timers
  Timer1.Enabled := False;

  // Fermer les connexions
  if TCPClient.Connected then
    TCPClient.Disconnect;

  // Sauver les configurations
  SaveSettings;

  // Libérer les ressources créées dynamiquement
  FDynamicList.Free;
end;
```

## Conseils pour choisir le bon composant

### Pour la gestion du temps
- **TTimer** : Intervalles réguliers, animations, rafraîchissements
- **TIdleTimer** : Tâches de fond quand l'application est inactive

### Pour les fichiers et dossiers
- **TOpenDialog/TSaveDialog** : Interaction utilisateur standard
- **TFileListBox** : Afficher une liste de fichiers
- **TProcess** : Lancer des programmes externes

### Pour la configuration
- **TIniFile** : Configuration simple, portable
- **TRegistry** : Intégration Windows profonde
- **TXMLDocument** : Configurations complexes, structurées

### Pour les menus et actions
- **TMainMenu** : Menu principal de l'application
- **TPopupMenu** : Menus contextuels
- **TActionList** : Centraliser la logique des commandes

### Pour les notifications
- **TTrayIcon** : Présence permanente dans la barre système
- **TNotification** : Alertes ponctuelles

## Points clés à retenir

1. **Les composants non-visuels sont essentiels** - Ils gèrent la logique métier de votre application
2. **Ils apparaissent en conception, pas en exécution** - Visibles uniquement pour le développeur
3. **TTimer est votre ami** - Pour toute action répétitive ou différée
4. **Les dialogues standard** - Utilisez-les pour une interface cohérente avec le système
5. **TActionList centralise** - Évite la duplication de code entre menus, boutons et barres d'outils
6. **Gérez la mémoire** - Libérez les composants créés avec nil comme propriétaire
7. **Nommez clairement** - Des noms explicites facilitent la maintenance
8. **Les composants système** - TProcess, TRegistry, TIniFile pour interagir avec l'OS
9. **La configuration portable** - Préférez TIniFile au TRegistry pour la portabilité
10. **Organisation logique** - Groupez les composants non-visuels par fonction sur le formulaire

Ces composants non-visuels transforment une simple interface en une application complète et fonctionnelle. Ils sont la colle qui lie votre interface utilisateur au système, aux données et à la logique métier de votre application.

## Composants avancés pour les services

### Services système et tâches de fond

#### TThread - Exécution en arrière-plan

Bien que TThread ne soit pas un composant visuel au sens strict, il mérite d'être mentionné pour les tâches de fond.

```pascal
// Créer un thread simple
type
  TBackgroundTask = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

constructor TBackgroundTask.Create;  
begin
  inherited Create(False);  // False = démarre immédiatement
  FreeOnTerminate := True;  // Libération automatique
end;

procedure TBackgroundTask.Execute;  
begin
  while not Terminated do
  begin
    // Travail en arrière-plan
    ProcessData;

    // Mettre à jour l'interface (thread-safe)
    Synchronize(@UpdateUI);

    Sleep(100);
  end;
end;

// Utilisation
BackgroundTask := TBackgroundTask.Create;
```

#### TAsyncProcess - Processus asynchrone

Version asynchrone de TProcess pour ne pas bloquer l'interface.

```pascal
AsyncProcess := TAsyncProcess.Create(nil);  
try
  AsyncProcess.Executable := 'longprocess.exe';
  AsyncProcess.OnTerminate := @ProcessTerminated;
  AsyncProcess.Execute;
  // L'application continue pendant l'exécution
finally
  // Nettoyer après terminaison
end;

procedure TForm1.ProcessTerminated(Sender: TObject);  
begin
  ShowMessage('Processus terminé avec code : ' +
              IntToStr(TAsyncProcess(Sender).ExitStatus));
end;
```

### Composants de monitoring

#### TSystemMonitor - Surveillance système (personnalisé)

Exemple de composant personnalisé pour surveiller le système.

```pascal
type
  TSystemMonitor = class(TComponent)
  private
    FTimer: TTimer;
    FOnMemoryLow: TNotifyEvent;
    FOnDiskSpaceLow: TNotifyEvent;
    procedure CheckSystem(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnMemoryLow: TNotifyEvent read FOnMemoryLow write FOnMemoryLow;
    property OnDiskSpaceLow: TNotifyEvent read FOnDiskSpaceLow write FOnDiskSpaceLow;
  end;

constructor TSystemMonitor.Create(AOwner: TComponent);  
begin
  inherited;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 60000;  // Vérifier chaque minute
  FTimer.OnTimer := @CheckSystem;
  FTimer.Enabled := True;
end;

procedure TSystemMonitor.CheckSystem(Sender: TObject);  
begin
  // Vérifier la mémoire
  if GetFreeMemory < MinMemory then
    if Assigned(FOnMemoryLow) then
      FOnMemoryLow(Self);

  // Vérifier l'espace disque
  if GetFreeDiskSpace('C:\') < MinDiskSpace then
    if Assigned(FOnDiskSpaceLow) then
      FOnDiskSpaceLow(Self);
end;
```

## Patterns d'utilisation avancés

### Singleton pour les services globaux

```pascal
type
  TConfigurationManager = class(TComponent)
  private
    class var FInstance: TConfigurationManager;
    FIniFile: TIniFile;
  public
    class function GetInstance: TConfigurationManager;
    function ReadValue(const Section, Key: string; Default: string = ''): string;
    procedure WriteValue(const Section, Key, Value: string);
  end;

class function TConfigurationManager.GetInstance: TConfigurationManager;  
begin
  if not Assigned(FInstance) then
    FInstance := TConfigurationManager.Create(Application);
  Result := FInstance;
end;

// Utilisation
Value := TConfigurationManager.GetInstance.ReadValue('General', 'Theme', 'Light');
```

### Factory pattern pour les dialogues

```pascal
type
  TDialogFactory = class
  public
    class function CreateOpenDialog(const Filter: string): TOpenDialog;
    class function CreateSaveDialog(const Filter: string): TSaveDialog;
    class function CreateConfirmDialog(const Message: string): Integer;
  end;

class function TDialogFactory.CreateOpenDialog(const Filter: string): TOpenDialog;  
begin
  Result := TOpenDialog.Create(nil);
  Result.Filter := Filter;
  Result.Options := [ofFileMustExist, ofPathMustExist];
  Result.InitialDir := GetDocumentsPath;
end;

// Utilisation
Dialog := TDialogFactory.CreateOpenDialog('Images|*.jpg;*.png|Tous|*.*');  
try
  if Dialog.Execute then
    LoadImage(Dialog.FileName);
finally
  Dialog.Free;
end;
```

### Observer pattern avec les événements

```pascal
uses Classes;  // TMethodList est dans l'unité Classes

type
  TDataManager = class(TComponent)
  private
    FOnDataChanged: TNotifyEvent;
    FObservers: TMethodList;  // TMethodList gère les method pointers
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterObserver(Observer: TNotifyEvent);
    procedure UnregisterObserver(Observer: TNotifyEvent);
    procedure NotifyObservers;
    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
  end;

constructor TDataManager.Create(AOwner: TComponent);  
begin
  inherited;
  FObservers := TMethodList.Create;
end;

destructor TDataManager.Destroy;  
begin
  FObservers.Free;
  inherited;
end;

procedure TDataManager.NotifyObservers;  
var
  i: Integer;
begin
  // Notifier l'événement principal
  if Assigned(FOnDataChanged) then
    FOnDataChanged(Self);

  // Notifier tous les observateurs enregistrés
  // TMethodList stocke des TMethod (method pointers à 2 champs)
  for i := 0 to FObservers.Count - 1 do
    TNotifyEvent(FObservers[i])(Self);
end;
```

## Débogage des composants non-visuels

### Techniques de débogage

```pascal
// Journalisation des timers
procedure TForm1.Timer1Timer(Sender: TObject);  
begin
  {$IFDEF DEBUG}
  DebugLn(Format('Timer1 triggered at %s', [TimeToStr(Now)]));
  {$ENDIF}

  // Code du timer
end;

// Traçage des dialogues
function ShowOpenDialog: string;  
begin
  Result := '';
  {$IFDEF DEBUG}
  DebugLn('Showing open dialog...');
  {$ENDIF}

  if OpenDialog1.Execute then
  begin
    Result := OpenDialog1.FileName;
    {$IFDEF DEBUG}
    DebugLn('File selected: ' + Result);
    {$ENDIF}
  end
  else
  begin
    {$IFDEF DEBUG}
    DebugLn('Dialog cancelled');
    {$ENDIF}
  end;
end;

// Surveillance des actions
procedure TForm1.ActionListExecute(Action: TBasicAction; var Handled: Boolean);  
begin
  {$IFDEF DEBUG}
  if Action is TAction then
    DebugLn('Executing action: ' + TAction(Action).Caption);
  {$ENDIF}
end;
```

### Outils de diagnostic

```pascal
// Composant de diagnostic personnalisé
type
  TDiagnostics = class(TComponent)
  private
    FLogFile: TextFile;
    FEnabled: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Log(const Message: string);
    procedure LogComponentState(Component: TComponent);
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

procedure TDiagnostics.LogComponentState(Component: TComponent);  
begin
  if not FEnabled then Exit;

  Log('Component: ' + Component.Name);
  Log('  Class: ' + Component.ClassName);

  if Component is TTimer then
  begin
    Log('  Enabled: ' + BoolToStr(TTimer(Component).Enabled, True));
    Log('  Interval: ' + IntToStr(TTimer(Component).Interval));
  end
  else if Component is TAction then
  begin
    Log('  Enabled: ' + BoolToStr(TAction(Component).Enabled, True));
    Log('  Caption: ' + TAction(Component).Caption);
  end;
end;
```

## Intégration avec les frameworks

### Intégration avec les bases de données

```pascal
// Composant de connexion personnalisé
type
  TDatabaseService = class(TComponent)
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
  public
    function Connect(const Host, Database, User, Password: string): Boolean;
    procedure Disconnect;
    function ExecuteQuery(const SQL: string): TSQLQuery;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
  end;

function TDatabaseService.Connect(const Host, Database, User, Password: string): Boolean;  
begin
  Result := False;
  try
    FConnection.HostName := Host;
    FConnection.DatabaseName := Database;
    FConnection.UserName := User;
    FConnection.Password := Password;
    FConnection.Open;

    Result := FConnection.Connected;
    if Result and Assigned(FOnConnected) then
      FOnConnected(Self);
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;
```

### Intégration avec les services web

```pascal
// Service REST personnalisé
type
  TRESTService = class(TComponent)
  private
    FHTTP: TIdHTTP;
    FBaseURL: string;
    FOnRequestComplete: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    function Get(const Endpoint: string): string;
    function Post(const Endpoint: string; Data: TJSONObject): string;
    property BaseURL: string read FBaseURL write FBaseURL;
    property OnRequestComplete: TNotifyEvent read FOnRequestComplete write FOnRequestComplete;
  end;

function TRESTService.Get(const Endpoint: string): string;  
begin
  Result := FHTTP.Get(FBaseURL + Endpoint);
  if Assigned(FOnRequestComplete) then
    FOnRequestComplete(Self);
end;
```

## Optimisation des performances

### Gestion efficace des timers

```pascal
// Timer unique pour plusieurs tâches
type
  TTaskScheduler = class(TComponent)
  private
    FTimer: TTimer;
    FTasks: TList;
    procedure ExecuteTasks(Sender: TObject);
  public
    procedure AddTask(Task: TScheduledTask; Interval: Integer);
    procedure RemoveTask(Task: TScheduledTask);
  end;

procedure TTaskScheduler.ExecuteTasks(Sender: TObject);  
var
  i: Integer;
  Task: TScheduledTask;
begin
  for i := 0 to FTasks.Count - 1 do
  begin
    Task := TScheduledTask(FTasks[i]);
    if (GetTickCount - Task.LastRun) >= Task.Interval then
    begin
      Task.Execute;
      Task.LastRun := GetTickCount;
    end;
  end;
end;
```

### Pool de composants réutilisables

```pascal
// Pool de dialogues pour éviter les créations/destructions répétées
type
  TDialogPool = class
  private
    FOpenDialogs: TStack;
    FSaveDialogs: TStack;
  public
    function GetOpenDialog: TOpenDialog;
    procedure ReturnOpenDialog(Dialog: TOpenDialog);
    function GetSaveDialog: TSaveDialog;
    procedure ReturnSaveDialog(Dialog: TSaveDialog);
  end;

function TDialogPool.GetOpenDialog: TOpenDialog;  
begin
  if FOpenDialogs.Count > 0 then
    Result := TOpenDialog(FOpenDialogs.Pop)
  else
    Result := TOpenDialog.Create(nil);

  // Réinitialiser
  Result.FileName := '';
  Result.Filter := '';
end;

procedure TDialogPool.ReturnOpenDialog(Dialog: TOpenDialog);  
begin
  FOpenDialogs.Push(Dialog);
end;
```

## Migration et compatibilité

### Portage depuis Delphi

La plupart des composants non-visuels de la LCL sont compatibles avec Delphi. `TRegistry.OpenKeyReadOnly` existe dans les deux environnements :

```pascal
// Ce code fonctionne tel quel en Delphi et en FPC/Lazarus
function GetRegistryValue(const Key, ValueName: string): string;  
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(Key) then  // Existe en FPC et Delphi
    begin
      if Reg.ValueExists(ValueName) then
        Result := Reg.ReadString(ValueName);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;
```

## Ressources et apprentissage continu

### Documentation et références

Les composants non-visuels sont documentés dans :
- L'aide intégrée de Lazarus (F1 sur un composant)
- Le wiki Lazarus officiel
- Les exemples fournis avec Lazarus
- Le code source de la LCL (instructif pour comprendre le fonctionnement)

### Création de vos propres composants non-visuels

```pascal
// Template de base pour un composant non-visuel personnalisé
type
  TMyService = class(TComponent)
  private
    FActive: Boolean;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    procedure SetActive(Value: Boolean);
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
  published
    property Active: Boolean read FActive write SetActive;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

procedure TMyService.SetActive(Value: Boolean);  
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
      DoStart
    else
      DoStop;
  end;
end;

procedure TMyService.DoStart;  
begin
  // Logique de démarrage
  if Assigned(FOnStart) then
    FOnStart(Self);
end;
```

## Conclusion

Les composants non-visuels sont l'épine dorsale de toute application Lazarus sérieuse. Ils permettent de :

- **Structurer la logique** de l'application de manière claire et maintenable
- **Réutiliser du code** grâce à l'encapsulation dans des composants
- **Séparer les préoccupations** entre interface et logique métier
- **Intégrer avec le système** et les services externes
- **Optimiser les performances** en centralisant les services

La maîtrise de ces composants vous permet de passer du statut de créateur d'interfaces à celui de développeur d'applications complètes. Ils sont le pont entre votre code et le monde extérieur : système d'exploitation, réseau, fichiers, et autres applications.

Avec cette base solide, vous êtes maintenant prêt à explorer la création de vos propres composants personnalisés et à construire des applications professionnelles robustes et évolutives !

⏭️ [Création de composants personnalisés portables](/04-framework-lcl/04-creation-composants-personnalises-portables.md)
