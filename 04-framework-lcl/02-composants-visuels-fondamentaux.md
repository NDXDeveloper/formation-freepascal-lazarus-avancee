🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.2 Composants visuels fondamentaux

## Introduction aux composants visuels

Les composants visuels sont les briques de base de toute application graphique. Ce sont les éléments que vos utilisateurs voient et avec lesquels ils interagissent : boutons, zones de texte, listes, images, etc. Dans Lazarus, ces composants sont organisés de manière logique et hiérarchique pour faciliter leur utilisation.

Imaginez que vous construisez une maison : les composants visuels sont comme les portes, fenêtres, interrupteurs et prises électriques. Chacun a un rôle spécifique et des propriétés particulières, mais tous travaillent ensemble pour créer un environnement fonctionnel.

## La hiérarchie des composants

### L'arbre généalogique des composants

Tous les composants visuels de la LCL descendent d'une même famille :

```
TComponent (l'ancêtre de tous)
    └── TControl (peut s'afficher)
            ├── TGraphicControl (léger, sans fenêtre système)
            └── TWinControl (complet, avec fenêtre système)
                    └── (La plupart des composants visuels)
```

**TGraphicControl** : Composants "peints" directement sur leur parent. Ils sont légers mais ne peuvent pas recevoir le focus clavier ni contenir d'autres composants. Exemples : TLabel, TImage, TBevel.

**TWinControl** : Composants avec leur propre "fenêtre" système. Ils peuvent recevoir le focus, contenir d'autres composants et gérer les événements clavier. Exemples : TButton, TEdit, TPanel, TForm.

## Les conteneurs : organiser l'interface

### TForm - La fenêtre principale

TForm est le composant le plus important : c'est la fenêtre de votre application.

**Propriétés essentielles** :
```pascal
// Titre de la fenêtre
Form1.Caption := 'Ma Super Application';

// Taille et position
Form1.Width := 800;
Form1.Height := 600;
Form1.Position := poScreenCenter;  // Centre de l'écran

// Style de bordure
Form1.BorderStyle := bsSizeable;   // Redimensionnable
// Autres options : bsNone, bsSingle, bsDialog

// État de la fenêtre
Form1.WindowState := wsNormal;     // Normal, wsMinimized, wsMaximized
```

**Événements principaux** :
```pascal
// À la création (initialisation)
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialiser vos variables
  // Charger les paramètres
end;

// À l'affichage
procedure TForm1.FormShow(Sender: TObject);
begin
  // Rafraîchir les données
  // Positionner le focus
end;

// À la fermeture
procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Sauvegarder les données
  // Demander confirmation si nécessaire
  if MessageDlg('Vraiment quitter ?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    CloseAction := caNone;  // Annuler la fermeture
end;
```

### TPanel - Le conteneur polyvalent

TPanel est le couteau suisse des conteneurs. Il organise d'autres composants en groupes logiques.

**Utilisation typique** :
```pascal
// Créer une barre d'outils en haut
ToolbarPanel := TPanel.Create(Self);
ToolbarPanel.Parent := Form1;
ToolbarPanel.Align := alTop;
ToolbarPanel.Height := 40;
ToolbarPanel.Caption := '';  // Pas de texte
ToolbarPanel.BevelOuter := bvLowered;

// Zone de contenu principal
ContentPanel := TPanel.Create(Self);
ContentPanel.Parent := Form1;
ContentPanel.Align := alClient;  // Remplit l'espace restant
```

**Astuce** : Les panels peuvent s'imbriquer pour créer des layouts complexes. Pensez-y comme des boîtes dans des boîtes.

### TGroupBox - Regrouper avec un titre

TGroupBox est parfait pour organiser visuellement des options liées.

```pascal
// Groupe d'options de connexion
LoginGroup := TGroupBox.Create(Self);
LoginGroup.Caption := 'Connexion';
LoginGroup.Parent := Form1;
LoginGroup.SetBounds(10, 10, 300, 150);

// Ajouter des contrôles dans le groupe
UserEdit := TEdit.Create(Self);
UserEdit.Parent := LoginGroup;  // Parent = le groupe, pas la form !
UserEdit.Top := 30;
UserEdit.Left := 10;
```

### TPageControl et TTabSheet - Organisation en onglets

Pour les interfaces complexes, les onglets permettent d'organiser le contenu en sections.

```pascal
// Créer le contrôle d'onglets
PageControl1 := TPageControl.Create(Self);
PageControl1.Parent := Form1;
PageControl1.Align := alClient;

// Premier onglet
TabGeneral := TTabSheet.Create(PageControl1);
TabGeneral.PageControl := PageControl1;
TabGeneral.Caption := 'Général';

// Deuxième onglet
TabAdvanced := TTabSheet.Create(PageControl1);
TabAdvanced.PageControl := PageControl1;
TabAdvanced.Caption := 'Avancé';

// Sélectionner un onglet
PageControl1.ActivePage := TabGeneral;
```

### TSplitter - Zones redimensionnables

TSplitter permet à l'utilisateur de redimensionner des zones de l'interface.

```pascal
// Panel de gauche
LeftPanel := TPanel.Create(Self);
LeftPanel.Parent := Form1;
LeftPanel.Align := alLeft;
LeftPanel.Width := 200;

// Splitter (DOIT être créé APRÈS le panel qu'il suit)
Splitter1 := TSplitter.Create(Self);
Splitter1.Parent := Form1;
Splitter1.Align := alLeft;  // Même alignement que le panel
Splitter1.Width := 5;

// Panel de droite
RightPanel := TPanel.Create(Self);
RightPanel.Parent := Form1;
RightPanel.Align := alClient;
```

## Les contrôles de saisie

### TEdit - Saisie de texte simple

Le composant le plus utilisé pour la saisie de texte sur une ligne.

```pascal
// Configuration de base
Edit1 := TEdit.Create(Self);
Edit1.Parent := Form1;
Edit1.Text := 'Texte initial';
Edit1.MaxLength := 50;  // Limite de caractères

// Options utiles
Edit1.CharCase := ecUpperCase;  // Tout en majuscules (ecNormal, ecLowerCase)
Edit1.PasswordChar := '*';      // Masquer le texte
Edit1.ReadOnly := True;         // Lecture seule
Edit1.NumbersOnly := True;      // Que des chiffres

// Événements importants
procedure TForm1.Edit1Change(Sender: TObject);
begin
  // Appelé à chaque modification
  StatusBar1.SimpleText := 'Longueur : ' + IntToStr(Length(Edit1.Text));
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  // Filtrer les caractères
  if Key = #13 then  // Entrée pressée
  begin
    ProcessInput(Edit1.Text);
    Key := #0;  // Annuler le bip
  end;
end;
```

### TMemo - Texte multiligne

Pour éditer plusieurs lignes de texte.

```pascal
// Configuration
Memo1 := TMemo.Create(Self);
Memo1.Parent := Form1;
Memo1.ScrollBars := ssVertical;  // ou ssBoth, ssHorizontal
Memo1.WordWrap := True;           // Retour à la ligne automatique

// Manipuler le texte
Memo1.Lines.Add('Nouvelle ligne');
Memo1.Lines.Insert(0, 'Première ligne');
Memo1.Lines.Delete(2);  // Supprimer la 3e ligne
Memo1.Lines.Clear;      // Tout effacer

// Charger/Sauver
Memo1.Lines.LoadFromFile('document.txt');
Memo1.Lines.SaveToFile('document.txt');

// Sélection et curseur
Memo1.SelStart := 10;   // Position du curseur
Memo1.SelLength := 5;   // Longueur de la sélection
Memo1.SelText := 'Remplacer';  // Remplacer la sélection
```

### TComboBox - Liste déroulante

Combine une zone de texte et une liste déroulante.

```pascal
// Ajouter des éléments
ComboBox1.Items.Add('Option 1');
ComboBox1.Items.Add('Option 2');
ComboBox1.Items.AddStrings(['Option 3', 'Option 4', 'Option 5']);

// Styles
ComboBox1.Style := csDropDownList;  // Liste seulement (pas de saisie)
// csDropDown : saisie + liste
// csSimple : liste toujours visible

// Sélection
ComboBox1.ItemIndex := 0;  // Sélectionner le premier
if ComboBox1.ItemIndex >= 0 then
  ShowMessage('Sélection : ' + ComboBox1.Text);

// Événement de changement
procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0: ConfigureOptionA;
    1: ConfigureOptionB;
  end;
end;
```

### TListBox - Liste de sélection

Pour afficher et sélectionner dans une liste d'éléments.

```pascal
// Configuration
ListBox1.MultiSelect := True;    // Sélection multiple
ListBox1.Sorted := True;         // Tri automatique

// Ajouter avec données associées
ListBox1.Items.AddObject('Item 1', TObject(100));  // 100 = donnée associée

// Parcourir les sélections multiples
for i := 0 to ListBox1.Items.Count - 1 do
  if ListBox1.Selected[i] then
    ProcessItem(ListBox1.Items[i]);

// Double-clic
procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  if ListBox1.ItemIndex >= 0 then
    EditItem(ListBox1.Items[ListBox1.ItemIndex]);
end;
```

### TCheckBox - Case à cocher

Pour les options oui/non.

```pascal
CheckBox1.Caption := 'Activer les notifications';
CheckBox1.Checked := True;

// État tristate (coché/décoché/grisé)
CheckBox1.AllowGrayed := True;
CheckBox1.State := cbGrayed;  // cbUnchecked, cbChecked, cbGrayed

// Réagir au changement
procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  NotificationsEnabled := CheckBox1.Checked;
  UpdateUI;
end;
```

### TRadioButton - Choix exclusif

Pour un choix parmi plusieurs options mutuellement exclusives.

```pascal
// Les RadioButtons sur le même parent s'excluent automatiquement
RadioButton1.Caption := 'Petit';
RadioButton2.Caption := 'Moyen';
RadioButton3.Caption := 'Grand';
RadioButton2.Checked := True;  // Sélection par défaut

// Pour des groupes séparés, utilisez des TGroupBox différents
// Groupe 1 : Taille
RadioSmall.Parent := GroupSize;
RadioLarge.Parent := GroupSize;

// Groupe 2 : Couleur (indépendant du groupe 1)
RadioRed.Parent := GroupColor;
RadioBlue.Parent := GroupColor;
```

## Les contrôles d'action

### TButton - Le bouton standard

Le composant d'action le plus basique et le plus utilisé.

```pascal
Button1.Caption := '&OK';  // & = raccourci Alt+O
Button1.Default := True;   // Bouton par défaut (Entrée)
Button1.Cancel := True;    // Bouton annulation (Échap)

// Personnalisation visuelle
Button1.Font.Style := [fsBold];
Button1.Font.Color := clBlue;

// Avec image
Button1.Glyph.LoadFromFile('icon.bmp');
Button1.Layout := blGlyphLeft;  // Position de l'image

// Action
procedure TForm1.Button1Click(Sender: TObject);
begin
  // Identifier qui a appelé
  if Sender = Button1 then
    ShowMessage('Button1 cliqué');
end;
```

### TBitBtn - Bouton avec image

Version étendue de TButton avec support d'images intégré.

```pascal
BitBtn1.Kind := bkOK;  // Boutons prédéfinis avec icônes
// Autres : bkCancel, bkHelp, bkYes, bkNo, bkClose, bkRetry

// Personnalisé
BitBtn1.Kind := bkCustom;
BitBtn1.Caption := 'Imprimer';
BitBtn1.Glyph.LoadFromFile('printer.png');
```

### TSpeedButton - Bouton pour barres d'outils

Bouton plat, idéal pour les barres d'outils.

```pascal
SpeedButton1.Flat := True;
SpeedButton1.Glyph.LoadFromFile('save.png');
SpeedButton1.ShowHint := True;
SpeedButton1.Hint := 'Sauvegarder le document';

// Groupes de boutons (un seul enfoncé à la fois)
SpeedButton1.GroupIndex := 1;
SpeedButton2.GroupIndex := 1;
SpeedButton3.GroupIndex := 1;
SpeedButton1.Down := True;  // Enfoncé par défaut
```

### TToolBar et TToolButton

Pour créer des barres d'outils professionnelles.

```pascal
// Créer la barre
ToolBar1 := TToolBar.Create(Self);
ToolBar1.Parent := Form1;
ToolBar1.Images := ImageList1;  // Liste d'images pour les boutons

// Ajouter des boutons
btnNew := TToolButton.Create(ToolBar1);
btnNew.Parent := ToolBar1;
btnNew.Caption := 'Nouveau';
btnNew.ImageIndex := 0;
btnNew.OnClick := @NewFileClick;

// Séparateur
btnSep := TToolButton.Create(ToolBar1);
btnSep.Parent := ToolBar1;
btnSep.Style := tbsSeparator;
```

## Les contrôles d'affichage

### TLabel - Afficher du texte

Le composant le plus simple pour afficher du texte.

```pascal
Label1.Caption := 'Nom d''utilisateur :';
Label1.Font.Style := [fsBold];

// Alignement
Label1.Alignment := taCenter;  // taLeftJustify, taRightJustify
Label1.Layout := tlCenter;      // tlTop, tlBottom

// Multi-ligne et retour automatique
Label1.WordWrap := True;
Label1.AutoSize := False;  // Nécessaire pour WordWrap

// Associer à un contrôle (focus avec Alt+N)
Label1.Caption := '&Nom :';
Label1.FocusControl := Edit1;
```

### TStaticText - Texte avec bordure

Similaire à TLabel mais peut avoir une bordure et recevoir des événements souris.

```pascal
StaticText1.BorderStyle := sbsSunken;
StaticText1.Caption := 'Zone d''information';
StaticText1.OnClick := @StaticTextClick;  // Peut réagir au clic
```

### TImage - Afficher des images

Pour afficher des images de différents formats.

```pascal
// Charger une image
Image1.Picture.LoadFromFile('photo.jpg');
// Formats supportés : BMP, JPG, PNG, GIF, ICO, etc.

// Options d'affichage
Image1.Stretch := True;      // Étirer à la taille du composant
Image1.Proportional := True; // Garder les proportions
Image1.Center := True;       // Centrer si plus petit

// Effacer
Image1.Picture.Clear;

// Dessiner dynamiquement
Image1.Canvas.Brush.Color := clYellow;
Image1.Canvas.FillRect(0, 0, 100, 100);
Image1.Canvas.TextOut(10, 10, 'Texte sur image');
```

### TShape - Formes géométriques

Pour dessiner des formes simples.

```pascal
Shape1.Shape := stCircle;
// Autres : stRectangle, stRoundRect, stEllipse, stSquare, stRoundSquare

Shape1.Brush.Color := clBlue;
Shape1.Pen.Color := clRed;
Shape1.Pen.Width := 3;
```

### TBevel - Lignes et cadres

Pour créer des séparations visuelles.

```pascal
Bevel1.Shape := bsFrame;     // Cadre
// Autres : bsBox, bsTopLine, bsBottomLine, bsLeftLine, bsRightLine

Bevel1.Style := bsLowered;   // Enfoncé
// Ou bsRaised pour relief
```

## Les contrôles de progression et de sélection

### TProgressBar - Barre de progression

Pour montrer l'avancement d'une opération.

```pascal
ProgressBar1.Min := 0;
ProgressBar1.Max := 100;
ProgressBar1.Position := 0;

// Style
ProgressBar1.Style := pbstNormal;  // ou pbstMarquee (défilement)
ProgressBar1.Smooth := True;       // Animation fluide

// Mise à jour
for i := 1 to 100 do
begin
  DoWork;
  ProgressBar1.Position := i;
  Application.ProcessMessages;  // Rafraîchir l'affichage
end;
```

### TTrackBar - Curseur de sélection

Pour sélectionner une valeur dans une plage.

```pascal
TrackBar1.Min := 0;
TrackBar1.Max := 255;
TrackBar1.Position := 128;
TrackBar1.Frequency := 10;  // Marques tous les 10
TrackBar1.TickStyle := tsAuto;

// Réagir au changement
procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Label1.Caption := 'Volume : ' + IntToStr(TrackBar1.Position);
  SetVolume(TrackBar1.Position);
end;
```

### TUpDown - Boutons incrément/décrément

Souvent associé à un TEdit pour saisir des nombres.

```pascal
// Association avec un Edit
UpDown1.Associate := Edit1;
UpDown1.Min := 0;
UpDown1.Max := 100;
UpDown1.Position := 50;
UpDown1.Increment := 5;  // Pas de 5

// Sans association
procedure TForm1.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  if Button = btNext then
    IncreaseValue
  else
    DecreaseValue;
end;
```

### TSpinEdit - Saisie numérique avec boutons

Combine TEdit et TUpDown en un seul composant.

```pascal
SpinEdit1.MinValue := 1;
SpinEdit1.MaxValue := 99;
SpinEdit1.Value := 10;
SpinEdit1.Increment := 1;

// Version pour décimaux : TFloatSpinEdit
FloatSpinEdit1.DecimalPlaces := 2;
FloatSpinEdit1.Value := 3.14;
```

## Les contrôles de date et heure

### TDateTimePicker - Sélecteur de date/heure

Pour saisir facilement dates et heures.

```pascal
// Mode date
DateTimePicker1.Kind := dtkDate;
DateTimePicker1.Date := Now;
DateTimePicker1.DateFormat := dfShort;  // ou dfLong
DateTimePicker1.MinDate := EncodeDate(2020, 1, 1);
DateTimePicker1.MaxDate := EncodeDate(2030, 12, 31);

// Mode heure
DateTimePicker2.Kind := dtkTime;
DateTimePicker2.Time := Now;
DateTimePicker2.TimeFormat := tf24;  // ou tf12

// Récupérer la valeur
ShowMessage('Date choisie : ' + DateToStr(DateTimePicker1.Date));
```

### TCalendar - Calendrier mensuel

Affiche un calendrier complet.

```pascal
Calendar1.Date := Date;  // Date d'aujourd'hui
Calendar1.OnChange := @CalendarChange;

procedure TForm1.CalendarChange(Sender: TObject);
begin
  ShowMessage('Date sélectionnée : ' + DateToStr(Calendar1.Date));
end;
```

## Contrôles de données avancés

### TStringGrid - Grille de texte

Pour afficher des données en tableau.

```pascal
// Configuration
StringGrid1.ColCount := 5;
StringGrid1.RowCount := 10;
StringGrid1.FixedCols := 1;  // Première colonne fixe
StringGrid1.FixedRows := 1;  // Première ligne fixe (en-têtes)

// En-têtes
StringGrid1.Cells[1, 0] := 'Nom';
StringGrid1.Cells[2, 0] := 'Prénom';
StringGrid1.Cells[3, 0] := 'Age';

// Données
StringGrid1.Cells[1, 1] := 'Dupont';
StringGrid1.Cells[2, 1] := 'Jean';
StringGrid1.Cells[3, 1] := '25';

// Options utiles
StringGrid1.Options := StringGrid1.Options + [goEditing];  // Édition
StringGrid1.Options := StringGrid1.Options + [goRowSelect]; // Sélection ligne
```

### TListView - Liste avec colonnes

Pour des listes complexes avec plusieurs colonnes et icônes.

```pascal
// Configuration des colonnes
ListView1.ViewStyle := vsReport;
ListView1.Columns.Add.Caption := 'Nom';
ListView1.Columns[0].Width := 150;
ListView1.Columns.Add.Caption := 'Taille';
ListView1.Columns.Add.Caption := 'Date';

// Ajouter des éléments
with ListView1.Items.Add do
begin
  Caption := 'Document.txt';     // Première colonne
  SubItems.Add('12 KB');         // Deuxième colonne
  SubItems.Add('2024-01-15');    // Troisième colonne
  ImageIndex := 0;               // Icône si ImageList associée
end;

// Autres styles de vue
ListView1.ViewStyle := vsIcon;   // Grandes icônes
ListView1.ViewStyle := vsList;   // Liste simple
ListView1.ViewStyle := vsSmallIcon; // Petites icônes
```

### TTreeView - Arborescence

Pour afficher des données hiérarchiques.

```pascal
// Créer la structure
var
  RootNode, ChildNode: TTreeNode;
begin
  RootNode := TreeView1.Items.Add(nil, 'Racine');
  ChildNode := TreeView1.Items.AddChild(RootNode, 'Enfant 1');
  TreeView1.Items.AddChild(ChildNode, 'Sous-enfant');
  TreeView1.Items.AddChild(RootNode, 'Enfant 2');

  // Développer
  RootNode.Expand(True);  // True = récursif

  // Sélection
  TreeView1.Selected := ChildNode;
end;

// Réagir à la sélection
procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if Node <> nil then
    ShowMessage('Sélection : ' + Node.Text);
end;
```

## Propriétés communes importantes

### Positionnement et taille

Tous les contrôles visuels partagent ces propriétés :

```pascal
// Position absolue
Control.Left := 10;
Control.Top := 20;

// Taille
Control.Width := 200;
Control.Height := 30;

// Tout en une fois
Control.SetBounds(10, 20, 200, 30);

// Alignement automatique
Control.Align := alTop;     // Haut de la fenêtre
// alBottom, alLeft, alRight, alClient (remplit), alNone

// Ancrage (suit les bords lors du redimensionnement)
Control.Anchors := [akLeft, akTop, akRight];  // S'étire horizontalement
```

### Visibilité et activation

```pascal
Control.Visible := False;   // Cacher
Control.Enabled := False;   // Désactiver (grisé)
Control.TabStop := True;    // Peut recevoir le focus avec Tab
Control.TabOrder := 0;      // Ordre dans la séquence Tab
```

### Apparence

```pascal
// Police
Control.Font.Name := 'Arial';
Control.Font.Size := 12;
Control.Font.Style := [fsBold, fsItalic];
Control.Font.Color := clBlue;

// Couleurs
Control.Color := clWindow;      // Couleur de fond
Control.ParentColor := True;    // Utiliser la couleur du parent

// Curseur souris
Control.Cursor := crHandPoint;  // Main
// crDefault, crCross, crHelp, crHourGlass, etc.

// Bulles d'aide
Control.ShowHint := True;
Control.Hint := 'Ceci est une aide contextuelle';
```

### Événements universels

La plupart des composants partagent ces événements :

```pascal
// Souris
OnClick         // Clic simple
OnDblClick      // Double-clic
OnMouseEnter    // Souris entre
OnMouseLeave    // Souris sort
OnMouseMove     // Mouvement souris
OnMouseDown/Up  // Bouton pressé/relâché

// Clavier
OnKeyDown       // Touche pressée
OnKeyPress      // Caractère saisi
OnKeyUp         // Touche relâchée

// Focus
OnEnter         // Reçoit le focus
OnExit          // Perd le focus

// Général
OnChange        // Contenu modifié
OnResize        // Taille changée
```

## Conseils pour bien utiliser les composants

### Organisation de l'interface

1. **Utilisez les panels et conteneurs** pour structurer logiquement
2. **Exploitez l'alignement** (Align) pour les layouts flexibles
3. **Utilisez les ancres** (Anchors) pour gérer le redimensionnement
4. **Groupez les contrôles liés** dans des GroupBox ou Panels
5. **Pensez à l'ordre de tabulation** pour la navigation clavier

### Performance et optimisation

```pascal
// Pour de nombreuses modifications, suspendre les mises à jour
ListBox1.Items.BeginUpdate;
try
  for i := 1 to 1000 do
    ListBox1.Items.Add('Item ' + IntToStr(i));
finally
  ListBox1.Items.EndUpdate;
end;

// Désactiver temporairement les événements
OldOnChange := Edit1.OnChange;
Edit1.OnChange := nil;
try
  Edit1.Text := 'Nouvelle valeur';  // Pas d'événement déclenché
finally
  Edit1.OnChange := OldOnChange;
end;
```

### Accessibilité

1. **Toujours définir les propriétés TabStop et TabOrder** pour la navigation clavier
2. **Utiliser les raccourcis** (&) dans les captions
3. **Associer les labels aux contrôles** avec FocusControl
4. **Fournir des hints** pour les actions non évidentes
5. **Respecter les couleurs système** pour les utilisateurs avec thèmes spéciaux

## Création dynamique de composants

Parfois, vous devez créer des composants pendant l'exécution :

```pascal
procedure TForm1.CreateDynamicButton;
var
  NewButton: TButton;
begin
  NewButton := TButton.Create(Self);  // Self = propriétaire (libération auto)
  NewButton.Parent := Panel1;         // Parent = conteneur visuel
  NewButton.Caption := 'Bouton dynamique';
  NewButton.SetBounds(10, 10, 150, 30);
  NewButton.OnClick := @DynamicButtonClick;

  // Stocker la référence si besoin
  FDynamicButtons.Add(NewButton);
end;

procedure TForm1.DynamicButtonClick(Sender: TObject);
begin
  ShowMessage('Bouton dynamique cliqué : ' + TButton(Sender).Caption);
end;
```

## Points clés à retenir

1. **Les composants visuels sont hiérarchiques** - Comprendre TControl vs TWinControl aide à choisir le bon composant
2. **Chaque composant a un rôle spécifique** - Utilisez le bon outil pour chaque tâche
3. **Les propriétés communes** facilitent l'apprentissage - Une fois maîtrisées, elles s'appliquent partout
4. **Les conteneurs organisent l'interface** - TPanel, TGroupBox, TPageControl structurent vos fenêtres
5. **L'alignement et l'ancrage** créent des interfaces flexibles qui s'adaptent
6. **Les événements permettent l'interactivité** - OnClick, OnChange, etc. donnent vie à l'interface
7. **La création dynamique** offre une flexibilité totale quand nécessaire

Ces composants fondamentaux constituent 90% de ce dont vous aurez besoin pour créer des applications complètes et professionnelles. La maîtrise de ces bases vous permettra ensuite d'explorer les composants plus spécialisés avec confiance !

⏭️ [Composants non-visuels et services](/04-framework-lcl/03-composants-non-visuels-services.md)
