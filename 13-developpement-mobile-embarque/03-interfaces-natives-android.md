🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.3 Interfaces natives Android

## Introduction

Les interfaces natives Android représentent l'ensemble des composants visuels et des API système que vous pouvez utiliser pour créer des applications qui s'intègrent parfaitement à l'écosystème Android. Dans cette section, nous explorerons comment utiliser ces interfaces depuis FreePascal avec LAMW pour créer des applications modernes et intuitives.

Une interface native signifie que votre application utilise les mêmes composants visuels et interactions que les applications Android standard, offrant ainsi une expérience utilisateur cohérente et familière.

## Material Design : Le langage visuel d'Android

### Qu'est-ce que Material Design ?

**Material Design** est le système de design développé par Google pour Android. Il définit les principes visuels, les animations, et les interactions qui rendent les applications Android reconnaissables et agréables à utiliser.

**Principes fondamentaux** :
- **Surfaces et ombres** : Les éléments semblent être des feuilles de papier empilées
- **Animations significatives** : Les transitions guident l'utilisateur
- **Hiérarchie visuelle claire** : Importance indiquée par la taille, couleur, position
- **Design responsive** : S'adapte à toutes les tailles d'écran

### Palette de couleurs Material

```
Primary Color (Couleur principale)
├── 500 (Standard)      ← Couleur de base
├── 700 (Dark)          ← Barre de statut
└── 100 (Light)         ← Éléments secondaires

Accent Color (Couleur d'accent)
└── A200 (Standard)     ← Boutons, liens, éléments interactifs
```

**Exemple de configuration** :
```pascal
// Définir les couleurs de l'application
procedure ConfigurerCouleurs;  
begin
    jForm.SetPrimaryColor($FF2196F3);      // Bleu Material 500
    jForm.SetPrimaryDarkColor($FF1976D2);  // Bleu Material 700
    jForm.SetAccentColor($FFFF4081);       // Rose Material A200
end;
```

### Élévation et ombres

Les composants Android ont différents niveaux d'élévation qui créent des ombres :

```
Niveau 0  : Surface de base (fond d'écran)  
Niveau 2  : Cartes au repos (jCardView)  
Niveau 4  : Cartes actives  
Niveau 6  : Snackbar  
Niveau 8  : Menu, boutons flottants au repos  
Niveau 12 : Boutons flottants actifs  
Niveau 16 : Dialogs, navigation drawer  
Niveau 24 : Navigation drawer au-dessus du contenu
```

**Application dans LAMW** :
```pascal
// Définir l'élévation d'une carte
jCardView1.Elevation := 4;  // 4dp d'élévation

// Le composant aura automatiquement des ombres appropriées
```

## Composants d'interface natifs

### Layouts (Conteneurs)

Les layouts organisent les composants visuels à l'écran.

#### LinearLayout (jPanel avec orientation)

Organise les éléments en ligne (horizontal) ou colonne (vertical).

```pascal
type
    TAndroidModule1 = class(jForm)
        jPanel1: jPanel;
        jButton1: jButton;
        jButton2: jButton;
    end;

procedure TAndroidModule1.ConfigurerLayout;  
begin
    // Configuration du panel comme LinearLayout vertical
    jPanel1.LayoutParamWidth := lpMatchParent;
    jPanel1.LayoutParamHeight := lpWrapContent;

    // Ajouter les boutons verticalement
    jButton1.LayoutParamWidth := lpMatchParent;
    jButton1.LayoutParamHeight := lpWrapContent;
    jButton1.MarginTop := 8;

    jButton2.LayoutParamWidth := lpMatchParent;
    jButton2.LayoutParamHeight := lpWrapContent;
    jButton2.MarginTop := 8;
end;
```

**Résultat visuel** :
```
┌────────────────────────┐
│  Panel (LinearLayout)  │
│  ┌──────────────────┐  │
│  │    Bouton 1      │  │
│  └──────────────────┘  │
│         ↓ 8dp          │
│  ┌──────────────────┐  │
│  │    Bouton 2      │  │
│  └──────────────────┘  │
└────────────────────────┘
```

#### RelativeLayout

Positionne les éléments les uns par rapport aux autres.

```pascal
procedure ConfigurerRelativeLayout;  
begin
    // Logo en haut à gauche
    jImageView1.LayoutParamWidth := 64;
    jImageView1.LayoutParamHeight := 64;
    jImageView1.SetId(100);

    // Titre à droite du logo
    jTextView1.LayoutParamWidth := lpWrapContent;
    jTextView1.LayoutParamHeight := lpWrapContent;
    jTextView1.AddLayoutRule(lprToRightOf, 100);  // À droite du logo
    jTextView1.AddLayoutRule(lprCenterVertical);   // Centré verticalement

    // Bouton en bas à droite
    jButton1.LayoutParamWidth := lpWrapContent;
    jButton1.LayoutParamHeight := lpWrapContent;
    jButton1.AddLayoutRule(lprAlignParentBottom);  // En bas
    jButton1.AddLayoutRule(lprAlignParentRight);   // À droite
end;
```

**Résultat visuel** :
```
┌───────────────────────────┐
│ [Logo] Mon Application    │
│                           │
│                           │
│                           │
│              [Bouton OK]  │
└───────────────────────────┘
```

#### ConstraintLayout (Moderne)

Le layout le plus flexible et performant pour des interfaces complexes.

```pascal
procedure ConfigurerConstraintLayout;  
begin
    // Centre l'élément horizontalement et verticalement
    jButton1.AddLayoutRule(lprCenterHorizontal);
    jButton1.AddLayoutRule(lprCenterVertical);

    // Contraintes de largeur
    jButton1.LayoutParamWidth := lpWrapContent;
    jButton1.MinWidth := 200;  // Largeur minimale
end;
```

#### FrameLayout

Empile les éléments les uns sur les autres, utile pour les superpositions.

```pascal
procedure ConfigurerFrameLayout;  
begin
    // Image de fond
    jImageView1.LayoutParamWidth := lpMatchParent;
    jImageView1.LayoutParamHeight := lpMatchParent;

    // Texte superposé
    jTextView1.LayoutParamWidth := lpWrapContent;
    jTextView1.LayoutParamHeight := lpWrapContent;
    jTextView1.AddLayoutRule(lprCenterInParent);
    jTextView1.TextColor := colbrWhite;
    jTextView1.TextSize := 24;
end;
```

### Composants de saisie

#### EditText (jEditText)

Champ de saisie de texte avec de nombreuses options.

```pascal
procedure ConfigurerEditText;  
begin
    // Configuration de base
    jEditText1.Hint := 'Entrez votre email';
    jEditText1.InputTypeEx := itxTextEmailAddress;  // Clavier email

    // Style
    jEditText1.TextSize := 16;
    jEditText1.TextColor := colbrBlack;
    jEditText1.HintColor := colbrGray;

    // Limitations
    jEditText1.MaxLength := 100;
    jEditText1.SingleLine := True;

    // Icône
    jEditText1.SetCompoundDrawables('drawable', 'ic_email', 0, 0, 0);
end;
```

**Types de clavier disponibles** :
```pascal
itxText              // Texte standard  
itxTextEmailAddress  // Clavier avec @ et .  
itxTextPassword      // Masque le texte  
itxNumber            // Numérique  
itxPhone             // Clavier téléphone  
itxTextUri           // URL  
itxTextMultiLine     // Multi-lignes  
itxNumberDecimal     // Nombre décimal
```

**Validation en temps réel** :
```pascal
procedure jEditText1Change(Sender: TObject);  
begin
    if not IsValidEmail(jEditText1.Text) then
    begin
        jEditText1.SetError('Email invalide');
    end
    else
    begin
        jEditText1.SetError('');  // Effacer l'erreur
    end;
end;

function IsValidEmail(const Email: string): Boolean;  
begin
    Result := Pos('@', Email) > 0;  // Validation simplifiée
end;
```

#### TextInputLayout

Améliore EditText avec un label flottant et des messages d'erreur élégants.

```pascal
procedure ConfigurerTextInputLayout;  
begin
    // Créer le layout
    jTextInputLayout1.Hint := 'Adresse email';
    jTextInputLayout1.HintTextColor := colbrGray;

    // Ajouter l'EditText à l'intérieur
    jEditText1.Hint := '';  // Le hint est géré par le layout

    // Afficher une erreur
    jTextInputLayout1.Error := 'Format email incorrect';
    jTextInputLayout1.ErrorEnabled := True;

    // Mode de boîte
    jTextInputLayout1.BoxBackgroundMode := 1;  // Outline
end;
```

**Animation du label** :
```
État initial (vide) :
┌─────────────────────┐
│ Adresse email       │  ← Hint à l'intérieur
└─────────────────────┘

État actif (focus) :
  Adresse email        ← Hint flottant au-dessus
┌─────────────────────┐
│ user@example.com    │  ← Texte saisi
└─────────────────────┘
```

### Composants de sélection

#### CheckBox (jCheckBox)

Case à cocher pour options binaires.

```pascal
procedure ConfigurerCheckBox;  
begin
    jCheckBox1.Text := 'Accepter les conditions';
    jCheckBox1.Checked := False;
    jCheckBox1.TextSize := 14;

    // Personnalisation
    jCheckBox1.ButtonDrawable := 'my_checkbox_selector';
end;

procedure jCheckBox1Click(Sender: TObject);  
begin
    if jCheckBox1.Checked then
        jButton1.Enabled := True  // Activer le bouton de validation
    else
        jButton1.Enabled := False;
end;
```

#### RadioButton (jRadioButton) et RadioGroup

Sélection exclusive parmi plusieurs options.

```pascal
type
    TAndroidModule1 = class(jForm)
        jRadioGroup1: jRadioGroup;
        jRadioButton1: jRadioButton;
        jRadioButton2: jRadioButton;
        jRadioButton3: jRadioButton;
    end;

procedure ConfigurerRadioGroup;  
begin
    // Configuration du groupe
    jRadioGroup1.LayoutParamWidth := lpMatchParent;

    // Options
    jRadioButton1.Text := 'Option 1';
    jRadioButton2.Text := 'Option 2';
    jRadioButton3.Text := 'Option 3';

    // Sélection par défaut
    jRadioButton1.Checked := True;
end;

procedure jRadioGroup1Click(Sender: TObject; checkedId: Integer);  
begin
    case checkedId of
        1: ShowMessage('Option 1 sélectionnée');
        2: ShowMessage('Option 2 sélectionnée');
        3: ShowMessage('Option 3 sélectionnée');
    end;
end;
```

#### Switch (jSwitch) et ToggleButton

Interrupteur on/off moderne.

```pascal
procedure ConfigurerSwitch;  
begin
    jSwitch1.Text := 'Notifications';
    jSwitch1.Checked := True;
    jSwitch1.TextOff := 'Désactivé';
    jSwitch1.TextOn := 'Activé';

    // Couleurs personnalisées
    jSwitch1.ThumbColor := colbrWhite;
    jSwitch1.TrackColor := colbrBlue;
end;

procedure jSwitch1Click(Sender: TObject);  
begin
    if jSwitch1.Checked then
        ActiverNotifications
    else
        DesactiverNotifications;
end;
```

#### Spinner (jSpinner)

Liste déroulante (ComboBox Android).

```pascal
procedure ConfigurerSpinner;  
var
    items: TStringList;
begin
    items := TStringList.Create;
    try
        items.Add('France');
        items.Add('Belgique');
        items.Add('Suisse');
        items.Add('Canada');

        jSpinner1.SetItems(items);
        jSpinner1.SetSelection(0);  // Sélectionner le premier
    finally
        items.Free;
    end;
end;

procedure jSpinner1ItemSelected(Sender: TObject; ItemIndex: Integer;
                                Item: string);
begin
    jTextView1.Text := 'Pays sélectionné : ' + Item;
end;
```

### Composants d'affichage

#### TextView (jTextView)

Affichage de texte avec formatage riche.

```pascal
procedure ConfigurerTextView;  
begin
    // Texte et style
    jTextView1.Text := 'Bienvenue dans l''application';
    jTextView1.TextSize := 18;
    jTextView1.TextColor := colbrBlack;
    jTextView1.TextAlignment := taCenter;

    // Style du texte
    jTextView1.TextStyle := [tsItalic, tsBold];

    // Arrière-plan
    jTextView1.BackgroundColor := colbrLightGray;
    jTextView1.SetRoundCorner(8);  // Coins arrondis

    // Padding interne
    jTextView1.SetPadding(16, 8, 16, 8);

    // Texte multiligne
    jTextView1.MaxLines := 3;
    jTextView1.Ellipsize := teEnd;  // "..." à la fin si trop long
end;
```

**Formatage HTML** :
```pascal
procedure AfficherTexteHTML;  
var
    html: string;
begin
    html := '<b>Gras</b>, <i>Italique</i>, <u>Souligné</u><br>' +
            '<font color="#FF0000">Rouge</font><br>' +
            '<big>Grand</big> <small>Petit</small>';

    jTextView1.SetTextHTML(html);
end;
```

#### ImageView (jImageView)

Affichage d'images avec gestion avancée.

```pascal
procedure ConfigurerImageView;  
begin
    // Charger depuis les ressources
    jImageView1.SetImage('drawable', 'logo');

    // Mode d'affichage
    jImageView1.ScaleType := stCenterCrop;  // Remplit en recadrant

    // Dimensions
    jImageView1.LayoutParamWidth := lpMatchParent;
    jImageView1.LayoutParamHeight := 200;

    // Coins arrondis
    jImageView1.SetRoundCorner(16);
end;
```

**Modes d'affichage (ScaleType)** :
```pascal
stCenter        // Centre sans redimensionner  
stCenterCrop    // Remplit en recadrant (conserve ratio)  
stCenterInside  // Contient complètement (conserve ratio)  
stFitCenter     // Ajuste pour que tout soit visible  
stFitXY         // Étire pour remplir (déforme)
```

**Charger depuis différentes sources** :
```pascal
// Depuis une URL
jImageView1.LoadFromURL('https://example.com/image.jpg');

// Depuis un fichier local
jImageView1.LoadFromFile('/sdcard/Pictures/photo.jpg');

// Depuis les assets
jImageView1.LoadFromAssets('images/splash.png');

// Bitmap créé programmatiquement
jImageView1.SetBitmap(MonBitmap);
```

#### ProgressBar (jProgressBar)

Indicateur de progression.

```pascal
// Barre de progression horizontale
procedure ConfigurerProgressBar;  
begin
    jProgressBar1.Style := pbsHorizontal;
    jProgressBar1.Max := 100;
    jProgressBar1.Progress := 0;

    // Couleurs
    jProgressBar1.ProgressColor := colbrBlue;
    jProgressBar1.ProgressBackgroundColor := colbrLightGray;
end;

// Progression circulaire indéterminée
procedure ConfigurerProgressCirculaire;  
begin
    jProgressBar2.Style := pbsSpinning;
    jProgressBar2.Indeterminate := True;
end;

// Mise à jour de la progression
procedure UpdateProgress(value: Integer);  
begin
    jProgressBar1.Progress := value;
    jTextView1.Text := Format('%d%%', [value]);
end;
```

### Composants de navigation

#### BottomNavigationView

Barre de navigation en bas d'écran (standard Android).

```pascal
type
    TAndroidModule1 = class(jForm)
        jBottomNavigationView1: jBottomNavigationView;
    end;

procedure ConfigurerBottomNavigation;  
begin
    // Ajouter les items
    jBottomNavigationView1.AddItem(0, 'Accueil', 'ic_home');
    jBottomNavigationView1.AddItem(1, 'Recherche', 'ic_search');
    jBottomNavigationView1.AddItem(2, 'Notifications', 'ic_notifications');
    jBottomNavigationView1.AddItem(3, 'Profil', 'ic_profile');

    // Sélectionner l'item par défaut
    jBottomNavigationView1.SetSelectedItem(0);

    // Couleurs
    jBottomNavigationView1.BackgroundColor := colbrWhite;
    jBottomNavigationView1.ItemIconTintColor := colbrBlue;
    jBottomNavigationView1.ItemTextColor := colbrBlue;
end;

procedure jBottomNavigationView1ItemSelected(Sender: TObject; ItemId: Integer);  
begin
    case ItemId of
        0: AfficherAccueil;
        1: AfficherRecherche;
        2: AfficherNotifications;
        3: AfficherProfil;
    end;
end;
```

**Résultat visuel** :
```
┌─────────────────────────────────┐
│                                 │
│      Contenu de la page         │
│                                 │
├─────────────────────────────────┤
│ [🏠]   [🔍]   [🔔]   [👤]        │
│ Accueil Cherche Notifs Profil   │
└─────────────────────────────────┘
```

#### TabLayout avec ViewPager

Navigation par onglets avec pages glissantes.

```pascal
type
    TAndroidModule1 = class(jForm)
        jTabLayout1: jTabLayout;
        jViewPager1: jViewPager;
    end;

procedure ConfigurerTabs;  
begin
    // Ajouter les onglets
    jTabLayout1.AddTab('Récents');
    jTabLayout1.AddTab('Favoris');
    jTabLayout1.AddTab('Tous');

    // Style
    jTabLayout1.TabMode := tmFixed;  // Onglets de taille égale
    jTabLayout1.TabGravity := tgFill;
    jTabLayout1.SelectedTabColor := colbrBlue;
    jTabLayout1.TabTextColor := colbrGray;

    // Lier au ViewPager
    jTabLayout1.SetupWithViewPager(jViewPager1);
end;

procedure jViewPager1PageChanged(Sender: TObject; position: Integer);  
begin
    case position of
        0: ChargerRecents;
        1: ChargerFavoris;
        2: ChargerTous;
    end;
end;
```

#### NavigationDrawer (Menu latéral)

Menu coulissant depuis le bord de l'écran.

```pascal
type
    TAndroidModule1 = class(jForm)
        jDrawerLayout1: jDrawerLayout;
        jNavigationView1: jNavigationView;
    end;

procedure ConfigurerDrawer;  
begin
    // Configuration du drawer
    jDrawerLayout1.DrawerWidth := 280;  // Largeur en dp
    jDrawerLayout1.DrawerGravity := dgLeft;

    // Menu de navigation
    jNavigationView1.AddItem(1, 'Accueil', 'ic_home');
    jNavigationView1.AddItem(2, 'Paramètres', 'ic_settings');
    jNavigationView1.AddDivider;
    jNavigationView1.AddItem(3, 'À propos', 'ic_info');
    jNavigationView1.AddItem(4, 'Déconnexion', 'ic_logout');

    // En-tête du drawer
    jNavigationView1.SetHeaderView('layout', 'drawer_header');
end;

procedure jNavigationView1ItemSelected(Sender: TObject; ItemId: Integer);  
begin
    // Fermer le drawer
    jDrawerLayout1.CloseDrawer;

    // Gérer la sélection
    case ItemId of
        1: AfficherAccueil;
        2: AfficherParametres;
        3: AfficherAPropos;
        4: Deconnexion;
    end;
end;

// Ouvrir/fermer programmatiquement
procedure ToggleDrawer;  
begin
    if jDrawerLayout1.IsDrawerOpen then
        jDrawerLayout1.CloseDrawer
    else
        jDrawerLayout1.OpenDrawer;
end;
```

### Composants de liste

#### ListView (jListView)

Liste déroulante classique.

```pascal
procedure ConfigurerListView;  
var
    items: TStringList;
begin
    items := TStringList.Create;
    try
        items.Add('Item 1');
        items.Add('Item 2');
        items.Add('Item 3');
        items.Add('Item 4');

        jListView1.SetItems(items);
        jListView1.SetItemLayout('layout', 'item_custom');
    finally
        items.Free;
    end;
end;

procedure jListView1ItemClick(Sender: TObject; ItemIndex: Integer;
                              ItemCaption: string);
begin
    ShowMessage('Vous avez sélectionné : ' + ItemCaption);
end;

procedure jListView1ItemLongClick(Sender: TObject; ItemIndex: Integer;
                                  ItemCaption: string);
begin
    // Menu contextuel ou action alternative
    AfficherMenuContextuel(ItemIndex);
end;
```

**Liste avec icônes** :
```pascal
procedure AjouterItemAvecIcone(const texte, icone: string);  
begin
    jListView1.AddItem(texte, icone);
end;

procedure RemplirListeContacts;  
begin
    AjouterItemAvecIcone('Alice Martin', 'ic_contact');
    AjouterItemAvecIcone('Bob Dupont', 'ic_contact');
    AjouterItemAvecIcone('Claire Bernard', 'ic_contact');
end;
```

#### RecyclerView (jRecyclerView)

Liste moderne, performante et flexible.

```pascal
procedure ConfigurerRecyclerView;  
begin
    // Type de layout
    jRecyclerView1.SetLayoutManager(lmLinear);  // Liste verticale
    // Alternatives: lmGrid (grille), lmStaggeredGrid (grille décalée)

    // Optimisations
    jRecyclerView1.HasFixedSize := True;
    jRecyclerView1.SetItemCacheSize(20);

    // Séparateurs
    jRecyclerView1.AddItemDecoration(idDivider);
end;

// Adapter personnalisé
type
    TMonAdapter = class
    private
        FData: TStringList;
    public
        constructor Create(AData: TStringList);
        function GetItemCount: Integer;
        procedure BindViewHolder(position: Integer; holder: TViewHolder);
    end;

procedure UtiliserRecyclerView;  
var
    data: TStringList;
begin
    data := TStringList.Create;
    try
        data.Add('Item 1');
        data.Add('Item 2');
        // ... plus d'items

        jRecyclerView1.SetAdapter(TMonAdapter.Create(data));
    finally
        // data sera libéré par l'adapter
    end;
end;
```

**Gestionnaire de layout en grille** :
```pascal
procedure ConfigurerGrille;  
begin
    jRecyclerView1.SetLayoutManager(lmGrid);
    jRecyclerView1.SetSpanCount(2);  // 2 colonnes
    jRecyclerView1.SetSpacing(8);    // 8dp entre les items
end;
```

### Composants de dialogue

#### AlertDialog

Boîte de dialogue standard.

```pascal
procedure AfficherDialogueSimple;  
begin
    jDialogYN1.Title := 'Confirmation';
    jDialogYN1.Msg := 'Voulez-vous vraiment supprimer cet élément ?';
    jDialogYN1.Yes := 'Supprimer';
    jDialogYN1.No := 'Annuler';
    jDialogYN1.Show;
end;

procedure jDialogYN1Click(Sender: TObject; Button: TDlgResponse);  
begin
    case Button of
        dlgYes: SupprimerElement;
        dlgNo: // Annulé, ne rien faire
    end;
end;
```

**Dialogue avec liste de choix** :
```pascal
procedure AfficherDialogueChoix;  
var
    items: TStringList;
begin
    items := TStringList.Create;
    try
        items.Add('Option 1');
        items.Add('Option 2');
        items.Add('Option 3');

        jDialogChoice1.Title := 'Choisissez une option';
        jDialogChoice1.SetItems(items);
        jDialogChoice1.Show;
    finally
        items.Free;
    end;
end;

procedure jDialogChoice1Click(Sender: TObject; ItemIndex: Integer;
                              ItemCaption: string);
begin
    ShowMessage('Vous avez choisi : ' + ItemCaption);
end;
```

**Dialogue personnalisé** :
```pascal
procedure AfficherDialoguePersonnalise;  
begin
    jDialogCustom1.SetTitle('Connexion');
    jDialogCustom1.SetLayout('layout', 'dialog_login');
    jDialogCustom1.SetPositiveButton('Se connecter');
    jDialogCustom1.SetNegativeButton('Annuler');
    jDialogCustom1.Show;
end;
```

#### BottomSheetDialog

Panneau qui glisse depuis le bas de l'écran.

```pascal
procedure AfficherBottomSheet;  
begin
    jBottomSheet1.SetContentView('layout', 'bottom_sheet_content');
    jBottomSheet1.PeekHeight := 200;  // Hauteur initiale
    jBottomSheet1.Show;
end;

procedure jBottomSheet1StateChanged(Sender: TObject; newState: Integer);  
begin
    case newState of
        bsExpanded: // Complètement étendu
        bsCollapsed: // Réduit à PeekHeight
        bsHidden: // Caché
    end;
end;
```

#### Snackbar

Notification temporaire en bas d'écran (alternative au Toast).

```pascal
procedure AfficherSnackbar;  
begin
    jSnackbar1.Text := 'Élément supprimé';
    jSnackbar1.Duration := sdShort;  // 2 secondes
    jSnackbar1.ActionText := 'ANNULER';
    jSnackbar1.ActionTextColor := colbrYellow;
    jSnackbar1.Show;
end;

procedure jSnackbar1ActionClick(Sender: TObject);  
begin
    // L'utilisateur a cliqué sur "ANNULER"
    RestaurerElement;
end;
```

### Composants d'action

#### FloatingActionButton (FAB)

Bouton d'action flottant, élément signature de Material Design.

```pascal
procedure ConfigurerFAB;  
begin
    jFloatingActionButton1.SetImage('drawable', 'ic_add');
    jFloatingActionButton1.BackgroundColor := colbrBlue;
    jFloatingActionButton1.Size := fsNormal;  // ou fsMini
    jFloatingActionButton1.Elevation := 6;

    // Position
    jFloatingActionButton1.LayoutParamWidth := lpWrapContent;
    jFloatingActionButton1.LayoutParamHeight := lpWrapContent;
    jFloatingActionButton1.AddLayoutRule(lprAlignParentBottom);
    jFloatingActionButton1.AddLayoutRule(lprAlignParentRight);
    jFloatingActionButton1.MarginBottom := 16;
    jFloatingActionButton1.MarginRight := 16;
end;

procedure jFloatingActionButton1Click(Sender: TObject);  
begin
    // Action principale de l'écran
    AjouterNouvelElement;
end;
```

**FAB étendu avec texte** :
```pascal
procedure ConfigurerFABEtendu;  
begin
    jFloatingActionButton1.Extended := True;
    jFloatingActionButton1.Text := 'NOUVEAU';
    jFloatingActionButton1.SetImage('drawable', 'ic_add');
end;
```

#### Toolbar (jToolbar)

Barre d'outils moderne remplaçant l'ActionBar.

```pascal
procedure ConfigurerToolbar;  
begin
    // Configuration de base
    jToolbar1.Title := 'Mon Application';
    jToolbar1.SubTitle := 'Version 1.0';
    jToolbar1.BackgroundColor := colbrBlue;
    jToolbar1.TitleTextColor := colbrWhite;

    // Logo
    jToolbar1.SetLogo('drawable', 'logo');

    // Bouton de navigation (hamburger ou back)
    jToolbar1.SetNavigationIcon('drawable', 'ic_menu');

    // Élévation
    jToolbar1.Elevation := 4;
end;

procedure jToolbar1NavigationClick(Sender: TObject);  
begin
    // Ouvrir le drawer ou retour arrière
    jDrawerLayout1.OpenDrawer;
end;
```

**Menu dans la Toolbar** :
```pascal
procedure ConfigurerMenuToolbar;  
begin
    // Ajouter des items au menu
    jToolbar1.AddMenuItem(1, 'Rechercher', 'ic_search', mitIfRoom);
    jToolbar1.AddMenuItem(2, 'Paramètres', 'ic_settings', mitNever);
    jToolbar1.AddMenuItem(3, 'À propos', '', mitNever);
end;

procedure jToolbar1MenuItemClick(Sender: TObject; MenuItemId: Integer);  
begin
    case MenuItemId of
        1: LancerRecherche;
        2: OuvrirParametres;
        3: AfficherAPropos;
    end;
end;
```

**Options de visibilité des items** :
```pascal
mitAlways     // Toujours visible dans la toolbar  
mitIfRoom     // Visible si place disponible, sinon dans menu overflow  
mitNever      // Toujours dans le menu overflow (trois points)  
mitWithText   // Afficher avec texte si possible
```

#### Menu contextuel

Menu qui apparaît sur un long clic.

```pascal
procedure ConfigurerMenuContextuel;  
begin
    jContextMenu1.AddItem(1, 'Copier');
    jContextMenu1.AddItem(2, 'Coller');
    jContextMenu1.AddItem(3, 'Supprimer');
end;

procedure jListView1ItemLongClick(Sender: TObject; ItemIndex: Integer);  
begin
    jContextMenu1.Show(jListView1, ItemIndex);
end;

procedure jContextMenu1ItemClick(Sender: TObject; MenuItemId: Integer;
                                 ItemIndex: Integer);
begin
    case MenuItemId of
        1: CopierItem(ItemIndex);
        2: CollerItem(ItemIndex);
        3: SupprimerItem(ItemIndex);
    end;
end;
```

### Composants de carte

#### CardView (jCardView)

Carte Material Design avec ombres et élévation.

```pascal
procedure ConfigurerCardView;  
begin
    // Dimensions
    jCardView1.LayoutParamWidth := lpMatchParent;
    jCardView1.LayoutParamHeight := lpWrapContent;

    // Style Material
    jCardView1.Elevation := 4;           // Ombre
    jCardView1.CardBackgroundColor := colbrWhite;
    jCardView1.Radius := 8;              // Coins arrondis

    // Marges externes
    jCardView1.MarginLeft := 16;
    jCardView1.MarginRight := 16;
    jCardView1.MarginTop := 8;
    jCardView1.MarginBottom := 8;

    // Padding interne du contenu
    jCardView1.ContentPaddingLeft := 16;
    jCardView1.ContentPaddingRight := 16;
    jCardView1.ContentPaddingTop := 16;
    jCardView1.ContentPaddingBottom := 16;
end;
```

**Carte cliquable avec effet ripple** :
```pascal
procedure ConfigurerCarteCliquable;  
begin
    jCardView1.Clickable := True;
    jCardView1.Focusable := True;
    jCardView1.SetRippleEffect(True);
    jCardView1.RippleColor := colbrLightGray;
end;

procedure jCardView1Click(Sender: TObject);  
begin
    // Action lors du clic sur la carte
    AfficherDetails;
end;
```

**Structure typique d'une carte** :
```
┌───────────────────────────────┐
│ ┌─────┐                       │
│ │Image│  Titre de la carte    │
│ └─────┘  Sous-titre           │
│                               │
│  Contenu de la carte avec     │
│  du texte descriptif...       │
│                               │
│  [BOUTON 1]    [BOUTON 2]     │
└───────────────────────────────┘
```

### Composants multimédia

#### WebView (jWebView)

Navigateur web intégré.

```pascal
procedure ConfigurerWebView;  
begin
    // Activer JavaScript
    jWebView1.JavaScriptEnabled := True;

    // Paramètres de zoom
    jWebView1.ZoomEnabled := True;
    jWebView1.BuiltInZoomControls := True;
    jWebView1.DisplayZoomControls := False;  // Masquer les boutons +/-

    // Cache et cookies
    jWebView1.CacheMode := cmCacheElseNetwork;
    jWebView1.AcceptCookies := True;

    // Charger une URL
    jWebView1.LoadURL('https://www.example.com');
end;

// Charger du HTML local
procedure ChargerHTMLLocal;  
var
    html: string;
begin
    html := '<html><body>' +
            '<h1>Bienvenue</h1>' +
            '<p>Contenu HTML local</p>' +
            '</body></html>';

    jWebView1.LoadHTML(html);
end;

// Charger depuis les assets
procedure ChargerDepuisAssets;  
begin
    jWebView1.LoadFromAssets('pages/index.html');
end;

// Événements
procedure jWebView1PageStarted(Sender: TObject; url: string);  
begin
    jProgressBar1.Visible := True;
    jProgressBar1.Indeterminate := True;
end;

procedure jWebView1PageFinished(Sender: TObject; url: string);  
begin
    jProgressBar1.Visible := False;
end;

procedure jWebView1Error(Sender: TObject; errorCode: Integer;
                         description, failingUrl: string);
begin
    ShowMessage('Erreur de chargement : ' + description);
end;
```

**Communication JavaScript ↔ Pascal** :
```pascal
// Appeler JavaScript depuis Pascal
procedure AppelerJavaScript;  
begin
    jWebView1.EvaluateJavaScript('alert("Hello from Pascal!")');
    jWebView1.EvaluateJavaScript('document.getElementById("titre").innerHTML = "Nouveau titre"');
end;

// Exposer une fonction Pascal à JavaScript
procedure ExposerFonctionPascal;  
begin
    jWebView1.AddJavaScriptInterface('MyApp', 'showToast');
end;

// Fonction appelable depuis JavaScript
procedure OnJavaScriptCall(const functionName, params: string);  
begin
    if functionName = 'showToast' then
        ShowMessage(params);
end;
```

#### VideoView (jVideoView)

Lecteur vidéo intégré.

```pascal
procedure ConfigurerVideoView;  
begin
    // Chemin de la vidéo
    jVideoView1.SetVideoPath('/sdcard/Movies/video.mp4');

    // Ou depuis une URL
    jVideoView1.SetVideoURL('https://example.com/video.mp4');

    // Contrôles de lecture
    jVideoView1.ShowControls := True;

    // Dimensions
    jVideoView1.LayoutParamWidth := lpMatchParent;
    jVideoView1.LayoutParamHeight := 300;
end;

procedure jButton1Click(Sender: TObject);  
begin
    jVideoView1.Start;  // Démarrer la lecture
end;

procedure jButton2Click(Sender: TObject);  
begin
    jVideoView1.Pause;  // Mettre en pause
end;

procedure jButton3Click(Sender: TObject);  
begin
    jVideoView1.Stop;   // Arrêter
end;

// Événements
procedure jVideoView1Prepared(Sender: TObject);  
begin
    // Vidéo prête, récupérer la durée
    jTextView1.Text := Format('Durée: %d secondes',
                              [jVideoView1.Duration div 1000]);
end;

procedure jVideoView1Completion(Sender: TObject);  
begin
    // Lecture terminée
    ShowMessage('Vidéo terminée');
end;
```

#### MediaPlayer (jMediaPlayer)

Lecteur audio avancé.

```pascal
type
    TAndroidModule1 = class(jForm)
        jMediaPlayer1: jMediaPlayer;
    end;

procedure ConfigurerMediaPlayer;  
begin
    // Charger depuis les assets
    jMediaPlayer1.LoadFromAssets('audio/music.mp3');

    // Configuration
    jMediaPlayer1.Looping := False;  // Ne pas boucler
    jMediaPlayer1.Volume := 0.8;     // Volume 80%
end;

procedure jButton1Click(Sender: TObject);  
begin
    if not jMediaPlayer1.IsPlaying then
        jMediaPlayer1.Play
    else
        jMediaPlayer1.Pause;
end;

procedure jSeekBar1ProgressChanged(Sender: TObject; progress: Integer);  
begin
    // Chercher dans la piste
    jMediaPlayer1.SeekTo(progress);
end;

// Mise à jour de la progression
procedure UpdateProgress;  
var
    position, duration: Integer;
begin
    if jMediaPlayer1.IsPlaying then
    begin
        position := jMediaPlayer1.CurrentPosition;
        duration := jMediaPlayer1.Duration;

        jSeekBar1.Max := duration;
        jSeekBar1.Progress := position;

        jTextView1.Text := FormatTime(position) + ' / ' + FormatTime(duration);
    end;
end;

function FormatTime(milliseconds: Integer): string;  
var
    seconds, minutes: Integer;
begin
    seconds := milliseconds div 1000;
    minutes := seconds div 60;
    seconds := seconds mod 60;
    Result := Format('%d:%2.2d', [minutes, seconds]);
end;
```

### Composants de dessin

#### Canvas (jCanvas)

Surface de dessin personnalisé.

```pascal
type
    TAndroidModule1 = class(jForm)
        jCanvas1: jCanvas;
    end;

procedure ConfigurerCanvas;  
begin
    jCanvas1.LayoutParamWidth := lpMatchParent;
    jCanvas1.LayoutParamHeight := 400;
    jCanvas1.BackgroundColor := colbrWhite;
end;

procedure jCanvas1Draw(Sender: TObject; Canvas: TObject);  
begin
    // Dessiner un rectangle
    jCanvas1.DrawRect(50, 50, 200, 150, colbrBlue);

    // Dessiner un cercle
    jCanvas1.DrawCircle(300, 100, 50, colbrRed);

    // Dessiner une ligne
    jCanvas1.DrawLine(50, 200, 350, 200, colbrBlack, 5);

    // Dessiner du texte
    jCanvas1.DrawText('Hello Canvas!', 100, 250, colbrBlack, 20);
end;

// Dessiner un chemin complexe
procedure DessinerChemin;  
begin
    jCanvas1.BeginPath;
    jCanvas1.MoveTo(100, 300);
    jCanvas1.LineTo(150, 250);
    jCanvas1.LineTo(200, 300);
    jCanvas1.LineTo(250, 250);
    jCanvas1.ClosePath;
    jCanvas1.StrokePath(colbrGreen, 3);
end;

// Dessin interactif
procedure jCanvas1TouchDown(Sender: TObject; x, y: Single);  
begin
    // Commencer un nouveau trait
    FLastX := x;
    FLastY := y;
end;

procedure jCanvas1TouchMove(Sender: TObject; x, y: Single);  
begin
    // Dessiner en suivant le doigt
    jCanvas1.DrawLine(FLastX, FLastY, x, y, colbrBlack, 3);
    FLastX := x;
    FLastY := y;
    jCanvas1.Invalidate;  // Rafraîchir l'affichage
end;
```

### Composants de notification

#### Notification Manager

Créer et afficher des notifications système.

```pascal
procedure AfficherNotification;  
begin
    jNotificationManager1.Title := 'Nouveau message';
    jNotificationManager1.Message := 'Vous avez reçu un nouveau message';
    jNotificationManager1.SmallIcon := 'ic_notification';
    jNotificationManager1.LargeIcon := 'ic_launcher';

    // Couleur d'accent (Android 5.0+)
    jNotificationManager1.Color := colbrBlue;

    // Son et vibration
    jNotificationManager1.Sound := True;
    jNotificationManager1.Vibrate := True;

    // Action au clic
    jNotificationManager1.AutoCancel := True;  // Se ferme au clic

    // Afficher
    jNotificationManager1.Notify(1);  // ID unique de notification
end;
```

**Notification avec actions** :
```pascal
procedure NotificationAvecActions;  
begin
    jNotificationManager1.Title := 'Appel entrant';
    jNotificationManager1.Message := 'Alice Martin';

    // Ajouter des boutons d'action
    jNotificationManager1.AddAction(1, 'Répondre', 'ic_call');
    jNotificationManager1.AddAction(2, 'Rejeter', 'ic_call_end');

    jNotificationManager1.Notify(2);
end;

procedure jNotificationManager1ActionClick(Sender: TObject; ActionId: Integer);  
begin
    case ActionId of
        1: RepondreAppel;
        2: RejeterAppel;
    end;
end;
```

**Notification avec progression** :
```pascal
procedure NotificationProgression;  
var
    i: Integer;
begin
    for i := 0 to 100 do
    begin
        jNotificationManager1.Title := 'Téléchargement';
        jNotificationManager1.Message := Format('%d%%', [i]);
        jNotificationManager1.Progress := i;
        jNotificationManager1.MaxProgress := 100;
        jNotificationManager1.Indeterminate := False;
        jNotificationManager1.Notify(3);

        Sleep(100);  // Simulation
    end;

    // Notification de fin
    jNotificationManager1.Message := 'Téléchargement terminé';
    jNotificationManager1.Progress := 0;  // Masquer la barre
    jNotificationManager1.Notify(3);
end;
```

**Canaux de notification (Android 8.0+)** :
```pascal
procedure CreerCanaux;  
begin
    // Canal pour messages
    jNotificationManager1.CreateChannel(
        'messages',                    // ID
        'Messages',                    // Nom
        'Notifications de messages',   // Description
        ncImportanceHigh              // Importance
    );

    // Canal pour mises à jour
    jNotificationManager1.CreateChannel(
        'updates',
        'Mises à jour',
        'Notifications de mise à jour',
        ncImportanceLow
    );
end;

procedure UtiliserCanal;  
begin
    jNotificationManager1.ChannelId := 'messages';
    jNotificationManager1.Title := 'Nouveau message';
    jNotificationManager1.Notify(4);
end;
```

## Gestion des permissions

### Permissions dangereuses

Certaines permissions nécessitent l'accord explicite de l'utilisateur à l'exécution (Android 6.0+).

**Permissions dangereuses courantes** :
- Localisation (ACCESS_FINE_LOCATION, ACCESS_COARSE_LOCATION)
- Caméra (CAMERA)
- Microphone (RECORD_AUDIO)
- Stockage (READ_EXTERNAL_STORAGE, WRITE_EXTERNAL_STORAGE)
- Contacts (READ_CONTACTS, WRITE_CONTACTS)
- Calendrier (READ_CALENDAR, WRITE_CALENDAR)
- Téléphone (CALL_PHONE, READ_PHONE_STATE)
- SMS (SEND_SMS, READ_SMS)

### Demander une permission

```pascal
procedure DemanderPermissionCamera;  
begin
    if jForm.HasPermission('android.permission.CAMERA') then
    begin
        // Permission déjà accordée
        OuvrirCamera;
    end
    else
    begin
        // Demander la permission
        jForm.RequestPermission('android.permission.CAMERA', 100);
    end;
end;

procedure OnPermissionResult(Permission: string; GrantResult: TPermissionResult);  
begin
    case GrantResult of
        prGranted:
        begin
            // Permission accordée
            ShowMessage('Permission accordée');
            OuvrirCamera;
        end;

        prDenied:
        begin
            // Permission refusée
            ShowMessage('Permission refusée. La caméra ne peut pas être utilisée.');
        end;

        prDeniedDontAskAgain:
        begin
            // Refusée avec "Ne plus demander"
            ShowMessage('Veuillez activer la permission dans les paramètres');
            OuvrirParametresApplication;
        end;
    end;
end;
```

### Demander plusieurs permissions

```pascal
procedure DemanderPermissionsMultiples;  
var
    permissions: TStringList;
begin
    permissions := TStringList.Create;
    try
        permissions.Add('android.permission.CAMERA');
        permissions.Add('android.permission.RECORD_AUDIO');
        permissions.Add('android.permission.WRITE_EXTERNAL_STORAGE');

        jForm.RequestPermissions(permissions, 200);
    finally
        permissions.Free;
    end;
end;

procedure OnMultiplePermissionsResult(Permissions: TStringList;
                                      Results: TPermissionResults);
var
    i: Integer;
    allGranted: Boolean;
begin
    allGranted := True;

    for i := 0 to Permissions.Count - 1 do
    begin
        if Results[i] <> prGranted then
        begin
            allGranted := False;
            Break;
        end;
    end;

    if allGranted then
        LancerEnregistrementVideo
    else
        ShowMessage('Toutes les permissions sont nécessaires');
end;
```

### Expliquer pourquoi la permission est nécessaire

```pascal
procedure DemanderPermissionAvecExplication;  
begin
    if jForm.ShouldShowRequestPermissionRationale('android.permission.CAMERA') then
    begin
        // L'utilisateur a déjà refusé, expliquer pourquoi c'est nécessaire
        jDialogYN1.Title := 'Permission caméra';
        jDialogYN1.Msg := 'Cette application a besoin d''accéder à la caméra ' +
                         'pour scanner les codes QR. Sans cette permission, ' +
                         'la fonctionnalité de scan ne sera pas disponible.';
        jDialogYN1.Yes := 'Autoriser';
        jDialogYN1.No := 'Refuser';
        jDialogYN1.Show;
    end
    else
    begin
        // Première demande, demander directement
        jForm.RequestPermission('android.permission.CAMERA', 100);
    end;
end;

procedure jDialogYN1Click(Sender: TObject; Button: TDlgResponse);  
begin
    if Button = dlgYes then
        jForm.RequestPermission('android.permission.CAMERA', 100);
end;
```

## Capteurs et matériel

### Géolocalisation

```pascal
type
    TAndroidModule1 = class(jForm)
    private
        FLocationEnabled: Boolean;
    end;

procedure DemarrerLocalisation;  
begin
    if not jForm.HasPermission('android.permission.ACCESS_FINE_LOCATION') then
    begin
        jForm.RequestPermission('android.permission.ACCESS_FINE_LOCATION', 300);
        Exit;
    end;

    // Intervalle de mise à jour en millisecondes
    jForm.StartLocationUpdates(5000);  // Toutes les 5 secondes
    FLocationEnabled := True;
end;

procedure ArreterLocalisation;  
begin
    jForm.StopLocationUpdates;
    FLocationEnabled := False;
end;

procedure OnLocationChanged(Latitude, Longitude: Double; Accuracy: Single);  
begin
    jTextView1.Text := Format('Lat: %.6f, Lon: %.6f', [Latitude, Longitude]);
    jTextView2.Text := Format('Précision: %.0f mètres', [Accuracy]);

    // Afficher sur une carte (exemple conceptuel)
    AfficherSurCarte(Latitude, Longitude);
end;
```

### Capteurs de mouvement

```pascal
procedure DemarrerCapteurs;  
begin
    // Démarrer l'accéléromètre
    jForm.StartSensor(stAccelerometer);

    // Démarrer le gyroscope
    jForm.StartSensor(stGyroscope);

    // Démarrer le magnétomètre
    jForm.StartSensor(stMagnetometer);
end;

procedure ArreterCapteurs;  
begin
    jForm.StopSensor(stAccelerometer);
    jForm.StopSensor(stGyroscope);
    jForm.StopSensor(stMagnetometer);
end;

procedure OnSensorChanged(SensorType: TSensorType; X, Y, Z: Single);  
begin
    case SensorType of
        stAccelerometer:
        begin
            // Détection de secousse
            if (Abs(X) > 15) or (Abs(Y) > 15) or (Abs(Z) > 15) then
                OnSecousse;

            jTextViewAccel.Text := Format('Accél: X=%.2f Y=%.2f Z=%.2f',
                                         [X, Y, Z]);
        end;

        stGyroscope:
        begin
            // Rotation de l'appareil
            jTextViewGyro.Text := Format('Gyro: X=%.2f Y=%.2f Z=%.2f',
                                        [X, Y, Z]);
        end;

        stMagnetometer:
        begin
            // Boussole
            CalculerOrientation(X, Y, Z);
        end;
    end;
end;

procedure CalculerOrientation(X, Y, Z: Single);  
var
    angle: Single;
begin
    angle := ArcTan2(Y, X) * 180 / Pi;
    if angle < 0 then
        angle := angle + 360;

    jTextViewCompass.Text := Format('Direction: %.0f°', [angle]);
end;
```

### Caméra

```pascal
procedure OuvrirCamera;  
begin
    if not jForm.HasPermission('android.permission.CAMERA') then
    begin
        jForm.RequestPermission('android.permission.CAMERA', 400);
        Exit;
    end;

    // Prendre une photo
    jForm.TakePicture('photo.jpg');
end;

procedure OnActivityResult(RequestCode, ResultCode: Integer; Data: jObject);  
begin
    if (RequestCode = 100) and (ResultCode = RESULT_OK) then
    begin
        // Photo prise avec succès
        jImageView1.LoadFromFile(jForm.GetExternalStoragePath + '/photo.jpg');
    end;
end;
```

**Caméra personnalisée** :
```pascal
type
    TAndroidModule1 = class(jForm)
        jCameraView1: jCameraView;
    end;

procedure ConfigurerCameraPersonnalisee;  
begin
    jCameraView1.LayoutParamWidth := lpMatchParent;
    jCameraView1.LayoutParamHeight := 400;
    jCameraView1.CameraFacing := cfBack;  // Caméra arrière
end;

procedure jButton1Click(Sender: TObject);  
begin
    // Capturer l'image
    jCameraView1.TakePicture;
end;

procedure jCameraView1PictureTaken(Sender: TObject; const FilePath: string);  
begin
    ShowMessage('Photo sauvegardée : ' + FilePath);
    jImageView1.LoadFromFile(FilePath);
end;

procedure jButton2Click(Sender: TObject);  
begin
    // Basculer entre caméra avant/arrière
    if jCameraView1.CameraFacing = cfBack then
        jCameraView1.CameraFacing := cfFront
    else
        jCameraView1.CameraFacing := cfBack;
end;
```

### Scanner de codes-barres / QR codes

```pascal
procedure ScannerCodeQR;  
begin
    if not jForm.HasPermission('android.permission.CAMERA') then
    begin
        jForm.RequestPermission('android.permission.CAMERA', 500);
        Exit;
    end;

    jBarcodeScanner1.Scan;
end;

procedure jBarcodeScanner1Result(Sender: TObject; const BarcodeData: string;
                                 BarcodeType: TBarcodeType);
begin
    case BarcodeType of
        btQRCode: jTextView1.Text := 'QR Code: ' + BarcodeData;
        btEAN13: jTextView1.Text := 'Code-barres EAN13: ' + BarcodeData;
        btCode128: jTextView1.Text := 'Code 128: ' + BarcodeData;
    else
        jTextView1.Text := 'Code scanné: ' + BarcodeData;
    end;

    // Traiter les données
    TraiterCodeScanne(BarcodeData, BarcodeType);
end;
```

### Vibration

```pascal
procedure FaireVibrer;  
begin
    if jForm.HasPermission('android.permission.VIBRATE') then
    begin
        // Vibration simple (durée en millisecondes)
        jForm.Vibrate(500);  // 500ms
    end;
end;

procedure VibrationPattern;  
var
    pattern: array[0..5] of Integer;
begin
    // Pattern: [attente, vibration, attente, vibration, ...]
    pattern[0] := 0;     // Démarrer immédiatement
    pattern[1] := 200;   // Vibrer 200ms
    pattern[2] := 100;   // Pause 100ms
    pattern[3] := 200;   // Vibrer 200ms
    pattern[4] := 100;   // Pause 100ms
    pattern[5] := 200;   // Vibrer 200ms

    jForm.VibratePattern(pattern, -1);  // -1 = ne pas répéter
end;
```

### Flash / Lampe torche

```pascal
procedure AllumerFlash;  
begin
    if jForm.HasCameraFlash then
    begin
        jForm.SetFlashMode(fmTorch);  // Mode lampe torche
    end
    else
    begin
        ShowMessage('Pas de flash disponible');
    end;
end;

procedure EteindreFlash;  
begin
    jForm.SetFlashMode(fmOff);
end;

procedure BasculerFlash;  
begin
    if jForm.IsFlashOn then
        EteindreFlash
    else
        AllumerFlash;
end;
```

## Stockage et fichiers

### Chemins de stockage

```pascal
function ObtenirChemins: string;  
var
    info: string;
begin
    // Stockage interne de l'app (privé)
    info := 'Interne: ' + jForm.GetInternalStoragePath + #13#10;

    // Stockage externe de l'app (privé mais accessible)
    info := info + 'Externe App: ' + jForm.GetExternalStoragePath + #13#10;

    // Stockage public (nécessite permission)
    info := info + 'Public: ' + jForm.GetPublicStoragePath + #13#10;

    // Dossiers spéciaux
    info := info + 'Documents: ' + jForm.GetDocumentsPath + #13#10;
    info := info + 'Pictures: ' + jForm.GetPicturesPath + #13#10;
    info := info + 'Downloads: ' + jForm.GetDownloadsPath + #13#10;

    Result := info;
end;
```

### Lire et écrire des fichiers

```pascal
procedure SauvegarderFichier;  
var
    content: string;
    filePath: string;
begin
    content := jEditText1.Text;
    filePath := jForm.GetInternalStoragePath + '/data.txt';

    try
        jForm.SaveTextToFile(filePath, content);
        ShowMessage('Fichier sauvegardé');
    except
        on E: Exception do
            ShowMessage('Erreur: ' + E.Message);
    end;
end;

procedure ChargerFichier;  
var
    content: string;
    filePath: string;
begin
    filePath := jForm.GetInternalStoragePath + '/data.txt';

    if jForm.FileExists(filePath) then
    begin
        content := jForm.LoadTextFromFile(filePath);
        jEditText1.Text := content;
    end
    else
    begin
        ShowMessage('Fichier non trouvé');
    end;
end;

procedure ListerFichiers;  
var
    files: TStringList;
    i: Integer;
begin
    files := TStringList.Create;
    try
        jForm.ListFiles(jForm.GetInternalStoragePath, files);

        jListView1.Clear;
        for i := 0 to files.Count - 1 do
            jListView1.AddItem(files[i], 'ic_file');
    finally
        files.Free;
    end;
end;
```

### Sélecteur de fichiers

```pascal
procedure OuvrirSelecteurFichier;  
begin
    jFilePicker1.FileType := 'image/*';  // Seulement les images
    jFilePicker1.Show;
end;

procedure jFilePicker1Result(Sender: TObject; const FilePath: string);  
begin
    if FilePath <> '' then
    begin
        jTextView1.Text := 'Fichier sélectionné : ' + FilePath;

        // Charger l'image
        jImageView1.LoadFromFile(FilePath);
    end
    else
    begin
        ShowMessage('Aucun fichier sélectionné');
    end;
end;

// Types de fichiers courants
procedure SelectionnerTypeFichier;  
begin
    // Images
    jFilePicker1.FileType := 'image/*';

    // Vidéos
    jFilePicker1.FileType := 'video/*';

    // Audio
    jFilePicker1.FileType := 'audio/*';

    // PDF
    jFilePicker1.FileType := 'application/pdf';

    // Tous les fichiers
    jFilePicker1.FileType := '*/*';

    jFilePicker1.Show;
end;
```

### SharedPreferences (Préférences)

Stockage léger de paires clé-valeur.

```pascal
procedure SauvegarderPreferences;  
begin
    jForm.SavePreference('username', jEditText1.Text);
    jForm.SavePreference('email', jEditText2.Text);
    jForm.SavePreference('age', 25);
    jForm.SavePreference('premium', True);
    ShowMessage('Préférences sauvegardées');
end;

procedure ChargerPreferences;  
var
    username, email: string;
    age: Integer;
    premium: Boolean;
begin
    username := jForm.LoadPreference('username', 'Invité');
    email := jForm.LoadPreference('email', '');
    age := jForm.LoadPreferenceInt('age', 0);
    premium := jForm.LoadPreferenceBool('premium', False);

    jEditText1.Text := username;
    jEditText2.Text := email;
    jTextView1.Text := Format('Âge: %d, Premium: %s',
                              [age, BoolToStr(premium, True)]);
end;

procedure EffacerPreferences;  
begin
    jForm.ClearPreferences;
    ShowMessage('Toutes les préférences effacées');
end;

procedure EffacerPreferenceSpecifique;  
begin
    jForm.DeletePreference('username');
end;
```

### Base de données SQLite

```pascal
type
    TAndroidModule1 = class(jForm)
        jSQLite1: jSQLite;
    end;

procedure CreerBaseDeDonnees;  
begin
    // Créer/ouvrir la base de données
    jSQLite1.DatabaseName := 'myapp.db';
    jSQLite1.Open;

    // Créer les tables
    jSQLite1.ExecuteSQL(
        'CREATE TABLE IF NOT EXISTS users (' +
        '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
        '  name TEXT NOT NULL,' +
        '  email TEXT UNIQUE,' +
        '  created_at DATETIME DEFAULT CURRENT_TIMESTAMP' +
        ')'
    );

    jSQLite1.ExecuteSQL(
        'CREATE TABLE IF NOT EXISTS messages (' +
        '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
        '  user_id INTEGER,' +
        '  content TEXT,' +
        '  FOREIGN KEY (user_id) REFERENCES users(id)' +
        ')'
    );
end;

procedure InsererDonnees;  
begin
    // Insertion simple
    jSQLite1.ExecuteSQL(
        'INSERT INTO users (name, email) VALUES (''Alice'', ''alice@example.com'')'
    );

    // Insertion avec paramètres (prévient les injections SQL)
    jSQLite1.BeginTransaction;
    try
        jSQLite1.PrepareSQL('INSERT INTO users (name, email) VALUES (?, ?)');
        jSQLite1.BindText(1, 'Bob');
        jSQLite1.BindText(2, 'bob@example.com');
        jSQLite1.ExecutePrepared;

        jSQLite1.PrepareSQL('INSERT INTO users (name, email) VALUES (?, ?)');
        jSQLite1.BindText(1, 'Charlie');
        jSQLite1.BindText(2, 'charlie@example.com');
        jSQLite1.ExecutePrepared;

        jSQLite1.Commit;
        ShowMessage('Utilisateurs ajoutés');
    except
        jSQLite1.Rollback;
        ShowMessage('Erreur lors de l''insertion');
    end;
end;

procedure LireDonnees;  
var
    cursor: jCursor;
    users: TStringList;
begin
    users := TStringList.Create;
    try
        cursor := jSQLite1.ExecuteQuery('SELECT id, name, email FROM users');

        while cursor.MoveToNext do
        begin
            users.Add(Format('ID: %d, Nom: %s, Email: %s',
                [cursor.GetInt(0), cursor.GetString(1), cursor.GetString(2)]));
        end;

        cursor.Close;

        jListView1.SetItems(users);
    finally
        users.Free;
    end;
end;

procedure RechercherDonnees;  
var
    cursor: jCursor;
    searchTerm: string;
begin
    searchTerm := jEditText1.Text;

    jSQLite1.PrepareSQL('SELECT * FROM users WHERE name LIKE ?');
    jSQLite1.BindText(1, '%' + searchTerm + '%');

    cursor := jSQLite1.ExecuteQueryPrepared;
    try
        if cursor.GetCount > 0 then
        begin
            while cursor.MoveToNext do
            begin
                jTextView1.Text := cursor.GetString(1);  // name
                // Afficher les résultats
            end;
        end
        else
            ShowMessage('Aucun résultat');
    finally
        cursor.Close;
    end;
end;

procedure MettreAJourDonnees;  
begin
    jSQLite1.PrepareSQL('UPDATE users SET email = ? WHERE id = ?');
    jSQLite1.BindText(1, 'newemail@example.com');
    jSQLite1.BindInt(2, 1);
    jSQLite1.ExecutePrepared;

    ShowMessage(Format('%d ligne(s) modifiée(s)', [jSQLite1.RowsAffected]));
end;

procedure SupprimerDonnees;  
begin
    jSQLite1.PrepareSQL('DELETE FROM users WHERE id = ?');
    jSQLite1.BindInt(1, 1);
    jSQLite1.ExecutePrepared;

    ShowMessage('Utilisateur supprimé');
end;
```

## Communication réseau

### HttpClient (Requêtes HTTP)

```pascal
type
    TAndroidModule1 = class(jForm)
        jHttpClient1: jHttpClient;
    end;

// Requête GET simple
procedure RequeteGET;  
begin
    jHttpClient1.URL := 'https://api.example.com/users';
    jHttpClient1.Method := hmGET;
    jHttpClient1.Execute;
end;

procedure jHttpClient1Response(Sender: TObject; StatusCode: Integer;
                               Response: string);
begin
    if StatusCode = 200 then
    begin
        // Succès
        jTextView1.Text := Response;
        TraiterReponseJSON(Response);
    end
    else
    begin
        ShowMessage('Erreur HTTP: ' + IntToStr(StatusCode));
    end;
end;

procedure jHttpClient1Error(Sender: TObject; ErrorMsg: string);  
begin
    ShowMessage('Erreur réseau: ' + ErrorMsg);
end;

// Requête POST avec données
procedure RequetePOST;  
var
    jsonData: string;
begin
    jsonData := '{"name": "Alice", "email": "alice@example.com"}';

    jHttpClient1.URL := 'https://api.example.com/users';
    jHttpClient1.Method := hmPOST;
    jHttpClient1.AddHeader('Content-Type', 'application/json');
    jHttpClient1.PostData := jsonData;
    jHttpClient1.Execute;
end;

// Upload de fichier
procedure UploadFichier;  
var
    filePath: string;
begin
    filePath := jForm.GetInternalStoragePath + '/photo.jpg';

    jHttpClient1.URL := 'https://api.example.com/upload';
    jHttpClient1.Method := hmPOST;
    jHttpClient1.AddFile('file', filePath);
    jHttpClient1.Execute;
end;

// Téléchargement de fichier
procedure TelechargerFichier;  
var
    savePath: string;
begin
    savePath := jForm.GetDownloadsPath + '/downloaded_file.pdf';

    jHttpClient1.URL := 'https://example.com/document.pdf';
    jHttpClient1.Method := hmGET;
    jHttpClient1.DownloadTo := savePath;
    jHttpClient1.Execute;
end;

procedure jHttpClient1DownloadProgress(Sender: TObject;
                                       BytesReceived, TotalBytes: Int64);
var
    progress: Integer;
begin
    if TotalBytes > 0 then
    begin
        progress := Round((BytesReceived / TotalBytes) * 100);
        jProgressBar1.Progress := progress;
        jTextView1.Text := Format('Téléchargement: %d%%', [progress]);
    end;
end;
```

### Traitement JSON

```pascal
uses
    fpjson, jsonparser;

procedure TraiterReponseJSON(const jsonString: string);  
var
    jsonData: TJSONData;
    jsonArray: TJSONArray;
    jsonObject: TJSONObject;
    i: Integer;
begin
    try
        jsonData := GetJSON(jsonString);
        try
            if jsonData is TJSONArray then
            begin
                jsonArray := TJSONArray(jsonData);

                for i := 0 to jsonArray.Count - 1 do
                begin
                    jsonObject := TJSONObject(jsonArray[i]);

                    jListView1.AddItem(
                        jsonObject.Get('name', 'Inconnu'),
                        'ic_person'
                    );
                end;
            end
            else if jsonData is TJSONObject then
            begin
                jsonObject := TJSONObject(jsonData);

                jTextView1.Text := jsonObject.Get('name', '');
                jTextView2.Text := jsonObject.Get('email', '');
            end;
        finally
            jsonData.Free;
        end;
    except
        on E: Exception do
            ShowMessage('Erreur JSON: ' + E.Message);
    end;
end;

function CreerJSON: string;  
var
    json: TJSONObject;
begin
    json := TJSONObject.Create;
    try
        json.Add('name', 'Alice');
        json.Add('email', 'alice@example.com');
        json.Add('age', 25);
        json.Add('premium', True);

        Result := json.AsJSON;
    finally
        json.Free;
    end;
end;
```

### WebSocket

```pascal
type
    TAndroidModule1 = class(jForm)
        jWebSocket1: jWebSocket;
    end;

procedure ConnecterWebSocket;  
begin
    jWebSocket1.URL := 'wss://echo.websocket.org';
    jWebSocket1.Connect;
end;

procedure jWebSocket1Connected(Sender: TObject);  
begin
    ShowMessage('WebSocket connecté');

    // Envoyer un message
    jWebSocket1.Send('Hello WebSocket!');
end;

procedure jWebSocket1MessageReceived(Sender: TObject; const Message: string);  
begin
    jTextView1.Text := 'Reçu: ' + Message;
end;

procedure jWebSocket1Error(Sender: TObject; const ErrorMsg: string);  
begin
    ShowMessage('Erreur WebSocket: ' + ErrorMsg);
end;

procedure jWebSocket1Closed(Sender: TObject);  
begin
    ShowMessage('WebSocket déconnecté');
end;

procedure EnvoyerMessage;  
begin
    if jWebSocket1.IsConnected then
        jWebSocket1.Send(jEditText1.Text)
    else
        ShowMessage('Non connecté');
end;

procedure DeconnecterWebSocket;  
begin
    jWebSocket1.Disconnect;
end;
```

## Partage et intégration système

### Partage de contenu

```pascal
procedure PartagerTexte;  
begin
    jForm.ShareText('Découvrez cette application !',
                    'Partage depuis mon app');
end;

procedure PartagerImage;  
var
    imagePath: string;
begin
    imagePath := jForm.GetExternalStoragePath + '/photo.jpg';
    jForm.ShareFile(imagePath, 'Partager la photo', 'image/*');
end;

procedure PartagerPlusieurs;  
var
    files: TStringList;
begin
    files := TStringList.Create;
    try
        files.Add(jForm.GetExternalStoragePath + '/photo1.jpg');
        files.Add(jForm.GetExternalStoragePath + '/photo2.jpg');
        files.Add(jForm.GetExternalStoragePath + '/photo3.jpg');

        jForm.ShareMultipleFiles(files, 'Partager les photos', 'image/*');
    finally
        files.Free;
    end;
end;

procedure PartagerVersApplication;  
begin
    // Partager spécifiquement vers une app (ex: WhatsApp)
    jForm.ShareToPackage('com.whatsapp', 'Message WhatsApp',
                         'Texte à partager');
end;
```

### Intents (Actions système)

```pascal
// Ouvrir une URL dans le navigateur
procedure OuvrirURL;  
begin
    jForm.OpenURL('https://www.example.com');
end;

// Appeler un numéro
procedure AppelerNumero;  
begin
    if jForm.HasPermission('android.permission.CALL_PHONE') then
        jForm.CallPhone('+33123456789')
    else
        jForm.DialPhone('+33123456789');  // Ouvre le composeur
end;

// Envoyer un SMS
procedure EnvoyerSMS;  
begin
    jForm.SendSMS('+33123456789', 'Bonjour depuis mon app!');
end;

// Envoyer un email
procedure EnvoyerEmail;  
begin
    jForm.SendEmail(
        'contact@example.com',           // Destinataire
        'Sujet du message',               // Sujet
        'Corps du message',               // Corps
        ''                                // Pièce jointe (optionnel)
    );
end;

// Ouvrir la galerie photos
procedure OuvrirGalerie;  
begin
    jForm.PickImageFromGallery;
end;

procedure OnImagePicked(const ImagePath: string);  
begin
    jImageView1.LoadFromFile(ImagePath);
end;

// Ouvrir Google Maps
procedure OuvrirMaps;  
var
    latitude, longitude: Double;
begin
    latitude := 48.8566;   // Paris
    longitude := 2.3522;

    jForm.OpenMaps(latitude, longitude, 'Tour Eiffel');
end;

// Ouvrir les paramètres de l'application
procedure OuvrirParametresApp;  
begin
    jForm.OpenAppSettings;
end;

// Installer une APK
procedure InstallerAPK;  
var
    apkPath: string;
begin
    apkPath := jForm.GetDownloadsPath + '/update.apk';
    jForm.InstallAPK(apkPath);
end;
```

## Animations et transitions

### Animations de composants

```pascal
// Animation de translation
procedure AnimerDeplacement;  
begin
    jButton1.AnimateTranslation(
        0, 0,           // Position de départ (relative)
        200, 0,         // Position d'arrivée (200dp vers la droite)
        500,            // Durée en ms
        aiLinear        // Interpolation
    );
end;

// Animation de rotation
procedure AnimerRotation;  
begin
    jImageView1.AnimateRotation(
        0,              // Angle de départ
        360,            // Angle d'arrivée
        1000,           // Durée
        aiAccelerate    // Accélération
    );
end;

// Animation d'échelle
procedure AnimerZoom;  
begin
    jImageView1.AnimateScale(
        1.0, 1.0,       // Échelle de départ (normale)
        1.5, 1.5,       // Échelle d'arrivée (150%)
        500,            // Durée
        aiOvershoot     // Effet de rebond
    );
end;

// Animation de transparence
procedure AnimerFadeIn;  
begin
    jTextView1.AnimateAlpha(
        0.0,            // Transparent
        1.0,            // Opaque
        500,            // Durée
        aiLinear
    );
end;

procedure AnimerFadeOut;  
begin
    jTextView1.AnimateAlpha(1.0, 0.0, 500, aiLinear);
end;

// Combinaison d'animations
procedure AnimationComplexe;  
begin
    // Déplacement + rotation + zoom simultanés
    jButton1.BeginAnimationSet;
    jButton1.AnimateTranslation(0, 0, 100, 100, 1000, aiLinear);
    jButton1.AnimateRotation(0, 360, 1000, aiLinear);
    jButton1.AnimateScale(1.0, 1.0, 1.5, 1.5, 1000, aiLinear);
    jButton1.EndAnimationSet;
end;

// Callback de fin d'animation
procedure jButton1AnimationEnd(Sender: TObject);  
begin
    ShowMessage('Animation terminée');
end;
```

**Types d'interpolation** :
```pascal
aiLinear           // Vitesse constante  
aiAccelerate       // Accélère  
aiDecelerate       // Décélère  
aiAccelerateDecelerate  // Accélère puis décélère  
aiBounce           // Rebondit à la fin  
aiOvershoot        // Dépasse puis revient  
aiAnticipate       // Recule avant d'avancer
```

### Transitions entre Activities

```pascal
procedure TransitionAvecAnimation;  
begin
    // Définir les animations de transition
    jForm.SetActivityTransition(
        atSlideLeft,     // Animation d'entrée
        atSlideRight     // Animation de sortie
    );

    // Lancer la nouvelle Activity
    LancerActivity('DetailActivity');
end;
```

**Types de transitions** :
```pascal
atSlideLeft, atSlideRight, atSlideUp, atSlideDown  
atFadeIn, atFadeOut  
atZoomIn, atZoomOut  
atNone  // Pas d'animation
```

## Widgets et écran d'accueil

### App Widget

Créer un widget pour l'écran d'accueil Android.

```pascal
type
    TMonWidget = class(jAppWidget)
    private
        jTextView1: jTextView;
        jButton1: jButton;
    public
        procedure OnCreate; override;
        procedure OnUpdate; override;
    end;

procedure TMonWidget.OnCreate;  
begin
    inherited;

    // Configuration du layout du widget
    SetLayoutResource('layout', 'widget_layout');

    // Configuration des composants
    jTextView1.Text := 'Mon Widget';
    jButton1.Text := 'Actualiser';
end;

procedure TMonWidget.OnUpdate;  
begin
    // Mise à jour périodique du widget
    jTextView1.Text := 'Heure: ' + TimeToStr(Now);
end;

procedure jButton1Click(Sender: TObject);  
begin
    // Action au clic sur le bouton du widget
    OnUpdate;  // Actualiser manuellement
end;
```

### Live Wallpaper

Fond d'écran animé.

```pascal
type
    TMonLiveWallpaper = class(jLiveWallpaper)
    private
        FCanvas: jCanvas;
    public
        procedure OnCreate; override;
        procedure OnDraw; override;
        procedure OnTouch(x, y: Single); override;
    end;

procedure TMonLiveWallpaper.OnCreate;  
begin
    inherited;
    SetFrameRate(30);  // 30 FPS
end;

procedure TMonLiveWallpaper.OnDraw;  
var
    i: Integer;
begin
    // Dessiner sur le fond d'écran
    FCanvas.Clear(colbrBlack);

    for i := 0 to 10 do
    begin
        FCanvas.DrawCircle(
            Random(FCanvas.Width),
            Random(FCanvas.Height),
            Random(50) + 10,
            RandomColor
        );
    end;
end;

procedure TMonLiveWallpaper.OnTouch(x, y: Single);  
begin
    // Réagir au toucher de l'utilisateur
    FCanvas.DrawCircle(x, y, 100, colbrWhite);
end;
```

## Optimisation de l'interface

### Performances d'affichage

```pascal
// Utiliser ViewHolder pattern pour les listes
type
    TViewHolder = class
        jTextView: jTextView;
        jImageView: jImageView;
    end;

procedure jListView1GetView(Sender: TObject; position: Integer;
                            convertView: jView; var view: jView);
var
    holder: TViewHolder;
begin
    if convertView = nil then
    begin
        // Créer une nouvelle vue
        view := InflateLayout('layout', 'list_item');

        // Créer le ViewHolder
        holder := TViewHolder.Create;
        holder.jTextView := FindViewById(view, 'textView1');
        holder.jImageView := FindViewById(view, 'imageView1');

        // Sauvegarder le holder
        view.SetTag(holder);
    end
    else
    begin
        // Réutiliser la vue existante
        view := convertView;
        holder := TViewHolder(view.GetTag);
    end;

    // Mettre à jour les données
    holder.jTextView.Text := FData[position];
    holder.jImageView.SetImage('drawable', FIcons[position]);
end;
```

### Chargement d'images optimisé

```pascal
procedure ChargerImageOptimisee;  
begin
    // Charger en arrière-plan
    jImageView1.LoadFromURLAsync('https://example.com/large-image.jpg');

    // Avec placeholder pendant le chargement
    jImageView1.SetPlaceholder('drawable', 'placeholder');

    // Avec mise en cache
    jImageView1.EnableCache := True;

    // Redimensionner automatiquement
    jImageView1.AutoResize := True;
    jImageView1.MaxWidth := 500;
    jImageView1.MaxHeight := 500;
end;

procedure jImageView1LoadComplete(Sender: TObject; Success: Boolean);  
begin
    if Success then
        jProgressBar1.Visible := False
    else
        jImageView1.SetImage('drawable', 'error_image');
end;
```

### Gestion de la mémoire

```pascal
procedure OnLowMemory;  
begin
    // Libérer les ressources non essentielles
    jImageView1.RecycleBitmap;

    // Vider les caches
    ClearImageCache;

    // Forcer le garbage collector
    System.gc;
end;

procedure OnTrimMemory(level: Integer);  
begin
    case level of
        TRIM_MEMORY_RUNNING_CRITICAL:
            // App en premier plan mais système critique
            LibererCachesNonEssentiels;

        TRIM_MEMORY_BACKGROUND:
            // App en arrière-plan
            LibererRessourcesLourdes;

        TRIM_MEMORY_COMPLETE:
            // App peut être tuée
            LibererTout;
    end;
end;
```

## Accessibilité

### Support des lecteurs d'écran

```pascal
procedure ConfigurerAccessibilite;  
begin
    // Descriptions pour les lecteurs d'écran
    jButton1.ContentDescription := 'Bouton de validation du formulaire';
    jImageView1.ContentDescription := 'Photo de profil de l''utilisateur';

    // Indiquer qu'un élément est important
    jTextView1.ImportantForAccessibility := True;

    // Grouper des éléments
    jPanel1.AccessibilityHeading := True;
end;

// Annoncer un événement
procedure AnnoncerEvenement;  
begin
    jForm.AnnounceForAccessibility('Fichier téléchargé avec succès');
end;
```

### Tailles de texte adaptatives

```pascal
procedure RespectferPreferencesTaille;  
var
    userTextScale: Single;
begin
    // Obtenir la préférence de taille de texte de l'utilisateur
    userTextScale := jForm.GetTextScaleFactor;

    // Adapter les tailles de texte
    jTextView1.TextSize := Round(16 * userTextScale);
    jButton1.TextSize := Round(14 * userTextScale);
end;
```

### Contraste et couleurs

```pascal
procedure AdapterContraste;  
var
    highContrastEnabled: Boolean;
begin
    highContrastEnabled := jForm.IsHighContrastEnabled;

    if highContrastEnabled then
    begin
        // Utiliser des couleurs à fort contraste
        jTextView1.TextColor := colbrBlack;
        jPanel1.BackgroundColor := colbrWhite;
    end
    else
    begin
        // Couleurs normales
        jTextView1.TextColor := colbrDarkGray;
        jPanel1.BackgroundColor := colbrLightGray;
    end;
end;
```

## Bonnes pratiques

### Responsive Design

```pascal
procedure AdapterALaTailleEcran;  
var
    screenSize: TScreenSize;
    orientation: TOrientation;
begin
    screenSize := jForm.GetScreenSize;
    orientation := jForm.GetOrientation;

    case screenSize of
        ssSmall:  // Téléphone petit (< 3.5")
        begin
            jTextView1.TextSize := 14;
            jButton1.LayoutParamHeight := 48;
        end;

        ssNormal:  // Téléphone normal (3.5" - 5")
        begin
            jTextView1.TextSize := 16;
            jButton1.LayoutParamHeight := 56;
        end;

        ssLarge:  // Tablette 7"
        begin
            jTextView1.TextSize := 18;
            jButton1.LayoutParamHeight := 64;
        end;

        ssXLarge:  // Tablette 10"+
        begin
            jTextView1.TextSize := 20;
            jButton1.LayoutParamHeight := 72;
        end;
    end;

    if orientation = orLandscape then
    begin
        // Adapter pour le mode paysage
        jPanel1.LayoutParamWidth := lpMatchParent;
        AfficherEnDeuxColonnes;
    end
    else
    begin
        // Mode portrait
        jPanel1.LayoutParamWidth := lpMatchParent;
        AfficherEnUneColonne;
    end;
end;
```

### Thèmes et styles cohérents

```pascal
procedure AppliquerTheme;  
begin
    // Thème clair
    jForm.SetTheme(tmLight);

    // Thème sombre
    jForm.SetTheme(tmDark);

    // Suivre le thème système (Android 10+)
    jForm.SetTheme(tmAuto);
end;

procedure DetecterThemeSysteme;  
begin
    if jForm.IsDarkModeEnabled then
    begin
        // L'utilisateur utilise le mode sombre
        AppliquerCouleursSombres;
    end
    else
    begin
        // Mode clair
        AppliquerCouleursClaires;
    end;
end;
```

### Tests et qualité

```pascal
// Simuler différentes configurations
procedure TesterConfigurations;  
begin
    {$IFDEF DEBUG}
    // Forcer une petite taille d'écran
    SimulerEcran(ssSmall);

    // Forcer le mode sombre
    SimulerModeSombre(True);

    // Forcer une locale
    SimulerLocale('fr_FR');
    {$ENDIF}
end;
```

## Conclusion

Les interfaces natives Android offrent une richesse incroyable de composants et de fonctionnalités. En les maîtrisant avec LAMW, vous pouvez créer des applications qui :

**✓ Respectent les conventions Android**
- Material Design
- Animations fluides
- Navigation intuitive

**✓ Offrent une excellente expérience utilisateur**
- Responsive sur tous les appareils
- Accessibles à tous
- Performantes

**✓ S'intègrent parfaitement au système**
- Permissions gérées correctement
- Partage de contenu
- Intégration avec les services système

**✓ Sont professionnelles et modernes**
- Design contemporain
- Fonctionnalités avancées
- Code maintenable

**Points clés à retenir** :

1. **Composants natifs** : Utilisez les composants Android standard pour une UI familière
2. **Material Design** : Suivez les principes de design de Google
3. **Permissions** : Demandez uniquement ce qui est nécessaire, au bon moment
4. **Performance** : Optimisez les listes, chargez les images intelligemment
5. **Accessibilité** : Pensez à tous les utilisateurs
6. **Responsive** : Adaptez-vous à toutes les tailles d'écran
7. **Thèmes** : Supportez les modes clair et sombre

**Prochaines étapes** :
- 13.4 Custom Drawn pour interfaces portables
- 13.5 Capteurs et périphériques mobiles
- 13.6 iOS avec FreePascal (expérimental)
- 13.7 Raspberry Pi et développement ARM

**Ressources complémentaires** :

📚 **Documentation** :
- Material Design Guidelines (material.io)
- Android Developer Documentation (developer.android.com)
- LAMW Documentation et exemples
- Wiki Lazarus - Section Android

🛠️ **Outils utiles** :
- Android Studio (pour concevoir les layouts XML)
- Material Design Icons (materialdesignicons.com)
- Android Asset Studio (pour générer les icônes)
- Device Frame Generator (pour les captures d'écran)

💡 **Conseils finaux** :

1. **Commencez simple** : Maîtrisez les composants de base avant les avancés
2. **Testez sur vrais appareils** : Les émulateurs ne reflètent pas toujours la réalité
3. **Suivez les conventions** : Les utilisateurs Android ont des attentes spécifiques
4. **Pensez offline-first** : La connexion n'est pas toujours stable
5. **Optimisez la batterie** : Respectez la durée de vie de la batterie
6. **Itérez** : Améliorez continuellement basé sur les retours utilisateurs

## Exemples de projets complets

### Application de notes

```pascal
type
    TNoteApp = class(jForm)
        jToolbar1: jToolbar;
        jRecyclerView1: jRecyclerView;
        jFAB1: jFloatingActionButton;
        jBottomSheet1: jBottomSheetDialog;
        jSQLite1: jSQLite;
    private
        procedure InitDatabase;
        procedure LoadNotes;
        procedure AddNote;
        procedure EditNote(noteId: Integer);
        procedure DeleteNote(noteId: Integer);
    public
        procedure jFormCreate(Sender: TObject);
        procedure jFAB1Click(Sender: TObject);
    end;

procedure TNoteApp.jFormCreate(Sender: TObject);  
begin
    // Configuration de la Toolbar
    jToolbar1.Title := 'Mes Notes';
    jToolbar1.BackgroundColor := colbrBlue;
    jToolbar1.AddMenuItem(1, 'Rechercher', 'ic_search', mitIfRoom);
    jToolbar1.AddMenuItem(2, 'Paramètres', 'ic_settings', mitNever);

    // Configuration du RecyclerView
    jRecyclerView1.SetLayoutManager(lmLinear);
    jRecyclerView1.AddItemDecoration(idDivider);

    // Configuration du FAB
    jFAB1.SetImage('drawable', 'ic_add');
    jFAB1.BackgroundColor := colbrBlue;

    // Initialiser et charger les données
    InitDatabase;
    LoadNotes;
end;

procedure TNoteApp.InitDatabase;  
begin
    jSQLite1.DatabaseName := 'notes.db';
    jSQLite1.Open;

    jSQLite1.ExecuteSQL(
        'CREATE TABLE IF NOT EXISTS notes (' +
        '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
        '  title TEXT NOT NULL,' +
        '  content TEXT,' +
        '  color INTEGER DEFAULT 0,' +
        '  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,' +
        '  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP' +
        ')'
    );
end;

procedure TNoteApp.LoadNotes;  
var
    cursor: jCursor;
begin
    jRecyclerView1.Clear;

    cursor := jSQLite1.ExecuteQuery(
        'SELECT id, title, content, color FROM notes ' +
        'ORDER BY updated_at DESC'
    );

    while cursor.MoveToNext do
    begin
        jRecyclerView1.AddItem(
            cursor.GetInt(0),      // id
            cursor.GetString(1),   // title
            cursor.GetString(2),   // content
            cursor.GetInt(3)       // color
        );
    end;

    cursor.Close;
end;

procedure TNoteApp.jFAB1Click(Sender: TObject);  
begin
    AddNote;
end;

procedure TNoteApp.AddNote;  
begin
    jBottomSheet1.SetContentView('layout', 'edit_note');
    jBottomSheet1.Show;

    // Configurer les composants du BottomSheet
    // ...
end;
```

### Application météo

```pascal
type
    TWeatherApp = class(jForm)
        jHttpClient1: jHttpClient;
        jImageView1: jImageView;
        jTextViewTemp: jTextView;
        jTextViewCity: jTextView;
        jTextViewDescription: jTextView;
        jRecyclerView1: jRecyclerView;
    private
        FApiKey: string;
        procedure LoadWeather(const city: string);
        procedure ParseWeatherData(const jsonData: string);
    public
        procedure jFormCreate(Sender: TObject);
    end;

procedure TWeatherApp.jFormCreate(Sender: TObject);  
begin
    FApiKey := 'YOUR_API_KEY';

    // Demander la permission de localisation
    if jForm.HasPermission('android.permission.ACCESS_FINE_LOCATION') then
        LoadWeatherByLocation
    else
        jForm.RequestPermission('android.permission.ACCESS_FINE_LOCATION', 100);
end;

procedure TWeatherApp.LoadWeather(const city: string);  
var
    url: string;
begin
    url := Format('https://api.openweathermap.org/data/2.5/weather?q=%s&appid=%s&units=metric&lang=fr',
                  [city, FApiKey]);

    jHttpClient1.URL := url;
    jHttpClient1.Method := hmGET;
    jHttpClient1.Execute;
end;

procedure TWeatherApp.ParseWeatherData(const jsonData: string);  
var
    json: TJSONObject;
    main, weather: TJSONObject;
    temp: Double;
    description, icon: string;
begin
    json := TJSONObject(GetJSON(jsonData));
    try
        main := TJSONObject(json.Get('main'));
        weather := TJSONObject(TJSONArray(json.Get('weather')).Items[0]);

        temp := main.Get('temp', 0.0);
        description := weather.Get('description', '');
        icon := weather.Get('icon', '');

        // Afficher les données
        jTextViewTemp.Text := Format('%.0f°C', [temp]);
        jTextViewDescription.Text := description;

        // Charger l'icône météo
        jImageView1.LoadFromURL(
            Format('https://openweathermap.org/img/wn/%s@2x.png', [icon])
        );
    finally
        json.Free;
    end;
end;
```

### Application de chat

```pascal
type
    TChatApp = class(jForm)
        jWebSocket1: jWebSocket;
        jRecyclerView1: jRecyclerView;
        jEditText1: jEditText;
        jButton1: jButton;
        jSQLite1: jSQLite;
    private
        FUsername: string;
        procedure ConnectToServer;
        procedure SendMessage(const message: string);
        procedure ReceiveMessage(const sender, message: string; timestamp: TDateTime);
        procedure SaveMessageToLocal(const sender, message: string; timestamp: TDateTime);
        procedure LoadLocalMessages;
    public
        procedure jFormCreate(Sender: TObject);
        procedure jButton1Click(Sender: TObject);
    end;

procedure TChatApp.jFormCreate(Sender: TObject);  
begin
    // Récupérer le nom d'utilisateur
    FUsername := jForm.LoadPreference('username', '');

    if FUsername = '' then
    begin
        // Demander le nom d'utilisateur
        AfficherDialogueUsername;
    end
    else
    begin
        ConnectToServer;
        LoadLocalMessages;
    end;

    // Configuration du RecyclerView
    jRecyclerView1.SetLayoutManager(lmLinear);
    jRecyclerView1.ReverseLayout := True;  // Messages récents en bas
end;

procedure TChatApp.ConnectToServer;  
begin
    jWebSocket1.URL := 'wss://chat.example.com/socket';
    jWebSocket1.Connect;
end;

procedure TChatApp.jButton1Click(Sender: TObject);  
begin
    if jEditText1.Text <> '' then
    begin
        SendMessage(jEditText1.Text);
        jEditText1.Text := '';
    end;
end;

procedure TChatApp.SendMessage(const message: string);  
var
    json: TJSONObject;
begin
    json := TJSONObject.Create;
    try
        json.Add('type', 'message');
        json.Add('sender', FUsername);
        json.Add('message', message);
        json.Add('timestamp', DateTimeToUnix(Now));

        jWebSocket1.Send(json.AsJSON);

        // Ajouter à l'interface
        jRecyclerView1.AddMessage(FUsername, message, Now, True);  // True = message envoyé

        // Sauvegarder localement
        SaveMessageToLocal(FUsername, message, Now);
    finally
        json.Free;
    end;
end;

procedure TChatApp.ReceiveMessage(const sender, message: string; timestamp: TDateTime);  
begin
    // Ajouter à l'interface
    jRecyclerView1.AddMessage(sender, message, timestamp, False);  // False = message reçu

    // Notification si app en arrière-plan
    if not jForm.IsActive then
    begin
        jNotificationManager1.Title := sender;
        jNotificationManager1.Message := message;
        jNotificationManager1.Notify(1);
    end;

    // Sauvegarder localement
    SaveMessageToLocal(sender, message, timestamp);
end;
```

## Checklist de publication

Avant de publier votre application sur le Play Store :

### Technique
- [ ] Testée sur plusieurs appareils (différentes tailles, versions Android)
- [ ] Testée sur différentes connexions (WiFi, 3G/4G/5G, hors ligne)
- [ ] Gestion des erreurs et crashes
- [ ] Permissions minimales et justifiées
- [ ] Pas de fuites mémoire
- [ ] Optimisée (taille APK, performance)
- [ ] Signée avec une clé de release
- [ ] Version code et version name corrects
- [ ] ProGuard/R8 configuré si nécessaire

### Design
- [ ] Respecte Material Design
- [ ] Icônes adaptées à toutes les densités (mdpi, hdpi, xhdpi, xxhdpi, xxxhdpi)
- [ ] Screenshots de qualité (téléphone et tablette)
- [ ] Feature graphic (1024x500)
- [ ] Icône de l'app (512x512)
- [ ] Support du mode sombre
- [ ] Support de toutes les orientations nécessaires
- [ ] Responsive sur toutes les tailles

### Contenu
- [ ] Description claire et attractive (français et anglais minimum)
- [ ] What's New pour chaque version
- [ ] Politique de confidentialité (obligatoire)
- [ ] Captures d'écran représentatives
- [ ] Vidéo de présentation (optionnelle mais recommandée)

### Légal
- [ ] Droits d'auteur respectés (images, sons, code)
- [ ] RGPD conforme si données personnelles
- [ ] Conditions d'utilisation
- [ ] Contact support visible
- [ ] Classification du contenu correcte

### Play Store
- [ ] Compte développeur créé (25$ unique)
- [ ] Catégorie appropriée
- [ ] Tags et mots-clés optimisés
- [ ] Prix défini (gratuit/payant)
- [ ] Pays de distribution sélectionnés
- [ ] Tests alpha/beta effectués

## Maintenance post-lancement

### Analyse des retours
```pascal
// Intégrer des analytics
procedure TrackEvent(const category, action, label: string);  
begin
    jAnalytics1.LogEvent(category, action, label);
end;

procedure TrackScreenView(const screenName: string);  
begin
    jAnalytics1.SetCurrentScreen(screenName);
end;

// Crash reporting
procedure OnCrash(const errorMessage, stackTrace: string);  
begin
    jCrashReporter1.LogError(errorMessage, stackTrace);
end;
```

### Mises à jour
```pascal
// Vérifier les mises à jour
procedure CheckForUpdates;  
begin
    jHttpClient1.URL := 'https://api.monapp.com/version';
    jHttpClient1.Method := hmGET;
    jHttpClient1.Execute;
end;

procedure OnVersionReceived(latestVersion: Integer);  
const
    CURRENT_VERSION = 10;  // Version actuelle
begin
    if latestVersion > CURRENT_VERSION then
    begin
        jDialogYN1.Title := 'Mise à jour disponible';
        jDialogYN1.Msg := 'Une nouvelle version est disponible. Souhaitez-vous la télécharger ?';
        jDialogYN1.Yes := 'Mettre à jour';
        jDialogYN1.No := 'Plus tard';
        jDialogYN1.Show;
    end;
end;
```

### Support utilisateurs
```pascal
// Feedback intégré
procedure AfficherFormulaireSupport;  
begin
    jBottomSheet1.SetContentView('layout', 'support_form');
    jBottomSheet1.Show;
end;

procedure EnvoyerFeedback(const sujet, message: string);  
var
    json: string;
begin
    json := Format('{"subject":"%s","message":"%s","version":"%d","device":"%s"}',
                   [sujet, message, GetVersionCode, GetDeviceInfo]);

    jHttpClient1.URL := 'https://api.monapp.com/support';
    jHttpClient1.Method := hmPOST;
    jHttpClient1.AddHeader('Content-Type', 'application/json');
    jHttpClient1.PostData := json;
    jHttpClient1.Execute;
end;
```

---

**En résumé**, la maîtrise des interfaces natives Android avec LAMW vous permet de créer des applications professionnelles, performantes et appréciées des utilisateurs. Continuez à pratiquer, expérimenter, et surtout : écoutez vos utilisateurs !

**Bonne création d'applications Android avec FreePascal ! 🚀📱**

⏭️ [Custom Drawn pour interfaces mobiles](/13-developpement-mobile-embarque/04-custom-drawn-interfaces-mobiles.md)
