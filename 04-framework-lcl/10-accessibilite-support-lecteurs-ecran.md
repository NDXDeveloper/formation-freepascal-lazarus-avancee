🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Accessibilité et support des lecteurs d'écran dans Lazarus/FreePascal

## Introduction : Qu'est-ce que l'accessibilité ?

L'accessibilité consiste à rendre vos applications utilisables par tous, y compris les personnes en situation de handicap. Les lecteurs d'écran sont des logiciels qui lisent à haute voix le contenu affiché à l'écran, permettant aux personnes malvoyantes ou aveugles d'utiliser un ordinateur.

### Principaux lecteurs d'écran
- **Windows** : NVDA (gratuit), JAWS (commercial), Narrateur Windows (intégré)
- **Ubuntu/Linux** : Orca (lecteur d'écran principal pour GNOME)

## Concepts fondamentaux de l'accessibilité

### 1. Le rôle des composants

Chaque élément de votre interface doit avoir un **rôle** clair : est-ce un bouton, un champ de texte, une liste ? Les lecteurs d'écran annoncent ces rôles aux utilisateurs pour qu'ils comprennent ce qu'ils peuvent faire avec chaque élément.

### 2. Les labels et descriptions

Les composants visuels doivent avoir des **étiquettes textuelles** qui expliquent leur fonction. Un bouton avec seulement une icône n'est pas accessible sans description textuelle.

### 3. L'ordre de navigation

L'ordre dans lequel l'utilisateur navigue avec la touche Tab doit être **logique et prévisible**, généralement de gauche à droite et de haut en bas.

## Configuration de base dans Lazarus

### Propriétés essentielles pour l'accessibilité

Pour chaque composant dans Lazarus, plusieurs propriétés sont cruciales pour l'accessibilité :

#### La propriété `Caption`
```pascal
Button1.Caption := 'Enregistrer le document';
// Le lecteur d'écran lira "Bouton Enregistrer le document"
```

#### La propriété `Hint`
```pascal
Button1.Hint := 'Enregistre le document actuel sur le disque (Ctrl+S)';  
Button1.ShowHint := True;
// Fournit une information supplémentaire
```

#### La propriété `TabOrder`
```pascal
Edit1.TabOrder := 0;  // Premier élément  
Edit2.TabOrder := 1;  // Deuxième élément  
Button1.TabOrder := 2; // Troisième élément
```

### Associer des labels aux contrôles

Pour les champs de saisie, il est essentiel d'associer un label descriptif :

```pascal
Label1.Caption := '&Nom :';  // Le & indique le raccourci Alt+N  
Label1.FocusControl := Edit1; // Association avec le champ de saisie
```

Quand l'utilisateur navigue vers `Edit1`, le lecteur d'écran lira "Nom, zone d'édition".

## Bonnes pratiques pour une interface accessible

### 1. Utiliser des composants standards

Les composants standards de la LCL (Lazarus Component Library) ont un support d'accessibilité intégré :
- `TButton`, `TEdit`, `TLabel`, `TListBox`, `TComboBox`, etc.

Ces composants communiquent automatiquement avec les lecteurs d'écran via les API d'accessibilité du système.

### 2. Fournir des descriptions pour les images et icônes

Pour les boutons avec seulement des icônes :

```pascal
// Mauvaise pratique
ToolButton1.Caption := '';  
ToolButton1.ImageIndex := 5; // Seulement une icône

// Bonne pratique
ToolButton1.Caption := 'Imprimer';  
ToolButton1.ImageIndex := 5;  
ToolButton1.Hint := 'Imprimer le document actuel';
```

### 3. Gérer le focus clavier

Le focus doit être visible et logique :

```pascal
procedure TForm1.FormShow(Sender: TObject);  
begin
  // Définir le focus initial sur le premier champ important
  Edit1.SetFocus;
end;
```

### 4. Utiliser des raccourcis clavier

Les raccourcis permettent une navigation rapide :

```pascal
// Dans le caption, & définit le raccourci
Button1.Caption := '&Ouvrir';     // Alt+O  
Button2.Caption := '&Sauvegarder'; // Alt+S

// Pour les menus
MenuItem1.Caption := '&Fichier';  
MenuItem1.ShortCut := ShortCut(Ord('O'), [ssCtrl]); // Ctrl+O
```

## Gestion des messages d'état et notifications

### Utiliser la barre d'état

Les messages importants doivent être affichés dans la barre d'état :

```pascal
StatusBar1.SimpleText := 'Fichier enregistré avec succès';
```

Les lecteurs d'écran peuvent détecter les changements dans la barre d'état.

### Afficher des messages accessibles

Pour les messages importants :

```pascal
procedure AfficherMessageAccessible(const Message: string);  
begin
  // Le MessageDlg est accessible aux lecteurs d'écran
  MessageDlg(Message, mtInformation, [mbOK], 0);

  // Alternative : aussi mettre dans la barre d'état
  StatusBar1.SimpleText := Message;
end;
```

## Spécificités Windows

### API d'accessibilité Windows

Windows utilise plusieurs API d'accessibilité :
- **MSAA** (Microsoft Active Accessibility) - Plus ancien mais largement supporté
- **UI Automation** - Plus moderne et plus riche

Lazarus/FreePascal supporte automatiquement MSAA pour les composants standards.

### Configuration pour Windows

Pour activer le support complet sous Windows :

```pascal
uses
  Windows, // Pour les API Windows
  ComObj;  // Pour le support COM/MSAA

procedure ConfigurerAccessibiliteWindows;  
begin
  // S'assurer que les hints sont activés
  Application.ShowHint := True;
  Application.HintPause := 500; // Délai avant affichage
end;
```

### Test avec le Narrateur Windows

Pour tester votre application :
1. Appuyez sur `Windows + Ctrl + Enter` pour activer le Narrateur
2. Naviguez avec Tab dans votre application
3. Écoutez ce que le Narrateur annonce

## Spécificités Ubuntu/Linux

### Support ATK/AT-SPI

Linux utilise ATK (Accessibility Toolkit) et AT-SPI (Assistive Technology Service Provider Interface).

### Configuration pour Ubuntu

Les applications Lazarus avec widgetset GTK2/GTK3 supportent automatiquement l'accessibilité :

```pascal
uses
  Interfaces; // Assure le bon widgetset

procedure ConfigurerAccessibiliteLinux;  
begin
  // GTK gère automatiquement l'accessibilité
  // S'assurer que les propriétés standards sont définies
  Application.ShowHint := True;
end;
```

### Test avec Orca

Pour tester sous Ubuntu :
1. Installez Orca : `sudo apt-get install orca`
2. Lancez Orca : `Alt+F2` puis tapez `orca`
3. Testez votre application

## Grouper les contrôles liés

### Utiliser des GroupBox

Les `TGroupBox` permettent de regrouper logiquement les contrôles :

```pascal
GroupBox1.Caption := 'Informations personnelles';
// Placez dedans les champs liés
// Le lecteur d'écran annoncera le contexte
```

### Utiliser des RadioGroup

Pour les boutons radio, utilisez `TRadioGroup` :

```pascal
RadioGroup1.Caption := 'Format d''export';  
RadioGroup1.Items.Add('PDF');  
RadioGroup1.Items.Add('Word');  
RadioGroup1.Items.Add('HTML');
```

## Gestion des tableaux et listes

### Listes accessibles

Pour les `TListBox` et `TListView` :

```pascal
// Fournir un label descriptif
Label1.Caption := 'Liste des documents récents :';  
Label1.FocusControl := ListBox1;

// S'assurer que les éléments sont clairs
ListBox1.Items.Add('Rapport_2024.pdf - Modifié le 15/01/2024');
```

### Grilles de données

Pour les `TStringGrid` :

```pascal
// Définir des en-têtes clairs
StringGrid1.Cells[0, 0] := 'Nom';  
StringGrid1.Cells[1, 0] := 'Date';  
StringGrid1.Cells[2, 0] := 'Taille';

// Activer la navigation au clavier
StringGrid1.Options := StringGrid1.Options + [goRowSelect, goThumbTracking];
```

## Validation et messages d'erreur

### Messages d'erreur explicites

```pascal
procedure ValiderFormulaire;  
begin
  if Edit1.Text = '' then
  begin
    // Message clair avec contexte
    MessageDlg('Erreur : Le champ "Nom" est obligatoire. ' +
               'Veuillez entrer votre nom complet.',
               mtError, [mbOK], 0);
    Edit1.SetFocus; // Replacer le focus sur le champ
    Exit;
  end;
end;
```

## Couleurs et contrastes

### Respecter les préférences système

```pascal
// Utiliser les couleurs système plutôt que des couleurs fixes
Edit1.Color := clWindow;      // Couleur de fenêtre système  
Edit1.Font.Color := clWindowText; // Couleur de texte système

// Éviter
Edit1.Color := clYellow; // Peut être illisible
```

### Éviter de communiquer uniquement par la couleur

```pascal
// Mauvaise pratique : seulement la couleur
Edit1.Color := clRed; // Pour indiquer une erreur

// Bonne pratique : couleur + texte
Edit1.Color := clRed;  
Label2.Caption := '⚠ Erreur : Format invalide';
```

## Documentation de l'accessibilité

### Créer une page d'aide sur l'accessibilité

Incluez dans votre application :
- Liste des raccourcis clavier
- Instructions pour la navigation au clavier
- Compatibilité avec les lecteurs d'écran

```pascal
procedure AfficherAideAccessibilite;  
var
  Message: string;
begin
  Message := 'Raccourcis clavier :' + sLineBreak +
             'Ctrl+O : Ouvrir un fichier' + sLineBreak +
             'Ctrl+S : Sauvegarder' + sLineBreak +
             'F1 : Afficher l''aide' + sLineBreak +
             sLineBreak +
             'Navigation : Utilisez Tab pour naviguer entre les champs';

  MessageDlg(Message, mtInformation, [mbOK], 0);
end;
```

## Tests d'accessibilité

### Liste de vérification

Avant de publier votre application, vérifiez :

1. **Navigation au clavier complète** : Peut-on accéder à toutes les fonctionnalités sans souris ?
2. **Ordre de tabulation logique** : La navigation Tab suit-elle un ordre logique ?
3. **Labels descriptifs** : Tous les contrôles ont-ils des labels clairs ?
4. **Messages d'état** : Les changements importants sont-ils annoncés ?
5. **Contraste suffisant** : Le texte est-il lisible avec les thèmes système ?
6. **Documentation** : Les raccourcis clavier sont-ils documentés ?

### Outils de test

#### Windows
- **Accessibility Insights** : Outil gratuit de Microsoft
- **NVDA** : Lecteur d'écran gratuit pour tests

#### Ubuntu/Linux
- **Accerciser** : Explorateur d'accessibilité pour GNOME
  ```bash
  sudo apt-get install accerciser
  ```

## Ressources supplémentaires

### Documentation officielle
- Documentation Lazarus sur l'accessibilité
- Guidelines WCAG (Web Content Accessibility Guidelines) - applicables aussi aux applications desktop

### Communauté
- Forums Lazarus - section accessibilité
- Groupes d'utilisateurs de lecteurs d'écran pour feedback

## Conclusion

L'accessibilité n'est pas une fonctionnalité optionnelle mais une nécessité pour créer des applications inclusives. En suivant ces principes de base :

1. Utilisez les composants standards de Lazarus
2. Fournissez des labels et descriptions clairs
3. Assurez une navigation au clavier complète
4. Testez avec de vrais lecteurs d'écran

Vous créerez des applications utilisables par tous, augmentant ainsi votre audience potentielle et respectant les standards d'accessibilité modernes.

L'effort supplémentaire investi dans l'accessibilité améliore également l'expérience utilisateur générale, car une interface bien structurée et claire bénéficie à tous les utilisateurs.

⏭️ [High-DPI et mise à l'échelle](/04-framework-lcl/11-high-dpi-mise-echelle.md)
