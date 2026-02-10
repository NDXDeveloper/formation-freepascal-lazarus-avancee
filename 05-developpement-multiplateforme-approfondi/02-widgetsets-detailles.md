🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.2 Widgetsets détaillés

## Introduction : Qu'est-ce qu'un Widgetset ?

### Comprendre le Concept de Widget

Avant de plonger dans les widgetsets, comprenons d'abord ce qu'est un **widget**. Le terme "widget" est une contraction de "window gadget" (gadget de fenêtre). Un widget est tout élément d'interface graphique avec lequel l'utilisateur peut interagir : bouton, zone de texte, case à cocher, menu, barre de défilement, etc.

Chaque système d'exploitation a sa propre façon de créer et gérer ces widgets :
- **Windows** utilise les contrôles Win32/Win64 (BUTTON, EDIT, LISTBOX, etc.)
- **Linux** peut utiliser GTK, Qt, ou d'autres bibliothèques graphiques
- **macOS** utilise Cocoa

### Le Défi de la Portabilité Graphique

Le problème fondamental est que chaque système d'exploitation a une API graphique complètement différente :

```pascal
// Créer un bouton sur Windows (API native)
hButton := CreateWindow('BUTTON', 'Cliquez-moi',
                       WS_VISIBLE or WS_CHILD or BS_PUSHBUTTON,
                       10, 10, 100, 30, hParentWindow, 0, hInstance, nil);

// Créer un bouton sur Linux avec GTK
button := gtk_button_new_with_label('Cliquez-moi');  
gtk_widget_set_size_request(button, 100, 30);  
gtk_fixed_put(GTK_FIXED(container), button, 10, 10);

// Créer un bouton sur Linux avec Qt
button := QPushButton_Create('Cliquez-moi');  
QPushButton_setGeometry(button, 10, 10, 100, 30);
```

Comment faire pour qu'un simple `TButton` de Lazarus fonctionne sur tous ces systèmes ? C'est là qu'interviennent les **widgetsets**.

## Qu'est-ce qu'un Widgetset dans Lazarus ?

Un **widgetset** est une couche d'abstraction qui fait le pont entre les composants LCL (Lazarus Component Library) et le système graphique natif de chaque OS. C'est le "traducteur" qui convertit les appels LCL en appels natifs du système.

### Architecture Conceptuelle

```
Votre Application Lazarus
         ↓
    Composants LCL
    (TButton, TEdit, TForm...)
         ↓
    Interface Widgetset
    (Couche d'abstraction)
         ↓
    Widgetset Concret
    ┌────────┬────────┬────────┬────────┐
    │ Win32  │  GTK2  │  GTK3  │   Qt5  │
    └────────┴────────┴────────┴────────┘
         ↓         ↓         ↓         ↓
    API Native du Système
```

### Le Rôle du Widgetset

Le widgetset remplit plusieurs fonctions essentielles :

1. **Création de fenêtres et contrôles** : Traduit les demandes LCL en appels natifs
2. **Gestion des événements** : Convertit les événements système en événements LCL
3. **Dessin et rendu** : Adapte les opérations graphiques au système
4. **Gestion des ressources** : Handles, contextes graphiques, polices, etc.
5. **Intégration système** : Presse-papiers, drag & drop, dialogues natifs

## Les Différents Widgetsets Disponibles

Lazarus propose plusieurs widgetsets, chacun avec ses caractéristiques :

### Widgetsets Principaux

| Widgetset | Plateformes | Bibliothèque Native | Statut |
|-----------|-------------|-------------------|---------|
| **Win32/Win64** | Windows | API Windows native | Stable, mature |
| **GTK2** | Linux, Unix, BSD | GTK+ 2.x | Stable, largement utilisé |
| **GTK3** | Linux, Unix, BSD | GTK+ 3.x | Stable, moderne |
| **Qt5** | Linux, Windows, macOS | Qt 5.x | Stable, cross-platform |
| **Qt6** | Linux, Windows, macOS | Qt 6.x | En développement |
| **Cocoa** | macOS | Cocoa/AppKit | Stable |
| **Carbon** | macOS (ancien) | Carbon | Obsolète |
| **Custom Drawn** | Toutes | Aucune (dessin manuel) | Expérimental |

### Comment Choisir le Bon Widgetset ?

Le choix du widgetset dépend de plusieurs facteurs :

#### 1. **Plateforme Cible**
- **Windows uniquement** → Win32/Win64
- **Linux uniquement** → GTK2 ou GTK3
- **macOS uniquement** → Cocoa
- **Multi-plateforme** → Qt5 ou Custom Drawn

#### 2. **Apparence Native**
- **Look natif important** → Widgetset natif (Win32 pour Windows, GTK pour Linux)
- **Look uniforme sur tous les OS** → Qt5 ou Custom Drawn

#### 3. **Dépendances**
- **Minimiser les dépendances** → Natif (Win32) ou GTK2
- **Accepter des dépendances** → Qt5 (nécessite les bibliothèques Qt)

#### 4. **Fonctionnalités**
- **Toutes les fonctionnalités LCL** → Win32, GTK2
- **Contrôle total du rendu** → Custom Drawn
- **Thèmes modernes** → GTK3, Qt5

## Comment les Widgetsets Fonctionnent

### Le Processus de Création d'un Composant

Prenons l'exemple de la création d'un bouton :

```pascal
Button1 := TButton.Create(Form1);  
Button1.Parent := Form1;  
Button1.Caption := 'Mon Bouton';  
Button1.SetBounds(10, 10, 100, 30);
```

Voici ce qui se passe en coulisses :

1. **Création LCL** : `TButton.Create` initialise l'objet Pascal
2. **Appel au Widgetset** : La LCL appelle `CreateHandle` du widgetset
3. **Création Native** : Le widgetset crée le contrôle natif
4. **Liaison** : Le handle natif est stocké dans l'objet LCL
5. **Configuration** : Les propriétés (Caption, Bounds) sont appliquées

### Exemple Concret : Le Code Interne

Voici une version simplifiée de ce qui se passe dans différents widgetsets :

```pascal
// Dans le widgetset Win32
function TWin32WSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
begin
  // Créer un bouton Windows natif
  Result := CreateWindowEx(
    0,                    // Styles étendus
    'BUTTON',            // Classe de fenêtre Windows
    PChar(TButton(AWinControl).Caption),
    WS_CHILD or WS_VISIBLE or BS_PUSHBUTTON,
    AParams.X, AParams.Y, AParams.Width, AParams.Height,
    AParams.WndParent,   // Fenêtre parent
    0,                   // ID du menu
    HInstance,           // Instance de l'application
    nil                  // Paramètres supplémentaires
  );
end;

// Dans le widgetset GTK2
function TGtk2WSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Widget: PGtkWidget;
begin
  // Créer un bouton GTK natif
  Widget := gtk_button_new_with_label(PChar(TButton(AWinControl).Caption));
  gtk_widget_set_size_request(Widget, AParams.Width, AParams.Height);

  // Convertir le widget GTK en handle LCL
  Result := HWND(Widget);
end;
```

### La Gestion des Événements

Les événements suivent le chemin inverse :

1. **Événement Système** : L'OS génère un événement (clic souris)
2. **Capture par le Widgetset** : Le widgetset intercepte l'événement natif
3. **Traduction** : L'événement est converti au format LCL
4. **Dispatch** : L'événement est envoyé au composant LCL
5. **Handler** : Votre code (OnClick) est exécuté

```pascal
// Widgetset Win32 - Traitement des messages
function WindowProc(Window: HWND; Msg: UINT; WParam: WPARAM;
                   LParam: LPARAM): LRESULT;
begin
  case Msg of
    WM_LBUTTONDOWN:
      begin
        // Convertir en événement LCL
        Control := GetLCLObject(Window);
        if Control is TButton then
          TButton(Control).Click;
      end;
  end;
end;

// Widgetset GTK2 - Callback GTK
procedure gtk_button_clicked_callback(Widget: PGtkWidget; Data: gpointer); cdecl;  
var
  Button: TButton;
begin
  // Récupérer l'objet LCL associé
  Button := TButton(GetLCLObject(Widget));
  if Assigned(Button) then
    Button.Click;
end;
```

## Avantages et Inconvénients des Différentes Approches

### Widgetsets Natifs (Win32, Cocoa)

**Avantages :**
- ✓ Apparence 100% native
- ✓ Intégration système parfaite
- ✓ Performances optimales
- ✓ Pas de dépendances externes
- ✓ Support complet des fonctionnalités OS

**Inconvénients :**
- ✗ Spécifique à une plateforme
- ✗ Maintenance séparée pour chaque OS
- ✗ Comportements potentiellement différents

### Widgetsets Cross-Platform (GTK, Qt)

**Avantages :**
- ✓ Code partagé entre plateformes
- ✓ Comportement plus uniforme
- ✓ Thèmes et styles cohérents
- ✓ Maintenance simplifiée

**Inconvénients :**
- ✗ Dépendances externes requises
- ✗ Peut ne pas respecter parfaitement le look natif
- ✗ Taille d'application plus importante

### Custom Drawn

**Avantages :**
- ✓ Contrôle total sur le rendu
- ✓ Apparence 100% identique sur tous les OS
- ✓ Pas de dépendances externes
- ✓ Idéal pour les interfaces personnalisées

**Inconvénients :**
- ✗ Ne respecte pas le look natif
- ✗ Plus de travail de développement
- ✗ Performances potentiellement moindres
- ✗ Accessibilité limitée

## Configuration et Sélection du Widgetset

### Au Moment de la Compilation

Le widgetset est sélectionné lors de la compilation. Vous pouvez le spécifier de plusieurs façons :

#### 1. Dans l'IDE Lazarus

```
Projet → Options du projet → Ajouts et Substitutions →  
Définir LCLWidgetType → [Sélectionner le widgetset]
```

#### 2. En Ligne de Commande

```bash
# Compiler pour Windows avec Win32
lazbuild --ws=win32 monprojet.lpi

# Compiler pour Linux avec GTK2
lazbuild --ws=gtk2 monprojet.lpi

# Compiler pour Linux avec Qt5
lazbuild --ws=qt5 monprojet.lpi
```

#### 3. Dans le Code Source

```pascal
{$IFDEF WINDOWS}
  {$DEFINE USE_WIN32}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE USE_GTK2}
{$ENDIF}

program MonApplication;

uses
  {$IFDEF USE_WIN32}
  Interfaces, // Charge automatiquement Win32
  {$ENDIF}
  {$IFDEF USE_GTK2}
  Interfaces, // Charge automatiquement GTK2
  {$ENDIF}
  Forms;
```

### Détection du Widgetset à l'Exécution

Vous pouvez déterminer quel widgetset est utilisé pendant l'exécution :

```pascal
uses InterfaceBase, LCLPlatformDef;

procedure AfficherWidgetset;  
var
  WS: TLCLPlatform;
begin
  WS := WidgetSet.LCLPlatform;

  case WS of
    lpWin32: WriteLn('Widgetset: Win32');
    lpWin64: WriteLn('Widgetset: Win64');
    lpGtk: WriteLn('Widgetset: GTK');
    lpGtk2: WriteLn('Widgetset: GTK2');
    lpGtk3: WriteLn('Widgetset: GTK3');
    lpQt: WriteLn('Widgetset: Qt4');
    lpQt5: WriteLn('Widgetset: Qt5');
    lpQt6: WriteLn('Widgetset: Qt6');
    lpCocoa: WriteLn('Widgetset: Cocoa');
    lpCustomDrawn: WriteLn('Widgetset: Custom Drawn');
  else
    WriteLn('Widgetset: Inconnu');
  end;
end;
```

## Impact sur le Développement

### Ce qui Change Selon le Widgetset

#### 1. **Apparence Visuelle**
Les mêmes composants peuvent avoir un aspect différent :
- Hauteur des boutons
- Espacement par défaut
- Styles de bordures
- Polices système

#### 2. **Comportements**
Certains comportements peuvent varier :
- Ordre de tabulation
- Gestion du focus
- Raccourcis clavier
- Comportement du drag & drop

#### 3. **Fonctionnalités Disponibles**
Tous les widgetsets n'implémentent pas toutes les fonctionnalités :
- Transparence
- Effets visuels
- Certains styles de dessin
- API système spécifiques

### Stratégies de Développement Multi-Widgetset

#### 1. **Tester sur Tous les Widgetsets Cibles**

```pascal
// Code de test automatisé
procedure TestButton;  
var
  Btn: TButton;
begin
  Btn := TButton.Create(nil);
  try
    // Test qui doit fonctionner sur tous les widgetsets
    AssertEquals('Caption', 'Test', Btn.Caption);

    // Test spécifique au widgetset
    {$IFDEF LCLWin32}
    // Test spécifique Windows
    {$ENDIF}

    {$IFDEF LCLGtk2}
    // Test spécifique GTK2
    {$ENDIF}
  finally
    Btn.Free;
  end;
end;
```

#### 2. **Gérer les Différences Connues**

```pascal
function GetButtonHeight: Integer;  
begin
  // Ajuster selon le widgetset
  {$IFDEF LCLWin32}
  Result := 25;  // Hauteur standard Windows
  {$ENDIF}

  {$IFDEF LCLGtk2}
  Result := 28;  // GTK2 a des boutons légèrement plus hauts
  {$ENDIF}

  {$IFDEF LCLQt5}
  Result := 26;  // Qt5 quelque part entre les deux
  {$ENDIF}
end;
```

#### 3. **Utiliser l'Abstraction Maximale**

```pascal
// ÉVITER - Code spécifique au widgetset
{$IFDEF LCLWin32}
SendMessage(Button1.Handle, BM_SETSTATE, 1, 0);
{$ENDIF}

// PRÉFÉRER - Utiliser les méthodes LCL
Button1.SetFocus;  
Button1.Invalidate;
```

## Fonctionnalités Avancées des Widgetsets

### Accès aux Fonctionnalités Natives

Parfois, vous devez accéder aux fonctionnalités spécifiques d'un widgetset :

```pascal
uses
  {$IFDEF LCLWin32}
  Windows, Win32Proc, Win32WSControls,
  {$ENDIF}
  {$IFDEF LCLGtk2}
  Gtk2, Glib2, Gtk2Proc, Gtk2WSControls,
  {$ENDIF}
  LCLType, LCLIntf;

procedure SetButtonStyle(Button: TButton);  
begin
  {$IFDEF LCLWin32}
  // Accès direct à l'API Windows
  SetWindowLong(Button.Handle, GWL_STYLE,
                GetWindowLong(Button.Handle, GWL_STYLE) or BS_FLAT);
  {$ENDIF}

  {$IFDEF LCLGtk2}
  // Accès direct à GTK
  gtk_button_set_relief(PGtkButton(Button.Handle), GTK_RELIEF_NONE);
  {$ENDIF}
end;
```

### Création de Contrôles Personnalisés

Les widgetsets permettent de créer des contrôles personnalisés qui s'intègrent naturellement :

```pascal
type
  TCustomWidget = class(TWinControl)
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

class function TCustomWidget.GetControlClassDefaultSize: TSize;  
begin
  Result.CX := 100;
  Result.CY := 100;
end;

procedure TCustomWidget.CreateParams(var Params: TCreateParams);  
begin
  inherited CreateParams(Params);

  // Paramètres spécifiques au widgetset
  {$IFDEF LCLWin32}
  Params.Style := Params.Style or WS_BORDER;
  {$ENDIF}
end;
```

## Optimisation et Performance

### Considérations de Performance par Widgetset

Chaque widgetset a ses caractéristiques de performance :

#### **Win32/Win64**
- ✓ Très rapide sur Windows
- ✓ Utilisation mémoire minimale
- ✓ Démarrage rapide

#### **GTK2**
- ✓ Bon équilibre performance/fonctionnalités
- ⚠ Plus lent au démarrage que Win32
- ⚠ Consommation mémoire modérée

#### **GTK3**
- ⚠ Plus lourd que GTK2
- ✓ Meilleur support des écrans haute résolution
- ✓ Animations plus fluides

#### **Qt5**
- ⚠ Le plus lourd en mémoire
- ✓ Excellent rendu graphique
- ✓ Très bonne gestion des thèmes

### Techniques d'Optimisation

```pascal
// 1. Réduire les mises à jour visuelles
ListBox1.Items.BeginUpdate;  
try
  // Ajouter beaucoup d'éléments
  for i := 0 to 10000 do
    ListBox1.Items.Add('Item ' + IntToStr(i));
finally
  ListBox1.Items.EndUpdate;
end;

// 2. Utiliser DoubleBuffered sur les widgetsets qui le supportent
{$IFDEF LCLWin32}
Form1.DoubleBuffered := True;
{$ENDIF}

// 3. Optimiser le dessin personnalisé
procedure TCustomControl.Paint;  
begin
  // Vérifier le widgetset pour optimiser
  {$IFDEF LCLWin32}
  // Utiliser l'API GDI directement pour la performance
  {$ENDIF}

  {$IFDEF LCLGtk2}
  // Utiliser Cairo pour le dessin avancé
  {$ENDIF}
end;
```

## Résolution de Problèmes Courants

### Problèmes Fréquents et Solutions

#### 1. **Composant qui ne s'affiche pas correctement**
```pascal
// Forcer le recalcul de la mise en page
Component.InvalidatePreferredSize;  
Component.AdjustSize;  
Application.ProcessMessages;
```

#### 2. **Événements qui ne se déclenchent pas**
```pascal
// Vérifier que le handle est créé
if not Button1.HandleAllocated then
  Button1.HandleNeeded;
```

#### 3. **Différences de taille entre widgetsets**
```pascal
// Utiliser AutoSize et Anchors plutôt que des positions fixes
Button1.AutoSize := True;  
Button1.Anchors := [akLeft, akTop];
```

## Prochaines Sections

Dans les sections suivantes, nous examinerons en détail chaque widgetset :

- **5.2.1** Win32/Win64 pour Windows - L'implémentation native Windows
- **5.2.2** GTK2/GTK3 pour Ubuntu/Linux - Les toolkits GNOME
- **5.2.3** Qt5 comme alternative universelle - Le framework cross-platform
- **5.2.4** Custom Drawn pour contrôle total - Dessiner ses propres widgets

Chaque section approfondira les spécificités, avantages, limitations et meilleures pratiques pour chaque widgetset.

⏭️ [Win32/Win64 pour Windows](/05-developpement-multiplateforme-approfondi/02.1-win32-win64-windows.md)
