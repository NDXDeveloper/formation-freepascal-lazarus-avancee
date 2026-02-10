🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.9 Thèmes et apparence personnalisée

## Introduction : L'importance de l'apparence visuelle

Dans le monde du développement d'applications modernes, l'apparence visuelle n'est plus un simple bonus esthétique, c'est un élément fondamental de l'expérience utilisateur. Une interface bien conçue et visuellement cohérente peut faire la différence entre une application que les utilisateurs adorent et une qu'ils évitent. Avec Lazarus et la LCL, vous avez accès à un système sophistiqué de personnalisation visuelle qui respecte les conventions de chaque plateforme tout en vous permettant d'exprimer votre créativité.

Imaginez que votre application soit comme un caméléon : capable de s'adapter parfaitement à son environnement (le système d'exploitation et les préférences de l'utilisateur) tout en conservant sa propre identité visuelle distinctive. C'est exactement ce que permet le système de thèmes de Lazarus.

## Comprendre les thèmes dans Lazarus

### Qu'est-ce qu'un thème ?

Un thème est un ensemble cohérent de règles visuelles qui définissent l'apparence de tous les éléments d'interface de votre application. Cela inclut :

- **Les couleurs** : Arrière-plans, textes, bordures, surbrillances
- **Les polices** : Familles, tailles, styles pour différents contextes
- **Les dimensions** : Hauteurs des barres, largeurs des bordures, espacements
- **Les effets visuels** : Ombres, gradients, animations, transparences
- **Les icônes et images** : Styles graphiques cohérents
- **Les comportements visuels** : Réactions aux survols, clics, focus

### La philosophie multi-plateforme des thèmes

La LCL adopte une approche unique pour la gestion des thèmes. Plutôt que d'imposer un look uniforme sur toutes les plateformes (ce qui donnerait des applications qui semblent étrangères), elle embrasse les différences tout en offrant des points de personnalisation communs.

Cette approche repose sur trois piliers :

1. **Respect de la plateforme** : L'application utilise par défaut les thèmes natifs du système
2. **Points d'extension** : Des mécanismes pour personnaliser sans casser l'intégration native
3. **Abstraction intelligente** : Une API commune qui s'adapte aux spécificités de chaque OS

## Architecture du système de thèmes

### Les couches de personnalisation

Le système de thèmes de Lazarus fonctionne en couches, chacune offrant différents niveaux de contrôle :

```
┌─────────────────────────────────────┐
│     Personnalisation complète       │ ← Votre code custom
├─────────────────────────────────────┤
│     Surcharge de composants         │ ← Composants personnalisés
├─────────────────────────────────────┤
│     Propriétés et styles            │ ← Configuration LCL
├─────────────────────────────────────┤
│     Widgetset themes                │ ← Thèmes du widgetset
├─────────────────────────────────────┤
│     Thème système natif             │ ← OS (Windows, GTK, Qt, etc.)
└─────────────────────────────────────┘
```

Chaque couche peut modifier ou compléter celle du dessous, offrant une flexibilité progressive depuis les simples ajustements jusqu'au contrôle total du rendu.

### Les différents types de personnalisation

#### 1. Personnalisation par propriétés
La méthode la plus simple et la plus portable. Vous modifiez les propriétés standard des composants :

```pascal
Button1.Color := clSkyBlue;  
Button1.Font.Name := 'Segoe UI';  
Button1.Font.Size := 10;
```

**Avantages** : Simple, portable, respecte les guidelines de chaque OS  
**Limites** : Contrôle limité sur certains aspects visuels  

#### 2. Personnalisation par styles
Utilisation de feuilles de style ou de thèmes prédéfinis :

```pascal
Application.SetStyle('Dark');
// ou
ThemeManager.LoadTheme('MonTheme.theme');
```

**Avantages** : Changement global rapide, cohérence garantie  
**Limites** : Dépend du support du widgetset  

#### 3. Personnalisation par surcharge
Création de composants personnalisés avec leur propre rendu :

```pascal
TThemedButton = class(TButton)  
protected
  procedure Paint; override;
end;
```

**Avantages** : Contrôle total, effets visuels avancés  
**Limites** : Plus complexe, peut perdre l'aspect natif  

#### 4. Personnalisation par interception
Modification du comportement de rendu au niveau système :

```pascal
ThemeServices.DrawElement(Canvas, Details, Rect);
```

**Avantages** : Intégration avec le système de thèmes natif  
**Limites** : Spécifique à la plateforme  

## Les composants du système de thèmes

### ThemeServices : Le cœur du système

`ThemeServices` est l'interface principale pour accéder aux capacités de thématisation du système. C'est un singleton qui fournit :

- Accès aux couleurs système
- Dessin d'éléments thématiques
- Information sur le thème actuel
- Notification des changements de thème

### Gestion des couleurs

Le système de couleurs de Lazarus distingue plusieurs catégories :

#### Couleurs système
Des constantes qui s'adaptent automatiquement au thème :
- `clBtnFace` : Couleur de fond des boutons
- `clWindow` : Fond des fenêtres de contenu
- `clWindowText` : Texte dans les fenêtres
- `clHighlight` : Couleur de sélection
- `clGrayText` : Texte désactivé

#### Couleurs standard
Des couleurs fixes indépendantes du thème :
- `clRed`, `clBlue`, `clGreen` : Couleurs de base
- `clSilver`, `clGray` : Nuances de gris
- Couleurs web : `clWebSlateBlue`, `clWebCoral`, etc.

#### Couleurs personnalisées
Définies par code RGB :
```pascal
MyColor := RGBToColor(64, 128, 192);
// ou en hexadécimal
MyColor := $C08040; // Format BGR
```

### Gestion des polices

Les polices sont un aspect crucial de la personnalisation. La LCL offre plusieurs approches :

#### Polices système
```pascal
Font.Name := 'default'; // Police par défaut du système
```

#### Polices logiques
Des alias qui s'adaptent à la plateforme :
- `'Sans'` : Police sans-serif du système
- `'Serif'` : Police serif du système
- `'Monospace'` : Police à chasse fixe

#### Polices spécifiques
```pascal
if Screen.Fonts.IndexOf('Segoe UI') >= 0 then
  Font.Name := 'Segoe UI'
else if Screen.Fonts.IndexOf('Ubuntu') >= 0 then
  Font.Name := 'Ubuntu'
else
  Font.Name := 'default';
```

## Défis de la personnalisation multi-plateforme

### Différences entre plateformes

Chaque système d'exploitation a ses propres conventions et limitations :

**Windows** :
- Support natif des thèmes visuels depuis XP
- API UxTheme pour le dessin thématique
- Styles visuels modifiables par manifeste
- Dark mode natif depuis Windows 10

**Linux/GTK** :
- Thèmes GTK2/GTK3 très flexibles
- Support CSS pour GTK3
- Intégration avec les environnements de bureau (GNOME, KDE, XFCE)
- Grande variété de thèmes disponibles

**macOS** :
- Guidelines strictes d'Apple (Human Interface Guidelines)
- Support limité de la personnalisation
- Aqua et Dark mode
- Intégration avec les préférences système

**Qt (cross-platform)** :
- Système de styles Qt
- QSS (Qt Style Sheets) similaire au CSS
- Cohérence entre plateformes
- Thèmes personnalisables

### Stratégies d'adaptation

Pour créer une application qui soit belle sur toutes les plateformes, plusieurs stratégies s'offrent à vous :

#### 1. Approche minimaliste
Utiliser uniquement les propriétés communes et laisser le système gérer l'apparence :

```pascal
// Code identique sur toutes les plateformes
Button1.Caption := 'Valider';  
Button1.Default := True;
// L'apparence s'adapte automatiquement
```

#### 2. Approche conditionnelle
Adapter le code selon la plateforme :

```pascal
{$IFDEF WINDOWS}
  Panel1.Color := clWindow;
  Panel1.BorderStyle := bsSingle;
{$ENDIF}
{$IFDEF LINUX}
  Panel1.Color := clForm;
  Panel1.BorderStyle := bsNone;
{$ENDIF}
```

#### 3. Approche abstraite
Créer une couche d'abstraction pour la gestion des thèmes :

```pascal
type
  TThemeAdapter = class
    procedure ApplyTheme(Control: TControl); virtual; abstract;
  end;

  TWindowsThemeAdapter = class(TThemeAdapter)
    procedure ApplyTheme(Control: TControl); override;
  end;

  TGTKThemeAdapter = class(TThemeAdapter)
    procedure ApplyTheme(Control: TControl); override;
  end;
```

## Outils et ressources pour la personnalisation

### Inspecteur de thèmes

Un outil précieux pour comprendre comment les thèmes fonctionnent :

```pascal
type
  TThemeInspector = class
    class procedure DumpThemeColors;
    class procedure DumpSystemMetrics;
    class function GetCurrentThemeName: string;
    class procedure ListAvailableFonts;
  end;

class procedure TThemeInspector.DumpThemeColors;  
begin
  WriteLn('clBtnFace: ', ColorToString(clBtnFace));
  WriteLn('clWindow: ', ColorToString(clWindow));
  WriteLn('clWindowText: ', ColorToString(clWindowText));
  // ... autres couleurs système
end;
```

### Gestionnaire de thèmes

Un système centralisé pour gérer les thèmes de votre application :

```pascal
type
  TThemeManager = class
  private
    FCurrentTheme: string;
    FThemeSettings: TStringList;
  public
    procedure LoadTheme(const ThemeName: string);
    procedure SaveTheme(const ThemeName: string);
    procedure ApplyTheme;
    function ListAvailableThemes: TStringList;

    property CurrentTheme: string read FCurrentTheme;
  end;
```

## Préparation pour les sections suivantes

Les sections qui suivent exploreront en détail chaque aspect de la personnalisation :

- **4.9.1** : Comment exploiter les styles visuels Windows
- **4.9.2** : Maîtriser les thèmes GTK et Qt sur Linux
- **Plus loin** : Techniques avancées de personnalisation

Cette introduction vous a donné les bases conceptuelles nécessaires pour comprendre le système de thèmes de Lazarus. Vous êtes maintenant prêt à explorer les spécificités de chaque plateforme et à créer des applications visuellement impressionnantes tout en respectant les conventions de chaque système d'exploitation.

## Points clés à retenir

1. **Les thèmes ne sont pas que cosmétiques** : Ils affectent l'utilisabilité et l'expérience utilisateur
2. **Respectez la plateforme** : Les utilisateurs s'attendent à une certaine cohérence
3. **Personnalisez intelligemment** : Trouvez le bon équilibre entre identité visuelle et intégration native
4. **Testez sur chaque plateforme** : Ce qui est beau sur Windows peut être horrible sur Linux
5. **Pensez accessibilité** : Les thèmes doivent supporter les besoins d'accessibilité (contraste, taille de police)
6. **Documentez vos choix** : Les décisions de design doivent être compréhensibles et maintenables

La personnalisation visuelle est un art autant qu'une science. Avec les outils que Lazarus met à votre disposition, vous avez tout ce qu'il faut pour créer des applications qui non seulement fonctionnent bien, mais qui sont aussi un plaisir à utiliser et à regarder.

⏭️ [Thèmes Windows (styles visuels)](/04-framework-lcl/09.1-themes-windows-styles-visuels.md)
