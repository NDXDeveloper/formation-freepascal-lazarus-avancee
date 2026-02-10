🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Ressources et icônes multi-plateformes dans FreePascal/Lazarus

## Introduction

Lors du développement d'applications multi-plateformes avec FreePascal/Lazarus, la gestion des ressources (images, icônes, fichiers de données) représente un défi important. Chaque système d'exploitation a ses propres formats, conventions et méthodes de stockage. Ce tutoriel vous guidera pas à pas pour créer des applications qui fonctionnent parfaitement sous Windows et Ubuntu/Linux.

## Comprendre les différences entre plateformes

### Formats d'icônes par système

**Windows** utilise principalement :
- `.ico` : Format natif pour les icônes d'application
- `.bmp` : Images bitmap simples
- `.png` : Images avec transparence

**Linux/Ubuntu** utilise :
- `.png` : Format standard pour toutes les icônes
- `.svg` : Images vectorielles (recommandé pour la scalabilité)
- `.xpm` : Format texte historique (moins utilisé aujourd'hui)

### Résolutions nécessaires

Pour une application moderne, vous devez prévoir plusieurs tailles d'icônes :

**Windows** :
- 16×16 pixels (barre des tâches, menus)
- 32×32 pixels (bureau, raccourcis)
- 48×48 pixels (vues détaillées)
- 256×256 pixels (Windows Vista et plus récent)

**Linux/Ubuntu** :
- 16×16, 22×22, 24×24 pixels (barres d'outils)
- 32×32 pixels (menus)
- 48×48 pixels (bureau)
- 64×64, 128×128 pixels (gestionnaires de fichiers)
- Format SVG scalable (idéal)

## Configuration dans Lazarus

### Étape 1 : Organisation des fichiers de ressources

Créez une structure de dossiers claire dans votre projet :

```
MonProjet/
├── resources/
│   ├── icons/
│   │   ├── windows/
│   │   │   └── app.ico
│   │   ├── linux/
│   │   │   ├── app16.png
│   │   │   ├── app32.png
│   │   │   ├── app48.png
│   │   │   └── app.svg
│   │   └── common/
│   │       ├── toolbar/
│   │       └── images/
│   └── data/
│       └── config.xml
```

### Étape 2 : Création d'un fichier de ressources portable

Dans Lazarus, créez un nouveau fichier de ressources (`.lrs` ou `.rc`) qui sera compilé avec votre application.

Pour un fichier `.rc` (recommandé) :

```pascal
// resources.rc
MAINICON ICON "resources/icons/windows/app.ico"

// Images communes
IMG_OPEN BITMAP "resources/common/toolbar/open.bmp"  
IMG_SAVE BITMAP "resources/common/toolbar/save.bmp"
```

### Étape 3 : Inclusion conditionnelle des ressources

Dans votre code source principal, utilisez la compilation conditionnelle :

```pascal
program MonApplication;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces;

{$R *.res}

// Inclusion conditionnelle des ressources selon l'OS
{$IFDEF WINDOWS}
  {$R resources_windows.rc}
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF LINUX}
    {$R resources_linux.rc}
  {$ENDIF}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrincipale, FormPrincipale);
  Application.Run;
end.
```

## Gestion des icônes d'application

### Configuration de l'icône principale

#### Dans l'IDE Lazarus

1. Ouvrez les **Options du projet** (Projet → Options du projet)
2. Allez dans **Application**
3. Dans la section **Icône**, cliquez sur **Charger une icône**
4. Sélectionnez votre fichier `.ico` (Windows) ou `.png` (Linux)

#### Par programmation

```pascal
procedure TFormPrincipale.FormCreate(Sender: TObject);  
begin
  {$IFDEF WINDOWS}
    Application.Icon.LoadFromFile('resources/icons/windows/app.ico');
  {$ENDIF}

  {$IFDEF UNIX}
    Application.Icon.LoadFromFile('resources/icons/linux/app48.png');
  {$ENDIF}
end;
```

### Création d'icônes multi-résolution

Pour Windows, créez un fichier `.ico` contenant plusieurs résolutions. Utilisez un outil comme :
- **GIMP** (gratuit, multi-plateforme)
- **IcoFX** (Windows)
- **Greenfish Icon Editor Pro** (gratuit)

Étapes dans GIMP :
1. Créez ou ouvrez votre image
2. Redimensionnez-la en plusieurs tailles (256, 48, 32, 16 pixels)
3. Exportez en `.ico` avec l'option "Enregistrer toutes les tailles"

## Chargement dynamique des ressources

### Création d'un gestionnaire de ressources

Voici une classe simple pour gérer les ressources de manière portable :

```pascal
unit ResourceManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TResourceManager = class
  private
    FResourcePath: string;
    function GetPlatformPath: string;
  public
    constructor Create;
    function LoadImage(const AName: string): TPicture;
    function LoadIcon(const AName: string; ASize: Integer = 32): TIcon;
    function GetResourcePath(const AResource: string): string;
  end;

var
  ResManager: TResourceManager;

implementation

constructor TResourceManager.Create;  
begin
  inherited Create;
  FResourcePath := ExtractFilePath(ParamStr(0)) + 'resources' + PathDelim;
end;

function TResourceManager.GetPlatformPath: string;  
begin
  {$IFDEF WINDOWS}
    Result := FResourcePath + 'windows' + PathDelim;
  {$ENDIF}

  {$IFDEF UNIX}
    Result := FResourcePath + 'linux' + PathDelim;
  {$ENDIF}
end;

function TResourceManager.LoadImage(const AName: string): TPicture;  
var
  ImagePath: string;
begin
  Result := TPicture.Create;

  // Chercher d'abord dans les ressources communes
  ImagePath := FResourcePath + 'common' + PathDelim + AName;

  if not FileExists(ImagePath) then
    ImagePath := GetPlatformPath + AName;

  if FileExists(ImagePath) then
    Result.LoadFromFile(ImagePath);
end;

function TResourceManager.LoadIcon(const AName: string; ASize: Integer): TIcon;  
var
  IconPath: string;
  IconName: string;
begin
  Result := TIcon.Create;

  {$IFDEF WINDOWS}
    IconName := AName + '.ico';
  {$ENDIF}

  {$IFDEF UNIX}
    IconName := AName + IntToStr(ASize) + '.png';
  {$ENDIF}

  IconPath := GetPlatformPath + 'icons' + PathDelim + IconName;

  if FileExists(IconPath) then
    Result.LoadFromFile(IconPath);
end;

function TResourceManager.GetResourcePath(const AResource: string): string;  
begin
  Result := FResourcePath + AResource;
end;

initialization
  ResManager := TResourceManager.Create;

finalization
  ResManager.Free;

end.
```

### Utilisation du gestionnaire

```pascal
procedure TFormPrincipale.LoadResources;  
var
  BtnIcon: TIcon;
begin
  // Charger une icône de bouton
  BtnIcon := ResManager.LoadIcon('save', 24);
  try
    BitBtnSave.Glyph.Assign(BtnIcon);
  finally
    BtnIcon.Free;
  end;

  // Charger une image
  ImageLogo.Picture := ResManager.LoadImage('logo.png');
end;
```

## Gestion des images dans les composants

### Images pour les boutons et menus

Lazarus propose la composant `TImageList` pour gérer les collections d'images :

1. Déposez un `TImageList` sur votre formulaire
2. Définissez les propriétés :
   - `Width` et `Height` : taille des images (16, 24, ou 32 pixels généralement)
   - `Scaled` : `True` pour le support du High-DPI
3. Double-cliquez pour ajouter des images

```pascal
procedure TFormPrincipale.InitializeImageList;  
begin
  ImageList1.Clear;

  // Ajouter des images selon la plateforme
  {$IFDEF WINDOWS}
    ImageList1.AddIcon(ResManager.LoadIcon('new', 16));
    ImageList1.AddIcon(ResManager.LoadIcon('open', 16));
    ImageList1.AddIcon(ResManager.LoadIcon('save', 16));
  {$ENDIF}

  {$IFDEF UNIX}
    // Sur Linux, utiliser des PNG
    ImageList1.Add(ResManager.LoadImage('new16.png').Bitmap, nil);
    ImageList1.Add(ResManager.LoadImage('open16.png').Bitmap, nil);
    ImageList1.Add(ResManager.LoadImage('save16.png').Bitmap, nil);
  {$ENDIF}
end;
```

## Support du High-DPI

### Configuration pour écrans haute résolution

Les écrans modernes ont des résolutions élevées. Votre application doit s'adapter :

```pascal
procedure TFormPrincipale.FormCreate(Sender: TObject);  
begin
  // Activer la mise à l'échelle automatique
  Self.Scaled := True;

  // Détecter le DPI du système
  if Screen.PixelsPerInch > 96 then
  begin
    // Charger des icônes plus grandes
    LoadHighDPIResources;
  end;
end;

procedure TFormPrincipale.LoadHighDPIResources;  
var
  IconSize: Integer;
begin
  // Calculer la taille d'icône appropriée
  IconSize := Round(32 * Screen.PixelsPerInch / 96);

  // Charger les icônes adaptées
  Application.Icon := ResManager.LoadIcon('app', IconSize);
end;
```

## Ressources embarquées vs externes

### Avantages des ressources embarquées

- **Fichier unique** : Tout est dans l'exécutable
- **Protection** : Les ressources ne peuvent pas être modifiées facilement
- **Performance** : Chargement plus rapide

### Avantages des ressources externes

- **Flexibilité** : Modification sans recompilation
- **Taille** : Exécutable plus petit
- **Personnalisation** : L'utilisateur peut modifier les thèmes

### Implémentation hybride

```pascal
function TResourceManager.LoadImageSmart(const AName: string): TPicture;  
var
  ExternalPath: string;
  ResStream: TResourceStream;
begin
  Result := TPicture.Create;

  // Vérifier d'abord les fichiers externes (priorité à la personnalisation)
  ExternalPath := GetResourcePath('custom' + PathDelim + AName);
  if FileExists(ExternalPath) then
  begin
    Result.LoadFromFile(ExternalPath);
    Exit;
  end;

  // Sinon, utiliser les ressources embarquées
  try
    ResStream := TResourceStream.Create(HInstance,
                   ChangeFileExt(AName, ''), RT_RCDATA);
    try
      Result.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
  except
    // En dernier recours, charger depuis le dossier par défaut
    ExternalPath := GetResourcePath('default' + PathDelim + AName);
    if FileExists(ExternalPath) then
      Result.LoadFromFile(ExternalPath);
  end;
end;
```

## Optimisation et bonnes pratiques

### Cache des ressources

Pour éviter de recharger constamment les mêmes ressources :

```pascal
type
  TResourceCache = class
  private
    FImages: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetImage(const AName: string): TPicture;
  end;

function TResourceCache.GetImage(const AName: string): TPicture;  
var
  Index: Integer;
begin
  Index := FImages.IndexOf(AName);
  if Index >= 0 then
    Result := TPicture(FImages.Objects[Index])
  else
  begin
    Result := ResManager.LoadImage(AName);
    FImages.AddObject(AName, Result);
  end;
end;
```

### Formats recommandés

- **Icônes d'application** : `.ico` (Windows), `.png` (Linux)
- **Images de l'interface** : `.png` pour la transparence
- **Photos et images complexes** : `.jpg` pour la taille réduite
- **Graphiques vectoriels** : `.svg` quand supporté

### Outils de conversion

Pour maintenir vos ressources synchronisées entre plateformes :

1. **ImageMagick** (ligne de commande) :
```bash
# Convertir ICO en plusieurs PNG
convert app.ico app%d.png

# Créer un ICO depuis plusieurs PNG
convert app16.png app32.png app48.png app.ico
```

2. **Script de conversion automatique** (bash) :
```bash
#!/bin/bash
# convert_resources.sh
for icon in resources/icons/source/*.svg; do
  filename=$(basename "$icon" .svg)
  inkscape -w 16 -h 16 "$icon" -o "resources/linux/${filename}16.png"
  inkscape -w 32 -h 32 "$icon" -o "resources/linux/${filename}32.png"
  inkscape -w 48 -h 48 "$icon" -o "resources/linux/${filename}48.png"
done
```

## Débogage et résolution de problèmes

### Problèmes courants et solutions

**Icône n'apparaît pas sous Linux :**
- Vérifiez les permissions du fichier
- Assurez-vous que le format est supporté (PNG recommandé)
- Testez avec différentes tailles

**Images floues sur écrans High-DPI :**
- Fournissez des versions haute résolution
- Utilisez des images vectorielles quand possible
- Activez le support DPI dans l'application

**Ressources non trouvées après déploiement :**
- Vérifiez les chemins relatifs vs absolus
- Incluez les ressources dans l'installateur
- Testez sur une machine vierge

### Vérification des ressources au démarrage

```pascal
procedure TFormPrincipale.CheckResources;  
var
  MissingResources: TStringList;
begin
  MissingResources := TStringList.Create;
  try
    // Vérifier les ressources critiques
    if not FileExists(ResManager.GetResourcePath('icons/app.ico')) then
      MissingResources.Add('Icône principale');

    if not FileExists(ResManager.GetResourcePath('data/config.xml')) then
      MissingResources.Add('Fichier de configuration');

    if MissingResources.Count > 0 then
    begin
      ShowMessage('Ressources manquantes :' + LineEnding +
                  MissingResources.Text);
      Application.Terminate;
    end;
  finally
    MissingResources.Free;
  end;
end;
```

## Conclusion

La gestion des ressources multi-plateformes dans FreePascal/Lazarus demande une planification initiale, mais avec les bonnes pratiques présentées dans ce tutoriel, vous pouvez créer des applications qui s'adaptent parfaitement à chaque système d'exploitation. L'essentiel est de :

1. Organiser vos ressources de manière logique
2. Utiliser la compilation conditionnelle intelligemment
3. Prévoir plusieurs formats et résolutions
4. Tester sur les différentes plateformes cibles
5. Implémenter un système de cache pour les performances

En suivant ces principes, vos applications auront une apparence native et professionnelle sur Windows comme sur Ubuntu/Linux.

⏭️  [Tests cross-platform automatisés](/05-developpement-multiplateforme-approfondi/06-tests-cross-platform-automatises.md)
