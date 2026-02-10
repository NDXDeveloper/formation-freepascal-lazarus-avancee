🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.6 iOS avec FreePascal (expérimental)

## Introduction

Le développement iOS avec FreePascal est actuellement dans une phase **expérimentale**. Contrairement à Android où le support est mature grâce à LAMW (Lazarus Android Module Wizard), le développement pour iOS nécessite une approche plus technique et comporte certaines limitations.

> ⚠️ **Attention** : Cette fonctionnalité est expérimentale et peut nécessiter des connaissances avancées en compilation croisée et en écosystème Apple.

## Prérequis

### Matériel et système d'exploitation

Pour développer pour iOS avec FreePascal, vous aurez besoin de :

- **Un Mac** avec macOS (obligatoire pour la compilation finale et le déploiement)
- **Xcode** installé (disponible gratuitement sur le Mac App Store)
- **Outils en ligne de commande Xcode** (`xcode-select --install`)
- **Un compte développeur Apple** (gratuit pour les tests, payant pour la distribution)
- **Un appareil iOS** (iPhone/iPad) pour les tests réels (optionnel mais recommandé)

### Logiciels nécessaires

- **FreePascal** version 3.2.0 ou supérieure
- **Lazarus** (optionnel mais recommandé pour l'IDE)
- **FPC** compilé avec support iOS (cross-compilation)

## État actuel du support iOS

### Ce qui fonctionne

- ✅ Compilation de code Pascal pour architecture ARM64 (iPhone 5s et supérieurs)
- ✅ Bibliothèques système et accès aux API Objective-C via bindings
- ✅ Applications console iOS
- ✅ Accès aux frameworks natifs (UIKit, Foundation, CoreGraphics, etc.)
- ✅ Intégration avec le système de build Xcode

### Limitations actuelles

- ❌ Pas de support LCL (Lazarus Component Library) natif
- ❌ Pas de concepteur visuel d'interface intégré à Lazarus
- ❌ Documentation limitée et communauté réduite
- ❌ Nécessite une bonne compréhension de l'écosystème Apple
- ❌ Débogage plus complexe qu'avec Xcode natif

## Installation et configuration

### Étape 1 : Installer FPC avec support iOS

Sur macOS, vous devez compiler FPC avec le support des cibles iOS :

```bash
# Télécharger les sources de FreePascal
git clone https://gitlab.com/freepascal.org/fpc/source.git fpc-source  
cd fpc-source

# Compiler FPC avec support iOS
make clean  
make all OS_TARGET=darwin CPU_TARGET=aarch64

# Installer
sudo make install OS_TARGET=darwin CPU_TARGET=aarch64
```

### Étape 2 : Configuration des cross-compilateurs

FreePascal doit être configuré pour cibler les architectures iOS :

```bash
# Installer les binutils pour iOS
fpcupdeluxe # Outil recommandé pour gérer les cross-compilateurs
```

Ou manuellement :

```bash
# Configurer le compilateur pour iOS
fpc -Pi386 -Tiphonesim # Pour le simulateur iOS (x86_64)  
fpc -Paarch64 -Tdarwin # Pour les appareils iOS réels (ARM64)
```

### Étape 3 : Fichier de configuration FPC

Créez un fichier `fpc.cfg` spécifique pour iOS :

```ini
# Configuration pour iOS
-Tdarwin
-Paarch64

# Chemins vers les SDK iOS
-XR/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk

# Frameworks nécessaires
-k-framework -kUIKit
-k-framework -kFoundation
-k-framework -kCoreGraphics

# Options de linkage
-k-ios_version_min -k12.0
```

## Structure d'un projet iOS minimal

### Programme principal (app.pas)

```pascal
program iOSApp;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

uses
  iPhoneAll, // Unité principale pour iOS
  CGGeometry;

type
  // Délégué de l'application
  TAppDelegate = objcclass(NSObject, UIApplicationDelegateProtocol)
  private
    window: UIWindow;
  public
    function application_didFinishLaunchingWithOptions(
      application: UIApplication;
      options: NSDictionary): Boolean; message 'application:didFinishLaunchingWithOptions:';
  end;

function TAppDelegate.application_didFinishLaunchingWithOptions(
  application: UIApplication;
  options: NSDictionary): Boolean;
var
  screenBounds: CGRect;
  label_: UILabel;
begin
  // Créer la fenêtre principale
  screenBounds := UIScreen.mainScreen.bounds;
  window := UIWindow.alloc.initWithFrame(screenBounds);

  // Créer un label simple
  label_ := UILabel.alloc.initWithFrame(CGRectMake(20, 100, 280, 50));
  label_.setText(NSSTR('Bonjour depuis FreePascal!'));
  label_.setTextAlignment(UITextAlignmentCenter);

  // Ajouter le label à la fenêtre
  window.addSubview(label_);
  window.makeKeyAndVisible;

  Result := True;
end;

// Point d'entrée de l'application
begin
  UIApplicationMain(argc, argv, nil, TAppDelegate.ClassName);
end.
```

### Fichier Info.plist

Chaque application iOS nécessite un fichier `Info.plist` :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleDevelopmentRegion</key>
    <string>fr_FR</string>

    <key>CFBundleExecutable</key>
    <string>iOSApp</string>

    <key>CFBundleIdentifier</key>
    <string>com.example.iosapp</string>

    <key>CFBundleName</key>
    <string>iOSApp</string>

    <key>CFBundlePackageType</key>
    <string>APPL</string>

    <key>CFBundleShortVersionString</key>
    <string>1.0</string>

    <key>CFBundleVersion</key>
    <string>1</string>

    <key>LSRequiresIPhoneOS</key>
    <true/>

    <key>UIRequiredDeviceCapabilities</key>
    <array>
        <string>arm64</string>
    </array>

    <key>UISupportedInterfaceOrientations</key>
    <array>
        <string>UIInterfaceOrientationPortrait</string>
    </array>
</dict>
</plist>
```

## Compilation

### Compilation pour le simulateur iOS

```bash
# Compiler pour simulateur (x86_64)
fpc -Pi386 -Tiphonesim -Cn app.pas

# Créer le bundle d'application
mkdir -p iOSApp.app  
cp app iOSApp.app/  
cp Info.plist iOSApp.app/
```

### Compilation pour appareil iOS réel

```bash
# Compiler pour ARM64
fpc -Paarch64 -Tdarwin app.pas

# Signer l'application (nécessite un certificat de développeur)
codesign -s "iPhone Developer" iOSApp.app
```

## Accès aux API iOS natives

### Utilisation d'Objective-C depuis Pascal

FreePascal permet d'appeler directement les API Objective-C grâce au mode `{$modeswitch objectivec1}` :

```pascal
{$mode objfpc}{$H+}
{$modeswitch objectivec1}

uses
  iPhoneAll, CocoaAll;

var
  alert: UIAlertController;
  action: UIAlertAction;
begin
  // Créer une alerte iOS native
  alert := UIAlertController.alertControllerWithTitle_message_preferredStyle(
    NSSTR('Titre'),
    NSSTR('Message de l''alerte'),
    UIAlertControllerStyleAlert
  );

  // Ajouter un bouton
  action := UIAlertAction.actionWithTitle_style_handler(
    NSSTR('OK'),
    UIAlertActionStyleDefault,
    nil
  );

  alert.addAction(action);

  // Afficher l'alerte (nécessite un view controller)
  // viewController.presentViewController_animated_completion(alert, True, nil);
end;
```

### Bindings des frameworks iOS

Les principaux frameworks iOS sont disponibles via des unités FreePascal :

```pascal
uses
  // Frameworks de base
  iPhoneAll,        // Unité principale iOS
  Foundation,       // Classes de base (NSString, NSArray, etc.)
  UIKit,           // Interface utilisateur
  CoreGraphics,    // Graphiques 2D
  CoreAnimation,   // Animations

  // Frameworks spécialisés
  CoreLocation,    // Géolocalisation
  AVFoundation,    // Audio/Vidéo
  CoreData,        // Base de données
  MapKit,          // Cartes
  WebKit;          // Navigateur web
```

## Gestion de l'interface utilisateur

### Création d'une interface simple

```pascal
type
  TMyViewController = objcclass(UIViewController)
  private
    myButton: UIButton;
  public
    procedure viewDidLoad; override;
    procedure buttonPressed(sender: id); message 'buttonPressed:';
  end;

procedure TMyViewController.viewDidLoad;  
var
  frame: CGRect;
begin
  inherited viewDidLoad;

  // Définir la couleur de fond
  view.setBackgroundColor(UIColor.whiteColor);

  // Créer un bouton
  frame := CGRectMake(50, 100, 220, 50);
  myButton := UIButton.buttonWithType(UIButtonTypeSystem);
  myButton.setFrame(frame);
  myButton.setTitle_forState(NSSTR('Cliquez-moi'), UIControlStateNormal);

  // Ajouter une action au bouton
  myButton.addTarget_action_forControlEvents(
    self,
    sel_registerName('buttonPressed:'),
    UIControlEventTouchUpInside
  );

  // Ajouter le bouton à la vue
  view.addSubview(myButton);
end;

procedure TMyViewController.buttonPressed(sender: id);  
begin
  WriteLn('Bouton pressé!');
  // Afficher une alerte, changer de vue, etc.
end;
```

## Débogage

### Logs et debugging

Pour afficher des messages de débogage :

```pascal
uses
  iPhoneAll;

begin
  // Utiliser NSLog (apparaît dans la console Xcode)
  NSLog(NSSTR('Message de débogage: %@'), NSSTR('valeur'));

  // Ou WriteLn pour les logs simples
  WriteLn('Log FreePascal');
end;
```

### Débogage avec Xcode

1. Compilez votre application avec les symboles de débogage : `-g`
2. Ouvrez le projet dans Xcode
3. Lancez l'application depuis Xcode
4. Utilisez les outils de débogage Xcode (breakpoints, inspection de mémoire, etc.)

## Déploiement

### Préparation pour l'App Store

Pour distribuer votre application sur l'App Store :

1. **Créer un profil de provisionnement** dans le portail développeur Apple
2. **Compiler en mode Release** avec optimisations :
   ```bash
   fpc -O3 -Paarch64 -Tdarwin app.pas
   ```
3. **Signer l'application** avec votre certificat de distribution
4. **Créer une archive IPA** :
   ```bash
   xcrun -sdk iphoneos PackageApplication -v iOSApp.app -o iOSApp.ipa
   ```
5. **Soumettre via Application Loader** ou Xcode

### Tests TestFlight

TestFlight permet de distribuer des versions de test :

1. Compiler et signer l'application
2. Télécharger sur App Store Connect
3. Inviter des testeurs via email
4. Les testeurs téléchargent l'app TestFlight

## Alternatives et solutions hybrides

### Custom Drawn Widgetset

Pour une interface plus portable, vous pouvez utiliser le widgetset Custom Drawn de Lazarus :

```pascal
{$mode objfpc}{$H+}

uses
  Forms, CustomDrawnInt, LCLType;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
```

Cette approche dessine l'interface manuellement, permettant une meilleure portabilité, mais avec des performances réduites.

### Utiliser Pas2JS pour des applications web iOS

Une alternative est de créer une **Progressive Web App** avec Pas2JS :

```pascal
// Code Pascal compilé en JavaScript
program WebApp;

{$mode objfpc}

uses
  JS, Web, Types;

begin
  document.getElementById('myButton').addEventListener('click',
    procedure(event: TJSEvent)
    begin
      window.alert('Bonjour depuis Pas2JS!');
    end
  );
end.
```

Avantages :
- Fonctionne sur iOS sans compilation native
- Pas besoin de compte développeur
- Distribution via web (pas d'App Store)

Inconvénients :
- Performances limitées
- Accès restreint aux API natives
- Expérience utilisateur moins native

## Ressources et communauté

### Documentation officielle

- **Wiki FreePascal iOS** : https://wiki.freepascal.org/iOS
- **Forum FreePascal** : https://forum.lazarus.freepascal.org/
- **Documentation Apple** : https://developer.apple.com/documentation/

### Projets de référence

Quelques projets open source utilisant FreePascal pour iOS :

- **PascalGames** : Moteur de jeu simple pour iOS
- **iOS Bindings** : Collection de bindings pour frameworks iOS
- **FPC iOS Demos** : Exemples de code dans les sources FPC

### Communauté et support

- **Mailing list FPC** : Discussion technique
- **Telegram FreePascal** : Support communautaire rapide
- **GitHub** : Nombreux exemples et bibliothèques

## Comparaison avec d'autres solutions

### FreePascal vs Swift/Objective-C natif

| Critère | FreePascal | Swift/Obj-C |
|---------|-----------|-------------|
| Courbe d'apprentissage | Moyenne | Élevée |
| Performance | Excellente | Excellente |
| Support iOS | Expérimental | Complet |
| Documentation | Limitée | Excellente |
| Outils de développement | Basiques | Avancés (Xcode) |
| Portabilité du code | Élevée | Faible |

### FreePascal vs Frameworks cross-platform

| Critère | FreePascal | Flutter | React Native | Xamarin |
|---------|-----------|---------|--------------|---------|
| Langage | Pascal | Dart | JavaScript | C# |
| Natif | Oui | Partiellement | Non | Oui |
| Taille app | Petite | Moyenne | Grande | Grande |
| Maturité iOS | Expérimentale | Mature | Mature | Mature |

## Conseils et bonnes pratiques

### 1. Commencer petit

Débutez avec des applications simples (calculatrice, liste de tâches) avant de vous lancer dans des projets complexes.

### 2. Maîtriser Objective-C

Une bonne compréhension d'Objective-C et des patterns iOS est essentielle, car vous appellerez directement ces API.

### 3. Utiliser un Mac pour le développement

Bien que la cross-compilation depuis Windows/Linux soit théoriquement possible, le développement sur Mac est fortement recommandé.

### 4. Tester régulièrement sur appareil réel

Le simulateur ne remplace pas les tests sur appareil physique, surtout pour les performances et les capteurs.

### 5. Suivre les mises à jour FPC

Le support iOS évolue avec chaque version de FreePascal. Restez à jour avec les dernières versions.

### 6. Contribuer à la communauté

Si vous développez des bindings ou des exemples, partagez-les avec la communauté pour faire progresser l'écosystème.

## Perspectives futures

Le support iOS dans FreePascal continue d'évoluer. Les développements en cours incluent :

- **Amélioration des bindings** pour les frameworks iOS récents
- **Support SwiftUI** via des bindings
- **Meilleure intégration avec Xcode**
- **Support macOS/Catalyst** pour applications universelles
- **Outils de déploiement simplifiés**

---

## Conclusion

Le développement iOS avec FreePascal est une aventure **expérimentale** mais **prometteuse**. Si vous êtes un développeur Pascal expérimenté cherchant à étendre vos compétences à la plateforme iOS, cette approche offre une alternative intéressante aux langages natifs d'Apple.

Cependant, pour des projets commerciaux critiques, il est recommandé d'utiliser les outils natifs (Swift/Xcode) ou des frameworks cross-platform plus matures (Flutter, React Native) jusqu'à ce que le support FreePascal iOS atteigne une maturité suffisante.

**Prochaines étapes suggérées** :
- Expérimenter avec des exemples simples
- Explorer les bindings de frameworks iOS
- Rejoindre la communauté FreePascal
- Contribuer aux efforts de développement iOS

> 💡 **Astuce** : Si vous ciblez principalement iOS, envisagez également le développement web avec Pas2JS ou l'utilisation de Custom Drawn pour maximiser la portabilité de votre code Pascal.

⏭️ [Raspberry Pi](/13-developpement-mobile-embarque/07-raspberry-pi.md)
