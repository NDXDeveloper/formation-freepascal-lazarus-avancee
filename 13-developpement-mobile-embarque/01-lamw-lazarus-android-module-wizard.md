🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.1 LAMW - Lazarus Android Module Wizard

## Introduction

LAMW (Lazarus Android Module Wizard) est un outil puissant qui permet de développer des applications Android natives directement depuis l'IDE Lazarus. Il transforme votre code FreePascal en applications mobiles fonctionnelles, ouvrant ainsi le monde du développement mobile aux développeurs Pascal.

## Qu'est-ce que LAMW ?

LAMW est un package pour Lazarus qui ajoute la capacité de créer des applications Android. Contrairement à d'autres solutions qui utilisent des frameworks intermédiaires, LAMW génère de véritables applications Android natives en combinant :

- **Code Pascal** : Votre logique métier écrite en Object Pascal
- **Interface Java/Kotlin** : L'interface utilisateur Android native
- **JNI (Java Native Interface)** : Le pont entre Pascal et Java

### Avantages de LAMW

- **Réutilisation du code** : Utilisez vos compétences FreePascal existantes
- **Performance native** : Applications compilées en code machine ARM
- **Accès complet à l'API Android** : Tous les services Android sont accessibles
- **Multi-plateforme** : Développez depuis Windows ou Linux
- **Gratuit et open source** : Pas de coûts de licence

## Architecture de LAMW

### Comment ça fonctionne ?

```
┌─────────────────────────────────────┐
│   Code FreePascal (.pas)            │
│   - Logique métier                  │
│   - Traitement de données           │
└──────────────┬──────────────────────┘
               │
               │ JNI (Java Native Interface)
               │
┌──────────────▼──────────────────────┐
│   Code Java/Android                 │
│   - Interface utilisateur (UI)      │
│   - Services Android                │
└─────────────────────────────────────┘
               │
               │ Compilation
               │
┌──────────────▼──────────────────────┐
│   Application Android (.apk)        │
│   Compatible ARM/ARM64/x86          │
└─────────────────────────────────────┘
```

### Composants principaux

1. **Le module Pascal** : Contient votre logique applicative
2. **Les classes Java** : Gèrent l'interface utilisateur Android
3. **Le système de contrôles** : Composants visuels adaptés à Android
4. **Le système de build** : Compilation et packaging automatiques

## Installation de LAMW

### Prérequis

Avant d'installer LAMW, vous devez avoir :

#### Sur Windows ou Linux :

1. **Lazarus** installé et fonctionnel (version 2.0 ou supérieure recommandée)
2. **Android SDK** (Software Development Kit)
3. **Android NDK** (Native Development Kit)
4. **JDK** (Java Development Kit) version 8 ou supérieure
5. **Ant** ou **Gradle** pour le système de build

### Téléchargement de LAMW

LAMW est disponible via plusieurs méthodes :

- **OPM (Online Package Manager)** : Directement depuis Lazarus
- **GitHub** : Téléchargement manuel depuis le dépôt officiel
- **Forum Lazarus** : Versions packagées prêtes à l'emploi

### Configuration de l'environnement Android

#### Installation du SDK Android

Le SDK Android contient les outils nécessaires pour compiler et déboguer des applications Android.

**Sous Windows :**
```
C:\Users\VotreNom\AppData\Local\Android\Sdk
```

**Sous Linux :**
```
~/Android/Sdk
```

Composants essentiels à installer via SDK Manager :
- Android SDK Platform (version cible, ex: Android 13)
- Android SDK Build-Tools
- Android SDK Platform-Tools
- Android Emulator (optionnel, pour les tests)

#### Installation du NDK Android

Le NDK permet de compiler du code natif (C/C++/Pascal) pour Android.

**Versions recommandées :**
- NDK r21e ou r23b pour une compatibilité optimale avec LAMW

**Emplacement typique :**
- Windows : `C:\Users\VotreNom\AppData\Local\Android\Sdk\ndk\21.4.7075529`
- Linux : `~/Android/Sdk/ndk/21.4.7075529`

#### Installation du JDK

Le JDK Java est nécessaire pour compiler la partie Java de l'application.

**Versions compatibles :**
- OpenJDK 8, 11, ou 17
- Oracle JDK (alternative commerciale)

### Configuration de LAMW dans Lazarus

Une fois LAMW installé, vous devez configurer les chemins :

**Menu : Tools → LAMW → LAMW Settings**

Paramètres à configurer :

1. **JDK Path** : Chemin vers votre installation Java
2. **SDK Path** : Chemin vers le Android SDK
3. **NDK Path** : Chemin vers le Android NDK
4. **Ant/Gradle Path** : Système de build à utiliser

### Vérification de l'installation

LAMW effectue une vérification automatique et affiche :
- ✓ Chemins corrects détectés
- ✗ Problèmes de configuration à résoudre

## Création de votre première application LAMW

### Assistant de nouveau projet

**Menu : File → New → Project → GUI Application → Android Module Wizard**

L'assistant vous guide à travers plusieurs étapes :

#### Étape 1 : Informations du projet

- **Project Name** : Nom de votre application (ex: MonAppli)
- **Package Name** : Identifiant unique Java inversé (ex: com.monentreprise.monappli)
- **Activity Name** : Nom de l'activité principale (ex: MainActivity)

#### Étape 2 : Paramètres Android

- **Minimum SDK Version** : Version Android minimale supportée
  - API 21 (Android 5.0) : Bonne couverture du marché
  - API 26 (Android 8.0) : Fonctionnalités modernes

- **Target SDK Version** : Version Android ciblée (dernière disponible recommandée)

#### Étape 3 : Architecture cible

Sélection des architectures processeur :
- **armeabi-v7a** : ARM 32-bit (anciens appareils)
- **arm64-v8a** : ARM 64-bit (appareils récents, recommandé)
- **x86** : Émulateurs Intel 32-bit
- **x86_64** : Émulateurs Intel 64-bit

**Recommandation** : Activer arm64-v8a et armeabi-v7a pour une large compatibilité.

### Structure du projet LAMW

Après création, votre projet contient :

```
MonProjet/
├── jni/
│   ├── unit1.pas          # Code Pascal principal
│   └── controls.pas       # Contrôles supplémentaires
├── libs/
│   └── armeabi-v7a/       # Bibliothèques natives compilées
├── src/
│   └── com/monentreprise/monappli/
│       ├── MainActivity.java    # Activité principale
│       └── Controls.java        # Classes de contrôles
├── res/
│   ├── layout/            # Layouts XML Android
│   ├── drawable/          # Images et icônes
│   └── values/            # Chaînes, couleurs, styles
├── AndroidManifest.xml    # Configuration de l'application
└── build.xml / build.gradle   # Configuration de build
```

## Composants LAMW

### Composants visuels disponibles

LAMW propose une palette de composants Android natifs :

#### Composants de base

- **jTextView** : Affichage de texte simple
- **jEditText** : Champ de saisie de texte
- **jButton** : Bouton standard Android
- **jCheckBox** : Case à cocher
- **jRadioButton** : Bouton radio (choix exclusif)
- **jToggleButton** : Bouton à bascule on/off

#### Composants de liste

- **jListView** : Liste déroulante classique
- **jGridView** : Grille d'éléments
- **jSpinner** : Liste déroulante compacte (combo box)
- **jRecyclerView** : Liste performante moderne

#### Composants de navigation

- **jViewPager** : Pages glissantes horizontales
- **jDrawer** : Menu latéral coulissant
- **jToolbar** : Barre d'outils moderne
- **jTabLayout** : Onglets de navigation

#### Composants d'affichage

- **jImageView** : Affichage d'images
- **jProgressBar** : Barre de progression
- **jWebView** : Navigateur web intégré
- **jCanvas** : Zone de dessin personnalisé

#### Composants conteneur

- **jPanel** : Conteneur générique
- **jScrollView** : Vue défilante
- **jLinearLayout** : Disposition linéaire (vertical/horizontal)
- **jRelativeLayout** : Disposition relative entre éléments

### Composants non-visuels

- **jTimer** : Temporisateur pour actions répétées
- **jSQLite** : Base de données locale SQLite
- **jHttpClient** : Client HTTP pour appels réseau
- **jNotificationManager** : Gestion des notifications
- **jCamera** : Accès à la caméra
- **jMediaPlayer** : Lecture audio/vidéo
- **jSensor** : Accès aux capteurs (accéléromètre, GPS, etc.)

## Programmation avec LAMW

### Structure de base d'un formulaire LAMW

```pascal
unit unit1;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls;

type
  TAndroidModule1 = class(jForm)
    jTextView1: jTextView;
    jButton1: jButton;
    jEditText1: jEditText;

    procedure jButton1Click(Sender: TObject);
  private
    {déclarations privées}
  public
    {déclarations publiques}
  end;

var
  AndroidModule1: TAndroidModule1;

implementation

{$R *.lfm}

procedure TAndroidModule1.jButton1Click(Sender: TObject);
begin
  // Votre code ici
  jTextView1.Text := 'Bouton cliqué !';
end;

end.
```

### Gestion des événements

Les événements Android sont similaires aux événements Lazarus standards :

```pascal
// Événement Click sur un bouton
procedure TAndroidModule1.jButton1Click(Sender: TObject);
begin
  ShowMessage('Bouton cliqué');
end;

// Événement de changement de texte
procedure TAndroidModule1.jEditText1Change(Sender: TObject);
begin
  jTextView1.Text := jEditText1.Text;
end;

// Événement de sélection dans une liste
procedure TAndroidModule1.jListView1ItemClick(Sender: TObject; ItemIndex: Integer);
begin
  ShowMessage('Item ' + IntToStr(ItemIndex) + ' sélectionné');
end;
```

### Propriétés des composants

Chaque composant LAMW possède des propriétés Android spécifiques :

```pascal
// Configuration d'un TextView
jTextView1.Text := 'Mon texte';
jTextView1.TextSize := 18;
jTextView1.TextColor := colbrRed;
jTextView1.TextAlignment := taCenter;

// Configuration d'un EditText
jEditText1.Hint := 'Entrez votre nom';
jEditText1.InputTypeEx := itxText;
jEditText1.MaxLength := 50;

// Configuration d'un Button
jButton1.Text := 'Valider';
jButton1.BackgroundColor := colbrBlue;
jButton1.Enabled := True;
```

## Communication Java-Pascal via JNI

### Appeler du code Java depuis Pascal

LAMW permet d'invoquer des méthodes Java depuis votre code Pascal :

```pascal
// Déclaration d'une méthode Java
function jShowToast(env: PJNIEnv; this: jobject;
                    message: jstring): jboolean; cdecl;
begin
  // Implémentation
  Result := JNI_True;
end;

// Utilisation
procedure TAndroidModule1.ShowToastMessage(const Msg: string);
begin
  // LAMW fournit des helpers pour simplifier les appels JNI
  jForm.ShowMessage(Msg);
end;
```

### Accéder aux API Android

LAMW fournit des wrappers pour les API Android courantes :

```pascal
// Vibration
jForm.Vibrate(500); // Vibre pendant 500ms

// Parole synthétisée
jForm.Speak('Bonjour le monde');

// Notification
jForm.ShowNotification('Titre', 'Message de la notification');

// Partage
jForm.ShareText('Texte à partager', 'Titre du partage');
```

## Gestion des permissions Android

### Déclaration des permissions

Les permissions doivent être déclarées dans le fichier `AndroidManifest.xml` :

```xml
<uses-permission android:name="android.permission.INTERNET" />
<uses-permission android:name="android.permission.CAMERA" />
<uses-permission android:name="android.permission.ACCESS_FINE_LOCATION" />
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />
```

### Permissions à l'exécution (Android 6.0+)

Pour Android 6.0 et supérieur, certaines permissions doivent être demandées à l'exécution :

```pascal
// Vérifier une permission
if jForm.HasPermission('android.permission.CAMERA') then
begin
  // Permission accordée, utiliser la caméra
  OpenCamera;
end
else
begin
  // Demander la permission
  jForm.RequestPermission('android.permission.CAMERA', 100);
end;

// Gérer la réponse de l'utilisateur
procedure TAndroidModule1.OnPermissionResult(
  Permission: string;
  GrantResult: TPermissionResult);
begin
  if GrantResult = prGranted then
    ShowMessage('Permission accordée')
  else
    ShowMessage('Permission refusée');
end;
```

## Compilation et déploiement

### Compilation du projet

**Menu : Run → Build** ou **F9**

Le processus de compilation LAMW :

1. **Compilation Pascal** : Votre code .pas est compilé en bibliothèque native (.so)
2. **Compilation Java** : Les classes Java sont compilées en bytecode
3. **Packaging** : Création du fichier APK
4. **Signature** : Signature de l'APK (debug ou release)

### Types de build

#### Debug Build

- APK signé avec une clé de debug
- Inclut les symboles de débogage
- Taille plus importante
- Ne peut pas être publié sur le Play Store

#### Release Build

- APK optimisé et réduit
- Signé avec votre clé privée
- Obfuscation du code (optionnel)
- Prêt pour publication

### Configuration de la signature

Pour une version release, vous devez créer un keystore :

**Menu : Tools → LAMW → Android Keystore Manager**

Informations nécessaires :
- Alias de la clé
- Mot de passe du keystore
- Mot de passe de la clé
- Informations d'organisation (CN, OU, O, L, ST, C)

### Déploiement sur appareil

#### Via USB (ADB)

1. Activer le mode développeur sur votre appareil Android
2. Activer le débogage USB
3. Connecter l'appareil au PC
4. **Run → Run** dans Lazarus

L'application sera installée et lancée automatiquement.

#### Via émulateur

1. Lancer un émulateur Android (AVD Manager)
2. Sélectionner l'émulateur dans LAMW
3. **Run → Run** dans Lazarus

### Publication sur Google Play Store

Étapes pour publier votre application :

1. **Créer un compte développeur Google Play** (frais unique de 25$)
2. **Compiler en mode Release** avec votre keystore
3. **Préparer les assets** : icônes, captures d'écran, description
4. **Respecter les politiques Google** : vie privée, contenu, sécurité
5. **Uploader l'APK** via la Google Play Console
6. **Configurer la fiche store** : titre, description, catégorie
7. **Soumettre pour révision**

## Débogage des applications LAMW

### LogCat - Logs Android

LAMW s'intègre avec le système de logs Android (LogCat) :

```pascal
// Écrire dans les logs
jForm.Log('TAG', 'Message de debug');

// Différents niveaux de log
jForm.LogVerbose('TAG', 'Détails verbeux');
jForm.LogDebug('TAG', 'Information de débogage');
jForm.LogInfo('TAG', 'Information générale');
jForm.LogWarning('TAG', 'Avertissement');
jForm.LogError('TAG', 'Erreur');
```

Visualiser les logs :
- **Android Studio** : Onglet LogCat
- **Ligne de commande** : `adb logcat`

### Débogueur intégré

Lazarus permet le débogage pas à pas du code Pascal :

- **Points d'arrêt** : F5 sur une ligne
- **Exécution pas à pas** : F7 (entrer), F8 (enjamber)
- **Inspection de variables** : Survol ou fenêtre Debug

**Limitation** : Le débogage ne fonctionne que pour le code Pascal, pas pour la partie Java.

### Techniques de débogage

```pascal
// Afficher des valeurs pour déboguer
procedure TAndroidModule1.DebugCalculation;
var
  Calcul: Integer;
begin
  Calcul := 10 + 20;
  ShowMessage('Résultat: ' + IntToStr(Calcul));
  jForm.Log('DEBUG', 'Résultat = ' + IntToStr(Calcul));
end;

// Gestion des exceptions
procedure TAndroidModule1.SafeOperation;
begin
  try
    // Code potentiellement dangereux
    PerformOperation;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur: ' + E.Message);
      jForm.LogError('ERROR', E.Message);
    end;
  end;
end;
```

## Optimisation des applications LAMW

### Performance

#### Gestion de la mémoire

```pascal
// Libérer les ressources
procedure TAndroidModule1.OnDestroy;
begin
  // Libérer les objets créés
  if Assigned(MyObject) then
    FreeAndNil(MyObject);

  inherited OnDestroy;
end;
```

#### Thread et tâches asynchrones

```pascal
// Exécuter du code en arrière-plan
procedure TAndroidModule1.LoadDataAsync;
begin
  jForm.AsyncTask(
    procedure // OnExecute (thread séparé)
    begin
      // Chargement de données (ne pas toucher l'UI ici)
      Sleep(2000); // Simulation
    end,
    procedure // OnPostExecute (thread UI)
    begin
      // Mise à jour de l'UI
      ShowMessage('Chargement terminé');
    end
  );
end;
```

### Taille de l'APK

Techniques pour réduire la taille :

1. **Compiler pour une seule architecture** en phase de test
2. **Activer ProGuard** pour réduire le code Java
3. **Optimiser les images** : utiliser WebP au lieu de PNG
4. **Supprimer les ressources inutilisées**
5. **Utiliser Android App Bundle** (.aab) pour distribution intelligente

### Consommation énergétique

```pascal
// Économie de batterie
procedure TAndroidModule1.OptimizePower;
begin
  // Désactiver les mises à jour quand non visible
  if not jForm.IsActive then
  begin
    jTimer1.Enabled := False;
  end;

  // Utiliser des intervalles plus longs
  jTimer1.Interval := 5000; // 5 secondes au lieu de 1
end;
```

## Ressources et composants Android

### Gestion des images

```pascal
// Charger une image depuis les ressources
jImageView1.SetImage('drawable', 'mon_image');

// Charger depuis un fichier
jImageView1.LoadFromFile('/sdcard/Pictures/photo.jpg');

// Charger depuis Internet
jImageView1.LoadFromURL('https://example.com/image.jpg');
```

### Fichiers et stockage

```pascal
// Chemin du stockage interne
var
  InternalPath: string;
begin
  InternalPath := jForm.GetEnvironmentDirectoryPath(dirInternalAppStorage);
  // Ex: /data/data/com.monapp/files/
end;

// Chemin du stockage externe
var
  ExternalPath: string;
begin
  ExternalPath := jForm.GetEnvironmentDirectoryPath(dirExternalAppStorage);
  // Ex: /sdcard/Android/data/com.monapp/files/
end;

// Sauvegarder un fichier
procedure TAndroidModule1.SaveTextFile;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('Ligne 1');
    SL.Add('Ligne 2');
    SL.SaveToFile(InternalPath + '/data.txt');
  finally
    SL.Free;
  end;
end;
```

### Sons et multimédia

```pascal
// Jouer un son
jMediaPlayer1.LoadFromAssets('sound.mp3');
jMediaPlayer1.Play;

// Contrôles
jMediaPlayer1.Pause;
jMediaPlayer1.Stop;
jMediaPlayer1.SeekTo(5000); // 5 secondes
```

## Fonctionnalités Android avancées

### Géolocalisation

```pascal
// Activer la localisation
jForm.StartLocationUpdates(1000); // Mise à jour toutes les secondes

// Recevoir les coordonnées
procedure TAndroidModule1.OnLocationChanged(
  Latitude, Longitude: Double; Accuracy: Single);
begin
  jTextView1.Text := Format('Lat: %.6f, Lon: %.6f',
                            [Latitude, Longitude]);
end;
```

### Capteurs

```pascal
// Accéléromètre
jForm.StartSensor(stAccelerometer);

procedure TAndroidModule1.OnSensorChanged(
  SensorType: TSensorType; X, Y, Z: Single);
begin
  if SensorType = stAccelerometer then
  begin
    // Traiter les données de l'accéléromètre
    jTextView1.Text := Format('X: %.2f, Y: %.2f, Z: %.2f', [X, Y, Z]);
  end;
end;
```

### Caméra

```pascal
// Prendre une photo
jForm.TakePicture('photo.jpg');

// Recevoir le résultat
procedure TAndroidModule1.OnActivityResult(
  RequestCode, ResultCode: Integer; Intent: jObject);
begin
  if RequestCode = 100 then // Code de la caméra
  begin
    if ResultCode = RESULT_OK then
    begin
      // Photo prise avec succès
      jImageView1.LoadFromFile(PhotoPath);
    end;
  end;
end;
```

## Gestion du cycle de vie Android

### Événements du cycle de vie

Une application Android passe par plusieurs états :

```pascal
// Application créée
procedure TAndroidModule1.OnCreate;
begin
  // Initialisation
  InitComponents;
end;

// Application visible
procedure TAndroidModule1.OnStart;
begin
  // Démarrer les mises à jour
  jTimer1.Enabled := True;
end;

// Application au premier plan
procedure TAndroidModule1.OnResume;
begin
  // Reprendre les opérations
  ResumeOperations;
end;

// Application en arrière-plan
procedure TAndroidModule1.OnPause;
begin
  // Suspendre les opérations coûteuses
  PauseOperations;
end;

// Application plus visible
procedure TAndroidModule1.OnStop;
begin
  // Arrêter les mises à jour
  jTimer1.Enabled := False;
end;

// Application détruite
procedure TAndroidModule1.OnDestroy;
begin
  // Libérer les ressources
  CleanupResources;
end;
```

### Gestion de la configuration

Lorsque l'appareil tourne (changement d'orientation), Android détruit et recrée l'activité :

```pascal
// Sauvegarder l'état
procedure TAndroidModule1.OnSaveInstanceState(outState: jObject);
begin
  // Sauvegarder les données importantes
  SaveStateString(outState, 'username', jEditText1.Text);
  SaveStateInt(outState, 'score', FCurrentScore);
end;

// Restaurer l'état
procedure TAndroidModule1.OnRestoreInstanceState(savedState: jObject);
begin
  // Restaurer les données
  jEditText1.Text := RestoreStateString(savedState, 'username', '');
  FCurrentScore := RestoreStateInt(savedState, 'score', 0);
end;
```

## Bonnes pratiques LAMW

### Architecture du code

1. **Séparer la logique métier de l'UI** : Créer des unités Pascal distinctes
2. **Utiliser des classes** : Organiser le code en classes réutilisables
3. **Gestion des erreurs** : Toujours utiliser try-except
4. **Commentaires** : Documenter le code complexe

### Interface utilisateur

1. **Simplicité** : Interfaces épurées et intuitives
2. **Feedback visuel** : Indiquer les actions en cours
3. **Responsive** : Tester sur différentes tailles d'écran
4. **Thème cohérent** : Suivre les Material Design Guidelines

### Performance

1. **Éviter les opérations lourdes sur le thread UI**
2. **Utiliser des images optimisées**
3. **Libérer les ressources** : Fermer fichiers, connexions, etc.
4. **Tester sur vrais appareils** : Les émulateurs ne reflètent pas toujours la réalité

### Sécurité

1. **Ne jamais stocker de mots de passe en clair**
2. **Valider les entrées utilisateur**
3. **Utiliser HTTPS** pour les communications réseau
4. **Respecter les permissions** : Ne demander que le nécessaire

## Limitations et considérations

### Limitations actuelles de LAMW

- **Pas de support iOS natif** : LAMW est spécifique à Android
- **Courbe d'apprentissage** : Nécessite de comprendre à la fois Pascal et Android
- **Documentation limitée** : Moins de ressources que pour les outils mainstream
- **Communauté plus petite** : Moins de packages et d'exemples

### Quand utiliser LAMW ?

LAMW est idéal pour :
- Développeurs Pascal souhaitant créer des apps Android
- Portage d'applications Lazarus existantes vers mobile
- Projets nécessitant des performances natives
- Applications avec logique métier complexe en Pascal

LAMW est moins adapté pour :
- Applications nécessitant l'écosystème iOS
- Projets purement UI avec peu de logique
- Équipes déjà formées sur React Native / Flutter

## Ressources et apprentissage

### Documentation officielle

- **Forum LAMW** : https://github.com/jmpessoa/lazandroidmodulewizard
- **Wiki Lazarus** : Section Android Development
- **YouTube** : Tutoriels vidéo LAMW

### Communauté

- **Forum Lazarus** : Section Mobile Development
- **Telegram / Discord** : Groupes de discussion LAMW
- **GitHub** : Exemples de projets open source

### Projets d'exemple

LAMW inclut de nombreux exemples dans le dossier `demos/` :
- Applications CRUD simples
- Utilisation des capteurs
- Intégration multimédia
- Communication réseau
- Bases de données SQLite

## Conclusion

LAMW ouvre la porte du développement Android aux développeurs FreePascal et Lazarus. Bien qu'il nécessite un apprentissage initial de l'écosystème Android, il permet de créer des applications natives performantes tout en réutilisant vos compétences Pascal existantes.

Les points clés à retenir :
- Installation nécessite SDK/NDK/JDK correctement configurés
- Architecture hybride Pascal + Java via JNI
- Large palette de composants Android natifs
- Gestion du cycle de vie Android essentielle
- Performance comparable aux applications natives Java/Kotlin

Avec de la pratique et l'exploration des exemples fournis, vous serez capable de créer des applications Android complètes et professionnelles avec LAMW.

⏭️ [Architecture Android et JNI](/13-developpement-mobile-embarque/02-architecture-android-jni.md)
