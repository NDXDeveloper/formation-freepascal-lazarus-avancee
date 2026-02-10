🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.10 Distribution sur stores

## Introduction

La distribution de votre application FreePascal sur les stores mobiles (Google Play Store, Apple App Store) est l'étape finale qui permet à vos utilisateurs d'installer facilement votre application. Ce chapitre couvre les processus, exigences et bonnes pratiques pour publier avec succès vos applications.

Bien que FreePascal ne soit pas le langage le plus courant pour le développement mobile, les applications compilées sont identiques aux applications natives et peuvent être distribuées exactement de la même manière.

## Vue d'ensemble des stores

### Comparaison des principaux stores

| Aspect | Google Play Store | Apple App Store | Alternatives |
|--------|-------------------|-----------------|--------------|
| **Plateforme** | Android | iOS | F-Droid, Amazon Appstore |
| **Frais inscription** | 25 $ (une fois) | 99 $/an | Gratuit (F-Droid) |
| **Processus validation** | Automatique + manuel | Manuel strict | Variable |
| **Délai approbation** | Quelques heures | 1-7 jours | Variable |
| **Commission** | 15-30% | 15-30% | 0% (F-Droid) |
| **Portée** | 2.5 milliards d'appareils | 1.5 milliard d'appareils | Limitée |
| **Mise à jour** | Rapide | Plus lente | Variable |

## Distribution sur Google Play Store

### Prérequis

**Compte développeur** :
- Créer un compte sur https://play.google.com/console
- Payer les frais d'inscription : 25 $ (paiement unique)
- Accepter l'accord de distribution

**Outils nécessaires** :
- Android SDK (pour la signature APK)
- Java JDK (pour les outils de build)
- Clé de signature (keystore)

**Application prête** :
- APK ou AAB (Android App Bundle) signé
- Icônes et captures d'écran
- Description et métadonnées
- Politique de confidentialité

### Préparation de l'application

#### 1. Configuration du manifest

Le fichier `AndroidManifest.xml` doit être correctement configuré :

```xml
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="com.votrecompagnie.votreapp"
    android:versionCode="1"
    android:versionName="1.0.0">

    <!-- Version minimale et cible d'Android -->
    <uses-sdk
        android:minSdkVersion="21"
        android:targetSdkVersion="33" />

    <!-- Permissions nécessaires -->
    <uses-permission android:name="android.permission.INTERNET" />
    <uses-permission android:name="android.permission.ACCESS_NETWORK_STATE" />

    <!-- Déclaration de l'application -->
    <application
        android:label="@string/app_name"
        android:icon="@mipmap/ic_launcher"
        android:roundIcon="@mipmap/ic_launcher_round"
        android:allowBackup="true"
        android:theme="@style/AppTheme">

        <!-- Activité principale -->
        <activity
            android:name=".MainActivity"
            android:label="@string/app_name"
            android:exported="true">
            <intent-filter>
                <action android:name="android.intent.action.MAIN" />
                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
        </activity>
    </application>
</manifest>
```

**Points importants** :
- `package` : Identifiant unique de votre application (ex: com.mycompany.myapp)
- `versionCode` : Numéro entier incrémenté à chaque version (1, 2, 3...)
- `versionName` : Nom de version lisible (1.0.0, 1.0.1...)
- `targetSdkVersion` : Version Android ciblée (obligatoire d'être récent)

#### 2. Génération de la clé de signature

La clé de signature identifie l'auteur de l'application de manière unique :

```bash
# Générer une nouvelle clé (à faire une seule fois)
keytool -genkey -v -keystore mon-app-key.keystore \
  -alias mon-app \
  -keyalg RSA \
  -keysize 2048 \
  -validity 10000

# Informations demandées :
# - Mot de passe du keystore (IMPORTANT : ne pas perdre!)
# - Nom, organisation, ville, pays
# - Mot de passe de la clé (peut être identique au keystore)
```

⚠️ **CRITIQUE** : Conservez cette clé en sécurité ! Sans elle, vous ne pourrez jamais mettre à jour votre application. Faites des sauvegardes !

**Stockage sécurisé** :
```
mon-app-key.keystore         # Le fichier de clé  
keystore-info.txt            # Mots de passe et informations
```

Sauvegardez ces fichiers dans :
- Un gestionnaire de mots de passe sécurisé
- Un stockage cloud chiffré
- Un coffre-fort physique (pour les entreprises)

#### 3. Signature de l'APK

```bash
# Aligner l'APK (optimisation)
zipalign -v -p 4 app-unsigned.apk app-unsigned-aligned.apk

# Signer l'APK
apksigner sign --ks mon-app-key.keystore \
  --ks-key-alias mon-app \
  --out app-signed.apk \
  app-unsigned-aligned.apk

# Vérifier la signature
apksigner verify app-signed.apk
```

**Ou avec jarsigner (ancien)** :
```bash
jarsigner -verbose -sigalg SHA256withRSA -digestalg SHA-256 \
  -keystore mon-app-key.keystore \
  app-unsigned.apk mon-app
```

#### 4. Création d'un Android App Bundle (AAB)

Google recommande maintenant le format AAB plutôt qu'APK :

**Avantages de l'AAB** :
- Taille de téléchargement réduite (Google génère des APK optimisés par appareil)
- Meilleure optimisation des ressources
- Obligatoire pour les nouvelles applications depuis août 2021

**Génération AAB avec FreePascal** :

```bash
# 1. Compiler le projet FreePascal pour Android
lazbuild --build-mode=Release MonProjet.lpi

# 2. Créer la structure AAB
mkdir -p build/aab/base/manifest  
mkdir -p build/aab/base/dex  
mkdir -p build/aab/base/lib/armeabi-v7a  
mkdir -p build/aab/base/lib/arm64-v8a

# 3. Copier les fichiers compilés
cp android/AndroidManifest.xml build/aab/base/manifest/  
cp android/classes.dex build/aab/base/dex/  
cp lib/armeabi-v7a/libmonprojet.so build/aab/base/lib/armeabi-v7a/  
cp lib/arm64-v8a/libmonprojet.so build/aab/base/lib/arm64-v8a/

# 4. Créer le fichier AAB (archive ZIP)
cd build/aab  
zip -r ../../MonProjet.aab *

# 5. Signer l'AAB
cd ../..  
jarsigner -keystore mon-app-key.keystore MonProjet.aab mon-app
```

### Publication sur Google Play Console

#### 1. Créer l'application

1. Se connecter à https://play.google.com/console
2. Cliquer sur **"Créer une application"**
3. Remplir les informations de base :
   - Nom de l'application
   - Langue par défaut
   - Type d'application (Gratuite/Payante)
   - Catégorie

#### 2. Fiche du Store

**Textes requis** :

```
Titre de l'application (max 50 caractères)
  Exemple: MonApp - Gestion de Tâches

Description courte (max 80 caractères)
  Exemple: Gérez vos tâches efficacement avec notre application intuitive

Description complète (max 4000 caractères)
  Exemple:
  MonApp est l'application parfaite pour organiser votre quotidien.

  Fonctionnalités principales :
  • Création rapide de tâches
  • Notifications personnalisées
  • Synchronisation cloud
  • Interface intuitive

  [...]
```

**Assets graphiques requis** :

| Type | Dimensions | Format | Nombre |
|------|-----------|--------|--------|
| Icône de l'application | 512x512 px | PNG 32-bit | 1 |
| Bannière de fonction | 1024x500 px | PNG/JPEG | 1 |
| Capture d'écran téléphone | 320-3840 px | PNG/JPEG | 2-8 |
| Capture d'écran tablette 7" | 320-3840 px | PNG/JPEG | Optionnel |
| Capture d'écran tablette 10" | 320-3840 px | PNG/JPEG | Optionnel |
| Vidéo YouTube | - | Lien | Optionnel |

**Création des icônes** :

```bash
# Générer différentes tailles d'icônes
# À partir d'une icône 1024x1024 px

convert icon-1024.png -resize 512x512 icon-512.png  
convert icon-1024.png -resize 192x192 mipmap-xxxhdpi/ic_launcher.png  
convert icon-1024.png -resize 144x144 mipmap-xxhdpi/ic_launcher.png  
convert icon-1024.png -resize 96x96 mipmap-xhdpi/ic_launcher.png  
convert icon-1024.png -resize 72x72 mipmap-hdpi/ic_launcher.png  
convert icon-1024.png -resize 48x48 mipmap-mdpi/ic_launcher.png
```

#### 3. Classification du contenu

Répondre au questionnaire Google sur :
- Public cible (enfants, adolescents, adultes)
- Contenu sensible (violence, langage, etc.)
- Publicités
- Achats intégrés

**Exemple pour une application simple** :
```
Public cible : Tout public (3+)  
Violence : Aucune  
Contenu sexuel : Aucun  
Langage inapproprié : Aucun  
Publicités : Non  
Achats intégrés : Non
```

#### 4. Politique de confidentialité

**OBLIGATOIRE** si votre application :
- Collecte des données personnelles
- Utilise des permissions sensibles
- Contient des publicités

**Structure minimale** :

```markdown
# Politique de Confidentialité de MonApp

Dernière mise à jour : [Date]

## Collecte de données
MonApp ne collecte aucune donnée personnelle.
[OU]
MonApp collecte les données suivantes :
- Adresse email (pour la création de compte)
- Localisation approximative (pour les fonctionnalités de...)

## Utilisation des données
Les données collectées sont utilisées uniquement pour :
- [But 1]
- [But 2]

## Partage des données
Nous ne partageons vos données avec aucun tiers.
[OU]
Nous partageons vos données avec :
- [Service 1] pour [raison]

## Sécurité
Nous utilisons des mesures de sécurité standard de l'industrie...

## Vos droits
Vous avez le droit de :
- Accéder à vos données
- Demander la suppression de vos données
- [...]

## Contact
Pour toute question : contact@monapp.com
```

Héberger cette politique sur :
- Votre site web (monapp.com/privacy)
- GitHub Pages (gratuit)
- Google Sites (gratuit)

#### 5. Test en interne et bêta

**Test interne** :
- Maximum 100 testeurs
- Publication instantanée (pas de validation)
- Idéal pour tests rapides

**Test fermé (bêta fermée)** :
- Maximum 10 000 testeurs
- Validation complète de Google
- Utiliser pour tests avant production

**Test ouvert (bêta ouverte)** :
- Illimité
- Accessible à tous via lien
- Bon pour feedback utilisateur

**Créer une piste de test** :

1. Aller dans **"Tests" → "Test interne"**
2. Créer une nouvelle version
3. Uploader l'AAB/APK
4. Ajouter des testeurs par email
5. Les testeurs reçoivent un lien pour installer

#### 6. Publication en production

1. **Aller dans "Production"**
2. **Créer une nouvelle version**
3. **Uploader l'AAB/APK signé**
4. **Remplir les notes de version** :

```
Version 1.0.0
• Première version de MonApp
• Création de tâches
• Notifications
• Synchronisation
```

5. **Sélectionner les pays** (ou mondial)
6. **Examiner et publier**

**Délai de traitement** :
- Première soumission : 2-7 jours
- Mises à jour : Quelques heures à 2 jours

### Mises à jour

Pour publier une mise à jour :

```bash
# 1. Incrémenter versionCode et versionName dans AndroidManifest.xml
#    versionCode: 1 → 2
#    versionName: "1.0.0" → "1.0.1"

# 2. Compiler la nouvelle version

# 3. Signer avec la MÊME clé

# 4. Publier sur Google Play Console
```

## Distribution sur Apple App Store

### Prérequis

**Compte développeur Apple** :
- S'inscrire sur https://developer.apple.com
- Coût : 99 $/an (obligatoire)
- Vérification d'identité requise

**Matériel nécessaire** :
- **Mac** (obligatoire pour Xcode et publication)
- Xcode installé
- Certificate et Provisioning Profile

**Application prête** :
- Archive IPA signée
- Icônes (App Icon Sets)
- Captures d'écran pour tous les appareils
- Description et métadonnées

### Configuration des certificats

#### 1. Créer un App ID

1. Aller sur https://developer.apple.com/account
2. **Certificates, IDs & Profiles** → **Identifiers**
3. Créer un **App ID** :
   - Description : "MonApp"
   - Bundle ID : com.votrecompagnie.monapp (doit être unique)
   - Capabilities : Cocher les fonctionnalités nécessaires

#### 2. Créer un certificat de distribution

```bash
# 1. Générer une demande de certificat (CSR)
# Dans "Trousseau d'accès" sur Mac :
# Menu → Trousseau d'accès → Assistant de certificat →
# Demander un certificat à une autorité de certification

# 2. Sur developer.apple.com :
# Certificates → + → iOS Distribution
# Uploader le fichier CSR
# Télécharger le certificat (.cer)

# 3. Double-cliquer sur le .cer pour l'installer dans le trousseau
```

#### 3. Créer un Provisioning Profile

1. **Profiles** → **+** → **App Store Distribution**
2. Sélectionner votre App ID
3. Sélectionner votre certificat
4. Télécharger le Provisioning Profile (.mobileprovision)
5. Double-cliquer pour l'installer

### Préparation de l'application

#### 1. Configuration Info.plist

Le fichier `Info.plist` contient les métadonnées de l'application :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleIdentifier</key>
    <string>com.votrecompagnie.monapp</string>

    <key>CFBundleName</key>
    <string>MonApp</string>

    <key>CFBundleDisplayName</key>
    <string>MonApp</string>

    <key>CFBundleVersion</key>
    <string>1</string>

    <key>CFBundleShortVersionString</key>
    <string>1.0.0</string>

    <key>UIRequiredDeviceCapabilities</key>
    <array>
        <string>arm64</string>
    </array>

    <key>UISupportedInterfaceOrientations</key>
    <array>
        <string>UIInterfaceOrientationPortrait</string>
        <string>UIInterfaceOrientationLandscapeLeft</string>
        <string>UIInterfaceOrientationLandscapeRight</string>
    </array>

    <key>NSAppTransportSecurity</key>
    <dict>
        <key>NSAllowsArbitraryLoads</key>
        <false/>
    </dict>
</dict>
</plist>
```

#### 2. Icônes de l'application

Apple exige un **App Icon Set** complet :

| Appareil | Tailles requises |
|----------|------------------|
| iPhone | 20x20, 29x29, 40x40, 60x60, 76x76, 83.5x83.5 (et @2x, @3x) |
| iPad | 20x20, 29x29, 40x40, 76x76, 83.5x83.5 (et @2x) |
| App Store | 1024x1024 |

**Structure des fichiers** :
```
Assets.xcassets/
└── AppIcon.appiconset/
    ├── Contents.json
    ├── Icon-20@2x.png (40x40)
    ├── Icon-20@3x.png (60x60)
    ├── Icon-29@2x.png (58x58)
    ├── Icon-29@3x.png (87x87)
    ├── Icon-40@2x.png (80x80)
    ├── Icon-40@3x.png (120x120)
    ├── Icon-60@2x.png (120x120)
    ├── Icon-60@3x.png (180x180)
    ├── Icon-76@2x.png (152x152)
    └── Icon-1024.png (1024x1024)
```

#### 3. Compilation et archivage

Depuis Xcode ou en ligne de commande :

```bash
# Compiler pour appareil iOS
xcodebuild -workspace MonApp.xcworkspace \
  -scheme MonApp \
  -sdk iphoneos \
  -configuration Release \
  archive -archivePath MonApp.xcarchive

# Exporter l'archive en IPA
xcodebuild -exportArchive \
  -archivePath MonApp.xcarchive \
  -exportOptionsPlist ExportOptions.plist \
  -exportPath ./build
```

**ExportOptions.plist** :
```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>method</key>
    <string>app-store</string>

    <key>teamID</key>
    <string>VOTRE_TEAM_ID</string>

    <key>uploadSymbols</key>
    <true/>

    <key>compileBitcode</key>
    <true/>
</dict>
</plist>
```

### Publication sur App Store Connect

#### 1. Créer l'application

1. Se connecter à https://appstoreconnect.apple.com
2. **Mes Apps** → **+** → **Nouvelle app**
3. Remplir :
   - Nom
   - Langue principale
   - Bundle ID
   - SKU (identifiant interne unique)

#### 2. Informations de l'app

**Textes requis** :

```
Nom (max 30 caractères)
  MonApp

Sous-titre (max 30 caractères)
  Gestion de tâches simple

Description (max 4000 caractères)
  MonApp vous permet de gérer vos tâches quotidiennes...

  Fonctionnalités :
  • [...]

Mots-clés (max 100 caractères, séparés par virgules)
  tâches,productivité,organisation,todo,rappels

URL de support
  https://monapp.com/support

URL marketing (optionnel)
  https://monapp.com
```

#### 3. Captures d'écran

**Tailles requises** (pour chaque appareil) :

| Appareil | Résolution |
|----------|-----------|
| iPhone 6.7" (14 Pro Max, 15 Pro Max) | 1290 x 2796 px |
| iPhone 6.5" (11 Pro Max, XS Max) | 1242 x 2688 px |
| iPhone 5.5" (8 Plus, 7 Plus, 6s Plus) | 1242 x 2208 px |
| iPad Pro 12.9" (6e gen) | 2048 x 2732 px |
| iPad Pro 12.9" (2e gen) | 2048 x 2732 px |

Minimum **3 captures**, maximum **10** par appareil.

**Astuce** : Vous pouvez fournir seulement les captures pour les plus grands écrans, Apple les adaptera automatiquement.

#### 4. Classification du contenu

Répondre au questionnaire Apple :

- Contenu  généré par l'utilisateur
- Localisation
- Achats intégrés
- Publicités tierces
- Classification d'âge

#### 5. Informations de version

Pour chaque nouvelle version :

```
Nouveautés de cette version (max 4000 caractères)
  Version 1.0.0
  • Première version
  • Création de tâches
  • Notifications push
  • Synchronisation iCloud
```

#### 6. Uploader le build

**Méthode 1 : Xcode** :
1. Ouvrir l'Organizer (Window → Organizer)
2. Sélectionner l'archive
3. **Distribute App** → **App Store Connect**
4. Suivre l'assistant

**Méthode 2 : Transporter** :
1. Télécharger "Transporter" depuis le Mac App Store
2. Glisser-déposer le fichier IPA
3. **Deliver**

**Méthode 3 : Ligne de commande** :
```bash
xcrun altool --upload-app \
  --type ios \
  --file MonApp.ipa \
  --username votre.email@example.com \
  --password "mot-de-passe-specifique-app"
```

#### 7. Soumettre pour validation

1. Retourner sur App Store Connect
2. Sélectionner le build uploadé
3. **Soumettre pour validation**
4. Répondre aux questions de conformité

**Délai de validation** :
- Première soumission : 2-7 jours (souvent plus long)
- Mises à jour : 1-3 jours

**Raisons courantes de rejet** :
- Fonctionnalité incomplète ou bugs
- Interface non conforme aux guidelines
- Politique de confidentialité manquante
- Permissions non justifiées
- Contenu inapproprié
- Métadonnées trompeuses

### Mises à jour

```bash
# 1. Incrémenter CFBundleVersion et CFBundleShortVersionString
#    CFBundleVersion: "1" → "2"
#    CFBundleShortVersionString: "1.0.0" → "1.0.1"

# 2. Recompiler et archiver

# 3. Uploader le nouveau build

# 4. Créer une nouvelle version dans App Store Connect
```

## Stores alternatifs

### F-Droid (Android - Open Source)

**Avantages** :
- ✅ Gratuit (pas de frais)
- ✅ 100% open source
- ✅ Pas de compte Google requis
- ✅ Respect de la vie privée
- ✅ Aucune commission

**Inconvénients** :
- ❌ Audience limitée
- ❌ Application doit être open source
- ❌ Build reproductible requis

**Publication sur F-Droid** :

1. Rendre votre code open source (GitHub, GitLab)
2. Créer un fichier `metadata/com.votre.app.yml` :

```yaml
Categories:
  - Productivity

License: GPL-3.0-only

AuthorName: Votre Nom  
AuthorEmail: email@example.com

Summary: Description courte

Description: |-
  Description complète de votre application.

  Fonctionnalités :
  * Feature 1
  * Feature 2

RepoType: git  
Repo: https://github.com/votre-username/votre-app

Builds:
  - versionName: '1.0.0'
    versionCode: 1
    commit: v1.0.0
    subdir: app
    gradle:
      - yes
```

3. Soumettre une merge request sur https://gitlab.com/fdroid/fdroiddata

### Amazon Appstore (Android)

**Avantages** :
- Pré-installé sur tablettes Amazon Fire
- Moins de concurrence
- Process similaire à Google Play

**Publication** :

1. Créer un compte sur https://developer.amazon.com
2. Uploader l'APK (même qu'utilisé pour Google Play)
3. Remplir les métadonnées
4. Validation généralement rapide

### Samsung Galaxy Store

Cibler spécifiquement les utilisateurs Samsung.

### Huawei AppGallery

Important pour les marchés où Google Play n'est pas disponible (Chine).

## Aspects légaux et conformité

### RGPD (Europe)

Si vous ciblez l'Europe, vous devez :

✅ Avoir une politique de confidentialité claire  
✅ Obtenir le consentement pour les cookies/tracking  
✅ Permettre l'accès et la suppression des données  
✅ Notifier les violations de données  
✅ Désigner un DPO si nécessaire

### COPPA (États-Unis)

Pour les applications destinées aux enfants (<13 ans) :

✅ Ne pas collecter de données personnelles sans consentement parental  
✅ Politique de confidentialité adaptée  
✅ Pas de publicité comportementale

### Licences open source

Si vous utilisez des bibliothèques open source dans votre app FreePascal :

✅ Respecter les termes de la licence (GPL, MIT, Apache, etc.)  
✅ Inclure les mentions légales dans l'app  
✅ Fournir le code source si requis (GPL)

## Monétisation

### Modèles de prix

| Modèle | Description | Avantages | Inconvénients |
|--------|-------------|-----------|---------------|
| **Gratuit** | App totalement gratuite | Maximum de téléchargements | Pas de revenus directs |
| **Payant** | Prix fixe à l'achat | Revenus immédiats | Moins de téléchargements |
| **Freemium** | Gratuit + achats intégrés | Large audience + revenus | Complexe à gérer |
| **Abonnement** | Paiement récurrent | Revenus réguliers | Désabonnements |
| **Publicités** | Revenus publicitaires | Gratuit pour l'utilisateur | Expérience dégradée |

### Achats intégrés (IAP)

**Implémentation avec FreePascal** :

```pascal
unit InAppPurchase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TIAPManager = class
  private
    {$IFDEF ANDROID}
    // Google Play Billing
    {$ENDIF}
    {$IFDEF IOS}
    // StoreKit
    {$ENDIF}
  public
    procedure Initialize;
    function Purchase(const ProductID: string): Boolean;
    function RestorePurchases: Boolean;
    function IsPurchased(const ProductID: string): Boolean;
  end;

implementation

{$IFDEF ANDROID}
// Implémentation Android avec JNI
// Utiliser Google Play Billing Library
{$ENDIF}

{$IFDEF IOS}
// Implémentation iOS avec StoreKit
{$ENDIF}

end.
```

### Configuration Google Play Billing

1. **Play Console** → **Monétisation** → **Produits**
2. Créer des produits :
   - **Consommables** : Peuvent être achetés plusieurs fois (jetons, vies)
   - **Non consommables** : Achat unique permanent (suppression pub)
   - **Abonnements** : Paiements récurrents

### Configuration App Store IAP

1. **App Store Connect** → **Fonctionnalités** → **Achats intégrés**
2. Types de produits :
   - **Consommable**
   - **Non consommable**
   - **Abonnement auto-renouvelable**
   - **Abonnement non renouvelable**

## Analytics et suivi

### Intégration Google Analytics (Android)

```pascal
unit Analytics;

{$mode objfpc}{$H+}

interface

procedure TrackScreen(const ScreenName: string);  
procedure TrackEvent(const Category, Action, Label: string; Value: Integer = 0);

implementation

{$IFDEF ANDROID}
uses
  jni;

procedure TrackScreen(const ScreenName: string);  
begin
  // Appel JNI vers Firebase Analytics
  // FirebaseAnalytics.logEvent("screen_view", params)
end;

procedure TrackEvent(const Category, Action, Label: string; Value: Integer);  
begin
  // Appel JNI vers Firebase Analytics
  // FirebaseAnalytics.logEvent(action, bundle)
end;

{$ENDIF}

{$IFDEF IOS}

procedure TrackScreen(const ScreenName: string);  
begin
  // Appel vers Firebase Analytics iOS
end;

procedure TrackEvent(const Category, Action, Label: string; Value: Integer);  
begin
  // Appel vers Firebase Analytics iOS
end;

{$ENDIF}

end.
```

**Utilisation** :

```pascal
program MyApp;

uses
  Analytics;

begin
  // Suivre l'ouverture d'un écran
  TrackScreen('MainScreen');

  // Suivre une action utilisateur
  TrackEvent('UserAction', 'ButtonClick', 'SubmitButton', 1);

  // Suivre un achat
  TrackEvent('Commerce', 'Purchase', 'PremiumUpgrade', 999);
end.
```

### Métriques importantes à suivre

**Métriques d'acquisition** :
- Nombre de téléchargements
- Sources de téléchargement (recherche, lien direct, etc.)
- Taux de conversion (vues de page → installations)

**Métriques d'engagement** :
- Utilisateurs actifs quotidiens (DAU)
- Utilisateurs actifs mensuels (MAU)
- Durée moyenne de session
- Nombre de sessions par utilisateur
- Taux de rétention (jour 1, 7, 30)

**Métriques techniques** :
- Taux de crash
- Temps de chargement
- Erreurs réseau
- Versions Android/iOS utilisées

**Métriques de monétisation** :
- Revenus par utilisateur (ARPU)
- Taux de conversion IAP
- Revenu moyen par utilisateur payant (ARPPU)

## Gestion des versions et releases

### Stratégie de versioning

**Semantic Versioning (recommandé)** :

```
MAJEUR.MINEUR.CORRECTIF

Exemple: 1.2.3
  1 = Version majeure (changements incompatibles)
  2 = Version mineure (nouvelles fonctionnalités compatibles)
  3 = Correctif (corrections de bugs)
```

**Exemples** :
- `1.0.0` - Première version stable
- `1.1.0` - Ajout de fonctionnalités
- `1.1.1` - Correction de bugs
- `2.0.0` - Changements majeurs (refonte UI, etc.)

### Release notes efficaces

**Mauvais exemple** ❌ :
```
Version 1.2.0
- Corrections de bugs
- Améliorations de performance
```

**Bon exemple** ✅ :
```
Version 1.2.0

Nouveautés :
• Mode sombre ajouté
• Synchronisation automatique avec le cloud
• Widget pour l'écran d'accueil

Améliorations :
• Temps de démarrage réduit de 40%
• Interface de recherche plus rapide
• Meilleure gestion de la batterie

Corrections :
• Correction du crash lors de l'ouverture des paramètres
• Résolution du problème de notifications manquantes
• Correction de l'affichage sur tablettes

Merci pour vos retours ! Continuez à nous envoyer vos suggestions.
```

### Gestion des releases progressives

**Phase 1 : Test interne (10-100 testeurs)**
```
Durée : 1-3 jours  
Objectif : Vérifier qu'il n'y a pas de bugs critiques
```

**Phase 2 : Bêta fermée (100-1000 utilisateurs)**
```
Durée : 3-7 jours  
Objectif : Tester sur différents appareils, recueillir feedback
```

**Phase 3 : Déploiement progressif**
```
Jour 1-2 : 10% des utilisateurs  
Jour 3-4 : 25% des utilisateurs  
Jour 5-6 : 50% des utilisateurs  
Jour 7+  : 100% des utilisateurs
```

**Avantages** :
- Détecter les problèmes avant impact massif
- Possibilité de rollback rapide
- Réduire le risque

### Hotfix en urgence

Si un bug critique est découvert :

```bash
# 1. Créer une branche de hotfix
git checkout -b hotfix/1.2.1 main

# 2. Corriger le bug rapidement
# ... modifications ...

# 3. Incrémenter uniquement le CORRECTIF
# 1.2.0 → 1.2.1

# 4. Compiler, tester, signer

# 5. Publier en urgence sur les stores
# Google Play : Quelques heures
# App Store : Demander une validation accélérée

# 6. Merger dans main et develop
git checkout main  
git merge hotfix/1.2.1  
git checkout develop  
git merge hotfix/1.2.1
```

## Support utilisateur

### Gestion des retours

**Canaux de support** :
- Email de support (support@monapp.com)
- Section FAQ/Aide dans l'app
- Forum communautaire
- Réseaux sociaux (Twitter, Facebook)
- Discord/Telegram pour utilisateurs actifs

**Temps de réponse recommandés** :
- Bugs critiques : < 24h
- Bugs mineurs : < 3 jours
- Questions générales : < 5 jours
- Suggestions : Acknowledgement rapide, implémentation variable

### Répondre aux avis sur les stores

**Google Play** :

```pascal
// Les avis sont accessibles via Google Play Console
// Répondre aux avis négatifs est particulièrement important
```

**Exemple de réponse à un avis négatif** :

```
Utilisateur: "App plante au démarrage, impossible de l'utiliser! ⭐"

Réponse développeur:
"Bonjour, merci pour votre retour. Nous sommes désolés pour ce problème.
Pourriez-vous nous envoyer un email à support@monapp.com avec :
- Modèle de votre appareil
- Version d'Android
- Une capture d'écran si possible

Nous allons résoudre ce problème rapidement.  
Mise à jour : Version 1.2.1 publiée aujourd'hui avec correction.

L'équipe MonApp"
```

**Bonnes pratiques** :
- ✅ Répondre rapidement (< 48h)
- ✅ Être poli et professionnel
- ✅ Proposer une solution concrète
- ✅ Demander plus d'informations si nécessaire
- ✅ Informer quand le problème est résolu
- ❌ Ne pas être défensif
- ❌ Ne pas blâmer l'utilisateur
- ❌ Ne pas ignorer les critiques

### Système de feedback dans l'app

```pascal
unit FeedbackSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFeedbackType = (ftBug, ftFeature, ftQuestion, ftOther);

  TFeedbackManager = class
  public
    procedure SendFeedback(FeedbackType: TFeedbackType;
      const Subject, Message: string;
      IncludeDeviceInfo: Boolean = True);
  end;

implementation

uses
  {$IFDEF ANDROID}
  Android.JNI,
  {$ENDIF}
  {$IFDEF IOS}
  iOS.API,
  {$ENDIF}
  HTTPClient;

procedure TFeedbackManager.SendFeedback(FeedbackType: TFeedbackType;
  const Subject, Message: string; IncludeDeviceInfo: Boolean);
var
  http: THTTPClient;
  data: string;
  deviceInfo: string;
begin
  // Collecter les informations de l'appareil
  if IncludeDeviceInfo then
  begin
    deviceInfo := Format(
      'Device: %s' + sLineBreak +
      'OS Version: %s' + sLineBreak +
      'App Version: %s',
      [GetDeviceModel, GetOSVersion, GetAppVersion]
    );
  end;

  // Préparer les données
  data := Format(
    '{"type": "%s", "subject": "%s", "message": "%s", "device_info": "%s"}',
    [FeedbackTypeToString(FeedbackType), Subject, Message, deviceInfo]
  );

  // Envoyer au serveur
  http := THTTPClient.Create;
  try
    http.Post('https://api.monapp.com/feedback', data);
  finally
    http.Free;
  end;
end;

end.
```

## Marketing et visibilité

### Optimisation ASO (App Store Optimization)

**ASO** est l'équivalent du SEO pour les stores d'applications.

**Éléments clés** :

1. **Titre de l'app** (70% du poids SEO)
   - Inclure 1-2 mots-clés principaux
   - Rester clair et descriptif
   - Exemple : "TodoList - Gestion de Tâches"

2. **Mots-clés** (Google Play : description / iOS : champ dédié)
   - Rechercher les mots-clés populaires
   - Éviter la sur-optimisation
   - Utiliser des variations

3. **Description**
   - Premiers 250 caractères = critiques (visible sans "lire plus")
   - Inclure naturellement les mots-clés
   - Listes à puces pour la lisibilité
   - Call-to-action clair

4. **Icône**
   - Simple et reconnaissable
   - Se démarquer de la concurrence
   - Tester différentes versions (A/B testing)

5. **Captures d'écran**
   - Première capture = la plus importante
   - Montrer les fonctionnalités clés
   - Ajouter du texte explicatif si pertinent

6. **Avis et notes**
   - Note moyenne > 4.0 recommandée
   - Nombre d'avis élevé = confiance
   - Répondre aux avis négatifs

**Outils ASO** :
- App Annie (analytics)
- Sensor Tower (recherche de mots-clés)
- Mobile Action (suivi ASO)
- TheTool (générateur de captures d'écran)

### Stratégies de lancement

**Pré-lancement** :

```
T-30 jours : Créer page de destination (landing page)  
T-21 jours : Ouvrir inscriptions bêta  
T-14 jours : Campagne teasing sur réseaux sociaux  
T-7 jours  : Envoyer communiqué de presse  
T-3 jours  : Contacter influenceurs/blogueurs  
Jour J     : Lancement officiel !
```

**Actions le jour du lancement** :
- ✅ Post sur tous les réseaux sociaux
- ✅ Email aux beta testeurs
- ✅ Publication sur Product Hunt / Hacker News
- ✅ Contacter médias tech
- ✅ Activer campagnes publicitaires
- ✅ Surveiller les premiers retours

**Post-lancement** :
- Jours 1-7 : Surveiller les crashs et bugs
- Jours 7-30 : Optimiser selon retours utilisateurs
- Mois 2+ : Stratégie de croissance long terme

### Campagnes publicitaires

**Google Ads (pour Android)** :
- Google Search
- Google Display Network
- YouTube
- Google Play Store (app campaigns)

**Apple Search Ads (pour iOS)** :
- Annonces dans l'App Store
- Ciblage par mots-clés
- Moins compétitif que Google Ads

**Budget recommandé pour démarrer** :
```
Test initial : 50-100 € / jour pendant 7 jours  
Optimisation : Ajuster selon CPI (Cost Per Install)  
Objectif CPI : 0.50 € - 2.00 € (variable selon secteur)
```

## Conformité et légal

### Mentions légales dans l'app

**Écran "À propos"** minimal :

```pascal
procedure ShowAboutScreen;  
begin
  ShowMessage(
    'MonApp v1.2.0' + sLineBreak +
    'Copyright © 2025 Votre Société' + sLineBreak +
    sLineBreak +
    'Développé avec FreePascal' + sLineBreak +
    sLineBreak +
    'Conditions d''utilisation : monapp.com/terms' + sLineBreak +
    'Politique de confidentialité : monapp.com/privacy' + sLineBreak +
    'Licences : monapp.com/licenses'
  );
end;
```

### Écran des licences open source

Si vous utilisez des bibliothèques open source :

```pascal
unit OpenSourceLicenses;

{$mode objfpc}{$H+}

interface

const
  LICENSES_TEXT =
    'FreePascal Compiler' + sLineBreak +
    'License: GPL + Static Linking Exception' + sLineBreak +
    'https://www.freepascal.org/license.html' + sLineBreak +
    sLineBreak +
    'Synapse Library' + sLineBreak +
    'License: Modified BSD License' + sLineBreak +
    'Copyright (c) Lukas Gebauer' + sLineBreak +
    sLineBreak +
    'BGRABitmap' + sLineBreak +
    'License: Modified LGPL' + sLineBreak +
    'Copyright (c) Johann Elsass' + sLineBreak;

procedure ShowLicenses;

implementation

uses
  Dialogs;

procedure ShowLicenses;  
begin
  ShowMessage(LICENSES_TEXT);
end;

end.
```

### Conditions générales d'utilisation (CGU)

**Structure minimale** :

```markdown
# Conditions Générales d'Utilisation

## 1. Acceptation des conditions
En téléchargeant et utilisant MonApp, vous acceptez ces conditions.

## 2. Description du service
MonApp fournit [description des fonctionnalités].

## 3. Compte utilisateur
- Vous êtes responsable de la sécurité de votre compte
- Un compte par personne
- Interdiction de partager vos identifiants

## 4. Utilisation acceptable
Vous vous engagez à ne pas :
- Utiliser l'app de manière illégale
- Transmettre du contenu offensant
- Tenter de contourner les mesures de sécurité
- [...]

## 5. Propriété intellectuelle
Tout le contenu de MonApp est protégé par le droit d'auteur.

## 6. Limitation de responsabilité
MonApp est fourni "tel quel" sans garantie.

## 7. Modifications
Nous nous réservons le droit de modifier ces conditions.

## 8. Contact
Pour toute question : legal@monapp.com

Date de dernière mise à jour : [Date]
```

### Conformité RGPD

**Obligations principales** :

1. **Consentement explicite**
```pascal
procedure RequestConsent;  
begin
  if not IsConsentGiven then
  begin
    ShowConsentDialog(
      'Nous utilisons des cookies pour améliorer votre expérience.' +
      sLineBreak + sLineBreak +
      'En continuant, vous acceptez notre politique de confidentialité.'
    );
  end;
end;
```

2. **Droit d'accès et suppression**
```pascal
procedure ExportUserData;  
begin
  // Générer un fichier JSON avec toutes les données utilisateur
  GenerateJSON(UserData, 'user_data.json');
end;

procedure DeleteUserAccount;  
begin
  if Confirm('Êtes-vous sûr de vouloir supprimer votre compte ?') then
  begin
    DeleteAllUserData;
    LogoutUser;
    ShowMessage('Votre compte a été supprimé.');
  end;
end;
```

## Checklist finale avant publication

### ✅ Checklist technique

**Application** :
- [ ] Compilée en mode Release (optimisations activées)
- [ ] Testée sur différents appareils (min 3)
- [ ] Aucun crash ou bug critique
- [ ] Performances acceptables (temps de chargement < 3s)
- [ ] Consommation batterie optimisée
- [ ] Mémoire bien gérée (pas de fuites)
- [ ] Toutes les traductions vérifiées
- [ ] Mode hors-ligne fonctionnel (si applicable)

**Sécurité** :
- [ ] Communications HTTPS uniquement
- [ ] Données sensibles chiffrées
- [ ] Clés API sécurisées (pas en clair dans le code)
- [ ] Certificats à jour
- [ ] Permissions justifiées et minimales

**Assets** :
- [ ] Toutes les icônes aux bonnes dimensions
- [ ] Captures d'écran pour tous les formats requis
- [ ] Images optimisées (taille de fichier réduite)
- [ ] Bannières et graphiques de qualité

### ✅ Checklist contenu

**Store listing** :
- [ ] Titre optimisé (mots-clés pertinents)
- [ ] Description complète et engageante
- [ ] Mots-clés recherchés et pertinents
- [ ] Captures d'écran attractives avec texte explicatif
- [ ] Vidéo de démonstration (recommandé)
- [ ] Classification de contenu correcte

**Légal** :
- [ ] Politique de confidentialité publiée
- [ ] CGU publiées (si nécessaire)
- [ ] Mentions légales dans l'app
- [ ] Licences open source incluses
- [ ] URL de support fonctionnelle

**Marketing** :
- [ ] Page de destination (landing page) créée
- [ ] Profils réseaux sociaux prêts
- [ ] Communiqué de presse rédigé
- [ ] Liste de contacts presse/influenceurs
- [ ] Email de lancement préparé

### ✅ Checklist post-lancement

**Jour 1** :
- [ ] Surveiller les crashs en temps réel
- [ ] Répondre aux premiers avis
- [ ] Vérifier les métriques de téléchargement
- [ ] Poster sur réseaux sociaux

**Semaine 1** :
- [ ] Analyser les retours utilisateurs
- [ ] Corriger les bugs urgents si nécessaires
- [ ] Optimiser ASO selon performance
- [ ] Répondre à tous les avis négatifs

**Mois 1** :
- [ ] Planifier la prochaine mise à jour
- [ ] Analyser les métriques d'engagement
- [ ] Ajuster la stratégie marketing
- [ ] Implémenter les suggestions utilisateurs prioritaires

## Outils et ressources

### Outils essentiels

| Catégorie | Outil | Usage | Prix |
|-----------|-------|-------|------|
| **Design** | Figma | Conception UI/UX | Gratuit/Payant |
| | Canva | Graphiques marketing | Gratuit/Payant |
| **Développement** | Android Studio | Build Android | Gratuit |
| | Xcode | Build iOS | Gratuit |
| **Testing** | Firebase Test Lab | Tests automatisés | Gratuit/Payant |
| | TestFlight | Bêta iOS | Gratuit |
| **Analytics** | Firebase Analytics | Métriques | Gratuit |
| | App Annie | Intelligence marché | Payant |
| **ASO** | Sensor Tower | Optimisation store | Payant |
| | App Radar | Mots-clés | Freemium |
| **Support** | Zendesk | Tickets support | Payant |
| | Intercom | Chat utilisateur | Payant |

### Ressources éducatives

**Documentation officielle** :
- Google Play Console Help : https://support.google.com/googleplay/android-developer
- App Store Connect Help : https://developer.apple.com/help/app-store-connect/

**Guides et tutoriels** :
- Android Developers : https://developer.android.com/
- iOS Developer : https://developer.apple.com/documentation/

**Communautés** :
- r/androiddev (Reddit)
- r/iOSProgramming (Reddit)
- Stack Overflow

**Blogs et newsletters** :
- Android Developers Blog
- iOS Dev Weekly
- Mobile Dev Memo

## Erreurs courantes à éviter

### ❌ Erreurs techniques

1. **Oublier d'incrémenter versionCode/CFBundleVersion**
   - Résultat : Impossible de publier la mise à jour

2. **Perdre la clé de signature**
   - Résultat : Impossible de mettre à jour l'app (Android)

3. **Tester uniquement sur émulateur**
   - Résultat : Bugs sur appareils réels non détectés

4. **Permissions inutiles demandées**
   - Résultat : Rejet ou méfiance utilisateurs

5. **APK/IPA non signé correctement**
   - Résultat : Échec de validation

### ❌ Erreurs de contenu

1. **Description trop courte ou vague**
   - Résultat : Faible taux de conversion

2. **Captures d'écran de mauvaise qualité**
   - Résultat : App paraît non professionnelle

3. **Mots-clés non pertinents**
   - Résultat : Mauvais référencement

4. **Politique de confidentialité manquante**
   - Résultat : Rejet automatique

5. **Prix incohérent entre stores**
   - Résultat : Confusion utilisateurs

### ❌ Erreurs stratégiques

1. **Lancer sans phase de test**
   - Résultat : Bugs massifs au lancement

2. **Ignorer les premiers retours négatifs**
   - Résultat : Mauvaise réputation durable

3. **Pas de plan de mise à jour**
   - Résultat : App devient obsolète rapidement

4. **Sur-monétisation agressive**
   - Résultat : Avis négatifs, désinstallations

5. **Négliger l'ASO**
   - Résultat : Visibilité faible, peu de téléchargements

## Conclusion

La distribution d'applications sur les stores est un processus qui demande de la rigueur et de la patience, mais qui est tout à fait accessible avec FreePascal. Bien que le langage ne soit pas mainstream pour le mobile, les applications compilées sont identiques aux applications natives et ne rencontrent aucun problème de validation.

### Points clés à retenir

✅ **Préparation** : Testez exhaustivement avant de publier  
✅ **Sécurité** : Gardez vos clés de signature en lieu sûr  
✅ **Conformité** : Respectez les guidelines et lois (RGPD, etc.)  
✅ **Marketing** : L'ASO est crucial pour la visibilité  
✅ **Support** : Répondez aux utilisateurs rapidement  
✅ **Itération** : Améliorez continuellement selon les retours

### Workflow recommandé

1. Développer et tester localement
2. Tester avec beta testers (famille, amis, communauté)
3. Publier en test interne/bêta sur les stores
4. Corriger les bugs remontés
5. Lancer en production avec déploiement progressif
6. Surveiller les métriques et retours
7. Itérer avec des mises à jour régulières

### Prochaines étapes

- Rejoindre les communautés de développeurs mobile
- Étudier les apps à succès dans votre catégorie
- Tester votre app sur le maximum d'appareils possible
- Préparer un calendrier de mises à jour (mensuel recommandé)
- Construire une communauté autour de votre app

> 💡 **Conseil final** : La publication est juste le début ! Une app à succès nécessite un engagement continu : corrections, nouvelles fonctionnalités, écoute des utilisateurs. Restez humble, apprenez de vos erreurs, et améliorez-vous constamment.

**Bonne chance pour la publication de votre application FreePascal ! 🚀📱**

⏭️ [Systèmes Embarqués et IoT](/14-systemes-embarques-iot/README.md)
