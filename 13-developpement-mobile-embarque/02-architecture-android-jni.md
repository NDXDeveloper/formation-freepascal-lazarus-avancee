🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.2 Architecture Android et JNI

## Introduction

Lorsque vous développez une application Android avec LAMW (Lazarus Android Module Wizard), vous créez en réalité une application hybride qui combine deux mondes technologiques : **Java/Kotlin** pour l'interface Android native et **FreePascal** pour la logique métier. Le pont entre ces deux univers s'appelle **JNI** (Java Native Interface).

Cette section explore en profondeur comment Android fonctionne, comment JNI permet la communication entre Java et Pascal, et comment maîtriser cette architecture pour créer des applications performantes et maintenables.

## Comprendre l'architecture Android

### Vue d'ensemble du système Android

Android est un système d'exploitation basé sur Linux, spécialement optimisé pour les appareils mobiles. Son architecture est organisée en couches :

```
┌─────────────────────────────────────────────┐
│        Applications                         │
│    (Contacts, Navigateur, etc.)             │
└─────────────────────────────────────────────┘
                    ↕
┌─────────────────────────────────────────────┐
│     Framework d'Applications                │
│  (Activity Manager, Content Provider,       │
│   View System, Notification Manager)        │
└─────────────────────────────────────────────┘
                    ↕
┌─────────────────────────────────────────────┐
│      Bibliothèques & Runtime Android        │
│  ┌──────────────┐  ┌────────────────────┐   │
│  │ Android      │  │ Bibliothèques      │   │
│  │ Runtime      │  │ natives C/C++      │   │
│  │ (ART/Dalvik) │  │ (libc, SSL, etc.)  │   │
│  └──────────────┘  └────────────────────┘   │
└─────────────────────────────────────────────┘
                    ↕
┌─────────────────────────────────────────────┐
│         Abstraction Matériel (HAL)          │
└─────────────────────────────────────────────┘
                    ↕
┌─────────────────────────────────────────────┐
│            Noyau Linux                      │
│     (Drivers, Gestion mémoire, etc.)        │
└─────────────────────────────────────────────┘
```

### Composants fondamentaux d'une application Android

#### Activities (Activités)

Une **Activity** représente un écran unique avec une interface utilisateur. C'est le composant le plus visible pour l'utilisateur.

**Caractéristiques** :
- Chaque écran = une Activity
- Possède un cycle de vie propre
- Gère les interactions utilisateur
- Peut lancer d'autres Activities

**Exemple conceptuel** :
```
Application "Contacts"
├── MainActivity (Liste des contacts)
├── DetailActivity (Détails d'un contact)
└── EditActivity (Édition d'un contact)
```

#### Services

Un **Service** est un composant qui s'exécute en arrière-plan, sans interface utilisateur.

**Usages typiques** :
- Lecture de musique
- Téléchargement de fichiers
- Synchronisation de données
- Traitement de longue durée

**Types** :
- **Started Service** : Lancé et s'exécute indépendamment
- **Bound Service** : Lié à une Activity, se termine avec elle

#### Content Providers

Un **Content Provider** gère l'accès aux données partagées.

**Exemples** :
- Contacts du téléphone
- Photos de la galerie
- Calendrier
- Base de données de l'application

#### Broadcast Receivers

Un **Broadcast Receiver** répond aux annonces système ou d'autres applications.

**Événements possibles** :
- Batterie faible
- Message reçu
- Connexion réseau établie
- Téléchargement terminé

### Cycle de vie d'une Activity

Le cycle de vie d'une Activity est crucial pour comprendre le fonctionnement d'Android :

```
        [Application lancée]
                 ↓
            onCreate()  ← Création, initialisation
                 ↓
             onStart()  ← Visible mais pas interactive
                 ↓
            onResume()  ← Au premier plan, interactive
                 ↓
         [Activity active]
                 │
    ┌────────────┴────────────┐
    │                         │
    ↓ (Interruption)          ↓ (Masquée)
onPause()                 onPause()
    ↓                         ↓
[Partiellement visible]    onStop()
    ↓                         ↓
onResume() ←────────── [Complètement cachée]
                             ↓
                        onDestroy()
                             ↓
                    [Activity détruite]
```

**Méthodes du cycle de vie** :

1. **onCreate()** : L'Activity est créée
   - Initialisation des composants
   - Chargement de l'interface
   - Restauration de l'état sauvegardé

2. **onStart()** : L'Activity devient visible
   - Préparation à l'affichage
   - Enregistrement des listeners

3. **onResume()** : L'Activity est au premier plan
   - Démarrage des animations
   - Activation de la caméra, GPS, etc.
   - L'utilisateur peut interagir

4. **onPause()** : L'Activity perd le focus
   - Pause des animations
   - Sauvegarde des données critiques
   - Libération des ressources lourdes

5. **onStop()** : L'Activity n'est plus visible
   - Arrêt des mises à jour
   - Sauvegarde complète de l'état

6. **onDestroy()** : L'Activity est détruite
   - Libération de toutes les ressources
   - Nettoyage final

**Important** : Android peut tuer votre Activity à tout moment pour libérer de la mémoire, surtout après onStop().

### AndroidManifest.xml

Le fichier **AndroidManifest.xml** est la carte d'identité de votre application. Il déclare :

```xml
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="com.example.monapp"
    android:versionCode="1"
    android:versionName="1.0">

    <!-- Version Android minimale requise -->
    <uses-sdk
        android:minSdkVersion="21"
        android:targetSdkVersion="33" />

    <!-- Permissions nécessaires -->
    <uses-permission android:name="android.permission.INTERNET" />
    <uses-permission android:name="android.permission.CAMERA" />
    <uses-permission android:name="android.permission.ACCESS_FINE_LOCATION" />

    <!-- Configuration de l'application -->
    <application
        android:allowBackup="true"
        android:icon="@drawable/icon"
        android:label="@string/app_name"
        android:theme="@style/AppTheme">

        <!-- Activity principale -->
        <activity
            android:name=".MainActivity"
            android:label="@string/app_name"
            android:exported="true">
            <intent-filter>
                <action android:name="android.intent.action.MAIN" />
                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
        </activity>

        <!-- Autres activities -->
        <activity android:name=".DetailActivity" />

        <!-- Services -->
        <service android:name=".MyBackgroundService" />

    </application>

</manifest>
```

**Éléments clés** :

- **package** : Identifiant unique de l'application (nom inversé)
- **versionCode** : Numéro de version interne (incrémenté à chaque release)
- **versionName** : Version lisible par l'utilisateur (ex: "1.2.3")
- **uses-sdk** : Versions Android supportées
- **uses-permission** : Permissions requises
- **application** : Configuration globale de l'app
- **activity** : Déclaration de chaque écran
- **intent-filter** : Définit comment lancer l'Activity

## JNI : Le pont Java-Pascal

### Qu'est-ce que JNI ?

**JNI (Java Native Interface)** est une interface de programmation qui permet au code Java d'interagir avec du code natif (C, C++, Pascal, etc.).

**Pourquoi utiliser JNI ?** :
- **Performance** : Code natif plus rapide pour calculs intensifs
- **Réutilisation** : Utiliser des bibliothèques existantes en C/C++/Pascal
- **Accès matériel** : Contrôle bas niveau du hardware
- **Portage** : Adapter du code existant pour Android

### Architecture avec LAMW et JNI

```
┌────────────────────────────────────────────────────────┐
│              Application Android                       │
├────────────────────────────────────────────────────────┤
│                                                        │
│  ┌───────────────────────────────────────────────┐     │
│  │         Couche Java (Activity)                │     │
│  │                                               │     │
│  │  public class MainActivity extends Activity { │     │
│  │      private native void jniMethod();         │     │
│  │                                               │     │
│  │      public void onCreate() {                 │     │
│  │          jniMethod(); // Appel vers Pascal    │     │
│  │      }                                        │     │
│  │  }                                            │     │
│  └─────────────────┬─────────────────────────────┘     │
│                    │                                   │
│                    │ JNI Bridge                        │
│                    │ (Communication bi-directionnelle) │
│                    │                                   │
│  ┌─────────────────▼─────────────────────────────┐     │
│  │         Couche Pascal (Logique)               │     │
│  │                                               │     │
│  │  procedure jniMethod; cdecl;                  │     │
│  │  begin                                        │     │
│  │      // Logique métier en Pascal              │     │
│  │      CalculerResultat();                      │     │
│  │      AppelerMethodeJava(); // Retour vers Java│     │
│  │  end;                                         │     │
│  │                                               │     │
│  │  exports jniMethod;                           │     │
│  └───────────────────────────────────────────────┘     │
│                                                        │
│  Compilé en bibliothèque native (.so)                  │
│  libapp.so (ARM32/ARM64)                               │
└────────────────────────────────────────────────────────┘
```

### Principes de fonctionnement de JNI

#### Appel Java → Pascal (Downward Call)

**Côté Java** : Déclaration d'une méthode native
```java
public class MainActivity extends Activity {
    // Déclaration d'une méthode native
    private native int calculer(int a, int b);

    public void executerCalcul() {
        int resultat = calculer(5, 3); // Appel vers Pascal
        System.out.println("Résultat: " + resultat);
    }

    // Chargement de la bibliothèque native
    static {
        System.loadLibrary("monapp");
    }
}
```

**Côté Pascal** : Implémentation de la fonction
```pascal
function Java_com_example_monapp_MainActivity_calculer(
    env: PJNIEnv;      // Environnement JNI
    this: jobject;     // Référence à l'objet Java
    a: jint;           // Premier paramètre
    b: jint            // Deuxième paramètre
): jint; cdecl;        // Convention d'appel C
begin
    Result := a + b;   // Logique en Pascal
end;

// Export de la fonction pour JNI
exports
    Java_com_example_monapp_MainActivity_calculer;
```

**Convention de nommage JNI** :
```
Java_<nom_package_avec_underscores>_<nom_classe>_<nom_methode>
```

Exemple :
- Package : `com.example.monapp`
- Classe : `MainActivity`
- Méthode : `calculer`
- Nom JNI : `Java_com_example_monapp_MainActivity_calculer`

#### Appel Pascal → Java (Upward Call)

**Côté Pascal** : Appel d'une méthode Java
```pascal
procedure AppelerMethodeJava;
var
    jcls: jclass;
    jmethod: jmethodID;
    jstr: jstring;
begin
    // Obtenir la classe Java
    jcls := (*env)->FindClass(env, 'com/example/monapp/MainActivity');

    // Obtenir l'ID de la méthode
    jmethod := (*env)->GetMethodID(env, jcls, 'afficherMessage',
                                    '(Ljava/lang/String;)V');

    // Créer une chaîne Java
    jstr := (*env)->NewStringUTF(env, 'Bonjour depuis Pascal!');

    // Appeler la méthode Java
    (*env)->CallVoidMethod(env, this, jmethod, jstr);

    // Libérer les références locales
    (*env)->DeleteLocalRef(env, jstr);
    (*env)->DeleteLocalRef(env, jcls);
end;
```

**Côté Java** : Méthode appelée depuis Pascal
```java
public class MainActivity extends Activity {
    public void afficherMessage(String message) {
        Toast.makeText(this, message, Toast.LENGTH_SHORT).show();
    }
}
```

### Types de données JNI

#### Types primitifs

Correspondance entre Java et JNI :

| Type Java | Type JNI | Type Pascal | Taille |
|-----------|----------|-------------|--------|
| boolean   | jboolean | jboolean    | 8 bits |
| byte      | jbyte    | jbyte       | 8 bits |
| char      | jchar    | jchar       | 16 bits |
| short     | jshort   | jshort      | 16 bits |
| int       | jint     | jint        | 32 bits |
| long      | jlong    | jlong       | 64 bits |
| float     | jfloat   | jfloat      | 32 bits |
| double    | jdouble  | jdouble     | 64 bits |
| void      | void     | void        | - |

**Exemple d'utilisation** :
```pascal
function AdditionnerNombres(
    env: PJNIEnv;
    this: jobject;
    a: jint;           // int Java
    b: jdouble         // double Java
): jdouble; cdecl;     // Retourne un double
begin
    Result := a + b;
end;
```

#### Types références (objets)

Les objets Java sont manipulés via des références opaques :

| Type Java       | Type JNI    | Description |
|-----------------|-------------|-------------|
| Object          | jobject     | Objet générique |
| Class           | jclass      | Classe Java |
| String          | jstring     | Chaîne de caractères |
| Array           | jarray      | Tableau générique |
| Object[]        | jobjectArray| Tableau d'objets |
| boolean[]       | jbooleanArray| Tableau de booléens |
| byte[]          | jbyteArray  | Tableau d'octets |
| int[]           | jintArray   | Tableau d'entiers |
| Throwable       | jthrowable  | Exception Java |

**Manipulation de chaînes** :
```pascal
function ConvertirTexte(
    env: PJNIEnv;
    this: jobject;
    javaString: jstring
): jstring; cdecl;
var
    nativeString: PChar;
    resultat: String;
begin
    // Convertir jstring en PChar
    nativeString := (*env)->GetStringUTFChars(env, javaString, nil);

    // Traiter la chaîne en Pascal
    resultat := UpperCase(String(nativeString));

    // Libérer la chaîne native
    (*env)->ReleaseStringUTFChars(env, javaString, nativeString);

    // Créer une nouvelle jstring
    Result := (*env)->NewStringUTF(env, PChar(resultat));
end;
```

**Manipulation de tableaux** :
```pascal
function SommeTableau(
    env: PJNIEnv;
    this: jobject;
    tableau: jintArray
): jint; cdecl;
var
    longueur: jsize;
    elements: PJint;
    i: Integer;
    somme: jint;
begin
    // Obtenir la longueur du tableau
    longueur := (*env)->GetArrayLength(env, tableau);

    // Obtenir un pointeur vers les éléments
    elements := (*env)->GetIntArrayElements(env, tableau, nil);

    // Calculer la somme
    somme := 0;
    for i := 0 to longueur - 1 do
        somme := somme + elements[i];

    // Libérer les éléments
    (*env)->ReleaseIntArrayElements(env, tableau, elements, 0);

    Result := somme;
end;
```

### Environnement JNI (JNIEnv)

**JNIEnv** est une structure contenant toutes les fonctions JNI disponibles. C'est votre interface vers la JVM (Java Virtual Machine).

**Structure** :
```pascal
type
    PJNIEnv = ^JNIEnv;
    JNIEnv = record
        functions: Pointer;  // Pointeur vers table de fonctions
    end;
```

**Fonctions principales** :

#### Manipulation de classes
```pascal
// Trouver une classe
jcls := (*env)->FindClass(env, 'java/lang/String');

// Obtenir la classe d'un objet
jcls := (*env)->GetObjectClass(env, obj);
```

#### Manipulation de méthodes
```pascal
// Obtenir l'ID d'une méthode
jmethod := (*env)->GetMethodID(env, jcls, 'toString', '()Ljava/lang/String;');

// Obtenir l'ID d'une méthode statique
jstaticmethod := (*env)->GetStaticMethodID(env, jcls, 'valueOf', '(I)Ljava/lang/String;');

// Appeler une méthode
result := (*env)->CallObjectMethod(env, obj, jmethod);

// Appeler une méthode statique
result := (*env)->CallStaticObjectMethod(env, jcls, jstaticmethod, 42);
```

#### Manipulation de champs (fields)
```pascal
// Obtenir l'ID d'un champ
jfield := (*env)->GetFieldID(env, jcls, 'value', 'I');

// Lire un champ
value := (*env)->GetIntField(env, obj, jfield);

// Écrire un champ
(*env)->SetIntField(env, obj, jfield, 100);
```

#### Gestion de la mémoire
```pascal
// Créer une référence globale (survit aux appels JNI)
globalRef := (*env)->NewGlobalRef(env, obj);

// Libérer une référence globale
(*env)->DeleteGlobalRef(env, globalRef);

// Créer une référence locale (automatiquement libérée)
localRef := (*env)->NewLocalRef(env, obj);

// Libérer une référence locale (optionnel mais recommandé)
(*env)->DeleteLocalRef(env, localRef);
```

### Signatures de méthodes JNI

Les signatures JNI décrivent les types de paramètres et de retour d'une méthode Java.

**Format général** :
```
(ParamètresTypes)TypeRetour
```

**Types de base** :

| Type Java | Signature |
|-----------|-----------|
| boolean   | Z         |
| byte      | B         |
| char      | C         |
| short     | S         |
| int       | I         |
| long      | J         |
| float     | F         |
| double    | D         |
| void      | V         |

**Types référence** :

| Type Java       | Signature |
|-----------------|-----------|
| String          | Ljava/lang/String; |
| Object          | Ljava/lang/Object; |
| Class[]         | [Ljava/lang/Class; |
| int[]           | [I |
| Ma classe custom| Lcom/example/MaClasse; |

**Exemples de signatures** :

```java
// Java
void method()
// Signature JNI: ()V

// Java
int add(int a, int b)
// Signature JNI: (II)I

// Java
String concat(String a, String b)
// Signature JNI: (Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

// Java
void setValues(int[] values)
// Signature JNI: ([I)V

// Java
MyObject createObject(String name, int age, double score)
// Signature JNI: (Ljava/lang/String;ID)Lcom/example/MyObject;
```

**Outil pour générer les signatures** :
```bash
javap -s -p MainActivity.class
```

## LAMW : Simplification de JNI

### Comment LAMW facilite JNI

LAMW (Lazarus Android Module Wizard) génère automatiquement la plupart du code JNI, vous permettant de vous concentrer sur la logique métier.

**Ce que LAMW fait pour vous** :

1. **Génération automatique du code Java**
   - Activities avec cycle de vie complet
   - Classes wrapper pour les contrôles
   - Configuration AndroidManifest.xml

2. **Génération du code Pascal**
   - Déclarations JNI correctes
   - Exports des fonctions
   - Gestion des événements

3. **Configuration du build**
   - Compilation croisée ARM
   - Liaison avec la bibliothèque native
   - Packaging APK

### Structure d'un projet LAMW

```
MonProjet/
├── jni/                           ← Code Pascal
│   ├── unit1.pas                  ← Logique principale
│   ├── controls.pas               ← Contrôles LAMW
│   └── libmonprojet.lpr           ← Programme principal
│
├── src/                           ← Code Java
│   └── com/example/monprojet/
│       ├── App.java               ← Application
│       ├── MainActivity.java      ← Activity principale
│       └── Controls.java          ← Wrapper contrôles
│
├── libs/                          ← Bibliothèques natives
│   ├── armeabi-v7a/
│   │   └── libmonprojet.so        ← Bibliothèque ARM32
│   └── arm64-v8a/
│       └── libmonprojet.so        ← Bibliothèque ARM64
│
├── res/                           ← Ressources Android
│   ├── drawable/                  ← Images
│   ├── layout/                    ← Layouts XML
│   └── values/                    ← Strings, couleurs
│
└── AndroidManifest.xml            ← Configuration app
```

### Composants LAMW et leur architecture JNI

#### jForm - Le formulaire principal

**Côté Pascal** :
```pascal
type
    TAndroidModule1 = class(jForm)
    private
        // Variables privées
    public
        // Méthodes publiques
    end;

var
    AndroidModule1: TAndroidModule1;
```

**Côté Java généré** :
```java
public class MainActivity extends Activity {
    // Instance native
    private long nativeInstance;

    // Méthodes natives (implémentées en Pascal)
    private native void jniOnCreate();
    private native void jniOnStart();
    private native void jniOnResume();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        nativeInstance = jniCreateInstance();
        jniOnCreate();
    }
}
```

#### Contrôles visuels (jButton, jTextView, etc.)

**Exemple avec jButton** :

**Côté Pascal** :
```pascal
TAndroidModule1 = class(jForm)
    jButton1: jButton;
    procedure jButton1Click(Sender: TObject);
end;

procedure TAndroidModule1.jButton1Click(Sender: TObject);
begin
    ShowMessage('Bouton cliqué !');
end;
```

**Côté Java généré** :
```java
public class jButton extends View implements OnClickListener {
    private long pascalInstance;

    // Méthode native appelée lors du clic
    private native void jniOnClick(long instance);

    @Override
    public void onClick(View v) {
        jniOnClick(pascalInstance);  // Appel vers Pascal
    }
}
```

**Flux d'événement** :
```
Utilisateur clique
       ↓
onClick() Java est appelé
       ↓
jniOnClick() appelle la fonction Pascal
       ↓
Java_...jButton_jniOnClick() exécute
       ↓
jButton1Click() en Pascal est déclenché
```

### Communication bi-directionnelle

#### Pascal appelle Java (Actions sur l'UI)

```pascal
// Changer le texte d'un TextView
jTextView1.Text := 'Nouveau texte';

// En coulisse, LAMW génère :
procedure jTextView_SetText(env: PJNIEnv; obj: jobject; text: jstring); cdecl;
begin
    // Appel de la méthode Java setText()
    (*env)->CallVoidMethod(env, obj, methodID_setText, text);
end;
```

#### Java appelle Pascal (Événements)

```java
// Événement déclenché par l'utilisateur
public void onButtonClick() {
    // Appel de la méthode native Pascal
    jniHandleButtonClick(nativeInstance);
}
```

```pascal
// Fonction Pascal appelée par Java
procedure Java_..._jniHandleButtonClick(
    env: PJNIEnv;
    this: jobject;
    instance: jlong
): void; cdecl;
var
    Form: TAndroidModule1;
begin
    Form := TAndroidModule1(Pointer(instance));
    Form.jButton1Click(nil);  // Déclenche l'événement Pascal
end;
```

## Gestion avancée de JNI

### Threads et JNI

**Problème** : Chaque thread a son propre JNIEnv. Vous ne pouvez pas partager un JNIEnv entre threads.

**Solution** : Attacher le thread à la JVM

```pascal
procedure ThreadWorker;
var
    env: PJNIEnv;
    vm: PJavaVM;
    attached: Boolean;
begin
    vm := GetJavaVM();  // Obtenir la VM globale
    attached := False;

    // Attacher le thread
    if (*vm)->AttachCurrentThread(vm, @env, nil) = JNI_OK then
    begin
        attached := True;
        try
            // Utiliser env pour les appels JNI
            // ...
        finally
            // Détacher le thread
            if attached then
                (*vm)->DetachCurrentThread(vm);
        end;
    end;
end;
```

### Gestion des exceptions Java

**Vérifier et gérer les exceptions** :

```pascal
procedure AppelerMethodePotentiellementDangereuse(env: PJNIEnv);
var
    exception: jthrowable;
begin
    // Appel d'une méthode Java qui pourrait lever une exception
    (*env)->CallVoidMethod(env, obj, methodID);

    // Vérifier si une exception s'est produite
    if (*env)->ExceptionCheck(env) = JNI_TRUE then
    begin
        // Obtenir l'exception
        exception := (*env)->ExceptionOccurred(env);

        // Logger l'exception (affiche dans logcat)
        (*env)->ExceptionDescribe(env);

        // Nettoyer l'exception
        (*env)->ExceptionClear(env);

        // Gérer l'erreur en Pascal
        WriteLn('Erreur Java détectée');
    end;
end;
```

### Optimisations JNI

#### Mise en cache des IDs

**Problème** : Chercher les IDs de méthodes/champs à chaque appel est coûteux.

**Solution** : Mettre en cache les IDs lors de l'initialisation

```pascal
var
    cachedMethodID: jmethodID;
    cachedFieldID: jfieldID;
    cachedClassRef: jclass;

// Initialisation (une seule fois)
procedure InitializeJNICache(env: PJNIEnv);
var
    localClass: jclass;
begin
    // Trouver la classe
    localClass := (*env)->FindClass(env, 'com/example/MyClass');

    // Créer une référence globale
    cachedClassRef := (*env)->NewGlobalRef(env, localClass);

    // Mettre en cache les IDs
    cachedMethodID := (*env)->GetMethodID(env, cachedClassRef,
                                          'myMethod', '()V');
    cachedFieldID := (*env)->GetFieldID(env, cachedClassRef,
                                        'myField', 'I');

    // Libérer la référence locale
    (*env)->DeleteLocalRef(env, localClass);
end;

// Utilisation ultérieure (rapide)
procedure AppelerMethode(env: PJNIEnv; obj: jobject);
begin
    (*env)->CallVoidMethod(env, obj, cachedMethodID);
end;
```

#### Minimiser les conversions de types

**Inefficace** :
```pascal
for i := 1 to 1000 do
begin
    jstr := ConvertToJString(env, 'Texte');  // Conversion répétée
    (*env)->CallVoidMethod(env, obj, methodID, jstr);
    (*env)->DeleteLocalRef(env, jstr);
end;
```

**Efficace** :
```pascal
jstr := ConvertToJString(env, 'Texte');  // Conversion unique
try
    for i := 1 to 1000 do
        (*env)->CallVoidMethod(env, obj, methodID, jstr);
finally
    (*env)->DeleteLocalRef(env, jstr);  // Libération unique
end;
```

**Gain** : 1000× moins d'allocations et de conversions.

#### Utiliser des tableaux primitifs

**Pour transférer beaucoup de données** :

```pascal
// Inefficace : appels multiples
procedure TransfererDonneesLent(env: PJNIEnv; obj: jobject; data: array of Integer);
var
    i: Integer;
begin
    for i := 0 to High(data) do
        (*env)->CallVoidMethod(env, obj, methodID, data[i]);  // 1000 appels JNI
end;

// Efficace : un seul appel avec tableau
procedure TransfererDonneesRapide(env: PJNIEnv; obj: jobject; data: array of Integer);
var
    jarray: jintArray;
    i: Integer;
begin
    // Créer un tableau Java
    jarray := (*env)->NewIntArray(env, Length(data));

    // Remplir le tableau
    (*env)->SetIntArrayRegion(env, jarray, 0, Length(data), @data[0]);

    // Un seul appel JNI avec tout le tableau
    (*env)->CallVoidMethod(env, obj, methodID, jarray);

    // Libérer
    (*env)->DeleteLocalRef(env, jarray);
end;
```

**Résultat** : 100-1000× plus rapide pour les gros volumes de données.

#### Références locales et limites

**Problème** : La JVM limite le nombre de références locales (typiquement 512).

**Mauvais code** :
```pascal
procedure TraiterBeaucoupObjets(env: PJNIEnv);
var
    i: Integer;
    obj: jobject;
begin
    for i := 1 to 10000 do  // Dépassera la limite !
    begin
        obj := (*env)->CallObjectMethod(env, ...);
        // obj n'est jamais libéré
        TraiterObjet(obj);
    end;
end;
```

**Solutions** :

**Option 1 : Libérer manuellement**
```pascal
procedure TraiterBeaucoupObjets(env: PJNIEnv);
var
    i: Integer;
    obj: jobject;
begin
    for i := 1 to 10000 do
    begin
        obj := (*env)->CallObjectMethod(env, ...);
        try
            TraiterObjet(obj);
        finally
            (*env)->DeleteLocalRef(env, obj);  // Libération explicite
        end;
    end;
end;
```

**Option 2 : Frame local (plus élégant)**
```pascal
procedure TraiterBeaucoupObjets(env: PJNIEnv);
var
    i: Integer;
    obj: jobject;
begin
    for i := 1 to 10000 do
    begin
        // Créer un frame local (capacité de 10 références)
        (*env)->PushLocalFrame(env, 10);
        try
            obj := (*env)->CallObjectMethod(env, ...);
            TraiterObjet(obj);
        finally
            // Libère automatiquement toutes les références du frame
            (*env)->PopLocalFrame(env, nil);
        end;
    end;
end;
```

### Débogage JNI

#### Activation des logs détaillés

**Dans AndroidManifest.xml** :
```xml
<application
    android:debuggable="true"
    ...>
```

**Dans le code Pascal** :
```pascal
procedure LogJNI(const tag, message: string);
begin
    {$IFDEF DEBUG}
    __android_log_write(ANDROID_LOG_DEBUG, PChar(tag), PChar(message));
    {$ENDIF}
end;

function MaFonctionJNI(env: PJNIEnv; this: jobject): jint; cdecl;
begin
    LogJNI('JNI', 'MaFonctionJNI appelée');

    // Logique...
    Result := 42;

    LogJNI('JNI', 'MaFonctionJNI terminée, résultat: ' + IntToStr(Result));
end;
```

**Visualisation avec logcat** :
```bash
adb logcat | grep "JNI"
```

#### Vérification des erreurs JNI

**Mode CheckJNI** (vérifie automatiquement les erreurs JNI) :

```bash
# Activer CheckJNI
adb shell setprop debug.checkjni 1

# Redémarrer l'app
adb shell am force-stop com.example.monapp
adb shell am start -n com.example.monapp/.MainActivity
```

**Erreurs détectées** :
- Références invalides
- Mauvais types passés
- Dépassement de capacité de références locales
- Utilisation après libération
- Violations de thread

#### Outils de débogage

**1. Android Studio Profiler**
- Affiche les appels JNI
- Montre le temps passé dans le code natif
- Détecte les fuites mémoire

**2. Valgrind (sur émulateur x86)**
```bash
adb shell setprop wrap.com.example.monapp "TMPDIR=/data/data/com.example.monapp \
    logwrapper valgrind --leak-check=full"
```

**3. AddressSanitizer (détection d'erreurs mémoire)**
```pascal
// Dans les options de compilation
{$IFDEF DEBUG}
  {$LINKLIB asan}
{$ENDIF}
```

## Patterns et bonnes pratiques JNI

### Pattern 1 : Wrapper de ressources

**Problème** : Gérer proprement les ressources JNI (références, arrays, strings).

**Solution** : Utiliser des wrappers avec try-finally

```pascal
type
    TJStringWrapper = class
    private
        FEnv: PJNIEnv;
        FJString: jstring;
        FNativeString: PChar;
    public
        constructor Create(AEnv: PJNIEnv; const AString: string);
        destructor Destroy; override;
        property JString: jstring read FJString;
        function ToString: string;
    end;

constructor TJStringWrapper.Create(AEnv: PJNIEnv; const AString: string);
begin
    inherited Create;
    FEnv := AEnv;
    FJString := (*FEnv)->NewStringUTF(FEnv, PChar(AString));
    FNativeString := nil;
end;

destructor TJStringWrapper.Destroy;
begin
    if Assigned(FNativeString) then
        (*FEnv)->ReleaseStringUTFChars(FEnv, FJString, FNativeString);
    if Assigned(FJString) then
        (*FEnv)->DeleteLocalRef(FEnv, FJString);
    inherited;
end;

function TJStringWrapper.ToString: string;
begin
    if not Assigned(FNativeString) then
        FNativeString := (*FEnv)->GetStringUTFChars(FEnv, FJString, nil);
    Result := String(FNativeString);
end;

// Utilisation
procedure ExempleUtilisation(env: PJNIEnv);
var
    jstr: TJStringWrapper;
begin
    jstr := TJStringWrapper.Create(env, 'Mon texte');
    try
        // Utiliser jstr.JString dans les appels JNI
        (*env)->CallVoidMethod(env, obj, methodID, jstr.JString);
    finally
        jstr.Free;  // Libération automatique
    end;
end;
```

### Pattern 2 : Singleton pour la communication Java-Pascal

**Problème** : Besoin d'accéder à l'instance Pascal depuis différents points du code Java.

**Solution** : Pattern Singleton

```pascal
type
    TAppController = class
    private
        class var FInstance: TAppController;
        FEnv: PJNIEnv;
        FMainActivity: jobject;
        class function GetInstance: TAppController; static;
    public
        constructor Create(AEnv: PJNIEnv; AActivity: jobject);
        destructor Destroy; override;
        class property Instance: TAppController read GetInstance;

        procedure ShowToast(const Message: string);
        procedure UpdateUI;
        property Env: PJNIEnv read FEnv;
        property MainActivity: jobject read FMainActivity;
    end;

class function TAppController.GetInstance: TAppController;
begin
    Result := FInstance;
end;

constructor TAppController.Create(AEnv: PJNIEnv; AActivity: jobject);
begin
    inherited Create;
    FEnv := AEnv;
    FMainActivity := (*AEnv)->NewGlobalRef(AEnv, AActivity);
    FInstance := Self;
end;

destructor TAppController.Destroy;
begin
    if Assigned(FMainActivity) then
        (*FEnv)->DeleteGlobalRef(FEnv, FMainActivity);
    FInstance := nil;
    inherited;
end;

procedure TAppController.ShowToast(const Message: string);
var
    jcls: jclass;
    jmethod: jmethodID;
    jstr: jstring;
begin
    jcls := (*FEnv)->GetObjectClass(FEnv, FMainActivity);
    jmethod := (*FEnv)->GetMethodID(FEnv, jcls, 'showToast',
                                     '(Ljava/lang/String;)V');
    jstr := (*FEnv)->NewStringUTF(FEnv, PChar(Message));
    try
        (*FEnv)->CallVoidMethod(FEnv, FMainActivity, jmethod, jstr);
    finally
        (*FEnv)->DeleteLocalRef(FEnv, jstr);
        (*FEnv)->DeleteLocalRef(FEnv, jcls);
    end;
end;

// Utilisation depuis n'importe où dans le code Pascal
procedure MaFonction;
begin
    TAppController.Instance.ShowToast('Opération terminée !');
end;
```

### Pattern 3 : Callbacks asynchrones

**Problème** : Opération longue en Pascal, besoin de notifier Java à la fin.

**Solution** : Interface de callback

**Côté Java** :
```java
public interface OperationCallback {
    void onSuccess(String result);
    void onError(String error);
}

public class MainActivity extends Activity {
    private native void startLongOperation(OperationCallback callback);

    public void lancerOperation() {
        startLongOperation(new OperationCallback() {
            @Override
            public void onSuccess(String result) {
                runOnUiThread(() -> {
                    Toast.makeText(MainActivity.this,
                                  "Succès: " + result,
                                  Toast.LENGTH_SHORT).show();
                });
            }

            @Override
            public void onError(String error) {
                runOnUiThread(() -> {
                    Toast.makeText(MainActivity.this,
                                  "Erreur: " + error,
                                  Toast.LENGTH_SHORT).show();
                });
            }
        });
    }
}
```

**Côté Pascal** :
```pascal
type
    TOperationCallback = record
        env: PJNIEnv;
        callback: jobject;
        successMethodID: jmethodID;
        errorMethodID: jmethodID;
    end;

procedure InitCallback(env: PJNIEnv; callback: jobject;
                      out cb: TOperationCallback);
var
    jcls: jclass;
begin
    cb.env := env;
    cb.callback := (*env)->NewGlobalRef(env, callback);

    jcls := (*env)->GetObjectClass(env, callback);
    cb.successMethodID := (*env)->GetMethodID(env, jcls, 'onSuccess',
                                              '(Ljava/lang/String;)V');
    cb.errorMethodID := (*env)->GetMethodID(env, jcls, 'onError',
                                            '(Ljava/lang/String;)V');
    (*env)->DeleteLocalRef(env, jcls);
end;

procedure CallSuccess(var cb: TOperationCallback; const result: string);
var
    jstr: jstring;
begin
    jstr := (*cb.env)->NewStringUTF(cb.env, PChar(result));
    try
        (*cb.env)->CallVoidMethod(cb.env, cb.callback,
                                  cb.successMethodID, jstr);
    finally
        (*cb.env)->DeleteLocalRef(cb.env, jstr);
    end;
end;

procedure CallError(var cb: TOperationCallback; const error: string);
var
    jstr: jstring;
begin
    jstr := (*cb.env)->NewStringUTF(cb.env, PChar(error));
    try
        (*cb.env)->CallVoidMethod(cb.env, cb.callback,
                                  cb.errorMethodID, jstr);
    finally
        (*cb.env)->DeleteLocalRef(cb.env, jstr);
    end;
end;

procedure FreeCallback(var cb: TOperationCallback);
begin
    if Assigned(cb.callback) then
        (*cb.env)->DeleteGlobalRef(cb.env, cb.callback);
end;

// Thread worker
procedure WorkerThread(data: Pointer);
var
    cb: TOperationCallback;
    vm: PJavaVM;
    env: PJNIEnv;
begin
    cb := TOperationCallback(data^);

    // Attacher le thread à la JVM
    vm := GetJavaVM();
    if (*vm)->AttachCurrentThread(vm, @env, nil) = JNI_OK then
    begin
        try
            // Opération longue
            Sleep(5000);

            // Simuler succès ou erreur
            if Random(2) = 0 then
                CallSuccess(cb, 'Opération réussie !')
            else
                CallError(cb, 'Une erreur est survenue');
        finally
            FreeCallback(cb);
            (*vm)->DetachCurrentThread(vm);
        end;
    end;
end;

// Fonction JNI appelée depuis Java
procedure Java_...startLongOperation(
    env: PJNIEnv;
    this: jobject;
    callback: jobject
); cdecl;
var
    cb: TOperationCallback;
begin
    InitCallback(env, callback, cb);

    // Lancer dans un thread séparé
    BeginThread(@WorkerThread, @cb);
end;
```

### Pattern 4 : Pool de connexions JNI

**Problème** : Création/destruction répétée de connexions coûteuses.

**Solution** : Pool d'objets réutilisables

```pascal
type
    TJNIConnection = class
    private
        FEnv: PJNIEnv;
        FClass: jclass;
        FObject: jobject;
        FMethodCache: TDictionary<string, jmethodID>;
    public
        constructor Create(AEnv: PJNIEnv; const AClassName: string);
        destructor Destroy; override;
        function GetMethod(const AName, ASignature: string): jmethodID;
        function CallMethod(const AName, ASignature: string;
                          const Args: array of const): jobject;
    end;

    TJNIConnectionPool = class
    private
        FPool: TThreadList<TJNIConnection>;
        FMaxSize: Integer;
    public
        constructor Create(AMaxSize: Integer = 10);
        destructor Destroy; override;
        function Acquire(AEnv: PJNIEnv; const AClassName: string): TJNIConnection;
        procedure Release(AConnection: TJNIConnection);
    end;

constructor TJNIConnectionPool.Create(AMaxSize: Integer);
begin
    inherited Create;
    FPool := TThreadList<TJNIConnection>.Create;
    FMaxSize := AMaxSize;
end;

destructor TJNIConnectionPool.Destroy;
var
    LockedList: TList<TJNIConnection>;
    i: Integer;
begin
    LockedList := FPool.LockList;
    try
        for i := LockedList.Count - 1 downto 0 do
            LockedList[i].Free;
        LockedList.Clear;
    finally
        FPool.UnlockList;
    end;
    FPool.Free;
    inherited;
end;

function TJNIConnectionPool.Acquire(AEnv: PJNIEnv;
                                    const AClassName: string): TJNIConnection;
var
    LockedList: TList<TJNIConnection>;
begin
    LockedList := FPool.LockList;
    try
        if LockedList.Count > 0 then
        begin
            Result := LockedList[LockedList.Count - 1];
            LockedList.Delete(LockedList.Count - 1);
        end
        else
            Result := TJNIConnection.Create(AEnv, AClassName);
    finally
        FPool.UnlockList;
    end;
end;

procedure TJNIConnectionPool.Release(AConnection: TJNIConnection);
var
    LockedList: TList<TJNIConnection>;
begin
    LockedList := FPool.LockList;
    try
        if LockedList.Count < FMaxSize then
            LockedList.Add(AConnection)
        else
            AConnection.Free;  // Pool plein, détruire
    finally
        FPool.UnlockList;
    end;
end;

// Utilisation
var
    Pool: TJNIConnectionPool;

procedure InitPool;
begin
    Pool := TJNIConnectionPool.Create(10);
end;

procedure UtiliserPool(env: PJNIEnv);
var
    conn: TJNIConnection;
begin
    conn := Pool.Acquire(env, 'com/example/MyClass');
    try
        conn.CallMethod('myMethod', '()V', []);
    finally
        Pool.Release(conn);
    end;
end;
```

## Cas d'usage avancés

### Accès aux ressources Android

**Lire une ressource string** :

```pascal
function GetStringResource(env: PJNIEnv; activity: jobject;
                          const resName: string): string;
var
    jcls, resCls: jclass;
    getResourcesMethod: jmethodID;
    getIdentifierMethod: jmethodID;
    getStringMethod: jmethodID;
    resources: jobject;
    resId: jint;
    jresName, jresType, jpackageName: jstring;
    jresult: jstring;
    nativeResult: PChar;
begin
    Result := '';

    // Obtenir l'objet Resources
    jcls := (*env)->GetObjectClass(env, activity);
    getResourcesMethod := (*env)->GetMethodID(env, jcls, 'getResources',
                                              '()Landroid/content/res/Resources;');
    resources := (*env)->CallObjectMethod(env, activity, getResourcesMethod);

    // Obtenir l'ID de la ressource
    resCls := (*env)->GetObjectClass(env, resources);
    getIdentifierMethod := (*env)->GetMethodID(env, resCls, 'getIdentifier',
                              '(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)I');

    jresName := (*env)->NewStringUTF(env, PChar(resName));
    jresType := (*env)->NewStringUTF(env, 'string');
    jpackageName := (*env)->NewStringUTF(env, 'com.example.monapp');

    resId := (*env)->CallIntMethod(env, resources, getIdentifierMethod,
                                   jresName, jresType, jpackageName);

    if resId <> 0 then
    begin
        // Obtenir la chaîne
        getStringMethod := (*env)->GetMethodID(env, resCls, 'getString',
                                               '(I)Ljava/lang/String;');
        jresult := (*env)->CallObjectMethod(env, resources,
                                            getStringMethod, resId);

        nativeResult := (*env)->GetStringUTFChars(env, jresult, nil);
        Result := String(nativeResult);
        (*env)->ReleaseStringUTFChars(env, jresult, nativeResult);
        (*env)->DeleteLocalRef(env, jresult);
    end;

    // Nettoyage
    (*env)->DeleteLocalRef(env, jresName);
    (*env)->DeleteLocalRef(env, jresType);
    (*env)->DeleteLocalRef(env, jpackageName);
    (*env)->DeleteLocalRef(env, resources);
    (*env)->DeleteLocalRef(env, resCls);
    (*env)->DeleteLocalRef(env, jcls);
end;
```

### Lancer une autre Activity

```pascal
procedure LaunchActivity(env: PJNIEnv; currentActivity: jobject;
                        const targetActivityClass: string);
var
    jcls, intentCls: jclass;
    intent: jobject;
    intentConstructor: jmethodID;
    startActivityMethod: jmethodID;
    jclassName: jstring;
begin
    // Créer un Intent
    intentCls := (*env)->FindClass(env, 'android/content/Intent');
    intentConstructor := (*env)->GetMethodID(env, intentCls, '<init>',
                         '(Landroid/content/Context;Ljava/lang/Class;)V');

    // Obtenir la classe cible
    jclassName := (*env)->NewStringUTF(env, PChar(targetActivityClass));
    jcls := (*env)->FindClass(env, PChar(targetActivityClass));

    // Créer l'intent
    intent := (*env)->NewObject(env, intentCls, intentConstructor,
                                currentActivity, jcls);

    // Lancer l'Activity
    jcls := (*env)->GetObjectClass(env, currentActivity);
    startActivityMethod := (*env)->GetMethodID(env, jcls, 'startActivity',
                                               '(Landroid/content/Intent;)V');
    (*env)->CallVoidMethod(env, currentActivity, startActivityMethod, intent);

    // Nettoyage
    (*env)->DeleteLocalRef(env, intent);
    (*env)->DeleteLocalRef(env, intentCls);
    (*env)->DeleteLocalRef(env, jcls);
    (*env)->DeleteLocalRef(env, jclassName);
end;
```

### Accéder aux SharedPreferences

```pascal
procedure SavePreference(env: PJNIEnv; activity: jobject;
                        const key, value: string);
var
    jcls, prefCls, editorCls: jclass;
    getPrefsMethod, editMethod, putStringMethod, applyMethod: jmethodID;
    prefs, editor: jobject;
    jkey, jvalue: jstring;
begin
    // Obtenir SharedPreferences
    jcls := (*env)->GetObjectClass(env, activity);
    getPrefsMethod := (*env)->GetMethodID(env, jcls, 'getSharedPreferences',
                      '(Ljava/lang/String;I)Landroid/content/SharedPreferences;');

    jkey := (*env)->NewStringUTF(env, 'MyPrefs');
    prefs := (*env)->CallObjectMethod(env, activity, getPrefsMethod,
                                      jkey, 0); // MODE_PRIVATE = 0
    (*env)->DeleteLocalRef(env, jkey);

    // Obtenir l'éditeur
    prefCls := (*env)->GetObjectClass(env, prefs);
    editMethod := (*env)->GetMethodID(env, prefCls, 'edit',
                  '()Landroid/content/SharedPreferences$Editor;');
    editor := (*env)->CallObjectMethod(env, prefs, editMethod);

    // Écrire la valeur
    editorCls := (*env)->GetObjectClass(env, editor);
    putStringMethod := (*env)->GetMethodID(env, editorCls, 'putString',
                       '(Ljava/lang/String;Ljava/lang/String;)' +
                       'Landroid/content/SharedPreferences$Editor;');

    jkey := (*env)->NewStringUTF(env, PChar(key));
    jvalue := (*env)->NewStringUTF(env, PChar(value));
    (*env)->CallObjectMethod(env, editor, putStringMethod, jkey, jvalue);

    // Appliquer les changements
    applyMethod := (*env)->GetMethodID(env, editorCls, 'apply', '()V');
    (*env)->CallVoidMethod(env, editor, applyMethod);

    // Nettoyage
    (*env)->DeleteLocalRef(env, jkey);
    (*env)->DeleteLocalRef(env, jvalue);
    (*env)->DeleteLocalRef(env, editor);
    (*env)->DeleteLocalRef(env, editorCls);
    (*env)->DeleteLocalRef(env, prefs);
    (*env)->DeleteLocalRef(env, prefCls);
    (*env)->DeleteLocalRef(env, jcls);
end;

function LoadPreference(env: PJNIEnv; activity: jobject;
                       const key: string): string;
var
    jcls, prefCls: jclass;
    getPrefsMethod, getStringMethod: jmethodID;
    prefs: jobject;
    jkey, jdefault, jresult: jstring;
    nativeResult: PChar;
begin
    Result := '';

    // Obtenir SharedPreferences
    jcls := (*env)->GetObjectClass(env, activity);
    getPrefsMethod := (*env)->GetMethodID(env, jcls, 'getSharedPreferences',
                      '(Ljava/lang/String;I)Landroid/content/SharedPreferences;');

    jkey := (*env)->NewStringUTF(env, 'MyPrefs');
    prefs := (*env)->CallObjectMethod(env, activity, getPrefsMethod,
                                      jkey, 0);
    (*env)->DeleteLocalRef(env, jkey);

    // Lire la valeur
    prefCls := (*env)->GetObjectClass(env, prefs);
    getStringMethod := (*env)->GetMethodID(env, prefCls, 'getString',
                       '(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;');

    jkey := (*env)->NewStringUTF(env, PChar(key));
    jdefault := (*env)->NewStringUTF(env, '');
    jresult := (*env)->CallObjectMethod(env, prefs, getStringMethod,
                                        jkey, jdefault);

    if Assigned(jresult) then
    begin
        nativeResult := (*env)->GetStringUTFChars(env, jresult, nil);
        Result := String(nativeResult);
        (*env)->ReleaseStringUTFChars(env, jresult, nativeResult);
        (*env)->DeleteLocalRef(env, jresult);
    end;

    // Nettoyage
    (*env)->DeleteLocalRef(env, jkey);
    (*env)->DeleteLocalRef(env, jdefault);
    (*env)->DeleteLocalRef(env, prefs);
    (*env)->DeleteLocalRef(env, prefCls);
    (*env)->DeleteLocalRef(env, jcls);
end;
```

## Performance et profiling

### Mesurer les appels JNI

> **Note** : Les exemples suivants utilisent `{$mode delphi}` avec `uses Generics.Collections` pour les types génériques (`TDictionary<>`, `TList<>`, `TThreadList<>`). Le type `TProc` n'existe pas en FPC et doit être défini : `type TProc = reference to procedure;` (nécessite FPC 3.3.1+).

```pascal
type
    TProc = reference to procedure;  // Équivalent du TProc Delphi

function MesureTempsJNI(env: PJNIEnv; const operation: string;
                       callback: TProc): Int64;
var
    startTime, endTime: Int64;
begin
    startTime := GetTickCount64;
    try
        callback();
    finally
        endTime := GetTickCount64;
        Result := endTime - startTime;
        LogJNI('PERF', Format('%s: %d ms', [operation, Result]));
    end;
end;

// Utilisation
procedure ExemplePerformance(env: PJNIEnv);
var
    temps: Int64;
begin
    temps := MesureTempsJNI(env, 'Appel méthode Java',
        procedure
        begin
            (*env)->CallVoidMethod(env, obj, methodID);
        end);

    if temps > 100 then
        LogJNI('WARNING', 'Appel JNI trop lent !');
end;
```

### Règles de performance JNI

**✓ À FAIRE** :

1. **Minimiser les appels JNI**
   - Regrouper les opérations
   - Transférer des tableaux plutôt que des valeurs individuelles

2. **Mettre en cache les IDs**
   - Classes, méthodes, champs
   - Initialiser une seule fois

3. **Utiliser des références globales judicieusement**
   - Pour les objets conservés longtemps
   - Toujours les libérer quand terminé

4. **Gérer la mémoire proprement**
   - Try-finally pour garantir la libération
   - DeleteLocalRef pour les boucles

5. **Traiter en natif autant que possible**
   - Algorithmes complexes en Pascal
   - Retourner seulement le résultat à Java

**✗ À ÉVITER** :

1. **Appels JNI dans les boucles serrées**
   ```pascal
   // MAUVAIS
   for i := 1 to 1000 do
       (*env)->CallVoidMethod(env, obj, methodID, i);

   // BON
   jarray := CreateIntArray(env, data);
   (*env)->CallVoidMethod(env, obj, methodID, jarray);
   ```

2. **Conversions répétées**
   ```pascal
   // MAUVAIS
   for i := 1 to 100 do
   begin
       jstr := ConvertToJString(env, myString);
       // utilisation
       (*env)->DeleteLocalRef(env, jstr);
   end;

   // BON
   jstr := ConvertToJString(env, myString);
   for i := 1 to 100 do
   begin
       // utilisation
   end;
   (*env)->DeleteLocalRef(env, jstr);
   ```

3. **Oublier de libérer les ressources**
   ```pascal
   // MAUVAIS - Fuite mémoire
   procedure FuiteMemoire(env: PJNIEnv);
   var
       jstr: jstring;
   begin
       jstr := (*env)->NewStringUTF(env, 'Test');
       (*env)->CallVoidMethod(env, obj, methodID, jstr);
       // jstr n'est jamais libéré !
   end;

   // BON - Libération garantie
   procedure PropreMemoire(env: PJNIEnv);
   var
       jstr: jstring;
   begin
       jstr := (*env)->NewStringUTF(env, 'Test');
       try
           (*env)->CallVoidMethod(env, obj, methodID, jstr);
       finally
           (*env)->DeleteLocalRef(env, jstr);
       end;
   end;
   ```

4. **Accès direct aux tableaux sans libération**
   ```pascal
   // MAUVAIS
   procedure MauvaiseGestionTableau(env: PJNIEnv; arr: jintArray);
   var
       elements: PJint;
   begin
       elements := (*env)->GetIntArrayElements(env, arr, nil);
       // Utilisation...
       // Jamais libéré !
   end;

   // BON
   procedure BonneGestionTableau(env: PJNIEnv; arr: jintArray);
   var
       elements: PJint;
   begin
       elements := (*env)->GetIntArrayElements(env, arr, nil);
       try
           // Utilisation...
       finally
           (*env)->ReleaseIntArrayElements(env, arr, elements, 0);
       end;
   end;
   ```

5. **Utiliser FindClass dans des threads attachés**
   ```pascal
   // PROBLÉMATIQUE - Peut échouer
   procedure ThreadWorkerMauvais;
   var
       env: PJNIEnv;
       jcls: jclass;
   begin
       AttachThreadToJVM(env);
       jcls := (*env)->FindClass(env, 'com/example/MyClass');
       // Peut retourner nil car le classloader n'est pas accessible
   end;

   // SOLUTION - Mettre en cache la classe au démarrage
   var
       GlobalClassRef: jclass;

   procedure InitGlobalClass(env: PJNIEnv);
   var
       localClass: jclass;
   begin
       localClass := (*env)->FindClass(env, 'com/example/MyClass');
       GlobalClassRef := (*env)->NewGlobalRef(env, localClass);
       (*env)->DeleteLocalRef(env, localClass);
   end;

   procedure ThreadWorkerBon;
   var
       env: PJNIEnv;
   begin
       AttachThreadToJVM(env);
       // Utiliser GlobalClassRef au lieu de FindClass
   end;
   ```

### Profiling des appels JNI

#### Avec Android Studio Profiler

**Étapes** :
1. Compiler en mode Debug avec symboles
2. Lancer l'app depuis Android Studio
3. Ouvrir le Profiler (View → Tool Windows → Profiler)
4. Sélectionner CPU Profiler
5. Enregistrer une trace

**Ce que vous verrez** :
- Temps passé dans chaque fonction JNI
- Nombre d'appels
- Call stack complet
- Comparaison Java vs Native

#### Mesure manuelle avec timestamps

```pascal
type
    TPerformanceMonitor = class
    private
        FMeasurements: TDictionary<string, TList<Int64>>;
    public
        constructor Create;
        destructor Destroy; override;
        procedure StartMeasure(const AName: string);
        procedure StopMeasure(const AName: string);
        procedure PrintStats;
    end;

var
    PerfMonitor: TPerformanceMonitor;

constructor TPerformanceMonitor.Create;
begin
    inherited;
    FMeasurements := TDictionary<string, TList<Int64>>.Create;
end;

procedure TPerformanceMonitor.StartMeasure(const AName: string);
begin
    if not FMeasurements.ContainsKey(AName) then
        FMeasurements.Add(AName, TList<Int64>.Create);

    FMeasurements[AName].Add(-GetTickCount64); // Négatif = début
end;

procedure TPerformanceMonitor.StopMeasure(const AName: string);
var
    list: TList<Int64>;
    lastIndex: Integer;
begin
    if FMeasurements.TryGetValue(AName, list) then
    begin
        lastIndex := list.Count - 1;
        if (lastIndex >= 0) and (list[lastIndex] < 0) then
            list[lastIndex] := GetTickCount64 + list[lastIndex];
    end;
end;

procedure TPerformanceMonitor.PrintStats;
var
    pair: TPair<string, TList<Int64>>;
    time: Int64;
    total, count, avg, min, max: Int64;
begin
    WriteLn('=== Performance Statistics ===');
    for pair in FMeasurements do
    begin
        total := 0;
        count := 0;
        min := MaxInt;
        max := 0;

        for time in pair.Value do
        begin
            if time > 0 then
            begin
                Inc(total, time);
                Inc(count);
                if time < min then min := time;
                if time > max then max := time;
            end;
        end;

        if count > 0 then
        begin
            avg := total div count;
            WriteLn(Format('%s: calls=%d, avg=%dms, min=%dms, max=%dms, total=%dms',
                          [pair.Key, count, avg, min, max, total]));
        end;
    end;
end;

// Utilisation
procedure MonFonctionJNI(env: PJNIEnv);
begin
    PerfMonitor.StartMeasure('MonFonctionJNI');
    try
        // Code à mesurer
        (*env)->CallVoidMethod(env, obj, methodID);
    finally
        PerfMonitor.StopMeasure('MonFonctionJNI');
    end;
end;

// À l'arrêt de l'application
procedure OnApplicationExit;
begin
    PerfMonitor.PrintStats;
    PerfMonitor.Free;
end;
```

## Sécurité et JNI

### Validation des entrées

**Toujours valider les paramètres** venant de Java :

```pascal
function SecureStringOperation(
    env: PJNIEnv;
    this: jobject;
    input: jstring
): jstring; cdecl;
var
    nativeStr: PChar;
    len: jsize;
    processedStr: string;
begin
    Result := nil;

    // 1. Vérifier que la chaîne n'est pas nulle
    if not Assigned(input) then
    begin
        LogJNI('ERROR', 'Input string is null');
        Exit;
    end;

    // 2. Vérifier la longueur
    len := (*env)->GetStringLength(env, input);
    if (len <= 0) or (len > 10000) then  // Limite de sécurité
    begin
        LogJNI('ERROR', 'String length invalid: ' + IntToStr(len));
        Exit;
    end;

    // 3. Convertir et valider
    nativeStr := (*env)->GetStringUTFChars(env, input, nil);
    if not Assigned(nativeStr) then
    begin
        LogJNI('ERROR', 'Failed to convert string');
        Exit;
    end;

    try
        // 4. Validation supplémentaire (caractères, format, etc.)
        processedStr := String(nativeStr);
        if not IsValidFormat(processedStr) then
        begin
            LogJNI('ERROR', 'String format invalid');
            Exit;
        end;

        // 5. Traitement sécurisé
        processedStr := ProcessSecurely(processedStr);

        // 6. Retour sécurisé
        Result := (*env)->NewStringUTF(env, PChar(processedStr));
    finally
        (*env)->ReleaseStringUTFChars(env, input, nativeStr);
    end;
end;
```

### Protection contre les buffer overflows

```pascal
function SafeArrayCopy(
    env: PJNIEnv;
    this: jobject;
    sourceArray: jbyteArray;
    destSize: jint
): jbyteArray; cdecl;
var
    sourceLen: jsize;
    elements: PJbyte;
    destArray: jbyteArray;
    copySize: jsize;
begin
    Result := nil;

    // Vérifier les paramètres
    if not Assigned(sourceArray) then Exit;
    if destSize <= 0 then Exit;

    // Obtenir la taille source
    sourceLen := (*env)->GetArrayLength(env, sourceArray);
    if sourceLen <= 0 then Exit;

    // Calculer la taille de copie sûre
    copySize := sourceLen;
    if copySize > destSize then
        copySize := destSize;  // Limiter pour éviter l'overflow

    // Créer le tableau destination
    destArray := (*env)->NewByteArray(env, copySize);
    if not Assigned(destArray) then Exit;

    // Copier de manière sécurisée
    elements := (*env)->GetByteArrayElements(env, sourceArray, nil);
    if Assigned(elements) then
    begin
        try
            (*env)->SetByteArrayRegion(env, destArray, 0, copySize, elements);
            Result := destArray;
        finally
            (*env)->ReleaseByteArrayElements(env, sourceArray, elements,
                                            JNI_ABORT); // Pas de copie retour
        end;
    end;
end;
```

### Gestion sécurisée des exceptions

```pascal
procedure SafeJNICall(env: PJNIEnv; operation: TProc);
var
    exception: jthrowable;
    jcls: jclass;
    method: jmethodID;
    message: jstring;
    nativeMessage: PChar;
begin
    try
        operation();

        // Vérifier si une exception Java s'est produite
        if (*env)->ExceptionCheck(env) = JNI_TRUE then
        begin
            exception := (*env)->ExceptionOccurred(env);

            // Logger l'exception
            (*env)->ExceptionDescribe(env);

            // Obtenir le message d'exception
            jcls := (*env)->GetObjectClass(env, exception);
            method := (*env)->GetMethodID(env, jcls, 'getMessage',
                                          '()Ljava/lang/String;');
            message := (*env)->CallObjectMethod(env, exception, method);

            if Assigned(message) then
            begin
                nativeMessage := (*env)->GetStringUTFChars(env, message, nil);
                LogJNI('EXCEPTION', 'Java exception: ' + String(nativeMessage));
                (*env)->ReleaseStringUTFChars(env, message, nativeMessage);
                (*env)->DeleteLocalRef(env, message);
            end;

            // Nettoyer
            (*env)->DeleteLocalRef(env, jcls);
            (*env)->ExceptionClear(env);
        end;
    except
        on E: Exception do
        begin
            LogJNI('ERROR', 'Pascal exception: ' + E.Message);
            // Ne pas propager vers Java
        end;
    end;
end;

// Utilisation
procedure MyJNIFunction(env: PJNIEnv; this: jobject); cdecl;
begin
    SafeJNICall(env,
        procedure
        begin
            // Code potentiellement dangereux
            (*env)->CallVoidMethod(env, obj, methodID);
        end);
end;
```

## Debugging avancé JNI

### Techniques de débogage

#### 1. Logs détaillés

```pascal
procedure LogJNICallWithParams(env: PJNIEnv; const funcName: string;
                               const params: array of const);
var
    i: Integer;
    paramStr: string;
begin
    paramStr := funcName + '(';
    for i := Low(params) to High(params) do
    begin
        case params[i].VType of
            vtInteger: paramStr := paramStr + IntToStr(params[i].VInteger);
            vtString: paramStr := paramStr + string(params[i].VString^);
            vtPChar: paramStr := paramStr + string(params[i].VPChar);
            vtObject: paramStr := paramStr + params[i].VObject.ClassName;
        else
            paramStr := paramStr + '?';
        end;

        if i < High(params) then
            paramStr := paramStr + ', ';
    end;
    paramStr := paramStr + ')';

    LogJNI('CALL', paramStr);
end;

// Utilisation
procedure MyFunction(env: PJNIEnv; value: jint; name: jstring);
begin
    LogJNICallWithParams(env, 'MyFunction', [value, 'name_param']);
    // ... reste du code
end;
```

#### 2. Assertions JNI

```pascal
procedure AssertJNI(condition: Boolean; const message: string);
begin
    if not condition then
    begin
        LogJNI('ASSERT', 'Assertion failed: ' + message);
        {$IFDEF DEBUG}
        raise Exception.Create('JNI Assertion: ' + message);
        {$ENDIF}
    end;
end;

// Utilisation
procedure ProcessObject(env: PJNIEnv; obj: jobject);
begin
    AssertJNI(Assigned(env), 'env must not be nil');
    AssertJNI(Assigned(obj), 'obj must not be nil');

    // ... traitement
end;
```

#### 3. Dump de l'état JNI

```pascal
procedure DumpJNIState(env: PJNIEnv; obj: jobject);
var
    jcls: jclass;
    method: jmethodID;
    className: jstring;
    nativeClassName: PChar;
begin
    LogJNI('DEBUG', '=== JNI State Dump ===');

    if not Assigned(env) then
    begin
        LogJNI('DEBUG', 'env: nil');
        Exit;
    end;

    LogJNI('DEBUG', 'env: assigned');

    if not Assigned(obj) then
    begin
        LogJNI('DEBUG', 'obj: nil');
        Exit;
    end;

    LogJNI('DEBUG', 'obj: assigned');

    // Obtenir le nom de la classe
    jcls := (*env)->GetObjectClass(env, obj);
    if Assigned(jcls) then
    begin
        // Obtenir Class.getName()
        method := (*env)->GetMethodID(env, jcls, 'getName',
                                      '()Ljava/lang/String;');
        if Assigned(method) then
        begin
            className := (*env)->CallObjectMethod(env, jcls, method);
            if Assigned(className) then
            begin
                nativeClassName := (*env)->GetStringUTFChars(env, className, nil);
                LogJNI('DEBUG', 'Class: ' + String(nativeClassName));
                (*env)->ReleaseStringUTFChars(env, className, nativeClassName);
                (*env)->DeleteLocalRef(env, className);
            end;
        end;
        (*env)->DeleteLocalRef(env, jcls);
    end;

    LogJNI('DEBUG', '======================');
end;
```

### Points d'arrêt natifs

Pour déboguer avec GDB ou LLDB :

**1. Compiler avec symboles de débogage**
```pascal
// Dans les options du projet
{$IFDEF DEBUG}
  {$DEBUGINFO ON}
{$ENDIF}
```

**2. Attacher le débogueur via ADB**
```bash
# Obtenir le PID de l'app
adb shell ps | grep com.example.monapp

# Attacher gdbserver
adb shell
gdbserver :5039 --attach <PID>

# Dans un autre terminal
adb forward tcp:5039 tcp:5039
gdb
(gdb) target remote :5039
(gdb) break Java_com_example_monapp_MainActivity_myNativeFunction
(gdb) continue
```

**3. Utiliser Android Studio**
- Debug → Attach Debugger to Android Process
- Sélectionner "Show all processes"
- Cocher "Debug type: Dual (Java + Native)"
- Placer des breakpoints dans le code Pascal

## Migration et maintenance

### Compatibilité entre versions Android

```pascal
function GetAndroidVersion(env: PJNIEnv): Integer;
var
    versionCls: jclass;
    sdkIntField: jfieldID;
begin
    Result := 0;
    versionCls := (*env)->FindClass(env, 'android/os/Build$VERSION');
    if Assigned(versionCls) then
    begin
        sdkIntField := (*env)->GetStaticFieldID(env, versionCls,
                                                'SDK_INT', 'I');
        Result := (*env)->GetStaticIntField(env, versionCls, sdkIntField);
        (*env)->DeleteLocalRef(env, versionCls);
    end;
end;

// Utilisation pour code conditionnel
procedure VersionSpecificCode(env: PJNIEnv);
var
    apiLevel: Integer;
begin
    apiLevel := GetAndroidVersion(env);

    if apiLevel >= 23 then  // Android 6.0+
    begin
        // Utiliser les nouvelles API de permissions runtime
        RequestRuntimePermission(env);
    end
    else
    begin
        // Méthode legacy
        CheckManifestPermission(env);
    end;

    if apiLevel >= 26 then  // Android 8.0+
    begin
        // Utiliser les canaux de notification
        CreateNotificationChannel(env);
    end;
end;
```

### Évolution de l'architecture JNI

**Préparer votre code pour l'avenir** :

1. **Isoler les appels JNI**
```pascal
// Interface abstraite
type
    IJNIBridge = interface
        function CallJavaMethod(const methodName: string;
                               const args: array of const): Variant;
        procedure RegisterCallback(const name: string; callback: TProc);
    end;

// Implémentation concrète
type
    TJNIBridgeImpl = class(TInterfacedObject, IJNIBridge)
    private
        FEnv: PJNIEnv;
        // ... implémentation
    end;

// Utilisation
var
    Bridge: IJNIBridge;

procedure InitBridge(env: PJNIEnv);
begin
    Bridge := TJNIBridgeImpl.Create(env);
end;

procedure BusinessLogic;
begin
    // Code métier indépendant de JNI
    Bridge.CallJavaMethod('showToast', ['Message']);
end;
```

2. **Versioning des interfaces**
```pascal
type
    TJNIInterfaceVersion = (
        jniV1_Legacy,
        jniV2_Marshmallow,    // Android 6.0+
        jniV3_Oreo,           // Android 8.0+
        jniV4_Q               // Android 10+
    );

function DetermineJNIVersion(apiLevel: Integer): TJNIInterfaceVersion;
begin
    if apiLevel >= 29 then
        Result := jniV4_Q
    else if apiLevel >= 26 then
        Result := jniV3_Oreo
    else if apiLevel >= 23 then
        Result := jniV2_Marshmallow
    else
        Result := jniV1_Legacy;
end;

// Adapter le comportement selon la version
procedure AdaptiveJNICall(env: PJNIEnv; version: TJNIInterfaceVersion);
begin
    case version of
        jniV4_Q: UseModernAPI(env);
        jniV3_Oreo: UseOreoAPI(env);
        jniV2_Marshmallow: UseMarshmallowAPI(env);
        jniV1_Legacy: UseLegacyAPI(env);
    end;
end;
```

### Documentation du code JNI

**Documenter vos fonctions JNI** :

```pascal
/// <summary>
/// Convertit une chaîne Java en chaîne Pascal
/// </summary>
/// <param name="env">Environnement JNI (ne doit pas être nil)</param>
/// <param name="jstr">Chaîne Java à convertir (peut être nil)</param>
/// <returns>Chaîne Pascal, vide si jstr est nil</returns>
/// <remarks>
/// La chaîne Java est automatiquement libérée après conversion.
/// Thread-safe si env correspond au thread courant.
/// </remarks>
/// <example>
/// <code>
/// var
///   result: string;
/// begin
///   result := JStringToPascal(env, myJavaString);
/// end;
/// </code>
/// </example>
function JStringToPascal(env: PJNIEnv; jstr: jstring): string;
var
    nativeStr: PChar;
begin
    Result := '';

    if not Assigned(env) then
    begin
        LogJNI('ERROR', 'JStringToPascal: env is nil');
        Exit;
    end;

    if not Assigned(jstr) then
        Exit;  // Retourne chaîne vide

    nativeStr := (*env)->GetStringUTFChars(env, jstr, nil);
    if Assigned(nativeStr) then
    begin
        try
            Result := String(nativeStr);
        finally
            (*env)->ReleaseStringUTFChars(env, jstr, nativeStr);
        end;
    end;
end;
```

## Ressources et références

### Documentation officielle

**JNI Specification**
- Oracle JNI Documentation
- Android NDK Documentation
- JNI Tips (developer.android.com)

**Outils Android**
- Android Studio
- SDK Manager
- NDK
- ADB (Android Debug Bridge)

### Erreurs courantes et solutions

| Erreur | Cause | Solution |
|--------|-------|----------|
| UnsatisfiedLinkError | Bibliothèque .so non trouvée | Vérifier les chemins libs/, architecture correcte |
| NoSuchMethodError | Signature incorrecte | Vérifier avec javap, corriger la signature |
| JNI WARNING: null | Référence null | Valider les paramètres avant utilisation |
| Local reference overflow | Trop de refs locales | DeleteLocalRef ou PushLocalFrame |
| Invalid jmethodID | ID invalide/périmé | Mettre en cache au démarrage |
| SIGSEGV crash | Accès mémoire invalide | Vérifier les pointeurs, utiliser CheckJNI |

### Checklist de validation JNI

**Avant chaque release** :

- [ ] Toutes les références sont libérées
- [ ] Les IDs sont mis en cache
- [ ] Les exceptions Java sont gérées
- [ ] Les paramètres sont validés
- [ ] Le code est thread-safe
- [ ] Pas d'appels JNI dans les boucles serrées
- [ ] La mémoire est gérée proprement
- [ ] Les logs de debug sont désactivés en production
- [ ] Tests sur plusieurs versions Android
- [ ] Tests sur plusieurs architectures (ARM32/ARM64)
- [ ] Profiling effectué
- [ ] Documentation à jour

## Conclusion

L'architecture Android avec JNI offre une puissance considérable pour les développeurs FreePascal. En comprenant les mécanismes sous-jacents, vous pouvez :

**Avantages maîtrisés** :
- ✓ Créer des applications Android natives performantes
- ✓ Réutiliser votre code Pascal existant
- ✓ Accéder à toutes les API Android
- ✓ Obtenir des performances comparables à Java/Kotlin
- ✓ Maintenir une base de code unifiée

**Points clés à retenir** :

1. **JNI est un pont** entre Java et Pascal, pas une barrière
2. **La gestion mémoire** est votre responsabilité (references, strings, arrays)
3. **La performance** dépend de la minimisation des appels JNI
4. **La sécurité** nécessite une validation stricte des entrées
5. **LAMW simplifie** énormément le travail mais comprendre JNI reste essentiel

**Bonnes pratiques essentielles** :
- Toujours utiliser try-finally pour libérer les ressources
- Mettre en cache les IDs de classes/méthodes/champs
- Valider tous les paramètres venant de Java
- Logger généreusement en développement
- Profiler régulièrement
- Tester sur vrais appareils

**Prochaines étapes** :
- Approfondir les composants LAMW spécifiques
- Explorer les interfaces natives Android
- Maîtriser le cycle de vie Android
- Optimiser pour différentes configurations
- Créer des applications production-ready

Avec une solide compréhension de JNI et de l'architecture Android, vous êtes maintenant équipé pour développer des applications mobiles sophistiquées et performantes en FreePascal !

---

**Ressources complémentaires** :

📚 **Lecture recommandée** :
- JNI Programmer's Guide and Specification
- Android Developer Documentation - NDK
- Forum LAMW - Exemples et discussions

🎓 **Pour aller plus loin** :
- 13.3 Interfaces natives Android
- 13.4 Custom Drawn pour interfaces mobiles
- 20. Optimisation et Performance

💡 **Astuce finale** : Commencez simple, testez fréquemment, et construisez progressivement vos connaissances JNI. Chaque application que vous créez renforcera votre maîtrise de cette technologie puissante !

⏭️ [Interfaces natives Android](/13-developpement-mobile-embarque/03-interfaces-natives-android.md)
