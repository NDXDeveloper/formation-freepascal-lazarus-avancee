🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.6 Java Native Interface (JNI)

## Introduction

**JNI (Java Native Interface)** est une interface de programmation qui permet au code Java de communiquer avec du code natif écrit dans d'autres langages comme C, C++ ou FreePascal. C'est le pont entre le monde Java (machine virtuelle) et le code natif compilé.

### Qu'est-ce que JNI ?

JNI est une **API standardisée** fournie par Java pour :
- Appeler des fonctions natives depuis Java
- Appeler des méthodes Java depuis du code natif
- Manipuler des objets Java depuis du code natif
- Gérer les exceptions entre Java et natif

**Analogie** : Imaginez Java comme une île où tout le monde parle "Java". JNI est le système de traduction simultanée qui permet aux visiteurs étrangers (code natif) de communiquer avec les habitants de l'île, et vice-versa.

```
┌───────────────────────────────────────┐
│        Machine Virtuelle Java (JVM)   │
│                                       │
│  ┌──────────────┐    ┌──────────────┐ │
│  │ Code Java    │    │ Code Java    │ │
│  │ (bytecode)   │    │ (bytecode)   │ │
│  └──────┬───────┘    └──────┬───────┘ │
│         │                   │         │
│         └───────┬───────────┘         │
│                 │                     │
│         ┌───────▼────────┐            │
│         │   JNI Layer    │            │
│         └───────┬────────┘            │
└─────────────────┼─────────────────────┘
                  │
         ┌────────▼────────┐
         │  Code Natif     │
         │  (FreePascal)   │
         │  (.dll / .so)   │
         └─────────────────┘
```

### Pourquoi utiliser JNI avec FreePascal ?

#### 1. Performance

Java est rapide, mais certaines opérations sont plus efficaces en code natif :
- Calculs intensifs
- Traitement d'images
- Algorithmes complexes
- Accès matériel direct

```pascal
// Code Pascal natif ultra-rapide
function CalculIntensif(n: Int64): Int64; cdecl;  
var
  i: Int64;
begin
  Result := 0;
  for i := 1 to n do
    Result := Result + i * i;
end;
```

```java
// Appelé depuis Java
long result = NativeLib.calculIntensif(1000000000L);
```

#### 2. Réutilisation de code existant

Vous avez déjà du code Pascal performant ? Utilisez-le depuis Java !

```pascal
// Bibliothèque Pascal existante pour le traitement d'images
procedure TraiterImage(pixels: PByte; width, height: Integer);
```

#### 3. Accès aux API système

Certaines fonctionnalités système ne sont pas disponibles en Java pur :
- Contrôle matériel bas niveau
- API Windows/Linux spécifiques
- Pilotes matériels

#### 4. Intégration avec l'écosystème Java

Java est omniprésent dans :
- Applications d'entreprise (Spring, Jakarta EE)
- Android (bien que limité pour JNI)
- Big Data (Hadoop, Spark)
- Serveurs d'applications

### Architecture JNI

#### Composants principaux

**1. La JVM (Java Virtual Machine)**
- Héberge le code Java
- Gère la mémoire Java (Garbage Collection)
- Fournit l'environnement d'exécution

**2. Le JNIEnv (Environnement JNI)**
- Interface pour manipuler Java depuis le natif
- Pointeur de fonction vers l'API JNI
- Différent pour chaque thread

**3. La bibliothèque native (.dll/.so)**
- Code compilé FreePascal
- Fonctions exportées avec convention d'appel spécifique
- Chargée dynamiquement par Java

#### Cycle de vie d'un appel JNI

```
┌─────────────────────────────────────────────────────────┐
│ 1. Application Java démarre                             │
│    System.loadLibrary("monlib");                        │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│ 2. JVM charge la bibliothèque native                    │
│    monlib.dll (Windows) ou libmonlib.so (Linux)         │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│ 3. Java appelle une méthode native                      │
│    int result = calculer(42);                           │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│ 4. JNI trouve la fonction native correspondante         │
│    Java_com_example_NativeLib_calculer                  │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│ 5. Fonction native exécutée (FreePascal)                │
│    Result := a * 2;                                     │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│ 6. Résultat retourné à Java                             │
│    return result;                                       │
└─────────────────────────────────────────────────────────┘
```

### Conventions de nommage JNI

JNI utilise une convention stricte pour mapper les méthodes Java aux fonctions natives :

**Format** :
```
Java_<package_name>_<class_name>_<method_name>
```

**Exemple** :
```java
package com.example;  
class MathLib {
    public native int add(int a, int b);
}
```

**Nom de la fonction native** :
```
Java_com_example_MathLib_add
```

**Règles de transformation** :
- `.` (point) → `_` (underscore)
- `_` (underscore) → `_1`
- `;` (point-virgule) → `_2`
- `[` (crochet) → `_3`

## Installation et prérequis

### Installer le JDK (Java Development Kit)

#### Windows

```batch
REM Télécharger depuis Oracle ou utiliser Chocolatey  
choco install openjdk

REM Vérifier l'installation  
java -version  
javac -version
```

#### Linux (Ubuntu/Debian)

```bash
# Installer OpenJDK
sudo apt update  
sudo apt install default-jdk

# Vérifier l'installation
java -version  
javac -version
```

### Localiser les fichiers JNI

Les fichiers d'en-tête JNI sont nécessaires pour compiler le code natif.

**Windows** :
```
C:\Program Files\Java\jdk-17\include\
├── jni.h
└── win32\
    └── jni_md.h
```

**Linux** :
```
/usr/lib/jvm/java-17-openjdk-amd64/include/
├── jni.h
└── linux/
    └── jni_md.h
```

### Variables d'environnement

```bash
# Windows
set JAVA_HOME=C:\Program Files\Java\jdk-17  
set PATH=%JAVA_HOME%\bin;%PATH%

# Linux
export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64  
export PATH=$JAVA_HOME/bin:$PATH
```

## Premier exemple : Hello JNI

### Étape 1 : Créer la classe Java

**HelloJNI.java** :
```java
public class HelloJNI {
    // Déclaration de la méthode native
    public native void sayHello();

    // Chargement de la bibliothèque native
    static {
        System.loadLibrary("hellojni");
    }

    // Méthode main pour tester
    public static void main(String[] args) {
        new HelloJNI().sayHello();
    }
}
```

### Étape 2 : Compiler et générer l'en-tête

```bash
# Compiler la classe Java
javac HelloJNI.java

# Générer l'en-tête C (JDK 8+)
javac -h . HelloJNI.java

# Ou avec javah (JDK < 10)
javah -jni HelloJNI
```

Cela génère **HelloJNI.h** :
```c
/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class HelloJNI */

#ifndef _Included_HelloJNI
#define _Included_HelloJNI
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Class:     HelloJNI
 * Method:    sayHello
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_HelloJNI_sayHello
  (JNIEnv *, jobject);

#ifdef __cplusplus
}
#endif
#endif
```

### Étape 3 : Implémenter en FreePascal

**hellojni.pas** :
```pascal
library hellojni;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  // Types JNI de base
  jint = LongInt;
  jboolean = Byte;

  // Pointeurs opaques
  JNIEnv = Pointer;
  jobject = Pointer;

// Implémentation de la méthode native
procedure Java_HelloJNI_sayHello(env: JNIEnv; obj: jobject); cdecl;  
begin
  WriteLn('Hello from FreePascal via JNI!');
end;

exports
  Java_HelloJNI_sayHello;

begin  
end.
```

### Étape 4 : Compiler la bibliothèque native

**Windows** :
```batch
fpc -ohellojni.dll hellojni.pas
```

**Linux** :
```bash
fpc -olibhellojni.so hellojni.pas
```

### Étape 5 : Exécuter

```bash
# S'assurer que la bibliothèque est accessible
# Windows: copier hellojni.dll dans le même répertoire
# Linux: définir LD_LIBRARY_PATH=. ou copier dans /usr/lib

java HelloJNI
```

**Sortie** :
```
Hello from FreePascal via JNI!
```

## Types de données JNI

### Types primitifs

| Type Java | Type JNI | Type Pascal | Taille |
|-----------|----------|-------------|--------|
| boolean | jboolean | Byte | 1 octet |
| byte | jbyte | ShortInt | 1 octet |
| char | jchar | Word | 2 octets |
| short | jshort | SmallInt | 2 octets |
| int | jint | LongInt | 4 octets |
| long | jlong | Int64 | 8 octets |
| float | jfloat | Single | 4 octets |
| double | jdouble | Double | 8 octets |
| void | void | - | - |

### Types référence

| Type Java | Type JNI | Description |
|-----------|----------|-------------|
| Object | jobject | Objet Java générique |
| Class | jclass | Classe Java |
| String | jstring | Chaîne Java |
| Array | jarray | Tableau Java générique |
| Object[] | jobjectArray | Tableau d'objets |
| boolean[] | jbooleanArray | Tableau de booléens |
| int[] | jintArray | Tableau d'entiers |
| ... | ... | Autres tableaux typés |
| Throwable | jthrowable | Exception Java |

### Définitions Pascal pour JNI

```pascal
unit JNITypes;

{$mode objfpc}{$H+}

interface

type
  // Types primitifs
  jboolean = Byte;
  jbyte = ShortInt;
  jchar = Word;
  jshort = SmallInt;
  jint = LongInt;
  jlong = Int64;
  jfloat = Single;
  jdouble = Double;
  jsize = jint;

  // Types référence (pointeurs opaques)
  jobject = Pointer;
  jclass = jobject;
  jstring = jobject;
  jarray = jobject;
  jobjectArray = jarray;
  jbooleanArray = jarray;
  jbyteArray = jarray;
  jcharArray = jarray;
  jshortArray = jarray;
  jintArray = jarray;
  jlongArray = jarray;
  jfloatArray = jarray;
  jdoubleArray = jarray;
  jthrowable = jobject;

  // Pointeurs
  JNIEnv = Pointer;
  JavaVM = Pointer;

implementation

end.
```

## Passage de paramètres

### Paramètres primitifs

**Java** :
```java
public class Calculator {
    public native int add(int a, int b);
    public native double multiply(double x, double y);
}
```

**Pascal** :
```pascal
function Java_Calculator_add(env: JNIEnv; obj: jobject;
  a, b: jint): jint; cdecl;
begin
  Result := a + b;
end;

function Java_Calculator_multiply(env: JNIEnv; obj: jobject;
  x, y: jdouble): jdouble; cdecl;
begin
  Result := x * y;
end;
```

### Chaînes de caractères

Les chaînes Java ne sont pas des char* C simples. Il faut les convertir.

**Java** :
```java
public class StringTest {
    public native void printString(String text);
    public native String reverseString(String text);
}
```

**Pascal** :
```pascal
uses
  JNITypes;

type
  // Pointeur vers la table de fonctions JNI
  PJNIEnv = ^JNINativeInterface;

  JNINativeInterface = record
    // ... autres fonctions
    GetStringUTFChars: function(env: PJNIEnv; str: jstring;
      isCopy: Pjboolean): PChar; cdecl;
    ReleaseStringUTFChars: procedure(env: PJNIEnv; str: jstring;
      utf: PChar); cdecl;
    NewStringUTF: function(env: PJNIEnv; utf: PChar): jstring; cdecl;
    // ... autres fonctions
  end;

procedure Java_StringTest_printString(env: PJNIEnv; obj: jobject;
  text: jstring); cdecl;
var
  nativeString: PChar;
  isCopy: jboolean;
begin
  // Convertir jstring en C string
  nativeString := env^^.GetStringUTFChars(env, text, @isCopy);
  try
    WriteLn('Texte reçu : ', nativeString);
  finally
    // Libérer la chaîne
    env^^.ReleaseStringUTFChars(env, text, nativeString);
  end;
end;

function Java_StringTest_reverseString(env: PJNIEnv; obj: jobject;
  text: jstring): jstring; cdecl;
var
  nativeString: PChar;
  reversed: string;
  i: Integer;
begin
  nativeString := env^^.GetStringUTFChars(env, text, nil);
  try
    reversed := '';
    for i := Length(nativeString) downto 1 do
      reversed := reversed + nativeString[i];

    // Créer une nouvelle jstring
    Result := env^^.NewStringUTF(env, PChar(reversed));
  finally
    env^^.ReleaseStringUTFChars(env, text, nativeString);
  end;
end;
```

### Tableaux

**Java** :
```java
public class ArrayTest {
    public native int sumArray(int[] numbers);
    public native int[] doubleArray(int[] numbers);
}
```

**Pascal** :
```pascal
function Java_ArrayTest_sumArray(env: PJNIEnv; obj: jobject;
  numbers: jintArray): jint; cdecl;
var
  len: jsize;
  elements: PJInt;
  i: Integer;
  sum: jint;
begin
  // Obtenir la longueur
  len := env^^.GetArrayLength(env, numbers);

  // Obtenir les éléments
  elements := env^^.GetIntArrayElements(env, numbers, nil);
  try
    sum := 0;
    for i := 0 to len - 1 do
      sum := sum + elements[i];
    Result := sum;
  finally
    // Libérer (0 = pas de copie vers Java)
    env^^.ReleaseIntArrayElements(env, numbers, elements, 0);
  end;
end;

function Java_ArrayTest_doubleArray(env: PJNIEnv; obj: jobject;
  numbers: jintArray): jintArray; cdecl;
var
  len: jsize;
  elements: PJInt;
  newArray: jintArray;
  i: Integer;
begin
  len := env^^.GetArrayLength(env, numbers);

  // Créer un nouveau tableau
  newArray := env^^.NewIntArray(env, len);

  elements := env^^.GetIntArrayElements(env, numbers, nil);
  try
    // Doubler les valeurs
    for i := 0 to len - 1 do
      elements[i] := elements[i] * 2;

    // Copier dans le nouveau tableau
    env^^.SetIntArrayRegion(env, newArray, 0, len, elements);
  finally
    env^^.ReleaseIntArrayElements(env, numbers, elements, JNI_ABORT);
  end;

  Result := newArray;
end;
```

## Appeler des méthodes Java depuis le code natif

### Trouver une classe

```pascal
procedure Java_CallbackTest_triggerCallback(env: PJNIEnv; obj: jobject); cdecl;  
var
  cls: jclass;
begin
  // Obtenir la classe de l'objet
  cls := env^^.GetObjectClass(env, obj);

  // Ou trouver une classe par nom
  cls := env^^.FindClass(env, 'java/lang/String');
end;
```

### Appeler une méthode

**Java** :
```java
public class CallbackTest {
    public native void triggerCallback();

    // Cette méthode sera appelée depuis le natif
    public void onCallback(String message, int value) {
        System.out.println("Callback: " + message + " = " + value);
    }
}
```

**Pascal** :
```pascal
procedure Java_CallbackTest_triggerCallback(env: PJNIEnv; obj: jobject); cdecl;  
var
  cls: jclass;
  methodID: jmethodID;
  message: jstring;
begin
  // 1. Obtenir la classe
  cls := env^^.GetObjectClass(env, obj);

  // 2. Obtenir l'ID de la méthode
  // Signature: (Ljava/lang/String;I)V
  // L = Object, V = void, I = int
  methodID := env^^.GetMethodID(env, cls, 'onCallback',
    '(Ljava/lang/String;I)V');

  // 3. Créer les paramètres
  message := env^^.NewStringUTF(env, 'Résultat');

  // 4. Appeler la méthode
  env^^.CallVoidMethod(env, obj, methodID, message, jint(42));
end;
```

### Signatures de méthodes JNI

Les signatures utilisent un format spécial :

| Type Java | Signature |
|-----------|-----------|
| boolean | Z |
| byte | B |
| char | C |
| short | S |
| int | I |
| long | J |
| float | F |
| double | D |
| void | V |
| Object | Lpackage/Class; |
| Array | [type |

**Exemples** :
```
void method()                    → ()V  
int method(String s)            → (Ljava/lang/String;)I  
void method(int[] arr, boolean b) → ([IZ)V  
String method(Object obj)       → (Ljava/lang/Object;)Ljava/lang/String;
```

## Gestion des exceptions

### Vérifier les exceptions

```pascal
function VerifierException(env: PJNIEnv): Boolean;  
begin
  Result := env^^.ExceptionCheck(env) = JNI_TRUE;
end;

procedure Java_ExceptionTest_causeException(env: PJNIEnv; obj: jobject); cdecl;  
var
  cls: jclass;
  methodID: jmethodID;
begin
  cls := env^^.FindClass(env, 'java/lang/String');

  // Ceci va échouer et lever une exception
  methodID := env^^.GetMethodID(env, cls, 'nonExistentMethod', '()V');

  if VerifierException(env) then
  begin
    WriteLn('Exception détectée !');
    env^^.ExceptionDescribe(env); // Afficher la stack trace
    env^^.ExceptionClear(env);     // Nettoyer l'exception
  end;
end;
```

### Lever une exception

```pascal
procedure LeverException(env: PJNIEnv; const message: string);  
var
  exceptionClass: jclass;
begin
  exceptionClass := env^^.FindClass(env, 'java/lang/RuntimeException');
  env^^.ThrowNew(env, exceptionClass, PChar(message));
end;

function Java_Calculator_divide(env: PJNIEnv; obj: jobject;
  a, b: jint): jint; cdecl;
begin
  if b = 0 then
  begin
    LeverException(env, 'Division par zéro !');
    Result := 0;
    Exit;
  end;

  Result := a div b;
end;
```

## Gestion de la mémoire

### Références locales vs globales

**Références locales** :
- Automatiquement libérées à la fin de la fonction native
- Limitées en nombre (généralement 16-32)
- Ne survivent pas entre appels

**Références globales** :
- Doivent être explicitement libérées
- Survivent entre appels
- Utilisées pour des callbacks ou états persistants

```pascal
var
  GlobalRef: jobject = nil;

procedure Java_MemoryTest_saveReference(env: PJNIEnv; obj: jobject;
  ref: jobject); cdecl;
begin
  // Supprimer l'ancienne référence globale si elle existe
  if GlobalRef <> nil then
    env^^.DeleteGlobalRef(env, GlobalRef);

  // Créer une nouvelle référence globale
  GlobalRef := env^^.NewGlobalRef(env, ref);
end;

procedure Java_MemoryTest_cleanup(env: PJNIEnv; obj: jobject); cdecl;  
begin
  if GlobalRef <> nil then
  begin
    env^^.DeleteGlobalRef(env, GlobalRef);
    GlobalRef := nil;
  end;
end;
```

### Libération des ressources

```pascal
procedure Java_ResourceTest_processFile(env: PJNIEnv; obj: jobject;
  filename: jstring); cdecl;
var
  nativeFilename: PChar;
  f: File;
begin
  nativeFilename := env^^.GetStringUTFChars(env, filename, nil);
  try
    // Traiter le fichier
    Assign(f, nativeFilename);
    try
      Reset(f);
      // ... traitement
    finally
      Close(f);
    end;
  finally
    // TOUJOURS libérer la chaîne
    env^^.ReleaseStringUTFChars(env, filename, nativeFilename);
  end;
end;
```

## Exemple complet : Bibliothèque mathématique

### Classe Java

**MathLibrary.java** :
```java
public class MathLibrary {
    // Chargement de la bibliothèque native
    static {
        System.loadLibrary("mathlib");
    }

    // Méthodes natives
    public native int factorial(int n);
    public native double power(double base, double exponent);
    public native int[] fibonacci(int n);
    public native String getVersion();

    // Test
    public static void main(String[] args) {
        MathLibrary lib = new MathLibrary();

        System.out.println("Version: " + lib.getVersion());
        System.out.println("Factorial(5) = " + lib.factorial(5));
        System.out.println("Power(2, 10) = " + lib.power(2, 10));

        int[] fib = lib.fibonacci(10);
        System.out.print("Fibonacci(10): ");
        for (int i = 0; i < fib.length; i++) {
            System.out.print(fib[i] + " ");
        }
        System.out.println();
    }
}
```

### Implémentation Pascal

**mathlib.pas** :
```pascal
library mathlib;

{$mode objfpc}{$H+}

uses
  SysUtils, Math, JNITypes;

type
  PJNIEnv = ^JNINativeInterface;

  JNINativeInterface = record
    reserved0: Pointer;
    reserved1: Pointer;
    reserved2: Pointer;
    reserved3: Pointer;

    GetVersion: function(env: PJNIEnv): jint; cdecl;
    // ... (simplifié, voir définition complète)

    NewStringUTF: function(env: PJNIEnv; utf: PChar): jstring; cdecl;
    GetStringUTFChars: function(env: PJNIEnv; str: jstring;
      isCopy: Pjboolean): PChar; cdecl;
    ReleaseStringUTFChars: procedure(env: PJNIEnv; str: jstring;
      chars: PChar); cdecl;

    NewIntArray: function(env: PJNIEnv; len: jsize): jintArray; cdecl;
    GetArrayLength: function(env: PJNIEnv; arr: jarray): jsize; cdecl;
    SetIntArrayRegion: procedure(env: PJNIEnv; arr: jintArray;
      start: jsize; len: jsize; buf: PJInt); cdecl;
  end;

// Factorial
function Java_MathLibrary_factorial(env: PJNIEnv; obj: jobject;
  n: jint): jint; cdecl;
var
  i: Integer;
  result: Int64;
begin
  if n < 0 then
  begin
    Result := -1;
    Exit;
  end;

  result := 1;
  for i := 2 to n do
    result := result * i;

  Result := result;
end;

// Power
function Java_MathLibrary_power(env: PJNIEnv; obj: jobject;
  base, exponent: jdouble): jdouble; cdecl;
begin
  Result := Math.Power(base, exponent);
end;

// Fibonacci
function Java_MathLibrary_fibonacci(env: PJNIEnv; obj: jobject;
  n: jint): jintArray; cdecl;
var
  fibArray: array of jint;
  i: Integer;
  result: jintArray;
begin
  SetLength(fibArray, n);

  if n > 0 then fibArray[0] := 0;
  if n > 1 then fibArray[1] := 1;

  for i := 2 to n - 1 do
    fibArray[i] := fibArray[i-1] + fibArray[i-2];

  // Créer un tableau Java
  result := env^^.NewIntArray(env, n);
  env^^.SetIntArrayRegion(env, result, 0, n, @fibArray[0]);

  Result := result;
end;

// GetVersion
function Java_MathLibrary_getVersion(env: PJNIEnv; obj: jobject): jstring; cdecl;  
begin
  Result := env^^.NewStringUTF(env, 'MathLibrary v1.0.0 (FreePascal)');
end;

exports
  Java_MathLibrary_factorial,
  Java_MathLibrary_power,
  Java_MathLibrary_fibonacci,
  Java_MathLibrary_getVersion;

begin  
end.
```

### Compilation et exécution

```bash
# Compiler Java
javac MathLibrary.java

# Compiler la bibliothèque native
# Windows
fpc -omathlib.dll mathlib.pas

# Linux
fpc -olibmathlib.so mathlib.pas

# Exécuter
java MathLibrary
```

**Sortie attendue** :
```
Version: MathLibrary v1.0.0 (FreePascal)  
Factorial(5) = 120  
Power(2, 10) = 1024.0  
Fibonacci(10): 0 1 1 2 3 5 8 13 21 34
```

## Multithreading et JNI

### Attacher/Détacher des threads

Si vous créez des threads natifs qui doivent appeler Java :

```pascal
uses
  JNITypes;

var
  JavaVM: PJavaVM;  // Sauvegardé lors de JNI_OnLoad

procedure MonThreadNatif;  
var
  env: PJNIEnv;
  attached: Boolean;
begin
  // Attacher le thread à la JVM
  attached := JavaVM^^.AttachCurrentThread(JavaVM, @env, nil) = JNI_OK;

  if attached then
  try
    // Maintenant on peut utiliser env pour appeler Java
    WriteLn('Thread attaché à la JVM');

    // Appeler des méthodes Java...

  finally
    // IMPORTANT : Détacher le thread quand on a fini
    JavaVM^^.DetachCurrentThread(JavaVM);
  end
  else
    WriteLn('Erreur lors de l''attachement du thread');
end;

// Cette fonction est appelée automatiquement par la JVM
function JNI_OnLoad(vm: PJavaVM; reserved: Pointer): jint; cdecl;  
begin
  // Sauvegarder le pointeur JavaVM pour utilisation ultérieure
  JavaVM := vm;

  WriteLn('Bibliothèque chargée, JNI version : 1.8');

  // Retourner la version JNI requise
  Result := JNI_VERSION_1_8;
end;

// Appelé lors du déchargement
procedure JNI_OnUnload(vm: PJavaVM; reserved: Pointer); cdecl;  
begin
  WriteLn('Bibliothèque déchargée');
  JavaVM := nil;
end;

exports
  JNI_OnLoad,
  JNI_OnUnload;
```

### Sécurité thread avec JNI

**Règles importantes** :
1. Chaque thread a son propre `JNIEnv`
2. Ne JAMAIS partager un `JNIEnv` entre threads
3. `JavaVM` peut être partagé entre threads
4. Utiliser des références globales pour les objets partagés

```pascal
type
  TCallbackThread = class(TThread)
  private
    FCallback: jobject;  // Référence globale
  protected
    procedure Execute; override;
  public
    constructor Create(env: PJNIEnv; callback: jobject);
    destructor Destroy; override;
  end;

constructor TCallbackThread.Create(env: PJNIEnv; callback: jobject);  
begin
  inherited Create(True);

  // Créer une référence globale (nécessaire pour l'utiliser dans un autre thread)
  FCallback := env^^.NewGlobalRef(env, callback);

  FreeOnTerminate := True;
end;

destructor TCallbackThread.Destroy;  
var
  env: PJNIEnv;
begin
  // Attacher pour libérer la référence
  if JavaVM^^.AttachCurrentThread(JavaVM, @env, nil) = JNI_OK then
  begin
    env^^.DeleteGlobalRef(env, FCallback);
    JavaVM^^.DetachCurrentThread(JavaVM);
  end;

  inherited;
end;

procedure TCallbackThread.Execute;  
var
  env: PJNIEnv;
  cls: jclass;
  methodID: jmethodID;
begin
  // Attacher ce thread à la JVM
  if JavaVM^^.AttachCurrentThread(JavaVM, @env, nil) <> JNI_OK then
    Exit;

  try
    // Appeler la méthode Java du callback
    cls := env^^.GetObjectClass(env, FCallback);
    methodID := env^^.GetMethodID(env, cls, 'onComplete', '()V');

    if methodID <> nil then
      env^^.CallVoidMethod(env, FCallback, methodID);

  finally
    // Détacher le thread
    JavaVM^^.DetachCurrentThread(JavaVM);
  end;
end;
```

## Optimisation des performances

### 1. Minimiser les transitions JNI

Les appels JNI ont un overhead. Regrouper les opérations.

**❌ Mauvais** : Appels multiples
```java
// Java
for (int i = 0; i < 1000; i++) {
    nativeIncrement(); // 1000 appels JNI !
}
```

**✅ Bon** : Un seul appel
```java
// Java
nativeIncrementBulk(1000); // 1 seul appel JNI
```

```pascal
// Pascal
procedure Java_Optimizer_nativeIncrementBulk(env: PJNIEnv; obj: jobject;
  count: jint); cdecl;
var
  i: Integer;
begin
  for i := 1 to count do
    Inc(GlobalCounter);
end;
```

### 2. Utiliser GetPrimitiveArrayCritical

Pour de grandes quantités de données :

```pascal
function Java_ArrayProcessor_processLargeArray(env: PJNIEnv; obj: jobject;
  data: jintArray): jint; cdecl;
var
  len: jsize;
  elements: PJInt;
  i: Integer;
  sum: jint;
begin
  len := env^^.GetArrayLength(env, data);

  // Plus rapide que GetIntArrayElements pour de grandes données
  elements := env^^.GetPrimitiveArrayCritical(env, data, nil);
  try
    sum := 0;
    for i := 0 to len - 1 do
      sum := sum + elements[i];
    Result := sum;
  finally
    // Mode 0 = copier les modifications vers Java
    env^^.ReleasePrimitiveArrayCritical(env, data, elements, 0);
  end;
end;
```

**Attention** : Entre `GetPrimitiveArrayCritical` et `ReleasePrimitiveArrayCritical`, ne PAS appeler d'autres fonctions JNI !

### 3. Mettre en cache les IDs

Chercher les méthodes/champs est coûteux. Les mettre en cache.

```pascal
var
  CachedMethodID: jmethodID = nil;
  CachedClass: jclass = nil;

procedure Java_CacheDemo_callMethod(env: PJNIEnv; obj: jobject); cdecl;  
var
  localClass: jclass;
begin
  // Initialiser le cache si nécessaire
  if CachedMethodID = nil then
  begin
    if CachedClass = nil then
    begin
      localClass := env^^.FindClass(env, 'com/example/MyClass');
      CachedClass := env^^.NewGlobalRef(env, localClass);
      env^^.DeleteLocalRef(env, localClass);
    end;

    CachedMethodID := env^^.GetMethodID(env, CachedClass, 'myMethod', '()V');
  end;

  // Utiliser le cache
  if CachedMethodID <> nil then
    env^^.CallVoidMethod(env, obj, CachedMethodID);
end;
```

### 4. Passer des buffers directs (ByteBuffer)

Pour éviter les copies de mémoire :

```java
// Java
ByteBuffer buffer = ByteBuffer.allocateDirect(1024);  
nativeProcessBuffer(buffer);
```

```pascal
// Pascal
procedure Java_BufferDemo_nativeProcessBuffer(env: PJNIEnv; obj: jobject;
  buffer: jobject); cdecl;
var
  data: Pointer;
  capacity: jlong;
begin
  // Obtenir l'adresse directe
  data := env^^.GetDirectBufferAddress(env, buffer);
  capacity := env^^.GetDirectBufferCapacity(env, buffer);

  if data <> nil then
  begin
    // Accéder directement à la mémoire, pas de copie !
    FillChar(data^, capacity, $FF);
  end;
end;
```

## Débogage JNI

### Activer les vérifications JNI

Au lancement de la JVM :

```bash
java -Xcheck:jni MonProgramme
```

Cela active des vérifications supplémentaires qui détectent :
- Références locales invalides
- Erreurs de signature de méthodes
- Utilisation incorrecte des tableaux
- Fuites de références globales

### Messages d'erreur courants

#### 1. UnsatisfiedLinkError

**Erreur** :
```
java.lang.UnsatisfiedLinkError: no mathlib in java.library.path
```

**Causes** :
- Bibliothèque non trouvée
- Nom incorrect (`mathlib` vs `libmathlib`)
- Mauvais répertoire

**Solutions** :
```bash
# Windows
set PATH=%PATH%;C:\chemin\vers\dll

# Linux
export LD_LIBRARY_PATH=/chemin/vers/so:$LD_LIBRARY_PATH

# Ou spécifier directement
java -Djava.library.path=/chemin/vers/lib MonProgramme
```

#### 2. Method not found

**Erreur** :
```
java.lang.UnsatisfiedLinkError: 'int add(int, int)'
```

**Causes** :
- Nom de fonction incorrect
- Signature incorrecte
- Fonction non exportée

**Vérifier** :
```bash
# Windows
dumpbin /EXPORTS mathlib.dll | findstr Java

# Linux
nm -D libmathlib.so | grep Java
```

#### 3. JNI DETECTED ERROR

**Erreur** :
```
JNI DETECTED ERROR: app passed bad pointer to JNI
```

**Causes** :
- Pointeur NULL
- Référence locale expirée
- Type incorrect

**Solution** : Utiliser `-Xcheck:jni` pour localiser le problème exact.

### Tracer les appels JNI

```pascal
unit JNIDebug;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, JNITypes;

procedure LogJNICall(const FunctionName: string; const Params: array of const);

implementation

var
  LogFile: TextFile;

procedure LogJNICall(const FunctionName: string; const Params: array of const);  
var
  i: Integer;
  ParamStr: string;
begin
  ParamStr := '';
  for i := Low(Params) to High(Params) do
  begin
    if i > Low(Params) then
      ParamStr := ParamStr + ', ';

    case Params[i].VType of
      vtInteger: ParamStr := ParamStr + IntToStr(Params[i].VInteger);
      vtInt64: ParamStr := ParamStr + IntToStr(Params[i].VInt64^);
      vtExtended: ParamStr := ParamStr + FloatToStr(Params[i].VExtended^);
      vtString: ParamStr := ParamStr + Params[i].VString^;
      vtAnsiString: ParamStr := ParamStr + string(Params[i].VAnsiString);
      vtPointer: ParamStr := ParamStr + Format('%p', [Params[i].VPointer]);
    end;
  end;

  WriteLn(LogFile, FormatDateTime('hh:nn:ss.zzz', Now), ' - ',
    FunctionName, '(', ParamStr, ')');
  Flush(LogFile);
end;

initialization
  AssignFile(LogFile, 'jni_debug.log');
  Rewrite(LogFile);
  WriteLn(LogFile, '=== JNI Debug Log ===');

finalization
  CloseFile(LogFile);

end.
```

Utilisation :
```pascal
function Java_Calculator_add(env: PJNIEnv; obj: jobject;
  a, b: jint): jint; cdecl;
begin
  LogJNICall('Java_Calculator_add', [a, b]);
  Result := a + b;
end;
```

## Gestion des objets complexes

### Créer des objets Java depuis le natif

```pascal
function Java_ObjectFactory_createPoint(env: PJNIEnv; obj: jobject;
  x, y: jint): jobject; cdecl;
var
  pointClass: jclass;
  constructor: jmethodID;
begin
  // Trouver la classe Point
  pointClass := env^^.FindClass(env, 'java/awt/Point');

  // Trouver le constructeur (int, int)
  constructor := env^^.GetMethodID(env, pointClass, '<init>', '(II)V');

  // Créer l'objet
  Result := env^^.NewObject(env, pointClass, constructor, x, y);
end;
```

### Accéder aux champs d'un objet

**Java** :
```java
public class Person {
    public String name;
    public int age;
}
```

**Pascal** :
```pascal
procedure Java_PersonHandler_printPerson(env: PJNIEnv; obj: jobject;
  person: jobject); cdecl;
var
  cls: jclass;
  nameFieldID, ageFieldID: jfieldID;
  nameStr: jstring;
  name: PChar;
  age: jint;
begin
  cls := env^^.GetObjectClass(env, person);

  // Obtenir les IDs des champs
  nameFieldID := env^^.GetFieldID(env, cls, 'name', 'Ljava/lang/String;');
  ageFieldID := env^^.GetFieldID(env, cls, 'age', 'I');

  // Lire les valeurs
  nameStr := env^^.GetObjectField(env, person, nameFieldID);
  age := env^^.GetIntField(env, person, ageFieldID);

  // Convertir le nom
  name := env^^.GetStringUTFChars(env, nameStr, nil);
  try
    WriteLn('Personne: ', name, ', âge: ', age);
  finally
    env^^.ReleaseStringUTFChars(env, nameStr, name);
  end;
end;
```

### Modifier des champs

```pascal
procedure Java_PersonHandler_birthday(env: PJNIEnv; obj: jobject;
  person: jobject); cdecl;
var
  cls: jclass;
  ageFieldID: jfieldID;
  age: jint;
begin
  cls := env^^.GetObjectClass(env, person);
  ageFieldID := env^^.GetFieldID(env, cls, 'age', 'I');

  // Lire l'âge actuel
  age := env^^.GetIntField(env, person, ageFieldID);

  // Incrémenter
  Inc(age);

  // Écrire le nouvel âge
  env^^.SetIntField(env, person, ageFieldID, age);
end;
```

## Interface JNI complète (fichier d'en-tête)

Pour une utilisation sérieuse, vous aurez besoin de la définition complète de l'interface JNI.

**JNI.pas** (version simplifiée - l'original fait plusieurs milliers de lignes) :

```pascal
unit JNI;

{$mode objfpc}{$H+}

interface

const
  JNI_VERSION_1_1 = $00010001;
  JNI_VERSION_1_2 = $00010002;
  JNI_VERSION_1_4 = $00010004;
  JNI_VERSION_1_6 = $00010006;
  JNI_VERSION_1_8 = $00010008;
  JNI_VERSION_9   = $00090000;
  JNI_VERSION_10  = $000a0000;

  JNI_OK        = 0;
  JNI_ERR       = -1;
  JNI_EDETACHED = -2;
  JNI_EVERSION  = -3;

  JNI_FALSE = 0;
  JNI_TRUE  = 1;

  JNI_COMMIT = 1;
  JNI_ABORT  = 2;

type
  jboolean = Byte;
  jbyte = ShortInt;
  jchar = Word;
  jshort = SmallInt;
  jint = LongInt;
  jlong = Int64;
  jfloat = Single;
  jdouble = Double;
  jsize = jint;

  jobject = Pointer;
  jclass = jobject;
  jstring = jobject;
  jarray = jobject;
  jobjectArray = jarray;
  jbooleanArray = jarray;
  jbyteArray = jarray;
  jcharArray = jarray;
  jshortArray = jarray;
  jintArray = jarray;
  jlongArray = jarray;
  jfloatArray = jarray;
  jdoubleArray = jarray;
  jthrowable = jobject;
  jweak = jobject;

  jvalue = record
    case Integer of
      0: (z: jboolean);
      1: (b: jbyte);
      2: (c: jchar);
      3: (s: jshort);
      4: (i: jint);
      5: (j: jlong);
      6: (f: jfloat);
      7: (d: jdouble);
      8: (l: jobject);
  end;
  Pjvalue = ^jvalue;

  jfieldID = Pointer;
  jmethodID = Pointer;

  PJNIEnv = ^JNINativeInterface;
  PPJNIEnv = ^PJNIEnv;

  PJavaVM = ^JNIInvokeInterface;
  PPJavaVM = ^PJavaVM;

  JNINativeInterface = record
    reserved0: Pointer;
    reserved1: Pointer;
    reserved2: Pointer;
    reserved3: Pointer;

    GetVersion: function(env: PJNIEnv): jint; cdecl;

    DefineClass: function(env: PJNIEnv; const name: PChar; loader: jobject;
      const buf: Pjbyte; len: jsize): jclass; cdecl;
    FindClass: function(env: PJNIEnv; const name: PChar): jclass; cdecl;

    // Réflexion
    FromReflectedMethod: function(env: PJNIEnv; method: jobject): jmethodID; cdecl;
    FromReflectedField: function(env: PJNIEnv; field: jobject): jfieldID; cdecl;
    ToReflectedMethod: function(env: PJNIEnv; cls: jclass; methodID: jmethodID;
      isStatic: jboolean): jobject; cdecl;

    GetSuperclass: function(env: PJNIEnv; sub: jclass): jclass; cdecl;
    IsAssignableFrom: function(env: PJNIEnv; sub: jclass; sup: jclass): jboolean; cdecl;
    ToReflectedField: function(env: PJNIEnv; cls: jclass; fieldID: jfieldID;
      isStatic: jboolean): jobject; cdecl;

    // Exceptions
    Throw: function(env: PJNIEnv; obj: jthrowable): jint; cdecl;
    ThrowNew: function(env: PJNIEnv; clazz: jclass; const msg: PChar): jint; cdecl;
    ExceptionOccurred: function(env: PJNIEnv): jthrowable; cdecl;
    ExceptionDescribe: procedure(env: PJNIEnv); cdecl;
    ExceptionClear: procedure(env: PJNIEnv); cdecl;
    FatalError: procedure(env: PJNIEnv; const msg: PChar); cdecl;

    // Références
    PushLocalFrame: function(env: PJNIEnv; capacity: jint): jint; cdecl;
    PopLocalFrame: function(env: PJNIEnv; result: jobject): jobject; cdecl;
    NewGlobalRef: function(env: PJNIEnv; lobj: jobject): jobject; cdecl;
    DeleteGlobalRef: procedure(env: PJNIEnv; gref: jobject); cdecl;
    DeleteLocalRef: procedure(env: PJNIEnv; obj: jobject); cdecl;
    IsSameObject: function(env: PJNIEnv; obj1: jobject; obj2: jobject): jboolean; cdecl;
    NewLocalRef: function(env: PJNIEnv; ref: jobject): jobject; cdecl;
    EnsureLocalCapacity: function(env: PJNIEnv; capacity: jint): jint; cdecl;

    // Objets
    AllocObject: function(env: PJNIEnv; clazz: jclass): jobject; cdecl;
    NewObject: function(env: PJNIEnv; clazz: jclass; methodID: jmethodID): jobject; cdecl varargs;
    NewObjectV: function(env: PJNIEnv; clazz: jclass; methodID: jmethodID;
      args: Pointer): jobject; cdecl;
    NewObjectA: function(env: PJNIEnv; clazz: jclass; methodID: jmethodID;
      args: Pjvalue): jobject; cdecl;

    GetObjectClass: function(env: PJNIEnv; obj: jobject): jclass; cdecl;
    IsInstanceOf: function(env: PJNIEnv; obj: jobject; clazz: jclass): jboolean; cdecl;

    // Méthodes
    GetMethodID: function(env: PJNIEnv; clazz: jclass; const name: PChar;
      const sig: PChar): jmethodID; cdecl;

    CallObjectMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID): jobject; cdecl varargs;
    CallBooleanMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID): jboolean; cdecl varargs;
    CallByteMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID): jbyte; cdecl varargs;
    CallCharMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID): jchar; cdecl varargs;
    CallShortMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID): jshort; cdecl varargs;
    CallIntMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID): jint; cdecl varargs;
    CallLongMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID): jlong; cdecl varargs;
    CallFloatMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID): jfloat; cdecl varargs;
    CallDoubleMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID): jdouble; cdecl varargs;
    CallVoidMethod: procedure(env: PJNIEnv; obj: jobject; methodID: jmethodID); cdecl varargs;

    // Champs
    GetFieldID: function(env: PJNIEnv; clazz: jclass; const name: PChar;
      const sig: PChar): jfieldID; cdecl;

    GetObjectField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID): jobject; cdecl;
    GetBooleanField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID): jboolean; cdecl;
    GetByteField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID): jbyte; cdecl;
    GetCharField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID): jchar; cdecl;
    GetShortField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID): jshort; cdecl;
    GetIntField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID): jint; cdecl;
    GetLongField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID): jlong; cdecl;
    GetFloatField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID): jfloat; cdecl;
    GetDoubleField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID): jdouble; cdecl;

    SetObjectField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID; val: jobject); cdecl;
    SetBooleanField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID; val: jboolean); cdecl;
    SetByteField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID; val: jbyte); cdecl;
    SetCharField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID; val: jchar); cdecl;
    SetShortField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID; val: jshort); cdecl;
    SetIntField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID; val: jint); cdecl;
    SetLongField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID; val: jlong); cdecl;
    SetFloatField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID; val: jfloat); cdecl;
    SetDoubleField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID; val: jdouble); cdecl;

    // Méthodes statiques
    GetStaticMethodID: function(env: PJNIEnv; clazz: jclass; const name: PChar;
      const sig: PChar): jmethodID; cdecl;

    // Chaînes
    NewStringUTF: function(env: PJNIEnv; const utf: PChar): jstring; cdecl;
    GetStringUTFLength: function(env: PJNIEnv; str: jstring): jsize; cdecl;
    GetStringUTFChars: function(env: PJNIEnv; str: jstring; isCopy: Pjboolean): PChar; cdecl;
    ReleaseStringUTFChars: procedure(env: PJNIEnv; str: jstring; const chars: PChar); cdecl;

    // Tableaux
    GetArrayLength: function(env: PJNIEnv; arr: jarray): jsize; cdecl;
    NewObjectArray: function(env: PJNIEnv; len: jsize; clazz: jclass;
      init: jobject): jobjectArray; cdecl;
    GetObjectArrayElement: function(env: PJNIEnv; arr: jobjectArray;
      index: jsize): jobject; cdecl;
    SetObjectArrayElement: procedure(env: PJNIEnv; arr: jobjectArray;
      index: jsize; val: jobject); cdecl;

    NewBooleanArray: function(env: PJNIEnv; len: jsize): jbooleanArray; cdecl;
    NewByteArray: function(env: PJNIEnv; len: jsize): jbyteArray; cdecl;
    NewCharArray: function(env: PJNIEnv; len: jsize): jcharArray; cdecl;
    NewShortArray: function(env: PJNIEnv; len: jsize): jshortArray; cdecl;
    NewIntArray: function(env: PJNIEnv; len: jsize): jintArray; cdecl;
    NewLongArray: function(env: PJNIEnv; len: jsize): jlongArray; cdecl;
    NewFloatArray: function(env: PJNIEnv; len: jsize): jfloatArray; cdecl;
    NewDoubleArray: function(env: PJNIEnv; len: jsize): jdoubleArray; cdecl;

    GetBooleanArrayElements: function(env: PJNIEnv; arr: jbooleanArray;
      isCopy: Pjboolean): Pjboolean; cdecl;
    GetByteArrayElements: function(env: PJNIEnv; arr: jbyteArray;
      isCopy: Pjboolean): Pjbyte; cdecl;
    GetCharArrayElements: function(env: PJNIEnv; arr: jcharArray;
      isCopy: Pjboolean): Pjchar; cdecl;
    GetShortArrayElements: function(env: PJNIEnv; arr: jshortArray;
      isCopy: Pjboolean): Pjshort; cdecl;
    GetIntArrayElements: function(env: PJNIEnv; arr: jintArray;
      isCopy: Pjboolean): Pjint; cdecl;
    GetLongArrayElements: function(env: PJNIEnv; arr: jlongArray;
      isCopy: Pjboolean): Pjlong; cdecl;
    GetFloatArrayElements: function(env: PJNIEnv; arr: jfloatArray;
      isCopy: Pjboolean): Pjfloat; cdecl;
    GetDoubleArrayElements: function(env: PJNIEnv; arr: jdoubleArray;
      isCopy: Pjboolean): Pjdouble; cdecl;

    ReleaseBooleanArrayElements: procedure(env: PJNIEnv; arr: jbooleanArray;
      elems: Pjboolean; mode: jint); cdecl;
    ReleaseByteArrayElements: procedure(env: PJNIEnv; arr: jbyteArray;
      elems: Pjbyte; mode: jint); cdecl;
    ReleaseCharArrayElements: procedure(env: PJNIEnv; arr: jcharArray;
      elems: Pjchar; mode: jint); cdecl;
    ReleaseShortArrayElements: procedure(env: PJNIEnv; arr: jshortArray;
      elems: Pjshort; mode: jint); cdecl;
    ReleaseIntArrayElements: procedure(env: PJNIEnv; arr: jintArray;
      elems: Pjint; mode: jint); cdecl;
    ReleaseLongArrayElements: procedure(env: PJNIEnv; arr: jlongArray;
      elems: Pjlong; mode: jint); cdecl;
    ReleaseFloatArrayElements: procedure(env: PJNIEnv; arr: jfloatArray;
      elems: Pjfloat; mode: jint); cdecl;
    ReleaseDoubleArrayElements: procedure(env: PJNIEnv; arr: jdoubleArray;
      elems: Pjdouble; mode: jint); cdecl;

    // Copie de régions de tableau
    GetBooleanArrayRegion: procedure(env: PJNIEnv; arr: jbooleanArray;
      start: jsize; len: jsize; buf: Pjboolean); cdecl;
    GetByteArrayRegion: procedure(env: PJNIEnv; arr: jbyteArray;
      start: jsize; len: jsize; buf: Pjbyte); cdecl;
    GetCharArrayRegion: procedure(env: PJNIEnv; arr: jcharArray;
      start: jsize; len: jsize; buf: Pjchar); cdecl;
    GetShortArrayRegion: procedure(env: PJNIEnv; arr: jshortArray;
      start: jsize; len: jsize; buf: Pjshort); cdecl;
    GetIntArrayRegion: procedure(env: PJNIEnv; arr: jintArray;
      start: jsize; len: jsize; buf: Pjint); cdecl;
    GetLongArrayRegion: procedure(env: PJNIEnv; arr: jlongArray;
      start: jsize; len: jsize; buf: Pjlong); cdecl;
    GetFloatArrayRegion: procedure(env: PJNIEnv; arr: jfloatArray;
      start: jsize; len: jsize; buf: Pjfloat); cdecl;
    GetDoubleArrayRegion: procedure(env: PJNIEnv; arr: jdoubleArray;
      start: jsize; len: jsize; buf: Pjdouble); cdecl;

    SetBooleanArrayRegion: procedure(env: PJNIEnv; arr: jbooleanArray;
      start: jsize; len: jsize; buf: Pjboolean); cdecl;
    SetByteArrayRegion: procedure(env: PJNIEnv; arr: jbyteArray;
      start: jsize; len: jsize; buf: Pjbyte); cdecl;
    SetCharArrayRegion: procedure(env: PJNIEnv; arr: jcharArray;
      start: jsize; len: jsize; buf: Pjchar); cdecl;
    SetShortArrayRegion: procedure(env: PJNIEnv; arr: jshortArray;
      start: jsize; len: jsize; buf: Pjshort); cdecl;
    SetIntArrayRegion: procedure(env: PJNIEnv; arr: jintArray;
      start: jsize; len: jsize; buf: Pjint); cdecl;
    SetLongArrayRegion: procedure(env: PJNIEnv; arr: jlongArray;
      start: jsize; len: jsize; buf: Pjlong); cdecl;
    SetFloatArrayRegion: procedure(env: PJNIEnv; arr: jfloatArray;
      start: jsize; len: jsize; buf: Pjfloat); cdecl;
    SetDoubleArrayRegion: procedure(env: PJNIEnv; arr: jdoubleArray;
      start: jsize; len: jsize; buf: Pjdouble); cdecl;

    // Enregistrement de méthodes natives
    RegisterNatives: function(env: PJNIEnv; clazz: jclass;
      const methods: PJNINativeMethod; nMethods: jint): jint; cdecl;
    UnregisterNatives: function(env: PJNIEnv; clazz: jclass): jint; cdecl;

    // Moniteurs
    MonitorEnter: function(env: PJNIEnv; obj: jobject): jint; cdecl;
    MonitorExit: function(env: PJNIEnv; obj: jobject): jint; cdecl;

    // JavaVM
    GetJavaVM: function(env: PJNIEnv; vm: PPJavaVM): jint; cdecl;

    // Opérations critiques (JNI 1.2+)
    GetPrimitiveArrayCritical: function(env: PJNIEnv; arr: jarray;
      isCopy: Pjboolean): Pointer; cdecl;
    ReleasePrimitiveArrayCritical: procedure(env: PJNIEnv; arr: jarray;
      carray: Pointer; mode: jint); cdecl;

    // Chaînes critiques
    GetStringCritical: function(env: PJNIEnv; str: jstring;
      isCopy: Pjboolean): Pjchar; cdecl;
    ReleaseStringCritical: procedure(env: PJNIEnv; str: jstring;
      cstring: Pjchar); cdecl;

    // Références faibles (JNI 1.2+)
    NewWeakGlobalRef: function(env: PJNIEnv; obj: jobject): jweak; cdecl;
    DeleteWeakGlobalRef: procedure(env: PJNIEnv; ref: jweak); cdecl;

    // Vérification des exceptions
    ExceptionCheck: function(env: PJNIEnv): jboolean; cdecl;

    // ByteBuffer (JNI 1.4+)
    NewDirectByteBuffer: function(env: PJNIEnv; address: Pointer;
      capacity: jlong): jobject; cdecl;
    GetDirectBufferAddress: function(env: PJNIEnv; buf: jobject): Pointer; cdecl;
    GetDirectBufferCapacity: function(env: PJNIEnv; buf: jobject): jlong; cdecl;

    // Type d'objet (JNI 1.6+)
    GetObjectRefType: function(env: PJNIEnv; obj: jobject): jobjectRefType; cdecl;
  end;

  JNIInvokeInterface = record
    reserved0: Pointer;
    reserved1: Pointer;
    reserved2: Pointer;

    DestroyJavaVM: function(vm: PJavaVM): jint; cdecl;
    AttachCurrentThread: function(vm: PJavaVM; penv: PPJNIEnv;
      args: Pointer): jint; cdecl;
    DetachCurrentThread: function(vm: PJavaVM): jint; cdecl;
    GetEnv: function(vm: PJavaVM; penv: PPointer; version: jint): jint; cdecl;
    AttachCurrentThreadAsDaemon: function(vm: PJavaVM; penv: PPJNIEnv;
      args: Pointer): jint; cdecl;
  end;

  JNINativeMethod = record
    name: PChar;
    signature: PChar;
    fnPtr: Pointer;
  end;
  PJNINativeMethod = ^JNINativeMethod;

  jobjectRefType = (
    JNIInvalidRefType = 0,
    JNILocalRefType = 1,
    JNIGlobalRefType = 2,
    JNIWeakGlobalRefType = 3
  );

implementation

end.
```

## Bonnes pratiques

### ✅ À faire

#### 1. Toujours vérifier les pointeurs NULL

```pascal
function Java_SafeCode_process(env: PJNIEnv; obj: jobject;
  str: jstring): jstring; cdecl;
var
  chars: PChar;
begin
  Result := nil;

  // Vérifier l'entrée
  if str = nil then
  begin
    LeverException(env, 'String parameter cannot be null');
    Exit;
  end;

  chars := env^^.GetStringUTFChars(env, str, nil);
  if chars = nil then
  begin
    // Exception déjà levée par JNI
    Exit;
  end;

  try
    // Traiter la chaîne
    Result := env^^.NewStringUTF(env, PChar(UpperCase(chars)));
  finally
    env^^.ReleaseStringUTFChars(env, str, chars);
  end;
end;
```

#### 2. Libérer toutes les ressources

```pascal
procedure Java_ResourceTest_processData(env: PJNIEnv; obj: jobject); cdecl;  
var
  cls: jclass;
  localRef: jobject;
  globalRef: jobject;
begin
  cls := env^^.FindClass(env, 'java/lang/String');
  localRef := env^^.NewObject(env, cls, ...);
  globalRef := env^^.NewGlobalRef(env, localRef);

  try
    // Utiliser les références
  finally
    // Libérer dans l'ordre inverse
    if globalRef <> nil then
      env^^.DeleteGlobalRef(env, globalRef);
    if localRef <> nil then
      env^^.DeleteLocalRef(env, localRef);
  end;
end;
```

#### 3. Vérifier les exceptions après chaque appel JNI

```pascal
function AppelerMethodeSafe(env: PJNIEnv; obj: jobject): Boolean;  
var
  cls: jclass;
  methodID: jmethodID;
begin
  Result := False;

  cls := env^^.GetObjectClass(env, obj);
  if env^^.ExceptionCheck(env) = JNI_TRUE then
  begin
    env^^.ExceptionDescribe(env);
    env^^.ExceptionClear(env);
    Exit;
  end;

  methodID := env^^.GetMethodID(env, cls, 'process', '()V');
  if env^^.ExceptionCheck(env) = JNI_TRUE then
  begin
    env^^.ExceptionDescribe(env);
    env^^.ExceptionClear(env);
    Exit;
  end;

  env^^.CallVoidMethod(env, obj, methodID);
  if env^^.ExceptionCheck(env) = JNI_TRUE then
  begin
    env^^.ExceptionDescribe(env);
    env^^.ExceptionClear(env);
    Exit;
  end;

  Result := True;
end;
```

#### 4. Utiliser des frames locales pour beaucoup d'objets

```pascal
procedure Java_LoopTest_processMany(env: PJNIEnv; obj: jobject;
  count: jint); cdecl;
var
  i: Integer;
  localObj: jobject;
begin
  for i := 1 to count do
  begin
    // Créer un frame local pour ce cycle
    if env^^.PushLocalFrame(env, 10) < 0 then
      Break; // Mémoire insuffisante

    try
      // Créer des objets locaux
      localObj := CreerObjet(env);

      // Les utiliser...

    finally
      // Libérer tous les objets locaux du frame
      env^^.PopLocalFrame(env, nil);
    end;
  end;
end;
```

### ❌ À éviter

#### 1. Ne jamais partager JNIEnv entre threads

```pascal
// ❌ MAUVAIS - JNIEnv n'est pas thread-safe !
var
  GlobalEnv: PJNIEnv;  // NE JAMAIS FAIRE ÇA

procedure Thread1;  
begin
  GlobalEnv^^.CallVoidMethod(...);  // DANGEREUX !
end;
```

#### 2. Ne pas oublier de libérer les chaînes

```pascal
// ❌ MAUVAIS - Fuite mémoire
procedure Java_Leak_process(env: PJNIEnv; obj: jobject; str: jstring); cdecl;  
var
  chars: PChar;
begin
  chars := env^^.GetStringUTFChars(env, str, nil);
  WriteLn(chars);
  // Oubli de ReleaseStringUTFChars !
end;
```

#### 3. Ne pas manipuler des références locales après leur portée

```pascal
// ❌ MAUVAIS
var
  SavedRef: jobject;

procedure Java_Bad_save(env: PJNIEnv; obj: jobject; ref: jobject); cdecl;  
begin
  SavedRef := ref;  // ERREUR : référence locale expirée !
end;

procedure Java_Bad_use(env: PJNIEnv; obj: jobject); cdecl;  
begin
  // SavedRef est invalide maintenant !
  env^^.CallVoidMethod(env, SavedRef, ...);  // CRASH probable
end;

// ✅ BON - Utiliser une référence globale
var
  SavedRef: jobject = nil;

procedure Java_Good_save(env: PJNIEnv; obj: jobject; ref: jobject); cdecl;  
begin
  if SavedRef <> nil then
    env^^.DeleteGlobalRef(env, SavedRef);
  SavedRef := env^^.NewGlobalRef(env, ref);
end;
```

## Cas d'usage avancés

### Intégration avec Android (NDK)

Bien que nous ne couvrions pas Android en détail, voici les bases :

**Java (Android)** :
```java
public class NativeLib {
    static {
        System.loadLibrary("native-lib");
    }

    public native String getDeviceInfo();
    public native void processImage(Bitmap bitmap);
}
```

**Pascal** :
```pascal
library nativelib;

{$mode objfpc}{$H+}

uses
  SysUtils, JNI;

function Java_com_example_app_NativeLib_getDeviceInfo(
  env: PJNIEnv; obj: jobject): jstring; cdecl;
begin
  Result := env^^.NewStringUTF(env,
    'Device Info: Android FreePascal Native');
end;

exports
  Java_com_example_app_NativeLib_getDeviceInfo;

begin  
end.
```

### Bibliothèque de traitement d'images

```pascal
library imageproc;

{$mode objfpc}{$H+}

uses
  SysUtils, JNI, FPImage, FPReadPNG, FPWritePNG;

function Java_ImageProcessor_applyGrayscale(env: PJNIEnv; obj: jobject;
  pixels: jintArray; width, height: jint): jintArray; cdecl;
var
  len: jsize;
  elements: PJInt;
  i: Integer;
  r, g, b, gray: Byte;
  pixel: jint;
begin
  len := env^^.GetArrayLength(env, pixels);
  elements := env^^.GetIntArrayElements(env, pixels, nil);

  try
    for i := 0 to len - 1 do
    begin
      pixel := elements[i];

      // Extraire RGB (format ARGB)
      r := (pixel shr 16) and $FF;
      g := (pixel shr 8) and $FF;
      b := pixel and $FF;

      // Calculer niveau de gris
      gray := Round(0.299 * r + 0.587 * g + 0.114 * b);

      // Reconstruire le pixel
      elements[i] := (pixel and $FF000000) or
                     (gray shl 16) or
                     (gray shl 8) or
                     gray;
    end;

    // Créer un nouveau tableau avec les valeurs modifiées
    Result := env^^.NewIntArray(env, len);
    env^^.SetIntArrayRegion(env, Result, 0, len, elements);

  finally
    env^^.ReleaseIntArrayElements(env, pixels, elements, JNI_ABORT);
  end;
end;

exports
  Java_ImageProcessor_applyGrayscale;

begin  
end.
```

### Wrapper pour bibliothèque Pascal existante

Supposons que vous avez une bibliothèque Pascal pour le calcul scientifique :

**MathCore.pas** (bibliothèque existante) :
```pascal
unit MathCore;

interface

type
  TVector = array of Double;
  TMatrix = array of array of Double;

function DotProduct(const A, B: TVector): Double;  
function MatrixMultiply(const A, B: TMatrix): TMatrix;  
function Determinant(const M: TMatrix): Double;

implementation

// Implémentations...

end.
```

**Wrapper JNI** :
```pascal
library mathjni;

{$mode objfpc}{$H+}

uses
  SysUtils, JNI, MathCore;

function Java_MathWrapper_dotProduct(env: PJNIEnv; obj: jobject;
  a, b: jdoubleArray): jdouble; cdecl;
var
  lenA, lenB: jsize;
  elemsA, elemsB: PJDouble;
  vecA, vecB: TVector;
  i: Integer;
begin
  Result := 0;

  lenA := env^^.GetArrayLength(env, a);
  lenB := env^^.GetArrayLength(env, b);

  if lenA <> lenB then
  begin
    LeverException(env, 'Vectors must have same length');
    Exit;
  end;

  elemsA := env^^.GetDoubleArrayElements(env, a, nil);
  elemsB := env^^.GetDoubleArrayElements(env, b, nil);

  try
    // Convertir en types Pascal
    SetLength(vecA, lenA);
    SetLength(vecB, lenB);

    for i := 0 to lenA - 1 do
    begin
      vecA[i] := elemsA[i];
      vecB[i] := elemsB[i];
    end;

    // Utiliser la bibliothèque Pascal
    Result := MathCore.DotProduct(vecA, vecB);

  finally
    env^^.ReleaseDoubleArrayElements(env, a, elemsA, JNI_ABORT);
    env^^.ReleaseDoubleArrayElements(env, b, elemsB, JNI_ABORT);
  end;
end;

exports
  Java_MathWrapper_dotProduct;

begin  
end.
```

## Comparaison avec d'autres technologies

### JNI vs JNA (Java Native Access)

| Aspect | JNI | JNA |
|--------|-----|-----|
| **Complexité** | Élevée | Faible |
| **Performance** | Excellente | Bonne |
| **Portabilité** | Manuelle | Automatique |
| **Type safety** | Compilé | Runtime |
| **Courbe d'apprentissage** | Raide | Douce |

**JNA** : Plus simple mais moins performant. Pas besoin de code C/Pascal intermédiaire.

**JNI** : Plus complexe mais contrôle total et meilleures performances.

### JNI vs FFI (Foreign Function Interface)

Autres langages offrent des alternatives :
- **Python** : ctypes, cffi
- **Ruby** : FFI gem
- **Rust** : FFI intégré
- **Go** : cgo

JNI est spécifique à Java mais très mature et optimisé.

## Checklist de déploiement

Avant de distribuer une application utilisant JNI :

- [ ] Compiler pour toutes les plateformes cibles (Windows, Linux, macOS)
- [ ] Compiler pour les architectures (x86, x64, ARM)
- [ ] Tester avec différentes versions de JVM (8, 11, 17, 21)
- [ ] Vérifier les fuites mémoire (jvisualvm, valgrind)
- [ ] Documenter les méthodes natives
- [ ] Fournir un fallback Java pur si possible
- [ ] Tester le chargement de la bibliothèque
- [ ] Gérer gracieusement l'absence de la bibliothèque
- [ ] Vérifier les permissions sur Android
- [ ] Logger les erreurs JNI
- [ ] Tests de charge et stress
- [ ] Validation thread-safety

## Ressources et références

### Documentation officielle

- **JNI Specification** : https://docs.oracle.com/en/java/javase/17/docs/specs/jni/
- **JNI Functions** : https://docs.oracle.com/en/java/javase/17/docs/specs/jni/functions.html
- **JNI Types** : https://docs.oracle.com/en/java/javase/17/docs/specs/jni/types.html

### Outils

- **javah** : Générateur d'en-têtes (JDK < 10)
- **javac -h** : Générateur d'en-têtes (JDK 8+)
- **jvisualvm** : Profilage et monitoring
- **MAT** : Memory Analyzer Tool

### Livres recommandés

- "Essential JNI: Java Native Interface" - Rob Gordon
- "The Java Native Interface: Programmer's Guide and Specification" - Sheng Liang

### Communauté

- Stack Overflow (tags: jni, java-native-interface)
- Oracle Java Forums
- Reddit r/java (pour questions JNI)

## Conclusion

JNI est un outil puissant pour combiner la portabilité de Java avec la performance du code natif FreePascal. Bien qu'il présente une courbe d'apprentissage importante, il offre des possibilités uniques.

### Points clés à retenir

✅ **Avantages de JNI** :
- Performance native optimale
- Réutilisation de code existant
- Accès bas niveau au système
- Intégration transparente avec Java
- Standard bien établi

⚠️ **Limitations** :
- Complexité élevée
- Risque de crashes natifs
- Débogage difficile
- Gestion mémoire délicate
- Portabilité manuelle

### Quand utiliser JNI avec FreePascal ?

**Utilisez JNI si** :
- Vous avez du code Pascal performant à réutiliser
- Vous devez optimiser des calculs intensifs
- Vous voulez accéder à des API système spécifiques
- Vous développez pour l'écosystème Java d'entreprise
- La performance est critique

**Évitez JNI si** :
- La logique est simple (Java suffit)
- Vous débutez avec Java ou FreePascal
- La portabilité est une priorité absolue
- L'équipe ne maîtrise pas les deux langages
- Le débogage doit être simple

### Message final

> JNI est comme un pont entre deux îles : Java et le code natif. Construire ce pont demande du temps et des compétences, mais une fois construit, il permet des échanges riches et performants entre les deux mondes.

L'interfaçage Java/FreePascal via JNI ouvre des possibilités fascinantes : utiliser les meilleures bibliothèques Pascal dans l'écosystème Java, créer des applications Android avec du code natif Pascal, ou encore optimiser des serveurs d'applications Java avec du Pascal compilé.

Bien maîtrisé, JNI devient un atout majeur dans votre boîte à outils de développement multi-langage.

---

## Pour aller plus loin

**Chapitres connexes** :
- **19.1-19.2** : Création de bibliothèques partagées (bases nécessaires)
- **19.3** : Interfaçage avec Python (comparaison des approches)
- **19.4** : COM/ActiveX (équivalent Windows)
- **Chapitre 11** : Multithreading (pour JNI thread-safe)

**Projets suggérés** :
1. Wrapper JNI pour bibliothèque de traitement d'images
2. Optimisation de calculs scientifiques Java avec Pascal
3. Plugin natif pour application Java
4. Bibliothèque de cryptographie haute performance
5. Intégration matérielle via JNI

**Prochaines étapes** :
1. Pratiquer avec des exemples simples
2. Étudier des projets open source utilisant JNI
3. Créer votre propre bibliothèque JNI
4. Optimiser et profiler vos implementations
5. Contribuer à des bindings JNI FreePascal

Bon développement avec JNI et FreePascal ! ☕🚀

⏭️ [.NET interop (Windows)](/19-interoperabilite-bindings/07-dotnet-interop-windows.md)
