🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.1 FPCUnit - Framework de tests unitaires

## Introduction

Les tests unitaires sont une pratique essentielle en développement logiciel. Ils permettent de vérifier automatiquement que chaque partie de votre code fonctionne correctement, de détecter rapidement les régressions lors de modifications, et d'améliorer la qualité globale de vos applications.

**FPCUnit** est le framework de tests unitaires intégré à FreePascal et Lazarus. Il s'inspire de la famille xUnit (JUnit, NUnit, etc.) et offre une solution simple et puissante pour tester vos applications de manière systématique.

## Pourquoi utiliser les tests unitaires ?

### Avantages des tests unitaires

1. **Détection précoce des bugs** : Les erreurs sont identifiées dès leur introduction
2. **Documentation vivante** : Les tests montrent comment utiliser votre code
3. **Refactoring sécurisé** : Vous pouvez modifier le code en toute confiance
4. **Conception améliorée** : Écrire des tests vous pousse à créer un code plus modulaire
5. **Régression prévenue** : Les anciens bugs ne réapparaissent pas

### Qu'est-ce qu'un test unitaire ?

Un test unitaire vérifie le comportement d'une **unité** de code (une fonction, une méthode, une classe) de manière isolée. Chaque test doit être :

- **Indépendant** : Ne dépend pas des autres tests
- **Rapide** : S'exécute en quelques millisecondes
- **Répétable** : Donne toujours le même résultat
- **Auto-vérifiant** : Indique clairement succès ou échec

## Installation et configuration de FPCUnit

### Installation dans Lazarus

FPCUnit est inclus par défaut dans FreePascal et Lazarus. Pour l'utiliser :

#### Sur Windows

1. Ouvrez Lazarus
2. Allez dans **Paquet** → **Ouvrir un paquet (.lpk)**
3. Naviguez vers `[répertoire_lazarus]\components\fpcunit\`
4. Ouvrez `fpcunitconsolerunner.lpk`
5. Cliquez sur **Compiler** puis **Utiliser** → **Ajouter au projet**

#### Sur Ubuntu/Linux

1. Ouvrez Lazarus
2. Même procédure que Windows
3. Le chemin typique est `/usr/lib/lazarus/[version]/components/fpcunit/`

### Création d'un projet de tests

Lazarus propose un assistant pour créer rapidement des projets de tests :

1. **Projet** → **Nouveau projet**
2. Sélectionnez **Application console FPCUnit**
3. Donnez un nom à votre projet (ex: `MonProjetTests`)
4. Lazarus génère automatiquement la structure de base

## Structure d'un projet FPCUnit

### Architecture de base

Un projet FPCUnit typique comprend :

```
MonProjet/
├── src/                    # Code source de votre application
│   └── MaClasse.pas
├── tests/                  # Vos tests
│   └── TestMaClasse.pas
└── MonProjetTests.lpr      # Programme principal des tests
```

### Le programme principal

Le fichier `.lpr` contient le point d'entrée qui exécute tous les tests :

```pascal
program MonProjetTests;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, TestMaClasse;

type
  TMonTestRunner = class(TTestRunner)
  protected
    // Configuration personnalisée si nécessaire
  end;

var
  Application: TMonTestRunner;

begin
  Application := TMonTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'Tests de Mon Projet';
  Application.Run;
  Application.Free;
end.
```

## Créer votre premier test

### Exemple : tester une classe simple

Supposons que vous ayez une classe `TCalculatrice` :

```pascal
unit Calculatrice;

{$mode objfpc}{$H+}

interface

type
  TCalculatrice = class
  public
    function Additionner(a, b: Integer): Integer;
    function Soustraire(a, b: Integer): Integer;
    function Multiplier(a, b: Integer): Integer;
    function Diviser(a, b: Double): Double;
  end;

implementation

function TCalculatrice.Additionner(a, b: Integer): Integer;  
begin
  Result := a + b;
end;

function TCalculatrice.Soustraire(a, b: Integer): Integer;  
begin
  Result := a - b;
end;

function TCalculatrice.Multiplier(a, b: Integer): Integer;  
begin
  Result := a * b;
end;

function TCalculatrice.Diviser(a, b: Double): Double;  
begin
  if b = 0 then
    raise Exception.Create('Division par zéro impossible');
  Result := a / b;
end;

end.
```

### Créer la classe de test

Voici comment tester cette calculatrice avec FPCUnit :

```pascal
unit TestCalculatrice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Calculatrice;

type
  TTestCalculatrice = class(TTestCase)
  private
    FCalculatrice: TCalculatrice;
  protected
    // Appelé avant chaque test
    procedure SetUp; override;
    // Appelé après chaque test
    procedure TearDown; override;
  published
    // Les méthodes de test doivent être publiées
    procedure TestAddition;
    procedure TestSoustraction;
    procedure TestMultiplication;
    procedure TestDivision;
    procedure TestDivisionParZero;
  end;

implementation

procedure TTestCalculatrice.SetUp;  
begin
  // Initialisation avant chaque test
  FCalculatrice := TCalculatrice.Create;
end;

procedure TTestCalculatrice.TearDown;  
begin
  // Nettoyage après chaque test
  FCalculatrice.Free;
end;

procedure TTestCalculatrice.TestAddition;  
begin
  // Vérification que 2 + 3 = 5
  AssertEquals('Addition simple', 5, FCalculatrice.Additionner(2, 3));

  // Test avec des nombres négatifs
  AssertEquals('Addition avec négatifs', -1, FCalculatrice.Additionner(-3, 2));

  // Test avec zéro
  AssertEquals('Addition avec zéro', 7, FCalculatrice.Additionner(7, 0));
end;

procedure TTestCalculatrice.TestSoustraction;  
begin
  AssertEquals('Soustraction simple', 2, FCalculatrice.Soustraire(5, 3));
  AssertEquals('Soustraction négative', -5, FCalculatrice.Soustraire(3, 8));
end;

procedure TTestCalculatrice.TestMultiplication;  
begin
  AssertEquals('Multiplication simple', 15, FCalculatrice.Multiplier(3, 5));
  AssertEquals('Multiplication par zéro', 0, FCalculatrice.Multiplier(42, 0));
  AssertEquals('Multiplication négative', -12, FCalculatrice.Multiplier(-3, 4));
end;

procedure TTestCalculatrice.TestDivision;  
begin
  AssertEquals('Division simple', 2.5, FCalculatrice.Diviser(5, 2), 0.001);
  AssertEquals('Division entière', 3.0, FCalculatrice.Diviser(9, 3), 0.001);
end;

procedure TTestCalculatrice.TestDivisionParZero;  
begin
  // Vérifier qu'une exception est levée
  AssertException('Division par zéro doit lever une exception',
                  Exception,
                  @TestDivisionReelle);
end;

procedure TestDivisionReelle;  
var
  Calc: TCalculatrice;
begin
  Calc := TCalculatrice.Create;
  try
    Calc.Diviser(10, 0);  // Doit lever une exception
  finally
    Calc.Free;
  end;
end;

initialization
  // Enregistrer la classe de test
  RegisterTest(TTestCalculatrice);

end.
```

## Les méthodes d'assertion principales

FPCUnit fournit de nombreuses méthodes pour vérifier les résultats. Voici les plus courantes :

### Assertions de base

```pascal
// Vérifier l'égalité
AssertEquals('Message', ValeurAttendue, ValeurObtenue);

// Vérifier qu'une condition est vraie
AssertTrue('Message', Condition);

// Vérifier qu'une condition est fausse
AssertFalse('Message', Condition);

// Vérifier qu'un pointeur est nil
AssertNull('Message', Pointeur);

// Vérifier qu'un pointeur n'est pas nil
AssertNotNull('Message', Pointeur);

// Vérifier que deux objets sont identiques (même référence)
AssertSame('Message', Objet1, Objet2);
```

### Assertions numériques avec tolérance

Pour les nombres à virgule flottante, utilisez une tolérance :

```pascal
// Vérifier l'égalité avec marge d'erreur
AssertEquals('Message', 3.14159, Pi, 0.00001);
```

### Assertions d'exceptions

```pascal
// Vérifier qu'une exception est levée
AssertException('Message', TypeException, @MaProcedure);

// Vérifier qu'aucune exception n'est levée
try
  MaFonction();
  // Succès : aucune exception
except
  Fail('Une exception inattendue a été levée');
end;
```

### Échec manuel

```pascal
// Forcer l'échec d'un test
Fail('Ce test a échoué pour cette raison');
```

## Méthodes SetUp et TearDown

### SetUp : initialisation avant chaque test

La méthode `SetUp` est appelée automatiquement **avant chaque test**. Utilisez-la pour :

- Créer les objets nécessaires
- Initialiser les données de test
- Configurer l'environnement

```pascal
procedure TTestCalculatrice.SetUp;  
begin
  FCalculatrice := TCalculatrice.Create;
  // Autres initialisations...
end;
```

### TearDown : nettoyage après chaque test

La méthode `TearDown` est appelée automatiquement **après chaque test**. Utilisez-la pour :

- Libérer les objets créés
- Fermer les connexions
- Nettoyer les ressources

```pascal
procedure TTestCalculatrice.TearDown;  
begin
  FCalculatrice.Free;
  // Autres nettoyages...
end;
```

### Pourquoi SetUp/TearDown ?

Ces méthodes garantissent que **chaque test démarre dans un état propre et prévisible**. Sans elles, un test pourrait être affecté par les effets de bord d'un test précédent.

## Organisation des tests

### Suites de tests

Vous pouvez regrouper plusieurs classes de tests en suites :

```pascal
initialization
  // Enregistrer plusieurs tests
  RegisterTest(TTestCalculatrice);
  RegisterTest(TTestConversions);
  RegisterTest(TTestStatistiques);
end.
```

### Nommage des tests

Adoptez une convention de nommage claire :

```pascal
// Format : Test + Méthode + Scénario
procedure TestAddition_NombresPositifs;  
procedure TestAddition_NombresNegatifs;  
procedure TestAddition_AvecZero;  
procedure TestDivision_ParZero_LeveException;
```

### Tests avec données multiples

Pour tester plusieurs cas similaires :

```pascal
procedure TTestCalculatrice.TestAddition_PlusieursCas;  
begin
  AssertEquals('Cas 1', 5, FCalculatrice.Additionner(2, 3));
  AssertEquals('Cas 2', 0, FCalculatrice.Additionner(-5, 5));
  AssertEquals('Cas 3', 100, FCalculatrice.Additionner(50, 50));
  AssertEquals('Cas 4', -10, FCalculatrice.Additionner(-7, -3));
end;
```

## Exécution des tests

### En mode console

Compilez et exécutez votre projet de tests. Vous verrez une sortie comme :

```
Tests de Mon Projet
====================
TTestCalculatrice
  TestAddition: OK
  TestSoustraction: OK
  TestMultiplication: OK
  TestDivision: OK
  TestDivisionParZero: OK

Tests exécutés: 5  
Succès: 5
Échecs: 0
Erreurs: 0

Temps total: 0.023s
```

### En mode GUI (interface graphique)

Vous pouvez également utiliser `guirunner` pour une interface visuelle :

1. Ajoutez le paquet `fpcunitguitestrunner.lpk`
2. Changez l'unité de `consoletestrunner` à `guitestrunner`
3. Vous obtiendrez une fenêtre avec une arborescence de tests

### Exécution sélective

Vous pouvez exécuter uniquement certains tests en ligne de commande :

```bash
# Windows
MonProjetTests.exe --suite=TTestCalculatrice

# Linux
./MonProjetTests --suite=TTestCalculatrice
```

## Bonnes pratiques

### 1. Un test = une vérification

Chaque test doit vérifier **un seul comportement** :

```pascal
// ✅ BON : test ciblé
procedure TestAddition_NombresPositifs;  
begin
  AssertEquals(5, FCalculatrice.Additionner(2, 3));
end;

// ❌ MAUVAIS : teste trop de choses
procedure TestTout;  
begin
  AssertEquals(5, FCalculatrice.Additionner(2, 3));
  AssertEquals(2, FCalculatrice.Soustraire(5, 3));
  AssertEquals(15, FCalculatrice.Multiplier(3, 5));
end;
```

### 2. Tests indépendants

Les tests ne doivent **jamais dépendre les uns des autres** :

```pascal
// ❌ MAUVAIS : dépendance entre tests
var
  ResultatGlobal: Integer;

procedure Test1;  
begin
  ResultatGlobal := FCalculatrice.Additionner(2, 3);
end;

procedure Test2;  
begin
  // Dépend de Test1 !
  AssertEquals(10, ResultatGlobal + 5);
end;
```

### 3. Messages d'assertion clairs

Donnez toujours un message descriptif aux assertions :

```pascal
// ✅ BON
AssertEquals('L''addition de 2 et 3 doit donner 5', 5, FCalculatrice.Additionner(2, 3));

// ❌ MAUVAIS
AssertEquals(5, FCalculatrice.Additionner(2, 3));
```

### 4. AAA : Arrange, Act, Assert

Structurez vos tests en trois parties :

```pascal
procedure TestAddition;  
var
  Resultat: Integer;
begin
  // Arrange : préparer les données
  FCalculatrice := TCalculatrice.Create;

  // Act : exécuter l'action
  Resultat := FCalculatrice.Additionner(2, 3);

  // Assert : vérifier le résultat
  AssertEquals('Addition incorrecte', 5, Resultat);
end;
```

### 5. Tester les cas limites

N'oubliez pas de tester :

- Les valeurs nulles ou vides
- Les valeurs extrêmes (maximum, minimum)
- Les cas d'erreur
- Les conditions limites

```pascal
procedure TestAddition_CasLimites;  
begin
  // Valeur maximale
  AssertEquals(MaxInt, FCalculatrice.Additionner(MaxInt, 0));

  // Valeur minimale
  AssertEquals(-MaxInt, FCalculatrice.Additionner(-MaxInt, 0));

  // Zéro
  AssertEquals(0, FCalculatrice.Additionner(0, 0));
end;
```

## Tests multi-plateformes (Windows/Ubuntu)

### Gestion des différences de plateforme

Lorsque votre code dépend de la plateforme, utilisez des directives conditionnelles :

```pascal
procedure TTestFichiers.TestCheminFichier;  
var
  CheminAttendu: String;
begin
  {$IFDEF WINDOWS}
  CheminAttendu := 'C:\Users\Test\fichier.txt';
  {$ENDIF}
  {$IFDEF UNIX}
  CheminAttendu := '/home/test/fichier.txt';
  {$ENDIF}

  AssertEquals('Chemin incorrect', CheminAttendu, ObtenirChemin());
end;
```

### Tests de compatibilité

Créez des tests spécifiques pour vérifier la compatibilité multi-plateforme :

```pascal
procedure TTestCompatibilite.TestSeparateurChemin;  
begin
  {$IFDEF WINDOWS}
  AssertEquals('\', PathDelim);
  {$ENDIF}
  {$IFDEF UNIX}
  AssertEquals('/', PathDelim);
  {$ENDIF}
end;
```

## Intégration continue (CI/CD)

### Automatisation des tests

FPCUnit s'intègre facilement dans les pipelines CI/CD :

```yaml
# Exemple GitHub Actions
name: Tests

on: [push, pull_request]

jobs:
  test-windows:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v2
      - name: Installer FreePascal
        run: choco install freepascal
      - name: Compiler les tests
        run: lazbuild MonProjetTests.lpi
      - name: Exécuter les tests
        run: ./MonProjetTests.exe --format=plain

  test-ubuntu:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Installer FreePascal
        run: sudo apt-get install fp-compiler lazarus
      - name: Compiler les tests
        run: lazbuild MonProjetTests.lpi
      - name: Exécuter les tests
        run: ./MonProjetTests --format=plain
```

### Format de sortie

FPCUnit supporte plusieurs formats de sortie pour l'intégration CI :

```bash
# Format XML (pour Jenkins, etc.)
./MonProjetTests --format=xml --file=resultats.xml

# Format plain (lisible par l'humain)
./MonProjetTests --format=plain

# Format latex
./MonProjetTests --format=latex --file=rapport.tex
```

## Couverture de code

### Activer la couverture avec FPC

Pour mesurer la couverture de code :

#### Sur Windows

```batch
REM Compiler avec les options de couverture  
fpc -B -gh -gl MonProjet.pas

REM Exécuter les tests  
MonProjetTests.exe

REM Générer le rapport (nécessite des outils tiers)
```

#### Sur Ubuntu/Linux

```bash
# Compiler avec gcov
fpc -B -Cg -gl MonProjet.pas

# Exécuter les tests
./MonProjetTests

# Générer le rapport avec lcov
lcov --capture --directory . --output-file coverage.info  
genhtml coverage.info --output-directory coverage_report
```

### Objectifs de couverture

Visez une couverture de code appropriée :

- **Code critique** : 90-100%
- **Code métier** : 70-90%
- **Code utilitaire** : 50-70%
- **Code d'interface** : peut être plus faible

## Debugging des tests

### Localiser les échecs

Quand un test échoue, FPCUnit fournit des informations détaillées :

```
ÉCHEC: TTestCalculatrice.TestDivision
  Attendu: <2.5>
  Obtenu: <2.0>
  Message: Division incorrecte
  Fichier: TestCalculatrice.pas
  Ligne: 84
```

### Utiliser le débogueur

Vous pouvez déboguer vos tests comme n'importe quel programme :

1. Placez un point d'arrêt dans votre test
2. Lancez en mode debug (F9 dans Lazarus)
3. Inspectez les variables et l'état

### Isoler un test problématique

Pour exécuter uniquement un test spécifique :

```pascal
// Temporairement, commentez les autres tests
procedure TTestCalculatrice.SetUp;  
begin
  FCalculatrice := TCalculatrice.Create;
end;

{
procedure TestAddition;  
procedure TestSoustraction;
}

procedure TestMultiplication;  // Seulement celui-ci  
begin
  // ...
end;
```

## Ressources complémentaires

### Documentation officielle

- **Wiki FreePascal** : https://wiki.freepascal.org/fpcunit
- **Documentation FPC** : Documentation intégrée dans FreePascal

### Packages et outils complémentaires

- **FPCUnit GUI Runner** : Interface graphique pour les tests
- **Lazarus Code Coverage** : Outil de couverture de code
- **TestInsight** : Plugin Lazarus pour exécution rapide des tests

### Communauté

- **Forum Lazarus** : https://forum.lazarus.freepascal.org
- **Forum FreePascal** : https://forum.freepascal.org
- **Stack Overflow** : Tag `freepascal` et `lazarus`

## Conclusion

FPCUnit est un outil puissant et simple pour améliorer la qualité de vos applications FreePascal/Lazarus. En adoptant les tests unitaires dès le début de vos projets, vous économiserez du temps à long terme et développerez un code plus robuste et maintenable.

Les tests unitaires sont un investissement qui paie rapidement, particulièrement dans le contexte multi-plateforme où ils garantissent que votre code fonctionne correctement sur Windows et Ubuntu.

**Points clés à retenir :**
- Écrivez des tests simples, indépendants et rapides
- Utilisez SetUp/TearDown pour garantir l'isolation des tests
- Testez les cas normaux, limites et d'erreur
- Intégrez les tests dans votre workflow quotidien
- Automatisez l'exécution avec CI/CD

Avec FPCUnit, vous disposez de tout ce qu'il faut pour développer des applications de qualité professionnelle !

⏭️ [Tests d'intégration automatisés](/18-tests-qualite-code/02-tests-integration-automatises.md)
