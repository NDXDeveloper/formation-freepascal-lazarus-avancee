🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 26.4 Documentation et tutoriels

## Introduction

La documentation est l'un des aspects les plus importants mais souvent négligés du développement logiciel. Une bonne documentation fait la différence entre un projet utilisé et apprécié, et un projet abandonné faute de compréhension. Ce chapitre vous guidera dans la création de documentation et tutoriels de qualité professionnelle pour vos projets FreePascal/Lazarus.

### Pourquoi la documentation est-elle importante ?

**Pour les utilisateurs :**
- Apprendre à utiliser votre code rapidement
- Comprendre les fonctionnalités disponibles
- Résoudre les problèmes courants
- Découvrir les meilleures pratiques
- Gagner du temps et éviter la frustration

**Pour vous :**
- Réduire les questions répétitives
- Faciliter la maintenance future (vous oubliez votre propre code !)
- Attirer plus d'utilisateurs et contributeurs
- Professionnaliser votre projet
- Créer une base de connaissances réutilisable

**Pour la communauté :**
- Enrichir l'écosystème FreePascal/Lazarus
- Partager les connaissances
- Établir des standards de qualité
- Faciliter l'adoption par les nouveaux développeurs

### Types de documentation

Il existe plusieurs types de documentation, chacun avec un objectif spécifique :

1. **Documentation API (référence)** : Documentation technique du code
2. **Guide utilisateur** : Comment utiliser le logiciel/bibliothèque
3. **Tutoriels** : Apprentissage pas-à-pas
4. **Guide de démarrage rapide** : Premiers pas rapides
5. **FAQ** : Questions fréquentes
6. **Exemples de code** : Cas d'usage concrets
7. **Documentation d'architecture** : Vue d'ensemble technique
8. **Changelog** : Historique des modifications

## Documentation du code source

### Commentaires dans le code

Les commentaires sont la base de toute documentation. Un bon commentaire explique le **pourquoi**, pas seulement le **quoi**.

**Mauvais commentaires :**

```pascal
// ❌ Incrémente i
i := i + 1;

// ❌ Boucle sur les éléments
for j := 0 to List.Count - 1 do
  ProcessItem(List[j]);

// ❌ Retourne le résultat
Result := x + y;
```

**Bons commentaires :**

```pascal
// ✅ Explique POURQUOI, pas QUOI
// Incrémenter avant utilisation car l'index est base-1 dans cette API
i := i + 1;

// ✅ Explique le contexte
// Parcourir en ordre inverse pour éviter les problèmes de
// suppression pendant l'itération
for j := List.Count - 1 downto 0 do
  if ShouldRemove(List[j]) then
    List.Delete(j);

// ✅ Documente les cas particuliers
// Retourne -1 si la valeur n'est pas trouvée,
// sinon l'index de la première occurrence
function FindValue(AValue: Integer): Integer;
```

**Quand commenter :**

✅ **Commenter :**
- Algorithmes complexes
- Décisions de design non évidentes
- Workarounds et hacks
- Paramètres et valeurs de retour non évidentes
- Code susceptible de surprendre
- TODOs et FIXMEs

❌ **Ne pas commenter :**
- Code évident
- Redondance avec le nom de la fonction
- Code qui devrait être réécrit plutôt que commenté

### Documentation PasDoc

**PasDoc** est le générateur de documentation standard pour Pascal, similaire à JavaDoc ou Doxygen.

#### Installation de PasDoc

**Linux :**
```bash
sudo apt install pasdoc
```

**Windows :**
- Télécharger depuis https://pasdoc.github.io/
- Extraire et ajouter au PATH

**Vérification :**
```bash
pasdoc --version
```

#### Syntaxe PasDoc de base

**Documentation d'une unité :**

```pascal
{**
  Cette unité fournit des utilitaires pour manipuler des chaînes.

  @author(Votre Nom <email@example.com>)
  @created(15 Janvier 2024)
  @lastmod(20 Janvier 2024)
}
unit StringHelpers;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

type
  TStringArray = array of string;

  {**
    Classe helper pour étendre TString avec des méthodes utilitaires.

    Cette classe fournit des méthodes pour manipuler les chaînes de
    caractères de manière plus intuitive et puissante.

    @bold(Exemple d'utilisation :)
    @longcode(#
    var
      S: string;
    begin
      S := 'Hello World';
      if TStringHelper.Contains(S, 'World') then
        WriteLn('Found!');
    end;
    #)
  }
  TStringHelper = class
  public
    {**
      Vérifie si une chaîne est vide ou ne contient que des espaces.

      @param(AText Chaîne à vérifier)
      @returns(True si la chaîne est vide ou contient seulement des espaces)

      @bold(Exemple :)
      @code(
      if TStringHelper.IsBlank('   ') then
        WriteLn('String is blank');
      )
    }
    class function IsBlank(const AText: string): Boolean;

    {**
      Divise une chaîne selon un délimiteur.

      @param(AText Chaîne à diviser)
      @param(ADelimiter Caractère délimiteur)
      @returns(Tableau de chaînes résultant de la division)
      @raises(EArgumentException Si ADelimiter est vide)

      @bold(Exemple :)
      @longcode(#
      var
        Parts: TStringArray;
      begin
        Parts := TStringHelper.Split('a,b,c', ',');
        // Parts[0] = 'a', Parts[1] = 'b', Parts[2] = 'c'
      end;
      #)
    }
    class function Split(const AText: string;
                        ADelimiter: Char): TStringArray;
  end;

implementation

class function TStringHelper.IsBlank(const AText: string): Boolean;  
begin
  Result := Trim(AText) = '';
end;

class function TStringHelper.Split(const AText: string;
                                  ADelimiter: Char): TStringArray;
var
  i, Count, Start: Integer;
begin
  // Compter le nombre de parties
  Count := 1;
  for i := 1 to Length(AText) do
    if AText[i] = ADelimiter then
      Inc(Count);

  // Allouer le tableau
  SetLength(Result, Count);

  // Remplir le tableau
  Count := 0;
  Start := 1;
  for i := 1 to Length(AText) do
  begin
    if AText[i] = ADelimiter then
    begin
      Result[Count] := Copy(AText, Start, i - Start);
      Inc(Count);
      Start := i + 1;
    end;
  end;

  // Dernier élément
  Result[Count] := Copy(AText, Start, Length(AText) - Start + 1);
end;

end.
```

#### Tags PasDoc courants

**Tags de description :**

| Tag | Usage | Exemple |
|-----|-------|---------|
| `@param(nom Description)` | Paramètre | `@param(AValue Valeur à traiter)` |
| `@returns(Description)` | Valeur de retour | `@returns(True si succès)` |
| `@raises(Exception Description)` | Exception levée | `@raises(EInvalidOp Si valeur invalide)` |
| `@see(Reference)` | Référence croisée | `@see(TMyOtherClass)` |
| `@seealso(Reference)` | Voir aussi | `@seealso(RelatedFunction)` |
| `@author(Nom)` | Auteur | `@author(Jean Dupont)` |
| `@created(Date)` | Date de création | `@created(2024-01-15)` |
| `@lastmod(Date)` | Dernière modification | `@lastmod(2024-01-20)` |
| `@deprecated(Message)` | Obsolète | `@deprecated(Use NewMethod)` |

**Tags de formatage :**

| Tag | Rendu | Exemple |
|-----|-------|---------|
| `@bold(texte)` | **Gras** | `@bold(Important!)` |
| `@italic(texte)` | *Italique* | `@italic(Note:)` |
| `@code(texte)` | `Code inline` | `@code(var x: Integer)` |
| `@longcode(#...#)` | Bloc de code | Voir exemple ci-dessus |
| `@link(url texte)` | Lien | `@link(http://example.com Site)` |
| `@html(code)` | HTML brut | `@html(<br/>)` |

**Tags de structure :**

```pascal
{**
  Description principale.

  @section(Introduction)
  Paragraphe d'introduction...

  @section(Utilisation)
  Comment utiliser cette classe...

  @section(Exemples)
  @longcode(#
  // Code exemple
  #)

  @section(Notes)
  Notes importantes...
}
```

#### Générer la documentation

**Commande de base :**

```bash
pasdoc \
  --format html \
  --output docs/html \
  --title "Ma Bibliothèque" \
  --source src/*.pas
```

**Options avancées :**

```bash
pasdoc \
  --format html \
  --output docs/html \
  --title "StringHelpers Library" \
  --header "String manipulation utilities" \
  --footer "© 2024 - Licensed under MIT" \
  --introduction docs/intro.txt \
  --conclusion docs/conclusion.txt \
  --css custom.css \
  --language en \
  --verbosity 2 \
  --source src/*.pas \
  --exclude src/internal/*.pas
```

**Fichier de configuration (pasdoc.cfg) :**

```ini
--format=html
--output=docs/html
--title=Ma Bibliothèque
--source=src/*.pas
--exclude=src/tests/*.pas
--introduction=docs/intro.txt
--css=docs/custom.css
--auto-link
--auto-abstract
--marker=///
--staronly
--write-uses-list
```

**Utiliser la configuration :**

```bash
pasdoc @pasdoc.cfg
```

#### Personnaliser l'apparence

**CSS personnalisé (custom.css) :**

```css
/* Style général */
body {
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  line-height: 1.6;
  color: #333;
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

/* Titres */
h1, h2, h3 {
  color: #2c3e50;
  border-bottom: 2px solid #3498db;
  padding-bottom: 10px;
}

/* Blocs de code */
pre.longcode {
  background-color: #f4f4f4;
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 15px;
  overflow-x: auto;
  font-family: 'Courier New', monospace;
  font-size: 14px;
}

/* Code inline */
code {
  background-color: #f0f0f0;
  padding: 2px 6px;
  border-radius: 3px;
  font-family: 'Courier New', monospace;
}

/* Tableaux */
table {
  border-collapse: collapse;
  width: 100%;
  margin: 20px 0;
}

table th {
  background-color: #3498db;
  color: white;
  padding: 12px;
  text-align: left;
}

table td {
  border: 1px solid #ddd;
  padding: 10px;
}

table tr:nth-child(even) {
  background-color: #f9f9f9;
}

/* Liens */
a {
  color: #3498db;
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

/* Avertissements */
.warning {
  background-color: #fff3cd;
  border-left: 4px solid #ffc107;
  padding: 15px;
  margin: 20px 0;
}

.deprecated {
  background-color: #f8d7da;
  border-left: 4px solid #dc3545;
  padding: 15px;
  margin: 20px 0;
}
```

## Guide utilisateur

### Structure d'un bon guide

Un guide utilisateur complet devrait contenir :

1. **Introduction**
   - Qu'est-ce que le logiciel/bibliothèque ?
   - À qui s'adresse-t-il ?
   - Prérequis

2. **Installation**
   - Étapes détaillées par système d'exploitation
   - Dépendances
   - Vérification de l'installation

3. **Démarrage rapide**
   - Premier exemple fonctionnel en 5 minutes
   - Cas d'usage minimal

4. **Concepts fondamentaux**
   - Architecture générale
   - Terminologie
   - Principes de base

5. **Guide détaillé**
   - Fonctionnalités une par une
   - Configuration avancée
   - Cas d'usage courants

6. **Référence**
   - API complète
   - Options et paramètres
   - Configuration

7. **Résolution de problèmes**
   - Problèmes courants et solutions
   - FAQ
   - Où obtenir de l'aide

8. **Annexes**
   - Glossaire
   - Ressources supplémentaires
   - Licence

### Exemple de guide utilisateur

**docs/user-guide.md :**

````markdown
# Guide Utilisateur - StringHelpers Library

## Introduction

StringHelpers est une bibliothèque Pascal qui étend les capacités  
de manipulation de chaînes de caractères avec des méthodes intuitives  
et puissantes.

### Public cible

- Développeurs FreePascal/Lazarus
- Niveau : Débutant à avancé
- Connaissances requises : Base de Pascal

### Prérequis

- FreePascal 3.2.0 ou supérieur
- Lazarus 2.0 ou supérieur (optionnel)

## Installation

### Via Online Package Manager

1. Ouvrir Lazarus
2. Menu : Package → Online Package Manager
3. Rechercher "StringHelpers"
4. Cliquer sur "Install"
5. Redémarrer Lazarus

### Installation manuelle

1. Télécharger depuis GitHub :
   ```bash
   git clone https://github.com/username/stringhelpers.git
   ```

2. Ouvrir le package :
   - Lazarus : Package → Open Package File
   - Ouvrir `stringhelpers.lpk`

3. Compiler :
   - Cliquer sur "Compile"

4. Ajouter à votre projet :
   - Project → Project Inspector → Add → Requirement
   - Sélectionner StringHelpers

### Vérification

Testez l'installation avec ce code :

```pascal
program TestInstall;  
uses
  StringHelpers;
begin
  if TStringHelper.IsBlank('   ') then
    WriteLn('Installation réussie!');
end.
```

## Démarrage rapide

Voici un exemple complet en 5 minutes :

```pascal
program QuickStart;

uses
  SysUtils, StringHelpers;

var
  Text: string;
  Parts: TStringArray;
  i: Integer;

begin
  // 1. Vérifier si une chaîne est vide
  Text := '   ';
  if TStringHelper.IsBlank(Text) then
    WriteLn('Chaîne vide');

  // 2. Diviser une chaîne
  Text := 'pomme,banane,cerise';
  Parts := TStringHelper.Split(Text, ',');
  for i := 0 to High(Parts) do
    WriteLn('Fruit ', i + 1, ': ', Parts[i]);

  // 3. Vérifier le contenu
  if TStringHelper.Contains(Text, 'banane') then
    WriteLn('Banane trouvée!');

  ReadLn;
end.
```

**Résultat attendu :**
````
Chaîne vide  
Fruit 1: pomme  
Fruit 2: banane  
Fruit 3: cerise  
Banane trouvée!
```

## Concepts fondamentaux

### Architecture

StringHelpers utilise des méthodes de classe (class methods) qui  
ne nécessitent pas d'instanciation :

```pascal
// ✅ Correct - Appel direct sur la classe
Result := TStringHelper.IsBlank(MyString);

// ❌ Incorrect - Pas besoin d'instancier
var
  Helper: TStringHelper;
begin
  Helper := TStringHelper.Create;  // Inutile!
```

### Principes

1. **Immutabilité** : Les méthodes ne modifient jamais la chaîne originale
2. **Thread-safe** : Toutes les méthodes sont sûres en multi-threading
3. **Performance** : Optimisé pour les cas d'usage courants

## Guide détaillé

### Vérification de contenu

#### IsBlank

Vérifie si une chaîne est vide ou ne contient que des espaces.

```pascal
function IsBlank(const AText: string): Boolean;
```

**Exemples :**

```pascal
TStringHelper.IsBlank('');        // True  
TStringHelper.IsBlank('   ');     // True  
TStringHelper.IsBlank('Hello');   // False  
TStringHelper.IsBlank('  x  ');   // False
```

**Cas d'usage :**

```pascal
// Valider une entrée utilisateur
if TStringHelper.IsBlank(EditNom.Text) then  
begin
  ShowMessage('Le nom est obligatoire');
  Exit;
end;
```

#### Contains

Vérifie si une sous-chaîne est présente.

```pascal
function Contains(const AText, ASubStr: string): Boolean;
```

**Exemples :**

```pascal
TStringHelper.Contains('Hello World', 'World');  // True  
TStringHelper.Contains('Hello World', 'world');  // False (sensible à la casse)  
TStringHelper.Contains('test', '');              // True (chaîne vide toujours trouvée)
```

### Manipulation de chaînes

#### Split

Divise une chaîne selon un délimiteur.

```pascal
function Split(const AText: string; ADelimiter: Char): TStringArray;
```

**Exemples :**

```pascal
var
  Parts: TStringArray;
begin
  // CSV simple
  Parts := TStringHelper.Split('a,b,c', ',');
  // Parts = ['a', 'b', 'c']

  // Chemin de fichier
  Parts := TStringHelper.Split('/home/user/file.txt', '/');
  // Parts = ['', 'home', 'user', 'file.txt']

  // Plusieurs délimiteurs consécutifs
  Parts := TStringHelper.Split('a,,c', ',');
  // Parts = ['a', '', 'c']  // Éléments vides préservés
end;
```

**Cas d'usage avancé :**

```pascal
// Parser un fichier CSV
procedure ParseCSV(const AFileName: string);  
var
  Lines: TStringList;
  Line: string;
  Fields: TStringArray;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);

    for Line in Lines do
    begin
      Fields := TStringHelper.Split(Line, ',');

      for i := 0 to High(Fields) do
        WriteLn('Champ ', i, ': ', Fields[i]);
    end;
  finally
    Lines.Free;
  end;
end;
```

## Résolution de problèmes

### Problème : "Unit StringHelpers not found"

**Cause :** Le package n'est pas dans le chemin de recherche.

**Solution :**

1. Vérifier que le package est installé
2. Ajouter le package aux dépendances du projet :
   - Project → Project Inspector → Add Requirement

### Problème : Access Violation avec Split

**Cause :** Tentative d'accéder à un élément inexistant du tableau.

**Solution :**

```pascal
// ❌ Mauvais
Parts := TStringHelper.Split(Text, ',');  
WriteLn(Parts[0]);  // Crash si Text est vide

// ✅ Bon
Parts := TStringHelper.Split(Text, ',');  
if Length(Parts) > 0 then
  WriteLn(Parts[0]);
```

### FAQ

**Q : Les méthodes sont-elles sensibles à la casse ?**

R : Oui, par défaut. Utilisez `UpperCase()` ou `LowerCase()` si nécessaire.

**Q : Comment gérer les caractères Unicode ?**

R : StringHelpers supporte complètement l'Unicode avec les chaînes UTF-8.

**Q : Y a-t-il un impact sur les performances ?**

R : Les méthodes sont optimisées. Pour des millions d'appels, mesurez avec votre
   cas d'usage spécifique.

### Obtenir de l'aide

- **Forum :** https://forum.lazarus.freepascal.org/
- **Issues GitHub :** https://github.com/username/stringhelpers/issues
- **Email :** support@example.com

## Annexes

### Glossaire

- **Délimiteur** : Caractère utilisé pour séparer des éléments
- **Immutabilité** : Propriété d'un objet qui ne peut pas être modifié
- **Thread-safe** : Code qui fonctionne correctement en multi-threading

### Ressources

- [Documentation API complète](api/index.html)
- [Exemples GitHub](https://github.com/username/stringhelpers/tree/main/examples)
- [Wiki communautaire](https://wiki.example.com/stringhelpers)

### Licence

MIT License - Libre d'utilisation commerciale et personnelle.
```

## Créer des tutoriels

### Principes d'un bon tutoriel

Un bon tutoriel doit :

1. **Avoir un objectif clair** : "Apprendre à créer une application de chat"
2. **Être progressif** : Commencer simple, augmenter la difficulté
3. **Être complet** : Tout le code nécessaire, pas de sauts
4. **Être testé** : Vérifier que ça fonctionne réellement
5. **Être à jour** : Versions actuelles des outils
6. **Avoir des captures d'écran** : Illustrations visuelles

### Structure type d'un tutoriel

```markdown
# Titre du tutoriel

## Introduction

### Ce que vous allez apprendre
- Point 1
- Point 2

### Prérequis
- Connaissance X
- Outil Y installé

### Durée estimée
30 minutes

## Étape 1 : Configuration

Description de l'étape...

**Action :**
1. Faire ceci
2. Puis cela

**Code :**
```pascal
// Code de l'étape
```

**Résultat attendu :**
Capture d'écran ou description

**Explication :**
Pourquoi on fait ça, comment ça marche

## Étape 2 : ...

[Répéter la structure]

## Conclusion

### Récapitulatif
Ce que vous avez appris...

### Prochaines étapes
- Amélioration possible 1
- Amélioration possible 2

### Ressources
- Lien 1
- Lien 2
```

### Exemple de tutoriel complet

**Tutoriel : Créer une application calculatrice**

```markdown
# Créer une calculatrice avec Lazarus

## Introduction

Dans ce tutoriel, nous allons créer une calculatrice simple mais  
fonctionnelle avec Lazarus. Cette application illustrera les concepts  
de base de la programmation d'interface graphique.

### Ce que vous allez apprendre

- Créer un projet Lazarus
- Utiliser les composants de base (TButton, TEdit, TLabel)
- Gérer les événements de clic
- Implémenter la logique d'une calculatrice

### Prérequis

- Lazarus 2.0+ installé
- Connaissance de base de Pascal
- Compréhension des formulaires Windows

### Durée estimée

45 minutes

## Étape 1 : Créer le projet

**Action :**

1. Lancer Lazarus
2. Menu : **Project → New Project**
3. Sélectionner **Application**
4. Cliquer **OK**
5. Sauvegarder immédiatement :
   - Project : `calculator.lpi`
   - Unit principale : `main.pas`

**Résultat attendu :**

Vous devriez voir un formulaire vide (Form1) et le code de l'unit principale.

![Nouveau projet](images/new_project.png)

## Étape 2 : Designer l'interface

Nous allons créer une interface de calculatrice classique.

**Action :**

1. **Ajouter un TEdit pour l'affichage :**
   - Palette : Standard
   - Composant : TEdit
   - Placer en haut du formulaire
   - Propriétés :
     - Name : `EditDisplay`
     - Text : `0`
     - Alignment : taRightJustify
     - ReadOnly : True
     - Font → Size : 16

2. **Ajouter les boutons chiffres (0-9) :**
   - Composant : TButton (x10)
   - Noms : `Btn0`, `Btn1`, ..., `Btn9`
   - Caption : '0', '1', ..., '9'
   - Organiser en grille 3x4

3. **Ajouter les boutons opérateurs :**
   - `BtnPlus` (Caption: '+')
   - `BtnMinus` (Caption: '-')
   - `BtnMultiply` (Caption: '×')
   - `BtnDivide` (Caption: '÷')
   - `BtnEquals` (Caption: '=')
   - `BtnClear` (Caption: 'C')

**Résultat attendu :**

![Interface calculatrice](images/calculator_interface.png)

**Disposition suggérée :**
```
┌─────────────────┐  
│ EditDisplay     │  
├─────┬─────┬─────┤  
│  7  │  8  │  9  │ ÷  
├─────┼─────┼─────┤  
│  4  │  5  │  6  │ ×  
├─────┼─────┼─────┤  
│  1  │  2  │  3  │ -  
├─────┼─────┼─────┤  
│  0  │  C  │  =  │ +  
└─────┴─────┴─────┘
```

## Étape 3 : Implémenter la logique

**Action :**

1. **Ajouter des variables dans la classe TForm1 :**

```pascal
type
  TForm1 = class(TForm)
    // ... composants déclarés par Lazarus
  private
    FCurrentValue: Double;
    FPreviousValue: Double;
    FOperation: Char;
    FNewNumber: Boolean;

    procedure ClearCalculator;
    procedure AppendDigit(ADigit: Char);
    procedure SetOperation(AOp: Char);
    procedure Calculate;
  public
    constructor Create(AOwner: TComponent); override;
  end;
```

2. **Initialiser dans le constructeur :**

```pascal
constructor TForm1.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  ClearCalculator;
end;

procedure TForm1.ClearCalculator;  
begin
  FCurrentValue := 0;
  FPreviousValue := 0;
  FOperation := #0;
  FNewNumber := True;
  EditDisplay.Text := '0';
end;
```

3. **Implémenter l'ajout de chiffres :**

```pascal
procedure TForm1.AppendDigit(ADigit: Char);  
begin
  if FNewNumber then
  begin
    EditDisplay.Text := ADigit;
    FNewNumber := False;
  end
  else
  begin
    if EditDisplay.Text = '0' then
      EditDisplay.Text := ADigit
    else
      EditDisplay.Text := EditDisplay.Text + ADigit;
  end;
end;
```

4. **Connecter les événements de clic des chiffres :**

Double-cliquer sur Btn1 et ajouter :

```pascal
procedure TForm1.Btn1Click(Sender: TObject);  
begin
  AppendDigit('1');
end;
```

Répéter pour tous les boutons chiffres.

**Astuce :** Pour éviter la répétition, utiliser le Tag :

```pascal
// Dans l'Object Inspector, définir Tag de chaque bouton
// Btn0.Tag := 0, Btn1.Tag := 1, etc.

// Puis une seule procédure pour tous :
procedure TForm1.BtnDigitClick(Sender: TObject);  
begin
  AppendDigit(Chr(Ord('0') + (Sender as TButton).Tag));
end;

// Et affecter BtnDigitClick à OnClick de tous les boutons chiffres
```

## Étape 4 : Opérations mathématiques

**Action :**

1. **Implémenter SetOperation :**

```pascal
procedure TForm1.SetOperation(AOp: Char);  
begin
  if not FNewNumber then
  begin
    Calculate;  // Calculer l'opération précédente si elle existe
  end;

  FPreviousValue := StrToFloatDef(EditDisplay.Text, 0);
  FOperation := AOp;
  FNewNumber := True;
end;
```

2. **Implémenter Calculate :**

```pascal
procedure TForm1.Calculate;  
var
  Result: Double;
begin
  FCurrentValue := StrToFloatDef(EditDisplay.Text, 0);

  case FOperation of
    '+': Result := FPreviousValue + FCurrentValue;
    '-': Result := FPreviousValue - FCurrentValue;
    '*', '×': Result := FPreviousValue * FCurrentValue;
    '/', '÷':
      begin
        if FCurrentValue <> 0 then
          Result := FPreviousValue / FCurrentValue
        else
        begin
          ShowMessage('Division par zéro impossible');
          ClearCalculator;
          Exit;
        end;
      end;
  else
    Result := FCurrentValue;
  end;

  EditDisplay.Text := FloatToStr(Result);
  FPreviousValue := Result;
  FNewNumber := True;
end;
```

3. **Connecter les boutons opérateurs :**

```pascal
procedure TForm1.BtnPlusClick(Sender: TObject);  
begin
  SetOperation('+');
end;

procedure TForm1.BtnMinusClick(Sender: TObject);  
begin
  SetOperation('-');
end;

procedure TForm1.BtnMultiplyClick(Sender: TObject);  
begin
  SetOperation('×');
end;

procedure TForm1.BtnDivideClick(Sender: TObject);  
begin
  SetOperation('÷');
end;

procedure TForm1.BtnEqualsClick(Sender: TObject);  
begin
  Calculate;
  FOperation := #0;
end;

procedure TForm1.BtnClearClick(Sender: TObject);  
begin
  ClearCalculator;
end;
```

## Étape 5 : Améliorer l'interface

**Action :**

1. **Ajouter le support du point décimal :**

```pascal
// Ajouter un bouton BtnDecimal (Caption: '.')

procedure TForm1.BtnDecimalClick(Sender: TObject);  
begin
  if Pos('.', EditDisplay.Text) = 0 then
  begin
    if FNewNumber then
    begin
      EditDisplay.Text := '0.';
      FNewNumber := False;
    end
    else
      EditDisplay.Text := EditDisplay.Text + '.';
  end;
end;
```

2. **Améliorer l'affichage des nombres :**

```pascal
// Dans Calculate, remplacer :
EditDisplay.Text := FloatToStr(Result);

// Par :
EditDisplay.Text := FloatToStrF(Result, ffGeneral, 10, 2);
```

3. **Ajouter le support du clavier :**

```pascal
procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);  
begin
  case Key of
    '0'..'9': AppendDigit(Key);
    '+', '-', '*', '/': SetOperation(Key);
    '=', #13: Calculate;  // = ou Entrée
    'c', 'C': ClearCalculator;
    '.', ',': BtnDecimalClick(nil);
  end;
end;

// Dans l'Object Inspector :
// Form1.KeyPreview := True
// Form1.OnKeyPress := FormKeyPress
```

## Étape 6 : Tester

**Actions de test :**

1. **Test basique :**
   - Cliquer 2, +, 3, =
   - Résultat attendu : 5

2. **Test chaîne d'opérations :**
   - Cliquer 10, +, 5, -, 3, =
   - Résultat attendu : 12

3. **Test division par zéro :**
   - Cliquer 5, ÷, 0, =
   - Résultat attendu : Message d'erreur

4. **Test décimal :**
   - Cliquer 3, ., 5, +, 1, ., 5, =
   - Résultat attendu : 5

5. **Test clear :**
   - Cliquer des opérations, puis C
   - Résultat attendu : Affichage à 0

## Conclusion

### Récapitulatif

Vous avez appris à :

✅ Créer une interface graphique avec Lazarus
✅ Gérer les événements de clic
✅ Implémenter une logique applicative
✅ Gérer les entrées clavier
✅ Afficher des messages d'erreur
✅ Tester une application

### Code complet

Le code source complet est disponible sur GitHub :  
https://github.com/username/lazarus-calculator

### Améliorations possibles

Vous pouvez améliorer cette calculatrice avec :

- **Historique** : Afficher les opérations précédentes
- **Mémoire** : Boutons M+, M-, MR, MC
- **Fonctions scientifiques** : sin, cos, sqrt, etc.
- **Gestion du copier/coller** : Ctrl+C, Ctrl+V
- **Thèmes** : Interface personnalisable
- **Multi-opérations** : (2+3)×5

### Ressources

- [Documentation LCL](https://lazarus-ccr.sourceforge.io/docs/lcl/)
- [Tutoriels Lazarus](https://wiki.lazarus.freepascal.org/Tutorials)
- [Forum Lazarus](https://forum.lazarus.freepascal.org/)

### Prochains tutoriels

- Créer une application de gestion de contacts
- Connecter à une base de données SQLite
- Développer un jeu simple avec BGRABitmap
```

## Formats de documentation

### Markdown

**Avantages :**
- Simple et lisible
- Supporté par GitHub/GitLab
- Convertible en HTML, PDF, etc.
- Édition facile

**Syntaxe de base :**

```markdown
# Titre niveau 1
## Titre niveau 2
### Titre niveau 3

**Gras** et *italique*

- Liste à puces
- Deuxième élément

1. Liste numérotée
2. Deuxième élément

[Lien](https://example.com)

![Image](image.png)

`Code inline`

```pascal
// Bloc de code
procedure Example;  
begin
  WriteLn('Hello');
end;
```

> Citation

| Colonne 1 | Colonne 2 |
|-----------|-----------|
| Valeur 1  | Valeur 2  |
```

### reStructuredText (RST)

Utilisé par la documentation FreePascal officielle.

**Exemple :**

```rst
Documentation Title
===================

Section
-------

This is a paragraph with **bold** and *italic* text.

- Bullet list
- Second item

.. code-block:: pascal

   procedure Example;
   begin
     WriteLn('Hello');
   end;

.. note::
   This is a note.

.. warning::
   This is a warning.
```

### AsciiDoc

Alternative plus puissante à Markdown.

**Exemple :**

```asciidoc
= Document Title

== Section

This is *bold* and _italic_.

[source,pascal]
----
procedure Example;  
begin
  WriteLn('Hello');
end;
----

TIP: This is a tip.

WARNING: This is a warning.
```

## Outils de documentation

### Documentation generators

**PasDoc :**
- Spécifique Pascal
- Format JavaDoc-like
- Génère HTML, LaTeX, etc.

**Doxygen :**
- Multi-langage (supporte Pascal)
- Très complet
- Génère HTML, PDF, etc.

**Installation Doxygen :**

```bash
# Linux
sudo apt install doxygen graphviz

# Windows
# Télécharger depuis doxygen.org
```

**Fichier Doxyfile :**

```
PROJECT_NAME           = "Ma Bibliothèque"  
OUTPUT_DIRECTORY       = docs  
INPUT                  = src  
FILE_PATTERNS          = *.pas *.pp *.inc  
RECURSIVE              = YES  
EXTRACT_ALL            = YES  
OPTIMIZE_OUTPUT_JAVA   = YES  
GENERATE_HTML          = YES  
GENERATE_LATEX         = NO
```

**Générer :**

```bash
doxygen Doxyfile
```

### Éditeurs et IDE

**Éditeurs Markdown :**
- **Typora** : WYSIWYG, payant
- **Mark Text** : Open source, gratuit
- **VS Code** : Avec extensions Markdown
- **Ghostwriter** : Linux, gratuit

**IDEs de documentation :**
- **Sphinx** : Pour grands projets (Python-based)
- **MkDocs** : Simple et élégant
- **GitBook** : Plateforme complète

### MkDocs - Installation et usage

**Installation :**

```bash
pip install mkdocs mkdocs-material
```

**Créer un projet :**

```bash
mkdocs new mon-projet  
cd mon-projet
```

**Structure :**

```
mon-projet/
├── mkdocs.yml          # Configuration
└── docs/
    ├── index.md        # Page d'accueil
    ├── installation.md
    ├── guide.md
    └── api.md
```

**Configuration (mkdocs.yml) :**

```yaml
site_name: Ma Documentation  
site_description: Documentation complète de Ma Bibliothèque  
site_author: Votre Nom

theme:
  name: material
  palette:
    primary: indigo
    accent: indigo
  features:
    - navigation.tabs
    - navigation.sections
    - toc.integrate

nav:
  - Accueil: index.md
  - Installation: installation.md
  - Guide utilisateur: guide.md
  - Référence API: api.md

markdown_extensions:
  - admonition
  - codehilite
  - pymdownx.superfences
  - pymdownx.tabbed
  - toc:
      permalink: true
```

**Serveur de développement :**

```bash
mkdocs serve
# Ouvre http://localhost:8000
```

**Build pour production :**

```bash
mkdocs build
# Génère le site dans site/
```

**Déployer sur GitHub Pages :**

```bash
mkdocs gh-deploy
```

## Captures d'écran et diagrammes

### Outils de capture

**Windows :**
- **Snipping Tool** : Intégré
- **ShareX** : Gratuit, puissant
- **Greenshot** : Open source

**Linux :**
- **Shutter** : Complet
- **Flameshot** : Moderne
- **GNOME Screenshot** : Simple

**macOS :**
- **Cmd+Shift+4** : Intégré
- **Skitch** : Annotation

### Bonnes pratiques

**Qualité des captures :**
- Résolution minimale : 1280x720
- Format : PNG pour interface, JPG pour photos
- Compression : Optimiser la taille sans perte de qualité

**Annotation :**
- Flèches pour indiquer des éléments
- Numéros pour séquences
- Zones colorées pour mettre en évidence
- Éviter le texte manuscrit

**Organisation :**
```
docs/
├── images/
│   ├── screenshots/
│   │   ├── 01-new-project.png
│   │   ├── 02-add-component.png
│   │   └── 03-final-result.png
│   └── diagrams/
│       ├── architecture.svg
│       └── workflow.svg
```

### Diagrammes

**Outils de diagrammes :**

1. **Draw.io / diagrams.net** (gratuit)
   - En ligne ou desktop
   - Nombreux templates
   - Export SVG/PNG

2. **PlantUML** (code-based)
   ```plantuml
   @startuml
   class TMyClass {
     +Field1: Integer
     +Method1()
   }
   @enduml
   ```

3. **Mermaid** (code-based, intégré GitHub)
   ```mermaid
   graph TD
     A[Début] --> B{Condition}
     B -->|Oui| C[Action 1]
     B -->|Non| D[Action 2]
     C --> E[Fin]
     D --> E
   ```

4. **Graphviz** (code-based)
   ```dot
   digraph G {
     Start -> Process;
     Process -> Decision;
     Decision -> End [label="Yes"];
     Decision -> Process [label="No"];
   }
   ```

## Publier la documentation

### GitHub/GitLab Pages

**GitHub Pages :**

1. **Créer une branche `gh-pages`**
2. **Pousser la documentation HTML**
3. **Activer Pages dans Settings**
4. **URL** : `https://username.github.io/projet/`

**Automatisation avec GitHub Actions :**

`.github/workflows/docs.yml` :

```yaml
name: Documentation

on:
  push:
    branches: [ main ]

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v2

    - name: Install PasDoc
      run: sudo apt-get install -y pasdoc

    - name: Generate docs
      run: pasdoc @pasdoc.cfg

    - name: Deploy to GitHub Pages
      uses: peaceiris/actions-gh-pages@v3
      with:
        github_token: ${{ secrets.GITHUB_TOKEN }}
        publish_dir: ./docs/html
```

**GitLab Pages :**

`.gitlab-ci.yml` :

```yaml
pages:
  script:
    - apt-get update -qq && apt-get install -y -qq pasdoc
    - pasdoc @pasdoc.cfg
    - mv docs/html public
  artifacts:
    paths:
      - public
  only:
    - main
```

### Read the Docs

**Read the Docs** (readthedocs.org) :

1. **Créer un compte**
2. **Connecter votre dépôt GitHub**
3. **Configurer** `.readthedocs.yaml` :

```yaml
version: 2

sphinx:
  configuration: docs/conf.py

formats:
  - pdf
  - epub

python:
  version: 3.8
  install:
    - requirements: docs/requirements.txt
```

4. **Build automatique** à chaque push

### Hébergement personnalisé

**Options :**

1. **Serveur web propre** : Apache, Nginx
2. **Services cloud** : AWS S3, Azure Storage
3. **CDN** : Cloudflare, Netlify
4. **Wiki hébergé** : MediaWiki, DokuWiki

## Maintenir la documentation

### Versionner la documentation

**Synchroniser avec les releases :**

```
docs/
├── latest/          # Version en développement
├── v2.0/           # Version 2.0 stable
├── v1.5/           # Version 1.5 (ancienne)
└── v1.0/           # Version 1.0 (legacy)
```

**Dans le code :**

```pascal
{**
  @version 2.0.0
  @since 1.5.0
  @deprecated Removed in 3.0.0, use NewMethod instead
}
```

### Processus de mise à jour

**Quand mettre à jour :**

✅ **Toujours :**
- Nouvelle fonctionnalité ajoutée
- API modifiée (breaking change)
- Bug corrigé (si impact utilisateur)
- Nouvelle version release

⚠️ **Parfois :**
- Améliorations internes
- Refactoring sans impact API
- Optimisations

**Checklist avant release :**

- [ ] Documentation API à jour
- [ ] Guide utilisateur mis à jour
- [ ] Changelog complété
- [ ] Exemples testés et fonctionnels
- [ ] Captures d'écran actualisées
- [ ] Liens vérifiés (pas de 404)
- [ ] Version synchronisée avec le code
- [ ] Build de la documentation réussi

### Obsolescence et dépréciation

**Marquer comme obsolète :**

```pascal
{**
  @deprecated(Use NewMethod instead. Will be removed in v3.0)
}
procedure OldMethod; deprecated 'Use NewMethod';
```

**Dans le guide :**

````markdown
## OldMethod [DEPRECATED]

⚠️ **Cette méthode est obsolète depuis la version 2.0 et sera supprimée
dans la version 3.0. Utilisez [NewMethod](#newmethod) à la place.**

### Migration

Pour migrer de OldMethod vers NewMethod :

```pascal
// Ancien code
OldMethod(param1, param2);

// Nouveau code
NewMethod(param1, param2, defaultValue);
```
````

## Mesurer l'efficacité

### Métriques utiles

**Quantitatives :**
- Nombre de vues de la documentation
- Temps passé sur chaque page
- Pages les plus consultées
- Recherches effectuées

**Qualitatives :**
- Questions répétitives sur le forum (manque de doc ?)
- Feedback utilisateurs
- Issues GitHub liées à la doc
- Taux de complétion des tutoriels

### Outils d'analyse

**Google Analytics :**
- Intégration dans les pages HTML générées
- Suivi des visites et parcours

**GitHub Insights :**
- Statistiques du dépôt
- Vues des fichiers

**Feedback utilisateurs :**
- Bouton "Cette page est-elle utile ?" [Oui] [Non]
- Formulaire de feedback
- Commentaires (avec Disqus, etc.)

## Ressources et inspirations

### Exemples de bonne documentation

**Projets open source exemplaires :**

1. **mORMot** : Documentation très complète
2. **Castle Game Engine** : Tutoriels excellents
3. **Synapse** : API bien documentée
4. **BGRABitmap** : Exemples visuels

**Hors Pascal :**
- **Django** : Structure exemplaire
- **React** : Tutoriels interactifs
- **Rust Book** : Pédagogie remarquable
- **Vue.js** : Documentation claire

### Guides de style

**Standards de documentation :**

- **Google Developer Documentation Style Guide**
- **Microsoft Writing Style Guide**
- **Write the Docs** (writethedocs.org)

**Principes généraux :**

1. **Simplicité** : Phrases courtes et claires
2. **Cohérence** : Mêmes termes pour mêmes concepts
3. **Complétude** : Toutes les informations nécessaires
4. **Actualité** : Toujours à jour
5. **Accessibilité** : Pour tous les niveaux

## Conclusion

La documentation est un investissement qui rapporte énormément :

✅ **Moins de questions** → Plus de temps pour développer  
✅ **Plus d'utilisateurs** → Adoption facilitée  
✅ **Meilleure qualité** → Réflexion sur l'API  
✅ **Collaboration** → Contributeurs mieux informés  
✅ **Professionnalisme** → Crédibilité accrue

**Rappels clés :**

1. Documentez **au fur et à mesure** du développement
2. Utilisez **PasDoc** pour l'API
3. Écrivez des **guides utilisateur** complets
4. Créez des **tutoriels** pas-à-pas
5. Ajoutez des **captures d'écran** et diagrammes
6. **Testez** vos exemples de code
7. **Maintenez** la documentation à jour
8. **Écoutez** les retours utilisateurs

**Commencez petit :**

Même un simple README.md avec :
- Description du projet
- Installation
- Exemple basique
- Où obtenir de l'aide

C'est déjà un excellent début !

**Votre documentation est le pont entre votre code et vos utilisateurs.** Investissez le temps nécessaire pour la rendre excellente, et vous récolterez les bénéfices pendant des années. 📚

Bonne documentation ! 🚀

⏭️ [Conférences et meetups](/26-communaute-ecosysteme/05-conferences-meetups.md)
