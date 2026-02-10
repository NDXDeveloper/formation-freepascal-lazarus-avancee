🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.4 Outils de refactoring intégrés dans Lazarus

## Introduction : Qu'est-ce que le refactoring ?

Le **refactoring** (ou refactorisation en français) consiste à améliorer la structure de votre code sans changer son comportement. C'est comme réorganiser votre bureau : les objets font toujours la même chose, mais ils sont mieux rangés, plus faciles à trouver et à utiliser.

Imaginez que vous ayez écrit une variable nommée `x` et que, 200 lignes plus tard, vous réalisiez qu'elle devrait s'appeler `nombreClients` pour être plus claire. Le refactoring vous permet de renommer cette variable partout automatiquement, sans risquer d'en oublier une occurrence.

**Pourquoi refactoriser ?**
- 🎯 **Clarté** : Rendre le code plus lisible
- 🔧 **Maintenance** : Faciliter les modifications futures
- 🐛 **Bugs** : Réduire les risques d'erreur
- 📚 **Réutilisation** : Créer du code plus modulaire

## Les outils de refactoring de Lazarus

Lazarus propose plusieurs outils puissants accessibles via :
- Le menu **Source**
- Le menu contextuel (clic droit dans l'éditeur)
- Les raccourcis clavier

Voici la liste des principaux outils :

```
Outils de refactoring Lazarus :
├── Renommer un identifiant (F2)
├── Extraire une procédure (Ctrl+Shift+M)
├── Complétion de classe (Ctrl+Shift+C)
├── Ajouter/Retirer une unité des uses
├── Inverser une assignation
├── Déclarer une variable/procédure
├── Rechercher et remplacer dans les fichiers
└── Formater le code source
```

## Renommer un identifiant (F2)

### Comment ça marche

C'est l'outil de refactoring le plus utilisé. Il permet de renommer :
- Variables
- Procédures/Fonctions
- Classes
- Propriétés
- Constantes
- Types

### Utilisation pratique

**Avant le refactoring :**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var
  x: Integer;  // Nom peu clair
  y: string;   // Nom peu clair
begin
  x := 0;
  y := Edit1.Text;
  while x < 10 do
  begin
    Memo1.Lines.Add(y + ' : ' + IntToStr(x));
    Inc(x);
  end;
end;
```

**Pour renommer :**
1. Placez le curseur sur `x`
2. Appuyez sur **F2** (ou clic droit → **Refactoring** → **Renommer l'identifiant**)
3. Une boîte de dialogue apparaît :

```
┌─ Renommer l'identifiant ─────────────────┐
│ Ancien nom : x                           │
│ Nouveau nom : [compteur_____]            │
│                                          │
│ Portée :                                 │
│ ○ Dans cette procédure uniquement        │
│ ● Dans toute l'unité                     │
│ ○ Dans tout le projet                    │
│                                          │
│ ☑ Renommer aussi dans les commentaires  │
│ ☑ Aperçu avant application              │
│                                          │
│ [OK] [Annuler]                           │
└──────────────────────────────────────────┘
```

**Après le refactoring :**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var
  compteur: Integer;    // Nom clair !
  texteUtilisateur: string;  // Nom clair !
begin
  compteur := 0;
  texteUtilisateur := Edit1.Text;
  while compteur < 10 do
  begin
    Memo1.Lines.Add(texteUtilisateur + ' : ' + IntToStr(compteur));
    Inc(compteur);
  end;
end;
```

### Options avancées

**💡 Portée du renommage :**
- **Locale** : Seulement dans la procédure/fonction actuelle
- **Unité** : Dans tout le fichier .pas
- **Projet** : Dans tous les fichiers du projet
- **Groupe de projets** : Dans tous les projets ouverts

**⚠️ Attention** : Le renommage dans tout le projet peut prendre du temps sur de gros projets.

## Extraire une procédure (Ctrl+Shift+M)

### Principe

Vous avez un bloc de code qui fait une tâche spécifique ? Transformez-le en procédure réutilisable !

### Exemple pratique

**Avant : Code dupliqué**
```pascal
procedure TForm1.CalculerTaxes;  
begin
  // Calcul complexe répété
  Total := SousTotal * 1.15;
  if ClientVIP then
    Total := Total * 0.95;
  if CodePromo <> '' then
    Total := Total * 0.90;
  Label1.Caption := FormatFloat('0.00', Total);

  // ... plus de code ...

  // Le même calcul ailleurs
  Total := SousTotal * 1.15;
  if ClientVIP then
    Total := Total * 0.95;
  if CodePromo <> '' then
    Total := Total * 0.90;
  Label2.Caption := FormatFloat('0.00', Total);
end;
```

**Pour extraire :**
1. Sélectionnez le bloc de code à extraire
2. **Ctrl+Shift+M** (ou menu **Source** → **Refactoring** → **Extraire une procédure**)
3. Configurez l'extraction :

```
┌─ Extraire une procédure ─────────────────┐
│ Nom : [CalculerMontantFinal____]         │
│                                          │
│ Type :                                   │
│ ● Procédure (pas de valeur retournée)    │
│ ○ Fonction (retourne une valeur)         │
│                                          │
│ Paramètres détectés :                    │
│ ☑ SousTotal : Double                    │
│ ☑ ClientVIP : Boolean                   │
│ ☑ CodePromo : String                    │
│                                          │
│ Valeur de retour : Double                │
│                                          │
│ [OK] [Annuler]                           │
└──────────────────────────────────────────┘
```

**Après : Code refactorisé**
```pascal
function TForm1.CalculerMontantFinal(ASousTotal: Double;
  AClientVIP: Boolean; ACodePromo: string): Double;
begin
  Result := ASousTotal * 1.15;
  if AClientVIP then
    Result := Result * 0.95;
  if ACodePromo <> '' then
    Result := Result * 0.90;
end;

procedure TForm1.CalculerTaxes;  
begin
  Total := CalculerMontantFinal(SousTotal, ClientVIP, CodePromo);
  Label1.Caption := FormatFloat('0.00', Total);

  // ... plus de code ...

  Total := CalculerMontantFinal(SousTotal, ClientVIP, CodePromo);
  Label2.Caption := FormatFloat('0.00', Total);
end;
```

### Détection automatique des paramètres

Lazarus analyse intelligemment :
- **Variables lues** → Deviennent des paramètres d'entrée
- **Variables modifiées** → Deviennent des paramètres var ou out
- **Une seule valeur modifiée** → Proposition de fonction
- **Plusieurs valeurs modifiées** → Procédure avec paramètres var

## Complétion de classe (Ctrl+Shift+C)

### Le super pouvoir de Lazarus

C'est l'outil le plus puissant et le plus utilisé ! Il génère automatiquement :
- Les corps de méthodes
- Les déclarations de propriétés
- Les getters/setters
- Les variables privées

### Génération de méthodes

**Vous écrivez dans la déclaration de classe :**
```pascal
type
  TPersonne = class
  private
    FNom: string;
    FAge: Integer;
  public
    constructor Create(const ANom: string; AAge: Integer);
    procedure Afficher;
    function EstMajeur: Boolean;
  end;
```

**Appuyez sur Ctrl+Shift+C, Lazarus génère :**
```pascal
{ TPersonne }

constructor TPersonne.Create(const ANom: string; AAge: Integer);  
begin

end;

procedure TPersonne.Afficher;  
begin

end;

function TPersonne.EstMajeur: Boolean;  
begin

end;
```

### Génération de propriétés

**Vous écrivez :**
```pascal
type
  TPersonne = class
  private
  public
    property Nom: string read FNom write SetNom;
    property Age: Integer read FAge write SetAge;
  end;
```

**Ctrl+Shift+C génère automatiquement :**
```pascal
type
  TPersonne = class
  private
    FAge: Integer;
    FNom: string;
    procedure SetAge(AValue: Integer);
    procedure SetNom(AValue: string);
  public
    property Nom: string read FNom write SetNom;
    property Age: Integer read FAge write SetAge;
  end;

implementation

procedure TPersonne.SetAge(AValue: Integer);  
begin
  if FAge = AValue then Exit;
  FAge := AValue;
end;

procedure TPersonne.SetNom(AValue: string);  
begin
  if FNom = AValue then Exit;
  FNom := AValue;
end;
```

### Génération inverse

**Astuce géniale** : Ça marche aussi dans l'autre sens !

Si vous écrivez d'abord l'implémentation :
```pascal
procedure TForm1.NouvelleMethode;  
begin
  ShowMessage('Hello');
end;
```

**Ctrl+Shift+C** ajoute automatiquement la déclaration dans la classe !

## Déclarer une variable/procédure

### Variable locale automatique

**Situation :** Vous utilisez une variable non déclarée :
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin
  i := 0;  // 'i' n'est pas déclaré !
  for i := 0 to 10 do
    Memo1.Lines.Add(IntToStr(i));
end;
```

**Solution :**
1. Placez le curseur sur `i`
2. **Ctrl+Shift+D** (ou menu **Source** → **Refactoring** → **Déclarer variable**)
3. Lazarus propose :

```
┌─ Déclarer Variable ──────────────────────┐
│ Nom : i                                  │
│ Type : [Integer_____] (détecté)          │
│                                          │
│ Emplacement :                            │
│ ● Variable locale                        │
│ ○ Champ privé de classe                  │
│ ○ Variable globale d'unité               │
│                                          │
│ [OK] [Annuler]                           │
└──────────────────────────────────────────┘
```

**Résultat :**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var
  i: Integer;  // Ajouté automatiquement !
begin
  i := 0;
  for i := 0 to 10 do
    Memo1.Lines.Add(IntToStr(i));
end;
```

### Procédure/Fonction automatique

**Vous appelez une procédure inexistante :**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin
  AfficherResultat(42, 'Test');  // N'existe pas encore
end;
```

**Ctrl+Shift+D propose de créer :**
```pascal
procedure TForm1.AfficherResultat(AValue: Integer; AText: string);  
begin
  // TODO: Implémenter AfficherResultat
end;
```

## Ajouter/Retirer des unités (uses)

### Ajouter une unité manquante

**Problème :** Vous utilisez `TStringList` mais `Classes` n'est pas dans uses :
```pascal
procedure TForm1.Test;  
var
  List: TStringList;  // Erreur : TStringList non trouvé
begin
  List := TStringList.Create;
end;
```

**Solution :**
1. Curseur sur `TStringList`
2. **Alt+F11** (ou menu **Source** → **Refactoring** → **Ajouter unité au uses**)
3. Lazarus trouve et ajoute `Classes` automatiquement

### Nettoyer les uses inutilisés

**Menu Source** → **Refactoring** → **Retirer les unités non utilisées**

Lazarus analyse et propose :
```
┌─ Unités non utilisées ─────────────────────┐
│ Les unités suivantes semblent inutiles :   │
│                                            │
│ ☑ DateUtils                               │
│ ☑ Math                                    │
│ ☐ SysUtils (peut être nécessaire)         │
│                                            │
│ [Retirer sélectionnées] [Annuler]          │
└────────────────────────────────────────────┘
```

**⚠️ Attention** : Certaines unités peuvent être utilisées indirectement. Testez après suppression !

## Inverser une assignation

### Principe

Transforme `A := B` en `B := A`. Utile pour les propriétés et les échanges de valeurs.

### Exemple

**Avant :**
```pascal
Edit1.Text := Label1.Caption;
```

**Après inversion :**
```pascal
Label1.Caption := Edit1.Text;
```

**Raccourci :** **Ctrl+Alt+R** avec le curseur sur l'assignation

## Formater le code source (Ctrl+D)

### Formatage automatique

Lazarus peut reformater tout votre code selon des règles prédéfinies :

**Avant formatage :**
```pascal
procedure TForm1.Test;  
var  
i,j:Integer;  
s:string;  
begin  
if True then begin  
ShowMessage('Test');
        for i:=0 to 10 do begin
    j:=i*2;
s:=IntToStr(j);  
Memo1.Lines.Add(s);  
end;
    end;
end;
```

**Après formatage (Ctrl+D) :**
```pascal
procedure TForm1.Test;  
var
  i, j: Integer;
  s: string;
begin
  if True then
  begin
    ShowMessage('Test');
    for i := 0 to 10 do
    begin
      j := i * 2;
      s := IntToStr(j);
      Memo1.Lines.Add(s);
    end;
  end;
end;
```

### Configuration du formatage

**Outils** → **Options** → **Outils de code** → **Formatage** :

```
Options de formatage :
├── Indentation
│   ├── Espaces par indentation : [2]
│   └── ☑ Indenter les begin/end
├── Espacement
│   ├── ☑ Espace avant '('
│   ├── ☑ Espace après ','
│   └── ☑ Espace autour des opérateurs
├── Retours à la ligne
│   ├── Begin sur nouvelle ligne : [Toujours]
│   └── Largeur max ligne : [80]
└── Casse
    ├── Mots-clés : [Minuscules]
    └── Identifiants : [Conserver]
```

## Rechercher et remplacer dans les fichiers

### Recherche multi-fichiers

**Ctrl+Shift+F** ouvre la recherche dans les fichiers :

```
┌─ Rechercher dans les fichiers ───────────┐
│ Rechercher : [MonAncienneClasse_____]    │
│ Remplacer par : [MaNouvelleClasse___]    │
│                                          │
│ Options :                                │
│ ☑ Mot entier uniquement                 │
│ ☐ Respecter la casse                    │
│ ☑ Expression régulière                  │
│                                          │
│ Portée :                                 │
│ ○ Fichier actuel                         │
│ ● Tous les fichiers ouverts              │
│ ○ Projet                                 │
│ ○ Répertoire : [________] [Parcourir]    │
│                                          │
│ Masque : [*.pas;*.pp;*.inc]              │
│                                          │
│ [Rechercher] [Remplacer] [Annuler]       │
└──────────────────────────────────────────┘
```

### Résultats de recherche

Les résultats s'affichent dans la fenêtre **Résultats de recherche** :

```
Résultats de recherche - 15 occurrences trouvées :
├── Unit1.pas
│   ├── Ligne 25: MonAncienneClasse.Create;
│   └── Ligne 47: if MonAncienneClasse.Active then
├── Unit2.pas
│   └── Ligne 12: uses MonAncienneClasse;
└── MainForm.pas
    └── Ligne 89: Obj: MonAncienneClasse;

[Remplacer tout] [Remplacer sélection] [Fermer]
```

## Déplacer des méthodes entre classes

### Couper/Coller intelligent

Lazarus peut déplacer une méthode d'une classe à une autre :

1. Sélectionnez la méthode complète (déclaration + implémentation)
2. **Ctrl+X** pour couper
3. Placez le curseur dans la nouvelle classe
4. **Ctrl+V** pour coller
5. **Ctrl+Shift+C** pour ajuster les déclarations

### Exemple de déplacement

**Avant : Méthode dans TForm1**
```pascal
type
  TForm1 = class(TForm)
  private
    procedure CalculerTotal;  // À déplacer
  end;

  TCalculateur = class
  private
    // Destination
  end;
```

**Après déplacement :**
```pascal
type
  TForm1 = class(TForm)
  private
    // Méthode retirée
  end;

  TCalculateur = class
  private
    procedure CalculerTotal;  // Déplacée ici
  end;
```

## Créer des getters/setters automatiquement

### Génération rapide

Pour une propriété avec getter/setter :

```pascal
type
  TProduit = class
  private
    FPrix: Double;
    FQuantite: Integer;
  published
    property Prix: Double read GetPrix write SetPrix;
    property Quantite: Integer read GetQuantite write SetQuantite;
  end;
```

**Ctrl+Shift+C génère :**
```pascal
function TProduit.GetPrix: Double;  
begin
  Result := FPrix;
end;

procedure TProduit.SetPrix(AValue: Double);  
begin
  if FPrix = AValue then Exit;
  FPrix := AValue;
end;

function TProduit.GetQuantite: Integer;  
begin
  Result := FQuantite;
end;

procedure TProduit.SetQuantite(AValue: Integer);  
begin
  if FQuantite = AValue then Exit;
  FQuantite := AValue;
end;
```

### Options de génération

Dans **Outils** → **Options** → **Outils de code** → **Complétion de classe** :

```
Options des propriétés :
├── Préfixe des champs : [F]
├── Préfixe des setters params : [A]
├── ☑ Générer test d'égalité dans setter
├── ☑ Setter inline pour types simples
└── Position des méthodes : [Après les champs]
```

## Réorganiser le code

### Trier les méthodes

**Source** → **Refactoring** → **Trier les membres de classe** :

```
┌─ Trier les membres ───────────────────────┐
│ Ordre de tri :                            │
│ 1. ☑ Champs privés                       │
│ 2. ☑ Méthodes privées                    │
│ 3. ☑ Champs protégés                     │
│ 4. ☑ Méthodes protégées                  │
│ 5. ☑ Champs publics                      │
│ 6. ☑ Méthodes publiques                  │
│ 7. ☑ Propriétés publiées                 │
│                                           │
│ ☑ Alphabétique dans chaque section       │
│                                           │
│ [Appliquer] [Annuler]                     │
└───────────────────────────────────────────┘
```

### Grouper les déclarations

Lazarus peut regrouper automatiquement :
- Les variables de même type
- Les uses similaires
- Les méthodes par fonction

## Outils spéciaux

### Synchroniser les prototypes

Si vous modifiez une déclaration de méthode, **Ctrl+Shift+S** synchronise avec l'implémentation :

**Vous changez :**
```pascal
// Dans l'interface
procedure MaMethode(NouveauParam: string);

// L'implémentation est automatiquement mise à jour
procedure TForm1.MaMethode(NouveauParam: string);  
begin
  // Code existant préservé
end;
```

### Complétion de code avec templates

**Ctrl+J** affiche les templates disponibles :

```
Templates disponibles :
├── b     : begin..end
├── c     : case statement
├── cls   : class declaration
├── for   : for loop
├── func  : function
├── if    : if then
├── proc  : procedure
├── try   : try..except
├── tryf  : try..finally
├── while : while do
└── [personnalisés...]
```

## Raccourcis essentiels du refactoring

### Les indispensables

| Raccourci | Action |
|-----------|--------|
| **F2** | Renommer l'identifiant |
| **Ctrl+Shift+C** | Complétion de classe |
| **Ctrl+Shift+D** | Déclarer variable/méthode |
| **Ctrl+Shift+M** | Extraire procédure |
| **Ctrl+D** | Formater le code |
| **Alt+F11** | Ajouter unité au uses |
| **Ctrl+Shift+F** | Rechercher dans fichiers |
| **Ctrl+J** | Templates de code |
| **Ctrl+Shift+S** | Synchroniser prototypes |
| **Ctrl+Alt+R** | Inverser assignation |

### Navigation rapide

| Raccourci | Action |
|-----------|--------|
| **Ctrl+Clic** | Aller à la déclaration |
| **Ctrl+Shift+Haut** | Basculer déclaration/implémentation |
| **Ctrl+Shift+Bas** | Basculer déclaration/implémentation |
| **Alt+Gauche** | Naviguer en arrière |
| **Alt+Droite** | Naviguer en avant |

## Bonnes pratiques du refactoring

### Quand refactoriser

**✅ Bon moment pour refactoriser :**
- Avant d'ajouter une nouvelle fonctionnalité
- Après avoir corrigé un bug
- Pendant la revue de code
- Quand le code devient difficile à comprendre

**❌ Évitez de refactoriser :**
- Juste avant une deadline
- Sur du code qui fonctionne et ne sera plus touché
- Sans tests pour vérifier
- Plusieurs choses à la fois

### Stratégie de refactoring

1. **Petits pas** : Un changement à la fois
2. **Tester** : Vérifier après chaque modification
3. **Commiter** : Sauvegarder dans Git après chaque refactoring réussi
4. **Documenter** : Expliquer pourquoi vous refactorisez

### Checklist avant refactoring

```
Avant de refactoriser :
☐ Le code fonctionne actuellement
☐ J'ai des tests (ou je peux tester manuellement)
☐ J'ai fait un commit Git
☐ Je comprends ce que fait le code
☐ J'ai identifié clairement l'amélioration
☐ Le refactoring apporte une vraie valeur
```

## Cas pratiques courants

### Transformer une procédure en méthode de classe

**Avant : Procédure globale**
```pascal
procedure CalculerTVA(Montant: Double; var Resultat: Double);  
begin
  Resultat := Montant * 1.20;
end;
```

**Refactoring : En méthode**
1. Créez ou identifiez la classe appropriée
2. Coupez la procédure
3. Collez dans la classe
4. Ctrl+Shift+C pour générer l'implémentation
5. Mettez à jour les appels

**Après : Méthode de classe**
```pascal
type
  TCalculateur = class
  public
    class function CalculerTVA(Montant: Double): Double;
  end;

class function TCalculateur.CalculerTVA(Montant: Double): Double;  
begin
  Result := Montant * 1.20;
end;
```

### Diviser une classe trop grosse

**Signes qu'une classe est trop grosse :**
- Plus de 500 lignes
- Plus de 20 méthodes
- Responsabilités multiples
- Nom vague (Manager, Handler, Processor)

**Stratégie de division :**
1. Identifiez les responsabilités distinctes
2. Créez de nouvelles classes
3. Déplacez les méthodes liées
4. Créez des relations entre classes

## Résolution de problèmes

### Le refactoring ne fonctionne pas

**Causes possibles :**
- Code avec erreurs de syntaxe
- Fichiers non sauvegardés
- Cache de code tools corrompu

**Solutions :**
1. Compilez d'abord (Ctrl+F9)
2. Sauvegardez tous les fichiers
3. **Outils** → **Rescan FPC Source Directory**

### Conflit après renommage

**Symptôme :** "Identifier redeclared"

**Solution :**
1. Vérifiez les doublons
2. Utilisez la recherche pour trouver les occurrences
3. Renommez avec un préfixe unique temporaire
4. Puis renommez vers le nom final

### Performance lente sur gros projets

**Optimisations :**
- Limitez la portée du refactoring
- Fermez les fichiers non nécessaires
- Désactivez la complétion automatique temporairement
- Augmentez la mémoire allouée à Lazarus

## Conclusion

Les outils de refactoring de Lazarus transforment des heures de travail manuel en quelques secondes d'automatisation. Ils réduisent drastiquement les erreurs et permettent de maintenir un code propre et évolutif.

**Points clés à retenir :**
- 🎯 **F2** pour renommer est votre meilleur ami
- 💪 **Ctrl+Shift+C** est magique pour les classes
- 🔧 Refactorisez par petits pas
- 🧪 Testez après chaque modification
- 📚 Un code bien structuré est plus facile à maintenir

La maîtrise de ces outils vous fera gagner un temps précieux et améliorera considérablement la qualité de votre code. Plus vous les utiliserez, plus ils deviendront naturels dans votre workflow quotidien.

Dans la prochaine section (2.5), nous explorerons le débogueur et ses fonctionnalités avancées pour traquer efficacement les bugs dans vos applications.

⏭️ [Débogueur GDB et alternatives](/02-maitrise-ide-lazarus/05-debogueur-gdb-alternatives.md)
