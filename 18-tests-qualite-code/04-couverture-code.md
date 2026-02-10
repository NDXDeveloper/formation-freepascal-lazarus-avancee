🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.4 Couverture de Code

## Introduction

La couverture de code (code coverage en anglais) est une métrique essentielle pour évaluer la qualité de vos tests. Elle mesure quelle proportion de votre code source est réellement exécutée lorsque vos tests s'exécutent.

### Pourquoi la Couverture de Code est Importante ?

Imaginez que vous avez écrit une application avec 10 000 lignes de code et une suite de tests. Comment savoir si vos tests vérifient bien tout votre code ? C'est exactement ce que la couverture de code vous permet de découvrir.

**Avantages de la couverture de code :**

1. **Identifier le code non testé** : découvrir les parties de votre application qui ne sont jamais exécutées par vos tests
2. **Améliorer la qualité des tests** : repérer les branches conditionnelles, les cas d'erreur ou les fonctions oubliées
3. **Réduire les bugs** : plus le code est testé, moins il y a de risques de bugs en production
4. **Confiance lors des modifications** : savoir qu'une modification ne casse rien grâce à une bonne couverture
5. **Documentation vivante** : la couverture montre quelles parties du code sont réellement utilisées

### Ce que la Couverture de Code N'est PAS

⚠️ **Attention aux idées fausses :**

- **100% de couverture ≠ code sans bugs** : vous pouvez exécuter chaque ligne sans vérifier que le résultat est correct
- **La couverture n'est pas une fin en soi** : c'est un indicateur, pas un objectif absolu
- **Quantité ≠ qualité** : 100 tests de mauvaise qualité valent moins que 10 excellents tests
- **La couverture ne teste pas la logique métier** : elle mesure seulement ce qui est exécuté, pas si c'est correct

> **Règle d'or :** La couverture de code est un outil de diagnostic, pas un badge d'honneur. Une couverture de 80% avec de bons tests vaut mieux que 100% avec des tests superficiels.

---

## Types de Couverture de Code

Il existe plusieurs niveaux de couverture, du plus simple au plus complexe :

### 1. Couverture de Ligne (Line Coverage)

**Définition :** Pourcentage de lignes de code exécutées par les tests.

**Exemple :**

```pascal
function Diviser(a, b: Integer): Integer;  
begin
  if b = 0 then          // Ligne 1
    raise Exception.Create('Division par zéro');  // Ligne 2
  Result := a div b;     // Ligne 3
end;
```

**Test :**
```pascal
// Test qui ne couvre que les lignes 1 et 3
resultat := Diviser(10, 2);  // ✅ Ligne 1 exécutée, ✅ Ligne 3 exécutée
                             // ❌ Ligne 2 jamais exécutée
```

**Couverture :** 2/3 lignes = **66.7%**

Pour atteindre 100%, il faut ajouter un test :
```pascal
// Test qui couvre la ligne 2
try
  Diviser(10, 0);  // ✅ Ligne 1, ✅ Ligne 2 exécutées
except
  // Exception attendue
end;
```

### 2. Couverture de Branche (Branch Coverage)

**Définition :** Pourcentage de branches conditionnelles (if, case, while) testées.

**Exemple :**

```pascal
function ClasserNote(note: Integer): string;  
begin
  if note >= 90 then       // Branche A: vraie ou fausse ?
    Result := 'Excellent'
  else if note >= 70 then  // Branche B: vraie ou fausse ?
    Result := 'Bien'
  else if note >= 50 then  // Branche C: vraie ou fausse ?
    Result := 'Passable'
  else
    Result := 'Insuffisant';
end;
```

**Pour une couverture complète des branches, il faut tester :**
- note = 95 → branche A vraie ✅
- note = 75 → branche A fausse, B vraie ✅
- note = 55 → branches A et B fausses, C vraie ✅
- note = 30 → toutes branches fausses, else exécuté ✅

**Minimum de tests nécessaires :** 4 tests pour couvrir toutes les branches.

### 3. Couverture de Fonction (Function Coverage)

**Définition :** Pourcentage de fonctions/procédures appelées au moins une fois.

**Exemple :**

```pascal
function Additionner(a, b: Integer): Integer;  
begin
  Result := a + b;
end;

function Soustraire(a, b: Integer): Integer;  
begin
  Result := a - b;
end;

function Multiplier(a, b: Integer): Integer;  
begin
  Result := a * b;
end;
```

**Test :**
```pascal
// Test qui appelle seulement 2 fonctions sur 3
x := Additionner(5, 3);    // ✅ Fonction appelée  
y := Multiplier(4, 2);     // ✅ Fonction appelée
                           // ❌ Soustraire jamais appelée
```

**Couverture de fonction :** 2/3 = **66.7%**

### 4. Couverture de Condition (Condition Coverage)

**Définition :** Teste toutes les combinaisons possibles dans les expressions booléennes complexes.

**Exemple :**

```pascal
if (age >= 18) and (permis = True) then
  PeutConduire := True
else
  PeutConduire := False;
```

**Pour une couverture complète :**
- Test 1: age=20, permis=True → les deux conditions vraies ✅
- Test 2: age=16, permis=True → première fausse, deuxième vraie ✅
- Test 3: age=20, permis=False → première vraie, deuxième fausse ✅
- Test 4: age=16, permis=False → les deux conditions fausses ✅

### 5. Couverture de Chemin (Path Coverage)

**Définition :** Teste tous les chemins d'exécution possibles à travers le code (la plus exhaustive et la plus difficile).

**Exemple :**

```pascal
procedure Traiter(x, y: Integer);  
begin
  if x > 0 then      // Point de décision 1
    WriteLn('X positif');

  if y > 0 then      // Point de décision 2
    WriteLn('Y positif');
end;
```

**Chemins possibles :**
1. x > 0 ET y > 0 → deux messages affichés
2. x > 0 ET y ≤ 0 → premier message seulement
3. x ≤ 0 ET y > 0 → deuxième message seulement
4. x ≤ 0 ET y ≤ 0 → aucun message

**Minimum de tests :** 4 tests pour couvrir tous les chemins.

---

## Métriques de Couverture

### Objectifs de Couverture Réalistes

Il n'existe pas de pourcentage magique, mais voici des recommandations générales :

| Type de Projet | Couverture Recommandée | Justification |
|----------------|------------------------|---------------|
| **Bibliothèque critique** | 90-100% | Code réutilisé, bugs coûteux |
| **Application métier** | 70-80% | Bon équilibre qualité/effort |
| **Prototype/POC** | 40-60% | Tests sur fonctionnalités clés |
| **Code legacy** | 50-70% | Amélioration progressive |
| **API publique** | 85-95% | Interface critique |

### Calculer la Couverture

**Formule générale :**

```
Couverture (%) = (Éléments exécutés / Éléments totaux) × 100
```

**Exemple concret :**

Votre projet contient :
- 500 lignes de code au total
- Vos tests exécutent 380 lignes

```
Couverture = (380 / 500) × 100 = 76%
```

### Interpréter les Résultats

**Couverture < 50%** 🔴
- Beaucoup de code non testé
- Risque élevé de bugs
- **Action :** Écrire des tests pour les fonctionnalités critiques

**Couverture 50-70%** 🟡
- Couverture acceptable pour débuter
- Fonctionnalités principales testées
- **Action :** Identifier les zones critiques non couvertes

**Couverture 70-85%** 🟢
- Bonne couverture générale
- Confiance raisonnable
- **Action :** Maintenir et améliorer progressivement

**Couverture > 85%** 🟢🟢
- Excellente couverture
- Haute confiance
- **Action :** Se concentrer sur la qualité des tests existants

---

## Stratégies de Test par Couverture

### Approche Progressive

#### Étape 1 : Identifier les Zones Critiques

Commencez par tester en priorité :

1. **Code métier complexe** : algorithmes, calculs, logique business
2. **Points d'entrée** : API, endpoints, interfaces publiques
3. **Code à haut risque** : gestion d'erreurs, sécurité, transactions
4. **Code fréquemment modifié** : parties instables du code

#### Étape 2 : Écrire les Tests de Base

```pascal
// Tester le chemin nominal (cas normal)
procedure TestAdditionNormale;  
begin
  AssertEquals(5, Additionner(2, 3));
end;

// Tester les cas limites
procedure TestAdditionZero;  
begin
  AssertEquals(5, Additionner(5, 0));
  AssertEquals(5, Additionner(0, 5));
end;

// Tester les cas d'erreur
procedure TestAdditionNegatif;  
begin
  AssertEquals(-1, Additionner(2, -3));
end;
```

#### Étape 3 : Mesurer la Couverture

Utilisez les outils de profiling (détaillés dans les sections suivantes) pour générer un rapport de couverture.

#### Étape 4 : Combler les Trous

Analysez le rapport et identifiez :
- Les fonctions jamais appelées
- Les branches conditionnelles non testées
- Les cas d'erreur ignorés

#### Étape 5 : Maintenir la Couverture

Intégrez la mesure de couverture dans votre processus :
- CI/CD : bloquer les PR si la couverture baisse
- Révisions de code : vérifier que les nouveaux tests couvrent le nouveau code
- Refactoring : maintenir ou améliorer la couverture

### Exemple de Stratégie par Priorités

```
HAUTE PRIORITÉ (viser 90%+)
├── Couche métier
│   ├── Calculs financiers
│   ├── Algorithmes de traitement
│   └── Validation des données
├── API publique
│   ├── Endpoints REST
│   └── Interfaces exposées

MOYENNE PRIORITÉ (viser 70%+)
├── Services applicatifs
├── Accès aux données
└── Gestion des erreurs

BASSE PRIORITÉ (viser 40%+)
├── Code d'interface utilisateur
├── Configuration
└── Logging
```

---

## Outils de Couverture pour FreePascal

### Vue d'Ensemble

FreePascal propose plusieurs approches pour mesurer la couverture de code :

| Outil/Méthode | Plateforme | Type de Couverture | Complexité |
|---------------|------------|-------------------|------------|
| **gprof** | Windows/Linux | Fonction, ligne | Moyenne |
| **Valgrind (Callgrind)** | Linux | Ligne, branche | Moyenne |
| **lcov** | Linux | Ligne, fonction | Faible |
| **FPCUnit + scripts** | Multi-plateforme | Personnalisable | Élevée |

### Processus Général

Quelle que soit la plateforme, le processus est similaire :

```
1. INSTRUMENTATION
   ↓
   Compiler le code avec des options spéciales
   qui ajoutent du code de traçage

2. EXÉCUTION
   ↓
   Lancer les tests
   Le programme enregistre quelles parties sont exécutées

3. COLLECTE
   ↓
   Générer un fichier de données brutes
   (gmon.out, callgrind.out, etc.)

4. ANALYSE
   ↓
   Transformer les données en rapport lisible

5. VISUALISATION
   ↓
   Afficher les résultats (texte, HTML, graphique)
```

### Choix de l'Outil

**Utilisez gprof si :**
- ✅ Vous voulez une solution simple et rapide
- ✅ La couverture de fonction suffit
- ✅ Vous travaillez sur Windows ou Linux

**Utilisez Valgrind si :**
- ✅ Vous êtes sous Linux
- ✅ Vous voulez une analyse très détaillée
- ✅ Vous avez besoin de couverture de branche

**Créez un script personnalisé si :**
- ✅ Vous avez des besoins spécifiques
- ✅ Vous intégrez dans un pipeline CI/CD
- ✅ Vous voulez automatiser complètement le processus

---

## Bonnes Pratiques

### 1. Ne Visez Pas Aveuglément 100%

**Mauvaise approche :**
```pascal
// Test inutile juste pour augmenter la couverture
procedure TestGetterSetterEvident;  
begin
  personne.SetNom('Jean');
  AssertEquals('Jean', personne.GetNom);
end;
```

**Bonne approche :**
```pascal
// Test qui vérifie la logique métier
procedure TestValidationNomPersonne;  
begin
  // Nom valide
  AssertTrue(personne.SetNom('Jean Dupont'));

  // Nom vide refusé
  AssertFalse(personne.SetNom(''));

  // Nom trop long refusé
  AssertFalse(personne.SetNom(StringOfChar('A', 200)));

  // Caractères spéciaux refusés
  AssertFalse(personne.SetNom('Jean<script>'));
end;
```

### 2. Testez les Cas Limites et les Erreurs

**Ne testez pas que le chemin heureux :**

```pascal
// ❌ Incomplet : teste seulement le cas normal
procedure TestDivision;  
begin
  AssertEquals(5, Diviser(10, 2));
end;

// ✅ Complet : teste normal + cas limites + erreurs
procedure TestDivisionComplete;  
begin
  // Cas normal
  AssertEquals(5, Diviser(10, 2));

  // Cas limites
  AssertEquals(0, Diviser(0, 5));
  AssertEquals(1, Diviser(7, 7));

  // Cas d'erreur
  AssertException(EDivisionParZero, @DiviserParZero);
end;
```

### 3. Excluez le Code Non Testable

Certains codes ne méritent pas d'être testés :

```pascal
{$IFDEF COVERAGE_EXCLUDE}
procedure LoggerDebug(msg: string);  
begin
  // Simple logging, pas de logique métier
  WriteLn(FormatDateTime('hh:nn:ss', Now), ' - ', msg);
end;
{$ENDIF}
```

**Code à exclure généralement :**
- Logging simple
- Getters/setters triviaux
- Code généré automatiquement
- Code de débogage uniquement

### 4. Intégrez dans le Cycle de Développement

**Workflow idéal :**

```
DÉVELOPPEMENT
├── 1. Écrire le test (TDD)
├── 2. Implémenter le code
├── 3. Exécuter les tests
├── 4. Mesurer la couverture
└── 5. Combler les trous si nécessaire

AVANT COMMIT
├── Tests passent ✅
├── Couverture acceptable ✅
└── Code review ✅

CI/CD
├── Build automatique
├── Tests automatiques
├── Rapport de couverture
└── Blocage si couverture < seuil
```

### 5. Utilisez la Couverture pour la Maintenance

**Avant de refactorer du code legacy :**

```
1. Mesurer la couverture actuelle (baseline)
2. Écrire des tests pour atteindre 70%+ sur le code à modifier
3. Refactorer en vérifiant que les tests passent toujours
4. Mesurer à nouveau : la couverture doit être maintenue ou améliorée
```

---

## Limitations de la Couverture de Code

### Ce que la Couverture NE Garantit PAS

#### 1. Correction de la Logique

```pascal
// Ce code est 100% couvert mais FAUX
function CalculerMoyenne(a, b: Integer): Real;  
begin
  Result := a + b;  // ❌ Oubli de diviser par 2
end;

// Test qui passe mais ne vérifie rien d'utile
procedure TestMoyenne;  
begin
  resultat := CalculerMoyenne(10, 20);
  AssertTrue(resultat > 0);  // ✅ Passe mais ne vérifie pas que résultat = 15
end;
```

#### 2. Qualité des Tests

```pascal
// 100% de couverture, 0% de valeur
procedure TestBidon;  
begin
  FonctionComplexe(param1, param2);
  // ❌ Aucune assertion ! Le test ne vérifie rien
end;
```

#### 3. Tests d'Intégration

La couverture mesure généralement les tests unitaires, pas :
- L'intégration entre composants
- Les tests end-to-end
- Les tests de performance
- Les tests de sécurité

### Métriques Complémentaires

Pour une évaluation complète de la qualité, combinez :

```
QUALITÉ DU CODE
├── Couverture de code (70%+)
├── Tests qui passent (100%)
├── Complexité cyclomatique (<10 par fonction)
├── Duplication de code (<3%)
├── Temps d'exécution des tests (<5 min)
└── Code review approuvé
```

---

## Conclusion

La couverture de code est un outil puissant mais doit être utilisée intelligemment :

### ✅ À Faire

- Mesurer régulièrement la couverture
- Viser une couverture réaliste (70-85% pour la plupart des projets)
- Prioriser le code critique
- Tester les cas limites et les erreurs
- Utiliser la couverture pour identifier les zones à risque

### ❌ À Éviter

- Viser 100% à tout prix
- Écrire des tests inutiles juste pour augmenter la couverture
- Ignorer la qualité des tests au profit de la quantité
- Négliger les tests d'intégration
- Considérer la couverture comme la seule métrique de qualité

> **Philosophie :** La couverture de code est une boussole, pas une destination. Elle vous indique où aller, mais c'est à vous de choisir le meilleur chemin.

Dans les sections suivantes, nous verrons comment mettre en œuvre concrètement la couverture de code avec les outils disponibles sur Windows et Linux.

⏭️ [Profiling Windows (DProf)](/18-tests-qualite-code/04.1-profiling-windows-dprof.md)
