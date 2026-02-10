🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.5 Analyse statique du code

## Introduction

L'analyse statique du code consiste à examiner le code source d'un programme **sans l'exécuter**, afin de détecter des erreurs potentielles, des problèmes de qualité, des vulnérabilités de sécurité ou des violations de bonnes pratiques de programmation.

Contrairement aux tests qui nécessitent d'exécuter le programme, l'analyse statique inspecte directement le code pour identifier les problèmes avant même la compilation ou l'exécution.

## Pourquoi utiliser l'analyse statique ?

### Avantages principaux

1. **Détection précoce des bugs** : Trouve les erreurs avant l'exécution
2. **Amélioration de la qualité** : Encourage les bonnes pratiques
3. **Sécurité renforcée** : Détecte les vulnérabilités potentielles
4. **Maintenance facilitée** : Code plus lisible et cohérent
5. **Économie de temps** : Moins de bugs en production = moins de corrections

### Types de problèmes détectés

- Variables non initialisées
- Code mort (jamais exécuté)
- Fuites mémoire potentielles
- Erreurs de logique
- Non-respect des conventions de codage
- Complexité excessive
- Dépendances circulaires

## Outils d'analyse statique pour FreePascal/Lazarus

### 1. Les avertissements du compilateur FPC

Le compilateur FreePascal intègre déjà un système d'analyse basique mais efficace.

#### Configuration des avertissements

Dans Lazarus, allez dans **Project → Project Options → Compiler Options → Messages**.

Options importantes à activer :

```pascal
// Dans votre code source, vous pouvez aussi utiliser :
{$WARNINGS ON}
{$HINTS ON}
{$NOTES ON}
```

#### Avertissements courants

**Variables non initialisées :**
```pascal
var
  x: Integer;
begin
  WriteLn(x);  // Warning: Variable "x" might not be initialized
end;
```

**Variable déclarée mais non utilisée :**
```pascal
var
  unused: String;  // Hint: Local variable "unused" not used
begin
  // Code sans utiliser "unused"
end;
```

**Valeur assignée mais jamais lue :**
```pascal
var
  temp: Integer;
begin
  temp := 10;  // Hint: Value assigned to "temp" never used
  temp := 20;
  WriteLn(temp);
end;
```

### 2. Pascal Analyzer (Windows)

**Pascal Analyzer** est un outil commercial puissant d'analyse statique spécialement conçu pour Pascal/Delphi, compatible avec FreePascal.

#### Fonctionnalités

- Détection de code complexe
- Analyse de la structure du programme
- Métriques de qualité (complexité cyclomatique)
- Suggestions d'optimisation
- Rapports détaillés HTML/XML

#### Installation sur Windows

1. Télécharger depuis le site officiel
2. Exécuter l'installateur
3. Configurer les chemins vers vos projets Lazarus

#### Utilisation basique

```bash
# Ligne de commande
PAL.exe /PROJECT=MonProjet.lpi /REPORT=rapport.html
```

### 3. PasDoc - Documentation et vérifications

**PasDoc** génère de la documentation mais effectue aussi des vérifications.

#### Installation

**Windows :**
```bash
# Télécharger l'archive depuis GitHub
# Extraire et ajouter au PATH
```

**Ubuntu :**
```bash
sudo apt-get install pasdoc
```

#### Utilisation

```bash
pasdoc --format=html --output=docs/ source/*.pas
```

PasDoc détecte :
- Commentaires manquants
- Documentation incohérente
- Déclarations sans description

### 4. FPCLint (Script personnalisé)

Vous pouvez créer vos propres scripts d'analyse avec des expressions régulières.

#### Exemple de script bash (Ubuntu)

```bash
#!/bin/bash
# fpc_lint.sh - Analyse basique du code

SOURCE_DIR="$1"

echo "=== Analyse statique FreePascal ==="

# Recherche de TODO/FIXME
echo -e "\n--- TODO et FIXME trouvés ---"
grep -rn "//.*\(TODO\|FIXME\)" "$SOURCE_DIR"

# Variables avec un seul caractère (mauvaise pratique)
echo -e "\n--- Variables à un caractère ---"
grep -rn "var\s\+[a-z]\s*:" "$SOURCE_DIR"

# Procédures très longues (> 100 lignes)
echo -e "\n--- Procédures potentiellement trop longues ---"
awk '/^procedure|^function/{start=NR; name=$0}
     /^end;/{if(NR-start>100) print FILENAME":"start":"name}' "$SOURCE_DIR"/*.pas

echo -e "\n=== Analyse terminée ==="
```

#### Utilisation

```bash
chmod +x fpc_lint.sh
./fpc_lint.sh /chemin/vers/projet
```

### 5. SonarQube (avec plugin personnalisé)

**SonarQube** est une plateforme d'analyse continue de qualité du code.

#### Installation sur Ubuntu

```bash
# Installer Java
sudo apt-get install openjdk-11-jdk

# Télécharger SonarQube
wget https://binaries.sonarsource.com/Distribution/sonarqube/sonarqube-9.9.0.65466.zip
unzip sonarqube-9.9.0.65466.zip
cd sonarqube-9.9.0.65466

# Démarrer
./bin/linux-x86-64/sonar.sh start
```

Accéder à http://localhost:9000

#### Installation sur Windows

```bash
# Télécharger l'archive ZIP
# Extraire et lancer bin\windows-x86-64\StartSonar.bat
```

**Note :** SonarQube ne supporte pas nativement FreePascal, mais peut analyser des métriques génériques.

## Analyse manuelle : Bonnes pratiques

### 1. Revue de code (Code Review)

La revue par les pairs reste l'une des meilleures analyses statiques.

#### Checklist de revue

- [ ] Le code est-il lisible et compréhensible ?
- [ ] Les noms de variables sont-ils explicites ?
- [ ] Y a-t-il des commentaires pour les parties complexes ?
- [ ] Les erreurs sont-elles gérées correctement ?
- [ ] Les ressources sont-elles libérées (mémoire, fichiers) ?
- [ ] Le code respecte-t-il les conventions du projet ?

### 2. Métriques de complexité

#### Complexité cyclomatique

Mesure le nombre de chemins d'exécution indépendants.

**Exemple simple (complexité = 1) :**
```pascal
function Addition(a, b: Integer): Integer;
begin
  Result := a + b;  // Un seul chemin
end;
```

**Exemple complexe (complexité = 4) :**
```pascal
function Categoriser(age: Integer): String;
begin
  if age < 18 then        // Chemin 1
    Result := 'Mineur'
  else if age < 65 then   // Chemin 2
    Result := 'Adulte'
  else                    // Chemin 3
    Result := 'Senior';
  // + chemin par défaut = 4
end;
```

**Règle :** Viser une complexité < 10 par fonction.

#### Lignes de code par fonction

**Bonne pratique :**
- Fonctions courtes : 10-30 lignes
- Maximum recommandé : 50-100 lignes
- Au-delà : refactoriser

### 3. Détection manuelle des anti-patterns

#### God Object (Objet Dieu)

**Problème :** Une classe qui fait tout.

```pascal
type
  TApplication = class
    procedure ConnectDatabase;
    procedure SendEmail;
    procedure GenerateReport;
    procedure ProcessPayment;
    procedure ManageUsers;
    procedure LogErrors;
    // ... 50 autres méthodes
  end;
```

**Solution :** Séparer en classes spécialisées.

#### Magic Numbers (Nombres magiques)

**Problème :**
```pascal
if age > 18 then  // Pourquoi 18 ?
  // ...
```

**Solution :**
```pascal
const
  AGE_MAJORITE = 18;

if age > AGE_MAJORITE then
  // ...
```

#### Code dupliqué

**Problème :**
```pascal
// Dans FormA
Button1.Width := 100;
Button1.Height := 30;
Button1.Font.Size := 10;

// Dans FormB
Button2.Width := 100;
Button2.Height := 30;
Button2.Font.Size := 10;
```

**Solution :**
```pascal
procedure ConfigurerBoutonStandard(btn: TButton);
begin
  btn.Width := 100;
  btn.Height := 30;
  btn.Font.Size := 10;
end;
```

## Configuration d'une analyse statique automatisée

### Intégration dans le processus de build

#### Script de build avec analyse (bash)

```bash
#!/bin/bash
# build_with_analysis.sh

PROJECT="MonProjet.lpi"

echo "=== Compilation avec analyse ==="

# Compilation avec tous les warnings
lazbuild --build-all --compiler-option=-vw $PROJECT

# Vérification du code de retour
if [ $? -ne 0 ]; then
  echo "Erreur de compilation détectée !"
  exit 1
fi

# Analyse personnalisée
./fpc_lint.sh src/

echo "=== Build et analyse terminés ==="
```

#### Intégration CI/CD (GitLab CI)

```yaml
# .gitlab-ci.yml
stages:
  - analyse
  - build

analyse_statique:
  stage: analyse
  script:
    - apt-get update
    - apt-get install -y lazarus-ide
    - ./fpc_lint.sh src/
  artifacts:
    reports:
      codequality: code-quality-report.json
  only:
    - merge_requests

compilation:
  stage: build
  script:
    - lazbuild --build-all MonProjet.lpi
  dependencies:
    - analyse_statique
```

### Configuration pour Windows et Ubuntu

#### Fichier de configuration commun

**`analysis_config.ini`**
```ini
[General]
MaxLineLength=120
MaxFunctionLines=100
MaxComplexity=10

[Warnings]
UnusedVariables=true
UnusedParameters=true
UninitializedVariables=true

[Style]
IndentSize=2
UseSpaces=true
```

#### Script PowerShell (Windows)

```powershell
# analyze.ps1
param(
    [string]$ProjectPath = "."
)

Write-Host "=== Analyse statique Windows ===" -ForegroundColor Green

# Compilation avec warnings
& "C:\lazarus\lazbuild.exe" --build-all --compiler-option=-vw "$ProjectPath\MonProjet.lpi"

# Recherche de patterns
Get-ChildItem -Path "$ProjectPath\src" -Filter *.pas -Recurse | ForEach-Object {
    $content = Get-Content $_.FullName

    # TODO/FIXME
    $content | Select-String -Pattern "//\s*(TODO|FIXME)" | ForEach-Object {
        Write-Host "$($_.Filename):$($_.LineNumber): $($_.Line)" -ForegroundColor Yellow
    }
}

Write-Host "=== Analyse terminée ===" -ForegroundColor Green
```

## Interprétation des résultats

### Priorisation des problèmes

**Criticité haute (corriger immédiatement) :**
- Variables non initialisées
- Fuites mémoire potentielles
- Code mort dans les chemins critiques
- Violations de sécurité

**Criticité moyenne (planifier correction) :**
- Complexité excessive
- Duplication de code
- Conventions non respectées
- Documentation manquante

**Criticité basse (amélioration continue) :**
- Optimisations mineures
- Commentaires à améliorer
- Refactoring cosmétique

### Exemple de rapport d'analyse

```
=== Rapport d'analyse statique ===
Projet: MonApplication
Date: 2025-10-06
Fichiers analysés: 45

--- Résumé ---
Erreurs critiques:    2
Avertissements:      15
Suggestions:         38
Total problèmes:     55

--- Détails ---
[ERREUR] main.pas:142 - Variable 'result' non initialisée
[ERREUR] database.pas:67 - Fuite mémoire potentielle (TStringList non libérée)

[WARN] utils.pas:234 - Fonction trop complexe (complexité=12)
[WARN] forms.pas:89 - Code dupliqué détecté (3 occurrences)

[INFO] main.pas:45 - Variable 'temp' déclarée mais non utilisée
[INFO] config.pas:12 - TODO: Implémenter validation
```

## Outils de métriques de code

### 1. Comptage de lignes de code

**Outil en ligne de commande (multi-plateforme) :**

```bash
# Compter les lignes de code Pascal
find . -name "*.pas" -o -name "*.pp" | xargs wc -l
```

**Script Pascal pour statistiques :**

```pascal
program CodeMetrics;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  TotalLines, CodeLines, CommentLines, BlankLines: Integer;

procedure AnalyzeFile(const FileName: string);
var
  F: TextFile;
  Line: string;
begin
  AssignFile(F, FileName);
  Reset(F);
  try
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      Inc(TotalLines);

      Line := Trim(Line);
      if Line = '' then
        Inc(BlankLines)
      else if (Pos('//', Line) = 1) or (Pos('{', Line) = 1) then
        Inc(CommentLines)
      else
        Inc(CodeLines);
    end;
  finally
    CloseFile(F);
  end;
end;

begin
  TotalLines := 0;
  CodeLines := 0;
  CommentLines := 0;
  BlankLines := 0;

  // Analyser tous les fichiers .pas
  // (logique de parcours de répertoire à ajouter)

  WriteLn('=== Métriques de code ===');
  WriteLn('Lignes totales:     ', TotalLines);
  WriteLn('Lignes de code:     ', CodeLines);
  WriteLn('Lignes commentaires:', CommentLines);
  WriteLn('Lignes vides:       ', BlankLines);
end.
```

## Bonnes pratiques d'analyse statique

### 1. Intégrer tôt et souvent

- Analyser à chaque commit
- Automatiser dans le pipeline CI/CD
- Traiter les alertes rapidement

### 2. Configurer intelligemment

- Adapter les règles au projet
- Éviter les faux positifs excessifs
- Documenter les exceptions justifiées

### 3. Former l'équipe

- Expliquer la valeur de l'analyse
- Former aux outils utilisés
- Encourager la qualité du code

### 4. Suivre l'évolution

- Mesurer la tendance des métriques
- Célébrer les améliorations
- Identifier les zones problématiques

## Conclusion

L'analyse statique est un outil puissant pour améliorer la qualité du code FreePascal/Lazarus. Bien qu'il n'existe pas d'outil unique parfait pour FreePascal, la combinaison de :

- Avertissements du compilateur FPC
- Scripts personnalisés
- Revues de code
- Métriques manuelles

permet d'obtenir d'excellents résultats sur **Windows** et **Ubuntu**.

L'objectif n'est pas d'atteindre zéro alerte, mais de **détecter les vrais problèmes** et d'**améliorer continuellement** la qualité du code.

---

**Points clés à retenir :**

✅ L'analyse statique détecte les bugs sans exécuter le code  
✅ Le compilateur FPC offre déjà de bonnes analyses de base  
✅ Les scripts personnalisés sont efficaces et portables  
✅ L'analyse doit être intégrée au processus de développement  
✅ La qualité du code est un effort continu, pas un objectif ponctuel

⏭️ [Tests de performance et benchmarking](/18-tests-qualite-code/06-tests-performance-benchmarking.md)
