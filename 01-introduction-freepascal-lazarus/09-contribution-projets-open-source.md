🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.9 Contribution aux projets open source FreePascal/Lazarus

## Introduction : Pourquoi contribuer ?

### L'esprit open source

FreePascal et Lazarus existent grâce aux **contributions de centaines de développeurs** à travers le monde. Ces projets incarnent la philosophie du logiciel libre :

```
Utilisateurs → Testeurs → Contributeurs → Mainteneurs
     ↑                                          ↓
     └──────────── Cycle vertueux ──────────────┘
```

### Les bénéfices de la contribution

#### Pour vous
- **Apprentissage** : Lire du code de qualité professionnelle
- **Compétences** : Développer votre expertise
- **Réseau** : Rencontrer des développeurs expérimentés
- **Portfolio** : Contributions visibles publiquement
- **Satisfaction** : Aider des milliers d'utilisateurs

#### Pour la communauté
- **Amélioration** : Bugs corrigés, fonctionnalités ajoutées
- **Documentation** : Guides et exemples enrichis
- **Support** : Aide aux nouveaux utilisateurs
- **Pérennité** : Projet maintenu et vivant
- **Innovation** : Nouvelles idées et approches

### Types de contributions

```
Contributions possibles
├── Code
│   ├── Corrections de bugs
│   ├── Nouvelles fonctionnalités
│   └── Optimisations
├── Documentation
│   ├── Traductions
│   ├── Tutoriels
│   └── Exemples
├── Tests
│   ├── Rapports de bugs
│   ├── Tests de régression
│   └── Validation
└── Communauté
    ├── Support forum
    ├── Mentorat
    └── Évangélisation
```

## Comprendre l'organisation des projets

### Structure de gouvernance

#### FreePascal

```
Core Team (5-10 membres)
    ├── Florian Klämpfl (Fondateur, Leader)
    ├── Jonas Maebe (Architecte principal)
    ├── Michael Van Canneyt (Documentation, FCL)
    └── ...

Développeurs avec commit (20-30)
    ├── Maintainers de plateformes
    ├── Experts domaines
    └── Contributeurs réguliers

Contributeurs externes (100+)
    └── Patches et suggestions
```

#### Lazarus

```
Core Team
    ├── Mattias Gaertner (Leader technique)
    ├── Juha Manninen (Éditeur, IDE)
    ├── Ondrej Pokorny (Windows, Delphi compat)
    └── ...

Maintainers de packages
    ├── BGRABitmap
    ├── VirtualTreeView
    └── Autres composants

Communauté
    └── Contributeurs occasionnels
```

### Infrastructure technique

```
Infrastructure FreePascal/Lazarus
├── GitLab (Code source)
│   ├── https://gitlab.com/freepascal.org/fpc/source
│   └── https://gitlab.com/freepascal.org/lazarus/lazarus
├── Bug Tracker
│   ├── https://gitlab.com/freepascal.org/fpc/source/-/issues
│   └── https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues
├── Wiki
│   └── https://wiki.freepascal.org
├── Forum
│   └── https://forum.lazarus.freepascal.org
└── Mailing Lists
    ├── fpc-devel
    ├── fpc-pascal
    └── lazarus
```

## Première étape : Devenir utilisateur actif

### 1. Utiliser intensivement

Avant de contribuer, il faut **bien connaître** les outils :

```pascal
// Testez les limites
// Explorez les fonctionnalités
// Identifiez les problèmes
// Notez les améliorations possibles
```

### 2. Participer aux forums

#### S'inscrire et se présenter

```
Forum Lazarus : https://forum.lazarus.freepascal.org

Message type de présentation :
"Bonjour,
Je suis [Nom], développeur Pascal depuis [X] ans.  
J'utilise Lazarus pour [type de projets].  
Intéressé par [domaines].  
Heureux de rejoindre la communauté !"
```

#### Commencer à aider

- Répondre aux questions simples
- Partager vos solutions
- Signaler les doublons
- Remercier les contributeurs

### 3. Explorer le wiki

```
Wiki FreePascal : https://wiki.freepascal.org

Pages importantes :
├── Getting Started
├── FAQ
├── Platform specific
├── Component Library
└── Developer pages
```

Commencez par :
- Corriger les typos
- Mettre à jour les liens morts
- Ajouter des exemples
- Traduire des pages

## Reporter des bugs efficacement

### Avant de reporter

#### Vérifier que c'est vraiment un bug

```bash
# 1. Reproduire le problème
# 2. Tester avec la dernière version
# 3. Chercher si déjà reporté
# 4. Isoler le problème
```

#### Rechercher les bugs existants

```
GitLab Issues → Search
- Mots-clés en anglais
- Vérifier les bugs fermés aussi
- Lire les discussions similaires
```

### Format d'un bon rapport de bug

````markdown
## Description
Brève description du problème

## Étapes pour reproduire
1. Créer nouveau projet
2. Ajouter composant X
3. Compiler
4. → Erreur

## Comportement attendu
Le programme devrait compiler sans erreur

## Comportement actuel
Erreur : "Access Violation at address..."

## Code minimal de reproduction
```pascal
program BugDemo;  
begin
  // Code minimal qui reproduit le bug
end.
```

## Environnement
- OS : Ubuntu 22.04 64-bit
- FPC : 3.2.2
- Lazarus : 3.0.0
- CPU : x86_64

## Informations supplémentaires
- Screenshots si pertinent
- Logs complets
- Workaround si trouvé
````

### Exemple de bug bien reporté

````markdown
Title: TStringGrid.OnDrawCell not called when ScrollBars = ssNone

## Description
The OnDrawCell event is not triggered when ScrollBars property is set to ssNone

## Steps to reproduce
1. Drop a TStringGrid on form
2. Set ScrollBars := ssNone
3. Implement OnDrawCell event
4. Run → Event never fires

## Expected behavior
OnDrawCell should fire regardless of ScrollBars setting

## Actual behavior
OnDrawCell only works when ScrollBars <> ssNone

## Minimal reproduction code
```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin
  StringGrid1.ScrollBars := ssNone;  // Bug trigger
  StringGrid1.OnDrawCell := @StringGrid1DrawCell;
end;

procedure TForm1.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  ShowMessage('DrawCell called');  // Never shows
end;
```

## Environment
- OS: Windows 10 21H2
- Lazarus: 3.0.0
- FPC: 3.2.2
- Widgetset: Win32

## Workaround
Set ScrollBars := ssAutoVertical and hide scrollbar with Windows API
````

## Contribuer du code

### Préparer son environnement

#### 1. Cloner les dépôts

```bash
# Créer structure de développement
mkdir ~/fpc-dev  
cd ~/fpc-dev

# Cloner FPC
git clone https://gitlab.com/freepascal.org/fpc/source.git fpc

# Cloner Lazarus
git clone https://gitlab.com/freepascal.org/lazarus/lazarus.git lazarus

# Structure résultante
~/fpc-dev/
├── fpc/        # Sources FPC
├── lazarus/    # Sources Lazarus
└── patches/    # Vos modifications
```

#### 2. Compiler depuis les sources

```bash
# Compiler FPC
cd ~/fpc-dev/fpc  
make clean all  
sudo make install PREFIX=/usr/local

# Compiler Lazarus
cd ~/fpc-dev/lazarus  
make clean all
./lazarus --pcp=~/.lazarus-dev  # Config séparée
```

#### 3. Créer une branche de travail

```bash
# Pour une correction de bug
git checkout -b fix-issue-12345

# Pour une nouvelle fonctionnalité
git checkout -b feature-new-component

# Pour de la documentation
git checkout -b doc-update-install-guide
```

### Standards de code

#### Style FreePascal

```pascal
// Indentation : 2 espaces
// Pas de tabs
// Ligne max : 80-100 caractères

type
  { TMyClass }
  TMyClass = class(TObject)
  private
    FValue: Integer;
    procedure SetValue(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    property Value: Integer read FValue write SetValue;
  end;

{ TMyClass }

constructor TMyClass.Create;  
begin
  inherited Create;
  FValue := 0;
end;

destructor TMyClass.Destroy;  
begin
  // Cleanup
  inherited Destroy;
end;

procedure TMyClass.SetValue(AValue: Integer);  
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    // Notify changes if needed
  end;
end;
```

#### Conventions de nommage

```pascal
// Types : T prefix
type
  TCustomWidget = class;

// Interfaces : I prefix
type
  ISerializable = interface;

// Fields : F prefix
private
  FWidth: Integer;

// Arguments : A prefix
procedure SetWidth(AWidth: Integer);

// Events : On prefix
property OnChange: TNotifyEvent;

// Constants : Uppercase
const
  DEFAULT_WIDTH = 100;
```

### Processus de contribution

#### 1. Développer la modification

```pascal
// Exemple : Ajouter une propriété à un composant
unit ExtCtrls;

type
  TPanel = class(TCustomPanel)
  private
    FCornerRadius: Integer;  // Nouvelle propriété
    procedure SetCornerRadius(AValue: Integer);
  protected
    procedure Paint; override;  // Modifier pour coins arrondis
  published
    property CornerRadius: Integer
      read FCornerRadius write SetCornerRadius default 0;
  end;

implementation

procedure TPanel.SetCornerRadius(AValue: Integer);  
begin
  if FCornerRadius <> AValue then
  begin
    FCornerRadius := AValue;
    Invalidate;  // Redessiner
  end;
end;

procedure TPanel.Paint;  
begin
  if FCornerRadius > 0 then
    // Dessiner panel avec coins arrondis
    Canvas.RoundRect(ClientRect, FCornerRadius, FCornerRadius)
  else
    inherited Paint;  // Comportement normal
end;
```

#### 2. Tester exhaustivement

```bash
# Tests unitaires
cd ~/fpc-dev/fpc/tests  
make full

# Tests Lazarus
cd ~/fpc-dev/lazarus/test
./runtests

# Créer test pour votre modification
# tests/test_panel_corner.pas
```

#### 3. Créer le patch

```bash
# Commiter localement
git add -A  
git commit -m "LCL: Add CornerRadius property to TPanel

- Adds CornerRadius property to allow rounded corners
- Overrides Paint method to handle rounded drawing
- Maintains backward compatibility (default = 0)
- Fixes issue #12345"

# Générer le patch
git format-patch -1 HEAD
# Crée : 0001-LCL-Add-CornerRadius-property-to-TPanel.patch
```

#### 4. Soumettre la contribution

##### Via GitLab (recommandé)

```bash
# Fork le projet sur GitLab
# https://gitlab.com/freepascal.org/lazarus/lazarus/-/forks/new

# Ajouter votre fork comme remote
git remote add myfork https://gitlab.com/yourusername/lazarus.git

# Pousser votre branche
git push myfork fix-issue-12345

# Créer Merge Request sur GitLab
# - Description claire
# - Référencer l'issue
# - Assigner aux maintainers
```

##### Via Patch sur bug tracker

```
1. Aller sur l'issue GitLab
2. Attacher le fichier .patch
3. Commenter : "Patch attached that fixes this issue"
4. Attendre review
```

### Exemple de Merge Request

```markdown
## Description
This MR adds a CornerRadius property to TPanel allowing rounded corners.

## What does this MR do?
- Adds `CornerRadius` property to TPanel
- Overrides Paint method to draw rounded rectangles when CornerRadius > 0
- Maintains full backward compatibility

## Screenshots
Before: [image]  
After: [image with rounded corners]

## Test plan
1. Drop TPanel on form
2. Set CornerRadius to 10
3. Run → Panel has rounded corners
4. Set to 0 → Normal rectangular panel

## Related issues
Fixes #12345

## Checklist
- [x] Code follows project style
- [x] Tests added/updated
- [x] Documentation updated
- [x] Tested on Windows, Linux, macOS
- [x] No regression in existing functionality
```

## Contribuer à la documentation

### Types de documentation

```
Documentation FreePascal/Lazarus
├── Référence API (XML)
├── Wiki (MediaWiki)
├── Exemples de code
├── Tutoriels
├── Guides d'installation
└── Notes de version
```

### Documentation XML

```xml
<!-- Documentation format pour FPC/Lazarus -->
<element name="TPanel.CornerRadius">
  <short>Specifies the radius for rounded corners</short>
  <descr>
    <p>
    CornerRadius determines the radius in pixels used to draw
    rounded corners for the panel. When set to 0 (default),
    the panel is drawn with normal rectangular corners.
    </p>
    <p>
    Values greater than 0 produce increasingly rounded corners,
    up to a maximum of half the smallest dimension of the panel.
    </p>
  </descr>
  <seealso>
    <link id="TPanel.Paint"/>
    <link id="TCanvas.RoundRect"/>
  </seealso>
  <example file="examples/panel_rounded.pas"/>
</element>
```

### Contribution au Wiki

#### Créer un compte

```
1. Aller sur https://wiki.freepascal.org
2. Create account (coin supérieur droit)
3. Confirmer email
4. Se connecter
```

#### Format Wiki (MediaWiki)

```mediawiki
== Rounded Panel Example ==

This example shows how to use the CornerRadius property:

<syntaxhighlight lang="pascal">
procedure TForm1.FormCreate(Sender: TObject);  
begin
  Panel1.CornerRadius := 15;
  Panel1.Color := clSkyBlue;
  Panel1.Caption := 'Rounded Panel';
end;
</syntaxhighlight>

=== See also ===
* [[TPanel]]
* [[Custom Drawing]]
* [[Canvas Methods]]

[[Category:Components]]
[[Category:LCL]]
```

### Traductions

#### Processus de traduction

```
1. Choisir une langue cible
2. Utiliser les fichiers .po
3. Traduire avec PoEdit ou équivalent
4. Tester l'interface traduite
5. Soumettre les fichiers
```

#### Fichier de traduction (.po)

```po
# lazarus.fr.po - Traduction française

msgid "Corner Radius"  
msgstr "Rayon des coins"

msgid "Specifies the radius for rounded corners"  
msgstr "Spécifie le rayon pour les coins arrondis"

msgid "The value must be positive"  
msgstr "La valeur doit être positive"
```

## Contribuer des packages et composants

### Créer un package

```pascal
// mycomponent.lpk - Package Lazarus

<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <Package Version="4">
    <Name Value="MyComponent"/>
    <Type Value="RunAndDesignTime"/>
    <Author Value="Your Name"/>
    <CompilerOptions>
      <SearchPaths>
        <UnitOutputDirectory Value="lib/$(TargetCPU)-$(TargetOS)"/>
      </SearchPaths>
    </CompilerOptions>
    <Description Value="Amazing new component"/>
    <License Value="LGPL-2.1"/>
    <Version Major="1" Minor="0" Release="0"/>
    <Files Count="2">
      <Item1>
        <Filename Value="mycomponent.pas"/>
        <HasRegisterProc Value="True"/>
      </Item1>
      <Item2>
        <Filename Value="mycomponent_icon.lrs"/>
      </Item2>
    </Files>
    <RequiredPkgs Count="2">
      <Item1>
        <PackageName Value="LCL"/>
      </Item1>
      <Item2>
        <PackageName Value="FCL"/>
      </Item2>
    </RequiredPkgs>
  </Package>
</CONFIG>
```

### Publier sur OPM (Online Package Manager)

```json
// update_mycomponent.json
{
  "UpdatePackageData": {
    "DisableInOPM": false,
    "DownloadZipURL": "https://github.com/user/mycomponent/archive/v1.0.0.zip",
    "Name": "mycomponent"
  },
  "UpdatePackageFiles": [
    {
      "ForceNotify": false,
      "InternalVersion": 1,
      "Name": "mycomponent.lpk",
      "Version": "1.0.0.0"
    }
  ]
}
```

## Participer à la communauté

### Répondre sur les forums

#### Bonnes pratiques

````markdown
Réponse type utile :

"Bonjour,

Pour résoudre ce problème, vous pouvez :

1. **Solution rapide** :
   ```pascal
   // Code solution
   ```

2. **Explication** :
   Le problème vient de...

3. **Alternative** :
   Vous pourriez aussi...

**Documentation** : [Lien vers wiki]

J'espère que cela aide !"
````

### Mentorat

#### Accueillir les nouveaux

- Répondre patiemment
- Diriger vers les ressources
- Encourager les contributions
- Partager votre expérience

### Évangélisation

#### Promouvoir FreePascal/Lazarus

- Articles de blog
- Présentations en meetups
- Projets open source
- Vidéos tutorielles
- Posts réseaux sociaux

## Processus de review

### Ce qui est évalué

```
Review Checklist
├── Qualité du code
│   ├── Style cohérent
│   ├── Pas de régression
│   └── Performance acceptable
├── Tests
│   ├── Tests unitaires
│   ├── Tests multi-plateformes
│   └── Cas limites couverts
├── Documentation
│   ├── Commentaires code
│   ├── Documentation API
│   └── Exemples si nécessaire
└── Compatibilité
    ├── Backward compatible
    ├── Multi-plateforme
    └── Pas de breaking changes
```

### Répondre aux reviews

```markdown
Réponse aux commentaires :

> Le nom de variable n'est pas clair

Corrigé dans le commit abc123. Renommé en `FCornerRadiusValue`.

> Manque un test pour valeurs négatives

Ajouté test dans commit def456. Vérifie que les valeurs < 0  
sont rejetées avec exception appropriée.

> Performance concern avec Invalidate

Bonne remarque. J'ai ajouté un flag pour éviter les Invalidate  
multiples. Voir commit ghi789.
```

## Devenir maintainer

### Progression typique

```
1. Utilisateur actif (0-6 mois)
   └── Utilisation, questions, apprentissage

2. Contributeur occasionnel (6-12 mois)
   └── Bugs reports, petits patches, wiki

3. Contributeur régulier (1-2 ans)
   └── Patches importants, reviews, aide communauté

4. Committer (2-3 ans)
   └── Accès écriture sur certaines parties

5. Core Team (3+ ans)
   └── Décisions architecture, releases
```

### Responsabilités d'un maintainer

- Review des contributions
- Correction bugs critiques
- Documentation à jour
- Support communauté
- Participation décisions
- Releases management

## Ressources et liens utiles

### Liens essentiels

```
Développement
├── GitLab FPC : https://gitlab.com/freepascal.org/fpc/source
├── GitLab Lazarus : https://gitlab.com/freepascal.org/lazarus/lazarus
├── Bug Tracker : https://gitlab.com/.../issues
└── Wiki : https://wiki.freepascal.org

Communication
├── Forum : https://forum.lazarus.freepascal.org
├── Mailing lists : https://lists.freepascal.org
├── Discord : https://discord.gg/lazarus
└── IRC : #fpc et #lazarus-ide sur Libera.Chat

Documentation
├── Contributor Guide : https://wiki.freepascal.org/Contributing
├── Code Style : https://wiki.freepascal.org/Coding_style
└── Git Workflow : https://wiki.freepascal.org/Git_mirrors
```

### Outils pour contributeurs

```bash
# Scripts utiles
~/fpc-dev/scripts/
├── build-all.sh       # Compiler FPC+Lazarus
├── run-tests.sh       # Lancer tests
├── create-patch.sh    # Générer patch
└── check-style.sh     # Vérifier style code
```

## Conclusion

### Commencer petit

Votre première contribution n'a pas besoin d'être énorme :
- Corriger une typo dans la doc
- Reporter un bug clairement
- Répondre à une question sur le forum
- Traduire une page wiki

### Être patient

- Les reviews prennent du temps
- Les maintainers sont bénévoles
- La qualité prime sur la rapidité
- Apprendre prend du temps

### Rester positif

- Accepter les critiques constructives
- Apprendre des reviews
- Célébrer les succès
- Remercier les autres contributeurs

### Le cycle vertueux

```
Utiliser → Apprendre → Contribuer → Améliorer
    ↑                                    ↓
    └──────── FreePascal/Lazarus ────────┘
```

Chaque contribution, même minime, renforce l'écosystème FreePascal/Lazarus. Votre aide est précieuse et appréciée !

**Bienvenue dans la communauté des contributeurs !**

⏭️ 2. [Maîtrise de l'IDE Lazarus](/02-maitrise-ide-lazarus/README.md)
