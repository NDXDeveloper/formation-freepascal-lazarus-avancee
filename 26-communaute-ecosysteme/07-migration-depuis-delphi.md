🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 26.7 Migration depuis Delphi

## Introduction

La migration depuis Delphi vers FreePascal/Lazarus représente l'une des transitions les plus fluides dans le monde du développement logiciel. Grâce à la compatibilité élevée entre Object Pascal de Delphi et FreePascal, ainsi qu'entre la VCL (Visual Component Library) et la LCL (Lazarus Component Library), des milliers de projets ont été portés avec succès ces dernières années.

Cette section vous guide à travers l'ensemble du processus de migration, depuis l'analyse initiale jusqu'au déploiement multi-plateforme d'un projet originellement développé sous Delphi pour Windows.

### Contexte historique

**Delphi et FreePascal : une histoire commune**

FreePascal n'est pas un simple "clone" de Delphi, mais un compilateur Pascal libre qui a été conçu dès le départ pour être compatible avec Turbo Pascal et Delphi. Cette compatibilité n'est pas accidentelle :

- **Années 1990** : Turbo Pascal (Borland) domine le marché
- **1995** : Sortie de Delphi 1, révolution avec la VCL et RAD
- **1997** : Début du projet FreePascal, visant la compatibilité
- **1999** : Démarrage du projet Lazarus (IDE visuel pour FPC)
- **2000s** : FreePascal atteint une excellente compatibilité Delphi 7
- **2010s** : Support progressif des versions modernes de Delphi
- **Aujourd'hui** : Compatibilité avec Delphi XE à Delphi 11/12

**Pourquoi cette compatibilité est importante :**

Les développeurs Delphi peuvent migrer vers FreePascal/Lazarus sans devoir réapprendre un nouveau langage ou paradigme. La courbe d'apprentissage est minimale, centrée principalement sur les différences d'IDE et quelques particularités du compilateur.

---

## Pourquoi migrer depuis Delphi ?

### Motivations courantes

**1. Coût des licences**

Delphi est un produit commercial avec des licences coûteuses :

| Version | Prix indicatif | Inclus |
|---------|---------------|--------|
| **Delphi Community** | Gratuit* | Limité (revenus < 5000$/an) |
| **Delphi Professional** | ~2000-3000 € | Windows + mobile limité |
| **Delphi Enterprise** | ~4000-5000 € | Full features |
| **Delphi Architect** | ~5000-6000 € | Toutes plateformes |

*Restrictions d'usage commercial

| FreePascal/Lazarus | Prix | Inclus |
|--------------------|------|--------|
| **Toutes versions** | **0 €** | Tout, sans restriction |

Pour une équipe de 5 développeurs : 10 000 à 30 000 € d'économie par an.

**2. Liberté et indépendance**

```
Delphi (propriétaire)          FreePascal/Lazarus (libre)
├── Dépend d'Embarcadero      ├── Communauté mondiale
├── Roadmap imposée            ├── Évolution collaborative
├── Support payant             ├── Support communautaire
├── Licences restrictives      ├── LGPL modifiée (permissive)
└── Risque d'abandon           └── Code source disponible
```

**Exemples historiques de risques avec Delphi :**
- Changements de propriétaire (Borland → CodeGear → Embarcadero)
- Suppressions de fonctionnalités entre versions
- Augmentations tarifaires importantes
- Support de certaines plateformes abandonné

**3. Portabilité multi-plateforme**

| Plateforme | Delphi Community | Delphi Pro/Enterprise | FreePascal/Lazarus |
|------------|------------------|----------------------|--------------------|
| **Windows 32-bit** | ✅ | ✅ | ✅ |
| **Windows 64-bit** | ✅ | ✅ | ✅ |
| **Linux 64-bit** | ❌ | ⚠️ (Enterprise+) | ✅ |
| **macOS** | ❌ | ⚠️ (Architect) | ✅ |
| **FreeBSD** | ❌ | ❌ | ✅ |
| **Raspberry Pi** | ❌ | ❌ | ✅ |
| **Android** | ⚠️ (limité) | ✅ | ✅ |
| **iOS** | ❌ | ⚠️ (Architect) | ⚠️ (expérimental) |

**FreePascal/Lazarus** : Un seul code source pour Windows, Linux et macOS sans surcoût.

**4. Modernisation et pérennité**

Migration = opportunité de :
- Nettoyer le code legacy
- Adopter les bonnes pratiques modernes
- Préparer l'avenir (open source, communauté active)
- Éliminer les dépendances obsolètes (BDE, composants tiers abandonnés)

**5. Exigences client ou réglementaires**

Certains contextes imposent l'open source :
- Administrations publiques (souveraineté numérique)
- Projets académiques et recherche
- Clients souhaitant éviter le vendor lock-in
- Secteurs nécessitant audit complet du code

---

## Compatibilité Delphi ↔ FreePascal/Lazarus

### Vue d'ensemble des compatibilités

```
┌─────────────────────────────────────────┐
│         DELPHI PROJECT                  │
├─────────────────────────────────────────┤
│ Langage Object Pascal       95-99%  ✅  │
│ RTL (Run-Time Library)      90-95%  ✅  │
│ VCL → LCL (Composants)      80-90%  ⚠️  │
│ Base de données             85-95%  ⚠️  │
│ Réseau (Indy/Synapse)       90-95%  ✅  │
│ API Windows                 100%    ✅  │
│ Composants tiers            Variable ⚠️  │
├─────────────────────────────────────────┤
│ Effort de migration global:             │
│ - Projet simple:      5-10% du code     │
│ - Projet moyen:      10-20% du code     │
│ - Projet complexe:   20-30% du code     │
└─────────────────────────────────────────┘
```

### Niveaux de compatibilité détaillés

#### Langage : 95-99% compatible

**Syntaxe Object Pascal :**

```pascal
// Ce code fonctionne identiquement en Delphi et FreePascal
type
  TPerson = class
  private
    FName: String;
    FAge: Integer;
  public
    constructor Create(const AName: String; AAge: Integer);
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

constructor TPerson.Create(const AName: String; AAge: Integer);  
begin
  FName := AName;
  FAge := AAge;
end;

// Génériques (Delphi 2009+)
type
  TMyList<T> = class(TList<T>)
  public
    procedure ProcessAll;
  end;
```

**Différences mineures du langage :**

| Fonctionnalité | Delphi | FreePascal | Note |
|----------------|--------|------------|------|
| **String = UnicodeString** | Depuis D2009 | {$modeswitch unicodestrings} | Compatible |
| **Inline variables** | Depuis D10.3 | En cours d'implémentation | À éviter |
| **Anonymous methods** | ✅ Depuis D2009 | ✅ Supporté | Compatible |
| **Generics** | ✅ | ✅ | Compatible |
| **Attributes** | ✅ | ✅ | Compatible |
| **RTTI extended** | ✅ | ✅ | Compatible |
| **Class helpers** | ✅ | ✅ | Compatible |
| **Record helpers** | ✅ | ✅ | Compatible |

**Compatibilité par version Delphi :**

- **Delphi 7 et antérieur** : ~99% compatible
- **Delphi 2007-2009** : ~98% compatible
- **Delphi XE-XE8** : ~97% compatible
- **Delphi 10.x-11.x** : ~95% compatible (fonctionnalités récentes à vérifier)

#### VCL → LCL : 80-90% compatible

La LCL (Lazarus Component Library) est inspirée de la VCL mais n'est pas un clone exact.

**Composants standards (haute compatibilité) :**

```pascal
// Ces composants sont quasi-identiques VCL ↔ LCL
TForm, TButton, TEdit, TLabel, TMemo, TListBox, TComboBox,  
TCheckBox, TRadioButton, TPanel, TGroupBox, TScrollBox,  
TImage, TBitmap, TCanvas, TTimer, TMainMenu, TPopupMenu,  
TToolBar, TStatusBar, TProgressBar, TTrackBar, TListView,  
TTreeView, TStringGrid, TTabControl, TPageControl, TSplitter
```

**Exemple de code portable VCL/LCL :**

```pascal
procedure TMainForm.Button1Click(Sender: TObject);  
begin
  Edit1.Text := 'Hello';
  Memo1.Lines.Add('New line');

  if CheckBox1.Checked then
    ShowMessage('Checked!')
  else
    ShowMessage('Not checked');

  ListBox1.Items.Add('Item ' + IntToStr(ListBox1.Count + 1));
end;
```

Ce code fonctionne **sans modification** en Delphi et Lazarus.

**Différences VCL/LCL nécessitant adaptation :**

| Aspect | VCL (Delphi) | LCL (Lazarus) | Effort |
|--------|--------------|---------------|--------|
| **Messages Windows** | `WM_*` | `LM_*` | Faible |
| **ClientHeight/Height** | Différents | Unifiés | Minimal |
| **Anchors/Constraints** | ✅ | ✅ Identique | Aucun |
| **Actions** | ✅ | ✅ Identique | Aucun |
| **Styles visuels** | Thèmes Windows | Widgetsets | Moyen |
| **TCanvas.TextOut** | ✅ | ✅ Identique | Aucun |

#### Bases de données : 85-95% compatible

**Remplacement des composants :**

| Delphi | FreePascal/Lazarus | Compatibilité |
|--------|-------------------|---------------|
| **ADO** | ZEOS / SQLdb | ⚠️ API différente |
| **BDE** | SQLdb | ⚠️ Migration nécessaire |
| **dbExpress** | ZEOS / SQLdb | ⚠️ API similaire |
| **FireDAC** | ZEOS / SQLdb | ⚠️ API différente |
| **TDataSet** | TDataSet | ✅ Identique |
| **TField** | TField | ✅ Identique |
| **TQuery/TTable** | TQuery/TTable | ✅ Quasi-identique |

**Code data-aware (haute compatibilité) :**

```pascal
// Composants data-aware identiques VCL ↔ LCL
TDBEdit, TDBMemo, TDBListBox, TDBComboBox, TDBCheckBox,  
TDBRadioGroup, TDBGrid, TDBImage, TDBNavigator, TDBLookupComboBox

// Utilisation identique
procedure TMainForm.FormCreate(Sender: TObject);  
begin
  DataSource1.DataSet := Query1;
  DBGrid1.DataSource := DataSource1;
  DBEdit1.DataSource := DataSource1;
  DBEdit1.DataField := 'Name';
end;
```

#### Réseau : 90-95% compatible

| Bibliothèque | Delphi | FreePascal/Lazarus | Note |
|--------------|--------|--------------------|------|
| **Indy** | ✅ Natif | ✅ Port disponible | Quasi-identique |
| **Synapse** | ⚠️ Tiers | ✅ Recommandé | Alternative légère |
| **TIdHTTP** | ✅ | ✅ | Compatible |
| **TIdTCPServer** | ✅ | ✅ | Compatible |
| **SSL/TLS** | ✅ OpenSSL | ✅ OpenSSL | Identique |

---

## Stratégies de migration

### Stratégie 1 : Migration complète (Big Bang)

**Principe :** Migrer tout le projet d'un coup.

```
Delphi Project
      ↓
  [MIGRATION]
      ↓
Lazarus Project
```

**Avantages :**
✅ Simple et direct  
✅ Pas de code dupliqué  
✅ Passage net à FreePascal  
✅ Rapidité (si petit projet)

**Inconvénients :**
❌ Risqué sur gros projets  
❌ Période de non-productivité  
❌ Tests massifs nécessaires  
❌ Retour arrière difficile

**Recommandé pour :**
- Projets < 50 000 lignes
- Projets simples (CRUD, formulaires)
- Peu de composants tiers
- Équipe disponible pour migration intensive

**Processus :**

```
Semaine 1-2 : Évaluation et planification  
Semaine 3-4 : Conversion automatique  
Semaine 5-6 : Correction des erreurs de compilation  
Semaine 7-8 : Adaptation des composants  
Semaine 9-12 : Tests et corrections  
Semaine 13+ : Déploiement progressif
```

### Stratégie 2 : Migration incrémentale (par module)

**Principe :** Migrer module par module, en maintenant deux versions parallèles.

```
Delphi Project                Lazarus Project
├── Module A (Delphi)    →    ├── Module A (migré) ✅
├── Module B (Delphi)    →    ├── Module B (migré) ✅
├── Module C (Delphi)         ├── Module C (en cours) ⚠️
└── Module D (Delphi)         └── Module D (à faire) ⏳
```

**Avantages :**
✅ Moins risqué  
✅ Tests progressifs  
✅ Apprentissage graduel  
✅ Production continue

**Inconvénients :**
❌ Plus long  
❌ Maintenance double temporairement  
❌ Gestion de la compatibilité  
❌ Coordination nécessaire

**Recommandé pour :**
- Projets > 100 000 lignes
- Architecture modulaire claire
- Impossibilité d'arrêter le développement
- Équipe devant maintenir l'existant

**Processus :**

```
Mois 1 : Module indépendant (ex: utilitaires)  
Mois 2 : Module avec peu de dépendances  
Mois 3-4 : Modules métier  
Mois 5-6 : Module principal et interface  
Mois 7+ : Finalisation et bascule
```

### Stratégie 3 : Réécriture partielle

**Principe :** Profiter de la migration pour réécrire les parties problématiques.

```
Delphi Project
├── Module Legacy (vieux code) → Réécriture moderne ✨
├── Module Standard → Migration simple ➡️
└── Module Tiers incompatible → Alternative LCL 🔄
```

**Avantages :**
✅ Code modernisé  
✅ Dette technique réduite  
✅ Meilleures pratiques  
✅ Documentation améliorée

**Inconvénients :**
❌ Effort plus important  
❌ Risque de régression  
❌ Délai plus long  
❌ Budget plus élevé

**Recommandé pour :**
- Code legacy de mauvaise qualité
- Composants tiers obsolètes
- Refonte fonctionnelle parallèle
- Budget disponible pour modernisation

### Stratégie 4 : Dual-maintenance (temporaire)

**Principe :** Maintenir les deux versions en parallèle pendant une période.

```
        ┌─────────────┐
        │ Code partagé│
        └──────┬──────┘
         ┌─────┴─────┐
    ┌────▼────┐  ┌───▼─────┐
    │ Delphi  │  │ Lazarus │
    │ version │  │ version │
    └─────────┘  └─────────┘
```

**Code partagé via directives :**

```pascal
unit SharedUnit;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, SysUtils;

type
  TMyClass = class
  public
    procedure DoSomething;
  end;

implementation

procedure TMyClass.DoSomething;  
begin
  {$IFDEF FPC}
  // Code spécifique Lazarus
  {$ELSE}
  // Code spécifique Delphi
  {$ENDIF}
end;

end.
```

**Avantages :**
✅ Aucune interruption de service  
✅ Tests A/B possibles  
✅ Rollback facile  
✅ Transition en douceur

**Inconvénients :**
❌ Double maintenance coûteuse  
❌ Code plus complexe (directives)  
❌ Synchronisation nécessaire  
❌ À limiter dans le temps

**Recommandé pour :**
- Applications critiques 24/7
- Période de transition longue nécessaire
- Validation progressive en production
- Clients multiples avec besoins différents

---

## Évaluation préalable : checklist

Avant de commencer toute migration, utilisez cette checklist :

### Analyse du projet existant

```markdown
## 1. Statistiques projet

- [ ] Version Delphi : _____________________
- [ ] Nombre de lignes de code : _____________________
- [ ] Nombre de formulaires : _____________________
- [ ] Nombre d'unités : _____________________
- [ ] Taille de l'équipe : _____________________
- [ ] Durée développement initial : _____________________

## 2. Dépendances

### Composants VCL
- [ ] Uniquement composants standard
- [ ] Quelques composants tiers (< 5)
- [ ] Nombreux composants tiers (> 5)
- [ ] Composants tiers critiques non portables

### Bibliothèques externes
- [ ] Pas de DLL externe
- [ ] DLL Windows standard (OK)
- [ ] DLL commerciales (licence à vérifier)
- [ ] Bibliothèques C/C++ (possibilité de recompiler)

### Bases de données
- [ ] ADO (nécessite remplacement)
- [ ] BDE (nécessite remplacement)
- [ ] dbExpress (nécessite adaptation)
- [ ] FireDAC (nécessite remplacement)
- [ ] Composants natifs (ex: MySQL Direct)

### Composants tiers populaires
- [ ] FastReport (version Lazarus existe ✅)
- [ ] ReportBuilder (LazReport comme alternative)
- [ ] Virtual TreeView (port Lazarus existe ✅)
- [ ] DevExpress (pas d'équivalent ❌)
- [ ] TMS Components (vérifier cas par cas)
- [ ] Indy (port Lazarus existe ✅)

## 3. Code spécifique

- [ ] Utilisation intensive API Windows
- [ ] Utilisation de COM/ActiveX
- [ ] Utilisation de la Registry Windows
- [ ] Code assembleur inline
- [ ] Multithreading complexe
- [ ] Manipulation de messages Windows

## 4. Fonctionnalités avancées

- [ ] DLL avec exports
- [ ] Services Windows
- [ ] Driver/Kernel mode (non portable)
- [ ] Intégration Office (COM)
- [ ] Cryptographie (vérifier équivalents)

## 5. Tests existants

- [ ] Aucun test automatisé
- [ ] Tests unitaires (DUnit)
- [ ] Tests d'intégration
- [ ] Tests de performance
- [ ] Documentation de test manuel

## 6. Contraintes

- [ ] Production ne peut pas s'arrêter
- [ ] Budget limité
- [ ] Délai court imposé
- [ ] Équipe doit apprendre Lazarus
- [ ] Client doit valider la migration
```

### Calcul du score de complexité

**Formule de complexité :**

```
Score = (
  Lignes_code / 10000 × 1 +
  Nb_formulaires × 0.5 +
  Composants_tiers_incompatibles × 5 +
  API_Windows_complexes × 3 +
  BDD_propriétaire × 2 +
  Absence_tests × 2
)

Complexité :
- Score < 10   : Simple ✅
- Score 10-25  : Moyenne ⚠️
- Score 25-50  : Élevée ⚠️⚠️
- Score > 50   : Très élevée ⚠️⚠️⚠️
```

**Exemple :**

```
Projet MyERP :
- 120 000 lignes de code : 12 points
- 80 formulaires : 40 points
- 3 composants DevExpress incompatibles : 15 points
- 5 APIs Windows complexes : 15 points
- ADO (BDD propriétaire) : 2 points
- Tests unitaires présents : 0 point

Total : 84 points → Complexité très élevée  
Recommandation : Migration incrémentale + réécriture partielle  
Durée estimée : 12-18 mois
```

---

## Prérequis techniques

### Installation de l'environnement

**1. Lazarus/FreePascal**

```bash
# Windows
# Télécharger depuis https://www.lazarus-ide.org/
# Installer lazarus-X.X.X-fpc-X.X.X-win64.exe

# Linux Ubuntu/Debian
sudo apt update  
sudo apt install lazarus lcl lcl-gtk2 lcl-qt5

# macOS
brew install lazarus
```

**2. Packages recommandés**

Via Online Package Manager (OPM) dans Lazarus :

```
- BGRABitmap (graphiques avancés)
- Virtual TreeView (si utilisé en Delphi)
- Synapse (réseau)
- ZEOS (bases de données multi-SGBD)
- LazReport (rapports)
```

**3. Outils de développement**

```
- Git (gestion de version)
- Diff tool (WinMerge/Meld/Beyond Compare)
- SQLite Browser (si migration depuis Access/Paradox)
- Debugger (GDB inclus avec Lazarus)
```

### Configuration recommandée

**Paramètres compilateur pour compatibilité Delphi :**

```pascal
// Dans les options du projet ou en en-tête d'unité
{$mode delphi}              // Mode de compatibilité Delphi
{$H+}                       // Long strings (AnsiString)
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN UNIT_DEPRECATED OFF}
```

**Options de compilation suggérées :**

```
Config → Compiler Options
├── Paths
│   ├── Other unit files: C:\MesUnits
│   └── Include files: C:\MesIncludes
├── Parsing
│   ├── Syntax mode: Delphi
│   └── String length: Long (255)
├── Code generation
│   ├── Target OS: Win64 / Linux
│   └── Target CPU: x86_64
└── Debugging
    ├── Debug info: -g
    └── Line numbers: -gl
```

---

## Ressources et support

### Documentation officielle

- **FreePascal Reference** : https://www.freepascal.org/docs.html
- **Lazarus Wiki** : https://wiki.lazarus.freepascal.org/
- **Guide de migration** : https://wiki.freepascal.org/Delphi_Converter
- **Compatibilité Delphi** : https://wiki.freepascal.org/Delphi_compatible

### Communauté et forums

- **Forum Lazarus** : https://forum.lazarus.freepascal.org/
  - Section "Third Party" : Composants tiers
  - Section "Beginners" : Questions débutants
  - Section "General" : Discussions générales

- **Forum FreePascal** : https://www.freepascal.org/forum.html

- **Stack Overflow** : Tag `freepascal` et `lazarus`

- **Reddit** : r/lazarus

- **Discord/Telegram** : Serveurs communautaires actifs

### Aide commerciale

**Support professionnel disponible :**

- Lazarus-CCR (communauté)
- Consultants FreePascal/Lazarus indépendants
- Sociétés spécialisées en migration Delphi

**Services typiques :**
- Audit de faisabilité
- Migration complète
- Formation équipe
- Support post-migration

### Comparaison avec alternatives

Si vous envisagez d'autres options que FreePascal/Lazarus :

| Alternative | Avantages | Inconvénients |
|-------------|-----------|---------------|
| **Rester sur Delphi** | Pas de migration | Coût licences |
| **Migrer vers C#/.NET** | Écosystème riche | Tout à réécrire |
| **Migrer vers Java** | Portable, populaire | Tout à réécrire |
| **Migrer vers C++** | Performance | Complexité accrue |
| **Migrer vers Python** | Simple, populaire | Performance moindre |
| **FreePascal/Lazarus** | Compatibilité élevée | Écosystème plus petit |

**Pourquoi FreePascal/Lazarus est souvent le meilleur choix pour Delphi :**

✅ **5-20% de réécriture** vs 100% dans les autres langages  
✅ **Compétences Pascal préservées** vs apprentissage nouveau langage  
✅ **Migration progressive possible** vs réécriture complète  
✅ **Coût minimal** vs investissement massif  
✅ **Multi-plateforme natif** sans frameworks lourds

---

## Témoignages et études de cas

### Cas réel 1 : Application de gestion commerciale

**Contexte :**
- Delphi 7, 200 000 lignes
- 120 formulaires
- Base de données Paradox (BDE)
- 2 composants tiers (FastReport, Virtual TreeView)

**Migration :**
- Durée : 6 mois
- Stratégie : Incrémentale par module
- Résultat : 85% de code conservé, 15% adapté/réécrit

**Gains :**
- Économie licences : 10 000 €/an
- Portabilité Linux obtenue
- Performance identique voire meilleure

### Cas réel 2 : Logiciel médical

**Contexte :**
- Delphi XE8, 500 000 lignes
- Nombreux composants DevExpress
- Base ADO + SQL Server
- Certification requise

**Migration :**
- Durée : 14 mois
- Stratégie : Réécriture partielle
- Composants DevExpress remplacés par LCL + custom

**Gains :**
- Indépendance vis-à-vis d'Embarcadero
- Version Linux pour hôpitaux sous Ubuntu
- Modernisation de l'UI
- Re-certification réussie

### Cas réel 3 : Outil scientifique

**Contexte :**
- Delphi 2007, 50 000 lignes
- Application simple (calculs + graphiques)
- Peu de composants tiers

**Migration :**
- Durée : 3 semaines
- Stratégie : Big Bang
- Résultat : 95% de code inchangé

**Gains :**
- Migration très rapide
- Versions Windows + Linux
- Distribution facilitée (open source)

---

## Conclusion de l'introduction

La migration depuis Delphi vers FreePascal/Lazarus est non seulement possible, mais souvent **fortement recommandée** pour des raisons économiques, de pérennité et de portabilité.

**Points clés à retenir :**

1. **Compatibilité élevée** : 80-95% selon les projets
2. **Choix de stratégie** : Adapter selon taille et contraintes
3. **Évaluation préalable** : Indispensable pour estimer l'effort
4. **Support communautaire** : Très actif et disponible
5. **ROI positif** : Généralement atteint en 1-2 ans

**Prochaines sections :**

Cette introduction générale vous a présenté les motivations, la compatibilité globale et les stratégies de migration. Les sections suivantes détaillent :

- **26.7.1 Portage de projets Windows** : Conversion technique complète, adaptation du code, gestion des composants
- **26.7.2 Adaptation pour Linux** : Spécificités Linux, chemins, packaging, déploiement

Commençons maintenant par le portage technique depuis Windows, avant d'aborder l'adaptation multi-plateforme vers Linux.

⏭️ [Portage de projets Windows](/26-communaute-ecosysteme/07.1-portage-projets-windows.md)
