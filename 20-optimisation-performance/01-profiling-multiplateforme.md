🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.1 Profiling Multi-plateforme

## Introduction

Le **profiling** (ou profilage en français) est l'une des compétences les plus importantes d'un développeur avancé. Il s'agit de l'art de mesurer et d'analyser les performances d'un programme pour identifier où et comment l'optimiser. Dans le contexte du développement multi-plateforme avec FreePascal/Lazarus, maîtriser les outils de profiling sur Windows et Linux est essentiel pour créer des applications performantes sur les deux systèmes.

## Qu'est-ce que le profiling ?

### Définition simple

Le profiling consiste à **observer un programme pendant son exécution** pour collecter des informations sur :

- **Le temps** : Combien de temps chaque fonction prend-elle ?
- **La mémoire** : Combien de mémoire est utilisée et où ?
- **Les appels** : Quelles fonctions appellent quelles autres fonctions ?
- **Les ressources** : Comment le CPU, le cache, et les autres ressources sont-ils utilisés ?

### Pourquoi profiler ?

L'optimisation prématurée est souvent citée comme "la racine de tous les maux" en programmation. Le profiling vous permet d'optimiser **au bon endroit** :

❌ **Sans profiling** :
```
"Je pense que cette boucle est lente, je vais l'optimiser..."
→ 2 heures de travail
→ Gain : 0.1% de performance
```

✅ **Avec profiling** :
```
"Le profiler montre que 80% du temps est dans la fonction X"
→ 30 minutes de travail sur X
→ Gain : 60% de performance globale
```

### La règle des 80/20 (Principe de Pareto)

En général, **80% du temps d'exécution** d'un programme est passé dans **20% du code**. Le profiling permet d'identifier précisément ces 20% critiques, appelés **hotspots** (points chauds).

## Types de profiling

Il existe plusieurs approches pour profiler un programme, chacune avec ses avantages et inconvénients.

### 1. Profiling par échantillonnage (Sampling)

**Principe** : Le profiler interrompt régulièrement le programme (par exemple toutes les 10ms) et enregistre quelle fonction est en cours d'exécution.

**Avantages** :
- Très faible overhead (< 5% de ralentissement)
- Adapté aux programmes en production
- Donne une vue statistique réaliste

**Inconvénients** :
- Moins précis pour les fonctions très rapides
- Peut manquer des événements rares

**Outils** :
- Windows : Intel VTune, Very Sleepy
- Linux : Perf
- Multi-plateforme : Profilers intégrés dans certains IDEs

**Exemple de résultat** :
```
Fonction                    Échantillons    % Total
--------------------------------------------------
TForm1.CalculComplexe       4,523          45.2%  
System.Move                 1,645          16.4%  
TDatabase.Query             987            9.8%
```

### 2. Profiling par instrumentation

**Principe** : Le profiler insère du code de mesure dans le programme (soit à la compilation, soit à l'exécution) pour enregistrer chaque entrée et sortie de fonction.

**Avantages** :
- Très précis, capture tous les appels
- Compte exact des appels et du timing
- Idéal pour analyser le flot d'exécution

**Inconvénients** :
- Overhead élevé (10x à 50x plus lent)
- Modifie légèrement le comportement du programme
- Génère beaucoup de données

**Outils** :
- Linux : Valgrind (Callgrind)
- Windows : Certains profilers commerciaux

**Exemple de résultat** :
```
Fonction                    Appels     Temps Self    Temps Total
-----------------------------------------------------------------
TForm1.CalculComplexe       1          2.450s        3.200s
  └─ TMath.Sqrt             1,000,000  0.650s        0.650s
  └─ TMath.Sin              1,000,000  0.750s        0.750s
```

### 3. Profiling manuel (chronométrage)

**Principe** : Le développeur insère manuellement du code pour mesurer le temps dans des sections spécifiques.

**Avantages** :
- Contrôle total sur ce qui est mesuré
- Aucun outil externe nécessaire
- Peut être laissé en production

**Inconvénients** :
- Fastidieux et source d'erreurs
- Ne donne qu'une vue partielle
- Difficile à maintenir

**Exemple en FreePascal** :
```pascal
uses SysUtils, DateUtils;

var
  StartTime, EndTime: TDateTime;
  ElapsedMs: Int64;
begin
  StartTime := Now;

  // Code à profiler
  CalculComplexe();

  EndTime := Now;
  ElapsedMs := MilliSecondsBetween(EndTime, StartTime);
  WriteLn('Temps écoulé : ', ElapsedMs, ' ms');
end;
```

### 4. Profiling mémoire

**Principe** : Analyse de l'utilisation de la mémoire (allocation, libération, fuites).

**Avantages** :
- Détecte les fuites mémoire
- Identifie les allocations excessives
- Améliore la stabilité

**Inconvénients** :
- Overhead souvent élevé
- Nécessite des outils spécialisés

**Outils** :
- Linux : Valgrind (Memcheck, Massif)
- Windows : Dr. Memory, Application Verifier
- Multi-plateforme : Heaptrc (intégré à FreePascal)

## Concepts fondamentaux du profiling

### Hotspot (Point chaud)

Un **hotspot** est une fonction ou une portion de code qui consomme une part significative du temps d'exécution total.

**Critères** :
- **> 20%** du temps total → Hotspot critique (à optimiser en priorité)
- **10-20%** → Hotspot important (optimisation recommandée)
- **5-10%** → Hotspot mineur (optimisation optionnelle)
- **< 5%** → Négligeable (ne pas perdre de temps dessus)

**Exemple** :
```
Si votre programme prend 10 secondes, et qu'une fonction prend 6 secondes,  
c'est un hotspot de 60% → TRÈS haute priorité d'optimisation
```

### Call Graph (Graphe d'appels)

Le **call graph** montre les relations entre les fonctions : qui appelle qui, et combien de fois.

**Exemple visuel** :
```
Main (100% du temps)
├─ InitApplication (2%)
├─ ProcessData (90%)
│  ├─ LoadFile (10%)
│  ├─ ParseData (30%)
│  │  └─ ValidateField (25%)
│  └─ SaveResults (50%)
└─ CleanUp (8%)
```

**Utilité** :
- Comprendre le contexte d'un hotspot
- Identifier les chaînes d'appels coûteuses
- Détecter les fonctions appelées trop souvent

### Self Time vs Total Time

Deux métriques fondamentales pour chaque fonction :

**Self Time (Temps propre)** :
- Temps passé **dans la fonction elle-même**
- N'inclut PAS le temps des sous-fonctions appelées
- Utile pour savoir si la fonction fait trop de calculs

**Total Time (Temps inclusif)** :
- Temps total incluant **les sous-fonctions**
- Temps de l'entrée à la sortie de la fonction
- Utile pour identifier les chemins critiques

**Exemple** :
```pascal
procedure Externe;       // Total: 10s, Self: 1s  
begin
  // 1s de travail propre
  Interne();            // 9s
end;

procedure Interne;       // Total: 9s, Self: 9s  
begin
  // 9s de travail propre
end;
```

Dans cet exemple :
- `Interne` est le vrai hotspot (9s de self time)
- `Externe` semble coûteuse en total time, mais elle-même est rapide

### IPC (Instructions Per Cycle)

**IPC** mesure l'efficacité du code au niveau du processeur :

- **IPC élevé (> 2.0)** : Code bien optimisé, utilise bien le pipeline CPU
- **IPC moyen (1.0-2.0)** : Code standard
- **IPC faible (< 1.0)** : Code inefficace, beaucoup d'attentes (cache miss, branch misprediction)

Cette métrique est avancée et nécessite des profilers matériels (Intel VTune, Perf avec compteurs PMU).

### Cache Miss

Le **cache miss** se produit quand le CPU doit aller chercher des données en RAM au lieu du cache (beaucoup plus lent).

**Impact** :
- Cache L1 hit : ~1 cycle
- Cache L2 hit : ~10 cycles
- Cache L3 hit : ~40 cycles
- RAM : ~200 cycles

Un taux de cache miss élevé (> 10%) indique souvent des problèmes de **localité des données**.

## Workflow de profiling

Un profiling efficace suit généralement ces étapes :

### 1. Mesurer d'abord (Baseline)

Avant toute optimisation, établissez une **baseline** (référence) :

```bash
# Exemple : temps d'exécution initial
$ time ./monprogramme
real    0m5.234s  
user    0m5.123s  
sys     0m0.089s
```

Notez cette valeur. C'est votre point de départ.

### 2. Profiler

Utilisez un profiler pour identifier les hotspots :

```bash
# Linux avec Perf
$ perf record -g ./monprogramme
$ perf report
```

### 3. Analyser

Identifiez les fonctions qui consomment le plus de temps :

```
45% → TDataProcessor.ParseCSV
20% → System.AnsiStrings.Copy
15% → TDatabase.ExecuteQuery
```

**Question à se poser** :
- Pourquoi cette fonction est-elle si coûteuse ?
- Est-elle appelée trop souvent ?
- Fait-elle des opérations inutiles ?
- Peut-on la remplacer par quelque chose de plus efficace ?

### 4. Optimiser

Concentrez-vous sur le hotspot #1 uniquement :

**Mauvaise approche** :
```
J'optimise 10 fonctions qui représentent chacune 2% du temps
→ Gain potentiel maximal : 20%
→ Beaucoup de travail pour peu de gain
```

**Bonne approche** :
```
J'optimise LA fonction qui représente 45% du temps
→ Gain potentiel : jusqu'à 45%
→ Un seul focus, maximum d'impact
```

### 5. Re-mesurer

Après optimisation, re-profiler pour vérifier le gain :

```bash
$ time ./monprogramme_optimise
real    0m2.987s    # Avant : 5.234s → Gain de 43% ✓
```

**Important** : Si le gain est négligeable ou inexistant, annulez l'optimisation et cherchez ailleurs.

### 6. Itérer

Répétez le processus jusqu'à atteindre vos objectifs de performance :

```
Cycle 1: 5.2s → 3.0s (optimisation parsing CSV)  
Cycle 2: 3.0s → 2.1s (optimisation requêtes DB)  
Cycle 3: 2.1s → 1.8s (cache des résultats)
→ Performance finale : 1.8s (65% plus rapide)
```

## Différences multi-plateformes

### Outils par plateforme

| Plateforme | Profiler CPU | Profiler Mémoire | Interface |
|------------|--------------|------------------|-----------|
| **Windows** | Intel VTune, Very Sleepy | Dr. Memory | GUI |
| **Linux** | Perf, Valgrind | Valgrind (Memcheck) | CLI + GUI |
| **macOS** | Instruments | Instruments | GUI |
| **Multi-plateforme** | Manuel (chrono) | HeapTrc (FPC) | Code |

### Différences de performance

**Attention** : Les performances peuvent varier significativement entre Windows et Linux pour le même code :

**Facteurs de variation** :
- **Compilateur** : Optimisations différentes selon l'OS
- **Bibliothèques** : GTK vs Win32 pour les interfaces
- **Système de fichiers** : NTFS vs ext4 ont des performances différentes
- **Gestion mémoire** : Allocateurs mémoire différents
- **Scheduler** : Ordonnancement des threads différent

**Exemple réel** :
```
Programme de traitement de fichiers :
- Windows : 5.2 secondes
- Linux : 3.8 secondes (27% plus rapide)

Raison : Les E/S fichiers sont plus rapides sur ext4 que NTFS
```

**Bonne pratique** : Profiler sur **les deux plateformes** si votre application doit être performante partout.

## Préparer son code pour le profiling

### 1. Compiler avec les symboles de débogage

Pour que les profilers puissent afficher les noms de fonctions et les numéros de lignes :

**Dans Lazarus** :
1. `Project` → `Project Options`
2. `Compiler Options` → `Debugging`
3. Cochez `Generate debug info for GDB (-g)`
4. Décochez `Strip symbols from executable (-Xs)`

**En ligne de commande** :
```bash
fpc -g -gl monprogramme.pas
```

**Important** : Les symboles de débogage n'affectent **pas** les performances mesurées, seulement la taille de l'exécutable.

### 2. Compiler avec ou sans optimisations ?

Deux approches selon l'objectif :

**Mode Debug (sans optimisations)** :
```bash
fpc -g monprogramme.pas
```
- Avantages : Code plus lisible dans le profiler, correspondance exacte avec le source
- Inconvénients : Ne reflète pas les performances réelles en production

**Mode Release (avec optimisations)** :
```bash
fpc -O3 -g monprogramme.pas
```
- Avantages : Performances réalistes, c'est ce qui sera déployé
- Inconvénients : Certaines fonctions peuvent être inline, le code peut être réorganisé

**Recommandation** :
- Profiler d'abord en **Release** pour avoir des mesures réalistes
- Si besoin de plus de détails, re-profiler en Debug certaines sections

### 3. Utiliser des builds séparés

Créez des configurations de build dans Lazarus :

```
Debug Build:
  - Optimisations : Désactivées
  - Symboles : Oui
  - Assertions : Activées
  - Usage : Développement quotidien

Release Build:
  - Optimisations : Niveau 3 (-O3)
  - Symboles : Oui (pour profiling)
  - Assertions : Désactivées
  - Usage : Tests de performance

Production Build:
  - Optimisations : Niveau 3 (-O3)
  - Symboles : Non (-Xs)
  - Assertions : Désactivées
  - Usage : Déploiement final
```

### 4. Éviter le code qui fausse les mesures

Certains patterns de code peuvent donner des résultats trompeurs :

**❌ Mauvais** : Chronomètre non représentatif
```pascal
// Code qui s'exécute en 0.001s
// Le profiler peut le manquer en mode sampling
procedure TresCourte;  
begin
  X := X + 1;
end;
```

**✓ Bon** : Code représentatif
```pascal
// Pour profiler du code très court, l'exécuter en boucle
procedure TestPerformance;  
var i: Integer;  
begin
  for i := 1 to 1000000 do
    TresCourte();  // Maintenant mesurable
end;
```

## Métriques importantes à surveiller

### Temps d'exécution

**Wall Clock Time (Temps réel)** :
- Temps total du début à la fin du programme
- Inclut TOUT : calculs, I/O, attentes, pauses
- C'est ce que l'utilisateur perçoit

**CPU Time (Temps CPU)** :
- Temps pendant lequel le CPU exécute réellement du code
- N'inclut PAS les attentes I/O
- Utile pour identifier le code coûteux en calculs

**Exemple** :
```
Programme qui lit un fichier de 1GB :
- Wall Clock Time : 30s
- CPU Time : 2s
→ Le programme passe 28s à attendre les I/O disque
→ Optimiser le code ne servira à rien, il faut optimiser les I/O
```

### Utilisation mémoire

**Heap Usage** :
- Mémoire allouée dynamiquement (GetMem, New, objets)
- Surveiller les fuites et allocations excessives

**Stack Usage** :
- Mémoire pour les variables locales et appels de fonctions
- Rarement un problème sauf récursion profonde

### Nombre d'appels

Le nombre de fois qu'une fonction est appelée :

```
Fonction A : 5% du temps, appelée 1 fois
→ Optimisation difficile, gain limité

Fonction B : 5% du temps, appelée 1,000,000 fois
→ Optimiser B et/ou réduire le nombre d'appels
```

## Pièges courants du profiling

### 1. Optimisation prématurée

```
"L'optimisation prématurée est la racine de tous les maux"
- Donald Knuth
```

**Ne jamais optimiser avant d'avoir profilé.** Votre intuition sur ce qui est lent est souvent fausse.

### 2. Micro-optimisations inutiles

Optimiser des détails qui représentent < 1% du temps total :

```pascal
// ❌ Perdre du temps sur ça
if (x = 0) then  // vs  if x = 0 then
```

**Impact réel** : 0.00001% de gain. Ne vaut pas la peine.

### 3. Effet observateur

Le simple fait de profiler peut changer le comportement du programme :

- Valgrind ralentit de 10x-50x → Chauffe moins → Moins de throttling CPU
- Profiling peut changer l'ordonnancement des threads
- Mesures chronométrées peuvent introduire des biais

**Solution** : Faire plusieurs mesures et comparer les moyennes.

### 4. Comparer des exécutions différentes

```
Test 1 : Programme seul → 5.2s  
Test 2 : Programme + Chrome + 10 tabs → 8.7s
```

**Conclusion erronée** : "Mon programme est devenu plus lent"  
**Réalité** : La charge système était différente  

**Solution** : Toujours profiler dans les **mêmes conditions** (même charge système, même température CPU, etc.)

### 5. Ignorer les variations

Un programme peut prendre 5.0s, puis 5.3s, puis 4.9s selon l'exécution (cache, scheduler, etc.).

**Solution** :
```bash
# Lancer 10 fois et prendre la moyenne
for i in {1..10}; do time ./monprogramme; done
```

## Outils complémentaires

En plus des profilers dédiés, plusieurs outils sont utiles :

### Commande `time`

**Linux/macOS** :
```bash
time ./monprogramme
```

**Windows PowerShell** :
```powershell
Measure-Command { .\monprogramme.exe }
```

### HeapTrc (FreePascal intégré)

FreePascal inclut un traceur de heap pour détecter les fuites mémoire :

**Compilation** :
```bash
fpc -gh monprogramme.pas
```

**Exécution** :
```bash
./monprogramme
```

À la fin de l'exécution, un rapport de fuites est affiché :

```
Heap dump by heaptrc unit
123 memory blocks allocated : 45678 bytes
120 memory blocks freed     : 45234 bytes
3 unfreed memory blocks : 444 bytes

Call trace for block $00123456 size 148
  $00401234  TMONOBJET__CREATE
  $00401567  main
```

### Benchmarking avec FPCUnit

Pour des micro-benchmarks reproductibles :

```pascal
uses fpcunit, testutils, SysUtils, DateUtils;

procedure TTestPerformance.TestOptimisation;  
var
  StartTime: TDateTime;
  i: Integer;
const
  Iterations = 1000000;
begin
  StartTime := Now;
  for i := 1 to Iterations do
    FonctionAOptimiser();

  WriteLn('Temps pour ', Iterations, ' itérations: ',
          MilliSecondsBetween(Now, StartTime), ' ms');
end;
```

## Quand profiler ?

### Pendant le développement

- **Rarement** : Focalisez sur la correction et la fonctionnalité d'abord
- **Sur demande** : Quand une fonctionnalité semble anormalement lente
- **Fin de sprint** : Check rapide avant de livrer

### Avant le déploiement

- **Tests de charge** : Profiler sous conditions réalistes (1000 utilisateurs, gros fichiers, etc.)
- **Comparaison baseline** : Est-ce plus rapide/lent que la version précédente ?
- **Profiling multi-plateforme** : Vérifier les performances sur Windows ET Linux

### En production (avec précaution)

- **Monitoring léger** : Temps de réponse, throughput
- **Profiling sampling** : Acceptable en production (overhead faible)
- **Jamais d'instrumentation** : Trop lent pour la production

## Objectifs de performance réalistes

Définissez des objectifs mesurables avant d'optimiser :

**Mauvais objectif** : "Rendre le programme plus rapide"  
**Bon objectif** : "Réduire le temps de chargement de 10s à 2s"  

**Exemples d'objectifs** :
- Temps de réponse < 100ms pour une requête
- Capacité de traiter 10,000 transactions/seconde
- Utilisation mémoire < 500 MB
- Temps de démarrage < 2 secondes

**Loi d'Amdahl** : Si une portion du code représente 50% du temps et que vous la rendez infiniment rapide, le gain maximal est de 50%.

```
Exemple :  
Programme : 10s total
- Partie A : 8s (80%)
- Partie B : 2s (20%)

Si on rend A infiniment rapide (0s) :  
Nouveau temps = 0s + 2s = 2s  
Gain maximal = 80% (même si A est infiniment optimisé)
```

## Prochaines étapes

Maintenant que vous comprenez les concepts fondamentaux du profiling multi-plateforme, les sections suivantes détailleront les outils spécifiques pour chaque système :

➡️ **Section 20.1.1 : Intel VTune (Windows)**
- Installation et configuration
- Utilisation pour identifier les hotspots
- Analyse des compteurs matériels
- Optimisations spécifiques Windows

➡️ **Section 20.1.2 : Perf et Valgrind (Linux)**
- Profiling avec Perf
- Détection de fuites mémoire avec Valgrind
- Callgrind pour analyse détaillée
- Optimisations spécifiques Linux

## Résumé des points clés

✅ **Profiler avant d'optimiser** : Ne jamais optimiser sans mesures  
✅ **Règle 80/20** : 80% du temps est dans 20% du code  
✅ **Focus sur les hotspots** : Optimiser ce qui compte vraiment  
✅ **Mesurer, optimiser, valider** : Workflow itératif  
✅ **Multi-plateforme** : Profiler sur Windows ET Linux  
✅ **Outils appropriés** : Sampling pour prod, instrumentation pour debug  
✅ **Symboles de débogage** : Essentiels pour des résultats lisibles  
✅ **Objectifs mesurables** : Définir ce qu'on veut atteindre

Le profiling est une compétence qui s'acquiert avec la pratique. Plus vous profilez, plus vous développerez une intuition sur ce qui peut causer des problèmes de performance, mais **toujours validez avec des mesures réelles**.

---

*Note : Ce tutoriel fait partie de la formation "FreePascal/Lazarus - Niveau Développeur Avancé - Edition Multi-plateforme Windows/Ubuntu"*

⏭️ [Intel VTune (Windows)](/20-optimisation-performance/01.1-intel-vtune-windows.md)
