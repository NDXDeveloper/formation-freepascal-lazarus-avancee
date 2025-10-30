🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.8 Temps réel et RTOS

## Introduction

Le **temps réel** en informatique ne signifie pas forcément "très rapide", mais plutôt "prévisible et déterministe". Un système temps réel garantit qu'une tâche sera exécutée dans un délai précis et constant. C'est essentiel pour les systèmes embarqués critiques (avionique, médical, automobile, robotique).

Un **RTOS** (Real-Time Operating System) est un système d'exploitation spécialement conçu pour ces contraintes temporelles strictes.

## Concepts fondamentaux du temps réel

### Temps réel strict vs souple

**Temps réel strict (Hard Real-Time)**
- Le non-respect d'une échéance peut avoir des conséquences catastrophiques
- Exemples : système de freinage ABS, contrôle de vol d'avion, pacemaker
- Déterminisme absolu requis

**Temps réel souple (Soft Real-Time)**
- Le dépassement d'échéance dégrade la qualité mais n'est pas critique
- Exemples : streaming vidéo, interface utilisateur réactive, jeu vidéo
- Performance moyenne acceptable

### Déterminisme

Le déterminisme signifie que le système répond toujours dans le même délai, quelles que soient les circonstances :

```pascal
// Exemple conceptuel de comportement déterministe
procedure TacheTempsReel;
var
  DebutExecution, FinExecution: TDateTime;
begin
  DebutExecution := Now;

  // Exécution de la tâche critique
  TraiterCapteur();
  CommanderActionneur();

  FinExecution := Now;

  // En temps réel strict, cette durée doit TOUJOURS être < 10ms
  // Pas de variation acceptable
  if MillisecondsBetween(FinExecution, DebutExecution) > 10 then
    // ERREUR CRITIQUE : échéance dépassée
    DeclencherAlarme();
end;
```

### Latence et Jitter

**Latence** : délai entre un événement et sa réponse
**Jitter** : variation de cette latence dans le temps

```
Système non temps réel :
Réponse 1: 15ms
Réponse 2: 8ms
Réponse 3: 45ms  ← Jitter élevé, imprévisible
Réponse 4: 12ms

Système temps réel :
Réponse 1: 10ms
Réponse 2: 10ms
Réponse 3: 10ms  ← Jitter minimal, prévisible
Réponse 4: 10ms
```

## Pourquoi les OS classiques ne conviennent pas

Windows et Linux standards (Ubuntu Desktop) ne sont **pas** des RTOS car :

1. **Ordonnancement non préemptif prioritaire** : d'autres tâches peuvent bloquer une tâche critique
2. **Gestion mémoire virtuelle** : les défauts de page créent des latences imprévisibles
3. **Interruptions non maîtrisées** : le système peut être interrompu n'importe quand
4. **Services en arrière-plan** : antivirus, indexation, mises à jour...

```pascal
// Sur Windows/Linux standard, ce code n'est PAS temps réel
procedure BoucleControle;
begin
  while True do
  begin
    Sleep(10); // ⚠️ Pas précis ! Peut durer 10-50ms selon la charge CPU
    LireCapteur();
    CalculerCommande();
    EnvoyerActuateur();
  end;
end;
```

## RTOS populaires compatibles FreePascal

### FreeRTOS

**Caractéristiques** :
- RTOS le plus populaire au monde
- Open source (licence MIT)
- Très léger (< 10 Ko)
- Support ARM, AVR, x86, RISC-V...
- Ordonnancement préemptif à priorités

**Concepts clés** :

```pascal
// Pseudo-code FreePascal style pour FreeRTOS
type
  TTaskHandle = pointer;

// Création d'une tâche temps réel
procedure TacheCapteursTemperature(Parametres: Pointer);
begin
  while True do
  begin
    Temperature := LireADC();
    if Temperature > SeuilAlerte then
      EnvoyerAlarme();

    // Attente précise de 100ms (tick RTOS)
    vTaskDelay(pdMS_TO_TICKS(100));
  end;
end;

// Dans le programme principal
var
  HandleTache: TTaskHandle;
begin
  // Créer la tâche avec priorité 2
  xTaskCreate(@TacheCapteursTemperature, 'Capteurs',
              128, nil, 2, @HandleTache);

  // Démarrer l'ordonnanceur RTOS
  vTaskStartScheduler();
end;
```

### RTLinux / PREEMPT_RT

**RTLinux** (abandonné) et **PREEMPT_RT** (patch pour Linux) transforment Linux en RTOS :

- Noyau Linux modifié pour être préemptible
- Latences < 100 µs possibles
- Compatible avec les applications Linux standards
- Utilisé en industrie, robotique, CNC

```pascal
// Sur Linux PREEMPT_RT avec FreePascal
uses
  BaseUnix, Unix;

procedure ConfigurerPrioriteTempsReel;
var
  Param: sched_param;
begin
  // Politique SCHED_FIFO (temps réel strict)
  Param.sched_priority := 50;

  if sched_setscheduler(0, SCHED_FIFO, @Param) = -1 then
    WriteLn('Erreur : droits root requis pour temps réel');
end;
```

### Zephyr

RTOS moderne et modulaire :
- Support massif des microcontrôleurs
- Architecture moderne
- Écosystème en croissance

## Ordonnancement temps réel

### Priorités statiques

Les tâches ont des priorités fixes. La tâche de plus haute priorité prête s'exécute toujours.

```pascal
// Configuration de priorités
const
  PRIORITE_SECURITE = 10;    // Plus haute priorité
  PRIORITE_CONTROLE = 5;     // Priorité moyenne
  PRIORITE_AFFICHAGE = 1;    // Plus basse priorité

procedure CreerTaches;
begin
  CreerTache(@TacheSecurite, PRIORITE_SECURITE);
  CreerTache(@TacheControle, PRIORITE_CONTROLE);
  CreerTache(@TacheAffichage, PRIORITE_AFFICHAGE);
end;
```

### Ordonnancement Rate Monotonic (RM)

Algorithme optimal pour tâches périodiques :
- Plus la période est courte, plus la priorité est élevée

```
Tâche A : période 10ms  → Priorité 3 (haute)
Tâche B : période 50ms  → Priorité 2 (moyenne)
Tâche C : période 100ms → Priorité 1 (basse)
```

### Ordonnancement Earliest Deadline First (EDF)

La tâche dont l'échéance est la plus proche s'exécute en premier :

```pascal
type
  TTacheTempsReel = record
    Nom: string;
    Echeance: TDateTime;
    Procedure_: TProcedure;
  end;

// L'ordonnanceur choisit la tâche avec l'échéance la plus proche
```

## Synchronisation et communication

### Sémaphores

Mécanisme de synchronisation binaire ou compteur :

```pascal
// Pseudo-code conceptuel
var
  SemaphoreRessource: TSemaphore;

// Producteur
procedure ProducteurDonnees;
begin
  while True do
  begin
    ProduireDonnee();
    SemaphoreSignal(SemaphoreRessource); // Libère pour le consommateur
    vTaskDelay(100);
  end;
end;

// Consommateur
procedure ConsommateurDonnees;
begin
  while True do
  begin
    SemaphoreWait(SemaphoreRessource); // Attend que des données soient prêtes
    TraiterDonnee();
  end;
end;
```

### Mutex (Mutual Exclusion)

Protection de sections critiques :

```pascal
var
  MutexUART: TMutex;

procedure EnvoyerUART(Donnees: string);
begin
  MutexLock(MutexUART);    // Verrouille l'accès exclusif
  try
    UART_Transmit(Donnees);
  finally
    MutexUnlock(MutexUART); // Libère l'accès
  end;
end;
```

### Files de messages (Queues)

Communication asynchrone entre tâches :

```pascal
// Conceptuel
type
  TMessageCapteur = record
    TypeCapteur: byte;
    Valeur: integer;
    Timestamp: cardinal;
  end;

var
  FileMessages: TQueue;

// Tâche capteur
procedure TacheCapteur;
var
  Msg: TMessageCapteur;
begin
  while True do
  begin
    Msg.TypeCapteur := 1;
    Msg.Valeur := LireADC();
    Msg.Timestamp := GetTickCount();

    QueueSend(FileMessages, @Msg, portMAX_DELAY);
    vTaskDelay(50);
  end;
end;

// Tâche traitement
procedure TacheTraitement;
var
  Msg: TMessageCapteur;
begin
  while True do
  begin
    if QueueReceive(FileMessages, @Msg, portMAX_DELAY) then
      TraiterMessage(Msg);
  end;
end;
```

## Gestion des interruptions

Les interruptions matérielles sont cruciales en temps réel :

```pascal
// Gestionnaire d'interruption (ISR)
// DOIT être le plus court possible !
procedure ISR_Timer; interrupt;
var
  TacheReveiller: TTaskHandle;
begin
  // Traitement minimal
  CompteurTicks := CompteurTicks + 1;

  // Réveiller une tâche pour traitement différé
  xTaskNotifyFromISR(TacheReveiller, 0, eNoAction, nil);

  // Acquitter l'interruption matérielle
  TIMER_ClearInterruptFlag();
end;

// Tâche qui fait le vrai traitement
procedure TacheTraitementTimer;
begin
  while True do
  begin
    // Attend notification de l'ISR
    ulTaskNotifyTake(pdTRUE, portMAX_DELAY);

    // Traitement long permis ici
    CalculsComplexes();
    MiseAJourAffichage();
  end;
end;
```

**Règle d'or** : dans une ISR, ne faites que le strict minimum et déléguez le reste à une tâche.

## Inversion de priorité

Problème classique en temps réel :

```
Tâche A (priorité haute) attend un mutex détenu par Tâche C (priorité basse)
Tâche B (priorité moyenne) s'exécute et bloque Tâche C
→ Tâche A est bloquée par Tâche B indirectement !
```

**Solution : héritage de priorité**

```pascal
// Le RTOS élève temporairement la priorité de C au niveau de A
// quand A attend le mutex de C
```

## Gestion mémoire en temps réel

### Allocation statique

Préférez l'allocation statique pour le déterminisme :

```pascal
// ✓ BON : allocation statique au démarrage
var
  BufferCapteurs: array[0..255] of integer;

// ✗ MAUVAIS en temps réel strict : allocation dynamique
procedure TacheDynamique;
var
  Buffer: ^integer;
begin
  GetMem(Buffer, 256 * SizeOf(integer)); // Durée imprévisible !
  try
    // ...
  finally
    FreeMem(Buffer);
  end;
end;
```

### Memory pools

Alternative déterministe à l'allocation dynamique :

```pascal
// Pool de mémoire pré-allouée
type
  TMemoryPool = record
    Blocs: array[0..15] of array[0..127] of byte;
    Disponible: array[0..15] of boolean;
  end;

function AllouerBloc(var Pool: TMemoryPool): pointer;
var
  i: integer;
begin
  for i := 0 to 15 do
    if Pool.Disponible[i] then
    begin
      Pool.Disponible[i] := False;
      Exit(@Pool.Blocs[i]);
    end;
  Result := nil; // Pool épuisé
end;
```

## Analyse temporelle

### Worst-Case Execution Time (WCET)

En temps réel strict, vous devez **prouver** que chaque tâche respecte ses échéances :

```pascal
// Analyse WCET
procedure TacheControle; // WCET : 2.5ms maximum
begin
  // Lecture capteurs : 0.8ms
  Temperature := LireCapteurTemperature();  // 0.5ms
  Pression := LireCapteurPression();        // 0.3ms

  // Calcul PID : 1.2ms
  Commande := CalculerPID(Temperature, Consigne); // 1.2ms

  // Envoi actuateur : 0.5ms
  EnvoyerPWM(Commande); // 0.5ms

  // Total WCET = 0.8 + 1.2 + 0.5 = 2.5ms
  // Si période = 10ms, utilisation CPU = 25%
end;
```

### Test de schedulabilité

Pour vérifier qu'un ensemble de tâches est exécutable :

```
Test Rate Monotonic (RM) :
Σ (WCET_i / Période_i) ≤ n × (2^(1/n) - 1)

Exemple avec 3 tâches :
Tâche A : 2ms / 10ms = 0.20
Tâche B : 3ms / 20ms = 0.15
Tâche C : 4ms / 50ms = 0.08
Total : 0.43

Limite pour n=3 : 3 × (2^(1/3) - 1) ≈ 0.78
0.43 < 0.78 ✓ Schedulable !
```

## FreePascal et temps réel : réalité pratique

### Ce qui est possible

**Sur microcontrôleurs (ARM Cortex-M avec FreeRTOS)** :
- FreePascal compile du code natif ARM
- Intégration possible avec FreeRTOS via bindings
- Déterminisme acceptable pour soft real-time
- Utilisable pour robotique, domotique, IoT

```pascal
{$MODE OBJFPC}
program RTOSDemo;

// Bindings FreeRTOS (simplifiés)
procedure vTaskDelay(Ticks: cardinal); external;
function xTaskCreate(Code: pointer; Name: PChar; StackSize: word;
                     Params: pointer; Priority: byte;
                     Handle: pointer): boolean; external;

// Votre code applicatif
procedure LED_Blink(Params: pointer);
begin
  while True do
  begin
    GPIO_Toggle(LED_PIN);
    vTaskDelay(500); // 500 ticks = 500ms si tick = 1ms
  end;
end;

begin
  xTaskCreate(@LED_Blink, 'LED', 128, nil, 1, nil);
  // Démarrer ordonnanceur...
end.
```

### Limitations

**FreePascal n'est pas optimal pour hard real-time** :
- Garbage collection inexistante (bien), mais gestion mémoire parfois imprévisible
- Runtime peut introduire de la latence
- Moins de support natif RTOS que C/C++
- Outils d'analyse temporelle limités

**Recommandation** :
- ✓ Soft real-time : FreePascal est viable
- ✗ Hard real-time critique : préférez C/Ada/Rust

## Exemple complet : contrôleur de température

```pascal
program ControleurTemperature;

{$MODE OBJFPC}

uses
  FreeRTOS; // Bindings hypothétiques

const
  PERIODE_LECTURE = 100;    // 100ms
  PERIODE_AFFICHAGE = 1000; // 1 seconde
  SEUIL_ALERTE = 80;        // 80°C

var
  MutexTemperature: TMutexHandle;
  TemperatureActuelle: real;
  FileAlarmes: TQueueHandle;

// Tâche lecture capteur (haute priorité)
procedure TacheLectureCapteur(Params: pointer);
var
  TempLue: real;
begin
  while True do
  begin
    TempLue := ADC_LireTemperature();

    // Mise à jour thread-safe
    xSemaphoreTake(MutexTemperature, portMAX_DELAY);
    TemperatureActuelle := TempLue;
    xSemaphoreGive(MutexTemperature);

    // Alarme si seuil dépassé
    if TempLue > SEUIL_ALERTE then
      xQueueSend(FileAlarmes, @TempLue, 0);

    vTaskDelay(pdMS_TO_TICKS(PERIODE_LECTURE));
  end;
end;

// Tâche affichage (basse priorité)
procedure TacheAffichage(Params: pointer);
var
  TempLocale: real;
begin
  while True do
  begin
    xSemaphoreTake(MutexTemperature, portMAX_DELAY);
    TempLocale := TemperatureActuelle;
    xSemaphoreGive(MutexTemperature);

    LCD_Afficher('Temp: ', TempLocale:0:1, ' C');

    vTaskDelay(pdMS_TO_TICKS(PERIODE_AFFICHAGE));
  end;
end;

// Tâche gestion alarmes (priorité moyenne)
procedure TacheAlarmes(Params: pointer);
var
  TempAlarme: real;
begin
  while True do
  begin
    if xQueueReceive(FileAlarmes, @TempAlarme, portMAX_DELAY) = pdTRUE then
    begin
      GPIO_Set(PIN_ALARME);
      Buzzer_Activer();
      EnvoyerNotificationUrgente(TempAlarme);
    end;
  end;
end;

// Programme principal
begin
  // Initialisation
  MutexTemperature := xSemaphoreCreateMutex();
  FileAlarmes := xQueueCreate(10, SizeOf(real));

  // Création des tâches avec priorités
  xTaskCreate(@TacheLectureCapteur, 'Capteur', 256, nil, 3, nil);
  xTaskCreate(@TacheAlarmes, 'Alarmes', 256, nil, 2, nil);
  xTaskCreate(@TacheAffichage, 'Affichage', 256, nil, 1, nil);

  // Démarrer l'ordonnanceur RTOS
  vTaskStartScheduler();

  // Ne devrait jamais arriver ici
  while True do ;
end.
```

## Outils et ressources

### Simulation et test

- **QEMU** : émulation ARM pour tester sans matériel
- **Renode** : simulation de systèmes embarqués complets
- **Analyseurs logiques** : visualiser le timing réel

### Documentation

- FreeRTOS.org : documentation officielle complète
- PREEMPT_RT wiki : pour Linux temps réel
- "Hard Real-Time Computing Systems" (Buttazzo) : référence académique

### Communauté FreePascal

- Forum Lazarus/FreePascal : section embedded
- GitHub : exemples FreeRTOS + FreePascal
- Embedded Pascal : projets spécialisés

## Conclusion

Le développement temps réel avec FreePascal est possible et viable pour de nombreuses applications, particulièrement en **soft real-time**. Pour du **hard real-time** critique (aéronautique, médical), d'autres langages comme C ou Ada sont plus appropriés et certifiables.

Les concepts restent identiques quel que soit le langage :
- **Déterminisme** avant performance
- **Priorités** bien définies
- **Synchronisation** rigoureuse
- **Analyse temporelle** systématique

Le temps réel est une discipline exigeante mais passionnante, au cœur de nombreux systèmes embarqués modernes !

⏭️ [Drivers et accès matériel direct](/14-systemes-embarques-iot/09-drivers-acces-materiel-direct.md)
