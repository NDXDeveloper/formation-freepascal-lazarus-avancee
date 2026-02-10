🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.8 Saga Pattern et Transactions Distribuées

## Introduction

Le **Saga Pattern** est une solution pour gérer les transactions distribuées dans les architectures microservices où les transactions ACID traditionnelles ne fonctionnent pas.

### Le problème des transactions distribuées

Dans un monolithe, les transactions sont simples :

```pascal
// ❌ Dans un monolithe - FACILE
Database.StartTransaction;  
try
  ReserverStock(ProduitId, Quantite);
  DebiterCompte(ClientId, Montant);
  CreerCommande(ClientId, ProduitId);

  Database.Commit;
except
  Database.Rollback;  // ← Annulation automatique
  raise;
end;
```

Dans une architecture distribuée, c'est **impossible** :

```pascal
// ❌ Dans des microservices - IMPOSSIBLE
// Chaque service a sa propre base de données
ServiceStock.ReserverStock(ProduitId, Quantite);     // ← Service 1  
ServicePaiement.DebiterCompte(ClientId, Montant);    // ← Service 2  
ServiceCommande.CreerCommande(ClientId, ProduitId);  // ← Service 3

// Que faire si le service 3 échoue ?
// On ne peut pas faire de rollback global !
```

### Qu'est-ce qu'une Saga ?

Une **Saga** est une séquence de transactions locales où chaque transaction a une **compensation** pour annuler ses effets.

```
SAGA RÉUSSIE :
Étape 1: Réserver stock          ✓
Étape 2: Débiter compte          ✓
Étape 3: Créer commande          ✓
= Succès total

SAGA ÉCHOUÉE :
Étape 1: Réserver stock          ✓
Étape 2: Débiter compte          ✓
Étape 3: Créer commande          ✗  ← Échec !
Compensation 2: Rembourser       ✓  ← Annulation  
Compensation 1: Libérer stock    ✓  ← Annulation
= Retour à l'état initial
```

**Analogie :** Comme un voyage avec plusieurs vols. Si un vol est annulé, vous devez annuler tous les vols précédents dans l'ordre inverse.

### Deux types de Sagas

**1. Choreography (Chorégraphie)**
Les services réagissent aux événements sans coordinateur central.

```
Service A ──► Événement ──► Service B ──► Événement ──► Service C
```

**2. Orchestration**
Un orchestrateur central coordonne toutes les étapes.

```
                Orchestrateur
                     │
        ┌────────────┼────────────┐
        ▼            ▼            ▼
    Service A    Service B    Service C
```

## Saga par Orchestration

### 1. Structure de base

```pascal
unit Saga.Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, fpjson;

type
  // Résultat d'une étape
  TSagaStepResult = (ssSuccess, ssFailure, ssCompensated);

// Helper : NewGUID est du Delphi,
// en FPC on utilise CreateGUID + GUIDToString
function NewGUID: string;

  // Contexte partagé entre les étapes
  TSagaContext = class
  private
    FData: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetValue(const AKey: string; const AValue: Variant);
    function GetValue(const AKey: string): Variant;

    property Data: TJSONObject read FData;
  end;

  // Étape de saga
  TSagaStep = class
  private
    FName: string;
    FExecuted: Boolean;
  public
    constructor Create(const AName: string);

    // Action principale
    function Execute(AContext: TSagaContext): TSagaStepResult; virtual; abstract;

    // Action de compensation (annulation)
    function Compensate(AContext: TSagaContext): Boolean; virtual; abstract;

    property Name: string read FName;
    property Executed: Boolean read FExecuted write FExecuted;
  end;

  // Orchestrateur de saga
  TSagaOrchestrator = class
  private
    FSteps: TObjectList<TSagaStep>;
  protected
    FContext: TSagaContext;
    FExecutedSteps: TList<TSagaStep>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddStep(AStep: TSagaStep);
    function Execute: Boolean;

    property Context: TSagaContext read FContext;
  end;

implementation

uses
  Variants;

function NewGUID: string;  
var
  G: TGUID;
begin
  CreateGUID(G);
  Result := GUIDToString(G);
end;

// TSagaContext

constructor TSagaContext.Create;  
begin
  inherited Create;
  FData := TJSONObject.Create;
end;

destructor TSagaContext.Destroy;  
begin
  FData.Free;
  inherited;
end;

procedure TSagaContext.SetValue(const AKey: string; const AValue: Variant);  
begin
  if VarIsStr(AValue) then
    FData.Add(AKey, VarToStr(AValue))
  else if VarIsNumeric(AValue) then
    FData.Add(AKey, VarAsType(AValue, varDouble))
  else if VarIsType(AValue, varBoolean) then
    FData.Add(AKey, Boolean(AValue));
end;

function TSagaContext.GetValue(const AKey: string): Variant;  
begin
  if FData.Find(AKey) <> nil then
    Result := FData.Get(AKey, '')
  else
    Result := Null;
end;

// TSagaStep

constructor TSagaStep.Create(const AName: string);  
begin
  inherited Create;
  FName := AName;
  FExecuted := False;
end;

// TSagaOrchestrator

constructor TSagaOrchestrator.Create;  
begin
  inherited Create;
  FSteps := TObjectList<TSagaStep>.Create(True);
  FContext := TSagaContext.Create;
  FExecutedSteps := TList<TSagaStep>.Create;
end;

destructor TSagaOrchestrator.Destroy;  
begin
  FExecutedSteps.Free;
  FContext.Free;
  FSteps.Free;
  inherited;
end;

procedure TSagaOrchestrator.AddStep(AStep: TSagaStep);  
begin
  FSteps.Add(AStep);
  WriteLn(Format('[Saga] Étape ajoutée: %s', [AStep.Name]));
end;

function TSagaOrchestrator.Execute: Boolean;  
var
  Step: TSagaStep;
  StepResult: TSagaStepResult;
  i: Integer;
begin
  Result := True;
  FExecutedSteps.Clear;

  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Exécution de la Saga                    ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  // Exécuter toutes les étapes
  for Step in FSteps do
  begin
    WriteLn(Format('[Saga] Exécution: %s', [Step.Name]));

    try
      StepResult := Step.Execute(FContext);

      if StepResult = ssSuccess then
      begin
        Step.Executed := True;
        FExecutedSteps.Add(Step);
        WriteLn(Format('[Saga] ✓ %s réussie', [Step.Name]));
        WriteLn;
      end
      else
      begin
        WriteLn(Format('[Saga] ✗ %s échouée', [Step.Name]));
        WriteLn;
        WriteLn('[Saga] Démarrage des compensations...');
        WriteLn;

        // Compenser dans l'ordre inverse
        for i := FExecutedSteps.Count - 1 downto 0 do
        begin
          WriteLn(Format('[Saga] Compensation: %s', [FExecutedSteps[i].Name]));

          if FExecutedSteps[i].Compensate(FContext) then
            WriteLn(Format('[Saga] ✓ Compensation %s réussie', [FExecutedSteps[i].Name]))
          else
            WriteLn(Format('[Saga] ✗ Compensation %s échouée', [FExecutedSteps[i].Name]));

          WriteLn;
        end;

        Result := False;
        Break;
      end;

    except
      on E: Exception do
      begin
        WriteLn(Format('[Saga] ERREUR dans %s: %s', [Step.Name, E.Message]));
        Result := False;
        Break;
      end;
    end;
  end;

  WriteLn('═══════════════════════════════════════════');
  if Result then
    WriteLn('  Saga complétée avec succès              ')
  else
    WriteLn('  Saga annulée - État initial restauré    ');
  WriteLn('═══════════════════════════════════════════');
end;

end.
```

### 2. Exemple concret : E-commerce

```pascal
unit Saga.ECommerce;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Saga.Core;

type
  // Étape 1 : Réserver le stock
  TSagaReserverStock = class(TSagaStep)
  private
    FProduitId: string;
    FQuantite: Integer;
    FReservationId: string;
  public
    constructor Create(const AProduitId: string; AQuantite: Integer);

    function Execute(AContext: TSagaContext): TSagaStepResult; override;
    function Compensate(AContext: TSagaContext): Boolean; override;
  end;

  // Étape 2 : Débiter le compte
  TSagaDebiterCompte = class(TSagaStep)
  private
    FClientId: string;
    FMontant: Currency;
    FTransactionId: string;
  public
    constructor Create(const AClientId: string; AMontant: Currency);

    function Execute(AContext: TSagaContext): TSagaStepResult; override;
    function Compensate(AContext: TSagaContext): Boolean; override;
  end;

  // Étape 3 : Créer la commande
  TSagaCreerCommande = class(TSagaStep)
  private
    FClientId: string;
    FProduitId: string;
    FCommandeId: string;
  public
    constructor Create(const AClientId, AProduitId: string);

    function Execute(AContext: TSagaContext): TSagaStepResult; override;
    function Compensate(AContext: TSagaContext): Boolean; override;
  end;

  // Étape 4 : Envoyer notification
  TSagaEnvoyerNotification = class(TSagaStep)
  private
    FClientId: string;
  public
    constructor Create(const AClientId: string);

    function Execute(AContext: TSagaContext): TSagaStepResult; override;
    function Compensate(AContext: TSagaContext): Boolean; override;
  end;

implementation

// TSagaReserverStock

constructor TSagaReserverStock.Create(const AProduitId: string; AQuantite: Integer);  
begin
  inherited Create('Réserver Stock');
  FProduitId := AProduitId;
  FQuantite := AQuantite;
end;

function TSagaReserverStock.Execute(AContext: TSagaContext): TSagaStepResult;  
begin
  WriteLn(Format('  → Réservation de %d unités du produit %s',
    [FQuantite, FProduitId]));

  // Simuler l'appel au service de stock
  Sleep(100);

  // Vérifier le stock disponible (simulation)
  if Random(100) < 90 then  // 90% de succès
  begin
    FReservationId := NewGUID;
    AContext.SetValue('reservation_id', FReservationId);
    AContext.SetValue('produit_id', FProduitId);
    AContext.SetValue('quantite', FQuantite);

    WriteLn(Format('  → Réservation créée: %s', [FReservationId]));
    Result := ssSuccess;
  end
  else
  begin
    WriteLn('  → Stock insuffisant !');
    Result := ssFailure;
  end;
end;

function TSagaReserverStock.Compensate(AContext: TSagaContext): Boolean;  
var
  ReservationId: string;
begin
  ReservationId := AContext.GetValue('reservation_id');

  WriteLn(Format('  → Annulation réservation: %s', [ReservationId]));

  // Simuler l'appel au service de stock pour libérer
  Sleep(50);

  WriteLn('  → Stock libéré');
  Result := True;
end;

// TSagaDebiterCompte

constructor TSagaDebiterCompte.Create(const AClientId: string; AMontant: Currency);  
begin
  inherited Create('Débiter Compte');
  FClientId := AClientId;
  FMontant := AMontant;
end;

function TSagaDebiterCompte.Execute(AContext: TSagaContext): TSagaStepResult;  
begin
  WriteLn(Format('  → Débit de %.2f € sur le compte %s', [FMontant, FClientId]));

  // Simuler l'appel au service de paiement
  Sleep(150);

  // Vérifier le solde (simulation)
  if Random(100) < 85 then  // 85% de succès
  begin
    FTransactionId := NewGUID;
    AContext.SetValue('transaction_id', FTransactionId);
    AContext.SetValue('montant', FMontant);

    WriteLn(Format('  → Transaction effectuée: %s', [FTransactionId]));
    Result := ssSuccess;
  end
  else
  begin
    WriteLn('  → Solde insuffisant !');
    Result := ssFailure;
  end;
end;

function TSagaDebiterCompte.Compensate(AContext: TSagaContext): Boolean;  
var
  TransactionId: string;
  Montant: Currency;
begin
  TransactionId := AContext.GetValue('transaction_id');
  Montant := AContext.GetValue('montant');

  WriteLn(Format('  → Remboursement de %.2f € (Transaction: %s)',
    [Montant, TransactionId]));

  // Simuler l'appel au service de paiement pour rembourser
  Sleep(100);

  WriteLn('  → Remboursement effectué');
  Result := True;
end;

// TSagaCreerCommande

constructor TSagaCreerCommande.Create(const AClientId, AProduitId: string);  
begin
  inherited Create('Créer Commande');
  FClientId := AClientId;
  FProduitId := AProduitId;
end;

function TSagaCreerCommande.Execute(AContext: TSagaContext): TSagaStepResult;  
begin
  WriteLn(Format('  → Création commande pour client %s', [FClientId]));

  // Simuler l'appel au service de commande
  Sleep(100);

  // Créer la commande (simulation)
  if Random(100) < 95 then  // 95% de succès
  begin
    FCommandeId := NewGUID;
    AContext.SetValue('commande_id', FCommandeId);

    WriteLn(Format('  → Commande créée: %s', [FCommandeId]));
    Result := ssSuccess;
  end
  else
  begin
    WriteLn('  → Erreur lors de la création de la commande !');
    Result := ssFailure;
  end;
end;

function TSagaCreerCommande.Compensate(AContext: TSagaContext): Boolean;  
var
  CommandeId: string;
begin
  CommandeId := AContext.GetValue('commande_id');

  WriteLn(Format('  → Annulation commande: %s', [CommandeId]));

  // Simuler l'appel au service de commande pour annuler
  Sleep(50);

  WriteLn('  → Commande annulée');
  Result := True;
end;

// TSagaEnvoyerNotification

constructor TSagaEnvoyerNotification.Create(const AClientId: string);  
begin
  inherited Create('Envoyer Notification');
  FClientId := AClientId;
end;

function TSagaEnvoyerNotification.Execute(AContext: TSagaContext): TSagaStepResult;  
var
  CommandeId: string;
begin
  CommandeId := AContext.GetValue('commande_id');

  WriteLn(Format('  → Envoi notification au client %s', [FClientId]));
  WriteLn(Format('  → Commande: %s', [CommandeId]));

  // Simuler l'envoi d'email
  Sleep(50);

  WriteLn('  → Email envoyé');
  Result := ssSuccess;
end;

function TSagaEnvoyerNotification.Compensate(AContext: TSagaContext): Boolean;  
begin
  WriteLn('  → Aucune compensation nécessaire pour la notification');
  Result := True;
end;

end.
```

### 3. Programme de démonstration

```pascal
program SagaDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Saga.Core, Saga.ECommerce;

procedure TestSagaReussie;  
var
  Saga: TSagaOrchestrator;
begin
  WriteLn;
  WriteLn('╔═══════════════════════════════════════════╗');
  WriteLn('║   TEST 1: Saga complète (succès)          ║');
  WriteLn('╚═══════════════════════════════════════════╝');
  WriteLn;

  Saga := TSagaOrchestrator.Create;
  try
    // Définir les étapes
    Saga.AddStep(TSagaReserverStock.Create('PROD-123', 2));
    Saga.AddStep(TSagaDebiterCompte.Create('CLIENT-456', 99.99));
    Saga.AddStep(TSagaCreerCommande.Create('CLIENT-456', 'PROD-123'));
    Saga.AddStep(TSagaEnvoyerNotification.Create('CLIENT-456'));

    // Exécuter
    Saga.Execute;
  finally
    Saga.Free;
  end;
end;

procedure TestSagaEchecEtCompensation;  
var
  Saga: TSagaOrchestrator;
begin
  WriteLn;
  WriteLn('╔═══════════════════════════════════════════╗');
  WriteLn('║   TEST 2: Saga avec échec et compensation ║');
  WriteLn('╚═══════════════════════════════════════════╝');
  WriteLn;

  // Forcer un échec en utilisant une faible probabilité de succès
  Randomize;

  Saga := TSagaOrchestrator.Create;
  try
    Saga.AddStep(TSagaReserverStock.Create('PROD-789', 1));
    Saga.AddStep(TSagaDebiterCompte.Create('CLIENT-999', 49.99));
    Saga.AddStep(TSagaCreerCommande.Create('CLIENT-999', 'PROD-789'));
    Saga.AddStep(TSagaEnvoyerNotification.Create('CLIENT-999'));

    // Exécuter (peut échouer de manière aléatoire)
    Saga.Execute;
  finally
    Saga.Free;
  end;
end;

begin
  Randomize;

  // Test 1: Saga réussie
  TestSagaReussie;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour le test suivant...');
  ReadLn;

  // Test 2: Saga avec échec
  TestSagaEchecEtCompensation;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Saga par Chorégraphie

Au lieu d'un orchestrateur central, les services réagissent aux événements.

### 1. Architecture basée sur les événements

```pascal
unit Saga.Choreography;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, fpjson,
  EventBus.Events, EventBus.Bus;

type
  // Événements de la saga
  TCommandeInitieeEvent = class(TEvent)
  public
    constructor Create(const ACommandeId, AClientId, AProduitId: string;
                      AQuantite: Integer; AMontant: Currency);
  end;

  TStockReserveEvent = class(TEvent)
  public
    constructor Create(const ACommandeId, AReservationId: string);
  end;

  TStockReservationEchoueeEvent = class(TEvent)
  public
    constructor Create(const ACommandeId, ARaison: string);
  end;

  TPaiementEffectueEvent = class(TEvent)
  public
    constructor Create(const ACommandeId, ATransactionId: string);
  end;

  TPaiementEchoueEvent = class(TEvent)
  public
    constructor Create(const ACommandeId, ARaison: string);
  end;

  TCommandeCreeeEvent = class(TEvent)
  public
    constructor Create(const ACommandeId: string);
  end;

  TCommandeAnnuleeEvent = class(TEvent)
  public
    constructor Create(const ACommandeId, ARaison: string);
  end;

implementation

// TCommandeInitieeEvent

constructor TCommandeInitieeEvent.Create(const ACommandeId, AClientId, AProduitId: string;
  AQuantite: Integer; AMontant: Currency);
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('commande_id', ACommandeId);
  Data.Add('client_id', AClientId);
  Data.Add('produit_id', AProduitId);
  Data.Add('quantite', AQuantite);
  Data.Add('montant', AMontant);

  inherited Create('commande.initiee', 'SagaStarter', Data);
end;

// TStockReserveEvent

constructor TStockReserveEvent.Create(const ACommandeId, AReservationId: string);  
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('commande_id', ACommandeId);
  Data.Add('reservation_id', AReservationId);

  inherited Create('stock.reserve', 'ServiceStock', Data);
end;

// TStockReservationEchoueeEvent

constructor TStockReservationEchoueeEvent.Create(const ACommandeId, ARaison: string);  
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('commande_id', ACommandeId);
  Data.Add('raison', ARaison);

  inherited Create('stock.reservation_echouee', 'ServiceStock', Data);
end;

// TPaiementEffectueEvent

constructor TPaiementEffectueEvent.Create(const ACommandeId, ATransactionId: string);  
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('commande_id', ACommandeId);
  Data.Add('transaction_id', ATransactionId);

  inherited Create('paiement.effectue', 'ServicePaiement', Data);
end;

// TPaiementEchoueEvent

constructor TPaiementEchoueEvent.Create(const ACommandeId, ARaison: string);  
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('commande_id', ACommandeId);
  Data.Add('raison', ARaison);

  inherited Create('paiement.echoue', 'ServicePaiement', Data);
end;

// TCommandeCreeeEvent

constructor TCommandeCreeeEvent.Create(const ACommandeId: string);  
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('commande_id', ACommandeId);

  inherited Create('commande.creee', 'ServiceCommande', Data);
end;

// TCommandeAnnuleeEvent

constructor TCommandeAnnuleeEvent.Create(const ACommandeId, ARaison: string);  
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('commande_id', ACommandeId);
  Data.Add('raison', ARaison);

  inherited Create('commande.annulee', 'ServiceCommande', Data);
end;

end.
```

### 2. Services réactifs

```pascal
unit Saga.Services;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  EventBus.Events, EventBus.Bus, Saga.Choreography;

type
  // Service de stock
  TServiceStock = class
  private
    FEventBus: TEventBus;

    procedure OnCommandeInitiee(AEvent: TEvent);
    procedure OnCommandeAnnulee(AEvent: TEvent);
  public
    constructor Create(AEventBus: TEventBus);

    procedure Subscribe;
  end;

  // Service de paiement
  TServicePaiement = class
  private
    FEventBus: TEventBus;

    procedure OnStockReserve(AEvent: TEvent);
    procedure OnCommandeAnnulee(AEvent: TEvent);
  public
    constructor Create(AEventBus: TEventBus);

    procedure Subscribe;
  end;

  // Service de commande
  TServiceCommande = class
  private
    FEventBus: TEventBus;

    procedure OnPaiementEffectue(AEvent: TEvent);
    procedure OnPaiementEchoue(AEvent: TEvent);
    procedure OnStockReservationEchouee(AEvent: TEvent);
  public
    constructor Create(AEventBus: TEventBus);

    procedure Subscribe;
  end;

implementation

// TServiceStock

constructor TServiceStock.Create(AEventBus: TEventBus);  
begin
  inherited Create;
  FEventBus := AEventBus;
end;

procedure TServiceStock.Subscribe;  
begin
  FEventBus.Subscribe('ServiceStock', 'commande.initiee', @OnCommandeInitiee);
  FEventBus.Subscribe('ServiceStock', 'commande.annulee', @OnCommandeAnnulee);
end;

procedure TServiceStock.OnCommandeInitiee(AEvent: TEvent);  
var
  CommandeId, ProduitId: string;
  Quantite: Integer;
  ReservationId: string;
begin
  CommandeId := AEvent.Data.Get('commande_id', '');
  ProduitId := AEvent.Data.Get('produit_id', '');
  Quantite := AEvent.Data.Get('quantite', 0);

  WriteLn(Format('[ServiceStock] Réservation de %d x %s', [Quantite, ProduitId]));

  // Simuler réservation
  Sleep(100);

  if Random(100) < 90 then
  begin
    ReservationId := NewGUID;
    WriteLn(Format('[ServiceStock] ✓ Réservation: %s', [ReservationId]));

    FEventBus.Publish(TStockReserveEvent.Create(CommandeId, ReservationId));
  end
  else
  begin
    WriteLn('[ServiceStock] ✗ Stock insuffisant');
    FEventBus.Publish(TStockReservationEchoueeEvent.Create(CommandeId, 'Stock insuffisant'));
  end;
end;

procedure TServiceStock.OnCommandeAnnulee(AEvent: TEvent);  
var
  CommandeId: string;
begin
  CommandeId := AEvent.Data.Get('commande_id', '');
  WriteLn(Format('[ServiceStock] Libération du stock pour: %s', [CommandeId]));
end;

// TServicePaiement

constructor TServicePaiement.Create(AEventBus: TEventBus);  
begin
  inherited Create;
  FEventBus := AEventBus;
end;

procedure TServicePaiement.Subscribe;  
begin
  FEventBus.Subscribe('ServicePaiement', 'stock.reserve', @OnStockReserve);
  FEventBus.Subscribe('ServicePaiement', 'commande.annulee', @OnCommandeAnnulee);
end;

procedure TServicePaiement.OnStockReserve(AEvent: TEvent);  
var
  CommandeId: string;
  TransactionId: string;
begin
  CommandeId := AEvent.Data.Get('commande_id', '');

  WriteLn(Format('[ServicePaiement] Traitement paiement pour: %s', [CommandeId]));

  // Simuler paiement
  Sleep(150);

  if Random(100) < 85 then
  begin
    TransactionId := NewGUID;
    WriteLn(Format('[ServicePaiement] ✓ Transaction: %s', [TransactionId]));

    FEventBus.Publish(TPaiementEffectueEvent.Create(CommandeId, TransactionId));
  end
  else
  begin
    WriteLn('[ServicePaiement] ✗ Paiement refusé');
    FEventBus.Publish(TPaiementEchoueEvent.Create(CommandeId, 'Carte refusée'));
  end;
end;

procedure TServicePaiement.OnCommandeAnnulee(AEvent: TEvent);  
var
  CommandeId: string;
begin
  CommandeId := AEvent.Data.Get('commande_id', '');
  WriteLn(Format('[ServicePaiement] Remboursement pour: %s', [CommandeId]));
end;

// TServiceCommande

constructor TServiceCommande.Create(AEventBus: TEventBus);  
begin
  inherited Create;
  FEventBus := AEventBus;
end;

procedure TServiceCommande.Subscribe;  
begin
  FEventBus.Subscribe('ServiceCommande', 'paiement.effectue', @OnPaiementEffectue);
  FEventBus.Subscribe('ServiceCommande', 'paiement.echoue', @OnPaiementEchoue);
  FEventBus.Subscribe('ServiceCommande', 'stock.reservation_echouee', @OnStockReservationEchouee);
end;

procedure TServiceCommande.OnPaiementEffectue(AEvent: TEvent);  
var
  CommandeId: string;
begin
  CommandeId := AEvent.Data.Get('commande_id', '');

  WriteLn(Format('[ServiceCommande] Création de la commande: %s', [CommandeId]));

  // Simuler création commande
  Sleep(100);

  if Random(100) < 95 then
  begin
    WriteLn(Format('[ServiceCommande] ✓ Commande créée: %s', [CommandeId]));
    FEventBus.Publish(TCommandeCreeeEvent.Create(CommandeId));
  end
  else
  begin
    WriteLn('[ServiceCommande] ✗ Erreur création commande');
    FEventBus.Publish(TCommandeAnnuleeEvent.Create(CommandeId, 'Erreur système'));
  end;
end;

procedure TServiceCommande.OnPaiementEchoue(AEvent: TEvent);  
var
  CommandeId, Raison: string;
begin
  CommandeId := AEvent.Data.Get('commande_id', '');
  Raison := AEvent.Data.Get('raison', '');

  WriteLn(Format('[ServiceCommande] Annulation commande: %s (Raison: %s)',
    [CommandeId, Raison]));

  FEventBus.Publish(TCommandeAnnuleeEvent.Create(CommandeId, Raison));
end;

procedure TServiceCommande.OnStockReservationEchouee(AEvent: TEvent);  
var
  CommandeId, Raison: string;
begin
  CommandeId := AEvent.Data.Get('commande_id', '');
  Raison := AEvent.Data.Get('raison', '');

  WriteLn(Format('[ServiceCommande] Annulation immédiate: %s (Raison: %s)',
    [CommandeId, Raison]));

  FEventBus.Publish(TCommandeAnnuleeEvent.Create(CommandeId, Raison));
end;

end.
```

### 3. Event Bus pour la chorégraphie

```pascal
unit EventBus.Events;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  // Événement de base
  TEvent = class
  private
    FEventType: string;
    FSource: string;
    FTimestamp: TDateTime;
    FData: TJSONObject;
    FEventId: string;
  public
    constructor Create(const AEventType, ASource: string; AData: TJSONObject);
    destructor Destroy; override;

    property EventId: string read FEventId;
    property EventType: string read FEventType;
    property Source: string read FSource;
    property Timestamp: TDateTime read FTimestamp;
    property Data: TJSONObject read FData;
  end;

  // Handler d'événement
  TEventHandler = procedure(AEvent: TEvent) of object;

implementation

// TEvent

constructor TEvent.Create(const AEventType, ASource: string; AData: TJSONObject);  
begin
  inherited Create;
  FEventId := NewGUID;
  FEventType := AEventType;
  FSource := ASource;
  FTimestamp := Now;
  FData := AData;
end;

destructor TEvent.Destroy;  
begin
  FData.Free;
  inherited;
end;

end.
```

```pascal
unit EventBus.Bus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  EventBus.Events;

type
  // Souscription à un événement
  TEventSubscription = record
    SubscriberId: string;
    EventType: string;
    Handler: TEventHandler;
  end;

  // Bus d'événements
  TEventBus = class
  private
    FSubscriptions: TList<TEventSubscription>;
    FEventHistory: TObjectList<TEvent>;
    FMaxHistorySize: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Subscribe(const ASubscriberId, AEventType: string;
      AHandler: TEventHandler);
    procedure Unsubscribe(const ASubscriberId, AEventType: string);
    procedure Publish(AEvent: TEvent);

    function GetHistory(const AEventType: string = ''): TList<TEvent>;

    property MaxHistorySize: Integer read FMaxHistorySize write FMaxHistorySize;
  end;

implementation

// TEventBus

constructor TEventBus.Create;  
begin
  inherited Create;
  FSubscriptions := TList<TEventSubscription>.Create;
  FEventHistory := TObjectList<TEvent>.Create(True);
  FMaxHistorySize := 1000;
end;

destructor TEventBus.Destroy;  
begin
  FEventHistory.Free;
  FSubscriptions.Free;
  inherited;
end;

procedure TEventBus.Subscribe(const ASubscriberId, AEventType: string;
  AHandler: TEventHandler);
var
  Subscription: TEventSubscription;
begin
  Subscription.SubscriberId := ASubscriberId;
  Subscription.EventType := AEventType;
  Subscription.Handler := AHandler;

  FSubscriptions.Add(Subscription);

  WriteLn(Format('[EventBus] %s souscrit à %s', [ASubscriberId, AEventType]));
end;

procedure TEventBus.Unsubscribe(const ASubscriberId, AEventType: string);  
var
  i: Integer;
begin
  for i := FSubscriptions.Count - 1 downto 0 do
  begin
    if (FSubscriptions[i].SubscriberId = ASubscriberId) and
       (FSubscriptions[i].EventType = AEventType) then
    begin
      FSubscriptions.Delete(i);
      WriteLn(Format('[EventBus] %s désinscrit de %s', [ASubscriberId, AEventType]));
    end;
  end;
end;

procedure TEventBus.Publish(AEvent: TEvent);  
var
  Subscription: TEventSubscription;
begin
  WriteLn(Format('[EventBus] Publication: %s (ID: %s)',
    [AEvent.EventType, AEvent.EventId]));

  // Ajouter à l'historique
  FEventHistory.Add(AEvent);

  // Limiter la taille de l'historique
  while FEventHistory.Count > FMaxHistorySize do
    FEventHistory.Delete(0);

  // Notifier tous les souscripteurs
  for Subscription in FSubscriptions do
  begin
    if Subscription.EventType = AEvent.EventType then
    begin
      WriteLn(Format('[EventBus] → Notification: %s', [Subscription.SubscriberId]));

      try
        Subscription.Handler(AEvent);
      except
        on E: Exception do
          WriteLn(Format('[EventBus] ERREUR dans %s: %s',
            [Subscription.SubscriberId, E.Message]));
      end;
    end;
  end;

  WriteLn;
end;

function TEventBus.GetHistory(const AEventType: string = ''): TList<TEvent>;  
var
  Event: TEvent;
begin
  Result := TList<TEvent>.Create;

  for Event in FEventHistory do
  begin
    if (AEventType = '') or (Event.EventType = AEventType) then
      Result.Add(Event);
  end;
end;

end.
```

### 4. Programme de démonstration chorégraphie

```pascal
program ChoreographyDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  EventBus.Events, EventBus.Bus,
  Saga.Choreography, Saga.Services;

procedure TestChoreographieReussie;  
var
  EventBus: TEventBus;
  ServiceStock: TServiceStock;
  ServicePaiement: TServicePaiement;
  ServiceCommande: TServiceCommande;
  CommandeId: string;
begin
  WriteLn;
  WriteLn('╔═══════════════════════════════════════════╗');
  WriteLn('║   TEST 1: Chorégraphie réussie            ║');
  WriteLn('╚═══════════════════════════════════════════╝');
  WriteLn;

  EventBus := TEventBus.Create;
  try
    // Créer les services
    ServiceStock := TServiceStock.Create(EventBus);
    ServicePaiement := TServicePaiement.Create(EventBus);
    ServiceCommande := TServiceCommande.Create(EventBus);

    try
      // S'abonner aux événements
      ServiceStock.Subscribe;
      ServicePaiement.Subscribe;
      ServiceCommande.Subscribe;

      WriteLn('[Demo] Services initialisés et abonnés');
      WriteLn;
      WriteLn('═══════════════════════════════════════════');
      WriteLn('  Démarrage de la saga par chorégraphie   ');
      WriteLn('═══════════════════════════════════════════');
      WriteLn;

      // Déclencher la saga
      CommandeId := NewGUID;
      EventBus.Publish(TCommandeInitieeEvent.Create(
        CommandeId, 'CLIENT-123', 'PROD-456', 2, 99.99));

      WriteLn('═══════════════════════════════════════════');
      WriteLn('  Fin de la saga                          ');
      WriteLn('═══════════════════════════════════════════');

    finally
      ServiceCommande.Free;
      ServicePaiement.Free;
      ServiceStock.Free;
    end;

  finally
    EventBus.Free;
  end;
end;

procedure TestChoreographieAvecEchec;  
var
  EventBus: TEventBus;
  ServiceStock: TServiceStock;
  ServicePaiement: TServicePaiement;
  ServiceCommande: TServiceCommande;
  CommandeId: string;
begin
  WriteLn;
  WriteLn('╔═══════════════════════════════════════════╗');
  WriteLn('║   TEST 2: Chorégraphie avec échec         ║');
  WriteLn('╚═══════════════════════════════════════════╝');
  WriteLn;

  EventBus := TEventBus.Create;
  try
    ServiceStock := TServiceStock.Create(EventBus);
    ServicePaiement := TServicePaiement.Create(EventBus);
    ServiceCommande := TServiceCommande.Create(EventBus);

    try
      ServiceStock.Subscribe;
      ServicePaiement.Subscribe;
      ServiceCommande.Subscribe;

      WriteLn('[Demo] Services initialisés');
      WriteLn;
      WriteLn('═══════════════════════════════════════════');
      WriteLn('  Démarrage saga (échec attendu)          ');
      WriteLn('═══════════════════════════════════════════');
      WriteLn;

      CommandeId := NewGUID;
      EventBus.Publish(TCommandeInitieeEvent.Create(
        CommandeId, 'CLIENT-789', 'PROD-999', 5, 249.99));

      WriteLn('═══════════════════════════════════════════');
      WriteLn('  Fin de la saga avec compensation        ');
      WriteLn('═══════════════════════════════════════════');

    finally
      ServiceCommande.Free;
      ServicePaiement.Free;
      ServiceStock.Free;
    end;

  finally
    EventBus.Free;
  end;
end;

begin
  Randomize;

  TestChoreographieReussie;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour le test suivant...');
  ReadLn;

  TestChoreographieAvecEchec;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Comparaison Orchestration vs Chorégraphie

### Tableau comparatif

| Critère | Orchestration | Chorégraphie |
|---------|---------------|--------------|
| **Coordination** | Centralisée (orchestrateur) | Décentralisée (événements) |
| **Complexité** | Simple à comprendre | Plus difficile à suivre |
| **Couplage** | Fort (dépendance orchestrateur) | Faible (événements) |
| **Évolution** | Facile (modifier orchestrateur) | Complexe (modifier flux) |
| **Performance** | Latence ajoutée | Rapide (direct) |
| **Debugging** | Facile (point central) | Difficile (distribué) |
| **Scalabilité** | Limitée (orchestrateur) | Excellente |
| **Résilience** | SPOF (orchestrateur) | Très résiliente |

### Quand utiliser l'orchestration ?

✅ **Utilisez l'orchestration quand :**

- La saga est **complexe** avec de nombreuses étapes conditionnelles
- Vous avez besoin d'une **vue d'ensemble** claire du processus
- Le **debugging** et le **monitoring** sont prioritaires
- L'équipe est **petite** et centralisée
- Les **règles métier** changent fréquemment

**Exemple :** Processus de commande e-commerce avec multiples vérifications et conditions.

### Quand utiliser la chorégraphie ?

✅ **Utilisez la chorégraphie quand :**

- Les services sont **autonomes** et indépendants
- La **scalabilité** est critique
- Vous voulez un **faible couplage**
- L'équipe est **distribuée** avec ownership par service
- Les événements sont **naturels** dans votre domaine

**Exemple :** Système de notification multi-canal, workflows de publication.

## Patterns avancés de Saga

### 1. Saga avec timeout

```pascal
unit Saga.Timeout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Saga.Core;

type
  // Étape avec timeout
  TSagaStepWithTimeout = class(TSagaStep)
  private
    FTimeoutMs: Integer;
    FStartTime: TDateTime;
  protected
    function IsTimeout: Boolean;
  public
    constructor Create(const AName: string; ATimeoutMs: Integer);

    function Execute(AContext: TSagaContext): TSagaStepResult; override;
  end;

  // Exemple : Paiement avec timeout
  TSagaPaiementAvecTimeout = class(TSagaStepWithTimeout)
  private
    FClientId: string;
    FMontant: Currency;
  public
    constructor Create(const AClientId: string; AMontant: Currency;
      ATimeoutMs: Integer = 5000);

    function Execute(AContext: TSagaContext): TSagaStepResult; override;
    function Compensate(AContext: TSagaContext): Boolean; override;
  end;

implementation

uses
  DateUtils;

// TSagaStepWithTimeout

constructor TSagaStepWithTimeout.Create(const AName: string; ATimeoutMs: Integer);  
begin
  inherited Create(AName);
  FTimeoutMs := ATimeoutMs;
end;

function TSagaStepWithTimeout.IsTimeout: Boolean;  
var
  ElapsedMs: Int64;
begin
  ElapsedMs := MilliSecondsBetween(Now, FStartTime);
  Result := ElapsedMs > FTimeoutMs;
end;

function TSagaStepWithTimeout.Execute(AContext: TSagaContext): TSagaStepResult;  
begin
  FStartTime := Now;
  Result := ssSuccess; // Override dans les classes dérivées
end;

// TSagaPaiementAvecTimeout

constructor TSagaPaiementAvecTimeout.Create(const AClientId: string;
  AMontant: Currency; ATimeoutMs: Integer);
begin
  inherited Create('Paiement avec Timeout', ATimeoutMs);
  FClientId := AClientId;
  FMontant := AMontant;
end;

function TSagaPaiementAvecTimeout.Execute(AContext: TSagaContext): TSagaStepResult;  
var
  DelayMs: Integer;
begin
  inherited Execute(AContext);

  WriteLn(Format('  → Paiement de %.2f € (timeout: %d ms)',
    [FMontant, FTimeoutMs]));

  // Simuler traitement avec délai aléatoire
  DelayMs := Random(8000);
  WriteLn(Format('  → Temps de traitement simulé: %d ms', [DelayMs]));

  Sleep(DelayMs);

  if IsTimeout then
  begin
    WriteLn('  → ✗ TIMEOUT dépassé !');
    Result := ssFailure;
  end
  else
  begin
    WriteLn('  → ✓ Paiement effectué dans les temps');
    AContext.SetValue('transaction_id', NewGUID);
    Result := ssSuccess;
  end;
end;

function TSagaPaiementAvecTimeout.Compensate(AContext: TSagaContext): Boolean;  
begin
  WriteLn('  → Remboursement suite au timeout');
  Result := True;
end;

end.
```

### 2. Saga avec retry (retry automatique)

```pascal
unit Saga.Retry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Saga.Core;

type
  // Étape avec retry automatique
  TSagaStepWithRetry = class(TSagaStep)
  private
    FMaxRetries: Integer;
    FRetryDelayMs: Integer;
    FCurrentRetry: Integer;
  protected
    function ExecuteWithRetry(AContext: TSagaContext): TSagaStepResult; virtual; abstract;
  public
    constructor Create(const AName: string; AMaxRetries: Integer = 3;
      ARetryDelayMs: Integer = 1000);

    function Execute(AContext: TSagaContext): TSagaStepResult; override;
  end;

  // Exemple : Service externe instable
  TSagaServiceExterneInstable = class(TSagaStepWithRetry)
  private
    FServiceUrl: string;
  public
    constructor Create(const AServiceUrl: string);

    function ExecuteWithRetry(AContext: TSagaContext): TSagaStepResult; override;
    function Compensate(AContext: TSagaContext): Boolean; override;
  end;

implementation

// TSagaStepWithRetry

constructor TSagaStepWithRetry.Create(const AName: string;
  AMaxRetries: Integer; ARetryDelayMs: Integer);
begin
  inherited Create(AName);
  FMaxRetries := AMaxRetries;
  FRetryDelayMs := ARetryDelayMs;
  FCurrentRetry := 0;
end;

function TSagaStepWithRetry.Execute(AContext: TSagaContext): TSagaStepResult;  
begin
  FCurrentRetry := 0;

  repeat
    Result := ExecuteWithRetry(AContext);

    if Result = ssSuccess then
      Exit;

    Inc(FCurrentRetry);

    if FCurrentRetry < FMaxRetries then
    begin
      WriteLn(Format('  → Échec, retry %d/%d dans %d ms...',
        [FCurrentRetry, FMaxRetries, FRetryDelayMs]));
      Sleep(FRetryDelayMs);
    end;

  until FCurrentRetry >= FMaxRetries;

  WriteLn(Format('  → ✗ Échec après %d tentatives', [FMaxRetries]));
  Result := ssFailure;
end;

// TSagaServiceExterneInstable

constructor TSagaServiceExterneInstable.Create(const AServiceUrl: string);  
begin
  inherited Create('Service Externe Instable', 3, 1000);
  FServiceUrl := AServiceUrl;
end;

function TSagaServiceExterneInstable.ExecuteWithRetry(
  AContext: TSagaContext): TSagaStepResult;
begin
  WriteLn(Format('  → Appel service: %s (tentative %d)',
    [FServiceUrl, FCurrentRetry + 1]));

  // Simuler instabilité (50% de succès)
  Sleep(500);

  if Random(100) < 50 then
  begin
    WriteLn('  → ✓ Service OK');
    AContext.SetValue('service_response', 'OK-' + NewGUID);
    Result := ssSuccess;
  end
  else
  begin
    WriteLn('  → ✗ Service temporairement indisponible');
    Result := ssFailure;
  end;
end;

function TSagaServiceExterneInstable.Compensate(AContext: TSagaContext): Boolean;  
begin
  WriteLn('  → Annulation appel service externe');
  Result := True;
end;

end.
```

### 3. Saga avec état persistant

```pascal
unit Saga.Persistent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser,
  Saga.Core;

type
  // État de la saga
  TSagaState = (ssNew, ssRunning, ssCompleted, ssFailed, ssCompensating, ssCompensated);

  // Saga persistante
  TPersistentSaga = class(TSagaOrchestrator)
  private
    FSagaId: string;
    FState: TSagaState;
    FStateFile: string;

    procedure SaveState;
    procedure LoadState;
  public
    constructor Create(const ASagaId: string);
    destructor Destroy; override;

    function Execute: Boolean; override;
    procedure Resume;

    property SagaId: string read FSagaId;
    property State: TSagaState read FState;
  end;

implementation

uses
  Variants;

// TPersistentSaga

constructor TPersistentSaga.Create(const ASagaId: string);  
begin
  inherited Create;
  FSagaId := ASagaId;
  FState := ssNew;

  {$IFDEF WINDOWS}
  FStateFile := 'C:\temp\saga_' + FSagaId + '.json';
  {$ENDIF}
  {$IFDEF UNIX}
  FStateFile := '/tmp/saga_' + FSagaId + '.json';
  {$ENDIF}
end;

destructor TPersistentSaga.Destroy;  
begin
  SaveState;
  inherited;
end;

procedure TPersistentSaga.SaveState;  
var
  JSON: TJSONObject;
  StateFile: TextFile;
  i: Integer;
begin
  JSON := TJSONObject.Create;
  try
    JSON.Add('saga_id', FSagaId);
    JSON.Add('state', Integer(FState));
    JSON.Add('executed_steps', FExecutedSteps.Count);

    // Sauvegarder le contexte
    JSON.Add('context', FContext.Data.Clone as TJSONObject);

    // Écrire dans le fichier
    AssignFile(StateFile, FStateFile);
    try
      Rewrite(StateFile);
      WriteLn(StateFile, JSON.AsJSON);
    finally
      CloseFile(StateFile);
    end;

    WriteLn(Format('[PersistentSaga] État sauvegardé: %s', [FStateFile]));

  finally
    JSON.Free;
  end;
end;

procedure TPersistentSaga.LoadState;  
var
  StateFile: TextFile;
  JSONStr: string;
  JSON: TJSONObject;
  Parser: TJSONParser;
begin
  if not FileExists(FStateFile) then
    Exit;

  AssignFile(StateFile, FStateFile);
  try
    Reset(StateFile);
    ReadLn(StateFile, JSONStr);
  finally
    CloseFile(StateFile);
  end;

  Parser := TJSONParser.Create(JSONStr, [joUTF8]);
  try
    JSON := Parser.Parse as TJSONObject;
    try
      FState := TSagaState(JSON.Get('state', 0));

      WriteLn(Format('[PersistentSaga] État restauré depuis: %s', [FStateFile]));
      WriteLn(Format('[PersistentSaga] État: %d', [Integer(FState)]));

    finally
      JSON.Free;
    end;
  finally
    Parser.Free;
  end;
end;

function TPersistentSaga.Execute: Boolean;  
begin
  FState := ssRunning;
  SaveState;

  Result := inherited Execute;

  if Result then
    FState := ssCompleted
  else
    FState := ssFailed;

  SaveState;
end;

procedure TPersistentSaga.Resume;  
begin
  WriteLn('[PersistentSaga] Tentative de reprise...');
  LoadState;

  case FState of
    ssNew, ssRunning:
    begin
      WriteLn('[PersistentSaga] Reprise de l''exécution');
      Execute;
    end;
    ssCompleted:
      WriteLn('[PersistentSaga] Saga déjà complétée');
    ssFailed, ssCompensated:
      WriteLn('[PersistentSaga] Saga terminée en échec');
  end;
end;

end.
```

## Gestion des erreurs et idempotence

### 1. Idempotence des opérations

**Principe crucial :** Chaque opération de saga doit être **idempotente**, c'est-à-dire que l'exécuter plusieurs fois produit le même résultat.

```pascal
unit Saga.Idempotent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  Saga.Core;

type
  // Gestionnaire d'idempotence
  TIdempotencyManager = class
  private
    FExecutedOperations: TDictionary<string, Boolean>;
  public
    constructor Create;
    destructor Destroy; override;

    function IsAlreadyExecuted(const AOperationId: string): Boolean;
    procedure MarkAsExecuted(const AOperationId: string);
    procedure Clear;
  end;

  // Étape idempotente
  TSagaIdempotentStep = class(TSagaStep)
  private
    FOperationId: string;
    FIdempotencyManager: TIdempotencyManager;
  public
    constructor Create(const AName, AOperationId: string;
      AManager: TIdempotencyManager);

    function Execute(AContext: TSagaContext): TSagaStepResult; override;
  end;

implementation

// TIdempotencyManager

constructor TIdempotencyManager.Create;  
begin
  inherited Create;
  FExecutedOperations := TDictionary<string, Boolean>.Create;
end;

destructor TIdempotencyManager.Destroy;  
begin
  FExecutedOperations.Free;
  inherited;
end;

function TIdempotencyManager.IsAlreadyExecuted(const AOperationId: string): Boolean;  
begin
  Result := FExecutedOperations.ContainsKey(AOperationId);
end;

procedure TIdempotencyManager.MarkAsExecuted(const AOperationId: string);  
begin
  FExecutedOperations.AddOrSetValue(AOperationId, True);
  WriteLn(Format('[Idempotency] Opération marquée: %s', [AOperationId]));
end;

procedure TIdempotencyManager.Clear;  
begin
  FExecutedOperations.Clear;
end;

// TSagaIdempotentStep

constructor TSagaIdempotentStep.Create(const AName, AOperationId: string;
  AManager: TIdempotencyManager);
begin
  inherited Create(AName);
  FOperationId := AOperationId;
  FIdempotencyManager := AManager;
end;

function TSagaIdempotentStep.Execute(AContext: TSagaContext): TSagaStepResult;  
begin
  if FIdempotencyManager.IsAlreadyExecuted(FOperationId) then
  begin
    WriteLn(Format('  → Opération %s déjà exécutée, skip', [FOperationId]));
    Result := ssSuccess;
  end
  else
  begin
    // Exécuter l'opération réelle (à override)
    Result := ssSuccess;
    FIdempotencyManager.MarkAsExecuted(FOperationId);
  end;
end;

end.
```

### 2. Gestion des pannes partielles

```pascal
// Exemple : Compensation partielle
procedure TOrderSaga.HandlePartialFailure;  
begin
  WriteLn('[Saga] Gestion panne partielle');

  // Log de l'état pour investigation
  LogCurrentState;

  // Alerter l'équipe
  SendAlert('Saga partiellement échouée', GetContext);

  // Marquer pour intervention manuelle si nécessaire
  if RequiresManualIntervention then
    FlagForManualReview;
end;
```

## Monitoring et Observabilité

### Métriques importantes à tracker

```pascal
unit Saga.Monitoring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, DateUtils;

type
  // Métriques de saga
  TSagaMetrics = class
  private
    FSagaId: string;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FStepMetrics: TDictionary<string, Integer>; // Step name → duration ms
    FSuccess: Boolean;
    FCompensationCount: Integer;
    FErrorMessage: string;
  public
    constructor Create(const ASagaId: string);
    destructor Destroy; override;

    procedure StartSaga;
    procedure EndSaga(ASuccess: Boolean; const AErrorMessage: string = '');
    procedure RecordStepDuration(const AStepName: string; ADurationMs: Integer);
    procedure IncrementCompensation;

    function GetTotalDurationMs: Int64;
    function GetAveragStepDurationMs: Double;
    procedure PrintReport;

    property SagaId: string read FSagaId;
    property Success: Boolean read FSuccess;
  end;

  // Collecteur de métriques global
  TSagaMetricsCollector = class
  private
    FMetrics: TObjectList<TSagaMetrics>;
    class var FInstance: TSagaMetricsCollector;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddMetrics(AMetrics: TSagaMetrics);
    procedure PrintGlobalReport;

    class function Instance: TSagaMetricsCollector;
    class procedure FreeInstance;
  end;

implementation

// TSagaMetrics

constructor TSagaMetrics.Create(const ASagaId: string);  
begin
  inherited Create;
  FSagaId := ASagaId;
  FStepMetrics := TDictionary<string, Integer>.Create;
  FCompensationCount := 0;
  FSuccess := False;
end;

destructor TSagaMetrics.Destroy;  
begin
  FStepMetrics.Free;
  inherited;
end;

procedure TSagaMetrics.StartSaga;  
begin
  FStartTime := Now;
end;

procedure TSagaMetrics.EndSaga(ASuccess: Boolean; const AErrorMessage: string);  
begin
  FEndTime := Now;
  FSuccess := ASuccess;
  FErrorMessage := AErrorMessage;
end;

procedure TSagaMetrics.RecordStepDuration(const AStepName: string; ADurationMs: Integer);  
begin
  FStepMetrics.AddOrSetValue(AStepName, ADurationMs);
end;

procedure TSagaMetrics.IncrementCompensation;  
begin
  Inc(FCompensationCount);
end;

function TSagaMetrics.GetTotalDurationMs: Int64;  
begin
  Result := MilliSecondsBetween(FEndTime, FStartTime);
end;

function TSagaMetrics.GetAveragStepDurationMs: Double;  
var
  Total: Integer;
  Duration: Integer;
begin
  if FStepMetrics.Count = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Total := 0;
  for Duration in FStepMetrics.Values do
    Total := Total + Duration;

  Result := Total / FStepMetrics.Count;
end;

procedure TSagaMetrics.PrintReport;  
var
  Pair: TPair<string, Integer>;
begin
  WriteLn;
  WriteLn('╔═══════════════════════════════════════════╗');
  WriteLn('║        RAPPORT MÉTRIQUES SAGA             ║');
  WriteLn('╚═══════════════════════════════════════════╝');
  WriteLn(Format('Saga ID: %s', [FSagaId]));
  WriteLn(Format('Succès: %s', [BoolToStr(FSuccess, True)]));
  WriteLn(Format('Durée totale: %d ms', [GetTotalDurationMs]));
  WriteLn(Format('Compensations: %d', [FCompensationCount]));

  if FErrorMessage <> '' then
    WriteLn(Format('Erreur: %s', [FErrorMessage]));

  WriteLn;
  WriteLn('Durée par étape:');
  for Pair in FStepMetrics do
    WriteLn(Format('  - %s: %d ms', [Pair.Key, Pair.Value]));

  WriteLn(Format('Durée moyenne: %.2f ms', [GetAveragStepDurationMs]));
  WriteLn('═══════════════════════════════════════════');
end;

// TSagaMetricsCollector

constructor TSagaMetricsCollector.Create;  
begin
  inherited Create;
  FMetrics := TObjectList<TSagaMetrics>.Create(True);
end;

destructor TSagaMetricsCollector.Destroy;  
begin
  FMetrics.Free;
  inherited;
end;

procedure TSagaMetricsCollector.AddMetrics(AMetrics: TSagaMetrics);  
begin
  FMetrics.Add(AMetrics);
end;

procedure TSagaMetricsCollector.PrintGlobalReport;  
var
  Metrics: TSagaMetrics;
  TotalSagas, SuccessCount, FailureCount: Integer;
  TotalDuration: Int64;
  TotalCompensations: Integer;
begin
  TotalSagas := FMetrics.Count;
  SuccessCount := 0;
  FailureCount := 0;
  TotalDuration := 0;
  TotalCompensations := 0;

  for Metrics in FMetrics do
  begin
    if Metrics.Success then
      Inc(SuccessCount)
    else
      Inc(FailureCount);

    TotalDuration := TotalDuration + Metrics.GetTotalDurationMs;
    TotalCompensations := TotalCompensations + Metrics.FCompensationCount;
  end;

  WriteLn;
  WriteLn('╔═══════════════════════════════════════════╗');
  WriteLn('║      RAPPORT GLOBAL DES SAGAS             ║');
  WriteLn('╚═══════════════════════════════════════════╝');
  WriteLn(Format('Total de sagas: %d', [TotalSagas]));
  WriteLn(Format('Succès: %d (%.1f%%)', [SuccessCount, (SuccessCount / TotalSagas) * 100]));
  WriteLn(Format('Échecs: %d (%.1f%%)', [FailureCount, (FailureCount / TotalSagas) * 100]));
  WriteLn(Format('Durée totale: %d ms', [TotalDuration]));
  WriteLn(Format('Durée moyenne: %.2f ms', [TotalDuration / TotalSagas]));
  WriteLn(Format('Total compensations: %d', [TotalCompensations]));
  WriteLn('═══════════════════════════════════════════');
end;

class function TSagaMetricsCollector.Instance: TSagaMetricsCollector;  
begin
  if FInstance = nil then
    FInstance := TSagaMetricsCollector.Create;
  Result := FInstance;
end;

class procedure TSagaMetricsCollector.FreeInstance;  
begin
  if FInstance <> nil then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

end.
```

## Saga avec Communication Asynchrone

### 1. Saga avec Message Queue

```pascal
unit Saga.MessageQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, fpjson;

type
  // Message pour la queue
  TSagaMessage = class
  private
    FMessageId: string;
    FMessageType: string;
    FPayload: TJSONObject;
    FTimestamp: TDateTime;
    FRetryCount: Integer;
  public
    constructor Create(const AMessageType: string; APayload: TJSONObject);
    destructor Destroy; override;

    property MessageId: string read FMessageId;
    property MessageType: string read FMessageType;
    property Payload: TJSONObject read FPayload;
    property RetryCount: Integer read FRetryCount write FRetryCount;
  end;

  // Handler de message
  TMessageHandler = procedure(AMessage: TSagaMessage) of object;

  // Queue de messages (simulation)
  TMessageQueue = class
  private
    FQueue: TThreadList<TSagaMessage>;
    FHandlers: TDictionary<string, TMessageHandler>;
    FProcessing: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Publish(AMessage: TSagaMessage);
    procedure Subscribe(const AMessageType: string; AHandler: TMessageHandler);
    procedure StartProcessing;
    procedure StopProcessing;

    procedure ProcessMessages;
  end;

  // Saga asynchrone avec queue
  TAsyncSagaStep = class
  private
    FStepId: string;
    FQueue: TMessageQueue;
  public
    constructor Create(const AStepId: string; AQueue: TMessageQueue);

    procedure ExecuteAsync(AContext: TJSONObject);
    procedure OnComplete(AMessage: TSagaMessage); virtual; abstract;
    procedure OnError(AMessage: TSagaMessage); virtual; abstract;
  end;

implementation

// TSagaMessage

constructor TSagaMessage.Create(const AMessageType: string; APayload: TJSONObject);  
begin
  inherited Create;
  FMessageId := NewGUID;
  FMessageType := AMessageType;
  FPayload := APayload;
  FTimestamp := Now;
  FRetryCount := 0;
end;

destructor TSagaMessage.Destroy;  
begin
  FPayload.Free;
  inherited;
end;

// TMessageQueue

constructor TMessageQueue.Create;  
begin
  inherited Create;
  FQueue := TThreadList<TSagaMessage>.Create;
  FHandlers := TDictionary<string, TMessageHandler>.Create;
  FProcessing := False;
end;

destructor TMessageQueue.Destroy;  
var
  List: TList<TSagaMessage>;
  Msg: TSagaMessage;
begin
  StopProcessing;

  List := FQueue.LockList;
  try
    for Msg in List do
      Msg.Free;
    List.Clear;
  finally
    FQueue.UnlockList;
  end;

  FQueue.Free;
  FHandlers.Free;
  inherited;
end;

procedure TMessageQueue.Publish(AMessage: TSagaMessage);  
var
  List: TList<TSagaMessage>;
begin
  List := FQueue.LockList;
  try
    List.Add(AMessage);
    WriteLn(Format('[Queue] Message publié: %s (%s)',
      [AMessage.MessageType, AMessage.MessageId]));
  finally
    FQueue.UnlockList;
  end;
end;

procedure TMessageQueue.Subscribe(const AMessageType: string;
  AHandler: TMessageHandler);
begin
  FHandlers.AddOrSetValue(AMessageType, AHandler);
  WriteLn(Format('[Queue] Souscription: %s', [AMessageType]));
end;

procedure TMessageQueue.StartProcessing;  
begin
  FProcessing := True;
  WriteLn('[Queue] Traitement démarré');
end;

procedure TMessageQueue.StopProcessing;  
begin
  FProcessing := False;
  WriteLn('[Queue] Traitement arrêté');
end;

procedure TMessageQueue.ProcessMessages;  
var
  List: TList<TSagaMessage>;
  Msg: TSagaMessage;
  Handler: TMessageHandler;
  i: Integer;
begin
  if not FProcessing then
    Exit;

  List := FQueue.LockList;
  try
    i := 0;
    while i < List.Count do
    begin
      Msg := List[i];

      if FHandlers.TryGetValue(Msg.MessageType, Handler) then
      begin
        WriteLn(Format('[Queue] Traitement message: %s', [Msg.MessageId]));

        try
          Handler(Msg);
          List.Delete(i);
          Msg.Free;
        except
          on E: Exception do
          begin
            WriteLn(Format('[Queue] Erreur traitement: %s', [E.Message]));
            Inc(Msg.FRetryCount);

            if Msg.RetryCount > 3 then
            begin
              WriteLn(Format('[Queue] Message abandonné après %d tentatives',
                [Msg.RetryCount]));
              List.Delete(i);
              Msg.Free;
            end
            else
              Inc(i);
          end;
        end;
      end
      else
        Inc(i);
    end;
  finally
    FQueue.UnlockList;
  end;
end;

// TAsyncSagaStep

constructor TAsyncSagaStep.Create(const AStepId: string; AQueue: TMessageQueue);  
begin
  inherited Create;
  FStepId := AStepId;
  FQueue := AQueue;
end;

procedure TAsyncSagaStep.ExecuteAsync(AContext: TJSONObject);  
var
  Msg: TSagaMessage;
begin
  Msg := TSagaMessage.Create('saga.step.execute', AContext.Clone as TJSONObject);
  Msg.Payload.Add('step_id', FStepId);

  FQueue.Publish(Msg);
end;

end.
```

### 2. Exemple de Saga distribuée asynchrone

```pascal
program AsyncSagaDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson,
  Saga.MessageQueue;

type
  // Étape asynchrone de réservation
  TAsyncReservationStep = class(TAsyncSagaStep)
  public
    procedure OnComplete(AMessage: TSagaMessage); override;
    procedure OnError(AMessage: TSagaMessage); override;
  end;

  // Étape asynchrone de paiement
  TAsyncPaymentStep = class(TAsyncSagaStep)
  public
    procedure OnComplete(AMessage: TSagaMessage); override;
    procedure OnError(AMessage: TSagaMessage); override;
  end;

// TAsyncReservationStep

procedure TAsyncReservationStep.OnComplete(AMessage: TSagaMessage);  
var
  CommandeId: string;
begin
  CommandeId := AMessage.Payload.Get('commande_id', '');
  WriteLn(Format('[AsyncReservation] Réservation complétée: %s', [CommandeId]));

  // Déclencher l'étape suivante
  // ...
end;

procedure TAsyncReservationStep.OnError(AMessage: TSagaMessage);  
begin
  WriteLn('[AsyncReservation] Erreur réservation');
  // Déclencher compensation
  // ...
end;

// TAsyncPaymentStep

procedure TAsyncPaymentStep.OnComplete(AMessage: TSagaMessage);  
var
  CommandeId: string;
begin
  CommandeId := AMessage.Payload.Get('commande_id', '');
  WriteLn(Format('[AsyncPayment] Paiement complété: %s', [CommandeId]));
end;

procedure TAsyncPaymentStep.OnError(AMessage: TSagaMessage);  
begin
  WriteLn('[AsyncPayment] Erreur paiement');
  // Déclencher compensation
  // ...
end;

var
  Queue: TMessageQueue;
  ReservationStep: TAsyncReservationStep;
  PaymentStep: TAsyncPaymentStep;
  Context: TJSONObject;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Démonstration Saga Asynchrone           ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  Queue := TMessageQueue.Create;
  try
    ReservationStep := TAsyncReservationStep.Create('reservation', Queue);
    PaymentStep := TAsyncPaymentStep.Create('payment', Queue);

    try
      // Souscrire aux événements
      Queue.Subscribe('saga.step.complete', @ReservationStep.OnComplete);
      Queue.Subscribe('saga.step.error', @ReservationStep.OnError);
      Queue.Subscribe('saga.step.complete', @PaymentStep.OnComplete);

      // Démarrer le traitement
      Queue.StartProcessing;

      // Créer le contexte
      Context := TJSONObject.Create;
      try
        Context.Add('commande_id', NewGUID);
        Context.Add('client_id', 'CLIENT-123');
        Context.Add('montant', 99.99);

        // Lancer la saga
        ReservationStep.ExecuteAsync(Context);

        // Simuler le traitement asynchrone
        Sleep(100);
        Queue.ProcessMessages;

      finally
        Context.Free;
      end;

    finally
      PaymentStep.Free;
      ReservationStep.Free;
    end;

  finally
    Queue.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Patterns de Compensation Avancés

### 1. Compensation sémantique

```pascal
unit Saga.SemanticCompensation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Saga.Core;

type
  // Compensation qui ne peut pas annuler exactement
  TSemanticCompensation = class(TSagaStep)
  private
    FOriginalAmount: Currency;
    FCompensationAmount: Currency;
  public
    constructor Create(const AName: string);

    function Execute(AContext: TSagaContext): TSagaStepResult; override;
    function Compensate(AContext: TSagaContext): Boolean; override;
  end;

  // Exemple : Envoi d'email (impossible à annuler exactement)
  TSagaEnvoyerEmailCompensation = class(TSemanticCompensation)
  private
    FDestinataire: string;
    FMessageEnvoye: Boolean;
  public
    constructor Create(const ADestinataire: string);

    function Execute(AContext: TSagaContext): TSagaStepResult; override;
    function Compensate(AContext: TSagaContext): Boolean; override;
  end;

implementation

// TSemanticCompensation

constructor TSemanticCompensation.Create(const AName: string);  
begin
  inherited Create(AName);
  FOriginalAmount := 0;
  FCompensationAmount := 0;
end;

function TSemanticCompensation.Execute(AContext: TSagaContext): TSagaStepResult;  
begin
  Result := ssSuccess;
end;

function TSemanticCompensation.Compensate(AContext: TSagaContext): Boolean;  
begin
  WriteLn('  → Compensation sémantique (action équivalente)');
  Result := True;
end;

// TSagaEnvoyerEmailCompensation

constructor TSagaEnvoyerEmailCompensation.Create(const ADestinataire: string);  
begin
  inherited Create('Envoyer Email');
  FDestinataire := ADestinataire;
  FMessageEnvoye := False;
end;

function TSagaEnvoyerEmailCompensation.Execute(AContext: TSagaContext): TSagaStepResult;  
begin
  WriteLn(Format('  → Envoi email à: %s', [FDestinataire]));
  WriteLn('  → Objet: Confirmation de commande');

  Sleep(50);

  FMessageEnvoye := True;
  WriteLn('  → ✓ Email envoyé');

  Result := ssSuccess;
end;

function TSagaEnvoyerEmailCompensation.Compensate(AContext: TSagaContext): Boolean;  
begin
  if not FMessageEnvoye then
  begin
    WriteLn('  → Aucune compensation nécessaire (email non envoyé)');
    Result := True;
    Exit;
  end;

  WriteLn('  → COMPENSATION SÉMANTIQUE:');
  WriteLn(Format('  → Envoi email d''annulation à: %s', [FDestinataire]));
  WriteLn('  → Objet: Annulation de commande');

  Sleep(50);

  WriteLn('  → ✓ Email d''annulation envoyé');
  WriteLn('  → Note: L''email original ne peut pas être "non-envoyé"');

  Result := True;
end;

end.
```

### 2. Compensation avec historique

```pascal
unit Saga.CompensationHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, fpjson;

type
  // Enregistrement d'une action
  TActionRecord = class
  private
    FActionId: string;
    FActionType: string;
    FTimestamp: TDateTime;
    FData: TJSONObject;
    FCompensated: Boolean;
  public
    constructor Create(const AActionType: string; AData: TJSONObject);
    destructor Destroy; override;

    property ActionId: string read FActionId;
    property ActionType: string read FActionType;
    property Compensated: Boolean read FCompensated write FCompensated;
  end;

  // Gestionnaire d'historique
  TCompensationHistory = class
  private
    FActions: TObjectList<TActionRecord>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RecordAction(const AActionType: string; AData: TJSONObject);
    procedure MarkAsCompensated(const AActionId: string);

    function GetUncompensatedActions: TList<TActionRecord>;
    procedure PrintHistory;
  end;

implementation

// TActionRecord

constructor TActionRecord.Create(const AActionType: string; AData: TJSONObject);  
begin
  inherited Create;
  FActionId := NewGUID;
  FActionType := AActionType;
  FTimestamp := Now;
  FData := AData;
  FCompensated := False;
end;

destructor TActionRecord.Destroy;  
begin
  FData.Free;
  inherited;
end;

// TCompensationHistory

constructor TCompensationHistory.Create;  
begin
  inherited Create;
  FActions := TObjectList<TActionRecord>.Create(True);
end;

destructor TCompensationHistory.Destroy;  
begin
  FActions.Free;
  inherited;
end;

procedure TCompensationHistory.RecordAction(const AActionType: string;
  AData: TJSONObject);
var
  Action: TActionRecord;
begin
  Action := TActionRecord.Create(AActionType, AData);
  FActions.Add(Action);

  WriteLn(Format('[History] Action enregistrée: %s (%s)',
    [AActionType, Action.ActionId]));
end;

procedure TCompensationHistory.MarkAsCompensated(const AActionId: string);  
var
  Action: TActionRecord;
begin
  for Action in FActions do
  begin
    if Action.ActionId = AActionId then
    begin
      Action.FCompensated := True;
      WriteLn(Format('[History] Action compensée: %s', [AActionId]));
      Break;
    end;
  end;
end;

function TCompensationHistory.GetUncompensatedActions: TList<TActionRecord>;  
var
  Action: TActionRecord;
begin
  Result := TList<TActionRecord>.Create;

  for Action in FActions do
  begin
    if not Action.Compensated then
      Result.Add(Action);
  end;
end;

procedure TCompensationHistory.PrintHistory;  
var
  Action: TActionRecord;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  HISTORIQUE DES ACTIONS                   ');
  WriteLn('═══════════════════════════════════════════');

  for Action in FActions do
  begin
    WriteLn(Format('Action: %s', [Action.ActionType]));
    WriteLn(Format('  ID: %s', [Action.ActionId]));
    WriteLn(Format('  Timestamp: %s', [DateTimeToStr(Action.FTimestamp)]));
    WriteLn(Format('  Compensée: %s', [BoolToStr(Action.Compensated, True)]));
    WriteLn;
  end;

  WriteLn('═══════════════════════════════════════════');
end;

end.
```

## Bonnes Pratiques

### ✅ À faire

1. **Rendre les opérations idempotentes**
```pascal
// ✅ BON: Vérifier avant d'agir
if not IsAlreadyProcessed(OrderId) then  
begin
  ProcessOrder(OrderId);
  MarkAsProcessed(OrderId);
end;
```

2. **Timeout sur toutes les opérations**
```pascal
// ✅ BON: Timeout défini
Result := ExecuteWithTimeout(Operation, 5000); // 5 secondes max
```

3. **Logger toutes les étapes**
```pascal
// ✅ BON: Logging complet
LogInfo('Début étape: ' + StepName);  
try
  Result := ExecuteStep;
  LogSuccess('Étape réussie: ' + StepName);
except
  on E: Exception do
    LogError('Étape échouée: ' + StepName, E);
end;
```

4. **Utiliser des identifiants de corrélation**
```pascal
// ✅ BON: Traçabilité complète
Context.SetValue('correlation_id', CorrelationId);  
Context.SetValue('saga_id', SagaId);  
Context.SetValue('step_id', StepId);
```

### ❌ À éviter

1. **Oublier l'idempotence**
```pascal
// ❌ MAUVAIS: Peut créer des doublons
Stock := Stock - Quantity; // Sans vérification
```

2. **Compensations partielles**
```pascal
// ❌ MAUVAIS: Compensation incomplète
procedure Compensate;  
begin
  ReleaseStock; // Mais oublie de rembourser !
end;
```

3. **Pas de timeout**
```pascal
// ❌ MAUVAIS: Peut bloquer indéfiniment
while not ServiceResponds do
  Sleep(1000); // Boucle infinie possible
```

4. **État partagé mutable**
```pascal
// ❌ MAUVAIS: Risque de concurrence
GlobalCounter := GlobalCounter + 1; // Non thread-safe
```

## Conclusion

Le Saga Pattern est essentiel pour gérer les transactions distribuées dans les architectures microservices. Les points clés à retenir :

**Concepts fondamentaux :**
- Une saga = séquence de transactions locales + compensations
- Deux approches : Orchestration (centralisée) vs Chorégraphie (événements)
- Toutes les opérations doivent être idempotentes

**Quand utiliser quoi :**
- **Orchestration** : workflows complexes, debugging important
- **Chorégraphie** : haute scalabilité, faible couplage

**Patterns avancés :**
- Timeout pour éviter les blocages
- Retry automatique pour la résilience
- État persistant pour la reprise
- Compensation sémantique quand annulation impossible

**Monitoring :**
- Métriques de performance
- Traçabilité complète
- Alertes sur échecs
- Historique des actions

Le Saga Pattern est plus complexe que les transactions ACID classiques, mais c'est le prix à payer pour construire des systèmes distribués scalables et résilients.

---

🔝 Retour au [Sommaire](/SOMMAIRE.md)

⏭️ [API Gateway patterns](/21-architecture-logicielle-avancee/09-api-gateway-patterns.md)
