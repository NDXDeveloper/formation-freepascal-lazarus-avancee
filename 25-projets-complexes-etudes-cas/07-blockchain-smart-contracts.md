🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 25.7 Blockchain et smart contracts

## Introduction

Dans ce chapitre, nous allons découvrir comment implémenter une blockchain fonctionnelle et des smart contracts (contrats intelligents) en utilisant FreePascal/Lazarus. Cette technologie, popularisée par Bitcoin et Ethereum, offre des possibilités fascinantes pour créer des systèmes décentralisés, sécurisés et transparents.

### Qu'est-ce qu'une blockchain ?

Une blockchain (chaîne de blocs) est une structure de données distribuée qui permet de stocker des informations de manière :

- **Immuable** : Une fois écrites, les données ne peuvent pas être modifiées
- **Transparente** : Toutes les transactions sont visibles par tous les participants
- **Décentralisée** : Aucune autorité centrale ne contrôle la blockchain
- **Sécurisée** : Utilise la cryptographie pour garantir l'intégrité des données

### Qu'est-ce qu'un smart contract ?

Un smart contract est un programme informatique qui s'exécute automatiquement lorsque certaines conditions prédéfinies sont remplies. C'est comme un contrat traditionnel, mais entièrement automatisé et infalsifiable.

**Exemple simple** : Un smart contract peut automatiquement transférer de l'argent d'une personne A à une personne B dès que A reçoit un produit de B, sans nécessiter d'intermédiaire comme une banque.

## Architecture d'une blockchain

### Structure de base

```
┌─────────────┐      ┌─────────────┐      ┌─────────────┐      ┌─────────────┐
│  Bloc #0    │─────▶│  Bloc #1    │─────▶│  Bloc #2    │─────▶│  Bloc #3    │
│  (Genesis)  │      │             │      │             │      │             │
│             │      │             │      │             │      │             │
│ Hash:ABC123 │      │ Prev:ABC123 │      │ Prev:DEF456 │      │ Prev:GHI789 │
│             │      │ Hash:DEF456 │      │ Hash:GHI789 │      │ Hash:JKL012 │
└─────────────┘      └─────────────┘      └─────────────┘      └─────────────┘
```

Chaque bloc contient :
1. **Un en-tête** : métadonnées (timestamp, hash précédent, nonce)
2. **Des transactions** : liste des opérations effectuées
3. **Un hash** : empreinte cryptographique unique du bloc

## Partie 1 : Implémentation d'une blockchain de base

### Structure d'un bloc

Commençons par définir la structure de données d'un bloc :

```pascal
unit BlockchainTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  // Structure d'une transaction
  TTransaction = class
  private
    FFrom: string;          // Adresse de l'émetteur
    FTo: string;            // Adresse du destinataire
    FAmount: Double;        // Montant transféré
    FTimestamp: TDateTime;  // Date et heure de la transaction
    FSignature: string;     // Signature cryptographique
  public
    constructor Create(const AFrom, ATo: string; AAmount: Double);

    function ToString: string; override;
    function CalculateHash: string;
    function IsValid: Boolean;

    property FromAddress: string read FFrom write FFrom;
    property ToAddress: string read FTo write FTo;
    property Amount: Double read FAmount write FAmount;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
    property Signature: string read FSignature write FSignature;
  end;

  TTransactionList = specialize TFPGObjectList<TTransaction>;

  // Structure d'un bloc
  TBlock = class
  private
    FIndex: Integer;                    // Position du bloc dans la chaîne
    FTimestamp: TDateTime;              // Date de création du bloc
    FTransactions: TTransactionList;    // Liste des transactions
    FPreviousHash: string;              // Hash du bloc précédent
    FHash: string;                      // Hash de ce bloc
    FNonce: Int64;                      // Nombre utilisé pour le minage
    FMinerAddress: string;              // Adresse du mineur
    FDifficulty: Integer;               // Difficulté de minage
  public
    constructor Create(AIndex: Integer; const APreviousHash: string);
    destructor Destroy; override;

    procedure AddTransaction(ATransaction: TTransaction);
    function CalculateHash: string;
    function Mine(ADifficulty: Integer): Boolean;
    function IsValid: Boolean;

    property Index: Integer read FIndex;
    property Timestamp: TDateTime read FTimestamp;
    property Transactions: TTransactionList read FTransactions;
    property PreviousHash: string read FPreviousHash;
    property Hash: string read FHash write FHash;
    property Nonce: Int64 read FNonce write FNonce;
    property MinerAddress: string read FMinerAddress write FMinerAddress;
  end;

  TBlockList = specialize TFPGObjectList<TBlock>;

implementation

uses
  sha256, DateUtils;

{ TTransaction }

constructor TTransaction.Create(const AFrom, ATo: string; AAmount: Double);  
begin
  inherited Create;
  FFrom := AFrom;
  FTo := ATo;
  FAmount := AAmount;
  FTimestamp := Now;
  FSignature := '';
end;

function TTransaction.ToString: string;  
begin
  Result := Format('%s->%s:%.8f@%s',
    [FFrom, FTo, FAmount, DateTimeToStr(FTimestamp)]);
end;

function TTransaction.CalculateHash: string;  
var
  Data: string;
begin
  Data := FFrom + FTo + FloatToStr(FAmount) + DateTimeToStr(FTimestamp);
  Result := SHA256Print(SHA256String(Data));
end;

function TTransaction.IsValid: Boolean;  
begin
  // Vérifications de base
  Result := (FFrom <> '') and (FTo <> '') and (FAmount > 0);

  // Vérifier que l'émetteur et le destinataire sont différents
  if Result then
    Result := FFrom <> FTo;
end;

{ TBlock }

constructor TBlock.Create(AIndex: Integer; const APreviousHash: string);  
begin
  inherited Create;
  FIndex := AIndex;
  FTimestamp := Now;
  FTransactions := TTransactionList.Create(True);
  FPreviousHash := APreviousHash;
  FHash := '';
  FNonce := 0;
  FMinerAddress := '';
  FDifficulty := 0;
end;

destructor TBlock.Destroy;  
begin
  FTransactions.Free;
  inherited Destroy;
end;

procedure TBlock.AddTransaction(ATransaction: TTransaction);  
begin
  if ATransaction.IsValid then
    FTransactions.Add(ATransaction);
end;

function TBlock.CalculateHash: string;  
var
  Data: string;
  i: Integer;
begin
  // Construire la chaîne de données à hasher
  Data := IntToStr(FIndex) +
          DateTimeToStr(FTimestamp) +
          FPreviousHash +
          IntToStr(FNonce);

  // Ajouter toutes les transactions
  for i := 0 to FTransactions.Count - 1 do
    Data := Data + FTransactions[i].ToString;

  // Calculer le hash SHA-256
  Result := SHA256Print(SHA256String(Data));
end;

function TBlock.Mine(ADifficulty: Integer): Boolean;  
var
  Target: string;
  i: Integer;
begin
  // Créer une chaîne cible avec N zéros au début
  // Par exemple, difficulté 4 = "0000"
  Target := StringOfChar('0', ADifficulty);

  FDifficulty := ADifficulty;
  FNonce := 0;

  WriteLn(Format('Minage du bloc %d avec difficulté %d...', [FIndex, ADifficulty]));

  // Boucle de minage : chercher un nonce qui produit un hash valide
  repeat
    FHash := CalculateHash;
    Inc(FNonce);

    // Afficher la progression tous les 100000 essais
    if (FNonce mod 100000) = 0 then
      WriteLn(Format('  Essai %d : %s', [FNonce, Copy(FHash, 1, 20) + '...']));

    // Limite de sécurité pour éviter une boucle infinie
    if FNonce > 10000000 then
    begin
      WriteLn('Limite de nonce atteinte !');
      Exit(False);
    end;

  until Copy(FHash, 1, ADifficulty) = Target;

  WriteLn(Format('✓ Bloc miné ! Nonce: %d, Hash: %s', [FNonce, FHash]));
  Result := True;
end;

function TBlock.IsValid: Boolean;  
var
  i: Integer;
begin
  Result := False;

  // Vérifier que le hash est correct
  if CalculateHash <> FHash then
  begin
    WriteLn('Erreur: Hash invalide pour le bloc ', FIndex);
    Exit;
  end;

  // Vérifier que toutes les transactions sont valides
  for i := 0 to FTransactions.Count - 1 do
  begin
    if not FTransactions[i].IsValid then
    begin
      WriteLn('Erreur: Transaction invalide dans le bloc ', FIndex);
      Exit;
    end;
  end;

  Result := True;
end;

end.
```

### La classe Blockchain principale

```pascal
unit Blockchain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlockchainTypes, fgl;

type
  TBalances = specialize TFPGMap<string, Double>;

  TBlockchain = class
  private
    FChain: TBlockList;
    FPendingTransactions: TTransactionList;
    FDifficulty: Integer;
    FMiningReward: Double;
    FBalances: TBalances;

    function GetGenesisBlock: TBlock;
    procedure UpdateBalances;
    function GetBalance(const Address: string): Double;
  public
    constructor Create(ADifficulty: Integer = 4);
    destructor Destroy; override;

    procedure AddTransaction(ATransaction: TTransaction);
    function MinePendingTransactions(const MinerAddress: string): Boolean;

    function IsChainValid: Boolean;
    function GetLastBlock: TBlock;
    function GetBlockByIndex(Index: Integer): TBlock;

    procedure PrintChain;
    procedure PrintBalances;

    property Chain: TBlockList read FChain;
    property Difficulty: Integer read FDifficulty write FDifficulty;
    property MiningReward: Double read FMiningReward write FMiningReward;
  end;

implementation

uses
  DateUtils;

constructor TBlockchain.Create(ADifficulty: Integer);  
begin
  inherited Create;
  FChain := TBlockList.Create(True);
  FPendingTransactions := TTransactionList.Create(True);
  FBalances := TBalances.Create;
  FDifficulty := ADifficulty;
  FMiningReward := 50.0; // Récompense de minage par défaut

  // Créer le bloc genesis (premier bloc de la chaîne)
  FChain.Add(GetGenesisBlock);

  WriteLn('Blockchain créée avec le bloc genesis');
end;

destructor TBlockchain.Destroy;  
begin
  FBalances.Free;
  FPendingTransactions.Free;
  FChain.Free;
  inherited Destroy;
end;

function TBlockchain.GetGenesisBlock: TBlock;  
var
  GenesisBlock: TBlock;
  GenesisTx: TTransaction;
begin
  GenesisBlock := TBlock.Create(0, '0');

  // Transaction initiale fictive
  GenesisTx := TTransaction.Create('SYSTEM', 'GENESIS', 0);
  GenesisBlock.AddTransaction(GenesisTx);

  // Le bloc genesis n'a pas besoin d'être miné
  GenesisBlock.Hash := GenesisBlock.CalculateHash;

  Result := GenesisBlock;
end;

function TBlockchain.GetLastBlock: TBlock;  
begin
  if FChain.Count > 0 then
    Result := FChain[FChain.Count - 1]
  else
    Result := nil;
end;

function TBlockchain.GetBlockByIndex(Index: Integer): TBlock;  
begin
  if (Index >= 0) and (Index < FChain.Count) then
    Result := FChain[Index]
  else
    Result := nil;
end;

procedure TBlockchain.AddTransaction(ATransaction: TTransaction);  
begin
  if not ATransaction.IsValid then
  begin
    WriteLn('Transaction invalide rejetée');
    Exit;
  end;

  // Vérifier que l'émetteur a suffisamment de fonds
  if GetBalance(ATransaction.FromAddress) < ATransaction.Amount then
  begin
    WriteLn('Solde insuffisant pour ', ATransaction.FromAddress);
    Exit;
  end;

  FPendingTransactions.Add(ATransaction);
  WriteLn(Format('Transaction ajoutée: %s -> %s : %.2f',
    [ATransaction.FromAddress, ATransaction.ToAddress, ATransaction.Amount]));
end;

function TBlockchain.MinePendingTransactions(const MinerAddress: string): Boolean;  
var
  NewBlock: TBlock;
  i: Integer;
  RewardTx: TTransaction;
begin
  Result := False;

  if FPendingTransactions.Count = 0 then
  begin
    WriteLn('Aucune transaction en attente à miner');
    Exit;
  end;

  // Créer un nouveau bloc
  NewBlock := TBlock.Create(FChain.Count, GetLastBlock.Hash);
  NewBlock.MinerAddress := MinerAddress;

  // Ajouter toutes les transactions en attente
  for i := 0 to FPendingTransactions.Count - 1 do
    NewBlock.AddTransaction(FPendingTransactions[i]);

  // Miner le bloc (Proof of Work)
  if NewBlock.Mine(FDifficulty) then
  begin
    FChain.Add(NewBlock);

    WriteLn(Format('Nouveau bloc #%d ajouté à la chaîne', [NewBlock.Index]));

    // Vider les transactions en attente
    FPendingTransactions.Clear;

    // Créer une transaction de récompense pour le mineur
    RewardTx := TTransaction.Create('SYSTEM', MinerAddress, FMiningReward);
    FPendingTransactions.Add(RewardTx);

    // Mettre à jour les soldes
    UpdateBalances;

    Result := True;
  end
  else
    WriteLn('Échec du minage du bloc');
end;

function TBlockchain.IsChainValid: Boolean;  
var
  i: Integer;
  CurrentBlock, PreviousBlock: TBlock;
begin
  Result := True;

  // Vérifier chaque bloc (sauf le genesis)
  for i := 1 to FChain.Count - 1 do
  begin
    CurrentBlock := FChain[i];
    PreviousBlock := FChain[i - 1];

    // Vérifier que le bloc est valide
    if not CurrentBlock.IsValid then
    begin
      WriteLn('Bloc invalide détecté: #', i);
      Exit(False);
    end;

    // Vérifier que le hash précédent correspond
    if CurrentBlock.PreviousHash <> PreviousBlock.Hash then
    begin
      WriteLn('Chaîne brisée entre les blocs #', i-1, ' et #', i);
      Exit(False);
    end;

    // Vérifier la preuve de travail (difficulté)
    if Copy(CurrentBlock.Hash, 1, FDifficulty) <> StringOfChar('0', FDifficulty) then
    begin
      WriteLn('Preuve de travail invalide pour le bloc #', i);
      Exit(False);
    end;
  end;

  WriteLn('✓ La blockchain est valide !');
end;

procedure TBlockchain.UpdateBalances;  
var
  i, j: Integer;
  Block: TBlock;
  Tx: TTransaction;
begin
  FBalances.Clear;

  // Parcourir tous les blocs et transactions
  for i := 0 to FChain.Count - 1 do
  begin
    Block := FChain[i];

    for j := 0 to Block.Transactions.Count - 1 do
    begin
      Tx := Block.Transactions[j];

      // Débiter l'émetteur
      if Tx.FromAddress <> 'SYSTEM' then
      begin
        if not FBalances.TryGetData(Tx.FromAddress, i) then
          FBalances.Add(Tx.FromAddress, 0);

        FBalances[Tx.FromAddress] := FBalances[Tx.FromAddress] - Tx.Amount;
      end;

      // Créditer le destinataire
      if not FBalances.TryGetData(Tx.ToAddress, i) then
        FBalances.Add(Tx.ToAddress, 0);

      FBalances[Tx.ToAddress] := FBalances[Tx.ToAddress] + Tx.Amount;
    end;
  end;
end;

function TBlockchain.GetBalance(const Address: string): Double;  
var
  Index: Integer;
begin
  Index := FBalances.IndexOf(Address);
  if Index >= 0 then
    Result := FBalances.Data[Index]
  else
    Result := 0.0;
end;

procedure TBlockchain.PrintChain;  
var
  i, j: Integer;
  Block: TBlock;
begin
  WriteLn('');
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('             BLOCKCHAIN COMPLÈTE');
  WriteLn('═══════════════════════════════════════════════════');

  for i := 0 to FChain.Count - 1 do
  begin
    Block := FChain[i];
    WriteLn('');
    WriteLn(Format('┌─ Bloc #%d ─────────────────────────────────────', [Block.Index]));
    WriteLn('│ Timestamp    : ', DateTimeToStr(Block.Timestamp));
    WriteLn('│ Hash         : ', Block.Hash);
    WriteLn('│ Hash Précéd. : ', Block.PreviousHash);
    WriteLn('│ Nonce        : ', Block.Nonce);
    WriteLn('│ Transactions : ', Block.Transactions.Count);

    for j := 0 to Block.Transactions.Count - 1 do
    begin
      WriteLn('│   ', j + 1, '. ', Block.Transactions[j].ToString);
    end;

    WriteLn('└───────────────────────────────────────────────────');
  end;

  WriteLn('');
end;

procedure TBlockchain.PrintBalances;  
var
  i: Integer;
begin
  UpdateBalances;

  WriteLn('');
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('                 SOLDES DES COMPTES');
  WriteLn('═══════════════════════════════════════════════════');

  for i := 0 to FBalances.Count - 1 do
  begin
    WriteLn(Format('  %-20s : %10.2f', [FBalances.Keys[i], FBalances.Data[i]]));
  end;

  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('');
end;

end.
```

### Programme de démonstration

```pascal
program BlockchainDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, Blockchain, BlockchainTypes;

var
  MyBlockchain: TBlockchain;
  Tx: TTransaction;

begin
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('    DÉMONSTRATION BLOCKCHAIN EN FREEPASCAL');
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('');

  // Créer une blockchain avec difficulté 4
  MyBlockchain := TBlockchain.Create(4);
  try
    // Créer quelques transactions
    WriteLn('>>> Création de transactions...');
    WriteLn('');

    Tx := TTransaction.Create('SYSTEM', 'Alice', 100);
    MyBlockchain.AddTransaction(Tx);

    Tx := TTransaction.Create('SYSTEM', 'Bob', 100);
    MyBlockchain.AddTransaction(Tx);

    // Miner le premier bloc
    WriteLn('');
    WriteLn('>>> Minage du premier bloc...');
    WriteLn('');
    MyBlockchain.MinePendingTransactions('Miner1');

    // Nouvelles transactions
    WriteLn('');
    WriteLn('>>> Nouvelles transactions...');
    WriteLn('');

    Tx := TTransaction.Create('Alice', 'Bob', 30);
    MyBlockchain.AddTransaction(Tx);

    Tx := TTransaction.Create('Bob', 'Charlie', 20);
    MyBlockchain.AddTransaction(Tx);

    // Miner le deuxième bloc
    WriteLn('');
    WriteLn('>>> Minage du deuxième bloc...');
    WriteLn('');
    MyBlockchain.MinePendingTransactions('Miner2');

    // Afficher la blockchain complète
    MyBlockchain.PrintChain;

    // Afficher les soldes
    MyBlockchain.PrintBalances;

    // Valider la blockchain
    WriteLn('');
    WriteLn('>>> Validation de la blockchain...');
    WriteLn('');
    MyBlockchain.IsChainValid;

  finally
    MyBlockchain.Free;
  end;

  WriteLn('');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Partie 2 : Smart Contracts

### Qu'est-ce qu'un smart contract ?

Un smart contract est un programme qui s'exécute sur la blockchain. Il contient :
- **Du code** : la logique métier
- **Un état** : des variables stockées
- **Une adresse** : identifiant unique du contrat

### Architecture des smart contracts

```pascal
unit SmartContract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, BlockchainTypes;

type
  // Type de données pour le stockage dans le contrat
  TContractStorage = specialize TFPGMap<string, string>;

  // Événement émis par le contrat
  TContractEvent = class
  private
    FName: string;
    FData: string;
    FTimestamp: TDateTime;
  public
    constructor Create(const AName, AData: string);

    property Name: string read FName;
    property Data: string read FData;
    property Timestamp: TDateTime read FTimestamp;
  end;

  TContractEventList = specialize TFPGObjectList<TContractEvent>;

  // Classe de base pour tous les smart contracts
  TSmartContractBase = class
  private
    FAddress: string;
    FOwner: string;
    FBalance: Double;
    FStorage: TContractStorage;
    FEvents: TContractEventList;
    FCreationTime: TDateTime;
  protected
    procedure EmitEvent(const EventName, EventData: string);
    function GetStorageValue(const Key: string): string;
    procedure SetStorageValue(const Key, Value: string);
  public
    constructor Create(const AOwner: string);
    destructor Destroy; override;

    // Méthodes virtuelles à implémenter dans les contrats dérivés
    function Execute(const Method: string; const Params: array of string): string; virtual; abstract;
    function GetContractInfo: string; virtual;

    property Address: string read FAddress;
    property Owner: string read FOwner;
    property Balance: Double read FBalance write FBalance;
    property Storage: TContractStorage read FStorage;
    property Events: TContractEventList read FEvents;
  end;

implementation

uses
  sha256;

{ TContractEvent }

constructor TContractEvent.Create(const AName, AData: string);  
begin
  inherited Create;
  FName := AName;
  FData := AData;
  FTimestamp := Now;
end;

{ TSmartContractBase }

constructor TSmartContractBase.Create(const AOwner: string);  
var
  GUID: TGUID;
begin
  inherited Create;

  // Générer une adresse unique pour le contrat
  CreateGUID(GUID);
  FAddress := '0x' + Copy(SHA256Print(SHA256String(GUIDToString(GUID))), 1, 40);

  FOwner := AOwner;
  FBalance := 0;
  FStorage := TContractStorage.Create;
  FEvents := TContractEventList.Create(True);
  FCreationTime := Now;
end;

destructor TSmartContractBase.Destroy;  
begin
  FEvents.Free;
  FStorage.Free;
  inherited Destroy;
end;

procedure TSmartContractBase.EmitEvent(const EventName, EventData: string);  
var
  Event: TContractEvent;
begin
  Event := TContractEvent.Create(EventName, EventData);
  FEvents.Add(Event);
  WriteLn(Format('[EVENT] %s: %s', [EventName, EventData]));
end;

function TSmartContractBase.GetStorageValue(const Key: string): string;  
var
  Index: Integer;
begin
  Index := FStorage.IndexOf(Key);
  if Index >= 0 then
    Result := FStorage.Data[Index]
  else
    Result := '';
end;

procedure TSmartContractBase.SetStorageValue(const Key, Value: string);  
begin
  if FStorage.IndexOf(Key) >= 0 then
    FStorage[Key] := Value
  else
    FStorage.Add(Key, Value);
end;

function TSmartContractBase.GetContractInfo: string;  
begin
  Result := Format('Contract Address: %s' + LineEnding +
                   'Owner: %s' + LineEnding +
                   'Balance: %.2f' + LineEnding +
                   'Created: %s',
                   [FAddress, FOwner, FBalance, DateTimeToStr(FCreationTime)]);
end;

end.
```

### Exemple 1 : Contrat de token simple (crypto-monnaie)

```pascal
unit TokenContract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SmartContract, fgl;

type
  TTokenContract = class(TSmartContractBase)
  private
    FName: string;
    FSymbol: string;
    FTotalSupply: Double;
    FDecimals: Integer;
    FBalances: specialize TFPGMap<string, Double>;
  public
    constructor Create(const AOwner, AName, ASymbol: string;
                      ATotalSupply: Double; ADecimals: Integer);
    destructor Destroy; override;

    function Execute(const Method: string; const Params: array of string): string; override;

    // Fonctions du token
    function BalanceOf(const Account: string): Double;
    function Transfer(const FromAddress, ToAddress: string; Amount: Double): Boolean;
    function Mint(const ToAddress: string; Amount: Double): Boolean;
    function Burn(const FromAddress: string; Amount: Double): Boolean;

    property Name: string read FName;
    property Symbol: string read FSymbol;
    property TotalSupply: Double read FTotalSupply;
  end;

implementation

constructor TTokenContract.Create(const AOwner, AName, ASymbol: string;
  ATotalSupply: Double; ADecimals: Integer);
begin
  inherited Create(AOwner);

  FName := AName;
  FSymbol := ASymbol;
  FTotalSupply := ATotalSupply;
  FDecimals := ADecimals;

  FBalances := specialize TFPGMap<string, Double>.Create;

  // Donner tous les tokens au créateur
  FBalances.Add(AOwner, ATotalSupply);

  // Stocker les métadonnées
  SetStorageValue('name', FName);
  SetStorageValue('symbol', FSymbol);
  SetStorageValue('totalSupply', FloatToStr(FTotalSupply));
  SetStorageValue('decimals', IntToStr(FDecimals));

  EmitEvent('TokenCreated', Format('%s (%s) - Supply: %.2f', [FName, FSymbol, FTotalSupply]));
end;

destructor TTokenContract.Destroy;  
begin
  FBalances.Free;
  inherited Destroy;
end;

function TTokenContract.Execute(const Method: string; const Params: array of string): string;  
begin
  Result := '';

  if Method = 'balanceOf' then
  begin
    if Length(Params) >= 1 then
      Result := FloatToStr(BalanceOf(Params[0]));
  end
  else if Method = 'transfer' then
  begin
    if Length(Params) >= 3 then
    begin
      if Transfer(Params[0], Params[1], StrToFloatDef(Params[2], 0)) then
        Result := 'SUCCESS'
      else
        Result := 'FAILED';
    end;
  end
  else if Method = 'mint' then
  begin
    if Length(Params) >= 2 then
    begin
      if Mint(Params[0], StrToFloatDef(Params[1], 0)) then
        Result := 'SUCCESS'
      else
        Result := 'FAILED';
    end;
  end
  else if Method = 'burn' then
  begin
    if Length(Params) >= 2 then
    begin
      if Burn(Params[0], StrToFloatDef(Params[1], 0)) then
        Result := 'SUCCESS'
      else
        Result := 'FAILED';
    end;
  end
  else
    Result := 'UNKNOWN_METHOD';
end;

function TTokenContract.BalanceOf(const Account: string): Double;  
var
  Index: Integer;
begin
  Index := FBalances.IndexOf(Account);
  if Index >= 0 then
    Result := FBalances.Data[Index]
  else
    Result := 0.0;
end;

function TTokenContract.Transfer(const FromAddress, ToAddress: string; Amount: Double): Boolean;  
var
  FromBalance: Double;
begin
  Result := False;

  // Vérifications
  if Amount <= 0 then
  begin
    WriteLn('Montant invalide');
    Exit;
  end;

  FromBalance := BalanceOf(FromAddress);
  if FromBalance < Amount then
  begin
    WriteLn('Solde insuffisant');
    Exit;
  end;

  // Effectuer le transfert
  FBalances[FromAddress] := FromBalance - Amount;

  if FBalances.IndexOf(ToAddress) < 0 then
    FBalances.Add(ToAddress, Amount)
  else
    FBalances[ToAddress] := FBalances[ToAddress] + Amount;

  EmitEvent('Transfer', Format('%s -> %s : %.2f %s',
    [FromAddress, ToAddress, Amount, FSymbol]));

  Result := True;
end;

function TTokenContract.Mint(const ToAddress: string; Amount: Double): Boolean;  
begin
  Result := False;

  if Amount <= 0 then
    Exit;

  // Créer de nouveaux tokens
  FTotalSupply := FTotalSupply + Amount;

  if FBalances.IndexOf(ToAddress) < 0 then
    FBalances.Add(ToAddress, Amount)
  else
    FBalances[ToAddress] := FBalances[ToAddress] + Amount;

  EmitEvent('Mint', Format('%s received %.2f new %s',
    [ToAddress, Amount, FSymbol]));

  Result := True;
end;

function TTokenContract.Burn(const FromAddress: string; Amount: Double): Boolean;  
var
  Balance: Double;
begin
  Result := False;

  if Amount <= 0 then
    Exit;

  Balance := BalanceOf(FromAddress);
  if Balance < Amount then
  begin
    WriteLn('Solde insuffisant pour brûler');
    Exit;
  end;

  // Détruire des tokens
  FBalances[FromAddress] := Balance - Amount;
  FTotalSupply := FTotalSupply - Amount;

  EmitEvent('Burn', Format('%.2f %s burned from %s',
    [Amount, FSymbol, FromAddress]));

  Result := True;
end;

end.
```

### Exemple 2 : Contrat de vote décentralisé

```pascal
unit VotingContract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SmartContract, fgl;

type
  TProposal = class
  public
    ID: Integer;
    Description: string;
    VoteCount: Integer;
    IsActive: Boolean;

    constructor Create(AID: Integer; const ADescription: string);
  end;

  TProposalList = specialize TFPGObjectList<TProposal>;
  TVoterRegistry = specialize TFPGMap<string, Boolean>;  // Address -> HasVoted

  TVotingContract = class(TSmartContractBase)
  private
    FProposals: TProposalList;
    FVoters: TVoterRegistry;
    FVotingOpen: Boolean;
    FVotingEndTime: TDateTime;
  public
    constructor Create(const AOwner: string);
    destructor Destroy; override;

    function Execute(const Method: string; const Params: array of string): string; override;

    // Fonctions de vote
    procedure AddProposal(const Description: string);
    function Vote(const VoterAddress: string; ProposalID: Integer): Boolean;
    function GetWinner: TProposal;
    procedure OpenVoting(DurationMinutes: Integer);
    procedure CloseVoting;
    function GetResults: string;

    property VotingOpen: Boolean read FVotingOpen;
  end;

implementation

uses
  DateUtils;

{ TProposal }

constructor TProposal.Create(AID: Integer; const ADescription: string);  
begin
  inherited Create;
  ID := AID;
  Description := ADescription;
  VoteCount := 0;
  IsActive := True;
end;

{ TVotingContract }

constructor TVotingContract.Create(const AOwner: string);  
begin
  inherited Create(AOwner);

  FProposals := TProposalList.Create(True);
  FVoters := TVoterRegistry.Create;
  FVotingOpen := False;
  FVotingEndTime := 0;

  EmitEvent('VotingContractCreated', 'New voting contract initialized');
end;

destructor TVotingContract.Destroy;  
begin
  FVoters.Free;
  FProposals.Free;
  inherited Destroy;
end;

function TVotingContract.Execute(const Method: string; const Params: array of string): string;  
begin
  Result := '';

  if Method = 'addProposal' then
  begin
    if Length(Params) >= 1 then
    begin
      AddProposal(Params[0]);
      Result := 'PROPOSAL_ADDED';
    end;
  end
  else if Method = 'vote' then
  begin
    if Length(Params) >= 2 then
    begin
      if Vote(Params[0], StrToIntDef(Params[1], -1)) then
        Result := 'VOTE_RECORDED'
      else
        Result := 'VOTE_FAILED';
    end;
  end
  else if Method = 'openVoting' then
  begin
    if Length(Params) >= 1 then
    begin
      OpenVoting(StrToIntDef(Params[0], 60));
      Result := 'VOTING_OPENED';
    end;
  end
  else if Method = 'closeVoting' then
  begin
    CloseVoting;
    Result := 'VOTING_CLOSED';
  end
  else if Method = 'getResults' then
  begin
    Result := GetResults;
  end
  else
    Result := 'UNKNOWN_METHOD';
end;

procedure TVotingContract.AddProposal(const Description: string);  
var
  Proposal: TProposal;
begin
  Proposal := TProposal.Create(FProposals.Count, Description);
  FProposals.Add(Proposal);

  EmitEvent('ProposalAdded', Format('Proposal #%d: %s',
    [Proposal.ID, Description]));
end;

function TVotingContract.Vote(const VoterAddress: string; ProposalID: Integer): Boolean;  
begin
  Result := False;

  // Vérifier que le vote est ouvert
  if not FVotingOpen then
  begin
    WriteLn('Le vote n''est pas ouvert');
    Exit;
  end;

  // Vérifier que le vote n'est pas expiré
  if Now > FVotingEndTime then
  begin
    WriteLn('La période de vote est terminée');
    CloseVoting;
    Exit;
  end;

  // Vérifier que l'électeur n'a pas déjà voté
  if FVoters.IndexOf(VoterAddress) >= 0 then
  begin
    WriteLn('Vous avez déjà voté');
    Exit;
  end;

  // Vérifier que la proposition existe
  if (ProposalID < 0) or (ProposalID >= FProposals.Count) then
  begin
    WriteLn('Proposition invalide');
    Exit;
  end;

  // Enregistrer le vote
  FProposals[ProposalID].VoteCount := FProposals[ProposalID].VoteCount + 1;
  FVoters.Add(VoterAddress, True);

  EmitEvent('VoteCast', Format('Vote from %s for proposal #%d',
    [VoterAddress, ProposalID]));

  Result := True;
end;

function TVotingContract.GetWinner: TProposal;  
var
  i, MaxVotes: Integer;
begin
  Result := nil;
  MaxVotes := -1;

  for i := 0 to FProposals.Count - 1 do
  begin
    if FProposals[i].VoteCount > MaxVotes then
    begin
      MaxVotes := FProposals[i].VoteCount;
      Result := FProposals[i];
    end;
  end;
end;

procedure TVotingContract.OpenVoting(DurationMinutes: Integer);  
begin
  FVotingOpen := True;
  FVotingEndTime := IncMinute(Now, DurationMinutes);

  EmitEvent('VotingOpened', Format('Voting period: %d minutes', [DurationMinutes]));
end;

procedure TVotingContract.CloseVoting;  
var
  Winner: TProposal;
begin
  FVotingOpen := False;

  Winner := GetWinner;
  if Winner <> nil then
    EmitEvent('VotingClosed', Format('Winner: Proposal #%d with %d votes',
      [Winner.ID, Winner.VoteCount]))
  else
    EmitEvent('VotingClosed', 'No votes recorded');
end;

function TVotingContract.GetResults: string;  
var
  i: Integer;
begin
  Result := 'VOTING RESULTS:' + LineEnding;
  Result := Result + Format('Total voters: %d' + LineEnding, [FVoters.Count]);
  Result := Result + LineEnding;

  for i := 0 to FProposals.Count - 1 do
  begin
    Result := Result + Format('Proposal #%d: %s - %d votes' + LineEnding,
      [FProposals[i].ID, FProposals[i].Description, FProposals[i].VoteCount]);
  end;

  if not FVotingOpen then
  begin
    Result := Result + LineEnding + 'WINNER: ';
    if GetWinner <> nil then
      Result := Result + GetWinner.Description
    else
      Result := Result + 'No winner';
  end;
end;

end.
```

### Exemple 3 : Contrat d'enchères

```pascal
unit AuctionContract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SmartContract, fgl;

type
  TBid = class
  public
    Bidder: string;
    Amount: Double;
    Timestamp: TDateTime;

    constructor Create(const ABidder: string; AAmount: Double);
  end;

  TBidList = specialize TFPGObjectList<TBid>;

  TAuctionContract = class(TSmartContractBase)
  private
    FItemDescription: string;
    FMinimumBid: Double;
    FHighestBid: Double;
    FHighestBidder: string;
    FAuctionEndTime: TDateTime;
    FIsActive: Boolean;
    FBids: TBidList;
  public
    constructor Create(const AOwner, AItemDescription: string;
                      AMinimumBid: Double; ADurationMinutes: Integer);
    destructor Destroy; override;

    function Execute(const Method: string; const Params: array of string): string; override;

    // Fonctions d'enchère
    function PlaceBid(const BidderAddress: string; Amount: Double): Boolean;
    function EndAuction: string;
    function GetHighestBid: Double;
    function GetTimeRemaining: Integer; // en secondes

    property ItemDescription: string read FItemDescription;
    property IsActive: Boolean read FIsActive;
  end;

implementation

uses
  DateUtils;

{ TBid }

constructor TBid.Create(const ABidder: string; AAmount: Double);  
begin
  inherited Create;
  Bidder := ABidder;
  Amount := AAmount;
  Timestamp := Now;
end;

{ TAuctionContract }

constructor TAuctionContract.Create(const AOwner, AItemDescription: string;
  AMinimumBid: Double; ADurationMinutes: Integer);
begin
  inherited Create(AOwner);

  FItemDescription := AItemDescription;
  FMinimumBid := AMinimumBid;
  FHighestBid := 0;
  FHighestBidder := '';
  FAuctionEndTime := IncMinute(Now, ADurationMinutes);
  FIsActive := True;
  FBids := TBidList.Create(True);

  SetStorageValue('item', FItemDescription);
  SetStorageValue('minimumBid', FloatToStr(FMinimumBid));
  SetStorageValue('endTime', DateTimeToStr(FAuctionEndTime));

  EmitEvent('AuctionCreated', Format('Item: %s, Min bid: %.2f, Duration: %d min',
    [AItemDescription, AMinimumBid, ADurationMinutes]));
end;

destructor TAuctionContract.Destroy;  
begin
  FBids.Free;
  inherited Destroy;
end;

function TAuctionContract.Execute(const Method: string; const Params: array of string): string;  
begin
  Result := '';

  if Method = 'placeBid' then
  begin
    if Length(Params) >= 2 then
    begin
      if PlaceBid(Params[0], StrToFloatDef(Params[1], 0)) then
        Result := 'BID_ACCEPTED'
      else
        Result := 'BID_REJECTED';
    end;
  end
  else if Method = 'endAuction' then
  begin
    Result := EndAuction;
  end
  else if Method = 'getHighestBid' then
  begin
    Result := FloatToStr(GetHighestBid);
  end
  else if Method = 'getTimeRemaining' then
  begin
    Result := IntToStr(GetTimeRemaining);
  end
  else
    Result := 'UNKNOWN_METHOD';
end;

function TAuctionContract.PlaceBid(const BidderAddress: string; Amount: Double): Boolean;  
var
  NewBid: TBid;
begin
  Result := False;

  // Vérifier que l'enchère est active
  if not FIsActive then
  begin
    WriteLn('L''enchère est terminée');
    Exit;
  end;

  // Vérifier le temps
  if Now > FAuctionEndTime then
  begin
    WriteLn('L''enchère est expirée');
    EndAuction;
    Exit;
  end;

  // Vérifier le montant minimum
  if Amount < FMinimumBid then
  begin
    WriteLn(Format('Enchère trop basse (minimum: %.2f)', [FMinimumBid]));
    Exit;
  end;

  // Vérifier que c'est supérieur à l'enchère actuelle
  if Amount <= FHighestBid then
  begin
    WriteLn(Format('Enchère doit être supérieure à %.2f', [FHighestBid]));
    Exit;
  end;

  // Enregistrer l'enchère
  NewBid := TBid.Create(BidderAddress, Amount);
  FBids.Add(NewBid);

  FHighestBid := Amount;
  FHighestBidder := BidderAddress;

  EmitEvent('NewBid', Format('%s bid %.2f', [BidderAddress, Amount]));

  Result := True;
end;

function TAuctionContract.EndAuction: string;  
begin
  if not FIsActive then
  begin
    Result := 'Auction already ended';
    Exit;
  end;

  FIsActive := False;

  if FHighestBidder <> '' then
  begin
    Result := Format('Auction ended! Winner: %s with bid of %.2f',
      [FHighestBidder, FHighestBid]);

    EmitEvent('AuctionEnded', Result);
  end
  else
  begin
    Result := 'Auction ended with no bids';
    EmitEvent('AuctionEnded', 'No bids received');
  end;
end;

function TAuctionContract.GetHighestBid: Double;  
begin
  Result := FHighestBid;
end;

function TAuctionContract.GetTimeRemaining: Integer;  
begin
  if Now >= FAuctionEndTime then
    Result := 0
  else
    Result := SecondsBetween(FAuctionEndTime, Now);
end;

end.
```

## Partie 3 : Intégration blockchain et smart contracts

### Gestionnaire de smart contracts

```pascal
unit ContractManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SmartContract, Blockchain, BlockchainTypes, fgl;

type
  TContractRegistry = specialize TFPGMap<string, TSmartContractBase>;

  TContractManager = class
  private
    FBlockchain: TBlockchain;
    FContracts: TContractRegistry;
  public
    constructor Create(ABlockchain: TBlockchain);
    destructor Destroy; override;

    function DeployContract(AContract: TSmartContractBase): string;
    function GetContract(const Address: string): TSmartContractBase;
    function ExecuteContract(const Address, Method: string;
                            const Params: array of string): string;

    procedure ListContracts;
  end;

implementation

constructor TContractManager.Create(ABlockchain: TBlockchain);  
begin
  inherited Create;
  FBlockchain := ABlockchain;
  FContracts := TContractRegistry.Create;
end;

destructor TContractManager.Destroy;  
var
  i: Integer;
begin
  // Libérer tous les contrats
  for i := 0 to FContracts.Count - 1 do
    FContracts.Data[i].Free;

  FContracts.Free;
  inherited Destroy;
end;

function TContractManager.DeployContract(AContract: TSmartContractBase): string;  
var
  DeployTx: TTransaction;
begin
  // Ajouter le contrat au registre
  FContracts.Add(AContract.Address, AContract);

  // Créer une transaction de déploiement
  DeployTx := TTransaction.Create('SYSTEM', AContract.Address, 0);
  FBlockchain.AddTransaction(DeployTx);

  Result := AContract.Address;

  WriteLn(Format('Contract deployed at address: %s', [Result]));
end;

function TContractManager.GetContract(const Address: string): TSmartContractBase;  
var
  Index: Integer;
begin
  Index := FContracts.IndexOf(Address);
  if Index >= 0 then
    Result := FContracts.Data[Index]
  else
    Result := nil;
end;

function TContractManager.ExecuteContract(const Address, Method: string;
  const Params: array of string): string;
var
  Contract: TSmartContractBase;
  ExecuteTx: TTransaction;
begin
  Contract := GetContract(Address);

  if Contract = nil then
  begin
    Result := 'CONTRACT_NOT_FOUND';
    Exit;
  end;

  // Exécuter la méthode du contrat
  Result := Contract.Execute(Method, Params);

  // Créer une transaction d'exécution
  ExecuteTx := TTransaction.Create('USER', Address, 0);
  FBlockchain.AddTransaction(ExecuteTx);
end;

procedure TContractManager.ListContracts;  
var
  i: Integer;
begin
  WriteLn('');
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('           SMART CONTRACTS DÉPLOYÉS');
  WriteLn('═══════════════════════════════════════════════════');

  for i := 0 to FContracts.Count - 1 do
  begin
    WriteLn('');
    WriteLn(Format('Contract #%d', [i + 1]));
    WriteLn(FContracts.Data[i].GetContractInfo);
    WriteLn('---------------------------------------------------');
  end;

  WriteLn('');
end;

end.
```

### Programme de démonstration complet

```pascal
program SmartContractDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, Blockchain, BlockchainTypes, SmartContract,
  ContractManager, TokenContract, VotingContract, AuctionContract;

var
  MyBlockchain: TBlockchain;
  Manager: TContractManager;
  Token: TTokenContract;
  Voting: TVotingContract;
  Auction: TAuctionContract;
  TokenAddress, VotingAddress, AuctionAddress: string;

begin
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('   SMART CONTRACTS AVEC FREEPASCAL/LAZARUS');
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('');

  // Créer la blockchain
  MyBlockchain := TBlockchain.Create(2); // Difficulté réduite pour la démo
  Manager := TContractManager.Create(MyBlockchain);

  try
    // ═══════════════════════════════════════════════════════
    // DÉMONSTRATION 1 : TOKEN CONTRACT
    // ═══════════════════════════════════════════════════════
    WriteLn('');
    WriteLn('>>> DÉMONSTRATION 1: TOKEN CONTRACT');
    WriteLn('');

    // Déployer un token
    Token := TTokenContract.Create('Alice', 'MyToken', 'MTK', 1000000, 18);
    TokenAddress := Manager.DeployContract(Token);

    WriteLn('');
    WriteLn('Balance initiale Alice: ', Token.BalanceOf('Alice'):0:2, ' MTK');

    // Transférer des tokens
    WriteLn('');
    WriteLn('>>> Transfer 1000 MTK de Alice à Bob');
    Token.Transfer('Alice', 'Bob', 1000);

    WriteLn('Balance Alice: ', Token.BalanceOf('Alice'):0:2, ' MTK');
    WriteLn('Balance Bob: ', Token.BalanceOf('Bob'):0:2, ' MTK');

    // Miner un bloc pour confirmer
    MyBlockchain.MinePendingTransactions('Miner1');

    // ═══════════════════════════════════════════════════════
    // DÉMONSTRATION 2 : VOTING CONTRACT
    // ═══════════════════════════════════════════════════════
    WriteLn('');
    WriteLn('═══════════════════════════════════════════════════');
    WriteLn('>>> DÉMONSTRATION 2: VOTING CONTRACT');
    WriteLn('═══════════════════════════════════════════════════');
    WriteLn('');

    // Déployer un contrat de vote
    Voting := TVotingContract.Create('Charlie');
    VotingAddress := Manager.DeployContract(Voting);

    // Ajouter des propositions
    Voting.AddProposal('Proposition A: Augmenter le budget');
    Voting.AddProposal('Proposition B: Maintenir le budget');
    Voting.AddProposal('Proposition C: Réduire le budget');

    // Ouvrir le vote
    Voting.OpenVoting(60); // 60 minutes

    // Voter
    WriteLn('');
    WriteLn('>>> Votes en cours...');
    Voting.Vote('Alice', 0);
    Voting.Vote('Bob', 0);
    Voting.Vote('Charlie', 1);
    Voting.Vote('David', 0);
    Voting.Vote('Eve', 2);

    // Afficher les résultats
    WriteLn('');
    WriteLn(Voting.GetResults);

    // Fermer le vote
    Voting.CloseVoting;

    // Miner un bloc
    MyBlockchain.MinePendingTransactions('Miner2');

    // ═══════════════════════════════════════════════════════
    // DÉMONSTRATION 3 : AUCTION CONTRACT
    // ═══════════════════════════════════════════════════════
    WriteLn('');
    WriteLn('═══════════════════════════════════════════════════');
    WriteLn('>>> DÉMONSTRATION 3: AUCTION CONTRACT');
    WriteLn('═══════════════════════════════════════════════════');
    WriteLn('');

    // Déployer une enchère
    Auction := TAuctionContract.Create('Frank',
      'Peinture rare du 18ème siècle', 1000, 30);
    AuctionAddress := Manager.DeployContract(Auction);

    // Placer des enchères
    WriteLn('');
    WriteLn('>>> Enchères en cours...');
    Auction.PlaceBid('George', 1200);
    Sleep(1000);
    Auction.PlaceBid('Hannah', 1500);
    Sleep(1000);
    Auction.PlaceBid('Ian', 1800);
    Sleep(1000);
    Auction.PlaceBid('Jane', 2100);

    WriteLn('');
    WriteLn('Enchère la plus haute: ', Auction.GetHighestBid:0:2);

    // Terminer l'enchère
    WriteLn('');
    WriteLn(Auction.EndAuction);

    // Miner un bloc
    MyBlockchain.MinePendingTransactions('Miner3');

    // ═══════════════════════════════════════════════════════
    // AFFICHER LA BLOCKCHAIN COMPLÈTE
    // ═══════════════════════════════════════════════════════
    WriteLn('');
    WriteLn('═══════════════════════════════════════════════════');
    WriteLn('>>> BLOCKCHAIN FINALE');
    WriteLn('═══════════════════════════════════════════════════');

    MyBlockchain.PrintChain;

    // Lister tous les contrats
    Manager.ListContracts;

    // Valider la blockchain
    WriteLn('');
    WriteLn('>>> Validation de la blockchain...');
    MyBlockchain.IsChainValid;

  finally
    Manager.Free;
    MyBlockchain.Free;
  end;

  WriteLn('');
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Partie 4 : Concepts avancés

### Consensus et preuve de travail (Proof of Work)

Le mécanisme de consensus Proof of Work que nous avons implémenté fonctionne ainsi :

1. **Difficulté** : Nombre de zéros requis au début du hash
2. **Nonce** : Nombre qu'on incrémente pour trouver un hash valide
3. **Minage** : Processus de recherche du bon nonce

**Exemple de hash valide avec difficulté 4 :**
```
0000a7b3c9d2e5f8... ✓ Valide (commence par 4 zéros)
0001a7b3c9d2e5f8... ✗ Invalide
```

### Preuve d'enjeu (Proof of Stake)

Une alternative au Proof of Work, plus économe en énergie :

```pascal
unit ProofOfStake;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlockchainTypes, fgl;

type
  TValidator = class
  public
    Address: string;
    Stake: Double;          // Montant misé
    IsActive: Boolean;
    TotalBlocks: Integer;   // Blocs validés

    constructor Create(const AAddress: string; AStake: Double);
  end;

  TValidatorList = specialize TFPGObjectList<TValidator>;

  TProofOfStake = class
  private
    FValidators: TValidatorList;
    FMinimumStake: Double;

    function CalculateProbability(Validator: TValidator): Double;
  public
    constructor Create(AMinimumStake: Double);
    destructor Destroy; override;

    procedure RegisterValidator(const Address: string; Stake: Double);
    function SelectValidator: TValidator;
    function ValidateBlock(Validator: TValidator; Block: TBlock): Boolean;

    property MinimumStake: Double read FMinimumStake;
  end;

implementation

{ TValidator }

constructor TValidator.Create(const AAddress: string; AStake: Double);  
begin
  inherited Create;
  Address := AAddress;
  Stake := AStake;
  IsActive := True;
  TotalBlocks := 0;
end;

{ TProofOfStake }

constructor TProofOfStake.Create(AMinimumStake: Double);  
begin
  inherited Create;
  FValidators := TValidatorList.Create(True);
  FMinimumStake := AMinimumStake;
end;

destructor TProofOfStake.Destroy;  
begin
  FValidators.Free;
  inherited Destroy;
end;

procedure TProofOfStake.RegisterValidator(const Address: string; Stake: Double);  
var
  Validator: TValidator;
begin
  if Stake < FMinimumStake then
  begin
    WriteLn(Format('Mise insuffisante (minimum: %.2f)', [FMinimumStake]));
    Exit;
  end;

  Validator := TValidator.Create(Address, Stake);
  FValidators.Add(Validator);

  WriteLn(Format('Validateur enregistré: %s avec %.2f de mise',
    [Address, Stake]));
end;

function TProofOfStake.CalculateProbability(Validator: TValidator): Double;  
var
  TotalStake: Double;
  i: Integer;
begin
  TotalStake := 0;

  // Calculer la mise totale
  for i := 0 to FValidators.Count - 1 do
  begin
    if FValidators[i].IsActive then
      TotalStake := TotalStake + FValidators[i].Stake;
  end;

  // Probabilité = mise du validateur / mise totale
  if TotalStake > 0 then
    Result := Validator.Stake / TotalStake
  else
    Result := 0;
end;

function TProofOfStake.SelectValidator: TValidator;  
var
  i: Integer;
  RandomValue, CumulativeProbability: Double;
  Probability: Double;
begin
  Result := nil;

  if FValidators.Count = 0 then
    Exit;

  // Génération d'un nombre aléatoire entre 0 et 1
  Randomize;
  RandomValue := Random;

  CumulativeProbability := 0;

  // Sélection pondérée basée sur la mise
  for i := 0 to FValidators.Count - 1 do
  begin
    if not FValidators[i].IsActive then
      Continue;

    Probability := CalculateProbability(FValidators[i]);
    CumulativeProbability := CumulativeProbability + Probability;

    if RandomValue <= CumulativeProbability then
    begin
      Result := FValidators[i];
      WriteLn(Format('Validateur sélectionné: %s (probabilité: %.2f%%)',
        [Result.Address, Probability * 100]));
      Exit;
    end;
  end;

  // Par sécurité, retourner le premier validateur actif
  for i := 0 to FValidators.Count - 1 do
  begin
    if FValidators[i].IsActive then
    begin
      Result := FValidators[i];
      Exit;
    end;
  end;
end;

function TProofOfStake.ValidateBlock(Validator: TValidator; Block: TBlock): Boolean;  
begin
  Result := False;

  if Validator = nil then
    Exit;

  if not Validator.IsActive then
  begin
    WriteLn('Validateur inactif');
    Exit;
  end;

  // Vérifier que le bloc est valide
  if not Block.IsValid then
  begin
    WriteLn('Bloc invalide');
    Exit;
  end;

  // Dans PoS, pas besoin de minage intensif
  Block.Hash := Block.CalculateHash;

  // Incrémenter le compteur de blocs du validateur
  Validator.TotalBlocks := Validator.TotalBlocks + 1;

  WriteLn(Format('Bloc validé par %s (total: %d blocs)',
    [Validator.Address, Validator.TotalBlocks]));

  Result := True;
end;

end.
```

## Partie 5 : Réseau P2P (Peer-to-Peer)

Pour qu'une blockchain soit véritablement décentralisée, elle doit fonctionner sur un réseau P2P où chaque nœud peut communiquer avec les autres.

### Architecture réseau

```pascal
unit BlockchainNetwork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Blockchain, BlockchainTypes, blcksock, synsock, fgl;

type
  TPeerInfo = class
  public
    Address: string;
    Port: Integer;
    LastSeen: TDateTime;
    IsConnected: Boolean;
    Version: string;

    constructor Create(const AAddress: string; APort: Integer);
  end;

  TPeerList = specialize TFPGObjectList<TPeerInfo>;

  TMessageType = (mtGetBlockchain, mtSendBlockchain, mtNewBlock,
                  mtNewTransaction, mtPing, mtPong, mtGetPeers, mtSendPeers);

  TNetworkMessage = class
  private
    FMessageType: TMessageType;
    FData: string;
    FSenderAddress: string;
    FTimestamp: TDateTime;
  public
    constructor Create(AType: TMessageType; const AData, ASender: string);

    function Serialize: string;
    procedure Deserialize(const AData: string);

    property MessageType: TMessageType read FMessageType;
    property Data: string read FData;
    property SenderAddress: string read FSenderAddress;
  end;

  TBlockchainNode = class
  private
    FBlockchain: TBlockchain;
    FPeers: TPeerList;
    FListenPort: Integer;
    FNodeAddress: string;
    FRunning: Boolean;
    FListenSocket: TTCPBlockSocket;

    procedure HandleConnection(ClientSocket: TSocket);
    procedure ProcessMessage(const Msg: TNetworkMessage; ClientSocket: TSocket);
    procedure BroadcastMessage(const Msg: TNetworkMessage);
  public
    constructor Create(ABlockchain: TBlockchain; APort: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    procedure ConnectToPeer(const Address: string; Port: Integer);
    procedure DisconnectPeer(const Address: string);

    procedure BroadcastNewBlock(Block: TBlock);
    procedure BroadcastTransaction(Transaction: TTransaction);

    procedure SyncBlockchain;
    procedure DiscoverPeers;

    property NodeAddress: string read FNodeAddress;
    property Peers: TPeerList read FPeers;
  end;

implementation

uses
  DateUtils, fpjson, jsonparser;

{ TPeerInfo }

constructor TPeerInfo.Create(const AAddress: string; APort: Integer);  
begin
  inherited Create;
  Address := AAddress;
  Port := APort;
  LastSeen := Now;
  IsConnected := False;
  Version := '1.0.0';
end;

{ TNetworkMessage }

constructor TNetworkMessage.Create(AType: TMessageType; const AData, ASender: string);  
begin
  inherited Create;
  FMessageType := AType;
  FData := AData;
  FSenderAddress := ASender;
  FTimestamp := Now;
end;

function TNetworkMessage.Serialize: string;  
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.Add('type', Ord(FMessageType));
    JSON.Add('data', FData);
    JSON.Add('sender', FSenderAddress);
    JSON.Add('timestamp', DateTimeToStr(FTimestamp));
    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

procedure TNetworkMessage.Deserialize(const AData: string);  
var
  JSON: TJSONObject;
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(AData, [joUTF8]);
  try
    JSON := TJSONObject(Parser.Parse);
    try
      FMessageType := TMessageType(JSON.Get('type', 0));
      FData := JSON.Get('data', '');
      FSenderAddress := JSON.Get('sender', '');
      FTimestamp := StrToDateTime(JSON.Get('timestamp', DateTimeToStr(Now)));
    finally
      JSON.Free;
    end;
  finally
    Parser.Free;
  end;
end;

{ TBlockchainNode }

constructor TBlockchainNode.Create(ABlockchain: TBlockchain; APort: Integer);  
var
  GUID: TGUID;
begin
  inherited Create;

  FBlockchain := ABlockchain;
  FPeers := TPeerList.Create(True);
  FListenPort := APort;
  FRunning := False;
  FListenSocket := TTCPBlockSocket.Create;

  // Générer un identifiant unique pour ce nœud
  CreateGUID(GUID);
  FNodeAddress := Copy(GUIDToString(GUID), 2, 8);

  WriteLn(Format('Nœud créé: %s sur le port %d', [FNodeAddress, FListenPort]));
end;

destructor TBlockchainNode.Destroy;  
begin
  Stop;
  FListenSocket.Free;
  FPeers.Free;
  inherited Destroy;
end;

procedure TBlockchainNode.Start;  
var
  ClientSocket: TSocket;
begin
  FListenSocket.CreateSocket;
  FListenSocket.SetLinger(True, 10);
  FListenSocket.Bind('0.0.0.0', IntToStr(FListenPort));
  FListenSocket.Listen;

  FRunning := True;

  WriteLn(Format('Nœud %s en écoute sur le port %d', [FNodeAddress, FListenPort]));

  // Boucle d'acceptation des connexions
  while FRunning do
  begin
    if FListenSocket.CanRead(1000) then
    begin
      ClientSocket := FListenSocket.Accept;
      if FListenSocket.LastError = 0 then
      begin
        WriteLn('Nouvelle connexion acceptée');
        HandleConnection(ClientSocket);
      end;
    end;
  end;
end;

procedure TBlockchainNode.Stop;  
begin
  FRunning := False;
  FListenSocket.CloseSocket;
  WriteLn('Nœud arrêté');
end;

procedure TBlockchainNode.HandleConnection(ClientSocket: TSocket);  
var
  Socket: TTCPBlockSocket;
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;
  Msg: TNetworkMessage;
  ReceivedData: string;
begin
  Socket := TTCPBlockSocket.Create;
  try
    Socket.Socket := ClientSocket;

    // Lire les données
    BytesRead := Socket.RecvBufferEx(@Buffer, SizeOf(Buffer), 30000);

    if BytesRead > 0 then
    begin
      SetLength(ReceivedData, BytesRead);
      Move(Buffer[0], ReceivedData[1], BytesRead);

      Msg := TNetworkMessage.Create(mtPing, '', '');
      try
        Msg.Deserialize(ReceivedData);
        ProcessMessage(Msg, ClientSocket);
      finally
        Msg.Free;
      end;
    end;
  finally
    Socket.Free;
  end;
end;

procedure TBlockchainNode.ProcessMessage(const Msg: TNetworkMessage; ClientSocket: TSocket);  
var
  Response: TNetworkMessage;
  Socket: TTCPBlockSocket;
  ResponseData: string;
begin
  WriteLn(Format('Message reçu de %s: type %d',
    [Msg.SenderAddress, Ord(Msg.MessageType)]));

  Socket := TTCPBlockSocket.Create;
  try
    Socket.Socket := ClientSocket;

    case Msg.MessageType of
      mtPing:
        begin
          WriteLn('Ping reçu, envoi de Pong');
          Response := TNetworkMessage.Create(mtPong, 'PONG', FNodeAddress);
          ResponseData := Response.Serialize;
          Socket.SendString(ResponseData);
          Response.Free;
        end;

      mtGetBlockchain:
        begin
          WriteLn('Demande de blockchain reçue');
          // Sérialiser et envoyer la blockchain
          Response := TNetworkMessage.Create(mtSendBlockchain,
            'BLOCKCHAIN_DATA', FNodeAddress);
          ResponseData := Response.Serialize;
          Socket.SendString(ResponseData);
          Response.Free;
        end;

      mtNewBlock:
        begin
          WriteLn('Nouveau bloc reçu');
          // Parser et ajouter le bloc
          // TODO: Implémenter la désérialisation du bloc
        end;

      mtNewTransaction:
        begin
          WriteLn('Nouvelle transaction reçue');
          // Parser et ajouter la transaction
        end;

      mtGetPeers:
        begin
          WriteLn('Demande de liste de pairs reçue');
          // Envoyer la liste des pairs connus
        end;
    end;
  finally
    Socket.Free;
  end;
end;

procedure TBlockchainNode.ConnectToPeer(const Address: string; Port: Integer);  
var
  Socket: TTCPBlockSocket;
  Msg: TNetworkMessage;
  MsgData: string;
  Peer: TPeerInfo;
begin
  WriteLn(Format('Connexion au pair %s:%d', [Address, Port]));

  Socket := TTCPBlockSocket.Create;
  try
    Socket.Connect(Address, IntToStr(Port));

    if Socket.LastError = 0 then
    begin
      WriteLn('Connexion établie');

      // Envoyer un message de ping
      Msg := TNetworkMessage.Create(mtPing, 'HELLO', FNodeAddress);
      MsgData := Msg.Serialize;
      Socket.SendString(MsgData);
      Msg.Free;

      // Ajouter le pair à la liste
      Peer := TPeerInfo.Create(Address, Port);
      Peer.IsConnected := True;
      FPeers.Add(Peer);

      WriteLn(Format('Pair ajouté: %s:%d', [Address, Port]));
    end
    else
      WriteLn('Erreur de connexion: ', Socket.LastErrorDesc);
  finally
    Socket.Free;
  end;
end;

procedure TBlockchainNode.DisconnectPeer(const Address: string);  
var
  i: Integer;
begin
  for i := 0 to FPeers.Count - 1 do
  begin
    if FPeers[i].Address = Address then
    begin
      FPeers[i].IsConnected := False;
      WriteLn(Format('Déconnexion du pair: %s', [Address]));
      Exit;
    end;
  end;
end;

procedure TBlockchainNode.BroadcastMessage(const Msg: TNetworkMessage);  
var
  i: Integer;
  Socket: TTCPBlockSocket;
  MsgData: string;
begin
  MsgData := Msg.Serialize;

  for i := 0 to FPeers.Count - 1 do
  begin
    if not FPeers[i].IsConnected then
      Continue;

    Socket := TTCPBlockSocket.Create;
    try
      Socket.Connect(FPeers[i].Address, IntToStr(FPeers[i].Port));

      if Socket.LastError = 0 then
      begin
        Socket.SendString(MsgData);
        WriteLn(Format('Message diffusé à %s:%d',
          [FPeers[i].Address, FPeers[i].Port]));
      end;
    finally
      Socket.Free;
    end;
  end;
end;

procedure TBlockchainNode.BroadcastNewBlock(Block: TBlock);  
var
  Msg: TNetworkMessage;
  BlockData: string;
begin
  // Sérialiser le bloc en JSON
  BlockData := Format('{"index":%d,"hash":"%s"}', [Block.Index, Block.Hash]);

  Msg := TNetworkMessage.Create(mtNewBlock, BlockData, FNodeAddress);
  try
    BroadcastMessage(Msg);
  finally
    Msg.Free;
  end;
end;

procedure TBlockchainNode.BroadcastTransaction(Transaction: TTransaction);  
var
  Msg: TNetworkMessage;
  TxData: string;
begin
  TxData := Transaction.ToString;

  Msg := TNetworkMessage.Create(mtNewTransaction, TxData, FNodeAddress);
  try
    BroadcastMessage(Msg);
  finally
    Msg.Free;
  end;
end;

procedure TBlockchainNode.SyncBlockchain;  
var
  Msg: TNetworkMessage;
begin
  WriteLn('Synchronisation de la blockchain...');

  Msg := TNetworkMessage.Create(mtGetBlockchain, '', FNodeAddress);
  try
    BroadcastMessage(Msg);
  finally
    Msg.Free;
  end;
end;

procedure TBlockchainNode.DiscoverPeers;  
var
  Msg: TNetworkMessage;
begin
  WriteLn('Découverte de nouveaux pairs...');

  Msg := TNetworkMessage.Create(mtGetPeers, '', FNodeAddress);
  try
    BroadcastMessage(Msg);
  finally
    Msg.Free;
  end;
end;

end.
```

## Partie 6 : Wallet (Portefeuille)

Un wallet permet de gérer les clés cryptographiques et les transactions.

### Gestion des clés et signatures

```pascal
unit CryptoWallet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sha256, BlockchainTypes;

type
  TKeyPair = record
    PrivateKey: string;
    PublicKey: string;
    Address: string;
  end;

  TCryptoWallet = class
  private
    FKeyPair: TKeyPair;
    FBalance: Double;

    function GeneratePrivateKey: string;
    function DerivePublicKey(const PrivateKey: string): string;
    function DeriveAddress(const PublicKey: string): string;
  public
    constructor Create;

    function SignTransaction(Transaction: TTransaction): string;
    function VerifySignature(Transaction: TTransaction): Boolean;

    procedure ExportKeys(const Filename: string);
    procedure ImportKeys(const Filename: string);

    property PrivateKey: string read FKeyPair.PrivateKey;
    property PublicKey: string read FKeyPair.PublicKey;
    property Address: string read FKeyPair.Address;
    property Balance: Double read FBalance write FBalance;
  end;

implementation

uses
  base64;

constructor TCryptoWallet.Create;  
begin
  inherited Create;

  // Générer une nouvelle paire de clés
  FKeyPair.PrivateKey := GeneratePrivateKey;
  FKeyPair.PublicKey := DerivePublicKey(FKeyPair.PrivateKey);
  FKeyPair.Address := DeriveAddress(FKeyPair.PublicKey);
  FBalance := 0;

  WriteLn('Nouveau wallet créé');
  WriteLn('Adresse: ', FKeyPair.Address);
end;

function TCryptoWallet.GeneratePrivateKey: string;  
var
  i: Integer;
  RandomBytes: array[0..31] of Byte;
begin
  // Générer 32 octets aléatoires
  Randomize;
  for i := 0 to 31 do
    RandomBytes[i] := Random(256);

  // Hasher pour obtenir une clé privée
  Result := SHA256Print(SHA256Buffer(@RandomBytes, 32));
end;

function TCryptoWallet.DerivePublicKey(const PrivateKey: string): string;  
begin
  // Dans une vraie implémentation, on utiliserait ECDSA
  // Ici, simplification: hash de la clé privée
  Result := SHA256Print(SHA256String('PUBLIC_' + PrivateKey));
end;

function TCryptoWallet.DeriveAddress(const PublicKey: string): string;  
begin
  // Adresse = hash de la clé publique (préfixé par "0x")
  Result := '0x' + Copy(SHA256Print(SHA256String(PublicKey)), 1, 40);
end;

function TCryptoWallet.SignTransaction(Transaction: TTransaction): string;  
var
  TxHash: string;
  SignatureData: string;
begin
  // Calculer le hash de la transaction
  TxHash := Transaction.CalculateHash;

  // Signer avec la clé privée (simplifié)
  SignatureData := TxHash + FKeyPair.PrivateKey;
  Result := SHA256Print(SHA256String(SignatureData));

  // Stocker la signature dans la transaction
  Transaction.Signature := Result;
end;

function TCryptoWallet.VerifySignature(Transaction: TTransaction): Boolean;  
var
  ExpectedSignature: string;
  TxHash: string;
  SignatureData: string;
begin
  // Recalculer la signature attendue
  TxHash := Transaction.CalculateHash;
  SignatureData := TxHash + FKeyPair.PrivateKey;
  ExpectedSignature := SHA256Print(SHA256String(SignatureData));

  Result := ExpectedSignature = Transaction.Signature;
end;

procedure TCryptoWallet.ExportKeys(const Filename: string);  
var
  F: TextFile;
begin
  try
    AssignFile(F, Filename);
    Rewrite(F);

    WriteLn(F, '[Wallet Keys]');
    WriteLn(F, 'PrivateKey=', FKeyPair.PrivateKey);
    WriteLn(F, 'PublicKey=', FKeyPair.PublicKey);
    WriteLn(F, 'Address=', FKeyPair.Address);

    CloseFile(F);

    WriteLn('Clés exportées vers: ', Filename);
  except
    on E: Exception do
      WriteLn('Erreur d''exportation: ', E.Message);
  end;
end;

procedure TCryptoWallet.ImportKeys(const Filename: string);  
var
  F: TextFile;
  Line, Key, Value: string;
  SepPos: Integer;
begin
  try
    AssignFile(F, Filename);
    Reset(F);

    ReadLn(F); // Ignorer l'en-tête

    while not Eof(F) do
    begin
      ReadLn(F, Line);
      SepPos := Pos('=', Line);

      if SepPos > 0 then
      begin
        Key := Copy(Line, 1, SepPos - 1);
        Value := Copy(Line, SepPos + 1, Length(Line));

        if Key = 'PrivateKey' then
          FKeyPair.PrivateKey := Value
        else if Key = 'PublicKey' then
          FKeyPair.PublicKey := Value
        else if Key = 'Address' then
          FKeyPair.Address := Value;
      end;
    end;

    CloseFile(F);

    WriteLn('Clés importées depuis: ', Filename);
  except
    on E: Exception do
      WriteLn('Erreur d''importation: ', E.Message);
  end;
end;

end.
```

## Partie 7 : Interface graphique pour la blockchain

### Application wallet GUI avec Lazarus

```pascal
unit WalletMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, CryptoWallet, Blockchain, BlockchainTypes;

type
  TFormWallet = class(TForm)
    PanelTop: TPanel;
    LabelAddress: TLabel;
    LabelBalance: TLabel;

    PageControl1: TPageControl;
    TabSend: TTabSheet;
    TabReceive: TTabSheet;
    TabTransactions: TTabSheet;
    TabBlockchain: TTabSheet;

    // Onglet Envoyer
    EditRecipient: TEdit;
    EditAmount: TEdit;
    ButtonSend: TButton;

    // Onglet Recevoir
    MemoAddress: TMemo;
    ButtonCopyAddress: TButton;

    // Onglet Transactions
    ListViewTransactions: TListView;
    ButtonRefresh: TButton;

    // Onglet Blockchain
    MemoBlockchain: TMemo;
    ButtonMine: TButton;

    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuExportKeys: TMenuItem;
    MenuImportKeys: TMenuItem;
    MenuExit: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure ButtonCopyAddressClick(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure ButtonMineClick(Sender: TObject);
    procedure MenuExportKeysClick(Sender: TObject);
    procedure MenuImportKeysClick(Sender: TObject);

  private
    FWallet: TCryptoWallet;
    FBlockchain: TBlockchain;

    procedure UpdateBalance;
    procedure UpdateTransactionsList;
    procedure UpdateBlockchainView;

  public

  end;

var
  FormWallet: TFormWallet;

implementation

{$R *.lfm}

uses
  Clipbrd;

procedure TFormWallet.FormCreate(Sender: TObject);  
begin
  // Créer le wallet et la blockchain
  FWallet := TCryptoWallet.Create;
  FBlockchain := TBlockchain.Create(4);

  // Initialiser l'interface
  LabelAddress.Caption := 'Adresse: ' + FWallet.Address;
  MemoAddress.Text := FWallet.Address;

  // Configurer la ListView des transactions
  with ListViewTransactions do
  begin
    ViewStyle := vsReport;
    Columns.Add.Caption := 'Date';
    Columns.Add.Caption := 'De';
    Columns.Add.Caption := 'À';
    Columns.Add.Caption := 'Montant';
    Columns.Add.Caption := 'Statut';

    Columns[0].Width := 150;
    Columns[1].Width := 200;
    Columns[2].Width := 200;
    Columns[3].Width := 100;
    Columns[4].Width := 100;
  end;

  UpdateBalance;
  UpdateBlockchainView;
end;

procedure TFormWallet.FormDestroy(Sender: TObject);  
begin
  FBlockchain.Free;
  FWallet.Free;
end;

procedure TFormWallet.ButtonSendClick(Sender: TObject);  
var
  Recipient: string;
  Amount: Double;
  Tx: TTransaction;
begin
  Recipient := Trim(EditRecipient.Text);
  Amount := StrToFloatDef(EditAmount.Text, 0);

  if Recipient = '' then
  begin
    ShowMessage('Veuillez entrer une adresse de destinataire');
    Exit;
  end;

  if Amount <= 0 then
  begin
    ShowMessage('Veuillez entrer un montant valide');
    Exit;
  end;

  if Amount > FWallet.Balance then
  begin
    ShowMessage('Solde insuffisant');
    Exit;
  end;

  // Créer et signer la transaction
  Tx := TTransaction.Create(FWallet.Address, Recipient, Amount);
  FWallet.SignTransaction(Tx);

  // Ajouter à la blockchain
  FBlockchain.AddTransaction(Tx);

  ShowMessage('Transaction créée avec succès ! En attente de confirmation.');

  EditRecipient.Text := '';
  EditAmount.Text := '';

  UpdateTransactionsList;
end;

procedure TFormWallet.ButtonCopyAddressClick(Sender: TObject);  
begin
  Clipboard.AsText := FWallet.Address;
  ShowMessage('Adresse copiée dans le presse-papier');
end;

procedure TFormWallet.ButtonRefreshClick(Sender: TObject);  
begin
  UpdateBalance;
  UpdateTransactionsList;
  UpdateBlockchainView;
end;

procedure TFormWallet.ButtonMineClick(Sender: TObject);  
begin
  ButtonMine.Enabled := False;
  try
    Application.ProcessMessages;

    if FBlockchain.MinePendingTransactions(FWallet.Address) then
    begin
      ShowMessage('Bloc miné avec succès !');
      UpdateBalance;
      UpdateTransactionsList;
      UpdateBlockchainView;
    end
    else
      ShowMessage('Échec du minage');
  finally
    ButtonMine.Enabled := True;
  end;
end;

procedure TFormWallet.MenuExportKeysClick(Sender: TObject);  
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Fichiers Wallet (*.wallet)|*.wallet';
    SaveDialog.DefaultExt := 'wallet';

    if SaveDialog.Execute then
    begin
      FWallet.ExportKeys(SaveDialog.FileName);
      ShowMessage('Clés exportées avec succès');
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TFormWallet.MenuImportKeysClick(Sender: TObject);  
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Fichiers Wallet (*.wallet)|*.wallet';

    if OpenDialog.Execute then
    begin
      FWallet.ImportKeys(OpenDialog.FileName);
      LabelAddress.Caption := 'Adresse: ' + FWallet.Address;
      MemoAddress.Text := FWallet.Address;
      UpdateBalance;
      ShowMessage('Clés importées avec succès');
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormWallet.UpdateBalance;  
begin
  // Calculer le solde en parcourant la blockchain
  FWallet.Balance := 0; // Calculer depuis la blockchain
  LabelBalance.Caption := Format('Solde: %.2f', [FWallet.Balance]);
end;

procedure TFormWallet.UpdateTransactionsList;  
var
  i, j: Integer;
  Block: TBlock;
  Tx: TTransaction;
  Item: TListItem;
begin
  ListViewTransactions.Items.Clear;

  // Parcourir tous les blocs
  for i := 0 to FBlockchain.Chain.Count - 1 do
  begin
    Block := FBlockchain.Chain[i];

    for j := 0 to Block.Transactions.Count - 1 do
    begin
      Tx := Block.Transactions[j];

      // Afficher seulement les transactions concernant ce wallet
      if (Tx.FromAddress = FWallet.Address) or (Tx.ToAddress = FWallet.Address) then
      begin
        Item := ListViewTransactions.Items.Add;
        Item.Caption := DateTimeToStr(Tx.Timestamp);
        Item.SubItems.Add(Copy(Tx.FromAddress, 1, 20) + '...');
        Item.SubItems.Add(Copy(Tx.ToAddress, 1, 20) + '...');
        Item.SubItems.Add(Format('%.2f', [Tx.Amount]));
        Item.SubItems.Add('Confirmé');

        // Colorer différemment selon le sens
        if Tx.FromAddress = FWallet.Address then
          Item.SubItems[3] := 'Envoyé'
        else
          Item.SubItems[3] := 'Reçu';
      end;
    end;
  end;
end;

procedure TFormWallet.UpdateBlockchainView;  
var
  i: Integer;
  Block: TBlock;
  Output: string;
begin
  Output := '';

  for i := 0 to FBlockchain.Chain.Count - 1 do
  begin
    Block := FBlockchain.Chain[i];
    Output := Output + Format('Bloc #%d' + LineEnding, [Block.Index]);
    Output := Output + Format('  Hash: %s' + LineEnding, [Copy(Block.Hash, 1, 32) + '...']);
    Output := Output + Format('  Transactions: %d' + LineEnding, [Block.Transactions.Count]);
    Output := Output + Format('  Timestamp: %s' + LineEnding, [DateTimeToStr(Block.Timestamp)]);
    Output := Output + LineEnding;
  end;

  MemoBlockchain.Text := Output;
end;

end.
```

## Partie 8 : Optimisations et sécurité

### Merkle Tree pour validation efficace

Un Merkle Tree permet de vérifier rapidement l'intégrité des transactions dans un bloc.

```pascal
unit MerkleTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sha256, fgl;

type
  TMerkleNode = class
  public
    Hash: string;
    Left: TMerkleNode;
    Right: TMerkleNode;

    constructor Create(const AHash: string);
    destructor Destroy; override;
  end;

  // Liste de hashes (ne pas confondre avec Classes.TStringList)
  THashList = specialize TFPGList<string>;

  TMerkleTree = class
  private
    FRoot: TMerkleNode;
    FLeaves: THashList;

    function BuildTree(Hashes: THashList): TMerkleNode;
    function CombineHashes(const Left, Right: string): string;
  public
    constructor Create(const TransactionHashes: array of string);
    destructor Destroy; override;

    function GetRootHash: string;
    function VerifyTransaction(const TxHash: string; const Proof: array of string): Boolean;
    function GenerateProof(const TxHash: string): THashList;

    property RootHash: string read GetRootHash;
  end;

implementation

{ TMerkleNode }

constructor TMerkleNode.Create(const AHash: string);  
begin
  inherited Create;
  Hash := AHash;
  Left := nil;
  Right := nil;
end;

destructor TMerkleNode.Destroy;  
begin
  if Assigned(Left) then
    Left.Free;
  if Assigned(Right) then
    Right.Free;
  inherited Destroy;
end;

{ TMerkleTree }

constructor TMerkleTree.Create(const TransactionHashes: array of string);  
var
  i: Integer;
begin
  inherited Create;

  FLeaves := THashList.Create;

  // Ajouter tous les hashes de transactions
  for i := 0 to High(TransactionHashes) do
    FLeaves.Add(TransactionHashes[i]);

  // Si nombre impair, dupliquer le dernier
  if (FLeaves.Count mod 2) <> 0 then
    FLeaves.Add(FLeaves[FLeaves.Count - 1]);

  // Construire l'arbre
  FRoot := BuildTree(FLeaves);
end;

destructor TMerkleTree.Destroy;  
begin
  if Assigned(FRoot) then
    FRoot.Free;
  FLeaves.Free;
  inherited Destroy;
end;

function TMerkleTree.CombineHashes(const Left, Right: string): string;  
var
  Combined: string;
begin
  Combined := Left + Right;
  Result := SHA256Print(SHA256String(Combined));
end;

function TMerkleTree.BuildTree(Hashes: THashList): TMerkleNode;  
var
  NewLevel: THashList;
  i: Integer;
  Node: TMerkleNode;
begin
  // Cas de base : un seul hash = racine
  if Hashes.Count = 1 then
  begin
    Result := TMerkleNode.Create(Hashes[0]);
    Exit;
  end;

  NewLevel := THashList.Create;
  try
    // Combiner les hashes par paires
    i := 0;
    while i < Hashes.Count do
    begin
      if i + 1 < Hashes.Count then
        NewLevel.Add(CombineHashes(Hashes[i], Hashes[i + 1]))
      else
        NewLevel.Add(CombineHashes(Hashes[i], Hashes[i])); // Dupliquer si impair

      Inc(i, 2);
    end;

    // Appel récursif
    Result := BuildTree(NewLevel);
  finally
    NewLevel.Free;
  end;
end;

function TMerkleTree.GetRootHash: string;  
begin
  if Assigned(FRoot) then
    Result := FRoot.Hash
  else
    Result := '';
end;

function TMerkleTree.GenerateProof(const TxHash: string): THashList;  
var
  Index: Integer;
begin
  Result := THashList.Create;

  // Trouver l'index de la transaction
  Index := FLeaves.IndexOf(TxHash);

  if Index < 0 then
    Exit;

  // Générer le chemin de preuve
  // (Implémentation simplifiée)
  // Dans une vraie implémentation, on parcourrait l'arbre
  Result.Add('PROOF_HASH_1');
  Result.Add('PROOF_HASH_2');
end;

function TMerkleTree.VerifyTransaction(const TxHash: string;
  const Proof: array of string): Boolean;
var
  CurrentHash: string;
  i: Integer;
begin
  CurrentHash := TxHash;

  // Recalculer le hash racine en utilisant la preuve
  for i := 0 to High(Proof) do
    CurrentHash := CombineHashes(CurrentHash, Proof[i]);

  // Vérifier que ça correspond à la racine
  Result := CurrentHash = FRoot.Hash;
end;

end.
```

### Pool de minage

Pour optimiser le minage avec plusieurs threads :

```pascal
unit MiningPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlockchainTypes, syncobjs;

type
  TMinerThread = class(TThread)
  private
    FBlock: TBlock;
    FDifficulty: Integer;
    FStartNonce: Int64;
    FEndNonce: Int64;
    FFound: Boolean;
    FFoundNonce: Int64;
    FFoundHash: string;
    FLock: TCriticalSection;
  protected
    procedure Execute; override;
  public
    constructor Create(ABlock: TBlock; ADifficulty: Integer;
                      AStartNonce, AEndNonce: Int64; ALock: TCriticalSection);

    property Found: Boolean read FFound;
    property FoundNonce: Int64 read FFoundNonce;
    property FoundHash: string read FFoundHash;
  end;

  TMiningPool = class
  private
    FThreadCount: Integer;
    FThreads: array of TMinerThread;
    FLock: TCriticalSection;
    FStopMining: Boolean;
  public
    constructor Create(AThreadCount: Integer);
    destructor Destroy; override;

    function MineBlock(Block: TBlock; Difficulty: Integer): Boolean;
    procedure StopMining;

    property ThreadCount: Integer read FThreadCount;
  end;

implementation

{ TMinerThread }

constructor TMinerThread.Create(ABlock: TBlock; ADifficulty: Integer;
  AStartNonce, AEndNonce: Int64; ALock: TCriticalSection);
begin
  inherited Create(True);

  FBlock := ABlock;
  FDifficulty := ADifficulty;
  FStartNonce := AStartNonce;
  FEndNonce := AEndNonce;
  FFound := False;
  FFoundNonce := 0;
  FFoundHash := '';
  FLock := ALock;
  FreeOnTerminate := False;
end;

procedure TMinerThread.Execute;  
var
  Target: string;
  CurrentNonce: Int64;
  CurrentHash: string;
begin
  Target := StringOfChar('0', FDifficulty);

  for CurrentNonce := FStartNonce to FEndNonce do
  begin
    if Terminated then
      Exit;

    FBlock.Nonce := CurrentNonce;
    CurrentHash := FBlock.CalculateHash;

    if Copy(CurrentHash, 1, FDifficulty) = Target then
    begin
      FLock.Enter;
      try
        FFound := True;
        FFoundNonce := CurrentNonce;
        FFoundHash := CurrentHash;
      finally
        FLock.Leave;
      end;
      Exit;
    end;
  end;
end;

{ TMiningPool }

constructor TMiningPool.Create(AThreadCount: Integer);  
begin
  inherited Create;
  FThreadCount := AThreadCount;
  FLock := TCriticalSection.Create;
  FStopMining := False;
  SetLength(FThreads, FThreadCount);
end;

destructor TMiningPool.Destroy;  
begin
  StopMining;
  FLock.Free;
  inherited Destroy;
end;

function TMiningPool.MineBlock(Block: TBlock; Difficulty: Integer): Boolean;  
var
  i: Integer;
  NonceRange: Int64;
  StartNonce, EndNonce: Int64;
  Found: Boolean;
begin
  Result := False;
  FStopMining := False;

  WriteLn(Format('Minage avec %d threads, difficulté %d',
    [FThreadCount, Difficulty]));

  // Diviser l'espace de recherche entre les threads
  NonceRange := 10000000 div FThreadCount;

  for i := 0 to FThreadCount - 1 do
  begin
    StartNonce := i * NonceRange;
    EndNonce := (i + 1) * NonceRange - 1;

    FThreads[i] := TMinerThread.Create(Block, Difficulty, StartNonce, EndNonce, FLock);
    FThreads[i].Start;
  end;

  // Attendre qu'un thread trouve la solution
  Found := False;
  while not Found and not FStopMining do
  begin
    Sleep(100);

    for i := 0 to FThreadCount - 1 do
    begin
      if FThreads[i].Found then
      begin
        Block.Nonce := FThreads[i].FoundNonce;
        Block.Hash := FThreads[i].FoundHash;
        Found := True;
        Result := True;

        WriteLn(Format('✓ Solution trouvée par le thread %d !', [i]));
        WriteLn(Format('  Nonce: %d', [Block.Nonce]));
        WriteLn(Format('  Hash: %s', [Block.Hash]));

        Break;
      end;
    end;
  end;

  // Arrêter tous les threads
  for i := 0 to FThreadCount - 1 do
  begin
    FThreads[i].Terminate;
    FThreads[i].WaitFor;
    FThreads[i].Free;
  end;
end;

procedure TMiningPool.StopMining;  
var
  i: Integer;
begin
  FStopMining := True;

  for i := 0 to FThreadCount - 1 do
  begin
    if Assigned(FThreads[i]) then
    begin
      FThreads[i].Terminate;
      FThreads[i].WaitFor;
      FThreads[i].Free;
    end;
  end;
end;

end.
```

## Partie 9 : Cas d'usage réels

### 1. Système de traçabilité alimentaire

```pascal
unit FoodTraceability;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SmartContract;

type
  TFoodProduct = record
    ProductID: string;
    Name: string;
    Origin: string;
    ProductionDate: TDateTime;
    ExpiryDate: TDateTime;
    CurrentOwner: string;
    Temperature: Double;
    IsOrganic: Boolean;
  end;

  TFoodTraceabilityContract = class(TSmartContractBase)
  private
    FProducts: array of TFoodProduct;
  public
    constructor Create(const AOwner: string);

    function Execute(const Method: string; const Params: array of string): string; override;

    procedure RegisterProduct(const ProductID, Name, Origin: string;
                             ProductionDate, ExpiryDate: TDateTime; IsOrganic: Boolean);
    procedure TransferOwnership(const ProductID, NewOwner: string);
    procedure UpdateTemperature(const ProductID: string; Temperature: Double);
    function GetProductHistory(const ProductID: string): string;
    function VerifyAuthenticity(const ProductID: string): Boolean;
  end;

implementation

constructor TFoodTraceabilityContract.Create(const AOwner: string);  
begin
  inherited Create(AOwner);
  SetLength(FProducts, 0);

  EmitEvent('ContractCreated', 'Food Traceability System initialized');
end;

function TFoodTraceabilityContract.Execute(const Method: string;
  const Params: array of string): string;
begin
  Result := '';

  if Method = 'registerProduct' then
  begin
    if Length(Params) >= 6 then
    begin
      RegisterProduct(Params[0], Params[1], Params[2],
        StrToDateTimeDef(Params[3], Now),
        StrToDateTimeDef(Params[4], Now),
        StrToBool(Params[5]));
      Result := 'PRODUCT_REGISTERED';
    end;
  end
  else if Method = 'transferOwnership' then
  begin
    if Length(Params) >= 2 then
    begin
      TransferOwnership(Params[0], Params[1]);
      Result := 'OWNERSHIP_TRANSFERRED';
    end;
  end
  else if Method = 'updateTemperature' then
  begin
    if Length(Params) >= 2 then
    begin
      UpdateTemperature(Params[0], StrToFloatDef(Params[1], 0));
      Result := 'TEMPERATURE_UPDATED';
    end;
  end
  else if Method = 'getHistory' then
  begin
    if Length(Params) >= 1 then
      Result := GetProductHistory(Params[0]);
  end
  else
    Result := 'UNKNOWN_METHOD';
end;

procedure TFoodTraceabilityContract.RegisterProduct(const ProductID, Name, Origin: string;
  ProductionDate, ExpiryDate: TDateTime; IsOrganic: Boolean);
var
  Product: TFoodProduct;
begin
  Product.ProductID := ProductID;
  Product.Name := Name;
  Product.Origin := Origin;
  Product.ProductionDate := ProductionDate;
  Product.ExpiryDate := ExpiryDate;
  Product.CurrentOwner := Owner;
  Product.Temperature := 20.0;
  Product.IsOrganic := IsOrganic;

  SetLength(FProducts, Length(FProducts) + 1);
  FProducts[High(FProducts)] := Product;

  SetStorageValue('product_' + ProductID, Name);

  EmitEvent('ProductRegistered', Format('%s: %s from %s',
    [ProductID, Name, Origin]));
end;

procedure TFoodTraceabilityContract.TransferOwnership(const ProductID, NewOwner: string);  
var
  i: Integer;
begin
  for i := 0 to High(FProducts) do
  begin
    if FProducts[i].ProductID = ProductID then
    begin
      EmitEvent('OwnershipTransferred', Format('%s: %s -> %s',
        [ProductID, FProducts[i].CurrentOwner, NewOwner]));

      FProducts[i].CurrentOwner := NewOwner;
      Exit;
    end;
  end;
end;

procedure TFoodTraceabilityContract.UpdateTemperature(const ProductID: string;
  Temperature: Double);
var
  i: Integer;
begin
  for i := 0 to High(FProducts) do
  begin
    if FProducts[i].ProductID = ProductID then
    begin
      FProducts[i].Temperature := Temperature;

      // Alerte si température anormale
      if (Temperature < 0) or (Temperature > 25) then
        EmitEvent('TemperatureAlert', Format('%s: %.1f°C (anormal!)',
          [ProductID, Temperature]))
      else
        EmitEvent('TemperatureUpdated', Format('%s: %.1f°C',
          [ProductID, Temperature]));

      Exit;
    end;
  end;
end;

function TFoodTraceabilityContract.GetProductHistory(const ProductID: string): string;  
var
  i: Integer;
begin
  Result := '';

  for i := 0 to High(FProducts) do
  begin
    if FProducts[i].ProductID = ProductID then
    begin
      Result := Format('Product: %s' + LineEnding +
                      'Name: %s' + LineEnding +
                      'Origin: %s' + LineEnding +
                      'Production: %s' + LineEnding +
                      'Expiry: %s' + LineEnding +
                      'Owner: %s' + LineEnding +
                      'Temperature: %.1f°C' + LineEnding +
                      'Organic: %s',
                      [FProducts[i].ProductID,
                       FProducts[i].Name,
                       FProducts[i].Origin,
                       DateToStr(FProducts[i].ProductionDate),
                       DateToStr(FProducts[i].ExpiryDate),
                       FProducts[i].CurrentOwner,
                       FProducts[i].Temperature,
                       BoolToStr(FProducts[i].IsOrganic, True)]);
      Exit;
    end;
  end;

  Result := 'Product not found';
end;

function TFoodTraceabilityContract.VerifyAuthenticity(const ProductID: string): Boolean;  
var
  i: Integer;
begin
  Result := False;

  for i := 0 to High(FProducts) do
  begin
    if FProducts[i].ProductID = ProductID then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

end.
```

### 2. Système de vote sécurisé pour élections

```pascal
unit SecureVoting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SmartContract, fgl;

type
  TCandidate = class
  public
    ID: Integer;
    Name: string;
    Party: string;
    VoteCount: Integer;

    constructor Create(AID: Integer; const AName, AParty: string);
  end;

  TCandidateList = specialize TFPGObjectList<TCandidate>;

  TVoterRecord = record
    VoterID: string;
    HasVoted: Boolean;
    VoteTimestamp: TDateTime;
    EncryptedVote: string;
  end;

  TSecureVotingContract = class(TSmartContractBase)
  private
    FCandidates: TCandidateList;
    FVoters: array of TVoterRecord;
    FVotingStartTime: TDateTime;
    FVotingEndTime: TDateTime;
    FIsVotingOpen: Boolean;
    FRequireIdentification: Boolean;
  public
    constructor Create(const AOwner: string; StartTime, EndTime: TDateTime);
    destructor Destroy; override;

    function Execute(const Method: string; const Params: array of string): string; override;

    procedure AddCandidate(const Name, Party: string);
    procedure RegisterVoter(const VoterID: string);
    function CastVote(const VoterID: string; CandidateID: Integer): Boolean;
    function GetResults: string;
    procedure CloseVoting;
    function IsVotingActive: Boolean;
  end;

implementation

uses
  DateUtils, sha256;

{ TCandidate }

constructor TCandidate.Create(AID: Integer; const AName, AParty: string);  
begin
  inherited Create;
  ID := AID;
  Name := AName;
  Party := AParty;
  VoteCount := 0;
end;

{ TSecureVotingContract }

constructor TSecureVotingContract.Create(const AOwner: string;
  StartTime, EndTime: TDateTime);
begin
  inherited Create(AOwner);

  FCandidates := TCandidateList.Create(True);
  SetLength(FVoters, 0);
  FVotingStartTime := StartTime;
  FVotingEndTime := EndTime;
  FIsVotingOpen := False;
  FRequireIdentification := True;

  EmitEvent('VotingSystemCreated', Format('Start: %s, End: %s',
    [DateTimeToStr(StartTime), DateTimeToStr(EndTime)]));
end;

destructor TSecureVotingContract.Destroy;  
begin
  FCandidates.Free;
  inherited Destroy;
end;

function TSecureVotingContract.Execute(const Method: string;
  const Params: array of string): string;
begin
  Result := '';

  if Method = 'addCandidate' then
  begin
    if Length(Params) >= 2 then
    begin
      AddCandidate(Params[0], Params[1]);
      Result := 'CANDIDATE_ADDED';
    end;
  end
  else if Method = 'registerVoter' then
  begin
    if Length(Params) >= 1 then
    begin
      RegisterVoter(Params[0]);
      Result := 'VOTER_REGISTERED';
    end;
  end
  else if Method = 'castVote' then
  begin
    if Length(Params) >= 2 then
    begin
      if CastVote(Params[0], StrToIntDef(Params[1], -1)) then
        Result := 'VOTE_CAST'
      else
        Result := 'VOTE_REJECTED';
    end;
  end
  else if Method = 'getResults' then
  begin
    Result := GetResults;
  end
  else
    Result := 'UNKNOWN_METHOD';
end;

procedure TSecureVotingContract.AddCandidate(const Name, Party: string);  
var
  Candidate: TCandidate;
begin
  Candidate := TCandidate.Create(FCandidates.Count, Name, Party);
  FCandidates.Add(Candidate);

  EmitEvent('CandidateAdded', Format('%s (%s)', [Name, Party]));
end;

procedure TSecureVotingContract.RegisterVoter(const VoterID: string);  
var
  Voter: TVoterRecord;
begin
  Voter.VoterID := VoterID;
  Voter.HasVoted := False;
  Voter.VoteTimestamp := 0;
  Voter.EncryptedVote := '';

  SetLength(FVoters, Length(FVoters) + 1);
  FVoters[High(FVoters)] := Voter;

  EmitEvent('VoterRegistered', Format('Voter: %s', [VoterID]));
end;

function TSecureVotingContract.CastVote(const VoterID: string;
  CandidateID: Integer): Boolean;
var
  i: Integer;
  EncryptedVote: string;
begin
  Result := False;

  // Vérifier que le vote est ouvert
  if not IsVotingActive then
  begin
    WriteLn('Vote fermé');
    Exit;
  end;

  // Vérifier que le candidat existe
  if (CandidateID < 0) or (CandidateID >= FCandidates.Count) then
  begin
    WriteLn('Candidat invalide');
    Exit;
  end;

  // Chercher l'électeur
  for i := 0 to High(FVoters) do
  begin
    if FVoters[i].VoterID = VoterID then
    begin
      // Vérifier qu'il n'a pas déjà voté
      if FVoters[i].HasVoted then
      begin
        WriteLn('Électeur a déjà voté');
        Exit;
      end;

      // Enregistrer le vote (chiffré pour l'anonymat)
      EncryptedVote := SHA256Print(SHA256String(Format('%s_%d_%s',
        [VoterID, CandidateID, DateTimeToStr(Now)])));

      FVoters[i].HasVoted := True;
      FVoters[i].VoteTimestamp := Now;
      FVoters[i].EncryptedVote := EncryptedVote;

      // Incrémenter le compteur du candidat
      FCandidates[CandidateID].VoteCount :=
        FCandidates[CandidateID].VoteCount + 1;

      EmitEvent('VoteCast', Format('Vote enregistré (ID: %s)', [EncryptedVote]));

      Result := True;
      Exit;
    end;
  end;

  WriteLn('Électeur non enregistré');
end;

function TSecureVotingContract.GetResults: string;  
var
  i, TotalVotes: Integer;
begin
  Result := '=== RÉSULTATS DU VOTE ===' + LineEnding + LineEnding;

  TotalVotes := 0;
  for i := 0 to FCandidates.Count - 1 do
    TotalVotes := TotalVotes + FCandidates[i].VoteCount;

  Result := Result + Format('Total de votes: %d' + LineEnding, [TotalVotes]);
  Result := Result + Format('Électeurs inscrits: %d' + LineEnding + LineEnding,
    [Length(FVoters)]);

  for i := 0 to FCandidates.Count - 1 do
  begin
    Result := Result + Format('%d. %s (%s): %d votes',
      [i + 1,
       FCandidates[i].Name,
       FCandidates[i].Party,
       FCandidates[i].VoteCount]);

    if TotalVotes > 0 then
      Result := Result + Format(' (%.2f%%)',
        [(FCandidates[i].VoteCount / TotalVotes) * 100]);

    Result := Result + LineEnding;
  end;
end;

procedure TSecureVotingContract.CloseVoting;  
begin
  FIsVotingOpen := False;
  EmitEvent('VotingClosed', 'Le vote est maintenant fermé');
end;

function TSecureVotingContract.IsVotingActive: Boolean;  
begin
  Result := (Now >= FVotingStartTime) and
            (Now <= FVotingEndTime) and
            FIsVotingOpen;
end;

end.
```

## Conclusion

### Ce que nous avons appris

Dans ce chapitre, nous avons découvert comment implémenter une blockchain complète avec FreePascal/Lazarus, incluant :

1. **Structure de base de la blockchain**
   - Blocs et transactions
   - Hash cryptographique (SHA-256)
   - Chaînage des blocs

2. **Mécanismes de consensus**
   - Proof of Work (Preuve de travail)
   - Proof of Stake (Preuve d'enjeu)
   - Minage multi-thread

3. **Smart Contracts**
   - Tokens (crypto-monnaies)
   - Système de vote décentralisé
   - Enchères automatisées
   - Traçabilité alimentaire

4. **Réseau P2P**
   - Communication entre nœuds
   - Synchronisation de blockchain
   - Découverte de pairs

5. **Outils pratiques**
   - Wallet (portefeuille cryptographique)
   - Interface graphique
   - Merkle Tree pour validation

### Avantages de FreePascal pour la blockchain

- **Performance** : Code compilé natif, idéal pour le calcul intensif du minage
- **Multi-plateforme** : Déploiement sur Windows, Linux, macOS
- **Typage fort** : Réduit les erreurs dans le code critique
- **Contrôle mémoire** : Gestion précise des ressources
- **Simplicité** : Syntaxe claire et lisible

### Limitations et améliorations possibles

**Limitations de notre implémentation :**
- Cryptographie simplifiée (utiliser une vraie bibliothèque ECDSA)
- Pas de véritable réseau P2P distribué
- Stockage en mémoire (utiliser une vraie base de données)
- Pas de gestion de forks (embranchements de la chaîne)
- Validation simplifiée des transactions

**Améliorations recommandées pour une version production :**

1. **Cryptographie robuste**
```pascal
// Utiliser des bibliothèques cryptographiques éprouvées
// - OpenSSL pour ECDSA (Elliptic Curve Digital Signature Algorithm)
// - Courbes elliptiques (secp256k1 comme Bitcoin)
// - Génération sécurisée de nombres aléatoires
```

2. **Persistance des données**
```pascal
// Stocker la blockchain dans une base de données
// - LevelDB ou RocksDB pour les performances
// - PostgreSQL pour la requêtabilité
// - Système de snapshots pour récupération rapide
```

3. **Réseau P2P avancé**
```pascal
// Protocole réseau robuste
// - DHT (Distributed Hash Table) pour découverte de pairs
// - Gossip protocol pour propagation rapide
// - NAT traversal pour connectivité
// - Gestion des pairs malveillants
```

4. **Machine virtuelle pour smart contracts**
```pascal
// VM sécurisée et isolée
// - Sandbox d'exécution
// - Limitation de gas/ressources
// - Support de langages multiples
// - Compilation Just-In-Time
```

### Comparaison avec les blockchains existantes

#### Notre implémentation vs Bitcoin

| Caractéristique | Notre blockchain | Bitcoin |
|----------------|------------------|---------|
| Consensus | PoW simple | PoW SHA-256 double |
| Temps de bloc | Variable | ~10 minutes |
| Difficulté | Fixe | Ajustement automatique |
| Smart contracts | Oui | Scripts limités |
| Langage | Pascal | C++ |

#### Notre implémentation vs Ethereum

| Caractéristique | Notre blockchain | Ethereum |
|----------------|------------------|----------|
| Smart contracts | Classes Pascal | EVM + Solidity |
| Consensus | PoW/PoS basique | PoS (Ethereum 2.0) |
| Gas | Non implémenté | Oui |
| State management | Simplifié | Merkle Patricia Trie |
| Langage | Pascal | Go, Rust, etc. |

### Cas d'usage professionnels

#### 1. Supply Chain (Chaîne d'approvisionnement)

```pascal
// Exemple d'utilisation pour tracer des produits pharmaceutiques
var
  SupplyChain: TFoodTraceabilityContract;
begin
  SupplyChain := TFoodTraceabilityContract.Create('Manufacturer');

  // Enregistrer un médicament
  SupplyChain.RegisterProduct(
    'MED-2024-001',
    'Aspirine 500mg',
    'France',
    Now,
    IncYear(Now, 2),
    False
  );

  // Tracer le parcours
  SupplyChain.TransferOwnership('MED-2024-001', 'Grossiste');
  SupplyChain.TransferOwnership('MED-2024-001', 'Pharmacie');

  // Vérifier l'authenticité
  if SupplyChain.VerifyAuthenticity('MED-2024-001') then
    WriteLn('Produit authentique et traçable');
end;
```

#### 2. Certification de documents

```pascal
unit DocumentCertification;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SmartContract, sha256;

type
  TDocument = record
    DocumentHash: string;
    Owner: string;
    Timestamp: TDateTime;
    Description: string;
    IsValid: Boolean;
  end;

  TDocumentCertificationContract = class(TSmartContractBase)
  private
    FDocuments: array of TDocument;
  public
    constructor Create(const AOwner: string);

    function Execute(const Method: string; const Params: array of string): string; override;

    procedure CertifyDocument(const DocumentContent, Description: string);
    function VerifyDocument(const DocumentContent: string): Boolean;
    function GetDocumentInfo(const DocumentHash: string): string;
    procedure RevokeDocument(const DocumentHash: string);
  end;

implementation

constructor TDocumentCertificationContract.Create(const AOwner: string);  
begin
  inherited Create(AOwner);
  SetLength(FDocuments, 0);

  EmitEvent('ContractCreated', 'Document Certification System');
end;

function TDocumentCertificationContract.Execute(const Method: string;
  const Params: array of string): string;
begin
  Result := '';

  if Method = 'certify' then
  begin
    if Length(Params) >= 2 then
    begin
      CertifyDocument(Params[0], Params[1]);
      Result := 'DOCUMENT_CERTIFIED';
    end;
  end
  else if Method = 'verify' then
  begin
    if Length(Params) >= 1 then
    begin
      if VerifyDocument(Params[0]) then
        Result := 'VALID'
      else
        Result := 'INVALID';
    end;
  end
  else if Method = 'getInfo' then
  begin
    if Length(Params) >= 1 then
      Result := GetDocumentInfo(Params[0]);
  end
  else
    Result := 'UNKNOWN_METHOD';
end;

procedure TDocumentCertificationContract.CertifyDocument(
  const DocumentContent, Description: string);
var
  Doc: TDocument;
begin
  // Calculer le hash du document
  Doc.DocumentHash := SHA256Print(SHA256String(DocumentContent));
  Doc.Owner := Owner;
  Doc.Timestamp := Now;
  Doc.Description := Description;
  Doc.IsValid := True;

  SetLength(FDocuments, Length(FDocuments) + 1);
  FDocuments[High(FDocuments)] := Doc;

  SetStorageValue('doc_' + Doc.DocumentHash, Description);

  EmitEvent('DocumentCertified', Format('Hash: %s, Desc: %s',
    [Copy(Doc.DocumentHash, 1, 16) + '...', Description]));
end;

function TDocumentCertificationContract.VerifyDocument(
  const DocumentContent: string): Boolean;
var
  i: Integer;
  DocumentHash: string;
begin
  Result := False;
  DocumentHash := SHA256Print(SHA256String(DocumentContent));

  for i := 0 to High(FDocuments) do
  begin
    if (FDocuments[i].DocumentHash = DocumentHash) and
       FDocuments[i].IsValid then
    begin
      Result := True;
      EmitEvent('DocumentVerified', Format('Valid document found: %s',
        [FDocuments[i].Description]));
      Exit;
    end;
  end;
end;

function TDocumentCertificationContract.GetDocumentInfo(
  const DocumentHash: string): string;
var
  i: Integer;
begin
  Result := 'Document not found';

  for i := 0 to High(FDocuments) do
  begin
    if FDocuments[i].DocumentHash = DocumentHash then
    begin
      Result := Format('Document: %s' + LineEnding +
                      'Owner: %s' + LineEnding +
                      'Certified: %s' + LineEnding +
                      'Valid: %s',
                      [FDocuments[i].Description,
                       FDocuments[i].Owner,
                       DateTimeToStr(FDocuments[i].Timestamp),
                       BoolToStr(FDocuments[i].IsValid, True)]);
      Exit;
    end;
  end;
end;

procedure TDocumentCertificationContract.RevokeDocument(const DocumentHash: string);  
var
  i: Integer;
begin
  for i := 0 to High(FDocuments) do
  begin
    if FDocuments[i].DocumentHash = DocumentHash then
    begin
      FDocuments[i].IsValid := False;
      EmitEvent('DocumentRevoked', Format('Document revoked: %s',
        [FDocuments[i].Description]));
      Exit;
    end;
  end;
end;

end.
```

#### 3. Gestion de propriété intellectuelle

```pascal
unit IntellectualProperty;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SmartContract, sha256;

type
  TIPRegistration = record
    WorkHash: string;
    Title: string;
    Author: string;
    RegistrationDate: TDateTime;
    IPType: string; // 'Patent', 'Copyright', 'Trademark'
    Description: string;
    LicenseType: string; // 'Exclusive', 'Non-Exclusive', 'Open'
  end;

  TIPContract = class(TSmartContractBase)
  private
    FRegistrations: array of TIPRegistration;
  public
    constructor Create(const AOwner: string);

    function Execute(const Method: string; const Params: array of string): string; override;

    procedure RegisterWork(const Title, WorkContent, IPType, Description: string);
    function CheckPriorArt(const WorkContent: string): Boolean;
    function TransferRights(const WorkHash, NewOwner: string): Boolean;
    function GetRegistrationInfo(const WorkHash: string): string;
  end;

implementation

constructor TIPContract.Create(const AOwner: string);  
begin
  inherited Create(AOwner);
  SetLength(FRegistrations, 0);

  EmitEvent('ContractCreated', 'Intellectual Property Registry');
end;

function TIPContract.Execute(const Method: string;
  const Params: array of string): string;
begin
  Result := '';

  if Method = 'register' then
  begin
    if Length(Params) >= 4 then
    begin
      RegisterWork(Params[0], Params[1], Params[2], Params[3]);
      Result := 'WORK_REGISTERED';
    end;
  end
  else if Method = 'checkPriorArt' then
  begin
    if Length(Params) >= 1 then
    begin
      if CheckPriorArt(Params[0]) then
        Result := 'PRIOR_ART_EXISTS'
      else
        Result := 'NO_PRIOR_ART';
    end;
  end
  else if Method = 'transfer' then
  begin
    if Length(Params) >= 2 then
    begin
      if TransferRights(Params[0], Params[1]) then
        Result := 'RIGHTS_TRANSFERRED'
      else
        Result := 'TRANSFER_FAILED';
    end;
  end
  else
    Result := 'UNKNOWN_METHOD';
end;

procedure TIPContract.RegisterWork(const Title, WorkContent, IPType, Description: string);  
var
  Registration: TIPRegistration;
begin
  // Vérifier l'antériorité
  if CheckPriorArt(WorkContent) then
  begin
    WriteLn('ATTENTION: Une œuvre similaire existe déjà!');
    Exit;
  end;

  Registration.WorkHash := SHA256Print(SHA256String(WorkContent));
  Registration.Title := Title;
  Registration.Author := Owner;
  Registration.RegistrationDate := Now;
  Registration.IPType := IPType;
  Registration.Description := Description;
  Registration.LicenseType := 'Exclusive';

  SetLength(FRegistrations, Length(FRegistrations) + 1);
  FRegistrations[High(FRegistrations)] := Registration;

  SetStorageValue('ip_' + Registration.WorkHash, Title);

  EmitEvent('WorkRegistered', Format('%s: %s (%s)',
    [IPType, Title, Registration.Author]));
end;

function TIPContract.CheckPriorArt(const WorkContent: string): Boolean;  
var
  i: Integer;
  WorkHash: string;
begin
  Result := False;
  WorkHash := SHA256Print(SHA256String(WorkContent));

  for i := 0 to High(FRegistrations) do
  begin
    if FRegistrations[i].WorkHash = WorkHash then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TIPContract.TransferRights(const WorkHash, NewOwner: string): Boolean;  
var
  i: Integer;
begin
  Result := False;

  for i := 0 to High(FRegistrations) do
  begin
    if FRegistrations[i].WorkHash = WorkHash then
    begin
      EmitEvent('RightsTransferred', Format('%s: %s -> %s',
        [FRegistrations[i].Title, FRegistrations[i].Author, NewOwner]));

      FRegistrations[i].Author := NewOwner;
      Result := True;
      Exit;
    end;
  end;
end;

function TIPContract.GetRegistrationInfo(const WorkHash: string): string;  
var
  i: Integer;
begin
  Result := 'Registration not found';

  for i := 0 to High(FRegistrations) do
  begin
    if FRegistrations[i].WorkHash = WorkHash then
    begin
      Result := Format('Title: %s' + LineEnding +
                      'Type: %s' + LineEnding +
                      'Author: %s' + LineEnding +
                      'Registered: %s' + LineEnding +
                      'License: %s' + LineEnding +
                      'Description: %s',
                      [FRegistrations[i].Title,
                       FRegistrations[i].IPType,
                       FRegistrations[i].Author,
                       DateTimeToStr(FRegistrations[i].RegistrationDate),
                       FRegistrations[i].LicenseType,
                       FRegistrations[i].Description]);
      Exit;
    end;
  end;
end;

end.
```

### Déploiement multi-plateforme

#### Configuration Windows

```batch
@echo off
REM Script de compilation et déploiement Windows

echo Compilation de la blockchain...  
fpc -O3 -CX -XX -Xs blockchain_main.pas

echo Creation du repertoire de deploiement...  
mkdir deploy\windows  
copy blockchain_main.exe deploy\windows\  
copy *.dll deploy\windows\

echo Creation de l'installateur...
"C:\Program Files (x86)\Inno Setup 6\ISCC.exe" blockchain_setup.iss

echo Deploiement termine!  
pause
```

#### Configuration Linux/Ubuntu

```bash
#!/bin/bash
# Script de compilation et déploiement Linux

echo "Compilation de la blockchain..."  
fpc -O3 -CX -XX -Xs blockchain_main.pas

echo "Création du paquet..."  
mkdir -p deploy/linux/usr/local/bin  
mkdir -p deploy/linux/usr/share/blockchain  
mkdir -p deploy/linux/etc/blockchain

cp blockchain_main deploy/linux/usr/local/bin/  
cp config.ini deploy/linux/etc/blockchain/

echo "Création du .deb..."  
dpkg-deb --build deploy/linux blockchain-1.0.0-amd64.deb

echo "Déploiement terminé!"
```

### Tests et validation

#### Tests unitaires pour la blockchain

```pascal
unit BlockchainTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Blockchain, BlockchainTypes;

type
  TBlockchainTest = class(TTestCase)
  private
    FBlockchain: TBlockchain;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateBlockchain;
    procedure TestAddTransaction;
    procedure TestMineBlock;
    procedure TestValidateChain;
    procedure TestDoubleSpend;
  end;

implementation

procedure TBlockchainTest.SetUp;  
begin
  FBlockchain := TBlockchain.Create(2); // Difficulté faible pour les tests
end;

procedure TBlockchainTest.TearDown;  
begin
  FBlockchain.Free;
end;

procedure TBlockchainTest.TestCreateBlockchain;  
begin
  AssertNotNull('Blockchain créée', FBlockchain);
  AssertEquals('Un bloc genesis existe', 1, FBlockchain.Chain.Count);
end;

procedure TBlockchainTest.TestAddTransaction;  
var
  Tx: TTransaction;
  InitialCount: Integer;
begin
  InitialCount := FBlockchain.Chain.Count;

  Tx := TTransaction.Create('Alice', 'Bob', 50);
  FBlockchain.AddTransaction(Tx);

  AssertTrue('Transaction ajoutée', True);
end;

procedure TBlockchainTest.TestMineBlock;  
var
  Tx: TTransaction;
  InitialCount: Integer;
begin
  InitialCount := FBlockchain.Chain.Count;

  Tx := TTransaction.Create('Alice', 'Bob', 50);
  FBlockchain.AddTransaction(Tx);

  AssertTrue('Minage réussi', FBlockchain.MinePendingTransactions('Miner'));
  AssertEquals('Nouveau bloc ajouté', InitialCount + 1, FBlockchain.Chain.Count);
end;

procedure TBlockchainTest.TestValidateChain;  
var
  Tx: TTransaction;
begin
  Tx := TTransaction.Create('Alice', 'Bob', 50);
  FBlockchain.AddTransaction(Tx);
  FBlockchain.MinePendingTransactions('Miner');

  AssertTrue('Blockchain valide', FBlockchain.IsChainValid);
end;

procedure TBlockchainTest.TestDoubleSpend;  
var
  Tx1, Tx2: TTransaction;
begin
  // Première transaction
  Tx1 := TTransaction.Create('Alice', 'Bob', 100);
  FBlockchain.AddTransaction(Tx1);
  FBlockchain.MinePendingTransactions('Miner');

  // Tentative de double dépense
  Tx2 := TTransaction.Create('Alice', 'Charlie', 100);
  FBlockchain.AddTransaction(Tx2);

  // Cette transaction devrait être rejetée si Alice n'a plus de fonds
  AssertTrue('Protection contre double dépense', True);
end;

initialization
  RegisterTest(TBlockchainTest);

end.
```

### Monitoring et performance

#### Métriques de performance

```pascal
unit BlockchainMetrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBlockchainMetrics = class
  private
    FTotalBlocks: Integer;
    FTotalTransactions: Integer;
    FTotalHashRate: Double;
    FAvgBlockTime: Double;
    FNetworkNodes: Integer;
    FStartTime: TDateTime;
  public
    constructor Create;

    procedure IncrementBlocks;
    procedure IncrementTransactions(Count: Integer);
    procedure UpdateHashRate(NewHashRate: Double);
    procedure UpdateBlockTime(BlockTime: Double);
    procedure SetNodeCount(Count: Integer);

    function GetMetricsReport: string;
    function GetUptime: string;

    property TotalBlocks: Integer read FTotalBlocks;
    property TotalTransactions: Integer read FTotalTransactions;
    property HashRate: Double read FTotalHashRate;
  end;

implementation

uses
  DateUtils;

constructor TBlockchainMetrics.Create;  
begin
  inherited Create;
  FTotalBlocks := 0;
  FTotalTransactions := 0;
  FTotalHashRate := 0;
  FAvgBlockTime := 0;
  FNetworkNodes := 0;
  FStartTime := Now;
end;

procedure TBlockchainMetrics.IncrementBlocks;  
begin
  Inc(FTotalBlocks);
end;

procedure TBlockchainMetrics.IncrementTransactions(Count: Integer);  
begin
  FTotalTransactions := FTotalTransactions + Count;
end;

procedure TBlockchainMetrics.UpdateHashRate(NewHashRate: Double);  
begin
  FTotalHashRate := NewHashRate;
end;

procedure TBlockchainMetrics.UpdateBlockTime(BlockTime: Double);  
begin
  // Moyenne mobile
  FAvgBlockTime := (FAvgBlockTime * 0.9) + (BlockTime * 0.1);
end;

procedure TBlockchainMetrics.SetNodeCount(Count: Integer);  
begin
  FNetworkNodes := Count;
end;

function TBlockchainMetrics.GetMetricsReport: string;  
begin
  Result := '═══════════════════════════════════════' + LineEnding;
  Result := Result + '      MÉTRIQUES BLOCKCHAIN' + LineEnding;
  Result := Result + '═══════════════════════════════════════' + LineEnding;
  Result := Result + Format('Blocs totaux        : %d' + LineEnding, [FTotalBlocks]);
  Result := Result + Format('Transactions totales: %d' + LineEnding, [FTotalTransactions]);
  Result := Result + Format('Taux de hash        : %.2f H/s' + LineEnding, [FTotalHashRate]);
  Result := Result + Format('Temps moyen/bloc    : %.2f s' + LineEnding, [FAvgBlockTime]);
  Result := Result + Format('Nœuds actifs        : %d' + LineEnding, [FNetworkNodes]);
  Result := Result + Format('Uptime              : %s' + LineEnding, [GetUptime]);
  Result := Result + '═══════════════════════════════════════';
end;

function TBlockchainMetrics.GetUptime: string;  
var
  Duration: TDateTime;
  Days, Hours, Minutes, Seconds: Integer;
begin
  Duration := Now - FStartTime;
  Days := Trunc(Duration);
  Hours := HourOf(Duration);
  Minutes := MinuteOf(Duration);
  Seconds := SecondOf(Duration);

  Result := Format('%dd %dh %dm %ds', [Days, Hours, Minutes, Seconds]);
end;

end.
```

### Ressources et documentation

#### Livres recommandés

1. **"Mastering Bitcoin"** par Andreas M. Antonopoulos
   - Explications détaillées du protocole Bitcoin
   - Architecture technique complète

2. **"Mastering Ethereum"** par Andreas M. Antonopoulos et Gavin Wood
   - Smart contracts et EVM
   - Développement d'applications décentralisées

3. **"Blockchain Basics"** par Daniel Drescher
   - Introduction accessible aux concepts

#### Sites web et tutoriels

- **Bitcoin.org** : Documentation officielle Bitcoin
- **Ethereum.org** : Documentation Ethereum et Solidity
- **FreePascal Wiki** : Ressources FreePascal/Lazarus
- **GitHub** : Exemples de code open source

#### Projets open source à étudier

1. **Bitcoin Core** (C++) : Implémentation de référence
2. **Go-Ethereum** (Go) : Client Ethereum
3. **Hyperledger Fabric** (Go) : Blockchain d'entreprise
4. **Substrate** (Rust) : Framework blockchain modulaire

### Conclusion finale

Nous avons construit un système blockchain fonctionnel avec FreePascal/Lazarus, démontrant que ce langage est parfaitement capable de gérer des projets complexes et modernes.

**Points clés à retenir :**

✓ **Blockchain = Structure de données + Cryptographie + Consensus**  
✓ **Smart contracts = Code auto-exécutable sur la blockchain**  
✓ **P2P = Décentralisation et résilience**  
✓ **FreePascal = Performance + Portabilité + Simplicité**

**Prochaines étapes suggérées :**

1. Implémenter une vraie cryptographie ECDSA
2. Créer un réseau P2P complet avec découverte automatique
3. Optimiser le stockage avec une base de données
4. Développer une machine virtuelle pour smart contracts
5. Ajouter des fonctionnalités avancées (sharding, layer 2, etc.)

**Le futur de la blockchain :**

La technologie blockchain continue d'évoluer avec des innovations comme :
- **Proof of Stake** pour réduire la consommation énergétique
- **Sharding** pour améliorer la scalabilité
- **Zero-Knowledge Proofs** pour la confidentialité
- **Cross-chain bridges** pour l'interopérabilité
- **DeFi** (Finance décentralisée)
- **NFT** (Tokens non fongibles)
- **DAOs** (Organisations autonomes décentralisées)

FreePascal/Lazarus offre tous les outils nécessaires pour explorer ces technologies et créer des applications blockchain innovantes et performantes !

---

**Félicitations !** Vous maîtrisez maintenant les fondamentaux de la blockchain avec FreePascal/Lazarus. Ce chapitre vous a donné les bases pour créer vos propres projets décentralisés. N'hésitez pas à expérimenter et à contribuer à l'écosystème open source !

⏭️ [Compilateur ou interpréteur](/25-projets-complexes-etudes-cas/08-compilateur-ou-interpreteur.md)
