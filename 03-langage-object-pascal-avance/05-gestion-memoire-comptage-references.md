🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Gestion mémoire et comptage de références en FreePascal/Lazarus

## Introduction : Pourquoi gérer la mémoire ?

La mémoire est comme un grand tableau blanc où votre programme écrit des informations. Chaque fois que vous créez une variable, un objet ou une structure de données, vous utilisez une partie de ce tableau. Le problème ? Ce tableau n'est pas infini ! Si vous continuez à écrire sans jamais effacer, vous finirez par manquer de place.

La **gestion de la mémoire** consiste à :
- Allouer (réserver) de la mémoire quand vous en avez besoin
- Libérer (rendre) cette mémoire quand vous n'en avez plus besoin
- Éviter les fuites mémoire (mémoire réservée mais jamais libérée)
- Éviter les erreurs d'accès (utiliser de la mémoire déjà libérée)

## Les bases de la mémoire en FreePascal

### Types de mémoire

FreePascal utilise principalement deux zones de mémoire :

```pascal
program MemoryBasics;

var
  // Variables sur la PILE (Stack) - gérées automatiquement
  LocalInteger: Integer;
  LocalString: String;

  // Pointeur vers le TAS (Heap) - gestion manuelle nécessaire
  DynamicObject: TObject;
  DynamicArray: array of Integer;

begin
  // Sur la pile : automatiquement libéré à la fin du bloc
  LocalInteger := 42;
  LocalString := 'Bonjour';

  // Sur le tas : doit être libéré manuellement
  DynamicObject := TObject.Create;
  try
    // Utiliser l'objet
  finally
    DynamicObject.Free; // Libération manuelle obligatoire
  end;

  // Tableaux dynamiques : gestion semi-automatique
  SetLength(DynamicArray, 10);
  // Pas besoin de Free, mais SetLength(DynamicArray, 0) peut aider
end.
```

### Allocation et libération basiques

```pascal
type
  TPerson = class
  private
    FName: String;
    FAge: Integer;
  public
    constructor Create(const AName: String; AAge: Integer);
    destructor Destroy; override;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

constructor TPerson.Create(const AName: String; AAge: Integer);  
begin
  inherited Create;
  FName := AName;
  FAge := AAge;
  WriteLn('Personne créée : ', FName);
end;

destructor TPerson.Destroy;  
begin
  WriteLn('Personne détruite : ', FName);
  inherited Destroy;
end;

// Utilisation correcte
var
  Person: TPerson;
begin
  Person := TPerson.Create('Alice', 30);
  try
    WriteLn(Person.Name, ' a ', Person.Age, ' ans');
  finally
    Person.Free; // TOUJOURS libérer ce qu'on a créé
  end;
end;
```

## Gestion manuelle de la mémoire

### Le pattern try-finally

C'est LE pattern fondamental pour une gestion sûre de la mémoire :

```pascal
procedure SafeMemoryPattern;  
var
  Obj1, Obj2, Obj3: TObject;
begin
  Obj1 := nil;
  Obj2 := nil;
  Obj3 := nil;

  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;

    // Utiliser les objets ici

  finally
    // Libération dans l'ordre inverse de création
    Obj3.Free;
    Obj2.Free;
    Obj1.Free;
  end;
end;

// Version plus robuste avec vérifications
procedure RobustMemoryPattern;  
var
  List: TStringList;
  Stream: TFileStream;
begin
  List := nil;
  Stream := nil;

  try
    List := TStringList.Create;

    try
      Stream := TFileStream.Create('data.txt', fmOpenRead);
      List.LoadFromStream(Stream);
    finally
      Stream.Free; // Libérer même si LoadFromStream échoue
    end;

    // Traiter la liste
    WriteLn('Lignes lues : ', List.Count);

  finally
    List.Free;
  end;
end;
```

### Gestion des listes d'objets

Attention particulière avec les conteneurs d'objets :

```pascal
type
  TPersonList = class
  private
    FItems: TList;
    FOwnsObjects: Boolean;
  public
    constructor Create(AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    procedure Add(Person: TPerson);
    procedure Clear;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

constructor TPersonList.Create(AOwnsObjects: Boolean);  
begin
  inherited Create;
  FItems := TList.Create;
  FOwnsObjects := AOwnsObjects;
end;

destructor TPersonList.Destroy;  
begin
  Clear;
  FItems.Free;
  inherited;
end;

procedure TPersonList.Add(Person: TPerson);  
begin
  FItems.Add(Person);
end;

procedure TPersonList.Clear;  
var
  I: Integer;
begin
  if FOwnsObjects then
  begin
    // La liste possède les objets : elle doit les libérer
    for I := FItems.Count - 1 downto 0 do
      TObject(FItems[I]).Free;
  end;
  FItems.Clear;
end;

// Utilisation
var
  List: TPersonList;
  Person: TPerson;
begin
  // Liste qui possède ses objets
  List := TPersonList.Create(True);
  try
    List.Add(TPerson.Create('Alice', 30));
    List.Add(TPerson.Create('Bob', 25));
    // Pas besoin de libérer Alice et Bob, la liste s'en charge
  finally
    List.Free; // Libère la liste ET les personnes
  end;

  // Liste qui ne possède pas ses objets
  List := TPersonList.Create(False);
  Person := TPerson.Create('Charlie', 35);
  try
    List.Add(Person);
    // Utilisation...
  finally
    List.Free;   // Libère seulement la liste
    Person.Free; // Il faut libérer Person séparément
  end;
end;
```

## Comptage de références avec les interfaces

### Introduction aux interfaces

Les interfaces en FreePascal supportent le comptage de références automatique :

```pascal
type
  // Interface avec comptage de références
  IPerson = interface
    ['{12345678-1234-1234-1234-123456789012}'] // GUID unique
    function GetName: String;
    procedure SetName(const Value: String);
    function GetAge: Integer;
    procedure SetAge(Value: Integer);
    property Name: String read GetName write SetName;
    property Age: Integer read GetAge write SetAge;
  end;

  // Implémentation avec comptage de références automatique
  TPersonRef = class(TInterfacedObject, IPerson)
  private
    FName: String;
    FAge: Integer;
    function GetName: String;
    procedure SetName(const Value: String);
    function GetAge: Integer;
    procedure SetAge(Value: Integer);
  public
    constructor Create(const AName: String; AAge: Integer);
    destructor Destroy; override;
  end;

constructor TPersonRef.Create(const AName: String; AAge: Integer);  
begin
  inherited Create;
  FName := AName;
  FAge := AAge;
  WriteLn('TPersonRef créé : ', FName, ' (RefCount=1)');
end;

destructor TPersonRef.Destroy;  
begin
  WriteLn('TPersonRef détruit : ', FName);
  inherited;
end;

function TPersonRef.GetName: String;  
begin
  Result := FName;
end;

procedure TPersonRef.SetName(const Value: String);  
begin
  FName := Value;
end;

function TPersonRef.GetAge: Integer;  
begin
  Result := FAge;
end;

procedure TPersonRef.SetAge(Value: Integer);  
begin
  FAge := Value;
end;

// Utilisation : pas de Free nécessaire !
procedure UseInterfaces;  
var
  Person1, Person2: IPerson;
begin
  WriteLn('Début du bloc');

  Person1 := TPersonRef.Create('Alice', 30);
  // RefCount = 1

  Person2 := Person1;
  // RefCount = 2 (deux références vers le même objet)

  Person1 := nil;
  // RefCount = 1 (une référence restante)

  WriteLn('Fin du bloc');
  // Person2 sort de portée, RefCount = 0, l'objet est automatiquement détruit
end;
```

### Mécanisme du comptage de références

```pascal
type
  TRefCountedObject = class(TInterfacedObject)
  protected
    function _AddRef: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  public
    constructor Create;
    destructor Destroy; override;
  end;

function TRefCountedObject._AddRef: Integer;  
begin
  Result := inherited _AddRef;
  WriteLn('AddRef appelé, nouveau compte : ', Result);
end;

function TRefCountedObject._Release: Integer;  
begin
  Result := inherited _Release;
  WriteLn('Release appelé, nouveau compte : ', Result);
  if Result = 0 then
    WriteLn('L''objet va être détruit');
end;

constructor TRefCountedObject.Create;  
begin
  inherited Create;
  WriteLn('Objet créé, RefCount initial : 1');
end;

destructor TRefCountedObject.Destroy;  
begin
  WriteLn('Destructeur appelé');
  inherited;
end;

// Démonstration du mécanisme
procedure DemoRefCounting;  
var
  Intf1, Intf2: IInterface;
begin
  WriteLn('=== Démonstration du comptage de références ===');

  WriteLn('1. Création de l''objet');
  Intf1 := TRefCountedObject.Create;

  WriteLn('2. Copie de la référence');
  Intf2 := Intf1;

  WriteLn('3. Mise à nil de la première référence');
  Intf1 := nil;

  WriteLn('4. Fin du bloc - libération automatique');
end;
```

## Gestion avancée avec smart pointers

### Implémentation d'un smart pointer simple

```pascal
type
  // Smart pointer générique
  generic TSmartPtr<T: class> = record
  private
    FObject: T;
    FRefCount: ^Integer;
    procedure AddRef;
    procedure Release;
  public
    class operator Initialize(var SP: TSmartPtr<T>);
    class operator Finalize(var SP: TSmartPtr<T>);
    class operator Copy(constref Source: TSmartPtr<T>;
                       var Dest: TSmartPtr<T>);

    procedure Create(AObject: T);
    function Get: T;
    function IsAssigned: Boolean;
    procedure Reset;

    property Value: T read Get;
  end;

class operator TSmartPtr<T>.Initialize(var SP: TSmartPtr<T>);  
begin
  SP.FObject := nil;
  SP.FRefCount := nil;
end;

class operator TSmartPtr<T>.Finalize(var SP: TSmartPtr<T>);  
begin
  SP.Release;
end;

class operator TSmartPtr<T>.Copy(constref Source: TSmartPtr<T>;
                                 var Dest: TSmartPtr<T>);
begin
  if @Source <> @Dest then
  begin
    Dest.Release;
    Dest.FObject := Source.FObject;
    Dest.FRefCount := Source.FRefCount;
    Dest.AddRef;
  end;
end;

procedure TSmartPtr<T>.AddRef;  
begin
  if Assigned(FRefCount) then
    Inc(FRefCount^);
end;

procedure TSmartPtr<T>.Release;  
begin
  if Assigned(FRefCount) then
  begin
    Dec(FRefCount^);
    if FRefCount^ = 0 then
    begin
      FObject.Free;
      Dispose(FRefCount);
      FObject := nil;
      FRefCount := nil;
    end;
  end;
end;

procedure TSmartPtr<T>.Create(AObject: T);  
begin
  Release;
  if Assigned(AObject) then
  begin
    FObject := AObject;
    New(FRefCount);
    FRefCount^ := 1;
  end;
end;

function TSmartPtr<T>.Get: T;  
begin
  Result := FObject;
end;

function TSmartPtr<T>.IsAssigned: Boolean;  
begin
  Result := Assigned(FObject);
end;

procedure TSmartPtr<T>.Reset;  
begin
  Release;
  FObject := nil;
  FRefCount := nil;
end;

// Utilisation du smart pointer
procedure UseSmartPointer;  
var
  SmartPerson1, SmartPerson2: specialize TSmartPtr<TPerson>;
begin
  WriteLn('Création avec smart pointer');
  SmartPerson1.Create(TPerson.Create('David', 40));

  WriteLn('Copie du smart pointer');
  SmartPerson2 := SmartPerson1; // Partage la même instance

  if SmartPerson1.IsAssigned then
    WriteLn('Personne : ', SmartPerson1.Value.Name);

  WriteLn('Fin du bloc - libération automatique');
  // Les smart pointers sont automatiquement nettoyés
end;
```

## Détection et prévention des fuites mémoire

### Utilisation du gestionnaire de mémoire pour détecter les fuites

```pascal
{$IFDEF DEBUG}
type
  TMemoryManager = class
  private
    FAllocations: TStringList;
    class var FInstance: TMemoryManager;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterAllocation(Obj: TObject; const Info: String);
    procedure UnregisterAllocation(Obj: TObject);
    procedure ReportLeaks;

    class function Instance: TMemoryManager;
  end;

constructor TMemoryManager.Create;  
begin
  inherited;
  FAllocations := TStringList.Create;
  FAllocations.OwnsObjects := False;
end;

destructor TMemoryManager.Destroy;  
begin
  ReportLeaks;
  FAllocations.Free;
  inherited;
end;

procedure TMemoryManager.RegisterAllocation(Obj: TObject; const Info: String);  
begin
  FAllocations.AddObject(Info, Obj);
end;

procedure TMemoryManager.UnregisterAllocation(Obj: TObject);  
var
  Index: Integer;
begin
  Index := FAllocations.IndexOfObject(Obj);
  if Index >= 0 then
    FAllocations.Delete(Index);
end;

procedure TMemoryManager.ReportLeaks;  
var
  I: Integer;
begin
  if FAllocations.Count > 0 then
  begin
    WriteLn('=== FUITES MÉMOIRE DÉTECTÉES ===');
    for I := 0 to FAllocations.Count - 1 do
      WriteLn('  - ', FAllocations[I]);
    WriteLn('Total : ', FAllocations.Count, ' objet(s) non libéré(s)');
  end
  else
    WriteLn('Aucune fuite mémoire détectée');
end;

class function TMemoryManager.Instance: TMemoryManager;  
begin
  if not Assigned(FInstance) then
    FInstance := TMemoryManager.Create;
  Result := FInstance;
end;

// Classe tracée
type
  TTrackedObject = class
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TTrackedObject.Create;  
begin
  inherited;
  TMemoryManager.Instance.RegisterAllocation(Self,
    Format('%s créé à %s', [ClassName, DateTimeToStr(Now)]));
end;

destructor TTrackedObject.Destroy;  
begin
  TMemoryManager.Instance.UnregisterAllocation(Self);
  inherited;
end;
{$ENDIF}
```

### Patterns pour éviter les fuites

```pascal
type
  // Pattern RAII (Resource Acquisition Is Initialization)
  TAutoFile = record
  private
    FHandle: THandle;
    FFileName: String;
  public
    procedure Open(const FileName: String);
    procedure Close;
    function Read(var Buffer; Count: Integer): Integer;
    class operator Initialize(var AF: TAutoFile);
    class operator Finalize(var AF: TAutoFile);
  end;

class operator TAutoFile.Initialize(var AF: TAutoFile);  
begin
  AF.FHandle := INVALID_HANDLE_VALUE;
  AF.FFileName := '';
end;

class operator TAutoFile.Finalize(var AF: TAutoFile);  
begin
  AF.Close;
end;

procedure TAutoFile.Open(const FileName: String);  
begin
  Close; // Fermer si déjà ouvert
  FFileName := FileName;
  FHandle := FileOpen(FileName, fmOpenRead);
  if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.Create('Impossible d''ouvrir le fichier');
end;

procedure TAutoFile.Close;  
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    FileClose(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
    WriteLn('Fichier fermé : ', FFileName);
  end;
end;

function TAutoFile.Read(var Buffer; Count: Integer): Integer;  
begin
  if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.Create('Fichier non ouvert');
  Result := FileRead(FHandle, Buffer, Count);
end;

// Utilisation : le fichier est automatiquement fermé
procedure UseAutoFile;  
var
  AF: TAutoFile;
  Buffer: array[0..255] of Byte;
begin
  AF.Open('test.txt');
  AF.Read(Buffer, SizeOf(Buffer));
  // Pas besoin d'appeler Close, c'est automatique
end;
```

## Gestion de la mémoire pour les chaînes et tableaux

### Chaînes avec comptage de références

```pascal
procedure StringMemoryManagement;  
var
  S1, S2, S3: String;
  P: PChar;
begin
  // Les chaînes utilisent le comptage de références
  S1 := 'Bonjour';
  WriteLn('S1 créé : RefCount = 1');

  S2 := S1; // Partage la même mémoire
  WriteLn('S2 := S1 : RefCount = 2');

  S3 := S1;
  WriteLn('S3 := S1 : RefCount = 3');

  // UniqueString force une copie
  UniqueString(S2);
  WriteLn('UniqueString(S2) : S2 a maintenant sa propre copie');

  S2 := S2 + ' monde';
  WriteLn('S2 modifié : ', S2);
  WriteLn('S1 inchangé : ', S1);

  // Attention avec les PChar
  P := PChar(S1); // OK tant que S1 existe
  // Ne pas utiliser P après que S1 soit hors de portée !
end;

// Optimisation des chaînes
procedure StringOptimization;  
var
  S: String;
  SB: TStringBuilder;
  I: Integer;
begin
  // MAUVAIS : concaténation répétée
  S := '';
  for I := 1 to 1000 do
    S := S + IntToStr(I) + ', '; // Réallocation à chaque fois

  // BON : utiliser TStringBuilder
  SB := TStringBuilder.Create;
  try
    for I := 1 to 1000 do
    begin
      SB.Append(IntToStr(I));
      SB.Append(', ');
    end;
    S := SB.ToString;
  finally
    SB.Free;
  end;
end;
```

### Tableaux dynamiques

```pascal
procedure DynamicArrayManagement;  
var
  IntArray: array of Integer;
  ObjectArray: array of TObject;
  I: Integer;
begin
  // Allocation
  SetLength(IntArray, 10);
  WriteLn('Tableau alloué : ', Length(IntArray), ' éléments');

  // Redimensionnement
  SetLength(IntArray, 20);
  WriteLn('Tableau redimensionné : ', Length(IntArray), ' éléments');

  // Libération explicite
  SetLength(IntArray, 0);
  WriteLn('Tableau libéré');

  // ATTENTION avec les tableaux d'objets
  SetLength(ObjectArray, 5);
  for I := 0 to High(ObjectArray) do
    ObjectArray[I] := TObject.Create;

  // Il faut libérer les objets avant de libérer le tableau
  for I := 0 to High(ObjectArray) do
    ObjectArray[I].Free;
  SetLength(ObjectArray, 0);
end;

// Copie de tableaux
procedure ArrayCopying;  
var
  Source, Dest: array of Integer;
  I: Integer;
begin
  SetLength(Source, 5);
  for I := 0 to High(Source) do
    Source[I] := I * 10;

  // Copie par référence (partage la mémoire)
  Dest := Source;
  Dest[0] := 999;
  WriteLn('Source[0] = ', Source[0]); // Affiche 999 !

  // Copie profonde
  Dest := Copy(Source);
  Dest[0] := 111;
  WriteLn('Source[0] = ', Source[0]); // Affiche toujours 999
  WriteLn('Dest[0] = ', Dest[0]);     // Affiche 111
end;
```

## Techniques avancées de gestion mémoire

### Pool d'objets

```pascal
type
  // Pool d'objets pour éviter les allocations/désallocations répétées
  generic TObjectPool<T: class, constructor> = class
  private
    FAvailable: TStack;
    FAll: TList;
    FMaxSize: Integer;
  public
    constructor Create(AMaxSize: Integer = 100);
    destructor Destroy; override;

    function Acquire: T;
    procedure Release(Obj: T);
    procedure Clear;
  end;

constructor TObjectPool<T>.Create(AMaxSize: Integer);  
begin
  inherited Create;
  FAvailable := TStack.Create;
  FAll := TList.Create;
  FMaxSize := AMaxSize;
end;

destructor TObjectPool<T>.Destroy;  
begin
  Clear;
  FAvailable.Free;
  FAll.Free;
  inherited;
end;

function TObjectPool<T>.Acquire: T;  
begin
  if FAvailable.Count > 0 then
    Result := T(FAvailable.Pop)
  else
  begin
    Result := T.Create;
    FAll.Add(Result);
  end;
end;

procedure TObjectPool<T>.Release(Obj: T);  
begin
  if Assigned(Obj) and (FAvailable.Count < FMaxSize) then
  begin
    // Réinitialiser l'objet si nécessaire
    FAvailable.Push(Obj);
  end
  else
    Obj.Free;
end;

procedure TObjectPool<T>.Clear;  
var
  I: Integer;
begin
  FAvailable.Clear;
  for I := 0 to FAll.Count - 1 do
    TObject(FAll[I]).Free;
  FAll.Clear;
end;

// Utilisation du pool
type
  TExpensiveObject = class
    Data: array[0..1023] of Byte;
    constructor Create;
    procedure Reset;
  end;

constructor TExpensiveObject.Create;  
begin
  inherited;
  WriteLn('TExpensiveObject créé (coûteux!)');
end;

procedure TExpensiveObject.Reset;  
begin
  FillChar(Data, SizeOf(Data), 0);
end;

procedure UseObjectPool;  
var
  Pool: specialize TObjectPool<TExpensiveObject>;
  Obj1, Obj2: TExpensiveObject;
begin
  Pool := specialize TObjectPool<TExpensiveObject>.Create(10);
  try
    // Premier acquire : création
    Obj1 := Pool.Acquire;
    WriteLn('Obj1 acquis');

    // Release : retour au pool
    Pool.Release(Obj1);
    WriteLn('Obj1 relâché');

    // Deuxième acquire : réutilisation
    Obj2 := Pool.Acquire;
    WriteLn('Obj2 acquis (réutilisé!)');

    Pool.Release(Obj2);
  finally
    Pool.Free;
  end;
end;
```

### Weak references (références faibles)

```pascal
type
  // Référence faible pour éviter les références circulaires
  IWeakReference = interface
    function IsAlive: Boolean;
    function GetTarget: TObject;
  end;

  TWeakReference = class(TInterfacedObject, IWeakReference)
  private
    FTarget: Pointer;
    FTargetClass: TClass;
  public
    constructor Create(ATarget: TObject);
    function IsAlive: Boolean;
    function GetTarget: TObject;
  end;

constructor TWeakReference.Create(ATarget: TObject);  
begin
  inherited Create;
  FTarget := ATarget;
  FTargetClass := ATarget.ClassType;
end;

function TWeakReference.IsAlive: Boolean;  
begin
  // Vérification basique - en production, utilisez une méthode plus robuste
  Result := Assigned(FTarget);
  if Result then
  begin
    try
      Result := TObject(FTarget).ClassType = FTargetClass;
    except
      Result := False;
      FTarget := nil;
    end;
  end;
end;

function TWeakReference.GetTarget: TObject;  
begin
  if IsAlive then
    Result := TObject(FTarget)
  else
    Result := nil;
end;

// Exemple d'utilisation pour éviter les références circulaires
type
  TParent = class;

  TChild = class
  private
    FParentRef: IWeakReference; // Référence faible vers le parent
  public
    constructor Create(AParent: TParent);
    function GetParent: TParent;
  end;

  TParent = class(TInterfacedObject)
  private
    FChildren: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChild(Child: TChild);
  end;

constructor TChild.Create(AParent: TParent);  
begin
  inherited Create;
  FParentRef := TWeakReference.Create(AParent);
end;

function TChild.GetParent: TParent;  
var
  Obj: TObject;
begin
  if FParentRef.IsAlive then
  begin
    Obj := FParentRef.GetTarget;
    if Obj is TParent then
      Result := TParent(Obj)
    else
      Result := nil;
  end
  else
    Result := nil;
end;

constructor TParent.Create;  
begin
  inherited;
  FChildren := TList.Create;
end;

destructor TParent.Destroy;  
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    TObject(FChildren[I]).Free;
  FChildren.Free;
  inherited;
end;

procedure TParent.AddChild(Child: TChild);  
begin
  FChildren.Add(Child);
end;
```

## Bonnes pratiques et conseils

### Règles d'or de la gestion mémoire

```pascal
// 1. TOUJOURS utiliser try-finally
procedure Rule1_TryFinally;  
var
  Obj: TObject;
begin
  Obj := TObject.Create;
  try
    // Utiliser Obj
  finally
    Obj.Free; // Garantit la libération même en cas d'exception
  end;
end;

// 2. Initialiser les pointeurs à nil
procedure Rule2_InitializeNil;  
var
  Obj1, Obj2: TObject;
begin
  Obj1 := nil;
  Obj2 := nil;
  try
    Obj1 := TObject.Create;
    // Si cette ligne échoue, Obj2 reste nil
    Obj2 := TObject.Create;
  finally
    // Free vérifie automatiquement si l'objet est nil
    Obj2.Free;
    Obj1.Free;
  end;
end;

// 3. Un seul propriétaire par objet
type
  TOwnershipDemo = class
  private
    FOwnedObject: TObject;
    FReferencedObject: TObject; // Ne pas libérer
  public
    constructor Create(AOwned: TObject; AReferenced: TObject);
    destructor Destroy; override;
  end;

constructor TOwnershipDemo.Create(AOwned: TObject; AReferenced: TObject);  
begin
  inherited Create;
  FOwnedObject := AOwned; // Nous possédons cet objet
  FReferencedObject := AReferenced; // Nous ne faisons que référencer
end;

destructor TOwnershipDemo.Destroy;  
begin
  FOwnedObject.Free; // Libérer ce qu'on possède
  // Ne PAS libérer FReferencedObject !
  inherited;
end;

// 4. FreeAndNil pour éviter les pointeurs pendants
procedure Rule4_FreeAndNil;  
var
  Obj: TObject;
begin
  Obj := TObject.Create;
  try
    // Utilisation...
  finally
    FreeAndNil(Obj); // Libère ET met à nil
  end;

  // Maintenant on peut tester en toute sécurité
  if Assigned(Obj) then
    WriteLn('Ceci ne sera jamais affiché');
end;

// 5. Éviter les références circulaires
type
  TNode = class
  private
    FParent: TNode;      // Référence faible (ne pas libérer)
    FChildren: TList;    // Références fortes (à libérer)
  public
    constructor Create(AParent: TNode = nil);
    destructor Destroy; override;
    procedure AddChild(Child: TNode);
  end;

constructor TNode.Create(AParent: TNode);  
begin
  inherited Create;
  FParent := AParent;
  FChildren := TList.Create;
  if Assigned(FParent) then
    FParent.AddChild(Self);
end;

destructor TNode.Destroy;  
var
  I: Integer;
begin
  // Libérer seulement les enfants, pas le parent
  for I := 0 to FChildren.Count - 1 do
    TNode(FChildren[I]).Free;
  FChildren.Free;
  inherited;
end;

procedure TNode.AddChild(Child: TNode);  
begin
  FChildren.Add(Child);
end;

// 6. Gérer correctement les exceptions
procedure Rule6_ExceptionHandling;  
var
  Obj1, Obj2, Obj3: TObject;
begin
  Obj1 := nil;
  Obj2 := nil;
  Obj3 := nil;

  try
    Obj1 := TObject.Create;
    try
      Obj2 := TObject.Create;
      try
        Obj3 := TObject.Create;
        // Utilisation des trois objets
        // Si une exception se produit ici, tout sera libéré correctement
      finally
        Obj3.Free;
      end;
    finally
      Obj2.Free;
    end;
  finally
    Obj1.Free;
  end;
end;
```

### Patterns de gestion mémoire sûrs

```pascal
// Pattern Guard Object
type
  TGuard = class
  private
    FObject: TObject;
  public
    constructor Create(AObject: TObject);
    destructor Destroy; override;
    property Obj: TObject read FObject;
  end;

constructor TGuard.Create(AObject: TObject);  
begin
  inherited Create;
  FObject := AObject;
end;

destructor TGuard.Destroy;  
begin
  FObject.Free;
  inherited;
end;

procedure UseGuard;  
var
  Guard: TGuard;
begin
  Guard := TGuard.Create(TStringList.Create);
  try
    // Utiliser Guard.Obj
    TStringList(Guard.Obj).Add('Test');
    // L'objet sera automatiquement libéré via le Guard
  finally
    Guard.Free;
  end;
end;

// Pattern Factory avec gestion de propriété
type
  TObjectFactory = class
  private
    FCreatedObjects: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateObject<T: class, constructor>: T;
    procedure ReleaseObject(Obj: TObject);
    procedure ReleaseAll;
  end;

constructor TObjectFactory.Create;  
begin
  inherited;
  FCreatedObjects := TList.Create;
end;

destructor TObjectFactory.Destroy;  
begin
  ReleaseAll;
  FCreatedObjects.Free;
  inherited;
end;

function TObjectFactory.CreateObject<T>: T;  
begin
  Result := T.Create;
  FCreatedObjects.Add(Result);
end;

procedure TObjectFactory.ReleaseObject(Obj: TObject);  
var
  Index: Integer;
begin
  Index := FCreatedObjects.IndexOf(Obj);
  if Index >= 0 then
  begin
    FCreatedObjects.Delete(Index);
    Obj.Free;
  end;
end;

procedure TObjectFactory.ReleaseAll;  
var
  I: Integer;
begin
  for I := FCreatedObjects.Count - 1 downto 0 do
    TObject(FCreatedObjects[I]).Free;
  FCreatedObjects.Clear;
end;
```

## Débogage de la mémoire

### Outils et techniques de détection

```pascal
// Activation du rapport de fuites mémoire
{$IFDEF DEBUG}
  {$DEFINE CHECKPOINTER}
{$ENDIF}

uses
  {$IFDEF CHECKPOINTER}
  HeapTrc,  // Pour FPC
  {$ENDIF}
  SysUtils;

// Configuration au démarrage du programme
initialization
  {$IFDEF CHECKPOINTER}
  // Configurer HeapTrc pour FPC
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF}

// Classe pour tracer les allocations
type
  TMemoryTracker = class
  private
    type
      TAllocationInfo = record
        Size: Integer;
        TimeStamp: TDateTime;
        StackTrace: String;
      end;

  private
    FAllocations: TDictionary<Pointer, TAllocationInfo>;
    FTotalAllocated: Int64;
    FPeakMemory: Int64;

  public
    constructor Create;
    destructor Destroy; override;

    procedure TrackAllocation(P: Pointer; Size: Integer);
    procedure TrackDeallocation(P: Pointer);
    procedure PrintStatistics;
    function GetLeaks: TStringList;
  end;

constructor TMemoryTracker.Create;  
begin
  inherited;
  FAllocations := TDictionary<Pointer, TAllocationInfo>.Create;
  FTotalAllocated := 0;
  FPeakMemory := 0;
end;

destructor TMemoryTracker.Destroy;  
var
  Leaks: TStringList;
begin
  Leaks := GetLeaks;
  try
    if Leaks.Count > 0 then
    begin
      WriteLn('=== FUITES MÉMOIRE DÉTECTÉES ===');
      WriteLn(Leaks.Text);
    end;
  finally
    Leaks.Free;
  end;

  FAllocations.Free;
  inherited;
end;

procedure TMemoryTracker.TrackAllocation(P: Pointer; Size: Integer);  
var
  Info: TAllocationInfo;
begin
  Info.Size := Size;
  Info.TimeStamp := Now;
  Info.StackTrace := ''; // Capturer la pile d'appels si nécessaire

  FAllocations.Add(P, Info);
  Inc(FTotalAllocated, Size);

  if FTotalAllocated > FPeakMemory then
    FPeakMemory := FTotalAllocated;
end;

procedure TMemoryTracker.TrackDeallocation(P: Pointer);  
var
  Info: TAllocationInfo;
begin
  if FAllocations.TryGetValue(P, Info) then
  begin
    Dec(FTotalAllocated, Info.Size);
    FAllocations.Remove(P);
  end;
end;

procedure TMemoryTracker.PrintStatistics;  
begin
  WriteLn('=== Statistiques Mémoire ===');
  WriteLn('Mémoire actuellement allouée : ', FTotalAllocated, ' octets');
  WriteLn('Pic mémoire : ', FPeakMemory, ' octets');
  WriteLn('Nombre d''allocations actives : ', FAllocations.Count);
end;

function TMemoryTracker.GetLeaks: TStringList;  
var
  Pair: TPair<Pointer, TAllocationInfo>;
begin
  Result := TStringList.Create;
  for Pair in FAllocations do
  begin
    Result.Add(Format('Fuite : %d octets alloués à %s',
      [Pair.Value.Size, DateTimeToStr(Pair.Value.TimeStamp)]));
  end;
end;
```

### Tests de stress mémoire

```pascal
// Test de stress pour détecter les fuites
procedure MemoryStressTest;  
var
  I: Integer;
  Objects: array of TObject;
  StartMem, EndMem: Int64;
begin
  StartMem := GetHeapStatus.TotalAllocated;
  WriteLn('Mémoire initiale : ', StartMem);

  // Test 1 : Allocations/libérations répétées
  WriteLn('Test 1 : 10000 allocations/libérations...');
  for I := 1 to 10000 do
  begin
    SetLength(Objects, 100);
    SetLength(Objects, 0);
  end;

  // Test 2 : Création/destruction d'objets
  WriteLn('Test 2 : Création/destruction d''objets...');
  for I := 1 to 10000 do
  begin
    with TStringList.Create do
    try
      Add('Test');
      Clear;
    finally
      Free;
    end;
  end;

  EndMem := GetHeapStatus.TotalAllocated;
  WriteLn('Mémoire finale : ', EndMem);
  WriteLn('Différence : ', EndMem - StartMem, ' octets');

  if EndMem > StartMem + 1024 then // Tolérance de 1KB
    WriteLn('ATTENTION : Possible fuite mémoire!')
  else
    WriteLn('OK : Pas de fuite significative détectée');
end;

// Fonction pour surveiller l'utilisation mémoire
function GetMemoryInfo: String;  
var
  Status: THeapStatus;
begin
  Status := GetHeapStatus;
  Result := Format('Utilisé: %d KB, Libre: %d KB, Total: %d KB',
    [Status.TotalAllocated div 1024,
     Status.TotalFree div 1024,
     (Status.TotalAllocated + Status.TotalFree) div 1024]);
end;
```

## Optimisation de la mémoire

### Techniques pour réduire l'utilisation mémoire

```pascal
// 1. Utilisation de types appropriés
type
  // MAUVAIS : gaspillage de mémoire
  TBadRecord = record
    Flag: Boolean;     // 1 octet + padding
    Value: Integer;    // 4 octets
    SmallNum: Byte;    // 1 octet + padding
  end; // Total : potentiellement 12 octets avec padding

  // BON : optimisé pour la taille
  TGoodRecord = packed record
    Value: Integer;    // 4 octets
    SmallNum: Byte;    // 1 octet
    Flag: Boolean;     // 1 octet
  end; // Total : 6 octets exactement

// 2. Partage de données avec Copy-on-Write
type
  TCowString = class
  private
    FData: String;
    FRefCount: Integer;
    FIsUnique: Boolean;

    procedure MakeUnique;
  public
    constructor Create(const S: String);
    function GetData: String;
    procedure SetData(const S: String);
    procedure Append(const S: String);
  end;

procedure TCowString.MakeUnique;  
begin
  if not FIsUnique then
  begin
    FData := Copy(FData); // Copie profonde
    FIsUnique := True;
    FRefCount := 1;
  end;
end;

constructor TCowString.Create(const S: String);  
begin
  inherited Create;
  FData := S;
  FRefCount := 1;
  FIsUnique := True;
end;

function TCowString.GetData: String;  
begin
  Result := FData; // Lecture sans copie
end;

procedure TCowString.SetData(const S: String);  
begin
  MakeUnique; // Copie seulement si nécessaire
  FData := S;
end;

procedure TCowString.Append(const S: String);  
begin
  MakeUnique; // Copie avant modification
  FData := FData + S;
end;

// 3. Lazy Loading (chargement différé)
type
  TLazyLoader<T: class, constructor> = class
  private
    FInstance: T;
    FLoaded: Boolean;
    function GetInstance: T;
  public
    destructor Destroy; override;
    property Instance: T read GetInstance;
  end;

function TLazyLoader<T>.GetInstance: T;  
begin
  if not FLoaded then
  begin
    FInstance := T.Create;
    FLoaded := True;
    WriteLn('Instance créée à la demande');
  end;
  Result := FInstance;
end;

destructor TLazyLoader<T>.Destroy;  
begin
  if FLoaded then
    FInstance.Free;
  inherited;
end;
```

### Compression et compactage de données

```pascal
// Utilisation de bits pour économiser la mémoire
type
  TBitFlags = class
  private
    FData: array of Byte;
    function GetBit(Index: Integer): Boolean;
    procedure SetBit(Index: Integer; Value: Boolean);
  public
    constructor Create(BitCount: Integer);
    property Bits[Index: Integer]: Boolean read GetBit write SetBit; default;
  end;

constructor TBitFlags.Create(BitCount: Integer);  
begin
  inherited Create;
  SetLength(FData, (BitCount + 7) div 8);
end;

function TBitFlags.GetBit(Index: Integer): Boolean;  
var
  ByteIndex, BitIndex: Integer;
begin
  ByteIndex := Index div 8;
  BitIndex := Index mod 8;
  Result := (FData[ByteIndex] and (1 shl BitIndex)) <> 0;
end;

procedure TBitFlags.SetBit(Index: Integer; Value: Boolean);  
var
  ByteIndex, BitIndex: Integer;
begin
  ByteIndex := Index div 8;
  BitIndex := Index mod 8;

  if Value then
    FData[ByteIndex] := FData[ByteIndex] or (1 shl BitIndex)
  else
    FData[ByteIndex] := FData[ByteIndex] and not (1 shl BitIndex);
end;

// Comparaison d'utilisation mémoire
procedure CompareMemoryUsage;  
var
  BoolArray: array[0..999] of Boolean; // 1000 octets
  BitFlags: TBitFlags;                 // 125 octets seulement !
begin
  WriteLn('Array de Boolean : ', SizeOf(BoolArray), ' octets');

  BitFlags := TBitFlags.Create(1000);
  try
    WriteLn('BitFlags : environ 125 octets');
    WriteLn('Économie : environ ', SizeOf(BoolArray) - 125, ' octets');
  finally
    BitFlags.Free;
  end;
end;
```

## Gestion mémoire multithread

### Synchronisation et sécurité thread

```pascal
uses
  SyncObjs;

type
  // Pool d'objets thread-safe
  TThreadSafePool<T: class, constructor> = class
  private
    FPool: TStack;
    FLock: TCriticalSection;
    FMaxSize: Integer;
    FSemaphore: TSemaphore;
  public
    constructor Create(AMaxSize: Integer);
    destructor Destroy; override;

    function Acquire: T;
    procedure Release(Item: T);
  end;

constructor TThreadSafePool<T>.Create(AMaxSize: Integer);  
begin
  inherited Create;
  FPool := TStack.Create;
  FLock := TCriticalSection.Create;
  FMaxSize := AMaxSize;
  FSemaphore := TSemaphore.Create(nil, AMaxSize, AMaxSize, '');
end;

destructor TThreadSafePool<T>.Destroy;  
var
  Item: T;
begin
  FLock.Enter;
  try
    while FPool.Count > 0 do
    begin
      Item := T(FPool.Pop);
      Item.Free;
    end;
  finally
    FLock.Leave;
  end;

  FPool.Free;
  FLock.Free;
  FSemaphore.Free;
  inherited;
end;

function TThreadSafePool<T>.Acquire: T;  
begin
  FSemaphore.WaitFor(INFINITE);

  FLock.Enter;
  try
    if FPool.Count > 0 then
      Result := T(FPool.Pop)
    else
      Result := T.Create;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafePool<T>.Release(Item: T);  
begin
  if not Assigned(Item) then
    Exit;

  FLock.Enter;
  try
    if FPool.Count < FMaxSize then
    begin
      FPool.Push(Item);
      FSemaphore.Release;
    end
    else
      Item.Free;
  finally
    FLock.Leave;
  end;
end;

// Allocateur thread-local
threadvar
  ThreadLocalBuffer: array[0..1023] of Byte;
  ThreadLocalOffset: Integer;

function ThreadLocalAlloc(Size: Integer): Pointer;  
begin
  if ThreadLocalOffset + Size > Length(ThreadLocalBuffer) then
    ThreadLocalOffset := 0; // Réinitialiser (simple exemple)

  Result := @ThreadLocalBuffer[ThreadLocalOffset];
  Inc(ThreadLocalOffset, Size);
end;

procedure ThreadLocalReset;  
begin
  ThreadLocalOffset := 0;
end;
```

### Éviter les contentions mémoire

```pascal
type
  // Structure alignée sur la ligne de cache pour éviter le false sharing
  TCacheLineAligned = record
  case Integer of
    0: (Value: Integer);
    1: (Padding: array[0..63] of Byte); // 64 octets = taille ligne cache
  end;

  // Compteurs par thread pour éviter la contention
  TPerThreadCounter = class
  private
    FCounters: array of TCacheLineAligned;
    FThreadCount: Integer;

    function GetThreadIndex: Integer;
  public
    constructor Create(AThreadCount: Integer);
    procedure Increment;
    function GetTotal: Int64;
  end;

constructor TPerThreadCounter.Create(AThreadCount: Integer);  
begin
  inherited Create;
  FThreadCount := AThreadCount;
  SetLength(FCounters, AThreadCount);
end;

function TPerThreadCounter.GetThreadIndex: Integer;  
begin
  // Simplification : utiliser l'ID du thread modulo le nombre de threads
  Result := GetCurrentThreadId mod FThreadCount;
end;

procedure TPerThreadCounter.Increment;  
var
  Index: Integer;
begin
  Index := GetThreadIndex;
  InterlockedIncrement(FCounters[Index].Value);
end;

function TPerThreadCounter.GetTotal: Int64;  
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FThreadCount - 1 do
    Result := Result + FCounters[I].Value;
end;
```

## Cas pratiques et exemples

### Gestionnaire de cache avec expiration

```pascal
type
  TCacheItem<T> = record
    Data: T;
    Expiration: TDateTime;
    Hits: Integer;
  end;

  TSmartCache<TKey, TValue> = class
  private
    FItems: TDictionary<TKey, TCacheItem<TValue>>;
    FMaxSize: Integer;
    FDefaultTTL: Integer; // Time To Live en secondes
    FLock: TMultiReadExclusiveWriteSynchronizer;

    procedure EvictLRU;
    procedure CleanExpired;
  public
    constructor Create(AMaxSize: Integer; ATTL: Integer);
    destructor Destroy; override;

    procedure Put(const Key: TKey; const Value: TValue);
    function TryGet(const Key: TKey; out Value: TValue): Boolean;
    procedure Clear;
  end;

constructor TSmartCache<TKey, TValue>.Create(AMaxSize: Integer; ATTL: Integer);  
begin
  inherited Create;
  FItems := TDictionary<TKey, TCacheItem<TValue>>.Create;
  FMaxSize := AMaxSize;
  FDefaultTTL := ATTL;
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TSmartCache<TKey, TValue>.Destroy;  
begin
  Clear;
  FItems.Free;
  FLock.Free;
  inherited;
end;

procedure TSmartCache<TKey, TValue>.Put(const Key: TKey; const Value: TValue);  
var
  Item: TCacheItem<TValue>;
begin
  FLock.BeginWrite;
  try
    // Nettoyer les éléments expirés
    CleanExpired;

    // Éviction si nécessaire
    if FItems.Count >= FMaxSize then
      EvictLRU;

    Item.Data := Value;
    Item.Expiration := Now + (FDefaultTTL / 86400); // Convertir secondes en jours
    Item.Hits := 0;

    FItems.AddOrSetValue(Key, Item);
  finally
    FLock.EndWrite;
  end;
end;

function TSmartCache<TKey, TValue>.TryGet(const Key: TKey;
                                          out Value: TValue): Boolean;
var
  Item: TCacheItem<TValue>;
begin
  FLock.BeginRead;
  try
    Result := FItems.TryGetValue(Key, Item);
    if Result then
    begin
      if Item.Expiration < Now then
      begin
        Result := False;
        FLock.EndRead;
        FLock.BeginWrite;
        try
          FItems.Remove(Key);
        finally
          FLock.EndWrite;
          FLock.BeginRead;
        end;
      end
      else
      begin
        Value := Item.Data;
        Inc(Item.Hits);
        FItems[Key] := Item;
      end;
    end;
  finally
    FLock.EndRead;
  end;
end;

procedure TSmartCache<TKey, TValue>.EvictLRU;  
var
  MinHits: Integer;
  KeyToRemove: TKey;
  Pair: TPair<TKey, TCacheItem<TValue>>;
  Found: Boolean;
begin
  MinHits := MaxInt;
  Found := False;

  for Pair in FItems do
  begin
    if Pair.Value.Hits < MinHits then
    begin
      MinHits := Pair.Value.Hits;
      KeyToRemove := Pair.Key;
      Found := True;
    end;
  end;

  if Found then
    FItems.Remove(KeyToRemove);
end;

procedure TSmartCache<TKey, TValue>.CleanExpired;  
var
  KeysToRemove: TList<TKey>;
  Pair: TPair<TKey, TCacheItem<TValue>>;
  Key: TKey;
begin
  KeysToRemove := TList<TKey>.Create;
  try
    for Pair in FItems do
    begin
      if Pair.Value.Expiration < Now then
        KeysToRemove.Add(Pair.Key);
    end;

    for Key in KeysToRemove do
      FItems.Remove(Key);
  finally
    KeysToRemove.Free;
  end;
end;

procedure TSmartCache<TKey, TValue>.Clear;  
begin
  FLock.BeginWrite;
  try
    FItems.Clear;
  finally
    FLock.EndWrite;
  end;
end;
```

### Exemple complet : Gestionnaire de ressources

```pascal
type
  // Type de fonction factory pour créer des objets
  TObjectFactory = function: TObject;

  // Gestionnaire de ressources avec comptage de références
  TResourceManager = class
  private
    type
      TResourceInfo = record
        Resource: TObject;
        RefCount: Integer;
        LastAccess: TDateTime;
      end;

  private
    FResources: TDictionary<String, TResourceInfo>;
    FLock: TCriticalSection;
    FMaxResources: Integer;

    procedure CleanupUnused;
  public
    constructor Create(AMaxResources: Integer = 100);
    destructor Destroy; override;

    function AcquireResource(const Name: String;
                           Factory: TObjectFactory): TObject;
    procedure ReleaseResource(const Name: String);
    procedure PrintStatistics;
  end;

constructor TResourceManager.Create(AMaxResources: Integer);  
begin
  inherited Create;
  FResources := TDictionary<String, TResourceInfo>.Create;
  FLock := TCriticalSection.Create;
  FMaxResources := AMaxResources;
end;

destructor TResourceManager.Destroy;  
var
  Info: TResourceInfo;
begin
  FLock.Enter;
  try
    for Info in FResources.Values do
    begin
      if Info.RefCount > 0 then
        WriteLn('ATTENTION : Ressource encore utilisée : RefCount=',
                Info.RefCount);
      Info.Resource.Free;
    end;
    FResources.Clear;
  finally
    FLock.Leave;
  end;

  FResources.Free;
  FLock.Free;
  inherited;
end;

function TResourceManager.AcquireResource(const Name: String;
                                         Factory: TObjectFactory): TObject;
var
  Info: TResourceInfo;
begin
  FLock.Enter;
  try
    if FResources.TryGetValue(Name, Info) then
    begin
      Inc(Info.RefCount);
      Info.LastAccess := Now;
      FResources[Name] := Info;
      Result := Info.Resource;
      WriteLn('Ressource réutilisée : ', Name, ' (RefCount=', Info.RefCount, ')');
    end
    else
    begin
      if FResources.Count >= FMaxResources then
        CleanupUnused;

      Info.Resource := Factory();
      Info.RefCount := 1;
      Info.LastAccess := Now;
      FResources.Add(Name, Info);
      Result := Info.Resource;
      WriteLn('Nouvelle ressource créée : ', Name);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TResourceManager.ReleaseResource(const Name: String);  
var
  Info: TResourceInfo;
begin
  FLock.Enter;
  try
    if FResources.TryGetValue(Name, Info) then
    begin
      Dec(Info.RefCount);
      if Info.RefCount <= 0 then
      begin
        Info.Resource.Free;
        FResources.Remove(Name);
        WriteLn('Ressource libérée : ', Name);
      end
      else
      begin
        FResources[Name] := Info;
        WriteLn('Ressource relâchée : ', Name, ' (RefCount=', Info.RefCount, ')');
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TResourceManager.CleanupUnused;  
var
  Pair: TPair<String, TResourceInfo>;
  ToRemove: TStringList;
  Name: String;
begin
  ToRemove := TStringList.Create;
  try
    // Identifier les ressources non utilisées
    for Pair in FResources do
    begin
      if (Pair.Value.RefCount = 0) and
         (Now - Pair.Value.LastAccess > 1/1440) then // Plus d'une minute
        ToRemove.Add(Pair.Key);
    end;

    // Les supprimer
    for Name in ToRemove do
    begin
      FResources[Name].Resource.Free;
      FResources.Remove(Name);
      WriteLn('Ressource nettoyée : ', Name);
    end;
  finally
    ToRemove.Free;
  end;
end;

procedure TResourceManager.PrintStatistics;  
var
  Pair: TPair<String, TResourceInfo>;
  TotalRefs: Integer;
begin
  FLock.Enter;
  try
    WriteLn('=== Statistiques des ressources ===');
    WriteLn('Nombre de ressources : ', FResources.Count);

    TotalRefs := 0;
    for Pair in FResources do
    begin
      WriteLn('  ', Pair.Key, ': RefCount=', Pair.Value.RefCount,
              ', Dernier accès=', DateTimeToStr(Pair.Value.LastAccess));
      TotalRefs := TotalRefs + Pair.Value.RefCount;
    end;

    WriteLn('Total des références : ', TotalRefs);
  finally
    FLock.Leave;
  end;
end;

// Utilisation
procedure UseResourceManager;  
var
  Manager: TResourceManager;
  Res1, Res2: TObject;
begin
  Manager := TResourceManager.Create(10);
  try
    // Acquérir une ressource
    Res1 := Manager.AcquireResource('Config',
      function: TObject
      begin
        Result := TStringList.Create;
        TStringList(Result).LoadFromFile('config.ini');
      end);

    // Réutiliser la même ressource
    Res2 := Manager.AcquireResource('Config', nil); // Factory ignorée

    // Utiliser les ressources...

    // Libérer
    Manager.ReleaseResource('Config'); // RefCount = 1
    Manager.ReleaseResource('Config'); // RefCount = 0, libérée

    Manager.PrintStatistics;
  finally
    Manager.Free;
  end;
end;
```

## Conclusion

La gestion de la mémoire en FreePascal/Lazarus est un aspect fondamental du développement d'applications robustes et performantes. Voici les points essentiels à retenir :

### Principes fondamentaux

1. **Responsabilité claire** : Chaque allocation doit avoir un propriétaire responsable de sa libération
2. **Try-Finally obligatoire** : Toujours protéger les allocations avec try-finally
3. **Initialisation à nil** : Initialiser les pointeurs pour pouvoir tester leur validité
4. **FreeAndNil** : Utiliser FreeAndNil pour éviter les pointeurs pendants
5. **Un seul propriétaire** : Éviter que plusieurs objets tentent de libérer la même mémoire

### Choix de la stratégie

#### Gestion manuelle
**Quand l'utiliser :**
- Contrôle précis nécessaire
- Performance critique
- Ressources système (fichiers, connexions)
- Compatibilité avec du code existant

**Avantages :**
- Contrôle total
- Prévisibilité
- Pas de surcharge

**Inconvénients :**
- Risque d'erreurs
- Code plus verbeux
- Maintenance plus difficile

#### Comptage de références (Interfaces)
**Quand l'utiliser :**
- Objets partagés entre plusieurs propriétaires
- Durée de vie complexe
- Simplification du code
- APIs publiques

**Avantages :**
- Libération automatique
- Pas de fuites mémoire
- Code plus simple

**Inconvénients :**
- Overhead du comptage
- Références circulaires possibles
- Moins de contrôle

#### Smart Pointers
**Quand l'utiliser :**
- Modernisation de code legacy
- Sécurité supplémentaire
- RAII pattern
- Gestion d'exceptions complexe

**Avantages :**
- Sécurité accrue
- RAII automatique
- Flexible

**Inconvénients :**
- Complexité d'implémentation
- Performance (légère surcharge)
- Courbe d'apprentissage

### Checklist de révision du code

Avant de valider votre code, vérifiez :

- [ ] **Chaque Create a son Free correspondant**
- [ ] **Try-Finally protège toutes les allocations**
- [ ] **Pas de références circulaires**
- [ ] **Les propriétaires sont clairement définis**
- [ ] **FreeAndNil utilisé pour les membres de classe**
- [ ] **Les tableaux d'objets sont correctement libérés**
- [ ] **Les threads ne partagent pas de mémoire non protégée**
- [ ] **Les caches ont une politique d'éviction**
- [ ] **Les pools ont une taille maximale**
- [ ] **Les tests de stress ne montrent pas de fuites**

### Outils recommandés

1. **HeapTrc** (FPC intégré) : Détection de fuites basique
2. **Valgrind** (Linux) : Analyse approfondie
3. **FastMM** (Delphi/FPC) : Gestionnaire de mémoire avancé
4. **Profilers** : Pour identifier les hot spots mémoire

### Erreurs courantes à éviter

```pascal
// ❌ MAUVAIS : Fuite mémoire
procedure Bad1;  
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.Add('Test');
  // Oubli de Free !
end;

// ✅ BON : Libération garantie
procedure Good1;  
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Add('Test');
  finally
    List.Free;
  end;
end;

// ❌ MAUVAIS : Double libération
procedure Bad2;  
var
  Obj: TObject;
  List: TObjectList;
begin
  Obj := TObject.Create;
  List := TObjectList.Create(True); // OwnsObjects = True
  try
    List.Add(Obj);
  finally
    Obj.Free;  // Erreur : sera libéré par List
    List.Free;
  end;
end;

// ✅ BON : Propriétaire unique
procedure Good2;  
var
  Obj: TObject;
  List: TObjectList;
begin
  List := TObjectList.Create(True);
  try
    Obj := TObject.Create;
    List.Add(Obj); // List devient propriétaire
  finally
    List.Free; // Libère List ET Obj
  end;
end;

// ❌ MAUVAIS : Utilisation après libération
procedure Bad3;  
var
  Obj: TObject;
begin
  Obj := TObject.Create;
  Obj.Free;
  WriteLn(Obj.ClassName); // CRASH !
end;

// ✅ BON : FreeAndNil
procedure Good3;  
var
  Obj: TObject;
begin
  Obj := TObject.Create;
  FreeAndNil(Obj);
  if Assigned(Obj) then
    WriteLn(Obj.ClassName); // Ne sera jamais exécuté
end;
```

### Patterns de conception recommandés

1. **Factory Pattern** : Centraliser la création d'objets
2. **Object Pool** : Réutiliser les objets coûteux
3. **RAII** : Lier la durée de vie aux scopes
4. **Weak References** : Éviter les cycles
5. **Smart Pointers** : Gestion automatique moderne

### Métriques de santé mémoire

Surveillez ces indicateurs :

- **Croissance mémoire** : La mémoire augmente-t-elle constamment ?
- **Fragmentation** : Y a-t-il beaucoup de petites allocations ?
- **Pic mémoire** : Quelle est l'utilisation maximale ?
- **Temps de GC** : Pour les parties avec comptage de références
- **Ratio alloc/free** : Les libérations suivent-elles les allocations ?

### Évolution et apprentissage continu

La gestion de la mémoire évolue. Restez informé sur :

- Nouvelles fonctionnalités du compilateur
- Patterns émergents
- Outils de détection améliorés
- Techniques d'optimisation
- Retours d'expérience de la communauté

### Ressources pour approfondir

1. **Documentation FreePascal** sur la gestion mémoire
2. **Forums Lazarus** pour les discussions
3. **Code source de la RTL** pour comprendre l'implémentation
4. **Projets open source** pour voir les bonnes pratiques
5. **Articles et blogs** de la communauté Pascal

### Message final

La maîtrise de la gestion mémoire est ce qui distingue un développeur débutant d'un développeur expérimenté en FreePascal/Lazarus. C'est un investissement qui paie sur le long terme :

- **Applications plus stables** : Moins de crashs et de bugs
- **Meilleures performances** : Utilisation optimale de la mémoire
- **Code maintenable** : Plus facile à comprendre et modifier
- **Confiance accrue** : Vous savez que votre code est solide

Commencez par maîtriser les bases (try-finally, Free), puis progressez vers les techniques avancées (interfaces, smart pointers) selon vos besoins. La pratique régulière et l'analyse de code existant vous aideront à développer les bons réflexes.

Rappelez-vous : **Chaque octet alloué est une responsabilité**, traitez-le avec le respect qu'il mérite !

⏭️ [Inline assembler multi-architecture (x86, x64, ARM)](/03-langage-object-pascal-avance/06-inline-assembler-multi-architecture.md)
