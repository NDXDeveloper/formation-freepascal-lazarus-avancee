🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Généricité avancée et spécialisation en FreePascal/Lazarus

## Introduction : Qu'est-ce que la généricité ?

La généricité (ou *generics* en anglais) est un mécanisme puissant qui permet d'écrire du code réutilisable en créant des classes, des fonctions ou des types qui peuvent travailler avec différents types de données sans avoir à réécrire le code pour chaque type.

Imaginez que vous devez créer une liste qui peut contenir des entiers, puis une autre pour des chaînes de caractères, puis une autre pour des objets... Sans la généricité, vous devriez écrire trois classes différentes. Avec la généricité, vous écrivez une seule classe "modèle" qui s'adapte à n'importe quel type !

## Concepts de base

### Paramètres de type

Un paramètre de type est comme une variable, mais au lieu de contenir une valeur, il représente un type de données. On le note généralement entre chevrons `<>` et on utilise souvent la lettre `T` (pour Type).

```pascal
type
  // Une classe générique simple
  TBox<T> = class
  private
    FValue: T;  // T sera remplacé par le type réel lors de l'utilisation
  public
    property Value: T read FValue write FValue;
  end;
```

### Spécialisation

La spécialisation est le processus de création d'une version concrète d'un type générique en remplaçant le paramètre de type par un type réel.

```pascal
var
  IntegerBox: TBox<Integer>;    // Spécialisation avec Integer
  StringBox: TBox<String>;      // Spécialisation avec String
begin
  IntegerBox := TBox<Integer>.Create;
  IntegerBox.Value := 42;

  StringBox := TBox<String>.Create;
  StringBox.Value := 'Bonjour';
end;
```

## Types génériques avancés

### Classes génériques avec plusieurs paramètres

Vous pouvez utiliser plusieurs paramètres de type dans une même classe :

```pascal
type
  // Une paire clé-valeur générique
  TPair<TKey, TValue> = class
  private
    FKey: TKey;
    FValue: TValue;
  public
    constructor Create(AKey: TKey; AValue: TValue);
    property Key: TKey read FKey;
    property Value: TValue read FValue;
  end;

// Utilisation
var
  PersonAge: TPair<String, Integer>;
begin
  PersonAge := TPair<String, Integer>.Create('Alice', 25);
  WriteLn(PersonAge.Key, ' a ', PersonAge.Value, ' ans');
end;
```

### Records génériques

Les records (structures) peuvent aussi être génériques :

```pascal
type
  TPoint<T> = record
    X, Y: T;
    procedure SetCoordinates(AX, AY: T);
  end;

procedure TPoint<T>.SetCoordinates(AX, AY: T);
begin
  X := AX;
  Y := AY;
end;

// Utilisation
var
  IntPoint: TPoint<Integer>;
  FloatPoint: TPoint<Single>;
begin
  IntPoint.SetCoordinates(10, 20);
  FloatPoint.SetCoordinates(3.14, 2.71);
end;
```

### Tableaux génériques

FreePascal permet de créer des types de tableaux génériques :

```pascal
type
  TDynamicArray<T> = array of T;

  // Classe qui gère un tableau redimensionnable
  TFlexibleArray<T> = class
  private
    FItems: TDynamicArray<T>;
    FCount: Integer;
  public
    procedure Add(const Item: T);
    function Get(Index: Integer): T;
    property Count: Integer read FCount;
  end;

procedure TFlexibleArray<T>.Add(const Item: T);
begin
  SetLength(FItems, FCount + 1);
  FItems[FCount] := Item;
  Inc(FCount);
end;
```

## Contraintes sur les types génériques

Les contraintes permettent de limiter les types qui peuvent être utilisés lors de la spécialisation. Cela garantit que le type utilisé possède certaines caractéristiques.

### Contrainte de classe

Spécifie que le type doit être une classe (ou dériver d'une classe spécifique) :

```pascal
type
  // T doit être une classe dérivée de TObject
  TObjectList<T: class> = class
  private
    FItems: array of T;
  public
    procedure Add(Item: T);
    procedure FreeAll;  // Possible car on sait que T est une classe
  end;

procedure TObjectList<T>.FreeAll;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    if Assigned(FItems[I]) then
      FItems[I].Free;  // On peut appeler Free car T est une classe
end;
```

### Contrainte d'interface

Le type doit implémenter une interface spécifique :

```pascal
type
  IComparable = interface
    function CompareTo(Other: TObject): Integer;
  end;

  // T doit implémenter IComparable
  TSortedList<T: IComparable> = class
  private
    FItems: array of T;
  public
    procedure Add(Item: T);
    procedure Sort;  // Peut utiliser CompareTo pour trier
  end;
```

### Contrainte de constructeur

Indique que le type doit avoir un constructeur sans paramètres :

```pascal
type
  // T doit avoir un constructeur Create sans paramètres
  TFactory<T: constructor> = class
  public
    function CreateNew: T;
  end;

function TFactory<T>.CreateNew: T;
begin
  Result := T.Create;  // Possible grâce à la contrainte
end;
```

### Contraintes multiples

Vous pouvez combiner plusieurs contraintes :

```pascal
type
  // T doit être une classe qui implémente IComparable et a un constructeur
  TAdvancedList<T: class, IComparable, constructor> = class
    // ...
  end;
```

## Méthodes et fonctions génériques

Les méthodes peuvent aussi être génériques, indépendamment de la classe :

```pascal
type
  TUtility = class
  public
    // Méthode générique pour échanger deux valeurs
    class procedure Swap<T>(var A, B: T);

    // Fonction générique pour trouver le maximum
    class function Max<T>(const A, B: T): T;
  end;

class procedure TUtility.Swap<T>(var A, B: T);
var
  Temp: T;
begin
  Temp := A;
  A := B;
  B := Temp;
end;

// Utilisation
var
  X, Y: Integer;
  S1, S2: String;
begin
  X := 10; Y := 20;
  TUtility.Swap<Integer>(X, Y);  // X=20, Y=10

  S1 := 'Hello'; S2 := 'World';
  TUtility.Swap<String>(S1, S2); // S1='World', S2='Hello'
end;
```

## Fonctions génériques autonomes

FreePascal permet aussi de créer des fonctions génériques en dehors des classes :

```pascal
// Fonction générique pour créer un tableau
generic function CreateArray<T>(Size: Integer; DefaultValue: T): array of T;
var
  I: Integer;
begin
  SetLength(Result, Size);
  for I := 0 to Size - 1 do
    Result[I] := DefaultValue;
end;

// Spécialisation explicite
var
  IntArray: array of Integer;
  StrArray: array of String;
begin
  IntArray := specialize CreateArray<Integer>(5, 0);
  StrArray := specialize CreateArray<String>(3, 'vide');
end;
```

## Collections génériques standard

FreePascal/Lazarus fournit plusieurs collections génériques prêtes à l'emploi dans l'unité `Generics.Collections` :

```pascal
uses
  Generics.Collections;

var
  IntList: TList<Integer>;
  StringDict: TDictionary<String, Integer>;
  Queue: TQueue<String>;
begin
  // Liste générique
  IntList := TList<Integer>.Create;
  IntList.Add(10);
  IntList.Add(20);
  IntList.Sort;

  // Dictionnaire générique
  StringDict := TDictionary<String, Integer>.Create;
  StringDict.Add('un', 1);
  StringDict.Add('deux', 2);

  // File d'attente générique
  Queue := TQueue<String>.Create;
  Queue.Enqueue('Premier');
  Queue.Enqueue('Deuxième');
  WriteLn(Queue.Dequeue); // Affiche 'Premier'
end;
```

## Spécialisation partielle

La spécialisation partielle permet de créer une version intermédiaire d'un type générique :

```pascal
type
  // Type générique avec deux paramètres
  TMatrix<TRow, TCol> = class
    // ...
  end;

  // Spécialisation partielle : on fixe un paramètre
  TIntMatrix<TCol> = TMatrix<Integer, TCol>;

  // Utilisation
  TStringIntMatrix = TIntMatrix<String>;
```

## Inférence de type

Dans certains cas, FreePascal peut déduire automatiquement le type sans qu'on ait besoin de le spécifier :

```pascal
type
  THelper = class
    class function CreatePair<T>(Value1, Value2: T): TPair<T, T>;
  end;

var
  IntPair: TPair<Integer, Integer>;
begin
  // Le compilateur déduit que T = Integer
  IntPair := THelper.CreatePair(10, 20);
  // Pas besoin d'écrire : THelper.CreatePair<Integer>(10, 20)
end;
```

## Héritage et généricité

Les classes génériques peuvent hériter d'autres classes génériques :

```pascal
type
  // Classe de base générique
  TContainer<T> = class
  protected
    FItem: T;
  public
    property Item: T read FItem write FItem;
  end;

  // Classe dérivée qui reste générique
  TLabeledContainer<T> = class(TContainer<T>)
  private
    FCaption: String;
  public
    property Caption: String read FCaption write FCaption;
  end;

  // Classe dérivée avec spécialisation
  TIntegerContainer = class(TContainer<Integer>)
  public
    function IsPositive: Boolean;
  end;

function TIntegerContainer.IsPositive: Boolean;
begin
  Result := FItem > 0;
end;
```

## Types imbriqués génériques

Vous pouvez définir des types génériques à l'intérieur d'autres types génériques :

```pascal
type
  TOuterClass<T> = class
  public
    type
      // Type imbriqué qui utilise le paramètre de la classe externe
      TInnerClass = class
      private
        FValue: T;
      public
        property Value: T read FValue write FValue;
      end;

      // Type imbriqué avec son propre paramètre générique
      TInnerGeneric<U> = class
      private
        FFirst: T;
        FSecond: U;
      public
        property First: T read FFirst write FFirst;
        property Second: U read FSecond write FSecond;
      end;
  end;

// Utilisation
var
  Inner: TOuterClass<Integer>.TInnerClass;
  Mixed: TOuterClass<String>.TInnerGeneric<Boolean>;
begin
  Inner := TOuterClass<Integer>.TInnerClass.Create;
  Inner.Value := 42;

  Mixed := TOuterClass<String>.TInnerGeneric<Boolean>.Create;
  Mixed.First := 'Test';
  Mixed.Second := True;
end;
```

## Bonnes pratiques

### Nommage des paramètres de type

- Utilisez `T` pour un type générique unique
- Utilisez des noms descriptifs pour plusieurs paramètres : `TKey`, `TValue`, `TItem`
- Préfixez avec `T` pour indiquer qu'il s'agit d'un type

### Organisation du code

```pascal
type
  // Définissez d'abord les types génériques
  generic TGenericList<T> = class
  private
    FItems: array of T;
    FCount: Integer;
  public
    procedure Add(const Item: T);
    function GetItem(Index: Integer): T;
    property Count: Integer read FCount;
  end;

  // Puis les spécialisations courantes
  TIntegerList = specialize TGenericList<Integer>;
  TStringList = specialize TGenericList<String>;
```

### Gestion de la mémoire

Attention à la gestion mémoire avec les types génériques :

```pascal
type
  TSmartContainer<T> = class
  private
    FItem: T;
    FOwnsItem: Boolean;
  public
    constructor Create(AOwnsItem: Boolean = False);
    destructor Destroy; override;
    property Item: T read FItem write FItem;
  end;

destructor TSmartContainer<T>.Destroy;
begin
  // Libération conditionnelle si T est un objet
  // Nécessite : uses TypInfo;
  if FOwnsItem and (PTypeInfo(TypeInfo(T))^.Kind = tkClass) then
    TObject(FItem).Free;
  inherited;
end;
```

## Limitations et considérations

### Comparaisons dans le code générique

La comparaison directe n'est pas toujours possible avec les types génériques :

```pascal
type
  TComparer<T> = class
  public
    // Utiliser des comparateurs spécialisés
    class function AreEqual(const A, B: T): Boolean;
  end;

class function TComparer<T>.AreEqual(const A, B: T): Boolean;
begin
  // Nécessite une implémentation spécifique selon le type
  // ou l'utilisation d'interfaces de comparaison
  Result := CompareMem(@A, @B, SizeOf(T));
end;
```

### Performance

Les génériques peuvent avoir un impact sur la taille du code compilé car chaque spécialisation génère du code distinct. Cependant, ils offrent généralement de meilleures performances que les alternatives basées sur des pointeurs ou variants.

## Conclusion

La généricité en FreePascal/Lazarus est un outil puissant qui permet d'écrire du code plus réutilisable, plus sûr et plus maintenable. En comprenant les concepts de base et en progressant vers les techniques avancées, vous pouvez créer des bibliothèques et des applications robustes qui s'adaptent à différents types de données sans duplication de code.

Les points clés à retenir :
- Les génériques permettent d'écrire du code une fois pour plusieurs types
- La spécialisation crée des versions concrètes des types génériques
- Les contraintes garantissent que les types utilisés ont les capacités requises
- FreePascal offre des collections génériques prêtes à l'emploi
- L'utilisation appropriée des génériques améliore la qualité et la maintenabilité du code

⏭️ [Types avancés et RTTI (Run-Time Type Information)](/03-langage-object-pascal-avance/03-types-avances-rtti.md)
