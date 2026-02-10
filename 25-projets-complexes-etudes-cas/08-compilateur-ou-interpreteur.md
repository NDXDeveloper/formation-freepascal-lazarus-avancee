🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 25.8 Compilateur ou Interpréteur

## Introduction

La création d'un compilateur ou d'un interpréteur représente l'un des projets les plus ambitieux et enrichissants en programmation. Avec FreePascal et Lazarus, vous disposez d'outils puissants pour réaliser ce type de projet, que ce soit sous Windows ou Ubuntu.

Dans ce chapitre, nous allons explorer comment concevoir et implémenter un langage de programmation simple, en commençant par les concepts fondamentaux jusqu'à la génération de code exécutable.

## Différence entre Compilateur et Interpréteur

### Le Compilateur

Un **compilateur** traduit l'intégralité du code source en code machine (ou bytecode) avant l'exécution. Le processus se déroule en plusieurs phases :

1. **Analyse lexicale** : découpage du texte en tokens
2. **Analyse syntaxique** : construction de l'arbre de syntaxe abstraite (AST)
3. **Analyse sémantique** : vérification des types et de la cohérence
4. **Optimisation** : amélioration du code
5. **Génération de code** : production du code exécutable

**Avantages** :
- Exécution rapide du programme final
- Détection d'erreurs avant l'exécution
- Optimisations possibles

**Inconvénients** :
- Temps de compilation nécessaire
- Plus complexe à développer

### L'Interpréteur

Un **interpréteur** lit et exécute le code source instruction par instruction, sans génération de code machine intermédiaire.

**Avantages** :
- Développement plus simple
- Débogage facilité
- Pas de phase de compilation

**Inconvénients** :
- Exécution plus lente
- Erreurs détectées uniquement à l'exécution

### Approche Hybride

Certains langages utilisent une approche mixte avec compilation vers un bytecode intermédiaire, puis interprétation ou compilation JIT (Just-In-Time).

## Architecture Générale

Voici l'architecture typique d'un compilateur/interpréteur :

```
Code Source
    ↓
[Analyseur Lexical] → Tokens
    ↓
[Analyseur Syntaxique] → AST (Arbre de Syntaxe Abstraite)
    ↓
[Analyseur Sémantique] → AST Validé
    ↓
[Optimiseur] → AST Optimisé
    ↓
[Générateur de Code] → Code Machine / Bytecode
    ↓
Exécution
```

## Phase 1 : L'Analyse Lexicale (Lexer)

L'analyse lexicale transforme une chaîne de caractères en une séquence de tokens (unités lexicales).

### Définition des Tokens

```pascal
type
  TTokenType = (
    // Mots-clés
    tkIf, tkThen, tkElse, tkWhile, tkDo, tkFor, tkTo,
    tkVar, tkConst, tkBegin, tkEnd, tkFunction, tkProcedure,

    // Identificateurs et littéraux
    tkIdentifier, tkNumber, tkString,

    // Opérateurs
    tkPlus, tkMinus, tkMultiply, tkDivide, tkAssign,
    tkEqual, tkNotEqual, tkLess, tkGreater, tkLessEqual, tkGreaterEqual,

    // Délimiteurs
    tkSemicolon, tkComma, tkLeftParen, tkRightParen,
    tkLeftBracket, tkRightBracket, tkDot,

    // Spéciaux
    tkEOF, tkError
  );

  TToken = record
    TokenType: TTokenType;
    Value: string;
    Line: Integer;
    Column: Integer;
  end;
```

### Implémentation du Lexer

```pascal
type
  TLexer = class
  private
    FSource: string;
    FPosition: Integer;
    FLine: Integer;
    FColumn: Integer;

    function CurrentChar: Char;
    function PeekChar(Offset: Integer = 1): Char;
    procedure Advance;
    procedure SkipWhitespace;
    procedure SkipComment;

    function ReadIdentifier: string;
    function ReadNumber: string;
    function ReadString: string;

    function IsKeyword(const Ident: string): TTokenType;
  public
    constructor Create(const ASource: string);
    function NextToken: TToken;
    function PeekToken: TToken;
  end;

constructor TLexer.Create(const ASource: string);  
begin
  FSource := ASource;
  FPosition := 1;
  FLine := 1;
  FColumn := 1;
end;

function TLexer.CurrentChar: Char;  
begin
  if FPosition <= Length(FSource) then
    Result := FSource[FPosition]
  else
    Result := #0;
end;

procedure TLexer.Advance;  
begin
  if CurrentChar = #10 then
  begin
    Inc(FLine);
    FColumn := 1;
  end
  else
    Inc(FColumn);
  Inc(FPosition);
end;

procedure TLexer.SkipWhitespace;  
begin
  while CurrentChar in [' ', #9, #10, #13] do
    Advance;
end;

function TLexer.ReadIdentifier: string;  
begin
  Result := '';
  while CurrentChar in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do
  begin
    Result := Result + CurrentChar;
    Advance;
  end;
end;

function TLexer.ReadNumber: string;  
var
  HasDot: Boolean;
begin
  Result := '';
  HasDot := False;

  while (CurrentChar in ['0'..'9']) or (CurrentChar = '.') do
  begin
    if CurrentChar = '.' then
    begin
      if HasDot then
        Break; // Erreur : deux points décimaux
      HasDot := True;
    end;
    Result := Result + CurrentChar;
    Advance;
  end;
end;

function TLexer.IsKeyword(const Ident: string): TTokenType;  
var
  LowerIdent: string;
begin
  LowerIdent := LowerCase(Ident);

  if LowerIdent = 'if' then Result := tkIf
  else if LowerIdent = 'then' then Result := tkThen
  else if LowerIdent = 'else' then Result := tkElse
  else if LowerIdent = 'while' then Result := tkWhile
  else if LowerIdent = 'do' then Result := tkDo
  else if LowerIdent = 'var' then Result := tkVar
  else if LowerIdent = 'begin' then Result := tkBegin
  else if LowerIdent = 'end' then Result := tkEnd
  else
    Result := tkIdentifier;
end;

function TLexer.NextToken: TToken;  
begin
  SkipWhitespace;

  Result.Line := FLine;
  Result.Column := FColumn;

  case CurrentChar of
    #0:
      Result.TokenType := tkEOF;

    'a'..'z', 'A'..'Z', '_':
    begin
      Result.Value := ReadIdentifier;
      Result.TokenType := IsKeyword(Result.Value);
    end;

    '0'..'9':
    begin
      Result.Value := ReadNumber;
      Result.TokenType := tkNumber;
    end;

    '+':
    begin
      Result.TokenType := tkPlus;
      Advance;
    end;

    '-':
    begin
      Result.TokenType := tkMinus;
      Advance;
    end;

    '*':
    begin
      Result.TokenType := tkMultiply;
      Advance;
    end;

    '/':
    begin
      Result.TokenType := tkDivide;
      Advance;
    end;

    '=':
    begin
      Result.TokenType := tkEqual;
      Advance;
    end;

    ':':
    begin
      Advance;
      if CurrentChar = '=' then
      begin
        Result.TokenType := tkAssign;
        Advance;
      end
      else
        Result.TokenType := tkError;
    end;

    ';':
    begin
      Result.TokenType := tkSemicolon;
      Advance;
    end;

    '(':
    begin
      Result.TokenType := tkLeftParen;
      Advance;
    end;

    ')':
    begin
      Result.TokenType := tkRightParen;
      Advance;
    end;

  else
    Result.TokenType := tkError;
    Advance;
  end;
end;
```

## Phase 2 : L'Analyse Syntaxique (Parser)

Le parser construit un arbre de syntaxe abstraite (AST) à partir des tokens.

### Définition de l'AST

```pascal
type
  TASTNodeType = (
    astProgram, astBlock, astVarDecl, astAssignment,
    astIfStatement, astWhileLoop, astForLoop,
    astFunctionCall, astBinaryOp, astUnaryOp,
    astIdentifier, astLiteral
  );

  TASTNode = class
  public
    NodeType: TASTNodeType;
    Line, Column: Integer;
    destructor Destroy; override;
  end;

  // Nœud pour les expressions binaires (a + b, a * b, etc.)
  TBinaryOpNode = class(TASTNode)
  public
    Operator: TTokenType;
    Left: TASTNode;
    Right: TASTNode;
    constructor Create(AOp: TTokenType; ALeft, ARight: TASTNode);
    destructor Destroy; override;
  end;

  // Nœud pour les littéraux (nombres, chaînes)
  TLiteralNode = class(TASTNode)
  public
    Value: Variant;
    constructor Create(AValue: Variant);
  end;

  // Nœud pour les identificateurs (noms de variables)
  TIdentifierNode = class(TASTNode)
  public
    Name: string;
    constructor Create(const AName: string);
  end;

  // Nœud pour les assignations
  TAssignmentNode = class(TASTNode)
  public
    Variable: string;
    Expression: TASTNode;
    constructor Create(const AVar: string; AExpr: TASTNode);
    destructor Destroy; override;
  end;

  // Nœud pour les blocs (séquence d'instructions)
  TBlockNode = class(TASTNode)
  public
    Statements: TList;
    constructor Create;
    destructor Destroy; override;
  end;

  // Nœud pour les instructions IF
  TIfNode = class(TASTNode)
  public
    Condition: TASTNode;
    ThenBranch: TASTNode;
    ElseBranch: TASTNode;
    constructor Create(ACond, AThen, AElse: TASTNode);
    destructor Destroy; override;
  end;

  // Nœud pour les boucles WHILE
  TWhileNode = class(TASTNode)
  public
    Condition: TASTNode;
    Body: TASTNode;
    constructor Create(ACond, ABody: TASTNode);
    destructor Destroy; override;
  end;
```

### Implémentation du Parser

```pascal
type
  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;

    procedure Eat(ATokenType: TTokenType);
    procedure Error(const Msg: string);

    function ParseProgram: TASTNode;
    function ParseBlock: TASTNode;
    function ParseStatement: TASTNode;
    function ParseAssignment: TASTNode;
    function ParseIfStatement: TASTNode;
    function ParseWhileStatement: TASTNode;
    function ParseExpression: TASTNode;
    function ParseTerm: TASTNode;
    function ParseFactor: TASTNode;
  public
    constructor Create(ALexer: TLexer);
    function Parse: TASTNode;
  end;

constructor TParser.Create(ALexer: TLexer);  
begin
  FLexer := ALexer;
  FCurrentToken := FLexer.NextToken;
end;

procedure TParser.Eat(ATokenType: TTokenType);  
begin
  if FCurrentToken.TokenType = ATokenType then
    FCurrentToken := FLexer.NextToken
  else
    Error(Format('Expected %s but got %s',
      [GetEnumName(TypeInfo(TTokenType), Ord(ATokenType)),
       GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType))]));
end;

procedure TParser.Error(const Msg: string);  
begin
  raise Exception.CreateFmt('Parse error at line %d, column %d: %s',
    [FCurrentToken.Line, FCurrentToken.Column, Msg]);
end;

function TParser.ParseExpression: TASTNode;  
var
  Left: TASTNode;
  Op: TTokenType;
begin
  // Expression = Term (('+' | '-') Term)*
  Result := ParseTerm;

  while FCurrentToken.TokenType in [tkPlus, tkMinus] do
  begin
    Op := FCurrentToken.TokenType;
    Eat(Op);
    Left := Result;
    Result := TBinaryOpNode.Create(Op, Left, ParseTerm);
  end;
end;

function TParser.ParseTerm: TASTNode;  
var
  Left: TASTNode;
  Op: TTokenType;
begin
  // Term = Factor (('*' | '/') Factor)*
  Result := ParseFactor;

  while FCurrentToken.TokenType in [tkMultiply, tkDivide] do
  begin
    Op := FCurrentToken.TokenType;
    Eat(Op);
    Left := Result;
    Result := TBinaryOpNode.Create(Op, Left, ParseFactor);
  end;
end;

function TParser.ParseFactor: TASTNode;  
begin
  // Factor = Number | Identifier | '(' Expression ')'
  case FCurrentToken.TokenType of
    tkNumber:
    begin
      Result := TLiteralNode.Create(StrToFloat(FCurrentToken.Value));
      Eat(tkNumber);
    end;

    tkIdentifier:
    begin
      Result := TIdentifierNode.Create(FCurrentToken.Value);
      Eat(tkIdentifier);
    end;

    tkLeftParen:
    begin
      Eat(tkLeftParen);
      Result := ParseExpression;
      Eat(tkRightParen);
    end;

  else
    Error('Expected number, identifier or (');
    Result := nil;
  end;
end;

function TParser.ParseAssignment: TASTNode;  
var
  VarName: string;
begin
  // Assignment = Identifier ':=' Expression
  VarName := FCurrentToken.Value;
  Eat(tkIdentifier);
  Eat(tkAssign);
  Result := TAssignmentNode.Create(VarName, ParseExpression);
end;

function TParser.ParseIfStatement: TASTNode;  
var
  Condition, ThenBranch, ElseBranch: TASTNode;
begin
  // If = 'if' Expression 'then' Statement ['else' Statement]
  Eat(tkIf);
  Condition := ParseExpression;
  Eat(tkThen);
  ThenBranch := ParseStatement;

  ElseBranch := nil;
  if FCurrentToken.TokenType = tkElse then
  begin
    Eat(tkElse);
    ElseBranch := ParseStatement;
  end;

  Result := TIfNode.Create(Condition, ThenBranch, ElseBranch);
end;

function TParser.ParseWhileStatement: TASTNode;  
var
  Condition, Body: TASTNode;
begin
  // While = 'while' Expression 'do' Statement
  Eat(tkWhile);
  Condition := ParseExpression;
  Eat(tkDo);
  Body := ParseStatement;

  Result := TWhileNode.Create(Condition, Body);
end;

function TParser.Parse: TASTNode;  
begin
  Result := ParseProgram;
end;
```

## Phase 3 : L'Interpréteur

L'interpréteur parcourt l'AST et exécute les instructions.

### Implémentation de l'Interpréteur

> **Note :** Les extraits suivants utilisent `TDictionary<K,V>` et `TList<T>` de `Generics.Collections`, qui nécessitent `{$mode delphi}`.

```pascal
type
  TInterpreter = class
  private
    FVariables: TDictionary<string, Variant>;

    function EvaluateNode(Node: TASTNode): Variant;
    procedure ExecuteStatement(Node: TASTNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(Root: TASTNode);
  end;

constructor TInterpreter.Create;  
begin
  FVariables := TDictionary<string, Variant>.Create;
end;

destructor TInterpreter.Destroy;  
begin
  FVariables.Free;
  inherited;
end;

function TInterpreter.EvaluateNode(Node: TASTNode): Variant;  
var
  BinOp: TBinaryOpNode;
  Left, Right: Variant;
begin
  if Node is TLiteralNode then
  begin
    Result := TLiteralNode(Node).Value;
  end
  else if Node is TIdentifierNode then
  begin
    if not FVariables.TryGetValue(TIdentifierNode(Node).Name, Result) then
      raise Exception.CreateFmt('Variable "%s" not defined',
        [TIdentifierNode(Node).Name]);
  end
  else if Node is TBinaryOpNode then
  begin
    BinOp := TBinaryOpNode(Node);
    Left := EvaluateNode(BinOp.Left);
    Right := EvaluateNode(BinOp.Right);

    case BinOp.Operator of
      tkPlus: Result := Left + Right;
      tkMinus: Result := Left - Right;
      tkMultiply: Result := Left * Right;
      tkDivide: Result := Left / Right;
      tkEqual: Result := Left = Right;
      tkNotEqual: Result := Left <> Right;
      tkLess: Result := Left < Right;
      tkGreater: Result := Left > Right;
    else
      raise Exception.Create('Unknown operator');
    end;
  end
  else
    raise Exception.Create('Cannot evaluate node type');
end;

procedure TInterpreter.ExecuteStatement(Node: TASTNode);  
var
  BlockNode: TBlockNode;
  AssignNode: TAssignmentNode;
  IfNode: TIfNode;
  WhileNode: TWhileNode;
  i: Integer;
begin
  if Node is TBlockNode then
  begin
    BlockNode := TBlockNode(Node);
    for i := 0 to BlockNode.Statements.Count - 1 do
      ExecuteStatement(TASTNode(BlockNode.Statements[i]));
  end
  else if Node is TAssignmentNode then
  begin
    AssignNode := TAssignmentNode(Node);
    FVariables.AddOrSetValue(AssignNode.Variable,
      EvaluateNode(AssignNode.Expression));
  end
  else if Node is TIfNode then
  begin
    IfNode := TIfNode(Node);
    if EvaluateNode(IfNode.Condition) then
      ExecuteStatement(IfNode.ThenBranch)
    else if IfNode.ElseBranch <> nil then
      ExecuteStatement(IfNode.ElseBranch);
  end
  else if Node is TWhileNode then
  begin
    WhileNode := TWhileNode(Node);
    while EvaluateNode(WhileNode.Condition) do
      ExecuteStatement(WhileNode.Body);
  end;
end;

procedure TInterpreter.Execute(Root: TASTNode);  
begin
  ExecuteStatement(Root);
end;
```

## Phase 4 : Le Compilateur vers Bytecode

Pour un compilateur, nous générons du bytecode ou du code machine.

### Définition du Bytecode

```pascal
type
  TOpCode = (
    opPush,      // Empiler une valeur
    opPop,       // Dépiler
    opLoad,      // Charger une variable
    opStore,     // Stocker dans une variable
    opAdd,       // Addition
    opSub,       // Soustraction
    opMul,       // Multiplication
    opDiv,       // Division
    opJmp,       // Saut inconditionnel
    opJmpIfFalse,// Saut conditionnel
    opEqual,     // Comparaison ==
    opLess,      // Comparaison <
    opGreater,   // Comparaison >
    opPrint,     // Afficher
    opHalt       // Arrêter
  );

  TInstruction = record
    OpCode: TOpCode;
    Operand: Variant;
  end;

  TByteCodeGenerator = class
  private
    FCode: TList<TInstruction>;
    FVariables: TDictionary<string, Integer>;
    FVarCount: Integer;

    procedure Emit(AOpCode: TOpCode; AOperand: Variant);
    procedure CompileNode(Node: TASTNode);
    function GetVariableIndex(const Name: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(Root: TASTNode): TArray<TInstruction>;
  end;

constructor TByteCodeGenerator.Create;  
begin
  FCode := TList<TInstruction>.Create;
  FVariables := TDictionary<string, Integer>.Create;
  FVarCount := 0;
end;

procedure TByteCodeGenerator.Emit(AOpCode: TOpCode; AOperand: Variant);  
var
  Instr: TInstruction;
begin
  Instr.OpCode := AOpCode;
  Instr.Operand := AOperand;
  FCode.Add(Instr);
end;

function TByteCodeGenerator.GetVariableIndex(const Name: string): Integer;  
begin
  if not FVariables.TryGetValue(Name, Result) then
  begin
    Result := FVarCount;
    FVariables.Add(Name, Result);
    Inc(FVarCount);
  end;
end;

procedure TByteCodeGenerator.CompileNode(Node: TASTNode);  
var
  BinOp: TBinaryOpNode;
  AssignNode: TAssignmentNode;
  IfNode: TIfNode;
  WhileNode: TWhileNode;
  JmpAddr, LoopAddr: Integer;
begin
  if Node is TLiteralNode then
  begin
    Emit(opPush, TLiteralNode(Node).Value);
  end
  else if Node is TIdentifierNode then
  begin
    Emit(opLoad, GetVariableIndex(TIdentifierNode(Node).Name));
  end
  else if Node is TBinaryOpNode then
  begin
    BinOp := TBinaryOpNode(Node);
    CompileNode(BinOp.Left);
    CompileNode(BinOp.Right);

    case BinOp.Operator of
      tkPlus: Emit(opAdd, Null);
      tkMinus: Emit(opSub, Null);
      tkMultiply: Emit(opMul, Null);
      tkDivide: Emit(opDiv, Null);
      tkEqual: Emit(opEqual, Null);
      tkLess: Emit(opLess, Null);
      tkGreater: Emit(opGreater, Null);
    end;
  end
  else if Node is TAssignmentNode then
  begin
    AssignNode := TAssignmentNode(Node);
    CompileNode(AssignNode.Expression);
    Emit(opStore, GetVariableIndex(AssignNode.Variable));
  end
  else if Node is TIfNode then
  begin
    IfNode := TIfNode(Node);
    CompileNode(IfNode.Condition);
    JmpAddr := FCode.Count;
    Emit(opJmpIfFalse, 0); // On patche l'adresse plus tard

    CompileNode(IfNode.ThenBranch);

    if IfNode.ElseBranch <> nil then
    begin
      // TODO: gérer le else
    end;

    // Patcher l'adresse de saut
    FCode[JmpAddr] := FCode[JmpAddr];
    FCode[JmpAddr].Operand := FCode.Count;
  end;
end;

function TByteCodeGenerator.Compile(Root: TASTNode): TArray<TInstruction>;  
begin
  FCode.Clear;
  CompileNode(Root);
  Emit(opHalt, Null);
  Result := FCode.ToArray;
end;
```

### Machine Virtuelle pour le Bytecode

```pascal
type
  TVirtualMachine = class
  private
    FCode: TArray<TInstruction>;
    FStack: TList<Variant>;
    FVariables: array[0..255] of Variant;
    FPC: Integer; // Program Counter

    procedure Push(Value: Variant);
    function Pop: Variant;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(const ACode: TArray<TInstruction>);
  end;

constructor TVirtualMachine.Create;  
begin
  FStack := TList<Variant>.Create;
end;

destructor TVirtualMachine.Destroy;  
begin
  FStack.Free;
  inherited;
end;

procedure TVirtualMachine.Push(Value: Variant);  
begin
  FStack.Add(Value);
end;

function TVirtualMachine.Pop: Variant;  
begin
  if FStack.Count = 0 then
    raise Exception.Create('Stack underflow');
  Result := FStack.Last;
  FStack.Delete(FStack.Count - 1);
end;

procedure TVirtualMachine.Execute(const ACode: TArray<TInstruction>);  
var
  Instr: TInstruction;
  a, b: Variant;
begin
  FCode := ACode;
  FPC := 0;

  while FPC < Length(FCode) do
  begin
    Instr := FCode[FPC];
    Inc(FPC);

    case Instr.OpCode of
      opPush:
        Push(Instr.Operand);

      opPop:
        Pop;

      opLoad:
        Push(FVariables[Integer(Instr.Operand)]);

      opStore:
        FVariables[Integer(Instr.Operand)] := Pop;

      opAdd:
      begin
        b := Pop;
        a := Pop;
        Push(a + b);
      end;

      opSub:
      begin
        b := Pop;
        a := Pop;
        Push(a - b);
      end;

      opMul:
      begin
        b := Pop;
        a := Pop;
        Push(a * b);
      end;

      opDiv:
      begin
        b := Pop;
        a := Pop;
        Push(a / b);
      end;

      opJmp:
        FPC := Integer(Instr.Operand);

      opJmpIfFalse:
        if not Boolean(Pop) then
          FPC := Integer(Instr.Operand);

      opEqual:
      begin
        b := Pop;
        a := Pop;
        Push(a = b);
      end;

      opPrint:
        WriteLn(Pop);

      opHalt:
        Break;
    end;
  end;
end;
```

## Génération de Code Natif

Pour un vrai compilateur, vous pouvez générer du code assembleur ou utiliser LLVM.

### Utilisation avec LLVM

LLVM est une infrastructure de compilation moderne qui facilite la génération de code natif optimisé.

```pascal
// Exemple conceptuel d'utilisation de LLVM
// (nécessite les bindings LLVM pour FreePascal)

type
  TLLVMCodeGenerator = class
  private
    FContext: LLVMContextRef;
    FModule: LLVMModuleRef;
    FBuilder: LLVMBuilderRef;
  public
    constructor Create(const ModuleName: string);
    destructor Destroy; override;
    procedure GenerateCode(Root: TASTNode);
    procedure EmitToFile(const FileName: string);
  end;
```

### Génération d'Assembleur Direct

Pour un contrôle total, vous pouvez générer directement de l'assembleur :

```pascal
type
  TAsmGenerator = class
  private
    FOutput: TStringList;
    FLabelCount: Integer;

    function NewLabel: string;
    procedure EmitLine(const Code: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GenerateAsm(Root: TASTNode);
    procedure SaveToFile(const FileName: string);
  end;

procedure TAsmGenerator.GenerateAsm(Root: TASTNode);  
begin
  // Génération d'assembleur x86-64
  EmitLine('section .text');
  EmitLine('global _start');
  EmitLine('_start:');

  // Compilation du code
  CompileNode(Root);

  // Sortie du programme
  EmitLine('  mov rax, 60');  // syscall exit
  EmitLine('  xor rdi, rdi'); // code retour 0
  EmitLine('  syscall');
end;
```

## Optimisations

### Optimisation de l'AST

Avant la génération de code, vous pouvez optimiser l'AST :

```pascal
type
  TASTOptimizer = class
  public
    function Optimize(Node: TASTNode): TASTNode;
    function FoldConstants(Node: TASTNode): TASTNode;
    function EliminateDeadCode(Node: TASTNode): TASTNode;
  end;

function TASTOptimizer.FoldConstants(Node: TASTNode): TASTNode;  
var
  BinOp: TBinaryOpNode;
  Left, Right: TLiteralNode;
begin
  if Node is TBinaryOpNode then
  begin
    BinOp := TBinaryOpNode(Node);
    BinOp.Left := FoldConstants(BinOp.Left);
    BinOp.Right := FoldConstants(BinOp.Right);

    // Si les deux opérandes sont des constantes
    if (BinOp.Left is TLiteralNode) and (BinOp.Right is TLiteralNode) then
    begin
      Left := TLiteralNode(BinOp.Left);
      Right := TLiteralNode(BinOp.Right);

      // Calculer la constante au moment de la compilation
      case BinOp.Operator of
        tkPlus:
          Exit(TLiteralNode.Create(Left.Value + Right.Value));
        tkMinus:
          Exit(TLiteralNode.Create(Left.Value - Right.Value));
        tkMultiply:
          Exit(TLiteralNode.Create(Left.Value * Right.Value));
        tkDivide:
          Exit(TLiteralNode.Create(Left.Value / Right.Value));
      end;
    end;
  end;

  Result := Node;
end;
```

### Optimisations du Bytecode

```pascal
type
  TBytecodeOptimizer = class
  public
    function OptimizeCode(const Code: TArray<TInstruction>): TArray<TInstruction>;
    function RemoveDeadCode(const Code: TArray<TInstruction>): TArray<TInstruction>;
    function PeepholeOptimization(const Code: TArray<TInstruction>): TArray<TInstruction>;
  end;

function TBytecodeOptimizer.PeepholeOptimization(
  const Code: TArray<TInstruction>): TArray<TInstruction>;
var
  Optimized: TList<TInstruction>;
  i: Integer;
begin
  Optimized := TList<TInstruction>.Create;
  try
    i := 0;
    while i < Length(Code) do
    begin
      // Pattern: Push X, Push Y, Add → Push (X+Y)
      if (i + 2 < Length(Code)) and
         (Code[i].OpCode = opPush) and
         (Code[i+1].OpCode = opPush) and
         (Code[i+2].OpCode = opAdd) then
      begin
        // Remplacer par une seule instruction Push
        with Optimized.Add do
        begin
          OpCode := opPush;
          Operand := Code[i].Operand + Code[i+1].Operand;
        end;
        Inc(i, 3);
      end
      else
      begin
        Optimized.Add(Code[i]);
        Inc(i);
      end;
    end;

    Result := Optimized.ToArray;
  finally
    Optimized.Free;
  end;
end;
```

## Gestion des Erreurs

Un bon compilateur doit fournir des messages d'erreur clairs et utiles.

### Système de Rapports d'Erreurs

```pascal
type
  TErrorLevel = (elInfo, elWarning, elError, elFatal);

  TCompilerError = record
    Level: TErrorLevel;
    Message: string;
    Line: Integer;
    Column: Integer;
    FileName: string;
  end;

  TErrorReporter = class
  private
    FErrors: TList<TCompilerError>;
    FWarnings: TList<TCompilerError>;
    FHasErrors: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ReportError(const Msg: string; Line, Col: Integer);
    procedure ReportWarning(const Msg: string; Line, Col: Integer);
    procedure ReportInfo(const Msg: string);

    procedure PrintReport;
    function HasErrors: Boolean;
    function ErrorCount: Integer;
    function WarningCount: Integer;
  end;

constructor TErrorReporter.Create;  
begin
  FErrors := TList<TCompilerError>.Create;
  FWarnings := TList<TCompilerError>.Create;
  FHasErrors := False;
end;

procedure TErrorReporter.ReportError(const Msg: string; Line, Col: Integer);  
var
  Error: TCompilerError;
begin
  Error.Level := elError;
  Error.Message := Msg;
  Error.Line := Line;
  Error.Column := Col;
  FErrors.Add(Error);
  FHasErrors := True;
end;

procedure TErrorReporter.PrintReport;  
var
  Error: TCompilerError;
begin
  WriteLn('=== Rapport de Compilation ===');
  WriteLn;

  if FErrors.Count > 0 then
  begin
    WriteLn('Erreurs:');
    for Error in FErrors do
      WriteLn(Format('  [Ligne %d:%d] %s',
        [Error.Line, Error.Column, Error.Message]));
    WriteLn;
  end;

  if FWarnings.Count > 0 then
  begin
    WriteLn('Avertissements:');
    for Error in FWarnings do
      WriteLn(Format('  [Ligne %d:%d] %s',
        [Error.Line, Error.Column, Error.Message]));
    WriteLn;
  end;

  WriteLn(Format('Total: %d erreur(s), %d avertissement(s)',
    [FErrors.Count, FWarnings.Count]));
end;
```

## Analyse Sémantique

L'analyse sémantique vérifie la cohérence du programme (types, portée des variables, etc.).

### Vérificateur de Types

```pascal
type
  TDataType = (dtInteger, dtReal, dtBoolean, dtString, dtVoid, dtUnknown);

  TSymbol = class
  public
    Name: string;
    DataType: TDataType;
    IsConstant: Boolean;
  end;

  TSymbolTable = class
  private
    FSymbols: TDictionary<string, TSymbol>;
    FParent: TSymbolTable;
  public
    constructor Create(AParent: TSymbolTable = nil);
    destructor Destroy; override;

    procedure Define(const Name: string; AType: TDataType);
    function Lookup(const Name: string): TSymbol;
    function LookupLocal(const Name: string): TSymbol;
  end;

  TSemanticAnalyzer = class
  private
    FSymbolTable: TSymbolTable;
    FErrorReporter: TErrorReporter;

    function CheckNode(Node: TASTNode): TDataType;
    procedure CheckAssignment(Node: TAssignmentNode);
    procedure CheckBinaryOp(Node: TBinaryOpNode);
  public
    constructor Create(AErrorReporter: TErrorReporter);
    destructor Destroy; override;

    procedure Analyze(Root: TASTNode);
  end;

constructor TSymbolTable.Create(AParent: TSymbolTable);  
begin
  FSymbols := TDictionary<string, TSymbol>.Create;
  FParent := AParent;
end;

procedure TSymbolTable.Define(const Name: string; AType: TDataType);  
var
  Symbol: TSymbol;
begin
  Symbol := TSymbol.Create;
  Symbol.Name := Name;
  Symbol.DataType := AType;
  FSymbols.Add(Name, Symbol);
end;

function TSymbolTable.Lookup(const Name: string): TSymbol;  
begin
  if not FSymbols.TryGetValue(Name, Result) then
  begin
    if FParent <> nil then
      Result := FParent.Lookup(Name)
    else
      Result := nil;
  end;
end;

procedure TSemanticAnalyzer.CheckBinaryOp(Node: TBinaryOpNode);  
var
  LeftType, RightType: TDataType;
begin
  LeftType := CheckNode(Node.Left);
  RightType := CheckNode(Node.Right);

  // Vérifier la compatibilité des types
  case Node.Operator of
    tkPlus, tkMinus, tkMultiply, tkDivide:
    begin
      if not (LeftType in [dtInteger, dtReal]) or
         not (RightType in [dtInteger, dtReal]) then
        FErrorReporter.ReportError(
          'Les opérateurs arithmétiques nécessitent des nombres',
          Node.Line, Node.Column);
    end;

    tkEqual, tkNotEqual:
    begin
      if LeftType <> RightType then
        FErrorReporter.ReportWarning(
          'Comparaison entre types différents',
          Node.Line, Node.Column);
    end;
  end;
end;
```

## Table des Symboles Avancée

Pour gérer les portées et les fonctions :

```pascal
type
  TSymbolKind = (skVariable, skConstant, skFunction, skParameter);

  TFunctionSymbol = class(TSymbol)
  public
    Parameters: TList<TSymbol>;
    ReturnType: TDataType;
    LocalTable: TSymbolTable;
    constructor Create;
    destructor Destroy; override;
  end;

  TScopeManager = class
  private
    FScopes: TStack<TSymbolTable>;
    FCurrentScope: TSymbolTable;
  public
    constructor Create;
    destructor Destroy; override;

    procedure EnterScope;
    procedure ExitScope;
    function CurrentScope: TSymbolTable;
  end;

constructor TScopeManager.Create;  
begin
  FScopes := TStack<TSymbolTable>.Create;
  FCurrentScope := TSymbolTable.Create(nil);
  FScopes.Push(FCurrentScope);
end;

procedure TScopeManager.EnterScope;  
var
  NewScope: TSymbolTable;
begin
  NewScope := TSymbolTable.Create(FCurrentScope);
  FScopes.Push(NewScope);
  FCurrentScope := NewScope;
end;

procedure TScopeManager.ExitScope;  
begin
  if FScopes.Count > 1 then
  begin
    FCurrentScope := FScopes.Pop;
    FCurrentScope.Free;
    FCurrentScope := FScopes.Peek;
  end;
end;
```

## Exemple Complet d'Utilisation

### Programme Principal

```pascal
program SimpleCompiler;

{$mode delphi}{$H+}

uses
  SysUtils, Classes, Generics.Collections;

var
  SourceCode: string;
  Lexer: TLexer;
  Parser: TParser;
  AST: TASTNode;
  Interpreter: TInterpreter;
  Compiler: TByteCodeGenerator;
  VM: TVirtualMachine;
  ByteCode: TArray<TInstruction>;

begin
  // Code source à compiler
  SourceCode :=
    'x := 10;' + sLineBreak +
    'y := 20;' + sLineBreak +
    'z := x + y * 2;' + sLineBreak +
    'if z > 40 then' + sLineBreak +
    '  result := 1' + sLineBreak +
    'else' + sLineBreak +
    '  result := 0;';

  WriteLn('=== Code Source ===');
  WriteLn(SourceCode);
  WriteLn;

  try
    // Phase 1: Analyse lexicale
    WriteLn('=== Analyse Lexicale ===');
    Lexer := TLexer.Create(SourceCode);
    try
      // Afficher tous les tokens
      while True do
      begin
        var Token := Lexer.NextToken;
        WriteLn(Format('Token: %s, Value: %s, Line: %d',
          [GetEnumName(TypeInfo(TTokenType), Ord(Token.TokenType)),
           Token.Value, Token.Line]));
        if Token.TokenType = tkEOF then
          Break;
      end;
    finally
      Lexer.Free;
    end;
    WriteLn;

    // Phase 2: Analyse syntaxique
    WriteLn('=== Analyse Syntaxique ===');
    Lexer := TLexer.Create(SourceCode);
    Parser := TParser.Create(Lexer);
    try
      AST := Parser.Parse;
      WriteLn('AST construit avec succès!');
    finally
      Parser.Free;
      Lexer.Free;
    end;
    WriteLn;

    // Option 1: Interprétation directe
    WriteLn('=== Interprétation ===');
    Interpreter := TInterpreter.Create;
    try
      Interpreter.Execute(AST);
      WriteLn('Exécution terminée avec succès!');
    finally
      Interpreter.Free;
    end;
    WriteLn;

    // Option 2: Compilation vers bytecode puis exécution
    WriteLn('=== Compilation vers Bytecode ===');
    Compiler := TByteCodeGenerator.Create;
    try
      ByteCode := Compiler.Compile(AST);
      WriteLn(Format('%d instructions générées', [Length(ByteCode)]));

      // Afficher le bytecode
      for var i := 0 to High(ByteCode) do
        WriteLn(Format('%4d: %s %s',
          [i,
           GetEnumName(TypeInfo(TOpCode), Ord(ByteCode[i].OpCode)),
           VarToStr(ByteCode[i].Operand)]));
    finally
      Compiler.Free;
    end;
    WriteLn;

    // Exécution du bytecode
    WriteLn('=== Exécution du Bytecode ===');
    VM := TVirtualMachine.Create;
    try
      VM.Execute(ByteCode);
      WriteLn('Machine virtuelle arrêtée normalement');
    finally
      VM.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('ERREUR: ', E.Message);
      ExitCode := 1;
    end;
  end;

  // Nettoyage
  AST.Free;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Spécificités Multi-plateformes

### Gestion des Chemins (Windows/Ubuntu)

```pascal
function GetCompilerPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := ExtractFilePath(ParamStr(0)) + 'compiler.exe';
  {$ENDIF}
  {$IFDEF UNIX}
  Result := ExtractFilePath(ParamStr(0)) + 'compiler';
  {$ENDIF}
end;

function GetOutputExtension: string;  
begin
  {$IFDEF WINDOWS}
  Result := '.exe';
  {$ENDIF}
  {$IFDEF UNIX}
  Result := '';
  {$ENDIF}
end;
```

### Génération d'Exécutables Natifs

```pascal
procedure CompileToNative(const InputFile, OutputFile: string);  
var
  Process: TProcess;
  AsmFile: string;
begin
  AsmFile := ChangeFileExt(InputFile, '.s');

  // Générer le fichier assembleur
  GenerateAssembly(InputFile, AsmFile);

  // Compiler avec GCC/Clang
  Process := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    Process.Executable := 'gcc.exe';
    Process.Parameters.Add('-m64');
    {$ENDIF}
    {$IFDEF UNIX}
    Process.Executable := 'gcc';
    {$ENDIF}

    Process.Parameters.Add(AsmFile);
    Process.Parameters.Add('-o');
    Process.Parameters.Add(OutputFile);

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    if Process.ExitStatus <> 0 then
      raise Exception.Create('Erreur de compilation native');

  finally
    Process.Free;
  end;
end;
```

## Débogueur Intégré

Un compilateur/interpréteur avancé peut inclure un débogueur :

```pascal
type
  TDebugger = class
  private
    FBreakpoints: TList<Integer>;
    FWatchVariables: TList<string>;
    FStepMode: Boolean;

    procedure OnBreakpoint(Line: Integer);
    procedure ShowVariables;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddBreakpoint(Line: Integer);
    procedure RemoveBreakpoint(Line: Integer);
    procedure AddWatch(const VarName: string);

    procedure StepInto;
    procedure StepOver;
    procedure Continue;
    procedure PrintStackTrace;
  end;

procedure TDebugger.OnBreakpoint(Line: Integer);  
begin
  WriteLn(Format('=== Breakpoint atteint ligne %d ===', [Line]));
  ShowVariables;

  WriteLn('Commandes: (s)tep, (n)ext, (c)ontinue, (p)rint, (q)uit');
  // Attendre commande utilisateur
end;

procedure TDebugger.ShowVariables;  
var
  VarName: string;
begin
  WriteLn('Variables:');
  for VarName in FWatchVariables do
  begin
    // Afficher la valeur de la variable
    WriteLn(Format('  %s = %s', [VarName, GetVariableValue(VarName)]));
  end;
end;
```

## Extensions et Améliorations

### Support de Fonctions

```pascal
type
  TFunctionNode = class(TASTNode)
  public
    Name: string;
    Parameters: TList<TIdentifierNode>;
    ReturnType: TDataType;
    Body: TASTNode;

    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

  TCallNode = class(TASTNode)
  public
    FunctionName: string;
    Arguments: TList<TASTNode>;

    constructor Create(const AFuncName: string);
    destructor Destroy; override;
  end;
```

### Support de Structures/Records

```pascal
type
  TStructNode = class(TASTNode)
  public
    Name: string;
    Fields: TList<TFieldDeclaration>;
  end;

  TFieldAccessNode = class(TASTNode)
  public
    Object: TASTNode;
    FieldName: string;
  end;
```

### Support de Tableaux

```pascal
type
  TArrayAccessNode = class(TASTNode)
  public
    ArrayExpr: TASTNode;
    IndexExpr: TASTNode;
  end;

  TArrayLiteralNode = class(TASTNode)
  public
    Elements: TList<TASTNode>;
  end;
```

## Optimisations Avancées

### Analyse de Flux de Données

```pascal
type
  TDataFlowAnalyzer = class
  private
    FLiveVariables: TDictionary<Integer, TSet<string>>;
    FReachingDefinitions: TDictionary<Integer, TSet<string>>;
  public
    procedure AnalyzeProgram(Root: TASTNode);
    function GetDeadCode: TList<TASTNode>;
    function GetUnusedVariables: TList<string>;
  end;
```

### Allocation de Registres

```pascal
type
  TRegisterAllocator = class
  private
    FAvailableRegisters: TList<string>;
    FVariableToRegister: TDictionary<string, string>;
    FSpillCode: TList<TInstruction>;
  public
    procedure AllocateRegisters(Code: TArray<TInstruction>);
    function GetRegisterForVariable(const VarName: string): string;
  end;
```

### Inline de Fonctions

```pascal
type
  TInliner = class
  public
    function ShouldInline(Func: TFunctionNode): Boolean;
    procedure InlineFunction(CallSite: TCallNode; Func: TFunctionNode);
  end;

function TInliner.ShouldInline(Func: TFunctionNode): Boolean;  
begin
  // Critères d'inlining
  Result := (GetFunctionSize(Func) < 50) and  // Petite fonction
            (GetCallCount(Func) > 3);          // Appelée fréquemment
end;
```

## Gestion de la Mémoire

### Garbage Collector Simple

```pascal
type
  TGarbageCollector = class
  private
    FAllocatedObjects: TList<Pointer>;
    FRootSet: TList<Pointer>;

    procedure Mark(Obj: Pointer);
    procedure Sweep;
  public
    constructor Create;
    destructor Destroy; override;

    function Allocate(Size: Integer): Pointer;
    procedure Collect;
    procedure AddRoot(Ptr: Pointer);
  end;

procedure TGarbageCollector.Collect;  
var
  Root: Pointer;
begin
  // Phase de marquage
  for Root in FRootSet do
    Mark(Root);

  // Phase de balayage
  Sweep;
end;

procedure TGarbageCollector.Sweep;  
var
  i: Integer;
  Obj: Pointer;
begin
  i := 0;
  while i < FAllocatedObjects.Count do
  begin
    Obj := FAllocatedObjects[i];
    if not IsMarked(Obj) then
    begin
      FreeMem(Obj);
      FAllocatedObjects.Delete(i);
    end
    else
    begin
      ClearMark(Obj);
      Inc(i);
    end;
  end;
end;
```

## Tests et Validation

### Suite de Tests

```pascal
procedure RunCompilerTests;  
begin
  TestLexer;
  TestParser;
  TestSemanticAnalysis;
  TestCodeGeneration;
  TestOptimizations;
end;

procedure TestLexer;  
begin
  WriteLn('=== Tests du Lexer ===');

  // Test 1: Tokens simples
  AssertTokenSequence('x + y', [tkIdentifier, tkPlus, tkIdentifier]);

  // Test 2: Nombres
  AssertTokenSequence('123.45', [tkNumber]);

  // Test 3: Mots-clés
  AssertTokenSequence('if then else', [tkIf, tkThen, tkElse]);

  WriteLn('Tous les tests du Lexer réussis!');
end;

procedure AssertTokenSequence(const Source: string;
  Expected: array of TTokenType);
var
  Lexer: TLexer;
  Token: TToken;
  i: Integer;
begin
  Lexer := TLexer.Create(Source);
  try
    for i := 0 to High(Expected) do
    begin
      Token := Lexer.NextToken;
      if Token.TokenType <> Expected[i] then
        raise Exception.CreateFmt(
          'Token attendu: %s, obtenu: %s',
          [GetEnumName(TypeInfo(TTokenType), Ord(Expected[i])),
           GetEnumName(TypeInfo(TTokenType), Ord(Token.TokenType))]);
    end;
  finally
    Lexer.Free;
  end;
end;
```

## Documentation du Langage

Pour un projet complet, documentez votre langage :

### Spécification BNF

```
Program     ::= StatementList  
StatementList ::= Statement (';' Statement)*  
Statement   ::= Assignment | IfStatement | WhileStatement  
Assignment  ::= Identifier ':=' Expression  
IfStatement ::= 'if' Expression 'then' Statement ['else' Statement]  
WhileStatement ::= 'while' Expression 'do' Statement  
Expression  ::= Term (('+' | '-') Term)*  
Term        ::= Factor (('*' | '/') Factor)*  
Factor      ::= Number | Identifier | '(' Expression ')'
```

### Manuel Utilisateur

```markdown
# Manuel du Langage SimpleScript

## Types de Données
- **Integer**: Nombres entiers (-2147483648 à 2147483647)
- **Real**: Nombres à virgule flottante
- **Boolean**: true ou false
- **String**: Chaînes de caractères entre guillemets

## Syntaxe

### Déclaration de Variables
var x: Integer;  
var nom: String;

### Assignation
x := 10;  
nom := "Alice";

### Instructions Conditionnelles
if x > 5 then
  writeln("x est grand")
else
  writeln("x est petit");

### Boucles
while x > 0 do  
begin
  writeln(x);
  x := x - 1;
end;
```

## Conclusion

La création d'un compilateur ou d'un interpréteur est un projet complexe mais extrêmement formateur. Vous avez maintenant les bases pour :

1. **Analyser** du code source (lexer et parser)
2. **Représenter** le code sous forme d'AST
3. **Vérifier** la validité sémantique
4. **Optimiser** le code
5. **Exécuter** via interprétation ou compilation

### Ressources pour Aller Plus Loin

- **Livres recommandés** :
  - "Compilers: Principles, Techniques, and Tools" (Dragon Book)
  - "Engineering a Compiler" de Keith Cooper
  - "Modern Compiler Implementation in ML" de Andrew Appel

- **Outils utiles** :
  - ANTLR pour la génération de parsers
  - LLVM pour la génération de code natif
  - fcl-passrc pour parser du code Pascal

- **Projets inspirants** :
  - FreePascal lui-même (code source disponible)
  - Python (CPython)
  - Lua (interpréteur simple et élégant)

### Prochaines Étapes

1. Implémentez les fonctionnalités de base
2. Ajoutez progressivement des fonctionnalités avancées
3. Écrivez des tests exhaustifs
4. Optimisez les performances
5. Documentez votre langage
6. Partagez votre projet avec la communauté

La création d'un langage de programmation est un voyage passionnant qui vous fera comprendre en profondeur le fonctionnement des langages que vous utilisez au quotidien !

⏭️ [Suite bureautique portable](/25-projets-complexes-etudes-cas/09-suite-bureautique-portable.md)
