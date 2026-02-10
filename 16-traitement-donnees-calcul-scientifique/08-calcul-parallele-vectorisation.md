🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.8 Calcul parallèle et vectorisation

## Introduction

Le calcul parallèle et la vectorisation sont deux techniques complémentaires qui permettent d'exploiter pleinement la puissance des processeurs modernes. Dans ce chapitre, nous allons découvrir comment FreePascal permet d'accélérer considérablement vos calculs scientifiques et de traitement de données.

### Pourquoi c'est important ?

Les processeurs modernes possèdent :
- **Plusieurs cœurs** (généralement 4 à 16 sur les ordinateurs de bureau)
- **Des instructions SIMD** (Single Instruction Multiple Data) qui permettent de traiter plusieurs données en une seule opération

Sans optimisation, votre code n'utilise qu'un seul cœur et traite les données une par une. Avec le calcul parallèle et la vectorisation, vous pouvez multiplier les performances par 10, 20, voire plus !

---

## Partie 1 : Le calcul parallèle

### 1.1 Comprendre le parallélisme

Le calcul parallèle consiste à diviser un travail en plusieurs tâches qui s'exécutent simultanément sur différents cœurs du processeur.

**Analogie simple :** Imaginez que vous devez plier 1000 feuilles de papier. Si vous êtes seul, cela prend du temps. Mais si 4 personnes travaillent ensemble, chacune pliant 250 feuilles, le travail est terminé 4 fois plus vite !

### 1.2 Les threads en FreePascal

FreePascal utilise la classe `TThread` pour créer des threads (fils d'exécution parallèles).

#### Exemple simple : Calcul parallèle d'une somme

```pascal
uses
  Classes, SysUtils;

type
  // Thread qui calcule la somme d'une partie du tableau
  TSumThread = class(TThread)
  private
    FData: array of Double;
    FStart, FEnd: Integer;
    FResult: Double;
  protected
    procedure Execute; override;
  public
    constructor Create(const Data: array of Double; AStart, AEnd: Integer);
    property Result: Double read FResult;
  end;

constructor TSumThread.Create(const Data: array of Double; AStart, AEnd: Integer);  
var
  i: Integer;
begin
  inherited Create(True); // Créé en pause
  FreeOnTerminate := False;
  FStart := AStart;
  FEnd := AEnd;

  // Copie des données
  SetLength(FData, Length(Data));
  for i := 0 to High(Data) do
    FData[i] := Data[i];
end;

procedure TSumThread.Execute;  
var
  i: Integer;
begin
  FResult := 0;
  for i := FStart to FEnd do
    FResult := FResult + FData[i];
end;

// Fonction qui utilise plusieurs threads
function ParallelSum(const Data: array of Double; NumThreads: Integer): Double;  
var
  Threads: array of TSumThread;
  ChunkSize, i, Start, Finish: Integer;
begin
  Result := 0;
  SetLength(Threads, NumThreads);
  ChunkSize := Length(Data) div NumThreads;

  // Création et démarrage des threads
  for i := 0 to NumThreads - 1 do
  begin
    Start := i * ChunkSize;
    if i = NumThreads - 1 then
      Finish := High(Data)
    else
      Finish := Start + ChunkSize - 1;

    Threads[i] := TSumThread.Create(Data, Start, Finish);
    Threads[i].Start;
  end;

  // Attente de la fin et récupération des résultats
  for i := 0 to NumThreads - 1 do
  begin
    Threads[i].WaitFor;
    Result := Result + Threads[i].Result;
    Threads[i].Free;
  end;
end;
```

### 1.3 La bibliothèque MTProcs

FreePascal inclut la bibliothèque **MTProcs** (Multi-Threading Procedures) qui simplifie grandement le calcul parallèle.

#### Installation et utilisation

```pascal
uses
  MTProcs;

// Exemple : Traitement parallèle d'un tableau
procedure ProcessArrayParallel(var Data: array of Double);  
var
  i: Integer;
begin
  ProcThreadPool.DoParallel(
    @ProcessElement,  // Fonction à appeler
    0,                // Index de début
    High(Data),       // Index de fin
    @Data             // Données
  );
end;

// Fonction appliquée à chaque élément
procedure ProcessElement(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);  
var
  Arr: ^array of Double;
begin
  Arr := Data;
  Arr^[Index] := Sqrt(Arr^[Index]) * 2.5;
end;
```

### 1.4 Parallélisation d'une boucle FOR

MTProcs offre une fonction très pratique pour paralléliser automatiquement une boucle :

```pascal
uses
  MTProcs;

var
  Data: array of Double;
  i: Integer;

begin
  SetLength(Data, 1000000);

  // Remplissage des données
  for i := 0 to High(Data) do
    Data[i] := Random * 100;

  // Traitement parallèle
  ProcThreadPool.DoParallelLocalProc(
    procedure(Index: PtrInt; ThreadIndex: PtrInt; Data: Pointer)
    begin
      // Cette procédure s'exécute en parallèle pour chaque index
      TDoubleArray(Data)[Index] := Sqrt(TDoubleArray(Data)[Index]);
    end,
    0, High(Data), @Data
  );
end;
```

### 1.5 Points d'attention

⚠️ **Problèmes courants à éviter :**

1. **Race conditions** : Plusieurs threads modifient la même variable
   ```pascal
   // MAUVAIS - race condition !
   var GlobalSum: Double = 0;

   // Plusieurs threads font :
   GlobalSum := GlobalSum + LocalValue;  // DANGER !
   ```

2. **Solution** : Utiliser des sections critiques ou des variables locales
   ```pascal
   uses
     SyncObjs;

   var
     CriticalSection: TCriticalSection;
     GlobalSum: Double = 0;

   // Dans le thread :
   CriticalSection.Enter;
   try
     GlobalSum := GlobalSum + LocalValue;  // Protégé
   finally
     CriticalSection.Leave;
   end;
   ```

---

## Partie 2 : La vectorisation (SIMD)

### 2.1 Qu'est-ce que SIMD ?

SIMD (Single Instruction Multiple Data) permet d'appliquer une même opération sur plusieurs données simultanément.

**Exemple concret :** Au lieu d'additionner 4 paires de nombres en 4 opérations :
```
a1 + b1 = c1  
a2 + b2 = c2  
a3 + b3 = c3  
a4 + b4 = c4
```

Une instruction SIMD fait les 4 additions EN UNE SEULE OPÉRATION !

### 2.2 Les jeux d'instructions SIMD

Les processeurs modernes supportent plusieurs générations de SIMD :

| Jeu d'instructions | Largeur | Support |
|-------------------|---------|---------|
| SSE | 128 bits | Tous les x86-64 |
| SSE2, SSE3, SSE4 | 128 bits | Très répandu |
| AVX | 256 bits | Processeurs récents (2011+) |
| AVX2 | 256 bits | Processeurs récents (2013+) |
| AVX-512 | 512 bits | Serveurs et très haut de gamme |

### 2.3 Vectorisation automatique par le compilateur

FreePascal peut vectoriser automatiquement certaines boucles si vous activez les bonnes options de compilation.

#### Options de compilation recommandées

```bash
# Compilation avec optimisations maximales
fpc -O3 -CpCOREAVX2 -FaAVX2 monprogramme.pas
```

**Explications :**
- `-O3` : Niveau d'optimisation maximum
- `-CpCOREAVX2` : Génère du code pour les processeurs supportant AVX2
- `-FaAVX2` : Active explicitement les instructions AVX2

#### Exemple de code vectorisable

```pascal
procedure AddArrays(const A, B: array of Single; var C: array of Single);  
var
  i: Integer;
begin
  // Cette boucle simple peut être vectorisée automatiquement
  for i := 0 to High(A) do
    C[i] := A[i] + B[i];
end;
```

Le compilateur avec `-O3` transformera cette boucle pour utiliser des instructions SIMD qui additionnent 4, 8 ou 16 valeurs à la fois !

### 2.4 Vectorisation manuelle avec inline assembler

Pour un contrôle total, vous pouvez écrire du code SIMD directement en assembleur inline.

#### Exemple SSE : Addition de 4 floats

```pascal
procedure AddVectorsSSE(const A, B: array of Single; var Result: array of Single; Count: Integer);  
var
  i: Integer;
begin
  i := 0;

  // Traitement par paquets de 4
  while i <= Count - 4 do
  begin
    asm
      mov eax, A
      mov ebx, B
      mov ecx, Result
      mov edx, i

      // Charger 4 floats de A
      movups xmm0, [eax + edx*4]

      // Charger 4 floats de B
      movups xmm1, [ebx + edx*4]

      // Addition vectorielle
      addps xmm0, xmm1

      // Sauvegarder le résultat
      movups [ecx + edx*4], xmm0
    end;

    Inc(i, 4);
  end;

  // Traiter les éléments restants
  while i < Count do
  begin
    Result[i] := A[i] + B[i];
    Inc(i);
  end;
end;
```

#### Exemple AVX : Addition de 8 floats

```pascal
procedure AddVectorsAVX(const A, B: array of Single; var Result: array of Single; Count: Integer);  
var
  i: Integer;
begin
  i := 0;

  // Traitement par paquets de 8
  while i <= Count - 8 do
  begin
    asm
      mov rax, A
      mov rbx, B
      mov rcx, Result
      mov rdx, i

      // Charger 8 floats de A (256 bits)
      vmovups ymm0, [rax + rdx*4]

      // Charger 8 floats de B
      vmovups ymm1, [rbx + rdx*4]

      // Addition vectorielle de 8 floats simultanément
      vaddps ymm0, ymm0, ymm1

      // Sauvegarder le résultat
      vmovups [rcx + rdx*4], ymm0
    end;

    Inc(i, 8);
  end;

  // Traiter les éléments restants
  while i < Count do
  begin
    Result[i] := A[i] + B[i];
    Inc(i);
  end;
end;
```

### 2.5 Détection des capacités du processeur

Avant d'utiliser des instructions SIMD avancées, vérifiez que le processeur les supporte :

```pascal
uses
  CPU;  // Unit incluse dans FreePascal

function GetCPUFeatures: string;  
begin
  Result := '';

  if SSESupport then
    Result := Result + 'SSE ';
  if SSE2Support then
    Result := Result + 'SSE2 ';
  if SSE3Support then
    Result := Result + 'SSE3 ';
  if SSSE3Support then
    Result := Result + 'SSSE3 ';
  if SSE41Support then
    Result := Result + 'SSE4.1 ';
  if SSE42Support then
    Result := Result + 'SSE4.2 ';
  if AVXSupport then
    Result := Result + 'AVX ';
  if AVX2Support then
    Result := Result + 'AVX2 ';
end;

// Utilisation adaptative
procedure AddVectorsAdaptive(const A, B: array of Single; var Result: array of Single);  
begin
  if AVX2Support then
    AddVectorsAVX(A, B, Result, Length(A))
  else if SSESupport then
    AddVectorsSSE(A, B, Result, Length(A))
  else
    AddVectorsScalar(A, B, Result, Length(A));
end;
```

---

## Partie 3 : Combiner parallélisme et vectorisation

### 3.1 La puissance de la combinaison

Pour des performances optimales, combinez calcul parallèle ET vectorisation :

```pascal
uses
  MTProcs;

procedure OptimizedMatrixMultiply(const A, B: TMatrix; var C: TMatrix);  
begin
  // Parallélisation sur les lignes
  ProcThreadPool.DoParallelLocalProc(
    procedure(RowIndex: PtrInt; ThreadIndex: PtrInt; Data: Pointer)
    var
      Col, K: Integer;
      Sum: Single;
    begin
      // Pour chaque colonne
      for Col := 0 to High(B[0]) do
      begin
        Sum := 0;

        // Produit scalaire vectorisé (le compilateur optimise cette boucle)
        for K := 0 to High(A[0]) do
          Sum := Sum + A[RowIndex][K] * B[K][Col];

        C[RowIndex][Col] := Sum;
      end;
    end,
    0, High(A), nil
  );
end;
```

### 3.2 Exemple complet : Traitement d'image

Voici un exemple complet qui combine tout ce que nous avons vu :

```pascal
program OptimizedImageProcessing;

uses
  Classes, SysUtils, MTProcs;

type
  TPixel = packed record
    B, G, R, A: Byte;
  end;

  TImageData = array of array of TPixel;

// Applique un flou gaussien en parallèle et vectorisé
procedure ApplyBlurParallel(var Image: TImageData; Radius: Integer);  
var
  Height, Width: Integer;
begin
  Height := Length(Image);
  Width := Length(Image[0]);

  // Traitement parallèle de chaque ligne
  ProcThreadPool.DoParallelLocalProc(
    procedure(Row: PtrInt; ThreadIndex: PtrInt; Data: Pointer)
    var
      Col, dy, dx: Integer;
      SumR, SumG, SumB, Count: Integer;
    begin
      // Pour chaque pixel de la ligne
      for Col := 0 to Width - 1 do
      begin
        SumR := 0; SumG := 0; SumB := 0; Count := 0;

        // Calcul de la moyenne dans le rayon
        // (Cette boucle peut être vectorisée par le compilateur)
        for dy := -Radius to Radius do
        begin
          if (Row + dy >= 0) and (Row + dy < Height) then
          begin
            for dx := -Radius to Radius do
            begin
              if (Col + dx >= 0) and (Col + dx < Width) then
              begin
                Inc(SumR, Image[Row + dy][Col + dx].R);
                Inc(SumG, Image[Row + dy][Col + dx].G);
                Inc(SumB, Image[Row + dy][Col + dx].B);
                Inc(Count);
              end;
            end;
          end;
        end;

        // Application du résultat
        Image[Row][Col].R := SumR div Count;
        Image[Row][Col].G := SumG div Count;
        Image[Row][Col].B := SumB div Count;
      end;
    end,
    0, Height - 1, nil
  );
end;

begin
  WriteLn('Traitement d''image optimisé avec parallélisme et vectorisation');
end.
```

---

## Partie 4 : Bonnes pratiques et optimisation

### 4.1 Quand utiliser le parallélisme ?

✅ **OUI, parallélisez quand :**
- Les calculs sont longs (>10ms par tâche)
- Les tâches sont indépendantes
- Vous traitez de grandes quantités de données

❌ **NON, évitez quand :**
- Les calculs sont très rapides (<1ms)
- Les tâches dépendent fortement les unes des autres
- Vous avez peu de données (overhead du parallélisme)

### 4.2 Mesurer les performances

Toujours mesurer avant et après optimisation :

```pascal
uses
  SysUtils;

var
  StartTime, EndTime: TDateTime;
  ElapsedMs: Int64;

begin
  StartTime := Now;

  // Votre code à mesurer
  ProcessData();

  EndTime := Now;
  ElapsedMs := MilliSecondsBetween(EndTime, StartTime);

  WriteLn(Format('Temps d''exécution : %d ms', [ElapsedMs]));
end;
```

### 4.3 Optimisation de l'alignement mémoire

Pour SIMD, les données doivent être alignées en mémoire :

```pascal
type
  // Alignement sur 32 octets pour AVX
  TAlignedArray = array[0..999] of Single align 32;

var
  Data: TAlignedArray;
```

### 4.4 Gestion multi-plateforme

Les différences entre Windows et Ubuntu :

```pascal
{$IFDEF WINDOWS}
  // Windows : utilise généralement plus de threads
  const OptimalThreads = 8;
{$ENDIF}

{$IFDEF LINUX}
  // Linux : peut bénéficier de plus de threads légers
  const OptimalThreads = 16;
{$ENDIF}

// Ou détecter automatiquement
uses
  {$IFDEF UNIX}BaseUnix{$ENDIF}
  {$IFDEF WINDOWS}Windows{$ENDIF};

function GetCPUCount: Integer;  
begin
  {$IFDEF WINDOWS}
  Result := GetCPUCount; // Fonction système Windows
  {$ENDIF}

  {$IFDEF LINUX}
  Result := sysconf(_SC_NPROCESSORS_ONLN);
  {$ENDIF}
end;
```

---

## Conclusion

Le calcul parallèle et la vectorisation sont des outils puissants pour accélérer vos programmes FreePascal :

1. **Calcul parallèle** : Utilise plusieurs cœurs du processeur (gain x4 à x16)
2. **Vectorisation** : Traite plusieurs données simultanément (gain x4 à x16)
3. **Combinaison** : Peut multiplier les performances par 50 ou plus !

**Points clés à retenir :**
- Utilisez MTProcs pour simplifier la parallélisation
- Laissez le compilateur vectoriser avec `-O3`
- Mesurez toujours les performances
- Attention aux race conditions
- Vérifiez la compatibilité du processeur pour SIMD avancé

Avec ces techniques, vous pouvez transformer des calculs qui prenaient des heures en quelques minutes, tout en gardant un code relativement simple et portable entre Windows et Ubuntu !

⏭️ [Intégration avec R et Python](/16-traitement-donnees-calcul-scientifique/09-integration-r-python.md)
