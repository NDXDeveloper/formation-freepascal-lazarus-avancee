🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.4 NLP et traitement de texte

## Introduction au Traitement du Langage Naturel

Le traitement du langage naturel (NLP - Natural Language Processing) est une branche de l'intelligence artificielle qui permet aux ordinateurs de comprendre, analyser et manipuler le langage humain. C'est ce qui se cache derrière :
- Les assistants vocaux (Siri, Alexa, Google Assistant)
- Les traducteurs automatiques (Google Translate, DeepL)
- Les correcteurs orthographiques
- L'analyse de sentiment sur les réseaux sociaux
- Les chatbots et agents conversationnels
- Les résumés automatiques de texte

### Pourquoi le NLP avec FreePascal/Lazarus ?

Bien que Python soit dominant dans le domaine du NLP, FreePascal offre des avantages uniques :
- **Performance** : applications standalone rapides
- **Déploiement facile** : exécutables sans dépendances
- **Multi-plateforme** : Windows/Linux avec le même code
- **Intégration** : ajouter du NLP à des applications existantes
- **Apprentissage** : comprendre les algorithmes en profondeur

---

## Les Fondamentaux du Traitement de Texte

### Encodage et gestion des caractères

Avant de traiter du texte, il faut comprendre comment il est stocké.

**UTF-8 : l'encodage universel**

```pascal
program EncodingDemo;

{$mode objfpc}{$H+}
{$codepage UTF8}  // Important pour gérer l'UTF-8

uses
  SysUtils, LazUTF8;

var
  texte: string;
  longueur, longueurUTF8: Integer;

begin
  // Texte avec accents et caractères spéciaux
  texte := 'Héllo wørld! 你好 🌍';

  // Longueur en bytes (peut être trompeuse)
  longueur := Length(texte);

  // Longueur réelle en caractères UTF-8
  longueurUTF8 := UTF8Length(texte);

  WriteLn('Bytes: ', longueur);
  WriteLn('Caractères UTF-8: ', longueurUTF8);

  // Conversion majuscule UTF-8
  WriteLn('Majuscules: ', UTF8UpperCase(texte));
end.
```

**Sortie :**
```
Bytes: 28
Caractères UTF-8: 18
Majuscules: HÉLLO WØRLD! 你好 🌍
```

### Normalisation du texte

La normalisation est la première étape de tout traitement NLP :

```pascal
unit TextNormalization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, RegExpr;

type
  TTextNormalizer = class
  private
    FRegEx: TRegExpr;
  public
    constructor Create;
    destructor Destroy; override;

    function ToLowerCase(const AText: string): string;
    function RemovePunctuation(const AText: string): string;
    function RemoveExtraSpaces(const AText: string): string;
    function RemoveAccents(const AText: string): string;
    function NormalizeAll(const AText: string): string;
  end;

implementation

constructor TTextNormalizer.Create;
begin
  FRegEx := TRegExpr.Create;
end;

destructor TTextNormalizer.Destroy;
begin
  FRegEx.Free;
  inherited;
end;

function TTextNormalizer.ToLowerCase(const AText: string): string;
begin
  Result := UTF8LowerCase(AText);
end;

function TTextNormalizer.RemovePunctuation(const AText: string): string;
begin
  // Remplacer toute ponctuation par un espace
  FRegEx.Expression := '[^\w\s]';
  Result := FRegEx.Replace(AText, ' ', True);
end;

function TTextNormalizer.RemoveExtraSpaces(const AText: string): string;
begin
  // Remplacer plusieurs espaces par un seul
  FRegEx.Expression := '\s+';
  Result := Trim(FRegEx.Replace(AText, ' ', True));
end;

function TTextNormalizer.RemoveAccents(const AText: string): string;
var
  i: Integer;
begin
  Result := AText;

  // Table de remplacement simple pour français
  Result := StringReplace(Result, 'é', 'e', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'è', 'e', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'ê', 'e', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'ë', 'e', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'à', 'a', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'â', 'a', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'ù', 'u', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'û', 'u', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'ï', 'i', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'î', 'i', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'ô', 'o', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'ç', 'c', [rfReplaceAll, rfIgnoreCase]);
end;

function TTextNormalizer.NormalizeAll(const AText: string): string;
begin
  Result := AText;
  Result := ToLowerCase(Result);
  Result := RemoveAccents(Result);
  Result := RemovePunctuation(Result);
  Result := RemoveExtraSpaces(Result);
end;

end.
```

**Utilisation :**

```pascal
var
  normalizer: TTextNormalizer;
  texte, resultat: string;
begin
  normalizer := TTextNormalizer.Create;
  try
    texte := 'Bonjour! Comment ça va aujourd''hui ???';
    resultat := normalizer.NormalizeAll(texte);

    WriteLn('Original: ', texte);
    WriteLn('Normalisé: ', resultat);
    // Sortie: bonjour comment ca va aujourd hui
  finally
    normalizer.Free;
  end;
end;
```

---

## Tokenisation : Découper le Texte en Mots

La tokenisation consiste à découper un texte en unités significatives (tokens), généralement des mots.

### Tokenisation simple

```pascal
unit SimpleTokenizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TStringList = specialize TList<string>;

  TTokenizer = class
  public
    function Tokenize(const AText: string): TStringList;
    function TokenizeByDelimiters(const AText: string;
                                  const ADelimiters: TSysCharSet): TStringList;
  end;

implementation

function TTokenizer.Tokenize(const AText: string): TStringList;
var
  i, startPos: Integer;
  currentChar: Char;
  currentToken: string;
  inWord: Boolean;
begin
  Result := TStringList.Create;
  currentToken := '';
  inWord := False;

  for i := 1 to Length(AText) do
  begin
    currentChar := AText[i];

    if currentChar in ['a'..'z', 'A'..'Z', '0'..'9', 'à'..'ÿ', 'À'..'Ÿ'] then
    begin
      currentToken := currentToken + currentChar;
      inWord := True;
    end
    else
    begin
      if inWord and (currentToken <> '') then
      begin
        Result.Add(currentToken);
        currentToken := '';
      end;
      inWord := False;
    end;
  end;

  // Ajouter le dernier token si nécessaire
  if currentToken <> '' then
    Result.Add(currentToken);
end;

function TTokenizer.TokenizeByDelimiters(const AText: string;
                                         const ADelimiters: TSysCharSet): TStringList;
var
  sl: TStringList;
  i: Integer;
begin
  Result := TStringList.Create;
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.StrictDelimiter := False;
    sl.DelimitedText := AText;

    for i := 0 to sl.Count - 1 do
      if Trim(sl[i]) <> '' then
        Result.Add(Trim(sl[i]));
  finally
    sl.Free;
  end;
end;

end.
```

**Exemple d'utilisation :**

```pascal
var
  tokenizer: TTokenizer;
  tokens: TStringList;
  i: Integer;

begin
  tokenizer := TTokenizer.Create;
  try
    tokens := tokenizer.Tokenize('Le chat mange la souris.');

    WriteLn('Tokens trouvés:');
    for i := 0 to tokens.Count - 1 do
      WriteLn('  ', i + 1, '. ', tokens[i]);

    tokens.Free;
  finally
    tokenizer.Free;
  end;
end;
```

**Sortie :**
```
Tokens trouvés:
  1. Le
  2. chat
  3. mange
  4. la
  5. souris
```

### Tokenisation avancée : N-grams

Les n-grams sont des séquences de n mots consécutifs, utiles pour capturer le contexte.

```pascal
type
  TNGramGenerator = class
  public
    function GenerateBigrams(const ATokens: TStringList): TStringList;
    function GenerateTrigrams(const ATokens: TStringList): TStringList;
    function GenerateNGrams(const ATokens: TStringList; N: Integer): TStringList;
  end;

implementation

function TNGramGenerator.GenerateBigrams(const ATokens: TStringList): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;

  for i := 0 to ATokens.Count - 2 do
    Result.Add(ATokens[i] + ' ' + ATokens[i + 1]);
end;

function TNGramGenerator.GenerateNGrams(const ATokens: TStringList;
                                        N: Integer): TStringList;
var
  i, j: Integer;
  ngram: string;
begin
  Result := TStringList.Create;

  for i := 0 to ATokens.Count - N do
  begin
    ngram := ATokens[i];
    for j := 1 to N - 1 do
      ngram := ngram + ' ' + ATokens[i + j];
    Result.Add(ngram);
  end;
end;
```

**Exemple :**

```pascal
var
  tokens, bigrams: TStringList;
  generator: TNGramGenerator;

begin
  tokens := TStringList.Create;
  tokens.Add('Le');
  tokens.Add('chat');
  tokens.Add('noir');
  tokens.Add('dort');

  generator := TNGramGenerator.Create;
  try
    bigrams := generator.GenerateBigrams(tokens);

    WriteLn('Bigrams:');
    for i := 0 to bigrams.Count - 1 do
      WriteLn('  ', bigrams[i]);
    // Sortie: "Le chat", "chat noir", "noir dort"

    bigrams.Free;
  finally
    generator.Free;
  end;

  tokens.Free;
end;
```

---

## Analyse de Fréquence et Statistiques

### Comptage de mots

```pascal
unit WordFrequency;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TWordFrequencyMap = specialize TDictionary<string, Integer>;

  TWordFrequency = class
  private
    FFrequencies: TWordFrequencyMap;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddWord(const AWord: string);
    procedure AddWords(const AWords: TStringList);
    function GetFrequency(const AWord: string): Integer;
    function GetTotalWords: Integer;
    function GetUniqueWords: Integer;
    function GetMostCommon(ACount: Integer): TStringList;
    procedure SaveToFile(const AFileName: string);
  end;

implementation

constructor TWordFrequency.Create;
begin
  FFrequencies := TWordFrequencyMap.Create;
end;

destructor TWordFrequency.Destroy;
begin
  FFrequencies.Free;
  inherited;
end;

procedure TWordFrequency.AddWord(const AWord: string);
var
  count: Integer;
  normalizedWord: string;
begin
  normalizedWord := LowerCase(Trim(AWord));
  if normalizedWord = '' then Exit;

  if FFrequencies.TryGetValue(normalizedWord, count) then
    FFrequencies[normalizedWord] := count + 1
  else
    FFrequencies.Add(normalizedWord, 1);
end;

procedure TWordFrequency.AddWords(const AWords: TStringList);
var
  i: Integer;
begin
  for i := 0 to AWords.Count - 1 do
    AddWord(AWords[i]);
end;

function TWordFrequency.GetFrequency(const AWord: string): Integer;
var
  normalizedWord: string;
begin
  normalizedWord := LowerCase(Trim(AWord));
  if not FFrequencies.TryGetValue(normalizedWord, Result) then
    Result := 0;
end;

function TWordFrequency.GetTotalWords: Integer;
var
  pair: TPair<string, Integer>;
begin
  Result := 0;
  for pair in FFrequencies do
    Result := Result + pair.Value;
end;

function TWordFrequency.GetUniqueWords: Integer;
begin
  Result := FFrequencies.Count;
end;

function TWordFrequency.GetMostCommon(ACount: Integer): TStringList;
type
  TWordFreqPair = record
    Word: string;
    Freq: Integer;
  end;
  TWordFreqArray = array of TWordFreqPair;
var
  pairs: TWordFreqArray;
  i, j: Integer;
  temp: TWordFreqPair;
  dictPair: TPair<string, Integer>;
begin
  Result := TStringList.Create;

  // Convertir le dictionnaire en tableau
  SetLength(pairs, FFrequencies.Count);
  i := 0;
  for dictPair in FFrequencies do
  begin
    pairs[i].Word := dictPair.Key;
    pairs[i].Freq := dictPair.Value;
    Inc(i);
  end;

  // Tri par fréquence décroissante (bubble sort simple)
  for i := 0 to High(pairs) - 1 do
    for j := i + 1 to High(pairs) do
      if pairs[j].Freq > pairs[i].Freq then
      begin
        temp := pairs[i];
        pairs[i] := pairs[j];
        pairs[j] := temp;
      end;

  // Retourner les N plus fréquents
  for i := 0 to Min(ACount - 1, High(pairs)) do
    Result.Add(Format('%s: %d', [pairs[i].Word, pairs[i].Freq]));
end;

procedure TWordFrequency.SaveToFile(const AFileName: string);
var
  sl: TStringList;
  pair: TPair<string, Integer>;
begin
  sl := TStringList.Create;
  try
    for pair in FFrequencies do
      sl.Add(Format('%s=%d', [pair.Key, pair.Value]));
    sl.SaveToFile(AFileName);
  finally
    sl.Free;
  end;
end;

end.
```

**Application pratique :**

```pascal
program AnalyzeText;

uses
  WordFrequency, SimpleTokenizer;

var
  freq: TWordFrequency;
  tokenizer: TTokenizer;
  tokens, mostCommon: TStringList;
  texte: string;
  i: Integer;

begin
  texte := 'Le chat est sur le tapis. Le chat dort. Le tapis est rouge.';

  tokenizer := TTokenizer.Create;
  freq := TWordFrequency.Create;
  try
    // Tokeniser
    tokens := tokenizer.Tokenize(texte);

    // Analyser les fréquences
    freq.AddWords(tokens);

    WriteLn('Total de mots: ', freq.GetTotalWords);
    WriteLn('Mots uniques: ', freq.GetUniqueWords);
    WriteLn;
    WriteLn('Mots les plus fréquents:');

    mostCommon := freq.GetMostCommon(5);
    for i := 0 to mostCommon.Count - 1 do
      WriteLn('  ', mostCommon[i]);

    mostCommon.Free;
    tokens.Free;
  finally
    freq.Free;
    tokenizer.Free;
  end;
end.
```

---

## Mots vides (Stop Words) et Lemmatisation

### Filtrage des mots vides

Les mots vides sont des mots très courants qui n'apportent pas de sens (le, la, de, à, etc.).

```pascal
unit StopWords;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TStopWordFilter = class
  private
    FStopWords: specialize TList<string>;
    procedure LoadFrenchStopWords;
    procedure LoadEnglishStopWords;
  public
    constructor Create(const ALanguage: string = 'fr');
    destructor Destroy; override;

    function IsStopWord(const AWord: string): Boolean;
    function FilterStopWords(const AWords: TStringList): TStringList;
    procedure AddStopWord(const AWord: string);
    procedure LoadFromFile(const AFileName: string);
  end;

implementation

constructor TStopWordFilter.Create(const ALanguage: string);
begin
  FStopWords := specialize TList<string>.Create;

  if ALanguage = 'fr' then
    LoadFrenchStopWords
  else if ALanguage = 'en' then
    LoadEnglishStopWords;
end;

destructor TStopWordFilter.Destroy;
begin
  FStopWords.Free;
  inherited;
end;

procedure TStopWordFilter.LoadFrenchStopWords;
const
  FrenchStops: array[0..39] of string = (
    'le', 'la', 'les', 'un', 'une', 'des', 'de', 'du',
    'et', 'ou', 'mais', 'donc', 'car', 'ni', 'or',
    'ce', 'cet', 'cette', 'ces',
    'mon', 'ma', 'mes', 'ton', 'ta', 'tes', 'son', 'sa', 'ses',
    'être', 'avoir', 'faire',
    'je', 'tu', 'il', 'elle', 'nous', 'vous', 'ils', 'elles',
    'à', 'dans'
  );
var
  i: Integer;
begin
  for i := Low(FrenchStops) to High(FrenchStops) do
    FStopWords.Add(FrenchStops[i]);
end;

procedure TStopWordFilter.LoadEnglishStopWords;
const
  EnglishStops: array[0..24] of string = (
    'the', 'a', 'an', 'and', 'or', 'but', 'in', 'on', 'at',
    'to', 'for', 'of', 'with', 'by', 'from', 'as',
    'is', 'was', 'are', 'were', 'be', 'been', 'being',
    'have', 'has'
  );
var
  i: Integer;
begin
  for i := Low(EnglishStops) to High(EnglishStops) do
    FStopWords.Add(EnglishStops[i]);
end;

function TStopWordFilter.IsStopWord(const AWord: string): Boolean;
var
  normalizedWord: string;
begin
  normalizedWord := LowerCase(Trim(AWord));
  Result := FStopWords.IndexOf(normalizedWord) >= 0;
end;

function TStopWordFilter.FilterStopWords(const AWords: TStringList): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;

  for i := 0 to AWords.Count - 1 do
    if not IsStopWord(AWords[i]) then
      Result.Add(AWords[i]);
end;

procedure TStopWordFilter.AddStopWord(const AWord: string);
begin
  if not IsStopWord(AWord) then
    FStopWords.Add(LowerCase(Trim(AWord)));
end;

procedure TStopWordFilter.LoadFromFile(const AFileName: string);
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    for i := 0 to sl.Count - 1 do
      AddStopWord(sl[i]);
  finally
    sl.Free;
  end;
end;

end.
```

---

## Analyse de Sentiment

L'analyse de sentiment détermine si un texte exprime une opinion positive, négative ou neutre.

### Approche par dictionnaire

```pascal
unit SentimentAnalysis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TSentimentScore = record
    Positive: Double;
    Negative: Double;
    Neutral: Double;
    Overall: Double; // -1 (très négatif) à +1 (très positif)
  end;

  TSentimentAnalyzer = class
  private
    FPositiveWords: specialize TList<string>;
    FNegativeWords: specialize TList<string>;
    FIntensifiers: specialize TDictionary<string, Double>;
    procedure LoadDefaultLexicon;
  public
    constructor Create;
    destructor Destroy; override;

    function AnalyzeText(const AText: string): TSentimentScore;
    function AnalyzeTokens(const ATokens: TStringList): TSentimentScore;
    procedure AddPositiveWord(const AWord: string);
    procedure AddNegativeWord(const AWord: string);
    function GetSentimentLabel(const AScore: TSentimentScore): string;
  end;

implementation

constructor TSentimentAnalyzer.Create;
begin
  FPositiveWords := specialize TList<string>.Create;
  FNegativeWords := specialize TList<string>.Create;
  FIntensifiers := specialize TDictionary<string, Double>.Create;
  LoadDefaultLexicon;
end;

destructor TSentimentAnalyzer.Destroy;
begin
  FPositiveWords.Free;
  FNegativeWords.Free;
  FIntensifiers.Free;
  inherited;
end;

procedure TSentimentAnalyzer.LoadDefaultLexicon;
begin
  // Mots positifs français
  FPositiveWords.Add('bon');
  FPositiveWords.Add('bien');
  FPositiveWords.Add('excellent');
  FPositiveWords.Add('super');
  FPositiveWords.Add('génial');
  FPositiveWords.Add('magnifique');
  FPositiveWords.Add('parfait');
  FPositiveWords.Add('merveilleux');
  FPositiveWords.Add('formidable');
  FPositiveWords.Add('fantastique');
  FPositiveWords.Add('agréable');
  FPositiveWords.Add('joli');
  FPositiveWords.Add('beau');
  FPositiveWords.Add('content');
  FPositiveWords.Add('heureux');
  FPositiveWords.Add('joie');
  FPositiveWords.Add('aimer');
  FPositiveWords.Add('adorer');

  // Mots négatifs français
  FNegativeWords.Add('mauvais');
  FNegativeWords.Add('mal');
  FNegativeWords.Add('horrible');
  FNegativeWords.Add('terrible');
  FNegativeWords.Add('nul');
  FNegativeWords.Add('médiocre');
  FNegativeWords.Add('affreux');
  FNegativeWords.Add('triste');
  FNegativeWords.Add('détester');
  FNegativeWords.Add('haïr');
  FNegativeWords.Add('problème');
  FNegativeWords.Add('erreur');
  FNegativeWords.Add('échec');
  FNegativeWords.Add('pire');
  FNegativeWords.Add('difficile');
  FNegativeWords.Add('ennuyeux');

  // Intensificateurs
  FIntensifiers.Add('très', 1.5);
  FIntensifiers.Add('vraiment', 1.5);
  FIntensifiers.Add('extrêmement', 2.0);
  FIntensifiers.Add('peu', 0.5);
  FIntensifiers.Add('assez', 0.8);
end;

function TSentimentAnalyzer.AnalyzeTokens(const ATokens: TStringList): TSentimentScore;
var
  i: Integer;
  word, prevWord: string;
  positiveCount, negativeCount: Integer;
  multiplier: Double;
  intensifierValue: Double;
begin
  positiveCount := 0;
  negativeCount := 0;
  prevWord := '';

  for i := 0 to ATokens.Count - 1 do
  begin
    word := LowerCase(ATokens[i]);
    multiplier := 1.0;

    // Vérifier si le mot précédent est un intensificateur
    if (prevWord <> '') and FIntensifiers.TryGetValue(prevWord, intensifierValue) then
      multiplier := intensifierValue;

    // Compter les mots positifs et négatifs
    if FPositiveWords.IndexOf(word) >= 0 then
      positiveCount := positiveCount + Round(multiplier)
    else if FNegativeWords.IndexOf(word) >= 0 then
      negativeCount := negativeCount + Round(multiplier);

    prevWord := word;
  end;

  // Calculer les scores
  Result.Positive := positiveCount;
  Result.Negative := negativeCount;
  Result.Neutral := ATokens.Count - (positiveCount + negativeCount);

  // Score global entre -1 et 1
  if (positiveCount + negativeCount) > 0 then
    Result.Overall := (positiveCount - negativeCount) / (positiveCount + negativeCount)
  else
    Result.Overall := 0;
end;

function TSentimentAnalyzer.AnalyzeText(const AText: string): TSentimentScore;
var
  tokenizer: TTokenizer;
  tokens: TStringList;
begin
  tokenizer := TTokenizer.Create;
  try
    tokens := tokenizer.Tokenize(AText);
    try
      Result := AnalyzeTokens(tokens);
    finally
      tokens.Free;
    end;
  finally
    tokenizer.Free;
  end;
end;

procedure TSentimentAnalyzer.AddPositiveWord(const AWord: string);
begin
  if FPositiveWords.IndexOf(LowerCase(AWord)) < 0 then
    FPositiveWords.Add(LowerCase(AWord));
end;

procedure TSentimentAnalyzer.AddNegativeWord(const AWord: string);
begin
  if FNegativeWords.IndexOf(LowerCase(AWord)) < 0 then
    FNegativeWords.Add(LowerCase(AWord));
end;

function TSentimentAnalyzer.GetSentimentLabel(const AScore: TSentimentScore): string;
begin
  if AScore.Overall > 0.5 then
    Result := 'Très positif'
  else if AScore.Overall > 0.1 then
    Result := 'Positif'
  else if AScore.Overall < -0.5 then
    Result := 'Très négatif'
  else if AScore.Overall < -0.1 then
    Result := 'Négatif'
  else
    Result := 'Neutre';
end;

end.
```

**Exemple d'utilisation :**

```pascal
program TestSentiment;

uses
  SentimentAnalysis;

var
  analyzer: TSentimentAnalyzer;
  score: TSentimentScore;
  texte: string;

begin
  analyzer := TSentimentAnalyzer.Create;
  try
    texte := 'Ce film est vraiment excellent et magnifique !';
    score := analyzer.AnalyzeText(texte);

    WriteLn('Texte: ', texte);
    WriteLn('Score global: ', score.Overall:0:2);
    WriteLn('Sentiment: ', analyzer.GetSentimentLabel(score));
    WriteLn;

    texte := 'C''est horrible et vraiment mauvais.';
    score := analyzer.AnalyzeText(texte);

    WriteLn('Texte: ', texte);
    WriteLn('Score global: ', score.Overall:0:2);
    WriteLn('Sentiment: ', analyzer.GetSentimentLabel(score));
  finally
    analyzer.Free;
  end;
end.
```

---

## Classification de Texte : Naive Bayes

L'algorithme Naive Bayes est simple mais puissant pour classifier du texte.

```pascal
unit NaiveBayesClassifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Generics.Collections;

type
  TWordProbabilities = specialize TDictionary<string, Double>;
  TClassData = class
    WordCount: TWordProbabilities;
    TotalWords: Integer;
    DocumentCount: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TNaiveBayesClassifier = class
  private
    FClasses: specialize TDictionary<string, TClassData>;
    FVocabulary: specialize TList<string>;
    FTotalDocuments: Integer;
    function CalculateProbability(const AWords: TStringList;
                                  const AClass: string): Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Train(const AText: string; const AClass: string);
    procedure TrainFromTokens(const ATokens: TStringList; const AClass: string);
    function Classify(const AText: string): string;
    function ClassifyWithProbabilities(const AText: string;
                                       out AProbabilities: TWordProbabilities): string;
    procedure SaveModel(const AFileName: string);
    procedure LoadModel(const AFileName: string);
  end;

implementation

{ TClassData }

constructor TClassData.Create;
begin
  WordCount := TWordProbabilities.Create;
  TotalWords := 0;
  DocumentCount := 0;
end;

destructor TClassData.Destroy;
begin
  WordCount.Free;
  inherited;
end;

{ TNaiveBayesClassifier }

constructor TNaiveBayesClassifier.Create;
begin
  FClasses := specialize TDictionary<string, TClassData>.Create;
  FVocabulary := specialize TList<string>.Create;
  FTotalDocuments := 0;
end;

destructor TNaiveBayesClassifier.Destroy;
var
  classData: TClassData;
begin
  for classData in FClasses.Values do
    classData.Free;
  FClasses.Free;
  FVocabulary.Free;
  inherited;
end;

procedure TNaiveBayesClassifier.TrainFromTokens(const ATokens: TStringList;
                                                 const AClass: string);
var
  classData: TClassData;
  i: Integer;
  word: string;
  count: Integer;
begin
  // Créer la classe si elle n'existe pas
  if not FClasses.TryGetValue(AClass, classData) then
  begin
    classData := TClassData.Create;
    FClasses.Add(AClass, classData);
  end;

  // Compter les mots
  for i := 0 to ATokens.Count - 1 do
  begin
    word := LowerCase(ATokens[i]);

    // Ajouter au vocabulaire global
    if FVocabulary.IndexOf(word) < 0 then
      FVocabulary.Add(word);

    // Compter dans la classe
    if classData.WordCount.TryGetValue(word, count) then
      classData.WordCount[word] := count + 1
    else
      classData.WordCount.Add(word, 1);

    Inc(classData.TotalWords);
  end;

  Inc(classData.DocumentCount);
  Inc(FTotalDocuments);
end;

procedure TNaiveBayesClassifier.Train(const AText: string; const AClass: string);
var
  tokenizer: TTokenizer;
  tokens: TStringList;
begin
  tokenizer := TTokenizer.Create;
  try
    tokens := tokenizer.Tokenize(AText);
    try
      TrainFromTokens(tokens, AClass);
    finally
      tokens.Free;
    end;
  finally
    tokenizer.Free;
  end;
end;

function TNaiveBayesClassifier.CalculateProbability(const AWords: TStringList;
                                                     const AClass: string): Double;
var
  classData: TClassData;
  i: Integer;
  word: string;
  wordCount: Integer;
  wordProbability: Double;
  classProbability: Double;
  vocabularySize: Integer;
begin
  if not FClasses.TryGetValue(AClass, classData) then
  begin
    Result := 0;
    Exit;
  end;

  // Probabilité a priori de la classe : P(classe)
  classProbability := classData.DocumentCount / FTotalDocuments;

  // Log-probabilité pour éviter l'underflow
  Result := Ln(classProbability);

  vocabularySize := FVocabulary.Count;

  // Pour chaque mot, calculer P(mot|classe) avec lissage de Laplace
  for i := 0 to AWords.Count - 1 do
  begin
    word := LowerCase(AWords[i]);

    if not classData.WordCount.TryGetValue(word, wordCount) then
      wordCount := 0;

    // Lissage de Laplace: (count + 1) / (totalWords + vocabularySize)
    wordProbability := (wordCount + 1) / (classData.TotalWords + vocabularySize);

    Result := Result + Ln(wordProbability);
  end;
end;

function TNaiveBayesClassifier.Classify(const AText: string): string;
var
  probabilities: TWordProbabilities;
begin
  Result := ClassifyWithProbabilities(AText, probabilities);
  probabilities.Free;
end;

function TNaiveBayesClassifier.ClassifyWithProbabilities(const AText: string;
                                                          out AProbabilities: TWordProbabilities): string;
var
  tokenizer: TTokenizer;
  tokens: TStringList;
  className: string;
  probability, maxProbability: Double;
  bestClass: string;
begin
  AProbabilities := TWordProbabilities.Create;

  tokenizer := TTokenizer.Create;
  try
    tokens := tokenizer.Tokenize(AText);
    try
      maxProbability := -MaxDouble;
      bestClass := '';

      // Calculer la probabilité pour chaque classe
      for className in FClasses.Keys do
      begin
        probability := CalculateProbability(tokens, className);
        AProbabilities.Add(className, probability);

        if probability > maxProbability then
        begin
          maxProbability := probability;
          bestClass := className;
        end;
      end;

      Result := bestClass;
    finally
      tokens.Free;
    end;
  finally
    tokenizer.Free;
  end;
end;

procedure TNaiveBayesClassifier.SaveModel(const AFileName: string);
var
  sl: TStringList;
  className: string;
  classData: TClassData;
  word: string;
  count: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Add('[METADATA]');
    sl.Add('TotalDocuments=' + IntToStr(FTotalDocuments));
    sl.Add('VocabularySize=' + IntToStr(FVocabulary.Count));
    sl.Add('');

    for className in FClasses.Keys do
    begin
      classData := FClasses[className];
      sl.Add('[CLASS:' + className + ']');
      sl.Add('DocumentCount=' + IntToStr(classData.DocumentCount));
      sl.Add('TotalWords=' + IntToStr(classData.TotalWords));

      for word in classData.WordCount.Keys do
      begin
        count := classData.WordCount[word];
        sl.Add(word + '=' + IntToStr(count));
      end;
      sl.Add('');
    end;

    sl.SaveToFile(AFileName);
  finally
    sl.Free;
  end;
end;

procedure TNaiveBayesClassifier.LoadModel(const AFileName: string);
var
  sl: TStringList;
  i: Integer;
  line, currentClass: string;
  key, value: string;
  eqPos: Integer;
  classData: TClassData;
begin
  // Nettoyer les données existantes
  for classData in FClasses.Values do
    classData.Free;
  FClasses.Clear;
  FVocabulary.Clear;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    currentClass := '';
    classData := nil;

    for i := 0 to sl.Count - 1 do
    begin
      line := Trim(sl[i]);
      if line = '' then Continue;

      if Pos('[CLASS:', line) = 1 then
      begin
        currentClass := Copy(line, 8, Length(line) - 8);
        classData := TClassData.Create;
        FClasses.Add(currentClass, classData);
      end
      else begin
        eqPos := Pos('=', line);
        if eqPos > 0 then
        begin
          key := Copy(line, 1, eqPos - 1);
          value := Copy(line, eqPos + 1, Length(line) - eqPos);
          if key = 'TotalDocuments' then
            FTotalDocuments := StrToInt(value)
          else if (classData <> nil) and (key = 'DocumentCount') then
            classData.DocumentCount := StrToInt(value)
          else if (classData <> nil) and (key = 'TotalWords') then
            classData.TotalWords := StrToInt(value)
          else if classData <> nil then
          begin
            classData.WordCount.Add(key, StrToInt(value));
            if FVocabulary.IndexOf(key) < 0 then
              FVocabulary.Add(key);
          end;
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

end.
```

**Exemple d'utilisation du classificateur :**

```pascal
program SpamClassifier;

uses
  SysUtils, NaiveBayesClassifier;

var
  classifier: TNaiveBayesClassifier;
  testText: string;
  classification: string;

begin
  classifier := TNaiveBayesClassifier.Create;
  try
    // Entraînement avec des exemples de spam
    classifier.Train('Gagnez de l''argent rapidement ! Offre exceptionnelle !', 'spam');
    classifier.Train('Cliquez ici pour gagner un iPhone gratuit !', 'spam');
    classifier.Train('Promotion incroyable ! Ne manquez pas cette opportunité !', 'spam');
    classifier.Train('Argent facile sans effort ! Rejoignez-nous maintenant !', 'spam');

    // Entraînement avec des exemples légitimes
    classifier.Train('Bonjour, voici le compte-rendu de la réunion d''hier', 'normal');
    classifier.Train('La présentation est prévue pour demain à 14h', 'normal');
    classifier.Train('Merci pour votre aide sur ce projet', 'normal');
    classifier.Train('Pouvez-vous relire ce document avant lundi ?', 'normal');

    // Test de classification
    testText := 'Opportunité unique de gagner de l''argent !';
    classification := classifier.Classify(testText);
    WriteLn('Texte: ', testText);
    WriteLn('Classification: ', classification);
    WriteLn;

    testText := 'La réunion est reportée à mercredi prochain';
    classification := classifier.Classify(testText);
    WriteLn('Texte: ', testText);
    WriteLn('Classification: ', classification);

    // Sauvegarder le modèle
    classifier.SaveModel('spam_model.txt');
    WriteLn;
    WriteLn('Modèle sauvegardé dans spam_model.txt');
  finally
    classifier.Free;
  end;
end.
```

---

## Mesures de Similarité de Texte

### Distance de Levenshtein

La distance de Levenshtein mesure le nombre minimum d'opérations (insertion, suppression, substitution) pour transformer une chaîne en une autre.

```pascal
unit TextSimilarity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TTextSimilarity = class
  public
    function LevenshteinDistance(const S1, S2: string): Integer;
    function LevenshteinSimilarity(const S1, S2: string): Double;
    function JaccardSimilarity(const ATokens1, ATokens2: TStringList): Double;
    function CosineSimilarity(const AText1, AText2: string): Double;
    function NGramSimilarity(const S1, S2: string; N: Integer): Double;
  end;

implementation

function TTextSimilarity.LevenshteinDistance(const S1, S2: string): Integer;
var
  len1, len2, i, j: Integer;
  cost: Integer;
  d: array of array of Integer;
begin
  len1 := Length(S1);
  len2 := Length(S2);

  SetLength(d, len1 + 1, len2 + 1);

  for i := 0 to len1 do
    d[i, 0] := i;
  for j := 0 to len2 do
    d[0, j] := j;

  for i := 1 to len1 do
    for j := 1 to len2 do
    begin
      if S1[i] = S2[j] then
        cost := 0
      else
        cost := 1;

      d[i, j] := Min(Min(
        d[i-1, j] + 1,      // Suppression
        d[i, j-1] + 1),     // Insertion
        d[i-1, j-1] + cost  // Substitution
      );
    end;

  Result := d[len1, len2];
end;

function TTextSimilarity.LevenshteinSimilarity(const S1, S2: string): Double;
var
  distance, maxLen: Integer;
begin
  distance := LevenshteinDistance(S1, S2);
  maxLen := Max(Length(S1), Length(S2));

  if maxLen = 0 then
    Result := 1.0
  else
    Result := 1.0 - (distance / maxLen);
end;

function TTextSimilarity.JaccardSimilarity(const ATokens1, ATokens2: TStringList): Double;
var
  set1, set2, intersection, union: TStringList;
  i: Integer;
begin
  // Créer des ensembles uniques
  set1 := TStringList.Create;
  set2 := TStringList.Create;
  intersection := TStringList.Create;
  union := TStringList.Create;
  try
    set1.Sorted := True;
    set1.Duplicates := dupIgnore;
    set2.Sorted := True;
    set2.Duplicates := dupIgnore;

    for i := 0 to ATokens1.Count - 1 do
      set1.Add(LowerCase(ATokens1[i]));
    for i := 0 to ATokens2.Count - 1 do
      set2.Add(LowerCase(ATokens2[i]));

    // Intersection
    for i := 0 to set1.Count - 1 do
      if set2.IndexOf(set1[i]) >= 0 then
        intersection.Add(set1[i]);

    // Union
    union.Assign(set1);
    for i := 0 to set2.Count - 1 do
      if union.IndexOf(set2[i]) < 0 then
        union.Add(set2[i]);

    if union.Count = 0 then
      Result := 0
    else
      Result := intersection.Count / union.Count;
  finally
    set1.Free;
    set2.Free;
    intersection.Free;
    union.Free;
  end;
end;

function TTextSimilarity.CosineSimilarity(const AText1, AText2: string): Double;
var
  tokenizer: TTokenizer;
  tokens1, tokens2: TStringList;
  wordFreq1, wordFreq2: TWordFrequencyMap;
  allWords: TStringList;
  i: Integer;
  word: string;
  freq1, freq2: Integer;
  dotProduct, magnitude1, magnitude2: Double;
begin
  tokenizer := TTokenizer.Create;
  try
    tokens1 := tokenizer.Tokenize(AText1);
    tokens2 := tokenizer.Tokenize(AText2);

    wordFreq1 := TWordFrequencyMap.Create;
    wordFreq2 := TWordFrequencyMap.Create;
    allWords := TStringList.Create;
    try
      // Compter les fréquences
      for i := 0 to tokens1.Count - 1 do
      begin
        word := LowerCase(tokens1[i]);
        if wordFreq1.TryGetValue(word, freq1) then
          wordFreq1[word] := freq1 + 1
        else
        begin
          wordFreq1.Add(word, 1);
          if allWords.IndexOf(word) < 0 then
            allWords.Add(word);
        end;
      end;

      for i := 0 to tokens2.Count - 1 do
      begin
        word := LowerCase(tokens2[i]);
        if wordFreq2.TryGetValue(word, freq2) then
          wordFreq2[word] := freq2 + 1
        else
        begin
          wordFreq2.Add(word, 1);
          if allWords.IndexOf(word) < 0 then
            allWords.Add(word);
        end;
      end;

      // Calculer le produit scalaire et les magnitudes
      dotProduct := 0;
      magnitude1 := 0;
      magnitude2 := 0;

      for i := 0 to allWords.Count - 1 do
      begin
        word := allWords[i];

        if not wordFreq1.TryGetValue(word, freq1) then freq1 := 0;
        if not wordFreq2.TryGetValue(word, freq2) then freq2 := 0;

        dotProduct := dotProduct + (freq1 * freq2);
        magnitude1 := magnitude1 + (freq1 * freq1);
        magnitude2 := magnitude2 + (freq2 * freq2);
      end;

      magnitude1 := Sqrt(magnitude1);
      magnitude2 := Sqrt(magnitude2);

      if (magnitude1 = 0) or (magnitude2 = 0) then
        Result := 0
      else
        Result := dotProduct / (magnitude1 * magnitude2);
    finally
      wordFreq1.Free;
      wordFreq2.Free;
      allWords.Free;
      tokens1.Free;
      tokens2.Free;
    end;
  finally
    tokenizer.Free;
  end;
end;

function TTextSimilarity.NGramSimilarity(const S1, S2: string; N: Integer): Double;
var
  ngrams1, ngrams2: TStringList;
  i: Integer;
  ngram: string;
begin
  ngrams1 := TStringList.Create;
  ngrams2 := TStringList.Create;
  try
    // Générer les n-grams
    for i := 1 to Length(S1) - N + 1 do
      ngrams1.Add(Copy(S1, i, N));

    for i := 1 to Length(S2) - N + 1 do
      ngrams2.Add(Copy(S2, i, N));

    // Utiliser Jaccard sur les n-grams
    Result := JaccardSimilarity(ngrams1, ngrams2);
  finally
    ngrams1.Free;
    ngrams2.Free;
  end;
end;

end.
```

**Exemple d'utilisation :**

```pascal
program TestSimilarity;

uses
  SysUtils, TextSimilarity;

var
  similarity: TTextSimilarity;
  text1, text2: string;
  score: Double;

begin
  similarity := TTextSimilarity.Create;
  try
    text1 := 'Le chat dort sur le tapis';
    text2 := 'Le chat dort sur le canapé';

    WriteLn('Texte 1: ', text1);
    WriteLn('Texte 2: ', text2);
    WriteLn;

    score := similarity.LevenshteinSimilarity(text1, text2);
    WriteLn('Levenshtein: ', score:0:3);

    score := similarity.CosineSimilarity(text1, text2);
    WriteLn('Cosinus: ', score:0:3);

    score := similarity.NGramSimilarity(text1, text2, 3);
    WriteLn('3-grams: ', score:0:3);
  finally
    similarity.Free;
  end;
end.
```

---

## Extraction d'Informations

### Extraction d'entités nommées (NER - Named Entity Recognition)

```pascal
unit NamedEntityRecognition;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, Generics.Collections;

type
  TEntityType = (etPerson, etLocation, etOrganization, etDate, etEmail, etPhone, etURL);

  TEntity = record
    Text: string;
    EntityType: TEntityType;
    Position: Integer;
  end;

  TEntityList = specialize TList<TEntity>;

  TNamedEntityRecognizer = class
  private
    FRegEx: TRegExpr;
    FPersonNames: TStringList;
    FLocations: TStringList;
    FOrganizations: TStringList;

    function ExtractEmails(const AText: string): TEntityList;
    function ExtractPhones(const AText: string): TEntityList;
    function ExtractURLs(const AText: string): TEntityList;
    function ExtractDates(const AText: string): TEntityList;
    function ExtractCapitalizedWords(const AText: string): TEntityList;
  public
    constructor Create;
    destructor Destroy; override;

    function ExtractEntities(const AText: string): TEntityList;
    procedure LoadGazetteer(const AFileName: string; AType: TEntityType);
    function EntityTypeToString(AType: TEntityType): string;
  end;

implementation

constructor TNamedEntityRecognizer.Create;
begin
  FRegEx := TRegExpr.Create;
  FPersonNames := TStringList.Create;
  FLocations := TStringList.Create;
  FOrganizations := TStringList.Create;

  // Charger quelques noms par défaut
  FPersonNames.Add('jean');
  FPersonNames.Add('marie');
  FPersonNames.Add('pierre');
  FPersonNames.Add('sophie');

  FLocations.Add('paris');
  FLocations.Add('lyon');
  FLocations.Add('marseille');
  FLocations.Add('france');
end;

destructor TNamedEntityRecognizer.Destroy;
begin
  FRegEx.Free;
  FPersonNames.Free;
  FLocations.Free;
  FOrganizations.Free;
  inherited;
end;

function TNamedEntityRecognizer.ExtractEmails(const AText: string): TEntityList;
var
  entity: TEntity;
begin
  Result := TEntityList.Create;

  FRegEx.Expression := '\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b';

  if FRegEx.Exec(AText) then
  begin
    repeat
      entity.Text := FRegEx.Match[0];
      entity.EntityType := etEmail;
      entity.Position := FRegEx.MatchPos[0];
      Result.Add(entity);
    until not FRegEx.ExecNext;
  end;
end;

function TNamedEntityRecognizer.ExtractPhones(const AText: string): TEntityList;
var
  entity: TEntity;
begin
  Result := TEntityList.Create;

  // Pattern pour numéros français
  FRegEx.Expression := '\b0[1-9](\s?\d{2}){4}\b';

  if FRegEx.Exec(AText) then
  begin
    repeat
      entity.Text := FRegEx.Match[0];
      entity.EntityType := etPhone;
      entity.Position := FRegEx.MatchPos[0];
      Result.Add(entity);
    until not FRegEx.ExecNext;
  end;
end;

function TNamedEntityRecognizer.ExtractURLs(const AText: string): TEntityList;
var
  entity: TEntity;
begin
  Result := TEntityList.Create;

  FRegEx.Expression := 'https?://[^\s]+';

  if FRegEx.Exec(AText) then
  begin
    repeat
      entity.Text := FRegEx.Match[0];
      entity.EntityType := etURL;
      entity.Position := FRegEx.MatchPos[0];
      Result.Add(entity);
    until not FRegEx.ExecNext;
  end;
end;

function TNamedEntityRecognizer.ExtractDates(const AText: string): TEntityList;
var
  entity: TEntity;
begin
  Result := TEntityList.Create;

  // Pattern simple pour dates françaises (jj/mm/aaaa)
  FRegEx.Expression := '\b\d{1,2}/\d{1,2}/\d{4}\b';

  if FRegEx.Exec(AText) then
  begin
    repeat
      entity.Text := FRegEx.Match[0];
      entity.EntityType := etDate;
      entity.Position := FRegEx.MatchPos[0];
      Result.Add(entity);
    until not FRegEx.ExecNext;
  end;
end;

function TNamedEntityRecognizer.ExtractCapitalizedWords(const AText: string): TEntityList;
var
  entity: TEntity;
  word: string;
begin
  Result := TEntityList.Create;

  FRegEx.Expression := '\b[A-ZÀ-Ÿ][a-zà-ÿ]+\b';

  if FRegEx.Exec(AText) then
  begin
    repeat
      word := LowerCase(FRegEx.Match[0]);

      // Vérifier dans les gazetteers
      if FPersonNames.IndexOf(word) >= 0 then
      begin
        entity.Text := FRegEx.Match[0];
        entity.EntityType := etPerson;
        entity.Position := FRegEx.MatchPos[0];
        Result.Add(entity);
      end
      else if FLocations.IndexOf(word) >= 0 then
      begin
        entity.Text := FRegEx.Match[0];
        entity.EntityType := etLocation;
        entity.Position := FRegEx.MatchPos[0];
        Result.Add(entity);
      end
      else if FOrganizations.IndexOf(word) >= 0 then
      begin
        entity.Text := FRegEx.Match[0];
        entity.EntityType := etOrganization;
        entity.Position := FRegEx.MatchPos[0];
        Result.Add(entity);
      end;
    until not FRegEx.ExecNext;
  end;
end;

function TNamedEntityRecognizer.ExtractEntities(const AText: string): TEntityList;
var
  emails, phones, urls, dates, words: TEntityList;
  i: Integer;
begin
  Result := TEntityList.Create;

  // Extraire tous les types d'entités
  emails := ExtractEmails(AText);
  phones := ExtractPhones(AText);
  urls := ExtractURLs(AText);
  dates := ExtractDates(AText);
  words := ExtractCapitalizedWords(AText);

  try
    // Combiner toutes les entités
    for i := 0 to emails.Count - 1 do
      Result.Add(emails[i]);
    for i := 0 to phones.Count - 1 do
      Result.Add(phones[i]);
    for i := 0 to urls.Count - 1 do
      Result.Add(urls[i]);
    for i := 0 to dates.Count - 1 do
      Result.Add(dates[i]);
    for i := 0 to words.Count - 1 do
      Result.Add(words[i]);
  finally
    emails.Free;
    phones.Free;
    urls.Free;
    dates.Free;
    words.Free;
  end;
end;

procedure TNamedEntityRecognizer.LoadGazetteer(const AFileName: string;
                                                AType: TEntityType);
var
  target: TStringList;
begin
  case AType of
    etPerson: target := FPersonNames;
    etLocation: target := FLocations;
    etOrganization: target := FOrganizations;
  else
    Exit;
  end;

  target.LoadFromFile(AFileName);
end;

function TNamedEntityRecognizer.EntityTypeToString(AType: TEntityType): string;
begin
  case AType of
    etPerson: Result := 'Personne';
    etLocation: Result := 'Lieu';
    etOrganization: Result := 'Organisation';
    etDate: Result := 'Date';
    etEmail: Result := 'Email';
    etPhone: Result := 'Téléphone';
    etURL: Result := 'URL';
  else
    Result := 'Inconnu';
  end;
end;

end.
```

**Exemple d'utilisation :**

```pascal
program ExtractEntities;

uses
  SysUtils, NamedEntityRecognition;

var
  ner: TNamedEntityRecognizer;
  entities: TEntityList;
  i: Integer;
  texte: string;

begin
  texte := 'Jean habite à Paris et son email est jean@example.com. ' +
           'Il travaille depuis le 15/03/2024. Son téléphone est 01 23 45 67 89. ' +
           'Visitez https://example.com pour plus d''infos.';

  ner := TNamedEntityRecognizer.Create;
  try
    WriteLn('Texte analysé:');
    WriteLn(texte);
    WriteLn;
    WriteLn('Entités extraites:');
    WriteLn('==================');

    entities := ner.ExtractEntities(texte);
    try
      for i := 0 to entities.Count - 1 do
      begin
        WriteLn(Format('[%s] %s (position: %d)',
          [ner.EntityTypeToString(entities[i].EntityType),
           entities[i].Text,
           entities[i].Position]));
      end;

      WriteLn;
      WriteLn('Total: ', entities.Count, ' entités trouvées');
    finally
      entities.Free;
    end;
  finally
    ner.Free;
  end;
end.
```

**Sortie attendue :**
```
Texte analysé:
Jean habite à Paris et son email est jean@example.com. Il travaille depuis le 15/03/2024. Son téléphone est 01 23 45 67 89. Visitez https://example.com pour plus d'infos.

Entités extraites:
==================
[Personne] Jean (position: 1)
[Lieu] Paris (position: 16)
[Email] jean@example.com (position: 43)
[Date] 15/03/2024 (position: 82)
[Téléphone] 01 23 45 67 89 (position: 112)
[URL] https://example.com (position: 140)

Total: 6 entités trouvées
```

---

## Résumé Automatique de Texte

Le résumé automatique consiste à extraire les informations les plus importantes d'un texte pour créer une version condensée.

### Méthode extractive : score TF-IDF

```pascal
unit TextSummarizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Generics.Collections;

type
  TSentenceScore = record
    Sentence: string;
    Score: Double;
    Position: Integer;
  end;

  TSentenceScoreList = specialize TList<TSentenceScore>;

  TTextSummarizer = class
  private
    function SplitIntoSentences(const AText: string): TStringList;
    function CalculateTFIDF(const ASentences: TStringList): TSentenceScoreList;
    function CalculateWordFrequency(const ASentences: TStringList): TWordFrequencyMap;
  public
    function Summarize(const AText: string; ASentenceCount: Integer): string;
    function SummarizeByRatio(const AText: string; ARatio: Double): string;
  end;

implementation

function TTextSummarizer.SplitIntoSentences(const AText: string): TStringList;
var
  i, startPos: Integer;
  currentSentence: string;
  ch: Char;
begin
  Result := TStringList.Create;
  currentSentence := '';
  startPos := 1;

  for i := 1 to Length(AText) do
  begin
    ch := AText[i];
    currentSentence := currentSentence + ch;

    // Fin de phrase détectée
    if (ch in ['.', '!', '?']) then
    begin
      // Vérifier que ce n'est pas une abréviation
      if (i < Length(AText)) and (AText[i + 1] = ' ') then
      begin
        Result.Add(Trim(currentSentence));
        currentSentence := '';
      end;
    end;
  end;

  // Ajouter la dernière phrase si elle existe
  if Trim(currentSentence) <> '' then
    Result.Add(Trim(currentSentence));
end;

function TTextSummarizer.CalculateWordFrequency(const ASentences: TStringList): TWordFrequencyMap;
var
  tokenizer: TTokenizer;
  stopWords: TStopWordFilter;
  i, j: Integer;
  tokens, filtered: TStringList;
  word: string;
  count: Integer;
begin
  Result := TWordFrequencyMap.Create;
  tokenizer := TTokenizer.Create;
  stopWords := TStopWordFilter.Create('fr');

  try
    for i := 0 to ASentences.Count - 1 do
    begin
      tokens := tokenizer.Tokenize(ASentences[i]);
      try
        filtered := stopWords.FilterStopWords(tokens);
        try
          for j := 0 to filtered.Count - 1 do
          begin
            word := LowerCase(filtered[j]);

            if Result.TryGetValue(word, count) then
              Result[word] := count + 1
            else
              Result.Add(word, 1);
          end;
        finally
          filtered.Free;
        end;
      finally
        tokens.Free;
      end;
    end;
  finally
    tokenizer.Free;
    stopWords.Free;
  end;
end;

function TTextSummarizer.CalculateTFIDF(const ASentences: TStringList): TSentenceScoreList;
var
  wordFreq: TWordFrequencyMap;
  tokenizer: TTokenizer;
  stopWords: TStopWordFilter;
  i, j: Integer;
  tokens, filtered: TStringList;
  sentenceScore: TSentenceScore;
  word: string;
  wordScore: Double;
  maxFreq: Integer;
  freq: Integer;
begin
  Result := TSentenceScoreList.Create;

  // Calculer les fréquences globales
  wordFreq := CalculateWordFrequency(ASentences);

  // Trouver la fréquence maximale
  maxFreq := 0;
  for freq in wordFreq.Values do
    if freq > maxFreq then
      maxFreq := freq;

  tokenizer := TTokenizer.Create;
  stopWords := TStopWordFilter.Create('fr');

  try
    // Calculer le score de chaque phrase
    for i := 0 to ASentences.Count - 1 do
    begin
      sentenceScore.Sentence := ASentences[i];
      sentenceScore.Position := i;
      sentenceScore.Score := 0;

      tokens := tokenizer.Tokenize(ASentences[i]);
      try
        filtered := stopWords.FilterStopWords(tokens);
        try
          // Score = somme des scores TF de chaque mot
          for j := 0 to filtered.Count - 1 do
          begin
            word := LowerCase(filtered[j]);

            if wordFreq.TryGetValue(word, freq) then
            begin
              // TF normalisé
              wordScore := freq / maxFreq;
              sentenceScore.Score := sentenceScore.Score + wordScore;
            end;
          end;

          // Normaliser par la longueur de la phrase
          if filtered.Count > 0 then
            sentenceScore.Score := sentenceScore.Score / filtered.Count;

          // Bonus pour les premières phrases (souvent importantes)
          if i < 3 then
            sentenceScore.Score := sentenceScore.Score * 1.5;

        finally
          filtered.Free;
        end;
      finally
        tokens.Free;
      end;

      Result.Add(sentenceScore);
    end;
  finally
    tokenizer.Free;
    stopWords.Free;
    wordFreq.Free;
  end;
end;

function TTextSummarizer.Summarize(const AText: string; ASentenceCount: Integer): string;
var
  sentences: TStringList;
  scores: TSentenceScoreList;
  i, j: Integer;
  temp: TSentenceScore;
  selectedSentences: TSentenceScoreList;
begin
  Result := '';

  sentences := SplitIntoSentences(AText);
  try
    if sentences.Count <= ASentenceCount then
    begin
      Result := AText;
      Exit;
    end;

    scores := CalculateTFIDF(sentences);
    try
      // Trier par score décroissant (tri à bulles simple)
      for i := 0 to scores.Count - 2 do
        for j := i + 1 to scores.Count - 1 do
          if scores[j].Score > scores[i].Score then
          begin
            temp := scores[i];
            scores[i] := scores[j];
            scores[j] := temp;
          end;

      // Sélectionner les N meilleures phrases
      selectedSentences := TSentenceScoreList.Create;
      try
        for i := 0 to Min(ASentenceCount - 1, scores.Count - 1) do
          selectedSentences.Add(scores[i]);

        // Retrier par position originale pour cohérence
        for i := 0 to selectedSentences.Count - 2 do
          for j := i + 1 to selectedSentences.Count - 1 do
            if selectedSentences[j].Position < selectedSentences[i].Position then
            begin
              temp := selectedSentences[i];
              selectedSentences[i] := selectedSentences[j];
              selectedSentences[j] := temp;
            end;

        // Construire le résumé
        for i := 0 to selectedSentences.Count - 1 do
        begin
          if i > 0 then
            Result := Result + ' ';
          Result := Result + selectedSentences[i].Sentence;
        end;
      finally
        selectedSentences.Free;
      end;
    finally
      scores.Free;
    end;
  finally
    sentences.Free;
  end;
end;

function TTextSummarizer.SummarizeByRatio(const AText: string; ARatio: Double): string;
var
  sentences: TStringList;
  sentenceCount: Integer;
begin
  sentences := SplitIntoSentences(AText);
  try
    sentenceCount := Max(1, Round(sentences.Count * ARatio));
    Result := Summarize(AText, sentenceCount);
  finally
    sentences.Free;
  end;
end;

end.
```

**Exemple d'utilisation :**

```pascal
program TestSummarizer;

uses
  SysUtils, TextSummarizer;

var
  summarizer: TTextSummarizer;
  texte, resume: string;

begin
  texte :=
    'L''intelligence artificielle transforme notre monde. ' +
    'Les algorithmes de machine learning permettent aux ordinateurs d''apprendre. ' +
    'Le deep learning utilise des réseaux de neurones artificiels. ' +
    'Ces technologies révolutionnent de nombreux domaines. ' +
    'La médecine bénéficie de diagnostics plus précis. ' +
    'L''industrie automobile développe des voitures autonomes. ' +
    'Les assistants vocaux comprennent le langage naturel. ' +
    'Cependant, l''IA soulève des questions éthiques importantes. ' +
    'La protection de la vie privée est un enjeu majeur. ' +
    'L''avenir de l''IA dépend d''une régulation appropriée.';

  summarizer := TTextSummarizer.Create;
  try
    WriteLn('TEXTE ORIGINAL (' + IntToStr(Length(texte)) + ' caractères):');
    WriteLn(texte);
    WriteLn;
    WriteLn('RÉSUMÉ (3 phrases):');

    resume := summarizer.Summarize(texte, 3);
    WriteLn(resume);

    WriteLn;
    WriteLn('RÉSUMÉ (30% du texte):');
    resume := summarizer.SummarizeByRatio(texte, 0.3);
    WriteLn(resume);
  finally
    summarizer.Free;
  end;
end.
```

---

## Correction Orthographique et Suggestions

### Algorithme de correction par distance de Levenshtein

```pascal
unit SpellChecker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, TextSimilarity;

type
  TWordSuggestion = record
    Word: string;
    Distance: Integer;
    Probability: Double;
  end;

  TSuggestionList = specialize TList<TWordSuggestion>;

  TSpellChecker = class
  private
    FDictionary: TStringList;
    FWordFrequency: TWordFrequencyMap;
    FSimilarity: TTextSimilarity;

    function GetSuggestions(const AWord: string; AMaxDistance: Integer): TSuggestionList;
    function CalculateProbability(const AWord: string): Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadDictionary(const AFileName: string);
    procedure AddWord(const AWord: string; AFrequency: Integer = 1);
    function IsCorrect(const AWord: string): Boolean;
    function Correct(const AWord: string): string;
    function GetCorrections(const AWord: string; ACount: Integer): TStringList;
  end;

implementation

constructor TSpellChecker.Create;
begin
  FDictionary := TStringList.Create;
  FDictionary.Sorted := True;
  FDictionary.Duplicates := dupIgnore;

  FWordFrequency := TWordFrequencyMap.Create;
  FSimilarity := TTextSimilarity.Create;

  // Charger quelques mots de base
  AddWord('le', 1000);
  AddWord('la', 1000);
  AddWord('les', 1000);
  AddWord('un', 800);
  AddWord('une', 800);
  AddWord('chat', 100);
  AddWord('chien', 100);
  AddWord('maison', 150);
  AddWord('voiture', 120);
  AddWord('ordinateur', 90);
  AddWord('internet', 85);
  AddWord('bonjour', 200);
  AddWord('merci', 180);
end;

destructor TSpellChecker.Destroy;
begin
  FDictionary.Free;
  FWordFrequency.Free;
  FSimilarity.Free;
  inherited;
end;

procedure TSpellChecker.LoadDictionary(const AFileName: string);
var
  sl: TStringList;
  i, eqPos: Integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);

    for i := 0 to sl.Count - 1 do
    begin
      // Format: mot=fréquence
      eqPos := Pos('=', sl[i]);
      if eqPos > 0 then
        AddWord(Copy(sl[i], 1, eqPos - 1),
                StrToIntDef(Copy(sl[i], eqPos + 1, Length(sl[i]) - eqPos), 1))
      else
        AddWord(sl[i], 1);
    end;
  finally
    sl.Free;
  end;
end;

procedure TSpellChecker.AddWord(const AWord: string; AFrequency: Integer);
var
  word: string;
begin
  word := LowerCase(Trim(AWord));
  if word = '' then Exit;

  FDictionary.Add(word);
  FWordFrequency.AddOrSetValue(word, AFrequency);
end;

function TSpellChecker.IsCorrect(const AWord: string): Boolean;
begin
  Result := FDictionary.IndexOf(LowerCase(AWord)) >= 0;
end;

function TSpellChecker.CalculateProbability(const AWord: string): Double;
var
  frequency, totalFreq: Integer;
  freq: Integer;
begin
  if not FWordFrequency.TryGetValue(LowerCase(AWord), frequency) then
  begin
    Result := 0;
    Exit;
  end;

  // Calculer la fréquence totale
  totalFreq := 0;
  for freq in FWordFrequency.Values do
    totalFreq := totalFreq + freq;

  if totalFreq = 0 then
    Result := 0
  else
    Result := frequency / totalFreq;
end;

function TSpellChecker.GetSuggestions(const AWord: string;
                                       AMaxDistance: Integer): TSuggestionList;
var
  i: Integer;
  dictWord: string;
  distance: Integer;
  suggestion: TWordSuggestion;
begin
  Result := TSuggestionList.Create;

  for i := 0 to FDictionary.Count - 1 do
  begin
    dictWord := FDictionary[i];
    distance := FSimilarity.LevenshteinDistance(LowerCase(AWord), dictWord);

    if distance <= AMaxDistance then
    begin
      suggestion.Word := dictWord;
      suggestion.Distance := distance;
      suggestion.Probability := CalculateProbability(dictWord);
      Result.Add(suggestion);
    end;
  end;
end;

function TSpellChecker.Correct(const AWord: string): string;
var
  suggestions: TSuggestionList;
  i, j: Integer;
  temp: TWordSuggestion;
  bestSuggestion: TWordSuggestion;
begin
  // Si le mot est correct, le retourner tel quel
  if IsCorrect(AWord) then
  begin
    Result := AWord;
    Exit;
  end;

  // Chercher des suggestions avec distance maximale de 2
  suggestions := GetSuggestions(AWord, 2);
  try
    if suggestions.Count = 0 then
    begin
      Result := AWord; // Pas de correction trouvée
      Exit;
    end;

    // Trier par distance croissante puis probabilité décroissante
    for i := 0 to suggestions.Count - 2 do
      for j := i + 1 to suggestions.Count - 1 do
      begin
        if (suggestions[j].Distance < suggestions[i].Distance) or
           ((suggestions[j].Distance = suggestions[i].Distance) and
            (suggestions[j].Probability > suggestions[i].Probability)) then
        begin
          temp := suggestions[i];
          suggestions[i] := suggestions[j];
          suggestions[j] := temp;
        end;
      end;

    Result := suggestions[0].Word;
  finally
    suggestions.Free;
  end;
end;

function TSpellChecker.GetCorrections(const AWord: string;
                                       ACount: Integer): TStringList;
var
  suggestions: TSuggestionList;
  i, j: Integer;
  temp: TWordSuggestion;
begin
  Result := TStringList.Create;

  if IsCorrect(AWord) then
    Exit;

  suggestions := GetSuggestions(AWord, 2);
  try
    // Trier par pertinence
    for i := 0 to suggestions.Count - 2 do
      for j := i + 1 to suggestions.Count - 1 do
      begin
        if (suggestions[j].Distance < suggestions[i].Distance) or
           ((suggestions[j].Distance = suggestions[i].Distance) and
            (suggestions[j].Probability > suggestions[i].Probability)) then
        begin
          temp := suggestions[i];
          suggestions[i] := suggestions[j];
          suggestions[j] := temp;
        end;
      end;

    // Retourner les N meilleures suggestions
    for i := 0 to Min(ACount - 1, suggestions.Count - 1) do
      Result.Add(suggestions[i].Word);
  finally
    suggestions.Free;
  end;
end;

end.
```

**Exemple d'utilisation :**

```pascal
program TestSpellChecker;

uses
  SysUtils, SpellChecker;

var
  checker: TSpellChecker;
  corrections: TStringList;
  i: Integer;
  motsFautes: array[0..4] of string =
    ('chta', 'ordiateur', 'bojour', 'mersi', 'internot');

begin
  checker := TSpellChecker.Create;
  try
    WriteLn('=== Test du correcteur orthographique ===');
    WriteLn;

    for i := 0 to High(motsFautes) do
    begin
      WriteLn('Mot erroné: "', motsFautes[i], '"');
      WriteLn('Correction: "', checker.Correct(motsFautes[i]), '"');

      WriteLn('Suggestions:');
      corrections := checker.GetCorrections(motsFautes[i], 3);
      try
        for j := 0 to corrections.Count - 1 do
          WriteLn('  ', j + 1, '. ', corrections[j]);
      finally
        corrections.Free;
      end;
      WriteLn;
    end;
  finally
    checker.Free;
  end;
end.
```

---

## Application Pratique Multi-plateforme

### Analyseur de documents avec interface Lazarus

Voici une application complète qui combine plusieurs techniques NLP :

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus,
  TextNormalization, SimpleTokenizer, WordFrequency, StopWords,
  SentimentAnalysis, NamedEntityRecognition, TextSummarizer, SpellChecker;

type
  TFormMain = class(TForm)
    ButtonAnalyze: TButton;
    ButtonLoadFile: TButton;
    MemoInput: TMemo;
    MemoOutput: TMemo;
    PageControl1: TPageControl;
    TabSheetFrequency: TTabSheet;
    TabSheetSentiment: TTabSheet;
    TabSheetEntities: TTabSheet;
    TabSheetSummary: TTabSheet;
    ListViewFrequency: TListView;
    MemoSentiment: TMemo;
    ListViewEntities: TListView;
    MemoSummary: TMemo;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;

    procedure ButtonAnalyzeClick(Sender: TObject);
    procedure ButtonLoadFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTokenizer: TTokenizer;
    FNormalizer: TTextNormalizer;
    FFrequency: TWordFrequency;
    FStopWords: TStopWordFilter;
    FSentiment: TSentimentAnalyzer;
    FNER: TNamedEntityRecognizer;
    FSummarizer: TTextSummarizer;

    procedure AnalyzeFrequency;
    procedure AnalyzeSentiment;
    procedure ExtractEntities;
    procedure GenerateSummary;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FTokenizer := TTokenizer.Create;
  FNormalizer := TTextNormalizer.Create;
  FFrequency := TWordFrequency.Create;
  FStopWords := TStopWordFilter.Create('fr');
  FSentiment := TSentimentAnalyzer.Create;
  FNER := TNamedEntityRecognizer.Create;
  FSummarizer := TTextSummarizer.Create;

  // Configuration de la ListView pour les fréquences
  ListViewFrequency.Columns.Add.Caption := 'Mot';
  ListViewFrequency.Columns.Add.Caption := 'Fréquence';
  ListViewFrequency.Columns[0].Width := 200;
  ListViewFrequency.Columns[1].Width := 100;

  // Configuration de la ListView pour les entités
  ListViewEntities.Columns.Add.Caption := 'Type';
  ListViewEntities.Columns.Add.Caption := 'Texte';
  ListViewEntities.Columns.Add.Caption := 'Position';
  ListViewEntities.Columns[0].Width := 120;
  ListViewEntities.Columns[1].Width := 200;
  ListViewEntities.Columns[2].Width := 80;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FTokenizer.Free;
  FNormalizer.Free;
  FFrequency.Free;
  FStopWords.Free;
  FSentiment.Free;
  FNER.Free;
  FSummarizer.Free;
end;

procedure TFormMain.ButtonLoadFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      MemoInput.Lines.LoadFromFile(OpenDialog1.FileName);
      StatusBar1.SimpleText := 'Fichier chargé: ' + OpenDialog1.FileName;
    except
      on E: Exception do
        ShowMessage('Erreur lors du chargement: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.ButtonAnalyzeClick(Sender: TObject);
begin
  if Trim(MemoInput.Text) = '' then
  begin
    ShowMessage('Veuillez entrer ou charger un texte à analyser.');
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    AnalyzeFrequency;
    AnalyzeSentiment;
    ExtractEntities;
    GenerateSummary;

    StatusBar1.SimpleText := 'Analyse terminée';
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormMain.AnalyzeFrequency;
var
  tokens, filtered: TStringList;
  mostCommon: TStringList;
  i, colonPos: Integer;
  item: TListItem;
begin
  ListViewFrequency.Items.Clear;

  // Tokeniser et filtrer
  tokens := FTokenizer.Tokenize(MemoInput.Text);
  try
    filtered := FStopWords.FilterStopWords(tokens);
    try
      // Analyser les fréquences
      FFrequency.AddWords(filtered);

      // Obtenir les 20 mots les plus fréquents
      mostCommon := FFrequency.GetMostCommon(20);
      try
        for i := 0 to mostCommon.Count - 1 do
        begin
          colonPos := Pos(':', mostCommon[i]);
          if colonPos > 0 then
          begin
            item := ListViewFrequency.Items.Add;
            item.Caption := Trim(Copy(mostCommon[i], 1, colonPos - 1));
            item.SubItems.Add(Trim(Copy(mostCommon[i], colonPos + 1,
              Length(mostCommon[i]) - colonPos)));
          end;
        end;
      finally
        mostCommon.Free;
      end;
    finally
      filtered.Free;
    end;
  finally
    tokens.Free;
  end;
end;

procedure TFormMain.AnalyzeSentiment;
var
  score: TSentimentScore;
begin
  MemoSentiment.Clear;

  score := FSentiment.AnalyzeText(MemoInput.Text);

  MemoSentiment.Lines.Add('=== ANALYSE DE SENTIMENT ===');
  MemoSentiment.Lines.Add('');
  MemoSentiment.Lines.Add('Mots positifs trouvés: ' + FloatToStr(score.Positive));
  MemoSentiment.Lines.Add('Mots négatifs trouvés: ' + FloatToStr(score.Negative));
  MemoSentiment.Lines.Add('Mots neutres: ' + FloatToStr(score.Neutral));
  MemoSentiment.Lines.Add('');
  MemoSentiment.Lines.Add('Score global: ' + FormatFloat('0.00', score.Overall));
  MemoSentiment.Lines.Add('');
  MemoSentiment.Lines.Add('Sentiment détecté: ' + FSentiment.GetSentimentLabel(score));

  if score.Overall > 0 then
    MemoSentiment.Font.Color := clGreen
  else if score.Overall < 0 then
    MemoSentiment.Font.Color := clRed
  else
    MemoSentiment.Font.Color := clBlack;
end;

procedure TFormMain.ExtractEntities;
var
  entities: TEntityList;
  i: Integer;
  item: TListItem;
begin
  ListViewEntities.Items.Clear;

  entities := FNER.ExtractEntities(MemoInput.Text);
  try
    for i := 0 to entities.Count - 1 do
    begin
      item := ListViewEntities.Items.Add;
      item.Caption := FNER.EntityTypeToString(entities[i].EntityType);
      item.SubItems.Add(entities[i].Text);
      item.SubItems.Add(IntToStr(entities[i].Position));
    end;
  finally
    entities.Free;
  end;
end;

procedure TFormMain.GenerateSummary;
var
  summary: string;
begin
  MemoSummary.Clear;

  summary := FSummarizer.SummarizeByRatio(MemoInput.Text, 0.3);

  MemoSummary.Lines.Add('=== RÉSUMÉ AUTOMATIQUE (30% du texte) ===');
  MemoSummary.Lines.Add('');
  MemoSummary.Lines.Add(summary);
end;

end.
```

### Gestion multi-plateforme

**Chemins de dictionnaires :**

```pascal
function GetDictionaryPath: string;
begin
  {$IFDEF WINDOWS}
  Result := ExtractFilePath(ParamStr(0)) + 'dictionaries\';
  {$ENDIF}
  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + '/.nlp_app/dictionaries/';
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetStopWordsFile(const ALanguage: string): string;
begin
  Result := GetDictionaryPath + 'stopwords_' + ALanguage + '.txt';

  // Créer le fichier par défaut s'il n'existe pas
  if not FileExists(Result) then
    CreateDefaultStopWords(Result, ALanguage);
end;

procedure CreateDefaultStopWords(const AFileName, ALanguage: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    if ALanguage = 'fr' then
    begin
      sl.Add('le');
      sl.Add('la');
      sl.Add('les');
      sl.Add('un');
      sl.Add('une');
      sl.Add('des');
      sl.Add('de');
      sl.Add('du');
      sl.Add('et');
      sl.Add('ou');
      sl.Add('mais');
      sl.Add('dans');
      sl.Add('sur');
      sl.Add('pour');
      sl.Add('avec');
      sl.Add('par');
      sl.Add('ce');
      sl.Add('cette');
      sl.Add('ces');
      sl.Add('mon');
      sl.Add('ma');
      sl.Add('mes');
      sl.Add('ton');
      sl.Add('ta');
      sl.Add('tes');
      sl.Add('son');
      sl.Add('sa');
      sl.Add('ses');
      sl.Add('être');
      sl.Add('avoir');
      sl.Add('faire');
    end
    else if ALanguage = 'en' then
    begin
      sl.Add('the');
      sl.Add('a');
      sl.Add('an');
      sl.Add('and');
      sl.Add('or');
      sl.Add('but');
      sl.Add('in');
      sl.Add('on');
      sl.Add('at');
      sl.Add('to');
      sl.Add('for');
      sl.Add('of');
      sl.Add('with');
      sl.Add('by');
      sl.Add('from');
    end;

    sl.SaveToFile(AFileName);
  finally
    sl.Free;
  end;
end;
```

### Gestion de l'encodage selon la plateforme

```pascal
unit PlatformTextUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8;

type
  TPlatformTextUtils = class
  public
    class function LoadTextFile(const AFileName: string): string;
    class procedure SaveTextFile(const AFileName, AText: string);
    class function GetLineEnding: string;
    class function NormalizePath(const APath: string): string;
  end;

implementation

class function TPlatformTextUtils.LoadTextFile(const AFileName: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

class procedure TPlatformTextUtils.SaveTextFile(const AFileName, AText: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := AText;
    sl.SaveToFile(AFileName);
  finally
    sl.Free;
  end;
end;

class function TPlatformTextUtils.GetLineEnding: string;
begin
  {$IFDEF WINDOWS}
  Result := #13#10;  // CRLF
  {$ENDIF}
  {$IFDEF LINUX}
  Result := #10;     // LF
  {$ENDIF}
end;

class function TPlatformTextUtils.NormalizePath(const APath: string): string;
begin
  Result := APath;

  {$IFDEF WINDOWS}
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
  {$ENDIF}
end;

end.
```

---

## Intégration avec des APIs de NLP

### Utiliser des services externes (Google Cloud, OpenAI, etc.)

```pascal
unit NLPAPIClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser;

type
  TNLPAPIClient = class
  private
    FAPIKey: string;
    FHTTPClient: TFPHTTPClient;
  public
    constructor Create(const AAPIKey: string);
    destructor Destroy; override;

    function AnalyzeSentiment(const AText: string): Double;
    function ExtractEntities(const AText: string): TJSONArray;
    function TranslateText(const AText, ASourceLang, ATargetLang: string): string;
  end;

implementation

constructor TNLPAPIClient.Create(const AAPIKey: string);
begin
  FAPIKey := AAPIKey;
  FHTTPClient := TFPHTTPClient.Create(nil);
end;

destructor TNLPAPIClient.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TNLPAPIClient.AnalyzeSentiment(const AText: string): Double;
var
  requestData, response: string;
  jsonData: TJSONData;
  jsonObj: TJSONObject;
begin
  Result := 0;

  // Exemple pour une API hypothétique
  requestData := Format('{"text": "%s"}', [AText]);

  try
    FHTTPClient.RequestBody := TStringStream.Create(requestData);
    FHTTPClient.AddHeader('Content-Type', 'application/json');
    FHTTPClient.AddHeader('Authorization', 'Bearer ' + FAPIKey);

    response := FHTTPClient.Post('https://api.example.com/sentiment');

    jsonData := GetJSON(response);
    try
      if jsonData is TJSONObject then
      begin
        jsonObj := TJSONObject(jsonData);
        Result := jsonObj.Get('sentiment', 0.0);
      end;
    finally
      jsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur API: ', E.Message);
  end;
end;

function TNLPAPIClient.ExtractEntities(const AText: string): TJSONArray;
var
  requestData, response: string;
  jsonData: TJSONData;
begin
  Result := TJSONArray.Create;

  requestData := Format('{"text": "%s"}', [AText]);

  try
    FHTTPClient.RequestBody := TStringStream.Create(requestData);
    FHTTPClient.AddHeader('Content-Type', 'application/json');
    FHTTPClient.AddHeader('Authorization', 'Bearer ' + FAPIKey);

    response := FHTTPClient.Post('https://api.example.com/entities');

    jsonData := GetJSON(response);
    try
      if jsonData is TJSONArray then
        Result := TJSONArray(jsonData.Clone);
    finally
      jsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur API: ', E.Message);
  end;
end;

function TNLPAPIClient.TranslateText(const AText, ASourceLang,
                                      ATargetLang: string): string;
var
  requestData, response: string;
  jsonData: TJSONData;
  jsonObj: TJSONObject;
begin
  Result := AText; // Par défaut, retourner le texte original

  requestData := Format('{"text": "%s", "source": "%s", "target": "%s"}',
                        [AText, ASourceLang, ATargetLang]);

  try
    FHTTPClient.RequestBody := TStringStream.Create(requestData);
    FHTTPClient.AddHeader('Content-Type', 'application/json');
    FHTTPClient.AddHeader('Authorization', 'Bearer ' + FAPIKey);

    response := FHTTPClient.Post('https://api.example.com/translate');

    jsonData := GetJSON(response);
    try
      if jsonData is TJSONObject then
      begin
        jsonObj := TJSONObject(jsonData);
        Result := jsonObj.Get('translatedText', AText);
      end;
    finally
      jsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur API: ', E.Message);
  end;
end;

end.
```

---

## Optimisation et Performance

### Cache de résultats pour améliorer les performances

```pascal
unit NLPCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, md5;

type
  TCacheEntry = record
    Result: string;
    Timestamp: TDateTime;
  end;

  TCacheMap = specialize TDictionary<string, TCacheEntry>;

  TNLPCache = class
  private
    FCache: TCacheMap;
    FMaxAge: Integer; // En secondes
    FMaxSize: Integer;

    function GetHash(const AText: string): string;
    procedure CleanOldEntries;
  public
    constructor Create(AMaxAge: Integer = 3600; AMaxSize: Integer = 1000);
    destructor Destroy; override;

    function Get(const AKey: string; out AValue: string): Boolean;
    procedure Put(const AKey, AValue: string);
    procedure Clear;
    function GetStats: string;
  end;

implementation

constructor TNLPCache.Create(AMaxAge: Integer; AMaxSize: Integer);
begin
  FCache := TCacheMap.Create;
  FMaxAge := AMaxAge;
  FMaxSize := AMaxSize;
end;

destructor TNLPCache.Destroy;
begin
  FCache.Free;
  inherited;
end;

function TNLPCache.GetHash(const AText: string): string;
begin
  Result := MD5Print(MD5String(AText));
end;

procedure TNLPCache.CleanOldEntries;
var
  keysToRemove: TStringList;
  key: string;
  entry: TCacheEntry;
  now: TDateTime;
begin
  now := Now;
  keysToRemove := TStringList.Create;
  try
    // Identifier les entrées expirées
    for key in FCache.Keys do
    begin
      entry := FCache[key];
      if SecondsBetween(now, entry.Timestamp) > FMaxAge then
        keysToRemove.Add(key);
    end;

    // Supprimer les entrées expirées
    for key in keysToRemove do
      FCache.Remove(key);
  finally
    keysToRemove.Free;
  end;
end;

function TNLPCache.Get(const AKey: string; out AValue: string): Boolean;
var
  hash: string;
  entry: TCacheEntry;
begin
  hash := GetHash(AKey);
  Result := FCache.TryGetValue(hash, entry);

  if Result then
  begin
    // Vérifier si l'entrée n'est pas expirée
    if SecondsBetween(Now, entry.Timestamp) > FMaxAge then
    begin
      FCache.Remove(hash);
      Result := False;
    end
    else
      AValue := entry.Result;
  end;
end;

procedure TNLPCache.Put(const AKey, AValue: string);
var
  hash: string;
  entry: TCacheEntry;
begin
  // Nettoyer les vieilles entrées si nécessaire
  if FCache.Count >= FMaxSize then
    CleanOldEntries;

  // Si toujours plein, supprimer l'entrée la plus ancienne
  if FCache.Count >= FMaxSize then
    Clear;

  hash := GetHash(AKey);
  entry.Result := AValue;
  entry.Timestamp := Now;

  FCache.AddOrSetValue(hash, entry);
end;

procedure TNLPCache.Clear;
begin
  FCache.Clear;
end;

function TNLPCache.GetStats: string;
begin
  Result := Format('Cache: %d entrées, max: %d, age max: %d secondes',
                   [FCache.Count, FMaxSize, FMaxAge]);
end;

end.
```

### Utilisation du cache dans les analyseurs

```pascal
// Exemple avec l'analyseur de sentiment
type
  TCachedSentimentAnalyzer = class(TSentimentAnalyzer)
  private
    FCache: TNLPCache;
  public
    constructor Create;
    destructor Destroy; override;
    function AnalyzeText(const AText: string): TSentimentScore; override;
  end;

constructor TCachedSentimentAnalyzer.Create;
begin
  inherited Create;
  FCache := TNLPCache.Create(1800, 500); // 30 minutes, 500 entrées max
end;

destructor TCachedSentimentAnalyzer.Destroy;
begin
  FCache.Free;
  inherited;
end;

function TCachedSentimentAnalyzer.AnalyzeText(const AText: string): TSentimentScore;
var
  cached: string;
  score: Double;
begin
  // Vérifier le cache
  if FCache.Get(AText, cached) then
  begin
    Result.Overall := StrToFloatDef(cached, 0);
    Exit;
  end;

  // Calculer et mettre en cache
  Result := inherited AnalyzeText(AText);
  FCache.Put(AText, FloatToStr(Result.Overall));
end;
```

---

## Traitement Batch et Multi-threading

### Analyse en parallèle de multiples documents

```pascal
unit BatchProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, SimpleTokenizer, SentimentAnalysis;

type
  TDocumentResult = record
    FileName: string;
    WordCount: Integer;
    SentimentScore: Double;
    ProcessingTime: Integer; // En millisecondes
  end;

  TBatchProcessor = class(TThread)
  private
    FFiles: TStringList;
    FResults: array of TDocumentResult;
    FCurrentIndex: Integer;
    FLock: TCriticalSection;
    FOnProgress: TNotifyEvent;

    procedure ProcessDocument(const AFileName: string; AIndex: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(AFiles: TStringList);
    destructor Destroy; override;

    function GetResults: TStringList;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

implementation

constructor TBatchProcessor.Create(AFiles: TStringList);
begin
  inherited Create(True); // Créer suspendu
  FreeOnTerminate := False;

  FFiles := TStringList.Create;
  FFiles.Assign(AFiles);

  SetLength(FResults, FFiles.Count);
  FCurrentIndex := 0;
  FLock := TCriticalSection.Create;
end;

destructor TBatchProcessor.Destroy;
begin
  FFiles.Free;
  FLock.Free;
  inherited;
end;

procedure TBatchProcessor.ProcessDocument(const AFileName: string; AIndex: Integer);
var
  startTime: QWord;
  content: string;
  tokenizer: TTokenizer;
  analyzer: TSentimentAnalyzer;
  tokens: TStringList;
  score: TSentimentScore;
  sl: TStringList;
begin
  startTime := GetTickCount64;

  sl := TStringList.Create;
  tokenizer := TTokenizer.Create;
  analyzer := TSentimentAnalyzer.Create;
  try
    // Charger le document
    sl.LoadFromFile(AFileName);
    content := sl.Text;

    // Analyser
    tokens := tokenizer.Tokenize(content);
    try
      FResults[AIndex].WordCount := tokens.Count;
    finally
      tokens.Free;
    end;

    score := analyzer.AnalyzeText(content);
    FResults[AIndex].SentimentScore := score.Overall;

    FResults[AIndex].FileName := ExtractFileName(AFileName);
    FResults[AIndex].ProcessingTime := GetTickCount64 - startTime;
  finally
    sl.Free;
    tokenizer.Free;
    analyzer.Free;
  end;
end;

procedure TBatchProcessor.Execute;
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
  begin
    if Terminated then Break;

    ProcessDocument(FFiles[i], i);

    FLock.Enter;
    try
      FCurrentIndex := i + 1;
      if Assigned(FOnProgress) then
        Synchronize(@FOnProgress);
    finally
      FLock.Leave;
    end;
  end;
end;

function TBatchProcessor.GetResults: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.Add('Fichier;Mots;Sentiment;Temps (ms)');

  for i := 0 to High(FResults) do
  begin
    Result.Add(Format('%s;%d;%.2f;%d',
      [FResults[i].FileName,
       FResults[i].WordCount,
       FResults[i].SentimentScore,
       FResults[i].ProcessingTime]));
  end;
end;

end.
```

**Utilisation du processeur batch :**

```pascal
program BatchAnalysis;

uses
  SysUtils, Classes, BatchProcessor;

var
  files: TStringList;
  processor: TBatchProcessor;
  results: TStringList;

procedure OnProgress(Sender: TObject);
begin
  WriteLn('Traitement: ', TBatchProcessor(Sender).FCurrentIndex, '/',
          TBatchProcessor(Sender).FFiles.Count);
end;

begin
  files := TStringList.Create;
  try
    // Ajouter les fichiers à traiter
    files.Add('document1.txt');
    files.Add('document2.txt');
    files.Add('document3.txt');

    processor := TBatchProcessor.Create(files);
    try
      processor.OnProgress := @OnProgress;
      processor.Start;
      processor.WaitFor;

      // Récupérer les résultats
      results := processor.GetResults;
      try
        results.SaveToFile('batch_results.csv');
        WriteLn('Résultats sauvegardés dans batch_results.csv');
      finally
        results.Free;
      end;
    finally
      processor.Free;
    end;
  finally
    files.Free;
  end;
end.
```

---

## Ressources et Bibliothèques Utiles

### Bibliothèques FreePascal pour le NLP

**1. RegExpr - Expressions régulières**
```pascal
uses
  RegExpr;

var
  regex: TRegExpr;
begin
  regex := TRegExpr.Create;
  try
    regex.Expression := '\b\w+@\w+\.\w+\b';
    if regex.Exec('Contact: test@example.com') then
      WriteLn('Email trouvé: ', regex.Match[0]);
  finally
    regex.Free;
  end;
end;
```

**2. LazUTF8 - Gestion UTF-8**
```pascal
uses
  LazUTF8;

var
  texte: string;
begin
  texte := 'Café français';
  WriteLn('Longueur: ', UTF8Length(texte));
  WriteLn('Majuscules: ', UTF8UpperCase(texte));
end;
```

**3. fpJSON - Manipulation JSON**
```pascal
uses
  fpjson, jsonparser;

var
  jsonData: TJSONData;
  jsonObj: TJSONObject;
begin
  jsonData := GetJSON('{"text": "Hello", "lang": "en"}');
  try
    if jsonData is TJSONObject then
    begin
      jsonObj := TJSONObject(jsonData);
      WriteLn(jsonObj.Get('text', 'default'));
    end;
  finally
    jsonData.Free;
  end;
end;
```

### Dictionnaires et ressources linguistiques

**Sources de dictionnaires français :**
- **Lexique.org** : Base lexicale du français (http://www.lexique.org)
- **French Word List** : Liste de mots français
- **DELA** : Dictionnaires électroniques

**Formats de fichiers :**

```
# stopwords_fr.txt
le
la
les
un
une
des
...

# dictionary_fr.txt
mot=fréquence
le=10000
chat=500
ordinateur=300
...

# entities_persons.txt
Jean
Marie
Pierre
Sophie
...

# entities_locations.txt
Paris
Lyon
Marseille
France
...
```

---

## Exemples d'Applications Complètes

### 1. Analyseur de CV automatique

```pascal
program CVAnalyzer;

uses
  SysUtils, Classes,
  NamedEntityRecognition, SimpleTokenizer, WordFrequency;

type
  TCVData = record
    Name: string;
    Email: string;
    Phone: string;
    Skills: TStringList;
    Experience: Integer; // Années d'expérience
  end;

function AnalyzeCV(const AFileName: string): TCVData;
var
  sl: TStringList;
  content: string;
  ner: TNamedEntityRecognizer;
  entities: TEntityList;
  i: Integer;
  tokenizer: TTokenizer;
  tokens: TStringList;
begin
  Result.Skills := TStringList.Create;
  Result.Experience := 0;

  sl := TStringList.Create;
  ner := TNamedEntityRecognizer.Create;
  tokenizer := TTokenizer.Create;
  try
    sl.LoadFromFile(AFileName);
    content := sl.Text;

    // Extraire les entités
    entities := ner.ExtractEntities(content);
    try
      for i := 0 to entities.Count - 1 do
      begin
        case entities[i].EntityType of
          etPerson:
            if Result.Name = '' then
              Result.Name := entities[i].Text;
          etEmail:
            Result.Email := entities[i].Text;
          etPhone:
            Result.Phone := entities[i].Text;
        end;
      end;
    finally
      entities.Free;
    end;

    // Rechercher des compétences (mots-clés techniques)
    tokens := tokenizer.Tokenize(content);
    try
      for i := 0 to tokens.Count - 1 do
      begin
        if (Pos('pascal', LowerCase(tokens[i])) > 0) or
           (Pos('python', LowerCase(tokens[i])) > 0) or
           (Pos('java', LowerCase(tokens[i])) > 0) or
           (Pos('sql', LowerCase(tokens[i])) > 0) then
          if Result.Skills.IndexOf(tokens[i]) < 0 then
            Result.Skills.Add(tokens[i]);
      end;
    finally
      tokens.Free;
    end;

    // Estimer l'expérience (simpliste)
    if Pos('10 ans', content) > 0 then Result.Experience := 10
    else if Pos('5 ans', content) > 0 then Result.Experience := 5
    else if Pos('3 ans', content) > 0 then Result.Experience := 3;

  finally
    sl.Free;
    ner.Free;
    tokenizer.Free;
  end;
end;

var
  cvData: TCVData;
  i: Integer;

begin
  cvData := AnalyzeCV('cv_candidat.txt');
  try
    WriteLn('=== ANALYSE DE CV ===');
    WriteLn('Nom: ', cvData.Name);
    WriteLn('Email: ', cvData.Email);
    WriteLn('Téléphone: ', cvData.Phone);
    WriteLn('Expérience: ', cvData.Experience, ' ans');
    WriteLn('Compétences:');
    for i := 0 to cvData.Skills.Count - 1 do
      WriteLn('  - ', cvData.Skills[i]);
  finally
    cvData.Skills.Free;
  end;
end.
```

### 2. Détecteur de plagiat simple

```pascal
program PlagiarismDetector;

uses
  SysUtils, Classes, TextSimilarity;

function DetectPlagiarism(const AFile1, AFile2: string;
                          AThreshold: Double): Boolean;
var
  sl1, sl2: TStringList;
  similarity: TTextSimilarity;
  score: Double;
begin
  sl1 := TStringList.Create;
  sl2 := TStringList.Create;
  similarity := TTextSimilarity.Create;
  try
    sl1.LoadFromFile(AFile1);
    sl2.LoadFromFile(AFile2);

    score := similarity.CosineSimilarity(sl1.Text, sl2.Text);

    WriteLn(Format('Similarité: %.2f%%', [score * 100]));
    Result := score >= AThreshold;

    if Result then
      WriteLn('⚠ PLAGIAT DÉTECTÉ!')
    else
      WriteLn('✓ Documents suffisamment différents');
  finally
    sl1.Free;
    sl2.Free;
    similarity.Free;
  end;
end;

begin
  DetectPlagiarism('document1.txt', 'document2.txt', 0.8);
end.
```

---

## Conclusion et Perspectives

### Points clés à retenir

**Ce que vous avez appris :**
1. **Bases du NLP** : tokenisation, normalisation, n-grams
2. **Analyse statistique** : fréquences, TF-IDF, mesures de similarité
3. **Classification** : Naive Bayes pour catégoriser du texte
4. **Analyse de sentiment** : détection d'opinions positives/négatives
5. **Extraction d'information** : entités nommées, extraction de patterns
6. **Résumé automatique** : condensation de texte
7. **Correction orthographique** : distance de Levenshtein
8. **Optimisation** : cache, multi-threading, batch processing

### Prochaines étapes

**Pour aller plus loin :**

1. **Apprentissage profond**
   - Intégrer TensorFlow via Python4Lazarus
   - Utiliser des modèles pré-entraînés (BERT, GPT)
   - Fine-tuning de modèles sur vos données

2. **Traitement multilingue**
   - Détection automatique de langue
   - Traduction automatique
   - Dictionnaires multilingues

3. **NLP avancé**
   - Analyse syntaxique (parsing)
   - Désambiguïsation sémantique
   - Graphes de connaissances

4. **Applications spécialisées**
   - Chatbots intelligents
   - Systèmes de questions-réponses
   - Génération de texte
   - Analyse juridique ou médicale

### Ressources complémentaires

**Documentation et tutoriels :**
- Documentation FreePascal : https://www.freepascal.org/docs.html
- Forum FreePascal : https://forum.lazarus.freepascal.org/
- Lazarus Wiki : https://wiki.lazarus.freepascal.org/

**Livres et cours :**
- "Speech and Language Processing" (Jurafsky & Martin)
- "Natural Language Processing with Python" (adapté en Pascal)
- Cours Stanford CS224N (théorie applicable)

**Datasets et corpus :**
- Common Crawl
- Wikipedia dumps
- OpenSubtitles corpus
- French TreeBank

### Portabilité Windows/Linux

**Bonnes pratiques multi-plateformes :**

```pascal
{$IFDEF WINDOWS}
const
  PATH_SEPARATOR = '\';
  CONFIG_DIR = 'AppData\Local\MyNLPApp\';
{$ENDIF}

{$IFDEF LINUX}
const
  PATH_SEPARATOR = '/';
  CONFIG_DIR = '.config/mynlpapp/';
{$ENDIF}

function GetConfigPath: string;
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('USERPROFILE') + '\' + CONFIG_DIR;
  {$ENDIF}
  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + '/' + CONFIG_DIR;
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;
```

### Mot de la fin

Le traitement du langage naturel avec FreePascal/Lazarus offre un excellent compromis entre :
- **Performance** : code natif compilé très rapide
- **Portabilité** : Windows, Linux, macOS avec le même code
- **Simplicité** : syntaxe Pascal claire et lisible
- **Déploiement** : exécutables standalone sans dépendances

Vous disposez maintenant d'une base solide pour créer des applications NLP professionnelles. Les techniques présentées sont les fondations sur lesquelles reposent des systèmes plus complexes utilisant l'apprentissage automatique.

**Le NLP est un domaine en constante évolution**. Continuez à expérimenter, à apprendre et à contribuer à la communauté FreePascal !

---

## Annexe : Code complet d'une mini-application NLP

```pascal
program MiniNLPApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  SimpleTokenizer, WordFrequency, StopWords,
  SentimentAnalysis, TextSummarizer;

var
  tokenizer: TTokenizer;
  frequency: TWordFrequency;
  stopWords: TStopWordFilter;
  sentiment: TSentimentAnalyzer;
  summarizer: TTextSummarizer;

  texte: string;
  tokens, filtered, mostCommon: TStringList;
  score: TSentimentScore;
  resume: string;
  i: Integer;

begin
  WriteLn('========================================');
  WriteLn('     Mini Application NLP             ');
  WriteLn('     FreePascal/Lazarus               ');
  WriteLn('========================================');
  WriteLn;

  // Initialisation
  tokenizer := TTokenizer.Create;
  frequency := TWordFrequency.Create;
  stopWords := TStopWordFilter.Create('fr');
  sentiment := TSentimentAnalyzer.Create;
  summarizer := TTextSummarizer.Create;

  try
    // Texte d'exemple
    texte :=
      'L''intelligence artificielle révolutionne notre monde. ' +
      'Les algorithmes de machine learning sont vraiment excellents et permettent ' +
      'aux ordinateurs d''apprendre de manière autonome. Le deep learning utilise ' +
      'des réseaux de neurones artificiels très puissants. Ces technologies ' +
      'transforment de nombreux domaines comme la médecine, l''industrie et ' +
      'les transports. La médecine bénéficie de diagnostics plus précis et rapides. ' +
      'L''industrie automobile développe des voitures autonomes fantastiques. ' +
      'Les assistants vocaux comprennent le langage naturel de manière impressionnante. ' +
      'Cependant, l''IA soulève aussi des questions éthiques importantes. ' +
      'La protection de la vie privée est un enjeu majeur pour notre société. ' +
      'L''avenir de l''IA dépend d''une régulation appropriée et responsable.';

    WriteLn('TEXTE ORIGINAL:');
    WriteLn('---------------');
    WriteLn(texte);
    WriteLn;
    WriteLn('Longueur: ', Length(texte), ' caractères');
    WriteLn;

    // 1. TOKENISATION
    WriteLn('1. TOKENISATION');
    WriteLn('---------------');
    tokens := tokenizer.Tokenize(texte);
    WriteLn('Nombre de mots: ', tokens.Count);
    WriteLn('Premiers mots: ');
    for i := 0 to Min(9, tokens.Count - 1) do
      Write(tokens[i], ' ');
    WriteLn;
    WriteLn;

    // 2. FILTRAGE DES MOTS VIDES
    WriteLn('2. FILTRAGE DES MOTS VIDES');
    WriteLn('--------------------------');
    filtered := stopWords.FilterStopWords(tokens);
    WriteLn('Avant filtrage: ', tokens.Count, ' mots');
    WriteLn('Après filtrage: ', filtered.Count, ' mots');
    WriteLn('Mots supprimés: ', tokens.Count - filtered.Count);
    WriteLn;

    // 3. ANALYSE DE FRÉQUENCE
    WriteLn('3. ANALYSE DE FRÉQUENCE');
    WriteLn('-----------------------');
    frequency.AddWords(filtered);
    WriteLn('Mots uniques: ', frequency.GetUniqueWords);
    WriteLn('Total de mots: ', frequency.GetTotalWords);
    WriteLn;
    WriteLn('Top 10 des mots les plus fréquents:');
    mostCommon := frequency.GetMostCommon(10);
    for i := 0 to mostCommon.Count - 1 do
      WriteLn('  ', i + 1, '. ', mostCommon[i]);
    mostCommon.Free;
    WriteLn;

    // 4. ANALYSE DE SENTIMENT
    WriteLn('4. ANALYSE DE SENTIMENT');
    WriteLn('-----------------------');
    score := sentiment.AnalyzeText(texte);
    WriteLn('Mots positifs: ', score.Positive:0:0);
    WriteLn('Mots négatifs: ', score.Negative:0:0);
    WriteLn('Mots neutres: ', score.Neutral:0:0);
    WriteLn('Score global: ', score.Overall:0:3);
    WriteLn('Sentiment: ', sentiment.GetSentimentLabel(score));
    WriteLn;

    // 5. RÉSUMÉ AUTOMATIQUE
    WriteLn('5. RÉSUMÉ AUTOMATIQUE (30%)');
    WriteLn('---------------------------');
    resume := summarizer.SummarizeByRatio(texte, 0.3);
    WriteLn(resume);
    WriteLn;
    WriteLn('Réduction: ', Length(texte), ' → ', Length(resume), ' caractères');
    WriteLn('Taux de compression: ',
            ((1 - Length(resume) / Length(texte)) * 100):0:1, '%');
    WriteLn;

    // 6. STATISTIQUES GLOBALES
    WriteLn('6. STATISTIQUES GLOBALES');
    WriteLn('------------------------');
    WriteLn('Phrases: ~', Round(Length(texte) / 100), ' (estimation)');
    WriteLn('Mots par phrase: ', tokens.Count div 11);
    WriteLn('Caractères par mot: ', Length(texte) div tokens.Count);
    WriteLn('Richesse lexicale: ',
            (frequency.GetUniqueWords / frequency.GetTotalWords * 100):0:1, '%');
    WriteLn;

    WriteLn('========================================');
    WriteLn('     Analyse terminée avec succès!    ');
    WriteLn('========================================');

    // Cleanup
    tokens.Free;
    filtered.Free;

  finally
    tokenizer.Free;
    frequency.Free;
    stopWords.Free;
    sentiment.Free;
    summarizer.Free;
  end;

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Guide de Compilation et Déploiement

### Compilation sur Windows

**Méthode 1 : Avec Lazarus IDE**
1. Ouvrir le projet dans Lazarus
2. Menu Projet → Options du projet
3. Vérifier les chemins des unités
4. Menu Exécution → Compiler (F9)

**Méthode 2 : Ligne de commande**
```batch
fpc -Mobjfpc -Sh -O3 MiniNLPApp.pas
```

**Options importantes :**
- `-Mobjfpc` : Mode Object Pascal
- `-Sh` : Utiliser les chaînes AnsiString
- `-O3` : Optimisation maximale
- `-gl` : Informations de débogage
- `-Xs` : Lien statique (exécutable standalone)

### Compilation sur Linux/Ubuntu

**Installation de FreePascal :**
```bash
sudo apt update
sudo apt install fpc lazarus
```

**Compilation :**
```bash
fpc -Mobjfpc -Sh -O3 MiniNLPApp.pas
```

**Rendre exécutable :**
```bash
chmod +x MiniNLPApp
./MiniNLPApp
```

### Script de compilation multi-plateforme

**build.sh (Linux/macOS)**
```bash
#!/bin/bash

echo "Construction de MiniNLPApp..."

# Compiler
fpc -Mobjfpc -Sh -O3 -FuUnits MiniNLPApp.pas

if [ $? -eq 0 ]; then
    echo "✓ Compilation réussie!"
    chmod +x MiniNLPApp
    echo "Exécutable créé: ./MiniNLPApp"
else
    echo "✗ Erreur de compilation"
    exit 1
fi
```

**build.bat (Windows)**
```batch
@echo off
echo Construction de MiniNLPApp...

fpc -Mobjfpc -Sh -O3 -FuUnits MiniNLPApp.pas

if %ERRORLEVEL% EQU 0 (
    echo Compilation réussie!
    echo Exécutable créé: MiniNLPApp.exe
) else (
    echo Erreur de compilation
    exit /b 1
)
```

---

## Structure de Projet Recommandée

```
MonProjetNLP/
│
├── src/
│   ├── MiniNLPApp.pas              # Programme principal
│   ├── units/                      # Unités personnalisées
│   │   ├── SimpleTokenizer.pas
│   │   ├── WordFrequency.pas
│   │   ├── StopWords.pas
│   │   ├── SentimentAnalysis.pas
│   │   ├── TextSummarizer.pas
│   │   ├── NaiveBayesClassifier.pas
│   │   ├── TextSimilarity.pas
│   │   └── NamedEntityRecognition.pas
│   │
│   └── gui/                        # Interface graphique (optionnel)
│       ├── MainForm.pas
│       └── MainForm.lfm
│
├── data/                           # Données et ressources
│   ├── dictionaries/
│   │   ├── stopwords_fr.txt
│   │   ├── stopwords_en.txt
│   │   ├── dictionary_fr.txt
│   │   └── sentiment_lexicon.txt
│   │
│   ├── models/                     # Modèles entraînés
│   │   └── spam_classifier.txt
│   │
│   └── samples/                    # Exemples de textes
│       ├── sample1.txt
│       └── sample2.txt
│
├── tests/                          # Tests unitaires
│   ├── TestTokenizer.pas
│   ├── TestFrequency.pas
│   └── TestSentiment.pas
│
├── docs/                           # Documentation
│   ├── README.md
│   ├── API.md
│   └── TUTORIAL.md
│
├── build/                          # Scripts de compilation
│   ├── build.sh
│   ├── build.bat
│   └── Makefile
│
├── bin/                            # Exécutables compilés
│   ├── windows/
│   │   └── MiniNLPApp.exe
│   └── linux/
│       └── MiniNLPApp
│
├── lib/                            # Bibliothèques compilées
│   ├── x86_64-win64/
│   └── x86_64-linux/
│
└── README.md                       # Documentation principale
```

---

## Tests Unitaires

### Framework de tests simple

```pascal
unit TestFramework;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTestResult = record
    TestName: string;
    Passed: Boolean;
    Message: string;
  end;

  TTestSuite = class
  private
    FResults: array of TTestResult;
    FTestCount: Integer;
    FPassedCount: Integer;
  public
    procedure AddTest(const AName: string; APassed: Boolean; const AMessage: string = '');
    procedure PrintResults;
    function AllTestsPassed: Boolean;
  end;

implementation

procedure TTestSuite.AddTest(const AName: string; APassed: Boolean;
                             const AMessage: string);
var
  result: TTestResult;
begin
  result.TestName := AName;
  result.Passed := APassed;
  result.Message := AMessage;

  SetLength(FResults, Length(FResults) + 1);
  FResults[High(FResults)] := result;

  Inc(FTestCount);
  if APassed then
    Inc(FPassedCount);
end;

procedure TTestSuite.PrintResults;
var
  i: Integer;
begin
  WriteLn;
  WriteLn('========== RÉSULTATS DES TESTS ==========');
  WriteLn;

  for i := 0 to High(FResults) do
  begin
    if FResults[i].Passed then
      Write('✓ PASS: ')
    else
      Write('✗ FAIL: ');

    WriteLn(FResults[i].TestName);

    if FResults[i].Message <> '' then
      WriteLn('  → ', FResults[i].Message);
  end;

  WriteLn;
  WriteLn('=========================================');
  WriteLn(Format('Tests réussis: %d/%d (%.1f%%)',
    [FPassedCount, FTestCount, (FPassedCount / FTestCount * 100)]));
  WriteLn('=========================================');
end;

function TTestSuite.AllTestsPassed: Boolean;
begin
  Result := FPassedCount = FTestCount;
end;

end.
```

### Tests pour le Tokenizer

```pascal
program TestTokenizer;

uses
  SimpleTokenizer, TestFramework;

var
  suite: TTestSuite;
  tokenizer: TTokenizer;
  tokens: TStringList;

begin
  suite := TTestSuite.Create;
  tokenizer := TTokenizer.Create;

  try
    // Test 1: Tokenisation simple
    tokens := tokenizer.Tokenize('Le chat dort');
    suite.AddTest('Tokenisation simple',
                  tokens.Count = 3,
                  Format('Attendu: 3, Obtenu: %d', [tokens.Count]));
    tokens.Free;

    // Test 2: Gestion de la ponctuation
    tokens := tokenizer.Tokenize('Bonjour! Comment ça va?');
    suite.AddTest('Gestion ponctuation',
                  tokens.Count = 4,
                  Format('Attendu: 4, Obtenu: %d', [tokens.Count]));
    tokens.Free;

    // Test 3: Texte vide
    tokens := tokenizer.Tokenize('');
    suite.AddTest('Texte vide',
                  tokens.Count = 0,
                  Format('Attendu: 0, Obtenu: %d', [tokens.Count]));
    tokens.Free;

    // Test 4: Espaces multiples
    tokens := tokenizer.Tokenize('Un    chat     noir');
    suite.AddTest('Espaces multiples',
                  tokens.Count = 3,
                  Format('Attendu: 3, Obtenu: %d', [tokens.Count]));
    tokens.Free;

    // Test 5: Caractères accentués
    tokens := tokenizer.Tokenize('Café français');
    suite.AddTest('Caractères accentués',
                  (tokens.Count = 2) and (tokens[0] = 'Café'),
                  Format('Attendu: "Café", Obtenu: "%s"', [tokens[0]]));
    tokens.Free;

    suite.PrintResults;

    if suite.AllTestsPassed then
      WriteLn('✓ Tous les tests ont réussi!')
    else
      WriteLn('✗ Certains tests ont échoué');

  finally
    tokenizer.Free;
    suite.Free;
  end;

  {$IFDEF WINDOWS}
  ReadLn;
  {$ENDIF}
end.
```

---

## Performance et Benchmarking

### Mesurer les performances

```pascal
program BenchmarkNLP;

uses
  SysUtils, DateUtils,
  SimpleTokenizer, SentimentAnalysis, TextSummarizer;

type
  TBenchmarkResult = record
    OperationName: string;
    ExecutionTime: Int64;  // En microsecondes
    ItemsProcessed: Integer;
    ItemsPerSecond: Double;
  end;

procedure RunBenchmark(const AName: string; AIterations: Integer; AProc: TProcedure);
var
  startTime, endTime: TDateTime;
  result: TBenchmarkResult;
  elapsed: Int64;
begin
  result.OperationName := AName;
  result.ItemsProcessed := AIterations;

  // Warm-up
  AProc();

  // Mesure
  startTime := Now;
  AProc();
  endTime := Now;

  elapsed := MilliSecondsBetween(endTime, startTime);
  result.ExecutionTime := elapsed * 1000; // Convertir en microsecondes

  if elapsed > 0 then
    result.ItemsPerSecond := AIterations / (elapsed / 1000)
  else
    result.ItemsPerSecond := 0;

  WriteLn(Format('%-30s : %6d ms (%8.0f items/s)',
    [result.OperationName, elapsed, result.ItemsPerSecond]));
end;

var
  tokenizer: TTokenizer;
  sentiment: TSentimentAnalyzer;
  summarizer: TTextSummarizer;
  texte: string;
  i: Integer;

begin
  WriteLn('========== BENCHMARK NLP ==========');
  WriteLn;

  tokenizer := TTokenizer.Create;
  sentiment := TSentimentAnalyzer.Create;
  summarizer := TTextSummarizer.Create;

  try
    texte := 'L''intelligence artificielle transforme notre monde de manière extraordinaire. ';
    texte := texte + texte + texte; // Tripler le texte

    WriteLn('Texte de test: ', Length(texte), ' caractères');
    WriteLn;

    // Benchmark tokenisation
    RunBenchmark('Tokenisation (1000x)', 1000,
      procedure
      var
        j: Integer;
        tokens: TStringList;
      begin
        for j := 1 to 1000 do
        begin
          tokens := tokenizer.Tokenize(texte);
          tokens.Free;
        end;
      end
    );

    // Benchmark analyse de sentiment
    RunBenchmark('Analyse sentiment (1000x)', 1000,
      procedure
      var
        j: Integer;
        score: TSentimentScore;
      begin
        for j := 1 to 1000 do
          score := sentiment.AnalyzeText(texte);
      end
    );

    // Benchmark résumé
    RunBenchmark('Résumé automatique (100x)', 100,
      procedure
      var
        j: Integer;
        resume: string;
      begin
        for j := 1 to 100 do
          resume := summarizer.SummarizeByRatio(texte, 0.3);
      end
    );

    WriteLn;
    WriteLn('===================================');

  finally
    tokenizer.Free;
    sentiment.Free;
    summarizer.Free;
  end;

  {$IFDEF WINDOWS}
  ReadLn;
  {$ENDIF}
end.
```

---

## Conclusion Générale

### Ce que vous maîtrisez maintenant

Félicitations ! Vous avez parcouru un tutoriel complet sur le **traitement du langage naturel avec FreePascal/Lazarus**. Vous êtes désormais capable de :

✅ **Comprendre les concepts fondamentaux du NLP**
- Tokenisation et normalisation de texte
- Analyse de fréquence et statistiques
- Mesures de similarité

✅ **Implémenter des algorithmes de NLP**
- Classification avec Naive Bayes
- Analyse de sentiment
- Extraction d'entités nommées
- Résumé automatique
- Correction orthographique

✅ **Créer des applications complètes**
- Interface en ligne de commande
- Interface graphique avec Lazarus
- Traitement batch multi-threadé
- Optimisation avec cache

✅ **Développer de manière multi-plateforme**
- Code compatible Windows/Linux
- Gestion des chemins et encodages
- Déploiement sur différentes plateformes

### Avantages de FreePascal pour le NLP

**Performance :**
- Code natif compilé ultra-rapide
- Pas d'interpréteur (contrairement à Python)
- Gestion mémoire efficace

**Déploiement :**
- Exécutables standalone (pas de runtime)
- Faible empreinte mémoire
- Installation simplifiée

**Développement :**
- Syntaxe claire et lisible
- Typage fort évitant les erreurs
- IDE gratuit et complet (Lazarus)

**Portabilité :**
- Windows, Linux, macOS, BSD
- ARM (Raspberry Pi, mobile)
- Code source unique

### Applications pratiques réalisables

Avec les connaissances acquises, vous pouvez développer :

1. **Outils d'analyse de texte**
   - Analyseur de documents
   - Détecteur de plagiat
   - Extracteur d'informations

2. **Applications métier**
   - Analyseur de CV automatique
   - Classification d'emails
   - Analyse de feedback clients

3. **Outils linguistiques**
   - Correcteur orthographique
   - Détecteur de langue
   - Traducteur simple

4. **Systèmes intelligents**
   - Chatbot basique
   - Système de questions-réponses
   - Moteur de recommandation de contenu

### Ressources pour continuer

**Communauté FreePascal :**
- Forum : https://forum.lazarus.freepascal.org/
- Wiki : https://wiki.lazarus.freepascal.org/
- Documentation : https://www.freepascal.org/docs.html

**NLP et IA :**
- Stanford NLP Course (théorie)
- Papers With Code (algorithmes)
- Hugging Face (modèles pré-entraînés)

**Projets open source à étudier :**
- Lazarus IDE (exemple de grand projet)
- Brook Framework (web services)
- mORMot (SOA et ORM)

### Message final

Le traitement du langage naturel est un domaine passionnant en constante évolution. FreePascal vous offre les outils pour :

- **Apprendre** les fondamentaux du NLP
- **Prototyper** rapidement vos idées
- **Déployer** des applications performantes
- **Contribuer** à des projets open source

N'hésitez pas à :
- Expérimenter avec vos propres données
- Combiner différentes techniques
- Partager vos créations avec la communauté
- Poser des questions sur les forums

**Le voyage ne fait que commencer !**

Bonne continuation dans vos aventures NLP avec FreePascal/Lazarus ! 🚀

---

*Fin du tutoriel 15.4 - NLP et traitement de texte*

⏭️ [Algorithmes génétiques](/15-intelligence-artificielle-machine-learning/05-algorithmes-genetiques.md)
