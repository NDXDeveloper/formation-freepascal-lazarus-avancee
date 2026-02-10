🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.6 FFT et analyse spectrale

## Introduction

La transformée de Fourier rapide (FFT - Fast Fourier Transform) est l'un des algorithmes les plus importants du traitement du signal numérique. Elle permet de passer du domaine temporel (comment un signal évolue dans le temps) au domaine fréquentiel (quelles fréquences composent le signal). Cette transformation est essentielle dans de nombreux domaines : audio, télécommunications, médical, radar, sismologie, et bien d'autres.

## Qu'est-ce que la FFT ?

### Analogie simple

Imaginez que vous écoutez un orchestre :
- **Domaine temporel** : Vous entendez le son qui change au fil du temps
- **Domaine fréquentiel** : Vous identifiez les instruments individuels (violon aigu, contrebasse grave)

La FFT fait exactement cela avec n'importe quel signal : elle le décompose en ses fréquences constitutives.

### Pourquoi "rapide" ?

- **Transformée de Fourier classique (DFT)** : O(N²) opérations
- **Transformée de Fourier Rapide (FFT)** : O(N log N) opérations

Pour N = 1024 points :
- DFT : ~1 million d'opérations
- FFT : ~10 000 opérations (100× plus rapide !)

## Types de données pour la FFT

```pascal
unit UnitFFT;

{$mode objfpc}{$H+}

interface

uses
  Math, SysUtils;

type
  TFloat = Double;
  TSignal = array of TFloat;

  // Nombre complexe
  TComplexe = record
    Reel: TFloat;
    Imag: TFloat;
  end;

  TSpectre = array of TComplexe;

  // Résultats d'analyse spectrale
  TAnalyseSpectrale = record
    Frequences: TSignal;        // Fréquences en Hz
    Magnitudes: TSignal;        // Amplitudes
    Phases: TSignal;            // Phases en radians
    Puissance: TSignal;         // Densité spectrale de puissance
    FrequenceDominante: TFloat;
    PuissanceTotale: TFloat;
  end;

implementation

end.
```

## Opérations sur les nombres complexes

```pascal
// Addition
function AdditionComplexe(const a, b: TComplexe): TComplexe;
begin
  Result.Reel := a.Reel + b.Reel;
  Result.Imag := a.Imag + b.Imag;
end;

// Soustraction
function SoustractionComplexe(const a, b: TComplexe): TComplexe;
begin
  Result.Reel := a.Reel - b.Reel;
  Result.Imag := a.Imag - b.Imag;
end;

// Multiplication
function MultiplicationComplexe(const a, b: TComplexe): TComplexe;
begin
  Result.Reel := a.Reel * b.Reel - a.Imag * b.Imag;
  Result.Imag := a.Reel * b.Imag + a.Imag * b.Reel;
end;

// Module (magnitude)
function ModuleComplexe(const c: TComplexe): TFloat;
begin
  Result := Sqrt(Sqr(c.Reel) + Sqr(c.Imag));
end;

// Phase (argument)
function PhaseComplexe(const c: TComplexe): TFloat;
begin
  Result := ArcTan2(c.Imag, c.Reel);
end;

// Exponentielle complexe : e^(i*θ) = cos(θ) + i*sin(θ)
function ExponentielleComplexe(angle: TFloat): TComplexe;
begin
  Result.Reel := Cos(angle);
  Result.Imag := Sin(angle);
end;
```

## Implémentation de la FFT (Cooley-Tukey)

### FFT récursive

```pascal
procedure FFTRecursive(var data: TSpectre);
var
  n, k: Integer;
  pair, impair: TSpectre;
  t: TComplexe;
  angle: TFloat;
begin
  n := Length(data);

  // Cas de base
  if n <= 1 then
    Exit;

  // Diviser en parties paire et impaire
  SetLength(pair, n div 2);
  SetLength(impair, n div 2);

  for k := 0 to n div 2 - 1 do
  begin
    pair[k] := data[2 * k];
    impair[k] := data[2 * k + 1];
  end;

  // Récursion
  FFTRecursive(pair);
  FFTRecursive(impair);

  // Combiner les résultats
  for k := 0 to n div 2 - 1 do
  begin
    angle := -2 * Pi * k / n;
    t := MultiplicationComplexe(ExponentielleComplexe(angle), impair[k]);

    data[k] := AdditionComplexe(pair[k], t);
    data[k + n div 2] := SoustractionComplexe(pair[k], t);
  end;
end;
```

### FFT itérative (plus efficace)

```pascal
procedure FFTIterative(var data: TSpectre);
var
  n, bits, i, j, k: Integer;
  temp: TComplexe;
  m, mh: Integer;
  w, wm, t, u: TComplexe;
  angle: TFloat;
begin
  n := Length(data);

  // Vérifier que n est une puissance de 2
  if (n = 0) or ((n and (n - 1)) <> 0) then
    raise Exception.Create('La taille doit être une puissance de 2');

  // Réarrangement bit-reversed
  bits := Round(Log2(n));
  for i := 0 to n - 1 do
  begin
    j := InverserBits(i, bits);
    if j > i then
    begin
      temp := data[i];
      data[i] := data[j];
      data[j] := temp;
    end;
  end;

  // FFT itérative
  m := 2;
  while m <= n do
  begin
    mh := m div 2;
    angle := -2 * Pi / m;
    wm := ExponentielleComplexe(angle);

    for k := 0 to n - 1 do
    begin
      if k mod m = 0 then
      begin
        w.Reel := 1;
        w.Imag := 0;
      end;

      for j := 0 to mh - 1 do
      begin
        t := MultiplicationComplexe(w, data[k + j + mh]);
        u := data[k + j];

        data[k + j] := AdditionComplexe(u, t);
        data[k + j + mh] := SoustractionComplexe(u, t);

        w := MultiplicationComplexe(w, wm);
      end;
    end;

    m := m * 2;
  end;
end;

function InverserBits(valeur, nbBits: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to nbBits - 1 do
  begin
    if (valeur and (1 shl i)) <> 0 then
      Result := Result or (1 shl (nbBits - 1 - i));
  end;
end;
```

### FFT inverse (IFFT)

```pascal
procedure IFFTIterative(var data: TSpectre);
var
  i, n: Integer;
begin
  n := Length(data);

  // Conjuguer les données
  for i := 0 to n - 1 do
    data[i].Imag := -data[i].Imag;

  // Appliquer la FFT
  FFTIterative(data);

  // Conjuguer à nouveau et normaliser
  for i := 0 to n - 1 do
  begin
    data[i].Imag := -data[i].Imag;
    data[i].Reel := data[i].Reel / n;
    data[i].Imag := data[i].Imag / n;
  end;
end;
```

## Fonctions utilitaires

### Conversion signal réel → complexe

```pascal
function SignalVersSpectre(const signal: TSignal): TSpectre;
var
  i: Integer;
begin
  SetLength(Result, Length(signal));
  for i := 0 to High(signal) do
  begin
    Result[i].Reel := signal[i];
    Result[i].Imag := 0;
  end;
end;
```

### Extraction des magnitudes

```pascal
function ExtraireMagnitudes(const spectre: TSpectre): TSignal;
var
  i: Integer;
begin
  SetLength(Result, Length(spectre));
  for i := 0 to High(spectre) do
    Result[i] := ModuleComplexe(spectre[i]);
end;
```

### Extraction des phases

```pascal
function ExtrairePhases(const spectre: TSpectre): TSignal;
var
  i: Integer;
begin
  SetLength(Result, Length(spectre));
  for i := 0 to High(spectre) do
    Result[i] := PhaseComplexe(spectre[i]);
end;
```

### Densité spectrale de puissance

```pascal
function DensiteSpectralePuissance(const spectre: TSpectre): TSignal;
var
  i, n: Integer;
begin
  n := Length(spectre);
  SetLength(Result, n);

  for i := 0 to n - 1 do
    Result[i] := (Sqr(spectre[i].Reel) + Sqr(spectre[i].Imag)) / n;
end;
```

## Analyse spectrale complète

```pascal
function AnalyserSpectre(const signal: TSignal;
                        sampleRate: Integer): TAnalyseSpectrale;
var
  spectre: TSpectre;
  n, i, indexMax: Integer;
  maxMag: TFloat;
begin
  n := Length(signal);

  // Convertir en complexe et calculer la FFT
  spectre := SignalVersSpectre(signal);
  FFTIterative(spectre);

  // Extraire les informations (seulement la moitié positive du spectre)
  SetLength(Result.Frequences, n div 2);
  SetLength(Result.Magnitudes, n div 2);
  SetLength(Result.Phases, n div 2);
  SetLength(Result.Puissance, n div 2);

  Result.PuissanceTotale := 0;
  maxMag := 0;
  indexMax := 0;

  for i := 0 to n div 2 - 1 do
  begin
    // Fréquence correspondante
    Result.Frequences[i] := i * sampleRate / n;

    // Magnitude
    Result.Magnitudes[i] := ModuleComplexe(spectre[i]);

    // Phase
    Result.Phases[i] := PhaseComplexe(spectre[i]);

    // Puissance
    Result.Puissance[i] := Sqr(Result.Magnitudes[i]) / n;
    Result.PuissanceTotale := Result.PuissanceTotale + Result.Puissance[i];

    // Trouver la fréquence dominante (ignorer DC)
    if (i > 0) and (Result.Magnitudes[i] > maxMag) then
    begin
      maxMag := Result.Magnitudes[i];
      indexMax := i;
    end;
  end;

  Result.FrequenceDominante := Result.Frequences[indexMax];
end;

procedure AfficherAnalyseSpectrale(const analyse: TAnalyseSpectrale);
begin
  WriteLn('=== Analyse Spectrale ===');
  WriteLn(Format('Fréquence dominante : %.2f Hz',
                [analyse.FrequenceDominante]));
  WriteLn(Format('Puissance totale    : %.2f',
                [analyse.PuissanceTotale]));
  WriteLn;
end;
```

## Fenêtrage (Windowing)

Le fenêtrage réduit les artefacts spectraux (fuites spectrales).

### Fenêtre rectangulaire (pas de fenêtre)

```pascal
procedure FenetreRectangulaire(var signal: TSignal);
begin
  // Ne fait rien - le signal reste inchangé
end;
```

### Fenêtre de Hann (Hanning)

```pascal
procedure FenetreHann(var signal: TSignal);
var
  i, n: Integer;
  facteur: TFloat;
begin
  n := Length(signal);
  for i := 0 to n - 1 do
  begin
    facteur := 0.5 * (1 - Cos(2 * Pi * i / (n - 1)));
    signal[i] := signal[i] * facteur;
  end;
end;
```

### Fenêtre de Hamming

```pascal
procedure FenetreHamming(var signal: TSignal);
var
  i, n: Integer;
  facteur: TFloat;
begin
  n := Length(signal);
  for i := 0 to n - 1 do
  begin
    facteur := 0.54 - 0.46 * Cos(2 * Pi * i / (n - 1));
    signal[i] := signal[i] * facteur;
  end;
end;
```

### Fenêtre de Blackman

```pascal
procedure FenetreBlackman(var signal: TSignal);
var
  i, n: Integer;
  facteur: TFloat;
const
  A0 = 0.42;
  A1 = 0.5;
  A2 = 0.08;
begin
  n := Length(signal);
  for i := 0 to n - 1 do
  begin
    facteur := A0 - A1 * Cos(2 * Pi * i / (n - 1)) +
               A2 * Cos(4 * Pi * i / (n - 1));
    signal[i] := signal[i] * facteur;
  end;
end;
```

### Fenêtre de Kaiser

```pascal
function BesselI0(x: TFloat): TFloat;
var
  somme, terme: TFloat;
  k: Integer;
begin
  somme := 1;
  terme := 1;

  for k := 1 to 50 do
  begin
    terme := terme * Sqr(x / (2 * k));
    somme := somme + terme;
    if terme < 1e-10 then Break;
  end;

  Result := somme;
end;

procedure FenetreKaiser(var signal: TSignal; beta: TFloat);
var
  i, n: Integer;
  facteur, alpha: TFloat;
begin
  n := Length(signal);
  alpha := BesselI0(beta);

  for i := 0 to n - 1 do
  begin
    facteur := BesselI0(beta * Sqrt(1 - Sqr(2 * i / (n - 1) - 1))) / alpha;
    signal[i] := signal[i] * facteur;
  end;
end;
```

### Comparaison des fenêtres

```pascal
procedure ComparerFenetres(const signal: TSignal);
var
  s1, s2, s3, s4: TSignal;
  a1, a2, a3, a4: TAnalyseSpectrale;
begin
  // Copier le signal pour chaque test
  s1 := Copy(signal);
  s2 := Copy(signal);
  s3 := Copy(signal);
  s4 := Copy(signal);

  // Appliquer différentes fenêtres
  // s1 : Rectangulaire (aucun fenêtrage)
  FenetreHann(s2);
  FenetreHamming(s3);
  FenetreBlackman(s4);

  // Analyser
  a1 := AnalyserSpectre(s1, 44100);
  a2 := AnalyserSpectre(s2, 44100);
  a3 := AnalyserSpectre(s3, 44100);
  a4 := AnalyserSpectre(s4, 44100);

  WriteLn('Comparaison des fenêtres :');
  WriteLn(Format('Rectangulaire : %.2f Hz', [a1.FrequenceDominante]));
  WriteLn(Format('Hann          : %.2f Hz', [a2.FrequenceDominante]));
  WriteLn(Format('Hamming       : %.2f Hz', [a3.FrequenceDominante]));
  WriteLn(Format('Blackman      : %.2f Hz', [a4.FrequenceDominante]));
end;
```

## Spectrogramme

Analyse temps-fréquence qui montre comment le spectre évolue dans le temps.

```pascal
type
  TSpectrogramme = array of array of TFloat;

function CalculerSpectrogramme(const signal: TSignal;
                              tailleFenetre: Integer;
                              chevauchement: Integer;
                              sampleRate: Integer): TSpectrogramme;
var
  nbFenetres, i, j, debut: Integer;
  fenetre: TSignal;
  analyse: TAnalyseSpectrale;
  hop: Integer;
begin
  hop := tailleFenetre - chevauchement;
  nbFenetres := (Length(signal) - tailleFenetre) div hop + 1;

  SetLength(Result, nbFenetres, tailleFenetre div 2);

  for i := 0 to nbFenetres - 1 do
  begin
    debut := i * hop;

    // Extraire la fenêtre
    SetLength(fenetre, tailleFenetre);
    for j := 0 to tailleFenetre - 1 do
    begin
      if debut + j < Length(signal) then
        fenetre[j] := signal[debut + j]
      else
        fenetre[j] := 0;
    end;

    // Appliquer une fenêtre de Hann
    FenetreHann(fenetre);

    // Calculer le spectre
    analyse := AnalyserSpectre(fenetre, sampleRate);

    // Stocker les magnitudes (en dB)
    for j := 0 to tailleFenetre div 2 - 1 do
    begin
      if analyse.Magnitudes[j] > 0 then
        Result[i, j] := 20 * Log10(analyse.Magnitudes[j])
      else
        Result[i, j] := -100;  // Valeur minimale
    end;
  end;
end;

procedure AfficherSpectrogrammeConsole(const spectro: TSpectrogramme;
                                      nbLignes: Integer = 20);
var
  i, j: Integer;
  min, max, valeur: TFloat;
  caractere: Char;
  chars: String = ' .:-=+*#%@';
begin
  // Trouver min et max
  min := spectro[0, 0];
  max := spectro[0, 0];

  for i := 0 to High(spectro) do
    for j := 0 to High(spectro[0]) do
    begin
      if spectro[i, j] < min then min := spectro[i, j];
      if spectro[i, j] > max then max := spectro[i, j];
    end;

  WriteLn('Spectrogramme (temps → / fréquence ↑):');
  WriteLn;

  // Afficher (échantillonné)
  for j := High(spectro[0]) downto 0 do
  begin
    if j mod (Length(spectro[0]) div nbLignes) <> 0 then Continue;

    for i := 0 to Min(79, High(spectro)) do
    begin
      valeur := (spectro[i, j] - min) / (max - min);
      caractere := chars[Round(valeur * (Length(chars) - 1)) + 1];
      Write(caractere);
    end;
    WriteLn;
  end;
end;
```

## Applications pratiques

### Détection de fréquence (pitch detection)

```pascal
function DetecterFrequence(const signal: TSignal;
                          sampleRate: Integer): TFloat;
var
  analyse: TAnalyseSpectrale;
begin
  analyse := AnalyserSpectre(signal, sampleRate);
  Result := analyse.FrequenceDominante;
end;

function FrequenceVersNote(frequence: TFloat): String;
const
  NOTES: array[0..11] of String =
    ('Do', 'Do#', 'Ré', 'Ré#', 'Mi', 'Fa',
     'Fa#', 'Sol', 'Sol#', 'La', 'La#', 'Si');
var
  semiTons, noteIndex: Integer;
  frequenceLa: TFloat;
begin
  frequenceLa := 440.0;  // La4 = 440 Hz

  // Calculer le nombre de demi-tons par rapport au La
  semiTons := Round(12 * Log2(frequence / frequenceLa));
  noteIndex := (semiTons + 9) mod 12;  // +9 car La est la 10ème note

  if noteIndex < 0 then noteIndex := noteIndex + 12;

  Result := NOTES[noteIndex];
end;

// Exemple : Accordeur de guitare
procedure AccordeurGuitare(const signal: TSignal; sampleRate: Integer);
var
  freq: TFloat;
  note: String;
  ecart: TFloat;
  cordesGuitare: array[0..5] of TFloat = (82.41, 110.00, 146.83, 196.00, 246.94, 329.63);
  i, corde PlusProche: Integer;
  ecartMin: TFloat;
begin
  freq := DetecterFrequence(signal, sampleRate);
  note := FrequenceVersNote(freq);

  WriteLn(Format('Fréquence détectée : %.2f Hz (%s)', [freq, note]));

  // Trouver la corde la plus proche
  ecartMin := Abs(freq - cordesGuitare[0]);
  cordePlusProche := 0;

  for i := 1 to 5 do
  begin
    ecart := Abs(freq - cordesGuitare[i]);
    if ecart < ecartMin then
    begin
      ecartMin := ecart;
      cordePlusProche := i;
    end;
  end;

  ecart := freq - cordesGuitare[cordePlusProche];

  WriteLn(Format('Corde %d ', [cordePlusProche + 1]));
  if Abs(ecart) < 1 then
    WriteLn('✓ Accordé !')
  else if ecart > 0 then
    WriteLn(Format('↑ Trop haut de %.2f Hz', [ecart]))
  else
    WriteLn(Format('↓ Trop bas de %.2f Hz', [-ecart]));
end;
```

### Filtre fréquentiel

Filtrer des fréquences spécifiques dans le domaine fréquentiel.

```pascal
function FiltrePasse BasFFT(const signal: TSignal;
                            frequenceCoupure: TFloat;
                            sampleRate: Integer): TSignal;
var
  spectre: TSpectre;
  n, i: Integer;
  freq: TFloat;
begin
  n := Length(signal);

  // FFT
  spectre := SignalVersSpectre(signal);
  FFTIterative(spectre);

  // Filtrer les hautes fréquences
  for i := 0 to n div 2 do
  begin
    freq := i * sampleRate / n;
    if freq > frequenceCoupure then
    begin
      spectre[i].Reel := 0;
      spectre[i].Imag := 0;
      // Symétrie : filtrer aussi la partie négative
      spectre[n - i].Reel := 0;
      spectre[n - i].Imag := 0;
    end;
  end;

  // IFFT
  IFFTIterative(spectre);

  // Extraire la partie réelle
  SetLength(Result, n);
  for i := 0 to n - 1 do
    Result[i] := spectre[i].Reel;
end;

function FiltrePasseHautFFT(const signal: TSignal;
                           frequenceCoupure: TFloat;
                           sampleRate: Integer): TSignal;
var
  spectre: TSpectre;
  n, i: Integer;
  freq: TFloat;
begin
  n := Length(signal);

  spectre := SignalVersSpectre(signal);
  FFTIterative(spectre);

  // Filtrer les basses fréquences
  for i := 0 to n div 2 do
  begin
    freq := i * sampleRate / n;
    if freq < frequenceCoupure then
    begin
      spectre[i].Reel := 0;
      spectre[i].Imag := 0;
      spectre[n - i].Reel := 0;
      spectre[n - i].Imag := 0;
    end;
  end;

  IFFTIterative(spectre);

  SetLength(Result, n);
  for i := 0 to n - 1 do
    Result[i] := spectre[i].Reel;
end;

function FiltrePasseBandeFFT(const signal: TSignal;
                            freqMin, freqMax: TFloat;
                            sampleRate: Integer): TSignal;
var
  spectre: TSpectre;
  n, i: Integer;
  freq: TFloat;
begin
  n := Length(signal);

  spectre := SignalVersSpectre(signal);
  FFTIterative(spectre);

  // Garder seulement la bande souhaitée
  for i := 0 to n div 2 do
  begin
    freq := i * sampleRate / n;
    if (freq < freqMin) or (freq > freqMax) then
    begin
      spectre[i].Reel := 0;
      spectre[i].Imag := 0;
      spectre[n - i].Reel := 0;
      spectre[n - i].Imag := 0;
    end;
  end;

  IFFTIterative(spectre);

  SetLength(Result, n);
  for i := 0 to n - 1 do
    Result[i] := spectre[i].Reel;
end;
```

### Convolution rapide par FFT

La convolution dans le domaine temporel = multiplication dans le domaine fréquentiel.

```pascal
function ConvolutionFFT(const signal1, signal2: TSignal): TSignal;
var
  n, i: Integer;
  s1Pad, s2Pad: TSignal;
  spec1, spec2, specResult: TSpectre;
begin
  // Taille nécessaire pour éviter le repliement circulaire
  n := 1;
  while n < Length(signal1) + Length(signal2) - 1 do
    n := n * 2;

  // Zero-padding
  SetLength(s1Pad, n);
  SetLength(s2Pad, n);

  for i := 0 to High(signal1) do
    s1Pad[i] := signal1[i];
  for i := Length(signal1) to n - 1 do
    s1Pad[i] := 0;

  for i := 0 to High(signal2) do
    s2Pad[i] := signal2[i];
  for i := Length(signal2) to n - 1 do
    s2Pad[i] := 0;

  // FFT des deux signaux
  spec1 := SignalVersSpectre(s1Pad);
  spec2 := SignalVersSpectre(s2Pad);
  FFTIterative(spec1);
  FFTIterative(spec2);

  // Multiplication point par point
  SetLength(specResult, n);
  for i := 0 to n - 1 do
    specResult[i] := MultiplicationComplexe(spec1[i], spec2[i]);

  // IFFT
  IFFTIterative(specResult);

  // Extraire le résultat
  SetLength(Result, Length(signal1) + Length(signal2) - 1);
  for i := 0 to High(Result) do
    Result[i] := specResult[i].Reel;
end;
```

## Padding et optimisation

### Zero-padding

Augmenter la résolution fréquentielle en ajoutant des zéros.

```pascal
function ZeroPadding(const signal: TSignal; nouvelleTaille: Integer): TSignal;
var
  i: Integer;
begin
  SetLength(Result, nouvelleTaille);

  for i := 0 to Min(High(signal), nouvelleTaille - 1) do
    Result[i] := signal[i];

  for i := Length(signal) to nouvelleTaille - 1 do
    Result[i] := 0;
end;

// Arrondir à la prochaine puissance de 2
function ProchainePuissanceDe2(n: Integer): Integer;
begin
  Result := 1;
  while Result < n do
    Result := Result * 2;
end;
```

### Optimisation de la taille

```pascal
procedure OptimiserPourFFT(var signal: TSignal);
var
  nouvelleTaille: Integer;
begin
  nouvelleTaille := ProchainePuissanceDe2(Length(signal));

  if nouvelleTaille > Length(signal) then
  begin
    WriteLn(Format('Padding de %d à %d échantillons',
                  [Length(signal), nouvelleTaille]));
    signal := ZeroPadding(signal, nouvelleTaille);
  end;
end;
```

## Transformées spécialisées

### DCT (Discrete Cosine Transform)

Utilisée dans la compression JPEG et MP3.

```pascal
function DCT(const signal: TSignal): TSignal;
var
  n, k, i: Integer;
  somme, facteur: TFloat;
begin
  n := Length(signal);
  SetLength(Result, n);

  for k := 0 to n - 1 do
  begin
    somme := 0;
    for i := 0 to n - 1 do
      somme := somme + signal[i] * Cos(Pi * k * (i + 0.5) / n);

    if k = 0 then
      facteur := Sqrt(1 / n)
    else
      facteur := Sqrt(2 / n);

    Result[k] := facteur * somme;
  end;
end;

function IDCT(const coefficients: TSignal): TSignal;
var
  n, k, i: Integer;
  somme, facteur: TFloat;
begin
  n := Length(coefficients);
  SetLength(Result, n);

  for i := 0 to n - 1 do
  begin
    somme := 0;
    for k := 0 to n - 1 do
    begin
      if k = 0 then
        facteur := Sqrt(1 / n)
      else
        facteur := Sqrt(2 / n);

      somme := somme + facteur * coefficients[k] *
               Cos(Pi * k * (i + 0.5) / n);
    end;
    Result[i] := somme;
  end;
end;
```

### Transformée de Hilbert

Pour l'analyse de signaux analytiques.

```pascal
function TransformeeHilbert(const signal: TSignal): TSignal;
var
  spectre: TSpectre;
  n, i: Integer;
begin
  n := Length(signal);

  // FFT
  spectre := SignalVersSpectre(signal);
  FFTIterative(spectre);

  // Multiplier les fréquences positives par -i et négatives par +i
  for i := 1 to n div 2 - 1 do
  begin
    // Fréquences positives : multiplier par -i
    spectre[i] := MultiplicationComplexe(spectre[i],
                                        TComplexe.Create(0, -1));
    // Fréquences négatives : multiplier par +i
    spectre[n - i] := MultiplicationComplexe(spectre[n - i],
                                             TComplexe.Create(0, 1));
  end;

  // DC et Nyquist restent inchangés
  spectre[0].Reel := 0;
  spectre[0].Imag := 0;
  if n mod 2 = 0 then
  begin
    spectre[n div 2].Reel := 0;
    spectre[n div 2].Imag := 0;
  end;

  // IFFT
  IFFTIterative(spectre);

  // Extraire la partie réelle
  SetLength(Result, n);
  for i := 0 to n - 1 do
    Result[i] := spectre[i].Reel;
end;
```

## Analyse temps-fréquence avancée

### Transformée en ondelettes continues (CWT)

Alternative à la FFT pour signaux non-stationnaires.

```pascal
type
  TOndelette = (owMorlet, owMexicanHat, owHaar);

function OndeletteMorlet(t, scale: TFloat): TFloat;
const
  SIGMA = 1.0;
begin
  Result := Exp(-Sqr(t) / (2 * Sqr(SIGMA))) * Cos(5 * t / scale);
end;

function OndeletteMexicanHat(t, scale: TFloat): TFloat;
begin
  Result := (1 - Sqr(t / scale)) * Exp(-Sqr(t / scale) / 2);
end;

function TransformeeOndelettes(const signal: TSignal;
                              ondelette: TOndelette;
                              scales: TSignal): TMatrice;
var
  i, j, k, n: Integer;
  somme, t: TFloat;
  onde: TFloat;
begin
  n := Length(signal);
  SetLength(Result, Length(scales), n);

  for i := 0 to High(scales) do
  begin
    for j := 0 to n - 1 do
    begin
      somme := 0;
      for k := 0 to n - 1 do
      begin
        t := (k - j) / scales[i];

        case ondelette of
          owMorlet: onde := OndeletteMorlet(t, scales[i]);
          owMexicanHat: onde := OndeletteMexicanHat(t, scales[i]);
          owHaar: onde := OndeletteeHaar(t);
        end;

        somme := somme + signal[k] * onde;
      end;
      Result[i, j] := somme / Sqrt(scales[i]);
    end;
  end;
end;
```

### STFT (Short-Time Fourier Transform)

FFT sur des fenêtres glissantes.

```pascal
type
  TSTFT = record
    Temps: TSignal;
    Frequences: TSignal;
    Spectrogramme: TMatrice;
  end;

function STFT(const signal: TSignal;
             tailleFenetre, hop, sampleRate: Integer): TSTFT;
var
  nbFenetres, i, j, debut: Integer;
  fenetre: TSignal;
  spectre: TSpectre;
begin
  nbFenetres := (Length(signal) - tailleFenetre) div hop + 1;

  SetLength(Result.Temps, nbFenetres);
  SetLength(Result.Frequences, tailleFenetre div 2);
  SetLength(Result.Spectrogramme, nbFenetres, tailleFenetre div 2);

  // Calculer les axes
  for i := 0 to nbFenetres - 1 do
    Result.Temps[i] := i * hop / sampleRate;

  for i := 0 to tailleFenetre div 2 - 1 do
    Result.Frequences[i] := i * sampleRate / tailleFenetre;

  // Calculer le spectrogramme
  for i := 0 to nbFenetres - 1 do
  begin
    debut := i * hop;

    // Extraire la fenêtre
    SetLength(fenetre, tailleFenetre);
    for j := 0 to tailleFenetre - 1 do
    begin
      if debut + j < Length(signal) then
        fenetre[j] := signal[debut + j]
      else
        fenetre[j] := 0;
    end;

    // Fenêtrage
    FenetreHann(fenetre);

    // FFT
    spectre := SignalVersSpectre(fenetre);
    FFTIterative(spectre);

    // Stocker les magnitudes
    for j := 0 to tailleFenetre div 2 - 1 do
      Result.Spectrogramme[i, j] := ModuleComplexe(spectre[j]);
  end;
end;

function ISTFT(const stft: TSTFT; hop: Integer): TSignal;
var
  i, j, k, pos: Integer;
  fenetre: TSignal;
  spectre: TSpectre;
  longueurSignal: Integer;
  comptes: TSignal;  // Pour la normalisation
begin
  longueurSignal := Length(stft.Temps) * hop + Length(stft.Frequences) * 2;
  SetLength(Result, longueurSignal);
  SetLength(comptes, longueurSignal);

  // Initialiser
  for i := 0 to longueurSignal - 1 do
  begin
    Result[i] := 0;
    comptes[i] := 0;
  end;

  // Reconstruire
  for i := 0 to High(stft.Temps) do
  begin
    // Reconstruire le spectre complet
    SetLength(spectre, Length(stft.Frequences) * 2);
    for j := 0 to High(stft.Frequences) do
    begin
      spectre[j].Reel := stft.Spectrogramme[i, j];
      spectre[j].Imag := 0;
      // Symétrie hermitienne
      spectre[Length(spectre) - 1 - j].Reel := stft.Spectrogramme[i, j];
      spectre[Length(spectre) - 1 - j].Imag := 0;
    end;

    // IFFT
    IFFTIterative(spectre);

    // Extraire et accumuler
    pos := i * hop;
    for j := 0 to High(spectre) do
    begin
      if pos + j < longueurSignal then
      begin
        Result[pos + j] := Result[pos + j] + spectre[j].Reel;
        comptes[pos + j] := comptes[pos + j] + 1;
      end;
    end;
  end;

  // Normaliser
  for i := 0 to longueurSignal - 1 do
    if comptes[i] > 0 then
      Result[i] := Result[i] / comptes[i];
end;
```

## Analyse de bandes de fréquences

### Octaves et bandes tierces

```pascal
type
  TBandeFrequence = record
    FreqMin: TFloat;
    FreqMax: TFloat;
    FreqCentrale: TFloat;
    Puissance: TFloat;
  end;

function AnalyseParOctaves(const signal: TSignal;
                          sampleRate: Integer): array of TBandeFrequence;
var
  analyse: TAnalyseSpectrale;
  i, indexBande: Integer;
  freq, freqMin: TFloat;
  nbBandes: Integer;
begin
  analyse := AnalyserSpectre(signal, sampleRate);

  // Calculer le nombre de bandes (de 20 Hz à Nyquist)
  freqMin := 20;
  nbBandes := 0;
  freq := freqMin;
  while freq < sampleRate / 2 do
  begin
    Inc(nbBandes);
    freq := freq * 2;  // Octave suivante
  end;

  SetLength(Result, nbBandes);

  // Remplir les bandes
  freq := freqMin;
  for i := 0 to nbBandes - 1 do
  begin
    Result[i].FreqMin := freq;
    Result[i].FreqMax := freq * 2;
    Result[i].FreqCentrale := freq * Sqrt(2);  // Moyenne géométrique
    Result[i].Puissance := 0;

    // Sommer la puissance dans cette bande
    for indexBande := 0 to High(analyse.Puissance) do
    begin
      if (analyse.Frequences[indexBande] >= Result[i].FreqMin) and
         (analyse.Frequences[indexBande] < Result[i].FreqMax) then
        Result[i].Puissance := Result[i].Puissance + analyse.Puissance[indexBande];
    end;

    freq := freq * 2;
  end;
end;

procedure AfficherAnalyseOctaves(const bandes: array of TBandeFrequence);
var
  i: Integer;
  maxPuissance, puissanceDB: TFloat;
  barreGraphique: String;
  longueur, j: Integer;
begin
  WriteLn('=== Analyse par octaves ===');
  WriteLn;

  // Trouver la puissance maximale
  maxPuissance := 0;
  for i := 0 to High(bandes) do
    if bandes[i].Puissance > maxPuissance then
      maxPuissance := bandes[i].Puissance;

  // Afficher
  for i := 0 to High(bandes) do
  begin
    puissanceDB := 10 * Log10(bandes[i].Puissance + 1e-10);

    Write(Format('%6.0f Hz : ', [bandes[i].FreqCentrale]));

    // Barre graphique
    longueur := Round(40 * bandes[i].Puissance / maxPuissance);
    barreGraphique := '';
    for j := 1 to longueur do
      barreGraphique := barreGraphique + '█';

    WriteLn(Format('%-40s %.1f dB', [barreGraphique, puissanceDB]));
  end;
end;
```

### Égaliseur graphique

```pascal
function EgaliseurGraphique(const signal: TSignal;
                           const gains: array of TFloat;
                           sampleRate: Integer): TSignal;
var
  spectre: TSpectre;
  n, i, indexBande: Integer;
  freq, freqMin: TFloat;
  gain: TFloat;
begin
  n := Length(signal);

  // FFT
  spectre := SignalVersSpectre(signal);
  FFTIterative(spectre);

  // Appliquer les gains par octave
  freqMin := 20;
  for i := 0 to n div 2 do
  begin
    freq := i * sampleRate / n;

    // Déterminer la bande
    indexBande := 0;
    while (indexBande < High(gains)) and (freq >= freqMin * Power(2, indexBande + 1)) do
      Inc(indexBande);

    // Appliquer le gain (en dB)
    gain := Power(10, gains[indexBande] / 20);
    spectre[i].Reel := spectre[i].Reel * gain;
    spectre[i].Imag := spectre[i].Imag * gain;

    // Symétrie
    spectre[n - i].Reel := spectre[n - i].Reel * gain;
    spectre[n - i].Imag := spectre[n - i].Imag * gain;
  end;

  // IFFT
  IFFTIterative(spectre);

  // Extraire le résultat
  SetLength(Result, n);
  for i := 0 to n - 1 do
    Result[i] := spectre[i].Reel;
end;
```

## Visualisation avec TAChart

### Afficher le spectre de fréquences

```pascal
uses
  TAGraph, TASeries;

procedure AfficherSpectre(Chart: TChart; const signal: TSignal;
                         sampleRate: Integer);
var
  Serie: TLineSeries;
  analyse: TAnalyseSpectrale;
  i, maxIndex: Integer;
begin
  analyse := AnalyserSpectre(signal, sampleRate);

  Serie := TLineSeries.Create(Chart);
  Serie.Title := 'Spectre de fréquences';

  // Afficher jusqu'à 10 kHz (pour lisibilité)
  maxIndex := Round(10000 * Length(signal) / sampleRate);
  maxIndex := Min(maxIndex, High(analyse.Frequences));

  for i := 0 to maxIndex do
    Serie.AddXY(analyse.Frequences[i], analyse.Magnitudes[i]);

  Chart.AddSeries(Serie);
  Chart.LeftAxis.Title.Caption := 'Magnitude';
  Chart.BottomAxis.Title.Caption := 'Fréquence (Hz)';
end;

procedure AfficherSpectreDB(Chart: TChart; const signal: TSignal;
                           sampleRate: Integer);
var
  Serie: TLineSeries;
  analyse: TAnalyseSpectrale;
  i, maxIndex: Integer;
  magnitudeDB: TFloat;
begin
  analyse := AnalyserSpectre(signal, sampleRate);

  Serie := TLineSeries.Create(Chart);
  Serie.Title := 'Spectre (dB)';
  Serie.LinePen.Color := clBlue;

  maxIndex := Round(10000 * Length(signal) / sampleRate);
  maxIndex := Min(maxIndex, High(analyse.Frequences));

  for i := 1 to maxIndex do  // Ignorer DC
  begin
    if analyse.Magnitudes[i] > 0 then
      magnitudeDB := 20 * Log10(analyse.Magnitudes[i])
    else
      magnitudeDB := -120;  // Minimum

    Serie.AddXY(analyse.Frequences[i], magnitudeDB);
  end;

  Chart.AddSeries(Serie);
  Chart.LeftAxis.Title.Caption := 'Magnitude (dB)';
  Chart.BottomAxis.Title.Caption := 'Fréquence (Hz)';
end;
```

### Afficher le spectrogramme

```pascal
uses
  Graphics, IntfGraphics, FPImage;

procedure AfficherSpectrogramme(Image: TImage; const signal: TSignal;
                               tailleFenetre, chevauchement, sampleRate: Integer);
var
  spectro: TSpectrogramme;
  bitmap: TBitmap;
  i, j: Integer;
  min, max, valeur: TFloat;
  couleur: TColor;
begin
  spectro := CalculerSpectrogramme(signal, tailleFenetre, chevauchement, sampleRate);

  // Trouver min et max
  min := spectro[0, 0];
  max := spectro[0, 0];
  for i := 0 to High(spectro) do
    for j := 0 to High(spectro[0]) do
    begin
      if spectro[i, j] < min then min := spectro[i, j];
      if spectro[i, j] > max then max := spectro[i, j];
    end;

  // Créer l'image
  bitmap := TBitmap.Create;
  try
    bitmap.Width := Length(spectro);
    bitmap.Height := Length(spectro[0]);

    // Dessiner le spectrogramme
    for i := 0 to bitmap.Width - 1 do
    begin
      for j := 0 to bitmap.Height - 1 do
      begin
        valeur := (spectro[i, bitmap.Height - 1 - j] - min) / (max - min);

        // Palette de couleurs (du bleu au rouge)
        if valeur < 0.25 then
          couleur := RGB(0, 0, Round(valeur * 1024))
        else if valeur < 0.5 then
          couleur := RGB(0, Round((valeur - 0.25) * 1024), 255)
        else if valeur < 0.75 then
          couleur := RGB(Round((valeur - 0.5) * 1024), 255,
                        255 - Round((valeur - 0.5) * 1024))
        else
          couleur := RGB(255, 255 - Round((valeur - 0.75) * 1024), 0);

        bitmap.Canvas.Pixels[i, j] := couleur;
      end;
    end;

    Image.Picture.Assign(bitmap);
  finally
    bitmap.Free;
  end;
end;
```

## Application complète : Analyseur de spectre

```pascal
program AnalyseurSpectre;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls,
  TAGraph, TASeries, Spin, ComCtrls, UnitFFT;

type
  TFormAnalyseur = class(TForm)
    ChartSignal: TChart;
    ChartSpectre: TChart;
    ButtonGenerer: TButton;
    ButtonCharger: TButton;
    ComboBoxSignal: TComboBox;
    SpinEditFrequence: TSpinEdit;
    TrackBarVolume: TTrackBar;
    LabelInfo: TLabel;
    ComboBoxFenetre: TComboBox;
    CheckBoxDB: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure ButtonGenererClick(Sender: TObject);
    procedure ButtonChargerClick(Sender: TObject);
    procedure ComboBoxFenetreChange(Sender: TObject);
    procedure CheckBoxDBChange(Sender: TObject);
  private
    FSignal: TSignal;
    FSampleRate: Integer;
    procedure GenererSignalTest;
    procedure AnalyserEtAfficher;
  public

  end;

var
  FormAnalyseur: TFormAnalyseur;

implementation

{$R *.lfm}

procedure TFormAnalyseur.FormCreate(Sender: TObject);
begin
  FSampleRate := 44100;

  ComboBoxSignal.Items.Add('Sinus pur');
  ComboBoxSignal.Items.Add('Accord (Do-Mi-Sol)');
  ComboBoxSignal.Items.Add('Bruit blanc');
  ComboBoxSignal.Items.Add('Chirp (sweep)');
  ComboBoxSignal.ItemIndex := 0;

  ComboBoxFenetre.Items.Add('Rectangulaire');
  ComboBoxFenetre.Items.Add('Hann');
  ComboBoxFenetre.Items.Add('Hamming');
  ComboBoxFenetre.Items.Add('Blackman');
  ComboBoxFenetre.ItemIndex := 1;

  SpinEditFrequence.Value := 440;
  TrackBarVolume.Position := 50;
end;

procedure TFormAnalyseur.ButtonGenererClick(Sender: TObject);
begin
  GenererSignalTest;
  AnalyserEtAfficher;
end;

procedure TFormAnalyseur.GenererSignalTest;
var
  i: Integer;
  freq: TFloat;
  amplitude: TFloat;
begin
  amplitude := TrackBarVolume.Position / 100;
  freq := SpinEditFrequence.Value;

  SetLength(FSignal, FSampleRate);  // 1 seconde

  case ComboBoxSignal.ItemIndex of
    0: // Sinus pur
    begin
      for i := 0 to High(FSignal) do
        FSignal[i] := amplitude * Sin(2 * Pi * freq * i / FSampleRate);
    end;

    1: // Accord Do-Mi-Sol (approximatif)
    begin
      for i := 0 to High(FSignal) do
        FSignal[i] := amplitude * (
          Sin(2 * Pi * 261.63 * i / FSampleRate) +  // Do
          Sin(2 * Pi * 329.63 * i / FSampleRate) +  // Mi
          Sin(2 * Pi * 392.00 * i / FSampleRate)    // Sol
        ) / 3;
    end;

    2: // Bruit blanc
    begin
      Randomize;
      for i := 0 to High(FSignal) do
        FSignal[i] := amplitude * (Random - 0.5) * 2;
    end;

    3: // Chirp (sweep)
    begin
      for i := 0 to High(FSignal) do
      begin
        freq := 20 + (20000 - 20) * i / High(FSignal);
        FSignal[i] := amplitude * Sin(2 * Pi * freq * i / FSampleRate);
      end;
    end;
  end;
end;

procedure TFormAnalyseur.AnalyserEtAfficher;
var
  signalFenetre: TSignal;
  analyse: TAnalyseSpectrale;
  SerieSignal, SerieSpectre: TLineSeries;
  i, pas: Integer;
  magnitudeDB: TFloat;
begin
  if Length(FSignal) = 0 then Exit;

  // Copier et appliquer le fenêtrage
  signalFenetre := Copy(FSignal);

  case ComboBoxFenetre.ItemIndex of
    1: FenetreHann(signalFenetre);
    2: FenetreHamming(signalFenetre);
    3: FenetreBlackman(signalFenetre);
  end;

  // Afficher le signal temporel
  ChartSignal.ClearSeries;
  SerieSignal := TLineSeries.Create(ChartSignal);
  SerieSignal.Title := 'Signal temporel';

  pas := Max(1, Length(FSignal) div 2000);
  for i := 0 to High(FSignal) do
    if i mod pas = 0 then
      SerieSignal.AddXY(i / FSampleRate, FSignal[i]);

  ChartSignal.AddSeries(SerieSignal);
  ChartSignal.LeftAxis.Title.Caption := 'Amplitude';
  ChartSignal.BottomAxis.Title.Caption := 'Temps (s)';

  // Analyser et afficher le spectre
  analyse := AnalyserSpectre(signalFenetre, FSampleRate);

  ChartSpectre.ClearSeries;
  SerieSpectre := TLineSeries.Create(ChartSpectre);
  SerieSpectre.Title := 'Spectre de fréquences';

  // Afficher jusqu'à 5 kHz
  for i := 0 to Min(High(analyse.Frequences),
                    Round(5000 * Length(FSignal) / FSampleRate)) do
  begin
    if CheckBoxDB.Checked then
    begin
      if analyse.Magnitudes[i] > 0 then
        magnitudeDB := 20 * Log10(analyse.Magnitudes[i])
      else
        magnitudeDB := -120;
      SerieSpectre.AddXY(analyse.Frequences[i], magnitudeDB);
    end
    else
      SerieSpectre.AddXY(analyse.Frequences[i], analyse.Magnitudes[i]);
  end;

  ChartSpectre.AddSeries(SerieSpectre);

  if CheckBoxDB.Checked then
    ChartSpectre.LeftAxis.Title.Caption := 'Magnitude (dB)'
  else
    ChartSpectre.LeftAxis.Title.Caption := 'Magnitude';

  ChartSpectre.BottomAxis.Title.Caption := 'Fréquence (Hz)';

  // Afficher les informations
  LabelInfo.Caption := Format('Fréquence dominante: %.2f Hz (%s)',
    [analyse.FrequenceDominante,
     FrequenceVersNote(analyse.FrequenceDominante)]);
end;

procedure TFormAnalyseur.ButtonChargerClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Self);
  try
    OpenDialog.Filter := 'Fichiers WAV|*.wav|Tous les fichiers|*.*';
    if OpenDialog.Execute then
    begin
      // Charger le fichier WAV
      FSignal := ChargerFichierWAV(OpenDialog.FileName, FSampleRate);
      AnalyserEtAfficher;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormAnalyseur.ComboBoxFenetreChange(Sender: TObject);
begin
  if Length(FSignal) > 0 then
    AnalyserEtAfficher;
end;

procedure TFormAnalyseur.CheckBoxDBChange(Sender: TObject);
begin
  if Length(FSignal) > 0 then
    AnalyserEtAfficher;
end;

end.
```

## Considérations multi-plateformes

### Optimisations spécifiques

```pascal
{$IFDEF CPUX86_64}
// Utiliser SIMD si disponible
{$DEFINE USE_SIMD}
{$ENDIF}

{$IFDEF USE_SIMD}
procedure FFTOptimiseeSIMD(var data: TSpectre);
begin
  // Implémentation avec instructions SSE/AVX
  // Pour des performances maximales
  FFTIterative(data);  // Fallback pour l'instant
end;
{$ELSE}
procedure FFTOptimiseeSIMD(var data: TSpectre);
begin
  FFTIterative(data);  // Version standard
end;
{$ENDIF}
```

### Utilisation de bibliothèques externes optimisées

#### FFTW (Fastest Fourier Transform in the West)

```pascal
unit UnitFFTW;

{$mode objfpc}{$H+}

interface

uses
  UnitFFT;

const
  {$IFDEF WINDOWS}
  FFTW_DLL = 'libfftw3-3.dll';
  {$ENDIF}
  {$IFDEF UNIX}
  FFTW_DLL = 'libfftw3.so.3';
  {$ENDIF}

const
  FFTW_FORWARD = -1;
  FFTW_BACKWARD = 1;
  FFTW_ESTIMATE = 64;
  FFTW_MEASURE = 0;

type
  fftw_plan = Pointer;
  Pfftw_complex = ^fftw_complex;
  fftw_complex = record
    re: Double;
    im: Double;
  end;

// Déclarations des fonctions FFTW
function fftw_plan_dft_1d(n: Integer; input, output: Pfftw_complex;
         sign: Integer; flags: Cardinal): fftw_plan;
         cdecl; external FFTW_DLL;

function fftw_plan_dft_r2c_1d(n: Integer; input: PDouble;
         output: Pfftw_complex; flags: Cardinal): fftw_plan;
         cdecl; external FFTW_DLL;

function fftw_plan_dft_c2r_1d(n: Integer; input: Pfftw_complex;
         output: PDouble; flags: Cardinal): fftw_plan;
         cdecl; external FFTW_DLL;

procedure fftw_execute(plan: fftw_plan); cdecl; external FFTW_DLL;
procedure fftw_destroy_plan(plan: fftw_plan); cdecl; external FFTW_DLL;
procedure fftw_free(ptr: Pointer); cdecl; external FFTW_DLL;

implementation

end.
```

**Utilisation de FFTW** :

```pascal
uses
  UnitFFTW;

function FFTAvecFFTW(const signal: TSignal): TSpectre;
var
  plan: fftw_plan;
  input: array of Double;
  output: array of fftw_complex;
  i, n: Integer;
begin
  n := Length(signal);
  SetLength(input, n);
  SetLength(output, n);
  SetLength(Result, n);

  // Copier les données
  for i := 0 to n - 1 do
    input[i] := signal[i];

  // Créer le plan FFT
  plan := fftw_plan_dft_r2c_1d(n, @input[0], @output[0], FFTW_ESTIMATE);

  // Exécuter la FFT
  fftw_execute(plan);

  // Copier les résultats
  for i := 0 to n - 1 do
  begin
    if i <= n div 2 then
    begin
      Result[i].Reel := output[i].re;
      Result[i].Imag := output[i].im;
    end
    else
    begin
      // Symétrie hermitienne
      Result[i].Reel := output[n - i].re;
      Result[i].Imag := -output[n - i].im;
    end;
  end;

  // Nettoyer
  fftw_destroy_plan(plan);
end;
```

### Gestion des chemins de bibliothèques

```pascal
function TrouverBibliothequeFFTW: String;
begin
  {$IFDEF WINDOWS}
  Result := ExtractFilePath(ParamStr(0)) + 'libfftw3-3.dll';
  if not FileExists(Result) then
    Result := 'C:\Windows\System32\libfftw3-3.dll';
  {$ENDIF}

  {$IFDEF UNIX}
  Result := '/usr/lib/x86_64-linux-gnu/libfftw3.so.3';
  if not FileExists(Result) then
    Result := '/usr/local/lib/libfftw3.so.3';
  {$ENDIF}

  if not FileExists(Result) then
    raise Exception.Create('Bibliothèque FFTW non trouvée');
end;
```

## Benchmarking et comparaison

### Comparaison des performances

```pascal
uses
  DateUtils;

procedure BenchmarkFFT;
var
  tailles: array[0..5] of Integer = (256, 512, 1024, 2048, 4096, 8192);
  i, j, taille: Integer;
  signal: TSignal;
  spectre: TSpectre;
  debut, fin: TDateTime;
  duree: Int64;
begin
  WriteLn('=== Benchmark FFT ===');
  WriteLn;
  WriteLn('Taille      Temps (ms)');
  WriteLn('------------------------');

  for i := 0 to High(tailles) do
  begin
    taille := tailles[i];

    // Générer un signal de test
    SetLength(signal, taille);
    for j := 0 to taille - 1 do
      signal[j] := Sin(2 * Pi * j / 100);

    // Mesurer le temps
    debut := Now;

    for j := 1 to 100 do  // 100 itérations
    begin
      spectre := SignalVersSpectre(signal);
      FFTIterative(spectre);
    end;

    fin := Now;
    duree := MilliSecondsBetween(fin, debut) div 100;

    WriteLn(Format('%6d      %6d', [taille, duree]));
  end;
end;
```

### Comparaison avec FFTW

```pascal
procedure ComparerFFTImplementations;
var
  signal: TSignal;
  spectre1, spectre2: TSpectre;
  i, n: Integer;
  debut, fin: TDateTime;
  dureeNative, dureeFFTW: Int64;
  erreur: TFloat;
begin
  n := 4096;
  SetLength(signal, n);

  // Générer un signal test
  for i := 0 to n - 1 do
    signal[i] := Sin(2 * Pi * 440 * i / 44100);

  // FFT native
  debut := Now;
  spectre1 := SignalVersSpectre(signal);
  FFTIterative(spectre1);
  fin := Now;
  dureeNative := MilliSecondsBetween(fin, debut);

  // FFT avec FFTW
  debut := Now;
  spectre2 := FFTAvecFFTW(signal);
  fin := Now;
  dureeFFTW := MilliSecondsBetween(fin, debut);

  // Calculer l'erreur
  erreur := 0;
  for i := 0 to n div 2 do
    erreur := erreur + Abs(ModuleComplexe(spectre1[i]) -
                          ModuleComplexe(spectre2[i]));
  erreur := erreur / (n div 2);

  WriteLn('=== Comparaison FFT ===');
  WriteLn(Format('FFT native : %d ms', [dureeNative]));
  WriteLn(Format('FFTW       : %d ms', [dureeFFTW]));
  WriteLn(Format('Speedup    : %.2fx', [dureeNative / dureeFFTW]));
  WriteLn(Format('Erreur moy : %.2e', [erreur]));
end;
```

## Applications avancées

### Compression audio

```pascal
function CompresserAudio(const signal: TSignal;
                        tauxCompression: TFloat): TSignal;
var
  spectre: TSpectre;
  i, n, nbCoeffs: Integer;
  seuil, magnitude: TFloat;
begin
  n := Length(signal);

  // FFT
  spectre := SignalVersSpectre(signal);
  FFTIterative(spectre);

  // Calculer le seuil basé sur le taux de compression
  nbCoeffs := Round(n * (1 - tauxCompression));

  // Trier les magnitudes pour trouver le seuil
  // (simplification - devrait utiliser un tri partiel)
  seuil := 0;  // Approximation

  // Éliminer les coefficients faibles
  for i := 0 to n - 1 do
  begin
    magnitude := ModuleComplexe(spectre[i]);
    if magnitude < seuil then
    begin
      spectre[i].Reel := 0;
      spectre[i].Imag := 0;
    end;
  end;

  // IFFT
  IFFTIterative(spectre);

  // Extraire le résultat
  SetLength(Result, n);
  for i := 0 to n - 1 do
    Result[i] := spectre[i].Reel;
end;
```

### Réduction de bruit spectral

```pascal
function ReduireBruitSpectral(const signal, bruit: TSignal;
                             facteur: TFloat): TSignal;
var
  spectreSignal, spectreBruit: TSpectre;
  i, n: Integer;
  magSignal, magBruit: TFloat;
begin
  n := Length(signal);

  // FFT du signal et du bruit
  spectreSignal := SignalVersSpectre(signal);
  spectreBruit := SignalVersSpectre(bruit);
  FFTIterative(spectreSignal);
  FFTIterative(spectreBruit);

  // Soustraction spectrale
  for i := 0 to n - 1 do
  begin
    magSignal := ModuleComplexe(spectreSignal[i]);
    magBruit := ModuleComplexe(spectreBruit[i]) * facteur;

    // Nouvelle magnitude
    magSignal := Max(0, magSignal - magBruit);

    // Appliquer la nouvelle magnitude en gardant la phase
    if ModuleComplexe(spectreSignal[i]) > 0 then
    begin
      spectreSignal[i].Reel := spectreSignal[i].Reel * magSignal /
                               ModuleComplexe(spectreSignal[i]);
      spectreSignal[i].Imag := spectreSignal[i].Imag * magSignal /
                               ModuleComplexe(spectreSignal[i]);
    end;
  end;

  // IFFT
  IFFTIterative(spectreSignal);

  SetLength(Result, n);
  for i := 0 to n - 1 do
    Result[i] := spectreSignal[i].Reel;
end;
```

### Changement de hauteur (pitch shifting)

```pascal
function ChangerHauteur(const signal: TSignal; facteur: TFloat): TSignal;
var
  spectre: TSpectre;
  nouveauSpectre: TSpectre;
  i, n, indexNouveau: Integer;
begin
  n := Length(signal);

  // FFT
  spectre := SignalVersSpectre(signal);
  FFTIterative(spectre);

  // Créer un nouveau spectre
  SetLength(nouveauSpectre, n);
  for i := 0 to n - 1 do
  begin
    nouveauSpectre[i].Reel := 0;
    nouveauSpectre[i].Imag := 0;
  end;

  // Décaler les fréquences
  for i := 0 to n div 2 do
  begin
    indexNouveau := Round(i * facteur);
    if (indexNouveau >= 0) and (indexNouveau <= n div 2) then
    begin
      nouveauSpectre[indexNouveau] := spectre[i];
      nouveauSpectre[n - indexNouveau] := spectre[n - i];
    end;
  end;

  // IFFT
  IFFTIterative(nouveauSpectre);

  SetLength(Result, n);
  for i := 0 to n - 1 do
    Result[i] := nouveauSpectre[i].Reel;
end;
```

### Vocoder

```pascal
function Vocodage(const porteuse, modulateur: TSignal;
                 nbBandes: Integer): TSignal;
var
  i, j, tailleBande: Integer;
  spectrePorteuse, spectreModulateur: TSpectre;
  enveloppe: TFloat;
begin
  if Length(porteuse) <> Length(modulateur) then
    raise Exception.Create('Les signaux doivent avoir la même taille');

  // FFT des deux signaux
  spectrePorteuse := SignalVersSpectre(porteuse);
  spectreModulateur := SignalVersSpectre(modulateur);
  FFTIterative(spectrePorteuse);
  FFTIterative(spectreModulateur);

  // Diviser le spectre en bandes
  tailleBande := Length(spectrePorteuse) div (2 * nbBandes);

  for i := 0 to nbBandes - 1 do
  begin
    // Calculer l'enveloppe du modulateur dans cette bande
    enveloppe := 0;
    for j := i * tailleBande to (i + 1) * tailleBande - 1 do
      enveloppe := enveloppe + ModuleComplexe(spectreModulateur[j]);
    enveloppe := enveloppe / tailleBande;

    // Appliquer l'enveloppe à la porteuse
    for j := i * tailleBande to (i + 1) * tailleBande - 1 do
    begin
      spectrePorteuse[j].Reel := spectrePorteuse[j].Reel * enveloppe;
      spectrePorteuse[j].Imag := spectrePorteuse[j].Imag * enveloppe;
    end;
  end;

  // IFFT
  IFFTIterative(spectrePorteuse);

  SetLength(Result, Length(porteuse));
  for i := 0 to High(Result) do
    Result[i] := spectrePorteuse[i].Reel;
end;
```

## Erreurs courantes et débogage

### Vérifications de validité

```pascal
function VerifierFFT(const signal: TSignal; const spectre: TSpectre): Boolean;
var
  spectreInverse: TSpectre;
  signalReconstruit: TSignal;
  i: Integer;
  erreur, erreurMax: TFloat;
begin
  Result := False;

  // Vérifier la taille
  if Length(signal) <> Length(spectre) then
  begin
    WriteLn('Erreur : Tailles différentes');
    Exit;
  end;

  // Vérifier que c'est une puissance de 2
  if (Length(signal) and (Length(signal) - 1)) <> 0 then
  begin
    WriteLn('Attention : Taille non optimale (pas une puissance de 2)');
  end;

  // Test de reconstruction (FFT puis IFFT)
  spectreInverse := Copy(spectre);
  IFFTIterative(spectreInverse);

  SetLength(signalReconstruit, Length(signal));
  for i := 0 to High(signal) do
    signalReconstruit[i] := spectreInverse[i].Reel;

  // Calculer l'erreur
  erreurMax := 0;
  for i := 0 to High(signal) do
  begin
    erreur := Abs(signal[i] - signalReconstruit[i]);
    if erreur > erreurMax then
      erreurMax := erreur;
  end;

  WriteLn(Format('Erreur maximale de reconstruction : %.2e', [erreurMax]));

  Result := erreurMax < 1e-10;

  if Result then
    WriteLn('✓ FFT valide')
  else
    WriteLn('✗ FFT invalide');
end;
```

### Détection de problèmes

```pascal
procedure DiagnostiquerProblemes(const signal: TSignal);
var
  i: Integer;
  hasNaN, hasInf, hasZero: Boolean;
  min, max: TFloat;
begin
  WriteLn('=== Diagnostic du signal ===');

  hasNaN := False;
  hasInf := False;
  hasZero := True;
  min := signal[0];
  max := signal[0];

  for i := 0 to High(signal) do
  begin
    if IsNaN(signal[i]) then hasNaN := True;
    if IsInfinite(signal[i]) then hasInf := True;
    if signal[i] <> 0 then hasZero := False;
    if signal[i] < min then min := signal[i];
    if signal[i] > max then max := signal[i];
  end;

  WriteLn(Format('Taille        : %d échantillons', [Length(signal)]));
  WriteLn(Format('Puissance de 2: %s',
    [BoolToStr((Length(signal) and (Length(signal) - 1)) = 0, True)]));
  WriteLn(Format('Valeurs NaN   : %s', [BoolToStr(hasNaN, True)]));
  WriteLn(Format('Valeurs Inf   : %s', [BoolToStr(hasInf, True)]));
  WriteLn(Format('Tout à zéro   : %s', [BoolToStr(hasZero, True)]));
  WriteLn(Format('Min/Max       : [%.2f, %.2f]', [min, max]));
  WriteLn;
end;
```

## Bonnes pratiques

### Checklist pour analyse spectrale

```pascal
procedure ChecklistAnalyseSpectrale;
begin
  WriteLn('=== Checklist Analyse Spectrale ===');
  WriteLn;
  WriteLn('□ Signal de taille puissance de 2');
  WriteLn('□ Fréquence d''échantillonnage connue');
  WriteLn('□ Signal normalisé (amplitude raisonnable)');
  WriteLn('□ Fenêtrage approprié appliqué');
  WriteLn('□ Zero-padding si nécessaire');
  WriteLn('□ Pas de valeurs NaN ou infinies');
  WriteLn('□ Résolution fréquentielle suffisante');
  WriteLn('□ Éviter l''aliasing (respecter Nyquist)');
  WriteLn('□ Interprétation correcte des résultats');
  WriteLn('□ Visualisation appropriée');
end;
```

### Conseils d'utilisation

```pascal
(*
BONNES PRATIQUES FFT :

1. Toujours utiliser des tailles de puissance de 2 (256, 512, 1024, 2048, etc.)
2. Appliquer un fenêtrage (Hann, Hamming) pour réduire les fuites spectrales
3. Normaliser le signal avant la FFT
4. Utiliser seulement la première moitié du spectre (0 à Fs/2)
5. Convertir en échelle logarithmique (dB) pour visualisation
6. Zero-padding pour améliorer la résolution fréquentielle
7. Chevauchement de 50% pour les spectrogrammes
8. Vérifier que Fs > 2 × Fmax (théorème de Nyquist)

PIÈGES À ÉVITER :

1. Ne pas oublier de normaliser après IFFT
2. Attention à la symétrie hermitienne pour signaux réels
3. Ne pas confondre magnitude et puissance
4. Tenir compte du fenêtrage dans les calculs de puissance
5. Ne pas sur-interpréter les artefacts spectraux
*)
```

## Ressources et documentation

### Bibliothèques recommandées

**Pour FreePascal** :
- **FFTW** : La référence pour FFT (via bindings)
- **KissFFT** : Alternative légère et portable
- **NumLib** : Inclut des fonctions FFT basiques
- **Math Unit** : Fonctions mathématiques de base

### Lectures recommandées

**Livres** :
- "Understanding Digital Signal Processing" - Richard G. Lyons
- "The Scientist and Engineer's Guide to Digital Signal Processing"
- "Discrete-Time Signal Processing" - Oppenheim & Schafer

**Cours en ligne** :
- MIT OpenCourseWare : Signals and Systems
- Coursera : Digital Signal Processing
- 3Blue1Brown : But what is the Fourier Transform? (YouTube)

**Documentation** :
- FFTW Documentation : http://www.fftw.org/doc/
- Wiki FreePascal : Section DSP
- Forum Lazarus : Multimedia Programming

### Outils de visualisation

- **Audacity** : Visualisation spectrale de fichiers audio
- **Sonic Visualiser** : Analyse spectrale avancée
- **GNU Octave/MATLAB** : Prototypage et vérification

## Conclusion

La FFT et l'analyse spectrale sont des outils puissants pour :

✓ **Comprendre les signaux** : Identifier les fréquences composantes  
✓ **Filtrer** : Éliminer les fréquences indésirables  
✓ **Analyser** : Détecter des patterns, fréquences dominantes  
✓ **Modifier** : Effets audio, compression, amélioration  
✓ **Compresser** : Réduire la taille des données  
✓ **Détecter** : Reconnaissance de hauteur, identification

### Points clés à retenir

1. **FFT vs DFT** : FFT est O(N log N), beaucoup plus rapide que DFT O(N²)
2. **Taille optimale** : Puissance de 2 pour performance maximale
3. **Fenêtrage** : Essentiel pour réduire les artefacts spectraux
4. **Nyquist** : Fs ≥ 2 × Fmax pour éviter l'aliasing
5. **Symétrie** : Spectre hermitien pour signaux réels
6. **Résolution** : Δf = Fs / N (compromis temps/fréquence)
7. **Applications** : Audio, télécoms, médical, radar, sismologie

### Liens avec d'autres chapitres

- **16.3 Traitement du signal (DSP)** : Applications de la FFT
- **16.2 TAChart** : Visualisation des spectres
- **20. Optimisation** : Accélération des calculs FFT
- **15. Intelligence artificielle** : Extraction de features audio

### Prochaines étapes

Pour approfondir :
1. Étudier les ondelettes (CWT, DWT)
2. Explorer les transformées temps-fréquence (STFT, Wigner-Ville)
3. Apprendre le traitement multi-taux (décimation, interpolation)
4. Découvrir les bancs de filtres
5. Pratiquer avec des signaux réels (audio, biomédicaux, RF)

La FFT est un des algorithmes les plus importants du 20ème siècle. Maîtriser son utilisation avec FreePascal vous ouvre les portes de nombreuses applications passionnantes dans le traitement du signal et l'analyse de données !

**Bon voyage dans le domaine fréquentiel avec FreePascal !** 🎵📊🔊

⏭️ [Optimisation et solveurs](/16-traitement-donnees-calcul-scientifique/07-optimisation-solveurs.md)
