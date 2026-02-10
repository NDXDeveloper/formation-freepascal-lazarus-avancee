🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.3 Traitement du signal (DSP)

## Introduction

Le traitement du signal (DSP - Digital Signal Processing) est l'ensemble des techniques qui permettent d'analyser, de modifier et de synthétiser des signaux numériques. Ces signaux peuvent être audio, vidéo, des mesures de capteurs, des données médicales, ou tout autre type de données variant dans le temps.

## Qu'est-ce que le traitement du signal ?

Le DSP consiste à manipuler mathématiquement des signaux pour :

- **Filtrer** : Éliminer le bruit ou isoler des fréquences
- **Analyser** : Décomposer un signal pour comprendre sa structure
- **Transformer** : Convertir un signal d'un domaine à un autre (temps → fréquence)
- **Compresser** : Réduire la taille des données
- **Générer** : Créer des signaux synthétiques

### Applications concrètes

- **Audio** : Égaliseurs, effets sonores, reconnaissance vocale
- **Médical** : Analyse d'ECG, traitement d'images médicales
- **Télécommunications** : Modems, compression de données
- **Industriel** : Analyse de vibrations, contrôle qualité
- **Scientifique** : Analyse de données de capteurs

## Concepts de base

### Le signal numérique

Un signal numérique est une suite de valeurs mesurées à intervalles réguliers.

```pascal
type
  TSignal = array of Double;  // Un signal = tableau de valeurs

var
  signal: TSignal;
  i: Integer;

begin
  SetLength(signal, 1000);  // Signal de 1000 échantillons

  // Générer un signal sinusoïdal simple
  for i := 0 to 999 do
    signal[i] := Sin(2 * Pi * i / 100);  // Fréquence = 0.01 Hz
end;
```

### Fréquence d'échantillonnage

La fréquence d'échantillonnage (sample rate) indique combien de mesures sont prises par seconde.

```pascal
const
  SAMPLE_RATE = 44100;  // 44.1 kHz (qualité CD audio)

// Pour un signal de 1 seconde :
var
  signal: TSignal;
begin
  SetLength(signal, SAMPLE_RATE);  // 44100 échantillons = 1 seconde
end;
```

**Règle importante** : Pour capturer une fréquence, il faut échantillonner au moins 2 fois plus vite (théorème de Nyquist).

```pascal
// Pour capturer des sons jusqu'à 20 kHz (limite audition humaine)
const
  FREQ_MAX = 20000;
  SAMPLE_RATE = FREQ_MAX * 2;  // Minimum 40 kHz
```

### Amplitude et normalisation

L'amplitude représente l'intensité du signal.

```pascal
procedure NormaliserSignal(var signal: TSignal);
var
  i: Integer;
  maxVal: Double;
begin
  // Trouver la valeur maximale
  maxVal := 0;
  for i := 0 to High(signal) do
    if Abs(signal[i]) > maxVal then
      maxVal := Abs(signal[i]);

  // Normaliser entre -1 et 1
  if maxVal > 0 then
    for i := 0 to High(signal) do
      signal[i] := signal[i] / maxVal;
end;
```

## Génération de signaux de base

### Signal sinusoïdal

```pascal
function GenererSinus(frequence, amplitude: Double;
                     duree: Double; sampleRate: Integer): TSignal;
var
  i, nbEchantillons: Integer;
begin
  nbEchantillons := Round(duree * sampleRate);
  SetLength(Result, nbEchantillons);

  for i := 0 to nbEchantillons - 1 do
    Result[i] := amplitude * Sin(2 * Pi * frequence * i / sampleRate);
end;

// Utilisation :
var
  signal: TSignal;
begin
  // Générer un La (440 Hz) pendant 1 seconde
  signal := GenererSinus(440, 1.0, 1.0, 44100);
end;
```

### Signal carré

```pascal
function GenererCarre(frequence, amplitude: Double;
                     duree: Double; sampleRate: Integer): TSignal;
var
  i, nbEchantillons: Integer;
  phase: Double;
begin
  nbEchantillons := Round(duree * sampleRate);
  SetLength(Result, nbEchantillons);

  for i := 0 to nbEchantillons - 1 do
  begin
    phase := Frac(frequence * i / sampleRate);
    if phase < 0.5 then
      Result[i] := amplitude
    else
      Result[i] := -amplitude;
  end;
end;
```

### Signal en dents de scie

```pascal
function GenererDentsDeScie(frequence, amplitude: Double;
                           duree: Double; sampleRate: Integer): TSignal;
var
  i, nbEchantillons: Integer;
  phase: Double;
begin
  nbEchantillons := Round(duree * sampleRate);
  SetLength(Result, nbEchantillons);

  for i := 0 to nbEchantillons - 1 do
  begin
    phase := Frac(frequence * i / sampleRate);
    Result[i] := amplitude * (2 * phase - 1);
  end;
end;
```

### Bruit blanc

```pascal
function GenererBruitBlanc(amplitude: Double;
                          duree: Double; sampleRate: Integer): TSignal;
var
  i, nbEchantillons: Integer;
begin
  nbEchantillons := Round(duree * sampleRate);
  SetLength(Result, nbEchantillons);

  Randomize;
  for i := 0 to nbEchantillons - 1 do
    Result[i] := amplitude * (Random - 0.5) * 2;  // Entre -amplitude et +amplitude
end;
```

## Opérations de base sur les signaux

### Addition de signaux

```pascal
function AdditionnerSignaux(const s1, s2: TSignal): TSignal;
var
  i, longueur: Integer;
begin
  longueur := Min(Length(s1), Length(s2));
  SetLength(Result, longueur);

  for i := 0 to longueur - 1 do
    Result[i] := s1[i] + s2[i];
end;

// Exemple : Mélanger une note avec du bruit
var
  note, bruit, melange: TSignal;
begin
  note := GenererSinus(440, 1.0, 1.0, 44100);
  bruit := GenererBruitBlanc(0.1, 1.0, 44100);
  melange := AdditionnerSignaux(note, bruit);
end;
```

### Multiplication (modulation)

```pascal
function MultiplierSignaux(const s1, s2: TSignal): TSignal;
var
  i, longueur: Integer;
begin
  longueur := Min(Length(s1), Length(s2));
  SetLength(Result, longueur);

  for i := 0 to longueur - 1 do
    Result[i] := s1[i] * s2[i];
end;
```

### Amplification

```pascal
procedure AmplifierSignal(var signal: TSignal; gain: Double);
var
  i: Integer;
begin
  for i := 0 to High(signal) do
    signal[i] := signal[i] * gain;
end;
```

### Inversion de phase

```pascal
procedure InverserPhase(var signal: TSignal);
var
  i: Integer;
begin
  for i := 0 to High(signal) do
    signal[i] := -signal[i];
end;
```

## Filtres numériques

Les filtres permettent de modifier le contenu fréquentiel d'un signal.

### Filtre passe-bas simple (moyenne mobile)

Ce filtre atténue les hautes fréquences (bruit rapide) et conserve les basses fréquences.

```pascal
function FiltrePasseBas(const signal: TSignal; tailleFenetre: Integer): TSignal;
var
  i, j: Integer;
  somme: Double;
begin
  SetLength(Result, Length(signal));

  for i := 0 to High(signal) do
  begin
    somme := 0;
    for j := Max(0, i - tailleFenetre div 2) to
             Min(High(signal), i + tailleFenetre div 2) do
      somme := somme + signal[j];

    Result[i] := somme / tailleFenetre;
  end;
end;

// Utilisation : lisser un signal bruité
var
  signalBruite, signalFiltre: TSignal;
begin
  signalBruite := GenererBruitBlanc(1.0, 1.0, 44100);
  signalFiltre := FiltrePasseBas(signalBruite, 50);  // Fenêtre de 50 échantillons
end;
```

### Filtre passe-haut simple

Ce filtre atténue les basses fréquences et conserve les hautes fréquences.

```pascal
function FiltrePasseHaut(const signal: TSignal; tailleFenetre: Integer): TSignal;
var
  signalFiltre: TSignal;
  i: Integer;
begin
  // Passe-haut = Signal original - Passe-bas
  signalFiltre := FiltrePasseBas(signal, tailleFenetre);
  SetLength(Result, Length(signal));

  for i := 0 to High(signal) do
    Result[i] := signal[i] - signalFiltre[i];
end;
```

### Filtre médian (élimination des pics)

Très efficace pour éliminer le bruit impulsionnel.

```pascal
uses
  Math;

function FiltreMedian(const signal: TSignal; tailleFenetre: Integer): TSignal;
var
  i, j, debut, fin: Integer;
  fenetre: array of Double;
begin
  SetLength(Result, Length(signal));
  SetLength(fenetre, tailleFenetre);

  for i := 0 to High(signal) do
  begin
    debut := Max(0, i - tailleFenetre div 2);
    fin := Min(High(signal), i + tailleFenetre div 2);

    // Copier la fenêtre
    for j := debut to fin do
      fenetre[j - debut] := signal[j];

    // Calculer la médiane
    Result[i] := MedianValue(fenetre);
  end;
end;
```

### Filtre IIR simple (Infinite Impulse Response)

Filtre récursif très efficace.

```pascal
function FiltreIIR(const signal: TSignal; alpha: Double): TSignal;
var
  i: Integer;
begin
  SetLength(Result, Length(signal));
  Result[0] := signal[0];

  // Formule : y[n] = alpha * x[n] + (1 - alpha) * y[n-1]
  for i := 1 to High(signal) do
    Result[i] := alpha * signal[i] + (1 - alpha) * Result[i - 1];
end;

// alpha proche de 1 : réponse rapide (moins de filtrage)
// alpha proche de 0 : réponse lente (plus de filtrage)
```

## Transformée de Fourier (FFT)

La transformée de Fourier convertit un signal du domaine temporel au domaine fréquentiel.

### Comprendre la FFT

Imaginez que vous écoutez de la musique :
- **Domaine temporel** : Ce que vous entendez instant par instant
- **Domaine fréquentiel** : Les notes jouées (graves, médiums, aigus)

La FFT décompose un signal en ses fréquences constitutives.

### Implémentation basique

```pascal
type
  TComplex = record
    Real: Double;
    Imag: Double;
  end;

  TComplexArray = array of TComplex;

function FFT(const signal: TSignal): TComplexArray;
var
  n, i: Integer;
begin
  n := Length(signal);
  SetLength(Result, n);

  // Conversion en complexes
  for i := 0 to n - 1 do
  begin
    Result[i].Real := signal[i];
    Result[i].Imag := 0;
  end;

  // Appel de l'algorithme FFT (voir implémentation complète ci-dessous)
  FFTRecursive(Result, n);
end;
```

### Utilisation d'une bibliothèque FFT

Il est recommandé d'utiliser une bibliothèque optimisée comme **FFTW** ou **KissFFT**.

```pascal
// Exemple avec une bibliothèque externe
uses
  FFTW; // Bibliothèque à installer séparément

procedure AnalyserSpectre(const signal: TSignal);
var
  spectre: TComplexArray;
  i: Integer;
  magnitude: Double;
begin
  // Calculer la FFT
  spectre := FFT(signal);

  // Calculer les magnitudes (amplitudes des fréquences)
  for i := 0 to Length(spectre) div 2 do  // Seulement la moitié (symétrie)
  begin
    magnitude := Sqrt(Sqr(spectre[i].Real) + Sqr(spectre[i].Imag));
    WriteLn(Format('Fréquence %d Hz : %.2f',
                   [i * SAMPLE_RATE div Length(signal), magnitude]));
  end;
end;
```

### Implémentation complète de la FFT (Cooley-Tukey)

```pascal
procedure FFTRecursive(var data: TComplexArray; n: Integer);
var
  i, j, k, m: Integer;
  temp: TComplex;
  w, wm, t, u: TComplex;
  angle: Double;
begin
  // Réorganisation bit-reversed
  j := 0;
  for i := 0 to n - 2 do
  begin
    if i < j then
    begin
      temp := data[i];
      data[i] := data[j];
      data[j] := temp;
    end;

    k := n div 2;
    while k <= j do
    begin
      j := j - k;
      k := k div 2;
    end;
    j := j + k;
  end;

  // Calcul FFT
  m := 2;
  while m <= n do
  begin
    angle := -2 * Pi / m;
    wm.Real := Cos(angle);
    wm.Imag := Sin(angle);

    for k := 0 to n - 1 do
    begin
      if k mod m = 0 then
      begin
        w.Real := 1;
        w.Imag := 0;
      end;

      for j := 0 to m div 2 - 1 do
      begin
        // Calcul papillon
        t.Real := w.Real * data[k + j + m div 2].Real -
                  w.Imag * data[k + j + m div 2].Imag;
        t.Imag := w.Real * data[k + j + m div 2].Imag +
                  w.Imag * data[k + j + m div 2].Real;

        u := data[k + j];

        data[k + j].Real := u.Real + t.Real;
        data[k + j].Imag := u.Imag + t.Imag;

        data[k + j + m div 2].Real := u.Real - t.Real;
        data[k + j + m div 2].Imag := u.Imag - t.Imag;

        // Mise à jour de w
        temp.Real := w.Real * wm.Real - w.Imag * wm.Imag;
        temp.Imag := w.Real * wm.Imag + w.Imag * wm.Real;
        w := temp;
      end;
    end;

    m := m * 2;
  end;
end;
```

## Analyse spectrale

### Spectrogramme

Analyse de l'évolution du spectre dans le temps.

```pascal
type
  TSpectrogramme = array of array of Double;

function CalculerSpectrogramme(const signal: TSignal;
                              tailleFenetre, chevauchement: Integer): TSpectrogramme;
var
  nbFenetres, i, j, debut: Integer;
  fenetre: TSignal;
  spectre: TComplexArray;
begin
  nbFenetres := (Length(signal) - tailleFenetre) div
                (tailleFenetre - chevauchement) + 1;
  SetLength(Result, nbFenetres, tailleFenetre div 2);

  for i := 0 to nbFenetres - 1 do
  begin
    debut := i * (tailleFenetre - chevauchement);

    // Extraire la fenêtre
    SetLength(fenetre, tailleFenetre);
    for j := 0 to tailleFenetre - 1 do
      fenetre[j] := signal[debut + j];

    // Appliquer une fenêtre de Hamming
    AppliquerFenetreHamming(fenetre);

    // Calculer la FFT
    spectre := FFT(fenetre);

    // Stocker les magnitudes
    for j := 0 to tailleFenetre div 2 - 1 do
      Result[i, j] := Sqrt(Sqr(spectre[j].Real) + Sqr(spectre[j].Imag));
  end;
end;

procedure AppliquerFenetreHamming(var signal: TSignal);
var
  i, n: Integer;
begin
  n := Length(signal);
  for i := 0 to n - 1 do
    signal[i] := signal[i] * (0.54 - 0.46 * Cos(2 * Pi * i / (n - 1)));
end;
```

### Détection de fréquence dominante

```pascal
function DetecterFrequenceDominante(const signal: TSignal;
                                   sampleRate: Integer): Double;
var
  spectre: TComplexArray;
  magnitudes: array of Double;
  i, indexMax: Integer;
  maxMag: Double;
begin
  // Calculer la FFT
  spectre := FFT(signal);

  // Calculer les magnitudes
  SetLength(magnitudes, Length(spectre) div 2);
  for i := 0 to High(magnitudes) do
    magnitudes[i] := Sqrt(Sqr(spectre[i].Real) + Sqr(spectre[i].Imag));

  // Trouver le maximum
  maxMag := 0;
  indexMax := 0;
  for i := 1 to High(magnitudes) do  // Ignorer DC (i=0)
  begin
    if magnitudes[i] > maxMag then
    begin
      maxMag := magnitudes[i];
      indexMax := i;
    end;
  end;

  // Convertir l'index en fréquence
  Result := indexMax * sampleRate / Length(signal);
end;
```

## Convolution

La convolution est une opération fondamentale en DSP, utilisée pour appliquer des filtres.

```pascal
function Convoluer(const signal, noyau: TSignal): TSignal;
var
  i, j: Integer;
  somme: Double;
  lenSignal, lenNoyau: Integer;
begin
  lenSignal := Length(signal);
  lenNoyau := Length(noyau);
  SetLength(Result, lenSignal + lenNoyau - 1);

  for i := 0 to High(Result) do
  begin
    somme := 0;
    for j := 0 to lenNoyau - 1 do
    begin
      if (i - j >= 0) and (i - j < lenSignal) then
        somme := somme + signal[i - j] * noyau[j];
    end;
    Result[i] := somme;
  end;
end;

// Exemple : Appliquer un filtre passe-bas par convolution
var
  signal, noyau, resultat: TSignal;
  i: Integer;
begin
  // Créer un noyau de filtre passe-bas simple
  SetLength(noyau, 5);
  for i := 0 to 4 do
    noyau[i] := 1 / 5;  // Moyenne de 5 points

  resultat := Convoluer(signal, noyau);
end;
```

## Corrélation

La corrélation mesure la similitude entre deux signaux.

```pascal
function Correlat ion(const signal1, signal2: TSignal): TSignal;
var
  i, j: Integer;
  somme: Double;
  len1, len2: Integer;
begin
  len1 := Length(signal1);
  len2 := Length(signal2);
  SetLength(Result, len1 + len2 - 1);

  for i := 0 to High(Result) do
  begin
    somme := 0;
    for j := 0 to len2 - 1 do
    begin
      if (i - j >= 0) and (i - j < len1) then
        somme := somme + signal1[i - j] * signal2[j];
    end;
    Result[i] := somme;
  end;
end;

// Auto-corrélation (corrélation d'un signal avec lui-même)
function AutoCorrelation(const signal: TSignal): TSignal;
begin
  Result := Correlation(signal, signal);
end;
```

## Fenêtrage

Le fenêtrage réduit les artefacts lors de l'analyse spectrale.

### Fenêtre de Hanning

```pascal
procedure AppliquerFenetreHanning(var signal: TSignal);
var
  i, n: Integer;
begin
  n := Length(signal);
  for i := 0 to n - 1 do
    signal[i] := signal[i] * (0.5 - 0.5 * Cos(2 * Pi * i / (n - 1)));
end;
```

### Fenêtre de Blackman

```pascal
procedure AppliquerFenetreBlackman(var signal: TSignal);
var
  i, n: Integer;
  a0, a1, a2: Double;
begin
  n := Length(signal);
  a0 := 0.42;
  a1 := 0.5;
  a2 := 0.08;

  for i := 0 to n - 1 do
    signal[i] := signal[i] * (a0 - a1 * Cos(2 * Pi * i / (n - 1)) +
                              a2 * Cos(4 * Pi * i / (n - 1)));
end;
```

### Fenêtre de Kaiser

```pascal
uses
  Math;

function BesselI0(x: Double): Double;
var
  somme, terme: Double;
  k: Integer;
begin
  somme := 1;
  terme := 1;
  for k := 1 to 50 do
  begin
    terme := terme * Sqr(x / (2 * k));
    somme := somme + terme;
  end;
  Result := somme;
end;

procedure AppliquerFenetreKaiser(var signal: TSignal; beta: Double);
var
  i, n: Integer;
  w: Double;
begin
  n := Length(signal);
  for i := 0 to n - 1 do
  begin
    w := BesselI0(beta * Sqrt(1 - Sqr(2 * i / (n - 1) - 1))) / BesselI0(beta);
    signal[i] := signal[i] * w;
  end;
end;
```

## Application pratique : Analyseur audio

Voici un exemple complet d'analyseur audio avec interface graphique.

```pascal
unit UnitAnalyseurAudio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  TAGraph, TASeries, ExtCtrls;

type
  TFormAnalyseur = class(TForm)
    ChartSignal: TChart;
    ChartSpectre: TChart;
    SerieSignal: TLineSeries;
    SerieSpectre: TBarSeries;
    ButtonCharger: TButton;
    ButtonAnalyser: TButton;
    Timer1: TTimer;
    LabelFrequence: TLabel;
    procedure ButtonChargerClick(Sender: TObject);
    procedure ButtonAnalyserClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FSignal: TSignal;
    FSampleRate: Integer;
    procedure AfficherSignal;
    procedure AfficherSpectre;
    function DetecterNote: String;
  public

  end;

var
  FormAnalyseur: TFormAnalyseur;

implementation

{$R *.lfm}

const
  SAMPLE_RATE = 44100;

procedure TFormAnalyseur.ButtonChargerClick(Sender: TObject);
var
  i: Integer;
begin
  // Générer un signal test (La 440 Hz + bruit)
  FSampleRate := SAMPLE_RATE;
  SetLength(FSignal, FSampleRate);  // 1 seconde

  Randomize;
  for i := 0 to High(FSignal) do
  begin
    FSignal[i] := Sin(2 * Pi * 440 * i / FSampleRate) +  // Note La
                  0.1 * (Random - 0.5) * 2;              // Bruit
  end;

  AfficherSignal;
  ShowMessage('Signal chargé : 1 seconde à 44.1 kHz');
end;

procedure TFormAnalyseur.ButtonAnalyserClick(Sender: TObject);
var
  freqDom: Double;
  note: String;
begin
  if Length(FSignal) = 0 then
  begin
    ShowMessage('Chargez d''abord un signal');
    Exit;
  end;

  // Détecter la fréquence dominante
  freqDom := DetecterFrequenceDominante(FSignal, FSampleRate);
  note := DetecterNote;

  LabelFrequence.Caption := Format('Fréquence: %.2f Hz (%s)', [freqDom, note]);

  // Afficher le spectre
  AfficherSpectre;
end;

procedure TFormAnalyseur.AfficherSignal;
var
  i, pas: Integer;
begin
  SerieSignal.Clear;

  // Afficher seulement 1000 points pour la performance
  pas := Max(1, Length(FSignal) div 1000);

  for i := 0 to Length(FSignal) - 1 do
  begin
    if i mod pas = 0 then
      SerieSignal.AddXY(i / FSampleRate, FSignal[i]);
  end;

  ChartSignal.LeftAxis.Title.Caption := 'Amplitude';
  ChartSignal.BottomAxis.Title.Caption := 'Temps (s)';
end;

procedure TFormAnalyseur.AfficherSpectre;
var
  spectre: TComplexArray;
  i, nbFreq: Integer;
  magnitude: Double;
begin
  SerieSpectre.Clear;

  // Calculer la FFT
  spectre := FFT(FSignal);

  // Afficher seulement les fréquences jusqu'à 2000 Hz
  nbFreq := Min(2000 * Length(FSignal) div FSampleRate, Length(spectre) div 2);

  for i := 0 to nbFreq do
  begin
    magnitude := Sqrt(Sqr(spectre[i].Real) + Sqr(spectre[i].Imag));
    SerieSpectre.AddXY(i * FSampleRate / Length(FSignal), magnitude);
  end;

  ChartSpectre.LeftAxis.Title.Caption := 'Magnitude';
  ChartSpectre.BottomAxis.Title.Caption := 'Fréquence (Hz)';
end;

function TFormAnalyseur.DetecterNote: String;
const
  NOTES: array[0..11] of String =
    ('Do', 'Do#', 'Ré', 'Ré#', 'Mi', 'Fa', 'Fa#', 'Sol', 'Sol#', 'La', 'La#', 'Si');
var
  freqDom, freqLa, semiTons: Double;
  noteIndex: Integer;
begin
  freqDom := DetecterFrequenceDominante(FSignal, FSampleRate);
  freqLa := 440.0;  // La de référence

  // Calculer le nombre de demi-tons par rapport au La
  semiTons := 12 * Log2(freqDom / freqLa);
  noteIndex := Round(semiTons) mod 12;
  if noteIndex < 0 then noteIndex := noteIndex + 12;

  Result := NOTES[noteIndex];
end;

procedure TFormAnalyseur.Timer1Timer(Sender: TObject);
begin
  // Mise à jour en temps réel (optionnel)
  if Length(FSignal) > 0 then
    AfficherSpectre;
end;

end.
```

## Bibliothèques DSP pour FreePascal

### 1. pasdsp

Bibliothèque complète de traitement du signal pour FreePascal.

**Installation** :
```bash
# Ubuntu
sudo apt-get install fp-units-math

# Ou télécharger depuis GitHub
git clone https://github.com/dort/pasdsp.git
cd pasdsp
fpc -B pasdsp.pas
```

**Windows** :
- Télécharger depuis GitHub
- Compiler avec : `fpc -B pasdsp.pas`
- Ajouter le chemin dans les options du projet

**Utilisation** :
```pascal
uses
  pasdsp;

var
  signal: TFloatArray;
  filtered: TFloatArray;
begin
  // Créer un filtre passe-bas
  filtered := LowPassFilter(signal, 1000, 44100);  // Cutoff 1kHz
end;
```

### 2. Math Unit (incluse avec FPC)

La bibliothèque mathématique standard de FreePascal contient des fonctions DSP de base.

```pascal
uses
  Math;

var
  moyenne, ecartType: Double;
  donnees: array of Double;
begin
  // Statistiques de base
  moyenne := Mean(donnees);
  ecartType := StdDev(donnees);

  // Fonctions trigonométriques
  // Interpolation
  // etc.
end;
```

### 3. FFTW (Fastest Fourier Transform in the West)

La référence pour les transformées de Fourier rapides.

**Installation Ubuntu** :
```bash
sudo apt-get install libfftw3-dev
```

**Installation Windows** :
- Télécharger les DLL depuis http://www.fftw.org
- Placer `libfftw3-3.dll` dans le dossier du projet

**Bindings FreePascal** :
```pascal
unit FFTW3;

interface

const
  {$IFDEF WINDOWS}
  FFTW_LIB = 'libfftw3-3.dll';
  {$ENDIF}
  {$IFDEF UNIX}
  FFTW_LIB = 'libfftw3.so.3';
  {$ENDIF}

type
  Pfftw_complex = ^fftw_complex;
  fftw_complex = record
    re: Double;
    im: Double;
  end;

  fftw_plan = Pointer;

// Déclarations des fonctions
function fftw_plan_dft_r2c_1d(n: Integer; input: PDouble;
         output: Pfftw_complex; flags: Cardinal): fftw_plan;
         cdecl; external FFTW_LIB;

procedure fftw_execute(plan: fftw_plan);
          cdecl; external FFTW_LIB;

procedure fftw_destroy_plan(plan: fftw_plan);
          cdecl; external FFTW_LIB;

implementation

end.
```

**Exemple d'utilisation** :
```pascal
uses
  FFTW3;

const
  FFTW_ESTIMATE = 64;

procedure CalculerFFTAvecFFTW(const signal: array of Double);
var
  plan: fftw_plan;
  output: array of fftw_complex;
  i: Integer;
begin
  SetLength(output, Length(signal) div 2 + 1);

  // Créer le plan FFT
  plan := fftw_plan_dft_r2c_1d(Length(signal), @signal[0],
                                @output[0], FFTW_ESTIMATE);

  // Exécuter la FFT
  fftw_execute(plan);

  // Utiliser les résultats
  for i := 0 to High(output) do
    WriteLn(Format('Freq %d: %.2f + %.2fi',
            [i, output[i].re, output[i].im]));

  // Nettoyer
  fftw_destroy_plan(plan);
end;
```

### 4. KissFFT

Alternative plus simple et portable que FFTW.

```pascal
// kissfft_wrapper.pas
unit KissFFT;

interface

type
  kiss_fft_cfg = Pointer;
  kiss_fft_cpx = record
    r: Single;
    i: Single;
  end;

const
  {$IFDEF WINDOWS}
  KISSFFT_LIB = 'kissfft.dll';
  {$ENDIF}
  {$IFDEF UNIX}
  KISSFFT_LIB = 'libkissfft.so';
  {$ENDIF}

function kiss_fft_alloc(nfft: Integer; inverse_fft: Integer;
         mem: Pointer; lenmem: PInteger): kiss_fft_cfg;
         cdecl; external KISSFFT_LIB;

procedure kiss_fft(cfg: kiss_fft_cfg; fin: Pointer; fout: Pointer);
          cdecl; external KISSFFT_LIB;

implementation

end.
```

## Traitement audio en temps réel

### Lecture et enregistrement audio

```pascal
uses
  {$IFDEF WINDOWS}
  MMSystem,  // Pour Windows
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix, Unix,  // Pour Linux/ALSA
  {$ENDIF}
  Classes, SysUtils;

type
  TAudioBuffer = array of SmallInt;  // 16-bit audio

const
  SAMPLE_RATE = 44100;
  BUFFER_SIZE = 4096;

{$IFDEF WINDOWS}
procedure EnregistrerAudioWindows(duree: Integer);
var
  hWaveIn: HWAVEIN;
  waveFormat: TWAVEFORMATEX;
  waveHeader: TWAVEHDR;
  buffer: TAudioBuffer;
  fichier: TFileStream;
begin
  // Configuration du format audio
  with waveFormat do
  begin
    wFormatTag := WAVE_FORMAT_PCM;
    nChannels := 1;  // Mono
    nSamplesPerSec := SAMPLE_RATE;
    wBitsPerSample := 16;
    nBlockAlign := nChannels * wBitsPerSample div 8;
    nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
    cbSize := 0;
  end;

  // Ouvrir le périphérique d'entrée
  if waveInOpen(@hWaveIn, WAVE_MAPPER, @waveFormat, 0, 0,
                 CALLBACK_NULL) <> MMSYSERR_NOERROR then
  begin
    WriteLn('Erreur d''ouverture du périphérique audio');
    Exit;
  end;

  SetLength(buffer, BUFFER_SIZE);

  // Préparer le buffer
  with waveHeader do
  begin
    lpData := @buffer[0];
    dwBufferLength := BUFFER_SIZE * SizeOf(SmallInt);
    dwFlags := 0;
  end;

  waveInPrepareHeader(hWaveIn, @waveHeader, SizeOf(TWAVEHDR));
  waveInAddBuffer(hWaveIn, @waveHeader, SizeOf(TWAVEHDR));

  // Démarrer l'enregistrement
  waveInStart(hWaveIn);

  fichier := TFileStream.Create('enregistrement.raw', fmCreate);
  try
    // Enregistrer pendant la durée spécifiée
    Sleep(duree * 1000);

    // Sauvegarder les données
    fichier.WriteBuffer(buffer[0], Length(buffer) * SizeOf(SmallInt));
  finally
    fichier.Free;
  end;

  // Arrêter et nettoyer
  waveInStop(hWaveIn);
  waveInUnprepareHeader(hWaveIn, @waveHeader, SizeOf(TWAVEHDR));
  waveInClose(hWaveIn);
end;
{$ENDIF}

{$IFDEF UNIX}
procedure EnregistrerAudioLinux(duree: Integer);
var
  buffer: TAudioBuffer;
  fichier: TFileStream;
  bytesLus: Integer;
  device: TextFile;
begin
  // Utilisation d'ALSA via arecord (simplifié)
  ExecuteProcess('/usr/bin/arecord',
    ['-f', 'S16_LE', '-r', '44100', '-c', '1',
     '-d', IntToStr(duree), 'enregistrement.raw']);
end;
{$ENDIF}
```

### Lecture audio avec callback

```pascal
type
  TAudioCallback = procedure(buffer: TAudioBuffer; size: Integer) of object;

  TAudioPlayer = class
  private
    FCallback: TAudioCallback;
    FBuffer: TAudioBuffer;
    FSampleRate: Integer;
    procedure ProcessAudio;
  public
    constructor Create(sampleRate: Integer);
    procedure Start;
    procedure Stop;
    property OnAudioData: TAudioCallback read FCallback write FCallback;
  end;

constructor TAudioPlayer.Create(sampleRate: Integer);
begin
  FSampleRate := sampleRate;
  SetLength(FBuffer, BUFFER_SIZE);
end;

procedure TAudioPlayer.ProcessAudio;
begin
  if Assigned(FCallback) then
    FCallback(FBuffer, Length(FBuffer));
end;
```

## Effets audio courants

### Effet d'écho (delay)

```pascal
function AjouterEcho(const signal: TSignal; delai: Double;
                    attenuation: Double; sampleRate: Integer): TSignal;
var
  i, offsetDelay: Integer;
begin
  offsetDelay := Round(delai * sampleRate);
  SetLength(Result, Length(signal) + offsetDelay);

  // Copier le signal original
  for i := 0 to High(signal) do
    Result[i] := signal[i];

  // Ajouter l'écho
  for i := 0 to High(signal) do
  begin
    if i + offsetDelay < Length(Result) then
      Result[i + offsetDelay] := Result[i + offsetDelay] +
                                 signal[i] * attenuation;
  end;
end;

// Utilisation : écho à 0.3 secondes, atténué de 50%
var
  signalAvecEcho: TSignal;
begin
  signalAvecEcho := AjouterEcho(monSignal, 0.3, 0.5, 44100);
end;
```

### Effet de réverbération

```pascal
function AjouterReverb(const signal: TSignal; sampleRate: Integer): TSignal;
const
  DELAIS: array[0..5] of Double = (0.029, 0.037, 0.041, 0.043, 0.047, 0.051);
  GAINS: array[0..5] of Double = (0.7, 0.6, 0.5, 0.4, 0.3, 0.2);
var
  i, j, offset: Integer;
  maxOffset: Integer;
begin
  // Trouver le délai maximum
  maxOffset := Round(DELAIS[High(DELAIS)] * sampleRate);
  SetLength(Result, Length(signal) + maxOffset);

  // Copier le signal original
  for i := 0 to High(signal) do
    Result[i] := signal[i];

  // Ajouter les réflexions
  for j := 0 to High(DELAIS) do
  begin
    offset := Round(DELAIS[j] * sampleRate);
    for i := 0 to High(signal) do
    begin
      if i + offset < Length(Result) then
        Result[i + offset] := Result[i + offset] + signal[i] * GAINS[j];
    end;
  end;
end;
```

### Effet de distorsion

```pascal
function AppliquerDistorsion(const signal: TSignal; gain: Double): TSignal;
var
  i: Integer;
  valeur: Double;
begin
  SetLength(Result, Length(signal));

  for i := 0 to High(signal) do
  begin
    valeur := signal[i] * gain;

    // Écrêtage doux (soft clipping)
    if valeur > 1.0 then
      Result[i] := 1.0
    else if valeur < -1.0 then
      Result[i] := -1.0
    else
      Result[i] := valeur;
  end;
end;

// Distorsion avec fonction tangente hyperbolique (plus musical)
function DistorsionTanh(const signal: TSignal; drive: Double): TSignal;
var
  i: Integer;
begin
  SetLength(Result, Length(signal));

  for i := 0 to High(signal) do
    Result[i] := Tanh(signal[i] * drive);
end;
```

### Effet de chorus

```pascal
function AppliquerChorus(const signal: TSignal; sampleRate: Integer): TSignal;
var
  i: Integer;
  lfo: Double;
  delai, delaiVariable: Double;
  index: Integer;
const
  FREQ_LFO = 1.0;  // Hz
  PROFONDEUR = 0.002;  // secondes
  DELAY_BASE = 0.020;  // 20 ms
begin
  SetLength(Result, Length(signal));

  for i := 0 to High(signal) do
  begin
    // LFO (Low Frequency Oscillator) pour moduler le délai
    lfo := Sin(2 * Pi * FREQ_LFO * i / sampleRate);

    // Calculer le délai variable
    delaiVariable := DELAY_BASE + lfo * PROFONDEUR;
    index := i - Round(delaiVariable * sampleRate);

    if (index >= 0) and (index < Length(signal)) then
      Result[i] := (signal[i] + signal[index]) / 2
    else
      Result[i] := signal[i];
  end;
end;
```

### Égaliseur paramétrique

```pascal
type
  TBandEQ = record
    Frequence: Double;
    Gain: Double;      // En dB
    Q: Double;         // Facteur de qualité
  end;

function AppliquerEQ(const signal: TSignal; const bande: TBandEQ;
                    sampleRate: Integer): TSignal;
var
  i: Integer;
  A, omega, sinOmega, alpha: Double;
  b0, b1, b2, a0, a1, a2: Double;
  x1, x2, y1, y2: Double;
begin
  // Calcul des coefficients du filtre biquad
  A := Power(10, bande.Gain / 40);
  omega := 2 * Pi * bande.Frequence / sampleRate;
  sinOmega := Sin(omega);
  alpha := sinOmega / (2 * bande.Q);

  // Coefficients pour un filtre peak
  b0 := 1 + alpha * A;
  b1 := -2 * Cos(omega);
  b2 := 1 - alpha * A;
  a0 := 1 + alpha / A;
  a1 := -2 * Cos(omega);
  a2 := 1 - alpha / A;

  // Normaliser
  b0 := b0 / a0;
  b1 := b1 / a0;
  b2 := b2 / a0;
  a1 := a1 / a0;
  a2 := a2 / a0;

  SetLength(Result, Length(signal));
  x1 := 0; x2 := 0;
  y1 := 0; y2 := 0;

  // Appliquer le filtre
  for i := 0 to High(signal) do
  begin
    Result[i] := b0 * signal[i] + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2;

    x2 := x1;
    x1 := signal[i];
    y2 := y1;
    y1 := Result[i];
  end;
end;

// Exemple : égaliseur 3 bandes
procedure EgaliseurTroisBandes(var signal: TSignal;
                               gainBas, gainMedium, gainHaut: Double);
var
  bandeBas, bandeMedium, bandeHaut: TBandEQ;
begin
  // Basse : 100 Hz
  bandeBas.Frequence := 100;
  bandeBas.Gain := gainBas;
  bandeBas.Q := 0.7;
  signal := AppliquerEQ(signal, bandeBas, 44100);

  // Medium : 1000 Hz
  bandeMedium.Frequence := 1000;
  bandeMedium.Gain := gainMedium;
  bandeMedium.Q := 1.0;
  signal := AppliquerEQ(signal, bandeMedium, 44100);

  // Haut : 10000 Hz
  bandeHaut.Frequence := 10000;
  bandeHaut.Gain := gainHaut;
  bandeHaut.Q := 0.7;
  signal := AppliquerEQ(signal, bandeHaut, 44100);
end;
```

## Compression et expansion de données

### Compression simple par quantification

```pascal
function CompresserSignal(const signal: TSignal; bits: Integer): TSignal;
var
  i: Integer;
  niveaux, niveau: Integer;
  min, max, plage: Double;
begin
  SetLength(Result, Length(signal));

  // Trouver min et max
  min := signal[0];
  max := signal[0];
  for i := 1 to High(signal) do
  begin
    if signal[i] < min then min := signal[i];
    if signal[i] > max then max := signal[i];
  end;

  plage := max - min;
  niveaux := 1 shl bits;  // 2^bits

  // Quantifier
  for i := 0 to High(signal) do
  begin
    niveau := Round((signal[i] - min) / plage * (niveaux - 1));
    Result[i] := min + (niveau * plage / (niveaux - 1));
  end;
end;
```

### Codage μ-law (utilisé en téléphonie)

```pascal
function MuLawEncode(valeur: Double): Byte;
const
  MU = 255.0;
var
  signe: Integer;
  magnitude, compressed: Double;
begin
  // Normaliser entre -1 et 1
  valeur := Max(-1.0, Min(1.0, valeur));

  if valeur < 0 then
  begin
    signe := 1;
    magnitude := -valeur;
  end
  else
  begin
    signe := 0;
    magnitude := valeur;
  end;

  compressed := Ln(1 + MU * magnitude) / Ln(1 + MU);

  if signe = 1 then
    compressed := -compressed;

  Result := Round((compressed + 1) * 127.5);
end;

function MuLawDecode(code: Byte): Double;
const
  MU = 255.0;
var
  valeur: Double;
begin
  valeur := (code / 127.5) - 1;

  if valeur < 0 then
    Result := -(Power(1 + MU, -valeur) - 1) / MU
  else
    Result := (Power(1 + MU, valeur) - 1) / MU;
end;
```

## Détection de caractéristiques

### Détection d'énergie (activité vocale)

```pascal
function CalculerEnergie(const signal: TSignal; debut, longueur: Integer): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := debut to Min(debut + longueur - 1, High(signal)) do
    Result := Result + Sqr(signal[i]);
  Result := Result / longueur;
end;

function DetecterActiviteVocale(const signal: TSignal;
                               tailleFenetre: Integer;
                               seuil: Double): TBooleanArray;
var
  i, nbFenetres: Integer;
  energie: Double;
begin
  nbFenetres := Length(signal) div tailleFenetre;
  SetLength(Result, nbFenetres);

  for i := 0 to nbFenetres - 1 do
  begin
    energie := CalculerEnergie(signal, i * tailleFenetre, tailleFenetre);
    Result[i] := energie > seuil;
  end;
end;
```

### Détection de passages par zéro

```pascal
function CompterPassagesParZero(const signal: TSignal): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to High(signal) do
  begin
    if (signal[i-1] >= 0) and (signal[i] < 0) then
      Inc(Result)
    else if (signal[i-1] < 0) and (signal[i] >= 0) then
      Inc(Result);
  end;
end;

// Utile pour estimer la fréquence fondamentale
function EstimerFrequenceParZeroCrossing(const signal: TSignal;
                                         sampleRate: Integer): Double;
var
  nbPassages: Integer;
  duree: Double;
begin
  nbPassages := CompterPassagesParZero(signal);
  duree := Length(signal) / sampleRate;
  Result := (nbPassages / 2) / duree;  // Divisé par 2 car 1 cycle = 2 passages
end;
```

### Extraction d'enveloppe

```pascal
function ExtraireEnveloppe(const signal: TSignal;
                          tailleFenetre: Integer): TSignal;
var
  i, j: Integer;
  maxLocal: Double;
begin
  SetLength(Result, Length(signal));

  for i := 0 to High(signal) do
  begin
    maxLocal := 0;
    for j := Max(0, i - tailleFenetre div 2) to
             Min(High(signal), i + tailleFenetre div 2) do
    begin
      if Abs(signal[j]) > maxLocal then
        maxLocal := Abs(signal[j]);
    end;
    Result[i] := maxLocal;
  end;
end;
```

## Amélioration de signal

### Réduction de bruit spectral

```pascal
function ReduireBruitSpectral(const signal: TSignal;
                             seuilBruit: Double): TSignal;
var
  spectre: TComplexArray;
  i: Integer;
  magnitude, phase: Double;
begin
  // Transformer en domaine fréquentiel
  spectre := FFT(signal);

  // Appliquer un seuil sur chaque composante fréquentielle
  for i := 0 to High(spectre) do
  begin
    magnitude := Sqrt(Sqr(spectre[i].Real) + Sqr(spectre[i].Imag));
    phase := ArcTan2(spectre[i].Imag, spectre[i].Real);

    // Soustraction spectrale
    if magnitude < seuilBruit then
      magnitude := 0
    else
      magnitude := magnitude - seuilBruit;

    // Reconstruire
    spectre[i].Real := magnitude * Cos(phase);
    spectre[i].Imag := magnitude * Sin(phase);
  end;

  // Retour au domaine temporel
  Result := IFFT(spectre);
end;
```

### Filtre adaptatif LMS (Least Mean Squares)

```pascal
type
  TFiltreLMS = class
  private
    FCoefficients: array of Double;
    FTampon: array of Double;
    FMu: Double;  // Pas d'adaptation
  public
    constructor Create(ordre: Integer; mu: Double);
    function Filtrer(entree, desire: Double): Double;
  end;

constructor TFiltreLMS.Create(ordre: Integer; mu: Double);
var
  i: Integer;
begin
  SetLength(FCoefficients, ordre);
  SetLength(FTampon, ordre);
  FMu := mu;

  // Initialiser à zéro
  for i := 0 to ordre - 1 do
  begin
    FCoefficients[i] := 0;
    FTampon[i] := 0;
  end;
end;

function TFiltreLMS.Filtrer(entree, desire: Double): Double;
var
  i: Integer;
  sortie, erreur: Double;
begin
  // Décaler le tampon
  for i := High(FTampon) downto 1 do
    FTampon[i] := FTampon[i-1];
  FTampon[0] := entree;

  // Calculer la sortie
  sortie := 0;
  for i := 0 to High(FCoefficients) do
    sortie := sortie + FCoefficients[i] * FTampon[i];

  // Calculer l'erreur
  erreur := desire - sortie;

  // Mettre à jour les coefficients
  for i := 0 to High(FCoefficients) do
    FCoefficients[i] := FCoefficients[i] + FMu * erreur * FTampon[i];

  Result := sortie;
end;
```

## Visualisation avec TAChart

### Afficher un spectrogramme

```pascal
uses
  TAGraph, TASeries, Graphics;

procedure AfficherSpectrogramme(Chart: TChart;
                               const spectrogramme: TSpectrogramme);
var
  i, j: Integer;
  bitmap: TBitmap;
  magnitude, maxMag: Double;
  couleur: TColor;
begin
  // Trouver la magnitude maximale
  maxMag := 0;
  for i := 0 to High(spectrogramme) do
    for j := 0 to High(spectrogramme[i]) do
      if spectrogramme[i, j] > maxMag then
        maxMag := spectrogramme[i, j];

  // Créer une image
  bitmap := TBitmap.Create;
  try
    bitmap.Width := Length(spectrogramme);
    bitmap.Height := Length(spectrogramme[0]);

    // Dessiner le spectrogramme
    for i := 0 to bitmap.Width - 1 do
      for j := 0 to bitmap.Height - 1 do
      begin
        magnitude := spectrogramme[i, j] / maxMag;

        // Convertir en couleur (du bleu au rouge)
        if magnitude < 0.5 then
          couleur := RGB(0, 0, Round(magnitude * 512))
        else
          couleur := RGB(Round((magnitude - 0.5) * 512), 0, 255);

        bitmap.Canvas.Pixels[i, bitmap.Height - 1 - j] := couleur;
      end;

    // Afficher dans le graphique (nécessite adaptation)
    // Ou sauvegarder l'image
    bitmap.SaveToFile('spectrogramme.bmp');
  finally
    bitmap.Free;
  end;
end;
```

### Afficher signal et spectre côte à côte

```pascal
procedure AfficherSignalEtSpectre(Form: TForm; const signal: TSignal);
var
  ChartSignal, ChartSpectre: TChart;
  SerieTemps: TLineSeries;
  SerieFreq: TBarSeries;
  spectre: TComplexArray;
  i: Integer;
  magnitude: Double;
begin
  // Graphique du signal temporel
  ChartSignal := TChart.Create(Form);
  ChartSignal.Parent := Form;
  ChartSignal.Left := 10;
  ChartSignal.Top := 10;
  ChartSignal.Width := Form.Width div 2 - 20;
  ChartSignal.Height := Form.Height - 40;
  ChartSignal.Title.Text.Text := 'Signal temporel';

  SerieTemps := TLineSeries.Create(ChartSignal);
  for i := 0 to Min(1000, High(signal)) do
    SerieTemps.AddXY(i, signal[i]);
  ChartSignal.AddSeries(SerieTemps);

  // Graphique du spectre
  ChartSpectre := TChart.Create(Form);
  ChartSpectre.Parent := Form;
  ChartSpectre.Left := Form.Width div 2 + 10;
  ChartSpectre.Top := 10;
  ChartSpectre.Width := Form.Width div 2 - 20;
  ChartSpectre.Height := Form.Height - 40;
  ChartSpectre.Title.Text.Text := 'Spectre fréquentiel';

  SerieFreq := TBarSeries.Create(ChartSpectre);
  spectre := FFT(signal);

  for i := 0 to Min(500, Length(spectre) div 2) do
  begin
    magnitude := Sqrt(Sqr(spectre[i].Real) + Sqr(spectre[i].Imag));
    SerieFreq.AddXY(i, magnitude);
  end;
  ChartSpectre.AddSeries(SerieFreq);
end;
```

## Considérations multi-plateformes

### Gestion des fichiers audio

```pascal
{$IFDEF WINDOWS}
uses
  MMSystem;  // Wave files
{$ENDIF}

{$IFDEF UNIX}
uses
  BaseUnix;  // ALSA ou PulseAudio
{$ENDIF}

function ChargerFichierWAV(nomFichier: String;
                          out sampleRate: Integer): TSignal;
var
  {$IFDEF WINDOWS}
  fichier: TFileStream;
  header: array[0..43] of Byte;
  {$ENDIF}
  {$IFDEF UNIX}
  fichier: TFileStream;
  header: array[0..43] of Byte;
  {$ENDIF}
  dataSize, nbEchantillons: Integer;
  i: Integer;
  sample: SmallInt;
begin
  fichier := TFileStream.Create(nomFichier, fmOpenRead);
  try
    // Lire l'en-tête WAV (44 octets)
    fichier.Read(header, 44);

    // Vérifier que c'est un fichier WAV
    if (Chr(header[0]) + Chr(header[1]) + Chr(header[2]) + Chr(header[3])) <> 'RIFF' then
      raise Exception.Create('Pas un fichier WAV valide');

    if (Chr(header[8]) + Chr(header[9]) + Chr(header[10]) + Chr(header[11])) <> 'WAVE' then
      raise Exception.Create('Format WAVE non reconnu');

    // Extraire la fréquence d'échantillonnage (octets 24-27)
    sampleRate := header[24] + (header[25] shl 8) +
                  (header[26] shl 16) + (header[27] shl 24);

    // Extraire la taille des données (octets 40-43)
    dataSize := header[40] + (header[41] shl 8) +
                (header[42] shl 16) + (header[43] shl 24);

    // Calculer le nombre d'échantillons (16-bit = 2 octets par échantillon)
    nbEchantillons := dataSize div 2;
    SetLength(Result, nbEchantillons);

    // Lire les échantillons
    for i := 0 to nbEchantillons - 1 do
    begin
      fichier.Read(sample, 2);
      Result[i] := sample / 32768.0;  // Normaliser entre -1 et 1
    end;
  finally
    fichier.Free;
  end;
end;

procedure SauvegarderFichierWAV(nomFichier: String; const signal: TSignal;
                               sampleRate: Integer);
var
  fichier: TFileStream;
  header: array[0..43] of Byte;
  i, dataSize: Integer;
  sample: SmallInt;
begin
  fichier := TFileStream.Create(nomFichier, fmCreate);
  try
    dataSize := Length(signal) * 2;  // 2 octets par échantillon

    // Construire l'en-tête WAV
    // "RIFF"
    header[0] := Ord('R'); header[1] := Ord('I');
    header[2] := Ord('F'); header[3] := Ord('F');

    // Taille du fichier - 8
    i := 36 + dataSize;
    header[4] := i and $FF;
    header[5] := (i shr 8) and $FF;
    header[6] := (i shr 16) and $FF;
    header[7] := (i shr 24) and $FF;

    // "WAVE"
    header[8] := Ord('W'); header[9] := Ord('A');
    header[10] := Ord('V'); header[11] := Ord('E');

    // "fmt "
    header[12] := Ord('f'); header[13] := Ord('m');
    header[14] := Ord('t'); header[15] := Ord(' ');

    // Taille du bloc fmt (16 pour PCM)
    header[16] := 16; header[17] := 0;
    header[18] := 0; header[19] := 0;

    // Format audio (1 = PCM)
    header[20] := 1; header[21] := 0;

    // Nombre de canaux (1 = mono)
    header[22] := 1; header[23] := 0;

    // Fréquence d'échantillonnage
    header[24] := sampleRate and $FF;
    header[25] := (sampleRate shr 8) and $FF;
    header[26] := (sampleRate shr 16) and $FF;
    header[27] := (sampleRate shr 24) and $FF;

    // Byte rate (SampleRate * NumChannels * BitsPerSample/8)
    i := sampleRate * 1 * 2;
    header[28] := i and $FF;
    header[29] := (i shr 8) and $FF;
    header[30] := (i shr 16) and $FF;
    header[31] := (i shr 24) and $FF;

    // Block align (NumChannels * BitsPerSample/8)
    header[32] := 2; header[33] := 0;

    // Bits par échantillon
    header[34] := 16; header[35] := 0;

    // "data"
    header[36] := Ord('d'); header[37] := Ord('a');
    header[38] := Ord('t'); header[39] := Ord('a');

    // Taille des données
    header[40] := dataSize and $FF;
    header[41] := (dataSize shr 8) and $FF;
    header[42] := (dataSize shr 16) and $FF;
    header[43] := (dataSize shr 24) and $FF;

    // Écrire l'en-tête
    fichier.Write(header, 44);

    // Écrire les échantillons
    for i := 0 to High(signal) do
    begin
      sample := Round(signal[i] * 32767);  // Dénormaliser
      if sample > 32767 then sample := 32767;
      if sample < -32768 then sample := -32768;
      fichier.Write(sample, 2);
    end;
  finally
    fichier.Free;
  end;
end;
```

### Lecture audio multi-plateforme avec Bass

BASS est une bibliothèque audio cross-platform très populaire.

**Installation** :
```bash
# Ubuntu
wget http://www.un4seen.com/files/bass24-linux.zip
unzip bass24-linux.zip
sudo cp libbass.so /usr/lib/

# Windows : télécharger bass.dll et la placer dans le dossier du projet
```

**Wrapper FreePascal** :
```pascal
unit Bass;

interface

const
  {$IFDEF WINDOWS}
  BASS_DLL = 'bass.dll';
  {$ENDIF}
  {$IFDEF UNIX}
  BASS_DLL = 'libbass.so';
  {$ENDIF}

type
  HSTREAM = DWORD;
  QWORD = Int64;

// Constantes
const
  BASS_SAMPLE_FLOAT = $100;
  BASS_STREAM_DECODE = $200000;

// Fonctions principales
function BASS_Init(device: Integer; freq, flags: DWORD;
         win: HWND; clsid: Pointer): BOOL; cdecl; external BASS_DLL;

function BASS_StreamCreateFile(mem: BOOL; f: Pointer;
         offset: QWORD; length: QWORD; flags: DWORD): HSTREAM;
         cdecl; external BASS_DLL;

function BASS_ChannelPlay(handle: DWORD; restart: BOOL): BOOL;
         cdecl; external BASS_DLL;

function BASS_ChannelGetData(handle: DWORD; buffer: Pointer;
         length: DWORD): DWORD; cdecl; external BASS_DLL;

procedure BASS_Free; cdecl; external BASS_DLL;

implementation

end.
```

**Utilisation** :
```pascal
uses
  Bass;

procedure LireFichierAudio(nomFichier: String);
var
  stream: HSTREAM;
begin
  // Initialiser BASS
  if not BASS_Init(-1, 44100, 0, 0, nil) then
  begin
    WriteLn('Erreur d''initialisation BASS');
    Exit;
  end;

  // Charger le fichier
  stream := BASS_StreamCreateFile(False, PChar(nomFichier), 0, 0, 0);
  if stream = 0 then
  begin
    WriteLn('Erreur de chargement du fichier');
    BASS_Free;
    Exit;
  end;

  // Lire
  BASS_ChannelPlay(stream, False);

  // Attendre la fin
  Sleep(5000);

  // Nettoyer
  BASS_Free;
end;

procedure ExtraireEchantillons(nomFichier: String; out signal: TSignal);
var
  stream: HSTREAM;
  buffer: array of Single;
  bytesLus: DWORD;
  i, totalEchantillons: Integer;
begin
  BASS_Init(-1, 44100, 0, 0, nil);

  // Ouvrir en mode décodage
  stream := BASS_StreamCreateFile(False, PChar(nomFichier), 0, 0,
                                   BASS_STREAM_DECODE or BASS_SAMPLE_FLOAT);

  SetLength(buffer, 8192);
  totalEchantillons := 0;

  repeat
    bytesLus := BASS_ChannelGetData(stream, @buffer[0], Length(buffer) * 4);

    if bytesLus > 0 then
    begin
      SetLength(signal, totalEchantillons + bytesLus div 4);
      for i := 0 to (bytesLus div 4) - 1 do
        signal[totalEchantillons + i] := buffer[i];

      Inc(totalEchantillons, bytesLus div 4);
    end;
  until bytesLus = 0;

  BASS_Free;
end;
```

## Traitement en temps réel avec threads

### Architecture de traitement audio temps réel

```pascal
type
  TAudioProcessor = class(TThread)
  private
    FInputBuffer: TAudioBuffer;
    FOutputBuffer: TAudioBuffer;
    FProcessCallback: TAudioCallback;
    FSampleRate: Integer;
    FBufferSize: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(sampleRate, bufferSize: Integer);
    procedure SetProcessCallback(callback: TAudioCallback);
  end;

constructor TAudioProcessor.Create(sampleRate, bufferSize: Integer);
begin
  inherited Create(False);
  FSampleRate := sampleRate;
  FBufferSize := bufferSize;
  SetLength(FInputBuffer, bufferSize);
  SetLength(FOutputBuffer, bufferSize);
  FreeOnTerminate := False;
end;

procedure TAudioProcessor.Execute;
begin
  while not Terminated do
  begin
    // Lire depuis l'entrée audio (simplification)
    // ReadAudioInput(FInputBuffer);

    // Appeler le callback de traitement
    if Assigned(FProcessCallback) then
      FProcessCallback(FInputBuffer, FBufferSize);

    // Écrire vers la sortie audio
    // WriteAudioOutput(FInputBuffer);

    Sleep(1);  // Éviter de consommer 100% CPU
  end;
end;

procedure TAudioProcessor.SetProcessCallback(callback: TAudioCallback);
begin
  FProcessCallback := callback;
end;
```

### Exemple : Filtre en temps réel

```pascal
type
  TFormFiltreTempsReel = class(TForm)
    TrackBarCutoff: TTrackBar;
    LabelFrequence: TLabel;
    ButtonStart: TButton;
    ButtonStop: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure TrackBarCutoffChange(Sender: TObject);
  private
    FProcessor: TAudioProcessor;
    FFiltreIIR: array[0..1] of Double;  // État du filtre
    FCutoffFreq: Double;
    procedure ProcessAudio(buffer: TAudioBuffer; size: Integer);
  end;

procedure TFormFiltreTempsReel.ButtonStartClick(Sender: TObject);
begin
  FProcessor := TAudioProcessor.Create(44100, 512);
  FProcessor.SetProcessCallback(@ProcessAudio);
  LabelStatus.Caption := 'Traitement actif';
end;

procedure TFormFiltreTempsReel.ButtonStopClick(Sender: TObject);
begin
  if Assigned(FProcessor) then
  begin
    FProcessor.Terminate;
    FProcessor.WaitFor;
    FProcessor.Free;
    FProcessor := nil;
  end;
  LabelStatus.Caption := 'Arrêté';
end;

procedure TFormFiltreTempsReel.ProcessAudio(buffer: TAudioBuffer; size: Integer);
var
  i: Integer;
  alpha: Double;
begin
  // Calculer le coefficient du filtre
  alpha := 2 * Pi * FCutoffFreq / 44100;
  if alpha > 1 then alpha := 1;

  // Appliquer le filtre IIR
  for i := 0 to size - 1 do
  begin
    FFiltreIIR[0] := alpha * buffer[i] + (1 - alpha) * FFiltreIIR[1];
    buffer[i] := Round(FFiltreIIR[0]);
    FFiltreIIR[1] := FFiltreIIR[0];
  end;
end;

procedure TFormFiltreTempsReel.TrackBarCutoffChange(Sender: TObject);
begin
  FCutoffFreq := TrackBarCutoff.Position;
  LabelFrequence.Caption := Format('Fréquence de coupure: %d Hz',
                                    [Round(FCutoffFreq)]);
end;
```

## Analyse avancée

### Cepstre (analyse de la hauteur vocale)

```pascal
function CalculerCepstre(const signal: TSignal): TSignal;
var
  spectre: TComplexArray;
  logSpectre: array of Double;
  i: Integer;
  magnitude: Double;
begin
  // 1. Calculer la FFT
  spectre := FFT(signal);

  // 2. Calculer le log du spectre de magnitude
  SetLength(logSpectre, Length(spectre));
  for i := 0 to High(spectre) do
  begin
    magnitude := Sqrt(Sqr(spectre[i].Real) + Sqr(spectre[i].Imag));
    if magnitude > 0 then
      logSpectre[i] := Ln(magnitude)
    else
      logSpectre[i] := -10;  // Valeur minimale
  end;

  // 3. Calculer la FFT inverse (ou FFT du log)
  // Ceci donne le cepstre
  Result := FFT(logSpectre);
end;
```

### MFCC (Mel-Frequency Cepstral Coefficients)

Très utilisé en reconnaissance vocale.

```pascal
type
  TMelFilterBank = array of array of Double;

function CreerBanqueFiltresMel(nbFiltres, tailleFFT, sampleRate: Integer): TMelFilterBank;
var
  i, j: Integer;
  melMin, melMax, melStep: Double;
  freqMel: array of Double;
  binMel: array of Integer;

  function HzVersEMel(freq: Double): Double;
  begin
    Result := 2595 * Log10(1 + freq / 700);
  end;

  function MelVersHz(mel: Double): Double;
  begin
    Result := 700 * (Power(10, mel / 2595) - 1);
  end;

begin
  SetLength(Result, nbFiltres, tailleFFT div 2);
  SetLength(freqMel, nbFiltres + 2);
  SetLength(binMel, nbFiltres + 2);

  // Calculer les fréquences Mel
  melMin := HzVersMel(0);
  melMax := HzVersMel(sampleRate / 2);
  melStep := (melMax - melMin) / (nbFiltres + 1);

  for i := 0 to nbFiltres + 1 do
  begin
    freqMel[i] := MelVersHz(melMin + i * melStep);
    binMel[i] := Round(freqMel[i] * tailleFFT / sampleRate);
  end;

  // Créer les filtres triangulaires
  for i := 0 to nbFiltres - 1 do
  begin
    for j := 0 to tailleFFT div 2 - 1 do
    begin
      if (j >= binMel[i]) and (j < binMel[i+1]) then
        Result[i, j] := (j - binMel[i]) / (binMel[i+1] - binMel[i])
      else if (j >= binMel[i+1]) and (j <= binMel[i+2]) then
        Result[i, j] := (binMel[i+2] - j) / (binMel[i+2] - binMel[i+1])
      else
        Result[i, j] := 0;
    end;
  end;
end;

function CalculerMFCC(const signal: TSignal; nbCoefficients: Integer): TSignal;
var
  spectre: TComplexArray;
  banqueFiltres: TMelFilterBank;
  energiesFiltrees: array of Double;
  i, j: Integer;
  magnitude, energie: Double;
begin
  // 1. FFT
  spectre := FFT(signal);

  // 2. Créer la banque de filtres Mel
  banqueFiltres := CreerBanqueFiltresMel(26, Length(signal), 44100);

  // 3. Appliquer les filtres
  SetLength(energiesFiltrees, 26);
  for i := 0 to 25 do
  begin
    energie := 0;
    for j := 0 to Length(spectre) div 2 - 1 do
    begin
      magnitude := Sqrt(Sqr(spectre[j].Real) + Sqr(spectre[j].Imag));
      energie := energie + magnitude * banqueFiltres[i, j];
    end;
    if energie > 0 then
      energiesFiltrees[i] := Ln(energie)
    else
      energiesFiltrees[i] := -10;
  end;

  // 4. DCT (Discrete Cosine Transform)
  SetLength(Result, nbCoefficients);
  for i := 0 to nbCoefficients - 1 do
  begin
    Result[i] := 0;
    for j := 0 to 25 do
      Result[i] := Result[i] + energiesFiltrees[j] *
                   Cos(Pi * i * (j + 0.5) / 26);
  end;
end;
```

### Ondelettes (Wavelets)

Alternative à la transformée de Fourier, meilleure pour les signaux non-stationnaires.

```pascal
type
  TOndelette = (owHaar, owDaubechies4, owMorlet);

procedure TransformeeOndelettes(var signal: TSignal; ondelette: TOndelette);
var
  temp: TSignal;
  i, n: Integer;
begin
  n := Length(signal);
  SetLength(temp, n);

  case ondelette of
    owHaar:
    begin
      // Ondelette de Haar (la plus simple)
      for i := 0 to n div 2 - 1 do
      begin
        temp[i] := (signal[2*i] + signal[2*i+1]) / Sqrt(2);  // Approximation
        temp[n div 2 + i] := (signal[2*i] - signal[2*i+1]) / Sqrt(2);  // Détail
      end;
    end;

    owDaubechies4:
    begin
      // Coefficients de Daubechies-4
      const
        h0 = 0.6830127;
        h1 = 1.1830127;
        h2 = 0.3169873;
        h3 = -0.1830127;
      var
        g0, g1, g2, g3: Double;
      begin
        g0 := h3; g1 := -h2; g2 := h1; g3 := -h0;

        for i := 0 to n div 2 - 1 do
        begin
          temp[i] := h0 * signal[(2*i) mod n] +
                     h1 * signal[(2*i+1) mod n] +
                     h2 * signal[(2*i+2) mod n] +
                     h3 * signal[(2*i+3) mod n];

          temp[n div 2 + i] := g0 * signal[(2*i) mod n] +
                               g1 * signal[(2*i+1) mod n] +
                               g2 * signal[(2*i+2) mod n] +
                               g3 * signal[(2*i+3) mod n];
        end;
      end;
    end;
  end;

  signal := temp;
end;

procedure TransformeeOndelettesInverse(var signal: TSignal; ondelette: TOndelette);
var
  temp: TSignal;
  i, n: Integer;
begin
  n := Length(signal);
  SetLength(temp, n);

  case ondelette of
    owHaar:
    begin
      for i := 0 to n div 2 - 1 do
      begin
        temp[2*i] := (signal[i] + signal[n div 2 + i]) / Sqrt(2);
        temp[2*i+1] := (signal[i] - signal[n div 2 + i]) / Sqrt(2);
      end;
    end;

    // Autres ondelettes...
  end;

  signal := temp;
end;
```

## Optimisation des performances

### Utilisation de SIMD (SSE/AVX)

FreePascal supporte les intrinsèques SIMD pour accélérer les calculs.

```pascal
{$IFDEF CPUX86_64}
uses
  cpu;

procedure AdditionVectorielleSSE(var a, b: array of Single; n: Integer);
var
  i: Integer;
  va, vb: TM128;
begin
  i := 0;
  while i < n - 3 do
  begin
    va := LoadPS(@a[i]);
    vb := LoadPS(@b[i]);
    va := AddPS(va, vb);
    StorePS(@a[i], va);
    Inc(i, 4);
  end;

  // Traiter les éléments restants
  while i < n do
  begin
    a[i] := a[i] + b[i];
    Inc(i);
  end;
end;
{$ENDIF}
```

### Optimisation de la FFT

```pascal
// Utiliser des tailles de puissance de 2
procedure OptimiserTailleFFT(var signal: TSignal);
var
  nouvelleTaille, i: Integer;
begin
  // Trouver la prochaine puissance de 2
  nouvelleTaille := 1;
  while nouvelleTaille < Length(signal) do
    nouvelleTaille := nouvelleTaille shl 1;

  // Ajouter du zero-padding
  SetLength(signal, nouvelleTaille);
  for i := Length(signal) to nouvelleTaille - 1 do
    signal[i] := 0;
end;

// Pré-calculer les facteurs de rotation
var
  TwiddleFactors: TComplexArray;

procedure PrecalculerTwiddle(n: Integer);
var
  i: Integer;
  angle: Double;
begin
  SetLength(TwiddleFactors, n div 2);
  for i := 0 to n div 2 - 1 do
  begin
    angle := -2 * Pi * i / n;
    TwiddleFactors[i].Real := Cos(angle);
    TwiddleFactors[i].Imag := Sin(angle);
  end;
end;
```

### Traitement par blocs

```pascal
procedure TraiterParBlocs(const signal: TSignal; tailleBloc: Integer;
                         callback: TBlocCallback);
var
  i, nbBlocs: Integer;
  bloc: TSignal;
begin
  nbBlocs := Length(signal) div tailleBloc;
  SetLength(bloc, tailleBloc);

  for i := 0 to nbBlocs - 1 do
  begin
    // Copier le bloc
    Move(signal[i * tailleBloc], bloc[0], tailleBloc * SizeOf(Double));

    // Traiter
    callback(bloc);

    // Recopier
    Move(bloc[0], signal[i * tailleBloc], tailleBloc * SizeOf(Double));
  end;
end;
```

## Projet complet : Station de traitement audio

```pascal
unit UnitStationAudio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, TAGraph, TASeries, Menus;

type
  TFormStation = class(TForm)
    // Composants UI
    PanelControles: TPanel;
    PanelGraphiques: TPanel;
    ButtonCharger: TButton;
    ButtonLire: TButton;
    ButtonSauvegarder: TButton;
    TrackBarVolume: TTrackBar;
    ComboBoxEffet: TComboBox;
    GroupBoxEgaliseur: TGroupBox;
    TrackBarBasse: TTrackBar;
    TrackBarMedium: TTrackBar;
    TrackBarAigu: TTrackBar;
    ChartSignal: TChart;
    ChartSpectre: TChart;
    SerieSignal: TLineSeries;
    SerieSpectre: TBarSeries;
    MainMenu1: TMainMenu;
    MenuFichier: TMenuItem;
    MenuEffets: TMenuItem;
    StatusBar1: TStatusBar;

    // Événements
    procedure FormCreate(Sender: TObject);
    procedure ButtonChargerClick(Sender: TObject);
    procedure ButtonLireClick(Sender: TObject);
    procedure ButtonSauvegarderClick(Sender: TObject);
    procedure TrackBarVolumeChange(Sender: TObject);
    procedure ComboBoxEffetChange(Sender: TObject);
    procedure TrackBarEgaliseurChange(Sender: TObject);

  private
    FSignal: TSignal;
    FSignalOriginal: TSignal;
    FSampleRate: Integer;
    FLecture: Boolean;

    procedure ChargerFichier(nomFichier: String);
    procedure MettreAJourAffichage;
    procedure AppliquerEffet;
    procedure AppliquerEgaliseur;
  public

  end;

var
  FormStation: TFormStation;

implementation

{$R *.lfm}

procedure TFormStation.FormCreate(Sender: TObject);
begin
  FSampleRate := 44100;
  FLecture := False;

  // Initialiser les contrôles
  ComboBoxEffet.Items.Add('Aucun');
  ComboBoxEffet.Items.Add('Écho');
  ComboBoxEffet.Items.Add('Réverbération');
  ComboBoxEffet.Items.Add('Distorsion');
  ComboBoxEffet.Items.Add('Chorus');
  ComboBoxEffet.ItemIndex := 0;

  TrackBarVolume.Position := 100;
  TrackBarBasse.Position := 0;
  TrackBarMedium.Position := 0;
  TrackBarAigu.Position := 0;

  StatusBar1.SimpleText := 'Prêt';
end;

procedure TFormStation.ButtonChargerClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Self);
  try
    OpenDialog.Filter := 'Fichiers WAV|*.wav|Tous les fichiers|*.*';
    if OpenDialog.Execute then
    begin
      ChargerFichier(OpenDialog.FileName);
      StatusBar1.SimpleText := Format('Chargé: %s (%d Hz, %.2f s)',
        [ExtractFileName(OpenDialog.FileName), FSampleRate,
         Length(FSignal) / FSampleRate]);
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormStation.ChargerFichier(nomFichier: String);
begin
  try
    FSignal := ChargerFichierWAV(nomFichier, FSampleRate);
    FSignalOriginal := Copy(FSignal);
    MettreAJourAffichage;
  except
    on E: Exception do
      ShowMessage('Erreur de chargement: ' + E.Message);
  end;
end;

procedure TFormStation.MettreAJourAffichage;
var
  i, pas: Integer;
  spectre: TComplexArray;
  magnitude: Double;
begin
  if Length(FSignal) = 0 then Exit;

  // Afficher le signal temporel
  SerieSignal.Clear;
  pas := Max(1, Length(FSignal) div 2000);
  for i := 0 to Length(FSignal) - 1 do
  begin
    if i mod pas = 0 then
      SerieSignal.AddXY(i / FSampleRate, FSignal[i]);
  end;

  // Afficher le spectre
  SerieSpectre.Clear;
  spectre := FFT(Copy(FSignal, 0, Min(8192, Length(FSignal))));

  for i := 0 to Min(500, Length(spectre) div 2) do
  begin
    magnitude := Sqrt(Sqr(spectre[i].Real) + Sqr(spectre[i].Imag));
    SerieSpectre.AddXY(i * FSampleRate / Length(spectre), magnitude);
  end;

  ChartSignal.LeftAxis.Title.Caption := 'Amplitude';
  ChartSignal.BottomAxis.Title.Caption := 'Temps (s)';
  ChartSpectre.LeftAxis.Title.Caption := 'Magnitude';
  ChartSpectre.BottomAxis.Title.Caption := 'Fréquence (Hz)';
end;

procedure TFormStation.ButtonLireClick(Sender: TObject);
begin
  if Length(FSignal) = 0 then
  begin
    ShowMessage('Aucun signal chargé');
    Exit;
  end;

  if not FLecture then
  begin
    // Démarrer la lecture
    FLecture := True;
    ButtonLire.Caption := 'Arrêter';

    // Ici, intégrer avec BASS ou autre bibliothèque audio
    // LireSignal(FSignal, FSampleRate);

    StatusBar1.SimpleText := 'Lecture en cours...';
  end
  else
  begin
    // Arrêter la lecture
    FLecture := False;
    ButtonLire.Caption := 'Lire';
    StatusBar1.SimpleText := 'Lecture arrêtée';
  end;
end;

procedure TFormStation.ButtonSauvegarderClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  if Length(FSignal) = 0 then
  begin
    ShowMessage('Aucun signal à sauvegarder');
    Exit;
  end;

  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.Filter := 'Fichiers WAV|*.wav';
    SaveDialog.DefaultExt := 'wav';

    if SaveDialog.Execute then
    begin
      SauvegarderFichierWAV(SaveDialog.FileName, FSignal, FSampleRate);
      StatusBar1.SimpleText := 'Fichier sauvegardé: ' +
                               ExtractFileName(SaveDialog.FileName);
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TFormStation.TrackBarVolumeChange(Sender: TObject);
var
  gain: Double;
  i: Integer;
begin
  if Length(FSignal) = 0 then Exit;

  gain := TrackBarVolume.Position / 100;

  // Restaurer le signal original
  FSignal := Copy(FSignalOriginal);

  // Appliquer le gain
  for i := 0 to High(FSignal) do
    FSignal[i] := FSignal[i] * gain;

  // Appliquer les autres effets
  AppliquerEffet;
  AppliquerEgaliseur;

  MettreAJourAffichage;
  StatusBar1.SimpleText := Format('Volume: %d%%', [TrackBarVolume.Position]);
end;

procedure TFormStation.ComboBoxEffetChange(Sender: TObject);
begin
  AppliquerEffet;
  MettreAJourAffichage;
end;

procedure TFormStation.AppliquerEffet;
begin
  if Length(FSignal) = 0 then Exit;

  case ComboBoxEffet.ItemIndex of
    0: ; // Aucun effet

    1: // Écho
    begin
      FSignal := AjouterEcho(FSignal, 0.3, 0.5, FSampleRate);
      StatusBar1.SimpleText := 'Effet: Écho appliqué';
    end;

    2: // Réverbération
    begin
      FSignal := AjouterReverb(FSignal, FSampleRate);
      StatusBar1.SimpleText := 'Effet: Réverbération appliquée';
    end;

    3: // Distorsion
    begin
      FSignal := AppliquerDistorsion(FSignal, 5.0);
      StatusBar1.SimpleText := 'Effet: Distorsion appliquée';
    end;

    4: // Chorus
    begin
      FSignal := AppliquerChorus(FSignal, FSampleRate);
      StatusBar1.SimpleText := 'Effet: Chorus appliqué';
    end;
  end;
end;

procedure TFormStation.TrackBarEgaliseurChange(Sender: TObject);
begin
  AppliquerEgaliseur;
  MettreAJourAffichage;
end;

procedure TFormStation.AppliquerEgaliseur;
var
  gainBas, gainMedium, gainAigu: Double;
begin
  if Length(FSignal) = 0 then Exit;

  // Convertir les positions en dB
  gainBas := TrackBarBasse.Position;
  gainMedium := TrackBarMedium.Position;
  gainAigu := TrackBarAigu.Position;

  // Appliquer l'égaliseur
  EgaliseurTroisBandes(FSignal, gainBas, gainMedium, gainAigu);

  StatusBar1.SimpleText := Format('EQ: Bas=%d Med=%d Aigu=%d',
    [Round(gainBas), Round(gainMedium), Round(gainAigu)]);
end;

end.
```

## Ressources et bibliothèques supplémentaires

### 1. PortAudio - Audio multi-plateforme

PortAudio est une bibliothèque cross-platform pour l'entrée/sortie audio.

**Installation** :
```bash
# Ubuntu
sudo apt-get install portaudio19-dev

# Windows : télécharger depuis http://www.portaudio.com/
```

**Exemple d'utilisation** :
```pascal
unit PortAudio;

interface

const
  {$IFDEF WINDOWS}
  PA_DLL = 'portaudio_x64.dll';
  {$ENDIF}
  {$IFDEF UNIX}
  PA_DLL = 'libportaudio.so.2';
  {$ENDIF}

type
  PaStream = Pointer;
  PaError = LongInt;
  PaStreamCallback = function(input, output: Pointer; frameCount: Cardinal;
    timeInfo, statusFlags, userData: Pointer): Integer; cdecl;

// Fonctions principales
function Pa_Initialize: PaError; cdecl; external PA_DLL;
function Pa_Terminate: PaError; cdecl; external PA_DLL;
function Pa_OpenDefaultStream(var stream: PaStream; numInputChannels,
  numOutputChannels: Integer; sampleFormat: Cardinal; sampleRate: Double;
  framesPerBuffer: Cardinal; callback: PaStreamCallback;
  userData: Pointer): PaError; cdecl; external PA_DLL;
function Pa_StartStream(stream: PaStream): PaError; cdecl; external PA_DLL;
function Pa_StopStream(stream: PaStream): PaError; cdecl; external PA_DLL;
function Pa_CloseStream(stream: PaStream): PaError; cdecl; external PA_DLL;

implementation

end.
```

### 2. SoX - Swiss Army Knife du traitement audio

Appeler SoX depuis FreePascal pour des opérations complexes.

```pascal
uses
  Process;

procedure AppliquerEffetSoX(fichierEntree, fichierSortie, effet: String);
var
  commande: String;
begin
  {$IFDEF WINDOWS}
  commande := 'sox.exe';
  {$ENDIF}
  {$IFDEF UNIX}
  commande := 'sox';
  {$ENDIF}

  // Exemple : ajouter un écho avec SoX
  RunCommand(commande, [fichierEntree, fichierSortie, effet], []);
end;

// Utilisation
begin
  AppliquerEffetSoX('entree.wav', 'sortie.wav', 'echo 0.8 0.9 1000 0.3');
end;
```

### 3. Aubio - Détection de pitch et tempo

```pascal
unit Aubio;

interface

const
  {$IFDEF UNIX}
  AUBIO_LIB = 'libaubio.so.5';
  {$ENDIF}
  {$IFDEF WINDOWS}
  AUBIO_LIB = 'aubio.dll';
  {$ENDIF}

type
  aubio_pitch_t = Pointer;
  fvec_t = Pointer;

function new_aubio_pitch(method: PChar; buf_size, hop_size, samplerate: Cardinal):
  aubio_pitch_t; cdecl; external AUBIO_LIB;
procedure del_aubio_pitch(o: aubio_pitch_t); cdecl; external AUBIO_LIB;
procedure aubio_pitch_do(o: aubio_pitch_t; input, output: fvec_t);
  cdecl; external AUBIO_LIB;

implementation

end.
```

## Bonnes pratiques DSP

### 1. Gestion des artefacts

```pascal
// Éviter les clics lors de la jonction de segments
procedure FonduEnchaineSignaux(var signal1, signal2: TSignal;
                              longueurFondu: Integer);
var
  i: Integer;
  facteur: Double;
begin
  for i := 0 to longueurFondu - 1 do
  begin
    facteur := i / longueurFondu;
    signal1[Length(signal1) - longueurFondu + i] :=
      signal1[Length(signal1) - longueurFondu + i] * (1 - facteur) +
      signal2[i] * facteur;
  end;
end;

// Fondu en entrée
procedure FonduEntree(var signal: TSignal; duree: Integer);
var
  i: Integer;
  facteur: Double;
begin
  for i := 0 to Min(duree - 1, High(signal)) do
  begin
    facteur := i / duree;
    signal[i] := signal[i] * facteur;
  end;
end;

// Fondu en sortie
procedure FonduSortie(var signal: TSignal; duree: Integer);
var
  i, debut: Integer;
  facteur: Double;
begin
  debut := Max(0, Length(signal) - duree);
  for i := debut to High(signal) do
  begin
    facteur := 1 - (i - debut) / duree;
    signal[i] := signal[i] * facteur;
  end;
end;
```

### 2. Prévention de l'aliasing

```pascal
// Anti-aliasing avant downsampling
function DownsampleAvecAntiAliasing(const signal: TSignal;
                                   facteur: Integer): TSignal;
var
  signalFiltre: TSignal;
  i: Integer;
begin
  // D'abord, filtrer passe-bas à Fs/2*facteur
  signalFiltre := FiltrePasseBas(signal, Length(signal) div (2 * facteur));

  // Puis, décimer
  SetLength(Result, Length(signal) div facteur);
  for i := 0 to High(Result) do
    Result[i] := signalFiltre[i * facteur];
end;

// Upsampling avec interpolation
function UpsampleAvecInterpolation(const signal: TSignal;
                                  facteur: Integer): TSignal;
var
  i, j: Integer;
begin
  SetLength(Result, Length(signal) * facteur);

  // Insérer des zéros
  for i := 0 to High(signal) do
  begin
    Result[i * facteur] := signal[i];
    for j := 1 to facteur - 1 do
      Result[i * facteur + j] := 0;
  end;

  // Filtrer pour interpoler
  Result := FiltrePasseBas(Result, Length(Result) div (2 * facteur));

  // Amplifier
  for i := 0 to High(Result) do
    Result[i] := Result[i] * facteur;
end;
```

### 3. Normalisation et protection contre le clipping

```pascal
function NormaliserAvecProtection(const signal: TSignal;
                                 niveau: Double = 0.95): TSignal;
var
  i: Integer;
  maxVal, facteur: Double;
begin
  SetLength(Result, Length(signal));

  // Trouver le maximum absolu
  maxVal := 0;
  for i := 0 to High(signal) do
    if Abs(signal[i]) > maxVal then
      maxVal := Abs(signal[i]);

  // Calculer le facteur de normalisation
  if maxVal > 0 then
    facteur := niveau / maxVal
  else
    facteur := 1;

  // Appliquer
  for i := 0 to High(signal) do
    Result[i] := signal[i] * facteur;
end;

// Limiteur doux (soft limiter)
function LimiteurDoux(valeur, seuil: Double): Double;
begin
  if Abs(valeur) <= seuil then
    Result := valeur
  else if valeur > 0 then
    Result := seuil + (1 - seuil) * Tanh((valeur - seuil) / (1 - seuil))
  else
    Result := -seuil + (1 - seuil) * Tanh((valeur + seuil) / (1 - seuil));
end;

procedure AppliquerLimiteur(var signal: TSignal; seuil: Double);
var
  i: Integer;
begin
  for i := 0 to High(signal) do
    signal[i] := LimiteurDoux(signal[i], seuil);
end;
```

### 4. Gestion de la latence

```pascal
type
  TBufferCirculaire = class
  private
    FBuffer: array of Double;
    FTaille: Integer;
    FLecture: Integer;
    FEcriture: Integer;
  public
    constructor Create(taille: Integer);
    procedure Ecrire(valeur: Double);
    function Lire: Double;
    function Disponible: Integer;
  end;

constructor TBufferCirculaire.Create(taille: Integer);
begin
  FTaille := taille;
  SetLength(FBuffer, taille);
  FLecture := 0;
  FEcriture := 0;
end;

procedure TBufferCirculaire.Ecrire(valeur: Double);
begin
  FBuffer[FEcriture] := valeur;
  FEcriture := (FEcriture + 1) mod FTaille;
end;

function TBufferCirculaire.Lire: Double;
begin
  Result := FBuffer[FLecture];
  FLecture := (FLecture + 1) mod FTaille;
end;

function TBufferCirculaire.Disponible: Integer;
begin
  if FEcriture >= FLecture then
    Result := FEcriture - FLecture
  else
    Result := FTaille - FLecture + FEcriture;
end;
```

## Tests et validation

### 1. Génération de signaux de test

```pascal
// Signal impulsionnel
function GenererImpulsion(position, longueur: Integer): TSignal;
var
  i: Integer;
begin
  SetLength(Result, longueur);
  for i := 0 to longueur - 1 do
    Result[i] := 0;

  if (position >= 0) and (position < longueur) then
    Result[position] := 1.0;
end;

// Bruit rose (1/f)
function GenererBruitRose(longueur, sampleRate: Integer): TSignal;
var
  bruitBlanc: TSignal;
  spectre: TComplexArray;
  i: Integer;
  freq: Double;
begin
  // Générer du bruit blanc
  bruitBlanc := GenererBruitBlanc(1.0, longueur / sampleRate, sampleRate);

  // Transformer en fréquentiel
  spectre := FFT(bruitBlanc);

  // Appliquer le filtrage 1/f
  for i := 1 to Length(spectre) div 2 do
  begin
    freq := i * sampleRate / Length(spectre);
    spectre[i].Real := spectre[i].Real / Sqrt(freq);
    spectre[i].Imag := spectre[i].Imag / Sqrt(freq);
  end;

  // Retour au domaine temporel
  Result := IFFT(spectre);
  NormaliserSignal(Result);
end;

// Sweep linéaire (chirp)
function GenererSweep(freqDebut, freqFin, duree: Double;
                     sampleRate: Integer): TSignal;
var
  i, nbEchantillons: Integer;
  t, freq, phase: Double;
begin
  nbEchantillons := Round(duree * sampleRate);
  SetLength(Result, nbEchantillons);

  phase := 0;
  for i := 0 to nbEchantillons - 1 do
  begin
    t := i / sampleRate;
    freq := freqDebut + (freqFin - freqDebut) * t / duree;
    phase := phase + 2 * Pi * freq / sampleRate;
    Result[i] := Sin(phase);
  end;
end;
```

### 2. Mesures de qualité

```pascal
// Rapport signal/bruit (SNR)
function CalculerSNR(const signal, bruit: TSignal): Double;
var
  i: Integer;
  puissanceSignal, puissanceBruit: Double;
begin
  puissanceSignal := 0;
  puissanceBruit := 0;

  for i := 0 to Min(High(signal), High(bruit)) do
  begin
    puissanceSignal := puissanceSignal + Sqr(signal[i]);
    puissanceBruit := puissanceBruit + Sqr(bruit[i]);
  end;

  if puissanceBruit > 0 then
    Result := 10 * Log10(puissanceSignal / puissanceBruit)
  else
    Result := Infinity;
end;

// Distorsion harmonique totale (THD)
function CalculerTHD(const signal: TSignal; freqFondamentale: Double;
                    sampleRate: Integer): Double;
var
  spectre: TComplexArray;
  indexFond, i: Integer;
  puissanceFond, puissanceHarmoniques, magnitude: Double;
const
  NB_HARMONIQUES = 5;
begin
  spectre := FFT(signal);

  // Index de la fréquence fondamentale
  indexFond := Round(freqFondamentale * Length(signal) / sampleRate);

  // Puissance de la fondamentale
  magnitude := Sqrt(Sqr(spectre[indexFond].Real) +
                    Sqr(spectre[indexFond].Imag));
  puissanceFond := Sqr(magnitude);

  // Puissance des harmoniques
  puissanceHarmoniques := 0;
  for i := 2 to NB_HARMONIQUES do
  begin
    magnitude := Sqrt(Sqr(spectre[i * indexFond].Real) +
                      Sqr(spectre[i * indexFond].Imag));
    puissanceHarmoniques := puissanceHarmoniques + Sqr(magnitude);
  end;

  // THD en pourcentage
  if puissanceFond > 0 then
    Result := 100 * Sqrt(puissanceHarmoniques / puissanceFond)
  else
    Result := 0;
end;

// Corrélation croisée normalisée
function CalculerCorrelationNormalisee(const signal1, signal2: TSignal): Double;
var
  i: Integer;
  somme12, somme1, somme2, sommeCarres1, sommeCarres2: Double;
  n: Integer;
begin
  n := Min(Length(signal1), Length(signal2));
  somme12 := 0;
  somme1 := 0;
  somme2 := 0;
  sommeCarres1 := 0;
  sommeCarres2 := 0;

  for i := 0 to n - 1 do
  begin
    somme12 := somme12 + signal1[i] * signal2[i];
    somme1 := somme1 + signal1[i];
    somme2 := somme2 + signal2[i];
    sommeCarres1 := sommeCarres1 + Sqr(signal1[i]);
    sommeCarres2 := sommeCarres2 + Sqr(signal2[i]);
  end;

  Result := (n * somme12 - somme1 * somme2) /
            Sqrt((n * sommeCarres1 - Sqr(somme1)) *
                 (n * sommeCarres2 - Sqr(somme2)));
end;
```

## Débogage et visualisation

### 1. Enregistrement de traces

```pascal
procedure EnregistrerTrace(const signal: TSignal; nomFichier: String);
var
  fichier: TextFile;
  i: Integer;
begin
  AssignFile(fichier, nomFichier);
  Rewrite(fichier);
  try
    for i := 0 to High(signal) do
      WriteLn(fichier, Format('%.8f', [signal[i]]));
  finally
    CloseFile(fichier);
  end;
end;

// Pour visualiser avec gnuplot ou autre outil externe
procedure VisualiserAvecGnuplot(const signal: TSignal; titre: String);
var
  nomFichier: String;
begin
  nomFichier := 'temp_signal.dat';
  EnregistrerTrace(signal, nomFichier);

  {$IFDEF UNIX}
  ExecuteProcess('gnuplot',
    ['-e', Format('plot "%s" with lines title "%s"; pause -1',
                  [nomFichier, titre])]);
  {$ENDIF}
end;
```

### 2. Validation par comparaison

```pascal
procedure ComparerSignaux(const signal1, signal2: TSignal;
                         tolerance: Double);
var
  i: Integer;
  diff, maxDiff: Double;
begin
  maxDiff := 0;
  for i := 0 to Min(High(signal1), High(signal2)) do
  begin
    diff := Abs(signal1[i] - signal2[i]);
    if diff > maxDiff then
      maxDiff := diff;
  end;

  WriteLn(Format('Différence maximale: %.8f', [maxDiff]));

  if maxDiff > tolerance then
    WriteLn('ATTENTION: La différence dépasse la tolérance!')
  else
    WriteLn('OK: Les signaux sont similaires');
end;
```

## Conclusion et perspectives

### Points clés du traitement du signal

✓ **Comprendre les bases** : Fréquence d'échantillonnage, théorème de Nyquist  
✓ **Maîtriser la FFT** : Transformation temps-fréquence  
✓ **Utiliser les filtres** : Passe-bas, passe-haut, passe-bande  
✓ **Gérer les effets** : Écho, réverbération, distorsion  
✓ **Optimiser** : SIMD, traitement par blocs, FFT optimisée  
✓ **Tester** : Validation avec signaux de test  
✓ **Multi-plateforme** : Code portable Windows/Ubuntu

### Applications avancées

- **Intelligence artificielle** : Reconnaissance vocale, classification audio
- **Musique** : Synthèse, analyse harmonique, séparation de sources
- **Biomédical** : ECG, EEG, traitement d'images médicales
- **Télécommunications** : Modulation, démodulation, correction d'erreurs
- **Géophysique** : Analyse sismique, prospection
- **Radar/Sonar** : Détection de cibles, traitement d'échos

### Ressources pour approfondir

**Livres** :
- "Understanding Digital Signal Processing" - Richard G. Lyons
- "The Scientist and Engineer's Guide to Digital Signal Processing"
- "Digital Signal Processing: A Practical Approach" - Emmanuel Ifeachor

**Cours en ligne** :
- Coursera : Digital Signal Processing
- MIT OpenCourseWare : Signals and Systems
- YouTube : 3Blue1Brown (séries sur la transformée de Fourier)

**Outils** :
- **Audacity** : Éditeur audio open source pour expérimenter
- **Octave/MATLAB** : Prototypage rapide d'algorithmes
- **Python + NumPy/SciPy** : Alternative pour tests

**Documentation FreePascal** :
- Wiki FreePascal : Section Math et DSP
- Forum Lazarus : Catégorie "Multimedia"
- GitHub : Exemples de projets DSP

### Liens avec d'autres chapitres

- **16.1 NumLib** : Calculs mathématiques pour DSP
- **16.2 TAChart** : Visualisation des signaux et spectres
- **15. Intelligence Artificielle** : Traitement audio pour IA
- **20. Optimisation** : Accélération des calculs DSP
- **11. Multithreading** : Traitement parallèle de signaux

### Exercice final suggéré

Créez une application complète de traitement audio qui :
1. Charge un fichier WAV
2. Affiche le signal temporel et le spectre
3. Applique des filtres en temps réel
4. Détecte la note jouée (pitch detection)
5. Applique des effets (écho, réverb, chorus)
6. Sauvegarde le résultat
7. Fonctionne sur Windows et Ubuntu

Le traitement du signal est un domaine passionnant qui combine mathématiques, programmation et créativité. Avec FreePascal/Lazarus, vous avez tous les outils pour créer des applications DSP professionnelles et multi-plateformes !

**Bon développement et bonne exploration du monde fascinant du DSP !** 🎵🔊📊

⏭️ [Algèbre linéaire et matrices](/16-traitement-donnees-calcul-scientifique/04-algebre-lineaire-matrices.md)
