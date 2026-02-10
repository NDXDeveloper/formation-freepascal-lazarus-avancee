🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.7 Protocoles binaires personnalisés

## Introduction aux protocoles binaires

### Qu'est-ce qu'un protocole binaire ?

Un protocole binaire est un format de communication qui utilise directement des octets (bytes) au lieu de texte pour échanger des données entre ordinateurs ou applications.

**Analogie simple :** Imaginez deux personnes qui communiquent :
- **Protocole texte (JSON, XML)** : Elles parlent avec des phrases complètes "Envoyer 42 pommes"
- **Protocole binaire** : Elles utilisent un code secret compact "E#42P"

### Pourquoi créer son propre protocole binaire ?

Les protocoles binaires personnalisés sont utiles quand vous avez besoin de :

1. **Performance maximale** - Moins de données à transférer
2. **Temps réel** - Jeux vidéo, streaming, IoT
3. **Bande passante limitée** - Réseaux satellites, connexions lentes
4. **Sécurité par obscurité** - Format non standard plus difficile à décoder
5. **Contrôle total** - Vous définissez exactement le format

### Avantages et inconvénients

| Avantages | Inconvénients |
|-----------|---------------|
| ✅ Taille réduite (2 à 10 fois moins) | ❌ Plus difficile à déboguer |
| ✅ Traitement plus rapide | ❌ Nécessite documentation précise |
| ✅ Économie de bande passante | ❌ Moins compatible entre plateformes |
| ✅ Précision pour nombres | ❌ Évolution plus complexe |
| ✅ Adapté au temps réel | ❌ Outils de test spécialisés |

### Comparaison : Texte vs Binaire

**Exemple : Envoyer la position d'un joueur**

**Format JSON (texte) :**
```json
{"x":123.45,"y":678.90,"angle":45.5,"health":100}
```
**Taille : 50 octets**

**Format binaire personnalisé :**
```
[4 octets float x][4 octets float y][4 octets float angle][1 octet health]
```
**Taille : 13 octets** (réduction de 74% !)

## Concepts fondamentaux

### Les types de données de base

En programmation binaire, tout est une suite d'octets. Voici les types fondamentaux :

| Type Pascal | Taille | Plage de valeurs | Usage |
|-------------|--------|------------------|-------|
| Byte | 1 octet | 0 à 255 | Petits nombres, flags |
| ShortInt | 1 octet | -128 à 127 | Nombres signés petits |
| Word | 2 octets | 0 à 65535 | Nombres moyens |
| SmallInt | 2 octets | -32768 à 32767 | Nombres signés moyens |
| LongWord | 4 octets | 0 à 4 milliards | Grands nombres |
| Integer | 4 octets | -2 milliards à 2 milliards | Nombres standards |
| Single | 4 octets | ±3.4e38 | Nombres à virgule |
| Double | 8 octets | ±1.7e308 | Grande précision |

### L'ordre des octets (Endianness)

Quand un nombre prend plusieurs octets, dans quel ordre les envoyer ?

**Exemple : Le nombre 1234 (hexadécimal : 0x04D2)**

**Big Endian (ordre naturel) :**
```
Octet 1: 04  
Octet 2: D2
```

**Little Endian (inversé) :**
```
Octet 1: D2  
Octet 2: 04
```

**Important :**
- Windows/Linux PC utilisent **Little Endian**
- Les réseaux utilisent traditionnellement **Big Endian** (network byte order)
- ARM peut être configuré dans les deux modes

**Solution :** FreePascal fournit des fonctions de conversion automatique :
```pascal
uses
  sockets; // Pour htonl, htons, ntohl, ntohs

var
  ValeurLocale, ValeurReseau: LongWord;
begin
  ValeurLocale := 1234;
  ValeurReseau := htonl(ValeurLocale); // Host TO Network Long
  // Envoyer ValeurReseau sur le réseau
end;
```

### Structure d'un message binaire

Un message binaire typique contient :

```
[En-tête][Données utiles][Somme de contrôle]
```

**En-tête :** Informations sur le message
- Numéro de version du protocole
- Type de message
- Taille des données
- Numéro de séquence

**Données utiles :** Le contenu réel

**Somme de contrôle :** Pour vérifier l'intégrité

## Créer un protocole simple

### Exemple 1 : Protocole de chat binaire

Créons un protocole pour un système de chat simple.

**Spécification du protocole :**

```
MESSAGE CHAT SIMPLE v1.0

Structure générale :
[1 octet : Type de message]
[2 octets : Taille des données]
[N octets : Données]
[1 octet : Checksum XOR]

Types de messages :
0x01 = Message texte
0x02 = Demande de pseudo
0x03 = Réponse pseudo
0x04 = Ping
0x05 = Pong
```

**Implémentation en Pascal :**

```pascal
unit ProtocoleChat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  // Types de messages
  MSG_TEXTE = $01;
  MSG_DEMANDE_PSEUDO = $02;
  MSG_REPONSE_PSEUDO = $03;
  MSG_PING = $04;
  MSG_PONG = $05;

  // Taille maximale d'un message
  TAILLE_MAX_MESSAGE = 1024;

type
  // Structure d'un message
  TMessageChat = record
    TypeMessage: Byte;
    TailleDonnees: Word;
    Donnees: array[0..TAILLE_MAX_MESSAGE-1] of Byte;
    Checksum: Byte;
  end;

  // Classe pour encoder/décoder les messages
  TProtocoleChat = class
  public
    // Créer un message texte
    class function CreerMessageTexte(const Texte: String): TMessageChat;

    // Créer un ping
    class function CreerPing: TMessageChat;

    // Créer un pong
    class function CreerPong: TMessageChat;

    // Encoder un message en octets pour l'envoi
    class function EncoderMessage(const Msg: TMessageChat): TBytes;

    // Décoder des octets reçus en message
    class function DecoderMessage(const Octets: TBytes): TMessageChat;

    // Calculer le checksum d'un message
    class function CalculerChecksum(const Msg: TMessageChat): Byte;

    // Vérifier l'intégrité d'un message
    class function VerifierMessage(const Msg: TMessageChat): Boolean;
  end;

implementation

class function TProtocoleChat.CreerMessageTexte(const Texte: String): TMessageChat;  
var
  i: Integer;
  Octets: TBytes;
begin
  Result.TypeMessage := MSG_TEXTE;

  // Convertir le texte en octets UTF-8
  Octets := TEncoding.UTF8.GetBytes(Texte);
  Result.TailleDonnees := Length(Octets);

  // Copier les octets dans le tableau de données
  for i := 0 to Result.TailleDonnees - 1 do
    Result.Donnees[i] := Octets[i];

  // Calculer le checksum
  Result.Checksum := CalculerChecksum(Result);
end;

class function TProtocoleChat.CreerPing: TMessageChat;  
begin
  Result.TypeMessage := MSG_PING;
  Result.TailleDonnees := 0;
  Result.Checksum := CalculerChecksum(Result);
end;

class function TProtocoleChat.CreerPong: TMessageChat;  
begin
  Result.TypeMessage := MSG_PONG;
  Result.TailleDonnees := 0;
  Result.Checksum := CalculerChecksum(Result);
end;

class function TProtocoleChat.EncoderMessage(const Msg: TMessageChat): TBytes;  
var
  i, Position: Integer;
begin
  // Taille totale : 1 (type) + 2 (taille) + N (données) + 1 (checksum)
  SetLength(Result, 4 + Msg.TailleDonnees);

  Position := 0;

  // Type de message
  Result[Position] := Msg.TypeMessage;
  Inc(Position);

  // Taille des données (2 octets, big endian)
  Result[Position] := (Msg.TailleDonnees shr 8) and $FF; // Octet de poids fort
  Inc(Position);
  Result[Position] := Msg.TailleDonnees and $FF; // Octet de poids faible
  Inc(Position);

  // Données
  for i := 0 to Msg.TailleDonnees - 1 do
  begin
    Result[Position] := Msg.Donnees[i];
    Inc(Position);
  end;

  // Checksum
  Result[Position] := Msg.Checksum;
end;

class function TProtocoleChat.DecoderMessage(const Octets: TBytes): TMessageChat;  
var
  i, Position: Integer;
begin
  if Length(Octets) < 4 then
    raise Exception.Create('Message trop court');

  Position := 0;

  // Type de message
  Result.TypeMessage := Octets[Position];
  Inc(Position);

  // Taille des données (2 octets, big endian)
  Result.TailleDonnees := (Octets[Position] shl 8) or Octets[Position + 1];
  Inc(Position, 2);

  // Vérifier la cohérence
  if Length(Octets) < 4 + Result.TailleDonnees then
    raise Exception.Create('Taille de message incohérente');

  // Données
  for i := 0 to Result.TailleDonnees - 1 do
  begin
    Result.Donnees[i] := Octets[Position];
    Inc(Position);
  end;

  // Checksum
  Result.Checksum := Octets[Position];

  // Vérifier l'intégrité
  if not VerifierMessage(Result) then
    raise Exception.Create('Checksum invalide');
end;

class function TProtocoleChat.CalculerChecksum(const Msg: TMessageChat): Byte;  
var
  i: Integer;
begin
  // Checksum XOR simple : XOR de tous les octets
  Result := Msg.TypeMessage;
  Result := Result xor (Msg.TailleDonnees shr 8);
  Result := Result xor (Msg.TailleDonnees and $FF);

  for i := 0 to Msg.TailleDonnees - 1 do
    Result := Result xor Msg.Donnees[i];
end;

class function TProtocoleChat.VerifierMessage(const Msg: TMessageChat): Boolean;  
begin
  Result := Msg.Checksum = CalculerChecksum(Msg);
end;

end.
```

### Utilisation du protocole

**Côté émetteur (envoi) :**

```pascal
program EmetteurChat;

uses
  ProtocoleChat, Classes, SysUtils, Sockets;

var
  Socket: TSocket;
  Message: TMessageChat;
  OctetsAEnvoyer: TBytes;
begin
  // Créer un message texte
  Message := TProtocoleChat.CreerMessageTexte('Bonjour le monde !');

  // L'encoder en octets
  OctetsAEnvoyer := TProtocoleChat.EncoderMessage(Message);

  // Afficher les octets (pour débogage)
  WriteLn('Octets à envoyer : ');
  for var Octet in OctetsAEnvoyer do
    Write(IntToHex(Octet, 2), ' ');
  WriteLn;

  // Envoyer sur le socket
  // fpSend(Socket, @OctetsAEnvoyer[0], Length(OctetsAEnvoyer), 0);
end.
```

**Côté récepteur (réception) :**

```pascal
program RecepteurChat;

uses
  ProtocoleChat, Classes, SysUtils, Sockets;

var
  Socket: TSocket;
  Buffer: array[0..1023] of Byte;
  OctetsRecus: TBytes;
  NbOctets: Integer;
  Message: TMessageChat;
  Texte: String;
begin
  // Recevoir des octets du socket
  // NbOctets := fpRecv(Socket, @Buffer[0], 1024, 0);

  // Simuler une réception pour l'exemple
  NbOctets := 10;

  if NbOctets > 0 then
  begin
    // Copier dans un TBytes
    SetLength(OctetsRecus, NbOctets);
    Move(Buffer[0], OctetsRecus[0], NbOctets);

    try
      // Décoder le message
      Message := TProtocoleChat.DecoderMessage(OctetsRecus);

      // Traiter selon le type
      case Message.TypeMessage of
        MSG_TEXTE:
          begin
            // Extraire le texte
            SetLength(Texte, Message.TailleDonnees);
            Move(Message.Donnees[0], Texte[1], Message.TailleDonnees);
            WriteLn('Message reçu : ', Texte);
          end;

        MSG_PING:
          begin
            WriteLn('Ping reçu, envoi d''un pong...');
            // Envoyer un pong en réponse
          end;
      end;

    except
      on E: Exception do
        WriteLn('Erreur de décodage : ', E.Message);
    end;
  end;
end.
```

## Exemple 2 : Protocole de jeu multijoueur

Pour un jeu en réseau, on a besoin d'envoyer fréquemment des positions de joueurs.

### Spécification

```
PROTOCOLE JEU MULTIJOUEUR v1.0

Message de position de joueur :
[1 octet : 0x10 = Type "Position joueur"]
[4 octets : ID du joueur (LongWord)]
[4 octets : Position X (Single - float)]
[4 octets : Position Y (Single - float)]
[4 octets : Position Z (Single - float)]
[4 octets : Angle de vue (Single - float)]
[1 octet : État (0=immobile, 1=marche, 2=court, 3=saute)]
[1 octet : Santé (0-100)]
[2 octets : CRC16]

Total : 26 octets par mise à jour de position
```

### Implémentation

```pascal
unit ProtocoleJeu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MSG_POSITION_JOUEUR = $10;
  MSG_TIR = $11;
  MSG_DEGATS = $12;

type
  TEtatJoueur = (ejImmobile, ejMarche, ejCourt, ejSaute);

  TPositionJoueur = packed record
    TypeMessage: Byte;
    IDJoueur: LongWord;
    X, Y, Z: Single;
    Angle: Single;
    Etat: Byte;
    Sante: Byte;
    CRC: Word;
  end;

  TProtocoleJeu = class
  public
    class function EncoderPosition(ID: LongWord; X, Y, Z, Angle: Single;
                                    Etat: TEtatJoueur; Sante: Byte): TBytes;
    class function DecoderPosition(const Octets: TBytes): TPositionJoueur;
    class function CalculerCRC16(const Data; Taille: Integer): Word;
  end;

implementation

class function TProtocoleJeu.EncoderPosition(ID: LongWord; X, Y, Z, Angle: Single;
                                             Etat: TEtatJoueur; Sante: Byte): TBytes;
var
  Pos: TPositionJoueur;
begin
  Pos.TypeMessage := MSG_POSITION_JOUEUR;
  Pos.IDJoueur := ID;
  Pos.X := X;
  Pos.Y := Y;
  Pos.Z := Z;
  Pos.Angle := Angle;
  Pos.Etat := Ord(Etat);
  Pos.Sante := Sante;

  // Calculer le CRC sur tout sauf le champ CRC lui-même
  Pos.CRC := CalculerCRC16(Pos, SizeOf(TPositionJoueur) - SizeOf(Word));

  // Convertir la structure en tableau d'octets
  SetLength(Result, SizeOf(TPositionJoueur));
  Move(Pos, Result[0], SizeOf(TPositionJoueur));
end;

class function TProtocoleJeu.DecoderPosition(const Octets: TBytes): TPositionJoueur;  
var
  CRCCalcule: Word;
begin
  if Length(Octets) <> SizeOf(TPositionJoueur) then
    raise Exception.Create('Taille de message invalide');

  // Copier les octets dans la structure
  Move(Octets[0], Result, SizeOf(TPositionJoueur));

  // Vérifier le CRC
  CRCCalcule := CalculerCRC16(Result, SizeOf(TPositionJoueur) - SizeOf(Word));
  if CRCCalcule <> Result.CRC then
    raise Exception.Create('CRC invalide');
end;

class function TProtocoleJeu.CalculerCRC16(const Data; Taille: Integer): Word;  
var
  i, j: Integer;
  Octet: Byte;
  Ptr: PByte;
begin
  Result := $FFFF;
  Ptr := @Data;

  for i := 0 to Taille - 1 do
  begin
    Octet := Ptr^;
    Result := Result xor Octet;

    for j := 0 to 7 do
    begin
      if (Result and 1) <> 0 then
        Result := (Result shr 1) xor $A001
      else
        Result := Result shr 1;
    end;

    Inc(Ptr);
  end;
end;

end.
```

### Utilisation dans un jeu

```pascal
program ServeurJeu;

uses
  ProtocoleJeu, Sockets, SysUtils;

var
  Position: TBytes;
  PosDecoded: TPositionJoueur;
begin
  // Encoder la position d'un joueur
  Position := TProtocoleJeu.EncoderPosition(
    42,           // ID joueur
    100.5,        // X
    200.75,       // Y
    50.0,         // Z
    45.0,         // Angle
    ejCourt,      // État : en train de courir
    85            // Santé
  );

  WriteLn('Taille du message : ', Length(Position), ' octets');

  // Envoyer à tous les clients
  // BroadcastToAllClients(Position);

  // Réception et décodage
  try
    PosDecoded := TProtocoleJeu.DecoderPosition(Position);
    WriteLn('Joueur ', PosDecoded.IDJoueur, ' à position (',
            PosDecoded.X:0:2, ', ', PosDecoded.Y:0:2, ', ', PosDecoded.Z:0:2, ')');
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end.
```

## Techniques avancées

### 1. Compression des données

Pour des données répétitives, on peut utiliser la compression :

```pascal
uses
  zstream;

function CompresserDonnees(const Donnees: TBytes): TBytes;  
var
  InputStream, OutputStream: TBytesStream;
  Compresseur: TCompressionStream;
begin
  InputStream := TBytesStream.Create(Donnees);
  OutputStream := TBytesStream.Create;
  try
    Compresseur := TCompressionStream.Create(clMax, OutputStream);
    try
      Compresseur.CopyFrom(InputStream, InputStream.Size);
    finally
      Compresseur.Free;
    end;

    Result := OutputStream.Bytes;
    SetLength(Result, OutputStream.Size);
  finally
    InputStream.Free;
    OutputStream.Free;
  end;
end;

function DecompresserDonnees(const Donnees: TBytes): TBytes;  
var
  InputStream, OutputStream: TBytesStream;
  Decompresseur: TDecompressionStream;
begin
  InputStream := TBytesStream.Create(Donnees);
  OutputStream := TBytesStream.Create;
  try
    Decompresseur := TDecompressionStream.Create(InputStream);
    try
      OutputStream.CopyFrom(Decompresseur, 0);
    finally
      Decompresseur.Free;
    end;

    Result := OutputStream.Bytes;
    SetLength(Result, OutputStream.Size);
  finally
    InputStream.Free;
    OutputStream.Free;
  end;
end;
```

### 2. Delta encoding (encodage différentiel)

Pour les positions qui changent peu, envoyer seulement la différence :

```pascal
type
  TPositionDelta = packed record
    TypeMessage: Byte;      // 0x20 = Delta position
    IDJoueur: LongWord;
    DeltaX: SmallInt;       // -32768 à 32767 (en millimètres)
    DeltaY: SmallInt;
    DeltaZ: SmallInt;
    DeltaAngle: SmallInt;   // En dixièmes de degré
    Etat: Byte;
    DeltaSante: ShortInt;   // -128 à 127
    CRC: Word;
  end;
  // Taille : 18 octets au lieu de 26 (économie de 30%)
```

### 3. Bit packing (empaquetage de bits)

Pour économiser encore plus, on peut empaqueter plusieurs valeurs dans un seul octet :

```pascal
type
  TFlagsJoueur = packed record
    // 1 octet contient plusieurs informations
    // Bits 0-1 : État (4 valeurs possibles)
    // Bit 2 : Est armé (0 ou 1)
    // Bit 3 : Est en vie (0 ou 1)
    // Bit 4 : Est accroupi (0 ou 1)
    // Bits 5-7 : Direction regard (8 directions)
    Flags: Byte;
  end;

function CreerFlags(Etat: TEtatJoueur; Arme, Vivant, Accroupi: Boolean;
                    Direction: 0..7): Byte;
begin
  Result := Ord(Etat) and $03;                    // Bits 0-1
  if Arme then Result := Result or $04;           // Bit 2
  if Vivant then Result := Result or $08;         // Bit 3
  if Accroupi then Result := Result or $10;       // Bit 4
  Result := Result or ((Direction and $07) shl 5); // Bits 5-7
end;

procedure ExtraireFlags(Flags: Byte; out Etat: TEtatJoueur;
                       out Arme, Vivant, Accroupi: Boolean; out Direction: Byte);
begin
  Etat := TEtatJoueur(Flags and $03);
  Arme := (Flags and $04) <> 0;
  Vivant := (Flags and $08) <> 0;
  Accroupi := (Flags and $10) <> 0;
  Direction := (Flags shr 5) and $07;
end;
```

### 4. Numéros de séquence

Pour détecter les messages perdus ou dans le désordre :

```pascal
type
  TMessageAvecSequence = packed record
    TypeMessage: Byte;
    NumeroSequence: LongWord;  // Incrémenté à chaque message
    // ... autres données
  end;

var
  DernierNumeroRecu: LongWord = 0;

procedure TraiterMessage(const Msg: TMessageAvecSequence);  
begin
  if Msg.NumeroSequence <= DernierNumeroRecu then
  begin
    WriteLn('Message en doublon ou désordonné ignoré');
    Exit;
  end;

  if Msg.NumeroSequence > DernierNumeroRecu + 1 then
    WriteLn('Attention : ', Msg.NumeroSequence - DernierNumeroRecu - 1,
            ' message(s) perdu(s)');

  DernierNumeroRecu := Msg.NumeroSequence;

  // Traiter le message normalement
end;
```

### 5. Horodatage (Timestamp)

Pour synchroniser les événements et mesurer la latence :

```pascal
type
  TMessageHorodate = packed record
    TypeMessage: Byte;
    Timestamp: Int64;  // Millisecondes depuis epoch
    // ... autres données
  end;

function ObtenirTimestamp: Int64;  
begin
  Result := DateTimeToUnix(Now) * 1000 + MilliSecondOf(Now);
end;

function CalculerLatence(const Msg: TMessageHorodate): Integer;  
begin
  Result := ObtenirTimestamp - Msg.Timestamp;
  WriteLn('Latence : ', Result, ' ms');
end;
```

## Gestion des buffers et streaming

### Problème du découpage des messages

Sur TCP, les données peuvent arriver par morceaux. Il faut gérer un buffer :

```pascal
type
  TRecepteurMessages = class
  private
    FBuffer: TBytes;
    FPosition: Integer;
  public
    procedure AjouterDonnees(const Nouvelles: TBytes);
    function ExtraireMessage: TBytes;  // nil si message incomplet
  end;

procedure TRecepteurMessages.AjouterDonnees(const Nouvelles: TBytes);  
var
  AncienneTaille: Integer;
begin
  AncienneTaille := Length(FBuffer);
  SetLength(FBuffer, AncienneTaille + Length(Nouvelles));
  Move(Nouvelles[0], FBuffer[AncienneTaille], Length(Nouvelles));
end;

function TRecepteurMessages.ExtraireMessage: TBytes;  
var
  TailleMessage: Word;
begin
  Result := nil;

  // Vérifier qu'on a au moins l'en-tête (3 octets minimum)
  if Length(FBuffer) < 3 then
    Exit;

  // Lire la taille du message (octets 1-2)
  TailleMessage := (FBuffer[1] shl 8) or FBuffer[2];

  // Vérifier qu'on a le message complet
  if Length(FBuffer) < 4 + TailleMessage then
    Exit;  // Message incomplet

  // Extraire le message
  SetLength(Result, 4 + TailleMessage);
  Move(FBuffer[0], Result[0], 4 + TailleMessage);

  // Retirer ce message du buffer
  if Length(FBuffer) > 4 + TailleMessage then
  begin
    Move(FBuffer[4 + TailleMessage], FBuffer[0],
         Length(FBuffer) - 4 - TailleMessage);
    SetLength(FBuffer, Length(FBuffer) - 4 - TailleMessage);
  end
  else
    SetLength(FBuffer, 0);
end;
```

### Utilisation du buffer

```pascal
var
  Recepteur: TRecepteurMessages;
  Socket: TSocket;
  Buffer: array[0..4095] of Byte;
  NbOctets: Integer;
  Message: TBytes;
begin
  Recepteur := TRecepteurMessages.Create;
  try
    while True do
    begin
      // Recevoir des données du réseau
      NbOctets := fpRecv(Socket, @Buffer[0], 4096, 0);

      if NbOctets > 0 then
      begin
        // Ajouter au buffer
        SetLength(Message, NbOctets);
        Move(Buffer[0], Message[0], NbOctets);
        Recepteur.AjouterDonnees(Message);

        // Extraire tous les messages complets disponibles
        repeat
          Message := Recepteur.ExtraireMessage;
          if Message <> nil then
            TraiterMessage(Message);
        until Message = nil;
      end;
    end;
  finally
    Recepteur.Free;
  end;
end;
```

## Sérialisation et désérialisation

### Classe générique pour la sérialisation

```pascal
type
  TSerializeur = class
  private
    FStream: TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;

    // Écriture
    procedure EcrireByte(Valeur: Byte);
    procedure EcrireWord(Valeur: Word);
    procedure EcrireLongWord(Valeur: LongWord);
    procedure EcrireSingle(Valeur: Single);
    procedure EcrireDouble(Valeur: Double);
    procedure EcrireString(const Valeur: String);
    procedure EcrireBoolean(Valeur: Boolean);

    // Lecture
    function LireByte: Byte;
    function LireWord: Word;
    function LireLongWord: LongWord;
    function LireSingle: Single;
    function LireDouble: Double;
    function LireString: String;
    function LireBoolean: Boolean;

    // Obtenir les données sérialisées
    function ObtenirDonnees: TBytes;

    // Charger des données pour désérialisation
    procedure ChargerDonnees(const Donnees: TBytes);

    // Réinitialiser
    procedure Reinitialiser;
  end;

implementation

constructor TSerializeur.Create;  
begin
  inherited Create;
  FStream := TMemoryStream.Create;
end;

destructor TSerializeur.Destroy;  
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TSerializeur.EcrireByte(Valeur: Byte);  
begin
  FStream.WriteByte(Valeur);
end;

procedure TSerializeur.EcrireWord(Valeur: Word);  
var
  ValeurReseau: Word;
begin
  // Convertir en big endian (network byte order)
  ValeurReseau := htons(Valeur);
  FStream.Write(ValeurReseau, SizeOf(Word));
end;

procedure TSerializeur.EcrireLongWord(Valeur: LongWord);  
var
  ValeurReseau: LongWord;
begin
  ValeurReseau := htonl(Valeur);
  FStream.Write(ValeurReseau, SizeOf(LongWord));
end;

procedure TSerializeur.EcrireSingle(Valeur: Single);  
begin
  // Les floats sont écrits directement (attention à l'endianness si nécessaire)
  FStream.Write(Valeur, SizeOf(Single));
end;

procedure TSerializeur.EcrireDouble(Valeur: Double);  
begin
  FStream.Write(Valeur, SizeOf(Double));
end;

procedure TSerializeur.EcrireString(const Valeur: String);  
var
  Octets: TBytes;
  Longueur: Word;
begin
  // Convertir en UTF-8
  Octets := TEncoding.UTF8.GetBytes(Valeur);
  Longueur := Length(Octets);

  // Écrire d'abord la longueur
  EcrireWord(Longueur);

  // Puis les octets
  if Longueur > 0 then
    FStream.Write(Octets[0], Longueur);
end;

procedure TSerializeur.EcrireBoolean(Valeur: Boolean);  
begin
  if Valeur then
    EcrireByte(1)
  else
    EcrireByte(0);
end;

function TSerializeur.LireByte: Byte;  
begin
  Result := FStream.ReadByte;
end;

function TSerializeur.LireWord: Word;  
var
  ValeurReseau: Word;
begin
  FStream.Read(ValeurReseau, SizeOf(Word));
  Result := ntohs(ValeurReseau);
end;

function TSerializeur.LireLongWord: LongWord;  
var
  ValeurReseau: LongWord;
begin
  FStream.Read(ValeurReseau, SizeOf(LongWord));
  Result := ntohl(ValeurReseau);
end;

function TSerializeur.LireSingle: Single;  
begin
  FStream.Read(Result, SizeOf(Single));
end;

function TSerializeur.LireDouble: Double;  
begin
  FStream.Read(Result, SizeOf(Double));
end;

function TSerializeur.LireString: String;  
var
  Longueur: Word;
  Octets: TBytes;
begin
  Longueur := LireWord;

  if Longueur > 0 then
  begin
    SetLength(Octets, Longueur);
    FStream.Read(Octets[0], Longueur);
    Result := TEncoding.UTF8.GetString(Octets);
  end
  else
    Result := '';
end;

function TSerializeur.LireBoolean: Boolean;  
begin
  Result := LireByte <> 0;
end;

function TSerializeur.ObtenirDonnees: TBytes;  
begin
  SetLength(Result, FStream.Size);
  if FStream.Size > 0 then
  begin
    FStream.Position := 0;
    FStream.Read(Result[0], FStream.Size);
  end;
end;

procedure TSerializeur.ChargerDonnees(const Donnees: TBytes);  
begin
  FStream.Clear;
  if Length(Donnees) > 0 then
    FStream.Write(Donnees[0], Length(Donnees));
  FStream.Position := 0;
end;

procedure TSerializeur.Reinitialiser;  
begin
  FStream.Clear;
end;
```

### Utilisation de la classe de sérialisation

```pascal
program ExempleSerialiseur;

uses
  Serializeur, SysUtils;

type
  TJoueur = record
    ID: LongWord;
    Nom: String;
    Score: Integer;
    X, Y, Z: Single;
    EstActif: Boolean;
  end;

procedure SerialiserJoueur(const Joueur: TJoueur; out Donnees: TBytes);  
var
  S: TSerializeur;
begin
  S := TSerializeur.Create;
  try
    S.EcrireLongWord(Joueur.ID);
    S.EcrireString(Joueur.Nom);
    S.EcrireLongWord(Joueur.Score);
    S.EcrireSingle(Joueur.X);
    S.EcrireSingle(Joueur.Y);
    S.EcrireSingle(Joueur.Z);
    S.EcrireBoolean(Joueur.EstActif);

    Donnees := S.ObtenirDonnees;
  finally
    S.Free;
  end;
end;

function DeserialiserJoueur(const Donnees: TBytes): TJoueur;  
var
  S: TSerializeur;
begin
  S := TSerializeur.Create;
  try
    S.ChargerDonnees(Donnees);

    Result.ID := S.LireLongWord;
    Result.Nom := S.LireString;
    Result.Score := S.LireLongWord;
    Result.X := S.LireSingle;
    Result.Y := S.LireSingle;
    Result.Z := S.LireSingle;
    Result.EstActif := S.LireBoolean;
  finally
    S.Free;
  end;
end;

var
  Joueur1, Joueur2: TJoueur;
  Donnees: TBytes;
begin
  // Créer un joueur
  Joueur1.ID := 12345;
  Joueur1.Nom := 'SuperGamer';
  Joueur1.Score := 9999;
  Joueur1.X := 100.5;
  Joueur1.Y := 200.75;
  Joueur1.Z := 50.0;
  Joueur1.EstActif := True;

  // Sérialiser
  SerialiserJoueur(Joueur1, Donnees);
  WriteLn('Données sérialisées : ', Length(Donnees), ' octets');

  // Désérialiser
  Joueur2 := DeserialiserJoueur(Donnees);

  // Vérifier
  WriteLn('ID : ', Joueur2.ID);
  WriteLn('Nom : ', Joueur2.Nom);
  WriteLn('Score : ', Joueur2.Score);
  WriteLn('Position : (', Joueur2.X:0:2, ', ', Joueur2.Y:0:2, ', ', Joueur2.Z:0:2, ')');
  WriteLn('Actif : ', Joueur2.EstActif);
end.
```

## Protocoles avec longueur variable

### Pattern TLV (Type-Length-Value)

Le pattern TLV est très utilisé dans les protocoles réseau professionnels :

```
[Type : 1 octet][Longueur : 2 octets][Valeur : N octets]
```

**Avantages :**
- Extensible : on peut ajouter de nouveaux types sans casser la compatibilité
- Flexible : chaque champ peut avoir une taille différente
- Parsable : on peut ignorer les types inconnus

```pascal
type
  TTLVType = (
    tlvNom = 1,
    tlvAge = 2,
    tlvEmail = 3,
    tlvAdresse = 4,
    tlvTelephone = 5
  );

  TTLVChamp = record
    TypeChamp: Byte;
    Longueur: Word;
    Valeur: TBytes;
  end;

  TProtocoleTLV = class
  public
    class function CreerChamp(TypeChamp: TTLVType; const Valeur: TBytes): TTLVChamp;
    class function EncoderChamp(const Champ: TTLVChamp): TBytes;
    class function DecoderChamp(const Octets: TBytes; var Position: Integer): TTLVChamp;
    class function EncoderMessage(const Champs: array of TTLVChamp): TBytes;
    class function DecoderMessage(const Octets: TBytes): TArray<TTLVChamp>;
  end;

implementation

class function TProtocoleTLV.CreerChamp(TypeChamp: TTLVType;
                                        const Valeur: TBytes): TTLVChamp;
begin
  Result.TypeChamp := Ord(TypeChamp);
  Result.Longueur := Length(Valeur);
  Result.Valeur := Valeur;
end;

class function TProtocoleTLV.EncoderChamp(const Champ: TTLVChamp): TBytes;  
var
  Position: Integer;
begin
  SetLength(Result, 3 + Champ.Longueur);
  Position := 0;

  // Type
  Result[Position] := Champ.TypeChamp;
  Inc(Position);

  // Longueur (big endian)
  Result[Position] := (Champ.Longueur shr 8) and $FF;
  Inc(Position);
  Result[Position] := Champ.Longueur and $FF;
  Inc(Position);

  // Valeur
  if Champ.Longueur > 0 then
    Move(Champ.Valeur[0], Result[Position], Champ.Longueur);
end;

class function TProtocoleTLV.DecoderChamp(const Octets: TBytes;
                                          var Position: Integer): TTLVChamp;
begin
  if Position + 3 > Length(Octets) then
    raise Exception.Create('Données insuffisantes pour lire le champ TLV');

  // Type
  Result.TypeChamp := Octets[Position];
  Inc(Position);

  // Longueur
  Result.Longueur := (Octets[Position] shl 8) or Octets[Position + 1];
  Inc(Position, 2);

  // Valeur
  if Position + Result.Longueur > Length(Octets) then
    raise Exception.Create('Longueur de champ TLV invalide');

  SetLength(Result.Valeur, Result.Longueur);
  if Result.Longueur > 0 then
  begin
    Move(Octets[Position], Result.Valeur[0], Result.Longueur);
    Inc(Position, Result.Longueur);
  end;
end;

class function TProtocoleTLV.EncoderMessage(const Champs: array of TTLVChamp): TBytes;  
var
  i, Position, TailleTotal: Integer;
  ChampEncode: TBytes;
begin
  // Calculer la taille totale
  TailleTotal := 0;
  for i := 0 to High(Champs) do
    TailleTotal := TailleTotal + 3 + Champs[i].Longueur;

  SetLength(Result, TailleTotal);
  Position := 0;

  // Encoder chaque champ
  for i := 0 to High(Champs) do
  begin
    ChampEncode := EncoderChamp(Champs[i]);
    Move(ChampEncode[0], Result[Position], Length(ChampEncode));
    Inc(Position, Length(ChampEncode));
  end;
end;

class function TProtocoleTLV.DecoderMessage(const Octets: TBytes): TArray<TTLVChamp>;  
var
  Position: Integer;
  Champ: TTLVChamp;
  Liste: TList;
  i: Integer;
begin
  Liste := TList.Create;
  try
    Position := 0;

    while Position < Length(Octets) do
    begin
      Champ := DecoderChamp(Octets, Position);
      Liste.Add(@Champ);
    end;

    SetLength(Result, Liste.Count);
    for i := 0 to Liste.Count - 1 do
      Result[i] := TTLVChamp(Liste[i]^);
  finally
    Liste.Free;
  end;
end;
```

### Exemple d'utilisation TLV

```pascal
program ExempleTLV;

uses
  ProtocoleTLV, SysUtils;

var
  Champs: array[0..2] of TTLVChamp;
  Message, NomOctets, AgeOctets, EmailOctets: TBytes;
  ChampsDecodes: TArray<TTLVChamp>;
  i: Integer;
begin
  // Créer des champs
  NomOctets := TEncoding.UTF8.GetBytes('Jean Dupont');
  Champs[0] := TProtocoleTLV.CreerChamp(tlvNom, NomOctets);

  SetLength(AgeOctets, 1);
  AgeOctets[0] := 35;
  Champs[1] := TProtocoleTLV.CreerChamp(tlvAge, AgeOctets);

  EmailOctets := TEncoding.UTF8.GetBytes('jean.dupont@example.com');
  Champs[2] := TProtocoleTLV.CreerChamp(tlvEmail, EmailOctets);

  // Encoder le message
  Message := TProtocoleTLV.EncoderMessage(Champs);
  WriteLn('Message encodé : ', Length(Message), ' octets');

  // Décoder le message
  ChampsDecodes := TProtocoleTLV.DecoderMessage(Message);
  WriteLn('Nombre de champs décodés : ', Length(ChampsDecodes));

  // Afficher les champs
  for i := 0 to High(ChampsDecodes) do
  begin
    WriteLn('Champ ', i, ':');
    WriteLn('  Type : ', ChampsDecodes[i].TypeChamp);
    WriteLn('  Longueur : ', ChampsDecodes[i].Longueur);

    case TTLVType(ChampsDecodes[i].TypeChamp) of
      tlvNom, tlvEmail:
        WriteLn('  Valeur : ', TEncoding.UTF8.GetString(ChampsDecodes[i].Valeur));
      tlvAge:
        WriteLn('  Valeur : ', ChampsDecodes[i].Valeur[0]);
    end;
  end;
end.
```

## Optimisations avancées

### 1. Pooling de buffers

Pour éviter les allocations mémoire fréquentes :

```pascal
type
  TBufferPool = class
  private
    FBuffersDisponibles: TList;
    FTailleBuffer: Integer;
  public
    constructor Create(TailleBuffer: Integer; NbBuffersInitial: Integer = 10);
    destructor Destroy; override;

    function Obtenir: TBytes;
    procedure Liberer(var Buffer: TBytes);
  end;

constructor TBufferPool.Create(TailleBuffer: Integer; NbBuffersInitial: Integer);  
var
  i: Integer;
  Buffer: TBytes;
begin
  inherited Create;
  FTailleBuffer := TailleBuffer;
  FBuffersDisponibles := TList.Create;

  // Pré-allouer des buffers
  for i := 1 to NbBuffersInitial do
  begin
    SetLength(Buffer, FTailleBuffer);
    FBuffersDisponibles.Add(Pointer(Buffer));
  end;
end;

destructor TBufferPool.Destroy;  
begin
  FBuffersDisponibles.Free;
  inherited Destroy;
end;

function TBufferPool.Obtenir: TBytes;  
begin
  if FBuffersDisponibles.Count > 0 then
  begin
    Result := TBytes(FBuffersDisponibles[0]);
    FBuffersDisponibles.Delete(0);
  end
  else
  begin
    // Allouer un nouveau buffer si le pool est vide
    SetLength(Result, FTailleBuffer);
  end;
end;

procedure TBufferPool.Liberer(var Buffer: TBytes);  
begin
  FBuffersDisponibles.Add(Pointer(Buffer));
  Buffer := nil;
end;
```

### 2. Lecture/écriture directe sans copie

Pour les meilleurs performances, éviter les copies de mémoire :

```pascal
type
  TLecteurBinaireDirect = class
  private
    FDonnees: PByte;
    FTaille: Integer;
    FPosition: Integer;
  public
    constructor Create(Donnees: PByte; Taille: Integer);

    function LireByte: Byte;
    function LireWord: Word;
    function LireLongWord: LongWord;
    procedure Sauter(NbOctets: Integer);

    property Position: Integer read FPosition write FPosition;
    property Taille: Integer read FTaille;
  end;

constructor TLecteurBinaireDirect.Create(Donnees: PByte; Taille: Integer);  
begin
  inherited Create;
  FDonnees := Donnees;
  FTaille := Taille;
  FPosition := 0;
end;

function TLecteurBinaireDirect.LireByte: Byte;  
begin
  if FPosition >= FTaille then
    raise Exception.Create('Lecture au-delà de la fin des données');

  Result := FDonnees[FPosition];
  Inc(FPosition);
end;

function TLecteurBinaireDirect.LireWord: Word;  
var
  P: PWord;
begin
  if FPosition + 2 > FTaille then
    raise Exception.Create('Lecture au-delà de la fin des données');

  P := PWord(FDonnees + FPosition);
  Result := ntohs(P^);
  Inc(FPosition, 2);
end;

function TLecteurBinaireDirect.LireLongWord: LongWord;  
var
  P: PLongWord;
begin
  if FPosition + 4 > FTaille then
    raise Exception.Create('Lecture au-delà de la fin des données');

  P := PLongWord(FDonnees + FPosition);
  Result := ntohl(P^);
  Inc(FPosition, 4);
end;

procedure TLecteurBinaireDirect.Sauter(NbOctets: Integer);  
begin
  Inc(FPosition, NbOctets);
  if FPosition > FTaille then
    FPosition := FTaille;
end;
```

### 3. Alignement mémoire

Pour de meilleures performances CPU, aligner les structures :

```pascal
type
  // Structure non alignée (taille: 13 octets)
  TMessageNonAligne = packed record
    Type1: Byte;
    Valeur: LongWord;
    X, Y: Single;
  end;

  // Structure alignée (taille: 16 octets, mais accès plus rapide)
  TMessageAligne = record
    Type1: Byte;
    Padding: array[0..2] of Byte;  // Padding pour alignement
    Valeur: LongWord;
    X, Y: Single;
  end;
```

**Note :** Utilisez `packed` pour les protocoles réseau (économie d'espace), mais pas pour les structures en mémoire (performance).

## Gestion des versions de protocole

### Stratégie de versionnement

```pascal
const
  VERSION_PROTOCOLE_ACTUELLE = $0102;  // Version 1.2

type
  TEnTeteMessage = packed record
    Signature: array[0..3] of Char;  // 'MESG'
    Version: Word;                    // Version du protocole
    TypeMessage: Byte;
    Longueur: LongWord;
    // ... autres champs
  end;

function VerifierCompatibilite(VersionRecue: Word): Boolean;  
var
  MajeurRecue, MineurRecue: Byte;
  MajeurActuelle, MineurActuelle: Byte;
begin
  MajeurRecue := (VersionRecue shr 8) and $FF;
  MineurRecue := VersionRecue and $FF;

  MajeurActuelle := (VERSION_PROTOCOLE_ACTUELLE shr 8) and $FF;
  MineurActuelle := VERSION_PROTOCOLE_ACTUELLE and $FF;

  // Compatible si même version majeure
  Result := MajeurRecue = MajeurActuelle;

  if Result and (MineurRecue < MineurActuelle) then
    WriteLn('Attention : version mineure ancienne, certaines fonctionnalités ',
            'peuvent ne pas être disponibles');
end;
```

### Migration entre versions

```pascal
function LireMessageV1(const Octets: TBytes): TMessage;  
begin
  // Lecture format version 1
  Result.X := ReadSingle(Octets, 0);
  Result.Y := ReadSingle(Octets, 4);
  Result.Z := 0;  // Pas de Z en v1
end;

function LireMessageV2(const Octets: TBytes): TMessage;  
begin
  // Lecture format version 2
  Result.X := ReadSingle(Octets, 0);
  Result.Y := ReadSingle(Octets, 4);
  Result.Z := ReadSingle(Octets, 8);  // Z ajouté en v2
end;

function LireMessage(Version: Word; const Octets: TBytes): TMessage;  
begin
  case Version of
    $0100: Result := LireMessageV1(Octets);
    $0200: Result := LireMessageV2(Octets);
    else
      raise Exception.Create('Version de protocole non supportée');
  end;
end;
```

## Debugging et outils de diagnostic

### Dump hexadécimal

```pascal
procedure AfficherHexDump(const Donnees: TBytes);  
var
  i, j: Integer;
  Ligne: String;
  Ascii: String;
begin
  WriteLn('Hex Dump (',Length(Donnees), ' octets):');
  WriteLn('Offset    00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F  ASCII');
  WriteLn(StringOfChar('-', 78));

  i := 0;
  while i < Length(Donnees) do
  begin
    // Offset
    Ligne := IntToHex(i, 8) + '  ';
    Ascii := '';

    // Octets hexadécimaux
    for j := 0 to 15 do
    begin
      if i + j < Length(Donnees) then
      begin
        Ligne := Ligne + IntToHex(Donnees[i + j], 2) + ' ';

        // ASCII
        if (Donnees[i + j] >= 32) and (Donnees[i + j] <= 126) then
          Ascii := Ascii + Chr(Donnees[i + j])
        else
          Ascii := Ascii + '.';
      end
      else
      begin
        Ligne := Ligne + '   ';
        Ascii := Ascii + ' ';
      end;

      // Séparateur au milieu
      if j = 7 then
        Ligne := Ligne + ' ';
    end;

    WriteLn(Ligne, ' ', Ascii);
    Inc(i, 16);
  end;
  WriteLn;
end;
```

**Exemple de sortie :**
```
Hex Dump (26 octets):  
Offset    00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F  ASCII
------------------------------------------------------------------------------
00000000  10 00 00 30 39 42 C8 00  00 48 43 00 00 C8 42 00  ...09B...HC...B.
00000010  00 34 42 01 64 4F A2     .4B.dO.
```

### Analyseur de protocole

```pascal
type
  TAnalyseurProtocole = class
  public
    class procedure AnalyserMessage(const Donnees: TBytes);
  end;

class procedure TAnalyseurProtocole.AnalyserMessage(const Donnees: TBytes);  
var
  Position: Integer;
begin
  if Length(Donnees) < 4 then
  begin
    WriteLn('Message trop court');
    Exit;
  end;

  Position := 0;
  WriteLn('=== Analyse du message ===');

  // Type
  WriteLn('Type: 0x', IntToHex(Donnees[Position], 2));
  Inc(Position);

  // Taille
  WriteLn('Taille: ', (Donnees[Position] shl 8) or Donnees[Position + 1], ' octets');
  Inc(Position, 2);

  // Données
  WriteLn('Données (', Length(Donnees) - Position - 1, ' octets):');
  AfficherHexDump(Copy(Donnees, Position, Length(Donnees) - Position - 1));

  // Checksum
  WriteLn('Checksum: 0x', IntToHex(Donnees[Length(Donnees) - 1], 2));
end;
```

### Générateur de messages de test

```pascal
type
  TGenerateurTest = class
  public
    class function GenererMessageAleatoire(TypeMsg: Byte; TailleMax: Integer): TBytes;
    class function GenererSequenceTest: TArray<TBytes>;
  end;

class function TGenerateurTest.GenererMessageAleatoire(TypeMsg: Byte;
                                                       TailleMax: Integer): TBytes;
var
  Taille, i: Integer;
begin
  Taille := Random(TailleMax) + 1;
  SetLength(Result, 4 + Taille);

  Result[0] := TypeMsg;
  Result[1] := (Taille shr 8) and $FF;
  Result[2] := Taille and $FF;

  // Données aléatoires
  for i := 0 to Taille - 1 do
    Result[3 + i] := Random(256);

  // Checksum simple
  Result[3 + Taille] := 0;
  for i := 0 to 2 + Taille do
    Result[3 + Taille] := Result[3 + Taille] xor Result[i];
end;

class function TGenerateurTest.GenererSequenceTest: TArray<TBytes>;  
begin
  SetLength(Result, 5);
  Result[0] := GenererMessageAleatoire($01, 10);
  Result[1] := GenererMessageAleatoire($02, 50);
  Result[2] := GenererMessageAleatoire($01, 100);
  Result[3] := GenererMessageAleatoire($04, 5);
  Result[4] := GenererMessageAleatoire($03, 75);
end;
```

## Cas pratiques multi-plateformes

### Protocole IoT pour capteurs

```pascal
type
  TTypeCapteur = (tcTemperature, tcHumidite, tcPression, tcCO2);

  TMessageCapteur = packed record
    Version: Byte;           // Version du protocole
    IDCapteur: LongWord;     // ID unique du capteur
    TypeCapteur: Byte;       // Type de capteur
    Timestamp: Int64;        // Horodatage (ms depuis epoch)
    Valeur: Single;          // Valeur mesurée
    Batterie: Byte;          // Niveau de batterie (0-100)
    CRC16: Word;             // Contrôle d'intégrité
  end;
  // Taille totale : 23 octets

function CreerMessageCapteur(ID: LongWord; TypeCap: TTypeCapteur;
                             Valeur: Single; Batterie: Byte): TBytes;
var
  Msg: TMessageCapteur;
begin
  Msg.Version := 1;
  Msg.IDCapteur := htonl(ID);
  Msg.TypeCapteur := Ord(TypeCap);
  Msg.Timestamp := GetTickCount64;
  Msg.Valeur := Valeur;
  Msg.Batterie := Batterie;
  Msg.CRC16 := CalculerCRC16(Msg, SizeOf(TMessageCapteur) - SizeOf(Word));

  // Convertir en tableau d'octets
  SetLength(Result, SizeOf(TMessageCapteur));
  Move(Msg, Result[0], SizeOf(TMessageCapteur));
end;

function DecoderMessageCapteur(const Octets: TBytes): TMessageCapteur;  
var
  CRCCalcule: Word;
begin
  if Length(Octets) <> SizeOf(TMessageCapteur) then
    raise Exception.Create('Taille de message invalide');

  Move(Octets[0], Result, SizeOf(TMessageCapteur));

  // Vérifier le CRC
  CRCCalcule := CalculerCRC16(Result, SizeOf(TMessageCapteur) - SizeOf(Word));
  if CRCCalcule <> Result.CRC16 then
    raise Exception.Create('CRC invalide - message corrompu');

  // Convertir de network byte order
  Result.IDCapteur := ntohl(Result.IDCapteur);
end;

procedure AfficherDonneesCapteur(const Msg: TMessageCapteur);  
var
  TypeStr: String;
  UniteStr: String;
begin
  WriteLn('=== Données capteur ===');
  WriteLn('ID Capteur: ', Msg.IDCapteur);

  case TTypeCapteur(Msg.TypeCapteur) of
    tcTemperature:
      begin
        TypeStr := 'Température';
        UniteStr := '°C';
      end;
    tcHumidite:
      begin
        TypeStr := 'Humidité';
        UniteStr := '%';
      end;
    tcPression:
      begin
        TypeStr := 'Pression';
        UniteStr := 'hPa';
      end;
    tcCO2:
      begin
        TypeStr := 'CO2';
        UniteStr := 'ppm';
      end;
  end;

  WriteLn('Type: ', TypeStr);
  WriteLn('Valeur: ', Msg.Valeur:0:2, ' ', UniteStr);
  WriteLn('Batterie: ', Msg.Batterie, '%');
  WriteLn('Timestamp: ', DateTimeToStr(UnixToDateTime(Msg.Timestamp div 1000)));
end;
```

**Exemple d'utilisation :**

```pascal
program ExempleIoT;

uses
  ProtocoleIoT, SysUtils, Sockets;

var
  Message: TBytes;
  MsgDecode: TMessageCapteur;
  Socket: TSocket;
begin
  // Créer un message de température
  Message := CreerMessageCapteur(
    12345,           // ID du capteur
    tcTemperature,   // Type
    22.5,            // Température en °C
    87               // Batterie à 87%
  );

  WriteLn('Message créé : ', Length(Message), ' octets');
  AfficherHexDump(Message);

  // Envoyer via UDP (idéal pour IoT)
  {$IFDEF WINDOWS}
  // Configuration Windows
  Socket := fpSocket(AF_INET, SOCK_DGRAM, 0);
  {$ENDIF}
  {$IFDEF UNIX}
  // Configuration Linux/Ubuntu
  Socket := fpSocket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  {$ENDIF}

  // fpSendTo(Socket, @Message[0], Length(Message), 0, @AdresseServeur, SizeOf(AdresseServeur));

  // Simuler la réception et le décodage
  try
    MsgDecode := DecoderMessageCapteur(Message);
    AfficherDonneesCapteur(MsgDecode);
  except
    on E: Exception do
      WriteLn('Erreur: ', E.Message);
  end;
end.
```

### Protocole de synchronisation de fichiers

```pascal
type
  TCommandeSync = (
    csListeFichiers,     // Demander la liste des fichiers
    csEnvoyerFichier,    // Envoyer un fichier
    csRecevoirFichier,   // Recevoir un fichier
    csSupprimerFichier,  // Supprimer un fichier
    csConfirmation       // Confirmation d'opération
  );

  TEnTeteSync = packed record
    Signature: array[0..3] of Char;  // 'SYNC'
    Version: Byte;
    Commande: Byte;
    NumeroSequence: LongWord;
    TailleDonnees: Int64;
    CRC32: LongWord;
  end;

  TInfoFichier = packed record
    NomLongueur: Word;
    // Suivi par le nom du fichier (NomLongueur octets)
    TailleFichier: Int64;
    DateModification: Int64;
    Attributs: LongWord;
    Hash: array[0..31] of Byte;  // SHA-256
  end;

function CreerCommandeListeFichiers(NumSeq: LongWord): TBytes;  
var
  EnTete: TEnTeteSync;
begin
  EnTete.Signature := 'SYNC';
  EnTete.Version := 1;
  EnTete.Commande := Ord(csListeFichiers);
  EnTete.NumeroSequence := htonl(NumSeq);
  EnTete.TailleDonnees := 0;
  EnTete.CRC32 := CalculerCRC32(EnTete, SizeOf(TEnTeteSync) - SizeOf(LongWord));

  SetLength(Result, SizeOf(TEnTeteSync));
  Move(EnTete, Result[0], SizeOf(TEnTeteSync));
end;

function CreerCommandeEnvoyerFichier(NumSeq: LongWord; const NomFichier: String;
                                     const Contenu: TBytes): TBytes;
var
  EnTete: TEnTeteSync;
  Info: TInfoFichier;
  NomOctets: TBytes;
  Position: Integer;
  Hash: TSHA256Digest;
begin
  // Préparer le nom du fichier
  NomOctets := TEncoding.UTF8.GetBytes(NomFichier);

  // Calculer le hash du contenu
  Hash := SHA256Buffer(Contenu[0], Length(Contenu));

  // Préparer l'info du fichier
  Info.NomLongueur := htons(Length(NomOctets));
  Info.TailleFichier := Length(Contenu);
  Info.DateModification := DateTimeToUnix(Now) * 1000;
  Info.Attributs := 0;
  Move(Hash, Info.Hash[0], 32);

  // Calculer la taille totale des données
  EnTete.Signature := 'SYNC';
  EnTete.Version := 1;
  EnTete.Commande := Ord(csEnvoyerFichier);
  EnTete.NumeroSequence := htonl(NumSeq);
  EnTete.TailleDonnees := SizeOf(TInfoFichier) + Length(NomOctets) + Length(Contenu);

  // Allouer le buffer complet
  SetLength(Result, SizeOf(TEnTeteSync) + EnTete.TailleDonnees);
  Position := 0;

  // Copier l'en-tête (CRC sera calculé plus tard)
  Move(EnTete, Result[Position], SizeOf(TEnTeteSync));
  Inc(Position, SizeOf(TEnTeteSync));

  // Copier l'info du fichier
  Move(Info, Result[Position], SizeOf(TInfoFichier));
  Inc(Position, SizeOf(TInfoFichier));

  // Copier le nom du fichier
  Move(NomOctets[0], Result[Position], Length(NomOctets));
  Inc(Position, Length(NomOctets));

  // Copier le contenu du fichier
  Move(Contenu[0], Result[Position], Length(Contenu));

  // Calculer et ajouter le CRC32
  EnTete.CRC32 := CalculerCRC32(Result[0], Length(Result) - SizeOf(LongWord));
  Move(EnTete.CRC32, Result[SizeOf(TEnTeteSync) - SizeOf(LongWord)], SizeOf(LongWord));
end;

procedure TraiterCommandeSync(const Octets: TBytes);  
var
  EnTete: TEnTeteSync;
  CRCCalcule: LongWord;
begin
  if Length(Octets) < SizeOf(TEnTeteSync) then
    raise Exception.Create('Message trop court');

  Move(Octets[0], EnTete, SizeOf(TEnTeteSync));

  // Vérifier la signature
  if EnTete.Signature <> 'SYNC' then
    raise Exception.Create('Signature invalide');

  // Vérifier le CRC
  CRCCalcule := CalculerCRC32(Octets[0], Length(Octets) - SizeOf(LongWord));
  if CRCCalcule <> EnTete.CRC32 then
    raise Exception.Create('CRC invalide');

  // Convertir de network byte order
  EnTete.NumeroSequence := ntohl(EnTete.NumeroSequence);

  // Traiter selon la commande
  case TCommandeSync(EnTete.Commande) of
    csListeFichiers:
      TraiterListeFichiers(EnTete);
    csEnvoyerFichier:
      TraiterEnvoyerFichier(EnTete, Octets);
    csRecevoirFichier:
      TraiterRecevoirFichier(EnTete, Octets);
    csSupprimerFichier:
      TraiterSupprimerFichier(EnTete, Octets);
    csConfirmation:
      TraiterConfirmation(EnTete, Octets);
  end;
end;
```

## Différences Windows / Ubuntu

### Gestion des sockets

**Windows :**
```pascal
{$IFDEF WINDOWS}
uses
  WinSock2;

function InitialiserReseau: Boolean;  
var
  WSAData: TWSAData;
begin
  Result := WSAStartup(MakeWord(2, 2), WSAData) = 0;
end;

procedure TerminerReseau;  
begin
  WSACleanup;
end;
{$ENDIF}
```

**Ubuntu/Linux :**
```pascal
{$IFDEF UNIX}
uses
  Sockets, BaseUnix;

function InitialiserReseau: Boolean;  
begin
  // Pas d'initialisation nécessaire sous Linux
  Result := True;
end;

procedure TerminerReseau;  
begin
  // Pas de nettoyage nécessaire sous Linux
end;
{$ENDIF}
```

### Envoi/réception de données

**Code multi-plateforme :**
```pascal
function EnvoyerDonnees(Socket: TSocket; const Donnees: TBytes): Boolean;  
var
  BytesEnvoyes: Integer;
begin
  {$IFDEF WINDOWS}
  BytesEnvoyes := WinSock2.Send(Socket, Donnees[0], Length(Donnees), 0);
  {$ENDIF}
  {$IFDEF UNIX}
  BytesEnvoyes := fpSend(Socket, @Donnees[0], Length(Donnees), 0);
  {$ENDIF}

  Result := BytesEnvoyes = Length(Donnees);

  if not Result then
    WriteLn('Erreur d''envoi. Envoyés: ', BytesEnvoyes, '/', Length(Donnees));
end;

function RecevoirDonnees(Socket: TSocket; var Buffer: TBytes;
                        TailleMax: Integer): Integer;
begin
  SetLength(Buffer, TailleMax);

  {$IFDEF WINDOWS}
  Result := WinSock2.Recv(Socket, Buffer[0], TailleMax, 0);
  {$ENDIF}
  {$IFDEF UNIX}
  Result := fpRecv(Socket, @Buffer[0], TailleMax, 0);
  {$ENDIF}

  if Result > 0 then
    SetLength(Buffer, Result)
  else
    SetLength(Buffer, 0);
end;
```

### Chemins de fichiers dans les protocoles

```pascal
function NormaliserChemin(const Chemin: String): String;  
begin
  Result := Chemin;

  {$IFDEF WINDOWS}
  // Remplacer / par \
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
  {$ENDIF}

  {$IFDEF UNIX}
  // Remplacer \ par /
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
  {$ENDIF}
end;

function CodifierCheminPourProtocole(const Chemin: String): String;  
begin
  // Dans le protocole, toujours utiliser / (convention Unix)
  Result := StringReplace(Chemin, '\', '/', [rfReplaceAll]);
end;

function DecodifierCheminDepuisProtocole(const Chemin: String): String;  
begin
  {$IFDEF WINDOWS}
  Result := StringReplace(Chemin, '/', '\', [rfReplaceAll]);
  {$ENDIF}
  {$IFDEF UNIX}
  Result := Chemin;
  {$ENDIF}
end;
```

### Endianness et portabilité

```pascal
// Fonctions de conversion multi-plateformes
function SwapEndian16(Value: Word): Word; inline;  
begin
  Result := (Value shr 8) or (Value shl 8);
end;

function SwapEndian32(Value: LongWord): LongWord; inline;  
begin
  Result := (Value shr 24) or
            ((Value and $00FF0000) shr 8) or
            ((Value and $0000FF00) shl 8) or
            (Value shl 24);
end;

function SwapEndian64(Value: Int64): Int64;  
var
  Bytes: array[0..7] of Byte absolute Value;
  ResultBytes: array[0..7] of Byte absolute Result;
begin
  ResultBytes[0] := Bytes[7];
  ResultBytes[1] := Bytes[6];
  ResultBytes[2] := Bytes[5];
  ResultBytes[3] := Bytes[4];
  ResultBytes[4] := Bytes[3];
  ResultBytes[5] := Bytes[2];
  ResultBytes[6] := Bytes[1];
  ResultBytes[7] := Bytes[0];
end;

// Conversion automatique selon la plateforme
function ToNetworkOrder16(Value: Word): Word; inline;  
begin
  {$IFDEF ENDIAN_LITTLE}
  Result := SwapEndian16(Value);
  {$ELSE}
  Result := Value;
  {$ENDIF}
end;

function ToNetworkOrder32(Value: LongWord): LongWord; inline;  
begin
  {$IFDEF ENDIAN_LITTLE}
  Result := SwapEndian32(Value);
  {$ELSE}
  Result := Value;
  {$ENDIF}
end;

function FromNetworkOrder16(Value: Word): Word; inline;  
begin
  {$IFDEF ENDIAN_LITTLE}
  Result := SwapEndian16(Value);
  {$ELSE}
  Result := Value;
  {$ENDIF}
end;

function FromNetworkOrder32(Value: LongWord): LongWord; inline;  
begin
  {$IFDEF ENDIAN_LITTLE}
  Result := SwapEndian32(Value);
  {$ELSE}
  Result := Value;
  {$ENDIF}
end;
```

## Tests et validation

### Framework de tests pour protocoles

```pascal
unit TestsProtocole;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, SysUtils, Classes;

type
  TTestProtocole = class(TTestCase)
  published
    procedure TestEncodageDecodage;
    procedure TestIntegrite;
    procedure TestVersions;
    procedure TestMessagesInvalides;
    procedure TestPerformance;
  end;

implementation

uses
  ProtocoleChat;

procedure TTestProtocole.TestEncodageDecodage;  
var
  MsgOriginal, MsgDecode: TMessageChat;
  Octets: TBytes;
begin
  // Créer un message
  MsgOriginal := TProtocoleChat.CreerMessageTexte('Test 123');

  // Encoder
  Octets := TProtocoleChat.EncoderMessage(MsgOriginal);

  // Décoder
  MsgDecode := TProtocoleChat.DecoderMessage(Octets);

  // Vérifier
  AssertEquals('Type incorrect', MsgOriginal.TypeMessage, MsgDecode.TypeMessage);
  AssertEquals('Taille incorrecte', MsgOriginal.TailleDonnees, MsgDecode.TailleDonnees);
  AssertEquals('Checksum incorrect', MsgOriginal.Checksum, MsgDecode.Checksum);
end;

procedure TTestProtocole.TestIntegrite;  
var
  Msg: TMessageChat;
  Octets: TBytes;
begin
  Msg := TProtocoleChat.CreerMessageTexte('Test intégrité');
  Octets := TProtocoleChat.EncoderMessage(Msg);

  // Corrompre un octet
  Octets[5] := Octets[5] xor $FF;

  // Le décodage doit échouer
  try
    TProtocoleChat.DecoderMessage(Octets);
    Fail('Le message corrompu aurait dû être rejeté');
  except
    on E: Exception do
      AssertTrue('Exception attendue', True);
  end;
end;

procedure TTestProtocole.TestVersions;  
begin
  // Tester différentes versions du protocole
  // À implémenter selon vos besoins
  AssertTrue('Test de versions', True);
end;

procedure TTestProtocole.TestMessagesInvalides;  
var
  OctetsTropCourts: TBytes;
begin
  // Message trop court
  SetLength(OctetsTropCourts, 2);
  try
    TProtocoleChat.DecoderMessage(OctetsTropCourts);
    Fail('Message trop court aurait dû échouer');
  except
    on E: Exception do
      AssertTrue('Exception attendue', True);
  end;
end;

procedure TTestProtocole.TestPerformance;  
var
  i: Integer;
  Debut, Fin: TDateTime;
  Msg: TMessageChat;
  Octets: TBytes;
const
  NB_ITERATIONS = 10000;
begin
  Debut := Now;

  for i := 1 to NB_ITERATIONS do
  begin
    Msg := TProtocoleChat.CreerMessageTexte('Message de test ' + IntToStr(i));
    Octets := TProtocoleChat.EncoderMessage(Msg);
    TProtocoleChat.DecoderMessage(Octets);
  end;

  Fin := Now;

  WriteLn(Format('Performance: %d messages en %d ms',
                [NB_ITERATIONS, MilliSecondsBetween(Fin, Debut)]));
end;

initialization
  RegisterTest(TTestProtocole);

end.
```

### Fuzzing (tests aléatoires)

```pascal
program FuzzingProtocole;

{$mode objfpc}{$H+}

uses
  SysUtils, ProtocoleChat;

procedure FuzzTest(NbIterations: Integer);  
var
  i, j, Taille: Integer;
  Octets: TBytes;
  ExceptionsAttrapees: Integer;
begin
  ExceptionsAttrapees := 0;

  WriteLn('Début du fuzzing avec ', NbIterations, ' itérations...');

  for i := 1 to NbIterations do
  begin
    // Générer une taille aléatoire entre 0 et 1000
    Taille := Random(1001);
    SetLength(Octets, Taille);

    // Remplir avec des données aléatoires
    for j := 0 to Taille - 1 do
      Octets[j] := Random(256);

    // Essayer de décoder
    try
      TProtocoleChat.DecoderMessage(Octets);
    except
      on E: Exception do
      begin
        Inc(ExceptionsAttrapees);
        // Normal, c'est du fuzzing
      end;
    end;

    // Afficher la progression
    if i mod 1000 = 0 then
      WriteLn('Progression: ', i, '/', NbIterations);
  end;

  WriteLn('Fuzzing terminé.');
  WriteLn('Exceptions attrapées: ', ExceptionsAttrapees, '/', NbIterations);
  WriteLn('Taux d''exceptions: ', (ExceptionsAttrapees * 100.0 / NbIterations):0:2, '%');
end;

begin
  Randomize;
  FuzzTest(100000);
end.
```

## Bonnes pratiques

### 1. Toujours valider les entrées

```pascal
function DecoderMessageSecurise(const Octets: TBytes): TMessage;  
begin
  // Vérifier la taille minimale
  if Length(Octets) < TAILLE_MIN_MESSAGE then
    raise EProtocoleException.Create('Message trop court');

  // Vérifier la taille maximale
  if Length(Octets) > TAILLE_MAX_MESSAGE then
    raise EProtocoleException.Create('Message trop grand');

  // Vérifier la signature
  if not VerifierSignature(Octets) then
    raise EProtocoleException.Create('Signature invalide');

  // Vérifier le checksum/CRC
  if not VerifierIntegrite(Octets) then
    raise EProtocoleException.Create('Intégrité compromise');

  // Décoder seulement si toutes les validations passent
  Result := DecoderMessageInterne(Octets);
end;
```

### 2. Logger pour le debugging

```pascal
procedure LoggerMessage(Direction: String; const Octets: TBytes);  
var
  Timestamp: String;
  LogFile: TextFile;
begin
  Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);

  {$I-}
  AssignFile(LogFile, 'protocole.log');
  if FileExists('protocole.log') then
    Append(LogFile)
  else
    Rewrite(LogFile);

  WriteLn(LogFile, Format('[%s] %s - %d octets',
                         [Timestamp, Direction, Length(Octets)]));
  WriteLn(LogFile, '  Hex: ', OctetsVersHex(Octets));

  CloseFile(LogFile);
  {$I+}
end;

// Utilisation
procedure EnvoyerMessage(Socket: TSocket; const Msg: TBytes);  
begin
  LoggerMessage('ENVOI', Msg);
  fpSend(Socket, @Msg[0], Length(Msg), 0);
end;
```

### 3. Gérer les timeouts

```pascal
function RecevoirAvecTimeout(Socket: TSocket; var Buffer: TBytes;
                            TimeoutMs: Integer): Boolean;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
  Resultat: Integer;
begin
  Result := False;

  // Configurer le timeout
  TimeVal.tv_sec := TimeoutMs div 1000;
  TimeVal.tv_usec := (TimeoutMs mod 1000) * 1000;

  // Préparer le set de descripteurs
  {$IFDEF WINDOWS}
  FD_ZERO(FDSet);
  FD_SET(Socket, FDSet);
  Resultat := WinSock2.Select(0, @FDSet, nil, nil, @TimeVal);
  {$ENDIF}
  {$IFDEF UNIX}
  fpFD_ZERO(FDSet);
  fpFD_SET(Socket, FDSet);
  Resultat := fpSelect(Socket + 1, @FDSet, nil, nil, @TimeVal);
  {$ENDIF}

  if Resultat > 0 then
  begin
    // Données disponibles
    Resultat := RecevoirDonnees(Socket, Buffer, 4096);
    Result := Resultat > 0;
  end
  else if Resultat = 0 then
    WriteLn('Timeout atteint')
  else
    WriteLn('Erreur lors du select');
end;
```

### 4. Documenter le protocole

Créez toujours une spécification claire :

```
═══════════════════════════════════════════════════════════════
PROTOCOLE CHAT BINAIRE v1.0
═══════════════════════════════════════════════════════════════

1. STRUCTURE GÉNÉRALE D'UN MESSAGE
───────────────────────────────────────────────────────────────
Offset  Taille  Type     Description
───────────────────────────────────────────────────────────────
0       1       Byte     Type de message (voir section 2)
1       2       Word     Taille des données (big endian)
3       N       Bytes    Données du message
3+N     1       Byte     Checksum XOR

Taille totale: 4 + N octets

2. TYPES DE MESSAGES
───────────────────────────────────────────────────────────────
0x01    MSG_TEXTE           Message texte UTF-8
0x02    MSG_DEMANDE_PSEUDO  Demande du pseudo (pas de données)
0x03    MSG_REPONSE_PSEUDO  Réponse avec pseudo (texte UTF-8)
0x04    MSG_PING            Ping keepalive (pas de données)
0x05    MSG_PONG            Réponse au ping (pas de données)

3. CALCUL DU CHECKSUM
───────────────────────────────────────────────────────────────
Le checksum est le XOR de tous les octets du message (type,  
taille, et données), mais pas du checksum lui-même.

Exemple:
  Type: 0x01
  Taille: 0x00 0x05
  Données: "Hello" (0x48 0x65 0x6C 0x6C 0x6F)

  Checksum = 0x01 XOR 0x00 XOR 0x05 XOR 0x48 XOR 0x65 XOR
             0x6C XOR 0x6C XOR 0x6F
           = 0x24

4. EXEMPLE COMPLET
───────────────────────────────────────────────────────────────
Message: "Hi"

Hex dump:
01 00 02 48 69 2A

Décodage:
  01       : Type MSG_TEXTE
  00 02    : Taille = 2 octets
  48 69    : "Hi" en ASCII
  2A       : Checksum

5. GESTION DES ERREURS
───────────────────────────────────────────────────────────────
- Si checksum invalide : ignorer le message
- Si taille > 1024 : fermer la connexion
- Si type inconnu : ignorer le message

6. COMPATIBILITÉ
───────────────────────────────────────────────────────────────
Windows: OK (testé sur Windows 10/11)  
Linux: OK (testé sur Ubuntu 20.04/22.04)  
macOS: OK (testé sur macOS 12+)
═══════════════════════════════════════════════════════════════
```

## Conclusion

Les protocoles binaires personnalisés offrent des avantages significatifs en termes de performance et d'efficacité, particulièrement pour :

- **Applications temps réel** : Jeux, trading, systèmes industriels
- **IoT et systèmes embarqués** : Bande passante limitée
- **Haute performance** : Minimiser la latence
- **Protocoles propriétaires** : Sécurité par obscurité

### Points clés à retenir

1. **Choisir le bon outil** : Binaire pour la performance, texte (JSON) pour la simplicité
2. **Valider systématiquement** : Checksum, CRC, taille, limites
3. **Documenter précisément** : Spécification détaillée indispensable
4. **Tester exhaustivement** : Tests unitaires, fuzzing, tests de charge
5. **Gérer l'endianness** : To

⏭️ [RPC et communication inter-processus](/10-programmation-reseau-avancee/08-rpc-communication-inter-processus.md)
