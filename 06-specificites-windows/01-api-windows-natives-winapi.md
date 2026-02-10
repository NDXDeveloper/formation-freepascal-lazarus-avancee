🔝 Retour au [Sommaire](/SOMMAIRE.md)

# API Windows natives (WinAPI) avec FreePascal/Lazarus

## Introduction : Qu'est-ce que l'API Windows ?

L'API Windows (WinAPI) est l'ensemble des fonctions fournies par le système d'exploitation Windows qui permettent aux programmes d'interagir directement avec le système. C'est comme une boîte à outils géante que Windows met à disposition des développeurs pour créer des applications.

Imaginez Windows comme une énorme bibliothèque avec des milliers de services : créer des fenêtres, dessiner à l'écran, gérer des fichiers, communiquer en réseau, etc. L'API Windows est le guichet où vous demandez ces services.

## Pourquoi utiliser l'API Windows avec FreePascal/Lazarus ?

Bien que Lazarus fournisse la LCL (Lazarus Component Library) qui simplifie grandement le développement, il existe des situations où accéder directement à l'API Windows est nécessaire :

- **Fonctionnalités spécifiques à Windows** : Certaines fonctions n'existent que sous Windows
- **Performance optimale** : Accès direct sans couche d'abstraction
- **Contrôle total** : Vous maîtrisez exactement ce qui se passe
- **Intégration système** : Pour des tâches système avancées

## Les unités principales pour WinAPI en FreePascal

FreePascal fournit plusieurs unités pour accéder à l'API Windows :

```pascal
uses
  Windows,    // Unité principale contenant la majorité des fonctions
  Messages,   // Constantes pour les messages Windows
  ShellAPI,   // Fonctions du Shell Windows (Explorer)
  CommCtrl,   // Contrôles communs Windows
  MMSystem;   // Multimédia système
```

### L'unité Windows

C'est l'unité la plus importante. Elle contient :
- Les types de données Windows (HWND, HDC, HANDLE, etc.)
- Les fonctions de gestion des fenêtres
- Les fonctions de dessin (GDI)
- Les fonctions système
- Les fonctions de gestion des fichiers

## Types de données Windows fondamentaux

Avant d'utiliser l'API Windows, il faut comprendre ses types de données spéciaux :

```pascal
type
  HANDLE  = THandle;     // Un handle générique (poignée vers une ressource)
  HWND    = HANDLE;      // Handle vers une fenêtre
  HDC     = HANDLE;      // Handle vers un contexte de dessin
  HICON   = HANDLE;      // Handle vers une icône
  HBITMAP = HANDLE;      // Handle vers une image bitmap
  HMENU   = HANDLE;      // Handle vers un menu

  DWORD   = Cardinal;    // Entier 32 bits non signé
  BOOL    = LongBool;    // Booléen Windows
  LPSTR   = PChar;       // Pointeur vers une chaîne
  LPCSTR  = PChar;       // Pointeur vers une chaîne constante
```

**Concept important : Les Handles**
Un handle est comme un numéro de ticket : Windows vous donne un numéro qui représente une ressource (fenêtre, fichier, etc.). Vous utilisez ce numéro pour demander à Windows d'agir sur cette ressource.

## Première utilisation : Les boîtes de dialogue

Commençons par quelque chose de simple : afficher une boîte de dialogue Windows native.

```pascal
program PremierWinAPI;
uses Windows;

begin
  // Afficher une boîte de message simple
  MessageBox(0, 'Bonjour depuis WinAPI!', 'Mon Programme', MB_OK);

  // Avec une icône d'information
  MessageBox(0, 'Ceci est une information', 'Info', MB_OK or MB_ICONINFORMATION);

  // Avec choix Oui/Non
  if MessageBox(0, 'Voulez-vous continuer?', 'Question',
                MB_YESNO or MB_ICONQUESTION) = IDYES then
    MessageBox(0, 'Vous avez choisi Oui!', 'Résultat', MB_OK);
end.
```

### Comprendre MessageBox

La fonction `MessageBox` a cette signature :
```pascal
function MessageBox(hWnd: HWND; lpText, lpCaption: LPCTSTR; uType: UINT): Integer;
```

- **hWnd** : Handle de la fenêtre parent (0 = pas de parent)
- **lpText** : Le texte du message
- **lpCaption** : Le titre de la boîte
- **uType** : Options combinées avec OR (type de boutons + icône)
- **Retour** : Identifiant du bouton cliqué (IDOK, IDYES, IDNO, etc.)

## Obtenir des informations système

L'API Windows permet d'obtenir de nombreuses informations sur le système :

```pascal
program InfoSysteme;
uses Windows, SysUtils;

var
  NomOrdinateur: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  TailleNom: DWORD;
  NomUtilisateur: array[0..255] of Char;
  TailleUtilisateur: DWORD;
  CheminWindows: array[0..MAX_PATH] of Char;
  Version: OSVERSIONINFO;
begin
  // Nom de l'ordinateur
  TailleNom := MAX_COMPUTERNAME_LENGTH + 1;
  if GetComputerName(NomOrdinateur, TailleNom) then
    WriteLn('Ordinateur : ', NomOrdinateur);

  // Nom de l'utilisateur
  TailleUtilisateur := 256;
  if GetUserName(NomUtilisateur, TailleUtilisateur) then
    WriteLn('Utilisateur : ', NomUtilisateur);

  // Répertoire Windows
  GetWindowsDirectory(CheminWindows, MAX_PATH);
  WriteLn('Répertoire Windows : ', CheminWindows);

  // Version de Windows
  Version.dwOSVersionInfoSize := SizeOf(Version);
  if GetVersionEx(Version) then
  begin
    WriteLn('Version Windows : ', Version.dwMajorVersion, '.',
            Version.dwMinorVersion, ' Build ', Version.dwBuildNumber);
  end;

  ReadLn;
end.
```

## Gestion des fenêtres

L'API Windows permet de manipuler toutes les fenêtres du système :

```pascal
program ManipulerFenetres;
uses Windows, SysUtils;

var
  HandleFenetre: HWND;
  TitreFenetre: array[0..255] of Char;
  NomClasse: array[0..255] of Char;
begin
  // Trouver une fenêtre par son titre
  HandleFenetre := FindWindow(nil, 'Calculatrice');

  if HandleFenetre <> 0 then
  begin
    WriteLn('Fenêtre trouvée!');

    // Obtenir le titre exact
    GetWindowText(HandleFenetre, TitreFenetre, 256);
    WriteLn('Titre : ', TitreFenetre);

    // Obtenir la classe de la fenêtre
    GetClassName(HandleFenetre, NomClasse, 256);
    WriteLn('Classe : ', NomClasse);

    // Manipuler la fenêtre
    ShowWindow(HandleFenetre, SW_MAXIMIZE);  // Maximiser
    Sleep(2000);
    ShowWindow(HandleFenetre, SW_MINIMIZE);  // Minimiser
    Sleep(2000);
    ShowWindow(HandleFenetre, SW_RESTORE);   // Restaurer

    // Déplacer la fenêtre
    SetWindowPos(HandleFenetre, 0, 100, 100, 0, 0,
                 SWP_NOSIZE or SWP_NOZORDER);
  end
  else
    WriteLn('Fenêtre non trouvée');

  ReadLn;
end.
```

### États de fenêtre avec ShowWindow

Les constantes principales pour `ShowWindow` :
- `SW_HIDE` : Cacher la fenêtre
- `SW_SHOW` : Afficher normalement
- `SW_MINIMIZE` : Réduire dans la barre des tâches
- `SW_MAXIMIZE` : Maximiser
- `SW_RESTORE` : Restaurer à la taille normale

## Travailler avec les fichiers et dossiers

L'API Windows offre des fonctions puissantes pour la gestion des fichiers :

```pascal
program GestionFichiers;
uses Windows, SysUtils;

var
  Attributs: DWORD;
  InfoFichier: WIN32_FILE_ATTRIBUTE_DATA;
  TempPath: array[0..MAX_PATH] of Char;
  TailleFichier: Int64;
begin
  // Obtenir le répertoire temporaire
  GetTempPath(MAX_PATH, TempPath);
  WriteLn('Dossier temporaire : ', TempPath);

  // Vérifier si un fichier existe et obtenir ses attributs
  if GetFileAttributesEx('C:\Windows\notepad.exe',
                         GetFileExInfoStandard,
                         @InfoFichier) then
  begin
    // Calculer la taille
    TailleFichier := Int64(InfoFichier.nFileSizeHigh) shl 32 +
                     InfoFichier.nFileSizeLow;
    WriteLn('Taille de notepad.exe : ', TailleFichier, ' octets');

    // Vérifier les attributs
    if (InfoFichier.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN) <> 0 then
      WriteLn('Le fichier est caché')
    else
      WriteLn('Le fichier est visible');

    if (InfoFichier.dwFileAttributes and FILE_ATTRIBUTE_READONLY) <> 0 then
      WriteLn('Le fichier est en lecture seule');
  end;

  // Créer un nouveau dossier
  if CreateDirectory('C:\TestWinAPI', nil) then
    WriteLn('Dossier créé avec succès')
  else
    WriteLn('Erreur lors de la création du dossier');

  ReadLn;
end.
```

## Intégration dans une application Lazarus

Dans une application Lazarus avec formulaire, vous pouvez combiner LCL et WinAPI :

```pascal
unit Unit1;

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Windows;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure ListerFenetres;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

// Fonction callback pour énumérer les fenêtres
function EnumWindowsProc(Handle: HWND; lParam: LPARAM): BOOL; stdcall;
var
  Titre: array[0..255] of Char;
  Memo: TMemo;
begin
  Result := True;  // Continuer l'énumération

  // Obtenir le titre de la fenêtre
  GetWindowText(Handle, Titre, 256);

  // Si la fenêtre a un titre, l'ajouter au Memo
  if Titre <> '' then
  begin
    Memo := TMemo(lParam);
    if IsWindowVisible(Handle) then
      Memo.Lines.Add(Titre);
  end;
end;

procedure TForm1.ListerFenetres;
begin
  Memo1.Clear;
  Memo1.Lines.Add('=== Fenêtres visibles ===');

  // Énumérer toutes les fenêtres de niveau supérieur
  EnumWindows(@EnumWindowsProc, LPARAM(Memo1));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ListerFenetres;
end;

end.
```

## Gestion des processus

L'API Windows permet de gérer les processus en cours d'exécution :

```pascal
program GestionProcessus;
uses Windows, SysUtils;

var
  InfoDemarrage: STARTUPINFO;
  InfoProcessus: PROCESS_INFORMATION;
  CodeSortie: DWORD;
begin
  // Initialiser les structures
  FillChar(InfoDemarrage, SizeOf(InfoDemarrage), 0);
  InfoDemarrage.cb := SizeOf(InfoDemarrage);
  FillChar(InfoProcessus, SizeOf(InfoProcessus), 0);

  // Lancer le Bloc-notes
  if CreateProcess(nil,                    // Nom de l'application
                   'notepad.exe',           // Ligne de commande
                   nil,                     // Sécurité processus
                   nil,                     // Sécurité thread
                   False,                   // Hériter les handles
                   NORMAL_PRIORITY_CLASS,   // Flags de création
                   nil,                     // Environnement
                   nil,                     // Répertoire courant
                   InfoDemarrage,           // Info de démarrage
                   InfoProcessus) then      // Info du processus créé
  begin
    WriteLn('Bloc-notes lancé!');
    WriteLn('PID du processus : ', InfoProcessus.dwProcessId);

    // Attendre que le processus se termine
    WriteLn('En attente de fermeture du Bloc-notes...');
    WaitForSingleObject(InfoProcessus.hProcess, INFINITE);

    // Obtenir le code de sortie
    GetExitCodeProcess(InfoProcessus.hProcess, CodeSortie);
    WriteLn('Le Bloc-notes s''est terminé avec le code : ', CodeSortie);

    // Fermer les handles
    CloseHandle(InfoProcessus.hProcess);
    CloseHandle(InfoProcessus.hThread);
  end
  else
    WriteLn('Erreur lors du lancement du Bloc-notes');

  ReadLn;
end.
```

## Le Registre Windows

L'API Windows permet d'accéder au registre système (base de données de configuration) :

```pascal
program LectureRegistre;
uses Windows, Registry, SysUtils;

var
  Reg: TRegistry;
  ListeValeurs: TStringList;
  Valeur: string;
begin
  Reg := TRegistry.Create;
  try
    // Ouvrir HKEY_CURRENT_USER
    Reg.RootKey := HKEY_CURRENT_USER;

    // Naviguer vers une clé
    if Reg.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Explorer') then
    begin
      // Lire une valeur
      if Reg.ValueExists('ShellState') then
        WriteLn('La valeur ShellState existe');

      // Lister toutes les valeurs
      WriteLn('Valeurs dans cette clé :');
      ListeValeurs := TStringList.Create;
      try
        Reg.GetValueNames(ListeValeurs);
        for Valeur in ListeValeurs do
          WriteLn('  - ', Valeur);
      finally
        ListeValeurs.Free;
      end;

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;

  ReadLn;
end.
```

## Bonnes pratiques avec l'API Windows

### 1. Toujours vérifier les valeurs de retour

```pascal
var
  CodeErreur: DWORD;
begin
  if FonctionAPI(...) = 0 then  // Beaucoup de fonctions retournent 0 en cas d'erreur
  begin
    // Obtenir le code d'erreur
    CodeErreur := GetLastError();
    WriteLn('Erreur : ', SysErrorMessage(CodeErreur));
  end;
end;
```

### 2. Libérer les ressources

```pascal
var
  Handle: HANDLE;
begin
  Handle := CreateFile(...);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    try
      // Utiliser le handle
    finally
      CloseHandle(Handle);  // TOUJOURS fermer les handles
    end;
  end;
end;
```

### 3. Initialiser les structures

```pascal
var
  MaStructure: SOME_STRUCTURE;
begin
  // Toujours initialiser à zéro
  FillChar(MaStructure, SizeOf(MaStructure), 0);

  // Beaucoup de structures Windows nécessitent leur taille
  MaStructure.cbSize := SizeOf(MaStructure);
end;
```

### 4. Utiliser les bonnes conventions d'appel

```pascal
// Pour les callbacks Windows, utiliser stdcall
function MonCallback(param: Integer): BOOL; stdcall;
begin
  Result := True;
end;
```

## Ressources pour approfondir

### Documentation officielle
- **MSDN (Microsoft Developer Network)** : La référence complète de toutes les fonctions WinAPI
- Cherchez "nom_de_la_fonction MSDN" sur Internet pour trouver la documentation

### Unités FreePascal utiles
- `Windows` : Fonctions principales
- `Messages` : Messages Windows (WM_*)
- `ShellAPI` : Intégration avec l'explorateur
- `Registry` : Accès simplifié au registre
- `ComObj` : Pour COM/ActiveX

### Conseils pour débuter
1. **Commencez simple** : MessageBox, informations système
2. **Progressez graduellement** : Fenêtres, puis fichiers, puis processus
3. **Utilisez le débogueur** : Pour comprendre les valeurs retournées
4. **Consultez MSDN** : Pour chaque fonction que vous utilisez
5. **Testez sur différentes versions** : Windows 10/11 peuvent avoir des différences

## Conclusion

L'API Windows est vaste et puissante. Ce tutoriel n'a couvert que les bases, mais vous avez maintenant les fondements pour :
- Comprendre la documentation Windows
- Utiliser les fonctions système de base
- Intégrer des fonctionnalités Windows natives dans vos applications Lazarus
- Explorer par vous-même d'autres aspects de l'API

N'oubliez pas que l'API Windows est spécifique à Windows. Si vous visez la portabilité vers Linux/macOS, préférez les composants LCL standard de Lazarus qui s'adaptent automatiquement à chaque système d'exploitation.

L'avantage de FreePascal/Lazarus est de vous offrir le choix : utiliser l'abstraction LCL pour la portabilité, ou plonger dans l'API native pour des besoins spécifiques Windows.

⏭️ [Services Windows](/06-specificites-windows/02-services-windows.md)
