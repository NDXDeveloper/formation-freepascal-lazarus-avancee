🔝 Retour au [Sommaire](/SOMMAIRE.md)

# X11 et Wayland avec FreePascal/Lazarus

## Introduction : Comprendre l'affichage graphique sous Linux

Quand vous utilisez Ubuntu ou toute autre distribution Linux avec une interface graphique, vous voyez des fenêtres, des boutons, des menus... Mais comment tout cela fonctionne-t-il réellement ? C'est là qu'interviennent **X11** et **Wayland**, les systèmes qui permettent d'afficher des éléments graphiques sur votre écran.

Imaginez que votre écran est une grande toile blanche et que vos applications sont des artistes. X11 et Wayland sont comme les gestionnaires de cette galerie d'art : ils disent à chaque artiste où dessiner, gèrent les superpositions quand plusieurs veulent dessiner au même endroit, et s'assurent que tout s'affiche correctement.

### Pourquoi est-ce important pour un développeur FreePascal/Lazarus ?

Même si Lazarus cache généralement cette complexité derrière la LCL (Lazarus Component Library), comprendre ces systèmes vous permettra de :
- Résoudre des problèmes d'affichage spécifiques
- Optimiser vos applications graphiques
- Créer des fonctionnalités avancées (transparence, effets visuels)
- Comprendre les différences de comportement entre systèmes

## X11 : Le système historique

### Qu'est-ce que X11 ?

X11 (aussi appelé X Window System ou simplement X) existe depuis 1987. C'est le système d'affichage traditionnel de Linux/Unix. Son architecture est basée sur un modèle client-serveur, ce qui peut surprendre au début.

**Le serveur X** : C'est le programme qui contrôle réellement votre écran, clavier et souris. Il "sert" l'affichage aux applications.

**Les clients X** : Ce sont vos applications (Firefox, votre programme Lazarus, etc.) qui demandent au serveur de dessiner des choses.

Cette séparation permet quelque chose de magique : vous pouvez faire tourner une application sur un ordinateur et l'afficher sur un autre !

### Architecture simplifiée de X11

```
Application (Client X)
         ↓
    Bibliothèque Xlib
         ↓
   Protocole X11
         ↓
    Serveur X
         ↓
  Pilote graphique
         ↓
      Écran
```

### Vérifier votre système X11

Avant de commencer à programmer, vérifions que X11 fonctionne sur votre système :

```pascal
program VerifierX11;

{$mode objfpc}{$H+}

uses
  SysUtils, Process;

var
  Output: string;

procedure VerifierVariable(const NomVar: string);
var
  Valeur: string;
begin
  Valeur := GetEnvironmentVariable(NomVar);
  if Valeur <> '' then
    WriteLn(NomVar, ' = ', Valeur)
  else
    WriteLn(NomVar, ' n''est pas définie');
end;

begin
  WriteLn('=== Vérification de l''environnement X11 ===');
  WriteLn;

  // Variables d'environnement importantes
  VerifierVariable('DISPLAY');
  VerifierVariable('WAYLAND_DISPLAY');
  VerifierVariable('XDG_SESSION_TYPE');

  WriteLn;

  // Vérifier le serveur X
  if FileExists('/usr/bin/X') then
    WriteLn('Serveur X trouvé : /usr/bin/X')
  else if FileExists('/usr/bin/Xorg') then
    WriteLn('Serveur X trouvé : /usr/bin/Xorg')
  else
    WriteLn('Serveur X non trouvé dans les emplacements standards');

  // Information sur la session
  if RunCommand('loginctl', ['show-session', 'self', '-p', 'Type'], Output) then
    WriteLn('Type de session : ', Trim(Output));
end.
```

## Utilisation basique de X11 avec FreePascal

### Première fenêtre X11 native

Normalement, avec Lazarus, vous utilisez la LCL qui s'occupe de tout. Mais voyons comment créer une fenêtre X11 directement pour comprendre le fonctionnement :

```pascal
program FenetreX11Simple;

{$mode objfpc}{$H+}
{$linklib X11}

uses
  BaseUnix, X, Xlib, Xutil;

var
  Display: PDisplay;
  Window: TWindow;
  Screen: cint;
  Event: TXEvent;
  Running: Boolean = True;

begin
  // Connexion au serveur X
  Display := XOpenDisplay(nil);
  if Display = nil then
  begin
    WriteLn('Impossible de se connecter au serveur X');
    Halt(1);
  end;

  Screen := DefaultScreen(Display);

  // Créer une fenêtre simple
  Window := XCreateSimpleWindow(
    Display,
    RootWindow(Display, Screen),
    100, 100,  // Position x, y
    400, 300,  // Largeur, hauteur
    1,         // Bordure
    BlackPixel(Display, Screen),
    WhitePixel(Display, Screen)
  );

  // Indiquer les événements qui nous intéressent
  XSelectInput(Display, Window, ExposureMask or KeyPressMask);

  // Afficher la fenêtre
  XMapWindow(Display, Window);

  WriteLn('Fenêtre X11 créée. Appuyez sur une touche dans la fenêtre pour quitter.');

  // Boucle d'événements
  while Running do
  begin
    XNextEvent(Display, @Event);

    case Event._type of
      Expose: begin
        // La fenêtre doit être redessinée
        // Ici on pourrait dessiner du contenu
      end;

      KeyPress: begin
        WriteLn('Touche appuyée - fermeture');
        Running := False;
      end;
    end;
  end;

  // Nettoyage
  XDestroyWindow(Display, Window);
  XCloseDisplay(Display);
end.
```

### Intégration avec Lazarus et la LCL

En pratique, vous n'écrirez pas de code X11 direct. Lazarus s'en occupe pour vous. Mais il est utile de savoir comment accéder aux fonctionnalités X11 depuis une application Lazarus :

```pascal
program LazarusAvecX11;

{$mode objfpc}{$H+}

uses
  Classes, Forms, Controls, StdCtrls, Graphics,
  {$IFDEF UNIX}
  X, Xlib, XUtil, gdk2x, gtk2,
  {$ENDIF}
  SysUtils;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AfficherInfosX11;
  end;

var
  MainForm: TMainForm;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := 'Informations X11/Wayland';
  Width := 500;
  Height := 400;

  Label1 := TLabel.Create(Self);
  Label1.Parent := Self;
  Label1.Left := 10;
  Label1.Top := 10;
  Label1.Width := 480;
  Label1.Height := 300;
  Label1.WordWrap := True;

  Button1 := TButton.Create(Self);
  Button1.Parent := Self;
  Button1.Caption := 'Obtenir les informations système';
  Button1.Left := 150;
  Button1.Top := 320;
  Button1.Width := 200;
  Button1.OnClick := @Button1Click;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  AfficherInfosX11;
end;

procedure TMainForm.AfficherInfosX11;
{$IFDEF UNIX}
var
  Display: PDisplay;
  InfoText: string;
  ScreenNum: Integer;
{$ENDIF}
begin
  InfoText := '';

  {$IFDEF UNIX}
  // Obtenir le display X11 utilisé par GTK
  Display := gdk_x11_get_default_xdisplay();

  if Display <> nil then
  begin
    InfoText := InfoText + 'Système d''affichage : X11' + LineEnding;
    InfoText := InfoText + 'Serveur : ' + XServerVendor(Display) + LineEnding;
    InfoText := InfoText + 'Version : ' + IntToStr(XVendorRelease(Display)) + LineEnding;

    ScreenNum := DefaultScreen(Display);
    InfoText := InfoText + 'Écran par défaut : ' + IntToStr(ScreenNum) + LineEnding;
    InfoText := InfoText + 'Résolution : ' +
                IntToStr(DisplayWidth(Display, ScreenNum)) + 'x' +
                IntToStr(DisplayHeight(Display, ScreenNum)) + LineEnding;
    InfoText := InfoText + 'Profondeur couleur : ' +
                IntToStr(DefaultDepth(Display, ScreenNum)) + ' bits' + LineEnding;
  end
  else
  begin
    InfoText := 'Impossible d''accéder aux informations X11' + LineEnding;
    InfoText := InfoText + 'Le système utilise peut-être Wayland' + LineEnding;
  end;

  // Vérifier Wayland
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
  begin
    InfoText := InfoText + LineEnding + 'Wayland détecté : ' +
                GetEnvironmentVariable('WAYLAND_DISPLAY') + LineEnding;

    if GetEnvironmentVariable('GDK_BACKEND') = 'x11' then
      InfoText := InfoText + 'Mais GTK utilise X11 (XWayland)' + LineEnding;
  end;
  {$ELSE}
  InfoText := 'Cette fonctionnalité n''est disponible que sous Unix/Linux';
  {$ENDIF}

  Label1.Caption := InfoText;
end;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

## Wayland : Le système moderne

### Qu'est-ce que Wayland ?

Wayland est le successeur moderne de X11, conçu pour être plus simple, plus sûr et plus performant. Contrairement à X11, Wayland n'utilise pas de modèle client-serveur traditionnel. À la place :

- **Le compositeur** gère tout : affichage, entrées, et composition des fenêtres
- **Les clients** communiquent directement avec le compositeur
- Pas de serveur séparé, tout est plus intégré

### Pourquoi Wayland ?

X11 a été conçu dans les années 80, quand les besoins étaient différents. Wayland résout plusieurs problèmes :

1. **Sécurité** : Les applications ne peuvent pas espionner les autres
2. **Performance** : Moins de copies de données, rendu plus direct
3. **Simplicité** : Architecture plus claire et moderne
4. **Effets visuels** : Meilleur support pour la transparence, les animations

### Architecture simplifiée de Wayland

```
Application
     ↓
Bibliothèque Wayland
     ↓
Compositeur Wayland
(GNOME Shell, KDE Kwin, etc.)
     ↓
Pilote graphique (DRM/KMS)
     ↓
   Écran
```

## XWayland : Le pont entre deux mondes

Pendant la transition vers Wayland, nous avons besoin de faire fonctionner les anciennes applications X11. C'est là qu'intervient **XWayland** :

```pascal
program DetecterXWayland;

{$mode objfpc}{$H+}

uses
  SysUtils, Process, Classes;

function EstSousWayland: Boolean;
begin
  Result := GetEnvironmentVariable('WAYLAND_DISPLAY') <> '';
end;

function EstXWayland: Boolean;
var
  Output: string;
begin
  Result := False;

  // Si on est sous Wayland et qu'on a un DISPLAY X11
  if EstSousWayland and (GetEnvironmentVariable('DISPLAY') <> '') then
  begin
    // Vérifier si Xwayland est en cours d'exécution
    if RunCommand('ps', ['aux'], Output) then
      Result := Pos('Xwayland', Output) > 0;
  end;
end;

procedure AfficherInfosSysteme;
var
  SessionType: string;
begin
  WriteLn('=== Configuration du système d''affichage ===');
  WriteLn;

  SessionType := GetEnvironmentVariable('XDG_SESSION_TYPE');
  WriteLn('Type de session : ', SessionType);

  if EstSousWayland then
  begin
    WriteLn('Wayland est actif');
    WriteLn('Display Wayland : ', GetEnvironmentVariable('WAYLAND_DISPLAY'));

    if EstXWayland then
    begin
      WriteLn('XWayland est en cours d''exécution');
      WriteLn('Les applications X11 peuvent fonctionner');
    end;
  end
  else if GetEnvironmentVariable('DISPLAY') <> '' then
  begin
    WriteLn('X11 natif est utilisé');
    WriteLn('Display X11 : ', GetEnvironmentVariable('DISPLAY'));
  end
  else
  begin
    WriteLn('Aucun système d''affichage détecté');
    WriteLn('Êtes-vous en mode console ?');
  end;
end;

begin
  AfficherInfosSysteme;

  WriteLn;
  WriteLn('Conseil pour votre application Lazarus :');

  if EstSousWayland then
  begin
    WriteLn('- Votre système utilise Wayland');
    WriteLn('- Les applications Lazarus/GTK2 utiliseront XWayland');
    WriteLn('- Pour du Wayland natif, utilisez GTK3 ou Qt5');
  end
  else
  begin
    WriteLn('- Votre système utilise X11 natif');
    WriteLn('- Toutes les applications Lazarus fonctionneront normalement');
  end;
end.
```

## Configuration de Lazarus pour X11 et Wayland

### Choisir le bon widgetset

Lazarus peut utiliser différents "widgetsets" (ensembles de widgets) qui déterminent comment votre application s'affiche :

```pascal
program WidgetsetInfo;

{$mode objfpc}{$H+}

uses
  Interfaces, // Important : doit être en premier
  Forms, LCLPlatform, LCLType, LCLVersion;

begin
  Application.Initialize;

  WriteLn('=== Configuration Lazarus ===');
  WriteLn('Version LCL : ', lcl_version);
  WriteLn;

  Write('Widgetset actuel : ');
  case WidgetSet.LCLPlatform of
    lpGtk:   WriteLn('GTK1 (obsolète)');
    lpGtk2:  WriteLn('GTK2 (X11 uniquement, stable)');
    lpGtk3:  WriteLn('GTK3 (X11 et Wayland, moderne)');
    lpQt:    WriteLn('Qt4 (obsolète)');
    lpQt5:   WriteLn('Qt5 (X11 et Wayland, moderne)');
    lpQt6:   WriteLn('Qt6 (X11 et Wayland, très moderne)');
    else     WriteLn('Autre/Inconnu');
  end;

  WriteLn;
  WriteLn('Recommandations :');
  WriteLn('- Pour X11 pur : GTK2 (le plus stable)');
  WriteLn('- Pour Wayland : GTK3 ou Qt5/Qt6');
  WriteLn('- Pour compatibilité maximale : GTK2 avec XWayland');
end.
```

### Compiler pour différents widgetsets

Pour compiler votre application avec différents widgetsets :

```bash
# Pour GTK2 (X11 uniquement)
lazbuild --ws=gtk2 monprojet.lpi

# Pour GTK3 (X11 et Wayland)
lazbuild --ws=gtk3 monprojet.lpi

# Pour Qt5 (X11 et Wayland)
lazbuild --ws=qt5 monprojet.lpi
```

## Gestion de la transparence et des effets

### Transparence sous X11

```pascal
program TransparenceX11;

{$mode objfpc}{$H+}

uses
  Classes, Forms, Graphics, Controls, ExtCtrls,
  {$IFDEF UNIX}
  gdk2, gtk2, glib2,
  {$ENDIF}
  SysUtils;

type
  TTransparentForm = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FOpacity: Byte;
    FDirection: Integer;
    procedure SetWindowOpacity(Value: Byte);
  end;

var
  TransparentForm: TTransparentForm;

procedure TTransparentForm.FormCreate(Sender: TObject);
begin
  Caption := 'Fenêtre avec transparence';
  Width := 400;
  Height := 300;
  Position := poScreenCenter;

  FOpacity := 255;
  FDirection := -5;

  // Timer pour animer la transparence
  Timer1 := TTimer.Create(Self);
  Timer1.Interval := 50;
  Timer1.OnTimer := @Timer1Timer;
  Timer1.Enabled := True;

  Color := clSkyBlue;
end;

procedure TTransparentForm.FormPaint(Sender: TObject);
begin
  Canvas.Font.Size := 20;
  Canvas.TextOut(50, 100, 'Transparence animée !');
end;

procedure TTransparentForm.Timer1Timer(Sender: TObject);
begin
  FOpacity := FOpacity + FDirection;

  if (FOpacity <= 100) or (FOpacity >= 250) then
    FDirection := -FDirection;

  SetWindowOpacity(FOpacity);
end;

procedure TTransparentForm.SetWindowOpacity(Value: Byte);
{$IFDEF UNIX}
var
  Widget: PGtkWidget;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Widget := PGtkWidget(Handle);
  if Widget <> nil then
  begin
    // Utiliser la fonction GTK pour définir l'opacité
    // 1.0 = opaque, 0.0 = transparent
    gtk_window_set_opacity(PGtkWindow(Widget), Value / 255);
  end;
  {$ENDIF}
end;

begin
  Application.Initialize;
  Application.CreateForm(TTransparentForm, TransparentForm);
  Application.Run;
end.
```

## Gestion du multi-écran

X11 et Wayland gèrent tous deux les configurations multi-écrans, mais différemment :

```pascal
program MultiEcran;

{$mode objfpc}{$H+}

uses
  Classes, Forms, Controls, StdCtrls, Graphics,
  {$IFDEF UNIX}
  gdk2, gtk2,
  {$ENDIF}
  SysUtils;

type
  TMultiScreenForm = class(TForm)
    Memo1: TMemo;
    ButtonMove: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonMoveClick(Sender: TObject);
  private
    procedure AfficherInfosEcrans;
  end;

var
  MultiScreenForm: TMultiScreenForm;

procedure TMultiScreenForm.FormCreate(Sender: TObject);
begin
  Caption := 'Informations Multi-écrans';
  Width := 600;
  Height := 400;
  Position := poScreenCenter;

  Memo1 := TMemo.Create(Self);
  Memo1.Parent := Self;
  Memo1.Left := 10;
  Memo1.Top := 10;
  Memo1.Width := 580;
  Memo1.Height := 320;
  Memo1.ScrollBars := ssVertical;

  ButtonMove := TButton.Create(Self);
  ButtonMove.Parent := Self;
  ButtonMove.Caption := 'Déplacer sur l''écran suivant';
  ButtonMove.Left := 200;
  ButtonMove.Top := 340;
  ButtonMove.Width := 200;
  ButtonMove.OnClick := @ButtonMoveClick;

  AfficherInfosEcrans;
end;

procedure TMultiScreenForm.AfficherInfosEcrans;
var
  i: Integer;
  {$IFDEF UNIX}
  Display: PGdkDisplay;
  NumScreens: gint;
  CurrentScreen: PGdkScreen;
  NumMonitors: gint;
  MonitorGeom: TGdkRectangle;
  j: Integer;
  {$ENDIF}
begin
  Memo1.Clear;
  Memo1.Lines.Add('=== Configuration des écrans ===');
  Memo1.Lines.Add('');

  // Informations de base Lazarus
  Memo1.Lines.Add('Nombre d''écrans selon Lazarus : ' + IntToStr(Screen.MonitorCount));
  Memo1.Lines.Add('Résolution de l''écran principal : ' +
                  IntToStr(Screen.Width) + 'x' + IntToStr(Screen.Height));

  for i := 0 to Screen.MonitorCount - 1 do
  begin
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Écran ' + IntToStr(i) + ':');
    Memo1.Lines.Add('  Position : ' +
                    IntToStr(Screen.Monitors[i].Left) + ',' +
                    IntToStr(Screen.Monitors[i].Top));
    Memo1.Lines.Add('  Taille : ' +
                    IntToStr(Screen.Monitors[i].Width) + 'x' +
                    IntToStr(Screen.Monitors[i].Height));
    Memo1.Lines.Add('  Principal : ' + BoolToStr(Screen.Monitors[i].Primary, True));
  end;

  {$IFDEF UNIX}
  // Informations détaillées GDK/GTK
  Memo1.Lines.Add('');
  Memo1.Lines.Add('=== Informations GDK/GTK ===');

  Display := gdk_display_get_default();
  if Display <> nil then
  begin
    NumScreens := gdk_display_get_n_screens(Display);
    Memo1.Lines.Add('Nombre d''écrans GDK : ' + IntToStr(NumScreens));

    for i := 0 to NumScreens - 1 do
    begin
      CurrentScreen := gdk_display_get_screen(Display, i);
      if CurrentScreen <> nil then
      begin
        NumMonitors := gdk_screen_get_n_monitors(CurrentScreen);
        Memo1.Lines.Add('');
        Memo1.Lines.Add('Écran GDK ' + IntToStr(i) + ' : ' +
                       IntToStr(NumMonitors) + ' moniteur(s)');

        for j := 0 to NumMonitors - 1 do
        begin
          gdk_screen_get_monitor_geometry(CurrentScreen, j, @MonitorGeom);
          Memo1.Lines.Add('  Moniteur ' + IntToStr(j) + ': ' +
                         IntToStr(MonitorGeom.width) + 'x' +
                         IntToStr(MonitorGeom.height) +
                         ' à la position (' + IntToStr(MonitorGeom.x) +
                         ',' + IntToStr(MonitorGeom.y) + ')');
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TMultiScreenForm.ButtonMoveClick(Sender: TObject);
var
  CurrentMonitor, NextMonitor: Integer;
  i: Integer;
begin
  // Trouver sur quel moniteur nous sommes
  CurrentMonitor := -1;
  for i := 0 to Screen.MonitorCount - 1 do
  begin
    if (Left >= Screen.Monitors[i].Left) and
       (Left < Screen.Monitors[i].Left + Screen.Monitors[i].Width) and
       (Top >= Screen.Monitors[i].Top) and
       (Top < Screen.Monitors[i].Top + Screen.Monitors[i].Height) then
    begin
      CurrentMonitor := i;
      Break;
    end;
  end;

  if CurrentMonitor >= 0 then
  begin
    NextMonitor := (CurrentMonitor + 1) mod Screen.MonitorCount;

    // Déplacer au centre du prochain écran
    Left := Screen.Monitors[NextMonitor].Left +
            (Screen.Monitors[NextMonitor].Width - Width) div 2;
    Top := Screen.Monitors[NextMonitor].Top +
           (Screen.Monitors[NextMonitor].Height - Height) div 2;

    Memo1.Lines.Add('');
    Memo1.Lines.Add('Déplacé de l''écran ' + IntToStr(CurrentMonitor) +
                   ' vers l''écran ' + IntToStr(NextMonitor));
  end;
end;

begin
  Application.Initialize;
  Application.CreateForm(TMultiScreenForm, MultiScreenForm);
  Application.Run;
end.
```

## Capture d'écran et manipulation

### Capturer l'écran sous X11/Wayland

```pascal
program CaptureEcran;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Graphics, IntfGraphics, GraphType,
  {$IFDEF UNIX}
  gdk2, gdk2pixbuf, gtk2,
  {$ENDIF}
  Forms;

function CaptureDesktop: TBitmap;
var
  {$IFDEF UNIX}
  RootWindow: PGdkWindow;
  Pixbuf: PGdkPixbuf;
  Width, Height: gint;
  RawImage: TRawImage;
  IntfImage: TLazIntfImage;
  {$ENDIF}
begin
  Result := TBitmap.Create;

  {$IFDEF UNIX}
  // Obtenir la fenêtre racine
  RootWindow := gdk_get_default_root_window();

  if RootWindow <> nil then
  begin
    // Obtenir les dimensions
    gdk_window_get_size(RootWindow, @Width, @Height);

    WriteLn('Capture d''écran : ', Width, 'x', Height);

    // Créer un pixbuf depuis la fenêtre
    Pixbuf := gdk_pixbuf_get_from_window(
      RootWindow,
      0, 0,     // Position x, y
      Width, Height
    );

    if Pixbuf <> nil then
    begin
      // Convertir le pixbuf en TBitmap
      Result.Width := Width;
      Result.Height := Height;

      // Créer une interface image
      IntfImage := TLazIntfImage.Create(Width, Height);
      try
        IntfImage.DataDescription := GetDescriptionFromDevice(0, Width, Height);

        // Copier les données du pixbuf
        // Note : Code simplifié, en production il faudrait
        // gérer correctement la conversion des pixels

        Result.LoadFromIntfImage(IntfImage);
      finally
        IntfImage.Free;
      end;

      g_object_unref(Pixbuf);
    end
    else
    begin
      WriteLn('Erreur : Impossible de créer le pixbuf');
      WriteLn('Note : Sous Wayland, la capture d''écran nécessite');
      WriteLn('des permissions spéciales via le portail desktop');
    end;
  end;
  {$ELSE}
  WriteLn('Capture d''écran uniquement disponible sous Unix/Linux');
  {$ENDIF}
end;

var
  Bitmap: TBitmap;

begin
  Application.Initialize;

  WriteLn('Tentative de capture d''écran...');

  Bitmap := CaptureDesktop;
  try
    if (Bitmap.Width > 0) and (Bitmap.Height > 0) then
    begin
      Bitmap.SaveToFile('capture.bmp');
      WriteLn('Capture sauvegardée dans capture.bmp');
    end
    else
    begin
      WriteLn('La capture a échoué');
      WriteLn('');
      WriteLn('Sous Wayland, utilisez plutôt :');
      WriteLn('- L''outil de capture d''écran du système');
      WriteLn('- Le portail xdg-desktop-portal');
      WriteLn('- Des outils comme gnome-screenshot');
    end;
  finally
    Bitmap.Free;
  end;
end.
```

## Gestion du presse-papiers

Le presse-papiers fonctionne différemment sous X11 et Wayland :

```pascal
program PressePapiers;

{$mode objfpc}{$H+}

uses
  Classes, Forms, Controls, StdCtrls, Clipbrd,
  {$IFDEF UNIX}
  gtk2, gdk2,
  {$ENDIF}
  SysUtils;

type
  TClipboardForm = class(TForm)
    MemoContent: TMemo;
    ButtonCopy: TButton;
    ButtonPaste: TButton;
    ButtonMonitor: TButton;
    LabelInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure ButtonPasteClick(Sender: TObject);
    procedure ButtonMonitorClick(Sender: TObject);
  private
    procedure UpdateClipboardInfo;
  end;

var
  ClipboardForm: TClipboardForm;

procedure TClipboardForm.FormCreate(Sender: TObject);
begin
  Caption := 'Gestion du presse-papiers X11/Wayland';
  Width := 600;
  Height := 400;
  Position := poScreenCenter;

  LabelInfo := TLabel.Create(Self);
  LabelInfo.Parent := Self;
  LabelInfo.Left := 10;
  LabelInfo.Top := 10;
  LabelInfo.Width := 580;
  LabelInfo.Caption := 'État du presse-papiers : vide';

  MemoContent := TMemo.Create(Self);
  MemoContent.Parent := Self;
  MemoContent.Left := 10;
  MemoContent.Top := 40;
  MemoContent.Width := 580;
  MemoContent.Height := 250;
  MemoContent.Text := 'Tapez du texte ici pour tester le presse-papiers';

  ButtonCopy := TButton.Create(Self);
  ButtonCopy.Parent := Self;
  ButtonCopy.Left := 10;
  ButtonCopy.Top := 300;
  ButtonCopy.Width := 180;
  ButtonCopy.Caption := 'Copier le texte';
  ButtonCopy.OnClick := @ButtonCopyClick;

  ButtonPaste := TButton.Create(Self);
  ButtonPaste.Parent := Self;
  ButtonPaste.Left := 200;
  ButtonPaste.Top := 300;
  ButtonPaste.Width := 180;
  ButtonPaste.Caption := 'Coller le texte';
  ButtonPaste.OnClick := @ButtonPasteClick;

  ButtonMonitor := TButton.Create(Self);
  ButtonMonitor.Parent := Self;
  ButtonMonitor.Left := 390;
  ButtonMonitor.Top := 300;
  ButtonMonitor.Width := 200;
  ButtonMonitor.Caption := 'Info presse-papiers';
  ButtonMonitor.OnClick := @ButtonMonitorClick;

  UpdateClipboardInfo;
end;

procedure TClipboardForm.ButtonCopyClick(Sender: TObject);
begin
  Clipboard.AsText := MemoContent.Text;
  UpdateClipboardInfo;
  ShowMessage('Texte copié dans le presse-papiers');
end;

procedure TClipboardForm.ButtonPasteClick(Sender: TObject);
begin
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    MemoContent.Text := Clipboard.AsText;
    UpdateClipboardInfo;
  end
  else
    ShowMessage('Le presse-papiers ne contient pas de texte');
end;

procedure TClipboardForm.ButtonMonitorClick(Sender: TObject);
var
  Info: TStringList;
  i: Integer;
begin
  Info := TStringList.Create;
  try
    Info.Add('=== Informations du presse-papiers ===');
    Info.Add('');

    // Vérifier les formats disponibles
    Info.Add('Formats disponibles :');
    if Clipboard.HasFormat(CF_TEXT) then
      Info.Add('  - Texte simple (CF_TEXT)');
    if Clipboard.HasFormat(CF_BITMAP) then
      Info.Add('  - Image bitmap (CF_BITMAP)');
    if Clipboard.HasFormat(CF_PICTURE) then
      Info.Add('  - Image (CF_PICTURE)');

    {$IFDEF UNIX}
    Info.Add('');
    Info.Add('Spécificités Unix/Linux :');
    Info.Add('- Sélection PRIMARY : sélection automatique avec la souris');
    Info.Add('- Sélection CLIPBOARD : Ctrl+C/Ctrl+V traditionnel');
    Info.Add('- Sélection SECONDARY : rarement utilisée');

    // Sous X11, il y a plusieurs sélections
    if GetEnvironmentVariable('WAYLAND_DISPLAY') = '' then
    begin
      Info.Add('');
      Info.Add('Mode X11 détecté :');
      Info.Add('- Le texte sélectionné est automatiquement copiable');
      Info.Add('- Clic du milieu pour coller la sélection');
    end
    else
    begin
      Info.Add('');
      Info.Add('Mode Wayland détecté :');
      Info.Add('- Sécurité renforcée du presse-papiers');
      Info.Add('- Pas d''accès inter-applications sans permission');
    end;
    {$ENDIF}

    ShowMessage(Info.Text);
  finally
    Info.Free;
  end;
end;

procedure TClipboardForm.UpdateClipboardInfo;
var
  Status: string;
begin
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    Status := Format('État : contient du texte (%d caractères)',
                     [Length(Clipboard.AsText)]);
  end
  else if Clipboard.HasFormat(CF_BITMAP) then
    Status := 'État : contient une image'
  else
    Status := 'État : vide ou format non reconnu';

  LabelInfo.Caption := Status;
end;

begin
  Application.Initialize;
  Application.CreateForm(TClipboardForm, ClipboardForm);
  Application.Run;
end.
```

### Différences entre les sélections X11

Sous X11, il existe plusieurs "sélections" (buffers de presse-papiers) :

```pascal
program SelectionsX11;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  X, Xlib, XAtom,
  {$ENDIF}
  BaseUnix, SysUtils;

{$IFDEF UNIX}
procedure DemontrerSelections;
var
  Display: PDisplay;
  PrimaryAtom, ClipboardAtom, SecondaryAtom: TAtom;
  Window: TWindow;
begin
  Display := XOpenDisplay(nil);
  if Display = nil then
  begin
    WriteLn('Impossible de se connecter au serveur X');
    Exit;
  end;

  try
    // Obtenir les atomes pour les différentes sélections
    PrimaryAtom := XInternAtom(Display, 'PRIMARY', False);
    ClipboardAtom := XInternAtom(Display, 'CLIPBOARD', False);
    SecondaryAtom := XInternAtom(Display, 'SECONDARY', False);

    WriteLn('=== Les trois sélections X11 ===');
    WriteLn;

    WriteLn('1. PRIMARY (', PrimaryAtom, ')');
    WriteLn('   - Activée automatiquement lors de la sélection de texte');
    WriteLn('   - Collée avec le clic du milieu de la souris');
    WriteLn('   - Comportement traditionnel Unix');
    WriteLn;

    WriteLn('2. CLIPBOARD (', ClipboardAtom, ')');
    WriteLn('   - Utilisée par Ctrl+C/Ctrl+V');
    WriteLn('   - Comportement moderne similaire à Windows');
    WriteLn('   - Supportée par la plupart des applications');
    WriteLn;

    WriteLn('3. SECONDARY (', SecondaryAtom, ')');
    WriteLn('   - Rarement utilisée');
    WriteLn('   - Réservée pour des usages spéciaux');
    WriteLn;

    // Vérifier qui possède chaque sélection
    WriteLn('=== Propriétaires actuels ===');

    Window := XGetSelectionOwner(Display, PrimaryAtom);
    if Window <> None then
      WriteLn('PRIMARY : Fenêtre ', Window)
    else
      WriteLn('PRIMARY : Aucun propriétaire');

    Window := XGetSelectionOwner(Display, ClipboardAtom);
    if Window <> None then
      WriteLn('CLIPBOARD : Fenêtre ', Window)
    else
      WriteLn('CLIPBOARD : Aucun propriétaire');

    Window := XGetSelectionOwner(Display, SecondaryAtom);
    if Window <> None then
      WriteLn('SECONDARY : Fenêtre ', Window)
    else
      WriteLn('SECONDARY : Aucun propriétaire');

  finally
    XCloseDisplay(Display);
  end;
end;
{$ENDIF}

begin
  {$IFDEF UNIX}
  if GetEnvironmentVariable('DISPLAY') <> '' then
    DemontrerSelections
  else
    WriteLn('X11 n''est pas disponible');
  {$ELSE}
  WriteLn('Ce programme nécessite Unix/Linux avec X11');
  {$ENDIF}
end.
```

## Gestion des événements système

### Surveillance des événements X11/Wayland

```pascal
program EvenementsSysteme;

{$mode objfpc}{$H+}

uses
  Classes, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  {$IFDEF UNIX}
  gtk2, gdk2,
  {$ENDIF}
  SysUtils;

type
  TEventMonitorForm = class(TForm)
    ListEvents: TListBox;
    ButtonClear: TButton;
    CheckBoxMouse: TCheckBox;
    CheckBoxKeyboard: TCheckBox;
    CheckBoxWindow: TCheckBox;
    LabelStats: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  protected
    procedure WndProc(var Message: TLMessage); override;
  private
    FEventCount: Integer;
    procedure LogEvent(const EventName, Details: string);
  end;

var
  EventMonitorForm: TEventMonitorForm;

procedure TEventMonitorForm.FormCreate(Sender: TObject);
begin
  Caption := 'Moniteur d''événements X11/Wayland';
  Width := 700;
  Height := 500;
  Position := poScreenCenter;

  FEventCount := 0;

  // Liste des événements
  ListEvents := TListBox.Create(Self);
  ListEvents.Parent := Self;
  ListEvents.Left := 10;
  ListEvents.Top := 10;
  ListEvents.Width := 500;
  ListEvents.Height := 400;

  // Contrôles
  CheckBoxMouse := TCheckBox.Create(Self);
  CheckBoxMouse.Parent := Self;
  CheckBoxMouse.Left := 520;
  CheckBoxMouse.Top := 10;
  CheckBoxMouse.Caption := 'Événements souris';
  CheckBoxMouse.Checked := True;

  CheckBoxKeyboard := TCheckBox.Create(Self);
  CheckBoxKeyboard.Parent := Self;
  CheckBoxKeyboard.Left := 520;
  CheckBoxKeyboard.Top := 40;
  CheckBoxKeyboard.Caption := 'Événements clavier';
  CheckBoxKeyboard.Checked := True;

  CheckBoxWindow := TCheckBox.Create(Self);
  CheckBoxWindow.Parent := Self;
  CheckBoxWindow.Left := 520;
  CheckBoxWindow.Top := 70;
  CheckBoxWindow.Caption := 'Événements fenêtre';
  CheckBoxWindow.Checked := True;

  ButtonClear := TButton.Create(Self);
  ButtonClear.Parent := Self;
  ButtonClear.Left := 520;
  ButtonClear.Top := 110;
  ButtonClear.Width := 150;
  ButtonClear.Caption := 'Effacer la liste';
  ButtonClear.OnClick := @ButtonClearClick;

  LabelStats := TLabel.Create(Self);
  LabelStats.Parent := Self;
  LabelStats.Left := 520;
  LabelStats.Top := 150;
  LabelStats.Caption := 'Événements : 0';

  // Timer pour mise à jour
  Timer1 := TTimer.Create(Self);
  Timer1.Interval := 100;
  Timer1.OnTimer := @Timer1Timer;
  Timer1.Enabled := True;

  LogEvent('Démarrage', 'Surveillance activée');

  {$IFDEF UNIX}
  LogEvent('Système', 'Backend : ' + GetEnvironmentVariable('GDK_BACKEND'));
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    LogEvent('Système', 'Wayland détecté : ' + GetEnvironmentVariable('WAYLAND_DISPLAY'))
  else if GetEnvironmentVariable('DISPLAY') <> '' then
    LogEvent('Système', 'X11 détecté : ' + GetEnvironmentVariable('DISPLAY'));
  {$ENDIF}
end;

procedure TEventMonitorForm.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
end;

procedure TEventMonitorForm.ButtonClearClick(Sender: TObject);
begin
  ListEvents.Clear;
  FEventCount := 0;
  LogEvent('Action', 'Liste effacée');
end;

procedure TEventMonitorForm.Timer1Timer(Sender: TObject);
begin
  LabelStats.Caption := Format('Événements : %d', [FEventCount]);

  // Limiter la liste pour éviter les problèmes de mémoire
  while ListEvents.Items.Count > 1000 do
    ListEvents.Items.Delete(0);
end;

procedure TEventMonitorForm.WndProc(var Message: TLMessage);
var
  EventName: string;
  ShouldLog: Boolean;
begin
  ShouldLog := False;
  EventName := '';

  case Message.Msg of
    // Événements souris
    LM_LBUTTONDOWN:
      if CheckBoxMouse.Checked then
      begin
        EventName := 'Souris : Clic gauche';
        ShouldLog := True;
      end;

    LM_RBUTTONDOWN:
      if CheckBoxMouse.Checked then
      begin
        EventName := 'Souris : Clic droit';
        ShouldLog := True;
      end;

    LM_MBUTTONDOWN:
      if CheckBoxMouse.Checked then
      begin
        EventName := 'Souris : Clic milieu';
        ShouldLog := True;
      end;

    LM_MOUSEWHEEL:
      if CheckBoxMouse.Checked then
      begin
        EventName := 'Souris : Molette';
        ShouldLog := True;
      end;

    // Événements clavier
    LM_KEYDOWN:
      if CheckBoxKeyboard.Checked then
      begin
        EventName := Format('Clavier : Touche %d', [Message.WParam]);
        ShouldLog := True;
      end;

    // Événements fenêtre
    LM_ACTIVATE:
      if CheckBoxWindow.Checked then
      begin
        EventName := 'Fenêtre : Activation';
        ShouldLog := True;
      end;

    LM_SIZE:
      if CheckBoxWindow.Checked then
      begin
        EventName := Format('Fenêtre : Redimensionnement (%dx%d)',
                           [Message.LParam and $FFFF,
                            Message.LParam shr 16]);
        ShouldLog := True;
      end;

    LM_MOVE:
      if CheckBoxWindow.Checked then
      begin
        EventName := 'Fenêtre : Déplacement';
        ShouldLog := True;
      end;
  end;

  if ShouldLog and (EventName <> '') then
    LogEvent('Événement', EventName);

  inherited WndProc(Message);
end;

procedure TEventMonitorForm.LogEvent(const EventName, Details: string);
var
  TimeStr: string;
begin
  Inc(FEventCount);
  TimeStr := FormatDateTime('hh:nn:ss.zzz', Now);
  ListEvents.Items.Add(Format('[%s] %s: %s', [TimeStr, EventName, Details]));

  // Défiler automatiquement vers le bas
  ListEvents.TopIndex := ListEvents.Items.Count - 1;
end;

begin
  Application.Initialize;
  Application.CreateForm(TEventMonitorForm, EventMonitorForm);
  Application.Run;
end.
```

## Gestion des fenêtres et du compositeur

### Interaction avec le gestionnaire de fenêtres

```pascal
program GestionnaireFenetres;

{$mode objfpc}{$H+}

uses
  Classes, Forms, Controls, StdCtrls,
  {$IFDEF UNIX}
  X, Xlib, XAtom, gtk2, gdk2, gdk2x,
  {$ENDIF}
  SysUtils;

type
  TWindowManagerForm = class(TForm)
    ButtonFullscreen: TButton;
    ButtonAlwaysOnTop: TButton;
    ButtonMinimize: TButton;
    ButtonMaximize: TButton;
    ButtonCenter: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ButtonFullscreenClick(Sender: TObject);
    procedure ButtonAlwaysOnTopClick(Sender: TObject);
    procedure ButtonMinimizeClick(Sender: TObject);
    procedure ButtonMaximizeClick(Sender: TObject);
    procedure ButtonCenterClick(Sender: TObject);
  private
    FIsFullscreen: Boolean;
    FIsAlwaysOnTop: Boolean;
    procedure SetFullscreen(Value: Boolean);
    procedure SetAlwaysOnTop(Value: Boolean);
    procedure LogInfo(const Msg: string);
  end;

var
  WindowManagerForm: TWindowManagerForm;

procedure TWindowManagerForm.FormCreate(Sender: TObject);
begin
  Caption := 'Contrôle du gestionnaire de fenêtres';
  Width := 600;
  Height := 400;
  Position := poScreenCenter;

  ButtonFullscreen := TButton.Create(Self);
  ButtonFullscreen.Parent := Self;
  ButtonFullscreen.Left := 10;
  ButtonFullscreen.Top := 10;
  ButtonFullscreen.Width := 180;
  ButtonFullscreen.Caption := 'Plein écran ON/OFF';
  ButtonFullscreen.OnClick := @ButtonFullscreenClick;

  ButtonAlwaysOnTop := TButton.Create(Self);
  ButtonAlwaysOnTop.Parent := Self;
  ButtonAlwaysOnTop.Left := 200;
  ButtonAlwaysOnTop.Top := 10;
  ButtonAlwaysOnTop.Width := 180;
  ButtonAlwaysOnTop.Caption := 'Toujours visible ON/OFF';
  ButtonAlwaysOnTop.OnClick := @ButtonAlwaysOnTopClick;

  ButtonMaximize := TButton.Create(Self);
  ButtonMaximize.Parent := Self;
  ButtonMaximize.Left := 390;
  ButtonMaximize.Top := 10;
  ButtonMaximize.Width := 180;
  ButtonMaximize.Caption := 'Maximiser';
  ButtonMaximize.OnClick := @ButtonMaximizeClick;

  ButtonMinimize := TButton.Create(Self);
  ButtonMinimize.Parent := Self;
  ButtonMinimize.Left := 10;
  ButtonMinimize.Top := 50;
  ButtonMinimize.Width := 180;
  ButtonMinimize.Caption := 'Minimiser';
  ButtonMinimize.OnClick := @ButtonMinimizeClick;

  ButtonCenter := TButton.Create(Self);
  ButtonCenter.Parent := Self;
  ButtonCenter.Left := 200;
  ButtonCenter.Top := 50;
  ButtonCenter.Width := 180;
  ButtonCenter.Caption := 'Centrer la fenêtre';
  ButtonCenter.OnClick := @ButtonCenterClick;

  Memo1 := TMemo.Create(Self);
  Memo1.Parent := Self;
  Memo1.Left := 10;
  Memo1.Top := 90;
  Memo1.Width := 560;
  Memo1.Height := 290;
  Memo1.ScrollBars := ssVertical;

  LogInfo('Application démarrée');

  {$IFDEF UNIX}
  // Détecter le gestionnaire de fenêtres
  var
    Display: PDisplay;
    Root: TWindow;
    WMCheck: TAtom;
    ActualType: TAtom;
    ActualFormat: cint;
    NItems, BytesAfter: culong;
    Data: PByte;
    WMWindow: TWindow;
    WMName: TAtom;
    Name: PChar;
  begin
    Display := gdk_x11_get_default_xdisplay();
    if Display <> nil then
    begin
      Root := DefaultRootWindow(Display);

      // Vérifier _NET_SUPPORTING_WM_CHECK
      WMCheck := XInternAtom(Display, '_NET_SUPPORTING_WM_CHECK', False);

      if XGetWindowProperty(Display, Root, WMCheck, 0, 1, False,
                           XA_WINDOW, @ActualType, @ActualFormat,
                           @NItems, @BytesAfter, @Data) = Success then
      begin
        if (ActualType = XA_WINDOW) and (NItems = 1) then
        begin
          WMWindow := PWindow(Data)^;

          // Obtenir le nom du gestionnaire
          WMName := XInternAtom(Display, '_NET_WM_NAME', False);
          if XGetWindowProperty(Display, WMWindow, WMName, 0, 256, False,
                               XInternAtom(Display, 'UTF8_STRING', False),
                               @ActualType, @ActualFormat,
                               @NItems, @BytesAfter, @Data) = Success then
          begin
            Name := PChar(Data);
            LogInfo('Gestionnaire de fenêtres : ' + Name);
            XFree(Data);
          end;
        end;

        if Data <> nil then
          XFree(Data);
      end;

      // Vérifier Wayland
      if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
      begin
        LogInfo('Session Wayland détectée');
        if GetEnvironmentVariable('XDG_CURRENT_DESKTOP') <> '' then
          LogInfo('Environnement : ' + GetEnvironmentVariable('XDG_CURRENT_DESKTOP'));
      end
      else
        LogInfo('Session X11 native');
    end;
  end;
  {$ENDIF}
end;

procedure TWindowManagerForm.SetFullscreen(Value: Boolean);
{$IFDEF UNIX}
var
  Widget: PGtkWidget;
{$ENDIF}
begin
  FIsFullscreen := Value;

  {$IFDEF UNIX}
  Widget := PGtkWidget(Handle);
  if Widget <> nil then
  begin
    if FIsFullscreen then
    begin
      gtk_window_fullscreen(PGtkWindow(Widget));
      LogInfo('Mode plein écran activé');
    end
    else
    begin
      gtk_window_unfullscreen(PGtkWindow(Widget));
      LogInfo('Mode plein écran désactivé');
    end;
  end;
  {$ELSE}
  if FIsFullscreen then
    WindowState := wsFullScreen
  else
    WindowState := wsNormal;
  {$ENDIF}
end;

procedure TWindowManagerForm.SetAlwaysOnTop(Value: Boolean);
{$IFDEF UNIX}
var
  Widget: PGtkWidget;
{$ENDIF}
begin
  FIsAlwaysOnTop := Value;

  {$IFDEF UNIX}
  Widget := PGtkWidget(Handle);
  if Widget <> nil then
  begin
    gtk_window_set_keep_above(PGtkWindow(Widget), FIsAlwaysOnTop);
    if FIsAlwaysOnTop then
      LogInfo('Fenêtre toujours au dessus activée')
    else
      LogInfo('Fenêtre toujours au dessus désactivée');
  end;
  {$ELSE}
  FormStyle := fsStayOnTop;
  {$ENDIF}
end;

procedure TWindowManagerForm.ButtonFullscreenClick(Sender: TObject);
begin
  SetFullscreen(not FIsFullscreen);
end;

procedure TWindowManagerForm.ButtonAlwaysOnTopClick(Sender: TObject);
begin
  SetAlwaysOnTop(not FIsAlwaysOnTop);
end;

procedure TWindowManagerForm.ButtonMinimizeClick(Sender: TObject);
begin
  WindowState := wsMinimized;
  LogInfo('Fenêtre minimisée');
end;

procedure TWindowManagerForm.ButtonMaximizeClick(Sender: TObject);
begin
  if WindowState = wsMaximized then
  begin
    WindowState := wsNormal;
    LogInfo('Fenêtre restaurée');
  end
  else
  begin
    WindowState := wsMaximized;
    LogInfo('Fenêtre maximisée');
  end;
end;

procedure TWindowManagerForm.ButtonCenterClick(Sender: TObject);
begin
  Position := poScreenCenter;
  LogInfo(Format('Fenêtre centrée à %d,%d', [Left, Top]));
end;

procedure TWindowManagerForm.LogInfo(const Msg: string);
begin
  Memo1.Lines.Add(FormatDateTime('[hh:nn:ss] ', Now) + Msg);
end;

begin
  Application.Initialize;
  Application.CreateForm(TWindowManagerForm, WindowManagerForm);
  Application.Run;
end.
```

## Optimisation et performances

### Réduction du scintillement et double buffering

```pascal
program OptimisationAffichage;

{$mode objfpc}{$H+}

uses
  Classes, Forms, Controls, Graphics, ExtCtrls,
  {$IFDEF UNIX}
  gtk2,
  {$ENDIF}
  SysUtils;

type
  TOptimizedForm = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FAngle: Double;
    FUseDoubleBuffer: Boolean;
    procedure DrawAnimation(ACanvas: TCanvas);
  end;

var
  OptimizedForm: TOptimizedForm;

procedure TOptimizedForm.FormCreate(Sender: TObject);
begin
  Caption := 'Optimisation X11/Wayland - Double Buffering';
  Width := 800;
  Height := 600;
  Position := poScreenCenter;

  // Activer le double buffering pour éviter le scintillement
  DoubleBuffered := True;
  FUseDoubleBuffer := True;

  {$IFDEF UNIX}
  // Configuration GTK pour optimiser le rendu
  gtk_widget_set_double_buffered(PGtkWidget(Handle), True);
  {$ENDIF}

  FAngle := 0;

  Timer1 := TTimer.Create(Self);
  Timer1.Interval := 20; // 50 FPS
  Timer1.OnTimer := @Timer1Timer;
  Timer1.Enabled := True;
end;

procedure TOptimizedForm.DrawAnimation(ACanvas: TCanvas);
var
  CenterX, CenterY: Integer;
  Radius: Integer;
  X, Y: Integer;
  i: Integer;
begin
  CenterX := Width div 2;
  CenterY := Height div 2;

  // Effacer l'arrière-plan
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(0, 0, Width, Height);

  // Dessiner plusieurs cercles en rotation
  for i := 0 to 5 do
  begin
    Radius := 50 + i * 30;

    X := CenterX + Round(Radius * Cos(FAngle + i * 0.5));
    Y := CenterY + Round(Radius * Sin(FAngle + i * 0.5));

    ACanvas.Brush.Color := RGBToColor(
      128 + Round(127 * Sin(FAngle + i)),
      128 + Round(127 * Cos(FAngle + i * 2)),
      128 + Round(127 * Sin(FAngle + i * 3))
    );

    ACanvas.Ellipse(X - 20, Y - 20, X + 20, Y + 20);
  end;

  // Afficher les informations
  ACanvas.Font.Size := 12;
  ACanvas.TextOut(10, 10, 'Animation optimisée avec double buffering');
  ACanvas.TextOut(10, 30, Format('FPS cible : 50 | Angle : %.1f°',
                                 [FAngle * 180 / Pi]));

  {$IFDEF UNIX}
  if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    ACanvas.TextOut(10, 50, 'Rendu : Wayland (optimisé)')
  else
    ACanvas.TextOut(10, 50, 'Rendu : X11');
  {$ENDIF}
end;

procedure TOptimizedForm.FormPaint(Sender: TObject);
var
  Buffer: TBitmap;
begin
  if FUseDoubleBuffer then
  begin
    // Utiliser un bitmap comme buffer
    Buffer := TBitmap.Create;
    try
      Buffer.Width := Width;
      Buffer.Height := Height;

      // Dessiner dans le buffer
      DrawAnimation(Buffer.Canvas);

      // Copier le buffer sur l'écran d'un coup
      Canvas.Draw(0, 0, Buffer);
    finally
      Buffer.Free;
    end;
  end
  else
  begin
    // Dessin direct (peut scintiller)
    DrawAnimation(Canvas);
  end;
end;

procedure TOptimizedForm.Timer1Timer(Sender: TObject);
begin
  // Mettre à jour l'angle de rotation
  FAngle := FAngle + 0.05;
  if FAngle > 2 * Pi then
    FAngle := FAngle - 2 * Pi;

  // Forcer le redessin
  Invalidate;
end;

begin
  Application.Initialize;
  Application.CreateForm(TOptimizedForm, OptimizedForm);
  Application.Run;
end.
```

## Gestion du High-DPI et mise à l'échelle

L'affichage sur écrans haute résolution est un défi important, surtout avec les différences entre X11 et Wayland :

```pascal
program GestionHighDPI;

{$mode objfpc}{$H+}

uses
  Classes, Forms, Controls, StdCtrls, Graphics,
  {$IFDEF UNIX}
  gtk2, gdk2,
  {$ENDIF}
  SysUtils, Math;

type
  THighDPIForm = class(TForm)
    LabelInfo: TLabel;
    ButtonDetect: TButton;
    ButtonApplyScale: TButton;
    MemoInfo: TMemo;
    ComboScale: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonDetectClick(Sender: TObject);
    procedure ButtonApplyScaleClick(Sender: TObject);
  private
    FBaseFontSize: Integer;
    procedure DetectDPISettings;
    procedure ApplyDPIScale(Scale: Double);
  end;

var
  HighDPIForm: THighDPIForm;

procedure THighDPIForm.FormCreate(Sender: TObject);
begin
  Caption := 'Gestion High-DPI X11/Wayland';
  Position := poScreenCenter;

  // Sauvegarder la taille de police de base
  FBaseFontSize := Font.Size;

  // Activer la mise à l'échelle automatique
  Scaled := True;

  LabelInfo := TLabel.Create(Self);
  LabelInfo.Parent := Self;
  LabelInfo.Left := 10;
  LabelInfo.Top := 10;
  LabelInfo.Caption := 'Configuration DPI du système';
  LabelInfo.Font.Style := [fsBold];

  ButtonDetect := TButton.Create(Self);
  ButtonDetect.Parent := Self;
  ButtonDetect.Left := 10;
  ButtonDetect.Top := 40;
  ButtonDetect.Width := 200;
  ButtonDetect.Height := 30;
  ButtonDetect.Caption := 'Détecter les paramètres DPI';
  ButtonDetect.OnClick := @ButtonDetectClick;

  LabelInfo := TLabel.Create(Self);
  LabelInfo.Parent := Self;
  LabelInfo.Left := 10;
  LabelInfo.Top := 80;
  LabelInfo.Caption := 'Facteur d''échelle manuel :';

  ComboScale := TComboBox.Create(Self);
  ComboScale.Parent := Self;
  ComboScale.Left := 150;
  ComboScale.Top := 78;
  ComboScale.Width := 100;
  ComboScale.Style := csDropDownList;
  ComboScale.Items.Add('100%');
  ComboScale.Items.Add('125%');
  ComboScale.Items.Add('150%');
  ComboScale.Items.Add('175%');
  ComboScale.Items.Add('200%');
  ComboScale.ItemIndex := 0;

  ButtonApplyScale := TButton.Create(Self);
  ButtonApplyScale.Parent := Self;
  ButtonApplyScale.Left := 260;
  ButtonApplyScale.Top := 76;
  ButtonApplyScale.Width := 120;
  ButtonApplyScale.Height := 30;
  ButtonApplyScale.Caption := 'Appliquer';
  ButtonApplyScale.OnClick := @ButtonApplyScaleClick;

  MemoInfo := TMemo.Create(Self);
  MemoInfo.Parent := Self;
  MemoInfo.Left := 10;
  MemoInfo.Top := 120;
  MemoInfo.Width := 600;
  MemoInfo.Height := 400;
  MemoInfo.ScrollBars := ssVertical;
  MemoInfo.ReadOnly := True;

  // Ajuster la taille initiale de la fenêtre selon le DPI
  Width := MulDiv(640, Screen.PixelsPerInch, 96);
  Height := MulDiv(540, Screen.PixelsPerInch, 96);

  // Détecter automatiquement au démarrage
  DetectDPISettings;
end;

procedure THighDPIForm.DetectDPISettings;
var
  Info: TStringList;
  ScaleFactor: Double;
  {$IFDEF UNIX}
  Display: PGdkDisplay;
  Screen: PGdkScreen;
  RealDPI: Double;
  XftDPI: string;
  {$ENDIF}
begin
  Info := TStringList.Create;
  try
    Info.Add('=== Informations DPI et mise à l''échelle ===');
    Info.Add('');

    // Informations Lazarus/LCL
    Info.Add('Paramètres Lazarus :');
    Info.Add(Format('  DPI de l''écran : %d', [Screen.PixelsPerInch]));
    Info.Add(Format('  DPI de la fenêtre : %d', [PixelsPerInch]));
    Info.Add(Format('  Mise à l''échelle activée : %s', [BoolToStr(Scaled, True)]));

    ScaleFactor := Screen.PixelsPerInch / 96;
    Info.Add(Format('  Facteur d''échelle calculé : %.2f', [ScaleFactor]));

    // Résolution de l'écran
    Info.Add('');
    Info.Add('Résolution de l''écran :');
    Info.Add(Format('  Taille : %d x %d pixels', [Screen.Width, Screen.Height]));

    {$IFDEF UNIX}
    // Informations spécifiques GTK/GDK
    Info.Add('');
    Info.Add('Paramètres GTK/GDK :');

    Display := gdk_display_get_default();
    if Display <> nil then
    begin
      Screen := gdk_display_get_default_screen(Display);
      if Screen <> nil then
      begin
        Info.Add(Format('  DPI horizontal : %.1f', [gdk_screen_get_resolution(Screen)]));

        // Taille physique de l'écran
        Info.Add(Format('  Taille physique : %d x %d mm',
                       [gdk_screen_get_width_mm(Screen),
                        gdk_screen_get_height_mm(Screen)]));

        // Calcul du DPI réel
        if gdk_screen_get_width_mm(Screen) > 0 then
        begin
          RealDPI := gdk_screen_get_width(Screen) * 25.4 / gdk_screen_get_width_mm(Screen);
          Info.Add(Format('  DPI calculé depuis la taille physique : %.1f', [RealDPI]));
        end;
      end;
    end;

    // Variables d'environnement
    Info.Add('');
    Info.Add('Variables d''environnement :');

    if GetEnvironmentVariable('GDK_SCALE') <> '' then
      Info.Add('  GDK_SCALE = ' + GetEnvironmentVariable('GDK_SCALE'))
    else
      Info.Add('  GDK_SCALE non définie');

    if GetEnvironmentVariable('GDK_DPI_SCALE') <> '' then
      Info.Add('  GDK_DPI_SCALE = ' + GetEnvironmentVariable('GDK_DPI_SCALE'))
    else
      Info.Add('  GDK_DPI_SCALE non définie');

    if GetEnvironmentVariable('QT_SCALE_FACTOR') <> '' then
      Info.Add('  QT_SCALE_FACTOR = ' + GetEnvironmentVariable('QT_SCALE_FACTOR'))
    else
      Info.Add('  QT_SCALE_FACTOR non définie');

    // Détection Wayland vs X11
    Info.Add('');
    if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    begin
      Info.Add('Mode Wayland :');
      Info.Add('  La mise à l''échelle est gérée par le compositeur');
      Info.Add('  Chaque moniteur peut avoir son propre facteur d''échelle');
    end
    else
    begin
      Info.Add('Mode X11 :');
      Info.Add('  La mise à l''échelle est globale pour tous les écrans');

      if GetEnvironmentVariable('DISPLAY') <> '' then
      begin
        // Essayer de lire les paramètres Xft
        XftDPI := GetEnvironmentVariable('Xft.dpi');
        if XftDPI <> '' then
          Info.Add('  Xft.dpi = ' + XftDPI);
      end;
    end;
    {$ENDIF}

    // Recommandations
    Info.Add('');
    Info.Add('=== Recommandations ===');

    if ScaleFactor < 1.2 then
      Info.Add('Écran standard (96-115 DPI) : pas de mise à l''échelle nécessaire')
    else if ScaleFactor < 1.6 then
      Info.Add('Écran haute densité (115-150 DPI) : mise à l''échelle 125-150% recommandée')
    else
      Info.Add('Écran très haute densité (>150 DPI) : mise à l''échelle 150-200% recommandée');

    Info.Add('');
    Info.Add('Pour une meilleure compatibilité :');
    Info.Add('- Utilisez des unités relatives (pas des pixels fixes)');
    Info.Add('- Testez avec différents facteurs d''échelle');
    Info.Add('- Utilisez des icônes vectorielles ou multiples résolutions');

    MemoInfo.Lines.Assign(Info);
  finally
    Info.Free;
  end;
end;

procedure THighDPIForm.ApplyDPIScale(Scale: Double);
var
  i: Integer;
begin
  // Appliquer le facteur d'échelle à tous les contrôles
  for i := 0 to ControlCount - 1 do
  begin
    Controls[i].Left := Round(Controls[i].Left * Scale);
    Controls[i].Top := Round(Controls[i].Top * Scale);
    Controls[i].Width := Round(Controls[i].Width * Scale);
    Controls[i].Height := Round(Controls[i].Height * Scale);

    if Controls[i] is TControl then
      TControl(Controls[i]).Font.Size := Round(FBaseFontSize * Scale);
  end;

  // Ajuster la taille de la fenêtre
  Width := Round(Width * Scale);
  Height := Round(Height * Scale);

  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add(Format('Échelle appliquée : %.0f%%', [Scale * 100]));
end;

procedure THighDPIForm.ButtonDetectClick(Sender: TObject);
begin
  DetectDPISettings;
end;

procedure THighDPIForm.ButtonApplyScaleClick(Sender: TObject);
var
  Scale: Double;
begin
  case ComboScale.ItemIndex of
    0: Scale := 1.0;    // 100%
    1: Scale := 1.25;   // 125%
    2: Scale := 1.5;    // 150%
    3: Scale := 1.75;   // 175%
    4: Scale := 2.0;    // 200%
    else Scale := 1.0;
  end;

  ApplyDPIScale(Scale);
end;

begin
  Application.Initialize;
  Application.CreateForm(THighDPIForm, HighDPIForm);
  Application.Run;
end.
```

## Intégration avec les environnements de bureau

### Détection et adaptation à l'environnement

```pascal
program IntegrationDesktop;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Process, Forms, Controls, StdCtrls, Graphics,
  {$IFDEF UNIX}
  gtk2, gdk2,
  {$ENDIF}
  FileUtil, LazFileUtils;

type
  TDesktopIntegrationForm = class(TForm)
    MemoInfo: TMemo;
    ButtonDetect: TButton;
    ButtonTheme: TButton;
    ButtonNotify: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonDetectClick(Sender: TObject);
    procedure ButtonThemeClick(Sender: TObject);
    procedure ButtonNotifyClick(Sender: TObject);
  private
    procedure DetectDesktopEnvironment;
    procedure ShowNotification(const Title, Message: string);
    procedure ApplySystemTheme;
  end;

var
  DesktopIntegrationForm: TDesktopIntegrationForm;

procedure TDesktopIntegrationForm.FormCreate(Sender: TObject);
begin
  Caption := 'Intégration avec l''environnement de bureau';
  Width := 700;
  Height := 500;
  Position := poScreenCenter;

  ButtonDetect := TButton.Create(Self);
  ButtonDetect.Parent := Self;
  ButtonDetect.Left := 10;
  ButtonDetect.Top := 10;
  ButtonDetect.Width := 200;
  ButtonDetect.Caption := 'Détecter l''environnement';
  ButtonDetect.OnClick := @ButtonDetectClick;

  ButtonTheme := TButton.Create(Self);
  ButtonTheme.Parent := Self;
  ButtonTheme.Left := 220;
  ButtonTheme.Top := 10;
  ButtonTheme.Width := 200;
  ButtonTheme.Caption := 'Appliquer le thème système';
  ButtonTheme.OnClick := @ButtonThemeClick;

  ButtonNotify := TButton.Create(Self);
  ButtonNotify.Parent := Self;
  ButtonNotify.Left := 430;
  ButtonNotify.Top := 10;
  ButtonNotify.Width := 200;
  ButtonNotify.Caption := 'Envoyer une notification';
  ButtonNotify.OnClick := @ButtonNotifyClick;

  MemoInfo := TMemo.Create(Self);
  MemoInfo.Parent := Self;
  MemoInfo.Left := 10;
  MemoInfo.Top := 50;
  MemoInfo.Width := 680;
  MemoInfo.Height := 440;
  MemoInfo.ScrollBars := ssVertical;
  MemoInfo.Font.Name := 'Monospace';

  DetectDesktopEnvironment;
end;

procedure TDesktopIntegrationForm.DetectDesktopEnvironment;
var
  Info: TStringList;
  DesktopEnv, SessionType: string;
  Output: string;
  {$IFDEF UNIX}
  Settings: PGtkSettings;
  ThemeName: PChar;
  IconTheme: PChar;
  FontName: PChar;
  {$ENDIF}
begin
  Info := TStringList.Create;
  try
    Info.Add('=== Environnement de bureau ===');
    Info.Add('');

    // Variables d'environnement principales
    DesktopEnv := GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
    SessionType := GetEnvironmentVariable('XDG_SESSION_TYPE');

    Info.Add('Environnement détecté : ' + DesktopEnv);
    Info.Add('Type de session : ' + SessionType);
    Info.Add('');

    // Détection spécifique
    if Pos('GNOME', UpperCase(DesktopEnv)) > 0 then
    begin
      Info.Add('GNOME détecté');
      Info.Add('  Shell : ' + GetEnvironmentVariable('GNOME_SHELL_SESSION_MODE'));

      // Version de GNOME
      if RunCommand('gnome-shell', ['--version'], Output) then
        Info.Add('  Version : ' + Trim(Output));

      Info.Add('  Thème : ' + GetEnvironmentVariable('GTK_THEME'));
      Info.Add('  Gestionnaire de fichiers : Nautilus');
      Info.Add('  Terminal : gnome-terminal');
    end
    else if Pos('KDE', UpperCase(DesktopEnv)) > 0 then
    begin
      Info.Add('KDE Plasma détecté');
      Info.Add('  Session : ' + GetEnvironmentVariable('KDE_SESSION_VERSION'));
      Info.Add('  Gestionnaire de fichiers : Dolphin');
      Info.Add('  Terminal : Konsole');
    end
    else if Pos('XFCE', UpperCase(DesktopEnv)) > 0 then
    begin
      Info.Add('XFCE détecté');
      Info.Add('  Gestionnaire de fichiers : Thunar');
      Info.Add('  Terminal : xfce4-terminal');
    end
    else if Pos('MATE', UpperCase(DesktopEnv)) > 0 then
    begin
      Info.Add('MATE détecté');
      Info.Add('  Gestionnaire de fichiers : Caja');
      Info.Add('  Terminal : mate-terminal');
    end
    else if Pos('CINNAMON', UpperCase(DesktopEnv)) > 0 then
    begin
      Info.Add('Cinnamon détecté');
      Info.Add('  Gestionnaire de fichiers : Nemo');
      Info.Add('  Terminal : gnome-terminal');
    end
    else if Pos('UNITY', UpperCase(DesktopEnv)) > 0 then
    begin
      Info.Add('Unity détecté');
      Info.Add('  Version : ' + GetEnvironmentVariable('UNITY_VERSION'));
    end
    else
    begin
      Info.Add('Environnement non reconnu ou minimaliste');
    end;

    // Informations additionnelles
    Info.Add('');
    Info.Add('=== Chemins standards XDG ===');
    Info.Add('CONFIG : ' + GetEnvironmentVariable('XDG_CONFIG_HOME'));
    Info.Add('DATA : ' + GetEnvironmentVariable('XDG_DATA_HOME'));
    Info.Add('CACHE : ' + GetEnvironmentVariable('XDG_CACHE_HOME'));
    Info.Add('RUNTIME : ' + GetEnvironmentVariable('XDG_RUNTIME_DIR'));

    // Serveur d'affichage
    Info.Add('');
    Info.Add('=== Serveur d''affichage ===');
    if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    begin
      Info.Add('Wayland : ' + GetEnvironmentVariable('WAYLAND_DISPLAY'));
      Info.Add('XWayland disponible : ' + GetEnvironmentVariable('DISPLAY'));
    end
    else if GetEnvironmentVariable('DISPLAY') <> '' then
    begin
      Info.Add('X11 : ' + GetEnvironmentVariable('DISPLAY'));
    end;

    // Thème et apparence
    Info.Add('');
    Info.Add('=== Thème et apparence ===');
    Info.Add('Thème GTK : ' + GetEnvironmentVariable('GTK_THEME'));
    Info.Add('Thème d''icônes : ' + GetEnvironmentVariable('ICON_THEME'));
    Info.Add('Thème de curseur : ' + GetEnvironmentVariable('XCURSOR_THEME'));

    {$IFDEF UNIX}
    // Paramètres GTK actuels
    Settings := gtk_settings_get_default();
    if Settings <> nil then
    begin
      g_object_get(Settings,
                    'gtk-theme-name', @ThemeName,
                    'gtk-icon-theme-name', @IconTheme,
                    'gtk-font-name', @FontName,
                    nil);

      Info.Add('');
      Info.Add('Paramètres GTK actuels :');
      if ThemeName <> nil then
        Info.Add('  Thème : ' + ThemeName);
      if IconTheme <> nil then
        Info.Add('  Icônes : ' + IconTheme);
      if FontName <> nil then
        Info.Add('  Police : ' + FontName);
    end;
    {$ENDIF}

    MemoInfo.Lines.Assign(Info);
  finally
    Info.Free;
  end;
end;

procedure TDesktopIntegrationForm.ShowNotification(const Title, Message: string);
var
  Process: TProcess;
  Desktop: string;
begin
  Desktop := UpperCase(GetEnvironmentVariable('XDG_CURRENT_DESKTOP'));

  Process := TProcess.Create(nil);
  try
    // Utiliser notify-send (fonctionne sur la plupart des environnements)
    if FileExists('/usr/bin/notify-send') then
    begin
      Process.Executable := 'notify-send';
      Process.Parameters.Add(Title);
      Process.Parameters.Add(Message);
      Process.Parameters.Add('--icon=dialog-information');
      Process.Parameters.Add('--expire-time=5000');
    end
    // Alternatives spécifiques
    else if Pos('KDE', Desktop) > 0 then
    begin
      Process.Executable := 'kdialog';
      Process.Parameters.Add('--passivepopup');
      Process.Parameters.Add(Message);
      Process.Parameters.Add('5');
      Process.Parameters.Add('--title');
      Process.Parameters.Add(Title);
    end
    else
    begin
      // Fallback : utiliser zenity
      Process.Executable := 'zenity';
      Process.Parameters.Add('--info');
      Process.Parameters.Add('--text=' + Message);
      Process.Parameters.Add('--title=' + Title);
    end;

    Process.Options := [poNoConsole];
    Process.Execute;

    MemoInfo.Lines.Add('');
    MemoInfo.Lines.Add('Notification envoyée : ' + Title);
  finally
    Process.Free;
  end;
end;

procedure TDesktopIntegrationForm.ApplySystemTheme;
{$IFDEF UNIX}
var
  Settings: PGtkSettings;
  DarkMode: Boolean;
  Output: string;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Settings := gtk_settings_get_default();
  if Settings <> nil then
  begin
    // Détecter si le mode sombre est activé
    DarkMode := False;

    // Pour GNOME
    if RunCommand('gsettings', ['get', 'org.gnome.desktop.interface',
                                'gtk-theme'], Output) then
    begin
      DarkMode := Pos('dark', LowerCase(Output)) > 0;
    end;

    // Appliquer les couleurs en fonction
    if DarkMode then
    begin
      Color := RGBToColor(30, 30, 30);
      Font.Color := clWhite;
      MemoInfo.Color := RGBToColor(40, 40, 40);
      MemoInfo.Font.Color := clWhite;

      MemoInfo.Lines.Add('');
      MemoInfo.Lines.Add('Thème sombre appliqué');
    end
    else
    begin
      Color := clBtnFace;
      Font.Color := clWindowText;
      MemoInfo.Color := clWindow;
      MemoInfo.Font.Color := clWindowText;

      MemoInfo.Lines.Add('');
      MemoInfo.Lines.Add('Thème clair appliqué');
    end;
  end;
  {$ENDIF}
end;

procedure TDesktopIntegrationForm.ButtonDetectClick(Sender: TObject);
begin
  DetectDesktopEnvironment;
end;

procedure TDesktopIntegrationForm.ButtonThemeClick(Sender: TObject);
begin
  ApplySystemTheme;
end;

procedure TDesktopIntegrationForm.ButtonNotifyClick(Sender: TObject);
begin
  ShowNotification('FreePascal/Lazarus',
                  'Notification depuis votre application !');
end;

begin
  Application.Initialize;
  Application.CreateForm(TDesktopIntegrationForm, DesktopIntegrationForm);
  Application.Run;
end.
```

## Conseils et bonnes pratiques

### Développement portable X11/Wayland

Voici les principes essentiels pour créer des applications qui fonctionnent bien sur X11 et Wayland :

#### 1. Utilisez l'abstraction de la LCL

La LCL de Lazarus gère automatiquement les différences entre X11 et Wayland. Privilégiez toujours les composants LCL plutôt que les appels directs aux API système.

#### 2. Testez sur les deux systèmes

```bash
# Forcer X11 même sous Wayland
GDK_BACKEND=x11 ./votre_application

# Forcer Wayland (si supporté)
GDK_BACKEND=wayland ./votre_application
```

#### 3. Gérez les différences de sécurité

Wayland est plus restrictif que X11 pour des raisons de sécurité :
- Pas de capture d'écran globale sans permission
- Pas d'espionnage des entrées d'autres applications
- Pas de positionnement absolu des fenêtres

#### 4. Compilation conditionnelle

```pascal
{$IFDEF UNIX}
  {$IFDEF LINUX}
    // Code spécifique Linux
    if GetEnvironmentVariable('WAYLAND_DISPLAY') <> '' then
    begin
      // Code spécifique Wayland
    end
    else
    begin
      // Code spécifique X11
    end;
  {$ENDIF}
{$ENDIF}
```

#### 5. Gestion des ressources

```pascal
// Toujours libérer les ressources graphiques
try
  Bitmap := TBitmap.Create;
  // Utilisation...
finally
  Bitmap.Free;
end;
```

### Résolution des problèmes courants

#### Problème : Application floue sur écran High-DPI

**Solution :**
```pascal
// Dans le fichier .lpr
Application.Scaled := True;

// Ou dans le manifest Linux
[Desktop Entry]
StartupWMClass=votre-application
X-GNOME-SingleWindow=true
```

#### Problème : Fenêtre ne se positionne pas correctement sous Wayland

**Solution :**
Wayland ne permet pas le positionnement absolu. Utilisez les positions relatives :
```pascal
// Au lieu de
Left := 100;
Top := 100;

// Utilisez
Position := poScreenCenter;
// ou
Position := poMainFormCenter;
```

#### Problème : Le presse-papiers ne fonctionne pas

**Solution :**
```pascal
// Vérifiez toujours le format avant utilisation
if Clipboard.HasFormat(CF_TEXT) then
  Text := Clipboard.AsText
else
  ShowMessage('Format non supporté');
```

### Performance et optimisation

1. **Utilisez le double buffering** pour éviter le scintillement
2. **Minimisez les redessins** avec `BeginUpdate`/`EndUpdate`
3. **Cachez les calculs coûteux** dans des bitmaps
4. **Utilisez `InvalidateRect`** plutôt que `Invalidate` pour les mises à jour partielles

## Conclusion

X11 et Wayland représentent deux approches différentes de l'affichage graphique sous Linux. Alors que X11 offre une compatibilité maximale et des fonctionnalités avancées, Wayland apporte sécurité et performance au prix de certaines restrictions.

Avec FreePascal/Lazarus, vous disposez d'outils puissants pour créer des applications qui fonctionnent sur les deux systèmes. La clé est de :

1. **Comprendre les différences** entre les deux systèmes
2. **Utiliser l'abstraction** fournie par la LCL
3. **Tester régulièrement** sur les deux environnements
4. **Respecter les conventions** de chaque système
5. **Anticiper les restrictions** de sécurité de Wayland

En suivant ces principes, vos applications seront prêtes pour le présent (X11) et l'avenir (Wayland) du bureau Linux, tout en conservant la portabilité et la maintenabilité qui font la force de FreePascal/Lazarus.

### Ressources additionnelles

- Documentation Wayland : https://wayland.freedesktop.org/
- Protocole X11 : https://www.x.org/releases/current/doc/
- Wiki Lazarus : https://wiki.lazarus.freepascal.org/
- Forums FreePascal : https://forum.lazarus.freepascal.org/

N'oubliez pas que la communauté FreePascal/Lazarus est active et prête à vous aider dans vos projets !

⏭️ [Politiques SELinux/AppArmor](/07-specificites-linux-ubuntu/12-politiques-selinux-apparmor.md)
