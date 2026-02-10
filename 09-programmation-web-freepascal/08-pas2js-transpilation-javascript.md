🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.8 Pas2JS - Transpilation vers JavaScript

## Introduction

Pas2JS est un transpileur qui convertit du code Object Pascal en JavaScript. Il permet aux développeurs FreePascal/Delphi d'utiliser leurs compétences en Pascal pour créer des applications web frontend modernes qui s'exécutent dans le navigateur.

**Qu'est-ce qu'un transpileur ?**

Un transpileur (ou transcompilateur) est un outil qui traduit du code source d'un langage vers un autre langage de même niveau d'abstraction. Dans ce cas, Pas2JS traduit du Pascal vers du JavaScript.

```
┌──────────────┐                    ┌──────────────┐
│              │                    │              │
│  Code Pascal │  ──► Pas2JS ──►   │  JavaScript  │
│   (.pas)     │                    │    (.js)     │
│              │                    │              │
└──────────────┘                    └──────────────┘
                                           │
                                           ▼
                                    ┌──────────────┐
                                    │  Navigateur  │
                                    │    Web       │
                                    └──────────────┘
```

## 9.8.1 Installation et configuration

### Installation de Pas2JS

**Windows :**

```bash
# Télécharger depuis SourceForge
# https://sourceforge.net/projects/pas2js/

# Ou via chocolatey (si disponible)
choco install pas2js

# Vérifier l'installation
pas2js --version
```

**Ubuntu/Linux :**

```bash
# Installer via les packages
sudo apt-get update  
sudo apt-get install pas2js

# Ou compiler depuis les sources
git clone https://gitlab.com/freepascal.org/fpc/pas2js.git  
cd pas2js  
make  
sudo make install

# Vérifier l'installation
pas2js --version
```

### Configuration dans Lazarus

Lazarus offre un support intégré pour Pas2JS :

1. **Installer le package Pas2JS** :
   - Menu : Paquetage → Installer/Désinstaller des paquets
   - Chercher "pas2jsdsgn" dans la liste
   - Ajouter à droite et reconstruire l'IDE

2. **Créer un nouveau projet Pas2JS** :
   - Fichier → Nouveau → Projet
   - Sélectionner "Application Web Browser" ou "Application Node.js"

## 9.8.2 Premier programme Pas2JS

### Hello World dans le navigateur

Créons notre première application web en Pascal :

**hello.pas :**

```pascal
program HelloWorld;

{$mode objfpc}

uses
  JS, Web;

procedure ShowMessage;  
begin
  window.alert('Hello World depuis Pascal!');
  console.log('Application démarrée');
end;

begin
  // Point d'entrée de l'application
  ShowMessage;
end.
```

**Compilation :**

```bash
# Compiler le fichier Pascal vers JavaScript
pas2js -Jirtl.js hello.pas

# Cela génère hello.js
```

**hello.html :**

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Hello World - Pas2JS</title>
    <script src="rtl.js"></script>
    <script src="hello.js"></script>
</head>
<body>
    <h1>Application Pas2JS</h1>
    <p>Ouvrez la console pour voir les messages</p>
</body>
</html>
```

### Structure d'un projet Pas2JS

```
MonProjet/
├── src/
│   ├── main.pas          # Code principal
│   └── units/
│       └── myunit.pas    # Unités personnalisées
├── public/
│   ├── index.html        # Page HTML
│   ├── css/
│   │   └── style.css     # Styles
│   └── assets/
│       └── images/       # Images
└── output/
    ├── rtl.js           # Runtime library
    └── main.js          # JavaScript généré
```

## 9.8.3 Manipulation du DOM

### Accès aux éléments HTML

```pascal
program DOMExample;

{$mode objfpc}

uses
  JS, Web;

var
  MyButton: TJSHTMLElement;
  MyParagraph: TJSHTMLElement;

procedure ButtonClick(Event: TJSMouseEvent);  
begin
  MyParagraph.innerHTML := 'Vous avez cliqué sur le bouton!';
  console.log('Bouton cliqué');
end;

procedure InitializeApp;  
begin
  // Récupérer les éléments par ID
  MyButton := TJSHTMLElement(document.getElementById('myButton'));
  MyParagraph := TJSHTMLElement(document.getElementById('myParagraph'));

  // Ajouter un gestionnaire d'événement
  MyButton.addEventListener('click', @ButtonClick);

  console.log('Application initialisée');
end;

begin
  // Attendre que le DOM soit chargé
  window.addEventListener('load', @InitializeApp);
end.
```

**HTML correspondant :**

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Manipulation DOM - Pas2JS</title>
    <script src="rtl.js"></script>
    <script src="domexample.js"></script>
</head>
<body>
    <h1>Exemple de manipulation du DOM</h1>
    <button id="myButton">Cliquez-moi</button>
    <p id="myParagraph">Texte initial</p>
</body>
</html>
```

### Création dynamique d'éléments

```pascal
program DynamicElements;

{$mode objfpc}

uses
  JS, Web;

procedure CreateList;  
var
  Container: TJSHTMLElement;
  List: TJSHTMLElement;
  ListItem: TJSHTMLElement;
  i: Integer;
begin
  Container := TJSHTMLElement(document.getElementById('container'));

  // Créer une liste
  List := TJSHTMLElement(document.createElement('ul'));
  List.className := 'dynamic-list';

  // Ajouter des éléments à la liste
  for i := 1 to 5 do
  begin
    ListItem := TJSHTMLElement(document.createElement('li'));
    ListItem.innerHTML := 'Élément ' + IntToStr(i);
    List.appendChild(ListItem);
  end;

  // Ajouter la liste au conteneur
  Container.appendChild(List);
end;

procedure AddButton;  
var
  Container: TJSHTMLElement;
  Button: TJSHTMLElement;
begin
  Container := TJSHTMLElement(document.getElementById('container'));

  Button := TJSHTMLElement(document.createElement('button'));
  Button.innerHTML := 'Nouveau bouton';
  Button.className := 'btn btn-primary';

  Button.addEventListener('click',
    procedure(Event: TJSMouseEvent)
    begin
      window.alert('Bouton dynamique cliqué!');
    end
  );

  Container.appendChild(Button);
end;

procedure Initialize;  
begin
  CreateList;
  AddButton;
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

## 9.8.4 Gestion des événements

### Événements souris

```pascal
program MouseEvents;

{$mode objfpc}

uses
  JS, Web;

var
  Box: TJSHTMLElement;
  StatusText: TJSHTMLElement;

procedure OnMouseEnter(Event: TJSMouseEvent);  
begin
  Box.style.setProperty('background-color', 'lightblue');
  StatusText.innerHTML := 'Souris entrée dans la boîte';
end;

procedure OnMouseLeave(Event: TJSMouseEvent);  
begin
  Box.style.setProperty('background-color', 'lightgray');
  StatusText.innerHTML := 'Souris sortie de la boîte';
end;

procedure OnMouseMove(Event: TJSMouseEvent);  
begin
  StatusText.innerHTML := Format('Position: X=%d, Y=%d',
                                 [Event.clientX, Event.clientY]);
end;

procedure OnClick(Event: TJSMouseEvent);  
begin
  window.alert('Clic détecté!');
end;

procedure Initialize;  
begin
  Box := TJSHTMLElement(document.getElementById('box'));
  StatusText := TJSHTMLElement(document.getElementById('status'));

  Box.addEventListener('mouseenter', @OnMouseEnter);
  Box.addEventListener('mouseleave', @OnMouseLeave);
  Box.addEventListener('mousemove', @OnMouseMove);
  Box.addEventListener('click', @OnClick);
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

### Événements clavier

```pascal
program KeyboardEvents;

{$mode objfpc}

uses
  JS, Web;

var
  InputField: TJSHTMLElement;
  Output: TJSHTMLElement;

procedure OnKeyPress(Event: TJSKeyboardEvent);  
begin
  Output.innerHTML := Format('Touche pressée: %s (Code: %d)',
                            [Event.key, Event.keyCode]);
end;

procedure OnKeyDown(Event: TJSKeyboardEvent);  
begin
  // Détecter les touches spéciales
  if Event.keyCode = 13 then // Entrée
  begin
    Event.preventDefault;
    window.alert('Vous avez appuyé sur Entrée');
  end
  else if Event.ctrlKey and (Event.keyCode = 83) then // Ctrl+S
  begin
    Event.preventDefault;
    console.log('Sauvegarde détectée');
  end;
end;

procedure Initialize;  
begin
  InputField := TJSHTMLElement(document.getElementById('inputField'));
  Output := TJSHTMLElement(document.getElementById('output'));

  InputField.addEventListener('keypress', @OnKeyPress);
  InputField.addEventListener('keydown', @OnKeyDown);
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

### Événements de formulaire

```pascal
program FormEvents;

{$mode objfpc}

uses
  JS, Web;

procedure ValidateForm(Event: TJSEvent);  
var
  Form: TJSHTMLFormElement;
  NameInput: TJSHTMLInputElement;
  EmailInput: TJSHTMLInputElement;
  ErrorDiv: TJSHTMLElement;
  ErrorMessage: string;
begin
  Event.preventDefault; // Empêcher la soumission par défaut

  Form := TJSHTMLFormElement(Event.target);
  NameInput := TJSHTMLInputElement(document.getElementById('name'));
  EmailInput := TJSHTMLInputElement(document.getElementById('email'));
  ErrorDiv := TJSHTMLElement(document.getElementById('errors'));

  ErrorMessage := '';

  // Validation du nom
  if Trim(NameInput.value) = '' then
    ErrorMessage := ErrorMessage + 'Le nom est requis.<br>';

  // Validation de l'email
  if Trim(EmailInput.value) = '' then
    ErrorMessage := ErrorMessage + 'L''email est requis.<br>'
  else if Pos('@', EmailInput.value) = 0 then
    ErrorMessage := ErrorMessage + 'Email invalide.<br>';

  // Afficher les erreurs ou soumettre
  if ErrorMessage <> '' then
  begin
    ErrorDiv.innerHTML := ErrorMessage;
    ErrorDiv.style.setProperty('display', 'block');
  end
  else
  begin
    ErrorDiv.style.setProperty('display', 'none');
    window.alert('Formulaire valide! Envoi des données...');
    // Ici, on peut envoyer les données via AJAX
  end;
end;

procedure Initialize;  
var
  Form: TJSHTMLElement;
begin
  Form := TJSHTMLElement(document.getElementById('myForm'));
  Form.addEventListener('submit', @ValidateForm);
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

## 9.8.5 Requêtes AJAX

### XMLHttpRequest

```pascal
program AjaxExample;

{$mode objfpc}

uses
  JS, Web;

procedure HandleResponse(Event: TJSEvent);  
var
  XHR: TJSXMLHttpRequest;
  Response: string;
  OutputDiv: TJSHTMLElement;
begin
  XHR := TJSXMLHttpRequest(Event.target);

  if XHR.readyState = 4 then // DONE
  begin
    if XHR.status = 200 then
    begin
      Response := XHR.responseText;
      OutputDiv := TJSHTMLElement(document.getElementById('output'));
      OutputDiv.innerHTML := '<pre>' + Response + '</pre>';
      console.log('Données reçues avec succès');
    end
    else
    begin
      console.error('Erreur HTTP: ' + IntToStr(XHR.status));
      window.alert('Erreur lors de la requête');
    end;
  end;
end;

procedure LoadData;  
var
  XHR: TJSXMLHttpRequest;
begin
  XHR := TJSXMLHttpRequest.new;

  XHR.addEventListener('readystatechange', @HandleResponse);

  XHR.open('GET', 'https://jsonplaceholder.typicode.com/users/1', True);
  XHR.send;

  console.log('Requête envoyée...');
end;

procedure Initialize;  
var
  LoadButton: TJSHTMLElement;
begin
  LoadButton := TJSHTMLElement(document.getElementById('loadButton'));
  LoadButton.addEventListener('click',
    procedure(Event: TJSMouseEvent)
    begin
      LoadData;
    end
  );
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

### Fetch API (moderne)

```pascal
program FetchExample;

{$mode objfpc}

uses
  JS, Web;

procedure HandleData(Response: TJSPromiseResolver);  
var
  Data: TJSObject;
  OutputDiv: TJSHTMLElement;
  HTML: string;
begin
  Data := TJSObject(Response);
  OutputDiv := TJSHTMLElement(document.getElementById('output'));

  // Construire le HTML à partir des données JSON
  HTML := '<h3>Utilisateur</h3>' +
          '<p>Nom: ' + String(Data['name']) + '</p>' +
          '<p>Email: ' + String(Data['email']) + '</p>' +
          '<p>Téléphone: ' + String(Data['phone']) + '</p>';

  OutputDiv.innerHTML := HTML;
end;

procedure HandleError(Reason: TJSPromiseResolver);  
begin
  console.error('Erreur:', Reason);
  window.alert('Erreur lors du chargement des données');
end;

procedure FetchUserData;  
begin
  window.fetch('https://jsonplaceholder.typicode.com/users/1')
    ._then(
      procedure(Response: TJSResponse)
      begin
        Response.json._then(@HandleData, @HandleError);
      end
    )
    .catch(@HandleError);
end;

procedure Initialize;  
var
  FetchButton: TJSHTMLElement;
begin
  FetchButton := TJSHTMLElement(document.getElementById('fetchButton'));
  FetchButton.addEventListener('click',
    procedure(Event: TJSMouseEvent)
    begin
      FetchUserData;
    end
  );
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

### POST avec JSON

```pascal
program PostExample;

{$mode objfpc}

uses
  JS, Web, SysUtils;

procedure SendData;  
var
  XHR: TJSXMLHttpRequest;
  PostData: TJSObject;
  JSONString: string;
begin
  // Créer l'objet de données
  PostData := TJSObject.new;
  PostData['title'] := 'Mon article';
  PostData['body'] := 'Contenu de l''article';
  PostData['userId'] := 1;

  // Convertir en JSON
  JSONString := TJSJSON.stringify(PostData);

  // Créer la requête
  XHR := TJSXMLHttpRequest.new;

  XHR.addEventListener('load',
    procedure(Event: TJSEvent)
    begin
      if XHR.status = 201 then
      begin
        console.log('Données envoyées avec succès');
        console.log('Réponse:', XHR.responseText);
        window.alert('Article créé!');
      end;
    end
  );

  XHR.open('POST', 'https://jsonplaceholder.typicode.com/posts', True);
  XHR.setRequestHeader('Content-Type', 'application/json');
  XHR.send(JSONString);
end;

procedure Initialize;  
var
  SendButton: TJSHTMLElement;
begin
  SendButton := TJSHTMLElement(document.getElementById('sendButton'));
  SendButton.addEventListener('click',
    procedure(Event: TJSMouseEvent)
    begin
      SendData;
    end
  );
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

## 9.8.6 Stockage local (LocalStorage)

### Sauvegarde et récupération de données

```pascal
program LocalStorageExample;

{$mode objfpc}

uses
  JS, Web;

procedure SaveToStorage;  
var
  NameInput: TJSHTMLInputElement;
  EmailInput: TJSHTMLInputElement;
  UserData: TJSObject;
begin
  NameInput := TJSHTMLInputElement(document.getElementById('name'));
  EmailInput := TJSHTMLInputElement(document.getElementById('email'));

  // Sauvegarder des valeurs simples
  window.localStorage.setItem('userName', NameInput.value);
  window.localStorage.setItem('userEmail', EmailInput.value);

  // Sauvegarder un objet JSON
  UserData := TJSObject.new;
  UserData['name'] := NameInput.value;
  UserData['email'] := EmailInput.value;
  UserData['savedAt'] := DateToStr(Now);

  window.localStorage.setItem('userData', TJSJSON.stringify(UserData));

  window.alert('Données sauvegardées!');
end;

procedure LoadFromStorage;  
var
  NameInput: TJSHTMLInputElement;
  EmailInput: TJSHTMLInputElement;
  UserDataJSON: string;
  UserData: TJSObject;
begin
  NameInput := TJSHTMLInputElement(document.getElementById('name'));
  EmailInput := TJSHTMLInputElement(document.getElementById('email'));

  // Charger des valeurs simples
  NameInput.value := window.localStorage.getItem('userName');
  EmailInput.value := window.localStorage.getItem('userEmail');

  // Charger un objet JSON
  UserDataJSON := window.localStorage.getItem('userData');
  if UserDataJSON <> '' then
  begin
    UserData := TJSObject(TJSJSON.parse(UserDataJSON));
    console.log('Données chargées depuis:', String(UserData['savedAt']));
  end;

  window.alert('Données chargées!');
end;

procedure ClearStorage;  
begin
  window.localStorage.removeItem('userName');
  window.localStorage.removeItem('userEmail');
  window.localStorage.removeItem('userData');

  // Ou tout effacer :
  // window.localStorage.clear;

  window.alert('Stockage vidé!');
end;

procedure Initialize;  
var
  SaveBtn, LoadBtn, ClearBtn: TJSHTMLElement;
begin
  SaveBtn := TJSHTMLElement(document.getElementById('saveBtn'));
  LoadBtn := TJSHTMLElement(document.getElementById('loadBtn'));
  ClearBtn := TJSHTMLElement(document.getElementById('clearBtn'));

  SaveBtn.addEventListener('click',
    procedure(Event: TJSMouseEvent) begin SaveToStorage; end);

  LoadBtn.addEventListener('click',
    procedure(Event: TJSMouseEvent) begin LoadFromStorage; end);

  ClearBtn.addEventListener('click',
    procedure(Event: TJSMouseEvent) begin ClearStorage; end);
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

## 9.8.7 Classes et POO en Pas2JS

### Définition de classes

```pascal
program ClassExample;

{$mode objfpc}

uses
  JS, Web, SysUtils;

type
  { Classe de base pour une tâche }
  TTask = class
  private
    FID: Integer;
    FTitle: string;
    FCompleted: Boolean;
  public
    constructor Create(AID: Integer; const ATitle: string);
    procedure Toggle;
    function ToHTML: string;
    property ID: Integer read FID;
    property Title: string read FTitle write FTitle;
    property Completed: Boolean read FCompleted write FCompleted;
  end;

  { Gestionnaire de liste de tâches }
  TTaskManager = class
  private
    FTasks: array of TTask;
    FContainer: TJSHTMLElement;
    function GetTaskCount: Integer;
  public
    constructor Create(AContainer: TJSHTMLElement);
    destructor Destroy; override;
    procedure AddTask(const Title: string);
    procedure RemoveTask(TaskID: Integer);
    procedure ToggleTask(TaskID: Integer);
    procedure Render;
    property TaskCount: Integer read GetTaskCount;
  end;

{ TTask }

constructor TTask.Create(AID: Integer; const ATitle: string);  
begin
  inherited Create;
  FID := AID;
  FTitle := ATitle;
  FCompleted := False;
end;

procedure TTask.Toggle;  
begin
  FCompleted := not FCompleted;
end;

function TTask.ToHTML: string;  
var
  CheckedAttr: string;
  CompletedClass: string;
begin
  if FCompleted then
  begin
    CheckedAttr := ' checked';
    CompletedClass := ' completed';
  end
  else
  begin
    CheckedAttr := '';
    CompletedClass := '';
  end;

  Result := Format(
    '<div class="task%s" data-id="%d">' +
    '  <input type="checkbox"%s class="task-checkbox"> ' +
    '  <span class="task-title">%s</span> ' +
    '  <button class="delete-btn">✖</button>' +
    '</div>',
    [CompletedClass, FID, CheckedAttr, FTitle]
  );
end;

{ TTaskManager }

constructor TTaskManager.Create(AContainer: TJSHTMLElement);  
begin
  inherited Create;
  FContainer := AContainer;
  SetLength(FTasks, 0);
end;

destructor TTaskManager.Destroy;  
var
  i: Integer;
begin
  for i := 0 to High(FTasks) do
    FTasks[i].Free;
  inherited Destroy;
end;

function TTaskManager.GetTaskCount: Integer;  
begin
  Result := Length(FTasks);
end;

procedure TTaskManager.AddTask(const Title: string);  
var
  NewTask: TTask;
  NewID: Integer;
begin
  if Trim(Title) = '' then
    Exit;

  NewID := TaskCount + 1;
  NewTask := TTask.Create(NewID, Title);

  SetLength(FTasks, TaskCount + 1);
  FTasks[High(FTasks)] := NewTask;

  Render;
end;

procedure TTaskManager.RemoveTask(TaskID: Integer);  
var
  i, j: Integer;
begin
  for i := 0 to High(FTasks) do
  begin
    if FTasks[i].ID = TaskID then
    begin
      FTasks[i].Free;

      // Décaler les éléments
      for j := i to High(FTasks) - 1 do
        FTasks[j] := FTasks[j + 1];

      SetLength(FTasks, TaskCount - 1);
      Render;
      Break;
    end;
  end;
end;

procedure TTaskManager.ToggleTask(TaskID: Integer);  
var
  i: Integer;
begin
  for i := 0 to High(FTasks) do
  begin
    if FTasks[i].ID = TaskID then
    begin
      FTasks[i].Toggle;
      Render;
      Break;
    end;
  end;
end;

procedure TTaskManager.Render;  
var
  HTML: string;
  i: Integer;
  TaskElements: TJSNodeList;
  j: Integer;
begin
  HTML := '';

  for i := 0 to High(FTasks) do
    HTML := HTML + FTasks[i].ToHTML;

  FContainer.innerHTML := HTML;

  // Ajouter les gestionnaires d'événements
  TaskElements := document.querySelectorAll('.task');

  for j := 0 to TaskElements.length - 1 do
  begin
    var TaskDiv := TJSHTMLElement(TaskElements[j]);
    var TaskID := StrToInt(TaskDiv.getAttribute('data-id'));

    // Checkbox
    var Checkbox := TJSHTMLElement(TaskDiv.querySelector('.task-checkbox'));
    Checkbox.addEventListener('change',
      procedure(Event: TJSEvent)
      begin
        ToggleTask(TaskID);
      end
    );

    // Bouton supprimer
    var DeleteBtn := TJSHTMLElement(TaskDiv.querySelector('.delete-btn'));
    DeleteBtn.addEventListener('click',
      procedure(Event: TJSEvent)
      begin
        RemoveTask(TaskID);
      end
    );
  end;
end;

var
  TaskManager: TTaskManager;

procedure Initialize;  
var
  Container: TJSHTMLElement;
  AddButton: TJSHTMLElement;
  TaskInput: TJSHTMLInputElement;
begin
  Container := TJSHTMLElement(document.getElementById('taskContainer'));
  TaskManager := TTaskManager.Create(Container);

  AddButton := TJSHTMLElement(document.getElementById('addTask'));
  TaskInput := TJSHTMLInputElement(document.getElementById('taskInput'));

  AddButton.addEventListener('click',
    procedure(Event: TJSEvent)
    begin
      TaskManager.AddTask(TaskInput.value);
      TaskInput.value := '';
    end
  );

  TaskInput.addEventListener('keypress',
    procedure(Event: TJSKeyboardEvent)
    begin
      if Event.keyCode = 13 then // Entrée
      begin
        TaskManager.AddTask(TaskInput.value);
        TaskInput.value := '';
      end;
    end
  );
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

## 9.8.8 Timers et animations

### SetTimeout et SetInterval

```pascal
program TimerExample;

{$mode objfpc}

uses
  JS, Web, SysUtils;

var
  CountdownElement: TJSHTMLElement;
  TimeLeft: Integer;
  IntervalID: NativeInt;

procedure UpdateCountdown;  
begin
  if TimeLeft > 0 then
  begin
    CountdownElement.innerHTML := Format('Temps restant: %d secondes', [TimeLeft]);
    Dec(TimeLeft);
  end
  else
  begin
    CountdownElement.innerHTML := 'Terminé!';
    window.clearInterval(IntervalID);
    window.alert('Compte à rebours terminé!');
  end;
end;

procedure StartCountdown;  
begin
  TimeLeft := 10;
  IntervalID := window.setInterval(@UpdateCountdown, 1000); // 1000 ms = 1 seconde
end;

procedure DelayedMessage;  
begin
  window.setTimeout(
    procedure
    begin
      window.alert('Ce message s''affiche après 3 secondes');
    end,
    3000
  );
end;

procedure Initialize;  
var
  StartBtn, DelayBtn: TJSHTMLElement;
begin
  CountdownElement := TJSHTMLElement(document.getElementById('countdown'));
  StartBtn := TJSHTMLElement(document.getElementById('startBtn'));
  DelayBtn := TJSHTMLElement(document.getElementById('delayBtn'));

  StartBtn.addEventListener('click',
    procedure(Event: TJSEvent) begin StartCountdown; end);

  DelayBtn.addEventListener('click',
    procedure(Event: TJSEvent) begin DelayedMessage; end);
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

### RequestAnimationFrame pour des animations fluides

```pascal
program AnimationExample;

{$mode objfpc}

uses
  JS, Web;

var
  Box: TJSHTMLElement;
  Position: Double;
  AnimationID: NativeInt;
  IsAnimating: Boolean;

procedure Animate(Timestamp: Double);  
begin
  if not IsAnimating then
    Exit;

  Position := Position + 2;

  if Position > 500 then
    Position := 0;

  Box.style.setProperty('left', FloatToStr(Position) + 'px');

  AnimationID := window.requestAnimationFrame(@Animate);
end;

procedure StartAnimation;  
begin
  if not IsAnimating then
  begin
    IsAnimating := True;
    AnimationID := window.requestAnimationFrame(@Animate);
  end;
end;

procedure StopAnimation;  
begin
  IsAnimating := False;
  if AnimationID <> 0 then
    window.cancelAnimationFrame(AnimationID);
end;

procedure Initialize;  
var
  StartBtn, StopBtn: TJSHTMLElement;
begin
  Box := TJSHTMLElement(document.getElementById('animatedBox'));
  Position := 0;
  IsAnimating := False;

  StartBtn := TJSHTMLElement(document.getElementById('startAnim'));
  StopBtn := TJSHTMLElement(document.getElementById('stopAnim'));

  StartBtn.addEventListener('click',
    procedure(Event: TJSEvent) begin StartAnimation; end);

  StopBtn.addEventListener('click',
    procedure(Event: TJSEvent) begin StopAnimation; end);
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

## 9.8.9 Intégration avec des bibliothèques JavaScript

### Utilisation de bibliothèques externes

**Déclaration de types pour jQuery :**

```pascal
unit jQueryBinding;

{$mode objfpc}

interface

uses
  JS, Web;

type
  TJQuery = class external name 'jQuery'(TJSObject)
  public
    // Sélecteurs
    constructor new(Selector: string); external name 'jQuery';

    // Manipulation DOM
    function html: string; overload;
    function html(Content: string): TJQuery; overload;
    function text: string; overload;
    function text(Content: string): TJQuery; overload;
    function val: string; overload;
    function val(Value: string): TJQuery; overload;
    function attr(Name: string): string; overload;
    function attr(Name, Value: string): TJQuery; overload;

    // Classes CSS
    function addClass(ClassName: string): TJQuery;
    function removeClass(ClassName: string): TJQuery;
    function toggleClass(ClassName: string): TJQuery;
    function hasClass(ClassName: string): Boolean;

    // Styles
    function css(PropertyName: string): string; overload;
    function css(PropertyName, Value: string): TJQuery; overload;

    // Événements
    function on(EventName: string; Handler: TJSEventHandler): TJQuery;
    function off(EventName: string): TJQuery;
    function click(Handler: TJSEventHandler): TJQuery;
    function change(Handler: TJSEventHandler): TJQuery;

    // Animations
    function show: TJQuery; overload;
    function show(Duration: Integer): TJQuery; overload;
    function hide: TJQuery; overload;
    function hide(Duration: Integer): TJQuery; overload;
    function fadeIn(Duration: Integer): TJQuery;
    function fadeOut(Duration: Integer): TJQuery;
    function slideDown(Duration: Integer): TJQuery;
    function slideUp(Duration: Integer): TJQuery;

    // AJAX
    function load(URL: string): TJQuery;
  end;

  TJQueryStatic = class external name 'jQuery'(TJSObject)
  public
    // Méthodes statiques
    class function ajax(Options: TJSObject): TJSObject;
    class function get(URL: string; Success: TJSEventHandler): TJSObject;
    class function post(URL: string; Data: TJSObject; Success: TJSEventHandler): TJSObject;
  end;

// Fonction globale jQuery
function jQuery(Selector: string): TJQuery; external name 'jQuery';  
function $(Selector: string): TJQuery; external name 'jQuery';

implementation

end.
```

**Utilisation de jQuery :**

```pascal
program jQueryExample;

{$mode objfpc}

uses
  JS, Web, jQueryBinding;

procedure Initialize;  
begin
  // Utiliser jQuery pour manipuler le DOM
  $('#myButton').click(
    procedure(Event: TJSEvent)
    begin
      $('#message').html('Bouton cliqué avec jQuery!');
      $('#box').fadeIn(500);
    end
  );

  // Changer le style
  $('.highlight').css('background-color', 'yellow');

  // AJAX avec jQuery
  TJQueryStatic.get('https://api.example.com/data',
    procedure(Data: TJSObject)
    begin
      console.log('Données reçues:', Data);
    end
  );
end;

begin
  $(document).ready(
    procedure
    begin
      Initialize;
    end
  );
end.
```

**HTML avec jQuery :**

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>jQuery avec Pas2JS</title>
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
    <script src="rtl.js"></script>
    <script src="jqueryexample.js"></script>
</head>
<body>
    <button id="myButton">Cliquez-moi</button>
    <div id="message"></div>
    <div id="box" style="display:none;">Boîte animée</div>
    <p class="highlight">Texte en surbrillance</p>
</body>
</html>
```

### Intégration avec Chart.js

```pascal
unit ChartJSBinding;

{$mode objfpc}

interface

uses
  JS, Web;

type
  TChartType = string;

const
  ctLine = 'line';
  ctBar = 'bar';
  ctPie = 'pie';
  ctDoughnut = 'doughnut';

type
  TChart = class external name 'Chart'(TJSObject)
  public
    constructor new(Context: TJSCanvasRenderingContext2D; Config: TJSObject);
    procedure update;
    procedure destroy;
  end;

implementation

end.
```

**Utilisation de Chart.js :**

```pascal
program ChartExample;

{$mode objfpc}

uses
  JS, Web, ChartJSBinding;

procedure CreateBarChart;  
var
  Canvas: TJSHTMLCanvasElement;
  Context: TJSCanvasRenderingContext2D;
  Config: TJSObject;
  Data: TJSObject;
  Datasets: TJSArray;
  Dataset: TJSObject;
  Labels: TJSArray;
  MyChart: TChart;
begin
  Canvas := TJSHTMLCanvasElement(document.getElementById('myChart'));
  Context := TJSCanvasRenderingContext2D(Canvas.getContext('2d'));

  // Préparer les labels
  Labels := TJSArray.new('Lundi', 'Mardi', 'Mercredi', 'Jeudi', 'Vendredi');

  // Préparer le dataset
  Dataset := TJSObject.new;
  Dataset['label'] := 'Ventes';
  Dataset['data'] := TJSArray.new(12, 19, 3, 5, 2);
  Dataset['backgroundColor'] := 'rgba(54, 162, 235, 0.2)';
  Dataset['borderColor'] := 'rgba(54, 162, 235, 1)';
  Dataset['borderWidth'] := 1;

  Datasets := TJSArray.new(Dataset);

  // Préparer les données
  Data := TJSObject.new;
  Data['labels'] := Labels;
  Data['datasets'] := Datasets;

  // Configuration du graphique
  Config := TJSObject.new;
  Config['type'] := ctBar;
  Config['data'] := Data;
  Config['options'] := TJSObject.new;

  // Créer le graphique
  MyChart := TChart.new(Context, Config);

  console.log('Graphique créé avec succès');
end;

procedure Initialize;  
begin
  CreateBarChart;
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

**HTML pour Chart.js :**

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Chart.js avec Pas2JS</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <script src="rtl.js"></script>
    <script src="chartexample.js"></script>
</head>
<body>
    <h1>Graphique des ventes</h1>
    <canvas id="myChart" width="400" height="200"></canvas>
</body>
</html>
```

## 9.8.10 Application complète : Gestionnaire de contacts

Créons une application complète pour démontrer les concepts appris :

```pascal
program ContactManager;

{$mode objfpc}

uses
  JS, Web, SysUtils;

type
  { Classe Contact }
  TContact = class
  private
    FID: Integer;
    FName: string;
    FEmail: string;
    FPhone: string;
  public
    constructor Create(AID: Integer; const AName, AEmail, APhone: string);
    function ToJSON: TJSObject;
    class function FromJSON(JSON: TJSObject): TContact;
    property ID: Integer read FID;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
    property Phone: string read FPhone write FPhone;
  end;

  { Gestionnaire de contacts }
  TContactManager = class
  private
    FContacts: array of TContact;
    FNextID: Integer;
    function GetContactCount: Integer;
    procedure SaveToStorage;
    procedure LoadFromStorage;
    function FindContactIndex(ContactID: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddContact(const Name, Email, Phone: string);
    procedure UpdateContact(ContactID: Integer; const Name, Email, Phone: string);
    procedure DeleteContact(ContactID: Integer);
    function GetContact(ContactID: Integer): TContact;
    function GetAllContacts: array of TContact;
    property ContactCount: Integer read GetContactCount;
  end;

  { Interface utilisateur }
  TContactUI = class
  private
    FManager: TContactManager;
    FContactList: TJSHTMLElement;
    FContactForm: TJSHTMLElement;
    FNameInput: TJSHTMLInputElement;
    FEmailInput: TJSHTMLInputElement;
    FPhoneInput: TJSHTMLInputElement;
    FEditingID: Integer;
    procedure SetupEventHandlers;
    procedure RenderContactList;
    procedure ShowAddForm;
    procedure ShowEditForm(ContactID: Integer);
    procedure HideForm;
    procedure HandleSubmit(Event: TJSEvent);
    procedure HandleEdit(ContactID: Integer);
    procedure HandleDelete(ContactID: Integer);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TContact }

constructor TContact.Create(AID: Integer; const AName, AEmail, APhone: string);  
begin
  inherited Create;
  FID := AID;
  FName := AName;
  FEmail := AEmail;
  FPhone := APhone;
end;

function TContact.ToJSON: TJSObject;  
begin
  Result := TJSObject.new;
  Result['id'] := FID;
  Result['name'] := FName;
  Result['email'] := FEmail;
  Result['phone'] := FPhone;
end;

class function TContact.FromJSON(JSON: TJSObject): TContact;  
begin
  Result := TContact.Create(
    Integer(JSON['id']),
    String(JSON['name']),
    String(JSON['email']),
    String(JSON['phone'])
  );
end;

{ TContactManager }

constructor TContactManager.Create;  
begin
  inherited Create;
  SetLength(FContacts, 0);
  FNextID := 1;
  LoadFromStorage;
end;

destructor TContactManager.Destroy;  
var
  i: Integer;
begin
  for i := 0 to High(FContacts) do
    FContacts[i].Free;
  inherited Destroy;
end;

function TContactManager.GetContactCount: Integer;  
begin
  Result := Length(FContacts);
end;

procedure TContactManager.SaveToStorage;  
var
  ContactsArray: TJSArray;
  i: Integer;
begin
  ContactsArray := TJSArray.new;

  for i := 0 to High(FContacts) do
    ContactsArray.push(FContacts[i].ToJSON);

  window.localStorage.setItem('contacts', TJSJSON.stringify(ContactsArray));
  window.localStorage.setItem('nextID', IntToStr(FNextID));
end;

procedure TContactManager.LoadFromStorage;  
var
  ContactsJSON: string;
  ContactsArray: TJSArray;
  i: Integer;
  Contact: TContact;
begin
  ContactsJSON := window.localStorage.getItem('contacts');

  if ContactsJSON <> '' then
  begin
    ContactsArray := TJSArray(TJSJSON.parse(ContactsJSON));

    for i := 0 to ContactsArray.length - 1 do
    begin
      Contact := TContact.FromJSON(TJSObject(ContactsArray[i]));
      SetLength(FContacts, Length(FContacts) + 1);
      FContacts[High(FContacts)] := Contact;
    end;
  end;

  FNextID := StrToIntDef(window.localStorage.getItem('nextID'), 1);
end;

function TContactManager.FindContactIndex(ContactID: Integer): Integer;  
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FContacts) do
  begin
    if FContacts[i].ID = ContactID then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TContactManager.AddContact(const Name, Email, Phone: string);  
var
  NewContact: TContact;
begin
  NewContact := TContact.Create(FNextID, Name, Email, Phone);
  Inc(FNextID);

  SetLength(FContacts, Length(FContacts) + 1);
  FContacts[High(FContacts)] := NewContact;

  SaveToStorage;
end;

procedure TContactManager.UpdateContact(ContactID: Integer;
                                       const Name, Email, Phone: string);
var
  Index: Integer;
begin
  Index := FindContactIndex(ContactID);

  if Index >= 0 then
  begin
    FContacts[Index].Name := Name;
    FContacts[Index].Email := Email;
    FContacts[Index].Phone := Phone;
    SaveToStorage;
  end;
end;

procedure TContactManager.DeleteContact(ContactID: Integer);  
var
  Index, i: Integer;
begin
  Index := FindContactIndex(ContactID);

  if Index >= 0 then
  begin
    FContacts[Index].Free;

    for i := Index to High(FContacts) - 1 do
      FContacts[i] := FContacts[i + 1];

    SetLength(FContacts, Length(FContacts) - 1);
    SaveToStorage;
  end;
end;

function TContactManager.GetContact(ContactID: Integer): TContact;  
var
  Index: Integer;
begin
  Result := nil;
  Index := FindContactIndex(ContactID);

  if Index >= 0 then
    Result := FContacts[Index];
end;

function TContactManager.GetAllContacts: array of TContact;  
begin
  Result := FContacts;
end;

{ TContactUI }

constructor TContactUI.Create;  
begin
  inherited Create;
  FManager := TContactManager.Create;
  FEditingID := -1;

  FContactList := TJSHTMLElement(document.getElementById('contactList'));
  FContactForm := TJSHTMLElement(document.getElementById('contactForm'));
  FNameInput := TJSHTMLInputElement(document.getElementById('contactName'));
  FEmailInput := TJSHTMLInputElement(document.getElementById('contactEmail'));
  FPhoneInput := TJSHTMLInputElement(document.getElementById('contactPhone'));

  SetupEventHandlers;
  RenderContactList;
end;

destructor TContactUI.Destroy;  
begin
  FManager.Free;
  inherited Destroy;
end;

procedure TContactUI.SetupEventHandlers;  
var
  AddBtn, CancelBtn: TJSHTMLElement;
  Form: TJSHTMLFormElement;
begin
  AddBtn := TJSHTMLElement(document.getElementById('addContactBtn'));
  AddBtn.addEventListener('click',
    procedure(Event: TJSEvent)
    begin
      ShowAddForm;
    end
  );

  CancelBtn := TJSHTMLElement(document.getElementById('cancelBtn'));
  CancelBtn.addEventListener('click',
    procedure(Event: TJSEvent)
    begin
      HideForm;
    end
  );

  Form := TJSHTMLFormElement(document.getElementById('contactFormElement'));
  Form.addEventListener('submit', @HandleSubmit);
end;

procedure TContactUI.RenderContactList;  
var
  HTML: string;
  Contacts: array of TContact;
  i: Integer;
  Contact: TContact;
begin
  HTML := '';
  Contacts := FManager.GetAllContacts;

  if Length(Contacts) = 0 then
  begin
    HTML := '<p class="empty-message">Aucun contact. Ajoutez-en un!</p>';
  end
  else
  begin
    HTML := '<div class="contacts-grid">';

    for i := 0 to High(Contacts) do
    begin
      Contact := Contacts[i];
      HTML := HTML + Format(
        '<div class="contact-card" data-id="%d">' +
        '  <h3>%s</h3>' +
        '  <p>📧 %s</p>' +
        '  <p>📱 %s</p>' +
        '  <div class="contact-actions">' +
        '    <button class="edit-btn" data-id="%d">Modifier</button>' +
        '    <button class="delete-btn" data-id="%d">Supprimer</button>' +
        '  </div>' +
        '</div>',
        [Contact.ID, Contact.Name, Contact.Email, Contact.Phone,
         Contact.ID, Contact.ID]
      );
    end;

    HTML := HTML + '</div>';
  end;

  FContactList.innerHTML := HTML;

  // Ajouter les gestionnaires d'événements
  var EditButtons := document.querySelectorAll('.edit-btn');
  for i := 0 to EditButtons.length - 1 do
  begin
    var Btn := TJSHTMLElement(EditButtons[i]);
    var ContactID := StrToInt(Btn.getAttribute('data-id'));

    Btn.addEventListener('click',
      procedure(Event: TJSEvent)
      begin
        HandleEdit(ContactID);
      end
    );
  end;

  var DeleteButtons := document.querySelectorAll('.delete-btn');
  for i := 0 to DeleteButtons.length - 1 do
  begin
    var Btn := TJSHTMLElement(DeleteButtons[i]);
    var ContactID := StrToInt(Btn.getAttribute('data-id'));

    Btn.addEventListener('click',
      procedure(Event: TJSEvent)
      begin
        HandleDelete(ContactID);
      end
    );
  end;
end;

procedure TContactUI.ShowAddForm;  
begin
  FEditingID := -1;
  FNameInput.value := '';
  FEmailInput.value := '';
  FPhoneInput.value := '';

  TJSHTMLElement(document.getElementById('formTitle')).innerHTML := 'Ajouter un contact';
  FContactForm.style.setProperty('display', 'block');
  FNameInput.focus;
end;

procedure TContactUI.ShowEditForm(ContactID: Integer);  
var
  Contact: TContact;
begin
  Contact := FManager.GetContact(ContactID);

  if Assigned(Contact) then
  begin
    FEditingID := ContactID;
    FNameInput.value := Contact.Name;
    FEmailInput.value := Contact.Email;
    FPhoneInput.value := Contact.Phone;

    TJSHTMLElement(document.getElementById('formTitle')).innerHTML := 'Modifier le contact';
    FContactForm.style.setProperty('display', 'block');
    FNameInput.focus;
  end;
end;

procedure TContactUI.HideForm;  
begin
  FContactForm.style.setProperty('display', 'none');
  FEditingID := -1;
end;

procedure TContactUI.HandleSubmit(Event: TJSEvent);  
var
  Name, Email, Phone: string;
begin
  Event.preventDefault;

  Name := Trim(FNameInput.value);
  Email := Trim(FEmailInput.value);
  Phone := Trim(FPhoneInput.value);

  // Validation simple
  if (Name = '') or (Email = '') or (Phone = '') then
  begin
    window.alert('Tous les champs sont requis!');
    Exit;
  end;

  if FEditingID = -1 then
  begin
    // Ajouter
    FManager.AddContact(Name, Email, Phone);
  end
  else
  begin
    // Modifier
    FManager.UpdateContact(FEditingID, Name, Email, Phone);
  end;

  HideForm;
  RenderContactList;
end;

procedure TContactUI.HandleEdit(ContactID: Integer);  
begin
  ShowEditForm(ContactID);
end;

procedure TContactUI.HandleDelete(ContactID: Integer);  
begin
  if window.confirm('Êtes-vous sûr de vouloir supprimer ce contact?') then
  begin
    FManager.DeleteContact(ContactID);
    RenderContactList;
  end;
end;

var
  App: TContactUI;

procedure Initialize;  
begin
  App := TContactUI.Create;
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

**HTML pour le gestionnaire de contacts :**

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Gestionnaire de Contacts</title>
    <style>
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body {
            font-family: Arial, sans-serif;
            background: #f5f5f5;
            padding: 20px;
        }
        .container {
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        h1 {
            color: #333;
            margin-bottom: 30px;
            text-align: center;
        }
        .header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 30px;
        }
        button {
            background: #007bff;
            color: white;
            border: none;
            padding: 10px 20px;
            border-radius: 5px;
            cursor: pointer;
            font-size: 14px;
        }
        button:hover { background: #0056b3; }
        .contacts-grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
            gap: 20px;
        }
        .contact-card {
            background: #f8f9fa;
            padding: 20px;
            border-radius: 8px;
            border: 1px solid #dee2e6;
        }
        .contact-card h3 {
            color: #333;
            margin-bottom: 10px;
        }
        .contact-card p {
            color: #666;
            margin: 5px 0;
        }
        .contact-actions {
            margin-top: 15px;
            display: flex;
            gap: 10px;
        }
        .edit-btn {
            background: #28a745;
        }
        .edit-btn:hover {
            background: #218838;
        }
        .delete-btn {
            background: #dc3545;
        }
        .delete-btn:hover {
            background: #c82333;
        }
        .empty-message {
            text-align: center;
            color: #999;
            padding: 40px;
            font-size: 18px;
        }
        #contactForm {
            display: none;
            position: fixed;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            background: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 5px 20px rgba(0,0,0,0.3);
            z-index: 1000;
            min-width: 400px;
        }
        #contactForm h2 {
            margin-bottom: 20px;
        }
        .form-group {
            margin-bottom: 15px;
        }
        .form-group label {
            display: block;
            margin-bottom: 5px;
            font-weight: bold;
            color: #333;
        }
        .form-group input {
            width: 100%;
            padding: 10px;
            border: 1px solid #ddd;
            border-radius: 5px;
            font-size: 14px;
        }
        .form-actions {
            display: flex;
            gap: 10px;
            margin-top: 20px;
        }
        .form-actions button {
            flex: 1;
        }
        #cancelBtn {
            background: #6c757d;
        }
        #cancelBtn:hover {
            background: #5a6268;
        }
    </style>
    <script src="rtl.js"></script>
    <script src="contactmanager.js"></script>
</head>
<body>
    <div class="container">
        <h1>📇 Gestionnaire de Contacts</h1>

        <div class="header">
            <h2>Mes Contacts (<span id="contactCount">0</span>)</h2>
            <button id="addContactBtn">➕ Ajouter un contact</button>
        </div>

        <div id="contactList"></div>
    </div>

    <div id="contactForm">
        <h2 id="formTitle">Ajouter un contact</h2>
        <form id="contactFormElement">
            <div class="form-group">
                <label for="contactName">Nom :</label>
                <input type="text" id="contactName" required>
            </div>
            <div class="form-group">
                <label for="contactEmail">Email :</label>
                <input type="email" id="contactEmail" required>
            </div>
            <div class="form-group">
                <label for="contactPhone">Téléphone :</label>
                <input type="tel" id="contactPhone" required>
            </div>
            <div class="form-actions">
                <button type="submit">💾 Enregistrer</button>
                <button type="button" id="cancelBtn">❌ Annuler</button>
            </div>
        </form>
    </div>
</body>
</html>
```

## 9.8.11 Tests et débogage

### Console et débogage

```pascal
program DebuggingExample;

{$mode objfpc}

uses
  JS, Web, SysUtils;

procedure DebugExample;  
var
  MyObject: TJSObject;
  MyArray: TJSArray;
begin
  // Logs simples
  console.log('Message de débogage');
  console.info('Information');
  console.warn('Avertissement');
  console.error('Erreur');

  // Afficher des objets
  MyObject := TJSObject.new;
  MyObject['name'] := 'Jean';
  MyObject['age'] := 30;
  console.log('Objet:', MyObject);

  // Afficher des tableaux
  MyArray := TJSArray.new(1, 2, 3, 4, 5);
  console.log('Tableau:', MyArray);

  // Mesurer le temps d'exécution
  console.time('Operation');
  // ... code à mesurer ...
  Sleep(100);
  console.timeEnd('Operation');

  // Tracer l'appel de pile
  console.trace('Trace de la pile d''appels');

  // Grouper les logs
  console.group('Groupe de logs');
  console.log('Log 1');
  console.log('Log 2');
  console.groupEnd;
end;

begin
  window.addEventListener('load',
    procedure
    begin
      DebugExample;
    end
  );
end.
```

### Gestion des erreurs

```pascal
program ErrorHandling;

{$mode objfpc}

uses
  JS, Web;

procedure SafeFunction;  
begin
  try
    // Code qui peut générer une erreur
    var Element := document.getElementById('nonExistant');
    Element.innerHTML := 'Test'; // Erreur si Element est nil
  except
    on E: Exception do
    begin
      console.error('Erreur attrapée:', E.Message);
      window.alert('Une erreur s''est produite: ' + E.Message);
    end;
  end;
end;

procedure CustomError;  
begin
  raise Exception.Create('Ceci est une erreur personnalisée');
end;

begin
  window.addEventListener('load',
    procedure
    begin
      SafeFunction;

      try
        CustomError;
      except
        on E: Exception do
          console.error('Erreur personnalisée:', E.Message);
      end;
    end
  );
end.
```

## 9.8.12 Optimisation et bonnes pratiques

### Minimisation du code généré

**Options de compilation :**

```bash
# Compilation avec optimisation
pas2js -Jirtl.js -O3 myapp.pas

# Générer du code minifié
pas2js -Jirtl.js -Jminclude myapp.pas
```

### Lazy loading

```pascal
program LazyLoading;

{$mode objfpc}

uses
  JS, Web;

procedure LoadModule(const ModuleName: string);  
var
  Script: TJSHTMLScriptElement;
begin
  Script := TJSHTMLScriptElement(document.createElement('script'));
  Script.src := ModuleName + '.js';

  Script.addEventListener('load',
    procedure(Event: TJSEvent)
    begin
      console.log('Module chargé:', ModuleName);
    end
  );

  document.head.appendChild(Script);
end;

procedure Initialize;  
begin
  // Charger un module uniquement quand nécessaire
  var LoadBtn := document.getElementById('loadModuleBtn');
  LoadBtn.addEventListener('click',
    procedure(Event: TJSEvent)
    begin
      LoadModule('heavy-module');
    end
  );
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

### Gestion de la mémoire

```pascal
program MemoryManagement;

{$mode objfpc}

uses
  JS, Web;

type
  TDataProcessor = class
  private
    FData: TJSArray;
    FElements: array of TJSHTMLElement;
    FEventHandlers: array of TJSEventHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadData;
    procedure ClearData;
    procedure AddEventHandler(Element: TJSHTMLElement; EventName: string;
                             Handler: TJSEventHandler);
    procedure RemoveAllEventHandlers;
  end;

constructor TDataProcessor.Create;  
begin
  inherited Create;
  FData := TJSArray.new;
  SetLength(FElements, 0);
  SetLength(FEventHandlers, 0);
end;

destructor TDataProcessor.Destroy;  
begin
  ClearData;
  RemoveAllEventHandlers;
  inherited Destroy;
end;

procedure TDataProcessor.LoadData;  
var
  i: Integer;
begin
  // Nettoyer les anciennes données avant d'en charger de nouvelles
  ClearData;

  // Charger de nouvelles données
  for i := 1 to 1000 do
    FData.push(TJSObject.new);

  console.log('Données chargées:', FData.length);
end;

procedure TDataProcessor.ClearData;  
begin
  // Libérer explicitement la mémoire
  FData := TJSArray.new;
  SetLength(FElements, 0);

  console.log('Mémoire libérée');
end;

procedure TDataProcessor.AddEventHandler(Element: TJSHTMLElement;
                                        EventName: string;
                                        Handler: TJSEventHandler);
begin
  Element.addEventListener(EventName, Handler);

  // Garder une référence pour pouvoir supprimer plus tard
  SetLength(FElements, Length(FElements) + 1);
  FElements[High(FElements)] := Element;

  SetLength(FEventHandlers, Length(FEventHandlers) + 1);
  FEventHandlers[High(FEventHandlers)] := Handler;
end;

procedure TDataProcessor.RemoveAllEventHandlers;  
var
  i: Integer;
begin
  // Supprimer tous les gestionnaires d'événements
  for i := 0 to High(FElements) do
  begin
    if Assigned(FElements[i]) then
      FElements[i].removeEventListener('click', FEventHandlers[i]);
  end;

  SetLength(FElements, 0);
  SetLength(FEventHandlers, 0);
end;

var
  Processor: TDataProcessor;

procedure Initialize;  
var
  LoadBtn, ClearBtn: TJSHTMLElement;
begin
  Processor := TDataProcessor.Create;

  LoadBtn := TJSHTMLElement(document.getElementById('loadBtn'));
  ClearBtn := TJSHTMLElement(document.getElementById('clearBtn'));

  LoadBtn.addEventListener('click',
    procedure(Event: TJSEvent)
    begin
      Processor.LoadData;
    end
  );

  ClearBtn.addEventListener('click',
    procedure(Event: TJSEvent)
    begin
      Processor.ClearData;
    end
  );
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

### Éviter les fuites mémoire

**Bonnes pratiques :**

1. **Supprimer les event listeners** quand ils ne sont plus nécessaires
2. **Nettoyer les timers** avec `clearInterval` et `clearTimeout`
3. **Éviter les références circulaires** entre objets
4. **Libérer les ressources** dans les destructeurs

```pascal
type
  TCleanComponent = class
  private
    FElement: TJSHTMLElement;
    FIntervalID: NativeInt;
    procedure StartUpdate;
    procedure StopUpdate;
  public
    constructor Create(Element: TJSHTMLElement);
    destructor Destroy; override;
  end;

constructor TCleanComponent.Create(Element: TJSHTMLElement);  
begin
  inherited Create;
  FElement := Element;
  StartUpdate;
end;

destructor TCleanComponent.Destroy;  
begin
  // Nettoyer avant de détruire
  StopUpdate;
  FElement := nil;
  inherited Destroy;
end;

procedure TCleanComponent.StartUpdate;  
begin
  FIntervalID := window.setInterval(
    procedure
    begin
      // Mise à jour périodique
      if Assigned(FElement) then
        FElement.innerHTML := DateTimeToStr(Now);
    end,
    1000
  );
end;

procedure TCleanComponent.StopUpdate;  
begin
  if FIntervalID <> 0 then
  begin
    window.clearInterval(FIntervalID);
    FIntervalID := 0;
  end;
end;
```

## 9.8.13 Progressive Web App (PWA)

### Service Worker avec Pas2JS

**serviceWorker.pas :**

```pascal
program ServiceWorker;

{$mode objfpc}

uses
  JS, Web;

const
  CACHE_NAME = 'my-app-v1';

var
  UrlsToCache: TJSArray;

procedure HandleInstall(Event: TJSEvent);  
begin
  console.log('Service Worker: Installation');

  // Préparer les URLs à mettre en cache
  UrlsToCache := TJSArray.new(
    '/',
    '/index.html',
    '/styles.css',
    '/rtl.js',
    '/app.js',
    '/icon.png'
  );

  // Attendre que le cache soit rempli
  Event['waitUntil'](
    window['caches']['open'](CACHE_NAME)['then'](
      function(cache: TJSObject): TJSPromise
      begin
        console.log('Cache ouvert');
        Result := cache['addAll'](UrlsToCache);
      end
    )
  );
end;

procedure HandleActivate(Event: TJSEvent);  
begin
  console.log('Service Worker: Activation');

  Event['waitUntil'](
    window['caches']['keys']()['then'](
      function(cacheNames: TJSArray): TJSPromise
      begin
        Result := TJSPromise['all'](
          cacheNames['map'](
            function(cacheName: string): TJSPromise
            begin
              if cacheName <> CACHE_NAME then
              begin
                console.log('Suppression ancien cache:', cacheName);
                Result := window['caches']['delete'](cacheName);
              end;
            end
          )
        );
      end
    )
  );
end;

procedure HandleFetch(Event: TJSEvent);  
var
  Request: TJSObject;
begin
  Request := Event['request'];

  Event['respondWith'](
    window['caches']['match'](Request)['then'](
      function(response: TJSObject): TJSObject
      begin
        // Retourner la réponse du cache si disponible
        if Assigned(response) then
        begin
          console.log('Réponse depuis le cache:', Request['url']);
          Result := response;
        end
        else
        begin
          // Sinon, faire une requête réseau
          console.log('Requête réseau:', Request['url']);
          Result := window['fetch'](Request);
        end;
      end
    )
  );
end;

begin
  // Enregistrer les gestionnaires d'événements
  self['addEventListener']('install', @HandleInstall);
  self['addEventListener']('activate', @HandleActivate);
  self['addEventListener']('fetch', @HandleFetch);

  console.log('Service Worker: Chargé');
end.
```

### Enregistrement du Service Worker

**app.pas :**

```pascal
procedure RegisterServiceWorker;  
begin
  if window['navigator']['serviceWorker'] <> undefined then
  begin
    window['navigator']['serviceWorker']['register']('/serviceWorker.js')
      ['then'](
        procedure(registration: TJSObject)
        begin
          console.log('Service Worker enregistré avec succès');
        end
      )
      ['catch'](
        procedure(error: TJSObject)
        begin
          console.error('Erreur d''enregistrement:', error);
        end
      );
  end
  else
    console.log('Service Workers non supportés');
end;

begin
  window.addEventListener('load',
    procedure
    begin
      RegisterServiceWorker;
    end
  );
end;
```

### Manifest.json pour PWA

```json
{
  "name": "Mon Application Pas2JS",
  "short_name": "MyApp",
  "description": "Application web progressive créée avec Pas2JS",
  "start_url": "/",
  "display": "standalone",
  "background_color": "#ffffff",
  "theme_color": "#007bff",
  "icons": [
    {
      "src": "/icon-192.png",
      "sizes": "192x192",
      "type": "image/png"
    },
    {
      "src": "/icon-512.png",
      "sizes": "512x512",
      "type": "image/png"
    }
  ]
}
```

## 9.8.14 Différences multi-plateformes

### Compilation spécifique par plateforme

**Windows :**

```batch
@echo off
echo Compilation pour Windows...

REM Compiler avec Pas2JS  
pas2js -Jirtl.js -O3 app.pas

REM Copier les fichiers dans le dossier de sortie  
xcopy /Y *.html output\  
xcopy /Y *.css output\  
xcopy /Y *.js output\

echo Terminé!  
pause
```

**Ubuntu/Linux :**

```bash
#!/bin/bash
echo "Compilation pour Linux..."

# Compiler avec Pas2JS
pas2js -Jirtl.js -O3 app.pas

# Copier les fichiers dans le dossier de sortie
cp -f *.html output/  
cp -f *.css output/  
cp -f *.js output/

echo "Terminé!"
```

### Script de build universel

**build.pas :**

```pascal
program BuildScript;

{$mode objfpc}

uses
  SysUtils, Process, Classes;

procedure ExecuteCommand(const Command: string);  
var
  Process: TProcess;
  OutputLines: TStringList;
begin
  Process := TProcess.Create(nil);
  OutputLines := TStringList.Create;
  try
    Process.CommandLine := Command;
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    WriteLn('Exécution: ', Command);
    Process.Execute;

    OutputLines.LoadFromStream(Process.Output);
    WriteLn(OutputLines.Text);
  finally
    OutputLines.Free;
    Process.Free;
  end;
end;

procedure CompileProject;  
begin
  WriteLn('=== Compilation Pas2JS ===');
  ExecuteCommand('pas2js -Jirtl.js -O3 app.pas');
  WriteLn('Compilation terminée!');
end;

procedure CopyFiles;  
var
  SourceDir, OutputDir: string;
begin
  WriteLn('=== Copie des fichiers ===');

  SourceDir := GetCurrentDir;
  OutputDir := SourceDir + DirectorySeparator + 'output';

  if not DirectoryExists(OutputDir) then
    CreateDir(OutputDir);

  {$IFDEF WINDOWS}
  ExecuteCommand('xcopy /Y *.html ' + OutputDir);
  ExecuteCommand('xcopy /Y *.css ' + OutputDir);
  ExecuteCommand('xcopy /Y *.js ' + OutputDir);
  {$ELSE}
  ExecuteCommand('cp -f *.html ' + OutputDir);
  ExecuteCommand('cp -f *.css ' + OutputDir);
  ExecuteCommand('cp -f *.js ' + OutputDir);
  {$ENDIF}

  WriteLn('Fichiers copiés!');
end;

procedure MinifyJS;  
begin
  WriteLn('=== Minification JavaScript ===');

  {$IFDEF WINDOWS}
  if FileExists('C:\Tools\terser.cmd') then
    ExecuteCommand('terser output\app.js -o output\app.min.js -c -m')
  else
    WriteLn('Terser non trouvé, minification ignorée');
  {$ELSE}
  if FileExists('/usr/bin/terser') or FileExists('/usr/local/bin/terser') then
    ExecuteCommand('terser output/app.js -o output/app.min.js -c -m')
  else
    WriteLn('Terser non trouvé, minification ignorée');
  {$ENDIF}
end;

begin
  WriteLn('========================================');
  WriteLn('  Script de build Pas2JS');
  WriteLn('  Plateforme: ' + {$I %FPCTARGETOS%});
  WriteLn('========================================');
  WriteLn('');

  try
    CompileProject;
    WriteLn('');
    CopyFiles;
    WriteLn('');
    MinifyJS;
    WriteLn('');
    WriteLn('Build réussi!');
  except
    on E: Exception do
    begin
      WriteLn('ERREUR: ', E.Message);
      Halt(1);
    end;
  end;
end.
```

## 9.8.15 Intégration avec les frameworks modernes

### Utilisation avec React (via bindings)

```pascal
unit ReactBinding;

{$mode objfpc}

interface

uses
  JS, Web;

type
  TReactElement = class external name 'Object'(TJSObject)
  end;

  TReactClass = class external name 'Object'(TJSObject)
  end;

  TReact = class external name 'React'(TJSObject)
  public
    class function createElement(ElementType: string;
                                 Props: TJSObject;
                                 Children: array of TJSObject): TReactElement;
    class function Component: TReactClass;
  end;

  TReactDOM = class external name 'ReactDOM'(TJSObject)
  public
    class procedure render(Element: TReactElement; Container: TJSHTMLElement);
  end;

implementation

end.
```

### Exemple d'application React

```pascal
program ReactApp;

{$mode objfpc}

uses
  JS, Web, ReactBinding;

var
  AppElement: TReactElement;
  Container: TJSHTMLElement;

procedure CreateApp;  
var
  Props: TJSObject;
  Title: TReactElement;
  Content: TReactElement;
begin
  // Créer un titre
  Props := TJSObject.new;
  Title := TReact.createElement('h1', Props, [TJSObject(JS.toObject('Mon App Pas2JS'))]);

  // Créer le contenu
  Props := TJSObject.new;
  Props['className'] := 'app-content';
  Content := TReact.createElement('div', Props, [Title]);

  AppElement := Content;
end;

procedure Initialize;  
begin
  CreateApp;
  Container := TJSHTMLElement(document.getElementById('root'));
  TReactDOM.render(AppElement, Container);

  console.log('Application React montée');
end;

begin
  window.addEventListener('load', @Initialize);
end.
```

## 9.8.16 Déploiement et hébergement

### Préparation pour la production

**Structure de déploiement :**

```
dist/
├── index.html
├── manifest.json
├── serviceWorker.js
├── css/
│   └── style.min.css
├── js/
│   ├── rtl.min.js
│   └── app.min.js
├── assets/
│   ├── images/
│   └── fonts/
└── icons/
    ├── icon-192.png
    └── icon-512.png
```

### Script de déploiement

```pascal
program DeployScript;

{$mode objfpc}{$H+}

uses
  SysUtils, Process;

procedure PrepareForProduction;  
begin
  WriteLn('Préparation pour la production...');

  // Compiler en mode release
  ExecuteProcess('pas2js', ['-Jirtl.js', '-O3', '-Jminclude', 'app.pas']);

  // Minifier CSS
  ExecuteProcess('cleancss', ['-o', 'dist/css/style.min.css', 'css/style.css']);

  // Copier les fichiers
  CopyFile('index.html', 'dist/index.html');
  CopyFile('manifest.json', 'dist/manifest.json');

  WriteLn('Préparation terminée!');
end;

procedure DeployToServer;  
var
  {$IFDEF WINDOWS}
  SCPCommand: string = 'pscp';
  {$ELSE}
  SCPCommand: string = 'scp';
  {$ENDIF}
begin
  WriteLn('Déploiement sur le serveur...');

  // Utiliser SCP pour transférer les fichiers
  ExecuteProcess(SCPCommand, [
    '-r',
    'dist/*',
    'user@server.com:/var/www/html/'
  ]);

  WriteLn('Déploiement terminé!');
end;

var
  Answer: string;
begin
  try
    PrepareForProduction;

    Write('Déployer sur le serveur? (o/n): ');
    ReadLn(Answer);
    if (Length(Answer) > 0) and (UpCase(Answer[1]) = 'O') then
      DeployToServer;
  except
    on E: Exception do
      WriteLn('ERREUR: ', E.Message);
  end;
end.
```

### Configuration serveur web

**Nginx (Ubuntu) :**

```nginx
server {
    listen 80;
    server_name monapp.com;
    root /var/www/html;
    index index.html;

    # Compression gzip
    gzip on;
    gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript;

    # Cache pour les assets statiques
    location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2)$ {
        expires 1y;
        add_header Cache-Control "public, immutable";
    }

    # Service Worker
    location /serviceWorker.js {
        add_header Cache-Control "no-cache";
    }

    # SPA fallback
    location / {
        try_files $uri $uri/ /index.html;
    }
}
```

**Apache (Windows/Ubuntu) :**

```apache
<VirtualHost *:80>
    ServerName monapp.com
    DocumentRoot /var/www/html

    # Compression
    <IfModule mod_deflate.c>
        AddOutputFilterByType DEFLATE text/html text/plain text/css application/javascript
    </IfModule>

    # Cache
    <IfModule mod_expires.c>
        ExpiresActive On
        ExpiresByType text/css "access plus 1 year"
        ExpiresByType application/javascript "access plus 1 year"
        ExpiresByType image/png "access plus 1 year"
    </IfModule>

    # Réécriture pour SPA
    <IfModule mod_rewrite.c>
        RewriteEngine On
        RewriteBase /
        RewriteRule ^index\.html$ - [L]
        RewriteCond %{REQUEST_FILENAME} !-f
        RewriteCond %{REQUEST_FILENAME} !-d
        RewriteRule . /index.html [L]
    </IfModule>
</VirtualHost>
```

## 9.8.17 Bonnes pratiques et recommandations

### Architecture recommandée

```
MonProjet/
├── src/
│   ├── units/              # Unités réutilisables
│   │   ├── models.pas      # Modèles de données
│   │   ├── services.pas    # Services (API, storage)
│   │   └── utils.pas       # Utilitaires
│   ├── components/         # Composants UI
│   │   ├── header.pas
│   │   ├── footer.pas
│   │   └── modal.pas
│   └── app.pas            # Point d'entrée
├── public/
│   ├── index.html
│   └── assets/
├── tests/                  # Tests unitaires
├── docs/                   # Documentation
└── build/                  # Scripts de build
```

### Conventions de code

```pascal
// Utiliser des noms descriptifs
procedure ShowUserProfile(UserID: Integer);  // Bon  
procedure SUP(ID: Integer);                  // Mauvais

// Commenter les sections complexes
procedure ProcessData(Data: TJSArray);  
begin
  // Étape 1: Filtrer les données invalides
  FilterInvalidData(Data);

  // Étape 2: Transformer les données
  TransformData(Data);

  // Étape 3: Sauvegarder
  SaveToStorage(Data);
end;

// Utiliser des constantes pour les valeurs magiques
const
  MAX_RETRIES = 3;
  TIMEOUT_MS = 5000;
  API_BASE_URL = 'https://api.example.com';
```

### Sécurité

**Validation des entrées :**

```pascal
function SanitizeInput(const Input: string): string;  
begin
  Result := Input;

  // Échapper les caractères HTML
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);

  // Limiter la longueur
  if Length(Result) > 1000 then
    Result := Copy(Result, 1, 1000);
end;

procedure HandleUserInput;  
var
  UserInput: string;
  SafeInput: string;
begin
  UserInput := TJSHTMLInputElement(document.getElementById('input')).value;
  SafeInput := SanitizeInput(UserInput);

  // Utiliser SafeInput au lieu de UserInput
  document.getElementById('output').innerHTML := SafeInput;
end;
```

**Protection CSRF pour les formulaires :**

```pascal
function GenerateCSRFToken: string;  
begin
  Result := IntToHex(Random(MaxInt), 16) + IntToHex(Random(MaxInt), 16);
  window.sessionStorage.setItem('csrf_token', Result);
end;

function ValidateCSRFToken(const Token: string): Boolean;  
var
  StoredToken: string;
begin
  StoredToken := window.sessionStorage.getItem('csrf_token');
  Result := (StoredToken <> '') and (StoredToken = Token);
end;
```

## 9.8.18 Ressources et documentation

### Documentation officielle

- **Pas2JS Wiki** : https://wiki.freepascal.org/pas2js
- **Forum FreePascal** : https://forum.lazarus.freepascal.org/
- **GitLab Pas2JS** : https://gitlab.com/freepascal.org/fpc/pas2js

### Exemples et tutoriels

```pascal
// Exemple de référence rapide
program QuickReference;

{$mode objfpc}

uses
  JS, Web;

// Manipulation DOM
var Element := document.getElementById('myId');  
Element.innerHTML := 'Nouveau contenu';

// Événements
Element.addEventListener('click', @MyHandler);

// AJAX
window.fetch('url').then(@HandleResponse);

// LocalStorage
window.localStorage.setItem('key', 'value');  
var Value := window.localStorage.getItem('key');

// Timers
window.setTimeout(@MyProc, 1000);  
var ID := window.setInterval(@MyProc, 1000);  
window.clearInterval(ID);

// Console
console.log('Message');  
console.error('Erreur');
```

## Conclusion

Pas2JS ouvre de nouvelles possibilités pour les développeurs FreePascal/Delphi :

**Avantages principaux :**
- Utiliser Pascal pour le développement web frontend
- Réutiliser les connaissances et le code existant
- POO complète dans le navigateur
- Type-safe, évitant de nombreuses erreurs JavaScript
- Génération de code JavaScript optimisé

**Cas d'usage idéaux :**
- Applications web d'entreprise
- Portage d'applications Delphi vers le web
- Équipes maîtrisant Pascal mais pas JavaScript
- Projets nécessitant une forte typage
- PWA et applications hors-ligne

**Limitations à considérer :**
- Écosystème moins riche que JavaScript natif
- Courbe d'apprentissage pour les spécificités web
- Debugging parfois complexe
- Taille du runtime (rtl.js) à prendre en compte

Pas2JS est particulièrement adapté aux développeurs FreePascal souhaitant créer des applications web modernes tout en conservant leur langage de prédilection et en bénéficiant de la sécurité du typage statique.

⏭️ [WebAssembly avec FreePascal](/09-programmation-web-freepascal/09-webassembly-avec-freepascal.md)
