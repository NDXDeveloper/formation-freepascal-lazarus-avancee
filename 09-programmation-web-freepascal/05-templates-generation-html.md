🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.5 Templates et génération HTML

## Introduction

La génération de pages HTML dynamiques est au cœur du développement web. FreePascal offre plusieurs approches pour créer du contenu HTML, allant de la concaténation simple de chaînes à l'utilisation de systèmes de templates sophistiqués. Ce chapitre explore ces différentes techniques de manière progressive.

## 9.5.1 Génération HTML basique

### Concaténation de chaînes

La méthode la plus simple consiste à construire le HTML directement dans le code :

```pascal
function GenerateSimplePage(const Title, Content: string): string;  
begin
  Result := '<!DOCTYPE html>' + LineEnding +
            '<html>' + LineEnding +
            '<head>' + LineEnding +
            '  <title>' + Title + '</title>' + LineEnding +
            '</head>' + LineEnding +
            '<body>' + LineEnding +
            '  <h1>' + Title + '</h1>' + LineEnding +
            '  <p>' + Content + '</p>' + LineEnding +
            '</body>' + LineEnding +
            '</html>';
end;
```

**Avantages :**
- Simple et direct
- Pas de dépendances externes
- Rapide pour de petites pages

**Inconvénients :**
- Code difficile à maintenir
- Mélange de logique et présentation
- Risques d'erreurs de syntaxe HTML
- Pas de séparation des responsabilités

### Utilisation de TStringList

Pour améliorer la lisibilité, on peut utiliser `TStringList` :

```pascal
function GeneratePageWithStringList(const Title, Content: string): string;  
var
  HTML: TStringList;
begin
  HTML := TStringList.Create;
  try
    HTML.Add('<!DOCTYPE html>');
    HTML.Add('<html>');
    HTML.Add('<head>');
    HTML.Add('  <title>' + Title + '</title>');
    HTML.Add('</head>');
    HTML.Add('<body>');
    HTML.Add('  <h1>' + Title + '</h1>');
    HTML.Add('  <p>' + Content + '</p>');
    HTML.Add('</body>');
    HTML.Add('</html>');
    Result := HTML.Text;
  finally
    HTML.Free;
  end;
end;
```

## 9.5.2 Échappement HTML et sécurité

### Le problème de l'injection XSS

Lorsqu'on insère du contenu utilisateur dans le HTML, il faut obligatoirement échapper les caractères spéciaux pour éviter les failles de sécurité (XSS - Cross-Site Scripting).

```pascal
function HTMLEscape(const Text: string): string;  
begin
  Result := StringReplace(Text, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);
end;

// Utilisation sécurisée
function SafeGeneratePage(const Title, UserContent: string): string;  
begin
  Result := '<!DOCTYPE html>' + LineEnding +
            '<html>' + LineEnding +
            '<head><title>' + HTMLEscape(Title) + '</title></head>' + LineEnding +
            '<body>' + LineEnding +
            '  <h1>' + HTMLEscape(Title) + '</h1>' + LineEnding +
            '  <p>' + HTMLEscape(UserContent) + '</p>' + LineEnding +
            '</body></html>';
end;
```

**Important :** Ne jamais insérer directement du contenu utilisateur sans échappement !

## 9.5.3 Système de templates simple

### Principe des marqueurs de substitution

Un système de template basique utilise des marqueurs qui seront remplacés par du contenu dynamique :

```pascal
function SimpleTemplate(const Template: string;
                        const Values: array of string): string;
var
  i: Integer;
  Marker: string;
begin
  Result := Template;
  for i := Low(Values) to High(Values) do
  begin
    Marker := '{' + IntToStr(i) + '}';
    Result := StringReplace(Result, Marker, Values[i], [rfReplaceAll]);
  end;
end;

// Utilisation
var
  Template: string;
  Page: string;
begin
  Template := '<!DOCTYPE html>' + LineEnding +
              '<html>' + LineEnding +
              '<head><title>{0}</title></head>' + LineEnding +
              '<body>' + LineEnding +
              '  <h1>{0}</h1>' + LineEnding +
              '  <p>{1}</p>' + LineEnding +
              '</body></html>';

  Page := SimpleTemplate(Template, ['Mon titre', 'Mon contenu']);
end;
```

### Templates avec marqueurs nommés

Une approche plus lisible utilise des noms plutôt que des numéros :

```pascal
function NamedTemplate(const Template: string;
                       const Pairs: array of string): string;
var
  i: Integer;
begin
  Result := Template;
  // Les paires sont : ['nom1', 'valeur1', 'nom2', 'valeur2', ...]
  i := Low(Pairs);
  while i <= High(Pairs) - 1 do
  begin
    Result := StringReplace(Result, '{{' + Pairs[i] + '}}',
                           Pairs[i + 1], [rfReplaceAll]);
    Inc(i, 2);
  end;
end;

// Utilisation
var
  Template: string;
  Page: string;
begin
  Template := '<!DOCTYPE html>' + LineEnding +
              '<html>' + LineEnding +
              '<head><title>{{title}}</title></head>' + LineEnding +
              '<body>' + LineEnding +
              '  <h1>{{heading}}</h1>' + LineEnding +
              '  <p>{{content}}</p>' + LineEnding +
              '</body></html>';

  Page := NamedTemplate(Template, [
    'title', 'Ma page',
    'heading', 'Bienvenue',
    'content', 'Ceci est le contenu de ma page.'
  ]);
end;
```

## 9.5.4 Templates depuis fichiers externes

### Chargement de fichiers template

Séparer les templates du code améliore grandement la maintenabilité :

```pascal
uses
  Classes, SysUtils;

type
  TTemplateEngine = class
  private
    FTemplateDir: string;
  public
    constructor Create(const TemplateDirectory: string);
    function LoadTemplate(const FileName: string): string;
    function Render(const TemplateName: string;
                    const Values: array of string): string;
  end;

constructor TTemplateEngine.Create(const TemplateDirectory: string);  
begin
  inherited Create;
  FTemplateDir := IncludeTrailingPathDelimiter(TemplateDirectory);
end;

function TTemplateEngine.LoadTemplate(const FileName: string): string;  
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  FileStream := TFileStream.Create(FTemplateDir + FileName, fmOpenRead);
  try
    StringStream := TStringStream.Create('');
    try
      StringStream.CopyFrom(FileStream, 0);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

function TTemplateEngine.Render(const TemplateName: string;
                                const Values: array of string): string;
var
  Template: string;
  i: Integer;
  Marker: string;
begin
  Template := LoadTemplate(TemplateName);
  Result := Template;

  for i := Low(Values) to High(Values) do
  begin
    Marker := '{{' + IntToStr(i) + '}}';
    Result := StringReplace(Result, Marker, Values[i], [rfReplaceAll]);
  end;
end;
```

**Exemple de fichier template** (`page.html`) :

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>{{0}}</title>
    <link rel="stylesheet" href="/css/style.css">
</head>
<body>
    <header>
        <h1>{{1}}</h1>
    </header>
    <main>
        <article>
            {{2}}
        </article>
    </main>
    <footer>
        <p>{{3}}</p>
    </footer>
</body>
</html>
```

**Utilisation :**

```pascal
var
  Engine: TTemplateEngine;
  HTML: string;
begin
  Engine := TTemplateEngine.Create('./templates/');
  try
    HTML := Engine.Render('page.html', [
      'Mon Site Web',
      'Bienvenue sur mon site',
      '<p>Voici le contenu principal de la page.</p>',
      '© 2025 - Tous droits réservés'
    ]);
    WriteLn(HTML);
  finally
    Engine.Free;
  end;
end;
```

## 9.5.5 Templates avec structures conditionnelles

### Implémentation de blocs conditionnels

Un moteur de template plus avancé peut gérer des conditions :

```pascal
type
  TAdvancedTemplate = class
  private
    function ProcessConditionals(const Template: string;
                                 const Conditions: array of Boolean): string;
  public
    function Render(const Template: string;
                    const Values: array of string;
                    const Conditions: array of Boolean): string;
  end;

function TAdvancedTemplate.ProcessConditionals(const Template: string;
                                               const Conditions: array of Boolean): string;
var
  StartPos, EndPos: Integer;
  CondIndex: Integer;
  BlockContent: string;
begin
  Result := Template;
  CondIndex := 0;

  // Traite les blocs {{#if_0}} ... {{/if_0}}
  while CondIndex <= High(Conditions) do
  begin
    StartPos := Pos('{{#if_' + IntToStr(CondIndex) + '}}', Result);
    if StartPos > 0 then
    begin
      EndPos := Pos('{{/if_' + IntToStr(CondIndex) + '}}', Result);
      if EndPos > StartPos then
      begin
        BlockContent := Copy(Result,
                            StartPos + Length('{{#if_' + IntToStr(CondIndex) + '}}'),
                            EndPos - StartPos - Length('{{#if_' + IntToStr(CondIndex) + '}}'));

        // Si condition vraie, garde le contenu, sinon supprime
        if Conditions[CondIndex] then
          Result := StringReplace(Result,
                                 '{{#if_' + IntToStr(CondIndex) + '}}' +
                                 BlockContent +
                                 '{{/if_' + IntToStr(CondIndex) + '}}',
                                 BlockContent, [])
        else
          Result := StringReplace(Result,
                                 '{{#if_' + IntToStr(CondIndex) + '}}' +
                                 BlockContent +
                                 '{{/if_' + IntToStr(CondIndex) + '}}',
                                 '', []);
      end;
    end;
    Inc(CondIndex);
  end;
end;
```

**Exemple de template avec conditions :**

```html
<!DOCTYPE html>
<html>
<head>
    <title>{{0}}</title>
</head>
<body>
    <h1>{{1}}</h1>

    {{#if_0}}
    <div class="logged-in">
        <p>Bienvenue, {{2}} !</p>
        <a href="/logout">Se déconnecter</a>
    </div>
    {{/if_0}}

    {{#if_1}}
    <div class="alert">
        <p>{{3}}</p>
    </div>
    {{/if_1}}

    <main>{{4}}</main>
</body>
</html>
```

## 9.5.6 Templates avec boucles

### Génération de listes dynamiques

Pour afficher des collections de données, on utilise des boucles :

```pascal
type
  TTemplateItem = record
    Key: string;
    Value: string;
  end;
  TTemplateItems = array of TTemplateItem;

function RenderList(const ItemTemplate: string;
                    const Items: TTemplateItems): string;
var
  i, j: Integer;
  ItemHTML: string;
begin
  Result := '';
  for i := Low(Items) to High(Items) do
  begin
    ItemHTML := ItemTemplate;
    // Remplace tous les marqueurs dans le template d'item
    for j := Low(Items[i]) to High(Items[i]) do
    begin
      ItemHTML := StringReplace(ItemHTML,
                                '{{' + Items[i][j].Key + '}}',
                                Items[i][j].Value,
                                [rfReplaceAll]);
    end;
    Result := Result + ItemHTML;
  end;
end;
```

**Exemple d'utilisation :**

```pascal
var
  Products: TTemplateItems;
  ItemTemplate: string;
  ProductsHTML: string;
begin
  ItemTemplate := '<div class="product">' + LineEnding +
                  '  <h3>{{name}}</h3>' + LineEnding +
                  '  <p>{{description}}</p>' + LineEnding +
                  '  <span class="price">{{price}}</span>' + LineEnding +
                  '</div>' + LineEnding;

  SetLength(Products, 3);

  Products[0].Key := 'name';
  Products[0].Value := 'Ordinateur portable';
  // ... définir description et price

  ProductsHTML := RenderList(ItemTemplate, Products);
end;
```

## 9.5.7 fpTemplate - Système de templates intégré

FreePascal inclut la bibliothèque `fptemplate` dans le package fcl-web :

```pascal
uses
  fpTemplate;

var
  Template: TTemplateParser;
  HTML: string;
begin
  Template := TTemplateParser.Create;
  try
    // Définir le template
    Template.Template :=
      '<!DOCTYPE html>' + LineEnding +
      '<html>' + LineEnding +
      '<head><title>{#title}</title></head>' + LineEnding +
      '<body>' + LineEnding +
      '  <h1>{#heading}</h1>' + LineEnding +
      '  <p>{#content}</p>' + LineEnding +
      '</body></html>';

    // Définir les valeurs
    Template.Values['title'] := 'Ma page';
    Template.Values['heading'] := 'Titre principal';
    Template.Values['content'] := 'Le contenu de ma page';

    // Obtenir le résultat
    HTML := Template.ParseTemplate;
    WriteLn(HTML);
  finally
    Template.Free;
  end;
end;
```

**Fonctionnalités de fpTemplate :**

- Substitution de variables : `{#variable}`
- Inclusion de fichiers : `{+fichier.html}`
- Blocs répétitifs avec `StartLoop` et `EndLoop`
- Conditions simples

## 9.5.8 Mustache et systèmes de templates modernes

### Le format Mustache

Mustache est un format de template populaire, disponible pour FreePascal via des packages tiers :

**Syntaxe Mustache :**

```mustache
<!DOCTYPE html>
<html>
<head>
    <title>{{title}}</title>
</head>
<body>
    <h1>{{heading}}</h1>

    {{#isLoggedIn}}
    <p>Bienvenue, {{username}} !</p>
    {{/isLoggedIn}}

    {{^isLoggedIn}}
    <p><a href="/login">Se connecter</a></p>
    {{/isLoggedIn}}

    <ul>
    {{#products}}
        <li>{{name}} - {{price}}€</li>
    {{/products}}
    </ul>
</body>
</html>
```

**Caractéristiques :**
- `{{variable}}` : substitution simple
- `{{#section}}...{{/section}}` : section (condition ou boucle)
- `{{^section}}...{{/section}}` : section inversée (si faux)
- `{{!commentaire}}` : commentaire
- `{{{html}}}` : HTML non échappé

## 9.5.9 Intégration avec fpWeb

### Utilisation dans un module web

```pascal
unit MyWebModule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, fpTemplate;

type
  TMyWebModule = class(TFPWebModule)
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);
  private
    FTemplateEngine: TTemplateParser;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

constructor TMyWebModule.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FTemplateEngine := TTemplateParser.Create;
end;

destructor TMyWebModule.Destroy;  
begin
  FTemplateEngine.Free;
  inherited Destroy;
end;

procedure TMyWebModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);  
var
  TemplatePath: string;
begin
  // Chemin cross-platform
  {$IFDEF WINDOWS}
  TemplatePath := 'C:\templates\page.html';
  {$ELSE}
  TemplatePath := '/var/www/templates/page.html';
  {$ENDIF}

  // Charger le template
  FTemplateEngine.Template := LoadFileToString(TemplatePath);

  // Définir les valeurs
  FTemplateEngine.Values['title'] := 'Ma Page Web';
  FTemplateEngine.Values['content'] := 'Contenu dynamique généré par FreePascal';

  // Rendre et envoyer la réponse
  AResponse.Content := FTemplateEngine.ParseTemplate;
  AResponse.ContentType := 'text/html; charset=utf-8';
  AResponse.SendResponse;
end;

end.
```

## 9.5.10 Mise en cache des templates

### Optimisation des performances

Pour éviter de recharger les templates à chaque requête :

```pascal
type
  TTemplateCache = class
  private
    FCache: TStringList;
    FTemplateDir: string;
  public
    constructor Create(const TemplateDirectory: string);
    destructor Destroy; override;
    function GetTemplate(const FileName: string): string;
    procedure ClearCache;
  end;

constructor TTemplateCache.Create(const TemplateDirectory: string);  
begin
  inherited Create;
  FTemplateDir := IncludeTrailingPathDelimiter(TemplateDirectory);
  FCache := TStringList.Create;
  FCache.Sorted := True;
end;

destructor TTemplateCache.Destroy;  
begin
  FCache.Free;
  inherited Destroy;
end;

function TTemplateCache.GetTemplate(const FileName: string): string;  
var
  Index: Integer;
  Content: string;
begin
  // Vérifie si le template est en cache
  if FCache.Find(FileName, Index) then
    Result := FCache.ValueFromIndex[Index]
  else
  begin
    // Charge le fichier et le met en cache
    Content := LoadFileToString(FTemplateDir + FileName);
    FCache.Add(FileName + '=' + Content);
    Result := Content;
  end;
end;

procedure TTemplateCache.ClearCache;  
begin
  FCache.Clear;
end;
```

## 9.5.11 Bonnes pratiques

### Organisation des templates

**Structure de répertoires recommandée :**

```
/templates
  /layouts
    base.html
    admin.html
  /partials
    header.html
    footer.html
    menu.html
  /pages
    home.html
    about.html
    contact.html
```

### Séparation des responsabilités

1. **Templates** : uniquement la structure HTML
2. **Contrôleurs** : logique métier et préparation des données
3. **Modèles** : accès aux données
4. **CSS/JS** : fichiers séparés, pas de code inline

### Sécurité

- **Toujours échapper** le contenu utilisateur
- **Valider** les données avant l'insertion
- **Limiter** les inclusions de fichiers aux répertoires autorisés
- **Éviter** d'exposer la structure interne via les erreurs

### Performance

- **Mettre en cache** les templates compilés
- **Minimiser** le nombre d'inclusions de fichiers
- **Précompiler** les templates en production
- **Utiliser** la compression gzip pour le HTML final

## 9.5.13 Différences multi-plateformes

### Gestion des chemins

```pascal
function GetTemplatePath(const FileName: string): string;  
begin
  {$IFDEF WINDOWS}
  Result := 'C:\inetpub\wwwroot\templates\' + FileName;
  {$ELSE}
  Result := '/var/www/html/templates/' + FileName;
  {$ENDIF}
end;
```

### Encodage des fichiers

Sur Linux/Ubuntu, les fichiers sont généralement en UTF-8, tandis que Windows peut utiliser différents encodages. Il est recommandé d'utiliser UTF-8 partout :

```pascal
function LoadTemplateUTF8(const FileName: string): string;  
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    StringStream := TStringStream.Create('', TEncoding.UTF8);
    try
      StringStream.CopyFrom(FileStream, 0);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;
```

## Conclusion

Les systèmes de templates sont essentiels pour créer des applications web maintenables. FreePascal offre plusieurs approches, depuis la simple concaténation jusqu'aux moteurs de templates sophistiqués. Le choix dépend de la complexité du projet et des besoins spécifiques.

**Points clés à retenir :**
- Toujours échapper le contenu utilisateur (sécurité XSS)
- Séparer la présentation de la logique métier
- Utiliser des fichiers externes pour les templates
- Mettre en cache les templates en production
- Adopter une organisation claire des fichiers templates

⏭️ [Sessions et authentification](/09-programmation-web-freepascal/06-sessions-authentification.md)
