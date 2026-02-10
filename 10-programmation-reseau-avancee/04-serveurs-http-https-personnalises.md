🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.4 Serveurs HTTP/HTTPS personnalisés

## Introduction

HTTP (HyperText Transfer Protocol) est le protocole fondamental du Web. Créer votre propre serveur HTTP vous permet de comprendre en profondeur son fonctionnement et de construire des applications web personnalisées. Dans ce chapitre, nous allons créer des serveurs HTTP de plus en plus sophistiqués.

### Pourquoi créer un serveur HTTP personnalisé ?

- **Apprentissage** : Comprendre le fonctionnement interne d'HTTP
- **Contrôle total** : Personnaliser chaque aspect du comportement
- **Légèreté** : Serveur minimal sans dépendances lourdes
- **Intégration** : Embarquer un serveur dans votre application
- **API REST** : Créer des services web personnalisés
- **Microservices** : Applications distribuées légères

### Qu'est-ce que HTTP ?

HTTP est un protocole **texte** basé sur un modèle **requête/réponse** :

```
Client                          Serveur
  |                                |
  |--- Requête HTTP -------------->|
  |                                |
  |<-- Réponse HTTP ---------------|
  |                                |
```

Une requête HTTP ressemble à ceci :

```http
GET /index.html HTTP/1.1  
Host: www.example.com  
User-Agent: Mozilla/5.0  
Accept: text/html  
Connection: keep-alive

```

Une réponse HTTP ressemble à ceci :

```http
HTTP/1.1 200 OK  
Content-Type: text/html  
Content-Length: 138  
Date: Mon, 27 Jul 2024 12:28:53 GMT

<!DOCTYPE html>
<html>
<head><title>Page d'accueil</title></head>
<body><h1>Bienvenue!</h1></body>
</html>
```

## Serveur HTTP minimal

Commençons par le serveur HTTP le plus simple possible :

```pascal
program MinimalHTTPServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes, StrUtils, Sockets;

function ParseHTTPRequest(const Request: string; out Method, Path, Version: string): Boolean;  
var
  FirstLine: string;
  SpacePos1, SpacePos2: Integer;
begin
  Result := False;

  // Extraction de la première ligne
  SpacePos1 := Pos(' ', Request);
  if SpacePos1 = 0 then Exit;

  Method := Copy(Request, 1, SpacePos1 - 1);

  SpacePos2 := PosEx(' ', Request, SpacePos1 + 1);
  if SpacePos2 = 0 then Exit;

  Path := Copy(Request, SpacePos1 + 1, SpacePos2 - SpacePos1 - 1);

  // Version HTTP
  Version := Copy(Request, SpacePos2 + 1,
                  Pos(#13#10, Request) - SpacePos2 - 1);

  Result := True;
end;

procedure HandleClient(ClientSocket: TSocket);  
var
  Request: string;
  Response: string;
  Buffer: array[0..4095] of Char;
  Received: Integer;
  Method, Path, Version: string;
begin
  // Réception de la requête
  Received := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer) - 1, 0);

  if Received > 0 then
  begin
    Buffer[Received] := #0;
    Request := Buffer;

    WriteLn('=== Requête reçue ===');
    WriteLn(Request);
    WriteLn('=====================');

    // Parsing de la requête
    if ParseHTTPRequest(Request, Method, Path, Version) then
    begin
      WriteLn('Méthode: ', Method);
      WriteLn('Chemin: ', Path);
      WriteLn('Version: ', Version);

      // Construction de la réponse
      Response :=
        'HTTP/1.1 200 OK'#13#10 +
        'Content-Type: text/html; charset=utf-8'#13#10 +
        'Connection: close'#13#10 +
        #13#10 +
        '<!DOCTYPE html>'#13#10 +
        '<html>'#13#10 +
        '<head><title>Mon Serveur HTTP</title></head>'#13#10 +
        '<body>'#13#10 +
        '<h1>Bienvenue sur mon serveur HTTP !</h1>'#13#10 +
        '<p>Vous avez demandé : ' + Path + '</p>'#13#10 +
        '<p>Méthode : ' + Method + '</p>'#13#10 +
        '</body>'#13#10 +
        '</html>'#13#10;

      // Envoi de la réponse
      fpSend(ClientSocket, @Response[1], Length(Response), 0);
    end;
  end;

  CloseSocket(ClientSocket);
end;

var
  ServerSocket, ClientSocket: TSocket;
  ServerAddr, ClientAddr: TInetSockAddr;
  AddrLen: TSockLen;
  OptVal: Integer;

begin
  // Création du socket serveur
  ServerSocket := fpSocket(AF_INET, SOCK_STREAM, 0);

  if ServerSocket = INVALID_SOCKET then
  begin
    WriteLn('Erreur création socket');
    Exit;
  end;

  try
    // Option SO_REUSEADDR
    OptVal := 1;
    fpSetSockOpt(ServerSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

    // Liaison au port 8080
    FillChar(ServerAddr, SizeOf(ServerAddr), 0);
    ServerAddr.sin_family := AF_INET;
    ServerAddr.sin_port := htons(8080);
    ServerAddr.sin_addr.s_addr := INADDR_ANY;

    if fpBind(ServerSocket, @ServerAddr, SizeOf(ServerAddr)) < 0 then
    begin
      WriteLn('Erreur bind sur le port 8080');
      Exit;
    end;

    // Mise en écoute
    if fpListen(ServerSocket, 5) < 0 then
    begin
      WriteLn('Erreur listen');
      Exit;
    end;

    WriteLn('=================================');
    WriteLn('Serveur HTTP démarré');
    WriteLn('URL: http://localhost:8080');
    WriteLn('Appuyez sur Ctrl+C pour arrêter');
    WriteLn('=================================');
    WriteLn;

    // Boucle d'acceptation
    while True do
    begin
      AddrLen := SizeOf(ClientAddr);
      ClientSocket := fpAccept(ServerSocket, @ClientAddr, @AddrLen);

      if ClientSocket >= 0 then
      begin
        WriteLn('Connexion depuis ', NetAddrToStr(ClientAddr.sin_addr));
        HandleClient(ClientSocket);
        WriteLn;
      end;
    end;

  finally
    CloseSocket(ServerSocket);
  end;
end.
```

### Test du serveur

Compilez et lancez le programme, puis ouvrez votre navigateur à l'adresse `http://localhost:8080`

## Parsing HTTP complet

Créons une classe pour parser les requêtes HTTP de manière complète :

```pascal
unit HTTPParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  THTTPMethod = (hmUnknown, hmGET, hmPOST, hmPUT, hmDELETE, hmHEAD, hmOPTIONS);

  THTTPHeaders = class
  private
    FHeaders: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Name, Value: string);
    function Get(const Name: string): string;
    function Has(const Name: string): Boolean;
    procedure Clear;
    property Headers: TStringList read FHeaders;
  end;

  THTTPRequest = class
  private
    FMethod: THTTPMethod;
    FMethodStr: string;
    FPath: string;
    FQueryString: string;
    FVersion: string;
    FHeaders: THTTPHeaders;
    FBody: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Parse(const RawRequest: string): Boolean;
    property Method: THTTPMethod read FMethod;
    property MethodStr: string read FMethodStr;
    property Path: string read FPath;
    property QueryString: string read FQueryString;
    property Version: string read FVersion;
    property Headers: THTTPHeaders read FHeaders;
    property Body: string read FBody;
  end;

implementation

{ THTTPHeaders }

constructor THTTPHeaders.Create;  
begin
  FHeaders := TStringList.Create;
  FHeaders.NameValueSeparator := ':';
end;

destructor THTTPHeaders.Destroy;  
begin
  FHeaders.Free;
  inherited;
end;

procedure THTTPHeaders.Add(const Name, Value: string);  
begin
  FHeaders.Add(Name + ':' + Trim(Value));
end;

function THTTPHeaders.Get(const Name: string): string;  
var
  Index: Integer;
begin
  Index := FHeaders.IndexOfName(Name);
  if Index >= 0 then
    Result := Trim(FHeaders.ValueFromIndex[Index])
  else
    Result := '';
end;

function THTTPHeaders.Has(const Name: string): Boolean;  
begin
  Result := FHeaders.IndexOfName(Name) >= 0;
end;

procedure THTTPHeaders.Clear;  
begin
  FHeaders.Clear;
end;

{ THTTPRequest }

constructor THTTPRequest.Create;  
begin
  FHeaders := THTTPHeaders.Create;
  FMethod := hmUnknown;
end;

destructor THTTPRequest.Destroy;  
begin
  FHeaders.Free;
  inherited;
end;

function THTTPRequest.Parse(const RawRequest: string): Boolean;  
var
  Lines: TStringList;
  i, QueryPos: Integer;
  Line, HeaderName, HeaderValue: string;
  ColonPos: Integer;
  BodyStarted: Boolean;
begin
  Result := False;
  Lines := TStringList.Create;
  try
    Lines.Text := RawRequest;

    if Lines.Count = 0 then Exit;

    // Parse première ligne (Request Line)
    Line := Lines[0];

    // Méthode
    i := Pos(' ', Line);
    if i = 0 then Exit;
    FMethodStr := Copy(Line, 1, i - 1);

    // Conversion en enum
    if FMethodStr = 'GET' then FMethod := hmGET
    else if FMethodStr = 'POST' then FMethod := hmPOST
    else if FMethodStr = 'PUT' then FMethod := hmPUT
    else if FMethodStr = 'DELETE' then FMethod := hmDELETE
    else if FMethodStr = 'HEAD' then FMethod := hmHEAD
    else if FMethodStr = 'OPTIONS' then FMethod := hmOPTIONS
    else FMethod := hmUnknown;

    Delete(Line, 1, i);
    Line := TrimLeft(Line);

    // Path et QueryString
    i := Pos(' ', Line);
    if i = 0 then Exit;
    FPath := Copy(Line, 1, i - 1);

    // Séparation path et query string
    QueryPos := Pos('?', FPath);
    if QueryPos > 0 then
    begin
      FQueryString := Copy(FPath, QueryPos + 1, Length(FPath));
      FPath := Copy(FPath, 1, QueryPos - 1);
    end
    else
      FQueryString := '';

    // Version HTTP
    Delete(Line, 1, i);
    FVersion := Trim(Line);

    // Parse des headers
    BodyStarted := False;
    FBody := '';

    for i := 1 to Lines.Count - 1 do
    begin
      Line := Lines[i];

      if not BodyStarted then
      begin
        if Trim(Line) = '' then
        begin
          BodyStarted := True;
          Continue;
        end;

        // Parse header
        ColonPos := Pos(':', Line);
        if ColonPos > 0 then
        begin
          HeaderName := Copy(Line, 1, ColonPos - 1);
          HeaderValue := Copy(Line, ColonPos + 1, Length(Line));
          FHeaders.Add(HeaderName, HeaderValue);
        end;
      end
      else
      begin
        // Corps de la requête
        if FBody <> '' then
          FBody := FBody + #13#10;
        FBody := FBody + Line;
      end;
    end;

    Result := True;

  finally
    Lines.Free;
  end;
end;

end.
```

## Serveur HTTP avec routing

Créons maintenant un serveur avec un système de routing :

```pascal
program HTTPServerWithRouting;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Sockets, HTTPParser;

type
  TRouteHandler = procedure(Request: THTTPRequest; out Response: string);

  TRoute = record
    Method: THTTPMethod;
    Path: string;
    Handler: TRouteHandler;
  end;

  THTTPServer = class
  private
    FPort: Word;
    FSocket: TSocket;
    FRoutes: array of TRoute;
    procedure HandleClient(ClientSocket: TSocket);
    function FindRoute(Method: THTTPMethod; const Path: string): Integer;
  public
    constructor Create(APort: Word);
    destructor Destroy; override;
    procedure AddRoute(Method: THTTPMethod; const Path: string; Handler: TRouteHandler);
    procedure Start;
    procedure Stop;
  end;

{ THTTPServer }

constructor THTTPServer.Create(APort: Word);  
begin
  FPort := APort;
  FSocket := INVALID_SOCKET;
  SetLength(FRoutes, 0);
end;

destructor THTTPServer.Destroy;  
begin
  Stop;
  inherited;
end;

procedure THTTPServer.AddRoute(Method: THTTPMethod; const Path: string;
                               Handler: TRouteHandler);
var
  Index: Integer;
begin
  Index := Length(FRoutes);
  SetLength(FRoutes, Index + 1);
  FRoutes[Index].Method := Method;
  FRoutes[Index].Path := Path;
  FRoutes[Index].Handler := Handler;

  WriteLn('Route ajoutée: ', Path);
end;

function THTTPServer.FindRoute(Method: THTTPMethod; const Path: string): Integer;  
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to High(FRoutes) do
  begin
    if (FRoutes[i].Method = Method) and (FRoutes[i].Path = Path) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure THTTPServer.HandleClient(ClientSocket: TSocket);  
var
  Buffer: array[0..8191] of Char;
  Received: Integer;
  Request: THTTPRequest;
  Response, HTTPResponse: string;
  RouteIndex: Integer;
begin
  // Réception de la requête
  Received := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer) - 1, 0);

  if Received > 0 then
  begin
    Buffer[Received] := #0;

    Request := THTTPRequest.Create;
    try
      if Request.Parse(Buffer) then
      begin
        WriteLn('Requête: ', Request.MethodStr, ' ', Request.Path);

        // Recherche de la route
        RouteIndex := FindRoute(Request.Method, Request.Path);

        if RouteIndex >= 0 then
        begin
          // Exécution du handler
          FRoutes[RouteIndex].Handler(Request, Response);

          // Construction de la réponse HTTP
          HTTPResponse :=
            'HTTP/1.1 200 OK'#13#10 +
            'Content-Type: text/html; charset=utf-8'#13#10 +
            'Content-Length: ' + IntToStr(Length(Response)) + #13#10 +
            'Connection: close'#13#10 +
            #13#10 +
            Response;
        end
        else
        begin
          // 404 Not Found
          Response :=
            '<!DOCTYPE html>'#13#10 +
            '<html><head><title>404 Not Found</title></head>'#13#10 +
            '<body><h1>404 - Page Not Found</h1>'#13#10 +
            '<p>La page ' + Request.Path + ' n''existe pas.</p>'#13#10 +
            '</body></html>';

          HTTPResponse :=
            'HTTP/1.1 404 Not Found'#13#10 +
            'Content-Type: text/html; charset=utf-8'#13#10 +
            'Content-Length: ' + IntToStr(Length(Response)) + #13#10 +
            'Connection: close'#13#10 +
            #13#10 +
            Response;
        end;

        // Envoi de la réponse
        fpSend(ClientSocket, @HTTPResponse[1], Length(HTTPResponse), 0);
      end;

    finally
      Request.Free;
    end;
  end;

  CloseSocket(ClientSocket);
end;

procedure THTTPServer.Start;  
var
  ServerAddr, ClientAddr: TInetSockAddr;
  ClientSocket: TSocket;
  AddrLen: TSockLen;
  OptVal: Integer;
begin
  // Création du socket
  FSocket := fpSocket(AF_INET, SOCK_STREAM, 0);

  if FSocket = INVALID_SOCKET then
    raise Exception.Create('Impossible de créer le socket');

  // Configuration
  OptVal := 1;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

  // Liaison
  FillChar(ServerAddr, SizeOf(ServerAddr), 0);
  ServerAddr.sin_family := AF_INET;
  ServerAddr.sin_port := htons(FPort);
  ServerAddr.sin_addr.s_addr := INADDR_ANY;

  if fpBind(FSocket, @ServerAddr, SizeOf(ServerAddr)) < 0 then
    raise Exception.Create('Impossible de lier le port ' + IntToStr(FPort));

  // Écoute
  if fpListen(FSocket, 10) < 0 then
    raise Exception.Create('Impossible de se mettre en écoute');

  WriteLn('===================================');
  WriteLn('Serveur HTTP démarré');
  WriteLn('Port: ', FPort);
  WriteLn('URL: http://localhost:', FPort);
  WriteLn('Appuyez sur Ctrl+C pour arrêter');
  WriteLn('===================================');
  WriteLn;

  // Boucle d'acceptation
  while True do
  begin
    AddrLen := SizeOf(ClientAddr);
    ClientSocket := fpAccept(FSocket, @ClientAddr, @AddrLen);

    if ClientSocket >= 0 then
    begin
      HandleClient(ClientSocket);
    end;
  end;
end;

procedure THTTPServer.Stop;  
begin
  if FSocket <> INVALID_SOCKET then
  begin
    CloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
end;

// ========== Handlers ==========

procedure HandleHome(Request: THTTPRequest; out Response: string);  
begin
  Response :=
    '<!DOCTYPE html>'#13#10 +
    '<html>'#13#10 +
    '<head><title>Accueil</title></head>'#13#10 +
    '<body>'#13#10 +
    '<h1>Bienvenue sur le serveur HTTP !</h1>'#13#10 +
    '<ul>'#13#10 +
    '<li><a href="/">Accueil</a></li>'#13#10 +
    '<li><a href="/about">À propos</a></li>'#13#10 +
    '<li><a href="/contact">Contact</a></li>'#13#10 +
    '<li><a href="/api/time">API - Heure</a></li>'#13#10 +
    '</ul>'#13#10 +
    '</body>'#13#10 +
    '</html>';
end;

procedure HandleAbout(Request: THTTPRequest; out Response: string);  
begin
  Response :=
    '<!DOCTYPE html>'#13#10 +
    '<html>'#13#10 +
    '<head><title>À propos</title></head>'#13#10 +
    '<body>'#13#10 +
    '<h1>À propos</h1>'#13#10 +
    '<p>Serveur HTTP personnalisé créé avec FreePascal/Lazarus</p>'#13#10 +
    '<p><a href="/">Retour à l''accueil</a></p>'#13#10 +
    '</body>'#13#10 +
    '</html>';
end;

procedure HandleContact(Request: THTTPRequest; out Response: string);  
begin
  Response :=
    '<!DOCTYPE html>'#13#10 +
    '<html>'#13#10 +
    '<head><title>Contact</title></head>'#13#10 +
    '<body>'#13#10 +
    '<h1>Contact</h1>'#13#10 +
    '<p>Email: contact@example.com</p>'#13#10 +
    '<p><a href="/">Retour à l''accueil</a></p>'#13#10 +
    '</body>'#13#10 +
    '</html>';
end;

procedure HandleAPITime(Request: THTTPRequest; out Response: string);  
begin
  Response :=
    '{"time":"' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '",' +
    '"timestamp":' + IntToStr(DateTimeToUnix(Now)) + '}';
end;

var
  Server: THTTPServer;

begin
  Server := THTTPServer.Create(8080);
  try
    // Configuration des routes
    Server.AddRoute(hmGET, '/', @HandleHome);
    Server.AddRoute(hmGET, '/about', @HandleAbout);
    Server.AddRoute(hmGET, '/contact', @HandleContact);
    Server.AddRoute(hmGET, '/api/time', @HandleAPITime);

    WriteLn;

    // Démarrage du serveur
    Server.Start;

  finally
    Server.Free;
  end;
end.
```

## Serveur de fichiers statiques

Ajoutons la capacité de servir des fichiers statiques :

```pascal
unit HTTPFileServer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  TFileServer = class
  private
    FRootDirectory: string;
    function GetContentType(const FileName: string): string;
    function FileExistsInRoot(const Path: string): Boolean;
    function GetFullPath(const Path: string): string;
    function IsPathSafe(const Path: string): Boolean;
  public
    constructor Create(const ARootDirectory: string);
    function ServeFile(const Path: string; out Content: string;
                       out ContentType: string): Boolean;
    function GenerateDirectoryListing(const Path: string): string;
    property RootDirectory: string read FRootDirectory;
  end;

implementation

uses
  StrUtils;

{ TFileServer }

constructor TFileServer.Create(const ARootDirectory: string);  
begin
  FRootDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ARootDirectory));

  if not DirectoryExists(FRootDirectory) then
    raise Exception.Create('Répertoire inexistant: ' + FRootDirectory);
end;

function TFileServer.GetContentType(const FileName: string): string;  
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));

  // case sur des chaînes n'est pas supporté en FreePascal
  if (Ext = '.html') or (Ext = '.htm') then Result := 'text/html'
  else if Ext = '.css' then Result := 'text/css'
  else if Ext = '.js' then Result := 'application/javascript'
  else if Ext = '.json' then Result := 'application/json'
  else if Ext = '.xml' then Result := 'application/xml'
  else if Ext = '.txt' then Result := 'text/plain'
  else if Ext = '.csv' then Result := 'text/csv'
  else if (Ext = '.jpg') or (Ext = '.jpeg') then Result := 'image/jpeg'
  else if Ext = '.png' then Result := 'image/png'
  else if Ext = '.gif' then Result := 'image/gif'
  else if Ext = '.bmp' then Result := 'image/bmp'
  else if Ext = '.ico' then Result := 'image/x-icon'
  else if Ext = '.svg' then Result := 'image/svg+xml'
  else if Ext = '.webp' then Result := 'image/webp'
  else if Ext = '.pdf' then Result := 'application/pdf'
  else if Ext = '.zip' then Result := 'application/zip'
  else if Ext = '.rar' then Result := 'application/x-rar-compressed'
  else if Ext = '.tar' then Result := 'application/x-tar'
  else if Ext = '.gz' then Result := 'application/gzip'
  else if Ext = '.mp3' then Result := 'audio/mpeg'
  else if Ext = '.wav' then Result := 'audio/wav'
  else if Ext = '.ogg' then Result := 'audio/ogg'
  else if Ext = '.mp4' then Result := 'video/mp4'
  else if Ext = '.avi' then Result := 'video/x-msvideo'
  else if Ext = '.webm' then Result := 'video/webm'
  else if Ext = '.woff' then Result := 'font/woff'
  else if Ext = '.woff2' then Result := 'font/woff2'
  else if Ext = '.ttf' then Result := 'font/ttf'
  else if Ext = '.otf' then Result := 'font/otf'
  else Result := 'application/octet-stream';
end;

function TFileServer.IsPathSafe(const Path: string): Boolean;  
begin
  // Empêcher les attaques de type "directory traversal"
  Result := (Pos('..', Path) = 0) and
            (Pos('~', Path) = 0) and
            (Copy(Path, 1, 1) <> '/');
end;

function TFileServer.GetFullPath(const Path: string): string;  
var
  CleanPath: string;
begin
  // Nettoyage du chemin
  CleanPath := StringReplace(Path, '/', PathDelim, [rfReplaceAll]);

  // Suppression du / initial
  if Copy(CleanPath, 1, 1) = PathDelim then
    Delete(CleanPath, 1, 1);

  Result := FRootDirectory + CleanPath;
end;

function TFileServer.FileExistsInRoot(const Path: string): Boolean;  
var
  FullPath: string;
begin
  if not IsPathSafe(Path) then
  begin
    Result := False;
    Exit;
  end;

  FullPath := GetFullPath(Path);
  Result := FileExists(FullPath) and (Copy(FullPath, 1, Length(FRootDirectory)) = FRootDirectory);
end;

function TFileServer.ServeFile(const Path: string; out Content: string;
                               out ContentType: string): Boolean;
var
  FullPath: string;
  FileStream: TFileStream;
  Buffer: array of Byte;
begin
  Result := False;
  Content := '';
  ContentType := '';

  if not IsPathSafe(Path) then
    Exit;

  FullPath := GetFullPath(Path);

  // Vérification de sécurité
  if Copy(FullPath, 1, Length(FRootDirectory)) <> FRootDirectory then
    Exit;

  if not FileExists(FullPath) then
    Exit;

  try
    // Lecture du fichier
    FileStream := TFileStream.Create(FullPath, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(Buffer, FileStream.Size);
      FileStream.Read(Buffer[0], FileStream.Size);

      // Conversion en string
      SetLength(Content, FileStream.Size);
      if FileStream.Size > 0 then
        Move(Buffer[0], Content[1], FileStream.Size);

      ContentType := GetContentType(FullPath);
      Result := True;

    finally
      FileStream.Free;
    end;

  except
    on E: Exception do
      WriteLn('Erreur lecture fichier: ', E.Message);
  end;
end;

function TFileServer.GenerateDirectoryListing(const Path: string): string;  
var
  FullPath: string;
  SearchRec: TSearchRec;
  HTML: TStringList;
  RelativePath: string;
begin
  if not IsPathSafe(Path) then
  begin
    Result := '<html><body><h1>403 Forbidden</h1></body></html>';
    Exit;
  end;

  FullPath := GetFullPath(Path);

  if Copy(FullPath, 1, Length(FRootDirectory)) <> FRootDirectory then
  begin
    Result := '<html><body><h1>403 Forbidden</h1></body></html>';
    Exit;
  end;

  if not DirectoryExists(FullPath) then
  begin
    Result := '<html><body><h1>404 Not Found</h1></body></html>';
    Exit;
  end;

  HTML := TStringList.Create;
  try
    RelativePath := StringReplace(FullPath, FRootDirectory, '', [rfIgnoreCase]);

    HTML.Add('<!DOCTYPE html>');
    HTML.Add('<html>');
    HTML.Add('<head>');
    HTML.Add('<title>Index de /' + RelativePath + '</title>');
    HTML.Add('<style>');
    HTML.Add('body { font-family: Arial, sans-serif; margin: 20px; }');
    HTML.Add('h1 { color: #333; }');
    HTML.Add('table { border-collapse: collapse; width: 100%; }');
    HTML.Add('th, td { text-align: left; padding: 8px; border-bottom: 1px solid #ddd; }');
    HTML.Add('th { background-color: #4CAF50; color: white; }');
    HTML.Add('a { color: #0066cc; text-decoration: none; }');
    HTML.Add('a:hover { text-decoration: underline; }');
    HTML.Add('.dir { font-weight: bold; }');
    HTML.Add('</style>');
    HTML.Add('</head>');
    HTML.Add('<body>');
    HTML.Add('<h1>Index de /' + RelativePath + '</h1>');
    HTML.Add('<hr>');
    HTML.Add('<table>');
    HTML.Add('<tr><th>Nom</th><th>Taille</th><th>Date</th></tr>');

    // Lien vers le répertoire parent
    if RelativePath <> '' then
      HTML.Add('<tr><td><a href="../" class="dir">[Parent Directory]</a></td><td>-</td><td>-</td></tr>');

    // Liste des répertoires
    if FindFirst(IncludeTrailingPathDelimiter(FullPath) + '*', faDirectory, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
           ((SearchRec.Attr and faDirectory) = faDirectory) then
        begin
          HTML.Add('<tr>');
          HTML.Add('<td><a href="' + SearchRec.Name + '/" class="dir">[' +
                   SearchRec.Name + ']</a></td>');
          HTML.Add('<td>-</td>');
          HTML.Add('<td>' + DateTimeToStr(FileDateToDateTime(SearchRec.Time)) + '</td>');
          HTML.Add('</tr>');
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;

    // Liste des fichiers
    if FindFirst(IncludeTrailingPathDelimiter(FullPath) + '*', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        if ((SearchRec.Attr and faDirectory) = 0) then
        begin
          HTML.Add('<tr>');
          HTML.Add('<td><a href="' + SearchRec.Name + '">' + SearchRec.Name + '</a></td>');
          HTML.Add('<td>' + FormatFloat('#,##0', SearchRec.Size) + ' octets</td>');
          HTML.Add('<td>' + DateTimeToStr(FileDateToDateTime(SearchRec.Time)) + '</td>');
          HTML.Add('</tr>');
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;

    HTML.Add('</table>');
    HTML.Add('<hr>');
    HTML.Add('<p><em>Serveur HTTP FreePascal</em></p>');
    HTML.Add('</body>');
    HTML.Add('</html>');

    Result := HTML.Text;

  finally
    HTML.Free;
  end;
end;

end.
```

### Utilisation du serveur de fichiers

```pascal
program StaticFileServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Sockets, HTTPParser, HTTPFileServer;

procedure HandleRequest(ClientSocket: TSocket; const RootDir: string);  
var
  Buffer: array[0..8191] of Char;
  Received: Integer;
  Request: THTTPRequest;
  FileServer: TFileServer;
  Content, ContentType, Response: string;
  FullPath: string;
begin
  Received := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer) - 1, 0);

  if Received > 0 then
  begin
    Buffer[Received] := #0;

    Request := THTTPRequest.Create;
    FileServer := TFileServer.Create(RootDir);
    try
      if Request.Parse(Buffer) then
      begin
        WriteLn('Requête: ', Request.MethodStr, ' ', Request.Path);

        // Chemin par défaut
        FullPath := Request.Path;
        if FullPath = '/' then
          FullPath := '/index.html';

        // Tentative de servir le fichier
        if FileServer.ServeFile(FullPath, Content, ContentType) then
        begin
          // Fichier trouvé
          Response :=
            'HTTP/1.1 200 OK'#13#10 +
            'Content-Type: ' + ContentType + #13#10 +
            'Content-Length: ' + IntToStr(Length(Content)) + #13#10 +
            'Connection: close'#13#10 +
            #13#10 +
            Content;
        end
        else if DirectoryExists(FileServer.GetFullPath(Request.Path)) then
        begin
          // Listing de répertoire
          Content := FileServer.GenerateDirectoryListing(Request.Path);
          Response :=
            'HTTP/1.1 200 OK'#13#10 +
            'Content-Type: text/html; charset=utf-8'#13#10 +
            'Content-Length: ' + IntToStr(Length(Content)) + #13#10 +
            'Connection: close'#13#10 +
            #13#10 +
            Content;
        end
        else
        begin
          // 404 Not Found
          Content :=
            '<!DOCTYPE html>'#13#10 +
            '<html><head><title>404 Not Found</title></head>'#13#10 +
            '<body><h1>404 - Fichier non trouvé</h1>'#13#10 +
            '<p>Le fichier ' + Request.Path + ' n''existe pas.</p>'#13#10 +
            '</body></html>';

          Response :=
            'HTTP/1.1 404 Not Found'#13#10 +
            'Content-Type: text/html; charset=utf-8'#13#10 +
            'Content-Length: ' + IntToStr(Length(Content)) + #13#10 +
            'Connection: close'#13#10 +
            #13#10 +
            Content;
        end;

        fpSend(ClientSocket, @Response[1], Length(Response), 0);
      end;

    finally
      FileServer.Free;
      Request.Free;
    end;
  end;

  CloseSocket(ClientSocket);
end;

var
  ServerSocket, ClientSocket: TSocket;
  ServerAddr, ClientAddr: TInetSockAddr;
  AddrLen: TSockLen;
  OptVal: Integer;
  RootDir: string;

begin
  // Répertoire racine
  if ParamCount > 0 then
    RootDir := ParamStr(1)
  else
    RootDir := GetCurrentDir;

  WriteLn('Répertoire racine: ', RootDir);

  // Création du socket
  ServerSocket := fpSocket(AF_INET, SOCK_STREAM, 0);

  if ServerSocket = INVALID_SOCKET then
  begin
    WriteLn('Erreur création socket');
    Exit;
  end;

  try
    OptVal := 1;
    fpSetSockOpt(ServerSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

    FillChar(ServerAddr, SizeOf(ServerAddr), 0);
    ServerAddr.sin_family := AF_INET;
    ServerAddr.sin_port := htons(8080);
    ServerAddr.sin_addr.s_addr := INADDR_ANY;

    if fpBind(ServerSocket, @ServerAddr, SizeOf(ServerAddr)) < 0 then
    begin
      WriteLn('Erreur bind');
      Exit;
    end;

    if fpListen(ServerSocket, 10) < 0 then
    begin
      WriteLn('Erreur listen');
      Exit;
    end;

    WriteLn('===================================');
    WriteLn('Serveur de fichiers statiques');
    WriteLn('URL: http://localhost:8080');
    WriteLn('Appuyez sur Ctrl+C pour arrêter');
    WriteLn('===================================');
    WriteLn;

    while True do
    begin
      AddrLen := SizeOf(ClientAddr);
      ClientSocket := fpAccept(ServerSocket, @ClientAddr, @AddrLen);

      if ClientSocket >= 0 then
      begin
        HandleRequest(ClientSocket, RootDir);
      end;
    end;

  finally
    CloseSocket(ServerSocket);
  end;
end.
```

## Gestion des méthodes HTTP

Implémentons correctement les différentes méthodes HTTP :

```pascal
unit HTTPMethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPParser;

type
  THTTPResponse = record
    StatusCode: Integer;
    StatusText: string;
    Headers: TStringList;
    Body: string;
  end;

function CreateResponse(StatusCode: Integer; const Body: string = ''): THTTPResponse;  
function ResponseToString(const Response: THTTPResponse): string;  
procedure SetResponseHeader(var Response: THTTPResponse; const Name, Value: string);

// Handlers pour différentes méthodes
procedure HandleGET(Request: THTTPRequest; var Response: THTTPResponse);  
procedure HandlePOST(Request: THTTPRequest; var Response: THTTPResponse);  
procedure HandlePUT(Request: THTTPRequest; var Response: THTTPResponse);  
procedure HandleDELETE(Request: THTTPRequest; var Response: THTTPResponse);  
procedure HandleHEAD(Request: THTTPRequest; var Response: THTTPResponse);  
procedure HandleOPTIONS(Request: THTTPRequest; var Response: THTTPResponse);

implementation

function GetStatusText(StatusCode: Integer): string;  
begin
  case StatusCode of
    200: Result := 'OK';
    201: Result := 'Created';
    204: Result := 'No Content';
    301: Result := 'Moved Permanently';
    302: Result := 'Found';
    304: Result := 'Not Modified';
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    503: Result := 'Service Unavailable';
  else
    Result := 'Unknown';
  end;
end;

function CreateResponse(StatusCode: Integer; const Body: string = ''): THTTPResponse;  
begin
  Result.StatusCode := StatusCode;
  Result.StatusText := GetStatusText(StatusCode);
  Result.Headers := TStringList.Create;
  Result.Headers.NameValueSeparator := ':';
  Result.Body := Body;

  // Headers par défaut
  Result.Headers.Add('Server:FreePascal HTTP Server');
  Result.Headers.Add('Date:' + FormatDateTime('ddd, dd mmm yyyy hh:nn:ss', Now) + ' GMT');
  Result.Headers.Add('Connection:close');

  if Body <> '' then
  begin
    Result.Headers.Add('Content-Length:' + IntToStr(Length(Body)));
    Result.Headers.Add('Content-Type:text/html; charset=utf-8');
  end;
end;

procedure SetResponseHeader(var Response: THTTPResponse; const Name, Value: string);  
var
  Index: Integer;
begin
  Index := Response.Headers.IndexOfName(Name);
  if Index >= 0 then
    Response.Headers[Index] := Name + ':' + Value
  else
    Response.Headers.Add(Name + ':' + Value);
end;

function ResponseToString(const Response: THTTPResponse): string;  
var
  i: Integer;
begin
  // Ligne de statut
  Result := Format('HTTP/1.1 %d %s'#13#10, [Response.StatusCode, Response.StatusText]);

  // Headers
  for i := 0 to Response.Headers.Count - 1 do
    Result := Result + Response.Headers[i] + #13#10;

  // Ligne vide
  Result := Result + #13#10;

  // Corps
  if Response.Body <> '' then
    Result := Result + Response.Body;
end;

procedure HandleGET(Request: THTTPRequest; var Response: THTTPResponse);  
begin
  Response := CreateResponse(200,
    '<html><body><h1>Méthode GET</h1>' +
    '<p>Chemin: ' + Request.Path + '</p></body></html>');
end;

procedure HandlePOST(Request: THTTPRequest; var Response: THTTPResponse);  
var
  Body: string;
begin
  Body :=
    '<html><body><h1>Méthode POST</h1>' +
    '<p>Chemin: ' + Request.Path + '</p>' +
    '<p>Corps reçu: ' + IntToStr(Length(Request.Body)) + ' octets</p>' +
    '<pre>' + Request.Body + '</pre>' +
    '</body></html>';

  Response := CreateResponse(200, Body);
end;

procedure HandlePUT(Request: THTTPRequest; var Response: THTTPResponse);  
begin
  Response := CreateResponse(201,
    '<html><body><h1>Méthode PUT</h1>' +
    '<p>Ressource créée/mise à jour: ' + Request.Path + '</p></body></html>');
end;

procedure HandleDELETE(Request: THTTPRequest; var Response: THTTPResponse);  
begin
  Response := CreateResponse(200,
    '<html><body><h1>Méthode DELETE</h1>' +
    '<p>Ressource supprimée: ' + Request.Path + '</p></body></html>');
end;

procedure HandleHEAD(Request: THTTPRequest; var Response: THTTPResponse);  
begin
  // HEAD est comme GET mais sans corps
  Response := CreateResponse(200);
  Response.Body := ''; // Pas de corps pour HEAD
end;

procedure HandleOPTIONS(Request: THTTPRequest; var Response: THTTPResponse);  
begin
  Response := CreateResponse(200);
  SetResponseHeader(Response, 'Allow', 'GET, POST, PUT, DELETE, HEAD, OPTIONS');
  SetResponseHeader(Response, 'Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, HEAD, OPTIONS');
  SetResponseHeader(Response, 'Access-Control-Allow-Origin', '*');
end;

end.
```

## API REST complète

Créons une API REST avec gestion de ressources :

```pascal
program RESTAPIServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Sockets, fpjson, jsonparser,
  HTTPParser, HTTPMethods;

type
  TUser = record
    ID: Integer;
    Name: string;
    Email: string;
  end;

var
  Users: array of TUser;
  NextID: Integer = 1;

// Fonctions utilitaires JSON
function UserToJSON(const User: TUser): TJSONObject;  
begin
  Result := TJSONObject.Create;
  Result.Add('id', User.ID);
  Result.Add('name', User.Name);
  Result.Add('email', User.Email);
end;

function UsersArrayToJSON: TJSONArray;  
var
  i: Integer;
begin
  Result := TJSONArray.Create;
  for i := 0 to High(Users) do
    Result.Add(UserToJSON(Users[i]));
end;

function FindUserByID(ID: Integer): Integer;  
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(Users) do
    if Users[i].ID = ID then
    begin
      Result := i;
      Exit;
    end;
end;

// Handlers API
procedure HandleGetUsers(Request: THTTPRequest; var Response: THTTPResponse);  
var
  JSONArray: TJSONArray;
  JSONResponse: TJSONObject;
begin
  JSONArray := UsersArrayToJSON;
  try
    JSONResponse := TJSONObject.Create;
    try
      JSONResponse.Add('success', True);
      JSONResponse.Add('count', Length(Users));
      JSONResponse.Add('users', JSONArray);

      Response := CreateResponse(200, JSONResponse.AsJSON);
      SetResponseHeader(Response, 'Content-Type', 'application/json');
    finally
      JSONResponse.Free;
    end;
  except
    JSONArray.Free;
    raise;
  end;
end;

procedure HandleGetUser(Request: THTTPRequest; var Response: THTTPResponse);  
var
  IDStr: string;
  ID, Index: Integer;
  JSONResponse: TJSONObject;
begin
  // Extraction de l'ID depuis l'URL (/api/users/123)
  IDStr := Copy(Request.Path, Length('/api/users/') + 1, MaxInt);

  if not TryStrToInt(IDStr, ID) then
  begin
    Response := CreateResponse(400, '{"error":"Invalid ID"}');
    SetResponseHeader(Response, 'Content-Type', 'application/json');
    Exit;
  end;

  Index := FindUserByID(ID);

  if Index < 0 then
  begin
    Response := CreateResponse(404, '{"error":"User not found"}');
    SetResponseHeader(Response, 'Content-Type', 'application/json');
    Exit;
  end;

  JSONResponse := TJSONObject.Create;
  try
    JSONResponse.Add('success', True);
    JSONResponse.Add('user', UserToJSON(Users[Index]));

    Response := CreateResponse(200, JSONResponse.AsJSON);
    SetResponseHeader(Response, 'Content-Type', 'application/json');
  finally
    JSONResponse.Free;
  end;
end;

procedure HandleCreateUser(Request: THTTPRequest; var Response: THTTPResponse);  
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  NewUser: TUser;
  JSONResponse: TJSONObject;
begin
  try
    JSONData := GetJSON(Request.Body);

    if JSONData is TJSONObject then
    begin
      JSONObj := TJSONObject(JSONData);

      NewUser.ID := NextID;
      Inc(NextID);
      NewUser.Name := JSONObj.Get('name', '');
      NewUser.Email := JSONObj.Get('email', '');

      SetLength(Users, Length(Users) + 1);
      Users[High(Users)] := NewUser;

      JSONResponse := TJSONObject.Create;
      try
        JSONResponse.Add('success', True);
        JSONResponse.Add('message', 'User created');
        JSONResponse.Add('user', UserToJSON(NewUser));

        Response := CreateResponse(201, JSONResponse.AsJSON);
        SetResponseHeader(Response, 'Content-Type', 'application/json');
      finally
        JSONResponse.Free;
      end;
    end
    else
    begin
      Response := CreateResponse(400, '{"error":"Invalid JSON"}');
      SetResponseHeader(Response, 'Content-Type', 'application/json');
    end;

    JSONData.Free;

  except
    on E: Exception do
    begin
      Response := CreateResponse(400, '{"error":"' + E.Message + '"}');
      SetResponseHeader(Response, 'Content-Type', 'application/json');
    end;
  end;
end;

procedure HandleUpdateUser(Request: THTTPRequest; var Response: THTTPResponse);  
var
  IDStr: string;
  ID, Index: Integer;
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  JSONResponse: TJSONObject;
begin
  IDStr := Copy(Request.Path, Length('/api/users/') + 1, MaxInt);

  if not TryStrToInt(IDStr, ID) then
  begin
    Response := CreateResponse(400, '{"error":"Invalid ID"}');
    SetResponseHeader(Response, 'Content-Type', 'application/json');
    Exit;
  end;

  Index := FindUserByID(ID);

  if Index < 0 then
  begin
    Response := CreateResponse(404, '{"error":"User not found"}');
    SetResponseHeader(Response, 'Content-Type', 'application/json');
    Exit;
  end;

  try
    JSONData := GetJSON(Request.Body);

    if JSONData is TJSONObject then
    begin
      JSONObj := TJSONObject(JSONData);

      if JSONObj.IndexOfName('name') >= 0 then
        Users[Index].Name := JSONObj.Get('name', Users[Index].Name);

      if JSONObj.IndexOfName('email') >= 0 then
        Users[Index].Email := JSONObj.Get('email', Users[Index].Email);

      JSONResponse := TJSONObject.Create;
      try
        JSONResponse.Add('success', True);
        JSONResponse.Add('message', 'User updated');
        JSONResponse.Add('user', UserToJSON(Users[Index]));

        Response := CreateResponse(200, JSONResponse.AsJSON);
        SetResponseHeader(Response, 'Content-Type', 'application/json');
      finally
        JSONResponse.Free;
      end;
    end;

    JSONData.Free;

  except
    on E: Exception do
    begin
      Response := CreateResponse(400, '{"error":"' + E.Message + '"}');
      SetResponseHeader(Response, 'Content-Type', 'application/json');
    end;
  end;
end;

procedure HandleDeleteUser(Request: THTTPRequest; var Response: THTTPResponse);  
var
  IDStr: string;
  ID, Index, i: Integer;
  JSONResponse: TJSONObject;
begin
  IDStr := Copy(Request.Path, Length('/api/users/') + 1, MaxInt);

  if not TryStrToInt(IDStr, ID) then
  begin
    Response := CreateResponse(400, '{"error":"Invalid ID"}');
    SetResponseHeader(Response, 'Content-Type', 'application/json');
    Exit;
  end;

  Index := FindUserByID(ID);

  if Index < 0 then
  begin
    Response := CreateResponse(404, '{"error":"User not found"}');
    SetResponseHeader(Response, 'Content-Type', 'application/json');
    Exit;
  end;

  // Suppression
  for i := Index to High(Users) - 1 do
    Users[i] := Users[i + 1];
  SetLength(Users, Length(Users) - 1);

  JSONResponse := TJSONObject.Create;
  try
    JSONResponse.Add('success', True);
    JSONResponse.Add('message', 'User deleted');

    Response := CreateResponse(200, JSONResponse.AsJSON);
    SetResponseHeader(Response, 'Content-Type', 'application/json');
  finally
    JSONResponse.Free;
  end;
end;

procedure HandleAPIRequest(ClientSocket: TSocket);  
var
  Buffer: array[0..8191] of Char;
  Received: Integer;
  Request: THTTPRequest;
  Response: THTTPResponse;
  ResponseStr: string;
begin
  Received := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer) - 1, 0);

  if Received > 0 then
  begin
    Buffer[Received] := #0;

    Request := THTTPRequest.Create;
    try
      if Request.Parse(Buffer) then
      begin
        WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] ',
                Request.MethodStr, ' ', Request.Path);

        // Routing API
        if Request.Path = '/api/users' then
        begin
          case Request.Method of
            hmGET: HandleGetUsers(Request, Response);
            hmPOST: HandleCreateUser(Request, Response);
          else
            Response := CreateResponse(405, '{"error":"Method not allowed"}');
            SetResponseHeader(Response, 'Content-Type', 'application/json');
          end;
        end
        else if Copy(Request.Path, 1, 11) = '/api/users/' then
        begin
          case Request.Method of
            hmGET: HandleGetUser(Request, Response);
            hmPUT: HandleUpdateUser(Request, Response);
            hmDELETE: HandleDeleteUser(Request, Response);
          else
            Response := CreateResponse(405, '{"error":"Method not allowed"}');
            SetResponseHeader(Response, 'Content-Type', 'application/json');
          end;
        end
        else
        begin
          Response := CreateResponse(404, '{"error":"Endpoint not found"}');
          SetResponseHeader(Response, 'Content-Type', 'application/json');
        end;

        ResponseStr := ResponseToString(Response);
        fpSend(ClientSocket, @ResponseStr[1], Length(ResponseStr), 0);

        Response.Headers.Free;
      end;

    finally
      Request.Free;
    end;
  end;

  CloseSocket(ClientSocket);
end;

var
  ServerSocket, ClientSocket: TSocket;
  ServerAddr, ClientAddr: TInetSockAddr;
  AddrLen: TSockLen;
  OptVal: Integer;

begin
  // Initialisation avec quelques utilisateurs de test
  SetLength(Users, 2);
  Users[0].ID := NextID; Inc(NextID);
  Users[0].Name := 'Jean Dupont';
  Users[0].Email := 'jean@example.com';

  Users[1].ID := NextID; Inc(NextID);
  Users[1].Name := 'Marie Martin';
  Users[1].Email := 'marie@example.com';

  ServerSocket := fpSocket(AF_INET, SOCK_STREAM, 0);

  if ServerSocket = INVALID_SOCKET then
  begin
    WriteLn('Erreur création socket');
    Exit;
  end;

  try
    OptVal := 1;
    fpSetSockOpt(ServerSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

    FillChar(ServerAddr, SizeOf(ServerAddr), 0);
    ServerAddr.sin_family := AF_INET;
    ServerAddr.sin_port := htons(8080);
    ServerAddr.sin_addr.s_addr := INADDR_ANY;

    if fpBind(ServerSocket, @ServerAddr, SizeOf(ServerAddr)) < 0 then
    begin
      WriteLn('Erreur bind');
      Exit;
    end;

    if fpListen(ServerSocket, 10) < 0 then
    begin
      WriteLn('Erreur listen');
      Exit;
    end;

    WriteLn('=========================================');
    WriteLn('API REST Server démarré');
    WriteLn('URL: http://localhost:8080');
    WriteLn;
    WriteLn('Endpoints disponibles:');
    WriteLn('  GET    /api/users      - Liste tous les utilisateurs');
    WriteLn('  GET    /api/users/:id  - Récupère un utilisateur');
    WriteLn('  POST   /api/users      - Crée un utilisateur');
    WriteLn('  PUT    /api/users/:id  - Met à jour un utilisateur');
    WriteLn('  DELETE /api/users/:id  - Supprime un utilisateur');
    WriteLn;
    WriteLn('Appuyez sur Ctrl+C pour arrêter');
    WriteLn('=========================================');
    WriteLn;

    while True do
    begin
      AddrLen := SizeOf(ClientAddr);
      ClientSocket := fpAccept(ServerSocket, @ClientAddr, @AddrLen);

      if ClientSocket >= 0 then
      begin
        HandleAPIRequest(ClientSocket);
      end;
    end;

  finally
    CloseSocket(ServerSocket);
  end;
end.
```

## Support HTTPS avec OpenSSL

Pour ajouter le support HTTPS, nous devons utiliser OpenSSL :

```pascal
unit HTTPSSupport;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Sockets, OpenSSL, OpenSSLSockets;

type
  THTTPSServer = class
  private
    FPort: Word;
    FSocket: TSocket;
    FSSLContext: PSSL_CTX;
    FCertFile: string;
    FKeyFile: string;
    function InitSSL: Boolean;
    procedure CleanupSSL;
    function AcceptSSLConnection(ClientSocket: TSocket): PSSL;
  public
    constructor Create(APort: Word; const ACertFile, AKeyFile: string);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

implementation

{ THTTPSServer }

constructor THTTPSServer.Create(APort: Word; const ACertFile, AKeyFile: string);  
begin
  FPort := APort;
  FCertFile := ACertFile;
  FKeyFile := AKeyFile;
  FSocket := INVALID_SOCKET;
  FSSLContext := nil;
end;

destructor THTTPSServer.Destroy;  
begin
  Stop;
  inherited;
end;

function THTTPSServer.InitSSL: Boolean;  
begin
  Result := False;

  // Initialisation de la bibliothèque OpenSSL
  SSL_load_error_strings;
  SSL_library_init;
  OpenSSL_add_all_algorithms;

  // Création du contexte SSL
  FSSLContext := SSL_CTX_new(TLS_server_method);

  if FSSLContext = nil then
  begin
    WriteLn('Erreur création contexte SSL');
    Exit;
  end;

  // Chargement du certificat
  if SSL_CTX_use_certificate_file(FSSLContext, PChar(FCertFile),
                                   SSL_FILETYPE_PEM) <= 0 then
  begin
    WriteLn('Erreur chargement certificat: ', FCertFile);
    Exit;
  end;

  // Chargement de la clé privée
  if SSL_CTX_use_PrivateKey_file(FSSLContext, PChar(FKeyFile),
                                  SSL_FILETYPE_PEM) <= 0 then
  begin
    WriteLn('Erreur chargement clé privée: ', FKeyFile);
    Exit;
  end;

  // Vérification de la cohérence
  if SSL_CTX_check_private_key(FSSLContext) = 0 then
  begin
    WriteLn('La clé privée ne correspond pas au certificat');
    Exit;
  end;

  WriteLn('SSL initialisé avec succès');
  Result := True;
end;

procedure THTTPSServer.CleanupSSL;  
begin
  if FSSLContext <> nil then
  begin
    SSL_CTX_free(FSSLContext);
    FSSLContext := nil;
  end;

  EVP_cleanup;
end;

function THTTPSServer.AcceptSSLConnection(ClientSocket: TSocket): PSSL;  
var
  SSL: PSSL;
begin
  Result := nil;

  SSL := SSL_new(FSSLContext);
  if SSL = nil then
  begin
    WriteLn('Erreur création SSL');
    Exit;
  end;

  SSL_set_fd(SSL, ClientSocket);

  if SSL_accept(SSL) <= 0 then
  begin
    WriteLn('Erreur SSL_accept');
    SSL_free(SSL);
    Exit;
  end;

  Result := SSL;
  WriteLn('Connexion SSL établie');
end;

procedure THTTPSServer.Start;  
var
  ServerAddr, ClientAddr: TInetSockAddr;
  ClientSocket: TSocket;
  AddrLen: TSockLen;
  OptVal: Integer;
  SSL: PSSL;
  Buffer: array[0..8191] of Char;
  Received: Integer;
  Response: string;
begin
  if not InitSSL then
    Exit;

  FSocket := fpSocket(AF_INET, SOCK_STREAM, 0);

  if FSocket = INVALID_SOCKET then
  begin
    WriteLn('Erreur création socket');
    CleanupSSL;
    Exit;
  end;

  try
    OptVal := 1;
    fpSetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

    FillChar(ServerAddr, SizeOf(ServerAddr), 0);
    ServerAddr.sin_family := AF_INET;
    ServerAddr.sin_port := htons(FPort);
    ServerAddr.sin_addr.s_addr := INADDR_ANY;

    if fpBind(FSocket, @ServerAddr, SizeOf(ServerAddr)) < 0 then
    begin
      WriteLn('Erreur bind');
      Exit;
    end;

    if fpListen(FSocket, 10) < 0 then
    begin
      WriteLn('Erreur listen');
      Exit;
    end;

    WriteLn('=========================================');
    WriteLn('Serveur HTTPS démarré');
    WriteLn('URL: https://localhost:', FPort);
    WriteLn('Certificat: ', FCertFile);
    WriteLn('Appuyez sur Ctrl+C pour arrêter');
    WriteLn('=========================================');
    WriteLn;

    while True do
    begin
      AddrLen := SizeOf(ClientAddr);
      ClientSocket := fpAccept(FSocket, @ClientAddr, @AddrLen);

      if ClientSocket >= 0 then
      begin
        WriteLn('Connexion de ', NetAddrToStr(ClientAddr.sin_addr));

        SSL := AcceptSSLConnection(ClientSocket);

        if SSL <> nil then
        begin
          try
            // Lecture de la requête via SSL
            Received := SSL_read(SSL, @Buffer, SizeOf(Buffer) - 1);

            if Received > 0 then
            begin
              Buffer[Received] := #0;
              WriteLn('Requête reçue: ', Copy(Buffer, 1, 50), '...');

              // Réponse simple
              Response :=
                'HTTP/1.1 200 OK'#13#10 +
                'Content-Type: text/html'#13#10 +
                'Connection: close'#13#10 +
                #13#10 +
                '<!DOCTYPE html>'#13#10 +
                '<html><head><title>HTTPS Server</title></head>'#13#10 +
                '<body><h1>Connexion HTTPS sécurisée !</h1>'#13#10 +
                '<p>Cette page est servie via HTTPS.</p></body></html>';

              SSL_write(SSL, @Response[1], Length(Response));
            end;

          finally
            SSL_shutdown(SSL);
            SSL_free(SSL);
          end;
        end;

        CloseSocket(ClientSocket);
      end;
    end;

  finally
    CleanupSSL;
    if FSocket <> INVALID_SOCKET then
      CloseSocket(FSocket);
  end;
end;

procedure THTTPSServer.Stop;  
begin
  if FSocket <> INVALID_SOCKET then
  begin
    CloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;

  CleanupSSL;
end;

end.
```

### Génération de certificat auto-signé

Pour tester, créez un certificat auto-signé :

**Linux/Ubuntu :**
```bash
# Générer une clé privée et un certificat
openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -nodes

# Remplir les informations demandées
# Common Name: localhost
```

**Windows :**
```cmd
# Télécharger OpenSSL pour Windows depuis: https://slproweb.com/products/Win32OpenSSL.html
# Puis exécuter la même commande
```

## Gestion des sessions

Implémentons un système de sessions simple :

```pascal
unit HTTPSessions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TSession = record
    ID: string;
    Data: TStringList;
    LastAccess: TDateTime;
  end;

  TSessionManager = class
  private
    FSessions: array of TSession;
    FTimeout: Integer; // en minutes
    function GenerateSessionID: string;
    function FindSession(const SessionID: string): Integer;
  public
    constructor Create(ATimeout: Integer = 30);
    destructor Destroy; override;
    function CreateSession: string;
    function GetSession(const SessionID: string; out Session: TSession): Boolean;
    procedure SetSessionData(const SessionID, Key, Value: string);
    function GetSessionData(const SessionID, Key: string): string;
    procedure DestroySession(const SessionID: string);
    procedure CleanupExpiredSessions;
  end;

implementation

uses
  MD5;

{ TSessionManager }

constructor TSessionManager.Create(ATimeout: Integer);  
begin
  FTimeout := ATimeout;
  SetLength(FSessions, 0);
end;

destructor TSessionManager.Destroy;  
var
  i: Integer;
begin
  for i := 0 to High(FSessions) do
    FSessions[i].Data.Free;
  inherited;
end;

function TSessionManager.GenerateSessionID: string;  
var
  GUIDStr: string;
begin
  GUIDStr := GUIDToString(TGUID.NewGuid);
  Result := MD5Print(MD5String(GUIDStr + DateTimeToStr(Now)));
end;

function TSessionManager.FindSession(const SessionID: string): Integer;  
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FSessions) do
    if FSessions[i].ID = SessionID then
    begin
      Result := i;
      Exit;
    end;
end;

function TSessionManager.CreateSession: string;  
var
  Index: Integer;
begin
  Result := GenerateSessionID;

  Index := Length(FSessions);
  SetLength(FSessions, Index + 1);

  FSessions[Index].ID := Result;
  FSessions[Index].Data := TStringList.Create;
  FSessions[Index].Data.NameValueSeparator := '=';
  FSessions[Index].LastAccess := Now;

  WriteLn('Session créée: ', Result);
end;

function TSessionManager.GetSession(const SessionID: string;
                                    out Session: TSession): Boolean;
var
  Index: Integer;
begin
  Result := False;

  Index := FindSession(SessionID);
  if Index >= 0 then
  begin
    Session := FSessions[Index];
    FSessions[Index].LastAccess := Now; // Mise à jour du dernier accès
    Result := True;
  end;
end;

procedure TSessionManager.SetSessionData(const SessionID, Key, Value: string);  
var
  Index: Integer;
begin
  Index := FindSession(SessionID);
  if Index >= 0 then
  begin
    FSessions[Index].Data.Values[Key] := Value;
    FSessions[Index].LastAccess := Now;
  end;
end;

function TSessionManager.GetSessionData(const SessionID, Key: string): string;  
var
  Index: Integer;
begin
  Result := '';

  Index := FindSession(SessionID);
  if Index >= 0 then
  begin
    Result := FSessions[Index].Data.Values[Key];
    FSessions[Index].LastAccess := Now;
  end;
end;

procedure TSessionManager.DestroySession(const SessionID: string);  
var
  Index, i: Integer;
begin
  Index := FindSession(SessionID);

  if Index >= 0 then
  begin
    FSessions[Index].Data.Free;

    for i := Index to High(FSessions) - 1 do
      FSessions[i] := FSessions[i + 1];

    SetLength(FSessions, Length(FSessions) - 1);
    WriteLn('Session détruite: ', SessionID);
  end;
end;

procedure TSessionManager.CleanupExpiredSessions;  
var
  i: Integer;
  ExpiredTime: TDateTime;
begin
  ExpiredTime := IncMinute(Now, -FTimeout);

  i := 0;
  while i <= High(FSessions) do
  begin
    if FSessions[i].LastAccess < ExpiredTime then
    begin
      WriteLn('Session expirée: ', FSessions[i].ID);
      DestroySession(FSessions[i].ID);
    end
    else
      Inc(i);
  end;
end;

end.
```

### Utilisation des sessions

```pascal
procedure HandleLoginRequest(Request: THTTPRequest;
                             SessionManager: TSessionManager;
                             var Response: THTTPResponse);
var
  SessionID: string;
  Username, Password: string;
begin
  // Récupération des credentials (simplifié)
  Username := 'admin';
  Password := 'password';

  // Validation (en réalité, vérifier en base de données)
  if (Username = 'admin') and (Password = 'password') then
  begin
    // Création de session
    SessionID := SessionManager.CreateSession;
    SessionManager.SetSessionData(SessionID, 'username', Username);
    SessionManager.SetSessionData(SessionID, 'logged_in', 'true');

    Response := CreateResponse(200,
      '{"success":true,"message":"Logged in"}');
    SetResponseHeader(Response, 'Content-Type', 'application/json');
    SetResponseHeader(Response, 'Set-Cookie',
      'session_id=' + SessionID + '; HttpOnly; Path=/');
  end
  else
  begin
    Response := CreateResponse(401,
      '{"success":false,"message":"Invalid credentials"}');
    SetResponseHeader(Response, 'Content-Type', 'application/json');
  end;
end;

function ExtractSessionIDFromCookie(const CookieHeader: string): string;  
var
  StartPos, EndPos: Integer;
begin
  Result := '';

  StartPos := Pos('session_id=', CookieHeader);
  if StartPos > 0 then
  begin
    StartPos := StartPos + Length('session_id=');
    EndPos := PosEx(';', CookieHeader, StartPos);
    if EndPos = 0 then
      EndPos := Length(CookieHeader) + 1;

    Result := Copy(CookieHeader, StartPos, EndPos - StartPos);
  end;
end;
```

## Serveur multi-thread

Pour améliorer les performances, créons un serveur multi-thread :

```pascal
program MultiThreadHTTPServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Sockets;

type
  TClientThread = class(TThread)
  private
    FClientSocket: TSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(AClientSocket: TSocket);
  end;

{ TClientThread }

constructor TClientThread.Create(AClientSocket: TSocket);  
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClientSocket := AClientSocket;
end;

procedure TClientThread.Execute;  
var
  Buffer: array[0..8191] of Char;
  Received: Integer;
  Response: string;
begin
  try
    Received := fpRecv(FClientSocket, @Buffer, SizeOf(Buffer) - 1, 0);

    if Received > 0 then
    begin
      Buffer[Received] := #0;

      WriteLn('[Thread ', ThreadID, '] Requête reçue: ',
              Copy(Buffer, 1, 50), '...');

      // Simulation d'un traitement long
      Sleep(1000);

      Response :=
        'HTTP/1.1 200 OK'#13#10 +
        'Content-Type: text/html'#13#10 +
        'Connection: close'#13#10 +
        #13#10 +
        '<!DOCTYPE html>'#13#10 +
        '<html><body><h1>Serveur Multi-Thread</h1>'#13#10 +
        '<p>Thread ID: ' + IntToStr(ThreadID) + '</p>'#13#10 +
        '<p>Temps: ' + FormatDateTime('hh:nn:ss', Now) + '</p>'#13#10 +
        '</body></html>';

      fpSend(FClientSocket, @Response[1], Length(Response), 0);

      WriteLn('[Thread ', ThreadID, '] Réponse envoyée');
    end;

  finally
    CloseSocket(FClientSocket);
  end;
end;

var
  ServerSocket, ClientSocket: TSocket;
  ServerAddr, ClientAddr: TInetSockAddr;
  AddrLen: TSockLen;
  OptVal: Integer;
  ActiveThreads: Integer = 0;

begin
  ServerSocket := fpSocket(AF_INET, SOCK_STREAM, 0);

  if ServerSocket = INVALID_SOCKET then
  begin
    WriteLn('Erreur création socket');
    Exit;
  end;

  try
    OptVal := 1;
    fpSetSockOpt(ServerSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

    FillChar(ServerAddr, SizeOf(ServerAddr), 0);
    ServerAddr.sin_family := AF_INET;
    ServerAddr.sin_port := htons(8080);
    ServerAddr.sin_addr.s_addr := INADDR_ANY;

    if fpBind(ServerSocket, @ServerAddr, SizeOf(ServerAddr)) < 0 then
    begin
      WriteLn('Erreur bind');
      Exit;
    end;

    if fpListen(ServerSocket, 50) < 0 then  // Queue plus grande
    begin
      WriteLn('Erreur listen');
      Exit;
    end;

    WriteLn('=========================================');
    WriteLn('Serveur HTTP Multi-Thread démarré');
    WriteLn('URL: http://localhost:8080');
    WriteLn('Appuyez sur Ctrl+C pour arrêter');
    WriteLn('=========================================');
    WriteLn;

    while True do
    begin
      AddrLen := SizeOf(ClientAddr);
      ClientSocket := fpAccept(ServerSocket, @ClientAddr, @AddrLen);

      if ClientSocket >= 0 then
      begin
        WriteLn('Nouvelle connexion de ', NetAddrToStr(ClientAddr.sin_addr));

        // Création d'un thread pour gérer le client
        TClientThread.Create(ClientSocket);
        Inc(ActiveThreads);

        WriteLn('Threads actifs: ', ActiveThreads);
      end;
    end;

  finally
    CloseSocket(ServerSocket);
  end;
end.
```

## Pool de threads

Pour optimiser, utilisons un pool de threads :

```pascal
type
  TWorkerThread = class(TThread)
  private
    FQueue: TThreadList;
  protected
    procedure Execute; override;
  public
    constructor Create(AQueue: TThreadList);
  end;

  TThreadPool = class
  private
    FThreads: array of TWorkerThread;
    FQueue: TThreadList;
    FMaxThreads: Integer;
  public
    constructor Create(AMaxThreads: Integer);
    destructor Destroy; override;
    procedure AddTask(ClientSocket: TSocket);
  end;

constructor TWorkerThread.Create(AQueue: TThreadList);  
begin
  inherited Create(False);
  FQueue := AQueue;
end;

procedure TWorkerThread.Execute;  
var
  List: TList;
  ClientSocket: TSocket;
  // ... (code de traitement)
begin
  while not Terminated do
  begin
    List := FQueue.LockList;
    try
      if List.Count > 0 then
      begin
        ClientSocket := TSocket(List[0]);
        List.Delete(0);
      end
      else
        ClientSocket := INVALID_SOCKET;
    finally
      FQueue.UnlockList;
    end;

    if ClientSocket <> INVALID_SOCKET then
    begin
      // Traiter le client
      // ...
    end
    else
      Sleep(10);
  end;
end;

constructor TThreadPool.Create(AMaxThreads: Integer);  
var
  i: Integer;
begin
  FMaxThreads := AMaxThreads;
  FQueue := TThreadList.Create;

  SetLength(FThreads, FMaxThreads);
  for i := 0 to FMaxThreads - 1 do
    FThreads[i] := TWorkerThread.Create(FQueue);

  WriteLn('Thread pool créé avec ', FMaxThreads, ' threads');
end;

destructor TThreadPool.Destroy;  
var
  i: Integer;
begin
  for i := 0 to High(FThreads) do
  begin
    FThreads[i].Terminate;
    FThreads[i].WaitFor;
    FThreads[i].Free;
  end;

  FQueue.Free;
  inherited;
end;

procedure TThreadPool.AddTask(ClientSocket: TSocket);  
var
  List: TList;
begin
  List := FQueue.LockList;
  try
    List.Add(Pointer(ClientSocket));
  finally
    FQueue.UnlockList;
  end;
end;
```

## Middleware et filtres

Ajoutons un système de middleware :

```pascal
type
  TMiddleware = procedure(Request: THTTPRequest; var Response: THTTPResponse;
                         var Continue: Boolean);

  TMiddlewareChain = class
  private
    FMiddlewares: array of TMiddleware;
  public
    procedure Add(Middleware: TMiddleware);
    procedure Execute(Request: THTTPRequest; var Response: THTTPResponse);
  end;

procedure TMiddlewareChain.Add(Middleware: TMiddleware);  
begin
  SetLength(FMiddlewares, Length(FMiddlewares) + 1);
  FMiddlewares[High(FMiddlewares)] := Middleware;
end;

procedure TMiddlewareChain.Execute(Request: THTTPRequest;
                                  var Response: THTTPResponse);
var
  i: Integer;
  Continue: Boolean;
begin
  Continue := True;

  for i := 0 to High(FMiddlewares) do
  begin
    if not Continue then
      Break;

    FMiddlewares[i](Request, Response, Continue);
  end;
end;

// Exemples de middleware

procedure LoggingMiddleware(Request: THTTPRequest; var Response: THTTPResponse;
                           var Continue: Boolean);
begin
  WriteLn('[', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), '] ',
          Request.MethodStr, ' ', Request.Path);
  Continue := True;
end;

procedure AuthMiddleware(Request: THTTPRequest; var Response: THTTPResponse;
                        var Continue: Boolean);
var
  AuthHeader: string;
begin
  AuthHeader := Request.Headers.Get('Authorization');

  if AuthHeader = '' then
  begin
    Response := CreateResponse(401, '{"error":"Unauthorized"}');
    SetResponseHeader(Response, 'Content-Type', 'application/json');
    SetResponseHeader(Response, 'WWW-Authenticate', 'Bearer');
    Continue := False;
  end
  else
    Continue := True;
end;

procedure CORSMiddleware(Request: THTTPRequest; var Response: THTTPResponse;
                        var Continue: Boolean);
begin
  SetResponseHeader(Response, 'Access-Control-Allow-Origin', '*');
  SetResponseHeader(Response, 'Access-Control-Allow-Methods',
                   'GET, POST, PUT, DELETE, OPTIONS');
  SetResponseHeader(Response, 'Access-Control-Allow-Headers', 'Content-Type, Authorization');
  Continue := True;
end;
```

## Bonnes pratiques

### 1. Sécurité

```pascal
// Validation des entrées
function SanitizeInput(const Input: string): string;  
begin
  Result := StringReplace(Input, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);
end;

// Protection contre les injections SQL
function EscapeSQL(const Input: string): string;  
begin
  Result := StringReplace(Input, '''', '''''', [rfReplaceAll]);
end;

// Limitation de taille
const
  MAX_REQUEST_SIZE = 1024 * 1024; // 1 MB

procedure ValidateRequestSize(Size: Integer);  
begin
  if Size > MAX_REQUEST_SIZE then
    raise Exception.Create('Request too large');
end;
```

### 2. Performance

```pascal
// Compression gzip
procedure CompressResponse(var Response: string);
// ... (utiliser une bibliothèque de compression)

// Cache
type
  TCacheEntry = record
    Path: string;
    Content: string;
    ContentType: string;
    Timestamp: TDateTime;
    ETag: string;
  end;

var
  Cache: array of TCacheEntry;

function FindInCache(const Path: string; MaxAge: Integer): Integer;  
var
  i: Integer;
  ExpireTime: TDateTime;
begin
  Result := -1;
  ExpireTime := IncSecond(Now, -MaxAge);

  for i := 0 to High(Cache) do
  begin
    if (Cache[i].Path = Path) and (Cache[i].Timestamp >= ExpireTime) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure AddToCache(const Path, Content, ContentType: string);  
var
  Index: Integer;
  ETag: string;
begin
  Index := Length(Cache);
  SetLength(Cache, Index + 1);

  // Génération d'un ETag simple
  ETag := MD5Print(MD5String(Content));

  Cache[Index].Path := Path;
  Cache[Index].Content := Content;
  Cache[Index].ContentType := ContentType;
  Cache[Index].Timestamp := Now;
  Cache[Index].ETag := ETag;
end;

// Keep-Alive
procedure SetKeepAlive(var Response: THTTPResponse; Timeout, MaxRequests: Integer);  
begin
  SetResponseHeader(Response, 'Connection', 'keep-alive');
  SetResponseHeader(Response, 'Keep-Alive',
    Format('timeout=%d, max=%d', [Timeout, MaxRequests]));
end;

// Chunked Transfer Encoding (pour les réponses volumineuses)
function CreateChunkedResponse(const Data: string): string;  
var
  ChunkSize: Integer;
  Pos, Remaining: Integer;
begin
  Result := '';
  ChunkSize := 8192; // 8KB par chunk
  Pos := 1;
  Remaining := Length(Data);

  while Remaining > 0 do
  begin
    if Remaining < ChunkSize then
      ChunkSize := Remaining;

    // Format: taille_en_hexa\r\n données\r\n
    Result := Result + IntToHex(ChunkSize, 1) + #13#10;
    Result := Result + Copy(Data, Pos, ChunkSize) + #13#10;

    Inc(Pos, ChunkSize);
    Dec(Remaining, ChunkSize);
  end;

  // Chunk final de taille 0
  Result := Result + '0'#13#10#13#10;
end;
```

### 3. Logging et monitoring

```pascal
unit HTTPLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  THTTPLogger = class
  private
    FLogFile: TextFile;
    FLogFilePath: string;
    FMinLevel: TLogLevel;
    procedure WriteToFile(const Line: string);
  public
    constructor Create(const ALogFilePath: string; AMinLevel: TLogLevel = llInfo);
    destructor Destroy; override;
    procedure Log(Level: TLogLevel; const Message: string);
    procedure Debug(const Message: string);
    procedure Info(const Message: string);
    procedure Warning(const Message: string);
    procedure Error(const Message: string);
    procedure LogRequest(const Method, Path, ClientIP: string; StatusCode: Integer);
  end;

implementation

{ THTTPLogger }

constructor THTTPLogger.Create(const ALogFilePath: string; AMinLevel: TLogLevel);  
begin
  FLogFilePath := ALogFilePath;
  FMinLevel := AMinLevel;

  AssignFile(FLogFile, FLogFilePath);
  if FileExists(FLogFilePath) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
end;

destructor THTTPLogger.Destroy;  
begin
  CloseFile(FLogFile);
  inherited;
end;

procedure THTTPLogger.WriteToFile(const Line: string);  
begin
  WriteLn(FLogFile, Line);
  Flush(FLogFile);
end;

procedure THTTPLogger.Log(Level: TLogLevel; const Message: string);  
var
  LevelStr: string;
  Line: string;
begin
  if Level < FMinLevel then
    Exit;

  case Level of
    llDebug: LevelStr := 'DEBUG';
    llInfo: LevelStr := 'INFO';
    llWarning: LevelStr := 'WARNING';
    llError: LevelStr := 'ERROR';
  end;

  Line := Format('[%s] [%s] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), LevelStr, Message]);

  WriteToFile(Line);
  WriteLn(Line); // Aussi dans la console
end;

procedure THTTPLogger.Debug(const Message: string);  
begin
  Log(llDebug, Message);
end;

procedure THTTPLogger.Info(const Message: string);  
begin
  Log(llInfo, Message);
end;

procedure THTTPLogger.Warning(const Message: string);  
begin
  Log(llWarning, Message);
end;

procedure THTTPLogger.Error(const Message: string);  
begin
  Log(llError, Message);
end;

procedure THTTPLogger.LogRequest(const Method, Path, ClientIP: string;
                                 StatusCode: Integer);
begin
  Info(Format('%s - %s %s - %d', [ClientIP, Method, Path, StatusCode]));
end;

end.
```

### 4. Configuration

```pascal
unit HTTPConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TServerConfig = class
  private
    FIniFile: TIniFile;
    FPort: Integer;
    FMaxConnections: Integer;
    FThreadPoolSize: Integer;
    FTimeout: Integer;
    FRootDirectory: string;
    FEnableSSL: Boolean;
    FCertFile: string;
    FKeyFile: string;
    FEnableLogging: Boolean;
    FLogFile: string;
  public
    constructor Create(const ConfigFile: string);
    destructor Destroy; override;
    procedure LoadFromFile;
    procedure SaveToFile;
    property Port: Integer read FPort write FPort;
    property MaxConnections: Integer read FMaxConnections write FMaxConnections;
    property ThreadPoolSize: Integer read FThreadPoolSize write FThreadPoolSize;
    property Timeout: Integer read FTimeout write FTimeout;
    property RootDirectory: string read FRootDirectory write FRootDirectory;
    property EnableSSL: Boolean read FEnableSSL write FEnableSSL;
    property CertFile: string read FCertFile write FCertFile;
    property KeyFile: string read FKeyFile write FKeyFile;
    property EnableLogging: Boolean read FEnableLogging write FEnableLogging;
    property LogFile: string read FLogFile write FLogFile;
  end;

implementation

{ TServerConfig }

constructor TServerConfig.Create(const ConfigFile: string);  
begin
  FIniFile := TIniFile.Create(ConfigFile);

  // Valeurs par défaut
  FPort := 8080;
  FMaxConnections := 100;
  FThreadPoolSize := 4;
  FTimeout := 30;
  FRootDirectory := GetCurrentDir;
  FEnableSSL := False;
  FCertFile := 'cert.pem';
  FKeyFile := 'key.pem';
  FEnableLogging := True;
  FLogFile := 'server.log';
end;

destructor TServerConfig.Destroy;  
begin
  FIniFile.Free;
  inherited;
end;

procedure TServerConfig.LoadFromFile;  
begin
  FPort := FIniFile.ReadInteger('Server', 'Port', FPort);
  FMaxConnections := FIniFile.ReadInteger('Server', 'MaxConnections', FMaxConnections);
  FThreadPoolSize := FIniFile.ReadInteger('Server', 'ThreadPoolSize', FThreadPoolSize);
  FTimeout := FIniFile.ReadInteger('Server', 'Timeout', FTimeout);
  FRootDirectory := FIniFile.ReadString('Server', 'RootDirectory', FRootDirectory);

  FEnableSSL := FIniFile.ReadBool('SSL', 'Enable', FEnableSSL);
  FCertFile := FIniFile.ReadString('SSL', 'CertFile', FCertFile);
  FKeyFile := FIniFile.ReadString('SSL', 'KeyFile', FKeyFile);

  FEnableLogging := FIniFile.ReadBool('Logging', 'Enable', FEnableLogging);
  FLogFile := FIniFile.ReadString('Logging', 'LogFile', FLogFile);
end;

procedure TServerConfig.SaveToFile;  
begin
  FIniFile.WriteInteger('Server', 'Port', FPort);
  FIniFile.WriteInteger('Server', 'MaxConnections', FMaxConnections);
  FIniFile.WriteInteger('Server', 'ThreadPoolSize', FThreadPoolSize);
  FIniFile.WriteInteger('Server', 'Timeout', FTimeout);
  FIniFile.WriteString('Server', 'RootDirectory', FRootDirectory);

  FIniFile.WriteBool('SSL', 'Enable', FEnableSSL);
  FIniFile.WriteString('SSL', 'CertFile', FCertFile);
  FIniFile.WriteString('SSL', 'KeyFile', FKeyFile);

  FIniFile.WriteBool('Logging', 'Enable', FEnableLogging);
  FIniFile.WriteString('Logging', 'LogFile', FLogFile);

  FIniFile.UpdateFile;
end;

end.
```

### Exemple de fichier de configuration (server.ini)

```ini
[Server]
Port=8080  
MaxConnections=100  
ThreadPoolSize=4  
Timeout=30  
RootDirectory=/var/www/html

[SSL]
Enable=false  
CertFile=cert.pem  
KeyFile=key.pem

[Logging]
Enable=true  
LogFile=server.log
```

## Serveur complet avec toutes les fonctionnalités

```pascal
program AdvancedHTTPServer;
{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Sockets,
  HTTPParser, HTTPMethods, HTTPFileServer, HTTPConfig, HTTPLogger, HTTPSessions;

type
  TAdvancedServer = class
  private
    FConfig: TServerConfig;
    FLogger: THTTPLogger;
    FSessionManager: TSessionManager;
    FFileServer: TFileServer;
    FSocket: TSocket;
    FRunning: Boolean;
    procedure HandleClient(ClientSocket: TSocket; const ClientIP: string);
    procedure ProcessRequest(Request: THTTPRequest; var Response: THTTPResponse);
  public
    constructor Create(const ConfigFile: string);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

{ TAdvancedServer }

constructor TAdvancedServer.Create(const ConfigFile: string);  
begin
  FConfig := TServerConfig.Create(ConfigFile);
  FConfig.LoadFromFile;

  if FConfig.EnableLogging then
    FLogger := THTTPLogger.Create(FConfig.LogFile)
  else
    FLogger := nil;

  FSessionManager := TSessionManager.Create(30); // 30 minutes
  FFileServer := TFileServer.Create(FConfig.RootDirectory);

  FSocket := INVALID_SOCKET;
  FRunning := False;
end;

destructor TAdvancedServer.Destroy;  
begin
  Stop;
  FFileServer.Free;
  FSessionManager.Free;
  if Assigned(FLogger) then
    FLogger.Free;
  FConfig.Free;
  inherited;
end;

procedure TAdvancedServer.ProcessRequest(Request: THTTPRequest;
                                         var Response: THTTPResponse);
var
  Content, ContentType: string;
begin
  // Routing
  if Copy(Request.Path, 1, 5) = '/api/' then
  begin
    // API REST
    if Request.Path = '/api/time' then
    begin
      Content := '{"time":"' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '"}';
      Response := CreateResponse(200, Content);
      SetResponseHeader(Response, 'Content-Type', 'application/json');
    end
    else
    begin
      Response := CreateResponse(404, '{"error":"API endpoint not found"}');
      SetResponseHeader(Response, 'Content-Type', 'application/json');
    end;
  end
  else
  begin
    // Fichiers statiques
    if Request.Path = '/' then
      Request.FPath := '/index.html';

    if FFileServer.ServeFile(Request.Path, Content, ContentType) then
    begin
      Response := CreateResponse(200, Content);
      SetResponseHeader(Response, 'Content-Type', ContentType);

      // Cache control
      SetResponseHeader(Response, 'Cache-Control', 'public, max-age=3600');
    end
    else if DirectoryExists(FFileServer.GetFullPath(Request.Path)) then
    begin
      Content := FFileServer.GenerateDirectoryListing(Request.Path);
      Response := CreateResponse(200, Content);
      SetResponseHeader(Response, 'Content-Type', 'text/html; charset=utf-8');
    end
    else
    begin
      Response := CreateResponse(404,
        '<!DOCTYPE html><html><body><h1>404 Not Found</h1></body></html>');
      SetResponseHeader(Response, 'Content-Type', 'text/html');
    end;
  end;
end;

procedure TAdvancedServer.HandleClient(ClientSocket: TSocket;
                                       const ClientIP: string);
var
  Buffer: array[0..8191] of Char;
  Received: Integer;
  Request: THTTPRequest;
  Response: THTTPResponse;
  ResponseStr: string;
  StartTime, EndTime: TDateTime;
  ProcessTime: Double;
begin
  StartTime := Now;

  try
    Received := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer) - 1, 0);

    if Received > 0 then
    begin
      Buffer[Received] := #0;

      Request := THTTPRequest.Create;
      try
        if Request.Parse(Buffer) then
        begin
          // Traitement de la requête
          ProcessRequest(Request, Response);

          // Envoi de la réponse
          ResponseStr := ResponseToString(Response);
          fpSend(ClientSocket, @ResponseStr[1], Length(ResponseStr), 0);

          // Logging
          EndTime := Now;
          ProcessTime := (EndTime - StartTime) * 24 * 3600 * 1000; // en ms

          if Assigned(FLogger) then
          begin
            FLogger.LogRequest(Request.MethodStr, Request.Path, ClientIP,
                              Response.StatusCode);
            FLogger.Debug(Format('Process time: %.2f ms', [ProcessTime]));
          end;

          Response.Headers.Free;
        end;

      finally
        Request.Free;
      end;
    end;

  except
    on E: Exception do
      if Assigned(FLogger) then
        FLogger.Error('Exception: ' + E.Message);
  end;

  CloseSocket(ClientSocket);
end;

procedure TAdvancedServer.Start;  
var
  ServerAddr, ClientAddr: TInetSockAddr;
  ClientSocket: TSocket;
  AddrLen: TSockLen;
  OptVal: Integer;
begin
  if FRunning then
    Exit;

  FSocket := fpSocket(AF_INET, SOCK_STREAM, 0);

  if FSocket = INVALID_SOCKET then
    raise Exception.Create('Impossible de créer le socket');

  try
    OptVal := 1;
    fpSetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

    FillChar(ServerAddr, SizeOf(ServerAddr), 0);
    ServerAddr.sin_family := AF_INET;
    ServerAddr.sin_port := htons(FConfig.Port);
    ServerAddr.sin_addr.s_addr := INADDR_ANY;

    if fpBind(FSocket, @ServerAddr, SizeOf(ServerAddr)) < 0 then
      raise Exception.Create('Impossible de lier le port ' + IntToStr(FConfig.Port));

    if fpListen(FSocket, FConfig.MaxConnections) < 0 then
      raise Exception.Create('Impossible de se mettre en écoute');

    FRunning := True;

    WriteLn('=========================================');
    WriteLn('Serveur HTTP Avancé');
    WriteLn('=========================================');
    WriteLn('Port: ', FConfig.Port);
    WriteLn('URL: http://localhost:', FConfig.Port);
    WriteLn('Répertoire racine: ', FConfig.RootDirectory);
    WriteLn('Max connexions: ', FConfig.MaxConnections);
    WriteLn('Timeout: ', FConfig.Timeout, ' secondes');
    WriteLn('Logging: ', BoolToStr(FConfig.EnableLogging, 'Activé', 'Désactivé'));
    if FConfig.EnableLogging then
      WriteLn('Fichier log: ', FConfig.LogFile);
    WriteLn('=========================================');
    WriteLn('Appuyez sur Ctrl+C pour arrêter');
    WriteLn('=========================================');
    WriteLn;

    if Assigned(FLogger) then
      FLogger.Info('Serveur démarré sur le port ' + IntToStr(FConfig.Port));

    while FRunning do
    begin
      AddrLen := SizeOf(ClientAddr);
      ClientSocket := fpAccept(FSocket, @ClientAddr, @AddrLen);

      if ClientSocket >= 0 then
      begin
        HandleClient(ClientSocket, NetAddrToStr(ClientAddr.sin_addr));

        // Nettoyage périodique des sessions expirées
        FSessionManager.CleanupExpiredSessions;
      end;
    end;

  finally
    if FSocket <> INVALID_SOCKET then
    begin
      CloseSocket(FSocket);
      FSocket := INVALID_SOCKET;
    end;

    if Assigned(FLogger) then
      FLogger.Info('Serveur arrêté');
  end;
end;

procedure TAdvancedServer.Stop;  
begin
  FRunning := False;

  if FSocket <> INVALID_SOCKET then
  begin
    CloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
end;

var
  Server: TAdvancedServer;
  ConfigFile: string;

begin
  if ParamCount > 0 then
    ConfigFile := ParamStr(1)
  else
    ConfigFile := 'server.ini';

  WriteLn('Chargement de la configuration: ', ConfigFile);

  Server := TAdvancedServer.Create(ConfigFile);
  try
    Server.Start;
  finally
    Server.Free;
  end;
end.
```

## Tests avec curl

Voici quelques commandes pour tester votre serveur :

```bash
# Test simple GET
curl http://localhost:8080/

# Test avec headers
curl -v http://localhost:8080/api/time

# Test POST avec données JSON
curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{"name":"John Doe","email":"john@example.com"}'

# Test PUT
curl -X PUT http://localhost:8080/api/users/1 \
  -H "Content-Type: application/json" \
  -d '{"name":"Jane Doe"}'

# Test DELETE
curl -X DELETE http://localhost:8080/api/users/1

# Test avec authentification
curl -H "Authorization: Bearer your-token" \
  http://localhost:8080/api/protected

# Test téléchargement de fichier
curl -O http://localhost:8080/files/document.pdf

# Test avec cookie
curl -b "session_id=abc123" http://localhost:8080/dashboard

# Test HTTPS (avec certificat auto-signé)
curl -k https://localhost:8443/
```

## Déploiement

### Sur Ubuntu/Linux

```bash
# 1. Compilation
fpc -O3 AdvancedHTTPServer.pas

# 2. Création d'un service systemd
sudo nano /etc/systemd/system/myhttp.service
```

Contenu du fichier `myhttp.service` :

```ini
[Unit]
Description=Mon Serveur HTTP FreePascal  
After=network.target

[Service]
Type=simple  
User=www-data  
Group=www-data  
WorkingDirectory=/opt/myserver  
ExecStart=/opt/myserver/AdvancedHTTPServer  
Restart=always  
RestartSec=5

[Install]
WantedBy=multi-user.target
```

```bash
# 3. Activation et démarrage
sudo systemctl daemon-reload  
sudo systemctl enable myhttp.service  
sudo systemctl start myhttp.service

# 4. Vérification du statut
sudo systemctl status myhttp.service

# 5. Logs
sudo journalctl -u myhttp.service -f
```

### Sur Windows

```powershell
# 1. Compilation
fpc -O3 AdvancedHTTPServer.pas

# 2. Installation comme service Windows (nécessite NSSM)
# Télécharger NSSM: https://nssm.cc/download

nssm install MyHTTPServer "C:\Server\AdvancedHTTPServer.exe"  
nssm set MyHTTPServer AppDirectory "C:\Server"  
nssm start MyHTTPServer

# Vérification
nssm status MyHTTPServer
```

### Configuration du pare-feu

**Ubuntu :**
```bash
sudo ufw allow 8080/tcp  
sudo ufw reload
```

**Windows :**
```powershell
netsh advfirewall firewall add rule name="HTTP Server" dir=in action=allow protocol=TCP localport=8080
```

## Reverse Proxy avec Nginx

Pour la production, utilisez Nginx comme reverse proxy :

```nginx
# /etc/nginx/sites-available/myapp
server {
    listen 80;
    server_name example.com;

    location / {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

```bash
# Activation
sudo ln -s /etc/nginx/sites-available/myapp /etc/nginx/sites-enabled/  
sudo nginx -t  
sudo systemctl reload nginx
```

## Conclusion

Vous avez maintenant toutes les connaissances pour créer des serveurs HTTP/HTTPS personnalisés avec FreePascal. Voici un récapitulatif :

### Ce que nous avons couvert

✅ **Bases HTTP** : Requêtes, réponses, méthodes, headers  
✅ **Parsing HTTP** : Analyse complète des requêtes  
✅ **Routing** : Système de routes flexible  
✅ **Fichiers statiques** : Serveur de fichiers avec types MIME  
✅ **API REST** : CRUD complet avec JSON  
✅ **HTTPS/SSL** : Sécurisation avec OpenSSL  
✅ **Sessions** : Gestion des sessions utilisateur  
✅ **Multi-threading** : Performance avec threads  
✅ **Middleware** : Système de filtres extensible  
✅ **Logging** : Traçabilité et débogage  
✅ **Configuration** : Fichiers INI  
✅ **Sécurité** : Validation, sanitization  
✅ **Performance** : Cache, keep-alive, compression  
✅ **Déploiement** : Production sur Linux et Windows

### Quand utiliser un serveur HTTP personnalisé ?

**✅ Utilisez un serveur personnalisé pour :**
- Applications embarquées (Raspberry Pi, IoT)
- Microservices légers
- APIs internes d'entreprise
- Applications desktop avec interface web
- Apprentissage et compréhension d'HTTP
- Prototypes et POCs

**❌ Utilisez plutôt Apache/Nginx pour :**
- Sites web publics à fort trafic
- Applications nécessitant de nombreux modules
- Déploiements nécessitant un support commercial
- Projets avec des exigences de conformité strictes

### Ressources complémentaires

- **RFC 2616** : Spécification HTTP/1.1
- **RFC 7540** : HTTP/2
- **RFC 8446** : TLS 1.3
- **MDN Web Docs** : Documentation HTTP
- **OWASP** : Sécurité des applications web

Votre serveur HTTP personnalisé est maintenant prêt pour la production ! 🚀

⏭️ [Configuration SSL/TLS](/10-programmation-reseau-avancee/05-configuration-ssl-tls.md)
