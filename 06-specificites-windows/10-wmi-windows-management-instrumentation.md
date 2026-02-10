🔝 Retour au [Sommaire](/SOMMAIRE.md)

# WMI (Windows Management Instrumentation) avec FreePascal/Lazarus

## Introduction : Qu'est-ce que WMI ?

WMI (Windows Management Instrumentation) est une technologie Microsoft qui permet d'accéder aux informations système et de gérer les ordinateurs Windows. C'est comme une grande base de données qui contient toutes les informations sur votre système : matériel, logiciels, processus, services, etc.

Imaginez WMI comme un annuaire téléphonique géant de votre ordinateur où vous pouvez chercher n'importe quelle information : combien de RAM avez-vous ? Quel est le modèle de votre processeur ? Quels programmes sont installés ? Quels processus sont en cours d'exécution ?

### Pourquoi utiliser WMI dans vos programmes FreePascal ?

- **Obtenir des informations système** : température CPU, espace disque, informations réseau
- **Surveiller le système** : détecter l'insertion d'une clé USB, surveiller les processus
- **Gérer le système** : démarrer/arrêter des services, créer des processus
- **Inventaire** : lister le matériel et les logiciels installés

## Prérequis

### Ce dont vous avez besoin

1. **FreePascal/Lazarus** installé sur Windows (WMI n'existe que sur Windows)
2. **Droits d'administrateur** pour certaines opérations WMI
3. **Les bonnes unités** dans votre projet FreePascal

### Unités nécessaires

```pascal
uses
  Windows,    // Pour les types Windows de base
  ActiveX,    // Pour COM et les interfaces
  ComObj,     // Pour créer des objets COM
  Variants;   // Pour gérer les types Variant
```

## Concepts de base

### La structure WMI

WMI organise les informations en :

- **Classes** : catégories d'objets (Win32_Process pour les processus, Win32_LogicalDisk pour les disques)
- **Propriétés** : caractéristiques d'un objet (Name, Size, FreeSpace)
- **Méthodes** : actions qu'on peut effectuer (Create, Delete, Terminate)
- **Instances** : objets réels (chaque processus en cours est une instance de Win32_Process)

### Le langage WQL

WMI utilise WQL (WMI Query Language), similaire à SQL mais pour interroger le système :

```sql
SELECT * FROM Win32_Process  
SELECT Name, ProcessId FROM Win32_Process WHERE Name = 'notepad.exe'
```

## Configuration initiale

### Initialiser COM

WMI utilise COM (Component Object Model), donc nous devons l'initialiser :

```pascal
program WMIExample;

uses
  Windows, ActiveX, ComObj, Variants, SysUtils;

begin
  // Initialiser COM - TOUJOURS nécessaire pour WMI
  CoInitialize(nil);
  try
    // Votre code WMI ici

  finally
    // Libérer COM - TOUJOURS faire ceci
    CoUninitialize;
  end;
end.
```

## Premier exemple : Obtenir des informations système

### Connexion à WMI

```pascal
procedure GetSystemInfo;  
var
  WMIService: OLEVariant;
  WQLQuery: string;
  ObjectSet: OLEVariant;
  SystemItem: OLEVariant;
  Enum: IEnumVariant;
  Value: LongWord;
begin
  try
    // Se connecter au service WMI local
    WMIService := CreateOleObject('WbemScripting.SWbemLocator')
                    .ConnectServer('localhost', 'root\CIMV2', '', '');

    // Créer une requête pour obtenir les informations du système
    WQLQuery := 'SELECT * FROM Win32_ComputerSystem';

    // Exécuter la requête
    ObjectSet := WMIService.ExecQuery(WQLQuery);

    // Parcourir les résultats
    Enum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;
    while Enum.Next(1, SystemItem, Value) = S_OK do
    begin
      WriteLn('Nom de l''ordinateur: ', SystemItem.Name);
      WriteLn('Fabricant: ', SystemItem.Manufacturer);
      WriteLn('Modèle: ', SystemItem.Model);
      WriteLn('RAM totale: ', Round(SystemItem.TotalPhysicalMemory / 1024 / 1024 / 1024), ' GB');
      WriteLn('Nombre de processeurs: ', SystemItem.NumberOfProcessors);
    end;

  except
    on E: Exception do
      WriteLn('Erreur: ', E.Message);
  end;
end;
```

## Exemples pratiques courants

### 1. Lister les processus en cours

```pascal
procedure ListProcesses;  
var
  WMIService, ObjectSet, ProcessItem: OLEVariant;
  Enum: IEnumVariant;
  Value: LongWord;
begin
  // Connexion à WMI
  WMIService := CreateOleObject('WbemScripting.SWbemLocator')
                  .ConnectServer('localhost', 'root\CIMV2', '', '');

  // Requête pour obtenir tous les processus
  ObjectSet := WMIService.ExecQuery('SELECT * FROM Win32_Process');

  // Parcourir et afficher
  Enum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;
  while Enum.Next(1, ProcessItem, Value) = S_OK do
  begin
    WriteLn('Processus: ', ProcessItem.Name,
            ' (PID: ', ProcessItem.ProcessId, ')');
  end;
end;
```

### 2. Informations sur les disques

```pascal
procedure GetDiskInfo;  
var
  WMIService, ObjectSet, DiskItem: OLEVariant;
  Enum: IEnumVariant;
  Value: LongWord;
  FreeSpace, TotalSize: Int64;
begin
  WMIService := CreateOleObject('WbemScripting.SWbemLocator')
                  .ConnectServer('localhost', 'root\CIMV2', '', '');

  // Requête pour les disques locaux
  ObjectSet := WMIService.ExecQuery(
    'SELECT * FROM Win32_LogicalDisk WHERE DriveType = 3');

  Enum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;
  while Enum.Next(1, DiskItem, Value) = S_OK do
  begin
    FreeSpace := DiskItem.FreeSpace;
    TotalSize := DiskItem.Size;

    WriteLn('Disque: ', DiskItem.DeviceID);
    WriteLn('  Nom: ', DiskItem.VolumeName);
    WriteLn('  Espace libre: ', FreeSpace div (1024*1024*1024), ' GB');
    WriteLn('  Taille totale: ', TotalSize div (1024*1024*1024), ' GB');
    WriteLn('  Système de fichiers: ', DiskItem.FileSystem);
    WriteLn('---');
  end;
end;
```

### 3. Surveiller l'insertion de clés USB

```pascal
procedure MonitorUSBInsertion;  
var
  WMIService, EventSource, USBEvent: OLEVariant;
  WQLEventQuery: string;
begin
  WriteLn('Surveillance des clés USB. Appuyez sur Ctrl+C pour arrêter...');

  WMIService := CreateOleObject('WbemScripting.SWbemLocator')
                  .ConnectServer('localhost', 'root\CIMV2', '', '');

  // Créer une requête d'événement pour détecter l'insertion USB
  WQLEventQuery := 'SELECT * FROM __InstanceCreationEvent WITHIN 2 ' +
                   'WHERE TargetInstance ISA ''Win32_LogicalDisk'' ' +
                   'AND TargetInstance.DriveType = 2';

  // S'abonner aux événements
  EventSource := WMIService.ExecNotificationQuery(WQLEventQuery);

  // Boucle d'attente des événements
  repeat
    USBEvent := EventSource.NextEvent(1000); // Timeout de 1 seconde
    if not VarIsNull(USBEvent) then
    begin
      WriteLn('Clé USB insérée !');
      WriteLn('  Lecteur: ', USBEvent.TargetInstance.DeviceID);
      WriteLn('  Nom: ', USBEvent.TargetInstance.VolumeName);
    end;
  until False; // Boucle infinie - arrêt avec Ctrl+C
end;
```

### 4. Obtenir les logiciels installés

```pascal
procedure ListInstalledSoftware;  
var
  WMIService, ObjectSet, SoftwareItem: OLEVariant;
  Enum: IEnumVariant;
  Value: LongWord;
begin
  WMIService := CreateOleObject('WbemScripting.SWbemLocator')
                  .ConnectServer('localhost', 'root\CIMV2', '', '');

  // Requête pour les programmes installés
  ObjectSet := WMIService.ExecQuery('SELECT * FROM Win32_Product');

  WriteLn('Logiciels installés:');
  WriteLn('==================');

  Enum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;
  while Enum.Next(1, SoftwareItem, Value) = S_OK do
  begin
    WriteLn('Nom: ', SoftwareItem.Name);
    WriteLn('  Version: ', SoftwareItem.Version);
    WriteLn('  Éditeur: ', SoftwareItem.Vendor);
    WriteLn('  Date d''installation: ', SoftwareItem.InstallDate);
    WriteLn('---');
  end;
end;
```

## Intégration dans une application Lazarus

### Créer une application graphique avec WMI

```pascal
unit MainForm;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Windows, ActiveX, ComObj, Variants;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    ListView1: TListView;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure RefreshProcessList;
  public
  end;

var
  Form1: TForm1;

implementation

procedure TForm1.FormCreate(Sender: TObject);  
begin
  // Initialiser COM au démarrage
  CoInitialize(nil);

  // Configurer le ListView
  ListView1.ViewStyle := vsReport;
  ListView1.Columns.Add.Caption := 'Processus';
  ListView1.Columns.Add.Caption := 'PID';
  ListView1.Columns.Add.Caption := 'Mémoire (MB)';

  // Ajuster la largeur des colonnes
  ListView1.Columns[0].Width := 200;
  ListView1.Columns[1].Width := 80;
  ListView1.Columns[2].Width := 100;
end;

procedure TForm1.FormDestroy(Sender: TObject);  
begin
  // Libérer COM à la fermeture
  CoUninitialize;
end;

procedure TForm1.Button1Click(Sender: TObject);  
begin
  RefreshProcessList;
end;

procedure TForm1.RefreshProcessList;  
var
  WMIService, ObjectSet, ProcessItem: OLEVariant;
  Enum: IEnumVariant;
  Value: LongWord;
  ListItem: TListItem;
  MemoryMB: Double;
begin
  ListView1.Clear;

  try
    // Connexion à WMI
    WMIService := CreateOleObject('WbemScripting.SWbemLocator')
                    .ConnectServer('localhost', 'root\CIMV2', '', '');

    // Obtenir la liste des processus
    ObjectSet := WMIService.ExecQuery('SELECT * FROM Win32_Process');

    // Remplir le ListView
    Enum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;
    while Enum.Next(1, ProcessItem, Value) = S_OK do
    begin
      ListItem := ListView1.Items.Add;
      ListItem.Caption := ProcessItem.Name;
      ListItem.SubItems.Add(IntToStr(ProcessItem.ProcessId));

      // Convertir la mémoire en MB
      if not VarIsNull(ProcessItem.WorkingSetSize) then
      begin
        MemoryMB := ProcessItem.WorkingSetSize / 1024 / 1024;
        ListItem.SubItems.Add(FormatFloat('0.00', MemoryMB));
      end
      else
        ListItem.SubItems.Add('N/A');
    end;

    Memo1.Lines.Add('Liste actualisée: ' + IntToStr(ListView1.Items.Count) + ' processus');

  except
    on E: Exception do
    begin
      Memo1.Lines.Add('Erreur: ' + E.Message);
      ShowMessage('Erreur WMI: ' + E.Message);
    end;
  end;
end;

end.
```

## Gestion des erreurs

### Erreurs courantes et solutions

```pascal
procedure SafeWMIQuery(const Query: string);  
var
  WMIService, ObjectSet: OLEVariant;
begin
  try
    // Tentative de connexion
    WMIService := CreateOleObject('WbemScripting.SWbemLocator')
                    .ConnectServer('localhost', 'root\CIMV2', '', '');

    // Exécuter la requête
    ObjectSet := WMIService.ExecQuery(Query);

    // Traiter les résultats...

  except
    on E: EOleException do
    begin
      case E.ErrorCode of
        $80041003: WriteLn('Erreur: Accès refusé - Exécutez en tant qu''administrateur');
        $80041010: WriteLn('Erreur: Classe WMI non trouvée');
        $80041017: WriteLn('Erreur: Requête WQL invalide');
        $80041002: WriteLn('Erreur: Objet non trouvé');
      else
        WriteLn('Erreur WMI: ', E.Message, ' (Code: ', IntToHex(E.ErrorCode, 8), ')');
      end;
    end;
    on E: Exception do
      WriteLn('Erreur générale: ', E.Message);
  end;
end;
```

## Classes WMI utiles

### Les classes les plus couramment utilisées

| Classe | Description | Utilisation |
|--------|-------------|------------|
| **Win32_Process** | Processus en cours | Gestion des programmes |
| **Win32_Service** | Services Windows | Contrôle des services |
| **Win32_LogicalDisk** | Disques logiques | Informations stockage |
| **Win32_PhysicalMemory** | Modules RAM | Détails mémoire |
| **Win32_Processor** | Processeur(s) | Info CPU |
| **Win32_NetworkAdapter** | Cartes réseau | Configuration réseau |
| **Win32_OperatingSystem** | Système d'exploitation | Version Windows |
| **Win32_Product** | Logiciels installés | Inventaire logiciel |
| **Win32_StartupCommand** | Programmes au démarrage | Gestion démarrage |
| **Win32_UserAccount** | Comptes utilisateurs | Gestion utilisateurs |

## Optimisation et bonnes pratiques

### 1. Limiter les requêtes

```pascal
// MAUVAIS - récupère toutes les propriétés
ObjectSet := WMIService.ExecQuery('SELECT * FROM Win32_Process');

// BON - récupère seulement ce dont on a besoin
ObjectSet := WMIService.ExecQuery('SELECT Name, ProcessId FROM Win32_Process');
```

### 2. Utiliser des filtres WHERE

```pascal
// MAUVAIS - récupère tout puis filtre
ObjectSet := WMIService.ExecQuery('SELECT * FROM Win32_Process');
// puis parcourir pour trouver notepad.exe

// BON - filtre directement dans la requête
ObjectSet := WMIService.ExecQuery(
  'SELECT * FROM Win32_Process WHERE Name = ''notepad.exe''');
```

### 3. Libérer les ressources

```pascal
procedure CleanWMIQuery;  
var
  WMIService, ObjectSet: OLEVariant;
begin
  try
    WMIService := CreateOleObject('WbemScripting.SWbemLocator')
                    .ConnectServer('localhost', 'root\CIMV2', '', '');

    ObjectSet := WMIService.ExecQuery('SELECT * FROM Win32_Process');

    // Utiliser les données...

  finally
    // Libérer explicitement les variants
    ObjectSet := Unassigned;
    WMIService := Unassigned;
  end;
end;
```

### 4. Gérer les valeurs NULL

```pascal
// Toujours vérifier les valeurs NULL avant utilisation
if not VarIsNull(ProcessItem.Description) then
  WriteLn('Description: ', ProcessItem.Description)
else
  WriteLn('Description: Non disponible');
```

## Limitations et alternatives

### Limitations de WMI

- **Windows uniquement** : WMI n'existe pas sur Linux/macOS
- **Performances** : Peut être lent pour certaines requêtes
- **Droits administrateur** : Certaines opérations nécessitent des privilèges élevés
- **Complexité** : La syntaxe COM/OLE peut être déroutante

### Alternatives pour le multi-plateforme

Si vous développez une application multi-plateforme :

```pascal
{$IFDEF WINDOWS}
  // Utiliser WMI sur Windows
  UseWMIForSystemInfo;
{$ENDIF}

{$IFDEF LINUX}
  // Utiliser /proc ou des commandes système sur Linux
  ReadProcForSystemInfo;
{$ENDIF}
```

## Ressources supplémentaires

### Documentation et outils

1. **WMI Explorer** : Outil gratuit pour explorer les classes WMI
2. **Documentation Microsoft** : docs.microsoft.com/en-us/windows/win32/wmisdk/
3. **PowerShell** : Tester vos requêtes WQL avec `Get-WmiObject`

### Tester une requête WQL dans PowerShell

```powershell
# Dans PowerShell, tester avant d'implémenter en Pascal
Get-WmiObject -Query "SELECT * FROM Win32_Process WHERE Name = 'notepad.exe'"
```

## Conclusion

WMI est un outil puissant pour interagir avec le système Windows depuis vos applications FreePascal/Lazarus. Bien qu'il puisse sembler complexe au début, une fois les concepts de base maîtrisés, il devient un allié précieux pour :

- Créer des outils d'administration système
- Surveiller les performances
- Gérer des inventaires matériels et logiciels
- Automatiser des tâches Windows

N'oubliez pas les points essentiels :
- Toujours initialiser et libérer COM
- Gérer les erreurs correctement
- Vérifier les valeurs NULL
- Optimiser vos requêtes WQL

Avec ces bases, vous êtes prêt à intégrer WMI dans vos projets FreePascal/Lazarus !

⏭️ [DirectX et technologies multimédia Windows](/06-specificites-windows/11-directx-technologies-multimedia-windows.md)
