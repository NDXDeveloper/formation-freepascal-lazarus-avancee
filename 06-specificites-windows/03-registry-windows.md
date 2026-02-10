🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Le Registre Windows avec FreePascal/Lazarus - Introduction

## Qu'est-ce que le Registre Windows ?

Le Registre Windows (Windows Registry) est une base de données hiérarchique centrale qui stocke les paramètres de configuration de Windows, des applications installées et des préférences utilisateur. C'est le cœur de la configuration de Windows depuis Windows 95.

### Analogie simple

Imaginez le Registre comme un gigantesque classeur à tiroirs :
- **Les armoires** (Ruches/Hives) : Les grandes catégories principales
- **Les tiroirs** (Clés) : Les dossiers organisés hiérarchiquement
- **Les dossiers suspendus** (Sous-clés) : Les sous-catégories
- **Les documents** (Valeurs) : Les paramètres réels stockés

Comme dans un bureau bien organisé, chaque information a sa place précise, et il faut connaître le chemin exact pour la retrouver.

## Pourquoi le Registre est-il important ?

### Pour Windows lui-même

Windows utilise le Registre pour stocker :
- **Configuration matérielle** : Drivers, périphériques, paramètres système
- **Configuration logicielle** : Applications installées, associations de fichiers
- **Profils utilisateurs** : Préférences, bureau, paramètres personnels
- **Sécurité** : Permissions, stratégies, certificats
- **Performance** : Cache, optimisations, priorités

### Pour vos applications

Votre application peut utiliser le Registre pour :
- **Sauvegarder ses paramètres** : Configuration persistante entre les redémarrages
- **S'intégrer à Windows** : Menu contextuel, démarrage automatique, associations
- **Gérer les licences** : Clés d'activation, périodes d'essai
- **Personnalisation par utilisateur** : Préférences différentes pour chaque compte
- **Communication inter-applications** : Partager des données avec d'autres programmes

## Structure du Registre : Les ruches principales

Le Registre est organisé en cinq ruches principales (plus quelques ruches dynamiques) :

### 1. HKEY_CLASSES_ROOT (HKCR)
```
But : Associations de fichiers et informations COM/OLE  
Contient :
- Extensions de fichiers (.txt, .docx, .exe...)
- Types MIME
- Identifiants de classe COM
- Associations d'applications

Exemple : Quel programme ouvre les fichiers .pdf ?
```

### 2. HKEY_CURRENT_USER (HKCU)
```
But : Configuration de l'utilisateur actuellement connecté  
Contient :
- Préférences du bureau
- Variables d'environnement utilisateur
- Configuration des applications pour cet utilisateur
- Connexions réseau mappées

Exemple : Fond d'écran, thème, paramètres de votre application
```

### 3. HKEY_LOCAL_MACHINE (HKLM)
```
But : Configuration globale de la machine  
Contient :
- HARDWARE : Configuration matérielle
- SOFTWARE : Applications installées pour tous
- SYSTEM : Configuration Windows
- SECURITY : Paramètres de sécurité

Exemple : Liste des programmes installés, configuration réseau
```

### 4. HKEY_USERS (HKU)
```
But : Profils de tous les utilisateurs  
Contient :
- .DEFAULT : Profil par défaut
- S-1-5-... : Profils des utilisateurs (par SID)
- Configuration individuelle de chaque compte

Note : HKCU est en fait un lien vers HKU\[SID_utilisateur_actuel]
```

### 5. HKEY_CURRENT_CONFIG (HKCC)
```
But : Configuration matérielle actuelle  
Contient :
- Profil matériel en cours d'utilisation
- Configuration d'affichage actuelle

Note : Lien vers HKLM\SYSTEM\CurrentControlSet\Hardware Profiles\Current
```

## Anatomie d'une entrée du Registre

### Structure hiérarchique

```
HKEY_CURRENT_USER                    <- Ruche (Hive)
    └── Software                      <- Clé
         └── MonEntreprise            <- Sous-clé
              └── MonApplication      <- Sous-clé
                   ├── Version        <- Valeur : "1.0.0"
                   ├── InstallPath    <- Valeur : "C:\Program Files\MonApp"
                   ├── LastRun        <- Valeur : "2024-01-15"
                   └── Settings       <- Sous-clé
                        ├── Theme     <- Valeur : "Dark"
                        └── Language  <- Valeur : "FR"
```

### Types de valeurs

Le Registre peut stocker différents types de données :

| Type | Nom constant | Description | Exemple |
|------|--------------|-------------|---------|
| **REG_SZ** | Chaîne | Texte simple | "MonApplication v1.0" |
| **REG_DWORD** | Entier 32 bits | Nombre entier | 42, 0xFF00 |
| **REG_QWORD** | Entier 64 bits | Grand nombre | 9223372036854775807 |
| **REG_BINARY** | Binaire | Données brutes | 0x4D 0x5A 0x90... |
| **REG_MULTI_SZ** | Multi-chaînes | Plusieurs lignes de texte | "Ligne1\0Ligne2\0Ligne3\0" |
| **REG_EXPAND_SZ** | Chaîne extensible | Texte avec variables | "%USERPROFILE%\Documents" |

### Chemin complet d'une clé

Pour accéder à une valeur, il faut connaître son chemin complet :

```
Ruche\Clé\Sous-clé\Sous-sous-clé\...\NomValeur

Exemple :  
HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Run
```

## Le Registre vs autres méthodes de configuration

### Comparaison avec les alternatives

| Méthode | Avantages | Inconvénients | Utilisation |
|---------|-----------|---------------|-------------|
| **Registre** | • Centralisé<br>• Rapide<br>• Intégré à Windows<br>• Hiérarchique | • Windows uniquement<br>• Peut être corrompu<br>• Nécessite privilèges<br>• Pas portable | Configuration Windows native |
| **Fichiers INI** | • Simple<br>• Portable<br>• Éditable manuellement<br>• Pas de privilèges | • Limité en structure<br>• Pas de types<br>• Performance (fichier)<br>• Localisation variable | Configuration simple et portable |
| **Fichiers XML** | • Structuré<br>• Portable<br>• Standard<br>• Validation possible | • Verbeux<br>• Plus lent<br>• Parsing nécessaire | Configuration complexe portable |
| **Fichiers JSON** | • Moderne<br>• Portable<br>• Léger<br>• Supporté partout | • Pas de commentaires<br>• Types limités<br>• Parsing nécessaire | Applications modernes |
| **Base de données** | • Requêtes complexes<br>• Transactions<br>• Multi-utilisateur | • Complexité<br>• Dépendance externe<br>• Overhead | Grandes applications |

### Quand utiliser le Registre ?

✅ **Utilisez le Registre pour :**
- Configuration système Windows
- Intégration avec l'Explorateur Windows
- Paramètres devant survivre à une réinstallation
- Démarrage automatique d'applications
- Associations de fichiers
- Configuration partagée entre utilisateurs

❌ **N'utilisez PAS le Registre pour :**
- Applications multi-plateformes
- Grandes quantités de données (> quelques KB)
- Données fréquemment modifiées
- Données temporaires
- Applications portables

## Sécurité et permissions

### Niveaux d'accès

Le Registre a différents niveaux de sécurité :

```
LECTURE SEULE
├── Tous les utilisateurs peuvent lire
├── Idéal pour récupérer des informations
└── Pas de risque de corruption

LECTURE/ÉCRITURE
├── HKCU : L'utilisateur peut modifier ses propres paramètres
├── HKLM : Nécessite les droits administrateur
└── Attention aux modifications système

CRÉATION/SUPPRESSION
├── Créer de nouvelles clés
├── Supprimer des clés existantes
└── Risque élevé si mal utilisé
```

### Bonnes pratiques de sécurité

1. **Principe du moindre privilège**
   - Utilisez HKCU plutôt que HKLM quand possible
   - Ne demandez les droits admin que si nécessaire

2. **Validation des données**
   - Vérifiez toujours les types de données lues
   - Gérez les cas où une clé n'existe pas

3. **Sauvegarde**
   - Exportez les clés importantes avant modification
   - Prévoyez une restauration en cas d'erreur

4. **Isolation**
   - Utilisez un espace de noms unique pour votre application
   - Évitez de modifier les clés système

## Outils pour explorer le Registre

### Outils Windows intégrés

#### 1. Éditeur du Registre (regedit.exe)
```
Lancement : Win+R → regedit  
Fonction : Interface graphique pour naviguer/modifier  
Utilité : Explorer, chercher, exporter/importer  
Attention : Modifications directes possibles !
```

#### 2. Ligne de commande (reg.exe)
```batch
REM Interroger une valeur  
reg query HKCU\Software\MonApp /v Version

REM Ajouter une valeur  
reg add HKCU\Software\MonApp /v Theme /t REG_SZ /d "Dark"

REM Exporter une clé  
reg export HKCU\Software\MonApp backup.reg

REM Supprimer une valeur  
reg delete HKCU\Software\MonApp /v OldSetting /f
```

#### 3. PowerShell
```powershell
# Lire une valeur
Get-ItemProperty -Path "HKCU:\Software\MonApp" -Name "Version"

# Écrire une valeur
Set-ItemProperty -Path "HKCU:\Software\MonApp" -Name "Theme" -Value "Dark"

# Créer une nouvelle clé
New-Item -Path "HKCU:\Software" -Name "MonApp"

# Lister toutes les valeurs
Get-ItemProperty -Path "HKCU:\Software\MonApp"
```

### Outils de surveillance

#### Process Monitor (ProcMon)
- Surveille les accès au Registre en temps réel
- Filtre par processus, clé, valeur
- Idéal pour déboguer les problèmes d'accès

#### Registry Monitor (RegMon) - Version legacy
- Outil dédié à la surveillance du Registre
- Plus simple que ProcMon pour le Registre uniquement

## Exemples d'utilisation courante

### 1. Démarrage automatique d'une application
```
Clé : HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Run  
Valeur : "MonApp" = "C:\Program Files\MonApp\MonApp.exe"  
Effet : L'application démarre à la connexion de l'utilisateur
```

### 2. Association de fichier
```
Clé : HKEY_CLASSES_ROOT\.monext  
Valeur par défaut : "MonApp.Document"

Clé : HKEY_CLASSES_ROOT\MonApp.Document\shell\open\command  
Valeur par défaut : "C:\Program Files\MonApp\MonApp.exe "%1""  
Effet : Double-clic sur .monext ouvre avec MonApp
```

### 3. Menu contextuel de l'Explorateur
```
Clé : HKEY_CLASSES_ROOT\*\shell\MonApp  
Valeur par défaut : "Ouvrir avec MonApp"

Clé : HKEY_CLASSES_ROOT\*\shell\MonApp\command  
Valeur par défaut : "C:\Program Files\MonApp\MonApp.exe "%1""  
Effet : "Ouvrir avec MonApp" dans le menu clic-droit
```

### 4. Configuration d'application
```
Clé : HKEY_CURRENT_USER\Software\MonEntreprise\MonApp\Settings  
Valeurs :
- "Theme" = "Dark"
- "Language" = "FR"
- "AutoSave" = dword:00000001
- "LastFile" = "C:\Users\User\Documents\projet.dat"
```

## Dangers et précautions

### ⚠️ Risques potentiels

1. **Corruption du Registre**
   - Windows peut ne plus démarrer
   - Applications peuvent dysfonctionner
   - Perte de configuration

2. **Conflits entre applications**
   - Écrasement de valeurs
   - Clés orphelines
   - Incompatibilités

3. **Problèmes de performance**
   - Registre gonflé (Registry bloat)
   - Recherches lentes
   - Fragmentation

4. **Sécurité**
   - Exposition d'informations sensibles
   - Malware utilisant le Registre
   - Modifications non autorisées

### 🛡️ Précautions essentielles

1. **Toujours sauvegarder**
   ```batch
   reg export HKCU\Software\MonApp backup.reg
   ```

2. **Tester en environnement isolé**
   - Machine virtuelle
   - Compte test
   - Snapshot système

3. **Gérer les erreurs**
   ```pascal
   try
     Registry.WriteString('Setting', Value);
   except
     on E: Exception do
       ShowMessage('Erreur Registre : ' + E.Message);
   end;
   ```

4. **Documenter les modifications**
   - Quelles clés sont utilisées
   - Quel est leur but
   - Comment les restaurer

## FreePascal/Lazarus et le Registre

### L'unité Registry

FreePascal fournit l'unité `Registry` qui simplifie grandement l'accès au Registre :

```pascal
uses
  Registry;  // Unité principale pour le Registre

var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey('Software\MonApp', True) then
    begin
      // Travailler avec le Registre
      Reg.WriteString('Version', '1.0.0');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;
```

### Avantages de l'approche FreePascal

1. **Abstraction de l'API Windows**
   - Plus simple que l'API Win32 directe
   - Gestion automatique des handles
   - Code plus lisible

2. **Gestion des erreurs intégrée**
   - Exceptions plutôt que codes d'erreur
   - Plus facile à déboguer

3. **Compatible Delphi**
   - Code portable entre Delphi et FreePascal
   - Syntaxe familière

4. **Support complet des types**
   - Tous les types de données du Registre
   - Conversion automatique

## Préparation pour la suite

Dans les sections suivantes, nous allons explorer :

1. **Lecture/écriture dans le registre** : Les opérations de base
2. **Surveillance des modifications** : Détecter les changements en temps réel
3. **Techniques avancées** : Import/export, permissions, transactions

Mais avant de commencer à coder, assurez-vous de :

✅ Comprendre la structure hiérarchique du Registre  
✅ Connaître la différence entre HKCU et HKLM  
✅ Avoir exploré le Registre avec regedit.exe  
✅ Avoir fait une sauvegarde de votre Registre  
✅ Comprendre les risques et précautions

## Résumé : L'essentiel à retenir

1. **Le Registre est la base de données de configuration de Windows**
2. **Structure hiérarchique** : Ruches → Clés → Valeurs
3. **5 ruches principales** : HKCR, HKCU, HKLM, HKU, HKCC
4. **Types de données variés** : Chaînes, entiers, binaire, multi-chaînes
5. **HKCU pour l'utilisateur**, HKLM pour le système (nécessite admin)
6. **Sauvegarder avant de modifier**
7. **FreePascal simplifie l'accès** avec l'unité Registry
8. **Utiliser avec parcimonie** : Le Registre n'est pas une base de données

Maintenant que vous comprenez ce qu'est le Registre Windows et pourquoi il est important, nous allons apprendre à l'utiliser concrètement avec FreePascal/Lazarus.

⏭️ [Lecture/écriture dans le registre](/06-specificites-windows/03.1-lecture-ecriture-registre.md)
