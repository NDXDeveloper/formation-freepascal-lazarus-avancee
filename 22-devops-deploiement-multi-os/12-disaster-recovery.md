🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.12 Disaster recovery

## Introduction au disaster recovery

### Qu'est-ce que le disaster recovery ?

Le **disaster recovery** (plan de reprise après sinistre) est l'ensemble des stratégies, procédures et outils qui permettent de restaurer votre application et vos données après un incident majeur.

**L'analogie de l'incendie** :

Imaginez que votre maison prend feu. Vous avez plusieurs niveaux de protection :
1. **Prévention** : Détecteurs de fumée, extincteurs (monitoring, alertes)
2. **Copies de sauvegarde** : Photos de famille dans un coffre-fort ailleurs (backups)
3. **Assurance** : Pour reconstruire si tout est perdu (plan de reprise)
4. **Plan d'évacuation** : Où aller, qui appeler, quoi faire (procédures documentées)

Le disaster recovery, c'est exactement ça pour votre application FreePascal !

### Types de sinistres

**Incidents matériels** :
- 💾 Panne de disque dur
- 🔥 Incendie dans le datacenter
- ⚡ Panne électrique prolongée
- 🌊 Inondation

**Incidents logiciels** :
- 🐛 Bug critique qui corrompt les données
- 🚫 Déploiement raté
- 🔐 Attaque de ransomware
- 🗑️ Suppression accidentelle

**Incidents humains** :
- 👤 Employé mécontent
- 🤦 Erreur de manipulation
- 📝 Mauvaise commande SQL (DROP TABLE)

**Incidents externes** :
- ☁️ Panne du fournisseur cloud
- 🌐 Problème réseau global
- 🏛️ Catastrophe naturelle

### Métriques clés

#### RTO (Recovery Time Objective)

**Définition** : Temps maximum acceptable avant que l'application soit de nouveau fonctionnelle.

**Exemples** :
- Application critique bancaire : RTO = 1 heure
- Site e-commerce : RTO = 4 heures
- Application interne : RTO = 24 heures

#### RPO (Recovery Point Objective)

**Définition** : Quantité de données maximale qu'on peut se permettre de perdre (en temps).

**Exemples** :
- Transactions financières : RPO = 0 (aucune perte)
- Application de vente : RPO = 15 minutes
- Blog : RPO = 24 heures

```
    Dernière sauvegarde          Incident
           ↓                        ↓
    ───────●────────────────────────X─────→ Temps
           ←──────── RPO ──────────→
                                    ←─ RTO ─→
                                             ↓
                                         Récupération
```

**Illustration** :
- **RPO = 1 heure** : Vous perdez au maximum 1 heure de données
- **RTO = 2 heures** : Votre système est de nouveau opérationnel en 2 heures maximum

### Niveaux de disaster recovery (Tiers)

**Tier 0 - Aucune reprise** :
- Pas de backup
- Reconstruction manuelle
- RTO/RPO = plusieurs jours

**Tier 1 - Backup et restauration** :
- Sauvegardes régulières
- Restauration manuelle
- RTO = 24-72 heures
- RPO = heures à jours

**Tier 2 - Site de secours froid** :
- Infrastructure de secours non démarrée
- RTO = 12-24 heures
- RPO = heures

**Tier 3 - Site de secours chaud** :
- Infrastructure de secours prête
- RTO = quelques heures
- RPO = minutes à heures

**Tier 4 - Site miroir actif-actif** :
- Deux sites actifs simultanément
- RTO = minutes
- RPO = secondes à minutes

## Stratégie de sauvegarde

### Règle 3-2-1

**Principe fondamental** :
- **3** copies de vos données
- Sur **2** types de supports différents
- **1** copie hors site (offsite)

**Exemple pour une application FreePascal** :
```
Copie 1 : Base de données en production (serveur principal)
Copie 2 : Backup quotidien sur disque externe dans le datacenter
Copie 3 : Backup dans le cloud (AWS S3, Azure Blob, autre datacenter)
```

### Types de sauvegardes

#### 1. Sauvegarde complète (Full Backup)

**Principe** : Copie intégrale de toutes les données.

**Avantages** :
- ✅ Restauration simple et rapide
- ✅ Un seul fichier à restaurer

**Inconvénients** :
- ❌ Prend beaucoup d'espace
- ❌ Longue durée de sauvegarde

**Fréquence recommandée** : Hebdomadaire

#### 2. Sauvegarde incrémentale (Incremental Backup)

**Principe** : Sauvegarde uniquement ce qui a changé depuis la **dernière sauvegarde** (complète ou incrémentale).

**Avantages** :
- ✅ Rapide
- ✅ Peu d'espace disque

**Inconvénients** :
- ❌ Restauration plus complexe (besoin de tous les incréments)

**Fréquence recommandée** : Quotidienne

#### 3. Sauvegarde différentielle (Differential Backup)

**Principe** : Sauvegarde ce qui a changé depuis la **dernière sauvegarde complète**.

**Avantages** :
- ✅ Restauration plus simple qu'incrémentale
- ✅ Plus rapide que complète

**Inconvénients** :
- ❌ Taille augmente jusqu'à la prochaine complète

**Fréquence recommandée** : Quotidienne

### Schéma de sauvegarde complet

```
Dimanche    : Full Backup         (100 GB)
Lundi       : Incremental         (5 GB)   - depuis Dimanche
Mardi       : Incremental         (3 GB)   - depuis Lundi
Mercredi    : Incremental         (4 GB)   - depuis Mardi
Jeudi       : Incremental         (6 GB)   - depuis Mercredi
Vendredi    : Incremental         (8 GB)   - depuis Jeudi
Samedi      : Incremental         (2 GB)   - depuis Vendredi
Dimanche    : Full Backup         (100 GB)
```

**Pour restaurer Vendredi** :
- Restaurer Full Backup (Dimanche)
- Appliquer tous les incréments (Lundi → Vendredi)

## Sauvegarde de base de données

### PostgreSQL

#### Script de backup automatique

```bash
#!/bin/bash
# backup-postgres.sh - Sauvegarde PostgreSQL

# Configuration
DB_NAME="myappdb"
DB_USER="postgres"
BACKUP_DIR="/backups/postgres"
RETENTION_DAYS=30
DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="${BACKUP_DIR}/${DB_NAME}_${DATE}.sql.gz"

# Créer le répertoire si nécessaire
mkdir -p "$BACKUP_DIR"

# Effectuer le backup avec compression
echo "[$(date)] Début du backup de $DB_NAME..."
pg_dump -U "$DB_USER" -h localhost "$DB_NAME" | gzip > "$BACKUP_FILE"

if [ $? -eq 0 ]; then
    echo "[$(date)] ✓ Backup créé: $BACKUP_FILE"

    # Taille du fichier
    SIZE=$(du -h "$BACKUP_FILE" | cut -f1)
    echo "[$(date)] Taille: $SIZE"

    # Vérifier l'intégrité
    if gzip -t "$BACKUP_FILE"; then
        echo "[$(date)] ✓ Intégrité vérifiée"
    else
        echo "[$(date)] ✗ Erreur d'intégrité!"
        exit 1
    fi

    # Supprimer les anciens backups
    find "$BACKUP_DIR" -name "${DB_NAME}_*.sql.gz" -mtime +$RETENTION_DAYS -delete
    echo "[$(date)] ✓ Anciens backups supprimés (>$RETENTION_DAYS jours)"

    # Upload vers le cloud (optionnel)
    # aws s3 cp "$BACKUP_FILE" s3://my-backups/postgres/

else
    echo "[$(date)] ✗ Échec du backup!"
    exit 1
fi

echo "[$(date)] Backup terminé"
```

**Configurer la tâche cron** :

```bash
# Éditer crontab
crontab -e

# Ajouter (backup quotidien à 2h du matin)
0 2 * * * /path/to/backup-postgres.sh >> /var/log/backup-postgres.log 2>&1
```

#### Restauration PostgreSQL

```bash
#!/bin/bash
# restore-postgres.sh - Restauration PostgreSQL

DB_NAME="myappdb"
DB_USER="postgres"
BACKUP_FILE="$1"

if [ -z "$BACKUP_FILE" ]; then
    echo "Usage: $0 <backup_file.sql.gz>"
    exit 1
fi

if [ ! -f "$BACKUP_FILE" ]; then
    echo "✗ Fichier non trouvé: $BACKUP_FILE"
    exit 1
fi

echo "⚠️  ATTENTION: Cette opération va écraser la base de données $DB_NAME"
read -p "Continuer? (oui/non): " CONFIRM

if [ "$CONFIRM" != "oui" ]; then
    echo "Annulé"
    exit 0
fi

echo "[$(date)] Arrêt des connexions..."
psql -U "$DB_USER" -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '$DB_NAME';"

echo "[$(date)] Suppression de l'ancienne base..."
dropdb -U "$DB_USER" "$DB_NAME"

echo "[$(date)] Création de la nouvelle base..."
createdb -U "$DB_USER" "$DB_NAME"

echo "[$(date)] Restauration depuis $BACKUP_FILE..."
gunzip -c "$BACKUP_FILE" | psql -U "$DB_USER" "$DB_NAME"

if [ $? -eq 0 ]; then
    echo "[$(date)] ✓ Restauration réussie"
else
    echo "[$(date)] ✗ Échec de la restauration"
    exit 1
fi
```

### MySQL/MariaDB

#### Backup MySQL

```bash
#!/bin/bash
# backup-mysql.sh

DB_NAME="myappdb"
DB_USER="root"
DB_PASSWORD="password"
BACKUP_DIR="/backups/mysql"
DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="${BACKUP_DIR}/${DB_NAME}_${DATE}.sql.gz"

mkdir -p "$BACKUP_DIR"

mysqldump -u "$DB_USER" -p"$DB_PASSWORD" \
    --single-transaction \
    --routines \
    --triggers \
    --events \
    "$DB_NAME" | gzip > "$BACKUP_FILE"

if [ $? -eq 0 ]; then
    echo "✓ Backup créé: $BACKUP_FILE"
else
    echo "✗ Échec du backup"
    exit 1
fi
```

#### Restauration MySQL

```bash
#!/bin/bash
# restore-mysql.sh

DB_NAME="myappdb"
DB_USER="root"
DB_PASSWORD="password"
BACKUP_FILE="$1"

if [ -z "$BACKUP_FILE" ]; then
    echo "Usage: $0 <backup_file.sql.gz>"
    exit 1
fi

gunzip -c "$BACKUP_FILE" | mysql -u "$DB_USER" -p"$DB_PASSWORD" "$DB_NAME"

if [ $? -eq 0 ]; then
    echo "✓ Restauration réussie"
else
    echo "✗ Échec de la restauration"
    exit 1
fi
```

### SQLite

#### Backup SQLite depuis FreePascal

```pascal
unit SQLiteBackup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB;

type
  TSQLiteBackupManager = class
  private
    FConnection: TSQLite3Connection;
    FBackupDir: string;
    FRetentionDays: Integer;
  public
    constructor Create(AConnection: TSQLite3Connection; const ABackupDir: string);

    function CreateBackup: string;
    function RestoreBackup(const BackupFile: string): Boolean;
    procedure CleanOldBackups;
    function VerifyBackup(const BackupFile: string): Boolean;

    property RetentionDays: Integer read FRetentionDays write FRetentionDays;
  end;

implementation

uses
  DateUtils, FileUtil;

constructor TSQLiteBackupManager.Create(AConnection: TSQLite3Connection;
                                       const ABackupDir: string);
begin
  FConnection := AConnection;
  FBackupDir := ABackupDir;
  FRetentionDays := 30;

  // Créer le répertoire de backup si nécessaire
  if not DirectoryExists(FBackupDir) then
    ForceDirectories(FBackupDir);
end;

function TSQLiteBackupManager.CreateBackup: string;
var
  SourceFile, BackupFile: string;
  Timestamp: string;
begin
  SourceFile := FConnection.DatabaseName;
  Timestamp := FormatDateTime('yyyymmdd_hhnnss', Now);
  BackupFile := IncludeTrailingPathDelimiter(FBackupDir) +
                'backup_' + Timestamp + '.db';

  try
    // SQLite peut être copié directement si fermé,
    // sinon utiliser VACUUM INTO ou pg_backup API

    // Méthode simple : copie de fichier
    if FileExists(SourceFile) then
    begin
      CopyFile(SourceFile, BackupFile);

      // Vérifier l'intégrité
      if VerifyBackup(BackupFile) then
      begin
        WriteLn('✓ Backup créé: ', BackupFile);
        Result := BackupFile;
      end
      else
      begin
        DeleteFile(BackupFile);
        raise Exception.Create('Backup corrompu');
      end;
    end
    else
      raise Exception.Create('Fichier source introuvable');

  except
    on E: Exception do
    begin
      WriteLn('✗ Erreur de backup: ', E.Message);
      Result := '';
    end;
  end;
end;

function TSQLiteBackupManager.RestoreBackup(const BackupFile: string): Boolean;
var
  TargetFile: string;
begin
  Result := False;

  if not FileExists(BackupFile) then
  begin
    WriteLn('✗ Fichier de backup introuvable: ', BackupFile);
    Exit;
  end;

  // Vérifier l'intégrité avant restauration
  if not VerifyBackup(BackupFile) then
  begin
    WriteLn('✗ Backup corrompu, restauration annulée');
    Exit;
  end;

  TargetFile := FConnection.DatabaseName;

  try
    // Fermer la connexion
    if FConnection.Connected then
      FConnection.Close;

    // Sauvegarder l'actuelle (au cas où)
    if FileExists(TargetFile) then
      RenameFile(TargetFile, TargetFile + '.before_restore');

    // Copier le backup
    CopyFile(BackupFile, TargetFile);

    // Rouvrir la connexion
    FConnection.Open;

    WriteLn('✓ Restauration réussie depuis: ', BackupFile);
    Result := True;

  except
    on E: Exception do
    begin
      WriteLn('✗ Erreur de restauration: ', E.Message);

      // Restaurer l'ancienne version si échec
      if FileExists(TargetFile + '.before_restore') then
      begin
        DeleteFile(TargetFile);
        RenameFile(TargetFile + '.before_restore', TargetFile);
      end;
    end;
  end;
end;

procedure TSQLiteBackupManager.CleanOldBackups;
var
  SearchRec: TSearchRec;
  FilePath: string;
  BackupAge: TDateTime;
begin
  if FindFirst(IncludeTrailingPathDelimiter(FBackupDir) + 'backup_*.db',
               faAnyFile, SearchRec) = 0 then
  begin
    repeat
      FilePath := IncludeTrailingPathDelimiter(FBackupDir) + SearchRec.Name;
      BackupAge := FileDateToDateTime(SysUtils.FileAge(FilePath));

      if DaysBetween(Now, BackupAge) > FRetentionDays then
      begin
        DeleteFile(FilePath);
        WriteLn('✓ Ancien backup supprimé: ', SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;

    FindClose(SearchRec);
  end;
end;

function TSQLiteBackupManager.VerifyBackup(const BackupFile: string): Boolean;
var
  TestConnection: TSQLite3Connection;
  Query: TSQLQuery;
begin
  Result := False;

  TestConnection := TSQLite3Connection.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    TestConnection.DatabaseName := BackupFile;
    Query.Database := TestConnection;

    try
      TestConnection.Open;

      // Vérifier l'intégrité
      Query.SQL.Text := 'PRAGMA integrity_check';
      Query.Open;

      Result := (Query.Fields[0].AsString = 'ok');
      Query.Close;

      TestConnection.Close;
    except
      Result := False;
    end;
  finally
    Query.Free;
    TestConnection.Free;
  end;
end;

end.
```

## Sauvegarde des fichiers applicatifs

### Script de backup complet

```bash
#!/bin/bash
# backup-application.sh - Sauvegarde complète de l'application

APP_NAME="myapp"
APP_DIR="/opt/$APP_NAME"
BACKUP_BASE="/backups"
BACKUP_DIR="$BACKUP_BASE/$APP_NAME"
DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="$BACKUP_DIR/${APP_NAME}_full_${DATE}.tar.gz"
RETENTION_DAYS=30

# Créer le répertoire
mkdir -p "$BACKUP_DIR"

echo "[$(date)] Début du backup complet de $APP_NAME"

# Créer l'archive
tar -czf "$BACKUP_FILE" \
    --exclude='*.log' \
    --exclude='tmp/*' \
    --exclude='cache/*' \
    -C "$(dirname $APP_DIR)" \
    "$(basename $APP_DIR)"

if [ $? -eq 0 ]; then
    SIZE=$(du -h "$BACKUP_FILE" | cut -f1)
    echo "[$(date)] ✓ Backup créé: $BACKUP_FILE ($SIZE)"

    # Checksum MD5
    MD5=$(md5sum "$BACKUP_FILE" | cut -d' ' -f1)
    echo "$MD5  $BACKUP_FILE" > "${BACKUP_FILE}.md5"
    echo "[$(date)] ✓ Checksum MD5: $MD5"

    # Nettoyage des anciens backups
    find "$BACKUP_DIR" -name "${APP_NAME}_full_*.tar.gz" -mtime +$RETENTION_DAYS -delete
    find "$BACKUP_DIR" -name "${APP_NAME}_full_*.md5" -mtime +$RETENTION_DAYS -delete

    # Upload vers le cloud (exemple avec AWS S3)
    if command -v aws &> /dev/null; then
        echo "[$(date)] Upload vers S3..."
        aws s3 cp "$BACKUP_FILE" "s3://my-backups/$APP_NAME/" --storage-class GLACIER
        aws s3 cp "${BACKUP_FILE}.md5" "s3://my-backups/$APP_NAME/"
        echo "[$(date)] ✓ Upload S3 terminé"
    fi

    echo "[$(date)] ✓ Backup terminé avec succès"
else
    echo "[$(date)] ✗ Échec du backup"
    exit 1
fi
```

### Script de restauration

```bash
#!/bin/bash
# restore-application.sh

APP_NAME="myapp"
APP_DIR="/opt/$APP_NAME"
BACKUP_FILE="$1"

if [ -z "$BACKUP_FILE" ]; then
    echo "Usage: $0 <backup_file.tar.gz>"
    exit 1
fi

if [ ! -f "$BACKUP_FILE" ]; then
    echo "✗ Fichier introuvable: $BACKUP_FILE"
    exit 1
fi

# Vérifier le checksum
if [ -f "${BACKUP_FILE}.md5" ]; then
    echo "Vérification du checksum..."
    md5sum -c "${BACKUP_FILE}.md5"

    if [ $? -ne 0 ]; then
        echo "✗ Checksum invalide! Fichier corrompu?"
        exit 1
    fi
    echo "✓ Checksum valide"
fi

echo "⚠️  ATTENTION: Cette opération va écraser $APP_DIR"
read -p "Continuer? (oui/non): " CONFIRM

if [ "$CONFIRM" != "oui" ]; then
    echo "Annulé"
    exit 0
fi

# Arrêter l'application
echo "Arrêt de l'application..."
systemctl stop $APP_NAME

# Sauvegarder l'existant
if [ -d "$APP_DIR" ]; then
    echo "Sauvegarde de l'installation actuelle..."
    mv "$APP_DIR" "${APP_DIR}.backup_$(date +%Y%m%d_%H%M%S)"
fi

# Extraire le backup
echo "Extraction du backup..."
tar -xzf "$BACKUP_FILE" -C "$(dirname $APP_DIR)"

if [ $? -eq 0 ]; then
    echo "✓ Extraction réussie"

    # Redémarrer l'application
    echo "Démarrage de l'application..."
    systemctl start $APP_NAME

    # Vérifier le statut
    sleep 2
    if systemctl is-active --quiet $APP_NAME; then
        echo "✓ Application démarrée avec succès"
    else
        echo "✗ Échec du démarrage"
        exit 1
    fi
else
    echo "✗ Échec de l'extraction"
    exit 1
fi
```

## Tests de restauration

### Pourquoi tester les restaurations ?

**Un backup non testé n'est pas un backup !**

Beaucoup d'entreprises découvrent que leurs backups sont inutilisables au moment critique. Il est essentiel de tester régulièrement.

### Script de test automatisé

```bash
#!/bin/bash
# test-backup-restore.sh - Test automatique de backup/restauration

APP_NAME="myapp"
TEST_DIR="/tmp/backup_test_$$"
BACKUP_FILE="$1"
LOG_FILE="/var/log/backup_test.log"

log() {
    echo "[$(date)] $1" | tee -a "$LOG_FILE"
}

cleanup() {
    rm -rf "$TEST_DIR"
}

trap cleanup EXIT

if [ -z "$BACKUP_FILE" ]; then
    log "✗ Usage: $0 <backup_file>"
    exit 1
fi

log "=== Début du test de restauration ==="
log "Backup: $BACKUP_FILE"

# Créer l'environnement de test
mkdir -p "$TEST_DIR"

# Test 1: Vérifier l'intégrité de l'archive
log "Test 1: Intégrité de l'archive"
if tar -tzf "$BACKUP_FILE" > /dev/null 2>&1; then
    log "✓ Archive intègre"
else
    log "✗ Archive corrompue"
    exit 1
fi

# Test 2: Extraire dans l'environnement de test
log "Test 2: Extraction"
tar -xzf "$BACKUP_FILE" -C "$TEST_DIR"
if [ $? -eq 0 ]; then
    log "✓ Extraction réussie"
else
    log "✗ Échec de l'extraction"
    exit 1
fi

# Test 3: Vérifier les fichiers critiques
log "Test 3: Fichiers critiques"
CRITICAL_FILES=(
    "myapp"
    "config.ini"
    "database.db"
)

for file in "${CRITICAL_FILES[@]}"; do
    if [ -e "$TEST_DIR/$APP_NAME/$file" ]; then
        log "✓ $file présent"
    else
        log "✗ $file manquant"
        exit 1
    fi
done

# Test 4: Tester la base de données
log "Test 4: Base de données"
if sqlite3 "$TEST_DIR/$APP_NAME/database.db" "PRAGMA integrity_check;" | grep -q "ok"; then
    log "✓ Base de données intègre"
else
    log "✗ Base de données corrompue"
    exit 1
fi

# Test 5: Tester l'exécutable
log "Test 5: Exécutable"
if [ -x "$TEST_DIR/$APP_NAME/myapp" ]; then
    log "✓ Exécutable valide"

    # Test de démarrage (si possible)
    # "$TEST_DIR/$APP_NAME/myapp" --test-config
else
    log "✗ Exécutable invalide"
    exit 1
fi

log "=== ✓ Tous les tests réussis ==="
log "Le backup $BACKUP_FILE est restaurable"

# Envoyer un rapport
mail -s "Test de backup réussi - $APP_NAME" admin@example.com < "$LOG_FILE"
```

**Automatiser les tests** :

```bash
# Tester tous les dimanches à 4h
0 4 * * 0 /usr/local/bin/test-backup-restore.sh /backups/myapp/latest_full.tar.gz
```

## Plan de reprise documenté

### Template de documentation

```markdown
# Plan de Reprise Après Sinistre (DRP)
## Application: MyFreePascalApp

**Version**: 1.0  
**Date**: 2025-10-08  
**Responsable**: Équipe DevOps  

---

## 1. Contacts d'urgence

| Rôle | Nom | Téléphone | Email |
|------|-----|-----------|-------|
| Responsable IT | Jean Dupont | +33 6 XX XX XX XX | jean@example.com |
| DBA | Marie Martin | +33 6 XX XX XX XX | marie@example.com |
| Développeur Senior | Pierre Durand | +33 6 XX XX XX XX | pierre@example.com |
| Support Hébergeur | OVH | +33 X XX XX XX XX | support@ovh.com |

## 2. Architecture système

```
┌─────────────────┐  
│   Load Balancer │  
│   (HAProxy)     │  
└────────┬────────┘  
         │  
    ┌────┴────┐  
    │         │  
┌───▼──┐   ┌──▼───┐  
│ App  │   │ App  │  
│  #1  │   │  #2  │  
└───┬──┘   └──┬───┘  
    │         │  
    └────┬────┘  
         │  
    ┌────▼─────┐  
    │PostgreSQL│  
    └────┬─────┘  
         │  
    ┌────▼────┐  
    │  Redis  │  
    └─────────┘
```

## 3. Emplacements des backups

| Type | Emplacement | Fréquence | Rétention |
|------|-------------|-----------|-----------|
| Base de données | /backups/postgres | Quotidien | 30 jours |
| Fichiers app | /backups/app | Quotidien | 30 jours |
| Configuration | /backups/config | Hebdo | 90 jours |
| Backup offsite | S3: s3://backups-dr/ | Quotidien | 90 jours |

## 4. Métriques

- **RTO**: 4 heures
- **RPO**: 1 heure

## 5. Procédures de reprise

### 5.1 Panne serveur application

**Symptômes**: Application inaccessible, erreur 502/503

**Procédure**:

1. **Vérifier le statut**
   ```bash
   systemctl status myapp
   journalctl -u myapp -n 50
   ```

2. **Redémarrer le service**
   ```bash
   systemctl restart myapp
   ```

3. **Si échec**: Déployer sur serveur de secours
   ```bash
   ./deploy-to-standby.sh
   ```

4. **Mettre à jour le DNS/Load Balancer**

**Durée estimée**: 15-30 minutes

### 5.2 Corruption de base de données

**Symptômes**: Erreurs SQL, données incohérentes

**Procédure**:

1. **Arrêter l'application**
   ```bash
   systemctl stop myapp
   ```

2. **Identifier le dernier backup valide**
   ```bash
   ls -lh /backups/postgres/
   ```

3. **Restaurer la base de données**
   ```bash
   ./restore-postgres.sh /backups/postgres/myappdb_20251008_020000.sql.gz
   ```

4. **Vérifier l'intégrité**
   ```bash
   psql -U postgres -d myappdb -c "SELECT COUNT(*) FROM users;"
   ```

5. **Redémarrer l'application**
   ```bash
   systemctl start myapp
   ```

6. **Tester fonctionnellement**
   - Connexion utilisateur
   - Opération critique (création commande)
   - Vérifier les logs

**Durée estimée**: 1-2 heures  
**Perte de données**: Jusqu'à 1 heure (dernier backup)  

### 5.3 Sinistre complet du datacenter

**Symptômes**: Tout est inaccessible

**Procédure**:

1. **Activer le site de secours**
   ```bash
   ssh backup-server.example.com
   cd /opt/disaster-recovery
   ./activate-standby-site.sh
   ```

2. **Restaurer depuis les backups cloud**
   ```bash
   # Télécharger depuis S3
   aws s3 sync s3://backups-dr/latest/ /restore/

   # Restaurer la base de données
   ./restore-postgres.sh /restore/database/latest.sql.gz

   # Restaurer l'application
   ./restore-application.sh /restore/app/latest.tar.gz
   ```

3. **Mettre à jour le DNS**
   ```bash
   # Pointer vers le nouveau serveur
   # IP: 203.0.113.100 (serveur de secours)
   ```

4. **Notifications**
   - Équipe technique
   - Clients (via status page)
   - Management

**Durée estimée**: 4-6 heures  
**Perte de données**: 1-2 heures  

### 5.4 Attaque ransomware

**Symptômes**: Fichiers chiffrés, demande de rançon

**Procédure**:

1. **ISOLER IMMÉDIATEMENT**
   ```bash
   # Déconnecter du réseau
   ip link set eth0 down

   # Arrêter tous les services
   systemctl stop myapp
   systemctl stop postgresql
   ```

2. **NE PAS PAYER LA RANÇON**

3. **Documenter l'incident**
   - Screenshots de la demande
   - Liste des fichiers affectés
   - Chronologie

4. **Contacter les autorités**
   - Police / Gendarmerie
   - ANSSI (FR): www.ssi.gouv.fr

5. **Reconstruire depuis les backups**
   - Utiliser des backups antérieurs à l'infection
   - Scanner avec antivirus avant restauration

6. **Enquête forensique**
   - Comment l'attaque est-elle arrivée?
   - Autres systèmes compromis?

**Durée estimée**: 1-3 jours  
**Impact**: Potentiellement critique  

## 6. Post-incident

### Checklist post-restauration

- [ ] Application fonctionne normalement
- [ ] Base de données intègre
- [ ] Utilisateurs peuvent se connecter
- [ ] Transactions testées
- [ ] Logs vérifiés
- [ ] Monitoring actif
- [ ] Équipe informée
- [ ] Clients notifiés

### Rapport post-mortem

À compléter dans les 48h suivant l'incident:

1. **Chronologie des événements**
2. **Cause racine**
3. **Impact (utilisateurs, données, finances)**
4. **Actions correctives**
5. **Améliorations du plan**

---

**Dernière mise à jour**: 2025-10-08  
**Prochaine révision**: 2026-01-08  
```

## Monitoring et alertes

### Surveillance de la santé des backups

```pascal
unit BackupMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TBackupStatus = (bsOK, bsWarning, bsCritical);

  TBackupHealth = record
    Status: TBackupStatus;
    LastBackupTime: TDateTime;
    LastBackupSize: Int64;
    Message: string;
  end;

  TBackupMonitor = class
  private
    FBackupDir: string;
    FExpectedIntervalHours: Integer;
    FMinimumSizeBytes: Int64;

    function GetLatestBackupFile: string;
    function GetFileAge(const FileName: string): TDateTime;
    function GetFileSize(const FileName: string): Int64;
  public
    constructor Create(const BackupDir: string);

    function CheckHealth: TBackupHealth;
    procedure SendAlert(const Health: TBackupHealth);

    property ExpectedIntervalHours: Integer read FExpectedIntervalHours write FExpectedIntervalHours;
    property MinimumSizeBytes: Int64 read FMinimumSizeBytes write FMinimumSizeBytes;
  end;

implementation

uses
  Process;

constructor TBackupMonitor.Create(const BackupDir: string);
begin
  FBackupDir := BackupDir;
  FExpectedIntervalHours := 24; // 1 backup par jour attendu
  FMinimumSizeBytes := 1024 * 1024; // 1 MB minimum
end;

function TBackupMonitor.GetLatestBackupFile: string;
var
  SearchRec: TSearchRec;
  LatestFile: string;
  LatestTime: TDateTime;
  CurrentTime: TDateTime;
begin
  Result := '';
  LatestTime := 0;

  if FindFirst(IncludeTrailingPathDelimiter(FBackupDir) + '*.sql.gz',
               faAnyFile, SearchRec) = 0 then
  begin
    repeat
      CurrentTime := FileDateToDateTime(SearchRec.Time);
      if CurrentTime > LatestTime then
      begin
        LatestTime := CurrentTime;
        LatestFile := SearchRec.Name;
      end;
    until FindNext(SearchRec) <> 0;

    FindClose(SearchRec);
  end;

  if LatestFile <> '' then
    Result := IncludeTrailingPathDelimiter(FBackupDir) + LatestFile;
end;

function TBackupMonitor.GetFileAge(const FileName: string): TDateTime;
var
  Age: LongInt;
begin
  Age := FileAge(FileName);
  if Age <> -1 then
    Result := FileDateToDateTime(Age)
  else
    Result := 0;
end;

function TBackupMonitor.GetFileSize(const FileName: string): Int64;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  begin
    Result := SearchRec.Size;
    FindClose(SearchRec);
  end;
end;

function TBackupMonitor.CheckHealth: TBackupHealth;
var
  LatestBackup: string;
  HoursSinceBackup: Int64;
begin
  Result.Status := bsOK;
  Result.Message := 'Backup OK';

  LatestBackup := GetLatestBackupFile;

  if LatestBackup = '' then
  begin
    Result.Status := bsCritical;
    Result.Message := 'Aucun backup trouvé!';
    Result.LastBackupTime := 0;
    Result.LastBackupSize := 0;
    Exit;
  end;

  Result.LastBackupTime := GetFileAge(LatestBackup);
  Result.LastBackupSize := GetFileSize(LatestBackup);

  // Vérifier l'âge
  HoursSinceBackup := HoursBetween(Now, Result.LastBackupTime);

  if HoursSinceBackup > (FExpectedIntervalHours * 2) then
  begin
    Result.Status := bsCritical;
    Result.Message := Format('Dernier backup il y a %d heures (critique)', [HoursSinceBackup]);
  end
  else if HoursSinceBackup > FExpectedIntervalHours then
  begin
    Result.Status := bsWarning;
    Result.Message := Format('Dernier backup il y a %d heures (attention)', [HoursSinceBackup]);
  end;

  // Vérifier la taille
  if Result.LastBackupSize < FMinimumSizeBytes then
  begin
    Result.Status := bsCritical;
    Result.Message := Format('Taille du backup suspecte: %d bytes', [Result.LastBackupSize]);
  end;
end;

procedure TBackupMonitor.SendAlert(const Health: TBackupHealth);
var
  AProcess: TProcess;
  Subject, Body, StatusStr: string;
begin
  if Health.Status = bsOK then
    Exit;

  case Health.Status of
    bsWarning: StatusStr := 'WARNING';
    bsCritical: StatusStr := 'CRITICAL';
  else
    StatusStr := 'UNKNOWN';
  end;
  Subject := 'Alerte Backup - ' + StatusStr;

  Body := Format(
    'Status: %s' + LineEnding +
    'Message: %s' + LineEnding +
    'Dernier backup: %s' + LineEnding +
    'Taille: %d bytes',
    [Subject, Health.Message,
     FormatDateTime('yyyy-mm-dd hh:nn:ss', Health.LastBackupTime),
     Health.LastBackupSize]
  );

  // Envoyer par email (exemple simple)
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'mail';
    AProcess.Parameters.Add('-s');
    AProcess.Parameters.Add(Subject);
    AProcess.Parameters.Add('admin@example.com');
    AProcess.Options := [poWaitOnExit, poUsePipes];

    AProcess.Execute;
    AProcess.Input.Write(Body[1], Length(Body));
    AProcess.CloseInput;
  finally
    AProcess.Free;
  end;
end;

end.
```

### Script de monitoring automatisé

```bash
#!/bin/bash
# monitor-backups.sh - Surveillance des backups

BACKUP_DIR="/backups"
MAX_AGE_HOURS=25  # Alerte si pas de backup depuis 25h
MIN_SIZE_MB=10    # Taille minimum attendue
EMAIL="admin@example.com"
SLACK_WEBHOOK="https://hooks.slack.com/services/YOUR/WEBHOOK/URL"

check_backup() {
    local backup_type=$1
    local backup_pattern=$2
    local max_age=$3

    echo "Vérification: $backup_type"

    # Trouver le dernier backup
    latest=$(find "$BACKUP_DIR" -name "$backup_pattern" -type f -printf '%T@ %p\n' | sort -rn | head -1)

    if [ -z "$latest" ]; then
        alert "CRITICAL" "$backup_type" "Aucun backup trouvé!"
        return 1
    fi

    latest_file=$(echo "$latest" | cut -d' ' -f2)
    latest_time=$(echo "$latest" | cut -d' ' -f1)
    current_time=$(date +%s)
    age_hours=$(( (current_time - ${latest_time%.*}) / 3600 ))

    # Vérifier l'âge
    if [ $age_hours -gt $max_age ]; then
        alert "WARNING" "$backup_type" "Dernier backup il y a ${age_hours}h"
        return 1
    fi

    # Vérifier la taille
    size_mb=$(du -m "$latest_file" | cut -f1)
    if [ $size_mb -lt $MIN_SIZE_MB ]; then
        alert "CRITICAL" "$backup_type" "Taille suspecte: ${size_mb}MB"
        return 1
    fi

    echo "✓ $backup_type OK (${age_hours}h, ${size_mb}MB)"
    return 0
}

alert() {
    local severity=$1
    local backup_type=$2
    local message=$3

    # Email
    echo "$message" | mail -s "[$severity] Backup Alert: $backup_type" "$EMAIL"

    # Slack
    if [ -n "$SLACK_WEBHOOK" ]; then
        curl -X POST "$SLACK_WEBHOOK" \
            -H 'Content-Type: application/json' \
            -d "{\"text\":\"🚨 [$severity] Backup Alert: $backup_type\n$message\"}"
    fi

    # Syslog
    logger -t backup-monitor -p user.err "[$severity] $backup_type: $message"
}

# Vérifier différents types de backups
check_backup "PostgreSQL" "myappdb_*.sql.gz" $MAX_AGE_HOURS
check_backup "Application" "myapp_full_*.tar.gz" $MAX_AGE_HOURS
check_backup "Configuration" "config_*.tar.gz" 168  # 1 semaine

echo "Monitoring terminé"
```

**Automatiser avec cron** :

```bash
# Vérifier toutes les 6 heures
0 */6 * * * /usr/local/bin/monitor-backups.sh >> /var/log/backup-monitor.log 2>&1
```

## Haute disponibilité (HA)

### Configuration Master-Slave PostgreSQL

#### Sur le serveur Master (principal)

**1. Configurer PostgreSQL pour la réplication** :

```bash
# Éditer postgresql.conf
sudo nano /etc/postgresql/15/main/postgresql.conf
```

```ini
# postgresql.conf
listen_addresses = '*'
wal_level = replica
max_wal_senders = 3
wal_keep_size = 64
hot_standby = on
```

**2. Autoriser la connexion du slave** :

```bash
# Éditer pg_hba.conf
sudo nano /etc/postgresql/15/main/pg_hba.conf
```

```
# pg_hba.conf
# TYPE  DATABASE        USER            ADDRESS                 METHOD
host    replication     replicator      192.168.1.101/32        md5
```

**3. Créer l'utilisateur de réplication** :

```sql
CREATE USER replicator WITH REPLICATION ENCRYPTED PASSWORD 'secret_password';
```

**4. Redémarrer PostgreSQL** :

```bash
sudo systemctl restart postgresql
```

#### Sur le serveur Slave (réplique)

**1. Arrêter PostgreSQL** :

```bash
sudo systemctl stop postgresql
```

**2. Supprimer les données existantes** :

```bash
sudo rm -rf /var/lib/postgresql/15/main/*
```

**3. Copier les données depuis le master** :

```bash
sudo -u postgres pg_basebackup \
    -h 192.168.1.100 \
    -D /var/lib/postgresql/15/main \
    -U replicator \
    -P \
    -v \
    -R \
    -X stream \
    -C -S replica_1
```

**4. Démarrer PostgreSQL** :

```bash
sudo systemctl start postgresql
```

**5. Vérifier le statut** :

Sur le **Master** :
```sql
SELECT * FROM pg_stat_replication;
```

Sur le **Slave** :
```sql
SELECT * FROM pg_stat_wal_receiver;
```

### Script de basculement automatique (Failover)

```bash
#!/bin/bash
# failover.sh - Bascule du slave en master

SLAVE_HOST="192.168.1.101"
MASTER_HOST="192.168.1.100"
APP_SERVERS=("app1.example.com" "app2.example.com")

echo "=== Procédure de failover ==="
echo "Master actuel: $MASTER_HOST"
echo "Nouveau master: $SLAVE_HOST"
echo

read -p "Continuer? (oui/non): " CONFIRM
if [ "$CONFIRM" != "oui" ]; then
    echo "Annulé"
    exit 0
fi

# 1. Promouvoir le slave en master
echo "1. Promotion du slave en master..."
ssh postgres@$SLAVE_HOST "pg_ctl promote -D /var/lib/postgresql/15/main"

if [ $? -eq 0 ]; then
    echo "✓ Slave promu en master"
else
    echo "✗ Échec de la promotion"
    exit 1
fi

# 2. Attendre que le nouveau master soit prêt
echo "2. Attente de la disponibilité du nouveau master..."
sleep 5

for i in {1..30}; do
    if psql -h $SLAVE_HOST -U postgres -c "SELECT 1" > /dev/null 2>&1; then
        echo "✓ Nouveau master opérationnel"
        break
    fi
    sleep 1
done

# 3. Reconfigurer les serveurs applicatifs
echo "3. Reconfiguration des serveurs applicatifs..."
for server in "${APP_SERVERS[@]}"; do
    echo "   Reconfiguration de $server..."
    ssh root@$server "sed -i 's/$MASTER_HOST/$SLAVE_HOST/g' /opt/myapp/config.ini"
    ssh root@$server "systemctl restart myapp"

    if [ $? -eq 0 ]; then
        echo "   ✓ $server reconfiguré"
    else
        echo "   ✗ Échec pour $server"
    fi
done

# 4. Mettre à jour le DNS (si applicable)
echo "4. Mise à jour DNS..."
# Ajouter votre logique de mise à jour DNS ici

echo "=== Failover terminé ==="
echo "Nouveau master: $SLAVE_HOST"
echo "Ancien master: $MASTER_HOST (à reconfigurer en slave)"
```

### Supervision avec un script de healthcheck

```bash
#!/bin/bash
# db-healthcheck.sh - Vérification de santé de la base de données

DB_HOST="localhost"
DB_USER="postgres"
DB_NAME="myappdb"
TIMEOUT=5

# Test de connexion
if timeout $TIMEOUT psql -h $DB_HOST -U $DB_USER -d $DB_NAME -c "SELECT 1" > /dev/null 2>&1; then
    echo "✓ Base de données accessible"
    exit 0
else
    echo "✗ Base de données inaccessible"

    # Déclencher le failover automatique
    /usr/local/bin/failover.sh

    exit 1
fi
```

**Configurer avec cron pour vérifier toutes les minutes** :

```bash
* * * * * /usr/local/bin/db-healthcheck.sh >> /var/log/db-healthcheck.log 2>&1
```

## Outils de disaster recovery

### Bacula - Solution de backup entreprise

**Installation sur Ubuntu** :

```bash
# Installer Bacula
sudo apt update
sudo apt install bacula-server bacula-client bacula-console

# Configurer
sudo nano /etc/bacula/bacula-dir.conf
```

**Configuration de base** :

```
# bacula-dir.conf
Director {
  Name = myapp-dir
  DIRport = 9101
  QueryFile = "/etc/bacula/query.sql"
  WorkingDirectory = "/var/lib/bacula"
  PidDirectory = "/var/run/bacula"
  Maximum Concurrent Jobs = 20
  Password = "director_password"
  Messages = Daemon
}

Job {
  Name = "BackupMyApp"
  Type = Backup
  Level = Incremental
  Client = myapp-client
  FileSet = "MyApp Files"
  Schedule = "WeeklyCycle"
  Storage = File
  Messages = Standard
  Pool = Default
  Priority = 10
  Write Bootstrap = "/var/lib/bacula/%c.bsr"
}

FileSet {
  Name = "MyApp Files"
  Include {
    Options {
      signature = MD5
      compression = GZIP
    }
    File = /opt/myapp
    File = /var/lib/postgresql/15/main
  }
  Exclude {
    File = /opt/myapp/tmp
    File = /opt/myapp/cache
  }
}

Schedule {
  Name = "WeeklyCycle"
  Run = Full 1st sun at 23:05
  Run = Differential 2nd-5th sun at 23:05
  Run = Incremental mon-sat at 23:05
}
```

### Duplicity - Backup chiffré

**Installation** :

```bash
sudo apt install duplicity python3-boto3
```

**Script de backup vers AWS S3** :

```bash
#!/bin/bash
# duplicity-backup.sh

export AWS_ACCESS_KEY_ID="your_access_key"
export AWS_SECRET_ACCESS_KEY="your_secret_key"
export PASSPHRASE="encryption_passphrase"

SOURCE="/opt/myapp"
DEST="s3://my-backups/myapp/"

# Backup incrémental
duplicity \
    --full-if-older-than 7D \
    --exclude /opt/myapp/tmp \
    --exclude /opt/myapp/logs \
    $SOURCE $DEST

# Nettoyer les anciens backups
duplicity remove-older-than 30D --force $DEST

# Vérifier
duplicity collection-status $DEST

unset PASSPHRASE
```

**Restauration** :

```bash
#!/bin/bash
# duplicity-restore.sh

export AWS_ACCESS_KEY_ID="your_access_key"
export AWS_SECRET_ACCESS_KEY="your_secret_key"
export PASSPHRASE="encryption_passphrase"

SOURCE="s3://my-backups/myapp/"
DEST="/restore/myapp"

# Restaurer la version la plus récente
duplicity restore $SOURCE $DEST

# Ou restaurer à une date spécifique
# duplicity restore --time 2025-10-01 $SOURCE $DEST

unset PASSPHRASE
```

### Rsync pour la synchronisation

**Script de synchronisation continue** :

```bash
#!/bin/bash
# rsync-mirror.sh - Miroir en temps quasi-réel

SOURCE="/opt/myapp/"
DEST="backup-server:/backups/myapp/"
LOG="/var/log/rsync-mirror.log"

# Synchronisation continue
while true; do
    rsync -avz \
        --delete \
        --exclude 'tmp/' \
        --exclude 'cache/' \
        --exclude '*.log' \
        --log-file="$LOG" \
        "$SOURCE" "$DEST"

    if [ $? -eq 0 ]; then
        echo "[$(date)] Synchronisation réussie" >> "$LOG"
    else
        echo "[$(date)] Échec de synchronisation" >> "$LOG"
    fi

    # Attendre 5 minutes
    sleep 300
done
```

## Stratégies cloud

### Backup sur AWS S3

```bash
#!/bin/bash
# backup-to-s3.sh

BACKUP_FILE="/backups/myapp/myapp_$(date +%Y%m%d).tar.gz"
S3_BUCKET="s3://my-disaster-recovery-bucket"
S3_REGION="eu-west-1"

# Créer le backup
tar -czf "$BACKUP_FILE" /opt/myapp

# Upload vers S3 avec storage class Glacier pour économiser
aws s3 cp "$BACKUP_FILE" "$S3_BUCKET/" \
    --storage-class GLACIER \
    --region "$S3_REGION"

if [ $? -eq 0 ]; then
    echo "✓ Backup uploadé vers S3"

    # Supprimer le fichier local pour économiser l'espace
    rm "$BACKUP_FILE"
else
    echo "✗ Échec de l'upload S3"
    exit 1
fi

# Configurer le lifecycle pour suppression automatique après 90 jours
aws s3api put-bucket-lifecycle-configuration \
    --bucket my-disaster-recovery-bucket \
    --lifecycle-configuration file://lifecycle.json
```

**lifecycle.json** :

```json
{
  "Rules": [
    {
      "Id": "DeleteOldBackups",
      "Status": "Enabled",
      "Prefix": "",
      "Expiration": {
        "Days": 90
      }
    }
  ]
}
```

### Backup cross-region

```bash
#!/bin/bash
# backup-cross-region.sh

PRIMARY_REGION="eu-west-1"
SECONDARY_REGION="us-east-1"
BUCKET_NAME="my-backups"

# Upload vers la région primaire
aws s3 cp backup.tar.gz s3://$BUCKET_NAME/ --region $PRIMARY_REGION

# Copier vers la région secondaire
aws s3 cp s3://$BUCKET_NAME/backup.tar.gz \
    s3://$BUCKET_NAME-$SECONDARY_REGION/ \
    --source-region $PRIMARY_REGION \
    --region $SECONDARY_REGION

echo "✓ Backup répliqué dans 2 régions"
```

## Checklist finale de disaster recovery

### Préparation

- [ ] **RTO/RPO définis** et documentés
- [ ] **Plan de reprise documenté** et accessible hors ligne
- [ ] **Contacts d'urgence** à jour
- [ ] **Sauvegardes automatisées** configurées
- [ ] **Sauvegardes offsite** actives
- [ ] **Tests de restauration** programmés (au moins trimestriels)
- [ ] **Monitoring des backups** en place
- [ ] **Alertes configurées** (email, SMS, Slack)

### Infrastructure

- [ ] **Serveur de secours** disponible
- [ ] **Configuration en haute disponibilité** (si applicable)
- [ ] **Réplication base de données** configurée
- [ ] **Load balancer** avec healthchecks
- [ ] **DNS avec TTL court** pour basculement rapide
- [ ] **Accès réseau sécurisé** au site de secours

### Documentation

- [ ] **Procédures de restauration** testées et à jour
- [ ] **Diagrammes d'architecture** à jour
- [ ] **Inventaire des systèmes** complet
- [ ] **Credentials** sauvegardés de manière sécurisée
- [ ] **Runbook** accessible 24/7
- [ ] **Contacts fournisseurs** (hébergeur, cloud, etc.)

### Formation

- [ ] **Équipe formée** aux procédures
- [ ] **Simulations de sinistres** régulières (annuelles minimum)
- [ ] **Rôles et responsabilités** clairs
- [ ] **Communication de crise** préparée

### Compliance

- [ ] **Conformité RGPD** pour les backups
- [ ] **Chiffrement des backups** sensibles
- [ ] **Logs d'accès** aux backups
- [ ] **Audit trail** des restaurations
- [ ] **Politique de rétention** respectée

## Conclusion

Le disaster recovery n'est pas une option, c'est une nécessité pour toute application professionnelle. Les points clés à retenir :

🎯 **Prévention** : Le meilleur sinistre est celui qui n'arrive pas
- Monitoring proactif
- Haute disponibilité
- Tests réguliers

💾 **Préparation** : Avoir un plan solide
- Backups automatisés et testés
- Documentation claire
- Équipe formée

⚡ **Réaction** : Agir vite et bien
- Procédures documentées
- Communication efficace
- Post-mortem systématique

🔄 **Amélioration continue** : Apprendre de chaque incident
- Mettre à jour les procédures
- Améliorer les outils
- Former l'équipe

**N'oubliez jamais** : Un backup non testé n'est pas un backup !

---

**Ressources complémentaires** :
- NIST Guidelines for Disaster Recovery : https://www.nist.gov/
- ISO 22301 Business Continuity Management
- DORA (Digital Operational Resilience Act) pour l'UE
- Bacula Documentation : https://www.bacula.org/documentation/
- AWS Disaster Recovery : https://aws.amazon.com/disaster-recovery/

⏭️ [Développement de Jeux](/23-developpement-jeux/README.md)
