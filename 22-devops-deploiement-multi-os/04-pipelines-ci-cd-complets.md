🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.4 Pipelines CI/CD complets

## Introduction au CI/CD

Vous avez développé une excellente application FreePascal/Lazarus. Vous savez la compiler, la tester, et même la déployer. Mais imaginez devoir faire tout cela manuellement à chaque modification du code :

**Processus manuel (sans CI/CD) :**
```
1. Modifier le code
2. Compiler sur Windows
3. Compiler sur Linux
4. Exécuter les tests manuellement
5. Créer l'installateur Windows
6. Créer les paquets Linux
7. Se connecter au serveur
8. Déployer l'application
9. Vérifier que tout fonctionne
```

**Temps nécessaire :** 30-60 minutes  
**Risque d'erreur :** Élevé  
**Ennui :** Maximum 😴  

**Avec un pipeline CI/CD :**
```
1. Commit + Push vers Git
2. ☕ Prendre un café
3. Tout est fait automatiquement !
```

**Temps nécessaire :** 5-15 minutes (automatique)  
**Risque d'erreur :** Minimal  
**Satisfaction :** Maximum 😊  

### Qu'est-ce que le CI/CD ?

**CI/CD** signifie **Continuous Integration / Continuous Deployment** (Intégration Continue / Déploiement Continu).

**Continuous Integration (CI) :**
- Intégrer le code fréquemment (plusieurs fois par jour)
- Compiler automatiquement à chaque commit
- Exécuter les tests automatiquement
- Détecter les bugs rapidement

**Continuous Deployment (CD) :**
- Déployer automatiquement en production
- Livrer rapidement les nouvelles fonctionnalités
- Rollback automatique si problème
- Déploiement sans intervention humaine

**Analogie simple :**
- **Sans CI/CD** = Écrire un livre, l'imprimer soi-même, le livrer en mains propres
- **Avec CI/CD** = Écrire un livre, un système automatique l'imprime, le livre et notifie les lecteurs

### Pourquoi le CI/CD est crucial pour FreePascal/Lazarus ?

**1. Multi-plateforme simplifié**
```
Un seul commit → Builds Windows + Linux automatiques
```

**2. Qualité garantie**
```
Tests sur toutes les plateformes avant déploiement  
Bug détecté ? → Build échoue → Pas de déploiement
```

**3. Gain de temps massif**
```
Manuelle : 1h par release  
Automatique : 10 minutes sans intervention
```

**4. Déploiements fréquents**
```
Sans CI/CD : 1 release par mois (risqué)  
Avec CI/CD : Plusieurs releases par jour (sûr)
```

**5. Traçabilité complète**
```
Qui a déployé quoi, quand, pourquoi  
Rollback en un clic
```

## Architecture d'un pipeline CI/CD

Visualisons un pipeline complet pour FreePascal/Lazarus :

```
┌─────────────────────────────────────────────────────────────┐
│                   DÉVELOPPEUR                               │
│  Écrit du code FreePascal → Commit → Push vers Git         │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│                DÉCLENCHEMENT (Trigger)                      │
│  • Push sur main/develop                                    │
│  • Pull Request créée                                       │
│  • Tag créé (v1.2.3)                                        │
└────────────────────┬────────────────────────────────────────┘
                     │
      ┌──────────────┴──────────────┐
      │                             │
      ▼                             ▼
┌──────────────┐            ┌──────────────┐
│ Build Windows│            │ Build Linux  │
│  • Compiler  │            │  • Compiler  │
│  • Tests     │            │  • Tests     │
└──────┬───────┘            └──────┬───────┘
       │                           │
       └──────────┬────────────────┘
                  ▼
┌─────────────────────────────────────────────────────────────┐
│              ÉTAPE D'ANALYSE                                │
│  • Linter (qualité du code)                                 │
│  • Analyse de sécurité                                      │
│  • Couverture de tests                                      │
└────────────────────┬────────────────────────────────────────┘
                     ▼
┌─────────────────────────────────────────────────────────────┐
│              PACKAGING                                      │
│  • Installateur Windows (Inno Setup)                        │
│  • Paquets Linux (DEB, RPM, AppImage)                       │
│  • Signature numérique                                      │
└────────────────────┬────────────────────────────────────────┘
                     ▼
┌─────────────────────────────────────────────────────────────┐
│              DÉPLOIEMENT                                    │
│  • Staging (tests finaux)                                   │
│  • Production (si staging OK)                               │
│  • Notification (Slack, Email)                              │
└─────────────────────────────────────────────────────────────┘
```

### Composants d'un pipeline

**1. Serveur CI/CD**
Exécute le pipeline. Options :
- **GitHub Actions** (gratuit pour open source)
- **GitLab CI** (intégré à GitLab)
- **Jenkins** (auto-hébergé, open source)
- **CircleCI, Travis CI, Azure DevOps...**

**2. Runners / Agents**
Machines qui exécutent les tâches :
- Fournis par la plateforme (GitHub Actions)
- Auto-hébergés (vos propres serveurs)

**3. Configuration**
Fichier décrivant le pipeline :
- `.github/workflows/*.yml` (GitHub Actions)
- `.gitlab-ci.yml` (GitLab CI)
- `Jenkinsfile` (Jenkins)

**4. Artefacts**
Résultats du build :
- Exécutables compilés
- Installateurs
- Logs de tests
- Rapports de couverture

## GitHub Actions : Pipeline complet

GitHub Actions est gratuit pour les projets open source et facile à utiliser.

### Structure de base

**Fichier : `.github/workflows/ci-cd.yml`**
```yaml
name: CI/CD Pipeline

# Déclencheurs
on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]
  release:
    types: [ created ]

# Variables d'environnement globales
env:
  FPC_VERSION: "3.2.2"
  LAZARUS_VERSION: "2.2.6"

# Jobs (tâches)
jobs:
  # Job 1 : Build et test
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Build application
        run: make build

  # Job 2 : Déploiement
  deploy:
    needs: build  # Attend que build soit terminé
    runs-on: ubuntu-latest
    steps:
      - name: Deploy to production
        run: make deploy
```

### Pipeline complet FreePascal/Lazarus

**Fichier : `.github/workflows/freepascal-cicd.yml`**
```yaml
name: FreePascal CI/CD Pipeline

on:
  push:
    branches: [ main, develop ]
    tags:
      - 'v*'
  pull_request:
    branches: [ main ]

env:
  APP_NAME: MonAppli
  FPC_VERSION: 3.2.2

jobs:
  # ============================================
  # JOB 1 : Lint et Qualité du code
  # ============================================
  lint:
    name: Code Quality Check
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Install FreePascal
        run: |
          sudo apt-get update
          sudo apt-get install -y fpc

      - name: Check syntax
        run: |
          fpc -vn src/*.pas || exit 0

      - name: Check for TODOs
        run: |
          echo "Vérification des TODOs..."
          grep -rn "TODO" src/ || echo "Aucun TODO trouvé"

  # ============================================
  # JOB 2 : Build Windows
  # ============================================
  build-windows:
    name: Build Windows
    runs-on: windows-latest
    needs: lint
    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Setup FreePascal (Windows)
        run: |
          choco install lazarus -y

      - name: Compile Application
        shell: cmd
        run: |
          "C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe" -O3 -XX -CX src\main.pas -o${{ env.APP_NAME }}.exe

      - name: Run Tests
        shell: cmd
        run: |
          "C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe" tests\test_main.pas -otest_runner.exe
          test_runner.exe

      - name: Create Installer (Inno Setup)
        shell: powershell
        run: |
          choco install innosetup -y
          iscc installer\windows\setup.iss

      - name: Upload Windows Artifacts
        uses: actions/upload-artifact@v3
        with:
          name: windows-build
          path: |
            ${{ env.APP_NAME }}.exe
            Output/${{ env.APP_NAME }}-Setup.exe

  # ============================================
  # JOB 3 : Build Linux
  # ============================================
  build-linux:
    name: Build Linux
    runs-on: ubuntu-latest
    needs: lint
    strategy:
      matrix:
        include:
          - distro: ubuntu-22.04
            format: deb
          - distro: ubuntu-20.04
            format: deb
    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Install FreePascal
        run: |
          sudo apt-get update
          sudo apt-get install -y fpc fp-compiler fp-utils

      - name: Compile Application
        run: |
          fpc -O3 -XX -CX src/main.pas -o${{ env.APP_NAME }}
          chmod +x ${{ env.APP_NAME }}

      - name: Run Tests
        run: |
          fpc tests/test_main.pas -otest_runner
          ./test_runner

      - name: Create DEB Package
        run: |
          chmod +x scripts/build-deb.sh
          ./scripts/build-deb.sh

      - name: Create AppImage
        run: |
          chmod +x scripts/build-appimage.sh
          ./scripts/build-appimage.sh

      - name: Upload Linux Artifacts
        uses: actions/upload-artifact@v3
        with:
          name: linux-build-${{ matrix.distro }}
          path: |
            ${{ env.APP_NAME }}
            *.deb
            *.AppImage

  # ============================================
  # JOB 4 : Tests d'intégration
  # ============================================
  integration-tests:
    name: Integration Tests
    runs-on: ubuntu-latest
    needs: [build-windows, build-linux]
    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_PASSWORD: testpass
          POSTGRES_DB: testdb
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
        ports:
          - 5432:5432
    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Download Linux Build
        uses: actions/download-artifact@v3
        with:
          name: linux-build-ubuntu-22.04

      - name: Run Integration Tests
        env:
          DB_HOST: localhost
          DB_PORT: 5432
          DB_NAME: testdb
          DB_USER: postgres
          DB_PASSWORD: testpass
        run: |
          chmod +x ${{ env.APP_NAME }}
          fpc tests/test_integration.pas -otest_integration
          ./test_integration

  # ============================================
  # JOB 5 : Analyse de sécurité
  # ============================================
  security:
    name: Security Scan
    runs-on: ubuntu-latest
    needs: [build-windows, build-linux]
    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Run Trivy vulnerability scanner
        uses: aquasecurity/trivy-action@master
        with:
          scan-type: 'fs'
          scan-ref: '.'
          format: 'sarif'
          output: 'trivy-results.sarif'

      - name: Upload Trivy results to GitHub Security
        uses: github/codeql-action/upload-sarif@v2
        with:
          sarif_file: 'trivy-results.sarif'

  # ============================================
  # JOB 6 : Déploiement Staging
  # ============================================
  deploy-staging:
    name: Deploy to Staging
    runs-on: ubuntu-latest
    needs: [integration-tests, security]
    if: github.ref == 'refs/heads/develop'
    environment:
      name: staging
      url: https://staging.monappli.com
    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Download Linux Build
        uses: actions/download-artifact@v3
        with:
          name: linux-build-ubuntu-22.04

      - name: Deploy to Staging Server
        uses: appleboy/ssh-action@master
        with:
          host: ${{ secrets.STAGING_HOST }}
          username: ${{ secrets.STAGING_USER }}
          key: ${{ secrets.SSH_PRIVATE_KEY }}
          script: |
            cd /opt/monappli
            systemctl stop monappli
            cp ~/monappli-new ~/monappli-backup
            systemctl start monappli

      - name: Health Check
        run: |
          sleep 10
          curl --fail https://staging.monappli.com/health || exit 1

      - name: Notify Slack
        uses: 8398a7/action-slack@v3
        with:
          status: ${{ job.status }}
          text: 'Déploiement staging: ${{ job.status }}'
          webhook_url: ${{ secrets.SLACK_WEBHOOK }}

  # ============================================
  # JOB 7 : Déploiement Production
  # ============================================
  deploy-production:
    name: Deploy to Production
    runs-on: ubuntu-latest
    needs: [integration-tests, security]
    if: startsWith(github.ref, 'refs/tags/v')
    environment:
      name: production
      url: https://monappli.com
    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Download All Artifacts
        uses: actions/download-artifact@v3

      - name: Create Release
        id: create_release
        uses: actions/create-release@v1
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        with:
          tag_name: ${{ github.ref }}
          release_name: Release ${{ github.ref }}
          draft: false
          prerelease: false

      - name: Upload Windows Installer
        uses: actions/upload-release-asset@v1
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        with:
          upload_url: ${{ steps.create_release.outputs.upload_url }}
          asset_path: ./windows-build/Output/${{ env.APP_NAME }}-Setup.exe
          asset_name: ${{ env.APP_NAME }}-Setup-${{ github.ref_name }}.exe
          asset_content_type: application/octet-stream

      - name: Upload Linux DEB
        uses: actions/upload-release-asset@v1
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        with:
          upload_url: ${{ steps.create_release.outputs.upload_url }}
          asset_path: ./linux-build-ubuntu-22.04/*.deb
          asset_name: ${{ env.APP_NAME }}-${{ github.ref_name }}-amd64.deb
          asset_content_type: application/vnd.debian.binary-package

      - name: Deploy to Production Servers
        uses: appleboy/ssh-action@master
        with:
          host: ${{ secrets.PROD_HOST }}
          username: ${{ secrets.PROD_USER }}
          key: ${{ secrets.SSH_PRIVATE_KEY }}
          script: |
            cd /opt/monappli
            ./deploy.sh ${{ github.ref_name }}

      - name: Smoke Tests
        run: |
          sleep 15
          curl --fail https://monappli.com/health || exit 1
          curl --fail https://monappli.com/api/version | grep ${{ github.ref_name }}

      - name: Notify Team
        uses: 8398a7/action-slack@v3
        with:
          status: ${{ job.status }}
          text: '🚀 Release ${{ github.ref_name }} déployée en production!'
          webhook_url: ${{ secrets.SLACK_WEBHOOK }}

  # ============================================
  # JOB 8 : Notification finale
  # ============================================
  notify:
    name: Send Notifications
    runs-on: ubuntu-latest
    needs: [deploy-staging, deploy-production]
    if: always()
    steps:
      - name: Send Email
        uses: dawidd6/action-send-mail@v3
        with:
          server_address: smtp.gmail.com
          server_port: 465
          username: ${{ secrets.EMAIL_USERNAME }}
          password: ${{ secrets.EMAIL_PASSWORD }}
          subject: Pipeline ${{ job.status }}
          body: |
            Pipeline terminé avec le statut: ${{ job.status }}
            Commit: ${{ github.sha }}
            Auteur: ${{ github.actor }}
          to: dev-team@monappli.com
```

### Secrets GitHub

Configurez les secrets dans GitHub :
```
Settings → Secrets and variables → Actions → New repository secret
```

**Secrets nécessaires :**
- `STAGING_HOST` : IP du serveur staging
- `STAGING_USER` : Utilisateur SSH staging
- `PROD_HOST` : IP du serveur production
- `PROD_USER` : Utilisateur SSH production
- `SSH_PRIVATE_KEY` : Clé SSH privée
- `SLACK_WEBHOOK` : URL du webhook Slack
- `EMAIL_USERNAME` : Email pour notifications
- `EMAIL_PASSWORD` : Mot de passe email

## GitLab CI : Alternative complète

GitLab CI est intégré à GitLab et très puissant.

### Pipeline GitLab CI

**Fichier : `.gitlab-ci.yml`**
```yaml
# Définir les stages
stages:
  - build
  - test
  - package
  - deploy

# Variables globales
variables:
  APP_NAME: "monappli"
  FPC_VERSION: "3.2.2"

# Template pour cache
.cache_template: &cache_definition
  cache:
    key: ${CI_COMMIT_REF_SLUG}
    paths:
      - .fpc-cache/

# ============================================
# STAGE : BUILD
# ============================================

build:windows:
  stage: build
  tags:
    - windows
  script:
    - choco install lazarus -y
    - fpc -O3 src\main.pas -o%APP_NAME%.exe
  artifacts:
    paths:
      - ${APP_NAME}.exe
    expire_in: 1 week

build:linux:
  stage: build
  image: ubuntu:22.04
  before_script:
    - apt-get update
    - apt-get install -y fpc fp-compiler
  script:
    - fpc -O3 src/main.pas -o${APP_NAME}
  artifacts:
    paths:
      - ${APP_NAME}
    expire_in: 1 week

# ============================================
# STAGE : TEST
# ============================================

test:unit:
  stage: test
  image: ubuntu:22.04
  dependencies:
    - build:linux
  before_script:
    - apt-get update && apt-get install -y fpc
  script:
    - fpc tests/test_main.pas -otest_runner
    - ./test_runner
  coverage: '/TOTAL.*\s+(\d+%)$/'
  artifacts:
    reports:
      junit: test-results.xml
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml

test:integration:
  stage: test
  image: ubuntu:22.04
  services:
    - name: postgres:15
      alias: postgres
  variables:
    POSTGRES_DB: testdb
    POSTGRES_USER: testuser
    POSTGRES_PASSWORD: testpass
    DB_HOST: postgres
  dependencies:
    - build:linux
  script:
    - apt-get update && apt-get install -y fpc postgresql-client
    - psql -h postgres -U testuser -d testdb -c "CREATE TABLE test (id INT);"
    - fpc tests/test_integration.pas -otest_integration
    - ./test_integration

# ============================================
# STAGE : PACKAGE
# ============================================

package:deb:
  stage: package
  image: ubuntu:22.04
  dependencies:
    - build:linux
  script:
    - apt-get update && apt-get install -y dpkg-dev
    - chmod +x scripts/build-deb.sh
    - ./scripts/build-deb.sh
  artifacts:
    paths:
      - "*.deb"

package:windows:
  stage: package
  tags:
    - windows
  dependencies:
    - build:windows
  script:
    - choco install innosetup -y
    - iscc installer\setup.iss
  artifacts:
    paths:
      - "Output/*.exe"

# ============================================
# STAGE : DEPLOY
# ============================================

deploy:staging:
  stage: deploy
  environment:
    name: staging
    url: https://staging.monappli.com
  only:
    - develop
  before_script:
    - 'which ssh-agent || ( apt-get update -y && apt-get install openssh-client -y )'
    - eval $(ssh-agent -s)
    - echo "$SSH_PRIVATE_KEY" | tr -d '\r' | ssh-add -
    - mkdir -p ~/.ssh
    - chmod 700 ~/.ssh
  script:
    - scp ${APP_NAME} ${STAGING_USER}@${STAGING_HOST}:/tmp/
    - ssh ${STAGING_USER}@${STAGING_HOST} "sudo systemctl stop monappli && sudo mv /tmp/${APP_NAME} /opt/monappli/ && sudo systemctl start monappli"
  after_script:
    - curl --fail https://staging.monappli.com/health

deploy:production:
  stage: deploy
  environment:
    name: production
    url: https://monappli.com
  only:
    - tags
  when: manual  # Déploiement manuel pour production
  before_script:
    - eval $(ssh-agent -s)
    - echo "$SSH_PRIVATE_KEY" | tr -d '\r' | ssh-add -
  script:
    - scp ${APP_NAME} ${PROD_USER}@${PROD_HOST}:/tmp/
    - ssh ${PROD_USER}@${PROD_HOST} "/opt/monappli/deploy.sh ${CI_COMMIT_TAG}"
  after_script:
    - sleep 10
    - curl --fail https://monappli.com/health
```

### Variables CI/CD GitLab

Configurez dans : `Settings → CI/CD → Variables`

- `STAGING_HOST`
- `STAGING_USER`
- `PROD_HOST`
- `PROD_USER`
- `SSH_PRIVATE_KEY` (type: File, protected, masked)

## Jenkins : Solution auto-hébergée

Jenkins offre une flexibilité maximale mais nécessite plus de configuration.

### Installation de Jenkins

**Sur Ubuntu :**
```bash
# Java requis
sudo apt update  
sudo apt install -y openjdk-11-jdk

# Jenkins
curl -fsSL https://pkg.jenkins.io/debian/jenkins.io-2023.key | sudo tee \
  /usr/share/keyrings/jenkins-keyring.asc > /dev/null
echo deb [signed-by=/usr/share/keyrings/jenkins-keyring.asc] \
  https://pkg.jenkins.io/debian binary/ | sudo tee \
  /etc/apt/sources.list.d/jenkins.list > /dev/null
sudo apt update  
sudo apt install -y jenkins

# Démarrer Jenkins
sudo systemctl start jenkins  
sudo systemctl enable jenkins

# Obtenir le mot de passe initial
sudo cat /var/lib/jenkins/secrets/initialAdminPassword
```

Accédez à : `http://localhost:8080`

### Jenkinsfile pour FreePascal

**Fichier : `Jenkinsfile`**
```groovy
pipeline {
    agent any

    environment {
        APP_NAME = 'monappli'
        FPC_VERSION = '3.2.2'
    }

    stages {
        stage('Checkout') {
            steps {
                checkout scm
            }
        }

        stage('Build Windows') {
            agent {
                label 'windows'
            }
            steps {
                bat '''
                    fpc -O3 src\\main.pas -o%APP_NAME%.exe
                '''
                archiveArtifacts artifacts: '*.exe', fingerprint: true
            }
        }

        stage('Build Linux') {
            agent {
                docker {
                    image 'ubuntu:22.04'
                }
            }
            steps {
                sh '''
                    apt-get update
                    apt-get install -y fpc
                    fpc -O3 src/main.pas -o${APP_NAME}
                '''
                archiveArtifacts artifacts: env.APP_NAME, fingerprint: true
            }
        }

        stage('Test') {
            parallel {
                stage('Unit Tests') {
                    steps {
                        sh '''
                            fpc tests/test_main.pas -otest_runner
                            ./test_runner
                        '''
                        junit 'test-results.xml'
                    }
                }

                stage('Integration Tests') {
                    steps {
                        sh '''
                            docker-compose up -d postgres
                            fpc tests/test_integration.pas -otest_integration
                            ./test_integration
                            docker-compose down
                        '''
                    }
                }
            }
        }

        stage('Package') {
            parallel {
                stage('DEB Package') {
                    steps {
                        sh './scripts/build-deb.sh'
                        archiveArtifacts artifacts: '*.deb'
                    }
                }

                stage('Windows Installer') {
                    agent {
                        label 'windows'
                    }
                    steps {
                        bat 'iscc installer\\setup.iss'
                        archiveArtifacts artifacts: 'Output\\*.exe'
                    }
                }
            }
        }

        stage('Deploy to Staging') {
            when {
                branch 'develop'
            }
            steps {
                sshagent(credentials: ['ssh-staging']) {
                    sh '''
                        scp ${APP_NAME} user@staging.example.com:/tmp/
                        ssh user@staging.example.com "sudo systemctl stop monappli && sudo mv /tmp/${APP_NAME} /opt/monappli/ && sudo systemctl start monappli"
                    '''
                }
            }
        }

        stage('Deploy to Production') {
            when {
                tag 'v*'
            }
            input {
                message "Deploy to production?"
                ok "Deploy"
            }
            steps {
                sshagent(credentials: ['ssh-production']) {
                    sh '''
                        scp ${APP_NAME} user@prod.example.com:/tmp/
                        ssh user@prod.example.com "/opt/monappli/deploy.sh ${TAG_NAME}"
                    '''
                }
            }
        }
    }

    post {
        success {
            slackSend(
                color: 'good',
                message: "Build ${env.BUILD_NUMBER} réussi: ${env.JOB_NAME}"
            )
        }
        failure {
            slackSend(
                color: 'danger',
                message: "Build ${env.BUILD_NUMBER} échoué: ${env.JOB_NAME}"
            )
            emailext(
                to: 'dev-team@example.com',
                subject: "Build Failed: ${env.JOB_NAME}",
                body: "Build ${env.BUILD_NUMBER} a échoué. Vérifiez les logs."
            )
        }
        always {
            cleanWs()
        }
    }
}
```

## Scripts de déploiement

### Script de déploiement production

**Fichier : `deploy.sh` (sur le serveur)**
```bash
#!/bin/bash
set -e

VERSION=$1  
APP_DIR="/opt/monappli"  
BACKUP_DIR="/opt/monappli-backups"  
NEW_BINARY="/tmp/monappli"

echo "=== Déploiement version ${VERSION} ==="

# 1. Backup
echo "[1/6] Backup de l'ancienne version..."  
mkdir -p ${BACKUP_DIR}  
cp ${APP_DIR}/monappli ${BACKUP_DIR}/monappli-$(date +%Y%m%d-%H%M%S)

# 2. Arrêt du service
echo "[2/6] Arrêt du service..."  
sudo systemctl stop monappli

# 3. Remplacement du binaire
echo "[3/6] Remplacement du binaire..."  
mv ${NEW_BINARY} ${APP_DIR}/monappli  
chmod +x ${APP_DIR}/monappli  
chown monappli:monappli ${APP_DIR}/monappli

# 4. Migration base de données (si nécessaire)
echo "[4/6] Migration base de données..."  
if [ -f "${APP_DIR}/migrations/${VERSION}.sql" ]; then
    psql -h localhost -U monappli -d monappli_prod < ${APP_DIR}/migrations/${VERSION}.sql
    echo "Migration ${VERSION} appliquée"
else
    echo "Aucune migration pour cette version"
fi

# 5. Redémarrage du service
echo "[5/6] Redémarrage du service..."  
sudo systemctl start monappli

# 6. Health check
echo "[6/6] Vérification de santé..."  
sleep 5

for i in {1..10}; do
    if curl -f http://localhost:8080/health > /dev/null 2>&1; then
        echo "✓ Application démarrée avec succès"
        echo "✓ Version ${VERSION} déployée"
        exit 0
    fi
    echo "Tentative ${i}/10..."
    sleep 3
done

# Échec - Rollback
echo "❌ Échec du health check - Rollback"  
sudo systemctl stop monappli  
LAST_BACKUP=$(ls -t ${BACKUP_DIR}/monappli-* | head -1)  
cp ${LAST_BACKUP} ${APP_DIR}/monappli  
sudo systemctl start monappli  
echo "❌ Rollback effectué vers ${LAST_BACKUP}"  
exit 1
```

### Script de rollback

**Fichier : `rollback.sh`**
```bash
#!/bin/bash
set -e

BACKUP_DIR="/opt/monappli-backups"  
APP_DIR="/opt/monappli"

echo "=== Rollback de l'application ==="

# Lister les backups disponibles
echo "Backups disponibles :"  
ls -lh ${BACKUP_DIR}/monappli-*

# Prendre le dernier backup
LAST_BACKUP=$(ls -t ${BACKUP_DIR}/monappli-* | head -1)

if [ -z "$LAST_BACKUP" ]; then
    echo "❌ Aucun backup disponible"
    exit 1
fi

echo "Rollback vers : ${LAST_BACKUP}"  
read -p "Confirmer ? (y/n) " -n 1 -r  
echo  
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Rollback annulé"
    exit 0
fi

# Arrêter le service
sudo systemctl stop monappli

# Restaurer le backup
cp ${LAST_BACKUP} ${APP_DIR}/monappli  
chmod +x ${APP_DIR}/monappli  
chown monappli:monappli ${APP_DIR}/monappli

# Redémarrer
sudo systemctl start monappli

# Vérifier
sleep 5  
if curl -f http://localhost:8080/health; then
    echo "✓ Rollback réussi"
else
    echo "❌ Problème après rollback"
    exit 1
fi
```

## Blue-Green Deployment

Le déploiement Blue-Green permet des mises à jour sans interruption de service.

### Principe

```
Production actuelle → BLUE (version 1.0)
                      ↓
Déployer nouvelle → GREEN (version 1.1)
                      ↓
Tester GREEN
                      ↓
Basculer le trafic : BLUE → GREEN
                      ↓
GREEN devient production  
BLUE reste en backup
```

### Implémentation avec Nginx

**Configuration Nginx : `/etc/nginx/sites-available/monappli`**
```nginx
upstream blue {
    server 127.0.0.1:8080;
}

upstream green {
    server 127.0.0.1:8081;
}

# Par défaut, pointer vers blue
upstream backend {
    server 127.0.0.1:8080;
}

server {
    listen 80;
    server_name monappli.com;

    location / {
        proxy_pass http://backend;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }

    # Endpoint de health check
    location /health {
        access_log off;
        proxy_pass http://backend/health;
    }
}
```

**Script de déploiement Blue-Green :**
```bash
#!/bin/bash
set -e

BLUE_PORT=8080  
GREEN_PORT=8081  
NGINX_CONF="/etc/nginx/sites-available/monappli"  
APP_DIR="/opt/monappli"  
NEW_BINARY="/tmp/monappli"

# Déterminer quelle version est active
ACTIVE=$(curl -s http://localhost/health | jq -r '.port')

if [ "$ACTIVE" == "$BLUE_PORT" ]; then
    echo "BLUE actif → Déployer sur GREEN"
    TARGET_PORT=$GREEN_PORT
    CURRENT="blue"
    NEW="green"
else
    echo "GREEN actif → Déployer sur BLUE"
    TARGET_PORT=$BLUE_PORT
    CURRENT="green"
    NEW="blue"
fi

# Déployer sur l'environnement inactif
echo "Déploiement sur ${NEW} (port ${TARGET_PORT})..."  
cp ${NEW_BINARY} ${APP_DIR}/monappli-${NEW}  
chmod +x ${APP_DIR}/monappli-${NEW}

# Arrêter l'ancien processus inactif
pkill -f "monappli-${NEW}" || true

# Démarrer la nouvelle version
${APP_DIR}/monappli-${NEW} --port=${TARGET_PORT} &
NEW_PID=$!

echo "Nouvelle version démarrée (PID: ${NEW_PID})"

# Attendre et health check
sleep 5  
for i in {1..10}; do
    if curl -f http://localhost:${TARGET_PORT}/health; then
        echo "✓ Health check OK sur ${NEW}"
        break
    fi
    if [ $i -eq 10 ]; then
        echo "❌ Health check échoué"
        kill $NEW_PID
        exit 1
    fi
    sleep 2
done

# Basculer Nginx
echo "Basculement du trafic vers ${NEW}..."  
sed -i "s/server 127.0.0.1:[0-9]*;/server 127.0.0.1:${TARGET_PORT};/" ${NGINX_CONF}  
sudo nginx -t && sudo nginx -s reload

echo "✓ Trafic basculé vers ${NEW}"  
echo "Ancien environnement ${CURRENT} reste actif comme backup"
```

## Canary Deployment

Le déploiement Canary expose la nouvelle version à un petit pourcentage d'utilisateurs.

### Configuration Nginx pour Canary

```nginx
split_clients "${remote_addr}" $backend {
    # 10% vers la nouvelle version (green)
    10%     green;
    # 90% vers l'ancienne version (blue)
    *       blue;
}

upstream blue {
    server 127.0.0.1:8080;
}

upstream green {
    server 127.0.0.1:8081;
}

server {
    listen 80;
    server_name monappli.com;

    location / {
        proxy_pass http://$backend;
        proxy_set_header Host $host;
    }
}
```

**Script d'augmentation progressive :**
```bash
#!/bin/bash

# Démarrer avec 10%
update_split 10

# Attendre et monitorer
sleep 300  # 5 minutes

# Si OK, augmenter à 50%
if check_error_rate; then
    update_split 50
    sleep 300

    # Si toujours OK, 100%
    if check_error_rate; then
        update_split 100
        echo "✓ Déploiement canary réussi"
    fi
else
    # Problème détecté, rollback
    update_split 0
    echo "❌ Rollback canary"
fi

function update_split() {
    PERCENT=$1
    sed -i "s/[0-9]*%/${PERCENT}%/" /etc/nginx/sites-available/monappli
    sudo nginx -s reload
}

function check_error_rate() {
    ERROR_RATE=$(curl -s http://localhost/metrics | grep error_rate | awk '{print $2}')
    if (( $(echo "$ERROR_RATE < 0.01" | bc -l) )); then
        return 0
    else
        return 1
    fi
}
```

## Monitoring et observabilité du pipeline

### Intégration avec Grafana/Prometheus

**Exporter des métriques de déploiement :**

```yaml
# Dans votre pipeline GitHub Actions
- name: Push deployment metrics
  run: |
    cat <<EOF | curl --data-binary @- http://prometheus-pushgateway:9091/metrics/job/deployment
    # HELP deployment_info Deployment information
    # TYPE deployment_info gauge
    deployment_info{version="${{ github.ref_name }}",environment="production",status="success"} 1
    deployment_duration_seconds{version="${{ github.ref_name }}"} ${{ steps.deploy.outputs.duration }}
    EOF
```

**Dashboard Grafana pour CI/CD :**

```json
{
  "dashboard": {
    "title": "CI/CD Metrics",
    "panels": [
      {
        "title": "Deployment Frequency",
        "targets": [
          {
            "expr": "rate(deployment_info[1h])"
          }
        ]
      },
      {
        "title": "Deployment Duration",
        "targets": [
          {
            "expr": "deployment_duration_seconds"
          }
        ]
      },
      {
        "title": "Success Rate",
        "targets": [
          {
            "expr": "sum(deployment_info{status=\"success\"}) / sum(deployment_info)"
          }
        ]
      }
    ]
  }
}
```

### Alertes sur échecs de pipeline

**Prometheus Alert Rules :**
```yaml
groups:
  - name: cicd
    interval: 30s
    rules:
      - alert: DeploymentFailed
        expr: deployment_info{status="failed"} == 1
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "Deployment failed for {{ $labels.version }}"
          description: "Deployment to {{ $labels.environment }} failed"

      - alert: DeploymentSlow
        expr: deployment_duration_seconds > 600
        for: 1m
        labels:
          severity: warning
        annotations:
          summary: "Deployment taking too long"
          description: "Deployment duration: {{ $value }}s"
```

## Tests automatisés dans le pipeline

### Tests unitaires FreePascal

**Fichier de test : `tests/test_main.pas`**
```pascal
program TestMain;

{$mode objfpc}{$H+}

uses
  SysUtils, fpcunit, testregistry, consoletestrunner;

type
  TTestCalculator = class(TTestCase)
  published
    procedure TestAddition;
    procedure TestSubtraction;
  end;

procedure TTestCalculator.TestAddition;  
begin
  AssertEquals('2 + 2 devrait égaler 4', 4, 2 + 2);
end;

procedure TTestCalculator.TestSubtraction;  
begin
  AssertEquals('5 - 3 devrait égaler 2', 2, 5 - 3);
end;

var
  App: TTestRunner;
begin
  RegisterTest(TTestCalculator);
  App := TTestRunner.Create(nil);
  try
    // Paramètres pour sortie XML (intégration CI)
    // Lancer avec : ./test_runner --format=xml --file=test-results.xml
    App.Initialize;
    App.Run;
  finally
    App.Free;
  end;
end.
```

### Tests d'intégration

**Fichier : `tests/test_integration.pas`**
```pascal
program TestIntegration;

{$mode objfpc}{$H+}

uses
  SysUtils, sqldb, pqconnection, fpcunit, testregistry, consoletestrunner;

type
  TTestDatabase = class(TTestCase)
  private
    FConnection: TPQConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnection;
    procedure TestInsert;
    procedure TestQuery;
  end;

procedure TTestDatabase.SetUp;  
begin
  FConnection := TPQConnection.Create(nil);
  FConnection.HostName := GetEnvironmentVariable('DB_HOST');
  FConnection.DatabaseName := GetEnvironmentVariable('DB_NAME');
  FConnection.UserName := GetEnvironmentVariable('DB_USER');
  FConnection.Password := GetEnvironmentVariable('DB_PASSWORD');
end;

procedure TTestDatabase.TearDown;  
begin
  FConnection.Free;
end;

procedure TTestDatabase.TestConnection;  
begin
  FConnection.Open;
  AssertTrue('Connexion devrait être établie', FConnection.Connected);
  FConnection.Close;
end;

procedure TTestDatabase.TestInsert;  
var
  Query: TSQLQuery;
begin
  FConnection.Open;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := 'INSERT INTO test_table (name) VALUES (''test'')';
    Query.ExecSQL;
    AssertTrue('Insert devrait réussir', True);
  finally
    Query.Free;
    FConnection.Close;
  end;
end;

// ... autres tests ...

var
  App: TTestRunner;
begin
  RegisterTest(TTestDatabase);
  App := TTestRunner.Create(nil);
  try
    App.Initialize;
    App.Run;
  finally
    App.Free;
  end;
end.
```

### Tests de performance

**Script de test de charge : `tests/load_test.sh`**
```bash
#!/bin/bash

URL="http://localhost:8080/api/endpoint"  
CONCURRENT_USERS=100  
DURATION=60  # secondes

echo "Test de charge : ${CONCURRENT_USERS} utilisateurs pendant ${DURATION}s"

# Utiliser Apache Bench
ab -n 10000 -c ${CONCURRENT_USERS} -t ${DURATION} ${URL}

# Ou utiliser wrk
wrk -t12 -c${CONCURRENT_USERS} -d${DURATION}s ${URL}

# Vérifier le temps de réponse
AVG_RESPONSE=$(ab -n 1000 -c 10 ${URL} | grep "Time per request" | head -1 | awk '{print $4}')

if (( $(echo "$AVG_RESPONSE > 1000" | bc -l) )); then
    echo "❌ Performance dégradée: ${AVG_RESPONSE}ms"
    exit 1
else
    echo "✓ Performance OK: ${AVG_RESPONSE}ms"
fi
```

## Gestion des environnements

### Configuration par environnement

**Structure :**
```
config/
├── development.ini
├── staging.ini
└── production.ini
```

**Fichier : `config/production.ini`**
```ini
[Application]
Environment=production  
Debug=false  
LogLevel=WARNING

[Database]
Host=${DB_HOST}  
Port=${DB_PORT}  
Name=${DB_NAME}  
User=${DB_USER}  
Password=${DB_PASSWORD}  
MaxConnections=100  
Timeout=30

[Cache]
Enabled=true  
Host=${REDIS_HOST}  
Port=${REDIS_PORT}  
TTL=3600

[Monitoring]
Enabled=true  
MetricsPort=9090
```

**Chargement dans votre application FreePascal :**
```pascal
program MonAppli;

{$mode objfpc}{$H+}

uses
  SysUtils, IniFiles;

var
  Config: TIniFile;
  Environment: String;
  ConfigFile: String;

begin
  // Détecter l'environnement
  Environment := GetEnvironmentVariable('APP_ENV');
  if Environment = '' then
    Environment := 'development';

  ConfigFile := Format('config/%s.ini', [Environment]);

  if not FileExists(ConfigFile) then
  begin
    WriteLn('Erreur: Fichier de configuration introuvable: ', ConfigFile);
    Halt(1);
  end;

  Config := TIniFile.Create(ConfigFile);
  try
    // Remplacer les variables d'environnement
    DBHost := Config.ReadString('Database', 'Host', 'localhost');
    DBHost := StringReplace(DBHost, '${DB_HOST}',
                           GetEnvironmentVariable('DB_HOST'),
                           [rfReplaceAll]);

    // Utiliser la configuration
    WriteLn('Environnement: ', Environment);
    WriteLn('Base de données: ', DBHost);
  finally
    Config.Free;
  end;
end.
```

## Bonnes pratiques CI/CD

### 1. Keep pipelines fast

**Optimisations :**
```yaml
# Cache des dépendances
- name: Cache FPC
  uses: actions/cache@v3
  with:
    path: ~/.fpc
    key: ${{ runner.os }}-fpc-${{ hashFiles('**/*.pas') }}

# Parallélisation
jobs:
  test:
    strategy:
      matrix:
        test: [unit, integration, e2e]
    runs-on: ubuntu-latest
    steps:
      - name: Run ${{ matrix.test }} tests
        run: ./run-tests.sh ${{ matrix.test }}

# Build incrémental
- name: Incremental build
  run: |
    if [ -f .last-build ]; then
      fpc -B $(find src/ -newer .last-build -name "*.pas")
    else
      fpc src/*.pas
    fi
    touch .last-build
```

### 2. Fail fast

```yaml
# Arrêter rapidement en cas d'erreur
set -e  # Dans les scripts bash

# Tests prioritaires d'abord
stages:
  - lint      # Rapide
  - unit      # Moyennement rapide
  - build     # Plus lent
  - integration  # Encore plus lent
  - deploy    # Le plus lent
```

### 3. Versioning sémantique automatique

**Fichier : `.github/workflows/version.yml`**
```yaml
name: Auto Version

on:
  push:
    branches: [ main ]

jobs:
  version:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0

      - name: Bump version
        id: bump
        uses: anothrNick/github-tag-action@1.61.0
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
          WITH_V: true
          DEFAULT_BUMP: patch
          # Commit message:
          # feat: → minor bump
          # fix: → patch bump
          # BREAKING CHANGE: → major bump

      - name: Create Release
        uses: actions/create-release@v1
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        with:
          tag_name: ${{ steps.bump.outputs.new_tag }}
          release_name: Release ${{ steps.bump.outputs.new_tag }}
```

### 4. Pipeline as Code reviewé

```
Traitez votre pipeline comme du code:
- Code review des changements
- Tests du pipeline (linting YAML)
- Versionning
- Documentation
```

### 5. Secrets sécurisés

```yaml
# NE JAMAIS faire ça:
- name: Deploy
  run: |
    PASSWORD="supersecret123"  # ❌ DANGEREUX!

# Toujours utiliser des secrets:
- name: Deploy
  env:
    PASSWORD: ${{ secrets.PASSWORD }}  # ✓ BON
  run: |
    echo "Using secured password"
```

## Métriques DORA

Mesurez la performance de votre CI/CD avec les métriques DORA :

**1. Deployment Frequency (Fréquence de déploiement)**
```
Combien de fois déployez-vous en production ?  
Elite: Multiple fois par jour  
High: Une fois par jour à une fois par semaine  
Medium: Une fois par semaine à une fois par mois  
Low: Moins d'une fois par mois
```

**2. Lead Time for Changes**
```
Temps entre commit et production  
Elite: Moins d'une heure  
High: Un jour à une semaine  
Medium: Une semaine à un mois  
Low: Plus d'un mois
```

**3. Time to Restore Service**
```
Temps pour restaurer le service après incident  
Elite: Moins d'une heure  
High: Moins d'un jour  
Medium: Un jour à une semaine  
Low: Plus d'une semaine
```

**4. Change Failure Rate**
```
% de déploiements causant des problèmes
Elite: 0-15%  
High: 16-30%  
Medium: 31-45%  
Low: 46-100%
```

**Calculer automatiquement :**
```bash
#!/bin/bash
# calculate-dora.sh

# Deployment Frequency (derniers 30 jours)
DEPLOYMENTS=$(git log --since="30 days ago" --grep="deploy:" --oneline | wc -l)  
echo "Déploiements (30j): ${DEPLOYMENTS}"  
echo "Fréquence: $((DEPLOYMENTS / 30)) par jour"

# Lead Time (moyenne derniers 10 commits)
git log -10 --pretty=format:"%h %ci" > /tmp/commits.txt
# Calculer différence entre commit et tag deploy...

# Change Failure Rate
TOTAL_DEPLOYS=$(git log --grep="deploy:" --oneline | wc -l)  
FAILED_DEPLOYS=$(git log --grep="rollback:" --oneline | wc -l)  
FAILURE_RATE=$((FAILED_DEPLOYS * 100 / TOTAL_DEPLOYS))  
echo "Taux d'échec: ${FAILURE_RATE}%"
```

## Conclusion

Vous maîtrisez maintenant les pipelines CI/CD pour vos applications FreePascal/Lazarus !

**Ce que vous avez appris :**

✅ **GitHub Actions** pour CI/CD cloud  
✅ **GitLab CI** comme alternative intégrée  
✅ **Jenkins** pour solution auto-hébergée  
✅ **Scripts de déploiement** robustes  
✅ **Blue-Green & Canary** deployments  
✅ **Tests automatisés** à tous les niveaux  
✅ **Monitoring** du pipeline  
✅ **Gestion des environnements**  
✅ **Bonnes pratiques** professionnelles  
✅ **Métriques DORA** pour mesurer la performance

**Bénéfices du CI/CD :**

🚀 **Rapidité** : Déploiements en minutes  
🔒 **Fiabilité** : Tests automatiques avant déploiement  
📊 **Visibilité** : Traçabilité complète  
🔄 **Agilité** : Rollback rapide si problème  
😌 **Sérénité** : Moins de stress, plus de confiance  
👥 **Collaboration** : Processus standardisé pour toute l'équipe

**Prochaines étapes :**

- **Section 22.5** : Build multi-plateforme automatisé
- **Section 22.6** : Packaging et distribution
- **Section 22.7** : Monitoring et métriques

**Ressources pour approfondir :**

- The Phoenix Project (livre)
- Continuous Delivery (livre - Jez Humble)
- DevOps Handbook (livre)
- DORA Research : https://dora.dev

**Bon CI/CD avec FreePascal/Lazarus ! 🚀⚡**

⏭️ [Build multi-plateforme automatisé](/22-devops-deploiement-multi-os/05-build-multiplateforme-automatise.md)
