🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.3 Infrastructure as Code (Terraform, Ansible)

## Introduction à l'Infrastructure as Code (IaC)

Imaginez que vous devez configurer 10 serveurs pour héberger votre application FreePascal/Lazarus. Vous pourriez :
1. Vous connecter manuellement à chaque serveur
2. Installer les paquets nécessaires un par un
3. Configurer chaque service manuellement
4. Répéter le processus pour chaque serveur

**Temps nécessaire :** Plusieurs heures, voire jours  
**Risques :** Erreurs humaines, configurations incohérentes, documentation obsolète  

**Avec l'Infrastructure as Code (IaC)**, vous écrivez du **code** qui décrit votre infrastructure. Ce code :
- Peut être versionné avec Git
- Est reproductible à l'infini
- Documente automatiquement votre infrastructure
- Peut être testé et validé
- Réduit drastiquement les erreurs

### Qu'est-ce que l'Infrastructure as Code ?

**Définition simple :**
L'Infrastructure as Code (IaC) est une approche qui consiste à **gérer et provisionner l'infrastructure informatique via des fichiers de configuration** plutôt que par des processus manuels.

**Analogie :**
- **Sans IaC** = Construire une maison sans plans, en improvisant
- **Avec IaC** = Construire une maison avec des plans détaillés que vous pouvez réutiliser

**Exemple concret :**

**Méthode manuelle (traditionnelle) :**
```bash
# Se connecter au serveur
ssh user@serveur1.com

# Installer FreePascal
sudo apt update  
sudo apt install fpc lazarus

# Configurer le firewall
sudo ufw allow 8080

# Créer un utilisateur
sudo useradd appuser

# Déployer l'application
# ... 20 autres étapes ...

# Répéter pour serveur2, serveur3, etc.
```

**Méthode IaC (Terraform + Ansible) :**
```hcl
# infrastructure.tf
resource "aws_instance" "app_servers" {
  count = 10
  # Configuration...
}
```

```yaml
# playbook.yml
- hosts: app_servers
  tasks:
    - name: Installer FreePascal
      apt: name=fpc state=present
```

```bash
# Une seule commande pour tout provisionner !
terraform apply && ansible-playbook playbook.yml
```

### Pourquoi l'IaC est crucial pour FreePascal/Lazarus ?

**1. Reproductibilité**
```
Développement → Staging → Production  
Toujours la même configuration !
```

**2. Versionning**
```
Git log :
- v1.0 : Configuration initiale
- v1.1 : Ajout serveur cache Redis
- v1.2 : Migration PostgreSQL 14→15
```

**3. Collaboration**
```
Équipe :
- Dev A modifie l'infrastructure
- Code Review par Dev B
- Merge → Déploiement automatique
```

**4. Documentation vivante**
```
Le code = La documentation  
Toujours à jour automatiquement
```

**5. Disaster Recovery**
```
Serveur crashé ?  
terraform apply → Serveur recréé en 5 minutes
```

## Terraform : Provisionnement d'infrastructure

### Qu'est-ce que Terraform ?

**Terraform** est un outil open source de HashiCorp qui permet de **provisionner et gérer l'infrastructure** sur n'importe quel cloud ou datacenter.

**Ce que Terraform fait :**
- 🌐 Crée des serveurs (AWS, Azure, GCP, DigitalOcean...)
- 🔗 Configure les réseaux
- 💾 Crée des bases de données
- 🔐 Gère les certificats SSL
- 📦 Orchestre l'infrastructure complète

**Ce que Terraform NE fait PAS :**
- ❌ N'installe pas de logiciels sur les serveurs (c'est le rôle d'Ansible)
- ❌ Ne déploie pas votre application
- ❌ Ne configure pas les services

**Analogie :**
- **Terraform** = Architecte qui construit la maison
- **Ansible** = Décorateur qui installe les meubles

### Installation de Terraform

**Sur Ubuntu :**
```bash
# Télécharger la clé GPG
wget -O- https://apt.releases.hashicorp.com/gpg | sudo gpg --dearmor -o /usr/share/keyrings/hashicorp-archive-keyring.gpg

# Ajouter le dépôt
echo "deb [signed-by=/usr/share/keyrings/hashicorp-archive-keyring.gpg] https://apt.releases.hashicorp.com $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/hashicorp.list

# Installer
sudo apt update && sudo apt install terraform

# Vérifier
terraform version
```

**Sur Windows :**
```powershell
# Avec Chocolatey
choco install terraform

# Vérifier
terraform version
```

**Ou télécharger depuis :**
https://www.terraform.io/downloads

### Concepts de base Terraform

**1. Providers**
Des plugins qui permettent à Terraform d'interagir avec différentes plateformes.

```hcl
provider "aws" {
  region = "eu-west-1"
}

provider "digitalocean" {
  token = var.do_token
}
```

**2. Resources**
Les éléments d'infrastructure que vous créez (serveurs, bases de données, réseaux).

```hcl
resource "aws_instance" "web" {
  ami           = "ami-0c55b159cbfafe1f0"
  instance_type = "t2.micro"
}
```

**3. Variables**
Paramètres réutilisables et personnalisables.

```hcl
variable "server_count" {
  description = "Nombre de serveurs"
  default     = 3
}
```

**4. Outputs**
Valeurs à afficher après le déploiement (IP, URLs, etc.).

```hcl
output "server_ip" {
  value = aws_instance.web.public_ip
}
```

**5. State**
Fichier qui garde trace de l'infrastructure actuelle (`terraform.tfstate`).

### Premier projet Terraform : Serveur pour FreePascal

Créons un serveur sur DigitalOcean pour héberger une application FreePascal.

**Structure du projet :**
```
freepascal-infra/
├── main.tf           # Configuration principale
├── variables.tf      # Variables
├── outputs.tf        # Outputs
└── terraform.tfvars  # Valeurs des variables (secrets)
```

**Fichier : `variables.tf`**
```hcl
variable "do_token" {
  description = "Token DigitalOcean"
  type        = string
  sensitive   = true
}

variable "region" {
  description = "Région DigitalOcean"
  type        = string
  default     = "fra1"  # Frankfurt
}

variable "server_size" {
  description = "Taille du serveur"
  type        = string
  default     = "s-1vcpu-1gb"  # 1 CPU, 1 GB RAM
}

variable "ssh_key_name" {
  description = "Nom de la clé SSH"
  type        = string
}
```

**Fichier : `main.tf`**
```hcl
# Configuration Terraform
terraform {
  required_providers {
    digitalocean = {
      source  = "digitalocean/digitalocean"
      version = "~> 2.0"
    }
  }
}

# Provider DigitalOcean
provider "digitalocean" {
  token = var.do_token
}

# Clé SSH
data "digitalocean_ssh_key" "main" {
  name = var.ssh_key_name
}

# Serveur pour application FreePascal
resource "digitalocean_droplet" "freepascal_app" {
  name   = "freepascal-app-server"
  region = var.region
  size   = var.server_size
  image  = "ubuntu-22-04-x64"

  ssh_keys = [
    data.digitalocean_ssh_key.main.id
  ]

  tags = ["freepascal", "production", "app"]

  # Script d'initialisation basique
  user_data = <<-EOF
    #!/bin/bash
    apt-get update
    apt-get install -y ufw
    ufw allow 22
    ufw allow 80
    ufw allow 443
    ufw --force enable
  EOF
}

# Firewall
resource "digitalocean_firewall" "app" {
  name = "freepascal-app-firewall"

  droplet_ids = [digitalocean_droplet.freepascal_app.id]

  # SSH
  inbound_rule {
    protocol         = "tcp"
    port_range       = "22"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # HTTP
  inbound_rule {
    protocol         = "tcp"
    port_range       = "80"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # HTTPS
  inbound_rule {
    protocol         = "tcp"
    port_range       = "443"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # Application (port custom)
  inbound_rule {
    protocol         = "tcp"
    port_range       = "8080"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # Tout sortant autorisé
  outbound_rule {
    protocol              = "tcp"
    port_range            = "1-65535"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }

  outbound_rule {
    protocol              = "udp"
    port_range            = "1-65535"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }
}

# Base de données PostgreSQL managée
resource "digitalocean_database_cluster" "postgres" {
  name       = "freepascal-db"
  engine     = "pg"
  version    = "15"
  size       = "db-s-1vcpu-1gb"
  region     = var.region
  node_count = 1

  tags = ["freepascal", "production", "database"]
}

# Utilisateur de base de données
resource "digitalocean_database_user" "app_user" {
  cluster_id = digitalocean_database_cluster.postgres.id
  name       = "freepascal_app"
}

# Base de données
resource "digitalocean_database_db" "app_db" {
  cluster_id = digitalocean_database_cluster.postgres.id
  name       = "freepascal_production"
}
```

**Fichier : `outputs.tf`**
```hcl
output "server_ip" {
  description = "Adresse IP publique du serveur"
  value       = digitalocean_droplet.freepascal_app.ipv4_address
}

output "server_id" {
  description = "ID du serveur"
  value       = digitalocean_droplet.freepascal_app.id
}

output "database_host" {
  description = "Hôte de la base de données"
  value       = digitalocean_database_cluster.postgres.host
  sensitive   = true
}

output "database_port" {
  description = "Port de la base de données"
  value       = digitalocean_database_cluster.postgres.port
}

output "database_user" {
  description = "Utilisateur de la base de données"
  value       = digitalocean_database_user.app_user.name
}

output "database_password" {
  description = "Mot de passe de la base de données"
  value       = digitalocean_database_user.app_user.password
  sensitive   = true
}
```

**Fichier : `terraform.tfvars`** (Ne JAMAIS committer ce fichier !)
```hcl
do_token      = "dop_v1_xxxxxxxxxxxxxxxxxxxxx"  
ssh_key_name  = "ma-cle-ssh"  
region        = "fra1"  
server_size   = "s-2vcpu-2gb"
```

### Utiliser Terraform

**Workflow Terraform :**

```bash
# 1. Initialiser le projet (télécharger les providers)
terraform init

# 2. Formater le code
terraform fmt

# 3. Valider la syntaxe
terraform validate

# 4. Voir les changements prévus
terraform plan

# 5. Appliquer les changements
terraform apply

# Terraform demandera confirmation, ou :
terraform apply -auto-approve

# 6. Voir les outputs
terraform output

# 7. Voir un output spécifique (sensible)
terraform output -raw database_password

# 8. Détruire l'infrastructure
terraform destroy
```

**Résultat après `terraform apply` :**
```
Apply complete! Resources: 5 added, 0 changed, 0 destroyed.

Outputs:

server_ip = "165.227.xxx.xxx"  
database_host = <sensitive>  
database_port = 25060  
database_user = "freepascal_app"
```

Votre infrastructure est créée ! 🎉

### Infrastructure multi-environnements

Gérons différents environnements (dev, staging, production).

**Structure :**
```
freepascal-infra/
├── modules/
│   └── app-server/
│       ├── main.tf
│       ├── variables.tf
│       └── outputs.tf
├── environments/
│   ├── dev/
│   │   ├── main.tf
│   │   └── terraform.tfvars
│   ├── staging/
│   │   ├── main.tf
│   │   └── terraform.tfvars
│   └── production/
│       ├── main.tf
│       └── terraform.tfvars
```

**Module : `modules/app-server/main.tf`**
```hcl
variable "environment" {
  type = string
}

variable "server_count" {
  type    = number
  default = 1
}

variable "server_size" {
  type = string
}

resource "digitalocean_droplet" "app" {
  count  = var.server_count
  name   = "app-${var.environment}-${count.index + 1}"
  region = "fra1"
  size   = var.server_size
  image  = "ubuntu-22-04-x64"

  tags = ["freepascal", var.environment, "app"]
}

output "server_ips" {
  value = digitalocean_droplet.app[*].ipv4_address
}
```

**Environnement Production : `environments/production/main.tf`**
```hcl
terraform {
  backend "s3" {
    bucket = "terraform-state-freepascal"
    key    = "production/terraform.tfstate"
    region = "eu-west-1"
  }
}

module "app_servers" {
  source = "../../modules/app-server"

  environment  = "production"
  server_count = 3        # 3 serveurs en prod
  server_size  = "s-2vcpu-4gb"  # Plus puissant
}

output "production_ips" {
  value = module.app_servers.server_ips
}
```

**Environnement Dev : `environments/dev/main.tf`**
```hcl
module "app_servers" {
  source = "../../modules/app-server"

  environment  = "dev"
  server_count = 1        # 1 serveur suffit en dev
  server_size  = "s-1vcpu-1gb"  # Plus petit
}
```

**Déployer par environnement :**
```bash
# Dev
cd environments/dev  
terraform init  
terraform apply

# Production
cd ../production  
terraform init  
terraform apply
```

### State Backend : Stockage distant

Le fichier `terraform.tfstate` est critique. Stockez-le de manière sécurisée et partagée.

**Backend S3 (AWS) :**
```hcl
terraform {
  backend "s3" {
    bucket         = "mon-terraform-state"
    key            = "freepascal/terraform.tfstate"
    region         = "eu-west-1"
    encrypt        = true
    dynamodb_table = "terraform-locks"  # Pour le verrouillage
  }
}
```

**Backend Azure :**
```hcl
terraform {
  backend "azurerm" {
    resource_group_name  = "terraform-state"
    storage_account_name = "terraformstate"
    container_name       = "tfstate"
    key                  = "freepascal.tfstate"
  }
}
```

**Backend Terraform Cloud (gratuit pour petites équipes) :**
```hcl
terraform {
  cloud {
    organization = "mon-organisation"

    workspaces {
      name = "freepascal-production"
    }
  }
}
```

## Ansible : Configuration Management

### Qu'est-ce qu'Ansible ?

**Ansible** est un outil d'automatisation open source de Red Hat qui permet de **configurer et gérer des serveurs**.

**Ce qu'Ansible fait :**
- 📦 Installe des logiciels (FreePascal, PostgreSQL, Nginx...)
- ⚙️ Configure des services
- 🚀 Déploie des applications
- 📝 Gère des fichiers de configuration
- 👥 Crée des utilisateurs et permissions

**Caractéristiques d'Ansible :**
- **Agentless** : Pas d'agent à installer sur les serveurs (utilise SSH)
- **Idempotent** : Exécuter 10 fois = même résultat
- **YAML** : Configuration lisible et simple
- **Extensible** : Modules pour tout

### Installation d'Ansible

**Sur Ubuntu :**
```bash
sudo apt update  
sudo apt install ansible

# Vérifier
ansible --version
```

**Sur Windows :**
Ansible ne fonctionne pas nativement sur Windows. Utilisez :
- **WSL 2** (Windows Subsystem for Linux)
- **Docker** avec image Ansible
- **Cygwin** (non recommandé)

**Via pip (toutes plateformes) :**
```bash
pip install ansible

# Vérifier
ansible --version
```

### Concepts de base Ansible

**1. Inventory**
Liste des serveurs à gérer.

**Fichier : `inventory.ini`**
```ini
[app_servers]
app1.example.com  
app2.example.com  
app3.example.com

[db_servers]
db1.example.com

[all:vars]
ansible_user=deployer  
ansible_python_interpreter=/usr/bin/python3
```

**2. Playbook**
Fichier YAML décrivant les tâches à exécuter.

**Fichier : `playbook.yml`**
```yaml
---
- name: Configurer serveur FreePascal
  hosts: app_servers
  become: yes

  tasks:
    - name: Installer FreePascal
      apt:
        name: fpc
        state: present
```

**3. Roles**
Organisations de tâches réutilisables.

```
roles/
└── freepascal/
    ├── tasks/
    │   └── main.yml
    ├── templates/
    │   └── config.ini.j2
    ├── files/
    ├── vars/
    └── handlers/
```

**4. Modules**
Unités de travail (apt, copy, service, user, etc.).

**5. Handlers**
Tâches exécutées uniquement si notifiées (ex: redémarrer un service).

### Premier Playbook : Configurer un serveur FreePascal

**Fichier : `setup-freepascal.yml`**
```yaml
---
- name: Configuration serveur FreePascal
  hosts: app_servers
  become: yes  # Utiliser sudo

  vars:
    app_user: freepascal
    app_dir: /opt/freepascal-app

  tasks:
    # Mise à jour du système
    - name: Mettre à jour APT cache
      apt:
        update_cache: yes
        cache_valid_time: 3600

    # Installation des paquets
    - name: Installer les dépendances
      apt:
        name:
          - fpc
          - fp-compiler
          - fp-utils
          - git
          - nginx
          - postgresql-client
          - redis-tools
        state: present

    # Créer un utilisateur
    - name: Créer utilisateur application
      user:
        name: "{{ app_user }}"
        shell: /bin/bash
        home: "{{ app_dir }}"
        createhome: yes
        system: yes

    # Créer des répertoires
    - name: Créer répertoires application
      file:
        path: "{{ item }}"
        state: directory
        owner: "{{ app_user }}"
        group: "{{ app_user }}"
        mode: '0755'
      loop:
        - "{{ app_dir }}/bin"
        - "{{ app_dir }}/config"
        - "{{ app_dir }}/logs"
        - "{{ app_dir }}/data"

    # Configuration du firewall
    - name: Installer UFW
      apt:
        name: ufw
        state: present

    - name: Configurer UFW - SSH
      ufw:
        rule: allow
        port: '22'
        proto: tcp

    - name: Configurer UFW - HTTP
      ufw:
        rule: allow
        port: '80'
        proto: tcp

    - name: Configurer UFW - HTTPS
      ufw:
        rule: allow
        port: '443'
        proto: tcp

    - name: Configurer UFW - Application
      ufw:
        rule: allow
        port: '8080'
        proto: tcp

    - name: Activer UFW
      ufw:
        state: enabled

    # Configuration système
    - name: Augmenter les limites de fichiers
      lineinfile:
        path: /etc/security/limits.conf
        line: "{{ item }}"
        create: yes
      loop:
        - "{{ app_user }} soft nofile 65536"
        - "{{ app_user }} hard nofile 65536"

    # Service systemd
    - name: Créer service systemd
      template:
        src: templates/freepascal-app.service.j2
        dest: /etc/systemd/system/freepascal-app.service
        mode: '0644'
      notify: Reload systemd

  handlers:
    - name: Reload systemd
      systemd:
        daemon_reload: yes
```

**Template systemd : `templates/freepascal-app.service.j2`**
```ini
[Unit]
Description=FreePascal Application  
After=network.target postgresql.service

[Service]
Type=simple  
User={{ app_user }}  
WorkingDirectory={{ app_dir }}  
ExecStart={{ app_dir }}/bin/app  
Restart=always  
RestartSec=10

Environment="DB_HOST={{ db_host }}"  
Environment="DB_PORT={{ db_port }}"  
Environment="DB_NAME={{ db_name }}"

[Install]
WantedBy=multi-user.target
```

**Exécuter le Playbook :**
```bash
# Vérifier la syntaxe
ansible-playbook --syntax-check setup-freepascal.yml

# Mode dry-run (simulation)
ansible-playbook setup-freepascal.yml --check

# Exécution réelle
ansible-playbook -i inventory.ini setup-freepascal.yml

# Avec verbosité (debugging)
ansible-playbook -i inventory.ini setup-freepascal.yml -v
# -vv, -vvv, -vvvv pour plus de détails
```

### Déploiement d'application FreePascal

**Playbook de déploiement : `deploy-app.yml`**
```yaml
---
- name: Déployer application FreePascal
  hosts: app_servers
  become: yes

  vars:
    app_user: freepascal
    app_dir: /opt/freepascal-app
    app_version: "{{ version | default('latest') }}"
    git_repo: https://github.com/username/freepascal-app.git

  tasks:
    - name: Arrêter l'application
      systemd:
        name: freepascal-app
        state: stopped
      ignore_errors: yes

    - name: Cloner/Mettre à jour le code
      git:
        repo: "{{ git_repo }}"
        dest: "{{ app_dir }}/source"
        version: "{{ app_version }}"
        force: yes
      become_user: "{{ app_user }}"

    - name: Compiler l'application
      command: fpc -O3 -XX -CX main.pas -o{{ app_dir }}/bin/app
      args:
        chdir: "{{ app_dir }}/source"
      become_user: "{{ app_user }}"

    - name: Copier la configuration
      template:
        src: templates/config.ini.j2
        dest: "{{ app_dir }}/config/config.ini"
        owner: "{{ app_user }}"
        group: "{{ app_user }}"
        mode: '0600'

    - name: Démarrer l'application
      systemd:
        name: freepascal-app
        state: started
        enabled: yes

    - name: Attendre que l'application soit prête
      wait_for:
        port: 8080
        delay: 5
        timeout: 60

    - name: Health check
      uri:
        url: http://localhost:8080/health
        status_code: 200
      retries: 3
      delay: 5
```

**Template de configuration : `templates/config.ini.j2`**
```ini
[Application]
Name={{ app_name }}  
Version={{ app_version }}  
Environment={{ environment }}

[Database]
Host={{ db_host }}  
Port={{ db_port }}  
Name={{ db_name }}  
User={{ db_user }}  
Password={{ db_password }}

[Redis]
Host={{ redis_host }}  
Port={{ redis_port }}

[Logging]
Level={{ log_level }}  
Path={{ app_dir }}/logs
```

**Variables d'environnement : `group_vars/production.yml`**
```yaml
---
environment: production  
app_name: MonAppli  
log_level: INFO

# Base de données
db_host: db1.example.com  
db_port: 5432  
db_name: freepascal_prod  
db_user: !vault |
          $ANSIBLE_VAULT;1.1;AES256
          ...crypté...
db_password: !vault |
          $ANSIBLE_VAULT;1.1;AES256
          ...crypté...

# Redis
redis_host: redis1.example.com  
redis_port: 6379
```

**Déployer :**
```bash
# Déployer la version 1.2.0
ansible-playbook -i inventory.ini deploy-app.yml -e "version=v1.2.0"

# Déployer latest
ansible-playbook -i inventory.ini deploy-app.yml
```

### Ansible Roles : Organisation avancée

Les Roles permettent d'organiser le code Ansible de manière réutilisable.

**Structure :**
```
roles/
├── common/
│   ├── tasks/
│   │   └── main.yml
│   └── handlers/
│       └── main.yml
├── freepascal/
│   ├── tasks/
│   │   └── main.yml
│   ├── templates/
│   │   └── config.ini.j2
│   ├── files/
│   ├── vars/
│   │   └── main.yml
│   └── defaults/
│       └── main.yml
├── nginx/
│   └── ...
└── postgresql/
    └── ...
```

**Role common : `roles/common/tasks/main.yml`**
```yaml
---
- name: Mettre à jour le système
  apt:
    upgrade: dist
    update_cache: yes

- name: Installer les paquets de base
  apt:
    name:
      - vim
      - htop
      - curl
      - wget
      - git
      - unzip
    state: present

- name: Configurer le timezone
  timezone:
    name: Europe/Paris

- name: Configurer NTP
  apt:
    name: ntp
    state: present

- name: Désactiver root SSH
  lineinfile:
    path: /etc/ssh/sshd_config
    regexp: '^PermitRootLogin'
    line: 'PermitRootLogin no'
  notify: Restart SSH

- name: Configurer fail2ban
  apt:
    name: fail2ban
    state: present
```

**Playbook utilisant les roles : `site.yml`**
```yaml
---
- name: Configuration complète des serveurs
  hosts: all
  become: yes

  roles:
    - common

- name: Serveurs d'application
  hosts: app_servers
  become: yes

  roles:
    - freepascal
    - nginx

- name: Serveurs de base de données
  hosts: db_servers
  become: yes

  roles:
    - postgresql
```

**Exécuter :**
```bash
ansible-playbook -i inventory.ini site.yml
```

### Ansible Vault : Gestion des secrets

Ansible Vault chiffre les données sensibles.

**Créer un fichier vault :**
```bash
ansible-vault create secrets.yml
# Entrer un mot de passe
# Éditer le fichier
```

**Contenu du fichier `secrets.yml` :**
```yaml
---
db_password: supersecret123  
api_key: abcdef123456  
ssl_private_key: |
  -----BEGIN PRIVATE KEY-----
  MIIEvgIBADANBgkqhk...
  -----END PRIVATE KEY-----
```

**Chiffrer un fichier existant :**
```bash
ansible-vault encrypt secrets.yml
```

**Déchiffrer :**
```bash
ansible-vault decrypt secrets.yml
```

**Éditer un fichier chiffré :**
```bash
ansible-vault edit secrets.yml
```

**Utiliser dans un playbook :**
```yaml
---
- name: Déploiement avec secrets
  hosts: app_servers
  become: yes

  vars_files:
    - secrets.yml  # Fichier chiffré

  tasks:
    - name: Configurer la base de données
      template:
        src: config.ini.j2
        dest: /etc/app/config.ini
      no_log: true  # Ne pas afficher dans les logs
```

**Exécuter avec vault :**
```bash
# Demander le mot de passe interactivement
ansible-playbook -i inventory.ini deploy.yml --ask-vault-pass

# Ou utiliser un fichier de mot de passe
echo "motdepasse" > .vault_pass  
ansible-playbook -i inventory.ini deploy.yml --vault-password-file .vault_pass

# Variables d'environnement
export ANSIBLE_VAULT_PASSWORD_FILE=.vault_pass  
ansible-playbook -i inventory.ini deploy.yml
```

**Chiffrer une seule variable :**
```bash
ansible-vault encrypt_string 'supersecret123' --name 'db_password'
```

Résultat à copier dans votre fichier YAML :
```yaml
db_password: !vault |
          $ANSIBLE_VAULT;1.1;AES256
          66386439653765386662643063633063656...
```

## Intégration Terraform + Ansible

Terraform et Ansible fonctionnent parfaitement ensemble.

### Workflow complet

```
1. Terraform → Créer l'infrastructure
2. Terraform → Générer l'inventory Ansible
3. Ansible → Configurer les serveurs
4. Ansible → Déployer l'application
```

### Génération automatique de l'inventory

**Dans Terraform : `main.tf`**
```hcl
# Créer les serveurs
resource "digitalocean_droplet" "app" {
  count  = 3
  name   = "app-${count.index + 1}"
  region = "fra1"
  size   = "s-2vcpu-2gb"
  image  = "ubuntu-22-04-x64"

  ssh_keys = [data.digitalocean_ssh_key.main.id]
}

# Générer l'inventory Ansible
resource "local_file" "ansible_inventory" {
  content = templatefile("${path.module}/inventory.tpl", {
    app_servers = digitalocean_droplet.app[*].ipv4_address
    db_host     = digitalocean_database_cluster.postgres.host
    db_port     = digitalocean_database_cluster.postgres.port
  })
  filename = "${path.module}/inventory.ini"
}
```

**Template d'inventory : `inventory.tpl`**
```ini
[app_servers]
%{ for ip in app_servers ~}
${ip}
%{ endfor ~}

[app_servers:vars]
ansible_user=root  
ansible_python_interpreter=/usr/bin/python3

[db_servers]
${db_host}

[all:vars]
db_host=${db_host}  
db_port=${db_port}
```

**Après `terraform apply`, l'inventory est généré automatiquement :**
```ini
[app_servers]
165.227.xxx.xxx
165.227.yyy.yyy
165.227.zzz.zzz

[app_servers:vars]
ansible_user=root  
ansible_python_interpreter=/usr/bin/python3

[db_servers]
db-postgresql-fra1-12345.b.db.ondigitalocean.com

[all:vars]
db_host=db-postgresql-fra1-12345.b.db.ondigitalocean.com  
db_port=25060
```

### Provisioner Ansible dans Terraform

Terraform peut exécuter Ansible automatiquement après création.

```hcl
resource "digitalocean_droplet" "app" {
  name   = "app-server"
  region = "fra1"
  size   = "s-2vcpu-2gb"
  image  = "ubuntu-22-04-x64"

  ssh_keys = [data.digitalocean_ssh_key.main.id]

  # Attendre que SSH soit prêt
  provisioner "remote-exec" {
    inline = ["echo 'SSH ready'"]

    connection {
      type        = "ssh"
      user        = "root"
      private_key = file("~/.ssh/id_rsa")
      host        = self.ipv4_address
    }
  }

  # Exécuter Ansible
  provisioner "local-exec" {
    command = <<-EOT
      ansible-playbook -i '${self.ipv4_address},' \
        -u root \
        --private-key ~/.ssh/id_rsa \
        setup-server.yml
    EOT
  }
}
```

### Script de déploiement complet

**Fichier : `deploy-infrastructure.sh`**
```bash
#!/bin/bash
set -e

echo "=== Déploiement complet de l'infrastructure ==="

# Variables
ENVIRONMENT=${1:-production}  
VERSION=${2:-latest}

echo "[1/5] Initialisation Terraform..."  
cd terraform/  
terraform init

echo "[2/5] Application Terraform..."  
terraform apply -var-file="environments/${ENVIRONMENT}.tfvars" -auto-approve

echo "[3/5] Attente de la disponibilité des serveurs (30s)..."  
sleep 30

echo "[4/5] Configuration des serveurs avec Ansible..."  
cd ../ansible/  
ansible-playbook -i inventory.ini site.yml

echo "[5/5] Déploiement de l'application..."  
ansible-playbook -i inventory.ini deploy-app.yml -e "version=${VERSION}"

echo ""  
echo "=== Déploiement terminé avec succès ==="  
terraform output -json > ../deployment-info.json  
echo "Informations de déploiement dans deployment-info.json"
```

**Utilisation :**
```bash
# Déploiement production avec version spécifique
./deploy-infrastructure.sh production v1.2.3

# Déploiement staging avec latest
./deploy-infrastructure.sh staging
```

## Patterns avancés et bonnes pratiques

### 1. Structure de projet recommandée

```
freepascal-infrastructure/
├── README.md
├── .gitignore
├── terraform/
│   ├── modules/
│   │   ├── compute/
│   │   │   ├── main.tf
│   │   │   ├── variables.tf
│   │   │   └── outputs.tf
│   │   ├── database/
│   │   │   └── ...
│   │   └── network/
│   │       └── ...
│   ├── environments/
│   │   ├── dev/
│   │   │   ├── main.tf
│   │   │   ├── terraform.tfvars
│   │   │   └── backend.tf
│   │   ├── staging/
│   │   │   └── ...
│   │   └── production/
│   │       └── ...
│   └── scripts/
│       └── deploy.sh
├── ansible/
│   ├── inventory/
│   │   ├── dev.ini
│   │   ├── staging.ini
│   │   └── production.ini
│   ├── roles/
│   │   ├── common/
│   │   ├── freepascal/
│   │   ├── nginx/
│   │   └── postgresql/
│   ├── group_vars/
│   │   ├── all.yml
│   │   ├── dev.yml
│   │   ├── staging.yml
│   │   └── production.yml
│   ├── playbooks/
│   │   ├── site.yml
│   │   ├── deploy.yml
│   │   └── rollback.yml
│   └── ansible.cfg
└── docs/
    ├── architecture.md
    └── deployment.md
```

### 2. Configuration Ansible recommandée

**Fichier : `ansible/ansible.cfg`**
```ini
[defaults]
inventory = inventory/production.ini  
remote_user = deployer  
host_key_checking = False  
retry_files_enabled = False  
gathering = smart  
fact_caching = jsonfile  
fact_caching_connection = /tmp/ansible_facts  
fact_caching_timeout = 3600

# Parallélisme
forks = 10

# Callbacks pour meilleurs outputs
stdout_callback = yaml  
callbacks_enabled = timer, profile_tasks

# SSH
ssh_args = -o ControlMaster=auto -o ControlPersist=60s

[privilege_escalation]
become = True  
become_method = sudo  
become_user = root  
become_ask_pass = False
```

### 3. Inventory dynamique

Plutôt qu'un fichier statique, générez l'inventory dynamiquement.

**Script : `ansible/inventory/digital_ocean.py`**
```python
#!/usr/bin/env python3
import json  
import os  
import requests

def get_droplets():
    token = os.environ.get('DO_TOKEN')
    headers = {'Authorization': f'Bearer {token}'}
    response = requests.get(
        'https://api.digitalocean.com/v2/droplets',
        headers=headers
    )
    return response.json()['droplets']

def generate_inventory():
    droplets = get_droplets()

    inventory = {
        'app_servers': {
            'hosts': [],
            'vars': {
                'ansible_user': 'deployer',
                'ansible_python_interpreter': '/usr/bin/python3'
            }
        },
        '_meta': {
            'hostvars': {}
        }
    }

    for droplet in droplets:
        if 'app' in droplet['tags']:
            ip = droplet['networks']['v4'][0]['ip_address']
            inventory['app_servers']['hosts'].append(ip)
            inventory['_meta']['hostvars'][ip] = {
                'droplet_id': droplet['id'],
                'droplet_name': droplet['name'],
                'region': droplet['region']['slug']
            }

    return inventory

if __name__ == '__main__':
    print(json.dumps(generate_inventory(), indent=2))
```

**Utilisation :**
```bash
# Rendre exécutable
chmod +x inventory/digital_ocean.py

# Tester
export DO_TOKEN="dop_v1_xxx..."
./inventory/digital_ocean.py

# Utiliser dans un playbook
ansible-playbook -i inventory/digital_ocean.py site.yml
```

### 4. Testing de l'infrastructure

**Avec Molecule (pour Ansible) :**

```bash
# Installer Molecule
pip install molecule molecule-docker

# Créer un rôle avec tests
molecule init role freepascal

# Structure créée
freepascal/
├── molecule/
│   └── default/
│       ├── molecule.yml
│       ├── converge.yml
│       └── verify.yml
├── tasks/
│   └── main.yml
└── ...
```

**Fichier : `molecule/default/molecule.yml`**
```yaml
---
driver:
  name: docker

platforms:
  - name: ubuntu22
    image: ubuntu:22.04
    pre_build_image: true

provisioner:
  name: ansible

verifier:
  name: ansible
```

**Tests : `molecule/default/verify.yml`**
```yaml
---
- name: Vérifier l'installation
  hosts: all
  gather_facts: false

  tasks:
    - name: Vérifier que FreePascal est installé
      command: fpc -version
      changed_when: false

    - name: Vérifier que le service est actif
      systemd:
        name: freepascal-app
        state: started
      check_mode: yes
      register: service_status

    - name: Assertion service actif
      assert:
        that:
          - service_status is not changed
        fail_msg: "Le service n'est pas actif"
```

**Exécuter les tests :**
```bash
# Test complet
molecule test

# Étapes individuelles
molecule create    # Créer l'environnement  
molecule converge  # Appliquer le rôle  
molecule verify    # Vérifier  
molecule destroy   # Nettoyer
```

**Avec Terratest (pour Terraform) :**

```go
// test/terraform_test.go
package test

import (
    "testing"
    "github.com/gruntwork-io/terratest/modules/terraform"
    "github.com/stretchr/testify/assert"
)

func TestTerraformInfrastructure(t *testing.T) {
    terraformOptions := &terraform.Options{
        TerraformDir: "../terraform/environments/dev",
    }

    defer terraform.Destroy(t, terraformOptions)

    terraform.InitAndApply(t, terraformOptions)

    serverIP := terraform.Output(t, terraformOptions, "server_ip")
    assert.NotEmpty(t, serverIP)
}
```

### 5. CI/CD pour Infrastructure as Code

**GitHub Actions : `.github/workflows/infrastructure.yml`**
```yaml
name: Infrastructure as Code

on:
  push:
    branches: [ main, develop ]
    paths:
      - 'terraform/**'
      - 'ansible/**'
  pull_request:
    branches: [ main ]

env:
  TF_VERSION: 1.6.0
  ANSIBLE_VERSION: 2.15.0

jobs:
  terraform-validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v2
        with:
          terraform_version: ${{ env.TF_VERSION }}

      - name: Terraform Init
        run: |
          cd terraform/environments/dev
          terraform init -backend=false

      - name: Terraform Format Check
        run: terraform fmt -check -recursive terraform/

      - name: Terraform Validate
        run: |
          cd terraform/environments/dev
          terraform validate

  ansible-lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Python
        uses: actions/setup-python@v4
        with:
          python-version: '3.11'

      - name: Install Ansible & Lint
        run: |
          pip install ansible==${{ env.ANSIBLE_VERSION }} ansible-lint

      - name: Ansible Lint
        run: |
          cd ansible
          ansible-lint playbooks/

  terraform-plan:
    needs: [terraform-validate, ansible-lint]
    runs-on: ubuntu-latest
    if: github.event_name == 'pull_request'
    steps:
      - uses: actions/checkout@v3

      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v2
        with:
          terraform_version: ${{ env.TF_VERSION }}

      - name: Terraform Plan
        env:
          DO_TOKEN: ${{ secrets.DO_TOKEN }}
        run: |
          cd terraform/environments/dev
          terraform init
          terraform plan -out=tfplan

      - name: Comment PR with Plan
        uses: actions/github-script@v6
        with:
          script: |
            // Commenter la PR avec le plan

  deploy:
    needs: [terraform-validate, ansible-lint]
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main' && github.event_name == 'push'
    steps:
      - uses: actions/checkout@v3

      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v2
        with:
          terraform_version: ${{ env.TF_VERSION }}

      - name: Setup Python
        uses: actions/setup-python@v4
        with:
          python-version: '3.11'

      - name: Install Ansible
        run: pip install ansible==${{ env.ANSIBLE_VERSION }}

      - name: Deploy Infrastructure
        env:
          DO_TOKEN: ${{ secrets.DO_TOKEN }}
          ANSIBLE_VAULT_PASSWORD: ${{ secrets.ANSIBLE_VAULT_PASSWORD }}
        run: |
          # Terraform
          cd terraform/environments/production
          terraform init
          terraform apply -auto-approve

          # Ansible
          cd ../../../ansible
          echo "$ANSIBLE_VAULT_PASSWORD" > .vault_pass
          ansible-playbook -i inventory.ini site.yml --vault-password-file .vault_pass
          rm .vault_pass

      - name: Notification
        if: always()
        uses: 8398a7/action-slack@v3
        with:
          status: ${{ job.status }}
          text: 'Déploiement infrastructure: ${{ job.status }}'
        env:
          SLACK_WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK }}
```

### 6. Disaster Recovery et Backup

**Playbook de backup : `ansible/playbooks/backup.yml`**
```yaml
---
- name: Backup de la configuration et des données
  hosts: all
  become: yes

  vars:
    backup_dir: /backup
    backup_date: "{{ ansible_date_time.iso8601_basic_short }}"
    s3_bucket: freepascal-backups

  tasks:
    - name: Créer répertoire de backup
      file:
        path: "{{ backup_dir }}/{{ backup_date }}"
        state: directory

    - name: Backup configuration
      archive:
        path: /etc/freepascal-app
        dest: "{{ backup_dir }}/{{ backup_date }}/config.tar.gz"

    - name: Backup données application
      archive:
        path: /opt/freepascal-app/data
        dest: "{{ backup_dir }}/{{ backup_date }}/data.tar.gz"

    - name: Backup base de données
      postgresql_db:
        name: freepascal_prod
        state: dump
        target: "{{ backup_dir }}/{{ backup_date }}/database.sql"
      delegate_to: "{{ db_host }}"

    - name: Upload vers S3
      aws_s3:
        bucket: "{{ s3_bucket }}"
        object: "{{ inventory_hostname }}/{{ backup_date }}/{{ item }}"
        src: "{{ backup_dir }}/{{ backup_date }}/{{ item }}"
        mode: put
      loop:
        - config.tar.gz
        - data.tar.gz
        - database.sql

    - name: Nettoyer les backups locaux > 7 jours
      find:
        paths: "{{ backup_dir }}"
        age: 7d
        recurse: yes
      register: old_backups

    - name: Supprimer vieux backups
      file:
        path: "{{ item.path }}"
        state: absent
      loop: "{{ old_backups.files }}"
```

**Automation avec cron :**
```yaml
- name: Configurer backup quotidien
  cron:
    name: "Backup quotidien"
    minute: "0"
    hour: "2"
    job: "ansible-playbook /etc/ansible/playbooks/backup.yml"
    user: root
```

**Playbook de restauration : `ansible/playbooks/restore.yml`**
```yaml
---
- name: Restauration depuis backup
  hosts: app_servers
  become: yes

  vars:
    backup_date: "{{ date | mandatory }}"
    s3_bucket: freepascal-backups
    restore_dir: /tmp/restore

  tasks:
    - name: Créer répertoire de restauration
      file:
        path: "{{ restore_dir }}"
        state: directory

    - name: Télécharger backup depuis S3
      aws_s3:
        bucket: "{{ s3_bucket }}"
        object: "{{ inventory_hostname }}/{{ backup_date }}/{{ item }}"
        dest: "{{ restore_dir }}/{{ item }}"
        mode: get
      loop:
        - config.tar.gz
        - data.tar.gz
        - database.sql

    - name: Arrêter l'application
      systemd:
        name: freepascal-app
        state: stopped

    - name: Restaurer la configuration
      unarchive:
        src: "{{ restore_dir }}/config.tar.gz"
        dest: /etc/
        remote_src: yes

    - name: Restaurer les données
      unarchive:
        src: "{{ restore_dir }}/data.tar.gz"
        dest: /opt/freepascal-app/
        remote_src: yes

    - name: Restaurer la base de données
      postgresql_db:
        name: freepascal_prod
        state: restore
        target: "{{ restore_dir }}/database.sql"
      delegate_to: "{{ db_host }}"

    - name: Redémarrer l'application
      systemd:
        name: freepascal-app
        state: started

    - name: Vérifier l'application
      uri:
        url: http://localhost:8080/health
        status_code: 200
      retries: 5
      delay: 10
```

**Utilisation :**
```bash
# Restaurer depuis un backup spécifique
ansible-playbook restore.yml -e "date=20241208T143022"
```

## Documentation de l'infrastructure

### 1. Documentation as Code

**Générer automatiquement la documentation Terraform :**

```bash
# Installer terraform-docs
brew install terraform-docs  # macOS
# ou télécharger depuis github.com/terraform-docs/terraform-docs

# Générer la documentation
cd terraform/modules/compute  
terraform-docs markdown table . > README.md
```

**Résultat automatique dans `README.md` :**
```markdown
## Requirements

| Name | Version |
|------|---------|
| terraform | >= 1.0 |
| digitalocean | ~> 2.0 |

## Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| server_count | Nombre de serveurs | `number` | `1` | no |
| server_size | Taille des serveurs | `string` | `"s-1vcpu-1gb"` | yes |

## Outputs

| Name | Description |
|------|-------------|
| server_ips | IP publiques des serveurs |
```

### 2. Diagrammes d'architecture

**Générer des diagrammes avec Terraform Graph :**

```bash
cd terraform/environments/production  
terraform graph | dot -Tpng > architecture.png
```

**Ou utiliser des outils comme Diagrams (Python) :**

```python
from diagrams import Diagram, Cluster  
from diagrams.digitalocean.compute import Droplet  
from diagrams.digitalocean.database import DbaasPrimary  
from diagrams.onprem.network import Nginx

with Diagram("FreePascal Infrastructure", show=False):
    nginx = Nginx("Load Balancer")

    with Cluster("Application Servers"):
        apps = [Droplet("app1"),
                Droplet("app2"),
                Droplet("app3")]

    db = DbaasPrimary("PostgreSQL")

    nginx >> apps >> db
```

### 3. Runbooks

**Fichier : `docs/runbooks/deploy.md`**
````markdown
# Runbook: Déploiement d'une nouvelle version

## Prérequis
- [ ] Accès SSH aux serveurs
- [ ] Accès au vault Ansible
- [ ] Version à déployer taggée dans Git

## Procédure

### 1. Préparation
```bash
# Vérifier l'état actuel
ansible all -i inventory.ini -m ping  
terraform plan
```

### 2. Backup
```bash
ansible-playbook playbooks/backup.yml
```

### 3. Déploiement
```bash
ansible-playbook playbooks/deploy.yml -e "version=v1.2.3"
```

### 4. Vérification
```bash
# Health check
curl https://api.example.com/health

# Vérifier les logs
ansible app_servers -m shell -a "tail -50 /var/log/freepascal-app/app.log"
```

### 5. Rollback (si nécessaire)
```bash
ansible-playbook playbooks/rollback.yml
```

## Contacts d'urgence
- Ops Team: ops@example.com
- On-call: +33 6 XX XX XX XX
````

## Monitoring de l'infrastructure

### Intégration avec Prometheus

**Playbook : `playbooks/setup-monitoring.yml`**
```yaml
---
- name: Installer Node Exporter (métriques système)
  hosts: all
  become: yes

  tasks:
    - name: Télécharger Node Exporter
      get_url:
        url: https://github.com/prometheus/node_exporter/releases/download/v1.6.1/node_exporter-1.6.1.linux-amd64.tar.gz
        dest: /tmp/node_exporter.tar.gz

    - name: Extraire
      unarchive:
        src: /tmp/node_exporter.tar.gz
        dest: /opt/
        remote_src: yes

    - name: Créer service systemd
      copy:
        dest: /etc/systemd/system/node_exporter.service
        content: |
          [Unit]
          Description=Node Exporter
          After=network.target

          [Service]
          Type=simple
          ExecStart=/opt/node_exporter-1.6.1.linux-amd64/node_exporter

          [Install]
          WantedBy=multi-user.target

    - name: Démarrer Node Exporter
      systemd:
        name: node_exporter
        state: started
        enabled: yes
        daemon_reload: yes
```

**Configuration Prometheus (via Terraform) :**
```hcl
resource "digitalocean_droplet" "prometheus" {
  name   = "prometheus"
  region = "fra1"
  size   = "s-2vcpu-2gb"
  image  = "ubuntu-22-04-x64"

  user_data = templatefile("${path.module}/prometheus-config.tpl", {
    targets = digitalocean_droplet.app[*].ipv4_address
  })
}
```

## Conclusion

Vous maîtrisez maintenant l'Infrastructure as Code avec Terraform et Ansible pour vos applications FreePascal/Lazarus !

**Ce que vous avez appris :**

✅ **Terraform** pour provisionner l'infrastructure  
✅ **Ansible** pour configurer les serveurs  
✅ **Intégration Terraform + Ansible**  
✅ **Gestion des secrets** avec Ansible Vault  
✅ **Environments multiples** (dev, staging, production)  
✅ **CI/CD** pour l'infrastructure  
✅ **Testing** de l'infrastructure  
✅ **Backup et Disaster Recovery**  
✅ **Documentation automatique**  
✅ **Monitoring** de l'infrastructure

**Avantages de l'IaC :**

🚀 **Rapidité** : Infrastructure en minutes au lieu de jours  
🔄 **Reproductibilité** : Même configuration partout  
📝 **Documentation** : Le code est la doc  
🔒 **Sécurité** : Configuration vérifiée et testée  
👥 **Collaboration** : Code review de l'infra  
💾 **Versionning** : Historique complet avec Git  
🔧 **Maintenance** : Changements contrôlés et tracés

**Prochaines étapes :**

- **Section 22.4** : Pipelines CI/CD complets
- **Section 22.5** : Build multi-plateforme automatisé
- **Section 22.6** : Packaging et distribution

**Ressources pour approfondir :**

- Terraform Documentation : https://www.terraform.io/docs/
- Ansible Documentation : https://docs.ansible.com/
- Livre "Terraform: Up & Running" - Yevgeniy Brikman
- Livre "Ansible for DevOps" - Jeff Geerling

**Bonne gestion de votre infrastructure ! 🏗️⚡**

⏭️ [Pipelines CI/CD complets](/22-devops-deploiement-multi-os/04-pipelines-ci-cd-complets.md)
