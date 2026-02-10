🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.2 Orchestration Kubernetes

## Introduction à Kubernetes

Vous avez appris à conteneuriser vos applications FreePascal/Lazarus avec Docker dans la section 22.1. Excellent ! Mais que se passe-t-il quand vous devez gérer **des dizaines, voire des centaines de conteneurs** ? Comment gérer la montée en charge, les pannes, les mises à jour sans interruption ? C'est là qu'intervient **Kubernetes**.

### Qu'est-ce que Kubernetes ?

**Kubernetes** (souvent abrégé **K8s** - K + 8 lettres + s) est un système d'orchestration de conteneurs open source développé par Google, maintenant maintenu par la Cloud Native Computing Foundation (CNCF).

**Analogie simple :**
- **Docker** = Un chef cuisinier qui prépare un plat
- **Kubernetes** = Un restaurant complet qui gère plusieurs chefs, commandes, clients, stocks

**Kubernetes fait pour vous :**
- 🚀 **Déploiement automatisé** : Lance vos applications sur un cluster
- ⚖️ **Load balancing** : Répartit le trafic entre plusieurs instances
- 🔄 **Auto-scaling** : Ajoute/retire des instances selon la charge
- 🏥 **Auto-healing** : Redémarre les conteneurs qui plantent
- 📦 **Rolling updates** : Mise à jour sans interruption de service
- 🔙 **Rollback** : Retour arrière si problème
- 💾 **Gestion du stockage** : Monte volumes et secrets
- 🌐 **Service discovery** : Les conteneurs se trouvent automatiquement

### Pourquoi Kubernetes pour FreePascal/Lazarus ?

**Scénario sans Kubernetes :**
```
Vous avez développé une API FreePascal qui fonctionne super bien.
Soudain, 1000 utilisateurs se connectent simultanément.
→ Votre serveur plante
→ Vous devez manuellement lancer plus d'instances
→ Configurer le load balancer manuellement
→ Gérer les adresses IP
→ Monitorer chaque serveur individuellement
→ Stress maximal ! 😰
```

**Scénario avec Kubernetes :**
```yaml
replicas: 1  # Normalement 1 instance suffit

# Charge élevée détectée
→ Kubernetes lance automatiquement 10 instances
→ Load balancer configuré automatiquement
→ Trafic réparti équitablement
→ Tout fonctionne parfaitement ! 😊

# Charge normale revenue
→ Kubernetes réduit à 2 instances
→ Économie de ressources
```

### Concepts de base (vocabulaire K8s)

Avant de plonger, familiarisons-nous avec le vocabulaire Kubernetes :

**Cluster**
Un ensemble de machines (physiques ou virtuelles) qui exécutent Kubernetes.
```
Cluster = Control Plane + Nodes
```

**Node (Nœud)**
Une machine (serveur) dans le cluster. Peut être :
- **Master Node** : Gère le cluster (Control Plane)
- **Worker Node** : Exécute vos applications

**Pod**
La plus petite unité déployable dans Kubernetes. Contient un ou plusieurs conteneurs.
```
Pod = 1 ou plusieurs conteneurs Docker qui partagent :
- Réseau (même IP)
- Stockage
- Cycle de vie
```

**Deployment**
Déclare l'état désiré de votre application.
```yaml
Je veux 3 instances de mon API FreePascal,
avec la version 1.2.3,
utilisant 512MB de RAM chacune
```

**Service**
Point d'accès stable pour communiquer avec vos Pods.
```
Service = Load Balancer interne pour vos Pods
```

**Namespace**
Espace de noms pour isoler les ressources.
```
namespace "production"
namespace "staging"
namespace "dev"
```

**ConfigMap & Secret**
- **ConfigMap** : Configuration en clair
- **Secret** : Configuration sensible (mots de passe, clés)

### Architecture Kubernetes

Comprenons comment Kubernetes fonctionne :

```
┌────────────────────────────────────────────────────────┐
│                    CONTROL PLANE                       │
│  (Cerveau du cluster - Prend les décisions)            │
│                                                        │
│  ┌─────────────┐  ┌──────────────┐  ┌──────────────┐   │
│  │ API Server  │  │  Scheduler   │  │  Controller  │   │
│  │ (Interface) │  │  (Placement) │  │   Manager    │   │
│  └─────────────┘  └──────────────┘  └──────────────┘   │
│                                                        │
│  ┌──────────────────────────────────────────────────┐  │
│  │              etcd (Base de données)              │  │
│  │         Stocke l'état du cluster                 │  │
│  └──────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────┘
                            │
          ┌─────────────────┼─────────────────┐
          │                 │                 │
┌─────────▼────────┐ ┌──────▼────────┐ ┌─────▼──────────┐
│   Worker Node 1  │ │ Worker Node 2 │ │ Worker Node 3  │
│                  │ │               │ │                │
│  ┌────────────┐  │ │  ┌──────────┐ │ │  ┌──────────┐  │
│  │   Pod 1    │  │ │  │  Pod 3   │ │ │  │  Pod 5   │  │
│  │ (API FPC)  │  │ │  │ (DB)     │ │ │  │ (Cache)  │  │
│  └────────────┘  │ │  └──────────┘ │ │  └──────────┘  │
│  ┌────────────┐  │ │  ┌──────────┐ │ │                │
│  │   Pod 2    │  │ │  │  Pod 4   │ │ │                │
│  │ (API FPC)  │  │ │  │ (Web)    │ │ │                │
│  └────────────┘  │ │  └──────────┘ │ │                │
│                  │ │               │ │                │
│    Kubelet       │ │   Kubelet     │ │   Kubelet      │
│    (Agent)       │ │   (Agent)     │ │   (Agent)      │
└──────────────────┘ └───────────────┘ └────────────────┘
```

**Composants du Control Plane :**

**API Server**
- Point d'entrée pour toutes les commandes
- Vous interagissez avec lui via `kubectl`

**Scheduler**
- Décide sur quel Node placer chaque Pod
- Prend en compte : ressources disponibles, contraintes, affinité

**Controller Manager**
- Surveille l'état du cluster
- Assure que l'état réel = état désiré
- Exemple : Si un Pod meurt, le Controller en lance un nouveau

**etcd**
- Base de données clé-valeur
- Stocke toute la configuration du cluster
- Source de vérité unique

**Composants des Worker Nodes :**

**Kubelet**
- Agent qui tourne sur chaque Node
- Reçoit les ordres du Control Plane
- Lance et surveille les Pods

**Kube-proxy**
- Gère le réseau
- Implémente les Services
- Fait du load balancing

**Container Runtime**
- Docker, containerd, ou CRI-O
- Exécute réellement les conteneurs

## Installation de Kubernetes

Il existe plusieurs façons d'installer Kubernetes selon vos besoins.

### Option 1 : Minikube (Développement local - Recommandé pour débuter)

Minikube crée un cluster Kubernetes local sur votre machine.

**Installation sur Ubuntu :**

```bash
# Télécharger Minikube
curl -LO https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64
sudo install minikube-linux-amd64 /usr/local/bin/minikube

# Vérifier
minikube version
```

**Installation sur Windows :**

```powershell
# Avec Chocolatey
choco install minikube

# Ou télécharger depuis
# https://minikube.sigs.k8s.io/docs/start/
```

**Démarrer Minikube :**

```bash
# Démarrer un cluster local
minikube start

# Vérifier le statut
minikube status

# Interface web (optionnel)
minikube dashboard
```

### Option 2 : Docker Desktop Kubernetes

Docker Desktop inclut Kubernetes intégré.

**Activation :**

1. Ouvrez Docker Desktop
2. Settings → Kubernetes
3. Cochez "Enable Kubernetes"
4. Apply & Restart

**Vérification :**
```bash
kubectl cluster-info
```

### Option 3 : Kind (Kubernetes IN Docker)

Kind crée des clusters Kubernetes dans des conteneurs Docker.

```bash
# Installation
curl -Lo ./kind https://kind.sigs.k8s.io/dl/v0.20.0/kind-linux-amd64
chmod +x ./kind
sudo mv ./kind /usr/local/bin/kind

# Créer un cluster
kind create cluster --name mon-cluster

# Lister les clusters
kind get clusters

# Supprimer un cluster
kind delete cluster --name mon-cluster
```

### Option 4 : Production (Cloud ou bare-metal)

Pour la production, utilisez des solutions gérées ou installées :

**Cloud managé (recommandé) :**
- **GKE** (Google Kubernetes Engine)
- **EKS** (Amazon Elastic Kubernetes Service)
- **AKS** (Azure Kubernetes Service)
- **DigitalOcean Kubernetes**

**Auto-hébergé :**
- **kubeadm** : Installation officielle
- **k3s** : Kubernetes léger (parfait pour edge/IoT)
- **RKE** : Rancher Kubernetes Engine

### Installation de kubectl

`kubectl` est l'outil en ligne de commande pour interagir avec Kubernetes.

**Sur Ubuntu :**
```bash
# Télécharger kubectl
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"

# Installer
sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl

# Vérifier
kubectl version --client
```

**Sur Windows :**
```powershell
# Avec Chocolatey
choco install kubernetes-cli

# Vérifier
kubectl version --client
```

**Vérifier la connexion au cluster :**
```bash
kubectl cluster-info
kubectl get nodes
```

## Premier déploiement : Application FreePascal simple

Déployons une application FreePascal simple sur Kubernetes.

### Étape 1 : Préparer l'image Docker

Supposons que vous avez cette application FreePascal :

**Fichier : `hello.pas`**
```pascal
program HelloK8s;

{$mode objfpc}{$H+}

uses
  SysUtils, fphttpserver;

var
  Server: TFPHTTPServer;

procedure HandleRequest(Sender: TObject; var Request: TFPHTTPConnectionRequest;
  var Response: TFPHTTPConnectionResponse);
begin
  Response.Content := '<h1>Hello from FreePascal on Kubernetes!</h1>' +
                      '<p>Hostname: ' + GetHostName + '</p>' +
                      '<p>Version: 1.0.0</p>';
  Response.Code := 200;
  Response.ContentType := 'text/html';
end;

begin
  Server := TFPHTTPServer.Create(nil);
  try
    Server.Port := 8080;
    Server.OnRequest := @HandleRequest;
    WriteLn('Serveur démarré sur le port 8080');
    Server.Active := True;
    ReadLn;
  finally
    Server.Free;
  end;
end.
```

**Dockerfile :**
```dockerfile
FROM ubuntu:22.04

RUN apt-get update && \
    apt-get install -y fpc && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY hello.pas .
RUN fpc hello.pas

EXPOSE 8080

CMD ["./hello"]
```

**Construire et pousser l'image :**
```bash
# Construire
docker build -t votre-username/hello-fpc:1.0.0 .

# Se connecter à Docker Hub
docker login

# Pousser
docker push votre-username/hello-fpc:1.0.0
```

### Étape 2 : Créer un Deployment

Un Deployment décrit comment déployer votre application.

**Fichier : `deployment.yaml`**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: hello-fpc-deployment
  labels:
    app: hello-fpc
spec:
  replicas: 3  # 3 instances de votre application
  selector:
    matchLabels:
      app: hello-fpc
  template:
    metadata:
      labels:
        app: hello-fpc
    spec:
      containers:
      - name: hello-fpc
        image: votre-username/hello-fpc:1.0.0
        ports:
        - containerPort: 8080
        resources:
          requests:
            memory: "128Mi"
            cpu: "100m"
          limits:
            memory: "256Mi"
            cpu: "200m"
```

**Explication ligne par ligne :**

- `apiVersion: apps/v1` : Version de l'API Kubernetes
- `kind: Deployment` : Type de ressource
- `metadata.name` : Nom unique du Deployment
- `spec.replicas: 3` : On veut 3 Pods identiques
- `selector.matchLabels` : Comment identifier les Pods
- `template` : Template du Pod
  - `containers` : Liste des conteneurs
    - `image` : Image Docker à utiliser
    - `ports` : Ports exposés
    - `resources` : Ressources CPU/RAM
      - `requests` : Minimum garanti
      - `limits` : Maximum autorisé

**Appliquer le Deployment :**
```bash
kubectl apply -f deployment.yaml
```

**Vérifier :**
```bash
# Voir les Deployments
kubectl get deployments

# Voir les Pods créés
kubectl get pods

# Détails d'un Pod
kubectl describe pod <nom-du-pod>

# Logs d'un Pod
kubectl logs <nom-du-pod>
```

### Étape 3 : Créer un Service

Un Service expose votre application sur le réseau.

**Fichier : `service.yaml`**
```yaml
apiVersion: v1
kind: Service
metadata:
  name: hello-fpc-service
spec:
  type: LoadBalancer  # Type de Service
  selector:
    app: hello-fpc  # Sélectionne les Pods avec ce label
  ports:
    - protocol: TCP
      port: 80        # Port externe
      targetPort: 8080  # Port du conteneur
```

**Types de Service :**

**ClusterIP (défaut)**
- Accessible uniquement dans le cluster
- Pour communication interne

**NodePort**
- Expose sur un port de chaque Node
- Accessible via <NodeIP>:<NodePort>

**LoadBalancer**
- Crée un load balancer externe (cloud)
- Meilleur pour production

**ExternalName**
- Mappe vers un nom DNS externe

**Appliquer le Service :**
```bash
kubectl apply -f service.yaml

# Voir les Services
kubectl get services

# Attendre l'IP externe (si LoadBalancer)
kubectl get services -w
```

### Étape 4 : Accéder à l'application

**Avec Minikube :**
```bash
# Obtenir l'URL
minikube service hello-fpc-service --url

# Ou ouvrir dans le navigateur
minikube service hello-fpc-service
```

**Avec LoadBalancer (cloud) :**
```bash
# Récupérer l'IP externe
kubectl get service hello-fpc-service

# Accéder via navigateur
http://<EXTERNAL-IP>
```

**Test avec curl :**
```bash
curl http://<IP-ou-URL>
```

Vous devriez voir :
```html
<h1>Hello from FreePascal on Kubernetes!</h1>
<p>Hostname: hello-fpc-deployment-abc123</p>
<p>Version: 1.0.0</p>
```

**Actualiser plusieurs fois** : vous verrez des hostnames différents → le load balancer répartit entre les 3 Pods !

## Gestion des configurations avec ConfigMap et Secret

Les applications ont besoin de configuration. Kubernetes offre ConfigMap et Secret.

### ConfigMap : Configuration non-sensible

**Fichier : `configmap.yaml`**
```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: app-config
data:
  APP_NAME: "Mon Application FreePascal"
  APP_VERSION: "1.0.0"
  LOG_LEVEL: "INFO"
  DATABASE_HOST: "postgres-service"
  DATABASE_PORT: "5432"
  config.ini: |
    [Application]
    Name=Mon Application
    Version=1.0.0

    [Database]
    Host=postgres-service
    Port=5432
```

**Utiliser dans un Deployment :**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: hello-fpc-deployment
spec:
  template:
    spec:
      containers:
      - name: hello-fpc
        image: votre-username/hello-fpc:1.0.0
        # Variables d'environnement depuis ConfigMap
        env:
        - name: APP_NAME
          valueFrom:
            configMapKeyRef:
              name: app-config
              key: APP_NAME
        - name: LOG_LEVEL
          valueFrom:
            configMapKeyRef:
              name: app-config
              key: LOG_LEVEL
        # Monter config.ini comme fichier
        volumeMounts:
        - name: config-volume
          mountPath: /app/config
      volumes:
      - name: config-volume
        configMap:
          name: app-config
          items:
          - key: config.ini
            path: config.ini
```

**Dans votre code FreePascal :**
```pascal
// Lire variable d'environnement
AppName := GetEnvironmentVariable('APP_NAME');

// Lire fichier de config monté
AssignFile(F, '/app/config/config.ini');
Reset(F);
// ...
```

### Secret : Données sensibles

Les Secrets stockent des données sensibles (mots de passe, clés API, certificats).

**Créer un Secret depuis la ligne de commande :**
```bash
kubectl create secret generic db-secret \
  --from-literal=username=dbuser \
  --from-literal=password=supersecret123
```

**Ou depuis un fichier YAML (Base64 encodé) :**
```yaml
apiVersion: v1
kind: Secret
metadata:
  name: db-secret
type: Opaque
data:
  username: ZGJ1c2Vy  # "dbuser" en Base64
  password: c3VwZXJzZWNyZXQxMjM=  # "supersecret123" en Base64
```

**Encoder en Base64 :**
```bash
echo -n "dbuser" | base64
echo -n "supersecret123" | base64
```

**Utiliser dans un Deployment :**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: hello-fpc-deployment
spec:
  template:
    spec:
      containers:
      - name: hello-fpc
        image: votre-username/hello-fpc:1.0.0
        env:
        - name: DB_USERNAME
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: username
        - name: DB_PASSWORD
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: password
```

**Dans votre code FreePascal :**
```pascal
DBUsername := GetEnvironmentVariable('DB_USERNAME');
DBPassword := GetEnvironmentVariable('DB_PASSWORD');
```

## Stockage persistant avec Persistent Volumes

Les conteneurs sont éphémères. Pour persister des données, utilisez des Persistent Volumes.

### PersistentVolumeClaim (PVC)

**Fichier : `pvc.yaml`**
```yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: app-data-pvc
spec:
  accessModes:
    - ReadWriteOnce  # Un seul Pod en lecture/écriture
  resources:
    requests:
      storage: 10Gi  # 10 GB de stockage
  storageClassName: standard  # Classe de stockage (dépend du provider)
```

**AccessModes :**
- `ReadWriteOnce` (RWO) : Un seul Node en lecture/écriture
- `ReadOnlyMany` (ROX) : Plusieurs Nodes en lecture seule
- `ReadWriteMany` (RWX) : Plusieurs Nodes en lecture/écriture

**Utiliser dans un Deployment :**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: hello-fpc-deployment
spec:
  template:
    spec:
      containers:
      - name: hello-fpc
        image: votre-username/hello-fpc:1.0.0
        volumeMounts:
        - name: data-volume
          mountPath: /app/data  # Chemin dans le conteneur
      volumes:
      - name: data-volume
        persistentVolumeClaim:
          claimName: app-data-pvc
```

**Dans votre code FreePascal :**
```pascal
// Écrire dans le volume persistant
DataPath := '/app/data/';
AssignFile(F, DataPath + 'database.db');
Rewrite(F);
// Les données survivent aux redémarrages du Pod
```

## Scaling : Montée en charge automatique

Kubernetes peut automatiquement ajuster le nombre de Pods selon la charge.

### Scaling manuel

```bash
# Passer de 3 à 10 réplicas
kubectl scale deployment hello-fpc-deployment --replicas=10

# Vérifier
kubectl get pods
```

### Horizontal Pod Autoscaler (HPA)

Le HPA ajuste automatiquement le nombre de Pods selon des métriques (CPU, RAM, custom).

**Prérequis : Metrics Server**

Sur Minikube :
```bash
minikube addons enable metrics-server
```

Sur cluster standard :
```bash
kubectl apply -f https://github.com/kubernetes-sigs/metrics-server/releases/latest/download/components.yaml
```

**Créer un HPA :**
```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: hello-fpc-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: hello-fpc-deployment
  minReplicas: 2    # Minimum 2 Pods
  maxReplicas: 10   # Maximum 10 Pods
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 50  # Maintenir CPU à 50%
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 70  # Maintenir RAM à 70%
```

**Appliquer :**
```bash
kubectl apply -f hpa.yaml

# Voir le statut
kubectl get hpa

# Surveiller en temps réel
kubectl get hpa -w
```

**Test de charge :**
```bash
# Générer du trafic
kubectl run -it --rm load-generator --image=busybox /bin/sh

# Dans le conteneur
while true; do wget -q -O- http://hello-fpc-service; done
```

Observez le HPA augmenter automatiquement les réplicas !

## Mises à jour et rollbacks

Kubernetes facilite les mises à jour sans interruption.

### Rolling Update (Mise à jour progressive)

**Nouvelle version de votre app :**
```bash
# Construire la v1.1.0
docker build -t votre-username/hello-fpc:1.1.0 .
docker push votre-username/hello-fpc:1.1.0
```

**Mettre à jour le Deployment :**
```bash
# Méthode 1 : Via kubectl
kubectl set image deployment/hello-fpc-deployment \
  hello-fpc=votre-username/hello-fpc:1.1.0

# Méthode 2 : Modifier le YAML
# Changez image: votre-username/hello-fpc:1.1.0
kubectl apply -f deployment.yaml
```

**Suivre la mise à jour :**
```bash
kubectl rollout status deployment/hello-fpc-deployment
```

**Comment ça fonctionne :**
```
1. Créer un nouveau Pod avec v1.1.0
2. Attendre qu'il soit prêt
3. Supprimer un ancien Pod v1.0.0
4. Répéter jusqu'à remplacement complet
```

**Stratégie de rolling update (dans deployment.yaml) :**
```yaml
spec:
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxSurge: 1        # Max 1 Pod supplémentaire pendant update
      maxUnavailable: 0  # Toujours au moins N Pods disponibles
```

### Rollback (Retour arrière)

Si la nouvelle version a des problèmes :

```bash
# Annuler la dernière mise à jour
kubectl rollout undo deployment/hello-fpc-deployment

# Retourner à une révision spécifique
kubectl rollout history deployment/hello-fpc-deployment
kubectl rollout undo deployment/hello-fpc-deployment --to-revision=2
```

## Application complète : Stack FreePascal avec PostgreSQL

Déployons une application complète avec API FreePascal, base de données PostgreSQL et cache Redis.

### Architecture

```
┌─────────────┐
│   Ingress   │ ← Point d'entrée externe
└──────┬──────┘
       │
┌──────▼──────────┐
│  API Service    │ ← Load Balancer
└──────┬──────────┘
       │
 ┌─────┴─────┬─────────┬─────────┐
 │           │         │         │
┌▼──────┐  ┌▼──────┐ ┌▼──────┐ ┌▼──────┐
│ API   │  │ API   │ │ API   │ │ Redis │
│ Pod 1 │  │ Pod 2 │ │ Pod 3 │ │  Pod  │
└───┬───┘  └───┬───┘ └───┬───┘ └───────┘
    │          │         │
    └──────────┴─────────┘
               │
      ┌────────▼────────┐
      │   PostgreSQL    │
      │   StatefulSet   │
      └─────────────────┘
```

### PostgreSQL StatefulSet

**Fichier : `postgres-statefulset.yaml`**
```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: postgres
spec:
  serviceName: postgres
  replicas: 1
  selector:
    matchLabels:
      app: postgres
  template:
    metadata:
      labels:
        app: postgres
    spec:
      containers:
      - name: postgres
        image: postgres:15-alpine
        ports:
        - containerPort: 5432
        env:
        - name: POSTGRES_DB
          value: "myapp"
        - name: POSTGRES_USER
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: username
        - name: POSTGRES_PASSWORD
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: password
        volumeMounts:
        - name: postgres-storage
          mountPath: /var/lib/postgresql/data
  volumeClaimTemplates:
  - metadata:
      name: postgres-storage
    spec:
      accessModes: [ "ReadWriteOnce" ]
      resources:
        requests:
          storage: 10Gi
```

**Service PostgreSQL :**
```yaml
apiVersion: v1
kind: Service
metadata:
  name: postgres
spec:
  clusterIP: None  # Headless Service pour StatefulSet
  selector:
    app: postgres
  ports:
  - port: 5432
    targetPort: 5432
```

### Redis Deployment

**Fichier : `redis-deployment.yaml`**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: redis
spec:
  replicas: 1
  selector:
    matchLabels:
      app: redis
  template:
    metadata:
      labels:
        app: redis
    spec:
      containers:
      - name: redis
        image: redis:7-alpine
        ports:
        - containerPort: 6379
        command: ["redis-server", "--appendonly", "yes"]
        volumeMounts:
        - name: redis-storage
          mountPath: /data
      volumes:
      - name: redis-storage
        persistentVolumeClaim:
          claimName: redis-pvc
```

### API FreePascal Deployment

**Fichier : `api-deployment.yaml`**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: api-fpc
spec:
  replicas: 3
  selector:
    matchLabels:
      app: api-fpc
  template:
    metadata:
      labels:
        app: api-fpc
        version: v1
    spec:
      containers:
      - name: api
        image: votre-username/api-fpc:1.0.0
        ports:
        - containerPort: 8080
        env:
        - name: DB_HOST
          value: "postgres"
        - name: DB_PORT
          value: "5432"
        - name: DB_NAME
          value: "myapp"
        - name: DB_USER
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: username
        - name: DB_PASSWORD
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: password
        - name: REDIS_HOST
          value: "redis"
        - name: REDIS_PORT
          value: "6379"
        resources:
          requests:
            memory: "256Mi"
            cpu: "100m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
```

**Service API :**
```yaml
apiVersion: v1
kind: Service
metadata:
  name: api-fpc-service
spec:
  type: ClusterIP
  selector:
    app: api-fpc
  ports:
  - port: 80
    targetPort: 8080
```

### Ingress pour exposition externe

L'Ingress gère l'accès HTTP/HTTPS depuis l'extérieur.

**Fichier : `ingress.yaml`**
```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: api-ingress
  annotations:
    nginx.ingress.kubernetes.io/rewrite-target: /
spec:
  ingressClassName: nginx
  rules:
  - host: api.monapp.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: api-fpc-service
            port:
              number: 80
```

**Installer Ingress Controller (nginx) :**

```bash
# Sur Minikube
minikube addons enable ingress

# Sur cluster standard
kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/controller-v1.8.1/deploy/static/provider/cloud/deploy.yaml
```

### Déploiement complet de la stack

**Script : `deploy-all.sh`**
```bash
#!/bin/bash
set -e

echo "=== Déploiement de la stack FreePascal sur Kubernetes ==="

# Créer le namespace
kubectl create namespace freepascal-app || true
kubectl config set-context --current --namespace=freepascal-app

# Secrets
echo "[1/7] Création des secrets..."
kubectl create secret generic db-secret \
  --from-literal=username=dbuser \
  --from-literal=password=supersecret123 \
  --dry-run=client -o yaml | kubectl apply -f -

# PostgreSQL
echo "[2/7] Déploiement PostgreSQL..."
kubectl apply -f postgres-statefulset.yaml
kubectl apply -f postgres-service.yaml

# Attendre que PostgreSQL soit prêt
echo "   Attente de PostgreSQL..."
kubectl wait --for=condition=ready pod -l app=postgres --timeout=300s

# Redis
echo "[3/7] Déploiement Redis..."
kubectl apply -f redis-pvc.yaml
kubectl apply -f redis-deployment.yaml
kubectl apply -f redis-service.yaml

# API FreePascal
echo "[4/7] Déploiement API FreePascal..."
kubectl apply -f api-deployment.yaml
kubectl apply -f api-service.yaml

# ConfigMap
echo "[5/7] Application de la configuration..."
kubectl apply -f configmap.yaml

# HPA
echo "[6/7] Configuration de l'autoscaling..."
kubectl apply -f hpa.yaml

# Ingress
echo "[7/7] Configuration de l'Ingress..."
kubectl apply -f ingress.yaml

echo ""
echo "=== Déploiement terminé ==="
echo ""
echo "Vérifiez l'état avec :"
echo "  kubectl get all"
echo ""
echo "Accédez à l'API via :"
if command -v minikube &> /dev/null; then
    minikube service api-fpc-service --url
fi
```

## Health Checks et Probes

Kubernetes utilise des probes pour surveiller la santé de vos Pods.

### Types de Probes

**1. Liveness Probe**
- "Est-ce que mon application fonctionne ?"
- Si échoue → Kubernetes redémarre le Pod

**2. Readiness Probe**
- "Est-ce que mon application est prête à recevoir du trafic ?"
- Si échoue → Le Pod est retiré du load balancing

**3. Startup Probe**
- "Est-ce que mon application a fini de démarrer ?"
- Utile pour applications à démarrage lent

### Implémentation dans FreePascal

**Code FreePascal avec endpoints de health check :**

```pascal
program APIHealthCheck;

{$mode objfpc}{$H+}

uses
  SysUtils, fphttpserver, sqldb, pqconnection;

var
  Server: TFPHTTPServer;
  DBConnection: TPQConnection;
  IsReady: Boolean = False;

function CheckDatabaseConnection: Boolean;
begin
  try
    if not DBConnection.Connected then
      DBConnection.Open;
    Result := DBConnection.Connected;
  except
    Result := False;
  end;
end;

procedure HandleHealth(var Response: TFPHTTPConnectionResponse);
begin
  // Liveness : L'application tourne-t-elle ?
  Response.Code := 200;
  Response.Content := '{"status":"ok","timestamp":"' +
                      FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '"}';
  Response.ContentType := 'application/json';
end;

procedure HandleReadiness(var Response: TFPHTTPConnectionResponse);
begin
  // Readiness : Peut-on servir des requêtes ?
  if IsReady and CheckDatabaseConnection then
  begin
    Response.Code := 200;
    Response.Content := '{"status":"ready","database":"connected"}';
  end
  else
  begin
    Response.Code := 503;  // Service Unavailable
    Response.Content := '{"status":"not ready","database":"disconnected"}';
  end;
  Response.ContentType := 'application/json';
end;

procedure HandleRequest(Sender: TObject;
  var Request: TFPHTTPConnectionRequest;
  var Response: TFPHTTPConnectionResponse);
begin
  if Request.URI = '/health' then
    HandleHealth(Response)
  else if Request.URI = '/ready' then
    HandleReadiness(Response)
  else if Request.URI = '/' then
  begin
    Response.Code := 200;
    Response.Content := '{"message":"API FreePascal","version":"1.0.0"}';
    Response.ContentType := 'application/json';
  end
  else
  begin
    Response.Code := 404;
    Response.Content := '{"error":"Not Found"}';
    Response.ContentType := 'application/json';
  end;
end;

procedure InitializeDatabase;
begin
  DBConnection := TPQConnection.Create(nil);
  DBConnection.HostName := GetEnvironmentVariable('DB_HOST');
  DBConnection.DatabaseName := GetEnvironmentVariable('DB_NAME');
  DBConnection.UserName := GetEnvironmentVariable('DB_USER');
  DBConnection.Password := GetEnvironmentVariable('DB_PASSWORD');

  try
    DBConnection.Open;
    IsReady := True;
    WriteLn('Base de données connectée - Application prête');
  except
    on E: Exception do
    begin
      WriteLn('Erreur connexion BD: ', E.Message);
      IsReady := False;
    end;
  end;
end;

begin
  WriteLn('Démarrage de l''API FreePascal...');

  // Initialiser la connexion BD
  InitializeDatabase;

  Server := TFPHTTPServer.Create(nil);
  try
    Server.Port := 8080;
    Server.OnRequest := @HandleRequest;
    WriteLn('Serveur démarré sur le port 8080');
    Server.Active := True;

    // Maintenir le serveur actif
    while True do
      Sleep(1000);
  finally
    Server.Free;
    DBConnection.Free;
  end;
end.
```

**Configuration des probes dans le Deployment :**

```yaml
livenessProbe:
  httpGet:
    path: /health
    port: 8080
  initialDelaySeconds: 30  # Attendre 30s après démarrage
  periodSeconds: 10        # Vérifier toutes les 10s
  timeoutSeconds: 5        # Timeout après 5s
  failureThreshold: 3      # Redémarrer après 3 échecs

readinessProbe:
  httpGet:
    path: /ready
    port: 8080
  initialDelaySeconds: 5
  periodSeconds: 5
  successThreshold: 1      # 1 succès = prêt
  failureThreshold: 3      # 3 échecs = pas prêt
```

## Namespaces : Isolation et organisation

Les Namespaces isolent les ressources dans un cluster.

### Créer et utiliser des Namespaces

```bash
# Créer des namespaces
kubectl create namespace production
kubectl create namespace staging
kubectl create namespace development

# Lister les namespaces
kubectl get namespaces

# Déployer dans un namespace spécifique
kubectl apply -f deployment.yaml -n production

# Définir le namespace par défaut
kubectl config set-context --current --namespace=production

# Voir les ressources d'un namespace
kubectl get all -n production
```

### Organisation recommandée

```
production/
├── api-fpc (3 replicas)
├── postgres
└── redis

staging/
├── api-fpc (2 replicas)
├── postgres
└── redis

development/
├── api-fpc (1 replica)
├── postgres
└── redis
```

**Fichier avec namespace explicite :**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: api-fpc
  namespace: production  # Namespace explicite
spec:
  replicas: 3
  # ...
```

### Resource Quotas par Namespace

Limitez les ressources par namespace :

```yaml
apiVersion: v1
kind: ResourceQuota
metadata:
  name: dev-quota
  namespace: development
spec:
  hard:
    requests.cpu: "4"        # Max 4 CPU
    requests.memory: 8Gi     # Max 8 GB RAM
    persistentvolumeclaims: "10"  # Max 10 PVCs
    pods: "20"               # Max 20 Pods
```

## Monitoring et Observabilité

Surveiller vos applications dans Kubernetes est crucial.

### Prometheus et Grafana

**Installation avec Helm :**

```bash
# Installer Helm
curl https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 | bash

# Ajouter le repo Prometheus
helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
helm repo update

# Installer Prometheus + Grafana
helm install prometheus prometheus-community/kube-prometheus-stack \
  --namespace monitoring \
  --create-namespace
```

**Accéder à Grafana :**

```bash
# Port-forward pour accès local
kubectl port-forward -n monitoring svc/prometheus-grafana 3000:80

# Ouvrir http://localhost:3000
# Login: admin / prom-operator
```

### Exporter des métriques depuis FreePascal

**Ajouter un endpoint /metrics :**

```pascal
procedure HandleMetrics(var Response: TFPHTTPConnectionResponse);
var
  Metrics: String;
begin
  Metrics := '# HELP api_requests_total Total API requests' + LineEnding +
             '# TYPE api_requests_total counter' + LineEnding +
             'api_requests_total ' + IntToStr(RequestCount) + LineEnding +
             LineEnding +
             '# HELP api_response_time_seconds API response time' + LineEnding +
             '# TYPE api_response_time_seconds histogram' + LineEnding +
             'api_response_time_seconds_sum ' + FloatToStr(ResponseTimeSum) + LineEnding +
             'api_response_time_seconds_count ' + IntToStr(ResponseTimeCount) + LineEnding;

  Response.Code := 200;
  Response.Content := Metrics;
  Response.ContentType := 'text/plain';
end;
```

**Service Monitor pour Prometheus :**

```yaml
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: api-fpc-monitor
spec:
  selector:
    matchLabels:
      app: api-fpc
  endpoints:
  - port: http
    path: /metrics
    interval: 30s
```

### Logging avec Fluentd

**Architecture de logging :**
```
Pods (logs) → Fluentd → Elasticsearch → Kibana
```

**DaemonSet Fluentd :**

```yaml
apiVersion: apps/v1
kind: DaemonSet
metadata:
  name: fluentd
  namespace: kube-system
spec:
  selector:
    matchLabels:
      k8s-app: fluentd
  template:
    metadata:
      labels:
        k8s-app: fluentd
    spec:
      containers:
      - name: fluentd
        image: fluent/fluentd-kubernetes-daemonset:v1-debian-elasticsearch
        env:
        - name: FLUENT_ELASTICSEARCH_HOST
          value: "elasticsearch.logging.svc.cluster.local"
        - name: FLUENT_ELASTICSEARCH_PORT
          value: "9200"
        volumeMounts:
        - name: varlog
          mountPath: /var/log
        - name: varlibdockercontainers
          mountPath: /var/lib/docker/containers
          readOnly: true
      volumes:
      - name: varlog
        hostPath:
          path: /var/log
      - name: varlibdockercontainers
        hostPath:
          path: /var/lib/docker/containers
```

**Dans votre application FreePascal, loggez en JSON :**

```pascal
procedure LogJSON(Level, Message: String);
var
  LogEntry: String;
begin
  LogEntry := Format('{"timestamp":"%s","level":"%s","message":"%s"}',
                     [FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now),
                      Level,
                      Message]);
  WriteLn(LogEntry);
end;

// Utilisation
LogJSON('INFO', 'Application démarrée');
LogJSON('ERROR', 'Connexion base de données échouée');
```

## Sécurité dans Kubernetes

### RBAC (Role-Based Access Control)

Contrôlez qui peut faire quoi dans le cluster.

**ServiceAccount pour votre application :**

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: api-fpc-sa
  namespace: production
```

**Role (permissions dans un namespace) :**

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: api-fpc-role
  namespace: production
rules:
- apiGroups: [""]
  resources: ["configmaps", "secrets"]
  verbs: ["get", "list"]
```

**RoleBinding (lier le Role au ServiceAccount) :**

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: api-fpc-binding
  namespace: production
subjects:
- kind: ServiceAccount
  name: api-fpc-sa
roleRef:
  kind: Role
  name: api-fpc-role
  apiGroup: rbac.authorization.k8s.io
```

**Utiliser dans le Deployment :**

```yaml
spec:
  template:
    spec:
      serviceAccountName: api-fpc-sa
      containers:
      - name: api
        # ...
```

### Network Policies

Contrôlez le trafic réseau entre Pods.

**Exemple : Seuls les Pods API peuvent contacter PostgreSQL**

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: postgres-policy
spec:
  podSelector:
    matchLabels:
      app: postgres
  policyTypes:
  - Ingress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: api-fpc
    ports:
    - protocol: TCP
      port: 5432
```

### Security Context

Configurez les options de sécurité des conteneurs.

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: api-fpc
spec:
  template:
    spec:
      securityContext:
        runAsNonRoot: true   # Ne pas tourner en root
        runAsUser: 1000      # UID de l'utilisateur
        fsGroup: 1000        # GID pour les volumes
      containers:
      - name: api
        image: votre-username/api-fpc:1.0.0
        securityContext:
          allowPrivilegeEscalation: false
          readOnlyRootFilesystem: true  # Système de fichiers en lecture seule
          capabilities:
            drop:
            - ALL  # Retirer toutes les capabilities Linux
        volumeMounts:
        - name: tmp
          mountPath: /tmp  # Seul /tmp est writable
      volumes:
      - name: tmp
        emptyDir: {}
```

## Helm : Gestionnaire de paquets Kubernetes

Helm simplifie le déploiement d'applications complexes.

### Installation de Helm

```bash
# Linux
curl https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 | bash

# Windows (Chocolatey)
choco install kubernetes-helm

# Vérifier
helm version
```

### Créer un Chart Helm pour votre application

**Structure d'un Chart :**

```
freepascal-app/
├── Chart.yaml
├── values.yaml
├── templates/
│   ├── deployment.yaml
│   ├── service.yaml
│   ├── ingress.yaml
│   ├── configmap.yaml
│   └── secret.yaml
└── charts/  # Dépendances
```

**Fichier : `Chart.yaml`**

```yaml
apiVersion: v2
name: freepascal-app
description: Application FreePascal sur Kubernetes
type: application
version: 1.0.0
appVersion: "1.0.0"
```

**Fichier : `values.yaml`**

```yaml
replicaCount: 3

image:
  repository: votre-username/api-fpc
  tag: "1.0.0"
  pullPolicy: IfNotPresent

service:
  type: ClusterIP
  port: 80

ingress:
  enabled: true
  className: nginx
  hosts:
    - host: api.monapp.com
      paths:
        - path: /
          pathType: Prefix

resources:
  requests:
    memory: "256Mi"
    cpu: "100m"
  limits:
    memory: "512Mi"
    cpu: "500m"

autoscaling:
  enabled: true
  minReplicas: 2
  maxReplicas: 10
  targetCPUUtilizationPercentage: 50

database:
  host: postgres
  name: myapp
  user: dbuser
  password: supersecret123
```

**Fichier : `templates/deployment.yaml`**

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: {{ include "freepascal-app.fullname" . }}
  labels:
    {{- include "freepascal-app.labels" . | nindent 4 }}
spec:
  {{- if not .Values.autoscaling.enabled }}
  replicas: {{ .Values.replicaCount }}
  {{- end }}
  selector:
    matchLabels:
      {{- include "freepascal-app.selectorLabels" . | nindent 6 }}
  template:
    metadata:
      labels:
        {{- include "freepascal-app.selectorLabels" . | nindent 8 }}
    spec:
      containers:
      - name: {{ .Chart.Name }}
        image: "{{ .Values.image.repository }}:{{ .Values.image.tag }}"
        imagePullPolicy: {{ .Values.image.pullPolicy }}
        ports:
        - containerPort: 8080
        env:
        - name: DB_HOST
          value: {{ .Values.database.host }}
        - name: DB_NAME
          value: {{ .Values.database.name }}
        - name: DB_USER
          value: {{ .Values.database.user }}
        - name: DB_PASSWORD
          valueFrom:
            secretKeyRef:
              name: {{ include "freepascal-app.fullname" . }}-secret
              key: password
        resources:
          {{- toYaml .Values.resources | nindent 10 }}
```

### Utiliser le Chart

```bash
# Installer
helm install mon-app ./freepascal-app

# Avec des valeurs personnalisées
helm install mon-app ./freepascal-app \
  --set replicaCount=5 \
  --set image.tag=1.1.0

# Avec un fichier de values custom
helm install mon-app ./freepascal-app -f custom-values.yaml

# Mettre à jour
helm upgrade mon-app ./freepascal-app

# Rollback
helm rollback mon-app

# Désinstaller
helm uninstall mon-app

# Lister les releases
helm list
```

### Publier votre Chart

```bash
# Packager le chart
helm package freepascal-app/

# Créer un index
helm repo index .

# Héberger sur GitHub Pages ou serveur web
# Les utilisateurs peuvent alors :
helm repo add mon-repo https://example.com/helm-charts
helm install mon-app mon-repo/freepascal-app
```

## Bonnes pratiques Kubernetes

### 1. Labels et Annotations

Utilisez des labels cohérents :

```yaml
metadata:
  labels:
    app: api-fpc
    version: v1.0.0
    component: backend
    tier: api
    environment: production
  annotations:
    description: "API FreePascal principale"
    maintainer: "votre.email@example.com"
    documentation: "https://docs.example.com/api"
```

### 2. Resource Limits

Définissez toujours requests et limits :

```yaml
resources:
  requests:  # Minimum garanti
    memory: "256Mi"
    cpu: "100m"
  limits:  # Maximum autorisé
    memory: "512Mi"
    cpu: "500m"
```

### 3. Graceful Shutdown

Gérez proprement l'arrêt des Pods :

```yaml
spec:
  terminationGracePeriodSeconds: 30  # Temps pour arrêt propre
```

**Dans votre code FreePascal :**

```pascal
uses
  BaseUnix;

var
  ShuttingDown: Boolean = False;

procedure SignalHandler(Signal: cint); cdecl;
begin
  WriteLn('Signal SIGTERM reçu, arrêt gracieux...');
  ShuttingDown := True;
end;

begin
  // Installer le handler
  FpSignal(SIGTERM, @SignalHandler);

  // Boucle principale
  while not ShuttingDown do
  begin
    // Traiter les requêtes
    Sleep(100);
  end;

  // Cleanup
  WriteLn('Fermeture des connexions...');
  // Fermer DB, Redis, etc.
  WriteLn('Arrêt terminé.');
end.
```

### 4. Configuration par environnement

Utilisez des overlays Kustomize :

```
k8s/
├── base/
│   ├── kustomization.yaml
│   ├── deployment.yaml
│   └── service.yaml
├── overlays/
│   ├── production/
│   │   ├── kustomization.yaml
│   │   └── replicas.yaml
│   └── staging/
│       ├── kustomization.yaml
│       └── replicas.yaml
```

**base/kustomization.yaml :**
```yaml
resources:
- deployment.yaml
- service.yaml
```

**overlays/production/kustomization.yaml :**
```yaml
bases:
- ../../base

patchesStrategicMerge:
- replicas.yaml

namespace: production
```

**Déployer :**
```bash
kubectl apply -k overlays/production
```

### 5. GitOps avec ArgoCD

Automatisez le déploiement depuis Git.

**Installation ArgoCD :**

```bash
kubectl create namespace argocd
kubectl apply -n argocd -f https://raw.githubusercontent.com/argoproj/argo-cd/stable/manifests/install.yaml

# Accéder à l'interface
kubectl port-forward svc/argocd-server -n argocd 8080:443
```

**Application ArgoCD :**

```yaml
apiVersion: argoproj.io/v1alpha1
kind: Application
metadata:
  name: freepascal-app
  namespace: argocd
spec:
  project: default
  source:
    repoURL: https://github.com/username/freepascal-k8s
    targetRevision: main
    path: k8s/overlays/production
  destination:
    server: https://kubernetes.default.svc
    namespace: production
  syncPolicy:
    automated:
      prune: true
      selfHeal: true
```

Maintenant, chaque commit vers Git déclenche un déploiement automatique !

## Troubleshooting : Résoudre les problèmes

### Commandes de debugging essentielles

```bash
# Voir l'état des Pods
kubectl get pods

# Détails d'un Pod
kubectl describe pod <pod-name>

# Logs d'un Pod
kubectl logs <pod-name>

# Logs en temps réel
kubectl logs -f <pod-name>

# Logs du conteneur précédent (si redémarré)
kubectl logs <pod-name> --previous

# Shell interactif dans un Pod
kubectl exec -it <pod-name> -- /bin/bash

# Copier des fichiers
kubectl cp <pod-name>:/chemin/fichier ./local-fichier

# Port-forward pour accès local
kubectl port-forward pod/<pod-name> 8080:8080

# Événements du cluster
kubectl get events --sort-by='.lastTimestamp'

# Utilisation des ressources
kubectl top nodes
kubectl top pods
```

### Problèmes courants

**1. Pod en CrashLoopBackOff**

```bash
# Voir les logs
kubectl logs <pod-name> --previous

# Causes fréquentes :
# - Application plante au démarrage
# - Port déjà utilisé
# - Dépendance manquante
```

**2. ImagePullBackOff**

```bash
# Vérifier l'image
kubectl describe pod <pod-name>

# Causes :
# - Image n'existe pas
# - Registry privé sans credentials
# - Typo dans le nom de l'image
```

**3. Pod Pending**

```bash
# Voir pourquoi
kubectl describe pod <pod-name>

# Causes :
# - Pas assez de ressources sur les Nodes
# - PVC en attente
# - Contraintes d'affinité non satisfaites
```

**4. Service inaccessible**

```bash
# Vérifier le Service
kubectl get svc
kubectl describe svc <service-name>

# Vérifier les endpoints
kubectl get endpoints <service-name>

# Tester depuis un Pod
kubectl run test --image=busybox --rm -it -- wget -O- http://<service-name>
```

## Conclusion

Vous maîtrisez maintenant l'orchestration Kubernetes pour vos applications FreePascal/Lazarus !

**Ce que vous avez appris :**

✅ **Concepts fondamentaux** de Kubernetes  
✅ **Déploiement d'applications** FreePascal  
✅ **Gestion de la configuration** (ConfigMap, Secret)  
✅ **Stockage persistant** avec PV/PVC  
✅ **Scaling automatique** avec HPA  
✅ **Mises à jour sans interruption** (Rolling Updates)  
✅ **Monitoring et logging** avec Prometheus/Grafana  
✅ **Sécurité** (RBAC, Network Policies)  
✅ **Helm** pour packaging  
✅ **Bonnes pratiques** professionnelles

**Prochaines étapes :**

- **Section 22.3** : Infrastructure as Code (Terraform, Ansible)
- **Section 22.4** : Pipelines CI/CD complets
- **Section 22.5** : Build multi-plateforme automatisé

**Ressources pour approfondir :**

- Documentation officielle : https://kubernetes.io/docs/
- Tutoriels interactifs : https://kubernetes.io/docs/tutorials/
- Kubernetes Patterns (livre)
- Production Kubernetes (livre)

**Bonne orchestration avec Kubernetes ! ☸️🚀**

⏭️ [Infrastructure as Code (Terraform, Ansible)](/22-devops-deploiement-multi-os/03-infrastructure-as-code-terraform-ansible.md)
