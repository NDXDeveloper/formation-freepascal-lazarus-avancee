🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.8 RPC et communication inter-processus

## Introduction

La communication inter-processus (IPC - Inter-Process Communication) permet à différents programmes ou processus de s'échanger des données et de collaborer. C'est un mécanisme fondamental dans les systèmes d'exploitation modernes et les architectures logicielles distribuées.

RPC (Remote Procedure Call) est un paradigme qui permet d'appeler des fonctions d'un processus distant comme si elles étaient locales, masquant la complexité de la communication réseau.

Ces techniques sont essentielles pour :
- Architecture microservices
- Applications distribuées
- Communication entre langages différents
- Optimisation des performances
- Séparation des préoccupations

## Qu'est-ce que l'IPC ?

### Définition

L'IPC (Inter-Process Communication) est un ensemble de mécanismes permettant à des processus de communiquer entre eux, que ce soit :
- **Sur la même machine** (communication locale)
- **Sur des machines différentes** (communication réseau)

```
┌─────────────────────────────────────────────┐
│            Machine locale                   │
│                                             │
│  ┌──────────┐      IPC       ┌──────────┐   │
│  │ Process A│ ←─────────────→│ Process B│   │
│  └──────────┘                └──────────┘   │
│                                             │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│      Communication réseau (RPC)             │
│                                             │
│  ┌──────────┐                 ┌──────────┐  │
│  │Machine A │ ←───────────────│Machine B │  │
│  │Process 1 │    Network      │Process 2 │  │
│  └──────────┘                 └──────────┘  │
│                                             │
└─────────────────────────────────────────────┘
```

### Pourquoi utiliser l'IPC ?

**Avantages :**

1. **Séparation des responsabilités** 🎯
   - Chaque processus fait une chose bien
   - Architecture modulaire

2. **Scalabilité** 📈
   - Ajout de processus selon la charge
   - Distribution sur plusieurs machines

3. **Robustesse** 💪
   - Un crash ne fait pas tomber tout le système
   - Isolation des erreurs

4. **Flexibilité** 🔄
   - Mise à jour d'un composant sans tout redémarrer
   - Différentes technologies/langages

5. **Performance** ⚡
   - Parallélisation du travail
   - Utilisation optimale des ressources

**Exemples concrets :**

```
Navigateur Web :
├─ Process Interface (UI)
├─ Process Rendu (rendering engine)
├─ Process Réseau (network)
└─ Process Plugins (extensions)
    └─ Communication via IPC

Système de base de données :
├─ Process Serveur
└─ Multiples Process Clients
    └─ Communication via sockets/pipes

Application Desktop :
├─ Interface utilisateur (GUI)
└─ Service en arrière-plan (daemon)
    └─ Communication via named pipes
```

## Qu'est-ce que RPC ?

### Définition

RPC (Remote Procedure Call) est un protocole qui permet à un programme d'appeler une procédure (fonction) exécutée dans un autre espace d'adressage, généralement sur une autre machine, comme s'il s'agissait d'un appel local.

```
Client                               Serveur
  │                                     │
  │ 1. Appel de fonction                │
  │    result := Add(5, 3)              │
  │                                     │
  │ 2. Sérialisation                    │
  │    [RPC Request: Add(5,3)]          │
  │                                     │
  │ 3. Envoi réseau                     │
  │────────────────────────────────────>│
  │                                     │
  │                          4. Désérialisation
  │                             [Add(5,3)]
  │                                     │
  │                          5. Exécution locale
  │                             result := 5 + 3
  │                                     │
  │                          6. Sérialisation réponse
  │                             [RPC Response: 8]
  │                                     │
  │ 7. Envoi réponse                    │
  │<────────────────────────────────────│
  │                                     │
  │ 8. Désérialisation                  │
  │    result := 8                      │
  │                                     │
```

### Transparence de la localisation

L'objectif de RPC est de rendre l'appel distant **transparent** :

```pascal
// Appel local (même processus)
Result := Calculator.Add(5, 3);

// Appel RPC (processus distant)
Result := RemoteCalculator.Add(5, 3);  // Même syntaxe !
```

Le développeur n'a pas besoin de gérer :
- La sérialisation des paramètres
- L'envoi réseau
- La gestion des erreurs réseau (dans la plupart des cas)
- La désérialisation de la réponse

## Types de communication IPC

### 1. Communication locale (même machine)

#### Named Pipes (Tubes nommés)

```
Windows : \\.\pipe\my_pipe  
Linux   : /tmp/my_pipe

Caractéristiques :
├─ Communication bidirectionnelle
├─ Flux d'octets
├─ Peut être réseau (Windows)
└─ Permissions système
```

**Usage :** Communication entre processus locaux (client-serveur local)

#### Unix Domain Sockets

```
Fichier socket : /var/run/app.sock

Caractéristiques :
├─ Comme TCP mais local
├─ Plus rapide que TCP/IP
├─ Contrôle d'accès par permissions fichiers
└─ Linux/Unix uniquement
```

**Usage :** Serveurs web, bases de données locales (PostgreSQL, MySQL)

#### Shared Memory (Mémoire partagée)

```
┌──────────────┐
│ Process A    │──┐
└──────────────┘  │
                  ├──> ┌─────────────────┐
┌──────────────┐  │    │ Shared Memory   │
│ Process B    │──┘    │   Region        │
└──────────────┘       └─────────────────┘

Caractéristiques :
├─ Communication la plus rapide
├─ Partage direct de mémoire
├─ Nécessite synchronisation
└─ Complexe à gérer
```

**Usage :** Échange de gros volumes de données, applications temps réel

#### Message Queues (Files de messages)

```
Sender ──> [Message Queue] ──> Receiver
            │  MSG1  │
            │  MSG2  │
            │  MSG3  │

Caractéristiques :
├─ Asynchrone
├─ Ordre préservé
├─ Persistance possible
└─ Découplage émetteur/récepteur
```

**Usage :** Traitement asynchrone, architecture événementielle

#### Signals (Signaux)

```
Process A ──[SIGUSR1]──> Process B
           (notification)

Caractéristiques :
├─ Notifications simples
├─ Pas de données (juste un signal)
├─ Unix/Linux principalement
└─ Asynchrone
```

**Usage :** Notifications, gestion de processus (SIGTERM, SIGKILL)

### 2. Communication réseau

#### TCP/IP Sockets

```
Client ─────[TCP]─────> Serveur
       (réseau/Internet)

Caractéristiques :
├─ Universel (toutes plateformes)
├─ Fiable (garantie de livraison)
├─ Peut traverser Internet
└─ Overhead plus important
```

**Usage :** Applications distribuées, services web

#### UDP Sockets

```
Client ─────[UDP]─────> Serveur
       (datagrammes)

Caractéristiques :
├─ Rapide mais non fiable
├─ Sans connexion
├─ Faible latence
└─ Pas d'ordre garanti
```

**Usage :** Streaming, jeux en ligne, DNS

#### HTTP/REST

```
Client ──[HTTP GET /api/users]──> Serveur
       ←[JSON: [{id:1,...}]]─────

Caractéristiques :
├─ Standard web
├─ Stateless
├─ Facile à utiliser
└─ Overhead texte
```

**Usage :** APIs web, microservices

## Comparaison des mécanismes IPC

| Mécanisme | Vitesse | Complexité | Portabilité | Usage typique |
|-----------|---------|------------|-------------|---------------|
| **Shared Memory** | ⚡⚡⚡⚡⚡ | ⭐⭐⭐⭐⭐ | Windows/Linux | Données volumineuses |
| **Unix Sockets** | ⚡⚡⚡⚡ | ⭐⭐ | Linux/Unix | Serveurs locaux |
| **Named Pipes** | ⚡⚡⚡⚡ | ⭐⭐ | Windows/Linux | IPC locale |
| **Message Queues** | ⚡⚡⚡ | ⭐⭐⭐ | Dépend de l'implémentation | Traitement async |
| **TCP Sockets** | ⚡⚡ | ⭐⭐ | ⭐⭐⭐⭐⭐ | Réseau, distribué |
| **HTTP/REST** | ⚡ | ⭐ | ⭐⭐⭐⭐⭐ | APIs web |

**Légende :**
- ⚡ = Vitesse (plus = mieux)
- ⭐ = Complexité (moins = plus simple)

## Architectures RPC

### 1. RPC Synchrone (Blocking)

```
Client                    Serveur
  │                          │
  │─── Appel RPC ───────────>│
  │                          │
  │      [Attend...]         │
  │                          │
  │<─── Réponse ─────────────│
  │                          │
  │ Continue l'exécution     │
```

**Caractéristiques :**
- Simple à comprendre et programmer
- Bloquant (le client attend)
- Similaire à un appel de fonction local

**Usage :** Requêtes simples, calculs

### 2. RPC Asynchrone (Non-blocking)

```
Client                    Serveur
  │                          │
  │─── Appel RPC ───────────>│
  │                          │
  │ Continue l'exécution     │
  │      ...                 │
  │                          │
  │<─── Callback ────────────│
  │                          │
```

**Caractéristiques :**
- Non-bloquant
- Utilise des callbacks ou futures/promises
- Meilleure utilisation des ressources

**Usage :** Applications réactives, GUI

### 3. Streaming RPC

```
Client                    Serveur
  │                          │
  │─── Request ─────────────>│
  │                          │
  │<─── Data Stream 1 ───────│
  │<─── Data Stream 2 ───────│
  │<─── Data Stream 3 ───────│
  │<─── ...                  │
```

**Caractéristiques :**
- Flux de données continu
- Bidirectionnel possible
- Efficace pour gros volumes

**Usage :** Streaming vidéo, logs temps réel

## Sérialisation de données

Pour envoyer des données via RPC, il faut les **sérialiser** (convertir en format transmissible) :

### Formats de sérialisation

#### 1. JSON (JavaScript Object Notation)

```json
{
  "method": "Add",
  "params": [5, 3],
  "id": 1
}
```

**Avantages :**
- ✅ Lisible par l'humain
- ✅ Standard web
- ✅ Support universel

**Inconvénients :**
- ❌ Verbeux (taille)
- ❌ Lent à parser
- ❌ Pas de schéma strict

#### 2. XML (eXtensible Markup Language)

```xml
<request>
  <method>Add</method>
  <params>
    <param>5</param>
    <param>3</param>
  </params>
</request>
```

**Avantages :**
- ✅ Standard ancien et éprouvé
- ✅ Validation par schéma (XSD)
- ✅ Expressif

**Inconvénients :**
- ❌ Très verbeux
- ❌ Complexe à parser
- ❌ Lourd

#### 3. Protocol Buffers (Protobuf)

```protobuf
message CalculRequest {
  string method = 1;
  int32 a = 2;
  int32 b = 3;
}
```

**Avantages :**
- ✅ Binaire (compact)
- ✅ Rapide
- ✅ Schéma strict
- ✅ Rétrocompatible

**Inconvénients :**
- ❌ Pas lisible par l'humain
- ❌ Nécessite compilation du schéma

#### 4. MessagePack

```
Format binaire compact similaire à JSON

Avantages :
├─ Plus compact que JSON
├─ Plus rapide que JSON
└─ Syntaxe similaire à JSON

Inconvénients :
└─ Moins universel
```

### Comparaison des formats

| Format | Taille | Vitesse | Lisibilité | Usage |
|--------|--------|---------|------------|-------|
| **JSON** | 100% | ⚡⚡ | ✅✅✅ | APIs web |
| **XML** | 150% | ⚡ | ✅✅ | Enterprise |
| **Protobuf** | 30% | ⚡⚡⚡⚡ | ❌ | gRPC, performance |
| **MessagePack** | 50% | ⚡⚡⚡ | ❌ | Alternative JSON |

## Gestion des erreurs en RPC

Contrairement aux appels locaux, RPC peut échouer de plusieurs façons :

### Types d'erreurs

```
┌─────────────────────────────────────┐
│  Erreurs possibles en RPC           │
├─────────────────────────────────────┤
│ 1. Réseau indisponible              │
│ 2. Timeout de connexion             │
│ 3. Serveur injoignable              │
│ 4. Serveur surchargé                │
│ 5. Erreur d'exécution côté serveur  │
│ 6. Désérialisation impossible       │
│ 7. Version incompatible             │
└─────────────────────────────────────┘
```

### Stratégies de gestion

#### 1. Retry (Réessayer)

```pascal
function CallWithRetry(MaxRetries: Integer): TResult;  
var
  Attempt: Integer;
begin
  for Attempt := 1 to MaxRetries do
  begin
    try
      Result := RemoteCall();
      Exit; // Succès
    except
      if Attempt = MaxRetries then
        raise; // Dernière tentative, on propage l'erreur
      Sleep(1000 * Attempt); // Backoff exponentiel
    end;
  end;
end;
```

#### 2. Circuit Breaker

```
État FERMÉ (normal)
  │
  │ Trop d'erreurs
  ▼
État OUVERT (arrêt des appels)
  │
  │ Après délai
  ▼
État SEMI-OUVERT (test)
  │
  ├─ Succès ──> État FERMÉ
  └─ Échec ───> État OUVERT
```

#### 3. Fallback (Alternative)

```pascal
try
  Result := RemoteService.GetData();
except
  on E: Exception do
    Result := LocalCache.GetData(); // Données en cache
end;
```

## Protocoles RPC courants

### 1. JSON-RPC

Format simple basé sur JSON :

```json
// Requête
{
  "jsonrpc": "2.0",
  "method": "subtract",
  "params": [42, 23],
  "id": 1
}

// Réponse
{
  "jsonrpc": "2.0",
  "result": 19,
  "id": 1
}
```

**Caractéristiques :**
- Simple et léger
- Transport agnostique (HTTP, WebSocket, TCP)
- Lisible

### 2. XML-RPC

Format XML ancien mais éprouvé :

```xml
<methodCall>
  <methodName>examples.getStateName</methodName>
  <params>
    <param><value><i4>40</i4></value></param>
  </params>
</methodCall>
```

**Caractéristiques :**
- Standard ancien (1998)
- Support large
- Verbeux

### 3. gRPC

Protocole moderne de Google basé sur HTTP/2 et Protobuf :

```protobuf
service Calculator {
  rpc Add (AddRequest) returns (AddResponse);
}
```

**Caractéristiques :**
- Très performant
- Streaming bidirectionnel
- Multi-langage
- Moderne et activement maintenu

### 4. SOAP

Protocole d'entreprise basé sur XML :

```xml
<soap:Envelope>
  <soap:Body>
    <m:GetPrice>
      <m:Item>Apples</m:Item>
    </m:GetPrice>
  </soap:Body>
</soap:Envelope>
```

**Caractéristiques :**
- Très structuré
- Sécurité intégrée (WS-Security)
- Complexe
- Surtout utilisé en entreprise

## Sécurité en RPC/IPC

### Menaces courantes

```
┌─────────────────────────────────────┐
│  Menaces de sécurité                │
├─────────────────────────────────────┤
│ 1. Écoute clandestine (eavesdrop)   │
│ 2. Man-in-the-middle (MITM)         │
│ 3. Injection de commandes           │
│ 4. Déni de service (DoS)            │
│ 5. Élévation de privilèges          │
│ 6. Accès non autorisé               │
└─────────────────────────────────────┘
```

### Mesures de protection

1. **Authentification**
   ```
   - Tokens (JWT, OAuth)
   - Certificats SSL/TLS
   - API Keys
   ```

2. **Chiffrement**
   ```
   - TLS pour le transport
   - Chiffrement des données sensibles
   ```

3. **Autorisation**
   ```
   - Contrôle d'accès basé sur les rôles (RBAC)
   - Permissions granulaires
   ```

4. **Validation**
   ```
   - Validation des entrées
   - Sanitization des données
   - Limites de taille
   ```

5. **Audit**
   ```
   - Logs des accès
   - Monitoring
   - Alertes
   ```

## Performance et optimisation

### Facteurs de performance

```
Latence totale RPC =
  Sérialisation
  + Envoi réseau
  + Traitement serveur
  + Désérialisation
  + Renvoi réseau
```

### Optimisations

1. **Protocole binaire**
   - Protobuf au lieu de JSON
   - Gain : 3-5x en taille et vitesse

2. **Compression**
   - gzip, lz4
   - Gain : 50-80% en taille

3. **Keep-Alive**
   - Réutiliser les connexions
   - Gain : économie du handshake

4. **Batching**
   ```pascal
   // Au lieu de :
   for i := 1 to 1000 do
     RemoteCall(i);

   // Faire :
   RemoteBatchCall([1..1000]);
   ```

5. **Caching**
   ```pascal
   if Cache.Has(Key) then
     Result := Cache.Get(Key)
   else
     Result := RemoteCall(Key);
   ```

6. **Connection pooling**
   - Pool de connexions réutilisables
   - Évite la création/destruction

## Cas d'usage

### 1. Microservices

```
┌──────────────┐     ┌─────────────┐
│   Service    │────>│   Service   │
│   Utilisateur│     │   Commande  │
└──────────────┘     └─────────────┘
      │                    │
      │                    ▼
      │             ┌─────────────┐
      └────────────>│   Service   │
                    │   Paiement  │
                    └─────────────┘
```

### 2. Application Desktop multi-processus

```
┌──────────────────┐
│   Interface GUI  │
└────────┬─────────┘
         │ (IPC)
         ▼
┌──────────────────┐
│ Service Métier   │
└────────┬─────────┘
         │ (IPC)
         ▼
┌──────────────────┐
│  Service Base    │
│  de Données      │
└──────────────────┘
```

### 3. Client-Serveur classique

```
Clients              Serveur
  ┌───┐               ┌───┐
  │ C1│──┐            │   │
  └───┘  │            │   │
  ┌───┐  ├──> RPC ───>│ S │
  │ C2│──┤            │   │
  └───┘  │            │   │
  ┌───┐  │            │   │
  │ C3│──┘            └───┘
  └───┘
```

## Outils et bibliothèques

### Pour FreePascal/Lazarus

1. **Named Pipes** (natif)
   ```pascal
   uses Windows; // ou Unix
   ```

2. **Sockets** (natif)
   ```pascal
   uses Sockets;
   ```

3. **Synapse** (bibliothèque tierce)
   ```pascal
   uses blcksock;
   ```

4. **Indy** (bibliothèque tierce)
   ```pascal
   uses IdTCPClient, IdTCPServer;
   ```

5. **mORMot** (framework complet)
   - RPC intégré
   - JSON-RPC
   - Performances excellentes

## Structure de cette section

**10.8.1** : Named Pipes (Windows) - IPC Windows natif  
**10.8.2** : Unix Domain Sockets (Linux) - IPC Linux rapide  
**10.8.3** : Shared Memory - Mémoire partagée  
**10.8.4** : Message Queues - Files de messages  
**10.8.5** : JSON-RPC - RPC simple avec JSON  
**10.8.6** : XML-RPC - RPC avec XML  
**10.8.7** : Custom RPC - Protocole RPC personnalisé  
**10.8.8** : IPC Performance - Benchmarks et optimisations  

## Prérequis

Avant de commencer cette section :

- ✅ Maîtriser la programmation réseau de base (chapitre 10.1-10.4)
- ✅ Comprendre les processus et threads
- ✅ Connaître la sérialisation (JSON, XML)
- ✅ Être à l'aise avec les sockets
- ✅ Comprendre les concepts de synchronisation

## Bonnes pratiques

### Checklist de développement RPC/IPC

✅ **Conception**
- Définir clairement les interfaces
- Documenter les contrats
- Gérer les versions

✅ **Sécurité**
- Authentifier les appelants
- Valider toutes les entrées
- Chiffrer les données sensibles

✅ **Fiabilité**
- Implémenter des timeouts
- Gérer les erreurs gracieusement
- Prévoir des mécanismes de retry

✅ **Performance**
- Utiliser des formats binaires si nécessaire
- Implémenter du caching
- Mesurer et profiler

✅ **Monitoring**
- Logger les appels importants
- Mesurer les métriques (latence, erreurs)
- Alerter sur les anomalies

---

Maintenant que vous comprenez les concepts fondamentaux de RPC et IPC, explorons les différentes implémentations spécifiques à chaque plateforme !

⏭️ [Named Pipes (Windows)](/10-programmation-reseau-avancee/08.1-named-pipes-windows.md)
