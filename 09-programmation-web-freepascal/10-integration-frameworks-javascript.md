🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.10 Intégration avec frameworks JavaScript

## Introduction

Bien que FreePascal soit principalement un langage backend, il existe plusieurs façons de l'intégrer avec des frameworks JavaScript modernes pour créer des applications web complètes. Ce chapitre explore différentes approches d'intégration entre votre code Pascal et des frameworks populaires comme React, Vue.js, Angular, et Express.js.

**Pourquoi intégrer FreePascal avec JavaScript ?**

- **Backend robuste** : Utiliser FreePascal pour la logique métier performante
- **Frontend moderne** : Bénéficier des frameworks JavaScript pour l'interface utilisateur
- **Réutilisation de code** : Exploiter le code Pascal existant
- **Architecture complète** : Créer des applications full-stack efficaces

```
┌─────────────────────────────────┐
│   Frontend (JavaScript)         │
│   React / Vue / Angular         │
└────────────┬────────────────────┘
             │ HTTP/REST API
             ▼
┌─────────────────────────────────┐
│   Backend (FreePascal)          │
│   fpWeb / Brook Framework       │
└─────────────────────────────────┘
```

## 9.10.1 API REST avec FreePascal pour frameworks JavaScript

### Création d'une API REST basique

**api_server.pas :**

```pascal
program APIServer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpapp, httpdefs, httproute, fpjson;

type
  TProduct = class
  public
    ID: Integer;
    Name: string;
    Price: Double;
    Stock: Integer;
    function ToJSON: TJSONObject;
  end;

function TProduct.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', ID);
  Result.Add('name', Name);
  Result.Add('price', Price);
  Result.Add('stock', Stock);
end;

var
  Products: array of TProduct;

// Initialiser des données de test
procedure InitializeData;
var
  P: TProduct;
begin
  P := TProduct.Create;
  P.ID := 1;
  P.Name := 'Laptop';
  P.Price := 999.99;
  P.Stock := 15;
  SetLength(Products, 1);
  Products[0] := P;

  P := TProduct.Create;
  P.ID := 2;
  P.Name := 'Mouse';
  P.Price := 29.99;
  P.Stock := 50;
  SetLength(Products, 2);
  Products[1] := P;
end;

// Activer CORS pour permettre les requêtes depuis JavaScript
procedure EnableCORS(AResponse: TResponse);
begin
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
  AResponse.SetCustomHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  AResponse.SetCustomHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');
end;

// GET /api/products - Liste tous les produits
procedure GetProducts(ARequest: TRequest; AResponse: TResponse);
var
  JSONArray: TJSONArray;
  i: Integer;
begin
  EnableCORS(AResponse);

  JSONArray := TJSONArray.Create;
  try
    for i := 0 to High(Products) do
      JSONArray.Add(Products[i].ToJSON);

    AResponse.ContentType := 'application/json';
    AResponse.Content := JSONArray.AsJSON;
    AResponse.SendResponse;
  finally
    JSONArray.Free;
  end;
end;

// GET /api/products/:id - Obtenir un produit par ID
procedure GetProduct(ARequest: TRequest; AResponse: TResponse);
var
  ProductID: Integer;
  i: Integer;
  Found: Boolean;
begin
  EnableCORS(AResponse);

  ProductID := StrToIntDef(ARequest.RouteParams['id'], 0);
  Found := False;

  for i := 0 to High(Products) do
  begin
    if Products[i].ID = ProductID then
    begin
      AResponse.ContentType := 'application/json';
      AResponse.Content := Products[i].ToJSON.AsJSON;
      AResponse.SendResponse;
      Found := True;
      Break;
    end;
  end;

  if not Found then
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Product not found"}';
    AResponse.SendResponse;
  end;
end;

// POST /api/products - Créer un nouveau produit
procedure CreateProduct(ARequest: TRequest; AResponse: TResponse);
var
  InputJSON: TJSONObject;
  NewProduct: TProduct;
  ResponseJSON: TJSONObject;
begin
  EnableCORS(AResponse);

  try
    InputJSON := GetJSON(ARequest.Content) as TJSONObject;
    try
      NewProduct := TProduct.Create;
      NewProduct.ID := Length(Products) + 1;
      NewProduct.Name := InputJSON.Get('name', '');
      NewProduct.Price := InputJSON.Get('price', 0.0);
      NewProduct.Stock := InputJSON.Get('stock', 0);

      SetLength(Products, Length(Products) + 1);
      Products[High(Products)] := NewProduct;

      ResponseJSON := NewProduct.ToJSON;
      try
        AResponse.Code := 201;
        AResponse.ContentType := 'application/json';
        AResponse.Content := ResponseJSON.AsJSON;
        AResponse.SendResponse;
      finally
        ResponseJSON.Free;
      end;
    finally
      InputJSON.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 400;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
      AResponse.SendResponse;
    end;
  end;
end;

// PUT /api/products/:id - Mettre à jour un produit
procedure UpdateProduct(ARequest: TRequest; AResponse: TResponse);
var
  ProductID: Integer;
  InputJSON: TJSONObject;
  i: Integer;
  Found: Boolean;
begin
  EnableCORS(AResponse);

  ProductID := StrToIntDef(ARequest.RouteParams['id'], 0);
  Found := False;

  try
    InputJSON := GetJSON(ARequest.Content) as TJSONObject;
    try
      for i := 0 to High(Products) do
      begin
        if Products[i].ID = ProductID then
        begin
          if InputJSON.IndexOfName('name') >= 0 then
            Products[i].Name := InputJSON.Get('name', Products[i].Name);

          if InputJSON.IndexOfName('price') >= 0 then
            Products[i].Price := InputJSON.Get('price', Products[i].Price);

          if InputJSON.IndexOfName('stock') >= 0 then
            Products[i].Stock := InputJSON.Get('stock', Products[i].Stock);

          AResponse.ContentType := 'application/json';
          AResponse.Content := Products[i].ToJSON.AsJSON;
          AResponse.SendResponse;
          Found := True;
          Break;
        end;
      end;

      if not Found then
      begin
        AResponse.Code := 404;
        AResponse.Content := '{"error": "Product not found"}';
        AResponse.SendResponse;
      end;
    finally
      InputJSON.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 400;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
      AResponse.SendResponse;
    end;
  end;
end;

// DELETE /api/products/:id - Supprimer un produit
procedure DeleteProduct(ARequest: TRequest; AResponse: TResponse);
var
  ProductID: Integer;
  i, j: Integer;
  Found: Boolean;
begin
  EnableCORS(AResponse);

  ProductID := StrToIntDef(ARequest.RouteParams['id'], 0);
  Found := False;

  for i := 0 to High(Products) do
  begin
    if Products[i].ID = ProductID then
    begin
      Products[i].Free;

      for j := i to High(Products) - 1 do
        Products[j] := Products[j + 1];

      SetLength(Products, Length(Products) - 1);

      AResponse.Code := 204; // No Content
      AResponse.SendResponse;
      Found := True;
      Break;
    end;
  end;

  if not Found then
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Product not found"}';
    AResponse.SendResponse;
  end;
end;

// OPTIONS - Pour les requêtes CORS preflight
procedure HandleOptions(ARequest: TRequest; AResponse: TResponse);
begin
  EnableCORS(AResponse);
  AResponse.Code := 204;
  AResponse.SendResponse;
end;

begin
  InitializeData;

  // Enregistrer les routes
  HTTPRouter.RegisterRoute('/api/products', @GetProducts);
  HTTPRouter.RegisterRoute('/api/products', rmPost, @CreateProduct);
  HTTPRouter.RegisterRoute('/api/products/:id', @GetProduct);
  HTTPRouter.RegisterRoute('/api/products/:id', rmPut, @UpdateProduct);
  HTTPRouter.RegisterRoute('/api/products/:id', rmDelete, @DeleteProduct);

  // Gérer OPTIONS pour CORS
  HTTPRouter.RegisterRoute('/api/products', rmOptions, @HandleOptions);
  HTTPRouter.RegisterRoute('/api/products/:id', rmOptions, @HandleOptions);

  Application.Port := 8080;
  Application.Title := 'Products API Server';

  WriteLn('API Server démarré sur http://localhost:8080');
  WriteLn('Endpoints:');
  WriteLn('  GET    /api/products     - Liste des produits');
  WriteLn('  GET    /api/products/:id - Produit par ID');
  WriteLn('  POST   /api/products     - Créer un produit');
  WriteLn('  PUT    /api/products/:id - Mettre à jour un produit');
  WriteLn('  DELETE /api/products/:id - Supprimer un produit');
  WriteLn('');

  Application.Initialize;
  Application.Run;
end.
```

**Compilation et exécution :**

```bash
# Compiler
fpc api_server.pas

# Lancer le serveur
./api_server

# Ou sur Windows
api_server.exe
```

## 9.10.2 Intégration avec React

### Application React utilisant l'API FreePascal

**src/api/productsApi.js :**

```javascript
const API_BASE_URL = 'http://localhost:8080/api';

class ProductsAPI {
    // Récupérer tous les produits
    static async getAll() {
        try {
            const response = await fetch(`${API_BASE_URL}/products`);
            if (!response.ok) {
                throw new Error('Erreur lors de la récupération des produits');
            }
            return await response.json();
        } catch (error) {
            console.error('Erreur API:', error);
            throw error;
        }
    }

    // Récupérer un produit par ID
    static async getById(id) {
        try {
            const response = await fetch(`${API_BASE_URL}/products/${id}`);
            if (!response.ok) {
                throw new Error('Produit non trouvé');
            }
            return await response.json();
        } catch (error) {
            console.error('Erreur API:', error);
            throw error;
        }
    }

    // Créer un nouveau produit
    static async create(product) {
        try {
            const response = await fetch(`${API_BASE_URL}/products`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(product)
            });

            if (!response.ok) {
                throw new Error('Erreur lors de la création du produit');
            }

            return await response.json();
        } catch (error) {
            console.error('Erreur API:', error);
            throw error;
        }
    }

    // Mettre à jour un produit
    static async update(id, product) {
        try {
            const response = await fetch(`${API_BASE_URL}/products/${id}`, {
                method: 'PUT',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(product)
            });

            if (!response.ok) {
                throw new Error('Erreur lors de la mise à jour');
            }

            return await response.json();
        } catch (error) {
            console.error('Erreur API:', error);
            throw error;
        }
    }

    // Supprimer un produit
    static async delete(id) {
        try {
            const response = await fetch(`${API_BASE_URL}/products/${id}`, {
                method: 'DELETE'
            });

            if (!response.ok && response.status !== 204) {
                throw new Error('Erreur lors de la suppression');
            }

            return true;
        } catch (error) {
            console.error('Erreur API:', error);
            throw error;
        }
    }
}

export default ProductsAPI;
```

**src/components/ProductList.jsx :**

```jsx
import React, { useState, useEffect } from 'react';
import ProductsAPI from '../api/productsApi';
import './ProductList.css';

function ProductList() {
    const [products, setProducts] = useState([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState(null);
    const [editingProduct, setEditingProduct] = useState(null);

    // Charger les produits au montage du composant
    useEffect(() => {
        loadProducts();
    }, []);

    const loadProducts = async () => {
        try {
            setLoading(true);
            setError(null);
            const data = await ProductsAPI.getAll();
            setProducts(data);
        } catch (err) {
            setError(err.message);
        } finally {
            setLoading(false);
        }
    };

    const handleDelete = async (id) => {
        if (!window.confirm('Êtes-vous sûr de vouloir supprimer ce produit ?')) {
            return;
        }

        try {
            await ProductsAPI.delete(id);
            await loadProducts(); // Recharger la liste
        } catch (err) {
            alert('Erreur lors de la suppression: ' + err.message);
        }
    };

    const handleEdit = (product) => {
        setEditingProduct(product);
    };

    const handleSave = async (product) => {
        try {
            await ProductsAPI.update(product.id, product);
            setEditingProduct(null);
            await loadProducts();
        } catch (err) {
            alert('Erreur lors de la mise à jour: ' + err.message);
        }
    };

    const handleCancel = () => {
        setEditingProduct(null);
    };

    if (loading) {
        return <div className="loading">Chargement des produits...</div>;
    }

    if (error) {
        return (
            <div className="error">
                <p>Erreur: {error}</p>
                <button onClick={loadProducts}>Réessayer</button>
            </div>
        );
    }

    return (
        <div className="product-list">
            <h2>Liste des Produits</h2>

            <button className="refresh-btn" onClick={loadProducts}>
                🔄 Actualiser
            </button>

            <table>
                <thead>
                    <tr>
                        <th>ID</th>
                        <th>Nom</th>
                        <th>Prix</th>
                        <th>Stock</th>
                        <th>Actions</th>
                    </tr>
                </thead>
                <tbody>
                    {products.map(product => (
                        <tr key={product.id}>
                            {editingProduct?.id === product.id ? (
                                <EditRow
                                    product={editingProduct}
                                    onSave={handleSave}
                                    onCancel={handleCancel}
                                    onChange={setEditingProduct}
                                />
                            ) : (
                                <>
                                    <td>{product.id}</td>
                                    <td>{product.name}</td>
                                    <td>{product.price.toFixed(2)} €</td>
                                    <td>{product.stock}</td>
                                    <td>
                                        <button
                                            className="edit-btn"
                                            onClick={() => handleEdit(product)}
                                        >
                                            ✏️ Modifier
                                        </button>
                                        <button
                                            className="delete-btn"
                                            onClick={() => handleDelete(product.id)}
                                        >
                                            🗑️ Supprimer
                                        </button>
                                    </td>
                                </>
                            )}
                        </tr>
                    ))}
                </tbody>
            </table>

            {products.length === 0 && (
                <p className="empty-message">Aucun produit disponible</p>
            )}
        </div>
    );
}

function EditRow({ product, onSave, onCancel, onChange }) {
    return (
        <>
            <td>{product.id}</td>
            <td>
                <input
                    type="text"
                    value={product.name}
                    onChange={(e) => onChange({...product, name: e.target.value})}
                />
            </td>
            <td>
                <input
                    type="number"
                    step="0.01"
                    value={product.price}
                    onChange={(e) => onChange({...product, price: parseFloat(e.target.value)})}
                />
            </td>
            <td>
                <input
                    type="number"
                    value={product.stock}
                    onChange={(e) => onChange({...product, stock: parseInt(e.target.value)})}
                />
            </td>
            <td>
                <button className="save-btn" onClick={() => onSave(product)}>
                    💾 Enregistrer
                </button>
                <button className="cancel-btn" onClick={onCancel}>
                    ❌ Annuler
                </button>
            </td>
        </>
    );
}

export default ProductList;
```

**src/components/ProductList.css :**

```css
.product-list {
    max-width: 1200px;
    margin: 20px auto;
    padding: 20px;
}

.product-list h2 {
    color: #333;
    margin-bottom: 20px;
}

.refresh-btn {
    background: #007bff;
    color: white;
    border: none;
    padding: 10px 20px;
    border-radius: 5px;
    cursor: pointer;
    margin-bottom: 20px;
}

.refresh-btn:hover {
    background: #0056b3;
}

table {
    width: 100%;
    border-collapse: collapse;
    background: white;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

thead {
    background: #f8f9fa;
}

th, td {
    padding: 12px;
    text-align: left;
    border-bottom: 1px solid #dee2e6;
}

th {
    font-weight: bold;
    color: #495057;
}

tr:hover {
    background: #f8f9fa;
}

button {
    margin: 0 5px;
    padding: 6px 12px;
    border: none;
    border-radius: 3px;
    cursor: pointer;
    font-size: 14px;
}

.edit-btn {
    background: #28a745;
    color: white;
}

.edit-btn:hover {
    background: #218838;
}

.delete-btn {
    background: #dc3545;
    color: white;
}

.delete-btn:hover {
    background: #c82333;
}

.save-btn {
    background: #007bff;
    color: white;
}

.cancel-btn {
    background: #6c757d;
    color: white;
}

input {
    padding: 6px;
    border: 1px solid #ced4da;
    border-radius: 3px;
    width: 100%;
}

.loading, .error {
    text-align: center;
    padding: 40px;
    font-size: 18px;
}

.error {
    color: #dc3545;
}

.empty-message {
    text-align: center;
    padding: 40px;
    color: #6c757d;
}
```

**src/App.jsx :**

```jsx
import React from 'react';
import ProductList from './components/ProductList';
import './App.css';

function App() {
    return (
        <div className="App">
            <header className="App-header">
                <h1>🛒 Gestion de Produits</h1>
                <p>Frontend React + Backend FreePascal</p>
            </header>

            <main>
                <ProductList />
            </main>

            <footer>
                <p>API FreePascal - Interface React</p>
            </footer>
        </div>
    );
}

export default App;
```

## 9.10.3 Intégration avec Vue.js

### Composant Vue.js avec API FreePascal

**src/services/ProductService.js :**

```javascript
import axios from 'axios';

const API_URL = 'http://localhost:8080/api';

export default {
    // Récupérer tous les produits
    async getAllProducts() {
        try {
            const response = await axios.get(`${API_URL}/products`);
            return response.data;
        } catch (error) {
            console.error('Erreur lors de la récupération des produits:', error);
            throw error;
        }
    },

    // Récupérer un produit
    async getProduct(id) {
        try {
            const response = await axios.get(`${API_URL}/products/${id}`);
            return response.data;
        } catch (error) {
            console.error('Erreur lors de la récupération du produit:', error);
            throw error;
        }
    },

    // Créer un produit
    async createProduct(product) {
        try {
            const response = await axios.post(`${API_URL}/products`, product);
            return response.data;
        } catch (error) {
            console.error('Erreur lors de la création du produit:', error);
            throw error;
        }
    },

    // Mettre à jour un produit
    async updateProduct(id, product) {
        try {
            const response = await axios.put(`${API_URL}/products/${id}`, product);
            return response.data;
        } catch (error) {
            console.error('Erreur lors de la mise à jour:', error);
            throw error;
        }
    },

    // Supprimer un produit
    async deleteProduct(id) {
        try {
            await axios.delete(`${API_URL}/products/${id}`);
            return true;
        } catch (error) {
            console.error('Erreur lors de la suppression:', error);
            throw error;
        }
    }
};
```

**src/components/ProductManager.vue :**

```vue
<template>
    <div class="product-manager">
        <h2>Gestionnaire de Produits</h2>

        <!-- Formulaire d'ajout -->
        <div class="add-form">
            <h3>Ajouter un produit</h3>
            <form @submit.prevent="addProduct">
                <input
                    v-model="newProduct.name"
                    placeholder="Nom du produit"
                    required
                >
                <input
                    v-model.number="newProduct.price"
                    type="number"
                    step="0.01"
                    placeholder="Prix"
                    required
                >
                <input
                    v-model.number="newProduct.stock"
                    type="number"
                    placeholder="Stock"
                    required
                >
                <button type="submit">➕ Ajouter</button>
            </form>
        </div>

        <!-- Liste des produits -->
        <div v-if="loading" class="loading">
            Chargement...
        </div>

        <div v-else-if="error" class="error">
            {{ error }}
            <button @click="loadProducts">Réessayer</button>
        </div>

        <div v-else class="products-grid">
            <div
                v-for="product in products"
                :key="product.id"
                class="product-card"
            >
                <div v-if="editingId === product.id">
                    <input v-model="editingProduct.name">
                    <input v-model.number="editingProduct.price" type="number" step="0.01">
                    <input v-model.number="editingProduct.stock" type="number">
                    <button @click="saveEdit">💾</button>
                    <button @click="cancelEdit">❌</button>
                </div>

                <div v-else>
                    <h3>{{ product.name }}</h3>
                    <p class="price">{{ product.price.toFixed(2) }} €</p>
                    <p class="stock">
                        Stock: {{ product.stock }}
                        <span :class="stockClass(product.stock)">
                            {{ stockStatus(product.stock) }}
                        </span>
                    </p>
                    <div class="actions">
                        <button @click="startEdit(product)" class="edit-btn">
                            ✏️ Modifier
                        </button>
                        <button @click="deleteProduct(product.id)" class="delete-btn">
                            🗑️ Supprimer
                        </button>
                    </div>
                </div>
            </div>
        </div>

        <div v-if="!loading && products.length === 0" class="empty">
            Aucun produit. Ajoutez-en un !
        </div>
    </div>
</template>

<script>
import ProductService from '../services/ProductService';

export default {
    name: 'ProductManager',

    data() {
        return {
            products: [],
            loading: false,
            error: null,
            newProduct: {
                name: '',
                price: 0,
                stock: 0
            },
            editingId: null,
            editingProduct: null
        };
    },

    mounted() {
        this.loadProducts();
    },

    methods: {
        async loadProducts() {
            this.loading = true;
            this.error = null;

            try {
                this.products = await ProductService.getAllProducts();
            } catch (error) {
                this.error = 'Erreur lors du chargement des produits';
            } finally {
                this.loading = false;
            }
        },

        async addProduct() {
            try {
                await ProductService.createProduct(this.newProduct);

                // Réinitialiser le formulaire
                this.newProduct = {
                    name: '',
                    price: 0,
                    stock: 0
                };

                // Recharger la liste
                await this.loadProducts();
            } catch (error) {
                alert('Erreur lors de l\'ajout du produit');
            }
        },

        startEdit(product) {
            this.editingId = product.id;
            this.editingProduct = { ...product };
        },

        async saveEdit() {
            try {
                await ProductService.updateProduct(
                    this.editingId,
                    this.editingProduct
                );
                this.editingId = null;
                this.editingProduct = null;
                await this.loadProducts();
            } catch (error) {
                alert('Erreur lors de la mise à jour');
            }
        },

        cancelEdit() {
            this.editingId = null;
            this.editingProduct = null;
        },

        async deleteProduct(id) {
            if (!confirm('Supprimer ce produit ?')) {
                return;
            }

            try {
                await ProductService.deleteProduct(id);
                await this.loadProducts();
            } catch (error) {
                alert('Erreur lors de la suppression');
            }
        },

        stockStatus(stock) {
            if (stock === 0) return '⚠️ Rupture';
            if (stock < 10) return '⚠️ Faible';
            return '✓ Disponible';
        },

        stockClass(stock) {
            if (stock === 0) return 'out-of-stock';
            if (stock < 10) return 'low-stock';
            return 'in-stock';
        }
    }
};
</script>

<style scoped>
.product-manager {
    max-width: 1200px;
    margin: 0 auto;
    padding: 20px;
}

.add-form {
    background: #f8f9fa;
    padding: 20px;
    border-radius: 8px;
    margin-bottom: 30px;
}

.add-form h3 {
    margin-top: 0;
}

.add-form form {
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
}

.add-form input {
    flex: 1;
    min-width: 150px;
    padding: 10px;
    border: 1px solid #ced4da;
    border-radius: 4px;
}

.add-form button {
    padding: 10px 20px;
    background: #28a745;
    color: white;
    border: none;
    border-radius: 4px;
    cursor: pointer;
}

.add-form button:hover {
    background: #218838;
}

.products-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
    gap: 20px;
}

.product-card {
    background: white;
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    transition: transform 0.2s;
}

.product-card:hover {
    transform: translateY(-5px);
    box-shadow: 0 4px 8px rgba(0,0,0,0.15);
}

.product-card h3 {
    margin: 0 0 10px 0;
    color: #333;
}

.price {
    font-size: 24px;
    font-weight: bold;
    color: #28a745;
    margin: 10px 0;
}

.stock {
    margin: 10px 0;
    color: #6c757d;
}

.in-stock { color: #28a745; }
.low-stock { color: #ffc107; }
.out-of-stock { color: #dc3545; }

.actions {
    display: flex;
    gap: 10px;
    margin-top: 15px;
}

.actions button {
    flex: 1;
    padding: 8px;
    border: none;
    border-radius: 4px;
    cursor: pointer;
    font-size: 14px;
}

.edit-btn {
    background: #007bff;
    color: white;
}

.delete-btn {
    background: #dc3545;
    color: white;
}

.loading, .error, .empty {
    text-align: center;
    padding: 40px;
    font-size: 18px;
}

.error {
    color: #dc3545;
}
</style>
```

## 9.10.4 Intégration avec Angular

### Service Angular pour l'API FreePascal

**src/app/services/product.service.ts :**

```typescript
import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { catchError, retry } from 'rxjs/operators';

export interface Product {
  id: number;
  name: string;
  price: number;
  stock: number;
}

@Injectable({
  providedIn: 'root'
})
export class ProductService {
  private apiUrl = 'http://localhost:8080/api/products';

  private httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json'
    })
  };

  constructor(private http: HttpClient) { }

  // Récupérer tous les produits
  getProducts(): Observable<Product[]> {
    return this.http.get<Product[]>(this.apiUrl)
      .pipe(
        retry(2),
        catchError(this.handleError)
      );
  }

  // Récupérer un produit par ID
  getProduct(id: number): Observable<Product> {
    return this.http.get<Product>(`${this.apiUrl}/${id}`)
      .pipe(
        catchError(this.handleError)
      );
  }

  // Créer un produit
  createProduct(product: Omit<Product, 'id'>): Observable<Product> {
    return this.http.post<Product>(this.apiUrl, product, this.httpOptions)
      .pipe(
        catchError(this.handleError)
      );
  }

  // Mettre à jour un produit
  updateProduct(id: number, product: Partial<Product>): Observable<Product> {
    return this.http.put<Product>(`${this.apiUrl}/${id}`, product, this.httpOptions)
      .pipe(
        catchError(this.handleError)
      );
  }

  // Supprimer un produit
  deleteProduct(id: number): Observable<void> {
    return this.http.delete<void>(`${this.apiUrl}/${id}`)
      .pipe(
        catchError(this.handleError)
      );
  }

  // Gestion des erreurs
  private handleError(error: any) {
    let errorMessage = 'Une erreur est survenue';

    if (error.error instanceof ErrorEvent) {
      // Erreur côté client
      errorMessage = `Erreur: ${error.error.message}`;
    } else {
      // Erreur côté serveur
      errorMessage = `Code: ${error.status}\nMessage: ${error.message}`;
    }

    console.error(errorMessage);
    return throwError(() => new Error(errorMessage));
  }
}
```

**src/app/components/product-list/product-list.component.ts :**

```typescript
import { Component, OnInit } from '@angular/core';
import { ProductService, Product } from '../../services/product.service';

@Component({
  selector: 'app-product-list',
  templateUrl: './product-list.component.html',
  styleUrls: ['./product-list.component.css']
})
export class ProductListComponent implements OnInit {
  products: Product[] = [];
  loading = false;
  error: string | null = null;

  newProduct = {
    name: '',
    price: 0,
    stock: 0
  };

  editingProduct: Product | null = null;

  constructor(private productService: ProductService) { }

  ngOnInit(): void {
    this.loadProducts();
  }

  loadProducts(): void {
    this.loading = true;
    this.error = null;

    this.productService.getProducts().subscribe({
      next: (data) => {
        this.products = data;
        this.loading = false;
      },
      error: (error) => {
        this.error = error.message;
        this.loading = false;
      }
    });
  }

  addProduct(): void {
    if (!this.newProduct.name || this.newProduct.price <= 0) {
      alert('Veuillez remplir tous les champs correctement');
      return;
    }

    this.productService.createProduct(this.newProduct).subscribe({
      next: () => {
        this.newProduct = { name: '', price: 0, stock: 0 };
        this.loadProducts();
      },
      error: (error) => {
        alert('Erreur lors de l\'ajout: ' + error.message);
      }
    });
  }

  startEdit(product: Product): void {
    this.editingProduct = { ...product };
  }

  saveEdit(): void {
    if (!this.editingProduct) return;

    this.productService.updateProduct(this.editingProduct.id, this.editingProduct).subscribe({
      next: () => {
        this.editingProduct = null;
        this.loadProducts();
      },
      error: (error) => {
        alert('Erreur lors de la mise à jour: ' + error.message);
      }
    });
  }

  cancelEdit(): void {
    this.editingProduct = null;
  }

  deleteProduct(id: number): void {
    if (!confirm('Êtes-vous sûr de vouloir supprimer ce produit ?')) {
      return;
    }

    this.productService.deleteProduct(id).subscribe({
      next: () => {
        this.loadProducts();
      },
      error: (error) => {
        alert('Erreur lors de la suppression: ' + error.message);
      }
    });
  }

  isEditing(product: Product): boolean {
    return this.editingProduct?.id === product.id;
  }
}
```

**src/app/components/product-list/product-list.component.html :**

```html
<div class="product-list-container">
  <h2>Gestion des Produits</h2>

  <!-- Formulaire d'ajout -->
  <div class="add-product-form">
    <h3>Ajouter un nouveau produit</h3>
    <form (ngSubmit)="addProduct()">
      <div class="form-group">
        <input
          [(ngModel)]="newProduct.name"
          name="name"
          placeholder="Nom du produit"
          required
        >
      </div>
      <div class="form-group">
        <input
          [(ngModel)]="newProduct.price"
          name="price"
          type="number"
          step="0.01"
          placeholder="Prix"
          required
        >
      </div>
      <div class="form-group">
        <input
          [(ngModel)]="newProduct.stock"
          name="stock"
          type="number"
          placeholder="Stock"
          required
        >
      </div>
      <button type="submit" class="btn btn-primary">➕ Ajouter</button>
    </form>
  </div>

  <!-- État de chargement -->
  <div *ngIf="loading" class="loading">
    <div class="spinner"></div>
    Chargement des produits...
  </div>

  <!-- Message d'erreur -->
  <div *ngIf="error" class="error-message">
    <p>{{ error }}</p>
    <button (click)="loadProducts()" class="btn btn-secondary">Réessayer</button>
  </div>

  <!-- Liste des produits -->
  <div *ngIf="!loading && !error" class="products-table">
    <table>
      <thead>
        <tr>
          <th>ID</th>
          <th>Nom</th>
          <th>Prix</th>
          <th>Stock</th>
          <th>Actions</th>
        </tr>
      </thead>
      <tbody>
        <tr *ngFor="let product of products">
          <ng-container *ngIf="!isEditing(product)">
            <td>{{ product.id }}</td>
            <td>{{ product.name }}</td>
            <td>{{ product.price | currency:'EUR' }}</td>
            <td>
              <span [class]="'stock-badge ' + (product.stock === 0 ? 'out' : product.stock < 10 ? 'low' : 'ok')">
                {{ product.stock }}
              </span>
            </td>
            <td>
              <button (click)="startEdit(product)" class="btn btn-edit">
                ✏️ Modifier
              </button>
              <button (click)="deleteProduct(product.id)" class="btn btn-delete">
                🗑️ Supprimer
              </button>
            </td>
          </ng-container>

          <ng-container *ngIf="isEditing(product)">
            <td>{{ product.id }}</td>
            <td>
              <input [(ngModel)]="editingProduct!.name" class="edit-input">
            </td>
            <td>
              <input [(ngModel)]="editingProduct!.price" type="number" step="0.01" class="edit-input">
            </td>
            <td>
              <input [(ngModel)]="editingProduct!.stock" type="number" class="edit-input">
            </td>
            <td>
              <button (click)="saveEdit()" class="btn btn-save">💾 Sauver</button>
              <button (click)="cancelEdit()" class="btn btn-cancel">❌ Annuler</button>
            </td>
          </ng-container>
        </tr>
      </tbody>
    </table>

    <p *ngIf="products.length === 0" class="empty-message">
      Aucun produit disponible. Ajoutez-en un !
    </p>
  </div>
</div>
```

## 9.10.5 Intégration avec Node.js/Express

### Proxy Node.js vers API FreePascal

Parfois, il est utile de créer un proxy Node.js qui fait le lien entre le frontend et le backend FreePascal, notamment pour gérer l'authentification ou agréger plusieurs APIs.

**server.js (Node.js/Express) :**

```javascript
const express = require('express');
const cors = require('cors');
const axios = require('axios');

const app = express();
const PORT = 3000;
const PASCAL_API = 'http://localhost:8080/api';

// Middleware
app.use(cors());
app.use(express.json());
app.use(express.static('public'));

// Logger middleware
app.use((req, res, next) => {
    console.log(`${new Date().toISOString()} - ${req.method} ${req.path}`);
    next();
});

// Route proxy pour les produits
app.get('/api/products', async (req, res) => {
    try {
        const response = await axios.get(`${PASCAL_API}/products`);
        res.json(response.data);
    } catch (error) {
        console.error('Erreur lors de la récupération des produits:', error.message);
        res.status(500).json({
            error: 'Erreur de communication avec le serveur Pascal'
        });
    }
});

app.get('/api/products/:id', async (req, res) => {
    try {
        const response = await axios.get(`${PASCAL_API}/products/${req.params.id}`);
        res.json(response.data);
    } catch (error) {
        if (error.response && error.response.status === 404) {
            res.status(404).json({ error: 'Produit non trouvé' });
        } else {
            res.status(500).json({ error: 'Erreur serveur' });
        }
    }
});

app.post('/api/products', async (req, res) => {
    try {
        const response = await axios.post(`${PASCAL_API}/products`, req.body);
        res.status(201).json(response.data);
    } catch (error) {
        res.status(400).json({ error: 'Données invalides' });
    }
});

app.put('/api/products/:id', async (req, res) => {
    try {
        const response = await axios.put(
            `${PASCAL_API}/products/${req.params.id}`,
            req.body
        );
        res.json(response.data);
    } catch (error) {
        res.status(400).json({ error: 'Mise à jour échouée' });
    }
});

app.delete('/api/products/:id', async (req, res) => {
    try {
        await axios.delete(`${PASCAL_API}/products/${req.params.id}`);
        res.status(204).send();
    } catch (error) {
        res.status(500).json({ error: 'Suppression échouée' });
    }
});

// Route de santé
app.get('/health', (req, res) => {
    res.json({
        status: 'ok',
        timestamp: new Date().toISOString(),
        backend: 'FreePascal API Proxy'
    });
});

// Gestion des erreurs 404
app.use((req, res) => {
    res.status(404).json({ error: 'Route non trouvée' });
});

// Démarrage du serveur
app.listen(PORT, () => {
    console.log(`Serveur proxy démarré sur http://localhost:${PORT}`);
    console.log(`Backend FreePascal : ${PASCAL_API}`);
});
```

**package.json :**

```json
{
  "name": "pascal-api-proxy",
  "version": "1.0.0",
  "description": "Proxy Node.js vers API FreePascal",
  "main": "server.js",
  "scripts": {
    "start": "node server.js",
    "dev": "nodemon server.js"
  },
  "dependencies": {
    "express": "^4.18.2",
    "cors": "^2.8.5",
    "axios": "^1.4.0"
  },
  "devDependencies": {
    "nodemon": "^3.0.1"
  }
}
```

## 9.10.6 WebSockets pour la communication temps réel

### Serveur WebSocket en FreePascal

**websocket_server.pas :**

```pascal
program WebSocketServer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpapp, httpdefs, fpwebsocket, fpjson;

type
  TProductUpdateMessage = record
    Action: string;
    ProductID: Integer;
    ProductName: string;
    Timestamp: TDateTime;
  end;

var
  ConnectedClients: TList;

// Diffuser un message à tous les clients connectés
procedure BroadcastMessage(const Message: string);
var
  i: Integer;
  Client: TWebSocketConnection;
begin
  for i := 0 to ConnectedClients.Count - 1 do
  begin
    Client := TWebSocketConnection(ConnectedClients[i]);
    try
      Client.Send(Message);
    except
      on E: Exception do
        WriteLn('Erreur lors de l''envoi au client ', i, ': ', E.Message);
    end;
  end;
end;

// Gestionnaire de connexion WebSocket
procedure HandleWebSocketConnect(Sender: TObject; AConnection: TWebSocketConnection);
begin
  WriteLn('Nouveau client connecté');
  ConnectedClients.Add(AConnection);

  // Envoyer un message de bienvenue
  AConnection.Send('{"type":"connected","message":"Bienvenue sur le serveur WebSocket"}');
end;

// Gestionnaire de déconnexion
procedure HandleWebSocketDisconnect(Sender: TObject; AConnection: TWebSocketConnection);
begin
  WriteLn('Client déconnecté');
  ConnectedClients.Remove(AConnection);
end;

// Gestionnaire de message reçu
procedure HandleWebSocketMessage(Sender: TObject; AConnection: TWebSocketConnection;
                                const AMessage: string);
var
  JSONMsg: TJSONObject;
  MessageType: string;
  Response: TJSONObject;
begin
  WriteLn('Message reçu: ', AMessage);

  try
    JSONMsg := GetJSON(AMessage) as TJSONObject;
    try
      MessageType := JSONMsg.Get('type', '');

      if MessageType = 'ping' then
      begin
        Response := TJSONObject.Create;
        try
          Response.Add('type', 'pong');
          Response.Add('timestamp', DateTimeToStr(Now));
          AConnection.Send(Response.AsJSON);
        finally
          Response.Free;
        end;
      end
      else if MessageType = 'subscribe' then
      begin
        WriteLn('Client abonné aux mises à jour');
        AConnection.Send('{"type":"subscribed","message":"Abonné aux notifications"}');
      end;
    finally
      JSONMsg.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur lors du traitement du message: ', E.Message);
  end;
end;

// Notifier les clients d'une mise à jour de produit
procedure NotifyProductUpdate(const Action: string; ProductID: Integer;
                              const ProductName: string);
var
  Notification: TJSONObject;
begin
  Notification := TJSONObject.Create;
  try
    Notification.Add('type', 'product_update');
    Notification.Add('action', Action);
    Notification.Add('product_id', ProductID);
    Notification.Add('product_name', ProductName);
    Notification.Add('timestamp', DateTimeToStr(Now));

    BroadcastMessage(Notification.AsJSON);
    WriteLn('Notification envoyée: ', Action, ' - ', ProductName);
  finally
    Notification.Free;
  end;
end;

begin
  ConnectedClients := TList.Create;

  try
    // Configuration du serveur WebSocket
    Application.Port := 8081;
    Application.Title := 'WebSocket Server';

    WriteLn('Serveur WebSocket démarré sur ws://localhost:8081');
    WriteLn('Appuyez sur Ctrl+C pour arrêter');

    // Note: Configuration WebSocket simplifiée
    // En production, utiliser une bibliothèque WebSocket complète

    Application.Initialize;
    Application.Run;
  finally
    ConnectedClients.Free;
  end;
end.
```

### Client JavaScript WebSocket

```javascript
class ProductWebSocketClient {
    constructor(url) {
        this.url = url;
        this.ws = null;
        this.reconnectInterval = 5000;
        this.listeners = new Map();
    }

    connect() {
        console.log('Connexion au serveur WebSocket...');
        this.ws = new WebSocket(this.url);

        this.ws.onopen = (event) => {
            console.log('Connecté au serveur WebSocket');
            this.emit('connected', event);

            // S'abonner aux mises à jour
            this.send({
                type: 'subscribe',
                topics: ['products']
            });
        };

        this.ws.onmessage = (event) => {
            try {
                const message = JSON.parse(event.data);
                console.log('Message reçu:', message);

                // Émettre l'événement selon le type
                this.emit(message.type, message);
            } catch (error) {
                console.error('Erreur lors du parsing du message:', error);
            }
        };

        this.ws.onerror = (error) => {
            console.error('Erreur WebSocket:', error);
            this.emit('error', error);
        };

        this.ws.onclose = (event) => {
            console.log('Déconnecté du serveur WebSocket');
            this.emit('disconnected', event);

            // Tentative de reconnexion automatique
            setTimeout(() => {
                console.log('Tentative de reconnexion...');
                this.connect();
            }, this.reconnectInterval);
        };
    }

    send(data) {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify(data));
        } else {
            console.warn('WebSocket non connecté');
        }
    }

    on(event, callback) {
        if (!this.listeners.has(event)) {
            this.listeners.set(event, []);
        }
        this.listeners.get(event).push(callback);
    }

    off(event, callback) {
        if (this.listeners.has(event)) {
            const callbacks = this.listeners.get(event);
            const index = callbacks.indexOf(callback);
            if (index > -1) {
                callbacks.splice(index, 1);
            }
        }
    }

    emit(event, data) {
        if (this.listeners.has(event)) {
            this.listeners.get(event).forEach(callback => {
                callback(data);
            });
        }
    }

    disconnect() {
        if (this.ws) {
            this.ws.close();
        }
    }
}

// Utilisation dans une application React
function ProductListWithWebSocket() {
    const [products, setProducts] = React.useState([]);
    const [notifications, setNotifications] = React.useState([]);
    const wsClient = React.useRef(null);

    React.useEffect(() => {
        // Initialiser le client WebSocket
        wsClient.current = new ProductWebSocketClient('ws://localhost:8081');

        // Gérer les mises à jour de produits
        wsClient.current.on('product_update', (message) => {
            console.log('Mise à jour produit:', message);

            // Ajouter une notification
            setNotifications(prev => [...prev, {
                id: Date.now(),
                message: `${message.action}: ${message.product_name}`,
                timestamp: message.timestamp
            }]);

            // Recharger la liste des produits
            loadProducts();
        });

        // Connexion
        wsClient.current.connect();

        // Nettoyage à la désinscription
        return () => {
            if (wsClient.current) {
                wsClient.current.disconnect();
            }
        };
    }, []);

    const loadProducts = async () => {
        // Charger les produits depuis l'API REST
        const response = await fetch('http://localhost:8080/api/products');
        const data = await response.json();
        setProducts(data);
    };

    return (
        <div>
            <h2>Produits (Temps Réel)</h2>

            {/* Notifications */}
            <div className="notifications">
                {notifications.map(notif => (
                    <div key={notif.id} className="notification">
                        {notif.message} - {notif.timestamp}
                    </div>
                ))}
            </div>

            {/* Liste des produits */}
            <div className="products">
                {products.map(product => (
                    <div key={product.id} className="product">
                        {product.name} - {product.price}€
                    </div>
                ))}
            </div>
        </div>
    );
}
```

## 9.10.7 Authentification JWT

### Génération de tokens JWT en FreePascal

**jwt_auth.pas :**

```pascal
unit JWTAuth;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpjson, base64, hmac, sha256;

type
  TJWTManager = class
  private
    FSecretKey: string;
    function Base64URLEncode(const Data: string): string;
    function Base64URLDecode(const Data: string): string;
    function HMACSHA256(const Data, Key: string): string;
  public
    constructor Create(const SecretKey: string);
    function CreateToken(const Payload: TJSONObject): string;
    function VerifyToken(const Token: string): TJSONObject;
    function IsTokenValid(const Token: string): Boolean;
  end;

implementation

constructor TJWTManager.Create(const SecretKey: string);
begin
  inherited Create;
  FSecretKey := SecretKey;
end;

function TJWTManager.Base64URLEncode(const Data: string): string;
begin
  Result := EncodeStringBase64(Data);
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

function TJWTManager.Base64URLDecode(const Data: string): string;
var
  Temp: string;
begin
  Temp := Data;
  Temp := StringReplace(Temp, '-', '+', [rfReplaceAll]);
  Temp := StringReplace(Temp, '_', '/', [rfReplaceAll]);

  // Ajouter le padding
  while Length(Temp) mod 4 <> 0 do
    Temp := Temp + '=';

  Result := DecodeStringBase64(Temp);
end;

function TJWTManager.HMACSHA256(const Data, Key: string): string;
// Implémentation simplifiée - utiliser une vraie bibliothèque HMAC en production
begin
  Result := SHA256Print(SHA256String(Key + Data));
end;

function TJWTManager.CreateToken(const Payload: TJSONObject): string;
var
  Header: TJSONObject;
  HeaderStr, PayloadStr, Signature: string;
begin
  // Créer le header
  Header := TJSONObject.Create;
  try
    Header.Add('alg', 'HS256');
    Header.Add('typ', 'JWT');

    HeaderStr := Base64URLEncode(Header.AsJSON);
    PayloadStr := Base64URLEncode(Payload.AsJSON);

    Signature := Base64URLEncode(HMACSHA256(HeaderStr + '.' + PayloadStr, FSecretKey));

    Result := HeaderStr + '.' + PayloadStr + '.' + Signature;
  finally
    Header.Free;
  end;
end;

function TJWTManager.VerifyToken(const Token: string): TJSONObject;
var
  HeaderStr, PayloadStr, SignatureStr: string;
  ExpectedSignature: string;
  PayloadJSON: string;
  DotPos1, DotPos2: Integer;
begin
  Result := nil;

  // Découper le token manuellement (3 parties séparées par '.')
  DotPos1 := Pos('.', Token);
  if DotPos1 = 0 then
    Exit;
  DotPos2 := Pos('.', Token, DotPos1 + 1);
  if DotPos2 = 0 then
    Exit;

  HeaderStr := Copy(Token, 1, DotPos1 - 1);
  PayloadStr := Copy(Token, DotPos1 + 1, DotPos2 - DotPos1 - 1);
  SignatureStr := Copy(Token, DotPos2 + 1, Length(Token) - DotPos2);

  // Vérifier la signature
  ExpectedSignature := Base64URLEncode(HMACSHA256(HeaderStr + '.' + PayloadStr, FSecretKey));

  if SignatureStr <> ExpectedSignature then
    Exit;

  // Décoder le payload
  PayloadJSON := Base64URLDecode(PayloadStr);
  Result := GetJSON(PayloadJSON) as TJSONObject;
end;

function TJWTManager.IsTokenValid(const Token: string): Boolean;
var
  Payload: TJSONObject;
begin
  Result := False;
  Payload := VerifyToken(Token);

  if Assigned(Payload) then
  try
    // Vérifier l'expiration
    if Payload.IndexOfName('exp') >= 0 then
    begin
      Result := DateTimeToUnix(Now) < Payload.Get('exp', Int64(0));
    end
    else
      Result := True;
  finally
    Payload.Free;
  end;
end;

end.
```

### Utilisation dans l'API

```pascal
procedure HandleLogin(ARequest: TRequest; AResponse: TResponse);
var
  InputJSON: TJSONObject;
  Username, Password: string;
  JWTManager: TJWTManager;
  Payload: TJSONObject;
  Token: string;
  Response: TJSONObject;
begin
  EnableCORS(AResponse);

  try
    InputJSON := GetJSON(ARequest.Content) as TJSONObject;
    try
      Username := InputJSON.Get('username', '');
      Password := InputJSON.Get('password', '');

      // Vérifier les identifiants (exemple simple)
      if (Username = 'admin') and (Password = 'password123') then
      begin
        JWTManager := TJWTManager.Create('votre_secret_key_tres_securisee');
        try
          // Créer le payload du token
          Payload := TJSONObject.Create;
          try
            Payload.Add('sub', Username);
            Payload.Add('name', 'Administrateur');
            Payload.Add('role', 'admin');
            Payload.Add('iat', DateTimeToUnix(Now));
            Payload.Add('exp', DateTimeToUnix(Now + 1)); // Expire dans 1 jour

            // Générer le token
            Token := JWTManager.CreateToken(Payload);

            // Créer la réponse
            Response := TJSONObject.Create;
            try
              Response.Add('success', True);
              Response.Add('token', Token);
              Response.Add('username', Username);
              Response.Add('role', 'admin');

              AResponse.ContentType := 'application/json';
              AResponse.Content := Response.AsJSON;
              AResponse.SendResponse;
            finally
              Response.Free;
            end;
          finally
            Payload.Free;
          end;
        finally
          JWTManager.Free;
        end;
      end
      else
      begin
        AResponse.Code := 401;
        AResponse.Content := '{"success":false,"error":"Identifiants invalides"}';
        AResponse.SendResponse;
      end;
    finally
      InputJSON.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 400;
      AResponse.Content := Format('{"success":false,"error":"%s"}', [E.Message]);
      AResponse.SendResponse;
    end;
  end;
end;

// Middleware pour vérifier le token JWT
function VerifyJWTToken(ARequest: TRequest; AResponse: TResponse): Boolean;
var
  AuthHeader: string;
  Token: string;
  JWTManager: TJWTManager;
  Payload: TJSONObject;
begin
  Result := False;

  // Récupérer le header Authorization
  AuthHeader := ARequest.GetCustomHeader('Authorization');

  if (AuthHeader = '') or (Copy(AuthHeader, 1, 7) <> 'Bearer ') then
  begin
    AResponse.Code := 401;
    AResponse.Content := '{"error":"Token manquant"}';
    AResponse.SendResponse;
    Exit;
  end;

  // Extraire le token
  Token := Copy(AuthHeader, 8, Length(AuthHeader) - 7);

  // Vérifier le token
  JWTManager := TJWTManager.Create('votre_secret_key_tres_securisee');
  try
    Payload := JWTManager.VerifyToken(Token);

    if not Assigned(Payload) then
    begin
      AResponse.Code := 401;
      AResponse.Content := '{"error":"Token invalide"}';
      AResponse.SendResponse;
      Exit;
    end;

    Payload.Free;
    Result := True;
  finally
    JWTManager.Free;
  end;
end;

// Endpoint protégé nécessitant l'authentification
procedure HandleProtectedResource(ARequest: TRequest; AResponse: TResponse);
begin
  EnableCORS(AResponse);

  if not VerifyJWTToken(ARequest, AResponse) then
    Exit;

  // L'utilisateur est authentifié, renvoyer les données
  AResponse.ContentType := 'application/json';
  AResponse.Content := '{"message":"Accès autorisé","data":"Données protégées"}';
  AResponse.SendResponse;
end;
```

### Client JavaScript avec JWT

```javascript
// Service d'authentification
class AuthService {
    constructor() {
        this.API_URL = 'http://localhost:8080/api';
        this.token = localStorage.getItem('jwt_token');
    }

    // Connexion
    async login(username, password) {
        try {
            const response = await fetch(`${this.API_URL}/login`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({ username, password })
            });

            const data = await response.json();

            if (data.success) {
                this.token = data.token;
                localStorage.setItem('jwt_token', data.token);
                localStorage.setItem('username', data.username);
                return { success: true, data };
            } else {
                return { success: false, error: data.error };
            }
        } catch (error) {
            return { success: false, error: error.message };
        }
    }

    // Déconnexion
    logout() {
        this.token = null;
        localStorage.removeItem('jwt_token');
        localStorage.removeItem('username');
    }

    // Vérifier si l'utilisateur est connecté
    isAuthenticated() {
        return this.token !== null;
    }

    // Obtenir le token
    getToken() {
        return this.token;
    }

    // Faire une requête authentifiée
    async authenticatedRequest(url, options = {}) {
        if (!this.token) {
            throw new Error('Non authentifié');
        }

        const headers = {
            ...options.headers,
            'Authorization': `Bearer ${this.token}`
        };

        const response = await fetch(url, {
            ...options,
            headers
        });

        if (response.status === 401) {
            // Token expiré ou invalide
            this.logout();
            throw new Error('Session expirée');
        }

        return response;
    }
}

// Composant React de connexion
function LoginForm() {
    const [username, setUsername] = React.useState('');
    const [password, setPassword] = React.useState('');
    const [error, setError] = React.useState('');
    const [loading, setLoading] = React.useState(false);

    const authService = new AuthService();

    const handleSubmit = async (e) => {
        e.preventDefault();
        setError('');
        setLoading(true);

        const result = await authService.login(username, password);

        setLoading(false);

        if (result.success) {
            window.location.href = '/dashboard';
        } else {
            setError(result.error);
        }
    };

    return (
        <div className="login-form">
            <h2>Connexion</h2>

            {error && (
                <div className="error-message">{error}</div>
            )}

            <form onSubmit={handleSubmit}>
                <div className="form-group">
                    <label>Nom d'utilisateur:</label>
                    <input
                        type="text"
                        value={username}
                        onChange={(e) => setUsername(e.target.value)}
                        required
                    />
                </div>

                <div className="form-group">
                    <label>Mot de passe:</label>
                    <input
                        type="password"
                        value={password}
                        onChange={(e) => setPassword(e.target.value)}
                        required
                    />
                </div>

                <button type="submit" disabled={loading}>
                    {loading ? 'Connexion...' : 'Se connecter'}
                </button>
            </form>
        </div>
    );
}

// Hook React pour utiliser l'authentification
function useAuth() {
    const authService = new AuthService();
    const [isAuthenticated, setIsAuthenticated] = React.useState(
        authService.isAuthenticated()
    );

    const login = async (username, password) => {
        const result = await authService.login(username, password);
        if (result.success) {
            setIsAuthenticated(true);
        }
        return result;
    };

    const logout = () => {
        authService.logout();
        setIsAuthenticated(false);
    };

    return { isAuthenticated, login, logout, authService };
}

// Composant protégé
function ProtectedComponent() {
    const { isAuthenticated, authService } = useAuth();
    const [data, setData] = React.useState(null);
    const [error, setError] = React.useState(null);

    React.useEffect(() => {
        if (isAuthenticated) {
            loadProtectedData();
        }
    }, [isAuthenticated]);

    const loadProtectedData = async () => {
        try {
            const response = await authService.authenticatedRequest(
                'http://localhost:8080/api/protected'
            );
            const data = await response.json();
            setData(data);
        } catch (error) {
            setError(error.message);
        }
    };

    if (!isAuthenticated) {
        return <LoginForm />;
    }

    if (error) {
        return <div className="error">{error}</div>;
    }

    return (
        <div>
            <h2>Données protégées</h2>
            {data && <pre>{JSON.stringify(data, null, 2)}</pre>}
        </div>
    );
}
```

## 9.10.8 Server-Sent Events (SSE)

### Implémentation SSE en FreePascal

```pascal
program SSEServer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpapp, httpdefs, httproute;

type
  TSSEConnection = class
  public
    Response: TResponse;
    LastEventID: Integer;
  end;

var
  SSEConnections: TThreadList;

procedure EnableSSEHeaders(AResponse: TResponse);
begin
  AResponse.ContentType := 'text/event-stream';
  AResponse.SetCustomHeader('Cache-Control', 'no-cache');
  AResponse.SetCustomHeader('Connection', 'keep-alive');
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
end;

procedure SendSSEMessage(AResponse: TResponse; const EventType, Data: string;
                        EventID: Integer = -1);
var
  Message: string;
begin
  Message := '';

  if EventID >= 0 then
    Message := Message + Format('id: %d'#10, [EventID]);

  if EventType <> '' then
    Message := Message + Format('event: %s'#10, [EventType]);

  Message := Message + Format('data: %s'#10#10, [Data]);

  AResponse.Content := AResponse.Content + Message;
  AResponse.SendContent;
end;

procedure HandleSSEConnect(ARequest: TRequest; AResponse: TResponse);
var
  Connection: TSSEConnection;
  LastEventID: string;
begin
  EnableSSEHeaders(AResponse);

  Connection := TSSEConnection.Create;
  Connection.Response := AResponse;
  Connection.LastEventID := 0;

  // Récupérer le dernier ID d'événement si présent
  LastEventID := ARequest.GetCustomHeader('Last-Event-ID');
  if LastEventID <> '' then
    Connection.LastEventID := StrToIntDef(LastEventID, 0);

  SSEConnections.Add(Connection);

  // Envoyer un message de connexion
  SendSSEMessage(AResponse, 'connected', '{"message":"Connecté au flux SSE"}', 0);

  WriteLn('Nouveau client SSE connecté');
end;

// Diffuser un événement à tous les clients SSE
procedure BroadcastSSEEvent(const EventType, Data: string);
var
  List: TList;
  i: Integer;
  Connection: TSSEConnection;
begin
  List := SSEConnections.LockList;
  try
    for i := List.Count - 1 downto 0 do
    begin
      Connection := TSSEConnection(List[i]);
      try
        Inc(Connection.LastEventID);
        SendSSEMessage(Connection.Response, EventType, Data, Connection.LastEventID);
      except
        on E: Exception do
        begin
          WriteLn('Erreur lors de l''envoi SSE: ', E.Message);
          // Retirer la connexion défaillante
          Connection.Free;
          List.Delete(i);
        end;
      end;
    end;
  finally
    SSEConnections.UnlockList;
  end;
end;

// Simuler des événements périodiques
procedure SimulateEvents;
var
  Counter: Integer;
  Data: string;
begin
  Counter := 0;

  while True do
  begin
    Sleep(5000); // Attendre 5 secondes

    Inc(Counter);
    Data := Format('{"count":%d,"timestamp":"%s"}', [Counter, DateTimeToStr(Now)]);

    BroadcastSSEEvent('update', Data);
    WriteLn('Événement envoyé: ', Counter);
  end;
end;

begin
  SSEConnections := TThreadList.Create;

  try
    HTTPRouter.RegisterRoute('/events', @HandleSSEConnect);

    Application.Port := 8082;
    Application.Title := 'SSE Server';

    WriteLn('Serveur SSE démarré sur http://localhost:8082/events');

    // Démarrer le thread de simulation d'événements
    BeginThread(@SimulateEvents);

    Application.Initialize;
    Application.Run;
  finally
    SSEConnections.Free;
  end;
end.
```

### Client JavaScript SSE

```javascript
class SSEClient {
    constructor(url) {
        this.url = url;
        this.eventSource = null;
        this.reconnectDelay = 3000;
        this.listeners = new Map();
    }

    connect() {
        console.log('Connexion au serveur SSE...');

        this.eventSource = new EventSource(this.url);

        this.eventSource.onopen = (event) => {
            console.log('Connecté au flux SSE');
            this.emit('connected', event);
        };

        this.eventSource.onerror = (error) => {
            console.error('Erreur SSE:', error);
            this.emit('error', error);

            if (this.eventSource.readyState === EventSource.CLOSED) {
                console.log('Connexion fermée, reconnexion dans', this.reconnectDelay / 1000, 's');
            }
        };

        this.eventSource.onmessage = (event) => {
            try {
                const data = JSON.parse(event.data);
                console.log('Message SSE reçu:', data);
                this.emit('message', data);
            } catch (error) {
                console.error('Erreur de parsing:', error);
            }
        };

        // Événements personnalisés
        this.eventSource.addEventListener('update', (event) => {
            try {
                const data = JSON.parse(event.data);
                console.log('Update reçu:', data);
                this.emit('update', data);
            } catch (error) {
                console.error('Erreur de parsing update:', error);
            }
        });
    }

    on(event, callback) {
        if (!this.listeners.has(event)) {
            this.listeners.set(event, []);
        }
        this.listeners.get(event).push(callback);
    }

    off(event, callback) {
        if (this.listeners.has(event)) {
            const callbacks = this.listeners.get(event);
            const index = callbacks.indexOf(callback);
            if (index > -1) {
                callbacks.splice(index, 1);
            }
        }
    }

    emit(event, data) {
        if (this.listeners.has(event)) {
            this.listeners.get(event).forEach(callback => {
                callback(data);
            });
        }
    }

    disconnect() {
        if (this.eventSource) {
            this.eventSource.close();
            console.log('Déconnecté du flux SSE');
        }
    }
}

// Utilisation dans React
function RealtimeUpdates() {
    const [updates, setUpdates] = React.useState([]);
    const [connected, setConnected] = React.useState(false);
    const sseClient = React.useRef(null);

    React.useEffect(() => {
        // Initialiser le client SSE
        sseClient.current = new SSEClient('http://localhost:8082/events');

        // Écouter les événements
        sseClient.current.on('connected', () => {
            console.log('Flux SSE connecté');
            setConnected(true);
        });

        sseClient.current.on('update', (data) => {
            setUpdates(prev => [...prev, {
                id: Date.now(),
                ...data
            }].slice(-10)); // Garder les 10 dernières mises à jour
        });

        sseClient.current.on('error', () => {
            setConnected(false);
        });

        // Connexion
        sseClient.current.connect();

        // Nettoyage
        return () => {
            if (sseClient.current) {
                sseClient.current.disconnect();
            }
        };
    }, []);

    return (
        <div className="realtime-updates">
            <h2>Mises à jour en temps réel (SSE)</h2>

            <div className={`status ${connected ? 'connected' : 'disconnected'}`}>
                {connected ? '🟢 Connecté' : '🔴 Déconnecté'}
            </div>

            <div className="updates-list">
                {updates.map(update => (
                    <div key={update.id} className="update-item">
                        <strong>Update #{update.count}</strong>
                        <span>{update.timestamp}</span>
                    </div>
                ))}
            </div>

            {updates.length === 0 && (
                <p className="no-updates">En attente de mises à jour...</p>
            )}
        </div>
    );
}
```

## 9.10.9 Déploiement et production

### Configuration multi-plateforme

**build-api.sh (Linux/Ubuntu) :**

```bash
#!/bin/bash

echo "=== Compilation API FreePascal ==="

# Compiler l'API
fpc -O3 api_server.pas

# Vérifier la compilation
if [ $? -eq 0 ]; then
    echo "✓ Compilation réussie"

    # Copier vers le dossier de production
    sudo cp api_server /usr/local/bin/
    sudo chmod +x /usr/local/bin/api_server

    # Créer le service systemd
    sudo tee /etc/systemd/system/pascal-api.service > /dev/null <<EOF
[Unit]
Description=FreePascal API Server
After=network.target

[Service]
Type=simple
User=www-data
WorkingDirectory=/var/www/api
ExecStart=/usr/local/bin/api_server
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
EOF

    # Activer et démarrer le service
    sudo systemctl daemon-reload
    sudo systemctl enable pascal-api
    sudo systemctl start pascal-api

    echo "✓ Service installé et démarré"
else
    echo "✗ Erreur de compilation"
    exit 1
fi
```

**build-api.bat (Windows) :**

```batch
@echo off
echo === Compilation API FreePascal ===

REM Compiler l'API
fpc -O3 api_server.pas

if %ERRORLEVEL% EQU 0 (
    echo Compilation reussie

    REM Copier vers le dossier de production
    copy api_server.exe C:\inetpub\api\

    REM Installer comme service Windows (avec NSSM)
    nssm install PascalAPI "C:\inetpub\api\api_server.exe"
    nssm set PascalAPI AppDirectory "C:\inetpub\api"
    nssm start PascalAPI

    echo Service installe et demarre
) else (
    echo Erreur de compilation
    exit /b 1
)
```

### Configuration Nginx comme reverse proxy

**nginx.conf (Ubuntu) :**

```nginx
# Configuration pour l'API FreePascal
upstream pascal_api {
    server localhost:8080;
    keepalive 32;
}

# Configuration pour le frontend
server {
    listen 80;
    server_name monapp.com;

    # Redirection HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name monapp.com;

    ssl_certificate /etc/letsencrypt/live/monapp.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/monapp.com/privkey.pem;

    # Frontend (React/Vue/Angular)
    root /var/www/html/frontend/build;
    index index.html;

    # Servir les fichiers statiques
    location / {
        try_files $uri $uri/ /index.html;
    }

    # Proxy vers l'API FreePascal
    location /api/ {
        proxy_pass http://pascal_api/api/;
        proxy_http_version 1.1;

        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # WebSocket support
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";

        # Timeouts
        proxy_connect_timeout 60s;
        proxy_send_timeout 60s;
        proxy_read_timeout 60s;
    }

    # Cache des assets statiques
    location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg)$ {
        expires 1y;
        add_header Cache-Control "public, immutable";
    }

    # Compression
    gzip on;
    gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript;
}
```

### Docker Compose pour l'ensemble

**docker-compose.yml :**

```yaml
version: '3.8'

services:
  # API FreePascal
  pascal-api:
    build:
      context: ./backend
      dockerfile: Dockerfile
    ports:
      - "8080:8080"
    environment:
      - DB_HOST=postgres
      - DB_PORT=5432
      - DB_NAME=myapp
      - DB_USER=admin
      - DB_PASSWORD=secret
    depends_on:
      - postgres
    restart: unless-stopped
    networks:
      - app-network

  # Frontend React/Vue/Angular
  frontend:
    build:
      context: ./frontend
      dockerfile: Dockerfile
    ports:
      - "3000:80"
    depends_on:
      - pascal-api
    restart: unless-stopped
    networks:
      - app-network

  # Base de données PostgreSQL
  postgres:
    image: postgres:15
    environment:
      POSTGRES_DB: myapp
      POSTGRES_USER: admin
      POSTGRES_PASSWORD: secret
    volumes:
      - postgres-data:/var/lib/postgresql/data
    networks:
      - app-network

  # Nginx reverse proxy
  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
      - ./ssl:/etc/ssl:ro
    depends_on:
      - pascal-api
      - frontend
    restart: unless-stopped
    networks:
      - app-network

networks:
  app-network:
    driver: bridge

volumes:
  postgres-data:
```

**backend/Dockerfile :**

```dockerfile
FROM debian:bullseye-slim

# Installer FreePascal
RUN apt-get update && \
    apt-get install -y fpc && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copier le code source
COPY . .

# Compiler l'application
RUN fpc -O3 api_server.pas

# Exposer le port
EXPOSE 8080

# Démarrer l'application
CMD ["./api_server"]
```

## 9.10.10 Bonnes pratiques

### Structure de projet recommandée

```
mon-projet/
├── backend/                  # API FreePascal
│   ├── src/
│   │   ├── api_server.pas
│   │   ├── models/
│   │   ├── controllers/
│   │   └── utils/
│   ├── tests/
│   ├── Dockerfile
│   └── README.md
├── frontend/                 # Application JavaScript
│   ├── src/
│   │   ├── components/
│   │   ├── services/
│   │   ├── utils/
│   │   └── App.jsx
│   ├── public/
│   ├── package.json
│   └── Dockerfile
├── nginx/                    # Configuration serveur
│   └── nginx.conf
├── docker-compose.yml
└── README.md
```

### Checklist de sécurité

- ✅ **CORS** : Configurer correctement les origines autorisées
- ✅ **HTTPS** : Utiliser SSL/TLS en production
- ✅ **Validation** : Valider toutes les entrées utilisateur
- ✅ **Authentification** : Implémenter JWT ou sessions sécurisées
- ✅ **Rate limiting** : Limiter le nombre de requêtes par IP
- ✅ **Sanitization** : Échapper les données avant l'insertion en base
- ✅ **Headers de sécurité** : Content-Security-Policy, X-Frame-Options, etc.
- ✅ **Logs** : Journaliser les accès et les erreurs
- ✅ **Mises à jour** : Maintenir FreePascal et les dépendances à jour

### Performance

```pascal
// Cache simple en mémoire (nécessite uses DateUtils pour MinutesBetween)
var
  CachedData: TJSONArray;
  CacheTime: TDateTime;
  CacheDuration: Integer = 5; // 5 minutes

procedure GetProductsCached(ARequest: TRequest; AResponse: TResponse);
begin
  EnableCORS(AResponse);

  // Vérifier le cache
  if Assigned(CachedData) and (MinutesBetween(Now, CacheTime) < CacheDuration) then
  begin
    AResponse.SetCustomHeader('X-Cache', 'HIT');
    AResponse.ContentType := 'application/json';
    AResponse.Content := CachedData.AsJSON;
    AResponse.SendResponse;
    Exit;
  end;

  // Charger depuis la base de données
  CachedData := LoadProductsFromDatabase;
  CacheTime := Now;

  AResponse.SetCustomHeader('X-Cache', 'MISS');
  AResponse.ContentType := 'application/json';
  AResponse.Content := CachedData.AsJSON;
  AResponse.SendResponse;
end;
```

## Conclusion

L'intégration de FreePascal avec les frameworks JavaScript modernes offre le meilleur des deux mondes :

**Avantages :**
- Backend performant et type-safe avec FreePascal
- Frontend moderne et réactif avec React/Vue/Angular
- Architecture scalable et maintenable
- Réutilisation du code existant

**Points clés :**
- API REST bien conçue avec CORS et authentification
- Communication en temps réel via WebSockets ou SSE
- Déploiement facilité avec Docker
- Sécurité renforcée avec JWT et HTTPS

FreePascal se révèle être un excellent choix pour créer des APIs robustes qui alimentent des applications web modernes.

⏭️ [Déploiement sur serveurs Windows/Linux](/09-programmation-web-freepascal/11-deploiement-serveurs-windows-linux.md)
