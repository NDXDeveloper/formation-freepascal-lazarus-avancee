🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.9 GPU computing avec CUDA/OpenCL

## Introduction au Calcul GPU

Le **GPU (Graphics Processing Unit)** est un processeur spécialisé conçu à l'origine pour les graphiques 3D, mais qui s'est révélé extrêmement puissant pour les calculs parallèles en général.

### CPU vs GPU : La différence

**CPU (Processeur Central)**
- Quelques cœurs puissants (4-16 typiquement)
- Excellent pour les tâches séquentielles
- Bonne gestion des branchements complexes
- Latence faible

**GPU (Processeur Graphique)**
- Des milliers de cœurs simples (1000-10000+)
- Excellent pour les calculs parallèles massifs
- Optimisé pour les opérations répétitives
- Débit très élevé

### Analogie simple

Imaginez que vous devez peindre 1000 chaises :

**Approche CPU** : Un peintre très qualifié qui peint les chaises une par une, très vite et avec une grande précision.

**Approche GPU** : 1000 peintres moins qualifiés qui peignent toutes les chaises en même temps. Chacun travaille plus lentement, mais ensemble ils finissent beaucoup plus vite !

### Quand utiliser le GPU ?

✅ **Parfait pour :**
- Traitement d'images (filtres, convolutions)
- Deep Learning (réseaux de neurones)
- Calculs mathématiques vectoriels
- Simulations scientifiques
- Cryptographie (minage, hashing)
- Analyse de données massives

❌ **Moins adapté pour :**
- Algorithmes séquentiels
- Beaucoup de branchements conditionnels
- Petites quantités de données
- Accès mémoire très aléatoires

### Accélération typique

Pour des calculs parallélisables :
- **10-50x** plus rapide pour des opérations simples
- **100-1000x** pour le deep learning
- **Parfois jusqu'à 10000x** pour certains algorithmes spécifiques

---

## CUDA vs OpenCL

Il existe deux principales technologies pour le calcul GPU :

### CUDA (Compute Unified Device Architecture)

**Développé par** : NVIDIA  
**Fonctionne sur** : Cartes NVIDIA uniquement  
**Langage** : C-like avec extensions  

**Avantages :**
- ✅ Très optimisé pour les GPU NVIDIA
- ✅ Excellent outillage (profiler, debugger)
- ✅ Grande communauté
- ✅ Bibliothèques puissantes (cuBLAS, cuDNN)

**Inconvénients :**
- ❌ Propriétaire (NVIDIA only)
- ❌ Pas portable vers AMD ou Intel

### OpenCL (Open Computing Language)

**Développé par** : Khronos Group (standard ouvert)  
**Fonctionne sur** : NVIDIA, AMD, Intel, ARM, CPU  
**Langage** : C99-like  

**Avantages :**
- ✅ Standard ouvert
- ✅ Multi-plateforme et multi-vendor
- ✅ Peut aussi utiliser le CPU
- ✅ Portable

**Inconvénients :**
- ❌ Parfois moins optimisé que CUDA
- ❌ API plus verbose
- ❌ Support variable selon les vendors

### Comparaison

| Critère | CUDA | OpenCL |
|---------|------|--------|
| **Performance** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Portabilité** | ⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Facilité** | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Outils** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Communauté** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |

**Recommandation :**
- Si vous avez une carte NVIDIA et que la portabilité n'est pas critique → **CUDA**
- Si vous voulez supporter plusieurs vendeurs ou architectures → **OpenCL**

---

## Installation et Configuration

### Installation CUDA (NVIDIA)

**Windows :**

1. **Vérifier votre GPU**
```batch
nvidia-smi
```

2. **Télécharger CUDA Toolkit**
- Site : https://developer.nvidia.com/cuda-downloads
- Choisir Windows, x86_64, version de Windows
- Télécharger l'installeur (≈3 GB)

3. **Installer**
- Exécuter l'installeur
- Choisir "Express Installation"
- Attendre la fin (peut prendre 15-30 min)

4. **Vérifier l'installation**
```batch
nvcc --version
```

**Linux/Ubuntu :**

```bash
# Vérifier le GPU
lspci | grep -i nvidia

# Ajouter le dépôt NVIDIA
wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2204/x86_64/cuda-keyring_1.0-1_all.deb
sudo dpkg -i cuda-keyring_1.0-1_all.deb
sudo apt-get update

# Installer CUDA
sudo apt-get install cuda

# Configurer PATH
echo 'export PATH=/usr/local/cuda/bin:$PATH' >> ~/.bashrc
echo 'export LD_LIBRARY_PATH=/usr/local/cuda/lib64:$LD_LIBRARY_PATH' >> ~/.bashrc
source ~/.bashrc

# Vérifier
nvcc --version
nvidia-smi
```

### Installation OpenCL

**Windows :**

OpenCL est généralement inclus avec les drivers GPU. Télécharger :
- NVIDIA : Drivers récents incluent OpenCL
- AMD : AMD APP SDK
- Intel : Intel SDK for OpenCL

**Linux/Ubuntu :**

```bash
# Pour NVIDIA
sudo apt install nvidia-opencl-dev

# Pour AMD
sudo apt install mesa-opencl-icd

# Pour Intel
sudo apt install intel-opencl-icd

# Outils de développement
sudo apt install opencl-headers ocl-icd-opencl-dev

# Vérifier
clinfo
```

### Vérification de l'installation

```pascal
program CheckGPU;

{$mode objfpc}{$H+}

uses
  SysUtils, dynlibs;

var
  cudaLib: TLibHandle;
  openclLib: TLibHandle;

begin
  WriteLn('=== Vérification GPU ===');
  WriteLn;

  // Vérifier CUDA
  {$IFDEF WINDOWS}
  cudaLib := LoadLibrary('cudart64_12.dll');
  {$ELSE}
  cudaLib := LoadLibrary('libcudart.so');
  {$ENDIF}

  if cudaLib <> 0 then
  begin
    WriteLn('✓ CUDA détecté');
    FreeLibrary(cudaLib);
  end
  else
    WriteLn('✗ CUDA non détecté');

  // Vérifier OpenCL
  {$IFDEF WINDOWS}
  openclLib := LoadLibrary('OpenCL.dll');
  {$ELSE}
  openclLib := LoadLibrary('libOpenCL.so');
  {$ENDIF}

  if openclLib <> 0 then
  begin
    WriteLn('✓ OpenCL détecté');
    FreeLibrary(openclLib);
  end
  else
    WriteLn('✗ OpenCL non détecté');

  {$IFDEF WINDOWS}
  ReadLn;
  {$ENDIF}
end.
```

---

## Premiers Pas avec OpenCL

### Architecture OpenCL

```
┌─────────────────────────────────┐
│        Application HOST         │
│     (FreePascal/Lazarus)        │
└──────────────┬──────────────────┘
               │ OpenCL API
               ▼
┌─────────────────────────────────┐
│        OpenCL Runtime           │
└──────────────┬──────────────────┘
               │
      ┌────────┴────────┐
      ▼                 ▼
┌──────────┐      ┌──────────┐
│   GPU    │      │   CPU    │
│ Device   │      │ Device   │
└──────────┘      └──────────┘
```

### Concepts clés

**Platform** : Un vendor (NVIDIA, AMD, Intel)  
**Device** : Un GPU ou CPU disponible  
**Context** : Environnement d'exécution  
**Command Queue** : File d'instructions pour le device  
**Kernel** : Fonction qui s'exécute sur le GPU  
**Buffer** : Mémoire sur le GPU  

### Bindings OpenCL pour FreePascal

```pascal
unit OpenCL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs;

const
  {$IFDEF WINDOWS}
  OpenCL_LIB = 'OpenCL.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  OpenCL_LIB = 'libOpenCL.so';
  {$ENDIF}
  {$IFDEF DARWIN}
  OpenCL_LIB = 'OpenCL';
  {$ENDIF}

type
  // Types de base
  cl_int = Int32;
  cl_uint = UInt32;
  cl_ulong = UInt64;
  size_t = NativeUInt;

  // Types opaques (pointeurs)
  cl_platform_id = Pointer;
  cl_device_id = Pointer;
  cl_context = Pointer;
  cl_command_queue = Pointer;
  cl_mem = Pointer;
  cl_program = Pointer;
  cl_kernel = Pointer;
  cl_event = Pointer;

  // Types tableau
  Pcl_platform_id = ^cl_platform_id;
  Pcl_device_id = ^cl_device_id;

  // Codes d'erreur
  cl_error_code = cl_int;

const
  // Codes de succès/erreur
  CL_SUCCESS = 0;
  CL_DEVICE_NOT_FOUND = -1;
  CL_DEVICE_NOT_AVAILABLE = -2;
  CL_COMPILER_NOT_AVAILABLE = -3;
  CL_OUT_OF_HOST_MEMORY = -6;

  // Types de device
  CL_DEVICE_TYPE_DEFAULT = 1 shl 0;
  CL_DEVICE_TYPE_CPU = 1 shl 1;
  CL_DEVICE_TYPE_GPU = 1 shl 2;
  CL_DEVICE_TYPE_ACCELERATOR = 1 shl 3;
  CL_DEVICE_TYPE_ALL = $FFFFFFFF;

  // Flags de mémoire
  CL_MEM_READ_WRITE = 1 shl 0;
  CL_MEM_WRITE_ONLY = 1 shl 1;
  CL_MEM_READ_ONLY = 1 shl 2;
  CL_MEM_COPY_HOST_PTR = 1 shl 5;

  // Informations device
  CL_DEVICE_NAME = $102B;
  CL_DEVICE_VENDOR = $102C;
  CL_DEVICE_VERSION = $102F;
  CL_DEVICE_MAX_COMPUTE_UNITS = $1002;
  CL_DEVICE_MAX_WORK_GROUP_SIZE = $1004;
  CL_DEVICE_GLOBAL_MEM_SIZE = $101F;

var
  // Fonctions OpenCL chargées dynamiquement
  clGetPlatformIDs: function(num_entries: cl_uint; platforms: Pcl_platform_id;
                             num_platforms: Pcl_uint): cl_int; cdecl;
  clGetDeviceIDs: function(platform: cl_platform_id; device_type: cl_uint;
                           num_entries: cl_uint; devices: Pcl_device_id;
                           num_devices: Pcl_uint): cl_int; cdecl;
  clGetDeviceInfo: function(device: cl_device_id; param_name: cl_uint;
                            param_value_size: size_t; param_value: Pointer;
                            param_value_size_ret: Psize_t): cl_int; cdecl;
  clCreateContext: function(properties: Pcl_uint; num_devices: cl_uint;
                            devices: Pcl_device_id; pfn_notify: Pointer;
                            user_data: Pointer; errcode_ret: Pcl_int): cl_context; cdecl;
  clCreateCommandQueue: function(context: cl_context; device: cl_device_id;
                                 properties: cl_uint; errcode_ret: Pcl_int): cl_command_queue; cdecl;
  clCreateBuffer: function(context: cl_context; flags: cl_uint; size: size_t;
                           host_ptr: Pointer; errcode_ret: Pcl_int): cl_mem; cdecl;
  clCreateProgramWithSource: function(context: cl_context; count: cl_uint;
                                      strings: PPChar; lengths: Psize_t;
                                      errcode_ret: Pcl_int): cl_program; cdecl;
  clBuildProgram: function(prog: cl_program; num_devices: cl_uint;
                           device_list: Pcl_device_id; options: PChar;
                           pfn_notify: Pointer; user_data: Pointer): cl_int; cdecl;
  clCreateKernel: function(prog: cl_program; kernel_name: PChar;
                           errcode_ret: Pcl_int): cl_kernel; cdecl;
  clSetKernelArg: function(kernel: cl_kernel; arg_index: cl_uint;
                           arg_size: size_t; arg_value: Pointer): cl_int; cdecl;
  clEnqueueWriteBuffer: function(command_queue: cl_command_queue; buffer: cl_mem;
                                 blocking_write: cl_uint; offset: size_t;
                                 size: size_t; ptr: Pointer; num_events: cl_uint;
                                 event_wait_list: Pointer; event: Pointer): cl_int; cdecl;
  clEnqueueReadBuffer: function(command_queue: cl_command_queue; buffer: cl_mem;
                                blocking_read: cl_uint; offset: size_t;
                                size: size_t; ptr: Pointer; num_events: cl_uint;
                                event_wait_list: Pointer; event: Pointer): cl_int; cdecl;
  clEnqueueNDRangeKernel: function(command_queue: cl_command_queue; kernel: cl_kernel;
                                   work_dim: cl_uint; global_work_offset: Psize_t;
                                   global_work_size: Psize_t; local_work_size: Psize_t;
                                   num_events: cl_uint; event_wait_list: Pointer;
                                   event: Pointer): cl_int; cdecl;
  clFinish: function(command_queue: cl_command_queue): cl_int; cdecl;
  clReleaseMemObject: function(memobj: cl_mem): cl_int; cdecl;
  clReleaseKernel: function(kernel: cl_kernel): cl_int; cdecl;
  clReleaseProgram: function(prog: cl_program): cl_int; cdecl;
  clReleaseCommandQueue: function(command_queue: cl_command_queue): cl_int; cdecl;
  clReleaseContext: function(context: cl_context): cl_int; cdecl;

function LoadOpenCL: Boolean;
procedure UnloadOpenCL;
function GetErrorString(AError: cl_int): string;

implementation

var
  OpenCLHandle: TLibHandle = 0;

function LoadOpenCL: Boolean;
begin
  Result := False;

  if OpenCLHandle <> 0 then
  begin
    Result := True;
    Exit;
  end;

  OpenCLHandle := LoadLibrary(OpenCL_LIB);
  if OpenCLHandle = 0 then
  begin
    WriteLn('Erreur: Impossible de charger ', OpenCL_LIB);
    Exit;
  end;

  // Charger les fonctions
  Pointer(clGetPlatformIDs) := GetProcAddress(OpenCLHandle, 'clGetPlatformIDs');
  Pointer(clGetDeviceIDs) := GetProcAddress(OpenCLHandle, 'clGetDeviceIDs');
  Pointer(clGetDeviceInfo) := GetProcAddress(OpenCLHandle, 'clGetDeviceInfo');
  Pointer(clCreateContext) := GetProcAddress(OpenCLHandle, 'clCreateContext');
  Pointer(clCreateCommandQueue) := GetProcAddress(OpenCLHandle, 'clCreateCommandQueue');
  Pointer(clCreateBuffer) := GetProcAddress(OpenCLHandle, 'clCreateBuffer');
  Pointer(clCreateProgramWithSource) := GetProcAddress(OpenCLHandle, 'clCreateProgramWithSource');
  Pointer(clBuildProgram) := GetProcAddress(OpenCLHandle, 'clBuildProgram');
  Pointer(clCreateKernel) := GetProcAddress(OpenCLHandle, 'clCreateKernel');
  Pointer(clSetKernelArg) := GetProcAddress(OpenCLHandle, 'clSetKernelArg');
  Pointer(clEnqueueWriteBuffer) := GetProcAddress(OpenCLHandle, 'clEnqueueWriteBuffer');
  Pointer(clEnqueueReadBuffer) := GetProcAddress(OpenCLHandle, 'clEnqueueReadBuffer');
  Pointer(clEnqueueNDRangeKernel) := GetProcAddress(OpenCLHandle, 'clEnqueueNDRangeKernel');
  Pointer(clFinish) := GetProcAddress(OpenCLHandle, 'clFinish');
  Pointer(clReleaseMemObject) := GetProcAddress(OpenCLHandle, 'clReleaseMemObject');
  Pointer(clReleaseKernel) := GetProcAddress(OpenCLHandle, 'clReleaseKernel');
  Pointer(clReleaseProgram) := GetProcAddress(OpenCLHandle, 'clReleaseProgram');
  Pointer(clReleaseCommandQueue) := GetProcAddress(OpenCLHandle, 'clReleaseCommandQueue');
  Pointer(clReleaseContext) := GetProcAddress(OpenCLHandle, 'clReleaseContext');

  Result := Assigned(clGetPlatformIDs);
end;

procedure UnloadOpenCL;
begin
  if OpenCLHandle <> 0 then
  begin
    FreeLibrary(OpenCLHandle);
    OpenCLHandle := 0;
  end;
end;

function GetErrorString(AError: cl_int): string;
begin
  case AError of
    CL_SUCCESS: Result := 'Success';
    CL_DEVICE_NOT_FOUND: Result := 'Device not found';
    CL_DEVICE_NOT_AVAILABLE: Result := 'Device not available';
    CL_COMPILER_NOT_AVAILABLE: Result := 'Compiler not available';
    CL_OUT_OF_HOST_MEMORY: Result := 'Out of host memory';
  else
    Result := Format('Unknown error: %d', [AError]);
  end;
end;

initialization

finalization
  UnloadOpenCL;

end.
```

---

## Premier Programme : Addition de Vecteurs

### Le kernel OpenCL

Un kernel est une fonction qui s'exécute sur le GPU. Voici un kernel simple qui additionne deux vecteurs :

```c
// kernel_add.cl
__kernel void vector_add(__global const float* A,
                         __global const float* B,
                         __global float* C,
                         const unsigned int N)
{
    int i = get_global_id(0);

    if (i < N) {
        C[i] = A[i] + B[i];
    }
}
```

**Explications :**
- `__kernel` : marque la fonction comme un kernel
- `__global` : mémoire globale accessible par tous les threads
- `get_global_id(0)` : obtient l'index du thread courant
- Chaque thread calcule un élément du résultat

### Programme FreePascal complet

```pascal
program VectorAddGPU;

{$mode objfpc}{$H+}

uses
  SysUtils, OpenCL;

const
  VECTOR_SIZE = 1024;

  // Code du kernel
  KERNEL_SOURCE =
    '__kernel void vector_add(__global const float* A,' + LineEnding +
    '                         __global const float* B,' + LineEnding +
    '                         __global float* C,' + LineEnding +
    '                         const unsigned int N)' + LineEnding +
    '{' + LineEnding +
    '    int i = get_global_id(0);' + LineEnding +
    '    if (i < N) {' + LineEnding +
    '        C[i] = A[i] + B[i];' + LineEnding +
    '    }' + LineEnding +
    '}';

var
  // Données CPU
  A, B, C: array[0..VECTOR_SIZE-1] of Single;

  // OpenCL
  platform: cl_platform_id;
  device: cl_device_id;
  context: cl_context;
  queue: cl_command_queue;
  bufferA, bufferB, bufferC: cl_mem;
  prog: cl_program;
  kernel: cl_kernel;

  // Variables
  err: cl_int;
  i: Integer;
  numPlatforms, numDevices: cl_uint;
  globalSize: size_t;
  kernelSource: PChar;
  startTime, endTime: QWord;
  gpuTime, cpuTime: Double;

begin
  WriteLn('=== Addition de vecteurs sur GPU ===');
  WriteLn;

  if not LoadOpenCL then
  begin
    WriteLn('Échec du chargement d''OpenCL');
    Exit;
  end;

  // Initialiser les données
  WriteLn('Initialisation des données...');
  for i := 0 to VECTOR_SIZE - 1 do
  begin
    A[i] := i;
    B[i] := i * 2;
  end;

  try
    // 1. Obtenir une plateforme OpenCL
    err := clGetPlatformIDs(1, @platform, @numPlatforms);
    if err <> CL_SUCCESS then
    begin
      WriteLn('Erreur clGetPlatformIDs: ', GetErrorString(err));
      Exit;
    end;
    WriteLn('✓ Plateforme trouvée');

    // 2. Obtenir un device GPU
    err := clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 1, @device, @numDevices);
    if err <> CL_SUCCESS then
    begin
      WriteLn('GPU non trouvé, essai avec CPU...');
      err := clGetDeviceIDs(platform, CL_DEVICE_TYPE_CPU, 1, @device, @numDevices);
      if err <> CL_SUCCESS then
      begin
        WriteLn('Aucun device trouvé');
        Exit;
      end;
    end;
    WriteLn('✓ Device trouvé');

    // 3. Créer un contexte
    context := clCreateContext(nil, 1, @device, nil, nil, @err);
    if err <> CL_SUCCESS then
    begin
      WriteLn('Erreur clCreateContext: ', GetErrorString(err));
      Exit;
    end;
    WriteLn('✓ Contexte créé');

    // 4. Créer une command queue
    queue := clCreateCommandQueue(context, device, 0, @err);
    if err <> CL_SUCCESS then
    begin
      WriteLn('Erreur clCreateCommandQueue: ', GetErrorString(err));
      clReleaseContext(context);
      Exit;
    end;
    WriteLn('✓ Command queue créée');

    // 5. Créer les buffers sur le GPU
    bufferA := clCreateBuffer(context, CL_MEM_READ_ONLY or CL_MEM_COPY_HOST_PTR,
                               VECTOR_SIZE * SizeOf(Single), @A[0], @err);
    bufferB := clCreateBuffer(context, CL_MEM_READ_ONLY or CL_MEM_COPY_HOST_PTR,
                               VECTOR_SIZE * SizeOf(Single), @B[0], @err);
    bufferC := clCreateBuffer(context, CL_MEM_WRITE_ONLY,
                               VECTOR_SIZE * SizeOf(Single), nil, @err);
    WriteLn('✓ Buffers GPU créés');

    // 6. Créer et compiler le programme
    kernelSource := PChar(KERNEL_SOURCE);
    prog := clCreateProgramWithSource(context, 1, @kernelSource, nil, @err);
    if err <> CL_SUCCESS then
    begin
      WriteLn('Erreur clCreateProgramWithSource: ', GetErrorString(err));
      Exit;
    end;

    err := clBuildProgram(prog, 1, @device, nil, nil, nil);
    if err <> CL_SUCCESS then
    begin
      WriteLn('Erreur de compilation du kernel: ', GetErrorString(err));
      Exit;
    end;
    WriteLn('✓ Kernel compilé');

    // 7. Créer le kernel
    kernel := clCreateKernel(prog, 'vector_add', @err);
    if err <> CL_SUCCESS then
    begin
      WriteLn('Erreur clCreateKernel: ', GetErrorString(err));
      Exit;
    end;

    // 8. Définir les arguments du kernel
    clSetKernelArg(kernel, 0, SizeOf(cl_mem), @bufferA);
    clSetKernelArg(kernel, 1, SizeOf(cl_mem), @bufferB);
    clSetKernelArg(kernel, 2, SizeOf(cl_mem), @bufferC);
    clSetKernelArg(kernel, 3, SizeOf(cl_uint), @VECTOR_SIZE);
    WriteLn('✓ Arguments du kernel définis');

    // 9. Exécuter le kernel
    WriteLn;
    WriteLn('Exécution sur GPU...');
    startTime := GetTickCount64;

    globalSize := VECTOR_SIZE;
    err := clEnqueueNDRangeKernel(queue, kernel, 1, nil, @globalSize, nil, 0, nil, nil);
    if err <> CL_SUCCESS then
    begin
      WriteLn('Erreur clEnqueueNDRangeKernel: ', GetErrorString(err));
      Exit;
    end;

    clFinish(queue);
    endTime := GetTickCount64;
    gpuTime := (endTime - startTime);

    WriteLn('✓ Kernel exécuté en ', gpuTime:0:2, ' ms');

    // 10. Lire le résultat
    err := clEnqueueReadBuffer(queue, bufferC, 1, 0, VECTOR_SIZE * SizeOf(Single),
                                @C[0], 0, nil, nil);
    if err <> CL_SUCCESS then
    begin
      WriteLn('Erreur clEnqueueReadBuffer: ', GetErrorString(err));
      Exit;
    end;

    // Vérifier les résultats
    WriteLn;
    WriteLn('Vérification des résultats...');
    WriteLn('Quelques valeurs:');
    for i := 0 to 9 do
      WriteLn(Format('  A[%d] + B[%d] = %.0f + %.0f = %.0f',
        [i, i, A[i], B[i], C[i]]));

    // Comparaison avec le CPU
    WriteLn;
    WriteLn('Comparaison CPU vs GPU:');

    startTime := GetTickCount64;
    for i := 0 to VECTOR_SIZE - 1 do
      C[i] := A[i] + B[i];
    endTime := GetTickCount64;
    cpuTime := (endTime - startTime);

    WriteLn('  Temps CPU: ', cpuTime:0:2, ' ms');
    WriteLn('  Temps GPU: ', gpuTime:0:2, ' ms');

    if cpuTime > 0 then
      WriteLn('  Accélération: ', (cpuTime / gpuTime):0:2, 'x');

    // Libérer les ressources
    clReleaseMemObject(bufferA);
    clReleaseMemObject(bufferB);
    clReleaseMemObject(bufferC);
    clReleaseKernel(kernel);
    clReleaseProgram(prog);
    clReleaseCommandQueue(queue);
    clReleaseContext(context);

    WriteLn;
    WriteLn('✓ Ressources libérées');

  finally
    UnloadOpenCL;
  end;

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

**Sortie attendue :**
```
=== Addition de vecteurs sur GPU ===

Initialisation des données...
✓ Plateforme trouvée
✓ Device trouvé
✓ Contexte créé
✓ Command queue créée
✓ Buffers GPU créés
✓ Kernel compilé
✓ Arguments du kernel définis

Exécution sur GPU...
✓ Kernel exécuté en 2.35 ms

Vérification des résultats...
Quelques valeurs:
  A[0] + B[0] = 0 + 0 = 0
  A[1] + B[1] = 1 + 2 = 3
  A[2] + B[2] = 2 + 4 = 6
  A[3] + B[3] = 3 + 6 = 9
  A[4] + B[4] = 4 + 8 = 12
  A[5] + B[5] = 5 + 10 = 15
  A[6] + B[6] = 6 + 12 = 18
  A[7] + B[7] = 7 + 14 = 21
  A[8] + B[8] = 8 + 16 = 24
  A[9] + B[9] = 9 + 18 = 27

Comparaison CPU vs GPU:
  Temps CPU: 0.02 ms
  Temps GPU: 2.35 ms
  Accélération: 0.01x

✓ Ressources libérées
```

**Note importante :** Pour de petites données (1024 éléments), le CPU est plus rapide car le transfert de données CPU↔GPU prend plus de temps que le calcul lui-même. Le GPU devient intéressant avec des données plus volumineuses (millions d'éléments).

---

## Wrapper Orienté Objet

Pour simplifier l'utilisation d'OpenCL, créons un wrapper orienté objet :

```pascal
unit GPUCompute;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpenCL;

type
  TGPUDevice = class
  private
    FPlatform: cl_platform_id;
    FDevice: cl_device_id;
    FContext: cl_context;
    FQueue: cl_command_queue;
    FDeviceName: string;
    FDeviceVendor: string;
    FMaxComputeUnits: Integer;
    FGlobalMemSize: Int64;
    FInitialized: Boolean;

    procedure LoadDeviceInfo;
  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(APreferGPU: Boolean = True): Boolean;
    function CreateBuffer(ASize: size_t; AFlags: cl_uint): cl_mem;
    function WriteBuffer(ABuffer: cl_mem; AData: Pointer; ASize: size_t): Boolean;
    function ReadBuffer(ABuffer: cl_mem; AData: Pointer; ASize: size_t): Boolean;
    procedure ReleaseBuffer(ABuffer: cl_mem);
    procedure Finish;

    property DeviceName: string read FDeviceName;
    property DeviceVendor: string read FDeviceVendor;
    property MaxComputeUnits: Integer read FMaxComputeUnits;
    property GlobalMemSize: Int64 read FGlobalMemSize;
    property Initialized: Boolean read FInitialized;
    property Context: cl_context read FContext;
    property Queue: cl_command_queue read FQueue;
    property Device: cl_device_id read FDevice;
  end;

  TGPUKernel = class
  private
    FDevice: TGPUDevice;
    FProgram: cl_program;
    FKernel: cl_kernel;
    FKernelName: string;
    FCompiled: Boolean;
  public
    constructor Create(ADevice: TGPUDevice);
    destructor Destroy; override;

    function CompileFromSource(const ASource, AKernelName: string): Boolean;
    function CompileFromFile(const AFileName, AKernelName: string): Boolean;
    function SetArg(AIndex: Integer; ASize: size_t; AValue: Pointer): Boolean;
    function Execute(AGlobalSize: size_t; ALocalSize: size_t = 0): Boolean;

    property Compiled: Boolean read FCompiled;
    property KernelName: string read FKernelName;
  end;

implementation

{ TGPUDevice }

constructor TGPUDevice.Create;
begin
  FInitialized := False;

  if not LoadOpenCL then
    raise Exception.Create('OpenCL non disponible');
end;

destructor TGPUDevice.Destroy;
begin
  if FInitialized then
  begin
    if Assigned(FQueue) then
      clReleaseCommandQueue(FQueue);
    if Assigned(FContext) then
      clReleaseContext(FContext);
  end;

  inherited;
end;

function TGPUDevice.Initialize(APreferGPU: Boolean): Boolean;
var
  err: cl_int;
  numPlatforms: cl_uint;
  deviceType: cl_uint;
begin
  Result := False;

  // Obtenir une plateforme
  err := clGetPlatformIDs(1, @FPlatform, @numPlatforms);
  if err <> CL_SUCCESS then
    Exit;

  // Choisir le type de device
  if APreferGPU then
    deviceType := CL_DEVICE_TYPE_GPU
  else
    deviceType := CL_DEVICE_TYPE_CPU;

  // Obtenir le device
  err := clGetDeviceIDs(FPlatform, deviceType, 1, @FDevice, nil);
  if err <> CL_SUCCESS then
  begin
    // Fallback sur CPU si GPU non disponible
    if deviceType = CL_DEVICE_TYPE_GPU then
    begin
      WriteLn('GPU non disponible, utilisation du CPU');
      err := clGetDeviceIDs(FPlatform, CL_DEVICE_TYPE_CPU, 1, @FDevice, nil);
    end;

    if err <> CL_SUCCESS then
      Exit;
  end;

  // Créer le contexte
  FContext := clCreateContext(nil, 1, @FDevice, nil, nil, @err);
  if err <> CL_SUCCESS then
    Exit;

  // Créer la command queue
  FQueue := clCreateCommandQueue(FContext, FDevice, 0, @err);
  if err <> CL_SUCCESS then
  begin
    clReleaseContext(FContext);
    Exit;
  end;

  // Charger les informations du device
  LoadDeviceInfo;

  FInitialized := True;
  Result := True;
end;

procedure TGPUDevice.LoadDeviceInfo;
var
  buffer: array[0..255] of Char;
  retSize: size_t;
  value: cl_uint;
  memSize: cl_ulong;
begin
  // Nom du device
  if clGetDeviceInfo(FDevice, CL_DEVICE_NAME, 256, @buffer[0], @retSize) = CL_SUCCESS then
    FDeviceName := string(PChar(@buffer[0]));

  // Vendeur
  if clGetDeviceInfo(FDevice, CL_DEVICE_VENDOR, 256, @buffer[0], @retSize) = CL_SUCCESS then
    FDeviceVendor := string(PChar(@buffer[0]));

  // Nombre d'unités de calcul
  if clGetDeviceInfo(FDevice, CL_DEVICE_MAX_COMPUTE_UNITS, SizeOf(cl_uint), @value, nil) = CL_SUCCESS then
    FMaxComputeUnits := value;

  // Mémoire globale
  if clGetDeviceInfo(FDevice, CL_DEVICE_GLOBAL_MEM_SIZE, SizeOf(cl_ulong), @memSize, nil) = CL_SUCCESS then
    FGlobalMemSize := memSize;
end;

function TGPUDevice.CreateBuffer(ASize: size_t; AFlags: cl_uint): cl_mem;
var
  err: cl_int;
begin
  Result := clCreateBuffer(FContext, AFlags, ASize, nil, @err);
  if err <> CL_SUCCESS then
    Result := nil;
end;

function TGPUDevice.WriteBuffer(ABuffer: cl_mem; AData: Pointer; ASize: size_t): Boolean;
var
  err: cl_int;
begin
  err := clEnqueueWriteBuffer(FQueue, ABuffer, 1, 0, ASize, AData, 0, nil, nil);
  Result := err = CL_SUCCESS;
end;

function TGPUDevice.ReadBuffer(ABuffer: cl_mem; AData: Pointer; ASize: size_t): Boolean;
var
  err: cl_int;
begin
  err := clEnqueueReadBuffer(FQueue, ABuffer, 1, 0, ASize, AData, 0, nil, nil);
  Result := err = CL_SUCCESS;
end;

procedure TGPUDevice.ReleaseBuffer(ABuffer: cl_mem);
begin
  if Assigned(ABuffer) then
    clReleaseMemObject(ABuffer);
end;

procedure TGPUDevice.Finish;
begin
  clFinish(FQueue);
end;

{ TGPUKernel }

constructor TGPUKernel.Create(ADevice: TGPUDevice);
begin
  FDevice := ADevice;
  FCompiled := False;
end;

destructor TGPUKernel.Destroy;
begin
  if FCompiled then
  begin
    if Assigned(FKernel) then
      clReleaseKernel(FKernel);
    if Assigned(FProgram) then
      clReleaseProgram(FProgram);
  end;

  inherited;
end;

function TGPUKernel.CompileFromSource(const ASource, AKernelName: string): Boolean;
var
  err: cl_int;
  source: PChar;
begin
  Result := False;
  FKernelName := AKernelName;

  // Créer le programme
  source := PChar(ASource);
  FProgram := clCreateProgramWithSource(FDevice.Context, 1, @source, nil, @err);
  if err <> CL_SUCCESS then
  begin
    WriteLn('Erreur création du programme: ', GetErrorString(err));
    Exit;
  end;

  // Compiler
  err := clBuildProgram(FProgram, 1, @FDevice.Device, nil, nil, nil);
  if err <> CL_SUCCESS then
  begin
    WriteLn('Erreur compilation: ', GetErrorString(err));
    clReleaseProgram(FProgram);
    Exit;
  end;

  // Créer le kernel
  FKernel := clCreateKernel(FProgram, PChar(AKernelName), @err);
  if err <> CL_SUCCESS then
  begin
    WriteLn('Erreur création du kernel: ', GetErrorString(err));
    clReleaseProgram(FProgram);
    Exit;
  end;

  FCompiled := True;
  Result := True;
end;

function TGPUKernel.CompileFromFile(const AFileName, AKernelName: string): Boolean;
var
  source: TStringList;
begin
  source := TStringList.Create;
  try
    source.LoadFromFile(AFileName);
    Result := CompileFromSource(source.Text, AKernelName);
  finally
    source.Free;
  end;
end;

function TGPUKernel.SetArg(AIndex: Integer; ASize: size_t; AValue: Pointer): Boolean;
var
  err: cl_int;
begin
  err := clSetKernelArg(FKernel, AIndex, ASize, AValue);
  Result := err = CL_SUCCESS;
end;

function TGPUKernel.Execute(AGlobalSize: size_t; ALocalSize: size_t): Boolean;
var
  err: cl_int;
  localSize: Psize_t;
begin
  if ALocalSize > 0 then
    localSize := @ALocalSize
  else
    localSize := nil;

  err := clEnqueueNDRangeKernel(FDevice.Queue, FKernel, 1, nil,
                                @AGlobalSize, localSize, 0, nil, nil);
  Result := err = CL_SUCCESS;
end;

end.
```

### Utilisation du wrapper

```pascal
program SimpleGPUCompute;

{$mode objfpc}{$H+}

uses
  SysUtils, GPUCompute;

const
  DATA_SIZE = 1024;

  KERNEL_SOURCE =
    '__kernel void square(__global float* input,' + LineEnding +
    '                     __global float* output)' + LineEnding +
    '{' + LineEnding +
    '    int i = get_global_id(0);' + LineEnding +
    '    output[i] = input[i] * input[i];' + LineEnding +
    '}';

var
  device: TGPUDevice;
  kernel: TGPUKernel;
  input, output: array[0..DATA_SIZE-1] of Single;
  bufferInput, bufferOutput: cl_mem;
  i: Integer;

begin
  WriteLn('=== Calcul GPU simplifié ===');
  WriteLn;

  // Créer le device GPU
  device := TGPUDevice.Create;
  try
    if not device.Initialize(True) then
    begin
      WriteLn('Impossible d''initialiser le GPU');
      Exit;
    end;

    WriteLn('Device: ', device.DeviceName);
    WriteLn('Vendor: ', device.DeviceVendor);
    WriteLn('Compute Units: ', device.MaxComputeUnits);
    WriteLn('Global Memory: ', device.GlobalMemSize div (1024*1024), ' MB');
    WriteLn;

    // Préparer les données
    for i := 0 to DATA_SIZE - 1 do
      input[i] := i;

    // Créer les buffers
    bufferInput := device.CreateBuffer(DATA_SIZE * SizeOf(Single),
                                       CL_MEM_READ_ONLY);
    bufferOutput := device.CreateBuffer(DATA_SIZE * SizeOf(Single),
                                        CL_MEM_WRITE_ONLY);

    // Écrire les données d'entrée
    device.WriteBuffer(bufferInput, @input[0], DATA_SIZE * SizeOf(Single));

    // Compiler le kernel
    kernel := TGPUKernel.Create(device);
    try
      if not kernel.CompileFromSource(KERNEL_SOURCE, 'square') then
      begin
        WriteLn('Erreur de compilation du kernel');
        Exit;
      end;

      WriteLn('✓ Kernel compilé');

      // Définir les arguments
      kernel.SetArg(0, SizeOf(cl_mem), @bufferInput);
      kernel.SetArg(1, SizeOf(cl_mem), @bufferOutput);

      // Exécuter
      WriteLn('Exécution du kernel...');
      if kernel.Execute(DATA_SIZE) then
      begin
        device.Finish;
        WriteLn('✓ Kernel exécuté');

        // Lire les résultats
        device.ReadBuffer(bufferOutput, @output[0], DATA_SIZE * SizeOf(Single));

        // Afficher quelques résultats
        WriteLn;
        WriteLn('Résultats:');
        for i := 0 to 9 do
          WriteLn(Format('  %.0f² = %.0f', [input[i], output[i]]));
      end
      else
        WriteLn('✗ Erreur d''exécution');

    finally
      kernel.Free;
    end;

    // Libérer les buffers
    device.ReleaseBuffer(bufferInput);
    device.ReleaseBuffer(bufferOutput);

  finally
    device.Free;
  end;

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Exemples Pratiques

### 1. Traitement d'Image : Filtre de Flou

```pascal
// Kernel pour un flou gaussien
const
  BLUR_KERNEL =
    '__kernel void gaussian_blur(__global const uchar4* input,' + LineEnding +
    '                            __global uchar4* output,' + LineEnding +
    '                            const int width,' + LineEnding +
    '                            const int height)' + LineEnding +
    '{' + LineEnding +
    '    int x = get_global_id(0);' + LineEnding +
    '    int y = get_global_id(1);' + LineEnding +
    '    ' + LineEnding +
    '    if (x >= width || y >= height) return;' + LineEnding +
    '    ' + LineEnding +
    '    // Noyau 3x3' + LineEnding +
    '    float kernel[9] = {1, 2, 1, 2, 4, 2, 1, 2, 1};' + LineEnding +
    '    float sum = 16.0f;' + LineEnding +
    '    ' + LineEnding +
    '    float4 result = (float4)(0, 0, 0, 0);' + LineEnding +
    '    ' + LineEnding +
    '    for (int dy = -1; dy <= 1; dy++) {' + LineEnding +
    '        for (int dx = -1; dx <= 1; dx++) {' + LineEnding +
    '            int nx = clamp(x + dx, 0, width - 1);' + LineEnding +
    '            int ny = clamp(y + dy, 0, height - 1);' + LineEnding +
    '            int idx = ny * width + nx;' + LineEnding +
    '            ' + LineEnding +
    '            uchar4 pixel = input[idx];' + LineEnding +
    '            float weight = kernel[(dy+1)*3 + (dx+1)];' + LineEnding +
    '            ' + LineEnding +
    '            result += convert_float4(pixel) * weight;' + LineEnding +
    '        }' + LineEnding +
    '    }' + LineEnding +
    '    ' + LineEnding +
    '    result /= sum;' + LineEnding +
    '    output[y * width + x] = convert_uchar4(result);' + LineEnding +
    '}';

program ImageBlurGPU;

{$mode objfpc}{$H+}

uses
  SysUtils, Graphics, GPUCompute;

procedure ApplyBlur(ABitmap: TBitmap);
var
  device: TGPUDevice;
  kernel: TGPUKernel;
  width, height, i: Integer;
  inputData, outputData: array of LongWord;
  bufferInput, bufferOutput: cl_mem;
  globalSize: array[0..1] of size_t;
begin
  width := ABitmap.Width;
  height := ABitmap.Height;

  SetLength(inputData, width * height);
  SetLength(outputData, width * height);

  // Copier les pixels
  for i := 0 to width * height - 1 do
    inputData[i] := PLongWord(ABitmap.ScanLine[i div width])[i mod width];

  device := TGPUDevice.Create;
  try
    if not device.Initialize then
    begin
      WriteLn('GPU non disponible');
      Exit;
    end;

    // Créer les buffers
    bufferInput := device.CreateBuffer(width * height * 4, CL_MEM_READ_ONLY);
    bufferOutput := device.CreateBuffer(width * height * 4, CL_MEM_WRITE_ONLY);

    device.WriteBuffer(bufferInput, @inputData[0], width * height * 4);

    // Compiler et exécuter
    kernel := TGPUKernel.Create(device);
    try
      if kernel.CompileFromSource(BLUR_KERNEL, 'gaussian_blur') then
      begin
        kernel.SetArg(0, SizeOf(cl_mem), @bufferInput);
        kernel.SetArg(1, SizeOf(cl_mem), @bufferOutput);
        kernel.SetArg(2, SizeOf(Integer), @width);
        kernel.SetArg(3, SizeOf(Integer), @height);

        globalSize[0] := width;
        globalSize[1] := height;

        // Note: Pour 2D, utilisez clEnqueueNDRangeKernel directement
        // ou modifiez TGPUKernel pour supporter 2D

        device.Finish;
        device.ReadBuffer(bufferOutput, @outputData[0], width * height * 4);

        // Copier les résultats
        for i := 0 to width * height - 1 do
          PLongWord(ABitmap.ScanLine[i div width])[i mod width] := outputData[i];
      end;
    finally
      kernel.Free;
    end;

    device.ReleaseBuffer(bufferInput);
    device.ReleaseBuffer(bufferOutput);

  finally
    device.Free;
  end;
end;

end.
```

### 2. Multiplication de Matrices

```pascal
const
  MATRIX_MUL_KERNEL =
    '__kernel void matmul(__global const float* A,' + LineEnding +
    '                     __global const float* B,' + LineEnding +
    '                     __global float* C,' + LineEnding +
    '                     const int M,' + LineEnding +
    '                     const int N,' + LineEnding +
    '                     const int K)' + LineEnding +
    '{' + LineEnding +
    '    int row = get_global_id(0);' + LineEnding +
    '    int col = get_global_id(1);' + LineEnding +
    '    ' + LineEnding +
    '    if (row >= M || col >= N) return;' + LineEnding +
    '    ' + LineEnding +
    '    float sum = 0.0f;' + LineEnding +
    '    for (int k = 0; k < K; k++) {' + LineEnding +
    '        sum += A[row * K + k] * B[k * N + col];' + LineEnding +
    '    }' + LineEnding +
    '    ' + LineEnding +
    '    C[row * N + col] = sum;' + LineEnding +
    '}';

type
  TMatrix = array of array of Single;

function MatrixMultiplyGPU(const A, B: TMatrix): TMatrix;
var
  device: TGPUDevice;
  kernel: TGPUKernel;
  M, N, K: Integer;
  flatA, flatB, flatC: array of Single;
  bufA, bufB, bufC: cl_mem;
  i, j: Integer;
begin
  M := Length(A);      // Lignes de A
  K := Length(A[0]);   // Colonnes de A = Lignes de B
  N := Length(B[0]);   // Colonnes de B

  // Aplatir les matrices
  SetLength(flatA, M * K);
  SetLength(flatB, K * N);
  SetLength(flatC, M * N);

  for i := 0 to M - 1 do
    for j := 0 to K - 1 do
      flatA[i * K + j] := A[i, j];

  for i := 0 to K - 1 do
    for j := 0 to N - 1 do
      flatB[i * N + j] := B[i, j];

  device := TGPUDevice.Create;
  try
    device.Initialize;

    // Créer les buffers
    bufA := device.CreateBuffer(M * K * SizeOf(Single), CL_MEM_READ_ONLY);
    bufB := device.CreateBuffer(K * N * SizeOf(Single), CL_MEM_READ_ONLY);
    bufC := device.CreateBuffer(M * N * SizeOf(Single), CL_MEM_WRITE_ONLY);

    device.WriteBuffer(bufA, @flatA[0], M * K * SizeOf(Single));
    device.WriteBuffer(bufB, @flatB[0], K * N * SizeOf(Single));

    kernel := TGPUKernel.Create(device);
    try
      if kernel.CompileFromSource(MATRIX_MUL_KERNEL, 'matmul') then
      begin
        kernel.SetArg(0, SizeOf(cl_mem), @bufA);
        kernel.SetArg(1, SizeOf(cl_mem), @bufB);
        kernel.SetArg(2, SizeOf(cl_mem), @bufC);
        kernel.SetArg(3, SizeOf(Integer), @M);
        kernel.SetArg(4, SizeOf(Integer), @N);
        kernel.SetArg(5, SizeOf(Integer), @K);

        kernel.Execute(M * N);
        device.Finish;

        device.ReadBuffer(bufC, @flatC[0], M * N * SizeOf(Single));

        // Reconstruire la matrice résultat
        SetLength(Result, M, N);
        for i := 0 to M - 1 do
          for j := 0 to N - 1 do
            Result[i, j] := flatC[i * N + j];
      end;
    finally
      kernel.Free;
    end;

    device.ReleaseBuffer(bufA);
    device.ReleaseBuffer(bufB);
    device.ReleaseBuffer(bufC);

  finally
    device.Free;
  end;
end;
```

### 3. Calcul de Mandelbrot

```pascal
const
  MANDELBROT_KERNEL =
    '__kernel void mandelbrot(__global uchar4* output,' + LineEnding +
    '                         const int width,' + LineEnding +
    '                         const int height,' + LineEnding +
    '                         const float zoom,' + LineEnding +
    '                         const float offsetX,' + LineEnding +
    '                         const float offsetY)' + LineEnding +
    '{' + LineEnding +
    '    int x = get_global_id(0);' + LineEnding +
    '    int y = get_global_id(1);' + LineEnding +
    '    ' + LineEnding +
    '    if (x >= width || y >= height) return;' + LineEnding +
    '    ' + LineEnding +
    '    float real = (x - width / 2.0f) * zoom + offsetX;' + LineEnding +
    '    float imag = (y - height / 2.0f) * zoom + offsetY;' + LineEnding +
    '    ' + LineEnding +
    '    float zr = 0.0f, zi = 0.0f;' + LineEnding +
    '    int iterations = 0;' + LineEnding +
    '    const int maxIter = 256;' + LineEnding +
    '    ' + LineEnding +
    '    while (zr * zr + zi * zi < 4.0f && iterations < maxIter) {' + LineEnding +
    '        float temp = zr * zr - zi * zi + real;' + LineEnding +
    '        zi = 2.0f * zr * zi + imag;' + LineEnding +
    '        zr = temp;' + LineEnding +
    '        iterations++;' + LineEnding +
    '    }' + LineEnding +
    '    ' + LineEnding +
    '    uchar color = (uchar)(iterations * 255 / maxIter);' + LineEnding +
    '    output[y * width + x] = (uchar4)(color, color, color, 255);' + LineEnding +
    '}';

procedure GenerateMandelbrotGPU(ABitmap: TBitmap; AZoom, AOffsetX, AOffsetY: Single);
var
  device: TGPUDevice;
  kernel: TGPUKernel;
  width, height: Integer;
  output: array of LongWord;
  buffer: cl_mem;
  globalSize: array[0..1] of size_t;
  i: Integer;
begin
  width := ABitmap.Width;
  height := ABitmap.Height;

  SetLength(output, width * height);

  device := TGPUDevice.Create;
  try
    device.Initialize;

    buffer := device.CreateBuffer(width * height * 4, CL_MEM_WRITE_ONLY);

    kernel := TGPUKernel.Create(device);
    try
      if kernel.CompileFromSource(MANDELBROT_KERNEL, 'mandelbrot') then
      begin
        kernel.SetArg(0, SizeOf(cl_mem), @buffer);
        kernel.SetArg(1, SizeOf(Integer), @width);
        kernel.SetArg(2, SizeOf(Integer), @height);
        kernel.SetArg(3, SizeOf(Single), @AZoom);
        kernel.SetArg(4, SizeOf(Single), @AOffsetX);
        kernel.SetArg(5, SizeOf(Single), @AOffsetY);

        globalSize[0] := width;
        globalSize[1] := height;

        // Exécution 2D - nécessite d'adapter le wrapper
        device.Finish;

        device.ReadBuffer(buffer, @output[0], width * height * 4);

        // Copier vers le bitmap
        for i := 0 to width * height - 1 do
          PLongWord(ABitmap.ScanLine[i div width])[i mod width] := output[i];
      end;
    finally
      kernel.Free;
    end;

    device.ReleaseBuffer(buffer);

  finally
    device.Free;
  end;
end;
```

---

## Optimisation des Performances

### 1. Mémoire locale (Local Memory)

La mémoire locale est partagée entre les threads d'un work-group et est beaucoup plus rapide que la mémoire globale.

```c
// Kernel optimisé avec mémoire locale
__kernel void optimized_convolution(
    __global const float* input,
    __global float* output,
    __local float* localMem,  // Mémoire locale
    const int width,
    const int height)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int lx = get_local_id(0);
    int ly = get_local_id(1);

    // Charger dans la mémoire locale
    localMem[ly * get_local_size(0) + lx] = input[gy * width + gx];

    // Synchroniser tous les threads du work-group
    barrier(CLK_LOCAL_MEM_FENCE);

    // Utiliser localMem pour les calculs
    // ... calculs rapides ...
}
```

### 2. Coalescence mémoire

Accéder à la mémoire de manière contiguë améliore les performances :

```c
// Mauvais : accès non coalescés
__kernel void bad_access(__global float* data) {
    int i = get_global_id(0);
    data[i * 1000] = i;  // Stride important
}

// Bon : accès coalescés
__kernel void good_access(__global float* data) {
    int i = get_global_id(0);
    data[i] = i;  // Accès contigus
}
```

### 3. Vectorisation

Utiliser les types vectoriels d'OpenCL :

```c
__kernel void vectorized(__global float4* input,
                         __global float4* output)
{
    int i = get_global_id(0);

    // Traiter 4 éléments à la fois
    float4 value = input[i];
    output[i] = value * value;
}
```

### 4. Mesure des performances

```pascal
unit GPUBenchmark;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, GPUCompute;

type
  TBenchmarkResult = record
    KernelTime: Double;      // Temps d'exécution kernel (ms)
    TransferTime: Double;    // Temps de transfert données (ms)
    TotalTime: Double;       // Temps total (ms)
    Throughput: Double;      // Débit (GB/s)
  end;

function BenchmarkKernel(ADevice: TGPUDevice;
                         AKernel: TGPUKernel;
                         ADataSize: Integer): TBenchmarkResult;

implementation

function BenchmarkKernel(ADevice: TGPUDevice;
                         AKernel: TGPUKernel;
                         ADataSize: Integer): TBenchmarkResult;
var
  buffer: cl_mem;
  data: array of Single;
  startTime, endTime: QWord;
  i: Integer;
begin
  SetLength(data, ADataSize);
  for i := 0 to ADataSize - 1 do
    data[i] := Random;

  // Mesurer le transfert Host → Device
  startTime := GetTickCount64;
  buffer := ADevice.CreateBuffer(ADataSize * SizeOf(Single), CL_MEM_READ_WRITE);
  ADevice.WriteBuffer(buffer, @data[0], ADataSize * SizeOf(Single));
  ADevice.Finish;
  endTime := GetTickCount64;
  Result.TransferTime := endTime - startTime;

  // Mesurer l'exécution du kernel
  startTime := GetTickCount64;
  AKernel.Execute(ADataSize);
  ADevice.Finish;
  endTime := GetTickCount64;
  Result.KernelTime := endTime - startTime;

  // Mesurer le transfert Device → Host
  startTime := GetTickCount64;
  ADevice.ReadBuffer(buffer, @data[0], ADataSize * SizeOf(Single));
  ADevice.Finish;
  endTime := GetTickCount64;
  Result.TransferTime := Result.TransferTime + (endTime - startTime);

  Result.TotalTime := Result.KernelTime + Result.TransferTime;

  // Calculer le débit
  Result.Throughput := (ADataSize * SizeOf(Single) * 2) / // Lecture + Écriture
                       (Result.TotalTime / 1000) / // Convertir en secondes
                       (1024 * 1024 * 1024); // GB/s

  ADevice.ReleaseBuffer(buffer);
  SetLength(data, 0);
end;

end.
```

---

## Interopérabilité OpenGL-OpenCL

Pour le rendu graphique accéléré, on peut partager des données entre OpenGL et OpenCL :

```pascal
unit GLCLInterop;

{$mode objfpc}{$H+}

interface

uses
  OpenCL, GL, GLext;

type
  TGLCLBuffer = class
  private
    FGLBuffer: GLuint;
    FCLBuffer: cl_mem;
    FSize: size_t;
    FContext: cl_context;
  public
    constructor Create(AContext: cl_context; ASize: size_t);
    destructor Destroy; override;

    procedure AcquireFromGL(AQueue: cl_command_queue);
    procedure ReleaseToGL(AQueue: cl_command_queue);

    property GLBuffer: GLuint read FGLBuffer;
    property CLBuffer: cl_mem read FCLBuffer;
  end;

implementation

constructor TGLCLBuffer.Create(AContext: cl_context; ASize: size_t);
var
  err: cl_int;
begin
  FContext := AContext;
  FSize := ASize;

  // Créer un VBO OpenGL
  glGenBuffers(1, @FGLBuffer);
  glBindBuffer(GL_ARRAY_BUFFER, FGLBuffer);
  glBufferData(GL_ARRAY_BUFFER, ASize, nil, GL_DYNAMIC_DRAW);

  // Créer un buffer OpenCL partagé
  FCLBuffer := clCreateFromGLBuffer(FContext, CL_MEM_READ_WRITE, FGLBuffer, @err);

  if err <> CL_SUCCESS then
    raise Exception.Create('Erreur création buffer GL/CL: ' + GetErrorString(err));
end;

destructor TGLCLBuffer.Destroy;
begin
  if FCLBuffer <> nil then
    clReleaseMemObject(FCLBuffer);

  if FGLBuffer <> 0 then
    glDeleteBuffers(1, @FGLBuffer);

  inherited;
end;

procedure TGLCLBuffer.AcquireFromGL(AQueue: cl_command_queue);
begin
  clEnqueueAcquireGLObjects(AQueue, 1, @FCLBuffer, 0, nil, nil);
end;

procedure TGLCLBuffer.ReleaseToGL(AQueue: cl_command_queue);
begin
  clEnqueueReleaseGLObjects(AQueue, 1, @FCLBuffer, 0, nil, nil);
end;

end.
```

---

## Gestion Multi-Plateforme

### Détection automatique du meilleur device

```pascal
function SelectBestDevice: TGPUDevice;
var
  platforms: array[0..9] of cl_platform_id;
  numPlatforms: cl_uint;
  i, j: Integer;
  devices: array[0..9] of cl_device_id;
  numDevices: cl_uint;
  deviceType: cl_uint;
  computeUnits: cl_uint;
  bestDevice: cl_device_id;
  maxComputeUnits: cl_uint;
  err: cl_int;
begin
  Result := TGPUDevice.Create;

  // Lister toutes les plateformes
  err := clGetPlatformIDs(10, @platforms[0], @numPlatforms);
  if err <> CL_SUCCESS then
  begin
    WriteLn('Aucune plateforme OpenCL trouvée');
    Exit;
  end;

  maxComputeUnits := 0;
  bestDevice := nil;

  // Parcourir toutes les plateformes et devices
  for i := 0 to numPlatforms - 1 do
  begin
    err := clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, 10, @devices[0], @numDevices);
    if err = CL_SUCCESS then
    begin
      for j := 0 to numDevices - 1 do
      begin
        clGetDeviceInfo(devices[j], CL_DEVICE_MAX_COMPUTE_UNITS,
                        SizeOf(cl_uint), @computeUnits, nil);

        if computeUnits > maxComputeUnits then
        begin
          maxComputeUnits := computeUnits;
          bestDevice := devices[j];
        end;
      end;
    end;
  end;

  if bestDevice <> nil then
    WriteLn('Meilleur device trouvé: ', maxComputeUnits, ' compute units');
end;
```

### Configuration spécifique par OS

```pascal
procedure ConfigureForPlatform(ADevice: TGPUDevice);
begin
  {$IFDEF WINDOWS}
  // Optimisations Windows
  WriteLn('Configuration Windows');
  // Utiliser les extensions NVIDIA si disponibles
  {$ENDIF}

  {$IFDEF LINUX}
  // Optimisations Linux
  WriteLn('Configuration Linux');
  // Vérifier les permissions sur /dev/dri
  {$ENDIF}

  {$IFDEF DARWIN}
  // Optimisations macOS
  WriteLn('Configuration macOS');
  // Utiliser Metal Performance Shaders si possible
  {$ENDIF}
end;
```

---

## Debugging et Profiling

### 1. Vérification des erreurs

```pascal
function CheckCLError(AError: cl_int; const AContext: string): Boolean;
begin
  Result := AError = CL_SUCCESS;

  if not Result then
  begin
    WriteLn('Erreur OpenCL dans ', AContext, ': ', GetErrorString(AError));

    // Informations supplémentaires selon l'erreur
    case AError of
      CL_OUT_OF_HOST_MEMORY:
        WriteLn('  → Mémoire RAM insuffisante');
      CL_OUT_OF_RESOURCES:
        WriteLn('  → Ressources GPU insuffisantes');
      CL_MEM_OBJECT_ALLOCATION_FAILURE:
        WriteLn('  → Allocation mémoire GPU échouée');
      CL_INVALID_WORK_GROUP_SIZE:
        WriteLn('  → Taille de work-group invalide');
    end;
  end;
end;
```

### 2. Profiling avec événements

```pascal
function ProfileKernelExecution(AQueue: cl_command_queue;
                                 AKernel: cl_kernel;
                                 AGlobalSize: size_t): Double;
var
  event: cl_event;
  err: cl_int;
  startTime, endTime: cl_ulong;
begin
  // Exécuter avec événement
  err := clEnqueueNDRangeKernel(AQueue, AKernel, 1, nil, @AGlobalSize,
                                nil, 0, nil, @event);

  if err <> CL_SUCCESS then
  begin
    Result := -1;
    Exit;
  end;

  clFinish(AQueue);

  // Récupérer les timestamps
  clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_START,
                          SizeOf(cl_ulong), @startTime, nil);
  clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END,
                          SizeOf(cl_ulong), @endTime, nil);

  // Temps en millisecondes
  Result := (endTime - startTime) / 1000000.0;

  clReleaseEvent(event);
end;
```

### 3. Informations détaillées du device

```pascal
procedure PrintDeviceInfo(ADevice: cl_device_id);
var
  buffer: array[0..1023] of Char;
  uintValue: cl_uint;
  ulongValue: cl_ulong;
  sizetValue: size_t;
begin
  WriteLn('=== Informations du Device ===');

  // Nom
  clGetDeviceInfo(ADevice, CL_DEVICE_NAME, 1024, @buffer[0], nil);
  WriteLn('Nom: ', string(PChar(@buffer[0])));

  // Vendeur
  clGetDeviceInfo(ADevice, CL_DEVICE_VENDOR, 1024, @buffer[0], nil);
  WriteLn('Vendeur: ', string(PChar(@buffer[0])));

  // Version
  clGetDeviceInfo(ADevice, CL_DEVICE_VERSION, 1024, @buffer[0], nil);
  WriteLn('Version OpenCL: ', string(PChar(@buffer[0])));

  // Compute Units
  clGetDeviceInfo(ADevice, CL_DEVICE_MAX_COMPUTE_UNITS, SizeOf(cl_uint), @uintValue, nil);
  WriteLn('Compute Units: ', uintValue);

  // Fréquence
  clGetDeviceInfo(ADevice, CL_DEVICE_MAX_CLOCK_FREQUENCY, SizeOf(cl_uint), @uintValue, nil);
  WriteLn('Fréquence max: ', uintValue, ' MHz');

  // Mémoire globale
  clGetDeviceInfo(ADevice, CL_DEVICE_GLOBAL_MEM_SIZE, SizeOf(cl_ulong), @ulongValue, nil);
  WriteLn('Mémoire globale: ', ulongValue div (1024*1024), ' MB');

  // Mémoire locale
  clGetDeviceInfo(ADevice, CL_DEVICE_LOCAL_MEM_SIZE, SizeOf(cl_ulong), @ulongValue, nil);
  WriteLn('Mémoire locale: ', ulongValue div 1024, ' KB');

  // Taille max work-group
  clGetDeviceInfo(ADevice, CL_DEVICE_MAX_WORK_GROUP_SIZE, SizeOf(size_t), @sizetValue, nil);
  WriteLn('Max work-group size: ', sizetValue);

  WriteLn('==============================');
end;
```

---

## Bonnes Pratiques

### 1. Gestion de la mémoire

```pascal
// Toujours vérifier et libérer les ressources
procedure SafeGPUCompute;
var
  device: TGPUDevice;
  buffer: cl_mem;
begin
  device := TGPUDevice.Create;
  try
    if not device.Initialize then
      raise Exception.Create('Initialisation GPU échouée');

    buffer := device.CreateBuffer(1024, CL_MEM_READ_WRITE);
    if buffer = nil then
      raise Exception.Create('Création buffer échouée');

    try
      // Utiliser le buffer
      // ...
    finally
      device.ReleaseBuffer(buffer);
    end;

  finally
    device.Free;
  end;
end;
```

### 2. Choix de la taille de work-group

```pascal
function GetOptimalWorkGroupSize(ADevice: cl_device_id;
                                  AKernel: cl_kernel): size_t;
var
  maxSize, preferredMultiple: size_t;
begin
  // Taille maximale
  clGetDeviceInfo(ADevice, CL_DEVICE_MAX_WORK_GROUP_SIZE,
                  SizeOf(size_t), @maxSize, nil);

  // Multiple préféré (pour les warps/wavefronts)
  clGetKernelWorkGroupInfo(AKernel, ADevice,
                           CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE,
                           SizeOf(size_t), @preferredMultiple, nil);

  // Choisir un multiple qui ne dépasse pas le max
  Result := preferredMultiple;
  while Result * 2 <= maxSize do
    Result := Result * 2;
end;
```

### 3. Gestion des erreurs de compilation

```pascal
function CompileWithErrorHandling(ADevice: TGPUDevice;
                                   const ASource, AKernelName: string): TGPUKernel;
var
  err: cl_int;
  buildLog: array[0..16383] of Char;
  logSize: size_t;
begin
  Result := TGPUKernel.Create(ADevice);

  if not Result.CompileFromSource(ASource, AKernelName) then
  begin
    // Récupérer le log de compilation
    err := clGetProgramBuildInfo(Result.FProgram, ADevice.Device,
                                  CL_PROGRAM_BUILD_LOG, 16384,
                                  @buildLog[0], @logSize);

    if err = CL_SUCCESS then
    begin
      WriteLn('Erreur de compilation:');
      WriteLn(string(PChar(@buildLog[0])));
    end;

    FreeAndNil(Result);
  end;
end;
```

---

## Ressources et Documentation

### Documentation officielle

📚 **OpenCL**
- Khronos OpenCL: https://www.khronos.org/opencl/
- OpenCL Reference: https://www.khronos.org/registry/OpenCL/
- Tutoriels: https://github.com/KhronosGroup/OpenCL-Guide

📚 **CUDA**
- NVIDIA CUDA Toolkit: https://developer.nvidia.com/cuda-toolkit
- CUDA C Programming Guide: https://docs.nvidia.com/cuda/cuda-c-programming-guide/
- CUDA Best Practices: https://docs.nvidia.com/cuda/cuda-c-best-practices-guide/

### Outils de développement

🛠️ **Profilers**
- NVIDIA Nsight (CUDA)
- AMD CodeXL (OpenCL)
- Intel VTune (OpenCL)

🛠️ **Debuggers**
- NVIDIA cuda-gdb
- AMD ROCm debugger
- Intel GDB with OpenCL support

### Bibliothèques utiles

**Pour OpenCL :**
- clBLAS - Algèbre linéaire
- clFFT - Transformées de Fourier
- clRNG - Générateurs de nombres aléatoires

**Pour CUDA :**
- cuBLAS - Algèbre linéaire
- cuDNN - Deep Learning
- Thrust - Algorithmes parallèles

---

## Conclusion

### Ce que vous avez appris

Au cours de ce tutoriel, vous avez découvert :

✅ **Fondamentaux du GPU computing**
- Différence CPU vs GPU
- Parallélisme massif
- Architecture SIMT

✅ **OpenCL et CUDA**
- Installation et configuration
- API et concepts clés
- Kernels et exécution

✅ **Implémentation en FreePascal**
- Bindings OpenCL
- Wrapper orienté objet
- Gestion de la mémoire

✅ **Applications pratiques**
- Addition de vecteurs
- Traitement d'images
- Multiplication de matrices
- Fractales (Mandelbrot)

✅ **Optimisation**
- Mémoire locale
- Coalescence
- Vectorisation
- Profiling

✅ **Déploiement multi-plateforme**
- Windows, Linux, macOS
- Sélection automatique du device
- Gestion des erreurs

### Avantages du GPU avec FreePascal

**Performance exceptionnelle**
- 🚀 10-1000× plus rapide pour calculs parallèles
- ⚡ Traitement temps réel d'images/vidéos
- 💪 Deep Learning performant

**Déploiement simplifié**
- 📦 Exécutables natifs avec OpenCL
- 🔧 Pas de dépendances Python
- 💾 Code compilé optimisé

**Portabilité**
- 🪟 Windows (NVIDIA, AMD, Intel)
- 🐧 Linux (drivers open source)
- 🍎 macOS (Metal + OpenCL)

### Quand utiliser le GPU ?

**✓ Parfait pour :**
- Traitement d'images/vidéos en masse
- Deep Learning (inférence)
- Simulations scientifiques
- Calculs mathématiques vectoriels
- Cryptographie (hashing, signatures)
- Rendu 3D temps réel

**✗ Moins adapté pour :**
- Petites quantités de données (<10k éléments)
- Algorithmes séquentiels
- Beaucoup de branchements
- Accès mémoire très irréguliers

### Comparaison finale

| Aspect | CPU | GPU | GPU + FreePascal |
|--------|-----|-----|------------------|
| **Performance séquentielle** | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐ |
| **Performance parallèle** | ⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Facilité développement** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Déploiement** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Portabilité** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |

### Prochaines étapes

**Pour aller plus loin :**

1. **Approfondir OpenCL**
   - Mémoire partagée avancée
   - Pipes et SVM (Shared Virtual Memory)
   - SPIR-V

2. **Explorer CUDA**
   - Unified Memory
   - Streams et concurrence
   - Tensor Cores (deep learning)

3. **Bibliothèques spécialisées**
   - Intégrer clBLAS/cuBLAS
   - Utiliser clFFT pour traitement du signal
   - TensorFlow/PyTorch avec GPU

4. **Applications avancées**
   - Ray tracing temps réel
   - Simulations physiques
   - Réseaux de neurones from scratch
   - Computer vision temps réel

### Message final

Le GPU computing avec FreePascal/Lazarus vous offre le **meilleur des deux mondes** :

- La **puissance** du calcul parallèle GPU
- La **simplicité** et **performance** de FreePascal
- Le **déploiement facile** sans dépendances lourdes

Que ce soit pour :
- Accélérer vos applications de traitement d'images
- Créer des simulations scientifiques performantes
- Déployer des modèles d'IA en production
- Développer des jeux avec physique réaliste

Le GPU est votre allié, et FreePascal vous permet de l'exploiter pleinement !

**Le calcul parallèle n'est plus réservé aux super-ordinateurs !** Avec un simple GPU de gaming et FreePascal, vous pouvez accomplir des performances qui auraient nécessité un cluster entier il y a quelques années.

Bon computing parallèle et bonnes optimisations ! 🚀💻⚡

---

*Fin du tutoriel 15.9 - GPU computing avec CUDA/OpenCL*

**Prochaine étape recommandée :** Combiner GPU computing avec deep learning, vision par ordinateur et ONNX pour créer des applications d'IA ultra-performantes !

⏭️ [Déploiement de modèles IA](/15-intelligence-artificielle-machine-learning/10-deploiement-modeles-ia.md)
