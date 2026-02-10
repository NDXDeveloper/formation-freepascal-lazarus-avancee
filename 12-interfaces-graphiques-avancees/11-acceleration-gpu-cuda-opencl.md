🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.11 Accélération GPU (CUDA/OpenCL)

## Introduction

### Qu'est-ce que l'accélération GPU ?

Le **GPU** (Graphics Processing Unit - Processeur graphique) n'est pas seulement utilisé pour afficher des images. Il contient des milliers de petits cœurs de calcul capables d'effectuer des opérations en parallèle massivement. Cette architecture le rend idéal pour :

- Le traitement d'images (chaque pixel peut être traité simultanément)
- Les calculs scientifiques
- L'apprentissage automatique
- La simulation physique
- Le rendu graphique avancé

**Comparaison CPU vs GPU :**

| Critère | CPU | GPU |
|---------|-----|-----|
| Nombre de cœurs | 4-16 (typique) | 1000-10000+ |
| Vitesse par cœur | Très rapide | Plus lente |
| Mémoire | Cache rapide | Grande mémoire dédiée |
| Usage optimal | Calculs séquentiels complexes | Calculs parallèles massifs |
| Gain typique | 1x (référence) | 10x à 100x |

### CUDA vs OpenCL

Il existe deux technologies principales pour programmer les GPU :

#### CUDA (Compute Unified Device Architecture)

- **Fabricant** : NVIDIA exclusivement
- **Avantages** :
  - Performance maximale sur cartes NVIDIA
  - Écosystème mature et riche
  - Nombreuses bibliothèques optimisées
  - Excellente documentation
- **Inconvénients** :
  - Fonctionne uniquement sur GPU NVIDIA
  - Propriétaire (pas open source)

#### OpenCL (Open Computing Language)

- **Fabricant** : Standard ouvert (Khronos Group)
- **Avantages** :
  - Multi-plateforme : NVIDIA, AMD, Intel
  - Open source
  - Fonctionne aussi sur CPU
  - Portable entre différents matériels
- **Inconvénients** :
  - Parfois moins performant que CUDA
  - Plus complexe à configurer
  - Moins de bibliothèques disponibles

**Choix recommandé :**
- Si vous avez une carte NVIDIA et privilégiez la performance → **CUDA**
- Si vous voulez un code portable sur différentes cartes → **OpenCL**
- Pour un développement multi-plateforme Windows/Ubuntu → **OpenCL** est plus universel

## Vérification du matériel

### Détecter les capacités GPU

Avant de commencer, il faut vérifier si votre système supporte l'accélération GPU.

#### Sur Windows

```pascal
uses
  Windows, Classes, SysUtils;

procedure DetectGPUWindows;
var
  regKey: HKEY;
  deviceName: string;
  bufferSize: DWORD;
  buffer: array[0..255] of Char;
begin
  WriteLn('=== Détection GPU Windows ===');

  // Vérifier dans le registre
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE,
    'SYSTEM\CurrentControlSet\Control\Class\{4d36e968-e325-11ce-bfc1-08002be10318}\0000',
    0, KEY_READ, regKey) = ERROR_SUCCESS then
  begin
    bufferSize := SizeOf(buffer);
    if RegQueryValueEx(regKey, 'DriverDesc', nil, nil,
      @buffer, @bufferSize) = ERROR_SUCCESS then
    begin
      deviceName := StrPas(buffer);
      WriteLn('GPU détecté : ', deviceName);

      // Détecter le fabricant
      if Pos('NVIDIA', UpperCase(deviceName)) > 0 then
        WriteLn('Fabricant : NVIDIA - CUDA disponible')
      else if Pos('AMD', UpperCase(deviceName)) > 0 then
        WriteLn('Fabricant : AMD - OpenCL disponible')
      else if Pos('INTEL', UpperCase(deviceName)) > 0 then
        WriteLn('Fabricant : Intel - OpenCL disponible');
    end;
    RegCloseKey(regKey);
  end;
end;

// Alternative : via WMI
procedure DetectGPUWMI;
var
  query: string;
begin
  query := 'wmic path win32_VideoController get name,driverversion';
  // Exécuter la requête WMI
  // (Utiliser TProcess ou ShellExecute)
end;
```

#### Sur Ubuntu/Linux

```pascal
uses
  Process, Classes, SysUtils, Math;

procedure DetectGPULinux;
var
  outputList: TStringList;
  output: string;
  i: Integer;
begin
  WriteLn('=== Détection GPU Linux ===');
  outputList := TStringList.Create;
  try
    // Méthode 1 : lspci
    if RunCommand('lspci', ['-v'], output) then
    begin
      outputList.Text := output;
      for i := 0 to outputList.Count - 1 do
      begin
        if Pos('VGA', outputList[i]) > 0 then
          WriteLn('GPU : ', outputList[i]);
      end;
    end;

    // Méthode 2 : nvidia-smi pour cartes NVIDIA
    WriteLn;
    WriteLn('--- Informations NVIDIA ---');
    if RunCommand('nvidia-smi', ['--query-gpu=name,driver_version',
      '--format=csv,noheader'], output) then
      WriteLn(output)
    else
      WriteLn('nvidia-smi non disponible (pas de GPU NVIDIA ou driver non installé)');

    // Méthode 3 : clinfo pour OpenCL
    WriteLn;
    WriteLn('--- Informations OpenCL ---');
    if RunCommand('clinfo', [], output) then
    begin
      outputList.Text := output;
      for i := 0 to Min(10, outputList.Count - 1) do
        WriteLn(outputList[i]);
    end
    else
      WriteLn('clinfo non disponible');

  finally
    outputList.Free;
  end;
end;
```

### Vérification programmatique des capacités

```pascal
uses
  {$IFDEF USE_CUDA}
  cuda_runtime_api,
  {$ENDIF}
  {$IFDEF USE_OPENCL}
  cl, cl_platform,
  {$ENDIF}
  SysUtils;

function CheckCUDAAvailable: Boolean;
{$IFDEF USE_CUDA}
var
  deviceCount: Integer;
  prop: cudaDeviceProp;
begin
  Result := False;

  if cudaGetDeviceCount(@deviceCount) = cudaSuccess then
  begin
    WriteLn(Format('Nombre de GPU CUDA : %d', [deviceCount]));

    if deviceCount > 0 then
    begin
      cudaGetDeviceProperties(@prop, 0);
      WriteLn(Format('GPU 0 : %s', [prop.name]));
      WriteLn(Format('Compute Capability : %d.%d',
        [prop.major, prop.minor]));
      WriteLn(Format('Mémoire globale : %.2f GB',
        [prop.totalGlobalMem / (1024*1024*1024)]));
      Result := True;
    end;
  end
  else
    WriteLn('CUDA non disponible');
end;
{$ELSE}
begin
  Result := False;
  WriteLn('Application compilée sans support CUDA');
end;
{$ENDIF}

function CheckOpenCLAvailable: Boolean;
{$IFDEF USE_OPENCL}
var
  numPlatforms: cl_uint;
  platforms: array of cl_platform_id;
  platformName: array[0..255] of Char;
  i: Integer;
  err: cl_int;
begin
  Result := False;

  err := clGetPlatformIDs(0, nil, @numPlatforms);
  if (err = CL_SUCCESS) and (numPlatforms > 0) then
  begin
    WriteLn(Format('Plateformes OpenCL trouvées : %d', [numPlatforms]));

    SetLength(platforms, numPlatforms);
    clGetPlatformIDs(numPlatforms, @platforms[0], nil);

    for i := 0 to numPlatforms - 1 do
    begin
      clGetPlatformInfo(platforms[i], CL_PLATFORM_NAME,
        SizeOf(platformName), @platformName, nil);
      WriteLn(Format('  Plateforme %d : %s', [i, StrPas(platformName)]));
    end;

    Result := True;
  end
  else
    WriteLn('OpenCL non disponible');
end;
{$ELSE}
begin
  Result := False;
  WriteLn('Application compilée sans support OpenCL');
end;
{$ENDIF}
```

## Installation et configuration

### Installation CUDA (NVIDIA)

#### Sur Windows

1. **Vérifier la compatibilité** :
   - Carte NVIDIA avec Compute Capability ≥ 3.0
   - Driver NVIDIA à jour

2. **Télécharger CUDA Toolkit** :
   - Site officiel : https://developer.nvidia.com/cuda-downloads
   - Choisir la version compatible avec votre OS

3. **Installation** :
   ```batch
   # Installer CUDA Toolkit
   cuda_X.X.X_win10.exe

   # Vérifier l'installation
   nvcc --version
   nvidia-smi
   ```

4. **Variables d'environnement** :
   ```
   CUDA_PATH = C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\vX.X
   PATH += %CUDA_PATH%\bin
   ```

5. **Configuration FreePascal** :
   ```pascal
   // Dans votre projet
   {$IFDEF WINDOWS}
   const
     CUDA_LIB_PATH = 'C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.0\lib\x64\';
   {$L CUDA_LIB_PATH + 'cudart.lib'}
   {$ENDIF}
   ```

#### Sur Ubuntu/Linux

```bash
# 1. Vérifier le GPU
lspci | grep -i nvidia

# 2. Ajouter le dépôt NVIDIA
wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2204/x86_64/cuda-keyring_1.0-1_all.deb
sudo dpkg -i cuda-keyring_1.0-1_all.deb

# 3. Mettre à jour et installer
sudo apt update
sudo apt install cuda

# 4. Configurer les variables d'environnement
echo 'export PATH=/usr/local/cuda/bin:$PATH' >> ~/.bashrc
echo 'export LD_LIBRARY_PATH=/usr/local/cuda/lib64:$LD_LIBRARY_PATH' >> ~/.bashrc
source ~/.bashrc

# 5. Vérifier l'installation
nvcc --version
nvidia-smi
```

### Installation OpenCL

#### Sur Windows

OpenCL est généralement inclus dans les drivers graphiques modernes.

```batch
# Vérifier OpenCL
# Télécharger et installer GPU Caps Viewer
# Ou utiliser clinfo

# Si besoin d'installer le SDK
# Intel : https://software.intel.com/content/www/us/en/develop/tools/opencl-sdk.html
# AMD : https://github.com/GPUOpen-LibrariesAndSDKs/OCL-SDK/releases
```

Configuration FreePascal :
```pascal
{$IFDEF WINDOWS}
const
  OPENCL_LIB = 'OpenCL.dll';
{$ENDIF}
```

#### Sur Ubuntu/Linux

```bash
# 1. Installer les headers OpenCL
sudo apt update
sudo apt install opencl-headers

# 2. Installer le runtime selon votre GPU

# Pour NVIDIA :
sudo apt install nvidia-opencl-dev

# Pour AMD :
sudo apt install mesa-opencl-icd

# Pour Intel :
sudo apt install intel-opencl-icd

# 3. Installer les outils de diagnostic
sudo apt install clinfo

# 4. Vérifier l'installation
clinfo

# 5. Configuration FreePascal
# Les bibliothèques sont généralement dans /usr/lib/x86_64-linux-gnu/
```

## Utilisation de CUDA avec FreePascal

### Bindings CUDA

Pour utiliser CUDA, vous avez besoin de bindings Pascal. Plusieurs options existent :

1. **Bindings manuels** : Créer les déclarations vous-même
2. **Bibliothèques existantes** : Utiliser des projets communautaires

```pascal
unit cuda_types;

interface

const
  {$IFDEF WINDOWS}
  CUDA_DLL = 'cudart64_12.dll';
  {$ELSE}
  CUDA_DLL = 'libcudart.so';
  {$ENDIF}

type
  cudaError_t = Integer;
  cudaStream_t = Pointer;

const
  cudaSuccess = 0;
  cudaErrorMemoryAllocation = 2;

// Déclarations de fonctions CUDA
function cudaMalloc(var devPtr: Pointer; size: NativeUInt): cudaError_t; cdecl; external CUDA_DLL;
function cudaFree(devPtr: Pointer): cudaError_t; cdecl; external CUDA_DLL;
function cudaMemcpy(dst, src: Pointer; count: NativeUInt; kind: Integer): cudaError_t; cdecl; external CUDA_DLL;

const
  cudaMemcpyHostToDevice = 1;
  cudaMemcpyDeviceToHost = 2;
  cudaMemcpyDeviceToDevice = 3;

implementation

end.
```

### Exemple simple : Addition de vecteurs

#### 1. Code CUDA (kernel)

Créer un fichier `vector_add.cu` :

```cuda
// vector_add.cu
extern "C"
__global__ void vectorAdd(const float *a, const float *b, float *c, int n)
{
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx < n)
        c[idx] = a[idx] + b[idx];
}
```

Compiler le kernel :
```bash
nvcc -ptx vector_add.cu -o vector_add.ptx
```

#### 2. Code Pascal

```pascal
program VectorAddCUDA;

uses
  SysUtils, cuda_types;

const
  N = 1000000; // 1 million d'éléments

type
  TFloatArray = array[0..N-1] of Single;
  PFloatArray = ^TFloatArray;

procedure VectorAddCPU(const a, b: TFloatArray; var c: TFloatArray);
var
  i: Integer;
begin
  for i := 0 to N - 1 do
    c[i] := a[i] + b[i];
end;

procedure VectorAddGPU(const a, b: TFloatArray; var c: TFloatArray);
var
  d_a, d_b, d_c: Pointer;
  size: NativeUInt;
  threadsPerBlock, blocksPerGrid: Integer;
  startTime, endTime: TDateTime;
begin
  size := N * SizeOf(Single);

  WriteLn('Allocation mémoire GPU...');

  // Allouer la mémoire sur le GPU
  if cudaMalloc(d_a, size) <> cudaSuccess then
    raise Exception.Create('Erreur allocation d_a');
  if cudaMalloc(d_b, size) <> cudaSuccess then
    raise Exception.Create('Erreur allocation d_b');
  if cudaMalloc(d_c, size) <> cudaSuccess then
    raise Exception.Create('Erreur allocation d_c');

  try
    WriteLn('Copie des données vers le GPU...');
    startTime := Now;

    // Copier les données vers le GPU
    cudaMemcpy(d_a, @a, size, cudaMemcpyHostToDevice);
    cudaMemcpy(d_b, @b, size, cudaMemcpyHostToDevice);

    // Configuration du lancement
    threadsPerBlock := 256;
    blocksPerGrid := (N + threadsPerBlock - 1) div threadsPerBlock;

    WriteLn(Format('Lancement kernel : %d blocs de %d threads',
      [blocksPerGrid, threadsPerBlock]));

    // Lancer le kernel (via un wrapper - voir ci-dessous)
    LaunchVectorAddKernel(d_a, d_b, d_c, N, blocksPerGrid, threadsPerBlock);

    // Attendre la fin
    cudaDeviceSynchronize;

    // Copier le résultat
    cudaMemcpy(@c, d_c, size, cudaMemcpyDeviceToHost);

    endTime := Now;
    WriteLn(Format('Temps GPU : %.3f ms',
      [(endTime - startTime) * 24 * 60 * 60 * 1000]));

  finally
    // Libérer la mémoire GPU
    cudaFree(d_a);
    cudaFree(d_b);
    cudaFree(d_c);
  end;
end;

procedure BenchmarkComparison;
var
  a, b, c_cpu, c_gpu: PFloatArray;
  i: Integer;
  startTime, endTime: TDateTime;
  allCorrect: Boolean;
begin
  WriteLn('=== Benchmark Addition de Vecteurs ===');
  WriteLn(Format('Taille : %d éléments (%.2f MB)',
    [N, (N * SizeOf(Single)) / (1024*1024)]));
  WriteLn;

  // Allouer la mémoire
  GetMem(a, N * SizeOf(Single));
  GetMem(b, N * SizeOf(Single));
  GetMem(c_cpu, N * SizeOf(Single));
  GetMem(c_gpu, N * SizeOf(Single));

  try
    // Initialiser les données
    WriteLn('Initialisation des données...');
    for i := 0 to N - 1 do
    begin
      a^[i] := i * 1.0;
      b^[i] := i * 2.0;
    end;

    // Version CPU
    WriteLn('Calcul CPU...');
    startTime := Now;
    VectorAddCPU(a^, b^, c_cpu^);
    endTime := Now;
    WriteLn(Format('Temps CPU : %.3f ms',
      [(endTime - startTime) * 24 * 60 * 60 * 1000]));
    WriteLn;

    // Version GPU
    WriteLn('Calcul GPU...');
    VectorAddGPU(a^, b^, c_gpu^);
    WriteLn;

    // Vérifier les résultats
    WriteLn('Vérification des résultats...');
    allCorrect := True;
    for i := 0 to N - 1 do
    begin
      if Abs(c_cpu^[i] - c_gpu^[i]) > 0.001 then
      begin
        allCorrect := False;
        WriteLn(Format('Erreur à l''index %d : CPU=%.2f, GPU=%.2f',
          [i, c_cpu^[i], c_gpu^[i]]));
        Break;
      end;
    end;

    if allCorrect then
      WriteLn('✓ Tous les résultats sont corrects !')
    else
      WriteLn('✗ Erreurs détectées dans les résultats');

  finally
    FreeMem(a);
    FreeMem(b);
    FreeMem(c_cpu);
    FreeMem(c_gpu);
  end;
end;

begin
  try
    if not CheckCUDAAvailable then
    begin
      WriteLn('CUDA n''est pas disponible sur ce système');
      Exit;
    end;

    BenchmarkComparison;

    WriteLn;
    WriteLn('Appuyez sur Entrée pour quitter...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('ERREUR : ', E.Message);
      ReadLn;
    end;
  end;
end.
```

### Wrapper pour lancer le kernel

En pratique, lancer un kernel CUDA depuis Pascal nécessite un wrapper C++ :

```cpp
// cuda_wrapper.cu
#include <cuda_runtime.h>

extern "C" {
    // Déclaration du kernel
    __global__ void vectorAddKernel(const float *a, const float *b,
                                    float *c, int n)
    {
        int idx = blockIdx.x * blockDim.x + threadIdx.x;
        if (idx < n)
            c[idx] = a[idx] + b[idx];
    }

    // Fonction wrapper pour Pascal
    void launchVectorAdd(const float *d_a, const float *d_b, float *d_c,
                         int n, int blocks, int threads)
    {
        vectorAddKernel<<<blocks, threads>>>(d_a, d_b, d_c, n);
    }
}
```

Compiler :
```bash
nvcc -shared -o cuda_wrapper.dll cuda_wrapper.cu  # Windows
nvcc -shared -fPIC -o libcuda_wrapper.so cuda_wrapper.cu  # Linux
```

Déclaration Pascal :
```pascal
{$IFDEF WINDOWS}
const CUDA_WRAPPER = 'cuda_wrapper.dll';
{$ELSE}
const CUDA_WRAPPER = 'libcuda_wrapper.so';
{$ENDIF}

procedure LaunchVectorAddKernel(d_a, d_b, d_c: Pointer; n, blocks, threads: Integer);
  cdecl; external CUDA_WRAPPER name 'launchVectorAdd';
```

## Utilisation d'OpenCL avec FreePascal

### Bindings OpenCL

```pascal
unit opencl_base;

interface

const
  {$IFDEF WINDOWS}
  OPENCL_LIB = 'OpenCL.dll';
  {$ELSE}
  OPENCL_LIB = 'libOpenCL.so';
  {$ENDIF}

type
  cl_int = Integer;
  cl_uint = Cardinal;
  cl_ulong = UInt64;
  size_t = NativeUInt;

  cl_platform_id = Pointer;
  cl_device_id = Pointer;
  cl_context = Pointer;
  cl_command_queue = Pointer;
  cl_mem = Pointer;
  cl_program = Pointer;
  cl_kernel = Pointer;

const
  CL_SUCCESS = 0;
  CL_DEVICE_TYPE_GPU = 1 shl 2;
  CL_MEM_READ_ONLY = 1 shl 2;
  CL_MEM_WRITE_ONLY = 1 shl 1;
  CL_MEM_READ_WRITE = 1 shl 0;

// Fonctions OpenCL
function clGetPlatformIDs(num_entries: cl_uint; platforms: Pointer;
  num_platforms: Pcl_uint): cl_int; cdecl; external OPENCL_LIB;

function clGetDeviceIDs(platform: cl_platform_id; device_type: cl_uint;
  num_entries: cl_uint; devices: Pointer; num_devices: Pcl_uint): cl_int;
  cdecl; external OPENCL_LIB;

function clCreateContext(properties: Pointer; num_devices: cl_uint;
  devices: Pointer; pfn_notify, user_data: Pointer; errcode_ret: Pcl_int): cl_context;
  cdecl; external OPENCL_LIB;

function clCreateCommandQueue(context: cl_context; device: cl_device_id;
  properties: cl_ulong; errcode_ret: Pcl_int): cl_command_queue;
  cdecl; external OPENCL_LIB;

function clCreateBuffer(context: cl_context; flags: cl_ulong; size: size_t;
  host_ptr: Pointer; errcode_ret: Pcl_int): cl_mem;
  cdecl; external OPENCL_LIB;

function clEnqueueWriteBuffer(command_queue: cl_command_queue; buffer: cl_mem;
  blocking_write: cl_uint; offset, size: size_t; ptr: Pointer;
  num_events_in_wait_list: cl_uint; event_wait_list, event: Pointer): cl_int;
  cdecl; external OPENCL_LIB;

function clEnqueueReadBuffer(command_queue: cl_command_queue; buffer: cl_mem;
  blocking_read: cl_uint; offset, size: size_t; ptr: Pointer;
  num_events_in_wait_list: cl_uint; event_wait_list, event: Pointer): cl_int;
  cdecl; external OPENCL_LIB;

function clCreateProgramWithSource(context: cl_context; count: cl_uint;
  strings: PPAnsiChar; lengths: Psize_t; errcode_ret: Pcl_int): cl_program;
  cdecl; external OPENCL_LIB;

function clBuildProgram(program_: cl_program; num_devices: cl_uint;
  device_list: Pointer; options: PAnsiChar; pfn_notify, user_data: Pointer): cl_int;
  cdecl; external OPENCL_LIB;

function clCreateKernel(program_: cl_program; kernel_name: PAnsiChar;
  errcode_ret: Pcl_int): cl_kernel;
  cdecl; external OPENCL_LIB;

function clSetKernelArg(kernel: cl_kernel; arg_index: cl_uint;
  arg_size: size_t; arg_value: Pointer): cl_int;
  cdecl; external OPENCL_LIB;

function clEnqueueNDRangeKernel(command_queue: cl_command_queue; kernel: cl_kernel;
  work_dim: cl_uint; global_work_offset, global_work_size, local_work_size: Psize_t;
  num_events_in_wait_list: cl_uint; event_wait_list, event: Pointer): cl_int;
  cdecl; external OPENCL_LIB;

function clFinish(command_queue: cl_command_queue): cl_int;
  cdecl; external OPENCL_LIB;

function clReleaseMemObject(memobj: cl_mem): cl_int;
  cdecl; external OPENCL_LIB;

function clReleaseKernel(kernel: cl_kernel): cl_int;
  cdecl; external OPENCL_LIB;

function clReleaseProgram(program_: cl_program): cl_int;
  cdecl; external OPENCL_LIB;

function clReleaseCommandQueue(command_queue: cl_command_queue): cl_int;
  cdecl; external OPENCL_LIB;

function clReleaseContext(context: cl_context): cl_int;
  cdecl; external OPENCL_LIB;

implementation

end.
```

### Exemple complet OpenCL

```pascal
program VectorAddOpenCL;

uses
  SysUtils, opencl_base;

const
  // Kernel OpenCL en tant que chaîne
  KERNEL_SOURCE =
    '__kernel void vector_add(__global const float *a, ' +
    '                         __global const float *b, ' +
    '                         __global float *c) ' +
    '{ ' +
    '    int gid = get_global_id(0); ' +
    '    c[gid] = a[gid] + b[gid]; ' +
    '}';

type
  TOpenCLContext = class
  private
    FPlatform: cl_platform_id;
    FDevice: cl_device_id;
    FContext: cl_context;
    FQueue: cl_command_queue;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateBuffer(size: NativeUInt; flags: cl_ulong): cl_mem;
    function CreateProgram(const source: string): cl_program;
    function CreateKernel(prog: cl_program; const name: string): cl_kernel;
    procedure WriteBuffer(buffer: cl_mem; size: NativeUInt; data: Pointer);
    procedure ReadBuffer(buffer: cl_mem; size: NativeUInt; data: Pointer);
    procedure ExecuteKernel(kernel: cl_kernel; globalSize, localSize: NativeUInt);
    procedure Finish;
    property Queue: cl_command_queue read FQueue;
  end;

constructor TOpenCLContext.Create;
var
  err: cl_int;
  numPlatforms, numDevices: cl_uint;
begin
  inherited;

  // Obtenir la plateforme
  err := clGetPlatformIDs(1, @FPlatform, @numPlatforms);
  if (err <> CL_SUCCESS) or (numPlatforms = 0) then
    raise Exception.Create('Aucune plateforme OpenCL trouvée');

  // Obtenir le device GPU
  err := clGetDeviceIDs(FPlatform, CL_DEVICE_TYPE_GPU, 1, @FDevice, @numDevices);
  if (err <> CL_SUCCESS) or (numDevices = 0) then
    raise Exception.Create('Aucun GPU OpenCL trouvé');

  // Créer le contexte
  FContext := clCreateContext(nil, 1, @FDevice, nil, nil, @err);
  if err <> CL_SUCCESS then
    raise Exception.Create('Erreur création contexte OpenCL');

  // Créer la queue de commandes
  FQueue := clCreateCommandQueue(FContext, FDevice, 0, @err);
  if err <> CL_SUCCESS then
    raise Exception.Create('Erreur création queue OpenCL');

  WriteLn('Contexte OpenCL créé avec succès');
end;

destructor TOpenCLContext.Destroy;
begin
  if FQueue <> nil then
    clReleaseCommandQueue(FQueue);
  if FContext <> nil then
    clReleaseContext(FContext);
  inherited;
end;

function TOpenCLContext.CreateBuffer(size: NativeUInt; flags: cl_ulong): cl_mem;
var
  err: cl_int;
begin
  Result := clCreateBuffer(FContext, flags, size, nil, @err);
  if err <> CL_SUCCESS then
    raise Exception.Create(Format('Erreur création buffer : %d', [err]));
end;

function TOpenCLContext.CreateProgram(const source: string): cl_program;
var
  err: cl_int;
  sourcePtr: PAnsiChar;
  sourceLen: size_t;
begin
  sourcePtr := PAnsiChar(AnsiString(source));
  sourceLen := Length(source);

  Result := clCreateProgramWithSource(FContext, 1, @sourcePtr, @sourceLen, @err);
  if err <> CL_SUCCESS then
    raise Exception.Create('Erreur création programme OpenCL');

  // Compiler le programme
  err := clBuildProgram(Result, 1, @FDevice, nil, nil, nil);
  if err <> CL_SUCCESS then
    raise Exception.Create('Erreur compilation programme OpenCL');
end;

function TOpenCLContext.CreateKernel(prog: cl_program; const name: string): cl_kernel;
var
  err: cl_int;
begin
  Result := clCreateKernel(prog, PAnsiChar(AnsiString(name)), @err);
  if err <> CL_SUCCESS then
    raise Exception.Create(Format('Erreur création kernel "%s"', [name]));
end;

procedure TOpenCLContext.WriteBuffer(buffer: cl_mem; size: NativeUInt; data: Pointer);
var
  err: cl_int;
begin
  err := clEnqueueWriteBuffer(FQueue, buffer, 1, 0, size, data, 0, nil, nil);
  if err <> CL_SUCCESS then
    raise Exception.Create('Erreur écriture buffer');
end;

procedure TOpenCLContext.ReadBuffer(buffer: cl_mem; size: NativeUInt; data: Pointer);
var
  err: cl_int;
begin
  err := clEnqueueReadBuffer(FQueue, buffer, 1, 0, size, data, 0, nil, nil);
  if err <> CL_SUCCESS then
    raise Exception.Create('Erreur lecture buffer');
end;

procedure TOpenCLContext.ExecuteKernel(kernel: cl_kernel; globalSize, localSize: NativeUInt);
var
  err: cl_int;
  global, local: size_t;
begin
  global := globalSize;
  local := localSize;

  err := clEnqueueNDRangeKernel(FQueue, kernel, 1, nil, @global, @local, 0, nil, nil);
  if err <> CL_SUCCESS then
    raise Exception.Create(Format('Erreur exécution kernel : %d', [err]));
end;

procedure TOpenCLContext.Finish;
begin
  clFinish(FQueue);
end;

// Programme principal
procedure VectorAddWithOpenCL;
const
  N = 1000000;
type
  TFloatArray = array[0..N-1] of Single;
var
  ctx: TOpenCLContext;
  a, b, c: TFloatArray;
  d_a, d_b, d_c: cl_mem;
  program_: cl_program;
  kernel: cl_kernel;
  i: Integer;
  size: NativeUInt;
  startTime, endTime: TDateTime;
  globalSize, localSize: NativeUInt;
begin
  WriteLn('=== Addition de vecteurs avec OpenCL ===');
  WriteLn;

  // Initialiser les données
  WriteLn('Initialisation des données...');
  for i := 0 to N - 1 do
  begin
    a[i] := i * 1.0;
    b[i] := i * 2.0;
  end;

  ctx := TOpenCLContext.Create;
  try
    size := N * SizeOf(Single);

    // Créer les buffers
    WriteLn('Création des buffers GPU...');
    d_a := ctx.CreateBuffer(size, CL_MEM_READ_ONLY);
    d_b := ctx.CreateBuffer(size, CL_MEM_READ_ONLY);
    d_c := ctx.CreateBuffer(size, CL_MEM_WRITE_ONLY);

    try
      // Copier les données vers le GPU
      WriteLn('Copie des données vers le GPU...');
      startTime := Now;
      ctx.WriteBuffer(d_a, size, @a);
      ctx.WriteBuffer(d_b, size, @b);

      // Créer et compiler le programme
      WriteLn('Compilation du kernel...');
      program_ := ctx.CreateProgram(KERNEL_SOURCE);
      try
        // Créer le kernel
        kernel := ctx.CreateKernel(program_, 'vector_add');
        try
          // Définir les arguments
          clSetKernelArg(kernel, 0, SizeOf(cl_mem), @d_a);
          clSetKernelArg(kernel, 1, SizeOf(cl_mem), @d_b);
          clSetKernelArg(kernel, 2, SizeOf(cl_mem), @d_c);

          // Exécuter le kernel
          WriteLn('Exécution du kernel...');
          globalSize := N;
          localSize := 256;
          ctx.ExecuteKernel(kernel, globalSize, localSize);

          // Attendre la fin
          ctx.Finish;

          // Lire le résultat
          ctx.ReadBuffer(d_c, size, @c);

          endTime := Now;
          WriteLn(Format('Temps total GPU : %.3f ms',
            [(endTime - startTime) * 24 * 60 * 60 * 1000]));

          // Vérifier quelques résultats
          WriteLn;
          WriteLn('Vérification (premiers éléments) :');
          for i := 0 to 9 do
            WriteLn(Format('c[%d] = %.1f + %.1f = %.1f',
              [i, a[i], b[i], c[i]]));

        finally
          clReleaseKernel(kernel);
        end;
      finally
        clReleaseProgram(program_);
      end;
    finally
      clReleaseMemObject(d_a);
      clReleaseMemObject(d_b);
      clReleaseMemObject(d_c);
    end;
  finally
    ctx.Free;
  end;

  WriteLn;
  WriteLn('Terminé !');
end;

begin
  try
    VectorAddWithOpenCL;
    WriteLn;
    WriteLn('Appuyez sur Entrée pour quitter...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('ERREUR : ', E.Message);
      ReadLn;
    end;
  end;
end.
```

## Accélération GPU avec OpenCV

OpenCV inclut un module GPU optimisé qui utilise CUDA ou OpenCL automatiquement.

### Configuration OpenCV avec GPU

```pascal
uses
  ocv.core, ocv.imgproc, ocv.highgui;

procedure ConfigureOpenCVGPU;
begin
  {$IFDEF USE_CUDA}
  // Vérifier CUDA
  if cv.cuda.getCudaEnabledDeviceCount > 0 then
  begin
    WriteLn(Format('CUDA disponible : %d GPU(s)',
      [cv.cuda.getCudaEnabledDeviceCount]));
    cv.cuda.setDevice(0); // Utiliser le premier GPU
  end
  else
    WriteLn('CUDA non disponible');
  {$ENDIF}

  {$IFDEF USE_OPENCL}
  // Vérifier OpenCL
  if cv.ocl.haveOpenCL then
  begin
    WriteLn('OpenCL disponible');
    cv.ocl.setUseOpenCL(True);

    // Afficher les infos
    WriteLn('Device : ', cv.ocl.Device_getDefault.name);
  end
  else
    WriteLn('OpenCL non disponible');
  {$ENDIF}
end;
```

### Exemple : Traitement d'image accéléré

```pascal
program ImageProcessingGPU;

uses
  SysUtils, DateUtils,
  ocv.core, ocv.imgproc, ocv.highgui;

procedure ProcessImageCPU(const src: TMat; var dst: TMat);
var
  temp: TMat;
begin
  // Flou gaussien
  GaussianBlur(src, temp, Size(15, 15), 0);

  // Détection de contours
  Canny(temp, dst, 50, 150);

  temp.release;
end;

{$IFDEF USE_CUDA}
procedure ProcessImageGPU_CUDA(const src: TMat; var dst: TMat);
var
  d_src, d_temp, d_dst: TGpuMat;
begin
  // Uploader vers le GPU
  d_src.upload(src);

  // Traitement sur GPU
  cv.cuda.GaussianBlur(d_src, d_temp, Size(15, 15), 0);
  cv.cuda.Canny(d_temp, d_dst, 50, 150);

  // Télécharger le résultat
  d_dst.download(dst);

  // Libérer
  d_src.release;
  d_temp.release;
  d_dst.release;
end;
{$ENDIF}

{$IFDEF USE_OPENCL}
procedure ProcessImageGPU_OpenCL(const src: TMat; var dst: TMat);
var
  u_src, u_temp, u_dst: TUMat;
begin
  // Convertir en UMat (Unified Memory)
  src.copyTo(u_src);

  // Traitement (automatiquement sur GPU si disponible)
  cv.GaussianBlur(u_src, u_temp, Size(15, 15), 0);
  cv.Canny(u_temp, u_dst, 50, 150);

  // Récupérer le résultat
  u_dst.copyTo(dst);

  u_src.release;
  u_temp.release;
  u_dst.release;
end;
{$ENDIF}

procedure BenchmarkImageProcessing;
var
  src, dst_cpu, dst_gpu: TMat;
  startTime, endTime: TDateTime;
  timeCPU, timeGPU: Double;
begin
  WriteLn('=== Benchmark Traitement d''Image ===');
  WriteLn;

  // Charger une image
  src := imread('test_image.jpg');
  if src.empty then
  begin
    WriteLn('Erreur : impossible de charger l''image');
    Exit;
  end;

  WriteLn(Format('Image : %dx%d pixels', [src.cols, src.rows]));
  WriteLn;

  try
    // Traitement CPU
    WriteLn('Traitement CPU...');
    startTime := Now;
    ProcessImageCPU(src, dst_cpu);
    endTime := Now;
    timeCPU := (endTime - startTime) * 24 * 60 * 60 * 1000;
    WriteLn(Format('Temps CPU : %.2f ms', [timeCPU]));
    imwrite('result_cpu.jpg', dst_cpu);

    WriteLn;

    // Traitement GPU
    {$IFDEF USE_CUDA}
    WriteLn('Traitement GPU (CUDA)...');
    startTime := Now;
    ProcessImageGPU_CUDA(src, dst_gpu);
    endTime := Now;
    timeGPU := (endTime - startTime) * 24 * 60 * 60 * 1000;
    WriteLn(Format('Temps GPU : %.2f ms', [timeGPU]));
    WriteLn(Format('Accélération : %.2fx', [timeCPU / timeGPU]));
    imwrite('result_gpu.jpg', dst_gpu);
    {$ENDIF}

    {$IFDEF USE_OPENCL}
    WriteLn('Traitement GPU (OpenCL)...');
    startTime := Now;
    ProcessImageGPU_OpenCL(src, dst_gpu);
    endTime := Now;
    timeGPU := (endTime - startTime) * 24 * 60 * 60 * 1000;
    WriteLn(Format('Temps GPU : %.2f ms', [timeGPU]));
    WriteLn(Format('Accélération : %.2fx', [timeCPU / timeGPU]));
    imwrite('result_gpu.jpg', dst_gpu);
    {$ENDIF}

  finally
    src.release;
    dst_cpu.release;
    dst_gpu.release;
  end;
end;

begin
  try
    ConfigureOpenCVGPU;
    WriteLn;
    BenchmarkImageProcessing;
  except
    on E: Exception do
      WriteLn('ERREUR : ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
end.
```

## Optimisations avancées

### 1. Transferts mémoire asynchrones

Les transferts entre CPU et GPU sont souvent le goulot d'étranglement. L'utilisation de streams permet de chevaucher calculs et transferts.

```pascal
{$IFDEF USE_CUDA}
type
  TCUDAStream = class
  private
    FStream: cudaStream_t;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AsyncMemcpy(dst, src: Pointer; size: NativeUInt; kind: Integer);
    procedure Synchronize;
    property Handle: cudaStream_t read FStream;
  end;

constructor TCUDAStream.Create;
begin
  inherited;
  cudaStreamCreate(@FStream);
end;

destructor TCUDAStream.Destroy;
begin
  cudaStreamDestroy(FStream);
  inherited;
end;

procedure TCUDAStream.AsyncMemcpy(dst, src: Pointer; size: NativeUInt; kind: Integer);
begin
  cudaMemcpyAsync(dst, src, size, kind, FStream);
end;

procedure TCUDAStream.Synchronize;
begin
  cudaStreamSynchronize(FStream);
end;

// Exemple d'utilisation
procedure PipelinedProcessing;
var
  stream1, stream2: TCUDAStream;
  hostData1, hostData2: Pointer;
  deviceData1, deviceData2: Pointer;
  size: NativeUInt;
begin
  stream1 := TCUDAStream.Create;
  stream2 := TCUDAStream.Create;
  try
    size := 1024 * 1024 * SizeOf(Single);

    // Allouer les buffers
    GetMem(hostData1, size);
    GetMem(hostData2, size);
    cudaMalloc(deviceData1, size);
    cudaMalloc(deviceData2, size);

    try
      // Pipeline : pendant que le GPU traite batch1,
      // on transfert batch2

      // Stream 1 : transfert batch1
      stream1.AsyncMemcpy(deviceData1, hostData1, size, cudaMemcpyHostToDevice);

      // Stream 2 : transfert batch2 en parallèle
      stream2.AsyncMemcpy(deviceData2, hostData2, size, cudaMemcpyHostToDevice);

      // Attendre que tout soit fini
      stream1.Synchronize;
      stream2.Synchronize;

    finally
      FreeMem(hostData1);
      FreeMem(hostData2);
      cudaFree(deviceData1);
      cudaFree(deviceData2);
    end;
  finally
    stream1.Free;
    stream2.Free;
  end;
end;
{$ENDIF}
```

### 2. Mémoire paginée (Pinned Memory)

La mémoire paginée permet des transferts plus rapides entre CPU et GPU.

```pascal
{$IFDEF USE_CUDA}
procedure UsePinnedMemory;
var
  hostData: Pointer;
  deviceData: Pointer;
  size: NativeUInt;
begin
  size := 1024 * 1024 * SizeOf(Single);

  // Allouer de la mémoire paginée (plus rapide pour les transferts)
  cudaMallocHost(@hostData, size);
  cudaMalloc(deviceData, size);

  try
    // Initialiser les données
    FillChar(hostData^, size, 0);

    // Transfert plus rapide qu'avec GetMem
    cudaMemcpy(deviceData, hostData, size, cudaMemcpyHostToDevice);

    // ... traitement ...

    cudaMemcpy(hostData, deviceData, size, cudaMemcpyDeviceToHost);

  finally
    cudaFreeHost(hostData);
    cudaFree(deviceData);
  end;
end;
{$ENDIF}
```

### 3. Mémoire unifiée (Unified Memory)

CUDA 6.0+ supporte la mémoire unifiée qui simplifie la gestion mémoire.

```pascal
{$IFDEF USE_CUDA}
procedure UseUnifiedMemory;
var
  data: PSingle;
  size: NativeUInt;
  i: Integer;
begin
  size := 1000000 * SizeOf(Single);

  // Allouer de la mémoire unifiée (accessible CPU et GPU)
  cudaMallocManaged(@data, size, cudaMemAttachGlobal);

  try
    // Initialiser depuis le CPU
    for i := 0 to 999999 do
      data[i] := i * 1.0;

    // Utiliser depuis le GPU (le kernel peut accéder directement)
    LaunchKernelWithUnifiedMemory(data, 1000000);

    // Attendre
    cudaDeviceSynchronize;

    // Lire depuis le CPU (automatiquement synchronisé)
    WriteLn('Résultat : ', data[0]:0:2);

  finally
    cudaFree(data);
  end;
end;
{$ENDIF}
```

### 4. Optimisation des kernels

```pascal
// Mauvais kernel : accès non coalescents
const BAD_KERNEL =
  '__global__ void badKernel(float *data, int n) {' +
  '    int tid = threadIdx.x;' +
  '    // Accès stride = n (mauvais pour cache)' +
  '    data[tid * n] = tid;' +
  '}';

// Bon kernel : accès coalescents
const GOOD_KERNEL =
  '__global__ void goodKernel(float *data, int n) {' +
  '    int tid = threadIdx.x + blockIdx.x * blockDim.x;' +
  '    if (tid < n)' +
  '        data[tid] = tid;  // Accès contigus (bon)' +
  '}';

// Kernel optimisé avec mémoire partagée
const OPTIMIZED_KERNEL =
  '__global__ void optimizedKernel(float *input, float *output, int n) {' +
  '    __shared__ float sharedMem[256];' +
  '    ' +
  '    int tid = threadIdx.x;' +
  '    int gid = blockIdx.x * blockDim.x + tid;' +
  '    ' +
  '    // Charger dans la mémoire partagée' +
  '    if (gid < n)' +
  '        sharedMem[tid] = input[gid];' +
  '    ' +
  '    __syncthreads();' +
  '    ' +
  '    // Traitement utilisant la mémoire partagée (très rapide)' +
  '    if (gid < n)' +
  '        output[gid] = sharedMem[tid] * 2.0f;' +
  '}';
```

## Applications pratiques

### 1. Traitement vidéo en temps réel

```pascal
program VideoProcessingGPU;

uses
  SysUtils, StrUtils, ocv.core, ocv.videoio, ocv.imgproc, ocv.highgui;

type
  TGPUVideoProcessor = class
  private
    FCapture: TVideoCapture;
    FUseGPU: Boolean;
  public
    constructor Create(deviceIndex: Integer; useGPU: Boolean);
    destructor Destroy; override;
    procedure Run;
  end;

constructor TGPUVideoProcessor.Create(deviceIndex: Integer; useGPU: Boolean);
begin
  inherited Create;
  FCapture := TVideoCapture.Create;
  FCapture.Open(deviceIndex);
  FUseGPU := useGPU;

  if FUseGPU then
  begin
    {$IFDEF USE_OPENCL}
    if cv.ocl.haveOpenCL then
      cv.ocl.setUseOpenCL(True);
    {$ENDIF}
  end;
end;

destructor TGPUVideoProcessor.Destroy;
begin
  FCapture.Free;
  inherited;
end;

procedure TGPUVideoProcessor.Run;
var
  frame, processed: TMat;
  {$IFDEF USE_OPENCL}
  uFrame, uProcessed: TUMat;
  {$ENDIF}
  frameCount: Integer;
  startTime, currentTime: TDateTime;
  fps: Double;
  key: Integer;
begin
  frameCount := 0;
  startTime := Now;

  namedWindow('Traitement vidéo', WINDOW_AUTOSIZE);

  while True do
  begin
    if not FCapture.Read(frame) then
      Break;

    Inc(frameCount);

    if FUseGPU then
    begin
      {$IFDEF USE_OPENCL}
      // Traitement GPU
      frame.copyTo(uFrame);
      cv.GaussianBlur(uFrame, uProcessed, Size(15, 15), 0);
      cv.Canny(uProcessed, uProcessed, 50, 150);
      uProcessed.copyTo(processed);
      {$ELSE}
      processed := frame.clone;
      {$ENDIF}
    end
    else
    begin
      // Traitement CPU
      GaussianBlur(frame, processed, Size(15, 15), 0);
      Canny(processed, processed, 50, 150);
    end;

    // Calculer FPS
    if frameCount mod 30 = 0 then
    begin
      currentTime := Now;
      fps := frameCount / ((currentTime - startTime) * 24 * 60 * 60);

      putText(processed,
        Format('FPS: %.1f (%s)', [fps, IfThen(FUseGPU, 'GPU', 'CPU')]),
        Point(10, 30), FONT_HERSHEY_SIMPLEX, 1,
        Scalar(255, 255, 255), 2);
    end;

    imshow('Traitement vidéo', processed);

    key := waitKey(1);
    if key = 27 then
      Break;

    frame.release;
    processed.release;
  end;

  destroyAllWindows;

  WriteLn(Format('FPS moyen : %.2f',
    [frameCount / ((Now - startTime) * 24 * 60 * 60)]));
end;

var
  processor: TGPUVideoProcessor;
  useGPU: Boolean;
  answer: string;

begin
  Write('Utiliser le GPU ? (o/n) : ');
  ReadLn(answer);
  useGPU := LowerCase(answer) = 'o';

  processor := TGPUVideoProcessor.Create(0, useGPU);
  try
    processor.Run;
  finally
    processor.Free;
  end;
end.
```

### 2. Calcul scientifique : Multiplication de matrices

```pascal
program MatrixMultiplyGPU;

uses
  SysUtils;

const
  MATRIX_SIZE = 1024;

  // Kernel optimisé avec tiling
  MATRIX_KERNEL =
    '#define TILE_SIZE 16' + #10 +
    '' + #10 +
    '__kernel void matrixMul(' + #10 +
    '    __global const float *A,' + #10 +
    '    __global const float *B,' + #10 +
    '    __global float *C,' + #10 +
    '    int N)' + #10 +
    '{' + #10 +
    '    __local float Asub[TILE_SIZE][TILE_SIZE];' + #10 +
    '    __local float Bsub[TILE_SIZE][TILE_SIZE];' + #10 +
    '    ' + #10 +
    '    int row = get_global_id(1);' + #10 +
    '    int col = get_global_id(0);' + #10 +
    '    int localRow = get_local_id(1);' + #10 +
    '    int localCol = get_local_id(0);' + #10 +
    '    ' + #10 +
    '    float sum = 0.0f;' + #10 +
    '    ' + #10 +
    '    for (int tile = 0; tile < N / TILE_SIZE; tile++) {' + #10 +
    '        // Charger les tuiles dans la mémoire partagée' + #10 +
    '        Asub[localRow][localCol] = A[row * N + (tile * TILE_SIZE + localCol)];' + #10 +
    '        Bsub[localRow][localCol] = B[(tile * TILE_SIZE + localRow) * N + col];' + #10 +
    '        ' + #10 +
    '        barrier(CLK_LOCAL_MEM_FENCE);' + #10 +
    '        ' + #10 +
    '        // Calculer le produit partiel' + #10 +
    '        for (int k = 0; k < TILE_SIZE; k++)' + #10 +
    '            sum += Asub[localRow][k] * Bsub[k][localCol];' + #10 +
    '        ' + #10 +
    '        barrier(CLK_LOCAL_MEM_FENCE);' + #10 +
    '    }' + #10 +
    '    ' + #10 +
    '    if (row < N && col < N)' + #10 +
    '        C[row * N + col] = sum;' + #10 +
    '}';

type
  TMatrix = array of Single;

procedure InitializeMatrix(var M: TMatrix; size: Integer);
var
  i: Integer;
begin
  SetLength(M, size * size);
  for i := 0 to High(M) do
    M[i] := Random;
end;

procedure MatrixMultiplyCPU(const A, B: TMatrix; var C: TMatrix; N: Integer);
var
  i, j, k: Integer;
begin
  for i := 0 to N - 1 do
    for j := 0 to N - 1 do
    begin
      C[i * N + j] := 0;
      for k := 0 to N - 1 do
        C[i * N + j] := C[i * N + j] + A[i * N + k] * B[k * N + j];
    end;
end;

procedure BenchmarkMatrixMultiply;
var
  A, B, C_cpu, C_gpu: TMatrix;
  startTime, endTime: TDateTime;
  timeCPU, timeGPU: Double;
  gflops: Double;
begin
  WriteLn(Format('=== Multiplication de matrices %dx%d ===',
    [MATRIX_SIZE, MATRIX_SIZE]));
  WriteLn;

  // Initialiser les matrices
  WriteLn('Initialisation...');
  InitializeMatrix(A, MATRIX_SIZE);
  InitializeMatrix(B, MATRIX_SIZE);
  SetLength(C_cpu, MATRIX_SIZE * MATRIX_SIZE);
  SetLength(C_gpu, MATRIX_SIZE * MATRIX_SIZE);

  // Benchmark CPU
  WriteLn('Calcul CPU...');
  startTime := Now;
  MatrixMultiplyCPU(A, B, C_cpu, MATRIX_SIZE);
  endTime := Now;
  timeCPU := (endTime - startTime) * 24 * 60 * 60;

  // Calcul GFLOPS : N³ * 2 opérations (mul + add)
  gflops := (2.0 * MATRIX_SIZE * MATRIX_SIZE * MATRIX_SIZE) / (timeCPU * 1e9);
  WriteLn(Format('Temps CPU : %.3f s', [timeCPU]));
  WriteLn(Format('Performance : %.2f GFLOPS', [gflops]));
  WriteLn;

  // Benchmark GPU avec OpenCL
  WriteLn('Calcul GPU (OpenCL)...');
  startTime := Now;
  MatrixMultiplyGPU(A, B, C_gpu, MATRIX_SIZE);
  endTime := Now;
  timeGPU := (endTime - startTime) * 24 * 60 * 60;

  gflops := (2.0 * MATRIX_SIZE * MATRIX_SIZE * MATRIX_SIZE) / (timeGPU * 1e9);
  WriteLn(Format('Temps GPU : %.3f s', [timeGPU]));
  WriteLn(Format('Performance : %.2f GFLOPS', [gflops]));
  WriteLn(Format('Accélération : %.2fx', [timeCPU / timeGPU]));
  WriteLn;

  // Vérifier la validité (comparer quelques éléments)
  WriteLn('Vérification des résultats...');
  if VerifyResults(C_cpu, C_gpu, MATRIX_SIZE) then
    WriteLn('✓ Résultats corrects !')
  else
    WriteLn('✗ Erreurs détectées !');
end;

procedure MatrixMultiplyGPU(const A, B: TMatrix; var C: TMatrix; N: Integer);
var
  ctx: TOpenCLContext;
  d_A, d_B, d_C: cl_mem;
  program_: cl_program;
  kernel: cl_kernel;
  size: NativeUInt;
  globalSize, localSize: array[0..1] of size_t;
begin
  ctx := TOpenCLContext.Create;
  try
    size := N * N * SizeOf(Single);

    // Créer les buffers
    d_A := ctx.CreateBuffer(size, CL_MEM_READ_ONLY);
    d_B := ctx.CreateBuffer(size, CL_MEM_READ_ONLY);
    d_C := ctx.CreateBuffer(size, CL_MEM_WRITE_ONLY);

    try
      // Copier les données
      ctx.WriteBuffer(d_A, size, @A[0]);
      ctx.WriteBuffer(d_B, size, @B[0]);

      // Compiler et créer le kernel
      program_ := ctx.CreateProgram(MATRIX_KERNEL);
      try
        kernel := ctx.CreateKernel(program_, 'matrixMul');
        try
          // Définir les arguments
          clSetKernelArg(kernel, 0, SizeOf(cl_mem), @d_A);
          clSetKernelArg(kernel, 1, SizeOf(cl_mem), @d_B);
          clSetKernelArg(kernel, 2, SizeOf(cl_mem), @d_C);
          clSetKernelArg(kernel, 3, SizeOf(Integer), @N);

          // Configuration 2D
          globalSize[0] := N;
          globalSize[1] := N;
          localSize[0] := 16;
          localSize[1] := 16;

          // Exécuter
          clEnqueueNDRangeKernel(ctx.Queue, kernel, 2, nil,
            @globalSize, @localSize, 0, nil, nil);

          ctx.Finish;

          // Lire le résultat
          ctx.ReadBuffer(d_C, size, @C[0]);

        finally
          clReleaseKernel(kernel);
        end;
      finally
        clReleaseProgram(program_);
      end;
    finally
      clReleaseMemObject(d_A);
      clReleaseMemObject(d_B);
      clReleaseMemObject(d_C);
    end;
  finally
    ctx.Free;
  end;
end;

function VerifyResults(const A, B: TMatrix; N: Integer): Boolean;
var
  i, samples: Integer;
  maxError: Single;
  error: Single;
begin
  Result := True;
  maxError := 0;
  samples := Min(100, N * N);

  for i := 0 to samples - 1 do
  begin
    error := Abs(A[i] - B[i]);
    if error > maxError then
      maxError := error;

    if error > 0.01 then // Tolérance pour erreurs de précision
    begin
      Result := False;
      WriteLn(Format('Erreur à l''index %d : %.6f vs %.6f (diff: %.6f)',
        [i, A[i], B[i], error]));
    end;
  end;

  if Result then
    WriteLn(Format('Erreur maximale : %.2e', [maxError]));
end;

begin
  Randomize;
  BenchmarkMatrixMultiply;

  WriteLn;
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
end.
```

### 3. Traitement d'images par lot (Batch Processing)

```pascal
program BatchImageProcessingGPU;

uses
  SysUtils, Classes, StrUtils, ocv.core, ocv.imgproc, ocv.highgui;

type
  TBatchProcessor = class
  private
    FInputFiles: TStringList;
    FOutputDir: string;
    FUseGPU: Boolean;
    procedure ProcessSingleImage(const inputPath, outputPath: string);
  public
    constructor Create(const inputDir, outputDir: string; useGPU: Boolean);
    destructor Destroy; override;
    procedure Execute;
  end;

constructor TBatchProcessor.Create(const inputDir, outputDir: string;
  useGPU: Boolean);
var
  searchRec: TSearchRec;
begin
  inherited Create;
  FInputFiles := TStringList.Create;
  FOutputDir := outputDir;
  FUseGPU := useGPU;

  // Créer le répertoire de sortie
  if not DirectoryExists(outputDir) then
    CreateDir(outputDir);

  // Lister les fichiers images
  if FindFirst(IncludeTrailingPathDelimiter(inputDir) + '*.jpg',
    faAnyFile, searchRec) = 0 then
  begin
    repeat
      if (searchRec.Attr and faDirectory) = 0 then
        FInputFiles.Add(IncludeTrailingPathDelimiter(inputDir) + searchRec.Name);
    until FindNext(searchRec) <> 0;
    FindClose(searchRec);
  end;

  WriteLn(Format('Fichiers trouvés : %d', [FInputFiles.Count]));

  {$IFDEF USE_OPENCL}
  if FUseGPU and cv.ocl.haveOpenCL then
    cv.ocl.setUseOpenCL(True);
  {$ENDIF}
end;

destructor TBatchProcessor.Destroy;
begin
  FInputFiles.Free;
  inherited;
end;

procedure TBatchProcessor.ProcessSingleImage(const inputPath, outputPath: string);
var
  src, dst: TMat;
  {$IFDEF USE_OPENCL}
  uSrc, uDst: TUMat;
  {$ENDIF}
begin
  src := imread(inputPath);
  if src.empty then
    Exit;

  try
    if FUseGPU then
    begin
      {$IFDEF USE_OPENCL}
      src.copyTo(uSrc);

      // Pipeline de traitement GPU
      cv.resize(uSrc, uDst, Size(800, 600));
      cv.GaussianBlur(uDst, uDst, Size(5, 5), 0);
      cv.cvtColor(uDst, uDst, CV_BGR2GRAY);
      cv.equalizeHist(uDst, uDst);

      uDst.copyTo(dst);
      {$ENDIF}
    end
    else
    begin
      // Pipeline CPU
      resize(src, dst, Size(800, 600));
      GaussianBlur(dst, dst, Size(5, 5), 0);
      cvtColor(dst, dst, CV_BGR2GRAY);
      equalizeHist(dst, dst);
    end;

    imwrite(outputPath, dst);

  finally
    src.release;
    dst.release;
  end;
end;

procedure TBatchProcessor.Execute;
var
  i: Integer;
  inputPath, outputPath, filename: string;
  startTime, endTime: TDateTime;
  totalTime: Double;
begin
  WriteLn(Format('Traitement de %d images (%s)...',
    [FInputFiles.Count, IfThen(FUseGPU, 'GPU', 'CPU')]));
  WriteLn;

  startTime := Now;

  for i := 0 to FInputFiles.Count - 1 do
  begin
    inputPath := FInputFiles[i];
    filename := ExtractFileName(inputPath);
    outputPath := IncludeTrailingPathDelimiter(FOutputDir) +
      ChangeFileExt(filename, '_processed.jpg');

    ProcessSingleImage(inputPath, outputPath);

    Write(Format(#13'Progression : %d/%d (%.1f%%)',
      [i + 1, FInputFiles.Count, (i + 1) / FInputFiles.Count * 100]));
  end;

  endTime := Now;
  totalTime := (endTime - startTime) * 24 * 60 * 60;

  WriteLn;
  WriteLn;
  WriteLn(Format('Temps total : %.2f s', [totalTime]));
  WriteLn(Format('Temps moyen par image : %.3f s',
    [totalTime / FInputFiles.Count]));
end;

var
  processor: TBatchProcessor;
  answer: string;
  useGPU: Boolean;

begin
  try
    Write('Utiliser le GPU ? (o/n) : ');
    ReadLn(answer);
    useGPU := LowerCase(answer) = 'o';

    processor := TBatchProcessor.Create('input_images', 'output_images', useGPU);
    try
      processor.Execute;
    finally
      processor.Free;
    end;

    WriteLn;
    WriteLn('Terminé !');
  except
    on E: Exception do
      WriteLn('ERREUR : ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
end.
```

## Profilage et débogage GPU

### Outils de profilage

#### NVIDIA Nsight (CUDA)

```bash
# Windows
"C:\Program Files\NVIDIA Corporation\Nsight Systems\nsys.exe" profile ./myapp.exe

# Linux
nsys profile ./myapp
```

#### AMD ROCm Profiler (OpenCL)

```bash
# Linux
rocprof --stats ./myapp
```

#### Intel VTune (OpenCL)

```bash
# Windows/Linux
vtune -collect gpu-hotspots ./myapp
```

### Profilage programmatique

```pascal
{$IFDEF USE_CUDA}
type
  TCUDAProfiler = class
  private
    FStartEvent, FStopEvent: cudaEvent_t;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    function Stop: Single; // Retourne le temps en ms
  end;

constructor TCUDAProfiler.Create;
begin
  inherited;
  cudaEventCreate(@FStartEvent);
  cudaEventCreate(@FStopEvent);
end;

destructor TCUDAProfiler.Destroy;
begin
  cudaEventDestroy(FStartEvent);
  cudaEventDestroy(FStopEvent);
  inherited;
end;

procedure TCUDAProfiler.Start;
begin
  cudaEventRecord(FStartEvent, 0);
end;

function TCUDAProfiler.Stop: Single;
var
  elapsedTime: Single;
begin
  cudaEventRecord(FStopEvent, 0);
  cudaEventSynchronize(FStopEvent);
  cudaEventElapsedTime(@elapsedTime, FStartEvent, FStopEvent);
  Result := elapsedTime;
end;

// Exemple d'utilisation
procedure ProfileKernel;
var
  profiler: TCUDAProfiler;
  time: Single;
begin
  profiler := TCUDAProfiler.Create;
  try
    profiler.Start;

    // Lancer le kernel
    LaunchMyKernel(...);

    time := profiler.Stop;
    WriteLn(Format('Temps kernel : %.3f ms', [time]));
  finally
    profiler.Free;
  end;
end;
{$ENDIF}
```

### Débogage CUDA

```pascal
function CheckCUDAError(err: cudaError_t; const operation: string): Boolean;
begin
  Result := err = cudaSuccess;
  if not Result then
  begin
    WriteLn(Format('Erreur CUDA dans %s : %s',
      [operation, cudaGetErrorString(err)]));
  end;
end;

procedure SafeCUDAOperation;
var
  devicePtr: Pointer;
  size: NativeUInt;
  err: cudaError_t;
begin
  size := 1024 * SizeOf(Single);

  err := cudaMalloc(devicePtr, size);
  if not CheckCUDAError(err, 'cudaMalloc') then
    Exit;

  try
    err := cudaMemset(devicePtr, 0, size);
    CheckCUDAError(err, 'cudaMemset');

    // ... autres opérations ...

  finally
    err := cudaFree(devicePtr);
    CheckCUDAError(err, 'cudaFree');
  end;
end;
```

### Débogage OpenCL

```pascal
function GetCLErrorString(err: cl_int): string;
begin
  case err of
    CL_SUCCESS: Result := 'CL_SUCCESS';
    CL_DEVICE_NOT_FOUND: Result := 'CL_DEVICE_NOT_FOUND';
    CL_DEVICE_NOT_AVAILABLE: Result := 'CL_DEVICE_NOT_AVAILABLE';
    CL_COMPILER_NOT_AVAILABLE: Result := 'CL_COMPILER_NOT_AVAILABLE';
    CL_MEM_OBJECT_ALLOCATION_FAILURE: Result := 'CL_MEM_OBJECT_ALLOCATION_FAILURE';
    CL_OUT_OF_RESOURCES: Result := 'CL_OUT_OF_RESOURCES';
    CL_OUT_OF_HOST_MEMORY: Result := 'CL_OUT_OF_HOST_MEMORY';
    CL_INVALID_VALUE: Result := 'CL_INVALID_VALUE';
    CL_INVALID_KERNEL_ARGS: Result := 'CL_INVALID_KERNEL_ARGS';
    CL_INVALID_WORK_GROUP_SIZE: Result := 'CL_INVALID_WORK_GROUP_SIZE';
    else Result := Format('Unknown error: %d', [err]);
  end;
end;

procedure CheckCLError(err: cl_int; const operation: string);
begin
  if err <> CL_SUCCESS then
    raise Exception.Create(Format('OpenCL Error in %s: %s',
      [operation, GetCLErrorString(err)]));
end;

procedure GetProgramBuildLog(program_: cl_program; device: cl_device_id);
var
  logSize: size_t;
  log: AnsiString;
begin
  clGetProgramBuildInfo(program_, device, CL_PROGRAM_BUILD_LOG,
    0, nil, @logSize);

  if logSize > 0 then
  begin
    SetLength(log, logSize);
    clGetProgramBuildInfo(program_, device, CL_PROGRAM_BUILD_LOG,
      logSize, @log[1], nil);

    WriteLn('=== Build Log ===');
    WriteLn(string(log));
    WriteLn('=================');
  end;
end;
```

## Comparaison de performance : Cas réels

### Tableau comparatif

Voici des résultats typiques de performance GPU vs CPU pour différentes tâches :

| Tâche | Taille | CPU (i7-9700K) | GPU (RTX 3070) | Accélération |
|-------|--------|----------------|----------------|--------------|
| Addition vecteurs | 10M éléments | 25 ms | 2 ms | **12.5x** |
| Multiplication matrices | 2048x2048 | 8.5 s | 0.18 s | **47x** |
| Flou gaussien | 4K image | 150 ms | 8 ms | **18.7x** |
| Détection contours | 4K image | 280 ms | 15 ms | **18.6x** |
| FFT | 8M points | 320 ms | 12 ms | **26.6x** |
| Tri | 50M éléments | 4.2 s | 0.24 s | **17.5x** |
| CNN Inference | ResNet-50 | 1200 ms | 45 ms | **26.6x** |

**Note** : Les performances varient selon le matériel, les données et l'optimisation du code.

### Facteurs influençant la performance

1. **Taille des données**
   - Petites données : overhead du GPU peut annuler le gain
   - Grandes données : le GPU excelle
   - Point d'équilibre : généralement > 100K éléments

2. **Type d'opération**
   - Parallélisable : excellent pour GPU
   - Séquentielle : le CPU est meilleur
   - Avec beaucoup de branches : le CPU est souvent plus rapide

3. **Transferts mémoire**
   - Peuvent dominer le temps total
   - Minimiser les transferts CPU↔GPU
   - Garder les données sur le GPU tant que possible

4. **Occupation du GPU**
   - Utiliser assez de threads pour saturer le GPU
   - Équilibrer charge de travail entre threads
   - Éviter les divergences de branchements

## Bonnes pratiques

### 1. Quand utiliser le GPU

**✓ Utilisez le GPU pour :**
- Traitement de grandes quantités de données (> 100K éléments)
- Opérations hautement parallélisables
- Calculs répétitifs sur des données similaires
- Traitement vidéo/image en temps réel
- Deep learning et réseaux de neurones
- Simulations physiques
- Calculs scientifiques massifs

**✗ N'utilisez PAS le GPU pour :**
- Petites quantités de données (< 10K éléments)
- Algorithmes séquentiels complexes
- Code avec beaucoup de branchements conditionnels
- Opérations nécessitant des transferts fréquents CPU↔GPU
- Systèmes sans GPU dédié

### 2. Optimisation du code GPU

```pascal
// ❌ MAUVAIS : Trop de transferts
procedure BadGPUCode;
var
  i: Integer;
  hostData, deviceData: Pointer;
begin
  GetMem(hostData, 1000 * SizeOf(Single));
  cudaMalloc(deviceData, 1000 * SizeOf(Single));

  for i := 0 to 99 do
  begin
    // Transfert à chaque itération = LENT !
    cudaMemcpy(deviceData, hostData, 1000 * SizeOf(Single),
      cudaMemcpyHostToDevice);
    LaunchKernel(deviceData, 1000);
    cudaMemcpy(hostData, deviceData, 1000 * SizeOf(Single),
      cudaMemcpyDeviceToHost);
  end;

  FreeMem(hostData);
  cudaFree(deviceData);
end;

// ✅ BON : Minimiser les transferts
procedure GoodGPUCode;
var
  hostData, deviceData: Pointer;
begin
  GetMem(hostData, 1000 * SizeOf(Single));
  cudaMalloc(deviceData, 1000 * SizeOf(Single));

  // Un seul transfert au début
  cudaMemcpy(deviceData, hostData, 1000 * SizeOf(Single),
    cudaMemcpyHostToDevice);

  // Tous les calculs sur le GPU
  LaunchKernelBatch(deviceData, 1000, 100);

  // Un seul transfert à la fin
  cudaMemcpy(hostData, deviceData, 1000 * SizeOf(Single),
    cudaMemcpyDeviceToHost);

  FreeMem(hostData);
  cudaFree(deviceData);
end;
```

### 3. Gestion des ressources

```pascal
type
  TGPUResourceManager = class
  private
    FAllocatedBuffers: TFPList;  // cl_mem = Pointer, TFPList suffit
    FContext: TOpenCLContext;
  public
    constructor Create(ctx: TOpenCLContext);
    destructor Destroy; override;
    function AllocateBuffer(size: NativeUInt; flags: cl_ulong): cl_mem;
    procedure FreeAllBuffers;
  end;

constructor TGPUResourceManager.Create(ctx: TOpenCLContext);
begin
  inherited Create;
  FContext := ctx;
  FAllocatedBuffers := TFPList.Create;
end;

destructor TGPUResourceManager.Destroy;
begin
  FreeAllBuffers;
  FAllocatedBuffers.Free;
  inherited;
end;

function TGPUResourceManager.AllocateBuffer(size: NativeUInt;
  flags: cl_ulong): cl_mem;
begin
  Result := FContext.CreateBuffer(size, flags);
  FAllocatedBuffers.Add(Result);
end;

procedure TGPUResourceManager.FreeAllBuffers;
var
  buffer: cl_mem;
begin
  for buffer in FAllocatedBuffers do
    clReleaseMemObject(buffer);
  FAllocatedBuffers.Clear;
end;

// Utilisation
procedure UseResourceManager;
var
  ctx: TOpenCLContext;
  manager: TGPUResourceManager;
  buf1, buf2, buf3: cl_mem;
begin
  ctx := TOpenCLContext.Create;
  manager := TGPUResourceManager.Create(ctx);
  try
    // Allouer plusieurs buffers
    buf1 := manager.AllocateBuffer(1024, CL_MEM_READ_WRITE);
    buf2 := manager.AllocateBuffer(2048, CL_MEM_READ_ONLY);
    buf3 := manager.AllocateBuffer(4096, CL_MEM_WRITE_ONLY);

    // Utiliser les buffers...

    // Tout est libéré automatiquement
  finally
    manager.Free;
    ctx.Free;
  end;
end;
```

### 4. Fallback CPU automatique

```pascal
type
  TSingleArray = array of Single;  // TArray<Single> n'existe pas en ObjFPC

  TComputeEngine = class
  private
    FGPUAvailable: Boolean;
    procedure ComputeCPU(const input: TSingleArray; var output: TSingleArray);
    procedure ComputeGPU(const input: TSingleArray; var output: TSingleArray);
  public
    constructor Create;
    procedure Compute(const input: TSingleArray; var output: TSingleArray);
  end;

constructor TComputeEngine.Create;
begin
  inherited;
  // Vérifier la disponibilité du GPU
  FGPUAvailable := CheckCUDAAvailable or CheckOpenCLAvailable;

  if FGPUAvailable then
    WriteLn('Mode : GPU')
  else
    WriteLn('Mode : CPU (fallback)');
end;

procedure TComputeEngine.Compute(const input: TSingleArray;
  var output: TSingleArray);
begin
  if FGPUAvailable then
  begin
    try
      ComputeGPU(input, output);
    except
      on E: Exception do
      begin
        WriteLn('Erreur GPU, basculement sur CPU : ', E.Message);
        FGPUAvailable := False;
        ComputeCPU(input, output);
      end;
    end;
  end
  else
    ComputeCPU(input, output);
end;
```

## Différences Windows / Ubuntu

### Chemins et bibliothèques

```pascal
{$IFDEF WINDOWS}
const
  // CUDA
  CUDA_PATH = 'C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.0\';
  CUDA_LIB_PATH = CUDA_PATH + 'lib\x64\';
  CUDA_BIN_PATH = CUDA_PATH + 'bin\';

  // OpenCL
  OPENCL_DLL = 'OpenCL.dll';
  OPENCL_PATH = 'C:\Windows\System32\';
{$ENDIF}

{$IFDEF LINUX}
const
  // CUDA
  CUDA_PATH = '/usr/local/cuda/';
  CUDA_LIB_PATH = CUDA_PATH + 'lib64/';
  CUDA_BIN_PATH = CUDA_PATH + 'bin/';

  // OpenCL
  OPENCL_SO = 'libOpenCL.so';
  OPENCL_PATH = '/usr/lib/x86_64-linux-gnu/';
{$ENDIF}

procedure ConfigurePaths;
begin
  {$IFDEF WINDOWS}
  // Ajouter au PATH si nécessaire
  SetEnvironmentVariable('PATH',
    PChar(GetEnvironmentVariable('PATH') + ';' + CUDA_BIN_PATH));
  {$ENDIF}

  {$IFDEF LINUX}
  // Vérifier LD_LIBRARY_PATH
  if Pos(CUDA_LIB_PATH, GetEnvironmentVariable('LD_LIBRARY_PATH')) = 0 then
    WriteLn('Attention : Ajouter CUDA à LD_LIBRARY_PATH');
  {$ENDIF}
end;
```

### Compilation croisée

```pascal
// Makefile ou script de build

{$IFDEF WINDOWS}
  // Compiler avec nvcc (Windows)
  {$L 'kernel_win.obj'}
{$ENDIF}

{$IFDEF LINUX}
  // Compiler avec nvcc (Linux)
  {$L 'kernel_linux.o'}
{$ENDIF}
```

Script de compilation :

```bash
#!/bin/bash
# build_gpu.sh

# Windows (via WSL ou MinGW)
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
    nvcc -c kernel.cu -o kernel_win.obj
    fpc -FCCUDA_PATH program.pas
fi

# Linux
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    nvcc -c kernel.cu -o kernel_linux.o -fPIC
    fpc -Fl/usr/local/cuda/lib64 program.pas
fi
```

## Conclusion

### Récapitulatif

L'accélération GPU avec CUDA et OpenCL offre des gains de performance spectaculaires pour certaines tâches :

**Points clés :**
- **CUDA** : Maximum de performance sur GPU NVIDIA
- **OpenCL** : Portabilité entre différents GPU et même CPU
- **Gains typiques** : 10x à 100x selon la tâche
- **Overhead** : Important pour petites données
- **Transferts mémoire** : Principal goulot d'étranglement à minimiser

### Ce que vous avez appris

Dans ce chapitre, vous avez découvert :

1. **Les fondamentaux du GPU computing**
   - Architecture GPU vs CPU
   - Différences entre CUDA et OpenCL
   - Concepts de threads, blocs et grilles

2. **Installation et configuration**
   - CUDA Toolkit sur Windows et Ubuntu
   - OpenCL sur différentes plateformes
   - Vérification du matériel disponible

3. **Programmation GPU avec FreePascal**
   - Bindings CUDA et OpenCL
   - Création et lancement de kernels
   - Gestion de la mémoire GPU
   - Transferts de données

4. **Optimisations avancées**
   - Streams et transferts asynchrones
   - Mémoire paginée et unifiée
   - Optimisation des kernels
   - Réduction des transferts CPU↔GPU

5. **Applications pratiques**
   - Traitement d'images avec OpenCV
   - Calculs scientifiques (multiplication de matrices)
   - Traitement vidéo en temps réel
   - Batch processing

6. **Débogage et profilage**
   - Outils professionnels (Nsight, VTune)
   - Gestion d'erreurs
   - Mesure de performance
   - Build logs et diagnostics

7. **Bonnes pratiques**
   - Quand utiliser le GPU
   - Gestion des ressources
   - Fallback CPU automatique
   - Portabilité Windows/Ubuntu

### Avantages et limitations

#### ✅ Avantages

- **Performance exceptionnelle** pour calculs parallèles massifs
- **Réduction drastique** des temps de traitement
- **Efficacité énergétique** : plus de calculs par watt
- **Évolutivité** : ajout de GPU pour plus de puissance
- **Écosystème mature** : bibliothèques optimisées disponibles
- **Support industriel** : utilisé massivement en production

#### ⚠️ Limitations

- **Complexité accrue** du code et du débogage
- **Overhead** non négligeable pour petites données
- **Transferts mémoire** peuvent annuler les gains
- **Dépendance matérielle** : nécessite GPU compatible
- **Consommation électrique** élevée sous charge
- **Coût** : GPU performants restent chers
- **Portabilité** : code moins universel qu'un code CPU pur

### Quand utiliser l'accélération GPU ?

#### Cas d'usage idéaux

**✓ Traitement d'images et vidéos**
```
- Filtrage, convolution, transformations
- Détection et reconnaissance d'objets
- Rendu et effets visuels
- Encodage/décodage vidéo
Gain typique : 15-30x
```

**✓ Deep Learning et IA**
```
- Entraînement de réseaux de neurones
- Inférence de modèles
- Computer vision
- Traitement du langage naturel
Gain typique : 20-100x
```

**✓ Calculs scientifiques**
```
- Simulations physiques
- Calculs matriciels
- FFT et traitement du signal
- Dynamique moléculaire
Gain typique : 10-50x
```

**✓ Finance et cryptographie**
```
- Monte Carlo simulations
- Analyse de risques
- Mining de cryptomonnaies
- Calculs de hash massifs
Gain typique : 30-100x
```

#### Cas où le CPU est préférable

**✗ Traitement séquentiel**
```
- Algorithmes non parallélisables
- Logique métier complexe avec branches
- Parsing et manipulation de texte
```

**✗ Petites quantités de données**
```
- Moins de 10 000 éléments
- L'overhead GPU domine le temps total
```

**✗ Transferts fréquents**
```
- Données changeant constamment
- Résultats intermédiaires nécessaires sur CPU
```

**✗ Contraintes de portabilité**
```
- Application devant tourner sur tous systèmes
- Environnements sans GPU
```

## Ressources et documentation

### Documentation officielle

#### CUDA
- **Site officiel** : https://developer.nvidia.com/cuda-zone
- **Documentation** : https://docs.nvidia.com/cuda/
- **Tutoriels** : https://developer.nvidia.com/cuda-education
- **Best Practices** : https://docs.nvidia.com/cuda/cuda-c-best-practices-guide/
- **Samples** : https://github.com/NVIDIA/cuda-samples

#### OpenCL
- **Site officiel** : https://www.khronos.org/opencl/
- **Spécification** : https://registry.khronos.org/OpenCL/
- **SDK Intel** : https://software.intel.com/content/www/us/en/develop/tools/opencl-sdk.html
- **AMD ROCm** : https://rocmdocs.amd.com/
- **Tutoriels** : https://www.khronos.org/opencl/resources

### Livres recommandés

1. **"CUDA by Example"** - Jason Sanders, Edward Kandrot
   - Excellent pour débuter avec CUDA
   - Exemples pratiques et progressifs

2. **"Programming Massively Parallel Processors"** - David Kirk, Wen-mei Hwu
   - Concepts fondamentaux du GPU computing
   - Théorie et pratique

3. **"OpenCL Programming Guide"** - Aaftab Munshi, Benedict Gaster
   - Guide complet pour OpenCL
   - Exemples multi-plateformes

4. **"Professional CUDA C Programming"** - John Cheng, Max Grossman
   - Niveau avancé
   - Optimisations et techniques professionnelles

### Communautés et forums

- **NVIDIA Developer Forums** : https://forums.developer.nvidia.com/
- **Stack Overflow** : Tag [cuda] et [opencl]
- **Reddit** : r/CUDA, r/OpenCL, r/gpgpu
- **Lazarus Forum** : Section GPU computing
- **GitHub** : Nombreux projets open source

### Bibliothèques utiles

#### Pour CUDA

**cuBLAS** - Algèbre linéaire
```pascal
// Multiplication de matrices optimisée
cublasSgemm(handle, CUBLAS_OP_N, CUBLAS_OP_N,
  m, n, k, alpha, A, lda, B, ldb, beta, C, ldc);
```

**cuFFT** - Transformée de Fourier
```pascal
// FFT rapide sur GPU
cufftExecC2C(plan, idata, odata, CUFFT_FORWARD);
```

**cuDNN** - Deep Learning
```pascal
// Convolution optimisée pour réseaux de neurones
cudnnConvolutionForward(handle, alpha, srcDesc, srcData,
  filterDesc, filterData, convDesc, algo, workspace,
  workspaceSize, beta, destDesc, destData);
```

**Thrust** - Bibliothèque d'algorithmes parallèles
```cpp
// Tri parallèle ultra-rapide
thrust::sort(thrust::device, d_vec.begin(), d_vec.end());
```

#### Pour OpenCL

**clBLAS** - Algèbre linéaire
```pascal
// Equivalent cuBLAS pour OpenCL
clblasSgemm(order, transA, transB, M, N, K, alpha,
  A, offA, lda, B, offB, ldb, beta, C, offC, ldc,
  1, @queue, 0, nil, @event);
```

**clFFT** - Transformée de Fourier
```pascal
// FFT avec OpenCL
clfftEnqueueTransform(plan, CLFFT_FORWARD, 1, @queue,
  0, nil, nil, @bufferIn, @bufferOut, tmpBuffer);
```

### Projets open source avec FreePascal/Lazarus

1. **OpenCL4Delphi**
   - Bindings OpenCL complets
   - https://github.com/Cooler2/OpenCL4Delphi

2. **CUDA4Delphi**
   - Wrappers CUDA pour Delphi/FreePascal
   - Exemples de kernels

3. **GPU-Image-Processing**
   - Filtres images accélérés GPU
   - Compatible Lazarus

## Projet complet : Application de benchmark

Voici un projet complet qui illustre tous les concepts abordés :

```pascal
program GPUBenchmark;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, DateUtils;

type
  TBenchmarkResult = record
    Name: string;
    TimeCPU: Double;
    TimeGPU: Double;
    Speedup: Double;
    Success: Boolean;
  end;

  TGPUBenchmark = class
  private
    FResults: array of TBenchmarkResult;
    FGPUAvailable: Boolean;
    procedure AddResult(const name: string; cpuTime, gpuTime: Double; success: Boolean);
    function BenchmarkVectorAdd: TBenchmarkResult;
    function BenchmarkMatrixMultiply: TBenchmarkResult;
    function BenchmarkImageProcessing: TBenchmarkResult;
  public
    constructor Create;
    procedure RunAllBenchmarks;
    procedure PrintResults;
    procedure SaveResultsToFile(const filename: string);
  end;

constructor TGPUBenchmark.Create;
begin
  inherited;
  FGPUAvailable := CheckCUDAAvailable or CheckOpenCLAvailable;

  WriteLn('=================================');
  WriteLn('   GPU BENCHMARK SUITE');
  WriteLn('=================================');
  WriteLn;
  WriteLn('Configuration système :');
  {$IFDEF WINDOWS}
  WriteLn('OS : Windows');
  {$ELSE}
  WriteLn('OS : Linux');
  {$ENDIF}
  WriteLn('GPU disponible : ', IfThen(FGPUAvailable, 'Oui', 'Non'));
  WriteLn;
end;

procedure TGPUBenchmark.AddResult(const name: string;
  cpuTime, gpuTime: Double; success: Boolean);
var
  result: TBenchmarkResult;
begin
  result.Name := name;
  result.TimeCPU := cpuTime;
  result.TimeGPU := gpuTime;
  if gpuTime > 0 then
    result.Speedup := cpuTime / gpuTime
  else
    result.Speedup := 0;
  result.Success := success;

  SetLength(FResults, Length(FResults) + 1);
  FResults[High(FResults)] := result;
end;

function TGPUBenchmark.BenchmarkVectorAdd: TBenchmarkResult;
const
  N = 10000000; // 10 millions
var
  startTime, endTime: TDateTime;
  timeCPU, timeGPU: Double;
  a, b, c: array of Single;
  i: Integer;
begin
  WriteLn('Test 1/3 : Addition de vecteurs (10M éléments)...');

  SetLength(a, N);
  SetLength(b, N);
  SetLength(c, N);

  // Initialiser
  for i := 0 to N - 1 do
  begin
    a[i] := i * 1.0;
    b[i] := i * 2.0;
  end;

  // Benchmark CPU
  Write('  CPU... ');
  startTime := Now;
  for i := 0 to N - 1 do
    c[i] := a[i] + b[i];
  endTime := Now;
  timeCPU := (endTime - startTime) * 24 * 60 * 60 * 1000;
  WriteLn(Format('%.2f ms', [timeCPU]));

  // Benchmark GPU
  if FGPUAvailable then
  begin
    Write('  GPU... ');
    try
      startTime := Now;
      // Appel fonction GPU (simplifié)
      // VectorAddGPU(a, b, c, N);
      Sleep(10); // Simulation pour l'exemple
      endTime := Now;
      timeGPU := (endTime - startTime) * 24 * 60 * 60 * 1000;
      WriteLn(Format('%.2f ms', [timeGPU]));

      Result.Success := True;
    except
      WriteLn('ERREUR');
      timeGPU := 0;
      Result.Success := False;
    end;
  end
  else
  begin
    WriteLn('  GPU... Non disponible');
    timeGPU := 0;
    Result.Success := False;
  end;

  AddResult('Addition de vecteurs', timeCPU, timeGPU, Result.Success);
  WriteLn;
end;

function TGPUBenchmark.BenchmarkMatrixMultiply: TBenchmarkResult;
const
  N = 512;
var
  startTime, endTime: TDateTime;
  timeCPU, timeGPU: Double;
begin
  WriteLn('Test 2/3 : Multiplication de matrices (512x512)...');

  Write('  CPU... ');
  startTime := Now;
  // Simulation du calcul CPU
  Sleep(2000); // 2 secondes
  endTime := Now;
  timeCPU := (endTime - startTime) * 24 * 60 * 60 * 1000;
  WriteLn(Format('%.2f ms', [timeCPU]));

  if FGPUAvailable then
  begin
    Write('  GPU... ');
    try
      startTime := Now;
      Sleep(50); // Simulation GPU
      endTime := Now;
      timeGPU := (endTime - startTime) * 24 * 60 * 60 * 1000;
      WriteLn(Format('%.2f ms', [timeGPU]));

      Result.Success := True;
    except
      WriteLn('ERREUR');
      timeGPU := 0;
      Result.Success := False;
    end;
  end
  else
  begin
    WriteLn('  GPU... Non disponible');
    timeGPU := 0;
    Result.Success := False;
  end;

  AddResult('Multiplication matrices', timeCPU, timeGPU, Result.Success);
  WriteLn;
end;

function TGPUBenchmark.BenchmarkImageProcessing: TBenchmarkResult;
var
  startTime, endTime: TDateTime;
  timeCPU, timeGPU: Double;
begin
  WriteLn('Test 3/3 : Traitement d''image (4K)...');

  Write('  CPU... ');
  startTime := Now;
  Sleep(500); // Simulation
  endTime := Now;
  timeCPU := (endTime - startTime) * 24 * 60 * 60 * 1000;
  WriteLn(Format('%.2f ms', [timeCPU]));

  if FGPUAvailable then
  begin
    Write('  GPU... ');
    try
      startTime := Now;
      Sleep(25); // Simulation GPU
      endTime := Now;
      timeGPU := (endTime - startTime) * 24 * 60 * 60 * 1000;
      WriteLn(Format('%.2f ms', [timeGPU]));

      Result.Success := True;
    except
      WriteLn('ERREUR');
      timeGPU := 0;
      Result.Success := False;
    end;
  end
  else
  begin
    WriteLn('  GPU... Non disponible');
    timeGPU := 0;
    Result.Success := False;
  end;

  AddResult('Traitement d''image', timeCPU, timeGPU, Result.Success);
  WriteLn;
end;

procedure TGPUBenchmark.RunAllBenchmarks;
begin
  WriteLn('Démarrage des benchmarks...');
  WriteLn;

  BenchmarkVectorAdd;
  BenchmarkMatrixMultiply;
  BenchmarkImageProcessing;

  WriteLn('Benchmarks terminés !');
  WriteLn;
end;

procedure TGPUBenchmark.PrintResults;
var
  i: Integer;
  totalSpeedup: Double;
  successCount: Integer;
begin
  WriteLn('=================================');
  WriteLn('         RÉSULTATS');
  WriteLn('=================================');
  WriteLn;

  WriteLn('Test                        CPU (ms)    GPU (ms)    Accélération');
  WriteLn('------------------------------------------------------------------------');

  totalSpeedup := 0;
  successCount := 0;

  for i := 0 to High(FResults) do
  begin
    with FResults[i] do
    begin
      Write(Format('%-25s', [Name]));
      Write(Format('%10.2f', [TimeCPU]));

      if Success then
      begin
        Write(Format('%12.2f', [TimeGPU]));
        Write(Format('%12.2fx', [Speedup]));
        totalSpeedup := totalSpeedup + Speedup;
        Inc(successCount);
      end
      else
        Write('           -           -');

      WriteLn;
    end;
  end;

  WriteLn('------------------------------------------------------------------------');

  if successCount > 0 then
  begin
    WriteLn;
    WriteLn(Format('Accélération moyenne : %.2fx', [totalSpeedup / successCount]));
  end;

  WriteLn;
end;

procedure TGPUBenchmark.SaveResultsToFile(const filename: string);
var
  f: TextFile;
  i: Integer;
begin
  AssignFile(f, filename);
  Rewrite(f);
  try
    WriteLn(f, 'GPU Benchmark Results');
    WriteLn(f, '=====================');
    WriteLn(f, 'Date: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    WriteLn(f);

    for i := 0 to High(FResults) do
    begin
      with FResults[i] do
      begin
        WriteLn(f, Name);
        WriteLn(f, '  CPU: ', TimeCPU:0:2, ' ms');
        if Success then
        begin
          WriteLn(f, '  GPU: ', TimeGPU:0:2, ' ms');
          WriteLn(f, '  Speedup: ', Speedup:0:2, 'x');
        end
        else
          WriteLn(f, '  GPU: N/A');
        WriteLn(f);
      end;
    end;

    WriteLn('Résultats sauvegardés dans : ', filename);
  finally
    CloseFile(f);
  end;
end;

// Programme principal
var
  benchmark: TGPUBenchmark;

begin
  try
    benchmark := TGPUBenchmark.Create;
    try
      benchmark.RunAllBenchmarks;
      benchmark.PrintResults;
      benchmark.SaveResultsToFile('benchmark_results.txt');
    finally
      benchmark.Free;
    end;
  except
    on E: Exception do
      WriteLn('ERREUR : ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Pour aller plus loin

### Prochaines étapes

1. **Approfondissement CUDA**
   - Streams et events avancés
   - Unified Virtual Memory
   - Multi-GPU programming
   - CUDA Graphs

2. **OpenCL avancé**
   - Sub-groups et work-group
   - Pipes et SVM (Shared Virtual Memory)
   - SPIR-V et compilation offline

3. **Frameworks de haut niveau**
   - ArrayFire : API simplifiée pour GPU
   - Halide : Language pour traitement d'images
   - Kokkos : Portabilité C++ pour HPC

4. **Deep Learning**
   - Intégration TensorFlow/PyTorch
   - Création de layers personnalisés
   - Optimisation d'inférence

5. **Calcul distribué**
   - MPI + GPU pour supercalculateurs
   - Multi-nœuds multi-GPU
   - Cloud computing GPU (AWS, Azure, GCP)

### Tendances futures

**2024-2025**
- GPUs toujours plus puissants (Ada Lovelace, RDNA 3)
- Ray tracing matériel généralisé
- AI accelerators intégrés
- Consommation énergétique optimisée

**Au-delà**
- Calcul quantique + GPU hybride
- Mémoire HBM3 ultra-rapide
- Chiplets et architectures modulaires
- Neuromorphic computing

### Certification et formation

**Certifications NVIDIA**
- CUDA Certified Associate
- Deep Learning Institute
- Jetson AI Specialist

**Cours en ligne recommandés**
- Coursera : "Introduction to Parallel Programming" (Udacity)
- edX : "GPU Programming" (Université Johns Hopkins)
- YouTube : Canaux NVIDIA Developer, Computerphile

## Conclusion finale

L'accélération GPU représente une révolution dans le calcul haute performance, rendant accessible à tous des capacités de calcul auparavant réservées aux supercalculateurs. Avec FreePascal et Lazarus, vous disposez maintenant des outils pour exploiter cette puissance dans vos applications, que ce soit pour du traitement d'images, de la simulation scientifique ou de l'intelligence artificielle.

**Les principaux enseignements :**

1. Le GPU n'est pas magique : il faut comprendre son architecture pour l'exploiter efficacement
2. Les transferts mémoire sont souvent le goulot d'étranglement principal
3. Toutes les tâches ne bénéficient pas de l'accélération GPU
4. L'optimisation est un processus itératif : mesurer, analyser, améliorer
5. La portabilité entre Windows et Ubuntu est possible avec OpenCL

**N'oubliez jamais :**
- Commencez simple, optimisez progressivement
- Mesurez les performances réelles, pas théoriques
- Gardez toujours un fallback CPU
- Documentez vos choix d'optimisation
- Partagez vos découvertes avec la communauté

L'aventure du GPU computing ne fait que commencer. Les applications que vous créerez aujourd'hui avec ces technologies façonneront les innovations de demain.

**Bon développement GPU avec FreePascal et Lazarus !** 🚀

---

**Prochaine étape recommandée** : Chapitre 13 - Développement Mobile et Embarqué

⏭️ [Développement Mobile et Embarqué](/13-developpement-mobile-embarque/README.md)
