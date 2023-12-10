#include <iostream>
#include <vector>
#include <omp.h>
#include <cstdlib>
#include <cmath>

using namespace std;

// #################### Estructuras ####################
// Estructura para representar un elemento no nulo
struct ElementoNoNulo {
    int fila;
    int columna;
    float valor;
};

// Estructura para representar una matriz dispersa
struct MatrizDispersa {
    int filas;
    int columnas;
    vector<ElementoNoNulo> elementosNoNulos;
};


// #################### Funciones para imprimir ####################

// Funcion para imprimir un vector en forma de columna
void imprimeVectorCol(const vector<float>& v){
    for(int i=0 ; i<v.size() ; i++) printf("%f\n", v[i]);
}

// Función para imprimir los elementos no nulos de una matriz dispersa
void imprimirElementosNoNulos(const MatrizDispersa& matriz) {
    cout << "Elementos no nulos:" << endl;
    for (const ElementoNoNulo& elemento : matriz.elementosNoNulos) {
        cout << "(" << elemento.fila << ", " << elemento.columna << ") = " << elemento.valor << endl;
    }
}

// Función para imprimir una matriz dispersa
void imprimirMatrizDispersa(const MatrizDispersa& matriz) {
    for (int i = 0; i < matriz.filas; ++i) {
        for (int j = 0; j < matriz.columnas; ++j) {
            bool encontrado = false;
            for (const ElementoNoNulo& elemento : matriz.elementosNoNulos) {
                if (elemento.fila == i && elemento.columna == j) {
                    cout << elemento.valor << "\t";
                    encontrado = true;
                    break;
                }
            }
            if (!encontrado) {
                cout << "0\t";
            }
        }
        cout << endl;
    }
}

// #################### Funciones Matriz dispersa ####################

// Función para llenar una matriz dispersa con una densidad dada
void llenarMatrizDispersa(MatrizDispersa& matriz, double densidad) {
    // Limpiar elementos existentes
    matriz.elementosNoNulos.clear();

    // Generar elementos no nulos aleatorios
    for (int i = 0; i < matriz.filas; ++i) {
        for (int j = 0; j < matriz.columnas; ++j) {
            if ((rand() % 100) < (densidad * 100)) {
                float valor = (static_cast<float>(rand()) / RAND_MAX)*10;
                matriz.elementosNoNulos.push_back({i, j, valor});
            }
        }
    }
} 

// Función para multiplicar una matriz dispersa por un vector (Paralelismo GPU)
__global__ void multiplicarMatrizPorVectorGPU(const ElementoNoNulo* elementos, int numElementos, const float* vec, float* vectorResultadoGPU) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;

    while (tid < numElementos) {
        const ElementoNoNulo& elemento = elementos[tid];
        atomicAdd(&vectorResultadoGPU[elemento.fila], elemento.valor * vec[elemento.columna]);
        tid += gridDim.x * blockDim.x;
    }
}
 
// Función auxiliar para el correcto funcionamiento de la función "multiplicarMatrizPorVectorGPU"
void multiplicarMatrizPorVectorCUDA(const MatrizDispersa& matriz, const vector<float>& vec, vector<float>& vectorResultadoGPU, double& tiempo_total_GPU) {
    if (matriz.columnas != vec.size() || matriz.filas != vectorResultadoGPU.size()) {
        cerr << "Error: Las dimensiones de la matriz y el vector no son compatibles para la multiplicación." << endl;
        exit(EXIT_FAILURE);
    }

    // Copiar datos a la GPU
    ElementoNoNulo* d_elementos;
    float* d_vec;
    float* d_vectorResultadoGPU;

    cudaMalloc((void**)&d_elementos, matriz.elementosNoNulos.size() * sizeof(ElementoNoNulo));
    cudaMalloc((void**)&d_vec, vec.size() * sizeof(float));
    cudaMalloc((void**)&d_vectorResultadoGPU, vectorResultadoGPU.size() * sizeof(float));

    cudaMemcpy(d_elementos, matriz.elementosNoNulos.data(), matriz.elementosNoNulos.size() * sizeof(ElementoNoNulo), cudaMemcpyHostToDevice);
    cudaMemcpy(d_vec, vec.data(), vec.size() * sizeof(float), cudaMemcpyHostToDevice);
    cudaMemcpy(d_vectorResultadoGPU, vectorResultadoGPU.data(), vectorResultadoGPU.size() * sizeof(float), cudaMemcpyHostToDevice);

    // Configuración de bloques y hilos
    int blockSize = 256;
    int numBlocks = (matriz.elementosNoNulos.size() + blockSize - 1) / blockSize;

    // Llamada al kernel de CUDA
    double t0 = omp_get_wtime();
    multiplicarMatrizPorVectorGPU<<<numBlocks, blockSize>>>(d_elementos, matriz.elementosNoNulos.size(), d_vec, d_vectorResultadoGPU);
    double t1 = omp_get_wtime();
    tiempo_total_GPU = t1-t0;

    // Copiar resultados de vuelta a la CPU
    cudaMemcpy(vectorResultadoGPU.data(), d_vectorResultadoGPU, vectorResultadoGPU.size() * sizeof(float), cudaMemcpyDeviceToHost);

    // Liberar memoria en la GPU
    cudaFree(d_elementos);
    cudaFree(d_vec);
    cudaFree(d_vectorResultadoGPU);
}

// Función para multiplicar una matriz dispersa por un vector (Parelelismo CPU)
void multiplicarMatrizPorVectorCPU(const MatrizDispersa& matriz, const vector<float>& vec, vector<float>& vectorResultadoCPU) {
    if (matriz.columnas != vec.size() || matriz.filas != vectorResultadoCPU.size()) {
        cerr << "Error: Las dimensiones de la matriz y el vector no son compatibles para la multiplicación." << endl;
        exit(EXIT_FAILURE);
    }

    #pragma omp parallel for
    for (int i = 0; i < matriz.elementosNoNulos.size(); ++i) {
        const ElementoNoNulo& elemento = matriz.elementosNoNulos[i];
        #pragma omp atomic
        vectorResultadoCPU[elemento.fila] += elemento.valor * vec[elemento.columna];
    }
}

// Función que genera un vector con números aleatorios entre 1 y 10
void generarVector(vector<float>& vec) {
    for (int i = 0; i < vec.size(); ++i) {
        float numeroAleatorio = static_cast<double>(std::rand()) / RAND_MAX;
        vec[i] = numeroAleatorio*10; // Números aleatorios entre 1 y 10
    }
}

// Función que compara si dos vectores de Floats son iguales, acepta cierta tolerancia
bool verificarResultados(const vector<float>& vector1, const vector<float>& vector2) {
    // No comprobé que sean del mismo largo porque si lo serán
    // Probé este mismo código con números enteros y siempre daban los mismo resultados, asi que el código si funciona como debe
    // Comparar elemento por elemento con tolerancia relativa
    // Tuve que seleccionar la tolerancia relativa ya que con la tolerancia absoluta me daban diferencias de hasta 3.5
    float tolerancia = 0.05;
    for (size_t i = 0; i < vector1.size(); ++i) {
        float max_valor = max(abs(vector1[i]), abs(vector2[i]));

        // Calcular la diferencia relativa
        float diferencia_relativa = abs(vector1[i] - vector2[i]) / max_valor;
        if (diferencia_relativa > tolerancia) {
            // La diferencia relativa entre los elementos es mayor que la tolerancia permitida
            return false;
        }
    }
    // Todos los elementos son iguales dentro de la tolerancia
    return true;
}

int main(int argc, char *argv[]) {
    if(argc != 6){
        cerr << "Ejecutar como: ./prog <n> <d> <s> <m> <nt>\nn = tamaño matriz\nd = densidad números no nulos (0 < d < 1)\ns = semilla de números aleatorios\nm = modo de ejecución\n\t-> 0 = modo CPU\n\t-> 1 = modo GPU\n\t-> 2 = modo CPU y GPU a la vez\nnt = número threads de la CPU" << endl;
        exit(EXIT_FAILURE);
    }

    system("clear");
    int n = atoi(argv[1]);
    double densidad = atof(argv[2]);
    int seed = atoi(argv[3]);
    int modo = atoi(argv[4]);
    int nt = atoi(argv[5]);
    printf("n = %i, d = %f, s = %i, m = %i, nt = %i\n", n, densidad, seed, modo, nt); fflush(stdout);
    omp_set_num_threads(nt);

    // Inicializar la semilla de números aleatorios
    srand(seed); 

    // Crear matriz dispersa
    MatrizDispersa matriz;
    matriz.filas = n;
    matriz.columnas = n;

    // Llenar matriz dispersa con números random entre 1 y 10
    printf("Llenando Matriz Dispersa......."); fflush(stdout);
    llenarMatrizDispersa(matriz, densidad);
    printf("Done!\n");fflush(stdout);

    if(n < 16) imprimirMatrizDispersa(matriz);

    // Generar vector de 1xN para multiplicar la matriz
    vector<float> vec(n);
    printf("Generando vector para multiplicar......."); fflush(stdout);
    generarVector(vec);
    printf("Done!\n");fflush(stdout);
    if(n < 16){
        printf("\nVector para multiplicar:\n"); fflush(stdout);
        imprimeVectorCol(vec);
    }

    if(modo == 0){ // MODO CPU
        // Multiplicar la matriz por el vector en modo CPU
        printf("\n\t######### MODO CPU #########");fflush(stdout);
        vector<float> vectorResultadoCPU(n,0.0f);
        double start_time_CPU = omp_get_wtime();
        multiplicarMatrizPorVectorCPU(matriz, vec, vectorResultadoCPU);
        double end_time_CPU = omp_get_wtime();
        double tiempo_total_CPU = end_time_CPU - start_time_CPU;

        if(n < 16){
            printf("\nResultado de la multiplicación CPU:\n"); fflush(stdout);
            imprimeVectorCol(vectorResultadoCPU);
        }

        printf("\nTiempo Total modo CPU (%i threads)= %f segudos\n", nt, tiempo_total_CPU);fflush(stdout);

    } else if(modo == 1){ // MODO GPU
        printf("\n\t######### MODO GPU #########");fflush(stdout);
        // Multiplicar la matriz por el vector en modo GPU
        vector<float> vectorResultadoGPU(n,0.0f);
        double tiempo_total_GPU;
        multiplicarMatrizPorVectorCUDA(matriz, vec, vectorResultadoGPU, tiempo_total_GPU);

        if(n < 16){
            printf("\nResultado de la multiplicación GPU:\n"); fflush(stdout);
            imprimeVectorCol(vectorResultadoGPU);
        }

        printf("\nTiempo Total modo GPU = %f segundos\n", tiempo_total_GPU);fflush(stdout);


    } else if(modo == 2){
        printf("\n\t####### MODO CPU y GPU #######");fflush(stdout);
        // Modo CPU y GPU a la vez para comprar
        // Multiplicar la matriz por el vector en modo CPU
        vector<float> vectorResultadoCPU(n,0.0f);
        double start_time_CPU = omp_get_wtime();
        multiplicarMatrizPorVectorCPU(matriz, vec, vectorResultadoCPU);
        double end_time_CPU = omp_get_wtime();
        double tiempo_total_CPU = end_time_CPU - start_time_CPU;

        // Multiplicar la matriz por el vector en modo GPU
        vector<float> vectorResultadoGPU(n,0.0f);
        double tiempo_total_GPU;
        multiplicarMatrizPorVectorCUDA(matriz, vec, vectorResultadoGPU, tiempo_total_GPU);

        // Imprimir el resultado
        if(n < 16){
            printf("\nResultado de la multiplicación CPU:\n"); fflush(stdout);
            imprimeVectorCol(vectorResultadoCPU);
            printf("\nResultado de la multiplicación GPU:\n"); fflush(stdout);
            imprimeVectorCol(vectorResultadoGPU);
        }

        // Imprimir tiempos totales
        printf("\nTiempo Total modo CPU (%i threads)= %f segudos\nTiempo Total modo GPU = %f segundos", nt, tiempo_total_CPU, tiempo_total_GPU);fflush(stdout);
        
        // Verificar que los resultados sean iguales
        if(verificarResultados(vectorResultadoCPU, vectorResultadoGPU)){ printf("\n\nModo CPU y modo GPU entregan el mismo resultado!\n"); fflush(stdout);}
        else {printf("\nAlgo salió mal, dan distintos resultados\n"); fflush(stdout);}
    }

    // Imprimir los elementos no nulos y su proporción (Para verificar la densidad)
    //imprimirElementosNoNulos(matriz);
    int elemNoNulos = matriz.elementosNoNulos.size();
    float proporcion = (float)matriz.elementosNoNulos.size() / ((float)n*(float)n);
    printf("\nHay %i de %i elementos no nulos (%f densidad real)\n", elemNoNulos, n*n, proporcion); fflush(stdout);

    float bytesConvencional = (float)(n*n*4)/(1024.0*1024.0);
    float bytes = (float)(elemNoNulos * 4)/(1024.0*1024.0);
    printf("Memoria de la matriz utilizada convencionalmente = %0.3f Mb\n", bytesConvencional);fflush(stdout);
    printf("Memoria de la matriz utilizada por nuestro método = %0.3f Mb\n", bytes);fflush(stdout);

    return 0;
}
