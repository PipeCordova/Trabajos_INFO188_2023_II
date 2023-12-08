#include <iostream>
#include <vector>
#include <omp.h>

using namespace std;

// Estructura para representar un elemento no nulo
struct ElementoNoNulo {
    int fila;
    int columna;
    int valor;
};

// Estructura para representar una matriz dispersa
struct MatrizDispersa {
    int filas;
    int columnas;
    vector<ElementoNoNulo> elementosNoNulos;
};

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

// Función para imprimir los elementos no nulos de una matriz dispersa
void imprimirElementosNoNulos(const MatrizDispersa& matriz) {
    cout << "Elementos no nulos:" << endl;
    for (const ElementoNoNulo& elemento : matriz.elementosNoNulos) {
        cout << "(" << elemento.fila << ", " << elemento.columna << ") = " << elemento.valor << endl;
    }
}

// Función para llenar una matriz dispersa con una densidad dada
void llenarMatrizDispersa(MatrizDispersa& matriz, double densidad) {
    // Limpiar elementos existentes
    matriz.elementosNoNulos.clear();

    // Generar elementos no nulos aleatorios
    for (int i = 0; i < matriz.filas; ++i) {
        for (int j = 0; j < matriz.columnas; ++j) {
            if ((rand() % 100) < (densidad * 100)) {
                int valor = rand() % 10 + 1;  // Valor aleatorio entre 1 y 10
                matriz.elementosNoNulos.push_back({i, j, valor});
            }
        }
    }
} 

// Función para multiplicar una matriz dispersa por un vector (Parelelismo)
void multiplicarMatrizPorVectorCPU(const MatrizDispersa& matriz, const vector<int>& vec, vector<int>& vectorResultado) {
    if (matriz.columnas != vec.size() || matriz.filas != vectorResultado.size()) {
        cerr << "Error: Las dimensiones de la matriz y el vector no son compatibles para la multiplicación." << endl;
        exit(EXIT_FAILURE);
    }

    #pragma omp parallel for
    for (int i = 0; i < matriz.elementosNoNulos.size(); ++i) {
        const ElementoNoNulo& elemento = matriz.elementosNoNulos[i];
        #pragma omp atomic
        vectorResultado[elemento.fila] += elemento.valor * vec[elemento.columna];
    }
}

// Función para multiplicar una matriz dispersa por un vector (Normal)
void multiplicarMatrizPorVectorNormal(const MatrizDispersa& matriz, const vector<int>& vec, vector<int>& vectorResultado) {
    if (matriz.columnas != vec.size() || matriz.filas != vectorResultado.size()) {
        cerr << "Error: Las dimensiones de la matriz y el vector no son compatibles para la multiplicación." << endl;
        exit(EXIT_FAILURE);
    }
    for (const ElementoNoNulo& elemento : matriz.elementosNoNulos) vectorResultado[elemento.fila] += elemento.valor * vec[elemento.columna];
}

void generarVector(vector<int>& vec) {
    for (int i = 0; i < vec.size(); ++i) {
        vec[i] = rand() % 10 + 1; // Números aleatorios entre 1 y 10
    }
}

void imprimeVectorCol(const vector<int>& v){
    for(int i=0 ; i<v.size() ; i++) printf("%i\n", v[i]);
}

int main(int argc, char *argv[]) {
    if(argc != 6){
        cerr << "Ejecutar como: ./prog <n> <d> <s> <m> <nt>\nn = tamaño matriz\nd = densidad números no nulos (0 < d < 1)\ns = semilla de números aleatorios\nm = modo CPU (0) o GPU (1)\nnt = número threads de la CPU" << endl;
        exit(EXIT_FAILURE);
    }
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

    printf("Llenando Matriz Dispersa......."); fflush(stdout);
    llenarMatrizDispersa(matriz, densidad);
    printf("Done!\n");fflush(stdout);

    if(n < 16) imprimirMatrizDispersa(matriz);

    vector<int> vec(n);
    printf("Generando vector para multiplicar......."); fflush(stdout);
    generarVector(vec);
    printf("Done!\n");fflush(stdout);
    vector<int> vectorResultadoCPU(n,0);
    vector<int> vectorResultadoNormal(n,0);
    if(n < 16){
        printf("\nVector para multiplicar:\n"); fflush(stdout);
        imprimeVectorCol(vec);
    }

    // Multiplicar la matriz por el vector
    double start_time_normal = omp_get_wtime();
    multiplicarMatrizPorVectorNormal(matriz, vec, vectorResultadoNormal);
    double end_time_normal = omp_get_wtime();
    double tiempo_total_normal = end_time_normal - start_time_normal;

    double start_time_CPU = omp_get_wtime();
    multiplicarMatrizPorVectorCPU(matriz, vec, vectorResultadoCPU);
    double end_time_CPU = omp_get_wtime();
    double tiempo_total_CPU = end_time_CPU - start_time_CPU;

    // Imprimir el resultado
    if(n < 16){
        printf("\nResultado de la multiplicación CPU:\n"); fflush(stdout);
        imprimeVectorCol(vectorResultadoCPU);
    }

    printf("\nTiempo Total modo normal = %f segundos\nTiempo Total modo CPU (%i threads)= %f segudos",tiempo_total_normal, nt, tiempo_total_CPU);fflush(stdout);

    if(vectorResultadoCPU == vectorResultadoNormal){ printf("\n\nModo CPU y modo Normal entregan el mismo resultado!\n"); fflush(stdout);}
    else {printf("\nAlgo salió mal, CPU y Normal dan distintos resultados\n"); fflush(stdout);}

    // Imprimir los elementos no nulos
    //imprimirElementosNoNulos(matriz);
    int elemNoNulos = matriz.elementosNoNulos.size();
    float proporcion = (float)matriz.elementosNoNulos.size() / ((float)n*(float)n);
    printf("Hay %i de %i elementos no nulos (%f)\n", elemNoNulos, n*n, proporcion); fflush(stdout);

    return 0;
}
