#include <iostream>
#include <ctime>
#include <cstdlib>
#include <vector>
#include <omp.h>

using namespace std;

struct SparseMatrix {
    int n;           // Lado de la matriz
    float* values;   // Valores no cero
    int* col_indices; // Índices de columna correspondientes a los valores
    int* row_ptr;     // Índices de inicio de fila
};

void generateSparseMatrix(SparseMatrix& Md, float density, int seed) {
    // Implementa la generación de la matriz dispersa aquí
}

void generateVector(vector<float>& v, int n, int seed) {
    srand(seed);
    for (int i = 0; i < n; ++i) {
        v.push_back(static_cast<float>(rand()) / RAND_MAX);
    }
}

void sparseMatrixVectorProductCPU(const SparseMatrix& Md, const vector<float>& v, vector<float>& result, int num_threads) {
    // Implementa la multiplicación de matriz dispersa por vector en CPU aquí
}

int main(int argc, char* argv[]) {
    if (argc != 6) {
        cerr << "Uso: ./prog <n> <d> <m> <s> <nt>" << endl;
        return 1;
    }

    int n = atoi(argv[1]);
    float density = atof(argv[2]);
    int mode = atoi(argv[3]);
    int seed = atoi(argv[4]);
    int num_threads = atoi(argv[5]);

    SparseMatrix Md;
    Md.n = n;
    generateSparseMatrix(Md, density, seed);

    vector<float> v;
    generateVector(v, n, seed);


    // imprimiento vector n x 1
    // for (size_t i = 0; i < n; i++){
    //     cout << v[i] << endl;
    // }
    // cout << endl;
    

    vector<float> result(n, 0.0f);

    clock_t start_time, end_time;
    start_time = clock();

    // Establecer el número de threads para OpenMP
    omp_set_num_threads(num_threads);

    sparseMatrixVectorProductCPU(Md, v, result, num_threads);

    end_time = clock();
    double cpu_time = static_cast<double>(end_time - start_time) / CLOCKS_PER_SEC;

    cout << "Tiempo de ejecución en CPU: " << cpu_time << " segundos" << endl;

    // Liberar memoria
    delete[] Md.values;
    delete[] Md.col_indices;
    delete[] Md.row_ptr;

    return 0;
}
