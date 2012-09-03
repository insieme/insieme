#pragma once

size_t setAll(const char* filePath, const char* filenameAdditon, double** sigPtr, double** strPtr, double** Dsig,
        double** Dstr, double** invF,double** invFT, double** G, double** P11q, double** P22, double** P21, double** T3Ptr, double** fe1q,
        double** fe2, double** fs, double** u, double** LBase, double** XG, size_t** nrOfOrder, ght_hash_table_t** icoShellSections,
        ght_hash_table_t** elsets, ght_hash_table_t** issSize, ght_hash_table_t** elsetSize, ght_hash_table_t** mat, int** interSec, 
        ght_hash_table_t** elements, int* elast);

int checkResult(char* filePath, const char* filenameAdditon, double* sigPtr, double* strPtr, double* Dsig, double* Dstr, double* invF, double* invFT, double* G,
        double* P11q, double* P22, double* P21, double* T3Ptr, double* fe1q, double* fe2, double* fs, size_t nElems);

void saveAll(char* filePath, const char* filenameAdditon, double* sigPtr, double* strPtr, double* Dsig, double* Dstr, double* invF, double* invFT, double* G,
        double* P11q, double* P22, double* P21, double* T3Ptr, double* fe1q, double* fe2, double* fs, size_t nElems, size_t nVertices);
