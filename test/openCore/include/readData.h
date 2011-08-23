#pragma once
#include <stdlib.h>

#define FAIL(str) { printf(str); return 0; }
#define max(a, b) a > b ? a : b;

size_t readScalarUint(size_t** data, size_t* initialSize, const char* pathname, const char* filename);
size_t readScalar(double** data, size_t* initialSize, const char* pathname, const char* filename);
size_t readVector(double*** data, size_t* len, size_t* initialSize, size_t* , const char* pathname, const char* filename);
size_t readVector1D(double** dataPtr, size_t* len, size_t* initialSize, size_t* offset, const char* pathname, const char* filename);
size_t readMatrix(double*** data, size_t* height, size_t* width, size_t* initialSize, size_t* offset, const char* pathname, const char* filename);
size_t readMatrix1D(double** dataPtr, size_t* height, size_t* width, size_t* initialSize, size_t* offset, const char* pathname, const char* filename);
size_t readMatrix1DT(double** dataPtr, size_t* height, size_t* width, size_t* size, size_t* offset, const char* pathname, const char* filename);
size_t readBase(double** dataPtr/*, char** name*/, size_t* num, size_t* height, size_t* width, size_t* initialSize, const char* pathname, const char* filename);

