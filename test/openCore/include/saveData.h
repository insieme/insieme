#pragma once
#include <stdlib.h>

void saveScalars(const double* ptr, const size_t n, const char* pathname, const char* filename);
void saveUintScalars(const size_t* ptr, const size_t n,const  char* pathname, const char* filename);
void saveVector(const double* ptr, const size_t nElem, const size_t len, const char* pathname, const char* filename);
void saveMatrix(const double* ptr, const size_t nElem, const size_t height, const size_t width, const size_t matrixsize, const char* pathname,const  char* filename);
void saveMatrixT(const double* ptr, const size_t nElem, const size_t height, const size_t width, const size_t matrixsize, const char* pathname,const  char* filename);
