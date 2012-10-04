#pragma once

#include <map>
#include "IcoVec.hpp"
#include "IcoMat.hpp"

typedef std::map<int, IcoVec> IcoVecMap;
typedef std::map<int, IcoMat> IcoMatMap;

void createIcoVecMap(double** in, const size_t nElems, const size_t len, IcoVecMap* out, size_t offset);
void createIcoMatMap(double **in, size_t nElems, size_t height, size_t width, IcoMatMap* InvF, size_t offset);
