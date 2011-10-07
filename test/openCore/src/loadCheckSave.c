#include <stdio.h>
#include <math.h>
#include <string.h>
#include <CL/cl.h>

#include "ght_hash_table.h"
#include "config.h"
#include "utils.h"
#include "Material.h"
#include "Element.h"
#include "readData.h"
#include "saveData.h"
#include "loadCheckSave.h"


void alocNcpy(double** dest, double* src, size_t n, size_t width) {
    // allocate the exact amout of memory
    *dest = (double*)malloc(n * width * sizeof(double));
    // copy data
    memcpy(*dest, src, n * width * sizeof(double));
}

size_t setChangingFiles(const char* path, double** sigPtr, double** strPtr, double** Dsig,
        double** Dstr, double** invF,double** invFT, double** G, double** P11q, double** P22, double** P21, double** fe1q,
        double** fe2, double** fs, double** data) {
    size_t height = 1, width = 1, offset = 0;
    size_t elemsInObject = 0, initialsize = 1024;

    // load
    elemsInObject = readVector1D(data, &width, &initialsize, &offset, path, "sig.sav");
    // copy data
    alocNcpy(sigPtr, *data, elemsInObject, width);

    // load
    elemsInObject = readVector1D(data, &width, &initialsize, &offset, path, "str.sav");
    // copy data
    alocNcpy(strPtr, *data, elemsInObject, width);

    // load
    elemsInObject = readVector1D(data, &width, &initialsize, &offset, path, "Dsig.sav");
    // copy data
    alocNcpy(Dsig, *data, elemsInObject, width);

    // load
    elemsInObject = readVector1D(data, &width, &initialsize, &offset, path,  "Dstr.sav");
    // copy data
    alocNcpy(Dstr, *data, elemsInObject, width);

    // load
    elemsInObject = readMatrix1DT(data, &height, &width, &initialsize, &offset, path, "invF.sav");
    // copy data
    alocNcpy(invF, *data, elemsInObject, height * width);


    // load
    elemsInObject = readMatrix1DT(data, &height, &width, &initialsize, &offset, path, "invFT.sav");
    // copy data
    alocNcpy(invFT, *data, elemsInObject, height * width);

    // handled separately because of inconsistency in file and actual size
    // load
//    elemsInObject = readMatrix1D(data, &height, &width, &initialsize, &offset, path, "G.sav");
    // copy data
//    alocNcpy(G, *data, elemsInObject, height * width);
//    *G = (double*)calloc(elemsInObject * 24 * 14, sizeof(double));

    // load
    elemsInObject = readMatrix1DT(data, &height, &width, &initialsize, &offset, path, "P11q.sav");
    // copy data
    alocNcpy(P11q, *data, elemsInObject, height * width);

    // load
    elemsInObject = readMatrix1DT(data, &height, &width, &initialsize, &offset, path, "P22.sav");
    // copy data
    alocNcpy(P22, *data, elemsInObject, height * width);

    // load
    elemsInObject = readMatrix1DT(data, &height, &width, &initialsize, &offset, path, "P21.sav");
    // copy data
    alocNcpy(P21, *data, elemsInObject, height * width);

    // load
    elemsInObject = readVector1D(data, &width, &initialsize, &offset, path, "fe1q.sav");
    // copy data
    alocNcpy(fe1q, *data, elemsInObject, width);

    // load
    elemsInObject = readVector1D(data, &width, &initialsize, &offset, path, "fe2.sav");
    // copy data
    alocNcpy(fe2, *data, elemsInObject, width);

    // load
    elemsInObject = readVector1D(data, &width, &initialsize, &offset, path, "fs.sav");
    // copy data
    alocNcpy(fs, *data, elemsInObject, width);

    return elemsInObject;
}

size_t setAll(const char* filePath, const char* filenameAdditon, double** sigPtr, double** strPtr, double** Dsig,
        double** Dstr, double** invF,double** invFT, double** G, double** P11q, double** P22, double** P21, double** T3Ptr, double** fe1q,
        double** fe2, double** fs, double** u, double** LBase, double** XG, size_t** nrOfOrder, ght_hash_table_t** icoShellSections,
        ght_hash_table_t** elsets, ght_hash_table_t** issSize, ght_hash_table_t** elsetSize, ght_hash_table_t** mat, int** interSec, 
        ght_hash_table_t** elements, int* elast, size_t* vertices) {
    char path[512];
    size_t initialsize = 64;
    size_t height = 1, width = 1, num = 1, offset = 0;
    size_t elems = 0;
    double* data = NULL;
    sprintf(path, "%s%s", filePath, filenameAdditon);

    *vertices = setChangingFiles(path, sigPtr, strPtr, Dsig, Dstr, invF, invFT, G, P11q, P22, P21, fe1q, fe2, fs, &data);

    // speacial handling for G. It is not read and size is not consitent with the size in the input file
    *G = (double*)calloc(*vertices * 24 * 14, sizeof(double));


    // load
    elems = readMatrix1D(&data, &height, &width, &initialsize, &offset, path, "T3.sav");
    // copy data
    alocNcpy(T3Ptr, data, elems, height * width);
    // TODO fix handling of T3Ptr

    // load u
    // TODO works only if Manfred gives you the data for this iteration
    *vertices = readScalar(&data, &initialsize, path, "ppu.sav");
    // copy data
    alocNcpy(u, data, *vertices, 1);

    // iteration independent stuff

    initialsize = elems;
    // load nrOfOrder
    elems = readScalarUint(nrOfOrder, &initialsize, filePath, "nrOfOrder.sav");

    // load LBase
    elems = readBase(&data, &num, &height, &width, &initialsize, filePath, "lBase.out");
    // copy data
    alocNcpy(LBase, data, elems, num*height*width);


    // load XG
    *vertices = readMatrix1D(&data, &width, &height, &initialsize, &offset, filePath, "XG.sav"); // height and width interchanged!
    // copy data
    alocNcpy(XG, data, *vertices, height*width);

    // TODO put one element in an IcoShellSection
    struct Material* mat_ = (struct Material*)malloc(sizeof(struct Material));
    mat_->materialName = "MAT";
    setMaterialElastic(mat_, 6.825e7, 0.30);
    *mat = ght_create(1);

    ght_insert((void*)*mat, mat_, sizeof(char*), mat_->materialName);

    const char* elemName = "fake";
    struct icoSSData* isd = (struct icoSSData*)malloc(sizeof(struct icoSSData));
    isd->ug = -0.02;
    isd->og = 0.02;
    isd->intPoints = 5;
    isd->matName = mat_->materialName;

    (*icoShellSections) = ght_create(1);
    ght_insert((void*)*icoShellSections, isd, sizeof(char*), elemName); //(*icoShellSections)[elemName] = new icoSSData[1];
    //(*icoShellSections)[elemName][0] = isd;
//     std::map<const char*, size_t> issSize;
    *issSize = ght_create(1);
    size_t* iss = (size_t*)malloc(sizeof(size_t));
    *iss = 1u;
    ght_insert((void*)*issSize, iss, sizeof(char*), elemName);

    // putting *vertices elements in elsets
    int** idxs = (int**)malloc(sizeof(int*));
    *idxs = (int*)malloc(*vertices * sizeof(int));
//    (*elsets)[elemName] = (int*)malloc(*vertices * sizeof(int));
    for(size_t i = 0; i < *vertices; ++i)
        (*idxs)[i] = (i+1);
    (*elsets) = ght_create(1);
    ght_insert((void*)*elsets, idxs, sizeof(char*), elemName);

    size_t* n = (size_t*)malloc(sizeof(size_t));
    *n = *vertices;

    *elsetSize = ght_create(1);
    ght_insert((void*)*elsetSize, n, sizeof(char*), elemName);

    //set intersect to 0
    *interSec = (int*)calloc(elems*4, sizeof(int));
    // set elements to 0
	*elements = ght_create(elems);
	int nodeNumbers = 1;
	// start counting at 1!!!!
	for(size_t i = 1; i <= elems; ++i) {
		struct Element entry = createElement(i, "QMHS4", &nodeNumbers, 1);
		ght_insert(*elements, &entry, sizeof(int), &i);
	}

    //set elast to 1
    *elast = 1;

    free(data);

    return elems;
}

int cmp(double* ref, double* data, size_t nElems, size_t height, size_t width, size_t memsize) {
    size_t matrixsize = height * width;
    for(size_t n = 0; n < nElems; ++n)
        for(size_t i = 0; i < height; ++i)
            for(size_t j = 0; j < width; ++j) {
                // 1% of error isa allowed
                double a = ref[n*matrixsize + i * width + j];
                double b = data[n*memsize + i * width + j];
                if(fabs(a - b) > fabs(a) * 0.1) {
                    printf("Error at %lu [%lu,%lu]: %g \nexpected:\t%g\nactual\t\t%g\n", n, i, j, a - b, a, b);
                    return -1;
                }
            }
    return 0;
}

int checkResult(char* filePath, const char* filenameAdditon, double* sigPtr, double* strPtr, double* Dsig, double* Dstr, double* invF, double* invFT, double* G,
        double* P11q, double* P22, double* P21, double* T3Ptr, double* fe1q, double* fe2, double* fs, size_t nElems) {
    char path[512];
    sprintf(path, "%s%s", filePath, filenameAdditon);
    size_t elemsInObject;
    int ret = 0;

    double* sigPtrRef = 0;
    double* strPtrRef = 0;
    double* DsigRef = 0;
    double* DstrRef = 0;
    double* invFRef = 0;
    double* invFTRef = 0;
    double* GRef = 0;
    double* P11qRef = 0;
    double* P22Ref = 0;
    double* P21Ref = 0;
    double* fe1qRef = 0;
    double* fe2Ref = 0;
    double* fsRef = 0;
    double* dataRef = NULL;

    elemsInObject = setChangingFiles(path, &sigPtrRef, &strPtrRef, &DsigRef, &DstrRef, &invFRef, &invFTRef, &GRef, &P11qRef, &P22Ref, &P21Ref, &fe1qRef,
            &fe2Ref, &fsRef, &dataRef);

    if(cmp(sigPtrRef, sigPtr, elemsInObject, 1, 14, 14) < 0) {
        printf("\tin sig\n\n");
        ret = -1;
    }

    if(cmp(strPtrRef, strPtr, elemsInObject, 1, 20, 20) < 0) {
        printf("\tin str\n\n");
        ret = -1;
    }

    if(cmp(DsigRef, Dsig, elemsInObject, 1, 14, 14) < 0) {
        printf("\tin Dsig\n\n");
        ret = -1;
    }

    if(cmp(DstrRef, Dstr, elemsInObject, 1, 20, 20) < 0) {
        printf("\tin Dstr\n\n");
        ret = -1;
    }

    if(cmp(invFRef, invF, elemsInObject, 14, 14, 14*14) < 0) {
        printf("\tin invF\n\n");
        ret = -1;
    }

    if(cmp(invFTRef, invFT, elemsInObject, 14, 14, 14*14) < 0) {
        printf("\tin invFT\n\n");
        ret = -1;
    }

    size_t height = 0, width = 0, initialsize = 1024, offset = 1;
    readMatrix1DT(&GRef, &height, &width, &initialsize, &offset, path, "G.sav");
    // copy data
    if(cmp(GRef, G, elemsInObject, 20, 14, 24*14) < 0) {
        printf("\tin G\n\n");
        ret = -1;
    }

    if(cmp(P11qRef, P11q, elemsInObject, 14, 14, 14*14) < 0) {
        printf("\tin P11q\n\n");
        ret = -1;
    }

    if(cmp(P21Ref, P21, elemsInObject, 6, 14, 6*14) < 0) {
        printf("\tin P21\n\n");
        ret = -1;
    }

    if(cmp(P22Ref, P22, elemsInObject, 6, 6, 6*6) < 0) {
        printf("\tin P22\n\n");
        ret = -1;
    }

    if(cmp(fe1qRef, fe1q, elemsInObject, 1, 14, 14) < 0) {
        printf("\tin fe1q\n\n");
        ret = -1;
    }

    if(cmp(fe2Ref, fe2, elemsInObject, 1, 6, 6) < 0) {
        printf("\tin fe2\n\n");
        ret = -1;
    }

    if(cmp(fsRef, fs, elemsInObject, 1, 14, 14) < 0) {
        printf("\tin fs\n\n");
        ret = -1;
    }

    free(sigPtrRef);
    free(strPtrRef);
    free(DsigRef);
    free(DstrRef);
    free(invFRef);
    free(invFTRef);
    free(GRef);
    free(P11qRef);
    free(P22Ref);
    free(P21Ref);
    free(fe1qRef);
    free(fe2Ref);
    free(fsRef);
    free(dataRef);

    return ret;
}

void saveAll(char* filePath, const char* filenameAdditon, double* sigPtr, double* strPtr, double* Dsig, double* Dstr, double* invF, double* invFT, double* G,
        double* P11q, double* P22, double* P21, double* T3Ptr, double* fe1q, double* fe2, double* fs, size_t nElems, size_t nVertices) {
    char path[512];
    sprintf(path, "%s%s", filePath, filenameAdditon);

    saveVector(sigPtr, nElems, 14, path, "sig.sav");

    saveVector(strPtr, nElems, 20, path, "str.sav");

    saveVector(Dsig, nElems, 14, path, "Dsig.sav");

    saveVector(Dstr, nElems, 20, path, "Dstr.sav");

    saveMatrix(invF, nElems, 14, 14, 14*14, path, "invF.sav");

    saveMatrix(invFT, nElems, 14, 14, 14*14, path, "invFT.sav");

    saveMatrix(G, nElems, 20, 14, 24*14, path, "G.sav");

    saveMatrix(P11q, nElems, 14, 14, 14*14, path, "P11q.sav");

    saveMatrix(P21, nElems, 6, 14, 6*14, path, "P21.sav");

    saveMatrix(P22, nElems, 6, 6, 6*6, path, "P22.sav");

    saveMatrixT(T3Ptr, nVertices, 3, 2, 3*2, path, "T3.sav");

    saveVector(fe1q, nElems, 14, path, "fe1q.sav");

    saveVector(fe2, nElems, 6, path, "fe2.sav");

    saveVector(fs, nElems, 14, path, "fs.sav");
}

