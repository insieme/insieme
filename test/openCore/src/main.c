#include <stdio.h>
#include <string.h>
#include <math.h>
#include "CLElemQMHS4.h"
//#include "SaveLoadQMHS4.h"
//#include "createContainer.h"
#include "utils.h"
#include "readData.h"
#include "saveData.h"

void alocNcpy(double** dest, double* src, size_t n, size_t width) {
	// allocate the exact amout of memory
	*dest = (double*)malloc(n * width * sizeof(double));
	// copy data
	memcpy(*dest, src, n * width * sizeof(double));
}

size_t setChangingFiles(const char* path, double** VecSig, double** VecStr, double** VecDsig,
		double** VecDstr, double** InvF,double** InvFT, double** G, double** P11q, double** P22, double** P21, double** Fe1q,
		double** Fe2, double** Fs, double** data) {
	size_t height = 1, width = 1, offset = 0;
	size_t elemsInObject = 0, initialsize = 1024;

	// load
	elemsInObject = readVector1D(data, &width, &initialsize, &offset, path, "sig.sav");
	// copy data
	alocNcpy(VecSig, *data, elemsInObject, width);

	// load
	elemsInObject = readVector1D(data, &width, &initialsize, &offset, path, "str.sav");
	// copy data
	alocNcpy(VecStr, *data, elemsInObject, width);

	// load
	elemsInObject = readVector1D(data, &width, &initialsize, &offset, path, "Dsig.sav");
	// copy data
	alocNcpy(VecDsig, *data, elemsInObject, width);

	// load
	elemsInObject = readVector1D(data, &width, &initialsize, &offset, path,  "Dstr.sav");
	// copy data
	alocNcpy(VecDstr, *data, elemsInObject, width);

	// load
	elemsInObject = readMatrix1DT(data, &height, &width, &initialsize, &offset, path, "invF.sav");
	// copy data
	alocNcpy(InvF, *data, elemsInObject, height * width);


	// load
	elemsInObject = readMatrix1DT(data, &height, &width, &initialsize, &offset, path, "invFT.sav");
	// copy data
	alocNcpy(InvFT, *data, elemsInObject, height * width);

	// handled separately because of inconsistency in file and actual size
	// load
//	elemsInObject = readMatrix1D(data, &height, &width, &initialsize, &offset, path, "G.sav");
	// copy data
//	alocNcpy(G, *data, elemsInObject, height * width);
//	*G = (double*)calloc(elemsInObject * 24 * 14, sizeof(double));

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
	alocNcpy(Fe1q, *data, elemsInObject, width);

	// load
	elemsInObject = readVector1D(data, &width, &initialsize, &offset, path, "fe2.sav");
	// copy data
	alocNcpy(Fe2, *data, elemsInObject, width);

	// load
	elemsInObject = readVector1D(data, &width, &initialsize, &offset, path, "fs.sav");
	// copy data
	alocNcpy(Fs, *data, elemsInObject, width);

	return elemsInObject;
}

size_t setAll(const char* filePath, const char* filenameAdditon, double** VecSig, double** VecStr, double** VecDsig,
		double** VecDstr, double** InvF,double** InvFT, double** G, double** P11q, double** P22, double** P21, double** T3, double** Fe1q,
		double** Fe2, double** Fs, double** ppu, double** lBase, double** XG, size_t** nrOfOrder, ght_hash_table_t** icoShellSections,
		ght_hash_table_t** elsets, ght_hash_table_t** issSizes, ght_hash_table_t** elsetSizes, ght_hash_table_t** matMap,
		struct CLElemQMHS4* cLElemQMHS4) {
	char path[512];
	size_t initialsize = 64;
	size_t height = 1, width = 1, num = 1, offset = 0;
	size_t elements = 0;
	size_t elemsInObject = 0;
	double* data = NULL;
	sprintf(path, "%s%s", filePath, filenameAdditon);

	elemsInObject = setChangingFiles(path, VecSig, VecStr, VecDsig, VecDstr, InvF, InvFT, G, P11q, P22, P21, Fe1q, Fe2, Fs, &data);

	// speacial handling for G. It is not read and size is not consitent with the size in the input file
	*G = (double*)calloc(elemsInObject * 24 * 14, sizeof(double));


	// load
	elements = readMatrix1D(&data, &height, &width, &initialsize, &offset, path, "T3.sav");
	// copy data
	alocNcpy(T3, data, elements, height * width);
	// TODO fix handling of T3
	cLElemQMHS4->T3Ptr = *T3;

	// load u
	// TODO works only if Manfred gives you the data for this iteration
	elemsInObject = readScalar(&data, &initialsize, path, "ppu.sav");
	// copy data
	alocNcpy(ppu, data, elemsInObject, 1);
	cLElemQMHS4->u = *ppu;

	// iteration independent stuff

	initialsize = elements;
	// load nrOfOrder
	elements = readScalarUint(nrOfOrder, &initialsize, filePath, "nrOfOrder.sav");
	cLElemQMHS4->nrOfOrder = *nrOfOrder;

	// load LBase
	elements = readBase(&data, &num, &height, &width, &initialsize, filePath, "lBase.out");
	// copy data
	alocNcpy(lBase, data, elements, num*height*width);
	cLElemQMHS4->LBase = *lBase;


	// load XG
	elemsInObject = readMatrix1D(&data, &width, &height, &initialsize, &offset, filePath, "XG.sav"); // height and width interchanged!
	// copy data
	alocNcpy(XG, data, elemsInObject, height*width);
	cLElemQMHS4->XG = *XG;


	// set fields
	cLElemQMHS4->sigPtr = *VecSig;
	cLElemQMHS4->strPtr = *VecStr;
	cLElemQMHS4->Dsig = *VecDsig;
	cLElemQMHS4->Dstr = *VecDstr;
	cLElemQMHS4->invF = *InvF;
	cLElemQMHS4->invFT = *InvFT;
	cLElemQMHS4->G = *G;
	cLElemQMHS4->P11q = *P11q;
	cLElemQMHS4->P22 = *P22;
	cLElemQMHS4->P21 = *P21;
	cLElemQMHS4->fe1q = *Fe1q;
	cLElemQMHS4->fe2 = *Fe2;
	cLElemQMHS4->fs = *Fs;


	// TODO put one element in an IcoShellSection
	struct Material* mat = (struct Material*)malloc(sizeof(struct Material));
	mat->materialName = "MAT";
	setMaterialElastic(mat, 6.825e7, 0.30);
	*matMap = ght_create(1);

	ght_insert((void*)*matMap, mat, sizeof(char*), mat->materialName);
	cLElemQMHS4->mat = *matMap;

	const char* elemName = "fake";
	struct icoSSData* isd = (struct icoSSData*)malloc(sizeof(struct icoSSData));
	isd->ug = -0.02;
	isd->og = 0.02;
	isd->intPoints = 5;
	isd->matName = mat->materialName;

	(*icoShellSections) = ght_create(1);
	ght_insert((void*)*icoShellSections, isd, sizeof(char*), elemName); //(*icoShellSections)[elemName] = new icoSSData[1];
	//(*icoShellSections)[elemName][0] = isd;
// 	std::map<const char*, size_t> issSizes;
	*issSizes = ght_create(1);
	size_t* iss = (size_t*)malloc(sizeof(size_t));
	*iss = 1u;
	ght_insert((void*)*issSizes, iss, sizeof(char*), elemName);
//	issSizes[elemName] = 1u;
	cLElemQMHS4->icoShellSections = *icoShellSections;
	cLElemQMHS4->issSize = *issSizes;

	// putting elemsInObject elements in elsets
	int** idxs = (int**)malloc(sizeof(int*));
	*idxs = (int*)malloc(elemsInObject * sizeof(int));
//	(*elsets)[elemName] = (int*)malloc(elemsInObject * sizeof(int));
	for(size_t i = 0; i < elemsInObject; ++i)
		(*idxs)[i] = (i+1);
	(*elsets) = ght_create(1);
	ght_insert((void*)*elsets, idxs, sizeof(char*), elemName);

	size_t* n = (size_t*)malloc(sizeof(size_t));
	*n = elemsInObject;

	*elsetSizes = ght_create(1);
	ght_insert((void*)*elsetSizes, n, sizeof(char*), elemName);

	cLElemQMHS4->elsets = *elsets;
	cLElemQMHS4->elsetSize = *elsetSizes;





	//set intersect to 0
	cLElemQMHS4->interSec = (int*)calloc(elements*4, sizeof(int));
	// set elements to 0
	cLElemQMHS4->elements = setElementsZero(elements);

	//set elast to 1
	cLElemQMHS4->elast = 1;

	free(data);

	return elements;
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

int checkResult(char* filePath, const char* filenameAdditon, double* VecSig, double* VecStr, double* VecDsig, double* VecDstr, double* InvF, double* InvFT, double* G,
		double* P11q, double* P22, double* P21, double* T3, double* Fe1q, double* Fe2, double* Fs, size_t nElems) {
	char path[512];
	sprintf(path, "%s%s", filePath, filenameAdditon);
	size_t elemsInObject;
	int ret = 0;

	double* VecSigRef = 0;
	double* VecStrRef = 0;
	double* VecDsigRef = 0;
	double* VecDstrRef = 0;
	double* InvFRef = 0;
	double* InvFTRef = 0;
	double* GRef = 0;
	double* P11qRef = 0;
	double* P22Ref = 0;
	double* P21Ref = 0;
	double* Fe1qRef = 0;
	double* Fe2Ref = 0;
	double* FsRef = 0;
	double* dataRef = NULL;

	elemsInObject = setChangingFiles(path, &VecSigRef, &VecStrRef, &VecDsigRef, &VecDstrRef, &InvFRef, &InvFTRef, &GRef, &P11qRef, &P22Ref, &P21Ref, &Fe1qRef,
			&Fe2Ref, &FsRef, &dataRef);

	if(cmp(VecSigRef, VecSig, elemsInObject, 1, 14, 14) < 0) {
		printf("\tin sig\n\n");
		ret = -1;
	}

	if(cmp(VecStrRef, VecStr, elemsInObject, 1, 20, 20) < 0) {
		printf("\tin str\n\n");
		ret = -1;
	}

	if(cmp(VecDsigRef, VecDsig, elemsInObject, 1, 14, 14) < 0) {
		printf("\tin Dsig\n\n");
		ret = -1;
	}

	if(cmp(VecDstrRef, VecDstr, elemsInObject, 1, 20, 20) < 0) {
		printf("\tin Dstr\n\n");
		ret = -1;
	}

	if(cmp(InvFRef, InvF, elemsInObject, 14, 14, 14*14) < 0) {
		printf("\tin InvF\n\n");
		ret = -1;
	}

	if(cmp(InvFTRef, InvFT, elemsInObject, 14, 14, 14*14) < 0) {
		printf("\tin InvFT\n\n");
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

	if(cmp(Fe1qRef, Fe1q, elemsInObject, 1, 14, 14) < 0) {
		printf("\tin Fe1q\n\n");
		ret = -1;
	}

	if(cmp(Fe2Ref, Fe2, elemsInObject, 1, 6, 6) < 0) {
		printf("\tin Fe2\n\n");
		ret = -1;
	}

	if(cmp(FsRef, Fs, elemsInObject, 1, 14, 14) < 0) {
		printf("\tin Fs\n\n");
		ret = -1;
	}

	free(VecSigRef);
	free(VecStrRef);
	free(VecDsigRef);
	free(VecDstrRef);
	free(InvFRef);
	free(InvFTRef);
	free(GRef);
	free(P11qRef);
	free(P22Ref);
	free(P21Ref);
	free(Fe1qRef);
	free(Fe2Ref);
	free(FsRef);
	free(dataRef);

	return ret;
}

void saveAll(char* filePath, const char* filenameAdditon, double* VecSig, double* VecStr, double* VecDsig, double* VecDstr, double* InvF, double* InvFT, double* G,
		double* P11q, double* P22, double* P21, double* T3, double* Fe1q, double* Fe2, double* Fs, size_t nElems, size_t nVertices) {
	char path[512];
	sprintf(path, "%s%s", filePath, filenameAdditon);

	saveVector(VecSig, nElems, 14, path, "sig.sav");

	saveVector(VecStr, nElems, 20, path, "str.sav");

	saveVector(VecDsig, nElems, 14, path, "Dsig.sav");

	saveVector(VecDstr, nElems, 20, path, "Dstr.sav");

	saveMatrix(InvF, nElems, 14, 14, 14*14, path, "invF.sav");

	saveMatrix(InvFT, nElems, 14, 14, 14*14, path, "invFT.sav");

	saveMatrix(G, nElems, 20, 14, 24*14, path, "G.sav");

	saveMatrix(P11q, nElems, 14, 14, 14*14, path, "P11q.sav");

	saveMatrix(P21, nElems, 6, 14, 6*14, path, "P21.sav");

	saveMatrix(P22, nElems, 6, 6, 6*6, path, "P22.sav");

	saveMatrixT(T3, nVertices, 3, 2, 3*2, path, "T3.sav");

	saveVector(Fe1q, nElems, 14, path, "fe1q.sav");

	saveVector(Fe2, nElems, 6, path, "fe2.sav");

	saveVector(Fs, nElems, 14, path, "fs.sav");
}



int main()  {
	struct CLElemQMHS4 cLElemQMHS4;

	double* VecSig = 0;
	double* VecStr = 0;
	double* VecDsig = 0;
	double* VecDstr = 0;
	double* InvF = 0;
	double* InvFT = 0;
	double* G = 0;
	double* P11q = 0;
	double* P22 = 0;
	double* P21 = 0;
	double* T3 = 0;
	double* Fe1q = 0;
	double* Fe2 = 0;
	double* Fs = 0;
	double* ppu = 0;
	double* lBase = 0;
	double* XG = 0;
	size_t* nrOfOrder = 0;
	ght_hash_table_t* icoShellSections = 0; //new std::map<const char*, icoSSData* >
	ght_hash_table_t* elsets = 0; //new std::map<const char*, int* >;
	ght_hash_table_t* issSizes = 0;
	ght_hash_table_t* elsetSizes = 0;
	ght_hash_table_t* matMap = 0;

	constructCLElemQMHS4(&cLElemQMHS4);

	// set up path to files
	char path[256];
	
#ifdef WIN32
	sprintf(path, "C:/Users/klois/uni/OpenCore/save/");
#else
	sprintf(path, "/home/klaus/dpsnfs/save/");
#endif
	char filenameAddition[128];
	sprintf(filenameAddition, "before-1-0.5-");

	printf("Reading input data from files\n");
	size_t elements = setAll(path, filenameAddition, &VecSig, &VecStr, &VecDsig, &VecDstr, &InvF, &InvFT, &G, &P11q, &P22, &P21, &T3, &Fe1q, &Fe2, &Fs,
			&ppu, &lBase, &XG, &nrOfOrder, &icoShellSections, &elsets, &issSizes, &elsetSizes, &matMap, &cLElemQMHS4);

	double* Kt = (double*)malloc(24 * 24 * elements * sizeof(double));
	double* fin = (double*)malloc(24 * elements * sizeof(double));
	int* elementsIntex = (int*)malloc(elements * sizeof(int));
	int* numDOFperIndex = (int*)malloc(elements * sizeof(int));

	printf("Calling initCalcQMHS4firstTime\n");
	if(initCalcQMHS4firstTime(&cLElemQMHS4, 1024, 1) != 0) {
		printf("initCalcQMHS4firstTime FAILED!\n");
		return -1;
	}

	printf("Calling calcQMHS4\n");
	if(calcQMHS4(&cLElemQMHS4, Kt, fin, elementsIntex, numDOFperIndex)) {
		printf("calcQMHS4 FAILED!\n");
		return -1;
	}

	printf("Checking result\n");
//	sprintf(path, "/home/klaus/dpsnfs/save/");
	sprintf(filenameAddition, "after-1-0.5-");
	if(checkResult(path, filenameAddition, VecSig, VecStr, VecDsig, VecDstr, InvF, InvFT, G, P11q, P22, P21, T3, Fe1q, Fe2, Fs, elements) == 0)
		printf("\tResult is correct\n");

	printf("Writing output files\n");
//	sprintf(path, "/home/klaus/dpsnfs/save/gc/");
	sprintf(filenameAddition, "gc/c-");
	saveAll(path, filenameAddition, VecSig, VecStr, VecDsig, VecDstr, InvF, InvFT, G, P11q, P22, P21, T3, Fe1q, Fe2, Fs, elements, 1088);

	destructCLElemQMHS4(&cLElemQMHS4);

	free( VecSig );
	free( VecStr );
	free( VecDsig );
	free( VecDstr );
	free( InvF );
	free( InvFT );
	free( G );
	free( P11q );
	free( P22 );
	free( P21 );
	free( T3 );
	free( Fe1q );
	free( Fe2 );
	free( Fs );
	free( ppu );
	free( lBase );
	free( XG );
	free( nrOfOrder );

	free(Kt);
	free(fin);
	free(elementsIntex);
	free(numDOFperIndex);

	// take care of correctly removing double pointers
	void* tmpPtr = 0;
	void* tmpKey = 0;
	ght_iterator_t iterator;
	for(tmpPtr = ght_first(elsets, &iterator, (const void**)&tmpKey); tmpPtr;
			tmpPtr = ght_next(elsets, &iterator, (const void**)&tmpKey)) {
		free(*(int**)tmpPtr);
		free(tmpPtr);
	}
	ght_finalize(icoShellSections);
	ght_finalize(elsets);
	ght_finalize(issSizes);
	ght_finalize(elsetSizes);
	ght_finalize(matMap);

	return 0;
}
