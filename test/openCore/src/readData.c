#include <stdio.h>
#include <math.h>
#include <string.h>
#include "readData.h"
#ifndef UNIX
#include "windows.h"
#endif

FILE* openFileR(const char* pathname, const char* filename) {
	char path[512];
	sprintf(path, "%s%s", pathname, filename);

	FILE* file = fopen(path, "r");

	if(!file) {
		fprintf(stderr, "Cannot open file %s for reading\n", path);
		FAIL("Aborting\n");
	}

	return file;
}

size_t resize(double*** dataPtr, size_t size, size_t idx) {
	size_t newSize = max(size*2, idx);
	*dataPtr = (double**)realloc(*dataPtr, newSize * sizeof(double*));
	memset(*dataPtr + size, 0, newSize - size);
	return newSize;
}

size_t resize1D(double** dataPtr, size_t size, size_t elemsNeeded) {
	size_t newSize = max(size*2, elemsNeeded);
//printf("New size is %lu\n", newSize * sizeof(double));
	*dataPtr = (double*)realloc(*dataPtr, newSize * sizeof(double));
// does not seem to be needed coz i always write continuously
//	memset(dataPtr + size, 0, newSize - size);
	return newSize;
}

size_t checkEol(FILE* in) {
	char buf[128] = {'\0'};
	if(fscanf(in, "%[^\n]", buf) < 0 || strlen(buf) > 1) {
		fprintf(stderr, "Read %s\n", buf);
		FAIL("Unexpected elements\n");
	}
	return 1;
}

size_t readScalarUint(size_t** dataPtr, size_t* size, const char* pathname, const char* filename) {
	FILE* in = openFileR(pathname, filename);

	if(*dataPtr == NULL)
		*dataPtr = (size_t*)calloc(*size, sizeof(size_t));

	size_t i = 0, maxI = 0;
	size_t tmp;

	while(fscanf(in, "%lu", &tmp) > 0) {
		if(i == *size) {
			*size *= 2;
			*dataPtr = (size_t*)realloc(*dataPtr, *size * sizeof(size_t));
		}
		maxI = max(i, maxI);
		*(*dataPtr+i) = tmp;

		++i;
	}

	fclose(in);
	return maxI+1;
}

size_t readScalar(double** dataPtr, size_t* size, const char* pathname, const char* filename) {
	FILE* in = openFileR(pathname, filename);

	if(*dataPtr == NULL)
		*dataPtr = (double*)calloc(*size, sizeof(double));

	size_t i = 0, maxI = 0;
	double tmp;

	while(fscanf(in, "%lf", &tmp) > 0) {
		if(i == *size) {
			*size *= 2;
			*dataPtr = (double*)realloc(*dataPtr, *size * sizeof(double));
		}
		maxI = max(i, maxI);
		*(*dataPtr+i) = tmp;

		++i;
	}

	fclose(in);
	return maxI+1;
}


size_t readVector(double*** dataPtr, size_t* len, size_t* size, size_t* offset, const char* pathname, const char* filename) {
	FILE* in = openFileR(pathname, filename);

	if(*dataPtr == NULL)
		*dataPtr = (double**)calloc(*size, sizeof(double*));
	double** data = *dataPtr;

	size_t i = 0, maxI = 0;

	FILE* check = openFileR(pathname, filename);
	fscanf(check, "%lu", offset);
	fclose(check);

	while(fscanf(in, "%lu", &i) > 0) {
		// subtract the starting offset
		i -= *offset;
		maxI = max(i, maxI);
		if(i >= *size) {
			*size = resize(dataPtr, *size, i);
			data = *dataPtr;
		}

		if(fscanf(in, "%lu", len) <= 0)
			FAIL("Cannot read length of vector\n");

		data[i] = (double*)malloc(*len * sizeof(double));
		for(size_t j = 0; j < *len; ++j) {
			if(fscanf(in, "%lf", data[i]+j) <= 0)
				FAIL("Error while reading vector elements\n");
		}

		// check if there is anything remaining in this line
		if(!checkEol(in))
			return 0;
	}

	fclose(in);
	return maxI+1;
}

size_t readVector1D(double** dataPtr, size_t* len, size_t* size, size_t* offset, const char* pathname, const char* filename) {
	FILE* in = openFileR(pathname, filename);

	if(*dataPtr == NULL)
		*dataPtr = (double*)malloc(*size * sizeof(double));

	size_t i = 0, maxI = 0;

	FILE* check = openFileR(pathname, filename);
	fscanf(check, "%lu", offset);
	fclose(check);

	while(fscanf(in, "%lu", &i) > 0) {
		// subtract the starting offset
		i -= *offset;
		maxI = max(i, maxI);
		if(fscanf(in, "%lu", len) <= 0)
			FAIL("Cannot read length of vector\n");

		// assuming all vectors have the same length, otherwise final values for len would be wrong for some vectors
		if((i+1)* *len >= *size)
			*size = resize1D(dataPtr, *size, (i+1)* *len);

		for(size_t j = 0; j < *len; ++j) {
			if(fscanf(in, "%lf", &(*dataPtr)[i* *len + j]) <= 0)
				FAIL("Error while reading vector elements\n");
		}

		// check if there is anything remaining in this line
		if(!checkEol(in))
			return 0;
	}

	fclose(in);
	return maxI+1;
}

// matrix is transposed at write

size_t readMatrix(double*** dataPtr, size_t* height, size_t* width, size_t* size, size_t* offset, const char* pathname, const char* filename) {
	FILE* in = openFileR(pathname, filename);

	if(*dataPtr == NULL)
		*dataPtr = (double**)calloc(*size, sizeof(double*));

	double** data = *dataPtr;

	char buf[128] = {'\0'};
	size_t i = 0, maxI = 0;

	FILE* check = openFileR(pathname, filename);
	fscanf(check, "%lu", offset);
	fclose(check);

	while(fscanf(in, "%lu", &i) > 0) {
		// subtract the starting offset
		i -= *offset;
		if(i >= *size) {
			*size = resize(dataPtr, *size, i);
			data = *dataPtr;
		}

		maxI = max(i, maxI);
		if(fscanf(in, "%lu", height) <= 0)
			FAIL("Cannot read height of matrix\n");
		if(fscanf(in, "%lu", width) <= 0)
			FAIL("Cannot read width of matrix\n");
		// check if there is anything remaining in this line
		if(fscanf(in, "%[^\n]", buf) < 0 || strlen(buf) > 1)
			FAIL("Unexpected matrix properties");
		// assuming all matrices have the same size, otherwise final values for height and width would be wrong for some matrices
		size_t nElems = *height * *width;
		data[i] = (double*)malloc(nElems * sizeof(double));
		for(size_t j = 0; j < *height; ++j) {
			for(size_t k = 0; k < *width; ++k)
				if(fscanf(in, "%lf", data[i]+(k* *height + j)) <= 0)
					FAIL("Error while reading matrix elements\n");
		}

		// check if there is anything remaining in this line
		if(!checkEol(in))
			return 0;
	}

	fclose(in);
	return maxI+1;
}

// matrix is transposed at write
size_t readMatrix1D(double** dataPtr, size_t* height, size_t* width, size_t* size, size_t* offset, const char* pathname, const char* filename) {
	FILE* in = openFileR(pathname, filename);

	if(*dataPtr == NULL)
		*dataPtr = (double*)malloc(*size * sizeof(double));

	size_t i = 0, maxI = 0;

	FILE* check = openFileR(pathname, filename);
	fscanf(check, "%lu", offset);
	fclose(check);

	while(fscanf(in, "%lu", &i) > 0) {
		// subtract the starting offset
		i -= *offset;
		maxI = max(i, maxI);
		if(fscanf(in, "%lu", height) <= 0)
			FAIL("Cannot read height of matrix\n");
		if(fscanf(in, "%lu", width) <= 0)
			FAIL("Cannot read width of matrix\n");
		// check if there is anything remaining in this line
		if(!checkEol(in))
			return 0;

		// assuming all matrices have the same size, otherwise final values for height and width would be wrong for some matrices
		size_t matrixSize = *height * *width;
		if((i+1)*matrixSize >= *size)
		*size = resize1D(dataPtr, *size, (i+1)*matrixSize);

		for(size_t j = 0; j < *height; ++j) {
			for(size_t k = 0; k < *width; ++k)
				if(fscanf(in, "%lf", &(*dataPtr)[i*matrixSize + j* *width + k]) <= 0)
					FAIL("Error while reading matrix elements\n");
/*		for(size_t j = 0; j < matrixSize; ++j) {
			if(fscanf(in, "%lf", &(*dataPtr)[i*matrixSize + j]) <= 0)
				FAIL("Error while reading matrix elements\n");*/
		}

		// check if there is anything remaining in this line
		if(!checkEol(in))
			return 0;
	}

	fclose(in);
	return maxI+1;
}

size_t readMatrix1DT(double** dataPtr, size_t* height, size_t* width, size_t* size, size_t* offset, const char* pathname, const char* filename) {
	FILE* in = openFileR(pathname, filename);

	if(*dataPtr == NULL)
		*dataPtr = (double*)malloc(*size * sizeof(double));

	size_t i = 0, maxI = 0;

	FILE* check = openFileR(pathname, filename);
	fscanf(check, "%lu", offset);
	fclose(check);

	while(fscanf(in, "%lu", &i) > 0) {
		// subtract the starting offset
		i -= *offset;
		maxI = max(i, maxI);
		if(fscanf(in, "%lu", height) <= 0)
			FAIL("Cannot read height of matrix\n");
		if(fscanf(in, "%lu", width) <= 0)
			FAIL("Cannot read width of matrix\n");
		// check if there is anything remaining in this line
		if(!checkEol(in))
			return 0;

		// assuming all matrices have the same size, otherwise final values for height and width would be wrong for some matrices
		size_t matrixSize = *height * *width;
		if((i+1)*matrixSize >= *size)
		*size = resize1D(dataPtr, *size, (i+1)*matrixSize);

		for(size_t j = 0; j < *height; ++j) {
			for(size_t k = 0; k < *width; ++k)
				if(fscanf(in, "%lf", &(*dataPtr)[i*matrixSize + k* *height + j]) <= 0)
					FAIL("Error while reading matrix elements\n");
/*		for(size_t j = 0; j < matrixSize; ++j) {
			if(fscanf(in, "%lf", &(*dataPtr)[i*matrixSize + j]) <= 0)
				FAIL("Error while reading matrix elements\n");*/
		}

		// check if there is anything remaining in this line
		if(!checkEol(in))
			return 0;
	}

	fclose(in);
	return maxI+1;
}


size_t readBase(double** dataPtr/*, char** name*/, size_t* num, size_t* height, size_t* width, size_t* size, const char* pathname, const char* filename) {
	FILE* in = openFileR(pathname, filename);
	char* name  = NULL;
	size_t tmp;

	if(*dataPtr == NULL)
		*dataPtr = (double*)malloc(*size * sizeof(double));

	double* data = *dataPtr;

	char buf[128] = {'\0'};
	size_t i = 0, maxI = 0;

	while(fscanf(in, "%s", buf) > 0) {
		if(i == 0) {
			name = (char*)malloc((strlen(buf)+1) * sizeof(char));
			strcpy(name, buf);
		}
		else
			if(strcmp(name, buf) != 0) {
				fprintf(stderr, "Old name: %s\nNew name: %s\n", name, buf);
				FAIL("YOU need to extent the base loader because there are several different names in it\n");
			}

		if(fscanf(in, "%lu", num) <= 0)
			FAIL("Cannot read number of matrices\n");

		maxI = max(i, maxI);
		if(fscanf(in, "%lu", height) <= 0)
			FAIL("Cannot read height of matrix\n");
		if(fscanf(in, "%lu", width) <= 0)
			FAIL("Cannot read width of matrix\n");
		// check if there is anything remaining in this line
		if(!checkEol(in))
			return 0;

		// assuming all matrices have the same size, otherwise final values for height and width would be wrong for some matrices
		size_t matrixSize = *height * *width;
		if((i+1)*matrixSize* *num >= *size) {
			*size = resize1D(dataPtr, *size, (i+1)*matrixSize*(*num));
			data = *dataPtr;
		}
		// read #num matrices of size height * width
		for(size_t n = 0; n < *num; ++n) {
			// read index and position
			if(fscanf(in, "%lu %lu\n", &tmp, &tmp) <= 0)
				FAIL("Error while reading matrix index and position\n");
//			for(size_t j = 0; j < *height; ++j) {
//				for(size_t k = 0; k < *width; ++k) transposing
//					if(fscanf(in, "%lf", &data[i*(*num)*matrixSize + n*matrixSize + k* *height + j]) <= 0)
//						FAIL("Error while reading matrix elements\n");
			for(size_t j = 0; j < matrixSize; ++j) {
				if(fscanf(in, "%lf", &data[i*(*num)*matrixSize + n*matrixSize + j]) <= 0)
					FAIL("Error while reading matrix elements\n");
			}
		}

		// check if there is anything remaining in this line
		if(!checkEol(in))
			return 0;

		++i;
	}

	free(name);
	fclose(in);
	return maxI+1;
}
