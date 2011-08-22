#include <stdio.h>

FILE* openFileW(const char* pathname, const char* filename) {
	char path[512];
	sprintf(path, "%s%s", pathname, filename);

	FILE* file = fopen(path, "w");

	if(!file) {
		fprintf(stderr, "Cannot open file %s for writing\nAborting\n", pathname);
		return 0;
	}

	return file;
}

void saveScalars(const double* ptr, const size_t n,const  char* pathname, const char* filename) {
	FILE* out = openFileW(pathname, filename);

	for(size_t i = 0; i < n; ++i)
		fprintf(out, "%g\n", ptr[i]);

	fclose(out);
}

void saveUintScalars(const size_t* ptr, const size_t n, const char* pathname, const char* filename) {
	FILE* out = openFileW(pathname, filename);

	for(size_t i = 0; i < n; ++i)
		fprintf(out, "%lu\n", ptr[i]);

	fclose(out);
}

void saveVector(const double* ptr, const size_t nElem, const size_t len, const char* pathname, const char* filename){
	FILE* out = openFileW(pathname, filename);

	for(size_t n = 0; n < nElem; ++n) {
		fprintf(out, "%lu\t%lu\t", n+1, len);
		for(size_t i = 0; i < len; ++i)
			fprintf(out, "%g\t", ptr[n*len + i]);
		fprintf(out, "\n");
	}

	fclose(out);
}

// ptr points to the data
// nElem is the number of matrices
// height is the number of meaningful rows
// width is the number of meaningful columns
// matrixsize is the actual size of a matrix in memory
// pathname is the path where to save to
// filename is the name of the file to create
void saveMatrix(const double* ptr, const size_t nElem, const size_t height, const size_t width, const size_t matrixsize, const char* pathname, const char* filename) {
	FILE* out = openFileW(pathname, filename);

	for(size_t n = 0; n < nElem; ++n) {
		fprintf(out, "%lu\t%lu\t%lu\n", n+1, height, width);
		for(size_t i = 0; i < height; ++i) {
			for(size_t j = 0; j < width; ++j) {
				fprintf(out, "%g\t", ptr[n*matrixsize + i + j * height]);
			}
			fprintf(out, "\n");
		}
	}

	fclose(out);
}

// ptr points to the data
// nElem is the number of matrices
// height is the number of meaningful rows
// width is the number of meaningful columns
// matrixsize is the actual size of a matrix in memory
// pathname is the path where to save to
// filename is the name of the file to create
// transposes the matrix while writing, adds no offset to the index (designed for T3 only)
void saveMatrixT(const double* ptr, const size_t nElem, const size_t height, const size_t width, const size_t matrixsize, const char* pathname, const char* filename) {
	FILE* out = openFileW(pathname, filename);

	for(size_t n = 0; n < nElem; ++n) {
		fprintf(out, "%lu\t%lu\t%lu\n", n, width, height);
		for(size_t j = 0; j < width; ++j) {
			for(size_t i = 0; i < height; ++i) {
				fprintf(out, "%g\t", ptr[n*matrixsize + j * height + i]);
			}
			fprintf(out, "\n");
		}
	}

	fclose(out);
}
