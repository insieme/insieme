#pragma once

#include <sys/stat.h>

size_t fileSize(const char *name) {
	struct stat fileStats;
	if(stat(name, &fileStats) != 0) {
		fprintf(stderr, "Could not 'stat' kernel file \"%s\" to determine its size.\n", name);
		exit(-1);
	}
	return fileStats.st_size;
}

char *readFile(const char *name) {
	FILE *file = fopen(name, "r");
	if(file == NULL) {
		fprintf(stderr, "Could not open kernel file \"%s\".\n", name);
		exit(-1);
	}
	size_t fsize = fileSize(name);
	char *ret = (char*)malloc(sizeof(char) * (fsize+1));
	size_t c = fread(ret, 1, fileSize(name), file);
	ret[c] = '\0';
	fclose(file);
	return ret;
}
