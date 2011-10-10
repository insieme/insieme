
#include <stdio.h>

void printFile(char* name) {
	FILE *fp;
	printf("Opening file %s ...\n", name);
	fp=fopen(name, "r");
	if (fp == 0) {
		printf("File not found!\n");
		return;
	}

	printf("Reading file ...\n");
	int cur = fgetc(fp);
	while(cur!=EOF) {
		printf("%c", cur);
		cur = fgetc(fp);
	}
	printf("Closing file ...\n");
	fflush(fp);
	fclose(fp);
}

int main() {

	printFile("file.txt");
	printFile("file2.txt");
	printf("Done.\n");

	//fflush(stderr);
}
