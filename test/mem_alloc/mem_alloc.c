
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char* argv[]) {
	
	int src_array[] = { 1, 2, 3, 4 };
	printf("%d\n", sizeof(src_array));
	
	int* dest_array = malloc(sizeof(src_array));
	memcpy(dest_array, src_array, sizeof(src_array));
	
	for(int idx=0; idx<sizeof(src_array)/sizeof(int); idx++) {
		printf("%d, ", dest_array[idx]);
	}
	printf("\n");
	
	free(dest_array);
}