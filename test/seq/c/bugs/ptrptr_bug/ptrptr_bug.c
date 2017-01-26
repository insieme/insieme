#include <stdlib.h>

void simple_alloc(int **array) {
	int* temp_array;
	*array = (int*)malloc(sizeof(int));
	// the following line is missing in the insieme-generated code
	temp_array = *array;
	temp_array = 0;
	temp_array = *array;
	temp_array[0] = 1;
}

void simple_free(int **array) {
	free(*array);
}

int main(int argc, char** argv) {
	int* array;
	simple_alloc(&array);
	simple_free(&array);
	return 0;
}
