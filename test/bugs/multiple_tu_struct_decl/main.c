#include <stdlib.h>

struct muha* init();

int main(int argc, char** argv) {
	struct muha* output = init();
	free(output);
	return 0;
}
