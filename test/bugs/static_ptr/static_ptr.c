#include <stdlib.h>

int ** outChunks;
int outChunksCount;

int main() {
	outChunks = ( int ** ) calloc(1, sizeof( int * ) * outChunksCount );
	return 0;
}


