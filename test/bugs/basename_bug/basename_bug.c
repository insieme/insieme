#include <libgen.h>
#include <stdio.h>

char g_my_string[256];


int main(int argc, char** argv) {


	strcpy(g_my_string, basename(argv[0]));
	printf("%s\n", g_my_string);

}
