#include <libgen.h>
#include <stdio.h>

char path[256] = "/foo/bar/test/basename";

int main(int argc, char** argv) {

	printf("%s\n", basename(path));
	return 0;
}
