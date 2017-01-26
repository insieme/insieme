#include <stdio.h>
#include <sys/time.h>

#ifndef _WIN32
	#include <sys/utsname.h>
#endif

int main() {
	struct timeval time;

	int res = gettimeofday(&time, NULL);
	printf("%d\n", res);

	#ifndef _WIN32
		struct utsname architecture;
		uname(&architecture);
		printf("%s\n", architecture.machine);
	#endif
}
