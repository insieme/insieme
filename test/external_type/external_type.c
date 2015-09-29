#include <stdio.h>
#include <sys/time.h>

int main() {
	struct timeval time;

	int res = gettimeofday(&time, NULL);
	printf("%d\n", res);
}
