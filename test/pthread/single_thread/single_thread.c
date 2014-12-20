#include <pthread.h>
#include <stdio.h>

void* threadBody(void* in) {
	int* x = (int*)in;
	(*x) = 5;

	return NULL;
}

int main(int argc, char** argv) {

	pthread_t thread;

	int x = 0;
	printf("x=%d\n", x);

	// start a thread
	pthread_create(&thread, NULL, threadBody, &x);

	// join the thread
	pthread_join(thread, NULL);

	printf("x=%d\n", x);

	return 0;
}
