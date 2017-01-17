#include <stdio.h>
#include <signal.h>

void handler(int s) {
	printf("Caught %d\n", s);
}

int main(int argc, char** argv) {
	signal(SIGRTMIN+1, handler);
	signal(SIGRTMIN+2, handler);
	raise(SIGRTMIN+1);
	raise(SIGRTMIN+2);
}