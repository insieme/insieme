#include <stdio.h>
#include <omp.h>
#include <unistd.h>

#define IMAX 1000000000ll

int main() {
	long long i = 0;
	int arr[10][10][25];
	#pragma omp parallel
	{
		#pragma omp for
		for(int a=0; a<10; ++a) {
			for(int b=0; b<10; ++b) {
				for(int c=0; c<25; ++c) {
					arr[a][b][c] = a+b+c;
				}
			}
		}
	}
	
	int ok = 1;
	for(int a=0; a<10; ++a) {
		for(int b=0; b<10; ++b) {
			for(int c=0; c<25; ++c) {
				if(arr[a][b][c] != a+b+c) {
					ok = 0;
					break;
				}
			}
		}
	}
	
	if(ok) {
		printf("Success!\n");
	} else {
		printf("Fail!\n");
	}		
}
