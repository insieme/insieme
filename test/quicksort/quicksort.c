#include<stdio.h>
#include <stdlib.h>

void swapIt(int *a, int *b) {

	int t=*a; 
	*a=*b; 
	*b=t;
}

void sortIt(int arr[], int beg, int end) {

	if (end > beg + 1) {
    	int piv = arr[beg], l = beg + 1, r = end;

		while (l < r) {
			if (arr[l] <= piv) 
				l++;
			else 
				swapIt(&arr[l], &arr[--r]);
		}
		swapIt(&arr[--l], &arr[beg]);
		sortIt(arr, beg, l);
		sortIt(arr, r, end);	
	}
}


void printArray(int a[], int size) {

	int i =0;
	for (i =0; i<size; i++) {
		printf("%d ",a[i]);
	}
	printf("\n");
}

void reverseIt(int a[], int S, int E) {
	for(int i=0; i<(E-S)/2; ++i)
		swapIt(&a[S+i], &a[E-i-1]);
}

#define N 20000
int main(int argc, char* argv[]) {

	int* a = malloc(N*sizeof(int));

	for(int iter=0; iter<10; ++iter) {
		for(int i=0;i<N;i++) 
			a[i] = rand()%N;

		// printArray(a,N);
		sortIt(a,0,N);
		// printArray(a,N);
		reverseIt(a,0,N);
		// printArray(a,N);
		sortIt(a,0,N);
		printf("a[N/2] == %d\n", a[N/2]);
	}
	free(a);
	return 0;
}
