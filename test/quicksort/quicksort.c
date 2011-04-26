#include<stdio.h>

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


int main(int argc, char* argv[]) {

	int a[]={23,2,1,7,8,15,5,3,11,10};

	printArray(a,10);
	sortIt(a,0,10);
	printArray(a,10);

	return 0;
}
