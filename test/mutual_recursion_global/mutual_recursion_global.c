#include <stdio.h>


#define NT 8
float arr[NT][NT];

void sumA(int row, int start);

void sumB(int row, int start) {
	sumA(row, start+1);
}

void sumA(int row, int start) {
	if(start == NT-1) return;
	if(start == 0) arr[row][0] = arr[row-1][NT-1];
	else arr[row][start] += arr[row][start-1];
	sumB(row, start);
}

int main() {
	sumA(0,1);
	for(int i=1; i<NT; ++i) {
		sumA(i,0);
	}
	printf("%f\n", arr[NT-1][NT-1]);
	return 0;
}
