#include <stdio.h>



int main(int argc, char** argv) {

//	int j, cnt = 0;
//	int v[15] = {0};
//	int vec[15] = {0};
//  	int iterations = 0;
//	int x;
//	int i;
//
//	int a;
//	for(a=1;a<15;a+=3) {
//		printf("13: a=%d\n", a);
//	}
//
//	for(i = 8; i>=0; --i) {
//		x = i;
//		iterations++;
//		printf("x: %d\n", x);
//	}
//	printf("Iterations: %d\n", iterations);
//	printf("x: %d\n", x);
//	printf("i: %d\n", i);
//	{
//	for(unsigned char i = 8, j = 7; i>0; --i) {
//		printf("j: %d\n", j);
//		iterations++;
//	}
//
//	for(unsigned char i = 8, j = 7; i>-1; --i) {
//		printf("j: %d\n", j);
//		iterations++;
//	}
//
//	}
//	printf("Iterations: %d\n", iterations);
//
//	for(; i < 10; i+=1) {
//		iterations++;
//	}
//
//	printf("Iterations: %d\n", iterations);
//	
//	vec[10] = 15;
//	for(j=0; j<vec[10]; j++)
//	{
//		cnt ++;
//		if(j == 10)
//			vec[10] ++;
//	}
//	printf("Iterations: %d\n", cnt);
//
//	int* ptr1;
//	int* ptr2 = &vec[14];
//
//	for(ptr1=vec; ptr1!=ptr2; ptr1++) {
//		printf("%d ", *ptr1);
//		cnt++;
//	}
//
//	printf("Iterations: %d\n", cnt);
//
//	int q=0;
//	int w = 10;
//	for( q = 1; q+1 < w; ++q) {
//		cnt++;
//	}
//
//	printf("Iterations: %d\n", cnt);
//
//
//	return 0;
//
    int i, a = 1;
    int j = 0;

    for(i=0; i<10; i += a+i) {
            printf("%d\n", i);
            
            if(j++ > 10) break;
    }
 
    return 0;
}
