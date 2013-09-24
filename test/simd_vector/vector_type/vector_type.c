#include <stdio.h>
typedef int __attribute__((vector_size(4*sizeof(int)))) vectorInt4;

void printVectorInt4(vectorInt4 v) {
	int *p = (int*)&v;
	for(int i=0;i<4;i++) {
		printf("%d ", *(p++));
	}
	printf("\n");
}

int main() {

	{
		vectorInt4 a, b, c;
		
		printVectorInt4(b);
		printVectorInt4(c);

		a = b + c;
		printVectorInt4(a);
		a = b * c;
		printVectorInt4(a);
		a = b & c;
		printVectorInt4(a);
		a = b | c;
		printVectorInt4(a);
		a = b ^ c;
		printVectorInt4(a);
		a = b >> c;
		printVectorInt4(a);
		a = b << c;
		printVectorInt4(a);
		printVectorInt4(a);
	}
	{
		vectorInt4 a = {1,1,1,1};
		vectorInt4 b = {0};
		vectorInt4 c = {1, 1};

		printVectorInt4(a);
		printVectorInt4(b);
		printVectorInt4(c);
	}
	{

		vectorInt4 a = {1};
		vectorInt4 b = {0};
		vectorInt4 c = {1};

		printVectorInt4(a);
		printVectorInt4(b);
		printVectorInt4(c);
		a = a + b + c;

		printVectorInt4(a);
		printVectorInt4(b);
		printVectorInt4(c);

	}
	{
		vectorInt4 a = {1,1,1,1};
		vectorInt4 b = {2,2,2,2};
		a = a*b;
		printVectorInt4(a);
		printVectorInt4(b);
	}
	{
		vectorInt4 a = {1,1,1,1};
		vectorInt4 b = {2,2,2,2};
		a = a/b;
		printVectorInt4(a);
		printVectorInt4(b);
	}
	{
		vectorInt4 a = {1,1,1,1};
		vectorInt4 b = {2,2,2,2};
		a = a%b;
		printVectorInt4(a);
		printVectorInt4(b);
	}
	return 0;
}
