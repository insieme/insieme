
#include <stdio.h>


int f (int x){

#pragma omp task firstprivate(x)
		{
			printf("%d\n", x);
		}

	return 0;
}


int main (){
	f(1);
	return 0;
}
