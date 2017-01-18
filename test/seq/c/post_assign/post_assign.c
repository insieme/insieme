#include <stdio.h>

/***********************************************************************
 *  * : 
 *   **********************************************************************/
void del(int k, int *print_ptr, int *last_print, int *displ)
{
   if (*last_print<0) *last_print = displ[(*print_ptr)-1] -=  k;
   else               *last_print = displ[(*print_ptr)++]  = -k;
}

int main (){

	int buff[1000];
	int print = 50, last = 60;
	for(int i=0; i < 1000; ++i)
		buff[i] = i;
	
	
	del(300, &print, &last, buff);
	del(310, &print, &last, buff);
	del(350, &print, &last, buff);

	for(int i=0; i < 1000; ++i)
		printf("%d,", buff[i]);
	printf("\n%d,%d\n,", print, last);


	return 0;
}
