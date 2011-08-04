#include <stdio.h>

int main ( int argc, char* argv[] ) {
	int a = 666;
	int* ptr1=&a;
	int** ptr2 =  &ptr1;
	int* ptr3 = *ptr2;
	ptr1 = &a;
	
	if(ptr1 && ptr1 != NULL) {
		printf("CHECK!\n");
	}
	
	if( !ptr1 || ptr1 == NULL ) {
		printf("DOES NOT CHECK\n");
	}
	
	if( *ptr1 == a || *ptr1 == 666 ) {
		printf("WORKS!\n");
	}
	
	if ( **(&ptr1) != a ) {
		printf("DOES NOT WORK!\n");
	}
	
	if ( ptr1 == *ptr2 && ptr3 == ptr1 ) {
		printf("AWESOME\n");
	}
	
	*ptr1=0;
	
	if ( *ptr3 == 0 && **ptr2 == 0 ) {
		printf("SUPER AWESOME\n");
	}

	// test null assignment
	char* p = NULL;

	return 0;
}
