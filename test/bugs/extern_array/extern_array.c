#include <stdio.h>
#include "extern_arr.h"

/*
 *	in this test we test if the resolution of external c arrays is done correctly
 *	as in IR they can be represented as array (if no size is known) or vector (if size is fixed) type
 *	as in the TU of extern_arr.c the size is known we get a vector type
 *	int th TU of extern_array.c the size is not known we get an array type
 *	after merging those translation units and taking care of init of the global variables we have
 *	the problem of using literals with array-type in the main and literals with vector-type for the
 *	global init 
 */
int main()
{
	extern_array[10];
	extern_array2[10] = 10;
	printf("%d\n", extern_array[10]);
	printf("%d\n", extern_array2[10]);
	
	return extern_arrayStruct[10].das;
	printf("%d\n", extern_arrayStruct[10].das);
	
	return extern_array[10];
}
