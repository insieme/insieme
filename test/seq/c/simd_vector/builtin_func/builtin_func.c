#include <xmmintrin.h> // needed to get builtins of gcc through the clang parsing
typedef float __attribute__ ((__vector_size__(16))) __v4sf;

void printf4vector(__v4sf* p) {

	for(int i=0;i<4;i++) {
		printf("%f ", *(p++));
	}
	printf("\n");
}
int main() {
	{
		__v4sf a = {1,2,3,4};
		__v4sf b = {5,6,7,8};
		__v4sf c = {9,10,11,12};

		printf4vector(&a);
		printf4vector(&b);
		printf4vector(&c);

		//builtin "ia32_mulps" not supported by clang -- use _mm_mul_ps
		__v4sf tmp = __builtin_ia32_mulps (a, b);   // a * b 
		printf4vector(&tmp);

		//builtin "ia32_addps" not supported by clang -- use _mm_mul_ps
		__v4sf e =   __builtin_ia32_addps(tmp, c);    // e = (a * b) + c
		printf4vector(&e);
	}
	return 0;
}
