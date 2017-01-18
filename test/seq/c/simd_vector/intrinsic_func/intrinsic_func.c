#include <stdio.h>
#include <xmmintrin.h>

void printf4vector(__v4sf* p) {

	for(int i=0;i<4;i++) {
		printf("%f ", *(p++));
	}
	printf("\n");
}
int main() {

	__v4sf x;
	 _mm_loadh_pi(x, 0);
	const __v2sf* cx = 0;
    __builtin_ia32_loadhps(x, cx);

	{
		__v4sf a = {1,2,3,4};
		__v4sf b = {5,6,7,8};
		__v4sf c = {9,10,11,12};

		printf4vector(&a);
		printf4vector(&b);
		printf4vector(&c);

		//builtin "ia32_mulps" not supported by clang -- use _mm_mul_ps
		__v4sf tmp = _mm_mul_ps(a, b);   // a * b 
		printf4vector(&tmp);

		//builtin "ia32_addps" not supported by clang -- use _mm_add_ps
		__v4sf e =   _mm_add_ps(tmp, c);    // e = (a * b) + c
		printf4vector(&e);
	}
	return 0;
}
