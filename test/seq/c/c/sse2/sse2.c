#include <emmintrin.h>

#define WEBP_INLINE inline

static WEBP_INLINE void GetBaseDelta(const __m128i* const p1,
                                     const __m128i* const p0,
                                     const __m128i* const q0,
                                     const __m128i* const q1,
                                     __m128i* const delta) {
  const __m128i p1_q1 = _mm_subs_epi8(*p1, *q1);   // p1 - q1
}

//// Applies filter on 2 pixels (p0 and q0)
//static WEBP_INLINE void DoFilter2(__m128i* const p1, __m128i* const p0,
//                                  __m128i* const q0, __m128i* const q1,
//                                  int thresh) {
//  __m128i a;
//  __m128i p1s, q1s;
//  GetBaseDelta(&p1s, p0, q0, &q1s, &a);
//}

int main() {
    //__m128i pi, qi, a0_lo, a0_hi, shuf01;
    __m128i p1, p0, q0, q1, delta;

    GetBaseDelta(&p1, &p0, &q0, &q1, &delta);

    //long long a, b, c;
    //long d;
    //unsigned long long e;

    //const __m128i shuf01_p = _mm_shufflehi_epi16(shuf01, _MM_SHUFFLE(2, 3, 0, 1));
    //const __m128i shuf01_p = ((__m128i)__builtin_ia32_pshufhw ((__v8hi)(shuf01), (int)(10)));
    
    //__m128i m;
    //__v8hi v = (__v8hi)(__m128i)m;

    //_mm_storel_epi64 (&qi, pi);

    //a = b * c;
    //a = d;
    //e = a;
    //a = e;

    return 0;
}


/*
#include <emmintrin.h>

#include <stdio.h>

#define WEBP_INLINE inline

//static WEBP_INLINE void GetBaseDelta(const __m128i* const p1,
//                                     const __m128i* const p0,
//                                     const __m128i* const q0,
//                                     const __m128i* const q1,
//                                     __m128i* const delta) {
//  const __m128i p1_q1 = _mm_subs_epi8(*p1, *q1);   // p1 - q1
//}
//
//// Applies filter on 2 pixels (p0 and q0)
//static WEBP_INLINE void DoFilter2(__m128i* const p1, __m128i* const p0,
//                                  __m128i* const q0, __m128i* const q1,
//                                  int thresh) {
//  __m128i a;
//  __m128i p1s, q1s;
//  GetBaseDelta(&p1s, p0, q0, &q1s, &a);
//}

int main() {
	__m128i pi, qi, a0_lo, a0_hi, shuf01;
	long long a = 2;
	long long b = 3;
	long long c = 4;
	long d = 5;
	unsigned long long e;

	const __m128i shuf01_p = _mm_shufflehi_epi16(shuf01, _MM_SHUFFLE(2, 3, 0, 1));
//	const __m128i shuf01_p = ((__m128i)__builtin_ia32_pshufhw ((__v8hi)(shuf01), (int)(10)));

	//__m128i m;
	//__v8hi v = (__v8hi)(__m128i)m;

	_mm_storel_epi64 (&qi, pi);
	//_mm_storel_epi64 (&a, b);
	a = b * c;
	a = d;
	e = a;
	a = e;

	// print value
	printf("a=%lld\n", a);
	
	return 0;
}
*/
