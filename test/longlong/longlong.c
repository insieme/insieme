#include <stdio.h>
#include <emmintrin.h>

#define WEBP_INLINE inline

static WEBP_INLINE void GetBaseDelta(const __m128i* const p1,
    const __m128i* const p0,
    const __m128i* const q0,
    const __m128i* const q1,
    __m128i* const delta) {
    const __m128i p1_q1 = _mm_subs_epi8(*p1, *q1);   // p1 - q1
}

int main() {
    __m128i p1, p0, q0, q1, delta;
    GetBaseDelta(&p1, &p0, &q0, &q1, &delta);

    printf("%llu \n", 1ULL << 40);

    return 0;
}
