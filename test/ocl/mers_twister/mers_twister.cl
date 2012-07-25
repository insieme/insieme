#ifdef INSIEME
#include "ocl_device.h"
#endif

#define MT_RNG_COUNT 4096
#define MT_MM 9
#define MT_NN 19
#define MT_WMASK 0xFFFFFFFFU
#define MT_UMASK 0xFFFFFFFEU
#define MT_LMASK 0x1U
#define MT_SHIFT0 12
#define MT_SHIFTB 7
#define MT_SHIFTC 15
#define MT_SHIFT1 18
#define PI 3.14159265358979f

typedef unsigned int uint32_t;

#pragma insieme mark
__kernel void mersenne_twister(__global uint* buff_ma, __global uint* buff_b, __global uint* buff_c, __global uint* buff_seed, __global float4* result, int length) {
	int gid = get_global_id(0);
	
	if(gid >= length) return;
	
	int iState, iState1, iStateM;
	unsigned int mti, mti1, mtiM, x;
	unsigned int matrix_a, mask_b, mask_c;

	unsigned int mt[MT_NN]; // FIXME

	matrix_a = buff_ma[gid];
	mask_b   = buff_b[gid];
	mask_c   = buff_c[gid];
	
	mt[0] = buff_seed[gid];
	for (iState = 1; iState < MT_NN; iState++)
		mt[iState] = (1812433253U * (mt[iState - 1] ^ (mt[iState - 1] >> 30)) + iState) & MT_WMASK;

	iState = 0;
	mti1 = mt[0];

	float tmp[5];
	for (int i = 0; i < 4; ++i) {
		iState1 = iState + 1;
		iStateM = iState + MT_MM;
		if(iState1 >= MT_NN) iState1 -= MT_NN;
		if(iStateM >= MT_NN) iStateM -= MT_NN;
		mti  = mti1;
		mti1 = mt[iState1];
		mtiM = mt[iStateM];

		x = (mti & MT_UMASK) | (mti1 & MT_LMASK);
		x = mtiM ^ (x >> 1) ^ ((x & 1) ? matrix_a : 0);

		mt[iState] = x;
		iState = iState1;

		// Tempering transformation
		x ^= (x >> MT_SHIFT0);
		x ^= (x << MT_SHIFTB) & mask_b;
		x ^= (x << MT_SHIFTC) & mask_c;
		x ^= (x >> MT_SHIFT1);
		
		tmp[i] = ((float)x + 1.0f) / 4294967296.0f;
	}

	float4 val;
	val.s0 = tmp[0];
	val.s1 = tmp[1];
	val.s2 = tmp[2];
	val.s3 = tmp[3];

	result[gid] = val;
}

