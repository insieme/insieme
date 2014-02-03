/*static const unsigned int BLOCK_SIZE = 16;*/

#define BLOCK_SIZE 16
/*
 * @note This kernel is based on the blocked matrix multiply.  We
 * expect to be caleld with blockDim(BLOCK_SIZE, BLOCK_SIZE) and a
 * sufficiently large grid to cover all othe output values.
 */
__kernel void pairwiseDistanceKernel(
        __global float8 * a, const int a_width, const int a_pitch_f,
        __global float8 * b, const int b_pitch_f,
        __global float* out, const int o_pitch_f)
{

    const int out_ry = get_group_id(0) * BLOCK_SIZE + get_local_id(0);
    const int out_cx = get_group_id(1) * BLOCK_SIZE + get_local_id(1);

    const int b_ry = get_group_id(1) * BLOCK_SIZE + get_local_id(0);

	const int a_mul = out_ry * a_pitch_f/8;

	const int b_mul = b_ry * b_pitch_f/8;

    __local float8 a_cache[16 * 16];
    __local float8 b_cache[16 * 16];

    float8 dst = 0;

	const int myCoef1 = get_local_id(1) * BLOCK_SIZE;
	const int myCoef0 = get_local_id(0) * BLOCK_SIZE;
	
	const int myInd = get_local_id(0) * BLOCK_SIZE + get_local_id(1);

    for (int i=0; i < a_width/8; i+=BLOCK_SIZE)
    {
        int read_cx = i + get_local_id(1);

                a_cache[myInd] =
                    a[a_mul + read_cx];

                b_cache[myInd] =
                    b[b_mul + read_cx];

        barrier(CLK_LOCAL_MEM_FENCE);
        
           
		float8 diff0 = a_cache[myCoef0 + 0]  - b_cache[myCoef1 + 0];
		dst= mad(diff0, diff0, dst);
        //dst += diff0 * diff0;

		float8 diff1 = a_cache[myCoef0 + 1]  - b_cache[myCoef1 + 1];
		dst= mad(diff1, diff1, dst);
        //dst += diff1 * diff1;

		float8 diff2 = a_cache[myCoef0 + 2]  - b_cache[myCoef1 + 2];
		dst=mad(diff2, diff2, dst);
        //dst += diff2 * diff2;

		float8 diff3 = a_cache[myCoef0 + 3]  - b_cache[myCoef1 + 3];
		dst=mad(diff3, diff3, dst);
        //dst += diff3 * diff3;

		float8 diff4 = a_cache[myCoef0 + 4]  - b_cache[myCoef1 + 4];
		dst=mad(diff4, diff4, dst);
        //dst += diff4 * diff4;

		float8 diff5 = a_cache[myCoef0 + 5]  - b_cache[myCoef1 + 5];
		dst=mad(diff5, diff5, dst);
        //dst += diff5 * diff5;

		float8 diff6 = a_cache[myCoef0 + 6]  - b_cache[myCoef1 + 6];
		dst=mad(diff6, diff6, dst);
        //dst += diff6 * diff6;

		float8 diff7 = a_cache[myCoef0 + 7]  - b_cache[myCoef1 + 7];
		dst=mad(diff7, diff7, dst);
        //dst += diff7 * diff7;

		float8 diff8 = a_cache[myCoef0 + 8]  - b_cache[myCoef1 + 8];
		dst=mad(diff8, diff8, dst);
        //dst += diff8 * diff8;

		float8 diff9 = a_cache[myCoef0 + 9]  - b_cache[myCoef1 + 9];
		dst=mad(diff9, diff9, dst);
        // dst += diff9 * diff9;

		float8 diff10 = a_cache[myCoef0 + 10]  - b_cache[myCoef1 + 10];
		dst=mad(diff10, diff10, dst);
        //dst += diff10 * diff10;

		float8 diff11 = a_cache[myCoef0 + 11]  - b_cache[myCoef1 + 11];
		dst=mad(diff11, diff11, dst);
        //dst += diff11 * diff11;

		float8 diff12 = a_cache[myCoef0 + 12]  - b_cache[myCoef1 + 12];
		dst=mad(diff12, diff12, dst);
        //dst += diff12 * diff12;

		float8 diff13 = a_cache[myCoef0 + 13]  - b_cache[myCoef1 + 13];
		dst=mad(diff13, diff13, dst);
        //dst += diff13 * diff13;

		float8 diff14 = a_cache[myCoef0 + 14]  - b_cache[myCoef1 + 14];
		dst=mad(diff14, diff14, dst);
        //dst += diff14 * diff14;
		
		float8 diff15 = a_cache[myCoef0 + 15]  - b_cache[myCoef1 + 15];
		dst=mad(diff15, diff15, dst);
        //dst += diff15 * diff15;

        
       barrier(CLK_LOCAL_MEM_FENCE);
    }

    //out[out_ry * o_pitch_f + out_cx] = dst.s0 +dst.s1 +dst.s2 +dst.s3 +dst.s4+dst.s5+dst.s6+dst.s7;
	//out[out_ry * o_pitch_f + out_cx] = dst;
	out[out_ry * o_pitch_f + out_cx] = dot(dst.lo,(float4)(1,1,1,1))+dot(dst.hi,(float4)(1,1,1,1));
	//out[out_ry * o_pitch_f + out_cx] = dot(dst,(float8)(1,1,1,1));
}
