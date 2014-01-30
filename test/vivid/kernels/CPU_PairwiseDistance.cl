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

    const int out_ry = get_group_id(0) * BLOCK_SIZE + get_local_id(1);
    const int out_cx = get_group_id(1) * BLOCK_SIZE + get_local_id(0);

    const int b_ry = get_group_id(1) * BLOCK_SIZE + get_local_id(0);

	const int a_mul = out_ry * a_pitch_f/8;

	const int b_mul = b_ry * b_pitch_f/8;


    float8 dst = 0;


    for (int i=0; i < a_width/8; i+=BLOCK_SIZE)
    {
        int a_ind = a_mul+i;
		int b_ind = b_mul+i;

        
           
		float8 diff0 = a[a_ind + 0]  - b[b_ind + 0];
		//dst=mad(diff0, diff0, dst);
        dst += diff0 * diff0;

		float8 diff1 = a[a_ind + 1]  - b[b_ind + 1];
		//dst= mad(diff1, diff1, dst);
        dst += diff1 * diff1;

		float8 diff2 = a[a_ind + 2]  - b[b_ind + 2];
		//dst=mad(diff2, diff2, dst);
        dst += diff2 * diff2;

		float8 diff3 = a[a_ind + 3]  - b[b_ind + 3];
		//dst=mad(diff3, diff3, dst);
        dst += diff3 * diff3;

		float8 diff4 = a[a_ind + 4]  - b[b_ind + 4];
		//dst=mad(diff4, diff4, dst);
        dst += diff4 * diff4;

		float8 diff5 = a[a_ind + 5]  - b[b_ind + 5];
        dst += diff5 * diff5;

		float8 diff6 = a[a_ind + 6]  - b[b_ind + 6];
        dst += diff6 * diff6;

		float8 diff7 = a[a_ind + 7]  - b[b_ind + 7];
        dst += diff7 * diff7;

		float8 diff8 = a[a_ind + 8]  - b[b_ind + 8];
        dst += diff8 * diff8;

		float8 diff9 = a[a_ind + 9]  - b[b_ind + 9];
        dst += diff9 * diff9;

		float8 diff10 = a[a_ind + 10]  - b[b_ind + 10];
        dst += diff10 * diff10;

		float8 diff11 = a[a_ind + 11]  - b[b_ind + 11];
        dst += diff11 * diff11;

		float8 diff12 = a[a_ind + 12]  - b[b_ind + 12];
        dst += diff12 * diff12;

		float8 diff13 = a[a_ind + 13]  - b[b_ind + 13];
        dst += diff13 * diff13;

		float8 diff14 = a[a_ind + 14]  - b[b_ind + 14];
        dst += diff14 * diff14;
		
		float8 diff15 = a[a_ind + 15]  - b[b_ind + 15];
        dst += diff15 * diff15;

        
    }
	out[out_ry * o_pitch_f + out_cx] = dot(dst.lo,(float4)(1,1,1,1))+ dot(dst.hi,(float4)(1,1,1,1));
    //out[out_ry * o_pitch_f + out_cx] = dst.s0 +dst.s1 +dst.s2 +dst.s3 +dst.s4 +dst.s5 +dst.s6 +dst.s7;
}
