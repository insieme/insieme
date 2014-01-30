__kernel  void argmaxKernel(
__global float* a, const int a_width, const int a_height, const int a_pitch,
__global float* out, const int o_width, const int o_height, const int o_pitch)
{

    const int a_pitch_f = a_pitch / sizeof(float);

    const int o_pitch_f = o_pitch / sizeof(float);
	
	const int ry = get_group_id(0) * get_local_size(0)  + get_local_id(0);

 

    if (ry < a_height) {
        int argmax = 0;
    	float maxval = a[ry * a_pitch_f + 0];
		

        for (int cx=1; cx < a_width; cx++) {
            float val = a[ry * a_pitch_f + cx];
            if (val > maxval) {
                argmax = cx;
                maxval = val;
            }
        }
		out[ry * o_pitch_f + 0] = argmax;
	 }
}

