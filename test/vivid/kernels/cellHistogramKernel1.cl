

__constant int MAX_HISTOGRAM_SIZE = 500;
 __constant int BLOCK_SIZE = 16;
 
 __constant int histogram_cache_sz =2000; // 4 * MAX_HISTOGRAM_SIZE





__kernel void  cellHistogramKernel1(
    __global float* histogram, 
    const int histogram_x,
    const int histogram_y, 
	const int histogram_t,    
	const int histogram_pitch_y,
	const int histogram_pitch_t,
	__global float* assignments, 
	const int assignments_width, 
	const int assignments_height,
	const int assignments_pitch,
	__global float* weights,
	 const int weights_width, 
	const int weights_height,
	const int weights_pitch,
    const int max_bin,
    const int cell_size,
    const int start_y,
    const int start_x)
{

	const int histogram_pitch_yf = histogram_pitch_y / sizeof(float);
	const int histogram_pitch_tf = histogram_pitch_t / sizeof(float);
    const int assignments_pitch_f = assignments_pitch / sizeof(float);
	const int weights_pitch_f = weights_pitch / sizeof(float);
	
    const float aval = assignments[(start_y + get_local_size(0) * 
	get_group_id(0))*assignments_pitch_f +
	start_x + get_local_size(1) * get_group_id(1) + get_local_id(1)
	];
	
	

    const float wval =weights[(start_y + get_local_size(0) * 
	get_group_id(0))*weights_pitch_f +
	start_x + get_local_size(1) * get_group_id(1) + get_local_id(1)
	];


     __local float block_hist_cache[500];
    
    int bin_count = 0;
    int thread_bin = get_local_id(1) * get_local_size(1) + get_local_id(0);

   barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);
    while (bin_count < max_bin){
        if (thread_bin < max_bin){     
            block_hist_cache[thread_bin] = 0;
        }
        thread_bin += (get_local_size(1) * get_local_size(0));
        bin_count += (get_local_size(1) * get_local_size(0));
    }

   barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);
    if (wval > 0.01f){
   //     atomicAdd(block_hist_cache + int(aval), wval);
   int i = (int)aval;
		block_hist_cache[i] = block_hist_cache[i]+wval;
    }

    bin_count = 0;
    thread_bin = get_local_id(1) * get_local_size(1) + get_local_id(0);

   barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);
    while (bin_count < max_bin){
        if (thread_bin < max_bin){     
				histogram[ get_group_id(0)*histogram_pitch_tf+
				get_group_id(1)*histogram_pitch_yf+thread_bin]
				  = block_hist_cache[thread_bin];
        }

        thread_bin += (get_local_size(1) * get_local_size(0));
        bin_count += (get_local_size(1) * get_local_size(0));
           
    }

}
