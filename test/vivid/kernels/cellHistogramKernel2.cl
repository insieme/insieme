__constant int MAX_HISTOGRAM_SIZE = 500;
__constant int BLOCK_SIZE = 16;
__constant int histogram_cache_sz =2000; // 4 * MAX_HISTOGRAM_SIZE

__kernel void  cellHistogramKernel2(
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
	
    const int cb_ind_y = get_local_id(0) / 8;
    const int cb_ind_x = get_local_id(1)  / 8;

    const int tc_ind_y = get_local_id(0) % 8;
    const int tc_ind_x = get_local_id(1)  % 8;

    const int target_y = get_group_id(0) * 2 + cb_ind_y;
    const int target_x = get_group_id(1) * 2 + cb_ind_x;

    const int source_y = start_y + BLOCK_SIZE * get_group_id(0) + get_local_id(0);
    const int source_x = start_x + BLOCK_SIZE * get_group_id(1) + get_local_id(1);

    const float aval = assignments[source_y*assignments_pitch_f + source_x];

    const float wval =weights[source_y*weights_pitch_f + source_x];


    const int cells_per_block_dim = 2;
    
    

    __local float histogram_cache[2000];

    const int cache_offset = MAX_HISTOGRAM_SIZE * 
        (cb_ind_y * cells_per_block_dim + cb_ind_x);

    //initialize the histogram
    int thread_bin_offset = tc_ind_y * cell_size + tc_ind_x;
    while (thread_bin_offset < max_bin)
    {
        const int cache_addr = cache_offset + thread_bin_offset;
        histogram_cache[cache_addr] = 0;
        thread_bin_offset += (cell_size * cell_size);
    }

   barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);

    //if (wval > 0.01f){
    //    atomicAdd(histogram_cache + cache_offset + (int) aval, wval);
			histogram_cache [cache_offset]= histogram_cache [cache_offset]+wval;

    //}

   barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);

    //if ((target_y < histogram.dim_t) && (target_x < histogram.dim_x))
    {
        thread_bin_offset = tc_ind_y * cell_size + tc_ind_x;
        while (thread_bin_offset < max_bin)
        {
            const int cache_addr = cache_offset + thread_bin_offset;
          
			histogram[target_y*histogram_pitch_tf+
				target_x*histogram_pitch_yf+
				thread_bin_offset ] = 
            histogram_cache[cache_addr];
            thread_bin_offset += (cell_size * cell_size);
        }
    }
   barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);
   
}


