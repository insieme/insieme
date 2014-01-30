
	__constant int MAX_FILTERBANK_SIZE  = 10000;
	__constant int N_MAX_CHANNELS = 10;

	
	__constant int HIST_CACHE_SIZE  = 10;
	__constant int FF_OPTYPE_EUCLIDEAN = 0;
	__constant int FF_OPTYPE_COSINE = 1;


__constant unsigned int  MAX_HISTOGRAM_SIZE = 512;
__constant unsigned int  MAX_FOLDS = 512 / (8 * 8);


__kernel void cell_histogram_kernel( 
__global float* input, const int input_x, const int input_y, const int input_t,    const int i_pitch_y,const int i_pitch_t,
__global float* output, const int output_x, const int output_y,const int output_t,  const int o_pitch_y,const int o_pitch_t,
				const int cell_size, const int offset_y,
				const int offset_x, const int max_bin,
				const int BLOCK_8, const int BLOCK_SIZE)
{
	
	const int i_pitch_yf = i_pitch_y / sizeof(float);
	const int i_pitch_tf = i_pitch_t / sizeof(float);
	const int o_pitch_yf = o_pitch_y / sizeof(float);
	const int o_pitch_tf = o_pitch_t / sizeof(float);
	
	const int out_y = get_group_id(0) ;
    const int out_x = get_group_id(1);

	const int n_threads = BLOCK_8 * BLOCK_8;
    const int thread_id =  get_local_id(0) * BLOCK_8  + get_local_id(1) ;
	
	__local int   id_cache[8*8];
    __local float wt_cache[8*8];
	
	const int base_y = offset_y + get_group_id(0) * cell_size;
    const int base_x = offset_x + get_group_id(1) * cell_size;

    const int cell_area = cell_size * cell_size;
    const int n_folds = (max_bin - 1) / n_threads + 1;
	
	 float local_hist[8];
	 
	  //read the cell into the shared memory
    if ((get_local_id(0) < cell_size) && (get_local_id(1) < cell_size)){
        id_cache[get_local_id(0)*cell_size + get_local_id(1)] = 
          //  *getPtr(input, 0, base_y + get_local_id(0), base_x + get_local_id(1));
			input[0+(base_y + get_local_id(0))*i_pitch_yf+base_x + get_local_id(1)];
        wt_cache[get_local_id(0)*cell_size + get_local_id(1)] = 
           // *getPtr(input, 1, base_y + get_local_id(0), base_x + get_local_id(1));
			input[1+(base_y + get_local_id(0))*i_pitch_yf+base_x + get_local_id(1)];
    }

    for (int i=0;i<MAX_FOLDS;i++){
        local_hist[i] = 0;
    }

	barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);
	
	  //loop over the cell pixels and increment the histogram bin if the id matches
    for (int i=0; i<cell_area; i++){
        const int cur_id = id_cache[i];
        const float cur_wt = wt_cache[i];
        int hist_i = thread_id;
        for (int fi=0; fi<n_folds; fi++){
            if (cur_id == hist_i){
                local_hist[fi] += cur_wt; 
            }
            hist_i += n_threads;
        }
    }
	
	barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);

	 //write out the histogram
    if ( (out_y < output_t) && (out_x < output_y) ){
        for (int fi=0; fi<n_folds; fi++){
            int hist_i = n_threads * fi + thread_id;
            if (hist_i < max_bin){
              //  *getPtr(output, out_y, out_x, hist_i) = local_hist[fi];
				output[out_y*o_pitch_tf+out_x*o_pitch_yf+hist_i] = local_hist[fi];
		
            }
        }
    }

    barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);

	
	
 }

