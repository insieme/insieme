__constant int MAX_FILTERBANK_SIZE  = 10000;
__constant int N_MAX_CHANNELS = 10;

__kernel void blockwise_distance_kernel(
    __global float * frame, const int f_width, const int f_height, const int f_pitch,
    __global float * output, const int output_x, const int output_y,
    const int output_t, const int o_pitch_y,const int o_pitch_t,
    const int frame_width, const int frame_height,
    const int FILTER_DIM, const int  BLOCK_MULT,const int BLOCK_SIZE,const int optype,
    __global float8 * c_FilterBank,
    const int n_filters)
{
	const int f_pitch_f = f_pitch / sizeof(float);
	const int o_pitch_yf = o_pitch_y / sizeof(float);
	const int o_pitch_tf = o_pitch_t / sizeof(float);
	
	const int out_pix_y0 = get_group_id(0) * (BLOCK_SIZE * BLOCK_MULT) + FILTER_DIM / 2;
	
	const int out_pix_x0 = get_group_id(1) * (BLOCK_SIZE * BLOCK_MULT) + FILTER_DIM / 2;

    const int out_pix_y_stop = min(out_pix_y0 + (BLOCK_SIZE * BLOCK_MULT),
                               frame_height - FILTER_DIM / 2);
    
	const int out_pix_x_stop = min(out_pix_x0 + (BLOCK_SIZE * BLOCK_MULT),
                               frame_width - FILTER_DIM / 2);
							   
	const int cache_size = 38;

	__local float4 filter_leftovers [9]; 
	filter_leftovers[0] = c_FilterBank[(n_filters/8)*9].lo;
	filter_leftovers[1] = c_FilterBank[(n_filters/8)*9].hi;

	filter_leftovers[2] = c_FilterBank[(n_filters/8)*9+1].lo;
	filter_leftovers[3] = c_FilterBank[(n_filters/8)*9+1].hi;

	filter_leftovers[4] = c_FilterBank[(n_filters/8)*9+2].lo;
	filter_leftovers[5] = c_FilterBank[(n_filters/8)*9+2].hi;

	filter_leftovers[6] = c_FilterBank[(n_filters/8)*9+3].lo;
	filter_leftovers[7] = c_FilterBank[(n_filters/8)*9+3].hi;
	filter_leftovers[8] = c_FilterBank[(n_filters/8)*9+4].lo;
	

    __local float image_cache[38*38];
	

    int read_pix_x, read_pix_y;
    int cache_ind_x, cache_ind_y;

	const int max_im_ind = f_height * f_pitch_f;

    read_pix_y = out_pix_y0 - FILTER_DIM / 2 + get_local_id(0);
    cache_ind_y = get_local_id(0);
    for (int ii = 0;  ii < BLOCK_MULT + 1; ii++){
        read_pix_x = out_pix_x0 - FILTER_DIM / 2 + get_local_id(1);
        cache_ind_x = get_local_id(1);
        for (int jj = 0; jj < BLOCK_MULT + 1; jj++){
            if ((cache_ind_x < cache_size) && (cache_ind_y < cache_size)){
				const int read_ind = read_pix_y * f_pitch_f + read_pix_x;

				if ((read_ind > 0) && (read_ind < max_im_ind))
				{
					image_cache[cache_ind_y*cache_size+cache_ind_x] = frame[read_ind];
				}
				else 
				{
					image_cache[cache_ind_y*cache_size+cache_ind_x] = 0;
				}
            }
            read_pix_x += BLOCK_SIZE;
            cache_ind_x += BLOCK_SIZE;
        }
        read_pix_y += BLOCK_SIZE;
        cache_ind_y += BLOCK_SIZE;
    }
	
    barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);

  int out_y = out_pix_y0 + get_local_id(0);
    for (int ii=0; ii<BLOCK_MULT; ii++){
        int out_x = out_pix_x0 + get_local_id(1);       
        for (int jj=0; jj<BLOCK_MULT; jj++){
            float curval = -1e6;
            float curid = -1;
			int cyi = get_local_id(0)+ ii * BLOCK_SIZE;		
            int cxi = get_local_id(1) + jj * BLOCK_SIZE;
            int fi = 0;
			float8 img0 = (float8 )(image_cache[cyi*cache_size+cxi]);
			
			float8 img1 = (float8 )(image_cache[cyi*cache_size+cxi+1]);

			float8 img2 = (float8 )(image_cache[cyi*cache_size+cxi+2]);

			float8 img3 = (float8 )(image_cache[(cyi+1)*cache_size+cxi]);

			float8 img4 = (float8 )(image_cache[(cyi+1)*cache_size+cxi+1]);

			float8 img5 = (float8 )(image_cache[(cyi+1)*cache_size+cxi+2]);

			float8 img6 = (float8 )(image_cache[(cyi+2)*cache_size+cxi]);

			float8 img7 = (float8 )(image_cache[(cyi+2)*cache_size+cxi+1]);

			float8 img8 = (float8 )(image_cache[(cyi+2)*cache_size+cxi+2]);


            if ((out_y < out_pix_y_stop) && (out_x < out_pix_x_stop)){ 
                for (int filter_id=0; filter_id<n_filters/8; filter_id++){
				 float8 tempval=(float8 )(0.0f);
                    

						tempval += c_FilterBank[fi++] * img0;
						tempval += c_FilterBank[fi++] * img1;
						tempval += c_FilterBank[fi++] * img2;
                        
                        
						tempval += c_FilterBank[fi++] * img3;
						tempval += c_FilterBank[fi++] * img4;
						tempval += c_FilterBank[fi++] * img5;
                        
                        
						tempval += c_FilterBank[fi++] * img6;
						tempval += c_FilterBank[fi++] * img7;
						tempval += c_FilterBank[fi++] * img8;
                         
                 // int8 sel = fabs(tempval) > curval;
				//	curval = select(fabs(tempval), curval, sel);
				//	curid = select(fabs(tempval), curid, sel);

				tempval = fabs(tempval);

				if ((tempval.s0) > curval){
                        curid = filter_id*8;
                        curval = (tempval.s0);
                    }
					if ((tempval.s1) > curval){
                        curid = filter_id*8+1;
                        curval = (tempval.s1);
                    }
					if ((tempval.s2) > curval){
                        curid = filter_id*8+2;
                        curval = (tempval.s2);
                    }
					if ((tempval.s3) > curval){
                        curid = filter_id*8+3;
                        curval = (tempval.s3);
                    }
					if ((tempval.s4) > curval){
                        curid = filter_id*8+4;
                        curval = (tempval.s4);
                    }
					if ((tempval.s5) > curval){
                        curid = filter_id*8+5;
                        curval = (tempval.s5);
                    }
					if ((tempval.s6) > curval){
                        curid = filter_id*8+6;
                        curval = (tempval.s6);
                    }
					if ((tempval.s7) > curval){
                        curid = filter_id*8+7;
                        curval = (tempval.s7);
                    }
					
                }
				float4 tempval = (float4 )(0.0f);
				

						tempval += filter_leftovers[0] * img0.lo;
						tempval += filter_leftovers[1] * img1.lo;
						tempval += filter_leftovers[2] * img2.lo;
                        
                        
						tempval += filter_leftovers[3] * img3.lo;
						tempval += filter_leftovers[4] * img4.lo;
						tempval += filter_leftovers[5] * img5.lo;
                        
                        
						tempval += filter_leftovers[6] * img6.lo;
						tempval += filter_leftovers[7] * img7.lo;
						tempval += filter_leftovers[8] * img8.lo;

					if (fabs(tempval.s0) > curval){
                        curid = (n_filters/8)*8;
                        curval = fabs(tempval.s0);
                    }
					if (fabs(tempval.s1) > curval){
                        curid = (n_filters/8)*8+1;
                        curval = fabs(tempval.s1);
                    }
					if (fabs(tempval.s2) > curval){
                        curid = (n_filters/8)*8+2;
                        curval = fabs(tempval.s2);
                    }
					if (fabs(tempval.s3) > curval){
                        curid = (n_filters/8)*8+3;
                        curval = fabs(tempval.s3);
                    }
					
                const int out_pix_offset = out_y * o_pitch_yf + out_x;

                *(output + out_pix_offset) = curid;
                *(output + o_pitch_tf + out_pix_offset) = curval;
            }
			 
            out_x += BLOCK_SIZE;
        }
        out_y += BLOCK_SIZE;
    }
    barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);
}
