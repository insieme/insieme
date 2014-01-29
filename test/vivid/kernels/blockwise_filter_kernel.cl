
	__constant int MAX_FILTERBANK_SIZE  = 10000;
	__constant int N_MAX_CHANNELS = 10;

	
	__constant int _FLEXIBLE_FILTER_HPP_  = 1;
	__constant int FF_OPTYPE_EUCLIDEAN = 0;
	__constant int FF_OPTYPE_COSINE = 1;



__kernel void blockwise_filter_kernel( __global float* frame, const int f_width, const int f_height, const int f_pitch,
 __global float* output, const int output_x, const int output_y,const int output_t,  const int o_pitch_y,const int o_pitch_t,
                  const int frame_width, const int frame_height,
				  const int apron_lo_y, const int apron_lo_x,
                  const int apron_hi_y, const int apron_hi_x,const int nchannels,
                  const int BLOCK_SIZE,const int optype,__constant float * c_FilterBank)
{
	
	
	const int dim_x = output_x;
	const int dim_y = output_y;
	const int dim_t = output_t;
	
	const int f_pitch_f = f_pitch / sizeof(float);
	const int o_pitch_yf = o_pitch_y / sizeof(float);
	const int o_pitch_tf = o_pitch_t / sizeof(float);
	
	const int pix_y = get_group_id(0) * (BLOCK_SIZE-dim_y+1) + get_local_id(0);
    const int pix_x = get_group_id(1) * (BLOCK_SIZE-dim_x+1) + get_local_id(1);

 	__local float image_cache[16*16];
	
	
	barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);

	
	const int min_y = apron_hi_y;
    const int min_x = apron_hi_x;
    const int max_y = frame_height - apron_hi_y ;
    const int max_x = frame_width  - apron_hi_x ;
	
	


  for (int di=0; di<dim_t;di++){
        float tempval = 0;
        int fi_base = di * dim_y * dim_x * nchannels;
        for (int chan_id=0; chan_id<nchannels; chan_id++){
            int fi = chan_id + fi_base;

            image_cache[get_local_id(0) * BLOCK_SIZE + get_local_id(1)] = 0;
            //load the particular channel
            if ( (pix_y >= 0) && (pix_y < frame_height) && (pix_x >= 0) && (pix_x < frame_width) ){
                image_cache[get_local_id(0) * BLOCK_SIZE + get_local_id(1)] = 
				frame[pix_y*f_pitch_f+ pix_x * nchannels + chan_id];
			
            }

            barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);

            if (optype==FF_OPTYPE_EUCLIDEAN){
                for (int fyi=-apron_lo_y; fyi <= apron_hi_y; fyi++){
                    for (int fxi=-apron_lo_x; fxi <= apron_hi_x; fxi++){
                        float diff = image_cache[(get_local_id(0)+fyi)*BLOCK_SIZE + get_local_id(1)+fxi] - c_FilterBank[fi];
                        tempval += diff*diff;
                        fi+=nchannels;
                    }
                }

            }else if (optype==FF_OPTYPE_COSINE){
                for (int fyi=-apron_lo_y; fyi <= apron_hi_y; fyi++){
                    for (int fxi=-apron_lo_x; fxi <= apron_hi_x; fxi++){
                        tempval += image_cache[(get_local_id(0)+fyi)*BLOCK_SIZE + get_local_id(1)+fxi] * c_FilterBank[fi];
                        fi+=nchannels;
                    }
                }
            }
            barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);
        }

        if ( (pix_y >= min_y) && (pix_y < max_y) && (pix_x >= min_x) && (pix_x < max_x) ){
            if ( (get_local_id(0) >= apron_lo_y) && (get_local_id(0) < BLOCK_SIZE-apron_hi_y) && 
                 (get_local_id(1) >= apron_lo_x) && (get_local_id(1) < BLOCK_SIZE-apron_hi_x) ){
				output[pix_y*o_pitch_tf + pix_x*o_pitch_yf + di] = tempval;
            }
        }
        else if ( (pix_y >= 0) && (pix_y < frame_height) && (pix_x >= 0) && (pix_x < frame_width) ){
				output[pix_y*o_pitch_tf + pix_x*o_pitch_yf + di] = -1;
        }
    
    }

     barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);
 }

