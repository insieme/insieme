void transposeBankSSE(float* &filter_bank)
{
	// reorganize data in SIMD4 vectors
	// |0 1 2 .. 8| 0 1 2 .. 8 ..  =>> 0 0 0 ... 1 1 1 .. 
	int filter_size = 9;
	int num_filters = 100;
	float* tmpbank = (float*)_mm_malloc(num_filters * filter_size*sizeof(float), 256);

	for(int i=0; i<num_filters/4; i++)
	{
		for(int j=0; j<9; j++) {
			for(int k=0; k<4; k++)
				tmpbank[i*4*9 + j*4+ k] = filter_bank[i*4*9 + j+ k*9];
		}
	}
	_mm_free(filter_bank);
	filter_bank = tmpbank;
}


void cosine_filter_sse(
	float* fr_data, float* fb_array, 
	const int height, const int width, 
	const int filter_h, const int filter_w, 
	const int n_filters, float* out_data)
{
	transposeBankSSE(fb_array);
	//do convolution
	const int apron_y = filter_h / 2;
	const int apron_x = filter_w / 2;

	const int filter_size = filter_h * filter_w;

	const int filter_bank_size = filter_size * n_filters;

	int *pixel_offsets=(int*) malloc(sizeof(int)*filter_size);

	int oi = 0;
	for (int ii=-apron_y; ii<=apron_y; ii++){
		for (int jj=-apron_y; jj<=apron_y; jj++){
			pixel_offsets[oi] = ii * width + jj;
			oi++;
		}
	}
	// 100 filters, each 9 values
	int imask = 0x7fffffff;
	float fmask = *((float*)&imask);


	int n_threads = omp_get_num_procs();
	omp_set_num_threads(n_threads);

	double tic = omp_get_wtime();
	for(int i=0; i<1000; i++) 
	{

		int valid_height = height - 2 * apron_y;
		int height_step = valid_height / n_threads + 1;

#pragma omp parallel for
		for (int tid=0; tid<n_threads; tid++){
			int start_y = apron_y + tid * height_step;
			int end_y = min(start_y + height_step, height - apron_y);
			//		for(int i=0; i<100; i++)
			for (int i=start_y; i<end_y; i++){
				float* fr_ptr = fr_data + i * width + apron_x;
				float* ass_out = out_data + i * width + apron_x;
				float* wgt_out = ass_out + height * width;

				for (int j=apron_x; j<(width - apron_x); j++ ){


					__m128 image_cache0 =  _mm_load1_ps(&fr_ptr[pixel_offsets[0]]);
					__m128 image_cache1 =  _mm_load1_ps(&fr_ptr[pixel_offsets[1]]);
					__m128 image_cache2 =  _mm_load1_ps(&fr_ptr[pixel_offsets[2]]);
					__m128 image_cache3 =  _mm_load1_ps(&fr_ptr[pixel_offsets[3]]);
					__m128 image_cache4 =  _mm_load1_ps(&fr_ptr[pixel_offsets[4]]);
					__m128 image_cache5 =  _mm_load1_ps(&fr_ptr[pixel_offsets[5]]);
					__m128 image_cache6 =  _mm_load1_ps(&fr_ptr[pixel_offsets[6]]);
					__m128 image_cache7 =  _mm_load1_ps(&fr_ptr[pixel_offsets[7]]);
					__m128 image_cache8 =  _mm_load1_ps(&fr_ptr[pixel_offsets[8]]);

					float max_sim[4] = {-1e6, -1e6, -1e6, -1e6};
					int best_ind = -1;

					int fi=0;
					int filter_ind = 0;

					// 100 filters, 9 values each, no need for leftover handling
					while (fi<filter_bank_size)
					{
						__m128 temp_sum = _mm_set1_ps(0.0f);

						// no fused multiply add :(
						// current value of 4 filters
						__m128 curr_filter = _mm_load_ps(&fb_array[fi]);
						fi+=4;
						temp_sum = _mm_add_ps(_mm_mul_ps(image_cache0, curr_filter), temp_sum);

						curr_filter = _mm_load_ps(&fb_array[fi]);
						fi+=4;
						temp_sum = _mm_add_ps(_mm_mul_ps(image_cache1, curr_filter), temp_sum);

						curr_filter = _mm_load_ps(&fb_array[fi]);
						fi+=4;
						temp_sum = _mm_add_ps(_mm_mul_ps(image_cache2, curr_filter), temp_sum);

						curr_filter = _mm_load_ps(&fb_array[fi]);
						fi+=4;
						temp_sum = _mm_add_ps(_mm_mul_ps(image_cache3, curr_filter), temp_sum);

						curr_filter = _mm_load_ps(&fb_array[fi]);
						fi+=4;
						temp_sum = _mm_add_ps(_mm_mul_ps(image_cache4, curr_filter), temp_sum);

						curr_filter = _mm_load_ps(&fb_array[fi]);
						fi+=4;
						temp_sum = _mm_add_ps(_mm_mul_ps(image_cache5, curr_filter), temp_sum);

						curr_filter = _mm_load_ps(&fb_array[fi]);
						fi+=4;
						temp_sum = _mm_add_ps(_mm_mul_ps(image_cache6, curr_filter), temp_sum);

						curr_filter = _mm_load_ps(&fb_array[fi]);
						fi+=4;
						temp_sum = _mm_add_ps(_mm_mul_ps(image_cache7, curr_filter), temp_sum);

						curr_filter = _mm_load_ps(&fb_array[fi]);
						fi+=4;
						temp_sum = _mm_add_ps(_mm_mul_ps(image_cache8, curr_filter), temp_sum);


						// calculating absolute value by clearing the last digit
						__m128 mask = _mm_set1_ps(fmask);

						temp_sum = _mm_and_ps(mask, temp_sum);


						__m128 max_fil = _mm_load_ss(max_sim);


						// first half 
						// if filter value greater than max
						if(_mm_comigt_ss(temp_sum, max_fil)) {
							// _mm_move_ss(max_fil, sum1);
							//	max_fil.m128_f32[0] = sum1.m128_f32[0];
							max_fil = temp_sum;
							best_ind = filter_ind+0;
						}

						// permute to second element of vector
						// if filter value greater than max
					//	__m128 ptmp = _mm_permute_ps(temp_sum, 0x1);
						__m128 ptmp = temp_sum;
						if(_mm_comigt_ss(ptmp, max_fil)) {
							//	_mm_move_ss(max_fil, sum1);
							max_fil = ptmp;
							best_ind = filter_ind+1;
						}

						// permute to second element of vector
						// if filter value greater than max
						ptmp = temp_sum;
					//	ptmp = _mm_permute_ps(temp_sum, 0x2);
						if(_mm_comigt_ss(ptmp, max_fil)) {
							//	_mm_move_ss(max_fil, sum1);
							max_fil = ptmp;
							best_ind = filter_ind+2;
						}

						// permute to second element of vector
						// if filter value greater than max
					//	ptmp = _mm_permute_ps(temp_sum, 0x3);
						ptmp = temp_sum;
						if(_mm_comigt_ss(ptmp,max_fil)) {
							//	_mm_move_ss(max_fil, sum1);
							max_fil = ptmp;
							best_ind = filter_ind+3;
						} 

						_mm_store_ss(max_sim, max_fil);
						// printf("max1 :%f\n", max_fil.m128_f32[0]);


						filter_ind += 4;
					}

					*ass_out = (float)best_ind;
					*wgt_out = max_sim[0];

					fr_ptr++;
					ass_out++;
					wgt_out++;
				}
			}
		}
		//#pragma omp barrier
	}
	double toc = omp_get_wtime();
	std::cout << "openmp filter sse time: " << toc - tic << std::endl;
	_mm_free(fb_array);
}
