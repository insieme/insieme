#include "opencv2/opencv.hpp"
#include "omp.h"
#include <iostream>
#include <fstream>
#include <algorithm>
using namespace std;
#include <immintrin.h>
#include "xmmintrin.h"
#include <malloc.h>

void transposeBank(float* &filter_bank);

/*void print_vec(__m256 curr_filter) {
	printf("values: %f %f %f %f %f %f %f %f\n",
		curr_filter.m256_f32[7],curr_filter.m256_f32[6],curr_filter.m256_f32[5],curr_filter.m256_f32[4],
		curr_filter.m256_f32[3],curr_filter.m256_f32[2],curr_filter.m256_f32[1],curr_filter.m256_f32[0]);
}
void print_vec(__m128 curr_filter) {
	printf("values: %f %f %f %f\n",
		curr_filter.m128_f32[3],curr_filter.m128_f32[2],curr_filter.m128_f32[1],curr_filter.m128_f32[0]);
}*/


//#include "cosine_filter_mix.h"
//#include "cosine_filter_avx.h"
#include "cosine_filter_sse.h"



void cosine_filter_old(
	float* fr_data, float* fb_array, 
	const int height, const int width, 
	const int filter_h, const int filter_w, 
	const int n_filters, float* out_data);

void cosine_filter_transpose(
	float* fr_data, float* fb_array, 
	const int height, const int width, 
	const int filter_h, const int filter_w, 
	const int n_filters, float* out_data);


static char* exampleImagePath = "../../media\\kewell1.jpg";

int main(int argc, char* argv[])
{
	cv::Mat exampleImage = cv::imread(exampleImagePath, 0);

	//convert to float
	exampleImage.convertTo(exampleImage, CV_32FC1);

	//pull the data
	float* f_imData= (float*) exampleImage.data;

	const int height= exampleImage.size().height;
	const int width= exampleImage.size().width;

	//create a random filterbank
	const int num_filters = 100;
	const int filter_dim = 3;

	float* filter_bank = (float*)_mm_malloc(num_filters * filter_dim * filter_dim*sizeof(float), 256);
	// float* filter_bank = new float[num_filters * filter_dim * filter_dim];

	for (int i = 0; i < num_filters * filter_dim * filter_dim; i++)
	{
		filter_bank[i] = float( std::rand() ) / RAND_MAX;
	}

	//C Reference
	float* retvalC = new float[2 * height * width];


	//cosine_filter_mix(f_imData, filter_bank, height, width, filter_dim, filter_dim, num_filters, retvalC);
	//cosine_filter_old(f_imData, filter_bank, height, width, filter_dim, filter_dim, num_filters, retvalC);
	cosine_filter_transpose(f_imData, filter_bank, height, width, filter_dim, filter_dim, num_filters, retvalC);
	//cosine_filter_avx(f_imData, filter_bank, height, width, filter_dim, filter_dim, num_filters, retvalC);
	//cosine_filter_avx_nocomp(f_imData, filter_bank, height, width, filter_dim, filter_dim, num_filters, retvalC);
	//cosine_filter_sse(f_imData, filter_bank, height, width, filter_dim, filter_dim, num_filters, retvalC);

	std::ofstream test_out_c("testc.out", std::ios_base::out);
	for (int j = 0; j < height; j++)
	{
		for (int i = 0; i < width; i++)
		{
			test_out_c << retvalC[j * width + i] << ", ";
		}

		test_out_c << std::endl;
	}

	test_out_c << std::endl << std::endl << std::endl;

	for (int j = 0; j < height; j++)
	{
		for (int i = 0; i < width; i++)
		{
			test_out_c << retvalC[height * width + j * width + i] << ", ";
		}
		test_out_c << std::endl;
	}
	test_out_c.close();


	// delete[] filter_bank;
	// _mm_free(filter_bank);
	delete[] retvalC;

	return 0;
}



void cosine_filter_old(
	float* fr_data, float* fb_array, 
	const int height, const int width, 
	const int filter_h, const int filter_w, 
	const int n_filters, float* out_data)
{
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

	int n_threads = omp_get_num_procs();
	omp_set_num_threads(n_threads);

	double tic = omp_get_wtime();

	int valid_height = height - 2 * apron_y;
	int height_step = valid_height / n_threads + 1;

	for(int i=0; i<1000; i++)
#pragma omp parallel for
		for (int tid=0; tid<n_threads; tid++){
			int start_y = apron_y + tid * height_step;
			int end_y = min(start_y + height_step, height - apron_y);
			//	for(int i=0; i<100; i++)
			for (int i=start_y; i<end_y; i++){
				float* fr_ptr = fr_data + i * width + apron_x;
				float* ass_out = out_data + i * width + apron_x;
				float* wgt_out = ass_out + height * width;

				float *image_cache=(float*) malloc(sizeof(float)*filter_size);
				for (int j=apron_x; j<(width - apron_x); j++){

					for (int ii=0; ii< filter_size; ii++){
						image_cache[ii] = fr_ptr[pixel_offsets[ii]];
					} 

					float max_sim = -1e6;
					float best_ind = -1.0f;

					int fi=0;
					int filter_ind = 0;
					float temp_sum;
					while (fi<filter_bank_size)
					{
						temp_sum = 0.0f;
						//__assume_aligned(fb_array,32);
						//__assume_aligned(image_cache,32);
						#pragma ivdep
						for (int ii=0; ii < filter_size; ii++){
							temp_sum += fb_array[fi++] * image_cache[ii];
						}

						temp_sum = fabs(temp_sum);

						if (temp_sum > max_sim){
							max_sim = temp_sum;
							best_ind = filter_ind;
						}

						filter_ind++;
					}
					*ass_out = best_ind;
					*wgt_out = max_sim;

					fr_ptr++;
					ass_out++;
					wgt_out++;
				}
			}
		}

		double toc = omp_get_wtime();
		std::cout << "openmp filter old time: " << toc - tic << std::endl;
}

void transposeBank(float* &filter_bank) {
	// reorganize data in SIMD8 vectors
	// |0 1 2 .. 8| 0 1 2 .. 8 ..  =>> 0 0 0 ... 1 1 1 .. 
	int filter_size = 9;
	int num_filters = 100;
	float* tmpbank = (float*)_mm_malloc(num_filters * filter_size*sizeof(float), 256);
	for(int i=0; i<num_filters/8; i++)
	{
		for(int j=0; j<9; j++) {
			for(int k=0; k<8; k++)
				tmpbank[i*8*9+ j*8+ k] = filter_bank[i*8*9+ j+ k*9];
		}
	}
	// leftovers in smaller vecs

	{
		for(int j=0; j<9; j++) {
			for(int k=0; k<4; k++)
				tmpbank[96*9 + j*4+ k] = filter_bank[96*9 + j+ k*9];
		}
	}
	_mm_free(filter_bank);
	filter_bank = tmpbank;
}

void cosine_filter_transpose(
	float* fr_data, float* fb_array, 
	const int height, const int width, 
	const int filter_h, const int filter_w, 
	const int n_filters, float* out_data)
{
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
	int n_threads = omp_get_num_procs();
	omp_set_num_threads(n_threads);

	double tic = omp_get_wtime();
	for(int i=0; i<1000; i++) 
	{

		int valid_height = height - 2 * apron_y;
		int height_step = valid_height / n_threads + 1;
		float *image_cache=(float*) malloc(sizeof(float)*filter_size);    

#pragma omp parallel for
		for (int tid=0; tid<n_threads; tid++){
			int start_y = apron_y + tid * height_step;
			int end_y = min(start_y + height_step, height - apron_y);


			//	printf("start_y:%d end_y:%d apron_x:%d width:%d\n",start_y,end_y,apron_x, width );
			//		for(int i=0;i<100; i++)
			for (int i=start_y; i<end_y; i++){
				float* fr_ptr = fr_data + i * width + apron_x;
				float* ass_out = out_data + i * width + apron_x;
				float* wgt_out = ass_out + height * width;

				for (int j=apron_x; j<(width - apron_x); j++ ){


					for (int ii=0; ii< filter_size; ii++){
						// copy each pixel to all elements of vector
						image_cache[ii] = fr_ptr[pixel_offsets[ii]];
					} 

					float max_sim = -1e6;
					int best_ind = -1;

					int fi=0;
					int filter_ind = 0;
					int sssize = 9;
					// 96 filters, 9 values each
					while (fi<((n_filters/8)*8)*filter_size)
					{
						float temp_sum[8] = {0,0,0,0,0,0,0,0};


						for(int i=0; i<sssize; i++) {
						//__assume_aligned(fb_array,32);
						//__assume_aligned(image_cache,32);						
						float img = image_cache[i];
							for(int j=0; j<8; j++) {
								temp_sum[j] += img*fb_array[fi++];
							}
						}
						
						
						for(int j=0; j<8; j++) {
							temp_sum[j] = abs(temp_sum[j]);
						}

						for(int j=0; j<8; j++) {
							if(temp_sum[j] > max_sim) {
								max_sim = temp_sum[j];
								best_ind = filter_ind+j;
							}
						}

						filter_ind += 8;
					}

					float temp_sum[4] = {0,0,0,0};
					
					for(int i=0; i<9; i++) {
						//__assume_aligned(fb_array,32);
						//__assume_aligned(image_cache,32);
						#pragma ivdep
						for(int j=0; j<4; j++) {
							temp_sum[j] += image_cache[i]*fb_array[fi++];
						}
					}
					
					for(int j=0; j<4; j++) {
						temp_sum[j] = abs(temp_sum[j]);
					}
					for(int j=0; j<4; j++) {
						if(temp_sum[j] > max_sim) {
							max_sim = temp_sum[j];
							best_ind = filter_ind+j;
						}
					}

					*ass_out = (float)best_ind;
					*wgt_out = max_sim;

					fr_ptr++;
					ass_out++;
					wgt_out++;
				}
			}
		}
		//#pragma omp barrier
	}
	double toc = omp_get_wtime();
	std::cout << "openmp cosine_filter_transpose filter time: " << toc - tic << std::endl;
}

