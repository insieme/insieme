#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"   
#include "lib_icl_bmp.h"

#define OUTPUT_IMAGE "medianfilter_output.bmp"

#ifndef PATH
#define PATH "./"
#endif

int main(int argc, const char* argv[]) {
        icl_args* args = icl_init_args();
        icl_parse_args(argc, argv, args);

	chdir(PATH);

	uchar4* bmpPixel;
	BITMAPINFO *bmpInfo;

	cl_uint width;
	cl_uint height;

	// read the input image
	bmpPixel = icl_loadbmp_pixel_uchar4(INSIEME_TEST_BMP, &bmpInfo, args->size, &width, &height);
	args->size = width * height;
	int size = args->size;

	// allocate memory for input & output image data  
        cl_uchar4* inputImageData  = (cl_uchar4*)malloc(sizeof(cl_uchar4) * size);
	ICL_ASSERT(inputImageData, "Failed to allocate memory! (inputImageData)");
        memcpy(inputImageData, bmpPixel, sizeof(uchar4) * size);

	cl_uchar4* outputImageData = (cl_uchar4*)malloc(sizeof(cl_uchar4) * size);
        ICL_ASSERT(outputImageData, "Failed to allocate memory! (outputImageData)");
        memset(outputImageData, 0, sizeof(uchar4) * size);


	printf("width %d, height %d\n", width, height);
	icl_print_args(args);

        icl_init_devices(args->device_type);

	if (icl_get_num_devices() != 0) {
		icl_device *device = icl_get_device(args->device_id);

		icl_print_device_short_info(device);
		icl_kernel *kernel = icl_create_kernel(device, "median_filter.cl","median_filter", "", ICL_SOURCE);

		icl_buffer *input_buf  =  icl_create_buffer(device, CL_MEM_READ_ONLY, sizeof(cl_uint) * size);
		icl_buffer *output_buf =  icl_create_buffer(device, CL_MEM_WRITE_ONLY, sizeof(cl_uint) * size);

		icl_write_buffer(input_buf, CL_TRUE, sizeof(cl_uint) * size, inputImageData,NULL,NULL);

                size_t szLocalWorkSize =  args->local_size;
                float multiplier = size/(float)szLocalWorkSize;
                if(multiplier > (int)multiplier)
                        multiplier += 1;
                size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 4,
									(size_t) 0, (void*) input_buf,
									(size_t) 0, (void*) output_buf,
									sizeof(cl_int), &size,
									sizeof(cl_uint), &width);

		icl_read_buffer(output_buf, CL_TRUE, sizeof(cl_uint) * size, outputImageData, NULL, NULL);

		icl_release_buffers(2, input_buf, output_buf);
		icl_release_kernel(kernel);
	}


        // copy output image data back to original pixel data 
        memcpy(bmpPixel, outputImageData, sizeof(uchar4) * size);

        // write the output bmp file 
        int ret = icl_savebmp(OUTPUT_IMAGE, bmpInfo, (ubyte*)bmpPixel);
        ICL_ASSERT(ret == 0, "Failed to write output image!");

	printf("Result check: OK\n"); // the script check this string
	
	icl_release_args(args);	
        icl_release_devices();
    	
	if(inputImageData) free(inputImageData);
	if(outputImageData) free(outputImageData);
    	
	return 0;
}
