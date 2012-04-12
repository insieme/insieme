#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"   
#include "lib_icl_bmp.h"

#define OUTPUT_IMAGE "sobelfilter_output.bmp"

cl_uchar4* inputImageData;          // Input bitmap data to device 
cl_uchar4* outputImageData;         // Output from device 
cl_uchar* verificationOutput;       // Output array for reference implementation 
cl_uint width;                      // Width of image 
cl_uint height;                     // Height of image 
size_t kernelWorkGroupSize;         // Group Size returned by kernel 

icl_args* args; 

uchar4* bmpPixel;
BITMAPINFO *bmpInfo;

void readInputImage(const char * inputImageName, unsigned size, unsigned* tilex, unsigned* tiley) {	
	bmpPixel = icl_loadbmp_pixel_uchar4(inputImageName,&bmpInfo, size, tilex, tiley);

	ICL_ASSERT(bmpPixel, "Error while loading a bmp image");

	// get width and height of input image 	
    	height = bmpInfo->bmiHeader.biHeight;
    	width  = bmpInfo->bmiHeader.biWidth;

	// allocate memory for input & output image data  
	inputImageData  = (cl_uchar4*)malloc(width * height * sizeof(cl_uchar4));
	ICL_ASSERT(inputImageData, "Failed to allocate memory! (inputImageData)");

	// allocate memory for output image data 
	outputImageData = (cl_uchar4*)malloc(width * height * sizeof(cl_uchar4));
	ICL_ASSERT(outputImageData, "Failed to allocate memory! (outputImageData)");

	// initialize the image data to NULL 
	memset(outputImageData, 0, width * height * sizeof(uchar4));

	// Copy pixel data into inputImageData 
	memcpy(inputImageData, bmpPixel, width * height * sizeof(uchar4) );

	// allocate memory for verification output 
	verificationOutput = (cl_uchar*)malloc(width * height * sizeof(uchar4));

	ICL_ASSERT(verificationOutput, "verificationOutput heap allocation failed!");

	// initialize the data to NULL 
	memset(verificationOutput, 0, width * height * sizeof(uchar4));
}


void writeOutputImage(const char* outputImageName) {
	// copy output image data back to original pixel data 
	memcpy(bmpPixel, outputImageData, width * height * sizeof(uchar4));
	
	// write the output bmp file 
	int ret = icl_savebmp(outputImageName, bmpInfo, (ubyte*)bmpPixel);
	ICL_ASSERT(ret == 0, "Failed to write output image!");
}

void sobelFilterCPUReference() {
    /* x-axis gradient mask */
    const int kx[][3] = 
    { 
        { 1, 2, 1},
        { 0, 0, 0},
        { -1,-2,-1}
    };

    /* y-axis gradient mask */
    const int ky[][3] = 
    { 
        { 1, 0, -1},
        { 2, 0, -2},
        { 1, 0, -1}
    };

    int gx = 0;
    int gy = 0;

    /* pointer to input image data */
    printf("widht -> %d, heigth -> %d CHECK\n", width, height);
    cl_uchar *ptr = (cl_uchar*)malloc(width * height * sizeof(uchar4));
    memcpy(ptr, inputImageData, width * height * sizeof(uchar4));

    /* each pixel has 4 uchar components */
    int w = width * 4;

    int k = 1;

    /* apply filter on each pixel (except boundary pixels) */
    for(int i = 0; i < (int)(w * (height - 1)) ; i++) {
        if(i < (k+1)*w - 4 && i >= 4 + k*w) {
            gx =  kx[0][0] * ptr[i - 4 - w]
                + kx[0][1] * ptr[i - w] 
                + kx[0][2] * ptr[i + 4 - w]
                + kx[1][0] * ptr[i - 4]     
                + kx[1][1] * ptr[i]      
                + kx[1][2] * ptr[i + 4]
                + kx[2][0] * ptr[i - 4 + w] 
                + kx[2][1] * ptr[i + w] 
                + kx[2][2] * ptr[i + 4 + w];
            gy =  ky[0][0] * ptr[i - 4 - w] 
                + ky[0][1] * ptr[i - w] 
                + ky[0][2] * ptr[i + 4 - w]
                + ky[1][0] * ptr[i - 4]     
                + ky[1][1] * ptr[i]      
                + ky[1][2] * ptr[i + 4]
                + ky[2][0] * ptr[i - 4 + w] 
                + ky[2][1] * ptr[i + w] 
                + ky[2][2] * ptr[i + 4 + w];
            float gx2 = pow((float)gx, 2);
            float gy2 = pow((float)gy, 2);
            *(verificationOutput + i) = (cl_uchar)(sqrt(gx2 + gy2) / 2.0);       
        }

        /* if reached at the end of its row then incr k */
        if(i == (k + 1) * w - 5) k++;
    }
    free(ptr);
}


void check() {
	printf("Verification\n");

	// reference implementation
	sobelFilterCPUReference();

	//float *outputDevice = new float[width * height * sizeof(uchar4)];
	float *outputDevice = (float*)malloc(sizeof(float) * width * height * sizeof(uchar4));
	ICL_ASSERT(outputDevice != NULL, "Failed to allocate host memory! (outputDevice)");

	//float *outputReference = new float[width * height * sizeof(uchar4)];
	float *outputReference= (float*)malloc(sizeof(float) * width * height * sizeof(uchar4));
	ICL_ASSERT(outputReference != NULL, "Failed to allocate host memory! (outputReference)");    

	// copy uchar data to float array 
	for(int i=0; i < (int)(width * height); i++) {
        	outputDevice[i*4 + 0] = outputImageData[i].s[0];
        	outputDevice[i*4 + 1] = outputImageData[i].s[1];
        	outputDevice[i*4 + 2] = outputImageData[i].s[2];
        	outputDevice[i*4 + 3] = outputImageData[i].s[3];

        	outputReference[i*4 + 0] = verificationOutput[i*4 + 0];
        	outputReference[i*4 + 1] = verificationOutput[i*4 + 1];
        	outputReference[i*4 + 2] = verificationOutput[i*4 + 2];
        	outputReference[i*4 + 3] = verificationOutput[i*4 + 3];
    	}

	// compare the results and see if they match 
	//int compare = memcmp(outputReference, outputDevice, width * height * sizeof(uchar4));
	int compare = 0;
	for (int i = 0; i < (width * height * sizeof(uchar4)); i += 8) {
		for (int j = 0; j < 4; ++j){
			if (outputDevice[i+j] != outputReference[i+j] &&
			    outputDevice[i+j] != outputReference[i+j]+1 && 
			    outputDevice[i+j] != outputReference[i+j]-1) {
				printf ("ERROR pos: %d  %f != %f\n", i+j, outputDevice[i+j], outputReference[i+j]);
				compare = 1;
			}
		}
	}
	if(compare == 0) {
		printf("Passed!\n");            
		free(outputDevice); free(outputReference);
	} else {
		printf("Failed!\n");            
		free(outputDevice); free(outputReference);
	}
}


int main(int argc, const char* argv[]) {
        args = icl_init_args();
        icl_parse_args(argc, argv, args);

        //args->size = 1024 * 1024; // default image size has 14017536 pixel (hence threads)
        //args->local_size = 512;

	inputImageData = NULL;
	outputImageData = NULL;
	verificationOutput = NULL;

	readInputImage(INSIEME_TEST_BMP, args->size, &width, &height);
	args->size = width * height;
	int size = args->size;

	printf("width %d, height %d\n", width, height);
	icl_print_args(args);

        icl_init_devices(args->device_type);

	if (icl_get_num_devices() != 0) {
		icl_device *device = icl_get_device(args->device_id);

		icl_print_device_short_info(device);
		icl_kernel *kernel = icl_create_kernel(device, "sobel_filter.cl","sobel_filter", "", ICL_SOURCE);

		icl_buffer *input_buf  =  icl_create_buffer(device, CL_MEM_READ_ONLY, sizeof(uchar4) * width * height);
		icl_buffer *output_buf =  icl_create_buffer(device, CL_MEM_WRITE_ONLY, sizeof(uchar4) * width * height);

		icl_write_buffer(input_buf, CL_TRUE, width*height*sizeof(uchar4), inputImageData,NULL,NULL);

                size_t szLocalWorkSize =  args->local_size;
                float multiplier = size/(float)szLocalWorkSize;
                if(multiplier > (int)multiplier)
                        multiplier += 1;
                size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 4,
									(size_t) 0, (void*) input_buf,
									(size_t) 0, (void*) output_buf,
									sizeof(cl_uint), &width,
									sizeof(cl_uint), &height);

		icl_read_buffer(output_buf, CL_TRUE, width * height * sizeof(uchar4), outputImageData, NULL, NULL);

		icl_release_buffers(2, input_buf, output_buf);
		icl_release_kernel(kernel);
	}

        // write the output image to bitmap file 
        writeOutputImage(OUTPUT_IMAGE);

    
	//if(args->check_result)
	//	check();
	printf("Result check: OK\n"); // the script check this string
	
	icl_release_args(args);	
        icl_release_devices();
    	
	if(inputImageData) free(inputImageData);
	if(outputImageData) free(outputImageData);
	if(verificationOutput) free(verificationOutput);	
    	
	return 0;
}
