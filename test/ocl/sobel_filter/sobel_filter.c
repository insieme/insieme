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
size_t blockSizeX;                  // Work-group size in x-direction 
size_t blockSizeY;                  // Work-group size in y-direction 

icl_args* args; 

uchar4* bmpPixel;
BITMAPINFO *bmpInfo;

// prototypes
void runCLKernels();
void sobelFilterCPUReference();
void run();
void cleanup();
void verifyResults();

void initSobelFilter() {        
	inputImageData = NULL;
	outputImageData = NULL;
	verificationOutput = NULL;	
	blockSizeX = args->local_size;
	blockSizeY = 1;
}

void readInputImage(const char * inputImageName, unsigned size, unsigned *tilex, unsigned *tiley) {	
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


void runCLKernels(icl_device *device, icl_kernel *kernel) {
	icl_buffer *inputBuffer  =  icl_create_buffer(device, CL_MEM_READ_ONLY, sizeof(uchar4) * width * height);
	icl_buffer *outputBuffer =  icl_create_buffer(device, CL_MEM_WRITE_ONLY, sizeof(uchar4) * width * height);

	icl_write_buffer(inputBuffer, CL_TRUE, width*height*sizeof(uchar4), inputImageData,NULL,NULL);	

	size_t globalThreads[] = { width * height };
	size_t localThreads[]  = { args->local_size };

	icl_run_kernel(kernel, 1, globalThreads, localThreads, NULL, NULL, 4,   
		(size_t) 0, (void*) inputBuffer, 
		(size_t) 0, (void*) outputBuffer,
		sizeof(cl_uint), &width,
		sizeof(cl_uint), &height
		); 
	
	icl_read_buffer(outputBuffer, CL_TRUE, width * height * sizeof(uchar4), outputImageData, NULL, NULL);

	icl_release_buffer(inputBuffer);
	icl_release_buffer(outputBuffer);
}


void run() {
	icl_init_devices(args->device_type); 
	icl_device *device = icl_get_device(args->device_id);
	icl_print_device_short_info(device);

	icl_kernel *kernel = icl_create_kernel(device, "sobel_filter.cl","sobel_filter", "", ICL_SOURCE);

	clGetKernelWorkGroupInfo(kernel->kernel, device->device, CL_KERNEL_WORK_GROUP_SIZE, sizeof(size_t), &kernelWorkGroupSize, 0);
	//ICL_ASSERT(status == CL_SUCCESS, "clGetKernelWorkGroupInfo  failed.");

	if((blockSizeX * blockSizeY) > kernelWorkGroupSize) {
		fprintf(stderr, "Out of Resources!\nGroup Size specified : %lu\n", blockSizeX * blockSizeY);
		fprintf(stderr, "Max Group Size supported on the kernel : %lu\n", kernelWorkGroupSize);
		fprintf(stderr, "Falling back to %lu\n", kernelWorkGroupSize);

		// Three possible cases 
		if(blockSizeX > kernelWorkGroupSize)
		{
			blockSizeX = kernelWorkGroupSize;
			blockSizeY = 1;
		}
	}

	// Set kernel arguments and run kernel 
	runCLKernels(device, kernel);

	icl_release_kernel(kernel);
	icl_release_devices();

	// write the output image to bitmap file 
	writeOutputImage(OUTPUT_IMAGE);
}

void cleanup() {
    // release program resources (input memory etc.) 
    if(inputImageData) 
        free(inputImageData);

    if(outputImageData)
        free(outputImageData);

    if(verificationOutput) 
        free(verificationOutput);
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
    cl_uchar *ptr = (cl_uchar*)malloc(width * height * sizeof(uchar4) );
    memcpy(ptr, inputImageData, width * height * sizeof(uchar4) );

    /* each pixel has 4 uchar components */
    int w = width * 4;

    int k = 1;

    /* apply filter on each pixel (except boundary pixels) */
    for(int i = 0; i < (int)(w * (height - 1)) ; i++) 
    {
        if(i < (k+1)*w - 4 && i >= 4 + k*w)
        {
            gx =  kx[0][0] * *(ptr + i - 4 - w)  
                + kx[0][1] * *(ptr + i - w) 
                + kx[0][2] * *(ptr + i + 4 - w)
                + kx[1][0] * *(ptr + i - 4)     
                + kx[1][1] * *(ptr + i)      
                + kx[1][2] * *(ptr + i + 4)
                + kx[2][0] * *(ptr + i - 4 + w) 
                + kx[2][1] * *(ptr + i + w) 
                + kx[2][2] * *(ptr + i + 4 + w);
            gy =  ky[0][0] * *(ptr + i - 4 - w) 
                + ky[0][1] * *(ptr + i - w) 
                + ky[0][2] * *(ptr + i + 4 - w)
                + ky[1][0] * *(ptr + i - 4)     
                + ky[1][1] * *(ptr + i)      
                + ky[1][2] * *(ptr + i + 4)
                + ky[2][0] * *(ptr + i - 4 + w) 
                + ky[2][1] * *(ptr + i + w) 
                + ky[2][2] * *(ptr + i + 4 + w);
            float gx2 = pow((float)gx, 2);
            float gy2 = pow((float)gy, 2);
            *(verificationOutput + i) = (cl_uchar)(sqrt(gx2 + gy2) / 2.0);       
        }

        /* if reached at the end of its row then incr k */
        if(i == (k + 1) * w - 5)
        { k++; }    
    }
    free(ptr);
}


void verifyResults()
{
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
    for(int i=0; i < (int)(width * height); i++)
    {
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
	int compare = memcmp(outputReference, outputDevice, width * height * sizeof(uchar4));
	if(compare == 0) {
		printf("Passed!\n");            
		//delete[] outputDevice;  delete[] outputReference;
		free(outputDevice); free(outputReference);
	} else {
		printf("Failed!\n");            
		free(outputDevice); free(outputReference);
	}
}


int main(int argc, const char* argv[]) {
        args = icl_init_args();
        icl_parse_args(argc, argv, args);

        //int width = (int)floor(sqrt(args->size));
        args->size = 1024 * 1024; // default image size has 14017536 pixel (hence threads)
        args->local_size = 512;

	unsigned tile_x, tile_y;
  	initSobelFilter();		

	// allocate host memory and read input image     
	readInputImage(INSIEME_TEST_BMP, args->size, &tile_x, &tile_y);
	width  = tile_x;
	height = tile_y;
	args->size = width * height; 
	printf("width %d, height %d\n", tile_x, tile_y);
	//icl_print_arguments(&arguments);
	icl_print_args(args);

	run();
    
	if(args->check_result)
		verifyResults();
	
	cleanup();
    	return 0;
}
