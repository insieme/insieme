#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"
#include <math.h>

static inline float icl_random01_float(){ return (float) rand()/(RAND_MAX); }

void icl_fillrandom_float(float* arrayPtr, int width, int height, float rangeMin, float rangeMax){
    if(!arrayPtr) {
        fprintf(stderr, "Cannot fill array: NULL pointer.\n");
        return;
    }
    double range = (double)(rangeMax - rangeMin);     
    for(int i = 0; i < height; i++)
        for(int j = 0; j < width; j++) {
            int index = i*width + j;
            arrayPtr[index] = rangeMin + (float)(range*icl_random01_float()); 	
        }    
}	

int main(int argc, const char* argv[]) 
{
	int numTimesteps = 16;
	cl_float2 origin   = { 0.f,  0.f  };
	cl_float2 cellSize = { 0.1f, 0.1f };	
	float startTime = 1.0f;
	float advectionTime = 0.5f;	

	
	icl_args* args = icl_init_args();
	//args->size = 1024;

	icl_parse_args(argc, argv, args);		
	icl_print_args(args);
	
	// from a work-item number to a 2D tile
	unsigned tilesize = (unsigned)sqrt((double)args->size );

	unsigned mulTile = 16;
	 
	if(tilesize < 32){
		mulTile = 16;
	}
	else if(tilesize < 128){
		mulTile = 32;
	}
	else if(tilesize < 512){
		mulTile = 128;
	}
	else // if(tilesize >= 512) 
	{
		mulTile = 512;
	}
	tilesize = (tilesize / mulTile ) * mulTile ; // this rounds the size to a multiple of <mulTile>
	unsigned int height = tilesize;
	unsigned int width = args->size / tilesize;

	printf("ftle for 2D image (%u, %u) - %d threads\n", width, height, args->size);

	// prepare inputs
	size_t size = width * height;
	float* data      = (float*) malloc(sizeof(float) * size * numTimesteps * 2);
	float* timesteps = (float*) malloc(sizeof(float) * numTimesteps);
	float* flowMap   = (float*) malloc(sizeof(float) * size * 2);
	float* output    = (float*) malloc(sizeof(float) * size * 2);
	
	icl_fillrandom_float(data, size*2, 1, -1.0f, 1.0f);
	for(int i=0; i<numTimesteps; i++)
		timesteps[i] = 0.05f * i;	
	icl_fillrandom_float(flowMap, size, 1, -1.0f, 1.0f); // filled in case we only run the 2nd kernel
	

	icl_init_devices(ICL_CPU);

	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(0);

		icl_print_device_short_info(dev);

		icl_kernel* kernel = icl_create_kernel(dev, "ftle.cl", "computeFTLE", "-I.", ICL_SOURCE);

		icl_buffer* buf_data      = icl_create_buffer(dev, CL_MEM_READ_ONLY,  sizeof(cl_float2) * size * numTimesteps);
		icl_buffer* buf_timesteps = icl_create_buffer(dev, CL_MEM_READ_ONLY,  sizeof(float) * numTimesteps);
		icl_buffer* buf_flowMap   = icl_create_buffer(dev, CL_MEM_READ_ONLY,  sizeof(cl_float2) * size);		
printf("JASFD\n");
		icl_buffer* buf_output    = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(cl_float2) * size);

		icl_write_buffer(buf_data, CL_FALSE, sizeof(cl_float2) * size * numTimesteps, &data[0], NULL, NULL);
		icl_write_buffer(buf_timesteps, CL_TRUE, sizeof(float) * numTimesteps, &timesteps[0], NULL, NULL);

		size_t localWorkSize = args->local_size;


		icl_write_buffer(buf_flowMap, CL_TRUE, sizeof(cl_float2) * size, &flowMap[0], NULL, NULL);		
		icl_run_kernel(kernel, 1, &size, &localWorkSize, NULL, NULL, 6,
			(size_t)0, (void *)buf_flowMap,
			sizeof(cl_uint), (void *)&width,
			sizeof(cl_uint), (void *)&height,			
			sizeof(cl_float2), (void *)&cellSize,
			sizeof(cl_float), (void *)&advectionTime,
			(size_t)0, (void *)buf_output
		);
		icl_read_buffer(buf_output, CL_TRUE, sizeof(cl_float2) * size, &output[0], NULL, NULL);

		icl_release_buffers(4, buf_data, buf_timesteps, buf_flowMap, buf_output);
		icl_release_kernel(kernel);		
	}

		
	if(args->check_result) printf("Checking results - not implemented.\n");
	
			
	icl_release_devices();

	
	free(timesteps);
	free(flowMap);
	free(data);
	free(output);

	#ifdef _MSC_VER
	icl_prompt();
	#endif

	return 0;
}
