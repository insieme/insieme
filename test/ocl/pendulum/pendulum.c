#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#include <string.h>
#include "time.h"
#include <sys/time.h>

#include "lib_icl.h"
#include "lib_icl_ext.h"

#define POS(Image, X, Y) Image.data[(X)][(Y)]

#define FILE_BUF 512*8

typedef unsigned char uchar;

// -- Image Handling -----------------------------------

typedef struct {
	unsigned** data;
	int x, y;
} Image;


Image create_image(int x, int y) {

	unsigned* block = (unsigned*)malloc(sizeof(unsigned)*x*y);
	unsigned** index = (unsigned**)malloc(sizeof(unsigned*)*x);

	for(int i=0; i<x; i++) {
		index[i] = &(block[i*x]);
	}

	return (Image){index, x, y};
}

void delete_image(Image* image) {
	free(image->data[0]);
	free(image->data);
}

void print_target_image_ASCII(Image image, unsigned sources) {
	for (int i=0; i<image.x; i++) {
		for (int j=0; j<image.y; j++) {
			if(POS(image,i,j) == 0) printf(" ");
			else if(POS(image,i,j) == sources) printf("X");
			else printf("%c", ((int)'A')+POS(image,i,j)-1);
		}	
		printf("\n");
	}
}

void write_image(Image target, Image dist, char* filename, unsigned minSteps, unsigned maxSteps);


// -- Sources Handling -----------------------------------

enum Kind { Linear, Magnet };

typedef struct {
	enum Kind type;
	double pos[2];
	double mult;
	double size;
} Source;


// -- Input File Handling -----------------------------------

typedef struct {
	unsigned numSources;
	int x;
	int y;
	double scale;
	double dt;
	double friction;
	double height;
	double abortVelocity;
	unsigned minSteps;
	unsigned maxSteps;
} Settings;

int main(int argc, const char* argv[]) {
        icl_args* args = icl_init_args();
        icl_parse_args(argc, argv, args);

        int width = (int)floor(sqrt(args->size));
        args->size = width * width;
        int size = args->size;
        icl_print_args(args);

	Settings settings = {4, width, width, 800.0, 0.01, 0.01, 0.3, 0.01, 400, 100000};
	Source* sources = (Source[]){ (Source){Linear,  {0, 0}, 0.01, 0.02},
			(Source){Magnet, {1, 0}, 0.08, 0.02},
			(Source){Magnet, {-0.5, +0.866025404}, 0.08, 0.02},
			(Source){Magnet, {-0.5, -0.866025404}, 0.08, 0.02},
			};

	// migrate settings
	unsigned num_sources = settings.numSources;
	int x = settings.x;
	int y = settings.y;
	double scale = settings.scale;
	double dt = settings.dt;
	double friction = settings.friction;
	double height = settings.height;
	unsigned min_steps = settings.minSteps;
	unsigned max_steps = settings.maxSteps;
	double abortVelocity = settings.abortVelocity;
	

	Image image = create_image(x,y);
	Image dist = create_image(x,y);

	icl_init_devices(args->device_type);
        if (icl_get_num_devices() != 0) {
                icl_device* dev = icl_get_device(args->device_id);

                icl_print_device_short_info(dev);
                icl_buffer* buf_image = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(unsigned) * size);
                icl_buffer* buf_dist = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(unsigned) * size);
                icl_buffer* buf_sources = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(Source) * num_sources);

                icl_write_buffer(buf_sources, CL_TRUE, sizeof(Source) * num_sources, &sources[0], NULL, NULL);
	
		icl_kernel* kernel = icl_create_kernel(dev, "pendulum.cl", "pendulum", "", ICL_SOURCE);

                size_t szLocalWorkSize =  args->local_size;
                float multiplier = size/(float)szLocalWorkSize;
                if(multiplier > (int)multiplier)
                        multiplier += 1;
                size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL,
					   13,  (size_t)0, 		(void*) buf_image,
						(size_t)0, 		(void*) buf_dist,
						(size_t)0, 		(void*) buf_sources,
						sizeof(unsigned), 	(void*) &num_sources,
						sizeof(double), 	(void*) &dt,
						sizeof(double), 	(void*) &friction,
						sizeof(double), 	(void*) &height,
						sizeof(unsigned), 	(void*) &min_steps,
						sizeof(unsigned),	(void*) &max_steps,
						sizeof(double), 	(void*) &abortVelocity,
						sizeof(int),		(void*) &width,
						sizeof(int),		(void*) &size,
						sizeof(double),		(void*) &scale);

                icl_read_buffer(buf_image, CL_TRUE, sizeof(unsigned) * size, &(image.data[0][0]), NULL, NULL);
                icl_read_buffer(buf_dist, CL_TRUE, sizeof(unsigned) * size, &(dist.data[0][0]), NULL, NULL);

                icl_release_buffers(3, buf_image, buf_dist, buf_sources);
                icl_release_kernel(kernel);
		
	}
	icl_release_devices();
	
	unsigned maxSteps = 0;
	unsigned minSteps = max_steps;
	for(int i=0; i<x; i++) {
		for(int j=0; j<y; j++) {
			maxSteps =  POS(dist, i,j) > maxSteps ? POS(dist, i,j) : maxSteps;
			minSteps =  POS(dist, i,j) < minSteps ? POS(dist, i,j) : minSteps;
		}
	}
	printf("Number of steps calculated: %u .. %u\n", minSteps, maxSteps);

	print_target_image_ASCII(image, num_sources);
	write_image(image, dist, "out.bmp", minSteps, maxSteps);

	delete_image(&image);
	delete_image(&dist);
}


// ----------------------------------------------------------------
//  Output of a file ...
// ----------------------------------------------------------------


typedef struct {
  unsigned char magic[2];
} bmp_magic;

typedef struct {
  uint32_t filesz;
  uint16_t creator1;
  uint16_t creator2;
  uint32_t bmp_offset;
} bmp_header;

typedef struct {
  uint32_t header_sz;
  int32_t width;
  int32_t height;
  uint16_t nplanes;
  uint16_t bitspp;
  uint32_t compress_type;
  uint32_t bmp_bytesz;
  int32_t hres;
  int32_t vres;
  uint32_t ncolors;
  uint32_t nimpcolors;
} bmp_info_header;


#pragma pack(1)
typedef struct {
	uchar r;
	uchar g;
	uchar b;
} Color;


Color getColor(unsigned target, unsigned distance, unsigned minSteps, unsigned maxSteps) {
	Color color;
	switch(target) {
	case 0: color = (Color){255,255,255}; break;
	case 1: color = (Color){255,0,0}; break;
	case 2: color = (Color){0,255,0}; break;
	case 3: color = (Color){0,0,255}; break;
	case 4: color = (Color){255,255,0}; break;
	case 5: color = (Color){0,255,255}; break;
	case 6: color = (Color){255,0,255}; break;
	default: color = (Color){0,0,0}; break;
	}
	
	double factor = 1.0 - (distance-minSteps)/(double)(maxSteps-minSteps);
	//factor = (factor > 1.0)?1.0:((factor < 0.0)?0.0:factor);
	color.r = (uchar)(color.r * factor);
	color.g = (uchar)(color.g * factor);
	color.b = (uchar)(color.b * factor);
	return color;
}

void write_image(Image target, Image dist, char* filename, unsigned minSteps, unsigned maxSteps) {


	FILE* out = fopen(filename, "wb");
	if (!out) {
		printf("Error opening output file!\n");
		return;
	}

	// write header
	bmp_magic magic = (bmp_magic){{0x42, 0x4D}};
	fwrite(&magic, sizeof(bmp_magic), 1, out);

	bmp_header header;
	header.bmp_offset = sizeof(bmp_magic) + sizeof(bmp_header) + sizeof(bmp_info_header);
	header.filesz = header.bmp_offset + sizeof(Color) * target.x * target.y;
	header.creator1 = 0;
	header.creator2 = 0;
	fwrite(&header, sizeof(bmp_header), 1, out);

	bmp_info_header info;
	info.header_sz = 40;
	info.width = target.x;
	info.height = target.y;
	info.nplanes = 1;
	info.bitspp = 24;
	info.compress_type = 0;
	info.bmp_bytesz = 0;
	info.hres = 1000;
	info.vres = 1000;
	info.ncolors = 0;
	info.nimpcolors = 0;
	fwrite(&info, sizeof(bmp_info_header), 1, out);

	// write data
	for (int i=0; i<target.x; i++) {
		for (int j=0; j<target.y; j++) {
			Color color = getColor(POS(target,i,j), POS(dist,i,j), minSteps, maxSteps);
			fwrite(&color, sizeof(Color), 1, out);
		}
		char pad = 0;
		int c = 3 * target.y;
		while (c%4) {
			fwrite(&pad, 1, 1, out);
			c++;
		}
	}

	// done
	fclose(out);
}


