#pragma once

#include <stdio.h>
#include <stdlib.h>

#include "particles.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

#define CTOU(color) (*(unsigned*)(void*)&(color))

#define FILE_BUF 512*8

typedef unsigned long long ull;
typedef unsigned char uchar;

// -- image Handling -----------------------------------

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

typedef struct {
	uchar r;
	uchar g;
	uchar b;
} Color;

typedef struct {
	Color** data;
	int x, y;
} Image;

Image create_image(int x, int y) {
	Color* block = (Color*)malloc(sizeof(Color)*x*y);
	Color** index = (Color**)malloc(sizeof(Color*)*x);

	for(int i=0; i<x; i++) {
		index[i] = &(block[i*x]);
	}

	Image ret = {index, x, y};
	return ret;
}

void delete_image(Image* image) {
	free(image->data[0]);
	free(image->data);
}

void write_image(Image target, char* filename) {
	FILE* out = fopen(filename, "wb");
	if (!out) {
		printf("Error opening output file!\n");
		return;
	}

	// write header
	bmp_magic magic = {{0x42, 0x4D}};
	fwrite(&magic, sizeof(bmp_magic), 1, out);

	bmp_header header;
	header.bmp_offset = sizeof(bmp_magic) + sizeof(bmp_header) + sizeof(bmp_info_header);
	header.filesz = header.bmp_offset + sizeof(Color) * target.x * target.y;
	header.creator1 = 0;
	header.creator2 = 0;
	fwrite(&header, sizeof(bmp_header), 1, out);

	bmp_info_header info;
	info.header_sz = 40;
	info.width = target.y;
	info.height = target.x;
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
			Color color = target.data[j][i];
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

static Color 
	g_colors[4] = { {255,255,255}, {255,0,0}, {0,255,0}, {0,0,255} },
	g_background = {0,0,0};
static int g_pointsize = 5;

void point(Image img, int width, int height, int x0, int y0, int r, Color color) {
	int x,y, x1,y1;
	
	for(x=-r; x<=r; x++) {
		for(y=-r; y<=r; y++) {
			if(x*x+y*y < r*r) {
				x1 = x0+x;
				y1 = y0+y;
				if(x1>=0 && x1<width && y1>=0 && y1<height)	{
					img.data[y1][x1] = color;
				}
			}
		}
	}
}

Image init_image(double *length, int pixels_per_unit, int *pwidth, int *pheight) {  
	Image image; 
	int width, height, size;
	
	width = (int)(pixels_per_unit*length[0]);
	height = (int)(pixels_per_unit*length[1]);
	size = width*height;
	
	//printf("Creating image of size %d x %d\n", width, height);
	image = create_image(width, height);
	
	for(int x=0; x<width; x++)
		for(int y=0; y<height; y++)
			image.data[y][x] = g_background;
	
	*pwidth = width; 
	*pheight = height;
	return image;
}

void add_particles(Image image, int width, int height,
		   particle *list, int N, double *length, int pixels_per_unit) {  
	int i,x,y;
	
	for(i=0; i<N; i++) {
		x = (int)(list[i].pos[0]/length[0]*width);
		y = (int)(list[i].pos[1]/length[1]*height);
		
		#ifdef _V_FLIP
		y = P->height-1-y;
		#endif
		
		if(x<0 || x>=width || y<0 || y>=height) {
			//printf("Warning: Particle not in  domain !\n");
		} else {
			point(image, width, height, x, y, g_pointsize, g_colors[list[i].kind]);
		}
	}
}

void write_particle_image(char * filename, particle *list, int N, double *length, int time_step, int pixels_per_unit) {  
	char dummy[80];
	int width, height;
	
	Image image = init_image(length, pixels_per_unit, &width, &height);  
	add_particles(image, width, height, list, N, length, pixels_per_unit);
	sprintf(dummy, "%s.%03d.bmp", filename, time_step);
	write_image(image, dummy);
	delete_image(&image);
}
