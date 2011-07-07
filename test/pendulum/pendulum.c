
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>

#define POS(Image, X, Y) Image.data[(X)][(Y)]

#define MIN_STEPS 400
#define MAX_STEPS 100000

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

void print_target_image_ASCII(Image image) {
	for (int i=0; i<image.x; i++) {
		for (int j=0; j<image.y; j++) {
			switch(POS(image,i,j)) {
			case 0: printf(" "); break;
			case 1: printf("A"); break;
			case 2: printf("B"); break;
			case 3: printf("C"); break;
			case 4: printf("X"); break;
			}
			
		}	
		printf("\n");
	}
}

void write_image(Image target, Image dist, char* filename);


// -- Sources Handling -----------------------------------

enum Kind { Linear, Magnet };

typedef struct {
	enum Kind type;
	double pos[2];
	double mult;
	double size;
} Source;


#define sqr(x) (x)*(x)
#define ABS(x) sqrt( sqr(x[0]) + sqr(x[1]) ) 
#define DIST(x, y) sqrt(sqr((x)[0]-(y)[0])+sqr((x)[1]-(y)[1]))
#define ASS(x, y) ((x)[0] = (y)[0], (x)[1] = (y)[1])

typedef struct {
	unsigned target;
	unsigned numSteps;
} Trace;

Trace getTarget(double i, double j, 
	Source* sources, unsigned num_sources,
	double dt, double friction, double height,
	unsigned min_steps, unsigned max_steps,
	double abortVelocity) {

	// pendulum properties pendulum
	double pos[2] = {i,j};  // the current position
	double vel[2] = {0,0};  // the velocity

	// acceleration
	double acc[2] = {0,0};	
	double acc_new[2] = {0,0};
	double acc_old[2] = {0,0}; 

	// some loop-invariant "constants"
	double sqrt_dt = sqrt(dt); 

	// iterate until close to a source and velocity is small
	for (unsigned step = 0; step < max_steps; ++step) {

		// ----- update position -----
		pos[0] += vel[0] * dt + sqrt_dt * (2.0/3.0 * acc[0] - 1.0/6.0 * acc_old[0]);	
		pos[1] += vel[1] * dt + sqrt_dt * (2.0/3.0 * acc[1] - 1.0/6.0 * acc_old[1]);

		acc_new[0] = 0;				
		acc_new[1] = 0;
		
		// 1) calculate forces (proportional to the inverse square of the distance)
		for (unsigned i = 0; i < num_sources; i++) {
			Source* cur = &(sources[i]); 

			double r[2] = { pos[0] - cur->pos[0], pos[1] - cur->pos[1] };

			if (cur->type == Linear) {
				// Hooke's law (pulling back the pendulum to (0,0)
				
				// add effect to forces
				acc_new[0] -= cur->mult * r[0];
				acc_new[1] -= cur->mult * r[1];

			} else {
				// Magnet forces: m * a = k * abs(r) / abs(r^3)

				double dist = sqrt ( sqr(pos[0] - cur->pos[0]) +
									 sqr(pos[1] - cur->pos[1]) +
									 sqr(height) );
			
				// add effect to forces
				acc_new[0] -= (cur->mult / (dist*dist*dist)) * r[0];
				acc_new[1] -= (cur->mult / (dist*dist*dist)) * r[1];
			}

			// check whether close enough to current source
			if (step > min_steps && ABS(r) < cur->size && ABS(vel) < abortVelocity) {

				// this is the right magnet
				return (Trace){i, step};
			}
		}

		// 2) add (linear) friction proportinal to velocity
		acc_new[0] -= vel[0] * friction;
		acc_new[1] -= vel[1] * friction;

		// 3) update velocity
		vel[0] += dt * ( 1.0/3.0 * acc_new[0] + 5.0/6.0 * acc[0] - 1.0/6.0 * acc_old[0]);
		vel[1] += dt * ( 1.0/3.0 * acc_new[1] + 5.0/6.0 * acc[1] - 1.0/6.0 * acc_old[1]);

		// 4) flip the accelerator values
		ASS(acc_old, acc);
		ASS(acc, acc_new);
	}

	// undecided after MAX number of steps => return num_sources + 1
	return (Trace){num_sources + 1, max_steps};
}

int main() {

	// set up resolution
	int x = 12;
	int y = 12;

//	x = y = 12;
	x = y = 60;
//	x = y = 128;
//	x = y = 256;
//	x = y = 1024;
//	x = y = 2048;

	double scale = 800;

	double magSize = 0.02;
	double magPower = 0.08;
//	double magPower = 0.2;

	unsigned num_sources = 4;
	Source sources[] = {
		(Source){Linear, {0, 0}, 0.01, magSize},
		(Source){Magnet, {1, 0}, magPower, magSize},
		(Source){Magnet, {-0.5, +0.866025404}, magPower, magSize},
		(Source){Magnet, {-0.5, -0.866025404}, magPower, magSize},
	};

	double dt = 0.01;
	double friction = 0.01;
	double height = 0.3;
	unsigned min_steps = MIN_STEPS;
	unsigned max_steps = MAX_STEPS;
	double abortVelocity = 0.01;


	Image image = create_image(x,y);
	Image dist = create_image(x,y);

	// compute target magnet for each point
	#pragma omp parallel for
	for(int i=0; i<x; i++) {
		for (int j=0; j<y; j++) {
			double curX = (-1.0 + (double)i/(double)(x-1) * 2.0) * scale;
			double curY = (-1.0 + (double)j/(double)(y-1) * 2.0) * scale;

			Trace res = getTarget(curX, curY,
						sources, num_sources, dt, friction,
						height, min_steps, max_steps, abortVelocity);

			POS(image,i,j) = res.target;
			POS(dist, i,j) = res.numSteps;
		}
	}

	print_target_image_ASCII(image);
//	write_image(image, dist, "out.bmp");

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
	char r;
	char g;
	char b;
} Color;


Color getColor(unsigned target, unsigned distance) {
	Color color;
	switch(target) {
	case 0: color = (Color){255,255,255}; break;
	case 1: color = (Color){255,0,0}; break;
	case 2: color = (Color){0,255,0}; break;
	case 3: color = (Color){0,0,255}; break;
	default: color = (Color){0,0,0}; break;
	}

//	double factor = (distance - MIN_STEPS)/(MAX_STEPS/1000-MIN_STEPS);
//	factor = (factor > 1.0)?1.0:((factor < 0.0)?0.0:factor);
//	color.r = (char)(color.r * factor);
//	color.g = (char)(color.g * factor);
//	color.b = (char)(color.b * factor);
	return color;
}

void write_image(Image target, Image dist, char* filename) {


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
			Color color = getColor(POS(target,i,j), POS(dist,i,j));
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


