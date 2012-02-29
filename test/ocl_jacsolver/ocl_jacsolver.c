/**************************************************************************
 Include Files
 **************************************************************************/

#include <unistd.h>
#include <libgen.h>
#include <getopt.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/timeb.h>
#include <math.h>

/* OpenCL */
#include "lib_icl.h"


/**************************************************************************
 Defines and Macros
 **************************************************************************/

/* PI of course */
#define PI                   3.14159265

/* default tolerance for Jacobi iteration */
#define DEFAULT_TOLERANCE    (0.000001f)

/* initial guess */
#define INITIAL_GUESS        0.5

/* minimum number of nodes */
#define NODE_SIZE_MIN        8

/* default number of nodes (same for all dimensions) */
#define DEFAULT_NODES        8

/* default size of OpenCL workblock (same for all dimensions) */
#define DEFAULT_BLOCK        8

/* maximum number of iterations */
#define MAX_ITERATIONS       1000000

/* maximum size of array that can be printed */
#define MAX_PRINT_SIZE       16

/* value of "device" used for reference or exact calculation */
/* these values should not overlap predefined OpenCL device types */
#define REFERENCE_CALC       -10
#define EXACT_CALC           -11

/* dimension indices */
#define X                    0
#define Y                    1
#define DIMENSIONS           2

/* 5 point stencil in 2 dimensions */
#define STENCIL_SIZE         5

/* ghost cell width */
#define GHOST_CELL_WIDTH     1

/* indices for old and new arrays */
#define OLD 0
#define NEW 1


/* for OpenCL Workgroup size */
#define DEFAULT_WORKGROUP_SIZE 8

/* used for bitmap output as V value in HSV color scheme */
#define BRIGHTNESS_DEFAULT     192

/* macros to help convert HSV color to RGB color */
#define CALC_H1(h, s)        ((unsigned char)(BRIGHTNESS_DEFAULT * (1.0f - (h-s))))
#define CALC_H2(h, s)        ((unsigned char)(BRIGHTNESS_DEFAULT * (h-s)))

/* MIN value macro */
#define    MIN(a, b)         ( (a>b) ? (b) : (a) )

/* macros to swap array pointers */
#define SWAP_PTR(a, b)       { value_type *tmp = a; a = b; b = tmp; }
#define SWAP_BUF(a, b)       { icl_buffer* tmp = a; a = b; b = tmp; }

/* error macro and message macros */
#define QUIT(fmt ...)   { printf("Error: %s", fmt); exit_app(EXIT_FAILURE);}

/* macros for asynchronous reading from or writing data to device memory */
#define READ_DEVICE_MEMORY(queue, device_mem, offset, size, host_mem) {                      \
        CLU_CHECK_ERROR("clEnqueueReadBuffer failed",                                        \
            clEnqueueReadBuffer(queue, device_mem, CL_FALSE, sizeof(value_type) * offset,    \
                                sizeof(value_type)*size, host_mem+offset, 0, NULL, NULL)); }

#define WRITE_DEVICE_MEMORY(queue, device_mem, offset, size, host_mem) {                     \
        CLU_CHECK_ERROR("clEnqueueWriteBuffer failed",                                       \
            clEnqueueWriteBuffer(queue, device_mem, CL_FALSE, sizeof(value_type) * offset,   \
                                sizeof(value_type)*size, host_mem+offset, 0, NULL, NULL)); }


/**************************************************************************
 Types
 **************************************************************************/

/*  Define precision for values.  */
typedef float value_type;


/**************************************************************************
 Local Functions
 **************************************************************************/

static void exit_app(int rc);
static void convert_RGB(value_type v, unsigned char *r,
                        unsigned char *g, unsigned char *b);
static void exact_compute(value_type *a,
                        size_t size[DIMENSIONS],
                        value_type d[DIMENSIONS]);
static void exchange_ghost_cells(value_type *a,
                        size_t size[DIMENSIONS]);
static void get_arguments(int argc, char *argv[],
                         unsigned int *max_iter,
                        size_t size[DIMENSIONS],
                        size_t block_size[DIMENSIONS],
                        cl_device_type *device,
                        unsigned int *full_copy,
                        unsigned int *verify);

/*
static void read_ghost_cells_from_device(cl_mem a_buf,
                                        value_type *a,
                                        size_t size[DIMENSIONS],
                                        cl_command_queue queue);*/
static void write_ghost_cells_to_device(cl_mem a_buf,
                                        value_type *a,
                                        size_t size[DIMENSIONS],
                                        cl_command_queue queue);


static value_type ocl_jacobi_reduce(value_type *delta,
                                    size_t delta_size[DIMENSIONS]);
static void ocl_jacobi(value_type *a[2],
                        unsigned int max_iter,
                        size_t size[DIMENSIONS],
                        value_type tolerance,
                        value_type d[DIMENSIONS],
                        size_t local_workblock_size[DIMENSIONS],
                        cl_device_type device_type,
                        unsigned int full_copy);
static void reference_jacobi(value_type *a[2],
                            unsigned int max_iter,
                            size_t size[DIMENSIONS],
                            value_type tolerance,
                            value_type d[DIMENSIONS]);
static value_type reference_jacobi_kernel(value_type *a, value_type *anew,
                                          size_t size[DIMENSIONS]);
static void print_array(const char *legend, 
                        value_type *a,
                        size_t size[DIMENSIONS],
                        value_type d[DIMENSIONS]);
static void set_boundary_conditions(value_type *a,
                        size_t size[DIMENSIONS],
                        value_type d[DIMENSIONS]);
static void set_initial_solution(value_type *a,
                        size_t size[DIMENSIONS],
                        value_type guess);
static void save_bitmap(value_type *a,
                        size_t size[DIMENSIONS]);


/**************************************************************************
 Globals
 **************************************************************************/

/* only globals are those needed for exit_app() which cleans up after an error
   or the end of the application */

/* 2D matrices for Jacobi iteration, each one is used alternately as input
 * and result. They are global as need to be freed on any application exit.
 */
static value_type *u[DIMENSIONS] = { NULL, NULL};

/* 2D matrices like u but used for verification purposes */
static value_type *v[DIMENSIONS] = { NULL, NULL};


/**************************************************************************
 Function: exit_app

 params:
    rc        return code for application
 **************************************************************************/
static void exit_app(int rc) {
    /* flush any output */
    fflush(stdout);

    /* free matrices */
    if (u[OLD]) {
        free(u[OLD]);
        u[OLD] = NULL;
    }
    if (u[NEW]) {
        free(u[NEW]);
        u[NEW] = NULL;
    }
    if (v[OLD]) {
        free(v[OLD]);
        v[OLD] = NULL;
    }
    if (v[NEW]) {
        free(v[NEW]);
        v[NEW] = NULL;
    }

    /* exit with supplied return code */
    exit(rc);
}

/**************************************************************************
 Function: convert_rgb

 Converts a floating point number in the range [0,1] to a RGB color from
 blue (0) through to red (1) using an intermediate hue value

 params:
    v            value to convert (should be in the range [0,1]
 returns:
    r            byte value for red
    g            byte value for green
    b            byte value for blue
 **************************************************************************/
static void convert_RGB(value_type v, unsigned char *r,
                        unsigned char *g, unsigned char *b) {
    value_type hue;
    unsigned int segment;

    /*  Use HSB (aka HSV) cylindrical color model and then convert to RGB. The
        calculation assumes a saturation of 1.0 and uses BRIGHTNESS_DEFAULT for
        brightness. The hue ranges from red (1.0) to blue (0.0) to avoid both
        0.0 and 1.0 being the same color */
    hue = 4.0f * (1- v);

    /* Use 0 through 4 of the 6 segments in the hue circle */
    segment = (int)floor(hue);

    switch (segment) {
        /* color in 0 to 60 degrees range (first sixth) */
        case 0:
            *r = BRIGHTNESS_DEFAULT;
            *g = CALC_H2(hue, segment);
            *b = 0;
            break;

        /* color in 60 to 120 degrees (second sixth) */
        case 1:
            *r = CALC_H1(hue, segment);
            *g = BRIGHTNESS_DEFAULT;
            *b = 0;
            break;

        /* color in 120 to 180 degrees (third sixth) */
        case 2:
            *r = 0;
            *g = BRIGHTNESS_DEFAULT;
            *b = CALC_H2(hue, segment);
            break;

        /* color in 180 to 240 degrees (fourth sixth) */
        case 3:
            *r = 0;
            *g = CALC_H1(hue, segment);
            *b = BRIGHTNESS_DEFAULT;
            break;

        /* color in 240 to 300 degrees (fifth sixth) */
        case 4:
            *r = CALC_H2(hue, segment);
            *g = 0;
            *b = BRIGHTNESS_DEFAULT;
            break;

        /* should not get here as hue is from 0 to 4 */
        default:
            *r = BRIGHTNESS_DEFAULT;
            *g = 0;
            *b = CALC_H1(hue, segment);
            break;
    }
}


/**************************************************************************
 Function: save_bitmap

 Saves a bitmap representation of the array. As a
 simplification, each MPI rank writes a separate file where the filename
 includes the cartesian coordinate of the rank so that the bitmap can be
 put together offline using a tool such as pnmcat. Another alternative
 would be to use the MPI2 I/O functions and write different parts of
 the file using MPI.

 params:
    a           array to set boundary conditions
    size        size of array 
 **************************************************************************/
static void save_bitmap(value_type *a,
                        size_t size[DIMENSIONS]) {

    int i, j, width, height, bitmap_size;
    char bitmap_filename[128];
    FILE *fh;
    int err;

    /* buffer to hold bitmap pixels before being written to file */
    unsigned char *pixels;

    /* do not include ghost nodes or boundary nodes */
    width = size[X];
    height = size[Y];

    /* allocate space for display pixels */
    bitmap_size = 3 * width * height;
    pixels = (unsigned char *)malloc(bitmap_size);

    /* convert values to RGB pixels */
    for (i=GHOST_CELL_WIDTH; i<width+GHOST_CELL_WIDTH; ++i) {
        for (j=GHOST_CELL_WIDTH; j<height+GHOST_CELL_WIDTH; ++j) {
            /* change to bitmap origin at lower left corner */
            int p = 3 * ((height - j) * width + (i-1));
            convert_RGB(a[i*(height+2)+j], &pixels[p], &pixels[p+1], &pixels[p+2]);
        }
    }

    /* generate file name */
    sprintf(bitmap_filename, "jac%dx%d.ppm", width, height);

    /* try to open the file */
    fh = fopen(bitmap_filename, "wb");
    if (fh == NULL) {
        free(pixels);
        QUIT("Open of bitmap file %s failed errno=%d\n", bitmap_filename, errno);
    }

    /* output PPM type P6 file header */
    fprintf(fh, "P6\n%d %d\n255\n", (unsigned int)size[X], (unsigned int)size[Y]);

    /* write pixels into the file */
    err = fwrite(pixels, 1, bitmap_size, fh);
    if (err != (int) bitmap_size) {
        fclose(fh);
        free(pixels);
        QUIT("Writing of bitmap pixels sized %d failed err=%d errno=%d\n",
             bitmap_size, err, errno);
    }

    /* close the file */
    err = fclose(fh);
    if (err != 0) {
        QUIT("Closing of bitmap file failed err=%d errno=%d\n", err, errno);
    }

    /* output completion message and cleanup */
    printf("Bitmap of generated data saved in %s\n", bitmap_filename);
    free(pixels);
}


/**************************************************************************
 Function: print_array

 Prints the contents of the given array .
 Note that ghost cells are repeated.

 params:
    legend      text data to describe the array
    a           array to set boundary conditions
    size        size of array
    d           discretion size
 **************************************************************************/
static void print_array(const char *legend,
                        value_type *a,
                        size_t size[DIMENSIONS],
                        value_type d[DIMENSIONS]) {

    unsigned int i, j, ystride;
    int r;

    /* convenience for y stride in array */
    ystride = size[Y] + 2*GHOST_CELL_WIDTH;

    /* return if array is too large */
    if ((MAX_PRINT_SIZE < size[X]+2*GHOST_CELL_WIDTH) || (MAX_PRINT_SIZE < ystride)) {
        return;
    }

    /* print title for array contents */
    printf("%s -------------------------\n", legend);
    fflush(stdout);

    /* print array */
	for (i=0; i<size[X]+2*GHOST_CELL_WIDTH; ++i) {
		printf("%0.3lf    ", i * d[X]);
	}
	printf("\n");

	/* print y prefix and x values */
	for (j=0; j<size[Y]+2*GHOST_CELL_WIDTH; ++j) {
		printf("y=%0.3lf ", j * d[Y]);
		for (i=0; i<size[X]+2*GHOST_CELL_WIDTH; ++i) {
			printf("%8.5lf ", a[i * ystride + j]);
		}
		printf("\n");
	}

	/* make sure output is complete */
	fflush(stdout);
}


/**************************************************************************
 Function: exact_compute

 Calculates the exact contents of the array using the analytical solution:
    u(x,y) = sin(pi * x) * exp(-pi * y)analyitcal solution

 params:
    a           array to compute solution into
    size        size of array for this MPI rank
    d           discretion size
 **************************************************************************/
static void exact_compute(value_type *a,
                        size_t size[DIMENSIONS],
                        value_type d[DIMENSIONS]) {

    unsigned int i, j, ystride;

    /* convenience for y stride in array */
    ystride = size[Y]+2*GHOST_CELL_WIDTH;

    /* loop over array including the boundaries and compute the result */
    for (i=0; i<size[X]+2*GHOST_CELL_WIDTH; ++i) {
        for (j=0; j<size[Y]+2*GHOST_CELL_WIDTH; ++j) {
            a[i * ystride + j] = (value_type) (sin(PI*(i*d[X])) *
                                               exp(-PI*(j*d[Y])));
        }
    }
}

/**************************************************************************
 Function: ocl_jacobi_reduce

 Calculates maximum error over array of differences returned from the device

 params:
    delta               delta differences between two iterations
    delta_buf           OpenCL buffer for deltas
    delta_size          size of delta buffer over dimensions
    delta_buffer_size   size of delta buffer in bytes
    queue               OpenCL command queue for kernel
    kernel_execution    OpenCL kernel event

 returns:
     maximum difference for this iteration
 **************************************************************************/
static value_type ocl_jacobi_reduce(value_type *delta,
                                    size_t delta_size[DIMENSIONS]) {

    unsigned int i, j;
    value_type max_diff;

    /* find final delta difference on previous iteration */
    max_diff = 0.0;
    for (i=0; i<delta_size[X]; ++i) {
        for (j=0; j<delta_size[Y]; ++j) {
            max_diff = fmax(max_diff, delta[i * delta_size[Y] + j]);
        }
    }

    /* return the maximum difference from last iteration */
    return max_diff;
}

/**************************************************************************
 Function: ocl_jacobi

  This routine contains the main iteration loop for the Jacobi iteration
  using OpenCL kernel.

 params:
    a                       two arrays to compute solution into
    max_iter                maximum number of iterations
    size                    size of array for this MPI rank
    tolerance               all differences should be les than this tolerance value
    mpi_ranks               number of MPI ranks in each dimension
    rank_pos                cartesian position of this rank
    origin                  origin for this rank
    d                       discretion size
    mpi_comm                MPI communications structure
    local_workblock_size    size of local workblock for OpenCL kernel
    device_type             OpenCL device type
    full_copy               boolean if full buffer copy is to be done
 **************************************************************************/
static void ocl_jacobi(value_type *a[2],
                        unsigned int max_iter,
                        size_t size[DIMENSIONS],
                        value_type tolerance,
                        value_type d[DIMENSIONS],
                        size_t local_workblock_size[DIMENSIONS],
                        cl_device_type device_type,
                        unsigned int full_copy) {

    size_t array_size;
    unsigned int i, j, rc, iter = 0;
    size_t delta_buffer_size, delta_size[DIMENSIONS];
    size_t tile_delta_size, tile_cache_size;
    value_type max_diff, timer;
    icl_device* device_id;
    icl_kernel* kernel;
    cl_int err;
    icl_buffer *a_buf[2], *delta_buf;
    value_type *delta;
 
    /* convenience for y stride in array */
    cl_uint ystride = size[Y]+2*GHOST_CELL_WIDTH;
    
    /* init devices */
    icl_init_devices(device_type);
    
    /* find OpenCL device */
    device_id  = icl_get_device(0);


    /* build the kernel and verify the kernel */
    kernel = icl_create_kernel(device_id, "jacsolver_kernel.cl", "ocl_jacobi_local_copy", "", ICL_SOURCE);

    /* calculate size of kernel local memory  - also used later for kernel params */
    tile_delta_size = local_workblock_size[X] * local_workblock_size[Y];
    tile_cache_size = (local_workblock_size[X]+2*GHOST_CELL_WIDTH) * (local_workblock_size[Y]+2*GHOST_CELL_WIDTH);

    /* verify the device has enough resources for this device */
/*  I'm an optimist, we just hope for the best
  	if ((cluGetAvailableLocalMem(device_id, kernel) < tile_delta_size + tile_cache_size) ||
        (! cluCheckLocalWorkgroupSize(device_id, kernel, DIMENSIONS, local_workblock_size))) {
        local_workblock_size[X] = 1;
        local_workblock_size[Y] = 1;
    }
*/
    printf("Estimating solution using OpenCL Jacobi iteration with %d x %d workblock.\n", (int)local_workblock_size[X], (int)local_workblock_size[Y]);
    fflush(stdout);

    /* init arrays by setting the initial value and the boundary conditions */
    set_initial_solution(a[OLD], size, INITIAL_GUESS);
    set_initial_solution(a[NEW], size, INITIAL_GUESS);
    set_boundary_conditions(a[OLD], size, d);
    set_boundary_conditions(a[NEW], size, d);

    /* print the initial solution guess */ 
    print_array("Init ", a[NEW], size, d);

    /* allocate memory for differences */
    delta_size[X] = size[X] / local_workblock_size[X];
    delta_size[Y] = size[Y] / local_workblock_size[Y];
    delta_buffer_size = delta_size[X] * delta_size[Y];
    delta = (value_type *)malloc(sizeof(value_type) * delta_buffer_size);
    
    /* initialize deltas so that first execution of kernel with overlapping 
     * reduction on the host will work correctly and not prematurely exit
     */
    for (i=0; i<delta_size[X]; ++i) {
        for (j=0; j<delta_size[Y]; ++j) {
            delta[i * delta_size[Y] + j] = 1.0;
        }
    }

    /* create buffers for OpenCL device using host memory */
    array_size = (size[X]+2*GHOST_CELL_WIDTH) * ystride;
    a_buf[OLD] = icl_create_buffer(device_id, CL_MEM_READ_WRITE | CL_MEM_ALLOC_HOST_PTR, sizeof(value_type) * array_size);
    a_buf[NEW] = icl_create_buffer(device_id, CL_MEM_READ_WRITE | CL_MEM_ALLOC_HOST_PTR, sizeof(value_type) * array_size);
    delta_buf = icl_create_buffer(device_id, CL_MEM_READ_WRITE | CL_MEM_ALLOC_HOST_PTR, sizeof(value_type) * delta_buffer_size);

    /* copy over buffers to device */
    icl_write_buffer(a_buf[OLD], CL_TRUE, sizeof(value_type) * array_size, a[OLD], NULL, NULL);
    icl_write_buffer(a_buf[NEW], CL_TRUE, sizeof(value_type) * array_size, a[NEW], NULL, NULL);

    /* set the kernel execution type  - data parallel */
 //   cluSetKernelNDRange(clu, kernel, DIMENSIONS, NULL, size, local_workblock_size);

    /*  iterate until maximum difference is less than the given tolerance
        or number of iterations is too high */
    do {
        /* swap array pointers for next iteration */
        SWAP_PTR(a[OLD], a[NEW]);
        SWAP_BUF(a_buf[OLD], a_buf[NEW]);

        icl_run_kernel(kernel, DIMENSIONS, size, local_workblock_size, NULL, NULL, 6,
                    (size_t)0,(void *) &a_buf[OLD],
                    (size_t)0, (void *) &a_buf[NEW],
                    sizeof(value_type) * tile_delta_size, NULL,
                    sizeof(value_type) * tile_cache_size, NULL,
                    (size_t)0, (void *) &delta_buf,
                    sizeof(cl_uint), (void *) &ystride);

        /* while the kernel is running, calculate the reduction for the previous iteration */
        max_diff = ocl_jacobi_reduce(delta, delta_size);
        
        /* enqueue a synchronous copy of the delta. This will not occur until the kernel 
         * has finished. The deltas for each workgroup is a much smaller array to process
         */
        icl_read_buffer(a_buf[NEW], CL_TRUE, sizeof(value_type) * array_size, a[NEW], NULL, NULL);
//        clEnqueueReadBuffer(queue, a_buf[NEW], CL_TRUE,    0, sizeof(value_type) * array_size, a[NEW], 0, NULL, NULL));

        /* output status for user, overwrite the same line */
        if ((0 == iter % 100)) {
            printf("Iteration=%5d, max difference=%0.7f, target=%0.7f\r",
                        iter, max_diff, tolerance);
            fflush(stdout);
        }

        
        /* increment the iteration counter */
        iter++;
    } while (max_diff > tolerance && max_iter >= iter); /* do loop */

    /* read back the final result */
    icl_read_buffer(a_buf[NEW], CL_TRUE, sizeof(value_type) * array_size, a[NEW], NULL, NULL);

    /* output final iteration count and maximum difference value */
    printf("Iteration=%5d, max difference=%0.7f, execution time=%.3f seconds\n", iter-1, max_diff, timer);
    fflush(stdout);

    /* finish usage of OpenCL device */
    icl_release_buffers(3, a_buf[OLD], a_buf[NEW], delta_buf);
    icl_release_kernel(kernel);
    free(delta);
}

/**************************************************************************
 Function: reference_jacobi_kernel

 Computes single Jacobi iteration using host code (no OpenCL) - 5 point stencil

 params:
    a            input array
    size        size of array for this MPI rank
    anew        output array
 returns:
    diff        maximum difference from last iteration
 **************************************************************************/
static value_type reference_jacobi_kernel(value_type *a, value_type *anew,
                                          size_t size[DIMENSIONS]) {

    unsigned int i, j, ystride;
    value_type new_val, max_diff = 0.0;

    /* convenience for y stride in array */
    ystride = size[Y]+2*GHOST_CELL_WIDTH;

    for (i=GHOST_CELL_WIDTH; i<size[X]+GHOST_CELL_WIDTH; ++i) {
        for (j=GHOST_CELL_WIDTH; j<size[Y]+GHOST_CELL_WIDTH; ++j) {
            new_val = (value_type) (0.25 * (a[(i-1) * ystride + j] +
                                            a[(i+1) * ystride + j] +
                                            a[i * ystride + j-1] +
                                            a[i * ystride + j+1]));
            max_diff = (value_type)fmax(max_diff, fabs(new_val - a[i * ystride + j]));
            anew[i * ystride + j] = new_val;
        }
    }

    /* return the maximum difference from last iteration */
    return max_diff;
}


/**************************************************************************
 Function: reference_jacobi

 This routine contains the main iteration loop for the Jacobi iteration
 reference implementation (no OpenCL).

 params:
    a           two arrays to compute solution into
    max_iter    maximum number of iterations   
    size        size of array for this MPI rank
    tolerance   all differences should be les than this tolerance value
    mpi_ranks   number of MPI ranks in each dimension
    rank_pos    cartesian position of this rank    
    origin      origin for this rank
    d           discretion size
    mpi_comm    MPI communications structure
 **************************************************************************/
static void reference_jacobi(value_type *a[2],
                            unsigned int max_iter,
                            size_t size[DIMENSIONS],
                            value_type tolerance,
                            value_type d[DIMENSIONS]) {

    unsigned int rc, iter = 0;
    value_type max_diff, timer;
    struct timeb start_time, stop_time;

    /* init arrays by setting the initial value and the boundary conditions */
    set_initial_solution(a[OLD], size, INITIAL_GUESS);
    set_initial_solution(a[NEW], size, INITIAL_GUESS);
    set_boundary_conditions(a[OLD], size, d);
    set_boundary_conditions(a[NEW], size, d);

    /* print the initial solution guess */
    print_array("Init ", a[NEW], size, d);

    /* get start time */
    ftime(&start_time);

    /*  iterate until maximum difference is less than the given tolerance
        or number of iterations is too high
     */
    do {
        /* swap array pointers for next iteration */
        SWAP_PTR(a[OLD], a[NEW]);

        /* iterate using a[OLD] as the input and a[NEW] as the output */
        max_diff = reference_jacobi_kernel(a[OLD], a[NEW], size);

        /* output status for user, overwrite the same line */
        if (0 == iter % 100) {
            printf("Iteration=%5d, max difference=%0.7f, target=%0.7f\r",
                iter, max_diff, tolerance);
            fflush(stdout);
        }

        /* increment counter */
        iter++;
    } while (max_diff > tolerance && max_iter > iter); /* do loop */

    /* output final iteration count and maximum difference value */
    printf("Iteration=%5d, max difference=%0.7f, execution time=%.3f seconds\n",
                    iter, max_diff, timer);

}



/**************************************************************************
 Function: set_boundary_conditions

 Sets the following boundary conditions in the given array
    u(x,0) = sin(pi * x)
    u(x,1) = sin(pi * x) * pow(e, -pi)
    u(0,y) = 0
    u(1,y) = 0

 params:
    a            array to set boundary conditions
    size        size of array for this MPI rank
    rank_pos    cartesian position of this rank
    origin         origin for this rank
    d            discretion size
 **************************************************************************/
static void set_boundary_conditions(value_type *a,
                                    size_t size[DIMENSIONS],
                                    value_type d[DIMENSIONS]) {
    unsigned int i, j, ystride;

    /* convenience for y stride in array */
    ystride = size[Y]+2*GHOST_CELL_WIDTH;

    /* Set condition if we are part of the bottom edge (y = 0.0) */
	for (i=GHOST_CELL_WIDTH; i<size[X]+GHOST_CELL_WIDTH; ++i) {        /* exclude corners */
		a[i * ystride] =
			(value_type)sin(PI * (i * d[X]));
	}

    /* Set condition if we are part of the top edge (y = 1.0) */
	for (i=GHOST_CELL_WIDTH; i<size[X]+GHOST_CELL_WIDTH; ++i) {        /* exclude corners */
		a[i * ystride + size[Y]+GHOST_CELL_WIDTH] =
			(value_type)(sin(PI * (i * d[X])) * exp(-PI));
	}

    /* Set condition if we are part of the left edge (x = 0.0) */
	for (j=0; j<ystride; ++j) {            /* include corners */
		a[j] = 0;
	}

    /* Set condition if we are part of the right edge (x = 1.0) */
	for (j=0; j<ystride; ++j) {            /* include corners */
		a[(size[X]+GHOST_CELL_WIDTH) * ystride + j] = 0;
	}
}


/**************************************************************************
 Function: set_initial_solution

 Sets the initial solution in the given array

 params:
    a            array to set boundary conditions
    size        size of array for this MPI rank
    guess        initial value
 **************************************************************************/
static void set_initial_solution(value_type *a,
                                 size_t size[DIMENSIONS],
                                 value_type guess) {

    unsigned int i, j, ystride;

    /* convenience for y stride in array */
    ystride = size[Y]+2*GHOST_CELL_WIDTH;

    /* set guess */
    for (i=0; i<size[X]+2*GHOST_CELL_WIDTH; ++i) {
        for (j=0; j<ystride; ++j) {
            a[i * ystride + j] = guess;
        }
    }
}


/**************************************************************************
 Function: main

 This is the main control flow for the example. After reading and verifying
 the command line arguments, MPI is initialized. Depending on which option
 is chosen one of 3 computations are called for analytical, Jacobi iteration
 using OpenCL, or Jacabo iteration using host and no OpenCL.

 After the computation is complete, a bitmap representation of the final
 array is stored in a PPM format file.

 **************************************************************************/
int main(int argc, char *argv[]) {

    /* size of matrices for iteration - input parameter */
    size_t mat_size[DIMENSIONS] = { DEFAULT_NODES, DEFAULT_NODES };

    /* size of OpenCL workblock in each dimension - input parameter */
    size_t block_size[DIMENSIONS] = { DEFAULT_BLOCK, DEFAULT_BLOCK };

    /* my cartesian position in MPI space */
    int my_position[DIMENSIONS] = { 0, 0 };

    /* size of matrix for each rank */
    size_t rank_size[DIMENSIONS] = {100, 100};

    /* size of the array needed */
    size_t array_size = 100;

    /* discretion size in each dimension assuming a unit square */
    value_type d[DIMENSIONS];

    /* maximum number of iterations */
    unsigned int max_iter = MAX_ITERATIONS;

    /* boolean for full copy of buffer or just ghost cells */
    unsigned int full_copy = 0;

    /* boolean for verificaton against reference implementation */
    unsigned int verify = 0;

    /* OpenCL device type - input parameter can override this */
    cl_device_type dev_type = CL_DEVICE_TYPE_GPU;

    /* print welcome message */
    printf("A simple %dD iterative Jacobi solver", DIMENSIONS);

    /* calculate size of each node in the unit square plate */
    d[X] = (value_type)(1.0 / (mat_size[X]+1));
    d[Y] = (value_type)(1.0 / (mat_size[Y]+1));

    /* flush output before starting the compute */
    fflush(stdout);

    /* calculate size of array and allocate the memory */
    /* size is 2 bigger to account for "ghost cells" and/or boundaries */
    /* exit_app takes care of cleaning up u */
    array_size = sizeof(value_type) * (rank_size[X]+2*GHOST_CELL_WIDTH) *
                                      (rank_size[Y]+2*GHOST_CELL_WIDTH);
    u[OLD] = (value_type *)malloc(array_size);
    u[NEW] = (value_type *)malloc(array_size);

    /*  At this point the main computation can occur. For the purposes of this example
     *  we are using a simple Jacobi iteration technique. This converges very slowly. 
     */

    /* compute the exact solution, result in u[NEW] */
    if (EXACT_CALC == (signed int)dev_type) {
        printf("Calculating solution using analytical formula.\n");
        exact_compute(u[NEW], rank_size, d);
    } else if (REFERENCE_CALC == (signed int)dev_type) {
        /* compute solution using reference implementation, result in u[NEW] */
        printf("Estimating solution using Jacobi reference implementation.\n");
        reference_jacobi(u, max_iter, rank_size, DEFAULT_TOLERANCE, d);

    /* compute solution using OpenCL kernel, result in u[NEW] */
    } else {
        ocl_jacobi(u, max_iter, rank_size, DEFAULT_TOLERANCE, d, block_size, dev_type, full_copy);
    }

    /* print solution */
    print_array("Solve", u[NEW], rank_size, d);

    /* perform verification if required */
    if (verify) {
        unsigned int i, j, ystride;
        value_type max_diff = 0.0;

        /* allocate for verification matrices */
        v[OLD] = (value_type *)malloc(array_size);
        v[NEW] = (value_type *)malloc(array_size);

        /* compute verfication solution, result in v[NEW] */
        printf("Starting verification using Jacobi reference implementation.\n");
        reference_jacobi(v, max_iter, rank_size, DEFAULT_TOLERANCE, d);

        /* compute array differences and store in v[OLD] */
        for (i=0; i<rank_size[X]+2*GHOST_CELL_WIDTH; ++i) {
            /* convenience for y stride in array */
            ystride = rank_size[Y]+2*GHOST_CELL_WIDTH;

            for (j=0; j<ystride; ++j) {
                /* calculate difference and update maximum difference */
                v[OLD][i * ystride + j] = v[NEW][i * ystride + j] - u[NEW][i * ystride + j];
                max_diff = (value_type)fmax(max_diff, fabs(v[OLD][i * ystride + j]));
            }
        }

        /* output differences */
        print_array("Verify Diff", v[OLD], rank_size, d);
        printf("Verification complete, max difference with reference implemention is %0.5f.\n", max_diff);

        /* return error code if max difference is too large */
        if (max_diff > 0.0001) {
            QUIT("Error: max difference greater than required tolerance.\n");
        }
    }

    /* save bitmap of computation */
    save_bitmap(u[NEW], rank_size);

    /* deallocate arrays and finish use of MPI */
    exit_app(0);
    return 0;
}
