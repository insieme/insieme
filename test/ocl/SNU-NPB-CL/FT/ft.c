//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB FT code for multiple    //
//  devices. This OpenCL version is developed by the Center for Manycore   //
//  Programming at Seoul National University and derived from the MPI      //
//  Fortran versions in "NPB3.3-MPI" developed by NAS.                     //
//                                                                         //
//  Permission to use, copy, distribute and modify this software for any   //
//  purpose with or without fee is hereby granted. This software is        //
//  provided "as is" without express or implied warranty.                  //
//                                                                         //
//  Information on NPB 3.3, including the technical report, the original   //
//  specifications, source code, results and information on how to submit  //
//  new results, is available at:                                          //
//                                                                         //
//           http://www.nas.nasa.gov/Software/NPB/                         //
//                                                                         //
//  Send comments or suggestions for this OpenCL version to                //
//  cmp@aces.snu.ac.kr                                                     //
//                                                                         //
//          Center for Manycore Programming                                //
//          School of Computer Science and Engineering                     //
//          Seoul National University                                      //
//          Seoul 151-744, Korea                                           //
//                                                                         //
//          E-mail:  cmp@aces.snu.ac.kr                                    //
//                                                                         //
//-------------------------------------------------------------------------//

//-------------------------------------------------------------------------//
// Authors: Sangmin Seo, Jungwon Kim, Jun Lee, Gangwon Jo, Jeongho Nah,    //
//          and Jaejin Lee                                                 //
//-------------------------------------------------------------------------//

//---------------------------------------------------------------------

//---------------------------------------------------------------------
// FT benchmark
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//---------------------------------------------------------------------
//
//      program ft
//
//---------------------------------------------------------------------
//---------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "global.h"
#include "randdp.h"
#include "timers.h"
#include "print_results.h"

#include "ft_dim.h"

#include <CL/cl.h>
//#include <CL/cl_ext.h>
#include "cl_util.h"
#define TIMER_OPENCL    20
#define TIMER_BUILD     21
#define TIMER_BUFFER    22
#define TIMER_RELEASE   23

#define USE_CHECK_FINISH
#define TIMER_DETAIL
//#define USE_COPY_BUFFER

#ifdef TIMER_DETAIL
#define DTIMER_START(id)    if (timers_enabled) timer_start(id)
#define DTIMER_STOP(id)     if (timers_enabled) timer_stop(id)
#else
#define DTIMER_START(id)
#define DTIMER_STOP(id)
#endif

#ifdef USE_CHECK_FINISH
#define CHECK_FINISH()      for (i = 0; i < num_devices; i++) { \
                              ecode = clFinish(cmd_queue[i]); \
                              clu_CheckError(ecode, "clFinish()"); \
                            }
#else
#define CHECK_FINISH()
#endif


#define DEFAULT_NUM_SUBS    32


// OPENCL Variables
static cl_device_type    device_type;
static cl_device_id      p_device;
static char             *device_name;
static cl_device_id     *devices;
static cl_uint           num_devices;
static cl_context        context;
static cl_command_queue *cmd_queue;
static cl_program        program;
static size_t  work_item_sizes[3];
static size_t  max_work_group_size;
static cl_uint max_compute_units;

static cl_kernel *k_compute_indexmap;
static cl_kernel *k_compute_ics1, *k_compute_ics2;
static cl_kernel *k_cffts1;
static cl_kernel *k_cffts2;
static cl_kernel *k_cffts3;
static cl_kernel *k_evolve;
static cl_kernel *k_checksum;
static cl_kernel *k_transpose2_local1; 
static cl_kernel *k_transpose2_local2; 
static cl_kernel *k_transpose2_local3; 
static cl_kernel *k_transpose2_finish; 
static cl_kernel *k_transpose_x_z_local1; 
static cl_kernel *k_transpose_x_z_local2; 
static cl_kernel *k_transpose_x_z_finish; 
static cl_kernel *k_transpose_x_y_local; 
static cl_kernel *k_transpose_x_y_finish; 

static cl_mem *m_u;
static cl_mem *m_dims;
static cl_mem *m_xstart, *m_xend;
static cl_mem *m_ystart, *m_yend;
static cl_mem *m_zstart, *m_zend;

//---------------------------------------------------------------------
// u0, u1, u2 are the main arrays in the problem. 
// Depending on the decomposition, these arrays will have different 
// dimensions. To accomodate all possibilities, we allocate them as 
// one-dimensional arrays and pass them to subroutines for different 
// views
//  - u0 contains the initial (transformed) initial condition
//  - u1 and u2 are working arrays
//  - twiddle contains exponents for the time evolution operator. 
//---------------------------------------------------------------------
//---------------------------------------------------------------------
// Large arrays are in common so that they are allocated on the
// heap rather than the stack. This common block is not
// referenced directly anywhere else. Padding is to avoid accidental 
// cache problems, since all array sizes are powers of two.
//---------------------------------------------------------------------
static cl_mem *m_u0;
static cl_mem *m_u1;
static cl_mem *m_u2;
static cl_mem *m_twiddle;
static cl_mem *m_starts;
static cl_mem *m_ty1;
static cl_mem *m_ty2;
static cl_mem *m_chk;

static size_t checksum_lws, checksum_gws, checksum_wgn;

static int COMPUTE_IMAP_DIM, EVOLVE_DIM;
static int TRANSPOSE2_LOCAL1_DIM;
static int TRANSPOSE2_LOCAL2_DIM;
static int TRANSPOSE2_LOCAL3_DIM;
static int TRANSPOSE2_FINISH_DIM;
static int TRANSPOSE_X_Z_LOCAL1_DIM;
static int TRANSPOSE_X_Z_LOCAL2_DIM;
static int TRANSPOSE_X_Z_FINISH_DIM;
static int TRANSPOSE_X_Y_LOCAL_DIM;
static int TRANSPOSE_X_Y_FINISH_DIM;
static int CFFTS1_DIM;
static int CFFTS2_DIM;
static int CFFTS3_DIM;


//---------------------------------------------------------------------------
int NTDIVNP;

static void evolve(cl_mem *u0, cl_mem *u1, cl_mem *twiddle,
                   int d1, int d2, int d3);
static void compute_initial_conditions(cl_mem *u0, int d1, int d2, int d3);
static void setup();
static void compute_indexmap(cl_mem *twiddle, int d1, int d2, int d3);
static void print_timers();
static void fft_init(int n);
static void fft(int dir, cl_mem *x1, cl_mem *x2);
static void cffts1(int is, int d1, int d2, int d3, cl_mem *x, cl_mem *xout);
static void cffts2(int is, int d1, int d2, int d3, cl_mem *x, cl_mem *xout);
static void cffts3(int is, int d1, int d2, int d3, cl_mem *x, cl_mem *xout);
static int ilog2(int n);
static void transpose_x_yz(int l1, int l2, cl_mem *xin, cl_mem *xout);
static void transpose_xy_z(int l1, int l2, cl_mem *xin, cl_mem *xout);
static void transpose2_local(int n1, int n2, cl_mem *xin, cl_mem *xout);
static void transpose2_global(cl_mem *xin, cl_mem *xout);
static void transpose2_finish(int n1, int n2, cl_mem *xin, cl_mem *xout);
static void transpose_x_z(int l1, int l2, cl_mem *xin, cl_mem *xout);
static void transpose_x_z_local(int d1, int d2, int d3, 
                                cl_mem *xin, cl_mem *xout);
static void transpose_x_z_global(int d1, int d2, int d3,
                                 cl_mem *xin, cl_mem *xout);
static void transpose_x_z_finish(int d1, int d2, int d3,
                                 cl_mem *xin, cl_mem *xout);
static void transpose_x_y(int l1, int l2, cl_mem *xin, cl_mem *xout);
static void transpose_x_y_local(int d1, int d2, int d3,
                                cl_mem *xin, cl_mem *xout);
static void transpose_x_y_global(int d1, int d2, int d3,
                                 cl_mem *xin, cl_mem *xout);
static void transpose_x_y_finish(int d1, int d2, int d3,
                                 cl_mem *xin, cl_mem *xout);
static void checksum(int it, cl_mem *u1, int d1, int d2, int d3);
static void verify(int d1, int d2, int d3, int nt, 
                   logical *verified, char *Class);

static void setup_opencl(int argc, char *argv[]);
static void release_opencl();
//---------------------------------------------------------------------------


#ifdef CLUSTER
int __main(int argc, char** argv)
#else
int main(int argc, char *argv[]) 
#endif
{
  int i;

  int iter;
  double total_time, mflops;
  logical verified;
  char Class;

  //---------------------------------------------------------------------
  // Run the entire problem once to make sure all data is touched. 
  // This reduces variable startup costs, which is important for such a 
  // short benchmark. The other NPB 2 implementations are similar. 
  //---------------------------------------------------------------------
  for (i = 0; i < T_max; i++) {
    timer_clear(i);
  }

  timer_start(T_init);
  setup_opencl(argc, argv);
  setup();
  compute_indexmap(m_twiddle, dims[2][0], dims[2][1], dims[2][2]);
  compute_initial_conditions(m_u1, dims[0][0], dims[0][1], dims[0][2]);
  fft_init(dims[0][0]);
  fft(1, m_u1, m_u0);

  for (i = 0; i < num_devices; i++) {
    cl_int ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  timer_stop(T_init);
  printf(" Initialization time = %f\n", timer_read(T_init));

  //---------------------------------------------------------------------
  // Start over from the beginning. Note that all operations must
  // be timed, in contrast to other benchmarks. 
  //---------------------------------------------------------------------
  for (i = 0; i < T_max; i++) {
    timer_clear(i);
  }

  timer_start(T_total);
  if (timers_enabled) timer_start(T_setup);

  compute_indexmap(m_twiddle, dims[2][0], dims[2][1], dims[2][2]);
  compute_initial_conditions(m_u1, dims[0][0], dims[0][1], dims[0][2]);
  fft_init(dims[0][0]);

  if (timers_enabled) timer_stop(T_setup);

  if (timers_enabled) timer_start(T_fft);
  fft(1, m_u1, m_u0);
  if (timers_enabled) timer_stop(T_fft);

  for (iter = 1; iter <= niter; iter++) {
    if (timers_enabled) timer_start(T_evolve);
    evolve(m_u0, m_u1, m_twiddle, dims[0][0], dims[0][1], dims[0][2]);
    if (timers_enabled) timer_stop(T_evolve);

    if (timers_enabled) timer_start(T_fft);
    fft(-1, m_u1, m_u2);
    if (timers_enabled) timer_stop(T_fft);

    if (timers_enabled) timer_start(T_checksum);
    checksum(iter, m_u2, dims[0][0], dims[0][1], dims[0][2]);
    if (timers_enabled) timer_stop(T_checksum);
  }

  verify(NX, NY, NZ, niter, &verified, &Class);
  timer_stop(T_total);

  total_time = timer_read(T_total);

  if (total_time != 0.0) {
    mflops = 1.0e-6*NTOTAL_F *
            (14.8157+7.19641*log(NTOTAL_F)
            + (5.23518+7.21113*log(NTOTAL_F))*niter)
            / total_time;
  } else {
    mflops = 0.0;
  }
  c_print_results("FT", Class, NX, NY, NZ, niter,
                  total_time, mflops, "          floating point", verified, 
                  NPBVERSION, COMPILETIME, CS1, CS2, CS3, CS4, CS5, CS6, CS7,
                  clu_GetDeviceTypeName(device_type),
                  device_name, num_devices);
  if (timers_enabled) print_timers();

  //release_opencl();

  return 0;
}


//---------------------------------------------------------------------
// evolve u0 -> u1 (t time steps) in fourier space
//---------------------------------------------------------------------
static void evolve(cl_mem *u0, cl_mem *u1, cl_mem *twiddle,
                   int d1, int d2, int d3)
{
  int i;
  cl_int ecode;
  size_t evolve_lws[3], evolve_gws[3];

  if (EVOLVE_DIM == 3) {
    evolve_lws[0] = d1 < work_item_sizes[0] ? d1 : work_item_sizes[0];
    int temp = max_work_group_size / evolve_lws[0];
    evolve_lws[1] = d2 < temp ? d2 : temp;
    temp = temp / evolve_lws[1];
    evolve_lws[2] = d3 < temp ? d3 : temp;

    evolve_gws[0] = clu_RoundWorkSize((size_t)d1, evolve_lws[0]);
    evolve_gws[1] = clu_RoundWorkSize((size_t)d2, evolve_lws[1]);
    evolve_gws[2] = clu_RoundWorkSize((size_t)d3, evolve_lws[2]);
  } else if (EVOLVE_DIM == 2) {
    evolve_lws[0] = d2 < work_item_sizes[0] ? d2 : work_item_sizes[0];
    int temp = max_work_group_size / evolve_lws[0];
    evolve_lws[1] = d3 < temp ? d3 : temp;

    evolve_gws[0] = clu_RoundWorkSize((size_t)d2, evolve_lws[0]);
    evolve_gws[1] = clu_RoundWorkSize((size_t)d3, evolve_lws[1]);
  } else {
    //int temp = d3 / max_compute_units;
    int temp = 1;
    evolve_lws[0] = temp == 0 ? 1 : temp;
    evolve_gws[0] = clu_RoundWorkSize((size_t)d3, evolve_lws[0]);
  }

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_evolve[i], 0, sizeof(cl_mem), &u0[i]);
    ecode |= clSetKernelArg(k_evolve[i], 1, sizeof(cl_mem), &u1[i]);
    ecode |= clSetKernelArg(k_evolve[i], 2, sizeof(cl_mem), &twiddle[i]);
    ecode |= clSetKernelArg(k_evolve[i], 3, sizeof(int), &d1);
    ecode |= clSetKernelArg(k_evolve[i], 4, sizeof(int), &d2);
    ecode |= clSetKernelArg(k_evolve[i], 5, sizeof(int), &d3);
    clu_CheckError(ecode, "clSetKernelArg() for evolve");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_evolve[i],
                                   EVOLVE_DIM, NULL,
                                   evolve_gws,
                                   evolve_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for evolve");
  }

  CHECK_FINISH();
}


//---------------------------------------------------------------------
// Fill in array u0 with initial conditions from 
// random number generator 
//---------------------------------------------------------------------
static void compute_initial_conditions(cl_mem *u0, int d1, int d2, int d3)
{
  int i;
  size_t cics2_lws, cics2_gws;
  cl_int ecode;

  cics2_lws = 1;
  cics2_gws = clu_RoundWorkSize((size_t)d3, cics2_lws);

  for (i = 0; i < num_devices; i++) {
    //---------------------------------------------------------------------
    ecode  = clSetKernelArg(k_compute_ics1[i], 0, sizeof(cl_mem),
                                                  &m_starts[i]);
    ecode |= clSetKernelArg(k_compute_ics1[i], 1, sizeof(cl_mem), &m_dims[i]);
    ecode |= clSetKernelArg(k_compute_ics1[i], 2, sizeof(cl_mem),
                                                  &m_ystart[i]);
    ecode |= clSetKernelArg(k_compute_ics1[i], 3, sizeof(cl_mem),
                                                  &m_zstart[i]);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueTask(cmd_queue[i],
                          k_compute_ics1[i],
                          0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueTask()");
    //---------------------------------------------------------------------

    //---------------------------------------------------------------------
    ecode  = clSetKernelArg(k_compute_ics2[i], 0, sizeof(cl_mem), &u0[i]);
    ecode |= clSetKernelArg(k_compute_ics2[i], 1, sizeof(cl_mem), 
                                                  &m_starts[i]);
    ecode |= clSetKernelArg(k_compute_ics2[i], 2, sizeof(cl_mem), &m_dims[i]);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_compute_ics2[i],
                                   1, NULL,
                                   &cics2_gws,
                                   &cics2_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
    //---------------------------------------------------------------------
  }

  CHECK_FINISH();
}


static void setup()
{
  int i;
  FILE *fp;
  cl_int ecode;
  debug = false;

  np = num_devices;
  NTDIVNP = (((NX*NY)/np)*NZ);

  printf("\n\n NAS Parallel Benchmarks (NPB3.3-OCL-MD) - FT Benchmark\n\n");

  timers_enabled = false;
  if ((fp = fopen("timer.flag", "r")) != NULL) {
    timers_enabled = true;
    fclose(fp);
  }

  if ((fp = fopen("inputft.data", "r")) != NULL) {
    int result;
    printf(" Reading from input file inputft.data\n");
    result = fscanf(fp, "%d", &niter);
    while (fgetc(fp) != '\n');
    result = fscanf(fp, "%d", &layout_type);
    while (fgetc(fp) != '\n');
    result = fscanf(fp, "%d%d", &np1, &np2);

    //---------------------------------------------------------------------
    // check to make sure input data is consistent
    //---------------------------------------------------------------------

    //---------------------------------------------------------------------
    // 1. product of processor grid dims must equal number of processors
    //---------------------------------------------------------------------

    if (np1 * np2 != np) {
      printf(" np1 and np2 given in input file are not valid.\n");
      printf(" Product is %5d and should be %5d\n", np1*np2, np);
      exit(EXIT_FAILURE);
    }

    //---------------------------------------------------------------------
    // 2. layout type must be valid
    //---------------------------------------------------------------------

    if (layout_type != layout_0D &&
        layout_type != layout_1D &&
        layout_type != layout_2D) {
      printf(" Layout type specified in inputft.data is invalid \n");
      exit(EXIT_FAILURE);
    }

    //---------------------------------------------------------------------
    // 3. 0D layout must be 1x1 grid
    //---------------------------------------------------------------------

    if (layout_type == layout_0D && (np1 !=1 || np2 != 1)) {
      printf(" For 0D layout, both np1 and np2 must be 1 \n");
      exit(EXIT_FAILURE);
    }

    //---------------------------------------------------------------------
    // 4. 1D layout must be 1xN grid
    //---------------------------------------------------------------------

    if (layout_type == layout_1D && np1 != 1) {
      printf(" For 1D layout, np1 must be 1 \n");
      exit(EXIT_FAILURE);
    }

  } else {
    printf(" No input file inputft.data. Using compiled defaults\n");
    niter = NITER_DEFAULT;
    if (np == 1) {
      np1 = 1;
      np2 = 1;
      layout_type = layout_0D;
    } else if (np <= NZ) {
      np1 = 1;
      np2 = np;
      layout_type = layout_1D;
    } else {
      np1 = NZ;
      np2 = np/NZ;
      layout_type = layout_2D;
    }
  }

  printf(" Size                : %4dx%4dx%4d\n", NX, NY, NZ);
  printf(" Iterations          :        %7d\n", niter);
  printf(" Number of devices   :        %7d\n", np);
  printf(" Device array        :      %4dx%4d\n", np1, np2);

  if (layout_type == layout_0D) {
    printf(" Layout type         :             0D\n");
  } else if (layout_type == layout_1D) {
    printf(" Layout type         :             1D\n");
  } else {
    printf(" Layout type         :             2D\n");
  }

  if (np1 == 1 && np2 == 1) {
    layout_type = layout_0D;
  } else if (np1 == 1) {
    layout_type = layout_1D;
  } else {
    layout_type = layout_2D;
  }

  if (layout_type == layout_0D) {
    for (i = 0; i < 3; i++) {
      dims[i][0] = NX;
      dims[i][1] = NY;
      dims[i][2] = NZ;
    }
  } else if (layout_type == layout_1D) {
    dims[0][0] = NX;
    dims[0][1] = NY;
    dims[0][2] = NZ;

    dims[1][0] = NX;
    dims[1][1] = NY;
    dims[1][2] = NZ;

    dims[2][0] = NZ;
    dims[2][1] = NX;
    dims[2][2] = NY;
  } else if (layout_type == layout_2D) {
    dims[0][0] = NX;
    dims[0][1] = NY;
    dims[0][2] = NZ;

    dims[1][0] = NY;
    dims[1][1] = NX;
    dims[1][2] = NZ;

    dims[2][0] = NZ;
    dims[2][1] = NX;
    dims[2][2] = NY;
  }
  for (i = 0; i < 3; i++) {
    dims[i][1] = dims[i][1] / np1;
    dims[i][2] = dims[i][2] / np2;
  }

  //---------------------------------------------------------------------
  // Determine device coordinates
  // Device grid is np1xnp2. 
  // Arrays are always [n3/np2][n2/np1][n1]
  // Device coords are zero-based. 
  //---------------------------------------------------------------------
  me1 = (int *)malloc(sizeof(int) * num_devices);
  me2 = (int *)malloc(sizeof(int) * num_devices);

  //---------------------------------------------------------------------
  // Communicators for rows/columns of processor grid. 
  // commslice1 is communicator of all procs with same me1, ranked as me2
  // commslice2 is communicator of all procs with same me2, ranked as me1
  // mpi_comm_split(comm, color, key, ...)
  //---------------------------------------------------------------------
  commslice1 = (int **)malloc(sizeof(int *) * np1);
  for (i = 0; i < np1; i++) {
    commslice1[i] = (int *)malloc(sizeof(int) * np2);
  }

  commslice2 = (int **)malloc(sizeof(int *) * np2); 
  for (i = 0; i < np2; i++) {
    commslice2[i] = (int *)malloc(sizeof(int) * np1);
  }

  for (i = 0; i < num_devices; i++) {
    me2[i] = i % np2;     // goes from 0...np2-1 : column
    me1[i] = i / np2;     // goes from 0...np1-1 : row

    commslice1[me1[i]][me2[i]] = i;
    commslice2[me2[i]][me1[i]] = i;
  }

  //---------------------------------------------------------------------
  // Determine which section of the grid is owned by this device.
  //---------------------------------------------------------------------
  xstart = (int (*)[3])malloc(sizeof(int)*3 * num_devices);
  xend   = (int (*)[3])malloc(sizeof(int)*3 * num_devices);
  ystart = (int (*)[3])malloc(sizeof(int)*3 * num_devices);
  yend   = (int (*)[3])malloc(sizeof(int)*3 * num_devices);
  zstart = (int (*)[3])malloc(sizeof(int)*3 * num_devices);
  zend   = (int (*)[3])malloc(sizeof(int)*3 * num_devices);

  if (layout_type == layout_0D) {

    for (i = 0; i < 3; i ++) {
      xstart[0][i] = 0;
      xend[0][i]   = NX;
      ystart[0][i] = 0;
      yend[0][i]   = NY;
      zstart[0][i] = 0;
      zend[0][i]   = NZ;
    }

  } else if (layout_type == layout_1D) {

    for (i = 0; i < num_devices; i++) {
      xstart[i][0] = 0;
      xend[i][0]   = NX;
      ystart[i][0] = 0;
      yend[i][0]   = NY;
      zstart[i][0] = me2[i] * NZ/np2;
      zend[i][0]   = (me2[i]+1) * NZ/np2;

      xstart[i][1] = 0;
      xend[i][1]   = NX;
      ystart[i][1] = 0;
      yend[i][1]   = NY;
      zstart[i][1] = me2[i] * NZ/np2;
      zend[i][1]   = (me2[i]+1) * NZ/np2;

      xstart[i][2] = 0;
      xend[i][2]   = NX;
      ystart[i][2] = me2[i] * NY/np2;
      yend[i][2]   = (me2[i]+1) * NY/np2;
      zstart[i][2] = 0;
      zend[i][2]   = NZ;
    }

  } else if (layout_type == layout_2D) {

    for (i = 0; i < num_devices; i++) {
      xstart[i][0] = 0;
      xend[i][0]   = NX;
      ystart[i][0] = me1[i] * NY/np1;
      yend[i][0]   = (me1[i]+1) * NY/np1;
      zstart[i][0] = me2[i] * NZ/np2;
      zend[i][0]   = (me2[i]+1) * NZ/np2;

      xstart[i][1] = me1[i] * NX/np1;
      xend[i][1]   = (me1[i]+1)*NX/np1;
      ystart[i][1] = 0;
      yend[i][1]   = NY;
      zstart[i][1] = zstart[i][0];
      zend[i][1]   = zend[i][0];

      xstart[i][2] = xstart[i][1];
      xend[i][2]   = xend[i][1];
      ystart[i][2] = me2[i] *NY/np2;
      yend[i][2]   = (me2[i]+1)*NY/np2;
      zstart[i][2] = 0;
      zend[i][2]   = NZ;
    }
  }

  //---------------------------------------------------------------------
  // Set up info for blocking of ffts and transposes.  This improves
  // performance on cache-based systems. Blocking involves
  // working on a chunk of the problem at a time, taking chunks
  // along the first, second, or third dimension. 
  //
  // - In cffts1 blocking is on 2nd dimension (with fft on 1st dim)
  // - In cffts2/3 blocking is on 1st dimension (with fft on 2nd and 3rd dims)

  // Since 1st dim is always in processor, we'll assume it's long enough 
  // (default blocking factor is 16 so min size for 1st dim is 16)
  // The only case we have to worry about is cffts1 in a 2d decomposition. 
  // so the blocking factor should not be larger than the 2nd dimension. 
  //---------------------------------------------------------------------

  fftblock = FFTBLOCK_DEFAULT;
  fftblockpad = FFTBLOCKPAD_DEFAULT;

  if (layout_type == layout_2D) {
    if (dims[0][1] < fftblock) fftblock = dims[0][1];
    if (dims[1][1] < fftblock) fftblock = dims[1][1];
    if (dims[2][1] < fftblock) fftblock = dims[2][1];
  }

  if (fftblock != FFTBLOCK_DEFAULT) fftblockpad = fftblock+3;

  free(me1);
  free(me2);

  //---------------------------------------------------------------------
  // Create buffers
  //---------------------------------------------------------------------
  if (timers_enabled) timer_start(TIMER_BUFFER);

  size_t temp = 1024 / max_compute_units;
  checksum_lws = temp == 0 ? 1 : temp;
  checksum_gws = clu_RoundWorkSize((size_t)1024, checksum_lws);
  checksum_wgn = checksum_gws / checksum_lws;

  m_u = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_dims = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_xstart = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_xend   = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_ystart = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_yend   = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_zstart = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_zend   = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_u0 = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_u1 = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_u2 = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_twiddle = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_starts = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_ty1 = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_ty2 = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_chk = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);

  size_t ty_size, ty_size1, ty_size2, max2;
  ty_size1 = sizeof(dcomplex) * NTDIVNP;
  max2 = NX * NY;
  if (max2 < NY*NZ) max2 = NY * NZ;
  if (max2 < NX*NZ) max2 = NX * NZ;
  ty_size2 = sizeof(dcomplex) * FFTBLOCKPAD_DEFAULT * max2 / np;
  if (CFFTS1_DIM == 2 && CFFTS2_DIM == 2 && CFFTS3_DIM == 2) {
    ty_size = ty_size1;
  } else if (ty_size1 > ty_size2) {
    ty_size = ty_size1;
  } else {
    ty_size = ty_size2;
  }

  for (i = 0; i < num_devices; i++) {
    m_u[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(dcomplex) * NX, NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_u");

    m_dims[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(int) * 3 * 3, dims, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_dims");
    clEnqueueWriteBuffer(cmd_queue[i], m_dims[i], CL_FALSE, 0, sizeof(int) * 3 * 3, dims, 0, NULL, NULL);

    m_xstart[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(int) * 3, xstart[i], &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_xstart");
    clEnqueueWriteBuffer(cmd_queue[i], m_xstart[i], CL_FALSE, 0, sizeof(int) * 3, xstart[i], 0, NULL, NULL);

    m_xend[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(int) * 3, xend[i], &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_xend");
    clEnqueueWriteBuffer(cmd_queue[i], m_xend[i], CL_FALSE, 0, sizeof(int) * 3, xend[i], 0, NULL, NULL);

    m_ystart[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(int) * 3, ystart[i], &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_ystart");
    clEnqueueWriteBuffer(cmd_queue[i], m_ystart[i], CL_FALSE, 0, sizeof(int) * 3, ystart[i], 0, NULL, NULL);

    m_yend[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(int) * 3, yend[i], &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_yend");
    clEnqueueWriteBuffer(cmd_queue[i], m_yend[i], CL_FALSE, 0, sizeof(int) * 3, yend[i], 0, NULL, NULL);

    m_zstart[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(int) * 3, zstart[i], &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_zstart");
    clEnqueueWriteBuffer(cmd_queue[i], m_zstart[i], CL_FALSE, 0, sizeof(int) * 3, zstart[i], 0, NULL, NULL);

    m_zend[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(int) * 3, zend[i], &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_zend");
    clEnqueueWriteBuffer(cmd_queue[i], m_zend[i], CL_FALSE, 0, sizeof(int) * 3, zend[i], 0, NULL, NULL);

    m_u0[i] = clCreateBuffer(context,
                             CL_MEM_READ_WRITE,
                             sizeof(dcomplex) * NTDIVNP,
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_u0");

    m_u1[i] = clCreateBuffer(context,
                             CL_MEM_READ_WRITE,
                             sizeof(dcomplex) * NTDIVNP,
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_u1");

    m_u2[i] = clCreateBuffer(context,
                             CL_MEM_READ_WRITE,
                             sizeof(dcomplex) * NTDIVNP,
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_u2");

    m_twiddle[i] = clCreateBuffer(context,
                             CL_MEM_READ_WRITE,
                             sizeof(double) * NTDIVNP,
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_twiddle");

    m_starts[i] = clCreateBuffer(context,
                             CL_MEM_READ_WRITE,
                             sizeof(double) * dims[0][2],
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_starts");

    m_ty1[i] = clCreateBuffer(context,
                             CL_MEM_READ_WRITE,
                             ty_size,
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_ty1");

    m_ty2[i] = clCreateBuffer(context,
                             CL_MEM_READ_WRITE,
                             ty_size,
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_ty2");

    m_chk[i] = clCreateBuffer(context,
                           CL_MEM_READ_WRITE,
                           sizeof(dcomplex) * checksum_wgn,
                           NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_chk");
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  if (timers_enabled) timer_stop(TIMER_BUFFER);
}


//---------------------------------------------------------------------
// compute function from local (i,j,k) to ibar^2+jbar^2+kbar^2 
// for time evolution exponent. 
//---------------------------------------------------------------------
static void compute_indexmap(cl_mem *twiddle, int d1, int d2, int d3)
{
  int i;
  size_t cimap_lws[3], cimap_gws[3], temp;
  cl_int ecode;

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_compute_indexmap[i], 0, sizeof(cl_mem),
                                                      &m_twiddle[i]);
    ecode |= clSetKernelArg(k_compute_indexmap[i], 1, sizeof(cl_mem),
                                                      &m_dims[i]);
    ecode |= clSetKernelArg(k_compute_indexmap[i], 2, sizeof(cl_mem), 
                                                      &m_xstart[i]);
    ecode |= clSetKernelArg(k_compute_indexmap[i], 3, sizeof(cl_mem),
                                                      &m_ystart[i]);
    ecode |= clSetKernelArg(k_compute_indexmap[i], 4, sizeof(cl_mem),
                                                      &m_zstart[i]);
    ecode |= clSetKernelArg(k_compute_indexmap[i], 5, sizeof(int),
                                                      &layout_type);
    clu_CheckError(ecode, "clSetKernelArg() for compute_indexmap");
    if (COMPUTE_IMAP_DIM == 3) {
      cimap_lws[0] = d1 < work_item_sizes[0] ? d1 : work_item_sizes[0];
      temp = max_work_group_size / cimap_lws[0];
      cimap_lws[1] = d2 < temp ? d2 : temp;
      temp = temp / cimap_lws[1];
      cimap_lws[2] = d3 < temp ? d3 : temp;

      cimap_gws[0] = clu_RoundWorkSize((size_t)d1, cimap_lws[0]);
      cimap_gws[1] = clu_RoundWorkSize((size_t)d2, cimap_lws[1]);
      cimap_gws[2] = clu_RoundWorkSize((size_t)d3, cimap_lws[2]);
    } else if (COMPUTE_IMAP_DIM == 2) {
      cimap_lws[0] = d2 < work_item_sizes[0] ? d2 : work_item_sizes[0];
      temp = max_work_group_size / cimap_lws[0];
      cimap_lws[1] = d3 < temp ? d3 : temp;

      cimap_gws[0] = clu_RoundWorkSize((size_t)d2, cimap_lws[0]);
      cimap_gws[1] = clu_RoundWorkSize((size_t)d3, cimap_lws[1]);
    } else {
      //temp = d3 / max_compute_units;
      temp = 1;
      cimap_lws[0] = temp == 0 ? 1 : temp;
      cimap_gws[0] = clu_RoundWorkSize((size_t)d3, cimap_lws[0]);
    }

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_compute_indexmap[i],
                                   COMPUTE_IMAP_DIM, NULL,
                                   cimap_gws,
                                   cimap_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for compute_indexmap");

    //clFinish(cmd_queue[i]);
  }

  CHECK_FINISH();
}


static void print_timers()
{
  int i;
  char *tstrings[T_max+2];
  double t1[T_max+2];

  tstrings[ 0] = "          total "; 
  tstrings[ 1] = "          setup "; 
  tstrings[ 2] = "            fft "; 
  tstrings[ 3] = "         evolve "; 
  tstrings[ 4] = "       checksum "; 
  tstrings[ 5] = "           fftx "; 
  tstrings[ 6] = "           ffty "; 
  tstrings[ 7] = "           fftz "; 
  tstrings[ 8] = "      transpose "; 
  tstrings[ 9] = " transpose1_loc "; 
  tstrings[10] = " transpose1_glo "; 
  tstrings[11] = " transpose1_fin "; 
  tstrings[12] = " transpose2_loc "; 
  tstrings[13] = " transpose2_glo "; 
  tstrings[14] = " transpose2_fin "; 
  tstrings[15] = "           init "; 
  tstrings[16] = "        totcomp "; 
  tstrings[17] = "        totcomm "; 

  for (i = 0; i < T_max; i++) {
    t1[i] = timer_read(i);
  }
  t1[T_max+1] = t1[T_transxzglo] + t1[T_transxyglo];
  t1[T_max+0] = t1[T_total] - t1[T_max+1];

  for (i = 0; i < T_max+2; i++) {
    if (t1[i] != 0.0) {
      printf(" timer %2d(%16s) :  %10.4f\n", i+1, tstrings[i], t1[i]);
    }
  }
}


//---------------------------------------------------------------------
// compute the roots-of-unity array that will be used for subsequent FFTs. 
//---------------------------------------------------------------------
static void fft_init(int n)
{
  int m, nu, ku, i, j, ln;
  double t, ti;

  //---------------------------------------------------------------------
  // Initialize the U array with sines and cosines in a manner that permits
  // stride one access at each FFT iteration.
  //---------------------------------------------------------------------
  nu = n;
  m = ilog2(n);
  u[0] = dcmplx(m, 0.0);
  ku = 2;
  ln = 1;

  for (j = 1; j <= m; j++) {
    t = PI / ln;

    for (i = 0; i <= ln - 1; i++) {
      ti = i * t;
      u[i+ku-1] = dcmplx(cos(ti), sin(ti));
    }

    ku = ku + ln;
    ln = 2 * ln;
  }

  int ecode;

  for (i = 0; i < num_devices; i++) {
    ecode = clEnqueueWriteBuffer(cmd_queue[i], m_u[i], CL_FALSE, 0, sizeof(dcomplex) * NX, u, 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer() for m_u");
  }
  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }
}


//---------------------------------------------------------------------
// note: args x1, x2 must be different arrays
// note: args for cfftsx are (direction, layout, xin, xout, scratch)
//       xin/xout may be the same and it can be somewhat faster
//       if they are
// note: args for transpose are (layout1, layout2, xin, xout)
//       xin/xout must be different
//---------------------------------------------------------------------
static void fft(int dir, cl_mem *x1, cl_mem *x2)
{
  if (dir == 1) {
    if (layout_type == layout_0D) {
      cffts1(1, dims[0][0], dims[0][1], dims[0][2], x1, x1);
      cffts2(1, dims[1][0], dims[1][1], dims[1][2], x1, x1);
      cffts3(1, dims[2][0], dims[2][1], dims[2][2], x1, x2);
    } 
    else if (layout_type == layout_1D) {
      cffts1(1, dims[0][0], dims[0][1], dims[0][2], x1, x1);
      cffts2(1, dims[1][0], dims[1][1], dims[1][2], x1, x1);
      if (timers_enabled) timer_start(T_transpose);
      transpose_xy_z(1, 2, x1, x2);
      if (timers_enabled) timer_stop(T_transpose);
      cffts1(1, dims[2][0], dims[2][1], dims[2][2], x2, x2);
    }
    else if (layout_type == layout_2D) {
      cffts1(1, dims[0][0], dims[0][1], dims[0][2], x1, x1);
      if (timers_enabled) timer_start(T_transpose);
      transpose_x_y(0, 1, x1, x2);
      if (timers_enabled) timer_stop(T_transpose);
      cffts1(1, dims[1][0], dims[1][1], dims[1][2], x2, x2);
      if (timers_enabled) timer_start(T_transpose);
      transpose_x_z(1, 2, x2, x1);
      if (timers_enabled) timer_stop(T_transpose);
      cffts1(1, dims[2][0], dims[2][1], dims[2][2], x1, x2);
    }
  } else {
    if (layout_type == layout_0D) {
      cffts3(-1, dims[2][0], dims[2][1], dims[2][2], x1, x1);
      cffts2(-1, dims[1][0], dims[1][1], dims[1][2], x1, x1);
      cffts1(-1, dims[0][0], dims[0][1], dims[0][2], x1, x2);
    }
    else if (layout_type == layout_1D) {
      cffts1(-1, dims[2][0], dims[2][1], dims[2][2], x1, x1);
      if (timers_enabled) timer_start(T_transpose);
      transpose_x_yz(2, 1, x1, x2);
      if (timers_enabled) timer_stop(T_transpose);
      cffts2(-1, dims[1][0], dims[1][1], dims[1][2], x2, x2);
      cffts1(-1, dims[0][0], dims[0][1], dims[0][2], x2, x2);
    }
    else if (layout_type == layout_2D) {
      cffts1(-1, dims[2][0], dims[2][1], dims[2][2], x1, x1);
      if (timers_enabled) timer_start(T_transpose);
      transpose_x_z(2, 1, x1, x2);
      if (timers_enabled) timer_stop(T_transpose);
      cffts1(-1, dims[1][0], dims[1][1], dims[1][2], x2, x2);
      if (timers_enabled) timer_start(T_transpose);
      transpose_x_y(1, 0, x2, x1);
      if (timers_enabled) timer_stop(T_transpose);
      cffts1(-1, dims[0][0], dims[0][1], dims[0][2], x1, x2);
    }
  }
}


static void cffts1(int is, int d1, int d2, int d3, cl_mem *x, cl_mem *xout)
{
  int i;
  int logd1 = ilog2(d1);
  size_t cffts1_lws[2], cffts1_gws[2], temp;
  cl_int ecode;

  if (timers_enabled) timer_start(T_fftx);

  if (CFFTS1_DIM == 2) {
    //cffts1_lws[0] = d2 < work_item_sizes[0] ? d2 : work_item_sizes[0];
    //temp = max_work_group_size / cffts1_lws[0];
    //cffts1_lws[1] = d3 < temp ? d3 : temp;
    cffts1_lws[0] = 1;
    cffts1_lws[1] = 1;

    cffts1_gws[0] = clu_RoundWorkSize((size_t)d2, cffts1_lws[0]);
    cffts1_gws[1] = clu_RoundWorkSize((size_t)d3, cffts1_lws[1]);

    //printf("cffts1_lws=(%lu, %lu) cffts1_gws=(%lu, %lu) wg=(%lu, %lu)\n",
    //    cffts1_lws[0], cffts1_lws[1], cffts1_gws[0], cffts1_gws[1],
    //    cffts1_gws[0]/cffts1_lws[0], cffts1_gws[1]/cffts1_lws[1]);
  } else {
    cffts1_lws[0] = 1;
    cffts1_gws[0] = clu_RoundWorkSize((size_t)d3, cffts1_lws[0]);
    //printf("d3=%d cffts1_lws=%lu cffts1_gws=%lu wg=%lu\n",
    //    d3, cffts1_lws[0], cffts1_gws[0], cffts1_gws[0]/cffts1_lws[0]);
  }


  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_cffts1[i], 0, sizeof(cl_mem), &x[i]);
    ecode |= clSetKernelArg(k_cffts1[i], 1, sizeof(cl_mem), &xout[i]);
    ecode |= clSetKernelArg(k_cffts1[i], 2, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_cffts1[i], 3, sizeof(cl_mem), &m_ty1[i]);
    ecode |= clSetKernelArg(k_cffts1[i], 4, sizeof(cl_mem), &m_ty2[i]);
    ecode |= clSetKernelArg(k_cffts1[i], 5, sizeof(int), &is);
    ecode |= clSetKernelArg(k_cffts1[i], 6, sizeof(int), &d1);
    ecode |= clSetKernelArg(k_cffts1[i], 7, sizeof(int), &d2);
    ecode |= clSetKernelArg(k_cffts1[i], 8, sizeof(int), &d3);
    ecode |= clSetKernelArg(k_cffts1[i], 9, sizeof(int), &logd1);
    clu_CheckError(ecode, "clSetKernelArg() for k_cffts1");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_cffts1[i],
                                   CFFTS1_DIM, NULL,
                                   cffts1_gws,
                                   cffts1_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for cffts1");
  }

  CHECK_FINISH();

  if (timers_enabled) timer_stop(T_fftx);
}


static void cffts2(int is, int d1, int d2, int d3, cl_mem *x, cl_mem *xout)
{
  int i;
  int logd2 = ilog2(d2);
  size_t cffts2_lws[2], cffts2_gws[2], temp;
  cl_int ecode;

  if (timers_enabled) timer_start(T_ffty);

  if (CFFTS2_DIM == 2) {
    //cffts2_lws[0] = d1 < work_item_sizes[0] ? d1 : work_item_sizes[0];
    //temp = max_work_group_size / cffts2_lws[0];
    //cffts2_lws[1] = d3 < temp ? d3 : temp;
    cffts2_lws[0] = 1;
    cffts2_lws[1] = 1;

    cffts2_gws[0] = clu_RoundWorkSize((size_t)d1, cffts2_lws[0]);
    cffts2_gws[1] = clu_RoundWorkSize((size_t)d3, cffts2_lws[1]);
  } else {
    cffts2_lws[0] = 1;
    cffts2_gws[0] = clu_RoundWorkSize((size_t)d3, cffts2_lws[0]);
  }

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_cffts2[i], 0, sizeof(cl_mem), &x[i]);
    ecode |= clSetKernelArg(k_cffts2[i], 1, sizeof(cl_mem), &xout[i]);
    ecode |= clSetKernelArg(k_cffts2[i], 2, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_cffts2[i], 3, sizeof(cl_mem), &m_ty1[i]);
    ecode |= clSetKernelArg(k_cffts2[i], 4, sizeof(cl_mem), &m_ty2[i]);
    ecode |= clSetKernelArg(k_cffts2[i], 5, sizeof(int), &is);
    ecode |= clSetKernelArg(k_cffts2[i], 6, sizeof(int), &d1);
    ecode |= clSetKernelArg(k_cffts2[i], 7, sizeof(int), &d2);
    ecode |= clSetKernelArg(k_cffts2[i], 8, sizeof(int), &d3);
    ecode |= clSetKernelArg(k_cffts2[i], 9, sizeof(int), &logd2);
    clu_CheckError(ecode, "clSetKernelArg() for k_cffts2");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_cffts2[i],
                                   CFFTS2_DIM, NULL,
                                   cffts2_gws,
                                   cffts2_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for cffts2");
  }

  CHECK_FINISH();

  if (timers_enabled) timer_stop(T_ffty);
}


static void cffts3(int is, int d1, int d2, int d3, cl_mem *x, cl_mem *xout)
{
  int i;
  int logd3 = ilog2(d3);
  size_t cffts3_lws[2], cffts3_gws[2], temp;
  cl_int ecode;

  if (timers_enabled) timer_start(T_fftz);

  if (CFFTS3_DIM == 2) {
    cffts3_lws[0] = d1 < work_item_sizes[0] ? d1 : work_item_sizes[0];
    temp = max_work_group_size / cffts3_lws[0];
    cffts3_lws[1] = d2 < temp ? d2 : temp;

    cffts3_gws[0] = clu_RoundWorkSize((size_t)d1, cffts3_lws[0]);
    cffts3_gws[1] = clu_RoundWorkSize((size_t)d2, cffts3_lws[1]);
  } else {
    cffts3_lws[0] = 1;
    cffts3_gws[0] = clu_RoundWorkSize((size_t)d2, cffts3_lws[0]);
  }

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_cffts3[i], 0, sizeof(cl_mem), &x[i]);
    ecode |= clSetKernelArg(k_cffts3[i], 1, sizeof(cl_mem), &xout[i]);
    ecode |= clSetKernelArg(k_cffts3[i], 2, sizeof(cl_mem), &m_u[i]);
    ecode |= clSetKernelArg(k_cffts3[i], 3, sizeof(cl_mem), &m_ty1[i]);
    ecode |= clSetKernelArg(k_cffts3[i], 4, sizeof(cl_mem), &m_ty2[i]);
    ecode |= clSetKernelArg(k_cffts3[i], 5, sizeof(int), &is);
    ecode |= clSetKernelArg(k_cffts3[i], 6, sizeof(int), &d1);
    ecode |= clSetKernelArg(k_cffts3[i], 7, sizeof(int), &d2);
    ecode |= clSetKernelArg(k_cffts3[i], 8, sizeof(int), &d3);
    ecode |= clSetKernelArg(k_cffts3[i], 9, sizeof(int), &logd3);
    clu_CheckError(ecode, "clSetKernelArg() for k_cffts3");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_cffts3[i],
                                   CFFTS3_DIM, NULL,
                                   cffts3_gws,
                                   cffts3_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for cffts3");
  }

  CHECK_FINISH();

  if (timers_enabled) timer_stop(T_fftz);
}


static int ilog2(int n)
{
  int nn, lg;
  if (n == 1) return 0;
  lg = 1;
  nn = 2;
  while (nn < n) {
    nn = nn*2;
    lg = lg+1;
  }
  return lg;
}


static void transpose_x_yz(int l1, int l2, cl_mem *xin, cl_mem *xout)
{
  transpose2_local(dims[l1][0], dims[l1][1]*dims[l1][2], xin, xout);
  transpose2_global(xout, xin);
  transpose2_finish(dims[l1][0], dims[l1][1]*dims[l1][2], xin, xout);
}


static void transpose_xy_z(int l1, int l2, cl_mem *xin, cl_mem *xout)
{
  transpose2_local(dims[l1][0]*dims[l1][1], dims[l1][2], xin, xout);
  transpose2_global(xout, xin);
  transpose2_finish(dims[l1][0]*dims[l1][1], dims[l1][2], xin, xout);
}


static void transpose2_local(int n1, int n2, cl_mem *xin, cl_mem *xout)
{
  int i;
  cl_int ecode;
  size_t t2l1_lws[3], t2l1_gws[3];
  size_t t2l2_lws[3], t2l2_gws[3];
  size_t t2l3_lws[3], t2l3_gws[3];
  size_t temp;

  if (timers_enabled) timer_start(T_transxzloc);

  if (n1 < TRANSBLOCK || n2 < TRANSBLOCK) {
    if (n1 >= n2) { 
      if (TRANSPOSE2_LOCAL1_DIM == 2) {
        t2l1_lws[0] = n1 < work_item_sizes[0] ? n1 : work_item_sizes[0];
        temp = max_work_group_size / t2l1_lws[0];
        t2l1_lws[1] = n2 < temp ? n2 : temp;
        t2l1_gws[0] = clu_RoundWorkSize((size_t)n1, t2l1_lws[0]);
        t2l1_gws[1] = clu_RoundWorkSize((size_t)n2, t2l1_lws[1]);
      } else { //TRANSPOSE2_LOCAL1_DIM == 1
        //temp = n2 / max_compute_units;
        temp = 1;
        t2l1_lws[0] = temp == 0 ? 1 : temp;
        t2l1_gws[0] = clu_RoundWorkSize((size_t)n2, t2l1_lws[0]);
      }

      for (i = 0; i < num_devices; i++) {
        ecode  = clSetKernelArg(k_transpose2_local1[i], 0, sizeof(cl_mem),
                                                           &xin[i]);
        ecode |= clSetKernelArg(k_transpose2_local1[i], 1, sizeof(cl_mem),
                                                           &xout[i]);
        ecode |= clSetKernelArg(k_transpose2_local1[i], 2, sizeof(int), &n1);
        ecode |= clSetKernelArg(k_transpose2_local1[i], 3, sizeof(int), &n2);
        clu_CheckError(ecode, "clSetKernelArg()");

        ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                       k_transpose2_local1[i],
                                       TRANSPOSE2_LOCAL1_DIM, NULL,
                                       t2l1_gws,
                                       t2l1_lws,
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
      }
    } else {
      if (TRANSPOSE2_LOCAL2_DIM == 2) {
        t2l2_lws[0] = n2 < work_item_sizes[0] ? n2 : work_item_sizes[0];
        temp = max_work_group_size / t2l2_lws[0];
        t2l2_lws[1] = n1 < temp ? n1 : temp;
        t2l2_gws[0] = clu_RoundWorkSize((size_t)n2, t2l2_lws[0]);
        t2l2_gws[1] = clu_RoundWorkSize((size_t)n1, t2l2_lws[1]);
      } else { //TRANSPOSE2_LOCAL2_DIM == 1
        //temp = n1 / max_compute_units;
        temp = 1;
        t2l2_lws[0] = temp == 0 ? 1 : temp;
        t2l2_gws[0] = clu_RoundWorkSize((size_t)n1, t2l2_lws[0]);
      }

      for (i = 0; i < num_devices; i++) {
        ecode  = clSetKernelArg(k_transpose2_local2[i], 0, sizeof(cl_mem),
                                                           &xin[i]);
        ecode |= clSetKernelArg(k_transpose2_local2[i], 1, sizeof(cl_mem),
                                                           &xout[i]);
        ecode |= clSetKernelArg(k_transpose2_local2[i], 2, sizeof(int), &n1);
        ecode |= clSetKernelArg(k_transpose2_local2[i], 3, sizeof(int), &n2);
        clu_CheckError(ecode, "clSetKernelArg()");

        ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                       k_transpose2_local2[i],
                                       TRANSPOSE2_LOCAL2_DIM, NULL,
                                       t2l2_gws,
                                       t2l2_lws,
                                       0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
      }
    }
  } else {
    if (TRANSPOSE2_LOCAL3_DIM == 2) {
      size_t num_1d = n1 / TRANSBLOCK;
      size_t num_2d = n2 / TRANSBLOCK;
      t2l3_lws[0] = num_1d < work_item_sizes[0] ? num_1d : work_item_sizes[0];
      temp = max_work_group_size / t2l3_lws[0];
      t2l3_lws[1] = num_2d < temp ? num_2d : temp;
      t2l3_gws[0] = clu_RoundWorkSize((size_t)num_1d, t2l3_lws[0]);
      t2l3_gws[1] = clu_RoundWorkSize((size_t)num_2d, t2l3_lws[1]);
    } else { //TRANSPOSE2_LOCAL3_DIM == 1
      size_t num_1d = n2 / TRANSBLOCK;
      //temp = num_1d / max_compute_units;
      temp = 1;
      t2l3_lws[0] = temp == 0 ? 1 : temp;
      t2l3_gws[0] = clu_RoundWorkSize((size_t)num_1d, t2l3_lws[0]);
    }

    for (i = 0; i < num_devices; i++) {
      ecode  = clSetKernelArg(k_transpose2_local3[i], 0, sizeof(cl_mem),
                                                         &xin[i]);
      ecode |= clSetKernelArg(k_transpose2_local3[i], 1, sizeof(cl_mem),
                                                         &xout[i]);
      ecode |= clSetKernelArg(k_transpose2_local3[i], 2, sizeof(int), &n1);
      ecode |= clSetKernelArg(k_transpose2_local3[i], 3, sizeof(int), &n2);
      clu_CheckError(ecode, "clSetKernelArg()");

      //printf("[LOCAL3_DIM] [%d] [%lu,%lu,%lu] [%lu,%lu,%lu]\n", TRANSPOSE2_LOCAL3_DIM, t2l3_gws[0], t2l3_gws[1], t2l3_gws[2], t2l3_lws[0], t2l3_lws[1], t2l3_lws[2]);
      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                     k_transpose2_local3[i],
                                     TRANSPOSE2_LOCAL3_DIM, NULL,
                                     t2l3_gws,
                                     t2l3_lws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
    }
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  if (timers_enabled) timer_stop(T_transxzloc);
}

static void transpose2_global(cl_mem *xin, cl_mem *xout)
{
#ifdef USE_COPY_BUFFER
  int row, col, i, src, dst;
  size_t cb, src_offset, dst_offset;
  cl_int ecode;

  if (timers_enabled) timer_start(T_transxzglo);

  cb = sizeof(dcomplex) * NTDIVNP/np;

  for (row = 0; row < np1; row++) {
    for (col = 0; col < np2; col++) {
      src = commslice1[row][col];
      dst_offset = cb * col;

      for (i = 0; i < np2; i++) {
        dst = commslice1[row][i];
        src_offset = cb * i;

        ecode = clEnqueueCopyBuffer(cmd_queue[dst],
                                    xin[src],
                                    xout[dst],
                                    src_offset,
                                    dst_offset,
                                    cb,
                                    0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueCopyBuffer()");
      }
    }
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  if (timers_enabled) timer_stop(T_transxzglo);

#else //USE_COPY_BUFFER
  int row, col, i;
  size_t cb;

  cl_command_queue *cmd_q_list;
  cl_mem *src_b_list;
  cl_mem *dst_b_list;
  cl_int ecode;

  if (timers_enabled) timer_start(T_transxzglo);

  cmd_q_list = (cl_command_queue *)malloc(sizeof(cl_command_queue) * np2);
  src_b_list = (cl_mem *)malloc(sizeof(cl_mem) * np2);
  dst_b_list = (cl_mem *)malloc(sizeof(cl_mem) * np2);

  cb = sizeof(dcomplex) * NTDIVNP/np;

  for (row = 0; row < np1; row++) {
    for (col = 0; col < np2; col++) {
      i = commslice1[row][col];
      cmd_q_list[col] = cmd_queue[i];
      src_b_list[col] = xin[i];
      dst_b_list[col] = xout[i];
    }

    ecode = clEnqueueAlltoAllBuffer(cmd_q_list,
                                    np2,
                                    src_b_list,
                                    dst_b_list,
                                    0, 0,
                                    cb,
                                    0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueAlltoAllBuffer()");
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  free(cmd_q_list);
  free(src_b_list);
  free(dst_b_list);

  if (timers_enabled) timer_stop(T_transxzglo);
#endif
}

static void transpose2_finish(int n1, int n2, cl_mem *xin, cl_mem *xout)
{
  int i;
  cl_int ecode;
  size_t t2f_lws[3], t2f_gws[3], temp;

  if (timers_enabled) timer_start(T_transxzfin);

  if (TRANSPOSE2_FINISH_DIM == 3) {
    size_t num_2d = n1/np2;
    t2f_lws[0] = n2 < work_item_sizes[0] ? n2 : work_item_sizes[0];
    temp = max_work_group_size / t2f_lws[0];
    t2f_lws[1] = num_2d < temp ? num_2d : temp;
    temp = temp / t2f_lws[1];
    t2f_lws[2] = np2 < temp ? np2 : temp;

    t2f_gws[0] = clu_RoundWorkSize((size_t)n2, t2f_lws[0]);
    t2f_gws[1] = clu_RoundWorkSize((size_t)num_2d, t2f_lws[1]);
    t2f_gws[2] = clu_RoundWorkSize((size_t)np2, t2f_lws[2]);
  } else if (TRANSPOSE2_FINISH_DIM == 2) {
    size_t num_1d = n1/np2;
    //t2f_lws[0] = num_1d < work_item_sizes[0] ? num_1d : work_item_sizes[0];
    //temp = max_work_group_size / t2f_lws[0];
    //t2f_lws[1] = np2 < temp ? np2 : temp;
    t2f_lws[0] = 1;
    t2f_lws[1] = 1;

    t2f_gws[0] = clu_RoundWorkSize((size_t)num_1d, t2f_lws[0]);
    t2f_gws[1] = clu_RoundWorkSize((size_t)np2, t2f_lws[1]);
  } else {
    //temp = np2 / max_compute_units;
    temp = 1;
    t2f_lws[0] = temp == 0 ? 1 : temp;
    t2f_gws[0] = clu_RoundWorkSize((size_t)np2, t2f_lws[0]);
  }

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_transpose2_finish[i], 0, sizeof(cl_mem),
                                                       &xin[i]);
    ecode |= clSetKernelArg(k_transpose2_finish[i], 1, sizeof(cl_mem),
                                                       &xout[i]);
    ecode |= clSetKernelArg(k_transpose2_finish[i], 2, sizeof(int), &n1);
    ecode |= clSetKernelArg(k_transpose2_finish[i], 3, sizeof(int), &n2);
    ecode |= clSetKernelArg(k_transpose2_finish[i], 4, sizeof(int), &np2);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_transpose2_finish[i],
                                   TRANSPOSE2_FINISH_DIM, NULL,
                                   t2f_gws,
                                   t2f_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  if (timers_enabled) timer_stop(T_transxzfin);
}


static void transpose_x_z(int l1, int l2, cl_mem *xin, cl_mem *xout)
{
  transpose_x_z_local(dims[l1][0], dims[l1][1], dims[l1][2], xin, xout);
  transpose_x_z_global(dims[l1][0], dims[l1][1], dims[l1][2], xout, xin);
  transpose_x_z_finish(dims[l2][0], dims[l2][1], dims[l2][2], xin, xout);
}


static void transpose_x_z_local(int d1, int d2, int d3, 
                                cl_mem *xin, cl_mem *xout)
{
  int block1, block3;
  int i;
  size_t txzl1_lws[3], txzl1_gws[3];
  size_t txzl2_lws[3], txzl2_gws[3];
  size_t temp;
  cl_int ecode;

  if (timers_enabled) timer_start(T_transxzloc);

  if (d1 < 32 || d3 == 1) {
    //---------------------------------------------------------------------
    // basic transpose
    //---------------------------------------------------------------------
    if (TRANSPOSE_X_Z_LOCAL1_DIM == 3) {
      txzl1_lws[0] = d1 < work_item_sizes[0] ? d1 : work_item_sizes[0];
      temp = max_work_group_size / txzl1_lws[0];
      txzl1_lws[1] = d3 < temp ? d3 : temp;
      temp = temp / txzl1_lws[1];
      txzl1_lws[2] = d2 < temp ? d2 : temp;

      txzl1_gws[0] = clu_RoundWorkSize((size_t)d1, txzl1_lws[0]);
      txzl1_gws[1] = clu_RoundWorkSize((size_t)d3, txzl1_lws[1]);
      txzl1_gws[2] = clu_RoundWorkSize((size_t)d2, txzl1_lws[2]);
    } else if (TRANSPOSE_X_Z_LOCAL1_DIM == 2) {
      txzl1_lws[0] = d3 < work_item_sizes[0] ? d3 : work_item_sizes[0];
      temp = max_work_group_size / txzl1_lws[0];
      txzl1_lws[1] = d2 < temp ? d2 : temp;

      txzl1_gws[0] = clu_RoundWorkSize((size_t)d3, txzl1_lws[0]);
      txzl1_gws[1] = clu_RoundWorkSize((size_t)d2, txzl1_lws[1]);
    } else {
      //temp = d2 / max_compute_units;
      temp = 1;
      txzl1_lws[0] = temp == 0 ? 1 : temp;
      txzl1_gws[0] = clu_RoundWorkSize((size_t)d2, txzl1_lws[0]);
    }

    for (i = 0; i < num_devices; i++) {
      ecode  = clSetKernelArg(k_transpose_x_z_local1[i], 0, sizeof(cl_mem),
                                                            &xin[i]);
      ecode |= clSetKernelArg(k_transpose_x_z_local1[i], 1, sizeof(cl_mem),
                                                            &xout[i]);
      ecode |= clSetKernelArg(k_transpose_x_z_local1[i], 2, sizeof(int), &d1);
      ecode |= clSetKernelArg(k_transpose_x_z_local1[i], 3, sizeof(int), &d2);
      ecode |= clSetKernelArg(k_transpose_x_z_local1[i], 4, sizeof(int), &d3);
      clu_CheckError(ecode, "clSetKernelArg()");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                     k_transpose_x_z_local1[i],
                                     TRANSPOSE_X_Z_LOCAL1_DIM, NULL,
                                     txzl1_gws,
                                     txzl1_lws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
    }
  } else {
    block3 = d3;
    if (block3 > TRANSBLOCK) block3 = TRANSBLOCK;
    block1 = d1;
    if (block1*block3 > TRANSBLOCK*TRANSBLOCK) 
      block1 = TRANSBLOCK*TRANSBLOCK/block3;

    //---------------------------------------------------------------------
    // blocked transpose
    //---------------------------------------------------------------------
    if (TRANSPOSE_X_Z_LOCAL2_DIM == 3) {
      int num_1d = d1 / block1;
      int num_2d = d3 / block3;
      txzl2_lws[0] = num_1d<work_item_sizes[0] ? num_1d : work_item_sizes[0];
      temp = max_work_group_size / txzl2_lws[0];
      txzl2_lws[1] = num_2d < temp ? num_2d : temp;
      temp = temp / txzl2_lws[1];
      txzl2_lws[2] = d2 < temp ? d2 : temp;

      txzl2_gws[0] = clu_RoundWorkSize((size_t)num_1d, txzl2_lws[0]);
      txzl2_gws[1] = clu_RoundWorkSize((size_t)num_2d, txzl2_lws[1]);
      txzl2_gws[2] = clu_RoundWorkSize((size_t)d2, txzl2_lws[2]);
    } else if (TRANSPOSE_X_Z_LOCAL2_DIM == 2) {
      int num_1d = d3 / block3;
      txzl2_lws[0] = num_1d<work_item_sizes[0] ? num_1d : work_item_sizes[0];
      temp = max_work_group_size / txzl2_lws[0];
      txzl2_lws[1] = d2 < temp ? d2 : temp;

      txzl2_gws[0] = clu_RoundWorkSize((size_t)num_1d, txzl2_lws[0]);
      txzl2_gws[1] = clu_RoundWorkSize((size_t)d2, txzl2_lws[1]);
    } else {
      //temp = d2 / max_compute_units;
      temp = 1;
      txzl2_lws[0] = temp == 0 ? 1 : temp;
      txzl2_gws[0] = clu_RoundWorkSize((size_t)d2, txzl2_lws[0]);
    }

    for (i = 0; i < num_devices; i++) {
      ecode  = clSetKernelArg(k_transpose_x_z_local2[i], 0, sizeof(cl_mem),
                                                            &xin[i]);
      ecode |= clSetKernelArg(k_transpose_x_z_local2[i], 1, sizeof(cl_mem),
                                                            &xout[i]);
      ecode |= clSetKernelArg(k_transpose_x_z_local2[i], 2, sizeof(int), &d1);
      ecode |= clSetKernelArg(k_transpose_x_z_local2[i], 3, sizeof(int), &d2);
      ecode |= clSetKernelArg(k_transpose_x_z_local2[i], 4, sizeof(int), &d3);
      ecode |= clSetKernelArg(k_transpose_x_z_local2[i], 5, sizeof(int),
                                                            &block1);
      ecode |= clSetKernelArg(k_transpose_x_z_local2[i], 6, sizeof(int),
                                                            &block3);
      clu_CheckError(ecode, "clSetKernelArg()");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                     k_transpose_x_z_local2[i],
                                     TRANSPOSE_X_Z_LOCAL2_DIM, NULL,
                                     txzl2_gws,
                                     txzl2_lws,
                                     0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
    }
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  if (timers_enabled) timer_stop(T_transxzloc);
}

static void transpose_x_z_global(int d1, int d2, int d3,
                                 cl_mem *xin, cl_mem *xout)
{
#ifdef USE_COPY_BUFFER
  int row, col, i, src, dst;
  size_t cb, src_offset, dst_offset;
  cl_int ecode;

  // do transpose among all  processes with same 1-coord (me1)
  //---------------------------------------------------------------------
  if (timers_enabled) timer_start(T_transxzglo);

  cb = sizeof(dcomplex) * d1*d2*d3/np2;

  for (row = 0; row < np1; row++) {
    for (col = 0; col < np2; col++) {
      src = commslice1[row][col];
      dst_offset = cb * col;

      for (i = 0; i < np2; i++) {
        dst = commslice1[row][i];
        src_offset = cb * i;

        ecode = clEnqueueCopyBuffer(cmd_queue[dst],
                                    xin[src],
                                    xout[dst],
                                    src_offset,
                                    dst_offset,
                                    cb,
                                    0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueCopyBuffer()");
      }
    }
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  if (timers_enabled) timer_stop(T_transxzglo);

#else //USE_COPY_BUFFER
  int row, col, i;
  size_t cb;

  cl_command_queue *cmd_q_list;
  cl_mem *src_b_list;
  cl_mem *dst_b_list;
  cl_int ecode;

  if (timers_enabled) timer_start(T_transxzglo);

  cmd_q_list = (cl_command_queue *)malloc(sizeof(cl_command_queue) * np2);
  src_b_list = (cl_mem *)malloc(sizeof(cl_mem) * np2);
  dst_b_list = (cl_mem *)malloc(sizeof(cl_mem) * np2);

  cb = sizeof(dcomplex) * d1*d2*d3/np2;

  for (row = 0; row < np1; row++) {
    for (col = 0; col < np2; col++) {
      i = commslice1[row][col];
      cmd_q_list[col] = cmd_queue[i];
      src_b_list[col] = xin[i];
      dst_b_list[col] = xout[i];
    }

    ecode = clEnqueueAlltoAllBuffer(cmd_q_list,
                                    np2,
                                    src_b_list,
                                    dst_b_list,
                                    0, 0,
                                    cb,
                                    0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueAlltoAllBuffer()");
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  free(cmd_q_list);
  free(src_b_list);
  free(dst_b_list);

  if (timers_enabled) timer_stop(T_transxzglo);
#endif
}

static void transpose_x_z_finish(int d1, int d2, int d3,
                                 cl_mem *xin, cl_mem *xout)
{
  int i;
  cl_int ecode;
  size_t txzf_lws[3], txzf_gws[3], temp;

  if (timers_enabled) timer_start(T_transxzfin);

  if (TRANSPOSE_X_Z_FINISH_DIM == 3) {
    txzf_lws[0] = d2 < work_item_sizes[0] ? d2 : work_item_sizes[0];
    temp = max_work_group_size / txzf_lws[0];
    txzf_lws[1] = d3 < temp ? d3 : temp;
    temp = temp / txzf_lws[1];
    txzf_lws[2] = np2 < temp ? np2 : temp;

    txzf_gws[0] = clu_RoundWorkSize((size_t)d2, txzf_lws[0]);
    txzf_gws[1] = clu_RoundWorkSize((size_t)d3, txzf_lws[1]);
    txzf_gws[2] = clu_RoundWorkSize((size_t)np2, txzf_lws[2]);
  } else if (TRANSPOSE_X_Z_FINISH_DIM == 2) {
    //txzf_lws[0] = d3 < work_item_sizes[0] ? d3 : work_item_sizes[0];
    //temp = max_work_group_size / txzf_lws[0];
    //txzf_lws[1] = np2 < temp ? np2 : temp;
    txzf_lws[0] = 1;
    txzf_lws[1] = 1;

    txzf_gws[0] = clu_RoundWorkSize((size_t)d3, txzf_lws[0]);
    txzf_gws[1] = clu_RoundWorkSize((size_t)np2, txzf_lws[1]);
  } else {
    //temp = np2 / max_compute_units;
    temp = 1;
    txzf_lws[0] = temp == 0 ? 1 : temp;
    txzf_gws[0] = clu_RoundWorkSize((size_t)np2, txzf_lws[0]);
  }

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_transpose_x_z_finish[i], 0, sizeof(cl_mem),
                                                          &xin[i]);
    ecode |= clSetKernelArg(k_transpose_x_z_finish[i], 1, sizeof(cl_mem),
                                                          &xout[i]);
    ecode |= clSetKernelArg(k_transpose_x_z_finish[i], 2, sizeof(int), &d1);
    ecode |= clSetKernelArg(k_transpose_x_z_finish[i], 3, sizeof(int), &d2);
    ecode |= clSetKernelArg(k_transpose_x_z_finish[i], 4, sizeof(int), &d3);
    ecode |= clSetKernelArg(k_transpose_x_z_finish[i], 5, sizeof(int), &np2);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_transpose_x_z_finish[i],
                                   TRANSPOSE_X_Z_FINISH_DIM, NULL,
                                   txzf_gws,
                                   txzf_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  if (timers_enabled) timer_stop(T_transxzfin);
}


//---------------------------------------------------------------------
// xy transpose is a little tricky, since we don't want
// to touch 3rd axis. But alltoall must involve 3rd axis (most 
// slowly varying) to be efficient. So we do
// (nx, ny/np1, nz/np2) -> (ny/np1, nz/np2, nx) (local)
// (ny/np1, nz/np2, nx) -> ((ny/np1*nz/np2)*np1, nx/np1) (global)
// then local finish. 
//---------------------------------------------------------------------
static void transpose_x_y(int l1, int l2, cl_mem *xin, cl_mem *xout)
{
  transpose_x_y_local(dims[l1][0], dims[l1][1], dims[l1][2], xin, xout);
  transpose_x_y_global(dims[l1][0], dims[l1][1], dims[l1][2], xout, xin);
  transpose_x_y_finish(dims[l2][0], dims[l2][1], dims[l2][2], xin, xout);
}


static void transpose_x_y_local(int d1, int d2, int d3,
                                cl_mem *xin, cl_mem *xout)
{
  int i;
  size_t txyl_lws[3], txyl_gws[3], temp;
  cl_int ecode;

  if (TRANSPOSE_X_Y_LOCAL_DIM == 3) {
    txyl_lws[0] = d2 < work_item_sizes[0] ? d2 : work_item_sizes[0];
    temp = max_work_group_size / txyl_lws[0];
    txyl_lws[1] = d1 < temp ? d1 : temp;
    temp = temp / txyl_lws[1];
    txyl_lws[2] = d3 < temp ? d3 : temp;

    txyl_gws[0] = clu_RoundWorkSize((size_t)d2, txyl_lws[0]);
    txyl_gws[1] = clu_RoundWorkSize((size_t)d1, txyl_lws[1]);
    txyl_gws[2] = clu_RoundWorkSize((size_t)d3, txyl_lws[2]);
  } else if (TRANSPOSE_X_Y_LOCAL_DIM == 2) {
    txyl_lws[0] = d1 < work_item_sizes[0] ? d1 : work_item_sizes[0];
    temp = max_work_group_size / txyl_lws[0];
    txyl_lws[1] = d3 < temp ? d3 : temp;

    txyl_gws[0] = clu_RoundWorkSize((size_t)d1, txyl_lws[0]);
    txyl_gws[1] = clu_RoundWorkSize((size_t)d3, txyl_lws[1]);
  } else {
    //temp = d3 / max_compute_units;
    temp = 1;
    txyl_lws[0] = temp == 0 ? 1 : temp;
    txyl_gws[0] = clu_RoundWorkSize((size_t)d3, txyl_lws[0]);
  }

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_transpose_x_y_local[i], 0, sizeof(cl_mem),
                                                         &xin[i]);
    ecode |= clSetKernelArg(k_transpose_x_y_local[i], 1, sizeof(cl_mem),
                                                         &xout[i]);
    ecode |= clSetKernelArg(k_transpose_x_y_local[i], 2, sizeof(int), &d1);
    ecode |= clSetKernelArg(k_transpose_x_y_local[i], 3, sizeof(int), &d2);
    ecode |= clSetKernelArg(k_transpose_x_y_local[i], 4, sizeof(int), &d3);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_transpose_x_y_local[i],
                                   TRANSPOSE_X_Y_LOCAL_DIM, NULL,
                                   txyl_gws,
                                   txyl_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }
}


//---------------------------------------------------------------------
// array is in form [nx][nz/np2][ny/np1]
//---------------------------------------------------------------------
static void transpose_x_y_global(int d1, int d2, int d3,
                                 cl_mem *xin, cl_mem *xout)
{
#ifdef USE_COPY_BUFFER
  int row, col, i, src, dst;
  size_t cb, src_offset, dst_offset;
  cl_int ecode;

  //---------------------------------------------------------------------
  // do transpose among all processes with same 1-coord (me1)
  //---------------------------------------------------------------------
  if (timers_enabled) timer_start(T_transxyglo);
  cb = sizeof(dcomplex) * d1*d2*d3/np1;

  for (row = 0; row < np2; row++) {
    for (col = 0; col < np1; col++) {
      src = commslice2[row][col];
      dst_offset = cb * col;

      for (i = 0; i < np2; i++) {
        dst = commslice2[row][i];
        src_offset = cb * i;

        ecode = clEnqueueCopyBuffer(cmd_queue[dst],
                                    xin[src],
                                    xout[dst],
                                    src_offset,
                                    dst_offset,
                                    cb,
                                    0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueCopyBuffer()");
      }
    }
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  if (timers_enabled) timer_stop(T_transxyglo);

#else //USE_COPY_BUFFER

  int row, col, i;
  size_t cb;

  cl_command_queue *cmd_q_list;
  cl_mem *src_b_list;
  cl_mem *dst_b_list;
  cl_int ecode;

  if (timers_enabled) timer_start(T_transxyglo);

  cmd_q_list = (cl_command_queue *)malloc(sizeof(cl_command_queue) * np1);
  src_b_list = (cl_mem *)malloc(sizeof(cl_mem) * np1);
  dst_b_list = (cl_mem *)malloc(sizeof(cl_mem) * np1);

  cb = sizeof(dcomplex) * d1*d2*d3/np1;

  for (row = 0; row < np2; row++) {
    for (col = 0; col < np1; col++) {
      i = commslice1[row][col];
      cmd_q_list[col] = cmd_queue[i];
      src_b_list[col] = xin[i];
      dst_b_list[col] = xout[i];
    }

    ecode = clEnqueueAlltoAllBuffer(cmd_q_list,
                                    np1,
                                    src_b_list,
                                    dst_b_list,
                                    0, 0,
                                    cb,
                                    0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueAlltoAllBuffer()");
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  free(cmd_q_list);
  free(src_b_list);
  free(dst_b_list);

  if (timers_enabled) timer_stop(T_transxyglo);
  
#endif //USE_COPY_BUFFER
}


static void transpose_x_y_finish(int d1, int d2, int d3,
                                 cl_mem *xin, cl_mem *xout)
{
  int i;
  cl_int ecode;
  size_t txyf_lws[3], txyf_gws[3], temp;

  if (timers_enabled) timer_start(T_transxyfin);

  if (TRANSPOSE_X_Y_FINISH_DIM == 3) {
    txyf_lws[0] = d2 < work_item_sizes[0] ? d2 : work_item_sizes[0];
    temp = max_work_group_size / txyf_lws[0];
    txyf_lws[1] = d3 < temp ? d3 : temp;
    temp = temp / txyf_lws[1];
    txyf_lws[2] = np1 < temp ? np1 : temp;

    txyf_gws[0] = clu_RoundWorkSize((size_t)d2, txyf_lws[0]);
    txyf_gws[1] = clu_RoundWorkSize((size_t)d3, txyf_lws[1]);
    txyf_gws[2] = clu_RoundWorkSize((size_t)np1, txyf_lws[2]);
  } else if (TRANSPOSE_X_Y_FINISH_DIM == 2) {
    txyf_lws[0] = d3 < work_item_sizes[0] ? d3 : work_item_sizes[0];
    temp = max_work_group_size / txyf_lws[0];
    txyf_lws[1] = np1 < temp ? np1 : temp;

    txyf_gws[0] = clu_RoundWorkSize((size_t)d3, txyf_lws[0]);
    txyf_gws[1] = clu_RoundWorkSize((size_t)np1, txyf_lws[1]);
  } else {
    //temp = np1 / max_compute_units;
    temp = 1;
    txyf_lws[0] = temp == 0 ? 1 : temp;
    txyf_gws[0] = clu_RoundWorkSize((size_t)np1, txyf_lws[0]);
  }

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_transpose_x_y_finish[i], 0, sizeof(cl_mem),
                                                          &xin[i]);
    ecode |= clSetKernelArg(k_transpose_x_y_finish[i], 1, sizeof(cl_mem),
                                                          &xout[i]);
    ecode |= clSetKernelArg(k_transpose_x_y_finish[i], 2, sizeof(int), &d1);
    ecode |= clSetKernelArg(k_transpose_x_y_finish[i], 3, sizeof(int), &d2);
    ecode |= clSetKernelArg(k_transpose_x_y_finish[i], 4, sizeof(int), &d3);
    ecode |= clSetKernelArg(k_transpose_x_y_finish[i], 5, sizeof(int), &np1);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_transpose_x_y_finish[i],
                                   TRANSPOSE_X_Y_FINISH_DIM, NULL,
                                   txyf_gws,
                                   txyf_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  if (timers_enabled) timer_stop(T_transxyfin);
}


static void checksum(int it, cl_mem *u1, int d1, int d2, int d3)
{
  int i, k;
  dcomplex chk = dcmplx(0.0, 0.0);
  cl_int ecode;

  dcomplex **g_chk = (dcomplex **)malloc(sizeof(dcomplex *) * num_devices);

  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_checksum[i], 0, sizeof(cl_mem), &u1[i]);
    ecode |= clSetKernelArg(k_checksum[i], 1, sizeof(cl_mem), &m_chk[i]);
    ecode |= clSetKernelArg(k_checksum[i], 2, sizeof(dcomplex) * checksum_lws,
                                              NULL);
    ecode |= clSetKernelArg(k_checksum[i], 3, sizeof(cl_mem), &m_xstart[i]);
    ecode |= clSetKernelArg(k_checksum[i], 4, sizeof(cl_mem), &m_xend[i]);
    ecode |= clSetKernelArg(k_checksum[i], 5, sizeof(cl_mem), &m_ystart[i]);
    ecode |= clSetKernelArg(k_checksum[i], 6, sizeof(cl_mem), &m_yend[i]);
    ecode |= clSetKernelArg(k_checksum[i], 7, sizeof(cl_mem), &m_zstart[i]);
    ecode |= clSetKernelArg(k_checksum[i], 8, sizeof(cl_mem), &m_zend[i]);
    ecode |= clSetKernelArg(k_checksum[i], 9, sizeof(int), &d1);
    ecode |= clSetKernelArg(k_checksum[i], 10, sizeof(int), &d2);
    clu_CheckError(ecode, "clSetKernelArg() for checksum");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
                                   k_checksum[i],
                                   1, NULL,
                                   &checksum_gws,
                                   &checksum_lws,
                                   0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel()");

    g_chk[i] = (dcomplex *)malloc(sizeof(dcomplex) * checksum_wgn);

    ecode = clEnqueueReadBuffer(cmd_queue[i],
                                m_chk[i],
                                CL_FALSE,
                                0, checksum_wgn * sizeof(dcomplex),
                                (void *)g_chk[i],
                                0, NULL, NULL);
    clu_CheckError(ecode, "clReadBuffer()");
  }

  // reduction
  for (i = 0; i < num_devices; i++) {
    ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");

    for (k = 0; k < checksum_wgn; k++) {
      chk = dcmplx_add(chk, g_chk[i][k]);
    }
    free(g_chk[i]);
  }

  chk = dcmplx_div2(chk, NTOTAL_F);

  printf(" T =%5d     Checksum =%22.12E%22.12E\n", it, chk.real, chk.imag);

  // sums[it] = allchk
  // If we compute the checksum for diagnostic purposes, we let it be
  // negative, so the result will not be stored in an array
  if (it > 0) sums[it] = chk;
  
  free(g_chk);
}


static void verify(int d1, int d2, int d3, int nt, 
                   logical *verified, char *Class)
{
  int i;
  double err, epsilon;

  //---------------------------------------------------------------------
  // Reference checksums
  //---------------------------------------------------------------------
  dcomplex csum_ref[25+1];

  *Class = 'U';

  epsilon = 1.0e-12;
  *verified = false;

  if (d1 == 64 && d2 == 64 && d3 == 64 && nt == 6) {
    //---------------------------------------------------------------------
    //   Sample size reference checksums
    //---------------------------------------------------------------------
    *Class = 'S';
    csum_ref[1] = dcmplx(5.546087004964E+02, 4.845363331978E+02);
    csum_ref[2] = dcmplx(5.546385409189E+02, 4.865304269511E+02);
    csum_ref[3] = dcmplx(5.546148406171E+02, 4.883910722336E+02);
    csum_ref[4] = dcmplx(5.545423607415E+02, 4.901273169046E+02);
    csum_ref[5] = dcmplx(5.544255039624E+02, 4.917475857993E+02);
    csum_ref[6] = dcmplx(5.542683411902E+02, 4.932597244941E+02);

  } else if (d1 == 128 && d2 == 128 && d3 == 32 && nt == 6) {
    //---------------------------------------------------------------------
    //   Class W size reference checksums
    //---------------------------------------------------------------------
    *Class = 'W';
    csum_ref[1] = dcmplx(5.673612178944E+02, 5.293246849175E+02);
    csum_ref[2] = dcmplx(5.631436885271E+02, 5.282149986629E+02);
    csum_ref[3] = dcmplx(5.594024089970E+02, 5.270996558037E+02);
    csum_ref[4] = dcmplx(5.560698047020E+02, 5.260027904925E+02);
    csum_ref[5] = dcmplx(5.530898991250E+02, 5.249400845633E+02);
    csum_ref[6] = dcmplx(5.504159734538E+02, 5.239212247086E+02);

  } else if (d1 == 256 && d2 == 256 && d3 == 128 && nt == 6) {
    //---------------------------------------------------------------------
    //   Class A size reference checksums
    //---------------------------------------------------------------------
    *Class = 'A';
    csum_ref[1] = dcmplx(5.046735008193E+02, 5.114047905510E+02);
    csum_ref[2] = dcmplx(5.059412319734E+02, 5.098809666433E+02);
    csum_ref[3] = dcmplx(5.069376896287E+02, 5.098144042213E+02);
    csum_ref[4] = dcmplx(5.077892868474E+02, 5.101336130759E+02);
    csum_ref[5] = dcmplx(5.085233095391E+02, 5.104914655194E+02);
    csum_ref[6] = dcmplx(5.091487099959E+02, 5.107917842803E+02);

  } else if (d1 == 512 && d2 == 256 && d3 == 256 && nt == 20) {
    //---------------------------------------------------------------------
    //   Class B size reference checksums
    //---------------------------------------------------------------------
    *Class = 'B';
    csum_ref[1]  = dcmplx(5.177643571579E+02, 5.077803458597E+02);
    csum_ref[2]  = dcmplx(5.154521291263E+02, 5.088249431599E+02);
    csum_ref[3]  = dcmplx(5.146409228649E+02, 5.096208912659E+02);
    csum_ref[4]  = dcmplx(5.142378756213E+02, 5.101023387619E+02);
    csum_ref[5]  = dcmplx(5.139626667737E+02, 5.103976610617E+02);
    csum_ref[6]  = dcmplx(5.137423460082E+02, 5.105948019802E+02);
    csum_ref[7]  = dcmplx(5.135547056878E+02, 5.107404165783E+02);
    csum_ref[8]  = dcmplx(5.133910925466E+02, 5.108576573661E+02);
    csum_ref[9]  = dcmplx(5.132470705390E+02, 5.109577278523E+02);
    csum_ref[10] = dcmplx(5.131197729984E+02, 5.110460304483E+02);
    csum_ref[11] = dcmplx(5.130070319283E+02, 5.111252433800E+02);
    csum_ref[12] = dcmplx(5.129070537032E+02, 5.111968077718E+02);
    csum_ref[13] = dcmplx(5.128182883502E+02, 5.112616233064E+02);
    csum_ref[14] = dcmplx(5.127393733383E+02, 5.113203605551E+02);
    csum_ref[15] = dcmplx(5.126691062020E+02, 5.113735928093E+02);
    csum_ref[16] = dcmplx(5.126064276004E+02, 5.114218460548E+02);
    csum_ref[17] = dcmplx(5.125504076570E+02, 5.114656139760E+02);
    csum_ref[18] = dcmplx(5.125002331720E+02, 5.115053595966E+02);
    csum_ref[19] = dcmplx(5.124551951846E+02, 5.115415130407E+02);
    csum_ref[20] = dcmplx(5.124146770029E+02, 5.115744692211E+02);

  } else if (d1 == 512 && d2 == 512 && d3 == 512 && nt == 20) {
    //---------------------------------------------------------------------
    //   Class C size reference checksums
    //---------------------------------------------------------------------
    *Class = 'C';
    csum_ref[1]  = dcmplx(5.195078707457E+02, 5.149019699238E+02);
    csum_ref[2]  = dcmplx(5.155422171134E+02, 5.127578201997E+02);
    csum_ref[3]  = dcmplx(5.144678022222E+02, 5.122251847514E+02);
    csum_ref[4]  = dcmplx(5.140150594328E+02, 5.121090289018E+02);
    csum_ref[5]  = dcmplx(5.137550426810E+02, 5.121143685824E+02);
    csum_ref[6]  = dcmplx(5.135811056728E+02, 5.121496764568E+02);
    csum_ref[7]  = dcmplx(5.134569343165E+02, 5.121870921893E+02);
    csum_ref[8]  = dcmplx(5.133651975661E+02, 5.122193250322E+02);
    csum_ref[9]  = dcmplx(5.132955192805E+02, 5.122454735794E+02);
    csum_ref[10] = dcmplx(5.132410471738E+02, 5.122663649603E+02);
    csum_ref[11] = dcmplx(5.131971141679E+02, 5.122830879827E+02);
    csum_ref[12] = dcmplx(5.131605205716E+02, 5.122965869718E+02);
    csum_ref[13] = dcmplx(5.131290734194E+02, 5.123075927445E+02);
    csum_ref[14] = dcmplx(5.131012720314E+02, 5.123166486553E+02);
    csum_ref[15] = dcmplx(5.130760908195E+02, 5.123241541685E+02);
    csum_ref[16] = dcmplx(5.130528295923E+02, 5.123304037599E+02);
    csum_ref[17] = dcmplx(5.130310107773E+02, 5.123356167976E+02);
    csum_ref[18] = dcmplx(5.130103090133E+02, 5.123399592211E+02);
    csum_ref[19] = dcmplx(5.129905029333E+02, 5.123435588985E+02);
    csum_ref[20] = dcmplx(5.129714421109E+02, 5.123465164008E+02);

  } else if (d1 == 2048 && d2 == 1024 && d3 == 1024 && nt == 25) {
    //---------------------------------------------------------------------
    //   Class D size reference checksums
    //---------------------------------------------------------------------
    *Class = 'D';
    csum_ref[1]  = dcmplx(5.122230065252E+02, 5.118534037109E+02);
    csum_ref[2]  = dcmplx(5.120463975765E+02, 5.117061181082E+02);
    csum_ref[3]  = dcmplx(5.119865766760E+02, 5.117096364601E+02);
    csum_ref[4]  = dcmplx(5.119518799488E+02, 5.117373863950E+02);
    csum_ref[5]  = dcmplx(5.119269088223E+02, 5.117680347632E+02);
    csum_ref[6]  = dcmplx(5.119082416858E+02, 5.117967875532E+02);
    csum_ref[7]  = dcmplx(5.118943814638E+02, 5.118225281841E+02);
    csum_ref[8]  = dcmplx(5.118842385057E+02, 5.118451629348E+02);
    csum_ref[9]  = dcmplx(5.118769435632E+02, 5.118649119387E+02);
    csum_ref[10] = dcmplx(5.118718203448E+02, 5.118820803844E+02);
    csum_ref[11] = dcmplx(5.118683569061E+02, 5.118969781011E+02);
    csum_ref[12] = dcmplx(5.118661708593E+02, 5.119098918835E+02);
    csum_ref[13] = dcmplx(5.118649768950E+02, 5.119210777066E+02);
    csum_ref[14] = dcmplx(5.118645605626E+02, 5.119307604484E+02);
    csum_ref[15] = dcmplx(5.118647586618E+02, 5.119391362671E+02);
    csum_ref[16] = dcmplx(5.118654451572E+02, 5.119463757241E+02);
    csum_ref[17] = dcmplx(5.118665212451E+02, 5.119526269238E+02);
    csum_ref[18] = dcmplx(5.118679083821E+02, 5.119580184108E+02);
    csum_ref[19] = dcmplx(5.118695433664E+02, 5.119626617538E+02);
    csum_ref[20] = dcmplx(5.118713748264E+02, 5.119666538138E+02);
    csum_ref[21] = dcmplx(5.118733606701E+02, 5.119700787219E+02);
    csum_ref[22] = dcmplx(5.118754661974E+02, 5.119730095953E+02);
    csum_ref[23] = dcmplx(5.118776626738E+02, 5.119755100241E+02);
    csum_ref[24] = dcmplx(5.118799262314E+02, 5.119776353561E+02);
    csum_ref[25] = dcmplx(5.118822370068E+02, 5.119794338060E+02);

  } else if (d1 == 4096 && d2 == 2048 && d3 == 2048 && nt == 25) {
    //---------------------------------------------------------------------
    //   Class E size reference checksums
    //---------------------------------------------------------------------
    *Class = 'E';
    csum_ref[1]  = dcmplx(5.121601045346E+02, 5.117395998266E+02);
    csum_ref[2]  = dcmplx(5.120905403678E+02, 5.118614716182E+02);
    csum_ref[3]  = dcmplx(5.120623229306E+02, 5.119074203747E+02);
    csum_ref[4]  = dcmplx(5.120438418997E+02, 5.119345900733E+02);
    csum_ref[5]  = dcmplx(5.120311521872E+02, 5.119551325550E+02);
    csum_ref[6]  = dcmplx(5.120226088809E+02, 5.119720179919E+02);
    csum_ref[7]  = dcmplx(5.120169296534E+02, 5.119861371665E+02);
    csum_ref[8]  = dcmplx(5.120131225172E+02, 5.119979364402E+02);
    csum_ref[9]  = dcmplx(5.120104767108E+02, 5.120077674092E+02);
    csum_ref[10] = dcmplx(5.120085127969E+02, 5.120159443121E+02);
    csum_ref[11] = dcmplx(5.120069224127E+02, 5.120227453670E+02);
    csum_ref[12] = dcmplx(5.120055158164E+02, 5.120284096041E+02);
    csum_ref[13] = dcmplx(5.120041820159E+02, 5.120331373793E+02);
    csum_ref[14] = dcmplx(5.120028605402E+02, 5.120370938679E+02);
    csum_ref[15] = dcmplx(5.120015223011E+02, 5.120404138831E+02);
    csum_ref[16] = dcmplx(5.120001570022E+02, 5.120432068837E+02);
    csum_ref[17] = dcmplx(5.119987650555E+02, 5.120455615860E+02);
    csum_ref[18] = dcmplx(5.119973525091E+02, 5.120475499442E+02);
    csum_ref[19] = dcmplx(5.119959279472E+02, 5.120492304629E+02);
    csum_ref[20] = dcmplx(5.119945006558E+02, 5.120506508902E+02);
    csum_ref[21] = dcmplx(5.119930795911E+02, 5.120518503782E+02);
    csum_ref[22] = dcmplx(5.119916728462E+02, 5.120528612016E+02);
    csum_ref[23] = dcmplx(5.119902874185E+02, 5.120537101195E+02);
    csum_ref[24] = dcmplx(5.119889291565E+02, 5.120544194514E+02);
    csum_ref[25] = dcmplx(5.119876028049E+02, 5.120550079284E+02);
  }

  if (*Class != 'U') {
    *verified = true;
    for (i = 1; i <= nt; i++) {
      err = dcmplx_abs(dcmplx_div(dcmplx_sub(sums[i], csum_ref[i]),
                                  csum_ref[i]));
      if (!(err <= epsilon)) {
        *verified = false;
        break;
      }
    }
  }

  if (*Class != 'U') {
    if (*verified) {
      printf(" Result verification successful\n");
    } else {
      printf(" Result verification failed\n");
    }
  }
  printf(" class = %c\n", *Class);
}


//---------------------------------------------------------------------
// Set up the OpenCL environment.
//---------------------------------------------------------------------
static void setup_opencl(int argc, char *argv[])
{
  int i;
//  size_t temp;
  cl_int ecode;
  char *source_dir = ".";  //FIXME
  int num_subs = DEFAULT_NUM_SUBS;
  int num_cus;

  if (argc > 1) source_dir = argv[1];
  if (argc > 2) num_subs = atoi(argv[2]);
  devices = (cl_device_id *)malloc(sizeof(cl_device_id) * num_subs);

  if (timers_enabled) {
    timer_clear(TIMER_OPENCL);
    timer_clear(TIMER_BUILD);
    timer_clear(TIMER_BUFFER);
    timer_clear(TIMER_RELEASE);

    timer_start(TIMER_OPENCL);
  }

  // 1. Find the default device type and get a device for the device type
  //    Then, create sub-devices from the parent device.
  device_type = CL_DEVICE_TYPE_GPU;

  cl_platform_id platform;
  ecode = clGetPlatformIDs(1, &platform, NULL);
  clu_CheckError(ecode, "clGetPlatformIDs()");

  ecode = clGetDeviceIDs(platform, device_type, 0, NULL, &num_devices);
  clu_CheckError(ecode, "clGetDeviceIDs()");

  ecode = clGetDeviceIDs(platform, device_type, num_devices, devices, NULL);
  clu_CheckError(ecode, "clGetDeviceIDs()");

  work_item_sizes[0] = work_item_sizes[1] = work_item_sizes[2] = 64;
  max_work_group_size = 64;
  max_compute_units = 22;

  // FIXME
  if (max_work_group_size > 64) {
    max_work_group_size = 64;
    int i;
    for (i = 0; i < 3; i++) {
      if (work_item_sizes[i] > 64) {
        work_item_sizes[i] = 64;
      }
    }
  }
  if (device_type == CL_DEVICE_TYPE_CPU) {
    COMPUTE_IMAP_DIM = COMPUTE_IMAP_DIM_CPU;
    EVOLVE_DIM = EVOLVE_DIM_CPU;
    TRANSPOSE2_LOCAL1_DIM = TRANSPOSE2_LOCAL1_DIM_CPU;
    TRANSPOSE2_LOCAL2_DIM = TRANSPOSE2_LOCAL2_DIM_CPU;
    TRANSPOSE2_LOCAL3_DIM = TRANSPOSE2_LOCAL3_DIM_CPU;
    TRANSPOSE2_FINISH_DIM = TRANSPOSE2_FINISH_DIM_CPU;
    TRANSPOSE_X_Z_LOCAL1_DIM = TRANSPOSE_X_Z_LOCAL1_DIM_CPU;
    TRANSPOSE_X_Z_LOCAL2_DIM = TRANSPOSE_X_Z_LOCAL2_DIM_CPU;
    TRANSPOSE_X_Z_FINISH_DIM = TRANSPOSE_X_Z_FINISH_DIM_CPU;
    TRANSPOSE_X_Y_LOCAL_DIM = TRANSPOSE_X_Y_LOCAL_DIM_CPU;
    TRANSPOSE_X_Y_FINISH_DIM = TRANSPOSE_X_Y_FINISH_DIM_CPU;
    CFFTS1_DIM = CFFTS1_DIM_CPU;
    CFFTS2_DIM = CFFTS2_DIM_CPU;
    CFFTS3_DIM = CFFTS3_DIM_CPU;
  } else {
    COMPUTE_IMAP_DIM = COMPUTE_IMAP_DIM_GPU;
    EVOLVE_DIM = EVOLVE_DIM_GPU;
    TRANSPOSE2_LOCAL1_DIM = TRANSPOSE2_LOCAL1_DIM_GPU;
    TRANSPOSE2_LOCAL2_DIM = TRANSPOSE2_LOCAL2_DIM_GPU;
    TRANSPOSE2_LOCAL3_DIM = TRANSPOSE2_LOCAL3_DIM_GPU;
    TRANSPOSE2_FINISH_DIM = TRANSPOSE2_FINISH_DIM_GPU;
    TRANSPOSE_X_Z_LOCAL1_DIM = TRANSPOSE_X_Z_LOCAL1_DIM_GPU;
    TRANSPOSE_X_Z_LOCAL2_DIM = TRANSPOSE_X_Z_LOCAL2_DIM_GPU;
    TRANSPOSE_X_Z_FINISH_DIM = TRANSPOSE_X_Z_FINISH_DIM_GPU;
    TRANSPOSE_X_Y_LOCAL_DIM = TRANSPOSE_X_Y_LOCAL_DIM_GPU;
    TRANSPOSE_X_Y_FINISH_DIM = TRANSPOSE_X_Y_FINISH_DIM_GPU;
    CFFTS1_DIM = CFFTS1_DIM_GPU;
    CFFTS2_DIM = CFFTS2_DIM_GPU;
    CFFTS3_DIM = CFFTS3_DIM_GPU;
  }

  // 2. Create a context for devices
  context = clCreateContext(NULL, 
                            num_devices,
                            devices,
                            NULL, NULL, &ecode);
  clu_CheckError(ecode, "clCreateContext()");

  // 3. Create a command queue
  cmd_queue = (cl_command_queue*)malloc(sizeof(cl_command_queue)*num_devices);
  for (i = 0; i < num_devices; i++) {
    cmd_queue[i] = clCreateCommandQueue(context, devices[i], 0, &ecode);
    clu_CheckError(ecode, "clCreateCommandQueue()");
  }

  // 4. Build the program
  if (timers_enabled) timer_start(TIMER_BUILD);
  char *source_file = "ft_kernel.cl";
  char build_option[50];
  if (device_type == CL_DEVICE_TYPE_CPU || device_type == CL_DEVICE_TYPE_ALL) {
    sprintf(build_option, "-I. -DCLASS=%d -DUSE_CPU", CLASS);
  } else if (device_type == CL_DEVICE_TYPE_GPU) {
    sprintf(build_option, "-I. -DCLASS=%d", CLASS);
  } else {
    fprintf(stderr, "Set the environment variable OPENCL_DEVICE_TYPE!\n");
    exit(EXIT_FAILURE);
  }

  program = clu_MakeProgram(context, devices, source_dir, source_file, build_option);

  if (timers_enabled) timer_stop(TIMER_BUILD);

  // 5. Create kernels
  size_t asize = sizeof(cl_kernel) * num_devices;
  k_compute_indexmap = (cl_kernel *)malloc(asize);
  k_compute_ics1 = (cl_kernel *)malloc(asize);
  k_compute_ics2 = (cl_kernel *)malloc(asize);
  k_cffts1 = (cl_kernel *)malloc(asize);
  k_cffts2 = (cl_kernel *)malloc(asize);
  k_cffts3 = (cl_kernel *)malloc(asize);
  k_evolve = (cl_kernel *)malloc(asize);
  k_checksum = (cl_kernel *)malloc(asize);
  k_transpose2_local1 = (cl_kernel *)malloc(asize);
  k_transpose2_local2 = (cl_kernel *)malloc(asize);
  k_transpose2_local3 = (cl_kernel *)malloc(asize);
  k_transpose2_finish = (cl_kernel *)malloc(asize);
  k_transpose_x_z_local1 = (cl_kernel *)malloc(asize);
  k_transpose_x_z_local2 = (cl_kernel *)malloc(asize);
  k_transpose_x_z_finish = (cl_kernel *)malloc(asize);
  k_transpose_x_y_local = (cl_kernel *)malloc(asize);
  k_transpose_x_y_finish = (cl_kernel *)malloc(asize);

  for (i = 0; i < num_devices; i++) {
    k_compute_indexmap[i] = clCreateKernel(program, "compute_indexmap", 
                                           &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_compute_ics1[i] = clCreateKernel(program,
                                   "compute_initial_conditions1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_compute_ics2[i] = clCreateKernel(program,
                                   "compute_initial_conditions2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_cffts1[i] = clCreateKernel(program, "cffts1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_cffts2[i] = clCreateKernel(program, "cffts2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_cffts3[i] = clCreateKernel(program, "cffts3", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_evolve[i] = clCreateKernel(program, "evolve", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_checksum[i] = clCreateKernel(program, "checksum", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_transpose2_local1[i] = clCreateKernel(program, 
                                            "transpose2_local1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_transpose2_local2[i] = clCreateKernel(program, 
                                            "transpose2_local2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_transpose2_local3[i] = clCreateKernel(program, 
                                            "transpose2_local3", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_transpose2_finish[i] = clCreateKernel(program, 
                                            "transpose2_finish", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_transpose_x_z_local1[i] = clCreateKernel(program, 
                                            "transpose_x_z_local1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_transpose_x_z_local2[i] = clCreateKernel(program, 
                                            "transpose_x_z_local2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_transpose_x_z_finish[i] = clCreateKernel(program, 
                                            "transpose_x_z_finish", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_transpose_x_y_local[i] = clCreateKernel(program, 
                                            "transpose_x_y_local", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_transpose_x_y_finish[i] = clCreateKernel(program, 
                                            "transpose_x_y_finish", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
  }

  if (timers_enabled) timer_stop(TIMER_OPENCL);
}


static void release_opencl()
{
  int i;

  if (timers_enabled) {
    timer_start(TIMER_OPENCL);
    timer_start(TIMER_RELEASE);
  }


  for (i = 0; i < num_devices; i++) {
    clReleaseMemObject(m_u[i]);
    clReleaseMemObject(m_dims[i]);
    clReleaseMemObject(m_xstart[i]);
    clReleaseMemObject(m_xend[i]);
    clReleaseMemObject(m_ystart[i]);
    clReleaseMemObject(m_yend[i]);
    clReleaseMemObject(m_zstart[i]);
    clReleaseMemObject(m_zend[i]);
    clReleaseMemObject(m_u0[i]);
    clReleaseMemObject(m_u1[i]);
    clReleaseMemObject(m_u2[i]);
    clReleaseMemObject(m_twiddle[i]);
    clReleaseMemObject(m_starts[i]);
    clReleaseMemObject(m_ty1[i]);
    clReleaseMemObject(m_ty2[i]);
    clReleaseMemObject(m_chk[i]);

    clReleaseKernel(k_compute_indexmap[i]);
    clReleaseKernel(k_compute_ics1[i]);
    clReleaseKernel(k_compute_ics2[i]);
    clReleaseKernel(k_cffts1[i]);
    clReleaseKernel(k_cffts2[i]);
    clReleaseKernel(k_cffts3[i]);
    clReleaseKernel(k_evolve[i]);
    clReleaseKernel(k_checksum[i]);
    clReleaseKernel(k_transpose2_local1[i]);
    clReleaseKernel(k_transpose2_local2[i]);
    clReleaseKernel(k_transpose2_local3[i]);
    clReleaseKernel(k_transpose2_finish[i]);
    clReleaseKernel(k_transpose_x_z_local1[i]);
    clReleaseKernel(k_transpose_x_z_local2[i]);
    clReleaseKernel(k_transpose_x_z_finish[i]);
    clReleaseKernel(k_transpose_x_y_local[i]);
    clReleaseKernel(k_transpose_x_y_finish[i]);

    clReleaseCommandQueue(cmd_queue[i]);
  }
  free(m_xstart);
  free(m_xend);
  free(m_ystart);
  free(m_yend);
  free(m_zstart);
  free(m_zend);
  free(m_u0);
  free(m_u1);
  free(m_u2);
  free(m_twiddle);
  free(m_starts);
  free(m_ty1);
  free(m_ty2);
  free(m_chk);

  free(k_compute_indexmap);
  free(k_compute_ics1);
  free(k_compute_ics2);
  free(k_cffts1);
  free(k_cffts2);
  free(k_cffts3);
  free(k_evolve);
  free(k_checksum);
  free(k_transpose2_local1);
  free(k_transpose2_local2);
  free(k_transpose2_local3);
  free(k_transpose2_finish);
  free(k_transpose_x_z_local1);
  free(k_transpose_x_z_local2);
  free(k_transpose_x_z_finish);
  free(k_transpose_x_y_local);
  free(k_transpose_x_y_finish);

  free(cmd_queue);

  clReleaseContext(context);
  clReleaseProgram(program);

  free(devices);

  if (timers_enabled) {
    timer_stop(TIMER_RELEASE);
    timer_stop(TIMER_OPENCL);

    double tt = timer_read(TIMER_OPENCL);
    printf(" OpenCL   : %9.4lf\n", tt);
    tt = timer_read(TIMER_BUILD);
    printf(" - Build  : %9.4lf\n", tt);
    tt = timer_read(TIMER_BUFFER);
    printf(" - Buffer : %9.4lf\n", tt);
    tt = timer_read(TIMER_RELEASE);
    printf(" - Release: %9.4lf\n", tt);
  }

  free(xstart);
  free(xend);
  free(ystart);
  free(yend);
  free(zstart);
  free(zend);
}

