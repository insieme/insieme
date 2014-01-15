//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB SP code for multiple    //
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
//       program MPSP
//---------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "header.h"
#include "print_results.h"

// OPENCL Variables
cl_device_type    device_type;
cl_device_id      p_device;
char             *device_name;
cl_device_id     *devices;
cl_uint           num_devices;
cl_context        context;
cl_command_queue *cmd_queue;
cl_program       p_program;
cl_program       *program;
size_t  work_item_sizes[3];
size_t  max_work_group_size;
cl_uint max_compute_units;

cl_int (*p_clCreateSubDevicesEXT)( cl_device_id,
                                   const cl_device_partition_property_ext *,
                                   cl_uint,
                                   cl_device_id *,
                                   cl_uint * ) = NULL;
cl_int (*p_clReleaseDeviceEXT)( cl_device_id ) = NULL;

cl_kernel* k_initialize1;
cl_kernel* k_initialize2;
cl_kernel* k_initialize3;
cl_kernel* k_initialize4;
cl_kernel* k_initialize5;
cl_kernel* k_initialize6;
cl_kernel* k_initialize7;
cl_kernel* k_initialize8;
cl_kernel* k_lhsinit;
cl_kernel* k_exact_rhs1;
cl_kernel* k_exact_rhs2;
cl_kernel* k_exact_rhs3;
cl_kernel* k_exact_rhs4;
cl_kernel* k_exact_rhs5;

cl_kernel (*k_copy_faces1)[MAXCELLS];
cl_kernel (*k_copy_faces2)[MAXCELLS];
cl_kernel (*k_copy_faces3)[MAXCELLS];
cl_kernel (*k_copy_faces4)[MAXCELLS];
cl_kernel (*k_copy_faces5)[MAXCELLS];
cl_kernel (*k_copy_faces6)[MAXCELLS];
cl_kernel (*k_compute_rhs1)[MAXCELLS];
cl_kernel (*k_compute_rhs2)[MAXCELLS];
cl_kernel (*k_compute_rhs3)[MAXCELLS];
cl_kernel (*k_compute_rhs4)[MAXCELLS];
cl_kernel (*k_compute_rhs5)[MAXCELLS];
cl_kernel (*k_compute_rhs6)[MAXCELLS];
cl_kernel (*k_txinvr)[MAXCELLS];
cl_kernel (*k_lhsx)[MAXCELLS];
cl_kernel (*k_ninvr)[MAXCELLS];
cl_kernel (*k_x_solve1)[MAXCELLS];
cl_kernel (*k_x_solve2)[MAXCELLS];
cl_kernel (*k_x_solve3)[MAXCELLS];
cl_kernel (*k_x_solve4)[MAXCELLS];
cl_kernel (*k_x_solve5)[MAXCELLS];
cl_kernel (*k_x_solve6)[MAXCELLS];
cl_kernel (*k_lhsy)[MAXCELLS];
cl_kernel (*k_pinvr)[MAXCELLS];
cl_kernel (*k_y_solve1)[MAXCELLS];
cl_kernel (*k_y_solve2)[MAXCELLS];
cl_kernel (*k_y_solve3)[MAXCELLS];
cl_kernel (*k_y_solve4)[MAXCELLS];
cl_kernel (*k_y_solve5)[MAXCELLS];
cl_kernel (*k_y_solve6)[MAXCELLS];
cl_kernel (*k_lhsz)[MAXCELLS];
cl_kernel (*k_tzetar)[MAXCELLS];
cl_kernel (*k_z_solve1)[MAXCELLS];
cl_kernel (*k_z_solve2)[MAXCELLS];
cl_kernel (*k_z_solve3)[MAXCELLS];
cl_kernel (*k_z_solve4)[MAXCELLS];
cl_kernel (*k_z_solve5)[MAXCELLS];
cl_kernel (*k_z_solve6)[MAXCELLS];
cl_kernel (*k_add)[MAXCELLS];

cl_kernel* k_error_norm;
cl_kernel* k_rhs_norm;

cl_mem* m_u;
cl_mem* m_us;
cl_mem* m_vs;
cl_mem* m_ws;
cl_mem* m_qs;
cl_mem* m_ainv;
cl_mem* m_rho_i;
cl_mem* m_speed;
cl_mem* m_square;
cl_mem* m_rhs;
cl_mem* m_forcing;
cl_mem* m_lhs;
cl_mem* m_in_buffer;
cl_mem* m_out_buffer;

cl_mem* m_ce;

int COPY_FACES1_DIM, COPY_FACES2_DIM, COPY_FACES3_DIM,
    COPY_FACES4_DIM, COPY_FACES5_DIM, COPY_FACES6_DIM,
    COMPUTE_RHS1_DIM, COMPUTE_RHS2_DIM, COMPUTE_RHS6_DIM,
    TXINVR_DIM, NINVR_DIM, X_SOLVE6_DIM,
    PINVR_DIM, Y_SOLVE6_DIM, TZETAR_DIM, Z_SOLVE6_DIM,
    ADD_DIM;

size_t copy_faces1_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces2_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces3_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces4_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces5_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces6_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs1_dim[MAX_DEVICE_NUM][MAXCELLS][3],
    compute_rhs2_dim[MAX_DEVICE_NUM][MAXCELLS][3],
    compute_rhs3_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs4_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs5_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs6_dim[MAX_DEVICE_NUM][MAXCELLS][3],
    txinvr_dim[MAX_DEVICE_NUM][MAXCELLS][3],
    lhsx_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    ninvr_dim[MAX_DEVICE_NUM][MAXCELLS][3],
    x_solve1_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve2_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve3_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve4_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve5_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve6_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    lhsy_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    pinvr_dim[MAX_DEVICE_NUM][MAXCELLS][3],
    y_solve1_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve2_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve3_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve4_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve5_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve6_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    lhsz_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    tzetar_dim[MAX_DEVICE_NUM][MAXCELLS][3],
    z_solve1_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve2_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve3_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve4_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve5_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve6_dim[MAX_DEVICE_NUM][MAXCELLS][2],
    add_dim[MAX_DEVICE_NUM][MAXCELLS][3];

size_t copy_faces1_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces2_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces3_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces4_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces5_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces6_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs1_lw[MAX_DEVICE_NUM][MAXCELLS][3],
    compute_rhs2_lw[MAX_DEVICE_NUM][MAXCELLS][3],
    compute_rhs3_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs4_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs5_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs6_lw[MAX_DEVICE_NUM][MAXCELLS][3],
    txinvr_lw[MAX_DEVICE_NUM][MAXCELLS][3],
    lhsx_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    ninvr_lw[MAX_DEVICE_NUM][MAXCELLS][3],
    x_solve1_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve2_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve3_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve4_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve5_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve6_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    lhsy_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    pinvr_lw[MAX_DEVICE_NUM][MAXCELLS][3],
    y_solve1_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve2_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve3_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve4_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve5_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve6_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    lhsz_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    tzetar_lw[MAX_DEVICE_NUM][MAXCELLS][3],
    z_solve1_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve2_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve3_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve4_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve5_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve6_lw[MAX_DEVICE_NUM][MAXCELLS][2],
    add_lw[MAX_DEVICE_NUM][MAXCELLS][3];

size_t copy_faces1_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces2_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces3_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces4_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces5_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    copy_faces6_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs1_gw[MAX_DEVICE_NUM][MAXCELLS][3],
    compute_rhs2_gw[MAX_DEVICE_NUM][MAXCELLS][3],
    compute_rhs3_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs4_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs5_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    compute_rhs6_gw[MAX_DEVICE_NUM][MAXCELLS][3],
    txinvr_gw[MAX_DEVICE_NUM][MAXCELLS][3],
    lhsx_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    ninvr_gw[MAX_DEVICE_NUM][MAXCELLS][3],
    x_solve1_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve2_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve3_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve4_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve5_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    x_solve6_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    lhsy_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    pinvr_gw[MAX_DEVICE_NUM][MAXCELLS][3],
    y_solve1_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve2_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve3_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve4_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve5_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    y_solve6_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    lhsz_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    tzetar_gw[MAX_DEVICE_NUM][MAXCELLS][3],
    z_solve1_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve2_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve3_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve4_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve5_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    z_solve6_gw[MAX_DEVICE_NUM][MAXCELLS][2],
    add_gw[MAX_DEVICE_NUM][MAXCELLS][3];

int MAX_CELL_DIM, IMAX, JMAX, KMAX, IMAXP, JMAXP, BUF_SIZE;

/* common /global/ */
int ncells, grid_points[3];

/* common /constants/ */
double ce[5][13];

/* common /partition/ */
int cell_coord[MAX_DEVICE_NUM][MAXCELLS][3],
    cell_low[MAX_DEVICE_NUM][MAXCELLS][3],
    cell_high[MAX_DEVICE_NUM][MAXCELLS][3],
    cell_size[MAX_DEVICE_NUM][MAXCELLS][3],
    predecessor[MAX_DEVICE_NUM][3],
    slice[MAX_DEVICE_NUM][MAXCELLS][3],
    grid_size[MAX_DEVICE_NUM][3],
    successor[MAX_DEVICE_NUM][3],
    cell_start[MAX_DEVICE_NUM][MAXCELLS][3],
    cell_end[MAX_DEVICE_NUM][MAXCELLS][3];

/* common /box/ */
int west_size[MAX_DEVICE_NUM],
    east_size[MAX_DEVICE_NUM],
    bottom_size[MAX_DEVICE_NUM],
    top_size[MAX_DEVICE_NUM],
    north_size[MAX_DEVICE_NUM],
    south_size[MAX_DEVICE_NUM],
    start_send_west[MAX_DEVICE_NUM], 
    start_send_east[MAX_DEVICE_NUM],
    start_send_south[MAX_DEVICE_NUM],
    start_send_north[MAX_DEVICE_NUM],
    start_send_bottom[MAX_DEVICE_NUM],
    start_send_top[MAX_DEVICE_NUM],
    start_recv_west[MAX_DEVICE_NUM],
    start_recv_east[MAX_DEVICE_NUM],
    start_recv_south[MAX_DEVICE_NUM],
    start_recv_north[MAX_DEVICE_NUM],
    start_recv_bottom[MAX_DEVICE_NUM],
    start_recv_top[MAX_DEVICE_NUM];

/* common /tflags/ */
logical timeron;

static void setup_opencl(int argc, char **argv);
static void release_opencl();

#define DEFAULT_NUM_SUBS    40


#ifdef CLUSTER
int __main(int argc, char** argv)
#else
int main(int argc, char **argv)
#endif
{
  int i, niter, step;
  double mflops, t, tmax;
  logical verified;
  char class;
  double tsum[t_last+2], t1[t_last+2],
         tming[t_last+2], tmaxg[t_last+2];
  char *t_recs[t_last+2] = {
       "total", "rhs", "xsolve", "ysolve", "zsolve", 
       "bpack", "exch", "xcomm", "ycomm", "zcomm",
       " totcomp", " totcomm" };

  //---------------------------------------------------------------------
  // Root node reads input file (if it exists) else takes
  // defaults from parameters
  //---------------------------------------------------------------------
  printf("\n\n NAS Parallel Benchmarks (NPB3.3-OCL-MD) - SP Benchmark\n\n");

  FILE *fp;
  fp = fopen("timer.flag", "r");
  timeron = false;
  if (fp != NULL) {
    timeron = true;
    fclose(fp);
  }

  if ((fp = fopen("inputsp.data", "r")) != NULL) {
    int result;
    printf(" Reading from input file inputsp.data\n");
    result = fscanf(fp, "%d", &niter);
    while (fgetc(fp) != '\n');
    result = fscanf(fp, "%*f");
    while (fgetc(fp) != '\n');
    result = fscanf(fp, "%d%d%d", &grid_points[0], &grid_points[1], 
                                  &grid_points[2]);
    fclose(fp);
  } else {
    printf(" No input file inputsp.data. Using compiled defaults\n");
    niter = NITER_DEFAULT;
    grid_points[0] = PROBLEM_SIZE;
    grid_points[1] = PROBLEM_SIZE;
    grid_points[2] = PROBLEM_SIZE;
  }

  setup_opencl(argc, argv);

  printf(" Size: %4dx%4dx%4d\n", 
      grid_points[0], grid_points[1], grid_points[2]);
  printf(" Iterations: %4d", niter);
  if (num_devices != MAXCELLS*MAXCELLS) 
    printf(" WARNING: compiled for %5d devices \n", MAXCELLS*MAXCELLS);
  printf(" Number of active devices: %5d\n\n", num_devices);

  make_set();

  for (i = 0; i < t_last; i++) {
    timer_clear(i);
  }

  set_constants();

  initialize();

  lhsinit();

  exact_rhs();

  compute_buffer_size(5);

  set_kernel_args();

  //---------------------------------------------------------------------
  // do one time step to touch all code, and reinitialize
  //---------------------------------------------------------------------
  adi();

  initialize();

  //---------------------------------------------------------------------
  // Synchronize before placing time stamp
  //---------------------------------------------------------------------
  for (i = 0; i < t_last; i++) {
    timer_clear(i);
  }

  timer_clear(0);
  timer_start(0);

  for (step = 1; step <= niter; step++) {

    if ((step % 20) == 0 || step == 1) {
      printf(" Time step %4d\n", step);
    }

    adi();

  }

  timer_stop(0);
  t = timer_read(0);

  verify(niter, &class, &verified);

  tmax = t;

  if( tmax != 0.0 ) {
    mflops = (881.174*(double)( PROBLEM_SIZE*PROBLEM_SIZE*PROBLEM_SIZE )
             -4683.91*(double)( PROBLEM_SIZE*PROBLEM_SIZE )
             +11484.5*(double)( PROBLEM_SIZE )
             -19272.4) * (double)( niter ) / (tmax*1000000.0);
  } else {
    mflops = 0.0;
  }

  c_print_results("SP", class, grid_points[0], 
      grid_points[1], grid_points[2], niter,
      tmax, mflops, "          floating point", 
      verified, NPBVERSION,COMPILETIME, CS1, CS2, CS3, CS4, CS5, 
      CS6, CS7, clu_GetDeviceTypeName(device_type), device_name, num_devices);

  if (timeron) {
/*
    for (i = 0; i < t_last; i++) {
      t1[i] = timer_read(i);
    }
    t1[t_xsolve] = t1[t_xsolve] - t1[t_xcomm];
    t1[t_ysolve] = t1[t_ysolve] - t1[t_ycomm];
    t1[t_zsolve] = t1[t_zsolve] - t1[t_zcomm];
    t1[t_last+2] = t1[t_xcomm]+t1[t_ycomm]+t1[t_zcomm]+t1[t_exch];
    t1[t_last+1] = t1[t_total]  - t1[t_last+2];

    MPI_Reduce(&t1, tsum,  t_last+2, dp_type, MPI_SUM, 0, comm_setup);
    MPI_Reduce(&t1, tming, t_last+2, dp_type, MPI_MIN, 0, comm_setup);
    MPI_Reduce(&t1, tmaxg, t_last+2, dp_type, MPI_MAX, 0, comm_setup);

    if (node == 0) {
      printf(" nprocs =%6d           minimum     maximum     average\n",
          total_nodes);
      for (i = 0; i < t_last+2; i++) {
        tsum[i] = tsum[i] / total_nodes;
          printf(" timer %2d(%8s) :  %10.4f  %10.4f  %10.4f\n",
              i+1, t_recs[i], tming[i], tmaxg[i], tsum[i]);
      }
    }
*/
  }

  release_opencl();

  return 0;
}


//---------------------------------------------------------------------
// Set up the OpenCL environment.
//---------------------------------------------------------------------
static void setup_opencl(int argc, char **argv)
{
  int i, c;
//  size_t temp;
  cl_int ecode = 0;
  char *source_dir = ".";  //FIXME
  int num_subs = DEFAULT_NUM_SUBS;
  int num_cus;
  int sqrt_num_devices;

  if (argc > 1) source_dir = argv[1];

  devices = (cl_device_id *)malloc(sizeof(cl_device_id) * num_subs);

  if (timeron) {
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

  work_item_sizes[0] = work_item_sizes[1] = work_item_sizes[2] = 1024;
  max_work_group_size = 1024;
  max_compute_units = 22;

  sqrt_num_devices = (int)(sqrt((double)(num_devices) + 0.00001));
  if (num_devices != sqrt_num_devices * sqrt_num_devices) {
    fprintf(stderr, "Number of devices is not a square of some integer\n");
    exit(EXIT_FAILURE);
  }

  ncells = (int)(sqrt((double)(num_devices) + 0.00001));
  MAX_CELL_DIM = ((PROBLEM_SIZE/ncells)+1);
  IMAX = MAX_CELL_DIM;
  JMAX = MAX_CELL_DIM;
  KMAX = MAX_CELL_DIM;
  IMAXP = (IMAX/2*2+1);
  JMAXP = (JMAX/2*2+1);
  //---------------------------------------------------------------------
  // +1 at end to avoid zero length arrays for 1 node
  //---------------------------------------------------------------------
  BUF_SIZE = (MAX_CELL_DIM*MAX_CELL_DIM*(MAXCELLS-1)*60*2+1);


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

  // 2. Create a context for devices
  context = clCreateContext(NULL, 
                            num_devices,
                            devices,
                            NULL, NULL, &ecode);
  clu_CheckError(ecode, "clCreateContext()");

  // 3. Create a command queue
  cmd_queue = (cl_command_queue*)malloc(sizeof(cl_command_queue)*num_devices*3);
  for (i = 0; i < num_devices * 2; i++) {
    cmd_queue[i] = clCreateCommandQueue(context, devices[i / 2], 0, &ecode);
    clu_CheckError(ecode, "clCreateCommandQueue()");
  }

  // 4. Build the program
  if (timeron) timer_start(TIMER_BUILD);
  char *source_file = "sp_kernel.cl";
  char build_option[200];
  if (device_type == CL_DEVICE_TYPE_CPU) {
    sprintf(build_option, "-I. -DCLASS=%d -DUSE_CPU -DMAX_CELL_DIM=%d -DIMAX=%d -DJMAX=%d -DKMAX=%d -DIMAXP=%d -DJMAXP=%d", CLASS, MAX_CELL_DIM, IMAX, JMAX, KMAX, IMAXP, JMAXP);
  } else {
    sprintf(build_option, "-I. -DCLASS=%d -DUSE_GPU -DMAX_CELL_DIM=%d -DIMAX=%d -DJMAX=%d -DKMAX=%d -DIMAXP=%d -DJMAXP=%d", CLASS, MAX_CELL_DIM, IMAX, JMAX, KMAX, IMAXP, JMAXP);
  }

  p_program = clu_MakeProgram(context, devices, source_dir, source_file, build_option);

  program = (cl_program *)malloc(sizeof(cl_program) * num_devices);
  for (i = 0; i < num_devices; i++) {
    program[i] = p_program;
  }
  if (timeron) timer_stop(TIMER_BUILD);

  // 5. Create kernels
  size_t asize = sizeof(cl_kernel) * num_devices;
  k_initialize1 = (cl_kernel *)malloc(asize);
  k_initialize2 = (cl_kernel *)malloc(asize);
  k_initialize3 = (cl_kernel *)malloc(asize);
  k_initialize4 = (cl_kernel *)malloc(asize);
  k_initialize5 = (cl_kernel *)malloc(asize);
  k_initialize6 = (cl_kernel *)malloc(asize);
  k_initialize7 = (cl_kernel *)malloc(asize);
  k_initialize8 = (cl_kernel *)malloc(asize);
  k_lhsinit = (cl_kernel *)malloc(asize);
  k_exact_rhs1 = (cl_kernel *)malloc(asize);
  k_exact_rhs2 = (cl_kernel *)malloc(asize);
  k_exact_rhs3 = (cl_kernel *)malloc(asize);
  k_exact_rhs4 = (cl_kernel *)malloc(asize);
  k_exact_rhs5 = (cl_kernel *)malloc(asize);
  k_copy_faces1 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_copy_faces2 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_copy_faces3 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_copy_faces4 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_copy_faces5 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_copy_faces6 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_compute_rhs1 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_compute_rhs2 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_compute_rhs3 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_compute_rhs4 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_compute_rhs5 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_compute_rhs6 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_txinvr = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_lhsx = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_ninvr = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_x_solve1 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_x_solve2 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_x_solve3 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_x_solve4 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_x_solve5 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_x_solve6 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_lhsy = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_pinvr = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_y_solve1 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_y_solve2 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_y_solve3 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_y_solve4 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_y_solve5 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_y_solve6 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_lhsz = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_tzetar = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_z_solve1 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_z_solve2 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_z_solve3 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_z_solve4 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_z_solve5 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_z_solve6 = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_add = (cl_kernel (*)[MAXCELLS])malloc(asize*MAXCELLS);
  k_error_norm = (cl_kernel *)malloc(asize);
  k_rhs_norm = (cl_kernel *)malloc(asize);

  for (i = 0; i < num_devices; i++) {
    k_initialize1[i] = clCreateKernel(program[i], "initialize1", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for initialize1");

    k_initialize2[i] = clCreateKernel(program[i], "initialize2", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for initialize2");

    k_initialize3[i] = clCreateKernel(program[i], "initialize3", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for initialize3");

    k_initialize4[i] = clCreateKernel(program[i], "initialize4", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for initialize4");

    k_initialize5[i] = clCreateKernel(program[i], "initialize5", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for initialize5");

    k_initialize6[i] = clCreateKernel(program[i], "initialize6", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for initialize6");

    k_initialize7[i] = clCreateKernel(program[i], "initialize7", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for initialize7");

    k_initialize8[i] = clCreateKernel(program[i], "initialize8", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for initialize8");

    k_lhsinit[i] = clCreateKernel(program[i], "lhsinit", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for lhsinit");

    k_exact_rhs1[i] = clCreateKernel(program[i], "exact_rhs1", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for exact_rhs1");

    k_exact_rhs2[i] = clCreateKernel(program[i], "exact_rhs2", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for exact_rhs2");

    k_exact_rhs3[i] = clCreateKernel(program[i], "exact_rhs3", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for exact_rhs3");

    k_exact_rhs4[i] = clCreateKernel(program[i], "exact_rhs4", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for exact_rhs4");

    k_exact_rhs5[i] = clCreateKernel(program[i], "exact_rhs5", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for exact_rhs5");

    for (c = 0; c < MAXCELLS; c++) {
      k_copy_faces1[i][c] = clCreateKernel(program[i], "copy_faces1", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for copy_faces1");

      k_copy_faces2[i][c] = clCreateKernel(program[i], "copy_faces2", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for copy_faces2");

      k_copy_faces3[i][c] = clCreateKernel(program[i], "copy_faces3", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for copy_faces3");

      k_copy_faces4[i][c] = clCreateKernel(program[i], "copy_faces4", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for copy_faces4");

      k_copy_faces5[i][c] = clCreateKernel(program[i], "copy_faces5", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for copy_faces5");

      k_copy_faces6[i][c] = clCreateKernel(program[i], "copy_faces6", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for copy_faces6");

      k_compute_rhs1[i][c] = clCreateKernel(program[i], "compute_rhs1", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for compute_rhs1");

      k_compute_rhs2[i][c] = clCreateKernel(program[i], "compute_rhs2", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for compute_rhs2");

      k_compute_rhs3[i][c] = clCreateKernel(program[i], "compute_rhs3", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for compute_rhs3");

      k_compute_rhs4[i][c] = clCreateKernel(program[i], "compute_rhs4", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for compute_rhs4");

      k_compute_rhs5[i][c] = clCreateKernel(program[i], "compute_rhs5", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for compute_rhs5");

      k_compute_rhs6[i][c] = clCreateKernel(program[i], "compute_rhs6", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for compute_rhs6");

      k_txinvr[i][c] = clCreateKernel(program[i], "txinvr", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for txinvr");

      k_lhsx[i][c] = clCreateKernel(program[i], "lhsx", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for lhsx");

      k_ninvr[i][c] = clCreateKernel(program[i], "ninvr", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for ninvr");

      k_x_solve1[i][c] = clCreateKernel(program[i], "x_solve1", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for x_solve1");

      k_x_solve2[i][c] = clCreateKernel(program[i], "x_solve2", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for x_solve2");

      k_x_solve3[i][c] = clCreateKernel(program[i], "x_solve3", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for x_solve3");

      k_x_solve4[i][c] = clCreateKernel(program[i], "x_solve4", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for x_solve4");

      k_x_solve5[i][c] = clCreateKernel(program[i], "x_solve5", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for x_solve5");

      k_x_solve6[i][c] = clCreateKernel(program[i], "x_solve6", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for x_solve6");

      k_lhsy[i][c] = clCreateKernel(program[i], "lhsy", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for lhsy");

      k_pinvr[i][c] = clCreateKernel(program[i], "pinvr", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for pinvr");

      k_y_solve1[i][c] = clCreateKernel(program[i], "y_solve1", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for y_solve1");

      k_y_solve2[i][c] = clCreateKernel(program[i], "y_solve2", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for y_solve2");

      k_y_solve3[i][c] = clCreateKernel(program[i], "y_solve3", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for y_solve3");

      k_y_solve4[i][c] = clCreateKernel(program[i], "y_solve4", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for y_solve4");

      k_y_solve5[i][c] = clCreateKernel(program[i], "y_solve5", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for y_solve5");

      k_y_solve6[i][c] = clCreateKernel(program[i], "y_solve6", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for y_solve6");

      k_lhsz[i][c] = clCreateKernel(program[i], "lhsz", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for lhsz");

      k_tzetar[i][c] = clCreateKernel(program[i], "tzetar", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for tzetar");

      k_z_solve1[i][c] = clCreateKernel(program[i], "z_solve1", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for z_solve1");

      k_z_solve2[i][c] = clCreateKernel(program[i], "z_solve2", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for z_solve2");

      k_z_solve3[i][c] = clCreateKernel(program[i], "z_solve3", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for z_solve3");

      k_z_solve4[i][c] = clCreateKernel(program[i], "z_solve4", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for z_solve4");

      k_z_solve5[i][c] = clCreateKernel(program[i], "z_solve5", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for z_solve5");

      k_z_solve6[i][c] = clCreateKernel(program[i], "z_solve6", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for z_solve6");

      k_add[i][c] = clCreateKernel(program[i], "add", &ecode);
      clu_CheckError(ecode, "clCreateKernel() for add");
    }

    k_error_norm[i] = clCreateKernel(program[i], "error_norm", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for error_norm");

    k_rhs_norm[i] = clCreateKernel(program[i], "rhs_norm", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for rhs_norm");
  }

  // 6. Create buffers
  if (timeron) timer_start(TIMER_BUFFER);

  asize = sizeof(cl_mem) * num_devices;

  m_u = (cl_mem *)malloc(asize);
  m_us = (cl_mem *)malloc(asize);
  m_vs = (cl_mem *)malloc(asize);
  m_ws = (cl_mem *)malloc(asize);
  m_qs = (cl_mem *)malloc(asize);
  m_ainv = (cl_mem *)malloc(asize);
  m_rho_i = (cl_mem *)malloc(asize);
  m_speed = (cl_mem *)malloc(asize);
  m_square = (cl_mem *)malloc(asize);
  m_rhs = (cl_mem *)malloc(asize);
  m_forcing = (cl_mem *)malloc(asize);
  m_lhs = (cl_mem *)malloc(asize);
  m_in_buffer = (cl_mem *)malloc(asize);
  m_out_buffer = (cl_mem *)malloc(asize);

  m_ce = (cl_mem *)malloc(asize);

  for (i = 0; i < num_devices; i++) {
    m_u[i] = clCreateBuffer(context,
                            CL_MEM_READ_WRITE,
                            sizeof(double)*MAXCELLS*(KMAX+4)*(JMAXP+4)*(IMAXP+4)*5,
                            NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_u");

    m_us[i] = clCreateBuffer(context,
                             CL_MEM_READ_WRITE,
                             sizeof(double)*MAXCELLS*(KMAX+2)*(JMAX+2)*(IMAX+2),
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_us");

    m_vs[i] = clCreateBuffer(context,
                             CL_MEM_READ_WRITE,
                             sizeof(double)*MAXCELLS*(KMAX+2)*(JMAX+2)*(IMAX+2),
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_vs");

    m_ws[i] = clCreateBuffer(context,
                             CL_MEM_READ_WRITE,
                             sizeof(double)*MAXCELLS*(KMAX+2)*(JMAX+2)*(IMAX+2),
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_ws");

    m_qs[i] = clCreateBuffer(context,
                             CL_MEM_READ_WRITE,
                             sizeof(double)*MAXCELLS*(KMAX+2)*(JMAX+2)*(IMAX+2),
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_qs");

    m_ainv[i] = clCreateBuffer(context,
                               CL_MEM_READ_WRITE,
                               sizeof(double)*MAXCELLS*(KMAX+2)*(JMAX+2)*(IMAX+2),
                               NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_ainv");

    m_rho_i[i] = clCreateBuffer(context,
                                CL_MEM_READ_WRITE,
                                sizeof(double)*MAXCELLS*(KMAX+2)*(JMAX+2)*(IMAX+2),
                                NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_rho_i");

    m_speed[i] = clCreateBuffer(context,
                                CL_MEM_READ_WRITE,
                                sizeof(double)*MAXCELLS*(KMAX+2)*(JMAX+2)*(IMAX+2),
                                NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_speed");

    m_square[i] = clCreateBuffer(context,
                                 CL_MEM_READ_WRITE,
                                 sizeof(double)*MAXCELLS*(KMAX+2)*(JMAX+2)*(IMAX+2),
                                 NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_square");

    m_rhs[i] = clCreateBuffer(context,
                              CL_MEM_READ_WRITE,
                              sizeof(double)*MAXCELLS*KMAX*JMAXP*IMAXP*5,
                              NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_rhs");

    m_forcing[i] = clCreateBuffer(context,
                                  CL_MEM_READ_WRITE,
                                  sizeof(double)*MAXCELLS*KMAX*JMAXP*IMAXP*5,
                                  NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_forcing");

    m_lhs[i] = clCreateBuffer(context,
                              CL_MEM_READ_WRITE,
                              sizeof(double)*MAXCELLS*KMAX*JMAXP*IMAXP*15,
                              NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_lhs");

    m_in_buffer[i] = clCreateBuffer(context,
                                    CL_MEM_READ_WRITE,
                                    sizeof(double)*BUF_SIZE,
                                    NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_in_buffer");

    m_out_buffer[i] = clCreateBuffer(context,
                                     CL_MEM_READ_WRITE,
                                     sizeof(double)*BUF_SIZE,
                                     NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_out_buffer");

    m_ce[i] = clCreateBuffer(context,
                             CL_MEM_READ_ONLY,
                             sizeof(double)*5*13,
                             NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_ce");
  }

  if (timeron) timer_stop(TIMER_BUFFER);

  if (timeron) timer_stop(TIMER_OPENCL);
}


static void release_opencl()
{
  int i, c;

  if (timeron) {
    timer_start(TIMER_OPENCL);
    timer_start(TIMER_RELEASE);
  }

  for (i = 0; i < num_devices; i++) {
    clReleaseMemObject(m_u[i]);
    clReleaseMemObject(m_us[i]);
    clReleaseMemObject(m_vs[i]);
    clReleaseMemObject(m_ws[i]);
    clReleaseMemObject(m_qs[i]);
    clReleaseMemObject(m_ainv[i]);
    clReleaseMemObject(m_rho_i[i]);
    clReleaseMemObject(m_speed[i]);
    clReleaseMemObject(m_square[i]);
    clReleaseMemObject(m_rhs[i]);
    clReleaseMemObject(m_forcing[i]);
    clReleaseMemObject(m_lhs[i]);
    clReleaseMemObject(m_in_buffer[i]);
    clReleaseMemObject(m_out_buffer[i]);
    clReleaseMemObject(m_ce[i]);

    clReleaseKernel(k_initialize1[i]);
    clReleaseKernel(k_initialize2[i]);
    clReleaseKernel(k_initialize3[i]);
    clReleaseKernel(k_initialize4[i]);
    clReleaseKernel(k_initialize5[i]);
    clReleaseKernel(k_initialize6[i]);
    clReleaseKernel(k_initialize7[i]);
    clReleaseKernel(k_initialize8[i]);
    clReleaseKernel(k_lhsinit[i]);
    clReleaseKernel(k_exact_rhs1[i]);
    clReleaseKernel(k_exact_rhs2[i]);
    clReleaseKernel(k_exact_rhs3[i]);
    clReleaseKernel(k_exact_rhs4[i]);
    clReleaseKernel(k_exact_rhs5[i]);

    for (c = 0; c < MAXCELLS; c++) {
      clReleaseKernel(k_copy_faces1[i][c]);
      clReleaseKernel(k_copy_faces2[i][c]);
      clReleaseKernel(k_copy_faces3[i][c]);
      clReleaseKernel(k_copy_faces4[i][c]);
      clReleaseKernel(k_copy_faces5[i][c]);
      clReleaseKernel(k_copy_faces6[i][c]);
      clReleaseKernel(k_compute_rhs1[i][c]);
      clReleaseKernel(k_compute_rhs2[i][c]);
      clReleaseKernel(k_compute_rhs3[i][c]);
      clReleaseKernel(k_compute_rhs4[i][c]);
      clReleaseKernel(k_compute_rhs5[i][c]);
      clReleaseKernel(k_compute_rhs6[i][c]);
      clReleaseKernel(k_txinvr[i][c]);
      clReleaseKernel(k_lhsx[i][c]);
      clReleaseKernel(k_ninvr[i][c]);
      clReleaseKernel(k_x_solve1[i][c]);
      clReleaseKernel(k_x_solve2[i][c]);
      clReleaseKernel(k_x_solve3[i][c]);
      clReleaseKernel(k_x_solve4[i][c]);
      clReleaseKernel(k_x_solve5[i][c]);
      clReleaseKernel(k_x_solve6[i][c]);
      clReleaseKernel(k_lhsy[i][c]);
      clReleaseKernel(k_pinvr[i][c]);
      clReleaseKernel(k_y_solve1[i][c]);
      clReleaseKernel(k_y_solve2[i][c]);
      clReleaseKernel(k_y_solve3[i][c]);
      clReleaseKernel(k_y_solve4[i][c]);
      clReleaseKernel(k_y_solve5[i][c]);
      clReleaseKernel(k_y_solve6[i][c]);
      clReleaseKernel(k_lhsz[i][c]);
      clReleaseKernel(k_tzetar[i][c]);
      clReleaseKernel(k_z_solve1[i][c]);
      clReleaseKernel(k_z_solve2[i][c]);
      clReleaseKernel(k_z_solve3[i][c]);
      clReleaseKernel(k_z_solve4[i][c]);
      clReleaseKernel(k_z_solve5[i][c]);
      clReleaseKernel(k_z_solve6[i][c]);
      clReleaseKernel(k_add[i][c]);
    }

    clReleaseKernel(k_error_norm[i]);
    clReleaseKernel(k_rhs_norm[i]);

    clReleaseProgram(program[i]);
  }
  for (i = 0; i < num_devices * 2; i++) {
    clReleaseCommandQueue(cmd_queue[i]);
  }
  free(m_u);
  free(m_us);
  free(m_vs);
  free(m_ws);
  free(m_qs);
  free(m_ainv);
  free(m_rho_i);
  free(m_speed);
  free(m_square);
  free(m_rhs);
  free(m_forcing);
  free(m_lhs);
  free(m_in_buffer);
  free(m_out_buffer);
  free(m_ce);

  free(k_initialize1);
  free(k_initialize2);
  free(k_initialize3);
  free(k_initialize4);
  free(k_initialize5);
  free(k_initialize6);
  free(k_initialize7);
  free(k_initialize8);
  free(k_lhsinit);
  free(k_exact_rhs1);
  free(k_exact_rhs2);
  free(k_exact_rhs3);
  free(k_exact_rhs4);
  free(k_exact_rhs5);
  free(k_copy_faces1);
  free(k_copy_faces2);
  free(k_copy_faces3);
  free(k_copy_faces4);
  free(k_copy_faces5);
  free(k_copy_faces6);
  free(k_compute_rhs1);
  free(k_compute_rhs2);
  free(k_compute_rhs3);
  free(k_compute_rhs4);
  free(k_compute_rhs5);
  free(k_compute_rhs6);
  free(k_txinvr);
  free(k_lhsx);
  free(k_ninvr);
  free(k_x_solve1);
  free(k_x_solve2);
  free(k_x_solve3);
  free(k_x_solve4);
  free(k_x_solve5);
  free(k_x_solve6);
  free(k_lhsy);
  free(k_pinvr);
  free(k_y_solve1);
  free(k_y_solve2);
  free(k_y_solve3);
  free(k_y_solve4);
  free(k_y_solve5);
  free(k_y_solve6);
  free(k_lhsz);
  free(k_tzetar);
  free(k_z_solve1);
  free(k_z_solve2);
  free(k_z_solve3);
  free(k_z_solve4);
  free(k_z_solve5);
  free(k_z_solve6);
  free(k_add);
  free(k_error_norm);
  free(k_rhs_norm);

  free(cmd_queue);
  free(program);

  clReleaseContext(context);

  free(devices);

  if (timeron) {
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
}

