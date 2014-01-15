//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB BT code for multiple    //
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
//       program BT
//---------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>

#include "header.h"
#include "timers.h"
#include "print_results.h"

#include "bt_dim.h"

//---------------------------------------------------------------------
// OPENCL Variables
//---------------------------------------------------------------------
cl_device_type    device_type;
cl_device_id      p_device;
char             *device_name;
cl_device_id     *devices;
cl_uint           num_devices;
cl_context        context;
cl_command_queue *cmd_queue;
cl_program       program;
cl_program       *p_initialize;
cl_program       *p_exact_rhs;
cl_program       *p_adi;
cl_program       *p_solve;
cl_program       *p_error;
size_t  work_item_sizes[3];
size_t  max_work_group_size;
cl_uint max_compute_units;

cl_kernel *k_cf_out1;
cl_kernel *k_cf_out2;
cl_kernel *k_cf_out3;
cl_kernel *k_cf_in1;
cl_kernel *k_cf_in2;
cl_kernel *k_cf_in3;
cl_kernel *k_compute_rhs1;
cl_kernel *k_compute_rhs2;
cl_kernel *k_compute_rhs3;
cl_kernel *k_compute_rhs4;
cl_kernel *k_compute_rhs5;
cl_kernel *k_compute_rhs6;
cl_kernel *k_x_usi;
cl_kernel *k_x_ssi;
cl_kernel *k_x_sbi;
cl_kernel *k_x_ubi;
cl_kernel *k_x_bs;
cl_kernel *k_x_sc;
cl_kernel *k_y_usi;
cl_kernel *k_y_ssi;
cl_kernel *k_y_sbi;
cl_kernel *k_y_ubi;
cl_kernel *k_y_bs;
cl_kernel *k_y_sc;
cl_kernel *k_z_usi;
cl_kernel *k_z_ssi;
cl_kernel *k_z_sbi;
cl_kernel *k_z_ubi;
cl_kernel *k_z_bs;
cl_kernel *k_z_sc;
cl_kernel *k_add;

cl_mem *m_cell_coord;
cl_mem *m_cell_low;
cl_mem *m_cell_high;
cl_mem *m_cell_size;
cl_mem *m_slice;
cl_mem *m_start;
cl_mem *m_end;
cl_mem *m_predecessor;
cl_mem *m_successor;
cl_mem *m_start_send;
cl_mem *m_start_recv;
cl_mem *m_p0_offset;
cl_mem *m_p1_offset;
cl_mem *m_p2_offset;
cl_mem *m_p3_offset;
cl_mem *m_p4_offset;
cl_mem *m_p5_offset;

cl_mem *m_ce;
cl_mem *m_us;
cl_mem *m_vs;
cl_mem *m_ws;
cl_mem *m_qs;
cl_mem *m_rho_i;
cl_mem *m_square;
cl_mem *m_forcing;
cl_mem *m_u;
cl_mem *m_rhs;
cl_mem *m_lhsc;
cl_mem *m_backsub_info;
cl_mem *m_in_buffer;
cl_mem *m_out_buffer;

cl_mem *m_lhsa;
cl_mem *m_lhsb;
cl_mem *m_fjac;
cl_mem *m_njac;
cl_mem *m_utmp;

int COPY_FACES_OUT1_DIM, COPY_FACES_OUT2_DIM, COPY_FACES_OUT3_DIM;
int COPY_FACES_IN1_DIM, COPY_FACES_IN2_DIM, COPY_FACES_IN3_DIM;
int COMPUTE_RHS1_DIM, COMPUTE_RHS2_DIM, COMPUTE_RHS6_DIM;
int X_SOLVE_DIM, Y_SOLVE_DIM, Z_SOLVE_DIM;
int ADD_DIM;

static void setup_opencl(int argc, char *argv[]);
static void release_opencl();
static int isqrt(int i);
//---------------------------------------------------------------------

//---------------------------------------------------------------------
int MAXCELLS;
int MAX_CELL_DIM;
int IMAX;
int JMAX;
int KMAX;
int BUF_SIZE;

/* common /global/ */
int ncells, grid_points[3];

/* common /constants/ */
double tx1, tx2, tx3, ty1, ty2, ty3, tz1, tz2, tz3, 
       dx1, dx2, dx3, dx4, dx5, dy1, dy2, dy3, dy4, 
       dy5, dz1, dz2, dz3, dz4, dz5, dssp, dt, 
       ce[5][13], dxmax, dymax, dzmax, xxcon1, xxcon2, 
       xxcon3, xxcon4, xxcon5, dx1tx1, dx2tx1, dx3tx1,
       dx4tx1, dx5tx1, yycon1, yycon2, yycon3, yycon4,
       yycon5, dy1ty1, dy2ty1, dy3ty1, dy4ty1, dy5ty1,
       zzcon1, zzcon2, zzcon3, zzcon4, zzcon5, dz1tz1, 
       dz2tz1, dz3tz1, dz4tz1, dz5tz1, dnxm1, dnym1, 
       dnzm1, c1c2, c1c5, c3c4, c1345, conz1, c1, c2, 
       c3, c4, c5, c4dssp, c5dssp, dtdssp, dttx1, bt,
       dttx2, dtty1, dtty2, dttz1, dttz2, c2dttx1, 
       c2dtty1, c2dttz1, comz1, comz4, comz5, comz6, 
       c3c4tx3, c3c4ty3, c3c4tz3, c2iv, con43, con16;

/* common /partition/ */
int *g_cell_coord;
int *g_cell_low;
int *g_cell_high;
int *g_cell_size;
int *g_slice;
int *g_start;
int *g_end;
int (*predecessor)[3];
int (*successor)[3];
int (*max_cell_size)[3];

/* common /box/ */
int (*box_size)[NUM_DIR];
int (*start_send)[NUM_DIR];
int (*start_recv)[NUM_DIR];

int *g_p0_offset;
int *g_p1_offset;
int *g_p2_offset;
int *g_p3_offset;
int *g_p4_offset;
int *g_p5_offset;

/* common /tflags/ */
logical timeron;
//---------------------------------------------------------------------

#ifdef CLUSTER
int __main(int argc, char** argv)
#else
int main(int argc, char *argv[])
#endif
{
  int i, niter, step, c;
  double navg, mflops, n3;

  double t;
  logical verified;
  char Class;
  double t1[t_last+2];
  char *t_recs[t_last+2] = {
    "total", "i/o", "rhs", "xsolve", "ysolve", "zsolve", 
    "bpack", "exch", "xcomm", "ycomm", "zcomm",
    " totcomp", " totcomm" };

  FILE *fp;
  fp = fopen("timer.flag", "r");
  timeron = false;
  if (fp != NULL) {
    timeron = true;
    fclose(fp);
  }

  setup_opencl(argc, argv);

  printf("\n\n NAS Parallel Benchmarks (NPB3.3-OCL-MD) - BT Benchmark\n\n");

  if ((fp = fopen("inputbt.data", "r")) != NULL) {
    int result;
    printf(" Reading from input file inputbt.data\n");
    result = fscanf(fp, "%d", &niter);
    while (fgetc(fp) != '\n');
    result = fscanf(fp, "%lf", &dt);
    while (fgetc(fp) != '\n');
    result = fscanf(fp, "%d%d%d\n", 
        &grid_points[0], &grid_points[1], &grid_points[2]);
    fclose(fp);
  } else {
    printf(" No input file inputbt.data. Using compiled defaults\n");
    niter = NITER_DEFAULT;
    dt    = DT_DEFAULT;
    grid_points[0] = PROBLEM_SIZE;
    grid_points[1] = PROBLEM_SIZE;
    grid_points[2] = PROBLEM_SIZE;
  }

  printf(" Size: %4dx%4dx%4d\n",
      grid_points[0], grid_points[1], grid_points[2]);
  printf(" Iterations: %4d    dt: %11.7f\n\n", niter, dt);

  make_set();

  int (*cell_size)[MAXCELLS][3] = (int (*)[MAXCELLS][3])g_cell_size;
  for (i = 0; i < num_devices; i++) {
    for (c = 0; c < MAXCELLS; c++) {
      if ( (cell_size[i][c][0] > IMAX) ||
           (cell_size[i][c][1] > JMAX) ||
           (cell_size[i][c][2] > KMAX) ) {
        printf("device=%d c=%d, cell_size-(%d, %d, %d)\n",
            i, c, cell_size[i][c][0], cell_size[i][c][1], cell_size[i][c][2]);
        printf(" Problem size too big for compiled array sizes\n");

        return 0;
      }
    }
  }

  for (i = 0; i < t_last; i++) {
    timer_clear(i);
  }

  set_constants();

  initialize();

  lhsinit();

  exact_rhs();

  compute_buffer_size(5);

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

  timer_start(0);

  for (step = 1; step <= niter; step++) {
    if ((step % 20) == 0 || step == niter || step == 1) {
      printf(" Time step %4d\n", step);
    }

    adi();
  }

  for (i = 0; i < num_devices; i++) {
    cl_int ecode = clFinish(cmd_queue[i]);
    clu_CheckError(ecode, "clFinish()");
  }

  timer_stop(0);
  t = timer_read(0);

  verify(niter, &Class, &verified);

  n3 = 1.0*grid_points[0]*grid_points[1]*grid_points[2];
  navg = (grid_points[0]+grid_points[1]+grid_points[2])/3.0;
  if (t != 0.0) {
    mflops = 1.0e-6*(double)(niter)*
      (3478.8*n3-17655.7*navg*navg+28023.7*navg)
      / t;
  } else {
    mflops = 0.0;
  }

  c_print_results("BT", Class, grid_points[0], 
                  grid_points[1], grid_points[2], niter,
                  t, mflops, "          floating point", 
                  verified, NPBVERSION,COMPILETIME, CS1, CS2, CS3, CS4, CS5, 
                  CS6, "(none)",
                  clu_GetDeviceTypeName(device_type),
                  device_name, num_devices);

  if (timeron) {
    for (i = 0; i < t_last; i++) {
      t1[i] = timer_read(i);
    }
    t1[t_xsolve] = t1[t_xsolve] - t1[t_xcomm];
    t1[t_ysolve] = t1[t_ysolve] - t1[t_ycomm];
    t1[t_zsolve] = t1[t_zsolve] - t1[t_zcomm];
    t1[t_last+2] = t1[t_xcomm]+t1[t_ycomm]+t1[t_zcomm]+t1[t_exch];
    t1[t_last+1] = t1[t_total]  - t1[t_last+2];

    for (i = 0; i < t_last+2; i++) {
      printf(" timer %2d(%-8s) :  %10.4f\n", i, t_recs[i], t1[i]);
    }
  }

  release_opencl();

  return 0;
}


//---------------------------------------------------------------------
// Set up the OpenCL environment.
//---------------------------------------------------------------------
static void setup_opencl(int argc, char *argv[])
{
  int i;
  cl_int ecode;
  char *source_dir = ".";

  if (argc > 1) source_dir = argv[1];

  if (timeron) {
    timer_clear(TIMER_OPENCL);
    timer_clear(TIMER_BUILD);
    timer_clear(TIMER_BUFFER);
    timer_clear(TIMER_RELEASE);

    timer_start(TIMER_OPENCL);
  }

  //-----------------------------------------------------------------------
  // 1. Find the default device type and get a device for the device type
  //    Then, create sub-devices from the parent device.
  //-----------------------------------------------------------------------
  device_type = CL_DEVICE_TYPE_CPU;

  cl_platform_id platform;
  ecode = clGetPlatformIDs(1, &platform, NULL);
  clu_CheckError(ecode, "clGetPlatformIDs()");

  ecode = clGetDeviceIDs(platform, device_type, 0, NULL, &num_devices);
  clu_CheckError(ecode, "clGetDeviceIDs()");

  devices = (cl_device_id *)malloc(sizeof(cl_device_id) * num_devices);
  ecode = clGetDeviceIDs(platform, device_type, num_devices, devices, NULL);
  clu_CheckError(ecode, "clGetDeviceIDs()");

  work_item_sizes[0] = work_item_sizes[1] = work_item_sizes[2] = 1024;
  max_work_group_size = 1024;
  max_compute_units = 22;

  //-------------------------------------------------------------------------
  MAXCELLS = isqrt(num_devices);
  MAX_CELL_DIM = (PROBLEM_SIZE/MAXCELLS)+1;
  IMAX = MAX_CELL_DIM;
  JMAX = MAX_CELL_DIM;
  KMAX = MAX_CELL_DIM;
  BUF_SIZE = MAX_CELL_DIM*MAX_CELL_DIM*(MAXCELLS-1)*60+1;
  //-------------------------------------------------------------------------

  ////////////////////////////////////////////////////////////////////////
  // FIXME
  size_t default_wg_size = 64;
  if (device_type == CL_DEVICE_TYPE_CPU) {
    if (CLASS == 'B') default_wg_size = 128;
  } else {
    if (CLASS == 'B') default_wg_size = 32;
  }
//  size_t default_wg_size = 128;
//  if (device_type == CL_DEVICE_TYPE_CPU) {
//    if (CLASS == 'B') default_wg_size = 128;
//  } else {
//    if (CLASS == 'B') default_wg_size = 32;
//  }
  if (max_work_group_size > default_wg_size) {
    max_work_group_size = default_wg_size;
    int i;
    for (i = 0; i < 3; i++) {
      if (work_item_sizes[i] > default_wg_size) {
        work_item_sizes[i] = default_wg_size;
      }
    }
  }
  if (device_type == CL_DEVICE_TYPE_CPU) {
    COPY_FACES_OUT1_DIM = COPY_FACES_OUT1_DIM_CPU;
    COPY_FACES_OUT2_DIM = COPY_FACES_OUT2_DIM_CPU;
    COPY_FACES_OUT3_DIM = COPY_FACES_OUT3_DIM_CPU;
    COPY_FACES_IN1_DIM = COPY_FACES_IN1_DIM_CPU;
    COPY_FACES_IN2_DIM = COPY_FACES_IN2_DIM_CPU;
    COPY_FACES_IN3_DIM = COPY_FACES_IN3_DIM_CPU;
    COMPUTE_RHS1_DIM = COMPUTE_RHS1_DIM_CPU;
    COMPUTE_RHS2_DIM = COMPUTE_RHS2_DIM_CPU;
    COMPUTE_RHS6_DIM = COMPUTE_RHS6_DIM_CPU;
    X_SOLVE_DIM = X_SOLVE_DIM_CPU;
    Y_SOLVE_DIM = Y_SOLVE_DIM_CPU;
    Z_SOLVE_DIM = Z_SOLVE_DIM_CPU;
    ADD_DIM = ADD_DIM_CPU;
  } else {
    COPY_FACES_OUT1_DIM = COPY_FACES_OUT1_DIM_GPU;
    COPY_FACES_OUT2_DIM = COPY_FACES_OUT2_DIM_GPU;
    COPY_FACES_OUT3_DIM = COPY_FACES_OUT3_DIM_GPU;
    COPY_FACES_IN1_DIM = COPY_FACES_IN1_DIM_GPU;
    COPY_FACES_IN2_DIM = COPY_FACES_IN2_DIM_GPU;
    COPY_FACES_IN3_DIM = COPY_FACES_IN3_DIM_GPU;
    COMPUTE_RHS1_DIM = COMPUTE_RHS1_DIM_GPU;
    COMPUTE_RHS2_DIM = COMPUTE_RHS2_DIM_GPU;
    COMPUTE_RHS6_DIM = COMPUTE_RHS6_DIM_GPU;
    X_SOLVE_DIM = X_SOLVE_DIM_GPU;
    Y_SOLVE_DIM = Y_SOLVE_DIM_GPU;
    Z_SOLVE_DIM = Z_SOLVE_DIM_GPU;
    ADD_DIM = ADD_DIM_GPU;
  }
  ////////////////////////////////////////////////////////////////////////

  //-----------------------------------------------------------------------
  // 2. Create a context for devices
  //-----------------------------------------------------------------------
  context = clCreateContext(NULL, 
                            num_devices,
                            devices,
                            NULL, NULL, &ecode);
  clu_CheckError(ecode, "clCreateContext()");

  //-----------------------------------------------------------------------
  // 3. Create a command queue
  //-----------------------------------------------------------------------
  cmd_queue = (cl_command_queue*)malloc(sizeof(cl_command_queue)*num_devices);
  for (i = 0; i < num_devices; i++) {
    cmd_queue[i] = clCreateCommandQueue(context, devices[i], 0, &ecode);
    clu_CheckError(ecode, "clCreateCommandQueue()");
  }

  //-----------------------------------------------------------------------
  // 4. Build programs
  //-----------------------------------------------------------------------
  if (timeron) timer_start(TIMER_BUILD);
  char build_option[100];

  if (device_type == CL_DEVICE_TYPE_CPU) {
    sprintf(build_option,
        "-I. -DCLASS=%d -DMAXCELLS=%d -DMAX_CELL_DIM=%d -DIMAX=%d -DJMAX=%d "
        "-DKMAX=%d -DBUF_SIZE=%d -DUSE_CPU",
        CLASS, MAXCELLS, MAX_CELL_DIM, IMAX, JMAX, KMAX, BUF_SIZE);
  } else {
    sprintf(build_option,
        "-I. -DCLASS=%d -DMAXCELLS=%d -DMAX_CELL_DIM=%d -DIMAX=%d "
        "-DJMAX=%d -DKMAX=%d -DBUF_SIZE=%d",
        CLASS, MAXCELLS, MAX_CELL_DIM, IMAX, JMAX, KMAX, BUF_SIZE);
  }

  p_initialize = (cl_program *)malloc(sizeof(cl_program) * num_devices);
  p_exact_rhs = (cl_program *)malloc(sizeof(cl_program) * num_devices);
  p_error = (cl_program *)malloc(sizeof(cl_program) * num_devices);
  p_adi = (cl_program *)malloc(sizeof(cl_program) * num_devices);
  p_solve = (cl_program *)malloc(sizeof(cl_program) * num_devices);

  program = clu_MakeProgram(context, devices, source_dir, "bt_kernel.cl", build_option);

  for (i = 0; i < num_devices; i++) {
    p_initialize[i] = program;
    p_exact_rhs[i] = program;
    p_error[i] = program;
    p_adi[i] = program;
    p_solve[i] = program;
  }

  //-----------------------------------------------------------------------
  // 5. Create buffers
  //-----------------------------------------------------------------------
  if (timeron) timer_start(TIMER_BUFFER);
  m_ce = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_us = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_vs = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_ws = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_qs = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_rho_i = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_square = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_forcing = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_u = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_rhs = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_lhsc = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_backsub_info = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_in_buffer = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  m_out_buffer = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);

  for (i = 0; i < num_devices; i++) {
    m_ce[i] = clCreateBuffer(context,
                      CL_MEM_READ_ONLY,
                      sizeof(double)*5*13,
                      NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_ce");

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

    m_rho_i[i] = clCreateBuffer(context,
                      CL_MEM_READ_WRITE,
                      sizeof(double)*MAXCELLS*(KMAX+2)*(JMAX+2)*(IMAX+2),
                      NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_rho_i");

    m_square[i] = clCreateBuffer(context,
                      CL_MEM_READ_WRITE,
                      sizeof(double)*MAXCELLS*(KMAX+2)*(JMAX+2)*(IMAX+2),
                      NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_square");

    m_forcing[i] = clCreateBuffer(context,
                      CL_MEM_READ_WRITE,
                      sizeof(double)*MAXCELLS*KMAX*JMAX*IMAX*5,
                      NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_forcing");

    m_u[i] = clCreateBuffer(context,
                      CL_MEM_READ_WRITE,
                      sizeof(double)*MAXCELLS*(KMAX+4)*(JMAX+4)*(IMAX+4)*5,
                      NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_u");

    m_rhs[i] = clCreateBuffer(context,
                      CL_MEM_READ_WRITE,
                      sizeof(double)*MAXCELLS*(KMAX+1)*(JMAX+1)*(IMAX+1)*5,
                      NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_rhs");

    m_lhsc[i] = clCreateBuffer(context,
                      CL_MEM_READ_WRITE,
                      sizeof(double)*MAXCELLS*(KMAX+1)*(JMAX+1)*(IMAX+1)*5*5,
                      NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_lhsc");

    m_backsub_info[i] = clCreateBuffer(context,
                  CL_MEM_READ_WRITE,
                  sizeof(double)*MAXCELLS*(MAX_CELL_DIM+1)*(MAX_CELL_DIM+1)*5,
                  NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for m_backsub_info");

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
  }
  if (timeron) timer_stop(TIMER_BUFFER);

  //-----------------------------------------------------------------------
  // 6. Create kernels
  //-----------------------------------------------------------------------
  if (MAXCELLS > 1) {
    k_cf_out1 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
    k_cf_out2 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
    k_cf_out3 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
    k_cf_in1 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
    k_cf_in2 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
    k_cf_in3 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);

    for (i = 0; i < num_devices; i++) {
      k_cf_out1[i] = clCreateKernel(p_adi[i], "copy_faces_out1", &ecode);
      clu_CheckError(ecode, "clCreateKernel()");

      k_cf_out2[i] = clCreateKernel(p_adi[i], "copy_faces_out2", &ecode);
      clu_CheckError(ecode, "clCreateKernel()");

      k_cf_out3[i] = clCreateKernel(p_adi[i], "copy_faces_out3", &ecode);
      clu_CheckError(ecode, "clCreateKernel()");

      k_cf_in1[i] = clCreateKernel(p_adi[i], "copy_faces_in1", &ecode);
      clu_CheckError(ecode, "clCreateKernel()");

      k_cf_in2[i] = clCreateKernel(p_adi[i], "copy_faces_in2", &ecode);
      clu_CheckError(ecode, "clCreateKernel()");

      k_cf_in3[i] = clCreateKernel(p_adi[i], "copy_faces_in3", &ecode);
      clu_CheckError(ecode, "clCreateKernel()");
    }
  }

  k_compute_rhs1 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_compute_rhs2 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_compute_rhs3 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_compute_rhs4 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_compute_rhs5 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_compute_rhs6 = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_add = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_x_usi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_x_ssi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_x_sbi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_x_ubi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_x_bs  = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_x_sc  = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_y_usi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_y_ssi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_y_sbi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_y_ubi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_y_bs  = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_y_sc  = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_z_usi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_z_ssi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_z_sbi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_z_ubi = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_z_bs  = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  k_z_sc  = (cl_kernel *)malloc(sizeof(cl_kernel) * num_devices);
  for (i = 0; i < num_devices; i++) {
    k_compute_rhs1[i] = clCreateKernel(p_adi[i], "compute_rhs1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_compute_rhs2[i] = clCreateKernel(p_adi[i], "compute_rhs2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_compute_rhs3[i] = clCreateKernel(p_adi[i], "compute_rhs3", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_compute_rhs4[i] = clCreateKernel(p_adi[i], "compute_rhs4", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_compute_rhs5[i] = clCreateKernel(p_adi[i], "compute_rhs5", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_compute_rhs6[i] = clCreateKernel(p_adi[i], "compute_rhs6", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_add[i] = clCreateKernel(p_adi[i], "add", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_x_usi[i] = clCreateKernel(p_solve[i], "x_unpack_solve_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_x_ssi[i] = clCreateKernel(p_solve[i], "x_send_solve_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_x_sbi[i] = clCreateKernel(p_solve[i], "x_send_backsub_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_x_ubi[i] = clCreateKernel(p_solve[i], "x_unpack_backsub_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_x_bs[i] = clCreateKernel(p_solve[i], "x_backsubstitute", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_x_sc[i] = clCreateKernel(p_solve[i], "x_solve_cell", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_y_usi[i] = clCreateKernel(p_solve[i], "y_unpack_solve_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_y_ssi[i] = clCreateKernel(p_solve[i], "y_send_solve_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_y_sbi[i] = clCreateKernel(p_solve[i], "y_send_backsub_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_y_ubi[i] = clCreateKernel(p_solve[i], "y_unpack_backsub_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_y_bs[i] = clCreateKernel(p_solve[i], "y_backsubstitute", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_y_sc[i] = clCreateKernel(p_solve[i], "y_solve_cell", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_z_usi[i] = clCreateKernel(p_solve[i], "z_unpack_solve_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_z_ssi[i] = clCreateKernel(p_solve[i], "z_send_solve_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_z_sbi[i] = clCreateKernel(p_solve[i], "z_send_backsub_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_z_ubi[i] = clCreateKernel(p_solve[i], "z_unpack_backsub_info", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_z_bs[i] = clCreateKernel(p_solve[i], "z_backsubstitute", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_z_sc[i] = clCreateKernel(p_solve[i], "z_solve_cell", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
  }

  if (timeron) timer_stop(TIMER_OPENCL);
}


static void release_opencl()
{
  int i;

  if (timeron) {
    timer_start(TIMER_OPENCL);
    timer_start(TIMER_RELEASE);
  }

  for (i = 0; i < num_devices; i++) {
    clReleaseMemObject(m_ce[i]);
    clReleaseMemObject(m_cell_coord[i]);
    clReleaseMemObject(m_cell_low[i]);
    clReleaseMemObject(m_cell_high[i]);
    clReleaseMemObject(m_cell_size[i]);
    clReleaseMemObject(m_slice[i]);
    clReleaseMemObject(m_start[i]);
    clReleaseMemObject(m_end[i]);
    clReleaseMemObject(m_predecessor[i]);
    clReleaseMemObject(m_successor[i]);
    if (ncells > 1) {
      clReleaseMemObject(m_start_send[i]);
      clReleaseMemObject(m_start_recv[i]);
      clReleaseMemObject(m_p0_offset[i]);
      clReleaseMemObject(m_p1_offset[i]);
      clReleaseMemObject(m_p2_offset[i]);
      clReleaseMemObject(m_p3_offset[i]);
      clReleaseMemObject(m_p4_offset[i]);
      clReleaseMemObject(m_p5_offset[i]);
    }
    clReleaseMemObject(m_us[i]);
    clReleaseMemObject(m_vs[i]);
    clReleaseMemObject(m_ws[i]);
    clReleaseMemObject(m_qs[i]);
    clReleaseMemObject(m_rho_i[i]);
    clReleaseMemObject(m_square[i]);
    clReleaseMemObject(m_forcing[i]);
    clReleaseMemObject(m_u[i]);
    clReleaseMemObject(m_rhs[i]);
    clReleaseMemObject(m_lhsc[i]);
    clReleaseMemObject(m_backsub_info[i]);
    clReleaseMemObject(m_in_buffer[i]);
    clReleaseMemObject(m_out_buffer[i]);
    clReleaseMemObject(m_lhsa[i]);
    clReleaseMemObject(m_lhsb[i]);
    clReleaseMemObject(m_fjac[i]);
    clReleaseMemObject(m_njac[i]);
    clReleaseMemObject(m_utmp[i]);

    if (ncells > 1) {
      clReleaseKernel(k_cf_out1[i]);
      clReleaseKernel(k_cf_out2[i]);
      clReleaseKernel(k_cf_out3[i]);
      clReleaseKernel(k_cf_in1[i]);
      clReleaseKernel(k_cf_in2[i]);
      clReleaseKernel(k_cf_in3[i]);
    }
    clReleaseKernel(k_compute_rhs1[i]);
    clReleaseKernel(k_compute_rhs2[i]);
    clReleaseKernel(k_compute_rhs3[i]);
    clReleaseKernel(k_compute_rhs4[i]);
    clReleaseKernel(k_compute_rhs5[i]);
    clReleaseKernel(k_compute_rhs6[i]);
    clReleaseKernel(k_add[i]);
    clReleaseKernel(k_x_usi[i]);
    clReleaseKernel(k_x_ssi[i]);
    clReleaseKernel(k_x_sbi[i]);
    clReleaseKernel(k_x_ubi[i]);
    clReleaseKernel(k_x_bs[i]);
    clReleaseKernel(k_x_sc[i]);
    clReleaseKernel(k_y_usi[i]);
    clReleaseKernel(k_y_ssi[i]);
    clReleaseKernel(k_y_sbi[i]);
    clReleaseKernel(k_y_ubi[i]);
    clReleaseKernel(k_y_bs[i]);
    clReleaseKernel(k_y_sc[i]);
    clReleaseKernel(k_z_usi[i]);
    clReleaseKernel(k_z_ssi[i]);
    clReleaseKernel(k_z_sbi[i]);
    clReleaseKernel(k_z_ubi[i]);
    clReleaseKernel(k_z_bs[i]);
    clReleaseKernel(k_z_sc[i]);

    clReleaseProgram(p_initialize[i]);
    clReleaseProgram(p_exact_rhs[i]);
    clReleaseProgram(p_adi[i]);
    clReleaseProgram(p_solve[i]);
    clReleaseProgram(p_error[i]);

    clReleaseCommandQueue(cmd_queue[i]);
  }

  free(m_cell_coord);
  free(m_cell_low);
  free(m_cell_high);
  free(m_cell_size);
  free(m_slice);
  free(m_start);
  free(m_end);
  free(m_predecessor);
  free(m_successor);
  if (ncells > 1) {
    free(m_start_send);
    free(m_start_recv);
    free(m_p0_offset);
    free(m_p1_offset);
    free(m_p2_offset);
    free(m_p3_offset);
    free(m_p4_offset);
    free(m_p5_offset);
  }
  free(m_us);
  free(m_vs);
  free(m_ws);
  free(m_qs);
  free(m_rho_i);
  free(m_square);
  free(m_forcing);
  free(m_u);
  free(m_rhs);
  free(m_lhsc);
  free(m_backsub_info);
  free(m_in_buffer);
  free(m_out_buffer);
  free(m_lhsa);
  free(m_lhsb);
  free(m_fjac);
  free(m_njac);
  free(m_utmp);

  if (ncells > 1) {
    free(k_cf_out1);
    free(k_cf_out2);
    free(k_cf_out3);
    free(k_cf_in1);
    free(k_cf_in2);
    free(k_cf_in3);
  }
  free(k_compute_rhs1);
  free(k_compute_rhs2);
  free(k_compute_rhs3);
  free(k_compute_rhs4);
  free(k_compute_rhs5);
  free(k_compute_rhs6);
  free(k_add);
  free(k_x_usi);
  free(k_x_ssi);
  free(k_x_sbi);
  free(k_x_ubi);
  free(k_x_bs);
  free(k_x_sc);
  free(k_y_usi);
  free(k_y_ssi);
  free(k_y_sbi);
  free(k_y_ubi);
  free(k_y_bs);
  free(k_y_sc);
  free(k_z_usi);
  free(k_z_ssi);
  free(k_z_sbi);
  free(k_z_ubi);
  free(k_z_bs);
  free(k_z_sc);

  free(p_initialize);
  free(p_exact_rhs);
  free(p_adi);
  free(p_solve);
  free(p_error);

  free(cmd_queue);

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

  free(g_cell_coord);
  free(g_cell_low);
  free(g_cell_high);
  free(g_cell_size);
  free(g_slice);
  free(g_start);
  free(g_end);
  free(predecessor);
  free(successor);
  free(max_cell_size);
  free(g_p0_offset);
  free(g_p1_offset);
  free(g_p2_offset);
  free(g_p3_offset);
  free(g_p4_offset);
  free(g_p5_offset);
  if (ncells > 1) {
    free(box_size);
    free(start_send);
    free(start_recv);
  }
}


/* integer square root. Return error if argument isn't
 * a perfect square or is less than or equal to zero 
 */
static int isqrt(int i)
{
  int root, square;
  if (i <= 0) return(-1);
  square = 0;
  for (root = 1; square <= i; root++) {
    square = root*root;
    if (square == i) return(root);
  }
  return(-1);
}


