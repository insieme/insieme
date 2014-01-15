//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB MG code for multiple    //
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
//      program mg_ocl_md
//---------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "globals.h"
#include "randdp.h"
#include "timers.h"
#include "print_results.h"

#include "mg_dim.h"

#include <CL/cl.h>
#include <CL/cl_ext.h>
#include "cl_util.h"
#define TIMER_OPENCL    20
#define TIMER_BUILD     21
#define TIMER_BUFFER    22
#define TIMER_RELEASE   23

#ifdef TIMER_DETAIL
#define DTIMER_START(id)    if (timers_enabled) timer_start(id)
#define DTIMER_STOP(id)     if (timers_enabled) timer_stop(id)
#else
#define DTIMER_START(id)
#define DTIMER_STOP(id)
#endif

int __devidx;
cl_int __ecode;
#define CHECK_FINISH()      for (__devidx = 0; __devidx < num_devices; __devidx++) { \
  __ecode = clFinish(cmd_queue[__devidx]); \
  clu_CheckError(__ecode, "clFinish()"); \
}

#define USE_CHECK_FINISH_DEBUG
#ifdef USE_CHECK_FINISH_DEBUG
#define CHECK_FINISH_DEBUG()      for (__devidx = 0; __devidx < num_devices; __devidx++) { \
  __ecode = clFinish(cmd_queue[__devidx]); \
  clu_CheckError(__ecode, "clFinish()"); \
}
#else
#define CHECK_FINISH_DEBUG()
#endif


#define DEFAULT_NUM_DEVICES 1

static int ready_cnt = 0;
static size_t ready_size = 0;

void initBuff();
static void setup(int *n1, int *n2, int *n3);
static void mg3P(cl_mem *ou_buf, cl_mem *ov_buf, cl_mem *or_buf,
    double a[4], double c[4], int* n1, int* n2, int* n3);
static void psinv(cl_mem *or_buf, cl_mem *ou_buf, int* n1, int* n2, int* n3,
    double c[4], int k, int* offset);
static void resid(cl_mem *ou_buf, cl_mem *ov_buf, cl_mem *or_buf, int* n1, int* n2, int* n3,
    double a[4], int k, int* offset);
static void rprj3(cl_mem *or_buf, int* m1k, int* m2k, int* m3k,
    int* m1j, int* m2j, int* m3j, int k, int* offset_r, int* offset_s);
static void interp(cl_mem *ou_buf, int* mm1, int* mm2, int* mm3,
    int* n1, int* n2, int* n3, int k, int* offset_z, int* offset_u);
static void norm2u3(cl_mem *or_buf, int* n1, int* n2, int* n3,
    double *rnm2, double *rnmu,
    int nx0, int ny0, int nz0, int* offset);
static void rep_nrm(cl_mem *ou_buf, int* n1, int* n2, int* n3, char *title, int kk, int* offset);
static void comm3(cl_mem *ou_buf, int *n1, int *n2, int *n3, int kk, int* offset);
static void comm3_ex(cl_mem *ou_buf, int *n1, int *n2, int *n3, int kk, int* offset);
static void ready(int axis, int dir, int k, int i);
static void give3(int axis, int dir, cl_mem *ou_buf, int* n1, int* n2, int* n3, int k, int i, int* offset);
static void copy_buffer(int axis, int dir, int i);
static void take3(int axis, int dir, cl_mem *ou_buf, int* n1, int* n2, int* n3, int i, int* offset);
static void give3_ex(int axis, int dir, cl_mem *ou_buf, int* n1, int* n2, int* n3,
    int k, int i, int* offset);
static void take3_ex(int axis, int dir, cl_mem *ou_buf, int* n1, int* n2, int* n3, int i, int* offset);
static void comm1p(int axis, cl_mem *ou_buf, int *n1, int *n2, int *n3, int kk, int* offset);
static void comm1p_ex(int axis, cl_mem *ou_buf, int* n1, int* n2, int* n3, int kk, int* offset);
static void zran3(cl_mem *oz_buf, int* n1, int* n2, int* n3, int nx, int ny, int k);
//static void show_l(void *oz, int n1, int n2, int n3);
static void showall(cl_mem *oz_buf, int* n1, int* n2, int* n3, int* offset);
//static void show(void *oz, int n1, int n2, int n3);
static double power(double a, int n1, int n2, int n3);
static void power_task(cl_mem* out_buf, double a, int n1, int *is1, int *is2, int *is3, int ny, double* out_val);
static void zero3(cl_mem *oz_buf, int* n1, int* n2, int* n3, int* offset);
static void zero3_for_one_dev(cl_mem *oz_buf, int* n1, int* n2, int* n3, int i, int* offset);
static void setup_opencl(int argc, char *argv[]);
static void release_opencl();
int ilog2(int i);

//-------------------------------------------------------------------------c
// These arrays are in common because they are quite large
// and probably shouldn't be allocated on the stack. They
// are always passed as subroutine args. 
//-------------------------------------------------------------------------c
/* commcon /noautom/ */
//static double u[NR];
//static double v[NR];
//static double r[NR];

/* common /grid/ */
static int *is1, *is2, *is3, *ie1, *ie2, *ie3;

static int *null_offset;

static int nprocs;

// OPENCL Variables
static cl_device_type    device_type;
static cl_device_id      p_device;
static char             *device_name;
static cl_device_id     *devices;
static cl_uint           num_devices;
static cl_context        context;
static cl_command_queue *cmd_queue;
static cl_program       p_program;
static cl_program       *program;
static size_t  work_item_sizes[3];
static size_t  max_work_group_size;
static cl_uint max_compute_units;

static cl_kernel *k_zero3;
static cl_kernel *k_power_task;
static cl_kernel *k_zran3_task1;
static cl_kernel *k_zran3_task2;
static cl_kernel *k_zran3_task3;
static cl_kernel *k_zran3_1;
static cl_kernel *k_comm1p_1;
static cl_kernel *k_comm1p_2_axis0;
static cl_kernel *k_comm1p_2_axis1;
static cl_kernel *k_comm1p_2_axis2;
static cl_kernel *k_comm1p_3;
static cl_kernel *k_comm1p_4_axis0;
static cl_kernel *k_comm1p_4_axis1;
static cl_kernel *k_comm1p_4_axis2;
static cl_kernel *k_ready_1;
static cl_kernel *k_give3_1_axis0;
static cl_kernel *k_give3_1_axis1;
static cl_kernel *k_give3_1_axis2;
static cl_kernel *k_take3_1_axis0;
static cl_kernel *k_take3_1_axis1;
static cl_kernel *k_take3_1_axis2;
static cl_kernel *k_comm1pex_2_axis0;
static cl_kernel *k_comm1pex_2_axis1;
static cl_kernel *k_comm1pex_2_axis2;
static cl_kernel *k_comm1pex_3_axis0;
static cl_kernel *k_comm1pex_3_axis1;
static cl_kernel *k_comm1pex_3_axis2;
static cl_kernel *k_comm1pex_4_axis0;
static cl_kernel *k_comm1pex_4_axis1;
static cl_kernel *k_comm1pex_4_axis2;
static cl_kernel *k_comm1pex_5_axis0;
static cl_kernel *k_comm1pex_5_axis1;
static cl_kernel *k_comm1pex_5_axis2;
static cl_kernel *k_norm2u3;
static cl_kernel *k_resid;
static cl_kernel *k_rprj3;
static cl_kernel *k_psinv;
static cl_kernel *k_interp_1;
static cl_kernel *k_interp_2;
static cl_kernel *k_interp_3;
static cl_kernel *k_interp_4;
static cl_kernel *k_interp_5;
static cl_kernel *k_copy_buffer;

static cl_mem *u_buf;
static cl_mem *v_buf;
static cl_mem *r_buf;
static cl_mem* j1_buf; 
static cl_mem* j2_buf; 
static cl_mem* j3_buf; 
static cl_mem* ten_buf; 
static cl_mem *power_task_out_buf;
static cl_mem *starts_buf;
static cl_mem *buff_buf;
static cl_mem *zero_buff_buf;
static int ZERO3_DIM;
static int COMM1P_1_DIM;
static int COMM1P_2_DIM;
static int COMM1P_3_DIM;
static int COMM1P_4_DIM;
static int READY_1_DIM;
static int GIVE3_1_DIM;
static int TAKE3_1_DIM;
static int COMM1PEX_2_DIM;
static int COMM1PEX_3_DIM;
static int COMM1PEX_4_DIM;
static int COMM1PEX_5_DIM;
static int GIVE3EX_1_DIM;
static int TAKE3EX_1_DIM;
static int NORM2U3_DIM;
static int RESID_DIM;
static int RPRJ3_DIM;
static int PSINV_DIM;
static int INTERP_1_DIM;
static int INTERP_2_DIM;
static int INTERP_3_DIM;
static int INTERP_4_DIM;
static int INTERP_5_DIM;
static int COPY_BUFFER_DIM;

struct recv_param_t{
  int src_id;
  cl_mem src_buf;
  cl_mem dst_buf;
  int src_offset;
  int dst_offset;
  int cb;
  cl_event* event;
};

static cl_event (*recv_events)[2][3][3];
static cl_event (*send_events)[3][3];
struct recv_param_t (*recv_params)[3][3];

static unsigned long long M;
static unsigned long long NM;
static unsigned long long NV;
static unsigned long long NM2;
static unsigned long long NR;

#ifdef CLUSTER
int __main(int argc, char** argv)
#else
int main(int argc, char *argv[])
#endif
{

  cl_int ecode;
  setup_opencl(argc, argv);

  //	/*
  //CHECK MEMORY USAGE
  printf("NR(%d) NM(%d) NM2(%d) nprocs(%d)\n",NR,NM,NM2,nprocs);
  unsigned long long  total_size = (NR*3 + 1 + NM + NM2*4)*sizeof(double)*nprocs;
  double GB = (double)total_size / ( 1024 * 1024 * 1024);
  int MB = total_size / (1024 * 1024);

  if( GB < 1.0 )
    printf("MEMORY USAGE : %llu ( %d MB )\n",total_size,MB);
  else
    printf("MEMORY USAGE : %llu ( %.2lf GB )\n",total_size,GB);


  printf("\nFOR 1 DEVICE DETAIL\n");
  printf(" for u_buf    : %llu\n",NR*sizeof(double));
  printf(" for v_buf    : %llu\n",NR*sizeof(double));
  printf(" for r_buf    : %llu\n",NR*sizeof(double));
  printf(" for pto_buf  : %llu\n",sizeof(double));
  printf(" for star_buf : %llu\n",NM*sizeof(double));
  printf(" for buff_buf : %llu\n",NM2*4*sizeof(double));
  //CHECK MEMORY USAGE
  //	*/

  //-------------------------------------------------------------------------c
  // k is the current level. It is passed down through subroutine args
  // and is NOT global. it is the current iteration
  //-------------------------------------------------------------------------c
  int k, it;
  double t, t0, tinit, mflops;

  double a[4], c[4];

  double rnm2, rnmu;
  double old2, oldu, epsilon;
  int *n1, *n2, *n3, nit;
  double nn, verify_value, err;
  logical verified;

  int i;

  double t1[t_last+2];
  char *t_recs[t_last+2] = {
    "total", "init", "psinv", "resid", "rprj3", 
    "ready", "give3", "take3", "give3ex", "take3ex",
    "comm1p_1", "comm1p_2", "comm1p_3", "comm1p_4", 
    "comm1pex", "zran3", "zero3", "power_task" ,
    "interp", "norm2u3","norm2u3A",  "copy_buffr", "rcomm", 
    "part1", "part2", "part3", "part4", "part5","comm3", "comm3_ex",
    " totcomp", " totcomm"};

  for (i = 0; i < t_last; i++) {
    timer_clear(i);
  }

  timer_start(t_init);


  //---------------------------------------------------------------------
  // Read in and broadcast input data
  //---------------------------------------------------------------------

  printf("\n\n NAS Parallel Benchmarks (NPB3.3-OCL-MD) - MG Benchmark\n\n");

  FILE *fp;
  timeron = false;
  double t_sum_ori = 0.0;
  double t_sum_part = 0.0;
  if ((fp = fopen("timer.flag", "r")) != NULL) {
    timeron = true;
    fclose(fp);
  }

  if ((fp = fopen("mg.input", "r")) != NULL) {
    int result;
    printf(" Reading from input file mg.input\n");
    result = fscanf(fp, "%d\n", &lt);
    while (fgetc(fp) != '\n');
    result = fscanf(fp, "%d%d%d", &nx[lt], &ny[lt], &nz[lt]);
    while (fgetc(fp) != '\n');
    result = fscanf(fp, "%d", &nit);
    while (fgetc(fp) != '\n');
    for (i = 0; i <= 7; i++) {
      result = fscanf(fp, "%d", &debug_vec[i]);
    }
    fclose(fp);
  } else {
    printf(" No input file. Using compiled defaults \n");
    lt = LT_DEFAULT;
    nit = NIT_DEFAULT;
    nx[lt] = NX_DEFAULT;
    ny[lt] = NY_DEFAULT;
    nz[lt] = NZ_DEFAULT;
    for (i = 0; i <= 7; i++) {
      debug_vec[i] = DEBUG_DEFAULT;
    }
  }

  if ( (nx[lt] != ny[lt]) || (nx[lt] != nz[lt]) ) {
    Class = 'U';
  } else if( nx[lt] == 32 && nit == 4 ) {
    Class = 'S';
  } else if( nx[lt] == 128 && nit == 4 ) {
    Class = 'W';
  } else if( nx[lt] == 256 && nit == 4 ) {  
    Class = 'A';
  } else if( nx[lt] == 256 && nit == 20 ) {
    Class = 'B';
  } else if( nx[lt] == 512 && nit == 20 ) {  
    Class = 'C';
  } else if( nx[lt] == 1024 && nit == 50 ) {  
    Class = 'D';
  } else if( nx[lt] == 2048 && nit == 50 ) {  
    Class = 'E';
  } else {
    Class = 'U';
  }

  //---------------------------------------------------------------------
  // Use these for debug info:
  //---------------------------------------------------------------------
  //    debug_vec[0] = 1 !=> report all norms
  //    debug_vec[1] = 1 !=> some setup information
  //    debug_vec[1] = 2 !=> more setup information
  //    debug_vec[2] = k => at level k or below, show result of resid
  //    debug_vec[3] = k => at level k or below, show result of psinv
  //    debug_vec[4] = k => at level k or below, show result of rprj
  //    debug_vec[5] = k => at level k or below, show result of interp
  //    debug_vec[6] = 1 => (unused)
  //    debug_vec[7] = 1 => (unused)
  //---------------------------------------------------------------------
  a[0] = -8.0/3.0;
  a[1] =  0.0;
  a[2] =  1.0/6.0;
  a[3] =  1.0/12.0;

  if(Class == 'A' || Class == 'S'|| Class =='W') {
    //---------------------------------------------------------------------
    //     Coefficients for the S(a) smoother
    //---------------------------------------------------------------------
    c[0] =  -3.0/8.0;
    c[1] =  +1.0/32.0;
    c[2] =  -1.0/64.0;
    c[3] =   0.0;
  } else {
    //---------------------------------------------------------------------
    //     Coefficients for the S(b) smoother
    //---------------------------------------------------------------------
    c[0] =  -3.0/17.0;
    c[1] =  +1.0/33.0;
    c[2] =  -1.0/61.0;
    c[3] =   0.0;
  }
  lb = 1;
  k  = lt-1;

  //initialize arrays for device dependent values
  n1 = (int*)malloc(sizeof(int)*num_devices);
  n2 = (int*)malloc(sizeof(int)*num_devices);
  n3 = (int*)malloc(sizeof(int)*num_devices);
  dead = (logical (*)[MAXLEVEL])malloc(sizeof(logical)*MAXLEVEL * num_devices);
  give_ex = (logical (*)[MAXLEVEL][3])malloc(sizeof(logical)*MAXLEVEL*3 * num_devices);
  take_ex = (logical (*)[MAXLEVEL][3])malloc(sizeof(logical)*MAXLEVEL*3 * num_devices);

  m1 = (int **)malloc(sizeof(int*)*(MAXLEVEL+1));
  m2 = (int **)malloc(sizeof(int*)*(MAXLEVEL+1));
  m3 = (int **)malloc(sizeof(int*)*(MAXLEVEL+1));
  ir = (int **)malloc(sizeof(int*)*(MAXLEVEL+1));
  for (i = 0; i < MAXLEVEL+1; i++) 
  {
    m1[i] = (int *)malloc(sizeof(int)*num_devices);
    m2[i] = (int *)malloc(sizeof(int)*num_devices);
    m3[i] = (int *)malloc(sizeof(int)*num_devices);
    ir[i] = (int *)malloc(sizeof(int)*num_devices);
  }


  nbr = (int (*)[MAXLEVEL][3][3])malloc(sizeof(int)*MAXLEVEL*3*3 * num_devices);
  is1 = (int*)malloc(sizeof(int) * num_devices);
  is2 = (int*)malloc(sizeof(int) * num_devices);
  is3 = (int*)malloc(sizeof(int) * num_devices);
  ie1 = (int*)malloc(sizeof(int) * num_devices);
  ie2 = (int*)malloc(sizeof(int) * num_devices);
  ie3 = (int*)malloc(sizeof(int) * num_devices);
  null_offset = (int*)malloc(sizeof(int) * num_devices);
  u_buf = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  v_buf = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  r_buf = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  j1_buf =(cl_mem*)malloc(sizeof(cl_mem)*num_devices);
  j2_buf =(cl_mem*)malloc(sizeof(cl_mem)*num_devices);
  j3_buf =(cl_mem*)malloc(sizeof(cl_mem)*num_devices);
  ten_buf =(cl_mem*)malloc(sizeof(cl_mem)*num_devices);
  power_task_out_buf = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  starts_buf = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  buff_buf = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);
  zero_buff_buf = (cl_mem *)malloc(sizeof(cl_mem) * num_devices);

  recv_events = (cl_event (*)[2][3][3])malloc(sizeof(cl_event)*2*3*3 * num_devices);
  send_events = (cl_event (*)[3][3])malloc(sizeof(cl_event)*3*3 * num_devices);
  recv_params = (struct recv_param_t (*)[3][3])malloc(sizeof(struct recv_param_t)*3*3 * num_devices);


  for (i = 0; i < num_devices; i++) 
  {
    null_offset[i] = 0;
    u_buf[i] = clCreateBuffer(context,
        CL_MEM_READ_WRITE,
        sizeof(double) * NR,
        NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for u_buf");

    v_buf[i] = clCreateBuffer(context,
        CL_MEM_READ_WRITE,
        sizeof(double) * NR,
        NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for v_buf");

    r_buf[i] = clCreateBuffer(context,
        CL_MEM_READ_WRITE,
        sizeof(double) * NR,
        NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for r_buf");

    power_task_out_buf[i] = clCreateBuffer(context,
        CL_MEM_WRITE_ONLY,
        sizeof(double),
        NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for power_task_out_buf");

    starts_buf[i] = clCreateBuffer(context,
        CL_MEM_READ_WRITE,
        sizeof(double) * NM,
        NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for starts_buf");

    buff_buf[i] = clCreateBuffer(context,
        CL_MEM_READ_WRITE,
        sizeof(double) * 4 * NM2,
        NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for buff_buf");

    zero_buff_buf[i] = clCreateBuffer(context,
        CL_MEM_READ_WRITE,
        sizeof(double) * NM2,
        NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for zero_buff_buf");

    j1_buf[i] = clCreateBuffer(context,
        CL_MEM_READ_WRITE,
        sizeof(int) * 10 * 2,
        NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for j1_buf");
    j2_buf[i] = clCreateBuffer(context,
        CL_MEM_READ_WRITE,
        sizeof(int) * 10 * 2,
        NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for j2_buf");
    j3_buf[i] = clCreateBuffer(context,
        CL_MEM_READ_WRITE,
        sizeof(int) * 10 * 2,
        NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for j3_buf");
    ten_buf[i] = clCreateBuffer(context,
        CL_MEM_READ_WRITE,
        sizeof(double) * 10 * 2,
        NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for ten_buf");


  }

  initBuff();

  setup(n1, n2, n3);

  zero3(u_buf, n1, n2, n3, null_offset);

  zran3(v_buf, n1, n2, n3, nx[lt], ny[lt], k);

  norm2u3(v_buf, n1, n2, n3, &rnm2, &rnmu, nx[lt], ny[lt], nz[lt], null_offset);

  printf(" Size: %4dx%4dx%4d  (class %c)\n", nx[lt], ny[lt], nz[lt], Class);
  printf(" Iterations: %4d\n", nit);

  printf(" Number of processes: %6d\n", num_devices);

  resid(u_buf, v_buf, r_buf, n1, n2, n3, a, k, null_offset);
  norm2u3(r_buf, n1, n2, n3, &rnm2, &rnmu, nx[lt], ny[lt], nz[lt], null_offset);
  old2 = rnm2;
  oldu = rnmu;

  //---------------------------------------------------------------------
  // One iteration for startup
  //---------------------------------------------------------------------
  mg3P(u_buf, v_buf, r_buf, a, c, n1, n2, n3);

  resid(u_buf, v_buf, r_buf, n1, n2, n3, a, k, null_offset);
  setup(n1, n2, n3);
  zero3(u_buf, n1, n2, n3, null_offset);
  zran3(v_buf, n1, n2, n3, nx[lt], ny[lt], k);

  timer_stop(t_init);
  tinit = timer_read(t_init);
  printf("\n Initialization time: %15.3f seconds\n\n", tinit);

  if (timeron) {
    for (i = 0; i < t_last; i++) {
      t1[i] = timer_read(i);
    }
    t1[t_last+1] = t1[t_rcomm] + t1[t_copy_buffer];
    t1[t_last+0] = t1[t_bench] - t1[t_last+1];

    printf(" nprocs =%6d           minimum     maximum     average\n",
        nprocs);
    for (i = 0; i < t_last+2; i++) {
      t1[i] = t1[i];
      printf(" timer %2d(%-8s) :  %10.4f\n",
          i+1, t_recs[i], t1[i]);
    }
  }

  for (i = 0; i < t_last; i++) {
    if (i != t_init) timer_clear(i);
  }

  timer_start(t_bench);

  timer_start(t_part1_resid);
  resid(u_buf, v_buf, r_buf, n1, n2, n3, a, k, null_offset);
  timer_stop(t_part1_resid);

  timer_stop(t_bench);

  if (timeron) {
    for (i = 0; i < t_last; i++) {
      t1[i] = timer_read(i);

      if( i>t_init && i <t_part1_resid&& i != t_norm2u3_all)
        t_sum_ori += t1[i];
      if( i >= t_part1_resid && i <= t_part5_norm2u3)
        t_sum_part += t1[i];

    }
    t1[t_last+1] = t1[t_rcomm] + t1[t_copy_buffer];
    t1[t_last+0] = t1[t_bench] - t1[t_last+1];

    printf(" nprocs =%6d           minimum     maximum     average\n",
        nprocs);
    for (i = 0; i < t_last+2; i++) {
      t1[i] = t1[i];
      printf(" timer %2d(%-8s) :  %10.4f\n",
          i+1, t_recs[i], t1[i]);
    }
    printf(" timer %2d(%-8s) :  %10.4f\n",
        i+1, "sum_ori", t_sum_ori);
    printf(" timer %2d(%-8s) :  %10.4f\n",
        i+2, "sum_part", t_sum_part);
  }


  timer_start(t_part2_norm2u3);
  norm2u3(r_buf, n1, n2, n3, &rnm2, &rnmu, nx[lt], ny[lt], nz[lt], null_offset);
  timer_stop(t_part2_norm2u3);
  printf("rnm2 = %lf, rnmu = %lf\n",rnm2, rnmu);
  old2 = rnm2;
  oldu = rnmu;


  for (it = 1; it <= nit; it++) {
    if ((it == 1) || (it == nit) || ((it % 5) == 0)) {
      //      if (me == root) printf("  iter %4d\n", it);
      printf("  iter %4d\n", it);
    }
    timer_start(t_part3_mg3p);
    mg3P(u_buf, v_buf, r_buf, a, c, n1, n2, n3);
    timer_stop(t_part3_mg3p);
    timer_start(t_part4_resid);
    resid(u_buf, v_buf, r_buf, n1, n2, n3, a, k, null_offset);
    timer_stop(t_part4_resid);
  }


  printf("rnm2 = %lf, rnmu = %lf\n",rnm2, rnmu);
  timer_start(t_part5_norm2u3);
  norm2u3(r_buf, n1, n2, n3, &rnm2, &rnmu, nx[lt], ny[lt], nz[lt], null_offset);
  timer_stop(t_part5_norm2u3);
  printf("rnm2 = %lf, rnmu = %lf\n",rnm2, rnmu);

  timer_stop(t_bench);

  t0 = timer_read(t_bench);

  t = t0;


  verified = false;
  verify_value = 0.0;
  printf("\n Benchmark completed \n");

  epsilon = 1.e-8;
  if (Class != 'U') {
    if (Class == 'S') {
      verify_value = 0.5307707005734e-04;
    } else if (Class == 'W') {
      verify_value = 0.6467329375339e-05;
    } else if (Class == 'A') {
      verify_value = 0.2433365309069e-05;
    } else if (Class == 'B') {
      verify_value = 0.1800564401355e-05;
    } else if (Class == 'C') {
      verify_value = 0.5706732285740e-06;
    } else if (Class == 'D') {
      verify_value = 0.1583275060440e-09;
    } else if (Class == 'E') {
      verify_value = 0.5630442584711e-10;
    }

    err = fabs( rnm2 - verify_value ) / verify_value;
    if (err <= epsilon) {
      verified = true;
      printf(" VERIFICATION SUCCESSFUL\n");
      printf(" L2 Norm is %20.13E\n", rnm2);
      printf(" Error is   %20.13E\n", err);
    } else {
      verified = false;
      printf(" VERIFICATION FAILED\n");
      printf(" L2 Norm is             %20.13E\n", rnm2);
      printf(" The correct L2 Norm is %20.13E\n", verify_value);
    }
  } else {
    verified = false;
    printf(" Problem size unknown\n");
    printf(" NO VERIFICATION PERFORMED\n");
    printf(" L2 Norm is %20.13E\n", rnm2);
  }

  nn = 1.0 * nx[lt] * ny[lt] * nz[lt];

  if (t != 0.0) {
    mflops = 58.0*1.0e-6*nit*nn / t;
  } else {
    mflops = 0.0;
  }

  c_print_results("MG", Class, nx[lt], ny[lt], nz[lt], 
      nit, t,
      mflops, "          floating point", 
      verified, NPBVERSION, COMPILETIME,
      CS1, CS2, CS3, CS4, CS5, CS6, CS7, clu_GetDeviceTypeName(device_type), device_name, num_devices);

  if (timeron) {
    for (i = 0; i < t_last; i++) {
      t1[i] = timer_read(i);

      if( i>t_init && i <t_part1_resid&& i != t_norm2u3_all)
        t_sum_ori += t1[i];
      if( i >= t_part1_resid && i <= t_part5_norm2u3)
        t_sum_part += t1[i];

    }
    t1[t_last+1] = t1[t_rcomm] + t1[t_copy_buffer];
    t1[t_last+0] = t1[t_bench] - t1[t_last+1];

    printf(" nprocs =%6d           minimum     maximum     average\n",
        nprocs);
    for (i = 0; i < t_last+2; i++) {
      t1[i] = t1[i];
      printf(" timer %2d(%-8s) :  %10.4f\n",
          i+1, t_recs[i], t1[i]);
    }
    printf(" timer %2d(%-8s) :  %10.4f\n",
        i+1, "sum_ori", t_sum_ori);
    printf(" timer %2d(%-8s) :  %10.4f\n",
        i+2, "sum_part", t_sum_part);
  }

  free(n1); free(n2); free(n3);
  release_opencl();

  printf("READY CNT[%d] SIZE[%lu]\n", ready_cnt, ready_size);

  return 0;
}


static void setup(int *n1, int *n2, int *n3)
{
  int k;
  int dx, dy, log_p, d, i, j;

  int ax;
  int ng[MAXLEVEL][3];
  int pi[3];
  int dir;

  int (*next)[3];
  int (*idi)[3];
  int (*idin)[3][3];
  int (*mi)[MAXLEVEL][3];
  int (*mip)[MAXLEVEL][3];

  next = (int (*)[3])malloc(sizeof(int)*3 * num_devices);
  idi = (int (*)[3])malloc(sizeof(int)*3 * num_devices);
  idin = (int (*)[3][3])malloc(sizeof(int)*3*3 * num_devices);
  mi = (int (*)[MAXLEVEL][3])malloc(sizeof(int)*MAXLEVEL*3 * num_devices);
  mip = (int (*)[MAXLEVEL][3])malloc(sizeof(int)*MAXLEVEL*3 * num_devices);

  /* variables that have the same values among MPI processes 
   * msg_type
   * ng
   * nx , ny , nz
   * next
   * log_p
   * nprocs
   * dx, dy
   * pi
   * ax
   */

  /* variables that have different values among MPI processes 
   * me
   * idi
   * mi, mip
   * give_ex, take_ex
   * is1, is2, is3
   * ie1, ie2, ie3
   * idin
   * nbr
   * dead
   * m1, m2, m3 
   * ir
   * n1, n2, n3 // ignore this
   */
  for (j = 0; j < 3; j++) {
    for (d = 0; d < 3; d++) {
      msg_type[j][d] = 100*(j+1+10*(d+1));
    }
  }

  ng[lt-1][0] = nx[lt];
  ng[lt-1][1] = ny[lt];
  ng[lt-1][2] = nz[lt];

  for(i = 0; i < num_devices; i++)
  {
    for (ax = 0; ax < 3; ax++) {
      next[i][ax] = 1;
      for (k = lt-2; k >= 0; k--) {
        ng[k][ax] = ng[k+1][ax]/2;
      }
    }
  }

  for (k = lt; k >= 1; k--) {
    nx[k] = ng[k-1][0];
    ny[k] = ng[k-1][1];
    nz[k] = ng[k-1][2];
  }

  log_p  = log((double)(nprocs)+0.0001)/log(2.0);
  dx     = log_p/3;
  pi[0]  = 1 << dx;

  for(i = 0; i < num_devices; i++)
    idi[i][0] = i % pi[0];

  dy     = (log_p-dx)/2;
  pi[1]  = 1 << dy;

  for(i = 0; i < num_devices; i++)
    idi[i][1] = (i/pi[0]) % pi[1];

  pi[2]  = nprocs/(pi[0]*pi[1]);

  for(i = 0; i < num_devices; i++)
    idi[i][2] = i/(pi[0]*pi[1]);

  for(i = 0; i < num_devices; i++)
  {
    for (k = lt-1; k >= 0; k--) {
      dead[i][k] = false;
      for (ax = 0; ax < 3; ax++) {
        take_ex[i][k][ax] = false;
        give_ex[i][k][ax] = false;

        mi[i][k][ax] = 2 + 
          ((idi[i][ax]+1)*ng[k][ax])/pi[ax] -
          ((idi[i][ax]+0)*ng[k][ax])/pi[ax];
        mip[i][k][ax] = 2 + 
          ((next[i][ax]+idi[i][ax]+1)*ng[k][ax])/pi[ax] -
          ((next[i][ax]+idi[i][ax]+0)*ng[k][ax])/pi[ax];

        if (mip[i][k][ax] == 2 || mi[i][k][ax] == 2){
          next[i][ax] = 2*next[i][ax];
        }

        if (k+1 <= lt-1) {
          if ((mip[i][k][ax] == 2) && (mi[i][k][ax] == 3)) {
            give_ex[i][k+1][ax] = true;
          }
          if ((mip[i][k][ax] == 3) && (mi[i][k][ax] == 2)){
            take_ex[i][k+1][ax] = true;
          }
        }
      }

      if (mi[i][k][0] == 2 || mi[i][k][1] == 2 || mi[i][k][2] == 2) {
        dead[i][k] = true;
      }
      m1[k+1][i] = mi[i][k][0];
      m2[k+1][i] = mi[i][k][1];
      m3[k+1][i] = mi[i][k][2];

      for (ax = 0; ax < 3; ax++) {
        idin[i][2][ax] = ( idi[i][ax] + next[i][ax] + pi[ax] ) % pi[ax];
        idin[i][0][ax] = ( idi[i][ax] - next[i][ax] + pi[ax] ) % pi[ax];
      }
      for (dir = 2; dir >= 0; dir -= 2) {
        nbr[i][k][dir][0] = idin[i][dir][0] + pi[0] 
          *(idi[i][1]       + pi[1] * idi[i][2]);
        nbr[i][k][dir][1] = idi[i][0]       + pi[0]
          *(idin[i][dir][1] + pi[1] * idi[i][2]);
        nbr[i][k][dir][2] = idi[i][0]       + pi[0]
          *(idi[i][1]       + pi[1] * idin[i][dir][2]);
      }
    }
  }

  k = lt-1;

  /* all processes have the same value for each n1, n2, and n3 */

  for(i = 0; i < num_devices; i++)
  {
    is1[i] = 2 + ng[k][0] - ((pi[0]  -idi[i][0])*ng[lt-1][0])/pi[0];
    ie1[i] = 1 + ng[k][0] - ((pi[0]-1-idi[i][0])*ng[lt-1][0])/pi[0];
    n1[i] = 3 + ie1[i] - is1[i];
    is2[i] = 2 + ng[k][1] - ((pi[1]  -idi[i][1])*ng[lt-1][1])/pi[1];
    ie2[i] = 1 + ng[k][1] - ((pi[1]-1-idi[i][1])*ng[lt-1][1])/pi[1];
    n2[i] = 3 + ie2[i] - is2[i];
    is3[i] = 2 + ng[k][2] - ((pi[2]  -idi[i][2])*ng[lt-1][2])/pi[2];
    ie3[i] = 1 + ng[k][2] - ((pi[2]-1-idi[i][2])*ng[lt-1][2])/pi[2];
    n3[i] = 3 + ie3[i] - is3[i];

    ir[lt][i] = 0;
    for (j = lt-1; j >= 1; j--) {
      ir[j][i] = ir[j+1][i]+m1[j+1][i]*m2[j+1][i]*m3[j+1][i];
    }
  }

  if (debug_vec[0] >= 1) {
    printf(" in setup, \n");
    printf(" me   k  lt  nx  ny  nz  n1  n2  n3 is1 is2 is3 ie1 ie2 ie3\n");
    for (i = 0; i < nprocs; i++) {
      printf("%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d%4d\n",
          i, k, lt, ng[k][0], ng[k][1], ng[k][2],
          n1[i], n2[i], n3[i], is1[i], is2[i], is3[i], ie1[i], ie2[i], ie3[i]);
    }
  }

  if (debug_vec[0] >= 2) {
    for (i = 0; i < nprocs; i++) {
      printf("\n");
      printf(" processor = %d\n", i);
      for (k = lt; k >= 1; k--) {
        printf("%4didi=%4d%4d%4dnbr=%4d%4d %4d%4d %4d%4d  mi=%4d%4d%4d \n",
            k, idi[i][0], idi[i][1], idi[i][2],
            nbr[i][k][0][0], nbr[i][k][2][0],
            nbr[i][k][0][1], nbr[i][k][2][1],
            nbr[i][k][0][2], nbr[i][k][2][2],
            mi[i][k][0], mi[i][k][1], mi[i][k][2]);
      }
      printf("idi[s] = %d %d %d\n", idi[i][0], idi[i][1], idi[i][2]);
      printf("dead[1], dead[0] = %d %d\n", dead[i][1], dead[i][0]);
      for (ax = 0; ax < 3; ax++) {
        printf("give_ex[1][ax]= %d\n", give_ex[i][1][ax]);
        printf("take_ex[1][ax]= %d\n", take_ex[i][1][ax]);
      }
    }
  }

  free(next);
  free(idi);
  free(idin);
  free(mi);
  free(mip);
}


//---------------------------------------------------------------------
// multigrid V-cycle routine
//---------------------------------------------------------------------
static void mg3P(cl_mem *ou_buf, cl_mem *ov_buf, cl_mem *or_buf,
    double a[4], double c[4], int* n1, int* n2, int* n3)
{
  int j, k;

  //---------------------------------------------------------------------
  // down cycle.
  // restrict the residual from the find grid to the coarse
  //---------------------------------------------------------------------
  for (k = lt; k >= lb+1; k--) {
    j = k - 1;
    rprj3(or_buf, m1[k], m2[k], m3[k], 
        m1[j], m2[j], m3[j], k-1, ir[k], ir[j]);
  }

  k = lb;

  zero3(ou_buf, m1[k], m2[k], m3[k], ir[k]);
  psinv(or_buf, ou_buf, m1[k], m2[k], m3[k], c, k-1, ir[k]);

  for (k = lb+1; k <= lt-1; k++) {
    j = k - 1;
    //---------------------------------------------------------------------
    // prolongate from level k-1  to k
    //---------------------------------------------------------------------
    zero3(ou_buf, m1[k], m2[k], m3[k], ir[k]);
    interp(ou_buf, m1[j], m2[j], m3[j], m1[k], m2[k], m3[k], k-1, ir[j], ir[k]);

    //---------------------------------------------------------------------
    // compute residual for level k
    //---------------------------------------------------------------------
    resid(ou_buf, or_buf, or_buf, m1[k], m2[k], m3[k], a, k-1, ir[k]);

    //---------------------------------------------------------------------
    // apply smoother
    //---------------------------------------------------------------------
    psinv(or_buf, ou_buf, m1[k], m2[k], m3[k], c, k-1, ir[k]);
  }

  j = lt - 1;
  k = lt - 1;
  interp(ou_buf, m1[j], m2[j], m3[j], n1, n2, n3, k, ir[j], null_offset);
  resid(ou_buf, ov_buf, or_buf, n1, n2, n3, a, k, null_offset);
  psinv(or_buf, ou_buf, n1, n2, n3, c, k, null_offset);
}


//---------------------------------------------------------------------
// psinv applies an approximate inverse as smoother:  u = u + Cr
//
// This  implementation costs  15A + 4M per result, where
// A and M denote the costs of Addition and Multiplication.  
// Presuming coefficient c(3) is zero (the NPB assumes this,
// but it is thus not a general case), 2A + 1M may be eliminated,
// resulting in 13A + 3M.
// Note that this vectorizes, and is also fine for cache 
// based machines.  
//---------------------------------------------------------------------
static void psinv(cl_mem *or_buf, cl_mem *ou_buf, int* n1, int* n2, int* n3,
    double c[4], int k, int* offset)
{
  int i;
  cl_int ecode;
  size_t psinv_lws[3], psinv_gws[3];

  if (timeron) timer_start(t_psinv);
  for (i = 0; i < num_devices; i++) 
  {
    if( n3[i] < 3 || n2[i] < 3)
      continue;

    //		printf("psinv (%d) n1 = %d, n2 = %d, n3 = %d\n",i,n1[i],n2[i],n3[i]);
    if(PSINV_DIM == 2)
    {
      psinv_lws[0] = (n2[i]-2) < work_item_sizes[0] ? (n2[i]-2) : work_item_sizes[0];
      int temp = max_work_group_size / psinv_lws[0];
      psinv_lws[1] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

      psinv_gws[0] = clu_RoundWorkSize((size_t)(n2[i]-2), psinv_lws[0]);
      psinv_gws[1] = clu_RoundWorkSize((size_t)(n3[i]-2), psinv_lws[1]);
    }
    else // PSINV_DIM == 1
    {
      int temp = 1;
      psinv_lws[0] = temp == 0 ? 1 : temp;
      psinv_gws[0] = clu_RoundWorkSize((size_t)(n3[i]-2), psinv_lws[0]);
    }

    ecode  = clSetKernelArg(k_psinv[i], 0, sizeof(cl_mem), &or_buf[i]);
    ecode |= clSetKernelArg(k_psinv[i], 1, sizeof(cl_mem), &ou_buf[i]);
    ecode |= clSetKernelArg(k_psinv[i], 2, sizeof(int), &n1[i]);
    ecode |= clSetKernelArg(k_psinv[i], 3, sizeof(int), &n2[i]);
    ecode |= clSetKernelArg(k_psinv[i], 4, sizeof(int), &n3[i]);
    ecode |= clSetKernelArg(k_psinv[i], 5, sizeof(double), &c[0]);
    ecode |= clSetKernelArg(k_psinv[i], 6, sizeof(double), &c[1]);
    ecode |= clSetKernelArg(k_psinv[i], 7, sizeof(double), &c[2]);
    ecode |= clSetKernelArg(k_psinv[i], 8, sizeof(double), &c[3]);
    ecode |= clSetKernelArg(k_psinv[i], 9, sizeof(int), &offset[i]);
    clu_CheckError(ecode, "clSetKernelArg() for k_psinv");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
        k_psinv[i],
        PSINV_DIM, NULL,
        psinv_gws,
        psinv_lws,
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_psinv");
  }

  CHECK_FINISH_DEBUG();

  if (timeron) timer_stop(t_psinv);

  //---------------------------------------------------------------------
  // exchange boundary points
  //---------------------------------------------------------------------
  comm3(ou_buf, n1, n2, n3, k, offset);

  if (debug_vec[0] >= 1) {
    rep_nrm(ou_buf, n1, n2, n3, "   psinv", k, offset);
  }

  if (debug_vec[3] >= k+1) {
    showall(ou_buf, n1, n2, n3, offset);
  }
}


//---------------------------------------------------------------------
// resid computes the residual:  r = v - Au
//
// This  implementation costs  15A + 4M per result, where
// A and M denote the costs of Addition (or Subtraction) and 
// Multiplication, respectively. 
// Presuming coefficient a(1) is zero (the NPB assumes this,
// but it is thus not a general case), 3A + 1M may be eliminated,
// resulting in 12A + 3M.
// Note that this vectorizes, and is also fine for cache 
// based machines.  
//---------------------------------------------------------------------
static void resid(cl_mem *ou_buf, cl_mem *ov_buf, cl_mem *or_buf, int* n1, int* n2, int* n3,
    double a[4], int k, int* offset)
{
  int i;
  cl_int ecode;
  size_t resid_lws[3], resid_gws[3];

  if (timeron) timer_start(t_resid);
  for (i = 0; i < num_devices; i++) 
  {
    if(RESID_DIM == 2)
    {
      resid_lws[0] = (n2[i]-2) < work_item_sizes[0] ? (n2[i]-2) : work_item_sizes[0];
      int temp = max_work_group_size / resid_lws[0];
      resid_lws[1] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

      resid_gws[0] = clu_RoundWorkSize((size_t)(n2[i]-2), resid_lws[0]);
      resid_gws[1] = clu_RoundWorkSize((size_t)(n3[i]-2), resid_lws[1]);
    }
    else // RESID_DIM == 1
    {
      int temp = 1;
      resid_lws[0] = temp == 0 ? 1 : temp;
      resid_gws[0] = clu_RoundWorkSize((size_t)(n3[i]-2), resid_lws[0]);
    }

    ecode  = clSetKernelArg(k_resid[i], 0, sizeof(cl_mem), &ou_buf[i]);
    ecode |= clSetKernelArg(k_resid[i], 1, sizeof(cl_mem), &ov_buf[i]);
    ecode |= clSetKernelArg(k_resid[i], 2, sizeof(cl_mem), &or_buf[i]);
    ecode |= clSetKernelArg(k_resid[i], 3, sizeof(int), &n1[i]);
    ecode |= clSetKernelArg(k_resid[i], 4, sizeof(int), &n2[i]);
    ecode |= clSetKernelArg(k_resid[i], 5, sizeof(int), &n3[i]);
    ecode |= clSetKernelArg(k_resid[i], 6, sizeof(int), &offset[i]);
    clu_CheckError(ecode, "clSetKernelArg() for k_resid");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
        k_resid[i],
        RESID_DIM, NULL,
        resid_gws,
        resid_lws,
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_resid");
  }

  CHECK_FINISH_DEBUG();
  if (timeron) timer_stop(t_resid);

  //---------------------------------------------------------------------
  // exchange boundary data
  //---------------------------------------------------------------------
  comm3(or_buf, n1, n2, n3, k, offset);

  if (debug_vec[0] >= 1) {
    rep_nrm(or_buf, n1, n2, n3, "   resid", k, offset);
  }

  if (debug_vec[2] >= k+1) {
    showall(or_buf, n1, n2, n3, offset);
  }
}


//---------------------------------------------------------------------
// rprj3 projects onto the next coarser grid, 
// using a trilinear Finite Element projection:  s = r' = P r
//     
// This  implementation costs  20A + 4M per result, where
// A and M denote the costs of Addition and Multiplication.  
// Note that this vectorizes, and is also fine for cache 
// based machines.  
//---------------------------------------------------------------------
static void rprj3(cl_mem *or_buf, int* m1k, int* m2k, int* m3k,
    int* m1j, int* m2j, int* m3j, int k, int* offset_r, int* offset_s)
{
  int i;
  cl_int ecode;
  size_t rprj3_lws[3], rprj3_gws[3];

  int d1, d2, d3, j;

  if (timeron) timer_start(t_rprj3);
  for (i = 0; i < num_devices; i++) 
  {
    if( m3j[i] < 3 || m2j[i] < 3)
      continue;

    if (m1k[i] == 3) {
      d1 = 2;
    } else {
      d1 = 1;
    }

    if (m2k[i] == 3) {
      d2 = 2;
    } else {
      d2 = 1;
    }

    if (m3k[i] == 3) {
      d3 = 2;
    } else {
      d3 = 1;
    }

    if(RPRJ3_DIM == 2)
    {
      rprj3_lws[0] = (m2j[i]-2) < work_item_sizes[0] ? (m2j[i]-2) : work_item_sizes[0];
      int temp = max_work_group_size / rprj3_lws[0];
      rprj3_lws[1] = (m3j[i]-2) < temp ? (m3j[i]-2) : temp;

      rprj3_gws[0] = clu_RoundWorkSize((size_t)(m2j[i]-2), rprj3_lws[0]);
      rprj3_gws[1] = clu_RoundWorkSize((size_t)(m3j[i]-2), rprj3_lws[1]);
    }
    else // RPRJ3_DIM == 1
    {
      int temp = 1;
      rprj3_lws[0] = temp == 0 ? 1 : temp;
      rprj3_gws[0] = clu_RoundWorkSize((size_t)(m3j[i]-2), rprj3_lws[0]);
    }

    ecode  = clSetKernelArg(k_rprj3[i], 0, sizeof(cl_mem), &or_buf[i]);
    ecode |= clSetKernelArg(k_rprj3[i], 1, sizeof(int), &m1k[i]);
    ecode |= clSetKernelArg(k_rprj3[i], 2, sizeof(int), &m2k[i]);
    ecode |= clSetKernelArg(k_rprj3[i], 3, sizeof(int), &m3k[i]);
    ecode |= clSetKernelArg(k_rprj3[i], 4, sizeof(int), &m1j[i]);
    ecode |= clSetKernelArg(k_rprj3[i], 5, sizeof(int), &m2j[i]);
    ecode |= clSetKernelArg(k_rprj3[i], 6, sizeof(int), &m3j[i]);
    ecode |= clSetKernelArg(k_rprj3[i], 7, sizeof(int), &offset_r[i]);
    ecode |= clSetKernelArg(k_rprj3[i], 8, sizeof(int), &offset_s[i]);
    ecode |= clSetKernelArg(k_rprj3[i], 9, sizeof(int), &d1);
    ecode |= clSetKernelArg(k_rprj3[i], 10, sizeof(int), &d2);
    ecode |= clSetKernelArg(k_rprj3[i], 11, sizeof(int), &d3);
    clu_CheckError(ecode, "clSetKernelArg() for k_rprj3");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
        k_rprj3[i],
        RPRJ3_DIM, NULL,
        rprj3_gws,
        rprj3_lws,
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_rprj3");
  }
  CHECK_FINISH_DEBUG();
  if (timeron) timer_stop(t_rprj3);

  j = k-1;
  comm3(or_buf, m1j, m2j, m3j, j, offset_s);

  if (debug_vec[0] >= 1) {
    rep_nrm(or_buf, m1j, m2j, m3j, "   rprj3", k-1, offset_s);
  }

  if (debug_vec[4] >= k+1) {
    showall(or_buf, m1j, m2j, m3j, offset_s);
  }
}


//---------------------------------------------------------------------
// interp adds the trilinear interpolation of the correction
// from the coarser grid to the current approximation:  u = u + Qu'
//     
// Observe that this  implementation costs  16A + 4M, where
// A and M denote the costs of Addition and Multiplication.  
// Note that this vectorizes, and is also fine for cache 
// based machines.  Vector machines may get slightly better 
// performance however, with 8 separate "do i1" loops, rather than 4.
//---------------------------------------------------------------------
static void interp(cl_mem *ou_buf, int* mm1, int* mm2, int* mm3,
    int* n1, int* n2, int* n3, int k, int* offset_z, int* offset_u)
{
  int i;
  cl_int ecode;
  size_t interp_1_lws[3], interp_1_gws[3];
  size_t interp_2_lws[3], interp_2_gws[3];
  size_t interp_3_lws[3], interp_3_gws[3];
  size_t interp_4_lws[3], interp_4_gws[3];
  size_t interp_5_lws[3], interp_5_gws[3];

  int d1, d2, d3, t1, t2, t3;

  // note that m = 1037 in globals.h but for this only need to be
  // 535 to handle up to 1024^3
  //      integer m
  //      parameter( m=535 )

  if (timeron) timer_start(t_interp);

  for (i = 0; i < num_devices; i++) 
  {
    if (n1[i] != 3 && n2[i] != 3 && n3[i] != 3) 
    {
      if(INTERP_1_DIM == 2)
      {
        interp_1_lws[0] = (mm2[i]-1) < work_item_sizes[0] ? (mm2[i]-1) : work_item_sizes[0];
        int temp = max_work_group_size / interp_1_lws[0];
        interp_1_lws[1] = (mm3[i]-1) < temp ? (mm3[i]-1) : temp;

        interp_1_gws[0] = clu_RoundWorkSize((size_t)(mm2[i]-1), interp_1_lws[0]);
        interp_1_gws[1] = clu_RoundWorkSize((size_t)(mm3[i]-1), interp_1_lws[1]);
      }
      else // INTERP_1_DIM == 1
      {
        int temp = 1;
        interp_1_lws[0] = temp == 0 ? 1 : temp;
        interp_1_gws[0] = clu_RoundWorkSize((size_t)(mm3[i]-1), interp_1_lws[0]);
      }

      ecode  = clSetKernelArg(k_interp_1[i], 0, sizeof(cl_mem), &ou_buf[i]);
      ecode |= clSetKernelArg(k_interp_1[i], 1, sizeof(int), &mm1[i]);
      ecode |= clSetKernelArg(k_interp_1[i], 2, sizeof(int), &mm2[i]);
      ecode |= clSetKernelArg(k_interp_1[i], 3, sizeof(int), &mm3[i]);
      ecode |= clSetKernelArg(k_interp_1[i], 4, sizeof(int), &n1[i]);
      ecode |= clSetKernelArg(k_interp_1[i], 5, sizeof(int), &n2[i]);
      ecode |= clSetKernelArg(k_interp_1[i], 6, sizeof(int), &n3[i]);
      ecode |= clSetKernelArg(k_interp_1[i], 7, sizeof(int), &offset_z[i]);
      ecode |= clSetKernelArg(k_interp_1[i], 8, sizeof(int), &offset_u[i]);
      clu_CheckError(ecode, "clSetKernelArg() for k_interp_1");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
          k_interp_1[i],
          INTERP_1_DIM, NULL,
          interp_1_gws,
          interp_1_lws,
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_interp_1");
    }
    else {
      if (n1[i] == 3) {
        d1 = 2;
        t1 = 1;
      } else {
        d1 = 1;
        t1 = 0;
      }

      if (n2[i] == 3) {
        d2 = 2;
        t2 = 1;
      } else {
        d2 = 1;
        t2 = 0;
      }

      if (n3[i] == 3) {
        d3 = 2;
        t3 = 1;
      } else {
        d3 = 1;
        t3 = 0;
      }

      if(INTERP_2_DIM == 2)
      {
        interp_2_lws[0] = (mm2[i]-1) < work_item_sizes[0] ? (mm2[i]-1) : work_item_sizes[0];
        int temp = max_work_group_size / interp_2_lws[0];
        interp_2_lws[1] = (mm3[i]-1) < temp ? (mm3[i]-1) : temp;

        interp_2_gws[0] = clu_RoundWorkSize((size_t)(mm2[i]-1), interp_2_lws[0]);
        interp_2_gws[1] = clu_RoundWorkSize((size_t)(mm3[i]-1), interp_2_lws[1]);
      }
      else // INTERP_2_DIM == 1
      {
        int temp = 1;
        interp_2_lws[0] = temp == 0 ? 1 : temp;
        interp_2_gws[0] = clu_RoundWorkSize((size_t)(mm3[i]-1), interp_2_lws[0]);
      }

      ecode  = clSetKernelArg(k_interp_2[i], 0, sizeof(cl_mem), &ou_buf[i]);
      ecode |= clSetKernelArg(k_interp_2[i], 1, sizeof(int), &mm1[i]);
      ecode |= clSetKernelArg(k_interp_2[i], 2, sizeof(int), &mm2[i]);
      ecode |= clSetKernelArg(k_interp_2[i], 3, sizeof(int), &mm3[i]);
      ecode |= clSetKernelArg(k_interp_2[i], 4, sizeof(int), &n1[i]);
      ecode |= clSetKernelArg(k_interp_2[i], 5, sizeof(int), &n2[i]);
      ecode |= clSetKernelArg(k_interp_2[i], 6, sizeof(int), &n3[i]);
      ecode |= clSetKernelArg(k_interp_2[i], 7, sizeof(int), &d1);
      ecode |= clSetKernelArg(k_interp_2[i], 8, sizeof(int), &d2);
      ecode |= clSetKernelArg(k_interp_2[i], 9, sizeof(int), &d3);
      ecode |= clSetKernelArg(k_interp_2[i], 10, sizeof(int), &t1);
      ecode |= clSetKernelArg(k_interp_2[i], 11, sizeof(int), &t2);
      ecode |= clSetKernelArg(k_interp_2[i], 12, sizeof(int), &t3);
      ecode |= clSetKernelArg(k_interp_2[i], 13, sizeof(int), &offset_z[i]);
      ecode |= clSetKernelArg(k_interp_2[i], 14, sizeof(int), &offset_u[i]);
      clu_CheckError(ecode, "clSetKernelArg() for k_interp_2");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
          k_interp_2[i],
          INTERP_2_DIM, NULL,
          interp_2_gws,
          interp_2_lws,
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_interp_2");

      if(INTERP_3_DIM == 2)
      {
        interp_3_lws[0] = (mm2[i]-1) < work_item_sizes[0] ? (mm2[i]-1) : work_item_sizes[0];
        int temp = max_work_group_size / interp_3_lws[0];
        interp_3_lws[1] = (mm3[i]-1) < temp ? (mm3[i]-1) : temp;

        interp_3_gws[0] = clu_RoundWorkSize((size_t)(mm2[i]-1), interp_3_lws[0]);
        interp_3_gws[1] = clu_RoundWorkSize((size_t)(mm3[i]-1), interp_3_lws[1]);
      }
      else // INTERP_3_DIM == 1
      {
        int temp = 1;
        interp_3_lws[0] = temp == 0 ? 1 : temp;
        interp_3_gws[0] = clu_RoundWorkSize((size_t)(mm3[i]-1), interp_3_lws[0]);
      }

      ecode  = clSetKernelArg(k_interp_3[i], 0, sizeof(cl_mem), &ou_buf[i]);
      ecode |= clSetKernelArg(k_interp_3[i], 1, sizeof(int), &mm1[i]);
      ecode |= clSetKernelArg(k_interp_3[i], 2, sizeof(int), &mm2[i]);
      ecode |= clSetKernelArg(k_interp_3[i], 3, sizeof(int), &mm3[i]);
      ecode |= clSetKernelArg(k_interp_3[i], 4, sizeof(int), &n1[i]);
      ecode |= clSetKernelArg(k_interp_3[i], 5, sizeof(int), &n2[i]);
      ecode |= clSetKernelArg(k_interp_3[i], 6, sizeof(int), &n3[i]);
      ecode |= clSetKernelArg(k_interp_3[i], 7, sizeof(int), &d1);
      ecode |= clSetKernelArg(k_interp_3[i], 8, sizeof(int), &d2);
      ecode |= clSetKernelArg(k_interp_3[i], 9, sizeof(int), &d3);
      ecode |= clSetKernelArg(k_interp_3[i], 10, sizeof(int), &t1);
      ecode |= clSetKernelArg(k_interp_3[i], 11, sizeof(int), &t2);
      ecode |= clSetKernelArg(k_interp_3[i], 12, sizeof(int), &t3);
      ecode |= clSetKernelArg(k_interp_3[i], 13, sizeof(int), &offset_z[i]);
      ecode |= clSetKernelArg(k_interp_3[i], 14, sizeof(int), &offset_u[i]);
      clu_CheckError(ecode, "clSetKernelArg() for k_interp_3");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
          k_interp_3[i],
          INTERP_3_DIM, NULL,
          interp_3_gws,
          interp_3_lws,
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_interp_3");

      if(INTERP_4_DIM == 2)
      {
        interp_4_lws[0] = (mm2[i]-1) < work_item_sizes[0] ? (mm2[i]-1) : work_item_sizes[0];
        int temp = max_work_group_size / interp_4_lws[0];
        interp_4_lws[1] = (mm3[i]-1) < temp ? (mm3[i]-1) : temp;

        interp_4_gws[0] = clu_RoundWorkSize((size_t)(mm2[i]-1), interp_4_lws[0]);
        interp_4_gws[1] = clu_RoundWorkSize((size_t)(mm3[i]-1), interp_4_lws[1]);
      }
      else // INTERP_4_DIM == 1
      {
        int temp = 1;
        interp_4_lws[0] = temp == 0 ? 1 : temp;
        interp_4_gws[0] = clu_RoundWorkSize((size_t)(mm3[i]-1), interp_4_lws[0]);
      }

      ecode  = clSetKernelArg(k_interp_4[i], 0, sizeof(cl_mem), &ou_buf[i]);
      ecode |= clSetKernelArg(k_interp_4[i], 1, sizeof(int), &mm1[i]);
      ecode |= clSetKernelArg(k_interp_4[i], 2, sizeof(int), &mm2[i]);
      ecode |= clSetKernelArg(k_interp_4[i], 3, sizeof(int), &mm3[i]);
      ecode |= clSetKernelArg(k_interp_4[i], 4, sizeof(int), &n1[i]);
      ecode |= clSetKernelArg(k_interp_4[i], 5, sizeof(int), &n2[i]);
      ecode |= clSetKernelArg(k_interp_4[i], 6, sizeof(int), &n3[i]);
      ecode |= clSetKernelArg(k_interp_4[i], 7, sizeof(int), &d1);
      ecode |= clSetKernelArg(k_interp_4[i], 8, sizeof(int), &d2);
      ecode |= clSetKernelArg(k_interp_4[i], 9, sizeof(int), &d3);
      ecode |= clSetKernelArg(k_interp_4[i], 10, sizeof(int), &t1);
      ecode |= clSetKernelArg(k_interp_4[i], 11, sizeof(int), &t2);
      ecode |= clSetKernelArg(k_interp_4[i], 12, sizeof(int), &t3);
      ecode |= clSetKernelArg(k_interp_4[i], 13, sizeof(int), &offset_z[i]);
      ecode |= clSetKernelArg(k_interp_4[i], 14, sizeof(int), &offset_u[i]);
      clu_CheckError(ecode, "clSetKernelArg() for k_interp_4");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
          k_interp_4[i],
          INTERP_4_DIM, NULL,
          interp_4_gws,
          interp_4_lws,
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_interp_4");

      if(INTERP_5_DIM == 2)
      {
        interp_5_lws[0] = (mm2[i]-1) < work_item_sizes[0] ? (mm2[i]-1) : work_item_sizes[0];
        int temp = max_work_group_size / interp_5_lws[0];
        interp_5_lws[1] = (mm3[i]-1) < temp ? (mm3[i]-1) : temp;

        interp_5_gws[0] = clu_RoundWorkSize((size_t)(mm2[i]-1), interp_5_lws[0]);
        interp_5_gws[1] = clu_RoundWorkSize((size_t)(mm3[i]-1), interp_5_lws[1]);
      }
      else // INTERP_5_DIM == 1
      {
        int temp = 1;
        interp_5_lws[0] = temp == 0 ? 1 : temp;
        interp_5_gws[0] = clu_RoundWorkSize((size_t)(mm3[i]-1), interp_5_lws[0]);
      }

      ecode  = clSetKernelArg(k_interp_5[i], 0, sizeof(cl_mem), &ou_buf[i]);
      ecode |= clSetKernelArg(k_interp_5[i], 1, sizeof(int), &mm1[i]);
      ecode |= clSetKernelArg(k_interp_5[i], 2, sizeof(int), &mm2[i]);
      ecode |= clSetKernelArg(k_interp_5[i], 3, sizeof(int), &mm3[i]);
      ecode |= clSetKernelArg(k_interp_5[i], 4, sizeof(int), &n1[i]);
      ecode |= clSetKernelArg(k_interp_5[i], 5, sizeof(int), &n2[i]);
      ecode |= clSetKernelArg(k_interp_5[i], 6, sizeof(int), &n3[i]);
      ecode |= clSetKernelArg(k_interp_5[i], 7, sizeof(int), &d1);
      ecode |= clSetKernelArg(k_interp_5[i], 8, sizeof(int), &d2);
      ecode |= clSetKernelArg(k_interp_5[i], 9, sizeof(int), &d3);
      ecode |= clSetKernelArg(k_interp_5[i], 10, sizeof(int), &t1);
      ecode |= clSetKernelArg(k_interp_5[i], 11, sizeof(int), &t2);
      ecode |= clSetKernelArg(k_interp_5[i], 12, sizeof(int), &t3);
      ecode |= clSetKernelArg(k_interp_5[i], 13, sizeof(int), &offset_z[i]);
      ecode |= clSetKernelArg(k_interp_5[i], 14, sizeof(int), &offset_u[i]);
      clu_CheckError(ecode, "clSetKernelArg() for k_interp_5");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
          k_interp_5[i],
          INTERP_5_DIM, NULL,
          interp_5_gws,
          interp_5_lws,
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_interp_5");
    }
  } 
  CHECK_FINISH_DEBUG();
  if (timeron) timer_stop(t_interp);

  comm3_ex(ou_buf, n1, n2, n3, k, offset_u);

  if (debug_vec[0] >= 1) {
    rep_nrm(ou_buf, mm1, mm2, mm3, "z: inter", k-1, offset_z);
    rep_nrm(ou_buf, n1, n2, n3, "u: inter", k, offset_u);
  }

  if (debug_vec[5] >= k+1) {
    showall(ou_buf, mm1, mm2, mm3, offset_z);
    showall(ou_buf, n1, n2, n3, offset_u);
  }
}


//---------------------------------------------------------------------
// norm2u3 evaluates approximations to the L2 norm and the
// uniform (or L-infinity or Chebyshev) norm, under the
// assumption that the boundaries are periodic or zero.  Add the
// boundaries in with half weight (quarter weight on the edges
// and eighth weight at the corners) for inhomogeneous boundaries.
//---------------------------------------------------------------------
static void norm2u3(cl_mem *or_buf, int* n1, int* n2, int* n3,
    double *rnm2, double *rnmu,
    int nx0, int ny0, int nz0, int* offset)
{
  double s, ss;
  int i3, i2, i1;
  int i;
  unsigned int addr;
  size_t norm2u3_lws[3], norm2u3_gws[3];

  double dn;

  if (timeron) timer_start(t_norm2u3_all);
  if (timeron) timer_start(t_norm2u3);
  dn = 1.0*nx0*ny0*nz0;

  int local_size;
  int (*group_size)[3] = (int (*)[3])malloc(sizeof(int)*3*num_devices);
  int* group_total_size = (int*)malloc(sizeof(int)*num_devices);
  double** res_sum = (double**)malloc(sizeof(double*)*num_devices);
  double** res_max = (double**)malloc(sizeof(double*)*num_devices);
  double* s_dev = (double*)malloc(sizeof(double)*num_devices);
  double* max_rnmu = (double*)malloc(sizeof(double)*num_devices);
  cl_mem *d_sum = (cl_mem*)malloc(sizeof(cl_mem)*num_devices);
  cl_mem *d_max = (cl_mem*)malloc(sizeof(cl_mem)*num_devices);
  cl_int ecode;

  for (i = 0; i < num_devices; i++) 
  {
    if(NORM2U3_DIM == 3)
    {
      norm2u3_lws[0] = (n1[i]-2) < work_item_sizes[0] ? (n1[i]-2) : work_item_sizes[0];
      int temp = max_work_group_size / norm2u3_lws[0];
      norm2u3_lws[1] = (n2[i]-2) < temp ? (n2[i]-2) : temp;
      temp = temp / norm2u3_lws[1];
      norm2u3_lws[2] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

      norm2u3_gws[0] = clu_RoundWorkSize((size_t)(n1[i]-2), norm2u3_lws[0]);
      norm2u3_gws[1] = clu_RoundWorkSize((size_t)(n2[i]-2), norm2u3_lws[1]);
      norm2u3_gws[2] = clu_RoundWorkSize((size_t)(n3[i]-2), norm2u3_lws[2]);

      local_size = norm2u3_lws[0] * norm2u3_lws[1] * norm2u3_lws[2];
      group_size[i][2] = norm2u3_gws[2]/norm2u3_lws[2];
      group_size[i][1] = norm2u3_gws[1]/norm2u3_lws[1];
      group_size[i][0] = norm2u3_gws[0]/norm2u3_lws[0];
      group_total_size[i] = group_size[i][2] * group_size[i][1] * group_size[i][0];
    }
    else if(NORM2U3_DIM == 2)
    {
      norm2u3_lws[0] = (n2[i]-2) < work_item_sizes[0] ? (n2[i]-2) : work_item_sizes[0];
      int temp = max_work_group_size / norm2u3_lws[0];
      norm2u3_lws[1] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

      norm2u3_gws[0] = clu_RoundWorkSize((size_t)(n2[i]-2), norm2u3_lws[0]);
      norm2u3_gws[1] = clu_RoundWorkSize((size_t)(n3[i]-2), norm2u3_lws[1]);

      local_size = norm2u3_lws[0] * norm2u3_lws[1];
      group_size[i][1] = norm2u3_gws[1]/norm2u3_lws[1];
      group_size[i][0] = norm2u3_gws[0]/norm2u3_lws[0];
      group_total_size[i] = group_size[i][1] * group_size[i][0];
    }
    else // NORM2U3_DIM == 1
    {
      int temp = 1;
      norm2u3_lws[0] = temp == 0 ? 1 : temp;
      norm2u3_gws[0] = clu_RoundWorkSize((size_t)(n3[i]-2), norm2u3_lws[0]);
      local_size = norm2u3_lws[0];
      group_size[i][0] = norm2u3_gws[0]/norm2u3_lws[0];
      group_total_size[i] = group_size[i][0];
    }

    d_sum[i] = clCreateBuffer(context, CL_MEM_WRITE_ONLY, group_total_size[i] * sizeof(double), NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for d_sum");
    d_max[i] = clCreateBuffer(context, CL_MEM_WRITE_ONLY, group_total_size[i] * sizeof(double), NULL, &ecode);
    clu_CheckError(ecode, "clCreateBuffer() for d_max");

    ecode  = clSetKernelArg(k_norm2u3[i], 0, sizeof(cl_mem), &or_buf[i]);
    ecode |= clSetKernelArg(k_norm2u3[i], 1, sizeof(int), &n1[i]);
    ecode |= clSetKernelArg(k_norm2u3[i], 2, sizeof(int), &n2[i]);
    ecode |= clSetKernelArg(k_norm2u3[i], 3, sizeof(int), &n3[i]);
    ecode |= clSetKernelArg(k_norm2u3[i], 4, sizeof(cl_mem), &d_sum[i]);
    ecode |= clSetKernelArg(k_norm2u3[i], 5, sizeof(cl_mem), &d_max[i]);
    ecode |= clSetKernelArg(k_norm2u3[i], 6, sizeof(int), &offset[i]);
    clu_CheckError(ecode, "clSetKernelArg() for k_norm2u3");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
        k_norm2u3[i],
        NORM2U3_DIM, NULL,
        norm2u3_gws,
        norm2u3_lws,
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_norm2u3");
  }
  CHECK_FINISH_DEBUG();

  for (i = 0; i < num_devices; i++) 
  {
    res_sum[i] = (double*)malloc(sizeof(double)*group_total_size[i]);
    res_max[i] = (double*)malloc(sizeof(double)*group_total_size[i]);
    s_dev[i] = 0.0;
    max_rnmu[i] = 0.0;

    ecode = clEnqueueReadBuffer(cmd_queue[i], d_sum[i], CL_FALSE, 0, (group_total_size[i]) * sizeof(double), res_sum[i], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueReadBuffer()");
    ecode = clEnqueueReadBuffer(cmd_queue[i], d_max[i], CL_FALSE, 0, (group_total_size[i]) * sizeof(double), res_max[i], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueReadBuffer()");
  }
  CHECK_FINISH();

  for (i = 0; i < num_devices; i++) 
  {
    if(NORM2U3_DIM == 3)
    {
      for( i3=0; i3<group_size[i][2]; i3++)
      {
        for( i2=0; i2<group_size[i][1]; i2++)
        {
          for( i1=0; i1<group_size[i][0]; i1++)
          {
            addr = i3*group_size[i][1]*group_size[i][0]+i2*group_size[i][0]+i1;
            s_dev[i] = s_dev[i] + res_sum[i][addr];
            if (max_rnmu[i] < res_max[i][addr]) max_rnmu[i] = res_max[i][addr];
          }
        }
      } 
    }
    else if(NORM2U3_DIM == 2)
    {
      for( i2=0; i2<group_size[i][1]; i2++)
      {
        for( i1=0; i1<group_size[i][0]; i1++)
        {
          addr = i2*group_size[i][0]+i1;
          s_dev[i] = s_dev[i] + res_sum[i][addr];
          if (max_rnmu[i] < res_max[i][addr]) max_rnmu[i] = res_max[i][addr];
        }
      } 
    }
    else
    {
      for( i1=0; i1<group_size[i][0]; i1++)
      {
        addr = i1;
        s_dev[i] = s_dev[i] + res_sum[i][addr];
        if (max_rnmu[i] < res_max[i][addr]) max_rnmu[i] = res_max[i][addr];
      } 
    }
  }

  for (i = 0; i < num_devices; i++) 
  {
    clReleaseMemObject(d_sum[i]);
    clReleaseMemObject(d_max[i]);
    free(res_sum[i]);
    free(res_max[i]);
  }
  free(d_sum);
  free(d_max);
  free(group_size);
  free(group_total_size);
  free(res_sum);
  free(res_max);

  CHECK_FINISH_DEBUG();
  if (timeron) timer_stop(t_norm2u3);

  if (timeron) timer_start(t_rcomm);
  ss = 0.0;
  for (i = 0; i < num_devices; i++) 
  {
    if( max_rnmu[i] > ss )
      ss = max_rnmu[i];
  }
  *rnmu = ss;

  ss = 0.0;
  for (i = 0; i < num_devices; i++) 
  {
    ss += s_dev[i];
  }
  s = ss;
  if (timeron) timer_stop(t_rcomm);

  if (timeron) timer_stop(t_norm2u3_all);
  *rnm2 = sqrt( s / dn );

  free(s_dev);
  free(max_rnmu);
}


//---------------------------------------------------------------------
// report on norm
//---------------------------------------------------------------------
static void rep_nrm(cl_mem *ou_buf, int* n1, int* n2, int* n3, char *title, int kk, int* offset)
{
  double rnm2, rnmu;

  norm2u3(ou_buf, n1, n2, n3, &rnm2, &rnmu, nx[kk], ny[kk], nz[kk], offset);

  printf(" Level%2d in %8s: norms =%21.14E%21.14E\n", kk, title, rnm2, rnmu);

}


//---------------------------------------------------------------------
// comm3 organizes the communication on all borders 
//---------------------------------------------------------------------
static void comm3(cl_mem *ou_buf, int *n1, int *n2, int *n3, int kk, int* offset)
{
  int axis, i;

  timer_start(t_comm3);

  if (nprocs == 1) {
    if (!dead[0][kk]) {
      for (axis = 0; axis < 3; axis++) {
        comm1p( axis, ou_buf, n1, n2, n3, kk , offset);
      }
    }
    else{
      zero3_for_one_dev(ou_buf, n1, n2, n3, 0, offset);
      CHECK_FINISH_DEBUG();
    }
  }
  else
  {
    for (i = 0; i < num_devices; i++) 
    {
      if (dead[i][kk]) {
        zero3_for_one_dev(ou_buf, n1, n2, n3, i, offset);
      }
    }
    CHECK_FINISH_DEBUG();
    for (axis = 0; axis < 3; axis++) 
    {
      if (timeron) timer_start(t_ready);
      for (i = 0; i < num_devices; i++) 
        if (!dead[i][kk]) 
          ready( axis, -1, kk , i);
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_ready);
      if (timeron) timer_start(t_ready);
      for (i = 0; i < num_devices; i++) 
        if (!dead[i][kk]) 
          ready( axis, +1, kk , i);
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_ready);
      if (timeron) timer_start(t_give3);
      for (i = 0; i < num_devices; i++) 
        if (!dead[i][kk]) 
          give3( axis, +1, ou_buf, n1, n2, n3, kk , i, offset);
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_give3);
      if (timeron) timer_start(t_give3);
      for (i = 0; i < num_devices; i++) 
        if (!dead[i][kk]) 
          give3( axis, -1, ou_buf, n1, n2, n3, kk , i, offset);
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_give3);
      if (timeron) timer_start(t_copy_buffer);
      for (i = 0; i < num_devices; i++) 
      {
        if (!dead[i][kk]) 
        {
          copy_buffer(axis, -1, i);
        }
      }
      CHECK_FINISH();
      if (timeron) timer_stop(t_copy_buffer);
      if (timeron) timer_start(t_take3);
      for (i = 0; i < num_devices; i++) 
      {
        if (!dead[i][kk]) 
        {
          take3( axis, -1, ou_buf, n1, n2, n3 , i, offset);
        }
      }
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_take3);
      if (timeron) timer_start(t_copy_buffer);
      for (i = 0; i < num_devices; i++) 
      {
        if (!dead[i][kk]) 
        {
          copy_buffer(axis, +1, i);
        }
      }
      CHECK_FINISH();
      if (timeron) timer_stop(t_copy_buffer);
      if (timeron) timer_start(t_take3);
      for (i = 0; i < num_devices; i++) 
      {
        if (!dead[i][kk]) 
        {
          take3( axis, +1, ou_buf, n1, n2, n3 , i, offset);
        }
      }
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_take3);
    }
  }
  timer_stop(t_comm3);
}


//---------------------------------------------------------------------
// comm3_ex  communicates to expand the number of processors
//---------------------------------------------------------------------
//static void comm3_ex(cl_mem *ou_buf, int *n1, int *n2, int *n3, int kk, int* offset)
//{
//  int axis, i;
//
//	for (axis = 0; axis < 3; axis++) {
//		for (i = 0; i < num_devices; i++) 
//		{
//			if (nprocs != 1) {
//				if (take_ex[i][kk][axis]) {
//					ready( axis, -1, kk , i);
//					ready( axis, +1, kk , i);
//				}
//				if (give_ex[i][kk][axis]) {
//					give3_ex( axis, +1, ou_buf, n1, n2, n3, kk , i, offset);
//					give3_ex( axis, -1, ou_buf, n1, n2, n3, kk , i, offset);
//				}
//			} else {
//				comm1p_ex( axis, ou_buf, n1, n2, n3, kk , offset);
//			}
//		}
//		for (i = 0; i < num_devices; i++) 
//		{
//			if (nprocs != 1) {
//				if (take_ex[i][kk][axis]) {
//					copy_buffer( axis, -1, i);
//					copy_buffer( axis, +1, i);
//				}
//			}
//		}
//		for (i = 0; i < num_devices; i++) 
//		{
//			if (nprocs != 1) {
//				if (take_ex[i][kk][axis]) {
//					take3_ex( axis, -1, ou_buf, n1, n2, n3 , i, offset);
//					take3_ex( axis, +1, ou_buf, n1, n2, n3 , i, offset);
//				}
//			}
//		}
//	}
//}
static void comm3_ex(cl_mem *ou_buf, int *n1, int *n2, int *n3, int kk, int* offset)
{
  int axis, i;

  timer_start(t_comm3_ex);

  if (nprocs == 1) {
    for (axis = 0; axis < 3; axis++) {
      comm1p_ex( axis, ou_buf, n1, n2, n3, kk , offset);
    }
  }
  else
  {
    for (axis = 0; axis < 3; axis++) {
      if (timeron) timer_start(t_ready);
      for (i = 0; i < num_devices; i++) 
        if (take_ex[i][kk][axis]) 
          ready( axis, -1, kk , i);
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_ready);
      if (timeron) timer_start(t_ready);
      for (i = 0; i < num_devices; i++) 
        if (take_ex[i][kk][axis]) 
          ready( axis, +1, kk , i);
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_ready);
      if (timeron) timer_start(t_give3ex);
      for (i = 0; i < num_devices; i++) 
        if (give_ex[i][kk][axis]) 
          give3_ex( axis, +1, ou_buf, n1, n2, n3, kk , i, offset);
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_give3ex);
      if (timeron) timer_start(t_give3ex);
      for (i = 0; i < num_devices; i++) 
        if (give_ex[i][kk][axis]) 
          give3_ex( axis, -1, ou_buf, n1, n2, n3, kk , i, offset);
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_give3ex);
      if (timeron) timer_start(t_copy_buffer);
      for (i = 0; i < num_devices; i++) 
      {
        if (take_ex[i][kk][axis]) 
        {
          copy_buffer( axis, -1, i);
        }
      }
      CHECK_FINISH();
      if (timeron) timer_stop(t_copy_buffer);
      if (timeron) timer_start(t_take3ex);
      for (i = 0; i < num_devices; i++) 
      {
        if (take_ex[i][kk][axis]) 
        {
          take3_ex( axis, -1, ou_buf, n1, n2, n3 , i, offset);
        }
      }
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_take3ex);
      if (timeron) timer_start(t_copy_buffer);
      for (i = 0; i < num_devices; i++) 
      {
        if (take_ex[i][kk][axis]) 
        {
          copy_buffer( axis, +1, i);
        }
      }
      CHECK_FINISH();
      if (timeron) timer_stop(t_copy_buffer);
      if (timeron) timer_start(t_take3ex);
      for (i = 0; i < num_devices; i++) 
      {
        if (take_ex[i][kk][axis]) 
        {
          take3_ex( axis, +1, ou_buf, n1, n2, n3 , i, offset);
        }
      }
      CHECK_FINISH_DEBUG();
      if (timeron) timer_stop(t_take3ex);
    }
  }
  timer_stop(t_comm3_ex);
}

//---------------------------------------------------------------------
// ready allocates a buffer to take in a message
//---------------------------------------------------------------------
static void ready(int axis, int dir, int k, int i)
{
	cl_int ecode;
	int buff_id;

	buff_id = 2 + dir;

	recv_params[i][1+dir][axis].dst_offset = buff_id * NM2 * sizeof(double);
}

//---------------------------------------------------------------------
// give3 sends border data out in the requested direction
//---------------------------------------------------------------------
static void give3(int axis, int dir, cl_mem *ou_buf, int* n1, int* n2, int* n3, int k, int i, int* offset)
{
  cl_int ecode;
  int buff_id, buff_len;

  buff_id = 1 + dir;

  size_t give3_1_lws[3], give3_1_gws[3];
  cl_kernel* k_give3_1 = NULL;

  if(axis == 0)
  {
    k_give3_1 = k_give3_1_axis0;
    buff_len = (n3[i]-2) * (n2[i]-2);
  }
  if(axis == 1)
  {
    k_give3_1 = k_give3_1_axis1;
    buff_len = (n3[i]-2) * n1[i];
  }
  if(axis == 2)
  {
    k_give3_1 = k_give3_1_axis2;
    buff_len = n2[i] * n1[i];
  }

  if (axis == 0) {
    if(GIVE3_1_DIM == 2)
    {
      give3_1_lws[0] = (n2[i]-2) < work_item_sizes[0] ? (n2[i]-2) : work_item_sizes[0];
      int temp = max_work_group_size / give3_1_lws[0];
      give3_1_lws[1] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

      give3_1_gws[0] = clu_RoundWorkSize((size_t)(n2[i]-2), give3_1_lws[0]);
      give3_1_gws[1] = clu_RoundWorkSize((size_t)(n3[i]-2), give3_1_lws[1]);
    }
    else // GIVE3_1_DIM == 1
    {
      int temp = 1;
      give3_1_lws[0] = temp == 0 ? 1 : temp;
      give3_1_gws[0] = clu_RoundWorkSize((size_t)(n3[i]-2), give3_1_lws[0]);
    }
  }
  if (axis == 1) {
    if(GIVE3_1_DIM == 2)
    {
      give3_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
      int temp = max_work_group_size / give3_1_lws[0];
      give3_1_lws[1] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

      give3_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], give3_1_lws[0]);
      give3_1_gws[1] = clu_RoundWorkSize((size_t)(n3[i]-2), give3_1_lws[1]);
    }
    else // GIVE3_1_DIM == 1
    {
      int temp = 1;
      give3_1_lws[0] = temp == 0 ? 1 : temp;
      give3_1_gws[0] = clu_RoundWorkSize((size_t)(n3[i]-2), give3_1_lws[0]);
    }
  }
  if (axis == 2) {
    if(GIVE3_1_DIM == 2)
    {
      give3_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
      int temp = max_work_group_size / give3_1_lws[0];
      give3_1_lws[1] = n2[i] < temp ? n2[i] : temp;

      give3_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], give3_1_lws[0]);
      give3_1_gws[1] = clu_RoundWorkSize((size_t)n2[i], give3_1_lws[1]);
    }
    else // GIVE3_1_DIM == 1
    {
      int temp = 1;
      give3_1_lws[0] = temp == 0 ? 1 : temp;
      give3_1_gws[0] = clu_RoundWorkSize((size_t)n2[i], give3_1_lws[0]);
    }
  }

  ecode  = clSetKernelArg(k_give3_1[i], 0, sizeof(cl_mem), &ou_buf[i]);
  ecode |= clSetKernelArg(k_give3_1[i], 1, sizeof(cl_mem), &buff_buf[i]);
  ecode |= clSetKernelArg(k_give3_1[i], 2, sizeof(int), &n1[i]);
  ecode |= clSetKernelArg(k_give3_1[i], 3, sizeof(int), &n2[i]);
  ecode |= clSetKernelArg(k_give3_1[i], 4, sizeof(int), &n3[i]);
  ecode |= clSetKernelArg(k_give3_1[i], 5, sizeof(int), &dir);
  ecode |= clSetKernelArg(k_give3_1[i], 6, sizeof(int), &offset[i]);
  clu_CheckError(ecode, "clSetKernelArg() for k_give3_1");

  ecode = clEnqueueNDRangeKernel(cmd_queue[i],
      k_give3_1[i],
      GIVE3_1_DIM, NULL,
      give3_1_gws,
      give3_1_lws,
      0, NULL, &send_events[i][1+dir][axis]);
  clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_give3_1");

//  printf("GIVE3 NDR [%lu / %lu = %lu]\n", give3_1_gws[0], give3_1_lws[0], (give3_1_gws[0] / give3_1_lws[0]) * (give3_1_gws[1] / give3_1_lws[1]));

  int dst_dev = nbr[i][k][1+dir][axis];
  recv_params[dst_dev][1+dir][axis].src_id = i;
  recv_params[dst_dev][1+dir][axis].src_buf = buff_buf[i];
  recv_params[dst_dev][1+dir][axis].dst_buf = buff_buf[dst_dev];
  recv_params[dst_dev][1+dir][axis].src_offset = buff_id * NM2 * sizeof(double);
  recv_params[dst_dev][1+dir][axis].cb = buff_len*sizeof(double);
  recv_params[dst_dev][1+dir][axis].event = &send_events[i][1+dir][axis];

}
static void copy_buffer(int axis, int dir, int i)
{
  cl_int ecode;
  size_t copy_buffer_lws[3], copy_buffer_gws[3];

  //	printf("COPY BUFFER SIZE = %d \n", recv_params[i][1+dir][axis].cb);

  //---------------------------------------------------------------------
  // fake message request type
  //---------------------------------------------------------------------

  if( i == recv_params[i][1+dir][axis].src_id )
  {
    int size = recv_params[i][1+dir][axis].cb / sizeof(double);

    if(COPY_BUFFER_DIM  == 1)
    {
      int temp = 1024;
      copy_buffer_lws[0] = temp == 0 ? 1 : temp;
      copy_buffer_gws[0] = clu_RoundWorkSize((size_t)(size), copy_buffer_lws[0]);
    }


    ecode  = clSetKernelArg(k_copy_buffer[i], 0, sizeof(cl_mem), &recv_params[i][1+dir][axis].src_buf);
    ecode |= clSetKernelArg(k_copy_buffer[i], 1, sizeof(cl_mem), &recv_params[i][1+dir][axis].dst_buf);
    ecode |= clSetKernelArg(k_copy_buffer[i], 2, sizeof(int), &recv_params[i][1+dir][axis].src_offset);
    ecode |= clSetKernelArg(k_copy_buffer[i], 3, sizeof(int), &recv_params[i][1+dir][axis].dst_offset);
    ecode |= clSetKernelArg(k_copy_buffer[i], 4, sizeof(int), &size);


    if (COPY_BUFFER_DIM  == 1)
    {
      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
          k_copy_buffer[i],
          COPY_BUFFER_DIM, NULL,
          copy_buffer_gws,
          copy_buffer_lws,
          1, recv_params[i][1+dir][axis].event, &recv_events[i][0][1+dir][axis]);
      clu_CheckError(ecode, "clEnqueueTask()");
    }
    else
    {
      ecode = clEnqueueTask(cmd_queue[i],
          k_copy_buffer[i],
          1, recv_params[i][1+dir][axis].event, &recv_events[i][0][1+dir][axis]);
      clu_CheckError(ecode, "clEnqueueTask()");
    }
  }
  else
  {
    ecode = clEnqueueCopyBuffer(cmd_queue[i],
        recv_params[i][1+dir][axis].src_buf,
        recv_params[i][1+dir][axis].dst_buf,
        recv_params[i][1+dir][axis].src_offset,
        recv_params[i][1+dir][axis].dst_offset,
        recv_params[i][1+dir][axis].cb,
        1, recv_params[i][1+dir][axis].event, &recv_events[i][0][1+dir][axis]);
    clu_CheckError(ecode, "clEnqueueCopyBuffer()");
  }
  // recv end
}


//---------------------------------------------------------------------
// take3 copies in border data from the requested direction
//---------------------------------------------------------------------
static void take3(int axis, int dir, cl_mem *ou_buf, int* n1, int* n2, int* n3, int i, int* offset)
{
  cl_int ecode;

  if (timeron) timer_start(t_copy_buffer);
  ecode = clWaitForEvents(1, &recv_events[i][0][1+dir][axis]);
  clu_CheckError(ecode, "clWaitForEvents()");
  if (timeron) timer_stop(t_copy_buffer);
  //	printf("COPY BUFFER RECV \n");
  
  size_t take3_1_lws[3], take3_1_gws[3];
  cl_kernel *k_take3_1 = NULL;

  if(axis == 0)
    k_take3_1 = k_take3_1_axis0;
  if(axis == 1)
    k_take3_1 = k_take3_1_axis1;
  if(axis == 2)
    k_take3_1 = k_take3_1_axis2;

  if (axis == 0) {
    if(TAKE3_1_DIM == 2)
    {
      take3_1_lws[0] = (n2[i]-2) < work_item_sizes[0] ? (n2[i]-2) : work_item_sizes[0];
      int temp = max_work_group_size / take3_1_lws[0];
      take3_1_lws[1] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

      take3_1_gws[0] = clu_RoundWorkSize((size_t)(n2[i]-2), take3_1_lws[0]);
      take3_1_gws[1] = clu_RoundWorkSize((size_t)(n3[i]-2), take3_1_lws[1]);
    }
    else // TAKE3_1_DIM == 1
    {
      int temp = 1;
      take3_1_lws[0] = temp == 0 ? 1 : temp;
      take3_1_gws[0] = clu_RoundWorkSize((size_t)(n3[i]-2), take3_1_lws[0]);
    }
  }
  if (axis == 1) {
    if(TAKE3_1_DIM == 2)
    {
      take3_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
      int temp = max_work_group_size / take3_1_lws[0];
      take3_1_lws[1] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

      take3_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], take3_1_lws[0]);
      take3_1_gws[1] = clu_RoundWorkSize((size_t)(n3[i]-2), take3_1_lws[1]);
    }
    else // TAKE3_1_DIM == 1
    {
      int temp = 1;
      take3_1_lws[0] = temp == 0 ? 1 : temp;
      take3_1_gws[0] = clu_RoundWorkSize((size_t)(n3[i]-2), take3_1_lws[0]);
    }
  }
  if (axis == 2) {
    if(TAKE3_1_DIM == 2)
    {
      take3_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
      int temp = max_work_group_size / take3_1_lws[0];
      take3_1_lws[1] = n2[i] < temp ? n2[i] : temp;

      take3_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], take3_1_lws[0]);
      take3_1_gws[1] = clu_RoundWorkSize((size_t)n2[i], take3_1_lws[1]);
    }
    else // TAKE3_1_DIM == 1
    {
      int temp = 1;
      take3_1_lws[0] = temp == 0 ? 1 : temp;
      take3_1_gws[0] = clu_RoundWorkSize((size_t)n2[i], take3_1_lws[0]);
    }
  }

  ecode  = clSetKernelArg(k_take3_1[i], 0, sizeof(cl_mem), &ou_buf[i]);
  ecode |= clSetKernelArg(k_take3_1[i], 1, sizeof(cl_mem), &buff_buf[i]);
  ecode |= clSetKernelArg(k_take3_1[i], 2, sizeof(int), &n1[i]);
  ecode |= clSetKernelArg(k_take3_1[i], 3, sizeof(int), &n2[i]);
  ecode |= clSetKernelArg(k_take3_1[i], 4, sizeof(int), &n3[i]);
  ecode |= clSetKernelArg(k_take3_1[i], 5, sizeof(int), &dir);
  ecode |= clSetKernelArg(k_take3_1[i], 6, sizeof(int), &offset[i]);
  clu_CheckError(ecode, "clSetKernelArg() for k_take3_1");

  ecode = clEnqueueNDRangeKernel(cmd_queue[i],
      k_take3_1[i],
      TAKE3_1_DIM, NULL,
      take3_1_gws,
      take3_1_lws,
      0, NULL, NULL);
  clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_take3_1");
}


//---------------------------------------------------------------------
// give3_ex sends border data out to expand number of processors
//---------------------------------------------------------------------
static void give3_ex(int axis, int dir, cl_mem *ou_buf, int* n1, int* n2, int* n3,
    int k, int i, int* offset)
{
  cl_int ecode;
  int buff_id, buff_len;

  buff_id = 1 + dir;

  size_t give3ex_1_lws[3], give3ex_1_gws[3];
  cl_kernel* k_give3ex_1 = NULL;

  if(dir == -1)
  {
    GIVE3EX_1_DIM = COMM1PEX_5_DIM;
    if(axis == 0)
    {
      k_give3ex_1 = k_comm1pex_5_axis0;
      buff_len = n3[i] * n2[i];
    }
    if(axis == 1)
    {
      k_give3ex_1 = k_comm1pex_5_axis1;
      buff_len = n3[i] * n1[i];
    }
    if(axis == 2)
    {
      k_give3ex_1 = k_comm1pex_5_axis2;
      buff_len = n2[i] * n1[i];
    }
  }
  else
  {
    GIVE3EX_1_DIM = COMM1PEX_4_DIM;
    if(axis == 0)
    {
      k_give3ex_1 = k_comm1pex_4_axis0;
      buff_len = n3[i] * n2[i] * 2;
    }
    if(axis == 1)
    {
      k_give3ex_1 = k_comm1pex_4_axis1;
      buff_len = n3[i] * n1[i] * 2;
    }
    if(axis == 2)
    {
      k_give3ex_1 = k_comm1pex_4_axis2;
      buff_len = n2[i] * n1[i] * 2;
    }
  }
  if(dir == -1)
  {
    if (axis == 0) {
      if(GIVE3EX_1_DIM == 2)
      {
        give3ex_1_lws[0] = n2[i] < work_item_sizes[0] ? n2[i] : work_item_sizes[0];
        int temp = max_work_group_size / give3ex_1_lws[0];
        give3ex_1_lws[1] = n3[i] < temp ? n3[i] : temp;

        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)n2[i], give3ex_1_lws[0]);
        give3ex_1_gws[1] = clu_RoundWorkSize((size_t)n3[i], give3ex_1_lws[1]);
      }
      else // GIVE3EX_1_DIM == 1
      {
        int temp = 1;
        give3ex_1_lws[0] = temp == 0 ? 1 : temp;
        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)n3[i], give3ex_1_lws[0]);
      }
    }
    if (axis == 1) {
      if(GIVE3EX_1_DIM == 2)
      {
        give3ex_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / give3ex_1_lws[0];
        give3ex_1_lws[1] = n3[i] < temp ? n3[i] : temp;

        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], give3ex_1_lws[0]);
        give3ex_1_gws[1] = clu_RoundWorkSize((size_t)n3[i], give3ex_1_lws[1]);
      }
      else // GIVE3EX_1_DIM == 1
      {
        int temp = 1;
        give3ex_1_lws[0] = temp == 0 ? 1 : temp;
        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)n3[i], give3ex_1_lws[0]);
      }
    }
    if (axis == 2) {
      if(GIVE3EX_1_DIM == 2)
      {
        give3ex_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / give3ex_1_lws[0];
        give3ex_1_lws[1] = n2[i] < temp ? n2[i] : temp;

        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], give3ex_1_lws[0]);
        give3ex_1_gws[1] = clu_RoundWorkSize((size_t)n2[i], give3ex_1_lws[1]);
      }
      else // GIVE3EX_1_DIM == 1
      {
        int temp = 1;
        give3ex_1_lws[0] = temp == 0 ? 1 : temp;
        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)n2[i], give3ex_1_lws[0]);
      }
    }
  }
  else
  {
    if (axis == 0) {
      if(GIVE3EX_1_DIM == 2)
      {
        give3ex_1_lws[0] = n2[i] < work_item_sizes[0] ? n2[i] : work_item_sizes[0];
        int temp = max_work_group_size / give3ex_1_lws[0];
        give3ex_1_lws[1] = n3[i] < temp ? n3[i] : temp;

        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)n2[i], give3ex_1_lws[0]);
        give3ex_1_gws[1] = clu_RoundWorkSize((size_t)n3[i], give3ex_1_lws[1]);
      }
      else // GIVE3EX_1_DIM == 1
      {
        int temp = 1;
        give3ex_1_lws[0] = temp == 0 ? 1 : temp;
        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)n3[i], give3ex_1_lws[0]);
      }
    }
    if (axis == 1) {
      if(GIVE3EX_1_DIM == 2)
      {
        give3ex_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / give3ex_1_lws[0];
        give3ex_1_lws[1] = (n3[i]*2) < temp ? (n3[i]*2) : temp;

        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], give3ex_1_lws[0]);
        give3ex_1_gws[1] = clu_RoundWorkSize((size_t)(n3[i]*2), give3ex_1_lws[1]);
      }
      else // GIVE3EX_1_DIM == 1
      {
        int temp = 1;
        give3ex_1_lws[0] = temp == 0 ? 1 : temp;
        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)n3[i], give3ex_1_lws[0]);
      }
    }
    if (axis == 2) {
      if(GIVE3EX_1_DIM == 2)
      {
        give3ex_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / give3ex_1_lws[0];
        give3ex_1_lws[1] = (n2[i]*2) < temp ? (n2[i]*2) : temp;

        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], give3ex_1_lws[0]);
        give3ex_1_gws[1] = clu_RoundWorkSize((size_t)(n2[i]*2), give3ex_1_lws[1]);
      }
      else // GIVE3EX_1_DIM == 1
      {
        int temp = 1;
        give3ex_1_lws[0] = temp == 0 ? 1 : temp;
        give3ex_1_gws[0] = clu_RoundWorkSize((size_t)(n2[i]*2), give3ex_1_lws[0]);
      }
    }
  }

  ecode  = clSetKernelArg(k_give3ex_1[i], 0, sizeof(cl_mem), &ou_buf[i]);
  ecode |= clSetKernelArg(k_give3ex_1[i], 1, sizeof(cl_mem), &buff_buf[i]);
  ecode |= clSetKernelArg(k_give3ex_1[i], 2, sizeof(int), &n1[i]);
  ecode |= clSetKernelArg(k_give3ex_1[i], 3, sizeof(int), &n2[i]);
  ecode |= clSetKernelArg(k_give3ex_1[i], 4, sizeof(int), &n3[i]);
  ecode |= clSetKernelArg(k_give3ex_1[i], 5, sizeof(int), &offset[i]);
  clu_CheckError(ecode, "clSetKernelArg() for k_give3ex_1");

  ecode = clEnqueueNDRangeKernel(cmd_queue[i],
      k_give3ex_1[i],
      GIVE3EX_1_DIM, NULL,
      give3ex_1_gws,
      give3ex_1_lws,
      0, NULL, &send_events[i][1+dir][axis]);
  clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_give3ex_1");

  int dst_dev = nbr[i][k][1+dir][axis];
  recv_params[dst_dev][1+dir][axis].src_id = i;
  recv_params[dst_dev][1+dir][axis].src_buf = buff_buf[i];
  recv_params[dst_dev][1+dir][axis].dst_buf = buff_buf[dst_dev];
  recv_params[dst_dev][1+dir][axis].src_offset = buff_id * NM2 * sizeof(double);
  recv_params[dst_dev][1+dir][axis].cb = buff_len*sizeof(double);
  recv_params[dst_dev][1+dir][axis].event = &send_events[i][1+dir][axis];

}


//---------------------------------------------------------------------
// take3_ex copies in border data to expand number of processors
//---------------------------------------------------------------------
static void take3_ex(int axis, int dir, cl_mem *ou_buf, int* n1, int* n2, int* n3, int i, int* offset)
{

  cl_int ecode;

  if (timeron) timer_start(t_copy_buffer);
  ecode = clWaitForEvents(1, &recv_events[i][0][1+dir][axis]);
  if (timeron) timer_stop(t_copy_buffer);

  size_t take3ex_1_lws[3], take3ex_1_gws[3];
  cl_kernel* k_take3ex_1 = NULL;

  if(dir == -1)
  {
    TAKE3EX_1_DIM = COMM1PEX_2_DIM;
    if(axis == 0)
      k_take3ex_1 = k_comm1pex_2_axis0;
    if(axis == 1)
      k_take3ex_1 = k_comm1pex_2_axis1;
    if(axis == 2)
      k_take3ex_1 = k_comm1pex_2_axis2;
  }
  else
  {
    TAKE3EX_1_DIM = COMM1PEX_3_DIM;
    if(axis == 0)
      k_take3ex_1 = k_comm1pex_3_axis0;
    if(axis == 1)
      k_take3ex_1 = k_comm1pex_3_axis1;
    if(axis == 2)
      k_take3ex_1 = k_comm1pex_3_axis2;
  }
  if(dir == -1)
  {
    if (axis == 0) {
      if(TAKE3EX_1_DIM == 2)
      {
        take3ex_1_lws[0] = n2[i] < work_item_sizes[0] ? n2[i] : work_item_sizes[0];
        int temp = max_work_group_size / take3ex_1_lws[0];
        take3ex_1_lws[1] = n3[i] < temp ? n3[i] : temp;

        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)n2[i], take3ex_1_lws[0]);
        take3ex_1_gws[1] = clu_RoundWorkSize((size_t)n3[i], take3ex_1_lws[1]);
      }
      else // TAKE3EX_1_DIM == 1
      {
        int temp = 1;
        take3ex_1_lws[0] = temp == 0 ? 1 : temp;
        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)n3[i], take3ex_1_lws[0]);
      }
    }
    if (axis == 1) {
      if(TAKE3EX_1_DIM == 2)
      {
        take3ex_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / take3ex_1_lws[0];
        take3ex_1_lws[1] = n3[i] < temp ? n3[i] : temp;

        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], take3ex_1_lws[0]);
        take3ex_1_gws[1] = clu_RoundWorkSize((size_t)n3[i], take3ex_1_lws[1]);
      }
      else // TAKE3EX_1_DIM == 1
      {
        int temp = 1;
        take3ex_1_lws[0] = temp == 0 ? 1 : temp;
        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)n3[i], take3ex_1_lws[0]);
      }
    }
    if (axis == 2) {
      if(TAKE3EX_1_DIM == 2)
      {
        take3ex_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / take3ex_1_lws[0];
        take3ex_1_lws[1] = n2[i] < temp ? n2[i] : temp;

        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], take3ex_1_lws[0]);
        take3ex_1_gws[1] = clu_RoundWorkSize((size_t)n2[i], take3ex_1_lws[1]);
      }
      else // TAKE3EX_1_DIM == 1
      {
        int temp = 1;
        take3ex_1_lws[0] = temp == 0 ? 1 : temp;
        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)n2[i], take3ex_1_lws[0]);
      }
    }
  }
  else
  {
    if (axis == 0) {
      if(TAKE3EX_1_DIM == 2)
      {
        take3ex_1_lws[0] = n2[i] < work_item_sizes[0] ? n2[i] : work_item_sizes[0];
        int temp = max_work_group_size / take3ex_1_lws[0];
        take3ex_1_lws[1] = n3[i] < temp ? n3[i] : temp;

        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)n2[i], take3ex_1_lws[0]);
        take3ex_1_gws[1] = clu_RoundWorkSize((size_t)n3[i], take3ex_1_lws[1]);
      }
      else // TAKE3EX_1_DIM == 1
      {
        int temp = 1;
        take3ex_1_lws[0] = temp == 0 ? 1 : temp;
        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)n3[i], take3ex_1_lws[0]);
      }
    }
    if (axis == 1) {
      if(TAKE3EX_1_DIM == 2)
      {
        take3ex_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / take3ex_1_lws[0];
        take3ex_1_lws[1] = (n3[i]*2) < temp ? (n3[i]*2) : temp;

        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], take3ex_1_lws[0]);
        take3ex_1_gws[1] = clu_RoundWorkSize((size_t)(n3[i]*2), take3ex_1_lws[1]);
      }
      else // TAKE3EX_1_DIM == 1
      {
        int temp = 1;
        take3ex_1_lws[0] = temp == 0 ? 1 : temp;
        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)n3[i], take3ex_1_lws[0]);
      }
    }
    if (axis == 2) {
      if(TAKE3EX_1_DIM == 2)
      {
        take3ex_1_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / take3ex_1_lws[0];
        take3ex_1_lws[1] = (n2[i]*2) < temp ? (n2[i]*2) : temp;

        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)n1[i], take3ex_1_lws[0]);
        take3ex_1_gws[1] = clu_RoundWorkSize((size_t)(n2[i]*2), take3ex_1_lws[1]);
      }
      else // TAKE3EX_1_DIM == 1
      {
        int temp = 1;
        take3ex_1_lws[0] = temp == 0 ? 1 : temp;
        take3ex_1_gws[0] = clu_RoundWorkSize((size_t)(n2[i]*2), take3ex_1_lws[0]);
      }
    }
  }

  ecode  = clSetKernelArg(k_take3ex_1[i], 0, sizeof(cl_mem), &ou_buf[i]);
  ecode |= clSetKernelArg(k_take3ex_1[i], 1, sizeof(cl_mem), &buff_buf[i]);
  ecode |= clSetKernelArg(k_take3ex_1[i], 2, sizeof(int), &n1[i]);
  ecode |= clSetKernelArg(k_take3ex_1[i], 3, sizeof(int), &n2[i]);
  ecode |= clSetKernelArg(k_take3ex_1[i], 4, sizeof(int), &n3[i]);
  ecode |= clSetKernelArg(k_take3ex_1[i], 5, sizeof(int), &offset[i]);
  clu_CheckError(ecode, "clSetKernelArg() for k_take3ex_1");

  ecode = clEnqueueNDRangeKernel(cmd_queue[i],
      k_take3ex_1[i],
      TAKE3_1_DIM, NULL,
      take3ex_1_gws,
      take3ex_1_lws,
      0, NULL, NULL);//&send_events[i][1+dir][axis]);
  clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_take3_1");
}


static void comm1p(int axis, cl_mem *ou_buf, int *n1, int *n2, int *n3, int kk, int* offset)
{
  int i;
  int dir, buff_id;
  cl_int ecode;

  if (timeron) timer_start(t_comm1p_1);
  if (timeron) timer_stop(t_comm1p_1);

  if (timeron) timer_start(t_comm1p_2);
  // comm1p_2 
  size_t comm1p_2_lws[2], comm1p_2_gws[2];
  cl_kernel *k_comm1p_2 = NULL;
  cl_kernel *k_comm1p_4 = NULL;

  if(axis == 0)
  {
    k_comm1p_2 = k_comm1p_2_axis0;
    k_comm1p_4 = k_comm1p_4_axis0;
  }
  if(axis == 1)
  {
    k_comm1p_2 = k_comm1p_2_axis1;
    k_comm1p_4 = k_comm1p_4_axis1;
  }
  if(axis == 2)
  {
    k_comm1p_2 = k_comm1p_2_axis2;
    k_comm1p_4 = k_comm1p_4_axis2;
  }

  for (i = 0; i < num_devices; i++) 
  {
    if (axis == 0) {
      if(COMM1P_2_DIM == 2)
      {
        comm1p_2_lws[0] = (n2[i]-2) < work_item_sizes[0] ? (n2[i]-2) : work_item_sizes[0];
        int temp = max_work_group_size / comm1p_2_lws[0];
        comm1p_2_lws[1] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

        comm1p_2_gws[0] = clu_RoundWorkSize((size_t)(n2[i]-2), comm1p_2_lws[0]);
        comm1p_2_gws[1] = clu_RoundWorkSize((size_t)(n3[i]-2), comm1p_2_lws[1]);
      }
      else // COMM1P_2_DIM == 1
      {
        int temp = 1;
        comm1p_2_lws[0] = temp == 0 ? 1 : temp;
        comm1p_2_gws[0] = clu_RoundWorkSize((size_t)(n3[i]-2), comm1p_2_lws[0]);
      }
    }
    if (axis == 1) {
      if(COMM1P_2_DIM == 2)
      {
        comm1p_2_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / comm1p_2_lws[0];
        comm1p_2_lws[1] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

        comm1p_2_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1p_2_lws[0]);
        comm1p_2_gws[1] = clu_RoundWorkSize((size_t)(n3[i]-2), comm1p_2_lws[1]);
      }
      else // COMM1P_2_DIM == 1
      {
        int temp = 1;
        comm1p_2_lws[0] = temp == 0 ? 1 : temp;
        comm1p_2_gws[0] = clu_RoundWorkSize((size_t)(n3[i]-2), comm1p_2_lws[0]);
      }
    }
    if (axis == 2) {
      if(COMM1P_2_DIM == 2)
      {
        comm1p_2_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / comm1p_2_lws[0];
        comm1p_2_lws[1] = n2[i] < temp ? n2[i] : temp;

        comm1p_2_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1p_2_lws[0]);
        comm1p_2_gws[1] = clu_RoundWorkSize((size_t)n2[i], comm1p_2_lws[1]);
      }
      else // COMM1P_2_DIM == 1
      {
        int temp = 1;
        comm1p_2_lws[0] = temp == 0 ? 1 : temp;
        comm1p_2_gws[0] = clu_RoundWorkSize((size_t)n2[i], comm1p_2_lws[0]);
      }
    }

    ecode  = clSetKernelArg(k_comm1p_2[i], 0, sizeof(cl_mem), &ou_buf[i]);
    ecode |= clSetKernelArg(k_comm1p_2[i], 1, sizeof(cl_mem), &buff_buf[i]);
    ecode |= clSetKernelArg(k_comm1p_2[i], 2, sizeof(int), &n1[i]);
    ecode |= clSetKernelArg(k_comm1p_2[i], 3, sizeof(int), &n2[i]);
    ecode |= clSetKernelArg(k_comm1p_2[i], 4, sizeof(int), &n3[i]);
    ecode |= clSetKernelArg(k_comm1p_2[i], 5, sizeof(int), &offset[i]);
    clu_CheckError(ecode, "clSetKernelArg() for k_comm1p_2");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
        k_comm1p_2[i],
        COMM1P_2_DIM, NULL,
        comm1p_2_gws,
        comm1p_2_lws,
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_comm1p_2");
  }
  CHECK_FINISH_DEBUG();
  // comm1p_2 end
  if (timeron) timer_stop(t_comm1p_2);


  if (timeron) timer_start(t_comm1p_3);
  // comm1p_3
  size_t comm1p_3_lws, comm1p_3_gws;
  for (i = 0; i < num_devices; i++) 
  {
    if(COMM1P_3_DIM == 1)
    {
      int temp = 1024;
      comm1p_3_lws = temp == 0 ? 1 : temp;
      comm1p_3_gws = clu_RoundWorkSize((size_t)(NM2), comm1p_3_lws);
    }
    else // COMM1P_3_DIM == 0
    {
      comm1p_3_lws = 1;
      comm1p_3_gws = 1;
    }

    ecode  = clSetKernelArg(k_comm1p_3[i], 0, sizeof(cl_mem), &buff_buf[i]);
    clu_CheckError(ecode, "clSetKernelArg() for k_comm1p_3");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
        k_comm1p_3[i],
        COMM1P_3_DIM, NULL,
        &comm1p_3_gws,
        &comm1p_3_lws,
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_comm1p_3");
    //    ecode = clEnqueueTask(cmd_queue[i],
    //                          k_comm1p_3[i],
    //                          0, NULL, NULL);
    //    clu_CheckError(ecode, "clEnqueueTask()");
  }
  CHECK_FINISH_DEBUG();
  if (timeron) timer_stop(t_comm1p_3);
  // comm1p_3 end

  // comm1p_4
  size_t comm1p_4_lws[3], comm1p_4_gws[3];

  if (timeron) timer_start(t_comm1p_4);

  for (i = 0; i < num_devices; i++) 
  {
    if (axis == 0) {
      if(COMM1P_4_DIM == 2)
      {
        comm1p_4_lws[0] = (n2[i]-2) < work_item_sizes[0] ? (n2[i]-2) : work_item_sizes[0];
        int temp = max_work_group_size / comm1p_4_lws[0];
        comm1p_4_lws[1] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

        comm1p_4_gws[0] = clu_RoundWorkSize((size_t)(n2[i]-2), comm1p_4_lws[0]);
        comm1p_4_gws[1] = clu_RoundWorkSize((size_t)(n3[i]-2), comm1p_4_lws[1]);
      }
      else // COMM1P_4_DIM == 1
      {
        int temp = 1;
        comm1p_4_lws[0] = temp == 0 ? 1 : temp;
        comm1p_4_gws[0] = clu_RoundWorkSize((size_t)(n3[i]-2), comm1p_4_lws[0]);
      }
    }
    if (axis == 1) {
      if(COMM1P_4_DIM == 2)
      {
        comm1p_4_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / comm1p_4_lws[0];
        comm1p_4_lws[1] = (n3[i]-2) < temp ? (n3[i]-2) : temp;

        comm1p_4_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1p_4_lws[0]);
        comm1p_4_gws[1] = clu_RoundWorkSize((size_t)(n3[i]-2), comm1p_4_lws[1]);
      }
      else // COMM1P_4_DIM == 1
      {
        int temp = 1;
        comm1p_4_lws[0] = temp == 0 ? 1 : temp;
        comm1p_4_gws[0] = clu_RoundWorkSize((size_t)(n3[i]-2), comm1p_4_lws[0]);
      }
    }
    if (axis == 2) {
      if(COMM1P_4_DIM == 2)
      {
        comm1p_4_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
        int temp = max_work_group_size / comm1p_4_lws[0];
        comm1p_4_lws[1] = n2[i] < temp ? n2[i] : temp;

        comm1p_4_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1p_4_lws[0]);
        comm1p_4_gws[1] = clu_RoundWorkSize((size_t)n2[i], comm1p_4_lws[1]);
      }
      else // COMM1P_4_DIM == 1
      {
        int temp = 1;
        comm1p_4_lws[0] = temp == 0 ? 1 : temp;
        comm1p_4_gws[0] = clu_RoundWorkSize((size_t)n2[i], comm1p_4_lws[0]);
      }
    }

    ecode  = clSetKernelArg(k_comm1p_4[i], 0, sizeof(cl_mem), &ou_buf[i]);
    ecode |= clSetKernelArg(k_comm1p_4[i], 1, sizeof(cl_mem), &buff_buf[i]);
    ecode |= clSetKernelArg(k_comm1p_4[i], 2, sizeof(int), &n1[i]);
    ecode |= clSetKernelArg(k_comm1p_4[i], 3, sizeof(int), &n2[i]);
    ecode |= clSetKernelArg(k_comm1p_4[i], 4, sizeof(int), &n3[i]);
    ecode |= clSetKernelArg(k_comm1p_4[i], 5, sizeof(int), &offset[i]);
    clu_CheckError(ecode, "clSetKernelArg() for k_comm1p_4");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
        k_comm1p_4[i],
        COMM1P_4_DIM, NULL,
        comm1p_4_gws,
        comm1p_4_lws,
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_comm1p_4");
  }
  CHECK_FINISH_DEBUG();
  if (timeron) timer_stop(t_comm1p_4);
  // comm1p_4 end
}


static void comm1p_ex(int axis, cl_mem *ou_buf, int* n1, int* n2, int* n3, int kk, int* offset)
{
  int i;
  int dir, buff_id;
  cl_int ecode;

  if (timeron) timer_start(t_comm1pex);
  for (i = 0; i < num_devices; i++) 
  {
    if (take_ex[i][kk][axis]) {

      // comm1pex_2
      size_t comm1pex_2_lws[3], comm1pex_2_gws[3];

      cl_kernel *k_comm1pex_2 = NULL;
      cl_kernel *k_comm1pex_3 = NULL;

      if(axis == 0)
      {
        k_comm1pex_2 = k_comm1pex_2_axis0;
        k_comm1pex_3 = k_comm1pex_3_axis0;
      }
      if(axis == 1)
      {
        k_comm1pex_2 = k_comm1pex_2_axis1;
        k_comm1pex_3 = k_comm1pex_3_axis1;
      }
      if(axis == 2)
      {
        k_comm1pex_2 = k_comm1pex_2_axis2;
        k_comm1pex_3 = k_comm1pex_3_axis2;
      }

      if (axis == 0) {
        if(COMM1PEX_2_DIM == 2)
        {
          comm1pex_2_lws[0] = n2[i] < work_item_sizes[0] ? n2[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_2_lws[0];
          comm1pex_2_lws[1] = n3[i] < temp ? n3[i] : temp;

          comm1pex_2_gws[0] = clu_RoundWorkSize((size_t)n2[i], comm1pex_2_lws[0]);
          comm1pex_2_gws[1] = clu_RoundWorkSize((size_t)n3[i], comm1pex_2_lws[1]);
        }
        else // COMM1PEX_2_DIM == 1
        {
          int temp = 1;
          comm1pex_2_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_2_gws[0] = clu_RoundWorkSize((size_t)n3[i], comm1pex_2_lws[0]);
        }
      }
      if (axis == 1) {
        if(COMM1PEX_2_DIM == 2)
        {
          comm1pex_2_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_2_lws[0];
          comm1pex_2_lws[1] = n3[i] < temp ? n3[i] : temp;

          comm1pex_2_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1pex_2_lws[0]);
          comm1pex_2_gws[1] = clu_RoundWorkSize((size_t)n3[i], comm1pex_2_lws[1]);
        }
        else // COMM1PEX_2_DIM == 1
        {
          int temp = 1;
          comm1pex_2_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_2_gws[0] = clu_RoundWorkSize((size_t)n3[i], comm1pex_2_lws[0]);
        }
      }
      if (axis == 2) {
        if(COMM1PEX_2_DIM == 2)
        {
          comm1pex_2_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_2_lws[0];
          comm1pex_2_lws[1] = n2[i] < temp ? n2[i] : temp;

          comm1pex_2_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1pex_2_lws[0]);
          comm1pex_2_gws[1] = clu_RoundWorkSize((size_t)n2[i], comm1pex_2_lws[1]);
        }
        else // COMM1PEX_2_DIM == 1
        {
          int temp = 1;
          comm1pex_2_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_2_gws[0] = clu_RoundWorkSize((size_t)n2[i], comm1pex_2_lws[0]);
        }
      }

      ecode  = clSetKernelArg(k_comm1pex_2[i], 0, sizeof(cl_mem), &ou_buf[i]);
      ecode |= clSetKernelArg(k_comm1pex_2[i], 1, sizeof(cl_mem), &buff_buf[i]);
      ecode |= clSetKernelArg(k_comm1pex_2[i], 2, sizeof(int), &n1[i]);
      ecode |= clSetKernelArg(k_comm1pex_2[i], 3, sizeof(int), &n2[i]);
      ecode |= clSetKernelArg(k_comm1pex_2[i], 4, sizeof(int), &n3[i]);
      ecode |= clSetKernelArg(k_comm1pex_2[i], 5, sizeof(int), &offset[i]);
      clu_CheckError(ecode, "clSetKernelArg() for k_comm1pex_2");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
          k_comm1pex_2[i],
          COMM1PEX_2_DIM, NULL,
          comm1pex_2_gws,
          comm1pex_2_lws,
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_comm1pex_2");
      // comm1pex_2 end

      // comm1pex_3
      size_t comm1pex_3_lws[3], comm1pex_3_gws[3];

      if (axis == 0) {
        if(COMM1PEX_3_DIM == 2)
        {
          comm1pex_3_lws[0] = n2[i] < work_item_sizes[0] ? n2[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_3_lws[0];
          comm1pex_3_lws[1] = n3[i] < temp ? n3[i] : temp;

          comm1pex_3_gws[0] = clu_RoundWorkSize((size_t)n2[i], comm1pex_3_lws[0]);
          comm1pex_3_gws[1] = clu_RoundWorkSize((size_t)n3[i], comm1pex_3_lws[1]);
        }
        else // COMM1PEX_3_DIM == 1
        {
          int temp = 1;
          comm1pex_3_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_3_gws[0] = clu_RoundWorkSize((size_t)n3[i], comm1pex_3_lws[0]);
        }
      }
      if (axis == 1) {
        if(COMM1PEX_3_DIM == 2)
        {
          comm1pex_3_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_3_lws[0];
          comm1pex_3_lws[1] = (n3[i]*2) < temp ? (n3[i]*2) : temp;

          comm1pex_3_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1pex_3_lws[0]);
          comm1pex_3_gws[1] = clu_RoundWorkSize((size_t)(n3[i]*2), comm1pex_3_lws[1]);
        }
        else // COMM1PEX_3_DIM == 1
        {
          int temp = 1;
          comm1pex_3_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_3_gws[0] = clu_RoundWorkSize((size_t)n3[i], comm1pex_3_lws[0]);
        }
      }
      if (axis == 2) {
        if(COMM1PEX_3_DIM == 2)
        {
          comm1pex_3_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_3_lws[0];
          comm1pex_3_lws[1] = (n2[i]*2) < temp ? (n2[i]*2) : temp;

          comm1pex_3_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1pex_3_lws[0]);
          comm1pex_3_gws[1] = clu_RoundWorkSize((size_t)(n2[i]*2), comm1pex_3_lws[1]);
        }
        else // COMM1PEX_3_DIM == 1
        {
          int temp = 1;
          comm1pex_3_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_3_gws[0] = clu_RoundWorkSize((size_t)(n2[i]*2), comm1pex_3_lws[0]);
        }
      }

      ecode  = clSetKernelArg(k_comm1pex_3[i], 0, sizeof(cl_mem), &ou_buf[i]);
      ecode |= clSetKernelArg(k_comm1pex_3[i], 1, sizeof(cl_mem), &buff_buf[i]);
      ecode |= clSetKernelArg(k_comm1pex_3[i], 2, sizeof(int), &n1[i]);
      ecode |= clSetKernelArg(k_comm1pex_3[i], 3, sizeof(int), &n2[i]);
      ecode |= clSetKernelArg(k_comm1pex_3[i], 4, sizeof(int), &n3[i]);
      ecode |= clSetKernelArg(k_comm1pex_3[i], 5, sizeof(int), &offset[i]);
      clu_CheckError(ecode, "clSetKernelArg() for k_comm1pex_3");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
          k_comm1pex_3[i],
          COMM1PEX_3_DIM, NULL,
          comm1pex_3_gws,
          comm1pex_3_lws,
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_comm1pex_3");
    }
  }

  for (i = 0; i < num_devices; i++) 
  {
    if (give_ex[i][kk][axis]) {
      // comm1pex_4
      size_t comm1pex_4_lws[3], comm1pex_4_gws[3];

      cl_kernel *k_comm1pex_4 = NULL;
      cl_kernel *k_comm1pex_5 = NULL;

      if(axis == 0)
      {
        k_comm1pex_4 = k_comm1pex_4_axis0;
        k_comm1pex_5 = k_comm1pex_5_axis0;
      }
      if(axis == 1)
      {
        k_comm1pex_4 = k_comm1pex_4_axis1;
        k_comm1pex_5 = k_comm1pex_5_axis1;
      }
      if(axis == 2)
      {
        k_comm1pex_4 = k_comm1pex_4_axis2;
        k_comm1pex_5 = k_comm1pex_5_axis2;
      }

      if (axis == 0) {
        if(COMM1PEX_4_DIM == 2)
        {
          comm1pex_4_lws[0] = n2[i] < work_item_sizes[0] ? n2[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_4_lws[0];
          comm1pex_4_lws[1] = n3[i] < temp ? n3[i] : temp;

          comm1pex_4_gws[0] = clu_RoundWorkSize((size_t)n2[i], comm1pex_4_lws[0]);
          comm1pex_4_gws[1] = clu_RoundWorkSize((size_t)n3[i], comm1pex_4_lws[1]);
        }
        else // COMM1PEX_4_DIM == 1
        {
          int temp = 1;
          comm1pex_4_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_4_gws[0] = clu_RoundWorkSize((size_t)n3[i], comm1pex_4_lws[0]);
        }
      }
      if (axis == 1) {
        if(COMM1PEX_4_DIM == 2)
        {
          comm1pex_4_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_4_lws[0];
          comm1pex_4_lws[1] = (n3[i]*2) < temp ? (n3[i]*2) : temp;

          comm1pex_4_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1pex_4_lws[0]);
          comm1pex_4_gws[1] = clu_RoundWorkSize((size_t)(n3[i]*2), comm1pex_4_lws[1]);
        }
        else // COMM1PEX_4_DIM == 1
        {
          int temp = 1;
          comm1pex_4_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_4_gws[0] = clu_RoundWorkSize((size_t)n3[i], comm1pex_4_lws[0]);
        }
      }
      if (axis == 2) {
        if(COMM1PEX_4_DIM == 2)
        {
          comm1pex_4_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_4_lws[0];
          comm1pex_4_lws[1] = (n2[i]*2) < temp ? (n2[i]*2) : temp;

          comm1pex_4_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1pex_4_lws[0]);
          comm1pex_4_gws[1] = clu_RoundWorkSize((size_t)(n2[i]*2), comm1pex_4_lws[1]);
        }
        else // COMM1PEX_4_DIM == 1
        {
          int temp = 1;
          comm1pex_4_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_4_gws[0] = clu_RoundWorkSize((size_t)(n2[i]*2), comm1pex_4_lws[0]);
        }
      }

      ecode  = clSetKernelArg(k_comm1pex_4[i], 0, sizeof(cl_mem), &ou_buf[i]);
      ecode |= clSetKernelArg(k_comm1pex_4[i], 1, sizeof(cl_mem), &buff_buf[i]);
      ecode |= clSetKernelArg(k_comm1pex_4[i], 2, sizeof(int), &n1[i]);
      ecode |= clSetKernelArg(k_comm1pex_4[i], 3, sizeof(int), &n2[i]);
      ecode |= clSetKernelArg(k_comm1pex_4[i], 4, sizeof(int), &n3[i]);
      ecode |= clSetKernelArg(k_comm1pex_4[i], 4, sizeof(int), &offset[i]);
      clu_CheckError(ecode, "clSetKernelArg() for k_comm1pex_4");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
          k_comm1pex_4[i],
          COMM1PEX_4_DIM, NULL,
          comm1pex_4_gws,
          comm1pex_4_lws,
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_comm1pex_4");


      // comm1pex_5
      size_t comm1pex_5_lws[3], comm1pex_5_gws[3];

      if (axis == 0) {
        if(COMM1PEX_5_DIM == 2)
        {
          comm1pex_5_lws[0] = n2[i] < work_item_sizes[0] ? n2[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_5_lws[0];
          comm1pex_5_lws[1] = n3[i] < temp ? n3[i] : temp;

          comm1pex_5_gws[0] = clu_RoundWorkSize((size_t)n2[i], comm1pex_5_lws[0]);
          comm1pex_5_gws[1] = clu_RoundWorkSize((size_t)n3[i], comm1pex_5_lws[1]);
        }
        else // COMM1PEX_5_DIM == 1
        {
          int temp = 1;
          comm1pex_5_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_5_gws[0] = clu_RoundWorkSize((size_t)n3[i], comm1pex_5_lws[0]);
        }
      }
      if (axis == 1) {
        if(COMM1PEX_5_DIM == 2)
        {
          comm1pex_5_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_5_lws[0];
          comm1pex_5_lws[1] = n3[i] < temp ? n3[i] : temp;

          comm1pex_5_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1pex_5_lws[0]);
          comm1pex_5_gws[1] = clu_RoundWorkSize((size_t)n3[i], comm1pex_5_lws[1]);
        }
        else // COMM1PEX_5_DIM == 1
        {
          int temp = 1;
          comm1pex_5_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_5_gws[0] = clu_RoundWorkSize((size_t)n3[i], comm1pex_5_lws[0]);
        }
      }
      if (axis == 2) {
        if(COMM1PEX_5_DIM == 2)
        {
          comm1pex_5_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
          int temp = max_work_group_size / comm1pex_5_lws[0];
          comm1pex_5_lws[1] = n2[i] < temp ? n2[i] : temp;

          comm1pex_5_gws[0] = clu_RoundWorkSize((size_t)n1[i], comm1pex_5_lws[0]);
          comm1pex_5_gws[1] = clu_RoundWorkSize((size_t)n2[i], comm1pex_5_lws[1]);
        }
        else // COMM1PEX_5_DIM == 1
        {
          int temp = 1;
          comm1pex_5_lws[0] = temp == 0 ? 1 : temp;
          comm1pex_5_gws[0] = clu_RoundWorkSize((size_t)n2[i], comm1pex_5_lws[0]);
        }
      }

      ecode  = clSetKernelArg(k_comm1pex_5[i], 0, sizeof(cl_mem), &ou_buf[i]);
      ecode |= clSetKernelArg(k_comm1pex_5[i], 1, sizeof(cl_mem), &buff_buf[i]);
      ecode |= clSetKernelArg(k_comm1pex_5[i], 2, sizeof(int), &n1[i]);
      ecode |= clSetKernelArg(k_comm1pex_5[i], 3, sizeof(int), &n2[i]);
      ecode |= clSetKernelArg(k_comm1pex_5[i], 4, sizeof(int), &n3[i]);
      ecode |= clSetKernelArg(k_comm1pex_5[i], 4, sizeof(int), &offset[i]);
      clu_CheckError(ecode, "clSetKernelArg() for k_comm1pex_5");

      ecode = clEnqueueNDRangeKernel(cmd_queue[i],
          k_comm1pex_5[i],
          COMM1PEX_5_DIM, NULL,
          comm1pex_5_gws,
          comm1pex_5_lws,
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_comm1pex_5");
    }
    // comm1pex_5 end
  }

  // comm1p_3
  size_t comm1p_3_lws, comm1p_3_gws;
  //  }
  //	CHECK_FINISH_DEBUG();
  for (i = 0; i < num_devices; i++) 
{
  if(COMM1P_3_DIM == 1)
  {
    int temp = 1024;
    comm1p_3_lws = temp == 0 ? 1 : temp;
    comm1p_3_gws = clu_RoundWorkSize((size_t)(NM2), comm1p_3_lws);
  }
  else // COMM1P_3_DIM == 0
  {
    comm1p_3_lws = 1;
    comm1p_3_gws = 1;
  }

  ecode  = clSetKernelArg(k_comm1p_3[i], 0, sizeof(cl_mem), &buff_buf[i]);
  clu_CheckError(ecode, "clSetKernelArg() for k_comm1p_3");

  ecode = clEnqueueNDRangeKernel(cmd_queue[i],
      k_comm1p_3[i],
      COMM1P_3_DIM, NULL,
      &comm1p_3_gws,
      &comm1p_3_lws,
      0, NULL, NULL);
  clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_comm1p_3");
  //    ecode = clEnqueueTask(cmd_queue[i],
  //                          k_comm1p_3[i],
  //                          0, NULL, NULL);
  //    clu_CheckError(ecode, "clEnqueueTask()");
}
CHECK_FINISH_DEBUG();
// comm1p_3 end
if (timeron) timer_stop(t_comm1pex);
}


//---------------------------------------------------------------------
// zran3  loads +1 at ten randomly chosen points,
// loads -1 at a different ten random points,
// and zero elsewhere.
//---------------------------------------------------------------------
static void zran3(cl_mem *oz_buf, int* n1, int* n2, int* n3, int nx, int ny, int k)
{
  cl_int ecode;

  int *i0, *i1, *m0, *m1;

  //	int *d1, *e1, *e2, *e3;
  int *d1, *e2, *e3;
  double a1, a2;
  double *ai;

  const int mm = 10;
  const double a = pow(5.0, 13.0);
  double temp=0;
  double best; 
  double* best_dev;
  int i,j; 
  int (*j1)[mm][2], (*j2)[mm][2], (*j3)[mm][2];
  //  int jg[2][mm][4], jg_temp[4];
  if (timeron) timer_start(t_zran3);

  ai = (double*)malloc(sizeof(double)*num_devices);
  d1 = (int*)malloc(sizeof(int)*num_devices);
  //	e1 = (int*)malloc(sizeof(int)*num_devices);
  e2 = (int*)malloc(sizeof(int)*num_devices);
  e3 = (int*)malloc(sizeof(int)*num_devices);
  j1 =(int (*)[mm][2])malloc(sizeof(int)*mm*2*num_devices);
  j2 =(int (*)[mm][2])malloc(sizeof(int)*mm*2*num_devices);
  j3 =(int (*)[mm][2])malloc(sizeof(int)*mm*2*num_devices);
  best_dev = (double*)malloc(sizeof(double)*num_devices);
  i0 = (int*)malloc(sizeof(int)*num_devices);
  i1 = (int*)malloc(sizeof(int)*num_devices);
  m0 = (int*)malloc(sizeof(int)*num_devices);
  m1 = (int*)malloc(sizeof(int)*num_devices);

  a1 = power( a, nx, 1, 0 );
  a2 = power( a, nx, ny, 0 );

  zero3(oz_buf, n1, n2, n3, null_offset);

  power_task( power_task_out_buf, a, nx, is1, is2, is3, ny, ai);

  for (i = 0; i < num_devices; i++) 
  {
    d1[i] = ie1[i] - is1[i] + 1;
    //		e1[i] = ie1[i] - is1[i] + 2;
    e2[i] = ie2[i] - is2[i] + 2;
    e3[i] = ie3[i] - is3[i] + 2;

    ecode  = clSetKernelArg(k_zran3_task1[i], 0, sizeof(cl_mem), &starts_buf[i]);
    ecode |= clSetKernelArg(k_zran3_task1[i], 1, sizeof(double), &ai[i]);
    ecode |= clSetKernelArg(k_zran3_task1[i], 2, sizeof(int), &e3[i]);
    ecode |= clSetKernelArg(k_zran3_task1[i], 3, sizeof(double), &a2);
    clu_CheckError(ecode, "clSetKernelArg() for k_zran3_task1");

    ecode = clEnqueueTask(cmd_queue[i],
        k_zran3_task1[i],
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueTask()");
  }

  // zran3_1 
  size_t zran3_1_lws, zran3_1_gws;
  for (i = 0; i < num_devices; i++) 
  {
    int temp = 1;
    zran3_1_lws = temp == 0 ? 1 : temp;
    zran3_1_gws = clu_RoundWorkSize((size_t)(e3[i]-1), zran3_1_lws);

    ecode  = clSetKernelArg(k_zran3_1[i], 0, sizeof(cl_mem), &oz_buf[i]);
    ecode |= clSetKernelArg(k_zran3_1[i], 1, sizeof(cl_mem), &starts_buf[i]);
    ecode |= clSetKernelArg(k_zran3_1[i], 2, sizeof(int), &d1[i]);
    ecode |= clSetKernelArg(k_zran3_1[i], 3, sizeof(int), &e2[i]);
    ecode |= clSetKernelArg(k_zran3_1[i], 4, sizeof(int), &e3[i]);
    ecode |= clSetKernelArg(k_zran3_1[i], 5, sizeof(double), &a);
    ecode |= clSetKernelArg(k_zran3_1[i], 6, sizeof(double), &a1);
    ecode |= clSetKernelArg(k_zran3_1[i], 7, sizeof(int), &n1[i]);
    ecode |= clSetKernelArg(k_zran3_1[i], 8, sizeof(int), &n2[i]);
    ecode |= clSetKernelArg(k_zran3_1[i], 9, sizeof(int), &n3[i]);
    clu_CheckError(ecode, "clSetKernelArg() for k_zran3_1");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
        k_zran3_1[i],
        1, NULL,
        &zran3_1_gws,
        &zran3_1_lws,
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for k_zran3_1");
  }
  CHECK_FINISH_DEBUG();

  //origianl code here
  // zran3_1 

  //---------------------------------------------------------------------
  // comm3(z,n1,n2,n3)
  // showall(z,n1,n2,n3)
  //---------------------------------------------------------------------

  //---------------------------------------------------------------------
  // each processor looks for twenty candidates
  //---------------------------------------------------------------------

  //zran3_task2


  for (i = 0; i < num_devices; i++) 
  {
    ecode  = clSetKernelArg(k_zran3_task2[i], 0, sizeof(cl_mem), &j1_buf[i]);
    ecode |= clSetKernelArg(k_zran3_task2[i], 1, sizeof(cl_mem), &j2_buf[i]);
    ecode |= clSetKernelArg(k_zran3_task2[i], 2, sizeof(cl_mem), &j3_buf[i]);
    ecode |= clSetKernelArg(k_zran3_task2[i], 3, sizeof(cl_mem), &oz_buf[i]);
    ecode |= clSetKernelArg(k_zran3_task2[i], 4, sizeof(int), &n1[i]);
    ecode |= clSetKernelArg(k_zran3_task2[i], 5, sizeof(int), &n2[i]);
    ecode |= clSetKernelArg(k_zran3_task2[i], 6, sizeof(int), &n3[i]);
    clu_CheckError(ecode, "clSetKernelArg() for k_zran3_task2");

    ecode = clEnqueueTask(cmd_queue[i],
        k_zran3_task2[i],
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueTask()");
  }

  for (i = 0; i < num_devices; i++) 
  {
    ecode = clEnqueueReadBuffer(cmd_queue[i],
        j1_buf[i],
        CL_FALSE,
        0, sizeof(int) * mm * 2,
        (void *)j1[i],
        0, NULL, NULL);
    clu_CheckError(ecode, "clReadBuffer()");
    ecode = clEnqueueReadBuffer(cmd_queue[i],
        j2_buf[i],
        CL_FALSE,
        0, sizeof(int) * mm * 2,
        (void *)j2[i],
        0, NULL, NULL);
    clu_CheckError(ecode, "clReadBuffer()");
    ecode = clEnqueueReadBuffer(cmd_queue[i],
        j3_buf[i],
        CL_FALSE,
        0, sizeof(int) * mm * 2,
        (void *)j3[i],
        0, NULL, NULL);
    clu_CheckError(ecode, "clReadBuffer()");
  }
  CHECK_FINISH();

  //---------------------------------------------------------------------
  // Now which of these are globally best?
  //---------------------------------------------------------------------

  size_t z_offset;
  for (j = 0; j < num_devices; j++) 
  {
    i1[j] = mm - 1;
    i0[j] = mm - 1;
  }
  for (i = mm-1; i >= 0; i--) 
  {
    for (j = 0; j < num_devices; j++) 
    {
      z_offset = ((j3[j][i1[j]][1] * n2[j] * n1[j]) + (j2[j][i1[j]][1] * n1[j]) + (j1[j][i1[j]][1]))*sizeof(double);

      ecode = clEnqueueReadBuffer(cmd_queue[j],
          oz_buf[j],
          CL_FALSE,
          z_offset, sizeof(double),
          (void *)&best_dev[j],
          0, NULL, NULL);
      clu_CheckError(ecode, "clReadBuffer()");
    }
    CHECK_FINISH();

    for (j = 0; j < num_devices; j++) 
    {
      if( j == 0 )
        temp = best_dev[j];
      else if( temp < best_dev[j] )
        temp = best_dev[j];
    }

    best = temp;
    for (j = 0; j < num_devices; j++) 
    {
      if (best == best_dev[j]){
        i1[j] = i1[j]-1;
      } 
    }

    for (j = 0; j < num_devices; j++) 
    {
      z_offset = ((j3[j][i0[j]][0] * n2[j] * n1[j]) + (j2[j][i0[j]][0] * n1[j]) + (j1[j][i0[j]][0]))*sizeof(double);
      ecode = clEnqueueReadBuffer(cmd_queue[j],
          oz_buf[j],
          CL_FALSE,
          z_offset, sizeof(double),
          (void *)&best_dev[j],
          0, NULL, NULL);
      clu_CheckError(ecode, "clReadBuffer()");
    }
    CHECK_FINISH();

    for (j = 0; j < num_devices; j++) 
    {
      if( j == 0 )
        temp = best_dev[j];
      else if( temp > best_dev[j] )
        temp = best_dev[j];
    }
    best = temp;
    for (j = 0; j < num_devices; j++) 
    {
      if (best == best_dev[j] ) {
        i0[j] = i0[j]-1;
      }
    }
  }
  for (j = 0; j < num_devices; j++) 
  {
    m1[j] = i1[j]+1;
    m0[j] = i0[j]+1;
  }

  //      if (me == root) {
  //         write(*,*)' '
  //         write(*,*)' negative charges at'
  //         write(*,9)(jg(1,i,0),jg(2,i,0),jg(3,i,0),i=1,mm)
  //         write(*,*)' positive charges at'
  //         write(*,9)(jg(1,i,1),jg(2,i,1),jg(3,i,1),i=1,mm)
  //         write(*,*)' small random numbers were'
  //         write(*,8)(ten( i,0),i=mm,1,-1)
  //         write(*,*)' and they were found on processor number'
  //         write(*,7)(jg(0,i,0),i=mm,1,-1)
  //         write(*,*)' large random numbers were'
  //         write(*,8)(ten( i,1),i=mm,1,-1)
  //         write(*,*)' and they were found on processor number'
  //         write(*,7)(jg(0,i,1),i=mm,1,-1)
  //      }
  // 9    format(5(' (',i3,2(',',i3),')'))
  // 8    format(5D15.8)
  // 7    format(10i4)
  zero3(oz_buf,n1,n2,n3,null_offset);

  for (i = 0; i < num_devices; i++) 
  {
    ecode  = clSetKernelArg(k_zran3_task3[i], 0, sizeof(cl_mem), &j1_buf[i]);
    ecode |= clSetKernelArg(k_zran3_task3[i], 1, sizeof(cl_mem), &j2_buf[i]);
    ecode |= clSetKernelArg(k_zran3_task3[i], 2, sizeof(cl_mem), &j3_buf[i]);
    ecode |= clSetKernelArg(k_zran3_task3[i], 3, sizeof(cl_mem), &oz_buf[i]);
    ecode |= clSetKernelArg(k_zran3_task3[i], 4, sizeof(int), &m0[i]);
    ecode |= clSetKernelArg(k_zran3_task3[i], 5, sizeof(int), &m1[i]);
    ecode |= clSetKernelArg(k_zran3_task3[i], 6, sizeof(int), &n1[i]);
    ecode |= clSetKernelArg(k_zran3_task3[i], 7, sizeof(int), &n2[i]);
    ecode |= clSetKernelArg(k_zran3_task3[i], 8, sizeof(int), &n3[i]);
    clu_CheckError(ecode, "clSetKernelArg() for k_zran3_task3");

    ecode = clEnqueueTask(cmd_queue[i],
        k_zran3_task3[i],
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueTask()");
  }
  CHECK_FINISH_DEBUG();
  if (timeron) timer_stop(t_zran3);
  //zran3_task3

  comm3(oz_buf, n1, n2, n3, k, null_offset);

  //---------------------------------------------------------------------
  // showall(z,n1,n2,n3);
  //---------------------------------------------------------------------


  free(ai);
  free(d1);
  //	free(e1);
  free(e2);
  free(e3);
  free(j1);
  free(j2);
  free(j3);
  free(best_dev);
  free(i0);
  free(i1);
  free(m0);
  free(m1);

}

static void showall(cl_mem *oz_buf, int* n1, int* n2, int* n3, int* offset)
{
  /*
     double (*z)[n2][n1] = (double (*)[n2][n1])oz;

     int i1, i2, i3, i;
     int m1, m2, m3;

     m1 = min(n1, 18);
     m2 = min(n2, 14);
     m3 = min(n3, 18);

     printf("   \n");
     for (i = 0; i < nprocs; i++) {
     if (me == i) {
     printf(" id = %d\n", me);
     for (i3 = 0; i3 < m3; i3++) {
     for (i1 = 0; i1 < m1; i1++) {
     for (i2 = 0; i2 < m2; i2++) {
     printf("%6.3f", z[i3][i2][i1]);
     }
     printf("\n");
     }
     printf("  - - - - - - - \n");
     }
     printf("   \n");
     }
     MPI_Barrier(MPI_COMM_WORLD);
     }
   */
}


#if 0
static void show(void *oz, int n1, int n2, int n3)
{
  double (*z)[n2][n1] = (double (*)[n2][n1])oz;

  int i1, i2, i3, i;

  printf("   \n");
  for (i = 0; i < nprocs; i++) {
    if (me == i) {
      printf(" id = %d\n", me);
      for (i3 = 1; i3 < n3-1; i3++) {
        for (i1 = 1; i1 < n1-1; i1++) {
          for (i2 = 1; i2 < n1-1; i2++) {
            printf("%10.3f", z[i3][i2][i1]);
          }
          printf("\n");
        }
        printf("  - - - - - - - \n");
      }
      printf("   \n");
    }
    MPI_Barrier(MPI_COMM_WORLD);
  }

  // comm3(z,n1,n2,n3);
}
#endif


//---------------------------------------------------------------------
// power  raises an integer, disguised as a double
// precision real, to an integer power.
// This version tries to avoid integer overflow by treating
// it as expressed in a form of "n1*n2+n3".
//---------------------------------------------------------------------
static double power(double a, int n1, int n2, int n3)
{
  double aj;
  int n1j, n2j, nj;
  double rdummy;
  double power;

  power = 1.0;
  aj = a;
  nj = n3;
  n1j = n1;
  n2j = n2;

  while (true) {
    if (n2j > 0) {
      if ((n2j % 2) == 1) nj = nj + n1j;
      n2j = n2j/2;
    } else if (nj == 0) break;
    if ((nj % 2) == 1) rdummy = randlc(&power, aj);
    rdummy = randlc(&aj, aj);
    nj = nj/2;
  }

  return power;
}
//---------------------------------------------------------------------
// Since multiple devices pass different arguments to the power function
// power function should be run multiple times for the devices.
// To make it parallel, we use the OpenCL task here.
//---------------------------------------------------------------------
static void power_task(cl_mem* out_buf, double a, int n1, int *is1, int *is2, int *is3, int ny, double* out_val)
{
  int i;
  cl_int ecode;

  if (timeron) timer_start(t_power_task);
  for (i = 0; i < num_devices; i++) {
    ecode  = clSetKernelArg(k_power_task[i], 0, sizeof(cl_mem), &out_buf[i]);
    ecode |= clSetKernelArg(k_power_task[i], 1, sizeof(double), &a);
    ecode |= clSetKernelArg(k_power_task[i], 2, sizeof(int), &n1);
    ecode |= clSetKernelArg(k_power_task[i], 3, sizeof(int), &is1[i]);
    ecode |= clSetKernelArg(k_power_task[i], 4, sizeof(int), &is2[i]);
    ecode |= clSetKernelArg(k_power_task[i], 5, sizeof(int), &is3[i]);
    ecode |= clSetKernelArg(k_power_task[i], 6, sizeof(int), &ny);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueTask(cmd_queue[i],
        k_power_task[i],
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueTask()");
  }

  for (i = 0; i < num_devices; i++) {
    ecode = clEnqueueReadBuffer(cmd_queue[i],
        out_buf[i],
        CL_FALSE,
        0, sizeof(double),
        (void *)&out_val[i],
        0, NULL, NULL);
    clu_CheckError(ecode, "clReadBuffer()");
  }
  CHECK_FINISH();
  if (timeron) timer_stop(t_power_task);
}


static void zero3(cl_mem *oz_buf, int* n1, int* n2, int* n3, int* offset)
{
  int i;
  cl_int ecode;
  size_t zero3_lws[3], zero3_gws[3];

  if (timeron) timer_start(t_zero3);
  for (i = 0; i < num_devices; i++) 
  {
    if(ZERO3_DIM == 3)
    {
      zero3_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
      int temp = max_work_group_size / zero3_lws[0];
      zero3_lws[1] = n2[i] < temp ? n2[i] : temp;
      temp = temp / zero3_lws[1];
      zero3_lws[2] = n3[i] < temp ? n3[i] : temp;

      zero3_gws[0] = clu_RoundWorkSize((size_t)n1[i], zero3_lws[0]);
      zero3_gws[1] = clu_RoundWorkSize((size_t)n2[i], zero3_lws[1]);
      zero3_gws[2] = clu_RoundWorkSize((size_t)n3[i], zero3_lws[2]);
    }
    else if(ZERO3_DIM == 2)
    {
      zero3_lws[0] = n2[i] < work_item_sizes[0] ? n2[i] : work_item_sizes[0];
      int temp = max_work_group_size / zero3_lws[0];
      zero3_lws[1] = n3[i] < temp ? n3[i] : temp;

      zero3_gws[0] = clu_RoundWorkSize((size_t)n2[i], zero3_lws[0]);
      zero3_gws[1] = clu_RoundWorkSize((size_t)n3[i], zero3_lws[1]);
    }
    else // ZERO3_DIM == 1
    {
      int temp = 1;
      zero3_lws[0] = temp == 0 ? 1 : temp;
      zero3_gws[0] = clu_RoundWorkSize((size_t)n3[i], zero3_lws[0]);
    }

    ecode  = clSetKernelArg(k_zero3[i], 0, sizeof(cl_mem), &oz_buf[i]);
    ecode |= clSetKernelArg(k_zero3[i], 1, sizeof(int), &n1[i]);
    ecode |= clSetKernelArg(k_zero3[i], 2, sizeof(int), &n2[i]);
    ecode |= clSetKernelArg(k_zero3[i], 3, sizeof(int), &n3[i]);
    ecode |= clSetKernelArg(k_zero3[i], 4, sizeof(int), &offset[i]);
    clu_CheckError(ecode, "clSetKernelArg() for zero3");

    ecode = clEnqueueNDRangeKernel(cmd_queue[i],
        k_zero3[i],
        ZERO3_DIM, NULL,
        zero3_gws,
        zero3_lws,
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for zero3");
  }
  if (timeron) timer_stop(t_zero3);

  CHECK_FINISH();
}
static void zero3_for_one_dev(cl_mem *oz_buf, int* n1, int* n2, int* n3, int i, int* offset)
{
  cl_int ecode;
  size_t zero3_lws[3], zero3_gws[3];

  if (timeron) timer_start(t_zero3);
  if(ZERO3_DIM == 3)
  {
    zero3_lws[0] = n1[i] < work_item_sizes[0] ? n1[i] : work_item_sizes[0];
    int temp = max_work_group_size / zero3_lws[0];
    zero3_lws[1] = n2[i] < temp ? n2[i] : temp;
    temp = temp / zero3_lws[1];
    zero3_lws[2] = n3[i] < temp ? n3[i] : temp;

    zero3_gws[0] = clu_RoundWorkSize((size_t)n1[i], zero3_lws[0]);
    zero3_gws[1] = clu_RoundWorkSize((size_t)n2[i], zero3_lws[1]);
    zero3_gws[2] = clu_RoundWorkSize((size_t)n3[i], zero3_lws[2]);
  }
  else if(ZERO3_DIM == 2)
  {
    zero3_lws[0] = n2[i] < work_item_sizes[0] ? n2[i] : work_item_sizes[0];
    int temp = max_work_group_size / zero3_lws[0];
    zero3_lws[1] = n3[i] < temp ? n3[i] : temp;

    zero3_gws[0] = clu_RoundWorkSize((size_t)n2[i], zero3_lws[0]);
    zero3_gws[1] = clu_RoundWorkSize((size_t)n3[i], zero3_lws[1]);
  }
  else // ZERO3_DIM == 1
  {
    int temp = 1;
    zero3_lws[0] = temp == 0 ? 1 : temp;
    zero3_gws[0] = clu_RoundWorkSize((size_t)n3[i], zero3_lws[0]);
  }

  ecode  = clSetKernelArg(k_zero3[i], 0, sizeof(cl_mem), &oz_buf[i]);
  ecode |= clSetKernelArg(k_zero3[i], 1, sizeof(int), &n1[i]);
  ecode |= clSetKernelArg(k_zero3[i], 2, sizeof(int), &n2[i]);
  ecode |= clSetKernelArg(k_zero3[i], 3, sizeof(int), &n3[i]);
  ecode |= clSetKernelArg(k_zero3[i], 4, sizeof(int), &offset[i]);
  clu_CheckError(ecode, "clSetKernelArg() for evolve");

  ecode = clEnqueueNDRangeKernel(cmd_queue[i],
      k_zero3[i],
      ZERO3_DIM, NULL,
      zero3_gws,
      zero3_lws,
      0, NULL, NULL);
  clu_CheckError(ecode, "clEnqueueNDRangeKernel() for zero3");

  if (timeron) timer_stop(t_zero3);
}

//---------------------------------------------------------------------
// Set up the OpenCL environment.
//---------------------------------------------------------------------
static void setup_opencl(int argc, char *argv[])
{
  int i;
  int real_num_devices;
  //  size_t temp;
  cl_int ecode;
  char *source_dir = ".";  //FIXME
  num_devices = DEFAULT_NUM_DEVICES;

  if (argc > 1) source_dir = argv[1];

  if (timers_enabled) {
    timer_clear(TIMER_OPENCL);
    timer_clear(TIMER_BUILD);
    timer_clear(TIMER_BUFFER);
    timer_clear(TIMER_RELEASE);

    timer_start(TIMER_OPENCL);
  }

  // 1. Find the default device type and get a device for the device type
  //    Then, create sub-devices from the parent device.
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
  max_compute_units = 16;

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
    ZERO3_DIM = ZERO3_DIM_CPU;
    COMM1P_1_DIM = COMM1P_1_DIM_CPU;
    COMM1P_2_DIM = COMM1P_2_DIM_CPU;
    COMM1P_3_DIM = COMM1P_3_DIM_CPU;
    COMM1P_4_DIM = COMM1P_4_DIM_CPU;
    TAKE3_1_DIM = TAKE3_1_DIM_CPU;
    READY_1_DIM = READY_1_DIM_CPU;
    GIVE3_1_DIM = GIVE3_1_DIM_CPU;
    COMM1PEX_2_DIM = COMM1PEX_2_DIM_CPU;
    COMM1PEX_3_DIM = COMM1PEX_3_DIM_CPU;
    COMM1PEX_4_DIM = COMM1PEX_4_DIM_CPU;
    COMM1PEX_5_DIM = COMM1PEX_5_DIM_CPU;
    NORM2U3_DIM = NORM2U3_DIM_CPU;
    RESID_DIM = RESID_DIM_CPU;
    RPRJ3_DIM = RPRJ3_DIM_CPU;
    PSINV_DIM = PSINV_DIM_CPU;
    INTERP_1_DIM = INTERP_1_DIM_CPU;
    INTERP_2_DIM = INTERP_2_DIM_CPU;
    INTERP_3_DIM = INTERP_3_DIM_CPU;
    INTERP_4_DIM = INTERP_4_DIM_CPU;
    INTERP_5_DIM = INTERP_5_DIM_CPU;
    COPY_BUFFER_DIM = COPY_BUFFER_DIM_CPU;
  } else {
    ZERO3_DIM = ZERO3_DIM_GPU;
    COMM1P_1_DIM = COMM1P_1_DIM_GPU;
    COMM1P_2_DIM = COMM1P_2_DIM_GPU;
    COMM1P_3_DIM = COMM1P_3_DIM_GPU;
    COMM1P_4_DIM = COMM1P_4_DIM_GPU;
    TAKE3_1_DIM = TAKE3_1_DIM_GPU;
    READY_1_DIM = READY_1_DIM_GPU;
    GIVE3_1_DIM = GIVE3_1_DIM_GPU;
    COMM1PEX_2_DIM = COMM1PEX_2_DIM_GPU;
    COMM1PEX_3_DIM = COMM1PEX_3_DIM_GPU;
    COMM1PEX_4_DIM = COMM1PEX_4_DIM_GPU;
    COMM1PEX_5_DIM = COMM1PEX_5_DIM_GPU;
    NORM2U3_DIM = NORM2U3_DIM_GPU;
    RESID_DIM = RESID_DIM_GPU;
    RPRJ3_DIM = RPRJ3_DIM_GPU;
    PSINV_DIM = PSINV_DIM_GPU;
    INTERP_1_DIM = INTERP_1_DIM_GPU;
    INTERP_2_DIM = INTERP_2_DIM_GPU;
    INTERP_3_DIM = INTERP_3_DIM_GPU;
    INTERP_4_DIM = INTERP_4_DIM_GPU;
    INTERP_5_DIM = INTERP_5_DIM_GPU;
    COPY_BUFFER_DIM = COPY_BUFFER_DIM_GPU;
  }


  //setup global values
  nprocs = num_devices;
  int log2_size = LT_DEFAULT;
  int log2_nprocs = ilog2(nprocs);
  int lm = log2_size - log2_nprocs/3;
  int ndim1 = lm;
  int ndim3 = log2_size - (log2_nprocs+2)/3;
  int ndim2 = log2_size - (log2_nprocs+1)/3;

  NM = (2+(1<<lm));
  NV = ((2+(1<<ndim1))*(2+(1<<ndim2))*(2+(1<<ndim3)));
  NM2 = (2*NM*NM);
  NR = ((8*(NV+NM*NM+5*NM+14*LT_DEFAULT-7*lm))/7);
  M = (NM+1);

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
  char *source_file = "mg_kernel.cl";
  char build_option[50];
  if (device_type == CL_DEVICE_TYPE_CPU) {
    sprintf(build_option, "-DM=%d -DNM2=%d -I. -DUSE_CPU", M,NM2);
  } else if (device_type == CL_DEVICE_TYPE_GPU) {
    sprintf(build_option, "-DM=%d -DNM2=%d -I.", M,NM2);
  } else {
    fprintf(stderr, "Set the environment variable OPENCL_DEVICE_TYPE!\n");
    exit(EXIT_FAILURE);
  }

  p_program = clu_MakeProgram(context, devices, source_dir, source_file, build_option);

  program = (cl_program *)malloc(sizeof(cl_program) * num_devices);

  for (i = 0; i < num_devices; i++) {
    program[i] = p_program;
  }
  if (timers_enabled) timer_stop(TIMER_BUILD);

  // 5. Create kernels
  size_t asize = sizeof(cl_kernel) * num_devices;
  k_zero3 = (cl_kernel *)malloc(asize);
  k_power_task = (cl_kernel *)malloc(asize);
  k_zran3_task1= (cl_kernel *)malloc(asize);
  k_zran3_task2= (cl_kernel *)malloc(asize);
  k_zran3_task3= (cl_kernel *)malloc(asize);
  k_zran3_1= (cl_kernel *)malloc(asize);
  k_comm1p_1= (cl_kernel *)malloc(asize);
  k_comm1p_2_axis0 = (cl_kernel *)malloc(asize);
  k_comm1p_2_axis1 = (cl_kernel *)malloc(asize);
  k_comm1p_2_axis2 = (cl_kernel *)malloc(asize);
  k_comm1p_3= (cl_kernel *)malloc(asize);
  k_comm1p_4_axis0 = (cl_kernel *)malloc(asize);
  k_comm1p_4_axis1 = (cl_kernel *)malloc(asize);
  k_comm1p_4_axis2 = (cl_kernel *)malloc(asize);
  k_ready_1= (cl_kernel *)malloc(asize);
  k_give3_1_axis0 = (cl_kernel *)malloc(asize);
  k_give3_1_axis1 = (cl_kernel *)malloc(asize);
  k_give3_1_axis2 = (cl_kernel *)malloc(asize);
  k_take3_1_axis0 = (cl_kernel *)malloc(asize);
  k_take3_1_axis1 = (cl_kernel *)malloc(asize);
  k_take3_1_axis2 = (cl_kernel *)malloc(asize);
  k_comm1pex_2_axis0 = (cl_kernel *)malloc(asize);
  k_comm1pex_2_axis1 = (cl_kernel *)malloc(asize);
  k_comm1pex_2_axis2 = (cl_kernel *)malloc(asize);
  k_comm1pex_3_axis0 = (cl_kernel *)malloc(asize);
  k_comm1pex_3_axis1 = (cl_kernel *)malloc(asize);
  k_comm1pex_3_axis2 = (cl_kernel *)malloc(asize);
  k_comm1pex_4_axis0 = (cl_kernel *)malloc(asize);
  k_comm1pex_4_axis1 = (cl_kernel *)malloc(asize);
  k_comm1pex_4_axis2 = (cl_kernel *)malloc(asize);
  k_comm1pex_5_axis0 = (cl_kernel *)malloc(asize);
  k_comm1pex_5_axis1 = (cl_kernel *)malloc(asize);
  k_comm1pex_5_axis2 = (cl_kernel *)malloc(asize);
  k_norm2u3 = (cl_kernel *)malloc(asize);
  k_resid = (cl_kernel *)malloc(asize);
  k_rprj3 = (cl_kernel *)malloc(asize);
  k_psinv = (cl_kernel *)malloc(asize);
  k_interp_1 = (cl_kernel *)malloc(asize);
  k_interp_2 = (cl_kernel *)malloc(asize);
  k_interp_3 = (cl_kernel *)malloc(asize);
  k_interp_4 = (cl_kernel *)malloc(asize);
  k_interp_5 = (cl_kernel *)malloc(asize);
  k_copy_buffer = (cl_kernel *)malloc(asize);

  for (i = 0; i < num_devices; i++) {
    k_zero3[i] = clCreateKernel(program[i], "kernel_zero3", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_power_task[i] = clCreateKernel(program[i], "kernel_power_task", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_zran3_task1[i] = clCreateKernel(program[i], "kernel_zran3_task1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_zran3_task2[i] = clCreateKernel(program[i], "kernel_zran3_task2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_zran3_task3[i] = clCreateKernel(program[i], "kernel_zran3_task3", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_zran3_1[i] = clCreateKernel(program[i], "kernel_zran3_1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_comm1p_1[i] = clCreateKernel(program[i], "kernel_comm1p_1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_comm1p_2_axis0[i] = clCreateKernel(program[i], "kernel_comm1p_2_axis0", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1p_2_axis1[i] = clCreateKernel(program[i], "kernel_comm1p_2_axis1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1p_2_axis2[i] = clCreateKernel(program[i], "kernel_comm1p_2_axis2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_comm1p_3[i] = clCreateKernel(program[i], "kernel_comm1p_3", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_comm1p_4_axis0[i] = clCreateKernel(program[i], "kernel_comm1p_4_axis0", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1p_4_axis1[i] = clCreateKernel(program[i], "kernel_comm1p_4_axis1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1p_4_axis2[i] = clCreateKernel(program[i], "kernel_comm1p_4_axis2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_ready_1[i] = clCreateKernel(program[i], "kernel_ready_1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_give3_1_axis0[i] = clCreateKernel(program[i], "kernel_give3_1_axis0", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_give3_1_axis1[i] = clCreateKernel(program[i], "kernel_give3_1_axis1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_give3_1_axis2[i] = clCreateKernel(program[i], "kernel_give3_1_axis2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_take3_1_axis0[i] = clCreateKernel(program[i], "kernel_take3_1_axis0", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_take3_1_axis1[i] = clCreateKernel(program[i], "kernel_take3_1_axis1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_take3_1_axis2[i] = clCreateKernel(program[i], "kernel_take3_1_axis2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_comm1pex_2_axis0[i] = clCreateKernel(program[i], "kernel_comm1pex_2_axis0", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1pex_2_axis1[i] = clCreateKernel(program[i], "kernel_comm1pex_2_axis1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1pex_2_axis2[i] = clCreateKernel(program[i], "kernel_comm1pex_2_axis2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_comm1pex_3_axis0[i] = clCreateKernel(program[i], "kernel_comm1pex_3_axis0", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1pex_3_axis1[i] = clCreateKernel(program[i], "kernel_comm1pex_3_axis1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1pex_3_axis2[i] = clCreateKernel(program[i], "kernel_comm1pex_3_axis2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_comm1pex_4_axis0[i] = clCreateKernel(program[i], "kernel_comm1pex_4_axis0", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1pex_4_axis1[i] = clCreateKernel(program[i], "kernel_comm1pex_4_axis1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1pex_4_axis2[i] = clCreateKernel(program[i], "kernel_comm1pex_4_axis2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_comm1pex_5_axis0[i] = clCreateKernel(program[i], "kernel_comm1pex_5_axis0", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1pex_5_axis1[i] = clCreateKernel(program[i], "kernel_comm1pex_5_axis1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_comm1pex_5_axis2[i] = clCreateKernel(program[i], "kernel_comm1pex_5_axis2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_norm2u3[i] = clCreateKernel(program[i], "kernel_norm2u3", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_resid[i] = clCreateKernel(program[i], "kernel_resid", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_rprj3[i] = clCreateKernel(program[i], "kernel_rprj3", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_psinv[i] = clCreateKernel(program[i], "kernel_psinv", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_interp_1[i] = clCreateKernel(program[i], "kernel_interp_1", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_interp_2[i] = clCreateKernel(program[i], "kernel_interp_2", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_interp_3[i] = clCreateKernel(program[i], "kernel_interp_3", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_interp_4[i] = clCreateKernel(program[i], "kernel_interp_4", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");
    k_interp_5[i] = clCreateKernel(program[i], "kernel_interp_5", &ecode);
    clu_CheckError(ecode, "clCreateKernel()");

    k_copy_buffer [i] = clCreateKernel(program[i], "kernel_copy_buffer", &ecode);
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
    clReleaseMemObject(u_buf[i]);
    clReleaseMemObject(v_buf[i]);
    clReleaseMemObject(r_buf[i]);
    clReleaseMemObject(j1_buf[i]);
    clReleaseMemObject(j2_buf[i]);
    clReleaseMemObject(j3_buf[i]);

    clReleaseProgram(program[i]);
    clReleaseCommandQueue(cmd_queue[i]);
  }

  clReleaseContext(context);

  free(dead); free(give_ex); free(take_ex);
  free(nbr);
  free(is1); free(is2); free(is3);
  free(ie1); free(ie2); free(ie3);
  for (i = 0; i < MAXLEVEL+1; i++) 
  {
    free(m1[i]); free(m2[i]); free(m3[i]); free(ir[i]);
  }
  free(m1); free(m2); free(m3); free(ir);
  free(buff_buf);
  free(u_buf);
  free(v_buf);
  free(r_buf);
  free(j1_buf);
  free(j2_buf);
  free(j3_buf);
  free(power_task_out_buf);
  free(starts_buf);
  free(null_offset);

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
}
int ilog2(int i)
{
  int log2;
  int exp2 = 1;
  if (i <= 0) return(-1);

  for (log2 = 0; log2 < 30; log2++) {
    if (exp2 == i) return(log2);
    if (exp2 > i) break;
    exp2 *= 2;
  }
  return(-1);
}
void initBuff()
{
  cl_int ecode;
  double *temp = (double*)malloc( sizeof(double) * NM2 );
  memset(temp,0, sizeof(double) * NM2 );

  int i;
  for (i = 0; i < num_devices; i++) 
  {
    ecode = clEnqueueWriteBuffer(cmd_queue[i], 
        zero_buff_buf[i], 
        CL_FALSE, 
        0, 
        sizeof(double) * NM2,
        temp,
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  }
  CHECK_FINISH();
}
