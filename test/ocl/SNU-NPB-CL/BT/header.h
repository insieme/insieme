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
//---------------------------------------------------------------------
//
//  header.h
//
//---------------------------------------------------------------------
//---------------------------------------------------------------------
 
//---------------------------------------------------------------------
// The following include file is generated automatically by the
// "setparams" utility. It defines 
//      maxcells:      the square root of the maximum number of processors
//      problem_size:  12, 64, 102, 162 (for class T, A, B, C)
//      dt_default:    default time step for this problem size if no
//                     config file
//      niter_default: default number of iterations for this problem size
//---------------------------------------------------------------------
#include "npbparams.h"

#include "type.h"
#include "timers.h"

#define AA            0
#define BB            1
#define CC            2
#define BLOCK_SIZE    5

extern int MAXCELLS;
extern int MAX_CELL_DIM;
extern int IMAX;
extern int JMAX;
extern int KMAX;
extern int BUF_SIZE;

/* common /global/ */
extern int ncells, grid_points[3];

/* common /constants/ */
extern double tx1, tx2, tx3, ty1, ty2, ty3, tz1, tz2, tz3, 
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

#define EAST    2000
#define WEST    3000
#define NORTH   4000
#define SOUTH   5000
#define BOTTOM  6000
#define TOP     7000

/* common /partition/ */
extern int *g_cell_coord;
extern int *g_cell_low;
extern int *g_cell_high;
extern int *g_cell_size;
extern int *g_slice;
extern int *g_start;
extern int *g_end;
extern int (*predecessor)[3];
extern int (*successor)[3];
extern int (*max_cell_size)[3];

/* common /box/ */
#define west      0
#define east      1
#define south     2
#define north     3
#define bottom    4
#define top       5
#define NUM_DIR   6
extern int (*box_size)[NUM_DIR];
extern int (*start_send)[NUM_DIR];
extern int (*start_recv)[NUM_DIR];

extern int *g_p0_offset;
extern int *g_p1_offset;
extern int *g_p2_offset;
extern int *g_p3_offset;
extern int *g_p4_offset;
extern int *g_p5_offset;


/* common /tflags/ */
extern logical timeron;
#define t_total     0
#define t_io        1
#define t_rhs       2
#define t_xsolve    3
#define t_ysolve    4 
#define t_zsolve    5
#define t_bpack     6
#define t_exch      7
#define t_xcomm     8 
#define t_ycomm     9
#define t_zcomm     10
#define t_last      11


//-----------------------------------------------------------------------
void make_set();
void initialize();
void lhsinit();
void exact_rhs();
void set_constants();
void adi();
void compute_buffer_size(int dim);
void copy_faces();
void compute_rhs();
void x_solve();
void y_solve();
void z_solve();
void add();
void error_norm(double rms[5]);
void rhs_norm(double rms[5]);
void verify(int no_time_steps, char *Class, logical *verified);
//-----------------------------------------------------------------------


#include <CL/cl.h>
#include <CL/cl_ext.h>
#include "cl_util.h"

#define TIMER_OPENCL    20
#define TIMER_BUILD     21
#define TIMER_BUFFER    22
#define TIMER_RELEASE   23

#define USE_CHECK_FINISH
#define TIMER_DETAIL

#ifdef TIMER_DETAIL
#define DTIMER_START(id)    if (timeron) timer_start(id)
#define DTIMER_STOP(id)     if (timeron) timer_stop(id)
#else
#define DTIMER_START(id)
#define DTIMER_STOP(id)
#endif

#ifdef USE_CHECK_FINISH
#define CHECK_FINISH()      for (i = 0; i < num_devices; i++) { \
                              cl_int ecode = clFinish(cmd_queue[i]); \
                              clu_CheckError(ecode, "clFinish()"); \
                            }
#else
#define CHECK_FINISH()
#endif


//---------------------------------------------------------------------
// OPENCL Variables
//---------------------------------------------------------------------
extern cl_device_type    device_type;
extern cl_device_id      p_device;
extern char             *device_name;
extern cl_device_id     *devices;
extern cl_uint           num_devices;
extern cl_context        context;
extern cl_command_queue *cmd_queue;
extern cl_program       *p_initialize;
extern cl_program       *p_exact_rhs;
extern cl_program       *p_adi;
extern cl_program       *p_solve;
extern cl_program       *p_error;
extern size_t  work_item_sizes[3];
extern size_t  max_work_group_size;
extern cl_uint max_compute_units;

extern cl_kernel *k_cf_out1;
extern cl_kernel *k_cf_out2;
extern cl_kernel *k_cf_out3;
extern cl_kernel *k_cf_in1;
extern cl_kernel *k_cf_in2;
extern cl_kernel *k_cf_in3;
extern cl_kernel *k_compute_rhs1;
extern cl_kernel *k_compute_rhs2;
extern cl_kernel *k_compute_rhs3;
extern cl_kernel *k_compute_rhs4;
extern cl_kernel *k_compute_rhs5;
extern cl_kernel *k_compute_rhs6;
extern cl_kernel *k_x_usi;
extern cl_kernel *k_x_ssi;
extern cl_kernel *k_x_sbi;
extern cl_kernel *k_x_ubi;
extern cl_kernel *k_x_bs;
extern cl_kernel *k_x_sc;
extern cl_kernel *k_y_usi;
extern cl_kernel *k_y_ssi;
extern cl_kernel *k_y_sbi;
extern cl_kernel *k_y_ubi;
extern cl_kernel *k_y_bs;
extern cl_kernel *k_y_sc;
extern cl_kernel *k_z_usi;
extern cl_kernel *k_z_ssi;
extern cl_kernel *k_z_sbi;
extern cl_kernel *k_z_ubi;
extern cl_kernel *k_z_bs;
extern cl_kernel *k_z_sc;
extern cl_kernel *k_add;

extern cl_mem *m_cell_coord;
extern cl_mem *m_cell_low;
extern cl_mem *m_cell_high;
extern cl_mem *m_cell_size;
extern cl_mem *m_slice;
extern cl_mem *m_start;
extern cl_mem *m_end;
extern cl_mem *m_predecessor;
extern cl_mem *m_successor;
extern cl_mem *m_start_send;
extern cl_mem *m_start_recv;
extern cl_mem *m_p0_offset;
extern cl_mem *m_p1_offset;
extern cl_mem *m_p2_offset;
extern cl_mem *m_p3_offset;
extern cl_mem *m_p4_offset;
extern cl_mem *m_p5_offset;

extern cl_mem *m_ce;
extern cl_mem *m_us;
extern cl_mem *m_vs;
extern cl_mem *m_ws;
extern cl_mem *m_qs;
extern cl_mem *m_rho_i;
extern cl_mem *m_square;
extern cl_mem *m_forcing;
extern cl_mem *m_u;
extern cl_mem *m_rhs;
extern cl_mem *m_lhsc;
extern cl_mem *m_backsub_info;
extern cl_mem *m_in_buffer;
extern cl_mem *m_out_buffer;

extern int COPY_FACES_OUT1_DIM, COPY_FACES_OUT2_DIM, COPY_FACES_OUT3_DIM;
extern int COPY_FACES_IN1_DIM, COPY_FACES_IN2_DIM, COPY_FACES_IN3_DIM;
extern int COMPUTE_RHS1_DIM, COMPUTE_RHS2_DIM, COMPUTE_RHS6_DIM;
extern int X_SOLVE_DIM, Y_SOLVE_DIM, Z_SOLVE_DIM;
extern int ADD_DIM;

