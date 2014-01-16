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

#define MAXCELLS 3
#define MAX_DEVICE_NUM ((MAXCELLS)*(MAXCELLS))

extern int MAX_CELL_DIM, IMAX, JMAX, KMAX, IMAXP, JMAXP, BUF_SIZE;

/* common /global/ */
extern int ncells, grid_points[3];

/* common /constants/ */
extern double ce[5][13];

/* common /partition/ */
extern int cell_coord[MAX_DEVICE_NUM][MAXCELLS][3],
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
extern int west_size[MAX_DEVICE_NUM],
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
extern logical timeron;
#define t_total     0
#define t_rhs       1
#define t_xsolve    2
#define t_ysolve    3
#define t_zsolve    4
#define t_bpack     5
#define t_exch      6
#define t_xcomm     7
#define t_ycomm     8
#define t_zcomm     9
#define t_last      10


#define dt      DT_DEFAULT

//---------------------------------------------------------------------
void make_set();
void initialize();
void lhsinit();
void exact_rhs();
void set_constants();
void adi();
void compute_buffer_size(int dim);
void set_kernel_args();
void copy_faces();
void compute_rhs();
void x_solve();
void y_solve();
void z_solve();
void add();
void txinvr();
void error_norm(double rms[5]);
void rhs_norm(double rms[5]);
void verify(int no_time_steps, char *Class, logical *verified);
//---------------------------------------------------------------------
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
#define CHECK_FINISH(i)      ecode = clFinish(cmd_queue[i]); \
                             clu_CheckError(ecode, "clFinish");
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
extern cl_program       *program;
extern size_t  work_item_sizes[3];
extern size_t  max_work_group_size;
extern cl_uint max_compute_units;

extern cl_kernel* k_initialize1;
extern cl_kernel* k_initialize2;
extern cl_kernel* k_initialize3;
extern cl_kernel* k_initialize4;
extern cl_kernel* k_initialize5;
extern cl_kernel* k_initialize6;
extern cl_kernel* k_initialize7;
extern cl_kernel* k_initialize8;
extern cl_kernel* k_lhsinit;
extern cl_kernel* k_exact_rhs1;
extern cl_kernel* k_exact_rhs2;
extern cl_kernel* k_exact_rhs3;
extern cl_kernel* k_exact_rhs4;
extern cl_kernel* k_exact_rhs5;

extern cl_kernel (*k_copy_faces1)[MAXCELLS];
extern cl_kernel (*k_copy_faces2)[MAXCELLS];
extern cl_kernel (*k_copy_faces3)[MAXCELLS];
extern cl_kernel (*k_copy_faces4)[MAXCELLS];
extern cl_kernel (*k_copy_faces5)[MAXCELLS];
extern cl_kernel (*k_copy_faces6)[MAXCELLS];
extern cl_kernel (*k_compute_rhs1)[MAXCELLS];
extern cl_kernel (*k_compute_rhs2)[MAXCELLS];
extern cl_kernel (*k_compute_rhs3)[MAXCELLS];
extern cl_kernel (*k_compute_rhs4)[MAXCELLS];
extern cl_kernel (*k_compute_rhs5)[MAXCELLS];
extern cl_kernel (*k_compute_rhs6)[MAXCELLS];
extern cl_kernel (*k_txinvr)[MAXCELLS];
extern cl_kernel (*k_lhsx)[MAXCELLS];
extern cl_kernel (*k_ninvr)[MAXCELLS];
extern cl_kernel (*k_x_solve1)[MAXCELLS];
extern cl_kernel (*k_x_solve2)[MAXCELLS];
extern cl_kernel (*k_x_solve3)[MAXCELLS];
extern cl_kernel (*k_x_solve4)[MAXCELLS];
extern cl_kernel (*k_x_solve5)[MAXCELLS];
extern cl_kernel (*k_x_solve6)[MAXCELLS];
extern cl_kernel (*k_lhsy)[MAXCELLS];
extern cl_kernel (*k_pinvr)[MAXCELLS];
extern cl_kernel (*k_y_solve1)[MAXCELLS];
extern cl_kernel (*k_y_solve2)[MAXCELLS];
extern cl_kernel (*k_y_solve3)[MAXCELLS];
extern cl_kernel (*k_y_solve4)[MAXCELLS];
extern cl_kernel (*k_y_solve5)[MAXCELLS];
extern cl_kernel (*k_y_solve6)[MAXCELLS];
extern cl_kernel (*k_lhsz)[MAXCELLS];
extern cl_kernel (*k_tzetar)[MAXCELLS];
extern cl_kernel (*k_z_solve1)[MAXCELLS];
extern cl_kernel (*k_z_solve2)[MAXCELLS];
extern cl_kernel (*k_z_solve3)[MAXCELLS];
extern cl_kernel (*k_z_solve4)[MAXCELLS];
extern cl_kernel (*k_z_solve5)[MAXCELLS];
extern cl_kernel (*k_z_solve6)[MAXCELLS];
extern cl_kernel (*k_add)[MAXCELLS];

extern cl_kernel* k_error_norm;
extern cl_kernel* k_rhs_norm;

extern cl_mem* m_u;
extern cl_mem* m_us;
extern cl_mem* m_vs;
extern cl_mem* m_ws;
extern cl_mem* m_qs;
extern cl_mem* m_ainv;
extern cl_mem* m_rho_i;
extern cl_mem* m_speed;
extern cl_mem* m_square;
extern cl_mem* m_rhs;
extern cl_mem* m_forcing;
extern cl_mem* m_lhs;
extern cl_mem* m_in_buffer;
extern cl_mem* m_out_buffer;

extern cl_mem* m_cv;
extern cl_mem* m_rhon;
extern cl_mem* m_rhos;
extern cl_mem* m_rhoq;

extern cl_mem* m_ce;

extern int COPY_FACES1_DIM, COPY_FACES2_DIM, COPY_FACES3_DIM,
           COPY_FACES4_DIM, COPY_FACES5_DIM, COPY_FACES6_DIM,
           COMPUTE_RHS1_DIM, COMPUTE_RHS2_DIM, COMPUTE_RHS6_DIM,
           TXINVR_DIM, NINVR_DIM, X_SOLVE6_DIM,
           PINVR_DIM, Y_SOLVE6_DIM, TZETAR_DIM, Z_SOLVE6_DIM,
           ADD_DIM;

extern size_t copy_faces1_dim[MAX_DEVICE_NUM][MAXCELLS][2],
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

extern size_t copy_faces1_lw[MAX_DEVICE_NUM][MAXCELLS][2],
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

extern size_t copy_faces1_gw[MAX_DEVICE_NUM][MAXCELLS][2],
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

void compute_ws_dim_1(size_t d[], size_t local_ws[], size_t global_ws[]);
void compute_ws_dim_2(size_t d[], size_t local_ws[], size_t global_ws[]);
void compute_ws_dim_3(size_t d[], size_t local_ws[], size_t global_ws[]);
