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
// Parameter lm (declared and set in "npbparams.h") is the log-base2 of 
// the edge size max for the partition on a given node, so must be changed 
// either to save space (if running a small case) or made bigger for larger 
// cases, for example, 512^3. Thus lm=7 means that the largest dimension 
// of a partition that can be solved on a node is 2^7 = 128. lm is set 
// automatically in npbparams.h
// Parameters ndim1, ndim2, ndim3 are the local problem dimensions. 
//---------------------------------------------------------------------
#include "npbparams.h"

#include "type.h"

// actual dimension including ghost cells for communications
//#define NM          (2+(1<<LM))

// size of rhs array
//#define NV          ((2+(1<<NDIM1))*(2+(1<<NDIM2))*(2+(1<<NDIM3)))

// size of communication buffer
//#define NM2         (2*NM*NM)

// maximum number of levels
#define MAXLEVEL    (LT_DEFAULT+1)

// size of residual array
//#define NR          ((8*(NV+NM*NM+5*NM+14*LT_DEFAULT-7*LM))/7)

// this is the upper proc limit that 
// the current "NR" parameter can handle
#define MAXPROCS    131072

//---------------------------------------------------------------------
/* common /mg3/ */
static int (*nbr)[MAXLEVEL][3][3];
static int msg_type[3][3];
//static MPI_Request msg_id[2][3][3];
static int nx[MAXLEVEL+1];
static int ny[MAXLEVEL+1];
static int nz[MAXLEVEL+1];

/* common /ClassType/ */
static char Class;

/* common /my_debug/ */
static int debug_vec[8];

/* common /fap/ */
static int **ir;
static int **m1;
static int **m2;
static int **m3;
static int lt, lb;

/* common /comm_ex/ */
static logical (*dead)[MAXLEVEL];
static logical (*give_ex)[MAXLEVEL][3];
static logical (*take_ex)[MAXLEVEL][3];

//---------------------------------------------------------------------
// Set at m=1024, can handle cases up to 1024^3 case
//---------------------------------------------------------------------
//#define M   (NM+1)

/* common /buffer/ */
//static double buff[4][NM2];

//---------------------------------------------------------------------
/* common /timers/ */
static logical timeron;
#define t_bench        0    
#define t_init         1    
#define t_psinv        2    
#define t_resid        3    
#define t_rprj3        4     
#define t_ready        5    
#define t_give3        6    
#define t_take3        7    
#define t_give3ex      8    
#define t_take3ex      9    
#define t_comm1p_1     10   
#define t_comm1p_2     11   
#define t_comm1p_3     12   
#define t_comm1p_4     13   
#define t_comm1pex     14   
#define t_zran3        15   
#define t_zero3        16
#define t_power_task   17
#define t_interp       18   
#define t_norm2u3      19   
#define t_norm2u3_all  20
#define t_copy_buffer  21   
#define t_rcomm        22   
#define t_part1_resid   23   
#define t_part2_norm2u3 24   
#define t_part3_mg3p    25   
#define t_part4_resid   26   
#define t_part5_norm2u3 27   
#define t_comm3        28   
#define t_comm3_ex        29   
#define t_last          30  

logical timers_enabled;



















