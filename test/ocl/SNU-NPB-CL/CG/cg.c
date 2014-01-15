//-------------------------------------------------------------------------//
//                                                                         //
//  This benchmark is an OpenCL version of the NPB CG code for multiple    //
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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "randdp.h"
#include "timers.h"
#include "print_results.h"
#include "npbparams.h"
#include "type.h"
#include "timing.h"
#include <string.h>

#include <CL/cl.h>
#include "cl_util.h"

#define DEFAULT_NUM_SUBS    32

#define USE_CHUNK_SCHEDULE
#define CHUNK_WG            (16)
#define REDUCTION_WG        (16)

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
                              ecode = clFinish(cmd_queue[i]); \
                              clu_CheckError(ecode, "clFinish()"); \
                            }
//#else
//#define CHECK_FINISH()
#endif

logical timers_enabled;


static size_t GLOBAL_REDUCE = 1024;
static size_t LOCAL_REDUCE = 256;
static size_t LOCAL_SIZE = 1024;

// OPENCL Variables

size_t **global, **local;

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


static cl_kernel *k_conj_grad_1;
static cl_kernel *k_conj_grad_2;
static cl_kernel *k_conj_grad_3;
static cl_kernel *k_conj_grad_4;
static cl_kernel *k_conj_grad_5;
static cl_kernel *k_conj_grad_6;
static cl_kernel *k_conj_grad_7;
static cl_kernel *k_conj_grad_8;
static cl_kernel *k_conj_grad_9;
static cl_kernel *k_conj_grad_10;
static cl_kernel *k_conj_grad_11;
static cl_kernel *k_conj_grad_12;
static cl_kernel *k_main_1;
static cl_kernel *k_main_2;
static cl_kernel *k_main_2_1;
static cl_kernel *k_main_3;
static cl_kernel *k_main_4;

//c---------------------------------------------------------------------
//c  num_procs must be a power of 2, and num_procs=num_proc_cols*num_proc_rows.
//c  num_proc_cols and num_proc_cols are to be found in npbparams.h.
//c  When num_procs is not square, then num_proc_cols must be = 2*num_proc_rows.
//c---------------------------------------------------------------------
//      integer    num_procs
//      parameter( num_procs = num_proc_cols * num_proc_rows )

//num_procs = NUM_PROC_COLS * NUM_PROC_ROW;
//nz = NA*(NONZER+1)/num_procs*(NONZER+1)+NONZER + NA*(NONZER+2+num_procs/256)/NUM_PROC_COLS;

//#define NUM_PROCS NUM_PROC_COLS * NUM_PROC_ROWS
//#define NZ NA*(NONZER+1)/NUM_PROCS*(NONZER+1)+NONZER + NA*(NONZER+2+NUM_PROCS/256)/NUM_PROC_COLS

//MPI_Request request;
//MPI_Status status;
logical timeron;

//      common / partit_size  /
int NUM_PROC_COLS, NUM_PROC_ROWS, NUM_PROCS;
int nprocs, NZ;
int me;

int naa, nzz;
int npcols, nprows; //fixed
int *proc_col;
int *proc_row;
int *firstrow;
int *lastrow;
int *firstcol;
int *lastcol;
int *exch_proc;
int *exch_recv_length;
int *send_start;
int *send_len;

//int colidx[NZ+1];
int **colidx;
cl_mem *m_colidx;
//int rowstr[NA+1+1];
int **rowstr;
cl_mem *m_rowstr;
//int iv[2*NA+1+1];
int **iv;
cl_mem *m_iv;
//int arow[NZ+1];
int **arow;
cl_mem *m_arow;
//int acol[NZ+1];
int **acol;
cl_mem *m_acol;

cl_mem *d_rho;
cl_mem *d_temp1, *d_temp2;

int *temp_size;

//double v[NA+1+1];
//double aelt[NZ+1];
//double a[NZ+1];
//double x[NA/NUM_PROC_ROWS+2+1];
//double z[NA/NUM_PROC_ROWS+2+1];
//double p[NA/NUM_PROC_ROWS+2+1];
//double q[NA/NUM_PROC_ROWS+2+1];
//double r[NA/NUM_PROC_ROWS+2+1];
//double w[NA/NUM_PROC_ROWS+2+1];

double **v;
double **aelt;
double **a;
double **x;
double **z;
double **p;
double **q;
double **r;
double **w;

cl_mem *m_v;
cl_mem *m_aelt;
cl_mem *m_a;
cl_mem *m_x;
cl_mem *m_z;
cl_mem *m_p;
cl_mem *m_q;
cl_mem *m_r;
cl_mem *m_w;

double amult;
double *tran;

//int l2npcols;
//int reduce_exch_proc[NUM_PROC_COLS+1];
//int reduce_send_starts[NUM_PROC_COLS+1];
//int reduce_send_lengths[NUM_PROC_COLS+1];
//int reduce_recv_starts[NUM_PROC_COLS+1];
//int reduce_recv_lengths[NUM_PROC_COLS+1];

int l2npcols;
int **reduce_exch_proc;
int **reduce_send_starts;
int **reduce_send_lengths;
int **reduce_recv_starts;
int **reduce_recv_lengths;

double zeta;
double rnorm;
//double norm_temp1[2+1];
//double norm_temp2[2+1];
double **norm_temp1;
double **norm_temp2;

//--------------------------------------------------------------------
// Added by Sangmin
//--------------------------------------------------------------------
#ifdef USE_CHUNK_SCHEDULE
double *d, *sum, *rho, *rho0, *alpha, *beta;
double **temp_result1;
double **temp_result2;
double **temp_result;
#endif
//--------------------------------------------------------------------

double t, tmax, mflops;
char Class;
logical verified;
double zeta_verify_value, epsilon, err;

//double tsum[t_last+2+1];
double t1[t_last+2+1];
//double tming[t_last+2+1];
//double tmaxg[t_last+2+1];

//character        t_recs(t_last+2)*8
char* t_recs[t_last+2+1];
//data t_recs/'total', 'conjg', 'rcomm', 'ncomm',' totcomp', ' totcomm'/

void conj_grad (int **colidx, int **rowstr, double **x, double **z, double **a, double **p, double **q, double **r,
    double **w, double *rnorm, int l2npcols,
    int **reduce_exch_proc,
    int **reduce_send_starts,
    int **reduce_send_lengths,
    int **reduce_recv_starts,
    int **reduce_recv_lengths );
void sparse(double *a, int *colidx, int *rowstr, int n, int *arow, int *acol, double *aelt,
				int firstrow, int lastrow,
				double *x, logical *mark, int *nzloc, int nnza );
int icnvrt(double x, int ipwr2);
void sprnvc(int n, int nz, double *v, int *iv, int *nzloc, int *mark, int id);
void vecset(int n, double* v, int *iv, int *nzv, int i, double val);

void makea(int n, int nz, double *a, int *colidx, int *rowstr, int nonzer,
						int firstrow, int lastrow, int firstcol, int lastcol,
						double rcond, int *arow, int* acol, double* aelt, double* v, int *iv, double shift, int id);

void setup_proc_info(int num_procs, int num_proc_rows, int num_proc_cols );

int next_round(int x, int n){
	if (x%n != 0){
		return (x/n+1)*n;
	}else return x;
}

void initialize_arrays()
{

	//int colidx[NZ+1];
	//int rowstr[NA+1+1];
	//int iv[2*NA+1+1];
	//int arow[NZ+1];
	//int acol[NZ+1];

	int i, j;
	cl_int ecode;
	colidx = (int**)malloc(sizeof(int*) * nprocs);
	rowstr = (int**)malloc(sizeof(int*) * nprocs);
	iv = (int**)malloc(sizeof(int*) * nprocs);
	arow = (int**)malloc(sizeof(int*) * nprocs);
	acol = (int**)malloc(sizeof(int*) * nprocs);

	m_colidx = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_rowstr = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_iv = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_arow = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_acol = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);

	for (i = 0; i < nprocs; i++)
	{
		colidx[i] = (int*)malloc(sizeof(int)*(NZ+1));
		m_colidx[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int)*(NZ+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NZ+1; j++) colidx[i][j] = 0;

		rowstr[i] = (int*)malloc(sizeof(int)*(NA+1+1));
		m_rowstr[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int)*(NA+1+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NA+1+1; j++) rowstr[i][j] = 0;

		iv[i] = (int*)malloc(sizeof(int)*(2*NA+1+1));
		m_iv[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int)*(NA+1+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < 2*NA+1+1; j++) iv[i][j] = 0;

		arow[i] = (int*)malloc(sizeof(int)*(NZ+1));
		m_arow[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int)*(NZ+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NZ+1; j++) arow[i][j] = 0;

		acol[i] = (int*)malloc(sizeof(int)*(NZ+1));
		m_acol[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int)*(NZ+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NZ+1; j++) acol[i][j] = 0;
	}

	//double v[NA+1+1];
	//double aelt[NZ+1];
	//double a[NZ+1];
	//double x[NA/NUM_PROC_ROWS+2+1];
	//double z[NA/NUM_PROC_ROWS+2+1];
	//double p[NA/NUM_PROC_ROWS+2+1];
	//double q[NA/NUM_PROC_ROWS+2+1];
	//double r[NA/NUM_PROC_ROWS+2+1];
	//double w[NA/NUM_PROC_ROWS+2+1];
	v = (double**)malloc(sizeof(double*) * nprocs);
	aelt = (double**)malloc(sizeof(double*) * nprocs);
	a = (double**)malloc(sizeof(double*) * nprocs);
	x = (double**)malloc(sizeof(double*) * nprocs);
	z = (double**)malloc(sizeof(double*) * nprocs);
	p = (double**)malloc(sizeof(double*) * nprocs);
	q = (double**)malloc(sizeof(double*) * nprocs);
	r = (double**)malloc(sizeof(double*) * nprocs);
	w = (double**)malloc(sizeof(double*) * nprocs);

	m_v = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_aelt = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_a = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_x = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_z = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_p = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_q = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_r = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	m_w = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	d_rho = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	d_temp1 = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);
	d_temp2 = (cl_mem*)malloc(sizeof(cl_mem) * nprocs);

#ifdef USE_CHUNK_SCHEDULE
  sum = (double*)malloc(nprocs * sizeof(double));
  d = (double*)malloc(nprocs * sizeof(double));
  rho = (double*)malloc(nprocs * sizeof(double));
  rho0 = (double*)malloc(nprocs * sizeof(double));
  alpha = (double*)malloc(nprocs * sizeof(double));
  beta = (double*)malloc(nprocs * sizeof(double));
  temp_size = (int*)malloc(sizeof(int)* nprocs);
  temp_result1 = (double**)malloc(nprocs * sizeof(double*));
  temp_result2 = (double**)malloc(nprocs * sizeof(double*));
  temp_result = (double**)malloc(nprocs * sizeof(double*));
#endif

	for (i = 0; i < nprocs; i++)
	{
		v[i] = (double*)malloc(sizeof(double) * (NA+1+1));
		m_v[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double)*(NA+1+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NA+1+1; j++) v[i][j] = 0;

		aelt[i] = (double*)malloc(sizeof(double) * (NZ+1));
		m_aelt[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double)*(NZ+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NZ+1; j++) aelt[i][j] = 0;

		a[i] = (double*)malloc(sizeof(double) * (NZ+1));
		m_a[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double)*(NZ+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NZ+1; j++) a[i][j] = 0;

		x[i] = (double*)malloc(sizeof(double) * (NA/NUM_PROC_ROWS+2+1));
		m_x[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double)*(NA/NUM_PROC_ROWS+2+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NA/NUM_PROC_ROWS+2+1; j++) x[i][j] = 0;

		z[i] = (double*)malloc(sizeof(double) * (NA/NUM_PROC_ROWS+2+1));
		m_z[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double)*(NA/NUM_PROC_ROWS+2+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NA/NUM_PROC_ROWS+2+1; j++) z[i][j] = 0;

		p[i] = (double*)malloc(sizeof(double) * (NA/NUM_PROC_ROWS+2+1));
		m_p[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double)*(NA/NUM_PROC_ROWS+2+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NA/NUM_PROC_ROWS+2+1; j++) p[i][j] = 0;

		q[i] = (double*)malloc(sizeof(double) * (NA/NUM_PROC_ROWS+2+1));
		m_q[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double)*(NA/NUM_PROC_ROWS+2+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NA/NUM_PROC_ROWS+2+1; j++) q[i][j] = 0;

		r[i] = (double*)malloc(sizeof(double) * (NA/NUM_PROC_ROWS+2+1));
		m_r[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double)*(NA/NUM_PROC_ROWS+2+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NA/NUM_PROC_ROWS+2+1; j++) r[i][j] = 0;

		w[i] = (double*)malloc(sizeof(double) * (NA/NUM_PROC_ROWS+2+1));
		m_w[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(double)*(NA/NUM_PROC_ROWS+2+1), 0, &ecode);
		clu_CheckError(ecode, "clCreateBuffer()");
		for (j = 0; j < NA/NUM_PROC_ROWS+2+1; j++) w[i][j] = 0;

#ifdef USE_CHUNK_SCHEDULE
    temp_size[i]   = REDUCTION_WG;
    temp_result1[i] = (double*)malloc(sizeof(double)*REDUCTION_WG);
    temp_result2[i] = (double*)malloc(sizeof(double)*REDUCTION_WG);
    temp_result[i] = (double*)malloc(temp_size[i]*sizeof(double));

		d_rho[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, REDUCTION_WG * sizeof(double), 0, &ecode);
		d_temp1[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, REDUCTION_WG * sizeof(double), 0, &ecode);
		d_temp2[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, REDUCTION_WG * sizeof(double), 0, &ecode);
#else
		d_rho[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, GLOBAL_REDUCE/LOCAL_REDUCE * sizeof(double), 0, &ecode);
		d_temp1[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, GLOBAL_REDUCE/LOCAL_REDUCE * sizeof(double), 0, &ecode);
		d_temp2[i] = clCreateBuffer(context, CL_MEM_READ_WRITE, GLOBAL_REDUCE/LOCAL_REDUCE * sizeof(double), 0, &ecode);
#endif
	}

	tran = (double*)malloc(sizeof(double)*nprocs);

	//int l2npcols;
	//int reduce_exch_proc[NUM_PROC_COLS+1];
	//int reduce_send_starts[NUM_PROC_COLS+1];
	//int reduce_send_lengths[NUM_PROC_COLS+1];
	//int reduce_recv_starts[NUM_PROC_COLS+1];
	//int reduce_recv_lengths[NUM_PROC_COLS+1];

	reduce_exch_proc = (int**)malloc(sizeof(int*)*nprocs);
	reduce_send_starts = (int**)malloc(sizeof(int*)*nprocs);
	reduce_send_lengths = (int**)malloc(sizeof(int*)*nprocs);
	reduce_recv_starts = (int**)malloc(sizeof(int*)*nprocs);
	reduce_recv_lengths = (int**)malloc(sizeof(int*)*nprocs);

	for (i = 0; i < nprocs; i++){
		reduce_exch_proc[i] = (int*)malloc(sizeof(int)*(NUM_PROC_COLS+1));
		reduce_send_starts[i] = (int*)malloc(sizeof(int)*(NUM_PROC_COLS+1));
		reduce_send_lengths[i] = (int*)malloc(sizeof(int)*(NUM_PROC_COLS+1));
		reduce_recv_starts[i] = (int*)malloc(sizeof(int)*(NUM_PROC_COLS+1));
		reduce_recv_lengths[i] = (int*)malloc(sizeof(int)*(NUM_PROC_COLS+1));
		for (j = 0; j < NUM_PROC_COLS+1; j++){
			reduce_exch_proc[i][j] = 0;
			reduce_send_starts[i][j] = 0;
			reduce_send_lengths[i][j] = 0;
			reduce_recv_starts[i][j] = 0;
			reduce_recv_lengths[i][j] = 0;
		}
		tran[i] = 0;
	}

	norm_temp1 = (double**)malloc(sizeof(double*)*nprocs);
	norm_temp2 = (double**)malloc(sizeof(double*)*nprocs);
	for (i = 0; i < nprocs; i++){
		norm_temp1[i] = (double*)malloc(sizeof(double)*(2+1));
		norm_temp2[i] = (double*)malloc(sizeof(double)*(2+1));
		for (j=0; j < 3; j++){
			norm_temp1[i][j] = 0;
			norm_temp2[i][j] = 0;
		}
	}

	proc_row = (int*)malloc(sizeof(int) * nprocs);
	proc_col = (int*)malloc(sizeof(int) * nprocs);
	firstrow = (int*)malloc(sizeof(int) * nprocs);
	firstcol = (int*)malloc(sizeof(int) * nprocs);
	lastrow = (int*)malloc(sizeof(int) * nprocs);
	lastcol = (int*)malloc(sizeof(int) * nprocs);
	exch_proc = (int*)malloc(sizeof(int) * nprocs);
	exch_recv_length = (int*)malloc(sizeof(int) * nprocs);
	send_start = (int*)malloc(sizeof(int) * nprocs);
	send_len = (int*)malloc(sizeof(int) * nprocs);
	for (i = 0; i < nprocs; i++)
	{
		proc_row[i] = 0;
		proc_col[i] = 0;
		firstrow[i] = 0;
		firstcol[i] = 0;
		lastrow[i] = 0;
		lastcol[i] = 0;
		exch_proc[i] = 0;
		exch_recv_length[i] = 0;
		send_start[i] = 0;
		send_len[i] = 0;
	}
}

void setup_proc_info(int num_procs, int num_proc_rows, int num_proc_cols )
{
      int i;
      int log2nprocs;

	  for (i = num_proc_cols; i>0; i=i/2){
		  if( i != 1 && i/2*2 != i ){
			printf("ERROR: num_proc_cols is not a power of two!\n");
			exit(EXIT_FAILURE);
		  }
	  }

	  for (i = num_proc_rows; i>0; i=i/2){
		  if( i != 1 && i/2*2 != i ){
			printf("ERROR: num_proc_cols is not a power of two!\n");
			exit(EXIT_FAILURE);
		  }
	  }

	  log2nprocs = 0;
	  for (i = nprocs; i/2>0; i=i/2){
		  if( i != 1 && i/2*2 != i ){
			printf("ERROR: num_proc_cols is not a power of two!\n");
			exit(EXIT_FAILURE);
		  }
		  log2nprocs++;
	  }

      npcols = num_proc_cols;
      nprows = num_proc_rows;
}



void setup_submatrix_info( int *l2npcols, //reference
     int *reduce_exch_proc,
     int *reduce_send_starts,
     int *reduce_send_lengths,
     int *reduce_recv_starts,
     int *reduce_recv_lengths )
{
      int col_size, row_size;
      int i, j;
      int div_factor;

      proc_row[me] = me / npcols;
      proc_col[me] = me - proc_row[me]*npcols;

//c  If naa evenly divisible by npcols, then it is evenly divisible
//c  by nprows


      if (naa/npcols*npcols == naa ){
          col_size = naa/npcols;
          firstcol[me] = proc_col[me]*col_size + 1;
          lastcol[me]  = firstcol[me] - 1 + col_size;
          row_size = naa/nprows;
          firstrow[me] = proc_row[me]*row_size + 1;
          lastrow[me]  = firstrow[me] - 1 + row_size;

      }

//c  If naa not evenly divisible by npcols, then first subdivide for nprows
//c  and then, if npcols not equal to nprows (i.e., not a sq number of procs),
//c  get col subdivisions by dividing by 2 each row subdivision.

      else {
    	  if( proc_row[me] < naa - naa/nprows*nprows){
              row_size = naa/nprows+ 1;
              firstrow[me] = proc_row[me]*row_size + 1;
              lastrow[me]  = firstrow[me] - 1 + row_size;
          }
          else {
              row_size = naa/nprows;
              firstrow[me] = (naa - naa/nprows*nprows)*(row_size+1) + (proc_row[me]-(naa-naa/nprows*nprows))*row_size + 1;
              lastrow[me]  = firstrow[me] - 1 + row_size;
          }
          if( npcols == nprows ){
              if( proc_col[me] < naa - naa/npcols*npcols ){
                  col_size = naa/npcols+ 1;
                  firstcol[me] = proc_col[me]*col_size + 1;
                  lastcol[me]  = firstcol[me] - 1 + col_size;
              }
              else {
                  col_size = naa/npcols;
                  firstcol[me] = (naa - naa/npcols*npcols)*(col_size+1) + (proc_col[me]-(naa-naa/npcols*npcols))*col_size + 1;
                  lastcol[me]  = firstcol[me] - 1 + col_size;
              }
          }
          else {
        	  if ((proc_col[me]/2)< naa - naa/(npcols/2)*(npcols/2)){
                  col_size = naa/(npcols/2) + 1;
                  firstcol[me] = (proc_col[me]/2)*col_size + 1;
                  lastcol[me]  = firstcol[me] - 1 + col_size;
			  }
              else{
                  col_size = naa/(npcols/2);
                  firstcol[me] = (naa - naa/(npcols/2)*(npcols/2))*(col_size+1)+ ((proc_col[me]/2)-(naa-naa/(npcols/2)*(npcols/2)))*col_size + 1;
                  lastcol[me] = firstcol[me] - 1 + col_size;
              }
			  printf("*,*: %d, %d, %d\n", col_size,firstcol[me],lastcol[me]);
              if (me%2 == 0 )
                  lastcol[me]  = firstcol[me] - 1 + (col_size-1)/2 + 1;
              else {
                  firstcol[me] = firstcol[me] + (col_size-1)/2 + 1;
                  lastcol[me] = firstcol[me] - 1 + col_size/2;
                  printf("*,*: %d, %d\n", firstcol[me], lastcol[me]);
              }
          }
		}

      if( npcols == nprows ){
          send_start[me] = 1;
          send_len[me] = lastrow[me] - firstrow[me] + 1;
      }
      else {
          if( me%2 == 0 ){
              send_start[me] = 1;
              send_len[me]   = (1 + lastrow[me]-firstrow[me]+1)/2;
          }
          else {
              send_start[me] = (1 + lastrow[me]-firstrow[me]+1)/2 + 1;
              send_len[me]   = (lastrow[me]-firstrow[me]+1)/2;
          }
      }

//Transpose exchange processor
      if (npcols == nprows )
          exch_proc[me] = (me%nprows) *nprows + me/nprows;
      else
          exch_proc[me] = 2*(((me/2)%nprows )*nprows + me/2/nprows) + me%2;

      i = npcols / 2;
      *l2npcols = 0;
      while (i>0) {
         *l2npcols = *l2npcols + 1;
         i = i / 2;
      }

//Set up the reduce phase schedules...
      div_factor = npcols;
      for (i = 1; i <= *l2npcols; i++){
         j = (proc_col[me]+div_factor/2) % div_factor + proc_col[me] / div_factor * div_factor;
         reduce_exch_proc[i] = proc_row[me]*npcols + j;
         div_factor = div_factor / 2;
      }

      for (i = *l2npcols; i >=1; i--){
		if (nprows == npcols ) {
		   reduce_send_starts[i]  = send_start[me];
		   reduce_send_lengths[i] = send_len[me];
		   reduce_recv_lengths[i] = lastrow[me] - firstrow[me] + 1;
		}
		else {
		   reduce_recv_lengths[i] = send_len[me];
		   if (i == *l2npcols) {
			  reduce_send_lengths[i] = lastrow[me]-firstrow[me]+1 - send_len[me];
			  if (me/2*2 == me)
				 reduce_send_starts[i] = send_start[me] + send_len[me];
			  else
				 reduce_send_starts[i] = 1;
		   }
		   else {
			  reduce_send_lengths[i] = send_len[me];
			  reduce_send_starts[i]  = send_start[me];
		   }
		}
		reduce_recv_starts[i] = send_start[me];
      }
      exch_recv_length[me] = lastcol[me] - firstcol[me] + 1;
}


//---------------------------------------------------------------------
// Set up the OpenCL environment.
//---------------------------------------------------------------------
static void setup_opencl(int argc, char *argv[])
{
  int i;
  cl_int ecode;
  char *source_dir = ".";  //FIXME
  int num_cus;

  if (argc > 1) source_dir = argv[1];

  if (timers_enabled) {
    timer_clear(TIMER_OPENCL);
    timer_clear(TIMER_BUILD);
    timer_clear(TIMER_BUFFER);
    timer_clear(TIMER_RELEASE);
    timer_start(TIMER_OPENCL);
  }

  // 1. Find the default device type and get a device for the device type

  device_type = CL_DEVICE_TYPE_CPU;

  cl_platform_id platform;
  ecode = clGetPlatformIDs(1, &platform, NULL);
  clu_CheckError(ecode, "clGetPlatformIDs()");

  ecode = clGetDeviceIDs(platform, device_type, 0, NULL, &num_devices);
  clu_CheckError(ecode, "clGetDeviceIDs()");

  devices = (cl_device_id *)malloc(sizeof(cl_device_id) * num_devices);

  ecode = clGetDeviceIDs(platform, device_type, num_devices, devices, NULL);
  clu_CheckError(ecode, "clGetDeviceIDs()");

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
  cmd_queue = (cl_command_queue*)malloc(sizeof(cl_command_queue)*num_devices);
  for (i = 0; i < num_devices; i++) {
    cmd_queue[i] = clCreateCommandQueue(context, devices[i], 0, &ecode);
    clu_CheckError(ecode, "clCreateCommandQueue()");
  }

  // 4. Build the program
  if (timers_enabled) timer_start(TIMER_BUILD);
  char *source_file = "cg_kernel.cl";
  char build_option[50];
  if (device_type == CL_DEVICE_TYPE_CPU) {
#ifdef USE_CHUNK_SCHEDULE
    sprintf(build_option, "-I. -DCLASS=%d -DUSE_CPU -DUSE_CHUNK_SCHEDULE", Class);
#else
    sprintf(build_option, "-I. -DCLASS=%d -DUSE_CPU", Class);
#endif
  } else if (device_type == CL_DEVICE_TYPE_GPU) {
#ifdef USE_CHUNK_SCHEDULE
    sprintf(build_option, "-I. -DCLASS=%d -DUSE_CHUNK_SCHEDULE", Class);
#else
    sprintf(build_option, "-I. -DCLASS=%d", Class);
#endif
  } else {
    fprintf(stderr, "Set the environment variable OPENCL_DEVICE_TYPE!\n");
    exit(EXIT_FAILURE);
  }

  p_program = clu_MakeProgram(context, devices, source_dir, source_file, build_option);

  // FIXME: Can we share the program among all devices if they are the same?
  program = (cl_program *)malloc(sizeof(cl_program) * num_devices);
  for (i = 0; i < num_devices; i++) {
    program[i] = p_program;
  }
  if (timers_enabled) timer_stop(TIMER_BUILD);

  // 5. Create kernels
  //size_t asize = sizeof(cl_kernel) * num_devices;
  //k_compute_indexmap = (cl_kernel *)malloc(asize);

  if (timers_enabled) timer_stop(TIMER_OPENCL);
}


void makea(int n, int nz, double *a, int *colidx, int *rowstr, int nonzer,
						int firstrow, int lastrow, int firstcol, int lastcol,
						double rcond, int *arow, int* acol, double* aelt, double* v, int *iv, double shift, int id)
{
  int i, nnza, iouter, ivelt, ivelt1, irow, nzv, jcol;

  //nonzer is approximately  (int(sqrt(nnza /n)));

  double  size, ratio, scale;
  //external          sparse, sprnvc, vecset
  size = 1.0;
  ratio = pow(rcond, (1.0 / (double)(n)));
  nnza = 0;

  //Initialize iv(n+1 .. 2n) to zero.
  //Used by sprnvc to mark nonzero positions

  for (i=1; i<=n; i++) iv[n+i] = 0;

  for (iouter = 1; iouter <= n;iouter++){
    nzv = nonzer;
    sprnvc( n, nzv, v, colidx, (int*)&iv[1], (int*)&iv[n+1], id);
    vecset( n, v, colidx, &nzv, iouter, 0.5 );


    for (ivelt = 1; ivelt <= nzv; ivelt++){
      jcol = colidx[ivelt];
      if (jcol>=firstcol && jcol<=lastcol) {
        scale = size * v[ivelt];
        for (ivelt1 = 1; ivelt1 <= nzv; ivelt1++)
        {
          irow = colidx[ivelt1];
          if (irow>=firstrow && irow<=lastrow) {
            nnza = nnza + 1;
            if (nnza > nz) {
              printf("Space for matrix elements exceeded in makea\n");
              printf("nnza, nzmax = %d, %d\n", nnza, nz);
              printf("iouter = %d\n",iouter);
              return;
            }
            acol[nnza] = jcol;
            arow[nnza] = irow;
            aelt[nnza] = v[ivelt1] * scale;
          }
        }
      }
    }
    size = size * ratio;
  }

  //printf("id: %d, nnaa : %d\n", id, nnza);

  //c       ... add the identity * rcond to the generated matrix to bound
  //c           the smallest eigenvalue from below by rcond

  //printf("id: %d, %d %d %d %d\n", firstrow, lastrow, firstcol, lastcol);

  for (i = firstrow; i<=lastrow; i++){
    if (i>=firstcol && i<=lastcol) {
      iouter = n + i;
      nnza = nnza + 1;
      if (nnza > nz) {
        printf("Space for matrix elements exceeded in makea\n");
        printf("nnza, nzmax = %d, %d\n", nnza, nz);
        printf("iouter = %d\n",iouter);
        return;
      }
      acol[nnza] = i;
      arow[nnza] = i;
      aelt[nnza] = rcond - shift;
    }
  }


  //c       ... make the sparse matrix from list of elements with duplicates
  //c           (v and iv are used as  workspace)
  sparse(a, colidx, rowstr, n, arow, acol, aelt,
      firstrow, lastrow,v, (logical*)&iv[1], (int*)&iv[n+1], nnza);
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

int ipow2(int i)
{
  int pow2 = 1;
  if (i < 0) return(-1);
  if (i == 0) return(1);
  while(i--) pow2 *= 2;
  return(pow2);
}

#ifdef CLUSTER
int __main(int argc, char** argv)
#else
int main(int argc, char *argv[]) 
#endif
{
  setup_opencl(argc, argv);
  nprocs = num_devices;
  int i, j, k, it, id, ecode;

  for (i = nprocs; i>0; i=i/2){
    if( i != 1 && i/2*2 != i ){
      printf("ERROR: nprocs is not a power of two!\n");
      exit(EXIT_FAILURE);
    }
  }
  NUM_PROC_COLS = NUM_PROC_ROWS = ilog2(nprocs)/2;
  if (NUM_PROC_COLS+NUM_PROC_ROWS != ilog2(nprocs)) NUM_PROC_COLS += 1;
  NUM_PROC_COLS = ipow2(NUM_PROC_COLS); NUM_PROC_ROWS = ipow2(NUM_PROC_ROWS);
  NUM_PROCS = nprocs;
  NZ = NA*(NONZER+1)/NUM_PROCS*(NONZER+1)+NONZER + NA*(NONZER+2+NUM_PROCS/256)/NUM_PROC_COLS;

  t_recs[1] = "total";
  t_recs[2] = "conjg";
  t_recs[3] = "rcomm";
  t_recs[4] = "ncomm";
  t_recs[t_last+1] = "totcomp";
  t_recs[t_last+2] = "totcomm";

  t_recs[t_conjg_1]   = "conjg_1";
  t_recs[t_conjg_2]   = "conjg_2";
  t_recs[t_conjg_3]   = "conjg_3";
  t_recs[t_conjg_4]   = "conjg_4";
  t_recs[t_conjg_5]   = "conjg_5";
  t_recs[t_conjg_6]   = "conjg_6";
  t_recs[t_conjg_7]   = "conjg_7";
  t_recs[t_conjg_8]   = "conjg_8";
  t_recs[t_conjg_9]   = "conjg_9";
  t_recs[t_conjg_10]  = "conjg_10";

  initialize_arrays();


  //subroutine initialize_mpi

  //    MPI_Init(&argc, &argv);
  //    MPI_Comm_rank(MPI_COMM_WORLD, &me);
  //    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

  FILE *fp;

  timeron = false;
  if ((fp = fopen("timer.flag", "r")) != NULL) {
    timeron = true;
    fclose(fp);
  }


  //    MPI_Bcast(&timeron, 1, MPI_INT, 0, MPI_COMM_WORLD);

  //Set up mpi initialization and number of proc testing
  //call initialize_mpi

  if (NA == 1400 && NONZER == 7 && NITER == 15 && SHIFT == 10) {
    Class = 'S';
    zeta_verify_value = 8.5971775078648;
  } else if (NA == 7000 && NONZER == 8 && NITER == 15 && SHIFT == 12) {
    Class = 'W';
    zeta_verify_value = 10.362595087124;
  } else if (NA == 14000 && NONZER == 11 && NITER == 15 && SHIFT == 20) {
    Class = 'A';
    zeta_verify_value = 17.130235054029;
  } else if (NA == 75000 && NONZER == 13 && NITER == 75 && SHIFT == 60) {
    Class = 'B';
    zeta_verify_value = 22.712745482631;
  } else if (NA == 150000 && NONZER == 15 && NITER == 75 && SHIFT == 110) {
    Class = 'C';
    zeta_verify_value = 28.973605592845;
  } else if (NA == 1500000 && NONZER == 21 && NITER == 100 && SHIFT == 500) {
    Class = 'D';
    zeta_verify_value = 52.514532105794;
  } else if (NA == 9000000 && NONZER == 26 && NITER == 100 && SHIFT == 1500) {
    Class = 'E';
    zeta_verify_value = 77.522164599383;
  } else {
    Class = 'U';
  }

  //	if (! convertdouble)
  //	 dp_type = MPI_DOUBLE_PRECISION;
  //	else
  //	 dp_type = MPI_REAL;
  //dp_type = MPI_DOUBLE;

  naa = NA;
  nzz = NZ;

  //Set up processor info, such as whether sq num of procs, etc
  setup_proc_info( NUM_PROCS, NUM_PROC_ROWS, NUM_PROC_COLS);

  //setup OPENCL stuffs

  global = (size_t**)malloc(sizeof(size_t*)*nprocs);
  local = (size_t**)malloc(sizeof(size_t*)*nprocs);
#ifndef USE_CHUNK_SCHEDULE
  double **temp_result1 = (double**)malloc(sizeof(double*) * nprocs);
  double **temp_result2 = (double**)malloc(sizeof(double*) * nprocs);
  temp_size = (int*)malloc(sizeof(int)* nprocs);
#endif
  for (id = 0; id < nprocs; id++){
    global[id] = (size_t*)malloc(sizeof(size_t)*3);
    local[id] = (size_t*)malloc(sizeof(size_t)*3);
#ifndef USE_CHUNK_SCHEDULE
    temp_result1[id] = (double*)malloc(sizeof(double)*(GLOBAL_REDUCE/LOCAL_REDUCE));
    temp_result2[id] = (double*)malloc(sizeof(double)*(GLOBAL_REDUCE/LOCAL_REDUCE));
#endif
  }

  size_t asize = sizeof(cl_kernel) * nprocs;
  k_conj_grad_1 = (cl_kernel*)malloc(asize);
  k_conj_grad_2 = (cl_kernel*)malloc(asize);
  k_conj_grad_3 = (cl_kernel*)malloc(asize);
  k_conj_grad_4 = (cl_kernel*)malloc(asize);
  k_conj_grad_5 = (cl_kernel*)malloc(asize);
  k_conj_grad_6 = (cl_kernel*)malloc(asize);
  k_conj_grad_7 = (cl_kernel*)malloc(asize);
  k_conj_grad_8 = (cl_kernel*)malloc(asize);
  k_conj_grad_9 = (cl_kernel*)malloc(asize);
  k_conj_grad_10 = (cl_kernel*)malloc(asize);
  k_conj_grad_11 = (cl_kernel*)malloc(asize);
  k_conj_grad_12 = (cl_kernel*)malloc(asize);

  k_main_1 = (cl_kernel*)malloc(asize);
  k_main_2 = (cl_kernel*)malloc(asize);
  k_main_2_1 = (cl_kernel*)malloc(asize);
  k_main_3 = (cl_kernel*)malloc(asize);
  k_main_4 = (cl_kernel*)malloc(asize);

  for (id = 0; id < nprocs; id++){
    k_conj_grad_1[id] = clCreateKernel(program[id], "conj_grad_1", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for conj_grad_1");
    ecode  = clSetKernelArg(k_conj_grad_1[id], 0, sizeof(cl_mem), &m_q[id]);
    ecode |= clSetKernelArg(k_conj_grad_1[id], 1, sizeof(cl_mem), &m_z[id]);
    ecode |= clSetKernelArg(k_conj_grad_1[id], 2, sizeof(cl_mem), &m_r[id]);
    ecode |= clSetKernelArg(k_conj_grad_1[id], 3, sizeof(cl_mem), &m_x[id]);
    ecode |= clSetKernelArg(k_conj_grad_1[id], 4, sizeof(cl_mem), &m_p[id]);
    ecode |= clSetKernelArg(k_conj_grad_1[id], 5, sizeof(cl_mem), &m_w[id]);
    clu_CheckError(ecode, "clSetKernelArg() for conj_grad_1");

    k_conj_grad_2[id] = clCreateKernel(program[id], "conj_grad_2", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for conj_grad_2");
    k_conj_grad_3[id] = clCreateKernel(program[id], "conj_grad_3", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for conj_grad_3");
    k_conj_grad_4[id] = clCreateKernel(program[id], "conj_grad_4", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for conj_grad_4");
    k_conj_grad_5[id] = clCreateKernel(program[id], "conj_grad_5", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for conj_grad_5");
    k_conj_grad_6[id] = clCreateKernel(program[id], "conj_grad_6", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for conj_grad_6");
    k_conj_grad_7[id] = clCreateKernel(program[id], "conj_grad_7", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for conj_grad_7");
    k_conj_grad_8[id] = clCreateKernel(program[id], "conj_grad_8", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for conj_grad_8");
    k_conj_grad_9[id] = clCreateKernel(program[id], "conj_grad_9", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for conj_grad_9");
    k_conj_grad_10[id] = clCreateKernel(program[id], "conj_grad_10", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for conj_grad_10");

    k_main_1[id] = clCreateKernel(program[id], "main_1", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for main_1");
    k_main_2[id] = clCreateKernel(program[id], "main_2", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for main_2");
    //		k_main_2_1[id] = clCreateKernel(program[id], "main_2_1", &ecode);
    //		clu_CheckError(ecode, "clCreateKernel() for main_2_1");
    k_main_3[id] = clCreateKernel(program[id], "main_3", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for main_3");
    k_main_4[id] = clCreateKernel(program[id], "main_4", &ecode);
    clu_CheckError(ecode, "clCreateKernel() for main_4");
  }


  //Set up partition's submatrix info: firstcol, lastcol, firstrow, lastrow
  for (id = 0; id < nprocs; id++){
    me = id;
    setup_submatrix_info(&l2npcols,
        reduce_exch_proc[id],
        reduce_send_starts[id],
        reduce_send_lengths[id],
        reduce_recv_starts[id],
        reduce_recv_lengths[id]);
  }

  for (i = 1; i <= t_last; i++) timer_clear(i);

  //Inialize random number generator
  for (id=0; id <nprocs; id++){
    tran[id]    = 314159265.0;
    amult   = 1220703125.0;
    zeta    = randlc(&tran[id], amult);
  }

  //printf("zeta  : %20.13E\n", zeta);

  //Set up partition's sparse random matrix for given class size
  for (id = 0; id < nprocs; id++){
    makea(naa, nzz, a[id], colidx[id], rowstr[id], NONZER,
        firstrow[id], lastrow[id], firstcol[id], lastcol[id],
        RCOND, arow[id], acol[id], aelt[id], v[id], iv[id], SHIFT, id);
    //printf("me : %d; value : %d\n", id, rowstr[id][lastrow[id]-firstrow[id]+1]);
  }

  for (id = 0; id < nprocs; id++){
    ecode = clEnqueueWriteBuffer(cmd_queue[id], m_p[id], CL_FALSE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), p[id],0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    ecode = clEnqueueWriteBuffer(cmd_queue[id], m_w[id], CL_FALSE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), w[id],0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    ecode = clEnqueueWriteBuffer(cmd_queue[id], m_a[id], CL_FALSE, 0, (NZ+1) * sizeof(double), a[id],0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    ecode = clEnqueueWriteBuffer(cmd_queue[id], m_rowstr[id], CL_FALSE, 0, (NA+1+1) * sizeof(int), rowstr[id],0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    ecode = clEnqueueWriteBuffer(cmd_queue[id], m_colidx[id], CL_FALSE, 0, (NZ+1) * sizeof(int), colidx[id],0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  }
  for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
  for (id = 0; id < nprocs; id++){
    for (id = 0; id < nprocs; id++){
      int length = lastrow[id]-firstrow[id]+1;
      local[id][0] = LOCAL_SIZE;
      global[id][0] = next_round(length, local[id][0]);

      ecode = clSetKernelArg(k_main_1[id], 0, sizeof(cl_mem), &m_colidx[id]);
      ecode = clSetKernelArg(k_main_1[id], 1, sizeof(cl_mem), &m_rowstr[id]);
      ecode = clSetKernelArg(k_main_1[id], 2, sizeof(int), &firstcol[id]);
      ecode = clSetKernelArg(k_main_1[id], 3, sizeof(int), &length);
      clu_CheckError(ecode, "clSetKernelArg() for main_1");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id],
          k_main_1[id],
          1, NULL,
          global[id], local[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for main_1");
    }
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    //		for (id = 0; id < nprocs; id++) {
    //			int length = NA/NUM_PROC_ROWS+1;
    //			ecode = clEnqueueReadBuffer(cmd_queue[id], m_x[id], CL_TRUE, 0, (length+1) * sizeof(double), x[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //		}
  }

  //    for (id = 0; id < nprocs; id++){
  //		for (j=1; j<=lastrow[id]-firstrow[id]+1; j++){
  //			for (k=rowstr[id][j]; k<rowstr[id][j+1]; k++){
  //				colidx[id][k] = colidx[id][k] - firstcol[id] + 1;
  //			}
  //		}
  //    }


  for (id = 0; id < nprocs; id++){
    for (id = 0; id < nprocs; id++){
      int length = NA/NUM_PROC_ROWS+1;
      local[id][0] = LOCAL_SIZE;
      global[id][0] = next_round(length, local[id][0]);

      ecode = clSetKernelArg(k_main_2[id], 0, sizeof(cl_mem), &m_x[id]);
      ecode = clSetKernelArg(k_main_2[id], 1, sizeof(int), &length);
      clu_CheckError(ecode, "clSetKernelArg() for main_2");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id],
          k_main_2[id],
          1, NULL,
          global[id], local[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for main_2");
    }
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    for (id = 0; id < nprocs; id++) {
      int length = NA/NUM_PROC_ROWS+1;
      ecode = clEnqueueReadBuffer(cmd_queue[id], m_x[id], CL_FALSE, 0, (length+1) * sizeof(double), x[id],0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueReadBuffer()");
    }
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
  }

  //    for (id = 0; id < nprocs; id++){
  //		for (i=1; i<=NA/NUM_PROC_ROWS+1; i++){
  //			x[id][i] = 1.0;
  //		}
  //    }
  zeta  = 0.0;

  //warming up

  for (it=1; it <= 1; it++){
    conj_grad (colidx, rowstr, x, z, a, p, q, r, w, &rnorm,
        l2npcols,
        reduce_exch_proc,
        reduce_send_starts,
        reduce_send_lengths,
        reduce_recv_starts,
        reduce_recv_lengths);

    for (id = 0; id < nprocs; id++){

      //			norm_temp1[id][1] = 0.0;
      //			norm_temp1[id][2] = 0.0;

      //			for (j=1; j<=lastcol[id]-firstcol[id]+1; j++){
      //				norm_temp1[id][1] = norm_temp1[id][1] + x[id][j]*z[id][j];
      //				norm_temp1[id][2] = norm_temp1[id][2] + z[id][j]*z[id][j];
      //			}

      int array_length = lastcol[id] - firstcol[id] + 1;
#ifdef USE_CHUNK_SCHEDULE
      global[id][0] = REDUCTION_WG;
      local[id][0] = 1;
#else
      global[id][0] = GLOBAL_REDUCE;
      local[id][0] = LOCAL_REDUCE;
#endif
      temp_size[id] = (int)(global[id][0] / local[id][0]);

      if (device_type == CL_DEVICE_TYPE_CPU) {
        ecode  = clSetKernelArg(k_main_3[id], 0, sizeof(cl_mem), &m_x[id]);
        ecode |= clSetKernelArg(k_main_3[id], 1, sizeof(cl_mem), &m_z[id]);
        ecode |= clSetKernelArg(k_main_3[id], 2, local[id][0]*2*sizeof(double), NULL);
        ecode |= clSetKernelArg(k_main_3[id], 3, local[id][0]*2*sizeof(double), NULL);
        ecode |= clSetKernelArg(k_main_3[id], 4, sizeof(int), &array_length);
        ecode |= clSetKernelArg(k_main_3[id], 5, sizeof(cl_mem), &d_temp1[id]);
        ecode |= clSetKernelArg(k_main_3[id], 6, sizeof(cl_mem), &d_temp2[id]);
      } else {
        ecode  = clSetKernelArg(k_main_3[id], 0, sizeof(cl_mem), &m_x[id]);
        ecode |= clSetKernelArg(k_main_3[id], 1, sizeof(cl_mem), &m_z[id]);
        ecode |= clSetKernelArg(k_main_3[id], 2, sizeof(int), &array_length);
        ecode |= clSetKernelArg(k_main_3[id], 3, sizeof(cl_mem), &d_temp1[id]);
        ecode |= clSetKernelArg(k_main_3[id], 4, sizeof(cl_mem), &d_temp2[id]);
      }
      clu_CheckError(ecode, "clSetKernelArg() for main_3");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id],
          k_main_3[id],
          1, NULL,
          global[id], local[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for main_3");
    }
    //CHECK_FINISH();
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    for (id = 0; id < nprocs; id++){
      norm_temp1[id][1] = 0.0;
      norm_temp1[id][2] = 0.0;

      ecode = clEnqueueReadBuffer(cmd_queue[id],
          d_temp1[id],
          CL_FALSE,
          0, temp_size[id] * sizeof(double),
          temp_result1[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueReadBuffer()");

      ecode = clEnqueueReadBuffer(cmd_queue[id],
          d_temp2[id],
          CL_FALSE,
          0, temp_size[id] * sizeof(double),
          temp_result2[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueReadBuffer()");
    }
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
    //CHECK_FINISH();

    for (id = 0; id < nprocs; id++){
      for (j = 0; j < temp_size[id]; j++) {

        norm_temp1[id][1] += temp_result1[id][j];
        norm_temp1[id][2] += temp_result2[id][j];
      }
    }

    for (i=1; i<=l2npcols; i++){
      if (timeron) timer_start(t_ncomm);
      //				MPI_Irecv((double*)&norm_temp2[id][1], 2, dp_type, reduce_exch_proc[id][i], i, MPI_COMM_WORLD, &request);
      //				MPI_Send((double*)&norm_temp1[id][1], 2, dp_type, reduce_exch_proc[id][i], i, MPI_COMM_WORLD);
      //				MPI_Wait(&request, &status);
      for (id = 0; id < nprocs; id++){
        memcpy(&norm_temp2[id][1], &norm_temp1[reduce_exch_proc[id][i]][1], sizeof(double) * 2);
      }

      if (timeron) timer_stop(t_ncomm);

      for (id = 0; id < nprocs; id++){
        norm_temp1[id][1] = norm_temp1[id][1] + norm_temp2[id][1];
        norm_temp1[id][2] = norm_temp1[id][2] + norm_temp2[id][2];
      }
    }

    for (id = 0; id < nprocs; id++){
      norm_temp1[id][2] = 1.0 / sqrt( norm_temp1[id][2]);

      int length = lastcol[id]-firstcol[id]+1;
      local[id][0] = LOCAL_SIZE;
      global[id][0] = next_round(length, local[id][0]);

      ecode = clSetKernelArg(k_main_4[id], 0, sizeof(cl_mem), &m_x[id]);
      ecode = clSetKernelArg(k_main_4[id], 1, sizeof(cl_mem), &m_z[id]);
      ecode = clSetKernelArg(k_main_4[id], 2, sizeof(double), &norm_temp1[id][2]);
      ecode = clSetKernelArg(k_main_4[id], 3, sizeof(int), &length);
      clu_CheckError(ecode, "clSetKernelArg() for main_4");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id],
          k_main_4[id],
          1, NULL,
          global[id], local[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for main_4");

      //Normalize z to obtain x
      //			for (j=1; j<=lastcol[id]-firstcol[id]+1; j++){
      //				x[id][j] = norm_temp1[id][2]*z[id][j];
      //			}
    }
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    //		for (id = 0; id < nprocs; id++) {
    //			int length = lastcol[id]-firstcol[id]+1;
    //			ecode = clEnqueueReadBuffer(cmd_queue[id], m_x[id], CL_TRUE, 0, (length+1) * sizeof(double), x[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //		}

  } //warming up


  //set starting vector to (1, 1, .... 1)
  //NOTE: a questionable limit on size:  should this be na/num_proc_cols+1 ?

  for (id = 0; id < nprocs; id++){
    for (id = 0; id < nprocs; id++){
      int length = NA/NUM_PROC_ROWS+1;
      local[id][0] = LOCAL_SIZE;
      global[id][0] = next_round(length, local[id][0]);

      ecode = clSetKernelArg(k_main_2[id], 0, sizeof(cl_mem), &m_x[id]);
      ecode = clSetKernelArg(k_main_2[id], 1, sizeof(int), &length);
      clu_CheckError(ecode, "clSetKernelArg() for main_2");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id],
          k_main_2[id],
          1, NULL,
          global[id], local[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for main_2");
    }
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    //		for (id = 0; id < nprocs; id++) {
    //			int length = NA/NUM_PROC_ROWS+1;
    //			ecode = clEnqueueReadBuffer(cmd_queue[id], m_x[id], CL_TRUE, 0, (length+1) * sizeof(double), x[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueReadBuffer()");
    //		}

    //		for (i=1; i<=NA/NUM_PROC_ROWS+1; i++){
    //			x[id][i] = 1.0;
    //		}
  }
  zeta  = 0.0;

  //Synchronize and start timing
  for (i=1; i<=t_last; i++){
    timer_clear(i);
  }
  //MPI_Barrier(MPI_COMM_WORLD);
  timer_clear(1);
  timer_start(1);

  //Main Iteration for inverse power method
  for (it=1; it <= NITER; it++){
    conj_grad ( colidx, rowstr, x, z, a, p, q, r, w, &rnorm,
        l2npcols,
        reduce_exch_proc,
        reduce_send_starts,
        reduce_send_lengths,
        reduce_recv_starts,
        reduce_recv_lengths);
    for (id = 0; id < nprocs; id++){

      //			norm_temp1[id][1] = 0.0;
      //			norm_temp1[id][2] = 0.0;
      //
      //			for (j=1; j<=lastcol[id]-firstcol[id]+1; j++){
      //				norm_temp1[id][1] = norm_temp1[id][1] + x[id][j]*z[id][j];
      //				norm_temp1[id][2] = norm_temp1[id][2] + z[id][j]*z[id][j];
      //			}

      int array_length = lastcol[id] - firstcol[id] + 1;
#ifdef USE_CHUNK_SCHEDULE
      global[id][0] = REDUCTION_WG;
      local[id][0] = 1;
#else
      global[id][0] = GLOBAL_REDUCE;
      local[id][0] = LOCAL_REDUCE;
#endif
      temp_size[id] = (int)(global[id][0] / local[id][0]);

      if (device_type == CL_DEVICE_TYPE_CPU) {
        ecode  = clSetKernelArg(k_main_3[id], 0, sizeof(cl_mem), &m_x[id]);
        ecode |= clSetKernelArg(k_main_3[id], 1, sizeof(cl_mem), &m_z[id]);
        ecode |= clSetKernelArg(k_main_3[id], 2, local[id][0]*2*sizeof(double), NULL);
        ecode |= clSetKernelArg(k_main_3[id], 3, local[id][0]*2*sizeof(double), NULL);
        ecode |= clSetKernelArg(k_main_3[id], 4, sizeof(int), &array_length);
        ecode |= clSetKernelArg(k_main_3[id], 5, sizeof(cl_mem), &d_temp1[id]);
        ecode |= clSetKernelArg(k_main_3[id], 6, sizeof(cl_mem), &d_temp2[id]);
      } else {
        ecode  = clSetKernelArg(k_main_3[id], 0, sizeof(cl_mem), &m_x[id]);
        ecode |= clSetKernelArg(k_main_3[id], 1, sizeof(cl_mem), &m_z[id]);
        ecode |= clSetKernelArg(k_main_3[id], 2, sizeof(int), &array_length);
        ecode |= clSetKernelArg(k_main_3[id], 3, sizeof(cl_mem), &d_temp1[id]);
        ecode |= clSetKernelArg(k_main_3[id], 4, sizeof(cl_mem), &d_temp2[id]);
      }
      clu_CheckError(ecode, "clSetKernelArg() for main_3");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id],
          k_main_3[id],
          1, NULL,
          global[id], local[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for main_3");
    }

    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    for (id = 0; id < nprocs; id++){
      norm_temp1[id][1] = 0.0;
      norm_temp1[id][2] = 0.0;
    }

// HERE UP AND DOWN
#if 1
    for (id = 0; id < nprocs; id++){
      ecode = clEnqueueReadBuffer(cmd_queue[id],
          d_temp1[id],
          CL_FALSE,
          0, temp_size[id] * sizeof(double),
          temp_result1[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueReadBuffer()");
    }

    for (id = 0; id < nprocs; id++){
      ecode = clEnqueueReadBuffer(cmd_queue[id],
          d_temp2[id],
          CL_FALSE,
          0, temp_size[id] * sizeof(double),
          temp_result2[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueReadBuffer()");
    }
#else
    for (id = 0; id < nprocs; id++){
      ecode = clEnqueueReadBuffer(cmd_queue[id],
          d_temp1[id],
          CL_TRUE,
          0, temp_size[id] * sizeof(double),
          temp_result1[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueReadBuffer()");

      ecode = clEnqueueReadBuffer(cmd_queue[id],
          d_temp2[id],
          CL_TRUE,
          0, temp_size[id] * sizeof(double),
          temp_result2[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueReadBuffer()");
    }
#endif
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    for (id = 0; id < nprocs; id++){
      for (j = 0; j < temp_size[id]; j++) {
        norm_temp1[id][1] += temp_result1[id][j];
        norm_temp1[id][2] += temp_result2[id][j];
      }
    }

    for (i=1; i<=l2npcols; i++){
      if (timeron) timer_start(t_ncomm);
      //				MPI_Irecv((double*)&norm_temp2[1], 2, dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD, &request);
      //				MPI_Send((double*)&norm_temp1[1], 2, dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD);
      //				MPI_Wait(&request, &status);
      for (id = 0; id < nprocs; id++){
        memcpy(&norm_temp2[id][1], &norm_temp1[reduce_exch_proc[id][i]][1], sizeof(double) * 2);
      }
      if (timeron) timer_stop(t_ncomm);
      for (id = 0; id < nprocs; id++){
        norm_temp1[id][1] = norm_temp1[id][1] + norm_temp2[id][1];
        norm_temp1[id][2] = norm_temp1[id][2] + norm_temp2[id][2];
      }
    }

    for (id = 0; id < nprocs; id++){
      //norm_temp1[id][2] = 1.0 / sqrt( norm_temp1[id][2]);

      if(id == 0){
        zeta = SHIFT + 1.0 / norm_temp1[id][1];
        if (it == 1)
          printf("\n   iteration           ||r||                 zeta\n");
        printf("    %5d       %20.14E%20.13f\n", it, rnorm, zeta);
      }
      //if (isnan(rnorm)) return -1;
    }

    for (id = 0; id < nprocs; id++){

      norm_temp1[id][2] = 1.0 / sqrt( norm_temp1[id][2]);

      int length = lastcol[id]-firstcol[id]+1;
      local[id][0] = LOCAL_SIZE;
      global[id][0] = next_round(length, local[id][0]);

      ecode = clSetKernelArg(k_main_4[id], 0, sizeof(cl_mem), &m_x[id]);
      ecode = clSetKernelArg(k_main_4[id], 1, sizeof(cl_mem), &m_z[id]);
      ecode = clSetKernelArg(k_main_4[id], 2, sizeof(double), &norm_temp1[id][2]);
      ecode = clSetKernelArg(k_main_4[id], 3, sizeof(int), &length);
      clu_CheckError(ecode, "clSetKernelArg() for main_4");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id],
          k_main_4[id],
          1, NULL,
          global[id], local[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for main_4");
    }
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    //		for (id = 0; id < nprocs; id++) {
    //			int length = lastcol[id]-firstcol[id]+1;
    //			ecode = clEnqueueReadBuffer(cmd_queue[id], m_x[id], CL_TRUE, 0, (length+1) * sizeof(double), x[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueReadBuffer()");
    //		}

    //  Normalize z to obtain x
    //			for (j=1; j<=lastcol[id]-firstcol[id]+1; j++){
    //				x[id][j] = norm_temp1[id][2]*z[id][j];
    //			}

  }//end of main iteration


  timer_stop(1);

  //End of timed section
  t = timer_read(1);
  //MPI_Reduce(&t, &tmax, 1, dp_type, MPI_MAX,root, MPI_COMM_WORLD);
  tmax = t;


  printf(" Benchmark completed\n");
  epsilon = 1.0e-10;
  if (Class != 'U') {
    err = fabs(zeta - zeta_verify_value) / zeta_verify_value;
    if (err <= epsilon) {
      verified = true;
      printf(" VERIFICATION SUCCESSFUL\n");
      printf(" Zeta is    %20.13E\n", zeta);
      printf(" Error is   %20.13E\n", err);
    } else {
      verified = false;
      printf(" VERIFICATION FAILED\n");
      printf(" Zeta                %20.13E\n", zeta);
      printf(" The correct zeta is %20.13E\n", zeta_verify_value);
    }
  } else {
    verified = false;
    printf(" Problem size unknown\n");
    printf(" NO VERIFICATION PERFORMED\n");
  }

  if( tmax != 0.0 ) {
    mflops = (double)( 2*NITER*NA )
      * ( 3.+(double)( NONZER*(NONZER+1) )
          + 25.*(5.+(double)( NONZER*(NONZER+1) ))
          + 3. ) / tmax / 1000000.0;
  }else{
    mflops = 0.0;
  }

  c_print_results("CG", Class, NA, 0, 0,
      NITER, tmax,
      mflops, "          floating point",
      verified, NPBVERSION, COMPILETIME, CS1, CS2, CS3, CS4, CS5, CS6, CS7,
      clu_GetDeviceTypeName(device_type),
      device_name, num_devices);



  if (timeron){
    for (i =1; i<=t_last; i++)
      t1[i] = timer_read(i);

    t1[t_conjg] = t1[t_conjg] - t1[t_rcomm];
    t1[t_last+2] = t1[t_rcomm] + t1[t_ncomm];
    t1[t_last+1] = t1[t_total] - t1[t_last+2];

    //		  MPI_Reduce(t1, tsum,  t_last+2, dp_type, MPI_SUM, 0, MPI_COMM_WORLD);
    //		  MPI_Reduce(t1, tming, t_last+2, dp_type, MPI_MIN, 0, MPI_COMM_WORLD);
    //		  MPI_Reduce(t1, tmaxg, t_last+2, dp_type, MPI_MAX, 0, MPI_COMM_WORLD);
    //
    //		  if (me == 0) {
    //			 //do i = 1, t_last+2
    for (i=1; i<=t_last+2; i++){
      printf(" timer %2d (%-9s) :  %10.4f\n", i, t_recs[i], t1[i]);
      //				tsum[i] = tsum[i] / nprocs;
      //				//write(*, 810) i, t_recs(i), tming(i), tmaxg(i), tsum(i)
    }

    double t_conjg_sum = 0.0;
    for (i = t_conjg_1; i < t_conjg_10; i++) {
      t_conjg_sum += t1[i];
    }
    printf(" conjg kernels: %10.4f\n", t_conjg_sum);
    printf(" conjg remains: %10.4f\n", t1[t_conjg] - t_conjg_sum);

    //		  }
  } //end if
  //     MPI_Finalize();

  return 0;
}//END MAIN


void conj_grad (int **colidx, int **rowstr, double **x, double **z, double **a, double **p, double **q, double **r,
    double **w, double *rnorm, int l2npcols,
    int **reduce_exch_proc,
    int **reduce_send_starts,
    int **reduce_send_lengths,
    int **reduce_recv_starts,
    int **reduce_recv_lengths )
{
  //Floaging point arrays here are named as in NPB1 spec discussion of CG algorithm
  //  integer status(MPI_STATUS_SIZE ), request
  int i, j, k, ecode;
  int cgit, cgitmax;
  int id;
  cgitmax = 25;
  if (timeron) timer_start(t_conjg);

#ifndef USE_CHUNK_SCHEDULE
  double *d, *sum, *rho, *rho0, *alpha, *beta;
  sum = (double*)malloc(nprocs * sizeof(double));
  d = (double*)malloc(nprocs * sizeof(double));
  rho = (double*)malloc(nprocs * sizeof(double));
  rho0 = (double*)malloc(nprocs * sizeof(double));
  alpha = (double*)malloc(nprocs * sizeof(double));
  beta = (double*)malloc(nprocs * sizeof(double));

  double **temp_result = (double**)malloc(nprocs * sizeof(double*));
  for (id = 0; id < nprocs; id++) {
    temp_size[id] = GLOBAL_REDUCE/LOCAL_REDUCE;
    temp_result[id] = (double*)malloc(temp_size[id]*sizeof(double));
  }
#endif

  //Initialize the CG algorithm:
  //removal 12
  //	for (id = 0; id < nprocs; id++){
  //	ecode = clEnqueueWriteBuffer(cmd_queue[id], m_x[id], CL_TRUE,0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), x[id],0, NULL, NULL);
  //	  clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //	ecode = clEnqueueWriteBuffer(cmd_queue[id], m_r[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), r[id],0, NULL, NULL);
  //	  clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //	}
  //CHECK_FINISH()

  DTIMER_START(t_conjg_1);
  for (id = 0; id < nprocs; id++){
    //	  global[id][0] = naa/nprows;
    //	  local[id][0] = LOCAL_SIZE;
#ifdef USE_CHUNK_SCHEDULE
    local[id][0] = 1;
    global[id][0] = REDUCTION_WG;
#else
    local[id][0] = LOCAL_SIZE;
    global[id][0] = next_round(naa/nprows, local[id][0]);
#endif

    int length = naa/nprows+1;
    ecode = clSetKernelArg(k_conj_grad_1[id], 6, sizeof(int), &length);
    clu_CheckError(ecode, "clSetKernelArg()");

    ecode = clEnqueueNDRangeKernel(cmd_queue[id],
        k_conj_grad_1[id],
        1, NULL,
        global[id], local[id],
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_1");
  }
  //CHECK_FINISH()
  for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
  DTIMER_STOP(t_conjg_1);

  //removal 12
  //	for (id = 0; id < nprocs; id++){
  //		ecode = clEnqueueReadBuffer(cmd_queue[id], m_p[id], CL_TRUE,0, (naa/nprows+1+1) * sizeof(double), p[id],0, NULL, NULL);
  //		clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //		ecode = clEnqueueReadBuffer(cmd_queue[id], m_r[id], CL_TRUE, 0, (naa/nprows+1+1) * sizeof(double), r[id],0, NULL, NULL);
  //		clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //		ecode = clEnqueueReadBuffer(cmd_queue[id], m_q[id], CL_TRUE, 0, (naa/nprows+1+1) * sizeof(double), q[id],0, NULL, NULL);
  //		clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //		ecode = clEnqueueReadBuffer(cmd_queue[id], m_x[id], CL_TRUE, 0, (naa/nprows+1+1) * sizeof(double), x[id],0, NULL, NULL);
  //		clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //		ecode = clEnqueueReadBuffer(cmd_queue[id], m_z[id], CL_TRUE, 0, (naa/nprows+1+1) * sizeof(double), z[id],0, NULL, NULL);
  //		clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //		ecode = clEnqueueReadBuffer(cmd_queue[id], m_w[id], CL_TRUE, 0, (naa/nprows+1+1) * sizeof(double), w[id],0, NULL, NULL);
  //		clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //
  //	}
  //CHECK_FINISH()

  //	for (id = 0; id < nprocs; id++){
  //		for (j=1; j<=naa/nprows+1; j++){
  //			q[id][j] = 0.0;
  //			z[id][j] = 0.0;
  //			r[id][j] = x[id][j];
  //			p[id][j] = r[id][j];
  //			w[id][j] = 0.0;
  //		}
  //	}

  //rho = r.r
  //Now, obtain the norm of r: First, sum squares of r elements locally...

  //	for (id = 0; id < nprocs; id++){
  //		ecode = clEnqueueWriteBuffer(cmd_queue[id], m_r[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), r[id],0, NULL, NULL);
  //		clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //	}
  //CHECK_FINISH()


  DTIMER_START(t_conjg_2);
  for (id = 0; id < nprocs; id++){
    int length = lastcol[id] - firstcol[id] + 1;
#ifdef USE_CHUNK_SCHEDULE
    global[id][0] = REDUCTION_WG;
    local[id][0] = 1;
#else
    global[id][0] = GLOBAL_REDUCE;
    local[id][0] = LOCAL_REDUCE;
#endif
    temp_size[id] = (int)(global[id][0] / local[id][0]);
    ecode  = clSetKernelArg(k_conj_grad_2[id], 0, sizeof(cl_mem), &m_r[id]);
    ecode |= clSetKernelArg(k_conj_grad_2[id], 1, local[id][0]*2*sizeof(double), NULL);
    ecode |= clSetKernelArg(k_conj_grad_2[id], 2, sizeof(int), &length);
    ecode |= clSetKernelArg(k_conj_grad_2[id], 3, sizeof(cl_mem), &d_rho[id]);
    clu_CheckError(ecode, "clSetKernelArg() for conj_grad_2");
    ecode = clEnqueueNDRangeKernel(cmd_queue[id], k_conj_grad_2[id], 1, NULL, global[id], local[id], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_2");
  }
  //CHECK_FINISH()
  for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

  for (id = 0; id < nprocs; id++){
    ecode = clEnqueueReadBuffer(cmd_queue[id], d_rho[id], CL_FALSE, 0, temp_size[id] * sizeof(double), temp_result[id], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueReadBuffer()");
  }
  for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
  //CHECK_FINISH()

  for (id = 0; id < nprocs; id++){
    sum[id] = 0.0;
    for (j = 0; j < temp_size[id]; j++) sum[id] = sum[id] + temp_result[id][j];
  }
  DTIMER_STOP(t_conjg_2);

  //	for (id = 0; id < nprocs; id++){
  //		sum[id] = 0.0;
  //		for (j=1; j<=lastcol[id]-firstcol[id]+1; j++){
  //			sum[id] = sum[id] + r[id][j]*r[id][j];
  //		}
  //	}

  //Exchange and sum with procs identified in reduce_exch_proc
  //(This is equivalent to mpi_allreduce.)
  //Sum the partial sums of rho, leaving rho on all processors


  //do i = 1, l2npcols
  for (i=1; i<=l2npcols; i++){
    if (timeron) timer_start(t_rcomm);
    //         MPI_Irecv(&rho, 1, dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD, &request);
    //         MPI_Send(&sum, 1, dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD);
    //         MPI_Wait(&request, &status);
    for (id = 0; id < nprocs; id++){
      memcpy(&rho[id], &sum[reduce_exch_proc[id][i]], sizeof(double) * 1);
    }

    if (timeron) timer_stop(t_rcomm);
    for (id = 0; id < nprocs; id++){
      sum[id] = sum[id] + rho[id];
    }

  }
  for (id = 0; id < nprocs; id++)
    rho[id] = sum[id];


  //The conj grad iteration loop
  //do cgit = 1, cgitmax
  for (cgit=1; cgit <= cgitmax; cgit++){
    //q = A.p
    //The partition submatrix-vector multiply: use workspace w

    //removal 5
    //    	  for (id = 0; id < nprocs; id++){
    ////				ecode = clEnqueueWriteBuffer(cmd_queue[id], m_p[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), p[id],0, NULL, NULL);
    ////				clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    ////				ecode = clEnqueueWriteBuffer(cmd_queue[id], m_w[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), w[id],0, NULL, NULL);
    ////				clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //				ecode = clEnqueueWriteBuffer(cmd_queue[id], m_a[id], CL_TRUE, 0, (NZ+1) * sizeof(double), a[id],0, NULL, NULL);
    //				clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //				ecode = clEnqueueWriteBuffer(cmd_queue[id], m_rowstr[id], CL_TRUE, 0, (NA+1+1) * sizeof(int), rowstr[id],0, NULL, NULL);
    //				clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //				ecode = clEnqueueWriteBuffer(cmd_queue[id], m_colidx[id], CL_TRUE, 0, (NZ+1) * sizeof(int), colidx[id],0, NULL, NULL);
    //				clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //		  }
    //CHECK_FINISH()

    DTIMER_START(t_conjg_3);
    for (id = 0; id < nprocs; id++){
      //				global[id][0] = (lastrow[id] - firstrow[id] + 1);
      //				local[id][0] = LOCAL_SIZE;
      int length = lastrow[id] - firstrow[id] + 1;
#ifdef USE_CHUNK_SCHEDULE
      local[id][0] = 1;
      global[id][0] = CHUNK_WG;
#else
      local[id][0] = LOCAL_SIZE;
      global[id][0] = next_round(length, local[id][0]);
#endif

//      printf("id=%d length=%d local=%lu global=%lu wg=%lu\n",
//          id, length, local[id][0], global[id][0], global[id][0]/local[id][0]);

      ecode  = clSetKernelArg(k_conj_grad_3[id], 0, sizeof(cl_mem), &m_rowstr[id]);
      ecode |= clSetKernelArg(k_conj_grad_3[id], 1, sizeof(cl_mem), &m_a[id]);
      ecode |= clSetKernelArg(k_conj_grad_3[id], 2, sizeof(cl_mem), &m_p[id]);
      ecode |= clSetKernelArg(k_conj_grad_3[id], 3, sizeof(cl_mem), &m_colidx[id]);
      ecode |= clSetKernelArg(k_conj_grad_3[id], 4, sizeof(cl_mem), &m_w[id]);
      ecode |= clSetKernelArg(k_conj_grad_3[id], 5, sizeof(int), &length);

      ecode = clEnqueueNDRangeKernel(cmd_queue[id],
          k_conj_grad_3[id],
          1, NULL,
          global[id], local[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_3");
    }
    //CHECK_FINISH()
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
    DTIMER_STOP(t_conjg_3);
//    exit(-1);

    //removal 6
    //		  for (id = 0; id < nprocs; id++){
    //				ecode = clEnqueueReadBuffer(cmd_queue[id], m_w[id], CL_TRUE, 0, (lastrow[id]-firstrow[id]+1+1) * sizeof(double), w[id],0, NULL, NULL);
    //				clu_CheckError(ecode, "clEnqueueReadBuffer()");
    //		  }
    //CHECK_FINISH()


    //    	  for (id = 0; id < nprocs; id++){
    //			for (j=1; j<=lastrow[id]-firstrow[id]+1; j++){
    //				sum[id] = 0.0;
    //				for (k=rowstr[id][j]; k<=rowstr[id][j+1]-1; k++){
    //					sum[id] = sum[id] + a[id][k]*p[id][colidx[id][k]];
    //				}
    //				w[id][j] = sum[id];
    //			}
    //    	  }

    //Sum the partition submatrix-vec A.p's across rows
    //Exchange and sum piece of w with procs identified in reduce_exch_proc

    for (i=l2npcols; i>=1; i--){
      if (timeron) timer_start(t_rcomm);
      //			MPI_Irecv((double*)&q[reduce_recv_starts[i]], reduce_recv_lengths[i], dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD, &request);
      //			MPI_Send((double*)&w[reduce_send_starts[i]], reduce_send_lengths[i], dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD);
      //			MPI_Wait(&request, &status);

      for (id = 0; id < nprocs; id++){
        //				memcpy(&q[id][reduce_recv_starts[id][i]], &w[reduce_exch_proc[id][i]][reduce_send_starts[reduce_exch_proc[id][i]][i]],
        //					sizeof(double) * reduce_recv_lengths[id][i]);
        //
        //				ecode = clEnqueueWriteBuffer(cmd_queue[id], m_q[id], CL_TRUE, reduce_recv_starts[id][i]*sizeof(double),
        //						reduce_recv_lengths[id][i] * sizeof(double), &q[id][reduce_recv_starts[id][i]],0, NULL, NULL);
        //				clu_CheckError(ecode, "clEnqueueWriteBuffer()");

        ecode = clEnqueueCopyBuffer(cmd_queue[id],
            m_w[reduce_exch_proc[id][i]],
            m_q[id],
            reduce_send_starts[reduce_exch_proc[id][i]][i] * sizeof(double),
            reduce_recv_starts[id][i] * sizeof(double),
            sizeof(double) * reduce_recv_lengths[id][i],
            0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueCopyBuffer()");
      }
      for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

      if (timeron) timer_stop(t_rcomm);

      DTIMER_START(t_conjg_10);
      for (id = 0; id < nprocs; id++){

        int length = send_start[id] + reduce_recv_lengths[id][i] - 1;
        local[id][0] = LOCAL_SIZE;
        global[id][0] = next_round(length - send_start[id], local[id][0]);

        ecode  = clSetKernelArg(k_conj_grad_10[id], 0, sizeof(cl_mem), &m_w[id]);
        ecode  |= clSetKernelArg(k_conj_grad_10[id], 1, sizeof(cl_mem), &m_q[id]);
        ecode  |= clSetKernelArg(k_conj_grad_10[id], 2, sizeof(int), &send_start[id]);
        ecode  |= clSetKernelArg(k_conj_grad_10[id], 3, sizeof(int), &length);

        ecode = clEnqueueNDRangeKernel(cmd_queue[id],
            k_conj_grad_10[id],
            1, NULL,
            global[id], local[id],
            0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_10");

        //				for (j=send_start[id]; j<=send_start[id] + reduce_recv_lengths[id][i] - 1; j++){
        //					w[id][j] = w[id][j] + q[id][j];
        //				}
      }

      for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
      DTIMER_STOP(t_conjg_10);

      //removal 7
      //			for (id = 0; id < nprocs; id++){
      //				int length = reduce_recv_lengths[id][i];
      //				ecode = clEnqueueReadBuffer(cmd_queue[id], m_w[id], CL_TRUE, (send_start[id])*sizeof(double),
      //						length * sizeof(double), &w[id][send_start[id]],0, NULL, NULL);
      //				clu_CheckError(ecode, "clEnqueueReadBuffer()");
      //			}
    }


    //Exchange piece of q with transpose processor:

    if (timeron) timer_start(t_rcomm);
    if(l2npcols != 0 ) {
      //            MPI_Irecv((double*)&q[1], exch_recv_length, dp_type, exch_proc, 1, MPI_COMM_WORLD, &request);
      //            MPI_Send((double*)&w[send_start], send_len, dp_type, exch_proc, 1, MPI_COMM_WORLD);
      //            MPI_Wait(&request, &status);

      for (id = 0; id < nprocs; id++){
        //				memcpy(&q[id][1], &w[exch_proc[id]][send_start[exch_proc[id]]],
        //					sizeof(double) * exch_recv_length[id]);
        ecode = clEnqueueCopyBuffer(cmd_queue[id],
            m_w[exch_proc[id]],
            m_q[id],
            send_start[exch_proc[id]] * sizeof(double),
            1 * sizeof(double),
            sizeof(double) * exch_recv_length[id],
            0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueCopyBuffer()");

        //removal 8
        //				ecode = clEnqueueReadBuffer(cmd_queue[id], m_q[id], CL_TRUE, 1*sizeof(double),
        //						exch_recv_length[id] * sizeof(double), &q[id][1],0, NULL, NULL);
        //				clu_CheckError(ecode, "clEnqueueReadBuffer()");
      }
      for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
    }
    else {
      //do j=1,exch_recv_length
      for (id = 0; id < nprocs; id++){
        //				for (j=1; j<= exch_recv_length[id]; j++){
        //				   q[id][j] = w[id][j];
        //				}
        ecode = clEnqueueCopyBuffer(cmd_queue[id],
            m_w[id],
            m_q[id],
            1 * sizeof(double),
            1 * sizeof(double),
            sizeof(double) * exch_recv_length[id],
            0, NULL, NULL);
        clu_CheckError(ecode, "clEnqueueCopyBuffer()");

        //removal 8
        //				ecode = clEnqueueReadBuffer(cmd_queue[id], m_q[id], CL_TRUE, 1*sizeof(double),
        //						exch_recv_length[id] * sizeof(double), &q[id][1],0, NULL, NULL);
        //				clu_CheckError(ecode, "clEnqueueReadBuffer()");
      }
      for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
    }
    if (timeron) timer_stop(t_rcomm);

    //         if (me == 0) for (j=1; j<=10; j++) printf("d sum  : %20.13E \n", q[j]);
    //         MPI_Finalize(); exit(0);


    //Clear w for reuse...
    DTIMER_START(t_conjg_9);
    for (id = 0; id < nprocs; id++){
      int length = max( lastrow[id]-firstrow[id]+1, lastcol[id]-firstcol[id]+1);
#ifdef USE_CHUNK_SCHEDULE
      local[id][0] = 1;
      global[id][0] = REDUCTION_WG;
#else
      local[id][0] = LOCAL_SIZE;
      global[id][0] = next_round(length, local[id][0]);
#endif
      ecode  = clSetKernelArg(k_conj_grad_9[id], 0, sizeof(cl_mem), &m_w[id]);
      ecode  |= clSetKernelArg(k_conj_grad_9[id], 1, sizeof(int), &length);
      clu_CheckError(ecode, "clSetKernelArg() for conj_grad_9");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id],
          k_conj_grad_9[id],
          1, NULL,
          global[id], local[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_9");
    }

    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
    DTIMER_STOP(t_conjg_9);

    //removal 9
    //         for (id = 0; id < nprocs; id++){
    //        	 int length = max( lastrow[id]-firstrow[id]+1, lastcol[id]-firstcol[id]+1 );
    //			 ecode = clEnqueueReadBuffer(cmd_queue[id], m_w[id], CL_TRUE, 0, (length+1) * sizeof(double), w[id],0, NULL, NULL);
    //			 clu_CheckError(ecode, "clEnqueueReadBuffer()");
    //		 }


    //         for (id = 0; id < nprocs; id++){
    //			 for (j=1; j<= max( lastrow[id]-firstrow[id]+1, lastcol[id]-firstcol[id]+1 ); j++){
    //				w[id][j] = 0.0;
    //			 }
    //         }

    //Obtain p.q


    //double **temp_result = (double**)malloc(nprocs * sizeof(double*));

    //removal 1
    //     	for (id = 0; id < nprocs; id++){
    //     		ecode = clEnqueueWriteBuffer(cmd_queue[id], m_p[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), p[id],0, NULL, NULL);
    //     		clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //     		ecode = clEnqueueWriteBuffer(cmd_queue[id], m_q[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), q[id],0, NULL, NULL);
    //     		clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //     	}
    //CHECK_FINISH()


    DTIMER_START(t_conjg_4);
    for (id = 0; id < nprocs; id++){

      int length = lastcol[id] - firstcol[id] + 1;
#ifdef USE_CHUNK_SCHEDULE
      global[id][0] = REDUCTION_WG;
      local[id][0] = 1;
      temp_size[id] = global[id][0] / local[id][0];
#else
      global[id][0] = GLOBAL_REDUCE;
      local[id][0] = LOCAL_REDUCE;

      temp_size[id] = (int)(global[id][0] / local[id][0]);
      //temp_result[id] = (double*)malloc(temp_size[id]*sizeof(double));
#endif

      ecode  = clSetKernelArg(k_conj_grad_4[id], 0, sizeof(cl_mem), &m_p[id]);
      ecode |= clSetKernelArg(k_conj_grad_4[id], 1, sizeof(cl_mem), &m_q[id]);
      ecode |= clSetKernelArg(k_conj_grad_4[id], 2, local[id][0]*2*sizeof(double), NULL);
      ecode |= clSetKernelArg(k_conj_grad_4[id], 3, sizeof(int), &length);
      ecode |= clSetKernelArg(k_conj_grad_4[id], 4, sizeof(cl_mem), &d_rho[id]);
      clu_CheckError(ecode, "clSetKernelArg() for conj_grad_4");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id], k_conj_grad_4[id], 1, NULL, global[id], local[id], 0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_4");
    }
    //CHECK_FINISH()
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    for (id = 0; id < nprocs; id++){
      ecode = clEnqueueReadBuffer(cmd_queue[id], d_rho[id], CL_FALSE, 0, temp_size[id] * sizeof(double), temp_result[id], 0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueReadBuffer()");
    }
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
    DTIMER_STOP(t_conjg_4);
    //CHECK_FINISH()

    for (id = 0; id < nprocs; id++){
      sum[id] = 0.0;
      for (j = 0; j < temp_size[id]; j++) sum[id] = sum[id] + temp_result[id][j];
    }

    //         for (id = 0; id < nprocs; id++){
    //			 sum[id] = 0.0;
    //			 for (j=1; j<= lastcol[id]-firstcol[id]+1; j++){
    //				sum[id] = sum[id] + p[id][j]*q[id][j];
    //			 }
    //         }

    //Obtain d with a sum-reduce

    for (i=1; i<=l2npcols; i++){
      if (timeron) timer_start(t_rcomm);
      //            MPI_Irecv(&d, 1, dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD, &request);
      //            MPI_Send(&sum, 1, dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD);
      //            MPI_Wait(&request, &status);
      for (id = 0; id < nprocs; id++){
        memcpy(&d[id], &sum[reduce_exch_proc[id][i]], sizeof(double) * 1);
      }
      if (timeron) timer_stop(t_rcomm);
      for (id = 0; id < nprocs; id++){
        sum[id] = sum[id] + d[id];
      }
    }

    for (id = 0; id < nprocs; id++){
      d[id] = sum[id];
    }

    //Obtain alpha = rho / (p.q)
    for (id = 0; id < nprocs; id++){
      alpha[id] = rho[id] / d[id];
    }

    //Save a temporary of rho
    for (id = 0; id < nprocs; id++){
      rho0[id] = rho[id];
    }

    //Obtain z = z + alpha*p
    //and    r = r - alpha*q

    //removal 2
    //		for (id = 0; id < nprocs; id++){
    //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_p[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), p[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_q[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), q[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_z[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), z[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_r[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), r[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //		}

    DTIMER_START(t_conjg_5);
    for (id = 0; id < nprocs; id++){
      int length = lastcol[id]-firstcol[id]+1;

#ifdef USE_CHUNK_SCHEDULE
      local[id][0] = 1;
      global[id][0] = REDUCTION_WG;
#else
      local[id][0] = LOCAL_SIZE;
      global[id][0] = next_round(length, local[id][0]);
#endif

      ecode  = clSetKernelArg(k_conj_grad_5[id], 0, sizeof(cl_mem), &m_p[id]);
      ecode |= clSetKernelArg(k_conj_grad_5[id], 1, sizeof(cl_mem), &m_q[id]);
      ecode |= clSetKernelArg(k_conj_grad_5[id], 2, sizeof(cl_mem), &m_r[id]);
      ecode |= clSetKernelArg(k_conj_grad_5[id], 3, sizeof(cl_mem), &m_z[id]);
      ecode |= clSetKernelArg(k_conj_grad_5[id], 4, sizeof(double), &alpha[id]);
      ecode |= clSetKernelArg(k_conj_grad_5[id], 5, sizeof(int), &length);

      clu_CheckError(ecode, "clSetKernelArg() for conj_grad_5");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id], k_conj_grad_5[id], 1, NULL, global[id], local[id], 0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_5");
    }
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
    DTIMER_STOP(t_conjg_5);

    //removal 12
    //		for (id = 0; id < nprocs; id++){
    //			int length = lastcol[id]-firstcol[id]+1;
    //			ecode = clEnqueueReadBuffer(cmd_queue[id], m_r[id], CL_TRUE, 0, (length+1) * sizeof(double), r[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueReadBuffer()");
    //			ecode = clEnqueueReadBuffer(cmd_queue[id], m_z[id], CL_TRUE, 0, (length+1) * sizeof(double), z[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueReadBuffer()");
    //		}

    //         for (id = 0; id < nprocs; id++){
    //			 for (j=1; j<=lastcol[id]-firstcol[id]+1; j++){
    //				z[id][j] = z[id][j] + alpha[id]*p[id][j];
    //				r[id][j] = r[id][j] - alpha[id]*q[id][j];
    //			 }
    //         }

    //rho = r.r
    //Now, obtain the norm of r: First, sum squares of r elements locally...


    //double **temp_result = (double**)malloc(nprocs * sizeof(double*));

    //removal 3
    //		for (id = 0; id < nprocs; id++){
    //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_r[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), r[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //		}
    //CHECK_FINISH()

    DTIMER_START(t_conjg_2);
    for (id = 0; id < nprocs; id++){
      int length = lastcol[id]-firstcol[id]+1;
#ifdef USE_CHUNK_SCHEDULE
      global[id][0] = REDUCTION_WG;
      local[id][0] = 1;
#else
      global[id][0] = GLOBAL_REDUCE;
      local[id][0] = LOCAL_REDUCE;
#endif

      temp_size[id] = (int)(global[id][0] / local[id][0]);
      //temp_result[id] = (double*)malloc(temp_size[id]*sizeof(double));

      ecode  = clSetKernelArg(k_conj_grad_2[id], 0, sizeof(cl_mem), &m_r[id]);
      ecode |= clSetKernelArg(k_conj_grad_2[id], 1, local[id][0]*2*sizeof(double), NULL);
      ecode |= clSetKernelArg(k_conj_grad_2[id], 2, sizeof(int), &length);
      ecode |= clSetKernelArg(k_conj_grad_2[id], 3, sizeof(cl_mem), &d_rho[id]);
      clu_CheckError(ecode, "clSetKernelArg() for conj_grad_2");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id], k_conj_grad_2[id], 1, NULL, global[id], local[id], 0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_2");
    }
    //CHECK_FINISH()
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    for (id = 0; id < nprocs; id++){
      ecode = clEnqueueReadBuffer(cmd_queue[id], d_rho[id], CL_FALSE, 0, temp_size[id] * sizeof(double), temp_result[id], 0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueReadBuffer()");
    }
    //CHECK_FINISH()
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    for (id = 0; id < nprocs; id++){
      sum[id] = 0.0;
      for (j = 0; j < temp_size[id]; j++) sum[id] = sum[id] + temp_result[id][j];
    }
    DTIMER_STOP(t_conjg_2);

    //         for (id = 0; id < nprocs; id++){
    //			 sum[id] = 0.0;
    //			 for (j=1; j<=lastcol[id]-firstcol[id]+1; j++){
    //				sum[id] = sum[id] + r[id][j]*r[id][j];
    //			 }
    //         }


    //Obtain rho with a sum-reduce
    if (timeron) timer_start(t_rcomm);
    for (i=1; i<=l2npcols; i++){
      //           MPI_Irecv(&rho, 1, dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD, &request);
      //           MPI_Send(&sum, 1, dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD);
      //           MPI_Wait(&request, &status);
      for (id = 0; id < nprocs; id++){
        memcpy(&rho[id], &sum[reduce_exch_proc[id][i]], sizeof(double) * 1);
      }
      for (id = 0; id < nprocs; id++){
        sum[id] = sum[id] + rho[id];
      }
    }
    if (timeron) timer_stop(t_rcomm);
    for (id = 0; id < nprocs; id++){
      rho[id] = sum[id];
    }

    //Obtain beta:

    for (id = 0; id < nprocs; id++){
      beta[id] = rho[id] / rho0[id];
    }

    //p = r + beta*p

    //removal 4
    //		for (id = 0; id < nprocs; id++){
    //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_p[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), p[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_r[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), r[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
    //		}
    //CHECK_FINISH()

    DTIMER_START(t_conjg_6);
    for (id = 0; id < nprocs; id++){
      int length = lastcol[id]-firstcol[id]+1;
#ifdef USE_CHUNK_SCHEDULE
      local[id][0] = 1;
      global[id][0] = REDUCTION_WG;
#else
      local[id][0] = LOCAL_SIZE;
      global[id][0] = next_round(length, local[id][0]);
#endif

      ecode  = clSetKernelArg(k_conj_grad_6[id], 0, sizeof(cl_mem), &m_p[id]);
      ecode |= clSetKernelArg(k_conj_grad_6[id], 1, sizeof(cl_mem), &m_r[id]);
      ecode |= clSetKernelArg(k_conj_grad_6[id], 2, sizeof(double), &beta[id]);
      ecode |= clSetKernelArg(k_conj_grad_6[id], 3, sizeof(int), &length);
      clu_CheckError(ecode, "clSetKernelArg() for conj_grad_6");

      ecode = clEnqueueNDRangeKernel(cmd_queue[id], k_conj_grad_6[id], 1, NULL, global[id], local[id], 0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_6");
    }
    //CHECK_FINISH()
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
    DTIMER_STOP(t_conjg_6);

    //removal 10
    //		for (id = 0; id < nprocs; id++){
    //			ecode = clEnqueueReadBuffer(cmd_queue[id], m_p[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), p[id],0, NULL, NULL);
    //			clu_CheckError(ecode, "clEnqueueReadBuffer()");
    //		}


    //         for (id = 0; id < nprocs; id++){
    //			 for (j=1; j<=lastcol[id]-firstcol[id]+1; j++){
    //				p[id][j] = r[id][j] + beta[id]*p[id][j];
    //			 }
    //         }

  }//endif enddo                             ! end of do cgit=1,cgitmax


  //Compute residual norm explicitly:  ||r|| = ||x - A.z||
  //First, form A.z
  //The partition submatrix-vector multiply


  //removal 11
  //      for (id = 0; id < nprocs; id++){
  //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_z[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), z[id],0, NULL, NULL);
  //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_w[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), w[id],0, NULL, NULL);
  //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_a[id], CL_TRUE, 0, (NZ+1) * sizeof(double), a[id],0, NULL, NULL);
  //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_rowstr[id], CL_TRUE, 0, (NA+1+1) * sizeof(int), rowstr[id],0, NULL, NULL);
  //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_colidx[id], CL_TRUE, 0, (NZ+1) * sizeof(int), colidx[id],0, NULL, NULL);
  //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //	  }
  //CHECK_FINISH()

  DTIMER_START(t_conjg_7);
  for (id = 0; id < nprocs; id++){
    //				global[id][0] = (lastrow[id] - firstrow[id] + 1);
    //				local[id][0] = LOCAL_SIZE;
    int length = lastrow[id] - firstrow[id] + 1;
#ifdef USE_CHUNK_SCHEDULE
    local[id][0] = 1;
    global[id][0] = CHUNK_WG;
#else
    local[id][0] = LOCAL_SIZE;
    global[id][0] = next_round(length, local[id][0]);
#endif

    ecode  = clSetKernelArg(k_conj_grad_7[id], 0, sizeof(cl_mem), &m_rowstr[id]);
    ecode  |= clSetKernelArg(k_conj_grad_7[id], 1, sizeof(cl_mem), &m_a[id]);
    ecode  |= clSetKernelArg(k_conj_grad_7[id], 2, sizeof(cl_mem), &m_z[id]);
    ecode  |= clSetKernelArg(k_conj_grad_7[id], 3, sizeof(cl_mem), &m_colidx[id]);
    ecode  |= clSetKernelArg(k_conj_grad_7[id], 4, sizeof(cl_mem), &m_w[id]);
    ecode  |= clSetKernelArg(k_conj_grad_7[id], 5, sizeof(int), &length);

    ecode = clEnqueueNDRangeKernel(cmd_queue[id],
        k_conj_grad_7[id],
        1, NULL,
        global[id], local[id],
        0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_7");
  }
  //CHECK_FINISH()
  for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
  DTIMER_STOP(t_conjg_7);

  //	  for (id = 0; id < nprocs; id++){
  //		    int length = lastrow[id] - firstrow[id] + 1;
  //			ecode = clEnqueueReadBuffer(cmd_queue[id], m_w[id], CL_TRUE, 0, (length+1) * sizeof(double), w[id],0, NULL, NULL);
  //			clu_CheckError(ecode, "clEnqueueReadBuffer()");
  //	  }

  //      for (id = 0; id < nprocs; id++){
  //		  for (j=1; j<=lastrow[id]-firstrow[id]+1; j++){
  //			 sum[id] = 0.0;
  //			 for (k=rowstr[id][j]; k<=rowstr[id][j+1]-1; k++){
  //				sum[id] = sum[id] + a[id][k]*z[id][colidx[id][k]];
  //			 }
  //			 w[id][j] = sum[id];
  //		  }
  //      }

  //Sum the partition submatrix-vec A.z's across rows

  //do i = l2npcols, 1, -1
  for (i=l2npcols; i>=1; i--){
    if (timeron) timer_start(t_rcomm);
    //         MPI_Irecv((double*)&r[reduce_recv_starts[i]], reduce_recv_lengths[i], dp_type, reduce_exch_proc[i],
    //						 i, MPI_COMM_WORLD, &request);
    //         MPI_Send((double*)&w[reduce_send_starts[i]], reduce_send_lengths[i], dp_type, reduce_exch_proc[i],
    //						 i, MPI_COMM_WORLD);
    //         MPI_Wait(&request, &status);
    for (id = 0; id < nprocs; id++){
      //			memcpy(&r[id][reduce_recv_starts[id][i]],
      //					&w[reduce_exch_proc[id][i]][reduce_send_starts[reduce_exch_proc[id][i]][i]],
      //					sizeof(double) * reduce_recv_lengths[id][i]);
      ecode = clEnqueueCopyBuffer(cmd_queue[id],
          m_w[reduce_exch_proc[id][i]],
          m_r[id],
          reduce_send_starts[reduce_exch_proc[id][i]][i] * sizeof(double),
          reduce_recv_starts[id][i] * sizeof(double),
          sizeof(double) * reduce_recv_lengths[id][i],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueCopyBuffer()");

      //			ecode = clEnqueueReadBuffer(cmd_queue[id], m_r[id], CL_TRUE, reduce_recv_starts[id][i]*sizeof(double),
      //					reduce_recv_lengths[id][i] * sizeof(double), &r[id][reduce_recv_starts[id][i]],0, NULL, NULL);
      //			clu_CheckError(ecode, "clEnqueueReadBuffer()");
    }
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
    if (timeron) timer_stop(t_rcomm);

    //do j=send_start,send_start + reduce_recv_lengths(i) - 1
    DTIMER_START(t_conjg_10);
    for (id = 0; id < nprocs; id++){
      int length = send_start[id] + reduce_recv_lengths[id][i] - 1;
      local[id][0] = LOCAL_SIZE;
      global[id][0] = next_round(length - send_start[id], local[id][0]);

      ecode  = clSetKernelArg(k_conj_grad_10[id], 0, sizeof(cl_mem), &m_w[id]);
      ecode  |= clSetKernelArg(k_conj_grad_10[id], 1, sizeof(cl_mem), &m_r[id]);
      ecode  |= clSetKernelArg(k_conj_grad_10[id], 2, sizeof(int), &send_start[id]);
      ecode  |= clSetKernelArg(k_conj_grad_10[id], 3, sizeof(int), &length);

      ecode = clEnqueueNDRangeKernel(cmd_queue[id],
          k_conj_grad_10[id],
          1, NULL,
          global[id], local[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_10");


      //			 for (j=send_start[id]; j<=send_start[id] + reduce_recv_lengths[id][i] - 1; j++){
      //				w[id][j] = w[id][j] + r[id][j];
      //			 }
    }

    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
    DTIMER_STOP(t_conjg_10);

    //         for (id = 0; id < nprocs; id++){
    //			 ecode = clEnqueueReadBuffer(cmd_queue[id], m_w[id], CL_TRUE, send_start[id]*sizeof(double),
    //					 reduce_recv_lengths[id][i] * sizeof(double), &w[id][send_start[id]],0, NULL, NULL);
    //			 clu_CheckError(ecode, "clEnqueueReadBuffer()");
    //         }
  }

  //Exchange piece of q with transpose processor:
  if( l2npcols != 0 ) {
    if (timeron) timer_start(t_rcomm);
    //         MPI_Irecv((double*)&r[1], exch_recv_length, dp_type, exch_proc, 1, MPI_COMM_WORLD, &request);
    //         MPI_Send((double*)&w[send_start], send_len, dp_type, exch_proc, 1, MPI_COMM_WORLD);
    //         MPI_Wait(&request, &status);

    //         for (id = 0; id < nprocs; id++){
    //			memcpy(&r[id][1], &w[exch_proc[id]][send_start[exch_proc[id]]],
    //					sizeof(double) * exch_recv_length[id]);
    //		 }

    for (id = 0; id < nprocs; id++){
      ecode = clEnqueueCopyBuffer(cmd_queue[id],
          m_w[exch_proc[id]],
          m_r[id],
          send_start[exch_proc[id]] * sizeof(double),
          1 * sizeof(double),
          sizeof(double) * exch_recv_length[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueCopyBuffer()");
    }

    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

    //         for (id = 0; id < nprocs; id++){
    //        	 ecode = clEnqueueReadBuffer(cmd_queue[id], m_r[id], CL_TRUE, 1*sizeof(double),
    //        			 exch_recv_length[id] * sizeof(double), &r[id][1],0, NULL, NULL);
    //			 clu_CheckError(ecode, "clEnqueueReadBuffer()");
    //         }


    if (timeron) timer_stop(t_rcomm);
  }
  else {
    //do j=1,exch_recv_length
    for (id = 0; id < nprocs; id++){
      //			 for (j=1; j<=exch_recv_length[id]; j++)
      //				r[id][j] = w[id][j];

      ecode = clEnqueueCopyBuffer(cmd_queue[id],
          m_w[id],
          m_r[id],
          1 * sizeof(double),
          1 * sizeof(double),
          sizeof(double) * exch_recv_length[id],
          0, NULL, NULL);
      clu_CheckError(ecode, "clEnqueueCopyBuffer()");
    }
    for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
    //    	  for (id = 0; id < nprocs; id++){
    //			 ecode = clEnqueueReadBuffer(cmd_queue[id], m_r[id], CL_TRUE, 1*sizeof(double),
    //					 exch_recv_length[id] * sizeof(double), &r[id][1],0, NULL, NULL);
    //			 clu_CheckError(ecode, "clEnqueueReadBuffer()");
    //		   }
  }

  //At this point, r contains A.z

  //removal 12
  //      for (id = 0; id < nprocs; id++){
  //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_x[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), x[id],0, NULL, NULL);
  //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //			ecode = clEnqueueWriteBuffer(cmd_queue[id], m_r[id], CL_TRUE, 0, (NA/NUM_PROC_ROWS+2+1) * sizeof(double), r[id],0, NULL, NULL);
  //			clu_CheckError(ecode, "clEnqueueWriteBuffer()");
  //	  }
  //CHECK_FINISH()

  DTIMER_START(t_conjg_8);
  for (id = 0; id < nprocs; id++){
    int length = lastcol[id]-firstcol[id]+1;
#ifdef USE_CHUNK_SCHEDULE
    global[id][0] = REDUCTION_WG;
    local[id][0] = 1;
#else
    global[id][0] = GLOBAL_REDUCE;
    local[id][0] = LOCAL_REDUCE;
#endif

    temp_size[id] = (int)(global[id][0] / local[id][0]);
    //temp_result[id] = (double*)malloc(temp_size[id]*sizeof(double));

    ecode  = clSetKernelArg(k_conj_grad_8[id], 0, sizeof(cl_mem), &m_x[id]);
    ecode  = clSetKernelArg(k_conj_grad_8[id], 1, sizeof(cl_mem), &m_r[id]);
    ecode |= clSetKernelArg(k_conj_grad_8[id], 2, local[id][0]*2*sizeof(double), NULL);
    ecode |= clSetKernelArg(k_conj_grad_8[id], 3, sizeof(int), &length);
    ecode |= clSetKernelArg(k_conj_grad_8[id], 4, sizeof(cl_mem), &d_rho[id]);
    clu_CheckError(ecode, "clSetKernelArg() for conj_grad_8");

    ecode = clEnqueueNDRangeKernel(cmd_queue[id], k_conj_grad_8[id], 1, NULL, global[id], local[id], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueNDRangeKernel() for conj_grad_8");
  }
  //CHECK_FINISH()
  for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);

  for (id = 0; id < nprocs; id++){
    ecode = clEnqueueReadBuffer(cmd_queue[id], d_rho[id], CL_FALSE, 0, temp_size[id] * sizeof(double), temp_result[id], 0, NULL, NULL);
    clu_CheckError(ecode, "clEnqueueReadBuffer()");
  }
  for (id = 0; id < nprocs; id++) clFinish(cmd_queue[id]);
  //CHECK_FINISH()

  for (id = 0; id < nprocs; id++){
    sum[id] = 0.0;
    for (j = 0; j < temp_size[id]; j++) sum[id] = sum[id] + temp_result[id][j];
  }
  DTIMER_STOP(t_conjg_8);

  //      for (id = 0; id < nprocs; id++){
  //         sum[id] = 0.0;
  //         for (j=1; j<=lastcol[id]-firstcol[id]+1; j++){
  //            d[id] = x[id][j] - r[id][j];
  //            sum[id] = sum[id] + d[id]*d[id];
  //         }
  //      }

  //Obtain d with a sum-reduce
  //do i = 1, l2npcols
  for (i=1; i<=l2npcols; i++){
    if (timeron) timer_start(t_rcomm);
    //         MPI_Irecv(&d, 1, dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD, &request);
    //         MPI_Send(&sum, 1, dp_type, reduce_exch_proc[i], i, MPI_COMM_WORLD);
    //         MPI_Wait(&request, &status);
    for (id = 0; id < nprocs; id++){
      memcpy(&d[id], &sum[reduce_exch_proc[id][i]], sizeof(double) * 1);
    }
    if (timeron) timer_stop(t_rcomm);
    for (id = 0; id < nprocs; id++){
      sum[id] = sum[id] + d[id];
    }
  }
  for (id = 0; id < nprocs; id++){
    d[id] = sum[id];
    //printf("me : %d; sum : %f\n", id, d[id]);
  }

  //exit(0);

  //if( me == root )
  *rnorm = sqrt( d[0] );

  if (timeron) timer_stop(t_conjg);
}


void sparse(double *a, int *colidx, int *rowstr, int n, int *arow, int *acol, double *aelt,
				int firstrow, int lastrow,
				double *x, logical *mark, int *nzloc, int nnza )
{
//rows range from firstrow to lastrow
//the rowstr pointers are defined for nrows = lastrow-firstrow+1 values

      int nrows;
//generate a sparse matrix from a list of
//[col, row, element] tri

      int i, j, jajp1, nza, k, nzrow;
      double xi;

      nrows = lastrow - firstrow + 1;

      for (j=1; j<=n; j++){
         rowstr[j] = 0;
         mark[j] = false;
      }
      rowstr[n+1] = 0;

      for (nza=1; nza<=nnza; nza++){
         j = (arow[nza] - firstrow + 1) + 1;
         rowstr[j] = rowstr[j] + 1;
      }

      rowstr[1] = 1;
      for (j=2; j<=nrows+1; j++)
         rowstr[j] = rowstr[j] + rowstr[j-1];

//do a bucket sort of the triples on the row index

      for (nza=1; nza<=nnza; nza++){
         j = arow[nza] - firstrow + 1;
         k = rowstr[j];
         a[k] = aelt[nza];
         colidx[k] = acol[nza];
         rowstr[j] = rowstr[j] + 1;
      }

//rowstr(j) now points to the first element of row j+1

      //do j = nrows, 1, -1
      for (j=nrows; j>=1; j--){
          rowstr[j+1] = rowstr[j];
      }
      rowstr[1] = 1;

//generate the actual output rows by adding elements

      nza = 0;
      for (i=1; i<=n; i++){
          x[i]    = 0.0;
          mark[i] = false;
      }

      jajp1 = rowstr[1];
      //do j = 1, nrows
      for (j=1; j<=nrows; j++) {
         nzrow = 0;

//...loop over the jth row of a
         for (k=jajp1; k<=rowstr[j+1]-1; k++){
            i = colidx[k];
            x[i] = x[i] + a[k];
            if ((!mark[i]) && (x[i] != 0.0)) {
             mark[i] = true;
             nzrow = nzrow + 1;
             nzloc[nzrow] = i;
            }
         }

//extract the nonzeros of this row
         for (k=1; k <= nzrow; k++){
            i = nzloc[k];
            mark[i] = false;
            xi = x[i];
            x[i] = 0.0;
            if (xi != 0.0) {
             nza = nza + 1;
             a[nza] = xi;
             colidx[nza] = i;
            }
         }
         jajp1 = rowstr[j+1];
         rowstr[j+1] = nza + rowstr[1];
      } //end for
}

/*
 *
	generate a sparse n-vector (v, iv)
	having nzv nonzeros

	mark(i) is set to 1 if position i is nonzero.
	mark is all zero on entry and is reset to all zero before exit
	this corrects a performance bug found by John G. Lewis, caused by
	reinitialization of mark on every one of the n calls to sprnvc
 *
 */

int icnvrt(double x, int ipwr2)
{
	return (int)(ipwr2 * x);
}

void sprnvc(int n, int nz, double *v, int* iv, int* nzloc, int* mark, int id)
{
    int nn1;
    int	nzrow, nzv, ii, i;
    double vecelt, vecloc;

    nzv = 0;
    nzrow = 0;
    nn1 = 1;

    while (nn1 < n){
    	nn1 = 2*nn1;
    }

	while (nzv < nz){
		vecelt = randlc(&tran[id], amult);
		vecloc = randlc(&tran[id], amult);

		i = icnvrt(vecloc, nn1) + 1;
		if (i > n) continue;
		if (mark[i] == 0) {
			mark[i] = 1;
			nzrow = nzrow + 1;
			nzloc[nzrow] = i;
			nzv = nzv + 1;
			v[nzv] = vecelt;
			iv[nzv] = i;
		}
	}//end while
	//printf("tran : %d : %d\n", id, nzrow); exit(0);
	for (ii=1; ii<=nzrow; ii++){
		i = nzloc[ii];
		mark[i] = 0;
	}
}

void vecset(int n, double *v, int *iv, int *nzv, int i, double val)
{
      logical set;
      set = false;
      int k;
      for (k=1; k<=*nzv; k++){
         if (iv[k] == i) {
            v[k] = val;
            set  = true;
         }
      }
      if (!set) {
         *nzv     = *nzv + 1;
         v[*nzv]  = val;
         iv[*nzv] = i;
      }
}
