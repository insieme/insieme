/*
Copyright (c) 2007, Lawrence Livermore National Security (LLNS), LLC
Produced at the Lawrence Livermore National Laboratory (LLNL)
Written by Adam Moody <moody20@llnl.gov>.
UCRL-CODE-232117.
All rights reserved.

This file is part of mpiGraph. For details, see
  http://www.sourceforge.net/projects/mpigraph
Please also read the Additional BSD Notice below.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
â* Redistributions of source code must retain the above copyright notice, this
   list of conditions and the disclaimer below.
â* Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the disclaimer (as noted below) in the documentation
   and/or other materials provided with the distribution.
â* Neither the name of the LLNS/LLNL nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific prior
   written permission.
â* 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL LLNS LLC, LLNL, THE U.S. DEPARTMENT
OF ENERGY OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Additional BSD Notice
1. This notice is required to be provided under our contract with the U.S. Department
   of Energy (DOE). This work was produced at LLNS, LLC, LLNL
   under Contract No. W-7405-ENG-48 with the DOE.
2. Neither the United States Government nor LLNS, LLC, LLNL nor any of
   their employees, makes any warranty, express or implied, or assumes any liability
   or responsibility for the accuracy, completeness, or usefulness of any information,
   apparatus, product, or process disclosed, or represents that its use would not
   infringe privately-owned rights.
3. Also, reference herein to any specific commercial products, process, or services
   by trade name, trademark, manufacturer or otherwise does not necessarily constitute
   or imply its endorsement, recommendation, or favoring by the United States Government
   or LLNS, LLC, LLNL. The views and opinions of authors expressed herein
   do not necessarily state or reflect those of the United States Government or LLNS, LLC,
   LLNL and shall not be used for advertising or product endorsement purposes.
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <signal.h>
#include <string.h>
#include <sys/time.h>
#include <mpi.h>
#include <math.h>

/*
#ifndef _AIX
#include "print_mpi_resources.h"
#endif
*/

/*
------------------------------------------------------
Globals
------------------------------------------------------
*/
#define KILO (1024)
#define MEGA (KILO*KILO)
#define GIGA (KILO*MEGA)

#define ITRS_EST       (5)        /* Number of iterations used to estimate time */
#define ITRS_RUN       (1000)     /* Number of iterations to run (without timelimits) */
#define MSG_SIZE_START (0)        /* Lower bound of message sizes in bytes */
#define MSG_SIZE_STOP  (512*KILO) /* Upper bound of message sizes in bytes */
#define MAX_PROC_MEM   (1*GIGA)   /* Limit on MPI buffer sizes in bytes */

/* Compile with -DNO_BARRIER to drop barriers between collective calls
     Adds barrier between test iterations to sync all procs before issuing next collective
     Prevents non-root MPI ranks from escaping ahead into future iterations
     Barrier overhead is not subtracted from timing results
*/
#ifdef NO_BARRIER
  #define __BAR__(comm)
#else
  #define __BAR__(comm) MPI_Barrier(comm)
#endif

/* we use a bit mask to flag which collectives to test */
#define BARRIER    (0x001)
#define BCAST      (0x002)
#define ALLTOALL   (0x004)
#define ALLGATHER  (0x008)
#define GATHER     (0x010)
#define SCATTER    (0x020)
#define ALLREDUCE  (0x040)
#define REDUCE     (0x080)
#define ALLTOALLV  (0x100)
#define ALLGATHERV (0x200)
#define GATHERV    (0x400)
#define NUM_TESTS  (11)

char* TEST_NAMES[] = {
  "Barrier", "Bcast", "Alltoall", "Allgather", "Gather", "Scatter", "Allreduce", "Reduce", "Alltoallv", "Allgatherv", "Gatherv"
};
int   TEST_FLAGS[] = {
   BARRIER,   BCAST,   ALLTOALL,   ALLGATHER,   GATHER,   SCATTER,   ALLREDUCE,   REDUCE,   ALLTOALLV,   ALLGATHERV,   GATHERV
};
  
int rank_local; /* my MPI rank */
int rank_count; /* number of ranks in job */
int dimid_key;
size_t allocated_memory = 0; /* number of bytes allocated */

/*
------------------------------------------------------
Utility Functions
------------------------------------------------------
*/

/* Print usage syntax and exit */
int usage()
{
    if (rank_local == 0) {
        printf("\n");
        printf("  Usage:  mpiBench [options] [operations]\n");
        printf("\n");
        printf("  Options:\n");
        printf("    -b <byte>  Beginning message size in bytes (default 0)\n");
        printf("    -e <byte>  Ending message size in bytes (default 1K)\n");
        printf("    -m <byte>  Process memory buffer limit (send+recv) in bytes (default 1G)\n");
        printf("    -i <itrs>  Maximum number of iterations for a single test (default 1000)\n");
        printf("    -t <usec>  Time limit for any single test in microseconds (default 0 = infinity)\n");
        printf("    -d <ndim>  Number of Cartesian dimensions to split processes in (default 0 = MPI_COMM_WORLD only)\n");
        printf("    -p <size>  Minimum partition size (number of ranks) to divide MPI_COMM_WORLD by\n");
        printf("    -c         Check receive buffer for expected data (default disabled)\n");
        printf("    -h         Print this help screen and exit\n");
        printf("    where <byte> = [0-9]+[KMG], e.g., 32K or 64M\n");
        printf("\n");
        printf("  Operations:\n");
        printf("    Barrier\n");
        printf("    Bcast\n");
        printf("    Alltoall, Alltoallv\n");
        printf("    Allgather, Allgatherv\n");
        printf("    Gather, Gatherv\n");
        printf("    Scatter\n");
        printf("    Allreduce\n");
        printf("    Reduce\n");
        printf("\n");
    }
    exit(1);
}

/* Allocate size bytes and keep track of amount allocated */
void* _ALLOC_MAIN_ (size_t size, char* debug) 
{
    void* p_buf;
    p_buf = malloc(size);
    if (!p_buf) {
        printf("ERROR:  Allocating memory %s:  requesting %ld bytes\n", debug, size);
        exit(1);
    }
    memset(p_buf, 0, size);
    allocated_memory += size;
    return p_buf;
}

/* Processes byte strings in the following format:
     <+ve_float_num>[kKmMgG][bB]
   and returns number of bytes as an size_t
   returns 0 on error
*/
size_t atobytes(char* str)
{
    char* next;
    size_t units = 1;

    double num = strtod(str, &next);
    if (num == 0.0 && next == str) return 0;
    if (*next != 0) {
        /* process units for kilo, mega, or gigabytes */
        switch(*next) {
            case 'k':
            case 'K':
                units = (size_t) KILO;
                break;
            case 'm':
            case 'M':
                units = (size_t) MEGA;
                break;
            case 'g':
            case 'G':
                units = (size_t) GIGA;
                break;
            default:
                printf("ERROR:  unexpected byte string %s\n", str);
                exit(1);
        }
        next++;
        if (*next == 'b' || *next == 'B') { next++; } /* handle optional b or B character, e.g. in 10KB */
        if (*next != 0) {
            printf("ERROR:  unexpected byte string: %s\n", str);
            exit(1);
        }
    }
    if (num < 0) { printf("ERROR:  byte string must be positive: %s\n", str);  exit(1); }
    return (size_t) (num * (double) units);
}

/*
------------------------------------------------------
TIMING CODE - start/stop the timer and measure the difference
------------------------------------------------------
*/

#ifdef USE_GETTIMEOFDAY /* use gettimeofday() for timers */

#include <sys/time.h>
#define __TIME_START__    (gettimeofday(&g_timeval__start, &g_timezone))
#define __TIME_END__      (gettimeofday(&g_timeval__end  , &g_timezone))
#define __TIME_USECS__    (d_Time_Diff_Micros(g_timeval__start, g_timeval__end))
#define d_Time_Diff_Micros(timeval__start, timeval__end) \
  ( \
    (double) (  (timeval__end.tv_sec  - timeval__start.tv_sec ) * 1000000 \
              + (timeval__end.tv_usec - timeval__start.tv_usec)  ) \
  )
#define d_Time_Micros(timeval) \
  ( \
    (double) (  timeval.tv_sec * 1000000 \
              + timeval.tv_usec  ) \
  )
struct timeval  g_timeval__start, g_timeval__end;
struct timezone g_timezone;

#else /* use MPI_Wtime for timers (recommened) */
/*
   on some systems gettimeofday may be occasionally reset backwards by some global clock,
   which leads to bad data including negative time periods
*/

#define __TIME_START__    (g_timeval__start    = MPI_Wtime())
#define __TIME_END__      (g_timeval__end      = MPI_Wtime())
#define __TIME_USECS__    ((g_timeval__end - g_timeval__start) * 1000000.0)
double g_timeval__start, g_timeval__end;

#endif /* of USE_GETTIMEOFDAY */

/* Gather value from each task and print statistics */
double Print_Timings(double value, char* title, size_t bytes, int iters, MPI_Comm comm, double limit)
{
    int i;
    double min, max, avg, dev;
    double* times = NULL;

    if(rank_local == 0) {
        times = (double*) malloc(sizeof(double) * rank_count);
    }

    /* gather single time value from each task to rank 0 */
    MPI_Gather(&value, 1, MPI_DOUBLE, times, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);

    /* rank 0 computes the min, max, and average over the set */
    if(rank_local == 0) {
        avg = 0;
        dev = 0;
        min = 100000000;
        max = -1;
        for(i = 0; i < rank_count; i++) {
            if(times[i] < min) { min = times[i]; }
            if(times[i] > max) { max = times[i]; }
            avg += times[i];
            dev += times[i] * times[i];
        }
        avg /= (double) rank_count;
        dev = 0; /*sqrt((dev / (double) rank_count - avg * avg)); */

        /* determine who we are in this communicator */
        int nranks, flag;
        char* str;
        MPI_Attr_get(comm, dimid_key, (void*) &str, &flag); 
        MPI_Comm_size(comm, &nranks);

        printf("%-25.25s\t", title);
        printf("Bytes:\t%8u\tIters:\t%7d\t", bytes, iters);
        printf("Avg:\t%8.4f\tMin:\t%8.4f\tMax:\t%8.4f\t", avg, min, max);
        printf("Limit:\t%8.4f\t", limit);
        printf("Comm: %s\tRanks: %d", str, nranks);
        printf("\n");
        fflush(stdout);

        free((void*) times);
    }

    /* broadcast the average value back out */
    MPI_Bcast(&avg, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);

    return avg;
}

/*
------------------------------------------------------
MAIN
------------------------------------------------------
*/

char *sbuffer;
char *rbuffer;
int  *sendcounts, *sdispls, *recvcounts, *rdispls;
size_t buffer_size = 0;
int check_buffers;

struct argList {
    int    iters;
    size_t messStart;
    size_t messStop;
    size_t memLimit;
    double timeLimit;
    int    testFlags;
    int    checkBuffers;
    int    ndims;
    int    partSize;
};

int processArgs(int argc, char **argv, struct argList* args)
{
  int i, j;
  char *argptr;
  char flag;

  /* set to default values */
  args->iters         = ITRS_RUN;
  args->messStart     = (size_t) MSG_SIZE_START;
  args->messStop      = (size_t) MSG_SIZE_STOP;
  args->memLimit      = (size_t) MAX_PROC_MEM;
  args->timeLimit     = 0;
  args->testFlags     = 0x1FFF;
  args->checkBuffers  = 0;
  args->ndims         = 3;
  args->partSize      = 0;

  for (i=0; i<argc; i++)
  {
    /* check for options */
    if (argv[i][0] == '-')
    {
      /* flag is the first char following the '-' */
      flag   = argv[i][1];
      argptr = NULL;

      /* single argument parameters */
      if (strchr("c", flag))
      {
        switch(flag)
        {
        case 'c':
          args->checkBuffers = 1;
          break;
        }
        continue;
      }
      
      /* check that we've got a valid option */
      if (!strchr("beithmdp", flag))
      {
        printf("\nInvalid flag -%c\n", flag);
        return(0);
      }
      
      /* handles "-i#" or "-i #" */
      if (argv[i][2] != 0) {
        argptr = &(argv[i][2]);
      } else {
        argptr = argv[i+1];
        i++;
      }

      switch(flag)
      {
      case 'b':
        args->messStart = atobytes(argptr);
        break;
      case 'e':
        args->messStop = atobytes(argptr);
        break;
      case 'i':
        args->iters = atoi(argptr);
        break;
      case 'm':
	args->memLimit = atobytes(argptr);
        break;
      case 't':
        args->timeLimit = (double) atol(argptr);
        break;
      case 'd':
        args->ndims = atoi(argptr);
        break;
      case 'p':
        args->partSize = atoi(argptr);
        break;
      default:
        return(0);
      }
    }
    
    /* turn on test flags requested by user
       if user doesn't specify any, all will be run */
    for(j=0; j<NUM_TESTS; j++) {
      if(!strcmp(TEST_NAMES[j], argv[i])) {
        if(args->testFlags == 0x1FFF) args->testFlags = 0;
        args->testFlags |= TEST_FLAGS[j];
      }
    }
  }

  if (args->iters == 0)
  {
    printf("\n  Must define number of operations per measurement!\n\n");
    return(0);
  }

  return(1);
}

/* fill the buffer with a pattern */
void init_buffer(char* buffer, int rank)
{
    size_t i;
    char value;
    for(i=0; i<buffer_size; i++) {
        value = (char) ((i+1)*(rank+1) + i);
        buffer[i] = value;
    }
}

/* check the send buffer for any deviation from expected pattern */
void check_sbuffer(int rank)
{
    size_t i;
    char value;
    for(i=0; i<buffer_size; i++) {
        value = (char) ((i+1)*(rank+1) + i);
        if (sbuffer[i] != value) {
            printf("Send buffer corruption detected on rank %d at sbuffer[%d]\n", rank, i);
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }
}

/* check the receive buffer for any deviation from expected pattern */
void check_rbuffer(char* buffer, size_t byte_offset, int rank, size_t src_byte_offset, size_t element_count)
{
    size_t i, j;
    char value;
    buffer += byte_offset;
    for(i=0, j=src_byte_offset; i<element_count; i++, j++) {
        value = (char) ((j+1)*(rank+1) + j);
        if (buffer[i] != value) {
              printf("Receive buffer corruption detected on rank %d at rbuffer[%d] from rank %d\n", rank_local, byte_offset+i, rank);
              MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }
}

struct collParams {
    size_t   size;     /* message (element) size in bytes */
    int      iter;     /* number of iterations to test with */
    int      root;     /* root of collective operation */
    MPI_Comm comm;     /* communicator to test collective on */
    int      myrank;   /* my rank in the above communicator */
    int      nranks;   /* number of ranks in the above communicator */
    int      count;    /* element count for collective */
    MPI_Datatype type; /* MPI_Datatype to be used in collective (assumed contiguous) */
    MPI_Op   reduceop; /* MPI_Reduce operation to be used */
};

double time_barrier(struct collParams* p)
{
    int i;
    MPI_Barrier(MPI_COMM_WORLD);

    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Barrier(p->comm);
    }
    __TIME_END__;

    return __TIME_USECS__ / (double)p->iter;
}

double time_bcast(struct collParams* p)
{
    int i;
    char* buffer = (p->myrank == p->root) ? sbuffer : rbuffer;
    MPI_Barrier(MPI_COMM_WORLD);

    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Bcast(buffer, p->size, p->type, p->root, p->comm);
        __BAR__(p->comm);
    }
    __TIME_END__;

    if (check_buffers) {
        check_sbuffer(p->myrank);
        check_rbuffer(buffer, 0, p->root, 0, p->size);
    }

    return __TIME_USECS__ / (double)p->iter;
}

double time_alltoall(struct collParams* p)
{
    int i;
    MPI_Barrier(MPI_COMM_WORLD);

    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Alltoall(sbuffer, p->size, p->type, rbuffer, p->size, p->type, p->comm);
        __BAR__(p->comm);
    }
    __TIME_END__;

    if (check_buffers) {
        check_sbuffer(p->myrank);
        for (i = 0; i < p->nranks; i++) {
            check_rbuffer(rbuffer, i*p->size, i, p->myrank*p->size, p->size);
        }
    }

    return __TIME_USECS__ / (double)p->iter;
}

double time_alltoallv(struct collParams* p)
{
    int i, j, size2;
    int disp = 0;
    for ( i = 0; i < p->nranks; i++) {
        int size2 = (i+p->myrank) % (p->size+1);
        sendcounts[i] = size2;
        recvcounts[i] = size2;
        sdispls[i] = disp;
        rdispls[i] = disp;
        disp += size2;
    }
    MPI_Barrier(MPI_COMM_WORLD);

    size2 = p->myrank % (p->size+1);
    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Alltoallv(sbuffer, sendcounts, sdispls, p->type, rbuffer, recvcounts, rdispls, p->type, p->comm);
        __BAR__(p->comm);
    }
    __TIME_END__;

    if (check_buffers) {
        check_sbuffer(p->myrank);
        for (i = 0; i < p->nranks; i++) {
            disp = 0;
            for (j = 0; j < p->myrank; j++) { disp += (j+i) % (p->size+1); }
            check_rbuffer(rbuffer, rdispls[i], i, disp, recvcounts[i]);
        }
    }

    return __TIME_USECS__ / (double)p->iter;
}

double time_allgather(struct collParams* p)
{
    int i;
    MPI_Barrier(MPI_COMM_WORLD);

    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Allgather(sbuffer, p->size, p->type, rbuffer, p->size, p->type, p->comm);
        __BAR__(p->comm);
    }
    __TIME_END__;

    if (check_buffers) {
        check_sbuffer(p->myrank);
        for (i = 0; i < p->nranks; i++) {
            check_rbuffer(rbuffer, i*p->size, i, 0, p->size);
        }
    }

    return __TIME_USECS__ / (double)p->iter;
}

double time_allgatherv(struct collParams* p)
{
    int i, size2;
    int disp = 0;
    for ( i = 0; i < p->nranks; i++) {
        int size2 = i % (p->size+1);
        recvcounts[i] = size2;
        rdispls[i] = disp;
        disp += size2;
    }
    MPI_Barrier(MPI_COMM_WORLD);

    size2 = p->myrank % (p->size+1);
    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Allgatherv(sbuffer, size2, p->type, rbuffer, recvcounts, rdispls, p->type, p->comm);
        __BAR__(p->comm);
    }
    __TIME_END__;

    if (check_buffers) {
        check_sbuffer(p->myrank);
        for (i = 0; i < p->nranks; i++) {
            check_rbuffer(rbuffer, rdispls[i], i, 0, recvcounts[i]);
        }
    }

    return __TIME_USECS__ / (double)p->iter;
}

double time_gather(struct collParams* p)
{
    int i;
    MPI_Barrier(MPI_COMM_WORLD);

    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Gather(sbuffer, p->size, p->type, rbuffer, p->size, p->type, p->root, p->comm);
        __BAR__(p->comm);
    }
    __TIME_END__;

    if (check_buffers) {
        check_sbuffer(p->myrank);
        if (p->myrank == p->root) {
            for (i = 0; i < p->nranks; i++) {
                check_rbuffer(rbuffer, i*p->size, i, 0, p->size);
            }
        }
    }

    return __TIME_USECS__ / (double)p->iter;
}

double time_gatherv(struct collParams* p)
{
    int i, size2;
    int disp = 0;
    for ( i = 0; i < p->nranks; i++) {
        int size2 = i % (p->size+1);
        recvcounts[i] = size2;
        rdispls[i] = disp;
        disp += size2;
    }
    MPI_Barrier(MPI_COMM_WORLD);

    size2 = p->myrank % (p->size+1);
    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Gatherv(sbuffer, size2, p->type, rbuffer, recvcounts, rdispls, p->type, p->root, p->comm);
        __BAR__(p->comm);
    }
    __TIME_END__;

    if (check_buffers) {
        check_sbuffer(p->myrank);
        if (p->myrank == p->root) {
            for (i = 0; i < p->nranks; i++) {
                check_rbuffer(rbuffer, rdispls[i], i, 0, recvcounts[i]);
            }
        }
    }

    return __TIME_USECS__ / (double)p->iter;
}

double time_scatter(struct collParams* p)
{
    int i;
    MPI_Barrier(MPI_COMM_WORLD);

    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Scatter(sbuffer, p->size, p->type, rbuffer, p->size, p->type, p->root, p->comm);
        __BAR__(p->comm);
    }
    __TIME_END__;

    if (check_buffers) {
        check_sbuffer(p->myrank);
        check_rbuffer(rbuffer, 0, p->root, p->myrank*p->size, p->size);
    }

    return __TIME_USECS__ / (double)p->iter;
}

double time_allreduce(struct collParams* p)
{
    int i;
    MPI_Barrier(MPI_COMM_WORLD);

    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Allreduce(sbuffer, rbuffer, p->count, p->type, p->reduceop, p->comm);
    }
    __TIME_END__;

    return __TIME_USECS__ / (double)p->iter;
}

double time_reduce(struct collParams* p)
{
    int i;
    MPI_Barrier(MPI_COMM_WORLD);

    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Reduce(sbuffer, rbuffer, p->count, p->type, p->reduceop, p->root, p->comm);
        __BAR__(p->comm);
    }
    __TIME_END__;

    return __TIME_USECS__ / (double)p->iter;
}

/* Prime, estimate, and time the collective called by the specified function
   for the given message size, iteration count, and time limit.  Then, print
   out the results.
*/
double get_time(double (*fn)(struct collParams* p), char* title, struct collParams* p, int iter, int time_limit)
{
    double time;
    double time_avg;
    int iter_limit;

    /* initialize buffers with known value */
    if (check_buffers) {
        init_buffer(sbuffer, p->myrank);
        memset(rbuffer, 0, buffer_size);
        check_sbuffer(p->myrank);
    }

    /* prime the collective with an intial call */
    p->iter = 1;
    time = fn(p);

    /* run through a small number of iterations to get a rough estimate of time */
    p->iter = ITRS_EST;
    time = fn(p);

    /* if a time limit has been specified, use the esitmate to limit the maximum number of iterations */
    iter_limit = (time_limit > 0   ) ? (int) (time_limit / time) : iter;
    iter_limit = (iter_limit < iter) ? iter_limit : iter;

    /* use the number calculated by the root (rank 0) which should be the slowest */
    MPI_Bcast(&iter_limit, 1, MPI_INT, 0, MPI_COMM_WORLD);

    /* run the tests (unless the limited iteration count is smaller than that used in the estimate) */
    if(iter_limit > ITRS_EST) {
        p->iter = iter_limit;
        time = fn(p);
    } else {
        iter_limit = ITRS_EST;
    }

    /* Collect and print the timing results recorded by each process */
    Print_Timings(time, title, p->size, iter_limit, p->comm, 4.0+0.001*p->count);

    return time;
}

/* Prime, estimate, and time the collective called by the specified function
   for the given message size, iteration count, and time limit.  Then, print
   out the results.
*/
double get_time3d(double (*fn)(struct collParams* p), char* title, struct collParams* p, int iter, int time_limit)
{
    double time;
    double time_avg;
    int iter_limit;

    /* initialize buffers with known value */
    if (check_buffers) {
        init_buffer(sbuffer, p->myrank);
        memset(rbuffer, 0, buffer_size);
        check_sbuffer(p->myrank);
    }

    int current_comm = 0;
    MPI_Comm comms[3];
    char comm_desc[3 * 256];

    /* if ndims is specified, map MPI_COMM_WORLD into ndims Cartesian space, and create 1-D communicators along each dimension */
    int d;
    int ndims = 3;
    if (ndims > 0) {
        MPI_Comm comm_dims;
        int* dims    = (int*) malloc(sizeof(int) * ndims);
        int* periods = (int*) malloc(sizeof(int) * ndims);
        for (d=0; d < ndims; d++) {
            dims[d]    = 0; /* set dims[d]=non-zero if you want to explicitly specify the number of processes in this dimension */
            periods[d] = 0; /* set period[d]=1 if you want this dimension to be periodic */ 
        }

        /*
        given the total number of processes, and the number of dimensions,
        fill in dims with the number of processes along each dimension (split as evenly as possible)
        */ 
        MPI_Dims_create(p->nranks, ndims, dims);

        /*
        then create a cartesian communicator,
        which we'll use to split into ndims 1-D communicators along each dimension
        */  
        MPI_Cart_create(p->comm, ndims, dims, periods, 0, &comm_dims);

        /*
        split cartesian communicator into ndims plane subcommunicators along each dimension
        (for MPI_Cart_sub, set dims[d]=1 if you want that dimension to remain in subcommunicator)
        */ 
        int d2;
        for (d=0; d < ndims; d++) {
            for (d2=0; d2 < ndims; d2++) {
                if (d == d2) dims[d2] = 0;
                else         dims[d2] = 1;
            }
            MPI_Cart_sub(comm_dims, dims, &comms[current_comm]);
            sprintf(&comm_desc[256*current_comm], "CartPlane-%dof%d", d+1, ndims);
            current_comm++;
        }

        free(dims);
        free(periods);
    }

    int i;

    /* prime the collective with an intial call */
    p->iter = 1;
    MPI_Barrier(MPI_COMM_WORLD);
    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Allreduce(sbuffer, rbuffer, p->count, p->type, p->reduceop, comms[0]);
        MPI_Allreduce(sbuffer, rbuffer, p->count, p->type, p->reduceop, comms[1]);
        MPI_Allreduce(sbuffer, rbuffer, p->count, p->type, p->reduceop, comms[2]);
    }
    __TIME_END__;
    time = __TIME_USECS__ / (double)(3*p->iter);

    /* run through a small number of iterations to get a rough estimate of time */
    p->iter = ITRS_EST;
    MPI_Barrier(MPI_COMM_WORLD);
    __TIME_START__;
    for (i = 0; i < p->iter; i++) {
        MPI_Allreduce(sbuffer, rbuffer, p->count, p->type, p->reduceop, comms[0]);
        MPI_Allreduce(sbuffer, rbuffer, p->count, p->type, p->reduceop, comms[1]);
        MPI_Allreduce(sbuffer, rbuffer, p->count, p->type, p->reduceop, comms[2]);
    }
    __TIME_END__;
    time = __TIME_USECS__ / (double)(3*p->iter);

    /* if a time limit has been specified, use the esitmate to limit the maximum number of iterations */
    iter_limit = (time_limit > 0   ) ? (int) (time_limit / time) : iter;
    iter_limit = (iter_limit < iter) ? iter_limit : iter;

    /* use the number calculated by the root (rank 0) which should be the slowest */
    MPI_Bcast(&iter_limit, 1, MPI_INT, 0, MPI_COMM_WORLD);

    /* run the tests (unless the limited iteration count is smaller than that used in the estimate) */
    if(iter_limit > ITRS_EST) {
        p->iter = iter_limit;
        MPI_Barrier(MPI_COMM_WORLD);
        __TIME_START__;
        for (i = 0; i < p->iter; i++) {
            MPI_Allreduce(sbuffer, rbuffer, p->count, p->type, p->reduceop, comms[0]);
            MPI_Allreduce(sbuffer, rbuffer, p->count, p->type, p->reduceop, comms[1]);
            MPI_Allreduce(sbuffer, rbuffer, p->count, p->type, p->reduceop, comms[2]);
        }
        __TIME_END__;
        time = __TIME_USECS__ / (double)(3*p->iter);
    } else {
        iter_limit = ITRS_EST;
    }

    /* Collect and print the timing results recorded by each process */
    Print_Timings(time, title, p->size, iter_limit, p->comm, 4.0+0.001*p->count);

    return time;
}

int main (int argc, char *argv[])
{
    int err;
    double time, time_limit, time_maxMsg;

    int iter, iter_limit;
    size_t size, messStart, messStop, mem_limit;
    int testFlags, ndims, partsize;
    int k;

    char  hostname[256];
    char* hostnames;

    int root = 0;

    struct argList args;
    /* process the command-line arguments, printing usage info on error */
    if (!processArgs(argc, argv, &args)) { usage(); }
    iter       = args.iters;
    messStart  = args.messStart;
    messStop   = args.messStop;
    mem_limit  = args.memLimit;
    time_limit = args.timeLimit;
    testFlags  = args.testFlags;
    check_buffers = args.checkBuffers;
    ndims      = args.ndims;
    partsize   = args.partSize; 

    /* initialize MPI */
    err = MPI_Init(&argc, &argv);
    if (err) { printf("Error in MPI_Init\n"); exit(1); }

    /* determine who we are in the MPI world */
    MPI_Comm_rank(MPI_COMM_WORLD, &rank_local);
    MPI_Comm_size(MPI_COMM_WORLD, &rank_count);

#ifdef PRINT_ENV
   /* Print environment as part of Sequoia SOW MPI requirements */
   extern void printEnv(void);
   if (rank_local == 0) { printEnv(); }
#endif 

    /* mark start of mpiBench output */
    if (rank_local == 0) { printf("START mpiBench_Allreduce v%s\n", VERS); }

    /* collect hostnames of all the processes and print rank layout */
    gethostname(hostname, sizeof(hostname));
    hostnames = (char*) _ALLOC_MAIN_(sizeof(hostname)*rank_count, "Hostname array");
    MPI_Gather(hostname, sizeof(hostname), MPI_CHAR, hostnames, sizeof(hostname), MPI_CHAR, 0, MPI_COMM_WORLD);
    if (rank_local == 0) {
        for(k=0; k<rank_count; k++) {
            printf("%d : %s\n", k, &hostnames[k*sizeof(hostname)]);
        }
    }

    /* allocate message buffers and initailize timing functions */
    while(messStop*((size_t)rank_count)*2 > mem_limit && messStop > 0) messStop /= 2;
    buffer_size = messStop * rank_count;
    sbuffer   = (char*) _ALLOC_MAIN_(messStop    * rank_count, "Send Buffer");
    rbuffer   = (char*) _ALLOC_MAIN_(messStop    * rank_count, "Receive Buffer");
    sendcounts = (int*) _ALLOC_MAIN_(sizeof(int) * rank_count, "Send Counts");
    sdispls    = (int*) _ALLOC_MAIN_(sizeof(int) * rank_count, "Send Displacements");
    recvcounts = (int*) _ALLOC_MAIN_(sizeof(int) * rank_count, "Recv Counts");
    rdispls    = (int*) _ALLOC_MAIN_(sizeof(int) * rank_count, "Recv Displacements");

    /*time_maxMsg = 2*time_limit; */
    time_maxMsg = 0.0;

    /* if partsize was specified, calculate the number of partions we need */
    int partitions = 0;
    if (partsize > 0) {
        /* keep dividing comm in half until we get to partsize */
        int currentsize = rank_count;
        while (currentsize >= partsize) {
            partitions++;
            currentsize >>= 1;
        }
    }

    /* set up communicators */
    int total_comms = 1+partitions;
    int current_comm = 0;
    int extra_state;
    MPI_Keyval_create(MPI_NULL_COPY_FN, MPI_NULL_DELETE_FN, &dimid_key, (void*) &extra_state);
    MPI_Comm* comms = (MPI_Comm*) _ALLOC_MAIN_(sizeof(MPI_Comm) * total_comms, "Communicator array");
    char* comm_desc = (char*)     _ALLOC_MAIN_(256              * total_comms, "Communicator description array");

    /* the first communicator is MPI_COMM_WORLD */
    comms[0]  = MPI_COMM_WORLD;
    strcpy(&comm_desc[256*current_comm], "MPI_COMM_WORLD");
    MPI_Attr_put(comms[0], dimid_key, (void*) &comm_desc[256*current_comm]); 
    current_comm++;

    /* if a partsize is specified, recursively divide MPI_COMM_WORLD in half until groups reach partsize */
    int currentsize = rank_count;
    int p = 0;
    while (p < partitions) {
        int partnum = (int) rank_local / currentsize;
        MPI_Comm_split(MPI_COMM_WORLD, partnum, rank_local, &comms[current_comm]);
        sprintf(&comm_desc[256*current_comm], "PartSize-%d", currentsize);
        MPI_Attr_put(comms[current_comm], dimid_key, (void*) &comm_desc[256*current_comm]); 
        current_comm++;
        currentsize >>= 1;
        p++;
    }

    /* for each communicator, run collective tests */
    int d;
    for (d=0; d < total_comms; d++) {
        MPI_Comm comm = comms[d];

        /* determine who we are in this communicator */
        int myrank, nranks;
        MPI_Comm_rank(comm, &myrank);
        MPI_Comm_size(comm, &nranks);

        struct collParams p;
        p.root   = 0;
        p.comm   = comm;
        p.myrank = myrank;
        p.nranks = nranks;
        p.type  = MPI_DOUBLE;

        /* time requested collectives */
        int o;
        struct op_desc {
            MPI_Op op;
            char   desc[20];
        } ops[3];

        ops[0].op = MPI_SUM;
        strcpy(ops[0].desc, "DOUBLE_SUM");
        ops[1].op = MPI_MIN;
        strcpy(ops[1].desc, "DOUBLE_MIN");
        ops[2].op = MPI_MAX;
        strcpy(ops[2].desc, "DOUBLE_MAX");

        char reduce_desc[256];
        if(testFlags & ALLREDUCE) {
            for(o=0; o<3; o++) {
                p.reduceop = ops[o].op;
                sprintf(reduce_desc, "Allreduce-%s", ops[o].desc);
                for(p.size = messStart; p.size <= messStop; p.size = (p.size > 0) ? p.size << 1 : 1) {
                    if(p.size < sizeof(double)) continue;
                    p.count = p.size / sizeof(double);
                    if(get_time(time_allreduce, reduce_desc, &p, iter, time_limit) > time_maxMsg && time_maxMsg > 0.0) break;
                }

                sprintf(reduce_desc, "Allreduce-%s-3D", ops[o].desc);
                for(p.size = messStart; p.size <= messStop; p.size = (p.size > 0) ? p.size << 1 : 1) {
                    if(p.size < sizeof(double)) continue;
                    p.count = p.size / sizeof(double);
                    if(get_time3d(time_allreduce, reduce_desc, &p, iter, time_limit) > time_maxMsg && time_maxMsg > 0.0) break;
                }
            }
        }
    } /* end loop over communicators */

    /* print memory usage */
    if (rank_local == 0) {
        printf("Message buffers (KB):\t%ld\n", allocated_memory/1024);
    }

/*
#ifndef _AIX
    print_mpi_resources();
#endif
*/

    /* mark end of output */
    if (rank_local == 0) { printf("END mpiBench_Allreduce\n"); }

    /* shut down */
    MPI_Finalize();
    return 0;
}
