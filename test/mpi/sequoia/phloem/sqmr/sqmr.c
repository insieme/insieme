/*
 * LLNL Sequoia Message Rate Benchmark (SQMR)
 *
 * Written by Andrew Friedley
 * LLNL Contacts:   Chris Chambreau (chcham@llnl.gov) or  Matt Leininger (leininger4@llnl.gov)
 * UCRL-CODE-400846
 * Copyright (C) 2007 The Regents of the University of California.
 * Copyright (C) 2007,2008 Lawrence Livermore National Security, LLC.
 * 
 * This software is available to you under the GNU General Public License (GPL)
 * Version 2, available from the file COPYING in the main directory of this source tree,
 * or from the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#define _XOPEN_SOURCE 600
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include "mpi.h"


/*
 *  Parameter key:
 *   num_cores      Number of MPI ranks on the 'core' node, correlates to
 *                  number of cores on one compute node.
 *   num_nbors      Number of distinct neighbors to each rank on the core node.
 *   num_iters      Number of benchmark iterations to perform for each message
 *                  size.
 *   win_size       How many concurrent sends/recv pairs to post for each
 *                  neighbor in a single iteration.
 *   min_msgsize    Smallest message size to benchmark.
 *   max_msgsize    Largest message size to benchmark.  Benchmarking starts with
 *                  min_msgsize, and then doubles the size repeatedly until
 *                  max_msgsize is reached.
 *   
 *  The purpose of this benchmark is to measure the maximal message rate of a
 *  single compute node.  The first num_cores ranks are expected to reside on
 *  the 'core' compute node for which message rate is being tested.  After that,
 *  the next num_nbors ranks are neighbors for the first core rank, the next set
 *  of num_nbors ranks are neighbors for the second core rank, and so on.  For
 *  example, testing an 8-core node (num_cores = 8) with 4 neighbors
 *  (num_nbors = 4) requires 8 + 8 * 4 = 40 ranks.  The first 8 of those 40
 *  ranks are expected to be on the 'core' node being benchmarked, while the
 *  rest of the ranks are on separate nodes.
 *
 *  Parameters are all specified on the command line, some are required
 *  (num_cores, num_nbors), while others are optional and have defaults set
 *  below.
 */

int mpi_rank;
int mpi_size;
MPI_Comm mpi_comm_core;

int num_cores = 0;
int num_nbors = 1;
int num_iters = 4096;
int win_size = 1;
int min_msgsize = 0;
int max_msgsize = 1048576 * 4;
int verbose = 0;

int version_major = 1;
int version_minor = 0;
int version_patch = 0;

void *sbuf = NULL;
void *rbuf = NULL;

MPI_Request *sreq;
MPI_Request *rreq;
MPI_Status *st;

struct option
{
  const char *name;
  int has_arg;
  int *flag;
  int val;
};
char *optarg;
const char *optflag;

int
sqmr_getopt_long_only (int argc, char *const argv[],
		       const char *optstring,
		       const struct option *longopts, int *longindex)
{
  static int argv_idx = 0;
  const char *curr_arg = NULL;
  int i = 0;

  *longindex = -1;
  optarg = NULL;

  argv_idx++;

  if (argv_idx >= argc || argv[argv_idx] == NULL)
    return -1;

  curr_arg = argv[argv_idx];
  while (*curr_arg == '-')
    curr_arg++;

  optflag = curr_arg;

  while (longopts[i].name != 0)
    {
      /*  Search for matching option  */
      int opt_len = strlen (longopts[i].name);
      if (strncmp (curr_arg, longopts[i].name, opt_len) == 0)
	{
	  *longindex = i;

	  /*  If appropriate, point to option value  */
	  if (longopts[i].has_arg != 0)
	    {
	      optarg = strchr (curr_arg, '=');
	      if (optarg != NULL && optarg[1] != '\0')
		optarg++;
              else
                {
                  if (!mpi_rank)
                    {
                      printf("Option %s requires an argument value.\n\n", 
                              longopts[i].name);
                    }
                  *longindex = 7;
                  return 0;
                }
	    }

	  return 0;
	}
      i++;
    }

  return '?';
}


/* Benchmark the messaging rate for a given message size */
void
benchmark (int msgsize, int iters)
{
  int i, j, k, len;

  MPI_Barrier (MPI_COMM_WORLD);

  if (mpi_rank < num_cores)
    {				/* core rank */
      int first_nbor = num_cores + (mpi_rank * num_nbors);
      double t = 0.0, total = 0.0, sum, max, mean, min;

      for (i = 0; i < iters + 10; i++)
	{
	  if (i == 10)
	    {
	      total = 0.0;
	      MPI_Barrier (MPI_COMM_WORLD);
	      t = MPI_Wtime ();
	    }

	  for (j = 0; j < win_size; j++)
	    {
	      for (k = 0; k < num_nbors; k++)
		{
		  int offset = (j * num_nbors) + k;

		  MPI_Irecv (rbuf, msgsize, MPI_BYTE, first_nbor + k,
			     /*offset */ 1,
			     MPI_COMM_WORLD, &rreq[offset]);
		  MPI_Isend (sbuf, msgsize, MPI_CHAR, first_nbor + k,
			     /*offset */ 1,
			     MPI_COMM_WORLD, &sreq[offset]);
		}
	    }

	  len = win_size * num_nbors;
	  MPI_Waitall (len, sreq, st);
	  MPI_Waitall (len, rreq, st);
	}

      total = MPI_Wtime () - t;
      MPI_Reduce (&total, &sum, 1, MPI_DOUBLE, MPI_SUM, 0, mpi_comm_core);
      MPI_Reduce (&total, &max, 1, MPI_DOUBLE, MPI_MAX, 0, mpi_comm_core);
      MPI_Reduce (&total, &min, 1, MPI_DOUBLE, MPI_MIN, 0, mpi_comm_core);
      mean = sum / num_cores;

      if (!mpi_rank)
	{
	  double msgs;

	  msgs = (double) (win_size * num_nbors * num_cores * iters * 2);
	  printf
	    ("%8d %5d %5.2lf %12.2lf %7.2lf %12.2lf %7.2lf %12.2lf %7.2lf\n",
	     msgsize, iters, mean, msgs / mean,
	     msgs / mean * (double) msgsize / 1048576.0, msgs / min,
	     msgs / min * (double) msgsize / 1048576.0, msgs / max,
	     msgs / max * (double) msgsize / 1048576.0);
	}
    }
  else
    {				/* neighbor rank */
      int core_rank = (mpi_rank - num_cores) / num_nbors;

      for (i = 0; i < iters + 10; i++)
	{
	  if (i == 10)
	    {
	      MPI_Barrier (MPI_COMM_WORLD);
	    }

	  for (j = 0; j < win_size; j++)
	    {
	      MPI_Irecv (rbuf, msgsize, MPI_CHAR, core_rank,
			 /*(j * num_nbors) + nbor_num */ 1, MPI_COMM_WORLD,
			 &rreq[j]);
	      MPI_Isend (sbuf, msgsize, MPI_BYTE, core_rank,
			 /*(j * num_nbors) + nbor_num */ 1, MPI_COMM_WORLD,
			 &sreq[j]);
	    }

	  MPI_Waitall (win_size, sreq, st);
	  MPI_Waitall (win_size, rreq, st);
	}
    }
}


void
usage (void)
{
  printf ("Usage:\n"
	  " --num_cores   <n> Number of MPI ranks on the 'core' node, correlates to\n"
	  "                   number of cores on one compute node.\n"
	  " --num_nbors   <n> Number of distinct neighbors to each rank on the core node.\n"
	  " --num_iters   <n> Number of benchmark iterations to perform for each message\n"
	  "                   size.\n"
	  " --win_size    <n> How many concurrent sends/recv pairs to post for each\n"
	  "                   neighbor in a single iteration.\n"
	  " --min_msgsize <n> Smallest message size to benchmark.\n"
	  " --max_msgsize <n> Largest message size to benchmark.  Benchmarking starts with\n"
	  "                   min_msgsize, and then doubles the size repeatedly until\n"
	  "                   max_msgsize is reached.\n\n");

}

int
main (int argc, char **argv)
{
  static struct option long_options[] = { {"num_cores", 1, 0, 0},
  {"num_nbors", 1, 0, 0},
  {"num_iters", 1, 0, 0},
  {"win_size", 1, 0, 0},
  {"min_msgsize", 1, 0, 0},
  {"max_msgsize", 1, 0, 0},
  {"verbose", 0, 0, 0},
  {"help", 0, 0, 0},
  {0, 0, 0, 0}
  };
  int range[3];
  MPI_Group group_world, group_core;
  int i, index, len, ncores_set = 0;

  MPI_Init (&argc, &argv);
  MPI_Comm_rank (MPI_COMM_WORLD, &mpi_rank);
  MPI_Comm_size (MPI_COMM_WORLD, &mpi_size);

#ifdef PRINT_ENV
  extern void printEnv(void);
  if ( mpi_rank == 0 )
    {
      printEnv();
    }
#endif

  while (1)
    {
      i = sqmr_getopt_long_only (argc, argv, "", long_options, &index);
      if (i == -1)
	{
	  break;
	}

      switch (index)
	{
	case 0:		/* num_cores */
	  num_cores = atoi (optarg);
          ncores_set = 1;
	  break;
	case 1:		/* num_nbors */
	  num_nbors = atoi (optarg);
	  break;
	case 2:		/* num_iters */
	  num_iters = atoi (optarg);
	  break;
	case 3:		/* win_size */
	  win_size = atoi (optarg);
	  break;
	case 4:		/* min_msgsize */
	  min_msgsize = atoi (optarg);
	  break;
	case 5:		/* max_msgsize */
	  max_msgsize = atoi (optarg);
	  break;
	case 6:		/* verbose */
	  verbose = 1;
	  break;
	case 7:		/* help */
	  if (!mpi_rank)
	    {
	      usage ();
	    }

	  MPI_Finalize ();
	  return 0;
	default:
	  if (!mpi_rank)
	    {
	      char *eqloc;
	      int width;

	      if (optflag != NULL)
		{
                  /*  Check for parameter to remove  */
		  eqloc = strchr (optflag, '=');
		  if (eqloc != NULL)
		    {
		      width = eqloc - optflag;
		      printf ("\nUnknown option %.*s\n\n", width, optflag);
		    }
		  else
		    printf ("\nUnknown option %s\n\n", optflag);
		}

	      usage ();
	    }

	  MPI_Finalize ();
	  return 0;
	}
    }

  if (num_cores <= 0 && !ncores_set)
    {
      /* Default is 1 neighbor */
      num_cores = mpi_size/2;
    }

  if (!mpi_rank)
    {
      if (num_cores <= 0)
	{
          fprintf (stderr, "ERROR Must specify num_cores > 0\n\n");
          usage ();
          MPI_Abort (MPI_COMM_WORLD, -1);
        }
      if (num_nbors <= 0)
	{
	  fprintf (stderr, "ERROR Must specify num_nbors > 0\n\n");
	  usage ();
	  MPI_Abort (MPI_COMM_WORLD, -1);
	}
      if (num_iters <= 0)
	{
	  fprintf (stderr, "ERROR Must specify num_iters > 0\n\n");
	  usage ();
	  MPI_Abort (MPI_COMM_WORLD, -1);
	}

      i = num_cores + (num_cores * num_nbors);
      if (i != mpi_size)
	{
	  fprintf (stderr,
		   "ERROR need exactly %d ranks for num_cores %d and num_nbors %d, have %d\n\n",
		   i, num_cores, num_nbors, mpi_size);
	  usage ();
	  MPI_Abort (MPI_COMM_WORLD, -1);
	}

      if (win_size < 1)
	{
	  fprintf (stderr, "ERROR Must specify win_size > 0\n\n");
	  usage ();
	  MPI_Abort (MPI_COMM_WORLD, -1);
	}

      if (min_msgsize < 0)
	{
	  fprintf (stderr, "ERROR Must specify min_msgsize >= 0\n\n");
	  usage ();
	  MPI_Abort (MPI_COMM_WORLD, -1);
	}
      if (max_msgsize < min_msgsize)
	{
	  fprintf (stderr,
		   "ERROR Must specify max_msgsize >= min_msgsize\n\n");
	  usage ();
	  MPI_Abort (MPI_COMM_WORLD, -1);
	}

      if (verbose)
	{
	  printf
	    ("# num_cores %d\n# num_nbors %d\n# num_iters %d\n# win_size %d\n# min_msgsize %d\n# max_msgsize %d\n\n",
	     num_cores, num_nbors, num_iters, win_size, min_msgsize,
	     max_msgsize);

	  for (i = 0; i < num_cores; i++)
	    {
	      int j;

	      printf ("# core rank %d has neighbor ranks", i);
	      for (j = 0; j < num_nbors; j++)
		{
		  printf (" %d", num_cores + (i * num_nbors) + j);
		}
	      puts ("");
	      fflush (stdout);
	    }
	  puts ("");
	}
    }


  if (verbose)
    {
      char buf[MPI_MAX_PROCESSOR_NAME];
      MPI_Get_processor_name (buf, &len);
      printf ("# rank %d is on %s\n", mpi_rank, buf);
    }


  /* Create a communicator for the ranks on the core node */
  range[0] = 0;
  range[1] = num_cores - 1;
  range[2] = 1;
  MPI_Comm_group (MPI_COMM_WORLD, &group_world);
  MPI_Group_range_incl (group_world, 1, &range, &group_core);
  MPI_Comm_create (MPI_COMM_WORLD, group_core, &mpi_comm_core);

  /* Allocate memory for communication */
  len = sizeof (char) * max_msgsize;
  posix_memalign (&sbuf, getpagesize (), len);
  posix_memalign (&rbuf, getpagesize (), len);

  memset (sbuf, 0, len);
  memset (rbuf, 0, len);

  len = sizeof (MPI_Request) * win_size * num_nbors;
  sreq = malloc (len);
  rreq = malloc (len);

  memset (sreq, 0, len);
  memset (rreq, 0, len);

  len = sizeof (MPI_Status) * win_size * num_nbors;
  st = malloc (len);
  memset (st, 0, len);

  if (!mpi_rank)
    {
      char nowstr[128];
      const struct tm *nowstruct;
      char *fmtstr = "%D %T";
      char procbuf[MPI_MAX_PROCESSOR_NAME];
      int len;
      time_t nowtime;

      printf ("# SQMR v%d.%d.%d - MPI maximal message rate benchmark\n", version_major, version_minor, version_patch);
      time (&nowtime);
      nowstruct = localtime (&nowtime);
      if (strftime (nowstr, 128, fmtstr, nowstruct) == (size_t) 0)
	fprintf (stderr, "Could not get string from strftime()\n");
      MPI_Get_processor_name (procbuf, &len);
      printf ("# Run at %s, with rank 0 on %s\n", nowstr, procbuf);
      puts ("#");
      printf("# MPI tasks per node                 : %d\n", num_cores);
      printf("# Neighbor tasks                     : %d\n", num_nbors);
      printf("# Iterations per message size        : %d\n", num_iters);
      printf("# Send/Recv operations per iteration : %d\n", win_size);
      puts ("#");
      puts
	("#                            average               max                  min");
      puts
	("# msgsize iters time     msgs/sec MiB/sec     msgs/sec MiB/sec     msgs/sec MiB/sec");
    }

  /* Do the benchmark starting from min_msgsize doubling to max_msgsize */
  for (i = min_msgsize, index = num_iters; i <= max_msgsize;
        i = (i ? i*2 : 1), index -= index / 5)
    {
      benchmark (i, index);
    }


  if (mpi_rank < num_cores)
    {
      MPI_Comm_free (&mpi_comm_core);
    }

  MPI_Group_free (&group_core);
  MPI_Finalize ();
  return 0;
}
