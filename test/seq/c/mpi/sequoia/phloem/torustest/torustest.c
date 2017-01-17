/*==================================================*/
/* MPI Torus-Test for Sequoia                       */
/* Main benchmark driver                            */
/*==================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>
#include <math.h>
#include <time.h>
#include <unistd.h>

#include "getopt.h"
#include "bmtime.h"


/*==================================================*/
/* Constants */

#define MAXMPITESTS 1
#define MAXFILENAMELEN 100
#define DEFAULT_REPEATS    1000
#define MAXWORDLINE 100

#define BM_TAG 42

#define VERSION_MAJOR 1
#define VERSION_MINOR 0
#define VERSION_PATCH 0


/*-------------------------------------------------*/
/* Error codes */

#define LTBM_OK                0
#define LTBM_ERR_MEM          -1
#define LTBM_ERR_ARG          -2
#define LTBM_ERR_MPI          -3
#define LTBM_ERR_LINETOOLONG  -4
#define LTBM_ERR_WRONGNODES   -5
#define LTBM_ERR_FILEFAIL     -6


/*-------------------------------------------------*/
/* Macros */

#define LINKS(l,s) links[globconf.stencilsize*l+s]


/*==================================================*/
/* Types */


typedef struct redtype_d
{
  double val;
  int loc;
} redtype_t;


/*-------------------------------------------------*/
/* Configuration */

typedef struct globconf_d 
{
  char argfile[MAXFILENAMELEN];
  int verbose,repeats;
  int minsize,maxsize,multsize,msgsizes;
  int numtasks,numcores,stencilsize;
} globconf_t;

typedef struct topology_d
{
  int rank,size;
  int node_width;
} topology_t;


/*-------------------------------------------------*/
/* Benchmark Variations */


typedef int (*benchmark_fct_t) (int links, int *linklist, int msgsize, int repeat, void* buf, bmtime_t *result2, bmtime_t *result1, MPI_Comm comm);

typedef struct mpi_tests_d
{
  benchmark_fct_t fct;
  char name[40];
} mpi_tests_t;
    

/*-------------------------------------------------*/
/* Result Storage */

typedef struct mpi_result_d
{
  double *results;
  double *times_in,*times_out;
  double maxbw,minbw;
  double sumbw;
  double in_time,out_time;
} mpi_result_t;

    
/*==================================================*/
/* Prototypes for benchmark routines */

int benchmark_isendrecv(int links, int *linklist, int msgsize, int repeat, void* buf, 
			bmtime_t *result2, bmtime_t *result1, MPI_Comm comm);
int benchmark_isendrecv_il(int links, int *linklist, int msgsize, int repeat, void* buf, 
			   bmtime_t *result2, bmtime_t *result1, MPI_Comm comm);


static mpi_tests_t mpitests[MAXMPITESTS] =
  {
    { benchmark_isendrecv, "BIDIR-ISENDRECV" },
  };    

int test_requested[MAXMPITESTS];


/*==================================================*/
/* Globals */

topology_t   topology;
globconf_t   globconf;
int *links;
int *linklist;
int          *msgsize_list;


/*==================================================*/
/* Error handling */

void throw_error(int err)
{
  int init_flag=0;

  if (err!=LTBM_OK)
    {
      fprintf(stderr,"Aborting: ");
      switch(err)
	{
	case LTBM_ERR_MEM:
	  fprintf(stderr,"Out of Memory");
	  break;
	case LTBM_ERR_ARG:
	  fprintf(stderr,"Commandline Argument Error");
	  break;
	case LTBM_ERR_MPI:
	  fprintf(stderr,"Error Returned from MPI");
	  break;
	case LTBM_ERR_LINETOOLONG:
	  fprintf(stderr,"Input Line Too Long");
	  break;
	case LTBM_ERR_WRONGNODES:
	  fprintf(stderr,"Wrong number of nodes to run the benchmark with this configuration file.");
	  break;
	case LTBM_ERR_FILEFAIL:
	  fprintf(stderr,"Failed to open configuration file.");
	  break;
	default:
	  fprintf(stderr,"Unknown Error %i",err);
	  break;
	}
      fprintf(stderr,"\n");

      MPI_Initialized(&init_flag);

      if (init_flag)
        MPI_Abort(MPI_COMM_WORLD, 2);
      else
        exit(2);
    } 
}


/*==================================================*/
/* Diagnose Routines */


/*==================================================*/
/* Option processing */

void usage()
{
  int i;

  if (topology.rank==0)
    {
      fprintf(stderr,"Usage: torustest -f <cfgfile>\n");
      fprintf(stderr,"                 [-m <min msg size>] [-M <max msg size>] [-i <msg size mult.>]\n");
      fprintf(stderr,"                 [-b <bwtest>{:<bwtest>}]\n");
      fprintf(stderr,"                 [-I <number of iterations>] [-h] [-v]\n");

      fprintf(stderr,"\nBandwidth tests available:\n");
      for (i=0; i<MAXMPITESTS; i++)
	{
	  fprintf(stderr,"    %2i: %s\n",i,mpitests[i].name);
	}
    }
  
  exit(0);
}


int process_cmdline(int *argc, char*** argv)
{
  char chnum[MAXWORDLINE];
  int msgsize,i,j,num, ch;
  
  /* set defaults */

  globconf.argfile[0]=(char) 0;
  globconf.verbose=0;
  globconf.minsize=1;
  globconf.maxsize=512*1024;
  globconf.multsize=2;
  globconf.repeats=DEFAULT_REPEATS;

  for (i=0; i<MAXMPITESTS; i++)
    test_requested[i]=1;

  /* process arguments and store them */

  while ((ch = getopt(*argc, *argv,"f:m:M:b:i:I:hv"))!= EOF)
    {
      switch (ch)
        {
        case 'f':
	  if (strlen(optarg)>MAXFILENAMELEN)
	    return LTBM_ERR_ARG;
	  strcpy(globconf.argfile,optarg);
          break;
	case 'm':
	  globconf.minsize=atoi(optarg);
	  break;
	case 'M':
	  globconf.maxsize=atoi(optarg);
	  break;
	case 'i':
	  globconf.multsize=atoi(optarg);
	  break;
	case 'I':
	  globconf.repeats=atoi(optarg);
	  break;
	case 'b':
	  for (i=0; i<MAXMPITESTS; i++)
	    test_requested[i]=0;

	  i=0;
	  j=0;
	  while (optarg[i]!=((char) 0))
	    {
	      if (optarg[i]==',')
		{
		  /* start new number */
		  if (j==0)
		    return LTBM_ERR_ARG;
		  chnum[j]=(char) 0;
		  num=atoi(chnum);
		  if ((num<0) || (num>=MAXMPITESTS))
		    return LTBM_ERR_ARG;
		  if (test_requested[num])
		    return LTBM_ERR_ARG;
		  test_requested[num]=1;
		  j=0;
		}
	      else
		{
		  chnum[j]=optarg[i];
		  j++;
		  if (j>=MAXWORDLINE)
		    return LTBM_ERR_ARG;
		}
	      i++;
	    }
	  if (j==0)
	    return LTBM_ERR_ARG;
	  chnum[j]=(char) 0;
	  num=atoi(chnum);
	  if ((num<0) || (num>=MAXMPITESTS))
	    return LTBM_ERR_ARG;
	  if (test_requested[num])
	    return LTBM_ERR_ARG;
	  test_requested[num]=1;
	case 'v':
	  globconf.verbose=1;
	  break;
	case 'h':
	default:
	  usage();
	  break;
	}
    }

  if ((optind<*argc) || (globconf.argfile[0]==(char)0))
    {
      usage();
    }

  /* Calculate derived values */

  globconf.msgsizes=0;
  msgsize=globconf.minsize;
  do
    {
      globconf.msgsizes++;
      msgsize *=globconf.multsize;
    }
  while (msgsize<=globconf.maxsize);

  msgsize_list=(int*)malloc(sizeof(int)*globconf.msgsizes);
  if (msgsize_list==NULL) return LTBM_OK;
  
  msgsize=globconf.minsize;
  i=0;
  do
    {
      msgsize_list[i]=msgsize;
      msgsize *=globconf.multsize;
      i++;
    }
  while (msgsize<=globconf.maxsize);

  return LTBM_OK;
}


int read_task(FILE *fh)
{
  int ptr,ws;
  char word[MAXWORDLINE];

  ptr=0;
  ws=1;
  do
    {
      word[ptr]=fgetc(fh);
      if (!feof(fh))
	{
	  if ((word[ptr]==' ') || 
	      (word[ptr]=='\n') || 
	      (word[ptr]=='\t'))
	    ws=1;
	  else
	    {
	      if (word[ptr]=='#')
		{
		  ws=1;
		  while ((fgetc(fh)!='\n') && (!(feof(fh))));
		}
	      else
		{
		  ws=0;
		  ptr++;
		  if (ptr==MAXWORDLINE)
		    throw_error(LTBM_ERR_LINETOOLONG);
		}
	    }
	}
    }
  while ((!feof(fh)) && ((ws==0) || (ptr==0)));

  word[ptr]=(char) 0;

  return atoi(word);
}


int parse_configfile()
{
  FILE *fh;
  int i,j,t;

  fh=fopen(globconf.argfile,"r");

  if (NULL==fh)
    return LTBM_ERR_FILEFAIL;
  
  globconf.numtasks=read_task(fh);
  globconf.numcores=read_task(fh);
  globconf.stencilsize=read_task(fh);


  /* sanity check */

  if (globconf.numtasks!=topology.size)
    return LTBM_ERR_WRONGNODES;


  /* read link lists */

  links=(int*) malloc(sizeof(int)*globconf.stencilsize*globconf.numtasks);
  if (links==NULL)
    throw_error(LTBM_ERR_MEM);

  for (i=0; i<globconf.numtasks; i++)
    {
      t=read_task(fh);
      for (j=0; j<globconf.stencilsize; j++)
	LINKS(t,j)=read_task(fh);
    }

  fclose(fh);

  return LTBM_OK;
}


int distribute_configfile()
{
  int err;

  err=MPI_Bcast(&globconf.numtasks,1,MPI_INT,0,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return err;
  err=MPI_Bcast(&globconf.numcores,1,MPI_INT,0,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return err;
  err=MPI_Bcast(&globconf.stencilsize,1,MPI_INT,0,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return err;
  
  /* allocate local linklist */

  linklist=(int*)malloc(sizeof(int)*globconf.stencilsize);
  if (linklist==NULL) return LTBM_ERR_MEM;

  /* scatter list */

  err=MPI_Scatter(links,globconf.stencilsize,MPI_INT,
		  linklist,globconf.stencilsize,MPI_INT,
		  0,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return err;

  return LTBM_OK;
}


/*==================================================*/
/* Result Management */

int result_allocate(mpi_result_t **newres)
{
  int i;

  *newres=(mpi_result_t*) malloc(sizeof(mpi_result_t));
  if (*newres==NULL) return LTBM_ERR_MEM;

  (*newres)->results=(double*) malloc(globconf.msgsizes*sizeof(double)*MAXMPITESTS);
  if ((*newres)->results==NULL) return LTBM_ERR_MEM;
  (*newres)->times_in=(double*) malloc(globconf.msgsizes*sizeof(double)*MAXMPITESTS);
  if ((*newres)->times_in==NULL) return LTBM_ERR_MEM;
  (*newres)->times_out=(double*) malloc(globconf.msgsizes*sizeof(double)*MAXMPITESTS);
  if ((*newres)->times_out==NULL) return LTBM_ERR_MEM;

  for (i=0; i<globconf.msgsizes*MAXMPITESTS; i++)
    {
      (*newres)->results[i]=0;
      (*newres)->times_in[i]=0;
      (*newres)->times_out[i]=0;
    }

  return LTBM_OK;
}


int result_addpoint(mpi_result_t *result,int mpitest,int cnt, double size, bmtime_t in_time, bmtime_t out_time)
{
  double bw;

  bw=(((double) size)/(1024*1024))/BMTIME_TO_SEC(in_time);
  result->results[cnt+mpitest*globconf.msgsizes]=bw;
  result->times_in[cnt+mpitest*globconf.msgsizes]=BMTIME_TO_SEC(in_time);
  result->times_out[cnt+mpitest*globconf.msgsizes]=BMTIME_TO_SEC(out_time);

  if ((globconf.verbose) && (topology.rank==0))
    {
      printf("\tDone with msg size %7i: %f MB/s (in %f, out %f)\n",
	     msgsize_list[cnt],bw,in_time,out_time);
      fflush(stdout);
    }

  return LTBM_OK;
}


int result_print(mpi_result_t *result)
{
  int i,err;
  double maxbw,min_time,max_time;
  int maxsize_all;
  redtype_t red,redin;

  maxbw=0;
  maxsize_all=-1;
  for (i=0; i<globconf.msgsizes*MAXMPITESTS; i++)
    {
      if (maxbw<result->results[i])
	{
	  maxbw=result->results[i];
	  maxsize_all=i;
	}
    }

  red.val=maxbw;
  red.loc=maxsize_all;

  err=MPI_Allreduce(&red,&redin,1,MPI_DOUBLE_INT,MPI_MAXLOC,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return err;

  maxbw=result->results[redin.loc];
  min_time=result->times_in[redin.loc];
  max_time=result->times_out[redin.loc];

  err=MPI_Allreduce(&maxbw,&(result->minbw),1,MPI_DOUBLE,MPI_MIN,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return err;
  err=MPI_Allreduce(&maxbw,&(result->maxbw),1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return err;
  err=MPI_Allreduce(&maxbw,&(result->sumbw),1,MPI_DOUBLE,MPI_SUM,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return err;

  err=MPI_Allreduce(&min_time,&(result->in_time),1,MPI_DOUBLE,MPI_MIN,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return err;
  err=MPI_Allreduce(&max_time,&(result->out_time),1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return err;

  if (topology.rank==0)
    {
      printf("\nTorusTest v%d.%d.%d done - BW Sum %f MB/s\n\tBandwidth range %f - %f\n\tMinimal inner time %f\n\tMaximal outer time %f\n\n",
	     VERSION_MAJOR,
             VERSION_MINOR,
             VERSION_PATCH,
	     result->sumbw,
	     result->maxbw,result->minbw,
	     result->in_time,result->out_time);
      fflush(stdout);
    }

  free(result->results);
  free(result->times_in);
  free(result->times_out);  
  free(result);

  return LTBM_OK;
}



/*==================================================*/
/* Benchmark communication routines */

/*-----------------------------------------------*/
/* isend/irecv / fixed iterations */

int benchmark_isendrecv(int links, int *linklist, int msgsize, int repeat, void* buf, 
			bmtime_t *result2, bmtime_t *result1, MPI_Comm comm)
{
  bmtime_t time1_start,time1_end;
  bmtime_t time2_start,time2_end;
  int i,l,err;
  MPI_Request *reqlist;

  reqlist=(MPI_Request*)malloc(2*links*sizeof(MPI_Request));
  if (reqlist==NULL) return LTBM_ERR_MEM;

  time1_start=bmtime_getTime();
  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time2_start=bmtime_getTime();

  for (i=0; i<repeat/2; i++)
    {
      for (l=0; l<links; l++)
	{
	  err=MPI_Isend(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,comm,&(reqlist[l]));
	  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	}
      for (l=0; l<links; l++)
	{
	  err=MPI_Irecv(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,comm,&(reqlist[l+links]));
	  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	}
      err=MPI_Waitall(2*links,reqlist,MPI_STATUSES_IGNORE);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
    }
 
  time2_end=bmtime_getTime();
  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time1_end=bmtime_getTime();
  *result2=BMTIME_DIFF(time2_start,time2_end);
  *result1=BMTIME_DIFF(time1_start,time1_end);
  free(reqlist);
  return LTBM_OK;
}


/*-----------------------------------------------*/
/* driver */

/* run a benchmark node configuration across all msgsizes and 
   communication routine options */

int bm_driver(int links, int *linklist, int repeat)
{
  double size;
  int err;
  bmtime_t locresult1;
  bmtime_t locresult2;
  void *buf;
  long msgsize;
  int msgcnt;
  mpi_result_t *test_result;
  int mpitest;

  buf=malloc(globconf.maxsize);
  if (buf==NULL) return LTBM_ERR_MEM;
  
  err=result_allocate(&test_result);
  if (err!=LTBM_OK) return err;
  
  for (mpitest=0; mpitest<MAXMPITESTS; mpitest++)
    {
      if (test_requested[mpitest])
	{
	  err=MPI_Barrier(MPI_COMM_WORLD);  
	  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	  msgsize=globconf.minsize;
	  msgcnt=0;
	  
	  do
	    {
	      size=((double)msgsize)*((double)links)*((double)repeat);
	      
	      err=MPI_Barrier(MPI_COMM_WORLD);
	      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	      
	      err=mpitests[mpitest].fct(links,linklist,msgsize,repeat,buf,&locresult2,&locresult1,MPI_COMM_WORLD);
	      if (err!=LTBM_OK) return err;

	      err=MPI_Barrier(MPI_COMM_WORLD);
	      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	      
	      /* store results */
	      
	      err=result_addpoint(test_result,mpitest,msgcnt,size,locresult2,locresult1);
	      if (err!=LTBM_OK) return err;

	      msgsize*=globconf.multsize;
	      msgcnt++;
	    }
	  while (msgsize<=globconf.maxsize);
	}
    }

  err=result_print(test_result);
  if (err!=LTBM_OK) return err;

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;

  free(buf);
  return LTBM_OK;
}


/*==================================================*/
/* Create link list and run tests */

int create_and_run_configuration()
{
  int err;

  err=bm_driver(globconf.stencilsize, 
		linklist,
		globconf.repeats);
  if (err!=LTBM_OK) return err;

  return LTBM_OK;
}


/*==================================================*/
/* Loop over all node configurations and initiate benchmark runs */

int run_all_benchmarks()
{
  int err;
  
  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;

  err=create_and_run_configuration();
  if (err!=LTBM_OK) return err;

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;

  return LTBM_OK;
}


/*==================================================*/
/* Main routine */

int main(int argc, char** argv)
{
  int err;

  /* start MPI */

  err=MPI_Init(&argc,&argv);
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);
  err=MPI_Comm_rank(MPI_COMM_WORLD,&(topology.rank));
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);
  err=MPI_Comm_size(MPI_COMM_WORLD,&(topology.size));
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);

#ifdef PRINT_ENV
  extern void printEnv(void);
  if ( topology.rank == 0 )
    {
      printEnv();
    }
#endif


  /* read arguments */

  err=process_cmdline(&argc,&argv);
  if (err!=LTBM_OK) throw_error(err);


  if (topology.rank==0)
    {
      /* read the configuration file and set defaults */
      
      err=parse_configfile();
      if (err!=LTBM_OK) throw_error(err);
    }      
      

  /* Setup Topology and Check it */

  err=distribute_configfile();
  if (err!=LTBM_OK) throw_error(err);


  /* Setup complete - barrier */
      
  fflush(stdout);
  fflush(stderr);
  
  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);
      
  sleep(1);

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);


  /* call the actual benchmark */

  err=run_all_benchmarks();
  if (err!=LTBM_OK) throw_error(LTBM_ERR_MPI);


  /* Benchmark complete - barrier */

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);


  /* all done - barrier */

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);

  err=MPI_Finalize();
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);


  /* Done */
  
  return 0;
}


/*==================================================*/
/* The End. */
