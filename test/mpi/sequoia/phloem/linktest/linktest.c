/*==================================================*/
/* MPI Link-Test for Sequoia                        */
/* Main benchmark driver                            */
/*==================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

#include "common.h"
#include "nodelib.h"
#include "getopt.h"
#include "bmtime.h"


/*==================================================*/
/* Constants */

#define MAXFILENAMELEN 100
#define MAXWORDLINE    100
#define MAXNODELIST    1000  /* needs to >= node width * largest stencil */

#define MAXMPITESTS 8
#define MAXMPILINKCONFIGS 2

#define LTBM_NODE_ROLE_NONE     0
#define LTBM_NODE_ROLE_ROOT     1
#define LTBM_NODE_ROLE_NEIGHBOR 2

#define BM_TAG 42

#define DEFAULT_REPEATS 1000

#define VERSION_MAJOR 1
#define VERSION_MINOR 0
#define VERSION_PATCH 0

/*-------------------------------------------------*/
/* Error codes */

#define NODELIB_ERR_CONV(err) (-(((-(err))|0x100)))
			       
#define LTBM_OK                0
#define LTBM_ERR_MEM          -1
#define LTBM_ERR_ARG          -2
#define LTBM_ERR_IO           -3
#define LTBM_ERR_LINETOOLONG  -4
#define LTBM_ERR_PARSE        -5
#define LTBM_ERR_HOSTNOTFOUND -6
#define LTBM_ERR_TOOFEWNODES  -7

#define LTBM_ERR_MPI       NODELIB_ERR_CONV(NODELIB_ERR_MPI)
#define LTBM_ERR_GETID     NODELIB_ERR_CONV(NODELIB_ERR_GETID)
#define LTBM_ERR_DEFAULTS  NODELIB_ERR_CONV(NODELIB_ERR_DEFAULTS)


/*==================================================*/
/* Types */

/*-------------------------------------------------*/
/* Configuration */

typedef struct globconf_d 
{
  char argfile[MAXFILENAMELEN];
  int maxtask,mintask;
  int roottask,rootnode;
  int maxneigh,minneigh;
  int maxoverload,minoverload;
  int verbose,repeats;
  int minsize,maxsize,multsize,msgsizes;
  int neighbor_list[MAXNODELIST],num_neighbor;
} globconf_t;


typedef struct topology_d
{
  MPI_Comm comm_local,comm_across;
  int rank,size,numnodes,node;
  int local_rank,root_size;
  int node_width,node_maxwidth;
  int role,rolerank;
} topology_t;


/*-------------------------------------------------*/
/* Benchmark Variations */


typedef int (*benchmark_fct_t) (int links, int *linklist, int msgsize, int repeat, void* buf, bmtime_t *result, MPI_Comm comm);

typedef struct mpi_tests_d
{
  benchmark_fct_t rootfct;
  benchmark_fct_t partnerfct;
  char name[40];
} mpi_tests_t;
    
typedef struct mpi_linkconfigs_d
{
  char name[40];
} mpi_linkconfigs_t;
    

/*-------------------------------------------------*/
/* Result Storage */

typedef struct mpi_config_d
{
  int rootnum;
  int linkconfig;
  int neighbors;
  int mpitest;
  int overload;
} mpi_config_t;
    
typedef struct mpi_result_d
{
  mpi_config_t config;
  double *results;
  struct mpi_result_d *next;
  double maxbw;
  int maxsize,halfsize;
} mpi_result_t;
    

/*==================================================*/
/* Prototypes for benchmark routines */

int benchmark_sendonly(int links, int *linklist, int msgsize, int repeat, void* buf, 
		       bmtime_t *result, MPI_Comm comm);
int benchmark_recvonly(int links, int *linklist, int msgsize, int repeat, void* buf, 
		       bmtime_t *result, MPI_Comm comm);
int benchmark_isendonly(int links, int *linklist, int msgsize, int repeat, void* buf, 
			bmtime_t *result, MPI_Comm comm);
int benchmark_irecvonly(int links, int *linklist, int msgsize, int repeat, void* buf, 
			bmtime_t *result, MPI_Comm comm);
int benchmark_isendonly_il(int links, int *linklist, int msgsize, int repeat, void* buf, 
			   bmtime_t *result, MPI_Comm comm);
int benchmark_irecvonly_il(int links, int *linklist, int msgsize, int repeat, void* buf, 
			   bmtime_t *result, MPI_Comm comm);
int benchmark_isendrecv(int links, int *linklist, int msgsize, int repeat, void* buf, 
			bmtime_t *result, MPI_Comm comm);
int benchmark_isendrecv_il(int links, int *linklist, int msgsize, int repeat, void* buf, 
			   bmtime_t *result, MPI_Comm comm);


/*==================================================*/
/* Globals */

topology_t   topology;
globconf_t   globconf;
nodeid_t     *nodeid_list;
int          *node_list,*task_list;
mpi_result_t *result_list = NULL;
int          *msgsize_list;

static mpi_tests_t mpitests[MAXMPITESTS] =
  {
    { benchmark_sendonly, benchmark_recvonly, "OUTBW-SEND" },
    { benchmark_recvonly, benchmark_sendonly, "INBW-RECV" },
    { benchmark_isendonly, benchmark_irecvonly, "OUTBW-ISEND" },
    { benchmark_irecvonly, benchmark_isendonly, "INBW-IRECV" },
    { benchmark_isendonly_il, benchmark_irecvonly_il, "OUTBW-ISEND-INTERLEAVE" },
    { benchmark_irecvonly_il, benchmark_isendonly_il, "INBW-IRECV-INTERLEAVE" },
    { benchmark_isendrecv, benchmark_isendrecv, "BIDIR-ISENDRECV" },
    { benchmark_isendrecv_il, benchmark_isendrecv_il, "BIDIR-ISENDRECV-INTERLEAVE" }
  };    

int test_requested[MAXMPITESTS];

#define LINKCONF_BLOCK 0
#define LINKCONF_ROUND 1

static mpi_linkconfigs_t linkconfigurations[MAXMPILINKCONFIGS] =
  {
    { "BLOCK" },
    { "ROUND_ROBIN" }
  };


/*==================================================*/
/* Error handling */

void throw_error(int err)
{
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
	case LTBM_ERR_GETID:
	  fprintf(stderr,"Can't Local Node ID");
	  break;
	case LTBM_ERR_IO:
	  fprintf(stderr,"File I/O Error");
	  break;
	case LTBM_ERR_LINETOOLONG:
	  fprintf(stderr,"Input Line Too Long");
	  break;
	case LTBM_ERR_HOSTNOTFOUND:
	  fprintf(stderr,"Can't Find a Host in the Current Partition");
	  break;
	case LTBM_ERR_DEFAULTS:
	  fprintf(stderr,"Can't compute defaults");
	  break;
	case LTBM_ERR_TOOFEWNODES:
	  fprintf(stderr,"The benchmarks needs at least two nodes to run");
	  break;
	default:
	  fprintf(stderr,"Unknown Error %i",err);
	  break;
	}
      fprintf(stderr,"\n");
      exit(2);
    } 
}


/*==================================================*/
/* Diagnose Routines */

/*-------------------------------------------------*/
/* print configuration on local node */

int diag_printTop()
{
  int i,j;

  printf("Topology\n");
  printf("\tRank = %i, Size = %i\n",topology.rank,topology.size);
  printf("\tNode = %i, Number of nodes = %i\n",topology.node,topology.numnodes);
  printf("\tLocal rank = %i\n",topology.local_rank);
  printf("\tRoot size = %i\n",topology.root_size);
  printf("\tlocal width = %i, Max width = %i\n",topology.node_width,topology.node_maxwidth);
  printf("\n");

  printf("Translation Task->Node\n");
  for (i=0; i<topology.size; i++)
    {
      printf("\t Task %3i --> Node %3i (%s)\n",i,node_list[i],nodeid_list[i].name);
    }
  printf("\n");

  printf("Translation Node->Tasks\n");
  fflush(stdout);
  for (i=0; i<topology.numnodes; i++)
    {
      printf("\t Node %3i --> Tasks",i);
      for (j=0; j<topology.node_maxwidth; j++)
	{
	  if (task_list[i*topology.node_maxwidth+j]>=0)
	    {
	      printf(" %3i",task_list[i*topology.node_maxwidth+j]);
	    }
	}
      printf("\n");
    }
  printf("\n");

  printf("Global configuration\n");
  printf("\tRoot task %i / Root node %i\n",globconf.roottask,globconf.rootnode);
  printf("\tTasks min %i / max %i\n",globconf.mintask,globconf.maxtask);
  printf("\tNeighbors min %i / max %i\n",globconf.minneigh,globconf.maxneigh);
  printf("\tOverload min %i / max %i\n",globconf.minoverload,globconf.maxoverload);
  printf("\tMessage min %i / max %i / mult %i (num %i)\n",
	 globconf.minsize,globconf.maxsize,globconf.multsize,globconf.msgsizes);
  printf("\tNeighbors (%i):",globconf.num_neighbor);
  for (i=0; i<globconf.num_neighbor; i++)
    printf(" %i",globconf.neighbor_list[i]);
  printf("\n");

  return LTBM_OK;
}


/*==================================================*/
/* Topology Support */

/*-------------------------------------------------*/
/* determine topology of given partition
 * Warning: must be called after MPI_Init
 *          collective operation */

int top_setupMPI(int *argc, char*** argv)
{
  int          min_gr_rank;
  int          numtask;
  int          err,i;
  MPI_Datatype idtype;


  /* start nodelib */

  err=nodelib_init(&idtype);
  if (err!=NODELIB_OK) return NODELIB_ERR_CONV(err);


  /* allocate ID space */

  nodeid_list=(nodeid_t*) malloc(sizeof(nodeid_t)*topology.size);
  if (nodeid_list==NULL) return LTBM_ERR_MEM;


  /* get local ID and distribute it */

  err=nodelib_getID(&(nodeid_list[topology.rank]));
  if (err!=NODELIB_OK) return NODELIB_ERR_CONV(err);
  err=MPI_Allgather(&(nodeid_list[topology.rank]),1,idtype,nodeid_list,1,idtype,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;


  /* find the speaker for the local group */

  min_gr_rank=-1;
  numtask=0;
  for (i=0; i<topology.size; i++)
    {
      if (nodelib_sameID(&(nodeid_list[i]),&(nodeid_list[topology.rank])))
	{
	  if (i==topology.rank)
	    topology.local_rank=numtask;
	  numtask++;
	  if (min_gr_rank==-1)
	    min_gr_rank=i;
	}
    }

  /* create local communicators */

  err=MPI_Comm_split(MPI_COMM_WORLD,min_gr_rank,topology.local_rank,&topology.comm_local);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  
  err=MPI_Comm_split(MPI_COMM_WORLD,topology.local_rank,topology.rank,&topology.comm_across);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  


  /* get and distribute the number of nodes */

  if (topology.rank==0)
    {
      err=MPI_Comm_size(topology.comm_across,&topology.numnodes);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  
    }
  err=MPI_Bcast(&topology.numnodes,1,MPI_INT,0,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  


  /* get and distribute the node number */

  if (topology.local_rank==0)
    {
      err=MPI_Comm_rank(topology.comm_across,&topology.node);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  
    }
  err=MPI_Bcast(&topology.node,1,MPI_INT,0,topology.comm_local);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;    


  /* create translation table task->node */

  node_list=(int*) malloc(sizeof(int)*topology.size);
  if (node_list==NULL) return LTBM_ERR_MEM;
  err=MPI_Allgather(&topology.node,1,MPI_INT,node_list,1,MPI_INT,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;


  /* create translation table node/local->task */
  /* a) find maximal node width 
     b) gather task list on local node
     c) gather all task lists on speaker tasks
     d) distribute list to all nodes */

  err=MPI_Comm_size(topology.comm_local,&topology.node_width);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  err=MPI_Allreduce(&topology.node_width,&topology.node_maxwidth,1,MPI_INT,MPI_MAX,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;

  task_list=(int*) malloc(sizeof(int)*topology.node_maxwidth*topology.numnodes);
  if (task_list==NULL) return LTBM_ERR_MEM;
  for (i=0; i<topology.node_maxwidth*topology.numnodes; i++)
    task_list[i]=-1;

  err=MPI_Allgather(&topology.rank,1,MPI_INT,&task_list[topology.node*topology.node_maxwidth],
		    1,MPI_INT,topology.comm_local);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  if (topology.local_rank==0)
    {
      err=MPI_Allgather(&task_list[topology.node*topology.node_maxwidth],topology.node_maxwidth,MPI_INT,
			task_list,topology.node_maxwidth,MPI_INT,topology.comm_across);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
    }

  err=MPI_Bcast(task_list,topology.numnodes*topology.node_maxwidth,MPI_INT,0,topology.comm_local);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
      
  /* get and distribute local node number to all ranks */

  err=MPI_Comm_rank(topology.comm_across,&topology.node);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  

  err=MPI_Bcast(&topology.node,1,MPI_INT,0,topology.comm_local);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  

  /* Done */
  
  return LTBM_OK;
}


/*-------------------------------------------------*/
/* Create the actual topology configuration */

int top_setupTop()
{
  int err,i,j;


  /* distribute root task on COMM_WORLD */

  err=MPI_Bcast(&globconf.rootnode,1,MPI_INT,0,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  

  if (topology.local_rank==0)
    {
      globconf.roottask=topology.rank;
      err=MPI_Bcast(&globconf.roottask,1,MPI_INT,globconf.rootnode,topology.comm_across);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  
    }
  
  err=MPI_Bcast(&globconf.roottask,1,MPI_INT,0,topology.comm_local);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  


  /* find max. number of tasks on root node and adjust max parameter */

  if (topology.rank==globconf.roottask)
    {
      err=MPI_Comm_size(topology.comm_local,&topology.root_size);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  
    }

  err=MPI_Bcast(&topology.root_size,1,MPI_INT,globconf.roottask,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  

  if (globconf.maxtask<0)
    globconf.maxtask=topology.root_size;


  /* distribute neighbor lists */

  err=MPI_Bcast(&globconf.num_neighbor,1,MPI_INT,0,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  
  err=MPI_Bcast(globconf.neighbor_list,globconf.num_neighbor,MPI_INT,0,MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;  


  /* check consistency (on rank 0) */

  if (topology.rank==0)
    {
      for (i=0; i<globconf.num_neighbor; i++)
	{
	  if (globconf.neighbor_list[i]==globconf.rootnode)
	    {
	      fprintf(stderr,"Root node in neighbor list\n");
	      return LTBM_ERR_ARG;
	    }
	  for (j=0; j<i; j++)
	    {
	      if (globconf.neighbor_list[i]==globconf.neighbor_list[j])
		{
		  fprintf(stderr,"Node twice in neighbor list\n");
		  return LTBM_ERR_ARG;
		}
	    }
	}
    }


  /* define local role */

  topology.role=LTBM_NODE_ROLE_NONE;
  topology.rolerank=-1;

  if (globconf.rootnode==topology.node)
    {
      topology.role=LTBM_NODE_ROLE_ROOT;
      topology.rolerank=0;
    }
  else
    {
      for (i=0; i<globconf.num_neighbor; i++)
	{
	  if (globconf.neighbor_list[i]==topology.node)
	    {
	      topology.role=LTBM_NODE_ROLE_NEIGHBOR;
	      topology.rolerank=i;
	    }
	}
    }

  return LTBM_OK;
}


int convert_rank(char *host)
{
  int i=0;
  for (i=0; i<topology.size; i++)
    {
      if (strcmp(nodeid_list[i].name,host)==0)
	return i;
    }
  return LTBM_ERR_HOSTNOTFOUND;
}


/*==================================================*/
/* Option processing */

void usage()
{
  int i;

  if (topology.rank==0)
    {
      fprintf(stderr,"Usage: linktest [-f <cfgfile>] [-t <mintask>] [-T <maxtask>]\n");
      fprintf(stderr,"                [-r <root task>] [-R <root node>]\n");
      fprintf(stderr,"                [-n <min neighbor>] [-N <max neighbor>]\n");
      fprintf(stderr,"                [-m <min msg size>] [-M <max msg size>] [-i <msg size mult.>]\n");
      fprintf(stderr,"                [-o <min overload>] [-O <max overload>]\n");
      fprintf(stderr,"                [-b <bwtest>{:<bwtest>}]\n");
      fprintf(stderr,"                [-I <number of iterations>] [-h] [-v]\n");

      fprintf(stderr,"\nBandwidth tests available:\n");
      for (i=0; i<MAXMPITESTS; i++)
	{
	  fprintf(stderr,"    %2i: %s\n",i,mpitests[i].name);
	}
    }
  
  exit(0);
}


#define MAXNUMWIDTH 10
int process_cmdline(int *argc, char*** argv)
{
  char chnum[MAXNUMWIDTH];
  int msgsize,i,j,num,ch;
  
  /* set defaults */

  globconf.argfile[0]=(char) 0;
  globconf.mintask=1;
  globconf.maxtask=-1;
  globconf.roottask=-1;
  globconf.rootnode=-1;
  globconf.maxneigh=6;
  globconf.minneigh=6;
  globconf.maxoverload=1;
  globconf.minoverload=1;
  globconf.verbose=0;
  globconf.num_neighbor=0;
  globconf.minsize=1;
  globconf.maxsize=512*1024;
  globconf.multsize=2;
  globconf.repeats=DEFAULT_REPEATS;

  for (i=0; i<MAXMPITESTS; i++)
    test_requested[i]=1;

  /* process arguments and store them */

  //  while ((ch = getopt_long(*argc, *argv,"f:t:T:r:R:n:N:m:M:o:O:b:i:I:hv",long_options, 0))!= EOF)
  while ((ch = getopt(*argc, *argv,"f:t:T:r:R:n:N:m:M:o:O:b:i:I:hv"))!= EOF)
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
	case 'T':
	  globconf.maxtask=atoi(optarg);
	  break;
	case 't':
	  globconf.mintask=atoi(optarg);
	  break;
	case 'r':
	  if (globconf.rootnode>=0)
	    return LTBM_ERR_ARG;
	  globconf.roottask=atoi(optarg);
	  break;
	case 'R':
	  if (globconf.roottask>=0)
	    return LTBM_ERR_ARG;
	  globconf.rootnode=atoi(optarg);
	  break;
	case 'n':
	  globconf.minneigh=atoi(optarg);
	  if (globconf.maxneigh<globconf.minneigh)
	    globconf.maxneigh=globconf.minneigh;
	  break;
	case 'N':
	  globconf.maxneigh=atoi(optarg);
	  if (globconf.maxneigh<globconf.minneigh)
	    globconf.minneigh=globconf.maxneigh;
	  break;
	case 'o':
	  globconf.minoverload=atoi(optarg);
	  break;
	case 'O':
	  globconf.maxoverload=atoi(optarg);
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
		  if (j>=MAXNUMWIDTH)
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

  if (optind<*argc)
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

  msgsize_list=(int*)malloc(sizeof(int)*globconf.maxsize);
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


int read_word(FILE *fh, char *word)
{
  int ptr,ws;

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
		    return LTBM_ERR_LINETOOLONG;
		}
	    }
	}
    }
  while ((!feof(fh)) && ((ws==0) || (ptr==0)));

  word[ptr]=(char) 0;

  return ptr;
}


int parse_configfile()
{
  FILE *argfile;
  char word[MAXWORDLINE];
  int err,ranktype=0,nodetype=0,rootdone=0,level,newrank;

  level=0;

  if (strcmp(globconf.argfile,"")!=0)
    {
      /* configuration file specified at command line */

      argfile=fopen(globconf.argfile,"r");
      if (argfile==NULL)
	return LTBM_ERR_IO;

      err=0;
      while ((!feof(argfile)) && (err>=0))
	{
	  err=read_word(argfile,word);
	  if (err>0)
	    {
	      /* got a valid word */

	      switch (level)
		{
		case 0:
		  nodetype=-1;
		  if (strcmp(word,"root")==0)
		    nodetype=0;
		  if (strcmp(word,"neighbor")==0)
		    nodetype=1;
		  if (nodetype<0)
		    {
		      fprintf(stderr,"Parsing failed: unknown node type %s\n", word);
		      return LTBM_ERR_PARSE;
		    }
		  level=1;
		  break;
		case 1:
		  ranktype=-1;
		  if (strcmp(word,"rank")==0)
		    ranktype=0;
		  if (strcmp(word,"host")==0)
		    ranktype=1;
		  if (strcmp(word,"node")==0)
		    ranktype=2;
		  if (ranktype<0)
		    {
		      fprintf(stderr,"Parsing failed: unknown rank type %s\n", word);
		      return LTBM_ERR_PARSE;
		    }
		  level=2;
		  break;
		case 2:
		  if (ranktype==1)
		    {
		      newrank=convert_rank(word);
		      if (newrank<0)
			return newrank;
		      newrank=node_list[newrank];
		    }
		  else
		    {
		      if (ranktype==0)
			{
			  newrank=atoi(word);
			  if ((newrank<0) || (newrank>topology.size))
			    {
			      fprintf(stderr,"Parsing failed: illegal rank number %i\n",newrank);
			      return LTBM_ERR_PARSE;
			    }
			  newrank=node_list[newrank];
			}
		      else
			{
			  newrank=atoi(word);
			  if ((newrank<0) || (newrank>topology.numnodes))
			    {
			      fprintf(stderr,"Parsing failed: illegal node number %i\n",newrank);
			      return LTBM_ERR_PARSE;
			    }
			}
		    }

		  switch (nodetype)
		    {
		    case 0:
		      if (rootdone)
			{
			  fprintf(stderr,"Center specified twice\n");
			  return LTBM_ERR_PARSE;
			}
		      else
			{
			  globconf.rootnode=newrank;
			  globconf.roottask=-1;
			  rootdone=1;
			}
		      break;
		    case 1:
		      if (globconf.num_neighbor==MAXNODELIST)
			{			
			  fprintf(stderr,"Too many Neighbor Nodes\n");
			  return LTBM_ERR_PARSE;
			}
		      globconf.neighbor_list[globconf.num_neighbor]=newrank;
		      globconf.num_neighbor++;
		      break;
		    }
		  level=0;
		  break;
		}
	    }
	}
      if (err<0)
	return LTBM_ERR_IO;

      if (level!=0)
	{
	  fprintf(stderr,"Parsing failed: incomplete line\n");
	  return LTBM_ERR_PARSE;
	}
    }

  /* now we see what has been defined and what not */
  /* define useful defaults for the rest */

  if (globconf.rootnode<0)
    {
      if (globconf.roottask>=0)
	{
	  globconf.rootnode=node_list[globconf.roottask];
	}
      else
	{
	  err=nodelib_getDefaultRoot(&globconf.rootnode,topology.numnodes);
	  if (err!=LTBM_OK) return err;
	}
    }

  if (globconf.num_neighbor==0)
    {
      err=nodelib_getDefaultNeighbors(&globconf.num_neighbor,globconf.neighbor_list,
				      globconf.rootnode,topology.numnodes);
      if (err!=LTBM_OK) return err;
    }

  return LTBM_OK;
}


/*==================================================*/
/* Result Management */

int result_allocate(mpi_result_t **newres,mpi_config_t *config)
{
  *newres=(mpi_result_t*) malloc(sizeof(mpi_result_t));
  if (*newres==NULL) return LTBM_ERR_MEM;

  (*newres)->results=(double*) malloc(globconf.msgsizes*sizeof(double));
  if ((*newres)->results==NULL) return LTBM_ERR_MEM;

  printf("Starting test with %i root tasks, neighbors %i,allocation %s, overload %i, and test %s\n",
	 config->rootnum,config->neighbors,
	 linkconfigurations[config->linkconfig].name,
	 config->overload,
	 mpitests[config->mpitest].name);
  fflush(stdout);

  (*newres)->config=*config;
  return LTBM_OK;
}


int result_addpoint(mpi_result_t *result,int cnt, double size, bmtime_t resulttime)
{
  double bw;

  bw=(((double) size)/(1024*1024))/BMTIME_TO_SEC(resulttime);
  result->results[cnt]=bw;

  if (globconf.verbose)
    {
      printf("\tDone with msg size %7i: %f MB/s\n",msgsize_list[cnt],bw);
      fflush(stdout);
    }

  return LTBM_OK;
}


int result_add(mpi_result_t *result)
{
  int i;
  double maxbw;
  int maxsize,halfsize;

  maxbw=0;
  halfsize=-1;
  maxsize=-1;
  for (i=0; i<globconf.msgsizes; i++)
    {
      if (maxbw<result->results[i])
	{
	  maxbw=result->results[i];
	  maxsize=i;
	}
    }

  halfsize=0;
  while (result->results[halfsize]<maxbw/2)
    halfsize++;

  result->maxbw=maxbw;
  result->maxsize=maxsize;
  result->halfsize=halfsize;

  if (globconf.verbose)
    {
      printf("Test done - max bw %f at %i / half bw at %i\n\n",
	     maxbw,msgsize_list[maxsize],msgsize_list[halfsize]);
      fflush(stdout);
    }

  result->next=result_list;
  result_list=result;
  return LTBM_OK;
}


int result_print()
{
  int start,i;
  mpi_result_t *ptr;
  double *bwres,n_bwres;
  char **mpitres,**linkres,*n_mpitres,*n_linkres;
  int *neighs,n_tasks,*overload,n_overload,n_neighs;

  bwres=(double*) malloc(topology.node_maxwidth*sizeof(double));
  if (bwres==NULL) return LTBM_ERR_MEM;
  mpitres=(char**) malloc(topology.node_maxwidth*sizeof(char*));
  if (mpitres==NULL) return LTBM_ERR_MEM;
  linkres=(char**) malloc(topology.node_maxwidth*sizeof(char*));
  if (linkres==NULL) return LTBM_ERR_MEM;
  overload=(int*) malloc(topology.node_maxwidth*sizeof(int));
  if (overload==NULL) return LTBM_ERR_MEM;
  neighs=(int*) malloc(topology.node_maxwidth*sizeof(int));
  if (neighs==NULL) return LTBM_ERR_MEM;

  for (i=globconf.mintask; i<=globconf.maxtask; i++)
    {
      start=1;
      bwres[i-1]=-1;
      ptr=result_list;
      while (ptr!=NULL)
	{
	  if (ptr->config.rootnum==i)
	    {
	      if (start)
		{
		  printf("\n\nResults for %i task on the root (with %i cores used)\n\n",
			 i,globconf.maxtask);
		  start=0;
		}
	      printf("\t %i neighbors - test: %s - links: %s (overload %i) = BW %f (at msg size #%i -> %i)\n",
		     ptr->config.neighbors,
		     mpitests[ptr->config.mpitest].name,
		     linkconfigurations[ptr->config.linkconfig].name,
		     ptr->config.overload,
		     ptr->maxbw,ptr->maxsize,msgsize_list[ptr->maxsize]);
	      if (ptr->maxbw>bwres[i-1]) 
		{
		  bwres[i-1]=ptr->maxbw;
		  mpitres[i-1]=mpitests[ptr->config.mpitest].name;
		  linkres[i-1]=linkconfigurations[ptr->config.linkconfig].name;
		  overload[i-1]=ptr->config.overload;
		  neighs[i-1]=ptr->config.neighbors;
		}
	    }
	  ptr=ptr->next;
	}
    }

  printf("\n\nNode scaling results\n\n");

  for (i=globconf.mintask; i<=globconf.maxtask; i++)
    {
      printf("\t%i task on root: ",i);
      if (bwres[i-1]<0)
	printf("not run\n");
      else
	printf("%f MB/s (%s/%s, OL %i, %i neighbors)\n",bwres[i-1],mpitres[i-1],
	       linkres[i-1],overload[i-1],neighs[i-1]);
    }

  printf("\n\nNeighbor scaling results\n\n");

  n_mpitres=NULL;
  n_linkres=NULL;
  n_tasks=0;
  n_overload=0;
  for (i=globconf.minneigh; i<=globconf.maxneigh; i++)
    {
      n_bwres=-1;
      ptr=result_list;
      while (ptr!=NULL)
	{
	  if (ptr->config.neighbors==i)
	    {
	      if (ptr->maxbw>n_bwres) 
		{
		  n_bwres=ptr->maxbw;
		  n_mpitres=mpitests[ptr->config.mpitest].name;
		  n_linkres=linkconfigurations[ptr->config.linkconfig].name;
		  n_tasks=ptr->config.rootnum;
		  n_overload=ptr->config.overload;
		}
	    }
	  ptr=ptr->next;
	}
      printf("\t%i Neighbors: ",i);
      if (n_bwres<0)
	printf("not run\n");
      else
	printf("%f MB/s (%s/%s, OL %i, %i tasks)\n",n_bwres,n_mpitres,n_linkres,n_overload,n_tasks);
    }

  printf("\n\nOverload scaling results\n\n");

  n_mpitres=NULL;
  n_linkres=NULL;
  n_tasks=0;
  n_neighs=0;
  for (i=globconf.minoverload; i<=globconf.maxoverload; i++)
    {
      n_bwres=-1;
      ptr=result_list;
      while (ptr!=NULL)
	{
	  if (ptr->config.overload==i)
	    {
	      if (ptr->maxbw>n_bwres) 
		{
		  n_bwres=ptr->maxbw;
		  n_mpitres=mpitests[ptr->config.mpitest].name;
		  n_linkres=linkconfigurations[ptr->config.linkconfig].name;
		  n_tasks=ptr->config.rootnum;
		  n_neighs=ptr->config.neighbors;
		}
	    }
	  ptr=ptr->next;
	}
      printf("\tOverload = %i: ",i);
      if (n_bwres<0)
	printf("not run\n");
      else
	printf("%f MB/s (%s/%s, Neighbors %i, %i tasks)\n",n_bwres,n_mpitres,n_linkres,n_neighs,n_tasks);
    }
  printf("\n");

  free(overload);
  free(neighs);
  free(linkres);
  free(mpitres);
  free(bwres);

  return LTBM_OK;
}


/*==================================================*/
/* Benchmark communication routines */

/*-----------------------------------------------*/
/* send only */

int benchmark_sendonly(int links, int *linklist, int msgsize, int repeat, void* buf, bmtime_t *result, MPI_Comm comm)
{
  bmtime_t time_start,time_end;
  int i,l,err;

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_start=bmtime_getTime();

  for (i=0; i<repeat; i++)
    {
      for (l=0; l<links; l++)
	{
	  err=MPI_Send(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,MPI_COMM_WORLD);
	  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	}
    }

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_end=bmtime_getTime();
  *result=BMTIME_DIFF(time_start,time_end);
  return LTBM_OK;
}


int benchmark_recvonly(int links, int *linklist, int msgsize, int repeat, void* buf, bmtime_t *result, MPI_Comm comm)
{
  bmtime_t time_start,time_end;
  int i,l,err;
  MPI_Status status;

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_start=bmtime_getTime();

  for (i=0; i<repeat; i++)
    {
      for (l=0; l<links; l++)
	{
	  err=MPI_Recv(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,MPI_COMM_WORLD,&status);
	  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	}
    }

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_end=bmtime_getTime();
  *result=BMTIME_DIFF(time_start,time_end);
  return LTBM_OK;
}


/*-----------------------------------------------*/
/* isend only / fixed iterations */

int benchmark_isendonly(int links, int *linklist, int msgsize, int repeat, void* buf, bmtime_t *result, MPI_Comm comm)
{
  bmtime_t time_start,time_end;
  int i,l,err;
  MPI_Request *reqlist;
  MPI_Status *statuslist;

  reqlist=(MPI_Request*)malloc(links*sizeof(MPI_Request));
  if (reqlist==NULL) return LTBM_ERR_MEM;

  statuslist=(MPI_Status*)malloc(links*sizeof(MPI_Status));
  if (statuslist==NULL) return LTBM_ERR_MEM;

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_start=bmtime_getTime();

  for (i=0; i<repeat; i++)
    {
      for (l=0; l<links; l++)
	{
	  err=MPI_Isend(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,MPI_COMM_WORLD,&(reqlist[l]));
	  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	}
      err=MPI_Waitall(links,reqlist,statuslist);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
    }

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_end=bmtime_getTime();
  *result=BMTIME_DIFF(time_start,time_end);
  free(reqlist);
  free(statuslist);
  return LTBM_OK;
}


/*-----------------------------------------------*/
/* irecv only / fixed iterations */

int benchmark_irecvonly(int links, int *linklist, int msgsize, int repeat, void* buf, bmtime_t *result, MPI_Comm comm)
{
  bmtime_t time_start,time_end;
  int i,l,err;
  MPI_Request *reqlist;
  MPI_Status *statuslist;

  reqlist=(MPI_Request*)malloc(links*sizeof(MPI_Request));
  if (reqlist==NULL) return LTBM_ERR_MEM;

  statuslist=(MPI_Status*)malloc(links*sizeof(MPI_Status));
  if (statuslist==NULL) return LTBM_ERR_MEM;

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_start=bmtime_getTime();

  for (i=0; i<repeat; i++)
    {
      for (l=0; l<links; l++)
	{
	  err=MPI_Irecv(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,MPI_COMM_WORLD,&(reqlist[l]));
	  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	}
      err=MPI_Waitall(links,reqlist,statuslist);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
    }

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_end=bmtime_getTime();
  *result=BMTIME_DIFF(time_start,time_end);
  free(reqlist);
  free(statuslist);
  return LTBM_OK;
}


/*-----------------------------------------------*/
/* isend only / interleaved iterations */

int benchmark_isendonly_il(int links, int *linklist, int msgsize, int repeat, void* buf, bmtime_t *result, MPI_Comm comm)
{
  bmtime_t time_start,time_end;
  int l,err,reqdone;
  int *count,done,*statind;
  MPI_Request *reqlist;

  reqlist=(MPI_Request*)malloc(links*sizeof(MPI_Request));
  if (reqlist==NULL) return LTBM_ERR_MEM;

  count=(int*)malloc(links*sizeof(int));
  if (count==NULL) return LTBM_ERR_MEM;

  statind=(int*)malloc(links*sizeof(int));
  if (statind==NULL) return LTBM_ERR_MEM;

  for (l=0; l<links; l++)
    count[l]=repeat;
  done=links;

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_start=bmtime_getTime();

  for (l=0; l<links; l++)
    {
      err=MPI_Isend(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,MPI_COMM_WORLD,&(reqlist[l]));
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
    }      

  while (done>0)
    {
      err=MPI_Waitsome(links,reqlist,&reqdone,statind,MPI_STATUSES_IGNORE);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
      
      for (l=0; l<reqdone; l++)
	{
	  count[statind[l]]--;
	  if (count[statind[l]]==0)
	    done--;
	  else
	    {
	      err=MPI_Isend(buf,msgsize,MPI_CHAR,linklist[statind[l]],BM_TAG,MPI_COMM_WORLD,&(reqlist[statind[l]]));
	      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;	
	    }
	}
    }

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_end=bmtime_getTime();
  *result=BMTIME_DIFF(time_start,time_end);
  free(reqlist);
  free(count);
  free(statind);
  return LTBM_OK;
}


/*-----------------------------------------------*/
/* irecv only / interleaved iterations */

int benchmark_irecvonly_il(int links, int *linklist, int msgsize, int repeat, void* buf, bmtime_t *result, MPI_Comm comm)
{
  bmtime_t time_start,time_end;
  int l,err,reqdone;
  int *count,done,*statind;
  MPI_Request *reqlist;

  reqlist=(MPI_Request*)malloc(links*sizeof(MPI_Request));
  if (reqlist==NULL) return LTBM_ERR_MEM;

  count=(int*)malloc(links*sizeof(int));
  if (count==NULL) return LTBM_ERR_MEM;

  statind=(int*)malloc(links*sizeof(int));
  if (statind==NULL) return LTBM_ERR_MEM;

  for (l=0; l<links; l++)
    count[l]=repeat;
  done=links;

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_start=bmtime_getTime();

  for (l=0; l<links; l++)
    {
      err=MPI_Irecv(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,MPI_COMM_WORLD,&(reqlist[l]));
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
    }      

  while (done>0)
    {
      err=MPI_Waitsome(links,reqlist,&reqdone,statind,MPI_STATUSES_IGNORE);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
      
      for (l=0; l<reqdone; l++)
	{
	  count[statind[l]]--;
	  if (count[statind[l]]==0)
	    done--;
	  else
	    {
	      err=MPI_Irecv(buf,msgsize,MPI_CHAR,linklist[statind[l]],BM_TAG,MPI_COMM_WORLD,&(reqlist[statind[l]]));
	      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;	
	    }
	}
    }

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_end=bmtime_getTime();
  *result=BMTIME_DIFF(time_start,time_end);
  free(reqlist);
  free(count);
  free(statind);
  return LTBM_OK;
}


/*-----------------------------------------------*/
/* isend/irecv / fixed iterations */

int benchmark_isendrecv(int links, int *linklist, int msgsize, int repeat, void* buf, bmtime_t *result, MPI_Comm comm)
{
  bmtime_t time_start,time_end;
  int i,l,err;
  MPI_Request *reqlist;

  reqlist=(MPI_Request*)malloc(2*links*sizeof(MPI_Request));
  if (reqlist==NULL) return LTBM_ERR_MEM;

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_start=bmtime_getTime();

  for (i=0; i<repeat/2; i++)
    {
      for (l=0; l<links; l++)
	{
	  err=MPI_Isend(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,MPI_COMM_WORLD,&(reqlist[l]));
	  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	}
      for (l=0; l<links; l++)
	{
	  err=MPI_Irecv(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,MPI_COMM_WORLD,&(reqlist[l+links]));
	  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	}
      err=MPI_Waitall(2*links,reqlist,MPI_STATUSES_IGNORE);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
    }

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_end=bmtime_getTime();
  *result=BMTIME_DIFF(time_start,time_end);
  free(reqlist);
  return LTBM_OK;
}


/*-----------------------------------------------*/
/* isend/irecv only / interleaved iterations */

int benchmark_isendrecv_il(int links, int *linklist, int msgsize, int repeat, void* buf, bmtime_t *result, MPI_Comm comm)
{
  bmtime_t time_start,time_end;
  int l,err,reqdone;
  int *count,done,*statind;
  MPI_Request *reqlist;

  reqlist=(MPI_Request*)malloc(2*links*sizeof(MPI_Request));
  if (reqlist==NULL) return LTBM_ERR_MEM;

  count=(int*)malloc(2*links*sizeof(int));
  if (count==NULL) return LTBM_ERR_MEM;

  statind=(int*)malloc(2*links*sizeof(int));
  if (statind==NULL) return LTBM_ERR_MEM;

  for (l=0; l<2*links; l++)
    count[l]=repeat/2;
  done=links*2;

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_start=bmtime_getTime();

  for (l=0; l<links; l++)
    {
      err=MPI_Isend(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,MPI_COMM_WORLD,&(reqlist[l]));
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
    }      
  for (l=0; l<links; l++)
    {
      err=MPI_Irecv(buf,msgsize,MPI_CHAR,linklist[l],BM_TAG,MPI_COMM_WORLD,&(reqlist[l+links]));
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
    }      

  while (done>0)
    {
      err=MPI_Waitsome(2*links,reqlist,&reqdone,statind,MPI_STATUSES_IGNORE);
      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
      
      for (l=0; l<reqdone; l++)
	{
	  count[statind[l]]--;
	  if (count[statind[l]]==0)
	    done--;
	  else
	    {
	      if (statind[l]<links)
		{
		  err=MPI_Isend(buf,msgsize,MPI_CHAR,linklist[statind[l]],BM_TAG,MPI_COMM_WORLD,&(reqlist[statind[l]]));
		  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;	
		}
	      else
		{
		  err=MPI_Irecv(buf,msgsize,MPI_CHAR,linklist[statind[l]-links],BM_TAG,MPI_COMM_WORLD,&(reqlist[statind[l]]));
		  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;	
		}
	    }
	}
    }

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
  time_end=bmtime_getTime();
  *result=BMTIME_DIFF(time_start,time_end);
  free(reqlist);
  free(count);
  free(statind);
  return LTBM_OK;
}


/*-----------------------------------------------*/
/* driver */

/* run a benchmark node configuration across all msgsizes and 
   communication routine options */

int bm_driver(mpi_config_t *config, int links, int *linklist, int repeat, MPI_Comm comm)
{
  double size;
  int err;
  bmtime_t locresult,result;
  void *buf;
  long msgsize;
  int msgcnt;
  mpi_result_t *test_result;

  buf=malloc(globconf.maxsize);
  if (buf==NULL) return LTBM_ERR_MEM;

  for (config->mpitest=0; config->mpitest<MAXMPITESTS; config->mpitest++)
    {
      if (test_requested[config->mpitest])
	{
	  if (globconf.roottask==topology.rank)
	    {
	      err=result_allocate(&test_result,config);
	      if (err!=LTBM_OK) return err;
	    }
	  
	  err=MPI_Barrier(comm);  
	  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	  msgsize=globconf.minsize;
	  msgcnt=0;
	  
	  do
	    {
	      size=((double)msgsize)*((double)links)*((double)repeat)*((double)config->rootnum);
	      
	      err=MPI_Barrier(comm);
	      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	      
	      if (globconf.rootnode==topology.node)
		{
		  err=mpitests[config->mpitest].rootfct(links,linklist,msgsize,repeat,buf,&locresult,comm);
		  if (err!=LTBM_OK) return err;
		}
	      else
		{
		  err=mpitests[config->mpitest].partnerfct(links,linklist,msgsize,repeat,buf,&locresult,comm);
		  if (err!=LTBM_OK) return err;
		  locresult=0;
		}
	      
	      err=MPI_Barrier(comm);
	      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	      
	      /* gather results */
	      
	      err=MPI_Allreduce(&locresult,&result,1,BMTIME_MPITYPE,MPI_MAX,comm);
	      if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;
	      
	      /* store results on root node / local rank 0 */
	      
	      if (globconf.roottask==topology.rank)
		{
		  err=result_addpoint(test_result,msgcnt,size,result);
		  if (err!=LTBM_OK) return err;
		}
	      msgsize*=globconf.multsize;
	      msgcnt++;
	    }
	  while (msgsize<=globconf.maxsize);
  
	  if (globconf.roottask==topology.rank)
	    {
	      err=result_add(test_result);
	      if (err!=LTBM_OK) return err;
	    }
	}
    }

  err=MPI_Barrier(comm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;

  free(buf);
  return LTBM_OK;
}


/*==================================================*/
/* Create link list and run tests */

int create_and_run_configuration(mpi_config_t *config)
{
  int i,err,numneighbortasks,numneighbornodes,j,group;
  int participant,links=-1,*linklist=NULL,*linklist_copy=NULL;
  int *participants,partrank;
  MPI_Comm bmcomm;
  int task,scan;

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;

  if (globconf.num_neighbor<config->neighbors)
    {
      fprintf(stderr,"Need at least as many neighbor nodes as communication neighbors\n");
      return LTBM_ERR_ARG;
    }

  partrank=-1;
  numneighbortasks=config->neighbors*config->rootnum;
  participants=(int*) malloc(sizeof(int)*numneighbortasks);
  
  task=0;
  scan=0;
  group=0;
  numneighbornodes=0;
  while (task<numneighbortasks)
    {
      if (numneighbornodes>=globconf.num_neighbor)
	{
	  fprintf(stderr,"Not enough neighbor nodes to complete tests (have %i)\n",task);
	  return LTBM_ERR_ARG;
	}	      
      if (task_list[globconf.neighbor_list[numneighbornodes]*topology.node_maxwidth+scan]>=0)
	{
	  /* let's use it */
	  participants[task]=task_list[globconf.neighbor_list[numneighbornodes]*topology.node_maxwidth+scan];
	  if (topology.rank==participants[task])
	    partrank=task;
	  task++;
	}

      numneighbornodes++;
      if ((numneighbornodes>=(group+1)*(config->neighbors)) ||
	  (numneighbornodes>=globconf.num_neighbor))
	{
	  /* next thread level */
	  scan++;
	  if (scan>=config->overload)
	    {
	      /* all overload options used, try next set */
	      scan=0;
	      group++;
	    }
	  numneighbornodes=group*config->neighbors;
	}
    }


  switch (topology.role)
    {
    case LTBM_NODE_ROLE_ROOT:
      if (topology.local_rank<config->rootnum)
	{
	  /* task on the root node that we want to use */
	  participant=1;
	}
      else
	{
	  /* task on the root node that we don't want to use */
	  participant=0;
	}
      break;
    case LTBM_NODE_ROLE_NEIGHBOR:
      if (partrank>=0)
	participant=1;
      else
	participant=0;
      break;
    case LTBM_NODE_ROLE_NONE:
    default:
      participant=0;
    }
  
  err=MPI_Comm_split(MPI_COMM_WORLD,participant,topology.rank,&bmcomm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;

  if (participant)
    {
      /* create link list */

      switch (topology.role)
	{
	case LTBM_NODE_ROLE_ROOT:
	  {
	    linklist=(int*)malloc(sizeof(int)*config->neighbors);
	    if (linklist==NULL) return LTBM_ERR_MEM;
	    linklist_copy=(int*)malloc(sizeof(int)*config->neighbors);
	    if (linklist_copy==NULL) return LTBM_ERR_MEM;
	    links=config->neighbors;

	    switch (config->linkconfig)
	      {
	      case LINKCONF_BLOCK:
		{
		  for (i=0; i<config->neighbors; i++)
		    linklist[i]=participants[config->neighbors*topology.local_rank+i];
		  break;
		}
	      case LINKCONF_ROUND:
		{
		  for (i=0; i<config->neighbors; i++)
		    linklist[i]=participants[config->rootnum*i+topology.local_rank];
		  break;
		}
	      default:
		return LTBM_ERR_ARG;
		break;
	      }

	    break;
	  }
	case LTBM_NODE_ROLE_NEIGHBOR:
	  {
	    linklist=(int*)malloc(sizeof(int)*1);
	    if (linklist==NULL) return LTBM_ERR_MEM;
	    links=1;

	    switch (config->linkconfig)
	      {
	      case LINKCONF_BLOCK:
		{
		  *linklist=task_list[globconf.rootnode*topology.node_maxwidth+
				      partrank/config->neighbors];
		  break;
		}
	      case LINKCONF_ROUND:
		{
		  *linklist=task_list[globconf.rootnode*topology.node_maxwidth+
				      partrank%config->rootnum];
		  break;
		}
	      default:
		return LTBM_ERR_ARG;
		break;
	      }

	    break;
	  }
	}

      if ((topology.node==globconf.rootnode) &&
	  (globconf.verbose))
	{
	  if (topology.local_rank==0)
	    {
	      printf("\n");
	      printf("Starting the following topology:");

	      for (i=0; i<config->rootnum; i++)
		{
		  if (i==0)
		    memcpy(linklist_copy,linklist,config->neighbors*sizeof(int));
		  else
		    MPI_Recv(linklist_copy,config->neighbors,MPI_INT,i,42,topology.comm_local,MPI_STATUS_IGNORE);

		  printf("  Root task %2i ->",i);
		  for (j=0; j<config->neighbors; j++)
		    printf(" %i (on %i)",linklist_copy[j],node_list[linklist_copy[j]]);
		  printf("\n");
		}
	      printf("\n");
	      fflush(stdout);
	    }
	  else
	    {
	      MPI_Send(linklist,config->neighbors,MPI_INT,0,42,topology.comm_local);
	    }
	}
      
      err=bm_driver(config, links, linklist, globconf.repeats, bmcomm);
      if (err!=LTBM_OK) return err;

      free(linklist);
      free(linklist_copy);
    }

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;

  err=MPI_Comm_free(&bmcomm);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;

  free(participants);

  return LTBM_OK;
}


/*==================================================*/
/* Loop over all node configurations and initiate benchmark runs */

int run_all_benchmarks()
{
  int err;
  mpi_config_t config;
  
  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) return LTBM_ERR_MPI;

  for (config.rootnum=globconf.mintask; config.rootnum<=globconf.maxtask; config.rootnum++)
    {
      for (config.neighbors=globconf.minneigh; config.neighbors<=globconf.maxneigh; config.neighbors++)
	{
	  for (config.overload=globconf.minoverload; config.overload<=globconf.maxoverload; config.overload++)
	    {
	      for (config.linkconfig=0; config.linkconfig<MAXMPILINKCONFIGS; config.linkconfig++)
		{
		  err=create_and_run_configuration(&config);
		  if (err!=LTBM_OK) return err;
		}
	    }
	}
    }

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

  if (topology.size<2)
    throw_error(LTBM_ERR_TOOFEWNODES);

#ifdef PRINT_ENV
  /* Print environment as part of Sequoia SOW MPI requirements */
  extern void printEnv(void);
  if (topology.rank == 0) { printEnv(); }
#endif

  /* read arguments */

  err=process_cmdline(&argc,&argv);
  if (err!=LTBM_OK) throw_error(err);


  /* Initialize MPI */

  err=top_setupMPI(&argc,&argv);
  if (err!=LTBM_OK) throw_error(err);


  if (topology.rank==0)
    {
      /* read the configuration file and set defaults */
      
      err=parse_configfile();
      if (err!=LTBM_OK) throw_error(err);
    }      
      

  /* Setup Topology and Check it */

  err=top_setupTop();
  if (err!=LTBM_OK) throw_error(err);


  /* Print configuration (if req) */

  if (topology.rank==0)
    {
      printf("Linktest v%d.%d.%d\n", VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
      if (globconf.verbose)
	{
	  printf("Configuration on Node 0\n");
	  diag_printTop();
	  printf("\n");
	}
    }

  
  /* Setup complete - barrier */
  /* root task does all I/O from now on */

  if (topology.rank==0)
    {
      fflush(stdout);
      fflush(stderr);
    }

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);

  sleep(1);

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);


  /* Print configuration (if req) */

  if (topology.rank==globconf.roottask)
    {
      if (globconf.verbose)
	{
	  printf("Configuration on task %i / node %i\n",globconf.roottask,globconf.rootnode);
	  diag_printTop();
	  printf("\n");
	}
    }

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);


  /* call the actual benchmark */

  err=run_all_benchmarks();
  if (err!=LTBM_OK) throw_error(LTBM_ERR_MPI);


  /* Benchmark complete - barrier */

  err=MPI_Barrier(MPI_COMM_WORLD);
  if (err!=MPI_SUCCESS) throw_error(LTBM_ERR_MPI);


  /* Print results */

  if (topology.rank==globconf.roottask)
    {
      diag_printTop();
      err=result_print();
      if (err!=LTBM_OK) throw_error(err);
    }

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
