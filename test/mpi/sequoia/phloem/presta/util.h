/*
    This work was performed under the auspices of the U.S. Department of
    Energy by Lawrence Livermore National Laboratory under Contract
    DE-AC52-07NA27344.

    DISCLAIMER

    This work was prepared as an account of work sponsored by an agency of
    the United States government. Neither the United States government nor
    Lawrence Livermore National Security, LLC, nor any of their employees
    makes any warranty, expressed or implied, or assumes any legal
    liability or responsibility for the accuracy, completeness, or
    usefulness of any information, apparatus, product, or process
    disclosed, or represents that its use would not infringe privately
    owned rights. Reference herein to any specific commercial product,
    process, or service by trade name, trademark, manufacturer, or
    otherwise does not necessarily constitute or imply its endorsement,
    recommendation, or favoring by the United States government or Lawrence
    Livermore National Security, LLC. The views and opinions of authors
    expressed herein do not necessarily state or reflect those of the
    United States government or Lawrence Livermore National Security, LLC,
    and shall not be used for advertising or product endorsement purposes.

    NOTIFICATION OF COMMERCIAL USE

    Commercialization of this product is prohibited without notifying the
    Department of Energy (DOE) or Lawrence Livermore National Laboratory
    (LLNL).

    UCRL-CODE-2001-028


    util.h

*/

#ifndef _UTIL_H
#define _UTIL_H

#include <limits.h>

#define MESS_START_DEF 32
#define MESS_STOP_DEF 8388608
#define MESS_FACTOR_DEF 2
#define ALLOC_DEF 'b'
#define USE_BARRIER_DEF 0
#define PRINT_PAIRS_DEF 0
#define NEAREST_RANK_DEF 0
#define MIN_MESS_SIZE 0
#define MAX_MESS_SIZE INT_MAX
#define MB_SIZE 1000000
#define PRESTA_SEED 128

typedef struct
{

  int debugFlag;
  int iters;
  int samples;
  int messFactor;
  int messStart;
  int messStop;
  int printHostInfo;
  int printPairs;
  int printRankTime;
  int procsPerNode;
  int useBarrier;
  int useNearestRank;
  char *procFile;
  char *messFile;
  char allocPattern;
  int *procList;
  int *testCountList;
  int procListSize;
  int *messList;
  int messListSize;
  int *iterList;
  char *testList;
  int sumLocalBW;
  int verbose;
  int allTasksOnly;

} ARGSTRUCT;


typedef struct
{
  unsigned int tasks;
  unsigned int msize;
  unsigned int iters;
  unsigned int samples;
  unsigned int count;
  double min;
  double tot;
  double mean;
  double max;

} STATSTRUCT;

typedef enum
{ PRESTA_OP_P2P, PRESTA_OP_COLL } PRESTA_OP_TYPE;

extern int rank, wsize;
extern char *targetFile;
extern int *targetArray;
extern int targetListSize;
extern char *targetDirectory;
extern ARGSTRUCT argStruct;
extern int majorVersion;
extern int minorVersion;
extern int patchVersion;
extern char procSrcTitle[256];
extern int presta_check_data;
extern unsigned long long presta_data_err_total;
extern long long presta_global_data_err_total;

double getWtimeOh ( void );
void populateData ( char *, int );
void generic_barrier ( MPI_Comm );
void printTimingInfo ( void );
void printCommTargets ( int procsPerNode, int useNearestRank );
void listRankLocations ( void );
int createActiveComm ( int procs, int procsPerNode,
                       char allocPattern, int useNearestRank,
                       MPI_Comm * activeComm );
int isActiveProc ( MPI_Comm * currCom );
int processArgs ( int argc, char **argv );
void printActivePairs ( int procs, int procsPerNode,
                        char allocPattern, int useNearestRank );
int getTargetRank ( int rank, int procsPerNode, int useNearestRank );
void getPair ( int idx, int procsPerNode,
               char allocPattern, int useNearestRank, int *rank1,
               int *rank2 );
int validateProcCount ( int count, int minCount, int maxCount );
int validateMessageSize ( int messSize, int minSize, int maxSize );
int validateTarget ( int count, int minCount, int maxCount );
int getProcessList ( int minVal, int maxVal,
                     int *listSize, char *listFile, int **list );
int getMessageList ( int minVal, int maxVal,
                     int *listSize, char *listFile, int **list );
int getList ( int ( *validateFunc ) ( int, int, int ), int minVal, int maxVal,
              int *listSize, char *listFile, int **list );
int get2entryList ( int ( *validateFunc ) ( int, int, int ), int minVal,
                    int maxVal, int *listSize, char *listFile, int **list1,
                    int **list2 );
int createSeqIntArray ( int start, int stop, int factor, int **array,
                        int *arraySize );
void prestaDebug ( char *fmt, ... );
void prestaRankDebug ( int targRank, char *fmt, ... );
void prestaRankPrint ( int targRank, char *fmt, ... );
void prestaWarn ( char *fmt, ... );
void prestaAbort ( char *fmt, ... );
int getNextTargetFile ( void );
int numcmp ( const void *num1, const void *num2 );
int validateRank ( int vrank, int minRank, int maxRank );
int getTargetList ( void );
void printGlobGroup ( MPI_Comm currComm );
char *getTimeStr ( void );
void set_data_values ( long long buf_size, void *buf_ptr );
long long check_data_values ( int data_count, void *in_buf, void *valid_buf,
                              MPI_Datatype data_type,
                              PRESTA_OP_TYPE op_type );
void init_stats ( STATSTRUCT * s, unsigned int t, unsigned int m,
                  unsigned int i, unsigned int p );
void update_stats ( STATSTRUCT * s, double r );
extern void printTestNames ( void );
void printSeparator ( void );
void printEnv(void );

extern char *executableName;
#endif
