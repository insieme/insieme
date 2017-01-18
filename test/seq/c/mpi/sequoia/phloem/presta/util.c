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


    util.c

    Provides functions common to benchmarks.

*/


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <assert.h>
#include <time.h>
#include <values.h>
#include <mpi.h>
#include "util.h"

#define GENERIC_BARRIER_TAG 1
#define FILE_DELIM " "

char *targetFile = NULL;
int *targetList = NULL;
int targetListSize = 0;
char *targetDirectory = NULL;
static DIR *dp = NULL;

int majorVersion = 1;
int minorVersion = 0;
int patchVersion = 0;

int presta_check_data = 0;
unsigned long long presta_data_err_total = 0;
long long presta_global_data_err_total = 0;

ARGSTRUCT argStruct;

/*  Global variables for MPI_COMM_WORLD rank and size  */
int rank, wsize;

char procSrcTitle[256] = "";
char *executableName = NULL;

extern char** environ;


/*
    If the Wtick granularity for Wtime is coarser than the
    overhead of calling MPI_Wtime, then approximate the MPI_Wtime
    overhead by counting calls within one tick.
*/

double
getWtimeOh (  )
{
  double start, stop, diff;
  int count;

  count = 1;
  start = MPI_Wtime (  );
  stop = MPI_Wtime (  );
  diff = stop - start;

  if ( diff == 0 || MPI_Wtime (  ) == stop )
  {
    stop = MPI_Wtime (  );
    while ( ( start = MPI_Wtime (  ) ) == stop );
    while ( ( stop = MPI_Wtime (  ) ) == start )
      count++;
    return ( stop - start ) / count;
  }

  return stop - start;
}


/* This barrier implementation is taken from the Sphinx MPI benchmarks
   and is based directly on the MPICH barrier implementation...        */

void
generic_barrier ( MPI_Comm curcomm )
{
  int rank, size, N2_prev, surfeit;
  int d, dst, src, temp;
  MPI_Status status;

  /* Intialize communicator size */
  ( void ) MPI_Comm_size ( curcomm, &size );

  /* If there's only one member, this is trivial */
  if ( size > 1 )
  {

    MPI_Comm_rank ( curcomm, &rank );

    for ( temp = size, N2_prev = 1;
          temp > 1; temp >>= 1, N2_prev <<= 1 ) /* NULL */ ;

    surfeit = size - N2_prev;

    /* Perform a combine-like operation */
    if ( rank < N2_prev )
    {
      if ( rank < surfeit )
      {

        /* get the fanin letter from the upper "half" process: */
        dst = N2_prev + rank;

        MPI_Recv ( ( void * ) 0, 0, MPI_INT, dst,
                   GENERIC_BARRIER_TAG, curcomm, &status );
      }

      /* combine on embedded N2_prev power-of-two processes */
      for ( d = 1; d < N2_prev; d <<= 1 )
      {
        dst = ( rank ^ d );

        MPI_Sendrecv ( ( void * ) 0, 0, MPI_INT, dst,
                       GENERIC_BARRIER_TAG, ( void * ) 0, 0, MPI_INT,
                       dst, GENERIC_BARRIER_TAG, curcomm, &status );
      }

      /* fanout data to nodes above N2_prev... */
      if ( rank < surfeit )
      {
        dst = N2_prev + rank;
        MPI_Send ( ( void * ) 0, 0, MPI_INT, dst, GENERIC_BARRIER_TAG,
                   curcomm );
      }
    }
    else
    {
      /* fanin data to power of 2 subset */
      src = rank - N2_prev;
      MPI_Sendrecv ( ( void * ) 0, 0, MPI_INT, src, GENERIC_BARRIER_TAG,
                     ( void * ) 0, 0, MPI_INT, src, GENERIC_BARRIER_TAG,
                     curcomm, &status );
    }
  }
  return;
}


void
printCommTargets ( int procsPerNode, int useNearestRank )
{
  /*  The rationale behind this implementation is to keep memory
   *  requirements down to only use 3x MPI_MAX_PROCESSOR_NAME rather than 
   *  wsize*MPI_MAX_PROCESSOR_NAME  
   */
  char myProcName[MPI_MAX_PROCESSOR_NAME];
  char processProcName[MPI_MAX_PROCESSOR_NAME];
  char targetProcName[MPI_MAX_PROCESSOR_NAME];
  int nameSize, currtarg, mytarg, i;
  MPI_Status status;

  MPI_Get_processor_name ( myProcName, &nameSize );

  if ( rank != 0 )
  {
    currtarg = getTargetRank ( rank, procsPerNode, useNearestRank );

    /* Send target and hostname twice.  As process 0 loops through
     * ranks, it recieves the current rank hostname as well as the target
     * rank information.
     */
    MPI_Send ( &currtarg, 1, MPI_INT, 0, 0, MPI_COMM_WORLD );
    MPI_Send ( myProcName, MPI_MAX_PROCESSOR_NAME, MPI_CHAR,
               0, 0, MPI_COMM_WORLD );
    if ( currtarg >= 0 )
    {

      MPI_Send ( &currtarg, 1, MPI_INT, 0, 0, MPI_COMM_WORLD );
      MPI_Send ( myProcName, MPI_MAX_PROCESSOR_NAME, MPI_CHAR,
                 0, 0, MPI_COMM_WORLD );
    }
  }
  else
  {
    printf ( "\n" );

    mytarg = getTargetRank ( 0, procsPerNode, useNearestRank );

    /*  Receive the target process target rank  */
    MPI_Recv ( &currtarg, 1, MPI_INT, mytarg, 0, MPI_COMM_WORLD, &status );
    /*  Receive the current process hostname  */
    if ( currtarg >= 0 )
    {
      MPI_Recv ( targetProcName, MPI_MAX_PROCESSOR_NAME, MPI_CHAR, mytarg,
                 0, MPI_COMM_WORLD, &status );
    }
    printf ( "  Rank id %4d (%s) paired with rank id %4d (%s)\n",
             0, myProcName, mytarg, targetProcName );

    for ( i = 1; i < wsize; i++ )
    {
      /*  Receive the current process target rank  */
      MPI_Recv ( &currtarg, 1, MPI_INT, i, 0, MPI_COMM_WORLD, &status );
      /*  Receive the current process hostname  */
      MPI_Recv ( processProcName, MPI_MAX_PROCESSOR_NAME, MPI_CHAR, i,
                 0, MPI_COMM_WORLD, &status );

      if ( currtarg != 0 )
      {
        if ( currtarg >= 0 )
        {
          /*  Receive the target process target rank  */
          MPI_Recv ( &mytarg, 1, MPI_INT, currtarg, 0, MPI_COMM_WORLD,
                     &status );
          /*  Receive the current process hostname  */
          MPI_Recv ( targetProcName, MPI_MAX_PROCESSOR_NAME, MPI_CHAR,
                     currtarg, 0, MPI_COMM_WORLD, &status );
        }
      }
      else
        strcpy ( targetProcName, myProcName );

      if ( currtarg < 0 )
        strcpy ( targetProcName, "Not Used" );

      printf ( "  Rank id %4d (%s) paired with rank id %4d (%s)\n",
               i, processProcName, currtarg, targetProcName );
    }
  }
}



void
printGlobGroup ( MPI_Comm currComm )
{
  char myProcName[MPI_MAX_PROCESSOR_NAME];
  char processProcName[MPI_MAX_PROCESSOR_NAME];
  int nameSize, i, commSize;
  MPI_Status status;

  MPI_Get_processor_name ( myProcName, &nameSize );

  if ( rank != 0 )
  {
    if ( isActiveProc ( &currComm ) )
      MPI_Send ( myProcName, MPI_MAX_PROCESSOR_NAME, MPI_CHAR,
                 0, 0, currComm );
  }
  else
  {
    putchar ( '\n' );

    printf ( "Active Process Ranks\n" );
    printf ( "Rank id %4d (%s)\n", 0, myProcName );

    MPI_Comm_size ( currComm, &commSize );
    for ( i = 1; i < commSize; i++ )
    {
      /*  Receive the current process hostname  */
      MPI_Recv ( processProcName, MPI_MAX_PROCESSOR_NAME, MPI_CHAR, i,
                 0, currComm, &status );

      printf ( "Rank id %4d (%s) \n", i, processProcName );
    }
    putchar ( '\n' );
  }
}



void
listRankLocations (  )
{
  /* The rationale behind this implementation is to keep memory
   * requirements down to only use 2x MPI_MAX_PROCESSOR_NAME rather than 
   * wsize*MPI_MAX_PROCESSOR_NAME  
   * 
   * This is used for the global operations where we don't have the concept of
   * process pairs.
   */
  char procNameBuf[MPI_MAX_PROCESSOR_NAME];
  int nameSize, i;
  MPI_Status status;

  MPI_Get_processor_name ( procNameBuf, &nameSize );

  if ( rank != 0 )
  {
    MPI_Send ( procNameBuf, MPI_MAX_PROCESSOR_NAME, MPI_CHAR,
               0, 0, MPI_COMM_WORLD );
  }
  else
  {
    putchar ( '\n' );

    printf ( "Rank id %6d ran on %s\n", 0, procNameBuf );

    for ( i = 1; i < wsize; i++ )
    {
      /*  Receive the current process hostname  */
      MPI_Recv ( procNameBuf, MPI_MAX_PROCESSOR_NAME, MPI_CHAR, i, 0,
                 MPI_COMM_WORLD, &status );

      printf ( "Rank id %6d ran on %s\n", i, procNameBuf );
    }
    putchar ( '\n' );
  }
}


void
printTimingInfo (  )
{
  double wtick, wtoverhead;

  wtick = MPI_Wtick (  );
  wtoverhead = getWtimeOh (  );

  printf ( "  MPI_Wtick returns           %11.9f\n", wtick );
  printf ( "  MPI_Wtime overhead          %11.9f\n", wtoverhead );
  /*  TODO : Figure out how to address this.   */
#if 0
  if ( argStruct.sumLocalBW == 0 )
    printf ( "  Ticks for minimum sample    %11.0f\n", mintime / wtick );
#endif
}


void
getPair ( int idx, int procsPerNode,
          char allocPattern, int useNearestRank, int *rank1, int *rank2 )
{
  if ( allocPattern == 'c' && procsPerNode > 0 && wsize > 2 * procsPerNode )
  {

    if ( useNearestRank )
    {
      int calcval;

      calcval = idx * procsPerNode * 2;
      *rank1 = calcval % wsize + calcval / wsize;
    }
    else
    {
      int calcval;

      calcval = idx * procsPerNode;
      *rank1 = calcval % ( wsize / 2 ) + calcval / ( wsize / 2 );
    }

  }
  else
  {
    if ( useNearestRank )
    {
      if ( procsPerNode < 1 )
      {
        prestaWarn
          ( "processes per node is not defined, reverting to default process allocation\n" );
        *rank1 = idx;
      }
      else
        *rank1 = idx + ( idx / procsPerNode * procsPerNode );
    }
    else
      *rank1 = idx;
  }

  *rank2 = getTargetRank ( *rank1, procsPerNode, useNearestRank );
}


int
getTargetRank ( int rank, int procsPerNode, int useNearestRank )
{
  int target = -1;

  if ( targetList != NULL )
  {
    target = targetList[rank];
  }
  else if ( useNearestRank && procsPerNode > 0 )
  {
    if ( rank % ( procsPerNode * 2 ) >= procsPerNode )
      target = rank - procsPerNode;
    else
      target = rank + procsPerNode;
  }
  else
  {
    if ( rank < wsize / 2 )
      target = rank + wsize / 2;
    else
      target = rank - wsize / 2;
  }

  return target;
}


int
numcmp ( const void *num1, const void *num2 )
{
  return *( int * ) num1 - *( int * ) num2;
}


int
createActiveComm ( int procs, int procsPerNode,
                   char allocPattern, int useNearestRank,
                   MPI_Comm * activeComm )
{
  int *activeIds;
  int i;
  MPI_Group worldGroup, activeGroup;
  char *procFlags = NULL;
  int taskID1, taskID2;
  int idCnt = 0;

  MPI_Comm_group ( MPI_COMM_WORLD, &worldGroup );

  /*  Create Communicator of all active processes  */
  activeIds = ( int * ) malloc ( procs * sizeof ( int ) );
  procFlags = ( char * ) malloc ( wsize * sizeof ( char ) );

  if ( activeIds == NULL || procFlags == NULL )
    prestaAbort ( "Failed to allocate communicator setup space.\n" );

  memset ( procFlags, 0, wsize * sizeof ( char ) );

  i = 0;
  while ( idCnt < procs && i < procs )
  {
    if ( allocPattern == 'l' )  /* linear pattern used for collective tests  */
    {
      activeIds[i] = i;
      i++;
      idCnt++;
    }
    else
    {
      getPair ( i, procsPerNode, allocPattern, useNearestRank,
                &taskID1, &taskID2 );


      prestaDebug ( "i:%d  wsize:%d  procsPerNode:%d\n", i, wsize,
                    procsPerNode );
      prestaDebug ( "taskID1: %d   taskID2:  %d\n", taskID1, taskID2 );

      if ( taskID1 > -1 && taskID2 > -1 )
      {
        if ( procFlags[taskID1] == 0 )
        {
          activeIds[idCnt++] = taskID1;
          procFlags[taskID1] = 1;
        }

        if ( procFlags[taskID2] == 0 )
        {
          activeIds[idCnt++] = taskID2;
          procFlags[taskID2] = 1;
        }
      }
      i++;
    }
  }

  if ( idCnt % 2 != 0 )
    prestaAbort ( "Total number of processes is invalid!\n" );

  qsort ( activeIds, idCnt, sizeof ( int ), numcmp );

  MPI_Group_incl ( worldGroup, idCnt, activeIds, &activeGroup );
  MPI_Comm_create ( MPI_COMM_WORLD, activeGroup, activeComm );

  free ( activeIds );
  free ( procFlags );

  MPI_Group_free ( &worldGroup );
  MPI_Group_free ( &activeGroup );

  return idCnt;
}


void
printActivePairs ( int procs, int procsPerNode, char allocPattern,
                   int useNearestRank )
{
  int i, cp1, cp2;

  if ( rank == 0 )
  {
    if ( allocPattern == 'c' && procsPerNode > 0 && wsize > procsPerNode * 2 )
      printf ( "Current pairs, cyclic allocation\n" );
    else
      printf ( "Current pairs, block allocation\n" );

    for ( i = 0; i < procs / 2; i++ )
    {
      getPair ( i, procsPerNode, allocPattern, useNearestRank, &cp1, &cp2 );

      printf ( "  %5d:%5d\n", cp1, cp2 );
    }
  }
}


int
isActiveProc ( MPI_Comm * currComm )
{
  MPI_Group tempGroup;
  int grank;

  if ( currComm == NULL )
    prestaAbort ( "currComm is NULL in isActiveProc" );

  if ( *currComm == MPI_COMM_NULL )
    return 0;

  MPI_Comm_group ( *currComm, &tempGroup );

  MPI_Group_rank ( tempGroup, &grank );

  MPI_Group_free ( &tempGroup );

  if ( grank == MPI_UNDEFINED )
    return 0;
  else
    return 1;
}



int
validateProcCount ( int count, int minCount, int maxCount )
{
  int retval = 0;

  if ( count > 1 && count % 2 == 0 && count >= minCount && count <= maxCount )
  {
    retval = 1;
  }

  return retval;
}


int
validateRank ( int vrank, int minRank, int maxRank )
{
  int retval = 0;

  if ( ( vrank >= minRank && vrank <= maxRank ) || vrank == -1 )
  {
    retval = 1;
  }

  return retval;
}


int
validateMessageSize ( int messSize, int minSize, int maxSize )
{
  int retval = 0;

  if ( messSize >= minSize && messSize <= maxSize )
  {
    retval = 1;
  }

  return retval;
}


int
getProcessList ( int minVal, int maxVal,
                 int *listSize, char *listFile, int **list )
{
  return getList ( validateProcCount, minVal, maxVal, listSize, listFile,
                   list );
}


int
getTargetList ( void )
{
  return getList ( validateRank, 0, wsize, &targetListSize, targetFile,
                   &targetList );
}


int
getMessageList ( int minVal, int maxVal,
                 int *listSize, char *listFile, int **list )
{
  return getList ( validateMessageSize, minVal, maxVal,
                   listSize, listFile, list );
}


/*
 * getList - process rank 0 reads a file into an array and broadcasts the entire
 *           array to all processes.
 *
 * in :
 *      validateFunc   : function to validate input values
 *      minVal, maxVal : values to pass the validate function
 *            listFile : the name of the file to read
 * out :
 *      listSize, list : pointers to the resulting list and list size
 */

int
getList ( int ( *validateFunc ) ( int, int, int ), int minVal, int maxVal,
          int *listSize, char *listFile, int **list )
{
  FILE *fp = NULL;
  char lineBuf[1024];
  int currVal, retval = 1, arval;

  *listSize = 0;

  if ( rank == 0 )
  {
    if ( listFile != NULL )
    {
      if ( ( fp = fopen ( listFile, "r" ) ) != NULL )
      {
        *list = ( int * ) malloc ( 1024 * sizeof ( int ) );
        if ( *list == NULL )
        {
          fclose ( fp );
          retval = 0;
        }

        while ( retval == 1 && fgets ( lineBuf, 1024, fp ) != NULL )
        {
          if ( lineBuf[0] == '#' )
          {
            if ( strstr ( lineBuf, "Title" ) != NULL )
              strcpy ( procSrcTitle, &lineBuf[24] );
            continue;
          }

          currVal = atoi ( lineBuf );
          prestaDebug ( "currVal is %d\n", currVal );
          if ( validateFunc ( currVal, minVal, maxVal ) )
          {
            ( *list )[*listSize] = currVal;
            prestaDebug ( "list[%d] is %d\n", *listSize,
                          ( *list )[*listSize] );
            ( *listSize )++;
            if ( *listSize % 1024 == 0 )
            {
              *list =
                ( int * ) realloc ( *list,
                                    ( *listSize / 1024 +
                                      1 ) * 1024 * sizeof ( int ) );

              if ( *list == NULL )
              {
                fclose ( fp );
                retval = 0;
              }
            }
          }
        }

        fclose ( fp );
      }
      else
      {
        retval = 0;
      }
    }
    else
      retval = 0;
  }

  MPI_Bcast ( listSize, 1, MPI_INT, 0, MPI_COMM_WORLD );
  if ( rank != 0 )
  {
    *list = ( int * ) malloc ( *listSize * sizeof ( int ) );
    if ( *list == NULL )
      retval = 0;
  }

  MPI_Allreduce ( &retval, &arval, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD );
  retval = arval;

  if ( retval != 0 )
    MPI_Bcast ( *list, *listSize, MPI_INT, 0, MPI_COMM_WORLD );

  return retval;
}


int
get2entryList ( int ( *validateFunc ) ( int, int, int ), int minVal,
                int maxVal, int *listSize, char *listFile, int **list1,
                int **list2 )
{
  FILE *fp = NULL;
  char lineBuf[1024];
  int currVal;
  char *pVal;

  *listSize = 0;

  if ( listFile != NULL )
  {
    if ( ( fp = fopen ( listFile, "r" ) ) != NULL )
    {
      *list1 = ( int * ) malloc ( 1024 * sizeof ( int ) );
      if ( *list1 == NULL )
      {
        fclose ( fp );
        return 0;
      }

      *list2 = ( int * ) malloc ( 1024 * sizeof ( int ) );
      if ( *list2 == NULL )
      {
        fclose ( fp );
        return 0;
      }

      while ( fgets ( lineBuf, 1024, fp ) != NULL )
      {
        if ( lineBuf[0] == '#' )
          continue;

        prestaDebug ( "processing line %s", lineBuf );
        pVal = strtok ( lineBuf, FILE_DELIM );
        if ( pVal )
          currVal = atoi ( pVal );
        else
          currVal = 0;
        prestaDebug ( "  currVal1 is %d from %s", currVal, pVal );

        if ( validateFunc ( currVal, minVal, maxVal ) )
        {
          ( *list1 )[*listSize] = currVal;
          if ( *listSize % 1024 == 0 )
          {
            *list1 =
              ( int * ) realloc ( *list1,
                                  ( *listSize / 1024 +
                                    1 ) * 1024 * sizeof ( int ) );

            if ( *list1 == NULL )
            {
              fclose ( fp );
              return 0;
            }
          }
        }

        pVal = strtok ( NULL, FILE_DELIM );
        if ( pVal )
          currVal = atoi ( pVal );
        else
          currVal = 0;
        prestaDebug ( "  currVal2 is %d from %s", currVal, pVal );

        ( *list2 )[*listSize] = currVal;

        if ( *listSize % 1024 == 0 )
        {
          *list2 =
            ( int * ) realloc ( *list2,
                                ( *listSize / 1024 +
                                  1 ) * 1024 * sizeof ( int ) );

          if ( *list1 == NULL )
          {
            fclose ( fp );
            return 0;
          }
        }
        ( *listSize )++;
      }

      fclose ( fp );
    }
    else
    {
      return 0;
    }
  }
  else
    return 0;

  return 1;
}


int
createSeqIntArray ( int start, int stop, int factor, int **array,
                    int *arraySize )
{
  int calcValue, calcSize, idx;

  *array = NULL;

  /*  Determine the size of the array by counting range by factor   */
  for ( calcValue = start, calcSize = 0; calcValue <= stop;
        calcValue *= factor )
  {
    calcSize++;

    if ( calcValue == 0 )
      calcValue = 1;
  }

  calcValue /= factor;

  /*  If the final value calculated with the factor multiplier does not equal 
   *  the stop value, increase the array size to include the stop value.  */
  if ( calcValue != stop )
    calcSize++;

  *arraySize = calcSize;
  *array = ( int * ) malloc ( *arraySize * sizeof ( int ) );

  if ( array == NULL )
  {
    fprintf ( stderr, "Failed to allocate array.\n" );

    return 0;
  }

  for ( calcValue = start, idx = 0; calcValue <= stop;
        calcValue *= factor, idx++ )
  {
    ( *array )[idx] = calcValue;

    if ( calcValue == 0 )
      calcValue = 1;
  }

  if ( calcValue / factor != stop )
    ( *array )[idx] = stop;

  return 1;
}


void
prestaDebug ( char *fmt, ... )
{
  va_list args;
  FILE *fp = stderr;

  if ( argStruct.debugFlag == 1 )
  {
    va_start ( args, fmt );
    fprintf ( fp, "DEBUG [%d] %s: ", rank, executableName );
    vfprintf ( fp, fmt, args );
    va_end ( args );
    fflush ( fp );
  }
}

void
prestaRankDebug ( int targRank, char *fmt, ... )
{
  va_list args;
  FILE *fp = stderr;

  if ( ( targRank < 0 || rank == targRank ) && argStruct.debugFlag == 1 )
  {
    va_start ( args, fmt );
    fprintf ( fp, "DEBUG : %s: ", executableName );
    vfprintf ( fp, fmt, args );
    va_end ( args );
    fflush ( fp );
  }
}


void
prestaRankPrint ( int targRank, char *fmt, ... )
{
  va_list args;
  FILE *fp = stdout;

  if ( ( targRank < 0 || rank == targRank ) )
  {
    va_start ( args, fmt );
    vfprintf ( fp, fmt, args );
    va_end ( args );
    fflush ( fp );
  }
}


void
prestaWarn ( char *fmt, ... )
{
  va_list args;
  FILE *fp = stderr;

  va_start ( args, fmt );
  fprintf ( fp, "Warning : %s: ", executableName );
  vfprintf ( fp, fmt, args );
  va_end ( args );
  fflush ( fp );
}


void
prestaAbort ( char *fmt, ... )
{
  va_list args;
  FILE *fp = stderr;

  va_start ( args, fmt );
  fprintf ( fp, "\n" );
  fprintf ( fp, "Failure : %s: ", executableName );
  vfprintf ( fp, fmt, args );
  va_end ( args );
  fprintf ( fp, "\n" );
  fflush ( fp );

  /* Not sure why this Barrier is here.  If a single process aborts, then
   * this would seem to keep any non-aborting processes from leaving the 
   * Barrier.

   MPI_Barrier ( MPI_COMM_WORLD );
   */
  MPI_Abort ( MPI_COMM_WORLD, 0 );
}


/*
 * getNextTargetFile - check the target directory for another target file
 *
 * Uses global variables:
 *  targetDirectory
 *  targetFile
 */

int
getNextTargetFile (  )
{
  static int openedDirectory = 0;
  struct dirent *dent;
  struct stat sbuf;
  int tlen;

  assert ( targetDirectory != NULL );

  if ( !openedDirectory )
  {
    if ( ( dp = opendir ( targetDirectory ) ) == NULL )
    {
      return 0;
    }
    openedDirectory = 1;
    prestaRankDebug ( 0, "opened directory %s\n", targetDirectory );
  }

  while ( ( dent = readdir ( dp ) ) != NULL
          && stat ( dent->d_name, &sbuf ) == 0 && !S_ISREG ( sbuf.st_rdev ) )
  {
    prestaRankDebug ( 0, "found file %s\n", dent->d_name );
  }

  if ( dent != NULL )
  {
    tlen = strlen ( targetDirectory ) + strlen ( dent->d_name ) + 2;
    if ( targetFile )
      free ( targetFile );
    targetFile = malloc ( tlen * sizeof ( char ) );
    strcpy ( targetFile, targetDirectory );
    strcat ( targetFile, "/" );
    strcat ( targetFile, dent->d_name );
    prestaRankDebug ( 0, "found target file %s\n", targetFile );
  }

  if ( dent != NULL )
    return 1;
  else
    closedir ( dp );

  return 0;
}


char *
getTimeStr ( void )
{
  time_t ct;
  static char nowstr[128];
  const struct tm *nowstruct;
  char *fmtstr = "%m/%d/%y  %T";

  time ( &ct );
  nowstruct = localtime ( &ct );
  strftime ( nowstr, 128, fmtstr, nowstruct );
  return strdup ( nowstr );
}


void
set_data_values ( long long buf_size, void *buf_ptr )
{
  long long idx;
  static int init_flag = 0;

  if ( init_flag == 0 )
  {
    srand ( PRESTA_SEED );
    init_flag = 1;
  }

  for ( idx = 0; idx < buf_size; idx++ )
  {
    ( ( char * ) buf_ptr )[idx] = ( char ) rand (  );
  }
}


long long
check_data_values ( int data_count, void *in_buf, void *valid_buf,
                    MPI_Datatype data_type, PRESTA_OP_TYPE op_type )
{
  int type_size;
  long long err_count = 0;

  if ( MPI_Type_size ( data_type, &type_size ) != MPI_SUCCESS )
  {
    prestaAbort ( "Failed to determine data type size.\n" );
  }

  if ( op_type == PRESTA_OP_P2P )
  {
    long long idx;

    for ( idx = 0; idx < type_size * data_count; idx++ )
    {
      if ( ( ( char * ) in_buf )[idx] != ( ( char * ) valid_buf )[idx] )
      {
        prestaDebug ( "Found data err : byte %lld %5d : %5d\n",
                      idx, ( ( char * ) in_buf )[idx],
                      ( ( char * ) valid_buf )[idx] );
        err_count++;
      }
    }
    return err_count;
  }

  return 0;
}


void
printSeparator (  )
{
  char *sep =
    "\n################################################################################\n";

  prestaRankPrint ( 0, sep );
}

void
init_stats ( STATSTRUCT * s, unsigned int t, unsigned int m, unsigned int i,
             unsigned int p )
{
  if ( s != NULL )
  {
    s->tasks = t;
    s->msize = m;
    s->iters = i;
    s->samples = p;
    s->count = 0;
    s->min = MAXDOUBLE;
    s->tot = 0;
    s->max = 0;
  }
  else
  {
    fprintf ( stderr, "Stat structure is NULL!\n" );
  }
}


void
update_stats ( STATSTRUCT * s, double r )
{
  s->count++;
  if ( r > s->max )
    s->max = r;
  if ( r < s->min )
    s->min = r;
  s->tot += r;
  s->mean = s->tot / s->count;
}

void printEnv()
{
  char * cenv = NULL;
  int i, indent = 3;
  char * disable_flag = NULL;

  disable_flag = getenv("PMB_DISABLE_ENV");
  if ( disable_flag != NULL )
    return;

  if ( environ != NULL )
  {
    printSeparator();
    printf("#\n#   Environment variable values\n#");
    printSeparator();

    for ( i = 0, cenv = environ[0]; environ[i] != NULL; i++, cenv = environ[i] )
      printf("%*s%s\n", indent, "", cenv);

    printSeparator();
  }
}

