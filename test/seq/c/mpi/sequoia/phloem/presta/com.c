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


    com.c

    Description:

      This test uses MPI_Send/Recv or MPI_Isend/Irecv to determine
      unidirectional bandwidth and MPI_Sendrecv or MPI_Isend/Irecv to determine
      bidirectional bandwidth between pairs of MPI processes.  It can be used
      to measure out-of-SMP bandwidth and bisectional bandwidth, provided the
      MPI processes are properly allocated.

*/


#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <mpi.h>
#include <time.h>
#include <values.h>
#include <unistd.h>
#include "util.h"
#include "com.h"


static char *outputFormat = "  %-14s  %9d  %15d  %5d  %12.3f  %12.3f\n";
static char *outputCharFormat = "  %-14s  %9s  %15s  %5s  %12s  %12s\n";

static char *outputLBWFormat = "  %-14s  %9d  %15d  %6d  %12.3f\n";
static char *outputCharLBWFormat = "  %-14s  %9s  %15s  %6s  %12s\n";

static double minTime = 99999.0;
extern char *executableName;
char hostName[MPI_MAX_PROCESSOR_NAME];


int
main ( int argc, char *argv[] )
{
  int *messList = NULL;
  int testIdx, doTestLoop;
  int i;

  executableName = "com";

  MPI_Init ( &argc, &argv );
  MPI_Get_processor_name ( hostName, &i );

  /* Set global wsize and rank values */
  MPI_Comm_size ( MPI_COMM_WORLD, &wsize );
  MPI_Comm_rank ( MPI_COMM_WORLD, &rank );

  if ( !initAllTestTypeParams ( &testParams ) )
  {
    MPI_Finalize (  );
    exit ( 1 );
  }

  argStruct.testList = "Bidirectional, BidirAsync";

  if ( !processArgs ( argc, argv ) )
  {
    if ( rank == 0 )
      printUse (  );

    MPI_Finalize (  );
    exit ( 1 );
  }

  /* If using a source directory of process rank target files,
   * get the next appropriate file.
   */
  if ( targetDirectory != NULL && getNextTargetFile (  ) == 0 )
  {
    prestaAbort ( "Failed to open target file in target directory %s\n",
                  targetDirectory );
  }

  doTestLoop = 1;
  while ( doTestLoop )
  {
    if ( !setupTestListParams (  ) || !initAllTestTypeParams ( &testParams ) )
    {
      if ( rank == 0 )
        printUse (  );

      MPI_Finalize (  );
      exit ( 1 );
    }

#ifdef PRINT_ENV
    if ( rank == 0 )
      printEnv();
#endif

    printReportHeader (  );

    for ( testIdx = 0; testIdx < TYPETOT; testIdx++ )
    {
      if ( argStruct.testList == NULL
           || ( argStruct.testList != NULL
                && strstr ( argStruct.testList,
                            testParams[testIdx].name ) != NULL ) )
      {
        prestaRankDebug ( 0, "running test index %d\n", testIdx );
        runTest ( &testParams[testIdx] );
      }
    }

    if ( presta_check_data == 1 )
    {
      MPI_Reduce ( &presta_data_err_total, &presta_global_data_err_total,
                   1, MPI_LONG_LONG, MPI_SUM, 0, MPI_COMM_WORLD );
    }

    if ( targetDirectory == NULL || getNextTargetFile (  ) == 0 )
    {
      doTestLoop = 0;
    }
  }

  printSeparator (  );

  freeBuffers ( &testParams );
  free ( messList );

  MPI_Finalize (  );

  exit ( 0 );
}


void
runTest ( TESTPARAMS * testParams )
{
  int procIdx, procs, messIdx;
  MPI_Comm activeComm = MPI_COMM_NULL;
  int *messList, messListSize;
  int testCount, iters;
  unsigned int i;
  char buf[256];
  int width;
  double result;
  STATSTRUCT dp, sum;

  prestaRankPrint ( 0,
                    "\n\n%s Test Results \n(tasks, size, ops/sample, samples) : min/mean/max \n",
                    testParams->name );

  if ( argStruct.verbose )
  {
    if ( testParams->id == LATEN )
    {
      if ( argStruct.sumLocalBW == 1 )
      {
        sprintf ( buf, outputCharLBWFormat, "Test", "Processes",
                  "Op Size (bytes)", "Ops", "Latency (us)" );
      }
      else
      {
        sprintf ( buf, outputCharFormat, "Test", "Processes",
                  "Op Size (bytes)", "Ops", "BW (MB)", "Latency (us)" );
      }
    }
    else
    {
      if ( argStruct.sumLocalBW == 1 )
      {
        sprintf ( buf, outputCharLBWFormat, "Test", "Processes",
                  "Op Size (bytes)", "Ops", "BW (MB)" );
      }
      else
      {
        sprintf ( buf, outputCharFormat, "Test", "Processes",
                  "Op Size (bytes)", "Ops", "BW (MB)", "Op time (us)" );
      }
    }
    prestaRankPrint ( 0, "%s", buf );
    width = strlen ( buf );
  }
  else
    width = 80;

  for ( i = 0; i < width; i++ )
    buf[i] = '-';
  buf[i] = '\0';

  prestaRankPrint ( 0, "%s\n", buf );

  messList = testParams->messList;
  messListSize = testParams->messListSize;

  testParams->maxBW = 0.0;
  testParams->maxBWMessSize = 0;

  init_stats ( &sum, 0, 0, 0, 0 );

  for ( procIdx = 0; procIdx < argStruct.procListSize; procIdx++ )
  {
    procs = argStruct.procList[procIdx];
    if ( procs > wsize )
      procs = wsize;

    /*  Create Communicator of all active processes  */
    procs = createActiveComm ( procs, argStruct.procsPerNode,
                               argStruct.allocPattern,
                               argStruct.useNearestRank, &activeComm );

    prestaDebug ( "rank %d returned from createActiveCom\n", rank );
    prestaDebug ( "messListSize is %d \n", messListSize );

    for ( messIdx = 0; messIdx < messListSize; messIdx++ )
    {
      if ( argStruct.iterList != NULL && argStruct.iterList[messIdx] != 0 )
        iters = argStruct.iterList[messIdx];
      else
        iters = argStruct.iters;

      if ( argStruct.testCountList != NULL )
      {
        fprintf ( stderr,
                  "Before init_stats! messIdx is %d, procIdx is %d, testCountList is %p\n",
                  messIdx, procIdx, argStruct.testCountList );
        init_stats ( &dp, procs, messList[messIdx], iters,
                     argStruct.testCountList[procIdx] );
      }
      else
      {
        init_stats ( &dp, procs, messList[messIdx], iters, argStruct.samples );
      }

      for ( testCount = 0; testCount < dp.samples; testCount++ )
      {
        /*  Run test and save current result  */
        testParams->rankResult =
          testParams->testFunc ( dp.msize, iters, &activeComm );

        /*  TODO : Remove this if unnecessary   */
        if ( testParams->rankResult < minTime )
          minTime = testParams->rankResult;

        if ( !generateResults
             ( testParams, procs, dp.msize, iters, &result ) )
          prestaAbort ( "Failed to generate test results." );

        update_stats ( &dp, result );
        update_stats ( &sum, result );
      }

      if ( testParams->id == LATEN )
      {
        prestaRankPrint ( 0,
                          "(%6d, %9d, %6d, %6d):  %6.3f / %6.3f / %6.3f\n",
                          dp.tasks, dp.msize, dp.iters, dp.samples, dp.min,
                          dp.mean, dp.max );
      }
      else
      {
        prestaRankPrint ( 0,
                          "(%6d, %7d, %5d, %5d):  %12.3f / %12.3f / %12.3f\n",
                          dp.tasks, dp.msize, dp.iters, dp.samples, dp.min,
                          dp.mean, dp.max );
      }
    }
  }

  if ( testParams->id == LATEN )
  {
    prestaRankPrint ( 0,
                      "\nSummary  :             min/mean/max = %6.3f / %6.3f / %6.3f\n",
                      sum.min, sum.mean, sum.max );
  }
  else
  {
    prestaRankPrint ( 0,
                      "\nSummary  :         min/mean/max = %12.3f / %12.3f / %12.3f\n",
                      sum.min, sum.mean, sum.max );
  }

  if ( rank == 0 && argStruct.printPairs )
  {
    printActivePairs ( procs, argStruct.procsPerNode,
                       argStruct.allocPattern, argStruct.useNearestRank );
  }

  if ( activeComm != MPI_COMM_NULL )
    MPI_Comm_free ( &activeComm );
}


int
generateResults ( TESTPARAMS * testParams, int procs, int messSize,
                  int iters, double *result )
{
  double maxTime, aggCommSize = 0, currBW;

  if ( argStruct.printRankTime && testParams->rankResult > 0.0 )
  {
    /*  Print rank information for easy grep/sort-ing    */
    /*  Fields :  test, procs, messSize, time, rank, hostName  */
    printf ( "Rank Data : %12s %6d %11d %12.9f %6d %s\n",
             testParams->name, procs, messSize, testParams->rankResult, rank,
             hostName );
  }

  if ( argStruct.sumLocalBW == 1 && testParams->id != LATEN )
  {
    currBW = 0;

    /*
       Bandwidth is calculated as :

       Sum of local process bandwith, where local bandwidth is calculated as

       (operations per time sample * message size)
       -------------------------------------------
       local sample length 
     */

    MPI_Reduce ( &testParams->rankResult, &currBW,
                 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD );

    currBW /= MB_SIZE;
    if ( currBW > 0 )
      prestaRankDebug ( 0, "Total local BW %f\n", currBW );
  }
  else
  {
    MPI_Reduce ( &testParams->rankResult, &maxTime,
                 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD );

    prestaRankDebug ( 0, "maxTime is %f\n", maxTime );

    if ( maxTime > testParams->maxResult )
      testParams->maxResult = maxTime;

    /*
       Bandwidth is calculated as :

       (# of processes * operations per time sample * message size)
       ------------------------------------------------------------
       maximum sample length of all processes
     */

    aggCommSize =
      ( double ) iters *testParams->sendFactor *
      ( double ) procs *( double ) messSize;
    currBW = ( aggCommSize / ( double ) maxTime ) / MB_SIZE;
  }

  prestaRankDebug ( 0, "iters is %d\n", iters );
  prestaRankDebug ( 0, "procs is %d\n", procs );
  prestaRankDebug ( 0, "messSize is %d\n", messSize );
  prestaRankDebug ( 0, "sendFactor is %f\n", testParams->sendFactor );
  prestaRankDebug ( 0, "aggCommSize is %f\n", aggCommSize );
  prestaRankDebug ( 0, "maxTime is %f\n", testParams->rankResult );
  prestaRankDebug ( 0, "currBW is %f\n", currBW );

  if ( argStruct.sumLocalBW == 1 )
  {
    if ( testParams->id == LATEN )
    {
      *result = maxTime * 1000000 / ( 2.0 * iters );    /* Report latency in msec  */
    }
    else
      *result = currBW;

    if ( argStruct.verbose )
      prestaRankPrint ( 0, outputLBWFormat, testParams->name, procs, messSize,
                        iters, *result );
  }
  else
  {
    if ( testParams->id == LATEN )
      *result = maxTime * 1000000 / ( 2.0 * iters );    /* Report latency in msec  */
    else
      *result = currBW;

    if ( argStruct.verbose )
      prestaRankPrint ( 0, outputFormat, testParams->name, procs, messSize,
                        iters, currBW, *result );
  }

  if ( currBW > testParams->maxBW )
  {
    testParams->maxBW = currBW;
    testParams->maxBWMessSize = messSize;
  }

  return 1;
}


double
runUnicomTest ( int bufsize, int iters, MPI_Comm * activeComm )
{
  int i, currtarg;
  double diff = 0.0;
  double start;
  MPI_Status stat;
  char *comBuf;

  currtarg =
    getTargetRank ( rank, argStruct.procsPerNode, argStruct.useNearestRank );

  diff = 0;

  if ( isActiveProc ( activeComm ) )
  {
    comBuf = ( char * ) malloc ( bufsize );
    memset ( comBuf, 0, bufsize );

    /*  Ensure communication paths have been initialized  */
    if ( rank < currtarg )
      MPI_Send ( comBuf, bufsize, MPI_BYTE, currtarg, 0, MPI_COMM_WORLD );
    else
    {
      MPI_Recv ( comBuf, bufsize, MPI_BYTE, currtarg, 0,
                 MPI_COMM_WORLD, &stat );
    }

    generic_barrier ( *activeComm );
    generic_barrier ( *activeComm );

    if ( rank < currtarg )
    {
      /*  Time operation loop  */
      start = MPI_Wtime (  );
      for ( i = 0; i < iters; i++ )
      {
        if ( presta_check_data == 1 )
          set_data_values ( bufsize, comBuf );
        MPI_Send ( comBuf, bufsize, MPI_BYTE, currtarg, 0, MPI_COMM_WORLD );
      }
    }
    else
    {
      void *validate_buf = NULL;
      int err_count = 0;
      if ( presta_check_data == 1 )
      {
        validate_buf = malloc ( bufsize );
      }

      start = MPI_Wtime (  );
      for ( i = 0; i < iters; i++ )
      {
        if ( presta_check_data == 1 )
          set_data_values ( bufsize, validate_buf );

        MPI_Recv ( comBuf, bufsize, MPI_BYTE, currtarg, 0,
                   MPI_COMM_WORLD, &stat );
        if ( presta_check_data == 1 )
        {
          err_count =
            check_data_values ( bufsize, comBuf, validate_buf,
                                MPI_BYTE, PRESTA_OP_P2P );
          if ( err_count > 0 )
          {
            prestaWarn
              ( "Unidirectional receive data check failed with %d errors\n",
                err_count );
            presta_data_err_total += err_count;

          }
        }
      }

      if ( presta_check_data == 1 )
      {
        free ( validate_buf );
      }
    }

    if ( argStruct.useBarrier )
      generic_barrier ( *activeComm );

    diff = MPI_Wtime (  ) - start;

    free ( comBuf );
  }

  MPI_Barrier ( MPI_COMM_WORLD );

  if ( diff > 0 && argStruct.sumLocalBW == 1 )
    return ( ( double ) bufsize * ( double ) iters ) / ( diff * 2 );
  else
    return diff;
}



double
runNonblockUnicomTest ( int bufsize, int iters, MPI_Comm * activeComm )
{
  int i, currtarg;
  double diff = 0.0;
  double start;
  char *comBuf;
  MPI_Request *requests;
  MPI_Status *statuses;

  currtarg = getTargetRank ( rank, argStruct.procsPerNode,
                             argStruct.useNearestRank );

  comBuf = ( char * ) malloc ( bufsize );
  memset ( comBuf, 0, bufsize );

  requests = malloc ( sizeof ( MPI_Request ) * argStruct.iters );
  statuses = malloc ( sizeof ( MPI_Status ) * argStruct.iters );

  if ( comBuf == NULL || requests == NULL || statuses == NULL )
    return 0;

  if ( isActiveProc ( activeComm ) )
  {
    /*  Ensure communication paths have been initialized  */
    if ( rank < currtarg )
      MPI_Isend ( comBuf, bufsize, MPI_BYTE, currtarg, 0,
                  MPI_COMM_WORLD, requests );
    else
    {
      MPI_Irecv ( comBuf, bufsize, MPI_BYTE, currtarg, 0,
                  MPI_COMM_WORLD, requests );
    }

    MPI_Wait ( requests, statuses );

    generic_barrier ( *activeComm );
    generic_barrier ( *activeComm );

#ifdef FINAL_WAITALL

    if ( rank < currtarg )
    {
      /*  Time operation loop  */
      start = MPI_Wtime (  );
      for ( i = 0; i < iters; i++ )
        MPI_Isend ( comBuf, bufsize, MPI_BYTE, currtarg, i,
                    MPI_COMM_WORLD, &requests[i] );
    }
    else
    {
      start = MPI_Wtime (  );
      for ( i = 0; i < iters; i++ )
      {
        MPI_Irecv ( comBuf, bufsize, MPI_BYTE, currtarg, MPI_ANY_TAG,
                    MPI_COMM_WORLD, &requests[i] );
      }
    }

    MPI_Waitall ( argStruct.iters, requests, statuses );

#else

    start = MPI_Wtime (  );
    for ( i = 0; i < iters; i++ )
    {
      if ( rank < currtarg )
      {
        /*  Time operation loop  */
        MPI_Isend ( comBuf, bufsize, MPI_BYTE, currtarg, i,
                    MPI_COMM_WORLD, requests );
      }
      else
      {
        MPI_Irecv ( comBuf, bufsize, MPI_BYTE, currtarg, MPI_ANY_TAG,
                    MPI_COMM_WORLD, requests );
      }

      MPI_Wait ( requests, statuses );
    }

#endif

    if ( argStruct.useBarrier )
      generic_barrier ( *activeComm );

    diff = MPI_Wtime (  ) - start;
  }

  free ( comBuf );
  free ( requests );
  free ( statuses );

  MPI_Barrier ( MPI_COMM_WORLD );


  if ( diff > 0 && argStruct.sumLocalBW == 1 )
    return ( ( double ) bufsize * ( double ) iters ) / ( diff * 2 );
  else
    return diff;
}


double
runBicomTest ( int bufsize, int iters, MPI_Comm * activeComm )
{
  int i, currtarg;
  double start, diff;
  char *sendbuf, *recvbuf, *validate_buf;
  MPI_Status stat;
  long long err_count = 0;

  currtarg = getTargetRank ( rank, argStruct.procsPerNode,
                             argStruct.useNearestRank );
  diff = 0.0;

  if ( currtarg != -1 && isActiveProc ( activeComm ) )
  {
    sendbuf = ( char * ) malloc ( bufsize );
    recvbuf = ( char * ) malloc ( bufsize );

    memset ( sendbuf, 0, bufsize );
    memset ( recvbuf, 0, bufsize );

    /*  Ensure communication paths have been initialized  */
    MPI_Sendrecv ( sendbuf, bufsize, MPI_BYTE, currtarg, 0,
                   recvbuf, bufsize, MPI_BYTE, currtarg, 0,
                   MPI_COMM_WORLD, &stat );

    generic_barrier ( *activeComm );
    generic_barrier ( *activeComm );

    /*  Time operation loop  */
    start = MPI_Wtime (  );

    if ( presta_check_data == 1 )
      validate_buf = malloc ( bufsize );

    for ( i = 0; i < iters; i++ )
    {
      if ( presta_check_data == 1 )
      {
        set_data_values ( bufsize, sendbuf );
        memcpy ( validate_buf, sendbuf, bufsize );
      }

      MPI_Sendrecv ( sendbuf, bufsize, MPI_BYTE, currtarg, 0,
                     recvbuf, bufsize, MPI_BYTE, currtarg, 0,
                     MPI_COMM_WORLD, &stat );

      if ( presta_check_data == 1 )
      {
        err_count = check_data_values ( bufsize, recvbuf, validate_buf,
                                        MPI_BYTE, PRESTA_OP_P2P );
        if ( err_count > 0 )
        {
          prestaWarn
            ( "Bidirectional receive data check failed with %d errors\n",
              err_count );
          presta_data_err_total += err_count;
        }
      }
    }

    if ( presta_check_data == 1 )
      free ( validate_buf );

    if ( argStruct.useBarrier )
      generic_barrier ( *activeComm );

    diff = MPI_Wtime (  ) - start;

    free ( sendbuf );
    free ( recvbuf );
  }

  MPI_Barrier ( MPI_COMM_WORLD );

  if ( diff > 0 && argStruct.sumLocalBW == 1 )
    return ( ( double ) bufsize * ( double ) iters ) / diff;
  else
    return diff;
}


double
runNonblockBicomTest ( int bufsize, int iters, MPI_Comm * activeComm )
{
  int i, currtarg;
  double diff = 0.0;
  double start;
  MPI_Status stat;
  char *sendBuf, *recvBuf;
  MPI_Request *sendRequests, *recvRequests;
  MPI_Status *sendStatuses, *recvStatuses;

  currtarg = getTargetRank ( rank, argStruct.procsPerNode,
                             argStruct.useNearestRank );

  sendBuf = ( char * ) malloc ( bufsize );
  recvBuf = ( char * ) malloc ( bufsize );

  sendRequests = malloc ( sizeof ( MPI_Request ) * argStruct.iters );
  recvRequests = malloc ( sizeof ( MPI_Request ) * argStruct.iters );
  sendStatuses = malloc ( sizeof ( MPI_Status ) * argStruct.iters );
  recvStatuses = malloc ( sizeof ( MPI_Status ) * argStruct.iters );

  if ( sendBuf == NULL || recvBuf == NULL ||
       sendRequests == NULL || recvRequests == NULL ||
       sendStatuses == NULL || recvStatuses == NULL )
    return 0;

  memset ( sendBuf, 0, bufsize );
  memset ( recvBuf, 0, bufsize );

  if ( isActiveProc ( activeComm ) )
  {
    /*  Ensure communication paths have been initialized  */
    MPI_Irecv ( recvBuf, bufsize, MPI_BYTE, currtarg, 0,
                MPI_COMM_WORLD, recvRequests );
    MPI_Isend ( sendBuf, bufsize, MPI_BYTE, currtarg, 0,
                MPI_COMM_WORLD, sendRequests );

    MPI_Wait ( recvRequests, recvStatuses );
    MPI_Wait ( sendRequests, sendStatuses );

    generic_barrier ( *activeComm );
    generic_barrier ( *activeComm );

    /*  Time operation loop  */
    start = MPI_Wtime (  );

#ifdef FINAL_WAITALL

    for ( i = 0; i < iters; i++ )
    {
      MPI_Irecv ( recvBuf, bufsize, MPI_BYTE, currtarg, MPI_ANY_TAG,
                  MPI_COMM_WORLD, &recvRequests[i] );
    }

    for ( i = 0; i < iters; i++ )
    {
      MPI_Isend ( sendBuf, bufsize, MPI_BYTE, currtarg, i,
                  MPI_COMM_WORLD, &sendRequests[i] );
    }

    MPI_Waitall ( argStruct.iters, sendRequests, sendStatuses );
    MPI_Waitall ( argStruct.iters, recvRequests, recvStatuses );

#else

    for ( i = 0; i < iters; i++ )
    {
      MPI_Isend ( sendBuf, bufsize, MPI_BYTE, currtarg, i,
                  MPI_COMM_WORLD, &sendRequests[0] );

      MPI_Recv ( recvBuf, bufsize, MPI_BYTE, currtarg, MPI_ANY_TAG,
                 MPI_COMM_WORLD, &stat );

      MPI_Wait ( sendRequests, sendStatuses );
    }

#endif

    if ( argStruct.useBarrier )
      generic_barrier ( *activeComm );

    diff = MPI_Wtime (  ) - start;
  }

  free ( sendBuf );
  free ( recvBuf );
  free ( sendRequests );
  free ( recvRequests );
  free ( sendStatuses );
  free ( recvStatuses );

  MPI_Barrier ( MPI_COMM_WORLD );

  if ( diff > 0 && argStruct.sumLocalBW == 1 )
    return ( ( double ) bufsize * ( double ) iters ) / diff;
  else
    return diff;
}


double
runLatencyTest ( int bufsize, int iters, MPI_Comm * activeComm )
{
  int i, currtarg;
  double start, diff;
  MPI_Status stat;
  char *comBuf = NULL;

  if ( bufsize > 0 )
  {
    comBuf = ( char * ) malloc ( bufsize );

    if ( comBuf == NULL )
      prestaAbort ( "Failed to allocate latency buffer.\n" );
  }

  currtarg =
    getTargetRank ( rank, argStruct.procsPerNode, argStruct.useNearestRank );
  diff = 0.0;

  if ( isActiveProc ( activeComm ) )
  {
    for ( i = 0; i < 1000; i++ )
    {
      if ( rank < currtarg )
      {
        /*  Ensure pair communication has been initialized  */
        MPI_Send ( comBuf, bufsize, MPI_BYTE, currtarg, 0, MPI_COMM_WORLD );
        MPI_Recv ( comBuf, bufsize, MPI_BYTE, currtarg, 0, MPI_COMM_WORLD,
                   &stat );
      }
      else
      {
        /*  Ensure pair communication has been initialized  */
        MPI_Recv ( comBuf, bufsize, MPI_BYTE, currtarg, 0, MPI_COMM_WORLD,
                   &stat );
        MPI_Send ( comBuf, bufsize, MPI_BYTE, currtarg, 0, MPI_COMM_WORLD );
      }
    }

    generic_barrier ( *activeComm );
    generic_barrier ( *activeComm );

    if ( rank < currtarg )
    {
      /*  Time operation loop  */
      start = MPI_Wtime (  );

      for ( i = 0; i < iters; i++ )
      {
        MPI_Send ( comBuf, bufsize, MPI_BYTE, currtarg, 0, MPI_COMM_WORLD );
        MPI_Recv ( comBuf, bufsize, MPI_BYTE, currtarg, 0, MPI_COMM_WORLD,
                   &stat );
      }
    }
    else
    {
      /*  Time operation loop  */
      start = MPI_Wtime (  );

      for ( i = 0; i < iters; i++ )
      {
        MPI_Recv ( comBuf, bufsize, MPI_BYTE, currtarg, 0, MPI_COMM_WORLD,
                   &stat );
        MPI_Send ( comBuf, bufsize, MPI_BYTE, currtarg, 0, MPI_COMM_WORLD );
      }
    }

    if ( argStruct.useBarrier )
      generic_barrier ( *activeComm );

    diff = MPI_Wtime (  ) - start;
  }

  MPI_Barrier ( MPI_COMM_WORLD );

  if ( comBuf != NULL )
    free ( comBuf );

  return diff;
}


void
printUse ( void )
{
  printf ( "\n%s : Point-to-Point MPI bandwidth and latency benchmark\n\n",
           executableName );
  printf ( "  syntax: %s [OPTION]...\n\n", executableName );
  printf ( "    -a print individual test results               default=%d\n",
           MESS_START_DEF );
  printf ( "    -b [message start size]                        default=%d\n",
           MESS_START_DEF );
  printf ( "    -c [samples]                                   default=%d\n",
           argStruct.samples );
  printf ( "    -d [task target list source file]\n" );
  printf ( "    -e [message stop  size]                        default=%d\n",
           MESS_STOP_DEF );
  printf ( "    -f [process count source file]\n" );
  printf ( "    -h print use information\n" );
  printf ( "    -i print process pairs for each measurement    default=%s\n",
           PRINT_PAIRS_DEF ? "true" : "false" );
  printf ( "    -k print individual rank times                 default=%s\n",
           "false" );
  printf ( "    -l print hostname information\n" );
  printf ( "    -m [message size source file]\n" );
  printf ( "    -n use barrier within measurement              default=%s\n",
           USE_BARRIER_DEF ? "barrier" : "no barrier" );
  printf ( "    -o [number of operations between measurements]\n" );
  printf ( "    -p [allocate processes: c(yclic) or b(lock)]   default=%c\n",
           ALLOC_DEF );
  printf ( "    -q print test names\n" );
  printf ( "    -r partner processes with nearby rank          default=%s\n",
           NEAREST_RANK_DEF ? "true" : "false" );
  printf ( "    -s [directory of task target source files]\n" );
  printf ( "    -t [processes per SMP]\n" );
  printf ( "    -v verify message data                         default=%s\n",
           "false" );
  printf ( "    -w '[list of full test names]'\n" );
  printf ( "    -x calculate BW by volume/longest task time    default=%s\n",
           "false" );
  printf ( "\n" );
}


void
printReportHeader ( void )
{
  char *now;
  int nameSize;
  char myProcName[MPI_MAX_PROCESSOR_NAME];

  if ( rank == 0 )
  {
    now = getTimeStr (  );
    MPI_Get_processor_name ( myProcName, &nameSize );

    printSeparator (  );
    printf ( "\n  com Point-to-Point MPI Bandwidth and Latency Benchmark\n" );
    printf ( "  Version %d.%d.%d\n", majorVersion, minorVersion,
             patchVersion );
    printf ( "  Run at %s, with rank 0 on %s\n\n", now, myProcName );
    free ( now );
    printParameters (  );

    if ( presta_check_data == 1 )
    {
      printf
        ( "  *** Verifying operation message buffers.  Benchmark results not valid! ***\n" );
    }

    if ( strlen ( procSrcTitle ) > 0 )
      printf ( "  Using Task Pair file %s\n", procSrcTitle );

    printf ( "\n" );

    if ( strcmp ( argStruct.testList, "Latency" ) != 0 )
    {
      if ( argStruct.sumLocalBW == 1 )
        printf ( "  Bandwidth calculated as sum of process bandwidths.\n" );
      else if ( argStruct.sumLocalBW == 0 )
        printf
          ( "  Bandwidth calculated as total volume / longest task communication.\n" );

      /*  TODO : Not necessary unless providing info about sample time.
         printTimingInfo ( );   
       */
    }
  }


  if ( argStruct.printHostInfo )
    printCommTargets ( argStruct.procsPerNode, argStruct.useNearestRank );

  printSeparator (  );

}


void
printParameters (  )
{
  if ( argStruct.procFile )
    printf ( "  Process count file                   : %s\n",
             argStruct.procFile );

  if ( argStruct.messFile )
    printf ( "  Message and iteration file           : %s\n",
             argStruct.messFile );

  if ( argStruct.procsPerNode > 0 )
    printf ( "  Processes per SMP                    : %d\n",
             argStruct.procsPerNode );

  if ( argStruct.allocPattern == 'b' )
    printf ( "  Process pair allocation              : %s\n", "block" );
  else
    printf ( "  Process pair allocation              : %s\n", "cyclic" );

  if ( targetFile != NULL )
    printf ( "  Process pair sourcefile              : %s\n", targetFile );

  if ( strcmp ( argStruct.testList, "Latency" ) != 0 )
    printf ( "  MB size for BW calculation           : %d\n", MB_SIZE );

  if ( presta_check_data == 1 )
    printf ( "  Total Data errors                    : %llu\n",
             presta_data_err_total );

  if ( argStruct.useBarrier )
    printf ( "\n  Barrier included in measurement.\n" );
}


int
setupTestListParams (  )
{
  if ( wsize % 2 != 0 )
    wsize--;

  if ( argStruct.procFile != NULL )
  {
    if ( !get2entryList ( validateProcCount, 2, MAXINT,
                          &argStruct.procListSize, argStruct.procFile,
                          &argStruct.procList, &argStruct.testCountList ) )
    {
      prestaAbort
        ( "Failed to generate valid process count list from file %s\n",
          argStruct.procFile );
      return 0;
    }
  }
  else
  {
    if ( argStruct.allTasksOnly )
    {
      argStruct.procList = &wsize;
      argStruct.procListSize = 1;
    }
    else if ( argStruct.allocPattern == 'c' )
    {
      if ( !createSeqIntArray ( 2, wsize, wsize / argStruct.procsPerNode,
                                &argStruct.procList,
                                &argStruct.procListSize ) )
      {
        return 0;
      }
    }
    else
    {
      if ( !createSeqIntArray ( 2, wsize, argStruct.messFactor,
                                &argStruct.procList,
                                &argStruct.procListSize ) )
      {
        return 0;
      }
    }
  }

  if ( argStruct.messFile != NULL )
  {
    if ( !get2entryList ( validateMessageSize, MIN_MESS_SIZE, MAX_MESS_SIZE,
                          &argStruct.messListSize, argStruct.messFile,
                          &argStruct.messList, &argStruct.iterList ) )
    {
      prestaRankPrint ( 0, "Failed to open message size source file %s.\n",
                        argStruct.messFile );
      prestaRankPrint ( 0, "Using default message sizes and iterations.\n" );
      if ( !createSeqIntArray
           ( argStruct.messStart, argStruct.messStop, argStruct.messFactor,
             &argStruct.messList, &argStruct.messListSize ) )
      {
        return 0;
      }
    }
  }
  else
  {
    if ( !createSeqIntArray ( argStruct.messStart, argStruct.messStop,
                              argStruct.messFactor, &argStruct.messList,
                              &argStruct.messListSize ) )
    {
      return 0;
    }
  }

  if ( targetFile != NULL && !getTargetList (  ) )
  {
    prestaAbort ( "Failed to open target list source file %s\n", targetFile );
    return 0;
  }

  if ( targetFile != NULL && targetListSize != wsize && rank == 0 )
  {
    prestaAbort
      ( "Target file %s entry count %d does not match MPI_COMM_WORLD size %d\n",
        targetFile, targetListSize, wsize );
    return 0;
  }

  return 1;
}


int
initAllTestTypeParams ( TESTPARAMS ** testParams )
{
  TESTPARAMS *ptp;
  int *latenList, idx;

  latenList = ( int * ) malloc ( sizeof ( int ) );

  *testParams = ( TESTPARAMS * ) malloc ( sizeof ( TESTPARAMS ) * TYPETOT );

  if ( latenList == NULL || *testParams == NULL )
  {
    fprintf ( stderr, "Failed to allocate test parameters\n" );
    return 0;
  }

  ptp = &( *testParams )[UNIDIR];

  ptp->id = UNIDIR;
  ptp->name = "Unidirectional";
  ptp->iters = argStruct.iters;
  ptp->sendFactor = .5;
  ptp->messList = argStruct.messList;
  ptp->messListSize = argStruct.messListSize;
  ptp->testFunc = runUnicomTest;

  ptp = &( *testParams )[UNIDIRNB];

  ptp->id = UNIDIRNB;
  ptp->name = "UnidirAsync";
  ptp->iters = argStruct.iters;
  ptp->sendFactor = .5;
  ptp->messList = argStruct.messList;
  ptp->messListSize = argStruct.messListSize;
  ptp->testFunc = runNonblockUnicomTest;

  ptp = &( *testParams )[BIDIR];

  ptp->id = BIDIR;
  ptp->name = "Bidirectional";
  ptp->iters = argStruct.iters;
  ptp->sendFactor = 1;
  ptp->messList = argStruct.messList;
  ptp->messListSize = argStruct.messListSize;
  ptp->testFunc = runBicomTest;

  ptp = &( *testParams )[BIDIRNB];

  ptp->id = BIDIRNB;
  ptp->name = "BidirAsync";
  ptp->iters = argStruct.iters;
  ptp->sendFactor = 1;
  ptp->messList = argStruct.messList;
  ptp->messListSize = argStruct.messListSize;
  ptp->testFunc = runNonblockBicomTest;

  ptp = &( *testParams )[LATEN];

  latenList[0] = 0;
  ptp->id = LATEN;
  ptp->name = "Latency";
  ptp->iters = argStruct.iters;
  ptp->messList = argStruct.messList;
  ptp->messListSize = argStruct.messListSize;
  ptp->testFunc = runLatencyTest;

  for ( idx = 0; idx < TYPETOT; idx++ )
  {
    ptp = &( *testParams )[idx];

    ptp->maxBW = 0;
    ptp->maxBWMessSize = 0;

    ptp->rankResult = 0.0;
    ptp->maxResult = 0.0;
  }

  return 1;
}


void
printTestNames ( void )
{
  int testIdx;

  prestaRankPrint ( 0, "\nTest Names\n" );

  for ( testIdx = 0; testIdx < TYPETOT; testIdx++ )
    prestaRankPrint ( 0, "  %s\n", ( &testParams[testIdx] )->name );

  prestaRankPrint ( 0, "\n" );
}


void
freeBuffers ( TESTPARAMS ** testParams )
{
  if ( !argStruct.allTasksOnly )
    free ( argStruct.procList );

  free ( *testParams );
}


int
processArgs ( int argc, char **argv )
{
  int flag;
  int dogetopt = 1;

  argStruct.iters = 0;
  argStruct.iterList = NULL;
  argStruct.samples = 1;
  argStruct.debugFlag = 0;
  argStruct.messStart = MESS_START_DEF;
  argStruct.messStop = MESS_STOP_DEF;
  argStruct.messFactor = MESS_FACTOR_DEF;
  argStruct.procFile = NULL;
  argStruct.messFile = NULL;
  argStruct.printHostInfo = 0;
  argStruct.procsPerNode = 0;
  argStruct.allocPattern = ALLOC_DEF;
  argStruct.useBarrier = USE_BARRIER_DEF;
  argStruct.printPairs = PRINT_PAIRS_DEF;
  argStruct.useNearestRank = NEAREST_RANK_DEF;
  argStruct.printRankTime = 0;
  argStruct.sumLocalBW = 1;
  argStruct.verbose = 0;
  argStruct.allTasksOnly = 0;

  opterr = 0;
  while ( ( flag =
            getopt ( argc, argv,
                     "ab:c:d:e:f:ghiklm:no:p:qrs:t:uvw:xz" ) ) != -1
          && dogetopt == 1 )
  {
    /*  Flags without arguments  */

    switch ( flag )
    {
    case 'a':
      argStruct.verbose = 1;
      break;
    case 'g':
      argStruct.debugFlag = 1;
      break;
    case 'h':
      return ( 0 );
      break;
    case 'i':
      argStruct.printPairs = 1;
      break;
    case 'k':
      argStruct.printRankTime = 1;
      break;
    case 'l':
      argStruct.printHostInfo = 1;
      break;
    case 'n':
      argStruct.useBarrier = 1;
      break;
    case 'q':
      printTestNames (  );
      MPI_Finalize (  );
      exit ( 0 );
      break;
    case 'r':
      argStruct.useNearestRank = 1;
      break;
    case 'u':
      argStruct.allTasksOnly = 1;
      break;
    case 'v':
      presta_check_data = 1;
      break;
    case 'x':
      argStruct.sumLocalBW = 0;
      break;

      /*  Flags with arguments  */
    case 'b':
      argStruct.messStart = atoi ( optarg );
      break;
    case 'c':
      argStruct.samples = atoi ( optarg );
      break;
    case 'd':
      targetFile = strdup ( optarg );
      break;
    case 'e':
      argStruct.messStop = atoi ( optarg );
      break;
    case 'f':
      argStruct.procFile = strdup ( optarg );
      break;
    case 'm':
      argStruct.messFile = strdup ( optarg );
      break;
    case 'o':
      argStruct.iters = atoi ( optarg );
      break;
    case 'p':
      argStruct.allocPattern = *optarg;
      break;
    case 's':
      targetDirectory = strdup ( optarg );
      break;
    case 't':
      argStruct.procsPerNode = atoi ( optarg );
      break;
    case 'w':
      if ( strlen ( optarg ) == 0 )
      {
        printTestNames (  );
        exit ( -1 );
      }
      argStruct.testList = strdup ( optarg );
      break;
    default:
      if ( rank == 0 )
        printf ( "\nInvalid option -%c\n", optopt );
      dogetopt = 0;
      break;
    }
  }

  if ( argStruct.iters == 0 )
  {
    argStruct.iters = 10;
  }

  if ( argStruct.allocPattern == 'c' && argStruct.procsPerNode == 0 )
  {
    if ( rank == 0 )
      printf
        ( "\n  Cyclic allocation requires a process per node argument!\n\n" );
    return ( 0 );
  }

  if ( argStruct.allocPattern == 'c' && wsize <= argStruct.procsPerNode * 2 )
  {
    if ( rank == 0 )
    {
      printf ( "\n  Cyclic allocation can only be used for jobs\n" );
      printf ( "    with MPI process count > 2*processes/node.\n\n" );

    }
    return ( 0 );
  }

  return ( 1 );
}

