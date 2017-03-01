/*
Copyright (c) 2005 Konstantin Isakov. Based on the original libbzip2 sources,
part of bzip2, version 1.0.2, Copyright (c) 1996-2002 Julian R Seward.
See LICENSE file for details.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <errno.h>
#include <omp.h>

#include "note.h"
#include "detectht.h"
#include "bzlib.h"

#ifndef HT_DETECTION
#warning Dont know how to detect hyperthreading on this system, please implement it to counter the need to specify it explicitly
#endif

#define BUFSZ 4096

/* Number of chunks currently available to be allocated and put to the
inChunks ring buffer */

/* The output chunks, organized in a ring buffer fasion. The tail chunk is the
one to be written to stdout next, when it is ready. The number of chunks is set
to number of threads * 2 + 1. When a thread finishes compression, it checks if
it finished the compression of a chunk which is to be written next. If so,
it triggers a flush condition.
Note that the chunk is present when its data pointer is non-NULL.
*/

static bz_stream ** outChunks;
static int outChunksCount;

/* Wraps around a negative index within the ring buffer */
int wrapChunkIndex( int idx )
{
  if ( idx < 0 )
    return outChunksCount + idx;
  else
    return idx;
}

static size_t blockSize100k = 9;

void * allocMem( size_t size )
{
  void * result = calloc(1, size );

  if ( result == NULL )
  {
    fprintf(stderr, "Failed to allocate %d bytes of memory\n", size );
    exit( 1 );
  }

  return result;
}

void writerThread(FILE* fp, int lastChunk);

/* The compressing threads' function */
void threadFunction(bz_stream * strm, int pos)
{
  /* Take the mutex to make the in-out chunk acquisition operation atomic */

  /* Allocate the out chunk */

  //fprintf(stderr, "allocating outchunk %d\n", pos );

  /* Now perform the blocksort */

  BZ2_bzCompressDoBlocksort( strm );

  /* Great, now indicate that the in chunk is free and make the
  out chunk available */

  #pragma omp critical
  {

	while( pos >= outChunksCount )
	{
  		outChunks = ( bz_stream ** ) realloc( outChunks, sizeof( bz_stream * ) *
                                         outChunksCount * 2 );
		if(outChunks == NULL)
		{
			fprintf(stderr, "REALLOC FAILED\n");
			exit(1);
		}

		outChunksCount *= 2;
	}

    	outChunks[ pos ] = strm;
  }

  //fprintf(stderr, "finished outchunk %d\n", pos );

  return;
}

/* This thread is only writing the results to stdout */
void writerThread(FILE *fp, int lastChunk)
{
  size_t blockSize;
  bz_stream_state_out savedState;
  void * buf = allocMem( blockSize100k * 101000 + 600 );
  int wasStateSaved = 0;
  int pos = 0;
  bz_stream * strm;

  while( outChunks[ pos ] != NULL && pos <= lastChunk )
  {
	//fprintf(stderr, "Writing to file chunk %d\n", pos);

	strm = outChunks[ pos ];
        outChunks[ pos ] = NULL;
	
	//fprintf(stderr, "flushing outchunk %d\n", pos );

	if ( wasStateSaved )
	  BZ2_bzCompressRestoreOutputState( strm, &savedState );
	
	blockSize = BZ2_bzCompressStoreBlocksort( strm, buf, strm->avail_in );
	
	BZ2_bzCompressSaveOutputState( strm, &savedState );
	wasStateSaved = 1;
	
	BZ2_bzCompressEnd( strm );
	
	free(strm);

  	if ( blockSize && fwrite( buf, blockSize, 1, fp ) != 1 )
  	{
  	  fprintf(stderr, "Error writing to stdout\n" );
  	  exit( 1 );
  	}

  	/* Since we have output the tail chunk, increase the tail, notifying
  	a thread in case it is waiting for a free chunk.*/

  	/* If we empty the ring buffer out, mark it as empty */

	pos++;
  }

  free(buf);
}

static char helpText[2048] = "Help\n";
/*
"\nbzip2smp, a parallelizing bzip2 implementation, version 1.0, 2-Dec-2005\n\n"
"Usage: bzip2smp [-123456789v] [-p#] [--ht|--no-ht] [--help]\n\n"
"The data is read from standard input and the result is output to standard ouput.\n"
"No other modes are supported.\n\n"
"Use -123456789 to specify the bzip2 block size (900k by default).\n\n"
"Use -v to increase verbosity (may be specified more than once).\n\n"
"Use -p# to specify the number of threads to use (default is the number of CPUs\n"
"present in system).\n\n"
"Please note that the use of hyperthreading generally degrades performance due\n"
"to the increased cache miss. Use --ht to make the program halve the number of\n"
"CPUs returned by the system to account to the presence of hyperthreading. "

#ifdef HT_DETECTION
                                                                           "By\n"
"default, this is attempted to be autodetected. Use -v to get a clue on how\n"
"the detection worked for you. In case the misdetection occured, use --no-ht.\n"
"There is no need to bother with the --ht flags if you specify the number of\n"
"threads (-p#) explicitly.\n"

#else

                                                                           "To\n"
"check out the number of CPUs detected, use -v key. There is no need to bother\n"
"with the --ht flag if you specify the number of threads (-p#) explicitly.\n"

#endif
;
*/
int main( int argc, char *argv[] )
{
  int error = 0;
  int hyperthreading = -1;
  unsigned int threadsCount = 0;
  char inputBuf[ BUFSZ ];
  char * inputBufPtr = inputBuf;
  size_t inputBufLeft = 0;
  bz_stream_state_bs savedState;
  int wasStateSaved = 0;
  int pos = 0;
  FILE* fp = stdin, *outFp = stdout;
  int fileFlag = 0, outFileFlag = 0;
  double startTime, endTime;

  int x;

  /* Parse arguments */
  for( x = 1; x < argc; ++x )
  {
    if ( !strcmp( argv[ x ], "--help" ) )
    {
      fprintf( stderr, helpText );
      return 1;
    }

    if ( !strcmp( argv[ x ], "--ht" ) )
    {
      hyperthreading = 1;
      continue;
    }

    if ( !strcmp( argv[ x ], "--no-ht" ) )
    {
      hyperthreading = 0;
      continue;
    }

    if ( argv[ x ][ 0 ] == '-' )
    {
      char * p = argv[ x ] + 1;
      char c;

      do
      {
        c = *p++;

        if (  c >= '1' && c <= '9' )
        {
          blockSize100k = c - '1' + 1;
        }
        else
        if ( c == 'p' )
        {
          if ( sscanf( p, "%u", &threadsCount ) != 1 )
          {
            fprintf(stderr, "Error parsing the number of threads passed: %s.\n",
                  argv[ x ] + 2 );

            return 1;
          }
          c = 0; /* We don't support parsing past -p# */
        }
        else
        if ( c == 'v' )
        {
          ++verbosityLevel;
        }
        else
	if( c == 'i' )
	{
		if( x +1 >= argc ) break;
		
		fp = fopen(argv[++x], "rb");
		if( fp == NULL )
			fp = stdin;
		else
			fileFlag = 1;
	}
        else
	if( c == 'o' )
	{
		if( x +1 >= argc ) break;
		
		outFp = fopen(argv[++x], "wb");
		if( outFp == NULL )
			outFp = stdout;
		else
			outFileFlag = 1;
	}
	else
          break;
      } while( c );

      if ( !c )
        continue;
    }

    fprintf(stderr, "Unrecognized option %s passed.\n", argv[ x ] );

    return 1;
  }

  /*if ( isatty ( fileno ( stdout ) ) )
  {
    fprintf(stderr, "Won't write compressed data to a terminal. Use --help to get help.\n" );
    return 1;
  }*/

  if ( !threadsCount )
  {
    // currently omp_get_num_procs() is not supported. 
    // threadsCount must be specified by command line (-p#)
    //x = omp_get_num_procs();
    x = -1;

    if ( x == -1 )
    {
      fprintf(stderr, "Failed to get the number of processors in the system: %s",
            strerror( errno ) );
  
      return 1;
    }

    threadsCount = x;

    if ( hyperthreading == -1 )
      hyperthreading = isHtPresent();

    if ( hyperthreading )
      threadsCount /= 2;
  
    fprintf(stderr, "CPUs detected: %d\n", threadsCount );
  }

  //fprintf(stderr, "Threads to use: %d\n", threadsCount );

  /* Allocate the input chunks buffer. The actual input chunks are malloc()ed
  dynamically, since they are consumed randomly. The inChunksFree variable
  holds the number of chunks we can malloc(). */

  /* Ok, now allocate output chunks */

  outChunksCount = threadsCount * 2;
  outChunks = ( bz_stream ** ) allocMem( sizeof( bz_stream * ) *
                                         outChunksCount );

  /* Output chunks allocated. */

  /* Create compression threads */

  /* All compression threads started, and are waiting for some data to eat. */

  /* Start the writer thread */

  /* This main thread acts only as a data reader. The compression threads
  pick the data up and process it, and the writer thread writes down the
  results.*/

  startTime = omp_get_wtime();

  #pragma omp parallel
  {
  #pragma omp master 
  {
  while(1) 
  {
    bz_stream * strm = (bz_stream *)allocMem( sizeof( bz_stream ) );
    int lastBlock = 1;

    /* Get an input chunk to place input data to */

    /* Init the compression */
    strm->bzalloc = NULL;
    strm->bzfree = NULL;
    strm->opaque = NULL;

    x = BZ2_bzCompressInit ( strm, blockSize100k, 0, 0, 1 );

    if ( x != BZ_OK )
    {
      fprintf(stderr, "bzip2 compress init returned error code %d\n", x );

      exit( 1 );
    }

    /* Any and all output data will be produced in a writer thread */
    strm->avail_out = 0;

    if ( wasStateSaved )
      BZ2_bzCompressRestoreState( strm, &savedState );

    /* Feed the compressor until it can't handle more */

    while(1) 
    {
      if ( inputBufLeft )
      {
        strm->next_in = inputBufPtr;
        strm->avail_in = inputBufLeft;
  
        x = BZ2_bzCompress( strm, BZ_RUN|BZ_STOP_BEFORE_BLOCKSORT );
  
        if ( x == BZ_STOPPED_BEFORE_BLOCKSORT )
        {
          /* The compressor can't handle no more. Leave the rest to the
          compression thread */
          inputBufLeft = strm->avail_in;
          inputBufPtr = strm->next_in;

          lastBlock = 0;
          break;
        }

        if ( x != BZ_RUN_OK )
        {
          fprintf(stderr, "bzip2 compressing routine (input) returned "
                   "error code %d\n", x );
	  error = 1;
	  break;
        }
        else
          inputBufLeft = 0;
      }
      else
      {
        inputBufLeft = fread( inputBuf, 1, sizeof( inputBuf ), fp );

        if ( !inputBufLeft )
        {
          /* eof or error */

          if ( feof( fp ) )
          {
            break;
          }
          else
          {
            /* 0 bytes read, and not on feof -- must be ferror */

            fprintf(stderr, "error reading data\n" );

	    error = 1;
	    break;
          }
        }

        inputBufPtr = inputBuf;
      }
    }

    if(error)
      break;

    if ( !lastBlock )
    {
      /* Save the state for the future blocks */
      BZ2_bzCompressSaveStateBeforeBlocksort( strm, &savedState );
      wasStateSaved = 1;
    }
    else
    {
      /* Finish up the block, so it goes to the blocksort stage */
      strm->avail_in = 0;
      x = BZ2_bzCompress( strm, BZ_FINISH|BZ_STOP_BEFORE_BLOCKSORT );
      if ( x != BZ_STOPPED_BEFORE_BLOCKSORT )
      {
        fprintf(stderr, "Error: bzip2 compressing routine (finish) "
                 "returned error code %d\n", x );
	error = 1;
	break;
      }
    }

    /* Save the info on whether more blocks will follow or not. This
    would be used to either run or finish the stream */
    /* We reuse avail_in. Way easier than putting our own superstruct. */
    strm->avail_in = lastBlock;

    /* We have made an input chunk on the input chunk ring buffer,
    now let's present it */

    #pragma omp task firstprivate(strm, pos)
    {
    	threadFunction(strm, pos);
    }
    pos++;

    if ( lastBlock )
      break;
  }
  }
  }

  endTime = omp_get_wtime();

  if(!error)
  	writerThread(outFp, pos -1);

  /* Now that the data is all read, just wait until all in chunks are consumed
  and out chunks are written. */

  //fprintf(stderr, "waiting for jobs to finish\n" );

  free(outChunks);

  if( fileFlag )
	  fclose(fp);
  if( outFileFlag )
	  fclose(outFp);

  /* Done */

  /* No further semantic cleanup is required.
  Don't care freeing up the resources, the OS must do it anyway. */

  //printf("Elapsed time %.16f (%f - %f)\n", endTime - startTime, endTime, startTime);

  return 0;
}
