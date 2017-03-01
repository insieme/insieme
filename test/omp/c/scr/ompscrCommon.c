/************************************************************************
  This library is part of the
	OpenMP Source Code Repository

	http://www.pcg.ull.es/OmpSCR/
	e-mail: ompscr@zion.deioc.ull.es

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

**************************************************************************/
/*
* OpenMP Source Code Repository
*
* Module: oscrCommon_c
* Version: v0.2
*
* Copyright (C) 2004, Arturo González Escribano, Francisco de Sande
*
* Common funtions to control command line arguments, timers and report
*
*/
#include<stdio.h>
#include<string.h>
#include"wtime.h"
#include"OmpSCR.h"

#define OSCR_HEADER "\nThis program is part of the OpenMP Source Code Repository (OmpSCR)\n"
#define OSCR_TESTARG "-test"

/* GLOBAL VARIABLES */
int	oscrData_init = 0;
int 	oscrData_numThreads = 0;

/* 1. ARGUMENTS CONTROL */
int	oscrData_numArgs = 0;
char	oscrData_usage[1024] = "?";
char	**oscrData_argNames = NULL;
char	**oscrData_argv = NULL;

/* 2. TIMERS CONTROL */
typedef struct {
	double	start;
	double	elapsed;
	int	active;
} oscrTimer;

int	oscrData_numTimers;
oscrTimer *oscrTimers;

int	oscrData_numReportedTimers;
char 	**oscrData_reportedTimerNames;

/* PAPI */
#ifdef __USE_PAPI
#define SCALEPAPI 1000000.0
int *oscrData_timerPAPIEventSet = NULL;
#endif

/*
*
* COMMON FUNTIONS
*
*/

/* -----------------------------------------------------------------
   Routine:     OSCR_calloc
   Description: Allocates nmemb * size bytes of memory and checks that
	              that memory is effectively allocated.
		If it is not possible to allocate memory, aborts execution.
 * ----------------------------------------------------------------- */
void *OSCR_calloc(size_t nmemb, size_t size) {
  void *pointer;

  pointer = calloc(nmemb, size);
  if (pointer == NULL)
	  OSCR_error("Not enough memory!! No. of elements requested: ", nmemb);

  return (pointer);
}

/* -----------------------------------------------------------------
   Routine:     OSCR_malloc
   Description: Allocates size bytes of memory and checks that
	              that memory is effectively allocated.
		If it is not possible to allocate memory, aborts execution.
 * ----------------------------------------------------------------- */
void *OSCR_malloc(size_t size) {
  void *pointer;

  pointer = malloc(size);
  if (pointer == NULL)
	  OSCR_error("Not enough memory!! No. of bytes requested: ", size);

  return (pointer);
}

/*
* ARGUMENTS ERROR: Show usage line
*/
void OSCR_args_error(const char *message, int ind) {
	fprintf(stderr,"\n");

	fprintf(stderr,"OSCR Error: %s", message);
	if (ind != 0) fprintf(stderr," %d", ind);
	fprintf(stderr,"\n\n");

	fprintf(stderr,"Usage: %s -test\n", oscrData_argv[0]);
	fprintf(stderr,"       %s\n\n", oscrData_usage);
	exit(1);
	}

/*
* OTHER INTERNAL ERRORS: Don't show usage line
*/
void OSCR_error(const char *message, int errInfo) {
	fprintf(stderr,"\n");

	fprintf(stderr,"OSCR Error: %s", message);
	if (errInfo != -1) fprintf(stderr," %d", errInfo);
	fprintf(stderr,"\n\n");

	exit(1);
	}


/*
* INITIALIZATION FUNCTION
* Receives:
*	- The maximum number of threads: Pass get_max_threads() value
*	- A descriptive message for the common report header
*	- An optional "usage" string to show in case of command line errors
*	- The number of arguments to require in the command line
*	- An array of strings with the names of these arguments
*		(Used to build the default "usage" line and for report)
*	- An array of strings with the default values for these arguments
*	- The number of timers to be defined
*	- The number of timers to be automatically reported when execution ends
*	- An array of strings with the names of these reported timers
*
*	- argc, argv main arguments
*/
void OSCR_init(
	int	numThreads,
	char *	description,
	char *	usage,
	int	numArgs,
	char *	argNames[],
	char *	defaultValues[],
	int	numTimers,
	int	numReportedTimers,
	char *	reportedTimerNames[],
	int	argc,
	char *	argv[]
	) {

	int i;

	/* 1. CHECK IF INITIALIZED TWICE */
	if ( oscrData_init )
		OSCR_args_error("internal - Initializing twice!!", 0);
	oscrData_init = 1;

	/* 2. FIX THE NUMBER OF THREADS FOR INFORMATION REPORTING */
	oscrData_numThreads = numThreads;
	if ( numThreads < 1 )
		OSCR_error("Invalid number of threads", numThreads);

	/* 3. PRINT HEADER */
	printf(OSCR_HEADER);
//	printf("\nProgram:       %s\n", argv[0]);
	printf("Description:   %s\n", description);

	/* 4. NUMBER OF PARAMETERS AND ARGUMENT NAMES */
	/* 4.1. STORE POINTERS AND SIZES */
	oscrData_argNames = argNames;
	oscrData_argv = argv;
	oscrData_numArgs = numArgs;

	/* 4.2. IF EXPLICIT USAGE IS NULL, BUILD DEFAULT USAGE LINE */
	if ( usage != NULL ) {
		strcpy(oscrData_usage, usage);
		}
	else {
		strcpy(oscrData_usage, argv[0]);
		for (i=0; i<numArgs; i++) {
			strcat(oscrData_usage," <");
			strcat(oscrData_usage, argNames[i]);
			strcat(oscrData_usage,">");
			}
		}

	/* 4.3. CHECK THE -test ARGUMENT ALONE IN COMMAND-LINE */
	if ( argc==2 ) {
		if ( !strcmp(argv[1],OSCR_TESTARG) ) {
			oscrData_argv = (char **)calloc(numArgs+1,sizeof(char *));
			oscrData_argv[0] = argv[0];
			for (i=1; i<=numArgs; i++)
				oscrData_argv[i] = defaultValues[i-1];

			/* CHEAT THE FOLLOWING NUMBER-OF-ARGUMENTS CHECK */
			argc = numArgs+1;
			}
		}

	/* 4.4. CHECK IF THE NUMBER OF ARGUMENTS IS CORRECT */
	if ( argc-1 != numArgs )
		OSCR_args_error("Invalid number of arguments", 0);

	/* 4.5. WRITE THE ARGUMENTS VALUES AND BEGIN-EXECUTION LINE */
	printf("\n");
	for (i=1; i<argc; i++) {
		printf("Argument %s %s\n", argNames[i-1], oscrData_argv[i]);
		}
//	printf("Argument NUMTHREADS %d\n", oscrData_numThreads);
	printf("\nRunning ...\n\n");


	/* 5. TIMERS: INIT */
	/* 5.1. CHECK VALID NUMBER OF TIMERS */
	if (numTimers < 1)
		OSCR_error("internal - number of timers invalid!!",numTimers);

	/* 5.2. ALLOCATE TIMERS */
	oscrData_numTimers = numTimers;
	oscrTimers = (oscrTimer *)calloc(numTimers, sizeof(oscrTimer) );
	if ( oscrTimers == NULL )
		OSCR_error("Not enough memory for timers!!", -1);

	/* 5.3. INITIALIZE TIMERS */
	for (i=0; i<numTimers; i++) {
		oscrTimers[i].active = 0;
		oscrTimers[i].elapsed = 0.0;
		oscrTimers[i].start = 0.0;
		}

	/* 5.3. CHECK VALLID NUMBER OF REPORTED TIMERS */
	if ((numReportedTimers<0) || (numReportedTimers >  oscrData_numTimers))
		OSCR_error("internal - report: invalid number of Reported-Timers -> ", numReportedTimers);

	/* 5.4. STORE REPORTED TIMER NAMES */
	oscrData_numReportedTimers = numReportedTimers;
	oscrData_reportedTimerNames = reportedTimerNames;

	/* 5.5 PAPI init */
#ifdef __USE_PAPI
	init_PAPI(numTimers);
#endif

/* 6. END */
}

/*
* CHECK ARGUMENT INDEX
*/
void OSCR_getarg_check( int ind ) {
	/* 1. CHECK PROPER INDEX */
	if ( ! oscrData_init )
		OSCR_args_error("internal, OSCR_init not set before getting arg", ind);

	if ( (ind < 1) || (ind >  oscrData_numArgs) )
		OSCR_args_error("internal, getting invalid param number",ind);
	}

/*
* GET ARGUMENT: INTEGER
*/
int OSCR_getarg_int(int ind) {
	int check;
	int value;

	/* 1. CHECK PROPER INDEX */
	OSCR_getarg_check(ind);

	/* 2. CONVERT ARG */
	check = sscanf(oscrData_argv[ind],"%d", &value);

	/* 3. CHECK CONVERSION */
	if ( check != 1 )
		OSCR_args_error("Incorrect type in argument",ind);

	return value;
	}

/*
* GET ARGUMENT: DOUBLE
*/
double OSCR_getarg_double(int ind) {
	int check;
	double value;

	/* 1. CHECK PROPER INDEX */
	OSCR_getarg_check(ind);

	/* 2. CONVERT ARG */
	check = sscanf(oscrData_argv[ind],"%lf", &value);

	/* 3. CHECK CONVERSION */
	if ( check != 1 )
		OSCR_args_error("Incorrect type in argument",ind);

	return value;
	}

/*
* GET ARGUMENT: STRING
*/
char * OSCR_getarg_string(int ind) {
	/* 1. CHECK PROPER INDEX */
	OSCR_getarg_check(ind);

	/* 2. CONVERT ARG */
	/* 3. CHECK CONVERSION */

	return oscrData_argv[ind];
	}


/*
* CHECK TIMER INDEX
*/
void OSCR_timer_check(const char * routine, int ind) {
	char message[1024];

	/* 1. CHECK PROPER INDEX */
	if ( ! oscrData_init )
		OSCR_error("internal - Using a timer before initialize OmpSCR",ind);

	if ( (ind < 0) || (ind >= oscrData_numTimers) ) {
		sprintf(message,"internal - %s: invalid timer index ->", routine);
		OSCR_error(message,ind);
		}
	}


/*
* TIMERS: TIME CLEAR
*/
void OSCR_timer_clear(int ind) {
	OSCR_timer_check("timerClear", ind);
	oscrTimers[ind].elapsed = 0.0;
	}

/*
* TIMERS: TIME START
*/
void OSCR_timer_start(int ind) {
	OSCR_timer_check("timerStart", ind);

	if ( oscrTimers[ind].active )
		OSCR_error("internal - timerStart, timer already started ->",ind);
	oscrTimers[ind].active = 1;

#ifdef __USE_PAPI
	oscrTimers[ind].start = PAPI_get_real_usec() / SCALEPAPI;
#else
	oscrTimers[ind].start = OSCR_wtime();
#endif


	}

/*
* TIMERS: TIME STOP
*/
void OSCR_timer_stop(int ind) {
	OSCR_timer_check("timerStop", ind);

	if ( ! oscrTimers[ind].active )
		OSCR_error("internal - timerStop, timer already stoped ->",ind);
	oscrTimers[ind].active = 0;


#ifdef __USE_PAPI
	oscrTimers[ind].elapsed += ((PAPI_get_real_usec() / SCALEPAPI) - oscrTimers[ind].start);
#else
	oscrTimers[ind].elapsed += (OSCR_wtime() - oscrTimers[ind].start);
#endif
	}

/*
* TIMERS: TIME READ
*/
double OSCR_timer_read(int ind) {
	OSCR_timer_check("timerRead", ind);
	return oscrTimers[ind].elapsed;
	}


/*
* REPORT
*/
void OSCR_report() {
	int i;

	/* 1. WRITE TIMERS */
	for (i=0; i<oscrData_numReportedTimers; i++) {
		//printf("Timer %s %12.6lf\n", oscrData_reportedTimerNames[i], oscrTimers[i].elapsed );
		}
	printf("\n");
	}

#ifdef __USE_PAPI
#include <papi.h>
#include <papiStdEventDefs.h>

void init_PAPI(int numTimers)
{
  int i;

  /* Initialize the library */
  if (PAPI_library_init(PAPI_VER_CURRENT) != PAPI_VER_CURRENT)
    OSCR_error ("PAPI library init error!\n", -1);

  oscrData_timerPAPIEventSet = (int *)calloc(numTimers, sizeof(int));

  if (oscrData_timerPAPIEventSet == NULL)
    OSCR_error("Not enough memory for timers!!", -1);

  if (PAPI_thread_init((unsigned long (*)(void))(omp_get_thread_num()),0) != PAPI_OK)
		OSCR_error("PAPI Thread init error!", -1);

	for (i=0; i<numTimers; i++)
		oscrData_timerPAPIEventSet[i] = 0;
}

void start_EVENTPAPI(int ind, int EventCode)
{
/* Create the Event Set */
	if (PAPI_create_eventset(&oscrData_timerPAPIEventSet[ind]) != PAPI_OK)
		OSCR_error("PAPI create eventset error!\n", -1);

/* Add Total Instructions Executed to our EventSet */
	if (PAPI_add_event(&oscrData_timerPAPIEventSet[ind], EventCode) != PAPI_OK)
		OSCR_error("PAPI add events error!\n", -1);

	if (PAPI_start(oscrData_timerPAPIEventSet[ind]) != PAPI_OK)
		OSCR_error("PAPI start init error!\n", -1);
}

long_long stop_PAPI(int ind)
{
	long_long values[1];

	if (PAPI_stop(oscrData_timerPAPIEventSet[ind], values) != PAPI_OK)
		OSCR_error("PAPI stop init error!\n", -1);

	if (PAPI_cleanup_eventset(&oscrData_timerPAPIEventSet[ind]) != PAPI_OK)
		OSCR_error("PAPI cleanning Eventset error!\n", -1);

	if (PAPI_destroy_eventset(&oscrData_timerPAPIEventSet[ind]) != PAPI_OK)
		OSCR_error("PAPI destroying Eventset error!\n", -1);

 return (values[0]);
}
#endif







/*
* END oscrCommon.c
*/
