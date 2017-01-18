/******************************************************************************\
*                                                                              *
*        Copyright (c) 2003, The Regents of the University of California       *
*      See the file COPYRIGHT for a complete copyright notice and license.     *
*                                                                              *
********************************************************************************
*
* CVS info:
*   $RCSfile: iordef.h,v $
*   $Revision: 1.116 $
*   $Date: 2007/09/07 16:25:16 $
*   $Author: loewe $
*
* Purpose:
*       This is a header file that contains the definitions and macros 
*       needed for IOR.
*
\******************************************************************************/

#ifndef _IORDEF_H
#define _IORDEF_H
#include <mpi.h>
#include <stdio.h>
#include <errno.h>

/************************** D E C L A R A T I O N S ***************************/

extern int numTasks,                           /* MPI variables */
           rank,
           rankOffset,
           verbose;                            /* verbose output */


/*************************** D E F I N I T I O N S ****************************/

#define IOR_RELEASE        "IOR-2.10.1"

#ifndef FALSE
#   define FALSE           0
#endif /* not FALSE */

#ifndef TRUE
#   define TRUE            1
#endif /* not TRUE */

#ifndef NULL
#   define NULL            ((void *)0)
#endif /* not NULL */

#define KILOBYTE           1000
#define MEGABYTE           1000000
#define GIGABYTE           1000000000

#define KIBIBYTE           (1 << 10)
#define MEBIBYTE           (1 << 20)
#define GIBIBYTE           (1 << 30)

/* for displaying MiB or MB */
#define BASE_TWO           0
#define BASE_TEN           1

/* any write/read access in code */
#define WRITE              0
#define WRITECHECK         1
#define READ               2
#define READCHECK          3
#define CHECK              4

/* verbosity settings */
#define VERBOSE_0          0
#define VERBOSE_1          1
#define VERBOSE_2          2
#define VERBOSE_3          3
#define VERBOSE_4          4
#define VERBOSE_5          5

#define MAX_STR            1024                /* max string length */  
#define MAX_HINTS          16                  /* max number of hints */  
#define MAX_RETRY          10000               /* max retries for POSIX xfer */

#define DELIMITERS         " \t\r\n="          /* ReadScript() */
#define FILENAME_DELIMITER '@'                 /* ParseFileName() */

/* MACROs for debugging */
#define HERE               fprintf(stdout, "** LINE %d (TASK=%d) **\n", \
                                   __LINE__, rank);

/* Other MACROs */
#define USE_UNDOC_OPT      TRUE                /* USE UNDOCumented OPTion */

typedef long long int      IOR_offset_t;
typedef long long int      IOR_size_t;


/******************************** M A C R O S *********************************/

/******************************************************************************/
/*
 * WARN_RESET will display a custom error message and set value to default
 */
#define WARN_RESET(MSG, VAL) do {                                        \
        test->VAL = defaultParameters.VAL;                               \
        if (rank == 0) {                                                 \
            fprintf(stdout, "WARNING: %s.  Using value of %d.\n",        \
                    MSG, test->VAL);                                     \
        }                                                                \
        fflush(stdout);                                                  \
} while (0)


/******************************************************************************/
/*
 * WARN will display a custom error message as well as an error string from
 * sys_errlist, but will not exit the program
 */

#define WARN(MSG) do {                                                   \
        if (verbose > VERBOSE_2) {                                       \
            fprintf(stdout, "WARNING: %s, in %s (line %d).\n",           \
                    MSG, __FILE__, __LINE__);                            \
        } else {                                                         \
            fprintf(stdout, "WARNING: %s.\n", MSG);                      \
        }                                                                \
        fflush(stdout);                                                  \
} while (0)


/******************************************************************************/
/*
 * ERR will display a custom error message as well as an error string from
 * sys_errlist and then exit the program
 */

#define ERR(MSG) do {                                                    \
        fprintf(stdout, "** error **\n");                                \
        fprintf(stdout, "ERROR in %s (line %d): %s.\n",                  \
                __FILE__, __LINE__, MSG);                                \
        fprintf(stdout, "ERROR: %s\n", strerror(errno));                 \
        fprintf(stdout, "** exiting **\n");                              \
        fflush(stdout);                                                  \
        MPI_Abort(MPI_COMM_WORLD, -1);                                   \
} while (0)


/******************************************************************************/
/*
 * MPI_CHECK will display a custom error message as well as an error string
 * from the MPI_STATUS and then exit the program
 */

#define MPI_CHECK(MPI_STATUS, MSG) do {                                  \
    char resultString[MPI_MAX_ERROR_STRING];                             \
    int resultLength;                                                    \
                                                                         \
    if (MPI_STATUS != MPI_SUCCESS) {                                     \
        fprintf(stdout, "** error **\n");                                \
        fprintf(stdout, "ERROR in %s (line %d): %s.\n",                  \
                __FILE__, __LINE__, MSG);                                \
        MPI_Error_string(MPI_STATUS, resultString, &resultLength);       \
        fprintf(stdout, "MPI %s\n", resultString);                       \
        fprintf(stdout, "** exiting **\n");                              \
        fflush(stdout);                                                  \
        MPI_Abort(MPI_COMM_WORLD, -1);                                   \
    }                                                                    \
} while(0)

#endif /* not _IORDEF_H */
