/*==================================================*/
/* MPI Link-Test for Sequoia                        */
/* Timing library / Header file                     */
/*==================================================*/

#ifndef BMTIME_H_
#define BMTIME_H_

/*#include "common.h"*/


/*==================================================*/
/* Macros and Constants */

#define BMTIME_TO_SEC(a) a
#define BMTIME_DIFF(a,b) (b-a)
#define BMTIME_MPITYPE MPI_DOUBLE

/*==================================================*/
/* Types */

typedef double bmtime_t;


/*==================================================*/
/* Prototypes */


bmtime_t bmtime_getTime();


/*==================================================*/
/* The End. */

#endif /* BMTIME_H_ */
