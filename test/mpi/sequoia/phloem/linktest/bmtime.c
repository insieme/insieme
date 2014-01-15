/*==================================================*/
/* MPI Link-Test for Sequoia                        */
/* Timing library / Header file                     */
/*==================================================*/

#include <sys/time.h>
#include "bmtime.h"


/*==================================================*/

bmtime_t bmtime_getTime()
{
  struct timeval tv;
  double v;

  gettimeofday(&tv,NULL);
  v=((double)tv.tv_sec)+(((double)tv.tv_usec)/1000000.0);

  return v;
}



/*==================================================*/
/* The End. */
