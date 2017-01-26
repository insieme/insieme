/*****************************************************************************
 * Sean Ho COMP203 Programming Assignment 1
 *
 * For the optimized version using Newton's third law, I calculated the forces
 * f_ij in parallel, but serialized the increments of the total forces on each
 * body.  Unfortunately, as the charts show, this actually slowed down the
 * process as compared to the straightforward version, probably because the
 * serialized increments were dominating.
 */


#ifndef _NBODY_H
#define _NBODY_H

/* Needed includes */
#include <stdlib.h>	/* atoi */
#include <stdio.h>	/* fprintf */
#include <time.h>	/* clock/clock_gettime */
#include <malloc.h>	/* malloc */
#include <math.h>	/* sqrt */

#ifdef WITH_OPENMP
#include <omp.h>
#endif

/* Some constants */
#define GRAV_CONST 6.673e-11	/* m^3/(kg*s^2) */
#define EPSILON 1e-12

/* for init_bodies */
#define INIT_LINEAR 0
#define INIT_SPIRAL 1

/* ickiness in naming of time stuff */
#define GNU_TIME
#ifdef GNU_TIME
#  define __need_clock_t
#  define mytspec clock_t
#  define get_time(tspec) tspec = clock()
#else
#  define mytspec timespec_t
#  define get_time(tspec) clock_gettime(CLOCK_SGI_CYCLE,&tspec)
#endif

#define X 0
#define Y 1

#if 0
#ifndef _BOOL
#endif

               #ifdef _STANDARD_C_PLUS_PLUS
                   // If -LANG:std is specified, it defines the macro
                   // in the preceding line.  Use new-style headers
                   // and a using directive to bring names from the
                   // std namespace into the global namespace.
                   #include<complex>
                   #include<iostream>
                   using namespace std;
               #else
                   // If -LANG:std is not specified, use old-style headers,
                   // and there is no need for a using directive.
                   #include<complex.h>
                   #include<iostream.h>
               #endif

               complex<float> x(1,2);
#endif

/* the Body structure */
struct Body_struct {
  double mass;
  double pos[2];
  double vel[2];
};
typedef struct Body_struct Body;

/* Subroutines in nbody.c */
Body* init_bodies(unsigned int num_bodies, int init_type);
int check_simulation(Body *bodies, int num_bodies);
double elapsed_time(const mytspec t2, const mytspec t1);

#endif /* _NBODY_H */
