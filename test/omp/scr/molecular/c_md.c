/* ***********************************************************************
  This program is part of the
	OpenMP Source Code Repository

	http://www.pcg.ull.es/ompscr/
	e-mail: ompscr@etsii.ull.es

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  (LICENSE file) along with this program; if not, write to
  the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
  Boston, MA  02111-1307  USA

	FILE:              c_md.c
  VERSION:           1.0
  DATE:              May 2004
  AUTHOR:            Bill Magro, Kuck and Associates, Inc. (KAI), 1998
  COMMENTS TO:       sande@csi.ull.es
  DESCRIPTION:       This program implements a simple molecular dynamics simulation,
                     using the velocity Verlet time integration scheme.
										 The particles interact with a central pair potential.
  COMMENTS:
  REFERENCES:        W. C. Swope and H. C. Andersen and P. H. Berens and K. R.  Wilson
                     A Computer Simulation Method for the Calculation of
                     Equilibrium Constants for the Formation of Physical
                     Clusters of Molecules: Application to Small Water Clusters
                     Journal of Chemical Physics, 1982 vol. 76 pg 637-649
  BASIC PRAGMAS:     parallel for
	USAGE:             ./c_md.par 8192 10
  INPUT:             Number of particles
	                   Number of simulation steps
  OUTPUT:            -
	FILE FORMATS:      -
	RESTRICTIONS:      -
	REVISION HISTORY:
**************************************************************************/
#include "OmpSCR.h"
#include <math.h>

#ifndef RAND_MAX
#define RAND_MAX 0x7fff
#endif

#ifndef M_PI_2
#define M_PI_2   1.57079632679489661923  /* pi/2 */
#endif

#define NUM_ARGS				2
#define NUM_TIMERS			1
#define DEFAULT_NPARTS 	8192
#define DEFAULT_NSTEPS 	10
#define USAGE_STR "NPARTS NSTEPS"
#define NDIM 3

int NPARTS;       /* No. of particles */
int NSTEPS;       /* No. of simulation steps */

typedef double vnd_t[NDIM];

/* -----------------------------------------------------------------------
                          PROTOTYPES
 * ----------------------------------------------------------------------- */

double v(double x);
double dv(double x);
void initialize(int np, int nd, vnd_t box, vnd_t *pos, vnd_t *vel, vnd_t *acc);
double dist(int nd, vnd_t r1, vnd_t r2, vnd_t dr);
double dot_prod(int n, vnd_t x,vnd_t y);
void compute(int np, int nd, vnd_t *pos, vnd_t *vel, double mass, vnd_t *f, double *pot_p, double *kin_p);
void update(int np, int nd, vnd_t *pos, vnd_t *vel, vnd_t *f, vnd_t *a, double mass, double dt);
int main (int argc, char **argv);

/* -----------------------------------------------------------------------
                          IMPLEMENTATION
 * ----------------------------------------------------------------------- */
/* -----------------------------------------------------------------------
   statement function for the pair potential.
   This potential is a harmonic well which smoothly saturates to a
   maximum value at PI/2.
 * ----------------------------------------------------------------------- */
double v(double x) {
  if (x < M_PI_2)
    return pow(sin(x), 2.0);
  else
    return 1.0;
}
/* -----------------------------------------------------------------------
   statement function for the derivative of the pair potential
 * ----------------------------------------------------------------------- */
double dv(double x) {
  if (x < M_PI_2)
    return 2.0 * sin(x) * cos(x);
  else
    return 0.0;
}
/* -----------------------------------------------------------------------
   Initialize the positions, velocities, and accelerations.
 * ----------------------------------------------------------------------- */
void initialize(int np, int nd, vnd_t box, vnd_t *pos, vnd_t *vel, vnd_t *acc) {
  int i, j;
  double x;

  srand(4711L);
  for (i = 0; i < np; i++) {
    for (j = 0; j < nd; j++) {
      x = rand() % 10000 / (double)10000.0;
      pos[i][j] = box[j] * x;
      vel[i][j] = 0.0;
      acc[i][j] = 0.0;
    }
  }
}
/* -----------------------------------------------------------------------
   Compute the displacement vector (and its norm) between two particles.
 * ----------------------------------------------------------------------- */
double dist(int nd, vnd_t r1, vnd_t r2, vnd_t dr) {
  int i;
  double d;

  d = 0.0;
  for (i = 0; i < nd; i++) {
    dr[i] = r1[i] - r2[i];
    d += dr[i] * dr[i];
  }
  return sqrt(d);
}
/* -----------------------------------------------------------------------
   Return the dot product between two vectors of type double and length n
 * ----------------------------------------------------------------------- */
double dot_prod(int n, vnd_t x, vnd_t y) {
  int i;
  double t = 0.0;

  for (i = 0; i < n; i++) {
    t += x[i] * y[i];
  }
  return t;
}
/* -----------------------------------------------------------------------
   Compute the forces and energies, given positions, masses,
   and velocities
 * ----------------------------------------------------------------------- */
void compute(int np, int nd, vnd_t *pos, vnd_t *vel,
	     double mass, vnd_t *f, double *pot_p, double *kin_p) {
  int i, j, k;
  vnd_t rij;
  double  d;
  double pot, kin;

  pot = 0.0;
  kin = 0.0;
  /* The computation of forces and energies is fully parallel. */
#pragma omp parallel for default(shared) private(i, j, k, rij, d) reduction(+ : pot, kin)
  for (i = 0; i < np; i++) {
    /* compute potential energy and forces */
    for (j = 0; j < nd; j++)
      f[i][j] = 0.0;
    for (j = 0; j < np; j++) {
      if (i != j) {
        d = dist(nd, pos[i], pos[j], rij);
        /* attribute half of the potential energy to particle 'j' */
        pot = pot + 0.5 * v(d);
        for (k = 0; k < nd; k++) {
          f[i][k] = f[i][k] - rij[k]* dv(d) /d;
        }
     }
   }
   /* compute kinetic energy */
   kin = kin + dot_prod(nd, vel[i], vel[j]);
  }
  kin = kin * 0.5 * mass;
  *pot_p = pot;
  *kin_p = kin;
}
/* -----------------------------------------------------------------------
   Perform the time integration, using a velocity Verlet algorithm
 * ----------------------------------------------------------------------- */
void update(int np, int nd, vnd_t *pos, vnd_t *vel, vnd_t *f, vnd_t *a, double mass, double dt) {
  int i, j;
  double rmass;

  rmass = 1.0/mass;
  /* The time integration is fully parallel */
#pragma omp parallel for default(shared) private(i, j) firstprivate(rmass, dt)
  for (i = 0; i < np; i++) {
    for (j = 0; j < nd; j++) {
      pos[i][j] = pos[i][j] + vel[i][j]*dt + 0.5*dt*dt*a[i][j];
      vel[i][j] = vel[i][j] + 0.5*dt*(f[i][j]*rmass + a[i][j]);
      a[i][j] = f[i][j]*rmass;
    }
  }
}
/* ----------------------------------------------------------------------- */
int main (int argc, char **argv) {
  /* simulation parameters */
  double mass = 1.0;
  double dt = 1.0e-4;
  vnd_t box;
  vnd_t *position;
  vnd_t *velocity;
  vnd_t *force;
  vnd_t *accel;
  double potential, kinetic, E0;
  int i;
	int NUMTHREADS;
	double total_time;
	char *PARAM_NAMES[NUM_ARGS] = {"Nparts", "Nsteps"};
	char *TIMERS_NAMES[NUM_TIMERS] = {"Total_time" };
  char *DEFAULT_VALUES[NUM_ARGS] = {"8192", "10"};


  NUMTHREADS = omp_get_max_threads();
  OSCR_init (NUMTHREADS, "Molecular dynamic simulation", "Use md <Nparts> <Nsteps>", NUM_ARGS,
    PARAM_NAMES, DEFAULT_VALUES , NUM_TIMERS, NUM_TIMERS, TIMERS_NAMES,
    argc, argv);


	NPARTS = OSCR_getarg_int(1);
  NSTEPS = OSCR_getarg_int(2);
  /* Default: DEFAULT_NPARTS, DEFAULT_NSTEPS */

	/* Memory allocation */
	position = OSCR_calloc(NPARTS, sizeof(vnd_t));
	velocity = OSCR_calloc(NPARTS, sizeof(vnd_t));
	force    = OSCR_calloc(NPARTS, sizeof(vnd_t));
	accel    = OSCR_calloc(NPARTS, sizeof(vnd_t));

  NUMTHREADS = omp_get_max_threads();
  for (i = 0; i < NDIM; i++)
    box[i] = 10.0;
  /* set initial positions, velocities, and accelerations */
  initialize(NPARTS, NDIM, box, position, velocity, accel);
	OSCR_timer_start(0);
  /* compute the forces and energies */
  compute(NPARTS, NDIM, position, velocity, mass, force, &potential, &kinetic);
  E0 = potential + kinetic;
  /* This is the main time stepping loop */
  for (i = 0; i < NSTEPS; i++) {
    compute(NPARTS, NDIM, position, velocity, mass, force, &potential, &kinetic);
#if 0
    printf("%17.9e %17.9e %17.9e\n", potential, kinetic, (potential + kinetic - E0) / E0);
#endif
    update(NPARTS, NDIM, position, velocity, force, accel, mass, dt);
  }
	OSCR_timer_stop(0);
  total_time = OSCR_timer_read(0);
	OSCR_report(1, TIMERS_NAMES);
	printf("\n \t# THREADS \tTIME (secs.) \n");
	//printf("\t %d \t\t%14.6lf\n", NUMTHREADS, total_time);
    printf("\t %d \t\t\n", NUMTHREADS);


  return 0;
}


/*
 * vim:ts=2:sw=2:
 */
