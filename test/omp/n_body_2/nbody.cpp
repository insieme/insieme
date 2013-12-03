/****************************************************************************
 * nbody.c
 * An implementation of the naive O(n^2)/timestep numerical integration
 * approach to the N-body problem.  Parallelism via OpenMP.
 *
 * Sean Ho, COMP203 10Mar2000
 ****************************************************************************/

#include "nbody.h"
#include <omp.h>

int main(int argc, char **argv) {

  mytspec start_time, end_time;
  int num_bodies, num_steps, max_threads=1;
  int i, j, k, l;
  double dt=1.0, dv[2];
  double r[2], dist, force_len, force_ij[2], tot_force_i[2];
  #if NEWTON_OPT
  double *forces_matrix;
  #endif

  Body *bodies;

  /* Parse command line */
  const char Usage[] = "Usage: nbody <num bodies> <num_steps>\n";
  if (argc < 2) {
    fprintf(stderr, Usage);
    exit(1);
  }

  num_bodies = atoi(argv[1]);
  num_steps = atoi(argv[2]);

  /* Initialize with OpenMP */

  #ifdef _OPENMP
  max_threads = omp_get_max_threads();
  #else
  printf("Warning: no OpenMP!\n");
  #endif

  #if NEWTON_OPT > 0
  printf("Using Newton's third law optimization, variant %d.\n", NEWTON_OPT);
  #endif 

  /**************************************************************************
   * Initialize bodies and allocate internal data structures
   *************************************************************************/

  printf("Initializing with %d threads, %d bodies, %d time steps\n",
		  max_threads, num_bodies, num_steps);

  bodies = init_bodies(num_bodies, INIT_SPIRAL);
  check_simulation(bodies, num_bodies);

  #if NEWTON_OPT == 1
  forces_matrix = (double *) malloc(sizeof(double)*2*num_bodies*num_bodies);
  #elif NEWTON_OPT == 2
  forces_matrix = (double *) malloc(sizeof(double)*2*num_bodies);
  #endif

  /* Start timing */
  printf("Running "); fflush(stdout);
  get_time(start_time);

  /**************************************************************************
   * Main loop
   *************************************************************************/

#define PRIVATE_VARS r, dist, force_len, force_ij, tot_force_i, dv

/* Store in force_ij[] the force on body i by body j */
#define Calc_Force_ij() \
  r[X] = bodies[j].pos[X] - bodies[i].pos[X]; \
  r[Y] = bodies[j].pos[Y] - bodies[i].pos[Y]; \
  dist = r[X]*r[X] + r[Y]*r[Y]; \
  force_len = GRAV_CONST * bodies[i].mass * bodies[j].mass  \
    / (dist*sqrt(dist)); \
  force_ij[X] = force_len * r[X]; \
  force_ij[Y] = force_len * r[Y]

/* Update velocity and position of body i, by numerical integration */
#define Step_Body_i() \
  dv[X] = dt * tot_force_i[X] / bodies[i].mass; \
  dv[Y] = dt * tot_force_i[Y] / bodies[i].mass; \
  bodies[i].pos[X] += dt * ( bodies[i].vel[X] + dv[X]/2 ); \
  bodies[i].pos[Y] += dt * ( bodies[i].vel[Y] + dv[Y]/2 ); \
  bodies[i].vel[X] += dt * dv[X]; \
  bodies[i].vel[Y] += dt * dv[Y]

  for (k=0; k<num_steps; k++) {
    printf(". "); fflush(stdout);

  #if !NEWTON_OPT

#pragma omp parallel for private(j, PRIVATE_VARS) 
    for (i=0; i<num_bodies; i++) {
      tot_force_i[X] = 0.0;
      tot_force_i[Y] = 0.0;

      /* Compute total force f(i) on each body i */
      for (j=0; j<num_bodies; j++) {
	if (j==i) continue;

	Calc_Force_ij();

	tot_force_i[X] += force_ij[X];
	tot_force_i[Y] += force_ij[Y];
      }

      Step_Body_i();
    }

  #elif NEWTON_OPT == 1

    #define forces(i,j,x) forces_matrix[x + 2*(j + num_bodies*i)]

    /* Fill in a big nxn table of forces */
#pragma omp parallel for private(j, PRIVATE_VARS) 
    for (i=0; i<num_bodies; i++) {
      for (j=i+1; j<num_bodies; j++) {
        Calc_Force_ij();

	forces(i, j, X) = force_ij[X];
	forces(i, j, Y) = force_ij[Y];
      }
    }

    /* Compute total force f(i) on each body i */
#pragma omp parallel for private(j, PRIVATE_VARS)
    for (i=0; i<num_bodies; i++) {
      tot_force_i[X] = 0.0;
      tot_force_i[Y] = 0.0;

      for (j=0; j<i; j++) {
        tot_force_i[X] -= forces(j, i, X);
        tot_force_i[Y] -= forces(j, i, Y);
      }
      for (j=i+1; j<num_bodies; j++) {
        tot_force_i[X] += forces(i, j, X);
        tot_force_i[Y] += forces(i, j, Y);
      }

      /* Update velocity and position of each body, by numerical integration */
      Step_Body_i();
    }

  #elif NEWTON_OPT == 2

    #define forces(i,x) forces_matrix[x + 2*i]

#pragma omp parallel for private(j, PRIVATE_VARS) 
    for (i=0; i<num_bodies; i++) {
      for (j=i+1; j<num_bodies; j++) {
        Calc_Force_ij();

#pragma omp critical
        {
          forces(i,X) += force_ij[X];
          forces(i,Y) += force_ij[Y];
	  forces(j,X) -= force_ij[X];
	  forces(j,Y) -= force_ij[Y];
	}
      }
    }

#pragma omp parallel for private(j, PRIVATE_VARS)
    for (i=0; i<num_bodies; i++) {
      Step_Body_i();
    }

  #endif /* !NEWTON_OPT */

  }

  /* Stop timing */
  get_time(end_time);
  check_simulation(bodies, num_bodies);

  printf("done!  interaction rate: \n\n");/*,
    num_bodies * (num_bodies-1) * num_steps /
    elapsed_time(end_time, start_time) / 1000);*/

  return(0);
}


/****************************************************************************
 * Allocate and setup initial conditions for the bodies
 ****************************************************************************/
Body *init_bodies(unsigned int num_bodies, int init_type) {
  int i;
  double n = num_bodies;
  Body *bodies = (Body *) malloc(num_bodies * sizeof(Body));

  for (i=0; i<num_bodies; i++) {
    switch (init_type) {
    case INIT_LINEAR:
      bodies[i].mass = 1.0;
      bodies[i].pos[X] = i/n;
      bodies[i].pos[Y] = i/n;
      bodies[i].vel[X] = 0.0;
      bodies[i].vel[Y] = 0.0;
      break;
    case INIT_SPIRAL:
      bodies[i].mass = (n-i)/n;
      bodies[i].pos[X] = (1+i/n) * cos(2*M_PI*i/n) / 2;
      bodies[i].pos[Y] = (1+i/n) * sin(2*M_PI*i/n) / 2;
      bodies[i].vel[X] = 0.0;
      bodies[i].vel[Y] = 0.0;
      break;
    }
  }

  return bodies;
}

/****************************************************************************
 * Verify that the simulation is running correctly:
 * it should satisfy the invariant of conservation of momentum
 ****************************************************************************/
int check_simulation(Body *bodies, int num_bodies) {
  int i, check_ok;
  double momentum[2] = { 0.0, 0.0 };

  for (i=0; i<num_bodies; i++) {
    momentum[X] += bodies[i].mass * bodies[i].vel[X];
    momentum[Y] += bodies[i].mass * bodies[i].vel[Y];
  }

  check_ok = ((abs(momentum[X]) < EPSILON) && (abs(momentum[Y]) < EPSILON));
  if (!check_ok) printf("Warning: total momentum = (%3.3f, %3.3f)\n", 
		  momentum[X], momentum[Y]);
  return check_ok;
}

/****************************************************************************
 * elapsed time in seconds for POSIX-compliant clocks
 ****************************************************************************/
#ifdef GNU_TIME
double elapsed_time(const mytspec t2, const mytspec t1) {
  return 1.0 * (t2 - t1) / CLOCKS_PER_SEC;
}
#else
double elapsed_time(const mytspec t2, const mytspec t1) {
  return (((double)t2.tv_sec) + ((double)t2.tv_nsec / 1e9))
       - (((double)t1.tv_sec) + ((double)t1.tv_nsec / 1e9));
}
#endif

