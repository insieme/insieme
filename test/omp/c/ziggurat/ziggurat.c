# include <stdlib.h>
# include <stdio.h>
# include <math.h>
# include <time.h>
# include <stdint.h>
# include <omp.h>

int main ( );
void test01 ( );
void test02 ( );
void test03 ( );
void test04 ( );
float r4_exp ( uint32_t *jsr, uint32_t ke[256], float fe[256], float we[256] );
void r4_exp_setup ( uint32_t ke[256], float fe[256], float we[256] );
float r4_nor ( uint32_t *jsr, uint32_t kn[128], float fn[128], float wn[128] );
void r4_nor_setup ( uint32_t kn[128], float fn[128], float wn[128] );
float r4_uni ( uint32_t *jsr );
uint32_t shr3_seeded ( uint32_t *jsr );
void timestamp ( );

/******************************************************************************/

int main ( )

/******************************************************************************/
/*
  Purpose:

    MAIN is the main program for ZIGGURAT_OPENMP.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    04 October 2013

  Author:

    John Burkardt
*/
{
  //timestamp ( );
  printf ( "\n" );
  printf ( "ZIGGURAT_OPENMP:\n" );
  printf ( "  C version\n" );

  printf ( "\n" );
  printf ( "  Number of processors available = %d\n", omp_get_num_procs ( ) );
  printf ( "  Number of threads =              %d\n", omp_get_max_threads ( ) );

  double start=omp_get_wtime();

  test01 ( );
  test02 ( );
  test03 ( );
  test04 ( );

  double end=omp_get_wtime();
  //printf("total time: %.4f\n",end-start);
/*
  Terminate.
*/
  printf ( "\n" );
  printf ( "ZIGGURAT_OPENMP:\n" );
  printf ( "  Normal end of execution.\n" );
  printf ( "\n" );
  //timestamp ( );

  return 0;
}
/******************************************************************************/

void test01 ( )

/******************************************************************************/
/*
  Purpose:

    TEST01 tests SHR3_SEEDED.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    04 October 2013

  Author:

    John Burkardt
*/
{
  uint32_t jsr;
  uint32_t jsr_value;
  double mega_rate_par;
  double mega_rate_seq;
  int r;
  int r_num = 1000;
  int *result_par;
  int *result_seq;
  int s;
  int s_num = 10000;
  uint32_t *seed;
  int thread;
  int thread_num;
  double wtime_par;
  double wtime_seq;

  printf ( "\n" );
  printf ( "TEST01\n" );
  printf ( "  SHR3_SEEDED computes random integers.\n" );
  printf ( "  Since the output is completely determined\n" );
  printf ( "  by the input value of SEED, we can run in\n" );
  printf ( "  parallel as long as we make an array of seeds.\n" );
/*
  Set up the SEED array, which will be used for both sequential and
  parallel computations.
*/
# pragma omp parallel
{
# pragma omp master 
  {
    thread_num = omp_get_num_threads ( );

    printf ( "\n" );
    printf ( "  The number of threads is %d\n", thread_num );
  }
}
  seed = ( uint32_t * ) malloc ( thread_num * sizeof ( uint32_t ) );
  result_seq = ( int * ) malloc ( thread_num * sizeof ( int ) );
  result_par = ( int * ) malloc ( thread_num * sizeof ( int ) );
/*
  Sequential execution.
  The sequential execution will only match the parallel execution if we can
  guarantee that the parallel threads are scheduled to execute the R loop
  consecutively.
*/
  jsr = 123456789;

  for ( thread = 0; thread < thread_num; thread++ )
  {
    seed[thread] = shr3_seeded ( &jsr );
  }

  wtime_seq = omp_get_wtime ( );

  for ( r = 0; r < r_num; r++ )
  {
    thread = ( r % thread_num );

    jsr = seed[thread];

    for ( s = 0; s < s_num; s++ )
    {
      jsr_value = shr3_seeded ( &jsr );
    }
    result_seq[thread] = jsr_value;
    seed[thread] = jsr;
  }

  wtime_seq = omp_get_wtime ( ) - wtime_seq;

  mega_rate_seq = ( double ) ( r_num ) * ( double ) ( s_num ) / wtime_seq 
    / 1000000.0;
/*
  Parallel.
*/
  jsr = 123456789;

  for ( thread = 0; thread < thread_num; thread++ )
  {
    seed[thread] = shr3_seeded ( &jsr );
  }

  wtime_par = omp_get_wtime ( );

# pragma omp parallel \
  shared ( result_par, seed ) \
  private ( jsr, jsr_value, r, s, thread )
  {

# pragma omp for schedule ( static, 1 )

    for ( r = 0; r < r_num; r++ )
    {
      thread = omp_get_thread_num ( );
 
      jsr = seed[thread];

      for ( s = 0; s < s_num; s++ )
      {
        jsr_value = shr3_seeded ( &jsr );
      }
      result_par[thread] = jsr_value;
      seed[thread] = jsr;
    }
  }

  wtime_par = omp_get_wtime ( ) - wtime_par;

  mega_rate_par = ( double ) ( r_num ) * ( double ) ( s_num ) / wtime_par 
    / 1000000.0;
/*
  Report.
*/
  printf ( "\n" );
  printf ( "  Correctness check:\n" );
  printf ( "\n" );
  printf ( "  Computing values sequentially should reach the\n" );
  printf ( "  same result as doing it in parallel:\n" );
  printf ( "\n" );
  printf ( "    THREAD    Sequential      Parallel    Difference\n" );
  printf ( "\n" );

  for ( thread = 0; thread < thread_num; thread++ )
  {
    printf ( "  %8d  %12d  %12d  %12d\n",  thread, result_seq[thread], 
      result_par[thread], result_seq[thread] - result_par[thread] );
  }

  printf ( "\n" );
  printf ( "  Efficiency check:\n" );
  printf ( "\n" );
  printf ( "  Computing values in parallel should be faster:\n" );
  printf ( "\n" );
  printf ( "              Sequential      Parallel\n" );
  printf ( "\n" );
 // printf ( "      TIME:  %14f  %14f\n", wtime_seq, wtime_par  );
//  printf ( "      RATE:  %14f  %14f\n", mega_rate_seq, mega_rate_par );
/*
  Free memory.
*/
  free ( result_par );
  free ( result_seq );
  free ( seed );

  return;
}
/******************************************************************************/

void test02 ( )

/******************************************************************************/
/*
  Purpose:

    TEST02 tests R4_UNI.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    04 October 2013

  Author:

    John Burkardt
*/
{
  uint32_t jsr;
  uint32_t jsr_value;
  double mega_rate_par;
  double mega_rate_seq;
  int r;
  int r_num = 1000;
  float r4_value;
  float *result_par;
  float *result_seq;
  int s;
  int s_num = 10000;
  uint32_t *seed;
  int thread;
  int thread_num;
  double wtime_par;
  double wtime_seq;

  printf ( "\n" );
  printf ( "TEST02\n" );
  printf ( "  R4_UNI computes uniformly random single precision real values.\n" );
  printf ( "  Since the output is completely determined\n" );
  printf ( "  by the input value of SEED, we can run in\n" );
  printf ( "  parallel as long as we make an array of seeds.\n" );
/*
  Set up the SEED array, which will be used for both sequential and
  parallel computations.
*/
# pragma omp parallel
{
# pragma omp master 
  {
    thread_num = omp_get_num_threads ( );

    printf ( "\n" );
    printf ( "  The number of threads is %d\n", thread_num );
  }
}
  seed = ( uint32_t * ) malloc ( thread_num * sizeof ( uint32_t ) );
  result_seq = ( float * ) malloc ( thread_num * sizeof ( float ) );
  result_par = ( float * ) malloc ( thread_num * sizeof ( float ) );
/*
  Sequential execution.
  The sequential execution will only match the parallel execution if we can
  guarantee that the parallel threads are scheduled to execute the R loop
  consecutively.
*/
  jsr = 123456789;

  for ( thread = 0; thread < thread_num; thread++ )
  {
    seed[thread] = shr3_seeded ( &jsr );
  }

  wtime_seq = omp_get_wtime ( );

  for ( r = 0; r < r_num; r++ )
  {
    thread = ( r % thread_num );

    jsr = seed[thread];

    for ( s = 0; s < s_num; s++ )
    {
      r4_value = r4_uni ( &jsr );
    }
    result_seq[thread] = r4_value;
    seed[thread] = jsr;
  }

  wtime_seq = omp_get_wtime ( ) - wtime_seq;

  mega_rate_seq = ( double ) ( r_num ) * ( double ) ( s_num ) / wtime_seq / 1000000.0;
/*
  Parallel.
*/
  jsr = 123456789;

  for ( thread = 0; thread < thread_num; thread++ )
  {
    seed[thread] = shr3_seeded ( &jsr );
  }

  wtime_par = omp_get_wtime ( );

# pragma omp parallel \
  shared ( result_par, seed ) \
  private ( jsr, r, r4_value, s, thread )
  {

# pragma omp for schedule ( static, 1 )

    for ( r = 0; r < r_num; r++ )
    {
      thread = omp_get_thread_num ( );
 
      jsr = seed[thread];

      for ( s = 0; s < s_num; s++ )
      {
        r4_value = r4_uni ( &jsr );
      }
      result_par[thread] = r4_value;
      seed[thread] = jsr;
    }
  }

  wtime_par = omp_get_wtime ( ) - wtime_par;

  mega_rate_par = ( double ) ( r_num ) * ( double ) ( s_num ) / wtime_par / 1000000.0;
/*
  Report.
*/
  printf ( "\n" );
  printf ( "  Correctness check:\n" );
  printf ( "\n" );
  printf ( "  Computing values sequentially should reach the\n" );
  printf ( "  same result as doing it in parallel:\n" );
  printf ( "\n" );
  printf ( "    THREAD    Sequential        Parallel      Difference\n" );
  printf ( "\n" );

  for ( thread = 0; thread < thread_num; thread++ )
  {
    printf ( "  %8d  %14f  %14f  %14f\n", thread, result_seq[thread],
      result_par[thread], result_seq[thread] - result_par[thread] );
  }

  printf ( "\n" );
  printf ( "  Efficiency check:\n" );
  printf ( "\n" );
  printf ( "  Computing values in parallel should be faster:'\n" );
  printf ( "\n" );
  printf ( "              Sequential      Parallel\n" );
  printf ( "\n" );
//  printf ( "      TIME:  %14f  %14f\n", wtime_seq, wtime_par  );
//  printf ( "      RATE:  %14f  %14f\n", mega_rate_seq, mega_rate_par );
/*
  Free memory.
*/
  free ( result_par );
  free ( result_seq );
  free ( seed );

  return;
}
/******************************************************************************/

void test03 ( )

/******************************************************************************/
/*
  Purpose:

    TEST03 tests R4_NOR.

  Discussion:

    The arrays FN, KN and WN, once set up by R4_NOR_SETUP, are "read only" when
    accessed by R4_NOR.  So we only need to have one copy of these arrays,
    and they can be shared.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    04 October 2013

  Author:

    John Burkardt
*/
{
  float fn[128];
  uint32_t jsr;
  uint32_t jsr_value;
  uint32_t kn[128];
  double mega_rate_par;
  double mega_rate_seq;
  int r;
  int r_num = 1000;
  float r4_value;
  float *result_par;
  float *result_seq;
  int s;
  int s_num = 10000;
  uint32_t *seed;
  int thread;
  int thread_num;
  float wn[128];
  double wtime_par;
  double wtime_seq;

  printf ( "\n" );
  printf ( "TEST03\n" );
  printf ( "  R4_NOR computes normal random single precision real values.\n" );
  printf ( "  Since the output is completely determined\n" );
  printf ( "  by the input value of SEED and the tables, we can run in\n" );
  printf ( "  parallel as long as we make an array of seeds and share the tables.\n" );
/*
  Set up the SEED array and the tables, which will be used for both sequential
  and parallel computations.
*/
# pragma omp parallel
{
# pragma omp master 
  {
    thread_num = omp_get_num_threads ( );

    printf ( "\n" );
    printf ( "  The number of threads is %d\n", thread_num );
  }
}
  seed = ( uint32_t * ) malloc ( thread_num * sizeof ( uint32_t ) );
  result_seq = ( float * ) malloc ( thread_num * sizeof ( float ) );
  result_par = ( float * ) malloc ( thread_num * sizeof ( float ) );

  r4_nor_setup ( kn, fn, wn );
/*
  Sequential execution.
  The sequential execution will only match the parallel execution if we can
  guarantee that the parallel threads are scheduled to execute the R loop
  consecutively.
*/
  jsr = 123456789;

  for ( thread = 0; thread < thread_num; thread++ )
  {
    seed[thread] = shr3_seeded ( &jsr );
  }

  wtime_seq = omp_get_wtime ( );

  for ( r = 0; r < r_num; r++ )
  {
    thread = ( r % thread_num );

    jsr = seed[thread];

    for ( s = 0; s < s_num; s++ )
    {
      r4_value = r4_nor ( &jsr, kn, fn, wn );
    }
    result_seq[thread] = r4_value;
    seed[thread] = jsr;
  }

  wtime_seq = omp_get_wtime ( ) - wtime_seq;

  mega_rate_seq = ( double ) ( r_num ) * ( double ) ( s_num ) / wtime_seq / 1000000.0;
/*
  Parallel.
*/
  jsr = 123456789;

  for ( thread = 0; thread < thread_num; thread++ )
  {
    seed[thread] = shr3_seeded ( &jsr );
  }

  wtime_par = omp_get_wtime ( );

# pragma omp parallel \
  shared ( result_par, seed ) \
  private ( jsr, r, r4_value, s, thread )
  {

# pragma omp for schedule ( static, 1 )

    for ( r = 0; r < r_num; r++ )
    {
      thread = omp_get_thread_num ( );
 
      jsr = seed[thread];

      for ( s = 0; s < s_num; s++ )
      {
        r4_value = r4_nor ( &jsr, kn, fn, wn );
      }
      result_par[thread] = r4_value;
      seed[thread] = jsr;
    }
  }

  wtime_par = omp_get_wtime ( ) - wtime_par;

  mega_rate_par = ( double ) ( r_num ) * ( double ) ( s_num ) / wtime_par 
    / 1000000.0;
/*
  Report.
*/
  printf ( "\n" );
  printf ( "  Correctness check:\n" );
  printf ( "\n" );
  printf ( "  Computing values sequentially should reach the\n" );
  printf ( "  same result as doing it in parallel:\n" );
  printf ( "\n" );
  printf ( "    THREAD    Sequential        Parallel      Difference\n" );
  printf ( "\n" );

  for ( thread = 0; thread < thread_num; thread++ )
  {
    printf ( "  %8d  %14f  %14f  %14f\n", thread, result_seq[thread],
      result_par[thread], result_seq[thread] - result_par[thread] );
  }

  printf ( "\n" );
  printf ( "  Efficiency check:\n" );
  printf ( "\n" );
  printf ( "  Computing values in parallel should be faster:\n" );
  printf ( "\n" );
  printf ( "              Sequential      Parallel\n" );
  printf ( "\n" );
//  printf ( "      TIME:  %14f  %14f\n", wtime_seq, wtime_par  );
//  printf ( "      RATE:  %14f  %14f\n", mega_rate_seq, mega_rate_par );
/*
  Free memory.
*/
  free ( result_par );
  free ( result_seq );
  free ( seed );

  return;
}
/******************************************************************************/

void test04 ( )

/******************************************************************************/
/*
  Purpose:

    TEST04 tests R4_EXP.

  Discussion:

    The arrays FE, KE and WE, once set up by R4_EXP_SETUP, are "read only" when
    accessed by R4_EXP.  So we only need to have one copy of these arrays,
    and they can be shared.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    19 October 2013

  Author:

    John Burkardt
*/
{
  float fe[256];
  uint32_t jsr;
  uint32_t jsr_value;
  uint32_t ke[256];
  double mega_rate_par;
  double mega_rate_seq;
  int r;
  int r_num = 1000;
  float r4_value;
  float *result_par;
  float *result_seq;
  int s;
  int s_num = 10000;
  uint32_t *seed;
  int thread;
  int thread_num;
  float we[256];
  double wtime_par;
  double wtime_seq;

  printf ( "\n" );
  printf ( "TEST04\n" );
  printf ( "  R4_EXP computes exponential random single precision real values.\n" );
  printf ( "  Since the output is completely determined\n" );
  printf ( "  by the input value of SEED and the tables, we can run in\n" );
  printf ( "  parallel as long as we make an array of seeds and share the tables.\n" );
/*
  Set up the SEED array and the tables, which will be used for both sequential
  and parallel computations.
*/
# pragma omp parallel
{
# pragma omp master 
  {
    thread_num = omp_get_num_threads ( );

    printf ( "\n" );
    printf ( "  The number of threads is %d\n", thread_num );
  }
}
  seed = ( uint32_t * ) malloc ( thread_num * sizeof ( uint32_t ) );
  result_seq = ( float * ) malloc ( thread_num * sizeof ( float ) );
  result_par = ( float * ) malloc ( thread_num * sizeof ( float ) );

  r4_exp_setup ( ke, fe, we );
/*
  Sequential execution.
  The sequential execution will only match the parallel execution if we can
  guarantee that the parallel threads are scheduled to execute the R loop
  consecutively.
*/
  jsr = 123456789;

  for ( thread = 0; thread < thread_num; thread++ )
  {
    seed[thread] = shr3_seeded ( &jsr );
  }

  wtime_seq = omp_get_wtime ( );

  for ( r = 0; r < r_num; r++ )
  {
    thread = ( r % thread_num );

    jsr = seed[thread];

    for ( s = 0; s < s_num; s++ )
    {
      r4_value = r4_exp ( &jsr, ke, fe, we );
    }
    result_seq[thread] = r4_value;
    seed[thread] = jsr;
  }

  wtime_seq = omp_get_wtime ( ) - wtime_seq;

  mega_rate_seq = ( double ) ( r_num ) * ( double ) ( s_num ) / wtime_seq 
    / 1000000.0;
/*
  Parallel.
*/
  jsr = 123456789;

  for ( thread = 0; thread < thread_num; thread++ )
  {
    seed[thread] = shr3_seeded ( &jsr );
  }

  wtime_par = omp_get_wtime ( );

# pragma omp parallel \
  shared ( result_par, seed ) \
  private ( jsr, r, r4_value, s, thread )
  {

# pragma omp for schedule ( static, 1 )

    for ( r = 0; r < r_num; r++ )
    {
      thread = omp_get_thread_num ( );
 
      jsr = seed[thread];

      for ( s = 0; s < s_num; s++ )
      {
        r4_value = r4_exp ( &jsr, ke, fe, we );
      }
      result_par[thread] = r4_value;
      seed[thread] = jsr;
    }
  }

  wtime_par = omp_get_wtime ( ) - wtime_par;

  mega_rate_par = ( double ) ( r_num ) * ( double ) ( s_num ) / wtime_par 
    / 1000000.0;
/*
  Report.
*/
  printf ( "\n" );
  printf ( "  Correctness check:\n" );
  printf ( "\n" );
  printf ( "  Computing values sequentially should reach the\n" );
  printf ( "  same result as doing it in parallel:\n" );
  printf ( "\n" );
  printf ( "    THREAD    Sequential        Parallel      Difference\n" );
  printf ( "\n" );

  for ( thread = 0; thread < thread_num; thread++ )
  {
    printf ( "  %8d  %14f  %14f  %14f\n", thread, result_seq[thread],
      result_par[thread], result_seq[thread] - result_par[thread] );
  }

  printf ( "\n" );
  printf ( "  Efficiency check:\n" );
  printf ( "\n" );
  printf ( "  Computing values in parallel should be faster:\n" );
  printf ( "\n" );
  printf ( "              Sequential      Parallel\n" );
  printf ( "\n" );
//  printf ( "      TIME:  %14f  %14f\n", wtime_seq, wtime_par  );
//  printf ( "      RATE:  %14f  %14f\n", mega_rate_seq, mega_rate_par );
/*
  Free memory.
*/
  free ( result_par );
  free ( result_seq );
  free ( seed );

  return;
}
/******************************************************************************/

float r4_exp ( uint32_t *jsr, uint32_t ke[256], float fe[256], float we[256] )

/******************************************************************************/
/*
  Purpose:

    R4_EXP returns an exponentially distributed single precision real value.

  Discussion:

    The underlying algorithm is the ziggurat method.

    Before the first call to this function, the user must call R4_EXP_SETUP
    to determine the values of KE, FE and WE.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    15 October 2013

  Author:

    John Burkardt

  Reference:

    George Marsaglia, Wai Wan Tsang,
    The Ziggurat Method for Generating Random Variables,
    Journal of Statistical Software,
    Volume 5, Number 8, October 2000, seven pages.

  Parameters:

    Input/output, uint32_t *JSR, the seed.

    Input, uint32_t KE[256], data computed by R4_EXP_SETUP.

    Input, float FE[256], WE[256], data computed by R4_EXP_SETUP.

    Output, float R4_EXP, an exponentially distributed random value.
*/
{
  uint32_t iz;
  uint32_t jz;
  float value;
  float x;

  jz = shr3_seeded ( jsr );
  iz = ( jz & 255 );

  if ( jz < ke[iz] )
  {
    value = ( float ) ( jz ) * we[iz];
  }
  else
  {
    for ( ; ; )
    {
      if ( iz == 0 )
      {
        value = 7.69711 - log ( r4_uni ( jsr ) );
        break;
      }

      x = ( float ) ( jz ) * we[iz];

      if ( fe[iz] + r4_uni ( jsr ) * ( fe[iz-1] - fe[iz] ) < exp ( - x ) )
      {
        value = x;
        break;
      }

      jz = shr3_seeded ( jsr );
      iz = ( jz & 255 );

      if ( jz < ke[iz] )
      {
        value = ( float ) ( jz ) * we[iz];
        break;
      }
    }
  }
  return value;
}
/******************************************************************************/

void r4_exp_setup ( uint32_t ke[256], float fe[256], float we[256] )

/******************************************************************************/
/*
  Purpose:

    R4_EXP_SETUP sets data needed by R4_EXP.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    14 October 2013

  Author:

    John Burkardt

  Reference:

    George Marsaglia, Wai Wan Tsang,
    The Ziggurat Method for Generating Random Variables,
    Journal of Statistical Software,
    Volume 5, Number 8, October 2000, seven pages.

  Parameters:

    Output, uint32_t KE[256], data needed by R4_EXP.

    Output, float FE[256], WE[256], data needed by R4_EXP.
*/
{
  double de = 7.697117470131487;
  int i;
  const double m2 = 2147483648.0;
  double q;
  double te = 7.697117470131487;
  const double ve = 3.949659822581572E-03;

  q = ve / exp ( - de );

  ke[0] = ( uint32_t ) ( ( de / q ) * m2 );
  ke[1] = 0;

  we[0] = ( float ) ( q / m2 );
  we[255] = ( float ) ( de / m2 );

  fe[0] = 1.0;
  fe[255] = ( float ) ( exp ( - de ) );

  for ( i = 254; 1 <= i; i-- )
  {
    de = - log ( ve / de + exp ( - de ) );
    ke[i+1] = ( uint32_t ) ( ( de / te ) * m2 );
    te = de;
    fe[i] = ( float ) ( exp ( - de ) );
    we[i] = ( float ) ( de / m2 );
  }
  return;
}
/******************************************************************************/

float r4_nor ( uint32_t *jsr, uint32_t kn[128], float fn[128], float wn[128] )

/******************************************************************************/
/*
  Purpose:

    R4_NOR returns a normally distributed single precision real value.

  Discussion:

    The value returned is generated from a distribution with mean 0 and 
    variance 1.

    The underlying algorithm is the ziggurat method.

    Before the first call to this function, the user must call R4_NOR_SETUP
    to determine the values of KN, FN and WN.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    14 October 2013

  Author:

    John Burkardt

  Reference:

    George Marsaglia, Wai Wan Tsang,
    The Ziggurat Method for Generating Random Variables,
    Journal of Statistical Software,
    Volume 5, Number 8, October 2000, seven pages.

  Parameters:

    Input/output, uint32_t *JSR, the seed.

    Input, uint32_t KN[128], data computed by R4_NOR_SETUP.

    Input, float FN[128], WN[128], data computed by R4_NOR_SETUP.

    Output, float R4_NOR, a normally distributed random value.
*/
{
  int hz;
  uint32_t iz;
  const float r = 3.442620;
  float value;
  float x;
  float y;

  hz = ( int ) shr3_seeded ( jsr );
  iz = ( hz & 127 );

  if ( fabs ( hz ) < kn[iz] )
  {
    value = ( float ) ( hz ) * wn[iz];
  }
  else
  {
    for ( ; ; )
    {
      if ( iz == 0 )
      {
        for ( ; ; )
        {
          x = - 0.2904764 * log ( r4_uni ( jsr ) );
          y = - log ( r4_uni ( jsr ) );
          if ( x * x <= y + y );
          {
            break;
          }
        }

        if ( hz <= 0 )
        {
          value = - r - x;
        }
        else
        {
          value = + r + x;
        }
        break;
      }

      x = ( float ) ( hz ) * wn[iz];

      if ( fn[iz] + r4_uni ( jsr ) * ( fn[iz-1] - fn[iz] ) 
        < exp ( - 0.5 * x * x ) )
      {
        value = x;
        break;
      }

      hz = ( int ) shr3_seeded ( jsr );
      iz = ( hz & 127 );

      if ( fabs ( hz ) < kn[iz] )
      {
        value = ( float ) ( hz ) * wn[iz];
        break;
      }
    }
  }

  return value;
}
/******************************************************************************/

void r4_nor_setup ( uint32_t kn[128], float fn[128], float wn[128] )

/******************************************************************************/
/*
  Purpose:

    R4_NOR_SETUP sets data needed by R4_NOR.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    14 October 2013

  Author:

    John Burkardt

  Reference:

    George Marsaglia, Wai Wan Tsang,
    The Ziggurat Method for Generating Random Variables,
    Journal of Statistical Software,
    Volume 5, Number 8, October 2000, seven pages.

  Parameters:

    Output, uint32_t KN[128], data needed by R4_NOR.

    Output, float FN[128], WN[128], data needed by R4_NOR.
*/
{
  double dn = 3.442619855899;
  int i;
  const double m1 = 2147483648.0;
  double q;
  double tn = 3.442619855899;
  const double vn = 9.91256303526217E-03;

  q = vn / exp ( - 0.5 * dn * dn );

  kn[0] = ( uint32_t ) ( ( dn / q ) * m1 );
  kn[1] = 0;

  wn[0] = ( float ) ( q / m1 );
  wn[127] = ( float ) ( dn / m1 );

  fn[0] = 1.0;
  fn[127] = ( float ) ( exp ( - 0.5 * dn * dn ) );

  for ( i = 126; 1 <= i; i-- )
  {
    dn = sqrt ( - 2.0 * log ( vn / dn + exp ( - 0.5 * dn * dn ) ) );
    kn[i+1] = ( uint32_t ) ( ( dn / tn ) * m1 );
    tn = dn;
    fn[i] = ( float ) ( exp ( - 0.5 * dn * dn ) );
    wn[i] = ( float ) ( dn / m1 );
  }

  return;
}
/******************************************************************************/

float r4_uni ( uint32_t *jsr )

/******************************************************************************/
/*
  Purpose:

    R4_UNI returns a uniformly distributed real value.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    04 October 2013

  Author:

    John Burkardt

  Reference:

    George Marsaglia, Wai Wan Tsang,
    The Ziggurat Method for Generating Random Variables,
    Journal of Statistical Software,
    Volume 5, Number 8, October 2000, seven pages.

  Parameters:

    Input/output, uint32_t *JSR, the seed.

    Output, float R4_UNI, a uniformly distributed random value in
    the range [0,1].
*/
{
  uint32_t jsr_input;
  float value;

  jsr_input = *jsr;

  *jsr = ( *jsr ^ ( *jsr <<   13 ) );
  *jsr = ( *jsr ^ ( *jsr >>   17 ) );
  *jsr = ( *jsr ^ ( *jsr <<    5 ) );

  value = fmod ( 0.5 + ( float ) ( jsr_input + *jsr ) / 65536.0 / 65536.0, 1.0 );

  return value;
}
/******************************************************************************/

uint32_t shr3_seeded ( uint32_t *jsr )

/******************************************************************************/
/*
  Purpose:

    SHR3_SEEDED evaluates the SHR3 generator for integers.

  Discussion:

    Thanks to Dirk Eddelbuettel for pointing out that this code needed to
    use the uint32_t data type in order to execute properly in 64 bit mode,
    03 October 2013.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    04 October 2013

  Author:

    John Burkardt

  Reference:

    George Marsaglia, Wai Wan Tsang,
    The Ziggurat Method for Generating Random Variables,
    Journal of Statistical Software,
    Volume 5, Number 8, October 2000, seven pages.

  Parameters:

    Input/output, uint32_t *JSR, the seed, which is updated 
    on each call.

    Output, uint32_t SHR3_SEEDED, the new value.
*/
{
  uint32_t value;

  value = *jsr;

  *jsr = ( *jsr ^ ( *jsr <<   13 ) );
  *jsr = ( *jsr ^ ( *jsr >>   17 ) );
  *jsr = ( *jsr ^ ( *jsr <<    5 ) );

  value = value + *jsr;

  return value;
}
/******************************************************************************/

void timestamp ( void )

/******************************************************************************/
/*
  Purpose:

    TIMESTAMP prints the current YMDHMS date as a time stamp.

  Example:

    31 May 2001 09:45:54 AM

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    24 September 2003

  Author:

    John Burkardt

  Parameters:

    None
*/
{
# define TIME_SIZE 40

  static char time_buffer[TIME_SIZE];
  const struct tm *tm;
  size_t len;
  time_t now;

  now = time ( NULL );
  tm = localtime ( &now );

  len = strftime ( time_buffer, TIME_SIZE, "%d %B %Y %I:%M:%S %p", tm );

  printf ( "%s\n", time_buffer );

  return;
# undef TIME_SIZE
}
