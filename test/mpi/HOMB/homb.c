/*
This is the source for HOMB, the Hybrid OpemMP MPI Benchmark, a Laplace Solver.
Data is distributed over MPI tasks and shared over OpenMP threads in a hybrid 
implementation.  When there is only one MPI task, the problem looks like a pure 
shared memory OpenMP implementation, and when there is only one OpenMP thread, 
the problem looks like a distributed memory MPI implimentation.  This is useful 
for testing Hybrid OpenMP/MPI performance on shared memory (NUMA) and multicore 
machines.
*/
/*
  Copyright 2009 Maxwell Lipford Hutchinson

  This file is part of HOMB.

  PGAF is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  PGAF is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with PGAF.  If not, see <http://www.gnu.org/licenses/>.
*/

/* Include Files */ 
#include <mpi.h>
#include <omp.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <float.h>

/* Book-keeping */
#define DOWN 100
#define UP 101
#define ROOT 0
#define MAX(x,y) (((x) > (y))? x : y)
#define MIN(x,y) (((x) > (y))? y : x)

/*********** HEADERS ***********/

/* Initialized the Global Context Variables (See Below) */
void initContext(int argc, char *argv[]);

/* Finds Task/Thread specific traits */
void setPEsParams(int *myPE);

/* Creats matrix for the Laplace solver's grid */
float*** createMatrix(void);

/* Applies boundry conditions and zeroes grid */
void initializeMatrix(float ***t, int myPE, int *new);

/* Prints the Global Context for diagnostics */
void printContext(void);

/* Does extra, useless floating point operations to manipulate comp/com ratio */
void extraCalculations(float ***t);

/* Does all the actual solving.  (Includes parallel region) */
float work(float ***t, int *new, int myPE, 
           MPI_Request request[], MPI_Status status[]);

/* Sums along the diagonal of the grid as a comparative integrity check */
void sumTrace(float ***t, int new, int myPE, float *sum);

/* Updates the ROOT's copy of the times matrix */
void timeUpdate(double **times, int myPE);

/* Runs statistical tests on the ROOT's copy of the times matrix */
void statistics(double **times, double **covar, double *minTime,
                double *meanTime, double *maxTime, double *stdvTime,
                double *NstdvTime);

/* File Output */
void fileIO(double **times, double **covar, double minTime, double meanTime, 
            double maxTIme, double NstdvTime);

/* Standard Ouput */
void stdoutIO(double **times, double ** covar, double minTime, 
              double meanTime, double maxTime, double NstdvTime);

/* Inverse of the erf function */
float ierf(int res);

/* Prints either the new or old plane of the grid matrix for diagnostics */
void printMatrix(float ***t, int plane);

/* Print CPU Location information */
void cpuLocationOutput(int myPE);

/* Get CPU_ID information */
static long getCpuId();

/* Get CPU Address information */
static int getCpuAddr(char* outBuf, int bufLen, long cpuId);

 
/* Global Context :: 
(All variables in Global Context are set once and then constant) */

/* Number of Rows, Columns, Iterations (samples), extra FLOP */
int NR, NC, NITER, EXTRA; 
/* Input/Ouput logicals */
int vOut, sOut, fOut, tOut, cOut, pHeader, pContext; 
/* Output file names */
char *fileOutName, *timesOutName, *cpuOutName; 
/* Number of Tasks, Threads, rows in local section of distributed grid */
int nPEs, nThreads, nrl; 
/* Logicals for which MPI calls are made at the end of each iteration */
int barrier, reduce; 

/*********** MAIN ***********/
int main(int argc, char *argv[]) {
  /* State information */
  int myPE, iter, new, old;

  /* Application measurements */
  float dt, dtg, sum, sumg;

  /* Timing measurements */
  double startTime, endTime;
  double meanTime = 0., minTime = FLT_MAX, maxTime = 0.;
  double stdvTime = 0., NstdvTime = 0.;

  /* Iterators */
  int iPE, jPE;

  /* Declaration for status of MPI_Recv */
  MPI_Status status[4];
  MPI_Request request[4];

  /* Initialize MPI */
  MPI_Init(&argc, &argv);

  /* Initialize Global Context */ 
  initContext(argc, argv);

  /* Get task/thread information */
  setPEsParams(&myPE);

  /* Create matrix for times */
  double **times = malloc(nPEs * sizeof(double *)); 
  for (iPE = 0; iPE < nPEs; iPE++)
    times[iPE] = malloc(NITER * sizeof(double));

  /* Create matrix for co-variations in times */
  double **covar = malloc(nPEs * sizeof(double *));
  for (iPE = 0; iPE < nPEs; iPE++)
    covar[iPE] = malloc(nPEs * sizeof(double));
 
  /* Create grids */
  float ***t;
  t = createMatrix();

  /* Initialize grids */
  initializeMatrix(t, myPE, &new);

  /* Print Global Context to standard out */
  if (myPE == ROOT && pContext){   
    printContext();
  }

  /* Solve */
  for (iter = 1; iter <= NITER; iter++) {

    /* Begin Timing */
    startTime = MPI_Wtime();

    if (barrier)
      /* Sync everything up because we want to */
      MPI_Barrier(MPI_COMM_WORLD);
      
    /* Do extra floating point ops (if wanted) */    
    if (EXTRA != 0 ){
      extraCalculations(t);
    }

    /* Do all the real work */
    dt = work(t, &new, myPE, request, status);

    if (reduce)
      /* Find max dt over the whole domain */
      MPI_Reduce(&dt, &dtg, 1, MPI_FLOAT, MPI_MAX, ROOT, MPI_COMM_WORLD);

    /* End timing */
    endTime = MPI_Wtime();

    /* Store Time */
    times[myPE][iter-1] = endTime-startTime;
  }

  /* Sum the diagonal values across PEs (error checker) */
  sumTrace(t, new, myPE, &sum);
  MPI_Reduce(&sum, &sumg, 1, MPI_FLOAT, MPI_SUM, ROOT, MPI_COMM_WORLD);

  /* Sync everything up before the anaysis stage of the program */
  MPI_Barrier(MPI_COMM_WORLD);

  /* Gather iteration runtimes to ROOT's matrix */
  timeUpdate(times, myPE);

  /* Run statistics on times (Root only) */
  if (myPE == ROOT) {
    statistics(times, covar, &minTime, &meanTime,
               &maxTime, &stdvTime, &NstdvTime);
  }

  /* Output */
  if (myPE == ROOT) {
    /* Check for file output */
    if ( (fOut + tOut) > 0 )
      fileIO(times, covar, minTime, meanTime, maxTime, NstdvTime);
    /* Check for Standard Output */
    if ( (sOut + vOut) > 0 )
      stdoutIO(times, covar, minTime, meanTime, maxTime, NstdvTime);
  }

  if (cOut)
    cpuLocationOutput(myPE);

  /* Finalize */
  MPI_Finalize();
}


/*********** FUNCTIONS ***********/

void initContext(int argc, char *argv[]){
  /* Allocate space for file names */
  fileOutName = malloc(100 * sizeof(char));
  timesOutName = malloc(100 * sizeof(char));
  cpuOutName = malloc(100 * sizeof(char));
  /* "Logicals" for file output */
  vOut = 0; sOut = 0; fOut = 0; tOut = 0; cOut = 0; pHeader = 1; pContext = 0;
  /* Logicals for which MPI calls are made at the end of each loop */
  barrier = 0; reduce = 1;
  /* Default required values */
  NR = 8192; NC = 8192; NITER = 20; EXTRA = 0;  nThreads = 1; nPEs = 1;
  /* indexs */
  int i;
  /* Cycle through command line args */
  for (i = 1; i < argc; i++){  
    /* Look for number of rows */
    if ( strcmp("-NR", argv[i]) == 0 ){
      sscanf(argv[i+1],"%d",&NR);
    }
    /* Look for number of columns */  
    if ( strcmp("-NC", argv[i]) == 0 ){
      sscanf(argv[i+1],"%d",&NC);
    }
    /* Look for number of rows and columns */  
    if ( strcmp("-NRC", argv[i]) == 0 ){
      sscanf(argv[i+1],"%d",&NR);
      sscanf(argv[i+1],"%d",&NC);
    }
    /* Look for number of iterations */
    if ( strcmp("-NITER", argv[i]) == 0 ){
      sscanf(argv[i+1],"%d",&NITER);
    }
    /* Look for number of extra flop */
    if ( strcmp("-EXTRA", argv[i]) == 0 ){
      sscanf(argv[i+1],"%d",&EXTRA);
    }
    /* Look "Barrier" option */
    if ( strcmp("-barrier", argv[i]) == 0 ){
      barrier = 1;
    }
    /* Look for "No Barrier" option */
    if ( strcmp("-no_barrier", argv[i]) == 0 ){
      barrier = 0;
    }
    /* Look for "Reduce" option */
    if ( strcmp("-reduce", argv[i]) == 0 ){
      reduce = 1;
    }
    /* Look for "No Reduce" option */
    if ( strcmp("-no_reduce", argv[i]) == 0 ){
      reduce = 0;
    }
    /* Look for "verbose" output filename */
    if ( strcmp("-vf",argv[i]) == 0 ){
      sscanf(argv[i+1],"%s",fileOutName);
      fOut = 1;
    }
    /* Look for time table file output */
    if ( strcmp("-tf",argv[i]) == 0 ){
      sscanf(argv[i+1],"%s",timesOutName);
      tOut = 1;
    }
    /* Look for CPU location file output */
    if ( strcmp("-cpuf",argv[i]) == 0 ){
      sscanf(argv[i+1],"%s",cpuOutName);
      cOut = 1;
    }
    /* Look for "summary" standard out */
    if ( strcmp("-s",argv[i]) == 0 ){
      sOut = 1;
    }
    /* Look for "verbose" standard out */
    if ( strcmp("-v",argv[i]) == 0 ){
      vOut = 1;
    }
    /* Look for "No Header" option */
    if ( strcmp("-nh",argv[i]) == 0 ){
      pHeader = 0;
    }
    /* Look for "verbose" standard out */
    if ( strcmp("--help",argv[i]) == 0 ){
      printf("Usage: %s {-NR num-rows -NC num-cols} | {-NRC num-rows/cols}\n \
             -NITER num-iter [-EXTRA extra-flop] \n \
             [-[no_]barrier] [-[no_]reduce] \n \
             [-v | -s] [-vf verbose-out-filename] \n \
             [-cpuf cpu_loc-out-filename] \n \
             [-tf time-out-filename] \n \
             [--help] \n", argv[0]);
      exit(EXIT_SUCCESS);
    }
    /* Look for "Print Context" option */
    if ( strcmp("-pc",argv[i]) == 0 ){
      pContext = 1;
    }
  }
  /* Check if NITER is large enough */
  if (NITER < 6){
    fprintf(stderr,"NITER is too small, must be > 5 \n");
    exit(EXIT_FAILURE);
  }
}

void setPEsParams(int *myPE) {
  /* Find nPEs */
  MPI_Comm_size(MPI_COMM_WORLD, &nPEs);
  /* Find myPE */
  MPI_Comm_rank(MPI_COMM_WORLD, myPE); 

  /* Re-organized PEs (if you want) */
  /*
  if ( myPE % 2 == 0)
    *myPE=nPEs-1-*myPE
  */

  /* Find number of threads per task */
  #pragma omp parallel shared(nThreads)
  {
    #pragma omp single
      nThreads = omp_get_num_threads();
  }
  /* Check to make sure matrix breaks into nPEs evenly sized peices */
  if ( NR %(nPEs)!=0) {
    MPI_Finalize();
    if (*myPE == 0) {
      fprintf(stderr, "The example is only for factors of %d \n", NR);
    }
    exit(EXIT_FAILURE);
  }
  /* Set distributed grid size */
  nrl = NR/nPEs;
}

float*** createMatrix() {
  /* Returned variable */
  float ***aux;
  /* Iterators */
  int plane, row;

  /* Allocate 2 pointers, one for new and one for old values */
  aux = malloc(2 * sizeof(float *));

  /* Allocate nrl+2 pointers, one for each row of each grid */
  aux[0] = malloc((nrl+2) * sizeof(float *));
  aux[1] = malloc((nrl+2) * sizeof(float *));

  /* Check */
  if (aux == NULL) {
    printf("\nFailure to allocate memory for row pointers.\n");
    exit(EXIT_FAILURE);
  }

  /* Allocate the rows */
  for (plane = 0; plane < 2; plane++)
    for (row = 0; row < (nrl+2); row++) {
      aux[plane][row] = malloc((NC+2) * sizeof(float));
  
      /* Check */
      if (aux[plane][row] == NULL) {
        printf("\nFailure to allocate memory for row %d\n",row);
        exit(EXIT_FAILURE);
      }
    }

  return aux;
}

void initializeMatrix(float ***t, int myPE, int *new) {
  /* Local boundry conditions */
  float tMin, tMax;
  /* Iterators */
  int i, j;

  /* pick which of two grids is "new" */
  *new = 1;
  /* Initialize everything to zero */
  for (i = 0; i <= nrl+1; i++)
    for (j = 0; j <= NC+1; j++)
      t[1][i][j] = 0.0;
  /* Lower BC bound */
  tMin = (myPE)*100.0/nPEs;
  /* Upper BC bound */
  tMax = (myPE+1)*100.0/nPEs;
  /* Left and Right boundaries */
  for (i = 0; i <= nrl+1; i++) {
    t[1][i][0] = 0.0;
    t[1][i][NC+1] = tMin + ((tMax-tMin)/nrl)*i;
  }
  /* Top boundary */
  if (myPE == 0)
    for (j = 0; j <= NC+1; j++)
      t[1][0][j] = 0.0;
  /* Bottom boundary */
  if (myPE == nPEs-1)
    for (j = 0; j <= NC+1; j++)
      t[1][nrl+1][j] = (100.0/NC) * j;
  /* Copy to other grid */
  for (i = 0; i <= nrl+1; i++)
    for (j = 0; j <= NC+1; j++)
      t[0][i][j]=t[1][i][j];
}

void printContext(void){
  printf("Number of Rows: %d, Number of Columns: %d, \
          Number of Iterations: %d \n", NR, NC, NITER);
  printf("Number of Tasks: %d, Number of Threads per Task: %d \n",
         nPEs, nThreads);
  if (EXTRA != 0)
    printf("Number of Extra Floating Point Operations per Iteration: %d \n",
           EXTRA);
  if (barrier)
    printf("Barrier at start of each iteration.\n");
  if (reduce)
    printf("Reduction at end of each iteration.\n");
  if (vOut)
    printf("Verbose Standard Output \n");
  else if (sOut)
    if (pHeader)
      printf("Summary Standard Ouput with Header \n");
    else
      printf("Summary Standard Output without Header \n");
  if (fOut)
    printf("Verbose File Output to: %s \n", fileOutName);
  if (tOut)
    printf("Times Matrix File Output to: %s \n", timesOutName);
  if (cOut)
    printf("CPU Locations File Output to: %s \n", cpuOutName);
}

void extraCalculations(float ***t){
  /* Iterators */
  int x, y, z;

  /* Loop over grid doing nothing (for extra FLOP) */
  #pragma omp parallel 
  {
  for (x = 1; x <= nrl; x++)
    for (y = 1; y <= NC; y++)
      for (z = 0; z <= EXTRA; z++) {
        t[1][x][y] += z;  // Do
        t[1][x][y] -= z;  // Undo
      }
  }
}

float work(float ***t, int *new, int myPE, 
           MPI_Request request[], MPI_Status status[]){
  /* Application measurments */
  float d = 0., dt = 0.;
  /* Indicies */
  int old = 1 - *new;
  /* Iterators */
  int i, j, k;
  		
  /* Begin parallel region if there are more than 1 thread per task */
  #pragma omp parallel if (nThreads != 1) shared(t, new, old, nrl, dt, NR, NC, NITER) private(d)
  {
    /* Use master thread to calculate and communicate boundies */
    #pragma omp master
    {
      /* Loop over top and bottom boundry */
      for (k = 1; k <= NC; k++){
        /*Calculate average of neighbors as new value (Point Jacobi method) */
        t[*new][1][k] = 0.25 *
                        (t[old][2][k] + t[old][0][k] +
                         t[old][1][k+1] + t[old][1][k-1]);
        t[*new][nrl][k] = 0.25 * 
                          (t[old][nrl+1][k] + t[old][nrl-1][k] +
                           t[old][nrl][k+1] + t[old][nrl][k-1]);
        /* Calculate local maximum change from last step */
        /* Puts thread's max in d */
        d = MAX(fabs(t[*new][1][k] - t[old][1][k]), d);  
        d = MAX(fabs(t[*new][nrl][k] - t[old][nrl][k]), d);
      }
      if (nPEs!=1){
        /* Exchange boundries with neighbor tasks */
        if (myPE < nPEs-1 )
          /* Sending Down; Only npes-1 do this */
          MPI_Isend(&(t[*new][nrl][1]), NC, MPI_FLOAT, 
                    myPE+1, DOWN, MPI_COMM_WORLD, &request[0]);
        if (myPE != 0)
         /* Sending Up; Only npes-1 do this */
         MPI_Isend(&t[*new][1][1], NC, MPI_FLOAT,
                   myPE-1, UP, MPI_COMM_WORLD, &request[1]);
        if (myPE != 0)
          /* Receive from UP */
          MPI_Irecv(&t[*new][0][1], NC, MPI_FLOAT, 
                    MPI_ANY_SOURCE, DOWN, MPI_COMM_WORLD, &request[2]);
        if (myPE != nPEs-1)
          /* Receive from DOWN */
          MPI_Irecv(&t[*new][nrl+1][1], NC, MPI_FLOAT, 
                    MPI_ANY_SOURCE, UP, MPI_COMM_WORLD, &request[3]);
      }
    }

    /* Everyone calculates values and finds local max change */
    #pragma omp for schedule(runtime) nowait
      for (i = 2; i <= nrl-1; i++)
        for (j = 1; j <= NC; j++){
          t[*new][i][j] = 0.25 *
                          (t[old][i+1][j] + t[old][i-1][j] +
                           t[old][i][j+1] + t[old][i][j-1]);
          d = MAX(fabs(t[*new][i][j] - t[old][i][j]), d);
        }

    /*Local max change become taks-global max change */    
    #pragma omp critical
      dt = MAX(d, dt); /* Finds max of the d's */
  }

  /* If there are multiple tasks, wait for the rest, find global max change */
  if (nPEs!=1){
    if (myPE != nPEs-1 )
      MPI_Wait(&request[0], &status[0]); 
    if (myPE != nPEs-1)
      MPI_Wait(&request[3], &status[3]);
    if (myPE != 0)
      MPI_Wait(&request[1], &status[1]);
    if (myPE != 0)
      MPI_Wait(&request[2], &status[2]);
  }

  /* "Flip" pointers */
  *new = old;
  dt=dt+1.0-1.0; //Keep this line! (prevents harmful optimization)

  return dt;
}

void sumTrace(float ***t, int new, int myPE, float *sum) {
  int jOff, i;
  jOff = myPE*nrl;
  *sum = 0.;
  int old = 1 - new;
  
  /* Sum over diagonal with knowledge of distributed grids */
  for (i = 1; i <= nrl; i++){
    *sum += t[old][i][jOff+i];
  }
  return;
}

void timeUpdate(double **times, int myPE){
  int iPE;
  /* Update Root's times matrix to include all times */
  if (myPE != ROOT){
    MPI_Request req;
    MPI_Status sta;

    /* Sending to ROOT; Only npes-1 do this */
    MPI_Isend(&times[myPE][0], NITER, MPI_DOUBLE,
              ROOT, myPE, MPI_COMM_WORLD, &req);
    /* Waiting for Root */
    MPI_Wait(&req, &sta);
  }

  if (myPE == ROOT){
    MPI_Request rootRequest[nPEs];
    MPI_Status rootStatus[nPEs];    

    /* Recieving times from other tasks */ 
    for (iPE = 1; iPE < nPEs; iPE++){
      MPI_Irecv(&times[iPE][0], NITER, MPI_DOUBLE,
                MPI_ANY_SOURCE, iPE, MPI_COMM_WORLD, &rootRequest[iPE]);
      MPI_Wait(&rootRequest[iPE], &rootStatus[iPE]);
    }
  }
}

void statistics(double **times, double **covar, 
                double *minTime, double *meanTime, double *maxTime,
                double *stdvTime, double *NstdvTime){
  double temp;
  int iPE, iter;
  /* Compute mean, max, min of times */
  for (iPE = 0; iPE < nPEs; iPE++)
    for (iter=5; iter<NITER; iter++){
      *meanTime += times[iPE][iter];
      *maxTime = MAX(*maxTime, times[iPE][iter]);
      *minTime = MIN(*minTime, times[iPE][iter]);
    }
  *meanTime = *meanTime / (NITER - 5) / nPEs;

  /* Compute standard deviation of times */
  for (iPE = 0; iPE < nPEs; iPE++)
    for (iter = 5; iter < NITER; iter++){
      *stdvTime += (times[iPE][iter] - *meanTime) *
                   (times[iPE][iter] - *meanTime);
    }
  *stdvTime = sqrt(*stdvTime / (NITER - 5) / nPEs);

  /* Normalized standard deviation (stdv / mean) */
  *NstdvTime = *stdvTime / *meanTime;
}

void fileIO(double **times, double **covar,
            double minTime, double meanTime, double maxTime, 
            double NstdvTime){
  FILE *outFile, *timesOutFile;
  int iter, iPE, jPE;

  if (fOut){
    outFile = fopen(fileOutName,"w");
    /* Print heading and summary data */
    fprintf(outFile,"#==========================================================================================================#\n");
    fprintf(outFile,"#\tTasks\tThreads\tNR\tNC\tNITER\tmeanTime \tmaxTime  \tminTime  \tNstdvTime  #\n");
    fprintf(outFile,"#==========================================================================================================#\n");
    fprintf(outFile,"\t%d\t%d\t%d\t%d\t%d\t%f\t%f\t%f\t%f\n",
            nPEs,nThreads,NR,NC,NITER, meanTime, maxTime, minTime, NstdvTime);
    fprintf(outFile,"\n"); fprintf(outFile,"\n");

    /* Print full times matrix */
    fprintf(outFile,"# Full Time Output (rows are times, cols are tasks)\n");
    for (iter = 0; iter < NITER; iter++) {
      for (iPE = 0; iPE < nPEs; iPE++)
        fprintf(outFile,"%e \t",times[iPE][iter]);
        fprintf(outFile,"\n");
      }

    /* Close file */
    fclose(outFile);
  }

  if (tOut){
    timesOutFile = fopen(timesOutName,"w");
    /* Print full times matrix */
    fprintf(timesOutFile,
            "# Full Time Output (rows are times, cols are tasks)\n");
    for (iter = 0; iter < NITER; iter++) {
      for (iPE = 0; iPE < nPEs; iPE++)
        fprintf(timesOutFile,"%e \t",times[iPE][iter]);
        fprintf(timesOutFile,"\n");
      }
    /* Close file */
    fclose(timesOutFile);
  }
}

void stdoutIO(double **times, double **covar, 
              double minTime, double meanTime, double maxTime, 
              double NstdvTime){
  int iter, iPE, jPE;

  /* Same data as above, just to Standard Output */
  if (pHeader){
    printf("#==========================================================================================================#\n");
    printf("#\tTasks\tThreads\tNR\tNC\tNITER\tmeanTime \tmaxTime  \tminTime  \tNstdvTime  #\n");
    printf("#==========================================================================================================#\n");
  }
  printf("\t%d\t%d\t%d\t%d\t%d\t%f\t%f\t%f\t%f\n",
         nPEs,nThreads,NR,NC,NITER, meanTime, maxTime, minTime, NstdvTime);

  /* Only if "Verbose Output" asked for */
  if (vOut == 1){ 
    printf("\n"); printf("\n");
    printf("# Full Time Output (rows are times, cols are tasks)\n");
    for (iter = 0; iter < NITER; iter++){ 
      for (iPE = 0; iPE < nPEs; iPE++)
        printf("%e \t",times[iPE][iter]);
      printf("\n");
      }
  }
}


/* Utilities to use if you want them */

/* Inverse of the erf function to get normaly destributed random numbers */
float ierf(int res){
  float LB, UB, MP, MPval, randomNum;
  int i;
  srand48( (unsigned int) time(NULL) );
  randomNum = (float) (drand48() * 2. - 1.);

  if(randomNum < 0.0)
    LB = -1.0;  UB = 0.0; MP = -.5;
  if (randomNum > 0.0)
    LB = 0.0; UB = 1.0; MP = .5;

  for (i=0; i<res; i++){
    MPval = erf(MP);
    if (randomNum < MPval){
      UB = MP;
      MP = (LB + UB) / 2.0;
    }else{
      LB = MP;
      MP = (LB + UB) / 2.0;
    }
  }
  return MP;
}

/* Will print whole matrix if you'd like.  
 ***t is the pointer the the matrix, 
 plane is either 0 or 1, 
 and nrl is the local grid size
*/
void printMatrix(float ***t, int plane){
  int i, j;
  printf("T Matrix: \n");
  for (i=0; i<nrl+2; i++)
  {    
    for (j=0; j<nrl+2; j++)
      printf("%10.5f  ", t[plane][i][j]); 
    printf("\n");
  }
  printf("\n");
}

/* Output's CPU physical location information */
void cpuLocationOutput(int myPE){
  long cpuId;
  char locStr[64];
  int i, j;
  FILE* cpuOutFile;

  char *outPiece[nThreads];
  for (i = 0; i < nThreads; i++)
    outPiece[i] = malloc(150 * sizeof(char));

  #pragma omp parallel private(cpuId, locStr) shared(outPiece)
  {
    int myTH = omp_get_thread_num();
    cpuId= getCpuId();
    if (!getCpuAddr(locStr,sizeof(locStr),cpuId))
      strncpy(locStr,"LocNotFound",sizeof(locStr));
    sprintf(outPiece[myTH],
           "  %d:%d\t\t%d:%d\t\t%ld\t%s   \n",
           myPE,myTH,nPEs,omp_get_num_threads(),
           cpuId,locStr);
  }

  if (myPE == ROOT){
    cpuOutFile = fopen(cpuOutName, "w");
    fprintf(cpuOutFile, "# Proc ID\tProc Env\tCPU ID\tCPU Loc\t\t#\n");
    fprintf(cpuOutFile, "#=======================================================#\n");
    fclose(cpuOutFile);
  }
  MPI_Barrier(MPI_COMM_WORLD);

  for (i = 0; i < nPEs; i++){
    if (myPE == i){
      cpuOutFile = fopen(cpuOutName, "a");
      for (j = 0; j < nThreads; j++)
        fprintf(cpuOutFile,outPiece[j]);
      fclose(cpuOutFile);
    }
    MPI_Barrier(MPI_COMM_WORLD);
  } 

}

/* These two give CPU location information on SGI machines*/
static long getCpuId(){
  FILE* f = NULL; char buf[64], buf2[512], *here;
  int nBlanks = 0;  long cpuId;
  pid_t pid = getpid();

  snprintf(buf,sizeof(buf),"/proc/%ld/stat",(long)pid);
  if (!(f = fopen(buf,"r"))) {
    perror("Unable to open proc status"); exit(-1);
  }
  if (!fgets(buf2,sizeof(buf2),f)) {
    perror("Read of proc status failed"); exit(-1);
  }
  fclose(f);
  here = buf2;
  while (nBlanks<38) { if (*here++==' ') nBlanks++;  }
  cpuId = atol(here);
  return cpuId;
}

static int getCpuAddr(char* outBuf, int bufLen, long cpuId){
  char buf[2048], matchBuf[64], *here, *there;
  FILE* f= fopen("/proc/sgi_sn/sn_topology","r");
  int match;
  if (!f) {
    perror("Unable to open topology file"); exit(-1);
  }
  snprintf(matchBuf,sizeof(matchBuf),"cpu %ld ",cpuId);
  match= 0;
  while (!feof(f) && !ferror(f)) {
    if (!fgets(buf,sizeof(buf),f)) {
      perror("Read from topology file failed"); exit(-1);
    }
    here= strstr(buf,matchBuf);
    if (here && here==buf) {
      match= 1; break;
    }
  }
  if (match) {
    here= buf+strlen(matchBuf); there= outBuf;
    while (*here != ' ' && (there-outBuf < bufLen-1)) *there++= *here++;
    outBuf[bufLen-1]= '\0';
    return 1;
  }
  else
    return 0;
}
