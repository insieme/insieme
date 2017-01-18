/*
    This work was performed under the auspices of the U.S. Department of
    Energy by Lawrence Livermore National Laboratory under Contract
    DE-AC52-07NA27344.

    DISCLAIMER

    This work was prepared as an account of work sponsored by an agency of
    the United States government. Neither the United States government nor
    Lawrence Livermore National Security, LLC, nor any of their employees
    makes any warranty, expressed or implied, or assumes any legal
    liability or responsibility for the accuracy, completeness, or
    usefulness of any information, apparatus, product, or process
    disclosed, or represents that its use would not infringe privately
    owned rights. Reference herein to any specific commercial product,
    process, or service by trade name, trademark, manufacturer, or
    otherwise does not necessarily constitute or imply its endorsement,
    recommendation, or favoring by the United States government or Lawrence
    Livermore National Security, LLC. The views and opinions of authors
    expressed herein do not necessarily state or reflect those of the
    United States government or Lawrence Livermore National Security, LLC,
    and shall not be used for advertising or product endorsement purposes.

    NOTIFICATION OF COMMERCIAL USE

    Commercialization of this product is prohibited without notifying the
    Department of Energy (DOE) or Lawrence Livermore National Laboratory
    (LLNL).

    UCRL-CODE-2001-028

    com.h

*/



typedef double ( *TESTFUNC ) ( int, int, MPI_Comm * );

enum TESTTYPE
{
  UNIDIR, UNIDIRNB, BIDIR, BIDIRNB, LATEN, TYPETOT
};

typedef struct
{
  int id;
  char *name;
  double rankResult;
  double maxResult;
  int iters;
  double sendFactor;
  int *messList;
  int messListSize;
  double maxBW;
  int maxBWMessSize;
  TESTFUNC testFunc;

}
TESTPARAMS;

TESTPARAMS *testParams;


void runTest ( TESTPARAMS * testParams );
int generateResults ( TESTPARAMS * testParams, int procs, int messSize,
                      int iters, double *result );
double runUnicomTest ( int bufsize, int iters, MPI_Comm * activeComm );
double runNonblockUnicomTest ( int bufsize, int iters,
                               MPI_Comm * activeComm );
double runNonblockBicomTest ( int bufsize, int iters, MPI_Comm * activeComm );
double runBicomTest ( int bufsize, int iters, MPI_Comm * activeComm );
double runLatencyTest ( int bufsize, int iters, MPI_Comm * activeComm );
void printUse ( void );
void printParameters ( void );
void printReportHeader ( void );
int setupTestListParams ( void );
int initAllTestTypeParams ( TESTPARAMS ** testParams );
void freeBuffers ( TESTPARAMS ** testParams );
int getNextTargetFile ( void );
void printTestNames ( void );
extern int getTargetList ( void );
extern void printSeparator ( void );
extern void printEnv ( void );
