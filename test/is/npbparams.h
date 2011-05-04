#define CLASS 'W'
/*
   This file is generated automatically by the setparams utility.
   It sets the number of processors and the class of the NPB
   in this directory. Do not modify it by hand.   */
   
#define COMPILETIME "02 Dec 2010"
#define NPBVERSION "2.3"
#define CC "cc"
#define CFLAGS "-O3 "
#define CLINK "cc"
#define CLINKFLAGS "-lm"
#define C_LIB "-lm"
#define C_INC "-I../common"

extern void timer_clear(int);
extern void timer_start(int);
extern void timer_stop(int);
extern double timer_read(int);

extern void c_print_results(char *name, char class, int n1, int n2,
			    int n3, int niter, int nthreads, double t,
			    double mops, char *optype, int passed_verification,
			    char *npbversion, char *compiletime, char *cc,
			    char *clink, char *c_lib, char *c_inc,
			    char *cflags, char *clinkflags, char *rand);
