// --- Generated Inspire Code ---
#include <stddef.h>
#define bool int
#define true 1
#define false 0
// --- Entry Point ---

// start code fragment :: type_declaration___insieme_globals //
struct __insieme_globals { 
    int sec;
    double[64] start;
    double[64] elapsed;
    int* key_buff_ptr_global;
    int passed_verification;
    int[65536] key_array;
    int[65536] key_buff1;
    int[65536] key_buff2;
    int[5] partial_verify_vals;
    int[5] test_index_array;
    int[5] test_rank_array;
    int(* S_test_index_array)[5];
    int(* S_test_rank_array)[5];
    int(* W_test_index_array)[5];
    int(* W_test_rank_array)[5];
    int(* A_test_index_array)[5];
    int(* A_test_rank_array)[5];
    int(* B_test_index_array)[5];
    int(* B_test_rank_array)[5];
    int(* C_test_index_array)[5];
    int(* C_test_rank_array)[5];
    int KS;
    double R23;
    double R46;
    double T23;
    double T46;
};

// start code fragment :: Prototype for external function: printf //
int printf(char*, ...);

// start code fragment :: Definitions for function type: __insieme_funType_type_0 //
// Abstract prototype for lambdas of type __insieme_funType_type_0
struct __insieme_funType_type_0 { 
    void(*fun)(void*,int);
    const size_t size;
};

// Type safe function for invoking lambdas of type __insieme_funType_type_0
void call___insieme_funType_type_0(struct __insieme_funType_type_0* lambda,int p1) { return lambda->fun(lambda,p1); }

// start code fragment :: Definitions for function type: __insieme_funType_type_1 //
// Abstract prototype for lambdas of type __insieme_funType_type_1
struct __insieme_funType_type_1 { 
    void(*fun)(void*,int);
    const size_t size;
    struct __insieme_globals* p0;
};

// start code fragment :: Definition of timer_clear //
void timer_clear(void* _capture, int n) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_13 = ((struct __insieme_funType_type_1*)_capture)->p0;
	// --------- Captured Stuff -  End  -------------
	{
		((*((*((*var_13).elapsed))[((unsigned int)(n))])) = 0.0);
	}
}


// start code fragment :: Definitions for function type: __insieme_funType_type_2 //
// Abstract prototype for lambdas of type __insieme_funType_type_2
struct __insieme_funType_type_2 { 
    void(*fun)(void*,double,double);
    const size_t size;
};

// Type safe function for invoking lambdas of type __insieme_funType_type_2
void call___insieme_funType_type_2(struct __insieme_funType_type_2* lambda,double p1,double p2) { return lambda->fun(lambda,p1,p2); }

// start code fragment :: Definitions for function type: __insieme_funType_type_3 //
// Abstract prototype for lambdas of type __insieme_funType_type_3
struct __insieme_funType_type_3 { 
    void(*fun)(void*,double,double);
    const size_t size;
    struct __insieme_globals* p0;
};

// start code fragment :: Definitions for function type: __insieme_funType_type_4 //
// Abstract prototype for lambdas of type __insieme_funType_type_4
struct __insieme_funType_type_4 { 
    double(*fun)(void*,double*,double*);
    const size_t size;
};

// Type safe function for invoking lambdas of type __insieme_funType_type_4
double call___insieme_funType_type_4(struct __insieme_funType_type_4* lambda,double* p1,double* p2) { return lambda->fun(lambda,p1,p2); }

// start code fragment :: Definitions for function type: __insieme_funType_type_5 //
// Abstract prototype for lambdas of type __insieme_funType_type_5
struct __insieme_funType_type_5 { 
    double(*fun)(void*,double*,double*);
    const size_t size;
    struct __insieme_globals* p0;
};

// start code fragment :: Definition of randlc //
double randlc(void* _capture, double* X, double* A) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_26 = ((struct __insieme_funType_type_5*)_capture)->p0;
	// --------- Captured Stuff -  End  -------------
	{
		double T1 = 0.0;
		double T2 = 0.0;
		double T3 = 0.0;
		double T4 = 0.0;
		double A1 = 0.0;
		double A2 = 0.0;
		double X1 = 0.0;
		double X2 = 0.0;
		double Z = 0.0;
		int i = 0;
		int j = 0;
		if((((*var_26).KS)==0)) {
			(((*var_26).R23) = 1.0);
			(((*var_26).R46) = 1.0);
			(((*var_26).T23) = 1.0);
			(((*var_26).T46) = 1.0);
			{
				for(int var_38 = 1; var_38 < 23; var_38 += 1) {
					(((*var_26).R23) = (0.50*((*var_26).R23)));
					(((*var_26).T23) = (2.0*((*var_26).T23)));
				};
				(i = 23);
			};
			{
				for(int var_39 = 1; var_39 < 46; var_39 += 1) {
					(((*var_26).R46) = (0.50*((*var_26).R46)));
					(((*var_26).T46) = (2.0*((*var_26).T46)));
				};
				(i = 46);
			};
			(((*var_26).KS) = 1);
		} else {};
		(T1 = (((*var_26).R23)*(A[0])));
		(j = ((int)(T1)));
		(A1 = ((double)(j)));
		(A2 = ((A[0])-(((*var_26).T23)*A1)));
		(T1 = (((*var_26).R23)*(X[0])));
		(j = ((int)(T1)));
		(X1 = ((double)(j)));
		(X2 = ((X[0])-(((*var_26).T23)*X1)));
		(T1 = ((A1*X2)+(A2*X1)));
		(j = ((int)((((*var_26).R23)*T1))));
		(T2 = ((double)(j)));
		(Z = (T1-(((*var_26).T23)*T2)));
		(T3 = ((((*var_26).T23)*Z)+(A2*X2)));
		(j = ((int)((((*var_26).R46)*T3))));
		(T4 = ((double)(j)));
		((X[0]) = (T3-(((*var_26).T46)*T4)));
		return (((*var_26).R46)*(X[0]));;
	}
}


// start code fragment :: Definition of create_seq //
void create_seq(void* _capture, double seed, double a) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_17 = ((struct __insieme_funType_type_3*)_capture)->p0;
	// --------- Captured Stuff -  End  -------------
	{
		double var_22 = seed;
		double var_23 = a;
		{
			double x = 0.0;
			int i = 0;
			int j = 0;
			int k = 0;
			(k = ((1 << 11)/4));
			{
				for(int var_41 = 0; var_41 < (1 << 16); var_41 += 1) {
					(x = call___insieme_funType_type_4(((struct __insieme_funType_type_4*)(&((struct __insieme_funType_type_5){&randlc, 0, &var_17}))), <?>{v22}</?>, <?>{v23}</?>));
					(x = (x+call___insieme_funType_type_4(((struct __insieme_funType_type_4*)(&((struct __insieme_funType_type_5){&randlc, 0, &var_17}))), <?>{v22}</?>, <?>{v23}</?>)));
					(x = (x+call___insieme_funType_type_4(((struct __insieme_funType_type_4*)(&((struct __insieme_funType_type_5){&randlc, 0, &var_17}))), <?>{v22}</?>, <?>{v23}</?>)));
					(x = (x+call___insieme_funType_type_4(((struct __insieme_funType_type_4*)(&((struct __insieme_funType_type_5){&randlc, 0, &var_17}))), <?>{v22}</?>, <?>{v23}</?>)));
					((*((*((*var_17).key_array))[((unsigned int)(var_41))])) = ((int)((((double)(k))*x))));
				};
				(i = (1 << 16));
			};
		};
	}
}


// start code fragment :: Definitions for function type: __insieme_funType_type_6 //
// Abstract prototype for lambdas of type __insieme_funType_type_6
struct __insieme_funType_type_6 { 
    int(*fun)(void*);
    const size_t size;
};

// Type safe function for invoking lambdas of type __insieme_funType_type_6
int call___insieme_funType_type_6(struct __insieme_funType_type_6* lambda) { return lambda->fun(lambda); }

// start code fragment :: Definitions for function type: __insieme_funType_type_7 //
// Abstract prototype for lambdas of type __insieme_funType_type_7
struct __insieme_funType_type_7 { 
    int(*fun)(void*);
    const size_t size;
    struct __insieme_globals* p0;
    int* p1;
    int(* p2)[2048];
};

// start code fragment :: Definition of __insieme_supp_8 //
int __insieme_supp_8(void* _capture) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_59 = ((struct __insieme_funType_type_7*)_capture)->p0;
	int* var_60 = ((struct __insieme_funType_type_7*)_capture)->p1;
	int[2048]* var_61 = ((struct __insieme_funType_type_7*)_capture)->p2;
	// --------- Captured Stuff -  End  -------------
	{
		int var_58 = (*((*var_61)[((unsigned int)((*((*((*var_59).key_buff2))[((unsigned int)((*var_60)))]))))]));
		((*((*var_61)[((unsigned int)((*((*((*var_59).key_buff2))[((unsigned int)((*var_60)))]))))])) = ((*((*var_61)[((unsigned int)((*((*((*var_59).key_buff2))[((unsigned int)((*var_60)))]))))]))+((int)(1))));
		return var_58;;
	}
}


// start code fragment :: Definition of rank //
void rank(void* _capture, int iteration) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_44 = ((struct __insieme_funType_type_1*)_capture)->p0;
	// --------- Captured Stuff -  End  -------------
	{
		int i = 0;
		int j = 0;
		int k = 0;
		int l = 0;
		int m = 0;
		int shift = (11-9);
		int key = 0;
		int min_key_val = 0;
		int max_key_val = 0;
		int prv_buff1[2048];
		{
			((*((*((*var_44).key_array))[((unsigned int)(iteration))])) = iteration);
			((*((*((*var_44).key_array))[((unsigned int)((iteration+10)))])) = ((1 << 11)-iteration));
			{
				for(int var_55 = 0; var_55 < 5; var_55 += 1) ((*((*((*var_44).partial_verify_vals))[((unsigned int)(var_55))])) = (*((*((*var_44).key_array))[((unsigned int)((*((*((*var_44).test_index_array))[((unsigned int)(var_55))]))))])));
				(i = 5);
			};
			{
				for(int var_56 = 0; var_56 < (1 << 11); var_56 += 1) ((*((*((*var_44).key_buff1))[((unsigned int)(var_56))])) = 0);
				(i = (1 << 11));
			};
		};
		{
			for(int var_57 = 0; var_57 < (1 << 11); var_57 += 1) ((*(prv_buff1[((unsigned int)(var_57))])) = 0);
			(i = (1 << 11));
		};
		{
			for(int var_63 = 0; var_63 < (1 << 16); var_63 += 1) {
				((*((*((*var_44).key_buff2))[((unsigned int)(var_63))])) = (*((*((*var_44).key_array))[((unsigned int)(var_63))])));
				call___insieme_funType_type_6(((struct __insieme_funType_type_6*)(&((struct __insieme_funType_type_7){&__insieme_supp_8, 0, &var_44, &var_63, &prv_buff1}))));
			};
			(i = (1 << 16));
		};
		{
			for(int var_64 = 0; var_64 < ((1 << 11)-1); var_64 += 1) ((*(prv_buff1[((unsigned int)((var_64+1)))])) = ((*(prv_buff1[((unsigned int)((var_64+1)))]))+(*(prv_buff1[((unsigned int)(var_64))]))));
			(i = ((1 << 11)-1));
		};
		{
			{
				for(int var_65 = 0; var_65 < (1 << 11); var_65 += 1) ((*((*((*var_44).key_buff1))[((unsigned int)(var_65))])) = ((*((*((*var_44).key_buff1))[((unsigned int)(var_65))]))+(*(prv_buff1[((unsigned int)(var_65))]))));
				(i = (1 << 11));
			};
		};
		{
			{
				for(int var_97 = 0; var_97 < 5; var_97 += 1) {
					(k = (*((*((*var_44).partial_verify_vals))[((unsigned int)(var_97))])));
					if(((0<=k) && (k<=((1 << 16)-1)))) {
						int var_66 = ((int)('S'));
						switch(var_66) {
						case 'S':
							if((var_97<=2)) {
								if(((*((*((*var_44).key_buff1))[((unsigned int)((k-1)))])) != ((*((*((*var_44).test_rank_array))[((unsigned int)(var_97))]))+iteration))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_97);
								};
							}; break;
						case 'W':
							if((var_97<2)) {
								if(((*((*((*var_44).key_buff1))[((unsigned int)((k-1)))])) != ((*((*((*var_44).test_rank_array))[((unsigned int)(var_97))]))+(iteration-2)))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_97);
								};
							}; break;
						case 'A':
							if((var_97<=2)) {
								if(((*((*((*var_44).key_buff1))[((unsigned int)((k-1)))])) != ((*((*((*var_44).test_rank_array))[((unsigned int)(var_97))]))+(iteration-1)))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_97);
								};
							}; break;
						case 'B':
							if((((var_97==1) || (var_97==2)) || (var_97==4))) {
								if(((*((*((*var_44).key_buff1))[((unsigned int)((k-1)))])) != ((*((*((*var_44).test_rank_array))[((unsigned int)(var_97))]))+iteration))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_97);
								};
							}; break;
						case 'C':
							if((var_97<=2)) {
								if(((*((*((*var_44).key_buff1))[((unsigned int)((k-1)))])) != ((*((*((*var_44).test_rank_array))[((unsigned int)(var_97))]))+iteration))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_97);
								};
							}; break;
						};
					} else {};
				};
				(i = 5);
			};
			if((iteration==10)) (((*var_44).key_buff_ptr_global) = (*((*var_44).key_buff1))) else {};
		};
	}
}


// start code fragment :: Definitions for function type: __insieme_funType_type_9 //
// Abstract prototype for lambdas of type __insieme_funType_type_9
struct __insieme_funType_type_9 { 
    double(*fun)(void*);
    const size_t size;
};

// Type safe function for invoking lambdas of type __insieme_funType_type_9
double call___insieme_funType_type_9(struct __insieme_funType_type_9* lambda) { return lambda->fun(lambda); }

// start code fragment :: Definitions for function type: __insieme_funType_type_10 //
// Abstract prototype for lambdas of type __insieme_funType_type_10
struct __insieme_funType_type_10 { 
    double(*fun)(void*);
    const size_t size;
    struct __insieme_globals* p0;
};

// start code fragment :: Definitions for function type: __insieme_funType_type_11 //
// Abstract prototype for lambdas of type __insieme_funType_type_11
struct __insieme_funType_type_11 { 
    void(*fun)(void*,double*);
    const size_t size;
};

// Type safe function for invoking lambdas of type __insieme_funType_type_11
void call___insieme_funType_type_11(struct __insieme_funType_type_11* lambda,double* p1) { return lambda->fun(lambda,p1); }

// start code fragment :: Definitions for function type: __insieme_funType_type_12 //
// Abstract prototype for lambdas of type __insieme_funType_type_12
struct __insieme_funType_type_12 { 
    void(*fun)(void*,double*);
    const size_t size;
    struct __insieme_globals* p0;
};

// start code fragment :: type_declaration_timeval //
struct timeval { 
    long tv_sec;
    long tv_usec;
};

// start code fragment :: Prototype for external function: gettimeofday //
int gettimeofday(struct timeval*, struct timezone*);

// start code fragment :: Definition of wtime //
void wtime(void* _capture, double* t) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_104 = ((struct __insieme_funType_type_12*)_capture)->p0;
	// --------- Captured Stuff -  End  -------------
	{
		struct timeval tv = ((struct timeval){00});
		gettimeofday(<?>{v105}</?>, ((struct timezone*)((*((void*)(0))))));
		if((((*var_104).sec)<0)) (((*var_104).sec) = ((int)((tv.tv_sec)))) else {};
		((t[0]) = (((double)(((tv.tv_sec)-((long)(((*var_104).sec))))))+(1.0e-6*((double)((tv.tv_usec))))));
	}
}


// start code fragment :: Definition of elapsed_time //
double elapsed_time(void* _capture) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_101 = ((struct __insieme_funType_type_10*)_capture)->p0;
	// --------- Captured Stuff -  End  -------------
	{
		double t = 0.0;
		call___insieme_funType_type_11(((struct __insieme_funType_type_11*)(&((struct __insieme_funType_type_12){&wtime, 0, &var_101}))), <?>{v102}</?>);
		return &t;;
	}
}


// start code fragment :: Definition of timer_start //
void timer_start(void* _capture, int n) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_100 = ((struct __insieme_funType_type_1*)_capture)->p0;
	// --------- Captured Stuff -  End  -------------
	{
		((*((*((*var_100).start))[((unsigned int)(n))])) = call___insieme_funType_type_9(((struct __insieme_funType_type_9*)(&((struct __insieme_funType_type_10){&elapsed_time, 0, &var_100})))));
	}
}


// start code fragment :: Definition of timer_stop //
void timer_stop(void* _capture, int n) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_111 = ((struct __insieme_funType_type_1*)_capture)->p0;
	// --------- Captured Stuff -  End  -------------
	{
		double t = 0.0;
		double now = 0.0;
		(now = call___insieme_funType_type_9(((struct __insieme_funType_type_9*)(&((struct __insieme_funType_type_10){&elapsed_time, 0, &var_111})))));
		(t = (now-(*((*((*var_111).start))[((unsigned int)(n))]))));
		((*((*((*var_111).elapsed))[((unsigned int)(n))])) = ((*((*((*var_111).elapsed))[((unsigned int)(n))]))+t));
	}
}


// start code fragment :: Definitions for function type: __insieme_funType_type_13 //
// Abstract prototype for lambdas of type __insieme_funType_type_13
struct __insieme_funType_type_13 { 
    double(*fun)(void*,int);
    const size_t size;
};

// Type safe function for invoking lambdas of type __insieme_funType_type_13
double call___insieme_funType_type_13(struct __insieme_funType_type_13* lambda,int p1) { return lambda->fun(lambda,p1); }

// start code fragment :: Definitions for function type: __insieme_funType_type_14 //
// Abstract prototype for lambdas of type __insieme_funType_type_14
struct __insieme_funType_type_14 { 
    double(*fun)(void*,int);
    const size_t size;
    struct __insieme_globals* p0;
};

// start code fragment :: Definition of timer_read //
double timer_read(void* _capture, int n) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_116 = ((struct __insieme_funType_type_14*)_capture)->p0;
	// --------- Captured Stuff -  End  -------------
	{
		return ((*((*var_116).elapsed))[((unsigned int)(n))]);;
	}
}


// start code fragment :: Definitions for function type: __insieme_funType_type_15 //
// Abstract prototype for lambdas of type __insieme_funType_type_15
struct __insieme_funType_type_15 { 
    void(*fun)(void*);
    const size_t size;
};

// Type safe function for invoking lambdas of type __insieme_funType_type_15
void call___insieme_funType_type_15(struct __insieme_funType_type_15* lambda) { return lambda->fun(lambda); }

// start code fragment :: Definitions for function type: __insieme_funType_type_16 //
// Abstract prototype for lambdas of type __insieme_funType_type_16
struct __insieme_funType_type_16 { 
    void(*fun)(void*);
    const size_t size;
    struct __insieme_globals* p0;
};

// start code fragment :: Definitions for function type: __insieme_funType_type_17 //
// Abstract prototype for lambdas of type __insieme_funType_type_17
struct __insieme_funType_type_17 { 
    int(*fun)(void*);
    const size_t size;
    struct __insieme_globals* p0;
    int* p1;
};

// start code fragment :: Definition of __insieme_supp_18 //
int __insieme_supp_18(void* _capture) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_124 = ((struct __insieme_funType_type_17*)_capture)->p0;
	int* var_125 = ((struct __insieme_funType_type_17*)_capture)->p1;
	// --------- Captured Stuff -  End  -------------
	{
		(((*((*var_124).key_buff_ptr_global))[((unsigned int)((*((*((*var_124).key_buff2))[((unsigned int)((*var_125)))]))))]) = (((*((*var_124).key_buff_ptr_global))[((unsigned int)((*((*((*var_124).key_buff2))[((unsigned int)((*var_125)))]))))])-((int)(1))));
		((*((*var_124).key_buff_ptr_global))[((unsigned int)((*((*((*var_124).key_buff2))[((unsigned int)((*var_125)))]))))]);
	}
}


// start code fragment :: Definitions for function type: __insieme_funType_type_19 //
// Abstract prototype for lambdas of type __insieme_funType_type_19
struct __insieme_funType_type_19 { 
    int(*fun)(void*);
    const size_t size;
    int* p0;
};

// start code fragment :: Definition of __insieme_supp_20 //
int __insieme_supp_20(void* _capture) {
	// --------- Captured Stuff - Begin -------------
	int* var_129 = ((struct __insieme_funType_type_19*)_capture)->p0;
	// --------- Captured Stuff -  End  -------------
	{
		int var_128 = (*var_129);
		((*var_129) = ((*var_129)+((int)(1))));
		return var_128;;
	}
}


// start code fragment :: Definition of full_verify //
void full_verify(void* _capture) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_globals* var_118 = ((struct __insieme_funType_type_16*)_capture)->p0;
	// --------- Captured Stuff -  End  -------------
	{
		int i = 0;
		int j = 0;
		int k = 0;
		int m = 0;
		int unique_keys = 0;
		{
			for(int var_127 = 0; var_127 < (1 << 16); var_127 += 1) ((*((*((*var_118).key_array))[((unsigned int)(call___insieme_funType_type_6(((struct __insieme_funType_type_6*)(&((struct __insieme_funType_type_17){&__insieme_supp_18, 0, &var_118, &var_127}))))))])) = (*((*((*var_118).key_buff2))[((unsigned int)(var_127))])));
			(i = (1 << 16));
		};
		(j = 0);
		{
			for(int var_131 = 1; var_131 < (1 << 16); var_131 += 1) if(((*((*((*var_118).key_array))[((unsigned int)((var_131-1)))]))>(*((*((*var_118).key_array))[((unsigned int)(var_131))])))) call___insieme_funType_type_6(((struct __insieme_funType_type_6*)(&((struct __insieme_funType_type_19){&__insieme_supp_20, 0, &j})))) else {};
			(i = (1 << 16));
		};
		if((j != 0)) {
			printf(((char*)("Full_verify: number of keys out of sort: %d\n")), j);
		};
	}
}


// start code fragment :: Definition of c_print_results //
void c_print_results(char* name, char class, int n1, int n2, int n3, int niter, int nthreads, double t, double mops, char* optype, int passed_verification, char* npbversion, char* compiletime, char* cc, char* clink, char* c_lib, char* c_inc, char* cflags, char* clinkflags, char* rand) {
	{
		char* evalue = ((char*)("1000"));
		printf(((char*)("\n\n %s Benchmark Completed\n")), name);
		printf(((char*)(" Class           =                        %c\n")), ((int)(class)));
		if(((n2==0) && (n3==0))) printf(((char*)(" Size            =             %12d\n")), n1);
		printf(((char*)(" Iterations      =             %12d\n")), niter);
		printf(((char*)(" Threads         =             %12d\n")), nthreads);
		printf(((char*)(" Time in seconds =             %12.2f\n")), t);
		printf(((char*)(" Mop/s total     =             %12.2f\n")), mops);
		printf(((char*)(" Operation type  = %24s\n")), optype);
		if(((bool)(passed_verification))) printf(((char*)(" Verification    =               SUCCESSFUL\n")), );
		printf(((char*)(" Version         =             %12s\n")), npbversion);
		printf(((char*)(" Compile date    =             %12s\n")), compiletime);
		printf(((char*)("\n Compile options:\n")), );
		printf(((char*)("    CC           = %s\n")), cc);
		printf(((char*)("    CLINK        = %s\n")), clink);
		printf(((char*)("    C_LIB        = %s\n")), c_lib);
		printf(((char*)("    C_INC        = %s\n")), c_inc);
		printf(((char*)("    CFLAGS       = %s\n")), cflags);
		printf(((char*)("    CLINKFLAGS   = %s\n")), clinkflags);
		printf(((char*)("    RAND         = %s\n")), rand);
	}
}


// start code fragment :: Definition of main //
int main(int argc, char** argv) {
	{
		struct __insieme_globals var_1 = ((struct __insieme_globals){el, start, elapsed, key_buff_ptr_global, passed_verification, key_array, key_buff1, key_buff2, partial_verify_vals, test_index_array, test_rank_array, <?>{ref.var(48427),ref.var(17148),ref.var(23627),ref.var(62548),ref.var(4431)}</?>, <?>{ref.var(0),ref.var(18),ref.var(346),ref.var(64917),ref.var(65463)}</?>, <?>{ref.var(357773),ref.var(934767),ref.var(875723),ref.var(898999),ref.var(404505)}</?>, <?>{ref.var(1249),ref.var(11698),ref.var(1039987),ref.var(1043896),ref.var(1048018)}</?>, <?>{ref.var(2112377),ref.var(662041),ref.var(5336171),ref.var(3642833),ref.var(4250760)}</?>, <?>{ref.var(104),ref.var(17523),ref.var(123928),ref.var(8288932),ref.var(8388264)}</?>, <?>{ref.var(41869),ref.var(812306),ref.var(5102857),ref.var(18232239),ref.var(26860214)}</?>, <?>{ref.var(33422937),ref.var(10244),ref.var(59149),ref.var(33135281),ref.var(99)}</?>, <?>{ref.var(44172927),ref.var(72999161),ref.var(74326391),ref.var(129606274),ref.var(21736814)}</?>, <?>{ref.var(61147),ref.var(882988),ref.var(266290),ref.var(133997595),ref.var(133525895)}</?>, 0, 0.0, 0.0, 0.00.0});
		int i = 0;
		int iteration = 0;
		int itemp = 0;
		int nthreads = 1;
		double timecounter = 0.0;
		double maxtime = 0.0;
		{
			for(int var_11 = 0; var_11 < 5; var_11 += 1) {
				int var_10 = ((int)('S'));
				switch(var_10) {
				case 'S':
					{
						((*((*(var_1.test_index_array))[((unsigned int)(var_11))])) = (*((var_1.S_test_index_array)[((unsigned int)(var_11))])));
						((*((*(var_1.test_rank_array))[((unsigned int)(var_11))])) = (*((var_1.S_test_rank_array)[((unsigned int)(var_11))])));
					}; break;
				case 'A':
					{
						((*((*(var_1.test_index_array))[((unsigned int)(var_11))])) = (*((var_1.A_test_index_array)[((unsigned int)(var_11))])));
						((*((*(var_1.test_rank_array))[((unsigned int)(var_11))])) = (*((var_1.A_test_rank_array)[((unsigned int)(var_11))])));
					}; break;
				case 'W':
					{
						((*((*(var_1.test_index_array))[((unsigned int)(var_11))])) = (*((var_1.W_test_index_array)[((unsigned int)(var_11))])));
						((*((*(var_1.test_rank_array))[((unsigned int)(var_11))])) = (*((var_1.W_test_rank_array)[((unsigned int)(var_11))])));
					}; break;
				case 'B':
					{
						((*((*(var_1.test_index_array))[((unsigned int)(var_11))])) = (*((var_1.B_test_index_array)[((unsigned int)(var_11))])));
						((*((*(var_1.test_rank_array))[((unsigned int)(var_11))])) = (*((var_1.B_test_rank_array)[((unsigned int)(var_11))])));
					}; break;
				case 'C':
					{
						((*((*(var_1.test_index_array))[((unsigned int)(var_11))])) = (*((var_1.C_test_index_array)[((unsigned int)(var_11))])));
						((*((*(var_1.test_rank_array))[((unsigned int)(var_11))])) = (*((var_1.C_test_rank_array)[((unsigned int)(var_11))])));
					}; break;
				};
			};
			(i = 5);
		};
		{};
		printf(((char*)("\n\n NAS Parallel Benchmarks 2.3 OpenMP C version")), );
		printf(((char*)(" Size:  %d  (class %c)\n")), (1 << 16), 'S');
		printf(((char*)(" Iterations:   %d\n")), 10);
		call___insieme_funType_type_0(((struct __insieme_funType_type_0*)(&((struct __insieme_funType_type_1){&timer_clear, 0, &var_1}))), 0);
		call___insieme_funType_type_2(((struct __insieme_funType_type_2*)(&((struct __insieme_funType_type_3){&create_seq, 0, &var_1}))), 314159265.00, 1220703125.00);
		call___insieme_funType_type_0(((struct __insieme_funType_type_0*)(&((struct __insieme_funType_type_1){&rank, 0, &var_1}))), 1);
		((var_1.passed_verification) = 0);
		if(('S' != 'S')) printf(((char*)("\n   iteration\n")), ) else {};
		call___insieme_funType_type_0(((struct __insieme_funType_type_0*)(&((struct __insieme_funType_type_1){&timer_start, 0, &var_1}))), 0);
		{
			for(int var_109 = 1; var_109 < 10; var_109 += 1) {
				if(('S' != 'S')) printf(((char*)("        %d\n")), var_109) else {};
				call___insieme_funType_type_0(((struct __insieme_funType_type_0*)(&((struct __insieme_funType_type_1){&rank, 0, &var_1}))), var_109);
			};
			(iteration = 10);
		};
		call___insieme_funType_type_0(((struct __insieme_funType_type_0*)(&((struct __insieme_funType_type_1){&timer_stop, 0, &var_1}))), 0);
		(timecounter = call___insieme_funType_type_13(((struct __insieme_funType_type_13*)(&((struct __insieme_funType_type_14){&timer_read, 0, &var_1}))), 0));
		call___insieme_funType_type_15(((struct __insieme_funType_type_15*)(&((struct __insieme_funType_type_16){&full_verify, 0, &var_1}))));
		if(((*(var_1.passed_verification)) != ((5*10)+1))) ((var_1.passed_verification) = 0) else {};
		c_print_results(((char*)("IS")), ((char)('S')), (1 << 16), 0, 0, 10, nthreads, timecounter, ((((double)((10*(1 << 16))))/timecounter)/1000000.), ((char*)("keys ranked")), (*(var_1.passed_verification)), ((char*)("2.3")), ((char*)("03 Nov 2010")), ((char*)("cc")), ((char*)("cc")), ((char*)("-lm")), ((char*)("-I../common")), ((char*)("-O3 ")), ((char*)("-lm")), ((char*)("randlc")));
	}
}

