// --- Generated Inspire Code ---
#include <stddef.h>
#define bool int
#define true 1
#define false 0
// --- Entry Point ---

// start code fragment :: lambda_main //
struct main_closure { 
    int(*fun)(void*,int,char**);
    const size_t size;
};

// start code fragment :: type_declaration___insieme_userdefined_type_type_0 //
struct __insieme_userdefined_type_type_0 { 
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
    int(*S_test_index_array)[5];
    int(*S_test_rank_array)[5];
    int(*W_test_index_array)[5];
    int(*W_test_rank_array)[5];
    int(*A_test_index_array)[5];
    int(*A_test_rank_array)[5];
    int(*B_test_index_array)[5];
    int(*B_test_rank_array)[5];
    int(*C_test_index_array)[5];
    int(*C_test_rank_array)[5];
    int KS;
    double R23;
    double R46;
    double T23;
    double T46;
};

// start code fragment :: fundef_codefragment_printf //
int printf(char*, ...);

// start code fragment :: lambda_struct___insieme_funType_type_1 //
struct __insieme_funType_type_1 { 
    void(*fun)(void*,int);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_1 //
void call___insieme_funType_type_1(struct __insieme_funType_type_1* lambda,int p1) { return lambda->fun(lambda,p1); }

// start code fragment :: fun_type_utilities___insieme_funType_type_1 //

// start code fragment :: lambda_timer_clear //
struct timer_clear_closure { 
    void(*fun)(void*,int);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_13;
};

// start code fragment :: fundef_codefragment_timer_clear //
void timer_clear(void* _capture, int n) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_userdefined_type_type_0* var_13 = ((struct timer_clear_closure*)_capture)->var_13;
	// --------- Captured Stuff -  End  -------------
	{
		(*((*var_13).elapsed))[((unsigned int)(n))] = 0.0;
	}
}


// start code fragment :: lambda_struct___insieme_funType_type_2 //
struct __insieme_funType_type_2 { 
    void(*fun)(void*,double,double);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_2 //
void call___insieme_funType_type_2(struct __insieme_funType_type_2* lambda,double p1,double p2) { return lambda->fun(lambda,p1,p2); }

// start code fragment :: fun_type_utilities___insieme_funType_type_2 //

// start code fragment :: lambda_create_seq //
struct create_seq_closure { 
    void(*fun)(void*,double,double);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_16;
};

// start code fragment :: lambda_struct___insieme_funType_type_3 //
struct __insieme_funType_type_3 { 
    double(*fun)(void*,double*,double*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_3 //
double call___insieme_funType_type_3(struct __insieme_funType_type_3* lambda,double* p1,double* p2) { return lambda->fun(lambda,p1,p2); }

// start code fragment :: fun_type_utilities___insieme_funType_type_3 //

// start code fragment :: lambda_randlc //
struct randlc_closure { 
    double(*fun)(void*,double*,double*);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_25;
};

// start code fragment :: fundef_codefragment_randlc //
double randlc(void* _capture, double* X, double* A) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_userdefined_type_type_0* var_25 = ((struct randlc_closure*)_capture)->var_25;
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
		if((((*var_25).KS) == 0)) {
			((*var_25).R23) = 1.0;
			((*var_25).R46) = 1.0;
			((*var_25).T23) = 1.0;
			((*var_25).T46) = 1.0;
			{
				for(int var_37 = 1; var_37 < 23; var_37 += 1) {
					((*var_25).R23) = (0.50 * ((*var_25).R23));
					((*var_25).T23) = (2.0 * ((*var_25).T23));
				};
				i = 23;
			};
			{
				for(int var_38 = 1; var_38 < 46; var_38 += 1) {
					((*var_25).R46) = (0.50 * ((*var_25).R46));
					((*var_25).T46) = (2.0 * ((*var_25).T46));
				};
				i = 46;
			};
			((*var_25).KS) = 1;
		};
		T1 = (((*var_25).R23) * A[0]);
		j = ((int)(T1));
		A1 = ((double)(j));
		A2 = (A[0] - (((*var_25).T23) * A1));
		T1 = (((*var_25).R23) * X[0]);
		j = ((int)(T1));
		X1 = ((double)(j));
		X2 = (X[0] - (((*var_25).T23) * X1));
		T1 = ((A1 * X2) + (A2 * X1));
		j = ((int)((((*var_25).R23) * T1)));
		T2 = ((double)(j));
		Z = (T1 - (((*var_25).T23) * T2));
		T3 = ((((*var_25).T23) * Z) + (A2 * X2));
		j = ((int)((((*var_25).R46) * T3)));
		T4 = ((double)(j));
		X[0] = (T3 - (((*var_25).T46) * T4));
		return (((*var_25).R46) * X[0]);;
	}
}


// start code fragment :: lambda_randlc //
struct randlc_closure { 
    double(*fun)(void*,double*,double*);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_25;
};

// start code fragment :: lambda_randlc //
struct randlc_closure { 
    double(*fun)(void*,double*,double*);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_25;
};

// start code fragment :: lambda_randlc //
struct randlc_closure { 
    double(*fun)(void*,double*,double*);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_25;
};

// start code fragment :: fundef_codefragment_create_seq //
void create_seq(void* _capture, double seed, double a) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_userdefined_type_type_0* var_16 = ((struct create_seq_closure*)_capture)->var_16;
	// --------- Captured Stuff -  End  -------------
	{
		double var_21 = seed;
		double var_22 = a;
		{
			double x = 0.0;
			int i = 0;
			int j = 0;
			int k = 0;
			k = ((1 << 11) / 4);
			{
				for(int var_39 = 0; var_39 < (1 << 16); var_39 += 1) {
					x = call___insieme_funType_type_3(((struct __insieme_funType_type_3*)&((struct randlc_closure){&randlc, sizeof(struct randlc_closure),&var_16})), <?>{v21}</?>, <?>{v22}</?>);
					x = (x + call___insieme_funType_type_3(((struct __insieme_funType_type_3*)&((struct randlc_closure){&randlc, sizeof(struct randlc_closure),&var_16})), <?>{v21}</?>, <?>{v22}</?>));
					x = (x + call___insieme_funType_type_3(((struct __insieme_funType_type_3*)&((struct randlc_closure){&randlc, sizeof(struct randlc_closure),&var_16})), <?>{v21}</?>, <?>{v22}</?>));
					x = (x + call___insieme_funType_type_3(((struct __insieme_funType_type_3*)&((struct randlc_closure){&randlc, sizeof(struct randlc_closure),&var_16})), <?>{v21}</?>, <?>{v22}</?>));
					(*((*var_16).key_array))[((unsigned int)(var_39))] = ((int)((((double)(k)) * x)));
				};
				i = (1 << 16);
			};
		};
	}
}


// start code fragment :: lambda_rank //
struct rank_closure { 
    void(*fun)(void*,int);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_41;
};

// start code fragment :: lambda_struct___insieme_funType_type_4 //
struct __insieme_funType_type_4 { 
    int(*fun)(void*,struct __insieme_userdefined_type_type_0*,int*,int[2048]*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_4 //
int call___insieme_funType_type_4(struct __insieme_funType_type_4* lambda,struct __insieme_userdefined_type_type_0* p1,int* p2,int(*p3)[2048]) { return lambda->fun(lambda,p1,p2,p3); }

// start code fragment :: fun_type_utilities___insieme_funType_type_4 //

// start code fragment :: lambda___insieme_lambda_fun_5 //
struct __insieme_lambda_fun_5_closure { 
    int(*fun)(void*,struct __insieme_userdefined_type_type_0*,int*,int[2048]*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_5 //
int __insieme_lambda_fun_5(void* _capture, struct __insieme_userdefined_type_type_0* var_56, int* var_57, int(*var_58)[2048]) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int var_55 = (*var_58)[((unsigned int)((*((*var_56).key_buff2))[((unsigned int)((*var_57)))]))];
		(*var_58)[((unsigned int)((*((*var_56).key_buff2))[((unsigned int)((*var_57)))]))] = (*var_58)[((unsigned int)((*((*var_56).key_buff2))[((unsigned int)((*var_57)))]))]+((int)(1));
		return var_55;;
	}
}


// start code fragment :: lambda_struct___insieme_funType_type_6 //
struct __insieme_funType_type_6 { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_6 //
int* call___insieme_funType_type_6(struct __insieme_funType_type_6* lambda,struct __insieme_userdefined_type_type_0* p1) { return lambda->fun(lambda,p1); }

// start code fragment :: fun_type_utilities___insieme_funType_type_6 //

// start code fragment :: lambda___insieme_lambda_fun_7 //
struct __insieme_lambda_fun_7_closure { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_7 //
int* __insieme_lambda_fun_7(void* _capture, struct __insieme_userdefined_type_type_0* var_64) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int* var_63 = ((*var_64).passed_verification);
		((*var_64).passed_verification) = ((*var_64).passed_verification)+((int*)(1));
		return &var_63;;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_8 //
struct __insieme_lambda_fun_8_closure { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_8 //
int* __insieme_lambda_fun_8(void* _capture, struct __insieme_userdefined_type_type_0* var_66) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int* var_65 = ((*var_66).passed_verification);
		((*var_66).passed_verification) = ((*var_66).passed_verification)+((int*)(1));
		return &var_65;;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_9 //
struct __insieme_lambda_fun_9_closure { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_9 //
int* __insieme_lambda_fun_9(void* _capture, struct __insieme_userdefined_type_type_0* var_68) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int* var_67 = ((*var_68).passed_verification);
		((*var_68).passed_verification) = ((*var_68).passed_verification)+((int*)(1));
		return &var_67;;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_10 //
struct __insieme_lambda_fun_10_closure { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_10 //
int* __insieme_lambda_fun_10(void* _capture, struct __insieme_userdefined_type_type_0* var_70) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int* var_69 = ((*var_70).passed_verification);
		((*var_70).passed_verification) = ((*var_70).passed_verification)+((int*)(1));
		return &var_69;;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_11 //
struct __insieme_lambda_fun_11_closure { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_11 //
int* __insieme_lambda_fun_11(void* _capture, struct __insieme_userdefined_type_type_0* var_72) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int* var_71 = ((*var_72).passed_verification);
		((*var_72).passed_verification) = ((*var_72).passed_verification)+((int*)(1));
		return &var_71;;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_12 //
struct __insieme_lambda_fun_12_closure { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_12 //
int* __insieme_lambda_fun_12(void* _capture, struct __insieme_userdefined_type_type_0* var_74) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int* var_73 = ((*var_74).passed_verification);
		((*var_74).passed_verification) = ((*var_74).passed_verification)+((int*)(1));
		return &var_73;;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_13 //
struct __insieme_lambda_fun_13_closure { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_13 //
int* __insieme_lambda_fun_13(void* _capture, struct __insieme_userdefined_type_type_0* var_76) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int* var_75 = ((*var_76).passed_verification);
		((*var_76).passed_verification) = ((*var_76).passed_verification)+((int*)(1));
		return &var_75;;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_14 //
struct __insieme_lambda_fun_14_closure { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_14 //
int* __insieme_lambda_fun_14(void* _capture, struct __insieme_userdefined_type_type_0* var_78) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int* var_77 = ((*var_78).passed_verification);
		((*var_78).passed_verification) = ((*var_78).passed_verification)+((int*)(1));
		return &var_77;;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_15 //
struct __insieme_lambda_fun_15_closure { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_15 //
int* __insieme_lambda_fun_15(void* _capture, struct __insieme_userdefined_type_type_0* var_80) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int* var_79 = ((*var_80).passed_verification);
		((*var_80).passed_verification) = ((*var_80).passed_verification)+((int*)(1));
		return &var_79;;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_16 //
struct __insieme_lambda_fun_16_closure { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_16 //
int* __insieme_lambda_fun_16(void* _capture, struct __insieme_userdefined_type_type_0* var_82) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int* var_81 = ((*var_82).passed_verification);
		((*var_82).passed_verification) = ((*var_82).passed_verification)+((int*)(1));
		return &var_81;;
	}
}


// start code fragment :: fundef_codefragment_rank //
void rank(void* _capture, int iteration) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_userdefined_type_type_0* var_41 = ((struct rank_closure*)_capture)->var_41;
	// --------- Captured Stuff -  End  -------------
	{
		int i = 0;
		int j = 0;
		int k = 0;
		int l = 0;
		int m = 0;
		int shift = (11 - 9);
		int key = 0;
		int min_key_val = 0;
		int max_key_val = 0;
		int prv_buff1[2048];
		{
			(*((*var_41).key_array))[((unsigned int)(iteration))] = iteration;
			(*((*var_41).key_array))[((unsigned int)((iteration + 10)))] = ((1 << 11) - iteration);
			{
				for(int var_52 = 0; var_52 < 5; var_52 += 1) (*((*var_41).partial_verify_vals))[((unsigned int)(var_52))] = (*((*var_41).key_array))[((unsigned int)((*((*var_41).test_index_array))[((unsigned int)(var_52))]))];
				i = 5;
			};
			{
				for(int var_53 = 0; var_53 < (1 << 11); var_53 += 1) (*((*var_41).key_buff1))[((unsigned int)(var_53))] = 0;
				i = (1 << 11);
			};
		};
		{
			for(int var_54 = 0; var_54 < (1 << 11); var_54 += 1) prv_buff1[((unsigned int)(var_54))] = 0;
			i = (1 << 11);
		};
		{
			for(int var_59 = 0; var_59 < (1 << 16); var_59 += 1) {
				(*((*var_41).key_buff2))[((unsigned int)(var_59))] = (*((*var_41).key_array))[((unsigned int)(var_59))];
				call___insieme_funType_type_4(((struct __insieme_funType_type_4*)&((struct __insieme_lambda_fun_5_closure){&__insieme_lambda_fun_5, sizeof(struct __insieme_lambda_fun_5_closure)})), &var_41, &var_59, &prv_buff1);
			};
			i = (1 << 16);
		};
		{
			for(int var_60 = 0; var_60 < ((1 << 11) - 1); var_60 += 1) prv_buff1[((unsigned int)((var_60 + 1)))] = (prv_buff1[((unsigned int)((var_60 + 1)))] + prv_buff1[((unsigned int)(var_60))]);
			i = ((1 << 11) - 1);
		};
		{
			{
				for(int var_61 = 0; var_61 < (1 << 11); var_61 += 1) (*((*var_41).key_buff1))[((unsigned int)(var_61))] = ((*((*var_41).key_buff1))[((unsigned int)(var_61))] + prv_buff1[((unsigned int)(var_61))]);
				i = (1 << 11);
			};
		};
		{
			{
				for(int var_83 = 0; var_83 < 5; var_83 += 1) {
					k = (*((*var_41).partial_verify_vals))[((unsigned int)(var_83))];
					if(((0 <= k) && (k <= ((1 << 16) - 1)))) {
						int var_62 = ((int)('S'));
						switch(var_62) {
						case 'S':
							if((var_83 <= 2)) {
								if(((*((*var_41).key_buff1))[((unsigned int)((k - 1)))] != ((*((*var_41).test_rank_array))[((unsigned int)(var_83))] + iteration))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_83);
								} else call___insieme_funType_type_6(((struct __insieme_funType_type_6*)&((struct __insieme_lambda_fun_7_closure){&__insieme_lambda_fun_7, sizeof(struct __insieme_lambda_fun_7_closure)})), &var_41);
							} else {
								if(((*((*var_41).key_buff1))[((unsigned int)((k - 1)))] != ((*((*var_41).test_rank_array))[((unsigned int)(var_83))] - iteration))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_83);
								} else call___insieme_funType_type_6(((struct __insieme_funType_type_6*)&((struct __insieme_lambda_fun_8_closure){&__insieme_lambda_fun_8, sizeof(struct __insieme_lambda_fun_8_closure)})), &var_41);
							}; break;
						case 'W':
							if((var_83 < 2)) {
								if(((*((*var_41).key_buff1))[((unsigned int)((k - 1)))] != ((*((*var_41).test_rank_array))[((unsigned int)(var_83))] + (iteration - 2)))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_83);
								} else call___insieme_funType_type_6(((struct __insieme_funType_type_6*)&((struct __insieme_lambda_fun_9_closure){&__insieme_lambda_fun_9, sizeof(struct __insieme_lambda_fun_9_closure)})), &var_41);
							} else {
								if(((*((*var_41).key_buff1))[((unsigned int)((k - 1)))] != ((*((*var_41).test_rank_array))[((unsigned int)(var_83))] - iteration))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_83);
								} else call___insieme_funType_type_6(((struct __insieme_funType_type_6*)&((struct __insieme_lambda_fun_10_closure){&__insieme_lambda_fun_10, sizeof(struct __insieme_lambda_fun_10_closure)})), &var_41);
							}; break;
						case 'A':
							if((var_83 <= 2)) {
								if(((*((*var_41).key_buff1))[((unsigned int)((k - 1)))] != ((*((*var_41).test_rank_array))[((unsigned int)(var_83))] + (iteration - 1)))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_83);
								} else call___insieme_funType_type_6(((struct __insieme_funType_type_6*)&((struct __insieme_lambda_fun_11_closure){&__insieme_lambda_fun_11, sizeof(struct __insieme_lambda_fun_11_closure)})), &var_41);
							} else {
								if(((*((*var_41).key_buff1))[((unsigned int)((k - 1)))] != ((*((*var_41).test_rank_array))[((unsigned int)(var_83))] - (iteration - 1)))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_83);
								} else call___insieme_funType_type_6(((struct __insieme_funType_type_6*)&((struct __insieme_lambda_fun_12_closure){&__insieme_lambda_fun_12, sizeof(struct __insieme_lambda_fun_12_closure)})), &var_41);
							}; break;
						case 'B':
							if((((var_83 == 1) || (var_83 == 2)) || (var_83 == 4))) {
								if(((*((*var_41).key_buff1))[((unsigned int)((k - 1)))] != ((*((*var_41).test_rank_array))[((unsigned int)(var_83))] + iteration))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_83);
								} else call___insieme_funType_type_6(((struct __insieme_funType_type_6*)&((struct __insieme_lambda_fun_13_closure){&__insieme_lambda_fun_13, sizeof(struct __insieme_lambda_fun_13_closure)})), &var_41);
							} else {
								if(((*((*var_41).key_buff1))[((unsigned int)((k - 1)))] != ((*((*var_41).test_rank_array))[((unsigned int)(var_83))] - iteration))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_83);
								} else call___insieme_funType_type_6(((struct __insieme_funType_type_6*)&((struct __insieme_lambda_fun_14_closure){&__insieme_lambda_fun_14, sizeof(struct __insieme_lambda_fun_14_closure)})), &var_41);
							}; break;
						case 'C':
							if((var_83 <= 2)) {
								if(((*((*var_41).key_buff1))[((unsigned int)((k - 1)))] != ((*((*var_41).test_rank_array))[((unsigned int)(var_83))] + iteration))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_83);
								} else call___insieme_funType_type_6(((struct __insieme_funType_type_6*)&((struct __insieme_lambda_fun_15_closure){&__insieme_lambda_fun_15, sizeof(struct __insieme_lambda_fun_15_closure)})), &var_41);
							} else {
								if(((*((*var_41).key_buff1))[((unsigned int)((k - 1)))] != ((*((*var_41).test_rank_array))[((unsigned int)(var_83))] - iteration))) {
									printf(((char*)("Failed partial verification: ")), iteration, var_83);
								} else call___insieme_funType_type_6(((struct __insieme_funType_type_6*)&((struct __insieme_lambda_fun_16_closure){&__insieme_lambda_fun_16, sizeof(struct __insieme_lambda_fun_16_closure)})), &var_41);
							}; break;
						};
					};
				};
				i = 5;
			};
			if((iteration == 10)) ((*var_41).key_buff_ptr_global) = (*((*var_41).key_buff1));
		};
	}
}


// start code fragment :: lambda_timer_start //
struct timer_start_closure { 
    void(*fun)(void*,int);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_85;
};

// start code fragment :: lambda_struct___insieme_funType_type_17 //
struct __insieme_funType_type_17 { 
    double(*fun)(void*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_17 //
double call___insieme_funType_type_17(struct __insieme_funType_type_17* lambda) { return lambda->fun(lambda); }

// start code fragment :: fun_type_utilities___insieme_funType_type_17 //

// start code fragment :: lambda_elapsed_time //
struct elapsed_time_closure { 
    double(*fun)(void*);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_86;
};

// start code fragment :: lambda_struct___insieme_funType_type_18 //
struct __insieme_funType_type_18 { 
    void(*fun)(void*,double*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_18 //
void call___insieme_funType_type_18(struct __insieme_funType_type_18* lambda,double* p1) { return lambda->fun(lambda,p1); }

// start code fragment :: fun_type_utilities___insieme_funType_type_18 //

// start code fragment :: lambda_wtime //
struct wtime_closure { 
    void(*fun)(void*,double*);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_89;
};

// start code fragment :: type_declaration_timeval //
struct timeval { 
    long tv_sec;
    long tv_usec;
};

// start code fragment :: fundef_codefragment_gettimeofday //
int gettimeofday(struct timeval*, struct timezone*);

// start code fragment :: fundef_codefragment_wtime //
void wtime(void* _capture, double* t) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_userdefined_type_type_0* var_89 = ((struct wtime_closure*)_capture)->var_89;
	// --------- Captured Stuff -  End  -------------
	{
		struct timeval tv = ((struct timeval){0, 0, });
		gettimeofday(<?>{v90}</?>, ((struct timezone*)((*((void*)(0))))));
		if((((*var_89).sec) < 0)) ((*var_89).sec) = ((int)((tv.tv_sec)));
		t[0] = (((double)(((tv.tv_sec) - ((long)(((*var_89).sec)))))) + (1.0e-6 * ((double)((tv.tv_usec)))));
	}
}


// start code fragment :: fundef_codefragment_elapsed_time //
double elapsed_time(void* _capture) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_userdefined_type_type_0* var_86 = ((struct elapsed_time_closure*)_capture)->var_86;
	// --------- Captured Stuff -  End  -------------
	{
		double t = 0.0;
		call___insieme_funType_type_18(((struct __insieme_funType_type_18*)&((struct wtime_closure){&wtime, sizeof(struct wtime_closure),&var_86})), <?>{v87}</?>);
		return &t;;
	}
}


// start code fragment :: fundef_codefragment_timer_start //
void timer_start(void* _capture, int n) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_userdefined_type_type_0* var_85 = ((struct timer_start_closure*)_capture)->var_85;
	// --------- Captured Stuff -  End  -------------
	{
		(*((*var_85).start))[((unsigned int)(n))] = call___insieme_funType_type_17(((struct __insieme_funType_type_17*)&((struct elapsed_time_closure){&elapsed_time, sizeof(struct elapsed_time_closure),&var_85})));
	}
}


// start code fragment :: lambda_rank //
struct rank_closure { 
    void(*fun)(void*,int);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_41;
};

// start code fragment :: lambda_timer_stop //
struct timer_stop_closure { 
    void(*fun)(void*,int);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_93;
};

// start code fragment :: lambda_elapsed_time //
struct elapsed_time_closure { 
    double(*fun)(void*);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_86;
};

// start code fragment :: fundef_codefragment_timer_stop //
void timer_stop(void* _capture, int n) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_userdefined_type_type_0* var_93 = ((struct timer_stop_closure*)_capture)->var_93;
	// --------- Captured Stuff -  End  -------------
	{
		double t = 0.0;
		double now = 0.0;
		now = call___insieme_funType_type_17(((struct __insieme_funType_type_17*)&((struct elapsed_time_closure){&elapsed_time, sizeof(struct elapsed_time_closure),&var_85})));
		t = (now - (*((*var_93).start))[((unsigned int)(n))]);
		(*((*var_93).elapsed))[((unsigned int)(n))] = ((*((*var_93).elapsed))[((unsigned int)(n))] + t);
	}
}


// start code fragment :: lambda_struct___insieme_funType_type_19 //
struct __insieme_funType_type_19 { 
    double(*fun)(void*,int);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_19 //
double call___insieme_funType_type_19(struct __insieme_funType_type_19* lambda,int p1) { return lambda->fun(lambda,p1); }

// start code fragment :: fun_type_utilities___insieme_funType_type_19 //

// start code fragment :: lambda_timer_read //
struct timer_read_closure { 
    double(*fun)(void*,int);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_97;
};

// start code fragment :: fundef_codefragment_timer_read //
double timer_read(void* _capture, int n) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_userdefined_type_type_0* var_97 = ((struct timer_read_closure*)_capture)->var_97;
	// --------- Captured Stuff -  End  -------------
	{
		return (*((*var_97).elapsed))[((unsigned int)(n))];;
	}
}


// start code fragment :: lambda_struct___insieme_funType_type_20 //
struct __insieme_funType_type_20 { 
    void(*fun)(void*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_20 //
void call___insieme_funType_type_20(struct __insieme_funType_type_20* lambda) { return lambda->fun(lambda); }

// start code fragment :: fun_type_utilities___insieme_funType_type_20 //

// start code fragment :: lambda_full_verify //
struct full_verify_closure { 
    void(*fun)(void*);
    const size_t size;
    struct __insieme_userdefined_type_type_0* var_98;
};

// start code fragment :: lambda_struct___insieme_funType_type_21 //
struct __insieme_funType_type_21 { 
    int(*fun)(void*,struct __insieme_userdefined_type_type_0*,int*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_21 //
int call___insieme_funType_type_21(struct __insieme_funType_type_21* lambda,struct __insieme_userdefined_type_type_0* p1,int* p2) { return lambda->fun(lambda,p1,p2); }

// start code fragment :: fun_type_utilities___insieme_funType_type_21 //

// start code fragment :: lambda___insieme_lambda_fun_22 //
struct __insieme_lambda_fun_22_closure { 
    int(*fun)(void*,struct __insieme_userdefined_type_type_0*,int*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_22 //
int __insieme_lambda_fun_22(void* _capture, struct __insieme_userdefined_type_type_0* var_104, int* var_105) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		(*((*var_104).key_buff_ptr_global))[((unsigned int)((*((*var_104).key_buff2))[((unsigned int)((*var_105)))]))] = (*((*var_104).key_buff_ptr_global))[((unsigned int)((*((*var_104).key_buff2))[((unsigned int)((*var_105)))]))]-((int)(1));
		(*((*var_104).key_buff_ptr_global))[((unsigned int)((*((*var_104).key_buff2))[((unsigned int)((*var_105)))]))];
	}
}


// start code fragment :: lambda_struct___insieme_funType_type_23 //
struct __insieme_funType_type_23 { 
    int(*fun)(void*,int*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_23 //
int call___insieme_funType_type_23(struct __insieme_funType_type_23* lambda,int* p1) { return lambda->fun(lambda,p1); }

// start code fragment :: fun_type_utilities___insieme_funType_type_23 //

// start code fragment :: lambda___insieme_lambda_fun_24 //
struct __insieme_lambda_fun_24_closure { 
    int(*fun)(void*,int*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_24 //
int __insieme_lambda_fun_24(void* _capture, int* var_108) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int var_107 = (*var_108);
		(*var_108) = (*var_108)+((int)(1));
		return var_107;;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_25 //
struct __insieme_lambda_fun_25_closure { 
    int*(*fun)(void*,struct __insieme_userdefined_type_type_0*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_25 //
int* __insieme_lambda_fun_25(void* _capture, struct __insieme_userdefined_type_type_0* var_111) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int* var_110 = ((*var_111).passed_verification);
		((*var_111).passed_verification) = ((*var_111).passed_verification)+((int*)(1));
		return &var_110;;
	}
}


// start code fragment :: fundef_codefragment_full_verify //
void full_verify(void* _capture) {
	// --------- Captured Stuff - Begin -------------
	struct __insieme_userdefined_type_type_0* var_98 = ((struct full_verify_closure*)_capture)->var_98;
	// --------- Captured Stuff -  End  -------------
	{
		int i = 0;
		int j = 0;
		int k = 0;
		int m = 0;
		int unique_keys = 0;
		{
			for(int var_106 = 0; var_106 < (1 << 16); var_106 += 1) (*((*var_98).key_array))[((unsigned int)(call___insieme_funType_type_21(((struct __insieme_funType_type_21*)&((struct __insieme_lambda_fun_22_closure){&__insieme_lambda_fun_22, sizeof(struct __insieme_lambda_fun_22_closure)})), &var_98, &var_106)))] = (*((*var_98).key_buff2))[((unsigned int)(var_106))];
			i = (1 << 16);
		};
		j = 0;
		{
			for(int var_109 = 1; var_109 < (1 << 16); var_109 += 1) if(((*((*var_98).key_array))[((unsigned int)((var_109 - 1)))] > (*((*var_98).key_array))[((unsigned int)(var_109))])) call___insieme_funType_type_23(((struct __insieme_funType_type_23*)&((struct __insieme_lambda_fun_24_closure){&__insieme_lambda_fun_24, sizeof(struct __insieme_lambda_fun_24_closure)})), &j);
			i = (1 << 16);
		};
		if((j != 0)) {
			printf(((char*)("Full_verify: number of keys out of sort: %d\n")), j);
		} else call___insieme_funType_type_6(((struct __insieme_funType_type_6*)&((struct __insieme_lambda_fun_25_closure){&__insieme_lambda_fun_25, sizeof(struct __insieme_lambda_fun_25_closure)})), &var_98);
	}
}


// start code fragment :: lambda_struct___insieme_funType_type_26 //
struct __insieme_funType_type_26 { 
    void(*fun)(void*,char*,char,int,int,int,int,int,double,double,char*,int,char*,char*,char*,char*,char*,char*,char*,char*,char*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_26 //
void call___insieme_funType_type_26(struct __insieme_funType_type_26* lambda,char* p1,char p2,int p3,int p4,int p5,int p6,int p7,double p8,double p9,char* p10,int p11,char* p12,char* p13,char* p14,char* p15,char* p16,char* p17,char* p18,char* p19,char* p20) { return lambda->fun(lambda,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20); }

// start code fragment :: fun_type_utilities___insieme_funType_type_26 //

// start code fragment :: lambda_c_print_results //
struct c_print_results_closure { 
    void(*fun)(void*,char*,char,int,int,int,int,int,double,double,char*,int,char*,char*,char*,char*,char*,char*,char*,char*,char*);
    const size_t size;
};

// start code fragment :: fundef_codefragment_c_print_results //
void c_print_results(void* _capture, char* name, char class, int n1, int n2, int n3, int niter, int nthreads, double t, double mops, char* optype, int passed_verification, char* npbversion, char* compiletime, char* cc, char* clink, char* c_lib, char* c_inc, char* cflags, char* clinkflags, char* rand) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		char* evalue = ((char*)("1000"));
		printf(((char*)("\n\n %s Benchmark Completed\n")), name);
		printf(((char*)(" Class           =                        %c\n")), ((int)(class)));
		if(((n2 == 0) && (n3 == 0))) printf(((char*)(" Size            =             %12d\n")), n1) else printf(((char*)(" Size            =              %3dx%3dx%3d\n")), n1, n2, n3);
		printf(((char*)(" Iterations      =             %12d\n")), niter);
		printf(((char*)(" Threads         =             %12d\n")), nthreads);
		printf(((char*)(" Time in seconds =             %12.2f\n")), t);
		printf(((char*)(" Mop/s total     =             %12.2f\n")), mops);
		printf(((char*)(" Operation type  = %24s\n")), optype);
		if(((bool)(passed_verification))) printf(((char*)(" Verification    =               SUCCESSFUL\n")), ) else printf(((char*)(" Verification    =             UNSUCCESSFUL\n")), );
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


// start code fragment :: fundef_codefragment_main //
int main(void* _capture, int argc, char** argv) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		struct __insieme_userdefined_type_type_0 var_1 = ((struct __insieme_userdefined_type_type_0){el, start, elapsed, key_buff_ptr_global, passed_verification, key_array, key_buff1, key_buff2, partial_verify_vals, test_index_array, test_rank_array, <?>{ref.var(48427),ref.var(17148),ref.var(23627),ref.var(62548),ref.var(4431)}</?>, <?>{ref.var(0),ref.var(18),ref.var(346),ref.var(64917),ref.var(65463)}</?>, <?>{ref.var(357773),ref.var(934767),ref.var(875723),ref.var(898999),ref.var(404505)}</?>, <?>{ref.var(1249),ref.var(11698),ref.var(1039987),ref.var(1043896),ref.var(1048018)}</?>, <?>{ref.var(2112377),ref.var(662041),ref.var(5336171),ref.var(3642833),ref.var(4250760)}</?>, <?>{ref.var(104),ref.var(17523),ref.var(123928),ref.var(8288932),ref.var(8388264)}</?>, <?>{ref.var(41869),ref.var(812306),ref.var(5102857),ref.var(18232239),ref.var(26860214)}</?>, <?>{ref.var(33422937),ref.var(10244),ref.var(59149),ref.var(33135281),ref.var(99)}</?>, <?>{ref.var(44172927),ref.var(72999161),ref.var(74326391),ref.var(129606274),ref.var(21736814)}</?>, <?>{ref.var(61147),ref.var(882988),ref.var(266290),ref.var(133997595),ref.var(133525895)}</?>, 0, 0.0, 0.0, 0.0, 0.0, });
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
						(*(var_1.test_index_array))[((unsigned int)(var_11))] = (var_1.S_test_index_array)[((unsigned int)(var_11))];
						(*(var_1.test_rank_array))[((unsigned int)(var_11))] = (var_1.S_test_rank_array)[((unsigned int)(var_11))];
					}; break;
				case 'A':
					{
						(*(var_1.test_index_array))[((unsigned int)(var_11))] = (var_1.A_test_index_array)[((unsigned int)(var_11))];
						(*(var_1.test_rank_array))[((unsigned int)(var_11))] = (var_1.A_test_rank_array)[((unsigned int)(var_11))];
					}; break;
				case 'W':
					{
						(*(var_1.test_index_array))[((unsigned int)(var_11))] = (var_1.W_test_index_array)[((unsigned int)(var_11))];
						(*(var_1.test_rank_array))[((unsigned int)(var_11))] = (var_1.W_test_rank_array)[((unsigned int)(var_11))];
					}; break;
				case 'B':
					{
						(*(var_1.test_index_array))[((unsigned int)(var_11))] = (var_1.B_test_index_array)[((unsigned int)(var_11))];
						(*(var_1.test_rank_array))[((unsigned int)(var_11))] = (var_1.B_test_rank_array)[((unsigned int)(var_11))];
					}; break;
				case 'C':
					{
						(*(var_1.test_index_array))[((unsigned int)(var_11))] = (var_1.C_test_index_array)[((unsigned int)(var_11))];
						(*(var_1.test_rank_array))[((unsigned int)(var_11))] = (var_1.C_test_rank_array)[((unsigned int)(var_11))];
					}; break;
				};
			};
			i = 5;
		};
		{};
		printf(((char*)("\n\n NAS Parallel Benchmarks 2.3 OpenMP C version")), );
		printf(((char*)(" Size:  %d  (class %c)\n")), (1 << 16), 'S');
		printf(((char*)(" Iterations:   %d\n")), 10);
		call___insieme_funType_type_1(((struct __insieme_funType_type_1*)&((struct timer_clear_closure){&timer_clear, sizeof(struct timer_clear_closure),&var_1})), 0);
		call___insieme_funType_type_2(((struct __insieme_funType_type_2*)&((struct create_seq_closure){&create_seq, sizeof(struct create_seq_closure),&var_1})), 314159265.00, 1220703125.00);
		call___insieme_funType_type_1(((struct __insieme_funType_type_1*)&((struct rank_closure){&rank, sizeof(struct rank_closure),&var_1})), 1);
		(var_1.passed_verification) = 0;
		if(('S' != 'S')) printf(((char*)("\n   iteration\n")), );
		call___insieme_funType_type_1(((struct __insieme_funType_type_1*)&((struct timer_start_closure){&timer_start, sizeof(struct timer_start_closure),&var_1})), 0);
		{
			for(int var_91 = 1; var_91 < 10; var_91 += 1) {
				if(('S' != 'S')) printf(((char*)("        %d\n")), var_91);
				call___insieme_funType_type_1(((struct __insieme_funType_type_1*)&((struct rank_closure){&rank, sizeof(struct rank_closure),&var_1})), var_91);
			};
			iteration = 10;
		};
		call___insieme_funType_type_1(((struct __insieme_funType_type_1*)&((struct timer_stop_closure){&timer_stop, sizeof(struct timer_stop_closure),&var_1})), 0);
		timecounter = call___insieme_funType_type_19(((struct __insieme_funType_type_19*)&((struct timer_read_closure){&timer_read, sizeof(struct timer_read_closure),&var_1})), 0);
		call___insieme_funType_type_20(((struct __insieme_funType_type_20*)&((struct full_verify_closure){&full_verify, sizeof(struct full_verify_closure),&var_1})));
		if(((*(var_1.passed_verification)) != ((5 * 10) + 1))) (var_1.passed_verification) = 0;
		call___insieme_funType_type_26(((struct __insieme_funType_type_26*)&((struct c_print_results_closure){&c_print_results, sizeof(struct c_print_results_closure)})), ((char*)("IS")), ((char)('S')), (1 << 16), 0, 0, 10, nthreads, timecounter, ((((double)((10 * (1 << 16)))) / timecounter) / 1000000.), ((char*)("keys ranked")), (*(var_1.passed_verification)), ((char*)("2.3")), ((char*)("03 Nov 2010")), ((char*)("cc")), ((char*)("cc")), ((char*)("-lm")), ((char*)("-I../common")), ((char*)("-O3 ")), ((char*)("-lm")), ((char*)("randlc")));
	}
}


// start code fragment :: root-node //
