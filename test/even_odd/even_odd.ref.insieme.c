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

// start code fragment :: fundef_codefragment_printf //
int printf(char*, ...);

// start code fragment :: lambda_struct___insieme_funType_type_0 //
struct __insieme_funType_type_0 { 
    char*(*fun)(void*,int*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_0 //
char* call___insieme_funType_type_0(struct __insieme_funType_type_0* lambda,int* p1) { return lambda->fun(lambda,p1); }

// start code fragment :: fun_type_utilities___insieme_funType_type_0 //

// start code fragment :: lambda___insieme_lambda_fun_1 //
struct __insieme_lambda_fun_1_closure { 
    char*(*fun)(void*,int*);
    const size_t size;
};

// start code fragment :: lambda_struct___insieme_funType_type_2 //
struct __insieme_funType_type_2 { 
    int(*fun)(void*,unsigned int);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_2 //
int call___insieme_funType_type_2(struct __insieme_funType_type_2* lambda,unsigned int p1) { return lambda->fun(lambda,p1); }

// start code fragment :: fun_type_utilities___insieme_funType_type_2 //

// start code fragment :: lambda_struct___insieme_funType_type_3 //
struct __insieme_funType_type_3 { 
    int(*fun)(void*,struct __insieme_funType_type_2*,unsigned int);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_3 //
int call___insieme_funType_type_3(struct __insieme_funType_type_3* lambda,struct __insieme_funType_type_2* p1,unsigned int p2) { return lambda->fun(lambda,p1,p2); }

// start code fragment :: fun_type_utilities___insieme_funType_type_3 //

// start code fragment :: lambda___insieme_lambda_fun_4 //
struct __insieme_lambda_fun_4_closure { 
    int(*fun)(void*,struct __insieme_funType_type_2*,unsigned int);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_4 //
int __insieme_lambda_fun_4(void* _capture, struct __insieme_funType_type_2* var_10, unsigned int var_11) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		if((var_11 == ((unsigned int)(0)))) return 0; else return var_10->fun(var_10, (var_11 - ((unsigned int)(1))));;
	}
}


// start code fragment :: fundecl_codefragment_even //
int even(unsigned int x);

// start code fragment :: fundecl_codefragment_odd //
int odd(unsigned int x);

// start code fragment :: fundef_codefragment_odd //
int odd(void* _capture, unsigned int x) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		return call___insieme_funType_type_3(((struct __insieme_funType_type_3*)&((struct __insieme_lambda_fun_4_closure){&__insieme_lambda_fun_4, sizeof(struct __insieme_lambda_fun_4_closure)})), , x);;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_5 //
struct __insieme_lambda_fun_5_closure { 
    int(*fun)(void*,struct __insieme_funType_type_2*,unsigned int);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_5 //
int __insieme_lambda_fun_5(void* _capture, struct __insieme_funType_type_2* var_7, unsigned int var_8) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		if((var_8 == ((unsigned int)(0)))) return 1; else return var_7->fun(var_7, (var_8 - ((unsigned int)(1))));;
	}
}


// start code fragment :: fundef_codefragment_even //
int even(void* _capture, unsigned int x) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		return call___insieme_funType_type_3(((struct __insieme_funType_type_3*)&((struct __insieme_lambda_fun_5_closure){&__insieme_lambda_fun_5, sizeof(struct __insieme_lambda_fun_5_closure)})), , x);;
	}
}


// start code fragment :: fundef_codefragment___insieme_lambda_fun_1 //
char* __insieme_lambda_fun_1(void* _capture, int* var_12) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		if(((bool)(call___insieme_funType_type_2(, ((unsigned int)((*var_12))))))) return ((char*)("true")); else return ((char*)("false"));;
	}
}


// start code fragment :: lambda___insieme_lambda_fun_6 //
struct __insieme_lambda_fun_6_closure { 
    char*(*fun)(void*,int*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_6 //
char* __insieme_lambda_fun_6(void* _capture, int* var_13) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		if(((bool)(call___insieme_funType_type_2(, ((unsigned int)((*var_13))))))) return ((char*)("true")); else return ((char*)("false"));;
	}
}


// start code fragment :: fundef_codefragment_main //
int main(void* _capture, int argc, char** argv) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int x = 10;
		printf(((char*)("x=%d\n")), x);
		printf(((char*)("even(x)=%s\n")), call___insieme_funType_type_0(((struct __insieme_funType_type_0*)&((struct __insieme_lambda_fun_1_closure){&__insieme_lambda_fun_1, sizeof(struct __insieme_lambda_fun_1_closure)})), &x));
		printf(((char*)("odd(x)=%s\n")), call___insieme_funType_type_0(((struct __insieme_funType_type_0*)&((struct __insieme_lambda_fun_6_closure){&__insieme_lambda_fun_6, sizeof(struct __insieme_lambda_fun_6_closure)})), &x));
		return 0;;
	}
}


// start code fragment :: root-node //
