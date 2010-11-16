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

// start code fragment :: lambda_struct___insieme_funType_type_0 //
struct __insieme_funType_type_0 { 
    int(*fun)(void*,int*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_0 //
int call___insieme_funType_type_0(struct __insieme_funType_type_0* lambda,int* p1) { return lambda->fun(lambda,p1); }

// start code fragment :: fun_type_utilities___insieme_funType_type_0 //

// start code fragment :: lambda_f //
struct f_closure { 
    int(*fun)(void*,int*);
    const size_t size;
};

// start code fragment :: lambda___insieme_lambda_fun_1 //
struct __insieme_lambda_fun_1_closure { 
    int(*fun)(void*,int*);
    const size_t size;
};

// start code fragment :: fundef_codefragment___insieme_lambda_fun_1 //
int __insieme_lambda_fun_1(void* _capture, int* var_6) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int var_5 = var_6[0];
		var_6[0] = var_6[0]+((int)(1));
		return var_5;;
	}
}


// start code fragment :: fundef_codefragment_f //
int f(void* _capture, int* a) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		return call___insieme_funType_type_0(((struct __insieme_funType_type_0*)&((struct __insieme_lambda_fun_1_closure){&__insieme_lambda_fun_1, sizeof(struct __insieme_lambda_fun_1_closure)})), a);;
	}
}


// start code fragment :: lambda_f //
struct f_closure { 
    int(*fun)(void*,int*);
    const size_t size;
};

// start code fragment :: fundef_codefragment_printf //
int printf(char*, ...);

// start code fragment :: fundef_codefragment_main //
int main(void* _capture, int argc, char** argv) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		int var_7 = argc;
		{
			int a = 0;
			call___insieme_funType_type_0(((struct __insieme_funType_type_0*)&((struct f_closure){&f, sizeof(struct f_closure)})), <?>{v3}</?>);
			call___insieme_funType_type_0(((struct __insieme_funType_type_0*)&((struct f_closure){&f, sizeof(struct f_closure)})), <?>{v7}</?>);
			printf(((char*)("%d\n")), var_7);var_7 = a;
			printf(((char*)("%d\n")), var_7);
		};
	}
}


// start code fragment :: root-node //
