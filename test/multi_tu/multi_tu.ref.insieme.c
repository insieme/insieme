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
    int(*fun)(void*,int,int);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_0 //
int call___insieme_funType_type_0(struct __insieme_funType_type_0* lambda,int p1,int p2) { return lambda->fun(lambda,p1,p2); }

// start code fragment :: fun_type_utilities___insieme_funType_type_0 //

// start code fragment :: lambda_f //
struct f_closure { 
    int(*fun)(void*,int,int);
    const size_t size;
};

// start code fragment :: lambda_struct___insieme_funType_type_1 //
struct __insieme_funType_type_1 { 
    int(*fun)(void*,int);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_1 //
int call___insieme_funType_type_1(struct __insieme_funType_type_1* lambda,int p1) { return lambda->fun(lambda,p1); }

// start code fragment :: fun_type_utilities___insieme_funType_type_1 //

// start code fragment :: lambda_g //
struct g_closure { 
    int(*fun)(void*,int);
    const size_t size;
};

// start code fragment :: fundef_codefragment_g //
int g(void* _capture, int a) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		return (a - 1);;
	}
}


// start code fragment :: fundef_codefragment_f //
int f(void* _capture, int a, int b) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		return (call___insieme_funType_type_1(((struct __insieme_funType_type_1*)&((struct g_closure){&g, sizeof(struct g_closure)})), (a + b)) + 1000);;
	}
}


// start code fragment :: fundef_codefragment_main //
int main(void* _capture, int argc, char** argv) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		return call___insieme_funType_type_0(((struct __insieme_funType_type_0*)&((struct f_closure){&f, sizeof(struct f_closure)})), argc, argc);;
	}
}


// start code fragment :: root-node //
