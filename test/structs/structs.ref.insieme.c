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

// start code fragment :: type_declaration_pStruct //
struct pStruct { 
    char(*name)[30];
    unsigned int age;
};

// start code fragment :: lambda_struct___insieme_funType_type_0 //
struct __insieme_funType_type_0 { 
    struct pStruct(*fun)(void*);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_0 //
struct pStruct call___insieme_funType_type_0(struct __insieme_funType_type_0* lambda) { return lambda->fun(lambda); }

// start code fragment :: fun_type_utilities___insieme_funType_type_0 //

// start code fragment :: lambda_getPerson //
struct getPerson_closure { 
    struct pStruct(*fun)(void*);
    const size_t size;
};

// start code fragment :: fundef_codefragment_vector.initUniform //
<?>'a</?>[a] vector.initUniform(<?>'a</?>);

// start code fragment :: fundef_codefragment_strcpy //
char* strcpy(char*, char*);

// start code fragment :: fundef_codefragment_getPerson //
struct pStruct getPerson(void* _capture) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		struct pStruct res = ((struct pStruct){vector.initUniform('), 0, });
		strcpy((res.name), ((char*)("John Doe")));
		(res.age) = ((unsigned int)(101));
	}
}


// start code fragment :: lambda_struct___insieme_funType_type_1 //
struct __insieme_funType_type_1 { 
    int(*fun)(void*,struct pStruct);
    const size_t size;
};

// start code fragment :: call__insieme_funType_type_1 //
int call___insieme_funType_type_1(struct __insieme_funType_type_1* lambda,struct pStruct p1) { return lambda->fun(lambda,p1); }

// start code fragment :: fun_type_utilities___insieme_funType_type_1 //

// start code fragment :: lambda_isTeenager //
struct isTeenager_closure { 
    int(*fun)(void*,struct pStruct);
    const size_t size;
};

// start code fragment :: fundef_codefragment_isTeenager //
int isTeenager(void* _capture, struct pStruct person) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		return (((person.age) >= ((unsigned int)(10))) && ((person.age) < ((unsigned int)(20))));;
	}
}


// start code fragment :: fundef_codefragment_main //
int main(void* _capture, int argc, char** argv) {
	// --------- Captured Stuff - Begin -------------
	// --------- Captured Stuff -  End  -------------
	{
		struct pStruct mrX = call___insieme_funType_type_0(((struct __insieme_funType_type_0*)&((struct getPerson_closure){&getPerson, sizeof(struct getPerson_closure)})));
		return call___insieme_funType_type_1(((struct __insieme_funType_type_1*)&((struct isTeenager_closure){&isTeenager, sizeof(struct isTeenager_closure)})), mrX);;
	}
}


// start code fragment :: root-node //
