// --- Generated Inspire Code ---
#include <stddef.h>
#define bool int
#define true 1
#define false 0
// --- Entry Point ---

// start code fragment :: type_declaration_pStruct //
struct pStruct { 
    char name[30];
    unsigned int age;
};

// start code fragment :: Prototype for external function: vector.initUniform //
<?>'elem</?>[a] vector.initUniform(<?>'elem</?>);

// start code fragment :: Prototype for external function: strcpy //
char* strcpy(char*, char*);

// start code fragment :: Definition of getPerson //
struct pStruct* getPerson() {
	{
		struct pStruct res = ((struct pStruct){vector.initUniform(')0});
		strcpy((res.name), ((char*)("John Doe")));
		((res.age) = ((unsigned int)(101)));
	}
}


// start code fragment :: Definition of isTeenager //
int isTeenager(struct pStruct* person) {
	{
		return (((person.age)>=((unsigned int)(10))) && ((person.age)<((unsigned int)(20))));;
	}
}


// start code fragment :: Definition of main //
int main(int argc, char** argv) {
	{
		struct pStruct mrX = getPerson();
		return isTeenager(mrX);;
	}
}

