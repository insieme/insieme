// --- Generated Inspire Code ---
#include <stddef.h>
#define bool int
#define true 1
#define false 0
// --- Entry Point ---

// start code fragment :: type_declaration_Person //
struct Person { 
    char name[30];
    unsigned int age;
};

// start code fragment :: Prototype for external function: vector.initUniform //
<?>'elem</?>[a] vector.initUniform(<?>'elem</?>);

// start code fragment :: Prototype for external function: strcpy //
char* strcpy(char*, char*);

// start code fragment :: Definition of getPerson //
struct Person* getPerson() {
	{
		struct Person res = ((struct Person){vector.initUniform(')0});
		strcpy((res.name), ((char*)("John Doe")));
		((res.age) = ((unsigned int)(101)));
	}
}


// start code fragment :: Definition of isTeenager //
int isTeenager(struct Person* person) {
	{
		return (((person.age)>=((unsigned int)(10))) && ((person.age)<((unsigned int)(20))));;
	}
}


// start code fragment :: Definition of main //
int main(int argc, char** argv) {
	{
		struct Person mrX = getPerson();
		return isTeenager(mrX);;
	}
}

