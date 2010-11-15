// --- Generated Inspire Code ---
#define bool int
#define true 1
#define false 0
// --- Entry Point ---

// start code fragment :: type_declaration_pStruct //
struct pStruct { 
    char(*name)[30];
    unsigned int age;
};

// start code fragment :: fundef_codefragment_vector.initUniform //
<?>'a</?>[a] vector.initUniform(<?>'a</?>);

// start code fragment :: fundef_codefragment_strcpy //
char* strcpy(char*, char*);

// start code fragment :: fundef_codefragment_getPerson //
struct pStruct getPerson(){
	struct pStruct res = ((struct pStruct){vector.initUniform('), 0, });
	strcpy((res.name), ((char*)("John Doe")));
	(res.age) = ((unsigned int)(101));
}

// start code fragment :: fundef_codefragment_isTeenager //
int isTeenager(struct pStruct person){
	return (((person.age) >= ((unsigned int)(10))) && ((person.age) < ((unsigned int)(20))));;
}

// start code fragment :: fundef_codefragment_main //
int main(int argc, char** argv){
	struct pStruct mrX = getPerson();
	return isTeenager(mrX);;
}

// start code fragment :: unnamed //
