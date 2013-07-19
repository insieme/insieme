

extern int ext;
int  glob;


typedef struct anon myStruct;
myStruct* ptr;                     // non extern var, but with non defined type inside

int withInit = 4;
static int myStatic= 1;

int *f(){
	return &ext;
}

void plus(int val){
	withInit+= val;
}


int main(){
	*f() += 4;
	plus(myStatic);
	return 0;
}


