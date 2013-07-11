
// externs
extern int globalInOther;
extern int pureExtern;

// globals
int var;
int array[100];
int*  pointer;

// in class
class Obj{
	static int member;
};

int f();

int main(){
	static int var;   // alias
	static int diffName;
	int local = f();
}
