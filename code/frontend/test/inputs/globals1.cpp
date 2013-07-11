
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

	Obj(){
		static int ctorGlobal;
	}
};


// class object with static inside
static struct Obj2{
	static int a;
	int b;
} instance;

// initialize static
int Obj2::a = 6;

int f();

int main(){
	static int var;   // alias
	static int diffName;
	int local = f();

	Obj2::a++;
}
