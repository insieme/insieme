



class Obj{

	int val;

public:
	Obj(int b,int a =1){
		val = a;
	}

	int f(){
		return val;
	}

};


int main(int argc, char **argv){

	Obj *a = new Obj(9);
	Obj b(3);

	a->f();
	b.f();

	return 0;
}
