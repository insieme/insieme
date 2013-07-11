


int globalInOther = 0;


int f(){
	static int a;  

	a++;
	globalInOther++;
	int local;
	return local;
}
