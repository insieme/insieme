#include <assert.h>


struct obj{
	int a;
	float b;
};




int main(int argc, char ** argv){

	struct obj o;
	int a = 34;

	o.a = 1;
	o.b = 3.0;



	assert (a == 34    && "int a is not 34");
	assert (o.a == 1   && "o.a is not 1");
	assert (o.b == 3.0 && "o.b is not 3.0");




	return 0;
}
