

#include <stdio.h>



class Papa{

	public:
		void f(int val){
			printf("I am your father! [%d]\n", val);
		}
};

class Son : public Papa{

};

int main(){

//	printf("++++++++++++++++++++++++");
//	Papa a;
//	a.f(1);
//	Papa* b = &a;
//	b->f(2);
//	printf("++++++++++++++++++++++++");
	Son c;
	c.f(3);
//	printf("++++++++++++++++++++++++");
	Son* d = &c;
	d->f(4);
//	printf("++++++++++++++++++++++++");
//	Papa* e;
//	e = &c;
//	e->f(5);

	return 0;
}
