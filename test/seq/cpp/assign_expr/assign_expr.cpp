#include <iostream>



int f(){
	static int v = 0;
	return v++;
}
int g(int x){
	return x+1;
}

typedef struct {
	int val;
} S;


int h(S& a){
	return a.val;
}


S &j(S& x){
	return (x = (S){5});
}


int main (){

	{
		int a;
		while ((a = f() ) < 6){
			std::cout << a << "\n";
		}
	}

	{
		int x=0;
		int y=10;
		int z=10;
		while ((x = f() ) < (y = g(z))){
			std::cout << x << "\n";
		}
	}
	{
		S a = {0};
		a.val=5;
		a.val=g(a.val = 6);
		std::cout << a.val << "\n";
	}
	{
		S a = {0};
		a.val=5;
		std::cout << h(a = (S){0}) << std::endl;
	}
	{
		S a = {0};
		std::cout << j(a).val << "\n";
	}

	return 0;
}
