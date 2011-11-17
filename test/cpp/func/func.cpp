#include <iostream>

int calculate(int a) { return a; }
double calculate(double a) { return a; }

int calcDefArg(int a, int b=10, int c=100) { return a+b+c; };

int main() {
	// test function overloading
	std::cout << "1 == " << calculate(1); 	// int
	std::cout << "2.2 == " << calculate(2.2);	// double

	// test default arguments
	std::cout << "100+0+0" << calcDefArg(100,0,0);	//=100
	std::cout << "1+100+100 == " << calcDefArg(1,100);		//=201
	std::cout << "100+10+100 == " << calcDefArg(100);		//=210

	return 0;
}
