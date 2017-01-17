

#include <iostream>

int main(){

	try{
	try{
		throw 3;
	}catch (const int& a){
		throw;
	}
	}catch (const int& b){
		std::cout << "hey: " << b << std::endl;
	}
}
