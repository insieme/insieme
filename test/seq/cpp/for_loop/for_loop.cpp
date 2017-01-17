#include <cstddef>


std::size_t f(){
	return 0;
}

int main (){

	for (std::size_t i = f(); i >0; i++){
	}

	return 0;
}
