#include <cstddef>

int main (){
	std::size_t s = __alignof__ (int);
	return 0;
}
