#include <chrono>
#include <iostream>

int main(int argc, char** argv) {
	int r = 1;
	auto start = std::chrono::high_resolution_clock::now();
	for(int i=0; i<argc; ++i) r *= 5;
	auto end = std::chrono::high_resolution_clock::now();
	std::cout << "R: " << r << std::endl;
	std::cout << "C: " << std::chrono::duration_cast<std::chrono::seconds>(end-start).count() << std::endl; 
	return (r != 0) && (std::chrono::duration_cast<std::chrono::seconds>(end-start).count() == 0) ? 0 : 1;
}



