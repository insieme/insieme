// read a file into memory
#include <iostream>     // std::cout
#include <fstream>      // std::ifstream

int main () {
	std::ifstream is ("text.txt", std::ifstream::binary);
	if (is) {
		// get length of file:
		is.seekg (0, is.end);
		int length = is.tellg();
		is.seekg (0, is.beg);

		// allocate memory:
		char * buffer = new char [length];

		// read data as a block:
		is.read (buffer,length);

		is.close();

		// print content:
		std::cout.write (buffer,length);

		delete[] buffer;
	}

	return 0;
}
