// read a file into memory
#include <iostream>     // std::cout
#include <fstream>      // std::ifstream

int main (int argc, char** argv) {
  std::cout << "STL - Read file test" << std::endl;
  std::ifstream is(argv[1], std::ifstream::in);
    
  if (is) {
    // get length of file:
    is.seekg (0, is.end);
    int length = is.tellg();
    is.seekg (0, is.beg);
	std::cout << "Length: " << length << std::endl;

    // allocate memory:
    char * buffer = new char [length];

    // read data as a block:
    is.read (buffer,length);

    is.close();

    // print content:
    std::cout.write (buffer,length).flush();

    delete[] buffer;
  } else {
	std::cout << "file not opened" << std::endl;
  }

  return 0;
}
