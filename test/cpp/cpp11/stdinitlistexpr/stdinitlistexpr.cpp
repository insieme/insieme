#include <vector>
#include <string>
#include <iostream>

int getNum() {
	return 0;
}

int* getNumPointer(int*& n) {
	return n;
}

int main() {
	int * n = new int(3);
	int * m = new int(4);
	//vector init with pointers
	const std::vector<int*> trueBlaPtr { getNumPointer(n), m };
	//vector init with string values
	std::string str = "false";
	const std::vector<std::string> trueSynonyms { "true", str };
	//vector init with function calls and int
	const std::vector<int> trueBla { getNum(), 2, 4, *getNumPointer(m) };

	for(int i=0; i<trueBlaPtr.size(); ++i)
		std::cout << *trueBlaPtr[i] << std::endl;
	for(int i=0; i<trueSynonyms.size(); ++i)
		std::cout << trueSynonyms[i] << std::endl;
	for(int i=0; i<trueBla.size(); ++i)
		std::cout << trueBla[i] << std::endl;

	delete n;
	return 0;
}
