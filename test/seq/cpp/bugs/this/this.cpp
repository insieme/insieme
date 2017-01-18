

#include <iostream>


#define ERROR_HANDLER(x, y) __check_error((x), (y), __FILE__, __LINE__)

inline void __check_error(bool condition, const std::string errorString, const char *file, const int line) {
if (!condition) {
std::cerr << "ERROR: "
<< errorString << std::endl
<< "in " << file << " line: " << line << std::endl;
}
}



class Obj{
	int a;
	int b;

	public:
	Obj()
		: a(1), b(1)
	{}

	void method(){
		ERROR_HANDLER(( a && !b), "here the error");
	}
};


int main(){

	Obj a;

	a.method();


	return 0;
}
