#include <string>
#include <iostream>
#include <boost/utility.hpp>

//the problem is that the fu in
//the ctor call is a materialize
//expr and therefore we synthesize
//a method for this call. The method
//calls the copy ctor, but this
//object is noncopyable -> error
class Test:boost::noncopyable {
private:
	std::string name;
public:
	Test(std::string _name):name(_name) {};
	void print() {
		std::cout << "Name: " << name << std::endl;
	}
};

int main() {
	Test t("fu");
	t.print();
	return 0;	
}
