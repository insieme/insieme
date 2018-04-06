
#include <iostream>
#include <vector>


struct Test : public std::vector<int> {

	// the problem here is the call to the parent contstructor
	Test() : std::vector<int>() {}
};

int main() {

	Test t;
	t.push_back(5);
	t.push_back(0);
	std::cout << t.front() << std::endl;

	return t.back();
}
