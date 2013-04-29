#include <iostream>
#include <list>

int main() {

	std::list<int> v;

	v.push_back(1);
	v.push_back(2);

	{
		std::list<int>::iterator it = v.begin();
		std::list<int>::iterator end = v.end();
		for(;it != end; it++) {
			std::cout << *it;
		}
	}
	
	{
		for(std::list<int>::iterator it = v.begin(), end = v.end(); it != end; it++) {
			std::cout << *it;
		}
	}
}
