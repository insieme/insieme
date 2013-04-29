#include <iostream>
#include <vector>

int main() {

	std::vector<int> v;
	
	v.push_back(1);
	v.push_back(2);

	{
		std::vector<int>::iterator it = v.begin();
		std::vector<int>::iterator end = v.end();
		for(;it != end; it++) {
			std::cout << *it;
		}
	}
	
	{
		for(std::vector<int>::iterator it = v.begin(), end = v.end(); it != end; it++) {
			std::cout << *it;
		}
	}
}
