#include <iostream>
#include <vector>

int main() {

	std::vector<int> v;
	
	v.push_back(1);
	v.push_back(2);
	v.push_back(3);
	
	{
		std::vector<int>::iterator it = v.begin();
		for(size_t i = 0; i < std::distance(it, v.end()); i++) {
			int curr = it[i];
			{
				std::cout << curr;
			}
		}
	}
	
	{
		for(size_t i = 0; i < std::distance(v.begin(), v.end()); i++) {
			int curr = (v.begin())[i];
			{
				std::cout << curr;
			}
		}
	}
}
