#include <vector>

int main() {
	std::vector<int> v;
	v.push_back(7u);	
	return v[0] != 7;
}
