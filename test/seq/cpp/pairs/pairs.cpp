#include <utility>
#include <iostream>

using std::pair;

int main() {

	pair<int,bool> x = std::make_pair(12,true);

	std::cout << "<" << x.first << "," << x.second << ">\n";

	return 0;
}
