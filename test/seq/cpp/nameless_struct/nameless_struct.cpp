#include <iostream>

struct X {
   struct {
      int x;
      int y;
   } point;
};

int main() {
	X x;

	std::cout << x.point.x << std::endl;
	std::cout << x.point.y << std::endl;

	x.point.x = 1;
	x.point.y = 10;

	std::cout << x.point.x << std::endl;
	std::cout << x.point.y << std::endl;
}
