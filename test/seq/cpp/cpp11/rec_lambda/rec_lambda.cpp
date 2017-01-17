#include <iostream>
#include <functional>

int rec_lambda_refcap(int n) {
	std::function<int (int)> x = [&](int y)->int {
		std::cout << "lambda call: " << y << std::endl;
		if(y==n) return y;
		return x(y-1);
	};
	return x(3);
}

int main() {
        std::cout << "recursive lambda refcap: " << rec_lambda_refcap(0) << std::endl;
}