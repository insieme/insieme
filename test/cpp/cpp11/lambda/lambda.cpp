#include <iostream>


int main (){

	//// value capture
	//{
	//	int x;

	//	auto lambda = [x](int i) {
	//		std::cout << " output: " << i << std::endl;
	//		std::cout << " captured x: " << x << std::endl;
	//	};

	//	lambda(1);
	//	std::cout << " x: " << x << std::endl;
	//	lambda(2);
	//	std::cout << " x: " << x << std::endl;
	//}

	//// mutable
	//{
	//	int x;

	//	auto lambda = [x](int i) mutable {
	//		std::cout << " output: " << i << std::endl;
	//		std::cout << " captured x: " << x << std::endl;
	//		x ++;
	//		std::cout << " captured x: " << x << std::endl;
	//	};

	//	lambda(1);
	//	std::cout << " x: " << x << std::endl;
	//	lambda(2);
	//	std::cout << " x: " << x << std::endl;
	//}

	// ref capture
	{
		int x;

		auto lambda = [&x](int i) {
			std::cout << " output: " << i << std::endl;
			std::cout << " captured x: " << x << std::endl;
			x=i;
			std::cout << " captured x: " << x << std::endl;
		};

		lambda(1);
		std::cout << " x: " << x << std::endl;
		lambda(2);
		std::cout << " x: " << x << std::endl;
	}

	// complete capture
	{
		int x;
		auto lambda = [&](int i) {
			std::cout << " output: " << i << std::endl;
			std::cout << " captured x: " << x << std::endl;
			x=i;
			std::cout << " captured x: " << x << std::endl;
		};

		lambda(1);
		std::cout << " x: " << x << std::endl;
		lambda(2);
		std::cout << " x: " << x << std::endl;
	}

	return 0;
}
