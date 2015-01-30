#include <iostream>


int main (){

    //no capture, no arguments
    {
        auto lambda = []() {
            std::cout << " no capture, no argument." << std::endl;
            return;
        };

        auto lambdaRetType = []()->void {
            std::cout << " no capture, no argument." << std::endl;
            return;
        };

        lambda();
        lambdaRetType();
    }

    //no capture, arguments
    {
        auto lambda = [](int x) {
            std::cout << " no capture, argument: " << x << std::endl;
            return;
        };

        auto lambdaRetType = [](int x)->void {
            std::cout << " no capture, argument: " << x << std::endl;
            return;
        };

        lambda(1);
        lambdaRetType(1);
    }

    //capture, no arguments
    {
        int x=1;
        auto lambda = [&]() {
            std::cout << " capture, no argument: " << x << std::endl;
            return;
        };

        auto lambdaRetType = [&]()->void {
            std::cout << " capture, no argument: " << x << std::endl;
            return;
        };

        lambda();
        lambdaRetType();
    }

    //capture, arguments
    {
        int x=1;
        auto lambda = [&](int y) {
            std::cout << " capture, argument: " << (x+y) << std::endl;
            return;
        };

        auto lambdaRetType = [&](int y)->int {
            std::cout << " capture, argument: " << (x+y) << std::endl;
            return (x+y);
        };

        lambda(2);
        lambdaRetType(2);
    }

	// value capture
	{
		int x=0;
		std::cout << " x: " << x << std::endl;

		auto lambda = [x](int i) {
			std::cout << " output: " << i << std::endl;
			std::cout << " captured x: " << x << std::endl;
		};

		lambda(1);
		std::cout << " x: " << x << std::endl;
		lambda(2);
		std::cout << " x: " << x << std::endl;
	}

	// mutable
	{
		int x=0;

		auto lambda = [x](int i) mutable {
			std::cout << " output: " << i << std::endl;
			std::cout << " captured x: " << x << std::endl;
			x ++;
			std::cout << " captured x: " << x << std::endl;
		};

		lambda(1);
		std::cout << " x: " << x << std::endl;
		lambda(2);
		std::cout << " x: " << x << std::endl;
	}

	// ref capture
	{
		int x=0;

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
		int x=0;
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
