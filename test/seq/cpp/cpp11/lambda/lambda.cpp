#include <iostream>

//don't worry about the strange
//if checks in the following methods.
//those were used for debugging issues

int sum(int a, int b) {
    //capture it by ref
    auto sumr = [&]() {
        if(a==0)
            return b;
        if(b==0)
            return a;
        return a+b;
    };
    //capture it by val
    auto sumv = [=]() {
        if(a==0)
            return b;
        if(b==0)
            return a;
        return a+b;
    };
    return sumr()+sumv();
}

int sum_ref(int& a, int &b) {
    //capture it by ref
    auto sumr = [&]() {
        if(a==0)
            return b;
        if(b==0)
            return a;
        return a+b;
    };
    //capture it by val
    auto sumv = [=]() {
        if(a==0)
            return b;
        if(b==0)
            return a;
        return a+b;
    };
    return sumr()+sumv();
}

int sum_ptr(int* a, int* b) {
    //capture it by ref
    auto sumr = [&]() {
        if(*a==0)
            return *b;
        if(*b==0)
            return *a;
        return *a+*b;
    };
    //capture it by val
    auto sumv = [=]() {
        if(*a==0)
            return *b;
        if(*b==0)
            return *a;
        return *a+*b;
    };
    return sumr()+sumv();
}

//recursive lambda that captures itself
//by value is not allowed...

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

    //take arguments from function
    {
        std::cout << "arguments from function: " << sum(1,2) << std::endl;
    }

    //take arguments from function (ref)
    {
        int a=10;
        int b=20;
        std::cout << "arguments from function ref: " << sum_ref(a,b) << std::endl;
    }

    //take arguments from function (ptr)
    {
        int a=10;
        int b=20;
        std::cout << "arguments from function ref: " << sum_ptr(&a,&b) << std::endl;
    }

    //create two lambdas with different return types
    //should create nearly the same struct type
    //but with different names...
    {
        auto l = []() { return 1; };
        auto k = []() { return 1.0; };
        std::cout << "k: " << k() << std::endl;
        std::cout << "l: " << l() << std::endl;
    }
	return 0;
}
