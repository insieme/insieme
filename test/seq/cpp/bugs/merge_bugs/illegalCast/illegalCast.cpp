#include <vector>
#include <iostream>

struct Container {
	int memberInt;
	std::vector<int> base;

	//----
	const int& test(int i) const {
		return (i==0) ? base[0] : base[1];
	}
	
	const int& test(int i) {
		return (i==0) ? base[0] : base[1];
	}

	//----
	const int& test1(int i) const {
		return base[1];
	}
	
	int testX(int i) const {
		return base[2];
	}
	
	int test2(int i) {
		return (i==0) ? test1(i) : base[3];
	}
	
//	const int& test3(int i) {
//		return (i==0) ? test1(i) : base[4];
//	}
	
	int test4(int i) {
		return (i==0) ? testX(i) : base[5];
	}
};

int main() {
	Container t;
	t.memberInt = -1;
	t.base.push_back(15); 
	t.base.push_back(16); 
	t.base.push_back(17); 
	t.base.push_back(18); 
	t.base.push_back(19); 
	t.base.push_back(20); 
	t.base.push_back(21); 
	t.base.push_back(22); 
	t.base.push_back(23); 


	{
		std::cout << t.test (0) << std::endl;
		std::cout << t.test1(0) << std::endl;
		std::cout << t.testX(0) << std::endl;
		std::cout << t.test2(0) << std::endl;
		//std::cout << t.test3(0) << std::endl;
		std::cout << t.test4(0) << std::endl;
	}
	std::cout << " == " <<std::endl;
	{
		std::cout << t.test (1) << std::endl;
		std::cout << t.test1(1) << std::endl;
		std::cout << t.testX(1) << std::endl;
		std::cout << t.test2(1) << std::endl;
		//std::cout << t.test3(1) << std::endl;
		std::cout << t.test4(1) << std::endl;
	}
	std::cout << " == " <<std::endl;
	{
		int i;
		i= t.test (0); std::cout << i << std::endl;
		i= t.test1(0); std::cout << i << std::endl;
		i= t.testX(0); std::cout << i << std::endl;
		i= t.test2(0); std::cout << i << std::endl;
		//i= t.test3(0); std::cout << i << std::endl;
		i= t.test4(0); std::cout << i << std::endl;
	}
	std::cout << " == " <<std::endl;
	{
		int i;
		i= t.test (1); std::cout << i << std::endl;
		i= t.test1(1); std::cout << i << std::endl;
		i= t.testX(1); std::cout << i << std::endl;
		i= t.test2(1); std::cout << i << std::endl;
		//i= t.test3(1); std::cout << i << std::endl;
		i= t.test4(1); std::cout << i << std::endl;
	}

	return 0;
}

