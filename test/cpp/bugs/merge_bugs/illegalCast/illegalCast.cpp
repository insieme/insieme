#include <vector>
#include <iostream>

class Triangle {
	int memberInt;
public:
	std::vector<int> base;

	//----
	const int& test(int i) const {
		return (i==0) ? base[0] : base[1];
	}
	
	/*
	const int& test(int i) {
		return (i==0) ? base[0] : base[1];
	}
	*/

	//----
	const int& test1(int i) const {
		return base[0];
	}
	
	int testX(int i) const {
		return base[0];
	}
	
	int test2(int i) {
		return (i==0) ? test1(i):base[0];
	}
	
	const int& test3(int i) {
		return (i==0) ? test1(i):base[0];
	}
	
	int test4(int i) {
		return (i==0) ? testX(i):base[0];
	}
	
	const int& test5(int i) {
		return (i==0) ? testX(i):base[0];
	}
};

int main() {
	Triangle t;
	t.base.push_back(0); //don't worry about this one, doesn't produce any errors
	t.base.push_back(1); //don't worry about this one, doesn't produce any errors
	{
		std::cout << t.test(1) << std::endl;
		std::cout << t.test1(1) << std::endl;
		std::cout << t.testX(1) << std::endl;
		std::cout << t.test2(1) << std::endl;
		std::cout << t.test3(1) << std::endl;
		std::cout << t.test4(1) << std::endl;
		std::cout << t.test5(1) << std::endl;
	}
	{
		int i;
		i= t.test(1);
		std::cout << i << std::endl;
		i= t.test1(1);
		std::cout << i << std::endl;
		i= t.testX(1);
		std::cout << i << std::endl;
		i= t.test2(1);
		std::cout << i << std::endl;
		i= t.test3(1);
		std::cout << i << std::endl;
		i= t.test4(1);
		std::cout << i << std::endl;
		i= t.test5(1);
		std::cout << i << std::endl;
	}

	return 0;
}

/*************OUTPUT******************/
/*
Errors:
[ERROR:   [00002] - TYPE / INVALID_ARGUMENT_TYPE @ (0-0-2-0-1-2-0-0-0-0 / ValueAnnotationKey(N7insieme4core13ClassMetaInfoE):0-2-0-1-2-0-0 - /home/stefanm/illCast.cpp@6:2-8:2) - MSG: Invalid argument type(s) - expected: (ref<'a>)
actual: (int<4>)
- function type: ((ref<'a>)->struct<_const_cpp_ref:ref<'a>>)

ERROR:   [00015] - TYPE / ILLEGAL_CAST @ (0-0-2-0-1-2-0-0-0-0 / ValueAnnotationKey(N7insieme4core13ClassMetaInfoE):0-2-0-1-2-0-0-2-3-2-1-2-0-1-2-0-0 - /home/stefanm/illCast.cpp@7:10-7:35) - 
MSG: Casting between incompatible types AP(struct<_const_cpp_ref:ref<int<4>>>) and AP(int<4>)

ERROR:   [00015] - TYPE / ILLEGAL_CAST @ (0-0-2-0-1-2-0-0-0-0 / ValueAnnotationKey(N7insieme4core13ClassMetaInfoE):0-2-0-1-2-0-0-2-4-2-1-2-0-1-2-0-0 - /home/stefanm/illCast.cpp@7:10-7:35) - 
MSG: Casting between incompatible types AP(struct<_const_cpp_ref:ref<int<4>>>) and AP(int<4>)

ERROR:   [00002] - TYPE / INVALID_ARGUMENT_TYPE @ (0-0-2-0-1-2-1-1-2-0-1-2-0-0 - /home/stefanm/illCast.cpp@6:2-8:2) - MSG: Invalid argument type(s) - 
expected: (ref<'a>)
actual: (int<4>)
- function type: ((ref<'a>)->struct<_const_cpp_ref:ref<'a>>)

ERROR:   [00015] - TYPE / ILLEGAL_CAST @ (0-0-2-0-1-2-1-1-2-0-1-2-0-0-2-3-2-1-2-0-1-2-0-0 - /home/stefanm/illCast.cpp@7:10-7:35) - 
MSG: Casting between incompatible types AP(struct<_const_cpp_ref:ref<int<4>>>) and AP(int<4>)

ERROR:   [00015] - TYPE / ILLEGAL_CAST @ (0-0-2-0-1-2-1-1-2-0-1-2-0-0-2-4-2-1-2-0-1-2-0-0 - /home/stefanm/illCast.cpp@7:10-7:35) - 
MSG: Casting between incompatible types AP(struct<_const_cpp_ref:ref<int<4>>>) and AP(int<4>)
*/
