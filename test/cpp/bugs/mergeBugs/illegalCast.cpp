#include <vector>

class Triangle {
public:
	std::vector<int> base;
	const int& test(int i) const {
		return (i==0) ? base[0] : base[1];
	}

};

int main() {
	Triangle t;
	t.base.push_back(0); //don't worry about this one, doesn't produce any errors
	t.base.push_back(1); //don't worry about this one, doesn't produce any errors
	t.test(1);
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
