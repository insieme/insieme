template <typename T>
class Test {
	public:
		Test() {}
		T make() {
			return T();
		}
		operator T() {
			return make();
		}
};

namespace X {
	enum T {
		A=0, B=1
	} element;
	class C {
	};
}

int main() {
	Test<enum X::T> t;
	Test<X::C> x;
	X::T elem = t;
	X::C elem2 = x;
	return 0;
}
