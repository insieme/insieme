#include <type_traits>

struct pod {
	int x;
	pod() : x(0) {}
	pod(int x) : x(x) {}
};

struct nonpod {
	int x;
	~nonpod() { }
};

template<typename T>
using base_type = typename std::remove_cv<typename std::remove_reference<T>::type>::type;

// argument type test
template <typename T>
base_type<T> setX(T p) {
	auto p1 = p;
	p1.x = 10;
	return p1;
}

int main() {
//////////////// test arguments T setX..(T) ////////////////
	{ // pod
		//call with lvalue
		pod p1;
		p1 = setX<pod>(p1);
		//call with init list expr
		pod p2 = setX<pod>({0});
		//call with xvalue
		pod p3 = setX<pod>(pod());
	}
	{ // nonpod
		//call with lvalue
		nonpod p1;
		p1 = setX<nonpod>(p1);
		//call with init list expr
		nonpod p2 = setX<nonpod>({0});
		//call with xvalue
		nonpod p3 = setX<nonpod>(nonpod());
	}

//////////////// test arguments T setX..(const T) ////////////////
	{ // pod
		//call with lvalue
		const pod p;
		pod p1 = setX<const pod>(p);
		//call with init list expr
		pod p2 = setX<const pod>({0});
		//call with xvalue
		pod p3 = setX<const pod>(pod());
	}
	{ // nonpod
		//call with lvalue
		nonpod p1;
		p1 = setX<nonpod>(p1);
		//call with init list expr
		nonpod p2 = setX<nonpod>({0});
		//call with xvalue
		nonpod p3 = setX<nonpod>(nonpod());
	}
	
//////////////// test arguments T setX..(T&) ////////////////
	{ // pod
		//call with lvalue
		pod p1;
		p1 = setX<pod&>(p1);
		//call with init list expr not possible
		//call with xvalue not possible
	}
	{ // nonpod
		//call with lvalue
		nonpod p1;
		p1 = setX<nonpod&>(p1);
		//call with init list expr not possible
		//call with xvalue not possible
	}

//////////////// test arguments T setX..(const T&) ////////////////
	{ // pod
		//call with lvalue
		pod p1;
		p1 = setX<const pod&>(p1);
		//call with init list expr
		pod p2 = setX<const pod&>({0});
		//call with xvalue
		pod p3 = setX<const pod&>(pod());
	}
	{ // nonpod
		//call with lvalue
		nonpod p1;
		p1 = setX<const nonpod&>(p1);
		//call with init list expr
		nonpod p2 = setX<const nonpod&>({0});
		//call with xvalue
		nonpod p3 = setX<const nonpod&>(nonpod());
	}

//////////////// test arguments T setX..(T&&) ////////////////
	{ // pod
		//call with lvalue not possible
		//call with init list expr
		pod p2 = setX<pod&&>({0});
		//call with xvalue
		pod p3 = setX<pod&&>(pod());
	}
	{ // nonpod
		//call with lvalue not possible
		//call with init list expr
		nonpod p2 = setX<nonpod&&>({0});
		//call with xvalue
		nonpod p3 = setX<nonpod&&>(nonpod());
	}

//////////////// test arguments T setX..(const T&&) ////////////////
	{ // pod
		//call with lvalue not possible
		//call with init list expr
		pod p2 = setX<const pod&&>({0});
		//call with xvalue
		pod p3 = setX<const pod&&>(pod());
	}
	{ // nonpod
		//call with lvalue not possible
		//call with init list expr
		nonpod p2 = setX<const nonpod&&>({0});
		//call with xvalue
		nonpod p3 = setX<const nonpod&&>(nonpod());
	}
	return 0;
}
