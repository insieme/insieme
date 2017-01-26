#include <type_traits>
#include <utility>

struct pod {
	int x;
};

struct nonpod {
	int x;
	~nonpod() { }
};

template<typename T>
using base_type = typename std::remove_cv<typename std::remove_reference<T>::type>::type;

// return type test
template <typename T>
T getX(base_type<T> p) {
	T x = p;
	return x;
}
template <typename T>
T getXZero() {
	return std::move(base_type<T>());
}


int main() {
//////////////// test return T getX(T) and assign to both const and non-const T, T&, T&& ////////////////
	{ // pod
		pod p1 = getX<pod>(pod());
		const pod p2 = getX<pod>(pod());
		const pod& p3 = getX<pod>(pod());
		pod&& p4 = getX<pod>(pod());
		const pod&& p5 = getX<pod>(pod());		
	}
	{ // nonpod
		nonpod p1 = getX<nonpod>(nonpod());
		const nonpod p2 = getX<nonpod>(nonpod());
		const nonpod& p3 = getX<nonpod>(nonpod());
		nonpod&& p4 = getX<nonpod>(nonpod());
		const nonpod&& p5 = getX<nonpod>(nonpod());
	}
//////////////// test return const T getX(T) and assign to both const and non-const T, T&, T&& ////////////////
	{ // pod
		pod p1 = getX<const pod>(pod());
		const pod& p3 = getX<const pod>(pod());
		const pod&& p5 = getX<const pod>(pod());		
	}
	{ // nonpod
		nonpod p1 = getX<const nonpod>(nonpod());
		const nonpod& p3 = getX<const nonpod>(nonpod());
		const nonpod&& p5 = getX<const nonpod>(nonpod());
	}	
//////////////// test return T& getX(T) and assign to both const and non-const T, T&, T&& ////////////////
	{ // pod
		pod& p1 = getX<pod&>(pod());
		pod p2 = getX<pod&>(pod());
		const pod& p3 = getX<pod&>(pod());
		const pod p4 = getX<pod&>(pod());
	}
	{ // nonpod
		nonpod& p1 = getX<nonpod&>(nonpod());
		nonpod p2 = getX<nonpod&>(nonpod());
		const nonpod& p3 = getX<nonpod&>(nonpod());
		const nonpod p4 = getX<nonpod&>(nonpod());		
	}

//////////////// test return const T& getX(T) and assign to both const and non-const T, T&, T&& ////////////////
	{ // pod
		const pod& p1 = getX<const pod&>(pod());
		pod p2 = getX<const pod&>(pod());
		const pod p3 = getX<const pod&>(pod());
	}
	{ // nonpod
		const nonpod& p1 = getX<const nonpod&>(nonpod());
		nonpod p2 = getX<const nonpod&>(nonpod());
		const nonpod p3 = getX<const nonpod&>(nonpod());
	}

//////////////// test return const T&& getX(T) and assign to both const and non-const T, T&, T&& ////////////////
	{ // pod
		pod&& p1 = getXZero<pod&&>();
		pod p2 = getXZero<pod&&>();
		const pod p3 = getXZero<pod&&>();
		const pod& p5 = getXZero<pod&&>();
		const pod&& p6 = getXZero<pod&&>();
	}
	{ // nonpod
		nonpod&& p3 = getXZero<nonpod&&>();
		nonpod p4 = getXZero<nonpod&&>();
		const nonpod p5 = getXZero<nonpod&&>();
		const nonpod& p6 = getXZero<nonpod&&>();
		const nonpod&& p7 = getXZero<nonpod&&>();
	}

//////////////// test arguments T getX(const T&&) and assign to both const and non-const T, T&, T&& ////////////////
	{ // pod
		const pod&& p1 = getXZero<const pod&&>();
		const pod& p2 = getXZero<const pod&&>();
		const pod p3 = getXZero<const pod&&>();
		pod p4 = getXZero<const pod&&>();
	}
	{ // nonpod
		const nonpod&& p1 = getXZero<const nonpod&&>();
		const nonpod& p2 = getXZero<const nonpod&&>();
		const nonpod p3 = getXZero<const nonpod&&>();
		nonpod p4 = getXZero<const nonpod&&>();
	}
	return 0;
}
