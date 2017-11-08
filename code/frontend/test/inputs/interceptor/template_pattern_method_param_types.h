template<unsigned AParm>
struct A {

};

template<unsigned Y, unsigned BParm>
struct B {

	using MyA = A<BParm>;

	B(const MyA&) {}


	static constexpr unsigned XT = Y;
	using MyAConstexpr = A<XT>;

	B(const MyAConstexpr&) {}

};
