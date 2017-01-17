namespace X {

template <typename T>
class B {
    public: T* t;
};

class A : B<A>{
	int mem;
public:
	A() : mem(1) {}
	A(const A& a) {}
	A& test(A &a) {
		if(a.mem ==1)
			return a+=a;
		return a;
	}
	bool operator==(A &a);
	A& operator+=(A &a);
	A& operator*=(A &a);
	A& operator-=(A &a);
	A& operator/=(A &a);

	A& operator+(A &a);
	A& operator*(A &a);
	A& operator-(A &a);
	A& operator/(A &a);

};


	bool A::operator==(A &a) {
		std::cout << "==" << std::endl;
        bool c = (mem == mem);
		return c;
	}
	A& A::operator+=(A &a) {
		std::cout << "+=" << std::endl;
        mem += a.mem;
		return a;
	}
	A& A::operator*=(A &a) {
		std::cout << "*=" << std::endl;
        mem *= a.mem;
		return a;
	}
	A& A::operator-=(A &a) {
		std::cout << "-=" << std::endl;
        mem -= a.mem;
		return a;
	}
	A& A::operator/=(A &a) {
		std::cout << "/=" << std::endl;
        if(a.mem)
            mem /= a.mem;
		return a;
	}
	
    A& A::operator+(A &a) {
		std::cout << "+" << std::endl;
        mem += a.mem;
		return a;
	}
	A& A::operator*(A &a) {
		std::cout << "*" << std::endl;
        mem *= a.mem;
		return a;
	}
	A& A::operator-(A &a) {
		std::cout << "-" << std::endl;
        mem -= a.mem;
		return a;
	}
	A& A::operator/(A &a) {
		std::cout << "/" << std::endl;
        if(a.mem)
            mem /= a.mem;
		return a;
	}
}
