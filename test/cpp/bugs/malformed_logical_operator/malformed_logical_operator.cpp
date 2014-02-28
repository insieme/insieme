template<typename T>
class Certain {
	T c;
public:
	Certain() : c() {}
	Certain(T t) : c(t) {}
	operator T() const {
		return c;
	}
};


namespace CGAL {

template <bool Protected=false>
class Interval_nt {
private:
	double sup,inf;
public:
	int i;
	Interval_nt() : inf(0), sup(0), i(0) {}
	const double & _inf() const {return inf;}
	const double & _sup() const {return sup;}

};

}

namespace FU {
	template <bool Prot>
	inline Certain<bool> test(const CGAL::Interval_nt<Prot> & x) {
		bool c = (x._sup() > 0.0) && (x._sup() == x._inf());
		if(x._sup() && x._sup())
			return false;
		if(/*x._inf() < 0.0 || */x._sup() == x._inf())
			return true;
		return false;
	}
}


using namespace FU;

int main() {
	CGAL::Interval_nt<> a;
	CGAL::Interval_nt<false> b;
	FU::test(b);
	FU::test(*new CGAL::Interval_nt<>());
	return 0;
}

