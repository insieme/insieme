
#include <vector>

using namespace std;

template<typename D>
struct F {

	typedef void(D::*mFun)();
	mFun op;
	vector<mFun> ops;

};


struct A : public F<A> {

};

int main() {

	A a;	

	return 0;
}
