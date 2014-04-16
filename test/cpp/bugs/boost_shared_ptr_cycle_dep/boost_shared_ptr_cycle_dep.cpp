

#include <boost/thread/tss.hpp>

struct Loop {
	void f(boost::shared_ptr<Loop> l) {};
};

int main() {

	Loop loop;

	

	return 0;
}
