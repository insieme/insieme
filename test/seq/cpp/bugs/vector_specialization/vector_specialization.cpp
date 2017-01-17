#include <vector>
#include <tr1/memory>

struct X; //FWD
typedef std::tr1::shared_ptr<X> XPtr;
struct X {
	std::vector<XPtr> v;
};

int main (){

	std::vector<X> collectionX;

	return 0;
}

