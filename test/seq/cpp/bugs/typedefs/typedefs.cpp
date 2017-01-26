
#include <vector>

using namespace std;

typedef struct {
	int x;
} A;


struct B {};

typedef B C;

int main() {
	A a;

	vector<A> as;
	vector<B> bs;
	vector<C> cs;

	return 0;
}
