#include <iostream>
#include <vector>

using namespace std;

struct A{
	int a;
	A(int a): a(a) {}
};


int main(){
	vector<A> v;
	for (int i = 0; i < 10; ++i)
		v.push_back(A(i));

	for (auto& x : v)
		cout << x.a << endl;

	int v2[] = {1, 2, 3};
	for (auto& x : v2) {
		if (x == 2) continue;
		cout << x << endl;
	}
}
