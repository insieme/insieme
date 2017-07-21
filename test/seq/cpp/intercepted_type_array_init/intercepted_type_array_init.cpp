
#include <array>

void takeArray(std::array<int, 3> a) {}

struct S {
	int v[3];
};

void takeS(S s) {

}

int main() {

	std::array<int, 3> a;
	takeArray(a);
	takeArray(std::array<int, 3>({1, 2, 3}));
	takeArray({1, 2, 3});

	S s;
	takeS(s);
	takeS(S({1, 2, 3}));
	takeS({1, 2, 3});
}
