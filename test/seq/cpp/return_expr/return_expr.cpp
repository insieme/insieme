
class Trivial {
	int x;
public:
	Trivial() {}
	Trivial(int x) : x(x) {}
};
int g_a;

class NonTrivial {
public:
	NonTrivial() {}
	NonTrivial(int x) {
		g_a = x;
	}
	~NonTrivial() {
		g_a++;
	}
};

Trivial returnTrivial() {
	return Trivial();
}
Trivial returnTrivialInit() {
	return {};
}
Trivial returnTrivialInitWithParam() {
	return { 5 };
}

NonTrivial returnNonTrivial() {
	return NonTrivial();
}
NonTrivial returnNonTrivialInit() {
	return {};
}
NonTrivial returnNonTrivialInitWithParam() {
	return { 7 };
}

int main() {
	returnTrivial();
	returnTrivialInit();
	returnTrivialInitWithParam();

	returnNonTrivial();
	returnNonTrivialInit();
	returnNonTrivialInitWithParam();
}

