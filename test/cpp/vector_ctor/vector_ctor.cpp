

struct C1 { };

struct C2 {
	int x;
	C2() : x(10) {}
};

int main() {
	C1 c1[4];
	C2 c2[4];
	return 0;
}
