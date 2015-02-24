// Test if shadow declarations are correctly resolved after being intercepted
// (std::ferror in output for B::ferror method)

#include <cstdio>

using namespace std;

class A {
public:
	virtual int ferror() = 0;
};

class B : public A {
public:
	B() {
		fp = fopen("test.out", "wb");
	}
	~B() {
		fclose(fp);
	}
	int ferror() {
		return std::ferror(fp);
	}
private:
	FILE* fp;
};

int main() {
	B b;
	printf("%d\n", b.ferror());
	return 0;
}