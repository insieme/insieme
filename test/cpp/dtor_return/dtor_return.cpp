
struct A  {
	~A() { return;  /* this return is the thing to be tested */ }
};

int main() {
	A a;
}
