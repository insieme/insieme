
struct S {
	template<class C>
	C a()  { return 0; }
	template<class C>
	C a() const  { return 0; }
};

int main() {
	S s;
	int b = s.a<int>();
}