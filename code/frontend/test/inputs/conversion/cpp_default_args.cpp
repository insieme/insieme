
int f(int x = 7) {
	return x;
}
#define F_IR "def IMP_f = function (v0 : ref<int<4>,f,f,plain>) -> int<4> { return *v0; }; "


void g(int x, unsigned y = 4u, char z = 'd') {
}
#define G_IR "def IMP_g = function (v0 : ref<int<4>,f,f,plain>, v1 : ref<uint<4>,f,f,plain>, v2 : ref<char,f,f,plain>) -> unit { }; "

int main() {

	#pragma test expect_ir(F_IR,"IMP_f(4)")
	f(4);
	
	#pragma test expect_ir(F_IR,"IMP_f(7)")
	f();
	
	#pragma test expect_ir(G_IR,R"(IMP_g(4, 19u, lit("'s'":char)))")
	g(4, 19u, 's');

	#pragma test expect_ir(G_IR,R"(IMP_g(5, 12u, lit("'d'":char)))")
	g(5, 12u);
	
	#pragma test expect_ir(G_IR,R"(IMP_g(7, 4u, lit("'d'":char)))")
	g(7);
	
	return 0;
}
