
int bar() { return 0; }

void bla(int a, int b, ...);
void foo(int a, ...);

int main() {

	#define PREAMBLE "using \"ext.varargs\";"
	#define BLA "lit(\"bla\":(int<4>,int<4>,var_list)->unit)"
	#define FOO "lit(\"foo\":(int<4>,var_list)->unit)"

	#pragma test expect_ir(PREAMBLE "{",BLA,"(1, 2, varlist_pack(())); }")
	{ bla(1,2); }
	
	#pragma test expect_ir(PREAMBLE "{",BLA,"(2, 3, varlist_pack((17))); }")
	{ bla(2,3, 17); }
	
	#pragma test expect_ir(PREAMBLE "{",BLA,"(5, 6, varlist_pack((5u, 17))); }")
	{ bla(5,6, 5u, 17); }
	
	#pragma test expect_ir(PREAMBLE "{",FOO,"(1, varlist_pack(())); }")
	{ foo(1); }
	
	#pragma test expect_ir(PREAMBLE "{",FOO,"(1, varlist_pack((() -> int<4> { return 0;}()))); }")
	{ foo(1, bar()); }

	return 0;
}


