
void declaration_definition_call(int x, int y);

void declaration_definition_call(int x, int y) {
	x;
	y;
}

void declaration_call_definition(int x);

int main() {
	#pragma test expect_ir("(v1: int<4>, v2: int<4>) -> unit { v1; v2; }(1,2)")
	declaration_definition_call(1,2);
	
	#pragma test expect_ir("(v1: int<4>) -> unit { v1; }(2)")
	declaration_call_definition(2);

	return 0;
}

void declaration_call_definition(int x) {
	x;
}
