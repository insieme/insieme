
int initedGlobal = 5;

#pragma test expect_ir(R"(()->int<4> { lit("initedGlobal":ref<int<4>>) = 5; *lit("initedGlobal":ref<int<4>>); return 0; })")
int main() {
	initedGlobal;
	return 0;
}
