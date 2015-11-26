
int initedGlobal = 5;

#pragma test expect_ir(R"(def IMP_main = ()->int<4> { lit("initedGlobal":ref<int<4>>) = 5; *lit("initedGlobal":ref<int<4>>); return 0; }; IMP_main)")
int main() {
	initedGlobal;
	return 0;
}
