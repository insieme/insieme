
int f() {
	static int x = 5;
	#pragma test expect_ir("REGEX",R"(return\s*\*x_static_local_.*c_static_dot_c_3_2)")
	return x;
}

#pragma test expect_ir("REGEX",R"(.*x_static_local_.*c_static_dot_c_3_2 = 5;.*return.*)")
int main() {
	return f();
}
