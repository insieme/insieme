
struct Simplest {
};

#define SIMPLEST_IR R"(
def struct IMP_Simplest {
};)"


int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(SIMPLEST_IR,R"( { 
		var ref<IMP_Simplest,f,f,plain> v0 = IMP_Simplest::(ref_var(type_lit(IMP_Simplest))); 
		var ref<IMP_Simplest,f,f,plain> v1 = IMP_Simplest::(ref_var(type_lit(IMP_Simplest)));
		IMP_Simplest::operator_assign(v0,v1);
	} )")
	{ 
		Simplest a, b;
		a = b;
	}

	return 0;
}
