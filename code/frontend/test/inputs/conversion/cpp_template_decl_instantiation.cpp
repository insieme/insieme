
// this template would not get instantiated by clang by default,
// so what is being tested here is our manual instantiation by
// means of "RequireCompleteType" in type_converter

template<typename Iter>
struct TemplatedStruct;

TemplatedStruct<int>* getFoo() {
	return nullptr;
}

template<typename Iter>
struct TemplatedStruct {
	Iter i;
};

int main() {
	int magic;

	#pragma test expect_ir(R"(
		def struct IMP_TemplatedStruct_int {
			i : int<4>;
		};
		def IMP_getFoo = function () -> ptr<IMP_TemplatedStruct_int> {
			return ptr_null(type_lit(IMP_TemplatedStruct_int), type_lit(f), type_lit(f));
		};
		{
			IMP_getFoo();
		}
	)")
	{
		getFoo();
	}
}

