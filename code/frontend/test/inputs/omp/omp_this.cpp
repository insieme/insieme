
struct StructWithOMP {

	int x;

	int foo() {
		int ret;

		#pragma omp parallel
		{
			ret = x;
		}

		#pragma omp parallel
		{
			this;
		}

		return ret;
	}
};


int main() {
	int i;

	#pragma test expect_ir(R"(
		decl struct IMP_StructWithOMP;
		decl _ins_omp_parallel_0 : (ref<IMP_StructWithOMP,f,f,plain>, ref<int<4>,f,f,plain>) -> unit;
		decl _ins_omp_parallel_1 : (ref<IMP_StructWithOMP,f,f,plain>) -> unit;
		decl IMP_StructWithOMP::x : int<4>;
		decl IMP_foo:IMP_StructWithOMP::() -> int<4>;

		def struct IMP_StructWithOMP {
			x : int<4>;
			function IMP_foo = () -> int<4> {
				var ref<int<4>,f,f,plain> v1 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
				{
					merge(parallel(job[1ul...] => _ins_omp_parallel_0(this, v1)));
				}
				{
					merge(parallel(job[1ul...] => _ins_omp_parallel_1(this)));
				}
				return *v1;
			}
		};
		def _ins_omp_parallel_0 = function (v0 : ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain>, v1 : ref<ref<int<4>,f,f,plain>,f,f,plain>) -> unit {
			{
				*v1 = *(*v0).x;
			}
			merge_all();
		};
		def _ins_omp_parallel_1 = function (v0 : ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain>) -> unit {
			{
				ptr_from_ref(*v0);
			}
			merge_all();
		};
		{
			var ref<IMP_StructWithOMP,f,f,plain> v0 = IMP_StructWithOMP::(ref_decl(type_lit(ref<IMP_StructWithOMP,f,f,plain>)));
		}
	)")
	{
		StructWithOMP s;
	}
}
