
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
		decl _ins_omp_parallel_0 : (ref<int<4>,f,f,plain>, ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain>) -> unit;
		decl _ins_omp_parallel_1 : (ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain>) -> unit;
		decl IMP_StructWithOMP::x : int<4>;
		decl IMP_foo:IMP_StructWithOMP::() -> int<4>;
		def struct IMP_StructWithOMP {
			x : int<4>;
			function IMP_foo = () -> int<4> {
				var ref<int<4>,f,f,plain> v1 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
				{
					var ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain> v2 = this;
					merge(parallel(job[1ul...] => _ins_omp_parallel_0(v1, v2)));
				}
				{
					var ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain> v3 = this;
					merge(parallel(job[1ul...] => _ins_omp_parallel_1(v3)));
				}
				return *v1;
			}
		};
		def _ins_omp_parallel_0 = function (v0 : ref<ref<int<4>,f,f,plain>,f,f,plain>, v1 : ref<ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain>,f,f,plain>) -> unit {
			{
				*v0 = *(**v1).x;
			}
			merge_all();
		};
		def _ins_omp_parallel_1 = function (v0 : ref<ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain>,f,f,plain>) -> unit {
			{
				ptr_from_ref(**v0);
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
