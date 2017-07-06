/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 */

template<typename M>
void templateFunWithLambda() {
	M a;
	[&](){
		a;
	}();
}

template<typename M>
void templateFunWithLambdaParam() {
	M a;
	[](M m){
		m;
	}(a);
}

struct ClassWithTemplatedFun {
	template<typename Op>
	void fun(Op op) {
		op();
	}
};
template<typename T>
void templatedFunUsingTemplatedClass() {
	ClassWithTemplatedFun s;
	s.fun([]() {T i;});
}

int main() {
	;

	/// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Lambda specified in template

	#pragma test expect_ir(R"(
		def struct __any_string__class_int {
			capture_0 : ref<int<4>,f,f,cpp_ref>;
			const function IMP__operator_call_ = () -> unit {
				(this).capture_0;
			}
		};
		def struct __any_string__class_real {
			capture_0 : ref<real<4>,f,f,cpp_ref>;
			const function IMP__operator_call_ = () -> unit {
				(this).capture_0;
			}
		};
		def IMP_templateFunWithLambda_int_returns_void = function () -> unit {
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			<ref<__any_string__class_int,f,f,plain>>(ref_temp(type_lit(__any_string__class_int))) {ref_kind_cast(v0, type_lit(cpp_ref))}.IMP__operator_call_();
		};
		def IMP_templateFunWithLambda_float_returns_void = function () -> unit {
			var ref<real<4>,f,f,plain> v0 = ref_decl(type_lit(ref<real<4>,f,f,plain>));
			<ref<__any_string__class_real,f,f,plain>>(ref_temp(type_lit(__any_string__class_real))) {ref_kind_cast(v0, type_lit(cpp_ref))}.IMP__operator_call_();
		};
		{
			IMP_templateFunWithLambda_int_returns_void();
			IMP_templateFunWithLambda_float_returns_void();
		}
	)")
	{
		templateFunWithLambda<int>();
		templateFunWithLambda<float>();
	}

	#pragma test expect_ir(R"(
		decl struct __any_string__class_int;
		decl struct __any_string__class_real;
		decl IMP__conversion_operator_void_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__class_int::() -> ptr<(int<4>) -> unit,t,f>;
		decl IMP__conversion_operator_void_space__lparen__star__rparen__lparen_float_rparen_:const __any_string__class_real::() -> ptr<(real<4>) -> unit,t,f>;
		def struct __any_string__class_int {
			const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>) -> unit {
				v1;
			}
		};
		def struct __any_string__class_real {
			const function IMP__operator_call_ = (v1 : ref<real<4>,f,f,plain>) -> unit {
				v1;
			}
		};
		def IMP_templateFunWithLambdaParam_int_returns_void = function () -> unit {
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			<ref<__any_string__class_int,f,f,plain>>(ref_temp(type_lit(__any_string__class_int))) {}.IMP__operator_call_(*v0);
		};
		def IMP_templateFunWithLambdaParam_float_returns_void = function () -> unit {
			var ref<real<4>,f,f,plain> v0 = ref_decl(type_lit(ref<real<4>,f,f,plain>));
			<ref<__any_string__class_real,f,f,plain>>(ref_temp(type_lit(__any_string__class_real))) {}.IMP__operator_call_(*v0);
		};
		{
			IMP_templateFunWithLambdaParam_int_returns_void();
			IMP_templateFunWithLambdaParam_float_returns_void();
		}
	)")
	{
		templateFunWithLambdaParam<int>();
		templateFunWithLambdaParam<float>();
	}

	#pragma test expect_ir(R"(
		decl struct __any_string__0;
		decl struct __any_string__1;
		decl struct __any_string__2;
		decl struct IMP_ClassWithTemplatedFun;
		decl IMP__conversion_operator_void_space__lparen__star__rparen__lparen__rparen_:const __any_string__0::() -> ptr<() -> unit,t,f>;
		decl IMP__conversion_operator_void_space__lparen__star__rparen__lparen__rparen_:const __any_string__1::() -> ptr<() -> unit,t,f>;
		decl IMP__conversion_operator_void_space__lparen__star__rparen__lparen__rparen_:const __any_string__2::() -> ptr<() -> unit,t,f>;
		def struct __any_string__0 {
			const function IMP__operator_call_ = () -> unit {
				var ref<int<4>,f,f,plain> v1 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			}
		};
		def struct __any_string__1 {
			const function IMP__operator_call_ = () -> unit {
				var ref<real<8>,f,f,plain> v1 = ref_decl(type_lit(ref<real<8>,f,f,plain>));
			}
		};
		def struct __any_string__2 {
			const function IMP__operator_call_ = () -> unit {
				var ref<IMP_ClassWithTemplatedFun,f,f,plain> v1 = IMP_ClassWithTemplatedFun::(ref_decl(type_lit(ref<IMP_ClassWithTemplatedFun,f,f,plain>)));
			}
		};
		def struct IMP_ClassWithTemplatedFun {
			function __any_string__fun = (v1 : ref<__any_string__0,f,f,plain>) -> unit {
				v1.IMP__operator_call_();
			}
			function __any_string__fun = (v1 : ref<__any_string__1,f,f,plain>) -> unit {
				v1.IMP__operator_call_();
			}
			function __any_string__fun = (v1 : ref<__any_string__2,f,f,plain>) -> unit {
				v1.IMP__operator_call_();
			}
		};
		def IMP_templatedFunUsingTemplatedClass_int_returns_void = function () -> unit {
			var ref<IMP_ClassWithTemplatedFun,f,f,plain> v0 = IMP_ClassWithTemplatedFun::(ref_decl(type_lit(ref<IMP_ClassWithTemplatedFun,f,f,plain>)));
			v0.__any_string__fun(<ref<__any_string__0,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__0)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {});
		};
		def IMP_templatedFunUsingTemplatedClass_double_returns_void = function () -> unit {
			var ref<IMP_ClassWithTemplatedFun,f,f,plain> v0 = IMP_ClassWithTemplatedFun::(ref_decl(type_lit(ref<IMP_ClassWithTemplatedFun,f,f,plain>)));
			v0.__any_string__fun(<ref<__any_string__1,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__1)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {});
		};
		def IMP_templatedFunUsingTemplatedClass_struct_ClassWithTemplatedFun_returns_void = function () -> unit {
			var ref<IMP_ClassWithTemplatedFun,f,f,plain> v0 = IMP_ClassWithTemplatedFun::(ref_decl(type_lit(ref<IMP_ClassWithTemplatedFun,f,f,plain>)));
			v0.__any_string__fun(<ref<__any_string__2,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__2)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {});
		};
		{
			IMP_templatedFunUsingTemplatedClass_int_returns_void();
			IMP_templatedFunUsingTemplatedClass_double_returns_void();
			IMP_templatedFunUsingTemplatedClass_struct_ClassWithTemplatedFun_returns_void();
		}
	)")
	{
		templatedFunUsingTemplatedClass<int>();
		templatedFunUsingTemplatedClass<double>();
		templatedFunUsingTemplatedClass<ClassWithTemplatedFun>();
	}

	return 0;
}
