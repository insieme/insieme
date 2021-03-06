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
 */

struct C {
	int x;

	C() {
		auto l = [this]{
			x;
		};
	}

	//TODO potential resolver bug
	/*void test() {
		[&](){ x + 1; }();
	}*/
};

template<typename T>
void takeLambda(T lam) {
	lam();
}

void takeLambdaAsFunction(void (*lam)()) {
	lam();
}

int main() {
	;

	/// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Trivial Lambda

	#pragma test expect_ir(R"(
		decl struct __any_string__;
		decl IMP__conversion_operator_auto_space__lparen__star__rparen__lparen_void_rparen__space__minus__gt__space_void:const __any_string__::() -> ptr<() -> unit,t,f>;
		def struct __any_string__ {
			const function IMP__operator_call_ = () -> unit {
				5;
			}
		};
		{
			var ref<__any_string__,f,f,plain> v0 = ref_cast(<ref<__any_string__,f,f,plain>>(ref_temp(type_lit(__any_string__))) {}, type_lit(f), type_lit(f), type_lit(cpp_rref));
			v0.IMP__operator_call_();
		}
	)")
	{
		auto l1 = []{ 5; };
		l1();
	}

	/// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Capture Copy

	#pragma test expect_ir(R"(
		def struct __any_string__class {
			capture_captureCopy : int<4>;
			const function IMP__operator_call_ = () -> unit {
				(this).capture_captureCopy;
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<__any_string__class,f,f,plain> v1 = ref_cast(<ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {*v0}, type_lit(f), type_lit(f), type_lit(cpp_rref));
		}
	)")
	{
		int captureCopy;
		auto l1 = [captureCopy]{ captureCopy; };
	}

	/// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Capture Copy Mutable

	#pragma test expect_ir(R"(
		def struct __any_string__class {
			capture_captureCopy : int<4>;
			function IMP__operator_call_ = () -> unit {
				(this).capture_captureCopy = 5;
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<__any_string__class,f,f,plain> v1 = ref_cast(<ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {*v0}, type_lit(f), type_lit(f), type_lit(cpp_rref));
		}
	)")
	{
		int captureCopy;
		auto l1 = [captureCopy]() mutable { captureCopy = 5; };
	}

	/// ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Capture Reference

	#pragma test expect_ir(R"(
		def struct __any_string__class {
			capture_captureRef : ref<int<4>,f,f,cpp_ref>;
			const function IMP__operator_call_ = () -> unit {
				ref_kind_cast(*(this).capture_captureRef, type_lit(plain)) = 5;
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<__any_string__class,f,f,plain> v1 = ref_cast(<ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {ref_kind_cast(v0, type_lit(cpp_ref))}, type_lit(f), type_lit(f), type_lit(cpp_rref));
		}
	)")
	{
		int captureRef;
		auto l1 = [&captureRef]{ captureRef = 5; };
	}

	/// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// With Parameter

	#pragma test expect_ir(R"(
		decl struct __any_string__class;
		decl IMP__conversion_operator_void_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__class::() -> ptr<(int<4>) -> unit,t,f>;
		def struct __any_string__class {
			const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>) -> unit {
				v1;
			}
		};
		{
			var ref<__any_string__class,f,f,plain> v0 = ref_cast(<ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {}, type_lit(f), type_lit(f), type_lit(cpp_rref));
		}
	)")
	{
		auto l1 = [](int param){ param; };
	}

	/// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Captures & Param

	#pragma test expect_ir(R"(
		def struct __any_string__class {
			capture_captureCopy : int<4>;
			capture_captureRef : ref<int<4>,f,f,cpp_ref>;
			const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>) -> unit {
				5;
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<int<4>,f,f,plain> v1 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<__any_string__class,f,f,plain> v2 = ref_cast(<ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {*v0, ref_kind_cast(v1, type_lit(cpp_ref))}, type_lit(f), type_lit(f), type_lit(cpp_rref));
		}
	)")
	{
		int captureCopy;
		int captureRef;
		auto l1 = [captureCopy,&captureRef](int param){ 5; };
	}

	/// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Return from Lambda

	#pragma test expect_ir(R"(
		decl struct __any_string__;
		decl IMP__conversion_operator_auto_space__lparen__star__rparen__lparen_void_rparen__space__minus__gt__space_int:const __any_string__::() -> ptr<() -> int<4>,t,f>;
		def struct __any_string__ {
			const function IMP__operator_call_ = () -> int<4> {
				return 5;
			}
		};
		{
			var ref<__any_string__,f,f,plain> v0 = ref_cast(<ref<__any_string__,f,f,plain>>(ref_temp(type_lit(__any_string__))) {}, type_lit(f), type_lit(f), type_lit(cpp_rref));
		}
	)")
	{
		auto l1 = []{ return 5; };
	}

	/// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// "This" Capture

	#pragma test expect_ir(R"(
		decl struct IMP_C;
		decl IMP_C::x : int<4>;
		def struct __any_string__class {
			capture_0 : ptr<IMP_C>;
			const function IMP__operator_call_ = () -> unit {
				ptr_to_ref(*(this).capture_0).x;
			}
		};
		def struct IMP_C {
			x : int<4>;
			ctor function () {
				var ref<__any_string__class,f,f,plain> v1 = ref_cast(<ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {ptr_from_ref(this)}, type_lit(f), type_lit(f), type_lit(cpp_rref));
			}
		};
		{
			var ref<IMP_C,f,f,plain> v0 = IMP_C::(ref_decl(type_lit(ref<IMP_C,f,f,plain>)));
		}
	)")
	{
		C c;
		//TODO potential resolver bug
		//c.test();
	}

	/// ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Nested Lambda

	#pragma test expect_ir(R"(
		def struct __any_string__inner {
			capture_num : ref<int<4>,t,f,cpp_ref>;
			const function IMP__operator_call_ = () -> int<4> {
				return **(this).capture_num;
			}
		};
		def struct __any_string__outer {
			capture_num : int<4>;
			const function IMP__operator_call_ = () -> int<4> {
				var ref<__any_string__inner,f,f,plain> v1 = ref_cast(<ref<__any_string__inner,f,f,plain>>(ref_temp(type_lit(__any_string__inner))) {ref_kind_cast((this).capture_num, type_lit(cpp_ref))}, type_lit(f), type_lit(f), type_lit(cpp_rref));
				return v1.IMP__operator_call_();
			}
		};
		{
			var ref<int<4>,f,f,plain> v0;
			var ref<__any_string__outer,f,f,plain> v1 = ref_cast(<ref<__any_string__outer,f,f,plain>>(ref_temp(type_lit(__any_string__outer))) {*v0}, type_lit(f), type_lit(f), type_lit(cpp_rref));
		}
	)")
	{
		int num;
		auto a = [num] {
			auto b = [&num] {
				return num;
			};
			return b();
		};
	}

	/// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Lambda Function Pointer Conversion

	#pragma test expect_ir(R"(
		decl __any_string__class__static__IMP___invoke : () -> unit;
		def struct __any_string__class {
			const function IMP__conversion_operator_auto_space__lparen__star__rparen__lparen_void_rparen__space__minus__gt__space_void = () -> ptr<() -> unit,t,f> {
				return ptr_of_function(__any_string__class__static__IMP___invoke);
			}
			const function IMP__operator_call_ = () -> unit {
				5;
			}
			static IMP___invoke = function () -> unit {
				5;
			}
		};
		{
			var ref<ptr<() -> unit,t,f>,f,f,plain> v0 = <ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {}.IMP__conversion_operator_auto_space__lparen__star__rparen__lparen_void_rparen__space__minus__gt__space_void();
		}
	)")
	{
		void (*fun)() = []{ 5; };
	}

	#pragma test expect_ir(R"(
		decl __any_string__class__static__IMP___invoke : (int<4>) -> unit;
		def struct __any_string__class {
			const function IMP__conversion_operator_void_space__lparen__star__rparen__lparen_int_rparen_ = () -> ptr<(int<4>) -> unit,t,f> {
				return ptr_of_function(__any_string__class__static__IMP___invoke);
			}
			const function IMP__operator_call_ = (p1 : ref<int<4>>) -> unit {
				p1;
			}
			static IMP___invoke = function (p1 : ref<int<4>>) -> unit {
				p1;
			}
		};
		{
			var ref<ptr<(int<4>) -> unit,t,f>,f,f,plain> v0 = <ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {}.IMP__conversion_operator_void_space__lparen__star__rparen__lparen_int_rparen_();
		}
	)")
	{
		void (*fun)(int) = [](int p1){ p1; };
	}

	/// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Lambda as Parameter

	#pragma test expect_ir(R"(
		decl struct __any_string__class;
		decl IMP__conversion_operator_auto_space__lparen__star__rparen__lparen_void_rparen__space__minus__gt__space_void:const __any_string__class::() -> ptr<() -> unit,t,f>;
		def struct __any_string__class {
			const function IMP__operator_call_ = () -> unit {
				42;
			}
		};
		def __any_string__take_lambda = function (v0 : ref<__any_string__class,f,f,plain>) -> unit {
			v0.IMP__operator_call_();
		};
		{
			__any_string__take_lambda(ref_cast(<ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {}, type_lit(f), type_lit(f), type_lit(cpp_rref)));
		}
	)")
	{
		takeLambda([] { 42; });
	}

	#pragma test expect_ir(R"(
		decl __any_string__class__static__IMP___invoke : () -> unit;
		def IMP_takeLambdaAsFunction = function (v0 : ref<ptr<() -> unit,t,f>,f,f,plain>) -> unit {
			ptr_deref(*v0)();
		};
		def struct __any_string__class {
			const function IMP__conversion_operator_auto_space__lparen__star__rparen__lparen_void_rparen__space__minus__gt__space_void = () -> ptr<() -> unit,t,f> {
				return ptr_of_function(__any_string__class__static__IMP___invoke);
			}
			const function IMP__operator_call_ = () -> unit {
				42;
			}
			static IMP___invoke = function () -> unit {
				42;
			}
		};
		{
			IMP_takeLambdaAsFunction(<ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {}.IMP__conversion_operator_auto_space__lparen__star__rparen__lparen_void_rparen__space__minus__gt__space_void());
		}
	)")
	{
		takeLambdaAsFunction([] { 42; });
	}

	/// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Direct Lambda Call

	#pragma test expect_ir(R"(
		decl struct __any_string__class;
		decl IMP__conversion_operator_void_space__lparen__star__rparen__lparen_void_rparen_:const __any_string__class::() -> ptr<() -> unit,t,f>;
		def struct __any_string__class {
			const function IMP__operator_call_ = () -> unit { }
		};
		{
			<ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {}.IMP__operator_call_();
		}
	)")
	{
		[](){}();
	}

	/// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Direct Lambda Call with capture

	#pragma test expect_ir(R"(
		def struct __any_string__class {
			capture_n : int<4>;
			const function IMP__operator_call_ = () -> unit {
				(this).capture_n;
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			<ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {*v0}.IMP__operator_call_();
		}
	)")
	{
		int n;
		[n]() {
			n;
		}();
	}

	return 0;
}
