/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
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
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 * regarding third party software licenses.
 */

struct C {
	int x;

	C() {
		auto l = [this]{
			x;
		};
	}
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
		decl IMP__conversion_operator_void_space__lparen__star__rparen__lparen__rparen_:const __any_string__::() -> ptr<() -> unit,t,f>;
		def struct __any_string__ {
			const function IMP__operator_call_ = () -> unit {
				5;
			}
		};
		{
			var ref<__any_string__,f,f,plain> v0 = <ref<__any_string__,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {};
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
			capture_0 : int<4>;
			const function IMP__operator_call_ = () -> unit {
				(this).capture_0;
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<__any_string__class,f,f,plain> v1 = <ref<__any_string__class,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__class)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {*v0};
		}
	)")
	{
		int captureCopy;
		auto l1 = [captureCopy]{ captureCopy; };
	}

	/// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Capture Copy Mutable

	#pragma test expect_ir(R"(
		def struct __any_string__class {
			capture_0 : int<4>;
			function IMP__operator_call_ = () -> unit {
				(this).capture_0 = 5;
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<__any_string__class,f,f,plain> v1 = <ref<__any_string__class,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__class)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {*v0};
		}
	)")
	{
		int captureCopy;
		auto l1 = [captureCopy]() mutable { captureCopy = 5; };
	}

	/// ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Capture Reference

	#pragma test expect_ir(R"(
		def struct __any_string__class {
			__any_string__field : ref<int<4>,f,f,cpp_ref>;
			const function IMP__operator_call_ = () -> unit {
				*(this).__any_string__field = 5;
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<__any_string__class,f,f,plain> v1 = <ref<__any_string__class,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__class)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {v0};
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
			var ref<__any_string__class,f,f,plain> v0 = <ref<__any_string__class,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__class)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {};
		}
	)")
	{
		auto l1 = [](int param){ param; };
	}

	/// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Captures & Param

	#pragma test expect_ir(R"(
		def struct __any_string__class {
			capture_0 : int<4>;
			capture_1 : ref<int<4>,f,f,cpp_ref>;
			const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>) -> unit {
				5;
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<int<4>,f,f,plain> v1 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<__any_string__class,f,f,plain> v2 = <ref<__any_string__class,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__class)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {*v0, v1};
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
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen__rparen_:const __any_string__::() -> ptr<() -> int<4>,t,f>;
		def struct __any_string__ {
				const function IMP__operator_call_ = () -> int<4> {
						return 5;
				}
		};
		{
				var ref<__any_string__,f,f,plain> v0 = <ref<__any_string__,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {};
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
				var ref<__any_string__class,f,f,plain> v1 = <ref<__any_string__class,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__class)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {ptr_from_ref(this)};
			}
		};
		var ref<IMP_C,f,f,plain> v0 = IMP_C::(ref_decl(type_lit(ref<IMP_C,f,f,plain>)));
	)")
	C c;

	/// ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Nested Lambda

	#pragma test expect_ir(R"(
		def struct __any_string__inner {
			capture_0 : ref<int<4>,t,f,cpp_ref>;
			const function IMP__operator_call_ = () -> int<4> {
				return **(this).capture_0;
			}
		};
		def struct __any_string__outer {
			capture_0 : int<4>;
			const function IMP__operator_call_ = () -> int<4> {
				var ref<__any_string__inner,f,f,plain> v1 = <ref<__any_string__inner,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__inner)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {(this).capture_0};
				return v1.IMP__operator_call_();
			}
		};
		{
			var ref<int<4>,f,f,plain> v0;
			var ref<__any_string__outer,f,f,plain> v1 = <ref<__any_string__outer,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__outer)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {*v0};
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
		def __any_string__invoke = function () -> unit {
			5;
		};
		def struct __any_string__class {
			const function IMP__conversion_operator_void_space__lparen__star__rparen__lparen__rparen_ = () -> ptr<() -> unit,t,f> {
				return ptr_of_function(__any_string__invoke);
			}
			const function IMP__operator_call_ = () -> unit {
				5;
			}
		};
		{
			var ref<ptr<() -> unit,t,f>,f,f,plain> v0 = <ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {}.IMP__conversion_operator_void_space__lparen__star__rparen__lparen__rparen_();
		}
	)")
	{
		void (*fun)() = []{ 5; };
	}

	#pragma test expect_ir(R"(
		def __any_string__invoke = function (p1 : ref<int<4>>) -> unit {
			p1;
		};
		def struct __any_string__class {
			const function IMP__conversion_operator_void_space__lparen__star__rparen__lparen_int_rparen_ = () -> ptr<(int<4>) -> unit,t,f> {
				return ptr_of_function(__any_string__invoke);
			}
			const function IMP__operator_call_ = (p1 : ref<int<4>>) -> unit {
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
		decl IMP__conversion_operator_void_space__lparen__star__rparen__lparen__rparen_:const __any_string__class::() -> ptr<() -> unit,t,f>;
		def struct __any_string__class {
			const function IMP__operator_call_ = () -> unit {
				42;
			}
		};
		def __any_string__take_lambda = function (v0 : ref<__any_string__class,f,f,plain>) -> unit {
			v0.IMP__operator_call_();
		};
		{
			__any_string__take_lambda(<ref<__any_string__class,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__class)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {});
		}
	)")
	{
		takeLambda([] { 42; });
	}

	#pragma test expect_ir(R"(
		def __any_string__invoke = function () -> unit {
			42;
		};
		def IMP_takeLambdaAsFunction = function (v0 : ref<ptr<() -> unit,t,f>,f,f,plain>) -> unit {
			ptr_deref(*v0)();
		};
		def struct __any_string__class {
			const function IMP__conversion_operator_void_space__lparen__star__rparen__lparen__rparen_ = () -> ptr<() -> unit,t,f> {
				return ptr_of_function(__any_string__invoke);
			}
			const function IMP__operator_call_ = () -> unit {
				42;
			}
		};
		{
			IMP_takeLambdaAsFunction(<ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {}.IMP__conversion_operator_void_space__lparen__star__rparen__lparen__rparen_());
		}
	)")
	{
		takeLambdaAsFunction([] { 42; });
	}

	/// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Direct Lambda Call

	#pragma test expect_ir(R"(
		decl struct __any_string__class;
		decl IMP__conversion_operator_void_space__lparen__star__rparen__lparen__rparen_:const __any_string__class::() -> ptr<() -> unit,t,f>;
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

	return 0;
}
