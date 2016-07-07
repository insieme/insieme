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

int main() {
	;

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

//	#pragma test expect_ir(R"({})")
//	{
//		int captureCopy;
//		auto l1 = [captureCopy]{ captureCopy; };
//	}

	//{
	//	int captureRef;
	//	auto l1 = [&captureRef]{ captureRef; };
	//}

	//{
	//	auto l1 = [](int param){ param; };
	//}

	//{
	//	auto l1 = [](int& param){ param; };
	//}

	//{
	//	int captureCopy;
	//	int captureRef;
	//	auto l1 = [captureCopy,&captureRef](int param){ 5; };
	//}

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

	return 0;
}
