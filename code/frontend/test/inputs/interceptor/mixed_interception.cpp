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

template<typename T>
struct TransitiveReturnType {
	T t;
};

template<typename T>
struct TransitiveType {
	T t;
};

struct Transitive {
	template<typename S>
	TransitiveReturnType<S> transitive() { return {}; }
};

template<typename T>
void functionFoo() {
	TransitiveType<T> unused;
	Transitive t;
	t.transitive<T>();
}

struct NotIntercepted {
	template<typename T>
	void foo() {
		functionFoo<T>();

		TransitiveType<T> unused;
		Transitive t;
		t.transitive<T>();
	}
};

#include "mixed_interception.h"

int main() {
	int magic;

	// Here we test, that the final IR will not contain any instantiation created via the intercepted type.
	// We should not have an instance if foo<float> in NotIntercepted, of transitive<float> in Transitive, as well as TransitiveType<float> and TransitiveReturnType<float>.
	// These member functions and the associated types should be cleaned iff we do not call them ourselves directly in a non-intercepted way,
	// as nobody else will be able to call/use them anyways otherwise

	// Note, that the same also applies for templated functions like functionFoo. Instantiations of this function (and all related types/functions)
	// should only end up in the program if they are called directly.
	#pragma test expect_ir(R"(
		def struct IMP_TransitiveType_int {
			t : int<4>;
		};
		def struct IMP_TransitiveReturnType_int {
			t : int<4>;
		};
		def struct IMP_Transitive {
			function IMP_transitive_int_returns_TransitiveReturnType_lt_int_gt_ = () -> IMP_TransitiveReturnType_int {
				return <ref<IMP_TransitiveReturnType_int,f,f,plain>>(ref_decl(
						type_lit(ref<IMP_TransitiveReturnType_int,f,f,plain>)
				)) {0};
			}
		};
		def IMP_functionFoo_int_returns_void = function () -> unit {
			var ref<IMP_TransitiveType_int,f,f,plain> v1 = IMP_TransitiveType_int::(ref_decl(type_lit(ref<IMP_TransitiveType_int,f,f,plain>)));
			var ref<IMP_Transitive,f,f,plain> v0 = IMP_Transitive::(ref_decl(type_lit(ref<IMP_Transitive,f,f,plain>)));
			v0.IMP_transitive_int_returns_TransitiveReturnType_lt_int_gt_();
		};
		def struct IMP_NotIntercepted {
			function IMP_foo_int_returns_void = () -> unit {
				IMP_functionFoo_int_returns_void();
				var ref<IMP_TransitiveType_int,f,f,plain> v1 = IMP_TransitiveType_int::(ref_decl(type_lit(ref<IMP_TransitiveType_int,f,f,plain>)));
				var ref<IMP_Transitive,f,f,plain> v2 = IMP_Transitive::(ref_decl(type_lit(ref<IMP_Transitive,f,f,plain>)));
				v2.IMP_transitive_int_returns_TransitiveReturnType_lt_int_gt_();
			}
		};
		{
			var ref<IMP_NotIntercepted,f,f,plain> v0 = IMP_NotIntercepted::(
					ref_decl(type_lit(ref<IMP_NotIntercepted,f,f,plain>))
			);
			var ref<IMP_Intercepted,f,f,plain> v1 = lit("IMP_Intercepted::ctor" : IMP_Intercepted::())(
					ref_decl(type_lit(ref<IMP_Intercepted,f,f,plain>))
			);
			v0.IMP_foo_int_returns_void();
			instantiate_member(
					lit("target_type" : <ref<real<4>,f,f,qualified>>IMP_Intercepted::() -> unit),
					lit("IMP_Intercepted::IMP_bar" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>IMP_Intercepted::() -> unit)
			)(v1);
		}
	)")
	{
		NotIntercepted ni;
		Intercepted i;
		ni.foo<int>();
		i.bar<float>();
	}
};
