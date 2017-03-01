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
// template class method
template<class T>
struct A {
	T bla() { return T(); }
};

// class templated method
struct B {
	template<class T>
	T bla() { return T(); }
};

// template class templated method
template<class T>
struct C {
	template<class T2>
	T bla() { return T() + T2(); }
};

// const and non const templated method
struct ConstNonConstInline {
	template<class T>
	T bla() { return T(); }
	template<class T>
	T bla() const { return T(); }
};

int main() {
	;

	#pragma test expect_ir(R"(
		def struct IMP_A_int {
			function IMP_bla_returns_int = () -> int<4> {
				return 0;
			}
		};
		{
			var ref<IMP_A_int,f,f,plain> v0 = IMP_A_int::(ref_decl(type_lit(ref<IMP_A_int,f,f,plain>)));
			v0.IMP_bla_returns_int();
		}
	)")
	{
		A<int> a;
		a.bla();
	}

	#pragma test expect_ir(R"(
		def struct IMP_B {
			function IMP_bla_float_returns_float = () -> real<4> {
				return 0.0f;
			}
			function IMP_bla_int_returns_int = () -> int<4> {
				return 0;
			}
		};
		{
			var ref<IMP_B,f,f,plain> v0 = IMP_B::(ref_decl(type_lit(ref<IMP_B,f,f,plain>)));
			v0.IMP_bla_int_returns_int();
			v0.IMP_bla_float_returns_float();
		}
	)")
	{
		B b;
		b.bla<int>();
		b.bla<float>();
	}

	#pragma test expect_ir(R"(
		def struct IMP_C_int {
			function IMP_bla_float_returns_int = () -> int<4> {
				return num_cast(num_cast(0, type_lit(real<4>))+0.0f, type_lit(int<4>));
			}
		};
		{
			var ref<IMP_C_int,f,f,plain> v0 = IMP_C_int::(ref_decl(type_lit(ref<IMP_C_int,f,f,plain>)));
			v0.IMP_bla_float_returns_int();
		}
	)")
	{
		C<int> c;
		c.bla<float>();
	}

	#pragma test expect_ir(R"(
		def struct IMP_ConstNonConstInline {
			function IMP_bla_int_returns_int = () -> int<4> {
				return 0;
			}
			const function IMP_bla_int_returns_int = () -> int<4> {
				return 0;
			}
		};
		{
			var ref<IMP_ConstNonConstInline,f,f,plain> v0 = IMP_ConstNonConstInline::(ref_decl(type_lit(ref<IMP_ConstNonConstInline,f,f,plain>)));
			v0.IMP_bla_int_returns_int();
			var ref<IMP_ConstNonConstInline,t,f,cpp_ref> v1 = v0;
			v1.IMP_bla_int_returns_int();
		}
	)")
	{
		ConstNonConstInline c;
		c.bla<int>();
		const ConstNonConstInline& cr = c;
		cr.bla<int>();
	}

	return 0;
}
