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

void fooValue(int i) {}
void fooReference(int& i) {}
void fooConstReference(const int& i) {}

template<unsigned Dims>
void templateInCondition() {
	for(unsigned i = 0; i < Dims; i++) {}
}

int main() {

	int magic;

	/////////// Test passing by value, reference and const reference -----------------------------------------------------------------------------------

	// passing by value is ok, can only be read and thus gets converted to a for loop
	#pragma test expect_ir(R"(
		def IMP_fooValue = function (v0 : ref<int<4>,f,f,plain>) -> unit { };
		{
			{
				for( int<4> v0 = 0 .. 10 : 1) {
					IMP_fooValue(v0);
				}
			}
		}
	)")
	{
		for(int i = 0; i < 10; i++) {
			fooValue(i);
		}
	}

	// passing by reference prohibits conversion
	#pragma test expect_ir(R"(
		def IMP_fooReference = function (v0 : ref<int<4>,f,f,cpp_ref>) -> unit { };
		{
			{
				var ref<int<4>,f,f,plain> v0 = 0;
				while(*v0<10) {
					IMP_fooReference(ref_kind_cast(v0, type_lit(cpp_ref)));
					gen_post_inc(v0);
				}
			}
		}
	)")
	{
		for(int i = 0; i < 10; i++) {
			fooReference(i);
		}
	}

	// Note that the same goes for passing by const reference. Here we actually might convert to a for loop again, but complex analysis would be required
	// to ensure that the const-ness is never violated for the iterator variable. Thus we stay conservative and don't convert the loop
	#pragma test expect_ir(R"(
		def IMP_fooConstReference = function (v0 : ref<int<4>,t,f,cpp_ref>) -> unit { };
		{
			{
				var ref<int<4>,f,f,plain> v0 = 0;
				while(*v0<10) {
					IMP_fooConstReference(ref_kind_cast(v0, type_lit(cpp_ref)));
					gen_post_inc(v0);
				}
			}
		}
	)")
	{
		for(int i = 0; i < 10; i++) {
			fooConstReference(i);
		}
	}


	/////////// Test comparing different types in the condition ----------------------------------------------------------------------------------------
	// If the left side of the condition has to be casted to be compared to the right side, we need to actually cast the right side down in the for loop condition

	#pragma test expect_ir(R"(
		{{
			for( int<4> v0 = 0 .. num_cast(10u, type_lit(int<4>)) : 1) { }
		}}
	)")
	{
		for(int i = 0; i < 10u; i++) {}
	}

	#pragma test expect_ir(R"(
		{{
			for( uint<4> v0 = num_cast(0, type_lit(uint<4>)) .. num_cast(10, type_lit(uint<4>)) : 1u) { }
		}}
	)")
	{
		for(unsigned i = 0; i < 10; i++) {}
	}

	#pragma test expect_ir(R"(
		{{
			for( int<4> v0 = 0 .. num_cast(10l, type_lit(int<4>)) : 1) { }
		}}
	)")
	{
		for(int i = 0; i < 10L; i++) {}
	}

	#pragma test expect_ir(R"(
		{{
			for( int<8> v0 = num_cast(0, type_lit(int<8>)) .. num_cast(10, type_lit(int<8>)) : 1l) { }
		}}
	)")
	{
		for(long i = 0; i < 10; i++) {}
	}

	#pragma test expect_ir(R"(
		def IMP_templateInCondition_10_returns_void = function () -> unit {
			for( uint<4> v0 = num_cast(0, type_lit(uint<4>)) .. 10u : 1u) { }
		};
		{
			IMP_templateInCondition_10_returns_void();
		}
	)")
	{
		templateInCondition<10>();
	}

	return 0;
}
