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

struct FMember {
	int t;
	FMember() = default;
	FMember(int i) {};
};
struct Base {};
struct Base2 {};

int global;
struct F : Base, Base2 {
	bool b;
	int i = 5;
	long l = 6;
	float f;
	int g = global;
	FMember t1{6};
	FMember t2;
};


int main() {

	int magic;

	// Check initialization of members in the absence of constructors
	// This test will ensure that we have a correctly filled body for the default ctor, in case we didn't specify any constructor in class F.
	// Note that also the other defaulted members (copy/move ctors/assignment operators and the dtor) will have code in their body which we check here
	#pragma test expect_ir(R"(
		def struct IMP_Base {};
		def struct IMP_Base2 {};
		def struct IMP_FMember {
			t : int<4>;
			ctor() = default;
			ctor function (v1 : ref<int<4>,f,f,plain>) { }
		};
		def struct IMP_F : [ public IMP_Base,public IMP_Base2 ] {
			b : bool;
			i : int<4>;
			l : int<8>;
			f : real<4>;
			g : int<4>;
			t1 : IMP_FMember;
			t2 : IMP_FMember;
			ctor function() {
				lit(""INSIEME_DEFAULTED"":ref<array<char,18u>,t,f>);
				IMP_Base::(ref_parent_cast(this, type_lit(IMP_Base)));
				IMP_Base2::(ref_parent_cast(this, type_lit(IMP_Base2)));
				<ref<int<4>,f,f,plain>>(this.i) {5};
				<ref<int<8>,f,f,plain>>(this.l) {num_cast(6, type_lit(int<8>))};
				<ref<int<4>,f,f,plain>>(this.g) {*lit("global" : ref<int<4>,f,f,plain>)};
				IMP_FMember::(this.t1, 6);
				IMP_FMember::(this.t2);
			}
			ctor function(other : ref<IMP_F,t,f,cpp_ref>) {
				lit(""INSIEME_DEFAULTED"":ref<array<char,18u>,t,f>);
				IMP_Base::(ref_parent_cast(this, type_lit(IMP_Base)), ref_parent_cast(other, type_lit(IMP_Base)));
				IMP_Base2::(ref_parent_cast(this, type_lit(IMP_Base2)), ref_parent_cast(other, type_lit(IMP_Base2)));
				<ref<bool,f,f,plain>>(this.b) {*other.b};
				<ref<int<4>,f,f,plain>>(this.i) {*other.i};
				<ref<int<8>,f,f,plain>>(this.l) {*other.l};
				<ref<real<4>,f,f,plain>>(this.f) {*other.f};
				<ref<int<4>,f,f,plain>>(this.g) {*other.g};
				IMP_FMember::(this.t1, ref_kind_cast(other.t1, type_lit(cpp_ref)));
				IMP_FMember::(this.t2, ref_kind_cast(other.t2, type_lit(cpp_ref)));
			}
			ctor function(other : ref<IMP_F,f,f,cpp_rref>) {
				lit(""INSIEME_DEFAULTED"":ref<array<char,18u>,t,f>);
				IMP_Base::(ref_parent_cast(this, type_lit(IMP_Base)), ref_parent_cast(other, type_lit(IMP_Base)));
				IMP_Base2::(ref_parent_cast(this, type_lit(IMP_Base2)), ref_parent_cast(other, type_lit(IMP_Base2)));
				<ref<bool,f,f,plain>>(this.b) {*other.b};
				<ref<int<4>,f,f,plain>>(this.i) {*other.i};
				<ref<int<8>,f,f,plain>>(this.l) {*other.l};
				<ref<real<4>,f,f,plain>>(this.f) {*other.f};
				<ref<int<4>,f,f,plain>>(this.g) {*other.g};
				IMP_FMember::(this.t1, ref_kind_cast(other.t1, type_lit(cpp_rref)));
				IMP_FMember::(this.t2, ref_kind_cast(other.t2, type_lit(cpp_rref)));
			}
			dtor function() {
				lit(""INSIEME_DEFAULTED"":ref<array<char,18u>,t,f>);
				IMP_FMember::~(this.t2);
				IMP_FMember::~(this.t1);
				IMP_Base2::~(ref_parent_cast(this, type_lit(IMP_Base2)));
				IMP_Base::~(ref_parent_cast(this, type_lit(IMP_Base)));
			}
			function IMP__operator_assign_ = (other : ref<IMP_F,t,f,cpp_ref>) -> ref<IMP_F,f,f,cpp_ref> {
				lit(""INSIEME_DEFAULTED"":ref<array<char,18u>,t,f>);
				ref_parent_cast(this, type_lit(IMP_Base)).IMP__operator_assign_(ref_parent_cast(other, type_lit(IMP_Base)));
				ref_parent_cast(this, type_lit(IMP_Base2)).IMP__operator_assign_(ref_parent_cast(other, type_lit(IMP_Base2)));
				this.b = *other.b;
				this.i = *other.i;
				this.l = *other.l;
				this.f = *other.f;
				this.g = *other.g;
				this.t1.IMP__operator_assign_(ref_kind_cast(other.t1, type_lit(cpp_ref)));
				this.t2.IMP__operator_assign_(ref_kind_cast(other.t2, type_lit(cpp_ref)));
				return ref_kind_cast(this, type_lit(cpp_ref));
			}
			function IMP__operator_assign_ = (other : ref<IMP_F,f,f,cpp_rref>) -> ref<IMP_F,f,f,cpp_ref> {
				lit(""INSIEME_DEFAULTED"":ref<array<char,18u>,t,f>);
				ref_parent_cast(this, type_lit(IMP_Base)).IMP__operator_assign_(ref_parent_cast(other, type_lit(IMP_Base)));
				ref_parent_cast(this, type_lit(IMP_Base2)).IMP__operator_assign_(ref_parent_cast(other, type_lit(IMP_Base2)));
				this.b = *other.b;
				this.i = *other.i;
				this.l = *other.l;
				this.f = *other.f;
				this.g = *other.g;
				this.t1.IMP__operator_assign_(ref_kind_cast(other.t1, type_lit(cpp_rref)));
				this.t2.IMP__operator_assign_(ref_kind_cast(other.t2, type_lit(cpp_rref)));
				return ref_kind_cast(this, type_lit(cpp_ref));
			}
		};
		{
			var ref<IMP_F,f,f,plain> v0 = IMP_F::(ref_decl(type_lit(ref<IMP_F,f,f,plain>)));
		}
	)")
	{
		F f;
	}
}
