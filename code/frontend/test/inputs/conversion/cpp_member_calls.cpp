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

struct Base {
  public:
	int a;
	void foo() { a = 5; }
};

struct Derived : Base {
  public:
	int b;
	void bar() { a = 6; b = 6; }
};

struct DerivedSpecifyingFoo : Derived {
  public:
	int c;
	void foo() { a = 7; c = 7; }
};

//struct VirtualBase {
//	virtual void foo() {}
//};
//
//struct VirtualDerived : VirtualBase {
//	virtual void foo() {}
//};

int main() {


	// -------- value types ------------------------------------------------------------------------------------------------------------------------------------
	#pragma test expect_ir(R"(
		def struct IMP_Base {
			a : int<4>;
			function IMP_foo = () -> unit {
				(this).a = 5;
			}
		};
		def struct IMP_Derived : [ public IMP_Base ] {
			b : int<4>;
			function IMP_bar = () -> unit {
				ptr_to_ref(ptr_parent_cast(ptr_from_ref(this), type_lit(IMP_Base))).a = 6;
				(this).b = 6;
			}
		};
		def struct IMP_DerivedSpecifyingFoo : [ public IMP_Derived ] {
			c : int<4>;
			function IMP_foo = () -> unit {
				ptr_to_ref(ptr_parent_cast(ptr_from_ref(this), type_lit(IMP_Base))).a = 7;
				(this).c = 7;
			}
		};
		{
			var ref<IMP_Base,f,f,plain> v0 = IMP_Base::(ref_decl(type_lit(ref<IMP_Base,f,f,plain>)));
			var ref<IMP_Derived,f,f,plain> v1 = IMP_Derived::(ref_decl(type_lit(ref<IMP_Derived,f,f,plain>)));
			var ref<IMP_DerivedSpecifyingFoo,f,f,plain> v2 = IMP_DerivedSpecifyingFoo::(ref_decl(type_lit(ref<IMP_DerivedSpecifyingFoo,f,f,plain>)));
			v0.IMP_foo();
			v1.IMP_bar();
			ref_parent_cast(v1, type_lit(IMP_Base)).IMP_foo();
			v2.IMP_foo();
		}
	)")
	{
		Base b;
		Derived d;
		DerivedSpecifyingFoo dsf;

		// simple call on base class
		b.foo();

		// simple call on derived class
		d.bar();

		// call of inherited function on derived class
		d.foo();

		// call of inherited function on derived class which specifies the same method again
		dsf.foo();
	}

	// -------- pointers ---------------------------------------------------------------------------------------------------------------------------------------
	#pragma test expect_ir(R"(
		def struct IMP_Base {
			a : int<4>;
			function IMP_foo = () -> unit {
				(this).a = 5;
			}
		};
		def struct IMP_Derived : [ public IMP_Base ] {
			b : int<4>;
			function IMP_bar = () -> unit {
				ptr_to_ref(ptr_parent_cast(ptr_from_ref(this), type_lit(IMP_Base))).a = 6;
				(this).b = 6;
			}
		};
		def struct IMP_DerivedSpecifyingFoo : [ public IMP_Derived ] {
			c : int<4>;
			function IMP_foo = () -> unit {
				ptr_to_ref(ptr_parent_cast(ptr_from_ref(this), type_lit(IMP_Base))).a = 7;
				(this).c = 7;
			}
		};
		{
			var ref<IMP_Base,f,f,plain> v0 = IMP_Base::(ref_decl(type_lit(ref<IMP_Base,f,f,plain>)));
			var ref<IMP_Derived,f,f,plain> v1 = IMP_Derived::(ref_decl(type_lit(ref<IMP_Derived,f,f,plain>)));
			var ref<IMP_DerivedSpecifyingFoo,f,f,plain> v2 = IMP_DerivedSpecifyingFoo::(ref_decl(type_lit(ref<IMP_DerivedSpecifyingFoo,f,f,plain>)));
			var ref<ptr<IMP_Base>,f,f,plain> v3 = ptr_from_ref(v0);
			var ref<ptr<IMP_Derived>,f,f,plain> v4 = ptr_from_ref(v1);
			var ref<ptr<IMP_DerivedSpecifyingFoo>,f,f,plain> v5 = ptr_from_ref(v2);
			var ref<ptr<IMP_Base>,f,f,plain> v6 = ptr_parent_cast(ptr_from_ref(v1), type_lit(IMP_Base));
			var ref<ptr<IMP_Base>,f,f,plain> v7 = ptr_parent_cast(ptr_from_ref(v2), type_lit(IMP_Base));
			ptr_to_ref(*v3).IMP_foo();
			ptr_to_ref(*v4).IMP_bar();
			ptr_to_ref(ptr_parent_cast(*v4, type_lit(IMP_Base))).IMP_foo();
			ptr_to_ref(*v5).IMP_foo();
			ptr_to_ref(*v6).IMP_foo();
			ptr_to_ref(*v7).IMP_foo();
		}
	)")
	{
		Base b;
		Derived d;
		DerivedSpecifyingFoo dsf;
		Base* pb = &b;
		Derived* pd = &d;
		DerivedSpecifyingFoo* pdsf = &dsf;
		Base* pb_derived = &d;
		Base* pb_dsf = &dsf;

		// simple call on base class
		pb->foo();

		// simple call on derived class
		pd->bar();

		// call of inherited function on derived class
		pd->foo();

		// call of inherited function on derived class which specifies the same method again
		pdsf->foo();

		// call of inherited function on derived class on a pointer of base-class type
		pb_derived->foo();

		// call of inherited function on derived class which specifies the same method again on a pointer of base-class type
		pb_dsf->foo();
	}

	// -------- references -------------------------------------------------------------------------------------------------------------------------------------
	#pragma test expect_ir(R"(
		def struct IMP_Base {
			a : int<4>;
			function IMP_foo = () -> unit {
				(this).a = 5;
			}
		};
		def struct IMP_Derived : [ public IMP_Base ] {
			b : int<4>;
			function IMP_bar = () -> unit {
				ptr_to_ref(ptr_parent_cast(ptr_from_ref(this), type_lit(IMP_Base))).a = 6;
				(this).b = 6;
			}
		};
		def struct IMP_DerivedSpecifyingFoo : [ public IMP_Derived ] {
			c : int<4>;
			function IMP_foo = () -> unit {
				ptr_to_ref(ptr_parent_cast(ptr_from_ref(this), type_lit(IMP_Base))).a = 7;
				(this).c = 7;
			}
		};
		{
			var ref<IMP_Base,f,f,plain> v0 = IMP_Base::(ref_decl(type_lit(ref<IMP_Base,f,f,plain>)));
			var ref<IMP_Derived,f,f,plain> v1 = IMP_Derived::(ref_decl(type_lit(ref<IMP_Derived,f,f,plain>)));
			var ref<IMP_DerivedSpecifyingFoo,f,f,plain> v2 = IMP_DerivedSpecifyingFoo::(ref_decl(type_lit(ref<IMP_DerivedSpecifyingFoo,f,f,plain>)));
			var ref<IMP_Base,f,f,cpp_ref> v3 = ref_kind_cast(v0, type_lit(cpp_ref));
			var ref<IMP_Derived,f,f,cpp_ref> v4 = ref_kind_cast(v1, type_lit(cpp_ref));
			var ref<IMP_DerivedSpecifyingFoo,f,f,cpp_ref> v5 = ref_kind_cast(v2, type_lit(cpp_ref));
			var ref<IMP_Base,f,f,cpp_ref> v6 = ref_kind_cast(ref_parent_cast(v1, type_lit(IMP_Base)), type_lit(cpp_ref));
			var ref<IMP_Base,f,f,cpp_ref> v7 = ref_kind_cast(ref_parent_cast(v2, type_lit(IMP_Base)), type_lit(cpp_ref));
			ref_kind_cast(v3, type_lit(plain)).IMP_foo();
			ref_kind_cast(v4, type_lit(plain)).IMP_bar();
			ref_kind_cast(ref_parent_cast(v4, type_lit(IMP_Base)), type_lit(plain)).IMP_foo();
			ref_kind_cast(v5, type_lit(plain)).IMP_foo();
			ref_kind_cast(v6, type_lit(plain)).IMP_foo();
			ref_kind_cast(v7, type_lit(plain)).IMP_foo();
		}
	)")
	{
		Base b;
		Derived d;
		DerivedSpecifyingFoo dsf;
		Base& pb = b;
		Derived& pd = d;
		DerivedSpecifyingFoo& pdsf = dsf;
		Base& pb_derived = d;
		Base& pb_dsf = dsf;

		// simple call on base class
		pb.foo();

		// simple call on derived class
		pd.bar();

		// call of inherited function on derived class
		pd.foo();

		// call of inherited function on derived class which specifies the same method again
		pdsf.foo();

		// call of inherited function on derived class on a pointer of base-class type
		pb_derived.foo();

		// call of inherited function on derived class which specifies the same method again on a pointer of base-class type
		pb_dsf.foo();
	}

//	// -------- plain virtual types --------
//	VirtualBase vb;
//	VirtualDerived vd;
//
//	//simple call on base class
//	vb.foo();
//
//	//simple call on derived class
//	vd.foo();
//
//
//	// -------- virtual types with pointers --------
//	VirtualBase* pvb = &vb;
//	VirtualDerived* pvd = &vd;
//	VirtualBase* pvb_base = &vd;
//
//	//call of function on base class on a pointer
//	pvb->foo();
//
//	//call of inherited function on derived class on a pointer
//	pvd->foo();
//
//	//call of inherited function on derived class on a pointer of base-class type
//	pvb_base->foo();

	return 0;
}
