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
	; // this is required because of the clang compound source location bug


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
			var ref<IMP_Base,f,f,cpp_ref> v3 = v0;
			var ref<IMP_Derived,f,f,cpp_ref> v4 = v1;
			var ref<IMP_DerivedSpecifyingFoo,f,f,cpp_ref> v5 = v2;
			var ref<IMP_Base,f,f,cpp_ref> v6 = ref_parent_cast(v1, type_lit(IMP_Base));
			var ref<IMP_Base,f,f,cpp_ref> v7 = ref_parent_cast(v2, type_lit(IMP_Base));
			v3.IMP_foo();
			v4.IMP_bar();
			ref_parent_cast(v4, type_lit(IMP_Base)).IMP_foo();
			v5.IMP_foo();
			v6.IMP_foo();
			v7.IMP_foo();
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
