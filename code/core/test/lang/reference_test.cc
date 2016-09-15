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

#include <gtest/gtest.h>

#include "insieme/core/lang/reference.h"
#include "insieme/core/test/test_utils.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace lang {

	TEST(Reference, SemanticChecks) {
		NodeManager nm;
		auto& ext = nm.getLangExtension<ReferenceExtension>();
		semanticCheckSecond(ext.getSymbols());
	}


	TEST(Reference, IsReference) {
		NodeManager nm;
		IRBuilder builder(nm);

		auto A = builder.parseType("A");
		EXPECT_TRUE(isReference(ReferenceType::create(A)));
		EXPECT_TRUE(isReference(ReferenceType::create(A, false, true)));

		EXPECT_FALSE(isReference(builder.parseType("A")));
		EXPECT_TRUE(isReference(builder.parseType("ref<A>")));
		EXPECT_TRUE(isReference(builder.parseType("ref<A,f,t>")));
		EXPECT_TRUE(isReference(builder.parseType("ref<A,f,t,plain>")));
		EXPECT_TRUE(isReference(builder.parseType("ref<A,f,t,cpp_ref>")));
		EXPECT_TRUE(isReference(builder.parseType("ref<A,f,t,cpp_rref>")));

		EXPECT_FALSE(isReference(builder.parseType("ref<A,f,t,bla>")));
		EXPECT_FALSE(isReference(builder.parseType("ref<A,c,t,cpp_rref>")));
		EXPECT_FALSE(isReference(builder.parseType("ref<A,f,c,cpp_rref>")));


		EXPECT_TRUE(isPlainReference(builder.parseType("ref<A,f,t,plain>")));
		EXPECT_FALSE(isPlainReference(builder.parseType("ref<A,f,t,cpp_ref>")));
		EXPECT_FALSE(isPlainReference(builder.parseType("ref<A,f,t,cpp_rref>")));

		EXPECT_FALSE(isCppReference(builder.parseType("ref<A,f,t,plain>")));
		EXPECT_TRUE(isCppReference(builder.parseType("ref<A,f,t,cpp_ref>")));
		EXPECT_FALSE(isCppReference(builder.parseType("ref<A,f,t,cpp_rref>")));

		EXPECT_FALSE(isCppRValueReference(builder.parseType("ref<A,f,t,plain>")));
		EXPECT_FALSE(isCppRValueReference(builder.parseType("ref<A,f,t,cpp_ref>")));
		EXPECT_TRUE(isCppRValueReference(builder.parseType("ref<A,f,t,cpp_rref>")));

	}

	TEST(Reference, MemoryManagement) {

		NodeManager nm;
		IRBuilder builder(nm);
		auto& ext = nm.getLangExtension<ReferenceExtension>();

		auto A = builder.parseType("A");
		auto Tlit = builder.getTypeLiteral(A);
		auto stack = ext.getMemLocStack();
		auto heap = ext.getMemLocHeap();
		auto value = builder.literal(A, "dummy");

		IRBuilder::EagerDefinitionMap symbols;
		symbols["v"] = value;
		symbols["T"] = Tlit;
		symbols["dummy"] = builder.literal(ReferenceType::create(A), "dummy");

		// Ref alloc
		auto allocS = builder.callExpr(ext.getRefAlloc(), Tlit, stack);
		auto allocH = builder.callExpr(ext.getRefAlloc(), Tlit, heap);
		EXPECT_EQ(allocS->getType(), builder.parseType("ref<A, f, f>"));
		EXPECT_EQ(allocH->getType(), builder.parseType("ref<A, f, f>"));
		EXPECT_EQ(allocH->getType(), allocS->getType());

		// free mem
		EXPECT_TRUE(builder.callExpr(ext.getRefDelete(), builder.literal(ReferenceType::create(A), "dummy")));
		EXPECT_TRUE(builder.parseExpr("ref_delete(dummy)", symbols));

		// stack alloc
		EXPECT_TRUE(builder.callExpr(ext.getRefTemp(), Tlit));
		EXPECT_EQ(builder.callExpr(ext.getRefTemp(), Tlit)->getType(),  builder.parseType("ref<A,f,f>"));
		EXPECT_TRUE(builder.parseExpr("ref_temp(T)", symbols));
		EXPECT_EQ(builder.parseExpr("ref_temp(T)", symbols)->getType(), builder.parseType("ref<A,f,f>"));

		// stack + init
		EXPECT_TRUE(builder.callExpr(ext.getRefTempInit(), value));
		EXPECT_EQ(builder.callExpr(ext.getRefTempInit(), value)->getType(),  builder.parseType("ref<A,f,f>"));
		EXPECT_TRUE(builder.parseExpr("ref_temp_init(v)", symbols));
		EXPECT_EQ(builder.parseExpr("ref_temp_init(v)", symbols)->getType(),  builder.parseType("ref<A,f,f>"));

		// heap
		EXPECT_TRUE(builder.callExpr(ext.getRefNew(), Tlit));
		EXPECT_EQ(builder.callExpr(ext.getRefNew(), Tlit)->getType(),  builder.parseType("ref<A,f,f>"));
		EXPECT_TRUE(builder.parseExpr("ref_new(T)", symbols));
		EXPECT_EQ(builder.parseExpr("ref_new(T)", symbols)->getType(), builder.parseType("ref<A,f,f>"));

		// heap + init
		EXPECT_TRUE(builder.callExpr(ext.getRefNewInit(), value));
		EXPECT_EQ(builder.callExpr(ext.getRefNewInit(), value)->getType(),  builder.parseType("ref<A,f,f>"));
		EXPECT_TRUE(builder.parseExpr("ref_new_init(v)", symbols));
		EXPECT_EQ(builder.parseExpr("ref_new_init(v)", symbols)->getType(),  builder.parseType("ref<A,f,f>"));

	}

	TEST(Reference, RefOfFunction) {

		NodeManager nm;
		IRBuilder builder(nm);
		auto& ext = nm.getLangExtension<ReferenceExtension>();

		auto f = builder.parseExpr("() -> unit { } ");
		ASSERT_TRUE(f);
		auto fType = f->getType();

		EXPECT_TRUE(builder.callExpr(ext.getRefOfFunction(), f));
		EXPECT_EQ(builder.callExpr(ext.getRefOfFunction(), f)->getType(), ReferenceType::create(fType, true));

	}

	TEST(Reference, Access) {

		NodeManager nm;
		IRBuilder builder(nm);
		auto& ext = nm.getLangExtension<ReferenceExtension>();

		auto A = builder.parseType("A");
		auto refExpr = builder.literal(ReferenceType::create(A), "Refdummy");
		auto constRefExpr = builder.literal(ReferenceType::create(A, true), "ConstRefdummy");
		auto volatileRefExpr = builder.literal(ReferenceType::create(A, false, true), "VolRefdummy");
		auto valueExpr = builder.literal(A, "dummy");

		// deref
		EXPECT_EQ(builder.callExpr(ext.getRefDeref(), refExpr)->getType(), A);
		EXPECT_EQ(builder.callExpr(ext.getRefDeref(), constRefExpr)->getType(), A);
		EXPECT_EQ(builder.callExpr(ext.getRefDeref(), volatileRefExpr)->getType(), A);

		// assign
		EXPECT_TRUE(builder.callExpr(ext.getRefAssign(), refExpr, valueExpr));
		ASSERT_DEATH_IF_SUPPORTED( builder.callExpr(ext.getRefAssign(), constRefExpr, valueExpr), ".*");
		// dies without message? Release nothing, debug assert: regex takes everithing
		EXPECT_TRUE(builder.callExpr(ext.getRefAssign(), volatileRefExpr, valueExpr));

	}

	TEST(Reference, Cast) {

		NodeManager nm;
		IRBuilder builder(nm);
		auto& ext = nm.getLangExtension<ReferenceExtension>();

		auto A = builder.parseType("A");
		auto B = builder.parseType("B");

		auto refExpr = builder.literal(ReferenceType::create(A), "Refdummy");
		auto constRefExpr = builder.literal(ReferenceType::create(A, true), "ConstRefdummy");
		auto volatileRefExpr = builder.literal(ReferenceType::create(A, false, true), "VolRefdummy");
		auto valueExpr = builder.literal(A, "dummy");

		// Cast inner type
		EXPECT_EQ(builder.callExpr(ext.getRefReinterpret(), refExpr, builder.getTypeLiteral(B))->getType(), ReferenceType::create(B));
		EXPECT_EQ(builder.callExpr(ext.getRefReinterpret(), constRefExpr, builder.getTypeLiteral(B))->getType(), ReferenceType::create(B, true));
		EXPECT_EQ(builder.callExpr(ext.getRefReinterpret(), volatileRefExpr, builder.getTypeLiteral(B))->getType(), ReferenceType::create(B, false, true));

		// Qualification change
		auto t = builder.getTypeLiteral(builder.parseType("t"));
		auto f = builder.getTypeLiteral(builder.parseType("f"));
		auto plain = builder.getTypeLiteral(builder.parseType("plain"));
		auto cppref = builder.getTypeLiteral(builder.parseType("cpp_ref"));
		auto cpprref = builder.getTypeLiteral(builder.parseType("cpp_rref"));

		// cast ref  expr
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, t, t, plain)->getType(), ReferenceType::create(A, true, true,   ReferenceType::Kind::Plain));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, t, f, plain)->getType(), ReferenceType::create(A, true, false,  ReferenceType::Kind::Plain));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, f, t, plain)->getType(), ReferenceType::create(A, false, true,  ReferenceType::Kind::Plain));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, f, f, plain)->getType(), ReferenceType::create(A, false, false, ReferenceType::Kind::Plain));

		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, t, t, cppref)->getType(), ReferenceType::create(A, true, true,   ReferenceType::Kind::CppReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, t, f, cppref)->getType(), ReferenceType::create(A, true, false,  ReferenceType::Kind::CppReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, f, t, cppref)->getType(), ReferenceType::create(A, false, true,  ReferenceType::Kind::CppReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, f, f, cppref)->getType(), ReferenceType::create(A, false, false, ReferenceType::Kind::CppReference));

		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, t, t, cpprref)->getType(), ReferenceType::create(A, true, true,   ReferenceType::Kind::CppRValueReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, t, f, cpprref)->getType(), ReferenceType::create(A, true, false,  ReferenceType::Kind::CppRValueReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, f, t, cpprref)->getType(), ReferenceType::create(A, false, true,  ReferenceType::Kind::CppRValueReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), refExpr, f, f, cpprref)->getType(), ReferenceType::create(A, false, false, ReferenceType::Kind::CppRValueReference));

		// cast const ref expr
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, t, t, plain)->getType(), ReferenceType::create(A, true, true,   ReferenceType::Kind::Plain));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, t, f, plain)->getType(), ReferenceType::create(A, true, false,  ReferenceType::Kind::Plain));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, f, t, plain)->getType(), ReferenceType::create(A, false, true,  ReferenceType::Kind::Plain));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, f, f, plain)->getType(), ReferenceType::create(A, false, false, ReferenceType::Kind::Plain));

		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, t, t, cppref)->getType(), ReferenceType::create(A, true, true,   ReferenceType::Kind::CppReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, t, f, cppref)->getType(), ReferenceType::create(A, true, false,  ReferenceType::Kind::CppReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, f, t, cppref)->getType(), ReferenceType::create(A, false, true,  ReferenceType::Kind::CppReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, f, f, cppref)->getType(), ReferenceType::create(A, false, false, ReferenceType::Kind::CppReference));

		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, t, t, cpprref)->getType(), ReferenceType::create(A, true, true,   ReferenceType::Kind::CppRValueReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, t, f, cpprref)->getType(), ReferenceType::create(A, true, false,  ReferenceType::Kind::CppRValueReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, f, t, cpprref)->getType(), ReferenceType::create(A, false, true,  ReferenceType::Kind::CppRValueReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), constRefExpr, f, f, cpprref)->getType(), ReferenceType::create(A, false, false, ReferenceType::Kind::CppRValueReference));

		// cast vol ref expr
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, t, t, plain)->getType(), ReferenceType::create(A, true, true,   ReferenceType::Kind::Plain));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, t, f, plain)->getType(), ReferenceType::create(A, true, false,  ReferenceType::Kind::Plain));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, f, t, plain)->getType(), ReferenceType::create(A, false, true,  ReferenceType::Kind::Plain));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, f, f, plain)->getType(), ReferenceType::create(A, false, false, ReferenceType::Kind::Plain));

		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, t, t, cppref)->getType(), ReferenceType::create(A, true, true,   ReferenceType::Kind::CppReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, t, f, cppref)->getType(), ReferenceType::create(A, true, false,  ReferenceType::Kind::CppReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, f, t, cppref)->getType(), ReferenceType::create(A, false, true,  ReferenceType::Kind::CppReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, f, f, cppref)->getType(), ReferenceType::create(A, false, false, ReferenceType::Kind::CppReference));

		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, t, t, cpprref)->getType(), ReferenceType::create(A, true, true,   ReferenceType::Kind::CppRValueReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, t, f, cpprref)->getType(), ReferenceType::create(A, true, false,  ReferenceType::Kind::CppRValueReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, f, t, cpprref)->getType(), ReferenceType::create(A, false, true,  ReferenceType::Kind::CppRValueReference));
		EXPECT_EQ(builder.callExpr(ext.getRefCast(), volatileRefExpr, f, f, cpprref)->getType(), ReferenceType::create(A, false, false, ReferenceType::Kind::CppRValueReference));

		// failtest
		auto somethingElse = builder.getTypeLiteral(builder.parseType("something_not_valid"));
		EXPECT_FALSE(isReference(builder.callExpr(ext.getRefCast(), refExpr, f, f, somethingElse)->getType()));
		EXPECT_FALSE(isReference(builder.callExpr(ext.getRefCast(), refExpr, f, somethingElse, plain)->getType()));
		EXPECT_FALSE(isReference(builder.callExpr(ext.getRefCast(), refExpr, somethingElse, somethingElse, plain)->getType()));

		// Const cast specialization
		EXPECT_EQ(builder.callExpr(ext.getRefConstCast(), refExpr, t)->getType(), ReferenceType::create(A, true, false));
		EXPECT_EQ(builder.callExpr(ext.getRefConstCast(), refExpr, f)->getType(), ReferenceType::create(A, false, false));

		EXPECT_EQ(builder.callExpr(ext.getRefConstCast(), constRefExpr, t)->getType(), ReferenceType::create(A, true, false));
		EXPECT_EQ(builder.callExpr(ext.getRefConstCast(), constRefExpr, f)->getType(), ReferenceType::create(A, false, false));

		EXPECT_EQ(builder.callExpr(ext.getRefConstCast(), volatileRefExpr, t)->getType(), ReferenceType::create(A, true, true));
		EXPECT_EQ(builder.callExpr(ext.getRefConstCast(), volatileRefExpr, f)->getType(), ReferenceType::create(A, false, true));

		// volatile cast specialization
		EXPECT_EQ(builder.callExpr(ext.getRefVolatileCast(), refExpr, t)->getType(), ReferenceType::create(A, false, true));
		EXPECT_EQ(builder.callExpr(ext.getRefVolatileCast(), refExpr, f)->getType(), ReferenceType::create(A, false, false));

		EXPECT_EQ(builder.callExpr(ext.getRefVolatileCast(), constRefExpr, t)->getType(), ReferenceType::create(A, true, true));
		EXPECT_EQ(builder.callExpr(ext.getRefVolatileCast(), constRefExpr, f)->getType(), ReferenceType::create(A, true, false));

		EXPECT_EQ(builder.callExpr(ext.getRefVolatileCast(), volatileRefExpr, t)->getType(), ReferenceType::create(A, false, true));
		EXPECT_EQ(builder.callExpr(ext.getRefVolatileCast(), volatileRefExpr, f)->getType(), ReferenceType::create(A, false, false));

	}

	TEST(Reference, SubReferencing) {

//		NodeManager nm;
//		IRBuilder builder(nm);
//		auto& ext = nm.getLangExtension<ReferenceExtension>();
//
//		auto A = builder.parseType("A");
//		auto B = builder.parseType("B");
//
//		auto refExpr = builder.literal(ReferenceType::create(A), "Refdummy");
//		auto constRefExpr = builder.literal(ReferenceType::create(A, true), "ConstRefdummy");
//		auto volatileRefExpr = builder.literal(ReferenceType::create(A, false, true), "VolRefdummy");
//		auto valueExpr = builder.literal(A, "dummy");

		std::cout << " This module requires a better testing, which may make use of more types. is better to do it appart" << std::endl;
//
//		// -- sub-referencing --
//
//		/**
//		 * The narrow operation is obtaining a reference to a sub-object within a referenced object.
//		 */
//		LANG_EXT_LITERAL(RefNarrow, "ref_narrow", "(ref<'a,'c,'v,'k>, datapath<'a,'b>) -> ref<'b,'c,'v,'k>")
//
//		/**
//		 * The expand operation is the inverse operation of the narrow operation.
//		 */
//		LANG_EXT_LITERAL(RefExpand, "ref_expand", "(ref<'b,'c,'v,'k>, datapath<'a,'b>) -> ref<'a,'c,'v,'k>")
//
//		/**
//		 * A derived operator providing access to an element in an array.
//		 */
//		LANG_EXT_DERIVED(RefArrayElement, "(r : ref<array<'a,'s>,'c,'v,plain>, i : int<8>) -> ref<'a,'c,'v, plain> { return ref_narrow(r, dp_element(dp_root(type_lit(array<'a,'s>)), i)); }")
//
//		/**
//		 * A derived reference navigation operator providing access to a member of a struct / union.
//		 */
//		LANG_EXT_DERIVED(RefMemberAccess, "(r : ref<'a,'c,'v,'k>, name : identifier, type : type<'b>) -> ref<'b,'c,'v, plain> { return ref_kind_cast(ref_narrow(r, dp_member(dp_root(type_lit('a)), name, type)), type_lit(plain)); }")
//
//		/**
//		 * A derived reference navigation operator providing access to a components of a tuple.
//		 */
//		LANG_EXT_DERIVED(RefComponentAccess, "(r : ref<'a,'c,'v,'k>, pos : uint<8>, type : type<'b>) -> ref<'b,'c,'v,'k> { return ref_narrow(r, dp_component(dp_root(type_lit('a)), pos, type)); }")
//
//		/**
//		 * A derived reference-navigation operation providing an array view on a scalar.
//		 */
//		LANG_EXT_DERIVED(RefScalarToRefArray, "(a : ref<'a,'c,'v,plain>) -> ref<array<'a>,'c,'v,plain> { return ref_expand(a, dp_element(dp_root(type_lit(array<'a>)), 0u)); }")
//
	}

	TEST(Reference, Null) {

		NodeManager nm;
		IRBuilder builder(nm);
		auto& ext = nm.getLangExtension<ReferenceExtension>();

		auto A = builder.getTypeLiteral(builder.parseType("A"));
		auto t = builder.getTypeLiteral(builder.parseType("t"));
		auto f = builder.getTypeLiteral(builder.parseType("f"));

		// Null
		EXPECT_TRUE(isReference(builder.callExpr(ext.getRefNull(), A, t, t)->getType()));
		EXPECT_TRUE(isReference(builder.callExpr(ext.getRefNull(), A, t, f)->getType()));
		EXPECT_TRUE(isReference(builder.callExpr(ext.getRefNull(), A, f, t)->getType()));
		EXPECT_TRUE(isReference(builder.callExpr(ext.getRefNull(), A, f, f)->getType()));

	}

	TEST(Reference, Operators) {

		// NOTE: nothing really to test here, is it?


//		// -- operators --
//
//		/**
//		 * An operator to compare two references on equality.
//		 */
//		LANG_EXT_LITERAL(RefEqual, "ref_eq", "(ref<'a1,'c1,'v1,'k1>, ref<'a2,'c2,'v2,'k2>) -> bool")
//
//		/**
//		 * An operator to compare two references for inequality.
//		 */
//		LANG_EXT_DERIVED(RefNotEqual, "(a : ref<'a1,'c1,'v1,'k1>, b : ref<'a2,'c2,'v2,'k2>) -> bool { return !ref_eq(a, b); }")
//
//		/**
//		 * A generic pre-order increment operator.
//		 */
//		LANG_EXT_DERIVED(GenPreInc, "(v : ref<'a,f,'v,'k>)->'a { v=*v+lit(\"1\":'a); return *v; }")
//
//		/**
//		 * A generic post-order increment operator.
//		 */
//		LANG_EXT_DERIVED(GenPostInc, "(v : ref<'a,f,'v,'k>)->'a { auto tmp=*v; v=*v+lit(\"1\":'a); return tmp; }")
//
//		/**
//		 * A generic pre-order decrement operator.
//		 */
//		LANG_EXT_DERIVED(GenPreDec, "(v : ref<'a,f,'v,'k>)->'a { v=*v-lit(\"1\":'a); return *v; }")
//
//		/**
//		 * A generic post-order decrement operator.
//		 */
//		LANG_EXT_DERIVED(GenPostDec, "(v : ref<'a,f,'v,'k>)->'a { auto tmp=*v; v=*v-lit(\"1\":'a); return tmp; }")
//
	}

	TEST(Reference, Utilities) {

		NodeManager nm;
		IRBuilder builder(nm);
//		auto& ext = nm.getLangExtension<ReferenceExtension>();

		auto A = builder.parseType("A");

		auto t = builder.getTypeLiteral(builder.parseType("t"));
		auto f = builder.getTypeLiteral(builder.parseType("f"));
		auto plain = builder.getTypeLiteral(builder.parseType("plain"));
		auto cppref = builder.getTypeLiteral(builder.parseType("cpp_ref"));
		auto cpprref = builder.getTypeLiteral(builder.parseType("cpp_rref"));


		auto a = ReferenceType::create(A, t, t);
		auto b = ReferenceType::create(A, t, t);
		auto c = ReferenceType::create(A, t, t);
		auto d = ReferenceType::create(A, t, t);

		auto a2 = ReferenceType::create(A, t, t, ReferenceType::Kind::CppReference);
		auto b2 = ReferenceType::create(A, t, f, ReferenceType::Kind::CppReference);
		auto c2 = ReferenceType::create(A, f, t, ReferenceType::Kind::CppReference);
		auto d2 = ReferenceType::create(A, f, f, ReferenceType::Kind::CppReference);

		auto a3 = ReferenceType::create(A, t, t, ReferenceType::Kind::CppRValueReference);
		auto b3 = ReferenceType::create(A, t, f, ReferenceType::Kind::CppRValueReference);
		auto c3 = ReferenceType::create(A, f, t, ReferenceType::Kind::CppRValueReference);
		auto d3 = ReferenceType::create(A, f, f, ReferenceType::Kind::CppRValueReference);

		EXPECT_TRUE( isReference(a));   EXPECT_TRUE(isReferenceTo(a, A));
		EXPECT_TRUE( isReference(b));   EXPECT_TRUE(isReferenceTo(b, A));
		EXPECT_TRUE( isReference(c));   EXPECT_TRUE(isReferenceTo(c, A));
		EXPECT_TRUE( isReference(d));   EXPECT_TRUE(isReferenceTo(d, A));

		EXPECT_TRUE( isReference(a2));   EXPECT_TRUE(isReferenceTo(a2, A));
		EXPECT_TRUE( isReference(b2));   EXPECT_TRUE(isReferenceTo(b2, A));
		EXPECT_TRUE( isReference(c2));   EXPECT_TRUE(isReferenceTo(c2, A));
		EXPECT_TRUE( isReference(d2));   EXPECT_TRUE(isReferenceTo(d2, A));

		EXPECT_TRUE( isReference(a3));   EXPECT_TRUE(isReferenceTo(a3, A));
		EXPECT_TRUE( isReference(b3));   EXPECT_TRUE(isReferenceTo(b3, A));
		EXPECT_TRUE( isReference(c3));   EXPECT_TRUE(isReferenceTo(c3, A));
		EXPECT_TRUE( isReference(d3));   EXPECT_TRUE(isReferenceTo(d3, A));


		EXPECT_TRUE( isPlainReference(a));
		EXPECT_TRUE( isPlainReference(b));
		EXPECT_TRUE( isPlainReference(c));
		EXPECT_TRUE( isPlainReference(d));

		EXPECT_FALSE( isPlainReference(a2));
		EXPECT_FALSE( isPlainReference(b2));
		EXPECT_FALSE( isPlainReference(c2));
		EXPECT_FALSE( isPlainReference(d2));

		EXPECT_FALSE( isPlainReference(a3));
		EXPECT_FALSE( isPlainReference(b3));
		EXPECT_FALSE( isPlainReference(c3));
		EXPECT_FALSE( isPlainReference(d3));

		EXPECT_FALSE(isCppReference(a));
		EXPECT_FALSE(isCppReference(b));
		EXPECT_FALSE(isCppReference(c));
		EXPECT_FALSE(isCppReference(d));

		EXPECT_TRUE( isCppReference(a2));
		EXPECT_TRUE( isCppReference(b2));
		EXPECT_TRUE( isCppReference(c2));
		EXPECT_TRUE( isCppReference(d2));

		EXPECT_FALSE(isCppReference(a3));
		EXPECT_FALSE(isCppReference(b3));
		EXPECT_FALSE(isCppReference(c3));
		EXPECT_FALSE(isCppReference(d3));

		EXPECT_FALSE(isCppRValueReference(a));
		EXPECT_FALSE(isCppRValueReference(b));
		EXPECT_FALSE(isCppRValueReference(c));
		EXPECT_FALSE(isCppRValueReference(d));

		EXPECT_FALSE(isCppRValueReference(a2));
		EXPECT_FALSE(isCppRValueReference(b2));
		EXPECT_FALSE(isCppRValueReference(c2));
		EXPECT_FALSE(isCppRValueReference(d2));

		EXPECT_TRUE(isCppRValueReference(a3));
		EXPECT_TRUE(isCppRValueReference(b3));
		EXPECT_TRUE(isCppRValueReference(c3));
		EXPECT_TRUE(isCppRValueReference(d3));

	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
