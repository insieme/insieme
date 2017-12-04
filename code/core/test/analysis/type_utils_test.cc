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

#include <vector>

#include <type_traits>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace core {
namespace analysis {

	bool hasNoFreeTypeVariables(const core::TypePtr& type) {
		return !hasFreeTypeVariables(type);
	}

	TEST(FreeTypeVariables, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// test some cases with free variables
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("'a"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("set<'a>"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("array<'a,1>"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("struct { data : 'a; }"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("struct { x : struct { data : 'a; }; }"));

		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("int<4>"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("set<int<4>>"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("array<int<4>,1>"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("struct { data : int<4>; }"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("struct { x : struct { data : int<4>; }; }"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("('a)->'a"));
	}

	TEST(TrivialType, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("int<4>")));
	}

	TEST(TrivialType, Reference) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("ref<int<4>>")));
	}

	TEST(ArrayOfTrivialType, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("array<int<4>,1>")));
	}

	TEST(ArrayOfNonTrivialType, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType("array<struct s { ctor() { return; } },1u>")));
	}

	TEST(TrivialStruct, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("struct class {}")));
	}

	TEST(TrivialUnion, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("union { data: int<4>; }")));
	}

	TEST(TrivialMember, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		// a struct with a trivial member must also be trivial, iff no user-defined ctor is in place
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  data: int<4>;"
			"}")));
	}

	TEST(TrivialMember, Reference) {
		NodeManager manager;
		IRBuilder builder(manager);
		// a member of type reference is non-trivial
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  data: ref<'a,f,f>;"
			"}")));
	}

	TEST(NonTrivialMember, Reference) {
		NodeManager manager;
		IRBuilder builder(manager);
		// a member of type reference without initialization is non-trivial
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  data: ref<'a,f,f,cpp_ref>;"
			"}")));
	}

	TEST(TrivialMemberNested, Reference) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  data: struct {"
			"    data: ref<'a,f,f>;"
			"  };"
			"}")));
	}

	TEST(NonTrivialMemberNested, Reference) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  data: struct {"
			"    data: ref<'a,f,f,cpp_ref>;"
			"  };"
			"}")));
	}

	TEST(NonVirtualLambda, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda fn = () -> unit { }"
			"}")));
	}

	TEST(VirtualLambda, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  virtual lambda fn = () -> unit { }"
			"}")));
	}

	TEST(PureVirtualLambda, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  pure virtual fn : () -> unit"
			"}")));
	}

	TEST(VirtualDestructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);

		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  dtor virtual() {}"
			"}")));
	}

	TEST(Constructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() { return; }"
			"}")));
	}

	TEST(Constructor, UserDefinedWithParam) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor(i : int<4>) { return; }"
			"}")));
	}

	TEST(Constructor, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() = default;"
			"}")));
	}

	TEST(CopyConstructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor(other: ref<class,t,f,cpp_ref>) { return; }"
			"}")));
	}

	TEST(CopyConstructor, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() = default;"
			"  ctor(other: ref<class,t,f,cpp_ref>) = default;"
			"}")));
	}

	TEST(MoveConstructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor(other: ref<class,f,f,cpp_rref>) { return; }"
			"}")));
	}

	TEST(MoveConstructor, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() = default;"
			"  ctor(other: ref<class,f,f,cpp_rref>) = default;"
			"}")));
	}

	TEST(Constructors, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() { return; }"
			"  ctor(other: ref<class,t,f,cpp_ref>) { return; }"
			"  ctor(other: ref<class,f,f,cpp_rref>) { return; }"
			"}")));
	}

	TEST(Constructors, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() = default;"
			"  ctor(other: ref<class,t,f,cpp_ref>) = default;"
			"  ctor(other: ref<class,f,f,cpp_rref>) = default;"
			"}")));
	}

	TEST(NonTrivialConstructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() { return; }"
			"}")));
	}

	TEST(TrivialStructWithTrivialBase, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"let base = struct base_class {} in struct class : [public base] {}")));
	}

	TEST(TrivialStructWithNonTrivialBase, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  ctor() { return; }"
			"} in struct class : [public base] {}")));
	}

	TEST(CopyAssignment, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> { return; }"
			"}")));
	}

	TEST(CopyAssignment, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> = default;"
			"}")));
	}

	TEST(MoveAssignment, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> { return; }"
			"}")));
	}

	TEST(MoveAssignment, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> = default;"
			"}")));
	}

	TEST(ConstructorsAndAssignments, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() = default;"
			"  ctor(other: ref<class,t,f,cpp_ref>) = default;"
			"  ctor(other: ref<class,f,f,cpp_rref>) = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> = default;"
			"}")));
	}

	TEST(ConstructorsAndAssignmentsWithNonTrivialBaseCopyConstructor, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  ctor(other: ref<base_class,t,f,cpp_ref>) { return; }"
			"} in struct class : [public base] {"
			"  ctor() = default;"
			"  ctor(other: ref<class,t,f,cpp_ref>) = default;"
			"  ctor(other: ref<class,f,f,cpp_rref>) = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> = default;"
			"}")));
	}

	TEST(ConstructorsAndAssignmentsWithNonTrivialBaseCopyAssignment, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<base_class,t,f,cpp_ref>) -> ref<base_class,f,f,cpp_ref> { return; }"
			"} in struct class : [public base] {"
			"  ctor() = default;"
			"  ctor(other: ref<class,t,f,cpp_ref>) = default;"
			"  ctor(other: ref<class,t,f,cpp_rref>) = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> = default;"
			"}")));
	}

	TEST(TrivialStructWithNonTrivialBaseCopyConstructor, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  ctor(other: ref<base_class,t,f,cpp_ref>) { return; }"
			"} in struct class : [public base] { }")));
	}

	TEST(TrivialStructWithNonTrivialBaseCopyAssignment, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<base_class,t,f,cpp_ref>) -> ref<base_class,f,f,cpp_ref> { 5; return ref_cast(this, type_lit(f), type_lit(f), type_lit(cpp_ref)); }"
			"} in struct class : [public base] { }")));
	}

	TEST(TriviallyCopyable, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// simple scalar values
		EXPECT_EQ(std::is_trivially_copyable<int>::value, isTriviallyCopyable(builder.parseType("int<4>")));
		EXPECT_EQ(std::is_trivially_copyable<double>::value, isTriviallyCopyable(builder.parseType("real<8>")));
		EXPECT_EQ(std::is_trivially_copyable<int[]>::value, isTriviallyCopyable(builder.parseType("array<int<4>,1>")));
		EXPECT_EQ(std::is_trivially_copyable<double[]>::value, isTriviallyCopyable(builder.parseType("array<real<8>,1>")));

		// empty struct
		struct S1 {
		};
		EXPECT_EQ(std::is_trivially_copyable<S1>::value, isTriviallyCopyable(builder.parseType(R"(struct S1 {
		})")));

		// test all default constructs
		struct SDC1 {
			SDC1() = default;
		};
		EXPECT_EQ(std::is_trivially_copyable<SDC1>::value, isTriviallyCopyable(builder.parseType(R"(struct SDC1 {
			ctor() = default;
		})")));
		struct SDC2 {
			SDC2() = delete;
		};
		EXPECT_EQ(std::is_trivially_copyable<SDC2>::value, isTriviallyCopyable(builder.parseType(R"(struct SDC2 {
			ctor() = delete;
		})")));
		struct SDC3 {
			SDC3() {};
		};
		EXPECT_EQ(std::is_trivially_copyable<SDC3>::value, isTriviallyCopyable(builder.parseType(R"(struct SDC3 {
			ctor() {}
		})")));

		struct SCC1 {
			SCC1(const SCC1&) = default;
		};
		EXPECT_EQ(std::is_trivially_copyable<SCC1>::value, isTriviallyCopyable(builder.parseType(R"(struct SCC1 {
			ctor(other : ref<SCC1,t,f,cpp_ref>) = default;
		})")));
		struct SCC2 {
			SCC2(const SCC2&) = delete;
		};
		EXPECT_EQ(std::is_trivially_copyable<SCC2>::value, isTriviallyCopyable(builder.parseType(R"(struct SCC2 {
			ctor(other : ref<SCC2,t,f,cpp_ref>) = default;
		})")));
		struct SCC3 {
			SCC3(const SCC3&) {}
		};
		EXPECT_EQ(std::is_trivially_copyable<SCC3>::value, isTriviallyCopyable(builder.parseType(R"(struct SCC3 {
			ctor(other : ref<SCC3,t,f,cpp_ref>) {}
		})")));

		struct SMC1 {
			SMC1(SMC1&&) = default;
		};
		EXPECT_EQ(std::is_trivially_copyable<SMC1>::value, isTriviallyCopyable(builder.parseType(R"(struct SMC1 {
			ctor(other : ref<SMC1,f,f,cpp_rref>) = default;
		})")));
		struct SMC2 {
			SMC2(SMC2&&) = delete;
		};
		EXPECT_EQ(std::is_trivially_copyable<SMC2>::value, isTriviallyCopyable(builder.parseType(R"(struct SMC2 {
			ctor(other : ref<SMC2,f,f,cpp_rref>) = default;
		})")));
		struct SMC3 {
			SMC3(SMC3&&) {}
		};
		EXPECT_EQ(std::is_trivially_copyable<SMC3>::value, isTriviallyCopyable(builder.parseType(R"(struct SMC3 {
			ctor(other : ref<SMC3,f,f,cpp_rref>) {}
		})")));

		struct SD1 {
			~SD1() = default;
		};
		EXPECT_EQ(std::is_trivially_copyable<SD1>::value, isTriviallyCopyable(builder.parseType(R"(struct SD1 {
			dtor() = default;
		})")));
		struct SD2 {
			~SD2() = delete;
		};
		EXPECT_EQ(std::is_trivially_copyable<SD2>::value, isTriviallyCopyable(builder.parseType(R"(struct SD2 {
			dtor() = default;
		})")));
		struct SD3 {
			~SD3() {}
		};
		EXPECT_EQ(std::is_trivially_copyable<SD3>::value, isTriviallyCopyable(builder.parseType(R"(struct SD3 {
			dtor() {}
		})")));
		struct SD4 {
			virtual ~SD4() {}
		};
		EXPECT_EQ(std::is_trivially_copyable<SD4>::value, isTriviallyCopyable(builder.parseType(R"(struct SD4 {
			dtor virtual () {}
		})")));

		struct SCA1 {
			SCA1& operator=(const SCA1&) = default;
		};
		EXPECT_EQ(std::is_trivially_copyable<SCA1>::value, isTriviallyCopyable(builder.parseType(R"(struct SCA1 {
			lambda IMP__operator_assign_ = (rhs : ref<SCA1,t,f,cpp_ref>) -> ref<SCA1,f,f,cpp_ref> = default;
		})")));
		struct SCA2 {
			SCA2& operator=(const SCA2&) = delete;
		};
		EXPECT_EQ(std::is_trivially_copyable<SCA2>::value, isTriviallyCopyable(builder.parseType(R"(struct SCA2 {
			lambda IMP__operator_assign_ = (rhs : ref<SCA2,t,f,cpp_ref>) -> ref<SCA2,f,f,cpp_ref> = default;
		})")));
		struct SCA3 {
			SCA3& operator=(const SCA3& rhs) { return *this; }
		};
		EXPECT_EQ(std::is_trivially_copyable<SCA3>::value, isTriviallyCopyable(builder.parseType(R"(struct SCA3 {
			lambda IMP__operator_assign_ = (rhs : ref<SCA3,t,f,cpp_ref>) -> ref<SCA3,f,f,cpp_ref> { return *this; }
		})")));

		struct SMA1 {
			SMA1& operator=(SMA1&&) = default;
		};
		EXPECT_EQ(std::is_trivially_copyable<SMA1>::value, isTriviallyCopyable(builder.parseType(R"(struct SMA1 {
			lambda IMP__operator_assign_ = (rhs : ref<SMA1,f,f,cpp_rref>) -> ref<SMA1,f,f,cpp_ref> = default;
		})")));
		struct SMA2 {
			SMA2& operator=(SMA2&&) = delete;
		};
		EXPECT_EQ(std::is_trivially_copyable<SMA2>::value, isTriviallyCopyable(builder.parseType(R"(struct SMA2 {
			lambda IMP__operator_assign_ = (rhs : ref<SMA2,f,f,cpp_rref>) -> ref<SMA2,f,f,cpp_ref> = default;
		})")));
		struct SMA3 {
			SMA3& operator=(SMA3&&) { return *this; }
		};
		EXPECT_EQ(std::is_trivially_copyable<SMA3>::value, isTriviallyCopyable(builder.parseType(R"(struct SMA3 {
			lambda IMP__operator_assign_ = (rhs : ref<SMA3,f,f,cpp_rref>) -> ref<SMA3,f,f,cpp_ref> { return *this; }
		})")));

		// test virtual member function
		struct SVM1 {
			virtual void foo() {};
		};
		EXPECT_EQ(std::is_trivially_copyable<SVM1>::value, isTriviallyCopyable(builder.parseType(R"(struct SVM1 {
			virtual lambda foo = () -> unit {}
		})")));

		// test (non)virtual parent
		struct P1 {};
		struct SP1 : public P1 { };
		EXPECT_EQ(std::is_trivially_copyable<SP1>::value, isTriviallyCopyable(builder.parseType(R"(def struct P1 {}; struct SP1 : [P1] {})")));
		struct P2 {};
		struct SP2 : public virtual P2 { };
		EXPECT_EQ(std::is_trivially_copyable<SP2>::value, isTriviallyCopyable(builder.parseType(R"(def struct P2 {}; struct SP2 : [virtual P2] {})")));
		struct P3 { ~P3() {} };
		struct SP3 : public P3 { };
		EXPECT_EQ(std::is_trivially_copyable<SP3>::value, isTriviallyCopyable(builder.parseType(R"(def struct P3 { dtor() {} }; struct SP3 : [P3] {})")));

		// fields
		struct SF1 {
			int f;
		};
		EXPECT_EQ(std::is_trivially_copyable<SF1>::value, isTriviallyCopyable(builder.parseType(R"(struct SF1 {
			f : int<4>;
		})")));
		struct SF2 {
			SF1 f;
		};
		EXPECT_EQ(std::is_trivially_copyable<SF2>::value, isTriviallyCopyable(builder.parseType(R"(def struct SF1 { f : int<4>; }; struct SF2 {
			f : SF1;
		})")));
		struct SF3 {
			P3 f;
		};
		EXPECT_EQ(std::is_trivially_copyable<SF3>::value, isTriviallyCopyable(builder.parseType(R"(def struct P3 { dtor() {} }; struct SF3 {
			f : P3;
		})")));
	}

	TEST(Trivial, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// here we only need to cover some cases with(out) default constructors. The rest is covered already by isTriviallyCopyable
		struct S1 {
			int f;
		};
		EXPECT_EQ(std::is_trivial<S1>::value, isTrivial(builder.parseType(R"(struct S1 {
			f : int<4>;
		})")));
		struct S2 {
			S2() = default;
		};
		EXPECT_EQ(std::is_trivial<S2>::value, isTrivial(builder.parseType(R"(struct S2 {
			ctor() = default;
		})")));
		struct S3 {
			S3() = delete;
		};
		EXPECT_EQ(false, isTrivial(builder.parseType(R"(struct S3 {
			ctor() = delete;
		})")));
		// Note: We can _not_ compare with the result of std::is_trivial here, since that seems ot be ill-defined in C++11 as well as C++14.
		// Deleting the default constructor results in an object, which is trivial in that versions of the standard, but the latest version says it isn't.
		// See also here: https://stackoverflow.com/questions/22812183/a-deleted-default-constructor-could-still-be-trivial
//		std::cout << "is_default_constructible: " << std::is_default_constructible<S3>::value << std::endl;
//		std::cout << "is_trivially_copyable: " << std::is_trivially_copyable<S3>::value << std::endl;
//		std::cout << "is_trivial: " << std::is_trivial<S3>::value << std::endl;
//		EXPECT_EQ(std::is_trivial<S3>::value, isTrivial(builder.parseType(R"(struct S3 {
//			ctor() = delete;
//		})")));
		struct S4 {
			S4() {}
		};
		EXPECT_EQ(std::is_trivial<S4>::value, isTrivial(builder.parseType(R"(struct S4 {
			ctor() {}
		})")));
		struct S5 {
			S5(int i) {}
		};
		EXPECT_EQ(std::is_trivial<S5>::value, isTrivial(builder.parseType(R"(struct S5 {
			ctor(i : int<4>) {}
		})")));
	}

	/*
	TEST(GlobalRec, InitBug) {
	    NodeManager manager;
	    IRBuilder builder(manager);
	    std::map<string, NodePtr> symbols;
	    auto recType = builder.parseType("let type0 = struct { ref<array<type0,1>> s; } in type0");
	    symbols["recTy"] = recType;

	    auto init = builder.parseExpr("ref_reinterpret(ref_null, lit(type<array<recTy,1>>))",symbols);
	    //std::cout << init << std::endl;

	    auto structExpr = builder.structExpr(toVector(builder.namedValue("s",init)));
	    //std::cout << structExpr << std::endl;

	    ExpressionList elements;
	    elements.push_back(structExpr);

	    auto vecPartialInit = builder.callExpr(
	        builder.getLangBasic().getVectorInitPartial(),
	        core::encoder::toIR(manager, elements),
	        builder.getIntParamLiteral(3));
	    //std::cout << vecPartialInit<< std::endl;

	    auto global = builder.literal("global", builder.parseType("ref<vector<recTy,3>>", symbols));
	    //std::cout << global << std::endl;

	    auto assign= builder.assign(global,vecPartialInit);
	    //std::cout << assign << std::endl;

	    auto semanticErrors = insieme::core::checks::check(assign);
	    std::cout << semanticErrors << std::endl;
	    EXPECT_TRUE(semanticErrors.empty());
	}
	*/

	TEST(GenericTypeVariable, Normalize) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto type = [&](const std::string& code) {
			return builder.parseType(code);
		};
		auto var = [&](const std::string& code) {
			return type(code).as<GenericTypeVariablePtr>();
		};

		EXPECT_EQ(type("'a<>"), normalize(var("'a<>")));
		EXPECT_EQ(type("'a<'_>"), normalize(var("'a<'b>")));
		EXPECT_EQ(type("'a<'_,'_>"), normalize(var("'a<'b,'c>")));
		EXPECT_EQ(type("'a<'_,'_,'_>"), normalize(var("'a<'b,'c,'d>")));

		EXPECT_EQ(type("'a<'_,'_>"), normalize(var("'a<A,B>")));
		EXPECT_EQ(type("'a<'_,'_>"), normalize(var("'a<A,B<C,D>>")));

		EXPECT_EQ(type("'a<'_,'_...>"), normalize(var("'a<A,'b...>")));

		EXPECT_EQ(type("'a<'_,'_<'_,'_>>"), normalize(var("'a<A,'b<'c,'d>>")));
		EXPECT_EQ(type("'a<'_,'_...<'_,'_>>"), normalize(var("'a<A,'b...<'c,'d>>")));

		EXPECT_EQ(type("'a<'_,'_...<'_,'_<'_>>>"), normalize(var("'a<A,'b...<'c,'d<'e>>>")));

	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
