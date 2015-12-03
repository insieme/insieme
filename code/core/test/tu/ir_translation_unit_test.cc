/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <sstream>

#include "insieme/core/tu/ir_translation_unit.h"
#include "insieme/core/tu/ir_translation_unit_io.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/type_utils.h"

namespace insieme {
namespace core {
namespace tu {

	using namespace std;

	TEST(TranslationUnit, Basic) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		IRTranslationUnit unit(mgr);

		// check adding types
		unit.addType(builder.parseType("A").as<core::GenericTypePtr>(), builder.parseType("struct { x: int<4>; }").as<TagTypePtr>());
		unit.addType(builder.parseType("B").as<core::GenericTypePtr>(), builder.parseType("struct { y: real<8>; a: ref<A>; }").as<TagTypePtr>());

		// check adding functions
		unit.addFunction(builder.parseExpr("lit(\"X\":()->unit)").as<core::LiteralPtr>(),
		                 builder.parseExpr("()->unit { return; }").as<core::LambdaExprPtr>());
		unit.addFunction(builder.parseExpr("lit(\"Y\":()->unit)").as<core::LiteralPtr>(),
		                 builder.parseExpr("()->unit { var int<4> x; return; }").as<core::LambdaExprPtr>());

		// check adding globals
		unit.addGlobal(builder.parseExpr("lit(\"a\":ref<int<4>>)").as<core::LiteralPtr>(), builder.parseExpr("12"));

		// print it
		EXPECT_FALSE(toString(unit).empty());
	}


	TEST(TranslationUnit, IO) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		// create a dummy translation unit
		IRTranslationUnit unit(mgr);

		// check adding types
		unit.addType(builder.parseType("A").as<core::GenericTypePtr>(), builder.parseType("struct { x: int<4>; }").as<TagTypePtr>());

		// check adding functions
		unit.addFunction(builder.parseExpr("lit(\"X\":()->unit)").as<core::LiteralPtr>(),
		                 builder.parseExpr("()->unit { return; }").as<core::LambdaExprPtr>());

		// check adding globals
		unit.addGlobal(builder.parseExpr("lit(\"a\":ref<int<4>>)").as<core::LiteralPtr>(), builder.parseExpr("12"));

		// -------------  dump it + restore it ------------

		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump unit
		dump(buffer, unit);

		// reload unit
		core::NodeManager managerB;
		IRTranslationUnit unitB = load(buffer, managerB);

		// they should be equal
		EXPECT_EQ(toString(unit), toString(unitB));
	}

	TEST(TranslationUnit, IR) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		// create a dummy translation unit
		IRTranslationUnit unit(mgr);

		// check adding types
		unit.addType(builder.parseType("A").as<core::GenericTypePtr>(), builder.parseType("struct { x : int<4>; }").as<TagTypePtr>());

		// check adding functions
		unit.addFunction(builder.parseExpr("lit(\"X\":()->unit)").as<core::LiteralPtr>(),
		                 builder.parseExpr("()->unit { return; }").as<core::LambdaExprPtr>());

		// check adding globals
		unit.addGlobal(builder.parseExpr("lit(\"a\":ref<int<4>>)").as<core::LiteralPtr>(), builder.parseExpr("12"));

		// -------------  dump it + restore it ------------

		core::ExpressionPtr ptr;

		// dump unit
		ptr = toIR(mgr, unit);

		// reload unit
		IRTranslationUnit unitB = fromIR(ptr);

		// they should be equal
		EXPECT_EQ(toString(unit), toString(unitB));
	}

	TEST(TranslationUnit, SimpleTypeExtraction) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		IRTranslationUnit tu(mgr);

		tu.addType(builder.genericType("a"), builder.parseType("struct a { x : int<4>; }").as<TagTypePtr>());

		std::cout << tu << "\n";

		// resolve the recursive type
		auto res = tu.resolve(builder.genericType("a")).as<core::TagTypePtr>();

		// there should not be any free type variables left
		EXPECT_FALSE(core::analysis::hasFreeTypeVariables(res));

		// and it should be a recursive type!
		EXPECT_TRUE(res->isStruct()) << res;
	}

	TEST(TranslationUnit, RecursiveTypeExtraction) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		IRTranslationUnit tu(mgr);
		tu.addType(builder.genericType("d"), builder.parseType("struct d { x : ref<array<e,1>>; }").as<TagTypePtr>());
		tu.addType(builder.genericType("e"), builder.parseType("struct e { x : ref<array<f,1>>; y : ref<array<d,1>>; }").as<TagTypePtr>());
		tu.addType(builder.genericType("f"), builder.parseType("struct f { x : ref<array<g,1>>; y : ref<array<e,1>>; }").as<TagTypePtr>());
		tu.addType(builder.genericType("g"), builder.parseType("struct g { x : ref<array<f,1>>; y : ref<array<a,1>>; }").as<TagTypePtr>());

		tu.addType(builder.genericType("a"), builder.parseType("struct a { x : int<4>; }").as<TagTypePtr>());

		std::cout << tu << "\n";

		// resolve the recursive type
		auto res = tu.resolve(builder.genericType("d")).as<core::TagTypePtr>();

		// there should not be any free type variables left
		EXPECT_FALSE(core::analysis::hasFreeTypeVariables(res));

		// and it should be a recursive type!
		EXPECT_TRUE(res->isRecursive()) << res;
	}

	TEST(TranslationUnit, MemberFunctionCall) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		IRTranslationUnit tu(mgr);

		auto genericType = builder.genericType("A");
		auto thisType = builder.refType(genericType);
		auto thisVariable = builder.variable(builder.refType(thisType));

		auto paramsF = toVector(thisVariable);
		auto bodyF = builder.compoundStmt(builder.returnStmt(builder.intLit(1)));
		auto lambdaF = builder.lambdaExpr(builder.functionType(toVector<TypePtr>(thisType), builder.getLangBasic().getInt4(), FK_MEMBER_FUNCTION), paramsF, bodyF);
		auto memberFunctionF = builder.memberFunction(false, "f", lambdaF);
		auto literalMemberFunctionF = builder.literal("A::f", lambdaF->getFunctionType());
		tu.addFunction(literalMemberFunctionF, lambdaF);
		std::cout << "Registered member function " << literalMemberFunctionF << " of type " << literalMemberFunctionF->getType() << "\n";

		auto paramsG = toVector(thisVariable);
		auto bodyG = builder.compoundStmt(builder.returnStmt(builder.callExpr(literalMemberFunctionF, builder.deref(thisVariable))));
		auto lambdaG = builder.lambdaExpr(builder.functionType(toVector<TypePtr>(thisType), builder.getLangBasic().getInt4(), FK_MEMBER_FUNCTION), paramsG, bodyG);
		auto memberFunctionG = builder.memberFunction(false, "g", lambdaG);
		auto literalMemberFunctionG = builder.literal("A::g", lambdaG->getFunctionType());
		tu.addFunction(literalMemberFunctionG, lambdaG);
		std::cout << "Registered member function " << literalMemberFunctionG << " of type " << literalMemberFunctionG->getType() << "\n";

		auto recordType = builder.structType("A", ParentList(), FieldList(), ExpressionList(), builder.getDefaultDestructor(thisType), false, toVector(memberFunctionF, memberFunctionG), PureVirtualMemberFunctionList());
		tu.addType(genericType, recordType);

		std::cout << "\nContents of TU:\n";
		std::cout << "Functions:\n";
		for (const auto& pair : tu.getFunctions()) {
			std::cout << "  " << pair.first << " -> " << pair.second << " (with type " << pair.second->getType() << " )\n";
		}
		std::cout << "Types:\n";
		for (const auto& pair : tu.getTypes()) {
			std::cout << "  " << pair.first << " -> " << pair.second << "\n";
		}

		std::cout << "\n\nTrying to resolve the type...\n";
		EXPECT_TRUE(tu.resolve(genericType));
	}


} // end namespace tu
} // end namespace core
} // end namespace insieme
