/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/tu/ir_translation_unit.h"
#include "insieme/frontend/tu/ir_translation_unit_io.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/ir_class_info.h"

namespace insieme {
namespace frontend {
namespace tu {

	using namespace std;

	TEST(TranslationUnit, Basic) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		IRTranslationUnit unit(mgr);

		// check adding types
		unit.addType(builder.parseType("A").as<core::GenericTypePtr>(), builder.parseType("struct { int<4> x; }"));
		unit.addType(builder.parseType("B").as<core::GenericTypePtr>(), builder.parseType("struct { real<8> y; ref<A> a; }"));

		// check adding functions
		unit.addFunction(builder.parseExpr("lit(\"X\":()->unit)").as<core::LiteralPtr>(), builder.parseExpr("()->unit { return; }").as<core::LambdaExprPtr>());
		unit.addFunction(builder.parseExpr("lit(\"Y\":()->unit)").as<core::LiteralPtr>(), builder.parseExpr("()->unit { int<4> x; return; }").as<core::LambdaExprPtr>());

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
		unit.addType(builder.parseType("A").as<core::GenericTypePtr>(), builder.parseType("struct { int<4> x; }"));

		// check adding functions
		unit.addFunction(builder.parseExpr("lit(\"X\":()->unit)").as<core::LiteralPtr>(), builder.parseExpr("()->unit { return; }").as<core::LambdaExprPtr>());

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

	TEST(TranslationUnit, MetaClassInfoMerge) {

		// build elements
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto symbol = builder.genericType("A");
		auto def = builder.parseType("struct { int<4> a; }");

		// build a first translation unit in a first manager
		core::NodeManager mgrA;
		core::IRBuilder builderA(mgrA);

		IRTranslationUnit unitA(mgrA);

		auto symbolA = mgrA.get(symbol);
		auto defA = mgrA.get(def);

		// attach meta info to the definition
		core::ClassMetaInfo infoA;
		infoA.addConstructor(builderA.parseExpr(
				"let A = struct { int<4> a; } in "
				"A::() {}"
		).as<core::LambdaExprPtr>());
		infoA.addConstructor(builderA.parseExpr(
				"let A = struct { int<4> a; } in "
				"A::(int<4> a) {}"
		).as<core::LambdaExprPtr>());
		core::setMetaInfo(defA, infoA);

		// add type to unit A
		unitA.addType(symbolA, defA);


		// -----------------------------------


		// built a second translation unit in another manager
		core::NodeManager mgrB;
		core::IRBuilder builderB(mgrB);

		IRTranslationUnit unitB(mgrB);

		auto symbolB = mgrB.get(symbol);
		auto defB = mgrB.get(def);

		// attach meta info to the definition
		core::ClassMetaInfo infoB;
		infoB.addConstructor(builderB.parseExpr(
				"let A = struct { int<4> a; } in "
				"A::() {}"
		).as<core::LambdaExprPtr>());
		infoB.addConstructor(builderB.parseExpr(
				"let A = struct { int<4> a; } in "
				"A::(int<4> a, int<4> b) {}"
		).as<core::LambdaExprPtr>());
		core::setMetaInfo(defB, infoB);

		// add type to unit A
		unitB.addType(symbolB, defB);


		// -----------------------------------

		// merge the two TUs
		core::NodeManager mgrC;
		IRTranslationUnit unitC = merge(mgrC, unitA, unitB);

		// -----------------------------------

		// now test the properties
		auto resA = unitA.resolve(symbol).as<core::TypePtr>();
		ASSERT_TRUE(core::hasMetaInfo(resA));
		EXPECT_EQ(*resA, *def);
		EXPECT_EQ(2, core::getMetaInfo(resA).getConstructors().size()) << core::getMetaInfo(resA);

		auto resB = unitB.resolve(symbol).as<core::TypePtr>();
		ASSERT_TRUE(core::hasMetaInfo(resB));
		EXPECT_EQ(*resB, *def);
		EXPECT_EQ(2, core::getMetaInfo(resB).getConstructors().size()) << core::getMetaInfo(resB);

		auto resC = unitC.resolve(symbol).as<core::TypePtr>();
		ASSERT_TRUE(core::hasMetaInfo(resC));
		EXPECT_EQ(*resC, *def);
		EXPECT_EQ(3, core::getMetaInfo(resC).getConstructors().size()) << core::getMetaInfo(resC);


	}

} // end namespace tu
} // end namespace frontend
} // end namespace insieme
