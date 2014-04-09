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
#include "insieme/core/analysis/type_utils.h"

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
		
		// add classinfo for type 
		unitA.addMetaInfo(defA, infoA);

		// add type to unit A
		unitA.addType(symbolA, defA);
		EXPECT_FALSE(core::hasMetaInfo(defA));


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

		// add classinfo for type 
		unitB.addMetaInfo(defB, infoB);

		// add type to unit A
		unitB.addType(symbolB, defB);
		EXPECT_FALSE(core::hasMetaInfo(defB));


		// -----------------------------------

		// merge the two TUs
		core::NodeManager mgrC;
		IRTranslationUnit unitC = merge(mgrC, unitA, unitB);

		//just check if the right amount of Ctors is in the metaInfo 
		//before merge
		auto metaInfoA = unitA.getMetaInfo(def);
		EXPECT_EQ(2, metaInfoA.getConstructors().size()) << metaInfoA;
		auto metaInfoB = unitB.getMetaInfo(def);
		EXPECT_EQ(2, metaInfoB.getConstructors().size()) << metaInfoB;
		//after merge
		auto metaInfoC = unitC.getMetaInfo(def);
		EXPECT_EQ(3, metaInfoC.getConstructors().size()) << metaInfoC;

		// -----------------------------------
		
		// now test the also the extracting
	
		//explicitly extractMetaInfos and attach them to the types 
		// for testing
		auto resA1 = unitA.resolve(symbol).as<core::TypePtr>();
		EXPECT_FALSE(core::hasMetaInfo(resA1));
		unitA.extractMetaInfos();
		auto resA = unitA.resolve(symbol).as<core::TypePtr>();
		EXPECT_TRUE(core::hasMetaInfo(resA));
		ASSERT_TRUE(core::hasMetaInfo(resA));
		EXPECT_EQ(*resA, *def);
		EXPECT_EQ(2, core::getMetaInfo(resA).getConstructors().size()) << core::getMetaInfo(resA);

		//explicitly extractMetaInfos and attach them to the types
		// for testing
		auto resB1 = unitB.resolve(symbol).as<core::TypePtr>();
		EXPECT_FALSE(core::hasMetaInfo(resB1));
		unitB.extractMetaInfos();
		auto resB = unitB.resolve(symbol).as<core::TypePtr>();
		EXPECT_TRUE(core::hasMetaInfo(resA));
		ASSERT_TRUE(core::hasMetaInfo(resB));
		EXPECT_EQ(*resB, *def);
		EXPECT_EQ(2, core::getMetaInfo(resB).getConstructors().size()) << core::getMetaInfo(resB);

		//explicitly extractMetaInfos and attach them to the types
		auto resC1 = unitC.resolve(symbol).as<core::TypePtr>();
		EXPECT_FALSE(core::hasMetaInfo(resC1));
		unitC.extractMetaInfos();
		auto resC = unitC.resolve(symbol).as<core::TypePtr>();
		ASSERT_TRUE(core::hasMetaInfo(resC));
		EXPECT_EQ(*resC, *def);
		EXPECT_EQ(3, core::getMetaInfo(resC).getConstructors().size()) << core::getMetaInfo(resC);

	}


	TEST(TranslationUnit, MetaClassInfoDumpMerge) {

		// build elements
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto symbol = builder.genericType("A");
		auto def = builder.parseType("struct { int<4> a; }");

		// create a in-memory streams
		stringstream bufferA(ios_base::out | ios_base::in | ios_base::binary);
		stringstream bufferB(ios_base::out | ios_base::in | ios_base::binary);

		// build a first translation unit in a first manager
		{
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
			unitA.addMetaInfo(defA, infoA);

			// add type to unit A
			unitA.addType(symbolA, defA);

			// dump it
			dump(bufferA, unitA);

			auto resA1 = unitA.resolve(symbol).as<core::TypePtr>();
			EXPECT_FALSE(core::hasMetaInfo(resA1));

		
			auto metaInfoA = unitA.getMetaInfo(def);
			EXPECT_EQ(2, metaInfoA.getConstructors().size()) << metaInfoA;

			// test the properties
			// for testing also do the extraction
			unitA.extractMetaInfos();
			auto resA = unitA.resolve(symbol).as<core::TypePtr>();
			ASSERT_TRUE(core::hasMetaInfo(resA));
			EXPECT_EQ(*resA, *def);
			EXPECT_EQ(2, core::getMetaInfo(resA).getConstructors().size()) << core::getMetaInfo(resA);


		}

		// -----------------------------------

		{
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
			unitB.addMetaInfo(defB, infoB);

			// add type to unit A
			unitB.addType(symbolB, defB);

			// dump it
			dump(bufferB, unitB);

			auto metaInfoB = unitB.getMetaInfo(def);
			EXPECT_EQ(2, metaInfoB.getConstructors().size()) << metaInfoB;

			// test the properties
			// for testing also do the extraction
			unitB.extractMetaInfos();
			auto resB = unitB.resolve(symbol).as<core::TypePtr>();
			ASSERT_TRUE(core::hasMetaInfo(resB));
			EXPECT_EQ(*resB, *def);
			EXPECT_EQ(2, core::getMetaInfo(resB).getConstructors().size()) << core::getMetaInfo(resB);
		}

		// -----------------------------------

		// load the translation units
		core::NodeManager mgrC;
		IRTranslationUnit unitC(mgrC);

		unitC = merge(mgrC, unitC, load(bufferA, mgrC));
		unitC = merge(mgrC, unitC, load(bufferB, mgrC));

		// -----------------------------------

		auto metaInfoC = unitC.getMetaInfo(def);
		EXPECT_EQ(3, metaInfoC.getConstructors().size()) << metaInfoC;

		// for testing
		unitC.extractMetaInfos();
		auto resC = unitC.resolve(symbol).as<core::TypePtr>();
		ASSERT_TRUE(core::hasMetaInfo(resC));
		EXPECT_EQ(*resC, *def);
		EXPECT_EQ(3, core::getMetaInfo(resC).getConstructors().size()) << core::getMetaInfo(resC);

	}

	TEST(TranslationUnit, TypeMerge) {
		
		core::NodeManager mgrA;
		IRTranslationUnit unitA(mgrA);
		core::IRBuilder builderA(mgrA);

		auto symbol = builderA.parseType("A").as<core::GenericTypePtr>();
		auto type   = builderA.parseType("struct A { int<4> x; }");

		unitA.addType(symbol, type);
			
		core::NodeManager mgrB;
		IRTranslationUnit unitB(mgrB);
		core::IRBuilder builderB(mgrB);
		{
			auto symbol = builderB.parseType("A").as<core::GenericTypePtr>();
			auto type   = builderB.parseType("struct A { int<4> x; }");
			unitB.addType(symbol, type);

			auto member = builderB.parseExpr("struct A { int<4> x; } :: ()->unit { return; }").as<core::LambdaExprPtr>();

			core::ClassMetaInfo classInfo;
			classInfo.addMemberFunction("method", member, false, false);
			unitB.addMetaInfo(type, classInfo);
			
			EXPECT_EQ(1, classInfo.getMemberFunctions().size());
			EXPECT_FALSE(core::hasMetaInfo(type));


			core::NodeManager mgr;
			IRTranslationUnit unit(mgr);
			unit =  merge(mgr, unitA, unitB);

			auto res1 = unit.resolve(symbol).as<core::TypePtr>();
			EXPECT_FALSE(core::hasMetaInfo(res1));
			EXPECT_FALSE(core::hasMetaInfo(type));
			
			{
				auto meta = unit.getMetaInfo(type);
				EXPECT_EQ(1, meta.getMemberFunctions().size());
			}

			{
				unit.extractMetaInfos();
				EXPECT_FALSE(core::hasMetaInfo(type));
				auto res = unit.resolve(symbol).as<core::TypePtr>();
				EXPECT_TRUE(core::hasMetaInfo(res));
				auto meta = core::getMetaInfo(res);
				EXPECT_EQ(1, meta.getMemberFunctions().size());
			}
		}

		//check if meta info traveld somehow
		{
			EXPECT_FALSE(core::hasMetaInfo(symbol));

			auto res = unitA.resolve(symbol).as<core::TypePtr>();
			EXPECT_FALSE(core::hasMetaInfo(res));

			core::TypePtr type = unitA[symbol];
			EXPECT_FALSE(core::hasMetaInfo(type));
		}

		core::NodeManager mgrC;
		IRTranslationUnit unitC(mgrC);
		core::IRBuilder builderC(mgrC);
		{
			auto symbol = builderC.parseType("A").as<core::GenericTypePtr>();
			auto type   = builderC.parseType("struct A { int<4> x; }");

			auto member = builderC.parseExpr("struct A { int<4> x; } :: ()->unit { return; }").as<core::LambdaExprPtr>();
			core::ClassMetaInfo classInfo;
			classInfo.addMemberFunction("method2", member, false, false);
			unitC.addMetaInfo(type, classInfo);
			EXPECT_FALSE(core::hasMetaInfo(type));
			EXPECT_FALSE(core::hasMetaInfo(symbol));

			unitC.addType(symbol, type);

			core::NodeManager mgr;
			IRTranslationUnit unit(mgr);
			unit = merge(mgrC, unitA, unitC);

			EXPECT_FALSE(core::hasMetaInfo(type));

			{
				auto meta = unit.getMetaInfo(type);
				EXPECT_EQ(1, meta.getMemberFunctions().size());
			}
			{
				unit.extractMetaInfos();
				EXPECT_TRUE(core::hasMetaInfo(type));
				auto res = unit.resolve(symbol).as<core::TypePtr>();
				EXPECT_TRUE(core::hasMetaInfo(res));
				auto meta = core::getMetaInfo(res);
				EXPECT_EQ(1, meta.getMemberFunctions().size());
			}
		}
	
		core::NodeManager mgr;
		IRTranslationUnit unit(mgr);
		unit = merge(mgr,{unitA, unitB, unitC});

		{
			core::IRBuilder builder(mgr);
			core::GenericTypePtr symbol = builder.parseType("A").as<core::GenericTypePtr>();
			EXPECT_FALSE(core::hasMetaInfo(symbol));

			core::TypePtr type = unit[symbol];
			EXPECT_FALSE(core::hasMetaInfo(type));

			auto res = unit.resolve(symbol).as<core::TypePtr>();
			EXPECT_FALSE(core::hasMetaInfo(res));

			{
				auto meta = unit.getMetaInfo(type);
				EXPECT_EQ(2, meta.getMemberFunctions().size());
			}
			{
				unit.extractMetaInfos();
				core::TypePtr type = unit[symbol];
				EXPECT_TRUE(core::hasMetaInfo(type));

				auto res = unit.resolve(symbol).as<core::TypePtr>();
				EXPECT_TRUE(core::hasMetaInfo(res));

				auto meta = core::getMetaInfo(type);
				EXPECT_EQ(2, meta.getMemberFunctions().size());
			}
		}

		// create a in-memory stream
		stringstream bufferA(ios_base::out | ios_base::in | ios_base::binary);
		stringstream bufferB(ios_base::out | ios_base::in | ios_base::binary);
		stringstream bufferC(ios_base::out | ios_base::in | ios_base::binary);

		// dump unit
		dump(bufferA, unitA);
		dump(bufferB, unitB);
		dump(bufferC, unitC);

		
		{
			// reload unit
			core::NodeManager manager;
			core::IRBuilder builder(manager);

			IRTranslationUnit unitA = load(bufferA, manager);
			IRTranslationUnit unitB = load(bufferB, manager);
			IRTranslationUnit unitC = load(bufferC, manager);
			
			IRTranslationUnit unit = merge(manager, {unitA, unitB, unitC});

			auto symbol = builder.parseType("A").as<core::GenericTypePtr>();
			EXPECT_EQ("AP(struct A <x:int<4>>)",toString(unit[symbol]));

			{
				auto meta = unit.getMetaInfo(type);
				EXPECT_EQ(2, meta.getMemberFunctions().size());
			}
			{
				unit.extractMetaInfos();
				EXPECT_TRUE(core::hasMetaInfo(unit[symbol]));
				auto meta = core::getMetaInfo(unit[symbol]);
				EXPECT_EQ(2, meta.getMemberFunctions().size());
			}
		}
	}



	TEST(TranslationUnit, RecursiveTypeExtraction) {

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		IRTranslationUnit tu(mgr);
		tu.addType(builder.genericType("d"), builder.parseType("struct d { ref<array<e,1>> x; }"));
		tu.addType(builder.genericType("e"), builder.parseType("struct e { ref<array<f,1>> x; ref<array<d,1>> y; }"));
		tu.addType(builder.genericType("f"), builder.parseType("struct f { ref<array<g,1>> x; ref<array<e,1>> y;}"));
		tu.addType(builder.genericType("g"), builder.parseType("struct g { ref<array<f,1>> x; }"));

		std::cout << tu << "\n";

		// resolve the recursive type
		auto res = tu.resolve(builder.genericType("d")).as<core::TypePtr>();

		// there should not be any free type variables left
		EXPECT_FALSE(core::analysis::hasFreeTypeVariables(res));

		// and it should be a recursive type!
		EXPECT_TRUE(res.isa<core::RecTypePtr>()) << res;

	}



} // end namespace tu
} // end namespace frontend
} // end namespace insieme
