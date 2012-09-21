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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/parser/ir_parse.h"

#include "insieme/analysis/features/cache_features.h"

#include "insieme/utils/timer.h"

namespace insieme {
namespace analysis {
namespace features {

	using namespace core;

	struct EmptyModel : public CacheModel {
		int counter;
		EmptyModel() : counter(0) {}
		virtual bool access(uint64_t location, unsigned size) {
			counter++; return false;
		}

		virtual void reset() { counter=0; }
		virtual TypePtr getFeatureType() const { return TypePtr(); }
		virtual Value getFeatureValue() const  { return 0; }
		virtual void invalidate() { };

	};

	TEST(CacheSimulator, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<vector<vector<uint<4>,1>,1>>"));

		// load some code sample ...
		auto forStmt = builder.parseStmt(
			"for(uint<4> k = 0u..10u) {"
			"	for(uint<4> i = 0u..20u) {"
			"		ref<uint<4>> m = var(10u);"
			"		v[i];"
			"		for(uint<4> j = 0u..30u : 1u) {"
			"			v[i];"
			"			v[i][j] = *m;"
			"		}"
			"	}"
			"}", symbols).as<ForStmtPtr>();
		EXPECT_TRUE(forStmt);

		EmptyModel model;
		evalModel(forStmt, model);

		EXPECT_EQ(10*20*30, model.counter);
	}

	TEST(CacheSimulator, SimpleCacheModel) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<uint<4>,1>>"));

		auto forStmt = builder.parseStmt(
			"for( uint<4> i = 0u..100u) {"
			"	v[i] = i;"
			"}", symbols).as<ForStmtPtr>();

		EXPECT_TRUE(forStmt);

		// createing a cache with 16 Byte line size, 8 lines
		DirectCacheModel<16,8> model;
		evalModel(forStmt, model);

		EXPECT_EQ((100*4)/16,  model.getMisses());
		EXPECT_EQ(100*4 - model.getMisses(), model.getHits());
	}

	TEST(CacheSimulator, SimpleCacheModel2) {
		NodeManager mgr;
		parse::IRParser parser(mgr);


		auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement(
			"for(decl uint<4>:k = 0 .. 10 : 1) {"
			"	for(decl uint<4>:i = 0 .. 20 : 1) {"
			"		for(decl uint<4>:j = 0 .. 30 : 1) {"
			"			(op<ref.assign>((op<vector.ref.elem>((op<vector.ref.elem>(ref<vector<vector<uint<4>,10>,10>>:v, i)), j)), (i+j)));"
			"		};"
			"	};"
			"}") );

		EXPECT_TRUE(forStmt);

		// createing a cache with 4 Byte line size, 8 lines
		DirectCacheModel<4,8> model;
		evalModel(forStmt, model);

		long readBytes = 10*20*30*4;
		EXPECT_EQ(readBytes / 4,  model.getMisses());
		EXPECT_EQ(readBytes - model.getMisses(), model.getHits());


		// and the optimized loop nest:
		forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement(
			"for(decl uint<4>:i = 0 .. 20 : 1) {"
			"	for(decl uint<4>:j = 0 .. 30 : 1) {"
			"		for(decl uint<4>:k = 0 .. 10 : 1) {"
			"			(op<ref.assign>((op<vector.ref.elem>((op<vector.ref.elem>(ref<vector<vector<uint<4>,10>,10>>:v, i)), j)), (i+j)));"
			"		};"
			"	};"
			"}") );

		EXPECT_TRUE(forStmt);

		// re-evaluate for new loop nest
		model.reset();
		evalModel(forStmt, model);

		// should only cause one miss within every iteration of the second-innermost loop
		EXPECT_EQ(20*30,  model.getMisses());
		EXPECT_EQ(readBytes - model.getMisses(), model.getHits());

	}


	TEST(CacheSimulator, LRUCacheModel) {
		NodeManager mgr;
		parse::IRParser parser(mgr);

		auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement(
			"for(decl uint<4>:k = 0 .. 10 : 1) {"
			"	for(decl uint<4>:i = 0 .. 20 : 1) {"
			"		for(decl uint<4>:j = 0 .. 30 : 1) {"
			"			(op<ref.assign>((op<vector.ref.elem>((op<vector.ref.elem>(ref<vector<vector<uint<4>,10>,10>>:v, i)), j)), (i+j)));"
			"		};"
			"	};"
			"}") );

		EXPECT_TRUE(forStmt);

		// creating a LRU cache with 4 Byte line size, 4 lines, 2-way associative
		LRUCacheModel<4,4,2> model;
		evalModel(forStmt, model);

		long readBytes = 10*20*30*4;
		EXPECT_EQ(readBytes / 4,  model.getMisses());
		EXPECT_EQ(readBytes - model.getMisses(), model.getHits());


		// creating a LRU cache with 4 Byte line size, 2 lines, 4-way associative
		LRUCacheModel<4,2,4> model2;
		evalModel(forStmt, model2);

		EXPECT_EQ(readBytes / 4,  model2.getMisses());
		EXPECT_EQ(readBytes - model2.getMisses(), model2.getHits());

	}

	TEST(CacheSimulator, MultiLevelCache) {
		NodeManager mgr;
		parse::IRParser parser(mgr);

		auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement(
			"for(decl uint<4>:k = 0 .. 10 : 1) {"
			"	for(decl uint<4>:i = 0 .. 20 : 1) {"
			"		for(decl uint<4>:j = 0 .. 30 : 1) {"
			"			(op<ref.assign>((op<vector.ref.elem>((op<vector.ref.elem>(ref<vector<vector<uint<4>,10>,10>>:v, i)), j)), (i+j)));"
			"		};"
			"	};"
			"}") );

		EXPECT_TRUE(forStmt);

		// create a 4-Level LRU cache with (4/2/1),(4,16,2),(4,512,16),(4,1024,1024)
		CacheModel* model = new MultiLevelCache<
			LRUCacheModel<4,2,1>,
			LRUCacheModel<4,16,2>,
			LRUCacheModel<4,512,16>,
			LRUCacheModel<4,1024,1024>
		>();

		evalModel(forStmt, *model);

		// you can see that L3 is capable of storing the entire matrix
		EXPECT_EQ("((int64,int64),(int64,int64),(int64,int64),(int64,int64))", toString(*model->getFeatureType()));
		EXPECT_EQ("[[6000,18000],[2200,21800],[220,8580],[220,660]]", toString(model->getFeatureValue()));

		delete model;
	}


	TEST(CacheSimulator, CacheModelPerformance) {
		NodeManager mgr;
		parse::IRParser parser(mgr);

		auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement(
			"for(decl uint<4>:k = 0 .. 1000 : 1) {"
			"	for(decl uint<4>:i = 0 .. 200 : 1) {"
			"		for(decl uint<4>:j = 0 .. 30 : 1) {"
			"			(op<ref.assign>((op<vector.ref.elem>((op<vector.ref.elem>(ref<vector<vector<uint<4>,10>,10>>:v, i)), j)), (i+j)));"
			"		};"
			"	};"
			"}") );

		EXPECT_TRUE(forStmt);

//		LRUCacheModel<4,4,1> model;
//		LRUCacheModel<4,4,8> model;
		LRUCacheModel<64,64,8> model;

		int num = 5;
		double sum = 0;
		for (int i = 0; i < num; i++) {
			utils::Timer time("Evaluation Time");
			evalModel(forStmt, model);
			time.stop();
			std::cout << "Time: " << time.getTime() << "\n";
			sum += time.getTime();
		}
		std::cout << "Avg:  " << (sum/num) << "\n";
		std::cout << "Num accesses: " << model.getFeatureValue() << "\n";

//		EXPECT_GT(1.5, sum/num) << "Performance should not be so bad!";

	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme

