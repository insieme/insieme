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

#include "insieme/analysis/features/feature_tryout.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/parser/ir_parse.h"

namespace insieme {
namespace analysis {
namespace features {

	using namespace core;

	struct EmptyModel : public CacheModel {
		int counter;
		EmptyModel() : counter(0) {}
		virtual void access(long location, int size) {
			counter++;
		}
	};

	TEST(CacheSimulator, Basic) {
		NodeManager mgr;
		parse::IRParser parser(mgr);

		// load some code sample ...
//		auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement(
//			"for(decl uint<4>:i = 10 .. 50 : 1) {"
//			"	(op<array.ref.elem.1D>(ref<array<uint<4>,1>>:v, i));"
//			"	for(decl uint<4>:j = 5 .. 25 : 1) {"
//			"		if ( (j < 10 ) ) {"
//			"			(op<ref.assign>((op<array.ref.elem.1D>(ref<array<uint<4>,1>>:v, (i+j))), i));"
//			"			(op<array.ref.elem.1D>(ref<array<uint<4>,1>>:v, (i+j)));"
//			"		} else {"
//			"			(op<array.ref.elem.1D>(ref<array<uint<4>,1>>:v, (i-j)));"
//			"			(op<array.ref.elem.1D>(ref<array<uint<4>,1>>:v, (i-j)));"
//			"		};"
//			"	};"
//			"}") );


//		auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement(
//			"for(decl uint<4>:i = 0 .. 10 : 1) {"
//			"	(op<ref.assign>((op<array.ref.elem.1D>(ref<array<uint<4>,1>>:v, i)), i));"
//			"}") );

		auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement(
			"for(decl uint<4>:k = 0 .. 10 : 1) {"
			"	for(decl uint<4>:i = 0 .. 20 : 1) {"
			"		decl ref<uint<4>>:m = (op<ref.var>(10));"
			"		(op<vector.ref.elem>(ref<vector<vector<uint<4>,10>,10>>:v, i));"
			"		for(decl uint<4>:j = 0 .. 30 : 1) {"
			"			(op<vector.ref.elem>(ref<vector<vector<uint<4>,10>,10>>:v, i));"
			"			(op<ref.assign>((op<vector.ref.elem>((op<vector.ref.elem>(ref<vector<vector<uint<4>,10>,10>>:v, i)), j)), (op<ref.deref>(m))));"
			"		};"
			"	};"
			"}") );


		EXPECT_TRUE(forStmt);

		EmptyModel model;
		evalModel(forStmt, model);

		EXPECT_EQ(10*20*30, model.counter);
	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme

