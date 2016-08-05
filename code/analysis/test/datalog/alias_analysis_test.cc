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

#include "insieme/analysis/datalog/alias_analysis.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace datalog {

	using namespace core;

	using SymbolTable = std::map<std::string,core::NodePtr>;


	TEST(AliasAnalysis, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(
				"{"
				"	var ref<bool> x = ref_new(type_lit(bool));"
				"	$x$;"
				"	$x$;"
				"}"
		);

		EXPECT_EQ(2,addresses.size());

		auto x1 = addresses[0].getRootAddress().as<CompoundStmtAddress>()[0].as<DeclarationStmtAddress>()->getVariable();
		auto x2 = addresses[0].as<VariableAddress>();
		auto x3 = addresses[1].as<VariableAddress>();

		EXPECT_TRUE(areAlias(x1,x1));
		EXPECT_TRUE(mayAlias(x1,x1));

		EXPECT_TRUE(areAlias(x2,x2));
		EXPECT_TRUE(mayAlias(x2,x2));

		EXPECT_TRUE(areAlias(x3,x3));
		EXPECT_TRUE(mayAlias(x3,x3));

		EXPECT_TRUE(areAlias(x1,x2));
		EXPECT_TRUE(mayAlias(x1,x2));

		EXPECT_TRUE(areAlias(x1,x3));
		EXPECT_TRUE(mayAlias(x1,x3));

		EXPECT_TRUE(areAlias(x2,x3));
		EXPECT_TRUE(mayAlias(x2,x3));
	}

	TEST(AliasAnalysis, MultipleReferences) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(
				"{"
				"	var ref<bool> x = ref_new(type_lit(bool));"
				"	var ref<bool> y = ref_new(type_lit(bool));"
				"	var ref<bool> z = x;"
				"	$x$;"
				"	$y$;"
				"	$z$;"
				"}"
		);

		EXPECT_EQ(3,addresses.size());

		auto x = addresses[0].as<VariableAddress>();;
		auto y = addresses[1].as<VariableAddress>();
		auto z = addresses[2].as<VariableAddress>();

		EXPECT_FALSE(areAlias(x,y));
		EXPECT_FALSE(mayAlias(x,y));

		EXPECT_TRUE(areAlias(x,z));
		EXPECT_TRUE(mayAlias(x,z));

		EXPECT_FALSE(areAlias(y,z));
		EXPECT_FALSE(mayAlias(y,z));

	}

	// TODO: test global variables

	// TODO: test free variables

} // end namespace datalog
} // end namespace analysis
} // end namespace insieme

