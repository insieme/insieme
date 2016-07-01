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

#include "insieme/transform/connectors.h"

#include "insieme/transform/primitives.h"
#include "insieme/transform/filter/filter.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/pattern/ir_pattern.h"



namespace insieme {
namespace transform {

namespace p = core::pattern;
namespace irp = core::pattern::irp;


TEST(ForAll, Basic) {
	// create something to transform
	core::NodeManager manager;
	core::IRBuilder builder(manager);
	core::NodePtr lit = builder.intLit(42);

	// build a transformation
	TransformationPtr replacer = makeLambdaTransformation([&](const core::NodePtr& cur) { return lit; });
	//		filter::TargetFilter filter = filter::pattern(p::var("x", irp::forStmt()), "x");

	auto forStmt = p::aT(p::var("x", irp::forStmt()));
	filter::TargetFilter filter = filter::pattern(p::node(*(forStmt | !forStmt)), "x");
	;

	TransformationPtr transform = makeForAll(filter, replacer);

	core::NodePtr in = builder.parseStmt("{"
	                                     "	for(uint<4> i = 6u .. 12u : 3u) {"
	                                     "		for(uint<4> k = 6u .. 12u : 3u) {"
	                                     "			i+1u;"
	                                     "		}"
	                                     "	}"
	                                     "	for(uint<4> j = 3u .. 25u : 1u) {"
	                                     "		j+1u;"
	                                     "	}"
	                                     "}");
	EXPECT_TRUE(in);

	core::NodePtr out = transform->apply(in);
	EXPECT_EQ("{42; 42;}", toString(*out));
}


TEST(ForAll, ExtractParameters) {
	// create something to transform
	core::NodeManager manager;
	core::IRBuilder builder(manager);
	core::NodePtr lit = builder.intLit(42);

	// build a transformation
	TransformationPtr replacer = makeLambdaTransformation([&](const core::NodePtr& cur) { return lit; });
	//		filter::TargetFilter filter = filter::pattern(p::var("x", irp::forStmt()), "x");

	auto forStmt = p::aT(p::var("x", irp::forStmt()));
	filter::TargetFilter filter = filter::pattern(p::node(*(forStmt | !forStmt)), "x");
	;

	TransformationPtr transform = makeForAll(filter, replacer);

	parameter::Value should = parameter::combineValues(parameter::makeValue(filter), parameter::makeValue(replacer));
	parameter::Value is = transform->getParameters();

	EXPECT_EQ(should, is);
}

TEST(ForAll, Clone) {
	// create something to transform
	core::NodeManager manager;
	core::IRBuilder builder(manager);
	core::NodePtr lit = builder.intLit(42);

	// build a transformation
	TransformationPtr replacer = makeLambdaTransformation([&](const core::NodePtr& cur) { return lit; });
	//		filter::TargetFilter filter = filter::pattern(p::var("x", irp::forStmt()), "x");

	auto forStmt = p::aT(p::var("x", irp::forStmt()));
	filter::TargetFilter filter = filter::pattern(p::node(*(forStmt | !forStmt)), "x");
	;

	TransformationPtr transform = makeForAll(filter, replacer);

	// clone transformation
	TransformationPtr clone = transform->getInstanceUsing(transform->getParameters());

	EXPECT_NE(transform, clone);
	EXPECT_EQ(*transform, *clone);


	// create modified version
	parameter::Value modValue = parameter::combineValues(parameter::makeValue(filter::empty), parameter::makeValue(replacer));

	TransformationPtr mod = transform->getInstanceUsing(modValue);
	EXPECT_NE(*transform, *mod);


	// test whether an exception is raised if the type is wrong
	modValue = parameter::combineValues(parameter::makeValue(filter::all), // wrong type of filter
	                                    parameter::makeValue(replacer));

	EXPECT_ANY_THROW(transform->getInstanceUsing(modValue));
}

} // end namespace transform
} // end namespace insieme
