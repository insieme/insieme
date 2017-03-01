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
 *
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
