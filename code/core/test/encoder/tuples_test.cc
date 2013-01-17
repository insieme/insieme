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


#include "insieme/core/encoder/tuples.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

namespace insieme {
namespace core {
namespace encoder {

TEST(Tuples, TypeUtilities) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	// check the create_tuple_type
	EXPECT_EQ("()", toString(*detail::create_tuple_type<>()(mgr)));
	EXPECT_EQ("(int<4>)", toString(*detail::create_tuple_type<int>()(mgr)));
	EXPECT_EQ("(int<4>,real<8>)", toString(*detail::create_tuple_type<int, double>()(mgr)));

	// check the encoding
	EXPECT_TRUE(detail::is_tuple<>()(builder.tupleExpr()));
	EXPECT_TRUE(detail::is_tuple<int>()(builder.tupleExpr(toIR(mgr, 8))));
	EXPECT_TRUE((detail::is_tuple<int, double>()(builder.tupleExpr(toIR(mgr, 8),toIR(mgr, 8.0)))));

	EXPECT_FALSE(detail::is_tuple<int>()(builder.tupleExpr(toIR(mgr, 8), toIR(mgr, 8.0))));
	EXPECT_FALSE((detail::is_tuple<int, double>()(builder.tupleExpr(toIR(mgr, 8.0),toIR(mgr, 8.0)))));
}


TEST(Tuples, TupleConversion) {

	NodeManager manager;

	// create a tuple

	auto value = std::make_tuple(1,1.3,string("hello"));
	core::ExpressionPtr ir = toIR(manager, value);
	auto back = toValue<decltype(value)>(ir);

	EXPECT_EQ("(1,1.3,hello)", toString(value));
	EXPECT_EQ("tuple(1,1.3,hello)", toString(*ir));

	EXPECT_TRUE((isEncodingOf<decltype(value)>(ir)));
	EXPECT_EQ(value, back);

	EXPECT_EQ("[]", toString(check(ir, checks::getFullCheck())));

}


} // end namespace lists
} // end namespace core
} // end namespace insieme

