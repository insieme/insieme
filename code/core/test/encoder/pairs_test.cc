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


#include "insieme/core/encoder/pairs.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

namespace insieme {
namespace core {
namespace encoder {

	TEST(Pairs, Basic) {
		NodeManager nm;
		IRBuilder b(nm);

		auto& basic = nm.getLangBasic();

		// create a pair
		auto value = std::make_pair(1, 2u);
		core::ExpressionPtr ir = toIR(nm, value);

		EXPECT_TRUE(isPairType(ir->getType()));
		EXPECT_EQ("int<4>", toString(*getFirstElementType(ir->getType())));
		EXPECT_EQ("uint<4>", toString(*getSecondElementType(ir->getType())));
		EXPECT_EQ("pair<int<4>,uint<4>>", toString(*getPairType(basic.getInt4(), basic.getUInt4())));
	}

	TEST(Pairs, LanguageExtension) {
		NodeManager manager;
		const PairExtension& ext = manager.getLangExtension<PairExtension>();

		EXPECT_EQ("(('a,'b)->pair<'a,'b>)", toString(*ext.pair->getType()));
	}


	TEST(Pairs, PairConversion) {
		NodeManager manager;

		// create a pair

		auto value = std::make_pair(1, 2);
		core::ExpressionPtr ir = toIR(manager, value);
		auto back = toValue<pair<int, int>>(ir);

		EXPECT_EQ("(1,2)", toString(value));
		EXPECT_EQ("pair(1, 2)", toString(*ir));

		EXPECT_TRUE((isEncodingOf<pair<int, int>>(ir)));
		EXPECT_EQ(value, back);

		EXPECT_EQ("[]", toString(check(ir, checks::getFullCheck())));


		// test another type
		EXPECT_EQ("pair(12, 1.47)", toString(*toIR(manager, std::make_pair(12, 1.47))));
		EXPECT_EQ("pair(12, pair(1.47, hello))", toString(*toIR(manager, std::make_pair(12, std::make_pair(1.47, string("hello"))))));
	}


} // end namespace lists
} // end namespace core
} // end namespace insieme
