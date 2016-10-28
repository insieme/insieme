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

#include "insieme/analysis/features/stack_size_estimation.h"

#include "insieme/core/ir_builder.h"


namespace insieme {
namespace analysis {
namespace features {

	TEST(StackSize, Basic) {
		core::NodeManager mgr;
		auto& basic = mgr.getLangBasic();
		core::IRBuilder builder(mgr);

		EXPECT_EQ(4,  getTypeSize(basic.getInt4()));
		EXPECT_EQ(8,  getTypeSize(basic.getUInt8()));
		EXPECT_EQ(16, getTypeSize(basic.getInt16()));
		EXPECT_EQ(4,  getTypeSize(basic.getReal4()));
		EXPECT_EQ(8,  getTypeSize(basic.getReal8()));
		EXPECT_EQ(12, getTypeSize(builder.parseType("def struct A { b : int<4>; c : real<8>; }; A")));
		EXPECT_EQ(8,  getTypeSize(builder.parseType("def union A { b : int<4>; c : real<8>; }; A")));
		EXPECT_EQ(40, getTypeSize(builder.parseType("array<int<4>, 10>")));
	}


} // end namespace features
} // end namespace analysis
} // end namespace insieme
