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

#include "insieme/analysis/inductive/memory_location.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace inductive {

	using namespace core;

	TEST(MemoryLocation, Basics) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		// also, we need a data path builder
		datapath::DataPathBuilder dbBuilder(mgr);


		// create a new integer object
		ExpressionPtr value = builder.uintLit(12);
		ExpressionPtr obj = builder.refNew(value);

		// create a simple memory location out of it
		MemoryLocation loc = ExpressionAddress(obj);

		// check location result
		EXPECT_EQ("0/<>", toString(loc));

		// access a reference of an array
		loc = MemoryLocation(ExpressionAddress(builder.refVar(builder.undefined(builder.arrayType(basic.getInt4())))));

		// access full array
		EXPECT_EQ("0/<>", toString(loc));

		// access an element of the array
		EXPECT_EQ("0/<>[12]", toString(loc.element(value)));
	}

} // end namespace inductive
} // end namespace analysis
} // end namespace insieme
