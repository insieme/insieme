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

#include "insieme/driver/predictor/dummy_predictor.h"

#include "insieme/core/ir_builder.h"
#include "insieme/utils/container_utils.h"
#include "insieme/transform/primitives.h"

namespace insieme {
namespace driver {
namespace predictor {

	TEST(DummyPredictor, Basic) {

		// the test is mainly focusing on the interface, not the actual predictor

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create some IR structure
		const core::StatementPtr stmt = builder.breakStmt();
		region::Region region(builder.compoundStmt(stmt));

		transform::TransformationPtr id = std::make_shared<transform::NoOp>();

		DummyPredictor predictor;
		EXPECT_EQ(Undecided, predictor.compare(region, id, id));

	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme
