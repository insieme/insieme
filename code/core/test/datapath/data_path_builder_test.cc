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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/datapath/datapath.h"

namespace insieme {
namespace core {
namespace datapath {

TEST(DataPathBuilder, Basic) {

	NodeManager mgr;

	EXPECT_EQ("dp.root", toString(*DataPathBuilder(mgr).getPath()));
	EXPECT_EQ("dp.member(dp.root, hello)", toString(*DataPathBuilder(mgr).member("hello").getPath()));
	EXPECT_EQ("dp.element(dp.root, 12)", toString(*DataPathBuilder(mgr).element(12).getPath()));
	EXPECT_EQ("dp.component(dp.root, 3)", toString(*DataPathBuilder(mgr).component(3).getPath()));


	EXPECT_EQ("dp.component(dp.member(dp.element(dp.root, 12), hello), 3)", toString(*DataPathBuilder(mgr)
			.element(12)
			.member("hello")
			.component(3)
			.getPath())
	);

}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
