/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/checks/full_check.h"
#include "insieme/frontend/ocl/ocl_code_snipplets.h"

namespace insieme {
namespace frontend {

using namespace core;

void doSemanticChecks(NodePtr toBeChecked) {
	auto semantic = checks::check(toBeChecked);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const checks::Message& cur) {
		std::cout << cur << std::endl;
	});

	auto errors = semantic.getErrors();
	EXPECT_EQ(0u, errors.size()) ;

	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const core::checks::Message& cur) {
		std::cout << cur << std::endl;
	});
}

TEST(OclCodeSnippletsTest, Device) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	ExpressionPtr convert = ocl::getConvert(5, builder);
	doSemanticChecks(convert);
}

TEST(OclCodeSnippletsTest, Host) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	ocl::Ocl2Inspire o2i;

	doSemanticChecks(o2i.getClCreateBuffer(false, true, builder));
	doSemanticChecks(o2i.getClCreateBuffer(true, false, builder));

	doSemanticChecks(o2i.getClCopyBuffer(builder));
	doSemanticChecks(o2i.getClCopyBufferFallback(builder));

	doSemanticChecks(o2i.getClGetIDs(builder));

	doSemanticChecks(o2i.getClReadBuffer(builder));
	doSemanticChecks(o2i.getClReadBufferFallback(builder));

	doSemanticChecks(o2i.getClWriteBuffer(builder));
	doSemanticChecks(o2i.getClWriteBufferFallback(builder));
}

}
}
