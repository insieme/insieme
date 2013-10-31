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

#include "insieme/frontend/stmt_converter.h"
#include "insieme/frontend/expr_converter.h"
#include "insieme/frontend/type_converter.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/clang_config.h"
#include "insieme/frontend/convert.h"
#include "insieme/frontend/pragma/insieme.h"

#include "insieme/frontend/extensions/frontend_plugin.h"

using namespace insieme;

bool declVisited = false;
bool typeVisited = false;

class ClangTestPlugin : public insieme::frontend::extensions::FrontendPlugin {

	virtual core::TypePtr Visit(const clang::Type* type, frontend::conversion::Converter& convFact) {
        typeVisited=true;
        return nullptr;
	}

	virtual bool Visit(const clang::Decl* decl, frontend::conversion::Converter& convFact) {
        declVisited = true;
        return false;
	}

};

/**
 *  This test checks if the user provided
 *  clang plugin registration works correctly.
 */
TEST(ClangStage, Initialization) {
	//initialization
	insieme::core::NodeManager mgr;
    insieme::frontend::ConversionJob job(SRC_DIR "/inputs/simple.c");
    job.registerFrontendPlugin<ClangTestPlugin>();

	// register the frontend plugin
	EXPECT_EQ(1, job.getPlugins().size());
}

/**
 *  This test checks if the user provided
 *  clang plugin works correctly.
 */
TEST(ClangStage, Conversion) {
	//initialization
	insieme::core::NodeManager mgr;
    insieme::frontend::ConversionJob job(SRC_DIR "/inputs/simple.c");
    job.registerFrontendPlugin<ClangTestPlugin>();

    //check if the decl visitor and
    //the type visitor is visited correctly
	EXPECT_FALSE(declVisited);
	EXPECT_FALSE(typeVisited);
	job.execute(mgr);
	EXPECT_TRUE(typeVisited);
	EXPECT_TRUE(declVisited);

}
