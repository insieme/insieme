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

// defines which are needed by LLVM
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#include "clang/AST/Decl.h"
#pragma GCC diagnostic pop
// DON'T MOVE THIS!

#include <gtest/gtest.h>

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/frontend/clang_config.h"
#include "insieme/frontend/program.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/frontend/frontend.h"

#include <fstream>
#include <string>

using namespace insieme::core;
namespace fe = insieme::frontend;
using namespace clang;

TEST(AttributedStmtTest, FileTest) {
/*
    //LOAD C FILE AND STORE AST INTO FILE
    //CREATE IR OUT OF AST CONTEXT AND STORE PrettyPrint
    //IN ast_content
	NodeManager manager;
	fe::ConversionJob job;
	fe::Program prog(manager, job);
	fe::TranslationUnit& tu = prog.addTranslationUnit( fe::ConversionJob(SRC_DIR "/inputs/simple_omp.c") );
	const pragma::PragmaList& pl = tu.getPragmaList();
	std::cout << "Checking parsed pragmas (" << pl.size() << "): " << std::endl;
	for(pragma::PragmaPtr ptr : pl) {
        //check if pragma is associated to stmt or decl
        if(ptr->isStatement()) {
            //check if pragma is an attributed stmt
            ptr->getStatement().is<clang::AttributedStmt const*> ()
        }
	}*/
}
