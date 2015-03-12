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

#include "insieme/core/ir_program.h"

#include "insieme/frontend/translation_unit.h"
#include "insieme/utils/config.h"
#include "insieme/frontend/convert.h"
#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/extensions/pragma_test_extension.h"

#include "insieme/utils/logging.h"

#include "clang/AST/ASTContext.h"

#include "clang/AST/Stmt.h"
#include "clang/AST/Type.h"

#include "insieme/core/printer/pretty_printer.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::utils::log;
using namespace insieme::frontend::conversion;
using namespace insieme::frontend::extensions;

TEST(TypeCast, FileTest) {

	NodeManager manager;
	insieme::frontend::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/casts.c");
	
	auto filter = [](const insieme::frontend::pragma::Pragma& curr){ return curr.getType() == "test"; };

	for(auto it = tu.pragmas_begin(filter), end = tu.pragmas_end(); it != end; ++it) {
		// we use an internal manager to have private counter for variables so we can write independent tests
		NodeManager mgr;

		Converter convFactory( mgr, tu);

		const TestPragma& tp = static_cast<const TestPragma&>(*(*it));

		if(tp.isStatement())
			EXPECT_EQ(tp.getExpected(), '\"' + toString(printer::PrettyPrinter(analysis::normalize(convFactory.convertStmt( tp.getStatement() )), printer::PrettyPrinter::PRINT_SINGLE_LINE)) + '\"' );
		else {
			if(const clang::TypeDecl* td = llvm::dyn_cast<const clang::TypeDecl>( tp.getDecl() )) {
				EXPECT_EQ(tp.getExpected(), '\"' + convFactory.convertType( td->getTypeForDecl()->getCanonicalTypeInternal() )->toString() + '\"' );
			} else if(const clang::VarDecl* vd = llvm::dyn_cast<const clang::VarDecl>( tp.getDecl() )) {
				EXPECT_EQ(tp.getExpected(), '\"' + convFactory.convertVarDecl( vd )->toString() + '\"' );
			}
		}
	}
}
