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

#include "utils/logging.h"

#include "frontend/clang_compiler.h"
#include "frontend/conversion.h"
#include "frontend/clang_config.h"
#include "frontend/insieme_pragma.h"

#include "clang/AST/Decl.h"
#include "clang/Index/Indexer.h"
#include "clang/Index/Program.h"

#include "core/program.h"
#include "core/ast_check.h"
#include "core/checks/typechecks.h"



using namespace insieme::core;
using namespace insieme::frontend;
using namespace insieme::frontend::conversion;

struct VariableResetHack {
	static void reset() { Variable::counter = 0; }
};

TEST(StmtConversion, FileTest) {
	using namespace clang;

	insieme::utils::InitLogger("ut_stmt_conversion_test", INFO, true);
	CommandLineOptions::Verbosity = 0;

	SharedNodeManager shared = std::make_shared<NodeManager>();
	insieme::frontend::Program prog(shared);
	prog.addTranslationUnit( std::string(SRC_DIR) + "/inputs/stmt.c" );

	const PragmaList& pl = (*prog.getTranslationUnits().begin())->getPragmaList();
	const ClangCompiler& comp = (*prog.getTranslationUnits().begin())->getCompiler();


	std::for_each(pl.begin(), pl.end(),
		[ & ](const PragmaPtr curr) {
			const TestPragma* tp = static_cast<const TestPragma*>(&*curr);
			// we reset the counter for variables so we can write independent tests
			VariableResetHack::reset();

			clang::idx::Program p;
			clang::idx::Indexer idx(p);

			ConversionFactory convFactory( shared, comp, prog.getClangIndexer(), prog.getClangProgram(), pl );

			if(tp->isStatement()) {
				StatementPtr&& stmt = convFactory.convertStmt( tp->getStatement() );
				EXPECT_EQ(tp->getExpected(), '\"' + stmt->toString() + '\"' );
				// do type checking
				MessageList&& msgList = check( stmt, checks::getFullCheck() );
				EXPECT_EQ(static_cast<unsigned int>(0), msgList.size());
				std::sort(msgList.begin(), msgList.end());
				std::for_each(msgList.begin(), msgList.end(), [&stmt](const Message& cur) {
					LOG(INFO) << *stmt;
					LOG(INFO) << cur << std::endl;
				});

			} else {
				if(const clang::TypeDecl* td = dyn_cast<const clang::TypeDecl>(tp->getDecl())) {
					EXPECT_EQ(tp->getExpected(), '\"' + convFactory.convertType( td->getTypeForDecl() )->toString() + '\"' );
				}else if(const clang::FunctionDecl* fd = dyn_cast<const clang::FunctionDecl>(tp->getDecl())) {
					EXPECT_EQ(tp->getExpected(), '\"' + convFactory.convertFunctionDecl( fd )->toString() + '\"' );
				}
			}


	});

}

