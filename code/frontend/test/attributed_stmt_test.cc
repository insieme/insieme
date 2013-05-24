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
/*
#include <gtest/gtest.h>

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <clang/AST/ASTContext.h>

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/frontend/clang_config.h"
#include "insieme/frontend/program.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/utils/logging.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/frontend/frontend.h"

#include <fstream>
#include <string>
#include <algorithm>

using namespace insieme::core;
namespace fe = insieme::frontend;
using namespace clang;

void check(clang::Stmt * s, vector<clang::AttributedStmt *> * attributedStmts) {
        if(llvm::isa<clang::AttributedStmt>(s)) {
                attributedStmts->push_back((clang::AttributedStmt *) s);
        }

        for(clang::Stmt::child_iterator it=s->child_begin(); it!=s->child_end(); ++it) {
            if((*it)!=nullptr)
                check((*it), attributedStmts);
        }
}

TEST(AttributedStmtTest, FileTest) {
    vector<clang::AttributedStmt *> statements;
    vector<string> expected_pragmas;

    expected_pragmas.push_back("[omp::parallel[for[for]][private[a]]]");
    expected_pragmas.push_back("[omp::parallel]");
    expected_pragmas.push_back("[omp::for[firstprivate[a]][nowait[]]]");
    expected_pragmas.push_back("[omp::barrier]");


    //CREATE TRANSLATION UNIT AND CHECK IF ALL
    //4 PRAGMAS HAVE BEEN MATCHED AND THE ATTRIBUTEDSTMTs
    //HAVE BEEN CREATED CORRECTLY
	NodeManager manager;
	fe::ConversionJob job;
	fe::Program prog(manager, job);
	fe::TranslationUnit& tu = prog.addTranslationUnit( fe::ConversionJob(SRC_DIR "/inputs/omp_for.c") );

	clang::TranslationUnitDecl * tudecl = tu.getCompiler().getASTContext().getTranslationUnitDecl();
	for(clang::DeclContext::decl_iterator it=tudecl->decls_begin(); it!=tudecl->decls_end(); ++it) {
        if((*it)->hasBody()) {
            check((*it)->getBody(), &statements);
        }
	}

    //CHECK IF ALL PRAGMAS HAVE BEEN MATCHED
    //std::cout << "CHECK PRAGMA VECTOR SIZE\n";
    //EXPECT_EQ(4, statements.size());

    //CHECK IF THE PRAGMA ANNOTATIONS ARE CORRECT
    for(clang::AttributedStmt * s : statements) {
        //std::cout << "PRAGMA:\n";
        for(const clang::Attr * attribute : s->getAttrs()) {
            //std::cout << "CHECK ANNOTATION " << (((AnnotateAttr *) attribute)->getAnnotation().str()) << std::endl;
            //EXPECT_TRUE(std::find(expected_pragmas.begin(), expected_pragmas.end(), ((AnnotateAttr *) attribute)->getAnnotation().str()) != expected_pragmas.end());
        }
    }

	const fe::pragma::PragmaList& pl = tu.getPragmaList();
	std::cout << "Checking parsed pragmas (" << pl.size() << ") in file " << SRC_DIR "inputs/omp_for.c" << ": " << std::endl;
	int k=0;
	for(fe::pragma::PragmaPtr ptr : pl) {
        std::cout << "Pragma: " << ptr->getType() << "\n";
        if(ptr->isStatement()) {
            //check if pragma is an attributed stmt
            if(llvm::isa<clang::AttributedStmt>(ptr->getStatement())) {
                llvm::ArrayRef<const clang::Attr *> attrs= ((clang::AttributedStmt *)(ptr->getStatement()))->getAttrs();
                for(auto it=attrs.begin(); it!=attrs.end(); ++it) {
                    std::cout << ((clang::AnnotateAttr *)(*it))->getAnnotation().str() << std::endl;
                }
                k++;
            }
        }
	}

}
*/
