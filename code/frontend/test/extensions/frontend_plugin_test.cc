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

#include <sstream>

#include "insieme/backend/sequential/sequential_backend.h"

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
bool exprVisited = false;
bool stmtVisited = false;
bool postTypeVisited = false;
bool postExprVisited = false;
bool postStmtVisited = false;
bool tuVisited = false;
bool progVisited = false;

class ClangTestPlugin : public insieme::frontend::extensions::FrontendPlugin {
public:
    ClangTestPlugin() {
        macros.insert(std::make_pair<std::string,std::string>("A","char *rule_one = \"MOOSI_FOR_PRESIDENT\""));
        injectedHeaders.push_back("injectedHeader.h");
    }

    ClangTestPlugin(bool kidnapping) {
        macros.insert(std::make_pair<std::string,std::string>("A","char *rule_one = \"MOOSI_FOR_PRESIDENT\""));
        kidnappedHeaders.push_back(SRC_DIR "/inputs/kidnapped");
    }

    //TYPE VISITOR
	virtual core::TypePtr Visit(const clang::Type* type, frontend::conversion::Converter& convFact) {
        typeVisited=true;
        return nullptr;
	}

    virtual insieme::core::TypePtr PostVisit(const clang::Type* type, const insieme::core::TypePtr& irType,
                                             frontend::conversion::Converter& convFact) {
        postTypeVisited=true;
        return irType;
	}

	//EXPR VISITOR
	virtual core::ExpressionPtr Visit(const clang::Expr* expr, frontend::conversion::Converter& convFact) {
        exprVisited=true;
        return nullptr;
	}

    virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
                                             frontend::conversion::Converter& convFact) {
        postExprVisited=true;
        return irExpr;
	}

	//STMT VISITOR
    virtual stmtutils::StmtWrapper Visit(const clang::Stmt* stmt, insieme::frontend::conversion::Converter& convFact) {
        stmtVisited=true;
        return stmtutils::StmtWrapper();
    }

    virtual stmtutils::StmtWrapper PostVisit(const clang::Stmt* stmt, const stmtutils::StmtWrapper& irStmt,
                                             frontend::conversion::Converter& convFact) {
        postStmtVisited=true;
        return irStmt;
	}

	virtual bool Visit(const clang::Decl* decl, frontend::conversion::Converter& convFact) {
        declVisited = true;
        return false;
	}

	virtual core::ProgramPtr IRVisit(core::ProgramPtr& prog) {
        progVisited = true;
        return prog;
	}

    virtual frontend::tu::IRTranslationUnit IRVisit(frontend::tu::IRTranslationUnit& tu) {
        tuVisited = true;
        return tu;
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
 *  clang visitors are working correctly.
 */
TEST(ClangStage, Conversion) {
	//initialization
	insieme::core::NodeManager mgr;
    insieme::frontend::ConversionJob job(SRC_DIR "/inputs/simple.c");
    job.registerFrontendPlugin<ClangTestPlugin>();

	EXPECT_FALSE(declVisited);
	EXPECT_FALSE(typeVisited);
	EXPECT_FALSE(stmtVisited);
	EXPECT_FALSE(exprVisited);
	EXPECT_FALSE(postTypeVisited);
	EXPECT_FALSE(postStmtVisited);
	EXPECT_FALSE(postExprVisited);
	auto program = job.execute(mgr);
	EXPECT_TRUE(typeVisited);
	EXPECT_TRUE(stmtVisited);
	EXPECT_TRUE(exprVisited);
	EXPECT_TRUE(postTypeVisited);
	EXPECT_TRUE(postStmtVisited);
	EXPECT_TRUE(postExprVisited);
	EXPECT_TRUE(declVisited);

}

/**
 *  This test checks if the user provided
 *  macros are working correctly.
 */
TEST(PreClangStage, Macros) {
	//initialization
	insieme::core::NodeManager mgr;
    insieme::frontend::ConversionJob job(SRC_DIR "/inputs/simple.c");
    job.registerFrontendPlugin<ClangTestPlugin>();
    //execute job
    auto program = job.execute(mgr);
  	auto targetCode = insieme::backend::sequential::SequentialBackend::getDefault()->convert(program);
  	std::stringstream code;
  	code << (*targetCode);
  	EXPECT_TRUE(code.str().find("MOOSI_FOR_PRESIDENT") != std::string::npos);
  	EXPECT_TRUE(code.str().find("char* rule_one") != std::string::npos);
}

/**
 *  This test checks if the user plugin
 *  header injection is working correctly.
 */
TEST(PreClangStage, HeaderInjection) {
	//initialization
	insieme::core::NodeManager mgr;
    insieme::frontend::ConversionJob job(SRC_DIR "/inputs/simple.c");
    job.registerFrontendPlugin<ClangTestPlugin>();
    //execute job
    auto program = job.execute(mgr);
  	auto targetCode = insieme::backend::sequential::SequentialBackend::getDefault()->convert(program);
  	std::stringstream code;
  	code << (*targetCode);
 	EXPECT_TRUE(code.str().find("int32_t magicFunction()") != std::string::npos);
 	EXPECT_TRUE(code.str().find("return 42;") != std::string::npos);
}

/**
 *  This test checks if the user plugin
 *  header injection is working correctly.
 *  We try to kidnap stdio.h and provide our
 *  implementation, that defines the magicFunction
 */
TEST(PreClangStage, HeaderKidnapping) {
	//initialization
	insieme::core::NodeManager mgr;
    insieme::frontend::ConversionJob job(SRC_DIR "/inputs/simple.c");
    job.registerFrontendPlugin<ClangTestPlugin>(true);
    //execute job
    auto program = job.execute(mgr);
  	auto targetCode = insieme::backend::sequential::SequentialBackend::getDefault()->convert(program);
  	std::stringstream code;
  	code << (*targetCode);
 	EXPECT_TRUE(code.str().find("int32_t magicFunction()") != std::string::npos);
 	EXPECT_TRUE(code.str().find("return -42;") != std::string::npos);
}

/**
 *  This test checks if the user plugin
 *  IR visitor (tu and program) is working
 *  correctly.
 */
TEST(PostClangStage, IRVisit) {
	//initialization
	insieme::core::NodeManager mgr;
    insieme::frontend::ConversionJob job(SRC_DIR "/inputs/simple.c");
    job.registerFrontendPlugin<ClangTestPlugin>();
    //execute job
    progVisited = false;
    tuVisited = false;
    EXPECT_FALSE(progVisited);
 	EXPECT_FALSE(tuVisited);
    auto program = job.execute(mgr);
 	EXPECT_TRUE(progVisited);
 	EXPECT_TRUE(tuVisited);
}


////////////////////////////////////////////////////////////////////////////////////////////////
//    Decls visitor

 	int varsPre, varsPost;
	int funcsPre, funsPost;
	int typesPre, typesPost;

struct DeclVistors : public insieme::frontend::extensions::FrontendPlugin {
	virtual bool Visit    (const clang::Decl* decl, frontend::conversion::Converter& convFact) {
		if (llvm::dyn_cast<clang::FunctionDecl>(decl)){
		//	std::cout << " ########## FUNcT ################## " << std::endl;
		//	decl->dump();
		//	std::cout << " ################################### " << std::endl;
			funcsPre++;
		}
		else if (llvm::dyn_cast<clang::VarDecl>(decl)){
			//std::cout << " ########## VAR ################## " << std::endl;
			//decl->dump();
			//std::cout << " ################################### " << std::endl;
			varsPre++;
		}
		else if (llvm::dyn_cast<clang::TypeDecl>(decl)){
	//		std::cout << " ########## TYPE ################## " << std::endl;
	//		decl->dump();
	//		std::cout << decl->getDeclKindName() <<std::endl;
	//		std::cout << " ################################### " << std::endl;
			typesPre++;
		}
		return false;
	}
	virtual void PostVisit(const clang::Decl* decl, frontend::conversion::Converter& convFact) {
		if (llvm::dyn_cast<clang::FunctionDecl>(decl)){
			funsPost++;
		}
		else if (llvm::dyn_cast<clang::VarDecl>(decl)){
			varsPost++;
		}
		else if (llvm::dyn_cast<clang::TypeDecl>(decl)){
			typesPost++;
		}
	}
};


TEST(DeclsStage, MatchVisits) {
	//initialization
	insieme::core::NodeManager mgr;
    insieme::frontend::ConversionJob job(SRC_DIR "/inputs/decls.cpp");
	
	varsPre  = varsPost = 0;
	funcsPre = funsPost = 0;
	typesPre = typesPost = 0;

	// register plugin
    job.registerFrontendPlugin<DeclVistors>();

    //execute job
    auto program = job.execute(mgr);

//	std::cout << varsPre   <<" , " << varsPost << std::endl;
//	std::cout << funcsPre  <<" , " << funsPost << std::endl;
//	std::cout << typesPre  <<" , " << typesPost<< std::endl;

	EXPECT_EQ (10, varsPre);  
	EXPECT_EQ (18, funcsPre);   // this is weird, but works
	EXPECT_EQ (6, typesPre);	

	EXPECT_EQ (varsPre, varsPost);
	EXPECT_EQ (funcsPre, funsPost);
	EXPECT_EQ (typesPre, typesPost);
}




