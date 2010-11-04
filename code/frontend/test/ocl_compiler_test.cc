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

#include "program.h"

#include "clang_compiler.h"
#include "ast_visitor.h"
#include "clang_config.h"
#include "naming.h"

#include <logging.h>

#include <iostream>
#include <fstream>
#include <sstream>

namespace fe = insieme::frontend;
namespace core = insieme::core;
//using namespace insieme::c_info;
using namespace insieme::utils::set;
using namespace google;

namespace {
class OclTestVisitor : public core::ASTVisitor<void> {
public:
    void visitLambdaExpr(const core::LambdaExprPtr& func) {
//        core::AnnotationMap map = func.getAnnotations();
 //       std::cout << "Size: " << map.size() << std::endl;

        // kernel function has at least 4 child nodes (type, 2 args, body)
        //TODO make me pretty
        if(func->getChildList().size() >= 4) {


        if(core::FunctionTypePtr funcType = core::dynamic_pointer_cast<const core::FunctionType>(func->getType())){
            core::FunctionType ft = *funcType;
            core::TypePtr retTy = ft.getReturnType();

            //check return type
            EXPECT_EQ("unit", retTy->getName());

            //check globalRange and localRange arguments
            core::TupleType::ElementTypeList args = funcType->getArgumentType()->getElementTypes();
            EXPECT_LE(static_cast<unsigned>(2), args.size());
            core::TypePtr globalRange = args.at(args.size()-2);
            EXPECT_EQ("vector<uint<4>,3>", globalRange->getName());
            core::TypePtr localRange = args.back();
            EXPECT_EQ("vector<uint<4>,3>", globalRange->getName());

        } else {
            assert(funcType && "Function has unexpected type");
        }

//std::cout << "Nchilds: " << func->getChildList().size() << std::endl;

        core::NodePtr node = func->getChildList()[0];
        std::cout << "this is lambdaaaa" << node->toString() << "\n";

        if(core::CompoundStmtPtr body = core::dynamic_pointer_cast<const core::CompoundStmt>(func->getChildList().back())){
            core::StatementPtr parFunc = body->getStatements().at(0);
            if(core::CallExprPtr parallelFunctionCall = dynamic_pointer_cast<const core::CallExpr>(parFunc))
                std::cout << "Found call for parallel  in function body\n";// Noop
            else
                EXPECT_TRUE(parallelFunctionCall);
        }
    }}

    void visitJobExpr(const core::JobExprPtr& job) {
        std::cout << "get a job!\n";

        core::JobExpr j = *job;
    }
};

}

TEST(OclCompilerTest, HelloCLTest) {
    // force logging to stderr
    LogToStderr();

    // Set severity level
    SetStderrLogging(5);
//    CommandLineOptions::Verbosity = 2;

    core::SharedNodeManager sharedManager = std::make_shared<core::NodeManager>();
    core::ProgramPtr program = core::Program::create(*sharedManager);

    LOG(INFO) << "Converting input program '" << std::string(SRC_DIR) << "hello.cl" << "' to IR...";
    fe::Program prog(sharedManager);
    prog.addTranslationUnit(std::string(SRC_DIR) + "hello.cl");
    program = prog.convert();
    LOG(INFO) << "Done.";

    LOG(INFO) << "Printing the IR: " << program;

    OclTestVisitor otv;
    core::visitAll(program, otv);

}
