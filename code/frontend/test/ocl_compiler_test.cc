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
#include "insieme/core/ir_visitor.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/annotations/c/naming.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/clang_config.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/logging.h"

#include <iostream>
#include <fstream>
#include <sstream>


namespace fe = insieme::frontend;
namespace core = insieme::core;
//using namespace insieme::c_info;
using namespace insieme::utils::set;
using namespace insieme::utils::log;

namespace {
class OclTestVisitor : public core::IRVisitor<void> {
public:

	OclTestVisitor() : core::IRVisitor<void>(false) {}

	void visitMarkerExpr(const core::MarkerExprPtr& marker) {
		// marker expression has not been removed
		EXPECT_FALSE(dynamic_pointer_cast<const core::LambdaExpr>(marker->getSubExpression()));
	}

    void visitLambdaExpr(const core::LambdaExprPtr& func) {
//        core::AnnotationMap map = func.getAnnotations();
 //       std::cout << "Size: " << map.size() << std::endl;

        //check globalRange and localRange arguments
        if(core::FunctionTypePtr&& funcType = core::dynamic_pointer_cast<const core::FunctionType>(func->getType())){
            const core::TypeList& args = funcType->getParameterTypes()->getElements();

            if(func->hasAnnotation(insieme::annotations::ocl::BaseAnnotation::KEY)) {

                const core::TypePtr& retTy = funcType->getReturnType();

                //check return type
                EXPECT_EQ("int<4>", toString(*retTy));
                EXPECT_GE(args.size(), static_cast<size_t>(2));
                core::TypePtr globalRange = args.at(args.size()-2);
                EXPECT_EQ("vector<uint<8>,3>", toString(*globalRange));
                core::TypePtr localRange = args.back();
                EXPECT_EQ("vector<uint<8>,3>", toString(*globalRange));

//LOG(INFO) << "Nchilds: " << func->getChildList().size() << std::endl;

                core::NodePtr node = func->getChildList()[0];
//                std::cout << "this is lambdaaaa  " << node->toString() << "\n";

                if(core::CompoundStmtPtr body = core::dynamic_pointer_cast<const core::CompoundStmt>(func->getChildList().back())){

                    // std::find_it returns this type:
                    //__gnu_cxx::__normal_iterator<const insieme::core::AnnotatedPtr<const insieme::core::Statement>*, std::vector<insieme::core::AnnotatedPtr<const insieme::core::Statement> > >
                    auto parallelFunctionCall = std::find_if(body->getStatements().begin(), body->getStatements().end(),
                            [] (core::StatementPtr bodyStatement) {
                        if(dynamic_pointer_cast<const core::CallExpr>(bodyStatement))
                            return true;
                        else if(core::DeclarationStmtPtr decl =  dynamic_pointer_cast<const core::DeclarationStmt>(bodyStatement)){
                            if(dynamic_pointer_cast<const core::CallExpr>(decl->getInitialization()))
                                return true;
                        }
                        return false;
                    });
                    if(core::NodeType::NT_DeclarationStmt != (*parallelFunctionCall)->getNodeType()) // check for call expr only if it is not a declexpr
                        EXPECT_EQ(core::NodeType::NT_CallExpr, (*parallelFunctionCall)->getNodeType());
                }
            }
       } else {
            assert(funcType && "Function has unexpected type");
        }
    }

    void visitJobExpr(const core::JobExprPtr& job) {
//        std::cout << ": get a job!\n";

        const core::NodeList& childs = job->getChildList();
        //at least the local and global range has to be captured as well as the type and the  range
        EXPECT_LE(static_cast<unsigned>(4), childs.size());
/*
        for(auto I = childs.begin(), E= childs.end(); I != E; ++I) {
            std::cout << "job's child: ";
            (*I)->printTo(std::cout);
            std::cout << std::endl;
        }*/
    }
};

}

TEST(OclCompilerTest, HelloCLTest) {
	Logger::get(std::cerr, DEBUG, 2);

	core::NodeManager manager;

    fe::ConversionJob job(SRC_DIR "inputs/hello.cl");
    job.addIncludeDirectory(SRC_DIR "inputs");
    job.setOption(fe::ConversionJob::OpenCL);

    LOG(INFO) << "Converting input program '" << std::string(SRC_DIR) << "inputs/hello.cl" << "' to IR...";
    core::ProgramPtr program = job.execute(manager);
    LOG(INFO) << "Done.";

    core::printer::PrettyPrinter pp(program, core::printer::PrettyPrinter::OPTIONS_DETAIL);

    LOG(INFO) << "Printing the IR: " << pp;

    OclTestVisitor otv;
    core::visitDepthFirst(program, otv);


//    LOG(INFO) << pp;

    auto errors = core::checks::check(program).getAll();

    EXPECT_EQ(0u, errors.size());

    std::sort(errors.begin(), errors.end());

    for_each(errors, [](const core::checks::Message& cur) {
        LOG(INFO) << cur << std::endl;
/*        core::NodeAddress address = cur.getAddress();
        core::NodePtr context = address.getParentNode(address.getDepth()-1);
        std::cout << "\t Context: " <<
                insieme::core::printer::PrettyPrinter(context, insieme::core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE, 3) << std::endl;
*/
    });
}
