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

#include "insieme/core/ast_builder.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/ast_address.h"
#include "insieme/core/parser/ir_parse.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace frontend {
namespace ocl {

// shortcut
#define BASIC builder.getNodeManager().basic

namespace {

/**
 * This struct holds inspire representations of OpenCL built-in host functions
 */
struct Ocl2Inspire {
private:
    core::parse::IRParser parser;

public:
    Ocl2Inspire(core::NodeManager& mgr) : parser(mgr) {}

    core::ExpressionPtr getClCreateBuffer();
};


/**
 * This class allows replaces a call to an OpenCL built-in function to an INSPIRE one
 *  */
class Handler {
public:

    virtual core::NodePtr handleNode(core::CallExprPtr node) =0;
};

template<typename Lambda>
class LambdaHandler : public Handler {
    // flag indicating if the definition of the actual function has already been added to the program
    static bool defAdded;
    core::ASTBuilder& builder;

    const char* fct;
    Lambda body;

public:
    LambdaHandler(core::ASTBuilder& build, const char* fun, Lambda lambda): builder(build), fct(fun), body(lambda) {}

    // creating a shared pointer to a LambdaHandler

    core::NodePtr handleNode(core::CallExprPtr node) {
        std::cout << "Handling node " << node << std::endl;

        return body(node);
    }
};

typedef std::shared_ptr<Handler> HandlerPtr;
typedef boost::unordered_map<string, HandlerPtr, boost::hash<string>> HandlerTable;
typedef boost::unordered_map<const core::VariablePtr, const core::VariablePtr> ClmemTable;

template<typename Lambda>
HandlerPtr make_handler(core::ASTBuilder& builder, const char* fct, Lambda lambda) {
    return std::make_shared<LambdaHandler<Lambda> >(builder, fct, lambda);
}

#define ADD_Handler(builder, fct, BODY) \
    handles.insert(std::make_pair(fct, make_handler(builder, fct, [&](core::CallExprPtr node){ BODY }))).second;



class HostMapper : public core::transform::CachedNodeMapping {
    core::ASTBuilder& builder;
//    Handlers handler;

    HandlerTable handles;
    ClmemTable cl_mems;
    Ocl2Inspire o2i;

public:
    HostMapper(core::ASTBuilder& build);

    const core::NodePtr resolveElement(const core::NodePtr& element);

};

class HostVisitor : public core::AddressVisitor<void> {
    core::ASTBuilder& builder;
    core::ProgramPtr& newProg;

    const core::NodePtr resolveElement(const core::NodePtr& element);

public:
    HostVisitor(core::ASTBuilder& build, core::ProgramPtr& prog) : core::AddressVisitor<void>(false), builder(build), newProg(prog) {};

    core::ProgramPtr getNewProg() { return newProg; }

    void visitCallExpr(const core::CallExprAddress& callExp);

};
}

class HostCompiler {
    core::ProgramPtr& mProgram;
    core::ASTBuilder builder;

public:
    HostCompiler(core::ProgramPtr& program, core::NodeManager& mgr): mProgram(program), builder(mgr) {}

    core::ProgramPtr compile();
};

} //namespace ocl
} //namespace frontend
} //namespace insieme
