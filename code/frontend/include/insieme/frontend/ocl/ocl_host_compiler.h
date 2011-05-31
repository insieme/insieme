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

#include "insieme/frontend/program.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace frontend {
namespace ocl {

// shortcut
#define BASIC builder.getNodeManager().basic

namespace {

/**
 * Class to visit the AST and return the value of a certain variable, holding the path to a OpenCL kernel
 */
class KernelCodeRetriver : public core::ASTVisitor<bool> {
    const core::VariablePtr& pathToKernelFile; // Variable to look for
    const core::NodePtr& breakingStmt; // place where the path would be needed, can stop searching there
    const core::ASTBuilder builder;
    string path;

    bool visitNode(const core::NodePtr& node);
    bool visitCallExpr(const core::CallExprPtr& callExpr);
    bool visitDeclarationStmt(const core::DeclarationStmtPtr& decl);

public:
    KernelCodeRetriver(const core::VariablePtr lookFor, const core::NodePtr& stopAt, core::ASTBuilder build):
        ASTVisitor<bool>(false), pathToKernelFile(lookFor), breakingStmt(stopAt), builder(build) { }
    string getKernelFilePath(){ return path; }
};

/**
 * This struct holds inspire representations of OpenCL built-in host functions
 */
struct Ocl2Inspire {
private:
    core::parse::IRParser parser;

public:
    Ocl2Inspire(core::NodeManager& mgr) : parser(mgr) {}

    bool extractSizeFromSizeof(const core::ExpressionPtr& arg, core::ExpressionPtr& size, core::TypePtr& type );

    core::ExpressionPtr getClCreateBuffer();
    core::ExpressionPtr getClWriteBuffer();
    core::ExpressionPtr getClWriteBufferFallback();
    core::ExpressionPtr getClReadBuffer();
    core::ExpressionPtr getClReadBufferFallback();
};


/**
 * This class allows replaces a call to an OpenCL built-in function to an INSPIRE one
 *  */
class Handler {
protected:
    core::ProgramPtr kernels;
public:
    Handler(core::ASTBuilder& build) {
        kernels = core::Program::create(build.getNodeManager());
    }

    virtual core::NodePtr handleNode(core::CallExprPtr node) =0;

    const vector<core::ExpressionPtr>& getKernels(){ return kernels->getEntryPoints(); }
};

template<typename Lambda>
class LambdaHandler : public Handler {
    // flag indicating if the definition of the actual function has already been added to the program
    static bool defAdded;
    core::ASTBuilder& builder;

    const char* fct;
    Lambda body;

public:

    LambdaHandler(core::ASTBuilder& build, const char* fun, Lambda lambda): Handler(build), builder(build), fct(fun), body(lambda) {}

    // creating a shared pointer to a LambdaHandler

    core::NodePtr handleNode(core::CallExprPtr node) {
        LOG(DEBUG) << "Handling node " << node << std::endl;

        return body(node, kernels);
    }

};

typedef std::shared_ptr<Handler> HandlerPtr;
typedef boost::unordered_map<string, HandlerPtr, boost::hash<string>> HandlerTable;
typedef boost::unordered_map<core::VariablePtr,  core::VariablePtr> ClmemTable;
typedef boost::unordered_map<core::ExpressionPtr, std::vector<core::ExpressionPtr> > KernelArgs;
typedef boost::unordered_map<string, core::ExpressionPtr> KernelNames;
typedef boost::unordered_map<core::ExpressionPtr, core::LambdaExprPtr> KernelLambdas;

template<typename Lambda>
HandlerPtr make_handler(core::ASTBuilder& builder, const char* fct, Lambda lambda) {
    return std::make_shared<LambdaHandler<Lambda> >(builder, fct, lambda);
}

#define ADD_Handler(builder, fct, BODY) \
    handles.insert(std::make_pair(fct, make_handler(builder, fct, [&](core::CallExprPtr node, core::ProgramPtr& kernels){ BODY }))).second;



class HostMapper : public core::transform::CachedNodeMapping {
    core::ASTBuilder& builder;

    HandlerTable handles;
    ClmemTable cl_mems;
    Ocl2Inspire o2i;
    KernelArgs kernelArgs;
    KernelNames kernelNames;
    vector<core::ExpressionPtr> kernelEntries;
    core::ProgramPtr& mProgram;

    core::CallExprPtr checkAssignment(const core::CallExprPtr& oldCall);

    bool translateClCreateBuffer(const core::VariablePtr& var, const core::CallExprPtr& fun, const core::CallExprPtr& newRhs, core::NodePtr& ret);
    bool handleClCreateKernel(const core::VariablePtr& var, const core::ExpressionPtr& call);
    void lookForKernelFilePragma(const core::TypePtr& type, const core::ExpressionPtr& createProgramWithSource, const core::StatementPtr& annotated);

public:
    HostMapper(core::ASTBuilder& build, core::ProgramPtr& program);

    const core::NodePtr resolveElement(const core::NodePtr& element);
    ClmemTable& getClMemMapping() { return cl_mems; }

    const vector<core::ExpressionPtr>& getKernels() { return kernelEntries; }
    KernelArgs& getKernelArgs() { return kernelArgs; }
    KernelNames& getKernelNames() { return kernelNames; }

};

class Host2ndPass {
    KernelNames& kernelNames;
    KernelLambdas kernelLambdas;

public:
    Host2ndPass(KernelNames& oclKernelNames, ClmemTable clMemTable, core::ASTBuilder& build) : kernelNames(oclKernelNames) { }
    void mapNamesToLambdas(const vector<core::ExpressionPtr>& kernelEntries);

    KernelNames& getKernelNames() { return kernelNames; }
    KernelLambdas& getKernelLambdas() { return kernelLambdas; }
};

class HostMapper3rdPass : public core::transform::CachedNodeMapping {
    const core::ASTBuilder& builder;
    ClmemTable& cl_mems;
    KernelArgs& kernelArgs;
    KernelNames& kernelNames;
    KernelLambdas& kernelLambdas;

    core::ExpressionPtr create3Dvec;

    void getVarOutOfCrazyInspireConstruct(core::ExpressionPtr& arg);

    const core::ExpressionPtr anythingToVec3(core::ExpressionPtr workDim, core::ExpressionPtr size);

public:
    HostMapper3rdPass(const core::ASTBuilder build, ClmemTable& clMemTable, KernelArgs& oclKernelArgs, KernelNames& oclKernelNames,
            KernelLambdas& oclKernelLambdas);

    const core::NodePtr resolveElement(const core::NodePtr& element);

};

/*
class HostVisitor : public core::AddressVisitor<void> {
    core::ASTBuilder& builder;
    core::ProgramPtr& newProg;

public:
    HostVisitor(core::ASTBuilder& build, core::ProgramPtr& prog) : core::AddressVisitor<void>(false), builder(build), newProg(prog) {};

    core::ProgramPtr getNewProg() { return newProg; }

    void visitCallExpr(const core::CallExprAddress& callExp);

};*/
}

class HostCompiler {
    core::ProgramPtr& mProgram;
//    frontend::Program& mProg;
    core::ASTBuilder builder;

public:
    HostCompiler(core::ProgramPtr& program, core::NodeManager& mgr): mProgram(program), builder(mgr) {}

    core::ProgramPtr compile();
};

} //namespace ocl
} //namespace frontend
} //namespace insieme
