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

#include <vector>

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/program.h"

namespace insieme {
namespace frontend {
namespace ocl {

namespace {

// shortcut
#define BASIC builder.getNodeManager().basic

// uniform initialization of 3D vecotr of type uint<4>
#define INT3DVECINIT(strVal)  builder.vectorExpr(toVector<core::ExpressionPtr>(builder.literal(BASIC.getInt4(), strVal), \
                              builder.literal(BASIC.getInt4(), strVal), builder.literal(BASIC.getInt4(), strVal)))

// accesses array arr at index idx
#define SUBSCRIPT(arr, idx, builder) builder.callExpr(builder.getNodeManager().basic.getUInt4(), builder.getNodeManager().basic.getVectorSubscript(), \
                                     toVector<core::ExpressionPtr>(arr, builder.castExpr(BASIC.getUInt4(), \
                                     builder.literal(toString(idx), builder.getNodeManager().basic.getUInt4() ))))

// store a the variable var in vector vec and overvrites var with a new variable. The mapping from the old to the new one is store in list
#define CAPTURE(vec, var, types) { core::VariablePtr tmp = builder.variable((var)->getType()); \
                                    vec[var] = tmp; \
                                    types.push_back((var)->getType()); \
                                    var = tmp; }

// generates a declaration of variable var which initialized with a new variable and stored in vector vec. The new variable is stored in var
#define SHARE(vec, var) { const core::VariablePtr initVal = builder.variable((var)->getType()); \
                          vec.push_back(builder.declarationStmt((var), initVal)); \
                          (var) = initVal; /* update inVec with new variables */ }

enum OCL_SCOPE { OCL_LOCAL_PAR, OCL_LOCAL_JOB, OCL_GLOBAL_PAR, OCL_GLOBAL_JOB };
enum OCL_PAR_LEVEL { OPL_GLOBAL, OPL_GROUP, OPL_LOCAL };
enum OCL_ADDRESS_SPACE { CONSTANT, GLOBAL, LOCAL, PRIVATE };

struct KernelData {
public:
    core::ASTBuilder& builder;
    // loop bounds
    core::VariablePtr globalRange; bool globalRangeUsed;
    core::VariablePtr numGroups; bool numGroupsUsed;
    core::VariablePtr localRange; bool localRangeUsed;

    core::CallExprPtr vecAccess(core::VariablePtr& vec, core::ExpressionPtr& idx) {
        return builder.callExpr(builder.getNodeManager().basic.getUInt4(), builder.getNodeManager().basic.getVectorSubscript(),
                toVector<core::ExpressionPtr>(vec, idx) );
    }

    static core::VariablePtr get3DvecVar(core::ASTBuilder& builder) {
        return builder.variable(builder.vectorType(builder.getNodeManager().basic.getUInt4(), core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3))));
    }

    //default constructor
    KernelData(core::ASTBuilder& astBuilder) :
        builder(astBuilder), globalRange(get3DvecVar(astBuilder)), numGroups(get3DvecVar(astBuilder)), localRange(get3DvecVar(astBuilder)) {
        globalRangeUsed = false;
        numGroupsUsed = false;
        localRangeUsed = false;
    };

    //returns a vector containing declarations with fresh initializations of all needed ocl-variables
    void appendCaptures(core::ASTBuilder::CaptureInits& captureList, OCL_SCOPE scope, core::TypeList types);

    //returns a vector containing declarations with fresh initializations of all needed ocl-variables
    void appendShared(std::vector<core::DeclarationStmtPtr>& captureList, OCL_SCOPE scope);

    //returns a call expression accessing the wished range at index idx and sets the appropriate Used flag
    core::CallExprPtr accessRange(OCL_PAR_LEVEL level, core::ExpressionPtr idx);

    //returns a call expression calculating the global id from the pfor loops variable and local range
    core::CallExprPtr accessId(OCL_PAR_LEVEL level, core::ExpressionPtr idx);

    //returns a call expression to a merge function
    core::CallExprPtr callBarrier(core::ExpressionPtr memFence);
};
} // namespace

class Compiler {
private:
//    class OclVisitor;

    core::ProgramPtr mProgram;
    core::ASTBuilder builder;
 //   core::ASTVisitor visitor;


public:
    Compiler(const core::ProgramPtr& program, core::NodeManager& mgr) : mProgram(program), builder(mgr) {}

    core::ProgramPtr lookForOclAnnotations();

    core::ProgramPtr getProgram() { return mProgram; }
};

} //namespace ocl
} //namespace frontend
} //namespace insieme
