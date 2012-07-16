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
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_program.h"

namespace insieme {
namespace frontend {
namespace ocl {

typedef std::pair<core::VariableList, core::ExpressionList> ArgList;

namespace {

// shortcut
#define BASIC builder.getNodeManager().getLangBasic()

// uniform initialization of 3D vecotr of type uint<4>
/*#define INT3DVECINIT(strVal)  builder.vectorExpr(toVector<core::ExpressionPtr>(builder.literal(BASIC.getInt4(), strVal), \
                              builder.literal(BASIC.getInt4(), strVal), builder.literal(BASIC.getInt4(), strVal)))
*/
// accesses array arr at index idx
#define SUBSCRIPT(arr, idx, builder) builder.arrayAccess(arr, builder.literal(toString(idx), builder.getNodeManager().getLangBasic().getUInt8() ))

// adding arguments and their value to the ArgList
#define ADD_PARAM(list, arg, val) { list.first.push_back(arg); \
                                    list.second.push_back(val); }


// store a the variable var in vector vec and overvrites var with a new variable. The mapping from the old to the new one is store in list
#define ADD_ARG(vec, var, types) { core::VariablePtr tmp = builder.variable((var)->getType()); \
                                    vec.first.push_back(var); \
                                    vec.second.push_back(tmp); \
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
    const core::IRBuilder& builder;
    // loop bounds
    core::VariablePtr globalRange; bool globalRangeUsed;
    core::VariablePtr numGroups; bool numGroupsUsed;
    core::VariablePtr localRange; bool localRangeUsed;

    core::CallExprPtr vecAccess(core::VariablePtr& vec, core::ExpressionPtr& idx) {
        return  builder.arrayAccess(vec, idx);

 //       		builder.callExpr(builder.getNodeManager().getLangBasic().getUInt8(), builder.getNodeManager().getLangBasic().getVectorSubscript(),
 //               toVector<core::ExpressionPtr>(vec, idx) );
    }

    static core::VariablePtr get3DvecVar(const core::IRBuilder& builder) {
        return builder.variable(builder.vectorType(builder.getNodeManager().getLangBasic().getUInt8(), builder.concreteIntTypeParam(static_cast<size_t>(3))));
    }

    //default constructor
    KernelData(const core::IRBuilder& astBuilder) :
        builder(astBuilder), globalRange(get3DvecVar(astBuilder)), numGroups(get3DvecVar(astBuilder)), localRange(get3DvecVar(astBuilder)) {
        globalRangeUsed = false;
        numGroupsUsed = false;
        localRangeUsed = false;
    };

    // calculates the 1D index for the 0th dimension
    core::CallExprPtr calcIdidx0(core::VariablePtr& threadId, core::VariablePtr& boundaries);

    // calculates the 1D index for the 1st dimension
    core::CallExprPtr calcIdidx1(core::VariablePtr& threadId, core::VariablePtr& boundaries);

    // calculates the 1D index for the 2nd dimension
    core::CallExprPtr calcIdidx2(core::VariablePtr& threadId, core::VariablePtr& boundaries);

    //returns a vector containing declarations with fresh initializations of all needed ocl-variables
    void appendArguments(std::pair<std::vector<core::VariablePtr>, std::vector<core::ExpressionPtr> >& argList, OCL_SCOPE scope, core::TypeList& aTypes);

    //returns a vector containing declarations with fresh initializations of all needed ocl-variables
    void appendShared(std::vector<core::DeclarationStmtPtr>& sharingList, OCL_SCOPE scope);

    //returns a call expression accessing the wished range at index idx and sets the appropriate Used flag
    core::CallExprPtr accessRange(OCL_PAR_LEVEL level, core::ExpressionPtr idx);

    //returns a call expression calculating the global id from the pfor loops variable and local range
    core::CallExprPtr accessId(OCL_PAR_LEVEL level, core::ExpressionPtr idx);

    //returns a call expression to one or two barrier function
    core::CallExprPtr callBarrier(const core::ExpressionPtr& memFence);
};
} // namespace

class Compiler {
private:
//    class OclVisitor;

    core::ProgramPtr& mProgram;
    core::IRBuilder builder;
 //   core::IRVisitor visitor;


public:
    Compiler(core::ProgramPtr& program, core::NodeManager& mgr) : mProgram(program), builder(mgr) {}

    core::ProgramPtr lookForOclAnnotations();

    core::ProgramPtr getProgram() { return mProgram; }
};

// responsible for adding ocl KernelFile annotation
void attatchOclAnnotation(const core::StatementPtr& irNode, const clang::Stmt* clangNode,
        frontend::conversion::ConversionFactory& fact);


} //namespace ocl
} //namespace frontend
} //namespace insieme
