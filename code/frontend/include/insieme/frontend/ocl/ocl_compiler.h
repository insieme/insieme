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

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/program.h"

namespace insieme {
namespace frontend {
namespace ocl {

namespace {
#define SUBSCRIPT(vec, idx) builder.callExpr(core::lang::TYPE_UINT_4_PTR, core::lang::OP_SUBSCRIPT_SINGLE_PTR, toVector<core::ExpressionPtr>( \
                            vec, builder.literal(toString(idx), core::lang::TYPE_UINT_4_PTR )))


struct KernelData {
public:
    core::ASTBuilder builder;
    // loop bounds
    core::VariablePtr globalRange;
    core::VariablePtr groupSize;
    core::VariablePtr localRange;
    // loop variables
    core::VariablePtr groupId;
    core::VariablePtr localId;
    // thread gropus
    core::VariablePtr groupTg;
    core::VariablePtr localTg;

    int test;

    static core::VariablePtr get3DvecVar(core::ASTBuilder builder)
    {
        return builder.variable(builder.vectorType(builder.uintType(4), core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3))));
    }

    //default constructor
    KernelData(core::ASTBuilder astBuilder) :
        globalRange(get3DvecVar(astBuilder)), groupSize(get3DvecVar(astBuilder)), localRange(get3DvecVar(astBuilder)),
        groupId(get3DvecVar(astBuilder)), localId(get3DvecVar(astBuilder)),
        groupTg(get3DvecVar(astBuilder)), localTg(get3DvecVar(astBuilder)){ };

    //copy constructor
    KernelData(KernelData& in) :
        globalRange(in.globalRange), groupSize(in.groupSize), localRange(in.localRange),
        groupId(in.groupId), localId(in.localId), groupTg(in.groupTg), localTg(in.localTg) { };


    void set(core::VariablePtr& globalR, core::VariablePtr& groupS, core::VariablePtr& localR,
            core::VariablePtr& groupI, core::VariablePtr& localI, core::VariablePtr& groupThreadGroup, core::VariablePtr& localThreadGroup) {
                   globalRange = globalR, groupSize = groupS, localRange = localR;
                   groupId = groupI, localId = localI, groupTg = groupThreadGroup, localTg = localThreadGroup;
    }
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
