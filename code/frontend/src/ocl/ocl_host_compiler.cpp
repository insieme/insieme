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

#include "insieme/core/expressions.h"
#include "insieme/core/ast_node.h"

#include "insieme/core/transform/node_replacer.h"

#include "insieme/c_info/naming.h"
#include "insieme/c_info/location.h"
#include "insieme/frontend/ocl/ocl_host_compiler.h"

namespace ba = boost::algorithm;

namespace insieme {
namespace frontend {
namespace ocl {
using namespace insieme::core;

namespace {

ExpressionPtr Ocl2Inspire::getClCreateBuffer() {
    return parser.parseExpression("fun(type<'a>:elemType, uint<8>:flags, uint<8>:size, anyRef:hostPtr, ref<array<int<4>, 1> >:errorcode_ret) -> array<'a, 1>  {{ \
            ( (op<array.ref.elem.1D>(errorcode_ret, lit<uint<8>, 0> )) = 0 ); \
            return (op<array.create.1D>( elemType, size )); \
       }}");
}


HostMapper::HostMapper(ASTBuilder& build) : builder(build), o2i(build.getNodeManager()) {
    ADD_Handler(builder, "clCreateBuffer",
        ExpressionPtr fun = o2i.getClCreateBuffer();

        // extract the size form argument size, relying on it using a multiple of sizeof(type)
        ExpressionPtr size;
        TypePtr type;
        if(CallExprPtr&& mul = dynamic_pointer_cast<const CallExpr>(node->getArgument(2))) {
            for(int i = 0; i < 2; ++i) {
                if(CallExprPtr&& sizeof_ = dynamic_pointer_cast<const CallExpr>(mul->getArgument(i))) {
                    if(sizeof_->toString().find("sizeof") != string::npos) {
                        // extract the type to be allocated
                        type = dynamic_pointer_cast<const Type>(sizeof_->getArgument(0)->getType()->getChildList().at(0));
                        // extract the number of elements to be allocated
                        size = mul->getArgument(1-i);
                    }
                }
            }
        }

        assert(type && "Unable to deduce type from clCreateBuffer call:\nNo sizeof call found, cannot translate to INSPIRE.");

        vector<ExpressionPtr> args;
        args.push_back(BASIC.getTypeLiteral(type));//(BASIC.getTypeLiteral(BASIC.getUInt4()))
        args.push_back(node->getArgument(1));
        args.push_back(size);
        args.push_back(node->getArgument(3));
        args.push_back(node->getArgument(4));
        return builder.callExpr(builder.arrayType(type), fun, args);/*builder.callExpr(BASIC.getRefVar(), */
    );
};


const NodePtr HostMapper::resolveElement(const NodePtr& element) {
    // stopp recursion at type level
    if (element->getNodeCategory() == NodeCategory::NC_Type) {
        return element->substitute(builder.getNodeManager(), *this);
    }
/*
    if(DeclarationStmtPtr decl = dynamic_pointer_cast<const DeclarationStmt>(element)) {
        const VariablePtr var = decl->getVariable();
        if(var->getType() == builder.refType(builder.arrayType(builder.genericType("_cl_mem")))) {
            std::cout << "Found cl_mem: " << var->toString() << std::endl;

            cl_mems.insert(std::make_pair(var, NULL));
        }

    }
*/
    if(const CallExprPtr& callExpr = dynamic_pointer_cast<const CallExpr>(element)){
 //       std::cout << callExpr->toString() << " FOUND\n";
        const ExpressionPtr& fun = callExpr->getFunctionExpr();
        vector<ExpressionPtr> args = callExpr->getArguments();

        if(const LiteralPtr& literal = dynamic_pointer_cast<const Literal>(fun)) {
            callExpr->substitute(builder.getNodeManager(), *this);
//            std::cout << "CALL: " << literal->getValue() << std::endl;
            if(HandlerPtr replacement = handles[literal->getValue()]) {
                return replacement->handleNode(callExpr);
            }
        }

        if(callExpr->getFunctionExpr() == BASIC.getRefAssign()) {
            if(const VariablePtr& lhs = dynamic_pointer_cast<const Variable>(callExpr->getArgument(0))) {
                if(lhs->getType() == builder.refType(builder.arrayType(builder.genericType("_cl_mem")))) {
                    CallExprPtr newCall = dynamic_pointer_cast<const CallExpr>(callExpr->substitute(builder.getNodeManager(), *this));

                    // get rid of deref operations, automatically inserted by the frontend coz _cl_mem* is translated to ref<array<...>>, and refs cannot be
                    // rhs of an assignment
                    if(const CallExprPtr& rhs = dynamic_pointer_cast<const CallExpr>(newCall->getArgument(1))) {
                        if(rhs->getFunctionExpr() == BASIC.getRefDeref()) {
                            if(const CallExprPtr& createBuffer = dynamic_pointer_cast<const CallExpr>(rhs->getArgument(0)))
                                newCall = createBuffer;
                        }
                    }

                    cl_mems.insert(std::make_pair(lhs, builder.variable(builder.refType(newCall->getArgument(1)->getType()))));

                    return newCall;
                }
            }
        }

    }

    return element->substitute(builder.getNodeManager(), *this);
}

void HostVisitor::visitCallExpr(const CallExprAddress& callExpr) {
    std::cout << callExpr->toString() << " FOUND\n";
//    newProg = dynamic_pointer_cast<const Program>(transform::replaceNode(builder.getNodeManager(), callExpr, builder.getThreadId(), true));
    std::cout << "CALL: " << callExpr->getChildList().at(0) << " Type: " << callExpr->getChildList().at(0)->getNodeType() << std::endl;
}

}
ProgramPtr HostCompiler::compile() {
//    HostVisitor oclHostVisitor(builder, mProgram);
    HostMapper oclHostMapper(builder);

//    visitAll(ProgramAddress(mProgram), oclHostVisitor);

//    mProgram = oclHostVisitor.getNewProg();

//    return mProgram;

    const NodePtr progNode = oclHostMapper.mapElement(0, mProgram);

    if(ProgramPtr newProg = dynamic_pointer_cast<const Program>(progNode)) {
        mProgram = newProg;
        return newProg;
    }
    else
        assert(newProg && "OclHostCompiler corrupted the program");
    return mProgram;
}

} //namespace ocl
} //namespace frontend
} //namespace insieme
