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

#include "expressions.h"
#include "ast_node.h"
#include "naming.h"
#include "ocl/ocl_compiler.h"
#include "ocl/ocl_annotations.h"
#include "utils/types_lenght.h"

namespace insieme {
namespace frontend {
namespace ocl {

namespace {

/* does not work at all
class OclVisitor : public core::ASTVisitor<core::ProgramPtr> {
private:
    const core::ASTBuilder& builder;
    core::ProgramPtr& program;

public:

    OclVisitor(core::ASTBuilder& astBuilder, core::ProgramPtr program) : builder(astBuilder), program(program){ }

    core::ProgramPtr visitLambdaExpr(const core::LambdaExprPtr& func) {
        auto funcAnnotation = func.getAnnotation(ocl::BaseAnnotation::KEY);
        if(funcAnnotation) {

            bool isKernelFunction;
            bool workGroupSizeDefined = false;
            size_t wgs[3];
            ocl::BaseAnnotation::AnnotationList::const_iterator I = funcAnnotation->getListBegin();
            for(ocl::BaseAnnotation::AnnotationList::const_iterator I = funcAnnotation->getListBegin(), E = funcAnnotation->getListEnd();
                I != E; ++I) {
                ocl::AnnotationPtr annot = (*I);

                if(ocl::WorkGroupSizeAnnotationPtr wgsap = std::dynamic_pointer_cast<ocl::WorkGroupSizeAnnotation>(annot)) {
                    workGroupSizeDefined = true;
                    wgs[0] = wgsap->getXdim();
                    assert(wgs[0] > 0 && "Work group Size x-dimension has to be greater than 0.");
                    wgs[1] = wgsap->getYdim();
                    assert(wgs[1] > 0 && "Work group Size y-dimension has to be greater than 0.");
                    wgs[2] = wgsap->getZdim();
                    assert(wgs[2] > 0 && "Work group Size z-dimension has to be greater than 0.");
                }

                if(ocl::KernelFctAnnotationPtr kf = std::dynamic_pointer_cast<ocl::KernelFctAnnotation>(annot)) {
                    isKernelFunction = kf->isKernelFct();
                }

            }
            if(!isKernelFunction)
                return program;

            //Todo remove
            const core::LambdaExpr kernel = *func;

            core::LambdaExpr::ParamList params = kernel.getParams();

            // add vector<uint<4>,3> globalRange and localRange to parameters
            // params.push_back

            const core::StatementPtr& body = kernel.getBody();
            //Todo remove
            const core::Statement& bodydummy = *kernel.getBody();
            const core::Node::ChildList& children = bodydummy.getChildList();

            std::for_each(children.begin(), children.end(),
                    [&builder] (const core::NodePtr& curr) {
                //look for ocl buildin functions and translate them to IR statements

                }
            );

            //add three parallel statements for the localRange
            core::JobExprPtr localZjob = builder.jobExpr(body);

            core::LambdaExprPtr newFunc = builder.lambdaExpr(kernel.getType(), params, localZjob);

            core::NodePtr newProgram = core::transform::replaceNode(builder, program, func, newFunc);

            std::cout << "replaced -" << newProgram->toString() << "-\n";



            if(core::ProgramPtr np = dynamic_pointer_cast<const core::Program>(newProgram)) {
            std::cout << " do it\n";
                //if replaced function was an entry point, update entry points
                core::Program::EntryPointSet entryList = program->getEntryPoints();

                for(std::unordered_set<core::ExpressionPtr>::iterator it = entryList.begin(), end = entryList.end();
                    it != end; it ++) {
                    if(*it == func) {
                        np = core::Program::remEntryPoint(*builder.getNodeManager(), np, func);
                        np = core::Program::addEntryPoint(*builder.getNodeManager(), np, newFunc);
                    }
                }
                program = np;
                return np;
          } else {
               assert(false && "OclCompiler corrupted program");
          }

        }
        return program;
    }
};
*/

class KernelMapper : public core::NodeMapping {
    const core::ASTBuilder& builder;

public:

    KernelMapper(core::ASTBuilder& astBuilder)
    : builder(astBuilder) { };

    const core::NodePtr mapElement(unsigned, const core::NodePtr& element) {
std::cout << "found \t";

        if(core::dynamic_pointer_cast<const core::LambdaExpr>(element)){
            std::cout << "the function \n";
            return element;
        }
        if(core::dynamic_pointer_cast<const core::FunctionType>(element)){
            std::cout << "the type \n";
            return element;
        }
        if(core::StatementPtr body = dynamic_pointer_cast<const core::Statement>(element)){
            std::cout << "the body\n";
/* do this recursively
            const core::Node::ChildList& children = body->getChildList();

            //&builder should be captured, but is member variable
            std::for_each(children.begin(), children.end(),
                    [] (const core::NodePtr& curr) {
                //look for ocl buildin functions and translate them to IR statements

                }
            );*/
            //add three parallel statements for the localRange
 //           if(core::StatementPtr newBody = dynamic_pointer_cast<const core::Statement>(body->substitute(*builder.getNodeManager(), *this))){
                core::JobExprPtr localZjob = builder.jobExpr(body);
                core::LambdaExpr::ParamList params;
                params.push_back(localZjob);
//                core::LambdaExpr localZparallel = builder.l
     //           core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, params,localZjob);

                return body;
/*            }
            else
                assert(newBody && "KenrnelMapper corrupted function body.");*/
        }

        if(false){
/*            core::LambdaExpr::ParamList params = func->getParams();

            // add vector<uint<4>,3> globalRange and localRange to parameters

            core::IntTypeParam vecSize = core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3));
            core::VariablePtr globalRange = builder.variable(builder.vectorType(builder.getUIntType( INT_LENGTH ), vecSize));
            params.push_back(globalRange);
            core::VariablePtr localRange = builder.variable(builder.vectorType(builder.getUIntType( INT_LENGTH ), vecSize));
            params.push_back(localRange);

            // update the type of the function
            core::FunctionTypePtr newFuncType;
            if(core::FunctionTypePtr funcType = core::dynamic_pointer_cast<const core::FunctionType>(func->getType())){
                core::TypePtr retTy = funcType->getReturnType();

                //check return type
                assert(retTy->getName() == "unit" && "Return type of kernel functions must be void.");

                core::TupleType::ElementTypeList args = funcType->getArgumentType()->getElementTypes();
                args.push_back(globalRange->getType());
                args.push_back(localRange->getType());

                newFuncType = builder.functionType(builder.tupleType(args), retTy);
            } else {
                assert(funcType && "Function has unexpected type");
            }*/
        }

//          newFuncType = func->substitute(*builder.getNodeManager(), kernelMapper);
//          core::FunctionTypePtr funcTy = builder.functionType( builder.tupleType(elemTy), retTy);


        return element;
    }

/*    const core::NodePtr mapElement(unsigned, const core::FunctionType& funcType) {
       std::cout << "found parameters\n";
        core::IntTypeParam vecSize = core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3));
        core::VariablePtr globalRange = builder.variable(builder.vectorType(builder.getUIntType( INT_LENGTH ), vecSize));
        params.push_back(globalRange);
        core::VariablePtr localRange = builder.variable(builder.vectorType(builder.getUIntType( INT_LENGTH ), vecSize));
        params.push_back(localRange);

        // update the type of the function
        core::FunctionTypePtr newFuncType;
        core::TypePtr retTy = funcType->getReturnType();

        //check return type
        assert(retTy->getName() == "unit" && "Return type of kernel functions must be void.");

        core::TupleType::ElementTypeList args = funcType->getArgumentType()->getElementTypes();
        args.push_back(globalRange->getType());
        args.push_back(localRange->getType());

        return builder.functionType(builder.tupleType(args), retTy)
        return funcType;
    }*/
};

class OclMapper : public core::NodeMapping {
    KernelMapper kernelMapper;
    const core::ASTBuilder& builder;
//    const core::Substitution::Mapping& mapping;


public:

    OclMapper(core::ASTBuilder& astBuilder)
        : kernelMapper(astBuilder), builder(astBuilder) { };

    const core::NodePtr mapElement(unsigned, const core::NodePtr& element) {
        // quick check - stop recursion at variables
        if (element->getNodeCategory() == core::NodeCategory::NC_Type) {
            return element;//->substitute(builder.getNodeManager(), *this);
        }


        //TODO keep annotations when copying, element should not be used after this call

        // check if we are at a function node
        if(core::LambdaExprPtr func = dynamic_pointer_cast<const core::LambdaExpr>(element)){
//        if(newNode->getNodeType() == core::NodeType::NT_LambdaExpr && false){
//            return builder.lambdaExpr(func->getType(), func->getParams(), builder.compoundStmt());
//return builder.lambdaExpr(func->getType(), func->getParams(), builder.compoundStmt());;
            core::AnnotationMap map = element.getAnnotations();

            bool isKernelFunction = false;
            bool workGroupSizeDefined = false;
            auto funcAnnotation = element.getAnnotation(ocl::BaseAnnotation::KEY);
            if(funcAnnotation) {

                size_t wgs[3];
                for(ocl::BaseAnnotation::AnnotationList::const_iterator I = funcAnnotation->getAnnotationListBegin(),
                        E = funcAnnotation->getAnnotationListEnd(); I != E; ++I) {
                    ocl::AnnotationPtr annot = (*I);

                    if(ocl::WorkGroupSizeAnnotationPtr wgsap = std::dynamic_pointer_cast<ocl::WorkGroupSizeAnnotation>(annot)) {
                        workGroupSizeDefined = true;
                        wgs[0] = wgsap->getXdim();
                        assert(wgs[0] > 0 && "Work group Size x-dimension has to be greater than 0.");
                        wgs[1] = wgsap->getYdim();
                        assert(wgs[1] > 0 && "Work group Size y-dimension has to be greater than 0.");
                        wgs[2] = wgsap->getZdim();
                        assert(wgs[2] > 0 && "Work group Size z-dimension has to be greater than 0.");
                    }
                    if(ocl::KernelFctAnnotationPtr kf = std::dynamic_pointer_cast<ocl::KernelFctAnnotation>(annot)) {
                        isKernelFunction = kf->isKernelFct();
                    }

                }
            }

            assert(!(workGroupSizeDefined & !isKernelFunction) && "Attribute Reqd_work_group_size can only be defined for kernel functions");

            //if function is not a OpenCL kernel function recursively check for child nodes
            if(!isKernelFunction) {
                return element->substitute(*builder.getNodeManager(), *this);
            } else {
                // call for kernel function mapper for subnodes
                return element->substitute(*builder.getNodeManager(), kernelMapper);
            }


            //TODO handle subnodes.
            //Maybe prettier in another mapper
            const core::StatementPtr& body = func->getBody();
            const core::Node::ChildList& children = body->getChildList();

			//&builder should be captured, but is member variable
            std::for_each(children.begin(), children.end(),
                    [] (const core::NodePtr& curr) {
                //look for ocl buildin functions and translate them to IR statements

                }
            );

            core::LambdaExpr::ParamList params = func->getParams();

            // add vector<uint<4>,3> globalRange and localRange to parameters

            core::IntTypeParam vecSize = core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3));
            core::VariablePtr globalRange = builder.variable(builder.vectorType(builder.getUIntType( INT_LENGTH ), vecSize));
            params.push_back(globalRange);
            core::VariablePtr localRange = builder.variable(builder.vectorType(builder.getUIntType( INT_LENGTH ), vecSize));
            params.push_back(localRange);

            // update the type of the function
            core::FunctionTypePtr newFuncType;
            if(core::FunctionTypePtr funcType = core::dynamic_pointer_cast<const core::FunctionType>(func->getType())){
                core::TypePtr retTy = funcType->getReturnType();

                //check return type
                assert(retTy->getName() == "unit" && "Return type of kernel functions must be void.");

                core::TupleType::ElementTypeList args = funcType->getArgumentType()->getElementTypes();
                args.push_back(globalRange->getType());
                args.push_back(localRange->getType());

                newFuncType = builder.functionType(builder.tupleType(args), retTy);
            } else {
                assert(funcType && "Function has unexpected type");
            }

  //          newFuncType = func->substitute(*builder.getNodeManager(), kernelMapper);
  //          core::FunctionTypePtr funcTy = builder.functionType( builder.tupleType(elemTy), retTy);


            //add three parallel statements for the localRange
            core::JobExprPtr localZjob = builder.jobExpr(body);
            core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, params,localZjob);

            return newFunc;
        }

        return element->substitute(*builder.getNodeManager(), *this);
    }

};

}

core::ProgramPtr Compiler::lookForOclAnnotations() {
//    core::RecursiveASTVisitor<OclVisitor> visitor(oclAnnotationExpander);
//    core::visitAll(mProgram, oclAnnotationExpander);

    OclMapper oclAnnotationExpander(builder);
//    visitor.visit(mProgram);
    const core::NodePtr progNode = oclAnnotationExpander.mapElement(0, mProgram);
    if(core::ProgramPtr newProg = dynamic_pointer_cast<const core::Program>(progNode)) {
        mProgram = newProg;
        return newProg;
    }
    else
        assert(newProg && "OclCompiler corrupted the program");
    return mProgram;
}

} //namespace ocl
} //namespace frontend
} //namespace insieme
