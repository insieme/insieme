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

    //prototype for parallel function, just for testing
    core::LambdaExprPtr fakePar;

    //TODO remove these two, should be empty anyway
    std::vector<core::VariablePtr> constantVars;
    std::vector<core::VariablePtr> globalVars;

    std::vector<core::VariablePtr> localVars;
    std::vector<core::VariablePtr> privateVars;

private:
    void append(std::vector<core::VariablePtr>& sink, std::vector<core::VariablePtr>& source) {
        for(std::vector<core::VariablePtr>::iterator I = source.begin(), E = source.end(); I != E; I++) {
            sink.push_back(*I);
        }
    }

public:


    KernelMapper(core::ASTBuilder& astBuilder)
    : builder(astBuilder) {
        core::LambdaExpr::ParamList params;
        //TODO remove dirty workaround
//        params.push_back(builder.variable(builder.jobType()));

        core::TupleType::ElementTypeList args;
//        args.push_back(builder.jobType());

        builder.functionType(builder.tupleType(args), builder.getUnitType());//, builder.compoundStmt());

        fakePar = builder.lambdaExpr(builder.functionType(builder.tupleType(args), builder.getUnitType()), params, builder.compoundStmt());
    };

    const core::NodePtr mapElement(unsigned, const core::NodePtr& element) {
//std::cout << "\t * found " << element->toString() << std::endl;

        if(core::dynamic_pointer_cast<const core::LambdaExpr>(element)){
            std::cout << "the function \n";
            return element;
        }
        if(core::dynamic_pointer_cast<const core::FunctionType>(element)){
 //           std::cout << "the type \n";
            return element;
        }
        if (core::DeclarationStmtPtr decl = dynamic_pointer_cast<const core::DeclarationStmt>(element)) {
//            std::cout << "a variable declaration " << (element.hasAnnotation(ocl::BaseAnnotation::KEY) ? "with . attributes " : " -  ") <<
//                    (element->hasAnnotation(ocl::BaseAnnotation::KEY) ? "with -> attributes \n" : " -  \n");
//            std::cout << "variable: " << decl->toString() << std::endl;

            if(decl->getVariable()->hasAnnotation(ocl::BaseAnnotation::KEY)) {
                ocl::BaseAnnotationPtr annot = decl->getVariable()->getAnnotation(ocl::BaseAnnotation::KEY);
                for(ocl::BaseAnnotation::AnnotationList::const_iterator I = annot->getAnnotationListBegin(),
                        E = annot->getAnnotationListEnd(); I != E; ++I) {
                    if(ocl::AddressSpaceAnnotationPtr asa = std::dynamic_pointer_cast<ocl::AddressSpaceAnnotation>(*I)) {
                        switch(asa->getAddressSpace()) {
                        case ocl::AddressSpaceAnnotation::LOCAL: {
                            localVars.push_back(decl->getVariable());
                            break;
                        }
                        case ocl::AddressSpaceAnnotation::PRIVATE: {
                            privateVars.push_back(decl->getVariable());
                            //TODO remove declaration from source code
                            break;
                        }
                        case ocl::AddressSpaceAnnotation::CONSTANT: {
                            assert(false && "Address space CONSTANT not allowed for local variables");
                        }
                        case ocl::AddressSpaceAnnotation::GLOBAL: {
                            assert(false && "Address space GLOBAL not allowed for local variables");
                        }
                        default:
                            assert(false && "Unexpected OpenCL address space attribute for local variable");
                        }
                    } else {
                        assert(false && "No other OpenCL attribute than oclAddressSpaceAnnotation allowed for variables");
                    }

                }
            }
            return element;//->substitute(builder.getNodeManager(), *this);
        }
/*
        if(core::VariablePtr var = dynamic_pointer_cast<const core::Variable>(element)) {
            core::Variable v = *var;
            std::cout << "VarID: " << v.getId() << " VarType: " << v.getType() << std::endl;
        }
*/
        if(core::CompoundStmtPtr body = dynamic_pointer_cast<const core::CompoundStmt>(element->substitute(*builder.getNodeManager(), *this))){
            std::cout << "the body\n";
/* do this recursively
            const core::Node::ChildList& children = body->getChildList();

            //&builder should be captured, but is member variable
            std::for_each(children.begin(), children.end(),
                    [] (const core::NodePtr& curr) {
                //look for ocl buildin functions and translate them to IR statements

                }
            );*/
            //TODO add three parallel statements for the localRange
 //           if(core::StatementPtr newBody = dynamic_pointer_cast<const core::Statement>(body/*->substitute(*builder.getNodeManager(), *this)*/)){
                core::JobExprPtr localZjob = builder.jobExpr(body);
                std::vector<core::ExpressionPtr> expr;
//                expr.push_back(localZjob);

                core::ExpressionPtr p = builder.callExpr(dynamic_pointer_cast<const core::FunctionType>(fakePar->getType())->getReturnType(), fakePar, expr);

                core::StatementPtr parBody = builder.compoundStmt(p);

                return parBody;

                return body;
//                core::LambdaExpr localZparallel = builder.l
     //           core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, params,localZjob);
            }
 //           else
   //             assert(body && "KenrnelMapper corrupted function body.");
//        }

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

    void getMemspaces(std::vector<core::VariablePtr>& constantV, std::vector<core::VariablePtr>& globalV,
            std::vector<core::VariablePtr>& localV, std::vector<core::VariablePtr>& privateV){
        append(globalV, globalVars);
        append(constantV, constantVars);
        append(localV, localVars);
        append(privateV, privateVars);
    }

    void resetMemspaces() {
        globalVars.clear();
        constantVars.clear();
        localVars.clear();
        privateVars.clear();
    }
};

class OclMapper : public core::NodeMapping {
    KernelMapper kernelMapper;
    const core::ASTBuilder& builder;
//    const core::Substitution::Mapping& mapping;

    //prototype for parallel function, just for testing
    core::LambdaExprPtr fakePar;

    std::vector<core::VariablePtr> constantVars;
    std::vector<core::VariablePtr> globalVars;
    std::vector<core::VariablePtr> localVars;
    std::vector<core::VariablePtr> privateVars;

public:

    OclMapper(core::ASTBuilder& astBuilder)
        : kernelMapper(astBuilder), builder(astBuilder) {
        core::LambdaExpr::ParamList params;

        //TODO remove dirty workaround
//        params.push_back(builder.variable(builder.jobType()));

        core::TupleType::ElementTypeList args;
//        args.push_back(builder.jobType());

        builder.functionType(builder.tupleType(args), builder.getUnitType());//, builder.compoundStmt());

        fakePar = builder.lambdaExpr(builder.functionType(builder.tupleType(args), builder.getUnitType()), params, builder.compoundStmt());

    };

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
            }


            core::LambdaExpr::ParamList params = func->getParams();

            // store memory spaces of arguments
            for(core::LambdaExpr::ParamList::iterator pi = params.begin(), pe = params.end(); pi != pe; pi++) {
                core::VariablePtr var = *pi;
                if(var->hasAnnotation(ocl::BaseAnnotation::KEY)) {
                    ocl::BaseAnnotationPtr annot = var->getAnnotation(ocl::BaseAnnotation::KEY);
                    for(ocl::BaseAnnotation::AnnotationList::const_iterator I = annot->getAnnotationListBegin(),
                            E = annot->getAnnotationListEnd(); I != E; ++I) {
                        if(ocl::AddressSpaceAnnotationPtr asa = std::dynamic_pointer_cast<ocl::AddressSpaceAnnotation>(*I)) {
                            switch(asa->getAddressSpace()) {
                            case ocl::AddressSpaceAnnotation::GLOBAL: {
                                globalVars.push_back(var);
                                break;
                            }
                            case ocl::AddressSpaceAnnotation::CONSTANT: {
                                constantVars.push_back(var);
                                break;
                            }
                            case ocl::AddressSpaceAnnotation::LOCAL: {
                                localVars.push_back(var);
                                break;
                            }
                            case ocl::AddressSpaceAnnotation::PRIVATE: {
                                privateVars.push_back(var);
                                //TODO think about what to do with local variable arguments
                                break;
                            }
                            default:
                                assert(false && "Unexpected OpenCL address space attribute for local variable");
                            }
                        }
                    }
                }
            }

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


            //TODO handle subnodes.
            //Maybe prettier in another mapper
            const core::StatementPtr& body = func->getBody();

            //TODO add three parallel statements for the localRange
            if(core::StatementPtr newBody = dynamic_pointer_cast<const core::Statement>(body->substitute(*builder.getNodeManager(), kernelMapper))){
                core::JobExprPtr localZjob = builder.jobExpr(newBody);
                std::vector<core::ExpressionPtr> expr;
                //TODO remove dirty workaround
//                expr.push_back(localZjob);

                core::ExpressionPtr p = builder.callExpr(dynamic_pointer_cast<const core::FunctionType>
                    (fakePar->getType())->getReturnType(), fakePar, expr);

                core::StatementPtr parBody = builder.compoundStmt(p);

                core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, params, parBody);

                return newFunc;
            }

            // get address spaces of variables in body
            kernelMapper.getMemspaces(globalVars, constantVars, localVars, privateVars);

/*            const core::Node::ChildList& children = body->getChildList();

            std::for_each(children.begin(), children.end(),
                    [] (const core::NodePtr& curr) {
                //look for ocl buildin functions and translate them to IR statements

                }
            );*/

  //          newFuncType = func->substitute(*builder.getNodeManager(), kernelMapper);
  //          core::FunctionTypePtr funcTy = builder.functionType( builder.tupleType(elemTy), retTy);


            //add three parallel statements for the localRange
//            core::JobExprPtr localZjob = builder.jobExpr(body);
/*            core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, params, localZjob);

            return newFunc;*/
        }

        return element->substitute(*builder.getNodeManager(), *this);
    }

    //TODO remove, for debugging only
    void showKernelMapper() {

        std::cout << "Constant vars: " << constantVars.size() << std::endl;
        std::cout << "Global vars: " << globalVars.size() << std::endl;
        std::cout << "Local vars: " << localVars.size() << std::endl;
        std::cout << "Private vars: " << privateVars.size() << std::endl;
    }
};

}

core::ProgramPtr Compiler::lookForOclAnnotations() {
//    core::RecursiveASTVisitor<OclVisitor> visitor(oclAnnotationExpander);
//    core::visitAll(mProgram, oclAnnotationExpander);

    OclMapper oclAnnotationExpander(builder);
//    visitor.visit(mProgram);
    const core::NodePtr progNode = oclAnnotationExpander.mapElement(0, mProgram);
    oclAnnotationExpander.showKernelMapper();

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
