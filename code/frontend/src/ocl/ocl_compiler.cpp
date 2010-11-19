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
#include "insieme/c_info/naming.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/ocl/ocl_annotations.h"
#include "insieme/frontend/utils/types_lenght.h"

// old version for compatibility reasons
#include "insieme/core/lang_basic.h"

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


void KernelData::appendCaptures(core::Lambda::CaptureList& captureList, OCL_SCOPE scope, core::CaptureInitExpr::Values inits) {

    if(globalRangeUsed)
        CAPTURE(captureList, globalRange, inits);

    if(numGroupsUsed)
        CAPTURE(captureList, numGroups, inits);

    if(localRangeUsed || scope == OCL_GLOBAL)
        CAPTURE(captureList, localRange, inits);

    if(groupTgUsed) {
        CAPTURE(captureList, groupTg, inits);
    }

    if(localTgUsed && scope == OCL_LOCAL)
        CAPTURE(captureList, localTg, inits);

}

void KernelData::appendShared(core::JobExpr::LocalDecls& sharedList, OCL_SCOPE scope) {

    if(globalRangeUsed)
         SHARE(sharedList, globalRange);

     if(numGroupsUsed)
         SHARE(sharedList, numGroups);

     if(localRangeUsed || scope == OCL_GLOBAL)
         SHARE(sharedList, localRange);

     if(groupTgUsed) {
         SHARE(sharedList, groupTg);
     }

     if(localTgUsed && scope == OCL_LOCAL)
         SHARE(sharedList, localTg);

}

core::CallExprPtr KernelData::accessGlobalRange(core::ExpressionPtr idx) {
    globalRangeUsed = true;
    return vecAccess(globalRange, idx);
}


core::CallExprPtr KernelData::accessNumGroups(core::ExpressionPtr idx) {
    numGroupsUsed = true;
    return vecAccess(numGroups, idx);
}


core::CallExprPtr KernelData::accessLocalRange(core::ExpressionPtr idx) {
    localRangeUsed = true;
    return vecAccess(localRange, idx);
}

core::CallExprPtr KernelData::callBarrier(core::ExpressionPtr memFence) {

    groupTgUsed = true;

    return builder.callExpr(/*builder.getNodeManager().basic.getUnit()*/core::lang::TYPE_UNIT_PTR, core::lang::OP_BARRIER_PTR, groupTg);
}


class KernelMapper : public core::NodeMapping {
    const core::ASTBuilder& builder;

    // Vectors to store local variables
    //TODO remove these two, should be empty anyway?
    std::vector<core::VariablePtr> constantArgs;
    std::vector<core::VariablePtr> globalVars;

    std::vector<core::DeclarationStmtPtr> localVars;
    std::vector<core::VariablePtr> privateVars;

    KernelData& kd;

private:

    void append(std::vector<core::VariablePtr>& sink, std::vector<core::VariablePtr>& source) {
        for(std::vector<core::VariablePtr>::iterator I = source.begin(), E = source.end(); I != E; I++) {
            sink.push_back(*I);
        }
    }

    void append(std::vector<core::VariablePtr>& sink, std::vector<core::DeclarationStmtPtr>& source) {
        for(std::vector<core::DeclarationStmtPtr>::iterator I = source.begin(), E = source.end(); I != E; I++) {
            sink.push_back((*I)->getVariable());
        }
    }


public:


    KernelMapper(core::ASTBuilder& astBuilder, KernelData& data)
    : builder(astBuilder), kd(data) { };

    const core::NodePtr mapElement(unsigned, const core::NodePtr& element) {
//std::cout << "\t * found " << element->toString() << std::endl;

        if(core::dynamic_pointer_cast<const core::LambdaExpr>(element)){
            std::cout << "the function \n";
            return element;
        }


        if(core::CallExprPtr call = core::dynamic_pointer_cast<const core::CallExpr>(element)){
//            std::cout << "found a call\n";
            core::ExpressionPtr fun = call->getFunctionExpr();
            if(core::LiteralPtr literal = core::dynamic_pointer_cast<const core::Literal>(fun)) {
                const vector<core::ExpressionPtr>& args = call->getArguments();
                // reading parallel loop boundaries
                if(literal->getValue() == "get_global_size") {
                    assert(args.size() == 1 && "Function get_global_id must have exactly 1 argument");

                    return kd.accessGlobalRange(args.at(0));
                }
                if(literal->getValue() == "get_num_groups") {
                    assert(args.size() == 1 && "Function get_num_groups must have exactly 1 argument");

                    return kd.accessNumGroups(args.at(0));
                }
                if(literal->getValue() == "get_local_size") {
                    assert(args.size() == 1 && "Function get_local_id must have exactly 1 argument");

                    return kd.accessLocalRange(args.at(0));
                }

                // syncronization
                if(literal->getValue() == "ocl_barrier") {
                    assert(args.size() == 1 && "Function barrier must have exactly 1 argument");

                    return kd.callBarrier(args.at(0));
                }
            }
        /*

        std::cout << "FUNCTION: " << fun << std::endl;
            const core::Node::ChildList& elems = call->getChildList();
            for(size_t i = 0; i < elems.size(); ++i) {
                std::cout << "child: " << elems.at(i) << std::endl;
            }
*/
            return element->substitute(builder.getNodeManager(), *this);
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
                            localVars.push_back(decl);
                            //TODO remove declaration from source code
                            return core::lang::STMT_NO_OP_PTR;//builder.getNodeManager().basic.getNoOp();
                            break;
                        }
                        case ocl::AddressSpaceAnnotation::PRIVATE: {
                            privateVars.push_back(decl->getVariable());
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
        if(core::CompoundStmtPtr body = dynamic_pointer_cast<const core::CompoundStmt>(element->substitute(builder.getNodeManager(), *this))){
            std::cout << "the body\n";
/* do this recursively
            const core::Node::ChildList& children = body->getChildList();

            //&builder should be captured, but is member variable
            std::for_each(children.begin(), children.end(),
                    [] (const core::NodePtr& curr) {
                //look for ocl buildin functions and translate them to IR statements

                }
            );*/

                return body;
//                core::LambdaExpr localZparallel = builder.l
     //           core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, params,localZjob);
            }
 //           else
   //             assert(body && "KenrnelMapper corrupted function body.");
//        }

        return element;
    }


    // gets vectors of variables and appends the variables found in the function body to them
    void getMemspaces(std::vector<core::VariablePtr>& constantV, std::vector<core::VariablePtr>& globalV,
            std::vector<core::VariablePtr>& localV, std::vector<core::VariablePtr>& privateV){
        append(globalV, globalVars);
        append(constantV, constantArgs);
        append(localV, localVars);
        append(privateV, privateVars);
    }

    // returns vector containing the private variables
    std::vector<core::VariablePtr>& getPrivateVars() {
        return privateVars;
    }

    // returns vector containing the local variable declarations
    std::vector<core::DeclarationStmtPtr>& getLocalDeclarations() {
        return localVars;
    }

    void resetMemspaces() {
        globalVars.clear();
        constantArgs.clear();
        localVars.clear();
        privateVars.clear();
    }
};
/*
class VariableMapping : public std::vector<std::pair<core::VariablePtr, core::VariablePtr >> {
public:
    // add variables to the mapping
    void add(std::vector<core::VariablePtr>& Vars) {
        for_each(Vars.begin(), Vars.end(), [&] (core::VariablePtr v) {
            this->push_back(std::make_pair(v,v));
        });
    }
    void add(std::vector<core::DeclarationStmtPtr>& Vars) {
        for_each(Vars.begin(), Vars.end(), [&] (core::DeclarationStmtPtr v) {
            this->push_back(std::make_pair(v->getVariable(),v->getVariable()));
        });
    }

    // look for the mapping of a specific root
    const core::VariablePtr& map(const core::VariablePtr& root) {
        for(auto I = this->begin(), E = this->end(); I != E; ++I) {
           if((*I).first == root) {
               return (*I).second;
           }
        }
        return root;
    }

    // Adjust mapping
    bool remap(const core::VariablePtr& oldTarget, const core::VariablePtr& newTarget) {
        for(auto I = this->begin(), E = this->end(); I != E; ++I) {
           if((*I).second == oldTarget) {
               std::cout << "if " << newTarget->getId() << std::endl;
               (*I).second = newTarget;
               return true;
           }
        }
        return false;
    }

};

*/

class OclMapper : public core::NodeMapping {
    KernelMapper kernelMapper;
    const core::ASTBuilder& builder;
//    const core::Substitution::Mapping& mapping;

    // vectors to store Arguments
    std::vector<core::VariablePtr> constantArgs;
    std::vector<core::VariablePtr> globalArgs;
    std::vector<core::VariablePtr> localArgs;
    std::vector<core::VariablePtr> privateArgs;

    //TODO replace with kernelData

    KernelData kd;

private:
    template <typename T>
    void appendToVector(std::vector<T>& outVec, std::vector<T>& inVec) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            outVec.push_back(*I);
        }
    }

    // function to calculate the product of all elements in a vector
    core::ExpressionPtr vecProduct(core::VariablePtr vec, size_t n) {
        assert(vec->getType()->getNodeType() == core::NodeType::NT_VectorType && "function vecProduct is only allowed for vector variables\n");
        --n;
        if(n == 0) {
            return SUBSCRIPT(vec,0,builder);
        }


        return builder.callExpr(builder.getNodeManager().basic.getUInt4(), core::lang::OP_UINT_MUL_PTR,
            toVector<core::ExpressionPtr>( vecProduct(vec, n), SUBSCRIPT(vec, n, builder) ));
    }



    // creates new declaration for all variables in the input vector and stores them in the output vector
    // The elements of the input vector are used as initialization values for the new variables
    // The last parameter contains a mapping of each variable to its original one (before chatching). This
    // will be automatically updated

    void createDeclarations(core::JobExpr::LocalDecls& outVec, std::vector<core::VariablePtr>& inVec) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            SHARE(outVec, *I);
        }
    }

    void createDeclarations(core::JobExpr::LocalDecls& outVec, std::vector<core::DeclarationStmtPtr>& inVec) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            const core::VariablePtr& initVal = builder.variable((*I)->getVariable()->getType());

            outVec.push_back(builder.declarationStmt((*I)->getVariable(), initVal));
            // update inVec with new variables, but the old initialization values
            (*I) = builder.declarationStmt(initVal, (*I)->getInitialization());
        }
    }

    // creates new variables for all variables in the input vector and stores them in the output vector
    // The elements of the input vector are stored as initialization values in map inits for the new variables
    void createCaptureList(core::Lambda::CaptureList& outVec, std::vector<core::VariablePtr>& inVec, core::CaptureInitExpr::Values inits) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            outVec.push_back(*I);
            *I = builder.variable((*I)->getType());

            inits[outVec.back()] = *I;
        }
    }

    void createCaptureList(core::Lambda::CaptureList& outVec, std::vector<core::DeclarationStmtPtr>& inVec, core::CaptureInitExpr::Values inits) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            outVec.push_back((*I)->getVariable());
            *I = builder.declarationStmt(((*I)->getVariable())->getType(), (*I)->getInitialization());

            inits[outVec.back()] = (*I)->getVariable();
        }
    }

    // appends the variables of declared in inVec to outVec and stores the initializations int inits
    void appendToCaptureList(core::Lambda::CaptureList& outVec, std::vector<core::DeclarationStmtPtr>& inVec, core::CaptureInitExpr::Values inits) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            outVec.push_back((*I)->getVariable());

            inits[outVec.back()] = (*I)->getVariable();
        }
    }



/*
    // Creates the initial mapping of the vaiables: each variable (except for in-body private variables) will be mapped on itself
    void getInitialVariables(VariableMapping& variableMapping) {
        // constant arguments
        variableMapping.add(constantArgs);

        // global arguments
        variableMapping.add(globalArgs);

        // local arguments
        variableMapping.add(localArgs);

        // local in-body variables
        variableMapping.add(kernelMapper.getLocalDeclarations());

        // private arguments
        variableMapping.add(privateArgs);

        // private in-body variables
        // not needed, is it?

    }
*/
public:

    OclMapper(core::ASTBuilder& astBuilder)
        : kernelMapper(astBuilder, kd), builder(astBuilder),
          kd(astBuilder){ };

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

            bool isKernelFunction = false;
            bool workGroupSizeDefined = false;
            auto cName = element.getAnnotation(c_info::CNameAnnotation::KEY);
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
                return element->substitute(builder.getNodeManager(), *this);
            }


            core::Lambda::ParamList params = func->getParameterList();

            // store memory spaces of arguments
            for(core::Lambda::ParamList::iterator pi = params.begin(), pe = params.end(); pi != pe; pi++) {
                core::VariablePtr var = *pi;
                if(var->hasAnnotation(ocl::BaseAnnotation::KEY)) {
                    ocl::BaseAnnotationPtr annot = var->getAnnotation(ocl::BaseAnnotation::KEY);
                    for(ocl::BaseAnnotation::AnnotationList::const_iterator I = annot->getAnnotationListBegin(),
                            E = annot->getAnnotationListEnd(); I != E; ++I) {
                        if(ocl::AddressSpaceAnnotationPtr asa = std::dynamic_pointer_cast<ocl::AddressSpaceAnnotation>(*I)) {
                            switch(asa->getAddressSpace()) {
                            case ocl::AddressSpaceAnnotation::GLOBAL: {
                                globalArgs.push_back(var);
                                break;
                            }
                            case ocl::AddressSpaceAnnotation::CONSTANT: {
                                constantArgs.push_back(var);
                                break;
                            }
                            case ocl::AddressSpaceAnnotation::LOCAL: {
                                localArgs.push_back(var);
                                //TODO think about what to do with local variable arguments
                                break;
                            }
                            case ocl::AddressSpaceAnnotation::PRIVATE: {
                                privateArgs.push_back(var);
                                break;
                            }
                            default:
                                assert(false && "Unexpected OpenCL address space attribute for kernel function argument");
                            }
                        }
                    }
                }
                else {
                    // arguments without address space modifiers are private per default
                    privateArgs.push_back(var);
                }
            }

            // add vector<uint<4>,3> globalRange and localRange to parameters
/*            core::IntTypeParam vecSize = core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3));
            core::VariablePtr globalRange = builder.variable(builder.vectorType(builder.uintType( INT_LENGTH ), vecSize));
            params.push_back(globalRange);
            core::VariablePtr localRange = builder.variable(builder.vectorType(builder.uintType( INT_LENGTH ), vecSize));
            params.push_back(localRange);*/
//            params.push_back(globalRange);
//            params.push_back(localRange);

            // update the type of the function
            core::FunctionTypePtr newFuncType;
            if(core::FunctionTypePtr funcType = core::dynamic_pointer_cast<const core::FunctionType>(func->getType())){
                core::TypePtr retTy = funcType->getReturnType();

                //check return type
                assert(retTy->getName() == "unit" && "Return type of kernel functions must be void.");

                core::TypeList args = funcType->getArgumentTypes();
                args.push_back(kd.globalRange->getType());
                args.push_back(kd.localRange->getType());

                newFuncType = builder.functionType(args, retTy);
            } else {
                assert(funcType && "Function has unexpected type");
            }

            //TODO handle subnodes.
            //Maybe prettier in another mapper
            const core::StatementPtr& oldBody = func->getBody();

            if(core::StatementPtr newBody = dynamic_pointer_cast<const core::Statement>(oldBody->substitute(builder.getNodeManager(), kernelMapper))){
                // parallel function's type, equal for all
                core::TypeList parArgs;
                parArgs.push_back(builder.getNodeManager().basic.getUInt4());
                parArgs.push_back(builder.getNodeManager().basic.getUInt4());
                parArgs.push_back(/*builder.getNodeManager().basic.getJob()*/core::lang::TYPE_JOB_PTR);

                // type of functions inside jobs
//                core::FunctionTypePtr funType = builder.functionType(builder.tupleType(), builder.unitType());

                core::Lambda::ParamList funParams;


               core::FunctionTypePtr parFuncType= builder.functionType(parArgs, builder.getNodeManager().basic.getUInt4());

// Top down generation of constructs

// Bottom up generation/composition of constructs

                core::JobExpr::GuardedStmts noGuardedStatementsNeeded;
//                , noGuardedStatementsNeeded, kernelMapper.getLocalDeclarations()
//TODO add pfor

                // capture all arguments
                core::Lambda::CaptureList localFunCaptures;
                core::CaptureInitExpr::Values localFunInits;

                createCaptureList(localFunCaptures, constantArgs, localFunInits);
                createCaptureList(localFunCaptures, globalArgs, localFunInits);
                createCaptureList(localFunCaptures, localArgs, localFunInits);
                createCaptureList(localFunCaptures, privateArgs, localFunInits);
                // in-body local variables
                std::vector<core::DeclarationStmtPtr> localVars = kernelMapper.getLocalDeclarations();
                createCaptureList(localFunCaptures, localVars, localFunInits);
                // catch loop boundaries
                kd.appendCaptures(localFunCaptures, OCL_LOCAL, localFunInits);

                //TODO store return Values of parallel
                core::LambdaExprPtr localParFct = builder.lambdaExpr(core::lang::TYPE_NO_ARGS_OP_PTR, localFunCaptures, funParams, newBody);

                // initialize captured variables
                core::CaptureInitExprPtr localCaptureInits = builder.captureInitExpr(localParFct, localFunInits);
                //TODO use globalCaptureInits

                // catch all arguments which are shared in local range
                core::JobExpr::LocalDecls localJobShared;

                createDeclarations(localJobShared, constantArgs);
                createDeclarations(localJobShared, globalArgs);
                createDeclarations(localJobShared, localArgs);
                // in-body local variables
                appendToVector(localJobShared, localVars);
                // catch loop boundaries
                kd.appendShared(localJobShared, OCL_LOCAL);
                // TODO catch global variables

                core::JobExprPtr localJob = builder.jobExpr(localParFct, noGuardedStatementsNeeded, localJobShared);

                std::vector<core::ExpressionPtr> expr;
                //construct vector of arguments for local parallel
                expr.push_back(builder.literal("1", builder.getNodeManager().basic.getUInt4()));

                // calculate localRange[0] * localRange[1] * localRange[2] to use as maximum number of threads
                core::ExpressionPtr localRangeProduct = vecProduct(kd.localRange, 3);

                expr.push_back(localRangeProduct);

                expr.push_back(localJob);

                core::CallExprPtr localPar = builder.callExpr(/*builder.getNodeManager().basic.getThreadGroup()*/core::lang::TYPE_THREAD_GROUP_PTR, core::lang::OP_PARALLEL_PTR, expr);

                // capture all arguments
                core::Lambda::CaptureList globalFunCaptures;
                core::CaptureInitExpr::Values globalFunInits;

                createCaptureList(globalFunCaptures, constantArgs, globalFunInits);
                createCaptureList(globalFunCaptures, globalArgs, globalFunInits);
                createCaptureList(globalFunCaptures, localArgs, globalFunInits);
                createCaptureList(globalFunCaptures, privateArgs, globalFunInits);
                // catch loop boundaries
                kd.appendCaptures(globalFunCaptures, OCL_GLOBAL, globalFunInits);
                // TODO catch global variables

                //core::DeclarationStmtPtr localThreadGroup = builder.declarationStmt(kd.localTg, localPar); inlined, see next line, created only if needed

                core::StatementPtr tmp;
                if(kd.localTgUsed)
                    tmp = builder.declarationStmt(kd.localTg, localPar);
                else
                    tmp = localPar;

                core::LambdaExprPtr globalParFct = builder.lambdaExpr(core::lang::TYPE_NO_ARGS_OP_PTR, globalFunCaptures, funParams, tmp);

                // catch all arguments which are shared in global range
                core::JobExpr::LocalDecls globalJobShared;

                createDeclarations(globalJobShared, constantArgs);
                createDeclarations(globalJobShared, globalArgs);
                // catch loop boundaries
                kd.appendShared(globalJobShared, OCL_GLOBAL);
                // TODO catch global variables

                core::JobExprPtr globalJob = builder.jobExpr(globalParFct, noGuardedStatementsNeeded, globalJobShared);

                // initialize captured variables
                core::CaptureInitExprPtr globalCaptureInits = builder.captureInitExpr(globalParFct, globalFunInits);
                //TODO use globalCaptureInits

                expr.clear();
                //construct vector of arguments for local parallel
                expr.push_back(builder.literal("1", builder.getNodeManager().basic.getUInt4()));

                // calculate groupRange[0] * groupRange[1] * groupRange[2] to use as maximum number of threads
                core::ExpressionPtr globalRangeProduct = vecProduct(kd.numGroups, 3);
                expr.push_back(globalRangeProduct);

                expr.push_back(globalJob);

                core::CallExprPtr globalPar = builder.callExpr(/*builder.getNodeManager().basic.getThreadGroup()*/core::lang::TYPE_THREAD_GROUP_PTR, core::lang::OP_PARALLEL_PTR, expr);

                // construct updated param list
                core::Lambda::ParamList newParams;
                appendToVector(newParams, constantArgs);
                appendToVector(newParams, globalArgs);
                appendToVector(newParams, localArgs);
                appendToVector(newParams, privateArgs);
                newParams.push_back(kd.globalRange); // add global range to parameters
                newParams.push_back(kd.localRange); // add local range to parameters

                std::vector<core::StatementPtr> newBodyStmts;

                //calculate groupRange = globalRange/localRange
                std::vector<core::ExpressionPtr> groupRdeclInit ;
                for(size_t i = 0; i < 3; ++i) {
                    groupRdeclInit.push_back(
                        builder.callExpr(core::lang::TYPE_UINT_4_PTR, core::lang::OP_UINT_DIV_PTR, toVector<core::ExpressionPtr>(
                            SUBSCRIPT(kd.globalRange, i, builder),  SUBSCRIPT(kd.localRange, i, builder) )));
                }

                //declare group range
                core::DeclarationStmtPtr groupRdecl = builder.declarationStmt(kd.numGroups, builder.vectorExpr(groupRdeclInit));
                newBodyStmts.push_back(groupRdecl);

                //core::DeclarationStmtPtr groupThreadGroup = builder.declarationStmt(kd.groupTg, globalPar); inlined, see next line, created only if needed

                if(kd.groupTgUsed)
                    tmp = builder.declarationStmt(kd.groupTg, globalPar);
                else
                    tmp = globalPar;


                newBodyStmts.push_back(tmp);

                core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, newParams, builder.compoundStmt(newBodyStmts));

                // get address spaces of variables in body
                kernelMapper.getMemspaces(globalArgs, constantArgs, localArgs, privateArgs);

                // put opencl annotation to the new function for eventual future use
                newFunc.addAnnotation(funcAnnotation);
                // put cname annotation to the new function
                newFunc.addAnnotation(cName);

                return newFunc;
            }

/*            const core::Node::ChildList& children = body->getChildList();

            std::for_each(children.begin(), children.end(),
                    [] (const core::NodePtr& curr) {
                //look for ocl build-in functions and translate them to IR statements

                }
            );*/

  //          newFuncType = func->substitute(*builder.getNodeManager(), kernelMapper);
  //          core::FunctionTypePtr funcTy = builder.functionType( builder.tupleType(elemTy), retTy);


            //add three parallel statements for the localRange
//            core::JobExprPtr localZjob = builder.jobExpr(body);
/*            core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, params, localZjob);

            return newFunc;*/
        }

        return element->substitute(builder.getNodeManager(), *this);
    }

    //TODO remove, for debugging only
    void showKernelMapper() {

        std::cout << "Constant vars: " << constantArgs.size() << std::endl;
        std::cout << "Global vars: " << globalArgs.size() << std::endl;
        std::cout << "Local vars: " << localArgs.size() << std::endl;
        std::cout << "Private vars: " << privateArgs.size() << std::endl;
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
