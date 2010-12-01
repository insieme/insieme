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

#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/c_info/naming.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/ocl/ocl_annotations.h"


namespace insieme {
namespace frontend {
namespace ocl {

namespace {

void KernelData::appendCaptures(core::ASTBuilder::CaptureInits& captureList, OCL_SCOPE scope, core::TypeList cTypes){

    if(globalRangeUsed)
        CAPTURE(captureList, globalRange, cTypes);

    if(numGroupsUsed || scope == OCL_GLOBAL_JOB)
        CAPTURE(captureList, numGroups, cTypes);

    if(localRangeUsed || scope != OCL_LOCAL_PAR)
        CAPTURE(captureList, localRange, cTypes);
}

void KernelData::appendShared(core::JobExpr::LocalDecls& sharedList, OCL_SCOPE scope) {

    if(globalRangeUsed)
         SHARE(sharedList, globalRange);

     if(numGroupsUsed || scope == OCL_GLOBAL_JOB)
         SHARE(sharedList, numGroups);

     if(localRangeUsed || scope != OCL_LOCAL_PAR)
         SHARE(sharedList, localRange);
}

core::CallExprPtr KernelData::accessRange(OCL_PAR_LEVEL level, core::ExpressionPtr idx) {
    switch(level) {
    case OPL_GLOBAL :
        globalRangeUsed = true;
        return vecAccess(globalRange, idx);
    case OPL_GROUP :
        numGroupsUsed = true;
        return vecAccess(numGroups, idx);
    //case OPL_LOCAL :
    default:
        localRangeUsed = true;
        return vecAccess(localRange, idx);
    }
}

core::CallExprPtr KernelData::accessId(OCL_PAR_LEVEL level, core::ExpressionPtr idx){
    switch(level) {
    case OPL_GLOBAL :
        localRangeUsed = true;
        return builder.callExpr(BASIC.getUnsignedIntAdd(), builder.callExpr(BASIC.getGetThreadId1D(), builder.uintLit(0), idx),
                builder.callExpr(BASIC.getUnsignedIntMul(), vecAccess(localRange, idx), builder.callExpr(BASIC.getGetThreadId1D(), builder.uintLit(1), idx)));
    case OPL_GROUP :
        return builder.callExpr(BASIC.getGetThreadId1D(), builder.uintLit(1), idx);
    //case OPL_LOCAL :
    default:
        return builder.callExpr(BASIC.getGetThreadId1D(), builder.uintLit(0), idx);
    }
}


core::CallExprPtr KernelData::callBarrier(core::ExpressionPtr memFence) {
    //get rid of casts
    core::NodePtr arg = memFence;
    while(arg->getNodeType() == core::NT_CastExpr) {
        arg = arg->getChildList().at(1);
    }

    if(core::LiteralPtr lit = core::dynamic_pointer_cast<const core::Literal>(arg)){
      if(lit->getValue() == "0u") {
            //if lit is 0 CLK_LOCAL_MEM_FENCE,
            return builder.callExpr(builder.getNodeManager().basic.getBarrier(), builder.getThreadGroup(builder.uintLit(0)));
        }
        if(lit->getValue() == "1u"){
            //if lit is 1 CLK_GLOBAL_MEM_FENCE
            return builder.callExpr(builder.getNodeManager().basic.getBarrier(), builder.getThreadGroup(builder.uintLit(1)));
        }
    }

    // TODO show warning
    assert(false && "OpenCL barrier has unexpected argument. Has to be 0u or 1u");
    return builder.callExpr(builder.getNodeManager().basic.getBarrier(), builder.getThreadGroup(builder.uintLit(0)));
}


class KernelMapper : public core::transform::CachedNodeMapping {
    const core::ASTBuilder& builder;

    // Vectors to store local variables

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

    const core::NodePtr resolveElement(const core::NodePtr& element) {
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
                    assert(args.size() == 1 && "Function get_global_size must have exactly 1 argument");

                    return kd.accessRange(OPL_GLOBAL, args.at(0));
                }
                if(literal->getValue() == "get_num_groups") {
                    assert(args.size() == 1 && "Function get_num_groups must have exactly 1 argument");

                    return kd.accessRange(OPL_GROUP, args.at(0));
                }
                if(literal->getValue() == "get_local_size") {
                    assert(args.size() == 1 && "Function get_local_size must have exactly 1 argument");

                    return kd.accessRange(OPL_LOCAL, args.at(0));
                }

                // thread identification
                if(literal->getValue() == "get_global_id") {
                    assert(args.size() == 1 && "Function get_global_id must have exactly 1 argument");

                    return kd.accessId(OPL_GLOBAL, args.at(0));
                }
                if(literal->getValue() == "get_group_id") {
                    assert(args.size() == 1 && "Function get_group_id must have exactly 1 argument");

                    return kd.accessId(OPL_GROUP, args.at(0));
                }
                if(literal->getValue() == "get_local_id") {
                    assert(args.size() == 1 && "Function get_local_id must have exactly 1 argument");

                    return kd.accessId(OPL_LOCAL, args.at(0));
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
                            return builder.getNodeManager().basic.getNoOp();
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
            return element->substitute(builder.getNodeManager(), *this);
        }
/*
        if(core::VariablePtr var = dynamic_pointer_cast<const core::Variable>(element)) {
            core::Variable v = *var;
            std::cout << "VarID: " << v.getId() << " VarType: " << v.getType() << std::endl;
        }
*//*
        if(core::CompoundStmtPtr body = dynamic_pointer_cast<const core::CompoundStmt>(element->substitute(builder.getNodeManager(), *this))){
            std::cout << "the body\n";
*//* do this recursively
            const core::Node::ChildList& children = body->getChildList();

            //&builder should be captured, but is member variable
            std::for_each(children.begin(), children.end(),
                    [] (const core::NodePtr& curr) {
                //look for ocl buildin functions and translate them to IR statements

                }
            );

                return body;
//                core::LambdaExpr localZparallel = builder.l
     //           core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, params,localZjob);
            }*/
 //           else
   //             assert(body && "KenrnelMapper corrupted function body.");
//        }

        return element->substitute(builder.getNodeManager(), *this);
    }


    // gets vectors of variables and appends the variables found in the function body to them
    void getMemspaces(std::vector<core::VariablePtr>& constantV, std::vector<core::VariablePtr>& globalV,
            std::vector<core::VariablePtr>& localV, std::vector<core::VariablePtr>& privateV){
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
        localVars.clear();
        privateVars.clear();
    }
};

class OclMapper : public core::transform::CachedNodeMapping {
    KernelMapper kernelMapper;
    const core::ASTBuilder& builder;
//    const core::Substitution::Mapping& mapping;

    // vectors to store Arguments
    std::vector<core::VariablePtr> constantArgs;
    std::vector<core::VariablePtr> globalArgs;
    std::vector<core::VariablePtr> localArgs;
    std::vector<core::VariablePtr> privateArgs;

    // struct holding information about the kernel function's body
    KernelData kd;

private:
    template <typename T>
    void appendToVector(std::vector<T>& outVec, std::vector<T>& inVec) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            outVec.push_back(*I);
        }
    }

    template <typename T>
    void appendToVector(std::vector<T>& outVec, std::vector<T*>& inVec) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            outVec.push_back(*(*I));
        }
    }

    // function to calculate the product of all elements in a vector
    core::ExpressionPtr vecProduct(core::VariablePtr vec, size_t n) {
        assert(vec->getType()->getNodeType() == core::NodeType::NT_VectorType && "function vecProduct is only allowed for vector variables\n");

        return builder.callExpr(BASIC.getUInt4(), BASIC.getVectorReduction(), vec, builder.uintLit(1), BASIC.getUnsignedIntMul());

        --n;
        if(n == 0) {
            return SUBSCRIPT(vec,0,builder);
        }


        return builder.callExpr(builder.getNodeManager().basic.getUInt4(), builder.getNodeManager().basic.getUnsignedIntMul(),
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
    void createCaptureList(core::ASTBuilder::CaptureInits& outVec, std::vector<core::VariablePtr>& inVec, core::TypeList& types) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            core::VariablePtr tmp = builder.variable((*I)->getType());

            outVec[(*I)] = tmp;
            types.push_back((*I)->getType());

            *I = tmp;
        }
    }

    void createCaptureList(core::ASTBuilder::CaptureInits& outVec, std::vector<core::DeclarationStmtPtr>& inVec, core::TypeList& types) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            core::DeclarationStmtPtr tmp = builder.declarationStmt((*I)->getVariable()->getType(), (*I)->getInitialization());

            outVec[(*I)->getVariable()] = tmp->getVariable();
            types.push_back((*I)->getVariable()->getType());

            *I = tmp;
        }
    }

    void createCaptureList(core::Lambda::CaptureList& outVec, std::vector<core::DeclarationStmtPtr>& inVec, core::CaptureInitExpr::Values inits,
            core::TypeList types) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            outVec.push_back((*I)->getVariable());
            types.push_back((*I)->getVariable()->getType());

            *I = builder.declarationStmt(((*I)->getVariable())->getType(), (*I)->getInitialization());

            inits.push_back((*I)->getVariable());
        }
    }

    // appends the variables of declared in inVec to outVec and stores the initializations int inits
    void appendToCaptureList(core::Lambda::CaptureList& outVec, std::vector<core::DeclarationStmtPtr>& inVec, core::CaptureInitExpr::Values inits) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            outVec.push_back((*I)->getVariable());

            inits.push_back((*I)->getVariable());
        }
    }


    // TODO make body ref
    core::ExpressionPtr genLocalCie(core::StatementPtr body, core::Lambda::ParamList params, OCL_SCOPE scope) {
        // capture all arguments
        core::ASTBuilder::CaptureInits localFunCaptures;
        core::TypeList localFunCtypes;

        createCaptureList(localFunCaptures, constantArgs, localFunCtypes);
        createCaptureList(localFunCaptures, globalArgs, localFunCtypes);
        createCaptureList(localFunCaptures, localArgs, localFunCtypes);
        createCaptureList(localFunCaptures, privateArgs, localFunCtypes);
        // in-body local variables
        if(scope == OCL_LOCAL_JOB || scope == OCL_LOCAL_PAR)
            createCaptureList(localFunCaptures, kernelMapper.getLocalDeclarations(), localFunCtypes);
        // catch loop boundaries

        kd.appendCaptures(localFunCaptures, scope, localFunCtypes);

        core::FunctionTypePtr lpfType = builder.functionType(localFunCtypes, core::TypeList(), builder.getNodeManager().basic.getUnit());

        return builder.lambdaExpr(lpfType, body, localFunCaptures, params);
    }


    std::vector<core::ExpressionPtr> gen3dPforArgs(core::VariablePtr end, core::ExpressionPtr body) {
        std::vector<core::ExpressionPtr> expr;

        expr.push_back(builder.getThreadGroup());
        // start vector of pfor loop: [0,0,0]
        expr.push_back(INT3DVECINIT("0"));
        // end vector of pfor
        expr.push_back(end)/*)*/;
        // increment vector of pfor loop: [1,1,1]
        expr.push_back(INT3DVECINIT("1"));
        // lambda to be called
        expr.push_back(body);

        return expr;
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

    const core::NodePtr resolveElement(const core::NodePtr& element) {
        // quick check - stop recursion at variables
        if (element->getNodeCategory() == core::NodeCategory::NC_Type) {
            return element;//->substitute(builder.getNodeManager(), *this);
        }

        if(element->getNodeType() == core::NT_LambdaExpr) {
        	// check if we are at a function node
        	const core::LambdaExprPtr& func = static_pointer_cast<const core::LambdaExpr>(element);
//        if(newNode->getNodeType() == core::NodeType::NT_LambdaExpr && false){
//            return builder.lambdaExpr(func->getType(), func->getParams(), builder.compoundStmt());

            bool isKernelFunction = false;
            bool workGroupSizeDefined = false;

            // TODO: annotations on pointers are no longer supported!
            // Code has been changed to read annotations form nodes => if wrong, please fix it
            auto cName = element->getAnnotation(c_info::CNameAnnotation::KEY);
            auto funcAnnotation = element->getAnnotation(ocl::BaseAnnotation::KEY);
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

            std::vector<core::VariablePtr*> orderedArgs;

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
                                orderedArgs.push_back(&globalArgs.back());
                                break;
                            }
                            case ocl::AddressSpaceAnnotation::CONSTANT: {
                                constantArgs.push_back(var);
                                orderedArgs.push_back(&constantArgs.back());
                                break;
                            }
                            case ocl::AddressSpaceAnnotation::LOCAL: {
                                localArgs.push_back(var);
                                orderedArgs.push_back(&localArgs.back());
                                break;
                            }
                            case ocl::AddressSpaceAnnotation::PRIVATE: {
                                privateArgs.push_back(var);
                                orderedArgs.push_back(&privateArgs.back());
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
                    orderedArgs.push_back(&privateArgs.back());
                }
            }

            // add vector<uint<4>,3> globalRange and localRange to parameters
/*            core::IntTypeParam vecSize = core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3));
            core::VariablePtr globalRange = builder.variable(builder.vectorType(builder.uintType( 4 ), vecSize));
            params.push_back(globalRange);
            core::VariablePtr localRange = builder.variable(builder.vectorType(builder.uintType( 4 ), vecSize));
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
            const core::StatementPtr& oldBody = func->getBody();

            if(core::StatementPtr newBody = dynamic_pointer_cast<const core::Statement>(oldBody->substitute(builder.getNodeManager(), kernelMapper))){
                // parallel function's type, equal for all
                core::TypeList parArgs;
                parArgs.push_back(builder.getNodeManager().basic.getUInt4());
                parArgs.push_back(builder.getNodeManager().basic.getUInt4());
                parArgs.push_back(builder.getNodeManager().basic.getJob());

                // type of functions inside jobs
//                core::FunctionTypePtr funType = builder.functionType(builder.tupleType(), builder.unitType());

               core::FunctionTypePtr parFuncType= builder.functionType(parArgs, builder.getNodeManager().basic.getUInt4());

// Top down generation of constructs

               std::vector<core::ExpressionPtr> expr;

                core::JobExpr::GuardedStmts noGuardedStatementsNeeded;

// Bottom up generation/composition of constructs
/*
                // build expression to be used as body of local pfor loop
                core::Lambda::ParamList localIdAsAVector = toVector(kd.localId);
                core::ExpressionPtr localPforFun = genLocalCie(newBody, localIdAsAVector, OCL_LOCAL_PAR);

                core::CallExprPtr localPfor = builder.callExpr(builder.getNodeManager().basic.getPFor(), gen3dPforArgs(kd.localRange, localPforFun));
*/
                // build expression to be used as body of local job
                core::ExpressionPtr localParFct = genLocalCie(/*localPfor*/newBody, core::Lambda::ParamList(), OCL_LOCAL_JOB);

                core::JobExpr::LocalDecls localJobShared;

                createDeclarations(localJobShared, constantArgs);
                createDeclarations(localJobShared, globalArgs);
                createDeclarations(localJobShared, localArgs);
                // in-body local variables
                appendToVector(localJobShared, kernelMapper.getLocalDeclarations());
                // catch loop boundaries
                kd.appendShared(localJobShared, OCL_LOCAL_JOB);
                // TODO catch global variables


                core::JobExprPtr localJob = builder.jobExpr(localParFct, noGuardedStatementsNeeded, localJobShared);

                expr.clear();
                //construct vector of arguments for local parallel
                expr.push_back(builder.literal("1", builder.getNodeManager().basic.getUInt4()));

                // calculate localRange[0] * localRange[1] * localRange[2] to use as maximum number of threads
                core::ExpressionPtr localRangeProduct = vecProduct(kd.localRange, 3);

                expr.push_back(localRangeProduct);

                expr.push_back(localJob);

                core::CallExprPtr localPar = builder.callExpr(builder.getNodeManager().basic.getThreadGroup(),
                        builder.getNodeManager().basic.getParallel(), expr);
/*
                // create global pfor loop body
                core::Lambda::ParamList groupIdAsAVector = toVector(kd.groupId);
                core::ExpressionPtr globalPforFun = genLocalCie(localPar, groupIdAsAVector, OCL_GLOBAL_PAR);

                core::CallExprPtr globalPfor = builder.callExpr(builder.getNodeManager().basic.getPFor(), gen3dPforArgs(kd.numGroups, globalPforFun));
*/
                std::vector<core::StatementPtr> gobalBodyStmts;
                gobalBodyStmts.push_back(/*globalPfor*/localPar);
                expr.clear();
                gobalBodyStmts.push_back(builder.callExpr(BASIC.getMergeAll(), expr));
                core::CompoundStmtPtr globalParBody = builder.compoundStmt(gobalBodyStmts);


                // build expression to be used as body of global job
                core::ExpressionPtr globalParFct = genLocalCie(globalParBody, core::Lambda::ParamList(), OCL_GLOBAL_JOB);

                // catch all arguments which are shared in global range
                core::JobExpr::LocalDecls globalJobShared;

                createDeclarations(globalJobShared, constantArgs);
                createDeclarations(globalJobShared, globalArgs);
                // catch loop boundaries
                kd.appendShared(globalJobShared, OCL_GLOBAL_JOB);
                // TODO catch global variables


                core::JobExprPtr globalJob = builder.jobExpr(globalParFct, noGuardedStatementsNeeded, globalJobShared);

                expr.clear();
                //construct vector of arguments for local parallel
                expr.push_back(builder.literal("1", builder.getNodeManager().basic.getUInt4()));

                // calculate groupRange[0] * groupRange[1] * groupRange[2] to use as maximum number of threads
                core::ExpressionPtr globalRangeProduct = vecProduct(kd.numGroups, 3);
                expr.push_back(globalRangeProduct);

                expr.push_back(globalJob);

                core::CallExprPtr globalPar = builder.callExpr(builder.getNodeManager().basic.getThreadGroup(), builder.getNodeManager().basic.getParallel(), expr);

                // construct updated param list
                core::Lambda::ParamList newParams;
                appendToVector(newParams, orderedArgs);
                newParams.push_back(kd.globalRange); // add global range to parameters
                newParams.push_back(kd.localRange); // add local range to parameters

                std::vector<core::StatementPtr> newBodyStmts;

                //calculate groupRange = globalRange/localRange
                std::vector<core::ExpressionPtr> groupRdeclInit ;
                for(size_t i = 0; i < 3; ++i) {
                    groupRdeclInit.push_back(
                        builder.callExpr(builder.getNodeManager().basic.getUInt4(), builder.getNodeManager().basic.getUnsignedIntDiv(), toVector<core::ExpressionPtr>(
                            SUBSCRIPT(kd.globalRange, i, builder),  SUBSCRIPT(kd.localRange, i, builder) )));
                }

                //declare and initialize start vector and increment vector fo parallel loops
 //               core::DeclarationStmtPtr startVecDecl = builder.declarationStmt(nullVec, )

                //declare group range
                core::DeclarationStmtPtr groupRdecl = builder.declarationStmt(kd.numGroups,
                        builder.callExpr(builder.vectorType(BASIC.getUInt4(), core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3))),
                        BASIC.getVectorPointwise(), kd.globalRange, kd.localRange, BASIC.getUnsignedIntDiv()));
                newBodyStmts.push_back(groupRdecl);

                //core::DeclarationStmtPtr groupThreadGroup = builder.declarationStmt(kd.groupTg, globalPar); inlined, see next line, created only if needed

                newBodyStmts.push_back(globalPar);

                core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, newParams, builder.compoundStmt(newBodyStmts));

                // get address spaces of variables in body
                kernelMapper.getMemspaces(globalArgs, constantArgs, localArgs, privateArgs);

                // put opencl annotation to the new function for eventual future use
                newFunc->addAnnotation(funcAnnotation);
                // put cname annotation to the new function
                newFunc->addAnnotation(cName);

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
