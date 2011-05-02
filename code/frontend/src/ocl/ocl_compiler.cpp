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
#include "insieme/c_info/location.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/ocl/ocl_annotations.h"

namespace ba = boost::algorithm;

namespace insieme {
namespace frontend {
namespace ocl {

namespace {

core::CallExprPtr KernelData::calcIdidx0(core::VariablePtr& threadId, core::VariablePtr& boundaries){
    core::ExpressionPtr one = builder.uintLit(1u);
    core::ExpressionPtr two = builder.uintLit(2u);
    return builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntDiv(),
               builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntDiv(), threadId,
                   vecAccess(boundaries, two)), vecAccess(boundaries, one));
}

core::CallExprPtr KernelData::calcIdidx1(core::VariablePtr& threadId, core::VariablePtr& boundaries){
    core::ExpressionPtr one = builder.uintLit(1u);
    core::ExpressionPtr two = builder.uintLit(2u);
    return builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMod(),
               builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntDiv(), threadId,
                   vecAccess(boundaries, two)), vecAccess(boundaries, one));
}

core::CallExprPtr KernelData::calcIdidx2(core::VariablePtr& threadId, core::VariablePtr& boundaries){
    core::ExpressionPtr two = builder.uintLit(2u);
    return builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMod(),
               threadId, vecAccess(boundaries, two));
}

void KernelData::appendArguments(std::pair<std::vector<core::VariablePtr>, std::vector<core::ExpressionPtr> >& arguments, OCL_SCOPE scope,
        core::TypeList& aTypes){

    if(globalRangeUsed)
        ADD_ARG(arguments, globalRange, aTypes);

    if(numGroupsUsed || scope == OCL_GLOBAL_JOB)
        ADD_ARG(arguments, numGroups, aTypes);

    if(localRangeUsed || scope != OCL_LOCAL_PAR)
        ADD_ARG(arguments, localRange, aTypes);
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

core::CallExprPtr KernelData::accessId(OCL_PAR_LEVEL opl, core::ExpressionPtr idx){
    // construct local variables
    core::VariablePtr idxVar = builder.variable(BASIC.getUInt4());
    core::VariablePtr boundaries = builder.variable(builder.vectorType(BASIC.getUInt4(), builder.concreteIntTypeParam(static_cast<size_t>(3))));
    core::VariablePtr bfgo = builder.variable(builder.vectorType(BASIC.getUInt4(), builder.concreteIntTypeParam(static_cast<size_t>(3))));

    core::LiteralPtr zero = builder.uintLit(0u);
    core::LiteralPtr one = builder.uintLit(1u);

    // TODO make prettier
    core::ExpressionPtr zeroExpr = zero;
    core::ExpressionPtr oneExpr = one;
    core::ExpressionPtr twoExpr = builder.uintLit(2);
    core::VariablePtr localId = builder.variable(BASIC.getUInt4());//builder.callExpr(BASIC.getUInt4(), BASIC.getGetThreadId(), zero);
    core::VariablePtr groupId = builder.variable(BASIC.getUInt4());
    core::DeclarationStmtPtr localDecl = builder.declarationStmt(localId, builder.callExpr(BASIC.getUInt4(), BASIC.getGetThreadId(), zero));
    core::DeclarationStmtPtr groupDecl = builder.declarationStmt(groupId, builder.callExpr(BASIC.getUInt4(), BASIC.getGetThreadId(), one));


//    core::LiteralPtr level = builder.uintLit(opl == OPL_GROUP ? 1u : 0u);
    core::VariablePtr& id = opl == OPL_GROUP ? groupId : localId;

    // construct the cases for each idx
    core::CallExprPtr id0 = opl == OPL_GLOBAL ?
        builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntAdd(), calcIdidx0(localId, boundaries),
            builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMul(), vecAccess(boundaries, zeroExpr), calcIdidx0(groupId, bfgo)))
        :
        calcIdidx0(id, boundaries);

    core::CallExprPtr id1 = opl == OPL_GLOBAL ?
        builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntAdd(), calcIdidx1(localId, boundaries),
            builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMul(), vecAccess(boundaries, oneExpr), calcIdidx1(groupId, bfgo)))
        :
        calcIdidx1(id, boundaries);

    core::CallExprPtr id2 = opl == OPL_GLOBAL ?
        builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntAdd(), calcIdidx2(localId, boundaries),
            builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMul(), vecAccess(boundaries, twoExpr), calcIdidx1(groupId, bfgo)))
        :
        calcIdidx2(id, boundaries);

    core::ReturnStmtPtr ret0 = builder.returnStmt(id0);
    core::ReturnStmtPtr ret1 = builder.returnStmt(id1);
    core::ReturnStmtPtr ret2 = builder.returnStmt(id2);

    // build switch
    vector<core::SwitchStmt::Case> cases;

    cases.push_back(std::make_pair(builder.uintLit(0), ret0));
    cases.push_back(std::make_pair(builder.uintLit(1), ret1));
    cases.push_back(std::make_pair(builder.uintLit(2), ret2));

    core::SwitchStmtPtr swtch = builder.switchStmt(idxVar, cases);

    std::vector<core::StatementPtr> stmts;

    // add ranges and variables as needed
    ArgList args;
    ADD_PARAM(args, idxVar, idx);

    switch(opl) {
    case OPL_GLOBAL :
        localRangeUsed = true;
        numGroupsUsed = true;
        ADD_PARAM(args, bfgo, numGroups);
        ADD_PARAM(args, boundaries, localRange);
        stmts.push_back(localDecl);
        stmts.push_back(groupDecl);
        break;
    case OPL_GROUP :
        numGroupsUsed = true;
        ADD_PARAM(args, boundaries, numGroups);
        stmts.push_back(groupDecl);
        break;
    case OPL_LOCAL :
        localRangeUsed = true;
        ADD_PARAM(args, boundaries, localRange);
        stmts.push_back(localDecl);
    }

    stmts.push_back(swtch);

    // set the argument for the get__id function
    return builder.callExpr(BASIC.getUInt4(), builder.lambdaExpr(BASIC.getUInt4(), builder.compoundStmt(stmts),  args.first), args.second);
}


core::CallExprPtr KernelData::callBarrier(core::ExpressionPtr memFence) {
    //get rid of casts
    core::NodePtr arg = memFence;
    while(arg->getNodeType() == core::NT_CastExpr) {
        arg = arg->getChildList().at(1);
    }

    if(core::LiteralPtr lit = core::dynamic_pointer_cast<const core::Literal>(arg)){
        if(lit->getValue() == "0") {
            //if lit is 0 CLK_LOCAL_MEM_FENCE,
            return builder.callExpr(builder.getNodeManager().basic.getBarrier(), builder.getThreadGroup(builder.uintLit(0)));
        }
        if(lit->getValue() == "1"){
            //if lit is 1 CLK_GLOBAL_MEM_FENCE
            return builder.callExpr(builder.getNodeManager().basic.getBarrier(), builder.getThreadGroup(builder.uintLit(1)));
        }
    }
    // can also be barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE)
    if(core::CallExprPtr call = core::dynamic_pointer_cast<const core::CallExpr>(arg)) {
        if(call->getFunctionExpr() == BASIC.getOperator(call->getType(), core::lang::BasicGenerator::Or)) {
            vector<core::ExpressionPtr> args = call->getArguments();

            // variable to check which flags have been set
            unsigned int b = 0x2;
            for(auto I = args.begin(), E = args.end(); I != E; ++I) {
                if(core::LiteralPtr lit = core::dynamic_pointer_cast<const core::Literal>(*I)){
                    if(lit->getValue() == "0") {
                        //if lit is 0 CLK_LOCAL_MEM_FENCE,
                        b &= 0x1; // set second last bit to 0 and preserve last bit
                    }
                    if(lit->getValue() == "1"){
                        //if lit is 1 CLK_GLOBAL_MEM_FENCE
                        b = 0x1; // set second last bit to 0 and last bit to 1
                    }
                }
            }
            if(b < 2) // if valid argument has been found
                return builder.callExpr(builder.getNodeManager().basic.getBarrier(), builder.getThreadGroup(builder.uintLit(b)));
        }
    }

    // TODO show warning
    assert(false && "OpenCL barrier has unexpected argument. Has to be 0 or 1");
    return builder.callExpr(builder.getNodeManager().basic.getBarrier(), builder.getThreadGroup(builder.uintLit(0)));
}


class KernelMapper : public core::transform::CachedNodeMapping {
    const core::ASTBuilder& builder;

    // Vectors to store local variables

    std::vector<core::DeclarationStmtPtr> globalVars;
    std::vector<core::DeclarationStmtPtr> localVars;
    std::vector<core::VariablePtr> privateVars;

    KernelData& kd;

private:
    core::ExpressionPtr tryDeref(const core::ExpressionPtr& expr) const {
        // core::ExpressionPtr retExpr = expr;
        if(core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
            return builder.callExpr( refTy->getElementType(), BASIC.getRefDeref(), expr );
        }
        return expr;
    }


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

    core::CallExprPtr resolveNative(const string& name, size_t preambleLength, const core::TypePtr& type,
            const core::ExpressionPtr accuracyFct,const vector<core::ExpressionPtr>& args) {
        assert((args.size() == 1 || args.size() == 2) && "Only native OpenCL functions with one or two arguments are supported");

        core::LiteralPtr literal;
        core::FunctionTypePtr fType = dynamic_pointer_cast<const core::FunctionType>(type); // default (=scalar) case
        assert(fType && "Native OpenCL function has invalid function type");

        bool isVector = false; //flag to inidicate if we are processing a vector function
        core::TypePtr resType = fType->getReturnType();
        core::TypePtr elemType = resType; // equal to the result type for scalars, the element type of it in case of vectors
        core::ExpressionPtr function;

        // check if it is a vector operation
        if(core::VectorTypePtr vecTy = dynamic_pointer_cast<const core::VectorType>(resType)) {
            isVector = true;
            elemType = vecTy->getElementType();
            // build a literal using scalars instead of vectors
            core::TypeList scalarArgs;
            for(size_t i = 0; i < args.size(); ++i) {
                // type of the arguments of native functions is always equal to the return type
                scalarArgs.push_back(elemType);
            }
            fType = builder.functionType(scalarArgs, elemType);
        }
        if(name  == "native_divide")
            literal = BASIC.getRealDiv();
        else if(name == "mul24") // special threatement bc we don't know the type at fct call level
            literal = BASIC.isUnsignedInt(elemType) ? BASIC.getUnsignedIntMul() : BASIC.getSignedIntMul();
        else
            literal = builder.literal(name.substr(preambleLength,name.size()), fType);

        if(isVector) {
            // build a pointwise operation in case of a vector
            function = args.size() == 1 ?
                    builder.callExpr(type, BASIC.getVectorPointwiseUnary(), literal) :
                    builder.callExpr(type, BASIC.getVectorPointwise(), literal);
        }
        else {
            function = literal;
        }

        core::CallExprPtr nativeFct = args.size() == 1 ?
                builder.callExpr(type, BASIC.getAccuracyFastUnary(), function) :
                builder.callExpr(type, BASIC.getAccuracyFastBinary(), function);

        return builder.callExpr(resType, nativeFct, args);
    }


    core::CastExprPtr resolveConvert(const core::CallExprPtr& castOp, const string& name, const core::TypePtr& type, const vector<core::ExpressionPtr>& args) {
        assert((args.size() == 1) && "Only cast OpenCL functions with one arguments are supported");

        if(core::FunctionTypePtr ftype = dynamic_pointer_cast<const core::FunctionType>(type)) {
            return builder.castExpr(ftype->getReturnType(), args.at(0));
        }

        assert(false && "Type of OpenCL convert function is not a function Type");

        return NULL;
    }

    // extract the argument of a call to refVar function
    core::ExpressionPtr removeRefVar(core::DeclarationStmtPtr decl) {
        core::ExpressionPtr oldInit = decl->getInitialization();
        // write the variable with it's initialization to the place the declaration was
        if(core::CallExprPtr initCall = core::dynamic_pointer_cast<const core::CallExpr>(oldInit)) {
            // check if initCall calles the var() operation
            core::ExpressionPtr argument = core::dynamic_pointer_cast<const core::Expression>(initCall->getArgument(0));
            if( initCall == builder.refVar(argument) ) {
                // set the argument of the var() operation as the new initialization statement
                oldInit = argument;
            }
        }
        return builder.callExpr(BASIC.getRefAssign(), decl->getVariable(), oldInit);
    }

public:


    KernelMapper(const core::ASTBuilder& astBuilder, KernelData& data)
    : builder(astBuilder), kd(data) { };

    const core::NodePtr resolveElement(const core::NodePtr& element) {

    // stopp recursion at typpe level
    if (element->getNodeCategory() == core::NodeCategory::NC_Type) {
        return element;//->substitute(builder.getNodeManager(), *this);
    }

        if(core::CallExprPtr call = core::dynamic_pointer_cast<const core::CallExpr>(element)){
//            std::cout << "found a call " << *call << std::endl;

            call = core::static_pointer_cast<const core::CallExpr>(call->substitute(builder.getNodeManager(), *this));
            const core::ExpressionPtr& fun = call->getFunctionExpr();
            vector<core::ExpressionPtr> args = call->getArguments();

            if(core::LiteralPtr literal = core::dynamic_pointer_cast<const core::Literal>(fun)) {
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

                // synchronization
                if(literal->getValue() == "ocl_barrier") {
                    assert(args.size() == 1 && "Function barrier must have exactly 1 argument");

                    return kd.callBarrier(args.at(0));
                }

                // native math functions
                if(literal->getValue().find("native_") != string::npos) {
                    assert(args.size() >= 1 && "Native mathematical operations must have at least 1 arguments");

                    return resolveNative(literal->getValue(), 7, literal->getType(),
                            (args.size() == 1) ? BASIC.getAccuracyFastUnary() : BASIC.getAccuracyFastBinary(), args);
                }
                if(literal->getValue().find("half_") != string::npos) { // since half is mapped to float we can use a low accuracy method
                    assert(args.size() >= 1 && "Mathematical operations must have at least 1 arguments");

                    return resolveNative(literal->getValue(), 5, literal->getType(),
                            (args.size() == 1) ? BASIC.getAccuracyFastUnary() : BASIC.getAccuracyFastBinary(), args);
                }
                if(literal->getValue() == "mul24") { // since it has lower precision and should be faster than standard mul it is mapped to accuracy.fast(mul)
                    assert(args.size() == 2 && "mul24 must have 2 arguments");

                    return resolveNative(literal->getValue(), 0, literal->getType(), BASIC.getAccuracyFastBinary(), args);
                }

                if(literal->getValue() == "fma") { // since it has lower precision and should be faster than standard mul it is mapped to accuracy.fast(mul)
                    assert(args.size() == 3 && "fma must have 3 arguments");

                    // construct function type: all elements have the same (real) datatype, but only 2 instead of three arguments
                    core::TypePtr fctType = builder.functionType(toVector(args.at(0)->getType(), args.at(0)->getType()), args.at(0)->getType());
                    return resolveNative("real.add", 0, fctType, BASIC.getAccuracyHighBinary(), toVector((core::ExpressionPtr)resolveNative(
                        "real.mul", 0, fctType, BASIC.getAccuracyHighBinary(), toVector(args.at(0), args.at(1))) , args.at(2)));
                }

                // vector conversion function
                if(literal->getValue().find("convert_") != string::npos) {
                    assert(args.size() == 1 && "Convert operations must have exactly 1 argument");

                    return resolveConvert(call, literal->getValue(), literal->getType(), args);
                }
            }

            return call;//element->substitute(builder.getNodeManager(), *this);
        }

        if(element->getNodeType() == core::NodeType::NT_LambdaExpr || element->getNodeType() == core::NodeType::NT_BindExpr) {
            core::BindExprPtr bind = core::dynamic_pointer_cast<const core::BindExpr>(element);
            core::LambdaExprPtr fun = bind ? // if we are in a bind expression we get the lambda out of it
                    core::dynamic_pointer_cast<const core::LambdaExpr>(bind->getCall()->getFunctionExpr()) : //TODO to be tested
                    core::dynamic_pointer_cast<const core::LambdaExpr>(element); // else we are in a lambda expession;

            // create a new KernelMapper to check if we need to capture a range variable and pass them if nececarry
            KernelData lambdaData(builder);
            KernelMapper lambdaMapper(builder, lambdaData);

            // transform body of lambda
            core::StatementPtr newBody = core::dynamic_pointer_cast<const core::Statement>(fun->getBody()->substitute(builder.getNodeManager(), lambdaMapper));

            // store capture list of function (if existent)
            ArgList args;
            core::Lambda::ParamList bindArgs;;

            if(bind) {
                args.first = fun->getParameterList();
                args.second = bind->getCall()->getArguments();
                bindArgs = bind->getParameters();
            } else {
                core::VariablePtr tmpVar = builder.variable(fun->getParameterList().at(0)->getType());
                ADD_PARAM(args, fun->getParameterList().at(0), tmpVar);
                bindArgs.push_back(tmpVar);
            }

            // add needed variables to the capture list
            if(lambdaData.globalRangeUsed) {
                kd.globalRangeUsed = true;
                ADD_PARAM(args, lambdaData.globalRange, kd.globalRange);
            }
            if(lambdaData.numGroupsUsed){
                kd.numGroupsUsed = true;
                ADD_PARAM(args, lambdaData.numGroups, kd.numGroups);
            }
            if(lambdaData.localRangeUsed) {
                kd.localRangeUsed = true;
                ADD_PARAM(args, lambdaData.localRange, kd.localRange);
            }
            core::TypePtr retTy = dynamic_pointer_cast<const core::FunctionType>(fun->getType())->getReturnType();

            return builder.bindExpr(bindArgs, builder.callExpr(builder.lambdaExpr(retTy, newBody, args.first), args.second));
        }

        if (core::DeclarationStmtPtr decl = dynamic_pointer_cast<const core::DeclarationStmt>(element)) {
//            std::cout << "a variable declaration " << (element.hasAnnotation(insieme::ocl::BaseAnnotation::KEY) ? "with . attributes " : " -  ") <<
//                    (element->hasAnnotation(insieme::ocl::BaseAnnotation::KEY) ? "with -> attributes \n" : " -  \n");
//            std::cout << "variable: " << decl->toString() << std::endl;

            if(decl->getVariable()->hasAnnotation(insieme::ocl::BaseAnnotation::KEY)) {
                insieme::ocl::BaseAnnotationPtr annot = decl->getVariable()->getAnnotation(insieme::ocl::BaseAnnotation::KEY);
                for(insieme::ocl::BaseAnnotation::AnnotationList::const_iterator I = annot->getAnnotationListBegin(),
                        E = annot->getAnnotationListEnd(); I != E; ++I) {
                    if(insieme::ocl::AddressSpaceAnnotationPtr asa = std::dynamic_pointer_cast<insieme::ocl::AddressSpaceAnnotation>(*I)) {
                        switch(asa->getAddressSpace()) {
                        case insieme::ocl::AddressSpaceAnnotation::LOCAL: {
                            core::ExpressionPtr init;
                            core::TypePtr varType = decl->getVariable()->getType();
                            core::NodeType derefType = tryDeref(decl->getVariable())->getType()->getNodeType();

                            if(derefType == core::NT_ArrayType || derefType == core::NT_VectorType)
                                init = builder.refVar(builder.callExpr(BASIC.getUndefined(), BASIC.getTypeLiteral(tryDeref(decl->getVariable())->getType())));
                            else if (varType->getNodeType() == core::NT_RefType)
                                init = builder.refVar(builder.castExpr(tryDeref(decl->getVariable())->getType(), builder.intLit(0)));
                            else {
                                init = builder.castExpr(varType, builder.intLit(0));
                            }

                            // store the variable in list, initialized with zero, will be declared in job shared var list
                            localVars.push_back(builder.declarationStmt(decl->getVariable(), init));

                            if(init == decl->getInitialization()) // place a noop if variable is only initialized with zeros (already done above)
                                return BASIC.getNoOp();
                            // write the variable with it's initialization to the place the declaration was
                            // if it was a call to refVar, remove it and replace it by it's argument
                            return removeRefVar(decl);
                            break;
                        }
                        case insieme::ocl::AddressSpaceAnnotation::PRIVATE: {
                            privateVars.push_back(decl->getVariable());
                            break;
                        }
                        case insieme::ocl::AddressSpaceAnnotation::GLOBAL: {
                            core::CallExprPtr init = builder.refVar(builder.callExpr(BASIC.getUndefined(),
                                 BASIC.getTypeLiteral(tryDeref(decl->getVariable())->getType())));
                             // store the variable in list, initialized with zero, will be declared in global variable list
                             globalVars.push_back(builder.declarationStmt(decl->getVariable(), init));

                             if(init == decl->getInitialization()) // place a noop if variable is only initialized with zeros (already done above)
                                 return BASIC.getNoOp();
                             // write the variable with it's initialization to the place the declaration was
                             // if it was a call to refVar, remove it and replace it by it's argument
                             return removeRefVar(decl);
                             break;
                        }
                        case insieme::ocl::AddressSpaceAnnotation::CONSTANT: {
                            assert(false && "Address space CONSTANT not allowed for local variables");
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

        // translate casts from scalars to OpenCL vectors to vector init expressions
        // moved to expr_convert.cpp

        return element->substitute(builder.getNodeManager(), *this);
    }


    // gets vectors of variables and appends the variables found in the function body to them
    void getMemspaces(std::vector<core::VariablePtr>& constantV, std::vector<core::VariablePtr>& globalV,
            std::vector<core::VariablePtr>& localV, std::vector<core::VariablePtr>& privateV){
        append(globalV, globalVars);
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

    // returns vector containing the private variables
    std::vector<core::DeclarationStmtPtr>& getGlobalDeclarations() {
        return globalVars;
    }

    void resetMemspaces() {
        globalVars.clear();
        localVars.clear();
        privateVars.clear();
    }
};

class OclMapper : public core::transform::CachedNodeMapping {
    const core::ASTBuilder& builder;

private:
    template <typename T>
    void appendToVector(std::vector<T>& outVec, std::vector<T>& inVec) {
        for(auto I = inVec.begin(), E = inVec.end(); I != E; I++) {
            outVec.push_back(*I);
        }
    }

    template <typename T>
    void appendToVectorOrdered(std::vector<T>& outVec, std::vector<T>& conVec, std::vector<T>& gloVec, std::vector<T>& locVec, std::vector<T>& priVec,
            std::vector<OCL_ADDRESS_SPACE>& order) {
        size_t con = 0, glo = 0, loc = 0, pri = 0;
        for(auto I = order.begin(), E = order.end(); I != E; I++) {
            switch (*I) {
            case CONSTANT: outVec.push_back(conVec.at(con++)); break;
            case GLOBAL: outVec.push_back(gloVec.at(glo++)); break;
            case LOCAL: outVec.push_back(locVec.at(loc++)); break;
            case PRIVATE: outVec.push_back(priVec.at(pri++)); break;
            }
        }
    }

    // function to calculate the product of all elements in a vector
    core::ExpressionPtr vecProduct(core::VariablePtr vec, size_t n) {
        assert(vec->getType()->getNodeType() == core::NodeType::NT_VectorType && "function vecProduct is only allowed for vector variables\n");

        return builder.callExpr(BASIC.getUInt4(), BASIC.getVectorReduction(), vec, builder.uintLit(1u), BASIC.getUnsignedIntMul());

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
    // The elements of the input vector are stored as initialization values in vector inits for the new variables
    void createArgList(ArgList& outVec, std::vector<core::VariablePtr>& inVec,
        core::TypeList& types) {
        for_each(inVec, [&](core::VariablePtr& in) {
/*            outVec.first.push_back(in);
            in = builder.variable((in)->getType());
            outVec.second.push_back(in);

            types.push_back(in->getType());*/
            core::VariablePtr tmp = builder.variable(in->getType());

            // capture a newly generated variable and initialize the one in in in it
            outVec.first.push_back(in);
            outVec.second.push_back(tmp);

            // store the new variable in in
            in = tmp;
            types.push_back(in->getType());
        });
    }

    void createArgList(ArgList& outVec, std::vector<core::DeclarationStmtPtr>& inVec,
        core::TypeList& types) {
        for_each(inVec, [&](core::DeclarationStmtPtr& in) {
            core::DeclarationStmtPtr tmp = builder.declarationStmt(in->getVariable()->getType(), in->getInitialization());

            // capture a newly generated variable and initialize the one in in in it
            outVec.first.push_back(in->getVariable());
            outVec.second.push_back(tmp->getVariable());

            // store the new variable in in
            in = tmp;
            types.push_back(in->getVariable()->getType());
        });
    }

    void initArgInList(ArgList& outVec, std::vector<core::DeclarationStmtPtr>& inVec,
        core::TypeList& types) {
        for_each(inVec, [&](core::DeclarationStmtPtr& in) {
            // capture a newly generated variable and initialize the one in (*I) in it
            outVec.first.push_back(in->getVariable());
            outVec.second.push_back(in->getInitialization());

            types.push_back(in->getVariable()->getType());
        });
    }



    core::ExpressionPtr genBindExpr(core::StatementPtr& body, OCL_SCOPE scope, KernelMapper& kernelMapper, KernelData& kd, std::vector<core::VariablePtr>&
            constantArgs, std::vector<core::VariablePtr>& globalArgs, std::vector<core::VariablePtr>& localArgs, std::vector<core::VariablePtr>& privateArgs) {
        // define and init all arguments

        ArgList arguments;
        core::TypeList argTypes;

        createArgList(arguments, constantArgs, argTypes);
        createArgList(arguments, globalArgs, argTypes);
        createArgList(arguments, localArgs, argTypes);
        createArgList(arguments, privateArgs, argTypes);

        // in-body local variables
        if(scope == OCL_LOCAL_JOB /*|| scope == OCL_LOCAL_PAR*/)
            createArgList(arguments, kernelMapper.getLocalDeclarations(), argTypes);
        // in-body pointers to global variables, map to a new variable at local scope
        if(scope == OCL_LOCAL_JOB)
            createArgList(arguments, kernelMapper.getGlobalDeclarations(), argTypes);
        else // map the existing variable to it's init expression
            initArgInList(arguments, kernelMapper.getGlobalDeclarations(), argTypes);

        kd.appendArguments(arguments, scope, argTypes);

        core::FunctionTypePtr funType = builder.functionType(argTypes, builder.getNodeManager().basic.getUnit());


        return builder.bindExpr(std::vector<core::VariablePtr>(), builder.callExpr(builder.lambdaExpr(funType, arguments.first, body), arguments.second));
    }

public:

    OclMapper(core::ASTBuilder& astBuilder)
        :  builder(astBuilder)
          { };

    const core::NodePtr resolveElement(const core::NodePtr& element) {
        // quick check - stop recursion at variables
        if (element->getNodeCategory() == core::NodeCategory::NC_Type) {
            return element;//->substitute(builder.getNodeManager(), *this);
        }

        if(element->getNodeType() == core::NT_MarkerExpr) {
        	// check if we are at a function node
            if(const core::LambdaExprPtr& func = dynamic_pointer_cast<const core::LambdaExpr>(
        	        static_pointer_cast<const core::MarkerExpr>(element)->getSubExpression())) {

//        if(newNode->getNodeType() == core::NodeType::NT_LambdaExpr && false){
//            return builder.lambdaExpr(func->getType(), func->getParams(), builder.compoundStmt());

                bool isKernelFunction = false;
                bool workGroupSizeDefined = false;

                auto cName = func->getAnnotation(c_info::CNameAnnotation::KEY);
                auto sourceLoc = func->getAnnotation(c_info::CLocAnnotation::KEY);
                auto funcAnnotation = element->getAnnotation(insieme::ocl::BaseAnnotation::KEY);

                if(!funcAnnotation)
                    return element->substitute(builder.getNodeManager(), *this);

                size_t wgs[3];
                for(insieme::ocl::BaseAnnotation::AnnotationList::const_iterator I = funcAnnotation->getAnnotationListBegin(),
                        E = funcAnnotation->getAnnotationListEnd(); I != E; ++I) {
                    insieme::ocl::AnnotationPtr annot = (*I);

                    if(insieme::ocl::WorkGroupSizeAnnotationPtr wgsap = std::dynamic_pointer_cast<insieme::ocl::WorkGroupSizeAnnotation>(annot)) {
                        workGroupSizeDefined = true;
                        wgs[0] = wgsap->getXdim();
                        assert(wgs[0] > 0 && "Work group Size x-dimension has to be greater than 0.");
                        wgs[1] = wgsap->getYdim();
                        assert(wgs[1] > 0 && "Work group Size y-dimension has to be greater than 0.");
                        wgs[2] = wgsap->getZdim();
                        assert(wgs[2] > 0 && "Work group Size z-dimension has to be greater than 0.");
                    }
                    if(insieme::ocl::KernelFctAnnotationPtr kf = std::dynamic_pointer_cast<insieme::ocl::KernelFctAnnotation>(annot)) {
                        isKernelFunction = kf->isKernelFct();
                    }

                }

                assert(!(workGroupSizeDefined & !isKernelFunction) && "Attribute Reqd_work_group_size can only be defined for kernel functions");

                //if function is not a OpenCL kernel function recursively check for child nodes
                if(!isKernelFunction) {
                    return element->substitute(builder.getNodeManager(), *this);
                }

                core::Lambda::ParamList params = func->getParameterList();

                std::vector<OCL_ADDRESS_SPACE> argsOrder;

                // vectors to store Arguments
                std::vector<core::VariablePtr> constantArgs;
                std::vector<core::VariablePtr> globalArgs;
                std::vector<core::VariablePtr> localArgs;
                std::vector<core::VariablePtr> privateArgs;

                // store memory spaces of arguments
                for(core::Lambda::ParamList::iterator pi = params.begin(), pe = params.end(); pi != pe; pi++) {
                    core::VariablePtr var = *pi;
                    if(var->hasAnnotation(insieme::ocl::BaseAnnotation::KEY)) {
                        insieme::ocl::BaseAnnotationPtr annot = var->getAnnotation(insieme::ocl::BaseAnnotation::KEY);
                        for(insieme::ocl::BaseAnnotation::AnnotationList::const_iterator I = annot->getAnnotationListBegin(),
                                E = annot->getAnnotationListEnd(); I != E; ++I) {
                            if(insieme::ocl::AddressSpaceAnnotationPtr asa = std::dynamic_pointer_cast<insieme::ocl::AddressSpaceAnnotation>(*I)) {
                                switch(asa->getAddressSpace()) {
                                case insieme::ocl::AddressSpaceAnnotation::GLOBAL: {
                                    globalArgs.push_back(var);
                                    argsOrder.push_back(GLOBAL);
                                    break;
                                }
                                case insieme::ocl::AddressSpaceAnnotation::CONSTANT: {
                                    constantArgs.push_back(var);
                                    argsOrder.push_back(CONSTANT);
                                    break;
                                }
                                case insieme::ocl::AddressSpaceAnnotation::LOCAL: {
                                    localArgs.push_back(var);
                                    argsOrder.push_back(LOCAL);
                                    break;
                                }
                                case insieme::ocl::AddressSpaceAnnotation::PRIVATE: {
                                    privateArgs.push_back(var);
                                    argsOrder.push_back(PRIVATE);
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
                        argsOrder.push_back(PRIVATE);
                    }
                }

                // struct holding information about the kernel function's body
                KernelData kd(builder);
                KernelMapper kernelMapper(builder, kd);


                // update the type of the function
                core::FunctionTypePtr newFuncType;
                if(core::FunctionTypePtr funcType = core::dynamic_pointer_cast<const core::FunctionType>(func->getType())){
                    core::TypePtr retTy = funcType->getReturnType();

                    //check return type
                    assert(retTy->getNodeManager().basic.isUnit(retTy) && "Return type of kernel functions must be void.");

                    core::TypeList args = funcType->getParameterTypes();
                    args.push_back(kd.globalRange->getType());
                    args.push_back(kd.localRange->getType());

                    newFuncType = builder.functionType(args, retTy);
                } else {
                    assert(funcType && "Function has unexpected type");
                }

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

                   std::vector<core::ExpressionPtr> expr;

                   core::JobExpr::GuardedStmts noGuardedStatementsNeeded;

    // generation/composition of constructs

                    // build expression to be used as body of local job
                    core::ExpressionPtr localParFct = genBindExpr(newBody, OCL_LOCAL_JOB, kernelMapper, kd, constantArgs, globalArgs, localArgs, privateArgs);

                    core::JobExpr::LocalDecls localJobShared;

                    createDeclarations(localJobShared, constantArgs);
                    createDeclarations(localJobShared, globalArgs);
                    createDeclarations(localJobShared, localArgs);
                    // in-body local variables
                    appendToVector(localJobShared, kernelMapper.getLocalDeclarations());
                    // catch loop boundaries
                    kd.appendShared(localJobShared, OCL_LOCAL_JOB);
                    // TODO catch global variables

                    // calculate localRange[0] * localRange[1] * localRange[2] to use as maximum number of threads
                    core::ExpressionPtr localRangeProduct = vecProduct(kd.localRange, 3);

                    // min and max number of threads is equal
                    core::CallExprPtr localThreadNum = builder.callExpr(BASIC.getCreateBoundRange(), localRangeProduct, localRangeProduct);

                    core::JobExprPtr localJob = builder.jobExpr(localThreadNum, localParFct, noGuardedStatementsNeeded, localJobShared);

                    expr.clear();
                    //construct vector of arguments for local parallel

//                    expr.push_back(localRangeProduct);
//                    expr.push_back(localRangeProduct); // min and max threads are equal

                    expr.push_back(localJob);

                    core::CallExprPtr localPar = builder.callExpr(builder.getNodeManager().basic.getThreadGroup(),
                            builder.getNodeManager().basic.getParallel(), expr);

                    std::vector<core::StatementPtr> gobalBodyStmts;
                    gobalBodyStmts.push_back(localPar);
                    expr.clear();
                    core::CallExprPtr merge = builder.callExpr(BASIC.getMergeAll(), expr);
                    gobalBodyStmts.push_back(merge);
                    core::StatementPtr globalParBody = builder.compoundStmt(gobalBodyStmts);


                    // build expression to be used as body of global job
                    core::ExpressionPtr globalParFct = genBindExpr(globalParBody, OCL_GLOBAL_JOB, kernelMapper, kd, constantArgs, globalArgs, localArgs,
                            privateArgs);

                    // catch all arguments which are shared in global range
                    core::JobExpr::LocalDecls globalJobShared;

                    createDeclarations(globalJobShared, constantArgs);
                    createDeclarations(globalJobShared, globalArgs);
                    // catch loop boundaries
                    kd.appendShared(globalJobShared, OCL_GLOBAL_JOB);
                    // TODO catch global variables

                    // calculate groupRange[0] * groupRange[1] * groupRange[2] to use as maximum number of threads
                    core::ExpressionPtr globalRangeProduct = vecProduct(kd.numGroups, 3);

                    // min and max number of threads is equal
                    core::CallExprPtr globalThreadNum = builder.callExpr(BASIC.getCreateBoundRange(), globalRangeProduct, globalRangeProduct);

                    core::JobExprPtr globalJob = builder.jobExpr(globalThreadNum, globalParFct, noGuardedStatementsNeeded, globalJobShared);

                    expr.clear();
                    //construct vector of arguments for local parallel

                    expr.push_back(globalJob);

                    core::CallExprPtr globalPar = builder.callExpr(builder.getNodeManager().basic.getThreadGroup(), builder.getNodeManager().basic.getParallel(), expr);

                    // construct updated param list
                    core::Lambda::ParamList newParams;
                    appendToVectorOrdered(newParams, constantArgs, globalArgs, localArgs, privateArgs, argsOrder);

                    newParams.push_back(kd.globalRange); // add global range to parameters

                    std::vector<core::StatementPtr> newBodyStmts;

                    // add local range to parameters
                    if(!workGroupSizeDefined)
                        newParams.push_back(kd.localRange); // add the local range argument
                    else {
                        // add a variable that will never be used just to keep interface consistent
                        newParams.push_back(builder.variable(builder.vectorType(BASIC.getUInt4(),
                        		builder.concreteIntTypeParam(static_cast<size_t>(3)))));

                        //declare and set the local range if provided by work group size attribute
                        core::DeclarationStmtPtr lrd = builder.declarationStmt(kd.localRange, builder.vectorExpr(toVector<core::ExpressionPtr>(
                                builder.uintLit(wgs[0]), builder.uintLit(wgs[1]), builder.uintLit(wgs[2]))));
                        newBodyStmts.push_back(lrd);
                    }
                    //declare group range TODO fix error of checker
                    core::TypePtr vecUint = builder.vectorType(BASIC.getUInt4(), builder.concreteIntTypeParam(static_cast<size_t>(3)));
                    core::DeclarationStmtPtr groupRdecl = builder.declarationStmt(kd.numGroups,
                            builder.callExpr(vecUint,
                            builder.callExpr(builder.functionType(toVector(vecUint, vecUint), vecUint ),
                                    BASIC.getVectorPointwise(), BASIC.getUnsignedIntDiv()), kd.globalRange, kd.localRange));

                    newBodyStmts.push_back(groupRdecl);

                    //core::DeclarationStmtPtr groupThreadGroup = builder.declarationStmt(kd.groupTg, globalPar); inlined, see next line, created only if needed

                    newBodyStmts.push_back(globalPar);
                    newBodyStmts.push_back(merge);

                    core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, newParams, builder.compoundStmt(newBodyStmts));

                    // get address spaces of variables in body
    //                kernelMapper.getMemspaces(globalArgs, constantArgs, localArgs, privateArgs);

                    // put opencl annotation to the new function for eventual future use

                    newFunc->addAnnotation(funcAnnotation);
                    // put cname annotation to the new function if it was there before
                    if(cName)
                        newFunc->getLambda()->addAnnotation(cName);
                    // put source location annotation to it if existent
                    if(sourceLoc)
                        newFunc->addAnnotation(sourceLoc);

                    return newFunc;
                }
            }

/*            const core::Node::ChildList& children = body->getChildList();

            std::for_each(children.begin(), children.end(),
                    [] (const core::NodePtr& curr) {
                //look for ocl build-in functions and translate them to IR statements

                }
            );*/

        }

        return element->substitute(builder.getNodeManager(), *this);
    }
};

}

core::ProgramPtr Compiler::lookForOclAnnotations() {
//    core::RecursiveASTVisitor<OclVisitor> visitor(oclAnnotationExpander);
//    core::visitAll(mProgram, oclAnnotationExpander);

//    OclVisitor ov(builder, mProgram);
//    core::visitAll(core::ProgramAddress(mProgram), ov);

    OclMapper oclAnnotationExpander(builder);

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
