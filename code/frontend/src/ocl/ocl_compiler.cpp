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

namespace ba = boost::algorithm;

namespace insieme {
namespace frontend {
namespace ocl {

namespace {
/* not needed any more
core::LambdaExprPtr KernelData::buildGetId(enum OCL_PAR_LEVEL opl) {
    if(opl == OPL_GLOBAL) {
        // do complicated stuff
    } else {
        core::LiteralPtr level = builder.uintLit(opl == OPL_GROUP ? 1 : 0);
        core::VariablePtr idx = builder.variable(BASIC.getUInt4());
        core::VariablePtr boundaries = builder.variable(builder.vectorType(BASIC.getUInt4(), core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3))));

        core::ExpressionPtr one = builder.uintLit(1);
        core::ExpressionPtr two = builder.uintLit(2);

        core::ReturnStmtPtr id0 = builder.returnStmt(builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntDiv(),
            builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntDiv(), builder.callExpr(BASIC.getUInt4(), BASIC.getGetThreadId(), level),
                vecAccess(boundaries, two)), vecAccess(boundaries, one)));

        core::ReturnStmtPtr id1 = builder.returnStmt(builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMod(),
            builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntDiv(), builder.callExpr(BASIC.getUInt4(), BASIC.getGetThreadId(), level),
                vecAccess(boundaries, two)), vecAccess(boundaries, one)));

        core::ReturnStmtPtr id2 = builder.returnStmt(builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMod(),
            builder.callExpr(BASIC.getUInt4(), BASIC.getGetThreadId(), level), vecAccess(boundaries, two)));

        vector<core::SwitchStmt::Case> cases;

        cases.push_back(std::make_pair(builder.uintLit(0), id0));
        cases.push_back(std::make_pair(builder.uintLit(1), id1));
        cases.push_back(std::make_pair(builder.uintLit(2), id2));

        core::SwitchStmtPtr swtch = builder.switchStmt(idx, cases);

        core::ASTBuilder::CaptureInits capture;

        capture[opl == OPL_GROUP ? numGroups : localRange] = boundaries;

        std::vector<core::StatementPtr> body;
//        core::FunctionTypePtr funTy = builder.f

        return builder.lambdaExpr(BASIC.getUInt4(), swtch, toVector(boundaries),
            toVector(idx));
    }

}*/

core::CallExprPtr KernelData::calcIdidx0(core::LiteralPtr& level, core::VariablePtr& boundaries){
    core::ExpressionPtr one = builder.uintLit(1u);
    core::ExpressionPtr two = builder.uintLit(2u);
    return builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntDiv(),
               builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntDiv(), builder.callExpr(BASIC.getUInt4(), BASIC.getGetThreadId(), level),
                   vecAccess(boundaries, two)), vecAccess(boundaries, one));
}

core::CallExprPtr KernelData::calcIdidx1(core::LiteralPtr& level, core::VariablePtr& boundaries){
    core::ExpressionPtr one = builder.uintLit(1u);
    core::ExpressionPtr two = builder.uintLit(2u);
    return builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMod(),
               builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntDiv(), builder.callExpr(BASIC.getUInt4(), BASIC.getGetThreadId(), level),
                   vecAccess(boundaries, two)), vecAccess(boundaries, one));
}

core::CallExprPtr KernelData::calcIdidx2(core::LiteralPtr& level, core::VariablePtr& boundaries){
    core::ExpressionPtr two = builder.uintLit(2u);
    return builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMod(),
               builder.callExpr(BASIC.getUInt4(), BASIC.getGetThreadId(), level), vecAccess(boundaries, two));
}

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

core::CallExprPtr KernelData::accessId(OCL_PAR_LEVEL opl, core::ExpressionPtr idx){
/*    switch(opl) {
    case OPL_GLOBAL :
        localRangeUsed = true;
        return builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntAdd(), builder.callExpr(BASIC.getGetThreadId(), idx),
            builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMul(), vecAccess(localRange, idx),
                builder.callExpr(BASIC.getGetThreadId(), idx)));
    case OPL_GROUP :
        return builder.callExpr(BASIC.getUInt4(), BASIC.getGetThreadId(), idx);
    //case OPL_LOCAL :
    default:
        return builder.callExpr(//BASIC.getUInt4(), BASIC.getGetThreadId(), idx);
                BASIC.getUInt4(), buildGetId(opl), idx);
    }*/

    //TODO add test
    core::VariablePtr idxVar = builder.variable(BASIC.getUInt4());
    core::VariablePtr boundaries = builder.variable(builder.vectorType(BASIC.getUInt4(), core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3))));
    core::VariablePtr bfgo = builder.variable(builder.vectorType(BASIC.getUInt4(), core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3))));

    core::LiteralPtr zero = builder.uintLit(0u);
    core::LiteralPtr one = builder.uintLit(1u);

    // TODO make prettier
    core::ExpressionPtr zeroExpr = zero;
    core::ExpressionPtr oneExpr = one;
    core::ExpressionPtr twoExpr = builder.uintLit(2);

    core::LiteralPtr level = builder.uintLit(opl == OPL_GROUP ? 1u : 0u);

    core::CallExprPtr id0 = opl == OPL_GLOBAL ?
        builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntAdd(), calcIdidx0(zero, boundaries),
            builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMul(), vecAccess(boundaries, zeroExpr), calcIdidx0(one, bfgo)))
        :
        calcIdidx0(level, boundaries);

    core::CallExprPtr id1 = opl == OPL_GLOBAL ?
        builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntAdd(), calcIdidx1(zero, boundaries),
            builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMul(), vecAccess(boundaries, oneExpr), calcIdidx1(one, bfgo)))
        :
        calcIdidx1(level, boundaries);

    core::CallExprPtr id2 = opl == OPL_GLOBAL ?
        builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntAdd(), calcIdidx2(zero, boundaries),
            builder.callExpr(BASIC.getUInt4(), BASIC.getUnsignedIntMul(), vecAccess(boundaries, twoExpr), calcIdidx1(one, bfgo)))
        :
        calcIdidx2(level, boundaries);

    core::ReturnStmtPtr ret0 = builder.returnStmt(id0);
    core::ReturnStmtPtr ret1 = builder.returnStmt(id1);
    core::ReturnStmtPtr ret2 = builder.returnStmt(id2);

    vector<core::SwitchStmt::Case> cases;

    cases.push_back(std::make_pair(builder.uintLit(0), ret0));
    cases.push_back(std::make_pair(builder.uintLit(1), ret1));
    cases.push_back(std::make_pair(builder.uintLit(2), ret2));

    core::SwitchStmtPtr swtch = builder.switchStmt(idxVar, cases);

    core::ASTBuilder::CaptureInits capture;
    switch(opl) {
    case OPL_GLOBAL :
        localRangeUsed = true;
        numGroupsUsed = true;
        capture[bfgo] = numGroups;
        capture[boundaries] = localRange;
        break;
    case OPL_GROUP :
        numGroupsUsed = true;
        capture[boundaries] = numGroups;
        break;
    case OPL_LOCAL :
        localRangeUsed = true;
        capture[boundaries] = localRange;
    }

    return builder.callExpr(BASIC.getUInt4(), builder.lambdaExpr(BASIC.getUInt4(), swtch, capture, toVector(idxVar)), idx);
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

    core::CallExprPtr resolveNative(const core::CallExprPtr& nativeOp, const string& name, const core::TypePtr& type, const vector<core::ExpressionPtr>& args,
            size_t preambleLength) {
        assert((args.size() == 1 || args.size() == 2) && "Only native OpenCL functions with one or two arguments are supported");

        if(name  == "native_divide")
            return builder.callExpr(builder.callExpr(BASIC.getAccuracyFastBinary(), BASIC.getRealDiv()), args);

        core::CallExprPtr nativeFct = args.size() == 1 ?
                builder.callExpr(BASIC.getAccuracyFastUnary(), builder.literal(name.substr(preambleLength,name.size()), type)) :
                builder.callExpr(BASIC.getAccuracyFastBinary(), builder.literal(name.substr(preambleLength,name.size()), type));

        return builder.callExpr(nativeFct, args);
    }


    core::CastExprPtr resolveConvert(const core::CallExprPtr& castOp, const string& name, const core::TypePtr& type, const vector<core::ExpressionPtr>& args) {
        assert((args.size() == 1) && "Only cast OpenCL functions with one arguments are supported");

        if(core::FunctionTypePtr ftype = dynamic_pointer_cast<const core::FunctionType>(type)) {
            return builder.castExpr(ftype->getReturnType(), args.at(0));
        }

        assert(false && "Type of OpenCL convert function is not a function Type");

        return NULL;
    }

public:


    KernelMapper(const core::ASTBuilder& astBuilder, KernelData& data)
    : builder(astBuilder), kd(data) { };

    const core::NodePtr resolveElement(const core::NodePtr& element) {


        if(core::CallExprPtr call = core::dynamic_pointer_cast<const core::CallExpr>(element)){
//            std::cout << "found a call\n";
            const core::ExpressionPtr& fun = call->getFunctionExpr();
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

                // synchronization
                if(literal->getValue() == "ocl_barrier") {
                    assert(args.size() == 1 && "Function barrier must have exactly 1 argument");

                    return kd.callBarrier(args.at(0));
                }

                // native math functions
                if(literal->getValue().find("native_") != string::npos) {
                    assert(args.size() >= 1 && "Native mathematical operations must have at least 1 arguments");

                    return resolveNative(call, literal->getValue(), literal->getType(), args, 7);
                }
                if(literal->getValue().find("half_") != string::npos) { // since half is mapped to float we can use a low accuracy method
                    assert(args.size() >= 1 && "Mathematical operations must have at least 1 arguments");

                    return resolveNative(call, literal->getValue(), literal->getType(), args, 5);
                }
                if(literal->getValue().find("convert_") != string::npos) {
                    assert(args.size() == 1 && "Convert operations must have exactly 1 argument");

                    return resolveConvert(call, literal->getValue(), literal->getType(), args);
                }
            }


//        std::cout << "FUNCTION: " << call << std::endl;
/*            const core::Node::ChildList& elems = call->getChildList();
            for(size_t i = 0; i < elems.size(); ++i) {
                std::cout << "child: " << elems.at(i) << std::endl;
            }
*/
            return element->substitute(builder.getNodeManager(), *this);
        }

        if(element->getNodeType() == core::NodeType::NT_LambdaExpr || element->getNodeType() == core::NodeType::NT_CaptureInitExpr) {
            core::CaptureInitExprPtr cie = core::dynamic_pointer_cast<const core::CaptureInitExpr>(element);
            core::LambdaExprPtr fun = cie ? // if we are in a capture init expression we get the lambda out of it
                    core::dynamic_pointer_cast<const core::LambdaExpr>(cie->getLambda()) :
                    core::dynamic_pointer_cast<const core::LambdaExpr>(element); // else we are in a lambda expession;

            // create a new KernelMapper to check if we need to capture a range variable and pass them if nececarry
            KernelData lambdaData(builder);
            KernelMapper lambdaMapper(builder, lambdaData);

            // transform body of lambda
            core::StatementPtr newBody = core::dynamic_pointer_cast<const core::Statement>(fun->getBody()->substitute(builder.getNodeManager(), lambdaMapper));

            // store capture list of function (if existent)
            core::ASTBuilder::CaptureList funCaptures = fun->getCaptureList();;
            core::CaptureInitExpr::Values funCaptInits;
            if(cie) funCaptInits = cie->getValues();

            // add needed variables to the capture list
            if(lambdaData.globalRangeUsed) {
                kd.globalRangeUsed = true;
                funCaptures.push_back(lambdaData.globalRange);
                funCaptInits.push_back(kd.globalRange);
            }
            if(lambdaData.numGroupsUsed){
                kd.numGroupsUsed = true;
                funCaptures.push_back(lambdaData.numGroups);
                funCaptInits.push_back(kd.numGroups);
            }
            if(lambdaData.localRangeUsed) {
                kd.localRangeUsed = true;
                funCaptures.push_back(lambdaData.localRange);
                funCaptInits.push_back(kd.localRange);
            }

            return builder.captureInitExpr(builder.lambdaExpr(newBody, funCaptures, fun->getParameterList()), funCaptInits);

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
                            localVars.push_back(decl);
                            return builder.getNodeManager().basic.getNoOp();
                            break;
                        }
                        case insieme::ocl::AddressSpaceAnnotation::PRIVATE: {
                            privateVars.push_back(decl->getVariable());
                            break;
                        }
                        case insieme::ocl::AddressSpaceAnnotation::CONSTANT: {
                            assert(false && "Address space CONSTANT not allowed for local variables");
                        }
                        case insieme::ocl::AddressSpaceAnnotation::GLOBAL: {
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

        // translate casts from scalars to OpenCL vectors to vector init expressions
        // moved to expr_convert.cpp
//        if(core::CastExprPtr cast = core::dynamic_pointer_cast<const core::CastExpr>(element)) {

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
    core::ExpressionPtr genLocalCie(core::StatementPtr& body, OCL_SCOPE scope, KernelMapper& kernelMapper, KernelData& kd, std::vector<core::VariablePtr>&
            constantArgs, std::vector<core::VariablePtr>& globalArgs, std::vector<core::VariablePtr>& localArgs, std::vector<core::VariablePtr>& privateArgs) {
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

        return builder.lambdaExpr(lpfType, body, localFunCaptures, core::Lambda::ParamList());
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
                    assert(retTy->getName() == "unit" && "Return type of kernel functions must be void.");

                    core::TypeList args = funcType->getArgumentTypes();
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
                    core::ExpressionPtr localParFct = genLocalCie(newBody, OCL_LOCAL_JOB, kernelMapper, kd, constantArgs, globalArgs, localArgs, privateArgs);

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
                    gobalBodyStmts.push_back(builder.callExpr(BASIC.getMergeAll(), expr));
                    core::StatementPtr globalParBody = builder.compoundStmt(gobalBodyStmts);


                    // build expression to be used as body of global job
                    core::ExpressionPtr globalParFct = genLocalCie(globalParBody, OCL_GLOBAL_JOB, kernelMapper, kd, constantArgs, globalArgs, localArgs,
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
                            core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3)))));

                        //declare and set the local range if provided by work group size attribute
                        core::DeclarationStmtPtr lrd = builder.declarationStmt(kd.localRange, builder.vectorExpr(toVector<core::ExpressionPtr>(
                                builder.uintLit(wgs[0]), builder.uintLit(wgs[1]), builder.uintLit(wgs[2]))));
                        newBodyStmts.push_back(lrd);
                    }

                    //declare group range
                    core::DeclarationStmtPtr groupRdecl = builder.declarationStmt(kd.numGroups,
                            builder.callExpr(builder.vectorType(BASIC.getUInt4(), core::IntTypeParam::getConcreteIntParam(static_cast<size_t>(3))),
                            builder.callExpr(BASIC.getVectorPointwise(), BASIC.getUnsignedIntDiv()), kd.globalRange, kd.localRange));

                    newBodyStmts.push_back(groupRdecl);

                    //core::DeclarationStmtPtr groupThreadGroup = builder.declarationStmt(kd.groupTg, globalPar); inlined, see next line, created only if needed

                    newBodyStmts.push_back(globalPar);

                    core::LambdaExprPtr newFunc = builder.lambdaExpr(newFuncType, newParams, builder.compoundStmt(newBodyStmts));

                    // get address spaces of variables in body
    //                kernelMapper.getMemspaces(globalArgs, constantArgs, localArgs, privateArgs);

                    // put opencl annotation to the new function for eventual future use

                    newFunc->addAnnotation(funcAnnotation);
                    // put cname annotation to the new function if it was there before
                    if(cName)
                        newFunc->addAnnotation(cName);

                    return newFunc;
                }
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
