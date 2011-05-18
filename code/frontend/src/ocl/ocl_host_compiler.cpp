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
#include "insieme/frontend/ocl/ocl_annotations.h"

namespace ba = boost::algorithm;

namespace insieme {
namespace frontend {
namespace ocl {
using namespace insieme::core;

namespace {

bool Ocl2Inspire::extractSizeFromSizeof(const core::ExpressionPtr& arg, core::ExpressionPtr& size, core::TypePtr& type ) {
    if(CallExprPtr&& mul = dynamic_pointer_cast<const CallExpr>(arg)) {
        for(int i = 0; i < 2; ++i) {
            if(CallExprPtr&& sizeof_ = dynamic_pointer_cast<const CallExpr>(mul->getArgument(i))) {
                if(sizeof_->toString().find("sizeof") != string::npos) {
                    // extract the type to be allocated
                    type = dynamic_pointer_cast<const Type>(sizeof_->getArgument(0)->getType()->getChildList().at(0));
                    // extract the number of elements to be allocated
                    size = mul->getArgument(1-i);
                    return true;
                }
            }
        }
    }
    return false;
}


ExpressionPtr Ocl2Inspire::getClCreateBuffer() {
    // flags ignored
    // hostPtr ignored
    // errcorcode always set to 0 = CL_SUCCESS
    return parser.parseExpression("fun(type<'a>:elemType, uint<8>:flags, uint<8>:size, anyRef:hostPtr, ref<array<int<4>, 1> >:errorcode_ret) -> array<'a, 1>  {{ \
            ( (op<array.ref.elem.1D>(errorcode_ret, lit<uint<8>, 0> )) = 0 ); \
            return (op<array.create.1D>( elemType, size )); \
       }}");
}

ExpressionPtr Ocl2Inspire::getClWriteBuffer() {
    // blocking_write ignored
    // event stuff removed
    // always returns 0 = CL_SUCCESS
    return parser.parseExpression("fun(ref<array<'a, 1> >:devicePtr, uint<4>:blocking_write, uint<8>:offset, uint<8>:cb, anyRef:hostPtr) -> int<4> {{ \
            decl ref<array<'a, 1> >:hp = (op<anyref.to.ref>(hostPtr, lit<type<array<'a, 1> >, type(array('a ,1)) > )); \
            for(decl uint<8>:i = lit<uint<8>, 0> .. cb : 1) \
                ( (op<array.ref.elem.1D>(devicePtr, (i + offset) )) = (op<ref.deref>( (op<array.ref.elem.1D>(hp, i )) )) ); \
            return 0; \
    }}");
}

ExpressionPtr Ocl2Inspire::getClWriteBufferFallback() {
    // blocking_write ignored
    // event stuff removed
    // always returns 0 = CL_SUCCESS
    return parser.parseExpression("fun(ref<array<'a, 1> >:devicePtr, uint<4>:blocking_write, uint<8>:offset, uint<8>:cb, anyRef:hostPtr) -> int<4> {{ \
            decl ref<array<'a, 1> >:hp = (op<anyref.to.ref>(hostPtr, lit<type<array<'a, 1> >, type<array<'a ,1 > )); \
            decl uint<8>:size = (cb / (op<sizeof>( lit<type<'a>, type('a) > )) ); \
            for(decl uint<8>:i = lit<uint<8>, 0> .. size : 1) \
                ( (op<array.ref.elem.1D>(devicePtr, (i + offset) )) = (op<ref.deref>( (op<array.ref.elem.1D>(hp, i )) )) ); \
            return 0; \
    }}");
}

ExpressionPtr Ocl2Inspire::getClReadBuffer() {
    // blocking_write ignored
    // event stuff removed
    // always returns 0 = CL_SUCCESS
    return parser.parseExpression("fun(ref<array<'a, 1> >:devicePtr, uint<4>:blocking_read, uint<8>:offset, uint<8>:cb, anyRef:hostPtr) -> int<4> {{ \
            decl ref<array<'a, 1> >:hp = (op<anyref.to.ref>(hostPtr, lit<type<array<'a, 1> >, type<array<'a ,1 > )); \
            for(decl uint<8>:i = lit<uint<8>, 0> .. cb : 1) \
                ( (op<array.ref.elem.1D>(hp, (i + offset) )) = (op<ref.deref>( (op<array.ref.elem.1D>(devicePtr, i )) )) ); \
            return 0; \
    }}");
}

ExpressionPtr Ocl2Inspire::getClReadBufferFallback() {
    // blocking_write ignored
    // event stuff removed
    // always returns 0 = CL_SUCCESS
    return parser.parseExpression("fun(ref<array<'a, 1> >:devicePtr, uint<4>:blocking_read, uint<8>:offset, uint<8>:cb, anyRef:hostPtr) -> int<4> {{ \
            decl ref<array<'a, 1> >:hp = (op<anyref.to.ref>(hostPtr, lit<type<array<'a, 1> >, type<array<'a ,1 > )); \
            decl uint<8>:size = (cb / (op<sizeof>( lit<type<'a>, type('a) > )) ); \
            for(decl uint<8>:i = lit<uint<8>, 0> .. size : 1) \
                ( (op<array.ref.elem.1D>(hp, (i + offset) )) = (op<ref.deref>( (op<array.ref.elem.1D>(devicePtr, i )) )) ); \
            return 0; \
    }}");
}

/*

 */
HostMapper::HostMapper(ASTBuilder& build) : builder(build), o2i(build.getNodeManager()) {
    ADD_Handler(builder, "clCreateBuffer",
        ExpressionPtr fun = o2i.getClCreateBuffer();

        // extract the size form argument size, relying on it using a multiple of sizeof(type)
        ExpressionPtr size;
        TypePtr type;
        ExpressionPtr hostPtr;

        assert(o2i.extractSizeFromSizeof(node->getArgument(2), size, type)
                && "Unable to deduce type from clCreateBuffer call:\nNo sizeof call found, cannot translate to INSPIRE.");

        if(CastExprPtr c = dynamic_pointer_cast<const CastExpr>(node->getArgument(3))) {
            if(c->getSubExpression()->getType() != BASIC.getAnyRef()) {// a scalar (probably NULL) has been passed as hostPtr arg
                hostPtr = builder.callExpr(BASIC.getRefToAnyRef(), builder.callExpr(BASIC.getRefVar(), c->getSubExpression()));
            }
        }
        else
            hostPtr = node->getArgument(3);

        vector<ExpressionPtr> args;
        args.push_back(BASIC.getTypeLiteral(type));
        args.push_back(node->getArgument(1));
        args.push_back(size);
        args.push_back(hostPtr);
        args.push_back(node->getArgument(4));
        return builder.callExpr(builder.arrayType(type), fun, args);
    );

    ADD_Handler(builder, "clEnqueueWriteBuffer",
        // extract the size form argument size, relying on it using a multiple of sizeof(type)
        ExpressionPtr size;
        TypePtr type;

        bool foundSizeOf = o2i.extractSizeFromSizeof(node->getArgument(4), size, type);

        vector<ExpressionPtr> args;
        args.push_back(node->getArgument(1));
        args.push_back(node->getArgument(2));
        args.push_back(node->getArgument(3));
        args.push_back(foundSizeOf ? size : node->getArgument(4));
        args.push_back(node->getArgument(5));
        return builder.callExpr(foundSizeOf ? o2i.getClWriteBuffer() : o2i.getClWriteBufferFallback(), args);
    );

    ADD_Handler(builder, "clEnqueueReadBuffer",
        // extract the size form argument size, relying on it using a multiple of sizeof(type)
        ExpressionPtr size;
        TypePtr type;

        bool foundSizeOf = o2i.extractSizeFromSizeof(node->getArgument(4), size, type);

        vector<ExpressionPtr> args;
        args.push_back(node->getArgument(1));
        args.push_back(node->getArgument(2));
        args.push_back(node->getArgument(3));
        args.push_back(foundSizeOf ? size : node->getArgument(4));
        args.push_back(node->getArgument(5));
        return builder.callExpr(foundSizeOf ? o2i.getClReadBuffer() : o2i.getClReadBufferFallback(), args);
    );

    ADD_Handler(builder, "clReleaseMemObject",
        return builder.callExpr(BASIC.getRefDelete(), node->getArgument(0));
    );

    ADD_Handler(builder, "clSetKernelArg",
        // arg_index must either be an integer literal or all arguments have to be specified in the right order in the source code
        const ExpressionPtr& kernel = node->getArgument(0);
        const ExpressionPtr& arg = node->getArgument(3);
        const ExpressionPtr& arg2 = node->getArgument(1);
        // check if the index argument is a (casted) integer literal
        const CastExprPtr& cast = dynamic_pointer_cast<const CastExpr>(arg2);
        if(const LiteralPtr& idx = dynamic_pointer_cast<const Literal>(cast ? cast->getSubExpression() : arg2)) {
            // use the literal as index for the argument
            unsigned int pos = atoi(idx->getValue().c_str());
            if(kernelArgs[kernel].size() <= pos)
                kernelArgs[kernel].resize(pos+1);

            kernelArgs[kernel].at(pos) = arg;
        } else {
            // use one argument after another
            kernelArgs[kernel].push_back(arg);
        }

        return builder.intLit(0); // returning CL_SUCCESS
    );

    ADD_Handler(builder, "oclLoadProgSource",
        NodePtr ret = node;
        if(const CallExprPtr& callSaC = dynamic_pointer_cast<const CallExpr>(node->getArgument(0))) {
            if(const LiteralPtr& stringAsChar = dynamic_pointer_cast<const Literal>(callSaC->getFunctionExpr())) {
                if(stringAsChar->getValue() == "string.as.char.pointer") {
                    if(const LiteralPtr& path = dynamic_pointer_cast<const Literal>(callSaC->getArgument(0))) {
                        // delete quotation marks form path
                        string p = path->getValue().substr(1, path->getValue().length()-2);
                        LOG(INFO) << "Converting kernel file '" << path->getValue() << "' to IR...";
                        frontend::Program fkernels(builder.getNodeManager());

                        fkernels.addTranslationUnit(p);
                        kernels = fkernels.convert();

                        // set source string to an empty char array
                        ret = builder.refVar(builder.literal("", builder.arrayType(BASIC.getChar())));
                    }
                }
            }
        }
        return ret;
    );

    ADD_Handler(builder, "clEnqueueNDRangeKernel",
        // get argument vector
        std::vector<core::ExpressionPtr> args = kernelArgs[node->getArgument(1)];
        assert(args.size() > 0u && "Cannot find any arguments for kernel function");
        // adding global and local size to the argument vector
        args.push_back(node->getArgument(4) );
        args.push_back(node->getArgument(5) );

//        std::cerr << "ARGUMENTS: " << toString(join(", ", args)) << std::endl;

        return node;
    );
};


core::CallExprPtr HostMapper::checkAssignment(const core::CallExprPtr& oldCall){
    CallExprPtr newCall;
    if((newCall = dynamic_pointer_cast<const CallExpr>(oldCall->substitute(builder.getNodeManager(), *this)))) {

        // get rid of deref operations, automatically inserted by the frontend coz _cl_mem* is translated to ref<array<...>>, and refs cannot be
        // rhs of an assignment
        if(const CallExprPtr& rhs = dynamic_pointer_cast<const CallExpr>(newCall->getArgument(1))) {
            if(rhs->getFunctionExpr() == BASIC.getRefDeref()) {
                if(const CallExprPtr& createBuffer = dynamic_pointer_cast<const CallExpr>(rhs->getArgument(0))) {
                    newCall = createBuffer;

                }
            }
        }
    }
    return newCall;
}


const NodePtr HostMapper::resolveElement(const NodePtr& element) {
    // stopp recursion at type level
    if (element->getNodeCategory() == NodeCategory::NC_Type) {
        return element->substitute(builder.getNodeManager(), *this);
    }

    if(const CallExprPtr& callExpr = dynamic_pointer_cast<const CallExpr>(element)){
 //       std::cout << callExpr->toString() << " FOUND\n";
        const ExpressionPtr& fun = callExpr->getFunctionExpr();
        vector<ExpressionPtr> args = callExpr->getArguments();

        if(const LiteralPtr& literal = dynamic_pointer_cast<const Literal>(fun)) {
            callExpr->substitute(builder.getNodeManager(), *this);
//            std::cout << "CALL: " << literal->getValue() << std::endl;
            if(const HandlerPtr& replacement = handles[literal->getValue()]) {
                NodePtr ret = replacement->handleNode(callExpr);
                // check if new kernels have been created
                vector<ExpressionPtr> kernels = replacement->getKernels();
                if(kernels.size() > 0)
                    for_each(kernels, [&](ExpressionPtr kernel){
                        kernelEntries.push_back(kernel);
                    });
                return ret;
            }
        }

        if(fun == BASIC.getRefAssign()) {
            if(const VariablePtr& lhs = dynamic_pointer_cast<const Variable>(callExpr->getArgument(0))) {
                if(lhs->getType() == builder.refType(builder.arrayType(builder.genericType("_cl_mem")))) {

                    if(const CallExprPtr& newCall = checkAssignment(callExpr)) {
/* omitting further checks
                        if(const LiteralPtr& fun = dynamic_pointer_cast<const Literal>(newCall->getFunctionExpr())){
                            std::cout << "++++++++++++VALUE: " << fun->getValue() << std::endl;
                        if(fun->getValue() == "clCreateBuffer" ) { */
                        TypePtr newType = builder.refType(newCall->getType());
                        // check if variable has already been put into replacement map with a different type
                        if(cl_mems[lhs] != static_cast<long int>(0))
                            assert((cl_mems[lhs]->getType() == newType) && "cl_mem variable allocated several times with different types.");

                        const VariablePtr& newVar = builder.variable(newType);
    //                    cl_mems.insert(std::make_pair(lhs, newVar));
                        cl_mems[lhs] = newVar;

                        return builder.callExpr(BASIC.getRefAssign(), lhs, newCall);
                    }//}}
                }
                if(lhs->getType() == builder.refType(builder.arrayType(builder.genericType("_cl_kernel")))) {
                    if(const CallExprPtr& newCall = checkAssignment(callExpr)) {
                        if(const LiteralPtr& fun = dynamic_pointer_cast<const Literal>(newCall->getFunctionExpr()))
                        if(fun->getValue() == "clCreateKernel" ) {

                            ExpressionPtr kn = newCall->getArgument(1);
                            // usually kernel name is enbeddid in a "string.as.char.pointer" call"
                            if(const CallExprPtr& sacp = dynamic_pointer_cast<const CallExpr>(kn))
                                kn = sacp->getArgument(0);

                            std::cout << "!!!!!!!!!!!!!Kernel: " << kn << std::endl;
                            if(const LiteralPtr& kl = dynamic_pointer_cast<const Literal>(kn))
                                kernelNames[kl->getValue().substr(1, kl->getValue().length()-2)] = lhs;  // delete quotation marks form name

                            return BASIC.getNoOp();
                        }
                    }
                }
            }
/*
            if(const CallExprPtr& rhs = dynamic_pointer_cast<const CallExpr>(callExpr->getArgument(1))) {
                    std::cout << "Assigning literal " << callExpr->getArgument(1) << " to " << callExpr->getArgument(0) << std::endl;
                if(const LiteralPtr literal = dynamic_pointer_cast<const Literal>(rhs->getFunctionExpr())) {
                    std::cout << "Where you wanted to be " <<  callExpr->getArgument(0) << std::endl;
                    if(HandlerPtr replacement = handles[literal->getValue()]) {
                        return replacement->handleNode(callExpr);
                    }
                }
            }*/
        }
    }

    if(const DeclarationStmtPtr& decl = dynamic_pointer_cast<const DeclarationStmt>(element)) {
        const VariablePtr& var = decl->getVariable();
        if(var->getType() == builder.refType(builder.arrayType(builder.genericType("_cl_mem")))) {
            if(const CallExprPtr& initFct = dynamic_pointer_cast<const CallExpr>(decl->getInitialization())) {
                if(const LiteralPtr& literal = core::dynamic_pointer_cast<const core::Literal>(initFct->getFunctionExpr())) {
                    if(literal->getValue() == "clCreateBuffer") { // clCreateBuffer is called at definition of cl_mem variable
                        const CallExprPtr& newInit = dynamic_pointer_cast<const CallExpr>(this->resolveElement(initFct));

                        //DeclarationStmtPtr newDecl = dynamic_pointer_cast<const DeclarationStmt>(decl->substitute(builder.getNodeManager(), *this));
                        TypePtr newType = builder.refType(newInit->getType());

                        const DeclarationStmtPtr newDecl = builder.declarationStmt(var, builder.refNew(newInit));

                        const VariablePtr& newVar = builder.variable(newType);

                        cl_mems[var] = newVar;

                        return newDecl;
                    }
                }
            }
        }
    }

    return element->substitute(builder.getNodeManager(), *this);
}

void Host2ndPass::mapNamesToLambdas(const vector<ExpressionPtr>& kernelEntries)
{
    for_each(kernelEntries, [&](ExpressionPtr entryPoint) {
        if(const LambdaExprPtr& lambdaEx = dynamic_pointer_cast<const LambdaExpr>(entryPoint))
        if(auto cname = lambdaEx->getLambda()->getAnnotation(c_info::CNameAnnotation::KEY)) {
            if(ExpressionPtr clKernel = kernelNames[cname->getName()]) {
                kernelLambdas[clKernel] = lambdaEx;
            }
        }
    });
}

HostMapper3rdPass::HostMapper3rdPass(const core::ASTBuilder build, ClmemTable& clMemTable, KernelArgs& oclKernelArgs, KernelNames& oclKernelNames,
        KernelLambdas& oclKernelLambdas):
    builder(build), cl_mems(clMemTable), kernelArgs(oclKernelArgs), kernelNames(oclKernelNames), kernelLambdas(oclKernelLambdas) {
//    create3Dvec = parse::parseExpression(builder.getNodeManager(), "fun(uint<4>:workDim, anyRef:size) -> vector<uint<4>, 3> {{ \
            decl vector<uint<4>,3> sv = \
             \
            }}");

}


void HostMapper3rdPass::getVarOutOfCrazyInspireConstruct(core::ExpressionPtr& arg){
    // remove stuff added by (void*)&
    if(const CallExprPtr& refToAnyref = dynamic_pointer_cast<const CallExpr>(arg))
        if(refToAnyref->getFunctionExpr() == BASIC.getRefToAnyRef())
            if(const CallExprPtr& scalarToArray = dynamic_pointer_cast<const CallExpr>(refToAnyref->getArgument(0)))
                if(scalarToArray->getFunctionExpr() == BASIC.getScalarToArray())
                    arg = scalarToArray->getArgument(0);

}

/* Assumptions:
 * 1. the work dimension is a scalar in the arguments
 * 2. The cast to void* of the local/global size happens in the argument
 * 3. 1D only
 */

const ExpressionPtr HostMapper3rdPass::anyRefToVec3(const ExpressionPtr& workDim, ExpressionPtr size) {
    CallExprPtr ret;
    const TypePtr vecTy = builder.vectorType(BASIC.getUInt4(), builder.concreteIntTypeParam(static_cast<size_t>(3)));

    // check if there is a scalar to array
    if(const CallExprPtr& scalarToArray = dynamic_pointer_cast<const CallExpr>(size))
        if(scalarToArray->getFunctionExpr() == BASIC.getScalarToArray()) {
            // check consitency with workDim, should be 1
            if(const LiteralPtr& dim = dynamic_pointer_cast<const Literal>(workDim)) {
                std::cout << "found Dim: " << dim->getValue() << std::endl;
                assert(strcmp(dim->getValue().c_str(), "1") == 0 && "Scalar passed to a multi dimensional work group");
            }
            VariablePtr param = builder.variable(scalarToArray->getArgument(0)->getType());

            ExpressionPtr init = param;
            if(dynamic_pointer_cast<const RefType>(param->getType())) {
                init = builder.deref(param);
            }
            if(init->getType() != BASIC.getUInt4()) {
                init = builder.castExpr(BASIC.getUInt4(), init);
            }

            DeclarationStmtPtr vDecl = builder.declarationStmt(vecTy,
                builder.vectorExpr(toVector<ExpressionPtr>(init, builder.literal(BASIC.getUInt4(), "1"), builder.literal(BASIC.getUInt4(), "1"))));


            FunctionTypePtr fctTy = builder.functionType(toVector(scalarToArray->getArgument(0)->getType()), vecTy);
            ret = builder.callExpr(vecTy,
                    builder.lambdaExpr(fctTy, toVector(param) , builder.compoundStmt(vDecl,
                            builder.returnStmt(vDecl->getVariable()))), scalarToArray->getArgument(0));
        }





    return ret;
}


const NodePtr HostMapper3rdPass::resolveElement(const NodePtr& element) {
    // stopp recursion at type level
    if (element->getNodeCategory() == NodeCategory::NC_Type) {
        return element->substitute(builder.getNodeManager(), *this);
    }

    if(const VariablePtr& var = dynamic_pointer_cast<const Variable>(element)) {
        if(cl_mems[var]) {
            return cl_mems[var];
        }
    }

    if(const DeclarationStmtPtr& decl = dynamic_pointer_cast<const DeclarationStmt>(element)) {
        const VariablePtr& var = decl->getVariable();
        if(cl_mems[var]) {
            if(const CallExprPtr& initFct = dynamic_pointer_cast<const CallExpr>(this->resolveElement(decl->getInitialization()))) {

                if(initFct->getArgument(0) == builder.callExpr(BASIC.getUndefined(), BASIC.getTypeLiteral(builder.arrayType(builder.genericType("_cl_mem"))))) {
                    TypePtr newType;
                    if(const RefTypePtr& rt = dynamic_pointer_cast<const RefType>(cl_mems[var]->getType()))
                        newType = rt->getElementType();
                    else
                        newType = cl_mems[var]->getType();
                    return builder.declarationStmt(cl_mems[var], builder.refVar(builder.callExpr(BASIC.getUndefined(), BASIC.getTypeLiteral(newType))));
                }
            }
        }
    }

    if(const CallExprPtr& callExpr = dynamic_pointer_cast<const CallExpr>(element)){
        if(const CallExprPtr& newCall = dynamic_pointer_cast<const CallExpr>(callExpr->substitute(builder.getNodeManager(), *this))) {
            if(const LiteralPtr& fun = dynamic_pointer_cast<const Literal>(newCall->getFunctionExpr())) {
                if(fun->getValue() == "clEnqueueNDRangeKernel" ) {
                    // get kernel function
                    ExpressionPtr k = newCall->getArgument(1);

                    // get corresponding lambda expression
                    LambdaExprPtr lambda = kernelLambdas[k];
                    assert(lambda && "No lambda expression for kernel call found");
                    vector<ExpressionPtr>& args = kernelArgs[k];
                    assert(args.size() > 0 && "No arguments for call to kernel function found");

                    vector<ExpressionPtr> newArgs;
                    for_each(args, [&](ExpressionPtr& arg) {
                        //global and private memory arguments must be variables
                        getVarOutOfCrazyInspireConstruct(arg);

                        newArgs.push_back(dynamic_pointer_cast<const Expression>(this->resolveElement(arg)));
                    });

                    // make a three element vector out of the global and local size
                    const ExpressionPtr global = anyRefToVec3(newCall->getArgument(2), newCall->getArgument(4));
                    const ExpressionPtr local = anyRefToVec3(newCall->getArgument(2), newCall->getArgument(5));

                    // add global and local size to arguments
                    newArgs.push_back(global);
                    newArgs.push_back(local);

                    // construct call to kernel function
                    return builder.callExpr(BASIC.getInt4(), lambda, newArgs);
                }
            }
            return newCall;
        }

    }

    return element->substitute(builder.getNodeManager(), *this);
}

}

ProgramPtr HostCompiler::compile() {
//    HostVisitor oclHostVisitor(builder, mProgram);
    HostMapper oclHostMapper(builder);

    const ProgramPtr& interProg = dynamic_pointer_cast<const core::Program>(oclHostMapper.mapElement(0, mProgram));
    assert(interProg && "First pass of OclHostCompiler corrupted the program");

    LOG(INFO) << "Adding kernels to host Program...";

    const vector<ExpressionPtr>& kernelEntries = oclHostMapper.getKernels();
    const ProgramPtr& progWithKernels = interProg->addEntryPoints(builder.getNodeManager(), interProg, kernelEntries);

    Host2ndPass ohv2nd(oclHostMapper.getKernelNames(), oclHostMapper.getClMemMapping(), builder);
    ohv2nd.mapNamesToLambdas(kernelEntries);

    HostMapper3rdPass ohm3rd(builder, oclHostMapper.getClMemMapping(), oclHostMapper.getKernelArgs(), ohv2nd.getKernelNames(), ohv2nd.getKernelLambdas());
    if(core::ProgramPtr newProg = dynamic_pointer_cast<const core::Program>(ohm3rd.mapElement(0, progWithKernels))) {
        mProgram = newProg;
        return newProg;
    }
    else
        assert(newProg && "Second pass of OclHostCompiler corrupted the program");
    return mProgram;
}

} //namespace ocl
} //namespace frontend
} //namespace insieme
