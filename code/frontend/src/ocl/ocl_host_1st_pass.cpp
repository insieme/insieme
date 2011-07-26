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

#include "insieme/core/ast_node.h"
#include "insieme/frontend/ocl/ocl_host_utils.h"
#include "insieme/frontend/ocl/ocl_host_passes.h"
#include "insieme/frontend/ocl/ocl_annotations.h"
#include "insieme/core/transform/node_replacer.h"

#include <fstream>

namespace ba = boost::algorithm;
namespace iocl = insieme::ocl;

namespace insieme {
namespace frontend {
namespace ocl {
using namespace insieme::core;

#define POINTER(type) builder.refType(builder.refType(builder.arrayType(type)))

const ProgramPtr& loadKernelsFromFile(string path, const ASTBuilder& builder) {
	// delete quotation marks form path
	if (path[0] == '"')
		path = path.substr(1, path.length() - 2);

	std::ifstream check;
	string root = path;
	size_t nIncludes = CommandLineOptions::IncludePaths.size();
	// try relative path first
	check.open(path);
	// if not found now, check the include directories
	for (size_t i = 0; i < nIncludes && check.fail(); ++i) {
		check.close();
		// try with include paths
		path = CommandLineOptions::IncludePaths.at(i) + "/" + root;
		check.open(path);
	}
	// if there is still no match, try the paths of the input files
	size_t nInputFiles = CommandLineOptions::InputFiles.size();
	for (size_t i = 0; i < nInputFiles && check.fail(); ++i) {
		// try the paths of the input files
		string ifp = CommandLineOptions::InputFiles.at(i);
		size_t slash = ifp.find_last_of("/");
		path = ifp.substr(0u, slash + 1) + root;
		check.open(path);
	}

	check.close();

	if (check.fail()) {// no kernel file found, leave the error printing to the compiler frontend
	//		std::cerr << "FAIL! " << path << std::endl;
		path = root;
	}

	LOG(INFO) << "Converting kernel file '" << path << "' to IR...";

	frontend::Program fkernels(builder.getNodeManager());
	fkernels.addTranslationUnit(path);
	return fkernels.convert();
}

void tryStructExtract(ExpressionPtr& expr, ASTBuilder& builder) {
	if (const CallExprPtr& cre = dynamic_pointer_cast<const CallExpr>(expr)) {
		if (cre->getFunctionExpr() == BASIC.getCompositeRefElem()) {
			expr = cre->getArgument(0);
		}
	}
}

bool isNullPtr(ExpressionPtr& expr, ASTBuilder& builder) {
	if (const CallExprPtr rta = dynamic_pointer_cast<const CallExpr>(expr))
		if (rta->getFunctionExpr() == BASIC.getRefToAnyRef())
			if (const CallExprPtr getNull = dynamic_pointer_cast<const CallExpr>(rta->getArgument(0)))
				if (getNull->getFunctionExpr() == BASIC.getGetNull())
					return true;

	return false;
}

bool KernelCodeRetriver::visitNode(const core::NodePtr& node) {
	if (node == breakingStmt) {
		return false; // stop recursion
	}
	return true; // go on with search
}

bool KernelCodeRetriver::visitCallExpr(const core::CallExprPtr& callExpr) {
	if (callExpr->getFunctionExpr() != BASIC.getRefAssign())
		return true;
	// check if it is the variable we are looking for
	if (const VariablePtr& lhs = dynamic_pointer_cast<const Variable>(callExpr->getArgument(0))) {
		if (lhs->getId() != pathToKernelFile->getId())
			return true;
	} else {
		return true;
	}
	if (const CallExprPtr& rhs = dynamic_pointer_cast<const CallExpr>(callExpr->getArgument(0))) {
		if (const CallExprPtr& callSaC = dynamic_pointer_cast<const CallExpr>(rhs->getArgument(0))) {
			if (const LiteralPtr& stringAsChar = dynamic_pointer_cast<const Literal>(callSaC->getFunctionExpr())) {
				if (stringAsChar->getValue() == "string.as.char.pointer") {
					if (const LiteralPtr& pl = dynamic_pointer_cast<const Literal>(callSaC->getArgument(0))) {
						path = pl->getValue();
						return false;
					}
				}
			}
		}
	}
	return true;
}

bool KernelCodeRetriver::visitDeclarationStmt(const core::DeclarationStmtPtr& decl) {
	if (decl->getVariable()->getId() != pathToKernelFile->getId())
		return true;

	if (const CallExprPtr& callSaC = dynamic_pointer_cast<const CallExpr>(decl->getInitialization())) {
		if (const LiteralPtr& stringAsChar = dynamic_pointer_cast<const Literal>(callSaC->getFunctionExpr())) {
			if (stringAsChar->getValue() == "string.as.char.pointer") {
				if (const LiteralPtr& pl = dynamic_pointer_cast<const Literal>(callSaC->getArgument(0))) {
					path = pl->getValue();
					return false;
				}
			}
		}
	}
	return true;
}
bool Ocl2Inspire::extractSizeFromSizeof(const core::ExpressionPtr& arg, core::ExpressionPtr& size, core::TypePtr& type) {
	// get rid of casts
	NodePtr uncasted = arg;
	while (uncasted->getNodeType() == core::NT_CastExpr) {
		uncasted = uncasted->getChildList().at(1);
	}

	while (const CallExprPtr& mul = dynamic_pointer_cast<const CallExpr> (uncasted)) {
		if(mul->getArguments().size() == 2)
			for (int i = 0; i < 2; ++i) {
				// get rid of casts
				core::NodePtr arg = mul->getArgument(i);
				while (arg->getNodeType() == core::NT_CastExpr) {
					arg = arg->getChildList().at(1);
				}
				if (const CallExprPtr& sizeof_ = dynamic_pointer_cast<const CallExpr>(arg)) {
					if (sizeof_->toString().substr(0, 6).find("sizeof") != string::npos) {
						// extract the type to be allocated
						type = dynamic_pointer_cast<const Type> (sizeof_->getArgument(0)->getType()->getChildList().at(0));
						// extract the number of elements to be allocated
						size = mul->getArgument(1 - i);
						return true;
					}
				}
			}
			uncasted = mul->getArgument(0);
	}
	return false;
}

ExpressionPtr Ocl2Inspire::getClCreateBuffer(bool copyHostPtr) {
	// read/write flags ignored
	// errcorcode always set to 0 = CL_SUCCESS
	if (copyHostPtr)
		return parser.parseExpression("fun(type<'a>:elemType, uint<8>:flags, uint<8>:size, anyRef:hostPtr, ref<array<int<4>, 1> >:errorcode_ret) -> \
					ref<array<'a, 1> >  {{ \
	            decl ref<array<'a, 1> >:devicePtr = (op<ref.new>( (op<array.create.1D>( elemType, size )) )); \
				decl ref<array<'a, 1> >:hp = (op<anyref.to.ref>(hostPtr, lit<type<array<'a, 1> >, type(array('a ,1)) > )); \
				for(decl uint<8>:i = lit<uint<8>, 0> .. size : 1) \
					( (op<array.ref.elem.1D>(devicePtr, i )) = (op<ref.deref>( (op<array.ref.elem.1D>(hp, i )) )) ); \
				 \
	            ( (op<array.ref.elem.1D>(errorcode_ret, lit<uint<8>, 0> )) = 0 ); \
				return devicePtr; \
	       }}");

	return parser.parseExpression("fun(type<'a>:elemType, uint<8>:flags, uint<8>:size, ref<array<int<4>, 1> >:errorcode_ret) -> ref<array<'a, 1> > {{ \
            ( (op<array.ref.elem.1D>(errorcode_ret, lit<uint<8>, 0> )) = 0 ); \
            return (op<ref.new>( (op<array.create.1D>( elemType, size )) )); \
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
            decl ref<array<'a, 1> >:hp = (op<anyref.to.ref>(hostPtr, lit<type<array<'a, 1> >, type(array('a ,1)) > )); \
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
            return 0; \
    }}");
	/*
	 decl ref<array<'a, 1> >:hp = (op<anyref.to.ref>(hostPtr, lit<type<array<'a, 1> >, type<array<'a ,1 > )); \
            for(decl uint<8>:i = lit<uint<8>, 0> .. cb : 1) \
                ( (op<array.ref.elem.1D>(hp, (i + offset) )) = (op<ref.deref>( (op<array.ref.elem.1D>(devicePtr, i )) )) );
	 */
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

HostMapper::HostMapper(ASTBuilder& build, ProgramPtr& program) :
	builder(build), o2i(build.getNodeManager()), mProgram(program), kernelArgs( // specify constructor arguments to pass the builder to the compare class
		boost::unordered_map<core::ExpressionPtr, std::vector<core::ExpressionPtr>, hash_target<core::ExpressionPtr>, equal_variables>::size_type(),
		hash_target_specialized(build),	equal_variables(build)) {
	ADD_Handler(builder, "clCreateBuffer",
			std::set<enum CreateBufferFlags> flags = this->getFlags<enum CreateBufferFlags>(node->getArgument(1));

			// check if CL_MEM_USE_HOST_PTR is set
			bool usePtr = flags.find(CreateBufferFlags::CL_MEM_USE_HOST_PTR) != flags.end();
			// check if CL_MEM_COPY_HOST_PTR is set
			bool copyPtr = flags.find(CreateBufferFlags::CL_MEM_COPY_HOST_PTR) != flags.end();

			// extract the size form argument size, relying on it using a multiple of sizeof(type)
			ExpressionPtr size;
			TypePtr type;
			ExpressionPtr hostPtr;

			hostPtr = node->getArgument(3);
			if(CastExprPtr c = dynamic_pointer_cast<const CastExpr>(node->getArgument(3))) {
				assert(!copyPtr && "When CL_MEM_COPY_HOST_PTR is set, host_ptr parameter must be a valid pointer");
				if(c->getSubExpression()->getType() != BASIC.getAnyRef()) {// a scalar (probably NULL) has been passed as hostPtr arg
					hostPtr = builder.callExpr(BASIC.getRefToAnyRef(), builder.callExpr(BASIC.getRefVar(), c->getSubExpression()));
				}
			}
			if(usePtr) { // in this case we can just use the host_ptr instead of the cl_mem variable
				return hostPtr;
			}

			ExpressionPtr fun = o2i.getClCreateBuffer(copyPtr);

			bool sizeFound = o2i.extractSizeFromSizeof(node->getArgument(2), size, type);
			assert(sizeFound && "Unable to deduce type from clCreateBuffer call: No sizeof call found, cannot translate to INSPIRE.");

			vector<ExpressionPtr> args;
			args.push_back(BASIC.getTypeLiteral(type));
			args.push_back(node->getArgument(1));
			args.push_back(size);
			if(copyPtr) args.push_back(hostPtr);
			args.push_back(node->getArgument(4));
			ExpressionPtr retVal = builder.callExpr(builder.refType(builder.arrayType(type)), fun, args);
			return retVal;
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

	ADD_Handler(builder, "clSetKernelArg",
			// arg_index must either be an integer literal or all arguments have to be specified in the right order in the source code
			ExpressionPtr kernel = tryRemove(BASIC.getRefDeref(), node->getArgument(0), builder);
			// check if kernel argument is in a struct, if yes, use the struct-variable
//			tryStructExtract(kernel, builder);

			ExpressionPtr arg = node->getArgument(3);

			if(isNullPtr(arg, builder)) {
				// in this case arg is a local variable which has to be declared in host code
				// need to read size parameter
				ExpressionPtr size;
				TypePtr type;
				ExpressionPtr hostPtr;

				bool sizeFound = o2i.extractSizeFromSizeof(node->getArgument(2), size, type);
				assert(sizeFound && "Unable to deduce type from clSetKernelArg call when allocating local memory: No sizeof call found, cannot translate to INSPIRE.");

				// declare a new variable to be used as argument
				VariablePtr localMem = builder.variable(builder.refType(builder.arrayType(type)));
				DeclarationStmtPtr localDecl = builder.declarationStmt(localMem, builder.callExpr(BASIC.getRefVar(),
								builder.callExpr(BASIC.getArrayCreate1D(), BASIC.getTypeLiteral(type), size)));
				// should I really have access to private members or HostMapper here or is this a compiler bug?
				localMemDecls[kernel].push_back(localDecl);
				arg = localMem;
			}
//			arg = tryRemove(BASIC.getRefToAnyRef(), arg, builder);

			// if the argument is a scalar, we have to deref it
			// TODO test propperly
/*			if(const CallExprPtr& sizeof_ = dynamic_pointer_cast<const CallExpr>(node->getArgument(2))) {
				if(sizeof_->getFunctionExpr() == BASIC.getSizeof()) {
					if(sizeof_->getArgument(0)->getType() != BASIC.getTypeLiteral(builder.arrayType(builder.genericType("_cl_mem")))->getType()) {
						arg = tryDeref(getVarOutOfCrazyInspireConstruct(arg, builder), builder);
					}
				}
			}*/

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
			//			NodePtr ret = node;
			if(const CallExprPtr& callSaC = dynamic_pointer_cast<const CallExpr>(node->getArgument(0))) {
				if(const LiteralPtr& stringAsChar = dynamic_pointer_cast<const Literal>(callSaC->getFunctionExpr())) {
					if(stringAsChar->getValue() == "string.as.char.pointer") {
						if(const LiteralPtr& path = dynamic_pointer_cast<const Literal>(callSaC->getArgument(0))) {
							kernels = loadKernelsFromFile(path->getValue(), builder);

							// set source string to an empty char array
							//							ret = builder.refVar(builder.literal("", builder.arrayType(BASIC.getChar())));
						}
					}
				}
			}
			if(const VariablePtr& pathVar = dynamic_pointer_cast<const Variable>(node->getArgument(0))) {
				//            std::cout << "PathVariable: " << pathVar << std::endl;
				KernelCodeRetriver kcr(pathVar, node, builder);
				visitAll(mProgram, kcr);
				string kernelFilePath = kcr.getKernelFilePath();
				if(kernelFilePath.size() > 0)
				kernels = loadKernelsFromFile(kernelFilePath, builder);
			}
			// set source string to an empty char array
			return builder.refVar(builder.literal("", builder.arrayType(BASIC.getChar())));
	);

	ADD_Handler(builder, "clEnqueueNDRangeKernel",
			// get argument vector
			ExpressionPtr k = tryRemove(BASIC.getRefDeref(), node->getArgument(1), builder);
//			tryStructExtract(k, builder);
			assert(kernelArgs.find(k) != kernelArgs.end() && "Cannot find any arguments for kernel function");

			std::vector<core::ExpressionPtr> args = kernelArgs[k];
			// adding global and local size to the argument vector
			args.push_back(node->getArgument(4) );
			args.push_back(node->getArgument(5) );

			return node;
	);

	ADD_Handler(builder, "clBuildProgram",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO add flags for profiling and out of order
	ADD_Handler(builder, "clGetEventProfilingInfo",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO add syncronization means when adding asynchronous queue
	ADD_Handler(builder, "clFinish",
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(builder, "clWaitForEvents",
			// return cl_success
			return builder.intLit(0);
	);

	// need to release clMem objects
	ADD_Handler(builder, "clReleaseMemObject",
			return builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), node->getArgument(0));
			// updataint of the type to update the deref operation in the argument done in thrid pass
	);

	// all other clRelease calls can be ignored since the variables are removed
	ADD_Handler(builder, "clRelease",
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(builder, "clRetain",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO implement, may have some semantic meaning
	ADD_Handler(builder, "clGetEventInfo",
			LOG(WARNING) << "Removing clGetEventInfo. Check the semantics!";
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(builder, "clFlush",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO maybe a bit too optimisitc?
	ADD_Handler(builder, "clGet",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO need to add exception this when adding image support
	ADD_Handler(builder, "clCreate",
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(builder, "clUnloadCompiler",
			// return cl_success
			return builder.intLit(0);
	);

	// DEPRECATED, but used in the NVIDIA examples
	ADD_Handler(builder, "clSetCommandQueueProperty",
			// return cl_success
			return builder.intLit(0);
	);

	// exceptions, will be handled in a later step
	ADD_Handler(builder, "clCreateContext", return node;);
	ADD_Handler(builder, "clCreateCommandQueue", return node;);
	ADD_Handler(builder, "clCreateKernel", return node;);
};

HandlerPtr& HostMapper::findHandler(const string& fctName) {
	// for performance reasons working with function prefixes is only enabled for cl* functions
	if (fctName.substr(0, 2).find("cl") == string::npos)
		return handles[fctName];

	// checking function names, starting from the full name
	for (int i = fctName.size(); i > 2; --i) {
		if (HandlerPtr& h = handles[fctName.substr(0,i)])
			return h;
	}
	return handles["break"]; // function is not in map
}

CallExprPtr HostMapper::checkAssignment(const core::NodePtr& oldCall) {
	CallExprPtr newCall;
	if ((newCall = dynamic_pointer_cast<const CallExpr> (oldCall))) {
		// get rid of deref operations, automatically inserted by the frontend coz _cl_mem* is translated to ref<array<...>>, and refs cannot be
		// rhs of an assignment
		if (const CallExprPtr& rhs = dynamic_pointer_cast<const CallExpr>(newCall->getArgument(1))) {
			if (rhs->getFunctionExpr() == BASIC.getRefDeref()) {
				newCall = dynamic_pointer_cast<const CallExpr> (rhs->getArgument(0));
			} else
				newCall = rhs;
		}
	}
	return newCall;
}

template<typename Enum>
void HostMapper::recursiveFlagCheck(const ExpressionPtr& flagExpr, std::set<Enum>& flags) {
	if (const CallExprPtr& call = dynamic_pointer_cast<const CallExpr>(flagExpr)) {
		// check if there is an lshift -> flag reached
		if (call->getFunctionExpr() == BASIC.getSignedIntLShift() || call->getFunctionExpr() == BASIC.getUnsignedIntLShift()) {
			if (const LiteralPtr& flagLit = dynamic_pointer_cast<const Literal>(call->getArgument(1))) {
				int flag = atoi(flagLit->getValue().c_str());
				if (flag < Enum::size) // last field of enum to be used must be size
					flags.insert(Enum(flag));
				else LOG(ERROR)
					<< "Flag " << flag << " is out of range. Max value is " << CreateBufferFlags::size - 1;
			}
		} else if (call->getFunctionExpr() == BASIC.getSignedIntOr() || call->getFunctionExpr() == BASIC.getUnsignedIntOr()) {
			// two flags are ored, search flags in the arguments
			recursiveFlagCheck(call->getArgument(0), flags);
			recursiveFlagCheck(call->getArgument(1), flags);
		} else LOG(ERROR)
			<< "Unexpected operation in flag argument: " << call << "\nUnable to deduce flags, using default settings";

	}
}

template<typename Enum>
std::set<Enum> HostMapper::getFlags(const ExpressionPtr& flagExpr) {
	std::set<Enum> flags;
	// remove cast to uint<8>
	if (const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(flagExpr)) {
		recursiveFlagCheck(cast->getSubExpression(), flags);
	} else LOG(ERROR)
		<< "No flags found in " << flagExpr << "\nUsing default settings";

	return flags;
}

bool HostMapper::handleClCreateKernel(const core::ExpressionPtr& expr, const ExpressionPtr& call, const ExpressionPtr& fieldName) {
	TypePtr type = getNonRefType(expr);
	// if it is a struct we have to check the field
	if (const StructTypePtr st = dynamic_pointer_cast<const StructType>(type)) {
		//TODO if one puts more than one kernel inside a struct (s)he should be hit in the face
		if (fieldName) {
			if (const LiteralPtr& fieldLit = dynamic_pointer_cast<const Literal>(fieldName)) {
				for_each(st->getEntries(), [&](StructType::Entry field) {
					if(field.first->getName() == fieldLit->getValue())
					type = field.second;
				});
			} else
				assert(fieldLit && "If the clKernel variable is inside a struct, the fieldName of it has to be passed to handleClCreateKernel");
		}
	}

	// if it is an array we have to pass the entire subscript call
	if( const VectorTypePtr vt = dynamic_pointer_cast<const VectorType>(type)) {
		type = vt->getElementType();
	}
	if( const ArrayTypePtr at = dynamic_pointer_cast<const ArrayType>(type)) {
		// since all cl_* types are pointers, an array would have two nested arrays
		if(at->getElementType()->getNodeType() == NT_ArrayType) {
			type = at->getElementType();
		}
	}

	if(getNonRefType(type) == builder.arrayType(builder.genericType("_cl_kernel"))) {
		if(const CallExprPtr& newCall = dynamic_pointer_cast<const CallExpr>(call)) {//!
			if(const LiteralPtr& fun = dynamic_pointer_cast<const Literal>(newCall->getFunctionExpr()))
			if(fun->getValue() == "clCreateKernel" ) {
				ExpressionPtr kn = newCall->getArgument(1);
				// usually kernel name is embedded in a "string.as.char.pointer" call"
				if(const CallExprPtr& sacp = dynamic_pointer_cast<const CallExpr>(kn))
				kn = sacp->getArgument(0);
				if(const LiteralPtr& kl = dynamic_pointer_cast<const Literal>(kn)) {
					string name = kl->getValue().substr(1, kl->getValue().length()-2); // delete quotation marks form name
					kernelNames[name] = expr;
				}

				return true;
			}
		}
	}
	return false;
}

bool HostMapper::lookForKernelFilePragma(const core::TypePtr& type, const core::ExpressionPtr& createProgramWithSource) {
if(type == POINTER(builder.genericType("_cl_program"))) {
	if(CallExprPtr cpwsCall = dynamic_pointer_cast<const CallExpr>(createProgramWithSource)) {
		if(iocl::KernelFileAnnotationPtr kfa =
				dynamic_pointer_cast<iocl::KernelFileAnnotation>(cpwsCall->getAnnotation(iocl::KernelFileAnnotation::KEY))) {
			string path = kfa->getKernelPath();
			//std::cerr << "Found OpenCL kernel file path: " << path;
			if(cpwsCall->getFunctionExpr() == BASIC.getRefDeref() && cpwsCall->getArgument(0)->getNodeType() == NT_CallExpr)
			cpwsCall = dynamic_pointer_cast<const CallExpr>(cpwsCall->getArgument(0));
			if(const LiteralPtr& clCPWS = dynamic_pointer_cast<const Literal>(cpwsCall->getFunctionExpr())) {
				if(clCPWS->getValue() == "clCreateProgramWithSource") {
					ProgramPtr kernels = loadKernelsFromFile(path, builder);
					for_each(kernels->getEntryPoints(), [&](ExpressionPtr kernel) {
								kernelEntries.push_back(kernel);
							});
				}
			}
		}
	}
	return true;
}
return false;
}

const NodePtr HostMapper::resolveElement(const NodePtr& element) {
	// stopp recursion at type level
	if (element->getNodeCategory() == NodeCategory::NC_Type) {
		return element;
	}

	/*    if(const MarkerExprPtr& marker = dynamic_pointer_cast<const MarkerExpr>(element)){
	 std::cout << "MarkerExpr: " << marker << std::endl;
	 }

	 if(const MarkerStmtPtr& marker = dynamic_pointer_cast<const MarkerStmt>(element)) {
	 std::cerr << "MarkerStmt: " << marker << std::endl;
	 }*/

	if(const CallExprPtr& callExpr = dynamic_pointer_cast<const CallExpr>(element)) {
		const ExpressionPtr& fun = callExpr->getFunctionExpr();

		if(const LiteralPtr& literal = dynamic_pointer_cast<const Literal>(fun)) {
			callExpr->substitute(builder.getNodeManager(), *this);
			if(const HandlerPtr& replacement = findHandler(literal->getValue())) {
				NodePtr ret = replacement->handleNode(callExpr);
				// check if new kernels have been created
				vector<ExpressionPtr> kernels = replacement->getKernels();
				if(kernels.size() > 0)
				for_each(kernels, [&](ExpressionPtr kernel) {
							kernelEntries.push_back(kernel);
						});

				copyAnnotations(callExpr, ret);
				return ret;
			}
		}

		if(fun == BASIC.getRefAssign()) {
			ExpressionPtr lhs = callExpr->getArgument(0);
			// on the left hand side we'll either have a variable or a struct, probably holding a reference to the global array
			// for the latter case we have to handle it differently
			if(const CallExprPtr& lhsCall = dynamic_pointer_cast<const CallExpr>(lhs)) {
				if(lhsCall->getFunctionExpr() == BASIC.getCompositeRefElem()) {
					if(const CallExprPtr& newCall = checkAssignment(callExpr->substitute(builder.getNodeManager(), *this))) {
						if(lhsCall->getType() == POINTER(builder.genericType("_cl_mem"))) {
							const TypePtr& newType = newCall->getType();

							const VariablePtr& struct_ = dynamic_pointer_cast<const Variable>(lhsCall->getArgument(0));
							assert(struct_ && "First argument of compostite.ref.elem has unexpected type, should be a struct variable");
							VariablePtr newStruct;
							// check if struct is already part of the replacement map
							if(cl_mems.find(struct_) != cl_mems.end()) {
								// get the variable out of the struct
								newStruct = cl_mems[struct_];
							} else
								newStruct = struct_;

							LiteralPtr id = dynamic_pointer_cast<const Literal>(lhsCall->getArgument(1));
							assert(id && "Second argument of composite.ref.elem has unexpected type, should be a literal");
							IdentifierPtr toChange = builder.identifier(id->getValue());
							StructTypePtr structType = dynamic_pointer_cast<const StructType>(getNonRefType(newStruct));
							StructType::Entries entries = structType->getEntries(); // actual fields of the struct
							StructType::Entries newEntries;

							// check if CL_MEM_USE_HOST_PTR flag was set and host_ptr was a global variable
							bool useHostPtr = false;
							if(const CallExprPtr& creToHostField = dynamic_pointer_cast<const CallExpr>(tryRemove(BASIC.getRefDeref(),
									newCall->getArgument(0), builder))) {
								if(creToHostField->getFunctionExpr() == BASIC.getCompositeRefElem()) {
									// in this case replace the identifier of the cl_mem object with the one of the host_ptr
									useHostPtr = true;
									replacements[lhsCall->getArgument(1)] = creToHostField->getArgument(1);
								} else {
									assert(false && "If a cl_mem variable at global scope/in a struct uses the CL_MEM_USE_HOST_PTR flag at create buffer, the \
											host_ptr must be in the global scope/the same struct too");}
							}

							for_each(entries, [&](std::pair<IdentifierPtr, TypePtr> entry) {
									if(entry.first == toChange) {
										if(!useHostPtr)
											newEntries.push_back(std::make_pair(entry.first, newType));
									} else {
										newEntries.push_back(entry);
									}
								});

							// update struct in replacement map
							NodePtr&& replacement = builder.variable(builder.refType(builder.structType(newEntries)));

							copyAnnotations(struct_, replacement);
							cl_mems[struct_] = dynamic_pointer_cast<const Variable>(replacement);

							if(useHostPtr)
								return BASIC.getNoOp();

							const CallExprPtr& structAccess = builder.callExpr(BASIC.getCompositeRefElem(), struct_,
									lhsCall->getArgument(1), BASIC.getTypeLiteral(newType));
							return builder.callExpr(BASIC.getRefAssign(), structAccess, newCall);
						}

						if(lhsCall->getType() == POINTER(builder.genericType("_cl_kernel"))) {
							if(const CallExprPtr& cre = dynamic_pointer_cast<const CallExpr>(callExpr->getArgument(0)))
								if(cre->getFunctionExpr() == BASIC.getCompositeRefElem())
									if(dynamic_pointer_cast<const Variable>(cre->getArgument(0))){
										if(handleClCreateKernel(cre, newCall, cre->getArgument(1)))
											return BASIC.getNoOp();
									}
						}
					}

					if(lookForKernelFilePragma(lhsCall->getType(), callExpr->getArgument(1))) {
						return BASIC.getNoOp();
					}
				}
				// also if the variable is an array we will have a function call on the left hand side
				if(lhsCall->getFunctionExpr() == BASIC.getArrayRefElem1D() || lhsCall->getFunctionExpr() ==  BASIC.getVectorRefElem()) {
					//prepare stuff for VariablePtr section
					lhs = dynamic_pointer_cast<const Variable>(lhsCall->getArgument(0));
				}
			}

			if(const VariablePtr& lhsVar = dynamic_pointer_cast<const Variable>(lhs)) {
				// handling clCreateBuffer
				if(lhsVar->getType()->toString().find("array<_cl_mem,1>") != string::npos) {// == builder.refType(builder.arrayType(builder.genericType("_cl_mem")))) {
					NodePtr createBuffer = callExpr->substitute(builder.getNodeManager(), *this);
					bool alreadyThere = cl_mems.find(lhsVar) != cl_mems.end();
					// check if data has to be copied to a new array
					if(const CallExprPtr& newCall = checkAssignment(createBuffer)) {
						// exchange the _cl_mem type with the new type, gathered from the clCreateBuffer call
						TypePtr newType = static_pointer_cast<const Type>(core::transform::replaceAll(builder.getNodeManager(), lhs->getType(),
								callExpr->getArgument(1)->getType(), newCall->getType()));
						// check if variable has already been put into replacement map with a different type
						if(alreadyThere)
							assert((cl_mems[lhsVar]->getType() == newType) && "cl_mem variable allocated several times with different types.");
						NodePtr ret;

						if(const VariablePtr& var = dynamic_pointer_cast<const Variable>(getVarOutOfCrazyInspireConstruct(newCall, builder))) {
							// use the host variable because CL_MEM_USE_HOST_PTR was set
							cl_mems[lhsVar] = var;
							// TODO check if err argument has been passed and set variable to 0
							ret = BASIC.getNoOp();
						} else {
							if(!alreadyThere) {
								const VariablePtr& newVar = builder.variable(newType);
								cl_mems[lhsVar] = newVar;
							}
							// replace the variable and the type in the lhs of the assignmend
							ExpressionPtr newLhs = static_pointer_cast<const Expression>(core::transform::replaceAll(builder.getNodeManager(),
									callExpr->getArgument(0), lhsVar, cl_mems[lhsVar]));
							newLhs = static_pointer_cast<const Expression>(core::transform::replaceAll(builder.getNodeManager(), newLhs,
									callExpr->getArgument(1)->getType(), newCall->getType()));
							ret = builder.callExpr(BASIC.getUnit(), BASIC.getRefAssign(), newLhs, newCall);
						}

						copyAnnotations(callExpr, ret);
						return ret;
					}
					// check if we can simply use the existing array
					if(const VariablePtr clMemReplacement = dynamic_pointer_cast<const Variable>(createBuffer)) {
						TypePtr newType = clMemReplacement->getType();

						if(alreadyThere)
							assert((cl_mems[lhsVar]->getType() == newType) && "cl_mem variable allocated several times with different types.");

						cl_mems[lhsVar] = clMemReplacement;
						return BASIC.getNoOp();
					}
				}

				if(const CallExprPtr& newCall = checkAssignment(callExpr->substitute(builder.getNodeManager(), *this))) {
					if(handleClCreateKernel(callExpr->getArgument(0), newCall, NULL)) {
						return BASIC.getNoOp();
					}
				}

				if(lookForKernelFilePragma(lhsVar->getType(), callExpr->getArgument(1))) {
					return BASIC.getNoOp();
				}
			}

		}
	}

	if(const DeclarationStmtPtr& decl = dynamic_pointer_cast<const DeclarationStmt>(element)) {
		const VariablePtr& var = decl->getVariable();
		if(var->getType() == POINTER(builder.genericType("_cl_mem"))) {
			if(const CallExprPtr& initFct = dynamic_pointer_cast<const CallExpr>(tryRemove(BASIC.getRefVar(), decl->getInitialization(), builder))) {
				if(const LiteralPtr& literal = core::dynamic_pointer_cast<const core::Literal>(initFct->getFunctionExpr())) {
					if(literal->getValue() == "clCreateBuffer") { // clCreateBuffer is called at definition of cl_mem variable
						const CallExprPtr& newInit = dynamic_pointer_cast<const CallExpr>(this->resolveElement(initFct));

						// check if the variable was created with CL_MEM_USE_HOST_PTR flag and can be removed
						if(const VariablePtr replacement = dynamic_pointer_cast<const Variable>(getVarOutOfCrazyInspireConstruct(newInit, builder))) {
							cl_mems[var] = replacement; // TODO check if error argument has been set and set error to CL_SUCCESS
							return BASIC.getNoOp();
						}

						//DeclarationStmtPtr newDecl = dynamic_pointer_cast<const DeclarationStmt>(decl->substitute(builder.getNodeManager(), *this));
						TypePtr newType = builder.refType(newInit->getType());

						NodePtr newDecl = builder.declarationStmt(var, builder.refVar(newInit));
						const VariablePtr& newVar = builder.variable(newType);
						cl_mems[var] = newVar;

						copyAnnotations(decl, newDecl);
						return newDecl;
					}
				}
			}
			assert(decl->getInitialization()->getNodeType() == NT_CallExpr && "Unexpected initialization of cl_mem variable");
		}

		lookForKernelFilePragma(var->getType(), decl->getInitialization());

		if(handleClCreateKernel(var, decl->getInitialization(), NULL)) {
			return BASIC.getNoOp();
		}

	}

	NodePtr ret = element->substitute(builder.getNodeManager(), *this);
	copyAnnotations(element, ret);
	return ret;
}

} //namespace ocl
} //namespace frontend
} //namespace insieme
