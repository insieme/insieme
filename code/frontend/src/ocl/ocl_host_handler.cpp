/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/ir_node.h"
#include "insieme/frontend/ocl/ocl_host_utils.h"
#include "insieme/frontend/ocl/ocl_host_handler.h"
#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/types/cast_tool.h"

#include <fstream>

namespace ba = boost::algorithm;

namespace insieme {
namespace frontend {
namespace ocl {

#define ALWAYS_FALLBACK 1

using namespace insieme::core;

#define POINTER(type) builder.refType(builder.refType(builder.arrayType(type)))

const ProgramPtr loadKernelsFromFile(string path, const IRBuilder& builder, const ConversionJob& job) {
	// delete quotation marks form path
	if (path[0] == '"')
		path = path.substr(1, path.length() - 2);

	std::ifstream check;
			string root = path;
	size_t nIncludes = job.getIncludeDirectories().size();
	// try relative path first
	check.open(path);
	// if not found now, check the include directories
	for (size_t i = 0; i < nIncludes && check.fail(); ++i) {
		check.close();
		// try with include paths
		path = (job.getIncludeDirectories().at(i) / root).string();
		check.open(path);
	}
	// if there is still no match, try the paths of the input files
	if (check.fail()) {
		assert_eq(job.getFiles().size(), 1u);
		string ifp = job.getFiles()[0].string();
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
	ConversionJob kernelJob = job;
	kernelJob.setFiles(toVector<frontend::path>(path));
	return kernelJob.execute(builder.getNodeManager(), false);
}

void tryStructExtract(ExpressionPtr& expr, IRBuilder& builder) {
	if (const CallExprPtr cre = dynamic_pointer_cast<const CallExpr>(expr)) {
		if (cre->getFunctionExpr() == BASIC.getCompositeRefElem()) {
			expr = cre->getArgument(0);
		}
	}
}

bool KernelCodeRetriver::saveString(const core::LiteralPtr& lit) {
	path = lit->getStringValue();
	return true;
}

bool KernelCodeRetriver::saveString(const core::CallExprPtr& call) {
	if (const LiteralPtr lit = dynamic_pointer_cast<const Literal>(call->getFunctionExpr())) {
		core::NodeManager& mgr = call.getNodeManager();
		core::IRBuilder builder(mgr);

		if (BASIC.isRefVectorToRefArray(lit)) {
			if (const LiteralPtr pl = dynamic_pointer_cast<const Literal>(tryRemove(BASIC.getRefDeref(), call->getArgument(0), builder))) {
				path = pl->getStringValue();
				return true;
			}
		}

		if(lit->getStringValue() == "shrFindFilePath") {

			const ExpressionPtr arg = tryRemove(BASIC.getRefDeref(), call->getArgument(0), builder);
			if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(arg))
				return saveString(call);
			if(const LiteralPtr lit = dynamic_pointer_cast<const Literal>(arg))
				return saveString(lit);
			if(const VariablePtr var = dynamic_pointer_cast<const Variable>(arg)){
				KernelCodeRetriver nkcr(var, call, program, builder);
				visitDepthFirstInterruptible(program, nkcr);
				string pathFound = nkcr.getKernelFilePath();
				if(pathFound.size() > 0) {
					path = pathFound;
					return true;
				}
			}
		}
	}
	return false;
}

bool KernelCodeRetriver::visitNode(const core::NodePtr& node) {
	if (node == breakingStmt) {
		return true; // stop recursion
	}
	return false; // go on with search
}

bool KernelCodeRetriver::visitCallExpr(const core::CallExprPtr& callExpr) {
	core::NodeManager& mgr = callExpr.getNodeManager();
	core::IRBuilder builder(mgr);

	if (callExpr->getFunctionExpr() != BASIC.getRefAssign())
		return false;
	// check if it is the variable we are looking for
	if (const VariablePtr pathVar = dynamic_pointer_cast<const Variable>(pathToKernelFile)) {
		if (const VariablePtr lhs = dynamic_pointer_cast<const Variable>(callExpr->getArgument(0))) {
			if (lhs->getId() != pathVar->getId())
				return false;
		}

		if (const CallExprPtr rhs = dynamic_pointer_cast<const CallExpr>(callExpr->getArgument(1))) {
			return saveString(rhs);
		}
	}

	if(callExpr->getArgument(0) == pathToKernelFile) {
		if (const CallExprPtr rhs = dynamic_pointer_cast<const CallExpr>(callExpr->getArgument(1))) {
			return saveString(rhs);
		}

	}

	return false;
}

bool KernelCodeRetriver::visitDeclarationStmt(const core::DeclarationStmtPtr& decl) {
	if (const VariablePtr& pathVar = dynamic_pointer_cast<const Variable>(pathToKernelFile)) {
		if (decl->getVariable()->getId() != pathVar->getId())
			return false;

		if (const CallExprPtr initCall = dynamic_pointer_cast<const CallExpr>(tryRemoveAlloc(decl->getInitialization(), builder))) {
			return saveString(initCall);
		}
	}
	return false;
}

void Handler::findKernelsUsingPathString(const ExpressionPtr& path, const ExpressionPtr& root, const ProgramPtr& mProgram) {
	core::NodeManager& mgr = root.getNodeManager();
	core::IRBuilder builder(mgr);

	kernelFileCache.clear();
	if(const CallExprPtr callSaC = dynamic_pointer_cast<const CallExpr>(path)) {
		if(const LiteralPtr stringAsChar = dynamic_pointer_cast<const Literal>(callSaC->getFunctionExpr())) {
			if(BASIC.isRefVectorToRefArray(stringAsChar)) {
				if(const LiteralPtr path = dynamic_pointer_cast<const Literal>(callSaC->getArgument(0))) {
					// check if file has already been added
//std::cout << "\n using path string " << path->getStringValue() << " \n\n";
					if(kernelFileCache.find(path->getStringValue()) == kernelFileCache.end()) {
						kernelFileCache.insert(path->getStringValue());
						ConversionJob job;
						kernels = loadKernelsFromFile(path->getStringValue(), builder, job);
					}
					//return;
// set source string to an empty char array
//					ret = builder.refVar(builder.literal("", builder.arrayType(BASIC.getChar())));
				}
			}
		}
	}

	string kernelFilePath;
	if(const VariablePtr pathVar = dynamic_pointer_cast<const Variable>(tryRemove(BASIC.getRefDeref(), path, builder))) {
		KernelCodeRetriver kcr(pathVar, root, mProgram, builder);
		visitDepthFirst(mProgram, kcr);
		kernelFilePath = kcr.getKernelFilePath();
	}

	if(const CallExprPtr pathCall = dynamic_pointer_cast<const CallExpr>(tryRemove(BASIC.getRefDeref(), path, builder))) {
		if(BASIC.isCompositeRefElem(pathCall->getFunctionExpr())) {
			KernelCodeRetriver kcr(pathCall, root, mProgram, builder);
			visitDepthFirst(mProgram, kcr);
			kernelFilePath = kcr.getKernelFilePath();
		}
	}
	if(kernelFilePath.size() > 0) {
		// check if file has already been added
		if(kernelFileCache.find(kernelFilePath) == kernelFileCache.end()) {
			kernelFileCache.insert(kernelFilePath);
			ConversionJob job;
			kernels = loadKernelsFromFile(kernelFilePath, builder, job);
		}
	}
}
/*
const ExpressionPtr Handler::getCreateBuffer(const ExpressionPtr& devicePtr, const ExpressionPtr& sizeArg, const bool copyPtr,
		const ExpressionPtr& hostPtr, const ExpressionPtr& errcode_ret) {
	core::NodeManager& mgr = devicePtr.getNodeManager();
	core::IRBuilder builder(mgr);

	ExpressionPtr fun = o2i.getClCreateBuffer(copyPtr, errcode_ret == builder.getTypeLiteral(builder.arrayType(BASIC.getInt4())));

	TypePtr type;
	ExpressionPtr size;
	o2i.extractSizeFromSizeof(sizeArg, size, type);
	assert_true(size && type) << "Unable to deduce type from clCreateBuffer call: No sizeof call found, cannot translate to INSPIRE.";

	vector<ExpressionPtr> args;
	args.push_back(builder.getTypeLiteral(type));
	args.push_back(devicePtr);
	args.push_back(size);
	if(copyPtr) args.push_back(hostPtr);
	args.push_back(errcode_ret);
	return builder.callExpr(builder.refType(builder.arrayType(type)), fun, args);
}
*/
const ExpressionPtr Handler::collectArgument(const ExpressionPtr& kernelArg, const ExpressionPtr& index, const ExpressionPtr& sizeArg,
		ExpressionPtr arg, KernelArgs& kernelArgs, LocalMemDecls& localMemDecls, ClmemTable& cl_mems, EquivalenceMap& eqMap) {
	core::NodeManager& mgr = kernelArg.getNodeManager();
	core::IRBuilder builder(mgr);

	// arg_index must either be an integer literal or all arguments have to be specified in the right order in the source code
	ExpressionPtr kernel = tryRemove(BASIC.getRefDeref(), kernelArg, builder);

	// TODO deal with out of order arguments non-scalar
	const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(index);
	const ExpressionPtr idxExpr = cast ? cast->getSubExpression() : index;
	LiteralPtr idx = dynamic_pointer_cast<const Literal>(idxExpr);

	// idx has to be a literal!
	const core::lang::BasicGenerator& gen = builder.getLangBasic();
	// remove cast to uint<8>
	if (idxExpr.isa<core::CallExprPtr>() && gen.isScalarCast(idxExpr.as<core::CallExprPtr>()->getFunctionExpr())){
		idx = idxExpr.as<core::CallExprPtr>()->getArgument(0).isa<core::LiteralPtr>();
	}
	assert_true(idx) << "idx MUST be a literal";

	VariablePtr tuple = builder.variable(kernel->getType());
	// set the new tuple equivalent with the kernel to be able to replace it by a tuple with correct type in 3rd pass
	eqMap[tuple] = eqMap[kernel];
	VariablePtr src = builder.variable(arg->getType());
	VariableList params;
	params.push_back(tuple);
	StatementList body;

	if(core::types::isNullPtrExpression(arg)) {
		// in this case arg is a local variable which has to be declared in host code
		// need to read size parameter
		ExpressionPtr size;
		TypePtr type;
		ExpressionPtr hostPtr;
		o2i.extractSizeFromSizeof(sizeArg, size, type);
		assert_true(size) << "Unable to deduce type from clSetKernelArg call when allocating local memory: No sizeof call found, cannot translate to INSPIRE.";

		// refresh variables used in the generation of the local variable
		VariableMap varMapping;
		refreshVariables(size, varMapping, builder);

		body.push_back(builder.callExpr(BASIC.getUnit(), BASIC.getRefAssign(), builder.callExpr(BASIC.getTupleRefElem(), tuple,
				(BASIC.isUInt8(idxExpr->getType()) ? idxExpr :	builder.castExpr(BASIC.getUInt8(), idxExpr)),
				builder.getTypeLiteral(type)), builder.refVar(builder.callExpr(type, BASIC.getArrayCreate1D(), builder.getTypeLiteral(type), size))));
		body.push_back(builder.returnStmt(builder.intLit(0)));

		//ßßß		kernelArgs[kernel].push_back(builder.getTypeLiteral((builder.refType(type))));

		if(varMapping.empty()) { // no variable from outside needed
			FunctionTypePtr fTy = builder.functionType(kernel->getType(), BASIC.getInt4());
			LambdaExprPtr function = builder.lambdaExpr(fTy, params, builder.compoundStmt(body));
			return builder.callExpr(BASIC.getInt4(), function, kernel);
		}

		TypeList argTys;
		ExpressionList args;
		args.push_back(kernel);
		argTys.push_back(kernel->getType());
		// add the needed variables
		for_each(varMapping, [&](std::pair<VariablePtr, VariablePtr> varMap) {
			args.push_back(varMap.first);
			argTys.push_back(varMap.first->getType());
			params.push_back(varMap.second);
		});

		FunctionTypePtr fTy = builder.functionType(argTys, BASIC.getInt4());
		LambdaExprPtr function = builder.lambdaExpr(fTy, params, builder.compoundStmt(body));
		return builder.callExpr(BASIC.getInt4(), function, args);


		// initialize local memory place with undefined
/*		return builder.callExpr(BASIC.getUnit(), BASIC.getRefAssign(), builder.callExpr(builder.refType(type), BASIC.getTupleRefElem(), kernel,
				(BASIC.isUInt8(idx) ? idxExpr :	builder.castExpr(BASIC.getUInt8(), idx)),
				builder.getTypeLiteral(type)), builder.refVar(builder.callExpr(type, BASIC.getArrayCreate1D(), builder.getTypeLiteral(type), size)));
*/
	}

	arg = getVarOutOfCrazyInspireConstruct(arg, builder);

//	kernelArgs[kernel] = builder.variable(builder.tupleType(argTypes));
//ßßß	kernelArgs[kernel].push_back(arg);
//std::cout << "ARGUMENT: \t" << kernel->getType() << std::endl;

	FunctionTypePtr fTy = builder.functionType(toVector(kernel->getType(), tryDeref(arg, builder)->getType()), BASIC.getInt4());
	params.push_back(src);

	body.push_back(builder.assign( builder.callExpr(BASIC.getTupleRefElem(), tuple,
								   (BASIC.isUInt8(idxExpr->getType()) ? idxExpr :	builder.castExpr(BASIC.getUInt8(), idxExpr)),
									builder.getTypeLiteral(src->getType())), src));
	body.push_back(builder.returnStmt(builder.intLit(0)));
	LambdaExprPtr function = builder.lambdaExpr(fTy, params, builder.compoundStmt(body));

	// store argument in a tuple
	return builder.callExpr(BASIC.getInt4(), function, kernel, tryDeref(arg, builder));
}


bool Ocl2Inspire::extractSizeFromSizeof(const core::ExpressionPtr& arg, core::ExpressionPtr& size, core::TypePtr& type, bool foundMul) {
	core::NodeManager& mgr = arg.getNodeManager();
	core::IRBuilder builder(mgr);

	// get rid of casts
	NodePtr uncasted = arg;
	while (uncasted->getNodeType() == core::NT_CastExpr) {
		uncasted = static_pointer_cast<CastExprPtr>(uncasted)->getType();
	}

	if (const CallExprPtr call = dynamic_pointer_cast<const CallExpr> (uncasted)) {
		// check if there is a multiplication
		if(call->getFunctionExpr()->toString().find(".mul") != string::npos && call->getArguments().size() == 2) {
			// recursively look into arguments of multiplication
			if(extractSizeFromSizeof(call->getArgument(0), size, type, true)) {
				if(size)
					size = builder.callExpr(call->getType(), call->getFunctionExpr(), size, call->getArgument(1));
				else
					size = call->getArgument(1);
				return true;
			}
			if(extractSizeFromSizeof(call->getArgument(1), size, type, true)){
				if(size)
					size = builder.callExpr(call->getType(), call->getFunctionExpr(), call->getArgument(0), size);
				else
					size = call->getArgument(0);
				return true;
			}
		}
		// check if we reached a sizeof call
		if (call->toString().substr(0, 6).find("sizeof") != string::npos) {
			// extract the type to be allocated
			type = dynamic_pointer_cast<GenericTypePtr>(call->getArgument(0)->getType())->getTypeParameter(0);
			assert_true(type) << "Type could not be extracted!";

			if(!foundMul){ // no multiplication, just sizeof alone is passed as argument -> only one element
				size = builder.literal(BASIC.getUInt8(), "1");
				return true;
			}

			return true;
		}
	}
	return false;
}

ExpressionPtr Ocl2Inspire::getClCreateBuffer(bool copyHostPtr, bool setErrcodeRet, core::IRBuilder builder) {
	// read/write flags ignored
	// errcorcode always set to 0 = CL_SUCCESS for clCreatBuffer and ignored for icl_create_buffer

	std::string returnErrorcode = setErrcodeRet ? "		errorcode_ret[0u] = 0; " : "";

	if (copyHostPtr)
		return builder.parseExpr(
		"("
		"	type<'a> 				elemType, "
		"	uint<8> 				flags, "
		"	uint<8> 				size, "
		"	ref<any> 				hostPtr, "
		"	ref<array<int<4>,1> > 	errorcode_ret"
		") -> ref<array<'a, 1> >  { "
		"		ref<array<'a,1>> devicePtr = new( array.create.1D( elemType, size ) ); "
		"		ref<array<'a,1>> 		hp = ref.reinterpret(hostPtr, lit(array<'a,1>)); "
		"		for(uint<8> i = 0u .. size) { "
		"			devicePtr[i] = *hp[i]; "
		"		} "
		+ returnErrorcode +
		"		return devicePtr; "
	 	"}");

	return builder.parseExpr(
		"( "
		"	type<'a>				elemType, "
		"	uint<8> 				flags, "
		"	uint<8> 				size, "
		"	ref<array<int<4>, 1> >  errorcode_ret"
		") -> ref<array<'a, 1> > { "
		+ returnErrorcode +
		"	return new( array.create.1D( elemType, size )); "
       	"}");
}

ExpressionPtr Ocl2Inspire::getClCopyBuffer(core::IRBuilder builder) {
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr(
		"( "
		"	ref<array<'a, 1> > srcBuffer, "
		"	 ref<array<'a, 1> > dstBuffer, "
		"	 uint<8>      		srcOffset, "
		"	 uint<8> 			dstOffset, "
		"	 uint<8> 			cb"
		") -> int<4> {"
		"	uint<8> do = dstOffset / sizeof( lit('a)); "
		"	uint<8> so = srcOffset / sizeof( lit('a)); "
        "   for(uint<8> i = 0u .. cb) { "
		"		dstBuffer[i + do] = *srcBuffer[i + so]; "
		"	}"
		"	return 0; "
    	"}");
}

ExpressionPtr Ocl2Inspire::getClCopyBufferFallback(core::IRBuilder builder) {
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr(
		"("
		"	ref<array<'a, 1> >	srcBuffer, "
		"	ref<array<'a, 1> > 	dstBuffer, "
		"	uint<8>				srcOffset, "
		"	uint<8>				dstOffset, "
		"	uint<8>				cb"
		") -> int<4> { "
		"	uint<8> size = cb / sizeof( lit('a) ); "
		"	uint<8>   do = dstOffset / sizeof( lit('a)); "
		"	uint<8>	  so = srcOffset / sizeof( lit('a)); "
		"	for(uint<8> i = 0u .. size) { "
		"		dstBuffer[i + dstOffset] = *srcBuffer[i + dstOffset]; "
		"	}"
		"	return 0; "
		"}");
}

ExpressionPtr Ocl2Inspire::getClWriteBuffer(core::IRBuilder builder) {
	// blocking_write ignored
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr(
		"("
		"	ref<array<'a, 1> > 	devicePtr, "
		"	uint<4>				blocking_write, "
		"	uint<8>				offset, "
		"	uint<8>				cb, "
		"	ref<any>			hostPtr"
		") -> int<4> { "
		"	ref<array<'a,1>> hp = ref.reinterpret(hostPtr, lit(array<'a, 1>)); "
		"	uint<8> 	  	  o = offset / sizeof( lit('a) ); "
		"	for(uint<8> i = 0u .. cb) { "
		"		devicePtr[i + o] = *hp[i]; "
		"	}"
		"	return 0; "
    	"}");
}

ExpressionPtr Ocl2Inspire::getClWriteBufferFallback(core::IRBuilder builder) {
	// blocking_write ignored
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr(
		"("
		"	ref<array<'a, 1>> 	devicePtr, "
		"	uint<4> 			blocking_write, "
		"	uint<8>				offset, "
		"	uint<8>				cb, "
		"	ref<any>			hostPtr"
		") -> int<4> { "
		"	ref<array<'a,1>> hp = ref.reinterpret(hostPtr, lit(array<'a,1>)); "
		"	uint<8> 		  o = offset / sizeof( lit('a) ); "
        "	uint<8> 	   size = cb / sizeof( lit('a) ); "
        "	for(uint<8> i = 0u .. size) { "
        "		devicePtr[i + o] = *hp[i]; "
		"	}"
        "	return 0; "
    	"}");
}

ExpressionPtr Ocl2Inspire::getClReadBuffer(core::IRBuilder builder) {
	// blocking_write ignored
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr(
		"("
		"	ref<array<'a,1>> 	devicePtr, "
		"	uint<4> 			blocking_read, "
		"	uint<8>				offset, "
		"	uint<8>				cb, "
		"	ref<any> 			hostPtr"
		") -> int<4> { "
		"	ref<array<'a,1>> hp = ref.reinterpret(hostPtr, lit(array<'a,1>)); "
		"	uint<8>			  o = offset / sizeof( lit('a) ); "
		"	for(uint<8> i = 0u .. cb) { "
		"		hp[i] = *devicePtr[i + o]; "
		"	}"
        "	return 0; "
 	   	"}");
}

ExpressionPtr Ocl2Inspire::getClReadBufferFallback(core::IRBuilder builder) {
	// blocking_write ignored
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr(
		"("
		"	ref<array<'a, 1> > 	devicePtr, "
		"	uint<4> 			blocking_read, "
		"	uint<8> 			offset, "
		"	uint<8> 			cb, "
		"	ref<any> 			hostPtr"
		") -> int<4> { "
        "	ref<array<'a, 1> > hp = ref.reinterpret(hostPtr, lit(array<'a,1>)); "
		"	uint<8> 		 size = cb / sizeof( lit('a) ); "
		"	uint<8> 			o = offset / sizeof( lit('a) ); "
		"	for(uint<8> i = 0u .. size) { "
		"		hp[i] = *devicePtr[i + o]; "
		"	} "
        "	return 0; "
    	"}");
}

ExpressionPtr Ocl2Inspire::getClGetIDs(core::IRBuilder builder) {
	// does simply set the number of devices to 1 and returns 0 = CL_SUCCESS
	// TODO add functionality
	return builder.parseExpr(
		"(ref<array<uint<4>,1>> num_devices) -> int<4> { "
		"	num_devices[0u] = 1u; "
		"	return 0; "
		"}");
}

ExpressionPtr Ocl2Inspire::getClSetKernelArg(core::IRBuilder builder) {
	// alsways returns 0 = CL_SUCCESS
	return builder.parseExpr(
		"(ref<'a> argTuple, uint<8> idx, type<'b> ty, 'b src) -> int<4> { "
        "	argTuple.idx = src; "
		"	return 0; "
  		"}");
}



HandlerPtr& OclSimpleFunHandler::findHandler(const string& fctName) {
	// for performance reasons working with function prefixes is only enabled for funcitons containing cl
	// needed for cl*, ocl* and icl_* functions
	if (fctName.find("cl") == string::npos)
		return handles[fctName];
	// checking function names, starting from the full name
	for (int i = fctName.size(); i > 2; --i) {
		if (HandlerPtr& h = handles[fctName.substr(0,i)])
			return h;
	}
	return handles["break"]; // function is not in map
}

OclSimpleFunHandler::OclSimpleFunHandler() {
	// TODO at the moment there will always be one platform and one device, change that!
	ADD_Handler(o2i, "clGetDeviceIDs",
			ocl::NullLitSearcher nls(builder);
			ExpressionPtr ret;
			if(visitDepthFirstInterruptible(node->getArgument(4), nls))
				ret = builder.intLit(0);
			else
				ret = builder.callExpr(o2i.getClGetIDs(IRBuilder(node->getNodeManager())), node->getArgument(4));
			return ret;
	);

	ADD_Handler(o2i, "clGetPlatformIDs",
			ocl::NullLitSearcher nls(builder);
			ExpressionPtr ret;

			if(visitDepthFirstInterruptible(node->getArgument(2), nls))
				ret = builder.intLit(0);
			else
				ret = builder.callExpr(o2i.getClGetIDs(IRBuilder(node->getNodeManager())), node->getArgument(2));
			return ret;
	);

	ADD_Handler(o2i, "icl_get_num_devices",
			return builder.literal("1u", BASIC.getUInt4());
	);
/*
	ADD_Handler(o2i, "clCreateBuffer",
			std::set<enum CreateBufferFlags> flags = this->getFlags<enum CreateBufferFlags>(node->getArgument(1));

			// check if CL_MEM_USE_HOST_PTR is set
			bool usePtr = flags.find(CreateBufferFlags::CL_MEM_USE_HOST_PTR) != flags.end();
			// check if CL_MEM_COPY_HOST_PTR is set
			bool copyPtr = flags.find(CreateBufferFlags::CL_MEM_COPY_HOST_PTR) != flags.end();

			// extract the size form argument size, relying on it using a multiple of sizeof(type)
			ExpressionPtr hostPtr;

			hostPtr = node->getArgument(3);
			if(CastExprPtr c = dynamic_pointer_cast<const CastExpr>(node->getArgument(3))) {
				assert_false(copyPtr) << "When CL_MEM_COPY_HOST_PTR is set, host_ptr parameter must be a valid pointer";
				if(c->getSubExpression()->getType() != BASIC.getAnyRef()) {// a scalar (probably NULL) has been passed as hostPtr arg
					hostPtr = builder.callExpr(BASIC.getRefVar(), c->getSubExpression());
				}
			}

			if(usePtr) { // in this case we can just use the host_ptr instead of the cl_mem variable
				return hostPtr;
			}

			return getCreateBuffer("clCreateBuffer", node->getArgument(1), node->getArgument(2), copyPtr, hostPtr, node->getArgument(4));
	);

	ADD_Handler(o2i, "icl_create_buffer",
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

			// Flags can be ignored, INSPIRE is always blocking
			return getCreateBuffer("icl_create_buffer", node->getArgument(1), node->getArgument(2), false, builder.intLit(0),
					mgr.getLangBasic().getRefNull());
	);
*/
	ADD_Handler(o2i, "clEnqueueCopyBuffer",
			// extract the size form argument size, relying on it using a multiple of sizeof(type)
			vector<ExpressionPtr> args;

#if ALWAYS_FALLBACK
			args.push_back(node->getArgument(1));
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			args.push_back(node->getArgument(4));
			args.push_back(node->getArgument(5));
			return builder.callExpr(o2i.getClCopyBufferFallback(builder), args);
#else
			ExpressionPtr size;
			TypePtr type;
			o2i.extractSizeFromSizeof(node->getArgument(4), size, type);

			args.push_back(node->getArgument(1));
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			args.push_back(node->getArgument(4));
			args.push_back(node->getArgument(5));
			return builder.callExpr(size ? o2i.getClCopyBuffer(builder) : o2i.getClCopyBufferFallback(builder), args);
#endif
	);

	ADD_Handler(o2i, "clEnqueueWriteBuffer",
			// extract the size form argument size, relying on it using a multiple of sizeof(type)
			NullLitSearcher nls(builder);
			vector<ExpressionPtr> args;

#if ALWAYS_FALLBACK
			args.push_back(node->getArgument(1));
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			args.push_back(node->getArgument(4));
			args.push_back(node->getArgument(5));
			return builder.callExpr(o2i.getClWriteBufferFallback(builder), args);
#else
			ExpressionPtr size;
			TypePtr type;
			o2i.extractSizeFromSizeof(node->getArgument(4), size, type);

			args.push_back(node->getArgument(1));
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			args.push_back(size ? size : node->getArgument(4));
			args.push_back(node->getArgument(5));
			return builder.callExpr(size ? o2i.getClWriteBuffer(builder) : o2i.getClWriteBufferFallback(builder), args);
#endif
	);

	ADD_Handler(o2i, "icl_write_buffer",
			// extract the size form argument size, relying on it using a multiple of sizeof(type)
			vector<ExpressionPtr> args;
#if ALWAYS_FALLBACK
			args.push_back(node->getArgument(0));
			args.push_back(node->getArgument(1));
			args.push_back(builder.uintLit(0)); // offset not supported
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			return builder.callExpr(o2i.getClWriteBufferFallback(builder), args);
#else
			ExpressionPtr size;
			TypePtr type;

			o2i.extractSizeFromSizeof(node->getArgument(2), size, type);

			args.push_back(node->getArgument(0));
			args.push_back(node->getArgument(1));
			args.push_back(builder.uintLit(0)); // offset not supported
			args.push_back(size ? size : node->getArgument(2));
			args.push_back(node->getArgument(3));
			return builder.callExpr(size ? o2i.getClWriteBuffer(builder) : o2i.getClWriteBufferFallback(builder), args);
#endif
	);

	ADD_Handler(o2i, "clEnqueueReadBuffer",
			vector<ExpressionPtr> args;
#if ALWAYS_FALLBACK
			args.push_back(node->getArgument(1));
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			args.push_back(node->getArgument(4));
			args.push_back(node->getArgument(5));
			return builder.callExpr(o2i.getClReadBufferFallback(builder), args);
#else
			// extract the size form argument size, relying on it using a multiple of sizeof(type)
			ExpressionPtr size;
			TypePtr type;

			o2i.extractSizeFromSizeof(node->getArgument(4), size, type);

			args.push_back(node->getArgument(1));
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			args.push_back(size ? size : node->getArgument(4));
			args.push_back(node->getArgument(5));
			return builder.callExpr(size ? o2i.getClReadBuffer(builder) : o2i.getClReadBufferFallback(builder), args);
#endif
	);

	ADD_Handler(o2i, "icl_read_buffer",
			vector<ExpressionPtr> args;
#if ALWAYS_FALLBACK
			args.push_back(node->getArgument(0));
			args.push_back(node->getArgument(1));
			args.push_back(builder.uintLit(0)); // offset not supported
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			return builder.callExpr(o2i.getClReadBufferFallback(builder), args);
#else
			// extract the size form argument size, relying on it using a multiple of sizeof(type)
			ExpressionPtr size;
			TypePtr type;

			o2i.extractSizeFromSizeof(node->getArgument(2), size, type);

			args.push_back(node->getArgument(0));
			args.push_back(node->getArgument(1));
			args.push_back(builder.uintLit(0)); // offset not supported
			args.push_back(size ? size : node->getArgument(2));
			args.push_back(node->getArgument(3));
			return builder.callExpr(size ? o2i.getClReadBuffer(builder) : o2i.getClReadBufferFallback(builder), args);
#endif
	);


/*
	ADD_Handler(o2i, "clSetKernelArg",
			return collectArgument("clSetKernelArg", node->getArgument(0), node->getArgument(1), node->getArgument(2), node->getArgument(3));
	);

	ADD_Handler(o2i, "oclLoadProgSource",
			this->findKernelsUsingPathString("oclLoadProgSource", node->getArgument(0), node);
			// set source string to an empty char array
			return builder.refVar(builder.literal("", builder.arrayType(BASIC.getChar())));
	);

	// TODO ignores 3rd argument (kernelName) and just adds all kernels to the program
	ADD_Handler(o2i, "icl_create_kernel",
			// find kernel source code
			this->findKernelsUsingPathString("icl_create_kernel", node->getArgument(1), node);
			return builder.uintLit(0);
	);
*/
	ADD_Handler(o2i, "clEnqueueNDRangeKernel",
			// get argument vector
/*			ExpressionPtr k = tryRemove(BASIC.getRefDeref(), node->getArgument(1), builder);
//			tryStructExtract(k, builder);

//			std::cout << "\nEqMap: " << eqMap;
//			std::cout << "\nKernelARgs: " << kernelArgs << "\nkernel: " << k << std::endl;
			assert(kernelArgs.find(k) != kernelArgs.end() && "Cannot find any arguments for kernel function");

			std::vector<core::ExpressionPtr> args = kernelArgs[k];
			// adding global and local size to the argument vector
			args.push_back(node->getArgument(4) );
			args.push_back(node->getArgument(5) );
*/
			return node;
	);
/*
	ADD_Handler(o2i, "icl_run_kernel",
			// construct argument vector
			ExpressionPtr k = tryRemove(BASIC.getRefDeref(), node->getArgument(0), builder);
			// get Varlist tuple
			if(const CallExprPtr varlistPack = dynamic_pointer_cast<const CallExpr>(node->getArgument(7))) {
				if(const TupleExprPtr varlist = dynamic_pointer_cast<const TupleExpr>(varlistPack->getArgument(0))) {
					size_t argCnt = 0;
					for(auto I = varlist->getExpressions().begin(); I != varlist->getExpressions().end(); ++I) {
						// collect arguments
						ExpressionPtr sizeArg = *I;
						++I; // skip size argument

						ExpressionPtr arg = *I;
						// check for local memory argument
						if(core::types::isNullPtrExpression(arg)) {
							// in this case arg is a local variable which has to be declared in host code
							// need to read size parameter
							ExpressionPtr size;
							TypePtr type;

							o2i.extractSizeFromSizeof(sizeArg, size, type);
							assert_true(size) << "Unable to deduce type from clSetKernelArg call when allocating local memory: No sizeof call found, cannot translate to INSPIRE.";

							// declare a new variable to be used as argument
							VariablePtr localMem = builder.variable(builder.refType(builder.arrayType(type)));
							DeclarationStmtPtr localDecl = builder.declarationStmt(localMem, builder.callExpr(BASIC.getRefVar(),
											builder.callExpr(BASIC.getArrayCreate1D(), builder.getTypeLiteral(type), size)));
							// should I really have access to private members or HostMapper here or is this a compiler bug?
							localMemDecls[k].push_back(localDecl);
							arg = localMem;
						}

						kernelArgs[k].push_back(getVarOutOfCrazyInspireConstruct(arg, builder));
						argCnt++;
					}
				}
			}
*//*
			// get argument vector
			assert(kernelArgs.find(k) != kernelArgs.end() && "Cannot find any arguments for kernel function");

			std::vector<core::ExpressionPtr> args = kernelArgs[k];
			// adding global and local size to the argument vector of original call
			// will be added to kernelArgs in 3rd pass like when using standard ocl routines
			args.push_back(node->getArgument(2) );
			args.push_back(node->getArgument(3) );
*//*
			return node;
	);
*/
	ADD_Handler(o2i, "clBuildProgram",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO add flags for profiling and out of order
	ADD_Handler(o2i, "clGetEventProfilingInfo",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO add syncronization means when adding asynchronous queue
	ADD_Handler(o2i, "clFinish",
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(o2i, "clWaitForEvents",
			// return cl_success
			return builder.intLit(0);
	);

	// need to release clMem objects
	ADD_Handler(o2i, "clReleaseMemObject",
			return builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), node->getArgument(0));
	);
	ADD_Handler(o2i, "icl_release_buffer",
			return builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), node->getArgument(0));
	);
	ADD_Handler(o2i, "icl_release_buffers",
			// execute a ref.delete for each pointer in the argument list inside a compound statement
			StatementList dels;

			if(const CallExprPtr varlistPack = dynamic_pointer_cast<const CallExpr>(node->getArgument(1))) {
				if(const TupleExprPtr varlist = dynamic_pointer_cast<const TupleExpr>(varlistPack->getArgument(0))) {
					for(auto I = varlist->getExpressions().begin(); I != varlist->getExpressions().end(); ++I) {
						dels.push_back(builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), *I));
					}
				}
			}
			return builder.compoundStmt(dels);
	);

	ADD_Handler(o2i, "clCreateProgram",
			// return cl_success
			return builder.refReinterpret(BASIC.getRefNull(), builder.arrayType(BASIC.getInt4()));
	);

	// release of kernel will be used to free the tuple holding the kernel arguments
	ADD_Handler(o2i, "clReleaseKernel",
			return builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), node->getArgument(0));
	);
	ADD_Handler(o2i, "icl_release_kernel",
			return builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), node->getArgument(0));
	);

	// all other clRelease calls can be ignored since the variables are removed
	ADD_Handler(o2i, "clRelease",
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(o2i, "clRetain",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO implement, may have some semantic meaning
	ADD_Handler(o2i, "clGetEventInfo",
			LOG(WARNING) << "Removing clGetEventInfo. Check the semantics!";
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(o2i, "clFlush",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO maybe a bit too optimisitc?
	ADD_Handler(o2i, "clGet",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO need to add exception this when adding image support
	ADD_Handler(o2i, "clCreate",
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(o2i, "clUnloadCompiler",
			// return cl_success
			return builder.intLit(0);
	);

	// DEPRECATED, but used in the NVIDIA examples
	ADD_Handler(o2i, "clSetCommandQueueProperty",
			// return cl_success
			return builder.intLit(0);
	);

	// exceptions, will be handled in a later step
	ADD_Handler(o2i, "clCreateContext", return node;);
	ADD_Handler(o2i, "clCreateCommandQueue", return node;);
	ADD_Handler(o2i, "clCreateKernel", return node;);

	// exceptions for runtime functions, keep them as they are
	ADD_Handler(o2i, "icl_init_args", return node;);
	ADD_Handler(o2i, "icl_parse_args", return node;);
	ADD_Handler(o2i, "icl_release_args", return node;);

	// exceptions for the icl_lib_bmp library
	ADD_Handler(o2i, "icl_savebmp", return node;);
	ADD_Handler(o2i, "icl_loadbmp", return node;);

	// exceptions for icl_lib power measurent
	ADD_Handler(o2i, "icl_start_energy_measurement", return node;);
	ADD_Handler(o2i, "icl_stop_energy_measurement", return node;);


	// handlers for lib_icl runtime stuff
	ADD_Handler(o2i, "icl_",
		return builder.literal(node->getType(), "0"); // default handling, remove it
	);
}

const NodePtr OclSimpleFunHandler::resolveElement(const NodePtr& ptr){
	// stopp recursion at type level
	if (ptr->getNodeCategory() == NodeCategory::NC_Type) {
		return ptr;
	}

	if(ptr->getNodeType() != NT_CallExpr)
		return ptr->substitute(ptr.getNodeManager(), *this);

	CallExprPtr callExpr = ptr.as<CallExprPtr>();

	const ExpressionPtr fun = callExpr->getFunctionExpr();

	if(const LiteralPtr literal = dynamic_pointer_cast<const Literal>(fun)) {
//std::cout << "\nTry to handle " << literal->getValue();
		//			callExpr->substitute(builder.getNodeManager(), *this);
		if(const HandlerPtr& replacement = findHandler(literal->getStringValue())) {
			NodePtr ret = replacement->handleNode(callExpr);
//std::cout << "\nHandling fct " << literal->getValue() << " -> " << ret << std::endl;
			// check if new kernels have been created
/*			const vector<ExpressionPtr>& kernels = replacement->getKernels();
			if(kernels.size() > 0) {
				for_each(kernels, [&](ExpressionPtr kernel) {
						kernelEntries.push_back(kernel);
					});
				replacement->resetKernels();
			}
			copyAnnotations(callExpr, ret);
			*/
			return ret;
		} else
			return ptr->substitute(ptr.getNodeManager(), *this);
	} else
		return ptr->substitute(ptr.getNodeManager(), *this);
	return ptr;
}

} //namespace ocl
} //namespace frontend
} //namespace insieme
