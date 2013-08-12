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

#include "insieme/core/ir_node.h"
#include "insieme/frontend/ocl/ocl_host_utils.h"
#include "insieme/frontend/ocl/ocl_host_1st_pass.h"
#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"

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
		assert(job.getFiles().size() == 1u);
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

bool isNullPtr(const ExpressionPtr& expr, const IRBuilder& builder) {
	// cast to void pointer
	if (BASIC.isRefNull(expr))
		return true;

	// null literal
	const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(expr);
	const ExpressionPtr idxExpr = cast ? cast->getSubExpression() : expr;
	const LiteralPtr idx = dynamic_pointer_cast<const Literal>(idxExpr);
	if(!!idx && idx->getValueAs<int>() == 0)
		return true;


	return false;
}

bool KernelCodeRetriver::saveString(const core::LiteralPtr& lit) {
	path = lit->getStringValue();
	return true;
}

bool KernelCodeRetriver::saveString(const core::CallExprPtr& call) {
	if (const LiteralPtr lit = dynamic_pointer_cast<const Literal>(call->getFunctionExpr())) {
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
	kernelFileCache.clear();
	if(const CallExprPtr callSaC = dynamic_pointer_cast<const CallExpr>(path)) {
		if(const LiteralPtr stringAsChar = dynamic_pointer_cast<const Literal>(callSaC->getFunctionExpr())) {
			if(BASIC.isRefVectorToRefArray(stringAsChar)) {
				if(const LiteralPtr path = dynamic_pointer_cast<const Literal>(callSaC->getArgument(0))) {
					// check if file has already been added
//std::cout << "\n using path string " << path->getStringValue() << " \n\n";
					if(kernelFileCache.find(path->getStringValue()) == kernelFileCache.end()) {
						kernelFileCache.insert(path->getStringValue());
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
			kernels = loadKernelsFromFile(kernelFilePath, builder, job);
		}
	}
}

const ExpressionPtr Handler::getCreateBuffer(const ExpressionPtr& devicePtr, const ExpressionPtr& sizeArg, const bool copyPtr,
		const ExpressionPtr& hostPtr, const ExpressionPtr& errcode_ret) {
	ExpressionPtr fun = o2i.getClCreateBuffer(copyPtr, errcode_ret == builder.getTypeLiteral(builder.arrayType(BASIC.getInt4())));

	TypePtr type;
	ExpressionPtr size;
	o2i.extractSizeFromSizeof(sizeArg, size, type);
	assert(size && type && "Unable to deduce type from clCreateBuffer call: No sizeof call found, cannot translate to INSPIRE.");

	vector<ExpressionPtr> args;
	args.push_back(builder.getTypeLiteral(type));
	args.push_back(devicePtr);
	args.push_back(size);
	if(copyPtr) args.push_back(hostPtr);
	args.push_back(errcode_ret);
	return builder.callExpr(builder.refType(builder.arrayType(type)), fun, args);
}

const ExpressionPtr Handler::collectArgument(const ExpressionPtr& kernelArg, const ExpressionPtr& index, const ExpressionPtr& sizeArg,
		ExpressionPtr arg, KernelArgs& kernelArgs, LocalMemDecls& localMemDecls, ClmemTable& cl_mems, EquivalenceMap& eqMap) {
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
	assert(idx && "idx MUST be a literal");

	VariablePtr tuple = builder.variable(kernel->getType());
	// set the new tuple equivalent with the kernel to be able to replace it by a tuple with correct type in 3rd pass
	eqMap[tuple] = eqMap[kernel];
	VariablePtr src = builder.variable(arg->getType());
	VariableList params;
	params.push_back(tuple);
	StatementList body;

	if(isNullPtr(arg, builder)) {
		// in this case arg is a local variable which has to be declared in host code
		// need to read size parameter
		ExpressionPtr size;
		TypePtr type;
		ExpressionPtr hostPtr;
		o2i.extractSizeFromSizeof(sizeArg, size, type);
		assert(size && "Unable to deduce type from clSetKernelArg call when allocating local memory: No sizeof call found, cannot translate to INSPIRE.");

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
			assert(type && "Type could not be extracted!");

			if(!foundMul){ // no multiplication, just sizeof alone is passed as argument -> only one element
				size = builder.literal(BASIC.getUInt8(), "1");
				return true;
			}

			return true;
		}
	}
	return false;
}

ExpressionPtr Ocl2Inspire::getClCreateBuffer(bool copyHostPtr, bool setErrcodeRet) {
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

ExpressionPtr Ocl2Inspire::getClCopyBuffer() {
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

ExpressionPtr Ocl2Inspire::getClCopyBufferFallback() {
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

ExpressionPtr Ocl2Inspire::getClWriteBuffer() {
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

ExpressionPtr Ocl2Inspire::getClWriteBufferFallback() {
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

ExpressionPtr Ocl2Inspire::getClReadBuffer() {
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

ExpressionPtr Ocl2Inspire::getClReadBufferFallback() {
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

ExpressionPtr Ocl2Inspire::getClGetIDs() {
	// does simply set the number of devices to 1 and returns 0 = CL_SUCCESS
	// TODO add functionality
	return builder.parseExpr(
		"(ref<array<uint<4>,1>> num_devices) -> int<4> { "
		"	num_devices[0u] = 1u; "
		"	return 0; "
		"}");
}

ExpressionPtr Ocl2Inspire::getClSetKernelArg() {
	// alsways returns 0 = CL_SUCCESS
	return builder.parseExpr(
		"(ref<'a> argTuple, uint<8> idx, type<'b> ty, 'b src) -> int<4> { "
        "	argTuple.idx = src; "
		"	return 0; "
  		"}");
}

HostMapper::HostMapper(IRBuilder& build, ProgramPtr& program, const ConversionJob& job) :
	builder(build), job(job), o2i(build), mProgram(program), kernelArgs( // specify constructor arguments to pass the builder to the compare class
		boost::unordered_map<core::ExpressionPtr, std::vector<core::ExpressionPtr>, hash_target<core::ExpressionPtr>, equal_variables>::size_type(),
		hash_target_specialized(build, eqMap), equal_variables(build, program)), localMemDecls(
		boost::unordered_map<core::ExpressionPtr, std::vector<core::ExpressionPtr>, hash_target<core::ExpressionPtr>, equal_variables>::size_type(),
		hash_target_specialized(build, eqMap), equal_variables(build, program)) {
	eqIdx = 1;

	// TODO at the moment there will always be one platform and one device, change that!
	ADD_Handler(builder, o2i, "clGetDeviceIDs",
			NullLitSearcher nls(builder);
			ExpressionPtr ret;
			if(visitDepthFirstInterruptible(node->getArgument(4), nls))
				ret = builder.intLit(0);
			else
				ret = builder.callExpr(o2i.getClGetIDs(), node->getArgument(4));
			return ret;
	);

	ADD_Handler(builder, o2i, "clGetPlatformIDs",
			NullLitSearcher nls(builder);
			ExpressionPtr ret;

			if(visitDepthFirstInterruptible(node->getArgument(2), nls))
				ret = builder.intLit(0);
			else
				ret = builder.callExpr(o2i.getClGetIDs(), node->getArgument(2));
			return ret;
	);

	ADD_Handler(builder, o2i, "icl_get_num_devices",
			return builder.literal("1u", BASIC.getUInt4());
	);

	ADD_Handler(builder, o2i, "clCreateBuffer",
			std::set<enum CreateBufferFlags> flags = this->getFlags<enum CreateBufferFlags>(node->getArgument(1));

			// check if CL_MEM_USE_HOST_PTR is set
			bool usePtr = flags.find(CreateBufferFlags::CL_MEM_USE_HOST_PTR) != flags.end();
			// check if CL_MEM_COPY_HOST_PTR is set
			bool copyPtr = flags.find(CreateBufferFlags::CL_MEM_COPY_HOST_PTR) != flags.end();

			// extract the size form argument size, relying on it using a multiple of sizeof(type)
			ExpressionPtr hostPtr;

			hostPtr = node->getArgument(3);
			if(CastExprPtr c = dynamic_pointer_cast<const CastExpr>(node->getArgument(3))) {
				assert(!copyPtr && "When CL_MEM_COPY_HOST_PTR is set, host_ptr parameter must be a valid pointer");
				if(c->getSubExpression()->getType() != BASIC.getAnyRef()) {// a scalar (probably NULL) has been passed as hostPtr arg
					hostPtr = builder.callExpr(BASIC.getRefVar(), c->getSubExpression());
				}
			}

			if(usePtr) { // in this case we can just use the host_ptr instead of the cl_mem variable
				return hostPtr;
			}

			return getCreateBuffer("clCreateBuffer", node->getArgument(1), node->getArgument(2), copyPtr, hostPtr, node->getArgument(4));
	);

	ADD_Handler(builder, o2i, "icl_create_buffer",
			// Flags can be ignored, INSPIRE is always blocking
			return getCreateBuffer("icl_create_buffer", node->getArgument(1), node->getArgument(2), false, builder.intLit(0), BASIC.getRefNull());
	);

	ADD_Handler(builder, o2i, "clEnqueueCopyBuffer",
			// extract the size form argument size, relying on it using a multiple of sizeof(type)
			vector<ExpressionPtr> args;

#if ALWAYS_FALLBACK
			args.push_back(node->getArgument(1));
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			args.push_back(node->getArgument(4));
			args.push_back(node->getArgument(5));
			return builder.callExpr(o2i.getClCopyBufferFallback(), args);
#else
			ExpressionPtr size;
			TypePtr type;
			o2i.extractSizeFromSizeof(node->getArgument(4), size, type);

			args.push_back(node->getArgument(1));
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			args.push_back(node->getArgument(4));
			args.push_back(node->getArgument(5));
			return builder.callExpr(size ? o2i.getClCopyBuffer() : o2i.getClCopyBufferFallback(), args);
#endif
	);

	ADD_Handler(builder, o2i, "clEnqueueWriteBuffer",
			// extract the size form argument size, relying on it using a multiple of sizeof(type)
			NullLitSearcher nls(builder);
			vector<ExpressionPtr> args;

#if ALWAYS_FALLBACK
			args.push_back(node->getArgument(1));
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			args.push_back(node->getArgument(4));
			args.push_back(node->getArgument(5));
			return builder.callExpr(o2i.getClWriteBufferFallback(), args);
#else
			ExpressionPtr size;
			TypePtr type;
			o2i.extractSizeFromSizeof(node->getArgument(4), size, type);

			args.push_back(node->getArgument(1));
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			args.push_back(size ? size : node->getArgument(4));
			args.push_back(node->getArgument(5));
			return builder.callExpr(size ? o2i.getClWriteBuffer() : o2i.getClWriteBufferFallback(), args);
#endif
	);

	ADD_Handler(builder, o2i, "icl_write_buffer",
			// extract the size form argument size, relying on it using a multiple of sizeof(type)
			vector<ExpressionPtr> args;
#if ALWAYS_FALLBACK
			args.push_back(node->getArgument(0));
			args.push_back(node->getArgument(1));
			args.push_back(builder.uintLit(0)); // offset not supported
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			return builder.callExpr(o2i.getClWriteBufferFallback(), args);
#else
			ExpressionPtr size;
			TypePtr type;

			o2i.extractSizeFromSizeof(node->getArgument(2), size, type);

			args.push_back(node->getArgument(0));
			args.push_back(node->getArgument(1));
			args.push_back(builder.uintLit(0)); // offset not supported
			args.push_back(size ? size : node->getArgument(2));
			args.push_back(node->getArgument(3));
			return builder.callExpr(size ? o2i.getClWriteBuffer() : o2i.getClWriteBufferFallback(), args);
#endif
	);

	ADD_Handler(builder, o2i, "clEnqueueReadBuffer",
			vector<ExpressionPtr> args;
#if ALWAYS_FALLBACK
			args.push_back(node->getArgument(1));
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			args.push_back(node->getArgument(4));
			args.push_back(node->getArgument(5));
			return builder.callExpr(o2i.getClReadBufferFallback(), args);
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
			return builder.callExpr(size ? o2i.getClReadBuffer() : o2i.getClReadBufferFallback(), args);
#endif
	);

	ADD_Handler(builder, o2i, "icl_read_buffer",
			vector<ExpressionPtr> args;
#if ALWAYS_FALLBACK
			args.push_back(node->getArgument(0));
			args.push_back(node->getArgument(1));
			args.push_back(builder.uintLit(0)); // offset not supported
			args.push_back(node->getArgument(2));
			args.push_back(node->getArgument(3));
			return builder.callExpr(o2i.getClReadBufferFallback(), args);
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
			return builder.callExpr(size ? o2i.getClReadBuffer() : o2i.getClReadBufferFallback(), args);
#endif
	);



	ADD_Handler(builder, o2i, "clSetKernelArg",
			return collectArgument("clSetKernelArg", node->getArgument(0), node->getArgument(1), node->getArgument(2), node->getArgument(3));
	);

	ADD_Handler(builder, o2i, "oclLoadProgSource",
			this->findKernelsUsingPathString("oclLoadProgSource", node->getArgument(0), node);
			// set source string to an empty char array
			return builder.refVar(builder.literal("", builder.arrayType(BASIC.getChar())));
	);

	// TODO ignores 3rd argument (kernelName) and just adds all kernels to the program
	ADD_Handler(builder, o2i, "icl_create_kernel",
			// find kernel source code
			this->findKernelsUsingPathString("icl_create_kernel", node->getArgument(1), node);
			return builder.uintLit(0);
	);

	ADD_Handler(builder, o2i, "clEnqueueNDRangeKernel",
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

	ADD_Handler(builder, o2i, "icl_run_kernel",
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
//std::cout << "\nArg: " << arg << " " <<  isNullPtr(arg, builder) << std::endl;
						if(isNullPtr(arg, builder)) {
							// in this case arg is a local variable which has to be declared in host code
							// need to read size parameter
							ExpressionPtr size;
							TypePtr type;

							o2i.extractSizeFromSizeof(sizeArg, size, type);
							assert(size && "Unable to deduce type from clSetKernelArg call when allocating local memory: No sizeof call found, cannot translate to INSPIRE.");

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
/*
			// get argument vector
			assert(kernelArgs.find(k) != kernelArgs.end() && "Cannot find any arguments for kernel function");

			std::vector<core::ExpressionPtr> args = kernelArgs[k];
			// adding global and local size to the argument vector of original call
			// will be added to kernelArgs in 3rd pass like when using standard ocl routines
			args.push_back(node->getArgument(2) );
			args.push_back(node->getArgument(3) );
*/
			return node;
	);

	ADD_Handler(builder, o2i, "clBuildProgram",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO add flags for profiling and out of order
	ADD_Handler(builder, o2i, "clGetEventProfilingInfo",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO add syncronization means when adding asynchronous queue
	ADD_Handler(builder, o2i, "clFinish",
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(builder, o2i, "clWaitForEvents",
			// return cl_success
			return builder.intLit(0);
	);

	// need to release clMem objects
	ADD_Handler(builder, o2i, "clReleaseMemObject",
			return builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), tryRemove(BASIC.getRefDeref(), node->getArgument(0), builder));
			// updating of the type to update the deref operation in the argument done in thrid pass
	);
	ADD_Handler(builder, o2i, "icl_release_buffer",
			return builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), tryRemove(BASIC.getRefDeref(), node->getArgument(0), builder));
			// updating of the type to update the deref operation in the argument done in third pass
	);
	ADD_Handler(builder, o2i, "icl_release_buffers",
			// execute a ref.delete for each pointer in the argument list inside a compound statement
			StatementList dels;

			if(const CallExprPtr varlistPack = dynamic_pointer_cast<const CallExpr>(node->getArgument(1))) {
				if(const TupleExprPtr varlist = dynamic_pointer_cast<const TupleExpr>(varlistPack->getArgument(0))) {
					for(auto I = varlist->getExpressions().begin(); I != varlist->getExpressions().end(); ++I) {
						dels.push_back(builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), tryRemove(BASIC.getRefDeref(), *I, builder)));
					}
				}
			}
			return builder.compoundStmt(dels);
	);

	// release of kernel will be used to free the tuple holding the kernel arguments
	ADD_Handler(builder, o2i, "clReleaseKernel",
			return builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), tryRemove(BASIC.getRefDeref(), node->getArgument(0), builder));
			// updating of the type to update the deref operation in the argument done in third pass
	);

	// all other clRelease calls can be ignored since the variables are removed
	ADD_Handler(builder, o2i, "clRelease",
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(builder, o2i, "clRetain",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO implement, may have some semantic meaning
	ADD_Handler(builder, o2i, "clGetEventInfo",
			LOG(WARNING) << "Removing clGetEventInfo. Check the semantics!";
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(builder, o2i, "clFlush",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO maybe a bit too optimisitc?
	ADD_Handler(builder, o2i, "clGet",
			// return cl_success
			return builder.intLit(0);
	);

	// TODO need to add exception this when adding image support
	ADD_Handler(builder, o2i, "clCreate",
			// return cl_success
			return builder.intLit(0);
	);

	ADD_Handler(builder, o2i, "clUnloadCompiler",
			// return cl_success
			return builder.intLit(0);
	);

	// DEPRECATED, but used in the NVIDIA examples
	ADD_Handler(builder, o2i, "clSetCommandQueueProperty",
			// return cl_success
			return builder.intLit(0);
	);

	// exceptions, will be handled in a later step
	ADD_Handler(builder, o2i, "clCreateContext", return node;);
	ADD_Handler(builder, o2i, "clCreateCommandQueue", return node;);
	ADD_Handler(builder, o2i, "clCreateKernel", return node;);

	// exceptions for runtime functions, keep them as they are
	ADD_Handler(builder, o2i, "icl_init_args", return node;);
	ADD_Handler(builder, o2i, "icl_parse_args", return node;);
	ADD_Handler(builder, o2i, "icl_release_args", return node;);

	// exceptions for the icl_lib_bmp library
	ADD_Handler(builder, o2i, "icl_savebmp", return node;);
	ADD_Handler(builder, o2i, "icl_loadbmp", return node;);

	// exceptions for icl_lib power measurent
	ADD_Handler(builder, o2i, "icl_start_energy_measurement", return node;);
	ADD_Handler(builder, o2i, "icl_stop_energy_measurement", return node;);


	// handlers for insieme opencl runtime stuff
	ADD_Handler(builder, o2i, "icl_",
		return builder.literal(node->getType(), "0"); // default handling, remove it
	);
};

HandlerPtr& HostMapper::findHandler(const string& fctName) {
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

CallExprPtr HostMapper::checkAssignment(const core::NodePtr& oldCall) {
	CallExprPtr newCall;
	if ((newCall = dynamic_pointer_cast<const CallExpr> (oldCall))) {
		if(newCall->getArguments().size() < 2)
			return newCall;
		// get rid of deref operations, automatically inserted by the frontend coz _cl_mem* is translated to ref<array<...>>, and refs cannot be
		// rhs of an assignment
		if (const CallExprPtr rhs = dynamic_pointer_cast<const CallExpr>(newCall->getArgument(1))) {
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
	if (const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(flagExpr)) {
		// check if there is an lshift -> flag reached
		if (call->getFunctionExpr() == BASIC.getSignedIntLShift() || call->getFunctionExpr() == BASIC.getUnsignedIntLShift()) {
			if (const LiteralPtr flagLit = dynamic_pointer_cast<const Literal>(call->getArgument(1))) {
				int flag = flagLit->getValueAs<int>();
				if (flag < Enum::size) // last field of enum to be used must be size
					flags.insert(Enum(flag));
				else
					LOG(ERROR) << "Flag " << flag << " is out of range. Max value is " << CreateBufferFlags::size - 1;
			}
		} else if (call->getFunctionExpr() == BASIC.getSignedIntOr() || call->getFunctionExpr() == BASIC.getUnsignedIntOr()) {
			// two flags are ored, search flags in the arguments
			recursiveFlagCheck(call->getArgument(0), flags);
			recursiveFlagCheck(call->getArgument(1), flags);
		} else
			LOG(ERROR) << "Unexpected operation in flag argument: " << call << "\nUnable to deduce flags, using default settings";

	}
}

template<typename Enum>
std::set<Enum> HostMapper::getFlags(const ExpressionPtr& flagExpr) {


	const core::lang::BasicGenerator& gen = builder.getLangBasic();
	std::set<Enum> flags;
	// remove cast to uint<8>
	if (flagExpr.isa<core::CallExprPtr>() && gen.isScalarCast(flagExpr.as<core::CallExprPtr>()->getFunctionExpr())){
		recursiveFlagCheck(flagExpr.as<core::CallExprPtr>()->getArgument(0), flags);
	}else
	if (const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(flagExpr)) {
		recursiveFlagCheck(cast->getSubExpression(), flags);
	} else
		LOG(ERROR) << "No flags found in " << flagExpr << "\nUsing default settings";

	return flags;
}

bool HostMapper::handleClCreateKernel(const core::ExpressionPtr& expr, const ExpressionPtr& call, const ExpressionPtr& fieldName) {
	if(call->getType()->toString().find("array<_cl_kernel,1>") == string::npos &&
			call->getType()->toString().find("struct<kernel:ref<array<_cl_kernel,1>>") == string::npos)
		return false; //TODO untested

	TypePtr type = getNonRefType(expr);

	// if it is a struct we have to check the field
	if (const StructTypePtr st = dynamic_pointer_cast<const StructType>(getBaseType(type))) {
		//TODO if one puts more than one kernel inside a struct (s)he should be hit in the face
		if (fieldName) {
			if (const LiteralPtr fieldLit = dynamic_pointer_cast<const Literal>(fieldName)) {
				for_each(st->getEntries(), [&](const NamedTypePtr& field) {
					if(field->getName()->getValue() == fieldLit->getStringValue())
					type = field->getType();
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

	if(type->toString().find("ref<array<_cl_kernel,1>>") != string::npos) {
		if(const CallExprPtr newCall = dynamic_pointer_cast<const CallExpr>(tryRemove(BASIC.getRefVar(), call, builder))) {//!
			if(const LiteralPtr fun = dynamic_pointer_cast<const Literal>(newCall->getFunctionExpr())) {
				if(fun->getStringValue() == "icl_create_kernel" ) {
					// call resolve element to load the kernel using the appropriate handler
					resolveElement(newCall);
					ExpressionPtr kn = newCall->getArgument(2);
					// usually kernel name is embedded in a "ref.vector.to.ref.array" call"
					if(const CallExprPtr sacp = dynamic_pointer_cast<const CallExpr>(kn))
						kn = sacp->getArgument(0);
					if(const LiteralPtr kl = dynamic_pointer_cast<const Literal>(kn)) {
							string name = kl->getStringValue().substr(1, kl->getStringValue().length()-2); // delete quotation marks form name
							kernelNames[name] = expr;
					}

					return true;
				}
			}
		}
	}

	if(type->toString().find("array<_cl_kernel,1>") != string::npos) {
		if(const CallExprPtr newCall = dynamic_pointer_cast<const CallExpr>(tryRemove(BASIC.getRefVar(), call, builder))) {//!
			if(const LiteralPtr fun = dynamic_pointer_cast<const Literal>(newCall->getFunctionExpr())) {
				if(fun->getStringValue() == "clCreateKernel" ) {
						ExpressionPtr kn = newCall->getArgument(1);
						// usually kernel name is embedded in a "ref.vector.to.ref.array" call"
						if(const CallExprPtr sacp = dynamic_pointer_cast<const CallExpr>(kn))
						kn = sacp->getArgument(0);
						if(const LiteralPtr kl = dynamic_pointer_cast<const Literal>(kn)) {
								string name = kl->getStringValue().substr(1, kl->getStringValue().length()-2); // delete quotation marks form name
								kernelNames[name] = expr;
						}

						return true;
				}
			}
		}
	}
	return false;
}

bool HostMapper::lookForKernelFilePragma(const core::TypePtr& type, const core::ExpressionPtr& createProgramWithSource) {
	if(type == POINTER(builder.genericType("_cl_program"))) {
		if(CallExprPtr cpwsCall = dynamic_pointer_cast<const CallExpr>(tryRemoveAlloc(createProgramWithSource, builder))) {
			if(annotations::ocl::KernelFileAnnotationPtr kfa = dynamic_pointer_cast<annotations::ocl::KernelFileAnnotation>
					(createProgramWithSource->getAnnotation(annotations::ocl::KernelFileAnnotation::KEY))) {
				const string& path = kfa->getKernelPath();
				if(cpwsCall->getFunctionExpr() == BASIC.getRefDeref() && cpwsCall->getArgument(0)->getNodeType() == NT_CallExpr)
					cpwsCall = dynamic_pointer_cast<const CallExpr>(cpwsCall->getArgument(0));
				if(const LiteralPtr clCPWS = dynamic_pointer_cast<const Literal>(cpwsCall->getFunctionExpr())) {
					if(clCPWS->getStringValue() == "clCreateProgramWithSource") {
						// check if file has already been added
						if(kernelFileCache.find(path) == kernelFileCache.end()) {
							kernelFileCache.insert(path);
							const ProgramPtr kernels = loadKernelsFromFile(path, builder, job);
							for_each(kernels->getEntryPoints(), [&](ExpressionPtr kernel) {
									kernelEntries.push_back(kernel);
							});
						}
					}
				}
			}
		}
		return true;
	}
	return false;
}

const NodePtr HostMapper::handleCreateBufferAssignment(const VariablePtr& lhsVar, const CallExprPtr& callExpr) {
	NodePtr createBuffer = callExpr->substitute(builder.getNodeManager(), *this);
	bool alreadyThere = cl_mems.find(lhsVar) != cl_mems.end();
	// check if data has to be copied to a new array
	if(const CallExprPtr newCall = checkAssignment(createBuffer)) {

		// exchange the _cl_mem type with the new type, gathered from the clCreateBuffer call
		TypePtr newType = static_pointer_cast<const Type>(core::transform::replaceAll(builder.getNodeManager(), lhsVar->getType(),
				callExpr->getArgument(1)->getType(), newCall->getType()));
		// check if variable has already been put into replacement map with a different type
		if(alreadyThere) {
			assert((cl_mems[lhsVar]->getType() == newType) && "cl_mem variable allocated several times with different types.");
		}

		NodePtr ret;

		if(const VariablePtr var = dynamic_pointer_cast<const Variable>(getVarOutOfCrazyInspireConstruct(newCall, builder))) {
			// use the host variable because CL_MEM_USE_HOST_PTR was set
			cl_mems[lhsVar] = var;
			// TODO check if err argument has been passed and set variable to 0
			assert(false);
			ret = builder.getNoOp();
		} else {
			if(!alreadyThere) {
				const VariablePtr newVar = builder.variable(newType);
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
	}
	return builder.getNoOp();
}

const NodePtr HostMapper::handleCreateBufferDecl(const VariablePtr& var, const ExpressionPtr& initFct, const DeclarationStmtPtr& decl) {
	const CallExprPtr newInit = dynamic_pointer_cast<const CallExpr>(this->resolveElement(initFct));

	// check if the variable was created with CL_MEM_USE_HOST_PTR flag and can be removed
	if(const VariablePtr replacement = dynamic_pointer_cast<const Variable>(getVarOutOfCrazyInspireConstruct(newInit, builder))) {
		cl_mems[var] = replacement; // TODO check if error argument has been set and set error to CL_SUCCESS
		return builder.getNoOp();
	}

	//DeclarationStmtPtr newDecl = dynamic_pointer_cast<const DeclarationStmt>(decl->substitute(builder.getNodeManager(), *this));
	TypePtr newType = builder.refType(newInit->getType());

	NodePtr newDecl = builder.declarationStmt(var, builder.refVar(newInit));
	const VariablePtr newVar = builder.variable(newType);
	cl_mems[var] = newVar;

	copyAnnotations(decl, newDecl);
	return newDecl;
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
	 std::cout << "MarkerStmt: " << marker << std::endl;
	 }*/

	if(const CallExprPtr callExpr = dynamic_pointer_cast<const CallExpr>(element)) {
		const ExpressionPtr fun = callExpr->getFunctionExpr();

		if(const LiteralPtr literal = dynamic_pointer_cast<const Literal>(fun)) {
//std::cout << "\nTry to handle " << literal->getValue();
			//			callExpr->substitute(builder.getNodeManager(), *this);
			if(const HandlerPtr& replacement = findHandler(literal->getStringValue())) {
				NodePtr ret = replacement->handleNode(callExpr);
//std::cout << "\nHandling fct " << literal->getValue() << " -> " << ret << std::endl;
				// check if new kernels have been created
				const vector<ExpressionPtr>& kernels = replacement->getKernels();
				if(kernels.size() > 0) {
					for_each(kernels, [&](ExpressionPtr kernel) {
							kernelEntries.push_back(kernel);
						});
					replacement->resetKernels();
				}
				copyAnnotations(callExpr, ret);
				return ret;
			}
		} else {
			// add all variables used as arguments and the corresponding parameters to the equivalence map
			LambdaExprPtr lambda = dynamic_pointer_cast<const LambdaExpr>(callExpr->getFunctionExpr());
//std::cout << "WRiting to the eqMap\ntype " << lambda->getType()->toString() << std::endl;
			if(lambda && lambda->getType()->toString().find("array<_cl_kernel,1>") != string::npos) { // TODO may extend it to other types
//std::cout << "found cl_kernel\n";
				auto paramIt = lambda->getParameterList().begin();
				for_each(callExpr->getArguments(), [&](ExpressionPtr arg) {
					if(const VariablePtr var = dynamic_pointer_cast<const Variable>(arg)) {
//std::cout << "found a variable\n";
						if(var->getType()->toString().find("array<_cl_kernel,1>") != string::npos) {
//std::cout << "Variable " << callExpr << " has the right type\n";
							if(eqMap.find(var) != eqMap.end())
								eqMap[*paramIt] = eqMap[var];
							else {
								eqMap[var] = eqIdx;
								eqMap[*paramIt] = eqIdx++;
							}
						}
					}
					if(paramIt != lambda->getParameterList().end()) // should always be true, only for security reasons
						++paramIt;
					else
						assert(false && "This parameter is unexpecetdly not inside the lambdas parameter list");
				});
			}
		}

		if(fun == BASIC.getRefAssign()) {
			ExpressionPtr lhs = callExpr->getArgument(0);

			// if this is loading a source file and assigning it to some target => (implicitly) register and drop it
			if(lookForKernelFilePragma(lhs->getType(), callExpr->getArgument(1))) {
				return builder.getNoOp();
			}

			// on the left hand side we'll either have a variable or a struct, probably holding a reference to the global array
			// for the latter case we have to handle it differently
			if(const CallExprPtr& lhsCall = dynamic_pointer_cast<const CallExpr>(lhs)) {
				if(lhsCall->getFunctionExpr() == BASIC.getCompositeRefElem()) {
					if(const CallExprPtr& newCall = checkAssignment(callExpr->substitute(builder.getNodeManager(), *this))) {

						if(lhsCall->getType()->toString().find("_cl_mem") != string::npos) {

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
							StringValuePtr toChange = builder.stringValue(id->getStringValue());
							StructTypePtr structType = dynamic_pointer_cast<const StructType>(getNonRefType(newStruct));
							StructType::Entries entries = structType->getEntries(); // actual fields of the struct
							StructType::Entries newEntries;

							// check if CL_MEM_USE_HOST_PTR flag was set and host_ptr was a global variable
							bool useHostPtr = false;
							if(const CallExprPtr creToHostField = dynamic_pointer_cast<const CallExpr>(tryRemove(BASIC.getRefDeref(),
									newCall->getArgument(0), builder))) {
								if(creToHostField->getFunctionExpr() == BASIC.getCompositeRefElem()) {
									// in this case replace the identifier of the cl_mem object with the one of the host_ptr
									useHostPtr = true;
									replacements[lhsCall->getArgument(1)] = creToHostField->getArgument(1);
								} else {
									assert(false && "If a cl_mem variable at global scope/in a struct uses the CL_MEM_USE_HOST_PTR flag at create buffer, the \
											host_ptr must be in the global scope/the same struct too");}
							}

							for_each(entries, [&](const NamedTypePtr& entry) {
									if(*entry->getName() == *toChange) {
										if(!useHostPtr)
											newEntries.push_back(builder.namedType(entry->getName(), newType));
									} else {
										newEntries.push_back(entry);
									}
								});

							// update struct in replacement map
							NodePtr replacement = builder.variable(builder.refType(builder.structType(newEntries)));

							copyAnnotations(struct_, replacement);

							cl_mems[struct_] = dynamic_pointer_cast<const Variable>(replacement);

							if(useHostPtr)
								return builder.getNoOp();

							const CallExprPtr structAccess = builder.callExpr(builder.refType(newType), BASIC.getCompositeRefElem(), struct_,
									lhsCall->getArgument(1), builder.getTypeLiteral(newType));
							return builder.callExpr(BASIC.getRefAssign(), structAccess, newCall);
						}

						if(lhsCall->getType()->toString().find("array<_cl_kernel,1>") != string::npos ) {
								if(lhsCall->getFunctionExpr() == BASIC.getCompositeRefElem())
//									if(dynamic_pointer_cast<const Variable>(lhsCall->getArgument(0))){
										if(handleClCreateKernel(lhsCall, callExpr->getArgument(1), lhsCall->getArgument(1)))
											return builder.getNoOp();
//									}
						}
					}

					if(lookForKernelFilePragma(lhsCall->getType(), callExpr->getArgument(1))) {
						return builder.getNoOp();
					}
				}
				// also if the variable is an array we will have a function call on the left hand side
				if(BASIC.isSubscriptOperator(lhsCall->getFunctionExpr())) {
					//prepare stuff for VariablePtr section
					lhs = getVariableArg(lhsCall, builder);
				}
			}

			if(const VariablePtr lhsVar = dynamic_pointer_cast<const Variable>(lhs)) {
				// handling clCreateBuffer
				if(lhsVar->getType()->toString().find("array<_cl_mem,1>") != string::npos) {
					if(callExpr->getArgument(1)->toString().find("clCreateBuffer") != string::npos){
						return handleCreateBufferAssignment(lhsVar, callExpr);
					}
				}

				// handling clCreateBuffer
				if(lhsVar->getType()->toString().find("ref<array<struct<mem:ref<array<_cl_mem,1>>,size:uint<8>") != string::npos){
					if(callExpr->getArgument(1)->toString().find("icl_create_buffer") != string::npos){
						return handleCreateBufferAssignment(lhsVar, callExpr);
					}
				}

				if(const CallExprPtr newCall = checkAssignment(callExpr)) {
					if(handleClCreateKernel(callExpr->getArgument(0), newCall, NULL)) {
						return builder.getNoOp();
					}
/* not needed any more since replace of clSetKernelArg with an Inpire function
						if(callExpr->getArgument(1)->toString().find("clSetKernelArg") != string::npos){
							std::vector<StatementPtr> stmts;
							// set error value to CL_SUCCESS
							stmts.push_back(builder.callExpr(BASIC.getUnit(), BASIC.getRefAssign(), lhs,
									builder.literal(tryDeref(lhs, builder)->getType(), "0")));
							// set tuple member to argument
							stmts.push_back(newCall);
							return builder.compoundStmt(stmts);
						}*/
				}

				if(lookForKernelFilePragma(lhsVar->getType(), callExpr->getArgument(1))) {
					return builder.getNoOp();
				}
			}

		}
	}

	if(const DeclarationStmtPtr decl = dynamic_pointer_cast<const DeclarationStmt>(element)) {
		const VariablePtr var = decl->getVariable();

		if(var->getType() == POINTER(builder.genericType("_cl_mem"))) {
			if(const CallExprPtr initFct = dynamic_pointer_cast<const CallExpr>(tryRemove(BASIC.getRefVar(), decl->getInitialization(), builder))) {
				if(const LiteralPtr literal = core::dynamic_pointer_cast<const core::Literal>(initFct->getFunctionExpr())) {
					if(literal->getStringValue() == "clCreateBuffer") { // clCreateBuffer is called at definition of cl_mem variable
						return handleCreateBufferDecl(var, initFct, decl);
					}
				}
			}
			assert(decl->getInitialization()->getNodeType() == NT_CallExpr && "Unexpected initialization of cl_mem variable");
		}

		if(var->getType()->toString().find("ref<ref<array<struct<mem:ref<array<_cl_mem,1>>,size:uint<8>") != string::npos){
			if(const CallExprPtr initFct = dynamic_pointer_cast<const CallExpr>(tryRemove(BASIC.getRefVar(), decl->getInitialization(), builder))) {
				if(const LiteralPtr literal = dynamic_pointer_cast<const core::Literal>(initFct->getFunctionExpr())) {
					if(literal->getStringValue() == "icl_create_buffer") {
						return handleCreateBufferDecl(var, initFct, decl);
					}
				}
			}
			assert(decl->getInitialization()->getNodeType() == NT_CallExpr && "Unexpected initialization of cl_mem variable");
		}

		lookForKernelFilePragma(var->getType(), decl->getInitialization());

		if(handleClCreateKernel(var, decl->getInitialization(), NULL)) {
			return decl;
		}

		if(dynamic_pointer_cast<const StructType>(getNonRefType(var))) {
			if(var->getType()->toString().find("array<_cl_") != string::npos) {
				// if the structure contains some cl_ variables, add it to cl_mem map so that it will be cleaned in second pass
				cl_mems[var] = var;
			}
		}
	}

	NodePtr ret = element->substitute(builder.getNodeManager(), *this);
	copyAnnotations(element, ret);
	return ret;
}

} //namespace ocl
} //namespace frontend
} //namespace insieme
