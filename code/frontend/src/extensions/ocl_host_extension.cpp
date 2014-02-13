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

#include "insieme/frontend/extensions/ocl_host_extension.h"
#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/frontend/utils/error_report.h"
#include "insieme/frontend/ocl/ocl_host_replace_buffers.h"
#include "insieme/frontend/ocl/ocl_type_fixer.h"
#include "insieme/frontend/ocl/ocl_host_utils.h"
#include "insieme/frontend/ocl/ocl_host_handler.h"

namespace fe = insieme::frontend;

using namespace insieme::frontend;

namespace insieme {
namespace frontend {
namespace extensions {

using namespace insieme::core;
using namespace insieme::frontend::ocl;

OclHostPlugin::OclHostPlugin() {
	// TODO at the moment there will always be one platform and one device, change that!
	ADD_Handler(o2i, "clGetDeviceIDs",
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

			ocl::NullLitSearcher nls(builder);
			ExpressionPtr ret;
			if(visitDepthFirstInterruptible(node->getArgument(4), nls))
				ret = builder.intLit(0);
			else
				ret = builder.callExpr(o2i.getClGetIDs(IRBuilder(node->getNodeManager())), node->getArgument(4));
			return ret;
	);

	ADD_Handler(o2i, "clGetPlatformIDs",
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

			ocl::NullLitSearcher nls(builder);
			ExpressionPtr ret;

			if(visitDepthFirstInterruptible(node->getArgument(2), nls))
				ret = builder.intLit(0);
			else
				ret = builder.callExpr(o2i.getClGetIDs(IRBuilder(node->getNodeManager())), node->getArgument(2));
			return ret;
	);

	ADD_Handler(o2i, "icl_get_num_devices",
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

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

	ADD_Handler(o2i, "icl_create_buffer",
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

			// Flags can be ignored, INSPIRE is always blocking
			return getCreateBuffer("icl_create_buffer", node->getArgument(1), node->getArgument(2), false, builder.intLit(0),
					mgr.getLangBasic().getRefNull());
	);
*/
	ADD_Handler(o2i, "clEnqueueCopyBuffer",
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

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
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

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
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

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
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

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
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

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
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

			this->findKernelsUsingPathString("oclLoadProgSource", node->getArgument(0), node);
			// set source string to an empty char array
			return builder.refVar(builder.literal("", builder.arrayType(BASIC.getChar())));
	);

	// TODO ignores 3rd argument (kernelName) and just adds all kernels to the program
	ADD_Handler(o2i, "icl_create_kernel",
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

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
			return builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), tryRemove(BASIC.getRefDeref(), node->getArgument(0), builder));
			// updating of the type to update the deref operation in the argument done in thrid pass
	);
	ADD_Handler(o2i, "icl_release_buffer",
			return builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), tryRemove(BASIC.getRefDeref(), node->getArgument(0), builder));
			// updating of the type to update the deref operation in the argument done in third pass
	);
	ADD_Handler(o2i, "icl_release_buffers",
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
	ADD_Handler(o2i, "clReleaseKernel",
			return builder.callExpr(BASIC.getUnit(), BASIC.getRefDelete(), tryRemove(BASIC.getRefDeref(), node->getArgument(0), builder));
			// updating of the type to update the deref operation in the argument done in third pass
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


	// handlers for insieme opencl runtime stuff
	ADD_Handler(o2i, "icl_",
		return builder.literal(node->getType(), "0"); // default handling, remove it
	);
*/
}

core::ProgramPtr OclHostPlugin::IRVisit(insieme::core::ProgramPtr& prog) {
	ocl::BufferReplacer br(prog);
	ocl::TypeFixer otf;
	core::NodePtr root = otf.mapElement(0, br.getTransformedProgram());

	core::IRBuilder builder(prog->getNodeManager());
	core::ExpressionList list;
	list.push_back(root.as<core::ExpressionPtr>());
//	prog = builder.program(list);

	return prog;
}

} //namespace plugin
} //namespace frontend
} //namespace extensions
