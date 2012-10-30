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

#pragma once

#include "insieme/utils/logging.h"
#include "insieme/frontend/ocl/ocl_host_passes.h"

namespace insieme {
namespace frontend {
namespace ocl {


// enums corresponding to the flags in clCreateBuffer
enum CreateBufferFlags {
		CL_MEM_READ_WRITE = 0,
		CL_MEM_WRITE_ONLY,
		CL_MEM_READ_ONLY,
		CL_MEM_USE_HOST_PTR,
		CL_MEM_ALLOC_HOST_PTR,
		CL_MEM_COPY_HOST_PTR,
		size
};

/**
 * Class to visit the AST and return the value of a certain variable, holding the path to a OpenCL kernel, if it exists at all
 */
class KernelCodeRetriver: public core::IRVisitor<bool> {
	const core::ExpressionPtr& pathToKernelFile; // Variable to look for
	const core::NodePtr& breakingStmt; // place where the path would be needed, can stop searching there
	const core::ProgramPtr program;
	const core::IRBuilder builder;
	string path;

	bool visitNode(const core::NodePtr& node);
	bool visitCallExpr(const core::CallExprPtr& callExpr);
	bool visitDeclarationStmt(const core::DeclarationStmtPtr& decl);

	bool saveString(const core::LiteralPtr& lit);
	bool saveString(const core::CallExprPtr& call);
public:
	KernelCodeRetriver(const core::ExpressionPtr lookFor,
			const core::NodePtr& stopAt, core::ProgramPtr prog, core::IRBuilder build) :
		IRVisitor<bool> (false), pathToKernelFile(lookFor),
				breakingStmt(stopAt), program(prog), builder(build) { }
	string getKernelFilePath() {
		return path;
	}
};

/**
 * This struct holds inspire representations of OpenCL built-in host functions
 */
struct Ocl2Inspire {
private:
	core::IRBuilder& builder;

public:
	Ocl2Inspire(core::IRBuilder& build) :
		builder(build) { }

	bool extractSizeFromSizeof(const core::ExpressionPtr& arg,
			core::ExpressionPtr& size, core::TypePtr& type, bool foundMul = false);

	core::ExpressionPtr getClCreateBuffer(bool copyHostPtr, bool setErrcodeRet);
	core::ExpressionPtr getClCopyBuffer();
	core::ExpressionPtr getClCopyBufferFallback();
	core::ExpressionPtr getClWriteBuffer();
	core::ExpressionPtr getClWriteBufferFallback();
	core::ExpressionPtr getClReadBuffer();
	core::ExpressionPtr getClReadBufferFallback();
	core::ExpressionPtr getClGetIDs();
	core::ExpressionPtr getClSetKernelArg();
};

/**
 * This class allows replaces a call to an OpenCL built-in function to an INSPIRE one
 */
class Handler {
protected:
	core::ProgramPtr kernels;
	const core::IRBuilder& builder;
	Ocl2Inspire o2i;
	size_t argCnt;

	// set to store paths of loaded kernel files
	std::set<string>& kernelFileCache;

public:
	Handler(const core::IRBuilder& build, Ocl2Inspire& ocl2inspire, std::set<string>& kernelFileCache)
		: builder(build), o2i(ocl2inspire), argCnt(0), kernelFileCache(kernelFileCache) {
		kernels = core::Program::get(build.getNodeManager());
	}

	virtual core::NodePtr handleNode(core::CallExprPtr node) =0;

	const vector<core::ExpressionPtr> getKernels() { return kernels->getEntryPoints();	}

	void resetKernels() { kernels = core::Program::get(builder.getNodeManager()); }

	// check if expression is a string conatining a path to a kernel file
	// if yes, load and compile these kernels and add them to the appropriate fields
	void findKernelsUsingPathString(const core::ExpressionPtr& path, const core::ExpressionPtr& root, const core::ProgramPtr& mProgram);

	// return an INSPIRE equivalent of clCreateBuffer/icl_create_buffer
	const core::ExpressionPtr getCreateBuffer(const core::ExpressionPtr& flags, const core::ExpressionPtr& sizeArg,
			const bool copyPtr, const core::ExpressionPtr& hostPtr, const core::ExpressionPtr& errcode_ret);

	// puts the passed argument in the right place inside the kernelArgs map
	const core::ExpressionPtr collectArgument(const core::ExpressionPtr& kernelArg, const core::ExpressionPtr& index, const core::ExpressionPtr& sizeArg,
			core::ExpressionPtr arg, KernelArgs& kernelArgs, LocalMemDecls& localMemDecls, ClmemTable& cl_mems, EquivalenceMap& eqMap);
};

/*
 * Provides templated child functions of the Handler class. The template argument determines which
 * OpenCL built-in funciton is handled
 */
template<typename Lambda>
class LambdaHandler: public Handler {
	// flag indicating if the definition of the actual function has already been added to the program
	static bool defAdded;

	const char* fct;
	Lambda body;
public:
	LambdaHandler(core::IRBuilder& build, Ocl2Inspire& ocl2inspire, const char* fun, std::set<string>& kernelFileCache, Lambda lambda) :
		Handler(build, ocl2inspire, kernelFileCache), fct(fun), body(lambda) {
	}

	// creating a shared pointer to a LambdaHandler

	core::NodePtr handleNode(core::CallExprPtr node) {
//		LOG(DEBUG) << "Handling node " << node << std::endl;

		return body(node, kernels);
	}

};


typedef std::shared_ptr<Handler> HandlerPtr;
typedef boost::unordered_map<string, HandlerPtr, boost::hash<string> > HandlerTable;

template<typename Lambda>
HandlerPtr make_handler(core::IRBuilder& builder, Ocl2Inspire& o2i, const char* fct, std::set<string> kernelFileCache,
		Lambda lambda) {
	return std::make_shared<LambdaHandler<Lambda> >(builder, o2i, fct, kernelFileCache, lambda);
}

#define ADD_Handler(builder, o2i, fct, BODY) \
    handles.insert(std::make_pair(fct, make_handler(builder, o2i, fct, kernelFileCache, [&](core::CallExprPtr node, core::ProgramPtr& kernels){ BODY }))).second;

/*
 * First pass when translating a program OpenCL to IR
 * Responsible for:
 * - finding path to kernel file (in-code kernel code strings not supported yet)
 * - find cl_mem variable replacements and store them in the cl_mems map
 * - store names of called kernels in kernelNames
 * - store arguments of kernels in kernelArgs (the kernel name is the key)
 * - translate kernel code to IR
 * - store all entry points to kernels in kernelEntries
 * - call Ocl2Inspire's functions when encountering a function which needs IR replacement (e.g. clEnqueueWriteBuffer)
 * No out of order queue supported yet
 */
class HostMapper: public core::transform::CachedNodeMapping {
	core::IRBuilder& builder;

	HandlerTable handles;
	ClmemTable cl_mems;
	Ocl2Inspire o2i;
	core::ProgramPtr& mProgram;
	KernelArgs kernelArgs;
	KernelNames kernelNames;
	vector<core::ExpressionPtr> kernelEntries;
	LocalMemDecls localMemDecls;
	insieme::utils::map::PointerMap<core::NodePtr, core::NodePtr> replacements;
	EquivalenceMap eqMap;
	size_t eqIdx;

	// check if the call is a call to ref.assign
	core::CallExprPtr checkAssignment(const core::NodePtr& oldCall);

	// needed to be able to work with handlers, identified by prefixes of function names
	HandlerPtr& findHandler(const string& fctName);

	// set to store paths of loaded kernel files
	std::set<string> kernelFileCache;

	// functions to get set of flags out of an Expression
	// recursiveFlagCheck is designed to be called form getFlags only
	template<typename Enum>
	void recursiveFlagCheck(const core::ExpressionPtr& flagExpr, std::set<Enum>& flags);
	template<typename Enum>
	std::set<Enum> getFlags(const core::ExpressionPtr& flagExpr);

	bool translateClCreateBuffer(const core::ExpressionPtr& var, const core::CallExprPtr& fun, const core::CallExprPtr& newRhs, core::NodePtr& ret);
	bool handleClCreateKernel(const core::ExpressionPtr& expr, const core::ExpressionPtr& call, const core::ExpressionPtr& fieldName);
	bool lookForKernelFilePragma(const core::TypePtr& type, const core::ExpressionPtr& createProgramWithSource);

	// handles create buffer at the rhs of assignments
	const core::NodePtr handleCreateBufferAssignment(const core::VariablePtr& lhsVar, const core::CallExprPtr& callExpr);
	// handles create buffer at the init expression of declarations
	const core::NodePtr handleCreateBufferDecl(const core::VariablePtr& var, const core::ExpressionPtr& initFct, const core::DeclarationStmtPtr& decl);
	// needed to add one level of indirection because calling the hander member function seems to be impossible form the ADD_Handler macro
	void findKernelsUsingPathString(const string& handleName, const core::ExpressionPtr& path, const core::ExpressionPtr& root) {
		handles[handleName]->findKernelsUsingPathString(path, root, mProgram);
	}
	core::ExpressionPtr getCreateBuffer(const string& handleName, const core::ExpressionPtr& flags, const core::ExpressionPtr& sizeArg, const bool copyPtr,
			const core::ExpressionPtr& hostPtr, const core::ExpressionPtr& errcode_ret) {
		return handles[handleName]->getCreateBuffer(flags, sizeArg, copyPtr, hostPtr, errcode_ret);
	}
	const core::ExpressionPtr collectArgument(const string& handleName, const core::ExpressionPtr& kernel, const core::ExpressionPtr& index,
			const core::ExpressionPtr& sizeArg, const core::ExpressionPtr& arg) {
		return handles[handleName]->collectArgument(kernel, index, sizeArg, arg, kernelArgs, localMemDecls, cl_mems, eqMap);
	}
public:
	HostMapper(core::IRBuilder& build, core::ProgramPtr& program);

	const core::NodePtr resolveElement(const core::NodePtr& element);

	ClmemTable& getClMemMapping() { return cl_mems; }
	const vector<core::ExpressionPtr>& getKernels() { return kernelEntries;	}
	const size_t getnKernels() { return kernelEntries.size(); }
	KernelArgs& getKernelArgs() { return kernelArgs; }
	KernelNames& getKernelNames() { return kernelNames; }
	LocalMemDecls& getLocalMemDecls() {	return localMemDecls; }
	EquivalenceMap& getEquivalenceMap() { return eqMap; }
	core::NodeMap& getReplacements() { return replacements; }
};

} //namespace ocl
} //namespace frontend
} //namespace insieme
