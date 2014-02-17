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
#include "insieme/frontend/frontend.h"
#include "insieme/frontend/ocl/ocl_host_passes.h"

namespace insieme {
namespace frontend {
namespace ocl {


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
			const core::NodePtr& stopAt, core::ProgramPtr prog, core::IRBuilder builder) :
		IRVisitor<bool> (false), pathToKernelFile(lookFor),
				breakingStmt(stopAt), program(prog), builder(builder) { }
	string getKernelFilePath() {
		return path;
	}
};

/**
 * This struct holds inspire representations of OpenCL built-in host functions
 */
struct Ocl2Inspire {
private:

public:
	Ocl2Inspire() { }

	bool extractSizeFromSizeof(const core::ExpressionPtr& arg,
			core::ExpressionPtr& size, core::TypePtr& type, bool foundMul = false);

	core::ExpressionPtr getClCreateBuffer(bool copyHostPtr, bool setErrcodeRet, core::IRBuilder builder);
	core::ExpressionPtr getClCopyBuffer(core::IRBuilder builder);
	core::ExpressionPtr getClCopyBufferFallback(core::IRBuilder builder);
	core::ExpressionPtr getClWriteBuffer(core::IRBuilder builder);
	core::ExpressionPtr getClWriteBufferFallback(core::IRBuilder builder);
	core::ExpressionPtr getClReadBuffer(core::IRBuilder builder);
	core::ExpressionPtr getClReadBufferFallback(core::IRBuilder builder);
	core::ExpressionPtr getClGetIDs(core::IRBuilder builder);
	core::ExpressionPtr getClSetKernelArg(core::IRBuilder builder);
};

/**
 * This class allows replaces a call to an OpenCL built-in function to an INSPIRE one
 */
class Handler {
protected:
	core::ProgramPtr kernels;
	Ocl2Inspire o2i;
	size_t argCnt;

	// set to store paths of loaded kernel files
	std::set<string>& kernelFileCache;

public:
	Handler(Ocl2Inspire& ocl2inspire, std::set<string>& kernelFileCache)
		: o2i(ocl2inspire), argCnt(0), kernelFileCache(kernelFileCache){
		kernels = NULL;
	}

	virtual core::NodePtr handleNode(core::CallExprPtr node) =0;

	const vector<core::ExpressionPtr> getKernels() { return kernels->getEntryPoints();	}

	void resetKernels() { kernels = NULL; }

	// check if expression is a string conatining a path to a kernel file
	// if yes, load and compile these kernels and add them to the appropriate fields
	void findKernelsUsingPathString(const core::ExpressionPtr& path, const core::ExpressionPtr& root, const core::ProgramPtr& mProgram);

	// return an INSPIRE equivalent of clCreateBuffer/icl_create_buffer
//	const core::ExpressionPtr getCreateBuffer(const core::ExpressionPtr& flags, const core::ExpressionPtr& sizeArg,
//			const bool copyPtr, const core::ExpressionPtr& hostPtr, const core::ExpressionPtr& errcode_ret);

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
	LambdaHandler(Ocl2Inspire& ocl2inspire, const char* fun, std::set<string>& kernelFileCache, Lambda lambda) :
		Handler(ocl2inspire, kernelFileCache), fct(fun), body(lambda) {
	}

	// creating a shared pointer to a LambdaHandler

	core::NodePtr handleNode(core::CallExprPtr node) {
		core::NodeManager& mgr = node.getNodeManager();
		core::IRBuilder builder(mgr);

//		LOG(DEBUG) << "Handling node " << node << std::endl;

		return body(node, builder, kernels);
	}

};


typedef std::shared_ptr<Handler> HandlerPtr;
typedef boost::unordered_map<string, HandlerPtr, boost::hash<string> > HandlerTable;

template<typename Lambda>
HandlerPtr make_handler(Ocl2Inspire& o2i, const char* fct, std::set<string> kernelFileCache, Lambda lambda) {
	return std::make_shared<LambdaHandler<Lambda> >(o2i, fct, kernelFileCache, lambda);
}

#define ADD_Handler(o2i, fct, BODY) \
    handles.insert(std::make_pair(fct, make_handler(o2i, fct, kernelFileCache, \
    		[&](core::CallExprPtr node, core::IRBuilder builder, core::ProgramPtr& kernels){ BODY }))).second;

class OclSimpleFunHandler: public core::transform::CachedNodeMapping {
	ocl::HandlerTable handles;
	ocl::Ocl2Inspire o2i;
	// set to store paths of loaded kernel files
	std::set<string> kernelFileCache;

	ocl::HandlerPtr& findHandler(const string& fctName);

public:
	OclSimpleFunHandler();

	const core::NodePtr resolveElement(const core::NodePtr& ptr);
};

} //namespace ocl
} //namespace frontend
} //namespace insieme
