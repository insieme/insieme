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

#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/parser/ir_parse.h"

#include "insieme/frontend/program.h"
#include "insieme/utils/logging.h"

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
class KernelCodeRetriver: public core::ASTVisitor<bool> {
	const core::VariablePtr& pathToKernelFile; // Variable to look for
	const core::NodePtr& breakingStmt; // place where the path would be needed, can stop searching there
	const core::ASTBuilder builder;
	string path;

	bool visitNode(const core::NodePtr& node);
	bool visitCallExpr(const core::CallExprPtr& callExpr);
	bool visitDeclarationStmt(const core::DeclarationStmtPtr& decl);

public:
	KernelCodeRetriver(const core::VariablePtr lookFor,
			const core::NodePtr& stopAt, core::ASTBuilder build) :
		ASTVisitor<bool> (false), pathToKernelFile(lookFor),
				breakingStmt(stopAt), builder(build) {
	}
	string getKernelFilePath() {
		return path;
	}
};

/**
 * This struct holds inspire representations of OpenCL built-in host functions
 */
struct Ocl2Inspire {
private:
	core::parse::IRParser parser;

public:
	Ocl2Inspire(core::NodeManager& mgr) :
		parser(mgr) {
	}

	bool extractSizeFromSizeof(const core::ExpressionPtr& arg,
			core::ExpressionPtr& size, core::TypePtr& type);

	core::ExpressionPtr getClCreateBuffer(bool copyHostPtr);
	core::ExpressionPtr getClWriteBuffer();
	core::ExpressionPtr getClWriteBufferFallback();
	core::ExpressionPtr getClReadBuffer();
	core::ExpressionPtr getClReadBufferFallback();
};

/**
 * This class allows replaces a call to an OpenCL built-in function to an INSPIRE one
 */
class Handler {
protected:
	core::ProgramPtr kernels;
public:
	Handler(core::ASTBuilder& build) {
		kernels = core::Program::create(build.getNodeManager());
	}

	virtual core::NodePtr handleNode(core::CallExprPtr node) =0;

	const vector<core::ExpressionPtr>& getKernels() {
		return kernels->getEntryPoints();
	}
};

/*
 * Provides templated child functions of the Handler class. The template argument determines which
 * OpenCL built-in funciton is handled
 */
template<typename Lambda>
class LambdaHandler: public Handler {
	// flag indicating if the definition of the actual function has already been added to the program
	static bool defAdded;
	core::ASTBuilder& builder;

	const char* fct;
	Lambda body;

public:
	LambdaHandler(core::ASTBuilder& build, const char* fun, Lambda lambda) :
		Handler(build), builder(build), fct(fun), body(lambda) {
	}

	// creating a shared pointer to a LambdaHandler

	core::NodePtr handleNode(core::CallExprPtr node) {
		LOG(DEBUG)
			<< "Handling node " << node << std::endl;

		return body(node, kernels);
	}

};

/**
 * This specialized hasher hashes array accesses to the same variable to the same bin
 * regardless of the array index
 */
struct hash_target_specialized : public hash_target<core::ExpressionPtr> {

	core::ASTBuilder builder;

	/**
	 * Explicit Default constructor required by VC.
	 */
	hash_target_specialized(core::ASTBuilder build) : hash_target(), builder(build) {}

	/**
	 * Computes the hash value of the given pointer based on the target it is pointing to.
	 */
	std::size_t operator()(const core::ExpressionPtr expr) const {
		if(!expr)
			return 0;

		const core::CallExprPtr& call = dynamic_pointer_cast<const core::CallExpr>(expr);

		if(!call)
			return hasher(*expr);

//		while(const core::CallExprPtr& tmp = dynamic_pointer_cast<const core::CallExpr>(call->getArgument(0)))
//			call = tmp;

		if(builder.getNodeManager().basic.isSubscriptOperator(call->getFunctionExpr()))
			return hasher(*call->getArgument(0));

		return hasher(*expr);
	}
};

/**
 * compares two Expressions. Returns true if they are equal or if they are both a SubscriptOperator on the same variable,
 * regardless of the index
 */
struct equal_variables {// : public std::binary_function<const core::ExpressionPtr&, const core::ExpressionPtr&, bool> {
	// needed to perform isSubscriptOperator()
	core::ASTBuilder& builder;

	equal_variables(core::ASTBuilder& build) : builder(build) {}

	/**
	 * Performs the actual comparison by using the operator== of the generic
	 * pointer type.
	 *
	 * @param x the pointer to the first element to be compared
	 * @param y the pointer to the second element to be compared
	 */
	bool operator()(const core::ExpressionPtr& x, const core::ExpressionPtr& y) const {
		if(x == y || *x == *y)
			return true;

		core::CallExprPtr xCall =  dynamic_pointer_cast<const core::CallExpr>(x);
/*std::cout << "xCall: " << xCall << std::endl;
		while(const core::CallExprPtr& xExpr = dynamic_pointer_cast<const core::CallExpr>(xCall->getArgument(0)))
			xCall = xExpr;
std::cout << "xExpr: " << xCall << std::endl;
*/
		core::CallExprPtr yCall = dynamic_pointer_cast<const core::CallExpr>(x);
/*std::cout << "yCall: " << yCall << std::endl;
		while(const core::CallExprPtr& yExpr = dynamic_pointer_cast<const core::CallExpr>(yCall->getArgument(0)))
			yCall = yExpr;
std::cout << "yExpr: " << yCall << std::endl;
*/
		if(builder.getNodeManager().basic.isSubscriptOperator(xCall->getFunctionExpr()))
			if(builder.getNodeManager().basic.isSubscriptOperator(yCall->getFunctionExpr()))
				if(xCall->getArgument(0) == yCall->getArgument(0))
					return true;


		return false;
	}
};


typedef std::shared_ptr<Handler> HandlerPtr;
typedef boost::unordered_map<string, HandlerPtr, boost::hash<string> > HandlerTable;
typedef insieme::utils::map::PointerMap<core::VariablePtr, core::VariablePtr> ClmemTable;
typedef boost::unordered_map<core::ExpressionPtr, std::vector<core::ExpressionPtr>, hash_target_specialized, equal_variables> KernelArgs;
//typedef std::map<core::ExpressionPtr, std::vector<core::ExpressionPtr>, equal_variables> KernelArgs;
//typedef insieme::utils::map::PointerMap<core::ExpressionPtr, std::vector<core::ExpressionPtr> > KernelArgs;
typedef boost::unordered_map<string, core::ExpressionPtr, boost::hash<string> > KernelNames;
typedef insieme::utils::map::PointerMap<core::ExpressionPtr, core::LambdaExprPtr> KernelLambdas;
typedef insieme::utils::map::PointerMap<core::ExpressionPtr, vector<core::DeclarationStmtPtr> > LocalMemDecls;

template<typename Lambda>
HandlerPtr make_handler(core::ASTBuilder& builder, const char* fct,
		Lambda lambda) {
	return std::make_shared<LambdaHandler<Lambda> >(builder, fct, lambda);
}

#define ADD_Handler(builder, fct, BODY) \
    handles.insert(std::make_pair(fct, make_handler(builder, fct, [&](core::CallExprPtr node, core::ProgramPtr& kernels){ BODY }))).second;

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
	core::ASTBuilder& builder;

	HandlerTable handles;
	ClmemTable cl_mems;
	Ocl2Inspire o2i;
	core::ProgramPtr& mProgram;
	KernelArgs kernelArgs;
	KernelNames kernelNames;
	vector<core::ExpressionPtr> kernelEntries;
	LocalMemDecls localMemDecls;
	insieme::utils::map::PointerMap<core::NodePtr, core::NodePtr> replacements;

	// check if the call is a call to ref.assign
	core::CallExprPtr checkAssignment(const core::NodePtr& oldCall);

	// needed to be able to work with handlers, identified by prefixes of function names
	HandlerPtr& findHandler(const string& fctName);

	// functions to get set of flags out of an Expression
	// recursiveFlagCheck is designed to be called form getFlags only
	template<typename Enum>
	void recursiveFlagCheck(const core::ExpressionPtr& flagExpr, std::set<Enum>& flags);
	template<typename Enum>
	std::set<Enum> getFlags(const core::ExpressionPtr& flagExpr);

	bool translateClCreateBuffer(const core::ExpressionPtr& var, const core::CallExprPtr& fun, const core::CallExprPtr& newRhs, core::NodePtr& ret);
	bool handleClCreateKernel(const core::ExpressionPtr& expr, const core::ExpressionPtr& call, const core::ExpressionPtr& fieldName);
	bool lookForKernelFilePragma(const core::TypePtr& type, const core::ExpressionPtr& createProgramWithSource);

public:
	HostMapper(core::ASTBuilder& build, core::ProgramPtr& program);

	const core::NodePtr resolveElement(const core::NodePtr& element);

	ClmemTable& getClMemMapping() { return cl_mems; }
	const vector<core::ExpressionPtr>& getKernels() { return kernelEntries;	}
	KernelArgs& getKernelArgs() { return kernelArgs; }
	KernelNames& getKernelNames() { return kernelNames; }
	LocalMemDecls& getLocalMemDecls() {	return localMemDecls; }
	insieme::utils::map::PointerMap<core::NodePtr, core::NodePtr>& getReplacements() { return replacements; }
};

/*
 * Second pass when translating a program OpenCL to IR
 * Responsible for:
 * - connecting the names of kernel functions with the IR entry points (= LambdaExpr)
 */
class Host2ndPass {
	KernelNames& kernelNames;
	KernelLambdas kernelLambdas;
	ClmemTable& cl_mems;
	const core::ASTBuilder& builder;

public:
	Host2ndPass(KernelNames& oclKernelNames, ClmemTable& clMemTable, core::ASTBuilder& build) :
		kernelNames(oclKernelNames), cl_mems(clMemTable), builder(build) {
	}
	void mapNamesToLambdas(const vector<core::ExpressionPtr>& kernelEntries);

	ClmemTable& getCleanedStructures();

	KernelNames& getKernelNames() {
		return kernelNames;
	}
	KernelLambdas& getKernelLambdas() {
		return kernelLambdas;
	}
};

/*
 * First pass when translating a program OpenCL to IR
 * Responsible for:
 * - replace the OpenCL cl_mem vars/structs containing cl_mem vars with IR variables (array<...>)
 * - remove instances of unused variables (cl_program, cl_kernel, ...)
 * - replace calls to cl_enqueueNDRangeKernls with function calls to the correct LambdaExpr with the appropriate arguments
 */
class HostMapper3rdPass: public core::transform::CachedNodeMapping {
	const core::ASTBuilder& builder;
	ClmemTable& cl_mems;
	KernelArgs& kernelArgs;
	LocalMemDecls& localMemDecls;
	KernelNames& kernelNames;
	KernelLambdas& kernelLambdas;
	insieme::utils::map::PointerMap<core::NodePtr, core::NodePtr> replacements;

	// takes the expression size which describes the work size for the clEnqueueNDRange and embed it in an IR function which returns a
	// vector<uint<4>, 3>, always awaited by the kernel function. The elements with index greater or equal to workDim will always be set
	// to 1, regardless of the argument size
	const core::ExpressionPtr anythingToVec3(core::ExpressionPtr workDim,
			core::ExpressionPtr size);

public:
	HostMapper3rdPass(const core::ASTBuilder build, ClmemTable& clMemTable, KernelArgs& oclKernelArgs, LocalMemDecls& oclLocalMemDecls,
			KernelNames& oclKernelNames, KernelLambdas& oclKernelLambdas, insieme::utils::map::PointerMap<core::NodePtr, core::NodePtr> oclReplacements) :
		builder(build), cl_mems(clMemTable), kernelArgs(oclKernelArgs),	localMemDecls(oclLocalMemDecls), kernelNames(oclKernelNames),
			kernelLambdas(oclKernelLambdas), replacements(oclReplacements) { }

	const core::NodePtr resolveElement(const core::NodePtr& element);

};

} //namespace ocl
} //namespace frontend
} //namespace insieme
