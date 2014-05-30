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

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/frontend/frontend.h"

namespace insieme {
namespace frontend {
namespace ocl {

namespace {
/*
 * Collects the kernel function names, specified using clCreateKernel and stores them in a map
 *
class FindKernelNames: public core::transform::CachedNodeMapping {
	std::map<core::ExpressionPtr, std::string> kernelNames;
	core::pattern::TreePatternPtr clCreateKernel;

	const core::NodePtr resolveElement(const core::CallExprPtr& ptr);

public:
	FindKernelNames();
};
*/


/**
 * Class to visit the AST and return the value of a certain variable, holding the path to a OpenCL kernel, if it exists at all
 */
class KernelCodeRetriver: public core::IRVisitor<bool> {
	const core::ExpressionPtr& pathToKernelFile; // Variable to look for
	const core::NodePtr& breakingStmt; // place where the path would be needed, can stop searching there
	const core::ProgramPtr program;
	const core::IRBuilder builder;
	const core::lang::BasicGenerator& gen;
	string path;

	bool visitNode(const core::NodePtr& node);
	bool visitCallExpr(const core::CallExprPtr& callExpr);
	bool visitDeclarationStmt(const core::DeclarationStmtPtr& decl);

	bool saveString(const core::LiteralPtr& lit);
	bool saveString(const core::CallExprPtr& call);
public:
	KernelCodeRetriver(const core::ExpressionPtr lookFor,
			const core::NodePtr& stopAt, core::ProgramPtr prog) :
		IRVisitor<bool> (false), pathToKernelFile(lookFor),
				breakingStmt(stopAt), program(prog), builder(prog->getNodeManager()), gen(prog->getNodeManager().getLangBasic()) { }
	string getKernelFilePath() {
		return path;
	}
};

}

typedef boost::unordered_map<string, core::ExpressionPtr, boost::hash<string> > KernelNames;
typedef boost::unordered_map<core::ExpressionPtr, core::TypeList> KernelTypes;
typedef boost::unordered_map<core::ExpressionPtr, core::LambdaExprPtr> KernelFunctions;
typedef boost::unordered_map<core::ExpressionPtr, std::set<unsigned int> > LocalMemArgs;
typedef std::map<core::NodeAddress, core::NodePtr> NodeAddressMap;
typedef std::map<core::ExpressionPtr, core::ExpressionPtr> KernelFields;

/*
 * Collects cl_kernel expressions, identifies all the arguments for the corresponding kernel functions and replaces it with a tuple, holding the arguments
 * Loads kernel source codes from files and adds them to the program
 * Replaces nd_range calls with calls to the actual function
 */
class KernelReplacer {
public:
	KernelReplacer(core::NodePtr prog, const std::vector<boost::filesystem::path>& includeDirs);
	virtual ~KernelReplacer() {}
protected:
	core::NodePtr prog;
	KernelNames kernelNames;
	KernelTypes kernelTypes;
	KernelFunctions kernelFunctions;
	KernelFields kernelFields;
	LocalMemArgs localMemArgs;
	const std::vector<boost::filesystem::path>& includeDirs;
	std::set<string> kernelFileCache;

	std::vector<std::string> findKernelNames(core::pattern::TreePatternPtr);
	virtual core::ExpressionPtr handleArgument(const core::TypePtr& argTy, const core::TypePtr& memberTy, const core::ExpressionPtr& tupleMemberAccess,
			core::StatementList& body);
	virtual void collectArguments();
	void replaceKernels();
	virtual void loadKernelCode(core::pattern::TreePatternPtr);
	void storeKernelLambdas(std::vector<core::ExpressionPtr>& kernelEntries, std::map<string, int>& checkDuplicates);
	virtual void inlineKernelCode();
	core::ProgramPtr findKernelsUsingPathString(const core::ExpressionPtr& path, const core::ExpressionPtr& root, const core::ProgramPtr& mProgram);
	std::vector<core::ExpressionPtr> lookForKernelFilePragma(const core::TypePtr& type, const core::ExpressionPtr& createProgramWithSource);
	core::ExpressionPtr createKernelCallLambda(const core::ExpressionAddress localKernel,
			const core::ExpressionPtr work_dim, const core::ExpressionPtr local_work_size, const core::ExpressionPtr global_work_size);
public:
	virtual core::NodePtr getTransformedProgram();
};

class IclKernelReplacer : public KernelReplacer {
public:
	IclKernelReplacer(core::NodePtr prog, const std::vector<boost::filesystem::path>& includeDirs) : KernelReplacer(prog, includeDirs) {}
	virtual core::NodePtr getTransformedProgram();
	virtual void loadKernelCode(core::pattern::TreePatternPtr);
	virtual void inlineKernelCode();
};

} //namespace ocl
} //namespace frontend
} //namespace insieme
