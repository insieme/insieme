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

#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/annotations/data_annotations.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {

using namespace insieme::core;

// shortcut
#define BASIC builder.getNodeManager().getLangBasic()

class InductionVarMapper : public core::transform::CachedNodeMapping {

	NodeManager& mgr;
	const IRBuilder builder;
	const Extensions& extensions;

	// counters for the local and global dimensions
//	size_t globalDim, groupDim, localDim;

	NodeMap replacements;
	/*
	 * checks if the passed variable is one of the 6 loop induction variables
	 * @param
	 * var the variable to be checked
	 * @return
	 * true if the passed variable is one of the loop induction variables, false otherwise
	 */
	bool isGetId(ExpressionPtr expr);

	/*
	 * checks if the first argument of the passed call is an integer literal. If yes and the value is between 0 and 2,
	 * it's value is returned, otherwise an assertion is raised
	 * @param
	 * call A CallExprPtr with an integer literal as first argument
	 * @return
	 * the value of the first argument
	 */
	size_t extractIndexFromArg(CallExprPtr call) const;

	/*
	 * removes stuff that bothers me when doing analyzes, e.g. casts, derefs etc
	 * @param epxr An expression with annoying stuff around
	 * @return An expression without annoying stuff
	 */
	ExpressionPtr removeAnnoyingStuff(ExpressionPtr expr) const;
public:
	InductionVarMapper(NodeManager& manager, NodeMap replacements = NodeMap()) :
		mgr(manager), builder(manager), extensions(manager.getLangExtension<Extensions>()), replacements(replacements) { }

	const NodePtr resolveElement(const NodePtr& ptr);

	/*
	 * returns the replacements for variables which can be replaced with loop induction variables
	 */
	NodeMap getReplacements() const { return replacements; }
};


typedef insieme::utils::map::PointerMap<core::VariablePtr, insieme::utils::map::PointerMap<core::ExpressionPtr, ACCESS_TYPE> > AccessMap;

class IndexExprEvaluator : public IRVisitor<void> {
	const IRBuilder& builder;
	// map to store global variables with accessing expressions. Should be the same instance as in the InductionVarMapper
	AccessMap& accesses;
	// pattern that describes an subscript access to an opencl global variable
	insieme::core::pattern::TreePatternPtr globalAccess;
	// pattern that describes a use of an opencl global variable
	insieme::core::pattern::TreePatternPtr globalUsed;
	// list of aliases of global variables
	utils::map::PointerMap<ExpressionPtr, VariablePtr> globalAliases;

	ACCESS_TYPE rw;

public:
	IndexExprEvaluator(	const IRBuilder& build, AccessMap& idxAccesses);

	void visitCallExpr(const CallExprPtr& idx);

	/*
	 * sets the read-write flag.
	 */
	void setAccessType(ACCESS_TYPE readWrite) { rw = readWrite; }

	void printGlobalAliases() {
		for_each(globalAliases, [](std::pair<ExpressionPtr, VariablePtr> ga) {
			std::cout << "GA " << ga.first << " -> " << ga.second << std::endl;
		});
	}
};

class AccessExprCollector : public IRVisitor<void> {
	const IRBuilder& builder;

	// map to store global variables with accessing expressions
	AccessMap accesses;

	// visitor to be called on the index argument of stubscript expressions
	IndexExprEvaluator iee;

public:
	AccessExprCollector(const IRBuilder& build) : IRVisitor<void>(false), builder(build), iee(build, accesses) { };
	void visitCallExpr(const CallExprPtr& call);
	void visitDeclarationStmt(const DeclarationStmtPtr& decl);

	AccessMap& getAccesses() { return accesses; }
};

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme

