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
#include "insieme/core/ir_address.h"

namespace insieme {
namespace frontend {
namespace ocl {

// shortcut
#define BASIC builder.getLangBasic()

/*
 *  get a VariablePtr which is hidden under the stuff added by the frontend if their is a cast to (void*) in the C input
 */
core::ExpressionPtr getVarOutOfCrazyInspireConstruct(const core::ExpressionPtr& arg, const core::IRBuilder& builder);

/*
 * Returns the very base type of the passed Expression
 */
const core::TypePtr getBaseType(const core::ExpressionPtr& singleElementExpr);
/*
 * Returns the very base type of the passed type
 */
const core::TypePtr getBaseType(const core::TypePtr& singleElementType);

/*
 * Function to get the type of an Expression
 * If it is a ref-type, it's element type is returned
 */
const core::TypePtr getNonRefType(const core::ExpressionPtr& refExpr);
/*
 * Returns either the type itself or the element type, in case that it is a referenceType
 */
const core::TypePtr getNonRefType(const core::TypePtr& refType);

/*
 * Builds a ref.deref call around an expression if the it is of ref-type
 */
core::ExpressionPtr tryDeref(const core::ExpressionPtr& expr, const core::IRBuilder& builder);

/*
 * removes the returns 'a if type is ref<'a>, type otherwise
 */
core::TypePtr removeSingleRef(const core::TypePtr& type);

/*
 * Builds a ref.deref call around an expression if the it is of type ref<ref<'a>>
 */
core::ExpressionPtr removeDoubleRef(const core::ExpressionPtr& expr, const core::IRBuilder& builder);

/*
 * Returns either the expression itself or the first argument if expression was a call to function
 */
core::ExpressionPtr tryRemove(const core::ExpressionPtr& function, const core::ExpressionPtr& expr, const core::IRBuilder& builder);

/*
 * Returns either the expression itself or the expression inside a nest of ref.new/ref.var calls
 */
core::ExpressionPtr tryRemoveAlloc(const core::ExpressionPtr& expr, const core::IRBuilder& builder);

/*
 * 'follows' the first argument as long it is a call expression until it reaches a variable. If a variable is found it returns it, otherwise NULL is returned
 * Usefull to get variable out of nests of array and struct accesses
 */
core::VariablePtr getVariableArg(const core::ExpressionPtr& function, const core::IRBuilder& builder);

/*
 * Function to copy all annotations form one NodePtr to another
 */
void copyAnnotations(const core::NodePtr& source, core::NodePtr& sink);

/*
 * Visitor returns true if the passed ast contains a zero literal
 */
class NullLitSearcher: public core::IRVisitor<bool> {
private:
	core::IRBuilder builder;
public:
	NullLitSearcher(core::IRBuilder build) : core::IRVisitor<bool>(false), builder(build) {}

	bool visitNode(const core::NodePtr& node) { return false; }// go on with search
	bool visitCallExpr(const core::CallExprPtr& call);
	bool visitLiteral(const core::LiteralPtr& literal) {
		return literal->getStringValue() == "0";
	}
};

/*
 * Visitor returns true if the passed ast the identifier passed to the construtor
 */
class IdSearcher: public core::IRVisitor<bool> {
private:
	const core::StringValuePtr& searchedId;
public:
	IdSearcher(const core::StringValuePtr& lookFor) : core::IRVisitor<bool>(false), searchedId(lookFor) {}

	bool visitNode(const core::NodePtr& node) { return false; }// go on with search
	bool visitStringValue(const core::StringValuePtr& id) { return *id == *searchedId; }
};

/*
 * Visitor return true if the lambda associated with the kernel variable is found in the ast
 */
class LambdaSearcher: public core::IRVisitor<bool, core::Address> {
private:
	const core::IRBuilder& builder;
	const core::NodeAddress vAddr;
	core::NodeAddress lAddr;
	const core::ProgramPtr& root;
public:
	LambdaSearcher(const core::IRBuilder& build, const core::VariablePtr& var, const core::ProgramPtr& searchRoot) :
		core::IRVisitor<bool, core::Address>(false), builder(build), vAddr(core::Address<const core::Variable>::find(var, searchRoot)), root(searchRoot) {}

	void setLambdaVariable(const core::VariablePtr& lambda) { lAddr = core::Address<const core::Variable>::find(lambda, root); }
	bool visitCallExpr(const core::CallExprAddress& call);
};

/*
 * Gets a list of expressions, looks for variables inside, replaces them with fresh variables and creates a map from the old to the new variables
 *
class VariableRefresher {
private:
	core::VariableMap varMap;

public:


};*/
	core::VariableMap refreshVariables(std::vector<core::DeclarationStmtPtr>& localMemDecls, const core::IRBuilder& builder);
	void refreshVariables(core::ExpressionPtr& localMemInit, core::VariableMap& varMapping, const core::IRBuilder& builder);

	/*
	 * takes a type ref<array<vector<'b,#l>,1>> and creates ref<array<'b>,1> from it
	 * @param arrayTy The type to be processed
	 * @return arrayTy unchanged if arrayTy is not ref<array<vector<'b,#l>,1>>, ref<array<'b>,1> otherwise
	 */
	core::TypePtr vectorArrayTypeToScalarArrayType(core::TypePtr arrayTy, const core::IRBuilder& builder);


	/*
	 * checkes if the type of expr is type, otherwise a refReinterpred is added around expr and returned;
	 * @param expr The expression to be reinterpreted if the type does not match
	 * @param type the type expr should have
	 * @param builder an IRBuilder
	 * @return expr if expr has type type, refReinterpret(expr, deref(type)) otherwise
	 */
	core::ExpressionPtr tryRefReinterpret(core::ExpressionPtr expr, core::TypePtr type, core::IRBuilder builder);
} //namespace ocl
} //namespace frontend
} //namespace insieme
