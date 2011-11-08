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
	core::IRBuilder& builder;
public:
	NullLitSearcher(core::IRBuilder build) : core::IRVisitor<bool>(false), builder(build) {}

	bool visitNode(const core::NodePtr& node) { return false; }// go on with search
	bool visitCallExpr(const core::CallExprPtr& call);
	bool visitLiteral(const core::LiteralPtr& literal);
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
	bool visitIdentifier(const core::StringValuePtr& id) { return *id == *searchedId; }
};


} //namespace ocl
} //namespace frontend
} //namespace insieme
