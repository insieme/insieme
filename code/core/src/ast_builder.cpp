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

#include "ast_builder.h"

#include "annotated_ptr.h"
#include "program.h"
#include "statements.h"
#include "expressions.h"
#include "types.h"

namespace insieme {
namespace core {

ProgramPtr ASTBuilder::createProgram(const Program::EntryPointSet& entryPoints) {
	return Program::create(sharedManager, entryPoints);
}

// ------------------------------- Build Basic Types -------------------------

lang::UnitTypePtr ASTBuilder::getUnitType() const {
	return manager.get(lang::TYPE_UNIT);
}

lang::BoolTypePtr ASTBuilder::getBoolType() const {
	return manager.get(lang::TYPE_BOOL);
}

lang::IntTypePtr  ASTBuilder::getIntType (unsigned short size) const {
	return manager.get(lang::getIntType(size));
}

lang::UIntTypePtr ASTBuilder::getUIntType(unsigned short size) const {
	return manager.get(lang::getUIntType(size));
}

lang::RealTypePtr ASTBuilder::getRealType(unsigned short size) const {
	return manager.get(lang::getRealType(size));
}

// ------------------------------- Build Expressions -------------------------

ExpressionPtr ASTBuilder::createExpr(const lang::OperatorPtr& op, const ExpressionPtr& operant) const {
	return CallExpr::get(manager, op, toVector(operant));
}
ExpressionPtr ASTBuilder::createExpr(const ExpressionPtr& rhs, const lang::OperatorPtr& op, const ExpressionPtr& lhs) const {
	return CallExpr::get(manager, op, toVector(rhs, lhs));
}


#include "ast_builder_impl.inl"

} // namespace core
} // namespace insieme
