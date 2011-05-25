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

#include <string>
#include <utility>
#include <vector>
#include <cassert>

namespace insieme {
namespace backend {
namespace c_ast {

	class CBasics {

		CNodeManager& manager;

	public:

		CBasics(CNodeManager& manager) : manager(manager) {}

		IdentifierPtr getIdentifier(const string& name) {
			return manager.create(name);
		}

		TypePtr getIntType() {
			return manager.create<PrimitiveType>(getIdentifier("int"));
		}

	};


	// --- create literals and variables ------------------------

	inline VariablePtr var(TypePtr& type, const string& name) {
		assert(type && type->getManager() && "Expected type to be present!");
		CNodeManager* manager = type->getManager();
		return manager->create<Variable>(type, manager->create(name));
	}

	// --- all kind of overloaded operators ---

	// -- Unary Operations --------------------------------------


	// -- Binary Operations -------------------------------------

	inline ExpressionPtr binaryOp(BinaryOperation::BinaryOp op, ExpressionPtr a, ExpressionPtr b) {
		assert(a && b && a->getManager() && a->getManager() == b->getManager() && "Manager should match!");
		return a->getManager()->create<BinaryOperation>(op, a, b);
	}

	inline ExpressionPtr add(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Additon, a, b);
	}

	inline ExpressionPtr sub(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Subtraction, a, b);
	}

	inline ExpressionPtr mul(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Multiplication, a, b);
	}

	inline ExpressionPtr div(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Division, a, b);
	}

	inline ExpressionPtr mod(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Modulo, a, b);
	}

	inline ExpressionPtr logAnd(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::LogicAnd, a, b);
	}


} // end namespace c_ast
} // end namespace backend
} // end namespace insieme
