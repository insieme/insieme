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

#include <boost/utility.hpp>

#include "insieme/utils/set_utils.h"
#include "insieme/core/forward_decls.h"

namespace insieme {
namespace core {

typedef utils::set::PointerSet<TypePtr> TypeSet;

namespace lang {

using std::string;

class LiteralNotFoundException : std::exception {
	string msg;
public:
	LiteralNotFoundException(const string& lit) throw();
	~LiteralNotFoundException() throw() { }
	const char* what() const throw() {
		return msg.c_str();
	}
};

class BasicGenerator : boost::noncopyable {
	NodeManager& nm;
	struct BasicGeneratorImpl;
	mutable BasicGeneratorImpl* pimpl;
	class SubTypeLattice;
	mutable SubTypeLattice* subTypeLattice;

public:
	BasicGenerator(NodeManager& nm);
	~BasicGenerator();

	NodeManager& getNodeManager() const {
		return nm;
	}

	enum Operator {
	#define OPERATOR(_id, _str) \
	_id,
	#include "insieme/core/lang/lang.def"
	};

	#define TYPE(_id, _spec) \
	TypePtr get##_id() const; \
	bool is##_id(const NodePtr& p) const;

	#define LITERAL(_id, _name, _spec) \
	LiteralPtr get##_id() const; \
	bool is##_id(const NodePtr& p) const;

	#define OPERATION(_type, _op, _name, _spec) \
	LiteralPtr get##_type##_op() const; \
	bool is##_type##_op(const NodePtr& p) const;

	#define GROUP(_id, ...) \
	bool is##_id(const NodePtr& p) const;

	#include "insieme/core/lang/lang.def"

	bool isBuiltIn(const NodePtr& node) const;
	LiteralPtr getLiteral(const std::string& name) const;
	// Obtains the LiteralPtr which implements the operator op for literals of
	// type 'type'
	ExpressionPtr getOperator(const TypePtr& type, const Operator& op) const;
	// Obtains the operator (as defined in the lang.def class) from a literal
	// expression obtained thorugh the previous metod (inverse procedure)
	Operator getOperator(const LiteralPtr& lit) const;
	// ----- extra material ---

	StatementPtr getNoOp() const;
	bool isNoOp(const NodePtr& p) const;

	// a factory method for intTypeParam literals
	LiteralPtr getIntTypeParamLiteral(const IntTypeParamPtr& param) const;

	// a factory method for a identifier literal
	LiteralPtr getIdentifierLiteral(const StringValuePtr& value) const;

	// a method generating a vector init expression form a scalar
	ExpressionPtr scalarToVector(const TypePtr& type, const ExpressionPtr& subExpr) const;

	// two methods obtaining all direct super / sub types of the given generic types
	TypeSet getDirectSuperTypesOf(const TypePtr& type) const;
	TypeSet getDirectSubTypesOf(const TypePtr& type) const;

	// a factory method for a type literal
	LiteralPtr getTypeLiteral(const TypePtr& type) const;

private:

	// obtains a pointer to a lazy-instantiated sub-type lattice
	const SubTypeLattice* getSubTypeLattice() const;
};

} // namespace lang
} // namespace core
} // namespace insieme
